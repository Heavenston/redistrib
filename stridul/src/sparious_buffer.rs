use crate::*;

use std::{time::Duration, sync::{Arc, Mutex, atomic::{AtomicU32, self}}, marker::PhantomData, pin::{Pin, pin}, task::Poll, io::Write, fmt::Display};

use tokio_util::sync::ReusableBoxFuture;
use std::future::Future;
use bytes::{BytesMut, Bytes, BufMut};
use tokio::{net::{UdpSocket, ToSocketAddrs}, io::{AsyncRead, AsyncWrite}, sync::{Notify, futures::Notified, Mutex as AMutex, MutexGuard as AMutexGuard}};
use thiserror::Error;
use itertools::Itertools;

#[derive(Debug, Clone, Default)]
pub struct BuffEl {
    pub start_idx: usize,
    pub bytes: Bytes,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferInsertError {
    /// Buffer is fully covered by other buffers
    FullOverlap,
    /// Buffer is partially covered by other buffers
    PartialOverlap,
    /// Insertion would make theoriatical size get over maxmimum
    WouldOverflow,
}

#[derive(Debug, Clone)]
pub struct SortedSpariousBuffer {
    /// May be more than the first element start_idx
    /// This is called over-flush and means that these first
    /// bytes are already flushed.
    pub flushed_bytes: usize,
    pub els: Vec<BuffEl>,
    pub max_size: usize,
}

impl Default for SortedSpariousBuffer {
    fn default() -> Self {
        Self {
            flushed_bytes: default(),
            els: default(),
            max_size: 4096,
        }
    }
}

impl SortedSpariousBuffer {
    pub fn insert(&mut self, el: BuffEl) -> Result<(), BufferInsertError> {
        let i = (0usize..self.els.len())
            .find(|&i| self.els[i].start_idx >= el.start_idx)
            .unwrap_or(self.els.len());

        let new_max_size = 
            self.els.iter()
            .chain([&el])
            .map(|x| x.start_idx + x.bytes.len())
            .max().unwrap_or(0)
            .saturating_sub(self.flushed_bytes);
        if new_max_size > self.max_size {
            return Err(BufferInsertError::WouldOverflow);
        }

        // TODO: Detect partial overlaps
        if let Some(el2) = self.els.get(i) {
            if el2.start_idx == el.start_idx {
                return Err(BufferInsertError::FullOverlap);
            }
        }
        if el.start_idx + el.bytes.len() <= self.flushed_bytes {
            return Err(BufferInsertError::FullOverlap);
        }

        self.els.insert(i, el);

        Ok(())
    }

    pub fn actual_len(&self) -> usize {
        self.els.iter()
            .map(|x| x.bytes.len())
            .sum()
    }

    /// Maximum theratical size if all dis-continuities are filled
    pub fn max_size(&self) -> usize {
        self.els.iter()
            .map(|x| x.start_idx + x.bytes.len())
            .max().unwrap_or(0)
            .saturating_sub(self.flushed_bytes)
    }

    pub fn contiguouses<'a>(&'a self) -> impl Iterator<Item = &'a BuffEl> {
        self.els.iter()
            .scan(self.flushed_bytes, |s, el| {
                let overflushed_bytes = s.saturating_sub(el.start_idx);

                let start = *s;
                *s = s.wrapping_add(el.bytes.len())
                      .wrapping_sub(overflushed_bytes);
                Some((start, el))
            })
            .take_while(|(cumul_idx, el)| {
                el.start_idx <= *cumul_idx
            })
            .map(|x| x.1)
    }

    pub fn contiguous_len(&self) -> usize {
        self.contiguouses().count()
    }

    pub fn contiguous_bytes_len(&self) -> usize {
        self.contiguouses()
            .map(|el| el.bytes.len())
            .sum()
    }

    pub fn remaining_capacity(&self) -> usize {
        self.max_size.saturating_sub(self.actual_len())
    }

    pub fn drain_contiguous(
        &mut self, max_bytes: Option<usize>,
    ) -> ContiguousFlushIterator<'_> {
        return ContiguousFlushIterator {
            buffer: self,
            max_remaining_bytes: max_bytes,
        };
    }
}

impl Display for SortedSpariousBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SortedSpariousBuffer({}", self.flushed_bytes)?;
        for el in &self.els {
            write!(f, " -> ({}+{}={})", el.start_idx, el.bytes.len(), el.start_idx + el.bytes.len())?;
        }
        write!(f, ")")?;

        Ok(())
    }
}

pub struct ContiguousFlushIterator<'a> {
    buffer: &'a mut SortedSpariousBuffer,
    max_remaining_bytes: Option<usize>,
}

impl<'a> Iterator for ContiguousFlushIterator<'a> {
    type Item = Bytes;

    fn next(&mut self) -> Option<Self::Item> {
        log::trace!("Buffer next - {}", self.buffer);
        let current = self.buffer.els.get(0)?;
        if current.start_idx > self.buffer.flushed_bytes
        { return None; }

        // Remainning bytes in the current element
        // (less than len in case of over flush)
        let rem_start = self.buffer.flushed_bytes
            .saturating_sub(current.start_idx);
        let rem_in_curr =
            (current.start_idx + current.bytes.len())
            .saturating_sub(self.buffer.flushed_bytes);
        debug_assert_ne!(rem_in_curr, 0, "Element with 0 remaning should be removed");
        log::trace!("First buffer is {} + {} - {}", current.start_idx, current.bytes.len(), self.buffer.flushed_bytes);

        let bytes_to_take = if let Some(max) = self.max_remaining_bytes {
            max.min(rem_in_curr)
        } else {
            rem_in_curr
        };
        log::trace!("Taking {} + {rem_start}..{}", self.buffer.flushed_bytes, rem_start + bytes_to_take);

        if bytes_to_take == 0 { return None; }

        // FIXPERF: Avoid bytes clone if taking all bytes
        //          Saves... picoseconds ?
        let bytes = current.bytes.slice(rem_start..rem_start + bytes_to_take);

        // Remove if fully flushed el
        // FIXPERF: Bulk remove all elements when iterator is dropped
        //          to avoid the movement to the left n times
        if bytes_to_take == rem_in_curr
        { self.buffer.els.remove(0); }

        self.buffer.flushed_bytes += bytes_to_take;

        if let Some(max) = &mut self.max_remaining_bytes {
            *max = max.saturating_sub(bytes_to_take);
        }

        Some(bytes)
    }
}
