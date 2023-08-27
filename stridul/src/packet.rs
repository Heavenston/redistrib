use crate::*;

use std::{
    time::{Duration, Instant}, sync::{Arc, RwLock, Mutex, Weak},
    collections::HashMap, ops::ControlFlow, mem::size_of, fmt::Display, io::{Write, Read}
};

use static_assertions as ca;
use bytes::{Bytes, BytesMut, Buf};
use itertools::Itertools;
use tokio::{net::{UdpSocket, ToSocketAddrs}, stream, time as ttime, sync::{mpsc, oneshot}};
use thiserror::Error;
use futures::future::Either as fEither;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PacketId {
    pub stream_id: StreamID,
    pub sequence_number: u32,
    pub retransmission: u8,
}

impl PacketId {
    pub fn serialize(
        &self, mut into: impl Write
    ) -> Result<(), StridulError> {
        into.write_u32(self.stream_id)?;
        into.write_u32(self.sequence_number)?;
        into.write_u8(self.retransmission)?;
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, StridulError> {
        Ok(Self {
            stream_id: from.read_u32()?,
            sequence_number: from.read_u32()?,
            retransmission: from.read_u8()?,
        })
    }

    pub fn with_rt(self, rt: u8) -> Self {
        Self {
            retransmission: rt,
            ..self
        }
    }
}

impl std::fmt::Display for PacketId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(s{}, seq{}, x{})", self.stream_id, self.sequence_number, self.retransmission)
    }
}

#[derive(Debug, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AckPack {
    pub acked_id: PacketId,
    pub window_size: u32,
}

impl AckPack {
    pub fn serialize(
        &self, mut into: impl Write
    ) -> Result<(), StridulError> {
        self.acked_id.serialize(&mut into)?;
        into.write_u32(self.window_size)?;
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, StridulError> {
        Ok(Self {
            acked_id: PacketId::deserialize(&mut from)?,
            window_size: from.read_u32()?,
        })
    }
}

impl std::fmt::Display for AckPack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ack({}, w {})", self.acked_id, self.window_size)
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct DataPack {
    pub id: PacketId,
    pub data: Bytes,
}

impl DataPack {
    pub fn serialize(
        &self, mut into: impl Write
    ) -> Result<(), StridulError> {
        self.id.serialize(&mut into)?;
        into.write_all(&self.data)?;
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, StridulError> {
        let id = PacketId::deserialize(&mut from)?;
        // FIXPERF: Allocation and copies >:(
        let mut data = Vec::new();
        from.read_to_end(&mut data)?;
        Ok(Self {
            id,
            data: Bytes::from(data),
        })
    }
}

impl std::fmt::Display for DataPack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Data({}, s {})", self.id, self.data.len())
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum StridulPacket {
    Ack(AckPack),
    Data(DataPack),
}

impl StridulPacket {
    pub fn serialize(
        &self, mut into: impl Write
    ) -> Result<(), StridulError> {
        match self {
            StridulPacket::Ack(ack) => {
                into.write_u16(0)?;
                ack.serialize(&mut into)?;
            },
            StridulPacket::Data(data) => {
                into.write_u16(1)?;
                data.serialize(&mut into)?;
            },
        }
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, StridulError> {
        // Only the low byte is used for possible future extensions
        match from.read_u16()? & 0xFF {
            0 => Ok(Self::Ack(AckPack::deserialize(&mut from)?)),
            1 => Ok(Self::Data(DataPack::deserialize(&mut from)?)),
            id => Err(StridulError::UnexpectedValueError {
                message: format!("Supported packet types are 0 and 1, got {id}"),
            })
        }
    }

    pub fn id(&self) -> PacketId {
        use StridulPacket::*;
        match self {
            Ack(AckPack { acked_id: id, .. }) |
            Data(DataPack { id, .. }) => *id,
        }
    }

    pub fn id_mut(&mut self) -> &mut PacketId {
        use StridulPacket::*;
        match self {
            Ack(AckPack { acked_id: id, .. }) |
            Data(DataPack { id, .. }) => id,
        }
    }

    pub fn has_data(&self) -> bool {
        match self {
            StridulPacket::Data { .. } => true,
            _ => false,
        }
    }

    pub fn data(&self) -> Option<&Bytes> {
        use StridulPacket::*;
        match self {
            Ack(..)
                => None,
            Data(DataPack { data, .. })
                => Some(data),
        }
    }

    pub fn data_len(&self) -> usize {
        self.data().map(|b| b.len()).unwrap_or(0)
    }
}

impl std::fmt::Display for StridulPacket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StridulPacket::Ack(pack) => write!(f, "{pack}"),
            StridulPacket::Data(pack) => write!(f, "{pack}")
        }
    }
}

impl From<AckPack> for StridulPacket {
    fn from(value: AckPack) -> Self {
        Self::Ack(value)
    }
}
impl From<DataPack> for StridulPacket {
    fn from(value: DataPack) -> Self {
        Self::Data(value)
    }
}

