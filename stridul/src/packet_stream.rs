use crate::{ Strategy, StreamReader, StreamWriter };

use std::{sync::Arc, task, pin::Pin};

use bytes::{BytesMut, BufMut, Bytes};
use futures::{Stream, Sink};
use tokio::io::AsyncWrite;
use tokio_util::io::poll_read_buf;

// Put in cobweb as util (With socket ?)
#[ouroboros::self_referencing]
pub struct PacketStream<Strat: Strategy> {
    stream: Arc<crate::Stream<Strat>>,
    buffer: BytesMut,
    send_buffer: BytesMut,

    #[borrows(stream)]
    #[covariant]
    read: StreamReader::<'this, Strat>,
    #[borrows(stream)]
    #[not_covariant]
    write: StreamWriter::<'this, Strat>,
}

impl<Strat: Strategy> PacketStream<Strat> {
    pub fn create(stream: Arc<crate::Stream<Strat>>) -> Self {
        PacketStreamBuilder {
            stream,
            buffer: BytesMut::with_capacity(4),

            read_builder: |stream| stream.reader(),
            write_builder: |stream| stream.writer(),

            send_buffer: BytesMut::new(),
        }.build()
    }

    pub fn inner(&self) -> &Arc<crate::Stream<Strat>> {
        &self.borrow_stream()
    }
}

impl<Strat: Strategy> Stream for PacketStream<Strat> {
    type Item = anyhow::Result<Bytes>;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut task::Context<'_>
    ) -> task::Poll<Option<Self::Item>> {
        self.get_mut().with_mut(|fields| {
            let reader = fields.read;
            tokio::pin!(reader);
            let buffer = fields.buffer;

            fn is_finished(buf: &mut BytesMut) -> bool {
                if buf.len() < 4 {
                    return false;
                }

                let packet_size =
                    u32::from_be_bytes((&buf[0..4]).try_into().unwrap())
                        as usize;

                buf.reserve((packet_size + 4 - buf.len()).min(4096));
                buf.len() >= packet_size
            }

            while !is_finished(buffer) {
                match poll_read_buf(reader.as_mut(), cx, buffer) {
                    task::Poll::Ready(Ok(_)) => (),
                    task::Poll::Ready(Err(e)) => return task::Poll::Ready(
                        Some(Err(e.into()))
                    ),
                    task::Poll::Pending =>
                        return task::Poll::Pending,
                }
            }

            let packet_size =
                u32::from_be_bytes((&buffer[0..4]).try_into().unwrap())
                    as usize;

            let packet_data = &buffer[4..packet_size];
            task::Poll::Ready(Some(Ok(Bytes::copy_from_slice(packet_data))))
        })
    }
}

impl<Strat: Strategy> Sink<Bytes> for PacketStream<Strat> {
    type Error = anyhow::Error;

    fn poll_ready(
        self: Pin<&mut Self>, _cx: &mut task::Context<'_>
    ) -> task::Poll<Result<(), Self::Error>> {
        // FIXME: Buffer is resized when needed, no need to wait ?
        task::Poll::Ready(Ok(()))
    }

    fn start_send(
        self: Pin<&mut Self>, item: Bytes
    ) -> Result<(), Self::Error> {
        self.get_mut().with_mut(|fields| {
            let sb = fields.send_buffer;

            let size_start = sb.len();
            sb.extend_from_slice(&[0; 4]);
            let content_start = sb.len();

            sb.put_slice(&item);

            let size: u32 = (content_start - sb.len()).try_into().unwrap();
            sb[size_start..size_start + 4].copy_from_slice(
                &size.to_be_bytes()
            );

            Ok(())
        })
    }

    fn poll_flush(
        mut self: Pin<&mut Self>, cx: &mut task::Context<'_>
    ) -> task::Poll<Result<(), Self::Error>> {
        while self.borrow_send_buffer().len() > 0 {
            let last_poll = self.as_mut().with_mut(|fields| {
                let sb = fields.send_buffer;
                let write = fields.write;
                tokio::pin!(write);

                let written = match write.poll_write(cx, sb)? {
                    task::Poll::Ready(w) => w,
                    task::Poll::Pending => return task::Poll::Pending,
                };

                // Remove the written bytes
                sb.copy_within(written.., 0);
                sb.truncate(sb.len() - written);

                task::Poll::Ready(Ok(()))
            });

            match last_poll {
                task::Poll::Ready(Ok(())) => (),
                x => return x,
            }
        }

        task::Poll::Ready(Ok(()))
    }

    fn poll_close(
        self: Pin<&mut Self>, cx: &mut task::Context<'_>
    ) -> task::Poll<Result<(), Self::Error>> {
        self.poll_flush(cx)
    }
}
