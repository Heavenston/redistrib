use crate::*;

use std::io::{Write, Read};

use bytes::Bytes;

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
    ) -> Result<(), Error> {
        into.write_u32(self.stream_id)?;
        into.write_u32(self.sequence_number)?;
        into.write_u8(self.retransmission)?;
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, Error> {
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
    ) -> Result<(), Error> {
        self.acked_id.serialize(&mut into)?;
        into.write_u32(self.window_size)?;
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, Error> {
        Ok(Self {
            acked_id: PacketId::deserialize(&mut from)?,
            window_size: from.read_u32()?,
        })
    }
}

impl std::fmt::Display for AckPack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ack({}, win{})", self.acked_id, self.window_size)
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
    ) -> Result<(), Error> {
        self.id.serialize(&mut into)?;
        into.write_all(&self.data)?;
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, Error> {
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
        write!(f, "Data({}, len{})", self.id, self.data.len())
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MessagePack {
    pub data: Bytes,
}

impl MessagePack {
    pub fn serialize(
        &self, mut into: impl Write
    ) -> Result<(), Error> {
        into.write_all(&self.data)?;
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, Error> {
        // FIXPERF: Allocation and copies >:(
        let mut data = Vec::new();
        from.read_to_end(&mut data)?;
        Ok(Self {
            data: Bytes::from(data),
        })
    }
}

impl std::fmt::Display for MessagePack {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Message(")?;
        if self.data.len() < 10 {
            write!(f, "{:?}, ", self.data)?;
        }
        write!(f, "len{})", self.data.len())?;

        Ok(())
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Packet {
    Ack(AckPack),
    Data(DataPack),
    Message(MessagePack),
}

impl Packet {
    pub fn serialize(
        &self, mut into: impl Write
    ) -> Result<(), Error> {
        match self {
            Packet::Ack(ack) => {
                into.write_u16(0x0000)?;
                ack.serialize(&mut into)?;
            },
            Packet::Data(data) => {
                into.write_u16(0x0001)?;
                data.serialize(&mut into)?;
            },
            Packet::Message(mess) => {
                into.write_u16(0x0002)?;
                mess.serialize(&mut into)?;
            },
        }
        Ok(())
    }

    pub fn deserialize(mut from: impl Read) -> Result<Self, Error> {
        // Only the low byte is used for possible future extensions
        match from.read_u16()? & 0xFF {
            0x00 => Ok(Self::Ack(AckPack::deserialize(&mut from)?)),
            0x01 => Ok(Self::Data(DataPack::deserialize(&mut from)?)),
            0x02 => Ok(Self::Message(MessagePack::deserialize(&mut from)?)),
            id => Err(Error::UnexpectedValueError {
                message: format!("Supported packet types are 0..=2, got {id}"),
            })
        }
    }

    pub fn id(&self) -> Option<PacketId> {
        match self {
            Packet::Ack(ack) => Some(ack.acked_id),
            Packet::Data(data) => Some(data.id),
            Packet::Message(_) => None,
        }
    }

    pub fn has_data(&self) -> bool {
        match self {
            Packet::Data(..) => true,
            Packet::Message(..) => true,
            Packet::Ack(..) => false,
        }
    }

    pub fn data(&self) -> Option<&Bytes> {
        use Packet::*;
        match self {
            Ack(..)
                => None,
            Data(DataPack { data, .. }) |
            Message(MessagePack { data, .. })
                => Some(data),
        }
    }

    pub fn data_len(&self) -> usize {
        self.data().map(|b| b.len()).unwrap_or(0)
    }
}

impl std::fmt::Display for Packet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Packet::Ack(pack) => write!(f, "{pack}"),
            Packet::Data(pack) => write!(f, "{pack}"),
            Packet::Message(pack) => write!(f, "{pack}"),
        }
    }
}

impl From<AckPack> for Packet {
    fn from(value: AckPack) -> Self {
        Self::Ack(value)
    }
}

impl From<DataPack> for Packet {
    fn from(value: DataPack) -> Self {
        Self::Data(value)
    }
}

impl From<MessagePack> for Packet {
    fn from(value: MessagePack) -> Self {
        Self::Message(value)
    }
}

