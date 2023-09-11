use std::net::SocketAddr;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord,
    serde::Serialize, serde::Deserialize
)]
#[repr(u8)]
pub enum RegionDirection {
    Low  = 0,
    High = 1,
}

impl From<bool> for RegionDirection {
    fn from(value: bool) -> Self {
        if value
        { Self::High } else
        { Self::Low }
    }
}

impl TryFrom<u8> for RegionDirection {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Low),
            1 => Ok(Self::High),
            _ => Err(()),
        }
    }
}

#[derive(
    Debug, Clone, serde::Serialize, serde::Deserialize, enum_kinds::EnumKind
)]
#[enum_kind(PacketKind, derive(serde::Serialize, serde::Deserialize))]
pub enum Packet {
    TopLevelRequest,
    TopLevelResponse {
        address: SocketAddr,
    },

    NewRegionRequest,
    NewRegionResponse {
        forward: Option<SocketAddr>,
        direction: RegionDirection,
    },
}

impl Packet {
    pub fn kind(&self) -> PacketKind {
        self.into()
    }
}
