use std::{net::{SocketAddrV4, SocketAddrV6, SocketAddr}, path::PathBuf};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum AddressInner {
    Ipv4(SocketAddrV4),
    Ipv6(SocketAddrV6),
    NamedPipe(PathBuf),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Address {
    pub(crate) inner: AddressInner,
}

impl From<SocketAddr> for Address {
    fn from(value: SocketAddr) -> Self {
        let inner = match value {
            SocketAddr::V4(x) => AddressInner::Ipv4(x),
            SocketAddr::V6(x) => AddressInner::Ipv6(x),
        };
        Self { inner }
    }
}

impl From<SocketAddrV4> for Address {
    fn from(value: SocketAddrV4) -> Self {
        Self {
            inner: AddressInner::Ipv4(value),
        }
    }
}

impl From<SocketAddrV6> for Address {
    fn from(value: SocketAddrV6) -> Self {
        Self {
            inner: AddressInner::Ipv6(value),
        }
    }
}

impl From<PathBuf> for Address {
    fn from(value: PathBuf) -> Self {
        Self {
            inner: AddressInner::NamedPipe(value),
        }
    }
}

impl TryFrom<Address> for SocketAddr {
    type Error = ();

    fn try_from(value: Address) -> Result<Self, Self::Error> {
        match value.inner {
            AddressInner::Ipv4(a) => Ok(SocketAddr::V4(a)),
            AddressInner::Ipv6(b) => Ok(SocketAddr::V6(b)),

            _ => Err(()),
        }
    }
}
