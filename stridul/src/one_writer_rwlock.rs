use std::sync::{RwLock, Arc};

pub fn new_owrw_lock<T>(v: T) -> (RwLockWriter<T>, RwLockReader<T>) {
    let lock = Arc::new(RwLock::new(v));

    let write = RwLockWriter {
        lock: lock.clone(),
    };
    let read = RwLockReader {
        lock: lock.clone(),
    };

    (write, read)
}

#[derive(Default, Debug)]
pub struct RwLockWriter<T> {
    lock: Arc<RwLock<T>>,
}

impl<T> RwLockWriter<T> {
    pub fn write<'a>(&'a self) -> std::sync::RwLockWriteGuard<'a, T> {
        self.lock.write().expect("Lock broken")
    }
    pub fn read<'a>(&'a self) -> std::sync::RwLockReadGuard<'a, T> {
        self.lock.read().expect("Lock broken")
    }
}

#[derive(Default, Debug, Clone)]
pub struct RwLockReader<T> {
    lock: Arc<RwLock<T>>,
}

impl<T> RwLockReader<T> {
    pub fn read<'a>(&'a self) -> std::sync::RwLockReadGuard<'a, T> {
        self.lock.read().expect("Lock broken")
    }
}
