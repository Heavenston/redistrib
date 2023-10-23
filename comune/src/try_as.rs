
pub trait TryAsRef<T> {
    fn try_as_ref(&self) -> Option<&T>;
}

pub trait TryAsMut<T> {
    fn try_as_mut(&mut self) -> Option<&mut T>;
}
