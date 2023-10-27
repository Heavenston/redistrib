
pub trait TryAsRef<T> {
    fn try_as_ref(&self) -> Option<&T>;
}

impl<T, Y> TryAsRef<T> for Y
    where Y: AsRef<T>
{
    fn try_as_ref(&self) -> Option<&T> {
        Some(self.as_ref())
    }
}

pub trait TryAsMut<T> {
    fn try_as_mut(&mut self) -> Option<&mut T>;
}

impl<T, Y> TryAsMut<T> for Y
    where Y: AsMut<T>
{
    fn try_as_mut(&mut self) -> Option<&mut T> {
        Some(self.as_mut())
    }
}
