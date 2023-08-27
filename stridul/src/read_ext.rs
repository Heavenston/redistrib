use std::{io::{ Write, Read, Result }, mem::MaybeUninit};

use bytes::BufMut;

macro_rules! read_int {
    ($name: ident, $int: ident) => {
        fn $name(&mut self) -> Result<$int> {
            let mut buf = [0u8; ($int::BITS / 8) as usize];
            self.read_exact(&mut buf)?;
            Ok($int::from_be_bytes(buf))
        }
    };
}

pub trait ReadExt: Read {
    read_int!(read_u8 , u8);
    read_int!(read_u16, u16);
    read_int!(read_u32, u32);
    read_int!(read_u64, u64);
    read_int!(read_u128, u128);
    read_int!(read_i8 , i8);
    read_int!(read_i16, i16);
    read_int!(read_i32, i32);
    read_int!(read_i64, i64);
    read_int!(read_i128, i128);

    /// Version of Read::read_to_end for reading into a Writer
    fn pipe_to_end<W: Write>(&mut self, into: &mut W) -> Result<usize> {
        let mut n = 0;
        let mut buf = [0u8; 2048];
        loop {
            let read = self.read(&mut buf)?;
            if read == 0 { break; }
            n += read;
            into.write_all(&buf[0..read])?;
        }
        return Ok(n);
    }

    /// Version of Read::read_to_end for reading into a BufMut, do not use
    /// this if the buf is a Vec, use the more optimized especially for
    /// reallocations Read::read_to_end
    #[deprecated = "Bugged, use Read::read_to_end"]
    fn buf_to_end<B: BufMut>(&mut self, into: &mut B) -> Result<usize> {
        let mut n = 0;
        loop {
            // I guessed the "undefined behaior" of slice_assume_init_mut
            // here is only applicable for reading the values so
            // this should be safe......................................

            // I also consider that BufMut::chunk_mut won't return a slice of
            // 0 unless there is no capacity left which is not specified so
            // may be lead to the buf not write to the end
            let mut buf = unsafe {
                MaybeUninit::slice_assume_init_mut(
                    into.chunk_mut().as_uninit_slice_mut()
                )
            };

            let read = self.read(&mut buf)?;
            if read == 0 { break; }
            n += read;
            unsafe { buf.advance_mut(read) };
        }
        return Ok(n);
    }
}

impl<T: Read> ReadExt for T
{ }

macro_rules! write_int {
    ($name: ident, $int: ident) => {
        fn $name(&mut self, int: $int) -> Result<()> {
            self.write_all(&$int::to_be_bytes(int))?;
            Ok(())
        }
    };
}

pub trait WriteExt: Write {
    write_int!(write_u8 , u8);
    write_int!(write_u16, u16);
    write_int!(write_u32, u32);
    write_int!(write_u64, u64);
    write_int!(write_u128, u128);
    write_int!(write_i8 , i8);
    write_int!(write_i16, i16);
    write_int!(write_i32, i32);
    write_int!(write_i64, i64);
    write_int!(write_i128, i128);
}

impl<T: Write> WriteExt for T
{ }
