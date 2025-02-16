use std::io;

use crate::{Codec, Error};

struct CodecReader<C, R> {
    codec: C,
    reader: R,
}
impl<C: Codec, R: io::BufRead> io::Read for CodecReader<C, R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }
        loop {
            match self.codec.lzip_read(buf) {
                Ok(0) if self.codec.lzip_finished() => break Ok(0),
                Ok(0) => {
                    let read_buf = self.reader.fill_buf()?;
                    if read_buf.is_empty() {
                        self.codec.lzip_finish()
                    } else {
                        let sz = self.codec.lzip_write(read_buf).map_err(io::Error::other)?;
                        self.reader.consume(sz)
                    }
                }
                Ok(n) => break Ok(n),
                Err(e) => break Err(io::Error::other(e)),
            }
        }
    }
}

pub struct LzEncoder<R>(CodecReader<super::Encoder, R>);
impl<R> LzEncoder<R> {
    pub fn new(param: super::Param, reader: R) -> Result<Self, Error> {
        Ok(Self(CodecReader {
            codec: super::Encoder::new(param)?,
            reader,
        }))
    }
}
impl<R: io::BufRead> io::Read for LzEncoder<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
}

pub struct LzDecoder<R>(CodecReader<super::Decoder, R>);
impl<R> LzDecoder<R> {
    pub fn new(reader: R) -> Result<Self, Error> {
        Ok(Self(CodecReader {
            codec: super::Decoder::new()?,
            reader,
        }))
    }
}
impl<R: io::BufRead> io::Read for LzDecoder<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0.read(buf)
    }
}
