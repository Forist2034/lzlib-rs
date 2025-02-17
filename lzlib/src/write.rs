use std::io;

use crate::Param;

const BUF_SIZE: usize = 16384;

struct CodecWriter<C, W> {
    codec: C,
    writer: W,
    buf: Box<[u8; BUF_SIZE]>,
}
impl<C: super::Codec, W: io::Write> CodecWriter<C, W> {
    fn write_buf(&mut self) -> io::Result<()> {
        let rdr_len = self
            .codec
            .lzip_read(self.buf.as_mut_slice())
            .map_err(io::Error::other)?;
        self.writer.write_all(&self.buf[0..rdr_len])
    }
    fn into_inner(mut self) -> io::Result<W> {
        self.codec.lzip_finish();
        while !self.codec.lzip_finished() {
            self.write_buf()?;
        }
        Ok(self.writer)
    }
}
impl<C: super::Codec, W: io::Write> io::Write for CodecWriter<C, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }
        loop {
            match self.codec.lzip_write(buf) {
                Ok(0) => self.write_buf()?,
                Ok(n) => {
                    self.write_buf()?;
                    break Ok(n);
                }
                Err(e) => break Err(io::Error::other(e)),
            }
        }
    }
    fn flush(&mut self) -> io::Result<()> {
        self.write_buf()
    }
}

pub struct LzEncoder<W>(CodecWriter<super::Encoder, W>);
impl<W: io::Write> LzEncoder<W> {
    pub fn new(param: Param, writer: W) -> Result<Self, super::Error> {
        Ok(Self(CodecWriter {
            codec: super::Encoder::new(param)?,
            writer,
            buf: Box::new([0; BUF_SIZE]),
        }))
    }
    pub fn restart_member(&mut self, member_size: u64) -> io::Result<()> {
        while !self.0.codec.member_finished() {
            self.0.write_buf()?;
        }
        self.0
            .codec
            .restart_member(member_size)
            .map_err(io::Error::other)
    }
    pub fn into_inner(self) -> io::Result<W> {
        self.0.into_inner()
    }
}
impl<W: io::Write> io::Write for LzEncoder<W> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }
    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

pub struct LzDecoder<W>(CodecWriter<super::Decoder, W>);
impl<W> LzDecoder<W> {
    pub fn new(writer: W) -> Result<Self, super::Error> {
        Ok(Self(CodecWriter {
            codec: super::Decoder::new()?,
            writer,
            buf: Box::new([0; BUF_SIZE]),
        }))
    }
}
impl<W: io::Write> LzDecoder<W> {
    pub fn into_inner(self) -> io::Result<W> {
        self.0.into_inner()
    }
}
impl<W: io::Write> io::Write for LzDecoder<W> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(buf)
    }
    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}
