use std::{
    ffi::{c_int, c_ulonglong},
    fmt::Display,
    ptr,
};

pub mod read;
pub mod write;

#[derive(Debug)]
#[non_exhaustive]
pub enum Error {
    BadArgument,
    MemError,
    SequenceError,
    HeaderError,
    UnexpectedEof,
    DataError,
    LibraryError,
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::BadArgument => "Bad argument",
            Self::MemError => "Not enough memory",
            Self::SequenceError => "Sequence Error",
            Self::HeaderError => "Header error",
            Self::UnexpectedEof => "Unexpected EOF",
            Self::DataError => "Data error",
            Self::LibraryError => "Library error",
        })
    }
}
impl std::error::Error for Error {}
impl Error {
    fn from_errno(errno: lzlib_sys::LZ_Errno) -> Self {
        match errno {
            lzlib_sys::LZ_Errno_LZ_bad_argument => Self::BadArgument,
            lzlib_sys::LZ_Errno_LZ_mem_error => Self::MemError,
            lzlib_sys::LZ_Errno_LZ_sequence_error => Self::SequenceError,
            lzlib_sys::LZ_Errno_LZ_header_error => Self::HeaderError,
            lzlib_sys::LZ_Errno_LZ_unexpected_eof => Self::UnexpectedEof,
            lzlib_sys::LZ_Errno_LZ_data_error => Self::DataError,
            lzlib_sys::LZ_Errno_LZ_library_error => Self::LibraryError,
            _ => unreachable!(),
        }
    }
}

macro_rules! try_compress {
    ($d:expr, $f:path, ($($e:expr),*)) => {
        match unsafe {$f($d $(,$e)*)} {
            -1 => return Err(Error::from_errno(unsafe {
                lzlib_sys::LZ_compress_errno($d)
            })),
            n => n
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Level {
    Level0,
    Level1,
    Level2,
    Level3,
    Level4,
    Level5,
    #[default]
    Level6,
    Level7,
    Level8,
    Level9,
}
impl Level {
    pub const FAST: Self = Self::Level0;
    pub const BEST: Self = Self::Level9;
    pub const DEFAULT: Self = Self::Level6;
}

const KIB: u32 = 10;
const MIB: u32 = 20;

///
#[derive(Debug, Clone, Copy)]
pub struct Param {
    /** sets the dictionary size to be used, in bytes.
    Valid values range from 4 KiB to 512 MiB. Note that dictionary
    sizes are quantized. If the size specified does not match one of the
    valid sizes, it is rounded upwards by adding up to
    `dictionary_size / 8` to it. */
    pub dictionary_size: u32,
    /** sets the match length limit in bytes. Valid values
       range from 5 to 273. Larger values usually give better compression ratios
       but longer compression times.

    If dictionary_size is 65535 and match_len_limit is 16, the fast
       variant of LZMA is chosen, which produces identical compressed output as
       `lzip -0`. (The dictionary size used is rounded upwards to
       64 KiB).
    */
    pub match_len_limit: u16,
    /** Sets the member size limit in bytes. Valid values range
    from 4 KiB to 2 PiB. A small member size may degrade compression
    ratio, so use it only when needed. To produce a single-member data stream,
    give member_size a value larger than the amount of data to be
    produced. Values larger than 2 PiB are reduced to 2 PiB to prevent
    the uncompressed size of the member from overflowing. */
    pub member_size: u64,
}
impl Param {
    pub const MIN_DICTIONARY_BITS: u8 = 12;
    pub const MAX_DICTIONARY_BITS: u8 = 29;
    pub const MIN_DICTIONARY_SIZE: u32 = 4 << KIB;
    pub const MAX_DICTIONARY_SIZE: u32 = 512 << MIB;
    pub const MIN_MATCH_LEN_LIMIT: u16 = 5;
    pub const MAX_MATCH_LEN_LIMIT: u16 = 273;
    pub const MIN_MEMBER_SIZE: u64 = 4 << KIB;
    pub const MAX_MEMBER_SIZE: u64 = 2 << 50;

    pub const fn from_level(lvl: Level) -> Self {
        const fn new(dictionary_size: u32, match_len_limit: u16) -> Param {
            Param {
                dictionary_size,
                match_len_limit,
                member_size: Param::MAX_MEMBER_SIZE,
            }
        }
        match lvl {
            Level::Level0 => new(64 << KIB, 16),
            Level::Level1 => new(1 << MIB, 5),
            Level::Level2 => new(/* 1.5 MiB */ 3 << (MIB - 1), 6),
            Level::Level3 => new(2 << MIB, 8),
            Level::Level4 => new(3 << MIB, 12),
            Level::Level5 => new(4 << MIB, 20),
            Level::Level6 => new(8 << MIB, 36),
            Level::Level7 => new(16 << MIB, 68),
            Level::Level8 => new(24 << MIB, 132),
            Level::Level9 => new(32 << MIB, 273),
        }
    }
    pub const fn from_level_size(lvl: Level, size: u64) -> Self {
        if let Level::Level0 = lvl {
            Self::from_level(Level::Level0)
        } else {
            let mut ret = Self::from_level(lvl);
            ret.dictionary_size = if ret.dictionary_size as u64 > size {
                let size32 = size as u32;
                if size32 < Param::MIN_DICTIONARY_SIZE {
                    Param::MIN_DICTIONARY_SIZE
                } else {
                    size32
                }
            } else {
                ret.dictionary_size
            };
            ret
        }
    }
}

trait Codec {
    fn lzip_finish(&mut self);
    fn lzip_finished(&mut self) -> bool;
    fn lzip_read(&mut self, buf: &mut [u8]) -> Result<usize, Error>;
    fn lzip_write(&mut self, buf: &[u8]) -> Result<usize, Error>;
}

pub struct Encoder(*mut lzlib_sys::LZ_Encoder);
impl Drop for Encoder {
    fn drop(&mut self) {
        unsafe {
            lzlib_sys::LZ_compress_close(self.0);
        }
    }
}
impl Encoder {
    pub fn new(param: Param) -> Result<Self, Error> {
        unsafe {
            let ret = lzlib_sys::LZ_compress_open(
                param.dictionary_size as c_int,
                param.match_len_limit as c_int,
                param.member_size as c_ulonglong,
            );
            match lzlib_sys::LZ_compress_errno(ret) {
                lzlib_sys::LZ_Errno_LZ_ok => Ok(Self(ret)),
                e => {
                    lzlib_sys::LZ_compress_close(ret);
                    Err(Error::from_errno(e))
                }
            }
        }
    }

    /** Use this function to tell 'lzlib' that all the data for this member
    have already been written (with the function [Self::write]). It
    is safe to call [Self::finish] as many times as needed. After
    all the compressed data have been read with [Self::read] and
    [Self::member_finished] returns [true], a new member can be started
    with [Self::restart_member].  */
    pub fn finish(&mut self) {
        unsafe {
            lzlib_sys::LZ_compress_finish(self.0);
        }
    }
    /** Use this function to start a new member in a multimember data stream.
    Call this function only after [Self::member_finished] indicates
    that the current member has been fully read (with the function
    [Self::read]). */
    pub fn restart_member(&mut self, member_size: u64) -> Result<(), Error> {
        try_compress!(self.0, lzlib_sys::LZ_compress_restart_member, (member_size));
        Ok(())
    }
    /** Use this function to make available to [Self::read] all the data
    already written with the function [Self::write]. First call
    [Self::sync_flush]. Then call [Self::read] until it
    returns 0.

    This function writes at least one LZMA marker '3' ('Sync Flush' marker)
    to the compressed output. Note that the sync flush marker is not
    allowed in lzip files; it is a device for interactive communication
    between applications using lzlib, but is useless and wasteful in a
    file, and is excluded from the media type `application/lzip`. The LZMA
    marker '2' ('End Of Stream' marker) is the only marker allowed in lzip
    files.

    Repeated use of [Self::sync_flush] may degrade compression
    ratio, so use it only when needed. If the interval between calls to
    [Self::sync_flush] is large (comparable to dictionary size),
    creating a multimember data stream with [Self::restart_member]
    may be an alternative.

    Combining multimember stream creation with flushing may be tricky. If
    there are more bytes available than those needed to complete
    MEMBER_SIZE, [Self::restart_member] needs to be called when
    [Self::member_finished] returns [true], followed by a new call to
    [Self::sync_flush]. */
    pub fn sync_flush(&mut self) -> Result<(), Error> {
        try_compress!(self.0, lzlib_sys::LZ_compress_sync_flush, ());
        Ok(())
    }
    /** Returns the number of bytes actually read. This might be less than
    SIZE; for example, if there aren't that many bytes left in the stream
    or if more bytes have to be yet written with the function
    [Self::write]. Note that reading less than SIZE bytes is not an
    error. */
    pub fn read(&mut self, buf: &mut [u8]) -> Result<usize, Error> {
        Ok(try_compress!(
            self.0,
            lzlib_sys::LZ_compress_read,
            (buf.as_mut_ptr(), buf.len() as c_int)
        ) as usize)
    }
    pub fn skip(&mut self, len: usize) -> Result<usize, Error> {
        Ok(try_compress!(
            self.0,
            lzlib_sys::LZ_compress_read,
            (ptr::null_mut(), len as c_int)
        ) as usize)
    }
    /**  Writes up to SIZE bytes from BUFFER to the stream pointed to by
    ENCODER. Returns the number of bytes actually written. This might be
    less than SIZE. Note that writing less than SIZE bytes is not an error. */
    pub fn write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        Ok(try_compress!(
            self.0,
            lzlib_sys::LZ_compress_write,
            (buf.as_ptr(), buf.len() as c_int)
        ) as usize)
    }
    /** Returns the maximum number of bytes that can be immediately written
    through [Self::write]. For efficiency reasons, once the input
    buffer is full and [Self::write_size] returns 0, almost all the
    buffer must be compressed before a size greater than 0 is returned
    again. (This is done to minimize the amount of data that must be
    copied to the beginning of the buffer before new data can be accepted).

    It is guaranteed that an immediate call to [Self::write] will
    accept a SIZE up to the returned number of bytes. */
    pub fn write_size(&self) -> u64 {
        unsafe { lzlib_sys::LZ_compress_write_size(self.0) as u64 }
    }
    /** Returns [true] if all the data have been read and self can
    be safely dropped. Otherwise it returns [false]. [Self::finished]
    implies [Self::member_finished]. */
    pub fn finished(&self) -> bool {
        unsafe { lzlib_sys::LZ_compress_finished(self.0) != 0 }
    }
    /** Returns [true] if the current member, in a multimember data stream, has been
    fully read and [Self::restart_member] can be safely called.
    Otherwise it returns [false]. */
    pub fn member_finished(&self) -> bool {
        unsafe { lzlib_sys::LZ_compress_member_finished(self.0) != 0 }
    }
    /** Returns the number of input bytes already compressed in the current
    member. */
    pub fn data_position(&self) -> u64 {
        unsafe { lzlib_sys::LZ_compress_data_position(self.0) as u64 }
    }
    /** Returns the number of compressed bytes already produced, but perhaps
    not yet read, in the current member. */
    pub fn member_position(&self) -> u64 {
        unsafe { lzlib_sys::LZ_compress_member_position(self.0) as u64 }
    }
    /// Returns the total number of input bytes already compressed.
    pub fn total_in_size(&self) -> u64 {
        unsafe { lzlib_sys::LZ_compress_total_in_size(self.0) as u64 }
    }
    /// Returns the total number of compressed bytes already produced, but perhaps not yet read.
    pub fn total_out_size(&self) -> u64 {
        unsafe { lzlib_sys::LZ_compress_total_out_size(self.0) as u64 }
    }
}
impl Codec for Encoder {
    #[inline]
    fn lzip_finish(&mut self) {
        self.finish()
    }
    #[inline]
    fn lzip_finished(&mut self) -> bool {
        self.finished()
    }
    #[inline]
    fn lzip_read(&mut self, buf: &mut [u8]) -> Result<usize, Error> {
        self.read(buf)
    }
    #[inline]
    fn lzip_write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        self.write(buf)
    }
}

macro_rules! try_decompress {
    ($d:expr, $i:path, ($($e:expr),*)) => {
        match unsafe {$i($d $(,$e)*)} {
            -1 => return Err(Error::from_errno(unsafe {
                lzlib_sys::LZ_decompress_errno($d)
            })),
            n => n
        }
    };
}

pub struct Decoder(*mut lzlib_sys::LZ_Decoder);
impl Drop for Decoder {
    fn drop(&mut self) {
        unsafe { lzlib_sys::LZ_decompress_close(self.0) };
    }
}
impl Decoder {
    pub fn new() -> Result<Self, Error> {
        unsafe {
            let ret = lzlib_sys::LZ_decompress_open();
            match lzlib_sys::LZ_decompress_errno(ret) {
                lzlib_sys::LZ_Errno_LZ_ok => Ok(Self(ret)),
                err => {
                    lzlib_sys::LZ_decompress_close(ret);
                    Err(Error::from_errno(err))
                }
            }
        }
    }
    /**  Use this function to tell 'lzlib' that all the data for this stream
    have already been written (with the function [Self::write]).
    It is safe to call [Self::finish] as many times as needed. It
    is not required to call [Self::finish] if the input stream
    only contains whole members, but not calling it prevents lzlib from
    detecting a truncated member. */
    pub fn finish(&mut self) {
        unsafe {
            lzlib_sys::LZ_decompress_finish(self.0);
        }
    }
    /** Resets the internal state of DECODER as it was just after opening it
    with the function [Self::new]. Data stored in the internal
    buffers are discarded. Position counters are set to 0. */
    pub fn reset(&mut self) -> Result<(), Error> {
        try_decompress!(self.0, lzlib_sys::LZ_decompress_reset, ());
        Ok(())
    }
    /** Resets the error state of DECODER and enters a search state that lasts
    until a new member header (or the end of the stream) is found. After a
    successful call to [Self::sync_to_member], data written with
    [Self::write] is consumed and [Self::read] returns 0
    until a header is found.

    This function is useful to discard any data preceding the first
    member, or to discard the rest of the current member, for example in
    case of a data error. If the decoder is already at the beginning of a
    member, this function does nothing. */
    pub fn sync_to_member(&mut self) -> Result<(), Error> {
        try_decompress!(self.0, lzlib_sys::LZ_decompress_sync_to_member, ());
        Ok(())
    }
    /** Returns the number of bytes actually read. This might be less than
    SIZE; for example, if there aren't that many bytes left in the stream
    or if more bytes have to be yet written with the function
    [Self::write]. Note that reading less than SIZE bytes is not
    an error.

    [Self::read] returns at least once per member so that
    [Self::member_finished] can be called (and trailer data
    retrieved) for each member, even for empty members. Therefore,
    [Self::read] returning 0 does not mean that the end of the
    stream has been reached. The increase in the value returned by
    [Self::total_in_size] can be used to tell the end of the stream
    from an empty member.

    In case of decompression error caused by corrupt or truncated data,
    [Self::read] does not signal the error immediately to the
    application, but waits until all the bytes decoded have been read. This
    allows tools like tarlz to recover as much data as possible from each
    damaged member. */
    pub fn read(&mut self, buf: &mut [u8]) -> Result<usize, Error> {
        Ok(try_decompress!(
            self.0,
            lzlib_sys::LZ_decompress_read,
            (buf.as_mut_ptr(), buf.len() as c_int)
        ) as usize)
    }
    /// Discard len data
    pub fn skip(&mut self, len: usize) -> Result<usize, Error> {
        Ok(try_decompress!(
            self.0,
            lzlib_sys::LZ_decompress_read,
            (ptr::null_mut(), len as c_int)
        ) as usize)
    }
    /** Writes up to SIZE bytes from BUFFER to the stream pointed to by
    DECODER. Returns the number of bytes actually written. This might be
    less than SIZE. Note that writing less than SIZE bytes is not an error.  */
    pub fn write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        Ok(try_decompress!(
            self.0,
            lzlib_sys::LZ_decompress_write,
            (buf.as_ptr(), buf.len() as c_int)
        ) as usize)
    }
    /** Returns the maximum number of bytes that can be immediately written
    through [Self::write]. This number varies smoothly; each
    compressed byte consumed may be overwritten immediately, increasing by
    1 the value returned. */
    pub fn write_size(&self) -> usize {
        unsafe { lzlib_sys::LZ_decompress_write_size(self.0) as usize }
    }
    /** Returns [true] if all the data have been read and can
    be safely dropped. Otherwise it returns [false]. [Self::finished]
    does not imply [Self::member_finished]. */
    pub fn finished(&self) -> bool {
        unsafe { lzlib_sys::LZ_decompress_finished(self.0) != 0 }
    }
    /** Returns [true] if the previous call to [Self::read] finished reading
    the current member, indicating that final values for the member are
    available through [Self::data_crc],
    [Self::data_position], and [Self::member_position].
    Otherwise it returns [false]. */
    pub fn member_finished(&self) -> bool {
        unsafe { lzlib_sys::LZ_decompress_member_finished(self.0) != 0 }
    }
    /// Returns the version of the current member, read from the member header.
    pub fn member_version(&self) -> u8 {
        unsafe { lzlib_sys::LZ_decompress_member_version(self.0) as u8 }
    }
    /// Returns the dictionary size of the current member, read from the member header.
    pub fn dictionary_size(&self) -> u32 {
        unsafe { lzlib_sys::LZ_decompress_dictionary_size(self.0) as u32 }
    }
    /** Returns the 32 bit Cyclic Redundancy Check of the data decompressed
    from the current member. The value returned is valid only when
    [Self::member_finished] returns [true]. */
    pub fn data_crc(&self) -> u32 {
        unsafe { lzlib_sys::LZ_decompress_data_crc(self.0) as u32 }
    }
    /// Returns the number of decompressed bytes already produced, but perhaps not yet read, in the current member.
    pub fn data_position(&self) -> u64 {
        unsafe { lzlib_sys::LZ_decompress_data_position(self.0) as u64 }
    }
    /// Returns the number of input bytes already decompressed in the current member.
    pub fn member_position(&self) -> u64 {
        unsafe { lzlib_sys::LZ_decompress_member_position(self.0) as u64 }
    }
    /// Returns the total number of input bytes already decompressed.
    pub fn total_in_size(&self) -> u64 {
        unsafe { lzlib_sys::LZ_decompress_total_in_size(self.0) as u64 }
    }
    /// Returns the total number of decompressed bytes already produced, but perhaps not yet read.
    pub fn total_out_size(&self) -> u64 {
        unsafe { lzlib_sys::LZ_decompress_total_out_size(self.0) as u64 }
    }
}
impl Codec for Decoder {
    #[inline]
    fn lzip_finish(&mut self) {
        self.finish()
    }
    #[inline]
    fn lzip_finished(&mut self) -> bool {
        self.finished()
    }

    #[inline]
    fn lzip_read(&mut self, buf: &mut [u8]) -> Result<usize, Error> {
        self.read(buf)
    }
    #[inline]
    fn lzip_write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        self.write(buf)
    }
}
