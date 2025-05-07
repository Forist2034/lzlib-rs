use std::io::{self, Read, Write};

use lzlib::{Level, Param};

/// test partial read and write
struct PartialIo<I>(I);
impl<I: io::Read> io::Read for PartialIo<I> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.0
            .read(if buf.is_empty() { buf } else { &mut buf[0..1] })
    }
}
impl<I: io::BufRead> io::BufRead for PartialIo<I> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        self.0
            .fill_buf()
            .map(|v| if v.is_empty() { v } else { &v[0..1] })
    }
    fn consume(&mut self, amt: usize) {
        self.0.consume(amt)
    }
}
impl<I: io::Write> io::Write for PartialIo<I> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.write(if buf.is_empty() { buf } else { &buf[0..1] })
    }
    fn flush(&mut self) -> io::Result<()> {
        self.0.flush()
    }
}

struct TestCase {
    original: &'static [u8],
    param: Param,
    compressed: &'static [u8],
}
impl TestCase {
    const fn from_level(lvl: Level, original: &'static [u8], compressed: &'static [u8]) -> Self {
        Self {
            original,
            param: Param::from_level_size(lvl, original.len() as u64),
            compressed,
        }
    }
    fn test_compress_reader(self) {
        let mut reader =
            PartialIo(lzlib::read::LzEncoder::new(self.param, PartialIo(self.original)).unwrap());
        let mut ret = Vec::new();
        reader.read_to_end(&mut ret).unwrap();
        assert_eq!(ret, self.compressed)
    }
    fn test_decompress_reader(self) {
        let mut reader =
            PartialIo(lzlib::read::LzDecoder::new(PartialIo(self.compressed)).unwrap());
        let mut ret = Vec::new();
        reader.read_to_end(&mut ret).unwrap();
        assert_eq!(ret, self.original);
    }
    fn test_compress_writer(self) {
        let mut ret = Vec::new();
        let mut writer =
            PartialIo(lzlib::write::LzEncoder::new(self.param, PartialIo(&mut ret)).unwrap());
        writer.write_all(self.original).unwrap();
        writer.0.into_inner().unwrap();
        assert_eq!(ret, self.compressed);
    }
    fn test_decompress_writer(self) {
        let mut ret = Vec::new();
        let mut writer = PartialIo(lzlib::write::LzDecoder::new(PartialIo(&mut ret)).unwrap());
        writer.write_all(self.compressed).unwrap();
        writer.0.into_inner().unwrap();
        assert_eq!(ret, self.original);
    }
}

macro_rules! mk_test {
    ($v:ident, $e:expr) => {
        mod $v {
            const TEST: crate::TestCase = $e;
            mod read {
                #[test]
                fn encoder() {
                    super::TEST.test_compress_reader();
                }
                #[test]
                fn decoder() {
                    super::TEST.test_decompress_reader();
                }
            }
            mod write {
                #[test]
                fn encoder() {
                    super::TEST.test_compress_writer()
                }
                #[test]
                fn decoder() {
                    super::TEST.test_decompress_writer()
                }
            }
        }
    };
}

macro_rules! mk_level_test {
    ($lvl:literal, $original:expr, $compress_base:literal) => {
        paste::paste!(
            mk_test!(
                [< level $lvl >],
                crate::TestCase::from_level(
                    lzlib::Level::[< Level $lvl >],
                    $original,
                    include_bytes!(concat!($compress_base, "/level_", $lvl, ".lz"))
                )
            );
        );
    };
}

// change dictionary size to level0 will change encoder algorithm,
// so not test level0 code yet
macro_rules! mk_level_tests {
    ($name:ident, $original:expr, $compress_base: literal) => {
        mod $name {
            const ORIGINAL: &[u8] = $original;

            // mk_level_test!(0, super::ORIGINAL, $compress_base);
            mk_level_test!(1, super::ORIGINAL, $compress_base);
            mk_level_test!(2, super::ORIGINAL, $compress_base);
            mk_level_test!(3, super::ORIGINAL, $compress_base);
            mk_level_test!(4, super::ORIGINAL, $compress_base);
            mk_level_test!(5, super::ORIGINAL, $compress_base);
            mk_level_test!(6, super::ORIGINAL, $compress_base);
            mk_level_test!(7, super::ORIGINAL, $compress_base);
            mk_level_test!(8, super::ORIGINAL, $compress_base);
            mk_level_test!(9, super::ORIGINAL, $compress_base);
        }
    };
}

mk_level_tests!(empty, b"", "./data/empty.lz");
mk_level_tests!(
    test,
    include_bytes!("./data/test.txt"),
    "./data/test.txt.lz"
);
mk_level_tests!(
    fox_lf,
    include_bytes!("./data/fox_lf.txt"),
    "./data/fox_lf.txt.lz"
);
