#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

/* automatically generated by rust-bindgen 0.70.1 */

pub const LZ_API_VERSION: u32 = 1015;
pub const LZ_version_string: &[u8; 5] = b"1.15\0";
pub const LZ_Errno_LZ_ok: LZ_Errno = 0;
pub const LZ_Errno_LZ_bad_argument: LZ_Errno = 1;
pub const LZ_Errno_LZ_mem_error: LZ_Errno = 2;
pub const LZ_Errno_LZ_sequence_error: LZ_Errno = 3;
pub const LZ_Errno_LZ_header_error: LZ_Errno = 4;
pub const LZ_Errno_LZ_unexpected_eof: LZ_Errno = 5;
pub const LZ_Errno_LZ_data_error: LZ_Errno = 6;
pub const LZ_Errno_LZ_library_error: LZ_Errno = 7;
pub type LZ_Errno = ::std::os::raw::c_uint;
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LZ_Encoder {
    _unused: [u8; 0],
}
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct LZ_Decoder {
    _unused: [u8; 0],
}
extern "C" {
    pub fn LZ_api_version() -> ::std::os::raw::c_int;
    pub fn LZ_version() -> *const ::std::os::raw::c_char;
    pub fn LZ_strerror(lz_errno: LZ_Errno) -> *const ::std::os::raw::c_char;
    pub fn LZ_min_dictionary_bits() -> ::std::os::raw::c_int;
    pub fn LZ_min_dictionary_size() -> ::std::os::raw::c_int;
    pub fn LZ_max_dictionary_bits() -> ::std::os::raw::c_int;
    pub fn LZ_max_dictionary_size() -> ::std::os::raw::c_int;
    pub fn LZ_min_match_len_limit() -> ::std::os::raw::c_int;
    pub fn LZ_max_match_len_limit() -> ::std::os::raw::c_int;
    pub fn LZ_compress_open(
        dictionary_size: ::std::os::raw::c_int,
        match_len_limit: ::std::os::raw::c_int,
        member_size: ::std::os::raw::c_ulonglong,
    ) -> *mut LZ_Encoder;
    pub fn LZ_compress_close(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_int;
    pub fn LZ_compress_finish(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_int;
    pub fn LZ_compress_restart_member(
        encoder: *mut LZ_Encoder,
        member_size: ::std::os::raw::c_ulonglong,
    ) -> ::std::os::raw::c_int;
    pub fn LZ_compress_sync_flush(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_int;
    pub fn LZ_compress_read(
        encoder: *mut LZ_Encoder,
        buffer: *mut u8,
        size: ::std::os::raw::c_int,
    ) -> ::std::os::raw::c_int;
    pub fn LZ_compress_write(
        encoder: *mut LZ_Encoder,
        buffer: *const u8,
        size: ::std::os::raw::c_int,
    ) -> ::std::os::raw::c_int;
    pub fn LZ_compress_write_size(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_int;
    pub fn LZ_compress_errno(encoder: *mut LZ_Encoder) -> LZ_Errno;
    pub fn LZ_compress_finished(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_int;
    pub fn LZ_compress_member_finished(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_int;
    pub fn LZ_compress_data_position(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_ulonglong;
    pub fn LZ_compress_member_position(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_ulonglong;
    pub fn LZ_compress_total_in_size(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_ulonglong;
    pub fn LZ_compress_total_out_size(encoder: *mut LZ_Encoder) -> ::std::os::raw::c_ulonglong;
    pub fn LZ_decompress_open() -> *mut LZ_Decoder;
    pub fn LZ_decompress_close(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_finish(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_reset(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_sync_to_member(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_read(
        decoder: *mut LZ_Decoder,
        buffer: *mut u8,
        size: ::std::os::raw::c_int,
    ) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_write(
        decoder: *mut LZ_Decoder,
        buffer: *const u8,
        size: ::std::os::raw::c_int,
    ) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_write_size(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_errno(decoder: *mut LZ_Decoder) -> LZ_Errno;
    pub fn LZ_decompress_finished(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_member_finished(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_member_version(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_dictionary_size(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_int;
    pub fn LZ_decompress_data_crc(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_uint;
    pub fn LZ_decompress_data_position(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_ulonglong;
    pub fn LZ_decompress_member_position(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_ulonglong;
    pub fn LZ_decompress_total_in_size(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_ulonglong;
    pub fn LZ_decompress_total_out_size(decoder: *mut LZ_Decoder) -> ::std::os::raw::c_ulonglong;
}
