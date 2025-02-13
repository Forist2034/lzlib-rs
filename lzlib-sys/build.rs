use std::{
    env,
    path::{Path, PathBuf},
};

fn main() {
    let dst = PathBuf::from(env::var("OUT_DIR").unwrap());

    build_lzlib(&dst);
}

fn build_lzlib(dst: &Path) {
    let mut cfg = cc::Build::new();
    cfg.include("lzlib")
        .file("./lzlib/lzlib.c")
        .opt_level(2)
        .out_dir(dst.join("lib"))
        .compile("lzlib");
    println!("cargo::rustc-link-lib=lzlib");
}
