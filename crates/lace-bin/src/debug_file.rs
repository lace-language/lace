use std::fs::File;
use std::path::Path;
use crate::lice::Lice;

pub fn create_debug_file(name: &str) -> File {
    let path = Path::new("debug_output");
    std::fs::create_dir_all(path).unwrap_or_lice("create debug dir");

    File::create(path.join(name)).unwrap_or_lice(&format!("create debug file {}", name))
}