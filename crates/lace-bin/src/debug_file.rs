use std::fs::File;
use std::path::Path;
use crate::ice::Ice;

pub fn create_debug_file(name: &str) -> File {
    let path = Path::new("debug_output");
    std::fs::create_dir_all(path).unwrap_or_ice("create debug dir");

    File::create(path.join(name)).unwrap_or_ice(&format!("create debug file {}", name))
}