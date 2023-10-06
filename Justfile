default:
  just check

check:
  cargo fmt --check
  cargo clippy
  cargo test

fmt:
  cargo fmt

clippy:
  cargo clippy
