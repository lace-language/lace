default:
  just check

check:
  cargo fmt --check
  just clippy
  cargo test

fmt:
  cargo fmt

clippy:
  cargo clippy --all -- -D warnings
