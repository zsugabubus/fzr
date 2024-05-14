# fzr

fzr is a command-line fuzzy finder.

## Features

- UTF-8 only.
- Case-insensitive ASCII matching: o matches o/O; O matches O only.
- Basic Unicode romanization: O matches Ő, 𝑶, 🅾… etc.
- ANSI colors: `\033[1;31mbold and red\033[m`.
- Mouse support.
- Non-fuzzy search: `'exact ^... $... !^...|!...$`.
- Sane ranking algorithm (based on an idiot's opinion).
- Minimal configuration.

## Installation

### Building from source

fzr is written in [Rust](https://www.rust-lang.org/).

```sh
git clone https://github.com/zsugabubus/fzr
cd fzr
cargo build --release

# Install for local user
cp target/release/fzr-cli ~/.local/bin/fzr

# Install for all users
cp target/release/fzr-cli /usr/local/bin/fzr
```
