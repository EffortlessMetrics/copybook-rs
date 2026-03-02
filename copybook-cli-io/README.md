# copybook-cli-io

Single-responsibility crate containing file I/O helpers used by `copybook-cli`:

- Atomic output writes (`atomic_write`)
- Portable file/stdin reads (`read_file_or_stdin`)

This keeps CLI command code focused on business logic while centralizing safe I/O behavior.
