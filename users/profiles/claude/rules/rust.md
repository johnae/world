---
paths:
  - "**/*.rs"
---
# Rust Preferences

## Performance

- Be careful with `.clone()` on hot paths. Prefer Arc or borrowing.
- `Vec::with_capacity()` when size is known. `Cow<'_, str>` when ownership is conditional.
- When using async, wrap blocking work in `spawn_blocking`, never block the async runtime.

## Style

- `#[deny(warnings)]` via RUSTFLAGS in CI, never in source.
- Exhaustive pattern matching: avoid catch-all `_` when possible.
- Prefer iterator pipelines over manual loops.
- All code should be fully optimized: efficient algorithms, DRY, no dead code.
