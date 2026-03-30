---
paths:
  - "**/*.go"
---
# Go Preferences

## Tooling

- golangci-lint with errcheck, staticcheck, gosec enabled
- nilaway (Uber's nil safety analyzer) as additional check - bring in through devenv
- Property-based tests with `pgregory.net/rapid` (build tag: `property`)
- Race detector always on: `-race` flag in GOFLAGS
- Pure Go SQLite via `modernc.org/sqlite` when applicable (no CGO)

## Style

- `log/slog` for structured logging unless project uses something else
- Always validate external inputs (MAC addresses, IPs, config values)
- Deferred Close/Rollback is the one place `_` on error is acceptable
