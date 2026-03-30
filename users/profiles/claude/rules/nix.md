---
paths:
  - "**/*.nix"
---
## Nix conventions
- Never use `with` at top level. Use `inherit (pkgs)` or explicit `pkgs.foo`.
- Never use `rec { }`. Use `let ... in` or named self-reference.
- Never use `<nixpkgs>` or lookup paths. Always use flake inputs.
- Never use `flake-utils` or `flake-parts`. Use `lib.genAttrs` over supported systems.
- Avoid IFD (Import From Derivation) in flakes.
- Prefer language-specific builders: `buildGoModule`, `crane`, `buildNpmPackage` over generic `mkDerivation`.
- Format with `nixfmt-rfc-style` conventions.
- When importing nixpkgs, always set `config = {}; overlays = [];` unless overlays are explicitly needed.
