# 🚀 Declarative Today. Utopia Tomorrow.

This is my [NixOS](https://nixos.org) configuration repository, managing all my machines and custom packages. In a way, this repo is my world—hence the name. 🌍

It's built on [Nix flakes](https://nixos.wiki/wiki/Flakes), providing a declarative and reproducible way to manage systems—kind of like `Cargo.toml` (Rust), `go.mod` (Go), or `package.json` (Node.js), but **language-agnostic** and handling any package or file. This setup ensures **deterministic** builds for both hosts and software.

For modularity, [flake-parts](https://flake.parts) is used to keep [flake.nix](./flake.nix) clean and structured. 🛠️

## ✨ Key Features

- **Multi-platform**: Manages both NixOS (Linux) and nix-darwin (macOS) systems
- **Stateless by default**: Ephemeral root filesystems with persistent data management
- **Automated updates**: CI-driven updates with smart reboot coordination
- **Encrypted secrets**: Age-encrypted secrets with automatic deployment
- **Remote unlock**: SSH-based LUKS unlock for headless encrypted servers
- **Modular design**: Reusable profiles for different machine roles
- **Comprehensive backups**: Automated Restic backups to multiple destinations

## 📁 Repository Structure

```
world/
├── hosts/          # Machine-specific configurations
├── profiles/       # Reusable system profiles (server, desktop, laptop)
├── modules/        # Custom NixOS/nix-darwin modules
├── users/          # User configurations (home-manager)
├── packages/       # Custom packages not in nixpkgs
├── secrets/        # Encrypted configuration (agenix)
├── flake/          # Flake components (organized with flake-parts)
└── flake.nix       # Main flake definition
```

For detailed development guidelines, see [DEVELOPMENT.md](./DEVELOPMENT.md).

## 🚀 Quick Start

### Prerequisites

Enable Nix flakes:
```bash
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

### Common Operations

This repo includes a `world` CLI for common tasks:

```bash
# Search for packages
world search <package>

# Open shell with packages
world shell <packages>

# Update system
world upgrade

# Build without switching
world build

# Garbage collect
world gc

# Run linters
world lint

# Check flake
world check
```

## 💻 System Management

### Update Existing System

From GitHub using world cli:
```bash
world upgrade
```

From local clone using world cli:
```bash
world upgrade .
```

From GitHub using nix cli:
```bash
nixos-rebuild switch --flake github:johnae/world --use-remote-sudo
```

From local clone using nix cli:
```bash
nixos-rebuild switch --flake . --use-remote-sudo
```

### Install New System

1. Create host configuration in `hosts/<arch>/<hostname>.nix`
2. Boot NixOS installer and enable flakes
3. Clone this repository
4. Install:

```bash
host=yourhostname
nix build .#"$host"-diskformat
./result/bin/diskformat
nixos-install --flake .#"$host" --no-root-passwd
```

## 🏗️ Architecture

The configuration follows a layered approach:

1. **Base**: Core packages and settings (`profiles/defaults.nix`)
2. **Profile**: Role-specific configs (server, desktop, workstation)
3. **Host**: Machine-specific settings and hardware
4. **User**: Personal configurations via home-manager

### Key Design Choices

- **Impermanence**: Root filesystem is tmpfs, only declared paths persist
- **Declarative storage**: Disk formatting is part of the configuration
- **Profile inheritance**: Hosts compose functionality from profiles
- **Unified tooling**: Same commands work across NixOS and macOS

## 🛠️ Customization

To adapt this configuration:

1. **Start small**: Pick one host configuration as a template
2. **Use profiles**: Leverage existing profiles rather than host-specific configs
3. **Keep secrets out**: Use agenix for sensitive data
4. **Test locally**: Use `world build` before switching

### Common Modifications

- **New machine**: Copy similar host file, adjust hardware config
- **New service**: Add to appropriate profile or create new module
- **User settings**: Modify configurations under `users/profiles/`

## 🔒 Security Features

- **Encrypted disks**: LUKS encryption with remote unlock capability
- **Secrets management**: Age-encrypted secrets, SSH key based
- **Secure boot**: Where supported by hardware
- **Firewall**: Enabled by default with explicit port management

## 🤖 Automation

- **CI/CD**: Buildkite pipeline tests all configurations
- **Dependency updates**: Automated flake input updates
- **Smart reboots**: Coordinated reboots for kernel updates
- **Backup verification**: Automated backup integrity checks

## 🧪 Experimental Features

This repo includes some experimental work like MicroVM configurations for container workloads, but these aren't actively used and may not be fully functional.

## 📚 Resources

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Nix Pills](https://nixos.org/guides/nix-pills/)
- [Home Manager](https://github.com/nix-community/home-manager)
- [Agenix](https://github.com/ryantm/agenix)

## 📜 License

[MIT](LICENSE)

---

*Note: This is a personal configuration that's highly customized to my workflow. Feel free to browse and borrow ideas, but you'll want to adapt it significantly for your own use.*
