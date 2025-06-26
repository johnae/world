# Development Guidelines

This document provides guidelines for working with code in this repository. Following these conventions ensures consistency and maintainability across the codebase.

## Repository Overview

This is a NixOS configuration repository that manages multiple machines and custom packages using Nix flakes. It provides a declarative and reproducible way to manage systems. The repository acts as a centralized configuration system, enabling consistent environment setups across different hosts.

## Repository Structure

The repository is organized into several key directories:

- **hosts/**: Contains machine-specific configurations organized by architecture (x86_64-linux, aarch64-darwin)
- **profiles/**: Reusable configuration templates for different system types (desktop, server, laptop, workstation)
- **modules/**: Core system functionality modules (networking, services, authentication)
- **secrets/**: Encrypted configuration files managed with agenix
- **flake/**: Components of the flake configuration (packages, hosts, devenv)
- **users/**: User-specific configurations using home-manager
- **packages/**: Custom package definitions not yet in nixpkgs

## Development Workflow

### Common Commands

The repository includes a custom `world` command (accessible through a wrapped Justfile) that provides various utilities:

#### System Management

```bash
# Update existing system from GitHub
nixos-rebuild switch --flake github:johnae/world --use-remote-sudo

# Update from local clone
nixos-rebuild switch --flake . --use-remote-sudo

# Upgrade the system (Linux)
world upgrade [flake_ref]

# Upgrade the system (macOS)
world upgrade [flake_ref]

# Build without applying (Linux)
world build [flake_ref]

# Build without applying (macOS)
world build [flake_ref]

# Search for packages
world search <query>

# Open a shell with packages available
world shell <packages>

# Garbage collect 
world gc
```

#### Installing a New System

```bash
# Format disk and install
nix build .#<hostname>-diskformat
./result/bin/diskformat
nixos-rebuild switch --flake .#<hostname> --no-root-passwd
```

#### Development Tools

```bash
# Lint the codebase
world lint

# Check for dead code
world dead

# Check flake for old inputs
world dscheck

# Run flake checks
world check
```

## Architecture and Design Principles

### System Organization

The configuration follows a hierarchical approach:

1. **Base System**: Defined in `profiles/defaults.nix`, setting up core packages and configs
2. **Profile Layer**: Specialized templates like server, desktop, laptop in the `profiles/` directory
3. **Host-specific config**: Individual machine configurations in `hosts/` directory
4. **User Configurations**: User-specific settings managed through home-manager

### Secret Management

Secrets are managed using agenix (age-encrypted secrets) stored in the `secrets/` directory. The secrets are decrypted during system activation.

## Contributing Guidelines

### Working with Hosts

To add a new machine:
1. Create a configuration file in the relevant architecture folder under `hosts/`
2. Reference appropriate profiles for the machine type
3. Define machine-specific configuration
4. Build and install using the commands above

### Code Style and Best Practices

- **Minimize modifications**: Always prefer editing existing files over creating new ones
- **Use profiles**: Prefer using existing profiles over adding host-specific configuration
- **Follow patterns**: When adding new components, examine existing ones for patterns and conventions
- **Test changes**: Use the built-in `world` commands to verify changes before committing
- **Documentation**: Only create documentation when it adds significant value to the project
- **Formatting**: Use Alejandra for Nix files, fnlfmt for Fennel files, stylua for Lua files
- **Naming**: Follow existing patterns in similar files/directories
- **Imports**: Group imports logically by functionality
- **Error Handling**: Use appropriate Nix error handling patterns
- **Commits**: Follow conventional commit format ("feat:", "fix:", "chore:", etc.)

Always run linting etc before committing: `world lint && world dead && world dscheck`

### Development Tips

1. Always check which host configuration you're modifying
2. Use the built-in `world` commands for common operations
3. When adding new packages, check if they should be added to a specific profile or globally
4. Keep changes focused and minimal - do what needs to be done, nothing more

## Automated Processes

### CI/CD Pipeline

The repository uses CI (buildkite) for automated updates. Updates create pull requests where all custom packages and machine configurations are built and tested before merging.

## Resources

For more information, consult:
- The [README.md](./README.md) file for project overview
- The Justfile (`files/Justfile`) for available commands
- Documentation for [NixOS](https://nixos.org), [Nix flakes](https://nixos.wiki/wiki/Flakes), and [flake-parts](https://flake.parts)
