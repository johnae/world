# Declarative Today. Utopia Tomorrow.

This is the [NixOS](https://nixos.org) configuration repository for all my machines and custom packages (i.e not part of [nixpkgs](https://github.com/nixos/nixpkgs) yet). In some sense it is my world, which is how it got its name.

It's based on a relatively recent feature of the [Nix package manager](https://nixos.org) called [flakes](https://nixos.wiki/wiki/Flakes). Flakes are somewhat similar to a `Cargo.toml/Cargo.lock` from the [Rust programming language](https://rust-lang.org), a `go.mod/go.sum` from the [Go programming language](https://golang.org/) or the `package.json/package.lock` files used by the [Node Package Manager](https://www.npmjs.com/). The difference being that it is language agnostic - it handles any package(s) or file(s) really. Here, we're using it to build hosts and software deterministically in a reproducible fashion. This would all be possible using Nix without the flakes feature, but it would be more work to design and keep it up-to-date. While flakes are still labelled "experimental" they have a bright future.

This repo is using [flake-parts](https://flake.parts) to modularize and clean up [./flake.nix](./flake.nix).

# Installing a new machine

Add a new file under the relevant architecture folder under the [hosts/](hosts/) directory, possibly copying an existing fitting host and modifying it. Add the wanted profiles and other configuration. Commit and push them to this repository.

## Running the installation process

Download a recent installer from [https://nixos.org/download.html#nixos-iso](https://nixos.org/download.html#nixos-iso) and enable flakes after booting it:

```sh
  mkdir -p /etc/nix
  cat<<EOF>>/etc/nix/nix.conf
  experimental-features = nix-command flakes
  EOF
```

Then proceed to clone this repo:

```sh
  cd /tmp
  git clone https://github.com/johnae/world
  cd world
```

After that, just pick the host you're installing, like this:

```sh
  host=eris
  nix build .#"$host"-diskformat
  ./result/bin/diskformat
  nixos-install --flake .#"$host" --no-root-passwd
```

Hosts are defined in the [hosts/](hosts/) directory.

_This setup is very customized to my taste. For example: all disks are encrypted using dm-crypt and then on top of that formatted as btrfs, swap is automatically setup to a "reasonable" value that is not ideal for a lot of use cases, root is mounted as tmpfs and will therefore use some amount of RAM and it'll also be ephemeral - a reboot results in a clean slate and you'll have to opt-in to what you want to keep._

# Updating an existing machine

Updates should be handled through standard commit/push/pullreq workflow. Updating a machine is done like this:

```sh
  nixos-rebuild switch --flake github:johnae/world --use-remote-sudo
```

or via a local clone of this repo:

```sh
  git clone git@github.com:johnae/world
  cd world
  nixos-rebuild switch --flake . --use-remote-sudo
```

# Package updates etc

Package updates and OS updates are automated through actions. The update action creates pull requests where all custom packages are built and then all machine configurations are built to test that things seem ok before merging. This should catch many issues coming from OS and package updates before they're deployed.

# License
[MIT](https://choosealicense.com/licenses/mit)

This repository references many packages with varying licenses so please only consider the actual code in this repo to be MIT licensed.
