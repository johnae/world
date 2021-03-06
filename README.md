# The World

This is a [NixOS](https://nixos.org) configuration repository for all my machines and custom packages (i.e not part of [nixpkgs](https://github.com/nixos/nixpkgs) yet). In essence it contains my world - which is how it got its name.

It's based on a relatively recent feature of the [Nix package manager](https://nixos.org) called [flakes](https://nixos.wiki/wiki/Flakes). Flakes are somewhat similar to a `Cargo.toml/Cargo.lock` from the [Rust programming language](https://rust-lang.org), a `go.mod/go.sum` from the [Go programming language](https://golang.org/) or the `package.json/package.lock` files used by the [Node Package Manager](https://www.npmjs.com/). The difference being that it is language agnostic - it handles any package or file/files really. Here, we're using it to build hosts and software deterministically in a reproducible fashion.

## Installing a new machine

Edit the [hosts.toml](hosts.toml) file and add a new host. Add the wanted profiles and other configuration. Commit and push them to this repository.
Currently I PXE boot the (mostly automatic) installer which will format disks and install the system and configuration for the defined hosts. To PXE boot the installer, enable any necessary options in BIOS and then - on another machine on the network from a checkout of this repo - run:

```sh
nix develop -c world pixieboot
```

You may have to turn off or tweak your firewall for the above to work. I've yet to do anything specific for that but I guess you could probably use the [NixOS configuration options for pxebooting](https://search.nixos.org/options?channel=21.05&from=0&size=50&sort=relevance&query=pixiecore) which would handle the firewall for you. But since this is usually a one-off thing for me I don't use that.

Anyway, after running the above command you should be able to pxeboot a machine into the installer. You will then be asked which host to install and after that it's mostly automatic - except for asking you what password to use to unlock the disk.

## Updating an existing machine

Updates should be handled through standard commit/push/pullreq workflow. Updating a machine called "hello" can be done like this (on that machine):

```sh
nixos-rebuild switch --flake github:johnae/world#hello --use-remote-sudo
```

or via a local clone of this repo:

```sh
git clone git@github.com:johnae/world
cd world
nixos-rebuild switch --flake .#hello --use-remote-sudo
```

## Package updates etc

Package updates and OS updates are automated through actions. The update action creates pull requests where all custom packages are built and then all machine configurations are built to test that things seem ok before merging. This should catch many issues coming from OS and package updates before they're deployed.


## License
[MIT](https://choosealicense.com/licenses/mit/)

This does reference many packages with varying licenses so please only consider the code in this repo to be MIT licensed, the rest you'll have to look up yourself.
