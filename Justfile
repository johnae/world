## This is also wrapped up into the command "world" which is available
## anywhere - even outside this repo. See: utils/world.nix

set shell := ["nu", "-c"]

alias help := default

default:
  @just --list -f {{justfile()}} -d {{invocation_directory()}}

# search for packages
search query:
  @nix search nixpkgs {{query}} --json | from json | transpose | flatten | select column0 version description

# open a shell with given packages available
shell +args:
  @nix shell (echo {{args}} | each { |it| $"nixpkgs#($it)" } )

# garbage collect the system
gc:
  @nix-collect-garbage -d

# upgrade the system using given flake ref
upgrade flake="github:johnae/world":
  @rm -rf ~/.cache/nix/fetcher-cache-v1.sqlite*
  @nixos-rebuild boot --flake {{flake}} --use-remote-sudo -L
  @if (echo initrd kernel kernel-modules | all { |it| (readlink $"/run/booted-system/($it)") != (readlink $"/nix/var/nix/profiles/system/($it)") }) { echo "The system must be rebooted for the changes to take effect" } else { nixos-rebuild switch --flake {{flake}} --use-remote-sudo -L }
