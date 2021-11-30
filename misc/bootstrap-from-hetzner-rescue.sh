#!/usr/bin/env bash

set -eu
set -o pipefail
set -x

apt-get install -y sudo

# Allow installing nix as root, see
#   https://github.com/NixOS/nix/issues/936#issuecomment-475795730
mkdir -p /etc/nix
echo "build-users-group =" > /etc/nix/nix.conf
echo "experimental-features = flakes nix-command" >> /etc/nix/nix.conf

curl -L https://github.com/numtide/nix-unstable-installer/releases/download/nix-2.4pre20210823_af94b54/install | sh

set +u +x
. $HOME/.nix-profile/etc/profile.d/nix.sh
set -u -x

nix-channel --add https://nixos.org/channels/nixos-unstable nixpkgs
nix-channel --update

# Getting NixOS installation tools
nix-env -iE "_: with import <nixpkgs/nixos> { configuration = {}; }; with config.system.build; [ nixos-generate-config nixos-install nixos-enter manual.manpages ]"
