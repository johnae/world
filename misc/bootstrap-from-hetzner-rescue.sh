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

sh <(curl -L https://nixos.org/nix/install) --no-daemon

set +u +x
. $HOME/.nix-profile/etc/profile.d/nix.sh
set -u -x

cat<<INFO
Now run:

nix shell nixpkgs#nixos-install-tools nixpkgs#mount
nixos-install --flake .#<hostname> --no-root-passwd --impure
INFO
