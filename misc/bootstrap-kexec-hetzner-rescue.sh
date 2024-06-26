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

ipv4="$(ip addr show dev eth0 | grep -E 'inet .*global.*' | awk '{print $2}')"
ipv6="$(ip addr show dev eth0 | grep -E 'inet6 .*global.*' | awk '{print $2}')"
ipv4gw="$(ip route | grep "default via" | awk '{print $3}')"
ipv6gw="$(ip -6 route | grep "default via" | awk '{print $3}')"

cat <<EOF > /root/config.nix
{pkgs, lib, ...}:
{
  services.openssh.enable = true;
  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";
  time.timeZone = "Europe/Stockholm";
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = ["xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc"];
  boot.supportedFilesystems = lib.mkForce [ "btrfs" "cifs" "f2fs" "jfs" "ntfs" "reiserfs" "vfat" "xfs" "bcachefs" ];
  users.users.root.openssh.authorizedKeys.keys = [
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJY3QSBIiRKN8/B3nHgCBDpauQBOftphOeuF2TaBHGQSAAAABHNzaDo="
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIAwJWtQ5ZU9U0szWzJ+/GH2uvXZ15u9lL0RdcHdsXM0VAAAABHNzaDo="
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK+trOinD68RD1efI6p05HeaNA0SjzeRnUvpf22+jsq+"
  ];

  nix = {
    settings.trusted-users = ["root"];
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
      tarball-ttl = 900
    '';
  };

  environment.systemPackages = with pkgs; [
    git
    nixos-install-tools
    bcachefs-tools
    btrfs-progs
    jq
  ];

  networking.useDHCP = false;
  systemd.network = {
    enable = true;
    networks."10-wan".extraConfig = ''
      [Match]
      Name = enp*
      [Network]
      Address = $ipv6
      Gateway = $ipv6gw
      Address =  $ipv4
      Gateway = $ipv4gw
    '';
  };
}
EOF

# Generate the kexec script
nix run github:nix-community/nixos-generators -- -o /root/result  -f kexec-bundle -c /root/config.nix -I nixpkgs=channel:nixos-unstable
# Run the kexec script
echo "kexec - please reconnect"
/root/result
