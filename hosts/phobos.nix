{ hostName, userName, config, lib, pkgs, inputs, ... }:
let
  secrets = (builtins.exec [
    "${pkgs.sops}/bin/sops"
    "-d"
    "${inputs.secrets}/${hostName}/meta.nix"
  ]) { inherit pkgs userName; };

  nixos-hardware = inputs.nixos-hardware;
in
with lib; {
  imports = [
    ../profiles/laptop.nix
    "${nixos-hardware}/dell/xps/13-9360"
    secrets
  ];

  nix.trustedUsers = [ "root" userName ];

  networking = {
    inherit hostName;
    extraHosts = "127.0.1.1 ${hostName}";
  };

  boot.btrfsCleanBoot = {
    enable = true;
    wipe = [ "@" "@var" ];
    keep = [
      "/var/lib/bluetooth"
      "/var/lib/iwd"
      "/var/lib/wireguard"
      "/root"
    ];
  };

  environment.systemPackages = [
    pkgs.man-pages
    pkgs.bmon
    pkgs.iftop
    pkgs.file
    pkgs.cacert
    pkgs.openssl
    pkgs.curl
    pkgs.gnupg
    pkgs.lsof
    pkgs.usbutils
    pkgs.mkpasswd
    pkgs.glib

    pkgs.powertop
    pkgs.socat
    pkgs.nmap
    pkgs.iptables
    pkgs.bridge-utils
    pkgs.dnsmasq
    pkgs.dhcpcd
    pkgs.dhcp
    pkgs.bind
    pkgs.pciutils

    pkgs.zip
    pkgs.wget
    pkgs.unzip
    pkgs.hdparm
    pkgs.libsysfs
    pkgs.htop
    pkgs.jq
    pkgs.binutils
    pkgs.psmisc
    pkgs.tree
    pkgs.ripgrep
    pkgs.vim
    pkgs.git
    pkgs.fish
    pkgs.tmux

    pkgs.blueman
    pkgs.pavucontrol
    pkgs.bluez
    pkgs.bluez-tools
    pkgs.fd
    pkgs.wireguard
    pkgs.iwd
  ];

  ## trying to fix bluetooth disappearing after suspend
  sleepManagement = {
    enable = true;
    sleepCommands = ''
      ${pkgs.libudev}/bin/systemctl stop bluetooth && ${pkgs.kmod}/bin/modprobe -r btusb
    '';
    wakeCommands = ''
      ${pkgs.kmod}/bin/modprobe btusb && ${pkgs.libudev}/bin/systemctl start bluetooth
    '';
  };
  ## end fix

  security.pam.services.swaylock = {
    text = ''
      auth include login
    '';
  };

  users.defaultUserShell = pkgs.fish;
  users.mutableUsers = false;
  users.groups = {
    "${userName}".gid = 1337;
    scard.gid = 1050;
  };
  users.extraUsers."${userName}" = {
    shell = pkgs.fish;
    extraGroups = [ "scard" ];
  };

  programs.sway.enable = true;
  home-manager.useUserPackages = true;
  home-manager.users."${userName}" = { ... }: {
    imports = [ ../home/home.nix ];

    wayland.windowManager.sway.config.output = {
      "eDP-1" = {
        scale = "1.6";
        pos = "0 0";
      };
    };

  };

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [ "subvol=@" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@home" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@var" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@nix" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/keep" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@keep" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  boot.initrd.luks.devices = {
    cryptkey = { device = "/dev/disk/by-label/cryptkey"; };

    encrypted_root = {
      device = "/dev/disk/by-label/encrypted_root";
      keyFile = "/dev/mapper/cryptkey";
    };

    encrypted_swap = {
      device = "/dev/disk/by-label/encrypted_swap";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

}
