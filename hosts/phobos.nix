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
    ../modules/state.nix
    #"${nixos-hardware}/dell/xps/13-9360"
    secrets
  ];

  nix.trustedUsers = [ "root" userName ];

  networking = {
    inherit hostName;
    extraHosts = "127.0.1.1 ${hostName}";
  };

  system.activationScripts.preStateSetup = ''
    [ -d "/keep/home" ] || ${pkgs.btrfsProgs}/bin/btrfs sub create /keep/home
    [ -d "/keep/home/${userName}" ] || ${pkgs.btrfsProgs}/bin/btrfs sub create /keep/home/${userName}
    [ -d "/keep/home/${userName}/Downloads" ] || ${pkgs.btrfsProgs}/bin/btrfs sub create /keep/home/${userName}/Downloads
    chown ${builtins.toString secrets.users.extraUsers."${userName}".uid}:100 /keep/home/${userName}
    chown ${builtins.toString secrets.users.extraUsers."${userName}".uid}:100 /keep/home/${userName}/Downloads
  '';

  environment.state."/keep" = {
    directories = [
      "/var/log"
      "/var/lib/bluetooth"
      "/var/lib/iwd"
      "/var/lib/wireguard"
      "/var/lib/systemd/coredump"
      "/root"
    ];
    files = [
      "/etc/machine-id"
    ];
    users.${userName} = {
      directories = [
        "/home/${userName}/Downloads"
        "/home/${userName}/Documents"
        "/home/${userName}/Development"
        "/home/${userName}/Photos"
        "/home/${userName}/Pictures"
        "/home/${userName}/Sync"
        "/home/${userName}/.gnupg/private-keys-v1.d"
        "/home/${userName}/.local/share/direnv"
        "/home/${userName}/.local/share/password-store"
        "/home/${userName}/.local/share/fish"
        "/home/${userName}/.mail"
        "/home/${userName}/.cargo"
        "/home/${userName}/.cache/mu"
      ];
      files = [
        "/home/${userName}/.gnupg/pubring.kbx"
        "/home/${userName}/.gnupg/sshcontrol"
        "/home/${userName}/.gnupg/trustdb.gpg"
        "/home/${userName}/.gnupg/random_seed"
        "/home/${userName}/.kube/config"
        "/home/${userName}/.ssh/known_hosts"
        "/home/${userName}/.spotify_token_cache.json"
      ];
    };
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
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=2G" "mode=755" ];
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
    neededForBoot = true;
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
