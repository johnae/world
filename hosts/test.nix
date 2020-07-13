{ userName, hostName, pkgs, config, lib, inputs, ... }:
let
  secrets = (builtins.exec [
    "${pkgs.sops}/bin/sops"
    "-d"
    "${inputs.secrets}/${hostName}/meta.nix"
  ]) { inherit pkgs userName; };
  persistenceDirs = config.environment.state."/keep".directories;
in
{
  imports = [
    ../profiles/server.nix
    ../modules/state.nix
    secrets
  ];

  system.activationScripts.keep-setup = ''
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
        "/home/${userName}/.gnupg/private-keys-v1.d"
        "/home/${userName}/.local/share/direnv"
        "/home/${userName}/.local/share/password-store"
        "/home/${userName}/.mail"
        "/home/${userName}/.cargo"
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

  home-manager.useUserPackages = true;
  home-manager.users."${userName}" = { ... }: {
    xdg.enable = true;
    home.packages = [
      pkgs.kubectl
    ];
    home.file.".default-background.jpg" = {
      source = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tri-fadeno.jpg";
    };
  };

  nix.trustedUsers = [ "root" userName ];

  networking = {
    inherit hostName;
    extraHosts = "127.0.1.1 ${hostName}";
  };

  users.defaultUserShell = pkgs.fish;
  users.mutableUsers = false;
  users.groups."${userName}".gid = 1337;
  users.extraUsers."${userName}" = { shell = pkgs.fish; };

  services.myk3s.enable = lib.mkForce false;
  services.myk3s.docker = lib.mkForce false;

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usbhid"
    "usb_storage"
    "uas"
    "sd_mod"
    "rtsx_pci_sdmmc"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=2G" "mode=755" ];
  };

  fileSystems."/keep" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    neededForBoot = true;
    options =
      [ "subvol=@keep" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@nix" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  nix.maxJobs = lib.mkDefault 4;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  # High-DPI console
  console.font =
    lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
  boot.initrd.luks.devices = {
    cryptkey = {
      device = "/dev/disk/by-label/cryptkey";
    };

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
