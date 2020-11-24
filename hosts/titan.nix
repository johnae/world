{ userName, hostName, importSecret, pkgs, config, lib, inputs, ... }:
let
  secrets = importSecret "${inputs.secrets}/${hostName}/meta.nix";
in
{
  imports = [
    ../profiles/server.nix
    secrets
  ];

  nix.trustedUsers = [ "root" userName ];

  networking = {
    inherit hostName;
    extraHosts = "127.0.1.1 ${hostName}";
  };

  users.defaultUserShell = pkgs.fish;
  users.mutableUsers = false;
  users.groups."${userName}".gid = 1337;
  users.extraUsers."${userName}" = { shell = pkgs.fish; };

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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [ "subvol=@" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [ "subvol=@home" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [ "subvol=@var" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/cow/local-disk-1" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-1" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-1" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-1"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-2" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-2" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-2" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-2"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-3" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-3" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-3" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-3"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-4" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-4" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-4" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-4"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-5" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-5" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-5" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-5"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-6" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-6" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-6" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-6"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-7" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-7" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-7" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-7"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-8" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-8" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-8" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-8"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-9" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-9" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-9" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-9"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-10" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-10" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-10" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-10"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-11" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-11" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-11" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-11"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-12" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-12" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-12" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-12"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-13" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-13" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-13" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-13"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-14" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-14" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-14" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-14"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-15" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-15" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-15" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-15"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-16" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-16" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-16" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-16"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-17" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-17" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-17" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-17"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-18" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-18" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-18" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-18"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-19" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-19" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-19" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-19"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-20" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-20" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-20" = {
    device = "/dev/disk/by-label/extra";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-20"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/backup" = {
    device = "/dev/disk/by-label/external";
    fsType = "btrfs";
    options = [ "rw" "noatime" "compress=zstd" "space_cache" ];
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
      keyFile = "/sys/class/dmi/id/product_uuid";
    };

    encrypted_root = {
      device = "/dev/disk/by-label/encrypted_root";
      keyFile = "/dev/mapper/cryptkey";
    };

    encrypted_swap = {
      device = "/dev/disk/by-label/encrypted_swap";
      keyFile = "/dev/mapper/cryptkey";
    };

    encrypted_extra = {
      device = "/dev/disk/by-label/encrypted_extra";
      keyFile = "/dev/mapper/cryptkey";
    };
  };

}
