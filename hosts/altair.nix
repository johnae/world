{ userName, hostName, importSecret, pkgs, config, lib, inputs, ... }:
let
  secrets = importSecret "${inputs.secrets}/${hostName}/meta.nix";
  tailscale = importSecret "${inputs.secrets}/tailscale/meta.nix";
  transmission = config.services.transmission;
in
{

  imports = [
    ../profiles/server.nix
    ../modules/state.nix
    secrets
    tailscale
  ];

  nix.trustedUsers = [ "root" userName ];

  networking = {
    inherit hostName;
    extraHosts = "127.0.1.1 ${hostName}";
  };

  environment.systemPackages = [ pkgs.netns-exec ];

  environment.state."/keep" = {
    directories = [
      "/var/log"
      "/var/lib/wireguard"
      "/var/lib/systemd/coredump"
      "/var/lib/docker"
      "/var/lib/dockershim"
      "/var/lib/plex"
      "/var/lib/k3s"
      "/var/lib/kubelet"
      "/var/lib/cni"
      "/var/lib/transmission"
      "/var/lib/tailscale"
      "/root"
      "/etc/rancher"
    ];
    files = [
      "/etc/machine-id"
      "/etc/k3s-node-name"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];
  };

  services.myk3s = {
    nodeName = hostName;
    extraFlags = [ "--flannel-iface tailscale0" ];
  };

  services.tailscale.enable = true;

  services.transmission = {
    enable = true;
    downloadDirPermissions = "775";
  };

  services.plex = {
    enable = true;
    openFirewall = true;
    user = "transmission";
    group = "transmission";
  };

  systemd.services.transmission = {
    serviceConfig.ExecStart = lib.mkForce "/run/wrappers/bin/netns-exec private ${pkgs.transmission}/bin/transmission-daemon -f --port ${toString config.services.transmission.port} --config-dir ${config.services.transmission.home}/.config/transmission-daemon";
  };

  systemd.services.transmission-forwarder = {
    enable = true;
    after = [ "transmission.service" ];
    bindsTo = [ "transmission.service" ];
    script = ''
      ${pkgs.socat}/bin/socat tcp-listen:9091,fork,reuseaddr,bind=127.0.0.1  exec:'/run/wrappers/bin/netns-exec private ${pkgs.socat}/bin/socat STDIO "tcp-connect:127.0.0.1:9091"',nofork
    '';
  };

  boot =
    let
      address = (builtins.head secrets.networking.interfaces.eth0.ipv4.addresses).address;
      subnet = "255.255.255.192"; ## fixme
      defaultGateway = secrets.networking.defaultGateway;
    in
    {

      loader.systemd-boot.enable = lib.mkForce false;
      loader.efi.canTouchEfiVariables = lib.mkForce false;

      loader.grub.enable = true;
      loader.grub.devices = [ "/dev/nvme0n1" "/dev/nvme1n1" ];
      loader.grub.enableCryptodisk = true;
      kernelParams = [
        "ip=${address}::${defaultGateway}:${subnet}:${hostName}:eth0:none"
      ];

      kernelModules = [ "kvm-amd" ];

      initrd.extraUtilsCommandsTest = lib.mkForce "";
      initrd.availableKernelModules = [
        "r8169"
        "nvme"
        "ahci"
        "usbhid"
      ];
      initrd.network = {
        enable = true;
        ssh = {
          enable = true;
          port = 2222;
          hostKeys = [
            "/etc/nixos/initrd_keys/dsa_key"
            "/etc/nixos/initrd_keys/rsa_key"
            "/etc/nixos/initrd_keys/ed25519_key"
          ];
          authorizedKeys = secrets.users.extraUsers."${userName}".openssh.authorizedKeys.keys;
        };
        postCommands = ''
          echo 'cryptsetup-askpass' >> /root/.profile
        '';
      };

      initrd.luks.devices = {
        cryptkey = {
          device = "/dev/disk/by-label/cryptkey";
        };

        encrypted_root = {
          device = "/dev/disk/by-label/encrypted_root";
          keyFile = "/dev/mapper/cryptkey";
        };

        encrypted_root2 = {
          device = "/dev/disk/by-label/encrypted_root2";
          keyFile = "/dev/mapper/cryptkey";
        };

        encrypted_swap = {
          device = "/dev/disk/by-label/encrypted_swap";
          keyFile = "/dev/mapper/cryptkey";
        };

      };

    };

  hardware.cpu.intel.updateMicrocode = lib.mkForce false;
  hardware.cpu.amd.updateMicrocode = true;

  users.defaultUserShell = pkgs.fish;
  users.mutableUsers = false;
  users.groups."${userName}".gid = 1337;
  users.extraUsers."${userName}" = { shell = pkgs.fish; };

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=16G" "mode=755" ];
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

  fileSystems."/mnt/disks/cow/local-disk-1" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-1" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-1" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-2" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-2" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-3" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-3" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-4" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-4" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-5" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-5" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-6" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-6" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-7" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-7" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-8" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-8" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-9" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-9" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-10" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-10" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-11" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-11" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-12" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-12" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-13" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-13" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-14" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-14" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-15" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-15" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-16" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-16" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-17" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-17" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-18" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-18" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-19" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-19" = {
    device = "/dev/disk/by-label/root";
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
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-20" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-20" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-20"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
}
