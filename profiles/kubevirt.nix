{
  modulesPath,
  pkgs,
  lib,
  hostName,
  ...
}: {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
    device = "nodev";
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.initrd.availableKernelModules = [
    "virtio_pci"
    "virtio_scsi"
    "virtio_blk"
    "virtio_net"
    "ahci"
    "sd_mod"
    "sr_mod"
  ];
  boot.kernelModules = ["virtio_balloon" "virtio_console" "virtio_rng"];

  fileSystems."/" = {
    device = "/dev/vda2";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/vda1";
    fsType = "vfat";
  };

  fileSystems."/home" = {
    device = "/dev/vdb";
    fsType = "ext4";
    autoFormat = true;
    neededForBoot = true;
  };

  # KubeVirt secret volumes appear as small FAT filesystems
  fileSystems."/mnt/kubevirt-secrets" = {
    device = "/dev/vdc";
    fsType = "vfat";
    options = ["ro" "nofail"];
  };

  # agenix uses the SSH host key from the mounted secrets disk
  age.identityPaths = ["/mnt/kubevirt-secrets/ssh_host_ed25519_key"];

  networking.hostName = hostName;
  networking.usePredictableInterfaceNames = false;
  networking.useDHCP = false;
  networking.useNetworkd = true;
  systemd.network.enable = true;
  systemd.network.networks."eth0" = {
    matchConfig.Name = ["eth*" "enp*"];
    networkConfig.DHCP = "ipv4";
  };

  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = false;

  services.qemuGuest.enable = true;

  nix = {
    settings.trusted-users = ["root"];
    extraOptions = ''
      experimental-features = nix-command flakes
      accept-flake-config = true
      keep-outputs = true
      keep-derivations = true
      tarball-ttl = 900
    '';
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };
  };

  security.pam.loginLimits = [
    {
      domain = "*";
      type = "-";
      item = "nofile";
      value = "16384";
    }
  ];

  environment.systemPackages = with pkgs; [
    binutils
    cacert
    curl
    fd
    file
    git
    iptables
    jq
    lsof
    bottom
    man-pages
    mkpasswd
    nmap
    openssl
    procs
    psmisc
    ripgrep
    sd
    tree
    unzip
    vim
    wget
    zip
  ];

  system.stateVersion = "25.05";
}
