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

  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.grub = {
    enable = lib.mkForce true;
    device = "/dev/vda";
  };

  boot.kernelParams = ["console=ttyS0"];
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;

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

  boot.growPartition = true;

  fileSystems."/" = {
    device = "/dev/vda2";
    fsType = "ext4";
    autoResize = true;
  };

  fileSystems."/home" = {
    device = "/dev/vdb";
    fsType = "ext4";
    autoFormat = true;
  };

  # Ensure home directories exist after /home is mounted (PVC may be fresh)
  systemd.tmpfiles.rules = [
    "d /home/john 0700 1337 100 -"
  ];

  # home-manager must run after /home is mounted and user dir exists
  systemd.services."home-manager-john" = {
    after = ["home.mount"];
    requires = ["home.mount"];
  };

  # KubeVirt secret volumes are mounted as ISO9660 filesystems
  fileSystems."/mnt/kubevirt-secrets" = {
    device = "/dev/vdc";
    fsType = "iso9660";
    options = ["ro" "nofail"];
  };

  fileSystems."/mnt/kubevirt-ts-auth" = {
    device = "/dev/vdd";
    fsType = "iso9660";
    options = ["ro" "nofail"];
  };

  # agenix uses the SSH host key from the mounted secrets disk
  age.identityPaths = ["/mnt/kubevirt-secrets/ssh_host_ed25519_key"];

  networking.hostName = hostName;
  networking.usePredictableInterfaceNames = false;
  networking.useDHCP = false;
  networking.useNetworkd = true;
  systemd.network.enable = true;
  # Drop the static resolvers from defaults.nix on a KubeVirt guest. The
  # cluster is IPv6-only, so reaching IPv4-only hosts (github, most of the
  # internet) needs DNS64, which only CoreDNS does, and passt hands CoreDNS
  # to the guest over DHCPv6 - the same path cryo-builder uses. A hardcoded
  # list here would shadow that lease.
  networking.nameservers = lib.mkForce [];
  systemd.network.networks."eth0" = {
    matchConfig.Name = ["eth*" "enp*"];
    # ipv6, not ipv4: the pod network has no v4, and it is the DHCPv6 lease
    # that carries CoreDNS (DNS64). SLAAC from passt's RA already supplies
    # the address and route; DHCP=ipv4 left the guest addressed but with no
    # resolver that could reach anything IPv4-only.
    networkConfig = {
      DHCP = "ipv6";
      IPv6AcceptRA = true;
    };
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
