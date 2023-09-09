{
  modulesPath,
  hostName,
  pkgs,
  inputs,
  config,
  lib,
  ...
}: let
  inherit (lib // builtins) hasAttr length attrNames filterAttrs mkIf;
  hasState =
    hasAttr "state" config.environment
    && (length (attrNames config.environment.state)) > 0;
  hasSecrets = config.age.secrets != {};
in {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    ../../cachix.nix
  ];
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.availableKernelModules = ["ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi"];
  boot.initrd.kernelModules = ["nvme"];
  fileSystems."/" = {
    device = "/dev/sda1";
    fsType = "ext4";
    autoResize = true;
  };
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.tmp.cleanOnBoot = true;
  boot.growPartition = true;
  zramSwap.enable = true;
  services.openssh.enable = true;

  networking.dhcpcd.enable = false; ## we're using systemd-networkd
  networking.hostName = "";
  networking.useNetworkd = true;
  systemd.network.enable = true;

  nix = {
    settings.trusted-users = ["root"];
    extraOptions = ''
      experimental-features = nix-command flakes
      accept-flake-config = true
      keep-outputs = true
      keep-derivations = true
      tarball-ttl = 900
    '';

    registry.nixpkgs.flake = inputs.nixpkgs;

    nixPath = ["nixpkgs=${inputs.nixpkgs}"];

    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };

    package = pkgs.nix;
  };

  nixpkgs.config.allowUnfree = true;

  system.activationScripts.agenixNewGeneration = mkIf (hasSecrets && hasState) {deps = ["hetzner-cloud-init"];};

  system.activationScripts.hetzner-cloud-init.text = ''
    echo ------- Running Hetzner User Data Script --------
    curl http://169.254.169.254/hetzner/v1/userdata | bash
    echo -------------------------------------------------
  '';

  environment.systemPackages = [
    pkgs.binutils
    pkgs.cacert
    pkgs.curl
    pkgs.fd
    pkgs.file
    pkgs.git
    pkgs.iptables
    pkgs.jq
    pkgs.lsof
    pkgs.man-pages
    pkgs.mkpasswd
    pkgs.nmap
    pkgs.openssl
    pkgs.procs
    pkgs.psmisc
    pkgs.ripgrep
    pkgs.sd
    pkgs.tree
    pkgs.unzip
    pkgs.vim
    pkgs.wget
    pkgs.zip
  ];
}
