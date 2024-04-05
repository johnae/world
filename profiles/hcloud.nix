{
  modulesPath,
  pkgs,
  config,
  lib,
  hostName,
  ...
}: let
  inherit (lib // builtins) hasAttr length attrNames mkIf;
  hasState =
    hasAttr "state" config.environment
    && (length (attrNames config.environment.state)) > 0;
  hasSecrets = config.age.secrets != {};
in {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  boot.loader.grub = {
    # no need to set devices, disko will add all devices that have a EF02 partition to the list already
    # devices = [ ];
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;

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

  networking.usePredictableInterfaceNames = false;
  networking.dhcpcd.enable = false;
  networking.useDHCP = false;
  networking.useNetworkd = true;
  systemd.network.enable = true;
  systemd.network.networks."eth0" = {
    matchConfig.Name = "eth0";
    networkConfig.DHCP = "ipv4";
  };
  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = false;

  networking.hostName = hostName;

  system.activationScripts.agenixNewGeneration = mkIf (hasSecrets && hasState) {deps = ["persist-files"];};

  ## networking must be up before we can run the cloud-init script
  systemd.services.hetzner-cloud-init = let
    cloudInitScript = pkgs.writeShellScript "cloud-init" ''
      echo ------- Running Hetzner User Data Script --------
      ${pkgs.curl}/bin/curl http://169.254.169.254/hetzner/v1/userdata | ${pkgs.bash}/bin/bash
      echo -------------------------------------------------
    '';
  in {
    description = "Hetzner Cloud Init";
    after = ["network.target"];
    before = ["tailscale-auth.service" "tailscaled.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = cloudInitScript;
    };
  };

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
