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

  environment.etc."bashrc.local".text = ''
    if [ -e /run/nixos/metadata ]; then
      source /run/nixos/metadata
      if [ "$TERM" != "dumb" ] || [ -n "$INSIDE_EMACS" ]; then
        PROMPT_COLOR="1;31m"
        ((UID)) && PROMPT_COLOR="1;32m"
        if [ -n "$INSIDE_EMACS" ]; then
          # Emacs term mode doesn't support xterm title escape sequence (\e]0;)
          PS1="\n\[\033[$PROMPT_COLOR\][\u@\h|$NODENAME:\w]\\$\[\033[0m\] "
        else
          PS1="\n\[\033[$PROMPT_COLOR\][\[\e]0;\u@\h|$NODENAME: \w\a\]\u@\h|$NODENAME:\w]\\$\[\033[0m\] "
        fi
        if test "$TERM" = "xterm"; then
          PS1="\[\033]2;\h|$NODENAME:\u:\w\007\]$PS1"
        fi
      fi
    fi
  '';

  security.pam.loginLimits = [
    {
      domain = "*";
      type = "-";
      item = "nofile";
      value = "16384";
    }
  ];

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
  systemd.services.metadata = let
    cloudInitScript = pkgs.writeShellScript "cloud-init" ''
      echo ------- Running Hetzner User Data Script --------
      ${pkgs.curl}/bin/curl http://169.254.169.254/hetzner/v1/userdata | ${pkgs.bash}/bin/bash
      echo -------------------------------------------------
    '';
  in {
    description = "Metadata Service";
    after = ["network.target"];
    before = ["tailscale-auth.service" "tailscaled.service" "k3s.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = cloudInitScript;
    };
  };

  system.stateVersion = "24.11";

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
    pkgs.bottom
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
