{
  hostName,
  pkgs,
  inputs,
  config,
  lib,
  ...
}: let
  inherit (lib // builtins) attrNames hasAttr mkIf length;
  hasState =
    hasAttr "persistence" config.environment
    && (length (attrNames config.environment.persistence)) > 0;
  hasSecrets = config.age.secrets != {};
in {
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
      tarball-ttl = 900
    '';

    nixPath = ["nixpkgs=${inputs.nixpkgs}"];

    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };

    package = pkgs.nix;

    settings = {
      trusted-users = ["root"];
      substituters = [
        "https://cache.nixos.org"
        "https://cachix.cachix.org"
        "https://insane.cachix.org"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
        "insane.cachix.org-1:cLCCoYQKkmEb/M88UIssfg2FiSDUL4PUjYj9tdo4P8o="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  environment.systemPackages = [
    pkgs.binutils
    pkgs.blueman
    pkgs.bluez
    pkgs.bluez-tools
    pkgs.bmon
    pkgs.bottom
    pkgs.bridge-utils
    pkgs.cacert
    pkgs.claude-code
    pkgs.curl
    pkgs.fd
    pkgs.file
    pkgs.fish
    pkgs.git
    pkgs.gnupg
    pkgs.headscale
    pkgs.htop
    pkgs.hyperfine
    pkgs.iftop
    pkgs.iptables
    pkgs.jq
    pkgs.lsof
    pkgs.man-pages
    pkgs.mkpasswd
    pkgs.nb
    pkgs.nmap
    pkgs.openssl
    pkgs.pavucontrol
    pkgs.pciutils
    pkgs.powertop
    pkgs.procs
    pkgs.psmisc
    pkgs.rbw-atomic-unlock
    pkgs.ripgrep
    pkgs.sd
    pkgs.serpl
    pkgs.socat
    pkgs.tmux
    pkgs.tree
    pkgs.tpm2-tss
    pkgs.unzip
    pkgs.usbutils
    pkgs.vim
    pkgs.virt-manager
    pkgs.wget
    pkgs.wireguard-tools
    pkgs.zip
  ];

  ## disable that slow "building man-cache" step
  documentation.man.generateCaches = lib.mkForce false;

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  networking.nameservers = ["9.9.9.9" "149.112.112.112" "2620:fe::fe" "2620:fe::9"];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc"];

  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";
  time.timeZone = "Europe/Stockholm";

  services.udev.extraRules = ''
    ATTR{idVendor}=="2357", ATTR{idProduct}=="0600", RUN+="${pkgs.usb-modeswitch}/bin/usb_modeswitch -K -v 0x2357 -p 0x0600 -V 0x2357 -P 0x0601 -R"
    ## allows using QMK/Via
    KERNEL=="hidraw*", SUBSYSTEM=="hidraw", MODE="0666", TAG+="uaccess", TAG+="udev-acl"
  '';

  security.pam.loginLimits = [
    {
      domain = "*";
      type = "-";
      item = "nofile";
      value = "65536";
    }
  ];

  environment.shells = [pkgs.bashInteractive pkgs.zsh pkgs.fish];

  programs.fish.enable = true;

  programs.command-not-found.dbPath = "${./..}/programs.sqlite";

  security.sudo.extraConfig = ''
    Defaults  lecture="never"
  '';

  networking = {
    firewall.enable = true;
    inherit hostName;
  };

  services.sshguard.enable = true;
  services.fstrim.enable = true;

  systemd.settings.Manager.DefaultTimeoutStopSec = "90s";

  ## only allow declarative user management
  users.mutableUsers = false;

  system.stateVersion = "25.05";

  ##
  system.activationScripts.agenixNewGeneration = mkIf (hasSecrets && hasState) {deps = ["persist-files"];};

  services.nix-dirs.enable = true;
}
