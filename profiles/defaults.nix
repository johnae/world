{ hostName, pkgs, inputs, ... }:
{

  imports = [
    ../cachix.nix
  ];

  nix = {
    trustedUsers = [ "root" ];
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
      keep-outputs = true
      keep-derivations = true
      tarball-ttl = 900
    '';

    registry.nixpkgs.flake = inputs.nixpkgs;

    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };

    package = pkgs.nixUnstable;
  };

  nixpkgs.config.allowUnfree = true;

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
    pkgs.powertop
    pkgs.socat
    pkgs.nmap
    pkgs.iptables
    pkgs.bridge-utils
    pkgs.pciutils
    pkgs.zip
    pkgs.wget
    pkgs.unzip
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
    pkgs.hyperfine
    pkgs.procs
    pkgs.sd
    pkgs.bottom
    pkgs.virtmanager
  ];


  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  networking.nameservers = [ "1.0.0.1" "1.1.1.1" "2606:4700:4700::1111" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];

  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";
  time.timeZone = "Europe/Stockholm";

  environment.shells = [ pkgs.bashInteractive pkgs.zsh pkgs.fish ];

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

  services.btrfs.autoScrub.enable = true;

  security.wrappers.netns-exec = {
   source = "${pkgs.netns-exec}/bin/netns-exec";
   owner = "root";
   group = "root";
   setuid = true;
  };

  ## This just auto-creates /nix/var/nix/{profiles,gcroots}/per-user/<USER>
  ## for all extraUsers setup on the system. Without this home-manager refuses
  ## to run on boot when setup as a nix module and the user has yet to install
  ## anything through nix (which is the case on a completely new install).
  ## I tend to install the full system from an iso so I really want home-manager
  ## to run properly on boot.
  services.nix-dirs.enable = true;
}
