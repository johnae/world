{ config, lib, pkgs, ... }:
{

  imports = [
    ../modules
    ../cachix.nix
  ];

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';

    nixPath = [ ];

    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 30d";
    };
  };

  nix.package = pkgs.nixFlakes;

  nixpkgs.config.allowUnfree = true;

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;

  hardware.cpu.intel.updateMicrocode = true;
  networking.nameservers = [ "1.0.0.1" "1.1.1.1" "2606:4700:4700::1111" ];

  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";
  time.timeZone = "Europe/Stockholm";

  environment.shells = [ pkgs.bashInteractive pkgs.zsh pkgs.fish ];

  programs.fish.enable = true;

  programs.command-not-found.dbPath = "${./..}/programs.sqlite";

  security.sudo.extraConfig = ''
    Defaults  lecture="never"
  '';

  networking.firewall.enable = true;
  services.sshguard.enable = true;

  services.btrfs.autoScrub.enable = true;

  security.wrappers.netns-exec.source = "${pkgs.netns-exec}/bin/netns-exec";

  ## This just auto-creates /nix/var/nix/{profiles,gcroots}/per-user/<USER>
  ## for all extraUsers setup on the system. Without this home-manager refuses
  ## to run on boot when setup as a nix module and the user has yet to install
  ## anything through nix (which is the case on a completely new install).
  ## I tend to install the full system from an iso so I really want home-manager
  ## to run properly on boot.
  services.nix-dirs.enable = true;
}
