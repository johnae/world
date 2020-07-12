{ config, lib, pkgs, inputs, ... }:
let
  nixos-hardware = inputs.nixos-hardware;
  #loadBuildMachines = with builtins;
  #  if getEnv "NIX_TEST" != ""
  #  then [ ]
  #  else (extraBuiltins.sops ../metadata/builders/hosts.yaml).buildMachines;
in
{
  imports = [
    "${nixos-hardware}/common/pc/ssd"
    ./defaults.nix
  ];


  #nix.buildMachines = loadBuildMachines;
  #nix.distributedBuilds = true;
  #nix.extraOptions = ''
  #  builders-use-substitutes = true
  #'';

  boot.kernel.sysctl = {
    "vm.dirty_writeback_centisecs" = 1500;
    "vm.laptop_mode" = 5;
    "vm.swappiness" = 1;
    "fs.inotify.max_user_watches" = 12288;
  };

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;

  #environment.variables = {
  #  MESA_LOADER_DRIVER_OVERRIDE = "iris";
  #};
  #hardware.opengl.package = pkgs.mesa-iris.drivers;
  hardware.opengl.extraPackages = [
    pkgs.intel-media-driver
    pkgs.vaapiIntel
    pkgs.vaapiVdpau
    pkgs.libvdpau-va-gl
  ];

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.support32Bit = true;
  hardware.bluetooth.enable = true;
  networking.wireless.iwd.enable = true;

  console.font = "latarcyrheb-sun32";

  environment.pathsToLink = [ "/etc/gconf" ];

  powerManagement.enable = true;
  powerManagement.powertop.enable = true;

  virtualisation.docker.enable = true;

  programs.ssh.startAgent = false;

  programs.dconf.enable = true;
  programs.light.enable = true;
  programs.firejail.enable = true;

  services.kbfs.enable = true;
  services.keybase.enable = true;

  #services.pcscd.enable = true;
  services.cron.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.gvfs.enable = true;
  #services.gnome3.gnome-keyring.enable = true;
  services.gnome3.sushi.enable = true;
  services.openssh.enable = true;

  services.interception-tools.enable = true;

  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint pkgs.hplip pkgs.gutenprintBin ];

  services.dbus.packages = with pkgs; [ gcr dconf gnome3.sushi ];
  services.udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];

  services.logind.lidSwitch = "suspend-then-hibernate";
  environment.etc."systemd/sleep.conf".text = "HibernateDelaySec=8h";

  ## yubikey v4
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="usb", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0407", MODE="660", GROUP="scard"
  '';

  services.upower.enable = true;
  services.disable-usb-wakeup.enable = true;
  services.pasuspender.enable = true;
  services.rbsnapper.enable = true;
  services.rbsnapper.volume = "/keep/home/john";

  fonts.fonts = with pkgs; [
    google-fonts
    #source-code-pro
    #office-code-pro-font
    #system-san-francisco-font
    #san-francisco-mono-font
    font-awesome_5
    powerline-fonts
    roboto
    mynerdfonts
    #fira-code
    #fira-code-symbols
  ];

}
