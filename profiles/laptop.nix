{ hostName, userName, config, lib, pkgs, inputs, ... }:
let
  nixos-hardware = inputs.nixos-hardware;
  buildMachines = (builtins.exec [
    "${pkgs.sops}/bin/sops"
    "-d"
    "${inputs.secrets}/builders/hosts.nix"
  ]);
in
{
  imports = [
    "${nixos-hardware}/common/pc/ssd"
    ./defaults.nix
  ];


  nix.buildMachines = buildMachines;
  nix.distributedBuilds = true;
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  boot.kernel.sysctl = {
    "vm.dirty_writeback_centisecs" = 1500;
    "vm.laptop_mode" = 5;
    "vm.swappiness" = 1;
    "fs.inotify.max_user_watches" = 12288;
  };

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

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
  hardware.bluetooth.enable = true;
  networking.wireless.iwd.enable = true;

  console.font = "latarcyrheb-sun32";

  environment.pathsToLink = [ "/etc/gconf" ];

  ##
  powerManagement.enable = true;
  powerManagement.powertop.enable = true;
  ##

  virtualisation.docker.enable = false;
  virtualisation.podman.enable = true;

  programs.ssh.startAgent = false;

  programs.dconf.enable = true;
  programs.light.enable = true;

  services.cron.enable = true;
  services.avahi.enable = true;
  services.avahi.nssmdns = true;
  services.gvfs.enable = true;
  services.gnome3.sushi.enable = true;
  services.openssh.enable = true;

  services.fwupd.enable = true;

  services.interception-tools.enable = true;

  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint pkgs.hplip pkgs.gutenprintBin ];

  services.dbus.packages = with pkgs; [ gcr dconf gnome3.sushi ];
  services.udev.packages = with pkgs; [ gnome3.gnome-settings-daemon ];

  ##
  services.logind.lidSwitch = "suspend-then-hibernate";
  environment.etc."systemd/sleep.conf".text = "HibernateDelaySec=8h";
  ##

  services.udev.extraRules = ''
    ## yubikey v4
    ACTION=="add", SUBSYSTEM=="usb", ENV{ID_VENDOR_ID}=="1050", ENV{ID_MODEL_ID}=="0407", MODE="660", GROUP="scard"
  '';

  ##
  services.upower.enable = true;
  services.disable-usb-wakeup.enable = true;
  services.pasuspender.enable = true;
  ##
  services.rbsnapper.enable = true;
  services.rbsnapper.volume = "/keep/home/john";

  services.pipewire.enable = true;
  xdg.portal.enable = true;
  xdg.portal.gtkUsePortal = true;
  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-wlr pkgs.xdg-desktop-portal-gtk ];

  fonts.fonts = with pkgs; [
    google-fonts
    font-awesome_5
    powerline-fonts
    roboto
    mynerdfonts
  ];

}
