{
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./defaults.nix
    inputs.nixos-hardware.nixosModules.common-pc-ssd
  ];

  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 12288;
  };

  hardware.graphics.enable = true;

  hardware.graphics.extraPackages = [
    pkgs.intel-media-driver
    pkgs.vaapiIntel
    pkgs.vaapiVdpau
    pkgs.libvdpau-va-gl
    # pkgs.rocm-opencl-icd
    # pkgs.rocm-opencl-runtime
    pkgs.amdvlk
  ];

  sound.enable = false; ## see pipewire config below
  security.rtkit.enable = true;
  hardware.bluetooth.enable = true;
  networking.wireless.iwd.enable = true;

  environment.pathsToLink = ["/etc/gconf"];

  security.pam.services.swaylock = {
    text = ''
      auth include login
    '';
  };

  powerManagement.enable = true;
  powerManagement.powertop.enable = true;

  environment.persistence."/keep".directories = ["/var/cache/powertop"];

  virtualisation.docker.enable = false;
  virtualisation.podman.enable = true;
  virtualisation.podman.dockerCompat = true;

  programs.ssh.startAgent = true;

  services.pcscd.enable = true;

  programs.dconf.enable = true;

  services.avahi.enable = true;
  services.avahi.nssmdns4 = true;
  services.avahi.nssmdns6 = true;
  services.avahi.openFirewall = true;
  services.gvfs.enable = true;
  services.gnome.sushi.enable = true;
  services.openssh.enable = true;

  services.fwupd.enable = true;

  services.printing.enable = true;
  services.printing.drivers = [pkgs.gutenprint];
  services.ipp-usb.enable = true;

  services.dbus.packages = with pkgs; [gcr dconf gnome.sushi];
  services.udev.packages = with pkgs; [gnome.gnome-settings-daemon];

  environment.etc."systemd/sleep.conf".text = "HibernateDelaySec=8h";

  services.write-iwd-secrets.enable = true;

  ## taken from NixOS wiki
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  xdg.portal.enable = true;
  xdg.portal.wlr.enable = true;
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gtk];
  xdg.portal.config = let
    wlrConf = {
      default = ["wlr" "gtk"];
      "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
    };
  in {
    common = {
      default = ["gtk"];
      "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
    };
    river = wlrConf;
    sway = wlrConf;
    # Hyprland = {
    #   default = ["hyprland" "gtk"];
    #   "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
    # };
  };

  fonts.packages = with pkgs; [
    emacs-all-the-icons-fonts
    etBook
    font-awesome_5
    google-fonts
    powerline-fonts
    roboto
    (pkgs.nerdfonts.override {
      fonts = ["JetBrainsMono" "DroidSansMono" "Iosevka" "IosevkaTerm" "RobotoMono"];
    })
  ];

  machinePurpose = "workstation";
}
