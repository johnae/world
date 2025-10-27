{
  inputs,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./defaults.nix
    ./restic-helper.nix
    inputs.nixos-hardware.nixosModules.common-pc-ssd
  ];

  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 65536;
  };

  hardware.graphics.enable = true;

  hardware.graphics.extraPackages = [
    pkgs.intel-media-driver
    pkgs.vaapiIntel
    pkgs.vaapiVdpau
    pkgs.libvdpau-va-gl
    pkgs.rocmPackages.clr
    pkgs.rocmPackages.clr.icd
  ];

  services.logind.settings.Login = {
    IdleAction = "suspend-then-hibernate";
    IdleActionSec = "30min";
  };

  security.rtkit.enable = true;
  hardware.bluetooth.enable = true;
  networking.wireless.iwd.enable = true;
  networking.networkmanager.enable = true;
  networking.networkmanager.wifi.backend = "iwd";

  environment.pathsToLink = ["/etc/gconf"];

  security.pam.services.swaylock = {
    text = ''
      auth include login
    '';
  };

  security.polkit.enable = true;
  services.gnome.gnome-keyring.enable = true;

  systemd.user.services.polkit-gnome-authentication-agent-1 = {
    description = "polkit-gnome-authentication-agent-1";
    wantedBy = ["graphical-session.target"];
    wants = ["graphical-session.target"];
    after = ["graphical-session.target"];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  powerManagement.enable = true;
  powerManagement.powertop.enable = true;

  environment.persistence."/keep".directories = ["/var/cache/powertop"];

  virtualisation.docker.enable = false;
  virtualisation.podman.enable = true;
  virtualisation.podman.dockerCompat = true;

  programs.ssh.startAgent = true;
  services.gnome.gcr-ssh-agent.enable = lib.mkForce false;

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

  services.dbus.packages = with pkgs; [gcr dconf sushi];
  services.udev.packages = with pkgs; [gnome-settings-daemon];

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
  xdg.portal.extraPortals = [pkgs.xdg-desktop-portal-gnome];
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
    niri = {
      default = ["gnome" "gtk"];
      "org.freedesktop.impl.portal.Access" = ["gtk"];
      "org.freedesktop.impl.portal.Notification" = ["gtk"];
      "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
    };
    river = wlrConf;
    sway = wlrConf;

  fonts.packages = with pkgs; [
    emacs-all-the-icons-fonts
    etBook
    font-awesome_5
    google-fonts
    powerline-fonts
    roboto
    nerd-fonts.jetbrains-mono
    nerd-fonts.droid-sans-mono
    nerd-fonts.iosevka
    nerd-fonts.iosevka-term
    nerd-fonts.roboto-mono
  ];

  machinePurpose = "workstation";
}
