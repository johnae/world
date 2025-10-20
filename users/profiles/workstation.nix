{
  lib,
  pkgs,
  config,
  ...
}: let
  inherit (config) gtk;
in {
  imports =
    [./default.nix]
    ++ [
      ./alacritty.nix
      ./chromium.nix
      ./dconf.nix
      ./firefox.nix
      ./foot.nix
      ./fuzzel.nix
      ./hyprland.nix
      ./kanshi.nix
      # ./mako.nix
      ./niri.nix
      ./noctalia.nix
      ./obs.nix
      ./rio.nix
      ./river-luatile/default.nix
      ./river.nix
      ./rofi.nix
      # ./spotifyd.nix
      ./sway.nix
      ./waybar.nix
      ./wezterm/default.nix
      ./wlsunset.nix
    ];

  home.packages = with pkgs; [
    # element-desktop-wayland
    gimp
    kanshi
    lswt
    mako
    nautilus
    nordic
    playerctl
    river-luatile
    shotcut
    slack
    spotify
    tenacity
    wl-clipboard
    wl-clipboard-x11
    xdg-utils
  ];

  services.wpaperd = {
    enable = false;
    settings = {
      default = {
        path = "~/Sync/wallpapers";
        duration = "30m";
        sorting = "random";
        apply-shadow = false;
      };
    };
  };

  ## because https://github.com/nix-community/home-manager/issues/1213
  xdg.configFile."mimeapps.list".force = true;
  xdg.mime.enable = true; ## default is true
  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "application/x-extension-htm" = "Chromium.desktop";
      "application/x-extension-html" = "Chromium.desktop";
      "application/x-extension-shtml" = "Chromium.desktop";
      "application/x-extension-xhtml" = "Chromium.desktop";
      "application/x-extension-xht" = "Chromium.desktop";
    };

    defaultApplications = {
      "text/html" = "Chromium.desktop";
      "x-scheme-handler/http" = "Chromium.desktop";
      "x-scheme-handler/https" = "Chromium.desktop";
      "x-scheme-handler/about" = "Chromium.desktop";
      "x-scheme-handler/unknown" = "Chromium.desktop";
      "x-scheme-handler/chrome" = "Chromium.desktop";
      "application/x-exension-htm" = "Chromium.desktop";
      "application/x-exension-html" = "Chromium.desktop";
      "application/x-exension-shtml" = "Chromium.desktop";
      "application/xhtml+xml" = "Chromium.desktop";
      "application/x-exension-xhtml" = "Chromium.desktop";
      "application/x-exension-xht" = "Chromium.desktop";
    };
  };

  home.file."Pictures/default-background.jpg".source = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tri-fadeno.jpg";

  xdg.configFile."wireplumber/main.lua.d/50-libcamera-config.lua".source = pkgs.writeText "50-libcamera-config.lua" ''
    libcamera_monitor.enabled = false
  '';

  base16-theme.enable = true;

  qt = {
    enable = true;
    platformTheme.name = "adwaita";
    style.name = "adwaita-dark";
    style.package = pkgs.adwaita-qt;
  };

  home.sessionVariables = {
    XCURSOR_THEME = gtk.cursorTheme.name;
    QT_STYLE_OVERRIDE = lib.mkForce "gtk";
    # Wayland environment variables for all WMs
    GDK_BACKEND = "wayland";
    CLUTTER_BACKEND = "wayland";
    QT_QPA_PLATFORM = "";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    QT_WAYLAND_FORCE_DPI = "physical";
    SDL_VIDEODRIVER = "wayland";
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_USE_XINPUT2 = "1";
    NIXOS_OZONE_WL = "1";
    _JAVA_AWT_WM_NONREPARENTING = "1";
  };

  gtk = {
    enable = true;
    font = {
      package = pkgs.roboto;
      name = "Roboto Medium 11";
    };
    cursorTheme = {
      package = pkgs.nordzy-cursor-theme;
      name = "Nordzy-cursors";
    };
    iconTheme = {
      package = pkgs.arc-icon-theme;
      name = "Arc";
    };
    theme = {
      package = pkgs.nordic;
      name = "Nordic-darker";
    };
  };
}
