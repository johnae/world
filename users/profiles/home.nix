{
  pkgs,
  config,
  ...
}: let
  inherit (config) home;
in {
  home.packages = [
    pkgs.alejandra
    pkgs.cachix
    pkgs.element-desktop-wayland
    pkgs.fzf # # for certain utilities that depend on it
    pkgs.git-branchless
    pkgs.git-crypt
    pkgs.gnome3.nautilus
    pkgs.google-cloud-sdk-gke
    pkgs.kanshi
    pkgs.kubectl
    pkgs.kubectx
    pkgs.kubelogin-oidc
    pkgs.kustomize
    pkgs.lm_sensors
    pkgs.mako
    pkgs.netns-dbus-proxy
    pkgs.nix-index
    pkgs.nordic
    pkgs.playerctl
    pkgs.pueue
    pkgs.rnix-lsp
    pkgs.rust-analyzer-bin
    pkgs.scripts
    pkgs.spotifyd
    pkgs.spotnix
    pkgs.wl-clipboard
    pkgs.wl-clipboard-x11
    pkgs.xdg_utils
  ];

  home.sessionVariables = {
    EDITOR = "emacsclient -c -a=";
    VISUAL = "emacsclient -c -a=";
    KUBECONFIG = "/home/${home.username}/.kube/config";
    COLORTERM = "truecolor";
  };

  xdg.enable = true;
  ## because https://github.com/nix-community/home-manager/issues/1213
  xdg.configFile."mimeapps.list".force = true;
  xdg.mime.enable = true; ## default is true
  xdg.mimeApps = {
    enable = true;
    associations.added = {
      "application/x-extension-htm" = "firefox.desktop";
      "application/x-extension-html" = "firefox.desktop";
      "application/x-extension-shtml" = "firefox.desktop";
      "application/x-extension-xhtml" = "firefox.desktop";
      "application/x-extension-xht" = "firefox.desktop";
    };

    defaultApplications = {
      "text/html" = "firefox.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/about" = "firefox.desktop";
      "x-scheme-handler/unknown" = "firefox.desktop";
      "x-scheme-handler/chrome" = "firefox.desktop";
      "application/x-exension-htm" = "firefox.desktop";
      "application/x-exension-html" = "firefox.desktop";
      "application/x-exension-shtml" = "firefox.desktop";
      "application/xhtml+xml" = "firefox.desktop";
      "application/x-exension-xhtml" = "firefox.desktop";
      "application/x-exension-xht" = "firefox.desktop";
    };
  };

  #home.file.".icons/default".source = "${pkgs.arc-icon-theme}/share/icons/Arc";
  home.file."Pictures/default-background.jpg".source = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tri-fadeno.jpg";

  xdg.configFile."wireplumber/main.lua.d/50-libcamera-config.lua".source = pkgs.writeText "50-libcamera-config.lua" ''
    libcamera_monitor.enabled = false
  '';

  base16-theme.enable = true;

  qt = {
    enable = true;
    platformTheme = "gnome";
    style.name = "adwaita-dark";
    style.package = pkgs.adwaita-qt;
  };

  gtk = {
    enable = true;
    font = {
      package = pkgs.roboto;
      name = "Roboto Medium 11";
    };
    cursorTheme = {
      package = pkgs.arc-icon-theme;
      name = "Arc";
    };
    iconTheme = {
      package = pkgs.arc-icon-theme;
      name = "Arc";
    };
    theme = {
      package = pkgs.nordic;
      name = "Nordic";
    };
  };

  programs.command-not-found.enable = false;

  programs.lsd = {
    enable = true;
    enableAliases = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.skim.enable = true;

  systemd.user.services.nix-index = {
    Unit.Description = "Nix-index indexes all files in nixpkgs etc";
    Service.ExecStart = "${pkgs.nix-index}/bin/nix-index";
  };

  systemd.user.timers.nix-index = {
    Unit.Description = "Nix-index indexes all files in nixpkgs etc";
    Timer = {
      OnCalendar = "daily";
      Unit = "nix-index.service";
    };
    Install.WantedBy = ["timers.target"];
  };

  services.syncthing.enable = true;

  home.stateVersion = "21.05";
}
