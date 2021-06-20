{ pkgs, config, nixosConfig, lib, options, ... }:
let
  home = config.home;
in
{
  home.packages =
    [
      pkgs.mako
      pkgs.spotifyd
      pkgs.spotnix
      pkgs.my-emacs
      pkgs.scripts
      pkgs.nix-index
      pkgs.git-crypt
      pkgs.wl-clipboard
      pkgs.wl-clipboard-x11
      pkgs.wf-recorder
      pkgs.nordic
      pkgs.nixpkgs-fmt
      pkgs.google-cloud-sdk
      pkgs.kubectl
      pkgs.kubectx
      pkgs.kustomize
      pkgs.fzf # # for certain utilities that depend on it
      pkgs.rust-analyzer-bin
      pkgs.rnix-lsp
      pkgs.xdg_utils
      pkgs.netns-dbus-proxy
      pkgs.spook
      pkgs.gnome3.nautilus
      pkgs.cachix
      pkgs.lm_sensors
    ];

  home.sessionVariables = {
    EDITOR = "emacsclient -c -a=";
    VISUAL = "emacsclient -c -a=";
    KUBECONFIG = "/home/${home.username}/.kube/config";
  };

  xsession.pointerCursor = {
    package = pkgs.arc-icon-theme;
    name = "Arc";
  };

  xdg.enable = true;
  ## because https://github.com/nix-community/home-manager/issues/1213
  xdg.configFile."mimeapps.list".force = true;
  xdg.mime.enable = true; ## default is true
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "firefox.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/about" = "firefox.desktop";
      "x-scheme-handler/unknown" = "firefox.desktop";
    };
  };

  home.file.".emacs".source = "${pkgs.my-emacs-config}/emacs.el";
  #home.file.".icons/default".source = "${pkgs.arc-icon-theme}/share/icons/Arc";
  home.file."Pictures/default-background.jpg".source = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tri-fadeno.jpg";

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
    Install.WantedBy = [ "timers.target" ];
  };

  services.syncthing.enable = true;

  home.stateVersion = "21.05";

}
