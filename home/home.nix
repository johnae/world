{ pkgs, config, nixosConfig, lib, options }:
let
  importsFrom = dir: lib.mapAttrsToList
    (
      name: _: dir + "/${name}"
    )
    (
      lib.filterAttrs
        (name: _: lib.hasSuffix ".nix" name)
        (builtins.readDir dir)
    );

  home = config.home;

in
{
  imports = (
    importsFrom ../hm-modules
  ) ++ (
    builtins.filter (path: path != ./home.nix) (importsFrom ./.)
  );

  home.packages =
    [
      pkgs.sway
      pkgs.swaybg
      pkgs.swayidle
      pkgs.swaylock
      pkgs.xwayland
      pkgs.iw
      pkgs.mako
      pkgs.spotifyd
      pkgs.spotnix
      pkgs.my-emacs
      pkgs.mu
      pkgs.bat
      pkgs.alacritty
      pkgs.project-select
      pkgs.launch
      pkgs.git-credential-pass
      pkgs.sk-sk
      pkgs.sk-run
      pkgs.sk-window
      pkgs.sk-passmenu
      pkgs.add-wifi-network
      pkgs.update-wifi-networks
      pkgs.update-wireguard-keys
      pkgs.initialize-user
      pkgs.nix-index
      pkgs.git-crypt
      pkgs.wl-clipboard
      pkgs.wl-clipboard-x11
      pkgs.wf-recorder
      pkgs.nordic
      pkgs.nordic-polar
      pkgs.nixpkgs-fmt
      pkgs.google-cloud-sdk
      pkgs.kubectl
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

  home.file.".emacs".source =
    (pkgs.callPackage ../pkgs/my-emacs/config.nix { }).emacsConfig;

  home.file.".icons/default" = {
    source = "${pkgs.arc-icon-theme}/share/icons/Arc";
  };

  home.file."Pictures/default-background.jpg" = {
    source = "${pkgs.adapta-backgrounds}/share/backgrounds/adapta/tri-fadeno.jpg";
  };

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

  programs.starship = {
    enable = true;
    settings = {
      kubernetes.disabled = false;
      kubernetes.style = "bold blue";
      nix_shell.disabled = false;
      nix_shell.use_name = true;
      rust.symbol = " ";
      gcloud = {
        format = "on [$symbol(\\($project\\))]($style) ";
      };
    };
  };

  programs.lsd = {
    enable = true;
    enableAliases = true;
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
  };

  programs.password-store.enable = true;
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

  home.stateVersion = "20.09";

}
