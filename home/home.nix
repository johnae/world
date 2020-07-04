{ pkgs, config, lib, options }:
let
  chromium-dev-ozone = import (
    builtins.fetchTarball {
      url =
        "https://github.com/colemickens/nixpkgs-chromium/archive/6b6ba812f3ba52922f668e48684ced76883e775c.tar.gz";
      sha256 = "0d5gmcnalh3x154mg40cx70d48a9nvn5x8kkcp2xxp0cha6hqh96";
    }
  );
  importsFrom = with lib; dir: mapAttrsToList
    (
      name: _: dir + "/${name}"
    )
    (
      filterAttrs
        (name: _: hasSuffix ".nix" name)
        (builtins.readDir dir)
    );
in
{
  #nixpkgs.config = import ../nix/nixpkgs-config.nix;
  #nixpkgs.overlays = import ../nix/overlays/overlays.nix { };

  imports = with lib; (
    importsFrom ../hm-modules
  ) ++ (
    filter (path: path != ./home.nix) (importsFrom ./.)
  );

  home.packages = with pkgs;
    [
      sway
      swaybg
      swayidle
      swaylock
      xwayland
      iw
      mako
      spotifyd
      spotnix
      my-emacs
      mu
      bat
      mail
      alacritty
      project-select
      launch
      git-credential-pass
      sk-sk
      sk-run
      sk-window
      sk-passmenu
      add-wifi-network
      update-wifi-networks
      update-wireguard-keys
      initialize-user
      #spotify-cmd
      #spotify-play-album
      #spotify-play-track
      #spotify-play-artist
      #spotify-play-playlist
      wl-clipboard
      wl-clipboard-x11
      wf-recorder
      nordic
      nordic-polar
      wayvnc
      nixpkgs-fmt
      google-cloud-sdk
      kubectl
      kustomize
      fzf # # for certain utilities that depend on it
      rust-analyzer-bin
      rnix-lsp
      xdg_utils
      netns-dbus-proxy
      spook
      gnome3.nautilus
      chromium-dev-ozone
      #(pkgs.firejailed { package = chrpkgs.chromium-dev-wayland; ignore = [ "nou2f" ]; })
    ];

  home.sessionVariables = rec {
    EDITOR = "emacsclient -t -a=";
    VISUAL = EDITOR;
  };

  xsession.pointerCursor = {
    package = pkgs.arc-icon-theme;
    name = "Arc";
  };

  xdg.enable = true;

  #xdg.configFile."nixpkgs/config.nix".source = ../nix/nixpkgs-config.nix;
  #xdg.configFile."nix/nix.conf".source = pkgs.writeText "nix.conf" ''
  #  substituters = ${lib.concatStringsSep " " config.nixpkgs.config.nix.binaryCaches}
  #  trusted-public-keys = ${lib.concatStringsSep " " config.nixpkgs.config.nix.binaryCachePublicKeys}
  #'';

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

  programs.git = {
    enable = true;
    userName = "John Axel Eriksson";
    userEmail = "john@insane.se";
    signing = {
      key = "0x04ED6F42C62F42E9";
      signByDefault = true;
    };
    extraConfig = {
      core.editor = "${pkgs.my-emacs}/bin/emacsclient -t -a=";
      push.default = "upstream";
      pull.rebase = true;
      rebase.autoStash = true;
      url."git@github.com:".insteadOf = "https://github.com/";
      color = {
        ui = "auto";
        branch = "auto";
        status = "auto";
        diff = "auto";
        interactive = "auto";
        grep = "auto";
        decorate = "auto";
        showbranch = "auto";
        pager = true;
      };
      credential = {
        "https://github.com" = {
          username = "johnae";
          helper = "pass web/github.com/johnae";
        };
        "https://repo.insane.se" = {
          username = "johnae";
          helper = "pass web/repo.insane.se/johnae";
        };
      };
    };
  };

  programs.command-not-found = {
    enable = true;
    dbPath = "${./..}/programs.sqlite";
  };

  programs.starship = {
    enable = true;
    settings = {
      kubernetes.disabled = false;
      kubernetes.style = "bold blue";
      nix_shell.disabled = false;
      nix_shell.use_name = true;
      rust.symbol = " ";
    };
  };

  programs.lsd = {
    enable = true;
    enableAliases = true;
  };

  programs.direnv = {
    enable = true;
    ## use lorri if available
    stdlib = ''
      eval "`declare -f use_nix | sed '1s/.*/_&/'`"
      use_nix() {
        if type lorri &>/dev/null; then
          echo "direnv: using lorri from PATH ($(type -p lorri))"
          eval "$(lorri direnv)"
        else
          _use_nix
        fi
      }
    '';
  };

  programs.password-store.enable = true;
  programs.skim.enable = true;

  services.lorri.enable = true;
  systemd.user.services.lorri.Service.Environment = with lib; mkForce
    (
      let
        path = with pkgs;
          makeSearchPath "bin" [ nixFlakes gitMinimal gnutar gzip ];
      in
      [ "PATH=${path}" ]
    );

  services.syncthing.enable = true;

}
