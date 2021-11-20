{
  description = "Packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-misc = {
      url = "github:johnae/nix-misc";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell = {
      url = "github:johnae/devshell";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    spotnix = {
      url = "github:johnae/spotnix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fenix.follows = "fenix";
    };
    persway = {
      url = "github:johnae/persway";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.fenix.follows = "fenix";
    };
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## non flakes
    rofi-wayland = { url = "github:lbonn/rofi/wayland"; flake = false; };
    age-plugin-yubikey = { url = "github:str4d/age-plugin-yubikey"; flake = false; };
    blur = { url = "github:johnae/blur"; flake = false; };
    fire = { url = "github:johnae/fire"; flake = false; };
    fish-kubectl-completions = { url = "github:evanlucas/fish-kubectl-completions"; flake = false; };
    google-cloud-sdk-fish-completion = { url = "github:Doctusoft/google-cloud-sdk-fish-completion"; flake = false; };
    grim = { url = "github:emersion/grim"; flake = false; };
    nixpkgs-fmt = { url = "github:nix-community/nixpkgs-fmt"; flake = false; };
    netns-exec = { url = "github:johnae/netns-exec"; flake = false; };
    slurp = { url = "github:emersion/slurp"; flake = false; };
    spotifyd = { url = "github:spotifyd/spotifyd"; flake = false; };
    wayland-protocols-master = { url = "git+https://gitlab.freedesktop.org/wayland/wayland-protocols?ref=main"; flake = false; };
    sway = { url = "github:swaywm/sway"; flake = false; };
    swaybg = { url = "github:swaywm/swaybg"; flake = false; };
    swayidle = { url = "github:swaywm/swayidle"; flake = false; };
    swaylock = { url = "github:swaywm/swaylock"; flake = false; };
    wlroots = { url = "git+https://gitlab.freedesktop.org/wlroots/wlroots?ref=master"; flake = false; };
    wf-recorder = { url = "github:ammen99/wf-recorder"; flake = false; };
    wl-clipboard = { url = "github:bugaevc/wl-clipboard"; flake = false; };
    xdg-desktop-portal-wlr = { url = "github:emersion/xdg-desktop-portal-wlr/v0.5.0"; flake = false; };
    git-branchless = { url = "github:arxanas/git-branchless"; flake = false; };
    pueue = { url = "github:Nukesor/pueue"; flake = false; };
  };

  outputs = { self, nixpkgs, ...} @ inputs:
    let
      inherit (nixpkgs.lib) genAttrs listToAttrs mapAttrsToList filterAttrs;
      inherit (builtins) filter attrNames pathExists toString mapAttrs hasAttr;

      overlays = mapAttrsToList (_: value: value) self.overlays;
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f: genAttrs supportedSystems (system:
        f (import nixpkgs {
          inherit system overlays;
        })
      );

      extraPkgs = [ "meson-0591" ];
      nonFlakePkgList = (filter (elem: ! (inputs.${elem} ? "sourceInfo") && pathExists (toString (./. + "/${elem}"))) (attrNames inputs)) ++ extraPkgs;
      exportedPackages = forAllSystems (pkgs:
        (mapAttrs (name: _: pkgs.${name})
          (filterAttrs (name: _: (hasAttr name pkgs) && nixpkgs.lib.isDerivation pkgs.${name}) self.overlays)
        )
      );
    in
    {
      overlays = (
        mapAttrs (_: value: value.overlay) (filterAttrs (_: v: v ? "overlay") inputs)
      )
      //
      (genAttrs nonFlakePkgList (key: (
        (final: prev: { ${key} = prev.callPackage (./. + "/${key}") { }; })
      )))
      //
      {
        world-updaters = import ./world-updaters-overlay.nix;
        devshell = inputs.devshell.overlay;
        nixos-generators = (final: prev: { inherit (inputs.nixos-generators.packages.${prev.system}) nixos-generators; });
        wlroots = (final: prev: { wlroots = prev.callpackage ./wlroots { wayland-protocols = final.wayland-protocols-master; }; });
        sway-unwrapped = (final: prev: { sway-unwrapped = prev.callpackage ./sway { wayland-protocols = final.wayland-protocols-master; }; });
        sway = (final: prev: { sway = prev.callPackage (prev.path + "/pkgs/applications/window-managers/sway/wrapper.nix") { }; } );
        inputs = (final: prev: { inherit inputs; });
        mynerdfonts = (final: prev: { mynerdfonts = prev.nerdfonts.override { fonts = [ "JetBrainsMono" "DroidSansMono" ]; }; });
        swaylock-dope = (final: prev: { swaylock-dope = prev.callPackage ./swaylock-dope { }; });
        my-emacs = (final: prev: { my-emacs = prev.callPackage ./my-emacs { }; });
        my-emacs-config = (final: prev: { my-emacs-config = prev.callPackage ./my-emacs/config.nix { }; });
        btr-snap = (final: prev: { btr-snap = prev.callPackage ./btr-snap { }; });
        wl-clipboard-x11 = (final: prev: { wl-clipboard-x11 = prev.callPackage ./wl-clipboard-x11 { }; });
        rust-analyzer-bin = (final: prev: { rust-analyzer-bin = prev.callPackage ./wl-clipboard-x11 { }; });
        netns-dbus-proxy = (final: prev: { netns-dbus-proxy = prev.callPackage ./wl-clipboard-x11 { }; });
        scripts = (final: prev: { scripts = prev.callPackage ./scripts { }; });
      }
      ##### remove this when meson 0.59.1 is in nixpkgs
      //
      {
        #meson-581 = (final: prev: { meson-581 = prev.meson.overrideAttrs
        #  (_:
        #    rec {
        #      pname = "meson";
        #      version = "0.58.1";
        #      src = prev.python3.pkgs.fetchPypi {
        #        inherit pname version;
        #        sha256 = "0padn0ykwz8azqiwkhi8p97bl742y8lsjbv0wpqpkkrgcvda6i1i";
        #      };
        #    }
        #  );
        #});
        wlroots = (final: prev: { wlroots = prev.callPackage ./wlroots { wayland-protocols = final.wayland-protocols-master; meson = prev.meson-0591; }; });
        sway-unwrapped = (final: prev: { sway-unwrapped = prev.callPackage ./sway { wayland-protocols = final.wayland-protocols-master; meson = prev.meson-0591; }; });
        swaylock = (final: prev: { swaylock = prev.callPackage ./swaylock { wayland-protocols = final.wayland-protocols-master; meson = prev.meson-0591; }; });
      }
      ###############################################
     ;
     packages = exportedPackages;
     devShell = forAllSystems (pkgs:
       pkgs.devshell.mkShell {
         imports = [
           (pkgs.devshell.importTOML ./devshell.toml)
         ];
       }
     );
    };
}
