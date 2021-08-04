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
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    spook = {
      url = "github:johnae/spook";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-misc.follows = "nix-misc";
    };
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
    rofi-nord-theme = { url = "github:undiabler/nord-rofi-theme"; flake = false; };
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
    sway = { url = "github:swaywm/sway"; flake = false; };
    swaybg = { url = "github:swaywm/swaybg"; flake = false; };
    swayidle = { url = "github:swaywm/swayidle"; flake = false; };
    swaylock = { url = "github:swaywm/swaylock"; flake = false; };
    wlroots = { url = "github:swaywm/wlroots"; flake = false; };
    wf-recorder = { url = "github:ammen99/wf-recorder"; flake = false; };
    wl-clipboard = { url = "github:bugaevc/wl-clipboard"; flake = false; };
    xdg-desktop-portal-wlr = { url = "github:emersion/xdg-desktop-portal-wlr/v0.2.0"; flake = false; };
  };

  outputs = { self, nixpkgs, ...} @ inputs:
    let
      inherit (nixpkgs.lib) genAttrs listToAttrs mapAttrsToList filterAttrs;
      inherit (builtins) filter attrNames pathExists toString mapAttrs hasAttr;
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = genAttrs supportedSystems;
      pkgs = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = mapAttrsToList (_: value: value) self.overlays;
      });
      nonFlakePkgList = filter (elem: ! (inputs.${elem} ? "sourceInfo") && pathExists (toString (./. + "/${elem}"))) (attrNames inputs);
      exportedPackages = mapAttrs (name: _: pkgs.x86_64-linux.${name}) (filterAttrs (name: _: (hasAttr name pkgs.x86_64-linux) && nixpkgs.lib.isDerivation pkgs.x86_64-linux.${name}) self.overlays);
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
        nixos-generators = (final: prev: { inherit (inputs.nixos-generators.packages.${prev.system}) nixos-generators; });
        sway-unwrapped = (final: prev: { sway-unwrapped = prev.callPackage ./sway { }; });
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
      ##### remove this when meson 0.58.1 is in nixpkgs
      //
      {
        meson-581 = (final: prev: { meson-581 = prev.meson.overrideAttrs
          (_:
            rec {
              pname = "meson";
              version = "0.58.1";
              src = prev.python3.pkgs.fetchPypi {
                inherit pname version;
                sha256 = "0padn0ykwz8azqiwkhi8p97bl742y8lsjbv0wpqpkkrgcvda6i1i";
              };
            }
          );
        });
        wlroots = (final: prev: { wlroots = prev.callPackage ./wlroots { meson = prev.meson-581; }; });
        sway-unwrapped = (final: prev: { sway-unwrapped = prev.callPackage ./sway { meson = prev.meson-581; }; });
      }
      ###############################################
     ;
     packages.x86_64-linux = exportedPackages;
     devShell = forAllSystems (system:
       pkgs.${system}.callPackage ./devshell.nix { }
     );
    };
}
