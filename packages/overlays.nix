{
  inputs,
  lib,
  ...
}: let
  inherit (lib) genAttrs;
  inherit (builtins) filter pathExists attrNames;

  pkgList =
    filter (elem: ! (inputs.${elem} ? "sourceInfo") && pathExists (toString (./. + "/${elem}"))) (attrNames inputs);
in
  (
    genAttrs pkgList (key: (final: prev: {${key} = prev.callPackage (./. + "/${key}") {inherit inputs;};}))
  )
  // {
    wayland-protocols-master = final: prev: {wayland-protocols-master = prev.callPackage ./wayland-protocols-master {};};
  }
  // {
    hwdata-master = final: prev: {
      hwdata-master = prev.stdenv.mkDerivation rec {
        pname = "hwdata";
        version = inputs.hwdata.rev;
        src = inputs.hwdata;

        postPatch = ''
          patchShebangs ./configure
        '';

        configureFlags = ["--datadir=${placeholder "out"}/share"];

        doCheck = false;

        meta = with lib; {
          homepage = "https://github.com/vcrhonek/hwdata";
          description = "Hardware Database, including Monitors, pci.ids, usb.ids, and video cards";
          license = licenses.gpl2Plus;
          platforms = platforms.all;
          maintainers = with maintainers; [lovesegfault];
        };
      };
    };

    world-updaters = import ./world-updaters-overlay.nix;
    wlroots-master = final: prev: {
      wlroots-master = prev.callPackage ./wlroots-master {
        wayland-protocols = final.wayland-protocols-master;
        hwdata = final.hwdata-master;
      };
    };
    sway-unwrapped = final: prev: {
      sway-unwrapped = prev.callPackage ./sway {
        wlroots = final.wlroots-master;
        wayland-protocols = final.wayland-protocols-master;
      };
    };
    sway = final: prev: {sway = prev.callPackage (prev.path + "/pkgs/applications/window-managers/sway/wrapper.nix") {};};
    swayidle = final: prev: {
      swayidle = prev.callPackage ./swayidle {
        wayland-protocols = final.wayland-protocols-master;
      };
    };
    inputs = final: prev: {inherit inputs;};
    mynerdfonts = final: prev: {mynerdfonts = prev.nerdfonts.override {fonts = ["JetBrainsMono" "DroidSansMono"];};};
    swaylock-dope = final: prev: {swaylock-dope = prev.callPackage ./swaylock-dope {};};
    my-emacs = final: prev: {my-emacs = prev.callPackage ./my-emacs {};};
    my-emacs-config = final: prev: {my-emacs-config = prev.callPackage ./my-emacs/config.nix {};};
    btr-snap = final: prev: {btr-snap = prev.callPackage ./btr-snap {};};
    wl-clipboard-x11 = final: prev: {wl-clipboard-x11 = prev.callPackage ./wl-clipboard-x11 {};};
    rust-analyzer-bin = final: prev: {rust-analyzer-bin = prev.callPackage ./wl-clipboard-x11 {};};
    netns-dbus-proxy = final: prev: {netns-dbus-proxy = prev.callPackage ./wl-clipboard-x11 {};};
    scripts = final: prev: {scripts = prev.callPackage ./scripts {};};
  }
