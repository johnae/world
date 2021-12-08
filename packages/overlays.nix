{ inputs, lib, ...}:

let
   inherit (lib) genAttrs;
   inherit (builtins) filter pathExists attrNames;

   pkgList = (filter (elem: ! (inputs.${elem} ? "sourceInfo") && pathExists (toString (./. + "/${elem}"))) (attrNames inputs)) ++ [
     "meson-061"
   ];
in

(
  genAttrs pkgList (key: (
    (final: prev: { ${key} = prev.callPackage (./. + "/${key}") { inherit inputs; }; })
  ))
)

//

{
  world-updaters = import ./world-updaters-overlay.nix;
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

//

{
  wlroots = (final: prev: { wlroots = prev.callPackage ./wlroots { wayland-protocols = final.wayland-protocols-master; meson = prev.meson-061; }; });
  sway-unwrapped = (final: prev: { sway-unwrapped = prev.callPackage ./sway { wayland-protocols = final.wayland-protocols-master; meson = prev.meson-061; }; });
  swaylock = (final: prev: { swaylock = prev.callPackage ./swaylock { wayland-protocols = final.wayland-protocols-master; meson = prev.meson-061; }; });
}
