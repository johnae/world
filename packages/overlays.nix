{ inputs, lib, ...}:

let
   inherit (lib) genAttrs;
   inherit (builtins) filter pathExists attrNames;

   pkgList = (filter (elem: ! (inputs.${elem} ? "sourceInfo") && pathExists (toString (./. + "/${elem}"))) (attrNames inputs)) ++ [
     "meson-061"
   ];

   libdrm24109 = pkgs: pkgs.libdrm.overrideAttrs(_: {
     version = "2.4.109";
     src = pkgs.fetchurl {
       url = "https://dri.freedesktop.org/libdrm/libdrm-2.4.109.tar.xz";
       sha256 = "sha256-YpNS4Iwf6EhiygRlmNigjOFNJqsl7h9HBPmT0HTLfyY=";
     };
   });

   wayland120 = pkgs: pkgs.wayland.overrideAttrs(_: {
     version = "1.20.0";
     src = pkgs.fetchFromGitLab {
       domain = "gitlab.freedesktop.org";
       owner = "wayland";
       repo = "wayland";
       rev = "1.20.0";
       sha256 = "sha256-N+riSb3F3vcyUlNdlSnOUrKdDtBcGn9rUyCAi3nel6E=";
     };
     patches = [
       (pkgs.writeText "patch.diff" ''
           From 378623b0e39b12bb04d3a3a1e08e64b31bd7d99d Mon Sep 17 00:00:00 2001
           From: Florian Klink <flokli@flokli.de>
           Date: Fri, 27 Nov 2020 10:22:20 +0100
           Subject: [PATCH] add placeholder for @nm@
           ---
           egl/meson.build | 2 +-
           1 file changed, 1 insertion(+), 1 deletion(-)
           diff --git a/egl/meson.build b/egl/meson.build
           index dee9b1d..e477546 100644
           --- a/egl/meson.build
           +++ b/egl/meson.build
           @@ -11,7 +11,7 @@ wayland_egl = library(

            executable('wayland-egl-abi-check', 'wayland-egl-abi-check.c')

           -nm_path = find_program('nm').path()
           +nm_path = find_program('${pkgs.stdenv.cc.targetPrefix}nm').path()

            test(
             'wayland-egl symbols check',
           --
           2.29.2
         '')
     ];
   });

in

(
  genAttrs pkgList (key: (
    (final: prev: { ${key} = prev.callPackage (./. + "/${key}") { inherit inputs; }; })
  ))
)

//

{
  libdrm24109 = (final: prev: { libdrm24109 = libdrm24109 prev; });
  wayland120 = (final: prev: { wayland120 = wayland120 prev; });
  wayland-protocols-master = (final: prev: { wayland-protocols-master = prev.callPackage ./wayland-protocols-master { wayland = final.wayland120; }; });
}

//

{
  world-updaters = import ./world-updaters-overlay.nix;
  wlroots-master = (final: prev: { wlroots-master = prev.callPackage ./wlroots-master { wayland-protocols = final.wayland-protocols-master; wayland = final.wayland120; }; });
  sway-unwrapped = (final: prev: { sway-unwrapped = prev.callPackage ./sway { wlroots = final.wlroots-master; wayland = final.wayland120; wayland-protocols = final.wayland-protocols-master; }; });
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
