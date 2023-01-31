{inputs, ...}: {
  perSystem = {
    pkgs,
    lib,
    ...
  }: let
    inherit
      (lib // builtins)
      filterAttrs
      filter
      pathExists
      attrNames
      readDir
      mapAttrs
      ;
    pkgList =
      filter
      (elem:
        ! (inputs.${elem} ? "sourceInfo")
        && pathExists (toString (../packages + "/${elem}")))
      (attrNames inputs);
    locallyDefinedPackages = mapAttrs (
      name: _: (pkgs.callPackage (../packages + "/${name}") {inherit inputs;})
    ) (filterAttrs (filename: type: type == "directory") (readDir ../packages));

    packages =
      locallyDefinedPackages
      // rec {
        inherit (pkgs.callPackage ../utils/world.nix {}) pixieboot world lint;
        my-emacs-config = pkgs.callPackage ../packages/my-emacs/config.nix {};
        mynerdfonts = pkgs.nerdfonts.override {fonts = ["JetBrainsMono" "DroidSansMono"];};

        libxkbcommon-150 = pkgs.libxkbcommon.overrideAttrs (oa: rec {
          pname = "libxkbcommon";
          version = "1.5.0";
          src = pkgs.fetchurl {
            url = "https://xkbcommon.org/download/${pname}-${version}.tar.xz";
            sha256 = "sha256-Vg8RxLu8oQ9JXz7306aqTKYrT4+wtS59RZ0Yom5G4Bc=";
          };
        });
        wlroots-master = pkgs.callPackage ../packages/wlroots-master {
          wayland-protocols = locallyDefinedPackages.wayland-protocols-master;
        };
        sway-unwrapped = pkgs.callPackage ../packages/sway {
          wlroots = wlroots-master;
          wayland-protocols = locallyDefinedPackages.wayland-protocols-master;
          libxkbcommon = libxkbcommon-150;
        };
        sway = pkgs.callPackage (pkgs.path + "/pkgs/applications/window-managers/sway/wrapper.nix") {};
        swayidle = pkgs.callPackage ../packages/swayidle {
          wayland-protocols = locallyDefinedPackages.wayland-protocols-master;
        };
      };
  in {
    inherit packages;
  };
}
