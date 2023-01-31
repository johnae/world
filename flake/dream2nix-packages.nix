{inputs, ...}: {
  perSystem = {
    pkgs,
    lib,
    config,
    ...
  }: let
    dream2nix = {
      inputs.age-plugin-yubikey = {
        source = inputs.age-plugin-yubikey;
        packageOverrides."^.*".addDeps = {
          overrideAttrs = old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.pkg-config];
            buildInputs = (old.buildInputs or []) ++ [pkgs.pcsclite];
            doCheck = false;
          };
        };
      };

      inputs.spotifyd = {
        source = inputs.spotifyd;
        packageOverrides."^.*".addDeps = {
          overrideAttrs = old: {
            nativeBuildInputs = with pkgs; (old.nativeBuildInputs or []) ++ [pkg-config];
            buildInputs = with pkgs; (old.buildInputs or []) ++ [libpulseaudio openssl alsa-lib dbus];
            doCheck = false;
            cargoBuildFlags = ["--features pulseaudio_backend,dbus_mpris"];
          };
        };
      };

      inputs.ristate = {
        source = inputs.ristate;
        packageOverrides."^.*".addDeps.doCheck = false;
      };

      inputs.netns-exec = {
        source = inputs.netns-exec;
        packageOverrides."^.*".addDeps.doCheck = false;
      };

      inputs.kile = {
        source = inputs.kile;
        packageOverrides."^.*".addDeps.doCheck = false;
      };

      inputs.blur = {
        source = inputs.blur;
        packageOverrides."^.*".addDeps.doCheck = false;
      };

      inputs.matrix-conduit = {
        source = inputs.matrix-conduit;
        packageOverrides."^.*".addDeps = {
          overrideAttrs = old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.rustPlatform.bindgenHook pkgs.pkg-config];
            buildInputs = (old.buildInputs or []) ++ [pkgs.rocksdb];
            doCheck = false;
            cargoBuildFlags = "--no-default-features --features conduit_bin,backend_sqlite,backend_rocksdb";
          };
        };
      };

      inputs.nushell = {
        source = inputs.nushell;
        projects.nu.translator = "cargo-lock";
        projects.nu.subsystem = "rust";
        packageOverrides."^.*".addDeps = {
          overrideAttrs = old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.pkg-config pkgs.python3];
            buildInputs = (old.buildInputs or []) ++ [pkgs.openssl pkgs.zstd pkgs.xorg.libX11];
            doCheck = false;
            buildFeatures = ["extra"];
            cargoUpdateHook = ''
              cargo add zstd-sys --features pkg-config --offline
              cargo update --package zstd-sys --offline
            '';
            shellPath = "/bin/nu";
          };
        };
      };
    };
  in {
    #packages = lib.mkMerge (
    #  lib.mapAttrsToList (_: output: lib.mkDefaultRecursive output.packages or {}) (lib.filterAttrs (name: _: (builtins.match "(nu-.*|nu_.*)" name) == null) (builtins.toJSON (builtins.attrNames dream2nix) dream2nix))
    #);
    inherit dream2nix;
    packages.default = config.packages.world;
    packages.resolveImpure = pkgs.hello;
  };
}
