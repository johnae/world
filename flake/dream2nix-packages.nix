{inputs, ...}: {
  imports = [
    inputs.dream2nix.flakeModuleBeta
  ];
  perSystem = {
    pkgs,
    lib,
    config,
    system,
    ...
  }: let
    dream2nix = {
      inputs.age-plugin-yubikey = {
        source = inputs.age-plugin-yubikey;
        packageOverrides."^.*".addDeps = {
          overrideAttrs = old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.pkg-config];
            buildInputs = (old.buildInputs or []) ++ [pkgs.pcsclite pkgs.openssl];
            doCheck = false;
          };
        };
        projects.age-plugin-yubikey = {
          subsystem = "rust";
          translator = "cargo-lock";
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
        projects.spotifyd = {
          subsystem = "rust";
          translator = "cargo-lock";
        };
      };

      inputs.ristate = {
        source = inputs.ristate;
        packageOverrides."^.*".addDeps.doCheck = false;
        projects.ristate = {
          subsystem = "rust";
          translator = "cargo-lock";
        };
      };

      inputs.netns-exec = {
        source = inputs.netns-exec;
        packageOverrides."^.*".addDeps.doCheck = false;
        projects.netns-exec = {
          subsystem = "rust";
          translator = "cargo-lock";
        };
      };

      inputs.kile = {
        source = inputs.kile;
        packageOverrides."^.*".addDeps.doCheck = false;
        projects.kile = {
          subsystem = "rust";
          translator = "cargo-lock";
        };
      };

      inputs.blur = {
        source = inputs.blur;
        packageOverrides."^.*".addDeps.doCheck = false;
        projects.blur = {
          subsystem = "rust";
          translator = "cargo-lock";
        };
      };

      inputs.conduit = {
        source = inputs.matrix-conduit;
        packageOverrides."^.*".addDeps = {
          overrideAttrs = old: {
            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.rustPlatform.bindgenHook pkgs.pkg-config];
            buildInputs = (old.buildInputs or []) ++ [pkgs.rocksdb];
            doCheck = false;
            cargoBuildFlags = "--no-default-features --features conduit_bin,backend_sqlite,backend_rocksdb";
          };
        };
        projects.conduit = {
          subsystem = "rust";
          translator = "cargo-lock";
        };
      };

      inputs.nu = {
        source = inputs.nushell-unstable;
        projects.nu = {
          subsystem = "rust";
          translator = "cargo-lock";
        };
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
    inherit dream2nix;
    packages = builtins.mapAttrs (k: v: v.packages.${k}) config.dream2nix.outputs;
  };
}
