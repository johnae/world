{
  inputs,
  lib,
  ...
}: {
  perSystem = {
    config,
    system,
    ...
  }: let
    inherit (lib // builtins) filterAttrs match;
  in {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        inputs.agenix.overlays.default
        #inputs.fenix.overlays.default
        inputs.nur.overlay
        (final: prev: (filterAttrs (name: _: ((match "nu-.*" name == null) && (match "nu_.*" name == null))) config.packages))

        (final: prev: {
          wlroots_river_0_17_2 = prev.wlroots_0_17.overrideAttrs (oa: rec {
            version = "0.17.2";
            src = prev.fetchFromGitLab {
              domain = "gitlab.freedesktop.org";
              owner = "wlroots";
              repo = "wlroots";
              rev = version;
              hash = "sha256-Of9qykyVnBURc5A2pvCMm7sLbnuuG7OPWLxodQLN2Xg=";
            };
            patches = [];
            buildInputs =
              oa.buildInputs
              ++ [
                prev.ffmpeg
                prev.hwdata
                prev.libliftoff
                prev.libdisplay-info
              ];
          });
        })

        (final: prev: {
          inherit inputs;
          google-cloud-sdk-gke =
            (prev.google-cloud-sdk.withExtraComponents [
              prev.google-cloud-sdk.components.gke-gcloud-auth-plugin
              prev.google-cloud-sdk.components.config-connector
            ])
            .overrideAttrs (oa: {
              buildCommand =
                oa.buildCommand
                + ''
                  if [ -e $out/google-cloud-sdk/bin/config-connector ]; then
                    echo patching config-connector binary
                    chmod +w $out/google-cloud-sdk/bin/config-connector
                    ${prev.patchelf}/bin/patchelf --set-interpreter "${prev.gcc}/nix-support/dynamic-linker" $out/google-cloud-sdk/bin/config-connector
                    chmod 744 $out/google-cloud-sdk/bin/config-connector
                  fi
                '';
            });
        })
      ];
    };
  };
}
