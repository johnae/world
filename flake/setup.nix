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
        (_final: _prev: (filterAttrs (name: _: ((match "nu-.*" name == null) && (match "nu_.*" name == null))) config.packages))
        (_final: prev: {
          awscli2 = prev.awscli2.overrideAttrs (oa: {
            patches = [
              # Temporary test fix until https://github.com/aws/aws-cli/pull/8838 is merged upstream
              (prev.fetchpatch {
                url = "https://github.com/aws/aws-cli/commit/b5f19fe136ab0752cd5fcab21ff0ab59bddbea99.patch";
                hash = "sha256-NM+nVlpxGAHVimrlV0m30d4rkFVb11tiH8Y6//2QhMI=";
              })
            ];
          });
        })
        (_final: prev: {
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

        (_final: prev: {
          inherit inputs;
          google-cloud-sdk-gke = prev.google-cloud-sdk.withExtraComponents [
            prev.google-cloud-sdk.components.gke-gcloud-auth-plugin
            prev.google-cloud-sdk.components.config-connector
          ];
        })
      ];
    };
  };
}
