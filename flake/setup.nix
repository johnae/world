{
  inputs,
  lib,
  self,
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
        inputs.devshell.overlay
        inputs.agenix.overlays.default
        inputs.devshell.overlay
        inputs.emacs-overlay.overlay
        inputs.fenix.overlays.default
        inputs.nur.overlay

        (final: prev: (filterAttrs (name: _: ((match "nu-.*" name == null) && (match "nu_.*" name == null))) config.packages))

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
