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
          google-cloud-sdk-gke = prev.google-cloud-sdk.withExtraComponents [prev.google-cloud-sdk.components.gke-gcloud-auth-plugin];
        })
      ];
    };
  };
}
