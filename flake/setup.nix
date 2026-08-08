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
        inputs.claude-code.overlays.default
        inputs.emacs-overlay.overlays.default
        # niri-flake hard-asserts libdisplay-info 0.2.0, but nixpkgs removed the
        # libdisplay-info_0_2 alias (deemed unused upstream). Source it from
        # niri-flake's own pinned nixpkgs, which still ships 0.2.0.
        (_final: _prev: {
          inherit (inputs.niri.inputs.nixpkgs.legacyPackages.${system}) libdisplay-info_0_2;
        })
        inputs.niri.overlays.niri
        inputs.nur.overlays.default
        (_final: _prev: (filterAttrs (name: _: ((match "nu-.*" name == null) && (match "nu_.*" name == null))) config.packages))
        (_final: prev: {
          nushell = prev.nushell.overrideAttrs (_oa: {
            doCheck = false;
          });
        })
        (_final: prev: {
          # afdko 5.x regressed variable-font autohinting, which breaks
          # cantarell-fonts' build (NixOS/nixpkgs#535887). Skip the VF autohint
          # step until afdko 5 support lands upstream; the font still builds and
          # installs, only the variable font is left unhinted.
          cantarell-fonts = prev.cantarell-fonts.overrideAttrs (old: {
            postPatch =
              (old.postPatch or "")
              + ''
                substituteInPlace scripts/make-variable-font.py \
                  --replace-fail "subprocess.check_call(" "0 and subprocess.check_call("
              '';
          });
        })
        (_final: prev: {
          google-cloud-sdk-gke = prev.google-cloud-sdk.withExtraComponents [
            prev.google-cloud-sdk.components.gke-gcloud-auth-plugin
            prev.google-cloud-sdk.components.config-connector
          ];
        })
      ];
    };
  };
}
