{inputs, ...}: {
  perSystem = {
    pkgs,
    lib,
    system,
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

    mkRootPath = rel:
      builtins.path {
        path = "${inputs.helix}/${rel}";
        name = rel;
      };

    makeOverridableHelix = old: config: let
      grammars = pkgs.callPackage "${inputs.helix}/grammars.nix" config;
      runtimeDir = pkgs.runCommand "helix-runtime" {} ''
        mkdir -p $out
        ln -s ${mkRootPath "runtime"}/* $out
        rm -r $out/grammars
        ln -s ${grammars} $out/grammars
      '';
      helix-wrapped =
        pkgs.runCommand
        old.name
        {
          inherit (old) pname version;
          meta = old.meta or {};
          passthru =
            (old.passthru or {})
            // {
              unwrapped = old;
            };
          nativeBuildInputs = [pkgs.makeWrapper];
          makeWrapperArgs = config.makeWrapperArgs or [];
        }
        ''
          cp -rs --no-preserve=mode,ownership ${old} $out
          wrapProgram "$out/bin/hx" ''${makeWrapperArgs[@]} --set HELIX_RUNTIME "${runtimeDir}"
        '';
    in
      helix-wrapped
      // {
        override = makeOverridableHelix old;
        passthru =
          helix-wrapped.passthru
          // {
            wrapper = old: makeOverridableHelix old config;
          };
      };

    packages =
      locallyDefinedPackages
      // {
        inherit (pkgs.callPackage ../utils/world.nix {}) pixieboot world lint;
        conduit = inputs.matrix-conduit.packages.${system}.default;
      }
      // rec {
        ## packages from other flakes
        helix-unwrapped-latest = inputs.helix.packages.${system}.helix-unwrapped.overrideAttrs (oa: {
          src = pkgs.runCommand "patched-source" {} ''
            cp -r ${oa.src} $out
            chmod -R u+w $out
            cd $out
            patch -f -p1 -t < ${inputs.helix-copilot-patch} || true
          '';
        });
        helix-latest = makeOverridableHelix helix-unwrapped-latest {};
        hyprland-unstable = inputs.hyprland.packages.${system}.hyprland;
        inherit
          (inputs.hyprland.packages.${system})
          hyprland-unwrapped
          hyprland-debug
          xdg-desktop-portal-hyprland
          hyprland-protocols
          wlroots-hyprland
          udis86
          ;
        inherit (inputs.persway.packages.${system}) persway;
        inherit (inputs.headscale.packages.${system}) headscale;
      };
  in {
    inherit packages;
  };
}
