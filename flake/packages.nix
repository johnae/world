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

    packages =
      locallyDefinedPackages
      // rec {
        inherit (pkgs.callPackage ../utils/world.nix {}) pixieboot world lint;
        conduit = inputs.matrix-conduit.packages.${system}.default;
      }
      // {
        ## packages from other flakes
        #inherit (inputs.spotnix.packages.${system}) spotnix;
        inherit (inputs.persway.packages.${system}) persway;
        inherit (inputs.headscale.packages.${system}) headscale;
      };
  in {
    inherit packages;
  };
}
