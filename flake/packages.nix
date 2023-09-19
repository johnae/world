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
