{inputs, ...}: {
  perSystem = {
    pkgs,
    lib,
    system,
    ...
  }: let
    inherit (lib // builtins) attrNames pathExists filter filterAttrs mapAttrs readDir;
    locallyDefinedPackages = mapAttrs (
      name: _: (pkgs.callPackage (../packages + "/${name}") {inherit inputs;})
    ) (filterAttrs (filename: type: type == "directory") (readDir ../packages));
  in {
    packages =
      locallyDefinedPackages
      // {
        world = pkgs.writeShellApplication {
          name = "world";
          runtimeInputs = with pkgs; [just nushell statix];
          text = ''
            just -f ${../Justfile} -d "$(pwd)" "$@"
          '';
        };
        helix-latest = inputs.helix.packages.${system}.helix;
        hyprland-unstable = inputs.hyprland.packages.${system}.hyprland;
        persway = inputs.persway.packages.${system}.default;
        inherit
          (inputs.hyprland.packages.${system})
          hyprland-unwrapped
          hyprland-debug
          xdg-desktop-portal-hyprland
          hyprland-protocols
          wlroots-hyprland
          udis86
          ;
      };
  };
}
