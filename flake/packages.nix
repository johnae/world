{
  inputs,
  self,
  ...
}: {
  perSystem = {
    pkgs,
    lib,
    system,
    ...
  }: let
    inherit (lib // builtins) filterAttrs mapAttrs readDir mapAttrs';
    locallyDefinedPackages = mapAttrs (
      name: _: (pkgs.callPackage (../packages + "/${name}") {inherit inputs;})
    ) (filterAttrs (filename: type: type == "directory") (readDir ../packages));

    tofuProvider = provider:
      provider.override (oldArgs: {
        provider-source-address =
          lib.replaceStrings
          ["https://registry.terraform.io/providers"]
          ["registry.opentofu.org"]
          oldArgs.homepage;
      });
  in {
    packages =
      (
        mapAttrs' (hostname: config: {
          name = "${hostname}-diskformat";
          value = pkgs.callPackage ../utils/diskformat.nix {
            inherit config;
            inherit lib;
          };
        })
        self.nixosConfigurations
      )
      // locallyDefinedPackages
      // {
        world = pkgs.writeShellApplication {
          name = "world";
          runtimeInputs = with pkgs; [just nushell statix];
          text = ''
            just -f ${../Justfile} -d "$(pwd)" "$@"
          '';
        };
        tofuWithPlugins = pkgs.opentofu.withPlugins (
          p:
            map tofuProvider [p.null p.external p.hcloud p.cloudflare p.random]
        );
        zjstatus = inputs.zjstatus.packages.${system}.default;
        zwift = inputs.zwift.packages.${system}.default;
        helix-latest = inputs.helix.packages.${system}.helix;
        hyprland-unstable = inputs.hyprland.packages.${system}.hyprland;
        persway = inputs.persway.packages.${system}.default;
        wlroots-river = pkgs.wlroots_river_0_17_2;
        river = locallyDefinedPackages.river.override {
          wlroots_0_17 = pkgs.wlroots_river_0_17_2;
        };
        inherit
          (inputs.hyprland.packages.${system})
          hyprland-debug
          xdg-desktop-portal-hyprland
          ;
      };
  };
}
