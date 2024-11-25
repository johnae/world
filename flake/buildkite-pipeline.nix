{
  withSystem,
  lib,
  self,
  ...
}: let
  inherit (lib) mapAttrsToList filterAttrs hasPrefix filter elem;
  defaultSkip = [
    "container-"
    "container-processes"
    "container-shell"
    "devenv-up"
  ];
  matrix = withSystem "x86_64-linux" (
    ctx @ {pkgs, ...}: let
      skip =
        (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
        ++ defaultSkip;
    in
      filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) ctx.config.packages)
  );
in {
  flake = {
    buildkite-pipeline = {
      env = {
        CACHE_NAME = "insane";
        NIX_CONFIG = "accept-flake-config = true";
      };
      steps = [
        {
          group = ":broom: Linting and syntax checks";
          key = "checks";
          steps = [
            {
              label = ":nix: Lint";
              command = "nix run .#world -- lint";
            }
            {
              label = ":nix: Check";
              command = "nix run .#world -- check";
            }
          ];
        }
        {
          group = ":hammer_and_pick: Building packages";
          key = "packages";
          steps = [
            {
              label = ":nix: {{matrix}} build";
              command = "nix build .#packages.x86_64-linux.{{matrix}} -L";
              inherit matrix;
            }
          ];
        }
      ];
    };
  };
}
