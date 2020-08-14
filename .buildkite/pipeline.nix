## Generate the buildkite json like this on the command line:
##
## nix eval .#buildkite.pipeline --json

{ config, pkgs, lib, pkgsToCache, containers, nixosConfigurations, ... }:
let
  util = import ./util { inherit lib config; };
  inherit (lib) mapAttrsToList filterAttrs isDerivation;
  inherit (util) withBuildEnv onlyDerivations
    listToDepKey pkgBatches keysOf;

  containerNames = builtins.attrNames (onlyDerivations containers);
  pkgNames = mapAttrsToList (n: _: n) (filterAttrs (n: v: isDerivation v) pkgsToCache);
  hostNames = builtins.attrNames (onlyDerivations nixosConfigurations);
  pkgChunks = pkgBatches pkgNames;

  containersToDeploy = lib.filter (v: v != "argocd" && v != "buildkite-agent") containerNames;
  deployContainers =
    {
      steps.deploys = lib.listToAttrs (
        map
          (
            name:
            {
              inherit name;
              value = {
                agents.queue = "linux";
                dependsOn = keysOf cachePkgs.steps.commands;
              };
            }
          )
          containersToDeploy
      );
    };

  cachePkgs =
    {
      steps.commands = lib.listToAttrs
        (
          map
            (
              pkgs:
              {
                name = "${listToDepKey pkgs}-cachix";
                value = {
                  agents.queue = "linux";
                  label = ":nix: Cache pkgs: ${lib.concatStringsSep " " pkgs}";
                  command = withBuildEnv ''
                    ${lib.concatStringsSep "\n"
                      (map
                        (pkg: "world package ${pkg} | cachix push insane")
                      pkgs)
                    }
                  '';
                };
              }
            )
            pkgChunks
        );
    };

  inherit (config.steps) commands triggers inputs;
in
{
  imports = [
    ./modules/deploys.nix
    cachePkgs
    deployContainers
  ];

  steps = {

    deploys.argocd = {
      agents.queue = "linux";
      #runDeploy = false; ## do this manually for now
      dependsOn = keysOf cachePkgs.steps.commands;
      image = "johnae/argocd";
    };

    deploys.buildkite-agent = {
      agents.queue = "linux";
      waitForCompletion = false;
      dependsOn = (keysOf cachePkgs.steps.commands);
      deployDependsOn = (keysOf deployContainers.steps.deploys) ++ [ config.steps.deploys.argocd ];
      image = "johnae/buildkite-agent";
    };

    commands.build-hosts = {
      agents.queue = "linux";
      label = ":nix: Build machines";
      dependsOn = keysOf cachePkgs.steps.commands;
      env.NIX_TEST = "yep"; ## uses dummy secrets
      command = withBuildEnv ''
        ${lib.concatStringsSep "\n"
          (map
            (host: "world build ${host}")
          hostNames)
        }
      '';
    };

  };

}
