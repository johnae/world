{ config, pkgs, lib, name, pkgsToCache, containers, nixosConfigurations, ... }:

let

  inherit (lib) isDerivation mapAttrsToList filterAttrs listToAttrs;

  onlyDerivations = filterAttrs (_: isDerivation);
  hostNames = builtins.attrNames nixosConfigurations;
  pkgNames = mapAttrsToList (n: _: n) (filterAttrs (n: v: isDerivation v) pkgsToCache);

  task = command: {
    taskRef.name = name;
    params = {
      giturl = "$(params.giturl)";
      gitrev = "$(params.gitrev)";
      command = ''
        eval "$(nix print-dev-env)"
        strict-bash <<SH
        ${command}
        SH
      '';
    };
  };
in
{
  imports = [ ./modules/tasks.nix ];
  resources.pipelines.${name}.spec = {
    params = {
      giturl.type = "string";
      gitrev.type = "string";
    };
    tasks = (listToAttrs (map
      (
        name: {
          name = "build-${name}";
          value = task "world package ${name} | cachix push insane";
        }
      )
      pkgNames)) //
    (listToAttrs (map
      (
        name: {
          name = "build-host-${name}";
          value = (task "world build ${name}") // { runAfter = map (n: "build-${n}") pkgNames; };
        }
      )
      hostNames));
  };
}
