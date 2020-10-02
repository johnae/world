{ config, pkgs, lib, name, pkgsToCache, containers, nixosConfigurations, ... }:

let

  inherit (lib) isDerivation mapAttrsToList filterAttrs
    listToAttrs take drop concatStringsSep;
  inherit (builtins) attrNames length;

  onlyDerivations = filterAttrs (_: isDerivation);
  hostNames = attrNames nixosConfigurations;
  pkgNames = mapAttrsToList (n: _: n) (filterAttrs (n: v: isDerivation v) pkgsToCache);

  chunksOf = n: l:
    if length l > 0
    then [ (take n l) ] ++ (chunksOf n (drop n l))
    else [ ];

  pkgBatches = pkgNames: chunksOf (length pkgNames / 4 + 1) pkgNames;

  pkgChunks = pkgBatches pkgNames;

  task = script: {
    taskRef.name = name;
    params = {
      giturl.value = "$(params.giturl)";
      gitrev.value = "$(params.gitrev)";
      command.value = ''
        eval "$(nix print-dev-env)"
        strict-bash <<SH
        ${script}
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
    tasks = listToAttrs (map
      (
        pkgList: {
          name = "build-${concatStringsSep "-" pkgList}";
          value = task (
            concatStringsSep "\n" (
              map (pkg: "world package ${pkg} | cachix push insane") pkgList
            )
          );
        }
      )
      pkgChunks);
  };
}
