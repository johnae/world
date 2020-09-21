{ lib, config }:
let
  inherit (builtins) length attrNames sort hashString;
  inherit (lib) take drop filterAttrs mapAttrsToList
    isDerivation last splitString concatStringsSep;

  withBuildEnv = cmd: ''
    eval "$(nix develop -c nix print-dev-env)"
    strict-bash <<'NIXSH'
    ${cmd}
    NIXSH
  '';

  onlyDerivations = filterAttrs (_: isDerivation);

  listToDepKey = pkgNames: hashString "sha256" (concatStringsSep "-" pkgNames);

  chunksOf = n: l:
    if builtins.length l > 0
    then [ (lib.take n l) ] ++ (chunksOf n (lib.drop n l))
    else [ ];

  pkgBatches = pkgNames: chunksOf (length pkgNames / 4 + 1) pkgNames;

  keysOf = mapAttrsToList (name: _: config.steps.commands."${name}");
in
{
  inherit chunksOf onlyDerivations withBuildEnv pkgBatches keysOf listToDepKey;
}
