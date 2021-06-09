{ lib }:

let
  inherit (lib) nameValuePair mapAttrs' removeSuffix;
  inherit (builtins) readDir;
in
mapAttrs' (name: _: nameValuePair (removeSuffix ".nix" name) (./profiles + "/${name}")) (builtins.readDir ./profiles)
