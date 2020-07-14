{ pkgs, lib, ... }:
let
  dir = ./cachix;
  toImport = name: _: dir + ("/" + name);
  isNix = key: value: value == "regular" && lib.hasSuffix ".nix" key;
  imports = lib.mapAttrsToList
    toImport
    (lib.filterAttrs isNix (
      builtins.readDir dir
    ));
in
{
  inherit imports;
}
