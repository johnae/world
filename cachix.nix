{lib, ...}: let
  inherit (lib) hasSuffix mapAttrsToList filterAttrs;
  inherit (builtins) readDir;
  toImport = name: _: ./cachix + ("/" + name);
  isNix = filename: filetype: filetype == "regular" && hasSuffix ".nix" filename;
  imports =
    mapAttrsToList toImport
    (filterAttrs isNix (readDir ./cachix));
in {
  inherit imports;
}
