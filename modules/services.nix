{ config, lib, pkgs, ... }:

with lib;

{
  imports = mapAttrsToList
    (
      name: _: import (./services + "/${name}")
    )
    (
      filterAttrs
        (name: _: hasSuffix ".nix" name)
        (builtins.readDir ./services)
    );
}
