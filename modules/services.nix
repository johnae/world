{lib, ...}: {
  imports =
    lib.mapAttrsToList
    (
      name: _: import (./services + "/${name}")
    )
    (
      lib.filterAttrs
      (name: _: lib.hasSuffix ".nix" name)
      (builtins.readDir ./services)
    );
}
