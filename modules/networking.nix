{hostConfig, config, lib, ...}:

let
  inherit (builtins) hasAttr mapAttrs attrNames length;
  inherit (lib) mkIf stringAfter;
  networking = if hasAttr "networking" hostConfig then hostConfig.networking else {};
in

{
  inherit networking;
}
