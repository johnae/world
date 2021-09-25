{pkgs, config, lib, ...}:

let
  inherit (builtins) attrNames;
  inherit (lib) filterAttrs mapAttrs;
  privateInterfaces = filterAttrs (_: v: v.interfaceNamespace != null) config.networking.wireguard.interfaces;
in
{
  config = mkIf (length (attrNames privateInterfaces) > 0) {
    networking.wireguard.interfaces = mapAttrs (_: v: v // {
      preSetup = ''
        ${pkgs.iproute2}/bin/ip netns add ${v.interfaceNamespace}
      '';
    }) privateInterfaces;
  };
}
