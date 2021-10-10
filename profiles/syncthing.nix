{ hostName, lib, hostConfigs, ... }:

let
  inherit (builtins) hasAttr mapAttrs;
  inherit (lib) filterAttrs;

  syncthingDevices = mapAttrs (_: value: { id = value.syncthingDeviceID; })
    (filterAttrs (name: value: hasAttr "syncthingDeviceID" value && name != hostName) hostConfigs);

in

{
  services.syncthing.devices = syncthingDevices;
}
