{
  lib,
  hostConfigurations,
  hostName,
  ...
}: let
  inherit (builtins) mapAttrs;
  inherit (lib) filterAttrs;

  syncthingDevices =
    mapAttrs (_: value: {id = value.syncthingDeviceID;})
    (filterAttrs (name: value: value.services.syncthing.enable && value.syncthingDeviceID != null && name != hostName) hostConfigurations);
in {
  services.syncthing.settings.devices = syncthingDevices;
}
