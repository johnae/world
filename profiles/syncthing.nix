{
  lib,
  hostConfigurations,
  ...
}: let
  inherit (builtins) hasAttr mapAttrs;
  inherit (lib) filterAttrs;

  syncthingDevices =
    mapAttrs (_: value: {id = value.syncthingDeviceID;})
    (filterAttrs (name: value: value.services.syncthing.enable && value.syncthingDeviceID != null) hostConfigurations);
in {
  services.syncthing.settings.devices = syncthingDevices;
}
