{
  lib,
  hostName,
  ...
}: let
  inherit (lib) filterAttrs mapAttrs;
  # Single source of truth for the mesh. Reading this static registry avoids
  # evaluating every peer host's full config just to collect their device IDs.
  devices = import ./syncthing-devices.nix;
in {
  services.syncthing.settings.devices =
    mapAttrs (_: id: {inherit id;})
    (filterAttrs (name: _: name != hostName) devices);
}
