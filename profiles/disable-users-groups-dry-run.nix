{lib, ...}: {
  ## this breaks agenix currently if set to true
  ## see: https://github.com/ryantm/agenix/issues/55
  ## and: https://github.com/NixOS/nixpkgs/pull/136605
  system.activationScripts.users.supportsDryActivation = lib.mkForce false;
}
