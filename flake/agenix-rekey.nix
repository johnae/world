{self, ...}: {
  perSystem = {
    config,
    pkgs,
    ...
  }: {
    agenix-rekey = {
      inherit (self) nixosConfigurations darwinConfigurations homeConfigurations;
      collectHomeManagerConfigurations = true;
    };
  };
}
