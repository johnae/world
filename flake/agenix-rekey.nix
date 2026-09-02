{self, ...}: {
  perSystem = _: {
    agenix-rekey = {
      inherit (self) nixosConfigurations darwinConfigurations homeConfigurations;
      collectHomeManagerConfigurations = true;
    };
  };
}
