{self, ...}: {
  perSystem = {
    config,
    pkgs,
    ...
  }: {
    agenix-rekey = {
      nixosConfigurations = self.nixosConfigurations;
      darwinConfigurations = self.darwinConfigurations;
      homeConfigurations = self.homeConfigurations;
      collectHomeManagerConfigurations = true;
    };
  };
}
