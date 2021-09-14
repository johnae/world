{
  imports = [
    ./services.nix
    ./cleanboot.nix
    ./state.nix
    ./sleep-management.nix
    ./unifi-config.nix
    ./k3s.nix
    ./innernet.nix
    ./config-from-data.nix
    ./host-config.nix
    ./user-config.nix
  ];
}
