{
  imports = [
    ./services.nix
    ./cleanboot.nix
    ./state.nix
    ./sleep-management.nix
    ./k3s.nix
    ./innernet.nix
    ./private-wireguard.nix
    ./config-from-data.nix
    ./host-config.nix
    ./user-config.nix
  ];
}
