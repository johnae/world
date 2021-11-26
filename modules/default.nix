{
  imports = [
    ./services.nix
    ./cleanboot.nix
    ./state.nix
    ./sleep-management.nix
    ./k3s.nix
    ./innernet.nix
    ./router.nix
    ./private-wireguard.nix
    ./config-from-data.nix
    ./host-config.nix
    ./home.nix
  ];
}
