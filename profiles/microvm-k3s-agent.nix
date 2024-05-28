{...}: {
  imports = [
    ../profiles/microvm-k3s.nix
  ];

  services.k3s = {
    enable = true;
    role = "agent";
    settings.server = "https://\"$INITIAL_MASTER\":6443";
  };
}
