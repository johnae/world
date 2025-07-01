{...}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ7m3AH34sXUa8UQIeDEyL2lpUwSbsrGjzPrjYauntOH";

  imports = [
    ../../profiles/hcloud.nix
    ../../profiles/hcloud-k3s-agent.nix
    ../../profiles/hcloud-remote-unlock.nix
    ../../profiles/disk/disko-basic.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = true; ## we're running kured on k8s for synchronized reboots
  };

  services.k3s.settings.server = "https://\"$INITIAL_MASTER\":6443";

  age.secrets = {
    k3s-token.file = ../../secrets/k3s/token.age;
  };

  services.tailscale.auth = {
    enable = true;
    after = ["metadata.service"];
    args.advertise-tags = ["tag:server" "tag:hcloud" "tag:k8s"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "\"$TS_AUTH_KEY\"";
    args.hostname = "\"$NODENAME\"";
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
  ];
}
