{
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN3wV0xe1C2JtwQHwHNL3yYnGsXPfnQAvElF37ux7qkc";

  imports = [
    ../../profiles/hcloud.nix
    ../../profiles/hcloud-k3s-master.nix
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

  services.k3s.settings.cluster-init = true;

  age.secrets = {
    ts-google-9k-hcloud.file = ../../secrets/ts-google-9k-hcloud.age;
    k3s-token.file = ../../secrets/k3s/token.age;
    tailscale-oauth-secret = {
      file = ../../secrets/k3s/tailscale-oauth-secret.yaml.age;
      path = "/var/lib/rancher/k3s/server/manifests/tailscale-oauth-secret.yaml";
    };
    cluster-secrets = {
      file = ../../secrets/k3s/cluster-secrets.yaml.age;
      path = "/var/lib/rancher/k3s/server/manifests/cluster-secrets.yaml";
    };
    hetzner-csi-encryption-secret = {
      file = ../../secrets/k3s/hetzner-csi-encryption-secret.yaml.age;
      path = "/var/lib/rancher/k3s/server/manifests/hetzner-csi-encryption-secret.yaml";
    };
    hetzner-api-secret = {
      file = ../../secrets/k3s/hetzner-api-secret.yaml.age;
      path = "/var/lib/rancher/k3s/server/manifests/hetzner-api-secret.yaml";
    };
  };

  services.tailscale.auth = {
    enable = true;
    after = ["metadata.service"];
    args.advertise-tags = ["tag:server" "tag:hcloud"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/var/run/agenix/ts-google-9k-hcloud";
    args.hostname = "\"$NODENAME\"";
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
  ];
}
