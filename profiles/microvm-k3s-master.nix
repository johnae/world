{
  hostName,
  lib,
  tailnet,
  ...
}: {
  imports = [
    ../profiles/microvm-k3s.nix
  ];

  services.k3s = {
    enable = true;
    role = "server";
    settings = {
      server = "https://\"$INITIAL_MASTER\":6443";
      advertise-address = "\"$(get-iface-ip tailscale0)\"";
      cluster-cidr = "10.128.128.0/21";
      service-cidr = "10.129.128.0/22";
      cluster-dns = "10.129.128.10";
      kube-controller-manager-arg.node-cidr-mask-size = 25;
      secrets-encryption = true;
      node-taint = "CriticalAddonsOnly=true:NoExecute";
      tls-san = [hostName "${hostName}.${tailnet}.ts.net"];
    };
  };

  age.secrets = {
    tailscale-oauth-secret = {
      file = ../secrets/k3s/tailscale-oauth-secret.yaml.age;
      path = "/var/lib/rancher/k3s/server/manifests/tailscale-oauth-secret.yaml";
    };
    cluster-secrets = {
      file = ../secrets/k3s/cluster-secrets.yaml.age;
      path = "/var/lib/rancher/k3s/server/manifests/cluster-secrets.yaml";
    };
  };

  microvm.vcpu = lib.mkForce 2;
  microvm.mem = lib.mkForce 8192;
}
