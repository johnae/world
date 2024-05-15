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

  microvm.vcpu = lib.mkForce 2;
  microvm.mem = lib.mkForce 8192;
}
