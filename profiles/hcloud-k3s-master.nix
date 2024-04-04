{
  pkgs,
  tailnet,
  ...
}: {
  services.k3s = {
    enable = true;
    role = "server";
    after = ["tailscale-auth.service"];
    settings = {
      token-file = "/run/agenix/k3s-token";
      flannel-iface = "tailscale0";
      node-ip = "\"$(get-iface-ip tailscale0)\"";
      node-external-ip = "\"$(get-iface-ip eth0)\"";
      advertise-address = "\"$(get-iface-ip tailscale0)\"";
      cluster-cidr = "10.128.128.0/21";
      service-cidr = "10.129.128.0/22";
      cluster-dns = "10.129.128.10";
      kubelet-arg.max-pods = 62;
      kube-controller-manager-arg.node-cidr-mask-size = 25;
      # kube-apiserver-arg.oidc-issuer-url = "https://id.9000.dev";
      # kube-apiserver-arg.oidc-username-claim = "email";
      # kube-apiserver-arg.oidc-groups-claim = "groups";
      # kube-apiserver-arg.oidc-client-id = "dex-auth";
      node-label."svccontroller.k3s.cattle.io/enablelb" = "true";
      node-label."topology.kubernetes.io/region" = "hetzner";
      node-label."topology.kubernetes.io/zone" = "hetzner-fi";
      secrets-encryption = true;
      node-taint = "CriticalAddonsOnly=true:NoExecute";
      tls-san = ["\"$(cat /etc/generated-hostname)\"" "\"$(cat /etc/generated-hostname)\".${tailnet}.ts.net" "\"$(awk -F- '{print $2}' < /etc/generated-hostname)\"" "\"$(awk -F- '{print $2}' < /etc/generated-hostname)\".${tailnet}.ts.net"];
    };
    autoDeploy = {
      kured = "${pkgs.kured-yaml}/kured.yaml";
      flux = "${pkgs.fluxcd-yaml}/flux.yaml";
      hetzner-csi-driver = "${pkgs.hetzner-csi-driver-yaml}/hetzner-csi-driver.yaml";
    };
  };
}
