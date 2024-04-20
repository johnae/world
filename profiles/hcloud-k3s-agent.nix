{
  services.k3s = {
    enable = true;
    role = "agent";
    after = ["tailscale-auth.service" "metadata.service"];
    settings = {
      token-file = "/run/agenix/k3s-token";
      flannel-iface = "tailscale0";
      node-name = "\"$NODENAME\"";
      node-ip = "\"$(get-iface-ip tailscale0)\"";
      node-external-ip = "\"$(get-iface-ip eth0)\"";
      kubelet-arg.max-pods = 62;
      node-label."svccontroller.k3s.cattle.io/enablelb" = "true";
      node-label."topology.kubernetes.io/region" = "$\"REGION\"";
      node-label."topology.kubernetes.io/zone" = "\"$ZONE\"";
      node-label."hostname" = "\"$NODENAME\"";
    };
  };
}
