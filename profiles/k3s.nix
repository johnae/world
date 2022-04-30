{hostName, ...}: {
  services.k3s.enable = true;
  services.k3s.extraFlagsList = [
    "--node-label hostname=${hostName}"
    "--kube-apiserver-arg=\"oidc-issuer-url=https://dex.insane.se\""
    "--kube-apiserver-arg=\"oidc-username-claim=email\""
    "--kube-apiserver-arg=\"oidc-groups-claim=groups\""
    "--kube-apiserver-arg=\"oidc-client-id=insane\""
  ];
  services.k3s.disable = ["traefik"];
  services.k3s.disableNetworkPolicy = true;
  services.k3s.autoDeployList = [
    ../files/kubernetes/kured.yaml
  ];
  networking.firewall.trustedInterfaces = ["cni+" "flannel.1" "calico+" "cilium+" "lxc+"];
  environment.state."/keep" = {
    directories = [
      "/etc/rancher"
      "/var/lib/dockershim"
      "/var/lib/rancher"
      "/var/lib/kubelet"
      "/var/lib/cni"
      "/var/lib/containerd"
    ];
  };
}
