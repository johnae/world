{hostName, ...}:
{
  services.k3s.enable = true;
  services.k3s.extraFlagsList = [
    "--node-label host=${hostName}"
  ];
  services.k3s.disable = [ "traefik" "local-storage" ];
  services.k3s.disableNetworkPolicy = true;
  networking.firewall.trustedInterfaces = [ "cni+" "flannel.1" "calico+" "cilium+" "lxc+" ];
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
