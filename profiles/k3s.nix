{pkgs, lib, config, hostName,...}:
let
  cfg = config.services.k3s;
in
{
  services.k3s.enable = true;
  services.k3s.disableFlannel = true;
  services.k3s.extraFlagsList = [
    "--node-label hostname=${hostName}"
    "--node-taint node.cilium.io/agent-not-ready=true:NoSchedule"
  ];
  services.k3s.autoDeployList = [
    ../files/k3s/cilium.yaml
  ];
  services.k3s.disable = [ "traefik" "local-storage" ];
  services.k3s.disableKubeProxy = true;
  networking.firewall.allowedTCPPorts = lib.mkIf (cfg.role == "server") [ 6443 ];
  networking.firewall.trustedInterfaces = [ "cni0" "flannel.1" "calico+" "cilium+" "lxc+" ];
  #networking.firewall.enable = lib.mkForce false;
  boot.kernel.sysctl."net.ipv4.conf.lxc*.rp_filter" = 0;
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
