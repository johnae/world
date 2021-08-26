{pkgs, lib, config, hostName,...}:
let
  cfg = config.services.k3s;
in
{
  services.k3s.enable = true;
  services.k3s.docker = true;
  services.k3s.package = pkgs.k3s-io;
  services.k3s.disableFlannel = true;
  services.k3s.extraFlagsList = [
    "--node-label hostname=${hostName}"
    "--kubelet-arg cgroup-driver=systemd"
  ];
  services.k3s.autoDeployList = [
    ../files/k3s/cilium.yaml
  ];
  services.k3s.disable = [ "traefik" "local-storage" ];
  services.k3s.disableKubeProxy = true;
  networking.firewall.allowedTCPPorts = lib.mkIf (cfg.role == "server") [ 6443 ];
  networking.firewall.allowedUDPPorts = [ 8472 6081 ];
  boot.kernel.sysctl = {
    "net.ipv4.conf.lxc*.rp_filter" = 0;
  };
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
