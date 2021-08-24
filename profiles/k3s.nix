{lib, config, hostName,...}:
{
  services.k3s.enable = true;
  services.k3s.extraFlagsList = [
    "--node-label hostname=${hostName}"
  ];
  environment.state."/keep" = {
    directories = [
      "/var/lib/dockershim"
      "/var/lib/k3s"
      "/var/lib/kubelet"
      "/var/lib/cni"
      "/var/lib/containerd"
    ];
  };
}
