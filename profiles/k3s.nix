{lib, config, hostName,...}:
let
  cfg = config.services.k3s;
in
{
  services.k3s.enable = true;
  services.k3s.extraFlagsList = [
    "--node-label hostname=${hostName}"
  ] ++ (lib.optional (cfg.role == "server") "--write-kubeconfig /kubeconfig");
  networking.firewall.allowedTCPPorts = lib.mkIf (cfg.role == "server") [ 6443 ];
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
