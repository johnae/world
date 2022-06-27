{
  lib,
  pkgs,
  config,
  hostName,
  ...
}: let
  inherit (lib) optionals mkIf;
  cfg = config.services.k3s;
in {
  systemd.enableUnifiedCgroupHierarchy = false;
  services.k3s.settings.node-label.hostname = hostName;
  services.k3s.disable = ["traefik"];

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
