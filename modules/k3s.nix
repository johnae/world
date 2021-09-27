{pkgs, lib, config, ...}:
let
  inherit (lib) mkOption mkIf mkMerge mkForce types optionals;
  inherit (builtins) concatStringsSep;
  cfg = config.services.k3s;
  k3sManifestsDir = "/var/lib/rancher/k3s/server/manifests";
in
{
  options.services.k3s.maxPodsPerNode = mkOption {
    type = types.int;
    default = 110;
    description = "Max no of pods per node";
  };
  options.services.k3s.clusterCIDR = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "IPv4/IPv6 network CIDRs to use for pod IPs (default: 10.42.0.0/16)";
  };
  options.services.k3s.serviceCIDR = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "IPv4/IPv6 network CIDRs to use for service IPs (default: 10.43.0.0/16)";
  };
  options.services.k3s.nodeCIDRMaskSize = mkOption {
    type = types.nullOr types.int;
    default = 24;
    description = "IPv4/IPv6 node CIDR mask size to use for pod IPs on a node (default: 24)";
  };
  options.services.k3s.clusterDNS = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "IPv4 Cluster IP for coredns service. Should be in your service-cidr range (default: 10.43.0.10)";
  };
  options.services.k3s.flannelIface = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "The interface flannel should use to establish cluster networking";
  };
  options.services.k3s.nodeIP = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "The internal IP of the node";
  };
  options.services.k3s.nodeExternalIP = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "The external IP of the node";
  };
  options.services.k3s.advertiseAddress = mkOption {
    type = types.nullOr types.str;
    default = null;
    description = "The ipv4 address that apiserver uses to advertise to members of the cluster (default node-external-ip/node-ip)";
  };
  options.services.k3s.uniqueNodeNames = mkOption {
    type = types.bool;
    default = true;
  };
  options.services.k3s.autoDeployList = mkOption {
    type = types.listOf types.path;
    default = [];
  };
  options.services.k3s.disableFlannel = mkOption {
    type = types.bool;
    default = false;
  };
  options.services.k3s.after = mkOption {
    type = types.listOf types.str;
    default = [];
  };
  options.services.k3s.disable = mkOption {
    type = types.listOf (types.enum [ "coredns" "servicelb" "traefik" "local-storage" "metrics-server" ]);
    default = [];
  };
  options.services.k3s.disableScheduler = mkOption {
    type = types.bool;
    default = false;
  };
  options.services.k3s.disableCloudController = mkOption {
    type = types.bool;
    default = false;
  };
  options.services.k3s.disableKubeProxy = mkOption {
    type = types.bool;
    default = false;
  };
  options.services.k3s.disableNetworkPolicy = mkOption {
    type = types.bool;
    default = false;
  };
  options.services.k3s.extraFlagsList = mkOption {
    type = types.listOf types.str;
    default = [];
  };

  config = mkIf cfg.enable {
    services.k3s.extraFlagsList = (optionals (cfg.disableFlannel && cfg.role == "server") [ "--flannel-backend=none" ])
    ++ (optionals (cfg.disableScheduler && cfg.role == "server") [ "--disable-scheduler" ])
    ++ (optionals (cfg.disableCloudController && cfg.role == "server") [ "--disable-cloud-controller" ])
    ++ (optionals (cfg.disableKubeProxy && cfg.role == "server") [ "--disable-kube-proxy" ])
    ++ (optionals (cfg.disableNetworkPolicy && cfg.role == "server") [ "--disable-network-policy" ])
    ++ (optionals (cfg.clusterCIDR != null && cfg.role == "server") [ "--cluster-cidr ${cfg.clusterCIDR}" ])
    ++ (optionals (cfg.serviceCIDR != null && cfg.role == "server") [ "--service-cidr ${cfg.serviceCIDR}" ])
    ++ (optionals (cfg.clusterDNS != null && cfg.role == "server") [ "--cluster-dns ${cfg.clusterDNS}" ])
    ++ (optionals (cfg.role == "server") [ "--kube-controller-manager-arg=\"node-cidr-mask-size=${toString cfg.nodeCIDRMaskSize}\"" ])
    ++ (optionals (cfg.flannelIface != null) [ "--flannel-iface=\"${cfg.flannelIface}\"" ])
    ++ (optionals (cfg.nodeIP != null) [ "--node-ip=\"${cfg.nodeIP}\"" ])
    ++ (optionals (cfg.nodeExternalIP != null) [ "--node-external-ip=\"${cfg.nodeExternalIP}\"" ])
    ++ (optionals (cfg.role == "server" && cfg.advertiseAddress != null) [ "--advertise-address=\"${cfg.advertiseAddress}\"" ])
    ++ [ "--kubelet-arg=\"max-pods=${toString cfg.maxPodsPerNode}\"" ]
    ++ (optionals (cfg.uniqueNodeNames) [ "--with-node-id" ]);
    services.k3s.extraFlags = concatStringsSep " " cfg.extraFlagsList;
    systemd.services.k3s.preStart = mkIf (cfg.role == "server") ''
    mkdir -p ${k3sManifestsDir}
    ${concatStringsSep "\n" (map (manifestPath:
      "cp ${manifestPath} ${k3sManifestsDir}/"
      ) cfg.autoDeployList)
    }
    ${concatStringsSep "\n" (map (manifestName:
      "touch ${k3sManifestsDir}/${manifestName}.yaml.skip"
      ) cfg.disable)
    }
    '';
    ## Random fixes and hacks for k3s networking
    ## see: https://github.com/NixOS/nixpkgs/issues/98766
    boot.kernelModules = [ "br_netfilter" "ip_conntrack" "ip_vs" "ip_vs_rr" "ip_vs_wrr" "ip_vs_sh" "overlay" ];
    systemd.services.k3s.after = [ "network-online.service" "firewall.service" ] ++ cfg.after;
    ## Really stupid obviously
    systemd.timers.restart-k3s-on-boot = {
      description = "Hack for fixing k3s vxlan networking :-|";
      enable = true;
      wantedBy = [ "timers.target" ];
      timerConfig.OnStartupSec = if cfg.role == "server" then "3m" else "15m";
    };
    systemd.services.restart-k3s-on-boot = {
      description = "Hack for fixing k3s vxlan networking :-|";
      script = ''
        echo Restarting k3s
        ${pkgs.systemd}/bin/systemctl restart k3s.service
      '';
    };
  };
}
