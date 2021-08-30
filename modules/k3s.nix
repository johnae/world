{lib, config, ...}:
let
  inherit (lib) mkOption mkIf mkMerge mkForce types optionals;
  inherit (builtins) concatStringsSep;
  cfg = config.services.k3s;
  k3sManifestsDir = "/var/lib/rancher/k3s/server/manifests";
in
{
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
    services.k3s.extraFlagsList = (optionals (cfg.uniqueNodeNames) [ "--with-node-id yes" ])
    ++ (optionals (cfg.disableFlannel && cfg.role == "server") [ "--flannel-backend=none" ])
    ++ (optionals (cfg.disableScheduler && cfg.role == "server") [ "--disable-scheduler" ])
    ++ (optionals (cfg.disableCloudController && cfg.role == "server") [ "--disable-cloud-controller" ])
    ++ (optionals (cfg.disableKubeProxy && cfg.role == "server") [ "--disable-kube-proxy" ])
    ++ (optionals (cfg.disableNetworkPolicy && cfg.role == "server") [ "--disable-network-policy" ]);
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
  };
}
