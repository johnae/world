{
  lib,
  pkgs,
  config,
  ...
}: let
  l = lib // builtins;
  inherit
    (l)
    optional
    mkOption
    mkIf
    mkForce
    types
    mapAttrsToList
    flatten
    concatStringsSep
    isList
    isString
    isPath
    isAttrs
    isBool
    mapAttrs
    sort
    lessThan
    ;
  cfg = config.services.k3s;
  k3sManifestsDir = "/var/lib/rancher/k3s/server/manifests";
  containerdConfigDir = "/var/lib/rancher/k3s/agent/etc/containerd";
  getIfaceIp = pkgs.writeShellApplication {
    name = "get-iface-ip";
    runtimeInputs = with pkgs; [jq iproute2];
    text = ''
      IFACE=''${1:-}
      if [ "$IFACE" = "" ]; then
        echo Please provide the interface to get the ip off of
        exit 1
      fi
      ip -o -4 -family inet -json addr show scope global dev "$IFACE" | jq -r '.[0].addr_info[0].local'
    '';
  };
  getDefaultRouteIp = pkgs.writeShellApplication {
    name = "get-default-route-ip";
    runtimeInputs = with pkgs; [jq iproute2];
    text = ''
      IFACE="$(ip -json route get 8.8.8.8 | jq -r .[].dev)"
      ${getIfaceIp}/bin/get-iface-ip "$IFACE"
    '';
  };
  getIfaceWithMac = pkgs.writeShellApplication {
    name = "get-iface-with-mac";
    runtimeInputs = with pkgs; [jq iproute2];
    text = ''
      MAC=''${1:-}
      if [ "$MAC" = "" ]; then
        echo Please provide the mac address to get the interface name
        exit 1
      fi
      ip -o -json link show | jq -r ".[] | select(.address == \"$MAC\") | .ifname"
    '';
  };
  settingsToCli = s: let
    boolToCli = path: value:
      if value
      then "--${path}"
      else "";
    listToCli = path: value:
      concatStringsSep " "
      (map (item: "--${path} ${toString item}") value);
    attrsToCli = path:
      mapAttrsToList (
        k: v:
          if isBool v
          then boolToCli path v
          else if v == null
          then ""
          else "--${path} ${k}=${toString v}"
      );
    fieldToCli = path: value:
      if isAttrs value
      then attrsToCli path value
      else if isBool value
      then boolToCli path value
      else if isList value
      then listToCli path value
      else if value == null
      then ""
      else "--${path} ${toString value}";
  in
    flatten (mapAttrsToList fieldToCli s);
in {
  options.services.k3s.allowedReplacementVars = mkOption {
    type = types.listOf types.str;
    default = [
      "$CLUSTER_ID"
      "$NODE_ID"
      "$NODENAME"
      "$TS_AUTH_KEY"
      "$REGION"
      "$ZONE"
      "$INITIAL_MASTER"
    ];
    apply = concatStringsSep " ";
  };
  options.services.k3s.autoDeploy = mkOption {
    type = types.attrsOf (
      types.either
      types.path
      (types.attrsOf types.anything)
    );
    default = {};
    apply = mapAttrs (name: value:
      if (isPath value || isString value)
      then value
      else
        pkgs.runCommand "${name}.yaml" {} ''
          cat<<'EOF'>$out
          ${builtins.toJSON value}
          EOF
        '');
  };

  options.services.k3s.after = mkOption {
    type = types.listOf types.str;
    default = [];
  };

  options.services.k3s.disable = mkOption {
    type = types.listOf (types.enum ["coredns" "servicelb" "traefik" "local-storage" "metrics-server" "runtimes"]);
    default = [];
  };

  options.services.k3s.settings = mkOption {
    type = types.attrsOf types.anything;
    default = {};
  };

  config = mkIf cfg.enable {
    assertions = mkForce [];
    services.k3s.extraFlags = concatStringsSep " " (sort lessThan (settingsToCli cfg.settings));
    systemd.services.k3s = let
      k3s = pkgs.writeShellApplication {
        name = "k3s";
        runtimeInputs = with pkgs; [getIfaceIp getDefaultRouteIp getIfaceWithMac gawk gettext];
        text =
          concatStringsSep " "
          ([
              "exec ${cfg.package}/bin/k3s ${cfg.role}"
            ]
            ++ (optional cfg.clusterInit "--cluster-init")
            ++ (optional cfg.disableAgent "--disable-agent")
            ++ (optional (cfg.serverAddr != "") "--server ${cfg.serverAddr}")
            ++ (optional (cfg.token != "") "--token ${cfg.token}")
            ++ (optional (cfg.tokenFile != null) "--token-file ${cfg.tokenFile}")
            ++ (optional (cfg.configPath != null) "--config ${cfg.configPath}")
            ++ [cfg.extraFlags]);
      };
    in {
      after = ["network-online.service" "firewall.service"] ++ cfg.after;
      preStart = ''
        rm -f ${containerdConfigDir}/config.toml.tmpl
        ${
          if cfg.role == "server"
          then ''
            mkdir -p ${k3sManifestsDir}
            ${
              concatStringsSep "\n" (mapAttrsToList (
                  name: path: "${pkgs.gettext}/bin/envsubst '${cfg.allowedReplacementVars}' < ${path} > ${k3sManifestsDir}/${name}.yaml"
                )
                cfg.autoDeploy)
            }
            ${
              concatStringsSep "\n" (map (
                  manifestName: "touch ${k3sManifestsDir}/${manifestName}.yaml.skip"
                )
                cfg.disable)
            }
          ''
          else ""
        }
      '';
      serviceConfig.EnvironmentFile = lib.mkForce "/run/nixos/metadata";
      serviceConfig.ExecStart = lib.mkForce "${k3s}/bin/k3s";
    };
    ## Random fixes and hacks for k3s networking
    ## see: https://github.com/NixOS/nixpkgs/issues/98766
    boot.kernelModules = ["br_netfilter" "ip_conntrack" "ip_vs" "ip_vs_rr" "ip_vs_wrr" "ip_vs_sh" "overlay"];
  };
}
