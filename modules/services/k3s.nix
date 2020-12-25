{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.k3s;
  isAgent = cfg.masterUrl != null;
  isMaster = !isAgent;
  k3sNodeNameGen = ''
    if [ ! -e /etc/k3s-node-name ]; then
      echo "${cfg.nodeName}-$(${pkgs.openssl}/bin/openssl rand -hex 4)" > /etc/k3s-node-name
    fi
  '';
in
{
  ## note: there is already a k3s service option defined in NixOS - this extends (and overrides some of)
  ## the existing one so that we can "do more stuff"
  options.services.k3s = {

    systemdAfter = mkOption {
      type = types.listOf types.str;
      example = [ "network.target" ];
      default = [ ];
      description = ''
        Additional systemd targets, services etc which this should
        run after.
      '';
    };

    kubeConfigPath = mkOption {
      type = types.str;
      example = "/path/to/kubeconfig.yaml";
      default = "/kubeconfig.yaml";
      description = ''
        Where k3s should write the kubeconfig
      '';
    };

    k3sDir = mkOption {
      type = types.str;
      example = "/var/lib/k3s";
      default = "/var/lib/k3s";
      description = ''
        Where k3s should store its state
      '';
    };

    nodeName = mkOption {
      type = types.nullOr types.str;
      example = "somenode";
      default = null;
      description = ''
        The node name for the current node. A random string will be appended to this.
      '';
    };

    labels = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [ "label-one" "label-two" ];
      description = ''
        The node labels to apply to the current node.
      '';
    };

    tokenFile = mkOption {
      type = types.nullOr types.path;
      example = "/var/secrets/k3s-token";
      default = null;
      description = ''
        The shared cluster secret enabling agents to join the master.
        Either use this or the token attr.
      '';
    };

    flannelBackend = mkOption {
      type = with types; nullOr (enum [ "none" "vxlan" "ipsec" "wireguard" "host-gw" ]);
      default = "vxlan";
      description = ''
        The type of flannel networking to use. If set to none, you are free to
        use your own network plugin.
      '';
    };

    cniPackage = mkOption {
      type = with types; nullOr package;
      default = null;
      description = ''
        The cni package to use. If set to null, the defaults are used.
      '';
    };

    extraManifests = mkOption {
      type = types.listOf types.path;
      default = [ ];
      description = ''
        A list of paths to kubernetes manifests to automatically apply.
      '';
    };

    masterUrl = mkOption {
      type = types.nullOr (types.strMatching "https://[0-9a-zA-Z.]+.*");
      example = "https://1.2.3.4:6332";
      default = null;
      description = ''
        The url to the master node that agents should connect to. By not specifying this
        the current node is assumed to be the master node.
      '';
    };

  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = (cfg.tokenFile != null) -> !(cfg.token == "");
        message = "You can't use both tokenFile and token.";
      }
    ];
    services.k3s.role = if isAgent then "agent" else "server";
    services.k3s.serverAddr = if cfg.masterUrl != null then cfg.masterUrl else "";
    services.k3s.extraFlags =
      let
        cniBinDir = if cfg.cniPackage != null then "${cfg.cniPackage}/bin" else "${cfg.k3sDir}/opt/cni/bin";
      in
      concatStringsSep " \\\n "
        (if isAgent then
          [
            " "
            ''-d ${cfg.k3sDir}/data --kubelet-arg "volume-plugin-dir=${cfg.k3sDir}/libexec/kubernetes/kubelet-plugins/volume/exec"''
            ''--kubelet-arg "cni-bin-dir=${cniBinDir}"''
            (lib.optionalString (cfg.nodeName != null) ''--node-name "$(cat /etc/k3s-node-name)"'')
            (lib.concatStringsSep " "
              (map (v: "--node-label ${v}") (cfg.labels ++ (optional (cfg.nodeName != null) "hostname=${cfg.nodeName}")))
            )
            " "
          ]
        else
          [
            " "
            ''--no-deploy=traefik --no-deploy=servicelb --no-deploy=local-storage -d ${cfg.k3sDir}/data''
            ''-o ${cfg.kubeConfigPath}''
            ''--flannel-backend=${cfg.flannelBackend}''
            ''--kubelet-arg "volume-plugin-dir=${cfg.k3sDir}/libexec/kubernetes/kubelet-plugins/volume/exec"''
            ''--kubelet-arg "cni-bin-dir=${cniBinDir}"''
            ''--kube-controller-arg "flex-volume-plugin-dir=${cfg.k3sDir}/libexec/kubernetes/kubelet-plugins/volume/exec"''
            (lib.optionalString (cfg.nodeName != null) ''--node-name "$(cat /etc/k3s-node-name)"'')
            (lib.concatStringsSep " "
              (map (v: "--node-label ${v}") (cfg.labels ++ (optional (cfg.nodeName != null) "hostname=${cfg.nodeName}")))
            )
          ]);

    systemd.services.k3s = {

      after = [ "network-online.service" "firewall.service" ] ++ cfg.systemdAfter;

      preStart = ''
        ${lib.optionalString (cfg.nodeName != null) k3sNodeNameGen}
      '';

      postStart = (
        if isMaster
        then ''
          echo Applying extra kubernetes manifests...
          set -x
          ${lib.concatStringsSep "\n" (
            map
              (m: "${pkgs.kubectl}/bin/kubectl --kubeconfig ${cfg.kubeConfigPath} apply -f ${m}")
              cfg.extraManifests
          )}
        '' else ""
      );

      serviceConfig = {
        NotifyAccess = "all";
        LimitNOFILE = "infinity";
        LimitNPROC = "infinity";
        LimitCORE = "infinity";
        TasksMax = "infinity";
        TimeoutStartSec = 0;
        Restart = "always";

        ExecStart = with lib;
          mkForce (
            pkgs.writeStrictShellScript "unit-script-k3s-start"
              (
                concatStringsSep " \\\n "
                  (
                    [
                      "export PATH=${pkgs.wireguard}/bin:${pkgs.iptables}/bin:${pkgs.kmod}/bin:${pkgs.bash}/bin:${pkgs.mount}/bin:\${PATH:+:}$PATH \n"
                      "exec ${cfg.package}/bin/k3s ${cfg.role}"
                    ]
                    ++ (optional cfg.docker "--docker")
                    ++ (optional cfg.disableAgent "--disable-agent")
                    ++ (optional (cfg.role == "agent") "--server ${cfg.serverAddr}")
                    ++ (optional (cfg.token != "") "--token ${cfg.token}")
                    ++ (optional (cfg.tokenFile != null) "--token-file ${cfg.tokenFile}")
                    ++ [ cfg.extraFlags ]
                  )
              )
          );
      };

    };

  };

}
