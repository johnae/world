{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.myk3s;
  k3s = config.services.k3s;
  isAgent = cfg.masterUrl != null;
  isMaster = !isAgent;
  k3sDir = "/var/lib/k3s";
  k3sDataDir = "${k3sDir}/data";
  k3sNodeNameGen = ''
    if [ ! -e /etc/k3s-node-name ]; then
      echo "${cfg.nodeName}-$(${pkgs.openssl}/bin/openssl rand -hex 4)" > /etc/k3s-node-name
    fi
  '';
in
{
  options.services.myk3s = {

    enable = mkEnableOption "enable k3s - lightweight kubernetes.";

    nodeName = mkOption {
      type = types.str;
      example = "somenode";
      description = ''
        The node name for the current node. A random string is appended to this.
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

    clusterSecret = mkOption {
      type = types.str;
      example = "some-random-string-99uqu9jq9c";
      description = ''
        The shared cluster secret enabling hosts to automatically connect to each other.
      '';
    };

    docker = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to use docker instead of containerd.
      '';
    };

    flannelBackend = mkOption {
      type = with types; nullOr (enum [ "none" "vxlan" "ipsec" "wireguard" ]);
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
        The cni package to user. If set to null, the defaults are used.
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
        The url to the master node agents should connect to. By not specifying this
        the current node is assumed to be the master node.
      '';
    };

  };

  config = mkIf cfg.enable {
    services.k3s.enable = true;
    services.k3s.role = if isAgent then "agent" else "server";
    services.k3s.serverAddr = if cfg.masterUrl != null then cfg.masterUrl else "";
    services.k3s.token = cfg.clusterSecret;
    services.k3s.docker = cfg.docker;
    services.k3s.package = pkgs.k3s;
    services.k3s.extraFlags =
      let
        cniBinDir = if cfg.cniPackage != null then "${cfg.cniPackage}/bin" else "${k3sDir}/opt/cni/bin";
      in
      lib.concatStringsSep " \\\n "
        (
          if isAgent then
            [
              ''--kubelet-arg "volume-plugin-dir=${k3sDir}/libexec/kubernetes/kubelet-plugins/volume/exec"''
              ''--kubelet-arg "cni-bin-dir=${cniBinDir}"''
              ''--node-name "$(cat /etc/k3s-node-name)"''
              (lib.concatStringsSep " "
                (map (v: "--node-label ${v}") (cfg.labels ++ [ "hostname=${cfg.nodeName}" ]))
              )
            ]
          else
            [
              ''--no-deploy=traefik --no-deploy=servicelb --no-deploy=local-storage -d ${k3sDataDir}''
              ''-o /kubeconfig.yml''
              ''--flannel-backend=${cfg.flannelBackend}''
              ''--kubelet-arg "volume-plugin-dir=${k3sDir}/libexec/kubernetes/kubelet-plugins/volume/exec"''
              ''--kubelet-arg "cni-bin-dir=${cniBinDir}"''
              ''--kube-controller-arg "flex-volume-plugin-dir=${k3sDir}/libexec/kubernetes/kubelet-plugins/volume/exec"''
              ''--node-name "$(cat /etc/k3s-node-name)"''
              (lib.concatStringsSep " "
                (map (v: "--node-label ${v}") (cfg.labels ++ [ "hostname=${cfg.nodeName}" ]))
              )
            ]
        );


    systemd.services.k3s = {

      preStart = k3sNodeNameGen;

      postStart = (
        if isMaster
        then ''
          echo Applying extra kubernetes manifests...
          set -x
          ${lib.concatStringsSep "\n" (
            map (
                m:
                "${pkgs.kubectl}/bin/kubectl --kubeconfig /kubeconfig.yml apply -f ${m}"
                )
            cfg.extraManifests
            )}
        '' else
          ""
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
                      "export PATH=${pkgs.wireguard}/bin:${pkgs.bash}/bin\${PATH:+:}$PATH \n"
                      "exec ${k3s.package}/bin/k3s ${k3s.role}"
                    ] ++ (optional k3s.docker "--docker")
                    ++ (optional k3s.disableAgent "--disable-agent")
                    ++ (optional (k3s.role == "agent") "--server ${k3s.serverAddr} --token ${k3s.token}")
                    ++ [ k3s.extraFlags ]
                  )
              )
          );
      };

    };

  };

}
