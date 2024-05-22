{
  pkgs,
  lib,
  tailnet,
  ...
}: {
  imports = [
    ../profiles/microvm-k3s-master.nix
  ];

  services.k3s = {
    enable = true;
    role = "server";
    settings = {
      server = lib.mkForce null;
      cluster-init = true;
    };

    autoDeploy = {
      kured = "${pkgs.kured-yaml}/kured.yaml";
      flux = "${pkgs.fluxcd-yaml}/flux.yaml";
      juicefs-csi-driver = "${pkgs.juicefs-csi-driver-yaml}/juicefs-csi-driver.yaml";
      cluster-vars = {
        apiVersion = "v1";
        kind = "ConfigMap";
        metadata = {
          name = "cluster-vars";
          namespace = "flux-system";
        };
        data = {
          tailnet = "${tailnet}";
          cluster_id = "\${CLUSTER_ID}"; ## comes from /run/nixos/metadata
        };
      };
      tailscale-helm-repo = {
        apiVersion = "source.toolkit.fluxcd.io/v1beta2";
        kind = "HelmRepository";
        metadata = {
          name = "tailscale";
          namespace = "flux-system";
        };
        spec = {
          interval = "5m";
          url = "https://pkgs.tailscale.com/helmcharts";
        };
      };
      tailscale-operator = {
        apiVersion = "helm.toolkit.fluxcd.io/v2beta2";
        kind = "HelmRelease";
        metadata = {
          name = "tailscale-operator";
          namespace = "flux-system";
        };
        spec = {
          chart.spec = {
            chart = "tailscale-operator";
            interval = "5m";
            sourceRef = {
              kind = "HelmRepository";
              name = "tailscale";
            };
          };
          install.createNamespace = true;
          releaseName = "tailscale-operator";
          targetNamespace = "tailscale";
          interval = "10m";
          timeout = "5m";
          values = {
            apiServerProxyConfig.mode = "true";
            operatorConfig.hostname = "k8s-api-\${CLUSTER_ID}";
          };
        };
      };
    };
  };
}
