{
  pkgs,
  tailnet,
  ...
}: {
  services.k3s = {
    enable = true;
    role = "server";
    after = ["tailscale-auth.service" "metadata.service"];
    settings = {
      token-file = "/run/agenix/k3s-token";
      flannel-iface = "tailscale0";
      node-name = "\"$NODENAME\"";
      node-ip = "\"$(get-iface-ip tailscale0)\"";
      node-external-ip = "\"$(get-iface-ip eth0)\"";
      advertise-address = "\"$(get-iface-ip tailscale0)\"";
      cluster-cidr = "10.128.128.0/21";
      service-cidr = "10.129.128.0/22";
      cluster-dns = "10.129.128.10";
      kubelet-arg.max-pods = 62;
      kube-controller-manager-arg.node-cidr-mask-size = 25;
      node-label."svccontroller.k3s.cattle.io/enablelb" = "true";
      node-label."topology.kubernetes.io/region" = "$\"REGION\"";
      node-label."topology.kubernetes.io/zone" = "\"$ZONE\"";
      node-label."hostname" = "\"$NODENAME\"";
      secrets-encryption = true;
      node-taint = "CriticalAddonsOnly=true:NoExecute";
      tls-san = ["\"$NODENAME\"" "\"$NODENAME\".${tailnet}.ts.net"];
    };
    autoDeploy = {
      kured = "${pkgs.kured-yaml}/kured.yaml";
      flux = "${pkgs.fluxcd-yaml}/flux.yaml";
      hetzner-csi-driver = "${pkgs.hetzner-csi-driver-yaml}/hetzner-csi-driver.yaml";
      cluster-vars = {
        apiVersion = "v1";
        kind = "ConfigMap";
        metadata = {
          name = "cluster-vars";
          namespace = "flux-system";
        };
        data = {
          tailnet = "${tailnet}";
          cluster_id = "\${CLUSTER_ID}"; ## comes from /run/nixos/metadata - see tofu/main.tf
        };
      };
      encrypted-storage-class = {
        apiVersion = "storage.k8s.io/v1";
        kind = "StorageClass";
        metadata.name = "hcloud-volumes-encrypted";
        provisioner = "csi.hetzner.cloud";
        reclaimPolicy = "Delete";
        volumeBindingMode = "WaitForFirstConsumer";
        allowVolumeExpansion = true;
        parameters."csi.storage.k8s.io/node-publish-secret-name" = "encryption-secret";
        parameters."csi.storage.k8s.io/node-publish-secret-namespace" = "kube-system";
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
          chart = {
            spec = {
              chart = "tailscale-operator";
              interval = "5m";
              releaseName = "tailscale-operator";
              sourceRef = {
                kind = "HelmRepository";
                name = "tailscale";
              };
            };
          };
          install.createNamespace = true;
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
