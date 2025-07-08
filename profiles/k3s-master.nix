{
  pkgs,
  config,
  tailnet,
  ...
}: let
  inherit (config.services.k3s) settings;
in {
  services.k3s = {
    enable = true;
    role = "server";
    after = ["tailscale-auth.service" "metadata.service"];
    settings = {
      token-file = "/run/agenix/k3s-token";
      flannel-backend = "none";
      disable-network-policy = true;
      disable-kube-proxy = true;
      # flannel-iface = "tailscale0"; ## no flannel?
      node-name = "\"$NODENAME\"";
      node-external-ip = "\"$(get-default-route-ip)\"";
      node-ip = "\"$(get-iface-ip tailscale0)\"";
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
      # node-taint = "CriticalAddonsOnly=true:NoExecute";
      tls-san = ["\"$NODENAME\"" "\"$NODENAME\".${tailnet}.ts.net"];
    };
    autoDeploy = let
      cilium = pkgs.runCommand "helm-template" {allowSubstitution = false;} ''
        mkdir -p "$out"
        ${pkgs.kubernetes-helm}/bin/helm template cilium ${pkgs.inputs.cilium-chart} \
          --namespace kube-system \
          --set kubeProxyReplacement=true \
          --set k8sServiceHost=${settings.node-ip} \
          --set k8sServicePort=6443 \
          --set tunnelProtocol=vxlan \
          --set enableExternalIPs=true \
          --set enableHostPort=true \
          --set enableNodePort=true \
          --set ipam.operator.clusterPoolIPv4PodCIDRList=${settings.cluster-cidr} \
          --set encryption.enabled=true \
          --set encryption.type=wireguard \
          --set encryption.nodeEncryption=true > "$out"/cilium.yaml
      '';
    in {
      kured = "${pkgs.kured-yaml}/kured.yaml";
      flux = "${pkgs.fluxcd-yaml}/flux.yaml";
      cilium = "${cilium}/cilium.yaml";
      # hetzner-csi-driver = "${pkgs.hetzner-csi-driver-yaml}/hetzner-csi-driver.yaml";
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
      # encrypted-storage-class = {
      #   apiVersion = "storage.k8s.io/v1";
      #   kind = "StorageClass";
      #   metadata.name = "hcloud-volumes-encrypted";
      #   provisioner = "csi.hetzner.cloud";
      #   reclaimPolicy = "Delete";
      #   volumeBindingMode = "WaitForFirstConsumer";
      #   allowVolumeExpansion = true;
      #   parameters."csi.storage.k8s.io/node-publish-secret-name" = "encryption-secret";
      #   parameters."csi.storage.k8s.io/node-publish-secret-namespace" = "kube-system";
      # };
      # cilium-helm-repo = {
      #   apiVersion = "source.toolkit.fluxcd.io/v1beta2";
      #   kind = "HelmRepository";
      #   metadata = {
      #     name = "cilium";
      #     namespace = "flux-system";
      #   };
      #   spec = {
      #     interval = "5m";
      #     url = "https://helm.cilium.io/";
      #   };
      # };
      # cilium = {
      #   apiVersion = "helm.toolkit.fluxcd.io/v2beta2";
      #   kind = "HelmRelease";
      #   metadata = {
      #     name = "cilium";
      #     namespace = "flux-system";
      #   };
      #   spec = {
      #     chart.spec = {
      #       chart = "cilium";
      #       interval = "5m";
      #       sourceRef = {
      #         kind = "HelmRepository";
      #         name = "cilium";
      #       };
      #     };
      #     releaseName = "cilium";
      #     targetNamespace = "kube-system";
      #     interval = "10m";
      #     timeout = "5m";
      #     values = {
      #       autoDirectNodeRoutes = "false";
      #       tunnelProtocol = "vxlan";
      #       encryption.enabled = "true";
      #       encryption.nodeEncryption = "true";
      #       encryption.type = "wireguard";
      #       enableIPv4Masquerade = "true";
      #       ipam.mode = "cluster-pool";
      #       ipv4NativeRoutingCIDR = "10.128.128.0/21";
      #     };
      #   };
      # };
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
