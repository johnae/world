{
  pkgs,
  config,
  tailnet,
  ...
}: let
  inherit (config.services.k3s) settings;
  cluster-cidr = "10.128.128.0/21";
  service-cidr = "10.129.128.0/22";
  cluster-dns = "10.129.128.10";
in {
  networking.firewall = {
    trustedInterfaces = [
      "lo" # localhost
      "cilium_host" # Cilium host gateway
      "cilium_net" # Cilium network interface
      "cilium_vxlan" # Cilium VXLAN interface
      "lxc+" # All Cilium pod interfaces (pattern match)
    ];
    # Critical: Allow packet forwarding for Kubernetes networking
    extraCommands = ''
      # Allow forwarding for pod and service networks
      iptables -A FORWARD -s 10.128.128.0/21 -j ACCEPT  # Default k3s pod CIDR
      iptables -A FORWARD -d 10.128.128.0/21 -j ACCEPT
      iptables -A FORWARD -s 10.129.128.0/22 -j ACCEPT  # Default k3s service CIDR
      iptables -A FORWARD -d 10.129.128.0/22 -j ACCEPT

      # Allow pods to reach API server through service IP
      iptables -A INPUT -s 10.128.128.0/21 -d 10.129.128.0/22 -j ACCEPT
      iptables -A OUTPUT -s 10.128.128.0/21 -d 10.129.128.0/22 -j ACCEPT

      # Allow established connections
      iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT

      # Allow ICMP for health checks
      iptables -A INPUT -p icmp --icmp-type echo-request -s 10.128.128.0/21 -j ACCEPT
      iptables -A INPUT -p icmp --icmp-type echo-reply -s 10.128.128.0/21 -j ACCEPT

      # If using VRRP mode for kube-vip (optional)
      # iptables -A nixos-fw -p 112 -d 224.0.0.18 -j nixos-fw-accept
    '';

    # Clean up custom rules when firewall stops
    extraStopCommands = ''
      iptables -D FORWARD -s 10.128.128.0/21 -j ACCEPT 2>/dev/null || true
      iptables -D FORWARD -d 10.128.128.0/21 -j ACCEPT 2>/dev/null || true
      iptables -D FORWARD -s 10.129.128.0/22 -j ACCEPT 2>/dev/null || true
      iptables -D FORWARD -d 10.129.128.0/22 -j ACCEPT 2>/dev/null || true
    '';
  };
  services.k3s = {
    enable = true;
    role = "server";
    after = ["tailscale-auth.service" "metadata.service"];
    settings = {
      token-file = "/run/agenix/k3s-token";
      flannel-backend = "none";
      disable = ["servicelb" "traefik" "local-storage"];
      disable-cloud-controller = true;
      disable-network-policy = true;
      disable-kube-proxy = true;
      # flannel-iface = "tailscale0"; ## no flannel?
      node-name = "\"$NODENAME\"";
      node-external-ip = "\"$(get-default-route-ip)\"";
      node-ip = "\"$(get-iface-ip tailscale0)\"";
      advertise-address = "\"$(get-iface-ip tailscale0)\"";
      inherit cluster-cidr service-cidr cluster-dns;
      kubelet-arg.max-pods = 62;
      kube-controller-manager-arg.node-cidr-mask-size = 25;
      # node-label."svccontroller.k3s.cattle.io/enablelb" = "true";
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
          --set k8sServiceHost=${settings.advertise-address} \
          --set k8sServicePort=6443 \
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
      kube-vip = {
        apiVersion = "apps/v1";
        kind = "DaemonSet";
        metadata = {
          creationTimestamp = null;
          labels = {
            "app.kubernetes.io/name" = "kube-vip-ds";
            "app.kubernetes.io/version" = "v0.9.2";
          };
          name = "kube-vip-ds";
          namespace = "kube-system";
        };
        spec = {
          selector.matchLabels."app.kubernetes.io/name" = "kube-vip-ds";
          template = {
            metadata = {
              creationTimestamp = null;
              labels = {
                "app.kubernetes.io/name" = "kube-vip-ds";
                "app.kubernetes.io/version" = "v0.9.2";
              };
            };
            spec = {
              containers = [
                {
                  args = [
                    "manager"
                  ];
                  env = [
                    {
                      name = "KUBERNETES_SERVICE_HOST";
                      value = "127.0.0.1";
                    }
                    {
                      name = "KUBERNETES_SERVICE_PORT";
                      value = "6443";
                    }
                    {
                      name = "vip_arp";
                      value = "true";
                    }
                    {
                      name = "port";
                      value = "6443";
                    }
                    {
                      name = "vip_nodename";
                      valueFrom.fieldRef.fieldPath = "spec.nodeName";
                    }
                    {
                      name = "vip_interface";
                      value = "enp195s0.4000";
                    }
                    {
                      name = "vip_subnet";
                      value = "32";
                    }
                    {
                      name = "dns_mode";
                      value = "first";
                    }
                    {
                      name = "cp_enable";
                      value = "true";
                    }
                    {
                      name = "cp_namespace";
                      value = "kube-system";
                    }
                    {
                      name = "address";
                      value = "192.168.123.100";
                    }
                    {
                      name = "prometheus_server";
                      value = ":2112";
                    }
                  ];
                  image = "ghcr.io/kube-vip/kube-vip:v0.9.2";
                  imagePullPolicy = "IfNotPresent";
                  name = "kube-vip";
                  resources = {};
                  securityContext.capabilities = {
                    add = [
                      "NET_ADMIN"
                      "NET_RAW"
                    ];
                    drop = [
                      "ALL"
                    ];
                  };
                }
              ];
              hostNetwork = true;
              serviceAccountName = "kube-vip";
            };
            updateStrategy = {};
          };
        };
      };
      kube-vip-service-account = {
        apiVersion = "v1";
        kind = "ServiceAccount";
        metadata = {
          name = "kube-vip";
          namespace = "kube-system";
        };
      };
      kube-vip-sa-cluster-role = {
        apiVersion = "rbac.authorization.k8s.io/v1";
        kind = "ClusterRole";
        metadata = {
          annotations."rbac.authorization.kubernetes.io/autoupdate" = "true";
          name = "system:kube-vip-role";
        };
        rules = [
          {
            apiGroups = [""];
            resources = ["services/status"];
            verbs = ["update"];
          }
          {
            apiGroups = [""];
            resources = ["services" "endpoints"];
            verbs = ["list" "get" "watch" "update"];
          }
          {
            apiGroups = [""];
            resources = ["nodes"];
            verbs = ["list" "get" "watch" "update" "patch"];
          }
          {
            apiGroups = ["coordination.k8s.io"];
            resources = ["leases"];
            verbs = ["list" "get" "watch" "update" "create"];
          }
          {
            apiGroups = ["discovery.k8s.io"];
            resources = ["endpointslices"];
            verbs = ["list" "get" "watch" "update"];
          }
          {
            apiGroups = [""];
            resources = ["pods"];
            verbs = ["list"];
          }
        ];
      };
      kube-vip-cluster-role-binding = {
        kind = "ClusterRoleBinding";
        apiVersion = "rbac.authorization.k8s.io/v1";
        metadata.name = "system:kube-vip-binding";
        roleRef = {
          apiGroup = "rbac.authorization.k8s.io";
          kind = "ClusterRole";
          name = "system:kube-vip-role";
        };
        subjects = [
          {
            kind = "ServiceAccount";
            name = "kube-vip";
            namespace = "kube-system";
          }
        ];
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
