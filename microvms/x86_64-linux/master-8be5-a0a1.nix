{
  hostName,
  pkgs,
  tailnet,
  config,
  ...
}: let
  inherit (builtins) head tail filter split;
  hnComponents = filter (i: i != []) (split "-" hostName);
  clusterId = head (tail hnComponents);
  nodeId = head (tail (tail hnComponents));
  initialMaster = "master-${clusterId}-a0a1";
  mac = (head config.microvm.interfaces).mac;
in {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRUfwPUxHqtVRsV3CdDRDEAYTg28ZdK5/Mz/GlcZdiv";

  imports = [
    ../../profiles/microvm.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  systemd.services.metadata = let
    cloudInitScript = pkgs.writeShellScript "cloud-init" ''
      mkdir -p /run/nixos
      touch /run/nixos/metadata
      cat<<META>/run/nixos/metadata
      CLUSTER_ID=${clusterId}
      NODENAME=${hostName}
      NODE_ID=${nodeId}
      REGION=fi
      ZONE=fi-a
      INITIAL_MASTER=${initialMaster}
      META
    '';
  in {
    description = "Metadata Service";
    after = ["network.target"];
    before = ["tailscale-auth.service" "tailscaled.service" "k3s.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = cloudInitScript;
    };
  };

  services.k3s = {
    enable = true;
    role = "server";
    after = ["tailscale-auth.service" "metadata.service"];
    settings = {
      cluster-init = true;
      token-file = "/run/agenix/k3s-token";
      flannel-iface = "tailscale0";
      node-name = hostName;
      node-ip = "\"$(get-iface-ip tailscale0)\"";
      node-external-ip = "\"$(get-iface-ip \"$(get-iface-with-mac ${mac})\")\"";
      advertise-address = "\"$(get-iface-ip tailscale0)\"";
      cluster-cidr = "10.128.128.0/21";
      service-cidr = "10.129.128.0/22";
      cluster-dns = "10.129.128.10";
      kubelet-arg.max-pods = 62;
      kube-controller-manager-arg.node-cidr-mask-size = 25;
      node-label."svccontroller.k3s.cattle.io/enablelb" = "true";
      node-label."topology.kubernetes.io/region" = "$\"REGION\"";
      node-label."topology.kubernetes.io/zone" = "\"$ZONE\"";
      node-label."hostname" = hostName;
      secrets-encryption = true;
      node-taint = "CriticalAddonsOnly=true:NoExecute";
      tls-san = [hostName "${hostName}.${tailnet}.ts.net"];
    };

    autoDeploy = {
      kured = "${pkgs.kured-yaml}/kured.yaml";
      flux = "${pkgs.fluxcd-yaml}/flux.yaml";
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
          cluster_id = "\${CLUSTER_ID}"; ## comes from /run/nixos/metadata
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

  age.secrets = {
    ts-google-9k = {
      file = ../../secrets/ts-google-9k.age;
    };
    k3s-token = {
      file = ../../secrets/k3s/token.age;
    };
  };

  age.identityPaths = ["/keep/etc/ssh/ssh_host_ed25519_key"];

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/var/run/agenix/ts-google-9k";
  };

  fileSystems."/keep".neededForBoot = true;
  microvm.volumes = [
    {
      mountPoint = "/var/lib/rancher";
      image = "rancher.img";
      size = 2048;
    }
  ];
  microvm.shares = [
    {
      proto = "virtiofs";
      tag = "keep";
      source = "/var/lib/microvms/${hostName}/keep";
      mountPoint = "/keep";
    }
  ];
}
