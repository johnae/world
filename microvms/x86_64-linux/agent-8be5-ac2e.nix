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
    role = "agent";
    after = ["tailscale-auth.service" "metadata.service"];
    settings = {
      server = "https://\"$INITIAL_MASTER\":6443";
      token-file = "/run/agenix/k3s-token";
      flannel-iface = "tailscale0";
      node-name = hostName;
      node-ip = "\"$(get-iface-ip tailscale0)\"";
      node-external-ip = "\"$(get-iface-ip \"$(get-iface-with-mac ${mac})\")\"";
      kubelet-arg.max-pods = 62;
      node-label."svccontroller.k3s.cattle.io/enablelb" = "true";
      node-label."topology.kubernetes.io/region" = "$\"REGION\"";
      node-label."topology.kubernetes.io/zone" = "\"$ZONE\"";
      node-label."hostname" = hostName;
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
