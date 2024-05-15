{
  hostName,
  pkgs,
  lib,
  config,
  hostConfigurations,
  ...
}: let
  inherit (builtins) head tail filter split attrNames;
  hnComponents = filter (i: i != []) (split "-" hostName);
  clusterId = head (tail hnComponents);
  nodeId = head (tail (tail hnComponents));
  clusterNodes = lib.attrsets.attrVals (filter (lib.strings.hasInfix clusterId) (attrNames hostConfigurations)) hostConfigurations;
  initialMasterNode = head (filter (lib.attrByPath ["services" "k3s" "settings" "cluster-init"] false) clusterNodes);
  initialMaster = builtins.trace initialMasterNode.networking.hostName initialMasterNode.networking.hostName;
  mac = (head config.microvm.interfaces).mac;
in {
  imports = [
    ../profiles/microvm.nix
    ../profiles/tailscale.nix
    ../profiles/zram.nix
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
    after = ["tailscale-auth.service" "metadata.service"];
    settings = {
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
      file = ../secrets/ts-google-9k.age;
    };
    k3s-token = {
      file = ../secrets/k3s/token.age;
    };
  };

  age.identityPaths = ["/keep/etc/ssh/ssh_host_ed25519_key"];

  services.openssh.hostKeys = [
    {
      path = "/keep/etc/ssh/ssh_host_ed25519_key";
      type = "ed25519";
    }
  ];

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
  fileSystems."/etc/rancher".neededForBoot = true;
  fileSystems."/var/lib/rancher".neededForBoot = true;
  microvm.volumes = [
    {
      mountPoint = "/var/lib/rancher";
      image = "rancher.img";
      size = 2048;
    }
    {
      mountPoint = "/etc/rancher";
      image = "rancher-etc.img";
      size = 16;
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
