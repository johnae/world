{
  adminUser,
  config,
  lib,
  pkgs,
  hostName,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ7m3AH34sXUa8UQIeDEyL2lpUwSbsrGjzPrjYauntOH";

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/hardware/ax101.nix
    ../../profiles/home-manager.nix
    ../../profiles/disk/disko-btrfs.nix
    ../../profiles/k3s-master.nix
    ../../profiles/h-remote-unlock.nix
    ../../profiles/server.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.grub = {
    # no need to set devices, disko will add all devices that have a EF02 partition to the list already
    # devices = [ ];
    enable = true;
    efiSupport = false;
    efiInstallAsRemovable = false;
  };

  boot.kernelParams = lib.mkForce [
    "ip=95.217.109.215::95.217.109.193:255.255.255.192:${hostName}::none"
  ];

  disko.devices.disk.disk1.device = "/dev/nvme0n1";

  systemd.services.metadata = let
    CLUSTER_ID = "5df5";
    NODE_ID = "c8c1";
    INITIAL_MASTER = "eridani";
    initScript = pkgs.writeShellScript "metadata-init" ''
      mkdir -p /run/nixos
      cat<<META>/run/nixos/metadata
      CLUSTER_ID=${CLUSTER_ID}
      NODE_ID=${NODE_ID}
      REGION=hetzner
      ZONE=fi
      INITIAL_MASTER=${INITIAL_MASTER}
      NODENAME=${hostName}
      META
    '';
  in {
    description = "Metadata Service";
    after = ["network.target"];
    before = ["k3s.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = initScript;
    };
  };

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = true; ## we're running kured on k8s for synchronized reboots
  };

  networking.firewall.trustedInterfaces = ["cilium_net@cilium_host" "cilium_host@cilium_net" "cilium_vxlan"];

  # services.k3s.settings.server = "https://\"$INITIAL_MASTER\":6443";
  services.k3s.settings = {
    cluster-init = true;
    node-ip = lib.mkForce "192.168.123.100";
    advertise-address = lib.mkForce "192.168.123.100";
  };

  age.secrets = {
    k3s-token.file = ../../secrets/k3s/token.age;
    cluster-secrets = {
      file = ../../secrets/k3s/cluster-secrets.yaml.age;
      path = "/var/lib/rancher/k3s/server/manifests/cluster-secrets.yaml";
    };
    tailscale-oauth-secret = {
      file = ../../secrets/k3s/tailscale-oauth-secret.yaml.age;
      path = "/var/lib/rancher/k3s/server/manifests/tailscale-oauth-secret.yaml";
    };
    id-ed25519-remote-unlock-key = {
      file = ../../secrets/h_id_ed25519_remote_unlock.age;
    };
    remote-disk-password = {
      file = ../../secrets/h_remote_unlock_password.age;
    };
  };

  services.networkd-dispatcher = let
    tailscale-forwarding = pkgs.writeShellApplication {
      name = "tailscale-forwarding";
      runtimeInputs = with pkgs; [iproute2 ethtool coreutils];
      text = ''
        for dev in $(ip route show 0/0 | cut -f5 -d' '); do echo ethtool -K "$dev" rx-udp-gro-forwarding on rx-gro-list off; done
      '';
    };
  in {
    enable = true;
    rules = {
      "50-tailscale" = {
        onState = ["routable"];
        script = "${tailscale-forwarding}/bin/tailscale-forwarding";
      };
    };
  };

  systemd.network = let
    iface = "enp195s0";
    vlanIface = "${iface}.4000";
  in {
    enable = true;
    netdevs."11-vlan" = {
      netdevConfig = {
        Kind = "vlan";
        Name = "${vlanIface}";
        MTUBytes = 1400;
      };
      vlanConfig.Id = 4000;
    };
    networks = {
      "10-wan" = {
        ## udevadm test-builtin net_id /sys/class/net/eth0
        ## https://www.freedesktop.org/software/systemd/man/latest/systemd.net-naming-scheme.html
        matchConfig.Name = [iface];
        address = [
          "95.217.109.215/26"
          "2a01:4f9:4a:2b5c::2/64"
        ];
        vlan = [vlanIface];
        routes = [
          {Gateway = "95.217.109.193";}
          {Gateway = "fe80::1";}
        ];
        linkConfig.RequiredForOnline = "routable";
      };
      "11-lan" = {
        matchConfig.Name = ["${vlanIface}"];
        address = [
          "192.168.123.100/24"
        ];
        linkConfig.RequiredForOnline = "routable";
      };
    };
  };

  services.tailscale.auth = {
    enable = true;
    after = ["metadata.service"];
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/etc/tailscale-auth-key";
    # args.hostname = "\"$NODENAME\"";
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
  ];

  services.remote-unlock = [
    # {
    #   enable = true;
    #   host = "95.217.109.215";
    #   port = 2222;
    #   identityFile = config.age.secrets.id-ed25519-remote-unlock-key.path;
    #   passwordFile = config.age.secrets.remote-disk-password.path;
    # }
    {
      enable = true;
      host = "95.217.56.105";
      port = 2222;
      identityFile = config.age.secrets.id-ed25519-remote-unlock-key.path;
      passwordFile = config.age.secrets.remote-disk-password.path;
    }
    {
      enable = true;
      host = "95.217.109.212";
      port = 2222;
      identityFile = config.age.secrets.id-ed25519-remote-unlock-key.path;
      passwordFile = config.age.secrets.remote-disk-password.path;
    }
  ];

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
    };
  };
}
