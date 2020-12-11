{ userName, hostName, importSecret, pkgs, config, lib, inputs, ... }:
let
  internalIP = "192.168.243.1";
  externalIP = "95.217.201.156";
  lbIP = "95.217.92.223";
  defaultGateway = "95.217.201.129";
  authorizedKeys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ== cardno:000607539231"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBTU5dDn7gokexQFPxH2tHWGh7MAEZ1aQ+cJdFgHFGxk john@insane.se"
  ];
in
{

  imports = [
    ../profiles/server.nix
    ../modules/state.nix
  ];

  nix.trustedUsers = [ "root" userName ];

  networking = {
    inherit hostName defaultGateway;
    hostId = "fa23b2cd";
    defaultGateway6 = {
      address = "fe80::1";
      interface = "eth0";
    };
    vlans.vlan4000 = {
      id = 4000;
      interface = "eth0";
    };
    interfaces = {
      vlan4000.ipv4.addresses = [
        {
          address = internalIP;
          prefixLength = 24;
        }
      ];
      eth0 = {
        ipv4.addresses = [
          {
            address = externalIP;
            prefixLength = 26;
          }
          {
            address = lbIP;
            prefixLength = 32;
          }
        ];
        ipv6.addresses = [
          {
            address = "2a01:4f9:4a:44a3::2";
            prefixLength = 64;
          }
        ];
      };
    };
    wireguard.interfaces.vpn = {
      ips = [
        "10.66.198.97/32"
        "fc00:bbbb:bbbb:bb01::3:c660/128"
      ];
      peers = [
        {
          allowedIPs = [
            "0.0.0.0/0"
            "::0/0"
          ];
          endpoint = "185.204.1.211:3017";
          persistentKeepalive = 25;
          publicKey = "R5LUBgM/1UjeAR4lt+L/yA30Gee6/VqVZ9eAB3ZTajs=";
        }
      ];
      preSetup = [
        "${pkgs.iproute}/bin/ip netns add private || true"
        "${pkgs.iproute}/bin/ip -n private link set lo up || true"
      ];
      interfaceNamespace = "private";
      privateKeyFile = config.sops.secrets.wireguardPrivateKey.path;
    };
    extraHosts = "127.0.1.1 ${hostName}";
  };

  services.k3s = {
    masterUrl = "https://100.85.6.50:6443";
    labels = [
      "cpu=high"
      "mem=high"
      "cloud=hetzner"
    ];
  };

  environment.systemPackages = [ pkgs.netns-exec ];

  environment.state."/keep" = {
    directories = [
      "/var/log"
      "/var/lib/wireguard"
      "/var/lib/systemd/coredump"
      "/var/lib/docker"
      "/var/lib/dockershim"
      "/var/lib/plex"
      "/var/lib/k3s"
      "/var/lib/kubelet"
      "/var/lib/cni"
      "/var/lib/transmission"
      "/var/lib/tailscale"
      "/root"
      "/etc/rancher"
    ];
    files = [
      "/etc/machine-id"
      "/etc/k3s-node-name"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
      "/etc/ssh/ssh_host_ed25519_key.pub"
    ];
  };

  services.transmission = {
    enable = true;
    downloadDirPermissions = "775";
    settings = {
      blocklist-enabled = true;
      blocklist-url = "https://github.com/sahsu/transmission-blocklist/releases/latest/download/blocklist.gz";
      encryption = 1;
      ratio-limit = 0.1;
    };
    port = 9091;
  };

  services.plex = {
    enable = true;
    openFirewall = true;
    user = "transmission";
    group = "transmission";
  };

  systemd.services.transmission
  .serviceConfig.NetworkNamespacePath = "/var/run/netns/private";

  systemd.services.transmission-forwarder = {
    enable = true;
    after = [ "transmission.service" ];
    bindsTo = [ "transmission.service" ];
    script = ''
      ${pkgs.socat}/bin/socat tcp-listen:9091,fork,reuseaddr,bind=127.0.0.1 exec:'/run/wrappers/bin/netns-exec private ${pkgs.socat}/bin/socat STDIO "tcp-connect:127.0.0.1:9091"',nofork
    '';
  };

  boot =
    let
      subnet = "255.255.255.192"; ## /26
    in
    {

      loader.systemd-boot.enable = lib.mkForce false;
      loader.efi.canTouchEfiVariables = lib.mkForce false;

      loader.grub.enable = true;
      loader.grub.devices = [ "/dev/nvme0n1" "/dev/nvme1n1" ];
      loader.grub.enableCryptodisk = true;
      kernelParams = [
        "ip=${externalIP}::${defaultGateway}:${subnet}:${hostName}:eth0:none"
      ];

      kernelModules = [ "kvm-amd" ];

      initrd.extraUtilsCommandsTest = lib.mkForce "";
      initrd.availableKernelModules = [
        "r8169"
        "nvme"
        "ahci"
        "usbhid"
      ];
      initrd.network = {
        enable = true;
        ssh = {
          enable = true;
          port = 2222;
          hostKeys = [
            "/etc/nixos/initrd_keys/dsa_key"
            "/etc/nixos/initrd_keys/rsa_key"
            "/etc/nixos/initrd_keys/ed25519_key"
            #config.sops.secrets.initrd-dsa-key.path
            #config.sops.secrets.initrd-rsa-key.path
            #config.sops.secrets.initrd-ed25519-key.path
          ];
        };
        postCommands = ''
          echo 'cryptsetup-askpass' >> /root/.profile
        '';
      };

      initrd.luks.devices = {
        cryptkey = {
          device = "/dev/disk/by-label/cryptkey";
        };

        encrypted_root = {
          device = "/dev/disk/by-label/encrypted_root";
          keyFile = "/dev/mapper/cryptkey";
        };

        encrypted_root2 = {
          device = "/dev/disk/by-label/encrypted_root2";
          keyFile = "/dev/mapper/cryptkey";
        };

        encrypted_swap = {
          device = "/dev/disk/by-label/encrypted_swap";
          keyFile = "/dev/mapper/cryptkey";
        };

      };

    };

  hardware.cpu.intel.updateMicrocode = lib.mkForce false;
  hardware.cpu.amd.updateMicrocode = true;

  users.defaultUserShell = pkgs.fish;
  users.mutableUsers = false;
  users.groups."${userName}".gid = 1337;
  users.users = {
    root = {
      passwordFile = config.sops.secrets.rootPassword.path;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ== cardno:000607539231"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBTU5dDn7gokexQFPxH2tHWGh7MAEZ1aQ+cJdFgHFGxk john@insane.se"
      ];
    };
    ${userName} = {
      isNormalUser = true;
      uid = 1337;
      shell = pkgs.fish;
      passwordFile = config.sops.secrets.userPassword.path;
      extraGroups = [
        "wheel"
        "docker"
        "video"
        "audio"
      ];
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ== cardno:000607539231"
      ];
    };
  };

  fileSystems."/" = {
    device = "none";
    fsType = "tmpfs";
    options = [ "defaults" "size=16G" "mode=755" ];
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@nix" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/keep" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@keep" "rw" "noatime" "compress=zstd" "ssd" "space_cache" ];
  };

  fileSystems."/mnt/disks/cow/local-disk-1" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-1" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-1" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-1"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-2" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-2" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-2" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-2"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-3" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-3" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-3" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-3"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-4" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-4" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-4" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-4"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-5" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-5" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-5" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-5"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-6" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-6" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-6" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-6"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-7" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-7" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-7" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-7"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-8" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-8" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-8" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-8"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-9" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-9" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-9" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-9"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-10" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-10" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-10" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-10"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-11" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-11" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-11" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-11"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-12" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-12" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-12" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-12"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-13" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-13" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-13" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-13"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-14" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-14" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-14" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-14"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-15" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-15" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-15" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-15"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-16" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-16" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-16" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-16"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-17" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-17" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-17" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-17"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-18" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-18" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-18" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-18"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-19" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-19" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-19" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-19"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/mnt/disks/cow/local-disk-20" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options =
      [ "subvol=@local-disk-20" "rw" "noatime" "compress=zstd" "space_cache" ];
  };

  fileSystems."/mnt/disks/nocow/local-disk-20" = {
    device = "/dev/disk/by-label/root";
    fsType = "btrfs";
    options = [
      "subvol=@local-disk-nocow-20"
      "rw"
      "noatime"
      "compress=zstd"
      "space_cache"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-label/swap"; }];

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "ondemand";
}
