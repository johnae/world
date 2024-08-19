{
  lib,
  adminUser,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDErC2NyMr7hmuNA9gnuLveTxPjYVqkmpLL9j6kzf2a5";

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/hcloud.nix
    ../../profiles/hcloud-remote-unlock.nix
    ../../profiles/home-manager.nix
    ../../profiles/restic-backup.nix
    ../../profiles/state.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  programs.ssh.startAgent = true;

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
  };

  age.secrets = {
    copilot-token = {
      file = ../../secrets/gh_copilot.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.config/github-copilot/hosts.json";
    };
    id_ed25519_bbph = {
      file = ../../secrets/id_ed25519_bbph.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_bbph";
    };
    id_ed25519_alt = {
      file = ../../secrets/id_ed25519_alt.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_ed25519_alt";
    };
    id_rsa_alt = {
      file = ../../secrets/id_rsa_alt.age;
      owner = "${toString adminUser.uid}";
      path = "/home/${adminUser.name}/.ssh/id_rsa_alt";
    };
  };

  networking.firewall.trustedInterfaces = ["tailscale0"];

  services.tailscale.auth = {
    enable = true;
    after = ["metadata.service"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/etc/ts-auth-key";
    args.hostname = "\"$NODENAME\"";
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
  ];

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
      programs.git.extraConfig.user.signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp";
    };
  };

  disko.devices = {
    disk.disk1 = {
      device = lib.mkDefault "/dev/sda";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            name = "boot";
            size = "1M";
            type = "EF02";
          };
          esp = {
            name = "ESP";
            size = "500M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          luks = {
            size = "100%";
            content = {
              type = "luks";
              name = "encrypted";
              settings.allowDiscards = true;
              passwordFile = "/tmp/disk.key";
              content = {
                type = "filesystem";
                format = "bcachefs";
                mountpoint = "/";
                mountOptions = ["defaults" "compression=zstd" "background_compression=zstd"];
              };
            };
          };
        };
      };
    };

    # disk.disk2 = {
    #   device = lib.mkDefault "/dev/sdb";
    #   type = "disk";
    #   content = {
    #     type = "gpt";
    #     partitions = {
    #       luks = {
    #         size = "100%";
    #         content = {
    #           type = "luks";
    #           name = "encryptedB";
    #           settings.allowDiscards = true;
    #           passwordFile = "/tmp/disk.key";
    #           content = {
    #             type = "filesystem";
    #             format = "bcachefs";
    #             mountpoint = "/keep";
    #             mountOptions = ["defaults" "compression=zstd" "background_compression=zstd"];
    #           };
    #         };
    #       };
    #     };
    #   };
    # };
  };
  # fileSystems."/keep" = {
  #   neededForBoot = true;
  # };
  #### after first install - remove above disk2, enable below
  fileSystems."/keep" = {
    device = "/dev/mapper/encryptedB";
    fsType = "bcachefs";
    options = ["defaults" "compression=zstd" "background_compression=zstd"];
    neededForBoot = true;
  };
  boot.initrd.luks.devices = {
    encryptedB = {
      device = "/dev/disk/by-partlabel/disk-disk2-luks";
      allowDiscards = true;
    };
  };
}
