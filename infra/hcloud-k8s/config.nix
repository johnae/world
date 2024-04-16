{
  lib,
  pkgs,
  ...
}: let
  diskEncryptionKeyScript = pkgs.writeShellApplication {
    name = "disk-encryption-key.sh";
    runtimeInputs = [pkgs.rbw];
    text = ''
      CLOUD_DISK_PASSWORD=''${CLOUD_DISK_PASSWORD:-}
      if [ -n "$CLOUD_DISK_PASSWORD" ]; then
        echo -n "$CLOUD_DISK_PASSWORD"
        exit 0
      fi

      rbw get hetzner -- cloud_disk_password
    '';
  };

  extraFilesScript = pkgs.writeShellApplication {
    name = "extra-files.sh";
    runtimeInputs = [pkgs.rbw];
    text = ''
      mkdir -p etc/ssh
      CLOUD_SSH_HOSTKEY=''${CLOUD_SSH_HOSTKEY:-}
      CLOUD_SSH_INITRD_KEY=''${CLOUD_SSH_INITRD_KEY:-}

      if [ -n "$CLOUD_SSH_HOSTKEY" ]; then
        echo "$CLOUD_SSH_HOSTKEY" | base64 -d > etc/ssh/ssh_host_ed25519_key
      else
        rbw get hetzner -- cloud_ssh_host_key | base64 -d > etc/ssh/ssh_host_ed25519_key
      fi

      if [ -n "$CLOUD_SSH_INITRD_KEY" ]; then
        echo "$CLOUD_SSH_INITRD_KEY" | base64 -d > etc/ssh/initrd_ed25519_key
      else
        rbw get hetzner -- cloud_ssh_initrd_key | base64 -d > etc/ssh/initrd_ed25519_key
      fi

      chmod 0600 etc/ssh/ssh_host_ed25519_key
      chmod 0600 etc/ssh/initrd_ed25519_key
    '';
  };

  master_server_type = "cpx11";
  agent_server_type = "cpx21";
  location = "hel1";
  dc = "hel1-dc2";
  image = "ubuntu-22.04";
  masters = 3;
  agents = 2;
  labels = {
    cluster_id = "\${random_string.cluster.id}";
    tailscale = "yes";
  };

  createRandomMachineId = {
    prefix,
    id,
  }: {
    "${prefix}-${toString id}" = {
      length = 4;
      upper = false;
      special = false;
      min_lower = 2;
      min_numeric = 2;
    };
  };
in {
  terraform.required_providers.hcloud.source = "hetznercloud/hcloud";
  terraform.required_providers.null.source = "hashicorp/null";
  terraform.required_providers.random.source = "hashicorp/random";
  terraform.required_providers.cloudflare.source = "cloudflare/cloudflare";

  resource.hcloud_placement_group.default = {
    name = "default";
    type = "spread";
    inherit labels;
  };

  resource.random_string = lib.mkMerge [
    {
      cluster = {
        length = 6;
        upper = false;
        special = false;
        min_lower = 2;
        min_numeric = 2;
      };
    }
    (lib.zipAttrs (map createRandomMachineId (builtins.genList (id: {
        prefix = "master";
        inherit id;
      })
      masters)))
    (lib.zipAttrs (map createRandomMachineId (builtins.genList (id: {
        prefix = "agent";
        inherit id;
      })
      agents)))
  ];

  resource.hcloud_ssh_key.default = {
    name = "default";
    public_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp";
  };

  resource.hcloud_server = let
    placement_group_id = "\${hcloud_placement_group.default.id}";
    ssh_keys = ["\${hcloud_ssh_key.default.id}"];
  in
    lib.mkMerge [
      (lib.genAttrs (builtins.genList (id: "master-${toString id}") masters) (id: {
        name = "master-\${random_string.cluster.id}-\${random_string.${id}.id}";
        server_type = master_server_type;
        inherit location labels placement_group_id ssh_keys image;
        user_data = ''
          mkdir -p /run/nixos
          cat<<META>/run/nixos/metadata
          CLUSTER_ID=''${random_string.cluster.id}
          NODE_ID=''${random_string.${id}.id}
          NODENAME=master-''${random_string.cluster.id}-''${random_string.${id}.id}
          REGION=${location}
          ZONE=${dc}
          INITIAL_MASTER=master-''${random_string.cluster.id}-''${random_string.master-0.id}
        '';
      }))

      (lib.genAttrs (builtins.genList (id: "agent-${toString id}") agents) (id: {
        name = "agent-\${random_string.cluster.id}-\${random_string.${id}.id}";
        server_type = agent_server_type;
        inherit location labels placement_group_id ssh_keys image;
        user_data = ''
          mkdir -p /run/nixos
          cat<<META>/run/nixos/metadata
          CLUSTER_ID=''${random_string.cluster.id}
          NODE_ID=''${random_string.${id}.id}
          NODENAME=agent-''${random_string.cluster.id}-''${random_string.${id}.id}
          REGION=${location}
          ZONE=${dc}
          INITIAL_MASTER=master-''${random_string.cluster.id}-''${random_string.master-0.id}
        '';
      }))
    ];

  module = lib.mkMerge ([
      {
        master-init-system-build = {
          source = "github.com/nix-community/nixos-anywhere//terraform/nix-build";
          attribute = ".#nixosConfigurations.hcloud-k3s-master-init.config.system.build.toplevel";
        };
      }

      {
        master-system-build = {
          source = "github.com/nix-community/nixos-anywhere//terraform/nix-build";
          attribute = ".#nixosConfigurations.hcloud-k3s-master.config.system.build.toplevel";
        };
      }

      {
        agent-system-build = {
          source = "github.com/nix-community/nixos-anywhere//terraform/nix-build";
          attribute = ".#nixosConfigurations.hcloud-k3s-agent.config.system.build.toplevel";
        };
      }

      {
        master-init-disko = {
          source = "github.com/nix-community/nixos-anywhere//terraform/nix-build";
          attribute = ".#nixosConfigurations.hcloud-k3s-master-init.config.system.build.diskoScript";
        };
      }

      {
        master-disko = {
          source = "github.com/nix-community/nixos-anywhere//terraform/nix-build";
          attribute = ".#nixosConfigurations.hcloud-k3s-master.config.system.build.diskoScript";
        };
      }

      {
        agent-disko = {
          source = "github.com/nix-community/nixos-anywhere//terraform/nix-build";
          attribute = ".#nixosConfigurations.hcloud-k3s-agent.config.system.build.diskoScript";
        };
      }

      {
        master-init-install = {
          source = "github.com/nix-community/nixos-anywhere//terraform/install";
          nixos_system = "\${module.master-init-system-build.result.out}";
          nixos_partitioner = "\${module.master-init-disko.result.out}";
          target_host = "\${hcloud_server.master-0.ipv4_address}";
          disk_encryption_key_scripts = [
            {
              path = "/tmp/disk.key";
              script = "${diskEncryptionKeyScript}/bin/disk-encryption-key.sh";
            }
          ];
          extra_files_script = "${extraFilesScript}/bin/extra-files.sh";
        };
      }
    ]
    ++ (
      map (id: {
        "master-install-${id}" = {
          source = "github.com/nix-community/nixos-anywhere//terraform/install";
          nixos_system = "\${module.master-system-build.result.out}";
          nixos_partitioner = "\${module.master-disko.result.out}";
          target_host = "\${hcloud_server.master-${id}.ipv4_address}";
          disk_encryption_key_scripts = [
            {
              path = "/tmp/disk.key";
              script = "${diskEncryptionKeyScript}/bin/disk-encryption-key.sh";
            }
          ];
          extra_files_script = "${extraFilesScript}/bin/extra-files.sh";
        };
      }) (
        builtins.genList (id: toString (id + 1)) (masters - 1)
      )
    )
    ++ (
      map (id: {
        "agent-install-${id}" = {
          source = "github.com/nix-community/nixos-anywhere//terraform/install";
          nixos_system = "\${module.agent-system-build.result.out}";
          nixos_partitioner = "\${module.master-disko.result.out}";
          target_host = "\${hcloud_server.agent-${id}.ipv4_address}";
          disk_encryption_key_scripts = [
            {
              path = "/tmp/disk.key";
              script = "${diskEncryptionKeyScript}/bin/disk-encryption-key.sh";
            }
          ];
          extra_files_script = "${extraFilesScript}/bin/extra-files.sh";
        };
      }) (
        builtins.genList (id: toString id) agents
      )
    ));

  output.k8s_api = {
    value = "k8s-api-\${random_string.cluster.id}";
  };
}
