terraform {
  required_providers {
    cloudflare = {
      source = "cloudflare/cloudflare"
    }
    hcloud = {
      source = "hetznercloud/hcloud"
    }
    null = {
      source = "hashicorp/null"
    }
    random = {
      source = "hashicorp/random"
    }
    tailscale = {
      source = "tailscale/tailscale"
    }
  }
}

locals {
  master_server_type = "cpx11" # AMD 2 vCPU, 2 GB RAM, 40 GB NVMe SSD
  agent_server_type = "cpx21" # AMD 3 vCPU, 4 GB RAM, 80 GB NVMe SSD
  region = "hel1"
  zone = "hel1-dc2"
  masters = 3
  agents = 2
  labels = {
    "tfstate" : "k3s"
    "cluster_id" : random_string.cluster.id
    "tailscale" : "yes"
  }
}

provider "tailscale" {}

resource "hcloud_placement_group" "default" {
  name = "default"
  type = "spread"
  labels = local.labels
}

resource "random_string" "cluster" {
  length = 6
  upper = false
  special = false
  min_lower = 2
  min_numeric = 2
}

resource "tailscale_tailnet_key" "master_key" {
  for_each    = toset([for i in range(0, local.masters): tostring(i)])
  reusable      = false
  ephemeral     = true
  preauthorized = true
  expiry        = 3600
  description   = "K3S Node Master TS Key"
  tags = ["tag:server", "tag:hcloud", "tag:k8s"]
}

resource "random_string" "master_id" {
  for_each    = toset([for i in range(0, local.masters): tostring(i)])
  length = 4
  upper = false
  special = false
  min_lower = 2
  min_numeric = 2
}

resource "tailscale_tailnet_key" "agent_key" {
  for_each    = toset([for i in range(0, local.agents): tostring(i)])
  reusable      = false
  ephemeral     = true
  preauthorized = true
  expiry        = 3600
  description   = "K3S Node Agent TS Key"
  tags = ["tag:server", "tag:hcloud", "tag:k8s"]
}

resource "random_string" "agent_id" {
  for_each    = toset([for i in range(0, local.agents): tostring(i)])
  length = 4
  upper = false
  special = false
  min_lower = 2
  min_numeric = 2
}

resource "hcloud_ssh_key" "default" {
  name       = "default"
  public_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
}

resource "hcloud_server" "master" {
  for_each    = toset([for i in range(0, local.masters): tostring(i)])
  name        = "master-${random_string.cluster.id}-${random_string.master_id[each.key].id}"
  image       = "ubuntu-22.04" # just to get the server started
  server_type = local.master_server_type
  location    = local.region
  labels = local.labels
  placement_group_id = hcloud_placement_group.default.id
  ssh_keys    = [hcloud_ssh_key.default.id]
  user_data = <<-EOF
  mkdir -p /run/nixos
  cat<<META>/run/nixos/metadata
  CLUSTER_ID=${random_string.cluster.id}
  NODE_ID=${random_string.master_id[each.key].id}
  NODENAME=master-${random_string.cluster.id}-${random_string.master_id[each.key].id}
  TS_AUTH_KEY=${tailscale_tailnet_key.master_key[each.key].key}
  REGION=${local.region}
  ZONE=${local.zone}
  INITIAL_MASTER=master-${random_string.cluster.id}-${random_string.master_id["0"].id}
  META
  EOF
}

resource "hcloud_server" "agent" {
  for_each    = toset([for i in range(0, local.agents): tostring(i)])
  name        = "agent-${random_string.cluster.id}-${random_string.agent_id[each.key].id}"
  image       = "ubuntu-22.04" # just to get the server started
  server_type = local.agent_server_type
  location    = local.region
  labels = local.labels
  placement_group_id = hcloud_placement_group.default.id
  ssh_keys    = [hcloud_ssh_key.default.id]
  user_data = <<-EOF
  mkdir -p /run/nixos
  cat<<META>/run/nixos/metadata
  CLUSTER_ID=${random_string.cluster.id}
  NODE_ID=${random_string.agent_id[each.key].id}
  NODENAME=agent-${random_string.cluster.id}-${random_string.agent_id[each.key].id}
  TS_AUTH_KEY=${tailscale_tailnet_key.agent_key[each.key].key}
  REGION=${local.region}
  ZONE=${local.zone}
  INITIAL_MASTER=master-${random_string.cluster.id}-${random_string.master_id["0"].id}
  META
  EOF
}

module "master-init-system-build" {
  source            = "github.com/nix-community/nixos-anywhere//terraform/nix-build"
  attribute         = ".#nixosConfigurations.hcloud-k3s-master-init.config.system.build.toplevel"
}

module "master-system-build" {
  source            = "github.com/nix-community/nixos-anywhere//terraform/nix-build"
  attribute         = ".#nixosConfigurations.hcloud-k3s-master.config.system.build.toplevel"
}

module "agent-system-build" {
  source            = "github.com/nix-community/nixos-anywhere//terraform/nix-build"
  attribute         = ".#nixosConfigurations.hcloud-k3s-agent.config.system.build.toplevel"
}

module "master-init-disko" {
  source         = "github.com/nix-community/nixos-anywhere//terraform/nix-build"
  attribute      = ".#nixosConfigurations.hcloud-k3s-master-init.config.system.build.diskoScript"
}

module "master-disko" {
  source         = "github.com/nix-community/nixos-anywhere//terraform/nix-build"
  attribute      = ".#nixosConfigurations.hcloud-k3s-master.config.system.build.diskoScript"
}

module "agent-disko" {
  source         = "github.com/nix-community/nixos-anywhere//terraform/nix-build"
  attribute      = ".#nixosConfigurations.hcloud-k3s-agent.config.system.build.diskoScript"
}

module "master-init-install" {
  source            = "github.com/nix-community/nixos-anywhere//terraform/install"
  nixos_system      = module.master-init-system-build.result.out
  nixos_partitioner = module.master-init-disko.result.out
  target_host       = hcloud_server.master["0"].ipv4_address
  disk_encryption_key_scripts = [
    {
      path = "/tmp/disk.key"
      script = "./disk-encryption-key.sh"
    }
  ]
  extra_files_script = "./extra-files-script.sh"
}

module "master-install" {
  for_each    = toset([for i in range(1, local.masters): tostring(i)])  
  source            = "github.com/nix-community/nixos-anywhere//terraform/install"
  nixos_system      = module.master-system-build.result.out
  nixos_partitioner = module.master-disko.result.out
  target_host       = hcloud_server.master[each.key].ipv4_address
  disk_encryption_key_scripts = [
    {
      path = "/tmp/disk.key"
      script = "./disk-encryption-key.sh"
    }
  ]
  extra_files_script = "./extra-files-script.sh"
}

module "agent-install" {
  for_each    = toset([for i in range(0, local.agents): tostring(i)])
  source            = "github.com/nix-community/nixos-anywhere//terraform/install"
  nixos_system      = module.agent-system-build.result.out
  nixos_partitioner = module.agent-disko.result.out
  target_host       = hcloud_server.agent[each.key].ipv4_address
  disk_encryption_key_scripts = [
    {
      path = "/tmp/disk.key"
      script = "./disk-encryption-key.sh"
    }
  ]
  extra_files_script = "./extra-files-script.sh"
}

output "k8s_api" {
  value = "k8s-api-${random_string.cluster.id}"
}
