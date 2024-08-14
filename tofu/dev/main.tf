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
  }
}

locals {
  server_type = "cpx21" # AMD 3 vCPU, 4 GB RAM, 80 GB NVMe SSD
  region = "hel1"
  zone = "hel1-dc2"
  labels = {
    "tfstate" : "dev"
    "tailscale" : "yes"
  }
}

resource "hcloud_placement_group" "default" {
  name = "default"
  type = "spread"
  labels = local.labels
}

resource "random_string" "host" {
  length = 6
  upper = false
  special = false
  min_lower = 2
  min_numeric = 2
}

resource "hcloud_ssh_key" "default" {
  name       = "default"
  public_key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
}

resource "hcloud_server" "dev" {
  name        = "dev-${random_string.host.id}"
  image       = "ubuntu-22.04" # just to get the server started
  server_type = local.server_type
  location    = local.region
  labels = local.labels
  placement_group_id = hcloud_placement_group.default.id
  ssh_keys    = [hcloud_ssh_key.default.id]
}

module "dev-system-build" {
  source            = "github.com/nix-community/nixos-anywhere//terraform/nix-build"
  attribute         = ".#nixosConfigurations.hcloud-dev.config.system.build.toplevel"
}

module "dev-disko" {
  source         = "github.com/nix-community/nixos-anywhere//terraform/nix-build"
  attribute      = ".#nixosConfigurations.hcloud-dev.config.system.build.diskoScript"
}

module "dev-install" {
  source            = "github.com/nix-community/nixos-anywhere//terraform/install"
  nixos_system      = module.dev-system-build.result.out
  nixos_partitioner = module.dev-disko.result.out
  target_host       = hcloud_server.dev.ipv4_address
  debug_logging = true
  # kexec_tarball_url = "$(nix build --print-out-paths .#packages.x86_64-linux.kexec-installer-nixos-unstable-noninteractive)/nixos-kexec-installer-noninteractive-x86_64-linux.tar.gz"
  kexec_tarball_url = "../../result/nixos-kexec-installer-noninteractive-x86_64-linux.tar.gz"
  disk_encryption_key_scripts = [
    {
      path = "/tmp/disk.key"
      script = "./disk-encryption-key.sh"
    }
  ]
  extra_files_script = "./extra-files-script.sh"
}