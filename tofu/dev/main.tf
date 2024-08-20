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

  backend "local" {
    path = "/home/john/Sync/world-tfstate/terraform.tfstate"
  }
}

variable "kexec_tarball" {
  type = string
  description = "path/url to the kexec tarball to use"
}

variable "server_type" {
  type = string
  description = "The hetzner server_type to deploy"
  default = "cpx31"
}

locals {
  server_type = var.server_type
  #server_type = "cpx21" # AMD 3 vCPU, 4 GB RAM, 80 GB NVMe SSD
  # server_type = "cpx51" # AMD 16 vCPU, 32 GB RAM, 360 GB NVMe SSD
  # server_type = "ccx63" # AMD 48 vCPU, 192 GB RAM etc
  region = "hel1"
  zone = "hel1-dc2"
  labels = {
    "tfstate" : "dev"
    "tailscale" : "yes"
  }
}

resource "tailscale_tailnet_key" "dev" {
  reusable      = false
  ephemeral     = true
  preauthorized = true
  expiry        = 1800
  description   = "hcloud-dev"
  tags = [
    "tag:hcloud",
    "tag:k8s-admins",
    "tag:server"
  ]
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

resource "hcloud_volume" "dev" {
  name = "dev"
  location = local.region
  size     = 100
}

resource "hcloud_volume_attachment" "dev" {
  volume_id = hcloud_volume.dev.id
  server_id = hcloud_server.dev.id
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
  extra_environment = {
    TS_AUTH_KEY = tailscale_tailnet_key.dev.key
  }
  debug_logging = true
  # kexec_tarball_url = "$(nix build --print-out-paths .#packages.x86_64-linux.kexec-installer-nixos-unstable-noninteractive)/nixos-kexec-installer-noninteractive-x86_64-linux.tar.gz"
  kexec_tarball_url = var.kexec_tarball
  disk_encryption_key_scripts = [
    {
      path = "/tmp/disk.key"
      script = "./disk-encryption-key.sh"
    }
  ]
  extra_files_script = "./extra-files-script.sh"
}
