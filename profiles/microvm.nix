{
  lib,
  pkgs,
  hostName,
  inputs,
  config,
  ...
}: let
  id = config.vmId;
in {
  imports = [
    inputs.microvm.nixosModules.microvm
  ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  networking.hostName = hostName;
  networking.firewall.allowedUDPPorts = [67];
  systemd.network.enable = true;
  systemd.network.networks."10-lan" = {
    matchConfig.Name = "enp*";
    networkConfig.DHCP = "ipv4";
  };
  services.tailscale.enable = true;

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ=="
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJY3QSBIiRKN8/B3nHgCBDp;auQBOftphOeuF2TaBHGQSAAAABHNzaDo="
    "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIAwJWtQ5ZU9U0szWzJ+/GH2uvXZ15u9lL0RdcHdsXM0VAAAABHNzaDo="
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK+trOinD68RD1efI6p05HeaNA0SjzeRnUvpf22+jsq+"
  ];

  environment.systemPackages = with pkgs; [
    zellij
    git
    helix
  ];

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
  };

  microvm = {
    volumes = [
      {
        mountPoint = "/var";
        image = "var.img";
        size = 256;
      }
    ];
    shares = [
      {
        # use "virtiofs" for MicroVMs that are started by systemd
        proto = "virtiofs";
        tag = "ro-store";
        # a host's /nix/store will be picked up so that the
        # size of the /dev/vda can be reduced.
        source = "/nix/store";
        mountPoint = "/nix/.ro-store";
      }
    ];
    socket = "control.socket";
    hypervisor = "cloud-hypervisor";
    vcpu = 4;
    mem = 16384;
    interfaces = [
      {
        type = "tap";
        id = "vm-${toString id}";
        mac = "02:00:00:00:00:${lib.fixedWidthString 2 "0" (toString id)}";
      }
    ];
  };
}
