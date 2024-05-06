{
  pkgs,
  lib,
  modulesPath,
  ...
}: {
  # publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEEPD945cTDxeNhGljSKqQfRCUeXcwIDKOBD847OECQs";
  # syncthingDeviceID = "XOPUBYF-LTOERSA-NGLA6ZJ-BU455JS-JOTCFQP-JGZP6WC-VRUTNOX-5YEUVQD";

  bcachefs = {
    disks = ["/dev/nvme0n1" "/dev/nvme1n1"];
    devices = ["/dev/mapper/encrypted_root" "/dev/mapper/encrypted_root1"];
    # disks = ["/dev/nvme0n1"];
    # devices = ["/dev/mapper/encrypted_root"];
  };

  # btrfs = {
  #   disks = ["/dev/nvme0n1" "/dev/nvme1n1"];
  # };

  imports = [
    ../../profiles/disk/bcachefs-on-luks.nix
    # ../../profiles/disk/btrfs-on-luks.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/server.nix
    ../../profiles/state.nix
    ../../profiles/zram.nix
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  programs.ssh.startAgent = true;

  boot.kernelParams = [
    "ip=dhcp"
  ];

  boot.initrd.availableKernelModules = [
    "igb"
    "nvme"
    "ahci"
    "usbhid"
    "virtio"
  ];

  # fileSystems."/keep" = {
  #   device = lib.mkForce "UUID=8e1e4b4c-3e5c-4b1d-9f4b-1b1c5d4b7a4c";
  # };
  # lsblk -f

  boot.initrd.systemd = let
    mount = pkgs.writeShellScriptBin "bcachefs-mount-helper" ''
      mkdir /sysroot
      until bcachefs mount -o rw,relatime,metadata_replicas=2,compression=zstd,background_compression=zstd /dev/mapper/encrypted_root:/dev/mapper/encrypted_root1 /sysroot; do
        sleep 1
      done
    '';
  in {
    enable = true;
    storePaths = ["${mount}/bin/bcachefs-mount-helper"];
    users.root.shell = "${mount}/bin/bcachefs-mount-helper";
    # services.bcachefs-mount-helper = {
    #   description = "Mount the bcachefs root filesystem";
    #   before = ["sysroot-keep.mount"];
    #   # after = ["local-fs.target"];
    #   # wants = ["local-fs.target"];
    #   serviceConfig = {
    #     Type = "oneshot";
    #     ExecStart = "${mount}/bin/bcachefs-mount-helper";
    #   };
    # };
  };

  boot.initrd.network = {
    enable = true;
    # postCommands = "echo 'cryptsetup-askpass' >> /root/.profile";
    flushBeforeStage2 = true;
    ssh = {
      enable = true;
      port = 2222;
      ## This isn't so nice. Have to copy the file to /keep/secrets and keep it there.
      hostKeys = [
        /keep/secrets/initrd_ed25519_key
      ];
      authorizedKeys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ=="
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIJY3QSBIiRKN8/B3nHgCBDp;auQBOftphOeuF2TaBHGQSAAAABHNzaDo="
        "sk-ssh-ed25519@openssh.com AAAAGnNrLXNzaC1lZDI1NTE5QG9wZW5zc2guY29tAAAAIAwJWtQ5ZU9U0szWzJ+/GH2uvXZ15u9lL0RdcHdsXM0VAAAABHNzaDo="
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK+trOinD68RD1efI6p05HeaNA0SjzeRnUvpf22+jsq+"
      ];
    };
  };

  # networking = {
  #   defaultGateway = "65.109.85.129";
  #   defaultGateway6 = {
  #     address = "fe80::1";
  #     interface = "eth0";
  #   };
  #   firewall.trustedInterfaces = ["tailscale0"];
  #   interfaces.eth0.ipv4.addresses = [
  #     {
  #       address = "65.109.85.161";
  #       prefixLength = 26;
  #     }
  #   ];

  #   interfaces.eth0.ipv6.addresses = [
  #     {
  #       address = "2a01:4f9:3051:5389::2";
  #       prefixLength = 64;
  #     }
  #   ];
  # };
}
