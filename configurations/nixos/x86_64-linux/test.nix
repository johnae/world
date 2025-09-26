{modulesPath, ...}: {
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
    ../../../profiles/disk/bcachefs-on-luks.nix
    # ../../../profiles/disk/btrfs-on-luks.nix
    ../../../profiles/admin-user/user.nix
    ../../../profiles/server.nix
    ../../../profiles/state.nix
    ../../../profiles/zram.nix
    (modulesPath + "/profiles/qemu-guest.nix")
  ];

  programs.ssh.startAgent = true;

  boot.kernelParams = [
    "ip=dhcp"
    "console=ttyS0"
  ];

  boot.initrd.availableKernelModules = [
    "igb"
    "nvme"
    "e1000e"
    "ahci"
    "usbhid"
    "virtio"
  ];

  # fileSystems."/keep" = {
  #   device = lib.mkForce "UUID=8e1e4b4c-3e5c-4b1d-9f4b-1b1c5d4b7a4c";
  # };
  # lsblk -f

  boot.initrd.network = {
    enable = true;
    # postCommands = "echo 'cryptsetup-askpass' >> /root/.profile";
    flushBeforeStage2 = true;
    ssh = {
      enable = true;
      port = 2222;
      ## This isn't so nice. Have to copy the file to /keep/secrets and keep it there.
      hostKeys = [
        "/keep/secrets/initrd_ed25519_key"
      ];
      authorizedKeys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK+trOinD68RD1efI6p05HeaNA0SjzeRnUvpf22+jsq+"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
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
