{
  adminUser,
  hostName,
  lib,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMfXrbXEQcJ85CGW9/qnm/jS8Vp6GP2yQ2piFF8+OFWR";
  syncthingDeviceID = "HBL5ZRB-R2STGW5-LMAYYHX-KOFTP3X-VO4IV6E-PEDKZ3N-WCRR7BY-F5C7AAP";

  imports = [
    ../../profiles/admin-user/home-manager.nix
    ../../profiles/admin-user/user.nix
    ../../profiles/home-manager.nix
    ../../profiles/server.nix
    ../../profiles/zram.nix
  ];

  services.btrfs.autoScrub.enable = lib.mkForce false;

  system.autoUpgrade = {
    enable = true;
    flake = "github:johnae/world";
    allowReboot = true;
    dates = "06:00";
    randomizedDelaySec = "5min";
    enableSentinel = false; ## not running kubernetes here
  };

  ## for tailscale exit node functionality
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;

  # services.tailscale.auth = {
  #   enable = true;
  #   args.advertise-tags = ["tag:server"];
  #   args.ssh = true;
  #   args.accept-routes = false;
  #   args.accept-dns = false;
  #   args.advertise-exit-node = true;
  #   args.auth-key = "file:/var/run/agenix/ts-google-9k";
  # };

  # networking = {
  #   defaultGateway = "192.168.20.1";
  #   firewall.trustedInterfaces = ["tailscale0"];
  #   interfaces.eth0.ipv4.addresses = [
  #     {
  #       address = "192.168.20.143";
  #       prefixLength = 24;
  #     }
  #   ];
  # };

  home-manager = {
    users.${adminUser.name} = {
      imports = [../../users/profiles/headless.nix];
    };
  };
}
