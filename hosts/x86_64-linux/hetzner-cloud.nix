{...}: let
  authkeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIzm5RyD+1nfy1LquvkEog4SZtPgdhzjr49jSC8PAinp"
  ];
in {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN3wV0xe1C2JtwQHwHNL3yYnGsXPfnQAvElF37ux7qkc";

  imports = [
    ../../profiles/hcloud.nix
    ../../profiles/disk/disko-basic.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  boot.kernelParams = [
    "ip=:::::eth0:dhcp"
  ];

  boot.initrd.network = {
    enable = true;
    postCommands = "echo 'cryptsetup-askpass' >> /root/.profile";
    flushBeforeStage2 = true;
    ssh = {
      enable = true;
      port = 2222;
      hostKeys = [
        "/etc/ssh/initrd_ed25519_key"
      ];
      authorizedKeys = authkeys;
    };
  };

  age.secrets = {
    ts-google-9k-hcloud = {
      file = ../../secrets/ts-google-9k-hcloud.age;
    };
  };

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server" "tag:hcloud"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/var/run/agenix/ts-google-9k-hcloud";
  };

  users.users.root.openssh.authorizedKeys.keys = authkeys;
}
