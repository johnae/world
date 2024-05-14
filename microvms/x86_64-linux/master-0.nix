{...}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPRUfwPUxHqtVRsV3CdDRDEAYTg28ZdK5/Mz/GlcZdiv";

  vmId = 0;

  imports = [
    ../../profiles/microvm.nix
    ../../profiles/tailscale.nix
    ../../profiles/zram.nix
  ];

  environment.persistence."/keep" = {
    hideMounts = true;
    files = [
      "/etc/ssh/ssh_host_ed25519_key"
    ];
  };

  age.secrets = {
    ts-google-9k = {
      file = ../../secrets/ts-google-9k.age;
    };
    k3s-token = {
      file = ../../secrets/k3s/token.age;
    };
  };

  services.tailscale.auth = {
    enable = true;
    args.advertise-tags = ["tag:server"];
    args.ssh = true;
    args.accept-routes = false;
    args.accept-dns = true;
    args.advertise-exit-node = true;
    args.auth-key = "file:/var/run/agenix/ts-google-9k";
  };

  microvm.shares = [
    {
      proto = "virtiofs";
      tag = "keep";
      source = "/var/lib/microvms/master-0/keep";
      mountPoint = "/keep";
    }
  ];
}
