{config, ...}: let
  rootKeys = config.users.users.root.openssh.authorizedKeys.keys;
in {
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
      authorizedKeys = rootKeys ++ ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGZIl2n7wHQPLzWBGPMdtWz5BoKcXnLKZ+CkMmvKAmr4"];
    };
  };
}
