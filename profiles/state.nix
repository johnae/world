{...}:

{
  environment.state."/keep" =
    {
      directories = [
        "/var/log"
        "/var/lib/bluetooth"
        "/var/lib/wireguard"
        "/var/lib/systemd/coredump"
        "/var/lib/containers"
        "/var/lib/tailscale"
        "/root"
      ];
      files = [
        "/etc/machine-id"
        "/etc/ssh/ssh_host_rsa_key"
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_rsa_key.pub"
        "/etc/ssh/ssh_host_ed25519_key.pub"
      ];
    };
}
