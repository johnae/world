{config, lib, ...}:

let
  inherit (lib) mapAttrs' nameValuePair filterAttrs;
  inherit (builtins) toString;
  users = config.users.users;
in
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
        "/var/lib/cups"
        "/var/lib/docker"
        "/root"
      ];
      files = [
        "/etc/machine-id"
        "/etc/ssh/ssh_host_rsa_key"
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_rsa_key.pub"
        "/etc/ssh/ssh_host_ed25519_key.pub"
      ];

      users = mapAttrs' (userName: conf:
        nameValuePair (toString conf.uid) {
          directories = [
            "/home/${userName}/Downloads"
            "/home/${userName}/Documents"
            "/home/${userName}/Development"
            "/home/${userName}/Photos"
            "/home/${userName}/Pictures"
            "/home/${userName}/Games"
            "/home/${userName}/Sync"
            "/home/${userName}/.local/share/direnv"
            "/home/${userName}/.local/share/password-store"
            "/home/${userName}/.local/share/fish"
            "/home/${userName}/.local/share/containers"
            "/home/${userName}/.local/share/Steam"
            "/home/${userName}/.local/share/vulkan"
            "/home/${userName}/.local/share/qutebrowser"
            "/home/${userName}/.mail"
            "/home/${userName}/.cargo"
            "/home/${userName}/.cache/mu"
            "/home/${userName}/.cache/nix"
            "/home/${userName}/.cache/nix-index"
            "/home/${userName}/.cache/rbw"
            "/home/${userName}/.mozilla/firefox/default"
            "/home/${userName}/.config/chromium"
            "/home/${userName}/.cache/chromium"
            "/home/${userName}/.config/qutebrowser"
            "/home/${userName}/.cache/qutebrowser"
            "/home/${userName}/.gnupg"
            "/home/${userName}/.config/gcloud"
            "/home/${userName}/.emacs.d"
          ];
          files = [
            "/home/${userName}/.kube/config"
            "/home/${userName}/.ssh/known_hosts"
            "/home/${userName}/.spotify_token_cache.json"
          ];
        }
      ) (filterAttrs (_: user: user.isNormalUser) users);
    };
}
