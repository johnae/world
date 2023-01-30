{
  config,
  lib,
  ...
}: let
  inherit (lib) mapAttrs' nameValuePair filterAttrs;
  inherit (builtins) toString;
  inherit (config.users) users;
in {
  environment.state."/keep" = {
    directories = [
      "/var/log"
      "/var/lib/bluetooth"
      "/var/lib/wireguard"
      "/var/lib/systemd"
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

    users = mapAttrs' (
      userName: conf:
        nameValuePair (toString conf.uid) {
          directories = [
            "/home/${userName}/Downloads"
            "/home/${userName}/Documents"
            "/home/${userName}/Development"
            "/home/${userName}/Photos"
            "/home/${userName}/Pictures"
            "/home/${userName}/Games"
            "/home/${userName}/Sync"
            "/home/${userName}/Mail"
            "/home/${userName}/.local/share/direnv"
            "/home/${userName}/.local/share/password-store"
            "/home/${userName}/.local/share/fish"
            "/home/${userName}/.local/share/containers"
            "/home/${userName}/.local/share/Steam"
            "/home/${userName}/.local/share/vulkan"
            "/home/${userName}/.local/share/qutebrowser"
            "/home/${userName}/.local/share/syncthing-data"
            "/home/${userName}/.local/share/nix"
            "/home/${userName}/.mail"
            "/home/${userName}/.cargo"
            "/home/${userName}/.cache/mu"
            "/home/${userName}/.cache/nix"
            "/home/${userName}/.cache/nix-index"
            "/home/${userName}/.cache/rbw"
            "/home/${userName}/.mozilla/firefox/default"
            "/home/${userName}/.config/chromium"
            "/home/${userName}/.cache/chromium"
            "/home/${userName}/.config/chromium-alt"
            "/home/${userName}/.cache/chromium-alt"
            "/home/${userName}/.config/chromium-private"
            "/home/${userName}/.cache/chromium-private"
            "/home/${userName}/.config/qutebrowser"
            "/home/${userName}/.cache/qutebrowser"
            "/home/${userName}/.config/obs-studio"
            "/home/${userName}/.config/Element"
            "/home/${userName}/.config/Slack"
            "/home/${userName}/.config/Microsoft"
            "/home/${userName}/.gnupg"
            "/home/${userName}/.config/gcloud"
            "/home/${userName}/.emacs.d"
            "/home/${userName}/.terraform.d"
          ];
          files = [
            "/home/${userName}/.kube/config"
            "/home/${userName}/.ssh/known_hosts"
            "/home/${userName}/.spotify_token_cache.json"
            "/home/${userName}/.config/nushell/history.txt"
          ];
        }
    ) (filterAttrs (_: user: user.isNormalUser) users);
  };
}
