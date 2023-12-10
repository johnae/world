{
  adminUser,
  lib,
  ...
}: {
  environment.persistence."/keep" = {
    hideMounts = true;
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

    users.${adminUser.name} = {
      directories = [
        "Downloads"
        "Documents"
        "Development"
        "Photos"
        "Pictures"
        "Games"
        "Sync"
        "Mail"
        ".local/share/direnv"
        ".local/share/password-store"
        ".local/share/fish"
        ".local/share/containers"
        ".local/share/atuin"
        ".local/share/Steam"
        ".local/share/vulkan"
        ".local/share/qutebrowser"
        ".local/share/syncthing-data"
        ".local/share/nix"
        ".mail"
        ".cargo"
        ".cache/mu"
        ".cache/nix"
        ".cache/nix-index"
        ".cache/rbw"
        ".cache/zellij"
        ".mozilla/firefox/default"
        ".config/chromium"
        ".cache/chromium"
        ".config/chromium-alt"
        ".cache/chromium-alt"
        ".config/chromium-private"
        ".cache/chromium-private"
        ".config/chromium-work"
        ".cache/chromium-work"
        ".config/github-copilot"
        ".config/qutebrowser"
        ".cache/qutebrowser"
        ".config/obs-studio"
        ".config/audacity"
        ".config/Element"
        ".config/Slack"
        ".config/Microsoft"
        ".gnupg"
        ".config/gcloud"
        ".emacs.d"
        ".terraform.d"
      ];
      files = [
        ".kube/config"
        ".ssh/known_hosts"
        ".spotify_token_cache.json"
        ".config/nushell/history.txt"
        ".config/tenacity.cfg"
      ];
    };
  };
}
