{
  adminUser,
  lib,
  ...
}: {
  environment.persistence."/keep" = {
    hideMounts = true;
    directories = [
      "/root"
      "/var/lib/bluetooth"
      "/var/lib/containers"
      "/var/lib/cups"
      "/var/lib/docker"
      "/var/lib/systemd"
      "/var/lib/tailscale"
      "/var/lib/wireguard"
      "/var/lib/libvirt"
      "/var/log"
    ];
    files = [
      "/etc/machine-id"
      "/etc/ssh/ssh_host_ed25519_key"
      "/etc/ssh/ssh_host_ed25519_key.pub"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_rsa_key.pub"
    ];

    users.${adminUser.name} = {
      directories = [
        "Development"
        "Documents"
        "Downloads"
        "Games"
        "Mail"
        "Photos"
        "Pictures"
        "Sync"
        ".cache/chromium"
        ".cache/chromium-alt"
        ".cache/chromium-private"
        ".cache/chromium-work"
        ".cache/mu"
        ".cache/nix"
        ".cache/nix-index"
        ".cache/qutebrowser"
        ".cache/rbw"
        ".cache/zellij"
        ".cargo"
        ".config/audacity"
        ".config/chromium"
        ".config/chromium-alt"
        ".config/chromium-private"
        ".config/chromium-work"
        ".config/Element"
        ".config/gcloud"
        ".config/github-copilot"
        ".config/Microsoft"
        ".config/obs-studio"
        ".config/qutebrowser"
        ".config/Slack"
        ".config/teams-for-linux"
        ".emacs.d"
        ".gnupg"
        ".local/share/atuin"
        ".local/share/containers"
        ".local/share/direnv"
        ".local/share/fish"
        ".local/share/nix"
        ".local/share/password-store"
        ".local/share/qutebrowser"
        ".local/share/Steam"
        ".local/share/syncthing-data"
        ".local/share/vulkan"
        ".mail"
        ".mozilla/firefox/default"
        ".terraform.d"
        ".steam"
      ];
      files = [
        ".config/nushell/history.txt"
        ".config/tenacity.cfg"
        ".kube/config"
        ".spotify_token_cache.json"
        ".ssh/known_hosts"
      ];
    };
  };
}
