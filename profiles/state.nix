{
  adminUser,
  config,
  lib,
  ...
}: {
  system.activationScripts = lib.mkIf config.ephemeralRoot {
    "createPersistentStorageDirs".deps = ["var-lib-private-permissions" "home-user-permissions" "users" "groups"];
    "var-lib-private-permissions" = {
      deps = ["specialfs"];
      text = ''
        mkdir -p /keep/var/lib/private
        chmod 0700 /keep/var/lib/private
      '';
    };
    "home-user-permissions" = {
      deps = ["specialfs"];
      text = ''
        mkdir -p /keep/home/${adminUser.name}
        chown -R ${toString adminUser.uid}:${toString adminUser.gid} /keep/home/${adminUser.name}
        chmod 0700 /keep/home/${adminUser.name}
      '';
    };
  };
  environment.persistence."/keep" = {
    enable = config.ephemeralRoot;
    hideMounts = true;
    directories = [
      "/root"
      "/var/lib/bluetooth"
      "/var/lib/containers"
      "/var/lib/cups"
      "/var/lib/docker"
      "/var/lib/nixos"
      "/var/lib/systemd"
      "/var/lib/tailscale"
      "/var/lib/wireguard"
      "/var/lib/libvirt"
      "/var/lib/microvms"
      "/var/lib/private/ollama"
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
        ".cache/shotwell"
        ".cache/mu"
        ".cache/nix"
        ".cache/nix-index"
        ".cache/qutebrowser"
        ".cache/rbw"
        ".cache/zellij"
        ".cargo"
        ".aws"
        ".config/audacity"
        ".config/chromium"
        ".config/chromium-alt"
        ".config/chromium-private"
        ".config/chromium-work"
        ".config/Element"
        ".config/gcloud"
        ".config/github-copilot"
        ".config/Mattermost"
        ".config/Microsoft"
        ".config/obs-studio"
        ".config/qutebrowser"
        ".config/shotwell"
        ".config/Slack"
        ".config/teams-for-linux"
        ".emacs.d"
        ".claude"
        ".gnupg"
        ".local/share/atuin"
        ".local/share/containers"
        ".local/share/direnv"
        ".local/share/fish"
        ".local/share/nix"
        ".local/share/password-store"
        ".local/share/qutebrowser"
        ".local/share/shotwell"
        ".local/share/Steam"
        ".local/share/syncthing-data"
        ".local/share/vulkan"
        ".mail"
        ".mozilla/firefox/default"
        ".nb"
        ".steam"
        ".terraform.d"
        ".ssh/config.d"
        ".zen"
      ];
      files = [
        ".config/nushell/history.txt"
        ".config/tenacity.cfg"
        ".kube/config"
        ".spotify_token_cache.json"
        ".claude.json"
        ".ssh/known_hosts"
      ];
    };
  };
}
