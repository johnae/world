{
  adminUser,
  config,
  pkgs,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL54GmqIwTv5EZ2t944ZQus3x3jXyVPu6//a89Kd/nIE";
  age.secrets = {
    ts-google-9k = {
      file = ../../secrets/ts-google-9k.age;
      owner = "${toString adminUser.uid}";
    };
    buildkite-agent-nix-build-token = {
      file = ../../secrets/buildkite-token.age;
      owner = "${toString adminUser.uid}";
    };
    buildkite-agent-nix-build-ssh-key = {
      file = ../../secrets/buildkite-ssh-key.age;
      owner = "${toString adminUser.uid}";
    };
    buildkite-agent-nix-build-cachix-signing-key = {
      file = ../../secrets/cachix-signing-key.age;
      owner = "${toString adminUser.uid}";
    };
    buildkite-agent-nix-build-github-app-auth-key = {
      file = ../../secrets/github-app-bk-auth.age;
      owner = "${toString adminUser.uid}";
    };
  };

  imports = [
    ../../profiles/home-manager.nix
  ];

  services.nix-daemon.enable = true;

  homebrew.enable = true;
  homebrew.casks = [
    "logi-options+"
    "chromium"
  ];

  #services.karabiner-elements.enable = true; ## doesn't work atm
  system.defaults.dock.autohide = true;
  security.pam.enableSudoTouchIdAuth = true;
  system.defaults.trackpad.Clicking = true;
  system.defaults.NSGlobalDomain.AppleInterfaceStyle = "Dark";
  system.defaults.NSGlobalDomain."com.apple.trackpad.scaling" = 3.0;
  system.defaults.NSGlobalDomain."com.apple.trackpad.trackpadCornerClickBehavior" = 1;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 15;
  system.defaults.NSGlobalDomain.KeyRepeat = 2;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;

  services.tailscale.enable = true;

  nix.settings.trusted-users = ["root" adminUser.name "buildkite-agent-nix-build"];
  services.buildkite-agents.nix-build = let
    inherit (config.services.buildkite-agents.nix-build) dataDir;
  in {
    enable = true;
    tokenPath = config.age.secrets.buildkite-agent-nix-build-token.path;
    privateSshKeyPath = config.age.secrets.buildkite-agent-nix-build-ssh-key.path;
    extraConfig = ''
      plugins-path=${dataDir}/plugins
      spawn=4
    '';
    runtimePackages = [
      pkgs.bash
      pkgs.cachix
      pkgs.coreutils
      pkgs.curl
      pkgs.git
      pkgs.gnutar
      pkgs.gzip
      pkgs.jq
      pkgs.nix
      pkgs.openssl
      pkgs.procps
    ];

    tags = {
      nix = "true";
      nix-darwin = "true";
      macos = "true";
      arch = "aarch64-darwin";
      queue = "default-queue";
    };

    hooks = {
      environment = ''
        CACHIX_SIGNING_KEY="$(head -1 ${config.age.secrets."buildkite-agent-nix-build-cachix-signing-key".path})"
        CACHE_NAME=insane
        GITHUB_APP_RSA_KEY_FILE="${config.age.secrets."buildkite-agent-nix-build-github-app-auth-key".path}"
        export CACHIX_SIGNING_KEY CACHE_NAME GITHUB_APP_RSA_KEY_FILE
      '';
      pre-command = ''
        #!/usr/bin/env bash
        cachix use "$CACHE_NAME"
      '';
      command = ''
        #!/usr/bin/env bash
        cachix --verbose watch-exec "$CACHE_NAME" -- bash -c "$BUILDKITE_COMMAND"
      '';
    };
  };

  nix.enable = false;

  users = {
    users.${adminUser.name} = {
      inherit (adminUser) uid;
      home = "/Users/${adminUser.name}";
      shell = pkgs.nushell; ## doesn't do anything on darwin I believe
    };
  };

  environment.systemPackages = [
    pkgs.git
    pkgs.fd
    pkgs.ripgrep
    pkgs.bottom
    pkgs.hyperfine
    pkgs.jq
    pkgs.sd
    pkgs.vim
    pkgs.zip
    pkgs.devenv
    pkgs.rbw-atomic-unlock
  ];

  fonts.packages = with pkgs; [
    emacs-all-the-icons-fonts
    etBook
    font-awesome_5
    google-fonts
    powerline-fonts
    roboto
    nerd-fonts.jetbrains-mono
    nerd-fonts.droid-sans-mono
    nerd-fonts.iosevka
    nerd-fonts.iosevka-term
    nerd-fonts.roboto-mono
  ];

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  environment.etc."zshrc.local".text = ''
    export PATH=/opt/homebrew/bin:$PATH
    export SHELL=nu
  '';

  environment.etc."bash.local".text = ''
    export PATH=/opt/homebrew/bin:$PATH
    export SHELL=nu
  '';

  ## this sets the PATH for GUI apps
  ## needs a restart
  launchd.user.agents = {
    user-paths = {
      command = "/bin/launchctl config user path '/opt/homebrew/bin:/Users/johnaxele/.nix-profile/bin:/etc/profiles/per-user/johnaxele/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin'";
      serviceConfig.RunAtLoad = true;
    };
  };

  home-manager.users.${adminUser.name} = {
    home.stateVersion = "25.05";
    home.username = "${adminUser.name}";
    home.homeDirectory = "/Users/${adminUser.name}";
    home.packages = [
      pkgs.jetbrains.idea-ultimate
      pkgs.grpcurl
    ];
    imports = [../../users/profiles/mac.nix];
    inherit (adminUser) userinfo;
    programs = {
      git = {
        signing.format = "ssh";
        extraConfig = {
          commit.gpgSign = true;
          tag.forceSignAnnotated = true;
        };
      };
    };
  };
}
