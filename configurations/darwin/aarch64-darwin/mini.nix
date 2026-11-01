{
  adminUser,
  config,
  hostName,
  pkgs,
  ...
}: {
  age.rekey = {
    hostPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL54GmqIwTv5EZ2t944ZQus3x3jXyVPu6//a89Kd/nIE";
    storageMode = "local";
    localStorageDir = ../../../secrets/rekeyed + "/${hostName}";
  };
  age.secrets = {
    ts-google-9k = {
      rekeyFile = ../../../secrets/ts-google-9k.age;
      owner = "${toString adminUser.uid}";
    };
    spool-agent-token = {
      rekeyFile = ../../../secrets/spool-agent-mini-token.age;
    };
  };

  imports = [
    ../../../profiles/home-manager.nix
  ];


  homebrew.enable = true;
  homebrew.casks = [
    "logi-options+"
    "chromium"
    # nixpkgs' jetbrains.idea can no longer unpack its darwin dmg.
    "intellij-idea"
  ];

  #services.karabiner-elements.enable = true; ## doesn't work atm
  system.primaryUser = adminUser.name;
  system.defaults.dock.autohide = true;
  security.pam.services.sudo_local.touchIdAuth = true;
  system.defaults.trackpad.Clicking = true;
  system.defaults.NSGlobalDomain.AppleInterfaceStyle = "Dark";
  system.defaults.NSGlobalDomain."com.apple.trackpad.scaling" = 3.0;
  system.defaults.NSGlobalDomain."com.apple.trackpad.trackpadCornerClickBehavior" = 1;
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 15;
  system.defaults.NSGlobalDomain.KeyRepeat = 2;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;

  services.tailscale.enable = true;

  # Determinate Nix owns nix.conf (nix.enable is off below) and includes this
  # file, so trusted users and our cache go here. The first switch needs the
  # hand-written one moved aside, and the daemon restarted after.
  environment.etc."nix/nix.custom.conf".text = ''
    trusted-users = root ${adminUser.name} spool-agent
    extra-substituters = https://nixcache.9000.dev
    extra-trusted-public-keys = nixcache.9000.dev:MNr2kidQv9GfkQKUarqdpaufX7JMMSixX/cdlS5i5i0=
    # CI builds the whole darwin set here: collect garbage when less than
    # 20G is left, up to 60G free, so a big update cannot fill the disk.
    min-free = 21474836480
    max-free = 64424509440
  '';

  # Takes world's aarch64-darwin builds (`requires: [aarch64-darwin]`) for
  # every project in the 9000 org.
  services.spool-agent = {
    enable = true;
    id = "mini";
    capabilities = ["shell" "aarch64-darwin"];
    tokenFile = config.age.secrets.spool-agent-token.path;
    runtimePackages = [pkgs.bash pkgs.git pkgs.jq pkgs.openssl pkgs.gnutar pkgs.gzip];
  };

  # What system.autoUpgrade does on the NixOS hosts, which nix-darwin lacks:
  # switch to world's main every morning. The nightly update has merged and
  # CI has cached the darwin build by then, so this mostly downloads. A
  # switch that changes the spool agent restarts it, which hands a running
  # job back to be dispatched again.
  launchd.daemons.darwin-upgrade = {
    path = [pkgs.git "/nix/var/nix/profiles/default/bin"];
    script = ''
      echo "darwin-upgrade: $(date)"
      /run/current-system/sw/bin/darwin-rebuild switch --refresh --flake github:johnae/world#${hostName}
    '';
    serviceConfig = {
      StartCalendarInterval = [{Hour = 6; Minute = 0;}];
      StandardOutPath = "/var/log/darwin-upgrade.log";
      StandardErrorPath = "/var/log/darwin-upgrade.log";
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
    et-book
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
      command = "/bin/launchctl config user path '/opt/homebrew/bin:/Users/${adminUser.name}/.nix-profile/bin:/etc/profiles/per-user/${adminUser.name}/bin:/run/current-system/sw/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin'";
      serviceConfig.RunAtLoad = true;
    };
  };

  home-manager.users.${adminUser.name} = {
    home.stateVersion = "25.05";
    home.username = "${adminUser.name}";
    home.homeDirectory = "/Users/${adminUser.name}";
    home.packages = [
      pkgs.grpcurl
    ];
    imports = [../../../users/profiles/mac.nix];
    inherit (adminUser) userinfo;
    programs = {
      git = {
        signing.format = "ssh";
        settings = {
          commit.gpgSign = true;
          tag.forceSignAnnotated = true;
        };
      };
    };
  };
}
