{
  adminUser,
  pkgs,
  ...
}: {
  publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKCP9Tx9xhe3GbxOUGqXxxQ1O/QO8h+IfJaDSlal25Ek";

  age.secrets = {
    openai-api-key = {
      file = ../../secrets/openai-api-key.age;
      owner = "${toString adminUser.uid}";
    };
    anthropic-api-key = {
      file = ../../secrets/anthropic-api-key.age;
      owner = "${toString adminUser.uid}";
    };
  };

  imports = [
    ../../profiles/home-manager.nix
  ];

  homebrew.enable = true;
  homebrew.taps = [
    {
      name = "spotify/sptaps";
      clone_target = "git@ghe.spotify.net:shared/homebrew-spotify.git";
      force_auto_update = true;
    }
  ];

  homebrew.brews = [
    "spotify/sptaps/kubectl-site"
    "spotify/sptaps/hmtools"
    "spotify/sptaps/protoman"
  ];

  homebrew.casks = [
    "logi-options+"
    "chromium"
  ];

  #services.karabiner-elements.enable = true; ## doesn't work atm
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

  services.yabai.enable = false;
  services.yabai.config = {
    focus_follows_mouse = "autoraise";
    mouse_follows_focus = "off";
    window_placement = "second_child";
    window_opacity = "off";
    top_padding = 36;
    bottom_padding = 10;
    left_padding = 10;
    right_padding = 10;
    window_gap = 10;
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
    pkgs.bottom
    pkgs.devenv
    pkgs.fd
    pkgs.git
    pkgs.hyperfine
    pkgs.jq
    pkgs.rbw-atomic-unlock
    pkgs.ripgrep
    pkgs.sd
    pkgs.serpl
    pkgs.stats
    pkgs.vim
    pkgs.zip
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
    programs.git.userEmail = "johnaxele@spotify.com";
  };
}
