{
  adminUser,
  hostName,
  config,
  pkgs,
  ...
}: {
  imports = [
    ../../profiles/home-manager.nix
  ];

  services.nix-daemon.enable = true;

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

  system.defaults.CustomSystemPreferences."com.apple.Terminal" = {
    Shell = "/Users/johnaxele/.nushim.sh";
    "Default Window Settings" = "Pro";
    "Startup Window Settings" = "Pro";
  };

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

  nix.settings.trusted-users = [adminUser.name];
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
  ];

  fonts.packages = with pkgs; [
    emacs-all-the-icons-fonts
    etBook
    font-awesome_5
    google-fonts
    powerline-fonts
    roboto
    (pkgs.nerdfonts.override {
      fonts = ["JetBrainsMono" "DroidSansMono" "Iosevka" "IosevkaTerm" "RobotoMono"];
    })
  ];

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  environment.etc."zshrc.local".text = ''
    export PATH=/opt/homebrew/bin:$PATH
  '';

  environment.etc."bash.local".text = ''
    export PATH=/opt/homebrew/bin:$PATH
  '';

  home-manager.users.${adminUser.name} = {
    home.stateVersion = "21.05";
    home.username = "${adminUser.name}";
    home.homeDirectory = "/Users/${adminUser.name}";
    home.file.".nushim.sh" = {
      executable = true;
      text = ''
	source /etc/bashrc
	exec nu
      '';
    };
    home.packages = [
      pkgs.jetbrains.idea-ultimate
      pkgs.grpcurl
    ];
    imports = [../../users/profiles/mac.nix];
    inherit (adminUser) userinfo;
    programs = {
      git = {
        extraConfig = {
          gpg.format = "ssh";
          commit.gpgSign = true;
          tag.forceSignAnnotated = true;
        };
      };
    };
  };
}