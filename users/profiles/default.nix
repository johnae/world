{
  config,
  pkgs,
  ...
}: let
  inherit (config.home) username;
in {
  imports = [
    ./accounts.nix
    ./aichat.nix
    ./aider.nix
    ./bat.nix
    ./bower.nix
    ./claude.nix
    ./git.nix
    ./gitui.nix
    # ./emacs/default.nix
    ./helix.nix
    ./jujutsu.nix
    ./kubie.nix
    ./mergiraf.nix
    ./nushell/default.nix
    ./rbw.nix
    ./ssh.nix
    ./starship.nix
    ./wezterm/default.nix
    ./zellij.nix
  ];

  home.sessionVariables = {
    NB_DIR = "/home/${username}/Sync/pkm";
    EDITOR = "hx";
    VISUAL = "hx";
    KUBECONFIG = "/home/${username}/.kube/config";
    COLORTERM = "truecolor";
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
  };

  home.homeDirectory = "/home/${username}";

  home.packages = with pkgs; [
    alejandra
    awscli2
    # azure-cli
    cachix
    carapace
    devenv
    fzf # # for certain utilities that depend on it
    difftastic
    gh
    git-crypt
    google-cloud-sdk-gke
    jwt-cli
    kubectl
    kubectx
    kubelogin
    kubelogin-oidc
    kustomize
    lm_sensors
    mattermost-desktop
    nil
    nixd
    nix-index
    scripts
    tdf
  ];

  xdg.enable = true;

  programs.command-not-found.enable = false;

  programs.lsd = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.skim.enable = true;

  systemd.user.services.nix-index = {
    Unit.Description = "Nix-index indexes all files in nixpkgs etc";
    Service.ExecStart = "${pkgs.nix-index}/bin/nix-index";
  };

  systemd.user.timers.nix-index = {
    Unit.Description = "Nix-index indexes all files in nixpkgs etc";
    Timer = {
      OnCalendar = "daily";
      Unit = "nix-index.service";
    };
    Install.WantedBy = ["timers.target"];
  };

  home.stateVersion = "25.05";
}
