{
  config,
  pkgs,
  ...
}: let
  inherit (config.home) homeDirectory;
  nushim = pkgs.writeShellScriptBin "nushim" ''
    source /etc/bashrc
    export SHELL=nu
    exec nu
  '';
in {
  imports = [
    ./aichat.nix
    ./aider.nix
    ./bat.nix
    ./git.nix
    ./gitui.nix
    ./emacs/default.nix
    ./helix.nix
    ./jujutsu.nix
    ./kubie.nix
    ./nushell/default.nix
    ./rbw.nix
    ./ssh.nix
    ./starship.nix
    ./wezterm/default.nix
    #    ./zellij.nix
  ];

  home.sessionVariables = {
    EDITOR = "hx";
    VISUAL = "hx";
    KUBECONFIG = "${homeDirectory}/.kube/config";
    COLORTERM = "truecolor";
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
  };

  home.packages = with pkgs; [
    alejandra
    awscli2
    # azure-cli
    cachix
    carapace
    devenv
    fzf # # for certain utilities that depend on it
    gh
    git-crypt
    google-cloud-sdk-gke
    jwt-cli
    kubectl
    kubectx
    kubelogin
    kubelogin-oidc
    kustomize
    nil
    nixd
    nix-index
    nushim
  ];

  xdg.enable = false;

  programs.command-not-found.enable = false;

  programs.lsd = {
    enable = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.skim.enable = true;
}
