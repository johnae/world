{pkgs, ...}: {
  services.code-server.extraPackages = with pkgs; [
    atuin
    carapace
    direnv
    fzf
    gh
    git
    git-branchless
    git-crypt
    kubectl
    kubectx
    kubelogin-oidc
    kustomize
  ];
}
