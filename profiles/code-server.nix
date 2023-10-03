{pkgs, ...}: {
  services.code-server.extraPackages = with pkgs; [
    direnv
    carapace
    fzf
    gh
    git-branchless
    git-crypt
    git
    kubectl
    kubectx
    kubelogin-oidc
    kustomize
  ];
}
