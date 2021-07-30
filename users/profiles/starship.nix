{
  programs.starship = {
    enable = true;
    settings = {
      kubernetes.disabled = false;
      kubernetes.style = "bold blue";
      nix_shell.disabled = false;
      rust.symbol = " ";
      gcloud = {
        format = "on [$symbol(\\($project\\))]($style) ";
      };
    };
  };
}
