{
  programs.starship = {
    enable = true;
    enableNushellIntegration = false; ## because starship generates bad config right now
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
