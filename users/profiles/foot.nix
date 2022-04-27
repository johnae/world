{...}: {
  programs.foot = {
    enable = true;
    server.enable = true;
    settings = {
      main = {
        font = "JetBrainsMono Nerd Font:size=8";
        term = "foot";
        dpi-aware = "yes";
      };
    };
  };
}
