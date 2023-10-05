{pkgs, ...}: {
  programs.zellij = {
    enable = true;
    settings = {
      theme = "nord";
      keybinds = {
        unbind = "Ctrl b"; ## don't need tmux mode + it messes with other things
      };
    };
  };
}
