{ pkgs, config, lib, options }:
let
  nordTheme = pkgs.fetchFromGitHub {
    owner = "arcticicestudio";
    repo = "nord-tmux";
    rev = "b0fd5838dbd5f3cf55eefd83ac84f3f9ac076610";
    sha256 = "14xhh49izvjw4ycwq5gx4if7a0bcnvgsf3irywc3qps6jjcf5ymk";
  };
in
{
  programs.tmux = {
    enable = true;
    clock24 = true;
    keyMode = "vi";
    shortcut = "a";
    extraConfig = ''
      set-option -g default-terminal "xterm-256color"
      set-option -ga terminal-overrides ",*256col*:Tc"
      set-option -sg escape-time 20
      set-option -g mouse on
      bind Escape copy-mode
      bind -T copy-mode-vi Escape send -X cancel
      run-shell ${nordTheme}/nord.tmux
    '';
  };
}
