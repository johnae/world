{pkgs, ...}: {
  home.packages = [
    pkgs.python3
  ];
  xdg.configFile."kitty/ssh.conf".text = ''
    hostname *
    interpreter python
  '';
  programs.kitty = {
    enable = true;
    theme = "Nord";
    font = {
      name = "JetBrainsMono Nerd Font";
      size = 14.0;
    };
    settings = {
      bold_font = "auto";
      italic_font = "auto";
      bold_italic_font = "auto";

      ## override theme bg
      background = "#00374e";
      tab_bar_style = "slant";

      update_check_interval = 0;
      scrollback_lines = 10000;
      enable_audio_bel = false;

      allow_remote_control = true;
      enabled_layouts = "fat:bias=75;full_size=1;mirrored=false,stack";

      cursor_blink_interval = 0;
    };
    keybindings = {
      "ctrl+shift+down" = "neighboring_window down";
      "ctrl+shift+up" = "neighboring_window up";
      "ctrl+shift+left" = "neighboring_window left";
      "ctrl+shift+right" = "neighboring_window right";
      "ctrl+tab" = "select_tab";
      "ctrl+space>right" = "next_tab";
      "ctrl+space>left" = "previous_tab";
      "ctrl+n" = "launch --cwd=current";
      "ctrl+t" = "launch --cwd=current --type=tab";
      "ctrl+shift+z" = "toggle_layout stack"; ## zoom
      "ctrl+space>q" = "close_window";
      "ctrl+space>shift+q" = "close_tab";
      "ctrl+space>g" = "launch --cwd=current --type=overlay gitui";
      #"ctrl+space>f" = "launch --cwd=current --type=overlay find-project"
    };
  };
}
