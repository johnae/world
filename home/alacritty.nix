{ pkgs, config, lib, options }:

rec {
  programs.alacritty = {
    enable = true;
    settings = rec {
      env = {
        TERM = "alacritty-direct";
      };
      window = {
        dimensions.columns = 80;
        dimensions.lines = 24;
        padding.x = 2;
        padding.y = 2;
      };
      draw_bold_text_with_bright_colors = true;
      scrolling = {
        history = 20000;
        multiplier = 20;
      };
      font = {
        normal.family = "JetBrainsMono Nerd Font";
        size = 14.0;
        offset.x = 0;
        offset.y = 0;
        glyph_offset.x = 0;
        glyph_offset.y = 0;
        use_thin_strokes = true; ## osx only but won't have a negative effect
      };
      background_opacity = 0.95;
      mouse.hide_cursor_when_typing = true;
    };
  };

  xdg.configFile."alacritty/alacritty-launcher.yml" = {
    text =
      lib.replaceStrings [ "\\\\" ] [ "\\" ] (
        builtins.toJSON
          (
            programs.alacritty.settings // {
              font.size = 28.0;
              font.normal.family = "Roboto Mono";
              colors = {
                primary.background = "0x00374e";
                primary.foreground = "0xD8DEE9";
              };
            }
          )
      );
  };

}
