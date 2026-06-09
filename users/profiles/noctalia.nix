{
  pkgs,
  inputs,
  config,
  ...
}: let
  inherit (config.home) homeDirectory;
  wallpaper = "${homeDirectory}/Sync/wallpapers/wallpapersden.com_76694-7680x4320.jpg";
in {
  # noctalia's screen recorder invokes this from PATH
  home.packages = [pkgs.gpu-screen-recorder];

  programs.noctalia = {
    enable = true;
    package = inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default;
    systemd.enable = true;
    settings = {
      bar.default = {
        background_opacity = 0.65;
        font_weight = 400;
        margin_edge = 0;
        margin_ends = 0;
        radius = 0;
        thickness = 28;
        end = [
          "caffeine"
          "media"
          "tray"
          "notifications"
          "clipboard"
          "network"
          "bluetooth"
          "volume"
          "brightness"
          "battery"
          "control-center"
          "session"
        ];
      };
      location.auto_locate = true;
      lockscreen_widgets = {
        enabled = false;
        schema_version = 2;
        widget_order = ["lockscreen-login-box@DP-1"];
        grid = {
          cell_size = 16;
          major_interval = 4;
          visible = true;
        };
        widget."lockscreen-login-box@DP-1" = {
          box_height = 0.0;
          box_width = 0.0;
          cx = 1536.0;
          cy = 1605.0;
          output = "DP-1";
          rotation = 0.0;
          type = "login_box";
        };
      };
      nightlight.enabled = true;
      shell = {
        avatar_path = "${homeDirectory}/Sync/Bilder-9000/mattiashamren_9000-ab-john-eriksson_2024-01-08_0952/9000 AB, John Eriksson/Web - JPEG small sRGB/John-Eriksson-cropped.jpg";
        niri_overview_type_to_launch_enabled = true;
        panel.transparency_mode = "glass";
        screen_corners = {
          enabled = true;
          size = 16;
        };
        shadow.direction = "down_right";
      };
      theme = {
        builtin = "Nord";
        community_palette = "Miasma";
        templates = {
          enable_builtin_templates = false;
          enable_community_templates = false;
        };
      };
      wallpaper = {
        default.path = wallpaper;
        last.path = wallpaper;
        monitors.DP-1.path = wallpaper;
      };
    };
  };
}
