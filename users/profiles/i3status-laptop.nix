{
  programs.i3status-rust = {
    enable = true;
    bars.default = {
      settings.theme.name = "modern";
      icons = "awesome5";
      blocks = [
        {
          block = "net";
          format = "{ssid} {signal_strength} {ip} {speed_down;K*b}/{speed_up;K*b}";
          interval = 5;
        }
        {
          block = "cpu";
          interval = 1;
        }
        {
          block = "battery";
          interval = 30;
          format = "{percentage}% {time}";
        }
        {
          block = "sound";
        }
        {
          block = "time";
          interval = 1;
          format = "%b-%d %H:%M:%S";
        }
      ];
    };
  };
}
