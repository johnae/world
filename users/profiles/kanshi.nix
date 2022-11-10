{
  services.kanshi.enable = true;
  services.kanshi.profiles = {
    default.outputs = [
      {
        criteria = "SDC 0x4165 Unknown";
        #criteria = "eDP-1";
        mode = "3840x2400";
        position = "0,0";
        scale = 1.5;
      }
    ];
    desktop.outputs = [
      {
        criteria = "Goldstar Company Ltd LG HDR 4K 0x0000AF76";
        #criteria = "DP-2";
        mode = "3840x2160";
        position = "0,0";
        scale = 1.0;
      }
      {
        criteria = "Unknown ASUS PB27U 0x0000C167";
        #criteria = "DP-1";
        mode = "3840x2160";
        position = "3840,0";
        scale = 1.0;
      }
    ];
  };
}
