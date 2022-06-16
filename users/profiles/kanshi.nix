{
  services.kanshi.enable = true;
  services.kanshi.profiles = {
    default.outputs = [
      {
        #criteria = "SDC 0x4152 Unknown";
        criteria = "eDP-1";
        mode = "2880x1800";
        position = "0,0";
        scale = 1.5;
      }
    ];

    desktop.outputs = [
      {
        #criteria = "Goldstar Company Ltd LG HDR 4K 0x0000AF76";
        criteria = "DP-2";
        mode = "3840x2160";
        position = "0,0";
        scale = 1.0;
      }
      {
        #criteria = "Unknown ASUS PB27U 0x0000C167";
        criteria = "DP-1";
        mode = "3840x2160";
        position = "3840,0";
        scale = 1.0;
      }
    ];
    home.outputs = [
      {
        criteria = "Goldstar Company Ltd LG HDR 4K 0x0000AF76";
        mode = "3840x2160";
        position = "0,0";
        scale = 1.0;
      }
      {
        criteria = "Unknown ASUS PB27U 0x0000C167";
        mode = "3840x2160";
        position = "0,0";
        scale = 1.0;
      }
      {
        criteria = "Sharp Corporation 0x14CC 0x00000000";
        mode = "3840x2400";
        position = "2560,0";
        scale = 2.0;
      }
    ];
    work.outputs = [
      {
        criteria = "Dell Inc. DELL P2419H H9T0943";
        mode = "1920x1080";
        position = "0,0";
        scale = 1.0;
      }
      {
        criteria = "Sharp Corporation 0x14CC 0x00000000";
        mode = "3840x2400";
        position = "1920,0";
        scale = 2.0;
      }
      {
        criteria = "Dell Inc. DELL P2419H J9T0943";
        mode = "1920x1080";
        position = "3840,0";
        scale = 1.0;
      }
    ];
  };
}
