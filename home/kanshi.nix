{ pkgs, config, lib, options }:

{
  services.kanshi.enable = true;
  services.kanshi.profiles = {
    default.outputs = [
      {
        criteria = "Sharp Corporation 0x14CC 0x00000000";
        mode = "3840x2400";
        position = "0,0";
        scale = 2.0;
      }
    ];
    home.outputs = [
      {
        criteria = "Ancor Communications Inc VG248 G2LMQS056758";
        mode = "1920x1080";
        position = "0,0";
        scale = 1.0;
        transform = "270";
      }
      {
        criteria = "Sharp Corporation 0x14CC 0x00000000";
        mode = "3840x2400";
        position = "1080,0";
        scale = 2.0;
      }
      {
        criteria = "Unknown ASUS PB27U 0x0000C167";
        mode = "3840x2160";
        position = "3000,0";
        scale = 1.5;
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
