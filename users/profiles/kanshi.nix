{
  services.kanshi.enable = true;
  services.kanshi.profiles = {
    default.outputs = [
      {
        criteria = "SDC 0x4165 Unknown";
        mode = "3840x2400";
        position = "0,0";
        scale = 1.5;
      }
    ];
    desktop.outputs = [
      {
        criteria = "LG Electronics LG HDR 4K 0x0000AF76";
        mode = "3840x2160";
        position = "0,0";
        scale = 1.0;
      }
      {
        criteria = "ASUSTek COMPUTER INC ASUS PB27U 0x0000C167";
        mode = "3840x2160";
        position = "3840,0";
        scale = 1.0;
      }
    ];
  };
}
