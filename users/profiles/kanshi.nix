{
  services.kanshi.enable = true;
  services.kanshi.profiles = {
    default.outputs = [
      {
        criteria = "Samsung Display Corp. 0x4165 Unknown";
        mode = "3840x2400";
        position = "0,0";
        scale = 1.5;
      }
    ];
    office.outputs = [
      {
        criteria = "LG Electronics LG UltraFine 204NTPC9C520";
        mode = "3840x2160";
        position = "0,0";
        scale = 1.2;
      }
      {
        criteria = "Samsung Display Corp. 0x4165 Unknown";
        mode = "3840x2400";
        position = "320,1801";
        scale = 1.5;
      }
    ];
    office2.outputs = [
      {
        criteria = "LG Electronics LG UltraFine 204NTHM9C521";
        mode = "3840x2160";
        position = "0,0";
        scale = 1.2;
      }
      {
        criteria = "Samsung Display Corp. 0x4165 Unknown";
        mode = "3840x2400";
        position = "320,1801";
        scale = 1.5;
      }
    ];
    desktop.outputs = [
      {
        criteria = "LG Electronics LG HDR 4K 0x0007A976";
        mode = "3840x2160";
        position = "0,0";
        scale = 1.0;
      }
      {
        criteria = "ASUSTek COMPUTER INC ASUS PB27U 0x0001C167";
        mode = "3840x2160";
        position = "3840,0";
        scale = 1.0;
      }
    ];
  };
}
