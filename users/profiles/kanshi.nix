{
  services.kanshi.enable = true;
  services.kanshi.settings = [
    {
      profile.name = "default";
      profile.outputs = [
        {
          criteria = "Samsung Display Corp. 0x4165 Unknown";
          mode = "3840x2400";
          position = "0,0";
          scale = 1.5;
        }
      ];
    }

    {
      profile.name = "office";
      profile.outputs = [
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
    }

    {
      profile.name = "office2";
      profile.outputs = [
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
    }

    {
      profile.name = "desktop";
      profile.outputs = [
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
    }

    {
      profile.name = "desktop2";
      profile.outputs = [
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
    }
  ];
}
