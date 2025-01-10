{
  services.kanshi.enable = true;
  services.kanshi.settings = [
    {
      profile.name = "z16";
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
      profile.name = "z16-2";
      profile.outputs = [
        {
          criteria = "Samsung Display Corp. 0x4165 Unknown";
          mode = "3840x2400";
          position = "0,0";
          scale = 1.5;
        }

        {
          criteria = "ViewSonic Corporation VX1655-OLED XBA235200337";
          mode = "3840x2160";
          position = "2560,0";
          scale = 1.5;
        }
      ];
    }

    {
      profile.name = "framework-13";
      profile.outputs = [
        {
          criteria = "BOE NE135A1M-NY1 Unknown";
          mode = "2880x1920";
          position = "0,0";
          scale = 1.0;
        }
      ];
    }

    {
      profile.name = "framework-13-2";
      profile.outputs = [
        {
          criteria = "BOE NE135A1M-NY1 Unknown";
          mode = "2880x1920";
          position = "0,0";
          scale = 1.0;
        }

        {
          criteria = "ViewSonic Corporation VX1655-OLED XBA235200337";
          mode = "3840x2160";
          position = "2256,0";
          scale = 1.5;
        }
      ];
    }

    {
      profile.name = "framework-13-3";
      profile.outputs = [
        {
          criteria = "LG Electronics LG HDR 4K 0x0007A976";
          mode = "3840x2160@59.997Hz";
          position = "0,0";
          scale = 1.0;
        }

        {
          criteria = "Samsung Electric Company Odyssey G80SD H1AK500000";
          mode = "3840x2160@120.000Hz";
          position = "3840,0";
          scale = 1.0;
        }

        {
          criteria = "BOE NE135A1M-NY1 Unknown";
          mode = "2880x1920@120.000Hz";
          position = "7680,0";
          scale = 1.0;
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
