{
  inputs,
  pkgs,
  ...
}: let
  noctalia = inputs.noctalia.packages.${pkgs.system}.default;
  setWallpaper = pkgs.writeShellApplication {
    name = "set-wallpaper";
    text = ''
      sleep 4
      ${noctalia}/bin/noctalia-shell ipc call wallpaper random
    '';
  };
  exec = "${setWallpaper}/bin/set-wallpaper";
in {
  services.kanshi.enable = true;
  services.kanshi.settings = [
    {
      profile = {
        inherit exec;
        name = "z16";
        outputs = [
          {
            criteria = "Samsung Display Corp. 0x4165 Unknown";
            mode = "3840x2400";
            position = "0,0";
            scale = 1.5;
          }
        ];
      };
    }

    {
      profile = {
        inherit exec;
        name = "z16-2";
        outputs = [
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
      };
    }

    {
      profile = {
        inherit exec;
        name = "framework-13";
        outputs = [
          {
            criteria = "BOE NE135A1M-NY1 Unknown";
            mode = "2880x1920";
            position = "0,0";
            scale = 1.0;
          }
        ];
      };
    }

    {
      profile = {
        inherit exec;
        name = "framework-13-2";
        outputs = [
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
      };
    }

    {
      profile = {
        inherit exec;
        name = "framework-13-3";
        outputs = [
          {
            criteria = "Dell Inc. DELL U3225QE 8278C34";
            mode = "3840x2160@119.880Hz";
            position = "0,0";
            scale = 1.0;
          }

          {
            criteria = "Samsung Electric Company Odyssey G80SD H1AK500000";
            mode = "3840x2160@119.880Hz";
            position = "3840,0";
            scale = 1.0;
          }

          {
            criteria = "BOE NE135A1M-NY1 Unknown";
            mode = "2880x1920@120.000Hz";
            position = "2905,2160";
            scale = 1.0;
          }
        ];
      };
    }

    {
      profile = {
        inherit exec;
        name = "framework-13-4";
        outputs = [
          {
            criteria = "Dell Inc. DELL U3225QE 8278C34";
            mode = "3840x2160@119.880Hz";
            position = "0,0";
            scale = 1.0;
          }

          {
            criteria = "BOE NE135A1M-NY1 Unknown";
            mode = "2880x1920@120.000Hz";
            position = "2905,2160";
            scale = 1.0;
          }
        ];
      };
    }

    {
      profile = {
        inherit exec;
        name = "framework-13-5";
        outputs = [
          {
            criteria = "Samsung Electric Company Odyssey G80SD H1AK500000";
            mode = "3840x2160@119.880Hz";
            position = "0,0";
            scale = 1.0;
          }

          {
            criteria = "BOE NE135A1M-NY1 Unknown";
            mode = "2880x1920@120.000Hz";
            position = "219,2160";
            scale = 1.0;
          }
        ];
      };
    }

    {
      profile = {
        inherit exec;
        name = "framework-13-4-lid-closed";
        outputs = [
          {
            criteria = "Dell Inc. DELL U3225QE 8278C34";
            mode = "3840x2160@119.880Hz";
            position = "0,0";
            scale = 1.0;
          }

          {
            criteria = "Samsung Electric Company Odyssey G80SD H1AK500000";
            mode = "3840x2160@119.880Hz";
            position = "3840,0";
            scale = 1.0;
          }
        ];
      };
    }

    {
      profile = {
        inherit exec;
        name = "office";
        outputs = [
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
      };
    }

    {
      profile = {
        inherit exec;
        name = "office2";
        outputs = [
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
      };
    }

    {
      profile = {
        inherit exec;
        name = "desktop";
        outputs = [
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

    {
      profile = {
        inherit exec;
        name = "desktop2";
        outputs = [
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
  ];
}
