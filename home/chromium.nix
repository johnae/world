{ config, pkgs, lib, ... }:

let
  chromium = pkgs.chromium.override {
    commandLineArgs = "-enable-features=UseOzonePlatform -ozone-platform=wayland -enable-features=VaapiVideoDecoder";
  };
in
{
  programs.chromium = {
    enable = true;
    package = chromium;
    extensions = [
      {
        ## vimium
        id = "dbepggeogbaibhgnhhndojpepiihcmeb";
      }
      {
        ## stadia plus
        id = "bbhmnnecicphphjamhdefpagipoegijd";
      }
    ];
  };
}
