{ config, pkgs, lib, ... }:

let
  chromium = pkgs.chromium.override {
    enableVaapi = true;
    commandLineArgs = "-enable-features=UseOzonePlatform -ozone-platform=wayland";
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
      #{
      #  ## stadia enhanced
      #  id = "ldeakaihfnkjmelifgmbmjlphdfncbfg";
      #}
    ];
  };
}
