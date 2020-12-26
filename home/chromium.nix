{ config, pkgs, lib, ... }:

let
  chromium = pkgs.writeStrictShellScriptBin "chromium" ''
    ${pkgs.chromium}/bin/chromium -enable-features=UseOzonePlatform -ozone-platform=wayland "$@"
  '';
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
    ];
  };
}
