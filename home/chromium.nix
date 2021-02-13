{ config, pkgs, lib, ... }:

let
  chromiumVaapi = pkgs.chromium.override { enableVaapi = true; };
  chromium = pkgs.writeStrictShellScriptBin "chromium" ''
    ${chromiumVaapi}/bin/chromium -enable-features=UseOzonePlatform -ozone-platform=wayland "$@"
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
