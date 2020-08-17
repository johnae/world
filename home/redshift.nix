{ pkgs, config, lib, options }:
let
  redshift-gammastep = pkgs.writeStrictShellScriptBin "redshift" ''
    exec ${pkgs.gammastep}/bin/gammastep "$@"
  '';
in
{
  services.redshift = {
    enable = true;
    package = redshift-gammastep;
    latitude = "59.3293";
    longitude = "18.0686";
    temperature = {
      day = 6500;
      night = 2700;
    };
    brightness = {
      day = "1";
      night = "1";
    };
    extraOptions = [ "-m" "wayland" ];
  };
}
