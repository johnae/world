{ pkgs, config, lib, options }:

{
  services.redshift = {
    enable = true;
    package = pkgs.redshift-wlr;
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
