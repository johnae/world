{ pkgs, config, lib, options, ... }:
{
  services.wlsunset = {
    enable = true;
    latitude = "59.3293";
    longitude = "18.0686";
    temperature = {
      day = 6500;
      night = 2700;
    };
    systemdTarget = "sway-session.target";
  };
}
