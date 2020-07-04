{ pkgs, config, lib, options }:

{

  xdg.configFile."pulse/default.pa".text = ''
    .include /etc/pulse/default.pa
    # automatically switch to newly-connected devices
    load-module module-switch-on-connect
  '';

}
