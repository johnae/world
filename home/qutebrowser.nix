{ config, pkgs, lib, ... }:

{
  programs.qutebrowser = {
    enable = true;
    package = pkgs.qutebrowser-pipewire;
    settings = {
      tabs.tabs_are_windows = true;
      qt.args = [
        "enable-native-gpu-memory-buffers"
        "enable-gpu-rasterization"
        "use-egl=desktop"
        "ignore-gpu-blocklist"
        "enable-features=WebRTCPipeWireCapturer"
      ];
      scrolling.smooth = true;
    };
  };
}
