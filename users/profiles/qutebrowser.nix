{
  programs.qutebrowser = {
    enable = true;
    settings = {
      tabs.tabs_are_windows = true;
      qt.args = [
        "enable-native-gpu-memory-buffers"
        "enable-gpu-rasterization"
        "use-egl=desktop"
        "ignore-gpu-blocklist"
      ];
      scrolling.smooth = true;
    };
  };
}
