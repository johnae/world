{
  programs.qutebrowser = {
    enable = true;
    searchEngines = {
      w = "https://en.wikipedia.org/wiki/Special:Search?search={}&go=Go&ns0=1";
      aw = "https://wiki.archlinux.org/?search={}";
      nw = "https://nixos.wiki/index.php?search={}";
      g = "https://www.google.com/search?hl=en&q={}";
    };
    settings = {
      tabs.tabs_are_windows = true;
      qt.args = [
        "enable-accelerated-video-decode"
        "enable-native-gpu-memory-buffers"
        "enable-gpu-rasterization"
        "use-egl=desktop"
        "ignore-gpu-blocklist"
      ];
      scrolling.smooth = true;
    };
  };
}
