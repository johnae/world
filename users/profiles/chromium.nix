let
  extensions = [
    {
      ## vimium
      id = "dbepggeogbaibhgnhhndojpepiihcmeb";
    }
    {
      ## tab-less
      id = "mdndkociaebjkggmhnemegoegnbfbgoo";
    }
    {
      ## custom new tab url
      id = "mmjbdbjnoablegbkcklggeknkfcjkjia";
    }
  ];
  commandLineArgs = [
    "-enable-features=UseOzonePlatform"
    "-ozone-platform=wayland"
    "-enable-features=VaapiVideoDecoder"
    "--enable-gpu"
  ];
in {
  programs.chromiums.chromium = {
    enable = true;
    inherit extensions commandLineArgs;
  };
  xdg.desktopEntries.chromium = {
    name = "Chromium";
    genericName = "Web Browser";
    exec = "chromium %U";
    terminal = false;
    categories = ["Network" "WebBrowser"];
    mimeType = ["text/html" "text/xml"];
    actions = {
      "New-Window" = {
        exec = "chromium --new-window %u";
      };
    };
  };
  programs.chromiums.chromium-alt = {
    enable = true;
    inherit extensions commandLineArgs;
  };
  xdg.desktopEntries.chromium-alt = {
    name = "Chromium-Alt";
    genericName = "Web Browser";
    exec = "chromium-alt %U";
    terminal = false;
    categories = ["Network" "WebBrowser"];
    mimeType = ["text/html" "text/xml"];
    actions = {
      "New-Window" = {
        exec = "chromium-alt --new-window %u";
      };
    };
  };
  programs.chromiums.chromium-private = {
    enable = true;
    inherit extensions commandLineArgs;
  };
  xdg.desktopEntries.chromium-private = {
    name = "Chromium-Private";
    genericName = "Web Browser";
    exec = "chromium-private %U";
    terminal = false;
    categories = ["Network" "WebBrowser"];
    mimeType = ["text/html" "text/xml"];
    actions = {
      "New-Window" = {
        exec = "chromium-private --new-window %u";
      };
    };
  };
  programs.chromiums.chromium-work = {
    enable = true;
    inherit extensions commandLineArgs;
  };
  xdg.desktopEntries.chromium-work = {
    name = "Chromium-Work";
    genericName = "Web Browser";
    exec = "chromium-work %U";
    terminal = false;
    categories = ["Network" "WebBrowser"];
    mimeType = ["text/html" "text/xml"];
    actions = {
      "New-Window" = {
        exec = "chromium-work --new-window %u";
      };
    };
  };
}
