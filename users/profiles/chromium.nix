{pkgs, ...}: let
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
  programs.chromiums.chromium-alt = {
    enable = true;
    inherit extensions commandLineArgs;
  };
  programs.chromiums.chromium = {
    enable = true;
    inherit extensions commandLineArgs;
  };
}
