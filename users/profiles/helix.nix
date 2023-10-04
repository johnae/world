{pkgs, ...}: {
  programs.helix = {
    enable = true;
    settings = {
      theme = "nord";

      editor = {
        line-number = "relative";
        mouse = false;

        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };

        file-picker = {
          hidden = false;
        };
      };
    };
    languages = {
      language = [
        {
          name = "nix";
          formatter = {command = "${pkgs.alejandra}/bin/alejandra";};
          language-server = {command = "${pkgs.nixd}/bin/nixd";};
          auto-format = true;
        }
      ];
    };
  };
}
