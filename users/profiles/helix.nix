{pkgs, ...}: {
  programs.helix = {
    enable = true;
    settings = {
      theme = "nord";

      editor = {
        line-number = "relative";
        mouse = true;
        cursorline = true;
        bufferline = "multiple";
        true-color = true;

        cursor-shape = {
          insert = "bar";
          normal = "block";
          select = "underline";
        };

        file-picker = {
          hidden = false;
        };

        lsp = {
          auto-signature-help = false;
          display-messages = true;
        };

        statusline.left = ["mode" "spinner" "version-control" "file-name"];
      };
    };
    languages = {
      language = [
        {
          name = "nix";
          formatter = {command = "${pkgs.alejandra}/bin/alejandra";};
          language-server = {command = "${pkgs.nil}/bin/nil";};
          auto-format = true;
        }
      ];
    };
  };
}
