{
  pkgs,
  inputs,
  ...
}: let
  copilot = pkgs.writeShellApplication {
    name = "copilot";
    text = ''
      ${pkgs.nodejs}/bin/node ${inputs.copilot-vim}/dist/agent.js "''$@"
    '';
  };
in {
  programs.helix = {
    enable = true;
    package = pkgs.helix-latest;
    settings = {
      theme = "nord";

      editor = {
        line-number = "relative";
        mouse = true;
        #cursorline = true;
        bufferline = "multiple";
        true-color = true;
        color-modes = true;

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
          display-inlay-hints = true;
          copilot-auto = true;
        };

        statusline = {
          left = ["mode" "spinner" "version-control" "file-name"];
          right = ["file-type" "file-encoding"];
          mode.normal = "NORMAL";
          mode.insert = "INSERT";
          mode.select = "SELECT";
        };
      };

      keys = {
        insert = {
          right = "apply_copilot_completion";
        };
      };
    };
    languages = {
      language-server.copilot = {
        command = "${copilot}/bin/copilot";
        args = ["--stdio"];
      };
      language = [
        {
          name = "nix";
          formatter = {command = "alejandra";};
          language-servers = ["nil" "copilot"];
          auto-format = true;
        }
        {
          name = "rust";
          language-servers = ["rust-analyzer" "copilot"];
        }
        {
          name = "javascript";
          language-servers = ["typescript-language-server" "copilot"];
        }
        {
          name = "typescript";
          language-servers = ["typescript-language-server" "copilot"];
        }
        {
          name = "bash";
          language-servers = ["bash-language-server" "copilot"];
        }
        {
          name = "hcl";
          language-servers = ["terraform-ls" "copilot"];
        }
        {
          name = "tfvars";
          language-servers = ["terraform-ls" "copilot"];
        }
        {
          name = "go";
          language-servers = ["gopls" "copilot"];
        }
        {
          name = "nu";
          language-servers = ["copilot"];
        }
        {
          name = "css";
          language-servers = ["vscode-css-language-server" "copilot"];
        }
        {
          name = "html";
          language-servers = ["vscode-html-language-server" "copilot"];
        }
        {
          name = "nickel";
          language-servers = ["copilot"];
        }
        {
          name = "yaml";
          language-servers = ["yaml-language-server" "copilot"];
        }
        {
          name = "just";
          language-servers = ["copilot"];
        }
      ];
    };
  };
}
