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
        bufferline = "multiple";
        true-color = true;
        color-modes = true;
        auto-format = true;
        auto-save = true;
        whitespace.render = {
          space = "all";
          tab = "all";
        };

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

        soft-wrap = {
          enable = true;
        };
      };

      keys = {
        insert = {
          right = "apply_copilot_completion";
        };
        normal = {
          space = {
            e = ":write";
            q = ":quit";
            space = "goto_last_accessed_file";
          };
        };
      };
    };
    languages = {
      language-server = {
        copilot = {
          command = "${copilot}/bin/copilot";
          args = ["--stdio"];
        };
        rust-analyzer = {
          config.check.command = "clippy";
        };
        yaml-language-server = {
          config.yaml.format.enable = true;
          config.yaml.validation = true;
          config.yaml.schemas = {
            "https://json.schemastore.org/github-workflow.json" = ".github/{actions,workflows}/*.{yml,yaml}";
            "https://raw.githubusercontent.com/ansible-community/schemas/main/f/ansible-tasks.json" = "roles/{tasks,handlers}/*.{yml,yaml}";
            kubernetes = "kubernetes/*.{yml,yaml}";
          };
        };
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
          name = "lua";
          language-servers = ["lua-language-server" "copilot"];
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
          name = "toml";
          language-servers = ["taplo" "copilot"];
        }
        {
          name = "just";
          language-servers = ["copilot"];
        }
      ];
    };
  };
}
