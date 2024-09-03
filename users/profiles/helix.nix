{
  pkgs,
  inputs,
  ...
}: let
  copilot = pkgs.writeShellApplication {
    name = "copilot";
    text = ''
      exec ${pkgs.nodejs}/bin/node ${inputs.copilot-vim}/dist/language-server.js "''$@"
    '';
  };
  helix-copilot = pkgs.writeShellApplication {
    name = "hx";
    runtimeInputs = [copilot];
    text = ''
      exec ${pkgs.helix-latest}/bin/hx -a "''$@"
    '';
  };
in {
  programs.helix = {
    enable = true;
    package = helix-copilot;
    settings = {
      theme = "catppuccin_frappe";

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
          #copilot-auto = true;
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
          right = "copilot_apply_completion";
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
        #copilot = {
        #  command = "${copilot}/bin/copilot";
        #  args = ["--stdio"];
        #};
        rust-analyzer = {
          config.check.command = "clippy";
        };
        nixd.command = "nixd";
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
          language-servers = ["nixd"];
          auto-format = true;
        }
        {
          name = "lua";
          formatter = {
            command = "stylua";
            args = ["-"];
          };
          auto-format = true;
        }
      ];
    };
  };
}
