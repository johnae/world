{pkgs, ...}: let
  search-replace = pkgs.writeShellApplication {
    name = "search-replace";
    text = ''
      PREVPANE="''${1:-}"
      serpl
      if [ -n "$PREVPANE" ]; then
        echo -e ":rla\r" | wezterm cli send-text --pane-id "$PREVPANE" --no-paste
      fi
    '';
  };
  global-search-replace-wezterm-helper = pkgs.writeShellApplication {
    name = "global-search-replace-wezterm-helper";
    text = ''
      wezterm cli split-pane --top --percent 80 -- ${search-replace}/bin/search-replace "$WEZTERM_PANE" > /dev/null
    '';
  };
  aichat-wezterm-helper = pkgs.writeShellApplication {
    name = "aichat-wezterm-helper";
    text = ''
      ARGS="''${*:--r coder-claude}"
      INPUT="$(mktemp /tmp/XXXXXX.input)"
      OUTPUT="$(mktemp -u /tmp/XXXXXX.output)"
      mkfifo "$OUTPUT"
      trap 'rm -f "$INPUT" "$OUTPUT"' EXIT
      cat > "$INPUT"
      PANE="$(wezterm cli split-pane --top --percent 80 -- bash -c "cat '$INPUT' | aichat $ARGS | tee '$OUTPUT'")"
      wezterm cli zoom-pane --pane-id "$PANE"
      cat "$OUTPUT"
    '';
  };
in {
  xdg.configFile."helix/runtime/queries/fennel/injections.scm".source = pkgs.writeText "fennel-injections.scm" ''
    ; inherits: scheme
  '';
  xdg.configFile."helix/runtime/queries/fennel/indents.scm".source = pkgs.writeText "fennel-indents.scm" ''
      ; Exclude literals in the first patterns, since different rules apply for them.
    ; Similarly, exclude certain keywords (detected by a regular expression).
    ; If a list has 2 elements on the first line, it is aligned to the second element.
    (list . (_) @first . (_) @anchor
      (#same-line? @first @anchor)
      (#set! "scope" "tail")
      (#not-kind-eq? @first "boolean") (#not-kind-eq? @first "character") (#not-kind-eq? @first "string") (#not-kind-eq? @first "number")
      (#not-match? @first "lambda.*|λ.*|let.*|set.*|fn.*")) @align
    ; If the first element in a list is also a list and on a line by itself, the outer list is aligned to it
    (list . (list) @anchor .
      (#set! "scope" "tail")
      (#not-kind-eq? @first "boolean") (#not-kind-eq? @first "character") (#not-kind-eq? @frst "string") (#not-kind-eq? @first "number")) @align
    (list . (list) @anchor . (_) @second
      (#not-same-line? @anchor @second)
      (#set! "scope" "tail")
      (#not-kind-eq? @first "boolean") (#not-kind-eq? @first "character") (#not-18ind-eq? @first "string") (#not-kind-eq? @first "number")
      (#not-match? @first "lambda.*|λ.*|let.*|set.*|fn.*")) @align
    ; If the first element in a list is not a list and on a line by itself, the outer list is aligned to
    ; it plus 1 additional space. This cannot currently be modelled exactly by our indent queries,
    ; but the following is equivalent, assuming that:
    ; - the indent width is 2 (the default for scheme)
    ; - There is no space between the opening parenthesis of the list and the first element
    (list . (_) @first .
      (#not-kind-eq? @first "boolean") (#not-kind-eq? @first "character") (#not-kind-eq? @first "string") (#not-kind-eq? @first "number")
      (#not-match? @first "def.*|let.*|set!")) @indent
    (list . (_) @first . (_) @second
      (#not-same-line? @first @second)
      (#not-kind-eq? @first "boolean") (#not-kind-eq? @first "character") (#not-kind-eq? @first "string") (#not-kind-eq? @first "number")
      (#not-match? @first "lambda.*|λ.*|let.*|set.*|fn.*")) @indent

    ; If the first element in a list is a literal, align the list to it
    (list . [(boolean) (character) (string) (number)] @anchor
      (#set! "scope" "tail")) @align

    ; If the first element is among a set of predefined keywords, align the list to this element
    ; plus 1 space (using the same workaround as above for now). This is a simplification since actually
    ; the second line of the list should be indented by 2 spaces more in some cases. Supporting this would
    ; be possible but require significantly more patterns.
    (list . (symbol) @first
      (#not-match? @first "lambda.*|λ.*|let.*|set.*|fn.*")) @indent
  '';
  xdg.configFile."helix/runtime/queries/fennel/highlights.scm".source = pkgs.writeText "fennel-highlights.scm" ''

    (number) @constant.numeric
    (character) @constant.character
    (boolean) @constant.builtin.boolean

    (string) @string

    (escape_sequence) @constant.character.escape

    (comment) @comment.line
    (block_comment) @comment.block
    (directive) @keyword.directive

    ; operators

    ((symbol) @operator
     (#match? @operator "^(\\+|-|\\*|/|=|>|<|>=|<=|~=|#|\\.|\\?\\.|\\.\\.|//|%|\\^)$"))

    ; keywords

    (list
      .
      ((symbol) @keyword.conditional
       (#match? @keyword.conditional "^(if|case|match|when|unless)$"
      )))

    (list
      .
      (symbol) @keyword
      (#match? @keyword
       "^(let\\*|fn|lambda|λ|case|=>|set|let|do|else|and|if|or|not=|not|lshift|rshift|band|bor|bxor|bnot|length|when|unless|case|match|assert|require|global|local|var|comment|doc|eval-compiler|lua|macros|unquote|quote|tset|values|tail\\!)$"
       ))

    (list
      .
      (symbol) @function.builtin
      (#match? @function.builtin
       "^(assert|collectgarbage|dofile|error|getmetatable|ipairs|load|loadfile|next|pairs|pcall|print|rawequal|rawget|rawlen|rawset|require|select|setmetatable|tonumber|tostring|type|warn|xpcall|module|setfenv|loadstring|unpack|require-macros|import-macros|include)$"
       ))

    ; special forms

    (list
     "["
     (symbol)+ @variable
     "]")

    (list
     .
     (symbol) @_f
     .
     (list
       (symbol) @variable)
     (#eq? @_f "lambda"))

    (list
     .
     (symbol) @_f
     .
     (list
       (list
         (symbol) @variable.parameter))
     (#match? @_f
      "^(let|let\\*|let-syntax|let-values|let\\*-values|letrec|letrec\\*|letrec-syntax)$"))

    ; quote

    (list
     .
     (symbol) @_f
     (#eq? @_f "quote")) @string.symbol

    ; library

    (list
     .
     (symbol) @_lib
     .
     (symbol) @namespace

     (#eq? @_lib "library"))

    ; procedure

    (list
      .
      (symbol) @function)

    ;; variables

    ((symbol) @variable.builtin
     (#eq? @variable.builtin "..."))

    ((symbol) @variable.builtin
     (#eq? @variable.builtin "."))

    (symbol) @variable

    ["(" ")" "[" "]" "{" "}"] @punctuation.bracket

    (quote "'") @operator
    (unquote_splicing ",@") @operator
    (unquote ",") @operator
    (quasiquote "`") @operator

  '';
  programs.helix = {
    enable = true;
    settings = {
      theme = "catppuccin_frappe";

      editor = {
        end-of-line-diagnostics = "hint";
        line-number = "relative";
        mouse = true;
        bufferline = "multiple";
        true-color = true;
        color-modes = true;
        auto-format = true;
        auto-save = {
          focus-lost = true;
          after-delay.enable = true;
          after-delay.timeout = 1000;
        };
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

        inline-diagnostics = {
          cursor-line = "error";
        };

        lsp = {
          auto-signature-help = false;
          display-messages = true;
          display-inlay-hints = true;
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
        normal = {
          space = {
            e = ":write";
            q = ":quit";
            space = "goto_last_accessed_file";
          };
          "+" = {
            s = ":pipe ${global-search-replace-wezterm-helper}/bin/global-search-replace-wezterm-helper";
            i = ":pipe ${aichat-wezterm-helper}/bin/aichat-wezterm-helper -r coder-openai";
            r = ":pipe ${aichat-wezterm-helper}/bin/aichat-wezterm-helper -r refactor-openai";
            # e = ":pipe ${aichat-wezterm-helper}/bin/aichat-wezterm-helper -r explain-openai";
            e = [":pipe-to tee /tmp/helix-tmp-explain" ":sh aichat -f /tmp/helix-tmp-explain -r explain-openai"];

            c = ":pipe ${aichat-wezterm-helper}/bin/aichat-wezterm-helper -r coder-claude";
            t = ":pipe ${aichat-wezterm-helper}/bin/aichat-wezterm-helper -r refactor-claude";
            # y = ":pipe ${aichat-wezterm-helper}/bin/aichat-wezterm-helper -r explain-claude";
            y = [":pipe-to tee /tmp/helix-tmp-explain" ":sh aichat -f /tmp/helix-tmp-explain -r explain-claude"];

            # c = ":pipe aichat -r coder-claude";
            # t = ":pipe aichat -r refactor-claude";
          };
        };
      };
    };
    languages = {
      language-server = {
        fennel-ls = {
          command = "fennel-ls";
        };
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
          name = "markdown";
          language-servers = ["marksman" "markdown-oxide"];
        }
        {
          name = "rust";
          language-servers = ["rust-analyzer"];
        }
        {
          name = "yaml";
          language-servers = ["yaml-language-server"];
        }
        {
          name = "fennel";
          scope = "source.fnl";
          injection-regex = "(fennel|fnl)";
          file-types = ["fnl"];
          shebangs = ["fennel"];
          roots = [];
          comment-token = ";";
          indent = {
            tab-width = 2;
            unit = "  ";
          };
          formatter = {
            command = "fnlfmt";
            args = ["-"];
          };
          language-servers = ["fennel-ls"];
          grammar = "scheme";
          auto-format = true;
        }
        {
          name = "lua";
          formatter = {
            command = "stylua";
            args = ["-"];
          };
          language-servers = ["lua-language-server"];
          auto-format = true;
        }
      ];
    };
  };
}
