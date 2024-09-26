{pkgs, ...}: let
  helix-ai = pkgs.writeShellApplication {
    name = "hx";
    runtimeInputs = [pkgs.lsp-ai];
    text = ''
      if [ -e /run/agenix/groq-lsp-ai ]; then
        GROQ_API_KEY="$(cat /run/agenix/groq-lsp-ai)"
        export GROQ_API_KEY
      fi
      if [ -e /run/agenix/anthropic-lsp-ai ]; then
        ANTHROPIC_API_KEY="$(cat /run/agenix/anthropic-lsp-ai)"
        export ANTHROPIC_API_KEY
      fi
      if [ -e /run/agenix/openrouter-lsp-ai ]; then
        OPENROUTER_API_KEY="$(cat /run/agenix/openrouter-lsp-ai)"
        export OPENROUTER_API_KEY
      fi
      exec ${pkgs.helix-latest}/bin/hx "''$@"
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
      (#not-kind-eq? @first "boolean") (#not-kind-eq? @first "character") (#not-kind-eq? @first "string") (#not-kind-eq? @first "number")) @align
    (list . (list) @anchor . (_) @second
      (#not-same-line? @anchor @second)
      (#set! "scope" "tail")
      (#not-kind-eq? @first "boolean") (#not-kind-eq? @first "character") (#not-kind-eq? @first "string") (#not-kind-eq? @first "number")
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
    package = helix-ai;
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
            t = ":open lsp-ai-chat.md";
            e = ":write";
            q = ":quit";
            space = "goto_last_accessed_file";
          };
        };
      };
    };
    languages = {
      language-server = {
        fennel-ls = {
          command = "fennel-ls";
        };
        lsp-ai = {
          command = "lsp-ai";
          config.memory.file_store = {};
          config.models.llama = {
            type = "open_ai";
            chat_endpoint = "https://api.groq.com/openai/v1/chat/completions";
            model = "llama-3.1-70b-versatile";
            auth_token_env_var_name = "GROQ_API_KEY";
          };
          config.models.llama405 = {
            type = "open_ai";
            chat_endpoint = "https://openrouter.ai/api/v1/chat/completions";
            model = "meta-llama/llama-3.1-405b-instruct";
            auth_token_env_var_name = "OPENROUTER_API_KEY";
          };
          config.models.claude = {
            type = "anthropic";
            chat_endpoint = "https://api.anthropic.com/v1/messages";
            model = "claude-3-5-sonnet-20240620";
            auth_token_env_var_name = "ANTHROPIC_API_KEY";
          };
          config.models.deepseek-local = {
            type = "ollama";
            model = "deepseek-coder-v2";
          };
          config.models.mistral-nemo-local = {
            type = "ollama";
            model = "mistral-nemo:12b-instruct-2407-q8_0";
          };
          config.models.mixtral-local = {
            type = "ollama";
            model = "mixtral";
          };
          config.chat = [
            {
              model = "claude";
              trigger = "!C";
              action_display_name = "Claude Chat";
              parameters = {
                max_context = 4096;
                max_tokens = 1024;
                system = "You are a code assistant chatbot. The user will ask you for assistance coding and you will do your best to answer succinctly and accurately";
              };
            }

            {
              model = "llama";
              trigger = "!C";
              action_display_name = "LLama Chat";
              parameters = {
                max_context = 4096;
                max_tokens = 1024;
                system = "You are a code assistant chatbot. The user will ask you for assistance coding and you will do your best to answer succinctly and accurately";
              };
            }

            {
              model = "llama405";
              trigger = "!C";
              action_display_name = "LLama405 Chat";
              parameters = {
                max_context = 4096;
                max_tokens = 1024;
                system = "You are a code assistant chatbot. The user will ask you for assistance coding and you will do your best to answer succinctly and accurately";
              };
            }

            {
              model = "deepseek-local";
              trigger = "!C";
              action_display_name = "DeepSeek Local Chat";
              parameters = {
                max_context = 4096;
                max_tokens = 1024;
                system = "You are a code assistant chatbot. The user will ask you for assistance coding and you will do your best to answer succinctly and accurately";
              };
            }

            {
              model = "mistral-nemo-local";
              trigger = "!C";
              action_display_name = "Mistral Nemo Local Chat";
              parameters = {
                max_context = 4096;
                max_tokens = 1024;
                system = "You are a code assistant chatbot. The user will ask you for assistance coding and you will do your best to answer succinctly and accurately";
              };
            }

            {
              model = "mixtral-local";
              trigger = "!C";
              action_display_name = "Mixtral Local Chat";
              parameters = {
                max_context = 4096;
                max_tokens = 1024;
                system = "You are a code assistant chatbot. The user will ask you for assistance coding and you will do your best to answer succinctly and accurately";
              };
            }
          ];
          config.actions = [
            {
              action_display_name = "Claude Complete";
              model = "claude";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                system = "You are an AI coding assistant. Your task is to complete code snippets. The user's cursor position is marked by \"<CURSOR>\". Follow these steps:\n\n1. Analyze the code context and the cursor position.\n2. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include thoughts about the cursor position, what needs to be completed, and any necessary formatting.\n3. Determine the appropriate code to complete the current thought, including finishing partial words or lines.\n4. Replace \"<CURSOR>\" with the necessary code, ensuring proper formatting and line breaks.\n5. Wrap your code solution in <answer> tags.\n\nYour response should always include both the reasoning and the answer. Pay special attention to completing partial words or lines before adding new lines of code.\n\n<examples>\n<example>\nUser input:\n--main.py--\n# A function that reads in user inpu<CURSOR>\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"inpu\" in a comment describing a function that reads user input.\n2. We need to complete the word \"input\" in the comment first.\n3. After completing the comment, we should add a new line before defining the function.\n4. The function should use Python's built-in `input()` function to read user input.\n5. We'll name the function descriptively and include a return statement.\n</reasoning>\n\n<answer>t\ndef read_user_input():\n    user_input = input(\"Enter your input: \")\n    return user_input\n</answer>\n</example>\n\n<example>\nUser input:\n--main.py--\ndef fibonacci(n):\n    if n <= 1:\n        return n\n    else:\n        re<CURSOR>\n\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"re\" in the 'else' clause of a recursive Fibonacci function.\n2. We need to complete the return statement for the recursive case.\n3. The \"re\" already present likely stands for \"return\", so we'll continue from there.\n4. The Fibonacci sequence is the sum of the two preceding numbers.\n5. We should return the sum of fibonacci(n-1) and fibonacci(n-2).\n</reasoning>\n\n<answer>turn fibonacci(n-1) + fibonacci(n-2)</answer>\n</example>\n</examples>";
                messages = [
                  {
                    role = "user";
                    content = "{CODE}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Llama Complete";
              model = "llama";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                messages = [
                  {
                    role = "system";
                    content = "You are an AI coding assistant. Your task is to complete code snippets. The user's cursor position is marked by \"<CURSOR>\". Follow these steps:\n\n1. Analyze the code context and the cursor position.\n2. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include thoughts about the cursor position, what needs to be completed, and any necessary formatting.\n3. Determine the appropriate code to complete the current thought, including finishing partial words or lines.\n4. Replace \"<CURSOR>\" with the necessary code, ensuring proper formatting and line breaks.\n5. Wrap your code solution in <answer> tags.\n\nYour response should always include both the reasoning and the answer. Pay special attention to completing partial words or lines before adding new lines of code.\n\n<examples>\n<example>\nUser input:\n--main.py--\n# A function that reads in user inpu<CURSOR>\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"inpu\" in a comment describing a function that reads user input.\n2. We need to complete the word \"input\" in the comment first.\n3. After completing the comment, we should add a new line before defining the function.\n4. The function should use Python's built-in `input()` function to read user input.\n5. We'll name the function descriptively and include a return statement.\n</reasoning>\n\n<answer>t\ndef read_user_input():\n    user_input = input(\"Enter your input: \")\n    return user_input\n</answer>\n</example>\n\n<example>\nUser input:\n--main.py--\ndef fibonacci(n):\n    if n <= 1:\n        return n\n    else:\n        re<CURSOR>\n\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"re\" in the 'else' clause of a recursive Fibonacci function.\n2. We need to complete the return statement for the recursive case.\n3. The \"re\" already present likely stands for \"return\", so we'll continue from there.\n4. The Fibonacci sequence is the sum of the two preceding numbers.\n5. We should return the sum of fibonacci(n-1) and fibonacci(n-2).\n</reasoning>\n\n<answer>turn fibonacci(n-1) + fibonacci(n-2)</answer>\n</example>\n</examples>";
                  }
                  {
                    role = "user";
                    content = "{CODE}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Llama405 Complete";
              model = "llama405";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                messages = [
                  {
                    role = "system";
                    content = "You are an AI coding assistant. Your task is to complete code snippets. The user's cursor position is marked by \"<CURSOR>\". Follow these steps:\n\n1. Analyze the code context and the cursor position.\n2. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include thoughts about the cursor position, what needs to be completed, and any necessary formatting.\n3. Determine the appropriate code to complete the current thought, including finishing partial words or lines.\n4. Replace \"<CURSOR>\" with the necessary code, ensuring proper formatting and line breaks.\n5. Wrap your code solution in <answer> tags.\n\nYour response should always include both the reasoning and the answer. Pay special attention to completing partial words or lines before adding new lines of code.\n\n<examples>\n<example>\nUser input:\n--main.py--\n# A function that reads in user inpu<CURSOR>\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"inpu\" in a comment describing a function that reads user input.\n2. We need to complete the word \"input\" in the comment first.\n3. After completing the comment, we should add a new line before defining the function.\n4. The function should use Python's built-in `input()` function to read user input.\n5. We'll name the function descriptively and include a return statement.\n</reasoning>\n\n<answer>t\ndef read_user_input():\n    user_input = input(\"Enter your input: \")\n    return user_input\n</answer>\n</example>\n\n<example>\nUser input:\n--main.py--\ndef fibonacci(n):\n    if n <= 1:\n        return n\n    else:\n        re<CURSOR>\n\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"re\" in the 'else' clause of a recursive Fibonacci function.\n2. We need to complete the return statement for the recursive case.\n3. The \"re\" already present likely stands for \"return\", so we'll continue from there.\n4. The Fibonacci sequence is the sum of the two preceding numbers.\n5. We should return the sum of fibonacci(n-1) and fibonacci(n-2).\n</reasoning>\n\n<answer>turn fibonacci(n-1) + fibonacci(n-2)</answer>\n</example>\n</examples>";
                  }
                  {
                    role = "user";
                    content = "{CODE}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "DeepSeek Local Complete";
              model = "deepseek-local";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                system = "You are an AI coding assistant. Your task is to complete code snippets. The user's cursor position is marked by \"<CURSOR>\". Follow these steps:\n\n1. Analyze the code context and the cursor position.\n2. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include thoughts about the cursor position, what needs to be completed, and any necessary formatting.\n3. Determine the appropriate code to complete the current thought, including finishing partial words or lines.\n4. Replace \"<CURSOR>\" with the necessary code, ensuring proper formatting and line breaks.\n5. Wrap your code solution in <answer> tags.\n\nYour response should always include both the reasoning and the answer. Pay special attention to completing partial words or lines before adding new lines of code.\n\n<examples>\n<example>\nUser input:\n--main.py--\n# A function that reads in user inpu<CURSOR>\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"inpu\" in a comment describing a function that reads user input.\n2. We need to complete the word \"input\" in the comment first.\n3. After completing the comment, we should add a new line before defining the function.\n4. The function should use Python's built-in `input()` function to read user input.\n5. We'll name the function descriptively and include a return statement.\n</reasoning>\n\n<answer>t\ndef read_user_input():\n    user_input = input(\"Enter your input: \")\n    return user_input\n</answer>\n</example>\n\n<example>\nUser input:\n--main.py--\ndef fibonacci(n):\n    if n <= 1:\n        return n\n    else:\n        re<CURSOR>\n\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"re\" in the 'else' clause of a recursive Fibonacci function.\n2. We need to complete the return statement for the recursive case.\n3. The \"re\" already present likely stands for \"return\", so we'll continue from there.\n4. The Fibonacci sequence is the sum of the two preceding numbers.\n5. We should return the sum of fibonacci(n-1) and fibonacci(n-2).\n</reasoning>\n\n<answer>turn fibonacci(n-1) + fibonacci(n-2)</answer>\n</example>\n</examples>";
                messages = [
                  {
                    role = "user";
                    content = "{CODE}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Mistral Nemo Local Complete";
              model = "mistral-nemo-local";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                system = "You are an AI coding assistant. Your task is to complete code snippets. The user's cursor position is marked by \"<CURSOR>\". Follow these steps:\n\n1. Analyze the code context and the cursor position.\n2. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include thoughts about the cursor position, what needs to be completed, and any necessary formatting.\n3. Determine the appropriate code to complete the current thought, including finishing partial words or lines.\n4. Replace \"<CURSOR>\" with the necessary code, ensuring proper formatting and line breaks.\n5. Wrap your code solution in <answer> tags.\n\nYour response should always include both the reasoning and the answer. Pay special attention to completing partial words or lines before adding new lines of code.\n\n<examples>\n<example>\nUser input:\n--main.py--\n# A function that reads in user inpu<CURSOR>\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"inpu\" in a comment describing a function that reads user input.\n2. We need to complete the word \"input\" in the comment first.\n3. After completing the comment, we should add a new line before defining the function.\n4. The function should use Python's built-in `input()` function to read user input.\n5. We'll name the function descriptively and include a return statement.\n</reasoning>\n\n<answer>t\ndef read_user_input():\n    user_input = input(\"Enter your input: \")\n    return user_input\n</answer>\n</example>\n\n<example>\nUser input:\n--main.py--\ndef fibonacci(n):\n    if n <= 1:\n        return n\n    else:\n        re<CURSOR>\n\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"re\" in the 'else' clause of a recursive Fibonacci function.\n2. We need to complete the return statement for the recursive case.\n3. The \"re\" already present likely stands for \"return\", so we'll continue from there.\n4. The Fibonacci sequence is the sum of the two preceding numbers.\n5. We should return the sum of fibonacci(n-1) and fibonacci(n-2).\n</reasoning>\n\n<answer>turn fibonacci(n-1) + fibonacci(n-2)</answer>\n</example>\n</examples>";
                messages = [
                  {
                    role = "user";
                    content = "{CODE}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Mixtral Local Complete";
              model = "mixtral-local";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                system = "You are an AI coding assistant. Your task is to complete code snippets. The user's cursor position is marked by \"<CURSOR>\". Follow these steps:\n\n1. Analyze the code context and the cursor position.\n2. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include thoughts about the cursor position, what needs to be completed, and any necessary formatting.\n3. Determine the appropriate code to complete the current thought, including finishing partial words or lines.\n4. Replace \"<CURSOR>\" with the necessary code, ensuring proper formatting and line breaks.\n5. Wrap your code solution in <answer> tags.\n\nYour response should always include both the reasoning and the answer. Pay special attention to completing partial words or lines before adding new lines of code.\n\n<examples>\n<example>\nUser input:\n--main.py--\n# A function that reads in user inpu<CURSOR>\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"inpu\" in a comment describing a function that reads user input.\n2. We need to complete the word \"input\" in the comment first.\n3. After completing the comment, we should add a new line before defining the function.\n4. The function should use Python's built-in `input()` function to read user input.\n5. We'll name the function descriptively and include a return statement.\n</reasoning>\n\n<answer>t\ndef read_user_input():\n    user_input = input(\"Enter your input: \")\n    return user_input\n</answer>\n</example>\n\n<example>\nUser input:\n--main.py--\ndef fibonacci(n):\n    if n <= 1:\n        return n\n    else:\n        re<CURSOR>\n\n\nResponse:\n<reasoning>\n1. The cursor is positioned after \"re\" in the 'else' clause of a recursive Fibonacci function.\n2. We need to complete the return statement for the recursive case.\n3. The \"re\" already present likely stands for \"return\", so we'll continue from there.\n4. The Fibonacci sequence is the sum of the two preceding numbers.\n5. We should return the sum of fibonacci(n-1) and fibonacci(n-2).\n</reasoning>\n\n<answer>turn fibonacci(n-1) + fibonacci(n-2)</answer>\n</example>\n</examples>";
                messages = [
                  {
                    role = "user";
                    content = "{CODE}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Mistral Nemo Local Refactor";
              model = "mistral-nemo-local";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                system = "You are an AI coding assistant specializing in code refactoring. Your task is to analyze the given code snippet and provide a refactored version. Follow these steps:\n\n1. Analyze the code context and structure.\n2. Identify areas for improvement, such as code efficiency, readability, or adherence to best practices.\n3. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include your analysis of the current code and explain your refactoring decisions.\n4. Rewrite the entire code snippet with your refactoring applied.\n5. Wrap your refactored code solution in <answer> tags.\n\nYour response should always include both the reasoning and the refactored code.\n\n<examples>\n<example>\nUser input:\ndef calculate_total(items):\n    total = 0\n    for item in items:\n        total = total + item['price'] * item['quantity']\n    return total\n\n\nResponse:\n<reasoning>\n1. The function calculates the total cost of items based on price and quantity.\n2. We can improve readability and efficiency by:\n   a. Using a more descriptive variable name for the total.\n   b. Utilizing the sum() function with a generator expression.\n   c. Using augmented assignment (+=) if we keep the for loop.\n3. We'll implement the sum() function approach for conciseness.\n4. We'll add a type hint for better code documentation.\n</reasoning>\n<answer>\nfrom typing import List, Dict\n\ndef calculate_total(items: List[Dict[str, float]]) -> float:\n    return sum(item['price'] * item['quantity'] for item in items)\n</answer>\n</example>\n\n<example>\nUser input:\ndef is_prime(n):\n    if n < 2:\n        return False\n    for i in range(2, n):\n        if n % i == 0:\n            return False\n    return True\n\n\nResponse:\n<reasoning>\n1. This function checks if a number is prime, but it's not efficient for large numbers.\n2. We can improve it by:\n   a. Adding an early return for 2, the only even prime number.\n   b. Checking only odd numbers up to the square root of n.\n   c. Using a more efficient range (start at 3, step by 2).\n3. We'll also add a type hint for better documentation.\n4. The refactored version will be more efficient for larger numbers.\n</reasoning>\n<answer>\nimport math\n\ndef is_prime(n: int) -> bool:\n    if n < 2:\n        return False\n    if n == 2:\n        return True\n    if n % 2 == 0:\n        return False\n    \n    for i in range(3, int(math.sqrt(n)) + 1, 2):\n        if n % i == 0:\n            return False\n    return True\n</answer>\n</example>\n</examples>";
                messages = [
                  {
                    role = "user";
                    content = "{SELECTED_TEXT}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Mixtral Local Refactor";
              model = "mixtral-local";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                system = "You are an AI coding assistant specializing in code refactoring. Your task is to analyze the given code snippet and provide a refactored version. Follow these steps:\n\n1. Analyze the code context and structure.\n2. Identify areas for improvement, such as code efficiency, readability, or adherence to best practices.\n3. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include your analysis of the current code and explain your refactoring decisions.\n4. Rewrite the entire code snippet with your refactoring applied.\n5. Wrap your refactored code solution in <answer> tags.\n\nYour response should always include both the reasoning and the refactored code.\n\n<examples>\n<example>\nUser input:\ndef calculate_total(items):\n    total = 0\n    for item in items:\n        total = total + item['price'] * item['quantity']\n    return total\n\n\nResponse:\n<reasoning>\n1. The function calculates the total cost of items based on price and quantity.\n2. We can improve readability and efficiency by:\n   a. Using a more descriptive variable name for the total.\n   b. Utilizing the sum() function with a generator expression.\n   c. Using augmented assignment (+=) if we keep the for loop.\n3. We'll implement the sum() function approach for conciseness.\n4. We'll add a type hint for better code documentation.\n</reasoning>\n<answer>\nfrom typing import List, Dict\n\ndef calculate_total(items: List[Dict[str, float]]) -> float:\n    return sum(item['price'] * item['quantity'] for item in items)\n</answer>\n</example>\n\n<example>\nUser input:\ndef is_prime(n):\n    if n < 2:\n        return False\n    for i in range(2, n):\n        if n % i == 0:\n            return False\n    return True\n\n\nResponse:\n<reasoning>\n1. This function checks if a number is prime, but it's not efficient for large numbers.\n2. We can improve it by:\n   a. Adding an early return for 2, the only even prime number.\n   b. Checking only odd numbers up to the square root of n.\n   c. Using a more efficient range (start at 3, step by 2).\n3. We'll also add a type hint for better documentation.\n4. The refactored version will be more efficient for larger numbers.\n</reasoning>\n<answer>\nimport math\n\ndef is_prime(n: int) -> bool:\n    if n < 2:\n        return False\n    if n == 2:\n        return True\n    if n % 2 == 0:\n        return False\n    \n    for i in range(3, int(math.sqrt(n)) + 1, 2):\n        if n % i == 0:\n            return False\n    return True\n</answer>\n</example>\n</examples>";
                messages = [
                  {
                    role = "user";
                    content = "{SELECTED_TEXT}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Claude Refactor";
              model = "claude";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                system = "You are an AI coding assistant specializing in code refactoring. Your task is to analyze the given code snippet and provide a refactored version. Follow these steps:\n\n1. Analyze the code context and structure.\n2. Identify areas for improvement, such as code efficiency, readability, or adherence to best practices.\n3. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include your analysis of the current code and explain your refactoring decisions.\n4. Rewrite the entire code snippet with your refactoring applied.\n5. Wrap your refactored code solution in <answer> tags.\n\nYour response should always include both the reasoning and the refactored code.\n\n<examples>\n<example>\nUser input:\ndef calculate_total(items):\n    total = 0\n    for item in items:\n        total = total + item['price'] * item['quantity']\n    return total\n\n\nResponse:\n<reasoning>\n1. The function calculates the total cost of items based on price and quantity.\n2. We can improve readability and efficiency by:\n   a. Using a more descriptive variable name for the total.\n   b. Utilizing the sum() function with a generator expression.\n   c. Using augmented assignment (+=) if we keep the for loop.\n3. We'll implement the sum() function approach for conciseness.\n4. We'll add a type hint for better code documentation.\n</reasoning>\n<answer>\nfrom typing import List, Dict\n\ndef calculate_total(items: List[Dict[str, float]]) -> float:\n    return sum(item['price'] * item['quantity'] for item in items)\n</answer>\n</example>\n\n<example>\nUser input:\ndef is_prime(n):\n    if n < 2:\n        return False\n    for i in range(2, n):\n        if n % i == 0:\n            return False\n    return True\n\n\nResponse:\n<reasoning>\n1. This function checks if a number is prime, but it's not efficient for large numbers.\n2. We can improve it by:\n   a. Adding an early return for 2, the only even prime number.\n   b. Checking only odd numbers up to the square root of n.\n   c. Using a more efficient range (start at 3, step by 2).\n3. We'll also add a type hint for better documentation.\n4. The refactored version will be more efficient for larger numbers.\n</reasoning>\n<answer>\nimport math\n\ndef is_prime(n: int) -> bool:\n    if n < 2:\n        return False\n    if n == 2:\n        return True\n    if n % 2 == 0:\n        return False\n    \n    for i in range(3, int(math.sqrt(n)) + 1, 2):\n        if n % i == 0:\n            return False\n    return True\n</answer>\n</example>\n</examples>";
                messages = [
                  {
                    role = "user";
                    content = "{SELECTED_TEXT}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Llama Refactor";
              model = "llama";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                messages = [
                  {
                    role = "system";
                    content = "You are an AI coding assistant specializing in code refactoring. Your task is to analyze the given code snippet and provide a refactored version. Follow these steps:\n\n1. Analyze the code context and structure.\n2. Identify areas for improvement, such as code efficiency, readability, or adherence to best practices.\n3. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include your analysis of the current code and explain your refactoring decisions.\n4. Rewrite the entire code snippet with your refactoring applied. Do not change the names of functions or other entities referenced elsewhere in the code.\n5. Wrap your refactored code solution in <answer> tags.\n\nYour response should always include both the reasoning and the refactored code.\n\n<examples>\n<example>\nUser input:\ndef calculate_total(items):\n    total = 0\n    for item in items:\n        total = total + item['price'] * item['quantity']\n    return total\n\n\nResponse:\n<reasoning>\n1. The function calculates the total cost of items based on price and quantity.\n2. We can improve readability and efficiency by:\n   a. Using a more descriptive variable name for the total.\n   b. Utilizing the sum() function with a generator expression.\n   c. Using augmented assignment (+=) if we keep the for loop.\n3. We'll implement the sum() function approach for conciseness.\n4. We'll add a type hint for better code documentation.\n</reasoning>\n<answer>\nfrom typing import List, Dict\n\ndef calculate_total(items: List[Dict[str, float]]) -> float:\n    return sum(item['price'] * item['quantity'] for item in items)\n</answer>\n</example>\n\n<example>\nUser input:\ndef is_prime(n):\n    if n < 2:\n        return False\n    for i in range(2, n):\n        if n % i == 0:\n            return False\n    return True\n\n\nResponse:\n<reasoning>\n1. This function checks if a number is prime, but it's not efficient for large numbers.\n2. We can improve it by:\n   a. Adding an early return for 2, the only even prime number.\n   b. Checking only odd numbers up to the square root of n.\n   c. Using a more efficient range (start at 3, step by 2).\n3. We'll also add a type hint for better documentation.\n4. The refactored version will be more efficient for larger numbers.\n</reasoning>\n<answer>\nimport math\n\ndef is_prime(n: int) -> bool:\n    if n < 2:\n        return False\n    if n == 2:\n        return True\n    if n % 2 == 0:\n        return False\n    \n    for i in range(3, int(math.sqrt(n)) + 1, 2):\n        if n % i == 0:\n            return False\n    return True\n</answer>\n</example>\n</examples>";
                  }
                  {
                    role = "user";
                    content = "{SELECTED_TEXT}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }

            {
              action_display_name = "Llama405 Refactor";
              model = "llama405";
              parameters = {
                max_context = 4096;
                max_tokens = 4096;
                messages = [
                  {
                    role = "system";
                    content = "You are an AI coding assistant specializing in code refactoring. Your task is to analyze the given code snippet and provide a refactored version. Follow these steps:\n\n1. Analyze the code context and structure.\n2. Identify areas for improvement, such as code efficiency, readability, or adherence to best practices.\n3. Provide your chain of thought reasoning, wrapped in <reasoning> tags. Include your analysis of the current code and explain your refactoring decisions.\n4. Rewrite the entire code snippet with your refactoring applied. Do not change the names of functions or other entities referenced elsewhere in the code.\n5. Wrap your refactored code solution in <answer> tags.\n\nYour response should always include both the reasoning and the refactored code.\n\n<examples>\n<example>\nUser input:\ndef calculate_total(items):\n    total = 0\n    for item in items:\n        total = total + item['price'] * item['quantity']\n    return total\n\n\nResponse:\n<reasoning>\n1. The function calculates the total cost of items based on price and quantity.\n2. We can improve readability and efficiency by:\n   a. Using a more descriptive variable name for the total.\n   b. Utilizing the sum() function with a generator expression.\n   c. Using augmented assignment (+=) if we keep the for loop.\n3. We'll implement the sum() function approach for conciseness.\n4. We'll add a type hint for better code documentation.\n</reasoning>\n<answer>\nfrom typing import List, Dict\n\ndef calculate_total(items: List[Dict[str, float]]) -> float:\n    return sum(item['price'] * item['quantity'] for item in items)\n</answer>\n</example>\n\n<example>\nUser input:\ndef is_prime(n):\n    if n < 2:\n        return False\n    for i in range(2, n):\n        if n % i == 0:\n            return False\n    return True\n\n\nResponse:\n<reasoning>\n1. This function checks if a number is prime, but it's not efficient for large numbers.\n2. We can improve it by:\n   a. Adding an early return for 2, the only even prime number.\n   b. Checking only odd numbers up to the square root of n.\n   c. Using a more efficient range (start at 3, step by 2).\n3. We'll also add a type hint for better documentation.\n4. The refactored version will be more efficient for larger numbers.\n</reasoning>\n<answer>\nimport math\n\ndef is_prime(n: int) -> bool:\n    if n < 2:\n        return False\n    if n == 2:\n        return True\n    if n % 2 == 0:\n        return False\n    \n    for i in range(3, int(math.sqrt(n)) + 1, 2):\n        if n % i == 0:\n            return False\n    return True\n</answer>\n</example>\n</examples>";
                  }
                  {
                    role = "user";
                    content = "{SELECTED_TEXT}";
                  }
                ];
              };
              post_process = {
                extractor = "(?s)<answer>(.*?)</answer>";
                remove_duplicate_start = true;
                remove_duplicate_end = false;
              };
            }
          ];
          # config.completion = {
          #   model = "llama";
          #   parameters = {
          #     max_tokens = 64;
          #     max_context = 1024;
          #     messages = [
          #       {
          #         role = "system";
          #         content = "Instructions:\n- You are an AI programming assistant.\n- Given a piece of code with the cursor location marked by \"<CURSOR>\", replace \"<CURSOR>\" with the correct code or comment.\n- First, think step-by-step.\n- Describe your plan for what to build in pseudocode, written out in great detail.\n- Then output the code replacing the \"<CURSOR>\"\n- Ensure that your completion fits within the language context of the provided code snippet (e.g., Python, JavaScript, Rust).\n\nRules:\n- Only respond with code or comments.\n- Only replace \"<CURSOR>\"; do not include any previously written code.\n- Never include \"<CURSOR>\" in your response\n- If the cursor is within a comment, complete the comment meaningfully.\n- Handle ambiguous cases by providing the most contextually appropriate completion.\n- Be consistent with your responses.";
          #       }
          #       {
          #         role = "user";
          #         content = "def greet(name):\n    print(f\"Hello, {<CURSOR>}\")";
          #       }
          #       {
          #         role = "assistant";
          #         content = "name";
          #       }
          #       {
          #         role = "user";
          #         content = "function sum(a, b) {\n    return a + <CURSOR>;\n}";
          #       }
          #       {
          #         role = "assistant";
          #         content = "b";
          #       }
          #       {
          #         role = "user";
          #         content = "fn multiply(a: i32, b: i32) -> i32 {\n    a * <CURSOR>\n}";
          #       }
          #       {
          #         role = "assistant";
          #         content = "b";
          #       }
          #       {
          #         role = "user";
          #         content = "# This function checks if a number is even\n<CURSOR>";
          #       }
          #       {
          #         role = "assistant";
          #         content = "def is_even(n):\n    return n % 2 == 0";
          #       }
          #       {
          #         role = "user";
          #         content = "{CODE}";
          #       }
          #     ];
          #   };
          #};
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
          language-servers = ["nixd" "lsp-ai"];
          auto-format = true;
        }
        {
          name = "markdown";
          language-servers = ["marksman" "markdown-oxide" "lsp-ai"];
        }
        {
          name = "rust";
          language-servers = ["rust-analyzer" "lsp-ai"];
        }
        {
          name = "yaml";
          language-servers = ["yaml-language-server" "lsp-ai"];
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
          language-servers = ["fennel-ls" "lsp-ai"];
          grammar = "scheme";
          auto-format = true;
        }
        {
          name = "lua";
          formatter = {
            command = "stylua";
            args = ["-"];
          };
          language-servers = ["lua-language-server" "lsp-ai"];
          auto-format = true;
        }
      ];
    };
  };
}
