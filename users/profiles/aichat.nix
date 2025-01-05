{
  config,
  pkgs,
  ...
}: let
  configDir =
    if pkgs.stdenv.isDarwin && !config.xdg.enable
    then "Library/Application Support/aichat"
    else "${config.xdg.configHome}/aichat";
  aichat = pkgs.writeShellApplication {
    name = "aichat";
    runtimeInputs = [pkgs.aichat];
    text = ''
      OPENAI_API_KEY="$(cat /run/agenix/openai-api-key)";
      CLAUDE_API_KEY="$(cat /run/agenix/anthropic-api-key)";
      ANTHROPIC_API_KEY="$(cat /run/agenix/anthropic-api-key)";
      export OPENAI_API_KEY CLAUDE_API_KEY ANTHROPIC_API_KEY
      exec aichat "$@"
    '';
  };
in {
  home.packages = [aichat];
  home.file."${configDir}/roles/coder-openai.md".text = ''
    ---
    model: openai:gpt-4o
    ---
    Provide only code without comments or explanations. Do not wrap in markdown. Just respond with the code, nothing else.
    ### INPUT:
    async sleep in js
    ### OUTPUT:
    async function timeout(ms) {
      return new Promise(resolve => setTimeout(resolve, ms));
    }
  '';
  home.file."${configDir}/roles/coder-claude.md".text = ''
    ---
    model: claude:claude-3-5-sonnet-latest
    ---
    Provide only code without comments or explanations. Do not wrap in markdown. Just respond with the code, nothing else.
    ### INPUT:
    async sleep in js
    ### OUTPUT:
    async function timeout(ms) {
      return new Promise(resolve => setTimeout(resolve, ms));
    }
  '';
  home.file."${configDir}/roles/refactor-openai.md".text = ''
    ---
    model: openai:gpt-4o
    ---
    Provide only code without comments or explanations. Take the input and refactor it to be more succinct, simpler and best-practice.
    Do not change the interface or return values. It must still work the same.

    Do not wrap in markdown. Just respond with the code, nothing else.
    ### INPUT:
    // Function to calculate the sum of all even numbers in an array
    function calculateSumOfEvens(array) {
        if (array === null || array === undefined || !Array.isArray(array)) {
            return "Invalid input";
        }

        let sum = 0;
        for (let i = 0; i < array.length; i++) {
            if (typeof array[i] === 'number') {
                if (array[i] % 2 === 0) {
                    sum = sum + array[i];
                }
            } else {
                console.log("Skipping non-numeric value: " + array[i]);
            }
        }

        return sum;
    }
    ### OUTPUT:
    /**
     * Calculates the sum of all even numbers in an array.
     * @param {Array} array - The array to process.
     * @returns {number} The sum of even numbers. Returns 0 if the array is empty or contains no valid numbers.
     */
    function calculateSumOfEvens(array) {
        if (!Array.isArray(array)) {
            throw new TypeError("Input must be an array");
        }

        return array
            .filter(item => typeof item === 'number' && item % 2 === 0)
            .reduce((sum, num) => sum + num, 0);
    }
  '';
  home.file."${configDir}/roles/refactor-claude.md".text = ''
    ---
    model: claude:claude-3-5-sonnet-latest
    ---
    Provide only code without comments or explanations. Take the input and refactor it to be more succinct, simpler and best-practice.
    Do not change the interface or return values. It must still work the same.

    Do not wrap in markdown. Just respond with the code, nothing else.
    ### INPUT:
    // Function to calculate the sum of all even numbers in an array
    function calculateSumOfEvens(array) {
        if (array === null || array === undefined || !Array.isArray(array)) {
            return "Invalid input";
        }

        let sum = 0;
        for (let i = 0; i < array.length; i++) {
            if (typeof array[i] === 'number') {
                if (array[i] % 2 === 0) {
                    sum = sum + array[i];
                }
            } else {
                console.log("Skipping non-numeric value: " + array[i]);
            }
        }

        return sum;
    }
    ### OUTPUT:
    /**
     * Calculates the sum of all even numbers in an array.
     * @param {Array} array - The array to process.
     * @returns {number} The sum of even numbers. Returns 0 if the array is empty or contains no valid numbers.
     */
    function calculateSumOfEvens(array) {
        if (!Array.isArray(array)) {
            throw new TypeError("Input must be an array");
        }

        return array
            .filter(item => typeof item === 'number' && item % 2 === 0)
            .reduce((sum, num) => sum + num, 0);
    }
  '';
  home.file."${configDir}/roles/coder-openai.md".text = ''
     ---
     model: openai:gpt-4o
     ---
     Provide only code without comments or explanations. Do not wrap in markdown. Just respond with the code, nothing else.
     ### INPUT:
     async sleep in js
     ### OUTPUT:
     async function timeout(ms) {
       return new Promise(resolve => setTimeout(resolve, ms));
    }
  '';
  home.file."${configDir}/roles/coder-claude.md".text = ''
    ---
    model: claude:claude-3-5-sonnet-latest
    ---
    Provide only code without comments or explanations. Do not wrap in markdown. Just respond with the code, nothing else.
    ### INPUT:
    async sleep in js
    ### OUTPUT:
    async function timeout(ms) {
      return new Promise(resolve => setTimeout(resolve, ms));
    }
  '';
  home.file."${configDir}/roles/explain-openai.md".text = ''
    ---
    model: openai:gpt-4o
    ---
    Explain what the given code does in a clear and concise manner. Comment the explanation in a way suitable for inclusion
    in the given code. Just respond with the comment before the code. Nothing else, do NOT wrap in markdown.
    ### INPUT:
    async function timeout(ms) {
      return new Promise(resolve => setTimeout(resolve, ms));
    }
    ### OUTPUT:
    /*
    This function returns a promise that can be awaited. When awaited it the function will sleep for at least the given number
    of milliseconds.
    */
    async function timeout(ms) {
      return new Promise(resolve => setTimeout(resolve, ms));
    }
  '';
  home.file."${configDir}/roles/explain-claude.md".text = ''
    ---
    model: claude:claude-3-5-sonnet-latest
    ---
    Explain what the given code does in a clear and concise manner. Comment the explanation in a way suitable for inclusion
    in the given code. Just respond with the comment before the code. Nothing else, do NOT wrap in markdown.
    ### INPUT:
    async function timeout(ms) {
      return new Promise(resolve => setTimeout(resolve, ms));
    }
    ### OUTPUT:
    /*
    This function returns a promise that can be awaited. When awaited it the function will sleep for at least the given number
    of milliseconds.
    */
    async function timeout(ms) {
      return new Promise(resolve => setTimeout(resolve, ms));
    }
  '';
  home.file."${configDir}/roles/refactor-openai.md".text = ''
    ---
    model: openai:gpt-4o
    ---
    Provide only code without comments or explanations. Take the input and refactor it to be more succinct, simpler and best-practice.
    Do not change the interface or return values. It must still work the same.

    Do not wrap in markdown. Just respond with the code, nothing else.
    ### INPUT:
    // Function to calculate the sum of all even numbers in an array
    function calculateSumOfEvens(array) {
        if (array === null || array === undefined || !Array.isArray(array)) {
            return "Invalid input";
        }

        let sum = 0;
        for (let i = 0; i < array.length; i++) {
            if (typeof array[i] === 'number') {
                if (array[i] % 2 === 0) {
                    sum = sum + array[i];
                }
            } else {
                console.log("Skipping non-numeric value: " + array[i]);
            }
        }

        return sum;
    }
    ### OUTPUT:
    /**
     * Calculates the sum of all even numbers in an array.
     * @param {Array} array - The array to process.
     * @returns {number} The sum of even numbers. Returns 0 if the array is empty or contains no valid numbers.
     */
    function calculateSumOfEvens(array) {
        if (!Array.isArray(array)) {
            throw new TypeError("Input must be an array");
        }

        return array
            .filter(item => typeof item === 'number' && item % 2 === 0)
            .reduce((sum, num) => sum + num, 0);
    }
  '';
  home.file."${configDir}/roles/refactor-claude.md".text = ''
    ---
    model: claude:claude-3-5-sonnet-latest
    ---
    Provide only code without comments or explanations. Take the input and refactor it to be more succinct, simpler and best-practice.
    Do not change the interface or return values. It must still work the same.

    Do not wrap in markdown. Just respond with the code, nothing else.
    ### INPUT:
    // Function to calculate the sum of all even numbers in an array
    function calculateSumOfEvens(array) {
        if (array === null || array === undefined || !Array.isArray(array)) {
            return "Invalid input";
        }

        let sum = 0;
        for (let i = 0; i < array.length; i++) {
            if (typeof array[i] === 'number') {
                if (array[i] % 2 === 0) {
                    sum = sum + array[i];
                }
            } else {
                console.log("Skipping non-numeric value: " + array[i]);
            }
        }

        return sum;
    }
    ### OUTPUT:
    /**
     * Calculates the sum of all even numbers in an array.
     * @param {Array} array - The array to process.
     * @returns {number} The sum of even numbers. Returns 0 if the array is empty or contains no valid numbers.
     */
    function calculateSumOfEvens(array) {
        if (!Array.isArray(array)) {
            throw new TypeError("Input must be an array");
        }

        return array
            .filter(item => typeof item === 'number' && item % 2 === 0)
            .reduce((sum, num) => sum + num, 0);
    }
  '';
  home.file."${configDir}/config.yaml".source = (pkgs.formats.yaml {}).generate "aichat-config.yaml" {
    model = "openai:gpt-4o";
    stream = true;
    keybindings = "vi";
    function_calling = true;
    clients = [
      {
        type = "openai-compatible";
        name = "ollama";
        api_base = "http://ollama.9000.dev:11434/v1";
        api_key = null;
      }
      {
        type = "openai";
        api_base = "https://api.openai.com/v1";
      }
      {
        type = "claude";
        api_base = "https://api.anthropic.com/v1";
      }
    ];
  };
}
