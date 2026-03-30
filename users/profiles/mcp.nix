{pkgs, ...}: {
  programs.mcp = {
    enable = true;
    servers = {
      devenv = {
        command = "devenv";
        args = ["mcp"];
      };
      exa = {
        url = "https://mcp.exa.ai/mcp";
        headers = {
          EXA_API_KEY = "{env:EXA_API_KEY}";
        };
      };
      context7 = {
        url = "https://mcp.context7.com/mcp";
        headers = {
          CONTEXT7_API_KEY = "{env:CONTEXT7_API_KEY}";
        };
      };
      playwright = {
        command = "${pkgs.nodejs}/bin/npx";
        args = ["@playwright/mcp@latest" "--browser" "chromium" "--executable-path" "${pkgs.chromium}/bin/chromium"];
      };
    };
  };
}
