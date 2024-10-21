{
  programs.starship = {
    enable = true;
    enableNushellIntegration = false; ## because starship generates bad config right now
    settings = {
      kubernetes.disabled = false;
      kubernetes.style = "bold blue";
      nix_shell.disabled = false;
      rust.symbol = " ";
      gcloud = {
        format = "on [$symbol(\\($project\\))]($style) ";
      };
      custom.jjstate = {
        detect_folders = [".jj"];
        command = ''
          jj log -r@ -n1 --no-graph -T "" --stat | tail -n1 | sd "(\d+) files? changed, (\d+) insertions?\(\+\), (\d+) deletions?\(-\)" ' $${1}m $${2}+ $${3}-' | sd " 0." ""
        '';
      };
      custom.jj = {
        detect_folders = [".jj"];
        symbol = "🥋 ";
        command = ''
          jj log -r::@ -n2 --ignore-working-copy --no-graph --color always  -T '
            separate(" ",
              bookmarks.map(|x| if(
                  x.name().substr(0, 10).starts_with(x.name()),
                  x.name().substr(0, 10),
                  x.name().substr(0, 9) ++ "…")
                ).join(" "),
              tags.map(|x| if(
                  x.name().substr(0, 10).starts_with(x.name()),
                  x.name().substr(0, 10),
                  x.name().substr(0, 9) ++ "…")
                ).join(" "),
              if(
                 description.first_line().substr(0, 24).starts_with(description.first_line()),
                 description.first_line().substr(0, 24),
                 description.first_line().substr(0, 23) ++ "…"
              ),
              if(conflict, "conflict"),
              if(divergent, "divergent"),
              if(hidden, "hidden"),
            )
          ' | head -n1
        '';
      };
    };
  };
}
