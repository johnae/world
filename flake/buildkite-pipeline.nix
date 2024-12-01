{
  withSystem,
  lib,
  ...
}: let
  inherit (lib) mapAttrsToList filterAttrs hasPrefix filter elem;
  defaultSkip = [
    "container-"
    "container-processes"
    "container-shell"
    "devenv-up"
  ];
  matrix = withSystem "x86_64-linux" (
    ctx @ {pkgs, ...}: let
      skip =
        (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
        ++ defaultSkip;
    in
      filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) ctx.config.packages)
  );
in {
  flake = {
    buildkite-flake-updater = {
      env = {
        CACHE_NAME = "insane";
        NIX_CONFIG = "accept-flake-config = true";
      };
      steps = [
        {
          label = ":nix: Update packages";
          plugins = [
            {
              "johnae/github-app-auth#v1.0.1" = {
                installation_id = 57780546;
                app_id = 1073609;
              };
            }
          ];
          command = ''
            nix shell .#world nixpkgs#gh nixpkgs#git nixpkgs#gnugrep nixpkgs#gawk -c bash<<'BASH'
            echo "+++ Authenticated as GitHub App"
            gh auth status
            GHUSER="$(gh auth status | awk '{ if ($2 == "Logged" && $6 == "account") { print $7 }}')"
            echo "Github user: $GHUSER"

            echo "~~~ Setup git"
            git config user.name "$GHUSER"
            git config user.email '|-<>-|'

            echo "+++ Update packages"
            world gh-release-update
            nix flake update

            echo "--- Commit changes"
            if [[ -n "$(git status --porcelain)" ]]; then
              BRANCH_EXISTS=$(git ls-remote --heads origin "automatic-updates")
              git checkout main
              if [[ -z "$BRANCH_EXISTS" ]]; then
                echo "--- Branch automatic-updates does not exist. Creating it."
                git checkout -b automatic-updates
                git commit -am "chore(auto): update flake inputs"
              else
                echo "--- Branch automatic-updates exists. Updating it."
                git fetch origin automatic-updates
                git checkout -b automatic-updates
                git commit --amend --no-edit
              fi
              git push -f origin automatic-updates

              echo "--- Check if pull request exists"
              PR="$(gh pr list --head automatic-updates --json number --jq '.[0].number')"

              if [[ -z "$PR" ]]; then
                PR="$(gh pr create -a johnae -r johnae -H automatic-updates -b main -f)"
              fi
              echo "+++ Enable PR auto merge"
              gh pr merge --auto -d -s "$PR"
            else
              echo "--- No changes, no PR"
            fi
            BASH
          '';
        }
      ];
    };
    buildkite-flake-builder = {
      env = {
        CACHE_NAME = "insane";
        NIX_CONFIG = "accept-flake-config = true";
      };
      steps = [
        {
          group = ":broom: Linting and syntax checks";
          key = "checks";
          steps = [
            {
              label = ":nix: Lint";
              command = "nix run .#world -- lint";
            }
            {
              label = ":nix: Check";
              command = "nix run .#world -- check";
            }
          ];
        }
        {
          group = ":hammer_and_pick: Building packages";
          key = "packages";
          steps = [
            {
              label = ":nix: {{matrix}} build";
              command = "nix build .#packages.x86_64-linux.{{matrix}} -L";
              inherit matrix;
            }
          ];
        }
      ];
    };
  };
}
