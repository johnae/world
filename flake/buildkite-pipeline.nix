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
  darwinSkip = [
    "agent-8be5-15c3-diskformat"
    "agent-8be5-32c4-diskformat"
    "agent-8be5-9792-diskformat"
    "agent-8be5-ac2e-diskformat"
    "agent-8be5-c91d-diskformat"
    "agent-8be5-d4a1-diskformat"
    "alnitak-diskformat"
    "antares-diskformat"
    "cygnus-diskformat"
    "eris-diskformat"
    "fluxcd-yaml"
    "hcloud-dev-diskformat"
    "hcloud-k3s-agent-diskformat"
    "hcloud-k3s-master-diskformat"
    "hcloud-k3s-master-init-diskformat"
    "hetzner-csi-driver-yaml"
    "hyperion-diskformat"
    "hyprland"
    "hyprland-unwrapped"
    "icarus-diskformat"
    "installer-diskformat"
    "juicefs-csi-driver-yaml"
    "kexec-installer-nixos-unstable-noninteractive"
    "kured-yaml"
    "master-8be5-a0a1-diskformat"
    "master-8be5-c1ce-diskformat"
    "master-8be5-f2ba-diskformat"
    "orion-diskformat"
    "persway"
    "playground-diskformat"
    "rbw-atomic-unlock"
    "scripts"
    "sirius-diskformat"
    "test-diskformat"
    "titan-diskformat"
    "victoriametrics-logs-datasource-plugin"
    "victoriametrics-logs-datasource-plugin"
    "victoriametrics-metrics-datasource-plugin"
    "xdg-desktop-portal-hyprland"
  ];
  pkgs.x86_64-linux = withSystem "x86_64-linux" (
    ctx @ {pkgs, ...}: let
      skip =
        (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
        ++ defaultSkip;
    in
      filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) ctx.config.packages)
  );
  pkgs.aarch64-darwin = withSystem "aarch64-darwin" (
    ctx @ {pkgs, ...}: let
      skip =
        (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
        ++ defaultSkip
        ++ darwinSkip;
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
          agents = [
            "queue=default-queue"
            "nixos=true"
          ];
          plugins = [
            {
              "johnae/github-app-auth#v1.0.1" = {
                installation_id = 57780546;
                app_id = 1073609;
              };
            }
          ];
          command = ''
            NIX_CONFIG="$NIX_CONFIG
            access-tokens = github.com=$GH_TOKEN"
            export NIX_CONFIG
            nix shell .#world nixpkgs#gh nixpkgs#git nixpkgs#gnugrep nixpkgs#gawk -c bash<<'BASH'
            echo "+++ Authenticated as GitHub App"
            gh auth status
            GHUSER="$(gh auth status | awk '{ if ($2 == "Logged" && $6 == "account") { print $7 }}')"
            echo "Github user: $GHUSER"

            echo "~~~ Setup git"
            git config user.name "$GHUSER"
            git config user.email '|-<>-|'

            echo "--- Updating checkout"
            git fetch origin main
            git checkout --no-track -B automatic-updates origin/main

            echo "+++ Update packages"
            world gh-release-update
            nix flake update

            echo "--- Commit changes"
            if [[ -n "$(git status --porcelain)" ]]; then
              git commit -am "chore(auto): update flake inputs"
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
      steps =
        [
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
                agents = [
                  "nixos=true" ## arguably should be made to work on darwin as well
                ];
              }
            ];
          }
        ]
        ++ (map (arch: {
            group = ":hammer_and_pick: Building packages for ${arch}";
            key = "packages-${arch}";
            steps =
              map (
                pkg: {
                  agents = [
                    "queue=default-queue"
                    "nix=true"
                    "arch=${arch}"
                  ];
                  label = ":nix: ${pkg} ${arch} build";
                  command = "nix build .#packages.${arch}.${pkg} -L";
                }
              )
              pkgs."${arch}";
          })
          (builtins.attrNames pkgs));
    };
  };
}
