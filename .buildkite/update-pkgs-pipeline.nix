## To generate the buildkite json, run this on the command line:
##
## nix eval .#buildkite.update-packages --json

##
{ config, pkgs, lib, inputs, pkgsToCache, ... }:
let
  util = import ./util { inherit lib config; };
  inherit (util) withBuildEnv;
  inherit (builtins) filter attrNames;
  inherit (lib) concatStringsSep filterAttrs
    isDerivation mapAttrsToList;

  inputsToUpdate = filter (n: n != "nixpkgs") (attrNames inputs);
  pkgsToUpdate = mapAttrsToList (_: v: v) (filterAttrs (n: v: isDerivation v) pkgsToCache);

  updateInputs = concatStringsSep " \n" (map
    (input:
      ''
        nix flake update --update-input "${input}"
        gitCommitUpdate "input ${input}" || echo no update
      ''
    )
    inputsToUpdate);

  updatePackages = concatStringsSep " \n" (map
    (pkg:
      ''
        if [ -d "pkgs/${pkg}" ]; then
          maybeUpdateCargoShas "${pkg}"
          gitCommitUpdate "${pkg}-cargo-sha-update" || echo no update
          maybeUpdateFixedOutputShas "${pkg}"
          gitCommitUpdate "${pkg}-fixed-output-sha-update" || echo no update
        fi
      ''
    )
    inputsToUpdate);

in
{
  steps.commands.update-packages = {
    label = "Update packages";
    command = withBuildEnv ''
      FORCE_UPDATE=''${FORCE_UPDATE:-}

      echo --- Ensuring proper git configuration
      git config user.name "$BUILDKITE_AGENT_META_DATA_HOSTNAME"
      git config user.email "$USER@$BUILDKITE_AGENT_META_DATA_HOSTNAME"

      remote=origin
      branch="$BUILDKITE_BRANCH"

      echo --- Resetting git repo
      git fetch "$remote" "$branch"
      git checkout "$branch"
      git reset --hard "$remote/$branch"

      gitCommitUpdate() {
        for change in $(git diff-index HEAD | awk '{print $NF}'); do
          git add "$change"
          if ! git diff --quiet --staged --exit-code; then
            echo Committing changes to "$1"
            git diff --staged
            git commit -m "Auto updated $1"
            return 0
          fi
        done
        return 1
      }

      maybeUpdateCargoShas() {
        if nix eval .#nixpkgs."$1".cargoSha256 > /dev/null 2>&1; then
          world update-rust-package-cargo "$1"
          gitCommitUpdate "$1 update cargo hash" || echo no update
        fi
      }

      maybeUpdateFixedOutputShas() {
        if nix eval .#nixpkgs."$1".outputHash > /dev/null 2>&1; then
          world update-fixed-output-derivation "$1"
          gitCommitUpdate "$1 update fixed output hash" || echo no update
        fi
      }

      nix flake update --update-input nixpkgs
      gitCommitUpdate nixpkgs || echo no update
      #if gitCommitUpdate nixpkgs; then
      #  FORCE_UPDATE=yes
      #fi

      world update-bin-pkg ./pkgs/k3s rancher/k3s k3s
      gitCommitUpdate k3s || echo no update

      world update-bin-pkg ./pkgs/rust-analyzer-bin rust-analyzer/rust-analyzer rust-analyzer-linux
      gitCommitUpdate rust-analyzer || echo no update

      echo --- Updating flake inputs
      ${updateInputs}

      echo --- Updating/caching packages
      ${updatePackages}

      echo --- Current revisions
      echo "local: $(git rev-parse HEAD)"
      echo "remote: $(git rev-parse "$remote/$branch")"

      LATEST="$(git rev-parse HEAD)"
      if ! git branch -r --contains "$LATEST" 2> /dev/null | grep -q "origin/master"; then
        echo --- Pushing to origin
        git push "$remote" "$branch"
      else
        echo --- Nothing to push
      fi

    '';
  };
}
