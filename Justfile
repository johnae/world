## This is also wrapped up into the command "world" which is available
## anywhere - even outside this repo. See: utils/world.nix

set shell := ["nu", "-c"]

alias help := default

default:
  @just --list -f {{justfile()}} -d {{invocation_directory()}}
  @if (echo {{invocation_directory()}} | str contains "world") { echo "\n    lint\n    check" }

# search for packages
search query:
  @nix search nixpkgs {{query}} --json | from json | transpose | flatten | select column0 version description | rename --column { column0: attribute }

# open a shell with given packages available
shell +args:
  @nix shell (echo '{{args}}' | each { |it| if ($it | str contains '#') { $it } else { $"nixpkgs#($it)" } } )

# open a shell with given packages available allowing unfree packages
shell-unfree +args:
  @with-env [NIXPKGS_ALLOW_UNFREE 1] { nix shell --impure (echo '{{args}}' | each { |it| if ($it | str contains '#') { $it } else { $"nixpkgs#($it)" } } ) }

# garbage collect the system
gc:
  @nix-collect-garbage -d

cachix +args:
  with-env [CACHIX_SIGNING_KEY (rbw get cachix)] { cachix push insane {{args}} }

# upgrade the system using given flake ref
upgrade flake="github:johnae/world":
  @rm -rf ~/.cache/nix/fetcher-cache-v1.sqlite*
  @nixos-rebuild boot --flake '{{flake}}' --use-remote-sudo -L
  @if (echo initrd kernel kernel-modules | all { |it| (readlink $"/run/booted-system/($it)") != (readlink $"/nix/var/nix/profiles/system/($it)") }) { echo "The system must be rebooted for the changes to take effect" } else { nixos-rebuild switch --flake '{{flake}}' --use-remote-sudo -L }

# build the system using given flake ref
build flake="github:johnae/world":
  @nixos-rebuild build --flake '{{flake}}' --use-remote-sudo -L

[private]
echo +args:
  @echo '{{args}}'

[private]
gh-release-update:
  ./misc/gh-release-update.nu

[private]
lint:
  @echo 'Linting...'
  @let out = (statix check . | complete); if ($out.exit_code > 0) { let span = (metadata $out).span; error make {msg: "Linting failed", label: {text: $out.stdout, span: $span}} } else { echo $out.stdout }

[private]
dead:
  @echo 'Checking for dead code...'
  @let out = (deadnix -f . | complete); if ($out.exit_code > 0) { let span = (metadata $out).span; error make {msg: "Dead code check failed", label: {text: $out.stdout, span: $span}} } else { echo $out.stdout }

[private]
dscheck:
  @echo 'Flake checker...'
  @let out = (nix run github:DeterminateSystems/flake-checker | complete); if ($out.exit_code > 0) { let span = (metadata $out).span; error make {msg: "Flake checker failed", label: {text: $out.stdout, span: $span}} } else { echo $out.stdout }

[private]
check:
  @nix flake check --impure # impure because of devenv
