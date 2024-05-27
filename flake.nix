{
  description = "Declarative Today. Utopia Tomorrow.";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://insane.cachix.org"
      "https://cachix.cachix.org"
      "https://hyprland.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "insane.cachix.org-1:cLCCoYQKkmEb/M88UIssfg2FiSDUL4PUjYj9tdo4P8o="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
    ];
  };

  inputs = {
    agenix.inputs.home-manager.follows = "nixpkgs";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.inputs.systems.follows = "systems";
    agenix.url = "github:ryantm/agenix";
    age-plugin-yubikey.flake = false;
    age-plugin-yubikey.url = "github:str4d/age-plugin-yubikey";
    cachix.url = "github:cachix/cachix";
    cachix.inputs = {
      devenv.follows = "devenv";
      flake-compat.follows = "flake-compat";
      nixpkgs.follows = "nixpkgs";
      pre-commit-hooks.follows = "pre-commit-hooks";
    };
    cloud-native-pg.flake = false;
    cloud-native-pg.url = "https://github.com/cloudnative-pg/cloudnative-pg/releases/download/v1.23.1/cnpg-1.23.1.yaml"; # gh-release-update
    copilot-vim.flake = false;
    copilot-vim.url = "github:github/copilot.vim";
    crane.inputs.nixpkgs.follows = "nixpkgs";
    crane.url = "github:ipetkov/crane";
    devenv.inputs.flake-compat.follows = "flake-compat";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    devenv.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    devenv.inputs.cachix.follows = "cachix";
    devenv.url = "github:cachix/devenv";
    disko.inputs.nixpkgs.follows = "nixpkgs";
    disko.url = "github:nix-community/disko";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    fenix.url = "github:nix-community/fenix";
    flake-compat.flake = false;
    flake-compat.url = "github:edolstra/flake-compat";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.systems.follows = "systems";
    fluxcd-install.flake = false;
    fluxcd-install.url = "https://github.com/fluxcd/flux2/releases/download/v2.3.0/install.yaml"; # gh-release-update
    helix.inputs.crane.follows = "crane";
    helix.inputs.flake-utils.follows = "flake-utils";
    helix.inputs.nixpkgs.follows = "nixpkgs";
    helix.inputs.rust-overlay.follows = "rust-overlay";
    helix.url = "github:johnae/helix/copilot"; ## copilot support
    hetzner-csi-driver.flake = false;
    hetzner-csi-driver.url = "https://raw.githubusercontent.com/hetznercloud/csi-driver/v2.6.0/deploy/kubernetes/hcloud-csi.yml"; # gh-release-update
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    impermanence.url = "github:nix-community/impermanence";
    juicefs-csi-driver.flake = false;
    juicefs-csi-driver.url = "https://raw.githubusercontent.com/juicedata/juicefs-csi-driver/v0.24.0/deploy/k8s.yaml"; # gh-release-update
    kured.flake = false;
    kured.url = "github:kubereboot/kured";
    microvm.url = "github:astro/microvm.nix";
    microvm.inputs.flake-utils.follows = "flake-utils";
    microvm.inputs.nixpkgs.follows = "nixpkgs";
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
    nix2container.inputs.flake-utils.follows = "flake-utils";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
    nix2container.url = "github:nlewo/nix2container";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    persway.inputs.crane.follows = "crane";
    persway.inputs.devenv.follows = "devenv";
    persway.inputs.fenix.follows = "fenix";
    persway.inputs.flake-parts.follows = "flake-parts";
    persway.inputs.flake-utils.follows = "flake-utils";
    persway.inputs.mk-shell-bin.follows = "mk-shell-bin";
    persway.inputs.nix2container.follows = "nix2container";
    persway.inputs.nixpkgs.follows = "nixpkgs";
    persway.url = "github:johnae/persway";
    pre-commit-hooks.inputs.flake-compat.follows = "flake-compat";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    river.url = "git+https://codeberg.org/river/river?submodules=1";
    river.flake = false;
    rust-overlay.inputs.flake-utils.follows = "flake-utils";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    rust-overlay.url = "github:oxalica/rust-overlay";
    systems.url = "github:nix-systems/default";
    zjstatus.inputs.crane.follows = "crane";
    zjstatus.inputs.flake-utils.follows = "flake-utils";
    zjstatus.inputs.nixpkgs.follows = "nixpkgs";
    zjstatus.inputs.rust-overlay.follows = "rust-overlay";
    zjstatus.url = "github:dj95/zjstatus";
    zwift.url = "github:johnae/zwift";
    zwift.inputs.crane.follows = "crane";
    zwift.inputs.devenv.follows = "devenv";
    zwift.inputs.fenix.follows = "fenix";
    zwift.inputs.flake-parts.follows = "flake-parts";
    zwift.inputs.flake-utils.follows = "flake-utils";
    zwift.inputs.mk-shell-bin.follows = "mk-shell-bin";
    zwift.inputs.nix2container.follows = "nix2container";
    zwift.inputs.nixpkgs.follows = "nixpkgs";
    zwift.inputs.rust-overlay.follows = "rust-overlay";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        ./flake/devenv.nix
        ./flake/github-actions.nix
        ./flake/helper-packages.nix
        ./flake/hosts.nix
        ./flake/kubernetes.nix
        ./flake/packages.nix
        ./flake/setup.nix
      ];
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
    };
}
