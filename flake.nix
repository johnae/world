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
    agenix.url = "github:ryantm/agenix";
    age-plugin-yubikey.flake = false;
    age-plugin-yubikey.url = "github:str4d/age-plugin-yubikey";
    copilot-vim.flake = false;
    copilot-vim.url = "github:github/copilot.vim";
    crane.inputs.nixpkgs.follows = "nixpkgs";
    crane.url = "github:ipetkov/crane";
    devenv.inputs.flake-compat.follows = "flake-compat";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    devenv.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    devenv.url = "github:cachix/devenv";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    fenix.url = "github:nix-community/fenix";
    flake-compat.flake = false;
    flake-compat.url = "github:edolstra/flake-compat";
    flake-utils.url = "github:numtide/flake-utils";
    helix.inputs.crane.follows = "crane";
    helix.inputs.flake-utils.follows = "flake-utils";
    helix.inputs.nixpkgs.follows = "nixpkgs";
    helix.url = "github:johnae/helix/copilot"; ## copilot support
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:johnae/home-manager/patch-1"; ## temporary
    hyprland.inputs.nixpkgs.follows = "nixpkgs";
    hyprland.url = "github:hyprwm/Hyprland";
    impermanence.url = "github:nix-community/impermanence";
    matrix-conduit.inputs.crane.follows = "crane";
    matrix-conduit.inputs.fenix.follows = "fenix";
    matrix-conduit.inputs.flake-utils.follows = "flake-utils";
    matrix-conduit.inputs.nixpkgs.follows = "nixpkgs";
    matrix-conduit.url = "gitlab:famedly/conduit";
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
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    river.url = "git+https://github.com/riverwm/river?submodules=1";
    river.flake = false;
    wezterm.flake = false;
    wezterm.url = "git+https://github.com/wez/wezterm/?rev=600652583594e9f6195a6427d1fabb09068622a7&submodules=1";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        ./flake/devenv.nix
        ./flake/github-actions.nix
        ./flake/helper-packages.nix
        ./flake/hosts.nix
        ./flake/packages.nix
        ./flake/setup.nix
      ];
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
    };
}
