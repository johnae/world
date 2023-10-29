{
  description = "John's Nixos Configurations, potions, packages and magical incantations";

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
    age-plugin-yubikey.flake = false;
    age-plugin-yubikey.url = "github:str4d/age-plugin-yubikey";
    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";
    alejandra.inputs.fenix.follows = "fenix";
    alejandra.inputs.nixpkgs.follows = "nixpkgs";
    alejandra.url = "github:kamadorueda/alejandra";
    aml.flake = false;
    aml.url = "github:any1/aml";
    blur.flake = false;
    blur.url = "github:johnae/blur";
    copilot-vim.url = "github:github/copilot.vim";
    copilot-vim.flake = false;
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    devenv.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    devenv.inputs.flake-compat.follows = "flake-compat";
    dream2nix_legacy.url = "github:nix-community/dream2nix/6819a15ccc8a1e12462d479d60a6fdb697b27041";
    dream2nix_legacy.inputs.nixpkgs.follows = "nixpkgs";
    dream2nix_legacy.inputs.flake-parts.follows = "flake-parts";
    dream2nix_legacy.inputs.pre-commit-hooks.follows = "pre-commit-hooks";
    dream2nix_legacy.inputs.flake-compat.follows = "flake-compat";
    #dream2nix.#inputs.#nixpkgs.#follows = "#nixpkgs";
    #dream2nix.#inputs.#flake-#parts.#follows = "#flake-#parts";
    #dream2nix.#inputs.#pre-#commit-#hooks.#follows = "#pre-#commit-#hooks";
    #dream2nix.#inputs.#flake-#compat.#follows = "#flake-#compat";
    #dream2nix.#url = "#github:#nix-#community/#dream2nix";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    fenix.url = "github:nix-community/fenix";
    fire.flake = false;
    fire.url = "github:johnae/fire";
    fish-kubectl-completions.flake = false;
    fish-kubectl-completions.url = "github:evanlucas/fish-kubectl-completions";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    fluxcd-install.flake = false;
    fluxcd-install.url = "https://github.com/fluxcd/flux2/releases/download/v2.1.2/install.yaml"; # gh-release-update
    git-branchless.flake = false;
    git-branchless.url = "github:arxanas/git-branchless";
    google-cloud-sdk-fish-completion.flake = false;
    google-cloud-sdk-fish-completion.url = "github:Doctusoft/google-cloud-sdk-fish-completion";
    grim.flake = false;
    grim.url = "github:emersion/grim";
    headscale.url = "github:juanfont/headscale";
    headscale.inputs.flake-utils.follows = "flake-utils";
    headscale.inputs.nixpkgs.follows = "nixpkgs";
    helix.url = "github:helix-editor/helix"; ## copilot support
    helix-copilot-patch.url = "https://patch-diff.githubusercontent.com/raw/helix-editor/helix/pull/6865.patch"; ## copilot support
    helix-copilot-patch.flake = false;
    hetzner-csi-driver.flake = false;
    hetzner-csi-driver.url = "https://raw.githubusercontent.com/hetznercloud/csi-driver/v2.5.1/deploy/kubernetes/hcloud-csi.yml"; # gh-release-update
    hwdata.url = "github:vcrhonek/hwdata";
    hwdata.flake = false;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    hyprland.url = "github:hyprwm/Hyprland/d0367d8560653eafdcce14778b2f44e6c6ddf707";
    kured.flake = false;
    kured.url = "github:weaveworks/kured";
    libdisplay-info.flake = false;
    libdisplay-info.url = "git+https://gitlab.freedesktop.org/emersion/libdisplay-info.git?ref=main";
    matrix-conduit.url = "gitlab:famedly/conduit";
    matrix-conduit.inputs.flake-utils.follows = "flake-utils";
    matrix-conduit.inputs.nixpkgs.follows = "nixpkgs";
    matrix-conduit.inputs.fenix.follows = "fenix";
    matrix-conduit.inputs.crane.follows = "crane";
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
    neatvnc.flake = false;
    neatvnc.url = "github:any1/neatvnc";
    netns-exec.flake = false;
    netns-exec.url = "github:johnae/netns-exec";
    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
    nix2container.inputs.flake-utils.follows = "flake-utils";
    nixlib.url = "github:nix-community/nixpkgs.lib";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    notracking.flake = false;
    notracking.url = "github:notracking/hosts-blocklists";
    nur.url = "github:nix-community/NUR";
    nushell.flake = false;
    nushell.url = "github:nushell/nushell/0.86.0"; # gh-release-update
    nushell-unstable.flake = false;
    nushell-unstable.url = "github:nushell/nushell";
    persway.inputs.dream2nix.follows = "dream2nix_legacy";
    persway.inputs.fenix.follows = "fenix";
    persway.inputs.flake-utils.follows = "flake-utils";
    persway.inputs.nixpkgs.follows = "nixpkgs";
    persway.url = "github:johnae/persway";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.flake-compat.follows = "flake-compat";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
    rofi-wayland.flake = false;
    rofi-wayland.url = "github:lbonn/rofi/wayland";
    slurp.flake = false;
    slurp.url = "github:emersion/slurp";
    spotifyd.flake = false;
    spotifyd.url = "github:spotifyd/spotifyd";
    wayvnc.flake = false;
    wayvnc.url = "github:any1/wayvnc";
    wl-clipboard.flake = false;
    wl-clipboard.url = "github:bugaevc/wl-clipboard";
    wlvncc.flake = false;
    wlvncc.url = "github:any1/wlvncc";
    xdg-desktop-portal-wlr.flake = false;
    xdg-desktop-portal-wlr.url = "github:emersion/xdg-desktop-portal-wlr/v0.7.0"; # gh-release-update
    ## emacs packages
    emacs-copilot.flake = false;
    emacs-copilot.url = "github:zerolfx/copilot.el";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux"];
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
        ./flake/containers.nix
        ./flake/devenv.nix
        ./flake/dream2nix-packages.nix
        ./flake/github-actions.nix
        ./flake/hosts.nix
        ./flake/kubernetes.nix
        ./flake/packages.nix
        ./flake/setup.nix
      ];
    };
}
