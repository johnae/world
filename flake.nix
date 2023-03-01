{
  description = "John's Nixos Configurations, potions, packages and magical incantations";

  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://insane.cachix.org"
      "https://cachix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "insane.cachix.org-1:cLCCoYQKkmEb/M88UIssfg2FiSDUL4PUjYj9tdo4P8o="
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
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
    devshell.inputs.flake-utils.follows = "flake-utils";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devshell.url = "github:numtide/devshell";
    dream2nix.inputs.alejandra.follows = "alejandra";
    dream2nix.inputs.devshell.follows = "devshell";
    dream2nix.inputs.nixpkgs.follows = "nixpkgs";
    dream2nix.inputs.flake-parts.follows = "flake-parts";
    dream2nix.url = "github:nix-community/dream2nix";
    emacs-overlay.inputs.flake-utils.follows = "flake-utils";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    fenix.url = "github:nix-community/fenix";
    fire.flake = false;
    fire.url = "github:johnae/fire";
    fish-kubectl-completions.flake = false;
    fish-kubectl-completions.url = "github:evanlucas/fish-kubectl-completions";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    fluxcd-install.flake = false;
    fluxcd-install.url = "https://github.com/fluxcd/flux2/releases/download/v0.40.1/install.yaml"; # gh-release-update
    git-branchless.flake = false;
    git-branchless.url = "github:arxanas/git-branchless";
    google-cloud-sdk-fish-completion.flake = false;
    google-cloud-sdk-fish-completion.url = "github:Doctusoft/google-cloud-sdk-fish-completion";
    grim.flake = false;
    grim.url = "github:emersion/grim";
    headscale.url = "github:juanfont/headscale";
    headscale.inputs.flake-utils.follows = "flake-utils";
    headscale.inputs.nixpkgs.follows = "nixpkgs";
    hwdata.url = "github:vcrhonek/hwdata";
    hwdata.flake = false;
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.utils.follows = "flake-utils";
    home-manager.url = "github:nix-community/home-manager";
    kile.flake = false;
    kile.url = "gitlab:snakedye/kile";
    kured.flake = false;
    kured.url = "github:weaveworks/kured";
    matrix-conduit.flake = false;
    matrix-conduit.url = "gitlab:famedly/conduit"; ## Update when tooling allows
    neatvnc.flake = false;
    neatvnc.url = "github:any1/neatvnc";
    netns-exec.flake = false;
    netns-exec.url = "github:johnae/netns-exec";
    nixlib.url = "github:nix-community/nixpkgs.lib";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    notracking.flake = false;
    notracking.url = "github:notracking/hosts-blocklists";
    nur.url = "github:nix-community/NUR";
    nushell.flake = false;
    nushell.url = "github:nushell/nushell/0.76.0"; # gh-release-update
    persway.inputs.devshell.follows = "devshell";
    persway.inputs.dream2nix.follows = "dream2nix";
    persway.inputs.fenix.follows = "fenix";
    persway.inputs.flake-utils.follows = "flake-utils";
    persway.inputs.nixpkgs.follows = "nixpkgs";
    persway.url = "github:johnae/persway";
    ristate.flake = false;
    ristate.url = "gitlab:snakedye/ristate";
    rofi-wayland.flake = false;
    rofi-wayland.url = "github:lbonn/rofi/wayland";
    slurp.flake = false;
    slurp.url = "github:emersion/slurp";
    spotifyd.flake = false;
    spotifyd.url = "github:spotifyd/spotifyd";
    spotnix.inputs.devshell.follows = "devshell";
    spotnix.inputs.dream2nix.follows = "dream2nix";
    spotnix.inputs.fenix.follows = "fenix";
    spotnix.inputs.flake-utils.follows = "flake-utils";
    spotnix.inputs.nixpkgs.follows = "nixpkgs";
    spotnix.url = "github:johnae/spotnix";
    sway.flake = false;
    sway.url = "github:swaywm/sway";
    swaybg.flake = false;
    swaybg.url = "github:swaywm/swaybg";
    swayidle.flake = false;
    swayidle.url = "github:swaywm/swayidle";
    swaylock.flake = false;
    swaylock.url = "github:swaywm/swaylock";
    wayland-protocols-master.flake = false;
    wayland-protocols-master.url = "git+https://gitlab.freedesktop.org/wayland/wayland-protocols.git?ref=main";
    wayvnc.flake = false;
    wayvnc.url = "github:any1/wayvnc";
    wl-clipboard.flake = false;
    wl-clipboard.url = "github:bugaevc/wl-clipboard";
    wlroots.flake = false;
    wlroots.url = "git+https://gitlab.freedesktop.org/wlroots/wlroots.git?ref=master";
    wlvncc.flake = false;
    wlvncc.url = "github:any1/wlvncc";
    xdg-desktop-portal-wlr.flake = false;
    xdg-desktop-portal-wlr.url = "github:emersion/xdg-desktop-portal-wlr/v0.6.0"; # gh-release-update
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux"];
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
        inputs.dream2nix.flakeModuleBeta
        ./flake/containers.nix
        ./flake/devshell.nix
        ./flake/dream2nix-packages.nix
        ./flake/github-actions.nix
        ./flake/hosts.nix
        ./flake/kubernetes.nix
        ./flake/packages.nix
        ./flake/setup.nix
      ];
    };
}
