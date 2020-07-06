{
  description = "A flake for building the world";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home = {
      url = "github:johnae/home-manager/flakes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    secrets = {
      url = "git+ssh://git@github.com/johnae/nixos-metadata?ref=flakes";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## non flakes
    nixos-hardware = { url = "github:nixos/nixos-hardware"; flake = false; };
    nixpkgs-fmt = { url = "github:nix-community/nixpkgs-fmt"; flake = false; };
    netns-exec = { url = "github:johnae/netns-exec"; flake = false; };
    aml = { url = "github:any1/aml"; flake = false; };
    argo-cd = { url = "github:argoproj/argo-cd/v1.6.1"; flake = false; };
    blur = { url = "github:johnae/blur"; flake = false; };
    fire = { url = "github:johnae/fire"; flake = false; };
    fish-kubectl-completions = { url = "github:evanlucas/fish-kubectl-completions"; flake = false; };
    google-cloud-sdk-fish-completion = { url = "github:Doctusoft/google-cloud-sdk-fish-completion"; flake = false; };
    grim = { url = "github:emersion/grim"; flake = false; };
    i3status-rust = { url = "github:greshake/i3status-rust"; flake = false; };
    mako = { url = "github:emersion/mako"; flake = false; };
    neatvnc = { url = "github:any1/neatvnc"; flake = false; };
    nixpkgs-mozilla = { url = "github:mozilla/nixpkgs-mozilla"; flake = false; };
    persway = { url = "github:johnae/persway"; flake = false; };
    slurp = { url = "github:emersion/slurp"; flake = false; };
    spotifyd = { url = "github:spotifyd/spotifyd"; flake = false; };
    spotnix = { url = "github:johnae/spotnix"; flake = false; };
    sway = { url = "github:swaywm/sway"; flake = false; };
    swaybg = { url = "github:swaywm/swaybg"; flake = false; };
    swayidle = { url = "github:swaywm/swayidle"; flake = false; };
    swaylock = { url = "github:swaywm/swaylock"; flake = false; };
    wlroots = { url = "github:swaywm/wlroots"; flake = false; };
    wayvnc = { url = "github:any1/wayvnc"; flake = false; };
    wf-recorder = { url = "github:ammen99/wf-recorder"; flake = false; };
    wl-clipboard = { url = "github:bugaevc/wl-clipboard"; flake = false; };
    xdg-desktop-portal-wlr = { url = "github:emersion/xdg-desktop-portal-wlr"; flake = false; };
    buildkite = { url = "github:buildkite/agent/v3.22.1"; flake = false; };
    #rust-analyzer = {
    #  type = "file";
    #  url = "https://github.com/rust-analyzer/rust-analyzer/releases/download/2020-06-15/rust-analyzer-linux";
    #  flake = false;
    #};
  };
  outputs = { self, ... }@inputs:
    let
      userName = "john";
      system = "x86_64-linux";

      systems = [ "x86_64-linux" ];

      forAllSystems = f: inputs.nixpkgs.lib.genAttrs systems (system: f system);

      genAttrs' = values: f: builtins.listToAttrs (map f values);

      pathsToImportedAttrs = paths:
        genAttrs' paths (path: {
          name = inputs.nixpkgs.lib.removeSuffix ".nix" (builtins.baseNameOf path);
          value = import path;
        });

      hosts = inputs.nixpkgs.lib.filterAttrs
        (name: _: inputs.nixpkgs.lib.hasSuffix ".nix" name)
        (builtins.readDir ./hosts);

      nixpkgsFor = forAllSystems (system:
        (import inputs.nixpkgs {
          localSystem = { inherit system; };
          overlays = builtins.attrValues self.overlays;
          config = { allowUnfree = true; };
        }));

      pkgs = nixpkgsFor.${system};

      toNixosConfiguration = name: _:
        let n = inputs.nixpkgs.lib.removeSuffix ".nix" name;
        in inputs.nixpkgs.lib.nameValuePair n (systemConfig n (./hosts + "/${name}"));


      systemConfig = hostName: configuration:
        inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit hostName userName inputs pkgs;
          };
          modules =
            [
              { nixpkgs = { inherit pkgs; }; }
              { system.nixos.versionSuffix = inputs.nixpkgs.lib.mkForce "git.${builtins.substring 0 11 inputs.nixpkgs.rev}"; }
              inputs.nixpkgs.nixosModules.notDetected
              inputs.home.nixosModules.home-manager
              configuration
            ];
        };

      nixosConfigurations =
        inputs.nixpkgs.lib.mapAttrs'
          toNixosConfiguration
          hosts;
    in
    {

      inherit nixosConfigurations;

      packages.x86_64-linux.nixpkgs = nixpkgsFor."x86_64-linux";

      ## for easy access to overlays which we might want to build in ci for example
      overlayedPackages = (import ./overlays/pkgs.nix) pkgs pkgs;

      overlays =
        let
          overlayDir = ./overlays;
          fullPath = name: overlayDir + "/${name}";
          overlayPaths = map fullPath (builtins.attrNames (builtins.readDir overlayDir));
        in
        (pathsToImportedAttrs overlayPaths) // {
          inputs = (final: prev: { inherit inputs; });
          emacs-overlay = inputs.emacs-overlay.overlay;
          mozilla = import inputs.nixpkgs-mozilla;
        };

      devShell = forAllSystems
        (sys:
          let
            nixpkgs = nixpkgsFor.${sys};
          in
          import ./shell.nix { inherit nixpkgs; });

    };
}
