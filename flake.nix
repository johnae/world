{
  description = "A flake for building the world";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    #nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/a31736120c5de6e632f5a0ba1ed34e53fc1c1b00";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    secrets = {
      url = "git+ssh://git@github.com/johnae/secret-world";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixkite = {
      url = "github:johnae/nixkite/flakes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-misc = {
      url = "github:johnae/nix-misc";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    spook = {
      url = "github:johnae/spook";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-misc.follows = "nix-misc";
    };
    spotnix = {
      url = "github:johnae/spotnix/flakes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home = {
      url = "github:rycee/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs-firefox-pipewire = {
      url = "github:colemickens/nixpkgs/nixpkgs-firefox-pipewire";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## non flakes
    nixos-hardware = { url = "github:nixos/nixos-hardware"; flake = false; };
    nixpkgs-fmt = { url = "github:nix-community/nixpkgs-fmt"; flake = false; };
    netns-exec = { url = "github:johnae/netns-exec"; flake = false; };
    aml = { url = "github:any1/aml"; flake = false; };
    argo-cd = { url = "github:argoproj/argo-cd/v1.7.2"; flake = false; };
    blur = { url = "github:johnae/blur"; flake = false; };
    fire = { url = "github:johnae/fire"; flake = false; };
    fish-kubectl-completions = { url = "github:evanlucas/fish-kubectl-completions"; flake = false; };
    google-cloud-sdk-fish-completion = { url = "github:Doctusoft/google-cloud-sdk-fish-completion"; flake = false; };
    grim = { url = "github:emersion/grim"; flake = false; };
    mako = { url = "github:emersion/mako"; flake = false; };
    neatvnc = { url = "github:any1/neatvnc"; flake = false; };
    persway = { url = "github:johnae/persway"; flake = false; };
    slurp = { url = "github:emersion/slurp"; flake = false; };
    spotifyd = { url = "github:spotifyd/spotifyd"; flake = false; };
    sway = { url = "github:swaywm/sway"; flake = false; };
    swaybg = { url = "github:swaywm/swaybg"; flake = false; };
    swayidle = { url = "github:swaywm/swayidle"; flake = false; };
    swaylock = { url = "github:swaywm/swaylock"; flake = false; };
    wlroots = { url = "github:swaywm/wlroots"; flake = false; };
    wayvnc = { url = "github:any1/wayvnc"; flake = false; };
    wf-recorder = { url = "github:ammen99/wf-recorder"; flake = false; };
    wl-clipboard = { url = "github:bugaevc/wl-clipboard"; flake = false; };
    xdg-desktop-portal-wlr = { url = "github:emersion/xdg-desktop-portal-wlr/v0.1.0"; flake = false; };
    buildkite = { url = "github:buildkite/agent/v3.22.1"; flake = false; };
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
          config.allowUnfree = true;
        }));

      pkgs = nixpkgsFor.${system};

      maybeTest = n:
        let test = builtins.match "(.*)-(test)$" n; # a match == [ "hostname" "test" ];
        in if test != null then builtins.head test else n;

      toNixosConfiguration = name: _:
        let n = inputs.nixpkgs.lib.removeSuffix ".nix" name;
        in inputs.nixpkgs.lib.nameValuePair n (systemConfig (maybeTest n) (./hosts + "/${name}"));

      toInstallerConfiguration = name: conf:
        inputs.nixpkgs.lib.nameValuePair name (installerConfig (maybeTest name) conf.config.system.build.toplevel);

      systemConfig = hostName: configuration:
        inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit hostName userName inputs pkgs;
          };
          modules =
            [
              { system.configurationRevision = inputs.nixpkgs.lib.mkIf (self ? rev) self.rev; }
              { nixpkgs = { inherit pkgs; }; }
              { system.nixos.versionSuffix = inputs.nixpkgs.lib.mkForce "git.${builtins.substring 0 11 inputs.nixpkgs.rev}"; }
              inputs.nixpkgs.nixosModules.notDetected
              inputs.home.nixosModules.home-manager
              configuration
            ];
        };

      installerConfig = hostName: systemClosure:
        inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            inherit hostName inputs pkgs system systemClosure;
          };
          modules =
            [
              { nixpkgs = { inherit pkgs; }; }
              ./bootstrap/installer.nix
            ];
        };

      nixosConfigurations =
        inputs.nixpkgs.lib.mapAttrs'
          toNixosConfiguration
          hosts;

      isoConfigurations =
        inputs.nixpkgs.lib.mapAttrs'
          toInstallerConfiguration
          self.nixosConfigurations;
    in
    {

      inherit nixosConfigurations;
      inherit isoConfigurations;

      ## for easy access to packages which we might want to build and cache in ci
      pkgsToCache = pkgs.lib.filterAttrs
        (_: pkgs.lib.isDerivation)
        (
          (import ./overlays/pkgs.nix) pkgs pkgs
        ) // {
        inherit (pkgs)
          spook
          firefox-pipewire
          spotnix;
      };

      overlays =
        let
          overlayDir = ./overlays;
          fullPath = name: overlayDir + "/${name}";
          overlayPaths = map fullPath (builtins.attrNames (builtins.readDir overlayDir));
        in
        (pathsToImportedAttrs overlayPaths) // {
          inputs = (final: prev: { inherit inputs; });
          emacs-overlay = inputs.emacs-overlay.overlay;
          nix-misc = inputs.nix-misc.overlay;
          spook = inputs.spook.overlay;
          spotnix = inputs.spotnix.overlay;
          firefox-pipewire = (final: prev: {
            firefox-pipewire = (import inputs.nixpkgs-firefox-pipewire {
              localSystem = { inherit system; };
              config.allowUnfree = true;
            }).firefox;
          });
        };

      containers =
        let
          containerDir = ./containers;
          fullPath = name: containerDir + "/${name}";
          containerPaths = map fullPath (builtins.attrNames (
            pkgs.lib.filterAttrs (_: t: t == "directory") (builtins.readDir containerDir)
          ));
        in
        pkgs.recurseIntoAttrs (genAttrs' containerPaths (path: {
          name = builtins.baseNameOf path;
          value =
            let
              archive = pkgs.callPackage path { };
            in
            {
              inherit archive;
              push = pkgs.pushDockerArchive { image = archive; };
            };
        }));

      devShell = forAllSystems
        (sys:
          let
            nixpkgs = nixpkgsFor.${sys};
          in
          import ./shell.nix { inherit nixpkgs; });

      buildkite =
        let
          pipelineDir = ./.buildkite;
          fullPath = name: pipelineDir + "/${name}";
          pipelinePaths = map fullPath (builtins.attrNames (builtins.readDir pipelineDir));
        in
        genAttrs' pipelinePaths (path: {
          name = inputs.nixpkgs.lib.removeSuffix ".nix" (builtins.baseNameOf path);
          value = import "${inputs.nixkite}" {
            inherit pkgs;
            pipeline = path;
            specialArgs = { inherit (self) containers pkgsToCache nixosConfigurations inputs; };
          };
        });

    };
}
