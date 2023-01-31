{inputs, ...}: {
  perSystem = {pkgs, ...}: {
    devShells.default = pkgs.devshell.mkShell {
      imports = [
        (pkgs.devshell.importTOML ../devshell.toml)
      ];
    };
  };
}
