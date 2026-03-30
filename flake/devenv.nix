{inputs, ...}: {
  imports = [
    inputs.devenv.flakeModule
  ];
  perSystem = {
    pkgs,
    lib,
    system,
    ...
  }: let
    inherit (builtins // lib) foldl' genList replaceStrings attrNames attrValues;
    esc = "\\e";
    ansiTable =
      {
        reset = "${esc}[0m";
        bold = "${esc}[1m";
        italic = "${esc}[3m";
        underline = "${esc}[4m";
      }
      // (foldl' (x: y: x // {"${toString y}" = "${esc}[38;5;${toString y}m";}) {} (genList (x: x) 256));

    ansiEscape =
      replaceStrings
      (map (key: "{${key}}") (attrNames ansiTable))
      (attrValues ansiTable);

    agenix-rekey-cli = inputs.agenix-rekey.packages.${system}.default;
  in {
    devenv.shells = lib.mapAttrs' (file: _: {
      name = builtins.replaceStrings [".nix"] [""] file;
      value = import "${../devenv}/${file}" {inherit pkgs lib ansiEscape agenix-rekey-cli;};
    }) (builtins.readDir ../devenv);
  };
}
