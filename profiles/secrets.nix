{hostConfig, ...}:

let
  inherit (builtins) hasAttr mapAttrs;
  secrets = if hasAttr "age" hostConfig then
    {
      age.secrets = mapAttrs (n: v:
        v // { file = ../. + "/${v.file}"; }
      ) hostConfig.age.secrets;
    }
  else {};
in

{
  imports = [ secrets ];
}
