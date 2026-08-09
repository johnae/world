{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) concatStringsSep literalExpression listToAttrs mkIf mkMerge mkOption types;
  chromiumModule = types.submodule ({name, ...}: {
    options = {
      enable = mkOption {
        type = types.bool;
        default = false;
        example = true;
        description = "Whether to enable ${name}.";
      };

      name = mkOption {
        type = types.str;
        default = name;
      };

      package = mkOption {
        type = types.package;
        default = pkgs.chromium;
        defaultText = literalExpression "pkgs.${name}";
        description = "The ${name} package to use.";
      };

      commandLineArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        example = ["--enable-logging=stderr" "--ignore-gpu-blocklist"];
        description = ''
          List of command-line arguments to be passed to ${name}.

          Note this option does not have any effect when using a
          custom package for `programs.${name}.package`.

          For a list of common switches, see
          [Chrome switches](https://chromium.googlesource.com/chromium/src/+/refs/heads/main/chrome/common/chrome_switches.cc).

          To search switches for other components, see
          [Chromium codesearch](https://source.chromium.org/search?q=file:switches.cc&ss=chromium%2Fchromium%2Fsrc).
        '';
      };

      extensions = mkOption {
        type = with types; let
          extensionType = submodule {
            options = {
              id = mkOption {
                type = strMatching "[a-zA-Z]{32}";
                description = ''
                  The extension's ID from the Chrome Web Store url or the unpacked crx.
                '';
                default = "";
              };

              updateUrl = mkOption {
                type = str;
                description = ''
                  URL of the extension's update manifest XML file. Linux only.
                '';
                default = "https://clients2.google.com/service/update2/crx";
              };

              crxPath = mkOption {
                type = nullOr path;
                description = ''
                  Path to the extension's crx file. Linux only.
                '';
                default = null;
              };

              version = mkOption {
                type = nullOr str;
                description = ''
                  The extension's version, required for local installation. Linux only.
                '';
                default = null;
              };
            };
          };
        in
          listOf (coercedTo str (v: {id = v;}) extensionType);
        default = [];
        example = literalExpression ''
          [
            { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock origin
            {
              id = "dcpihecpambacapedldabdbpakmachpb";
              updateUrl = "https://raw.githubusercontent.com/iamadamdev/bypass-paywalls-chrome/master/updates.xml";
            }
            {
              id = "aaaaaaaaaabbbbbbbbbbcccccccccc";
              crxPath = "/home/share/extension.crx";
              version = "1.0";
            }
          ]
        '';
        description = ''
          List of chromium extensions to install.
          To find the extension ID, check its URL on the
          [Chrome Web Store](https://chrome.google.com/webstore/category/extensions).

          To install extensions outside of the Chrome Web Store set
          `updateUrl` or `crxPath` and
          `version` as explained in the
          [Chrome
          documentation](https://developer.chrome.com/docs/extensions/mv2/external_extensions).
        '';
      };
    };
  });

  chromiumPkg = cfg: let
    inherit (cfg) name;
    configDir = "${config.xdg.configHome}/" + name;
    commandLineArgs = concatStringsSep " " (cfg.commandLineArgs ++ ["--user-data-dir=${configDir}"]);
    pkg = cfg.package.override {
      inherit commandLineArgs;
    };
  in
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = [pkg];
      text = ''
        chromium "$@"
      '';
    };

  chromiumConfig = cfg: let
    configDir = "${config.xdg.configHome}/" + cfg.name;

    extensionJson = ext:
      assert ext.crxPath != null -> ext.version != null; let
        inherit (builtins) toJSON;
      in {
        name = "${configDir}/External Extensions/${ext.id}.json";
        value.text = toJSON (
          if ext.crxPath != null
          then {
            external_crx = ext.crxPath;
            external_version = ext.version;
          }
          else {
            external_update_url = ext.updateUrl;
          }
        );
      };
  in
    mkIf cfg.enable {
      packages = [(chromiumPkg cfg)];
      file = listToAttrs (map extensionJson cfg.extensions);
    };
in {
  options.programs.chromiums = mkOption {
    type = types.attrsOf chromiumModule;
    default = {};
  };

  config.home = mkMerge (
    lib.mapAttrsToList (
      _: chromiumConfig
    )
    config.programs.chromiums
  );
}
