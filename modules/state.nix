{ config, pkgs, lib, ... }:

## example
#  environment.state."/keep" = {
#    directories = [
#      "/var/lib/bluetooth"
#      "/var/lib/iwd"
#      "/var/lib/wireguard"
#      "/var/lib/systemd/coredump"
#      "/root"
#    ];
#    files = [
#      "/etc/machine-id"
#    ];
#    users.john.directories = [
#      "/home/john/Downloads"
#      "/home/john/Documents"
#      "/home/john/Development"
#      "/home/john/Photos"
#      "/home/john/Sync"
#      "/home/john/.gnupg/private-keys-v1.d"
#      "/home/john/.local/share/direnv"
#      "/home/john/.local/share/password-store"
#      "/home/john/.mail"
#      "/home/john/.cargo"
#      "/home/john/.cache/mu"
#    ];
#    users.john.files = [
#      "/home/john/.gnupg/pubring.kbx"
#      "/home/john/.gnupg/sshcontrol"
#      "/home/john/.gnupg/trustdb.gpg"
#      "/home/john/.gnupg/random_seed"
#      "/home/john/.kube/config"
#      "/home/john/.ssh/known_hosts"
#      "/home/john/.spotify_token_cache.json"
#      "/home/john/.local/share/fish/fish_history"
#    ];
#  };
let
  inherit (lib.types) listOf attrsOf str int submodule;
  inherit (lib) mkOption stringAfter flatten
    nameValuePair concatMapStrings mapAttrsToList;
  inherit (builtins) replaceStrings listToAttrs attrNames toString;

  cfg = config.environment.state;
  stateStoragePaths = attrNames cfg;
  users = config.users.users;
  bindMounts =
    let
      eachStateRoot = stateRoot:
        let
          sysDirs = cfg.${stateRoot}.directories;
          userDirs = (flatten (map (u: u.directories) (mapAttrsToList (_: v: v) cfg.${stateRoot}.users)));
        in
        (map
          (where: {
            what = "${stateRoot}/${where}"; inherit where;
          })
          (sysDirs ++ userDirs));
    in
    flatten (map eachStateRoot stateStoragePaths);

  stateOptions = { user ? "root", group ? "root" }: {
    user = mkOption {
      type = str;
      default = user;
    };
    group = mkOption {
      type = str;
      default = group;
    };
    directories = mkOption {
      type = listOf str;
      default = [ ];
    };
    files = mkOption {
      type = listOf str;
      default = [ ];
    };
  };

  stateModule = submodule
    {
      options = {
        inherit (stateOptions { }) user group directories files;
        name = mkOption {
          type = str;
          default = "root";
        };
        users = mkOption
          {
            type = attrsOf
              (submodule ({ config, name, ... }:
                let
                  user = name;
                  group = "users";
                in
                {
                  options = {
                    inherit (stateOptions { inherit user group; }) user group directories files;
                    name = mkOption {
                      type = str;
                      default = name;
                    };
                  };
                }));
            default = { };
          };
      };
    };
in
{
  options = {
    environment.state = mkOption {
      default = { };
      type = attrsOf stateModule;
    };
  };

  config = {
    system.activationScripts =
      (
        let
          mkLinksToStateStorage = { mode ? null, user ? "root", group ? "root" }: stateRoot: target:
            let
              actualMode =
                if mode == null then
                  if user == "root" then
                    755
                  else
                    775
                else
                  mode;
            in
            ''
              (
                stateRoot="${stateRoot}"
                target="${target}"
                stateRoot="''${stateRoot%/}"
                target="''${target%/}"

                stateTarget="$stateRoot$target"
                previousPath="/"

                echo "Linking '$stateTarget' to '$target'"
                for pathPart in $(echo "$target" | tr "/" " "); do
                  currentTargetPath="$previousPath$pathPart/"
                  currentSourcePath="$stateRoot$currentTargetPath"

                  if [ ! -e "$currentSourcePath" ]; then
                    if [ "$currentSourcePath" != "$stateTarget/" ]; then
                      echo Creating source directory "'$currentSourcePath'"
                      mkdir "$currentSourcePath"
                      chown ${user}:${group} "$currentSourcePath"
                      chmod ${toString actualMode} "$currentSourcePath"
                    fi
                  fi

                  if [ ! -e "$currentTargetPath" ]; then
                    if [ "$currentTargetPath" != "$target/" ]; then
                      echo Creating target directory "'$currentTargetPath'"
                      mkdir "$currentTargetPath"
                      currentRealSourcePath="$(realpath "$currentSourcePath")"
                      chown --reference="$currentRealSourcePath" "$currentTargetPath"
                      chmod --reference="$currentRealSourcePath" "$currentTargetPath"
                    else
                      echo Linking "'$target'" to "'$stateTarget'"
                      ln -f -s "$stateTarget" "$target"
                    fi
                  fi

                  previousPath="$currentTargetPath"
                done
              )
            '';
          mkStateSourceTargetDir = { mode ? null, user ? "root", group ? "root" }: stateRoot: target:
            let
              actualMode =
                if mode == null then
                  if user == "root" then
                    755
                  else
                    775
                else
                  mode;
            in
            ''
              (
                stateRoot="${stateRoot}"
                target="${target}"

                target="''${target%/}"

                stateTarget="$stateRoot$target"
                if [ ! -d "$stateTarget" ]; then
                   printf "\e[1;31mBind source '%s' does not exist; it will be created for you.\e[0m\n" "$stateTarget"
                fi
                previousPath="/"

                echo "Creating '$stateTarget' and '$target'"
                for pathPart in $(echo "$target" | tr "/" " "); do
                  currentTargetPath="$previousPath$pathPart/"
                  currentSourcePath="$stateRoot$currentTargetPath"

                  if [ ! -d "$currentSourcePath" ]; then
                    mkdir "$currentSourcePath"
                    chown ${toString user}:${group} "$currentSourcePath"
                    chmod ${toString actualMode} "$currentSourcePath"
                  fi
                  [ -d "$currentTargetPath" ] || mkdir "$currentTargetPath"

                  currentRealSourcePath="$(realpath "$currentSourcePath")"
                  chown --reference="$currentRealSourcePath" "$currentTargetPath"
                  chmod --reference="$currentRealSourcePath" "$currentTargetPath"
                  previousPath="$currentTargetPath"
                done
              )
            '';

          mkDirCreationScriptForPath = stateRoot:
            let
              entries = [ cfg.${stateRoot} ] ++ (mapAttrsToList (_: v: v) cfg.${stateRoot}.users);

              toEntry = entry: nameValuePair
                "createDirsIn-${replaceStrings [ "/" "." " " ] [ "-" "" "" ] "${stateRoot}-for-${entry.name}"}"
                (stringAfter [ "preStateSetup" ] (concatMapStrings
                  (mkStateSourceTargetDir { inherit (entry) user group; } stateRoot)
                  entry.directories
                ));
            in
            map toEntry entries;

          mkLinkCreationScriptForPath = stateRoot:
            let
              entries = [ cfg.${stateRoot} ] ++ (mapAttrsToList (_: v: v) cfg.${stateRoot}.users);

              toEntry = entry: nameValuePair
                "createLinksIn-${replaceStrings [ "/" "." " " ] [ "-" "" "" ] "${stateRoot}-for-${entry.name}"}"
                (stringAfter [ "preStateSetup" ] (concatMapStrings
                  (mkLinksToStateStorage { inherit (entry) user group; } stateRoot)
                  entry.files
                ));
            in
            map toEntry entries;
        in
        (listToAttrs (flatten (map mkDirCreationScriptForPath stateStoragePaths))) //
        (listToAttrs (flatten (map mkLinkCreationScriptForPath stateStoragePaths)))
      ) // {
        preStateSetup = lib.mkDefault ''
          true
        '';
      };

    systemd.mounts =
      map
        (binding:
          {
            before = [ "local-fs.target" ];
            wantedBy = [ "local-fs.target" ];
            inherit (binding) what where;
            type = "none";
            options = "bind";
          }
        )
        bindMounts;
  };
}
