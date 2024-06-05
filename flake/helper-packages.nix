{
  perSystem = {pkgs, ...}: let
    inherit
      (pkgs)
      stdenv
      lib
      writeShellApplication
      buildEnv
      fd
      fire
      hostname
      pass
      rbw
      rofi-wayland
      fuzzel
      skim
      wtype
      wpa_supplicant
      ;

    rbw-git-creds = writeShellApplication {
      name = "rbw-git-creds";
      runtimeInputs = [rbw];
      text = ''
        username=''${GIT_USERNAME:-}
        password=''${GIT_PASSWORD:-}
        if [ -z "$username" ] || [ -z "$password" ]; then
          record=''${1:-}
          item=''${2:-}
          if [ "$record" = "" ]; then
            echo Please provide the record the item is stored in
            exit 1
          fi
          if [ "$item" = "" ]; then
            echo Please provide the item name to get
            exit 1
          fi
          password="$(rbw get "$record" "$item" | head -1)"
          username="$(rbw get --full "$record" "$item" | grep "Username:" | awk '{print $2}')"
        fi
        cat<<CREDS
        username=$username
        password=$password
        CREDS
      '';
    };

    rbw-atomic-unlock = writeShellApplication {
      name = "rbw-atomic-unlock";
      runtimeInputs = [rbw];
      text = ''
        rbw_lock=/tmp/lock.rbw.lock
        rbw_pid_lock="/tmp/lock.$$.tmp"
        trap 'rm -f $rbw_lock $rbw_pid_lock' EXIT
        if ! rbw unlocked 2>/dev/null; then
          touch "$rbw_pid_lock"
          if ln "$rbw_pid_lock" "$rbw_lock" 2>/dev/null; then
              rbw unlock
              rm -f "$rbw_lock"
          else
              while ! rbw unlocked 2>/dev/null; do
                sleep 0.2
              done
          fi
        fi
      '';
    };

    sk-sk = writeShellApplication {
      name = "sk-sk";
      runtimeInputs = [skim];
      text = ''
        SK_MIN_HEIGHT=''${SK_MIN_HEIGHT:-100}
        SK_MARGIN=''${SK_MARGIN:-5,5,5,5}
        SK_PROMPT=''${SK_PROMPT:- >}

        export SKIM_DEFAULT_OPTIONS=''${SK_OPTS:-"--reverse"}
        exec sk --min-height="$SK_MIN_HEIGHT" \
            --margin="$SK_MARGIN" \
            --prompt="$SK_PROMPT"
      '';
    };

    project-select = writeShellApplication {
      name = "project-select";
      runtimeInputs = [sk-sk fd];
      text = ''
        projects=$*
        if [ -z "$projects" ]; then
          echo "Please provide the project root directories to search as arguments"
          exit 1
        fi
        export SK_PROMPT="goto project >"
        export SK_OPTS="--color=bw --tac --reverse"
        # shellcheck disable=SC2086
        fd -d 8 -pHI -t f '.*\.git/config|.*\.projectile' $projects | \
          sed -e 's|/\.git/config||g' \
              -e 's|/\.projectile||g' \
              -e "s|$HOME/||g" | \
          sk-sk | \
          xargs -I{} echo "$HOME/{}"
      '';
    };

    launch = writeShellApplication {
      name = "launch";
      runtimeInputs = [fire];
      text = ''
        cmd=$*
        if [ -z "$cmd" ]; then
          read -r cmd
        fi
        echo "fire $cmd" | ${stdenv.shell}
      '';
    };

    rofi-rbw = writeShellApplication {
      name = "rofi-rbw";
      runtimeInputs = [rofi-wayland rbw wtype];
      text = ''
        passonly=''${passonly:-}
        selection="$(rbw list --fields name,user | \
           sed 's|\t|/|g' | \
           rofi -normal-window -matching fuzzy -i -dmenu)"

        entry="$(echo "$selection" | awk -F'/' '{print $1}')"
        login="$(echo "$selection" | awk -F'/' '{print $2}')"
        pass="$(rbw get "$entry" "$login")"

        if [ -z "$passonly" ]; then
          echo -en "$login\t$pass" | wtype -
        else
          echo -n "$pass" | wtype -
        fi
      '';
    };

    fuzzel-rbw = writeShellApplication {
      name = "fuzzel-rbw";
      runtimeInputs = [fuzzel rbw wtype];
      text = ''
        passonly=''${passonly:-}
        selection="$(rbw list --fields name,user | \
           sed 's|\t|/|g' | \
           fuzzel -d)"

        entry="$(echo "$selection" | awk -F'/' '{print $1}')"
        login="$(echo "$selection" | awk -F'/' '{print $2}')"
        pass="$(rbw get "$entry" "$login")"

        if [ -z "$passonly" ]; then
          echo -en "$login\t$pass" | wtype -
        else
          echo -n "$pass" | wtype -
        fi
      '';
    };

    update-wireguard-keys = writeShellApplication {
      name = "update-wireguard-keys";
      runtimeInputs = [hostname pass];
      text = ''
        IFS=$'\n'
        HN="$(hostname)"
        for KEY in $(find "$PASSWORD_STORE_DIR"/vpn/wireguard/"$HN"/ -type f -print0 | xargs -0 -I{} basename {}); do
          KEYNAME=$(basename "$KEY" .gpg)
          echo "Ensure wireguard key \"$KEYNAME\" is available"
          pass show "vpn/wireguard/$HN/$KEYNAME" | sudo tee /var/lib/wireguard/"$KEYNAME" > /dev/null
          sudo chmod 0600 /var/lib/wireguard/"$KEYNAME"
        done
      '';
    };

    update-wifi-networks = writeShellApplication {
      name = "update-wifi-networks";
      runtimeInputs = [pass];
      text = ''
        IFS=$'\n'
        for NET in $(find "$PASSWORD_STORE_DIR"/wifi/networks/ -type f -print0 | xargs -0 -I{} basename {}); do
          NETNAME=$(basename "$NET" .gpg)
          echo "Ensure wireless network \"$NETNAME\" is available"
          pass show "wifi/networks/$NETNAME" | sudo tee "/var/lib/iwd/$NETNAME.psk" > /dev/null
        done
      '';
    };

    add-wifi-network = writeShellApplication {
      name = "add-wifi-network";
      runtimeInputs = [wpa_supplicant pass update-wifi-networks];
      text = ''
        NET=''${1:-}
        PASS=''${2:-}
        if [ -z "$NET" ]; then
          echo Please provide the network as first argument
          exit 1
        fi
        if [ -z "$PASS" ]; then
          echo Please provide the password as second argument
          exit 1
        fi
        PSK=$(wpa_passphrase "$1" "$2" | grep "[^#]psk=" | awk -F'=' '{print $2}')
        if [ -z "$PSK" ]; then
          echo Hmm PSK was empty
          exit 1
        fi
        cat <<EOF | pass insert -m "wifi/networks/$NET"
        [Security]
        PreSharedKey=$PSK
        Passphrase=$PASS
        EOF
        update-wifi-networks
      '';
    };
  in {
    packages = {
      scripts = buildEnv {
        name = "scripts";
        paths = [
          add-wifi-network
          launch
          project-select
          rbw-git-creds
          rbw-atomic-unlock
          rofi-rbw
          fuzzel-rbw
          update-wifi-networks
          update-wireguard-keys
        ];
        meta.platforms = lib.platforms.linux;
      };
    };
  };
}
