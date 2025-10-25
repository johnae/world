{
  perSystem = {pkgs, ...}: let
    inherit
      (pkgs)
      stdenv
      lib
      writeShellApplication
      buildEnv
      dotool
      fd
      fire
      fuzzel
      hostname
      pass
      rbw
      rofi
      skim
      tailscale
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
      runtimeInputs = [rofi rbw dotool];
      text = ''
        passonly=''${passonly:-}
        codeonly=''${codeonly:-}
        selection="$(rbw list --fields name,user | \
           sed 's|\t|/|g' | \
           rofi -normal-window -matching fuzzy -i -dmenu)"

        entry="$(echo "$selection" | awk -F'/' '{print $1}')"
        login="$(echo "$selection" | awk -F'/' '{print $2}')"
        pass="$(rbw get "$entry" "$login")"

        if [ -n "$passonly" ]; then
          echo -n "type $pass" | dotool
        elif [ -n "$codeonly" ]; then
          code="$(rbw code "$entry" "$login")"
          echo -n "type $code" | dotool
        else
          echo -en "type $login\t$pass" | dotool
        fi
      '';
    };

    list-projects = writeShellApplication {
      name = "list-projects";
      runtimeInputs = [pkgs.fd pkgs.openssh];
      text = ''
        LOCATION="''${1:-}"

        if [ -z "$LOCATION" ]; then
          echo "you must provide the location to search as the first argument"
          echo "either /path/to/dir or user@host:/path/to/dir works"
          exit 1
        fi

        # Detect if this is a remote SSH location or local path
        if [[ "$LOCATION" == *:* ]] && [[ "$LOCATION" != /* ]]; then
          # Remote SSH location (format: [user@]host:path)
          HOST="''${LOCATION%%:*}"
          REMOTE_PATH="''${LOCATION#*:}"

          # Check if ssh is available
          if ! command -v ssh &> /dev/null; then
            echo "Error: 'ssh' command not found but remote location specified"
            exit 1
          fi

          # Execute fd on remote host, explicitly using bash to avoid shell compatibility issues
          # shellcheck disable=SC2087
          ssh "$HOST" bash <<EOF
        fd '(\.git|\.jj|workspace\.yaml)\$' $REMOTE_PATH -d 8 -H -X echo '{//}' | tr ' ' '\n' | sort -u
        EOF
        else
          # Local location
          fd '(\.git|\.jj|workspace\.yaml)$' "$LOCATION" -d 8 -H -X echo "{//}" | tr ' ' '\n' | sort -u
        fi
      '';
    };

    fuzzel-wezterm-domain = writeShellApplication {
      name = "fuzzel-wezterm";
      runtimeInputs = [fuzzel tailscale];
      text = ''
        tailscale status | \
          grep linux | \
          awk '{print $2}' | \
          xargs -r -I{} echo -e '- name: {}\n  username: john\n  remote_address: {}' > "$HOME"/.config/wezterm/ssh_domains.yaml
        if [ -e "$HOME"/Development/more_wezterm_domains.yaml ]; then
          cat "$HOME"/Development/more_wezterm_domains.yaml >> "$HOME"/.config/wezterm/ssh_domains.yaml
        fi
        (grep ' name: ' < "$HOME"/.config/wezterm/ssh_domains.yaml | awk '{print $3}'; echo local-dev) | fuzzel -d
      '';
    };

    fuzzel-rbw = writeShellApplication {
      name = "fuzzel-rbw";
      runtimeInputs = [fuzzel rbw dotool];
      text = ''
        passonly=''${passonly:-}
        selection="$(rbw list --fields name,user | \
           sed 's|\t|/|g' | \
           fuzzel -d)"

        entry="$(echo "$selection" | awk -F'/' '{print $1}')"
        login="$(echo "$selection" | awk -F'/' '{print $2}')"
        pass="$(rbw get "$entry" "$login")"

        if [ -z "$passonly" ]; then
          echo -en "type $login\t$pass" | dotool
        else
          echo -n "type $pass" | dotool
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
          fuzzel-rbw
          fuzzel-wezterm-domain
          launch
          list-projects
          project-select
          rbw-git-creds
          rofi-rbw
          update-wifi-networks
          update-wireguard-keys
        ];
        meta.platforms = lib.platforms.linux;
      };
    };
  };
}
