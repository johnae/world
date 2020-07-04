{ stdenv
, lib
, writeScriptBin
, writeTextFile
, writeStrictShellScriptBin
, my-emacs
, wl-clipboard
, ps
, jq
, fire
, sway
, udev
, fd
, skim
, bashInteractive
, pass
, wpa_supplicant
, cloud-sql-proxy
, gnupg
, gawk
, gnused
, openssl
, gnugrep
, findutils
, coreutils
, alacritty
, libnotify
, hostname
, maim
, slop
, killall
, wget
, procps
, openssh
, kubectl
, diffutils
, chromium
, nix-prefetch-github
, signal-desktop
, btrfs-progs
, ...
}:
let
  emacsclient = "${my-emacs}/bin/emacsclient";

  random-name = writeStrictShellScriptBin "random-name" ''
    NAME=''${1:-}
    if [ -z "$NAME" ]; then
      echo "Please provide a base name as the only argument"
      exit 1
    fi
    echo "$NAME-$(openssl rand 4 -hex)"
  '';

  git-credential-pass = writeStrictShellScriptBin "git-credential-pass" ''
    passfile="$1"
    echo password="$(${pass}/bin/pass show "$passfile" | head -1)"
  '';

  sk-sk = writeStrictShellScriptBin "sk-sk" ''
    SK_MIN_HEIGHT=''${SK_MIN_HEIGHT:-100}
    SK_MARGIN=''${SK_MARGIN:-5,5,5,5}
    SK_PROMPT=''${SK_PROMPT:- >}

    export SKIM_DEFAULT_OPTIONS=''${SK_OPTS:-"--reverse"}
    exec ${skim}/bin/sk --min-height="$SK_MIN_HEIGHT" \
        --margin="$SK_MARGIN" \
        --prompt="$SK_PROMPT"
  '';

  btrfs-diff = writeStrictShellScriptBin "btrfs-diff" ''
    usage() { echo "$@" >&2; echo "Usage: $0 <older-snapshot> <newer-snapshot>" >&2; exit 1; }

    [ $# -eq 2 ] || usage "Wrong number of arguments"
    snapshot_old="$1"
    snapshot_new="$2"

    [ -d "$snapshot_old" ] || usage "$snapshot_old does not exist"
    [ -d "$snapshot_new" ] || usage "$snapshot_new does not exist"

    old_transid="$(btrfs subvolume find-new "$snapshot_old" 9999999)"
    old_transid=''${old_transid#transid marker was }

    { [ -n "$old_transid" ] && [ "$old_transid" -gt 0 ]; } || usage "Couldn't find generation for $snapshot_new"

    ${btrfs-progs}/bin/btrfs subvolume find-new "$snapshot_new" "$old_transid" | \
                             ${gnused}/bin/sed '$d' | \
                             ${gawk}/bin/awk '{print $17}' | \
                             sort -u
  '';

  project-select = writeStrictShellScriptBin "project-select" ''
    projects=$*
    if [ -z "$projects" ]; then
      ${coreutils}/bin/echo "Please provide the project root directories to search as arguments"
      exit 1
    fi
    export SK_PROMPT="goto project >"
    export SK_OPTS="--color=bw --tac --reverse"
    # shellcheck disable=SC2086
    ${fd}/bin/fd -d 8 -pHI -t f '.*\.git/config|.*\.projectile' $projects | \
      ${gnused}/bin/sed -e 's|/\.git/config||g' \
                        -e 's|/\.projectile||g' \
                        -e "s|$HOME/||g" | \
      ${sk-sk}/bin/sk-sk | \
      ${findutils}/bin/xargs -I{} ${coreutils}/bin/echo "$HOME/{}"
  '';

  spotify-cmd = writeStrictShellScriptBin "spotify-cmd" ''
    echo "$@" > "$XDG_RUNTIME_DIR"/spotnix_input
  '';

  spotify-play = writeStrictShellScriptBin "spotify-play" ''
    TYPE="$1"
    set +e
    search="$(SK_OPTS="--print-query" ${sk-sk}/bin/sk-sk < /dev/null)"
    set -e
    echo "$TYPE" "$search" > "$XDG_RUNTIME_DIR"/spotnix_input
    ${sk-sk}/bin/sk-sk < "$XDG_RUNTIME_DIR"/spotnix_output | \
        awk '{print $NF}' | xargs -r -I{} ${spotify-cmd}/bin/spotify-cmd {}
  '';

  spotify-play-track = writeStrictShellScriptBin "spotify-play-track" ''
    exec ${spotify-play}/bin/spotify-play s
  '';

  spotify-play-artist = writeStrictShellScriptBin "spotify-play-artist" ''
    exec ${spotify-play}/bin/spotify-play sar
  '';

  spotify-play-album = writeStrictShellScriptBin "spotify-play-album" ''
    exec ${spotify-play}/bin/spotify-play sab
  '';

  spotify-play-playlist = writeStrictShellScriptBin "spotify-play-playlist" ''
    exec ${spotify-play}/bin/spotify-play sap
  '';

  screenshot = writeStrictShellScriptBin "screenshot" ''
    name=$(${coreutils}/bin/date +%Y-%m-%d_%H:%M:%S_screen)
    output_dir=$HOME/Pictures/screenshots
    fmt=png
    ${coreutils}/bin/mkdir -p "$output_dir"
    ${maim}/bin/maim -s --format="$fmt $output_dir/$name.$fmt"
  '';

  browse-chromium = writeStrictShellScriptBin "browse-chromium" ''
    export GDK_BACKEND=x11
    exec ${chromium}/bin/chromium
  '';

  signal = writeStrictShellScriptBin "signal" ''
    export GDK_BACKEND=x11
    exec ${signal-desktop}/bin/signal-desktop
  '';

  launch = writeStrictShellScriptBin "launch" ''
    cmd=$*
    if [ -z "$cmd" ]; then
      read -r cmd
    fi
    echo "${fire}/bin/fire $cmd" | ${stdenv.shell}
  '';

  sk-run = writeScriptBin "sk-run" ''
    #!${bashInteractive}/bin/bash
    export SK_PROMPT="run >> "
    export SK_OPTS="$SK_OPTS''${SK_OPTS:+ }--no-bold --color=bw --height=40 --no-hscroll --no-mouse --print-query --reverse"

    compgen -c | \
    sort -u | \
    ${gawk}/bin/awk '{ if (length($0) > 2) print }' | \
    ${gnugrep}/bin/grep -v -E '^\..*' | \
    ${sk-sk}/bin/sk-sk | \
    tail -n1 | \
    ${findutils}/bin/xargs -r ${launch}/bin/launch
  '';

  sk-window = writeStrictShellScriptBin "sk-window" ''
    cmd=''${1:-}
    if [ -z "$cmd" ]; then
      echo "Please provide a command to run in the window as the argument"
      exit 1
    fi
    shift
    if ${ps}/bin/ps aux | ${gnugrep}/bin/grep '\-t sk-window' | \
       ${gnugrep}/bin/grep -v grep > /dev/null 2>&1; then
        ${ps}/bin/ps aux | \
            ${gnugrep}/bin/grep '\-t sk-window' | \
            ${gnugrep}/bin/grep -v grep | \
            ${gawk}/bin/awk '{print $2}' | \
            ${findutils}/bin/xargs -r -I{} kill {}
        exit
    fi

    # shellcheck disable=SC2086
    exec ${alacritty}/bin/alacritty --config-file ~/.config/alacritty/alacritty-launcher.yml --class "sk-window" -e $cmd
  '';

  sk-passmenu = writeStrictShellScriptBin "sk-passmenu" ''
    export SK_PROMPT="copy password >> "
    export SK_OPTS="--no-bold --color=bw  --height=40 --reverse --no-hscroll --no-mouse"

    passfile=''${1:-}
    nosubmit=''${nosubmit:-}
    passonly=''${passonly:-}
    _passmenu_didsearch=''${_passmenu_didsearch:-}
    SWAYSOCK=''${SWAYSOCK:-}
    prefix=$(readlink -f "$PASSWORD_STORE_DIR")
    if [ -z "$_passmenu_didsearch" ]; then
      export _passmenu_didsearch=y
      ${fd}/bin/fd --type f -E '/notes/' '.gpg$' "$PASSWORD_STORE_DIR" | \
         ${gnused}/bin/sed "s|$prefix/||g" | ${gnused}/bin/sed 's|.gpg$||g' | \
         ${sk-sk}/bin/sk-sk | \
         ${findutils}/bin/xargs -r -I{} ${coreutils}/bin/echo "$0 {}" | \
         ${launch}/bin/launch
    fi

    if [ "$passfile" = "" ]; then
      exit
    fi

    error_icon=~/Pictures/icons/essential/error.svg

    getlogin() {
      ${coreutils}/bin/echo -n "$(${coreutils}/bin/basename "$1")"
    }

    getpass() {
      ${coreutils}/bin/echo -n "$(${gnupg}/bin/gpg --decrypt "$prefix/$1.gpg" \
                            2>/dev/null | ${coreutils}/bin/head -1)"
    }

    login=$(getlogin "$passfile")
    pass=$(getpass "$passfile")

    if [ "$pass" = "" ]; then
      ${libnotify}/bin/notify-send -i $error_icon -a "Password store" -u critical \
      "Decrypt error" "Error decrypting password file, is your gpg card inserted?"
    else
      if [ -z "$passonly" ]; then
        ${coreutils}/bin/echo -n "$login" | ${wl-clipboard}/bin/wl-copy -onf
        ${coreutils}/bin/echo -n "$pass" | ${wl-clipboard}/bin/wl-copy -onf
      else
        ${coreutils}/bin/echo -n "$pass" | ${wl-clipboard}/bin/wl-copy -onf
      fi
    fi

  '';

  update-wireguard-keys = writeStrictShellScriptBin "update-wireguard-keys" ''
    IFS=$'\n'
    HN="$(${hostname}/bin/hostname)"
    for KEY in $(find "$PASSWORD_STORE_DIR"/vpn/wireguard/"$HN"/ -type f -print0 | xargs -0 -I{} basename {}); do
      KEYNAME=$(basename "$KEY" .gpg)
      echo "Ensure wireguard key \"$KEYNAME\" is available"
      ${pass}/bin/pass show "vpn/wireguard/$HN/$KEYNAME" | sudo tee /var/lib/wireguard/"$KEYNAME" > /dev/null
      sudo chmod 0600 /var/lib/wireguard/"$KEYNAME"
    done
  '';

  update-wifi-networks = writeStrictShellScriptBin "update-wifi-networks" ''
    IFS=$'\n'
    for NET in $(find "$PASSWORD_STORE_DIR"/wifi/networks/ -type f -print0 | xargs -0 -I{} basename {}); do
      NETNAME=$(basename "$NET" .gpg)
      echo "Ensure wireless network \"$NETNAME\" is available"
      ${pass}/bin/pass show "wifi/networks/$NETNAME" | sudo tee "/var/lib/iwd/$NETNAME.psk" > /dev/null
    done
  '';

  add-wifi-network = writeStrictShellScriptBin "add-wifi-network" ''
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
    PSK=$(${wpa_supplicant}/bin/wpa_passphrase "$1" "$2" | grep "[^#]psk=" | awk -F'=' '{print $2}')
    if [ -z "$PSK" ]; then
      echo Hmm PSK was empty
      exit 1
    fi
    cat <<EOF | ${pass}/bin/pass insert "wifi/networks/$NET"
    [Security]
    PreSharedKey=$PSK
    Passphrase=$PASS
    EOF
    ${update-wifi-networks}/bin/update-wifi-networks
  '';

  mail = writeStrictShellScriptBin "mail" ''
    exec ${alacritty}/bin/alacritty -e ${emacsclient} -t -a= -e '(mu4e)'
  '';
in
{
  paths = {
    inherit mail project-select launch git-credential-pass sk-sk
      sk-run sk-window sk-passmenu browse-chromium signal
      screenshot random-name add-wifi-network update-wifi-networks
      update-wireguard-keys spotify-play-album spotify-play-track spotify-cmd
      spotify-play-artist spotify-play-playlist btrfs-diff
      ;
  };
}
