{ stdenv
, lib
, writeStrictShellScriptBin
, writeText
, btrfsProgs
, coreutils
, openssh
, bashInteractive
, dockerTools
, dockerRegistry ? "johnae"
, dockerTag ? "latest"
}:
let
  rbreceive = writeStrictShellScriptBin "rbreceive" ''
    # shellcheck disable=SC2086
    set -- $SSH_ORIGINAL_COMMAND

    export PATH=${btrfsProgs}/bin:${coreutils}/bin:${PATH:+:}$PATH

    cmd=''${1:-}
    dest=''${2:-}
    echo "cmd: '$cmd', dest: '$dest'"
    maxdaily=''${MAX_DAILY:-5}
    keepdaily=''${KEEP_DAILY:-1}
    today=$(date +%Y%m%d)

    snapshot=.snapshot
    current=current
    new=new

    declare -a keep
    for i in {0..7}; do
        keep[$(date +%Y%m%d -d "-$i day")]="1";
    done

    if [ -z "$dest" ]; then
      echo "sorry, you must provide a destination as second argument"
      exit 1
    fi

    gc() {
      store=$1
      if [ -e "$store/$snapshot-$new" ]; then
        last="$(date +%Y%m%d%H%M%S -d @"$(stat -c %Z "$store/$snapshot-$current")")"
        if [ -e "$store/$snapshot-$last" ]; then
          echo "preexisting $store/$snapshot-$last, removing first"
          btrfs subvolume delete "$store/$snapshot-$last"
        fi
        echo "move $store/$snapshot-$current to $store/$snapshot-$last"
        mv "$store/$snapshot-$current" "$store/$snapshot-$last"

        echo "moving new remote backup $store/$snapshot-$new to $store/$snapshot-$current..."
        mv "$store/$snapshot-$new" "$store/$snapshot-$current"

        echo "cleaning out old daily snapshots"
        for snap in $( (ls -da "$store/$snapshot-$today"* || true) | sort -r | tail -n +$((maxdaily+1)) ); do
          echo "removing old daily snapshot: '$snap'"
          echo "btrfs subvolume delete $snap"
          btrfs subvolume delete "$snap"
        done

        echo "cleaning out snapshots older than today, keeping a weeks worth ($keepdaily per day)"
        for snap in $( (ls -da "$store/$snapshot-2"* || true) | sort -r ); do
          name=$(basename "$snap")
          when=''${name//$snapshot-/}
          day=$(echo "$when" | cut -c1-8)
          if [ "$day" = "$today" ]; then
            echo "skip $snap (today)"
            continue
          fi
          k=''${keep[$day]:-}
          if [ "$k" != "1" ]; then
            echo "removing snap older than a week: $snap"
            echo "btrfs subvolume delete $snap"
            btrfs subvolume delete "$snap"
          else
            for dailysnap in $( (ls -da "$store/$snapshot-$day"* || true) | sort -r | tail -n +$((keepdaily+1)) ); do
              echo "remove old snap $dailysnap (keeping one per day)"
              echo "btrfs subvolume delete $dailysnap"
              btrfs subvolume delete "$dailysnap"
            done
          fi
        done
      fi
    }

    receive() {
      store=$1
      if [ -e "$store/$snapshot-$new" ]; then
        echo "preexisting $store/$snapshot-$new, removing before receiving..."
        btrfs subvolume delete "$store/$snapshot-$new"
      fi
      echo "btrfs receive \"$store\""
      if ! btrfs receive "$store"; then
        echo >&2 "error receiving snapshot"
        exit 1
      fi
      sync
      gc "$store"
    }

    exists() {
      store=$1
      if test -e "$store" && test -e "$store/$snapshot-$current"; then
        echo "$store and $store/$snapshot-$current exist"
        exit 0
      else
        echo "$store and $store/$snapshot-$current do not exist"
        exit 1
      fi
    }

    check() {
      echo "ok"
      exit 0
    }

    setup() {
      store=$1
      echo "setting up backup '$store'"
      echo "mkdir -p \"$(dirname "$store")\""
      mkdir -p "$(dirname "$store")"
      echo "btrfs subvolume create \"$store\""
      btrfs subvolume create "$store" || true
      exit 0
    }

    nocommand() {
      echo >&2 "sorry only receive, exists, check and setup commands are allowed - they all take the destination path"
      exit 1
    }

    case "$cmd" in
      receive)
        receive "$dest"
        ;;
      setup)
        setup "$dest"
        ;;
      exists)
        exists "$dest"
        ;;
      check)
        check "$dest"
        ;;
      *)
        nocommand "$dest"
        ;;
    esac
  '';

  writeKeys = with lib; loginKeys: backupKeys: to: ''
    {
    ${concatMapStringsSep "\n"
      (x: '' echo '${x}' '')
      loginKeys}
    ${concatMapStringsSep "\n"
      (x: '' echo 'command="${rbreceive}/bin/rbreceive",no-port-forwarding,no-X11-forwarding,no-agent-forwarding,no-pty ${x}' '')
      backupKeys}
    } >> ${to}
  '';

  authorizedLoginKeys = [
    ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCyjMuNOFrZBi7CrTyu71X+aRKyzvTmwCEkomhB0dEhENiQ3PTGVVWBi1Ta9E9fqbqTW0HmNL5pjGV+BU8j9mSi6VxLzJVUweuwQuvqgAi0chAJVPe0FSzft9M7mJoEq5DajuSiL7dSjXpqNFDk/WCDUBE9pELw+TXvxyQpFO9KZwiYCCNRQY6dCjrPJxGwG+JzX6l900GFrgOXQ3KYGk8vzep2Qp+iuH1yTgEowUICkb/9CmZhHQXSvq2gAtoOsGTd9DTyLOeVwZFJkTL/QW0AJNRszckGtYdA3ftCUNsTLSP/VqYN9EjxcMHQe4PGjkK7VLb59DQJFyRQqvPXiUyxNloHcu/sDuiKHIk/0qDLHlVn2xc5zkvzSqoQxoXx+P4dDbje1KHLY8E96gLe2Csu0ti+qsM5KEvgYgwWwm2g3IBlaWwgAtC0UWEzIuBPrAgPd5vi+V50ITIaIk6KIV7JPOubLUXaLS5KW77pWyi9PqAGOXj+DgTWoB3QeeZh7CGhPL5fAecYN7Pw734cULZpnw10Bi/jp4Nlq1AJDk8BwLUJbzZ8aexwMf78syjkHJBBrTOAxADUE02nWBQd0w4K5tl/a3UnBYWGyX8TD44046Swl/RY/69PxFvYcVRuF4eARI6OWojs1uhoR9WkO8eGgEsuxxECwNpWxR5gjKcgJQ== card''
  ];

  authorizedBackupKeys = [
    ''ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDCfgTJK7V8XzYW+YJ725xo8m2beLDi2xaO3yI6/wpDIWxVOrsYYZTWIh6960n4juD7pAz5RYbYtQn87f74cXMTE6GWFlvIgTX1UL9sn7nBRowUQQ7bJdwx7PYz0upu1KoB/sW4QRCYLGCnQz19tQwrC49VynMT7x9H66R9kvWavgyWz8XvAPg5nQG5Fs+k7QZAydULJUcZ/ddLWKkMEubz4syBtwvODRP0Duhlr1YQvYsyF9lNxRFiWrl82PuymtZEzsJcaVqfMGxS5TZNfPA0AlDdHTKES/LAh9vqkTeZe0z+a1cpHzpx2aadk2l8/YavI6Wq9ctA23wa76qnmETgjgQc8pkxTzhvSVgeszWl9WHNOZdPqPYuczqWHsG/iNqoqv3+aCH61HGJoFkLOetO+KczlbrtMUDZQYSTzqkTubo+USBpDvlH0i8DENWWuNyyx4P7HWRaqiaJ/f2oylZImRjG4oQXVbl9uf+6IZL71My4feDVvCHxOTfgX4ucPP8= john@backup''
  ];

  entrypoint = writeStrictShellScriptBin "entrypoint.sh" ''
    export PATH=${rbreceive}/bin:${coreutils}/bin:${PATH:+:}$PATH
    env > /etc/environment

    if [ ! -e "/storage/ssh_host_rsa_key" ]; then
       ssh-keygen -q -t rsa -N "" -f /storage/ssh_host_rsa_key
    fi

    cp /storage/ssh_host_rsa_key /etc/ssh/
    mkdir -p /run /var/empty /root/.ssh
    chmod 0700 /root/.ssh
    ${writeKeys
      authorizedLoginKeys
      authorizedBackupKeys
      "/root/.ssh/authorized_keys"
    }
    chmod 0600 /root/.ssh/authorized_keys
    chmod 0600 /root/.ssh/authorized_keys

    exec ${openssh}/bin/sshd -e -D -p 22
  '';

  passwd = writeText "passwd" ''
    root:x:0:0:System administrator:/root:/bin/bash
    sshd:x:498:65534:SSH privilege separation user:/var/empty:/bin/nologin
  '';

  rootfs = stdenv.mkDerivation {
    version = "1";
    name = "rootfs-btrfs-backups";
    buildCommand = ''
      mkdir -p $out/etc
      cp ${passwd} $out/etc/passwd
    '';
  };
in
dockerTools.buildLayeredImage {
  name = "${dockerRegistry}/btrfs-backups";
  tag = dockerTag;
  contents = [
    rbreceive
    bashInteractive
    openssh
    btrfsProgs
    coreutils
    rootfs
  ];

  config = {
    Entrypoint = [ "${entrypoint}/bin/entrypoint.sh" ];
    ExposedPorts = {
      "22/tcp" = { };
    };
    WorkingDir = "/root";
    Volumes = {
      "/storage" = { };
    };
  };
}
