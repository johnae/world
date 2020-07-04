#!@bash@/bin/bash

PATH=@gnugrep@/bin:@socat@/bin${PATH:+:}$PATH
export PATH

dbus_addr="$DBUS_SESSION_BUS_ADDRESS"

is_abstract() {
    local IFS
    IFS=';'
    for addr in $dbus_addr; do
        case "$addr" in
            unix:path=*|unix:*,path=*)
                return 1 ;;
        esac
    done
    return 0
}

if [ -n "$dbus_addr" ] && is_abstract; then
    IFS=';'
    for addr in $dbus_addr; do
        case "$addr" in
            unix:abstract=*|unix:*,abstract=*)
                name=
                guid_opt=
                IFS=','
                for part in ${addr#unix:}; do
                    case "$part" in
                        abstract=*)
                            name="${part#abstract=}" ;;
                        guid=*)
                            guid_opt=",$part" ;;
                    esac
                done

                if [ -n "$name" ]; then
                    proxy_path="/tmp/dbus-proxy-$$"
                    socat UNIX-LISTEN:"$proxy_path",fork ABSTRACT-CONNECT:"$name" &
                    trap 'kill -TERM $! && wait' 0
                    export DBUS_SESSION_BUS_ADDRESS="unix:path=$proxy_path$guid_opt"
                fi
                break
                ;;
        esac
    done
fi

"$@"