#!@bash@/bin/bash

WL_COPY=@wl-copy@
WL_PASTE=@wl-paste@

CLIPBOARD_BACKEND="WL_CLIPBOARD"
CLIPBOARD_COMMAND="${WL_COPY}"
VERSION=2

calc() { awk "BEGIN{print $*}"; }

show_help() {
    cat << EOF
Usage:
    $(basename "$0") [argument...]

A wrapper to use wl-clipboard as a drop-in replacement to X11 clipboard tools.

See wl-clipboard-x11(1) for more details.
EOF
}

show_version() {
    cat << EOF
wl-clipboard-x11 ${VERSION}
Copyright (C) 2019 Ian Brunelli
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
===============================================================================
EOF
    ${CLIPBOARD_COMMAND} --version
}

APPEND=0
FOLLOW=0
TIMEOUT=0
TIMEOUT_COMMAND=""
: $((COUNT = $#))
set -- "${@}" --

while [ "$COUNT" -ne 0 ]
do
    case "$1" in
        -i|-in) CLIPBOARD_COMMAND="${WL_COPY}";;
        -o|-out)
            CLIPBOARD_COMMAND="${WL_PASTE}"
            set -- "${@}" "--no-newline"
            ;;
        -f|-filter) true;;
        -r|-rmlastnl) set -- "${@}" "--trim-newline";;
        -l|-loops)
            [ "$2" -gt 0 ] 2> /dev/null && set -- "${@}" "--paste-once"
            : $((COUNT = COUNT - 1))
            shift 1
            ;;
        -t|-target) set -- "${@}" "--type";;
        -d|-display)
            : $((COUNT = COUNT -1 ))
            shift 1
            ;;
        -h|-help)
            show_help
            exit 0
            ;;
        -selection)
            case "$2" in
                primary) set -- "${@}" "--primary";;
                secondary) true;;
                clipboard) true;;
                *) set -- "${@}" "$2"
            esac
            : $((COUNT = COUNT - 1))
            shift 1
            ;;
        -version)
            show_version
            exit 0
            ;;
        -silent) shift;;
        -quiet) set -- "${@}" "--foreground";;
        -verbose) true;;
        -noutf8) true;;
        *) set -- "${@}" "$1";;
    esac
    : $((COUNT = COUNT - 1))
    shift 1
done

shift
[ "$TIMEOUT" -ne 0 ] && TIMEOUT_COMMAND="timeout $TIMEOUT"

if [ $((FOLLOW)) -ne 0 ]
then
    TEMP=$(mktemp -t ${CLIPBOARD_BACKEND}-buffer-XXXXXX)

    trap 'rm -f ${TEMP}; exit' EXIT
    trap '' INT

    if [ "$TIMEOUT_COMMAND" ]
    then
        sleep "${TIMEOUT}"; kill $$ &
    fi

    while dd count=1 of="${TEMP}" status=none
    do
        {
            [ $((APPEND)) -ne 0 ] && ${WL_PASTE} -n 2> /dev/null
            cat "${TEMP}"
        } |
            ${WL_COPY} "${@}"

        [ -s "${TEMP}" ] || break
        APPEND=1
    done
elif [ $((APPEND)) -ne 0 ]
then
    {
        ${WL_PASTE} -n 2> /dev/null
        cat
    } |
        ${TIMEOUT_COMMAND} "${WL_COPY}" "${@}"
else
    ${TIMEOUT_COMMAND} "${CLIPBOARD_COMMAND}" "${@}"
fi