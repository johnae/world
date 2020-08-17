{ pkgs, config, lib, options }:

{
  programs.notmuch.enable = true;
  programs.notmuch.new.tags = [ "new" ];
  programs.notmuch.hooks.postNew = ''
    maildir="${config.accounts.email.maildirBasePath}";

    for msg in $(notmuch search --output=messages tag:new); do

      if list="$(notmuch show --format=raw "$msg" 2>/dev/null | grep -E '^List-ID:\s.+')"; then
        listname="$(echo "$list" | awk '{print $2}')"
        notmuch tag -new +list/"$listname" +thefeed -- "$msg"
      fi

    done

    touch "$maildir"/.notmuch/hooks/ledger.db
    while IFS= read -r line; do
        tag=$(echo "$line" | cut -d' ' -f1 -)
        entry=$(echo "$line" | cut -d' ' -f2 -)
        if [ -n "$entry" ]; then
            notmuch tag +ledger/"$nm_tag" -new -- tag:new and from:"$entry"
        fi
    done < "$maildir"/.notmuch/hooks/ledger.db

    touch "$maildir"/.notmuch/hooks/spam.db
    for entry in $(cat "$maildir"/.notmuch/hooks/spam.db); do
        if [ -n "$entry" ]; then
            notmuch tag +spam +deleted -new -- tag:new and from:"$entry"
        fi
    done

    touch "$maildir"/.notmuch/hooks/thefeed.db
    for entry in $(cat "$maildir"/.notmuch/hooks/thefeed.db); do
        if [ -n "$entry" ]; then
            notmuch tag +thefeed -new -- tag:new and from:"$entry"
        fi
    done

    touch "$maildir"/.notmuch/hooks/screened.db
    for entry in $(cat "$maildir"/.notmuch/hooks/screened.db); do
        if [ -n "$entry" ]; then
            notmuch tag +screened -- tag:new and from:"$entry"
        fi
    done

    notmuch tag +inbox +unread -new -- tag:new
  '';
}
