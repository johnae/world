{ pkgs, config, lib, options }:

{
  programs.notmuch.enable = true;
  programs.notmuch.hooks.postNew = ''
    maildir="${config.accounts.email.maildirBasePath}";

    touch "$maildir"/.notmuch/hooks/ledger.db
    while IFS= read -r line; do
        tag=$(echo "$line" | cut -d' ' -f1 -)
        entry=$(echo "$line" | cut -d' ' -f2 -)
        if [ -n "$entry" ]; then
            notmuch tag +ledger/"$nm_tag" -inbox -- tag:inbox and tag:unread and from:"$entry"
        fi
    done < "$maildir"/.notmuch/hooks/ledger.db

    touch "$maildir"/.notmuch/hooks/spam.db
    for entry in $(cat "$maildir"/.notmuch/hooks/spam.db); do
        if [ -n "$entry" ]; then
            notmuch tag +spam +deleted -inbox -unread -- tag:inbox and tag:unread and from:"$entry"
        fi
    done

    touch "$maildir"/.notmuch/hooks/thefeed.db
    for entry in $(cat "$maildir"/.notmuch/hooks/thefeed.db); do
        if [ -n "$entry" ]; then
            notmuch tag +thefeed -inbox -- tag:inbox and tag:unread and from:"$entry"
        fi
    done

    touch "$maildir"/.notmuch/hooks/screened.db
    for entry in $(cat "$maildir"/.notmuch/hooks/screened.db); do
        if [ -n "$entry" ]; then
            notmuch tag +screened -- tag:inbox and tag:unread and from:"$entry"
        fi
    done
  '';
}
