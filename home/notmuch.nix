{ pkgs, config, lib, options }:
let
  post-new-notmuch = pkgs.writeStrictShellScript "post-new-notmuch" ''
    maildir="${config.accounts.email.maildirBasePath}";

    for msg in $(notmuch search --output=messages tag:new); do

      if list="$(notmuch show --format=raw "$msg" 2>/dev/null | grep -E '^List-ID:\s.+')"; then
        read -r -a parts <<< "''${list//[<>]/}"
        notmuch tag -inbox -new +list/"''${parts[1]}" +thefeed -- "$msg"
      fi

    done

    touch "$maildir"/.notmuch/hooks/ledger.db
    while IFS= read -r line; do
        tag=$(echo "$line" | cut -d' ' -f1 -)
        entry=$(echo "$line" | cut -d' ' -f2 -)
        if [ -n "$entry" ]; then
            notmuch tag +ledger/"$tag" -inbox -new -- tag:new and from:"$entry"
        fi
    done < "$maildir"/.notmuch/hooks/ledger.db

    touch "$maildir"/.notmuch/hooks/spam.db
    while IFS= read -r entry; do
        if [ -n "$entry" ]; then
            notmuch tag +spam +deleted -inbox -new -- tag:new and from:"$entry"
        fi
    done < "$maildir"/.notmuch/hooks/spam.db

    touch "$maildir"/.notmuch/hooks/thefeed.db
    while IFS= read -r entry; do
        if [ -n "$entry" ]; then
            notmuch tag +thefeed -inbox -new -- tag:new and from:"$entry"
        fi
    done < "$maildir"/.notmuch/hooks/thefeed.db

    touch "$maildir"/.notmuch/hooks/screened.db
    while IFS= read -r entry; do
        if [ -n "$entry" ]; then
            notmuch tag +screened -- tag:new and from:"$entry"
        fi
    done < "$maildir"/.notmuch/hooks/screened.db

    notmuch tag +inbox -new -- tag:new
  '';
in
{
  programs.notmuch.enable = true;
  programs.notmuch.new.tags = [ "new" ];
  programs.notmuch.hooks.postNew = "exec ${post-new-notmuch}";
}
