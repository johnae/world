{ pkgs, config, lib, options }:
let
  list-filter = pkgs.writeStrictShellScript "list-filter" ''
    NOTMUCH_MSG_ID=''${NOTMUCH_MSG_ID:-}
    list="$(notmuch show --format=raw "$NOTMUCH_MSG_ID" 2>/dev/null | \
                    grep -E '^List-ID:\s.+')"
    read -r -a parts <<< "''${list//[<>]/}"
    notmuch tag +list/"''${parts[1]}" -- "$NOTMUCH_MSG_ID"
  '';

  post-new-notmuch = pkgs.writeStrictShellScript "post-new-notmuch" ''

    maildir="${config.accounts.email.maildirBasePath}"
    notcoal_rules="$maildir"/.notmuch/hooks/notcoal-rules.json
    comma=

    cat<<EOF>"$notcoal_rules"
    [
    {
      "name": "Any list",
      "desc": "Anything with a List-ID",
      "rules": [
        {"List-ID": ".*"}
      ],
      "op": {
        "add": [ "thefeed", "list" ],
        "rm": [ "inbox", "new" ],
        "run": [ "${list-filter}" ]
      }
    }
    EOF
    comma=,

    touch "$maildir"/.notmuch/hooks/ledger.db
    while IFS= read -r line; do
        tag=$(echo "$line" | cut -d' ' -f1 -)
        entry=$(echo "$line" | cut -d' ' -f2 -)
        if [ -n "$entry" ]; then

            cat<<EOF>>"$notcoal_rules"
    $comma{
      "name": "$tag",
      "desc": "Ledger - $entry = $tag",
      "rules": [
          {"from": "$entry"}
      ],
      "op": {
          "add": "ledger/$tag",
          "rm": [ "inbox", "new" ]
      }
    }
    EOF
            comma=,
        fi
    done < "$maildir"/.notmuch/hooks/ledger.db

    touch "$maildir"/.notmuch/hooks/spam.db
    while IFS= read -r entry; do
        if [ -n "$entry" ]; then
            cat<<EOF>>"$notcoal_rules"
    $comma{
      "name": "spam/$entry",
      "desc": "$entry = spam",
      "rules": [
          {"from": "$entry"}
      ],
      "op": {
          "add": [ "spam", "deleted" ],
          "rm": [ "inbox", "new" ]
      }
    }
    EOF
            comma=,
        fi
    done < "$maildir"/.notmuch/hooks/spam.db

    touch "$maildir"/.notmuch/hooks/thefeed.db
    while IFS= read -r entry; do
        if [ -n "$entry" ]; then
            cat<<EOF>>"$notcoal_rules"
    $comma{
      "name": "thefeed/$entry",
      "desc": "$entry = thefeed",
      "rules": [
          {"from": "$entry"}
      ],
      "op": {
          "add": [ "thefeed" ],
          "rm": [ "inbox", "new" ]
      }
    }
    EOF
            comma=,
        fi
    done < "$maildir"/.notmuch/hooks/thefeed.db

    touch "$maildir"/.notmuch/hooks/screened.db
    while IFS= read -r entry; do
        if [ -n "$entry" ]; then
            cat<<EOF>>"$notcoal_rules"
    $comma{
      "name": "screened/$entry",
      "desc": "$entry = screened",
      "rules": [
          {"from": "$entry"}
      ],
      "op": {
          "add": [ "screened" ],
          "rm": [ "new" ]
      }
    }
    EOF
            comma=,
        fi
    done < "$maildir"/.notmuch/hooks/screened.db

    cat<<EOF>>"$notcoal_rules"
    $comma{
      "name": "inbox",
      "desc": "Send to inbox",
      "rules": [
        {"@tags": [ "new" ]}
      ],
      "op": {
        "add": [ "inbox" ],
        "rm": [ "new" ]
      }
    }
    ]
    EOF

    ${pkgs.notcoal}/bin/notcoal -c "$NOTMUCH_CONFIG" -f "$notcoal_rules"

  '';
in
{
  programs.notmuch.enable = true;
  programs.notmuch.new.tags = [ "new" ];
  programs.notmuch.hooks.postNew = "exec ${post-new-notmuch}";
}
