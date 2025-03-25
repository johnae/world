{
  pkgs,
  lib,
  adminUser,
  ...
}: let
  common = pkgs.writeShellScript "common" ''
    DRYRUN="''${DRYRUN:-}"
    DEBUG="''${DEBUG:-}"

    dry_run() {
        local cmd="$@"
        echo "DRYRUN: $cmd"
    }

    run() {
        if [ -n "$DRYRUN" ]; then
            dry_run "$@"
        else
            eval "$@"
        fi
    }

    maybe_debug() {
        if [ -n "$DEBUG" ]; then
          set -x
        fi
    }

    maybe_debug
  '';
  invoiceExtraction = pkgs.writeShellApplication {
    name = "invoice-extraction";
    runtimeInputs = with pkgs; [html2text ripmime poppler_utils];
    text = ''
      # shellcheck disable=SC1091
      source ${common}

      MSGID="''${1:-}"

      if [ -z "$MSGID" ]; then
        echo Please specify the MSGID as the first argument
        exit 1
      fi

      WORKDIR="$(mktemp -d /tmp/invoice.XXXXXXXXXX)"
      trap 'rm -rf $WORKDIR' EXIT

      if notmuch show --format json --include-html --entire-thread=false "$MSGID AND NOT attachment:pdf" | jq -e '. | length > 0' >/dev/null; then
        notmuch show --entire-thread=false --include-html --format=json "$MSGID AND NOT attachment.pdf" | \
                  jq -r '.. | objects | select(."content-type"=="text/html") | .content' | \
                  html2text -nobs | tee -a /dev/stderr | \
                  grep -oE '[^[:space:]]+' | grep -oE '[[:alnum:]]+' | tr '[:space:]' ' ' | tr -s ' ' | \
                  grep -oP '^.{0,1200}' | \
                  aichat -r invoice-summer | tee -a /dev/stderr > "$WORKDIR/invoice-sum-html.json"
        if jq -e '. |
          select(
            (.amount? | type == "number") and
            (.confidence? | type == "number") and
            (.currency? and (.currency | type == "string") and (.currency != "")) and
            (.reason? and (.reason | type == "string") and (.reason != "")) and
            (.from? and (.from | type == "string") and (.from != "")) and
            (.invoice_number? and (.invoice_number | type == "string") and (.invoice_number != "")) and
            (.date? and (.date | type == "string") and (.date != "")) and
            (.due? and (.due | type == "string") and (.due != ""))
          )
        ' < "$WORKDIR/invoice-sum-html.json" >/dev/null; then
          cat "$WORKDIR/invoice-sum-html.json"
        else
          notmuch show --entire-thread=false --include-html --format=json "$MSGID AND NOT attachment.pdf" | \
                    jq -r '.. | objects | select(."content-type"=="text/html") | .content' | \
                    html2text -nobs | tee -a /dev/stderr | \
                    aichat -r invoice-summer | tee -a /dev/stderr
        fi
      else
        echo "Show pdf" 1>&2
        PDFNAME="$(notmuch show --entire-thread=false --format=json "$MSGID AND attachment:pdf" | jq -r '.. | objects | select(."content-type"=="application/pdf") | .filename' | head -1)"
        echo "PDFNAME: $WORKDIR/$PDFNAME" 1>&2
        notmuch show --format=raw "$MSGID" | ripmime --no-nameless --overwrite -i - -d "$WORKDIR"
        PAGES="$(pdfinfo "$WORKDIR"/"$PDFNAME" | grep ^Pages: | awk '{print $2}')"
        for i in $(seq 1 "$PAGES"); do
          pdftotext -layout -f "$i" -l "$i" "$WORKDIR"/"$PDFNAME" - | aichat -r invoice-summer | tee -a /dev/stderr > "$WORKDIR/invoice-sum-$i.json"
          if jq -e '. |
            select(
              (.amount? | type == "number") and
              (.confidence? | type == "number") and
              (.currency? and (.currency | type == "string") and (.currency != "")) and
              (.reason? and (.reason | type == "string") and (.reason != "")) and
              (.from? and (.from | type == "string") and (.from != "")) and
              (.invoice_number? and (.invoice_number | type == "string") and (.invoice_number != "")) and
              (.date? and (.date | type == "string") and (.date != "")) and
              (.due? and (.due | type == "string") and (.due != ""))
            )
          ' < "$WORKDIR/invoice-sum-$i.json" >/dev/null; then
            cat "$WORKDIR/invoice-sum-$i.json"
            break
          fi
        done
      fi
    '';
  };
  tagMessage = pkgs.writeShellApplication {
    name = "tag-message";
    runtimeInputs = with pkgs; [procmail pandoc jq];
    text = ''
      # shellcheck disable=SC1091
      source ${common}

      MSGID="''${1:-}"

      if [ -z "$MSGID" ]; then
        echo Please specify the MSGID as the first argument
        exit 1
      fi

      JQFLATTEN=". | flatten | map(select(. != null))[0]"

      echo "Categorizing $MSGID"

      SUBJECT="$(notmuch show --entire-thread=false --include-html --format json "$MSGID" | \
                 jq -r "$JQFLATTEN.headers.Subject")"
      FROM="$(notmuch show --entire-thread=false --include-html --format json "$MSGID" | \
                 jq -r "$JQFLATTEN.headers.From")"
      TO="$(notmuch show --entire-thread=false --include-html --format json "$MSGID" | \
                 jq -r "$JQFLATTEN.headers.To")"
      CONTENT_TYPE="$(notmuch show --entire-thread=false --include-html --format json "$MSGID" | \
                 jq -r "$JQFLATTEN.body[0].\"content-type\"")"

      echo "SUBJECT: $SUBJECT"
      echo "FROM: $FROM"
      echo "TO: $TO"
      echo "CONTENT_TYPE: $CONTENT_TYPE"

      if [ "$CONTENT_TYPE" = "text/plain" ]; then
        OUTPUT="$( (
                echo "SUBJECT: $SUBJECT"
                echo "FROM: $FROM"
                echo "TO: $TO"
                notmuch show --entire-thread=false --format json "$MSGID" | \
                jq -r "$JQFLATTEN.body[0].content" | \
                grep -oE '[^[:space:]]+' | grep -oE '[[:alnum:]]+' | tr '[:space:]' ' ' | tr -s ' ' | \
                grep -oP '^.{0,1200}') | \
               aichat -r email-tagger | tee /dev/stderr)"
      else
        OUTPUT="$( (
               echo "SUBJECT: $SUBJECT"
               echo "FROM: $FROM"
               echo "TO: $TO"
               notmuch show --entire-thread=false --include-html --format json "$MSGID" | \
               jq -r "$JQFLATTEN.body[0].content" | pandoc -f html -t plain - | \
               grep -oE '[^[:space:]]+' | grep -oE '[[:alnum:]]+' | tr '[:space:]' ' ' | tr -s ' ' | \
               grep -oP '^.{0,1200}') | \
              aichat -r email-tagger | tee /dev/stderr)"
      fi
      TAG="$(echo "$OUTPUT" | jq -r '.tag')"
      UNTAG="$(echo "$OUTPUT" | jq -r '[.untag[] | "-\(.)"] | join(" ")')"
      echo "TAG: $TAG"
      echo "UNTAG: $UNTAG"
      if [ -n "$TAG" ]; then
        if [ -n "$UNTAG" ]; then
          run "notmuch tag $UNTAG '$MSGID'"
        fi
        echo "tagging message $MSGID with tag $TAG"
        run "notmuch tag +$TAG '$MSGID'"

        if ! [[ "$TAG" == "Invoice" || "$TAG" == "Selfinvoice" ]]; then
          echo "Not an invoice or selfinvoice, skip extracting more information"
          exit
        fi
        if echo "$FROM" | grep -i -E '(john@insane|john@9000)' > /dev/null; then
          echo "Sent from me, skip extracting more information"
          exit
        fi
        if echo "$TO" | grep -i -E '30231_30365' > /dev/null; then
          echo "Sent to accounting already, skip extracting more information"
          exit
        fi
        if echo "$SUBJECT" | grep -i -E '^(Fwd|Re|Reply|Forward):' > /dev/null; then
          echo "This message is a follow-up or forward in a thread, skip extracting more information"
          exit
        fi
        FIRST_ID="$(notmuch show --format=json "$MSGID" | jq -r '.[] | flatten | .[0].id')"
        if [[ "id:$FIRST_ID" != "$MSGID" ]]; then
          echo "This message is not the first in a thread, skip extracting more information"
          exit
        fi
        echo "since this is a(n) $TAG, I will try to extract more information"
        JSON="$(${invoiceExtraction}/bin/invoice-extraction "$MSGID" | tee /dev/stderr)"
        if ! echo "$JSON" | jq -e . >/dev/null 2>&1; then
          echo "Could not extract invoice information, skip"
          echo "Untagging this with invoice, tagging with +TagFail +InvoiceFail"
          run "notmuch tag +Important +TagFail +InvoiceFail -$TAG '$MSGID'"
          exit
        fi
        FILE="$(notmuch search --output=files "$MSGID" | head -1)"
        echo "FILE: $FILE"
        AMOUNT="$(echo "$JSON" | jq -r .amount)"
        CURRENCY="$(echo "$JSON" | jq -r .currency)"
        INVOICE_NUMBER="$(echo "$JSON" | jq -r .invoice_number)"
        DATE="$(echo "$JSON" | jq -r .date)"
        DUE="$(echo "$JSON" | jq -r .due)"
        run "formail -f -I 'X-Aim-Invoice-Amount: $AMOUNT' -I 'X-Aim-Invoice-Currency: $CURRENCY' -I 'X-Aim-Invoice-Number: $INVOICE_NUMBER' -I 'X-Aim-Invoice-Date: $DATE' -I 'X-Aim-Invoice-Due: $DUE' < '$FILE' > '$FILE'.tmp"
        run mv "'$FILE'.tmp '$FILE'"
        echo reindexing "$MSGID"
        run "notmuch reindex '$MSGID'"
      else
        echo "tagging message $MSGID with TagFail"
        run "notmuch tag +TagFail '$MSGID'"
      fi
    '';
  };
  emailTagger = pkgs.writeShellApplication {
    name = "email-tagger";
    text = ''
      SEARCH="''${1:-}"
      DRYRUN="''${DRYRUN:-}"

      if [ -z "$SEARCH" ]; then
        echo Please specify the search as the first argument
        exit 1
      fi

      for MSGID in $(notmuch search --output=messages "$SEARCH"); do
        ${tagMessage}/bin/tag-message "$MSGID" "$DRYRUN"
      done
    '';
  };
  postNew = pkgs.writeShellApplication {
    name = "post-new";
    text = ''
      # shellcheck disable=SC1091
      source ${common}
      run "notmuch tag +inbox +unread -- tag:new"
      ${emailTagger}/bin/email-tagger "tag:new"
      run "notmuch tag -new -- tag:new"
    '';
  };
  tags = ["Important" "Invoice" "Selfinvoice" "Jobs" "Social" "Newsletter" "Promotional" "Mailinglist" "General"];
  model = "ollama:mistral-small:24b";
  temperature = "0.05";
in {
  home.packages = with pkgs; [
    protonmail-bridge
    gnupg
    just
    pass
    muchsync
  ];

  xdg.configFile."aichat/roles/email-tagger.md".text = ''
    ---
    model: ${model}
    temperature: ${temperature}
    ---
    Your role is to tag and categorize email. Output the result in JSON format with the keys "confidence", "tag", "untag" and "reason".

    The tags you may choose from are:
    ${lib.concatStringsSep ", " tags}

    You should only pick one tag. The one that best fits the message.

    Untag should always contain a list of all possible tags.

    You will state your confidence in your tag selection with a number between 0 - 10, 0 being no confidence, 10 being complete confidence.
    You will also state the reason for choosing the tag you picked.

    When something is a receipt rather than an Invoice, tag it as an Invoice.
    When something is an issue or pull request from github, it is always a Mailinglist.
    When something is from Wint, please mark it as Important.
    When something is from Fieldglass, please mark it as Important.
    When something is about a direct message from someone on LinkedIn, mark it as Important.
    When something is about a failure to run a script or some process failed, it should be marked as Important.

    When something seems to be an Invoice, but mentions "Self-billing" or "Självfaktura", it's a Selfinvoice. These come from emagine or Red Road AB.
    Please note that "elfaktura" means electricity bill. Those are NOT selfinvoices.

    DO NOT wrap the output in ```json markdown formatting. Output raw JSON only, like in this example:

    {
        "tag": "Invoice",
        "untag": ${builtins.toJSON tags}
        "confidence": 8,
        "reason": "The email contains an attachment and the content states that it is an invoice. So it be tagged Invoice."
    }

    What follows are correct outputs based on inputs:

    ### INPUT
    Subject: Meeting Reminder

    Hi Team,
    This is a reminder about our upcoming meeting scheduled for tomorrow at 10 AM. Please make sure to review the agenda attached.
    Best regards,
    John Doe

    ### OUTPUT
    {
      "tag": "Important",
      "untag": ${builtins.toJSON tags}
      "confidence": 9,
      "reason": "This seems to be a reminder of a meeting tomorrow at 10 AM. This is important."
    }

    ### INPUT
    Subject: Your invoice from Amazon

    Dear customer,

    Attached is the invoice for the previous months AWS usage. The invoice amount is $4.92.

    Regards,
    Jane Smith

    ### OUTPUT
    {
      "tag": "Invoice",
      "untag": ${builtins.toJSON tags}
      "confidence": 8,
      "reason": "This seems to be an invoice from AWS. This should be tagged Invoice."
    }
  '';

  xdg.configFile."aichat/roles/invoice-summer.md".text = ''
    ---
    model: ${model}
    temperature: ${temperature}
    ---
    Your role is to determine the total of invoices and receipts. You need to determine if an invoice total sum is 0 or more. Output the result in JSON format with the keys "confidence", "amount", "currency", "reason", "invoice_number", "date", "due" and "from".
    Always translate a symbol currency, like $, to letters in the currency field - like $ becomes USD for example. In the reason field, please keep the original currency symbol or letters as they were.
    Amount and Confidence should both be numbers.
    If it looks like the invoice is from 9000, look again and pick the other option as there should be another company mentioned.
    There should be an invoice_number (or "fakturanummer" in Swedish) somewhere. Put that in the invoice_number field in the json output.
    The date field should contain the date when the invoice was issued. In Swedish "Fakturadatum" or "Datum" for example. In English it could be Date, Document Date, Invoice Date or similar.
    The due date should contain the date when the invoice is due. In Swedish it might be "Förfallodatum" or "Oss tillhanda". In English it could be Due Date, Payment Date or similar.
    The due date may be missing sometimes (on receipts for example), in that case just enter the same in due as in date.
    Please note that the due date will ALWAYS be at least the same day or later than the date.
    Dates should always be formatted to look like this: 2025-01-05.

    Do not deviate from the the structure of the output as exampled below. This is important.

    Please double check your output so it really does match what you find in the input text you get. If you are unsure about something, please do not make something up.

    DO NOT wrap the output in ```json markdown formatting. Output raw JSON only, like in these examples:

    ### INPUT
    From the bar: Here is your receipt

    Date issued: 2025-01-20
    Date due: 2025-02-03

    Receipt id: xh88ehhcd

    Food: 0.0 SEK
    Drink: 0.0 SEK

    Total: 0 SEK

    ### OUTPUT
    {
      "amount": 0,
      "currency": "SEK",
      "confidence": 10,
      "reason": "The total of this receipt is 0 SEK.",
      "from": "The bar",
      "invoice_number": "xh88ehhcd",
      "date": "2025-01-20",
      "due": "2025-02-03"
    }


    ### INPUT
    Invoice: Here is your DigitalOcean invoice

    Inv. no: fuee8866d

    Cloud Storage: $10.3
    Compute: $0.1
    Database: $0

    Total: $10.4

    Date: 2025-01-04
    Due: 2025-01-05

    ### OUTPUT
    {
      "amount": 10.4,
      "currency": "USD",
      "confidence": 10,
      "reason": "The total of this receipt is $10.4.",
      "from": "Digital Ocean",
      "invoice_number": "fuee8866d",
      "date": "2025-01-20",
      "due": "2025-02-03"
    }

    ### INPUT
    Invoice: Here is your Hetzner Online Gmbh invoice

    Date: 2022-10-15
    Due: 2022-11-14

    ID: 982wjdshh

    Cloud Storage: €10.3
    Compute: €0.1
    Database: €0

    Total: $10.4

    ### OUTPUT
    {
      "amount": 10.4,
      "currency": "EUR",
      "confidence": 10,
      "reason": "The total of this receipt is €10.4.",
      "from": "Digital Ocean",
      "invoice_number": "982wjdshh",
      "date": "2022-10-15",
      "due": "2022-11-14"
    }
  '';

  programs.notmuch = {
    enable = true;
    new = {
      tags = ["new"];
      ignore = [
        "/.devenv.*$/"
        "/aerc.*$/"
        "/dot.*$/"
        "/rust.*$/"
        "/.*.sh$/"
        "Justfile"
        "/.*.md$/"
        "/not.*$/"
        "wint"
        ".gitignore"
        "/devenv.*$/"
        "/email.*$/"
        "/general.*$/"
        "mbsyncrc"
        "notmuch-config"
        "post-new"
        "tag-message"
        ".envrc"
        "msmtprc"
        "invoice-extraction"
        "9000-soverin"
        "protonmail-bridge"
      ];
    };
    maildir.synchronizeFlags = false;
    hooks.postNew = ''
      ${postNew}/bin/post-new
    '';
    extraConfig = {
      user.other_email = "john@insane.se";
      index = {
        "header.InvoiceAmount" = "X-Aim-Invoice-Amount";
        "header.InvoiceCurrency" = "X-Aim-Invoice-Currency";
        "header.InvoiceNumber" = "X-Aim-Invoice-Number";
        "header.InvoiceDate" = "X-Aim-Invoice-Date";
        "header.InvoiceDue" = "X-Aim-Invoice-Due";
      };
    };
  };

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  services.mbsync = {
    enable = true;
    postExec = ''
      ${pkgs.notmuch}/bin/notmuch new
    '';
  };

  systemd.user.services.proton-bridge = {
    Unit.Description = "Run the proton-bridge";
    Service.ExecStart = "${pkgs.protonmail-bridge}/bin/protonmail-bridge -n";
    Service.Environment = [
      "PATH=${pkgs.gnupg}/bin:${pkgs.pass}/bin:${pkgs.protonmail-bridge}/bin"
      "GNUPGHOME=/home/${adminUser.name}/Mail/protonmail-bridge/gnupg"
      "XDG_CACHE_HOME=/home/${adminUser.name}/Mail/protonmail-bridge/cache"
      "XDG_DATA_HOME=/home/${adminUser.name}/Mail/protonmail-bridge/local/data"
      "XDG_CONFIG_HOME=/home/${adminUser.name}/Mail/protonmail-bridge/local/config"
      "PASSWORD_STORE_DIR=/home/${adminUser.name}/Mail/protonmail-bridge/password-store"
    ];
  };
}
