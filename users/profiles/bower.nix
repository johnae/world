{pkgs, ...}: let
  bower = pkgs.writeShellApplication {
    name = "bower";
    runtimeInputs = with pkgs; [
      pandoc
      w3m
      poppler_utils
    ];
    text = ''
      ${pkgs.notmuch-bower}/bin/bower "$*"
    '';
  };
in {
  home.packages = [bower];
  xdg.configFile."bower/bower.conf".source = pkgs.writeText "bower.conf" ''
    # Bower configuration file.
    #
    # This file belongs in ~/.config/bower/bower.conf or
    # $XDG_CONFIG_DIRS/bower/bower.conf, where $XDG_CONFIG_DIRS
    # is defined by the XDG Base Directory Specification.
    #
    # The lines beginning with '#' or ';' are treated as comment lines.

    # Values representing shell commands use shell-style quoting rules:
    #
    # - within single quotes, all characters represent themselves
    # - within double quotes, all characters represent themselves except for
    #   backslash which escapes the next character
    # - outside of quotes, backslash escapes the next character
    # - no escape sequences are supported, i.e. "\n" is the same as "n".
    #
    # "~" at the start of an unquoted word will be replaced with the value
    # of the HOME environment variable.
    #
    # If the first word of a command contains the unquoted and unescaped
    # string "ssh" then an additional level of shell quoting will be applied
    # to any arguments that bower adds to the command.

    #-----------------------------------------------------------------------------#

    [command]

        # How to run notmuch.
        # Set this if notmuch is not already on your PATH.
        #
        # I run bower locally but keep my mail and notmuch on a remote machine,
        # accessed via ssh.  For that, you may like to enable the OpenSSH
        # ControlMaster option, and also set ControlPersist to greater than 60
        # seconds (or your selected polling period).
        #
      notmuch = ssh john@icarus notmuch

        # How to edit a message.
        # The default is to use $EDITOR, or else "vi".
        #
      ; editor = vim '+set ft=mail'

        # Specifying an alt_html_filter allows to compose multipart/alternative
        # messages with bower. The filter will receive the text/plain part on
        # stdin and is meant to produce an equivalent text/html part on stdout.
        # If no filter is specified, bower will try to use pandoc.
        #
        # Some of the email's header values will be available to the filter
        # as environment variables. The following environment variables are
        # set by bower: MAIL_FROM, MAIL_TO, MAIL_CC, MAIL_BCC, MAIL_SUBJECT,
        # MAIL_REPLY_TO, MAIL_IN_REPLY_TO.
        #
      alt_html_filter = pandoc -f markdown -t html

        # When to use the alt_html_filter. Can be one of never, manual,
        # default or always. If either manual or default are chosen,
        # the filter can be toggled on or off on a per message basis by
        # pressing H.
        #
      use_alt_html_filter = always

        # Default command(s) to open a part.
        # An unquoted & suffix causes the command to run in the background.
        # Multiple commands can be added to the input history by separating them
        # with a semicolon.
        #
      ; open_part = xdg-open&

        # Default command(s) to open a URL.
        # An unquoted & suffix causes the command to run in the background.
        # Multiple commands can be added to the input history by separating them
        # with a semicolon.
        #
      ; open_url = xdg-open&

        # Default command(s) to pass thread or message IDs to using the '|'
        # command. Multiple commands can be added to the input history by
        # separating them with a semicolon.
        #
        # The executed command will be passed the thread or message IDs
        # on standard input, separated by spaces.
        #
      ; pipe_id = xclip

        # Command to execute after polling finds there are new unread messages
        # matching the current search terms in the index view.
        # The notification message is provided as an argument.
        #
      ; poll_notify = notify-send -i mail-message-new -c email.arrived -a Bower Bower

        # These keys are deprecated and should be moved to an [account.*] section.
        #
      ; sendmail =
      ; post_sendmail =

    #-----------------------------------------------------------------------------#

    # How to convert non-text parts to text using external commands.
    # Each command receives input on standard input and must output UTF-8.
    # The process will inherit these environment variables:
    #   PART_CONTENT_TYPE - content type of the part being filtered
    #   PART_CHARSET - charset of the part being filtered (if available)

    [filter]

        # Command to "dump" HTML as plain text.
        #
      text/html = w3m -dump -T text/html -O UTF-8 -o display_link_number=1
      ; text/html = lynx -dump -force-html -stdin -display-charset=utf-8

        # You can specify commands to filter other media types.
        # The following examples are not enabled by default.
        #
      application/pdf = pdftotext - -

    #-----------------------------------------------------------------------------#

    # How to send mail for one or more email accounts.
    # Each account is defined in a section called [account.NAME] where NAME is some
    # short name of your choosing.

    [account.default]

        # An account is selected by matching the From address on the message to
        # from_address value. Defaults to the combination of user.name and
        # user.primary_email from .notmuch-config.
        #
      from_address = John Axel Eriksson <john@insane.se>

        # One account can be designated as the default account.
        #
      default = true

        # How to send a message from this account.
        # The default is to use sendmail but I use msmtp.
        # The command should understand the "-t" option to read recipients
        # from the message headers itself.
        #
      ; sendmail = /usr/bin/sendmail -oi -oem
      sendmail = msmtp --account=insane

        # Command to execute after the sendmail command is successful.
        # By default, the sent message will be added to the maildir with
        # "notmuch insert" (see README.md for the folder), and tags applied
        # to the message.
        #
        # You can set the post_sendmail key to run another command instead of
        # "notmuch insert", or leave the value empty to do nothing.
        # Tags will then be applied to the sent message as a separate step after
        # the post_sendmail command.
        #
      post_sendmail =

    [account.9000]
      from_address = John Axel Eriksson <john@9000.dev>
      sendmail = msmtp --account=9000
      post_sendmail =

    #-----------------------------------------------------------------------------#

    [ui]

        # How often to check for new messages matching the current search
        # terms in the index view, or new messages in the current thread
        # in the thread view. Disable with "off".
        #
      ; poll_period_secs = 60

        # Automatically refresh the index view after this many seconds of
        # inactivity when there are new messages matching the current search terms.
        # Disable with "off".
        #
      ; auto_refresh_inactive_secs = off

        # Wrap lines in message bodies at this column, if the terminal is wider.
        #
      ; wrap_width =

        # Default thread ordering when viewing messages. May be "flat" or
        # "threaded". Default: "threaded". The ordering can be toggled by
        # pressing 'O' while viewing a thread of messages.
        #
      ; thread_ordering = threaded

        # Directory to save attachments into by default.
        #
      ; default_save_directory =

        # Cap search results at this number unless overridden. Default: 300.
        # Use "none" to disable this cap.
        #
      ; default_max_threads = 300

    #-----------------------------------------------------------------------------#

    [compose]

        # Append contents of this file when composing messages.
        # The path must be an absolute path; relative paths are not supported yet.
        # An initial ~/ expands to the home directory.
        #
      ; signature_file = ~/.signature

    #-----------------------------------------------------------------------------#

    [crypto]

        # Enable encryption by default when composing messages.
        #
      ; encrypt_by_default = false

        # Enable signing by default when composing messages.
        #
      ; sign_by_default = false

        # Attempt to decrypt messages when a thread is opened.
        #
      ; decrypt_by_default = false

        # Attempt to verify signatures when a thread is opened.
        #
      ; verify_by_default = false

    #-----------------------------------------------------------------------------#

    # Colours are defined in the [color] and [color.CONTEXT] sections.
    # The more-specific sections override keys in the generic section.
    #
    # Each value has the form:
    #   [attribute] [foreground] [/ background]
    #
    # attribute may be:
    #   normal, bold
    #
    # foreground and background may be:
    #   default, black, red, green, yellow, blue, magenta, cyan, white

    [color]
      ; current = bold yellow / red
      ; relative_date = bold blue
      ; selected = bold magenta
      ; standard_tag = normal
      ; flagged = bold red
      ; author = normal
      ; subject = normal
      ; other_tag = bold red
      ; field_name = bold red
      ; field_body = normal
      ; good_key = bold green
      ; bad_key = bold red

    [color.status]
      ; bar = white / blue
      ; info = bold cyan
      ; warning = bold red
      ; prompt = normal

    [color.pager]
      ; body = normal
      ; quote_odd = bold blue
      ; quote_even = green
      ; diff_common = normal
      ; diff_add = bold cyan
      ; diff_rem = bold red
      ; diff_hunk = bold yellow
      ; diff_index = bold green
      ; url = magenta
      ; part_head = bold magenta
      ; part_head_low = magenta
      ; part_message = magenta
      ; fold = magenta
      ; separator = bold blue
      ; obscured = bold black

    [color.index]
      ; count = green
      ; total = bold black

    [color.thread]
      ; tree = magenta
      ; author = normal
      ; subject = green
      ; obscured = bold black

    [color.compose]
      ; address = bold blue
      ; invalid = red

    #-----------------------------------------------------------------------------#

  '';
}
