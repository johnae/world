#!/usr/bin/env gawk -f
# awk script for converting an iCal formatted file to a sequence of org-mode headings.
# this may not work in general but seems to work for day and timed events from Google's
# calendar, which is really all I need right now...
#
# usage:
#   awk -f THISFILE < icalinputfile.ics > orgmodeentries.org
#
# Note: change org meta information generated below for author and
# email entries!
#
# Caveats:
#
# - date entries with no time specified are assumed to be local time zone;
#   same remark for date entries that do have a time but do not end with Z
#   e.g.: 20130101T123456 is local and will be kept as 2013-01-01 12:34
#   where 20130223T123422Z is UTC and will be corrected appropriately
#
# - UTC times are changed into local times, using the time zone of the
#   computer that runs the script; it would be very hard in an awk script
#   to respect the time zone of a file belonging to another time zone:
#   the offsets will be different as well as the switchover time(s);
#   (consider a remote shell to a computer with the file's time zone)
#
# - the UTC conversion entirely relies on the built-in strftime method;
#   the author is not responsible for any erroneous conversions nor the
#   consequence of such conversions
#
# - does process RRULE recurring events, but ignores COUNT specifiers
#
# - does not process EXDATE to exclude date(s) from recurring events
#
# Eric S Fraga
# 20100629 - initial version
# 20100708 - added end times to timed events
#          - adjust times according to time zone information
#          - fixed incorrect transfer for entries with ":" embedded within the text
#          - added support for multi-line summary entries (which become headlines)
# 20100709 - incorporated time zone identification
#          - fixed processing of continuation lines as Google seems to
#            have changed, in the last day, the number of spaces at
#            the start of the line for each continuation...
#          - remove backslashes used to protect commas in iCal text entries
# no further revision log after this as the file was moved into a git
# repository...
#
# Updated by: Guido Van Hoecke <guivhoATgmailDOTcom>
# Last change: 2013.05.26 14:28:33
#----------------------------------------------------------------------------------

BEGIN {
    ### config section

    attending_types["UNSET"] = 0;
    attending_types["ATTENDING"] = 1;
    attending_types["NEEDS_ACTION"] = 2;
    attending_types["NOT_ATTENDING"] = 3;
    attending_types[0] = "UNSET";
    attending_types[1] = "ATTENDING";
    attending_types[2] = "NEEDS_ACTION";
    attending_types[3] = "NOT_ATTENDING";

    # map of UIDS for duplicate checking -- sometimes the same id comes down
    # with multiple VEVENTS
    UIDS[0];

    # map of people attending a given event
    people_attending[0];

    # maximum age in days for entries to be output: set this to -1 to
    # get all entries or to N>0 to only get enties that start or end
    # less than N days ago
    max_age = 7;

    # set to 1 or 0 to yes or not output a header block with TITLE,
    # AUTHOR, EMAIL etc...
    header = 1;

    # set to 1 or 0 to yes or not output the original ical preamble as
    # comment
    preamble = 1;

    # set to 1 to output time and summary as one line starting with
    # the time (value 1) or to 0 to output the summary as first line
    # and the date and time info as a later line (after the property
    # drawer or org complains)
    condense = 0;

    # set to 1 or 0 to yes or not output the original ical entry as a
    # comment (mostly useful for debugging purposes)
    original = 1;

    # google truncates long subjects with ... which is misleading in
    # an org file: it gives the unfortunate impression that an
    # expanded entry is still collapsed; value 1 will trim those
    # ... and value 0 doesn't touch them
    trimdots = 1;

    # change this to your name
    author = ENVIRON["AUTHOR"] != "" ? ENVIRON["AUTHOR"] : "Marc Sherry"

    # and to your email address
    emailaddress = ENVIRON["EMAIL"] != "" ? ENVIRON["EMAIL"] : "unknown"

    # calendar/category name for display in org-mode
    calendarname = ENVIRON["CALENDAR"] != "" ? ENVIRON["CALENDAR"] : "unknown"

    # any tags for this calendar (e.g. "WORK" or "PERSONAL")
    filetags = ENVIRON["FILETAGS"] != "" ? ENVIRON["FILETAGS"] : "unknown"

    # timezone offsets
    # TODO: this is stupid
    tz_offsets["America/Los_Angeles"] = 0
    tz_offsets["America/Chicago"] = 2

    ### end config section

    # use a colon to separate the type of data line from the actual contents
    FS = ":";

    # we only need to preserve the original entry lines if either the
    # preamble or original options are true
    preserve = preamble || original
    first = 1;      # true until an event has been found
    max_age_seconds = max_age*24*60*60

    if (header) {
        print "#+TITLE:       Main Google calendar entries"
        print "#+AUTHOR:     ", author
        print "#+EMAIL:      ", emailaddress
        print "#+DESCRIPTION: converted using the ical2org awk script"
        print "#+CATEGORY:   ", calendarname
        print "#+STARTUP:     hidestars"
        print "#+STARTUP:     overview"
        print "#+FILETAGS:   ", filetags
        print ""
    }
}

# continuation lines (at least from Google) start with a space. If the
# continuation is after a processed field (description, summary, attendee,
# etc.) append the entry to the respective variable
/^[ ]/ {
    if (indescription) {
        entry = entry gensub("\r", "", "g", gensub("^[ ]", "", 1, $0));
        # print "entry continuation: " entry
    } else if (insummary) {
        summary = summary gensub("\r", "", "g", gensub("^[ ]", "", 1, $0))
        # print "summary continuation: " summary
    } else if (inattendee) {
        attendee = attendee gensub("\r", "", "g", gensub("^[ ]", "", 1, $0))
        # print "attendee continuation: " attendee
        are_we_going(attendee)
        add_attendee(attendee)
    } else if (inlocation) {
        location = location unescape(gensub("\r", "", "g", $0), 0);

    }
    if (preserve)
        icalentry = icalentry "\n" $0
}

/^BEGIN:VEVENT/ {
    # start of an event: initialize global values used for each event
    date = "";
    entry = ""
    headline = ""
    icalentry = ""  # the full entry for inspection
    id = ""
    indescription = 0;
    insummary = 0
    inattendee = 0
    inlocation = 0
    in_alarm = 0
    got_end_date = 0
    attending = attending_types["UNSET"];
    # http://unix.stackexchange.com/a/147958/129055
    intfreq = "" # the interval and frequency for repeating org timestamps
    lasttimestamp = -1;
    location = ""
    rrend = ""
    status = ""
    summary = ""
    attendee = ""
    delete people_attending;

    # if this is the first event, output the preamble from the iCal file
    if (first) {
        if(preamble) {
            print "* COMMENT original iCal preamble"
            print gensub("\r", "", "g", icalentry)
        }
        if (preserve)
            icalentry = ""
        first = 0;
    }
}

# any line that starts at the left with a non-space character is a new data field

/^BEGIN:VALARM/ {
    # alarms have their own UID, DESCRIPTION, etc. We don't want these polluting the real fields
    in_alarm = 1
}

/^END:VALARM/ {
    in_alarm = 0
}

/^[A-Z]/ {
    # we do not copy DTSTAMP lines as they change every time you download
    # the iCal format file which leads to a change in the converted
    # org file as I output the original input.  This change, which is
    # really content free, makes a revision control system update the
    # repository and confuses.
    if (preserve)
        if (! index("DTSTAMP", $1))
            icalentry = icalentry "\n" $0
    # this line terminates the collection of description and summary entries
    indescription = 0;
    insummary = 0;
    inattendee = 0;
}

# this type of entry represents a day entry, not timed, with date stamp YYYYMMDD

/^DTSTART;VALUE=DATE/ {
    date = datestring($2);
}

/^DTEND;VALUE=DATE/ {
    got_end_date = 1
    end_date = datestring($2, 1);
    if ( issameday )
        end_date = ""
}


# this represents a timed entry with date and time stamp YYYYMMDDTHHMMSS
# we ignore the seconds
/^DTSTART[:;][^V]/ {
    tz = "";
    match($0, /TZID=([^:]*)/, a)
    {
        tz = a[1];
    }
    offset = tz_offsets[tz]

    date = datetimestring($2, offset);
    # print date;

    if (date != "" && got_end_date) {
        fix_date_time()
    }
}

# and the same for the end date;

/^DTEND[:;][^V]/ {
    # NOTE: this doesn't necessarily appear after DTSTART
    tz = "";
    match($0, /TZID=([^:]*)/, a)
    {
        tz = a[1];
    }
    offset = tz_offsets[tz]

    end_date = datetimestring($2, offset);
    got_end_date = 1

    if (date != "" && got_end_date) {
        # We got start and end date/time, let's munge as appropriate
        fix_date_time()
    }
}

# repetition rule

/^RRULE:FREQ=(DAILY|WEEKLY|MONTHLY|YEARLY)/ {
    # TODO: handle BYDAY values for events that repeat weekly for multiple days
    # (e.g. a "Gym" event)

    # get the d, w, m or y value
    freq = tolower(gensub(/.*FREQ=(.).*/, "\\1", 1, $0))
    # get the interval, and use 1 if none specified
    interval =  $2 ~ /INTERVAL=/ ? gensub(/.*INTERVAL=([0-9]+);.*/, "\\1", 1, $2) : 1
    # get the enddate of the rule and use "" if none specified
    rrend = $2 ~ /UNTIL=/ ? datestring(gensub(/.*UNTIL=([0-9]{8}).*/, "\\1", 1, $2)) : ""
    rrend_raw = $2 ~ /UNTIL=/ ? gensub(/.*UNTIL=([0-9]{8}).*/, "\\1", 1, $2) : ""
    repeat_count = $2 ~ /COUNT=/ ? gensub(/.*COUNT=([0-9]+).*/, "\\1", 1, $2) : ""
    # build the repetitor vale as understood by org
    intfreq =  " +" interval freq
    # if the repetition is daily, and there is an end date, drop the repetitor
    # as that is the default
    if (intfreq == " +1d" && end_date == "" && rrend != "")
        intfreq = ""
    now = strftime("%Y%m%dT%H%M%SZ")
    if (rrend_raw != "" && rrend_raw < now)
        intfreq = ""
    if (repeat_count != "")      # TODO: count repeats correctly
        intfreq = ""
}

# The description will the contents of the entry in org-mode.
# this line may be continued.

/^DESCRIPTION/ {
    if (!in_alarm) {
        # Setting $1 to "" clears colons from items like "1:1 with Marc", so we
        # strip "DESCRIPTION:" off of the front instead
        # $1 = "";
        entry = entry gensub("\r", "", "g", gensub(/^DESCRIPTION:/, "", 1, $0));
        indescription = 1;
    }
}

# the summary will be the org heading

/^SUMMARY/ {
    # Setting $1 to "" clears colons from items like "1:1 with Marc", so we
    # strip "SUMMARY:" off of the front instead
    summary = gensub("\r", "", "g", gensub(/^SUMMARY:/, "", 1, $0));

    # trim trailing dots if requested by config option
    if(trimdots && summary ~ /\.\.\.$/)
        sub(/\.\.\.$/, "", summary)
    insummary = 1;
    # print "Summary: " summary
}

# the unique ID will be stored as a property of the entry

/^UID/ {
    if (!in_alarm) {
        id = gensub("\r", "", "g", $2);
    }
}

/^LOCATION/ {
    location = unescape(gensub("\r", "", "g", $2), 0);
    inlocation = 1;
    # print "Location: " location
}

/^STATUS/ {
    status = gensub("\r", "", "g", $2);
    # print "Status: " status
}

/^ATTENDEE/ {
    attendee = gensub("\r", "", "g", $0);
    inattendee = 1;
    # print "Attendee: " attendee
}

# when we reach the end of the event line, we output everything we
# have collected so far, creating a top level org headline with the
# date/time stamp, unique ID property and the contents, if any

/^END:VEVENT/ {
    #output event
    # print "max_age: " max_age
    # print "lasttimestamp: " lasttimestamp
    # print "lasttimestamp+max_age_seconds: " lasttimestamp+max_age_seconds
    # print "systime(): " systime()

    is_duplicate = (id in UIDS);
    if(is_duplicate == 0 && (max_age<0 || intfreq != "" || ( lasttimestamp>0 && systime()<lasttimestamp+max_age_seconds )) )
    {
        if (attending != attending_types["NOT_ATTENDING"]) {
            # build org timestamp
            if (intfreq != "")
                date = date intfreq
            # TODO: http://orgmode.org/worg/org-faq.html#org-diary-class
            else if (end_date != "")
                date = date ">--<" end_date
            else if (rrend != "")
                date = date ">--<" rrend

            # translate \n sequences to actual newlines and unprotect commas (,)
            if (condense)
                print "* <" date "> " gensub("^[ ]+", "", "", unescape(summary, 0))
            else
                print "* " gensub("^[ ]+", "", "g", unescape(summary, 0))
            print "  :PROPERTIES:"
            print     "  :ID:        " id
            if(length(location))
                print "  :LOCATION:  " location
            if(length(status))
                print "  :STATUS:    " status
            attending_string = attending_types[attending]
            print "  :ATTENDING: " attending_string
            print "  :ATTENDEES: " join_keys(people_attending)
            print "  :END:"
            if (date2 != "")
            {
                # Fake some logbook entries so we can generate a clock report
                print "  :LOGBOOK:"
                print "  CLOCK: [" date1 "]--[" date2 "] =>  " "0:00"
                print "  :END"
            }
            if (!condense)
                 print "<" date ">"

            print ""
            if(length(entry)>1)
                print gensub("^[ ]+", "", "g", unescape(entry, 1));

            # output original entry if requested by 'original' config option
            if (original)
                print "** COMMENT original iCal entry\n", gensub("\r", "", "g", icalentry)
        }
        UIDS[id] = 1;
    }
}


# Join keys in an array, return a string
function join_keys(input)
{
    joined = "";
    first_key = 1;
    for (key in input)
    {
        if (first_key != 1)
            joined = joined ", "
        joined = joined key
        first_key = 0;
    }
    return joined;
}


# unescape commas, newlines, etc. newlines are optionally converted to just
# spaces -- it's good to preserve them in descriptions for e.g. interview
# calendar events, but addresses look better with spaces as more info fits on a
# line
function unescape(input, preserve_newlines)
{
    ret = gensub("\\\\,", ",", "g",
                 gensub("\\\\;", ";", "g", input))
    if (preserve_newlines)
        ret = gensub("\\\\n", "\n", "g", ret)
    else
        ret = gensub("\\\\n", " ", "g", ret)
    return ret
    # return gensub("\\\\,", ",", "g",
    #               gensub("\\\\n", " ", "g",
    #                       gensub("\\\\;", ";", "g", input)))
}


# funtion to convert an iCal time string 'yyyymmddThhmmss[Z]' into a
# date time string as used by org, preferably including the short day
# of week: 'yyyy-mm-dd day hh:mm' or 'yyyy-mm-dd hh:mm' if we cannot
# define the day of the week

function datetimestring(input, offset)
{
    # print "________"
    # print "input : " input
    # convert the iCal Date+Time entry to a format that mktime can understand
    spec  = match(input, "([0-9]{4})([0-9]{2})([0-9]{2})T([0-9]{2})([0-9]{2})([0-9]{2}).*[\r]*", a);
    year = a[1]
    month = a[2]
    day = a[3]
    hour = a[4]
    min = a[5]
    sec = a[6]
    # print "spec :" spec

    if (offset > 0)
    {
        hour -= offset
    }

    # print "input: " input
    # print "datetime: " year" "month" "day" "hour" "min" "sec
    stamp = mktime(year" "month" "day" "hour" "min" "sec);
    lasttimestamp = stamp;

    if (stamp <= 0) {
        # this is a date before the start of the epoch, so we cannot
        # use strftime and will deliver a 'yyyy-mm-dd hh:mm' string
        # without day of week; this assumes local time, and does not
        # attempt UTC offset correction
        spec = gensub("([0-9]{4})([0-9]{2})([0-9]{2})T([0-9]{2})([0-9]{2})([0-9]{2}).*[\r]*", "\\1-\\2-\\3 \\4:\\5", "g", input);
        # print "==> spec:" spec;
        return spec;
    }

    if (input ~ /[0-9]{8}T[0-9]{6}Z/ ) {
        # this is an utc time;
        # we need to correct the timestamp by the utc offset for this time
        offset = strftime("%z", stamp)
        pm = substr(offset,1,1) 1 # define multiplier +1 or -1
        hh = substr(offset,2,2) * 3600 * pm
        mm = substr(offset,4,2) * 60 * pm

        # adjust the timestamp
        stamp = stamp + hh + mm
    }

    return strftime("%Y-%m-%d %a %H:%M", stamp);
}

# function to convert an iCal date into an org date;
# the optional parameter indicates whether this is an end date;
# for single or multiple whole day events, the end date given by
# iCal is the date of the first day after the event;
# if the optional 'isenddate' parameter is non zero, this function
# tries to reduce the given date by one day

function datestring(input, isenddate)
{
    #convert the iCal string to a an mktime input string
    spec = gensub("([0-9]{4})([0-9]{2})([0-9]{2}).*[\r]*", "\\1 \\2 \\3 00 00 00", "g", input);

    # compute the nr of seconds after or before the epoch
    # dates before the epoch will have a negative timestamp
    # days after the epoch will have a positive timestamp
    stamp = mktime(spec);

    if (isenddate) {
        # subtract 1 day from the timestamp
        # note that this also works for dates before the epoch
        stamp = stamp - 86400;

        # register whether the end date is same as the start date
        issameday = lasttimestamp == stamp
    }
    # save timestamp to allow for check of max_age
    lasttimestamp = stamp

    if (stamp < 0) {
        # this date is before the epoch;
        # the returned datestring will not have the short day of week string
        # as strftime does not handle negative times;
        # we have to construct the datestring directly from the input
        if (isenddate) {
            # we really should return the date before the input date, but strftime
            # does not work with negative timestamp values; so we can not use it
            # to obtain the string representation of the corrected timestamp;
            # we have to return the date specified in the iCal input and we
            # add time 00:00 to clarify this
            return spec = gensub("([0-9]{4})([0-9]{2})([0-9]{2}).*[\r]*", "\\1-\\2-\\3 00:00", "g", input);
        } else {
            # just generate the desired representation of the input date, without time;
            return gensub("([0-9]{4})([0-9]{2})([0-9]{2}).*[\r]*", "\\1-\\2-\\3", "g", input);
        }
    }

    # return the date and day of week
    return strftime("%Y-%m-%d %a", stamp);
}

# Add the current attendee's response to a set, so we can list who's going
# and who's declined
function add_attendee(attendee)
{
    match(attendee, /CN=([^;]+)/, m)
    {
        CN = tolower(m[1]);
        people_attending[CN] = 1;
    }
}

function fix_date_time()
{
    if (substr(date,1,10) == substr(end_date,1,10)) {
        # timespan within same date, use one date with a time range, but preserve
        # original dates for org-clocktable
        date1 = date
        date2 = end_date

        date = date "-" substr(end_date, length(end_date)-4)
        end_date = ""
    }
}

# Parse the current ATTENDEE line and see if it belongs to us. If so, check if
# we've accepted this calendar invite, and if so, set `attending` to True. It
# may be the case that there are no attendees (e.g. personal calendar items),
# and if that's the case, we'll leave `attending` unset. If there are attendees,
# we'll parse our status out and set `attending` appropriately.
function are_we_going(attendee)
{
    if (attending != attending_types["UNSET"])
    {
        # print "Bailing out early, attending is " attending
        return;
    }

    match(attendee, /CN=([^;]+)/, m)
    {
        # CN's can optionally be surrounded by quotes (google calendar download
        # omits, apple calendar export includes them)
        CN = gensub("\"", "", "g", tolower(m[1]));
        # TODO: no hardcoding
        if (CN == tolower(author) || CN == tolower(emailaddress))
        {
            # This is us -- did we accept the meeting?
            if (attendee ~ /PARTSTAT=ACCEPTED/)
            {
                attending = attending_types["ATTENDING"];
            }
            else if (attendee ~ /PARTSTAT=NEEDS-ACTION/)
            {
                attending = attending_types["NEEDS_ACTION"];
            }
            else {
                attending = attending_types["NOT_ATTENDING"];
            }
        }
    }
    # print "are_we_going: " attending
}

# Local Variables:
# time-stamp-line-limit: 1000
# time-stamp-format: "%04y.%02m.%02d %02H:%02M:%02S"
# time-stamp-active: t
# time-stamp-start: "Last change:[ \t]+"
# time-stamp-end: "$"
# End:
