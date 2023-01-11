APL\B5500

APL interpreter for the Burroughs B5500, written by Gary Kildall (of
CP/M fame), Leroy Smith, Sally Swedine, and Mary Zosel at the University
of Washington (Seattle, Washington US) in the early 1970s. This version
was modified by Jose Hernandez of Burroughs Corporation for operation
under the standard Burroughs Timesharing MCP (TSSMCP).

APL-IMAGE.alg_m
    The source of the APL interpreter, written in Burroughs B5500
    Extended Algol, dated "1-11-71" (1971-01-11) in the source. This was
    transcribed from a photocopy of a listing sent to Paul Kimpel by Ed
    Vandergriff of Chaska, Minnesota US. According to Ed:

        "Originally this came from a long-ago colleague, George P.
        Williams (then of Huntsville, AL [Alabama US]) who shared my
        interests in computer architecture and language implementation;
        if I recall correctly he encountered it as a student at Georgia
        Tech."

    A scan of that listing is available from:
    http://bitsavers.org/pdf/georgiaTech/APL-B5500-Listing-19710111.pdf

    Transcription was performed jointly by Hans Pufal of Angouleme,
    France and Fausto Saporito of Naples, Italy during August-September
    2013. Proofreading, corrections, and application of original
    sequence numbers (the last digits of which are often obscured on the
    listing) was done in late 2016 by Richard Cornwell of Durham, North
    Carolina US.

    In May 2018, three CUBE (Burroughs user organization) Library tapes
    were successfully read by the Computer History Museum in Mountain
    View, California US, and made available to a few B5500 enthusiasts.
    On the CUBEA13 tape is the file APL/L200013 (creation date
    1971-03-08), which appears to be a slightly later version of the
    program, dated 1971-03-05 in the source. Paul Kimpel of San Diego,
    California US used this machine-readable version to correct left-
    margin indentation, last digits of sequence numbers, and numerous
    typographical errors that had escaped detection during proofreading.

APL-IMAGE-List.lst
    Compilation listing of APL-IMAGE.alg_m. This was compiled using Mark
    XIII Algol using the following deck:

        ?COMPILE 0APL/DISK ALGOL LIBRARY
        ?ALGOL STACK=1000
        ?ALGOL FILE NEWTAPE=NEWSYM/APL SERIAL
        ?DATA CARD
        $CARD LIST SINGLE NEW
            <source images from APL-IMAGE.alg_m
        ?END

    The "0APL" MFID is necessary in order to be able to execute the
    codefile under CANDE as APL/DISK.

APLPTCH.19710305.alg_m
    A patch deck containing the few differences between the 1971-01-11
    version of the source archived here and the 1971-03-05 version on
    the CUBEA13 library tape.

APLPTCH.L200014.alg_m
    A patch deck for the APL.L200013 source on the CUBEA13 library tape.
    This should be applied to APL-IMAGE.alg_m after the
    APLPTCH.19710305.alg_m patch above.

    The 1971-01-11 and 1971-03-05 versions both print two question marks
    ("??") at the end of each line of terminal output. These appear to
    represent a carriage-return/line-feed pair appended to the end of
    messages by the program, but translated to question marks by the
    TSSMCP before being sent to the terminal. This behavior appears to
    be something that was missed in the conversion of the APL program
    from the custom data communications handler used by the University
    of Washington at the time to the standard Burroughs TSSMCP. This
    patch removes these extra characters from output messages.

