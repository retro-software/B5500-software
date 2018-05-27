CUBE Library Version 13 (February 1972)

CUBE, the Cooperating Users of Burroughs Equipment, was a U.S. based
user organization that was active from the 1950s through the early
1990s. After the merger of Burroughs and Sperry Univac in 1986, CUBE and
USE (the Sperry user group) evenually merged to form the UNITE
organization (http://www.unite.org).

One of the functions CUBE performed during the 1960s and early '70s was
to maintain a library of programs for the B5000 and B5500 computer
systems. These programs were donated both by Burroughs and by CUBE
members. The library was freely available to CUBE members and Burroughs
support staff.

The library eventually grew to three reels of 7-track, odd parity
magnetic tape. Tapes for version 13 of the library were acquired several
years ago by Jim Haynes from the B5500 site at the University of
California at Santa Cruz and donated to the Computer History Museum in
Mountain View, California. The CHM was finally able to read these tapes
in May 2018, producing binary images in .tap (taput) format. For
information on the .tap format, see:

    http://simh.trailing-edge.com/docs/simh_magtape.pdf

The .tap format was not originally designed to support the even- and
odd-parity encoding available on 7-track tapes, and there is some
difference of opinion on how it should be used for 7-track images. These
images produced by the CHM do not record the parity. Each 6-bit
character frame is stored right-justified in 8-bit bytes with two
leading zero bits. The data is encoded in B5500 Internal Code (BIC). See
Appendix A in:

    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/
    1021326_B5500_RefMan_May67.pdf

The .tap format is not presently supported by my retro-b5500 emulator.
Using a program I wrote for the modern Unisys MCP systems, I converted
the .tap files to the .bcd format used by retro-b5500 and several
others. See /Unisys-Emode-Tools/TAPBCD.alg_m in this repo. For
information on the .bcd format, see:

    http://www.piercefuller.com/oldibm-shadow/tool.html

Both image formats are included in this repository. The tape images are
identified as follows:

    CUBE_LBR  labeled CASTC, 56-word (448-character) blocks, 2.8MB
    CUBEA13   labeled CUBEA13/FILE000, B5500 Library/Maintenance, 12.0MB
    CUBEB13   labeled CUBEB13/FILE000, B5500 Library/Maintenance, 10.4MB

CUBE_LBR is in CAST format. The tape label has a creation date of
1976-06-10, but that is probably the date the tape was last copied, and
it does not appear that any of the files had been updated since the late
1960s. These tapes are maintained by the standard B5500 utility MAKCAST/
DISK. The Algol and COBOL compilers understood this format and could
compile programs and include individual routines directly from CAST
tapes or a disk file. See the README in the CAST-Examples/ directory for
details.

Imaging of the CUBE_LBR tape detected errors in two areas of the tape.
The first error was in the PTS041A module, in the block containing
sequence numbers "KPKP0024"-"KPKP0028". These records are part of a
large comment. The second error was in the PTS051 module, in the block
containing sequence numbers "ANTP  11"-"ANTP  13", and including the two
blank records on either side of those sequence numbers. These records
occurred at the end of a large comment.

There were actually three blocks with errors in this second area of the
tape, but two appear to be duplicates of the middle one, probably
introduced by tape positioning errors during error retry, either when
the tape was written or when it was recently imaged. The two duplicated
blocks caused module boundaries to be offset by 10 records in all the
modules after that point.

Some research showed that almost identical comments were part of similar
routines in other modules on the tape. I was able to correct the .tap
image using a hex editor, drop the two duplicated blocks, and
reconstruct the corrupted records from the similar comments elsewhere on
the image. The .bcd image and extracted files were then generated from
that corrected .tap image.

CUBEA13 and CUBEB13 are standard B5500 Library/Maintenance tapes. These
two tapes were imaged without error. Their tape labels indicate creation
dates of 1972-08-07 and 1972-08-05 respectively, but none of the files
on these tapes has a creation date later than February 1972. Files from
these tapes can be loaded to disk using the ?LOAD and ?ADD control card
commands. See page 4-15ff in:

    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/
    1024916_B5500_B5700_OperMan_Sep68.pdf

The individual files have been extracted from these tape images and
converted to standard text file format in the Files/ subdirectory
discussed below. Directories of the files on each tape are also
described below.

CUBEA13 and CUBEB13 contain identical copies of an index file for the
library (CUBELIB/INDEX) and an Algol program that can sort and format
the index in multiple ways (CUBELST/Q000007). This program was used to
generate the text files CUBELIB.LIST.txt and CUBELIB.LIST-1.text
below. These files appear to match the following scans of
listings on bitsavers:

    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
    CUBE_13_Library_Feb72.pdf

    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
    CUBE_Library_Listing.pdf

The disk file system for the B5500 used a two-part name, the Multi-file
identifier (MFID) and the File Identifier (FID), written MFID/FID. In
the CUBE Library index, the MFID is the name of the program or package.
The FID is a seven-character string termed the CUBE ID. The first two
characters of this ID are a letter-number code denoting a classification
scheme. The remaining characters (usually numeric) are a unique value
within the classification code. The classifications are shown in
CUBELIB.LIST.txt.

Most files on the CUBEA13 and CUBEB13 tapes use the CUBE ID as the FID,
but quite a few of the files (especially data files) do not. These are
noted within the file descriptions in the index. To add to the
confusion, files on the CUBE_LBR CAST tape have only one name. The CUBE
ID, where available in the library index, had been appended as the FID
to the names of these in the Files/ subdirectory.

In preparing the files for this repository I noticed a number of
discrepancies between the files actually on the tape images and the
entries in the index:

    The following files are on CUBE_LBR but are not in the index:

        MRS115,  MRS117,  MRS125,  MRS138,  MSS002,
        ORS023,  ORS029,  ORS033,  ORS036,  PTS074,  URS046

    The following files are in the index but not on any tape:

        PTS061/T200023
        SYSX-O400001 (noted in the index as available separately)

    The following files have different names between CUBE_LBR and the
    index (note that several other files have a different names but they
    are identified as such in the index):

        DSS029 on tape is DS029/T300001 in index
        DSS030 on tape is DS030/T300002 in index
        DSS031 on tape is DS031/T300003 in index
        PTS024 on tape is PTS024R/E200008 in index

Library contents:

CAST-Examples/
    Card decks illustrating the MAKCAST/DISK program and use of CAST
    files for the CUBE_LBR tape image. See the embedded README file for
    details.

CUBE_LBR.bcd
    Image in .bcd format for the CUBE_LBR tape. CAST format.

CUBE_LBR.tap
    Image in .tap format for the CUBE_LBR tape. CAST format.

CUBE_LBR-Tape-Directory.txt
    Listing of the files on the CUBE_LBR tape. This was generated by the
    MAKCAST program. The 6-digit number to the left of the program name
    is the 1-relative record number on the tape where that program
    starts. The numbering starts with the fourth block on the tape,
    i.e., after the three directory blocks on the front of the tape.

CUBEA13.bcd
    Image in .bcd format for the CUBEA13 tape. B5500 Library/Maintenance
    format.

CUBEA13.tap
    Image in .tap format for the CUBEA13 tape. B5500 Library/Maintenance
    format.

CUBEA13-Tape-Directory.pdf
    Listing of the files on the CUBEA13 tape. This was generated from
    the .bcd image by a Javascript utility, see https://github.com/
    pkimpel/retro-b5500/blob/master/tools/B5500LibMaintDir.html.

CUBEB13.bcd
    Image in .bcd format for the CUBEB13 tape. B5500 Library/Maintenance
    format.

CUBEB13.tap
    Image in .tap format for the CUBEB13 tape. B5500 Library/Maintenance
    format.

CUBEB13-Tape-Directory.pdf
    Listing of the files on the CUBEB13 tape. This was generated as for
    the CUBEA13 directory.

CUBELIB.LIST.txt
    Index of the CUBE Library generated from the CUBELIB/INDEX file
    using the CUBELST/Q000007 program with ?COMMON=0 (the default). This
    listing is organized by the library's classification scheme.

CUBELIB.LIST-1.txt
    Index of the CUBE Library generated from the CUBELIB/INDEX file
    using the CUBELST/Q000007 program with ?COMMON=1. This listing is
    sorted by the CUBE ID (second file name).

Files/
    Individual files extracted from the three tape images. These are
    named as they are on the tapes, with the exception of the files from
    CUBE_LBR, which have had their CUBE ID appended as a second name
    where that ID is available in the library index.

    These are plain text files encoded in ASCII. The following
    substitutions were made for the five B5500 character glyphs that do
    not have ASCII equivalents:

        `~`  left-arrow (Algol assignment operator)
        `|`  small-cross (Algol multiply operator)
        `{`  less-than-or-equal operator
        `}`  greater-than-or-equal operator
        `!`  not-equal operator

    On the B5500, these files would be named MFID/FID. In Windows and
    Unix-like file systems, however, treating the MFID as a directory
    name would result in a large number of directories containing a
    single file. This would make the library a little tedious to
    navigate, and would make it more difficult to find files by their
    CUBE ID in the second name. Therefore, the text files have been
    named in the form MFID-FID.ext, where ".ext" attempts to identify
    the files by type:

        .alg    B5500 Extended Algol
        .cob    B5500 COBOL (not COBOL-68)
        .dat    Data or text in ASCII (no binary encoding)
        .for    B5500 FORTRAN IV
        .gtl    GTL (Georgia Tech Language)
        .mca    Westinghouse Research MCALGOL
        .sno    SNOBOL
        .wipl   WIPL (Wisconsin Interactive Problem-Solving Language)
        .xla    B5500 XALGOL (Compatible Algol)

    Several files were transcribed earlier by Richard Cornwell and me,
    before these tape images became available, and were included
    elsewhere in this repository. These transcriptions have now been
    renamed and moved "underneath" the corresponding files extracted
    from the tape images so that they appear as versioned history to the
    tape files. The renamed files are:

        /KRUNCH-UNKRNCH/KRUNCH.UTILITY.alg_m => KRUNCH-V0104AA.alg
        /KRUNCH-UNKRNCH/UNKRNCH.UTILITY.alg_m => UNKRNCH-V0107AA.alg
        /RC-Ron-Brody/RCSY94.RON.alg_m => RCSY94-Z100006.alg
        /RC-Ron-Brody/TEACHER.0000094.txt_m => TEACHER-0000094.dat
        /SNOBOL-UW-Chambers/SYMBOL.SNOBOL.alg_m => SNOBOL-L200010.alg

__________
2018-05-27 Paul Kimpel
    Initial creation of the library.

