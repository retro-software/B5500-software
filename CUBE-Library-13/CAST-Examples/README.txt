CAST Tape Examples for CUBE Library Version 13.

The CUBE_LBR tape image in the library is in CAST format, a sequential
source archive originating from the days of the B5000. The B5000 did not
have the large Head-per-Track disks that were introduced with the B5500,
only two relatively small drums, so source programs had to be maintained
either as card decks or on tape.

The CAST format allowed multiple source modules to be maintained on a
single tape volume. These tapes were maintained by the standard utility
program MAKCAST/DISK. The Algol and COBOL compilers understood this
format and could compile programs and include individual routines
directly from CAST tapes or a disk file.

CAST tapes had a directory on the front of the tape that identified the
files stored on that tape. The directory included the relative record
number of the start of each file. This allowed MAKCAST and the compilers
to use the Algol SPACE statement to position the tape to individual
files relatively efficiently. If the CAST file was on disk, SPACE
provided random access to the modules. If the file was on tape, it could
take about five minutes to traverse a full reel.

Here is what I have deduced about the format of CAST tapes:

    1. The tape is labeled with standard B5500 tape labels.

    2. Tapes are written with fixed-length 448-character (56 word)
    blocks.

    3. The first three blocks on the tape contain a directory of the
    files on the tape:

        a. The first word of the first directory block appears to be a
        count of the number of blocks in the directory. This appears to
        be a fixed value of 3, however, and is hard-wired into the
        MAKCAST/DISK utility program that maintains these tapes.

        b. Entries in the tape directory are variable length,
        consisting of N+4 characters, where N is the number of
        characters in the library module name.

        c. The first character in an entry is the length in binary of
        the module name, followed by the characters of the name.

        d. Following the name are three characters that specify a big-
        endian 18-bit binary number -- the 1-relative logical record
        number on the tape where the module starts. This number is
        relative to the first non-directory block on the tape (i.e., the
        block following the directory blocks).

        e. Directory entries are not split across tape blocks. If there
        is insufficient room at the end of a block for the next entry, a
        zero-length entry is inserted at the end of that block and the
        entry is stored at the beginning of the next block.

    4. The remainder of the tape after the directory blocks consists of
    blocks with the text of the library modules.

    5. The first word of each of these text blocks is the binary value
    of the 1-relative record number of the first logical record in the
    block, using the same relative basis as in the 18-bit directory
    record numbers.

    6. The remainder of the block consists of five logical records of 88
    characters (11 words) each (thus 5*88+8=448). The first 80
    characters of a logical record hold a card image. The last eight
    characters of a logical record do not appear to be used and are
    zero.

    7. The library is terminated by a physical tape mark and ending tape
    label.

    8. A 2400-foot reel of tape could hold almost 110,000 records at 800
    bpi. The maximum capacity of a library is limited by the three
    directory blocks and the 18-bit record number in the directory
    entries.

The MAKCAST/DISK program is described on page 5-5ff in:

    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/
    1024916_B5500_B5700_OperMan_Sep68.pdf

The Algol and COBOL compilers use "$$" cards to include source modules
from a CAST tape or disk file into the program being compiled. These
cards are described on page 4-41ff in the link above.


CASTTST.card
    Card deck illustrating how to compile a program from a CAST file. By
    default, the "A" file was "CASTA/LIBRARY" on disk.

COMCAST.card
    Card deck illustrating how to compile and run a program by inserting
    fragments of a module from a CAST tape and insert patches between
    the fragments. By default, the "C" file was labeled "CASTC" on tape.

CPYCAST.card
    Card deck for a program to copy the "CASTC" file from tape to
    "CASTA/LIBRARY" on disk

MAKCAST.card
    Card deck to illustrate simple use of MAKCAST/DISK. This deck
    generates a listing of the tape directory, a listing of module
    URS046, and punches a deck for module PTS024.

PTS051.lst
    The listing resulting from the compilation and execution in
    COMCAST.card.

__________
2018-05-27 Paul Kimpel
    Initial version.


