KRUNCH and UNKRNCH Utilities for the Burroughs B5500

KRUNCH reads a card deck containing a B5500 Algol source program,
removing all extraneous spaces and reducing the source to the minimum
number of card images, writing the filtered source to the card punch.
All comments are also removed from the source, and the output deck is
resequenced 10+10. The source program is still compilable, but it is not
very readable. A sample run deck would be:

        ?EXECUTE KRUNCH/UTILITY
        ?DATA CARD
            :                   :
            : Algol source deck :
            :                   :
        ?END

UNKRNCH reads a B5500 Algol source program and reformats it, applying
standard spacing and indentation to it. It was probably written
originally to reverse what KRUNCH does, but it will work on any Algol
source to function as a primitive "pretty print" formatter. It preserves
comments in the input, but of course cannot reinsert comments that were
stripped out by KRUNCH.

Input can be from a card deck or a "0CRDIMG" library tape (unblocked
card images). Output can be to a new punched card deck, another
"0CRDIMG" tape, or the line printer. The reformatted program is
resequenced 100+100. The program reads a parameter card in free-field
format with two integers that define the modes of input and output. The
values of these integers are defined in a comment at the beginning of
the source. A sample run deck to read the original program from cards
and output to the line printer would be (note the trailing comma on the
parameter card):

        ?EXECUTE UNKRNCH/UTILITY
        ?DATA CARD
        0,2,
            :                   :
            : Algol source deck :
            :                   :
        ?END

These programs appear to have been written by someone at Burroughs. They
were donated by Burroughs to the CUBE (user association) library in
1968. I encountered them at the University of Delaware in 1970 and saved
compile listings of them.

The source files below were transcribed from those listings before the
CUBE Library tapes became available to this project in May 2018. The
transcriptions have now been renamed and moved under the respective
files extracted from the CUBEB13 tape so that they will appear in the
versioned history of those files.

KRUNCH.UTILITY.alg_m
    Transcribed source for the KRUNCH program. Moved to the versioned
    history of /CUBE-Library-13/Files/KRUNCH-V0104AA.alg in this
    repository.

KRUNCH.PATCH.alg
    Patch for KRUNCH containing the one-line difference between the
    version on the CUBEB13 tape and the listing of the program obtained
    at U.Delaware in 1970. This appears to recognize "%" comments and
    drop them from the crunched output.

KRUNCH-As-Krunched.card
    Card deck resulting from running the original transcribed source for
    KRUNCH through KRUNCH itself.

KRUNCH-Compile.lst
    Algol compile listing for KRUNCH generated using the retro-B5500
    emulator running Mark XIII software.

UNKRNCH.UTILITY.alg_m
    Transcribed source for the UNKRNCH program. Moved to the versioned
    history of /CUBE-Library-13/Files/UNKRNCH-V0107AA.alg in this
    repository.

UNKRNCH-Compile-Run.lst
    Algol file listing for UNKRNCH generated using the retro-B5500
    emulator running Mark XIII software, followed by an execution of
    UNKRNCH that read the KRUNCH-As-krunched.card file and output the
    reformatted source to the line printer.

__________
2016-05-30 Paul Kimpel
    Commit original transcription.
2018-05-26 Paul Kimpel
    Rename and move transcribed sources under /CUBE-Library-13.



