SNOBOL3 Interpreter for the Burroughs B5500

This implementation of SNOBOL3 was written by John Chambers at the
University of Wisconsin. The source was obtained from CUBE Library tape
CUBEA13, file SNOBOL/L200010.

Before the CUBE Library tapes became available to this project in May
2018, the compiler and test program were transcribed from
http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
B5700_Snobol_Compiler_Apr77.pdf
by Rich Cornwell of North Carolina, US.

That transcribed source has now been renamed from SYMBOL.SNOBOL.alg_m to
SNOBOL-L200010.alg and moved under the CUBE-Library-13/Files directory
of this repository so that it will appear as versioned history of the
file extracted from the CUBEA13 tape.

A user manual is available at
http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
B5700_Snobol_Manual_Jun76.pdf.

The manual can also be generated using the SNOBOL program SNOBOL-
L200012.sno against the file SNOBOL-L200011.dat. These can be found in
the CUBE-Library-13/Files directory of this repository.


SNOBOL3-Compile.card
    Card deck to compile the SNOBOL interpreter.

SYMBOL.SNOBOL.alg_m
    Source for the SNOBOL program. Moved to the versioned history of
    /CUBE-Library-13/Files/SNOBOL-L200010.alg in this repository.

SNTEST.card
    A card deck with a series of SNOBOL test runs supplied by Rich
    Cornwell.

__________
2016-07-16 Paul Kimpel
    Commit initial files as transcribed by Rich Cornwell; overwrite with
    revised source having up-cased text and sequence number corrections,
    ready for proofreading.
2018-05-26 Paul Kimpel
    Rename and move transcribed source under /CUBE-Library-13.


