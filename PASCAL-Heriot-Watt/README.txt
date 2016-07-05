PASCAL Compiler for the Burroughs B5500/B5700

A compiler and run-time system for Niklaus Wirth's Pascal language,
written by Dag F. Langmyhr at Heriot-Watt University in Edinburgh,
Scotland, ca. 1975.

Rather than compiling Pascal source to B5500 object code, this compiler
translates the Pascal source to Burroughs Algol. The PASCRUN/DISK file
is Algol source that is inserted into the Algol generated from the
Pascal source to provide a run-time system -- actually it is more like a
shim between Pascal and standard Algol intrinsics and I/O.

The compiler, run-time system, and patches were originally transcribed
by Rich Cornwell of North Carolina, US. Proofing and correction were
performed by Paul Kimpel of San Diego, California, US.

HMSS2.TEST.card
    Card deck to compile and run a sample Pascal program. This program
    computes the temperature profile in a square chimney with one side
    against a perfectly-insulated wall and the other three sides exposed
    to ambient air. Note that this program takes almost nine minutes to
    run in the retro-B5500 emulator (which is close to the performance
    of a real B5500).

HMSS2.TEST.lst
    Listing produced by running the HMSS2.TEST.card job, including the
    Pascal compiler listing, a listing of the generated Algol code, and
    the output from running the generated program.

PASCAL.MARKXIII.card
    Compile deck and patches to allow the PASCAL compiler to compile
    using B5500 Mark XIII Algol.

PASCAL.MARKXIII-Compile.lst
    Listing produced by running the PASCAL.MARKXIII.card job.

PASCRUN.DISK.alg_m
    Algol source for the run-time system inserted into the translated
    Algol by the compiler. Transcribed from
    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
    B5700_Pascal_Apr78.pdf.

PATCHES.PASCAL.card
    Card deck containing patches to the Pascal compiler in PATCH/MERGE
    format. Transcribed from
    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
    B5700_Pascal_Mar79.pdf.

SYMBOL.PASCAL.alg_m
    Source for the Pascal compiler/translator, written in Burroughs
    Extended Algol for the B5500. Transcribed from
    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
    B5700_Pascal_Mar79.pdf.


2016-06-12 Paul Kimpel
    Initial commits to source control.
2016-07-04 Paul Kimpel
    Commit proofreading corrections to SYMBOL.PASCAL and PASCRUN.DISK.
    Commit compile deck and listing with patches to allow the compiler
    to work with B5500 Mark XIII Algol. Commit compile & go deck and
    output listing for HMSS2.TEST sample program.

