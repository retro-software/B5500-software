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


Paul Kimpel
June 2016
