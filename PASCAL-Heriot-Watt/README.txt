PASCAL Compiler for the Burroughs B5500/B5700

A compiler and run-time system for Niklaus Wirth's Pascal language,
written by Dag F. Langmyhr at Heriot-Watt University in Edinburgh,
Scotland, ca. 1975.

Rather than compiling Pascal source to B5500 object code, this compiler
translates the Pascal source to Burroughs Compatible Algol (XAlgol). The
PASCRUN/DISK file is an XAlgol source that is inserted into the XAlgol
generated from the Pascal source to provide a run-time system --
actually it is more like a shim between Pascal and standard XAlgol
intrinsics and I/O.

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
    Pascal compiler listing, a listing of the generated XAlgol code, and
    the output from running the generated program.

PASCAL.MKXIII.card
    Compile deck and patches to allow SYMBOL.PASCAL.alg_m to compile
    using the B5500 Mark XIII XAlgol compiler.

PASCAL.MKXIII-Compile.lst
    Listing produced by running the PASCAL.MARKXIII.card job.

PASCAL.MKXV-Compile.lst
    Listing produced by compiling unpatched SYMBOL.PASCAL.alg_m with the
    Mark XV XAlgol compiler.

PASCRUN.DISK.alg_m
    XAlgol source for the run-time system inserted into the translated
    XAlgol by the compiler. Transcribed from
    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
    B5700_Pascal_Apr78.pdf.

PATCHES.PASCAL.card
    Card deck containing patches to the Pascal compiler in PATCH/MERGE
    format. Transcribed from the listing in the front of
    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
    B5700_Pascal_Mar79.pdf.
    **NOTE** This series of patches uses compiler features, primarily $-
    cards, that were implemented after Mark XIII. It works under Mark
    XV, but will not work under Mark XIII software without some
    modifications.

PATCHES.PASCAL.MKXV-Compile.lst
    PATCH/MERGE output and XAlgol listing produced by running
    PATCHES.PASCAL.card under Mark XV system software. This run
    generated the updated symbol file SYMNEW.PASCAL.alg_m.

SYMBOL.PASCAL.alg_m
    Source for the Pascal compiler/translator, written in Burroughs
    XAlgol for the B5500. Transcribed from
    http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
    B5700_Pascal_Mar79.pdf.

SYMNEW.PASCAL.alg_m
    Updated XAlgol source for the Pascal compiler/translator, produced
    by applying the patches in PATCHES.PASCAL.card to
    SYMBOL.PASCAL.alg_m.
    **PLEASE NOTE**
        1.  This source was generated using Mark XV system software. The
            source uses XAlgol constructs that are not available in the
            Mark XIII compiler.
        2.  Use of this compiler requires changes to the PASCRUN/DISK
            run-time system. AT PRESENT WE DO NOT HAVE THOSE CHANGES.
            This file and PATCHES.PASCAL.MKXV-Compile.lst are provided
            mainly for historical interest; at this point you probably
            do not want to try to use them.

__________
2016-06-12 Paul Kimpel
    Initial commits to source control.
2016-07-04 Paul Kimpel
    Commit proofreading corrections to SYMBOL.PASCAL.alg_m and
    PASCRUN.DISK.alg_m. Commit compile deck and listing with patches to
    allow the compiler to work with B5500 Mark XIII Algol. Commit
    compile & go deck and output listing for HMSS2.TEST sample program.
2017-07-16 Paul Kimpel
    Commit proofreading corrections to PASCAL.PATCHES.card. Commit
    additional listings for Mark XV, the patch deck, and resulting
    updated compiler source file.

