Burroughs B6500 Simulator and Early MCP for the B5500

These files were transcribed by James Fehlinger of New Jersey, US in
mid-2014, from scans of listings provided by Al Kossow of bitsavers.org.
Paul Kimpel assisted in proofing and debugging of the transcribed files.

B65SIM.alg_m
    An engineering simulator for the Burroughs B6500, written in a
    variant of B5500 Extended Algol, termed LONGALG, described below.
    This simulator appears to have been built to model the "flows"
    (schematic logic and state diagrams) for the B6500 processor. It
    runs as a normal user task under the B5500 MCP.

B65SIM-COMPILE.card
    A card deck to compile B65SIM using the LONGALG compiler described
    below. Note that since LONGALG is not a standard compiler name, the
    compile card must specify "WITH LONGALG".

B65SIM-COMPILE-PRT.lst
    The listing of the B6500 Simulator (with $PRT set) produced by the
    card deck above.

B65ESPOL.alg_m
    An early implementation of an ESPOL cross-compiler for the B6500,
    written to run on the B5500. We believe that Don Lyle was the
    principal implementer of this compiler.

B65MCP-MARK00.esp_m
    A very preliminary version of the B6500 MCP. It is notable for its
    use of ESPOL Queue structures, which eventually proved to be too
    expensive at run time, and were removed in the Mark II.0 release.

LONGALG.alg_m
    LONGALG is a specially modified version of the Mark XIII Algol
    compiler that supports the LONG ARRAY type required by B65SIM. We do
    not have the source for the original LONGALG, which was written no
    later than 1968, and would have been based on an earlier release
    than XIII (which is from late 1971). Support for LONG ARRAYS was
    reverse-engineered from comments in the B65SIM listing and
    implemented for XIII Algol by Paul Kimpel in June 2017.

    LONGALG automatically doubles the declared size of the last
    dimension of a LONG ARRAY, and automatically doubles any index
    values for that last dimension when accessing elements of the array
    (thus addressing only the even-numbered words). This was done to
    implement a simulated memory array that allowed B6500 tag values to
    be stored in the odd words. In addition, a LONG ARRAY physically has
    one more dimension than is declared in the program. The last (or
    only) declared dimension is split into rows of 256 words (128
    emulated words). As many of these rows are allocated in the next-to-
    last physical dimension to accommodate the required number of rows
    for the (doubled) size of the last declared dimension.

LONGALG-PATCH.alg_m
    Kimpel's patch to Mark XIII Algol to produce the LONGALG compiler.
    This deck compiles LONGALG/NEW, which then must be marked as a
    compiler by the SPO command "MC LONGALG/NEW", which will also change
    the name of the file to the standard convention for compilers,
    LONGALG/DISK.

LONGALG-DELTA.pdf
    A PDF document showing a side-by-side comparison of LONGALG to the
    base Mark XIII source.

