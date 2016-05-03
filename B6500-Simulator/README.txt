Burroughs B6500 Simulator and Early MCP

B65SIM.alg_m is a simulator for the B6500 written in a variant of B5500
Extended Algol, termed LONGALG. This variant implement a "Long Array"
type, which appeared to the programmer to be a single-dimensional array,
but was implemented as a two-dimensional array. The compiler generated
code to partition the array index into column and row portions, and
index the row and column accordingly.

B65ESPOL.alg_m is an early implementation of an ESPOL cross-compiler for
the B6500, written to run on the B5500. We believe that Don Lyle was the
principle implementor of this compiler.

B65MCP-MARK00.esp_m is a very early version of the B6500 MCP. It is
notable for its use of ESPOL Queue structure, which eventually proved to
be too expensive at run time, and were removed in the Mark II.0 release.

These files were transcribed by James Fehlinger of New Jersey, US in
mid-2014, from scans of listings provided by Al Kossow of bitsavers.org.


