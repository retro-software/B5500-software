R/C (Remote/Card) Interactive Source Editor for Burroughs B5500

R/C was written in the late 1960s by Ron Brody at Burroughs Defense,
Space, and Special Systems Group in Paoli, Pennsylvania, US. It had some
of the functionality of a timesharing system, but was not one. Unlike
CANDE, which had to run under the Timesharing MPC (TSMCP), R/C was
designed to run under the B5500 Data Communications MCP (DCMCP)
operating system. The datacom facilities of DCMCP were oriented towards
on-line transaction processing (OLTP) rather than multi-user
timesharing.

R/C provided for the remote preparation and maintenance of card-image
files, submission of card image files as batch jobs to the B5500 host,
and some remote control of those jobs. In that sense, it was more like a
Remote Job Entry (RJE) system, but one in which the card decks were
maintained on the central host rather than at the remote site. Output
from programs was generally printed or punched at the host, although
printer and punch files could be file-equated to disk and listed by R/C
from disk to the remote terminal.

The source files for R/C were transcribed from portions of
http://bitsavers.org/pdf/burroughs/B5000_5500_5700/listing/
B5700_Text_Editors_Sep76.pdf
by Rich Cornwell of Durham, North Carolina, US. Proofing and correction
of the source was done by Paul Kimpel.

RC.alg_m
    The R/C program, written in Burroughs Extended Algol.

RC-Compile.card
    A card deck that can be used to compile the R/C source.

RC-Reference.txt_m
    The raw text of the R/C Reference Manual. This is coded in the
    markup notation used by the XREF/JONES utility, available on the
    Mark XIII SYSTEM tape. This file was not transcribed from original
    markup -- Rich Cornwell reverse-engineered the markup for the first
    few sections from B5700_Text_Editors_Sep76.pdf; the reconstruction
    was completed by Paul Kimpel.


