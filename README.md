The **B5500-software** project maintains source code and software artifacts for the Burroughs B5500 computer system, a 1960s mainframe system designed for high-level languages, and specifically Algol 60. Many of the files haven been transcribed from listings or scans of listings.

Contributions of new material and corrections to existing material are most welcome. Please submit a pull request.

Files are organized in top-level directories by program or application and origin. Each top-level directory contains a `README.txt` file describing the contents and their provenance.

Source code formats for the B5500 were based on 80-column punched cards and card images stored on disk and magnetic tape. For most languages, a range of columns on the card were reserved for "sequence numbers," which were used by a number of tools for locating lines in the program and merging symbolic patch files. These sequence numbers are a vital part of the source and must be maintained by all contributors.

Source files should be prepared in Unisys "PWB format," named for the Programmers Workbench editor available with modern Unisys MCP systems. These are ordinary Windows text files, but padded with spaces to the full fixed-length record size used when storing records on disk. PWB understands sequence numbers and maintains the files with proper padding. There is also an Eclipse plug-in that offers much of the functionality of PWB. To obtain the plug-in, search for "MCP IDE for Eclipse" on the Unisys web site, http://www.unisys.com.

File name extensions of the form `.*_m` denote PWB-format files. The following table shows the common extensions used with B5500 source files, the locations of text, sequence, and mark fields with in the record, and the full length to which records should be padded. The length does not include line delimiters. "Mark" fields are used to carry patchmark ("blame") information, but were not used on the B5500, and may be left blank:

|File Kind  |Ext     | Text|  Seq| Mark|   Length|
|:----------|:-------|----:|----:|----:|--------:|
|ALGOL      |`.alg_m`| 1-72|73-80|81-90|       90|
|BASIC      |`.bas_m`| 5-72|  1-4|73-80|       80|
|COBOL      |`.cob_m`| 7-72|  1-6|73-80|       80|
|DATA       |`.dat_m`|  all|  n/a|  n/a| variable|
|DCP        |`.dcp_m`| 1-72|73-80|81-90|       90|
|ESPOL      |`.esp_m`| 1-72|73-80|81-88|       88|
|FORTRAN    |`.for_m`| 1-72|73-80|81-80|       80|
|JOB        |`.wfl_m`| 1-80|83-90|91-90|       90|
|NDL        |`.ndl_m`| 1-72|73-80|81-90|       90|
|PASCAL     |`.pas_m`| 1-72|73-80|81-90|       90|
|SEQDATA    |`.sqd_m`| 1-72|73-80|81-90|       80|
|TEXTDATA   |`.txt_m`| 1-72|73-80|81-90|       90|
|XFORTRAN   |`.xfr_m`| 1-72|73-80|81-80|       80|
|Card deck  |`.card` |  all|  n/a|  n/a|      n/a|

The initial set of files for this repository were extracted from http://github.com/pkimpel/retro-b5500 in April 2016.


