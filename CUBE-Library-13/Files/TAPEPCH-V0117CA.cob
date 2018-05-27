000100TAPEPUNCH   000117CA                                                    00
000200                                                                        00
000300  *******************************************************************   00
000400          PLEASE NOTE THAT THE BURROUGHS CORPORATIONS ASSUMES           00
000500     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00
000600  *******************************************************************   00
000700                                                                        00
000800   THIS PROGRAM WILL PUNCH 80-CHARACTER RECORDS FROM A FILE OF          00
000900   CARDS ON TAPE.                                                       00
001000                                                                        00
001100   THE FILE MUST BE MADE UP OF 80-CHARACTER RECORDS. THERE MUST         00
001200   BE NO MORE THAN FIFTEEN (15) RECORDS PER BLOCK.                      00
001300                                                                        00
001400   THE FIRST RECORD OF A FILE TO BE PUNCHED MUST CONTAIN:               00
001500      COLS  1 - 10   "BEGIN-FILE"                                       00
001600           11 - 15   BLANKS                                             00
001700           16 - 24   "PUNCHOUT/"                                        00
001800           25 - 31   FILE ID (E.G., 602300N)                            00
001900           32 - 80   BLANKS                                             00
002000                                                                        00
002100   A PROGRAM WHICH BUILDS ONE OR MORE PUNCH FILES MUST PASS THESE       00
002200   PUNCH FILES TO THE UTILITY/TAPEPUNCH. AT END-OF-JOB (PRECEEDING      00
002300   EACH "STOP RUN") THE FOLLOWING INSTRUCTION MUST BE EXECUTED FOR      00
002400   EACH FILE TO BE PUNCHED:                                             00
002500      DISPLAY "FILE NNNNNNN READY FOR PUNCHING."                        00
002600   THE "NNNNNNN" REFERRED TO IS THE ID OF THE FILE TO BE PUNCHED        00
002700   AS STATED IN THE "VALUE OF ID IS NNNNNNN".                           00
002800                                                                        00
002900   WHEN THE PRODUCTION PACKAGE IS TURNED OVER TO THE OPERATORS, IT      00
003000   MUST INCLUDE AN EXECUTE CARD FOR THE PROGRAM CREATING THE            00
003100   PUNCH FILES, AN EXECUTE PACKET FOR EACH FILE WHICH THE UTILITY       00
003200   IS TO PUNCH AND OPERATING INSTRUCTIONS WHICH SPECIFY THE FILE        00
003300   NUMBERS OF THE FILES WHICH THE UTILITY IS TO PUNCH.                  00
003400                                                                        00
003500   WHEN THE UTILIIY HAS FINISHED PUNCHING A FILE, THAT FILE WILL        00
003600   BE CLOSED "WITH LOCK". THIS WILL PREVENT THE OPERATOR FROM           00
003700   PUNCHING A FILE A SECOND TIME BUT YET PROVIDE A BACKUP PROTECTION.   00
003800                                                                        00
003900                                                                        00
004100 IDENTIFICATION DIVISION.                                         TAPPCH00
004200 PROGRAM-ID. UTILITY TAPE PUNCH "TAPEPCH".                        TAPPCH00
004300 DATE-COMPILED.                                                   TAPPCH00
004400 REMARKS. THIS PROGRAM WILL PUNCH CARD-IMAGE RECORDS IDENTIFIED   TAPPCH00
004500     WITH A FIRST-RECORD CONTAINING PROGRAM NUMBER.               TAPPCH00
004600 ENVIRONMENT DIVISION.                                            TAPPCH00
004700 CONFIGURATION SECTION.                                           TAPPCH00
004800 SOURCE-COMPUTER. B-5500.                                         TAPPCH00
004900 OBJECT-COMPUTER. B-5500.                                         TAPPCH00
005000 INPUT-OUTPUT SECTION.                                            TAPPCH00
005100 FILE-CONTROL.                                                    TAPPCH00
005200     SELECT PUNCHER ASSIGN TO TAPE.                               TAPPCH00
005300     SELECT PUNCHIT ASSIGN TO PUNCH.                              TAPPCH00
005400 I-O-CONTROL.                                                     TAPPCH00
005500     APPLY TECHNIQUE-A ON PUNCHER.                                TAPPCH00
005600 DATA DIVISION.                                                   TAPPCH00
005700 FILE SECTION.                                                    TAPPCH00
005800 FD  PUNCHER                                                      TAPPCH00
005900     BLOCK CONTAINS 15 RECORDS                                    TAPPCH00
006000     RECORD CONTAINS 80 CHARACTERS                                TAPPCH00
006100     LABEL RECORD IS STANDARD                                     TAPPCH00
006200     VALUE OF ID IS "PCHTAP"                                      TAPPCH00
006300     DATA RECORD IS CRD.                                          TAPPCH00
006400 01  CRD             SZ 80.                                       TAPPCH00
006500     05  BEGINIT                 PC X(10).                        TAPPCH00
006600     05 FILLER       SZ 14.                                       TAPPCH00
006700     05  PGM-ID                  PC X(7).                         TAPPCH00
006800     05  FILLER      SZ 49.                                       TAPPCH00
006900 FD  PUNCHIT                                                      TAPPCH00
007000     LABEL RECORD IS OMITTED                                      TAPPCH00
007100     DATA RECORD IS PCH.                                          TAPPCH00
007200 01  PCH             SZ 80.                                       TAPPCH00
007300 WORKING-STORAGE SECTION.                                         TAPPCH00
007400 77  START                       PC X(10) VA "BEGIN-FILE".        TAPPCH00
007500 77  ENDIT                       PC X(10) VA "COMPLETED ".        TAPPCH00
007600 01  MESSAGE         SZ 80.                                       TAPPCH00
007700     05  SPOMSG      SZ 31.                                       TAPPCH00
007800         10  MSG                 PC X(10) VA "BEGIN-FILE".        TAPPCH00
007900         10  FILLER              PC X(14) VA "     PUNCHOUT/".    TAPPCH00
008000         10 PGM                  PC X(7)  VA SPACE.               TAPPCH00
008100     05 FILLER SZ 49 VA "*****************************************TAPPCH00
008200-    "********".                                                  TAPPCH00
008300 01  CHGIT           SZ 57.                                       TAPPCH00
008400     05  FILLER      PC X(23)    VA "CC CHANGE      PUNCHOU/".    TAPPCH00
008500     05  ENDID1      PC X(7).                                     TAPPCH00
008600     05  FILLER      PC X(15)    VA " TO    PUNCHDO/".            TAPPCH00
008700     05  ENDID2      PC X(7).                                     TAPPCH00
008800     05  FILLER      PC X(5)     VA ";END.".                      TAPPCH00
008900 PROCEDURE DIVISION.                                              TAPPCH00
009000 101.                                                             TAPPCH00
009100     OPEN INPUT PUNCHER.                                          TAPPCH00
009200     OPEN OUTPUT PUNCHIT.                                         TAPPCH00
009300     READ PUNCHER AT END DISPLAY "NO PUNCH FILE ON TAPE" STOP RUN.TAPPCH00
009400     IF BEGINIT ! "BEGIN-FILE"               DISPLAY "INVALID PUNCDSKPCH00
009500-    "H FILE ID. RUN ABORTED." STOP RUN.                          TAPPCH00
009600     MOVE PGM-ID TO PGM.                                          TAPPCH00
009700     MOVE START TO MSG.                                           TAPPCH00
009800     DISPLAY SPOMSG.                                              TAPPCH00
009900     WRITE PCH FROM MESSAGE.                                      TAPPCH00
010000 102.                                                             TAPPCH00
010100     READ PUNCHER AT END GO TO 103.                               TAPPCH00
010200     WRITE PCH FROM CRD.                                          TAPPCH00
010300     GO TO 102.                                                   TAPPCH00
010400 103.                                                             TAPPCH00
010500     MOVE ENDIT TO MSG.                                           TAPPCH00
010600     DISPLAY SPOMSG.                                              TAPPCH00
010700     CLOSE PUNCHIT.  CLOSE PUNCHER WITH LOCK.                     TAPPCH00
010800     STOP RUN.                                                    TAPPCH00
010900 END-OF-JOB.                                                      TAPPCH00
