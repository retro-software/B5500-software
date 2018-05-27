000100DISKPUNCH   000118CA                                                    00
000200                                                                        00
000300************************************************************************00
000400          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00
000500     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00
000600************************************************************************00
000700                                                                        00
000800   THIS PROGRAM WILL PUNCH 80-CHARACTER RECORDS FROM A FILE OF          00
000900   CARDS ON DISK.                                                       00
001000                                                                        00
001100   THE FILE MUST BE MADE UP OF 80-CHARACTER RECORDS. THERE MUST         00
001200   BE THREE (3) RECORDS PER DISK SEGMENT. (I.E., THE BLOCKING           00
001300   FACTOR MUST BE A MULTIPLE OF 3.)                                     00
001400                                                                        00
001500   THE FIRST RECORD OF A FILE TO BE PUNCHED MUST CONTAIN:               00
001600                                                                        00
001700      COLS  1 - 10   "BEGIN-FILE"                                       00
001800           11 - 15   BLANKS                                             00
001900           16 - 24   "PUNCHOUT/"                                        00
002000           25 - 31   FILE ID (E.G., 602300N)                            00
002100           32 - 80   BLANKS                                             00
002200                                                                        00
002300   A PROGRAM WHICH BUILDS ONE OR MORE PUNCH FILES MUST PASS THESE       00
002400   PUNCH FILES TO THE UTILITY/DISKPUNCH. AT END-OF-JOB (PRECEDING       00
002500   THE "STOP RUN") THE FOLLOWING INSTRUCTIONS MUST BE EXECUTED FOR      00
002600   EACH FILE TO BE PUNCHED:                                             00
002700                                                                        00
002800      PERFORM WITH CHANGE-INFO.                                         00
002900      DISPLAY "FILE NNNNNNN READY FOR PUNCHING".                        00
003000                                                                        00
003100   "CHANGE-INFO" IS AN "OL" LEVEL ENTRY IN WORKING STORAGE CONTAINING   00
003200   THE VALUE "CC CHANGE 0000000/NNNNNNN TO PUNCHOUT/NNNNNNN;END".       00
003300   THE "NNNNNNN" REFERRED TO IN THE ABOVE CHANGE CARD AND THE "DISPLAY" 00
003400   STATEMENT IS THE ID OF THE FILE TO BE PUNCHED AS STATED IN THE       00
003500   "VALUE OF ID IS NNNNNNN".                                            00
003600                                                                        00
003700   WHEN THE PRODUCTION PACKAGE IS TURNED OVER TO THE OPERATORS, IT      00
003800   MUST INCLUDE AN EXECUTE CARD FOR THE PROGRAM WHICH IS CREATING       00
003900   THE PUNCH FILES, AN EXECUTE CARD AND A FILE CARD TO CAUSE            00
004000   EXECUTION OF UTILITY/DISKPUNCH FOR EACH FILE TO BE PUNCHED AND       00
004100   OPERATING INSTRUCTIONS WHICH SPECIFY THE FILE NUMBERS OF THE         00
004200   FILES WHICH ARE TO BE PUNCHED.                                       00
004300                                                                        00
004400   FOLLOWING THE EXECUTE FOR UTILITY/DISKPUNCH MUST BE THE FOLLOWING:   00
004500                                                                        00
004600      CC FILE PUNCHER = PUNCHOUT/NNNNNNN;END.                           00
004700                                                                        00
004800   WHERE "NNNNNNN" IS AS SPECIFIED IN THE "CHANGE" PASSED TO THE MCP.   00
004900                                                                        00
005000   WHEN THE UTILITY HAS FINISHED PUNCHING A FILE, THE UTILITY WILL      00
005100   CHHANGE THE NAME OF THE FILE FROM PUNCHOUT/NNNNNNN TO PUNCHDONE/     00
005200   NNNNNNN SO THAT THE OPERATOR WILL NOT TRY TO PUNCH THE SAME FILE     00
005300   A SECOND TIME THRU ERROR.                                            00
005400                                                                        00
005500                                                                        00
005600 IDENTIFICATION DIVISION.                                         DSKPCH00
005700 PROGRAM-ID. UTILITY DISK PUNCH "DISKPCH".                        DSKPCH00
005800 DATE-COMPILED.                                                   DSKPCH00
005900 REMARKS. THIS PROGRAM WILL PUNCH CARD-IMAGE RECORDS IDENTIFIED   DSKPCH00
006000     WITH A FIRST-RECORD CONTAINING PROGRAM NUMBER.               DSKPCH00
006100 ENVIRONMENT DIVISION.                                            DSKPCH00
006200 CONFIGURATION SECTION.                                           DSKPCH00
006300 SOURCE-COMPUTER. B-5500.                                         DSKPCH00
006400 OBJECT-COMPUTER. B-5500.                                         DSKPCH00
006500 INPUT-OUTPUT SECTION.                                            DSKPCH00
006600 FILE-CONTROL.                                                    DSKPCH00
006700     SELECT PUNCHER ASSIGN TO DISK.                               DSKPCH00
006800     SELECT PUNCHIT ASSIGN TO PUNCH.                              DSKPCH00
006900 I-O-CONTROL.                                                     DSKPCH00
007000     APPLY TECHNIQUE-A ON PUNCHER.                                DSKPCH00
007100 DATA DIVISION.                                                   DSKPCH00
007200 FILE SECTION.                                                    DSKPCH00
007300 MD  PUNCHER                                                      DSKPCH00
007400     FILE CONTAINS 20 | 1500 RECORDS                              DSKPCH00
007500     ACCESS MODE IS SEQUENTIAL                                    DSKPCH00
007600     BLOCK CONTAINS 15 RECORDS                                    DSKPCH00
007700     RECORD CONTAINS 80 CHARACTERS                                DSKPCH00
007800     LABEL RECORD IS STANDARD                                     DSKPCH00
007900     VALUE OF ID IS "PCHDSK"                                      DSKPCH00
008000     DATA RECORD IS CRD.                                          DSKPCH00
008100 01  CRD             SZ 80.                                       DSKPCH00
008200     05  BEGINIT                 PC X(10).                        DSKPCH00
008300     05 FILLER       SZ 14.                                       DSKPCH00
008400     05  PGM-ID                  PC X(7).                         DSKPCH00
008500     05  FILLER      SZ 49.                                       DSKPCH00
008600 FD  PUNCHIT                                                      DSKPCH00
008700     LABEL RECORD IS OMITTED                                      DSKPCH00
008800     DATA RECORD IS PCH.                                          DSKPCH00
008900 01  PCH             SZ 80.                                       DSKPCH00
009000 WORKING-STORAGE SECTION.                                         DSKPCH00
009100 77  START                       PC X(10) VA "BEGIN-FILE".        DSKPCH00
009200 77  ENDIT                       PC X(10) VA "COMPLETED ".        DSKPCH00
009300 01  MESSAGE         SZ 80.                                       DSKPCH00
009400     05  SPOMSG      SZ 31.                                       DSKPCH00
009500         10  MSG                 PC X(10) VA "BEGIN-FILE".        DSKPCH00
009600         10  FILLER              PC X(14) VA "     PUNCHOUT/".    DSKPCH00
009700         10 PGM                  PC X(7)  VA SPACE.               DSKPCH00
009800     05 FILLER SZ 49 VA "*****************************************DSKPCH00
009900-    "********".                                                  DSKPCH00
010000 01  CHGIT           SZ 57.                                       DSKPCH00
010100     05  FILLER      PC X(23)    VA "CC CHANGE      PUNCHOU/".    DSKPCH00
010200     05  ENDID1      PC X(7).                                     DSKPCH00
010300     05  FILLER      PC X(15)    VA " TO    PUNCHDO/".            DSKPCH00
010400     05  ENDID2      PC X(7).                                     DSKPCH00
010500     05  FILLER      PC X(5)     VA ";END.".                      DSKPCH00
010600 PROCEDURE DIVISION.                                              DSKPCH00
010700 101.                                                             DSKPCH00
010800     OPEN INPUT PUNCHER.                                          DSKPCH00
010900     OPEN OUTPUT PUNCHIT.                                         DSKPCH00
011000     READ PUNCHER AT END DISPLAY "NO PUNCH FILE ON DISK" STOP RUN.DSKPCH00
011100     IF BEGINIT ! "BEGIN-FILE"               DISPLAY "INVALID PUNCDSKPCH00
011200-    "H FILE ID. RUN ABORTED." STOP RUN.                          DSKPCH00
011300     MOVE PGM-ID TO PGM.                                          DSKPCH00
011400     MOVE START TO MSG.                                           DSKPCH00
011500     DISPLAY SPOMSG.                                              DSKPCH00
011600     WRITE PCH FROM MESSAGE.                                      DSKPCH00
011700 102.                                                             DSKPCH00
011800     READ PUNCHER AT END GO TO 103.                               DSKPCH00
011900     WRITE PCH FROM CRD.                                          DSKPCH00
012000     GO TO 102.                                                   DSKPCH00
012100 103.                                                             DSKPCH00
012200     MOVE ENDIT TO MSG.                                           DSKPCH00
012300     DISPLAY SPOMSG.                                              DSKPCH00
012400     MOVE PGM TO ENDID1, ENDID2.                                  DSKPCH00
012500     CLOSE PUNCHER.  CLOSE PUNCHIT.                               DSKPCH00
012600     PERFORM WITH CHGIT.                                          DSKPCH00
012700     STOP RUN.                                                    DSKPCH00
012800 END-OF-JOB.                                                      DSKPCH00
