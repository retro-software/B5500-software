000100PUNCHSOLT   000122CA                                                    00
000200                                                                        00
000300************************************************************************00
000400          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00
000500     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00
000600************************************************************************00
000700                                                                        00
000800   THIS PROGRAM WILL PUNCH FROM A SOLT TAPE, AN UNBLOCKED CARD-IMAGE    00
000900   TAPE OR A DECK OF CARDS, A NEW SOURCE PROGRAM. THE NEW SOURCE        00
001000   DECK WILL NOT BE RESEQUENCED AND BLANK CARDS WILL BE PUNCHED         00
001100   WHERE THEY ARE ENCOUNTERED.                                          00
001200                                                                        00
001300   THE ID OF THE INPUT MUST "SOLT". THIS ID IS ON ALL SOLT TAPES        00
001400   CREATED BY THE PROGRAM UTILITY/MAKESOLT AND MAY BE LABEL-EQUATED     00
001500   BY A FILE CARD AS FOLLOWS:                                           00
001600                                                                        00
001700      CC FILE "SOLT-TAPE" = 0000000/NNNNNNN;END                         00
001800         (WHERE "NNNNNNN" IS THE ID OF THE FILE TO BE USED.)            00
001900                                                                        00
002000   THE FIRST CARD PUNCHED OUT WILL BE THE PUNCH BACK-UP HEADER          00
002100   ("BEGIN-FILE" IN COLS. 1-10, "PUNCHOUT/SOLT" IN COLS/. 16-28         00
002200   AND "**********" IN COLS. 71-80).                                    00
002300                                                                        00
002400   IF THE LAST CARD-IMAGE DOES NOT HAVE "END-OF-JOB." PUNCHED IN        00
002500   COLUMNS 8-18, THE MESSAGE "NO END-OF-JOB SOLT" WILL TYPE OUT.        00
002600                                                                        00
002700   THE LAST CARD PUNCHED OUT WILL HAVE "999999 END-OF-JOB." PUNCHED     00
002800   IN COLS. 1-18 AND WILL REPLACE THE END OF JOB CARD FROM THE INPUT    00
002900   IF THERE WAS ONE.                                                    00
003000                                                                        00
003100   EXECUTE PACKETS FOR UTILITY/PUNCHSOLT MUST BE ONE OF THE FOLLOWING:  00
003200      CC EXECUTE UTILITY/PUNCHSOLT                                      00
003300      CC DATA SOLT                                                      00
003400      ******SOURCE DECK******                                           00
003500      CC END                                                            00
003600         (NOTE THAT INPUT IS FROM THE CARD-READER.)                     00
003700                                                                        00
003800      CC EXECUTE UTILITY/PUNCHSOLT;END                                  00
003900         (INPUT IS FROM A TAPE WITH THE ID "SOLT")                      00
004000                                                                        00
004100      CC EXECUTE UTILITY/PUNCHSOLT                                      00
004200      CC FILE "SOLT-TAPE" = 0000000/|||||||;END                         00
004300         (||||||| IS THE ID OF THE TAPE TO BE USED AS INPUT.)           00
004400                                                                        00
004500   IN ADDITION, EACH OF THE ABOVE WILL NEED CARDS FOR THE PUNCH         00
004600   BACK-UP PROGRAM:                                                     00
004700                                                                        00
004800      CC EXECUTE UTILITY/DISKPUNCH                                      00
004900      CC FILE PUNCHER = PUNCHOUT/CARDS                                  00
005000      CC END                                                            00
005100                                                                        00
005200                                                                        00
005400 IDENTIFICATION DIVISION.                                               00
005500 PROGRAM-ID.         SOLT TAPE TO CARDS    "PCHSOLT".                   00
005600 DATE-COMPILED.                                                         00
005700                                                                        00
005800 ENVIRONMENT DIVISION.                                                  00
005900 CONFIGURATION SECTION.                                                 00
006000 SOURCE-COMPUTER.       B-5500.                                         00
006100 OBJECT-COMPUTER.       B-5500.                                         00
006200 INPUT-OUTPUT SECTION.                                                  00
006300 FILE-CONTROL.                                                          00
006400         SELECT SOLT-TAPE        ASSIGN TO TAPE.                        00
006500         SELECT  OUTCRD          ASSIGN TO DISK.                        00
006600 I-O-CONTROL.  APPLY TECHNIQUE-A ON OUTCRD.                             00
006700 DATA DIVISION.                                                         00
006800 FILE SECTION.                                                          00
006900 FD  SOLT-TAPE                                                          00
007000         LABEL RECORD STANDARD                                          00
007100         VALUE OF ID "SOLT"                                             00
007200         DATA RECORD INREC1 AND INREC2.                                 00
007300 01  INREC1.                                                            00
007400     05   INRECA             OC 5   SZ 88.                              00
007500         10   FILLERA                            SZ 8.                  00
007600         10   SOLT-REC-1                         PC X(80).              00
007700     05   INRECB                                 SZ 8.                  00
007800 01  INREC2                          SZ 456.                            00
007900     05   FILLERCARD                             SZ 80.                 00
008000     05   FILLER                                 SZ 376.                00
008100 MD  OUTCRD                                                             00
008200                                                                        00
008300         LABEL RECORD STANDARD                                          00
008400         VALUE OF ID "CARDS"                                            00
008500         SAVE-FACTOR 2                                                  00
008600         FILE CONTAINS 6 | 1500 RECORDS                                 00
008700         ACCESS MODE SEQUENTIAL                                         00
008800         BLOCK CONTAINS 15 RECORDS                                      00
008900         DATA RECORD CARDOUT.                                           00
009000 01  CARDOUT                 SZ 80.                                     00
009100     05    FILLER                    SZ 7.                              00
009200     05    EOJ-CHECK         SZ 10.                                     00
009300     05    FILLER            SZ 63.                                     00
009400 WORKING-STORAGE SECTION.                                               00
009500     77    SUB               CMP-1   PC 9.                              00
009600 01  MCP-INFORMATION.                                                   00
009700     05   FILLER VA "CC CHANGE 0000000/CARDS TO PUNCHOU/CARDS;END"      00
009800                             SZ 45.                                     00
009900 PROCEDURE DIVISION.                                                    00
010000 0302.  OPEN INPUT SOLT-TAPE  OUTPUT OUTCRD.                            00
010100         MOVE "BEGIN-FILE     PUNCHOUT/SOLT" TO CARDOUT.                00
010200         PERFORM 0401-PUNCH-CARD.                                       00
010300         READ SOLT-TAPE  AT END DISPLAY "NO RECORDS ON SOLT TAPE"       00
010400                 STOP RUN.                                              00
010500         IF FILLERA(1) = "00000088" GO TO 0405-SOLTFORMAT.              00
010600 0308.   MOVE INREC2 TO CARDOUT.                                        00
010700         IF EOJ-CHECK = "END-OF-JOB" GO TO 0316-EOJ.                    00
010800         PERFORM 0401-PUNCH-CARD.                                       00
010900         READ SOLT-TAPE AT END DISPLAY "NO END-JOB SOLT"                00
011000                 GO TO 0316-EOJ.                                        00
011100     GO TO 0308.                                                        00
011200 0316-EOJ.   MOVE "999999 END-OF-JOB." TO CARDOUT.                      00
011300         PERFORM 0401-PUNCH-CARD.  CLOSE OUTCRD LOCK SOLT-TAPE.         00
011400         PERFORM WITH MCP-INFORMATION.                                  00
011500         DISPLAY "FILE SOLT READY FOR PUNCHING.".                       00
011600         STOP RUN.                                                      00
011700 0401-PUNCH-CARD.    WRITE CARDOUT  INVALID KEY DISPLAY                 00
011800           "CARD FILE EXCEEDED"   STOP RUN.                             00
011900 0405-SOLTFORMAT.     PERFORM 0412-SOLTX VARYING SUB FROM 1 BY 1        00
012000        UNTIL SUB > 5.                                                  00
012100         READ SOLT-TAPE  AT END DISPLAY "NO END-OF-JOB SOLT"            00
012200                 GO TO 0316-EOJ.                                        00
012300         GO TO 0405-SOLTFORMAT.                                         00
012400 0412-SOLTX.     MOVE SOLT-REC-1(SUB) TO CARDOUT.                       00
012500         IF EOJ-CHECK = "END-OF-JOB" GO TO 0316-EOJ.                    00
012600         PERFORM 0401-PUNCH-CARD.                                       00
012700 END-OF-JOB.                                                            00
