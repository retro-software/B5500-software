000100MAKESOLT    000121CA                                                    00
000200                                                                        00
000300************************************************************************00
000400          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00
000500     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00
000600************************************************************************00
000700                                                                        00
000800   MAKESOLT CREATES AN UNBLOCKED "SOLT" TAPE (ACTUALLY IN CARD-IMAGE    00
000900   FORMAT). DURING CREATION OF THIS TAPE, ALL CARDS WILL BE RESEQUENCED 00
001000   BY AN INCREMENT SPECIFIED BY THE OPERATOR AT EXECUTION TIME. THE     00
001100   TAPE WILL HAVE THE ID "SOLT" AND WILL BE ACCEPTABLE TO THE COBOL     00
001200   COMPILER AND PUNCHSOLT WITHOUT A FILE CARD AND TO CARDCROSREF        00
001300   WITH THE FILE CARD DESCRIBED IN THAT PROGRAMS WRITE-UP.              00
001400                                                                        00
001500   THE PROGRAM REQUIRES CARD-IMAGE INPUT EITHER FROM THE CARD-READER    00
001600   OR FROM TAPE. THE ID OF THE INPUT MUST BE "CARD".                    00
001700                                                                        00
001800   EXECUTE CARDS FOR MAKESOLT MUST BE ONE OF THE FOLLOWING:             00
001900                                                                        00
002000      CC EXECUTE UTILITY/MAKESOLT                                       00
002100      CC DATA CARD                                                      00
002200      ******SOURCE DECK******                                           00
002300      CC END                                                            00
002400                                                                        00
002500      CC EXECUTE UTILITY/MAKESOLT; END                                  00
002600         (NOTE THAT INPUT IS A CARD-IMAGE TAPE WITH                     00
002700         THE ID BEING "CARD")                                           00
002800                                                                        00
002900   WILL TYPE OUT. WITH THE MIX AX, THE OPERATOR MUST ENTER A TWO        00
003000   DIGIT NUMBER WHICH WILL BE USED AS THE INCREMENT FOR RESEQUENCING    00
003100   THE DECK. INSTRUCTIONS TO THE OPERATOR MUST INDICATE THE             00
003200   DESIRED INCREMENT.                                                   00
003300                                                                        00
003400                                                                        00
003600 IDENTIFICATION DIVISION.                                               00
003700 PROGRAM-ID.  CARD TO UNBLOCKED SOLT TAPE RESEQUENCED "CDSOLT".         00
003800 DATE-COMPILED.                                                         00
003900 ENVIRONMENT DIVISION.                                                  00
004000 CONFIGURATION SECTION.                                                 00
004100 SOURCE-COMPUTER.   B-5500.                                             00
004200 OBJECT-COMPUTER.   B-5500.                                             00
004300 INPUT-OUTPUT SECTION.                                                  00
004400 FILE-CONTROL.                                                          00
004500         SELECT SOLT-TAPE    ASSIGN TO TAPE.                            00
004600         SELECT CARDIN       ASSIGN TO CARD-READER.                     00
004700 DATA DIVISION.                                                         00
004800 FILE SECTION.                                                          00
004900 FD  SOLT-TAPE                                                          00
005000         LABEL RECORD STANDARD                                          00
005100         VALUE OF ID "SOLT"                                             00
005200         SAVE-FACTOR 10                                                 00
005300         DATA RECORD SOLTREC.                                           00
005400 01  SOLTREC                 SZ 80.                                     00
005500 FD  CARDIN                                                             00
005600         LABEL RECORD STANDARD                                          00
005700         VALUE OF ID "CARD"                                             00
005800         DATA RECORD CARDREC.                                           00
005900 01  CARDREC                 SZ 80.                                     00
006000     05    SEQNO                     PC 999999.                         00
006100     05    FILLER            SZ 1.                                      00
006200     05    EOJ-CHECK                 SZ 10.                             00
006300     05    FILLER                    SZ 63.                             00
006400 WORKING-STORAGE SECTION.                                               00
006500     77   ACCUM-SEQ-NO    VA 0    CMP-1   PC 999999.                    00
006600     77    TWO-DIGIT-INCREMENT          PC 99.                          00
006700 PROCEDURE DIVISION.                                                    00
006800 0206.   ACCEPT TWO-DIGIT-INCREMENT.                                    00
006900         OPEN OUTPUT SOLT-TAPE  INPUT CARDIN.                           00
007000 0210.                                                                  00
007100         READ CARDIN  AT END DISPLAY "END-OF-JOB MISSING"               00
007200                 GO TO 0218-EOJ.                                        00
007300         IF EOJ-CHECK = "END-OF-JOB" GO TO 0218-EOJ.                    00
007400         ADD TWO-DIGIT-INCREMENT TO ACCUM-SEQ-NO.                       00
007500         MOVE ACCUM-SEQ-NO TO SEQNO.                                    00
007600         MOVE CARDREC TO SOLTREC.                                       00
007700         WRITE SOLTREC.                                                 00
007800         GO TO 0210.                                                    00
007900 0218-EOJ.                                                              00
008000        MOVE "999999 END-OF-JOB." TO SOLTREC. WRITE SOLTREC.            00
008100         CLOSE SOLT-TAPE LOCK CARDIN.                                   00
008200         STOP RUN.                                                      00
008300 END-OF-JOB.                                                            00
