000100CARDCROSREF 000115CA                                                    00
000200                                                                        00
000300  **********************************************************************00
000400          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00
000500     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00
000600  **********************************************************************00
000700                                                                        00
000800   THIS PROGRAM WILL PRODUCE A CROSS REFERENCE OF ALL DATA AND          00
000900   PROCEDURE NAMES USED WITHIN A COBOL PROGRAM. INDICATION WILL BE      00
001000   MADE OF THE LINE NUMBER ON WHICH THE NAME IS DEFINED, THE NAME       00
001100   ITSELF AND THE LINE NUMBERS ON WHICH THE NAME IS USED. NAMES         00
001200   WHICH ARE NOT USED WILL BE SO IDENTIFIED. IF A NAME IS MULTIPLY      00
001300   DEFINED, ALL LINE NUMBERS ON WHICH IT IS DEFINED WILL BE             00
001400   IDENTIFIED. NAMES WHICH ARE NEVER DEFINED WILL BE BYPASSED.          00
001500                                                                        00
001600   THE PROGRAM REQUIRES CARD-IMAGE INPUT EITHER FROM A CARD-READER      00
001700   OR FROM A TAPE. THE ID OF THE INPUT MUST BE "CARD" OR A FILE         00
001800   CARD MUST FOLLOW THE EXECUTE CARD:                                   00
001900                                                                        00
002000      CC FILE CARDS = 0000000/|||||||                                   00
002100                                                                        00
002200   WHERE ||||||| IS THE ID OF THE CARD-IMAGE FILE. THE OUTPUT TAPE      00
002202   FROM THE UTILITY PROGRAM "MAKESOLT" IS AN UNBLOCKED TAPE             00
002204   (ACCEPTABLE TO THE COBOL COMPILER AS A SOLT TAPE) WHICH COULD        00
002206   BE USED AS AN INPUT TO "CARDCROSREF" WITH THE FOLLOWING FILE CARD:   00
002208                                                                        00
002210      CC FILE CARDS = 0000000/SOLT                                      00
002212                                                                        00
002300                                                                        00
002400   EXECUTE PACKETS FOR DISKCROSREF MUST BE ONE OF THE FOLLOWING:        00
002500                                                                        00
002600      CC EXECUTE UTILITY/DISKCROSREF                                    00
002602      CC DATA CARD                                                      00
002604        *******SOURCE DECK**********                                    00
002606      CC END                                                            00
002608                                                                        00
002610      CC EXECUTE UTILITY/CARDCROSREF;END.                               00
002612           (NOTE THAT INPUT IS A CARD-IMAGE TAPE WITH                   00
002614            THE ID "CARD")                                              00
002616                                                                        00
002618      CC EXECUTE UTILITY/CARDCROSREF                                    00
002620      CC FILE CARDS = 0000000/|||||||;END                               00
002622           (||||||| IS THE ID OF THE TAPE TO BE USED AS INPUT.)         00
003400                                                                        00
003500                                                                        00
003700 IDENTIFICATION DIVISION.                                               00
003800 PROGRAM-ID. "ANALYZE".                                                 00
003900 DATE-COMPILED.                                                         00
004000 AUTHOR. DAVE MOMENEE, YOUNGSTOWN BRANCH; AND PAUL SLEEPER, STS   X-REF 00
004100         MODIFIED BY C CODDINGTON  BURROUGHS  OAKLAND, CALIF            00
004200     LISTING OF SOURCE PROGRAM IS SUPPRESSED                            00
004300     SORT IS A DISK SORT                                                00
004400     PRINTER FILE IS  "LINE" AND IS ASSIGNED TO BACKUP DISK             00
004500     THE INPUT FILE CARDS IS NOW ASSIGNED TO DISK                       00
004600 REMARKS. PRODUCES A CROSS-REFERENCE LIST OF A COBOL PROGRAM            00
004700     RELIES UPON THE COMPILER FOR SYNTAX ANALYSIS.                      00
004800     UNDEFINED NAMES ARE DELETED.                                       00
004900     AT EXECUTION TIME A FILE CARD MAY BE USED TO SPECIFY               00
005000    THAT A FILE OF UNBLOCKED CARD IMAGES WITH AN ID                     00
005100    OTHER THAN "CARD" IS TO BE CROSS REFERENCED.                        00
005200             EXECUTION FROM THE SPO IS AS FOLLOWS:                      00
005300    CC EXECUTE UTILITY/CARDCROSREF;FILE CARDS=XXX/YYY;END               00
005400     WHERE "XXX/YYY" IS THE MFID AND ID OF THE SOURCE DECK              00
005500    ON AN UNBLOCKED CARD-IMAGE TAPE OR ON CARDS.                        00
005600             EXECUTION MAY ALSO COME FROM REMOTES OR CARD.              00
005700 ENVIRONMENT DIVISION.                                                  00
005800 CONFIGURATION SECTION.                                           X-REF 00
005900 SOURCE-COMPUTER. B-5000.                                         X-REF 00
006000 OBJECT-COMPUTER. B-5500, MEMORY 6000 WORDS.                            00
006100 INPUT-OUTPUT SECTION.                                            X-REF 00
006200 FILE-CONTROL.                                                    X-REF 00
006300     SELECT CARDS ASSIGN TO TAPE.                                 X-REF 00
006400 SELECT LINE  ASSIGN TO PRINTER DISK.                                   00
006500   SELECT SFIL ASSIGN TO SORT DISK.                                     00
006600                                                                        00
006700 DATA DIVISION.                                                   X-REF 00
006800 FILE SECTION.                                                    X-REF 00
006900 FD LINE  LABEL RECORD STANDARD VA ID "LISTING"                   X-REF 00
007000     DATA RECORDS LSTG FRMT1 FRMT2.                                     00
007100 01 LSTG SZ 120. 01 FRMT1. 02 FILLER SZ 24. 02 CRD SZ 80.         X-REF 00
007200 02 FILLER SZ 16. 01 FRMT2. 02 FILLER SZ 8. 02 PSNO SZ 6.         X-REF 00
007300 02 FILLER SZ 3. 02 ALF SZ 32. 02 MSG SZ 70. 04 FILLER OC 10.     X-REF 00
007400 06 LSNO SZ 6. 06 FILLER SZ 1. 02 FILLER SZ 1.                    X-REF 00
007500 FD  CARDS                                                              00
007600                                                                        00
007700            VA ID "CARD"   DATA RECORDS CRDS COLS.                      00
007800                01 CRDS. 02 CSNO SZ 6. 02 NXT SZ 1. 02 CBR SZ 24. X-REF 00
007900 02 FILLER SZ 49. 01 COLS. 02 COL SZ 1 OC 80.                     X-REF 00
008000 SD SFIL DATA RECORD SREC. 01 SREC. 02 SALF SZ 32. 02 FILLER      X-REF 00
008100  REDEFINES SALF. 04 CHR SZ 1 OC 32. 02 STYP SZ 1. 2 SSNO SZ 6.   X-REF 00
008200 WORKING-STORAGE SECTION.                                         X-REF 00
008300 77 I PC 99 CMP-1. 77 LI PC 99 CMP-1. 77 CI PC 99 CMP-1.          X-REF 00
008400 77 WI PC 99 CMP-1. 77 EOFSW PC 9 CMP-1.                          X-REF 00
008500 77 WALF SZ 32.                                                         00
008600 PROCEDURE DIVISION.                                              X-REF 00
008700 INIT. OPEN INPUT CARDS.                                                00
008800     MOVE ZERO TO EOFSW GO TO SORTER.                             X-REF 00
008900 IP SECTION.                                                      X-REF 00
009000 RD.  READ CARDS AT END MOVE 1 TO EOFSW .                               00
009100 IE. MOVE 8 TO CI. PERFORM LOOK. IF SALF="FILE" PERFORM LOOK IF   X-REF 00
009200  SALF="SECTION" GO TO DA. PERFORM RD IF EOFSW!0 GO TO EF ELSE    X-REF 00
009300      GO TO IE.                                                         00
009400 DA. PERFORM RD IF EOFSW!0 GO TO EF. MOVE 8 TO CI. PERFORM LOOK.  X-REF 00
009500  IF SALF="PROCEDURE"PERFORM LOOK IF"DIVISION" MOVE 8 TO CI       X-REF 00
009600 PERFORM RD GO TO P1. IF NXT ! SPACE GO TO DA. MOVE 7 TO CI.      X-REF 00
009700 D1.  ADD 1 TO CI IF COL(CI) = SPACE IF CI<72 GO TO D1 ELSE       X-REF 00
009800 GO TO DA. ADD 1 TO CI IF COL(CI)!SPACE ADD 1 TO CI IF COL(CI)    X-REF 00
009900     ! SPACE GO TO DA.                                            X-REF 00
010000 LOOK. IF CI<73 IF COL(CI)=SPACE ADD 1 TO CI GO TO LOOK ELSE PERFOX-REF 00
010100#    RM COL-CHR THRU C-C1 ELSE PERFORM RD MOVE 7 TO CI GO TO LOOK.X-REF 00
010200 D3. IF SALF!"FILLER" MOVE 1 TO STYP RELEASE SREC. PERFORM LOOK.  X-REF 00
010300  IF SALF="REDEFINES" MOVE 2 TO STYP PERFORM LOOK RELEASE SREC    X-REF 00
010400  ELSE IF SALF="."GO TO D1. PERFORM SKP-NOTE GO TO D1.            X-REF 00
010500 COL-CHR.  MOVE 1 TO WI. MOVE SPACES TO SALF. MOVE CSNO TO SSNO.  X-REF 00
010600 C-C1. MOVE COL(CI) TO CHR(WI) ADD 1 TO CI ADD 1 TO WI.           X-REF 00
010700  IF CI<73 IF COL(CI)<"A"IF!"-"NEXT SENTENCE ELSE GO TO C-C1      X-REF 00
010800     ELSE IF="!"NEXT SENTENCE ELSE IF="|"NEXT SENTENCE ELSE GO TO X-REF 00
010900               C-C1.                                              X-REF 00
011000 C-C2. IF SALF EQUALS"NOTE" PERFORM SKP-NOTE. SUBTRACT 1 FROM CI. X-REF 00
011100 C-C3.                                                            X-REF 00
011200  IF CI < 72 ADD 1 TO CI IF COL(CI)<"A" THEN IF = QUOTE NEXT      X-REF 00
011300  SENTENCE ELSE IF = "-" NEXT SENTENCE ELSE                       X-REF 00
011400     GO TO C-C3 ELSE NEXT SENTENCE ELSE PERFORM RD IF EOFSW !     X-REF 00
011500     ZERO NEXT SENTENCE ELSE  MOVE 7 TO CI IF NXT ! SPACE         X-REF 00
011600     PERFORM C-C3 GO TO C-C1 ELSE GO TO C-C3.                     X-REF 00
011700 SKP-LIT. ADD 1 TO CI. IF CI>72 PERFORM RD IF EOFSW!0 NEXT SENT   X-REF 00
011800-    ENCE ELSE MOVE 7 TO CI IF NXT!SPACE PERFORM SKP-LIT GO TO    X-REF 00
011900  SKP-LIT ELSE NEXT SENTENCE ELSE IF COL(CI)!QUOTE GO TO SKP-LIT  X-REF 00
012000     ELSE IF CI<72 ADD 1 TO CI.                                   X-REF 00
012100 SKP-NOTE.                                                        X-REF 00
012200     IF COL(CI) = QUOTE PERFORM  SKP-LIT.                               00
012300     IF COL(CI) = "." ADD 1 TO CI IF COL(CI)!SPACE IF CI>72       X-REF 00
012400     NEXT SENTENCE ELSE GO TO SKP-NOTE ELSE NEXT SENTENCE         X-REF 00
012500     ELSE ADD 1 TO CI IF CI < 73 GO TO  SKP-NOTE ELSE             X-REF 00
012600     PERFORM RD IF EOFSW ! ZERO NEXT SENTENCE ELSE  MOVE  7 TO    X-REF 00
012700     CI GO TO SKP-NOTE.                                           X-REF 00
012800 P1. IF COL(CI) = QUOTE PERFORM SKP-LIT     .                     X-REF 00
012900     IF CI = 8 MOVE 1 TO STYP ELSE MOVE 2 TO STYP.                X-REF 00
013000 PERFORM COL-CHR THRU C-C3. IF EOFSW ! ZERO GO TO EF.             X-REF 00
013100   RELEASE SREC. GO TO P1.                                        X-REF 00
013200 EF.  CLOSE CARDS WITH RELEASE.                                   X-REF 00
013300 OP SECTION.                                                      X-REF 00
013400 XYZ.    OPEN OUTPUT LINE.                                              00
013500 PO. MOVE 1 TO I. MOVE "  DEFINED ON:" TO LSTG. MOVE "  NAME" TO  X-REF 00
013600 ALF. MOVE "REFERENCED ON:" TO MSG. WRITE LSTG BEFORE 2 LINES.    X-REF 00
013700     MOVE SPACES TO LSTG.                                         X-REF 00
013800 RT. RETURN SFIL AT END GO TO FE.                                 X-REF 00
013900     IF SALF=WALF GO TO EQ. IF STYP=2 GO TO RT.                   X-REF 00
014000     IF I = ZERO MOVE "NO EXPLICIT REFERENCE" TO MSG ELSE         X-REF 00
014100 MOVE ZERO TO I. WRITE LSTG BEFORE 2 LINES. MOVE SPACES TO LSTG.  X-REF 00
014200   MOVE SSNO TO PSNO. MOVE SALF TO ALF WALF. GO TO RT.            X-REF 00
014300 EQ.  IF STYP = 1 MOVE "DUPLICATE NAME" TO MSG WRITE LSTG         X-REF 00
014400     MOVE SPACES TO LSTG                                                00
014500   MOVE SSNO TO PSNO GO TO RT.                                    X-REF 00
014600 IF I LESS 10 ADD 1 TO I ELSE MOVE 1 TO I WRITE LSTG              X-REF 00
014700     MOVE SPACES TO LSTG.                                         X-REF 00
014800     MOVE SSNO TO LSNO(I) GO TO RT.                               X-REF 00
014900 FE. IF I=ZERO MOVE "NO EXPLICIT REFERENCE" TO MSG.               X-REF 00
015000     WRITE LSTG.  CLOSE LINE  WITH RELEASE.                             00
015100 SORTING SECTION.                                                 X-REF 00
015200 SORTER. SORT SFIL ON ASCENDING KEY SALF STYP SSNO                X-REF 00
015300  INPUT PROCEDURE IP OUTPUT PROCEDURE OP.                         X-REF 00
015400 STOPPER. STOP RUN.                                               X-REF 00
015500 END-OF-JOB.                                                      X-REF 00
