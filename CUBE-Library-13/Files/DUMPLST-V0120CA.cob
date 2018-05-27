000100DUMPLIST    000120CA                                                    00
000200                                                                        00
000300************************************************************************00
000400          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00
000500     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00
000600************************************************************************00
000700                                                                        00
000800   THIS PROGRAM LISTS INFORMATION FROM THE SYSTEM LOG BUT DOES          00
000900   NOT RESET THE LOG POINTER (I.E., VOID THE LOG). THE NAMES OF         00
001000   THE PROGRAMS RUN AND THE OPTION (RUN,EXECUTE,ETC.) ARE ALIGNED       00
001100   FOR EASE OF READING.                                                 00
001200                                                                        00
001300                                                                        00
001500 IDENTIFICATION DIVISION.                                               00
001600 PROGRAM-ID.  "CHRISLOG".                                               00
001700 DATE-COMPILED.                                                         00
001800 AUTHOR. C CODDINGTON   BURROUGHS   OAKLAND.                            00
001900 REMARKS.                                                               00
002000         DUMP LOG TO A DISK FILE.                                       00
002100         PREPARE A LIST OF THE LOG.                                     00
002200 ENVIRONMENT DIVISION.                                                  00
002300 CONFIGURATION SECTION.                                                 00
002400 SOURCE-COMPUTER.   B-5500.                                             00
002500 OBJECT-COMPUTER.   B-5500.                                             00
002600 INPUT-OUTPUT SECTION.                                                  00
002700 FILE-CONTROL.                                                          00
002800         SELECT INPUT-LOG      ASSIGN TO DISK.                          00
002900         SELECT HOLD-FILE      ASSIGN TO DISK.                          00
003000         SELECT PRINT-FILE     ASSIGN TO PRINTER DISK.                  00
003100         SELECT INDATA         ASSIGN TO DISK.                          00
003200 I-O-CONTROL.                                                           00
003300         APPLY TECHNIQUE-A ON INPUT-LOG                                 00
003400         APPLY TECHNIQUE-A ON HOLD-FILE                                 00
003500         APPLY TECHNIQUE-A ON INDATA                                    00
003600 DATA DIVISION.                                                         00
003700 FILE SECTION.                                                          00
003800 MD  INPUT-LOG                                                          00
003900         FILE 1500 SEGMENTS                                             00
004000         ACCESS SEQUENTIAL                                              00
004100         BLOCK 30 RECORDS                                               00
004200         VALUE OF ID "LOG"                                              00
004300         DATA RECORD INLOG.                                             00
004400 01          INLOG           SZ 40.                                     00
004500     05      CTL-WORD-LOG            PC 9(8).                           00
004600     05      FILLER                  PC X(32).                          00
004700 MD  HOLD-FILE                                                          00
004800              FILE CONTAINS  10 | 1200 RECORDS                          00
004900         ACCESS SEQUENTIAL                                              00
005000         BLOCK 30 RECORDS                                               00
005100         VALUE OF ID "LOGHOLD"   SAVE-FACTOR 007                        00
005200         DATA RECORD OUTLOG.                                            00
005300 01           OUTLOG         SZ 40.                                     00
005400 MD  INDATA                                                             00
005500         FILE CONTAINS 10 | 1200 RECORDS                                00
005600         ACCESS SEQUENTIAL                                              00
005700         BLOCK 30 RECORDS                                               00
005800         VALUE OF ID "LOGHOLD"                                          00
005900         DATA RECORD INREC.                                             00
006000 01           INREC          SZ 40.                                     00
006100     05       CTL-WORD-IN            PC 9(8).                           00
006200     05       32-COLS                PC X(32).                          00
006300 FD  PRINT-FILE                                                         00
006400         VALUE OF ID "LOGDUMP"                                          00
006500         DATA RECORD PRINT-REC.                                         00
006600 01           PRINT-REC      SZ 132.                                    00
006700     05        FILLER                SZ 1.                              00
006800     05       START-TIME-HOUR        PC 99.                             00
006900     05       START-TIME-MIN         PC 99.                             00
007000     05       FILLER                 SZ 2.                              00
007100     05       DEVICE-CODE            SZ 4.                              00
007200       10     STOP-TIME-HOUR         PC 99.                             00
007300       10     STOP-TIME-MIN          PC 99.                             00
007400     05       ELAPSED-MIN            PC ZZZZZZ.                         00
007500     05       DECIMAL-1              PC X.                              00
007600     05       ELAPSED-SEC            PC 99.                             00
007700     05       PROCESS-MIN            PC ZZZZZZ.                         00
007800     05       DECIMAL-2              PC X.                              00
007900     05       PROCESS-SEC            PC 99.                             00
008000     05       I-O-MIN                PC ZZZZZZ.                         00
008100     05       DECIMAL-3              PC X.                              00
008200     05       I-O-SEC                PC 99.                             00
008300     05       PRORATE-MIN            PC Z(6).                           00
008400     05       DECIMAL-4              PC X.                              00
008500     05       PRORATE-SEC            PC 99.                             00
008600     05       FILLER                 SZ 2.                              00
008700     05       RESULT                 PC X(6).                           00
008800     05        FILLER                SZ 2.                              00
008900     05       CTL-CARD       SZ 72.                                     00
009000     05       PRINT-RUN-SEQ          PC 999.                            00
009100 WORKING-STORAGE SECTION.                                               00
009200 77           ACCUM-ELAPSED  CMP-1   PC 9(8)     VA 0.                  00
009300 77           ACCUM-PROCESS  CMP-1   PC 9(8)     VA 0.                  00
009400 77           ACCUM-I-O      CMP-1   PC 9(8)     VA 0.                  00
009500 77           ACCUM-PRORATE  CMP-1   PC 9(8)     VA 0.                  00
009600 77           24-HOUR-CONSTANT       VA 5184000  PC 9(8)   CMP-1.       00
009700 77           CONSTANT-1     CMP-1   PC 9(8)     VA 1.                  00
009800 77           CONSTANT-2     CMP-1   PC 9(8)     VA 2.                  00
009900 77           CONSTANT-3     CMP-1   PC 9(8)     VA 3.                  00
010000 77           CONSTANT-4     CMP-1   PC 9(8)     VA 4.                  00
010100 77           CONSTANT-5     CMP-1   PC 9(8)     VA 5.                  00
010200 77           CONSTANT-32    CMP-1   PC 9(8)     VA 32.                 00
010300 77           CONSTANT-3600  CMP-1   PC 9(8)     VA 3600.               00
010400 77           BYPASSER       CMP-1   PC 9(8)     VA 0.                  00
010500 77           CONSTANT-216K  CMP-1   PC 9(8)     VA 216000.             00
010600 77          SUB2            CMP-1           PC 9(8).                   00
010700 77           SUB1           CMP-1   PC 99.                             00
010800 77           SUB            CMP-1   PC 99.                             00
010900 77           SUB-APPL       CMP-1   PC 99.                             00
011000 77           SUB-SCRIPT     CMP-1   PC 9        VA 0.                  00
011100 77           LINE-COUNT     CMP-1   PC 99.                             00
011200 77          EOJ-TERMINATION         PC 9(8) CMP-1.                     00
011300 77          SYNTAX-TERMINATION      PC 9(8) CMP-1.                     00
011400 77          DS-TERMINATION          PC 9(8) CMP-1.                     00
011500 77          ABORT-TERMINATION       PC 9(8) CMP-1.                     00
011600 01  HISTORY-REC                     SZ 120.                            00
011700     05       H-START                PC 9(8)               VA 0.        00
011800     05       H-STOP                 PC 9(8)               VA 0.        00
011900     05       TP-OR-C                PC X.                              00
012000     05       REC-CODE               PC 9.                              00
012100     05       RUN-DATE               PC X(5).                           00
012200     05       RUN-CODE               PC X.                              00
012300     05        FILLER                SZ 1.                              00
012400     05       APPLICATION-NAME       PC X(7).                           00
012500     05        FILLER                SZ 1.                              00
012600     05       T-OR-P                 PC X.                              00
012700           88 TEST-RUN       VA "T".                                    00
012800           88 PRODUCTION-RUN VA "P".                                    00
012900     05       PROG-NO                PC X(6).                           00
013000     05       PRGRMR-INITIALS        PC XXX.                            00
013100     05       RERUN-CODE             PC X.                              00
013200     05       RERUN-INITIALS         PC XXX.                            00
013300     05       FINISH-CODE            PC 9.                              00
013400     05       APPLICATION-AREA       PC X(16).                          00
013500     05       LOG-RECORD             PC X(40).                          00
013600     05       RUN-COUNTER            PC 9999.                           00
013700     05        FILLER                SZ 12.                             00
013800 01         A-RECORD-HOLDING SZ 160.                                    00
013900     02       FILLERAXXX.                                               00
014000      05      SUB-REC-1              PC X(40).                          00
014100      05      SUB-REC-2              PC X(40).                          00
014200      05      SUB-REC-3              PC X(40).                          00
014300      05      SUB-REC-4              PC X(40).                          00
014400     02       RECORD-HOLDING REDEFINES FILLERAXXX.                      00
014500     05       CODE-3         CMP     PC 9(8).                           00
014600     05       CTL-CARD-CONT          PC X(72).                          00
014700     05       72-COLS REDEFINES CTL-CARD-CONT.                          00
014800       10     CARD-COL       OC 72   PC X.                              00
014900     05       GP-CODE        CMP     PC 9(8).                           00
015000     05       HOLD-FILES     CMP     PC 9(8).                           00
015100     05       HOLD-PROCESS   CMP     PC 9(8).                           00
015200     05       HOLD-I-O       CMP     PC 9(8).                           00
015300     05       HOLD-PRORATE   CMP     PC 9(8).                           00
015400     05        FILLER                SZ 3.                              00
015500     05       HOLD-JULIAN            PC 9(5).                           00
015600     05       HOLD-START     CMP     PC 9(8).                           00
015700     05       HOLD-STOP      CMP     PC 9(8).                           00
015800     05       HOLD-FINISH    CMP     PC 9(8).                           00
015900     05       FILLER                 SZ 8.                              00
016000   02         REC40EC REDEFINES RECORD-HOLDING       OC 4.              00
016100     05       SUB-REC        SZ 40.                                     00
016200       10     CODE-REC               PC 9(8).                           00
016300       10     FILLER                 SZ 32.                             00
016400 01           72-CHAR-HOLD                       SZ 72.                 00
016500 01           FILE-RECORDS   SZ 40.                                     00
016600     05       FILLER                 SZ 1.                              00
016700     05       FILE-MFID              PC X(7).                           00
016800     05       FILLER                 SZ 1.                              00
016900     05       FILE-ID                PC X(7).                           00
017000     05       FILE-REEL-NO           PC X(3).                           00
017100     05       FILE-CREATION          PC X(5).                           00
017200     05       FILE-CY-NOE-UNIT.                                         00
017300       10     FILE-CYCLE             PC X(2).                           00
017400       10     FILLER                 SZ 2.                              00
017500       10     ERROR-COUNT            PC XX.                             00
017600       10     UNIT-CODE              PC X.                              00
017700       10     FILLER                 SZ 1.                              00
017800     05       TIME-OPEN              PC 9(8)     CMP.                   00
017900 01           TABLE-OF-ERRORS.                                          00
018000     05       TABLE-ENTRIES.                                            00
018100       10     DEVICES        VA "MTAMTBMTCMTDMTEMTFMTHMTJMTKMTLMTM      00
018200-    "MTNMTPMTRMTSMTTDRADRBDKADKBLPALPBCPACRACRBSPOPPAPRAPPBPRBDCA      00
018300-    ""                              PC X(93).                          00
018400       10     DEVICE-NAME REDEFINES DEVICES OC 31  PC XXX.              00
018500       10     FILLER                 SZ 3.                              00
018600       10     DEVICE-ERRORS          OC 31  CMP      PC 9(8).           00
018700       10     DEVICE-OPEN            OC 31  CMP      PC 9(8).           00
018800     05       CONVERT-CMP            CMP     PC 9(8).                   00
018900     05       CHARACTER-MODE REDEFINES CONVERT-CMP.                     00
019000       10     FILLER                 SZ 6.                              00
019100       10     HOLD-2-CHAR            PC XX.                             00
019200       10     SINGLE-HOLD REDEFINES HOLD-2-CHAR.                        00
019300         15   FILLER                 SZ 1.                              00
019400         15   HOLD-1-CHAR            PC X.                              00
019500     05       CONTROL-WORD-OF-GO     PC XX.                             00
019600           88 NO-GO          VA "NO".                                   00
019700     05       HOLD-TODAYS-DATE.                                         00
019800       10     MM                     PC 99.                             00
019900       10     DD                     PC 99.                             00
020000       10     YY                     PC 99.                             00
020100     05       GO-IF-OK-NO-IF-NOT     PC XX.                             00
020200           88 NO-INPUT       VA "GO".                                   00
020300     05       PAGE-NO    VA 0        PC 9999.                           00
020400     05       HOLD-RUN-SEQ           PC 999.                            00
020500     05       END-MARK  VA END       PC X.                              00
020600     05       COMPILE-SWITCH         PC 9.                              00
020700     05 FILLER SZ 5.                                                    00
020800     05       RUN-TYPE       CMP     PC 9(8) OC 6.                      00
020900 01  CTLXXX.                                                            00
021000     05      CTL-COLS        OC 72           PC X.                      00
021100 01           PRINT-MAIN-HEADING     SZ 132.                            00
021200     05       FILLER VA "B-5500 DAILY RUN LOG         (UTILITYLOG/      00
021300-    "DUMPLIST)"                             PC X(100).                 00
021400     05       REPORT-DATE                    PC 99/99/99.               00
021500     05       FILLER VA SPACES                   SZ 4.                  00
021600     05       FILLER         VA SPACES       SZ 5.                      00
021700     05       FILLER VA "    PAGE "              PC X(9).               00
021800     05       HEADING-PAGE-NO                    PC ZZZZ.               00
021900     05 FILLER PC XX VA SPACES.                                         00
022000 01           LOG-DATE-HEADING       SZ 132.                            00
022100     05       FILLER VA "LOG DATE  "             PC X(10).              00
022200     05       ENTRY-DATE                         PC 9(5).               00
022300     05       FILLER     VA SPACES               PC X(117).             00
022400 01           TEST-FIELD             SZ 16.                             00
022500     05       TEST-1     OC 16       PC X.                              00
022600 01  CALL-LOG-ANAL.                                                     00
022700     05      FILLER  VA "CC EXECUTE ANALOF/LOGGING;END." SZ 80.         00
022800 PROCEDURE DIVISION.                                                    00
022900 DUMP-LOG SECTION.                                                      00
023000 0703-BEGIN.                                                            00
023100         OPEN OUTPUT PRINT-FILE.                                        00
023200         ACCEPT CONTROL-WORD-OF-GO.                                     00
023300         IF NO-GO GO TO 0807-LOG-PROCESS.                               00
023400         OPEN INPUT INPUT-LOG.                                          00
023500         OPEN OUTPUT HOLD-FILE.                                         00
023600         MOVE SPACES TO PRINT-REC.                                      00
023700         WRITE PRINT-REC BEFORE ADVANCING TO CHANNEL 1.                 00
023800         MOVE TODAYS-DATE TO REPORT-DATE.                               00
023900         MOVE CONSTANT-1 TO HEADING-PAGE-NO AND PAGE-NO.                00
024000         WRITE PRINT-REC FROM PRINT-MAIN-HEADING                        00
024100     BEFORE ADVANCING 2 LINES.                                          00
024200         MOVE "ERROR RECORDS FOUND IN LOG" TO PRINT-REC.                00
024300         WRITE PRINT-REC BEFORE ADVANCING 2 LINES                       00
024400         READ INPUT-LOG  AT END GO TO 0801-LOG-ERROR.                   00
024500         MOVE INLOG TO OUTLOG.                                          00
024600 0703X.  WRITE OUTLOG INVALID KEY DISPLAY "NOTIFY TECH REP"             00
024700             ACCEPT CONTROL-WORD-OF-GO GO TO 0703X.                     00
024800 0713-READ-START.                                                       00
024900         READ INPUT-LOG                                                 00
025000             AT END GO TO 0801-LOG-ERROR.                               00
025100         MOVE INLOG TO SUB-REC-1.                                       00
025200         IF CTL-WORD-LOG  = 4 GO TO 0802-END-LOG-DUMP.                  00
025300         IF CTL-WORD-LOG = 0 AND CARD-COL(9) = 0 AND CARD-COL(10)       00
025400     = 0 AND CARD-COL(11) = 0 AND CARD-COL(12) = 0                      00
025500     AND COMPILE-SWITCH = 1 GO TO 0794.                                 00
025600        MOVE 0 TO COMPILE-SWITCH.                                       00
025700         IF CTL-WORD-LOG ! 3                                            00
025800             PERFORM 0758-LOG-ERROR-REJECT                              00
025900             GO TO 0713-READ-START.                                     00
026000         READ INPUT-LOG                                                 00
026100             AT END GO TO 0801-LOG-ERROR.                               00
026200         MOVE INLOG TO SUB-REC-2.                                       00
026300     MOVE CTL-CARD-CONT TO PRINT-REC.                                   00
026400     WRITE PRINT-REC.                                                   00
026500         MOVE SPACES TO CTLXXX.                                         00
026600         MOVE 1     TO SUB AND SUB2.                                    00
026700 0721-CHECK-FOR-CC.                                                     00
026800         IF CARD-COL(SUB) > "9"                                         00
026900             MOVE CARD-COL(SUB) TO CTL-COLS(1)                          00
027000             ADD 1 TO SUB                                               00
027100             GO TO 0737-FIND-EX-RUN-ETC.                                00
027200         IF CARD-COL(SUB) = "C" AND CARD-COL(SUB+1) = "C"               00
027300             MOVE "C" TO CTL-COLS(1)                                    00
027400             MOVE "C" TO CTL-COLS(2)                                    00
027500             ADD 2 TO SUB                                               00
027600             GO TO 0737-FIND-EX-RUN-ETC.                                00
027700         ADD 1 TO SUB.                                                  00
027800         IF SUB EXCEEDS 72 GO TO 0756-CTL-ERROR.                        00
027900         GO TO 0721-CHECK-FOR-CC.                                       00
028000                                                                        00
028100 0737-FIND-EX-RUN-ETC.                                                  00
028200         IF SUB EXCEEDS 72 GO TO 0756-CTL-ERROR.                        00
028300         IF CARD-COL(SUB) = " "                                         00
028400             ADD 1 TO SUB                                               00
028500             GO TO 0737-FIND-EX-RUN-ETC.                                00
028600         IF CARD-COL(SUB) = "U" GO TO 0761-USER-MSG.                    00
028700         IF CARD-COL(SUB) = "C" OR "E"                                  00
028800             MOVE CARD-COL(SUB) TO CTL-COLS(4)                          00
028900             MOVE CARD-COL(SUB+1) TO CTL-COLS(5)                        00
029000             MOVE CARD-COL(SUB+2) TO CTL-COLS(6)                        00
029100             MOVE CARD-COL(SUB+3) TO CTL-COLS(7)                        00
029200             MOVE CARD-COL(SUB+4) TO CTL-COLS(8)                        00
029300             MOVE CARD-COL(SUB+5) TO CTL-COLS(9)                        00
029400             MOVE CARD-COL(SUB+6) TO CTL-COLS(10)                       00
029500             MOVE 12 TO SUB2                                            00
029600             ADD 7 TO SUB                                               00
029700             GO TO 0784-REMAINING-CTL.                                  00
029800         IF CARD-COL(SUB) = "R"                                         00
029900             MOVE "R" TO CTL-COLS(4)                                    00
030000             MOVE "U" TO CTL-COLS(5)                                    00
030100             MOVE "N" TO CTL-COLS(6)                                    00
030200             MOVE 12 TO SUB2                                            00
030300             ADD 3 TO SUB                                               00
030400             GO TO 0784-FIND-REMAINING-CTL.                             00
030500 0756-CTL-ERROR.                                                        00
030600         MOVE "1" TO PRINT-RUN-SEQ.                                     00
030700             MOVE SUB-REC-1 TO PRINT-REC.                               00
030800             WRITE PRINT-REC.                                           00
030900             MOVE SUB-REC-2 TO PRINT-REC.                               00
031000             WRITE PRINT-REC.                                           00
031100             ADD 2 TO LINE-COUNT.                                       00
031200             GO TO 0713-READ-START.                                     00
031300 0758-LOG-ERROR-REJECT.                                                 00
031400         MOVE "3" TO PRINT-RUN-SEQ.                                     00
031500             MOVE SUB-REC-1 TO PRINT-REC.                               00
031600             WRITE PRINT-REC.    ADD 1 TO LINE-COUNT.                   00
031700 0761-USER-MSG.                                                         00
031800         IF CARD-COL(SUB) = "=" GO TO 0765-FIND-USER-NO.                00
031900         ADD 1 TO SUB.                                                  00
032000         IF SUB > 72 GO TO 0756-CTL-ERROR.                              00
032100         GO TO 0761-USER-MSG.                                           00
032200 0765-FIND-USER-NO.                                                     00
032300             ADD 1 TO SUB.                                              00
032400         IF CARD-COL(SUB) ! SPACE                                       00
032500             MOVE CARD-COL(SUB) TO CTL-COLS(65)                         00
032600             ADD 1 TO SUB                                               00
032700         ELSE ADD 1 TO SUB  GO TO 0765-FIND-USER-NO.                    00
032800             IF CARD-COL(SUB) = SPACE OR ";" GO TO 0782.                00
032900                 MOVE CARD-COL(SUB) TO CTL-COLS(66).                    00
033000                 ADD 1 TO SUB.                                          00
033100             IF CARD-COL(SUB) = SPACE OR ";" GO TO 0782.                00
033200                 MOVE CARD-COL(SUB) TO CTL-COLS(67).                    00
033300                 ADD 1 TO SUB.                                          00
033400             IF CARD-COL(SUB) = SPACE OR ";" GO TO 0782.                00
033500                 MOVE CARD-COL(SUB) TO CTL-COLS(68).                    00
033600                 ADD 1 TO SUB.                                          00
033700             IF CARD-COL(SUB) = SPACE OR ";" GO TO 0782.                00
033800                 MOVE CARD-COL(SUB) TO CTL-COLS(69).                    00
033900                 ADD 1 TO SUB.                                          00
034000             IF CARD-COL(SUB) = SPACE OR ";" GO TO 0782.                00
034100                 MOVE CARD-COL(SUB) TO CTL-COLS(70).                    00
034200                 ADD 1 TO SUB.                                          00
034300             IF CARD-COL(SUB) = SPACE OR ";" GO TO 0782.                00
034400                 MOVE CARD-COL(SUB) TO CTL-COLS(71).                    00
034500 0782.           ADD 1 TO SUB.                                          00
034600         IF CARD-COL(SUB) = ";" OR SPACE GO TO 0782.                    00
034700         GO TO 0737-FIND-EX-RUN-ETC.                                    00
034800 0784-REMAINING-CTL.                                                    00
034900 0784-FIND-REMAINING-CTL.                                               00
035000         IF CARD-COL(SUB) = SPACE                                       00
035100             ADD 1 TO SUB                                               00
035200             GO TO 0784-REMAINING-CTL.                                  00
035300     IF CTL-COLS(4) = "C" MOVE 1 TO COMPILE-SWITCH.                     00
035400         IF CARD-COL(SUB) = "P"                                         00
035500             AND CARD-COL(SUB+1) = "R"                                  00
035600             AND CARD-COL(SUB+2) = "N"                                  00
035700             AND CARD-COL(SUB+3) = "P"                                  00
035800             AND CARD-COL(SUB+4) = "B"                                  00
035900             AND CARD-COL(SUB+5) = "T"                                  00
036000                 NEXT SENTENCE   ELSE      GO TO 0792.                  00
036100                 READ INPUT-LOG AT END GO TO 0801-LOG-ERROR.            00
036200                 IF CTL-WORD-LOG = 4 GO TO 0802-END-LOG-DUMP.           00
036300                 READ INPUT-LOG AT END GO TO 0801-LOG-ERROR.            00
036400                 IF CTL-WORD-LOG = 4 GO TO 0802-END-LOG-DUMP.           00
036500                 READ INPUT-LOG AT END GO TO 0801-LOG-ERROR.            00
036600                 IF CTL-WORD-LOG = 4 GO TO 0802-END-LOG-DUMP.           00
036700                 READ INPUT-LOG AT END GO TO 0801-LOG-ERROR.            00
036800                 IF CTL-WORD-LOG = 4 GO TO 0802-END-LOG-DUMP.           00
036900                 GO TO 0713-READ-START.                                 00
037000 0792.   IF SUB2 < 65 AND CARD-COL(SUB) ! ";" AND SUB < 73              00
037100     AND CARD-COL(SUB) ! END-MARK                                       00
037200             MOVE CARD-COL(SUB) TO CTL-COLS(SUB2)                       00
037300             ADD 1 TO SUB    ADD 1 TO SUB2                              00
037400             GO TO 0792.                                                00
037500         MOVE CTLXXX TO 72-COLS.                                        00
037600         MOVE SUB-REC-1 TO OUTLOG.                                      00
037700         WRITE OUTLOG INVALID KEY DISPLAY "LOG OVERFLOW"                00
037800             GO TO 0801-LOG-ERROR.                                      00
037900         MOVE SUB-REC-2 TO OUTLOG.                                      00
038000         WRITE OUTLOG INVALID KEY DISPLAY "LOG-OVERFLOW"                00
038100             GO TO 0801-LOG-ERROR.                                      00
038200         READ INPUT-LOG  AT END GO TO 0801-LOG-ERROR.                   00
038300         IF CTL-WORD-LOG = 4 GO TO 0802-END-LOG-DUMP.                   00
038400 0794.   MOVE INLOG TO OUTLOG AND SUB-REC-3.                            00
038500         WRITE OUTLOG INVALID KEY DISPLAY "LOG OVERFLOW"                00
038600             GO TO 0801-LOG-ERROR.                                      00
038700         READ INPUT-LOG AT END GO TO 0801-LOG-ERROR.                    00
038800         IF CTL-WORD-LOG = 4 GO TO 0802-END-LOG-DUMP.                   00
038900         MOVE INLOG TO OUTLOG.                                          00
039000         WRITE OUTLOG INVALID KEY DISPLAY "LOG OVERFLOW"                00
039100             GO TO 0801-LOG-ERROR.                                      00
039200         IF HOLD-FILES = 0          GO TO 0713-READ-START.              00
039300 0795.   READ INPUT-LOG INTO OUTLOG  AT END GO TO 0801-LOG-ERROR.       00
039400         IF CTL-WORD-LOG = 4 GO TO 0802-END-LOG-DUMP.                   00
039500         WRITE OUTLOG INVALID KEY DISPLAY "LOG OVERFLOW"                00
039600             GO TO 0801-LOG-ERROR.                                      00
039700         ADD 1 TO SUB-APPL. IF SUB-APPL ! HOLD-FILES GO TO 0795.        00
039800         MOVE ZERO TO SUB-APPL.                                         00
039900         GO TO 0713-READ-START.                                         00
040000 0801-LOG-ERROR.                                                        00
040100         DISPLAY "SYSTEM/LOG CAME TO EOF BEFORE CODE 4 RECORD.".        00
040200         DISPLAY "NOTIFY BURROUGHS SYSTEMS REPRESENTATIVE.".            00
040300 0802-END-LOG-DUMP.                                                     00
040400         MOVE "00000004" TO OUTLOG.                                     00
040500         WRITE OUTLOG INVALID KEY DISPLAY "LOG OVERFLOWED".             00
040600         CLOSE INPUT-LOG WITH RELEASE.                                  00
040700         CLOSE HOLD-FILE WITH RELEASE.                                  00
040800 0806-LOG-PROCESS SECTION.                                              00
040900 0807-LOG-PROCESS.                                                      00
041000         OPEN INPUT INDATA.                                             00
041100             READ INDATA  AT END GO TO 1101-END-LOG-LIST.               00
041200 0809.   MOVE SPACES TO PRINT-REC.                                      00
041300         PERFORM 1012-OFLOW.                                            00
041400         READ INDATA   AT END GO TO 1101-END-LOG-LIST.                  00
041500         MOVE INREC TO SUB-REC (1).                                     00
041600 0812.   READ INDATA   AT END GO TO 1101-END-LOG-LIST.                  00
041700         MOVE INREC TO SUB-REC (2).                                     00
041800         READ INDATA   AT END GO TO 1101-END-LOG-LIST.                  00
041900         MOVE INREC TO SUB-REC (3).                                     00
042000         READ INDATA   AT END GO TO 1101-END-LOG-LIST.                  00
042100         MOVE INREC TO SUB-REC (4).                                     00
042200 0819.                                                                  00
042300         IF HOLD-JULIAN ! ENTRY-DATE                                    00
042400              MOVE HOLD-JULIAN TO ENTRY-DATE                            00
042500              MOVE SPACES TO PRINT-REC                                  00
042600              IF LINE-COUNT EXCEEDS 37                                  00
042700                   PERFORM 1012-OFLOW                                   00
042800                   WRITE PRINT-REC FROM LOG-DATE-HEADING                00
042900         MOVE SPACES TO PRINT-REC                                       00
043000                   ADD 2 TO LINE-COUNT                                  00
043100                   ELSE                                                 00
043200         WRITE PRINT-REC                                                00
043300                        WRITE PRINT-REC FROM LOG-DATE-HEADING           00
043400         MOVE SPACES TO PRINT-REC                                       00
043500                        ADD 2 TO LINE-COUNT.                            00
043600 0821.   IF HOLD-FINISH = 0 MOVE "EOJ" TO RESULT                        00
043700             ADD CONSTANT-1 TO EOJ-TERMINATION                          00
043800     ELSE IF HOLD-FINISH = CONSTANT-1, MOVE "SYNTAX" TO RESULT          00
043900             ADD CONSTANT-1 TO SYNTAX-TERMINATION                       00
044000     ELSE IF HOLD-FINISH = CONSTANT-2, MOVE "DS-ED" TO RESULT           00
044100             ADD CONSTANT-1 TO DS-TERMINATION                           00
044200     ELSE MOVE "ABORT" TO RESULT                                        00
044300             ADD CONSTANT-1 TO ABORT-TERMINATION.                       00
044400         IF GP-CODE = 5 MOVE "PRNPBT" TO RESULT.                        00
044500         IF GP-CODE > 0 AND < 7                                         00
044600             ADD CONSTANT-1 TO RUN-TYPE (GP-CODE)    ELSE               00
044700             ADD CONSTANT-1 TO RUN-TYPE (4).                            00
044800         COMPUTE START-TIME-HOUR = HOLD-START / CONSTANT-216K.          00
044900         COMPUTE START-TIME-MIN = (HOLD-START - START-TIME-HOUR         00
045000     * CONSTANT-216K) / CONSTANT-3600.                                  00
045100         COMPUTE STOP-TIME-HOUR = HOLD-STOP / CONSTANT-216K.            00
045200         COMPUTE STOP-TIME-MIN = (HOLD-STOP - STOP-TIME-HOUR            00
045300     * CONSTANT-216K) / CONSTANT-3600.                                  00
045400         IF HOLD-START > HOLD-STOP ADD 24-HOUR-CONSTANT TO              00
045500                 HOLD-STOP.                                             00
045600         SUBTRACT HOLD-START FROM HOLD-STOP.                            00
045700         ADD HOLD-STOP TO ACCUM-ELAPSED.                                00
045800         ADD HOLD-PROCESS TO ACCUM-PROCESS.                             00
045900         ADD HOLD-I-O TO ACCUM-I-O.                                     00
046000         ADD HOLD-PRORATE TO ACCUM-PRORATE.                             00
046100         COMPUTE HOLD-START = HOLD-STOP / CONSTANT-3600.                00
046200         MOVE HOLD-START TO ELAPSED-MIN.                                00
046300         COMPUTE ELAPSED-SEC = (HOLD-STOP - HOLD-START                  00
046400     * CONSTANT-3600) / 60.                                             00
046500         COMPUTE HOLD-START = HOLD-PROCESS / CONSTANT-3600.             00
046600         MOVE HOLD-START TO PROCESS-MIN.                                00
046700         COMPUTE PROCESS-SEC = (HOLD-PROCESS - HOLD-START               00
046800     * CONSTANT-3600) / 60.                                             00
046900         COMPUTE HOLD-START = HOLD-I-O / CONSTANT-3600.                 00
047000         MOVE HOLD-START TO I-O-MIN.                                    00
047100         COMPUTE I-O-SEC = (HOLD-I-O - HOLD-START                       00
047200     * CONSTANT-3600) / 60.                                             00
047300         COMPUTE HOLD-START = HOLD-PRORATE / CONSTANT-3600.             00
047400         MOVE HOLD-START TO PRORATE-MIN.                                00
047500         COMPUTE PRORATE-SEC = (HOLD-PRORATE - HOLD-START               00
047600     * CONSTANT-3600) / 60.                                             00
047700         MOVE ":" TO DECIMAL-1 DECIMAL-2 DECIMAL-3 DECIMAL-4.           00
047800             MOVE CTL-CARD-CONT TO CTL-CARD.                            00
047900         ADD 1 TO LINE-COUNT.                                           00
048000             ADD 1 TO HOLD-RUN-SEQ.                                     00
048100         MOVE HOLD-RUN-SEQ TO PRINT-RUN-SEQ.                            00
048200         IF LINE-COUNT EXCEEDS 38 PERFORM 1012-OFLOW                    00
048300     ELSE WRITE PRINT-REC.                                              00
048400         PERFORM      READ-BYPASS-FILES HOLD-FILES TIMES.               00
048500 1003.   READ INDATA AT END GO TO 1101-END-LOG-LIST.                    00
048600         IF CTL-WORD-IN = CONSTANT-3,                                   00
048700             MOVE INREC TO SUB-REC (1)                                  00
048800             GO TO 0812.                                                00
048900         MOVE INREC TO SUB-REC (3).                                     00
049000         READ INDATA   AT END GO TO 1101-END-LOG-LIST.                  00
049100         MOVE INREC TO SUB-REC (4).                                     00
049200         GO TO 0821.                                                    00
049300 1012-OFLOW.                                                            00
049400         WRITE PRINT-REC BEFORE ADVANCING CHANNEL 1.                    00
049500         ADD 1 TO PAGE-NO.                                              00
049600         MOVE PAGE-NO TO HEADING-PAGE-NO.                               00
049700         WRITE PRINT-REC FROM PRINT-MAIN-HEADING BEFORE 2 LINES.        00
049800         MOVE "24-HOUR CLOCK    TIME IS IN MINUTES:SECONDS"             00
049900     TO PRINT-REC.                                                      00
050000         WRITE PRINT-REC.                                               00
050100         MOVE "START  STOP  ELAPSED  PROCESS     I/O  PRORATED          00
050200-    "      CONTENTS OF INITIATING CONTROL CARD"                        00
050300     TO PRINT-REC.                                                      00
050400         WRITE PRINT-REC.                                               00
050500         MOVE " TIME  TIME     TIME     TIME     TIME     TIME  RE      00
050600-    "SULT" TO PRINT-REC.                                               00
050700         WRITE PRINT-REC BEFORE ADVANCING 2 LINES.                      00
050800         MOVE ZEROS TO LINE-COUNT.                                      00
050900         MOVE SPACES TO PRINT-REC.                                      00
051000 READ-BYPASS-FILES.                                                     00
051100     READ INDATA INTO FILE-RECORDS AT END GO TO 1101-END-LOG-LIST.      00
051200         MOVE ZEROS TO CONVERT-CMP.                                     00
051300         MOVE UNIT-CODE TO HOLD-1-CHAR.                                 00
051400         MOVE CONVERT-CMP TO SUB.                                       00
051500         IF SUB ! 0,                                                    00
051600              ADD TIME-OPEN TO DEVICE-OPEN(SUB)                         00
051700              MOVE ERROR-COUNT TO HOLD-2-CHAR                           00
051800              ADD CONVERT-CMP TO DEVICE-ERRORS(SUB).                    00
051900 1101-END-LOG-LIST.                                                     00
052000         CLOSE INDATA.                                                  00
052100         PERFORM WITH CALL-LOG-ANAL.                                    00
052200         MOVE SPACES TO HISTORY-REC.                                    00
052300         MOVE SPACES TO PRINT-REC.                                      00
052400         WRITE PRINT-REC BEFORE ADVANCING 2 LINES.                      00
052500         MOVE "** TOTAL **" TO PRINT-REC.                               00
052600         COMPUTE HOLD-START = ACCUM-ELAPSED / CONSTANT-3600.            00
052700         MOVE HOLD-START TO ELAPSED-MIN.                                00
052800         COMPUTE ELAPSED-SEC = (ACCUM-ELAPSED - HOLD-START              00
052900     * CONSTANT-3600) / 60.                                             00
053000         COMPUTE HOLD-START = ACCUM-PROCESS / CONSTANT-3600.            00
053100         MOVE HOLD-START TO PROCESS-MIN.                                00
053200         COMPUTE PROCESS-SEC = (ACCUM-PROCESS - HOLD-START              00
053300     * CONSTANT-3600) / 60.                                             00
053400         COMPUTE HOLD-START = ACCUM-I-O / CONSTANT-3600.                00
053500         MOVE HOLD-START TO I-O-MIN.                                    00
053600         COMPUTE I-O-SEC = (ACCUM-I-O - HOLD-START                      00
053700     * CONSTANT-3600) / 60.                                             00
053800         COMPUTE HOLD-START = ACCUM-PRORATE / CONSTANT-3600.            00
053900         MOVE ":" TO DECIMAL-1, DECIMAL-2, DECIMAL-3 DECIMAL-4.         00
054000         PERFORM 1012-OFLOW.                                            00
054100         MOVE "   ERRORS BY DEVICE  MINUTES:SECS DEVICE OPEN"           00
054200     TO PRINT-REC.                                                      00
054300         WRITE PRINT-REC BEFORE ADVANCING 2 LINES.                      00
054400         MOVE SPACES TO PRINT-REC.                                      00
054500         PERFORM 1701-ERROR-PRINT VARYING SUB FROM 1 BY 1               00
054600     UNTIL SUB EXCEEDS 31.       CLOSE PRINT-FILE.                      00
054700         STOP RUN.                                                      00
054800 1701-ERROR-PRINT.                                                      00
054900              MOVE DEVICE-ERRORS(SUB) TO ELAPSED-MIN.                   00
055000         MOVE DEVICE-NAME(SUB) TO DEVICE-CODE.                          00
055100         COMPUTE HOLD-START = DEVICE-OPEN(SUB) / CONSTANT-3600.         00
055200         MOVE HOLD-START TO PROCESS-MIN.                                00
055300         COMPUTE PROCESS-SEC = (DEVICE-OPEN(SUB) - HOLD-START           00
055400         * CONSTANT-3600) / 60.                                         00
055500         MOVE ":" TO DECIMAL-2.                                         00
055600              WRITE PRINT-REC.                                          00
055700 END-OF-JOB.                                                            00
