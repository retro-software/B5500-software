000100 REMARKS  PROGRAM - KWIC2.                                                
000200REMARKS  CUBE LIBRARY NUMBER IS M100004.                                  
000300REMARKS   THIS VERSION DATED 6/20/67.                                     
000400 IDENTIFICATION DIVISION.                                          U998150
000500 PROGRAM-ID.  "U050070" KEY-WORD-IN-CONTEX.                        U998150
000600 DATE-COMPILED.                                                    U998150
000700 ENVIRONMENT DIVISION.                                             U998150
000800 CONFIGURATION SECTION.                                            U998150
000900 SOURCE-COMPUTER. B-5500.                                          U998150
001000 OBJECT-COMPUTER. B-5500, MEMORY SIZE 10000 WORDS.                 U998150
001100 INPUT-OUTPUT SECTION.                                             U998150
001200 FILE-CONTROL.                                                     U998150
001300     SELECT CARD-FILE ASSIGN TO CARD-READER.                       U998150
001400     SELECT PRINT-FILE ASSIGN TO PRINTER.                          U998150
001500     SELECT SORT-FILE  ASSIGN TO 3 SORT-TAPES.                     U998150
001600 DATA DIVISION.                                                    U998150
001700 FILE SECTION.                                                     U998150
001800 FD  CARD-FILE;                                                    U998150
001900     LABEL RECORD IS STANDARD                                      U998150
002000     VALUE OF ID IS "U050070"                                      U998150
002100     DATA RECORDS ARE CRD0 CTL-CRD.                                U998150
002200     01  CTL-CRD.                                                  U998150
002300         04  CTL-CHR   PC X.                                       U998150
002400         04  CRD-TYP   PC 9.                                       U998150
002500         04  TYP1.   ~ SZ 78                                       U998150
002600             08  ENTNO PC 999.                                     U998150
002700             08  BEGCOL PC 99.                                     U998150
002800             08  BEGPNT PC 999.                                    U998150
002900             08  NOCHAR PC 99.                                     U998150
003000             08  FILLER  SZ 68.                                    U998150
003100         04  TYP2 REDEFINES TYP1.                                  U998150
003200             08  FILLER SZ 10.                                     U998150
003300             08  NOPNT  PC 999.                                    U998150
003400             08  FILLER SZ 65.                                     U998150
003500         04  TYP3 REDEFINES TYP2.                                  U998150
003600             08 FILLER SZ 03.                                      U998150
003700             08  BYPASS PC X(10).                                  U998150
003800             08  FILLER SZ 65.                                     U998150
003900     01  CRD0 SZ 80.                                               U998150
004000         02  CRD1  PC X OC 80.                                     U998150
004100 FD  PRINT-FILE;                                                   U998150
004200     LABEL RECORD IS STANDARD                                      U998150
004300     VALUE OF ID IS "KWICLST"                                      U998150
004400     DATA RECORDS ARE PRT1.                                        U998150
004500     01  PRT1.                                                     U998150
004600         02  PRT2 PC X(120).                                       U998150
004700 SD  SORT-FILE;                                                    U998150
004800     DATA RECORD IS KWIC1.                                         U998150
004900 01  KWIC1.                                                        U998150
005000     04  KWIC2 PC X(120).                                          U998150
005100     04  KWIC3 SZ 10.                                              U998150
005200     04  FILLER  SZ 6 .                                            U998150
005300 WORKING-STORAGE SECTION.                                          U998150
005400     77  X        PC 99   CMP-1.                                   U998150
005500     77  Y        PC 99   CMP-1.                                   U998150
005600     77  Z        PC 999  CMP-1.                                   U998150
005700     77  A        PC 99   CMP-1.                                   U998150
005800     77  B        PC 999  CMP-1.                                   U998150
005900     77  C        PC 999  CMP-1.                                   U998150
006000     77  SCAN     PC 99   CMP-1.                                   U998150
006100     77  CONX     PC 999  CMP-1.                                   U998150
006200     77  SAVE1          PC  999  CMP-1.                            U998150
006300     77  PRT3     PC X.                                            U998150
006400     77  BYPASS-REVERSE  PC 9 VA ZERO.                             U998150
006500     01  PNT.                                                      U998150
006600         02  PNT3.                                                 U998150
006700             04  PNT1  PC X OC 120.                                U998150
006800         02  PNT2  SZ  10.                                         U998150
006900             04  PNT4  PC X.                                       U998150
007000             04  PNT5  PC X(9).                                    U998150
007100         02  FILLER  SZ 6.                                         U998150
007200     01  FOR1.                                                     U998150
007300         04  FOR-DUM1  OC 15.                                      U998150
007400             08  BEG-COL  PC 99.                                   U998150
007500             08  BEG-PNT  PC 999.                                  U998150
007600             08  NO-CHAR  PC 99.                                   U998150
007700     01  FOR2.                                                     U998150
007800         04  BEG-COL2     PC 99.                                   U998150
007900         04  BEG-PNT2     PC 999.                                  U998150
008000         04  NO-CHAR2     PC 99.                                   U998150
008100         04  NO-PNT2      PC 999.                                  U998150
008200         04  LST-COL2   PC  99.                                    U998150
008300         04  LST-PNT2   PC  999.                                   U998150
008400     01  WRD1.                                                     U998150
008500         04  WRD2 PC X OC 10.                                      U998150
008600     01  CON.                                                      U998150
008700         04  CON1 PC X(10) OC 100 .                                U998150
008800 PROCEDURE  DIVISION.                                              U998150
008900 DUMMY SECTION.                                                    U998150
009000 AA. GO TO SRTG.                                                   U998150
009100 GO1 SECTION.                                                      U998150
009200 PAR1.                                                             U998150
009300     OPEN  INPUT CARD-FILE.                                        U998150
009400     MOVE ZEROS TO FOR1 FOR2.                                      U998150
009500 PAR2.                                                             U998150
009600     READ  CARD-FILE AT END GO TO CARD-END.                        U998150
009700     IF CTL-CHR ! "$"                                              U998150
009800     GO TO  PAR4.                                                  U998150
009900     IF CRD-TYP = 1                                                U998150
010000     MOVE ENTNO TO A                                               U998150
010100     MOVE BEGCOL TO BEG-COL(A)                                     U998150
010200     MOVE BEGPNT TO BEG-PNT(A)                                     U998150
010300     MOVE NOCHAR TO NO-CHAR(A)                                     U998150
010400     GO TO PAR2.                                                   U998150
010500     IF CRD-TYP = 2                                                U998150
010600     MOVE BEGCOL TO BEG-COL2                                       U998150
010700     MOVE BEGPNT TO BEG-PNT2                                       U998150
010800     MOVE NOCHAR TO NO-CHAR2                                       U998150
010900     MOVE NOPNT  TO NO-PNT2                                        U998150
011000     GO TO PAR2.                                                   U998150
011100     IF CRD-TYP = 3                                                U998150
011200     MOVE ENTNO TO A                                               U998150
011300     MOVE BYPASS TO CON1(A)                                        U998150
011400     GO TO PAR2.                                                   U998150
011500     IF CRD-TYP = 9                                                U998150
011600     MOVE CRD-TYP TO BYPASS-REVERSE                                U998150
011700     GO TO PAR2.                                                   U998150
011800     DISPLAY "ILLEGAL CTL CRD".                                    U998150
011900     GO TO PAR2.                                                   U998150
012000 PAR4.                                                             U998150
012100     IF BEG-COL2 = ZERO OR                                         U998150
012200     BEG-PNT2 = ZERO OR                                            U998150
012300     NO-CHAR2 = ZERO OR                                            U998150
012400     NO-PNT2  = ZERO                                               U998150
012500     DISPLAY "NO TYPE 2 CONTROL CARD"                              U998150
012600     CLOSE  CARD-FILE WITH RELEASE                                 U998150
012700     STOP RUN.                                                     U998150
012800     COMPUTE LST-COL2 = BEG-COL2 + NO-CHAR2 - 1.                   U998150
012900     MOVE SPACES TO PNT.                                           U998150
013000     COMPUTE LST-PNT2 = BEG-PNT2 + NO-PNT2 - 1.                    U998150
013100     IF LST-PNT2 > 120  DISPLAY "PRINTER EXCEEDED" STOP RUN.       U998150
013200     GO TO KWCD.                                                   U998150
013300 KWCA.                                                             U998150
013400     READ CARD-FILE  AT END GO TO CARD-END.                        U998150
013500     MOVE  SPACES TO PNT.                                          U998150
013600     GO TO KWCD.                                                   U998150
013700 KWCB.                                                             U998150
013800     COMPUTE Y = A + C.                                            U998150
013900     COMPUTE Z = B + C.                                            U998150
014000     MOVE CRD1(Y) TO PNT1(Z).                                      U998150
014100 KWCC.                                                             U998150
014200     MOVE BEG-COL(X) TO A.                                         U998150
014300     MOVE BEG-PNT(X) TO B                                          U998150
014400     IF NO-CHAR(X) ! ZERO                                          U998150
014500     PERFORM KWCB  VARYING C FROM 0 BY 1 UNTIL  C } NO-CHAR(X).    U998150
014600 KWCD.                                                             U998150
014700     PERFORM KWCC  VARYING X FROM 1 BY 1 UNTIL  X > 15.            U998150
014800     ~ THIS SETION MOVES THOSE PORTIONS OF THE INPUT               U998150
014900     ~ WHICH ARE FIXED FIELDS TO THEIR PRINT AREAS                 U998150
015000     MOVE BEG-COL2 TO A. COMPUTE Y = A + NO-CHAR2.                 U998150
015100     COMPUTE  B = BEG-PNT2 +(NO-PNT2 / 3).                         U998150
015200     GO TO KWCO.                                                   U998150
015300 KWCE.                                                             U998150
015400     COMPUTE Y = A + C .                                           U998150
015500     IF CRD1(Y) ! SPACE AND SCAN = ZERO                            U998150
015600     AND Y { LST-COL2                                              U998150
015700     MOVE CRD1(Y) TO WRD2(C+1)                                     U998150
015800     ELSE IF SCAN = ZERO                                           U998150
015900     COMPUTE SCAN = C + 1.                                                
016000 KWCF.                                                             U998150
016100     MOVE SPACES TO WRD1.                                          U998150
016200     PERFORM KWCE VARYING  C FROM 0 BY 1 UNTIL C }                 U998150
016300     10.                                                           U998150
016400     IF SCAN ! 0                                                   U998150
016500     GO TO KWCI.                                                   U998150
016600 KWCG.                                                             U998150
016700     COMPUTE  Y = A + C.                                           U998150
016800     IF CRD1(Y) ! SPACE                                            U998150
016900     AND Y ! LST-COL2                                              U998150
017000     ADD 1 TO C                                                    U998150
017100     GO TO KWCG.                                                   U998150
017200     COMPUTE SCAN = C + 1.                                         U998150
017300     GO TO KWCI.                                                   U998150
017400 KWCH.                                                             U998150
017500     IF WRD1 = CON1(C)                                             U998150
017600     MOVE C TO CONX.                                               U998150
017700 KWCI.                                                             U998150
017800     MOVE ZERO TO CONX.                                            U998150
017900     PERFORM KWCH VARYING C FROM 1 BY 1 UNTIL C > 100.             U998150
018000     IF BYPASS-REVERSE ! 0                                         U998150
018100     IF CONX = 0                                                   U998150
018200     GO TO KWCN ELSE GO TO KWCK ELSE                               U998150
018300     IF CONX ! ZERO                                                U998150
018400     GO TO KWCN ELSE GO TO KWCK.                                   U998150
018500 KWCJ.                                                             U998150
018600     COMPUTE Y = BEG-PNT2 + C - 1.                                 U998150
018700     MOVE SPACE TO PNT1(Y).                                        U998150
018800 KWCK.                                                             U998150
018900     PERFORM KWCJ VARYING C FROM 1 BY 1 UNTIL C                    U998150
019000     > NO-PNT2.                                                    U998150
019100     GO TO KWCM.                                                   U998150
019200 KWCL.                                                             U998150
019300     COMPUTE Y = BEG-PNT2 + C - 1.                                 U998150
019400     COMPUTE Z = BEG-COL2 + C - 1.                                 U998150
019500     IF Y > LST-PNT2                                               U998150
019600     COMPUTE Y = SAVE1 + ( Y - LST-PNT2 ).                         U998150
019700     IF Y > LST-PNT2                                               U998150
019800     STOP RUN.                                                     U998150
019900     MOVE CRD1(Z) TO PNT1(Y).                                      U998150
020000 KWCM.                                                             U998150
020100     COMPUTE Z = A - BEG-COL2.                                     U998150
020200     COMPUTE Y = B - Z.                                            U998150
020300     MOVE BEG-PNT2 TO SAVE1.                                       U998150
020400     IF Y < BEG-PNT2                                               U998150
020500     COMPUTE Z = BEG-PNT2 - Y                                      U998150
020600     COMPUTE BEG-PNT2 = LST-PNT2 - Z                               U998150
020700     ELSE                                                          U998150
020800     MOVE  Y TO BEG-PNT2.                                          U998150
020900     PERFORM KWCL VARYING C FROM 1 BY 1 UNTIL C >                  U998150
021000     NO-CHAR2. MOVE WRD1 TO PNT2.                                  U998150
021100     MOVE  SAVE1 TO BEG-PNT2.                                      U998150
021200     MOVE PNT TO KWIC1.                                            U998150
021300     RELEASE KWIC1.                                                U998150
021400 KWCN.                                                             U998150
021500     MOVE ZERO TO CONX.                                            U998150
021600     COMPUTE A = A + SCAN.                                         U998150
021700     MOVE ZERO TO SCAN.                                            U998150
021800 KWCO.                                                             U998150
021900     IF CRD1(A) ! SPACE                                            U998150
022000     OR A } LST-COL2                                               U998150
022100     NEXT SENTENCE ELSE                                            U998150
022200     ADD 1 TO A                                                    U998150
022300     GO TO KWCO.                                                   U998150
022400     IF A < LST-COL2                                               U998150
022500     GO TO KWCF.                                                   U998150
022600     GO TO KWCA.                                                   U998150
022700 CARD-END.                                                         U998150
022800     CLOSE CARD-FILE WITH RELEASE.                                 U998150
022900 GO2 SECTION.                                                      U998150
023000 GO2A.                                                             U998150
023100     OPEN OUTPUT PRINT-FILE.  MOVE SPACE TO PRT3.                  U998150
023200 GO2B.                                                             U998150
023300     RETURN SORT-FILE RECORD AT END GO TO GO2D.                    U998150
023400     MOVE KWIC1 TO PNT.                                            U998150
023500     MOVE PNT3 TO PRT2.                                            U998150
023600     IF PRT3 ! PNT4                                                U998150
023700     GO TO GO2C.                                                   U998150
023800     WRITE PRT1.                                                   U998150
023900     GO TO GO2B.                                                   U998150
024000 GO2C.                                                             U998150
024100     MOVE SPACES TO PRT2.                                          U998150
024200     WRITE PRT1 BEFORE ADVANCING 2 LINES.                          U998150
024300     MOVE PNT3  TO PRT2.                                           U998150
024400     WRITE PRT1.                                                   U998150
024500     MOVE PNT4  TO PRT3.                                           U998150
024600     GO TO GO2B.                                                   U998150
024700 GO2D.                                                             U998150
024800     CLOSE PRINT-FILE WITH RELEASE.                                U998150
024900 SRTG SECTION.                                                     U998150
025000 S-1.                                                              U998150
025100     SORT SORT-FILE ON ASCENDING KEY KWIC3                         U998150
025200     INPUT PROCEDURE IS GO1                                        U998150
025300     OUTPUT PROCEDURE IS GO2.                                      U998150
025400 S-2.                                                              U998150
025500     STOP RUN.                                                     U998150
025600 END-OF-JOB.                                                       U998150
