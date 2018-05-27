000100 IDENTIFICATION DIVISION.                                         DETAB-65
000200 PROGRAM-ID. PREPROCESSOR FOR DETAB-65.                           DETAB-65
000300 AUTHOR. ANSON CHAPMAN.                                           DETAB-65
000400 DATE-WRITTEN. 12/30/64.                                          DETAB-65
000500 DATE-COMPILED.                                                   DETAB-65
000600 REMARKS.                                                                 
000700 THE CUBE LIBRARY NUMBER IS L400001,  THE PROGRAM NAME IS                 
000800 "DETAB65",  THIS VERSION DATED 6/10/68.                                  
000900 REMARKS.                                                         DETAB-65
001000     THE GENERATOR PORTION OF THE PREPROCESSOR ANALIZES A         DETAB-65
001100     DECISION TABLE AND GENERATES SIMPLE CONDITIONAL STATEMENTS   DETAB-65
001200     FOR Y"S, N"S AND BLANKS AND WILL GENERATE IF STATEMENTS FOR  DETAB-65
001300     ONE PATH THRU THE TREE THE ACTION CORRESPONDING TO THE PATH  DETAB-65
001400     IS GENERATED IN STMTS DX014 THRU DX032 THIS PATH IS DELETED  DETAB-65
001500     FROM THE TREE IN DX016 THRU DX020  DX301 THRU DX061          DETAB-65
001600     REINITIALIZES THE TREE, FINDS THE LAST NODE CONNECTED TO     DETAB-65
001700     THIS PATH AND COMES BACK TO DX003 FOR ANOTHER PASS THRU THE  DETAB-65
001800     NEXT PATH  THIS PROCESS IS REPEATED UNTIL IF STATEMENTS      DETAB-65
001900     HAVE BEEN GENERATED FOR ALL PATHS THRU THE DECISION TABLE    DETAB-65
002000     TREE STRUCTURE.                                              DETAB-65
002100 ENVIRONMENT DIVISION.                                            DETAB-65
002200 CONFIGURATION SECTION.                                           DETAB-65
002300 SOURCE-COMPUTER. B-5500.                                                 
002400 OBJECT-COMPUTER. B-5500.                                                 
002500                                                                          
002600                                                                          
002700 INPUT-OUTPUT SECTION.                                            DETAB-65
002800 FILE-CONTROL.                                                    DETAB-65
002900     SELECT CARD-INPUT, ASSIGN TO READER.                                 
003000     SELECT CARD-OUTPUT, ASSIGN TO PUNCH.                                 
003100     SELECT LIST-OUTPUT, ASSIGN TO PRINTER.                               
003200 DATA DIVISION.                                                   DETAB-65
003300 FILE SECTION.                                                    DETAB-65
003400 FD  CARD-INPUT                                                   DETAB-65
003500     LABEL RECORD STANDARD, VA ID "KARDS",                                
003600     DATA RECORDS ARE TEST-CARD.                                  DETAB-65
003700 01  TEST-CARD.                                                   DETAB-65
003800     02 FILLER   PICTURE X(80).                                   DETAB-65
003900 FD  CARD-OUTPUT                                                  DETAB-65
004000     LABEL RECORD STANDARD, VA ID "PNCHCRD",                              
004100     DATA RECORDS ARE CRD-OUT, DETAB-CRD, DUM-1.                  DETAB-65
004200 01 CRD-OUT.                                                      DETAB-65
004300     02 FILLER   PICTURE X(7).                                    DETAB-65
004400     02 BODY.                                                     DETAB-65
004500       03 FILLER PICTURE X(4).                                    DETAB-65
004600       03 B-MARG PICTURE X(61).                                   DETAB-65
004700     02 IDFLD    PICTURE X(8).                                    DETAB-65
004800 01  DETAB-CRD.                                                   DETAB-65
004900     02 FILLER   PICTURE XXX.                                     DETAB-65
005000     02 IDENT.                                                    DETAB-65
005100       03 ROW-NO PICTURE 999.                                     DETAB-65
005200       03 LINE-ID PICTURE X.                                      DETAB-65
005300     02 FILLER   PICTURE X(73).                                   DETAB-65
005400 01  DUM-1.                                                       DETAB-65
005500     02 CRD-COL  PICTURE X       OCCURS 80 TIMES.                 DETAB-65
005600 FD  LIST-OUTPUT                                                  DETAB-65
005700     LABEL RECORD STANDARD, VA ID "LINE",                                 
005800     DATA RECORD IS TAPE-LIST.                                    DETAB-65
005900 01  TAPE-LIST.                                                   DETAB-65
006000     02 FILLER   PICTURE X(11).                                   DETAB-65
006100     02 CARDX    PICTURE 999.                                     DETAB-65
006200     02 FILLER   PICTURE X(66).                                   DETAB-65
006300 WORKING-STORAGE SECTION.                                         DETAB-65
006400 77  AZ          PICTURE XX       VALUE "AZ".                     DETAB-65
006500 77  CARDCNT     PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
006600 77  COLIX       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
006700 77  COLUM       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
006800 77  DUMIX       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
006900 77  ELMCT       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007000 77  ELMCX       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007100 77  ELMRX       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007200 77  EXIX        PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007300 77  KEY-1       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007400 77  KEY-2       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007500 77  KEY-3       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007600 77  LABIX       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007700 77  LABNO       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007800 77  NACTS       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
007900 77  NCOLS       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
008000 77  NORLS       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
008100 77  NOCON       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
008200 77  NRLS        PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
008300 77  NROWS       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
008400 77  ROWIX       PICTURE 999     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
008500 01  DUM-2.                                                       DETAB-65
008600     02 FILLER         OCCURS 50 TIMES.                           DETAB-65
008700       03 STRTCOL PICTURE 99     COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
008800       03 NMCOLS PICTURE 99      COMPUTATIONAL SYNCHRONIZED RIGHT.DETAB-65
008900 01  DUM-3.                                                       DETAB-65
009000     02 COLS     PICTURE X     OCCURS 12 TIMES.                   DETAB-65
009100 01  DUM-4.                                                       DETAB-65
009200     02 EGOTO    PICTURE X     OCCURS 5 TIMES.                    DETAB-65
009300 01  DUM-5.                                                       DETAB-65
009400     02 TEMP     PICTURE X     OCCURS 58 TIMES.                   DETAB-65
009500 01  DUM-10  PICTURE X(8)  VALUE "SECTION.".                      DETAB-65
009600 01 DUM-12  REDEFINES DUM-10.                                     DETAB-65
009700     02 NMSEC    PICTURE X     OCCURS 8 TIMES.                    DETAB-65
009800 01  HEADER.                                                      DETAB-65
009900     02 FILLER   PICTURE X(8).                                    DETAB-65
010000     02 TBLNME   PICTURE X(30).                                   DETAB-65
010100     02 FORMID   PICTURE XX.                                      DETAB-65
010200     02 NCOND    PICTURE 9(3).                                    DETAB-65
010300     02 ACTNS    PICTURE 9(3).                                    DETAB-65
010400     02 NORULS   PICTURE 9(3).                                    DETAB-65
010500     02 FILLER   PICTURE X(51).                                   DETAB-65
010600 01  DPRINT.                                                      DETAB-65
010700     02 DLABEL.                                                   DETAB-65
010800       03 FILLER PICTURE X(7)     VALUE SPACES.                   DETAB-65
010900       03 DUM-6.                                                  DETAB-65
011000         04 LABNM PICTURE XX.                                     DETAB-65
011100         04 LABVL PICTURE 9(3).                                   DETAB-65
011200       03 FILLER PICTURE X        VALUE ".".                      DETAB-65
011300     02 DGOTO.                                                    DETAB-65
011400       03 FILLER PICTURE A(7)     VALUE " GO TO ".                DETAB-65
011500       03 DGOLN.                                                  DETAB-65
011600         04 DGOLB PICTURE XX.                                     DETAB-65
011700         04 DGONO PICTURE 999.                                    DETAB-65
011800     02 HOUSTON.                                                  DETAB-65
011900       03 CNDI   PICTURE X(58) OCCURS 50 TIMES.                   DETAB-65
012000       03 ATBL   PICTURE X(58) OCCURS 50 TIMES.                   DETAB-65
012100 01  LINE1.                                                       DETAB-65
012200     02 FILLER   PICTURE X(14)    VALUE "           IF ".         DETAB-65
012300     02 COND     PICTURE X(58).                                   DETAB-65
012400 01  TEXAS.                                                       DETAB-65
012500     02 LINE2.                                                    DETAB-65
012600       03 FILLER PICTURE A(11).                                   DETAB-65
012700       03 CDOPR  PICTURE X(12).                                   DETAB-65
012800       03 PIF    PICTURE X.                                       DETAB-65
012900       03 DELSE  PICTURE X(6).                                    DETAB-65
013000       03 ELOPR  PICTURE X(12).                                   DETAB-65
013100       03 PELSE  PICTURE X.                                       DETAB-65
013200       03 FILLER PICTURE A(29).                                   DETAB-65
013300     02 LINE3  REDEFINES  LINE2.                                  DETAB-65
013400       03 FILLER PICTURE X(7).                                    DETAB-65
013500       03 DNAME.                                                  DETAB-65
013600         04 TCOLS PICTURE X    OCCURS 58 TIMES.                   DETAB-65
013700       03 FILLER PICTURE X(7).                                    DETAB-65
013800     02  FILLERZ REDEFINES LINE3.                                         
013900       03 FILLER PICTURE X(11).                                   DETAB-65
014000       03 BNAME  PICTURE X(58).                                   DETAB-65
014100       03 FILLER PICTURE XXX.                                     DETAB-65
014200     02 DECISION-TABLE.                                           DETAB-65
014300       03 ROW                  OCCURS 50 TIMES.                   DETAB-65
014400         04 COLMN PICTURE X    OCCURS 100 TIMES.                  DETAB-65
014500 01  ELIMT.                                                       DETAB-65
014600     02 ELIMC    PICTURE 999     OCCURS 25 TIMES.                 DETAB-65
014700 01  MATIT.                                                       DETAB-65
014800     02 MATIX    PICTURE 999     OCCURS 25 TIMES.                 DETAB-65
014900 01  MICDESCR.                                                    DETAB-65
015000     02 PDPUL    PICTURE 999   OCCURS 128 TIMES.                  DETAB-65
015100     02 SAVCL    PICTURE X       OCCURS 25 TIMES.                 DETAB-65
015200 01  WRNING-PRINT.                                                DETAB-65
015300     02 FILLER   PICTURE X(17)    VALUE                           DETAB-65
015400        " ****** WARNING. ".                                      DETAB-65
015500     02 WRNING-IMAGE PICTURE X(52).                               DETAB-65
015600 01  WARNING-MESSAGES.                                            DETAB-65
015700     02 WRNING-1 PICTURE X(52)    VALUE                           DETAB-65
015800        "NO ELSE RULE CARD. LAST RULE PROCESSED AS ELSE RULE.".   DETAB-65
015900     02 WRNING-2 PICTURE X(31)    VALUE                           DETAB-65
016000        "REDUNDANCY. CHECK THESE RULES -".                        DETAB-65
016100 01  ERR-PRNT.                                                    DETAB-65
016200     02 FILLER   PICTURE X(30)    VALUE                           DETAB-65
016300        " ****** ERROR. TABLE SKIPPED. ".                         DETAB-65
016400     02 ERR-IMAGE PICTURE X(53).                                  DETAB-65
016500 01  ERROR-MESSAGES.                                              DETAB-65
016600     02 ERR-1    PICTURE X(48)    VALUE                           DETAB-65
016700        "PRESENTLY, TABLES RESTRICTED TO LIMITED ENTRIES.".       DETAB-65
016800     02 ERR-2    PICTURE X(42)    VALUE                           DETAB-65
016900        "TABLE-NAME MISSING FROM TABLE HEADER CARD.".             DETAB-65
017000     02 ERR-3    PICTURE X(19)    VALUE                           DETAB-65
017100        "RULES CARD MISSING.".                                    DETAB-65
017200     02 ERR-4    PICTURE X(39)    VALUE                           DETAB-65
017300        "LESS THAN THREE RULE COLUMNS SPECIFIED.".                DETAB-65
017400     02 ERR-5    PICTURE X(43)    VALUE                           DETAB-65
017500        "PRESENTLY, CONTINUED RULES NOT IMPLEMENTED.".            DETAB-65
017600     02 ERR-6    PICTURE X(40)    VALUE                           DETAB-65
017700        "CONDITION STUB ENTRY EXCEEDS 58 COLUMNS.".               DETAB-65
017800     02 ERR-7    PICTURE X(26)    VALUE                           DETAB-65
017900        "MORE THAN 12 RULE COLUMNS.".                             DETAB-65
018000     02 ERR-8    PICTURE X(53)    VALUE                           DETAB-65
018100        "NUMBER OF RULES ENCOUNTERED DISAGREES WITH RULE CARD.".  DETAB-65
018200     02 ERR-9    PICTURE X(41)    VALUE                           DETAB-65
018300        "MORE THAN 50 ACTION OR CONDITION ENTRIES.".              DETAB-65
018400     02 ERR-10   PICTURE X(46)    VALUE                           DETAB-65
018500        "DECISION TABLE LOGIC ERROR. PROCESSING HALTED.".         DETAB-65
018600 PROCEDURE DIVISION.                                              DETAB-65
018700 DETAB65.                                                         DETAB-65
018800     OPEN INPUT CARD-INPUT, OUTPUT CARD-OUTPUT, LIST-OUTPUT.      DETAB-65
018900 DT001.                                                           DETAB-65
019000     PERFORM READ-1.                                              DETAB-65
019100     IF "0000" = IDENT OF DETAB-CRD GO TO MONITER.                        
019200     WRITE DETAB-CRD.                                             DETAB-65
019300     GO TO DT001.                                                 DETAB-65
019400 MONITER.                                                                 
019500     MOVE DETAB-CRD TO HEADER.                                    DETAB-65
019600     IF TBLNME = SPACES GO TO EM02.                               DETAB-65
019700     IF FORMID OF HEADER NOT = "L" GO TO EM01.                    DETAB-65
019800     MOVE SPACES TO HOUSTON, TEXAS.                               DETAB-65
019900     MOVE ZEROES TO DUM-2.                                        DETAB-65
020000     MOVE TBLNME TO DUM-5, DNAME.                                 DETAB-65
020100     PERFORM RSCAN.                                               DETAB-65
020200     PERFORM DT005 VARYING EXIX FROM 1 BY 1 UNTIL EXIX = 9.       DETAB-65
020300     PERFORM READ-1.                                              DETAB-65
020400     IF IDENT OF DETAB-CRD NOT = "0001" GO TO EM03.               DETAB-65
020500                                                                  DETAB-65
020600     NOTE RULES CONVERSION SECTION.                               DETAB-65
020700                                                                  DETAB-65
020800     MOVE 0 TO CARDCNT.                                           DETAB-65
020900     MOVE 1 TO NRLS.                                              DETAB-65
021000     MOVE 9 TO COLUM, STRTCOL (NRLS).                             DETAB-65
021100 DT050.                                                           DETAB-65
021200     IF CRD-COL (COLUM) = SPACE GO TO DT053.                      DETAB-65
021300     IF CARDCNT IS LESS THAN 3 GO TO EM04.                        DETAB-65
021400     MOVE CARDCNT TO NMCOLS (NRLS).                               DETAB-65
021500     IF CRD-COL (COLUM) = "$" GO TO DT055.                        DETAB-65
021600     ADD 1 TO NRLS.                                               DETAB-65
021700     MOVE COLUM TO STRTCOL (NRLS).                                DETAB-65
021800     MOVE 3 TO CARDCNT.                                           DETAB-65
021900     ADD 3 TO COLUM.                                              DETAB-65
022000     IF COLUM IS GREATER THAN 80 GO TO EM05.                      DETAB-65
022100     GO TO DT050.                                                 DETAB-65
022200 DT005.                                                           DETAB-65
022300     MOVE NMSEC (EXIX) TO TCOLS (DUMIX).                          DETAB-65
022400     ADD 1 TO DUMIX.                                              DETAB-65
022500 DT053.                                                           DETAB-65
022600     ADD 1 TO CARDCNT, ADD 1 TO COLUM.                            DETAB-65
022700     IF CARDCNT IS NOT GREATER THAN 12 GO TO DT050.               DETAB-65
022800     IF CARDCNT IS GREATER THAN 58 GO TO EM06.                    DETAB-65
022900     IF NRLS = 1 GO TO DT050 ELSE GO TO EM07.                     DETAB-65
023000 DT055.                                                           DETAB-65
023100     SUBTRACT 1 FROM NMCOLS (NRLS), SUBTRACT 1 FROM NRLS.         DETAB-65
023200     IF NRLS NOT = NORULS GO TO EM08.                             DETAB-65
023300                                                                  DETAB-65
023400     NOTE DETAB CARD SECTION.                                     DETAB-65
023500                                                                  DETAB-65
023600     ADD 1 TO NRLS.                                               DETAB-65
023700     MOVE STRTCOL (NRLS) TO COLUM.                                DETAB-65
023800     IF CRD-COL (COLUM) = "E" GO TO DT056.                        DETAB-65
023900     MOVE WRNING-1 TO WRNING-IMAGE.                               DETAB-65
024000     WRITE TAPE-LIST FROM WRNING-PRINT.                           DETAB-65
024100 DT056.                                                           DETAB-65
024200     MOVE 1 TO KEY-2, KEY-3, ROWIX.                               DETAB-65
024300 DT057.                                                           DETAB-65
024400     PERFORM READ-1.                                              DETAB-65
024500     IF ROW-NO OF DETAB-CRD =  999  GO TO DT057.                  DETAB-65
024600     MOVE 1 TO KEY-1, COLIX.                                      DETAB-65
024700     IF LINE-ID OF DETAB-CRD = "$" GO TO TBLPROC.                 DETAB-65
024800     MOVE STRTCOL (KEY-1) TO COLUM.                               DETAB-65
024900                                                                  DETAB-65
025000     NOTE CONDACT SECTION.                                        DETAB-65
025100                                                                  DETAB-65
025200     MOVE SPACES TO DUM-5.                                        DETAB-65
025300     MOVE 1 TO EXIX.                                              DETAB-65
025400 CONDACT.                                                         DETAB-65
025500     MOVE CRD-COL (COLUM) TO TEMP (EXIX).                         DETAB-65
025600     IF EXIX GREATER NMCOLS (KEY-1) GO TO DT057-1.                DETAB-65
025700     ADD 1 TO EXIX, ADD 1 TO COLUM, GO TO CONDACT.                DETAB-65
025800 DT057-1.                                                         DETAB-65
025900     IF KEY-2 IS GREATER THAN 50 GO TO EM09.                      DETAB-65
026000     IF KEY-2 IS GREATER THAN NCOND GO TO DT058.                  DETAB-65
026100     MOVE DUM-5 TO CNDI (KEY-2).                                  DETAB-65
026200     ADD 1 TO KEY-2.                                              DETAB-65
026300     GO TO DT059.                                                 DETAB-65
026400 DT058.                                                           DETAB-65
026500     IF KEY-3 IS GREATER THAN 50 GO TO EM09.                      DETAB-65
026600     MOVE DUM-5 TO ATBL (KEY-3).                                  DETAB-65
026700     ADD 1 TO KEY-3.                                              DETAB-65
026800 DT059.                                                           DETAB-65
026900     PERFORM DT060 THRU DT061 VARYING KEY-1 FROM 2 BY 1 UNTIL     DETAB-65
027000       KEY-1 IS GREATER THAN NRLS.                                DETAB-65
027100     ADD 1 TO ROWIX.                                              DETAB-65
027200     GO TO DT057.                                                 DETAB-65
027300 DT060.                                                           DETAB-65
027400     MOVE STRTCOL (KEY-1) TO COLUM.                               DETAB-65
027500                                                                  DETAB-65
027600     NOTE VARAMOVE SECTION.                                       DETAB-65
027700                                                                  DETAB-65
027800     MOVE SPACES TO DUM-3.                                        DETAB-65
027900     MOVE 1 TO EXIX.                                              DETAB-65
028000 VARAMVE.                                                         DETAB-65
028100     MOVE CRD-COL (COLUM) TO COLS (EXIX).                         DETAB-65
028200     IF EXIX GREATER NMCOLS (KEY-1) GO TO DT060-1.                DETAB-65
028300     ADD 1 TO EXIX, ADD 1 TO COLUM, GO TO VARAMVE.                DETAB-65
028400 DT060-1.                                                         DETAB-65
028500     EXAMINE DUM-3 REPLACING ALL "-" BY SPACES.                   DETAB-65
028600     IF DUM-3 = SPACES GO TO DT061.                               DETAB-65
028700     EXAMINE DUM-3 TALLYING UNTIL FIRST "N".                      DETAB-65
028800     IF TALLY = 12 MOVE "Y" TO COLMN (ROWIX, COLIX) ELSE          DETAB-65
028900       MOVE "N" TO COLMN (ROWIX, COLIX).                          DETAB-65
029000 DT061.                                                           DETAB-65
029100     ADD 1 TO COLIX.                                              DETAB-65
029200 TBLPROC.                                                         DETAB-65
029300     PERFORM L2OUT THRU RITAB.                                    DETAB-65
029400     MOVE "DX000" TO DUM-6.                                       DETAB-65
029500     PERFORM DLOUT THRU RITAB.                                    DETAB-65
029600                                                                  DETAB-65
029700     NOTE DECISION SECTION.                                       DETAB-65
029800                                                                  DETAB-65
029900     MOVE ZERO TO LABIX, LABNO.                                   DETAB-65
030000     MOVE ACTNS TO NACTS.                                         DETAB-65
030100     COMPUTE NORLS = NORULS - 1.                                  DETAB-65
030200     MOVE NCOND TO NOCON.                                         DETAB-65
030300     PERFORM DX042 VARYING COLIX FROM 1 BY 1 UNTIL COLIX = NORLS. DETAB-65
030400 DX042.                                                           DETAB-65
030500     MOVE COLIX TO MATIX (COLIX).                                 DETAB-65
030600 DX001.                                                           DETAB-65
030700     PERFORM DX002 VARYING COLIX FROM 1 BY 1 UNTIL COLIX = NORLS. DETAB-65
030800 DX002.                                                           DETAB-65
030900     MOVE COLIX TO ELIMC (COLIX).                                 DETAB-65
031000 DX050.                                                           DETAB-65
031100     MOVE NOCON TO NROWS.                                         DETAB-65
031200     MOVE NORLS TO NCOLS.                                         DETAB-65
031300     MOVE 0 TO ROWIX.                                             DETAB-65
031400     GO TO DX004.                                                 DETAB-65
031500 DX003.                                                           DETAB-65
031600     PERFORM L1OUT THRU RITAB.                                    DETAB-65
031700     PERFORM L2OUT THRU RITAB.                                    DETAB-65
031800 DX004.                                                           DETAB-65
031900     MOVE SPACES TO LINE2.                                        DETAB-65
032000 DX005.                                                           DETAB-65
032100     ADD 1 TO ROWIX.                                              DETAB-65
032200     MOVE ZERO TO DUMIX.                                          DETAB-65
032300     IF ROWIX = NOCON GO TO DX014.                                DETAB-65
032400     MOVE 1 TO COLIX.                                             DETAB-65
032500                                                                  DETAB-65
032600     NOTE  ARE THERE ALL BLANKS IN THIS ROW.                      DETAB-65
032700                                                                  DETAB-65
032800 DX005-1.                                                         DETAB-65
032900     IF COLIX GREATER NCOLS GO TO DX005-2.                        DETAB-65
033000     MOVE ELIMC (COLIX) TO ELMCX.                                 DETAB-65
033100     IF COLMN (ROWIX, ELMCX) = " " OR "B"                         DETAB-65
033200         NEXT SENTENCE ELSE GO TO DX051.                          DETAB-65
033300     ADD 1 TO COLIX.                                              DETAB-65
033400     GO TO DX005-1.                                               DETAB-65
033500 DX005-2.                                                         DETAB-65
033600     PERFORM DX400 THRU DX402 VARYING COLIX FROM 1 BY 1           DETAB-65
033700       UNTIL COLIX IS GREATER THAN NCOLS.                         DETAB-65
033800     GO TO DX005.                                                 DETAB-65
033900 DX400.                                                           DETAB-65
034000     MOVE ELIMC (COLIX) TO ELMCT.                                 DETAB-65
034100     MOVE 1 TO ELMRX.                                             DETAB-65
034200 DX400-1.                                                         DETAB-65
034300     IF ELMRX = ROWIX GO TO DX400-2.                              DETAB-65
034400     IF COLMN (ELMRX, ELMCT) = " "                                DETAB-65
034500        MOVE "B" TO COLMN (ROWIX, ELMCT)                          DETAB-65
034600        GO TO DX402.                                              DETAB-65
034700     ADD 1 TO ELMRX.                                              DETAB-65
034800     GO TO DX400-1.                                               DETAB-65
034900 DX400-2.                                                         DETAB-65
035000     MOVE "Y" TO COLMN (ROWIX, ELMCT).                            DETAB-65
035100 DX402.                                                           DETAB-65
035200     EXIT.                                                        DETAB-65
035300 DX051.                                                           DETAB-65
035400     MOVE CNDI (ROWIX) TO COND.                                   DETAB-65
035500                                                                  DETAB-65
035600     NOTE  IS THERE A Y OR N IN THIS ROW.                         DETAB-65
035700                                                                  DETAB-65
035800     MOVE 1 TO COLIX.                                             DETAB-65
035900 DX051-1.                                                         DETAB-65
036000     IF COLIX GREATER NCOLS GO TO DX051-2.                        DETAB-65
036100     MOVE ELIMC (COLIX) TO ELMCX.                                 DETAB-65
036200     IF COLMN (ROWIX, ELMCX) NOT = "N" GO TO DX052.               DETAB-65
036300     ADD 1 TO COLIX.                                              DETAB-65
036400     GO TO DX051-1.                                               DETAB-65
036500 DX051-2.                                                         DETAB-65
036600     MOVE "EL001" TO DGOLN.                                       DETAB-65
036700     MOVE DGOTO TO CDOPR.                                         DETAB-65
036800     GO TO DX202.                                                 DETAB-65
036900 DX052.                                                           DETAB-65
037000     MOVE ROWIX TO ELMRX.                                         DETAB-65
037100                                                                  DETAB-65
037200     NOTE  ARE THE REST OF THE ELEMENTS IN THIS COLUMN BLANK.     DETAB-65
037300                                                                  DETAB-65
037400 DX052-1.                                                         DETAB-65
037500     IF ELMRX = NOCON GO TO DX052-2.                              DETAB-65
037600     COMPUTE ELMCT = ELMRX + 1.                                   DETAB-65
037700     IF COLMN (ELMCT, ELMCX) NOT = " " GO TO DX201.               DETAB-65
037800     ADD 1 TO ELMRX.                                              DETAB-65
037900     GO TO DX052-1.                                               DETAB-65
038000 DX052-2.                                                         DETAB-65
038100     IF NCOLS = 1 THEN MOVE ROWIX TO NOCON GO TO DX014.           DETAB-65
038200     MOVE COLIX TO DUMIX.                                         DETAB-65
038300     GO TO DX202.                                                 DETAB-65
038400                                                                  DETAB-65
038500     NOTE  PUSH LAST-IN-FIRST-OUT LIST.                           DETAB-65
038600                                                                  DETAB-65
038700 DX201.                                                           DETAB-65
038800     MOVE "DX" TO DGOLB.                                          DETAB-65
038900     ADD 1 TO LABNO, ADD 1 TO LABIX.                              DETAB-65
039000     MOVE LABNO TO DGONO, PDPUL (LABIX).                          DETAB-65
039100     MOVE DGOTO TO CDOPR.                                         DETAB-65
039200 DX202.                                                           DETAB-65
039300     MOVE 1 TO COLIX.                                             DETAB-65
039400                                                                  DETAB-65
039500     NOTE  IS THERE A N OR A BLANK IN THIS ROW.                   DETAB-65
039600                                                                  DETAB-65
039700 DX202-1.                                                         DETAB-65
039800     IF COLIX GREATER NCOLS GO TO DX202-2.                        DETAB-65
039900     MOVE ELIMC (COLIX) TO ELMCX.                                 DETAB-65
040000     IF COLMN (ROWIX, ELMCX) NOT = "Y" GO TO DX053.               DETAB-65
040100     ADD 1 TO COLIX.                                              DETAB-65
040200     GO TO DX202-1.                                               DETAB-65
040300 DX202-2.                                                         DETAB-65
040400     MOVE "EL001" TO DGOLN.                                       DETAB-65
040500     MOVE " ELSE " TO DELSE.                                      DETAB-65
040600     MOVE DGOTO TO ELOPR.                                         DETAB-65
040700     PERFORM DX204 THRU DX205.                                    DETAB-65
040800     GO TO DX300.                                                 DETAB-65
040900 DX053.                                                           DETAB-65
041000     MOVE ROWIX TO ELMRX.                                         DETAB-65
041100                                                                  DETAB-65
041200     NOTE  ARE THE REST OF THE ELEMENTS IN THIS COLUMN BLANK.     DETAB-65
041300                                                                  DETAB-65
041400 DX053-1.                                                         DETAB-65
041500     IF ELMRX = NOCON GO TO DX053-2.                              DETAB-65
041600     COMPUTE ELMCT = 1 + ELMRX.                                   DETAB-65
041700     IF COLMN (ELMCT, ELMCX) NOT = " "                            DETAB-65
041800         MOVE "." TO PIF, GO TO DX204.                            DETAB-65
041900     ADD 1 TO ELMRX.                                              DETAB-65
042000     GO TO DX053-1.                                               DETAB-65
042100 DX053-2.                                                         DETAB-65
042200     MOVE ROWIX TO NOCON.                                         DETAB-65
042300     IF DUMIX NOT = ZERO OR NCOLS = 1 THEN GO TO DX014.           DETAB-65
042400     MOVE COLIX TO ELMRX.                                         DETAB-65
042500     MOVE AZ TO DGOLB.                                            DETAB-65
042600     MOVE ELMCX TO DGONO.                                         DETAB-65
042700     MOVE " ELSE " TO DELSE.                                      DETAB-65
042800     MOVE DGOTO TO ELOPR.                                         DETAB-65
042900     PERFORM DX016 THRU DX020.                                    DETAB-65
043000     PERFORM DX011 THRU DX055.                                    DETAB-65
043100     MOVE NOCON TO ROWIX.                                         DETAB-65
043200     MOVE NROWS TO NOCON.                                         DETAB-65
043300 DX300.                                                           DETAB-65
043400     MOVE "." TO PELSE.                                           DETAB-65
043500     PERFORM L1OUT THRU RITAB.                                    DETAB-65
043600     PERFORM L2OUT THRU RITAB.                                    DETAB-65
043700     IF NORLS = ZERO GO TO DX038.                                 DETAB-65
043800     MOVE "DX" TO LABNM.                                          DETAB-65
043900     MOVE PDPUL (LABIX) TO LABVL.                                 DETAB-65
044000     SUBTRACT 1 FROM LABIX.                                       DETAB-65
044100     PERFORM DLOUT THRU RITAB.                                    DETAB-65
044200     GO TO DX004.                                                 DETAB-65
044300 DX204.                                                           DETAB-65
044400     IF DUMIX = ZERO GO TO DX205.                                 DETAB-65
044500     MOVE ROWIX TO NOCON.                                         DETAB-65
044600     MOVE AZ TO DGOLB.                                            DETAB-65
044700     MOVE ELIMC (DUMIX) TO DGONO.                                 DETAB-65
044800     MOVE DGOTO TO CDOPR.                                         DETAB-65
044900     MOVE DUMIX TO COLIX.                                         DETAB-65
045000     PERFORM DX016 THRU DX020.                                    DETAB-65
045100     MOVE NOCON TO ROWIX.                                         DETAB-65
045200     MOVE NROWS TO NOCON.                                         DETAB-65
045300 DX205.                                                           DETAB-65
045400     EXIT.                                                        DETAB-65
045500 DX009.                                                           DETAB-65
045600     PERFORM DX010 THRU DX055 VARYING ELMRX FROM 1 BY 1 UNTIL     DETAB-65
045700       ELMRX IS GREATER THAN NCOLS.                               DETAB-65
045800     GO TO DX003.                                                 DETAB-65
045900                                                                  DETAB-65
046000     NOTE  DELETE FROM PATH INDEX ALL COLUMNS THAT HAVE A Y       DETAB-65
046100     IN THIS ROW.                                                 DETAB-65
046200                                                                  DETAB-65
046300 DX010.                                                           DETAB-65
046400     MOVE ELIMC (ELMRX) TO COLIX.                                 DETAB-65
046500     IF COLMN (ROWIX, COLIX) NOT = "Y" GO TO DX055.               DETAB-65
046600 DX011.                                                           DETAB-65
046700     SUBTRACT 1 FROM NCOLS.                                       DETAB-65
046800     PERFORM DX012 VARYING ELMCX FROM ELMRX BY 1 UNTIL ELMCX      DETAB-65
046900       GREATER THAN NCOLS.                                        DETAB-65
047000     SUBTRACT 1 FROM ELMRX, SUBTRACT 1 FROM COLIX.                DETAB-65
047100 DX012.                                                           DETAB-65
047200     COMPUTE ELMCT = 1 + ELMCX.                                   DETAB-65
047300     MOVE ELIMC (ELMCT) TO ELIMC (ELMCX).                         DETAB-65
047400 DX055.                                                           DETAB-65
047500     EXIT.                                                        DETAB-65
047600 DX014.                                                           DETAB-65
047700     MOVE ELIMC (1) TO COLIX.                                     DETAB-65
047800     PERFORM DX015 VARYING ROWIX FROM 1 BY 1 UNTIL ROWIX = NROWS. DETAB-65
047900 DX015.                                                           DETAB-65
048000     MOVE COLMN (ROWIX, COLIX) TO SAVCL (ROWIX).                  DETAB-65
048100 DX056.                                                           DETAB-65
048200     MOVE 4 TO DUMIX.                                             DETAB-65
048300     PERFORM DX022 THRU DX031 VARYING COLIX FROM 1 BY 1 UNTIL     DETAB-65
048400       COLIX IS GREATER THAN NCOLS.                               DETAB-65
048500     GO TO DX032.                                                 DETAB-65
048600                                                                  DETAB-65
048700     NOTE  DETERMINE ACTION LABELS AND CHECK FOR REDUNDENCY.      DETAB-65
048800                                                                  DETAB-65
048900 DX022.                                                           DETAB-65
049000     MOVE ELIMC (COLIX) TO ELMCX.                                 DETAB-65
049100     IF COLMN (NOCON, ELMCX) NOT = "Y" GO TO DX029.               DETAB-65
049200     IF DUMIX = 3 OR DUMIX = 1 THEN GO TO DX059.                  DETAB-65
049300     IF DUMIX = 2 MOVE 3 TO DUMIX ELSE MOVE 1 TO DUMIX.           DETAB-65
049400     MOVE AZ TO DGOLB.                                            DETAB-65
049500     MOVE " ELSE " TO DELSE.                                      DETAB-65
049600     MOVE ELMCX TO DGONO.                                         DETAB-65
049700     MOVE  DGOTO TO CDOPR.                                        DETAB-65
049800     GO TO DX031.                                                 DETAB-65
049900 DX059.                                                           DETAB-65
050000     MOVE WRNING-2 TO WRNING-IMAGE.                               DETAB-65
050100     WRITE TAPE-LIST FROM WRNING-PRINT.                           DETAB-65
050200     PERFORM DX028 VARYING ELMRX FROM 1 BY 1 UNTIL ELMRX = NCOLS. DETAB-65
050300 DX028.                                                           DETAB-65
050400     MOVE "       RULE" TO TAPE-LIST.                             DETAB-65
050500     MOVE ELIMC (ELMRX) TO CARDX.                                 DETAB-65
050600     WRITE TAPE-LIST.                                             DETAB-65
050700 DX013.                                                           DETAB-65
050800     EXIT.                                                        DETAB-65
050900 DX029.                                                           DETAB-65
051000     IF COLMN (NOCON, ELMCX) NOT = "N" GO TO DX031.               DETAB-65
051100     IF DUMIX = 3 OR DUMIX = 2 PERFORM DX059 THRU DX013,          DETAB-65
051200       GO TO DX031.                                               DETAB-65
051300     IF DUMIX = 1 MOVE 3 TO DUMIX ELSE MOVE 2 TO DUMIX.           DETAB-65
051400     MOVE AZ TO DGOLB.                                            DETAB-65
051500     MOVE " ELSE " TO DELSE.                                      DETAB-65
051600     MOVE ELMCX TO DGONO.                                         DETAB-65
051700     MOVE DGOTO TO ELOPR.                                         DETAB-65
051800 DX031.                                                           DETAB-65
051900     EXIT.                                                        DETAB-65
052000 DX032.                                                           DETAB-65
052100     MOVE "EL001" TO DGOLN.                                       DETAB-65
052200     MOVE "." TO PELSE.                                           DETAB-65
052300     IF DUMIX = 2 MOVE DGOTO TO CDOPR ELSE                        DETAB-65
052400       IF DUMIX = 1 MOVE DGOTO TO ELOPR.                          DETAB-65
052500     MOVE CNDI (NOCON) TO COND.                                   DETAB-65
052600     PERFORM DX016 THRU DX020 VARYING COLIX FROM 1 BY 1 UNTIL     DETAB-65
052700       COLIX IS GREATER THAN NCOLS.                               DETAB-65
052800     GO TO DX301.                                                 DETAB-65
052900 DX016.                                                           DETAB-65
053000     MOVE ELIMC (COLIX) TO DUMIX.                                 DETAB-65
053100     MOVE 1 TO ROWIX.                                             DETAB-65
053200 DX016-1.                                                         DETAB-65
053300     IF ROWIX GREATER NOCON GO TO DX016-2.                        DETAB-65
053400     IF COLMN (ROWIX, DUMIX) = "B" GO TO DX504.                   DETAB-65
053500     ADD 1 TO ROWIX.                                              DETAB-65
053600     GO TO DX016-1.                                               DETAB-65
053700 DX016-2.                                                         DETAB-65
053800     MOVE 0 TO ROWIX.                                             DETAB-65
053900 DX016-3.                                                         DETAB-65
054000     IF ROWIX = NOCON GO TO DX016-4.                              DETAB-65
054100     COMPUTE ELMCX = NOCON - ROWIX.                               DETAB-65
054200     IF COLMN (ELMCX, DUMIX) = " " THEN                           DETAB-65
054300         MOVE "B" TO COLMN (ELMCX, DUMIX), GO TO DX020.           DETAB-65
054400     ADD 1 TO ROWIX.                                              DETAB-65
054500     GO TO DX016-3.                                               DETAB-65
054600 DX016-4.                                                         DETAB-65
054700     SUBTRACT 1 FROM NORLS.                                       DETAB-65
054800     PERFORM DX100 VARYING ELMCX FROM 1 BY 1                      DETAB-65
054900     UNTIL ELMCX IS GREATER THAN NORLS.                           DETAB-65
055000     GO TO DX020.                                                 DETAB-65
055100 DX100.                                                           DETAB-65
055200     COMPUTE ELMCT = ELMCX + 1                                    DETAB-65
055300     IF MATIX (ELMCX) IS NOT LESS THAN DUMIX                      DETAB-65
055400         MOVE MATIX (ELMCT) TO MATIX (ELMCX).                     DETAB-65
055500 DX504.                                                           DETAB-65
055600     MOVE 1 TO ELMCT.                                             DETAB-65
055700 DX504-1.                                                         DETAB-65
055800     IF ELMCT = ROWIX GO TO DX504-2.                              DETAB-65
055900     COMPUTE ELMCX = ROWIX - ELMCT.                               DETAB-65
056000     IF COLMN (ELMCX, DUMIX) = " " GO TO DX507.                   DETAB-65
056100     ADD 1 TO ELMCT.                                              DETAB-65
056200     GO TO DX504-1.                                               DETAB-65
056300 DX504-2.                                                                 
056400     MOVE "Y" TO COLMN (ROWIX, DUMIX).                            DETAB-65
056500     GO TO DX016.                                                 DETAB-65
056600 DX507.                                                           DETAB-65
056700     MOVE "B" TO COLMN (ELMCX, DUMIX).                            DETAB-65
056800     PERFORM DX508 VARYING ELMCX FROM ROWIX BY 1                  DETAB-65
056900        UNTIL ELMCX = NOCON.                                      DETAB-65
057000     GO TO DX020.                                                 DETAB-65
057100 DX508.                                                           DETAB-65
057200     IF COLMN (ELMCX, DUMIX) = "B"                                DETAB-65
057300        MOVE " " TO COLMN (ELMCX, DUMIX).                         DETAB-65
057400 DX020.                                                           DETAB-65
057500 EXIT.                                                                    
057600 DUMMY-1.                                                                 
057700     NOTE  POP  LAST-IN-FIRST-OUT LIST.                           DETAB-65
057800                                                                  DETAB-65
057900 DX301.                                                           DETAB-65
058000     PERFORM L1OUT THRU RITAB.                                    DETAB-65
058100     PERFORM L2OUT THRU RITAB.                                    DETAB-65
058200     IF NORLS = ZEROES GO TO DX038.                               DETAB-65
058300     MOVE "DX" TO LABNM.                                          DETAB-65
058400     MOVE PDPUL (LABIX) TO LABVL.                                 DETAB-65
058500     SUBTRACT 1 FROM LABIX.                                       DETAB-65
058600     PERFORM DLOUT THRU RITAB.                                    DETAB-65
058700                                                                  DETAB-65
058800     NOTE  SETUP INDEXES FOR NEXT PASS.                           DETAB-65
058900                                                                  DETAB-65
059000 DX302.                                                           DETAB-65
059100     MOVE NORLS TO NCOLS.                                         DETAB-65
059200     MOVE NROWS TO NOCON.                                         DETAB-65
059300     MOVE MATIT TO ELIMT.                                         DETAB-65
059400     MOVE 1 TO ROWIX.                                             DETAB-65
059500 DX302-1.                                                         DETAB-65
059600     IF ROWIX = NOCON                                             DETAB-65
059700       MOVE ERR-10 TO ERR-IMAGE                                   DETAB-65
059800       WRITE TAPE-LIST FROM ERR-PRNT                              DETAB-65
059900       GO TO DT001.                                               DETAB-65
060000                                                                  DETAB-65
060100     NOTE  DELETE THAT PATH GENERATED ON THE LAST PASS AND        DETAB-65
060200     FIND THE NEXT HIGHER NODE ON THE TREE.                       DETAB-65
060300                                                                  DETAB-65
060400     MOVE 1 TO COLIX.                                             DETAB-65
060500 DX034-1.                                                         DETAB-65
060600     IF SAVCL (ROWIX) = " " MOVE "N" TO SAVCL (ROWIX).            DETAB-65
060700     IF COLIX GREATER NCOLS GO TO DX004.                          DETAB-65
060800     MOVE ELIMC (COLIX) TO ELMCX.                                 DETAB-65
060900     IF COLMN (ROWIX, ELMCX) = " " OR COLMN (ROWIX, ELMCX)        DETAB-65
061000         = SAVCL (ROWIX) GO TO DX034-2.                           DETAB-65
061100     ADD 1 TO COLIX.                                              DETAB-65
061200     GO TO DX034-1.                                               DETAB-65
061300 DX034-2.                                                         DETAB-65
061400     PERFORM DX037 VARYING COLIX FROM 1 BY 1 UNTIL COLIX = NCOLS. DETAB-65
061500 DX037.                                                           DETAB-65
061600     MOVE ELIMC (COLIX) TO ELMCX.                                 DETAB-65
061700     MOVE COLIX TO ELMRX.                                         DETAB-65
061800     IF COLMN (ROWIX, ELMCX) NOT = " " AND COLMN (ROWIX, ELMCX)   DETAB-65
061900         NOT = SAVCL (ROWIX) PERFORM DX011 THRU DX055.            DETAB-65
062000 DX061.                                                           DETAB-65
062100     ADD 1 TO ROWIX.                                              DETAB-65
062200     GO TO DX302-1.                                               DETAB-65
062300 DX038.                                                           DETAB-65
062400     MOVE SPACES TO LINE3.                                        DETAB-65
062500     COMPUTE KEY-2 = NORULS - 1.                                  DETAB-65
062600     PERFORM DX039 THRU DX039B VARYING COLIX FROM 1 BY 1          DETAB-65
062700       UNTIL COLIX = KEY-2.                                       DETAB-65
062800 DX039.                                                           DETAB-65
062900     MOVE AZ TO LABNM.                                            DETAB-65
063000     MOVE COLIX TO LABVL.                                         DETAB-65
063100     PERFORM DLOUT THRU RITAB.                                    DETAB-65
063200     ADD 1 NCOND GIVING KEY-1.                                    DETAB-65
063300     PERFORM DXA01 THRU DXA04 VARYING EXIX FROM 1 BY 1 UNTIL      DETAB-65
063400       EXIX IS GREATER THAN NACTS.                                DETAB-65
063500     MOVE SPACES TO CRD-OUT.                                      DETAB-65
063600     EXAMINE DUM-5 TALLYING UNTIL FIRST "G".                      DETAB-65
063700     IF TALLY = 58 GO TO DX039H.                                  DETAB-65
063800     IF TALLY NOT = ZERO, THEN                                    DETAB-65
063900         IF TEMP (TALLY) NOT = SPACE GO TO DX039H.                DETAB-65
064000     COMPUTE DUMIX = TALLY + 1.                                   DETAB-65
064100     PERFORM DX039F VARYING TALLY FROM 1 BY 1 UNTIL TALLY = 6.    DETAB-65
064200     GO TO DX039G.                                                DETAB-65
064300 DX039F.                                                          DETAB-65
064400     MOVE TEMP (DUMIX)  TO EGOTO (TALLY).                         DETAB-65
064500     ADD 1 TO DUMIX.                                              DETAB-65
064600 DXA01.                                                           DETAB-65
064700     IF COLMN (KEY-1, COLIX) = " " GO TO DXA04.                   DETAB-65
064800     MOVE ATBL (EXIX) TO DUM-5, BNAME.                            DETAB-65
064900     PERFORM RSCAN.                                               DETAB-65
065000     ADD 3 TO DUMIX.                                              DETAB-65
065100     MOVE "." TO TCOLS (DUMIX).                                   DETAB-65
065200     PERFORM L2OUT THRU RITAB.                                    DETAB-65
065300 DXA04.                                                           DETAB-65
065400     ADD 1 TO KEY-1.                                              DETAB-65
065500 DX039G.                                                          DETAB-65
065600     IF DUM-4 = "GO TO" GO TO DX039B.                             DETAB-65
065700 DX039H.                                                          DETAB-65
065800     MOVE "GO TO DEXIT." TO B-MARG OF CRD-OUT.                    DETAB-65
065900     MOVE CRD-OUT TO TAPE-LIST.                                   DETAB-65
066000     PERFORM RITAB.                                               DETAB-65
066100 DX039B.                                                          DETAB-65
066200     EXIT.                                                        DETAB-65
066300 DX040.                                                           DETAB-65
066400     MOVE SPACES TO LINE3.                                        DETAB-65
066500     COMPUTE KEY-1 = NCOND + 1.                                   DETAB-65
066600     MOVE NORULS TO COLIX.                                        DETAB-65
066700     MOVE 1 TO EXIX.                                              DETAB-65
066800     MOVE KEY-1 TO TALLY.                                         DETAB-65
066900     MOVE 0 TO NRLS.                                              DETAB-65
067000 DX040-2.                                                         DETAB-65
067100     IF EXIX GREATER NACTS GO TO DX040-3.                         DETAB-65
067200     IF COLMN (TALLY, COLIX) NOT = " " ADD 1 TO NRLS.             DETAB-65
067300     ADD 1 TO TALLY, ADD 1 TO EXIX.                               DETAB-65
067400     GO TO DX040-2.                                               DETAB-65
067500 DX040-3.                                                         DETAB-65
067600     IF NRLS = ZEROES GO TO DX040-1.                              DETAB-65
067700     MOVE "EL001" TO DUM-6.                                       DETAB-65
067800     PERFORM DLOUT THRU RITAB.                                    DETAB-65
067900 DX040-1.                                                         DETAB-65
068000     PERFORM DXA01 THRU DXA04 VARYING EXIX FROM 1 BY 1 UNTIL      DETAB-65
068100       EXIX IS GREATER THAN NACTS.                                DETAB-65
068200     MOVE SPACES TO CRD-OUT.                                      DETAB-65
068300     MOVE "DEXIT. EXIT." TO BODY OF CRD-OUT.                      DETAB-65
068400     MOVE CRD-OUT TO TAPE-LIST.                                   DETAB-65
068500     PERFORM RITAB.                                               DETAB-65
068600     GO TO DT001.                                                 DETAB-65
068700 L1OUT.                                                           DETAB-65
068800     MOVE LINE1  TO CRD-OUT, TAPE-LIST.  GO TO RITAB.             DETAB-65
068900 L2OUT.                                                           DETAB-65
069000     MOVE LINE2  TO CRD-OUT, TAPE-LIST.  GO TO RITAB.             DETAB-65
069100 DLOUT.                                                           DETAB-65
069200     MOVE DLABEL TO CRD-OUT, TAPE-LIST.                           DETAB-65
069300 RITAB.                                                           DETAB-65
069400     WRITE TAPE-LIST.                                             DETAB-65
069500     WRITE CRD-OUT.                                               DETAB-65
069600 RSCAN.                                                           DETAB-65
069700     MOVE 58 TO DUMIX.                                            DETAB-65
069800     PERFORM RS001 THRU RS003.                                    DETAB-65
069900 RS001.                                                           DETAB-65
070000     IF TEMP (DUMIX) = SPACE GO TO RS002.                         DETAB-65
070100     ADD 2 TO DUMIX.                                              DETAB-65
070200     GO TO RS003.                                                 DETAB-65
070300 RS002.                                                           DETAB-65
070400     IF DUMIX = 1 GO TO RS003.                                    DETAB-65
070500     SUBTRACT 1 FROM DUMIX.                                       DETAB-65
070600     GO TO RS001.                                                 DETAB-65
070700 RS003.                                                           DETAB-65
070800 EXIT.                                                                    
070900 DUMMY-2.                                                                 
071000     NOTE DIAGNOSTIC SECTION.                                     DETAB-65
071100                                                                  DETAB-65
071200 EM01.                                                            DETAB-65
071300     MOVE ERR-1 TO ERR-IMAGE.                                     DETAB-65
071400     GO TO EM99.                                                  DETAB-65
071500 EM02.                                                            DETAB-65
071600     MOVE ERR-2 TO ERR-IMAGE.                                     DETAB-65
071700     GO TO EM99.                                                  DETAB-65
071800 EM03.                                                            DETAB-65
071900     MOVE ERR-3 TO ERR-IMAGE.                                     DETAB-65
072000     GO TO EM99.                                                  DETAB-65
072100 EM04.                                                            DETAB-65
072200     MOVE ERR-4 TO ERR-IMAGE.                                     DETAB-65
072300     GO TO EM99.                                                  DETAB-65
072400 EM05.                                                            DETAB-65
072500     MOVE ERR-5 TO ERR-IMAGE.                                     DETAB-65
072600     GO TO EM99.                                                  DETAB-65
072700 EM06.                                                            DETAB-65
072800     MOVE ERR-6 TO ERR-IMAGE.                                     DETAB-65
072900     GO TO EM99.                                                  DETAB-65
073000 EM07.                                                            DETAB-65
073100     MOVE ERR-7 TO ERR-IMAGE.                                     DETAB-65
073200     GO TO EM99.                                                  DETAB-65
073300 EM08.                                                            DETAB-65
073400     MOVE ERR-8 TO ERR-IMAGE.                                     DETAB-65
073500     GO TO EM99.                                                  DETAB-65
073600 EM09.                                                            DETAB-65
073700     MOVE ERR-9 TO ERR-IMAGE.                                     DETAB-65
073800 EM99.                                                            DETAB-65
073900     WRITE TAPE-LIST FROM ERR-PRNT.                               DETAB-65
074000 READ-1.                                                          DETAB-65
074100     READ CARD-INPUT INTO DETAB-CRD, AT END GO TO EOF.            DETAB-65
074200     MOVE SPACES TO IDFLD.                                        DETAB-65
074300     IF IDENT OF DETAB-CRD = "0000"                               DETAB-65
074400       MOVE "0" TO TAPE-LIST,                                     DETAB-65
074500       WRITE TAPE-LIST.                                           DETAB-65
074600     WRITE TAPE-LIST FROM DETAB-CRD.                              DETAB-65
074700     IF IDENT OF DETAB-CRD = "999X" GO TO EOF.                    DETAB-65
074800 SKIP01.                                                          DETAB-65
074900     IF LINE-ID OF DETAB-CRD NOT = "$" GO TO READ-1.              DETAB-65
075000     GO TO DT001.                                                 DETAB-65
075100 EOF.                                                             DETAB-65
075200     MOVE "0END DETAB/65 PREPROCESSOR RUN." TO TAPE-LIST.         DETAB-65
075300     WRITE TAPE-LIST.                                             DETAB-65
075400     CLOSE CARD-INPUT WITH LOCK.                                  DETAB-65
075500     CLOSE CARD-OUTPUT WITH LOCK, LIST-OUTPUT WITH LOCK.          DETAB-65
075600     STOP RUN.                                                    DETAB-65
075700 END-OF-JOB.                                                              
