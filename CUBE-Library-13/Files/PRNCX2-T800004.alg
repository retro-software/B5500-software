          BEGIN                                                         00000000
COMMENT        PRNCX2/STATMAN;                                          00000100
COMMENT        JACOBI TYPE SIMULTANEOUS SOLUTION FACTOR ANALYSIS        00000200
               CONVERTED FROM FORTRAN II 4/18/66: BY JOHN WACKWITZ  ;   00000300
FILE IN CARD 0(2,10);     FILE OUT  LINE 4(2,15);                       00000400
FILE OUT PUNCH 0(2,10);                                                 00000500
FILE IN        CORMAT 2(2,100) ;                                        00000600
FORMAT IN      FL3(10A6/R8.6,2I4,2R6.3,3I1);                            00000700
INTEGER        N,NLQ,I,WRT,WRC;                                         00000800
INTEGER        INP;                                                     00000900
REAL           P,RN,RA;                                                 00001000
INTEGER ARRAY  ID[0:9];                                                 00001100
FORMAT         OKTL(100O) ;                                             00001200
FORMAT OUT     TITLE(//X36,"JACOBI TYPE SIMULTANEOUS SOLUTION FACTOR",  00001300
                     " ANALYSIS"//),                                    00001400
               CASE (//X30,10A6//X10,"EIGENVALUES EXTRACTED TO:  ",R6.3/00001500
                X10,"CASE:   ",I5  /X10,"NUMBER OF VARIABLES:",I5),     00001600
               EVAL (//X54,"EIGENVALUES"/(10R12.5)/),                   00001700
               FACT1(//X53,"FACTOR MATRIX"//),                          00001800
               FACT2(/X1,"VARIABLE",I3/(10R12.5)/),                     00001900
               SQEVAL(//X47,"SQUARE ROOT OF EIGENVALUES"/(10R12.5)/),   00002000
               CUMP (//X40,"CUMULATIVE PROPORTION OF TOTAL VARIANCE"/   00002100
                    (10R12.5)/);                                        00002200
FORMAT         DATA(/////////////////////////////////////////////////   00002300
                    /////////////////////////////////////////////////   00002400
                    /////////////////////////////////////////////////   00002500
                    /////////////////////////////////////////////////); 00002600
REAL  XPR;                                                              00002700
INTEGER  QQ;                                                            00002800
LABEL FINIS,START;                                                      00002900
PROCEDURE ERROR(N); VALUE N; INTEGER N; BEGIN                           00003000
  FORMAT F(///////"STOP / PAUSE  NO. ", I5);                            00003100
  WRITE (LINE,  F, N);  GO TO FINIS END;                                00003200
REAL PROCEDURE INT(ARG1); VALUE ARG1; REAL ARG1;                        00003300
    BEGIN INT~(SIGN(ARG1)|ENTIER(ABS(ARG1))) END;                       00003400
REAL PROCEDURE MAX(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00003500
  BEGIN MAX~(IF ARG1}ARG2 THEN ARG1 ELSE ARG2) END;                     00003600
REAL PROCEDURE MIN(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00003700
  BEGIN MIN~(IF ARG1{ARG2 THEN ARG1 ELSE ARG2) END;                     00003800
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00003900
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00004000
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00004100
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00004200
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00004300
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00004400
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00004500
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00004600
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00004700
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00004800
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00004900
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00005000
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00005100
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00005200
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00005300
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00005400
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00005500
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00005600
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300005700
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00005800
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00005900
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00006000
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00006100
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00006200
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00006300
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00006400
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00006500
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100006600
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00006700
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00006800
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00006900
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00007000
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00007100
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00007200
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00007300
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00007400
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00007500
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00007600
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00007700
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00007800
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00007900
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00008000
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00008100
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00008200
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00008300
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00008400
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00008500
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00008600
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00008700
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00008800
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00008900
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00009000
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00009100
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00009200
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00009300
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800009400
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00009500
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00009600
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00009700
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00009800
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00009900
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00010000
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00010100
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00010200
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00010300
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00010400
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00010500
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00010600
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00010700
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00010800
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00010900
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00011000
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00011100
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00011200
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00011300
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00011400
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00011500
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00011600
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00011700
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00011800
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00011900
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00012000
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00012100
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00012200
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00012300
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00012400
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00012500
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00012600
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00012700
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00012800
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00012900
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00013000
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00013100
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100013200
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00013300
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00013400
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00013500
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00013600
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00013700
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000013800
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00013900
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00014000
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00014100
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00014200
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00014300
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEA00014400
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",00014500
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"///),LAYT(//"EXECUTION TIM00014600
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///00014700
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=00014800
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAY00014900
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FE00015000
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.00015100
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];EN00015200
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THEN00015300
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,00015400
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       00015500
COMMENT **************************  MAINPRO  ****************;          00015600
START:    BEGIN                                                         00015700
INTEGER        EX,AR;                                                   00015800
LABEL          LL1;                                                     00015900
LIST           HEAD(FOR I~ 0 STEP 1 UNTIL 9 DO ID[I],P,N,NLQ,RN,RA,WRT, 00016000
                     WRC,INP);                                          00016100
               READ(CARD,FL3,HEAD)[FINIS];                              00016200
               WRITE(LINE[PAGE]);                                       00016300
               DATELINE("PRNCX2");                                      00016400
               WRITE(LINE,TITLE);                                       00016500
               IF INP = 1 THEN READ(CORMAT,OKTL,N,N) ;                  00016600
               IF RA>0 THEN  AR ~1 ELSE AR ~ 0;                         00016700
               EX ~ N | 2;                                              00016800
LL1:      BEGIN                                                         00016900
REAL ARRAY     R[0:N,0:EX], D[0:N];                                     00017000
INTEGER        DX1;                                                     00017100
INTEGER        N1Q,N11Q,N2Q,J,NIQ,L,I1Q,K,NF;                           00017200
REAL           A,B,C,U,S,DRQ;                                           00017300
FORMAT         FL39(I2);                                                00017400
LIST LIST1(FOR I ~ 0 STEP 1 UNTIL 9 DO ID[I],RN,AR,N),                  00017500
     LIST2(FOR DX1 ~ 1 STEP 1 UNTIL N DO R[I,DX1]),                     00017600
     LIST3(L),                                                          00017700
     LIST4(FOR DX1 ~ 1 STEP 1 UNTIL NF DO R[DX1,DX1]),                  00017800
     LIST5(FOR DX1 ~ 1 STEP 1 UNTIL NF DO D[DX1]),                      00017900
    LIST6(AR,FOR DX1 ~ 1 STEP 1 UNTIL NF DO R[DX1,I]);                  00018000
LABEL L14,L38,L39,L40;                                                  00018100
               IF INP > 0 THEN                                          00018200
          BEGIN                                                         00018300
               FOR I ~ 1 STEP 1 UNTIL N DO                              00018400
               READ(CORMAT,OKTL,LIST2)[FINIS] ;                         00018500
          END  ELSE                                                     00018600
          BEGIN                                                         00018700
               OBJECTFMTGEN(DATA);                                      00018800
               WRITE(LINE[DBL]);                                        00018900
               FOR I ~ 1 STEP 1 UNTIL N DO                              00019000
               READ(CARD,DATA,LIST2) [FINIS];                           00019100
          END;                                                          00019200
               WRITE(LINE,CASE,LIST1);                                  00019300
          N1Q ~ N+1;                                                    00019400
          N11Q ~ N-1;                                                   00019500
          N2Q ~ N|2;                                                    00019600
          FOR I ~ N1Q, I+1 STEP 1 UNTIL N2Q DO BEGIN                    00019700
            FOR J ~ 1, J+1 STEP 1 UNTIL N DO BEGIN                      00019800
              R[J,I] ~ 0  END;                                          00019900
            END;                                                        00020000
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00020100
            NIQ ~ N+I;                                                  00020200
            R[I,NIQ] ~ 1  END;                                          00020300
          L ~ 0;                                                        00020400
L14:      FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00020500
            D[I] ~ R[I,I]  END;                                         00020600
          FOR I ~ 1, I+1 STEP 1 UNTIL N11Q DO BEGIN                     00020700
            I1Q ~ I+1;                                                  00020800
            FOR J ~ I1Q, J+1 STEP 1 UNTIL N DO BEGIN                    00020900
              DRQ ~ R[I,I]-R[J,J];                                      00021000
              A ~ SQRT(DRQ*2+4|R[J,I]*2);                               00021100
              A ~ SQRT((A+DRQ)/(2|A));                                  00021200
              B ~ SQRT(1-A*2);                                          00021300
              C ~ (ABS(1)|SIGN(R[J,I]));                                00021400
              FOR K ~ 1, K+1 STEP 1 UNTIL N2Q DO BEGIN                  00021500
                U ~ R[I,K]|A|C+R[J,K]|B;                                00021600
                R[J,K] ~ -R[I,K]|B|C+R[J,K]|A;                          00021700
                R[I,K] ~ U  END;                                        00021800
              FOR K ~ 1, K+1 STEP 1 UNTIL N DO BEGIN                    00021900
                U ~ R[K,I]|A|C+R[K,J]|B;                                00022000
                R[K,J] ~ -R[K,I]|B|C+R[K,J]|A;                          00022100
                R[K,I] ~ U  END;                                        00022200
              END;                                                      00022300
            END;                                                        00022400
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00022500
            D[I] ~ ABS(D[I]-R[I,I])  END;                               00022600
          S ~ 0;                                                        00022700
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00022800
            S ~ MAX(S,D[I])  END;                                       00022900
          IF S{P THEN GO TO L38;                                        00023000
          L ~ L+1;                                                      00023100
          IF L}NLQ THEN GO TO L38;                                      00023200
          GO TO L14;                                                    00023300
L38:      WRITE(LINE,FL39,LIST3);                                       00023400
          FOR I ~ 1 STEP 1 UNTIL N DO                                   00023500
          BEGIN                                                         00023600
               IF R[I,I] { RN THEN                                      00023700
          BEGIN                                                         00023800
               NF ~ I-1;                                                00023900
               GO TO L39;                                               00024000
          END  END;                                                     00024100
               NF ~ N;                                                  00024200
L39:           IF RA > 0 THEN                                           00024300
          BEGIN                                                         00024400
               FOR I ~ 1 STEP 1 UNTIL NF DO                             00024500
          BEGIN                                                         00024600
            IF ABS(1 - R[I-1,I-1]/R[I,I]) { RA THEN                     00024700
          BEGIN                                                         00024800
               RN ~ I-1;                                                00024900
               GO TO L40;                                               00025000
          END  END;                                                     00025100
               RN ~ NF;                                                 00025200
L40:           NF ~ RN;                                                 00025300
          END;                                                          00025400
               WRITE(LINE,EVAL,LIST4);                                  00025500
               D[1] ~ R[1,1];                                           00025600
               FOR I ~ 2 STEP 1 UNTIL NF DO                             00025700
               D[I] ~ D[I-1] + R[I,I];                                  00025800
               FOR I ~ 1 STEP 1 UNTIL NF DO                             00025900
               D[I] ~ D[I] / D[NF];                                     00026000
               WRITE(LINE,CUMP,LIST5);                                  00026100
               FOR I ~ 1 STEP 1 UNTIL NF DO                             00026200
               D[I] ~ SQRT(R[I,I]);                                     00026300
               FOR J ~ 1 STEP 1 UNTIL NF DO                             00026400
               FOR I ~ N1Q STEP 1 UNTIL N2Q DO                          00026500
               R[J,I] ~ R[J,I] | D[J];                                  00026600
               WRITE(LINE,SQEVAL,LIST5);                                00026700
               WRITE(LINE,FACT1);                                       00026800
               FOR I ~ N1Q STEP 1 UNTIL N2Q DO BEGIN                    00026900
               AR ~ I - N;                                              00027000
               WRITE(LINE,FACT2,LIST6) END;                             00027100
               IF WRT > 0 OR WRC > 0 THEN                               00027200
          BEGIN                                                         00027300
COMMENT        WRITE FACTOR MATRIX ON TAPE AND CARDS;                   00027400
FILE OUT       FACMAT 2(2,100, SAVE 10);                                00027500
INTEGER        J1Q,J2Q,JSECQ;                                           00027600
FORMAT         PUNC(2I4,X2,10F7.4);                                     00027700
LIST       FACP(AR,JSECQ,FOR DX1 ~ J1Q STEP 1 UNTIL J2Q DO R[DX1,I]),   00027800
            TAPO(FOR DX1 ~ 1 STEP 1 UNTIL NF DO R[DX1,I]);              00027900
LABEL          L77,L78,L79;                                             00028000
               IF WRT > 0 THEN BEGIN                                    00028100
               WRITE(FACMAT,OKTL,N,NF) ;                                00028200
               FOR I ~ N1Q STEP 1 UNTIL N2Q DO                          00028300
               WRITE(FACMAT,OKTL,TAPO) ;                                00028400
               REWIND(FACMAT) ;                                         00028500
          END ;                                                         00028600
               IF WRC >0 THEN  BEGIN                                    00028700
L77:           FOR I ~ N1Q STEP 1 UNTIL N2Q DO                          00028800
          BEGIN                                                         00028900
               AR ~ I - N;                                              00029000
               J1Q ~ 0;                                                 00029100
               J2Q ~ 0;                                                 00029200
               JSECQ ~ 0;                                               00029300
L78:           J1Q ~ J2Q+1;                                             00029400
               J2Q ~ J1Q+9;                                             00029500
               IF J2Q { NF   THEN GO TO L79;                            00029600
               J2Q ~NF;                                                 00029700
L79:           JSECQ ~ JSECQ+1;                                         00029800
               WRITE(PUNCH,PUNC,FACP);                                  00029900
               IF J2Q < NF   THEN GO TO L78;                            00030000
          END;                                                          00030100
          END;                                                          00030200
          END;                                                          00030300
               GO TO START;                                             00030400
          END;                                                          00030500
          END;                                                          00030600
                                                                        00030700
COMMENT **************************  OUTER BLOCK  ************;          00030800
FINIS:         DATELINE(0);                                             00030900
          END OF PROGRAM PRNCX2 .                                       00031000
                LAST CARD ON CRDIMG TAPE                                99999999
