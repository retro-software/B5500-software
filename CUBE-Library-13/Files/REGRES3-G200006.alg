          BEGIN                                                         00000000
COMMENT        REGRES3/STATMAN;                                         00000100
FILE IN        CARD (2,10);                                             00000200
FILE IN        TAPE2 (2,100),                                           00000300
               TAPE3 (2,100),                                           00000400
               TAPE4 (2,100),                                           00000500
               CORMAT (2,100),                                          00000600
               FACMAT (2,100);                                          00000700
SWITCH FILE     SW~CARD,CARD,TAPE2,TAPE3,TAPE4,CORMAT,FACMAT;           00000800
FORMAT         OKTL(100O);                                              00000900
FILE OUT       LINE 18(2,15),                                           00001000
               REGRES 0 (2,10);                                         00001100
FILE           TEM 2 (2,100);                                           00001200
FILE           ZSCOR 2 (2,100);                                         00001300
INTEGER        NROWA,NCOLA;                                             00001400
REAL           DET,EPS;                                                 00001500
INTEGER        RUNS,SCOR,N,SUB,K,J,I,VAR,VARS,INP;                      00001600
INTEGER        SUBT,DUMMY;                                              00001700
BOOLEAN        MULTI,BOOL;                                              00001800
LABEL          AGAIN,CONTINUE,EOT,EOC;                                  00001900
ARRAY          Z[0:50],SAM[0:50,0:50],B[0:50],RB[0:50],TITLE[0:9],      00002000
               RAWSC[0:50],IND[0:50],CORSC[0:50,0:50],S[0:50],SQ[0:50], 00002100
               CP[0:50,0:50],RMOD[0:50,0:50];                           00002200
INTEGER ARRAY  PRT[0:8],PUN[0:6];                                       00002300
REAL           SCAL,F,T,C,X,SS;                                         00002400
FORMAT         F1(10A6),                                                00002500
               F2(2I5,I2,2I1,   X6,9I1,X1,7I1),                         00002600
               F3(40I2),                                                00002700
               FERROR("NOT ENOUGH DATA ON TAPE"),                       00002800
               HEADING(X56,"REGRES3"/),                                 00002900
               FCR(/"RAW SCORES READ FROM CARDS"/),                     00003000
               FCC(/"CORRELATIONS READ FROM CARDS"/),                   00003100
               FTR(/"RAW SCORES READ FROM TAPE"),                       00003200
               FTC(/"CORRELATIONS READ FROM TAPE"),                     00003300
               FINP("FORMAT FOR INPUT OF RAW SCORES OR CORRELATIONS"),  00003400
               FINPM(/"FORMAT FOR INPUT OF MEANS"),                     00003500
               FINPS(/"FORMAT FOR INPUT OF SIGMAS"),                    00003600
               FHMSVI(/"MEANS,SIGMAS,VARIANCES AND INTERCORRELATIONS "/ 00003700
               "FOR INDEPENDENT (IV) AND DEPENDENT (DV) VARIABLES"/"WHE"00003800
             ,"RE DV IS THE LAST VARIABLE"/),                           00003900
               HMEAN(X10,"MEANS"),                                      00004000
               FMEAN(12F10.2),                                          00004100
               PMEAN("MEAN",X6,7F10.4),                                 00004200
               HSIGMA(X10,"SIGMAS"),                                    00004300
               FSIGMA(12F10.2),                                         00004400
               PSIGMA("SIGMA",X5,7F10.4),                               00004500
               HVAR(X10,"VARIANCES"),                                   00004600
               FVAR(12F10.2),                                           00004700
               PVAR("VARIANCES",X1,7E9.2),                              00004800
               HCOR(X10,"CORRELATION MATRIX"),                          00004900
               FCOR(12F10.2),                                           00005000
               PCOR("CORREL",X4,7F10.4),                                00005100
               HINV("INVERSE OF CORRELATION MATRIX FOR INDEPENDENT",    00005200
               " VARIABLES"),                                           00005300
               FINV(12F10.2),                                           00005400
               PINV("INVERSE",X3,7F10.4),                               00005500
               HAM(X10,"INVERSE OF AUGMENTED MATRIX"),                  00005600
               PAM("INVAUG MAT",X3,7F10.4),                             00005700
              HREGC(/"VECTOR OF REGRESSION COEFFICIENTS FOR STANDARD SC"00005800
               ,"ORE VARIABLES"),                                       00005900
               FREGC(12F10.2),                                          00006000
               HSMC(/"SQUARED MULTIPLE CORRELATION"),                   00006100
               FSMC(F12.2),                                             00006200
               HMC(/"MULTIPLE CORRELATION"),                            00006300
               FMC(F10.2),                                              00006400
               HSE(/"STANDARD ERROR OF ESTIMATE FOR STANDARD SCORES"),  00006500
               FSE(F10.2),                                              00006600
               HSER(/"STANDARD ERROR OF ESTIMATE FOR RAW AND DEVIATION "00006700
               ,"SCORES"),                                              00006800
               FSER(F10.2),                                             00006900
               HES(/"ESTIMATED SCORES"),                                00007000
               FES(20F6.2),                                             00007100
               PES("ES SCORES",X1,11F6.2),                              00007200
               HTABLE(//"ANALYSIS OF VARIANCE FOR REGRESSION"/X20,"DF", 00007300
               X5,"SUM SQUARES",X5,"MEAN SQUARES",X9,"F"/),             00007400
               FT1("REGRESSION",X9,I3,X5,F11.2,X5,F12.2,X5,F7.2),       00007500
               FT2(/"DEVIATION ABOUT"/"REGRESSION",X9,I3,X5,F11.2,X5,   00007600
               F12.2/),                                                 00007700
               FT3("TOTAL",X14,I3,X5,F11.2),                            00007800
               HSES(/"STANDARD ERROR OF SAMPLING FOR LARGE N: N GREATER"00007900
               ," THAN 50"),                                            00008000
               FSES(F10.2),                                             00008100
               HMCL(/"MULTIPLE CORRELATION CORRECTED FOR LEAST-SQUARE ",00008200
               "BIAS AND SMALLNESS OF SAMPLE."/"(UNBIASED ESTIMATE OF ",00008300
               "POPULATION VALUE)"),                                    00008400
               FMCL(F10.2),                                             00008500
               HRS(/"VECTOR OR REGRESSION COEFFICIENTS FOR RAW AND DEV",00008600
               "IATION SCORES"),                                        00008700
               SRS(/"REGRESSION COEFFICIENT"),                          00008800
               FRS(12F10.2);                                            00008900
FORMAT         MEAN(////////////////////////////////////////////////////00009000
               //////////////////////////////////////////////////////); 00009100
FORMAT         SIGMA(///////////////////////////////////////////////////00009200
               //////////////////////////////////////////////////////); 00009300
FORMAT         DATA(////////////////////////////////////////////////////00009400
               //////////////////////////////////////////////////////); 00009500
LIST           HEAD(N,VARS,RUNS,SCOR,INP,    FOR I~0 STEP 1 UNTIL 8 DO  00009600
               PRT[I],FOR I~0 STEP 1 UNTIL 6 DO PUN[I]),                00009700
               SUBSCRP(DUMMY,FOR I~0 STEP 1 UNTIL SUBT DO IND[I]),      00009800
               RAW(FOR J~0 STEP 1 UNTIL VAR DO RAWSC[J]),               00009900
               COR(FOR J~1 STEP 1 UNTIL VARS DO CORSC[I,J]);            00010000
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00010100
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00010200
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00010300
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00010400
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00010500
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00010600
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00010700
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00010800
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00010900
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00011000
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00011100
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00011200
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00011300
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00011400
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00011500
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00011600
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00011700
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00011800
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300011900
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00012000
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00012100
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00012200
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00012300
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00012400
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00012500
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00012600
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00012700
2";TALLY~1;TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY00012800
~TALLY+1;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TA00012900
LLY+CNTV;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~00013000
DI+CNTV;SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF 00013100
SCAN;PROCEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],P00013200
RNT[0],LCR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BE00013300
GIN LABEL L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR00013400
 THEN BEGIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN00013500
 CNT~ACCUM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;00013600
IF EXAMIN(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)E00013700
LSE IF ELCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER00013800
;ELCLASS~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TE00013900
MP,FINAL,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM 00014000
WDS;ST1~SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);E00014100
ND;END OF MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN 00014200
INTEGER CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,F00014300
LG);VALUE CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~00014400
LOC ST1;DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;00014500
SI~FMT;CTR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TAL00014600
LY+1;DI~DI-4;SI~SI+4); ST1~TALLY;                                %PTFFC 00014700
GO TO EXIT; FND: ST1~TALLY; SI~SI-4; DI~FLG; DS~WDS; EXIT:    %PTFFC    00014800
  SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FMAX00014900
~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RETU00015000
RN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~LOC00015100
 F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZE 00015200
IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LARGE00015300
R;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(1300015400
7);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALUE 00015500
NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMBER00015600
~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<8TH00015700
EN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(NF00015800
WDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~NFW00015900
D;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN L00016000
ABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,W200016100
,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W,W00016200
1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SKIP00016300
[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]&R00016400
EAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCEDUR00016500
E PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B";00016600
SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI+700016700
;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S;I00016800
NTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#;D00016900
EFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0THE00017000
N BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPEAT00017100
~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEGIN00017200
 SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:IF00017300
 FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO BE00017400
GIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"AND00017500
 ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,0,00017600
0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN BE00017700
GIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE IF 00017800
ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF ELCL00017900
ASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEXTE00018000
NT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0THE00018100
N BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,RSC00018200
ALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN CODE~00018300
100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCUM[00018400
1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F+100018500
;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SKIP00018600
+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END ELSE00018700
 BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE(W00018800
,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END ELSE00018900
 IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN C00019000
ODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(ELCL00019100
ASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-EL00019200
CLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE="00019300
X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"THEN00019400
 BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W1~500019500
;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,REP00019600
EAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR(100019700
36);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;DO 00019800
NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(3200019900
);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHRAS00020000
E;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[0])00020100
;WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(PRN00020200
T[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9];W00020300
HIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN BE00020400
GIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,IN00020500
FORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                   00020600
PROCEDURE     INVERT (N, A, EPS) ;                                      00020700
VALUE         N, EPS ;                                                  00020800
INTEGER       N ;                                                       00020900
REAL          EPS ;                                                     00021000
REAL ARRAY    A[0,0] ;                                                  00021100
         BEGIN                                                          00021200
INTEGER       I, J, K, II, N1, K2, L ;                                  00021300
REAL          BIG, TEMP, DIAG, Q ;                                      00021400
INTEGER ARRAY F[0:N] ;                                                  00021500
LABEL         I2, I3, I4, I5, I6, SK3 ,FINIS ;                          00021600
COMMENT       THE MATRIX IS TRIANGULARIZED USING ROW PIVOTS.  IF THE    00021700
              LARGEST ELEMENT IN A NEWLY CALCULATED COLUMN IS, IN       00021800
              ABSOLUTE VALUE, LESS THAN EPS, AN EXIT IS MADE ;          00021900
FORMAT         FERR9("NUMBER OF ROWS ! NUMBER OF COLS"),                00022000
               FERR8("MATRIX IS SINGULAR DET=0  NO INVERSE EXISTS");    00022100
               BOOL~FALSE;                                              00022200
               IF NROWA!NCOLA THEN BEGIN WRITE(LINE,FERR9);BOOL~TRUE;   00022300
               GO TO FINIS; END;                                        00022400
               DET ~ 1 ;                                                00022500
I2:           FOR I ~ 1 STEP 1 UNTIL N DO                               00022600
         BEGIN                                                          00022700
              II ~ I-1 ;                                                00022800
              FOR J ~ I STEP 1 UNTIL N DO                               00022900
         BEGIN                                                          00023000
              Q ~ 0 ;                                                   00023100
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[J,K] | A[K,I] + Q ;    00023200
              A[J,I] ~ A[J,I] - Q                                       00023300
         END ;                                                          00023400
              BIG ~ 0 ;  K2 ~ I ;                                       00023500
I3:           FOR K ~ I STEP 1 UNTIL N DO                               00023600
         BEGIN                                                          00023700
              IF ABS(A[K,I]) > BIG THEN                                 00023800
         BEGIN                                                          00023900
              BIG ~ ABS(A[K,I]) ;  K2 ~ K                               00024000
         END                                                            00024100
         END ;                                                          00024200
               IF BIG{EPS THEN BEGIN WRITE(LINE,FERR8); BOOL~TRUE;      00024300
               GO TO FINIS; END;                                        00024400
              F[I] ~ K2 ;                                               00024500
              IF K2 ! I THEN                                            00024600
          BEGIN                                                         00024700
               DET ~ - DET ;                                            00024800
I4:           FOR K ~ 1 STEP 1 UNTIL N DO                               00024900
         BEGIN                                                          00025000
              TEMP ~ A[I,K] ;  A[I,K] ~ A[K2,K] ;  A[K2,K] ~ TEMP       00025100
          END                                                           00025200
         END ;                                                          00025300
              DIAG ~ A[I,I] ;  DET ~ DET | DIAG ;                       00025400
                                                                        00025500
COMMENT       A PIVOT WAS MADE.  A ROW IS NOW CALCULATED.  ;            00025600
                                                                        00025700
              FOR J ~ I+1 STEP 1 UNTIL N DO                             00025800
         BEGIN                                                          00025900
              Q ~ 0 ;                                                   00026000
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;    00026100
              A[I,J] ~ (A[I,J] - Q) / DIAG                              00026200
         END                                                            00026300
         END ;                                                          00026400
                                                                        00026500
COMMENT       THE LOWER TRIANGULAR MATRIX IS INVERTED.  ;               00026600
                                                                        00026700
I5:           FOR I ~ 1 STEP 1 UNTIL N DO                               00026800
         BEGIN                                                          00026900
              II ~ I-1 ;  DIAG ~ A[I,I] ;                               00027000
              FOR J ~ 1 STEP 1 UNTIL I DO                               00027100
         BEGIN                                                          00027200
              IF I=J THEN A[I,J] ~ 1/DIAG ELSE                          00027300
         BEGIN                                                          00027400
              Q ~ 0 ;                                                   00027500
              FOR K ~ J STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;    00027600
              A[I,J] ~ -Q/DIAG                                          00027700
         END                                                            00027800
         END                                                            00027900
         END ;                                                          00028000
              N1 ~ N-1 ;                                                00028100
                                                                        00028200
COMMENT       THE UPPER TRIANGULAR MATRIX IS INVERTED.  ;               00028300
                                                                        00028400
              FOR I ~ N1 STEP -1 UNTIL 1 DO                             00028500
         BEGIN                                                          00028600
              II ~ I+1 ;                                                00028700
              FOR J ~ N STEP -1 UNTIL II DO                             00028800
         BEGIN                                                          00028900
              Q~0 ;  L ~ J-1 ;                                          00029000
              FOR K ~ II STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;    00029100
              A[I,J] ~ -A[I,J] - Q                                      00029200
         END                                                            00029300
         END ;                                                          00029400
                                                                        00029500
COMMENT       THE INVERTED TRIANGULAR MATRICES ARE MULTIPLIED  TOGETHER.00029600
              THEIR PRODUCT IS THE DESIRED INVERSE.  ;                  00029700
                                                                        00029800
              FOR I ~ 1 STEP 1 UNTIL N1 DO                              00029900
              FOR J ~ 1 STEP 1 UNTIL N DO                               00030000
         BEGIN                                                          00030100
              Q ~ 0 ;                                                   00030200
              IF I}J THEN                                               00030300
         BEGIN                                                          00030400
              FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;   00030500
              A[I,J] ~ A[I,J] + Q                                       00030600
         END                                                            00030700
              ELSE                                                      00030800
         BEGIN                                                          00030900
              FOR K ~ J STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;     00031000
              A[I,J] ~ Q                                                00031100
         END                                                            00031200
         END ;                                                          00031300
                                                                        00031400
COMMENT       THE COLUMNS OF THE PRODUCT ARE NOW PERMUTED IN SUCH A WAY 00031500
              AS TO COMPENSATE FOR THE ROW PIVOTS MADE IN THE COURSE OF 00031600
              TRIANGULARIZATION.  ;                                     00031700
                                                                        00031800
I6:           FOR J ~ N STEP -1 UNTIL 1 DO                              00031900
         BEGIN                                                          00032000
              K2 ~ F[J] ;                                               00032100
              IF F[J] = J THEN GO TO SK3 ;                              00032200
              FOR K ~ 1 STEP 1 UNTIL N DO                               00032300
         BEGIN                                                          00032400
              TEMP ~ A[K,K2] ;  A[K,K2] ~ A[K,J] ;  A[K,J] ~ TEMP       00032500
         END ;                                                          00032600
SK3:     END;                                                           00032700
FINIS:    END;                                                          00032800
               WRITE(LINE,HEADING);                                     00032900
               READ(CARD,F1,FOR I~0 STEP 1 UNTIL  9 DO TITLE[I])[EOC];  00033000
               READ(CARD,F2,HEAD)[EOC];                                 00033100
               VAR~VARS-1;                                              00033200
               MULTI~FALSE;                                             00033300
               IF INP=1 AND SCOR=1 THEN WRITE(LINE,FCR);                00033400
               IF INP=1 AND SCOR>1 THEN WRITE(LINE,FCC);                00033500
               IF INP>1 AND SCOR=1 THEN WRITE(LINE,FTR);                00033600
               IF INP>1 AND SCOR>1 THEN WRITE(LINE,FTC);                00033700
AGAIN:         READ(CARD[NO],F3,SUB)[EOC];                              00033800
               SUBT~SUB-1;                                              00033900
               READ(CARD,F3,SUBSCRP)[EOC];                              00034000
               IF MULTI THEN GO TO CONTINUE;                            00034100
               IF INP=1 THEN WRITE(LINE,FINP);                          00034200
               IF INP=1 THEN                                            00034300
               OBJECTFMTGEN(DATA) ELSE                                  00034400
               READ(SW[INP],OKTL,I,I)[EOT];                             00034500
               IF SCOR=1 THEN BEGIN                                     00034600
               FOR I~1 STEP 1 UNTIL N DO BEGIN                          00034700
               IF INP=1 THEN                                            00034800
               READ(CARD,DATA,RAW)[EOC] ELSE                            00034900
               READ(SW[INP],OKTL,RAW)[EOT];                             00035000
               WRITE(TEM,VARS,RAWSC[*]); END;                           00035100
               END;                                                     00035200
               IF SCOR=2 OR SCOR=3 THEN BEGIN                           00035300
               FOR I~1 STEP 1 UNTIL VARS   DO                           00035400
               IF INP=1 THEN                                            00035500
               READ(CARD,DATA,COR)[EOC] ELSE                            00035600
               READ(SW[INP],OKTL,COR)[EOT];                             00035700
               END;                                                     00035800
               IF SCOR=3 THEN BEGIN                                     00035900
               WRITE(LINE,FINPM);                                       00036000
               OBJECTFMTGEN(MEAN);                                      00036100
               READ(CARD,MEAN,FOR I~1 STEP 1 UNTIL VARS DO CORSC[0,I])[ 00036200
               EOC];                                                    00036300
               WRITE(LINE,FINPS);                                       00036400
               OBJECTFMTGEN(SIGMA);                                     00036500
               READ(CARD,SIGMA,FOR I~1 STEP 1 UNTIL VARS DO CORSC[I,0])[00036600
               EOC];                                                    00036700
               END;                                                     00036800
               IF SCOR=2 THEN                                           00036900
               FOR I~1 STEP 1 UNTIL VARS DO BEGIN                       00037000
               CORSC[0,I]~100;                                          00037100
               CORSC[I,0]~10;                                           00037200
               END;                                                     00037300
               IF SCOR=1 THEN BEGIN  REWIND(TEM);                       00037400
               FOR I~1 STEP 1 UNTIL N DO BEGIN READ(TEM,VARS,RAWSC[*]); 00037500
               FOR J~0 STEP 1 UNTIL VAR DO BEGIN                        00037600
               S[J]~S[J]+RAWSC[J];                                      00037700
               FOR K~0 STEP 1 UNTIL VAR DO                              00037800
               CP[J,K]~CP[J,K]+RAWSC[J]|RAWSC[K];                       00037900
               END;                                                     00038000
               END;                                                     00038100
               FOR J~0 STEP 1 UNTIL VAR DO SQ[J]~CP[J,J];               00038200
               FOR I~1 STEP 1 UNTIL VARS DO BEGIN                       00038300
               J~I-1;                                                   00038400
               CORSC[0,I]~S[J]/N;                                       00038500
               CORSC[I,0]~SQRT(SQ[J]/N-CORSC[0,I]*2);                   00038600
               END;                                                     00038700
               IF PRT[8]=1 OR PUN[6]=1 THEN BEGIN                       00038800
               REWIND(TEM);                                             00038900
               FOR I~1 STEP 1 UNTIL N DO BEGIN                          00039000
               READ(TEM,VARS,RAWSC[*]);                                 00039100
               FOR J~0 STEP 1 UNTIL VAR DO                              00039200
               RAWSC[J]~(RAWSC[J]-CORSC[0,J+1])/CORSC[J+1,0];           00039300
               WRITE(ZSCOR,VARS,RAWSC[*]);                              00039400
               END;                                                     00039500
               END;                                                     00039600
               FOR J~1 STEP 1 UNTIL VARS DO                             00039700
               FOR K~1 STEP 1 UNTIL VARS DO                             00039800
               CORSC[J,K]~(CP[J-1,K-1]-N|(CORSC[0,J]|CORSC[0,K]))/      00039900
                           (N|(CORSC[J,0]|CORSC[K,0]));                 00040000
               CLOSE(TEM,RELEASE);                                      00040100
               END;                             %END OF RAW SCORE-CORREL00040200
               IF PRT[0]=1 OR PRT[1]=1 OR PRT[2]=1 OR PRT[3]=1 THEN     00040300
               WRITE(LINE,FHMSVI);                                      00040400
               IF PRT[0]=1 THEN BEGIN           %MEANS                  00040500
               WRITE(LINE,HMEAN);                                       00040600
               WRITE(LINE,FMEAN,FOR I~1 STEP 1 UNTIL VARS DO CORSC[0,I])00040700
               ; END;                                                   00040800
               IF PUN[0]=1 THEN                                         00040900
              WRITE(REGRES,PMEAN,FOR I~1 STEP 1 UNTIL VARS DO CORSC[0,I]00041000
               );                                                       00041100
               IF PRT[1]=1 THEN BEGIN           %SIGMAS                 00041200
               WRITE(LINE,HSIGMA);                                      00041300
               WRITE(LINE,FSIGMA,FOR I~1 STEP 1 UNTIL VARS DO CORSC[I,0]00041400
               ); END;                                                  00041500
               IF PUN[1]=1 THEN                                         00041600
              WRITE(REGRES,PSIGMA,FOR I~1 STEP 1 UNTIL VARS DO CORSC[I,000041700
               ]);                                                      00041800
               IF PRT[2]=1 THEN BEGIN            %VARIANCES             00041900
               WRITE(LINE,HVAR);                                        00042000
               WRITE(LINE,FVAR,FOR I~1 STEP 1 UNTIL VARS DO (CORSC[I,0]*00042100
               2)); END;                                                00042200
               IF PUN[2]=1 THEN                                         00042300
              WRITE(REGRES,PVAR,FOR I~1 STEP 1 UNTIL VARS DO (CORSC[I,0]00042400
               *2));                                                    00042500
               IF PRT[3]=1 THEN BEGIN            %CORRELATIONS          00042600
               WRITE(LINE,HCOR);                                        00042700
               FOR I~1 STEP 1 UNTIL VARS DO                             00042800
               WRITE(LINE,FCOR,FOR J~1 STEP 1 UNTIL VARS DO CORSC[I,J]);00042900
               END;                                                     00043000
               IF PUN[3]=1 THEN                                         00043100
               FOR I~1 STEP 1 UNTIL VARS DO                             00043200
              WRITE(REGRES,PCOR,FOR J~1 STEP 1 UNTIL VARS DO CORSC[I,J])00043300
               ;                                                        00043400
CONTINUE:      FOR I~1 STEP 1 UNTIL SUBT DO BEGIN                       00043500
               FOR J~1 STEP 1 UNTIL SUBT DO                             00043600
               RMOD[I,J]~CORSC[IND[I],IND[J]];                          00043700
               RMOD[0,I ]~CORSC[0,IND[I]];                              00043800
               RMOD[I,0 ]~CORSC[IND[I],0];                              00043900
               RMOD[SUB,I]~CORSC[IND[0],IND[I]];                        00044000
               RMOD[I,SUB]~CORSC[IND[I],IND[0]];                        00044100
               END;                                                     00044200
               RMOD[SUB,0]~CORSC[IND[0],0];                             00044300
               RMOD[0,SUB]~CORSC[0,IND[0]];                             00044400
               FOR I~1 STEP 1 UNTIL SUBT DO                             00044500
               FOR J~1 STEP 1 UNTIL SUBT DO                             00044600
               SAM[I,J]~RMOD[I,J];                                      00044700
               NROWA~SUBT;NCOLA~SUBT;                                   00044800
               INVERT(SUBT,SAM,EPS);                                    00044900
               IF BOOL THEN GO TO EOC;                                  00045000
               IF PRT[4]=1 OR PUN[4]=1 THEN BEGIN                       00045100
               IF PRT[4]=1 THEN BEGIN                                   00045200
               WRITE(LINE,HINV);                                        00045300
               FOR I~1 STEP 1 UNTIL SUBT DO                             00045400
               WRITE(LINE,FINV,FOR J~1 STEP 1 UNTIL SUBT DO SAM[I,J]);  00045500
               END;                                                     00045600
               IF PUN[4]=1 THEN                                         00045700
               FOR I~1 STEP 1 UNTIL SUBT DO                             00045800
              WRITE(REGRES,PAM,FOR J~1 STEP 1 UNTIL SUBT DO SAM[I,J]);  00045900
               END;                                                     00046000
               IF PRT[5]=1 OR PUN[5]=1 THEN BEGIN                       00046100
               FOR I~1 STEP 1 UNTIL SUB DO                              00046200
               FOR J~1 STEP 1 UNTIL SUB DO                              00046300
               CP[I,J]~RMOD[I,J];                                       00046400
               NROWA~SUB;NCOLA~SUB;                                     00046500
               INVERT(SUB,CP,EPS);                                      00046600
               IF NOT BOOL THEN BEGIN                                   00046700
               IF PRT[5]=1 THEN BEGIN                                   00046800
               WRITE(LINE,HAM);                                         00046900
               FOR I~1 STEP 1 UNTIL SUB DO                              00047000
               WRITE(LINE,FINV,FOR J~1 STEP 1 UNTIL SUB DO CP[I,J]);    00047100
               END;                                                     00047200
               IF PUN[5]=1 THEN                                         00047300
               FOR I~1 STEP 1 UNTIL SUB DO                              00047400
              WRITE(REGRES,PAM,FOR J~1 STEP 1 UNTIL SUB DO CP[I,J]);    00047500
               END;                                                     00047600
               END;                                                     00047700
               FOR I~1 STEP 1 UNTIL SUBT DO          %MATRIX OF STANDARD00047800
               FOR K~1 STEP 1 UNTIL SUBT DO            %SCORE REGRESSION00047900
               B[I]~B[I]+SAM[I,K]|RMOD[K,SUB];                          00048000
               FOR I~1 STEP 1 UNTIL SUBT DO          %SQUARED MULTIPLE  00048100
               SCAL~B[I]|RMOD[I,SUB]+SCAL;            %CORRELATION      00048200
               WRITE(LINE,HSMC);                                        00048300
               WRITE(LINE,FSMC,SCAL);                                   00048400
               WRITE(LINE,HMC);   %MULTIPLE CORRELATION R               00048500
               WRITE(LINE,FMC,SQRT(SCAL));                              00048600
               WRITE(LINE,HSE);   %STANDARD ERROR OF ESTIMATE FOR STAND-00048700
               WRITE(LINE,FSE,SQRT(1-SCAL));      %ARD SCORES           00048800
               WRITE(LINE,HSER);  %STANDARD ERROR OF ESTIMATE FOR RAW   00048900
                                  %AND DEVIATION SCORES                 00049000
               WRITE(LINE,FSER,CORSC[VARS,0]|SQRT(1-SCAL));             00049100
               WRITE(LINE,HREGC);                                       00049200
               WRITE(LINE,FREGC,FOR I~1 STEP 1 UNTIL SUBT DO B[I]);     00049300
               F~((N-SUBT-1)|SCAL)/((1-SCAL)|SUBT);                     00049400
               WRITE(LINE,HTABLE);                                      00049500
               T~N|(CORSC[VARS,0]*2)|SCAL;                              00049600
               WRITE(LINE,FT1,SUBT,T,T/SUBT,F);                         00049700
               T~N|(CORSC[VARS,0]*2)|(1-SCAL);                          00049800
               WRITE(LINE,FT2,N-SUBT-1,T,T/(N-SUBT-1));                 00049900
               WRITE(LINE,FT3,N-1,N|CORSC[VARS,0]*2);                   00050000
               IF N>50 THEN BEGIN  WRITE(LINE,HSES);                    00050100
               WRITE(LINE,FSES,(1-SCAL)/SQRT(N)); %STANDARD ERROR OF    00050200
               END;                               %SAMPLING FOR LARGE N 00050300
               T~SQRT(1-((1-SCAL)|((N-1)/(N-SUBT))));                   00050400
               WRITE(LINE,HMCL);%MULTIPLE CORRELATION CORRECTED FOR     00050500
                                %LEAST-SQUARE BIAS AND SMALLNESS OF     00050600
                                %SAMPLE (UNBIASED ESTIMATE OF POPULATION00050700
                                %VALUE)                                 00050800
               WRITE(LINE,FMCL,T);                                      00050900
               IF PRT[6]=1 OR PRT[7]=1 THEN BEGIN WRITE(LINE,HRS);      00051000
               FOR I~1 STEP 1 UNTIL SUBT DO                             00051100
                 RB[I]~B[I]|(CORSC[VARS,0]/CORSC[I,0]);                 00051200
               WRITE(LINE,FRS,FOR I~1 STEP 1 UNTIL SUBT DO RB[I]);      00051300
               FOR I~1 STEP 1 UNTIL SUBT DO                             00051400
               C~C+RB[I]|CORSC[0,I];                                    00051500
               C~CORSC[0,VARS]-C;                                       00051600
               WRITE(LINE,SRS);                                         00051700
               WRITE(LINE,FRS,C);                                       00051800
               END;                                                     00051900
               IF PRT[8]=1 OR PUN[6]=1 THEN BEGIN                       00052000
               REWIND(ZSCOR);                                           00052100
               FOR I~1 STEP 1 UNTIL N DO BEGIN READ(ZSCOR,VARS,RAWSC[*])00052200
               ;                                                        00052300
               FOR J~1 STEP 1 UNTIL SUBT DO                             00052400
               Z[I]~RAWSC[IND[J]]|B[J]+Z[I];                            00052500
               X~X+Z[I];                                                00052600
               SS~SS+Z[I]*2;                                            00052700
               END;                                                     00052800
               FOR I~1 STEP 1 UNTIL N DO                                00052900
               Z[I]~(Z[I]-X/N)/SQRT(SS/N-(X/N)*2);                      00053000
               IF PRT[8]=1 THEN                                         00053100
               WRITE(LINE,HES);                                         00053200
               IF PRT[8]=1 THEN                                         00053300
               WRITE(LINE,FES,FOR I~1 STEP 1 UNTIL N DO Z[I]);          00053400
               IF PUN[6]=1 THEN                                         00053500
              WRITE(REGRES,PES,FOR I~1 STEP 1 UNTIL N DO Z[I]);         00053600
               END;                                                     00053700
               MULTI~TRUE;       %FOR MULTIPLE DEPENDENT VARIABLES      00053800
               GO TO AGAIN;                                             00053900
EOT:           WRITE(LINE,FERROR);                                      00054000
EOC:      END.                                                          00054100
                LAST CARD ON CRDIMG TAPE                                99999999
