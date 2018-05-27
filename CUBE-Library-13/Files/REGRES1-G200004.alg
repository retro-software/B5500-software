         BEGIN                                                          00000000
COMMENT        REGRES1/STATMAN;                                         00000100
COMMENT       MULTIPLE REGRESSION AND CORRELATION ANALYSIS              00000200
              CHARLES L. CLARK                                          00000300
              PROFESSIONAL SERVICES DIVISIONAL GROUP                    00000400
              BURROUGHS CORPORATION                                     00000500
              PROGRAM CONTAINS 416 CARDS                                00000600
              FIRST RELEASE DATE 12-1-64  ;                             00000700
FILE IN        CARD (2,10),                                             00000800
               TAPE1 (2,100),                                           00000900
               TAPE2 (2,100),                                           00001000
               TAPE3 (2,100),                                           00001100
               TAPE4 (2,100),                                           00001200
               CORMAT (2,100),                                          00001300
               FACMAT (2,100),                                          00001400
               ROTMAT (2,100);                                          00001500
SWITCH FILE    TIN~CARD,TAPE1,TAPE2,TAPE3,TAPE4,CORMAT,FACMAT,ROTMAT;   00001600
INTEGER        H,I,M,N,Q,INP;                                           00001700
INTEGER ARRAY ID[0:4] ;                                                 00001800
BOOLEAN        TR,MEAN,VAR,NORM,INV,RES,PCC,CC,CCINV,SWICH,SAVET;       00001900
LABEL         START, LL1,NOMO ;                                         00002000
FORMAT         OKTL(100O);                                              00002100
FORMAT       DATAFMT(/////////////////////////////////////////////////  00002200
                     /////////////////////////////////////////////////  00002300
                     /////////////////////////////////////////////////);00002400
FORMAT         FORM1(5A6,4I5,9(L1,X1),I1,L1),                           00002500
              FORM2 (26I3) ,                                            00002600
               FORM3(40I1);                                             00002700
FORMAT OUT    TITLE(X34,"GENERAL MULTIPLE REGRESSION AND ",             00002800
                    "CORRELATION ANALYSIS"///X45,5A6//                  00002900
                    X1,"SAMPLE SIZE",X2,I5/                             00003000
                    X1,"NO. OF INDEPENDENT VARIABLES",X2,I3/            00003100
                    X1,"NO. OF DEPENDENT VARIABLES",X4,I3//) ,          00003200
              FRM1 (X1,"NORMAL MATRIX"/) ,                              00003300
              FRM2 (X1,"ROW",I3/(6(X1,I3,X1,E15.8)/)) ,                 00003400
              FRM3 (//X1,"MEANS OF INDEPENDENT AND " ,                  00003500
                    "DEPENDENT VARIABLES"/) ,                           00003600
              FRM4 ((6(X1,I3,X1,E15.8)/)/) ,                            00003700
              FRM5 (/X1,"VARIANCES OF INDEPENDENT AND " ,               00003800
                    "DEPENDENT VARIABLES"/) ,                           00003900
              FRM6 (/X1,"THE INVERSE MATRIX"/) ,                        00004000
              FRM7 (/X1,"PRODUCT OF NORMAL AND INVERSE " ,              00004100
                    "MATRICES"/) ,                                      00004200
              FRM8 (/X47,"DEPENDENT VARIABLE NO.",I3/) ,                00004300
              FRM9 (X1,"REGRESSION COEFFICIENTS"//                      00004400
                    X1,"THE CONSTANT TERM IS",X3,E15.8) ,               00004500
              FRM10(/X1,"THE ERROR SUM SQUARE IS",X3,E15.8//            00004600
                    X1,"THE ERROR MEAN SQUARE IS",X2,E15.8//            00004700
                    X1,"THE STANDARD ERROR OF ESTIMATE IS" ,            00004800
                    X3,E15.8//                                          00004900
                    X1,"THE COEFFICIENT OF DETERMINATION " ,            00005000
                    "IS",X6,E15.8//                                     00005100
                    X1,"THE MULTIPLE CORRELATION " ,                    00005200
                    "COEFFICIENT IS",X2,E15.8//) ,                      00005300
              FRM11(X1,"STANDARD DEVIATIONS OF THE " ,                  00005400
                    "REGRESSION COEFFICIENTS"/) ,                       00005500
              FRM12(/X1,"T VALUE OF REGRESSION " ,                      00005600
                    "COEFFICIENTS"/) ,                                  00005700
              FRM13(/X36,"ANALYSIS OF VARIANCE"/X29,                    00005800
                    "FOR THE MULTIPLE LINEAR REGRESSION"//              00005900
                    X7,"SOURCE OF VARIATION",X7,"D.F.",X8,              00006000
                   "SUM OF",X13,"MEAN",X15,"F"/X45,                     00006100
                    "SQUARES",X11,"SQUARES",X11,"RATIO"/                00006200
                    X1,"DUE TO REGRESSION",X14,I5,X3,E15.8,             00006300
                    X3,E15.8,X3,E15.8/                                  00006400
                    X1,"DEVIATION ABOUT REGRESSION",X5,I5,              00006500
                    X3,E15.8,X3,E15.8/                                  00006600
                    X22,"TOTAL",X5,I5,X3,E15.8//) ,                     00006700
              FRM14(/X51,"TABLE OF RESIDUALS"//                         00006800
                    X25,"OBSERVATION",X8,"Y CALCULATED",X9,             00006900
                    "Y OBSERVED",X11,"RESIDUAL") ,                      00007000
              FRM15(X28,I5,X9,E15.8,X5,E15.8,X5,E15.8) ,                00007100
              FRM16(//X25,"TEST OF EXTREME RESIDUALS"//                 00007200
                    X25,"RANGE OF RESIDUALS",X20,E15.8/                 00007300
                    X25,"RANGE / STANDARD ERROR OF " ,                  00007400
                    "ESTIMATE",X4,E15.8//) ,                            00007500
              FRM17(X1,"CORRELATION COEFFICIENTS"/) ,                   00007600
              FRM18(/X1,"INVERSE OF CORRELATION MATRIX"/) ,             00007700
              FRM19(/X1,"PARTIAL CORRELATION COEFFICIENTS"/) ;          00007800
FILE OUT      PRINT 4 (1,15) ;                                          00007900
DEFINE         LINE =PRINT#;                                            00008000
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00008100
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00008200
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00008300
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00008400
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00008500
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00008600
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00008700
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00008800
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00008900
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00009000
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00009100
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00009200
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00009300
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00009400
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00009500
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00009600
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00009700
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00009800
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300009900
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00010000
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00010100
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00010200
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00010300
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00010400
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00010500
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00010600
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00010700
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100010800
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00010900
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00011000
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00011100
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00011200
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00011300
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00011400
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00011500
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00011600
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00011700
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00011800
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00011900
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00012000
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00012100
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00012200
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00012300
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00012400
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00012500
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00012600
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00012700
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00012800
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00012900
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00013000
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00013100
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00013200
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00013300
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00013400
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00013500
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800013600
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00013700
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00013800
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00013900
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00014000
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00014100
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00014200
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00014300
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00014400
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00014500
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00014600
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00014700
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00014800
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00014900
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00015000
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00015100
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00015200
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00015300
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00015400
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00015500
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00015600
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00015700
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00015800
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00015900
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00016000
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00016100
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00016200
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00016300
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00016400
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00016500
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00016600
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00016700
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00016800
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00016900
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00017000
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00017100
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00017200
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00017300
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100017400
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00017500
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00017600
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00017700
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00017800
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00017900
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000018000
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00018100
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00018200
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00018300
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00018400
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00018500
START:   BEGIN                                                          00018600
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],M,N,Q,             00018700
               H,TR,MEAN,VAR,NORM,INV,RES,PCC,CC,CCINV,INP,SAVET);      00018800
              READ (CARD ,FORM1,HEAD) [NOMO] ;                          00018900
         END ;                                                          00019000
              SWICH ~ FALSE ;                                           00019100
LL1:     BEGIN                                                          00019200
INTEGER       J, K, L, P ;                                              00019300
ARRAY         X[0:Q],YSQ[0:Q-N],A[0:N+1,0:Q],B[0:H],SB[0:N] ,           00019400
              T[0:Q] ;                                                  00019500
REAL          C, RSS, RMS, F, ERS, EMS, SE, CD, MCC, HI, LO, R, S ;     00019600
LABEL         RERED, RESDU ;                                            00019700
LIST          TRAN (FOR J ~ 1 STEP 1 UNTIL Q DO T[J]) ,                 00019800
              DATA (FOR I ~ 1 STEP 1 UNTIL H DO B[I]) ,                 00019900
              OUT1 (FOR J ~ 0 STEP 1 UNTIL 4 DO ID[J], M, N,            00020000
                    L) ,                                                00020100
              OUT2 (I, FOR J ~ I STEP 1 UNTIL N DO                      00020200
                    [J, A[I,J]]) ,                                      00020300
              OUT3 (FOR I ~ 1 STEP 1 UNTIL N DO [I, B[I]]) ,            00020400
              OUT4 (FOR I ~ 1 STEP 1 UNTIL N DO [I, SB[I]]),            00020500
              OUT5 (FOR I ~ 1 STEP 1 UNTIL N DO [I, X[I]]) ,            00020600
              OUT6 (I, FOR J ~ I STEP 1 UNTIL N DO                      00020700
                    [J, X[J]]) ;                                        00020800
PROCEDURE      INVERT(N, B);                                            00020900
VALUE          N;                                                       00021000
INTEGER        N;                                                       00021100
ARRAY          B[0,0];                                                  00021200
COMMENT        THIS PROCEDURE INVERTS A SYMMETRIC MATRIX                00021300
               A[I,J] OF WHICH THE UPPER HALF IS GIVEN.                 00021400
               ALL FLOATING POINT OPERATIONS ARE DONE IN                00021500
               DOUBLE PRECISION;                                        00021600
          BEGIN                                                         00021700
INTEGER        I,J,K,L;                                                 00021800
ARRAY          AH, AL[0:N,0:N], ZH, ZL[0:N];                            00021900
REAL           DGH, DGL, RH, RL;                                        00022000
               FOR I ~ 1 STEP 1 UNTIL N DO                              00022100
               FOR J ~ 1 STEP 1 UNTIL N DO                              00022200
               DOUBLE(B[I,J],0,~,AH[I,J],AL[I,J]);                      00022300
               FOR K ~ 1 STEP 1 UNTIL N-1 DO                            00022400
          BEGIN                                                         00022500
               DOUBLE(AH[K,K], AL[K,K],~,DGH, DGL);                     00022600
               FOR I ~ K+1 STEP 1 UNTIL N DO                            00022700
          BEGIN                                                         00022800
               DOUBLE(AH[K,I], AL[K,I], DGH, DGL,/,~,RH, RL);           00022900
               FOR J ~ I STEP 1 UNTIL N DO                              00023000
               DOUBLE(AH[I,J], AL[I,J], AH[K,J], AL[K,J], RH, RL,|,-,~, 00023100
               AH[I,J], AL[I,J]);                                       00023200
               FOR L ~ 1 STEP 1 UNTIL K-1 DO                            00023300
               DOUBLE(AH[L,I], AL[L,I], AH[L,K], AL[L,K], RH, RL,|,-,~, 00023400
               AH[L,I], AL[L,I]);                                       00023500
               AH[K,I] ~ - RH;                                          00023600
               AL[K,I] ~ - RL;                                          00023700
          END                                                           00023800
          END  ;                                                        00023900
COMMENT        BACKWARD PASS;                                           00024000
               FOR K ~ 1 STEP 1 UNTIL N DO                              00024100
          BEGIN                                                         00024200
               FOR J ~ 1 STEP 1 UNTIL N DO                              00024300
               DOUBLE(AH[K,J], AL[K,J], AH[J,J], AL[J,J],/,~,           00024400
               ZH[J], ZL[J]);                                           00024500
               DOUBLE(,1.0, AH[K,K], AL[K,K],/,~, ZH[K], ZL[K]);        00024600
               FOR I ~ K STEP 1 UNTIL N DO                              00024700
          BEGIN                                                         00024800
               FOR J ~ I+1 STEP 1 UNTIL N DO                            00024900
               DOUBLE(ZH[I], ZL[I], ZH[J], ZL[J], AH[I,J], AL[I,J],|,+, 00025000
               ~, ZH[I], ZL[I]);                                        00025100
               B[K,I] ~ ZH[I]                                           00025200
          END                                                           00025300
          END                                                           00025400
          END  INVERT;                                                  00025500
PROCEDURE      INVPDS(N,B,DIAGONAL);                                    00025600
VALUE          N;                                                       00025700
INTEGER        N;                                                       00025800
REAL ARRAY     B[0,0], DIAGONAL[0];                                     00025900
COMMENT        DOUBLE PRECISION VERSION OF SYMMETRIC MATRIX             00026000
               INVERSION PROCEDURE;                                     00026100
          BEGIN                                                         00026200
INTEGER        I,J,K,I1,L;                                              00026300
REAL           DIAGH, DIAGL, QH, QL;                                    00026400
REAL ARRAY     AH, AL[0:N,0:N], TEMPH, TEMPL[0:N];                      00026500
LABEL          IN1, IN2, IN3;                                           00026600
               FOR I ~ 1 STEP 1 UNTIL N DO                              00026700
               FOR J ~ 1 STEP 1 UNTIL N DO                              00026800
               DOUBLE(B[I,J],0,~,AH[I,J],AL[I,J]);                      00026900
IN1:           FOR I ~ N STEP -1 UNTIL 1 DO                             00027000
          BEGIN                                                         00027100
               DIAGONAL[I] ~ B[I,I];                                    00027200
               FOR K~ I+1 STEP 1 UNTIL N DO                             00027300
               DOUBLE(AH[I,K], AL[I,K], AH[K,K], AL[K,K], |,~,          00027400
               TEMPH[K], TEMPL[K]);                                     00027500
               FOR J ~ I STEP -1 UNTIL 1 DO                             00027600
          BEGIN                                                         00027700
               DOUBLE(,0.0, ~, QH, QL);                                 00027800
               FOR K ~ I+1 STEP 1 UNTIL N DO                            00027900
               DOUBLE(QH, QL, AH[J,K], AL[J,K], TEMPH[K], TEMPL[K],     00028000
               |, +, ~, QH, QL);                                        00028100
               IF I=J THEN                                              00028200
          BEGIN                                                         00028300
               DOUBLE (AH[J,I], AL[J,I], QH, QL, -, ~, DIAGH, DIAGL);   00028400
               DOUBLE(DIAGH, DIAGL, ~, AH[J,I], AL[J,I])                00028500
          END                                                           00028600
               ELSE                                                     00028700
               DOUBLE(AH[J,I], AL[J,I], QH, QL, -, DIAGH, DIAGL, /, ~,  00028800
               AH[J,I], AL[J,I])                                        00028900
          END                                                           00029000
          END  ;                                                        00029100
IN2:           FOR I ~ N STEP -1 UNTIL 1 DO                             00029200
          BEGIN                                                         00029300
               I1 ~ I+1;                                                00029400
               DOUBLE(,1.0, AH[I,I], AL[I,I],  /, ~, AH[I,I], AL[I,I]); 00029500
               FOR J~ N STEP -1 UNTIL I1 DO                             00029600
          BEGIN                                                         00029700
               DOUBLE(,0.0, ~, QH, QL);                                 00029800
               L ~ J-1;                                                 00029900
               FOR K ~ I1 STEP 1 UNTIL L DO                             00030000
               DOUBLE(QH, QL, AH[I,K], AL[I,K], AH[K,J], AL[K,J],       00030100
               |, +, ~, QH, QL);                                        00030200
               DOUBLE(AH[I,J], AL[I,J], QH, QL,+, ~, AH[I,J], AL[I,J]); 00030300
               AH[I,J] ~ -AH[I,J];                                      00030400
               AL[I,J] ~ -AL[I,J]                                       00030500
          END                                                           00030600
          END  ;                                                        00030700
IN3:           FOR I~ N STEP -1 UNTIL 1 DO                              00030800
          BEGIN                                                         00030900
               I1 ~ I-1;                                                00031000
               DOUBLE(AH[I,I], AL[I,I], ~, DIAGH, DIAGL);               00031100
               FOR K ~ 1 STEP 1 UNTIL I1 DO                             00031200
               DOUBLE(AH[K,K], AL[K,K], AH[K,I], AL[K,I], |, ~,         00031300
               TEMPH[K], TEMPL[K]);                                     00031400
               FOR J ~ N STEP -1 UNTIL I DO                             00031500
          BEGIN                                                         00031600
               DOUBLE(,0.0, ~, QH, QL);                                 00031700
               FOR K ~ 1 STEP 1 UNTIL I1 DO                             00031800
               DOUBLE(QH, QL, AH[K,J], AL[K,J], TEMPH[K], TEMPL[K],     00031900
               |, +, ~,  QH, QL);                                       00032000
               IF I=J THEN                                              00032100
               DOUBLE(AH[I,J], AL[I,J], QH, QL, +, ~, AH[I,J], AL[I,J]) 00032200
               ELSE                                                     00032300
               DOUBLE(QH, QL, AH[I,J], AL[I,J], DIAGH, DIAGL, |,+,~,    00032400
               AH[I,J], AL[I,J])                                        00032500
          END                                                           00032600
          END  ;                                                        00032700
               FOR I ~ 1 STEP 1 UNTIL N DO                              00032800
               FOR J ~ I STEP 1 UNTIL N DO                              00032900
               B[I,J] ~ AH[I,J]                                         00033000
          END  INVPDS IN DOUBLE PRECISION;                              00033100
LABEL         NAXT, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10,            00033200
              L11, L12, L13, L14, L15 ,LL3;                             00033300
SWITCH        U ~ L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L1  ;  00033400
FILE          TAPEA (2,Q+1) ;                                           00033500
INTEGER        II, JJ;                                                  00033600
ARRAY          DIAGONAL[0:N], SAM[0:N,0:N];                             00033700
              IF TR THEN                                                00033800
              READ (CARD ,FORM2,TRAN) ;                                 00033900
              FOR I ~ 0 STEP 1 UNTIL N DO                               00034000
              FOR J ~ I STEP 1 UNTIL Q DO                               00034100
              A[I,J] ~ 0 ;                                              00034200
              L ~ Q-N ;                                                 00034300
              FOR P ~ 1 STEP 1 UNTIL L DO                               00034400
              YSQ[P] ~ 0 ;                                              00034500
               IF INP=0 THEN                                            00034600
               OBJECTFMTGEN(DATAFMT)                                    00034700
               ELSE                                                     00034800
               READ(TIN[INP],OKTL,M,H);                                 00034900
               WRITE(LINE[DBL]);                                        00035000
              FOR L ~ 1 STEP 1 UNTIL M DO                               00035100
         BEGIN                                                          00035200
               IF INP=0 THEN READ(CARD,DATAFMT,DATA)                    00035300
               ELSE READ(TIN[INP],OKTL,DATA);                           00035400
              FOR J ~ 1 STEP 1 UNTIL Q DO                               00035500
         BEGIN                                                          00035600
              IF TR THEN                                                00035700
         BEGIN                                                          00035800
              GO TO U[T[J]] ;                                           00035900
L1:           X[J] ~ B[J] ;                                             00036000
              GO TO NAXT ;                                              00036100
L2:           X[J] ~ B[J] | B[J] ;                                      00036200
              GO TO NAXT ;                                              00036300
L3:           X[J] ~ 1 / B[J] ;                                         00036400
              GO TO NAXT ;                                              00036500
L4:           X[J] ~ SQRT (B[J]) ;                                      00036600
              GO TO NAXT ;                                              00036700
L5:           X[J] ~ EXP (B[J]) ;                                       00036800
              GO TO NAXT ;                                              00036900
L6:           X[J] ~ LN (B[J]) ;                                        00037000
              GO TO NAXT ;                                              00037100
L7:           X[J] ~ SIN (B[J]) ;                                       00037200
              GO TO NAXT ;                                              00037300
L8:           X[J] ~ COS (B[J]) ;                                       00037400
              GO TO NAXT ;                                              00037500
L9:           X[J] ~ B[J+1] ;                                           00037600
              GO TO NAXT ;                                              00037700
L10:          X[J] ~ B[J+2] ;                                           00037800
              GO TO NAXT ;                                              00037900
L11:          X[J] ~ B[J+3] ;                                           00038000
              GO TO NAXT ;                                              00038100
L12:          X[J] ~ B[J+4] ;                                           00038200
              GO TO NAXT ;                                              00038300
L13:          X[J] ~ B[J+5] ;                                           00038400
              GO TO NAXT ;                                              00038500
L14:          X[J] ~ B[J+6] ;                                           00038600
              GO TO NAXT ;                                              00038700
L15:          X[J] ~ B[J+7] ;                                           00038800
              GO TO NAXT ;                                              00038900
NAXT:    END                                                            00039000
              ELSE                                                      00039100
              X[J] ~ B[J]                                               00039200
         END ;                                                          00039300
              WRITE(TAPEA,Q+1,X[*])                                     00039400
         END;                                                           00039500
              REWIND(TAPEA) ;                                           00039600
RERED:        FOR L ~ 1 STEP 1 UNTIL M DO                               00039700
         BEGIN                                                          00039800
              READ (TAPEA,Q+1,X[*]) ;                                   00039900
              IF SWICH THEN                                             00040000
         BEGIN                                                          00040100
              F ~ B[0] ;                                                00040200
              FOR I ~ 1 STEP 1 UNTIL N DO                               00040300
              F ~ F + B[I] | X[I] ;                                     00040400
              C ~ F - X[P] ;                                            00040500
              WRITE (PRINT,FRM15,L,F,X[P],C) ;                          00040600
              IF HI < C THEN                                            00040700
              HI ~ C ;                                                  00040800
              IF LO > C THEN                                            00040900
              LO ~ C                                                    00041000
         END                                                            00041100
              ELSE                                                      00041200
         BEGIN                                                          00041300
              FOR K ~ 1 STEP 1 UNTIL Q DO                               00041400
         BEGIN                                                          00041500
              C ~ X[K] ;                                                00041600
              A[0,K] ~ A[0,K] + C ;                                     00041700
              IF K { N THEN                                             00041800
         BEGIN                                                          00041900
              FOR J ~ K STEP 1 UNTIL Q DO                               00042000
              A[K,J] ~ A[K,J] + C | X[J]                                00042100
         END                                                            00042200
         END ;                                                          00042300
              FOR J ~ N+1 STEP 1 UNTIL Q DO                             00042400
              YSQ[J-N] ~ YSQ[J-N] + X[J] | X[J]                         00042500
         END                                                            00042600
         END ;                                                          00042700
              IF SWICH THEN                                             00042800
              GO TO RESDU ;                                             00042900
              FOR I ~ 1 STEP 1 UNTIL Q DO                               00043000
         BEGIN                                                          00043100
              R ~ A[0,I] / M ;                                          00043200
              IF I { N THEN                                             00043300
         BEGIN                                                          00043400
              C ~ A[I,I] - A[0,I] | R ;                                 00043500
              A[I,I] ~ C ;                                              00043600
              A[I,0] ~ C ;                                              00043700
              FOR J ~ I+1 STEP 1 UNTIL N DO                             00043800
         BEGIN                                                          00043900
              C ~ A[I,J] - A[0,J]|R ;                                   00044000
              A[I,J] ~ C ;                                              00044100
              A[J,I] ~ C                                                00044200
         END ;                                                          00044300
              FOR J ~ N+1 STEP 1 UNTIL Q DO                             00044400
              A[I,J] ~ A[I,J] - A[0,J]|R                                00044500
         END                                                            00044600
              ELSE                                                      00044700
              YSQ[I-N] ~ YSQ[I-N] - A[0,I] | R ;                        00044800
              A[0,I] ~ R                                                00044900
         END ;                                                          00045000
              L ~ Q-N ;                                                 00045100
              WRITE(PRINT[PAGE]) ;                                      00045200
              WRITE(PRINT,TITLE,OUT1) ;                                 00045300
COMMENT       PRINTOUT OF NORMAL MATRIX ;                               00045400
              IF NORM THEN                                              00045500
         BEGIN                                                          00045600
              WRITE (PRINT,FRM1) ;                                      00045700
              FOR I ~ 1 STEP 1 UNTIL N DO                               00045800
              WRITE (PRINT,FRM2,OUT2)                                   00045900
         END ;                                                          00046000
COMMENT       COMPUTATION OF MEANS AND VARIANCES ;                      00046100
              IF MEAN THEN                                              00046200
         BEGIN                                                          00046300
              FOR I ~ 1 STEP 1 UNTIL Q DO                               00046400
              X[I] ~ A[0,I] ;                                           00046500
              WRITE (PRINT,FRM3) ;                                      00046600
              L ~ N ;                                                   00046700
              N ~ Q ;                                                   00046800
              WRITE (PRINT,FRM4,OUT5) ;                                 00046900
              N ~ L                                                     00047000
         END ;                                                          00047100
              IF VAR THEN                                               00047200
         BEGIN                                                          00047300
              FOR I ~ 1 STEP 1 UNTIL N DO                               00047400
              X[I] ~ A[I,0] / (M-1) ;                                   00047500
              FOR I ~ N+1 STEP 1 UNTIL Q DO                             00047600
              X[I] ~ YSQ[I-N] / (M-1) ;                                 00047700
              WRITE (PRINT,FRM5) ;                                      00047800
              L ~ N ;                                                   00047900
              N ~ Q ;                                                   00048000
              WRITE (PRINT,FRM4,OUT5) ;                                 00048100
              N ~ L                                                     00048200
         END ;                                                          00048300
COMMENT                                                                 00048400
              INVERT (N, A) ;                                           00048500
               FOR II ~ 1 STEP 1 UNTIL N DO                             00048600
               FOR JJ ~ 1 STEP 1 UNTIL N DO                             00048700
               SAM[II,JJ] ~ A[II,JJ];                                   00048800
               INVPDS(N,SAM,DIAGONAL);                                  00048900
               FOR II ~ 1 STEP 1 UNTIL N DO                             00049000
               FOR JJ ~ 1 STEP 1 UNTIL N DO                             00049100
               A[II,JJ] ~ SAM[II,JJ];                                   00049200
COMMENT       PRINTOUT OF INVERSE MATRIX ;                              00049300
              IF INV THEN                                               00049400
         BEGIN                                                          00049500
              WRITE (PRINT,FRM6) ;                                      00049600
              FOR I ~ 1 STEP 1 UNTIL N DO                               00049700
              WRITE (PRINT,FRM2,OUT2) ;                                 00049800
COMMENT       PRODUCT OF NORMAL AND INVERSE MATRICES ;                  00049900
              WRITE (PRINT,FRM7) ;                                      00050000
              FOR I ~ 1 STEP 1 UNTIL N DO                               00050100
         BEGIN                                                          00050200
              FOR J ~ I STEP 1 UNTIL N DO                               00050300
         BEGIN                                                          00050400
              X[J] ~ 0 ;                                                00050500
              FOR L ~ 1 STEP 1 UNTIL I-1 DO                             00050600
              X[J] ~ X[J] + A[L,I] | A[J,L] ;                           00050700
              FOR L ~ I STEP 1 UNTIL N DO                               00050800
         BEGIN                                                          00050900
              IF L < J THEN                                             00051000
              S ~ A[J,L]                                                00051100
              ELSE IF L > J THEN                                        00051200
              S ~ A[L,J]                                                00051300
              ELSE                                                      00051400
              S ~ A[J,0] ;                                              00051500
              X[J] ~ X[J] + S | A[I,L]                                  00051600
         END                                                            00051700
         END ;                                                          00051800
              WRITE (PRINT,FRM2,OUT6)                                   00051900
         END                                                            00052000
         END ;                                                          00052100
              FOR I ~ 1 STEP 1 UNTIL N DO                               00052200
         BEGIN                                                          00052300
              R ~ A[I,0] ;                                              00052400
              FOR J ~ I+1 STEP 1 UNTIL N DO                             00052500
               IF SQRT(R|A[J,0])!0 THEN                                 00052600
              A[J,I] ~ A[J,I] / SQRT (R | A[J,0])                       00052700
               ELSE A[J,I]~0;                                           00052800
         END ;                                                          00052900
COMMENT       CONSIDER EACH DEPENDENT VARIABLE SEPARATELY ;             00053000
              FOR P ~ N+1 STEP 1 UNTIL Q DO                             00053100
         BEGIN                                                          00053200
COMMENT       COMPUTATION OF REGRESSION COEFFICIENTS ;                  00053300
              FOR I ~ 1 STEP 1 UNTIL N DO                               00053400
         BEGIN                                                          00053500
              B[I] ~ 0 ;                                                00053600
              FOR L ~ 1 STEP 1 UNTIL I-1 DO                             00053700
              B[I] ~ B[I] + A[L,I] | A[L,P] ;                           00053800
              FOR L ~ I STEP 1 UNTIL N DO                               00053900
              B[I] ~ B[I] + A[I,L] | A[L,P];                            00054000
         END ;                                                          00054100
              B[0] ~ A[0,P] ;                                           00054200
              FOR I ~ 1 STEP 1 UNTIL N DO                               00054300
              B[0] ~ B[0] - B[I] | A[0,I] ;                             00054400
COMMENT       COMPUTATION OF REGRESSION SUM OF SQUARES ;                00054500
              RSS ~ 0 ;                                                 00054600
              FOR I ~ 1 STEP 1 UNTIL N DO                               00054700
              RSS ~ RSS + B[I] | A[I,P] ;                               00054800
              RMS ~ RSS / N ;                                           00054900
COMMENT       COMPUTATION OF ERROR SUM SQUARE ;                         00055000
              C ~ YSQ[P-N] ;                                            00055100
              ERS ~ C - RSS ;                                           00055200
COMMENT       COMPUTATION OF ERROR MEAN SQUARE AND                      00055300
              STANDARD ERROR OF ESTIMATE ;                              00055400
              K ~ M-1 ;                                                 00055500
              L ~ K-N ;                                                 00055600
              EMS ~ ERS / L ;                                           00055700
              SE ~ SQRT (EMS) ;                                         00055800
COMMENT       COMPUTATION OF COEFFICIENT OF DETERMINATION ;             00055900
              CD ~ RSS / C ;                                            00056000
COMMENT       MULTIPLE CORRELATION COEFFICIENT ;                        00056100
              MCC ~ SQRT (CD) ;                                         00056200
COMMENT       COMPUTATION OF STANDARD DEVIATIONS AND T                  00056300
              VALUES OF REGRESSION COEFFICIENTS ;                       00056400
              FOR I ~ 1 STEP 1 UNTIL N DO                               00056500
         BEGIN                                                          00056600
              S ~ SQRT (EMS | A[I,I]) ;                                 00056700
              SB[I] ~ S ;                                               00056800
              X[I] ~ B[I] / S                                           00056900
         END ;                                                          00057000
              I ~ P-N ;                                                 00057100
              WRITE (PRINT,FRM8,I) ;                                    00057200
              WRITE (PRINT,FRM9,B[0]) ;                                 00057300
              WRITE (PRINT,FRM4,OUT3) ;                                 00057400
              WRITE (PRINT,FRM10,ERS,EMS,SE,CD,MCC) ;                   00057500
              WRITE (PRINT,FRM11) ;                                     00057600
              WRITE (PRINT,FRM4,OUT4) ;                                 00057700
              WRITE (PRINT,FRM12) ;                                     00057800
              WRITE (PRINT,FRM4,OUT5) ;                                 00057900
COMMENT       ANALYSIS OF VARIANCE ;                                    00058000
              F ~ RMS / EMS ;                                           00058100
              WRITE (PRINT,FRM13,N,RSS,RMS,F,L,ERS,EMS,K,C);            00058200
COMMENT       COMPUTATION OF RESIDUES ;                                 00058300
              IF RES THEN                                               00058400
         BEGIN                                                          00058500
              REWIND(TAPEA) ;                                           00058600
              SWICH ~ TRUE ;                                            00058700
              WRITE (PRINT,FRM14) ;                                     00058800
              HI ~ -0.9@+49 ;                                           00058900
              LO ~  0.9@+49 ;                                           00059000
              GO TO RERED ;                                             00059100
RESDU:        CD ~ HI - LO ;                                            00059200
              F ~ CD / SE ;                                             00059300
              WRITE (PRINT,FRM16,CD,F)                                  00059400
         END                                                            00059500
         END ;                                                          00059600
              IF PCC THEN                                               00059700
         BEGIN                                                          00059800
COMMENT       CONSIDER EACH DEPENDENT VARIABLE SEPARATELY ;             00059900
              FOR P ~ N+1 STEP 1 UNTIL Q DO                             00060000
         BEGIN                                                          00060100
COMMENT       GET AUGMENTED CORRELATION COEFFICIENT MATRIX ;            00060200
              L ~ P-N ;                                                 00060300
              FOR I ~ 1 STEP 1 UNTIL N+1 DO                             00060400
              A[I,I] ~ 1 ;                                              00060500
              FOR K ~ 1 STEP 1 UNTIL N DO                               00060600
         BEGIN                                                          00060700
              FOR J ~ K+1 STEP 1 UNTIL N DO                             00060800
              A[K,J] ~ A[J,K] ;                                         00060900
              A[K,N+1] ~ A[K,N+1] / SQRT (A[K,0] | YSQ[L])              00061000
         END ;                                                          00061100
              WRITE (PRINT,FRM8,L) ;                                    00061200
              N ~ N+1 ;                                                 00061300
              IF CC THEN                                                00061400
         BEGIN                                                          00061500
              WRITE (PRINT,FRM17) ;                                     00061600
              FOR I ~ 1 STEP 1 UNTIL N DO                               00061700
              WRITE (PRINT,FRM2,OUT2)                                   00061800
         END ;                                                          00061900
              INVERT (N, A) ;                                           00062000
              IF CCINV THEN                                             00062100
         BEGIN                                                          00062200
              WRITE (PRINT,FRM18) ;                                     00062300
              FOR I ~ 1 STEP 1 UNTIL N DO                               00062400
              WRITE (PRINT,FRM2,OUT2)                                   00062500
         END ;                                                          00062600
COMMENT       DETERMINE PARTIAL CORRELATION COEFFICIENTS ;              00062700
              R ~ A[N,N] ;                                              00062800
              N ~ N-1 ;                                                 00062900
              FOR J ~ 1 STEP 1 UNTIL N DO                               00063000
              X[J] ~ -A[J,N+1] / SQRT (R | A[J,J]) ;                    00063100
              WRITE (PRINT,FRM19) ;                                     00063200
              WRITE (PRINT,FRM4,OUT5) ;                                 00063300
COMMENT       MOVE COLUMN FOR NEXT DEPENDENT VARIABLE ;                 00063400
              IF P+1 { Q THEN                                           00063500
         BEGIN                                                          00063600
              FOR J ~ 1 STEP 1 UNTIL N+1 DO                             00063700
              A[J,N+1] ~ A[J,P+1]                                       00063800
         END                                                            00063900
         END                                                            00064000
         END                                                            00064100
         END ;                                                          00064200
               IF INP>0 THEN                                            00064300
               IF SAVET THEN CLOSE(TIN[INP],RELEASE) ELSE               00064400
               CLOSE(TIN[INP],PURGE);                                   00064500
NOMO:    END  PROGRAM.                                                  00064600
                LAST CARD ON CRDIMG TAPE                                99999999
