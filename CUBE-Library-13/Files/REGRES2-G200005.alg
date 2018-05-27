          BEGIN                                                         00000000
COMMENT        REGRES2/STATMAN;                                         00000100
COMMENT        STEPWISE MULTIPLE REGRESSION;                            00000200
FILE IN        CARD (2,10);                                             00000300
INTEGER        I,NOVA,NSAM,NAD,INP;                                     00000400
BOOLEAN        WT,STAP,SSQ,MEAN,RESD,COEF,PRED,CNST,TR,SAVET;           00000500
INTEGER ARRAY  ID[0:4];                                                 00000600
REAL           TOL,FIN,FOUT,CONS;                                       00000700
LABEL          START,NOMO,LL1;                                          00000800
FORMAT       DATAFMT(/////////////////////////////////////////////////  00000900
                     /////////////////////////////////////////////////  00001000
                     /////////////////////////////////////////////////);00001100
FORMAT IN FORM1(5A6,3F10.5,2I5,2L5/7L5,I2,I1,L5),                       00001200
               FORM2(40I2),                                             00001300
               FORM3(26I3),                                             00001400
               FORM4(                                                   00001500
               12F6.1                                                   00001600
               );                                                       00001700
FORMAT OUT     TITLE(X41,"STEPWISE MULTIPLE REGRESSION ANALYSIS"///),   00001800
               FRM2(X45,5A6//X1,"NO. OF DATA =",I5/                     00001900
                   X1,"NO, OF VARIABLES =",I5/                          00002000
                   X1,"WEIGHTED DEGREES OF FREEDOM =",F12.2/            00002100
                   X1,"F LEVEL TO ENTER VARIABLE =",F10.3/              00002200
                   X1,"F LEVEL TO REMOVE VARIABLE =",F9.3//),           00002300
               FRM3(//X1,"SUM OF VARIABLES"//                           00002400
                   (X6,"SUM X(",I2,") =",F12.4)),                       00002500
               FRM4(X6,"SUM   Y   =",F12.4///X1,"SUM OF SQUARES AND ",  00002600
                   "CROSS PRODUCTS"//),                                 00002700
               FRM5(X6,"X(",I2,") VS X(",I2,") =",F15.6),               00002800
               FRM6(X6,"X(",I2,") VS   Y   =",F15.6),                   00002900
               FRM7(X6,"  Y   VS   Y   =",F15.6),                       00003000
               FRM8(//X1,"MEAN VALUE OF VARIABLES"//                    00003100
                   (X5,"MEAN X(",I2,") =",F12.4)),                      00003200
               FRM9(X5,"MEAN   Y   =",F12.4//),                         00003300
              FRM10(X1,"RESIDUALS SUMS OF SQUARES AND CROSS PRODUCTS"/),00003400
               FRM11(X1,"VARIABLE",I4," IS CONSTANT"/),                 00003500
               FRM12(//X1,"CORRELATION COEFFICIENTS"/),                 00003600
               FRM13(//X1,"SQUARE NON-POSITIVE,END STEP",I4/),          00003700
               FRM14(//X1,"NO MORE DEGREES OF FREEDOM, STEP ",I4/),     00003800
               FRM15(//X1,"SQUARE OF X",I4," IS NEGATIVE, END OF STEP", 00003900
                   I4/),                                                00004000
               FRM16(//X2,"STANDARD ERROR OF Y = ",F12.6/),             00004100
               FRM17(//"STEP NO.",I4/X1,"VARIABLE REMOVED",I5),         00004200
               FRM18(//"STEP NO.",I4/X1,"VARIABLE ENTERING",I5),        00004300
              FRM19(X1,"F LEVEL ",F12.4/X1,"STANDARD ERROR OF ESTIMATE",00004400
                   X1,F12.4/X1,"CONSTANT ",F13.5/X1,"VARIABLE",X3,      00004500
                   "COEFFICIENT",X3,"STD ERROR OF COEF"//               00004600
                   ((X3,"X",I3,X5,F10.5,X5,F10.5)/)),                   00004700
               FRM20(//"STEPS OF REGRESSION COMPLETED",I5/),            00004800
               FRM21(//"DIAGONAL ELEMENTS OF INVERSE OF CORRELATION ",  00004900
                   "MATRIX"//X4,"VAR. NO.",X5,"VALUE"//(I7,F16.6)/),    00005000
               FRM22(///X19,"PREDICTED VS ACTUAL VALUES"/X10,"RUN NO.", 00005100
                   X4,"ACTUAL",X7,"PREDICTED",X5,"DEVIATION"/),         00005200
               FRM23(X8,I6,X2,F12.5,X3,F12.5,X2,F12.5);                 00005300
FORMAT         OKTL(100O);                                              00005400
FILE OUT       PRINT 4(2,15);                                           00005500
DEFINE         LINE =PRINT#;                                            00005600
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00005700
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00005800
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00005900
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00006000
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00006100
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00006200
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00006300
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00006400
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00006500
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00006600
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00006700
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00006800
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00006900
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00007000
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00007100
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00007200
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00007300
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00007400
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300007500
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00007600
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00007700
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00007800
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00007900
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00008000
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00008100
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00008200
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00008300
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100008400
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00008500
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00008600
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00008700
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00008800
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00008900
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00009000
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00009100
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00009200
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00009300
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00009400
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00009500
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00009600
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00009700
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00009800
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00009900
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00010000
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00010100
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00010200
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00010300
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00010400
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00010500
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00010600
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00010700
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00010800
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00010900
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00011000
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00011100
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800011200
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00011300
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00011400
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00011500
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00011600
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00011700
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00011800
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00011900
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00012000
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00012100
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00012200
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00012300
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00012400
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00012500
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00012600
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00012700
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00012800
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00012900
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00013000
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00013100
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00013200
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00013300
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00013400
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00013500
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00013600
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00013700
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00013800
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00013900
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00014000
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00014100
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00014200
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00014300
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00014400
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00014500
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00014600
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00014700
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00014800
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00014900
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100015000
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00015100
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00015200
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00015300
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00015400
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00015500
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000015600
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00015700
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00015800
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00015900
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00016000
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00016100
START:    BEGIN                                                         00016200
LIST           HEAD(FOR I~0 STEP 1 UNTIL 4 DO ID[I],TOL,FIN,FOUT,NOVA,  00016300
               NSAM,WT,STAP,SSQ,MEAN,RESD,COEF,PRED,CNST,TR,NAD,INP,    00016400
               SAVET);                                                  00016500
               READ(CARD,FORM1,HEAD)[NOMO];                             00016600
               WRITE(PRINT[PAGE]);                                      00016700
               WRITE(PRINT,TITLE);                                      00016800
          END;                                                          00016900
LL1:      BEGIN                                                         00017000
INTEGER        NIN,K,NENT,NMIN,NMAX,NVP1,MARY,NEWV,NVA1;                00017100
REAL           VAR,FLEV;                                                00017200
LABEL          LL2;                                                     00017300
               K ~ 0;                                                   00017400
               NENT ~ 0 ;                                               00017500
               NMIN ~ 0;                                                00017600
               NMAX ~ 0;                                                00017700
               NVA1 ~ NOVA + 1;                                         00017800
               NVP1 ~ NOVA + 1 + NAD;                                   00017900
               MARY ~ NOVA + 1 + ABS(NAD);                              00018000
               NEWV ~ NOVA + NAD;                                       00018100
LL2:      BEGIN                                                         00018200
INTEGER        N,L,J,VMI,VPL,NSTEP,IP1,VM2,STM1;                        00018300
BOOLEAN        SWICH;                                                   00018400
INTEGER ARRAY  T[0:99],SC[0:NVA1],INDX[0:NVP1];                         00018500
REAL           W,DF,SIGY,VMIN,VMAX,YPRE,DEV;                            00018600
REAL ARRAY    VEC[0:NVP1,0:NVP1],DATA[0:MARY],X[0:MARY],AVE[0:NVP1];    00018700
REAL ARRAY     SIG[0:NVP1],COEN[0:NVP1],SIGC[0:NVP1];                   00018800
LABEL          LL6,LL8,LL9,LL10,LL11,LL12,LL13,LL14,LL15,LL16,NAXT,     00018900
               L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15;      00019000
LIST           SCAL(FOR I ~ 1 STEP 1 UNTIL NVA1 DO SC[I]),              00019100
               OUTT(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NSAM,NOVA,        00019200
                   VEC[VPL,VPL],FIN,FOUT),                              00019300
               TRAN(FOR J ~ 1 STEP 1 UNTIL NEWV DO T[J]),               00019400
               DATR(FOR L ~ 1 STEP 1 UNTIL NOVA DO DATA[L]),            00019500
               DATE(FOR L ~ 1 STEP 1 UNTIL NOVA DO DATA[L],W),          00019600
               SUMX(FOR I ~ 1 STEP 1 UNTIL  VMI DO [I,VEC[I,VPL]]),     00019700
               SSQX(FOR J ~ I STEP 1 UNTIL VMI DO [I,J,VEC[I,J]]),      00019800
               SSYX(FOR I ~ 1 STEP 1 UNTIL VMI DO [I,VEC[I,NOVA]]),     00019900
               AVEX(FOR I ~ 1 STEP 1 UNTIL VMI DO [I,AVE[I]]),          00020000
               CORR(FOR J ~ IP1 STEP 1 UNTIL VMI DO [I,J,VEC[I,J]]),    00020100
               KOEF(FLEV,SIGY,CONS,FOR J ~ 1 STEP 1 UNTIL NIN DO        00020200
                   [INDX[J],COEN[J],SIGC[J]]),                          00020300
               DIAG(FOR L ~ 1 STEP 1 UNTIL VMI DO [L,VEC[L,L]]);        00020400
SWITCH         U ~ L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15;  00020500
FILE           TAPEB (2,NEWV+1);                                        00020600
FILE IN        TAPE1 (2,100),                                           00020700
               TAPE2 (2,100),                                           00020800
               TAPE3 (2,100),                                           00020900
               TAPE4 (2,100),                                           00021000
               CORMAT (2,100),                                          00021100
               FACMAT (2,100),                                          00021200
               ROTMAT (2,100);                                          00021300
SWITCH FILE    TFN~CARD,TAPE1,TAPE2,TAPE3,TAPE4,CORMAT,FACMAT,ROTMAT;   00021400
               FOR I ~ 1 STEP 1 UNTIL NVP1 DO                           00021500
               FOR J ~ 1 STEP 1 UNTIL NVP1 DO                           00021600
               VEC[I,J] ~ 0.0;                                          00021700
               READ(CARD,FORM2,SCAL);                                   00021800
               IF TR THEN                                               00021900
               READ(CARD,FORM3,TRAN);                                   00022000
               IF INP=0 THEN                                            00022100
               OBJECTFMTGEN(DATAFMT)                                    00022200
               ELSE READ(TFN[INP],OKTL,NSAM,NOVA);                      00022300
               WRITE(LINE[DBL]);                                        00022400
               FOR N ~ 1 STEP 1 UNTIL NSAM DO                           00022500
          BEGIN                                                         00022600
               IF INP=0 THEN                                            00022700
               IF WT THEN                                               00022800
               READ(CARD,DATAFMT,DATE) ELSE                             00022900
               READ(CARD,DATAFMT,DATR)                                  00023000
               ELSE                                                     00023100
               IF WT THEN                                               00023200
               READ(TFN[INP],OKTL,DATE) ELSE                            00023300
               READ(TFN[INP],OKTL,DATR);                                00023400
               FOR J ~ 1 STEP 1 UNTIL NOVA  DO                          00023500
          BEGIN                                                         00023600
               X[J] ~ DATA[J] | 10.0*SC[J];                             00023700
               DATA[J] ~ X[J];                                          00023800
          END;                                                          00023900
COMMENT        TRANSFORMATIONS;                                         00024000
               IF TR THEN                                               00024100
          BEGIN                                                         00024200
               FOR J ~ NEWV STEP -1 UNTIL 1 DO                          00024300
          BEGIN                                                         00024400
               GO TO U[T[J]];                                           00024500
L1:            DATA[J] ~ X[J];                                          00024600
               GO TO NAXT;                                              00024700
L2:            DATA[J] ~ X[J]|X[J];                                     00024800
               GO TO NAXT;                                              00024900
L3:            DATA[J] ~ 1 / X[J];                                      00025000
               GO TO NAXT;                                              00025100
L4:            DATA[J] ~ SQRT(X[J]);                                    00025200
               GO TO NAXT;                                              00025300
L5:            DATA[J] ~ EXP(X[J]);                                     00025400
               GO TO NAXT;                                              00025500
L6:            DATA[J] ~ LN(X[J]);                                      00025600
               GO TO NAXT;                                              00025700
L7:            DATA[J] ~ SIN(X[J]);                                     00025800
               GO TO NAXT;                                              00025900
L8:            DATA[J] ~ COS(X[J]);                                     00026000
               GO TO NAXT;                                              00026100
L9:            DATA[J] ~ X[J+1];                                        00026200
               GO TO NAXT;                                              00026300
L10:           DATA[J] ~ X[J+2];                                        00026400
               GO TO NAXT;                                              00026500
L11:           DATA[J] ~ X[J+3];                                        00026600
               GO TO NAXT;                                              00026700
L12:           DATA[J]~ X[J+4];                                         00026800
               GO TO NAXT;                                              00026900
L13:           DATA[J] ~ X[J+5];                                        00027000
               GO TO NAXT;                                              00027100
L14:           DATA[J] ~ X[J+6];                                        00027200
               GO TO NAXT;                                              00027300
L15:           DATA[J] ~ X[J+7];                                        00027400
               GO TO NAXT;                                              00027500
NAXT:     END;                                                          00027600
          END;                                                          00027700
               IF WT THEN                                               00027800
               W ~ W|10.0*SC[NVA1];                                     00027900
               WRITE(TAPEB,NEWV+1,DATA[*]);                             00028000
               FOR I ~ 1 STEP 1 UNTIL NEWV DO                           00028100
          BEGIN                                                         00028200
               L ~ NEWV +1;                                             00028300
               IF WT THEN                                               00028400
               DF ~ DATA[I]|W                                           00028500
               ELSE    DF ~ DATA[I];                                    00028600
               VEC[I,L] ~ VEC[I,L] + DF;                                00028700
               FOR J ~ I STEP 1 UNTIL NEWV DO                           00028800
               VEC[I,J] ~ VEC[I,J] + DF|DATA[J];                        00028900
          END;                                                          00029000
               IF NOT WT THEN                                           00029100
               W ~ 1.0;                                                 00029200
               VEC[NVP1,NVP1] ~ VEC[NVP1,NVP1] + W;                     00029300
          END;                                                          00029400
               NOVA ~ NEWV;                                             00029500
               VMI ~ NOVA -1;                                           00029600
               VPL ~ NOVA + 1;                                          00029700
               WRITE(PRINT,FRM2,OUTT);                                  00029800
               IF SSQ THEN                                              00029900
          BEGIN                                                         00030000
               WRITE(PRINT,FRM3,SUMX);                                  00030100
               WRITE(PRINT,FRM4,VEC[NOVA,VPL]);                         00030200
               FOR I ~ 1 STEP 1 UNTIL VMI DO                            00030300
               WRITE(PRINT,FRM5,SSQX);                                  00030400
               WRITE(PRINT,FRM6,SSYX);                                  00030500
               WRITE(PRINT,FRM7,VEC[NOVA,NOVA]);                        00030600
           END;                                                         00030700
               IF CNST THEN                                             00030800
               FOR I ~ 1 STEP 1 UNTIL NOVA DO                           00030900
               FOR J ~ 1 STEP 1 UNTIL NOVA DO                           00031000
               VEC[I,J] ~ VEC[I,J]-(VEC[I,VPL]|VEC[J,VPL]               00031100
               /VEC[VPL,VPL]);                                          00031200
               FOR I ~ 1 STEP 1 UNTIL NOVA DO                           00031300
               AVE[I] ~ VEC[I,VPL]/VEC[VPL,VPL];                        00031400
               IF MEAN THEN                                             00031500
          BEGIN                                                         00031600
               WRITE(PRINT,FRM8,AVEX);                                  00031700
               WRITE(PRINT,FRM9,AVE[NOVA]);                             00031800
          END;                                                          00031900
               IF RESD THEN                                             00032000
          BEGIN                                                         00032100
               WRITE(PRINT,FRM10);                                      00032200
               FOR I ~ 1 STEP 1 UNTIL VMI DO                            00032300
               WRITE (PRINT,FRM5,SSQX);                                 00032400
               WRITE (PRINT,FRM6,SSYX);                                 00032500
               WRITE (PRINT,FRM7,VEC[NOVA,NOVA]);                       00032600
            END;                                                        00032700
               NSTEP ~ -1;                                              00032800
               SWICH ~ FALSE;                                           00032900
               DF ~ VEC[VPL,VPL] - 1.0;                                 00033000
               FOR I ~ 1 STEP 1 UNTIL NOVA DO                           00033100
          BEGIN                                                         00033200
               IF VEC[I,I]= 0 THEN                                      00033300
          BEGIN                                                         00033400
               WRITE(PRINT,FRM11,I);                                    00033500
               SIG[I] ~ 1.0                                             00033600
          END                                                           00033700
               ELSE    SIG[I] ~ SQRT(VEC[I,I]);                         00033800
               VEC[I,I] ~ 1.0;                                          00033900
          END;                                                          00034000
               FOR I ~ 1 STEP 1 UNTIL VMI DO                            00034100
          BEGIN                                                         00034200
               IP1 ~ I+1;                                               00034300
               FOR J ~ IP1 STEP 1 UNTIL NOVA DO                         00034400
          BEGIN                                                         00034500
               VEC[I,J] ~ VEC[I,J]/(SIG[I]|SIG[J]);                     00034600
               VEC[J,I] ~ VEC[I,J];                                     00034700
          END;                                                          00034800
          END;                                                          00034900
               IF COEF THEN                                             00035000
          BEGIN                                                         00035100
               WRITE(PRINT,FRM12);                                      00035200
               VM2 ~ VMI-1;                                             00035300
               FOR I ~ 1 STEP 1 UNTIL VM2 DO                            00035400
          BEGIN                                                         00035500
               IP1 ~ I + 1;                                             00035600
               WRITE(PRINT,FRM5,CORR);                                  00035700
          END;                                                          00035800
               WRITE(PRINT,FRM6,SSYX);                                  00035900
          END;                                                          00036000
COMMENT        COMPUTE NEXT REGRESSION EQUATION;                        00036100
LL12:          NSTEP ~ NSTEP +1;                                        00036200
               IF VEC[NOVA,NOVA] { 0 THEN                               00036300
          BEGIN                                                         00036400
               STM1~NSTEP-1;                                            00036500
               WRITE(PRINT,FRM13,STM1);                                 00036600
               GO TO LL14;                                              00036700
          END;                                                          00036800
               SIGY ~ SIG[NOVA] | SQRT(VEC[NOVA,NOVA] / DF);            00036900
               DF ~ DF - 1.0;                                           00037000
               IF DF { 0 THEN                                           00037100
          BEGIN                                                         00037200
               WRITE(PRINT,FRM14,NSTEP);                                00037300
               GO TO LL14;                                              00037400
          END;                                                          00037500
               VMIN ~ 0.0;                                              00037600
               VMAX ~ 0.0;                                              00037700
               NIN ~ 0;                                                 00037800
               FOR I ~ 1 STEP 1 UNTIL VMI DO                            00037900
          BEGIN                                                         00038000
               IF VEC[I,I] < 0 THEN                                     00038100
          BEGIN                                                         00038200
               WRITE(PRINT,FRM15,NSTEP);                                00038300
               GO TO LL14                                               00038400
          END                                                           00038500
               ELSE   IF VEC[I,I] = 0 THEN                              00038600
               GO TO LL8                                                00038700
               ELSE  IF VEC[I,I] - TOL < 0 THEN                         00038800
               GO TO LL8                                                00038900
               ELSE                                                     00039000
               VAR ~ VEC[I,NOVA] | VEC[NOVA,I] / VEC[I,I];              00039100
               IF VAR < 0 THEN                                          00039200
          BEGIN                                                         00039300
               NIN ~ NIN + 1;                                           00039400
               INDX[NIN] ~ I;                                           00039500
               COEN[NIN] ~ VEC[I,NOVA] | SIG[NOVA]/SIG[I];              00039600
              SIGC[NIN] ~ (SIGY/ SIG[I])| SQRT(VEC[I,I]);               00039700
COMMENT        CHOOSE NEXT ENTERING VARIABLE AND LEAVING VARIABLE;      00039800
               IF VMIN > 0 THEN                                         00039900
          BEGIN                                                         00040000
               REWIND (TAPEB);                                          00040100
               GO TO START;                                             00040200
          END                                                           00040300
               ELSE IF VMIN = 0 THEN                                    00040400
          BEGIN                                                         00040500
LL6:           VMIN ~ VAR;                                              00040600
               NMIN ~ I;                                                00040700
               GO TO LL8                                                00040800
          END                                                           00040900
               ELSE IF VAR - VMIN > 0 THEN                              00041000
               GO TO LL6                                                00041100
               ELSE                                                     00041200
               GO TO LL8                                                00041300
          END                                                           00041400
               ELSE IF VAR =0 THEN                                      00041500
               GO TO LL8                                                00041600
               ELSE                                                     00041700
          BEGIN                                                         00041800
               IF VAR - VMAX { 0 THEN                                   00041900
               GO TO LL8                                                00042000
               ELSE                                                     00042100
          BEGIN                                                         00042200
               VMAX ~ VAR;                                              00042300
               NMAX ~ I;                                                00042400
          END                                                           00042500
          END;                                                          00042600
LL8:      END;                                                          00042700
               IF NIN < 0 THEN                                          00042800
          BEGIN                                                         00042900
               REWIND(TAPEB);                                           00043000
               GO TO START                                              00043100
          END                                                           00043200
               ELSE IF NIN = 0 THEN                                     00043300
          BEGIN                                                         00043400
               WRITE(PRINT,FRM16,SIGY);                                 00043500
               GO TO LL10                                               00043600
          END                                                           00043700
               ELSE IF CNST THEN                                        00043800
               CONS ~ AVE[NOVA]                                         00043900
               ELSE                                                     00044000
          BEGIN                                                         00044100
               CONS ~ 0.0;                                              00044200
               GO TO LL9                                                00044300
          END;                                                          00044400
               FOR I ~ 1 STEP 1 UNTIL NIN DO                            00044500
          BEGIN                                                         00044600
               J ~ INDX[I];                                             00044700
               CONS ~ CONS-(COEN[I] | AVE[J])                           00044800
          END;                                                          00044900
LL9:           IF STAP THEN                                             00045000
          BEGIN                                                         00045100
               IF NENT { 0 THEN                                         00045200
               WRITE(PRINT,FRM17,NSTEP,K)                               00045300
               ELSE                                                     00045400
               WRITE(PRINT,FRM18,NSTEP,K);                              00045500
               WRITE(PRINT,FRM19,KOEF)                                  00045600
          END;                                                          00045700
COMMENT        TESTS TO REMOVE OR ENTER VARIABLE;                       00045800
               IF SWICH THEN                                            00045900
               GO TO LL15;                                              00046000
               FLEV ~ VMIN | DF / VEC[NOVA,NOVA];                       00046100
               IF FOUT + FLEV > 0 THEN                                  00046200
          BEGIN                                                         00046300
               K ~ NMIN;                                                00046400
               NENT ~ 0;                                                00046500
               GO TO LL16;                                              00046600
          END;                                                          00046700
LL10:          FLEV ~ VMAX | DF / (VEC[NOVA,NOVA] - VMAX);              00046800
               IF (FIN> FLEV) OR ((FIN = FLEV) AND (FIN { 0)) THEN      00046900
               GO TO LL13;                                              00047000
               K ~ NMAX;                                                00047100
               NENT ~ K;                                                00047200
COMMENT        COMPUTE NEXT MATRIX;                                     00047300
LL16:          IF K { 0 THEN                                            00047400
          BEGIN                                                         00047500
               REWIND(TAPEB);                                           00047600
               GO TO START;                                             00047700
          END;                                                          00047800
               FOR I ~ 1 STEP 1 UNTIL NOVA DO                           00047900
          BEGIN                                                         00048000
               IF I = K THEN                                            00048100
               GO TO LL11;                                              00048200
               FOR J ~ 1 STEP 1 UNTIL NOVA DO                           00048300
          BEGIN                                                         00048400
               IF J ! K THEN                                            00048500
               VEC[I,J] ~ VEC[I,J] - VEC[I,K] | VEC[K,J] / VEC[K,K]     00048600
          END;                                                          00048700
LL11:     END;                                                          00048800
               FOR I ~ 1 STEP 1 UNTIL NOVA DO                           00048900
          BEGIN                                                         00049000
               IF I ! K THEN                                            00049100
               VEC[I,K] ~ - VEC[I,K] / VEC[K,K]                         00049200
          END;                                                          00049300
               FOR J ~ 1 STEP 1 UNTIL NOVA DO                           00049400
          BEGIN                                                         00049500
               IF J ! K THEN                                            00049600
               VEC[K,J] ~ VEC[K,J] / VEC[K,K]                           00049700
          END;                                                          00049800
               VEC[K,K] ~ 1.0 / VEC[K,K];                               00049900
               GO TO LL12;                                              00050000
LL13:          WRITE(PRINT,FRM20,NSTEP);                                00050100
LL14:          IF STAP THEN                                             00050200
               GO TO LL15;                                              00050300
               SWICH ~ TRUE;                                            00050400
               GO TO LL9;                                               00050500
LL15:          WRITE(PRINT,FRM21,DIAG);                                 00050600
               IF PRED THEN                                             00050700
          BEGIN                                                         00050800
               REWIND(TAPEB);                                           00050900
               WRITE(PRINT,FRM22);                                      00051000
               FOR N ~ 1 STEP 1 UNTIL NSAM DO                           00051100
          BEGIN                                                         00051200
               READ(TAPEB,NOVA+1,DATA[*]);                              00051300
               YPRE ~ CONS;                                             00051400
               FOR I ~ 1 STEP 1 UNTIL NIN DO                            00051500
          BEGIN                                                         00051600
               K~ INDX[I];                                              00051700
               YPRE ~ YPRE + COEN[I] | DATA[K];                         00051800
          END;                                                          00051900
               DEV ~ DATA[NOVA] - YPRE;                                 00052000
               WRITE(PRINT,FRM23,N,DATA[NOVA],YPRE,DEV)                 00052100
          END                                                           00052200
          END;                                                          00052300
               REWIND(TAPEB);                                           00052400
               IF INP>0 THEN                                            00052500
               IF SAVET THEN CLOSE(TFN[INP],RELEASE) ELSE               00052600
               CLOSE(TFN[INP],PURGE);                                   00052700
          END;                                                          00052800
          END;                                                          00052900
NOMO:     END OF PROGRAM.                                               00053000
                LAST CARD ON CRDIMG TAPE                                99999999
