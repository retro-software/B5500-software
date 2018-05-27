         BEGIN                                                          00000100
COMMENT        PROGRAM - FACVAR,                                        00000200
               CUBE LIBRARY NUMBER IS T800001.                          00000300
               THIS VERSION DATED 5/31/67;                              00000400
COMMENT       FACTOR ANALYSIS                                           00000500
              CHARLES L. CLARK                                          00000600
              PROFESSIONAL SERVICES DIVISIONAL GROUP                    00000700
              BURROUGHS CORPORATION                                     00000800
              PROGRAM CONTAINS 712 CARDS                                00000900
              FIRST RELEASE DATE DEC. 1, 1964  ;                        00001000
     COMMENT   DATE LAST REVISED:  14 SEPTEMBER 1965                    00001100
               REVISED BY:         FORREST F. CARHART, III             ;00001200
COMMENT        DATE LAST REVISED DECEMBER 15,1966 BY J. HORN  ;         00001300
INTEGER       I,NVAR,NSAM,CAS,NRO ;                                     00001400
INTEGER        QZ,INP,COMM,PNCH,COVARPRNT                           ;   00001500
INTEGER        SFMT;                                                    00001600
INTEGER ARRAY ID[0:4] ;                                                 00001700
BOOLEAN       TR ;                                                      00001800
REAL          LIM ;                                                     00001900
LABEL         START,NOMO,LL1 ;                                          00002000
FORMAT IN      FORM1 (5A6,I3,I5,I3,F5.2,I3,L5,6I1),                     00002100
               FORM2 (40I2),                                            00002200
               FORM3(26I3);                                             00002300
FORMAT         CHKFOR (A6);                                             00002400
FORMAT         PUNCHOUT (      16F5.2/X5,15F5.2);                       00002500
FORMAT         NOFRM ("ERROR: NO FORMAT SPECIFICATIONS FOR THIS RUN"//);00002600
FORMAT         ERRCNT ("ERROR: WRONG N INDICATED ON HEADER CARD" //);   00002700
LABEL          WRONGCOUNT;                                              00002800
FORMAT         SWF (//////////////////////////////////////////////////  00002900
                    //////////////////////////////////////////////////  00003000
                    //////////////////////////////////////////////////  00003100
                    //////////////////////////////////////////////////);00003200
FORMAT         COF (//////////////////////////////////////////////////  00003300
                    //////////////////////////////////////////////////  00003400
                    //////////////////////////////////////////////////  00003500
                    //////////////////////////////////////////////////);00003600
COMMENT                                                                 00003700
              FORM4 (12F6.1) ;                                          00003800
FORMAT OUT    TITLE(X52,"FACTOR ANALYSIS"///),                          00003900
              FRM1 (X45,5A6//X1,"CASE",I3/X1,"NUMBER OF VARIABLES",I4/  00004000
                    X1,"SAMPLE SIZE",I6),                               00004100
              FRM2 (//X1,"MEAN VALUE OF VARIABLES"),                    00004200
              FRM3 (8F15.5),                                            00004300
              FRM4 (//X1,"STANDARD DEVIATIONS"),                        00004400
              FRM5 (//X1,"CORRELATION COEFFICIENTS"),                   00004500
               FRM52(//X1,"COVARIANCES"),                               00004600
              FRM6 (/X1,"ROW",I3/(10F12.5)/),                           00004700
              FRM7 (//X1,"EIGENVALUES"),                                00004800
              FRM8 (//X7,"ACCURACY INSUFFICIENT FOR MORE EIGENVALUES"), 00004900
              FRM9 (//X1,"CUMULATIVE PROPORTION OF TOTAL VARIANCE"),    00005000
              FRM10(//X1,"EIGENVECTORS"),                               00005100
              FRM11(/X1,"VECTOR",I3/(10F12.5)/),                        00005200
              FRM12(//X1,"FACTOR MATRIX"),                              00005300
              FRM13(/X1,"VARIABLE",I3/(10F12.5)/),                      00005400
              FRM14(//X1,"CHECK MATRIX - EIGENVALUES ON DIAGONAL"),     00005500
              FRM15(//X1,"CHECK MATRIX - CORRELATION COEFFICIENTS"),    00005600
              FRM16(//X1,"VALUE GIVEN TO LIMIT NUMBER OF FACTORS TO BE",00005700
                    " ROTATED",F9.4/),                                  00005800
              FRM17(//X1,"FACTOR MATRIX CAN NOT BE ROTATED, CHANGE THE",00005900
                    " LIMIT VALUE LIM"/),                               00006000
              FRM18(/X1,"NUMBER OF FACTORS ROTATED",I4/X1,"NUMBER OF ", 00006100
                    "ITERATION CYCLES",I3///X1,"ROTATED FACTOR MATRIX"),00006200
              FRM19(/X1,"ORIGINAL AND SUCCESSIVE VARIANCES"//X3,"CYCLE",00006300
                    " NO.",X10,"VARIANCES"),                            00006400
              FRM20(I8,X8,F15.7),                                       00006500
              FRM21(//X1,"CHECK ON COMMUNALITIES"//X3,"VARIABLES",X8,   00006600
                    "ORIGINAL",X9,"FINAL",X8,"DIFFERENCE"),             00006700
              FRM22(I8,X5,3F15.5) ;                                     00006800
FILE IN        CARD(2,10);                                              00006900
DEFINE         CARDR=CARD#;                                             00007000
FILE OUT       PRINT  4(2,15);                                          00007100
FILE OUT       PUNCH 0 (2,15);                                          00007200
DEFINE         LINE=PRINT#;                                             00007300
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00007400
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00007500
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00007600
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00007700
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00007800
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00007900
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00008000
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00008100
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00008200
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00008300
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00008400
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00008500
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00008600
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00008700
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00008800
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00008900
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00009000
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00009100
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300009200
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00009300
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00009400
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00009500
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00009600
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00009700
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00009800
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00009900
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00010000
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100010100
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00010200
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00010300
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00010400
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00010500
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00010600
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00010700
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00010800
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00010900
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00011000
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00011100
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00011200
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00011300
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00011400
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00011500
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00011600
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00011700
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00011800
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00011900
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00012000
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00012100
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00012200
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00012300
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00012400
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00012500
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00012600
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00012700
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00012800
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800012900
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00013000
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00013100
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00013200
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00013300
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00013400
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00013500
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00013600
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00013700
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00013800
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00013900
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00014000
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00014100
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00014200
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00014300
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00014400
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00014500
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00014600
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00014700
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00014800
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00014900
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00015000
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00015100
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00015200
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00015300
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00015400
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00015500
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00015600
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00015700
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00015800
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00015900
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00016000
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00016100
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00016200
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00016300
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00016400
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00016500
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00016600
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100016700
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00016800
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00016900
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00017000
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00017100
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00017200
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000017300
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00017400
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00017500
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00017600
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00017700
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00017800
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEA00017900
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",00018000
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"///),LAYT(//"EXECUTION TIM00018100
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///00018200
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=00018300
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAY00018400
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FE00018500
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.00018600
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];EN00018700
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THEN00018800
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,00018900
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       00019000
START:   BEGIN                                                          00019100
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NVAR,NSAM,CAS,     00019200
                   LIM,NRO,TR,SFMT,QZ,INP,COMM,PNCH,COVARPRNT);         00019300
              READ (CARDR,FORM1,HEAD) [NOMO] ;                          00019400
              WRITE(PRINT[PAGE]) ;                                      00019500
               DATELINE("FACVAR");                                      00019600
              WRITE(PRINT,TITLE);                                       00019700
               IF QZ < 1 THEN                                           00019800
          BEGIN                                                         00019900
               WRITE(LINE[DBL]);                                        00020000
               READ(CARD[NO], CHKFOR, SFMT)[NOMO];                      00020100
               IF SFMT ! "FORMAT"  THEN                                 00020200
          BEGIN                                                         00020300
               WRITE(LINE, NOFRM);                                      00020400
               GO TO NOMO;                                              00020500
          END  ELSE  OBJECTFMTGEN(SWF);                                 00020600
               WRITE(LINE[DBL]);                                        00020700
          END;                                                          00020800
         END ;                                                          00020900
LL1:     BEGIN                                                          00021000
INTEGER       EX ;                                                      00021100
REAL          NS,NSM1,RN ;                                              00021200
LABEL         LL2 ;                                                     00021300
              NS ~ NSAM ;                                               00021400
              NSM1 ~ NS - 1.0 ;                                         00021500
              EX ~ NVAR + 50 ;                                          00021600
              RN ~ 0.0 ;                                                00021700
LL2:     BEGIN                                                          00021800
INTEGER       J,JJ,K,KK,L,LL,II,M,MM,Z ;                                00021900
INTEGER ARRAY T[0:99],SC[0:NVAR] ;                                      00022000
REAL          NORM,T1,T2,SYN,KOS,P,TA,TE,BE,W,S,MARY,B ;                00022100
REAL ARRAY    A[0:NVAR,0:NVAR],DATA[0:EX],SCA[0:NVAR,0:NVAR],           00022200
              SUMXX[0:NVAR,0:NVAR],R[0:NVAR,0:NVAR],DIAG[0:NVAR],       00022300
              VAL[0:NVAR],SD[0:NVAR],X[0:NVAR],SUMX[0:NVAR] ;           00022400
LABEL          LCAS6,LCAS6A;                                            00022500
LABEL         NAXT,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,  00022600
              LL3,LL23,LL24,LL25,LL26,LL27,LL33,LL34,LL35 ;             00022700
LIST          SCAL(FOR I ~ 1 STEP 1 UNTIL NVAR DO SC[I]),               00022800
              TRAN(FOR J ~ 1 STEP 1 UNTIL NVAR DO T[J]),                00022900
              DATR(FOR L ~ 1 STEP 1 UNTIL NVAR DO DATA[L]),             00023000
              OUTT(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],CAS,NVAR,NSAM),    00023100
              AVEX(FOR I ~ 1 STEP 1 UNTIL NVAR DO SUMX[I]),             00023200
              SDEV(FOR K ~ 1 STEP 1 UNTIL NVAR DO A[K,1]),              00023300
              CORR(I,FOR J ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]),          00023400
               COVAR(I,FOR J ~ 1 STEP 1 UNTIL NVAR DO R[I,J]),          00023500
              EVAL(FOR I ~ 1 STEP 1 UNTIL Z DO SUMX[I]),                00023600
              CUMP(FOR I ~ 1 STEP 1 UNTIL Z DO A[I,1]),                 00023700
              EVEC(J,FOR I ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]),          00023800
              FACT(I,FOR J ~ 1 STEP 1 UNTIL Z DO A[I,J]),               00023900
              CHEK(I,FOR J ~ 1 STEP 1 UNTIL Z DO R[I,J]),               00024000
              ROTM(I,FOR J ~ 1 STEP 1 UNTIL NRO DO A[I,J]),             00024100
              CAS4(FOR J ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]) ;           00024200
SWITCH        U ~ L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15 ;  00024300
FILE IN        TAPE2 2(2,100);                                          00024400
FILE IN        TAPE3 2(2,100);                                          00024500
FILE IN        TAPE4 2(2,100);                                          00024600
SWITCH FILE    FILESW ~ TAPE2,TAPE2,TAPE2,TAPE3,TAPE4;                  00024700
FORMAT         OKTL(100O);                                              00024800
              WRITE(PRINT,FRM1,OUTT) ;                                  00024900
               IF CAS>3 THEN                                            00025000
         BEGIN                                                          00025100
FILE IN        CORMAT 2(2,100);                                         00025200
FORMAT         FCM(                                                     00025300
                    12F6.3                                              00025400
                   );                                                   00025500
               IF CAS = 4 THEN RN ~ 1.0;                                00025600
               IF QZ  < 1 THEN                                          00025700
                   FOR I ~ 1 STEP 1 UNTIL NVAR DO                       00025800
               BEGIN                                                    00025900
               READ(CARD,SWF,CAS4);                                     00026000
               WRITE(PRINT,FRM5);                                       00026100
               WRITE(PRINT,FRM6,CORR);                                  00026200
               END ELSE                                                 00026300
               BEGIN                                                    00026400
                   READ(CORMAT,OKTL,NVAR,NVAR);                         00026500
                   FOR I ~ 1 STEP 1 UNTIL NVAR DO                       00026600
                   READ(CORMAT,OKTL,CAS4);                              00026700
               END;                                                     00026800
               IF CAS!6 THEN GO TO LL33;                                00026900
COMMENT        IF CORRELATIONS ARE READ IN FOR CASE 6 THE MAIN DIAGONAL 00027000
                    SHOULD CONTAIN THE STANDARD DEVIATIONS ;            00027100
               FOR I~1 STEP 1 UNTIL NVAR DO                             00027200
               FOR J~1 STEP 1 UNTIL NVAR DO                             00027300
               R[I,J] ~ SCA[I,J] | NSAM | SCA[I,I] | SCA[J,J];          00027400
               FOR J ~ 1 STEP 1 UNTIL NVAR DO BEGIN                     00027500
               A[J,1] ~ SCA[J,J];   A[J,3] ~ SCA[J,J]|SCA[J,J]|NSM1;    00027600
               END;                                                     00027700
               GO TO LCAS6;                                             00027800
         END ;                                                          00027900
              READ (CARDR,FORM2,SCAL) ;                                 00028000
              IF CAS = 1 THEN                                           00028100
              RN ~ 1.0 ;                                                00028200
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00028300
         BEGIN                                                          00028400
              A[I,2] ~ 0.0 ;                                            00028500
              A[I,3] ~ 0.0 ;                                            00028600
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00028700
              SUMXX[I,J] ~ 0.0                                          00028800
         END ;                                                          00028900
              IF TR THEN                                                00029000
              READ (CARDR,FORM3,TRAN) ;                                 00029100
              FOR M ~ 1 STEP 1 UNTIL NSAM DO                            00029200
         BEGIN                                                          00029300
               IF QZ = 1 THEN IF INP>1 AND INP<5 THEN                   00029400
               READ(FILESW[INP],OKTL,DATR)[WRONGCOUNT] ELSE BEGIN       00029500
               WRITE(LINE,<"INPUT TAPE INCORRECTLY SPECIFIED">);        00029600
               GO TO NOMO END ELSE                                      00029700
               READ(CARD, SWF, DATR)[WRONGCOUNT];                       00029800
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00029900
         BEGIN                                                          00030000
              X[J] ~ DATA[J]|10.0*SC[J] ;                               00030100
              DATA[J] ~ X[J]                                            00030200
         END ;                                                          00030300
              IF TR THEN                                                00030400
         BEGIN                                                          00030500
              FOR J ~ NVAR STEP -1 UNTIL 1 DO                           00030600
         BEGIN                                                          00030700
              GO TO U[T[J]] ;                                           00030800
L1:           GO TO NAXT ;                                              00030900
L2:           DATA[J] ~ X[J]|X[J] ;                                     00031000
              GO TO NAXT ;                                              00031100
L3:           DATA[J] ~ 1 / X[J] ;                                      00031200
              GO TO NAXT ;                                              00031300
L4:           DATA[J] ~ SQRT(X[J]) ;                                    00031400
              GO TO NAXT ;                                              00031500
L5:           DATA[J] ~ EXP(X[J]) ;                                     00031600
              GO TO NAXT ;                                              00031700
L6:           DATA[J] ~ LN(X[J]) ;                                      00031800
              GO TO NAXT ;                                              00031900
L7:           DATA[J] ~ SIN(X[J]) ;                                     00032000
              GO TO NAXT ;                                              00032100
L8:           DATA[J] ~ COS(X[J]) ;                                     00032200
              GO TO NAXT ;                                              00032300
L9:           DATA[J] ~ X[J+1] ;                                        00032400
              GO TO NAXT ;                                              00032500
L10:          DATA[J] ~ X[J+2] ;                                        00032600
              GO TO NAXT ;                                              00032700
L11:          DATA[J] ~ X[J+3] ;                                        00032800
              GO TO NAXT ;                                              00032900
L12:          DATA[J] ~ X[J+4] ;                                        00033000
              GO TO NAXT ;                                              00033100
L13:          DATA[J] ~ X[J+5] ;                                        00033200
              GO TO NAXT ;                                              00033300
L14:          DATA[J] ~ X[J+6] ;                                        00033400
              GO TO NAXT ;                                              00033500
L15:          DATA[J] ~ X[J+7] ;                                        00033600
              GO TO NAXT ;                                              00033700
NAXT:    END                                                            00033800
         END ;                                                          00033900
              FOR L ~ 1 STEP 1 UNTIL NVAR DO                            00034000
         BEGIN                                                          00034100
              A[L,2] ~ A[L,2] + DATA[L] ;                               00034200
              A[L,3] ~ A[L,3] + DATA[L]|DATA[L]                         00034300
         END ;                                                          00034400
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00034500
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            00034600
              SUMXX[J,K] ~ SUMXX[J,K] + DATA[J]|DATA[K]                 00034700
         END ;                                                          00034800
              JJ ~ NVAR - 1 ;                                           00034900
              FOR I ~ 1 STEP 1 UNTIL JJ DO                              00035000
         BEGIN                                                          00035100
              KK ~ I + 1 ;                                              00035200
              FOR J ~ KK STEP 1 UNTIL NVAR DO                           00035300
              SUMXX[J,I] ~ SUMXX[I,J]                                   00035400
         END ;                                                          00035500
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00035600
              SUMX[I] ~ A[I,2]/NS ;                                     00035700
              WRITE(PRINT,FRM2) ;                                       00035800
              WRITE(PRINT,FRM3,AVEX) ;                                  00035900
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00036000
         BEGIN                                                          00036100
              SUMX[J] ~ A[J,3] - ((A[J,2]|A[J,2])/NS) ;                 00036200
              A[J,1] ~ SQRT(SUMX[J]/NSM1)                               00036300
         END ;                                                          00036400
              WRITE(PRINT,FRM4) ;                                       00036500
              WRITE(PRINT,FRM3,SDEV) ;                                  00036600
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00036700
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00036800
              R[I,J] ~ SUMXX[I,J] - ((A[I,2]|A[J,2])/NS) ;              00036900
COMMENT       HERE FOLLOWS  A ROUTINE TO GIVE THE COVARIANCE MATRIX IF  00037000
               SUCH IS DESIRED AND DSIGNATED IN THE PARAMETER CARD ;    00037100
               IF COVARPRNT > 0 THEN   BEGIN                            00037200
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00037300
               FOR J ~ 1 STEP 1 UNTIL NVAR DO                           00037400
               SCA[I,J] ~ R[I,J]/NSM1  ;                                00037500
               WRITE(PRINT,FRM52);                                      00037600
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00037700
               WRITE(PRINT,FRM6,CORR);                                  00037800
               END;                                                     00037900
              FOR K ~ 1 STEP 1 UNTIL JJ DO                              00038000
         BEGIN                                                          00038100
              KK ~ K+1 ;                                                00038200
              FOR L ~ KK STEP 1 UNTIL NVAR DO                           00038300
              R[L,K] ~ R[K,L]                                           00038400
         END ;                                                          00038500
              LL ~ NVAR - 1 ;                                           00038600
              IF CAS < 3 THEN                                           00038700
              GO TO LL3 ;                                               00038800
LCAS6:    BEGIN                                                         00038900
REAL ARRAY     SCA[0:NVAR,0:NVAR];                                      00039000
COMMENT TO AVOID USING SAME ARRAY AS READ INTO IN CASE 6;               00039100
              LL ~ NVAR - 1 ;                                           00039200
              FOR II ~ 1 STEP 1 UNTIL NVAR DO                           00039300
         BEGIN                                                          00039400
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00039500
              T[I] ~ I ;                                                00039600
              FOR I ~ II STEP 1 UNTIL LL DO                             00039700
              T[I] ~ T[I+1] ;                                           00039800
              T[NVAR] ~ II ;                                            00039900
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00040000
         BEGIN                                                          00040100
              JJ ~ T[J] ;                                               00040200
              FOR I ~ 1 STEP 1 UNTIL J DO                               00040300
         BEGIN                                                          00040400
              KK ~ T[I] ;                                               00040500
              SUMXX[I,J] ~ R[KK,JJ]                                     00040600
         END                                                            00040700
         END ;                                                          00040800
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00040900
              A[1,J] ~ SUMXX[1,J] ;                                     00041000
              FOR I ~ 1 STEP 1 UNTIL LL DO                              00041100
         BEGIN                                                          00041200
              FOR J ~ I STEP 1 UNTIL NVAR DO                            00041300
              SCA[I,J] ~ A[I,J]/A[I,I] ;                                00041400
              MM ~ I+1 ;                                                00041500
              FOR J ~ MM STEP 1 UNTIL NVAR DO                           00041600
         BEGIN                                                          00041700
              S ~ 0.0 ;                                                 00041800
              FOR M ~ 1 STEP 1 UNTIL I DO                               00041900
              S ~ S + SCA[M,MM]|A[M,J] ;                                00042000
              A[MM,J] ~ SUMXX[MM,J] - S                                 00042100
         END                                                            00042200
         END ;                                                          00042300
              DATA[II] ~ 0.0 ;                                          00042400
              FOR I ~ 1 STEP 1 UNTIL LL DO                              00042500
              DATA[II] ~ DATA[II] + A[I,NVAR]|SCA[I,NVAR] ;             00042600
              DATA[II] ~ DATA[II] / SUMXX[NVAR,NVAR]                    00042700
         END ;                                                          00042800
          END;                                                          00042900
COMMENT   THE FOLLOWING STATEMENT IS BELIEVED BY HORN TO BE INCORRECT   00043000
               IF CAS=6 THEN GO TO LCAS6A;                              00043100
LL3:          FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00043200
              SUMX[I] ~ SQRT(SUMX[I]) ;                                 00043300
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00043400
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00043500
              SCA[I,J] ~ R[I,J] / (SUMX[I]|SUMX[J]) ;                   00043600
              FOR I ~ 1 STEP 1 UNTIL LL DO                              00043700
         BEGIN                                                          00043800
              KK ~ I+1 ;                                                00043900
              FOR J ~ KK STEP 1 UNTIL NVAR DO                           00044000
              SCA[J,I] ~ SCA[I,J]                                       00044100
         END ;                                                          00044200
               IF CAS = 3 OR CAS = 6 THEN                               00044300
LCAS6A:       FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00044400
              SCA[I,I] ~ DATA[I] ;                                      00044500
LL33:          IF COMM > 0 THEN BEGIN                                   00044600
               OBJECTFMTGEN(COF);                                       00044700
               WRITE(LINE[DBL]);                                        00044800
               READ(CARD,COF,DATR);                                     00044900
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00045000
               SCA[I,I] ~ DATA[I];                                      00045100
          END;                                                          00045200
               WRITE(LINE,FRM5);                                        00045300
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00045400
              WRITE(PRINT,FRM6,CORR) ;                                  00045500
COMMENT       CALCULATE EIGENVALUES AND EIGENVECTORS ;                  00045600
LL34:    BEGIN                                                          00045700
REAL ARRAY    IND[0:NVAR] ;                                             00045800
LABEL         LL4,LL5,LL6,LL7,LL8,LL9,LL10,LL11,LL12,LL13,LL14,LL15,    00045900
              LL16,LL17,LL18,LL19,LL20,LL21,LL22,LL36 ;                 00046000
              NORM ~ 0.0 ;                                              00046100
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00046200
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00046300
              NORM ~ NORM + SCA[I,J]*2 ;                                00046400
              NORM ~ SQRT(NORM) ;                                       00046500
              IF NVAR = 0 THEN                                          00046600
              GO TO LL4 ;                                               00046700
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00046800
         BEGIN                                                          00046900
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00047000
              R[I,J] ~ 0.0 ;                                            00047100
              R[I,I] ~ 1.0                                              00047200
         END ;                                                          00047300
LL4:          EX ~ 1 ;                                                  00047400
              LL ~ NVAR - 2 ;                                           00047500
              IF LL < 0 THEN                                            00047600
              GO TO LL23 ;                                              00047700
              IF LL = 0 THEN                                            00047800
              GO TO LL5 ;                                               00047900
              FOR I ~ 1 STEP 1 UNTIL LL DO                              00048000
         BEGIN                                                          00048100
              II ~ I + 2 ;                                              00048200
              FOR J ~ II STEP 1 UNTIL NVAR DO                           00048300
         BEGIN                                                          00048400
              T1 ~ SCA[I,I+1] ;                                         00048500
              T2 ~ SCA[I,J] ;                                           00048600
LL20:         IF T2 ! 0 THEN                                            00048700
         BEGIN                                                          00048800
LL18:         TE ~ SQRT(T1|T1 + T2|T2) ;                                00048900
              KOS ~ T1 / TE ;                                           00049000
              SYN ~ T2 / TE ;                                           00049100
              IF EX = 1 THEN                                            00049200
              GO TO LL22                                                00049300
              ELSE                                                      00049400
              GO TO LL19                                                00049500
         END ;                                                          00049600
              IF EX = 1 THEN                                            00049700
              GO TO LL36                                                00049800
              ELSE                                                      00049900
              GO TO LL18 ;                                              00050000
LL22:         FOR K ~ I STEP 1 UNTIL NVAR DO                            00050100
         BEGIN                                                          00050200
              T2 ~ KOS | SCA[K,I+1] + SYN | SCA[K,J] ;                  00050300
              SCA[K,J] ~ KOS | SCA[K,J] - SYN | SCA[K,I+1] ;            00050400
              SCA[K,I+1] ~ T2                                           00050500
         END ;                                                          00050600
              FOR K ~ I STEP 1 UNTIL NVAR DO                            00050700
         BEGIN                                                          00050800
              T2 ~ KOS | SCA[I+1,K] + SYN | SCA[J,K] ;                  00050900
              SCA[J,K] ~ KOS | SCA[J,K] - SYN | SCA[I+1,K] ;            00051000
              SCA[I+1,K] ~ T2                                           00051100
         END ;                                                          00051200
              IF NVAR ! 0 THEN                                          00051300
         BEGIN                                                          00051400
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            00051500
         BEGIN                                                          00051600
              T2 ~ KOS | R[K,I+1] + SYN | R[K,J] ;                      00051700
              R[K,J] ~ KOS | R[K,J] - SYN | R[K,I+1] ;                  00051800
              R[K,I+1] ~ T2                                             00051900
         END                                                            00052000
         END ;                                                          00052100
LL36:    END                                                            00052200
         END ;                                                          00052300
LL5:          FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00052400
         BEGIN                                                          00052500
              DIAG[I] ~ SCA[I,I] ;                                      00052600
              SUMX[I] ~ NORM ;                                          00052700
              VAL[I] ~ -NORM                                            00052800
         END ;                                                          00052900
              FOR I ~ 2 STEP 1 UNTIL NVAR DO                            00053000
         BEGIN                                                          00053100
              SD[I-1] ~ SCA[I-1,I] ;                                    00053200
              X[I-1] ~ SD[I-1]*2                                        00053300
         END ;                                                          00053400
              TA ~ 0.0 ;                                                00053500
              I ~ 1 ;                                                   00053600
LL13:         M ~ 0 ;                                                   00053700
              T2 ~ 0.0 ;                                                00053800
              T1 ~ 1.0 ;                                                00053900
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00054000
         BEGIN                                                          00054100
              P ~ DIAG[J] - TA ;                                        00054200
              IF T2 = 0 THEN                                            00054300
              GO TO LL6 ;                                               00054400
              IF T1 = 0 THEN                                            00054500
              GO TO LL7 ;                                               00054600
              TE ~ P|T1 - X[J-1]|T2 ;                                   00054700
              GO TO LL8 ;                                               00054800
LL6:          IF T1 < 0 THEN                                            00054900
         BEGIN                                                          00055000
              T1 ~ -1.0 ;                                               00055100
              TE ~ -P ;                                                 00055200
              GO TO LL8                                                 00055300
         END ;                                                          00055400
LL9:          T1 ~ 1.0 ;                                                00055500
              TE ~ P ;                                                  00055600
              GO TO LL8 ;                                               00055700
LL7:          IF X[J-1] = 0 THEN                                        00055800
              GO TO LL9 ;                                               00055900
              IF T2 } 0 THEN                                            00056000
         BEGIN                                                          00056100
              TE ~ -1.0 ;                                               00056200
              GO TO LL8                                                 00056300
         END ;                                                          00056400
              TE ~ 1.0 ;                                                00056500
LL8:          IF ((T1 < 0) AND (TE < 0)) OR ((T1 } 0) AND (TE } 0)) THEN00056600
              M ~ M+1 ;                                                 00056700
              T2 ~ T1 ;                                                 00056800
              T1 ~ TE                                                   00056900
              ;WHILE ABS(T1)>10 OR ABS(T2)>10 DO                        00057000
          BEGIN                                                         00057100
               T1 ~ T1/10;                                              00057200
               T2~ T2/10;                                               00057300
          END;                                                          00057400
         END ;                                                          00057500
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            00057600
         BEGIN                                                          00057700
              IF K { M THEN                                             00057800
         BEGIN                                                          00057900
              IF TA > VAL[K] THEN                                       00058000
              VAL[K] ~ TA                                               00058100
         END                                                            00058200
              ELSE                                                      00058300
         BEGIN                                                          00058400
              IF TA < SUMX[K] THEN                                      00058500
              SUMX[K] ~ TA                                              00058600
         END                                                            00058700
         END ;                                                          00058800
LL12:         W ~ SUMX[I] - VAL[I] - 5.0@-8 ;                           00058900
              IF W { 0 THEN                                             00059000
              GO TO LL10 ;                                              00059100
              IF SUMX[I] = 0 THEN                                       00059200
              GO TO LL11 ;                                              00059300
              W ~ ABS(VAL[I] / SUMX[I] - 1.0) - 5.0@-8 ;                00059400
              IF W > 0 THEN                                             00059500
              GO TO LL11 ;                                              00059600
LL10:         I ~ I + 1 ;                                               00059700
              IF I > NVAR THEN                                          00059800
              GO TO LL14                                                00059900
              ELSE                                                      00060000
              GO TO LL12 ;                                              00060100
LL11:         TA ~ (VAL[I] + SUMX[I]) / 2.0 ;                           00060200
              GO TO LL13 ;                                              00060300
LL14:         EX ~ 2 ;                                                  00060400
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00060500
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00060600
              SCA[I,J] ~ 0.0 ;                                          00060700
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00060800
         BEGIN                                                          00060900
              IF I = 1 THEN                                             00061000
              GO TO LL21 ;                                              00061100
              IF W { 0 THEN                                             00061200
              GO TO LL16 ;                                              00061300
              IF SUMX[I-1] ! 0 THEN                                     00061400
         BEGIN                                                          00061500
              W ~ ABS(SUMX[I] / SUMX[I-1] - 1.0) - 5.0@-7 ;             00061600
              IF W { 0 THEN                                             00061700
              GO TO LL16                                                00061800
         END ;                                                          00061900
LL21:         KOS ~ 1.0 ;                                               00062000
              SYN ~ 0.0 ;                                               00062100
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00062200
         BEGIN                                                          00062300
              IF J = 1 THEN                                             00062400
              GO TO LL15                                                00062500
              ELSE                                                      00062600
              GO TO LL20 ;                                              00062700
LL19:         X[J-1] ~ SYN ;                                            00062800
              DATA[J-1] ~ KOS ;                                         00062900
              VAL[J-1] ~ T1|KOS + T2|SYN ;                              00063000
LL15:         T1 ~ (DIAG[J]-SUMX[I])|KOS - TA|SYN ;                     00063100
              T2 ~ SD[J] ;                                              00063200
              TA ~ SD[J]|KOS                                            00063300
         END ;                                                          00063400
              VAL[NVAR] ~ T1 ;                                          00063500
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00063600
              IND[J] ~ 0 ;                                              00063700
LL16:         TE ~ NORM ;                                               00063800
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00063900
         BEGIN                                                          00064000
              IF IND[J] } 1 THEN                                        00064100
              GO TO LL17 ;                                              00064200
              IF ABS(TE) { ABS(VAL[J]) THEN                             00064300
              GO TO LL17 ;                                              00064400
              TE ~ VAL[J] ;                                             00064500
              LL ~ J ;                                                  00064600
LL17:    END ;                                                          00064700
              IND[LL] ~ 1 ;                                             00064800
              P ~ 1.0 ;                                                 00064900
              IF LL ! 1 THEN                                            00065000
         BEGIN                                                          00065100
              FOR K ~ 2 STEP 1 UNTIL LL DO                              00065200
         BEGIN                                                          00065300
              M ~ LL+1-K ;                                              00065400
              SCA[M+1,I] ~ DATA[M]|P ;                                  00065500
              P ~ -P|X[M]                                               00065600
         END                                                            00065700
         END ;                                                          00065800
              SCA[1,I] ~ P                                              00065900
         END ;                                                          00066000
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00066100
         BEGIN                                                          00066200
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            00066300
              IND[K] ~ SCA[K,J] ;                                       00066400
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00066500
         BEGIN                                                          00066600
              SCA[I,J] ~ 0.0 ;                                          00066700
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            00066800
              SCA[I,J] ~ R[I,K]|IND[K] + SCA[I,J]                       00066900
         END                                                            00067000
         END                                                            00067100
         END ;                                                          00067200
LL23:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00067300
         BEGIN                                                          00067400
              IF SUMX[I] { RN THEN                                      00067500
         BEGIN                                                          00067600
              Z ~ I-1 ;                                                 00067700
              GO TO LL24                                                00067800
         END                                                            00067900
         END ;                                                          00068000
              Z ~ NVAR ;                                                00068100
LL24:         WRITE(PRINT,FRM7) ;                                       00068200
              WRITE(PRINT,FRM3,EVAL) ;                                  00068300
              IF NVAR > Z THEN                                          00068400
              WRITE(PRINT,FRM8) ;                                       00068500
COMMENT       CALCULATE CUMULATIVE PROPORTIONS OF TOTAL VARIANCE ;      00068600
              A[1,1] ~ SUMX[1] ;                                        00068700
              FOR I ~ 2 STEP 1 UNTIL Z DO                               00068800
              A[I,1] ~ A[I-1,1] + SUMX[I] ;                             00068900
              FOR I ~ 1 STEP 1 UNTIL Z DO                               00069000
              A[I,1] ~ A[I,1] / (       A[Z,1]) ;                       00069100
COMMENT        WAS:                                                     00069200
              A[I,1] ~ A[I,1] / (NVAR | A[Z,1]) ;                       00069300
              WRITE(PRINT,FRM9) ;                                       00069400
              WRITE(PRINT,FRM3,CUMP) ;                                  00069500
              WRITE(PRINT,FRM10) ;                                      00069600
              FOR J ~ 1 STEP 1 UNTIL Z DO                               00069700
              WRITE(PRINT,FRM11,EVEC) ;                                 00069800
              FOR I ~ 1 STEP 1 UNTIL Z DO                               00069900
         BEGIN                                                          00070000
              SUMX[I] ~ SQRT(SUMX[I]) ;                                 00070100
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00070200
              A[J,I] ~ SUMX[I]|SCA[J,I]                                 00070300
         END ;                                                          00070400
              WRITE(PRINT,FRM12) ;                                      00070500
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00070600
              WRITE(PRINT,FRM13,FACT) ;                                 00070700
              IF NVAR > Z THEN                                          00070800
         BEGIN                                                          00070900
              FOR I ~ 1 STEP 1 UNTIL Z DO                               00071000
              FOR J ~ 1 STEP 1 UNTIL Z DO                               00071100
         BEGIN                                                          00071200
              R[I,J] ~ 0.0 ;                                            00071300
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            00071400
              R[I,J] ~ R[I,J] + A[K,I]|A[K,J]                           00071500
         END ;                                                          00071600
              WRITE(PRINT,FRM14)                                        00071700
         END                                                            00071800
              ELSE                                                      00071900
         BEGIN                                                          00072000
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00072100
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            00072200
         BEGIN                                                          00072300
              R[I,J] ~ 0.0 ;                                            00072400
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            00072500
              R[I,J] ~ R[I,J] + A[I,K]|A[J,K]                           00072600
         END ;                                                          00072700
              WRITE(PRINT,FRM15)                                        00072800
         END ;                                                          00072900
              FOR I ~ 1 STEP 1 UNTIL Z DO                               00073000
              WRITE(PRINT,FRM6,CHEK) ;                                  00073100
              WRITE(PRINT,FRM16,LIM) ;                                  00073200
              IF LIM = 0 THEN                                           00073300
              GO TO LL26 ;                                              00073400
              FOR J ~ 1 STEP 1 UNTIL Z DO                               00073500
         BEGIN                                                          00073600
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00073700
         BEGIN                                                          00073800
              W ~ ABS(A[I,J]) ;                                         00073900
              IF W } LIM THEN                                           00074000
              GO TO LL25                                                00074100
         END ;                                                          00074200
               RN ~ J-1;                                                00074300
               GO TO LL27;                                              00074400
LL25:     END;                                                          00074500
LL26:          RN ~ Z;                                                  00074600
LL27:          IF (NRO=0) OR (NRO>RN) THEN                              00074700
               NRO ~ RN;                                                00074800
               IF NRO { 1 THEN                                          00074900
         BEGIN                                                          00075000
              WRITE(PRINT,FRM17) ;                                      00075100
              GO TO START                                               00075200
         END ;                                                          00075300
COMMENT       ROTATE FACTOR MATRIX ;                                    00075400
LL35:    BEGIN                                                          00075500
REAL          TN4P,ST,CT,S2T,C2T,S4T,C4T,TN4T,CN4T ;                    00075600
LABEL         LL28,LL29,LL30,LL31,LL32 ;                                00075700
              T1 ~ 0.00116 ;                                            00075800
              JJ ~ 3 ;                                                  00075900
              P ~ 1.0 ;                                                 00076000
              KK ~ 0 ;                                                  00076100
              MM ~ (NRO|(NRO-1))/2 ;                                    00076200
              DATA[1] ~ 0.0 ;                                           00076300
              LL ~ NRO - 1 ;                                            00076400
              II ~ 1 ;                                                  00076500
              EX ~ 0 ;                                                  00076600
              T2 ~ NVAR ;                                               00076700
              TA ~ T2*2 ;                                               00076800
              W ~ 1.0 / SQRT(2.0) ;                                     00076900
              NS ~ 0.0001 ;                                             00077000
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00077100
              SUMX[I] ~ 0.0 ;                                           00077200
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00077300
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00077400
              SUMX[I] ~ SUMX[I] + A[I,J]|A[I,J] ;                       00077500
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00077600
         BEGIN                                                          00077700
              SUMX[I] ~ SQRT(SUMX[I]) ;                                 00077800
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00077900
              A[I,J] ~ A[I,J] / SUMX[I]                                 00078000
         END ;                                                          00078100
LL29:         II ~ II + 1 ;                                             00078200
              DATA[II] ~ 0.0 ;                                          00078300
              Z ~ II - 1 ;                                              00078400
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00078500
         BEGIN                                                          00078600
              RN ~ 0.0 ;                                                00078700
              BE ~ 0.0 ;                                                00078800
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00078900
         BEGIN                                                          00079000
              S ~ A[I,J]|A[I,J] ;                                       00079100
              RN ~ RN + S ;                                             00079200
              BE ~ BE + S|S                                             00079300
         END ;                                                          00079400
              DATA[II] ~ DATA[II] + (T2|BE - RN|RN)/TA                  00079500
         END ;                                                          00079600
              IF II } 50 THEN                                           00079700
              GO TO LL30 ;                                              00079800
              MARY ~ ABS(DATA[II] - DATA[Z]) - 1.0@-7 ;                 00079900
              IF MARY { 0 THEN                                          00080000
         BEGIN                                                          00080100
              EX ~ EX + 1 ;                                             00080200
              IF EX > JJ THEN                                           00080300
              GO TO LL30                                                00080400
         END ;                                                          00080500
              FOR J ~ 1 STEP 1 UNTIL LL DO                              00080600
         BEGIN                                                          00080700
              L ~ J + 1 ;                                               00080800
              FOR K ~ L STEP 1 UNTIL NRO DO                             00080900
         BEGIN                                                          00081000
              IF KK } MM THEN                                           00081100
              GO TO LL31 ;                                              00081200
              IF ABS(P) > NS THEN                                       00081300
              KK ~ 0 ;                                                  00081400
              RN ~ 0.0 ;                                                00081500
              BE ~ 0.0 ;                                                00081600
              S ~ 0.0 ;                                                 00081700
              NORM ~ 0.0 ;                                              00081800
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00081900
         BEGIN                                                          00082000
              MARY ~ (A[I,J] + A[I,K])|(A[I,J] - A[I,K]) ;              00082100
              TE ~ A[I,J]|A[I,K] ;                                      00082200
              TE ~ TE + TE ;                                            00082300
              S ~ S + (MARY + TE)|(MARY - TE) ;                         00082400
              NORM ~ NORM + 2.0|MARY|TE ;                               00082500
              RN ~ RN + MARY ;                                          00082600
              BE ~ BE + TE                                              00082700
         END ;                                                          00082800
              TE ~ NORM - 2.0|RN|BE/T2 ;                                00082900
              B ~ S - (RN|RN - BE|BE)/T2 ;                              00083000
              P ~ 0.25|ARCTAN(TE/B) ;                                   00083100
              IF ABS(P) { NS THEN                                       00083200
         BEGIN                                                          00083300
              KK ~ KK + 1 ;                                             00083400
              GO TO LL31                                                00083500
         END ;                                                          00083600
              TN4P ~ TE/B ;                                             00083700
              IF TE = B THEN                                            00083800
         BEGIN                                                          00083900
              IF TE + B < T1 THEN                                       00084000
              GO TO LL31 ;                                              00084100
              C4T ~ W ;                                                 00084200
              S4T ~ W ;                                                 00084300
              GO TO LL28                                                00084400
         END                                                            00084500
              ELSE IF TE < B THEN                                       00084600
         BEGIN                                                          00084700
              TN4T ~ ABS(TE)/ABS(B) ;                                   00084800
              IF TN4T } T1 THEN                                         00084900
         BEGIN                                                          00085000
              C4T ~ 1.0 / SQRT(1.0 + TN4T*2) ;                          00085100
              S4T ~ TN4T|C4T ;                                          00085200
              GO TO LL28                                                00085300
         END ;                                                          00085400
              IF B } 0 THEN                                             00085500
              GO TO LL31 ;                                              00085600
              SYN ~ W ;                                                 00085700
              KOS ~ W ;                                                 00085800
              GO TO LL32                                                00085900
         END                                                            00086000
              ELSE                                                      00086100
         BEGIN                                                          00086200
              CN4T ~ ABS(TE/B) ;                                        00086300
              IF CN4T } T1 THEN                                         00086400
         BEGIN                                                          00086500
              S4T ~ 1.0 / SQRT(1.0 + CN4T*2) ;                          00086600
              C4T ~ CN4T|S4T ;                                          00086700
              GO TO LL28                                                00086800
         END ;                                                          00086900
              C4T ~ 0.0 ;                                               00087000
              S4T ~ 1.0                                                 00087100
         END ;                                                          00087200
LL28:         C2T ~ SQRT((1.0 + C4T)/2.0) ;                             00087300
              S2T ~ S4T/(2.0|C2T) ;                                     00087400
              CT ~ SQRT((1.0 + C2T)/2.0) ;                              00087500
              ST ~ S2T/(2.0|CT) ;                                       00087600
              IF B > 0 THEN                                             00087700
         BEGIN                                                          00087800
              KOS ~ CT ;                                                00087900
              SYN ~ ST                                                  00088000
         END                                                            00088100
              ELSE                                                      00088200
         BEGIN                                                          00088300
              KOS ~ W|CT + W|ST ;                                       00088400
              SYN ~ ABS(W|CT - W|ST)                                    00088500
         END ;                                                          00088600
              IF TE { 0 THEN                                            00088700
              SYN ~ -SYN ;                                              00088800
LL32:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00088900
         BEGIN                                                          00089000
              RN ~ A[I,J]|KOS + A[I,K]|SYN ;                            00089100
              BE ~ -A[I,J]|SYN + A[I,K]|KOS ;                           00089200
              A[I,J] ~ RN ;                                             00089300
              A[I,K] ~ BE                                               00089400
         END                                                            00089500
         END ;                                                          00089600
LL31:    END ;                                                          00089700
              GO TO LL29 ;                                              00089800
LL30:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00089900
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00090000
              A[I,J] ~ A[I,J]|SUMX[I] ;                                 00090100
              EX ~ II - 2 ;                                             00090200
              WRITE(PRINT,FRM18,NRO,EX) ;                               00090300
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00090400
              WRITE(PRINT,FRM13,ROTM) ;                                 00090500
              WRITE(PRINT,FRM19) ;                                      00090600
               IF PNCH = 1 THEN                                         00090700
               BEGIN                                                    00090800
               FOR I~1 STEP 1 UNTIL NVAR DO                             00090900
                   WRITE(PUNCH,PUNCHOUT,ROTM);                          00091000
               END;                                                     00091100
              FOR I ~ 2 STEP 1 UNTIL II DO                              00091200
         BEGIN                                                          00091300
              EX ~ I - 2 ;                                              00091400
              WRITE(PRINT,FRM20,EX,DATA[I])                             00091500
         END ;                                                          00091600
              WRITE(PRINT,FRM21) ;                                      00091700
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00091800
         BEGIN                                                          00091900
              SUMX[I] ~ SUMX[I]|SUMX[I] ;                               00092000
              BE ~ 0.0 ;                                                00092100
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00092200
              BE ~ BE + A[I,J]|A[I,J] ;                                 00092300
              RN ~ BE - SUMX[I] ;                                       00092400
              WRITE(PRINT,FRM22,I,SUMX[I],BE,RN)                        00092500
         END                                                            00092600
         END                                                            00092700
         END                                                            00092800
         END ;                                                          00092900
              GO TO START ;                                             00093000
WRONGCOUNT:    WRITE(LINE, ERRCNT);                                     00093100
NOMO:          DATELINE(0);                                             00093200
          END.                                                          00093300
