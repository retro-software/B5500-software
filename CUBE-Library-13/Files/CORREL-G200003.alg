          BEGIN                                                         00000000
COMMENT        CORREL/STATMAN;                                          00000100
COMMENT        THIS IS CORREL: A PROGRAM TO COMPUTE: 1. MEANS,          00000200
               2. STANDARD DEVIATIONS, 3. RAW SUMS OF SQUARES AND CROSS 00000300
               PRODUCTS, 4. DEVIATION SUMS OF SQUARES AND CROSS PRODUCTS00000400
               5. UARIANCE COUARIANCE MATRIX AND 6. CORRELATION MATRIX: 00000500
               VERSION OF JULY 20, 1966 BY JOHN WACKWITZ  ;             00000600
FILE IN        CARD  0(2,10) ;                                          00000700
FILE OUT       LINE  4(2,15) ;                                          00000800
FILE OUT       PUNCH 0(2,10) ;                                          00000900
FILE OUT       TAPE2  2(2,100, SAVE 10) ;                               00001000
FILE OUT       TAPE3  2(2,100, SAVE 10) ;                               00001100
FILE OUT       TAPE4  2(2,100, SAVE 10) ;                               00001200
FILE OUT       CORMAT 2(2,100, SAVE 10) ;                               00001300
SWITCH FILE    SWFILE ~ TAPE2, TAPE3, TAPE4, CORMAT ;                   00001400
INTEGER        M, NCASES, L,INP, PR, P1, P2, P3, P4, P5,      PROB, I,  00001500
               J, K, DX1, T2, T3, T4, T5, LIN2, LIN3, LIN4, LIN5 ;      00001600
LABEL          START, FINIS ;                                           00001700
FORMAT         FMTIN(I3,I4) ,                                           00001800
               FMTQ(/////////////////////////////////////////////////   00001900
                    /////////////////////////////////////////////////   00002000
                    /////////////////////////////////////////////////   00002100
                    /////////////////////////////////////////////////); 00002200
STREAM PROCEDURE    REINITIAL (FMT);                                    00002300
          BEGIN                                                         00002400
     LOCAL     STROKE;                                                  00002500
               SI ~FMT;                                                 00002600
               DI ~ LOC STROKE;                                         00002700
               DS ~ 8 LIT "F00000+0";                                   00002800
               SI ~ LOC STROKE;                                         00002900
               DI ~ FMT;                                                00003000
               DI ~ DI + 8;                                             00003100
               3(63(DS ~ WDS; SI ~ LOC STROKE););                       00003200
               4(DS ~ WDS;  SI ~ LOC STROKE);                           00003300
               DS ~ 8 LIT "+0000036";                                   00003400
               DS ~ 8 LIT "0000HY00";                                   00003500
          END  OF REINITIAL;                                            00003600
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00003700
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00003800
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00003900
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00004000
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00004100
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00004200
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00004300
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00004400
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00004500
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00004600
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00004700
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00004800
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00004900
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00005000
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00005100
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00005200
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00005300
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00005400
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300005500
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00005600
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00005700
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00005800
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00005900
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00006000
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00006100
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00006200
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00006300
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100006400
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00006500
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00006600
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00006700
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00006800
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00006900
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00007000
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00007100
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00007200
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00007300
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00007400
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00007500
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00007600
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00007700
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00007800
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00007900
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00008000
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00008100
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00008200
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00008300
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00008400
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00008500
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00008600
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00008700
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00008800
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00008900
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00009000
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00009100
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800009200
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00009300
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00009400
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00009500
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00009600
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00009700
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00009800
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00009900
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00010000
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00010100
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00010200
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00010300
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00010400
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00010500
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00010600
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00010700
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00010800
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00010900
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00011000
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00011100
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00011200
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00011300
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00011400
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00011500
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00011600
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00011700
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00011800
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00011900
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00012000
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00012100
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00012200
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00012300
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00012400
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00012500
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00012600
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00012700
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00012800
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00012900
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100013000
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00013100
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00013200
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00013300
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00013400
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00013500
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000013600
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00013700
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00013800
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00013900
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00014000
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00014100
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEA00014200
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",00014300
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"///),LAYT(//"EXECUTION TIM00014400
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///00014500
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=00014600
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAY00014700
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FE00014800
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.00014900
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];EN00015000
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THEN00015100
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,00015200
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       00015300
PROCEDURE      MPRINT (NROW, NCOL, R, ID) ;                             00015400
VALUE          NROW, NCOL ;                                             00015500
INTEGER        NROW, NCOL ;                                             00015600
REAL ARRAY     R[0,0] ;                                                 00015700
ALPHA          ID ;                                                     00015800
          BEGIN                                                         00015900
INTEGER ARRAY  II[0:NCOL] ;                                             00016000
INTEGER        J1, J2, JSEC ;                                           00016100
FORMAT         HEAD (/X3,A6,X2,"SECTION",I3//),                         00016200
               FI   (/X3,"ROW",X3,10I11) ,                              00016300
               FDATA(I6,X4,10R11.2) ;                                   00016400
LIST           LI   (FOR DX1 ~ J1 STEP 1 UNTIL J2 DO II[DX1]) ,         00016500
               LDATA(I,FOR DX1 ~ J1 STEP 1 UNTIL J2 DO R[I,DX1]) ;      00016600
LABEL          LSTART;                                                  00016700
               J1 ~ 0 ;                                                 00016800
               J2 ~ 0 ;                                                 00016900
               JSEC ~ 0 ;                                               00017000
               FOR J ~ 1 STEP 1 UNTIL NCOL DO                           00017100
               II[J] ~ J ;                                              00017200
LSTART:        J1 ~ J2 + 1 ;                                            00017300
               J2 ~ J1 + 9 ;                                            00017400
               IF J2 > NCOL THEN                                        00017500
               J2 ~ NCOL ;                                              00017600
               JSEC ~ JSEC + 1 ;                                        00017700
               WRITE(LINE,HEAD,ID,JSEC) ;                               00017800
               WRITE(LINE,FI,LI) ;                                      00017900
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00018000
               WRITE(LINE,FDATA,LDATA) ;                                00018100
               IF J2 < NCOL THEN                                        00018200
          BEGIN                                                         00018300
               WRITE(LINE[PAGE]);                                       00018400
               GO TO LSTART ;                                           00018500
          END ;                                                         00018600
               WRITE(LINE[PAGE]) ;                                      00018700
          END  OF MPRINT ;                                              00018800
PROCEDURE      MPUNCH ( NROW, NCOL, R, ID, L, PROB) ;                   00018900
VALUE          NROW, NCOL, L ;                                          00019000
INTEGER        NROW, NCOL, L, PROB ;                                    00019100
REAL ARRAY     R[0,0] ;                                                 00019200
ALPHA          ID ;                                                     00019300
          BEGIN                                                         00019400
INTEGER        J1,J2,JSEC,L1 ;                                          00019500
FORMAT         FSMAL(I3,X1,A1,I3,I2,10R7.2) ,                           00019600
               FLRGE(I3,X1,A1,I3,I2,5R14.7) ;                           00019700
LIST           LDATA(PROB, ID, I, JSEC,FOR DX1 ~ J1 STEP 1 UNTIL J2 DO  00019800
                    R[I,DX1]) ;                                         00019900
LABEL          PSTART ;                                                 00020000
               IF L = 1 THEN                                            00020100
               L1 ~ 9 ELSE                                              00020200
               L1 ~ 4 ;                                                 00020300
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00020400
          BEGIN                                                         00020500
               J1 ~ 0 ;                                                 00020600
               J2 ~ 0 ;                                                 00020700
               JSEC ~ 0 ;                                               00020800
PSTART:        J1 ~ J2 + 1 ;                                            00020900
               J2 ~ J1 + L1 ;                                           00021000
               IF J2 > NCOL THEN                                        00021100
               J2 ~ NCOL ;                                              00021200
               JSEC ~ JSEC + 1 ;                                        00021300
               IF L = 1 THEN                                            00021400
               WRITE(PUNCH,FSMAL,LDATA)  ELSE                           00021500
               WRITE(PUNCH,FLRGE,LDATA) ;                               00021600
               IF J2 < NCOL THEN                                        00021700
               GO TO PSTART ;                                           00021800
          END ;                                                         00021900
          END  OF MPUNCH ;                                              00022000
PROCEDURE      MTAPE (NROW, NCOL, R, SWF) ;                             00022100
VALUE          NROW, NCOL ;                                             00022200
INTEGER        NROW, NCOL ;                                             00022300
REAL ARRAY     R[0,0] ;                                                 00022400
FILE OUT       SWF ;                                                    00022500
          BEGIN                                                         00022600
FORMAT         OKTL (100O) ;                                            00022700
LIST           LTAPE (FOR DX1 ~ 1 STEP 1 UNTIL NCOL DO R[I,DX1]) ;      00022800
               WRITE(SWF,OKTL,NROW,NCOL) ;                              00022900
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00023000
               WRITE(SWF,OKTL,LTAPE) ;                                  00023100
          END  OF MTAPE ;                                               00023200
PROCEDURE      MAINPRO ;                                                00023300
          BEGIN                                                         00023400
OWN REAL ARRAY R[0:M,0:M], SUMX[0:M], SD[0:M], DATA[0:M] ;              00023500
INTEGER ARRAY  ID[0:12] ;                                               00023600
LABEL          DUMMY ;                                                  00023700
FORMAT         FID  (13A6) ,                                            00023800
               TITLE(//X50,"CORRELATION ANALYSIS"//),                   00023900
               FOUT (//X10,"NUMBER OF VARIABLES= ",I6/                  00024000
                       X10,"NUMBER OF SUBJECTS = ",I6//),               00024100
               FRAW (//X41,"RAW SUMS OF SQUARES AND CROSS PRODUCTS"//), 00024200
               FDEV (//X38,"DEVIATION SUMS OF SQUARES AND CROSS ",      00024300
                           "PRODUCTS"//),                               00024400
               FVAR (//X47,"VARIANCE-COVARIANCE MATRIX"//),             00024500
               FCOR (//X51,"CORRELATION MATRIX"//),                     00024600
               FIPAR (15I1,I3) ;                                        00024700
LIST           LID  (FOR DX1 ~ 0 STEP 1 UNTIL 12 DO ID[DX1]),           00024800
               LIPAR(PR,P1,P2,T2,LIN2,P3,T3,LIN3,P4,T4,LIN4,P5,T5,LIN5, 00024900
                 INP,PROB) ;                                            00025000
PROCEDURE      MEANSANDSD ;                                             00025100
          BEGIN                                                         00025200
FILE IN        RAWMAT  2(2,100) ;                                       00025300
FORMAT         OK(100O) ;                                               00025400
FORMAT         FMEAN(//X10,"MEANS OF THIS GROUP"//),                    00025500
               FSDEV(//X10,"STANDARD DEVIATIONS OF THIS GROUP"//),      00025600
               FDATA(X5, 9(X2,R10.4)) ,                                 00025700
               FPUNC(7R10.4) ;                                          00025800
LIST           LMEAN(FOR DX1 ~ 1 STEP 1 UNTIL M DO SUMX[DX1]),          00025900
               LSDEV(FOR DX1 ~ 1 STEP 1 UNTIL M DO SD[DX1]),            00026000
               LDATA(FOR DX1 ~ 1 STEP 1 UNTIL M DO DATA[DX1]) ;         00026100
               FOR I ~ 1 STEP 1 UNTIL M DO                              00026200
          BEGIN                                                         00026300
               SUMX[I] ~ 0 ;                                            00026400
               FOR J ~ 1 STEP 1 UNTIL M DO                              00026500
               R[I,J] ~ 0 ;                                             00026600
          END ;                                                         00026700
               IF INP > 0 THEN                                          00026800
               READ(RAWMAT,OK,NCASES,M) ELSE                            00026900
          BEGIN                                                         00027000
               REINITIAL(FMTQ) ;                                        00027100
               OBJECTFMTGEN(FMTQ) ;                                     00027200
          END ;                                                         00027300
               FOR K ~ 1 STEP 1 UNTIL NCASES DO                         00027400
          BEGIN                                                         00027500
               IF INP > 0 THEN                                          00027600
               READ(RAWMAT,OK,LDATA) [FINIS]  ELSE                      00027700
               READ(CARD,FMTQ,LDATA)[FINIS] ;                           00027800
               FOR I ~ 1 STEP 1 UNTIL M DO                              00027900
          BEGIN                                                         00028000
               SUMX[I] ~ SUMX[I] + DATA[I] ;                            00028100
               FOR J ~ 1 STEP 1 UNTIL M DO                              00028200
               R[I,J] ~ R[I,J] + DATA[I] | DATA[J] ;                    00028300
          END ;                                                         00028400
          END ;                                                         00028500
               FOR I ~ 1 STEP 1 UNTIL M DO                              00028600
          BEGIN                                                         00028700
               SD[I] ~ R[I,I] - SUMX[I] * 2 / NCASES ;                  00028800
               SD[I] ~ SQRT(SD[I]/(NCASES - 1)) ;                       00028900
               SUMX[I] ~ SUMX[I] / NCASES ;                             00029000
          END ;                                                         00029100
               WRITE(LINE,FMEAN) ;                                      00029200
               WRITE(LINE,FDATA,LMEAN) ;                                00029300
               WRITE(LINE,FSDEV) ;                                      00029400
               WRITE(LINE,FDATA,LSDEV) ;                                00029500
               WRITE(LINE[PAGE]) ;                                      00029600
               IF P1 > 0 THEN                                           00029700
          BEGIN                                                         00029800
               WRITE(PUNCH,FMEAN) ;                                     00029900
               WRITE(PUNCH,FPUNC,LMEAN) ;                               00030000
               WRITE(PUNCH,FSDEV) ;                                     00030100
               WRITE(PUNCH,FPUNC,LSDEV) ;                               00030200
          END ;                                                         00030300
          END  OF MEANSANDSD ;                                          00030400
                                                                        00030500
PROCEDURE      DEVSUMSQ ;                                               00030600
          BEGIN                                                         00030700
               FOR I ~ 1 STEP 1 UNTIL M DO                              00030800
               FOR J ~ 1 STEP 1 UNTIL M DO                              00030900
               R[I,J] ~ R[I,J] - SUMX[I] | SUMX[J] | NCASES ;           00031000
          END  OF DEVSUMSQ ;                                            00031100
                                                                        00031200
PROCEDURE      VARCOVAR ;                                               00031300
          BEGIN                                                         00031400
               FOR I ~ 1 STEP 1 UNTIL M DO                              00031500
               FOR J ~ 1 STEP 1 UNTIL M DO                              00031600
               R[I,J] ~ R[I,J] /(NCASES - 1) ;                          00031700
          END  OF VARCOVAR ;                                            00031800
                                                                        00031900
PROCEDURE      CORRELMAT ;                                              00032000
          BEGIN                                                         00032100
               FOR I ~ 1 STEP 1 UNTIL M DO                              00032200
               FOR J ~ 1 STEP 1 UNTIL M DO                              00032300
               IF SD[I] = 0 OR SD[J] = 0 THEN R[I,J] ~ 0 ELSE           00032400
               R[I,J] ~ R[I,J] /(SD[I] | SD[J]);                        00032500
          END  OF CORRELMAT ;                                           00032600
                                                                        00032700
COMMENT        *****  START OF MAINPRO  *****  ;                        00032800
               WRITE(LINE,TITLE) ;                                      00032900
               READ(CARD,FID,LID)[FINIS] ;                              00033000
               READ(CARD,FIPAR,LIPAR)[FINIS] ;                          00033100
               WRITE(LINE,FID,LID) ;                                    00033200
               WRITE(LINE,FOUT,M,NCASES) ;                              00033300
               L ~ 0 ;                                                  00033400
               MEANSANDSD;                                              00033500
               IF PR > 0 THEN                                           00033600
               GO TO DUMMY ;                                            00033700
               IF P2 > 0 AND P2 < 3 THEN                                00033800
               MPUNCH (M,M,R,"B",P2,PROB) ;                             00033900
               IF T2 > 0 THEN                                           00034000
               MTAPE (M,M,R,SWFILE[L]) ;                                00034100
               IF LIN2 > 0 THEN                                         00034200
          BEGIN                                                         00034300
               WRITE(LINE,FRAW) ;                                       00034400
               MPRINT (M,M,R,"RAW SS") ;                                00034500
          END;                                                          00034600
               L ~ 1 ;                                                  00034700
               DEVSUMSQ ;                                               00034800
               IF P3 > 0 AND P3 < 3 THEN                                00034900
               MPUNCH (M,M,R,"D",P3,PROB) ;                             00035000
               IF T3 > 0 THEN                                           00035100
               MTAPE (M,M,R,SWFILE[L]) ;                                00035200
               IF LIN3 > 0 THEN                                         00035300
          BEGIN                                                         00035400
               WRITE(LINE,FDEV) ;                                       00035500
               MPRINT (M,M,R,"DEV SS") ;                                00035600
          END;                                                          00035700
               L ~ 2 ;                                                  00035800
               VARCOVAR ;                                               00035900
               IF P4 > 0 AND P4 < 3 THEN                                00036000
               MPUNCH (M,M,R,"C",P4,PROB) ;                             00036100
               IF T4 > 0 THEN                                           00036200
               MTAPE (M,M,R,SWFILE[L]) ;                                00036300
               IF LIN4 > 0 THEN                                         00036400
          BEGIN                                                         00036500
               WRITE(LINE,FVAR) ;                                       00036600
               MPRINT (M,M,R,"COVAR ") ;                                00036700
          END;                                                          00036800
               L ~ 3 ;                                                  00036900
               CORRELMAT ;                                              00037000
               IF P5 > 0 AND P5 < 3 THEN                                00037100
               MPUNCH (M,M,R,"R",P5,PROB) ;                             00037200
               IF T5 > 0 THEN                                           00037300
               MTAPE (M,M,R,SWFILE[L]) ;                                00037400
               IF LIN5 > 0 THEN                                         00037500
          BEGIN                                                         00037600
               WRITE(LINE,FCOR) ;                                       00037700
               MPRINT (M,M,R,"R MAT ") ;                                00037800
          END;                                                          00037900
DUMMY:    END  OF MAINPRO ;                                             00038000
                                                                        00038100
COMMENT        *****  START OF PROGRAM  *****  ;                        00038200
START:         READ(CARD,FMTIN,M,NCASES)[FINIS] ;                       00038300
               DATELINE("CORREL") ;                                     00038400
               MAINPRO ;                                                00038500
               GO TO START ;                                            00038600
FINIS:         DATELINE(0) ;                                            00038700
          END  OF CORREL .                                              00038800
                LAST CARD ON CRDIMG TAPE                                99999999
