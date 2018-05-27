          BEGIN                                                         00000100
COMMENT        PARCOR/STATMAN;                                          00000200
COMMENT        THIS IS PARCOR:A PROGRAM TO COMPUTE: 1. MEANS, 2. STANDA 00000300
               RD DEVIATIONS, 3. RAW SUMS OF SQUARES AND CROSSPRODUCTS, 00000400
               4. DEVIATION SUMS OF SQUARES AND CROSSPRODUCTS, 5. VARIA 00000500
               NCE COVARIANCE MATRIX 6. CORRELATION MATRIX, AND 7. SUC  00000600
               CESSIVE PARTIAL CORRELATION MATRICES AND PARTIAL VARIANCE00000700
               S WHEN INPUT IS RAW DATA: OR TO COMPUTE ONLY SUCCESSIVE  00000800
               PARTIAL CORRELATION MATRICES AND PARTIAL VARIANCES WHEN  00000900
               INPUT IS A CORRELATION MATRIX;                           00001000
FILE IN        CARD 0(2,10);                                            00001100
FILE           CORMAT 2(2,100);                                         00001200
FILE OUT       LINE 4(2,15);                                            00001300
FILE OUT       PUNCH 0(2,10);                                           00001400
FILE OUT       TAPE2 2(2,100, SAVE 10);                                 00001500
FILE OUT       TAPE3 2(2,100, SAVE 10);                                 00001600
FILE OUT       TAPE4 2(2,100, SAVE 10);                                 00001700
SWITCH FILE    SWFILE ~ TAPE2, TAPE3, TAPE4, CORMAT;                    00001800
INTEGER        M, NCASES, L, INP, PR, P1, P2, P3, P4, P5, PROB, I, J, K,00001900
               DX1, T2, T3, T4, T5, LIN2, LIN3, LIN4, LIN5, DATATYPE,   00002000
               INDEVICE, S, NPARS, P6, P7, LIN6, LIN7, T6, T7, T8, T9,  00002100
               CALC;                                                    00002200
LABEL          START, FINIS;                                            00002300
FORMAT         FMTIN(I3,I4,I1,I1,I2);                                   00002400
COMMENT        START OF PROGRAM;                                        00002500
               READ(CARD,FMTIN,M,NCASES,DATATYPE,INDEVICE,NPARS)[FINIS];00002600
               IF NPARS > M - 1 THEN GO TO FINIS;                       00002700
          BEGIN                                                         00002800
REAL ARRAY     CORMATT[0:M,0:M];                                        00002900
REAL ARRAY     PCOVAR[0:M,0:M];                                         00003000
REAL ARRAY     VCOVAR[0:M,0:M];                                         00003100
INTEGER ARRAY  ID[0:12];                                                00003200
LIST           LDATA(FOR J ~ 1 STEP 1 UNTIL M DO CORMATT[I,J]),         00003300
               LID(FOR DX1 ~ 0 STEP 1 UNTIL 12 DO ID[DX1]),             00003400
               LIPAR(PR,P1,P2,T2,LIN2,P3,T3,LIN3,P4,T4,LIN4,P5,LIN5,INP,00003500
               PROB,LIN6,P6,T6,T7,T8,T9,CALC,LIN7,P7);                  00003600
LABEL          START, FINIS, HAVECOR, TAPEIN, CARDIN, STARTPAR, REPEAT, 00003700
               CHECK, PARCORREL;                                        00003800
FORMAT         OK(100O),                                                00003900
               FIPAR(14I1,I3,9I1),                                      00004000
               FID(13A6),                                               00004100
               FOUT(//X10,"NUMBER OF VARIABLES = ",I6/                  00004200
                      X10,"NUMBER OF SUBJECTS = ",I6//),                00004300
               PCVHEAD(//X3,"PARTIAL COVARIANCES AND VARIANCES AFTER    00004400
REMOVAL OF VARIABLES ONE THROUGH",X2,I2/),                              00004500
               PCHEAD(//X3,"PARTIAL CORRELATION MATRIX  AFTER REMOVAL OF00004600
VARIABLES ONE THROUGH",X2,I2/);                                         00004700
FORMAT         FMTQ(/////////////////////////////////////////////////   00004800
                    /////////////////////////////////////////////////   00004900
                    /////////////////////////////////////////////////   00005000
                    /////////////////////////////////////////////////); 00005100
STREAM PROCEDURE    REINITIAL (FMT);                                    00005200
          BEGIN                                                         00005300
     LOCAL     STROKE;                                                  00005400
               SI ~FMT;                                                 00005500
               DI ~ LOC STROKE;                                         00005600
               DS ~ 8 LIT "F00000+0";                                   00005700
               SI ~ LOC STROKE;                                         00005800
               DI ~ FMT;                                                00005900
               DI ~ DI + 8;                                             00006000
               3(63(DS ~ WDS; SI ~ LOC STROKE););                       00006100
               4(DS ~ WDS;  SI ~ LOC STROKE);                           00006200
               DS ~ 8 LIT "+0000036";                                   00006300
               DS ~ 8 LIT "0000HY00";                                   00006400
          END  OF REINITIAL;                                            00006500
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00006600
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00006700
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00006800
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00006900
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00007000
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00007100
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00007200
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00007300
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00007400
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00007500
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00007600
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00007700
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00007800
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00007900
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00008000
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00008100
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00008200
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00008300
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300008400
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00008500
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00008600
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00008700
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00008800
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00008900
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00009000
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00009100
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00009200
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100009300
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00009400
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00009500
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00009600
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00009700
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00009800
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00009900
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00010000
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00010100
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00010200
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00010300
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00010400
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00010500
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00010600
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00010700
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00010800
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00010900
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00011000
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00011100
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00011200
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00011300
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00011400
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00011500
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00011600
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00011700
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00011800
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00011900
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00012000
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800012100
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00012200
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00012300
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00012400
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00012500
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00012600
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00012700
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00012800
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00012900
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00013000
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00013100
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00013200
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00013300
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00013400
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00013500
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00013600
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00013700
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00013800
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00013900
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00014000
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00014100
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00014200
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00014300
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00014400
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00014500
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00014600
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00014700
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00014800
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00014900
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00015000
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00015100
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00015200
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00015300
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00015400
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00015500
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00015600
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00015700
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00015800
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100015900
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00016000
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00016100
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00016200
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00016300
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00016400
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000016500
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00016600
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00016700
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00016800
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00016900
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00017000
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEA00017100
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",00017200
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"///),LAYT(//"EXECUTION TIM00017300
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///00017400
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=00017500
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAY00017600
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FE00017700
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.00017800
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];EN00017900
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THEN00018000
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,00018100
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       00018200
PROCEDURE      MPRINT (NROW, NCOL, R, ID) ;                             00018300
VALUE          NROW, NCOL ;                                             00018400
INTEGER        NROW, NCOL ;                                             00018500
REAL ARRAY     R[0,0] ;                                                 00018600
ALPHA          ID ;                                                     00018700
          BEGIN                                                         00018800
INTEGER ARRAY  II[0:NCOL] ;                                             00018900
INTEGER        J1, J2, JSEC ;                                           00019000
FORMAT         HEAD (/X3,A6,X2,"SECTION",I3//),                         00019100
               FI   (/X3,"ROW",X3,10I11) ,                              00019200
               FDATA(I6,X4,10R11.2) ;                                   00019300
LIST           LI   (FOR DX1 ~ J1 STEP 1 UNTIL J2 DO II[DX1]) ,         00019400
               LDATA(I,FOR DX1 ~ J1 STEP 1 UNTIL J2 DO R[I,DX1]) ;      00019500
LABEL          LSTART;                                                  00019600
               J1 ~ 0 ;                                                 00019700
               J2 ~ 0 ;                                                 00019800
               JSEC ~ 0 ;                                               00019900
               FOR J ~ 1 STEP 1 UNTIL NCOL DO                           00020000
               II[J] ~ J ;                                              00020100
LSTART:        J1 ~ J2 + 1 ;                                            00020200
               J2 ~ J1 + 9 ;                                            00020300
               IF J2 > NCOL THEN                                        00020400
               J2 ~ NCOL ;                                              00020500
               JSEC ~ JSEC + 1 ;                                        00020600
               WRITE(LINE,HEAD,ID,JSEC) ;                               00020700
               WRITE(LINE,FI,LI) ;                                      00020800
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00020900
               WRITE(LINE,FDATA,LDATA) ;                                00021000
               IF J2 < NCOL THEN                                        00021100
          BEGIN                                                         00021200
               WRITE(LINE[PAGE]);                                       00021300
               GO TO LSTART ;                                           00021400
          END ;                                                         00021500
               WRITE(LINE[PAGE]) ;                                      00021600
          END  OF MPRINT ;                                              00021700
PROCEDURE      MPRINT1 (NROW, NCOL, R, ID, S);                          00021800
VALUE          NROW, NCOL, S;                                           00021900
INTEGER        NROW, NCOL, S;                                           00022000
REAL ARRAY     R[0,0];                                                  00022100
ALPHA          ID;                                                      00022200
          BEGIN                                                         00022300
INTEGER ARRAY  II[0:NCOL];                                              00022400
INTEGER        J1, J2, JSEC;                                            00022500
FORMAT         HEAD (/X3,A6,X2,"SECTION",I3//),                         00022600
               FI   (/X3,"ROW",X3,10I11),                               00022700
               FDATA(I6,X4,10R11.2);                                    00022800
LIST           LI  (FOR DX1 ~ J1 STEP 1 UNTIL J2 DO II[DX1]),           00022900
               LDATA(I,FOR DX1 ~ J1 STEP 1 UNTIL J2 DO R[I,DX1]);       00023000
LABEL          LSTART;                                                  00023100
               J1 ~ 0;                                                  00023200
               J2 ~ S - 1;                                              00023300
               JSEC ~ 0;                                                00023400
               FOR J ~ J2+1 STEP 1 UNTIL NCOL DO                        00023500
               II[J] ~ J;                                               00023600
LSTART:        J1 ~ J2+1;                                               00023700
               J2 ~ J1+9;                                               00023800
               IF J2 > NCOL THEN                                        00023900
               J2 ~ NCOL;                                               00024000
               JSEC ~ JSEC+1;                                           00024100
               WRITE(LINE,HEAD,ID,JSEC);                                00024200
               WRITE(LINE,FI,LI);                                       00024300
               FOR I ~ S STEP 1 UNTIL NROW DO                           00024400
               WRITE(LINE,FDATA,LDATA);                                 00024500
               IF J2 < NCOL THEN                                        00024600
          BEGIN                                                         00024700
               WRITE(LINE[PAGE]);                                       00024800
               GO TO LSTART;                                            00024900
          END;                                                          00025000
               WRITE(LINE[PAGE]);                                       00025100
          END  OF MPRINT1;                                              00025200
PROCEDURE      MPUNCH ( NROW, NCOL, R, ID, L, PROB) ;                   00025300
VALUE          NROW, NCOL, L ;                                          00025400
INTEGER        NROW, NCOL, L, PROB ;                                    00025500
REAL ARRAY     R[0,0] ;                                                 00025600
ALPHA          ID ;                                                     00025700
          BEGIN                                                         00025800
INTEGER        J1,J2,JSEC,L1 ;                                          00025900
FORMAT         FSMAL(I3,X1,A1,I3,I2,10R7.2) ,                           00026000
               FLRGE(I3,X1,A1,I3,I2,5R14.7) ;                           00026100
LIST           LDATA(PROB, ID, I, JSEC,FOR DX1 ~ J1 STEP 1 UNTIL J2 DO  00026200
                    R[I,DX1]) ;                                         00026300
LABEL          PSTART ;                                                 00026400
               IF L = 1 THEN                                            00026500
               L1 ~ 9 ELSE                                              00026600
               L1 ~ 4 ;                                                 00026700
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00026800
          BEGIN                                                         00026900
               J1 ~ 0 ;                                                 00027000
               J2 ~ 0 ;                                                 00027100
               JSEC ~ 0 ;                                               00027200
PSTART:        J1 ~ J2 + 1 ;                                            00027300
               J2 ~ J1 + L1 ;                                           00027400
               IF J2 > NCOL THEN                                        00027500
               J2 ~ NCOL ;                                              00027600
               JSEC ~ JSEC + 1 ;                                        00027700
               IF L = 1 THEN                                            00027800
               WRITE(PUNCH,FSMAL,LDATA)  ELSE                           00027900
               WRITE(PUNCH,FLRGE,LDATA) ;                               00028000
               IF J2 < NCOL THEN                                        00028100
               GO TO PSTART ;                                           00028200
          END ;                                                         00028300
          END  OF MPUNCH ;                                              00028400
PROCEDURE      MPUNCH1 (NROW,NCOL, R, ID, L, PROB, S);                  00028500
VALUE          NROW, NCOL, L, S;                                        00028600
INTEGER        NROW, NCOL, L, PROB, S;                                  00028700
REAL ARRAY     R[0,0];                                                  00028800
ALPHA          ID;                                                      00028900
          BEGIN                                                         00029000
INTEGER        J1,J2,JSEC,L1;                                           00029100
FORMAT         FSMAL(I3,X1,A1,I3,I2,10R7.2),                            00029200
               FLRGE(I3,X1,A1,I3,I2,5R14.7);                            00029300
LIST           LDATA(PROB, ID, I, JSEC, FOR DX1 ~ J1 STEP 1 UNTIL J2 DO 00029400
               R[I,DX1]);                                               00029500
LABEL          PSTART;                                                  00029600
               IF L = 1 THEN                                            00029700
               L1 ~ 9 ELSE                                              00029800
               L1 ~ 4;                                                  00029900
               FOR I ~ S STEP 1 UNTIL NROW DO                           00030000
          BEGIN                                                         00030100
               J1 ~ 0;                                                  00030200
               J2 ~ S - 1;                                              00030300
               JSEC ~ 0;                                                00030400
PSTART:        J1 ~ J2 + 1;                                             00030500
               J2 ~ J1 + L1;                                            00030600
               IF J2 > NCOL THEN                                        00030700
               J2 ~ NCOL;                                               00030800
               JSEC ~ JSEC + 1;                                         00030900
               IF L = 1 THEN                                            00031000
               WRITE(PUNCH,FSMAL,LDATA) ELSE                            00031100
               WRITE(PUNCH,FLRGE,LDATA);                                00031200
               IF J2 < NCOL THEN                                        00031300
               GO TO PSTART;                                            00031400
          END;                                                          00031500
          END  OF MPUNCH1;                                              00031600
PROCEDURE      MTAPE (NROW, NCOL, R, SWF) ;                             00031700
VALUE          NROW, NCOL ;                                             00031800
INTEGER        NROW, NCOL ;                                             00031900
REAL ARRAY     R[0,0] ;                                                 00032000
FILE OUT       SWF ;                                                    00032100
          BEGIN                                                         00032200
FORMAT         OKTL (100O) ;                                            00032300
LIST           LTAPE (FOR DX1 ~ 1 STEP 1 UNTIL NCOL DO R[I,DX1]) ;      00032400
               WRITE(SWF,OKTL,NROW,NCOL) ;                              00032500
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00032600
               WRITE(SWF,OKTL,LTAPE) ;                                  00032700
          END  OF MTAPE ;                                               00032800
PROCEDURE      MTAPE1 (NROW, NCOL, R, SWF, S);                          00032900
VALUE          NROW, NCOL, S;                                           00033000
INTEGER        NROW, NCOL, S;                                           00033100
REAL ARRAY     R[0,0];                                                  00033200
FILE OUT       SWF;                                                     00033300
          BEGIN                                                         00033400
FORMAT         OKTL (100O);                                             00033500
LIST           LTAPE(FOR DX1 ~ S STEP 1 UNTIL NCOL+S-1 DO R[I,DX1]);    00033600
               WRITE(SWF,OKTL,NROW,NCOL);                               00033700
               FOR I ~ S STEP 1 UNTIL NROW+S-1 DO                       00033800
               WRITE(SWF,OKTL,LTAPE);                                   00033900
          END  OF MTAPE1;                                               00034000
PROCEDURE      CORPRO;                                                  00034100
          BEGIN                                                         00034200
OWN REAL ARRAY R[0:M,0:M], SUMX[0:M], SD[0:M], DATA[0:M] ;              00034300
INTEGER ARRAY  ID[0:12] ;                                               00034400
LABEL          DUMMY ;                                                  00034500
FORMAT         FID  (13A6) ,                                            00034600
               TITLE(//X50,"CORRELATION ANALYSIS"//),                   00034700
               FOUT (//X10,"NUMBER OF VARIABLES= ",I6/                  00034800
                       X10,"NUMBER OF SUBJECTS = ",I6//),               00034900
               FRAW (//X41,"RAW SUMS OF SQUARES AND CROSS PRODUCTS"//), 00035000
               FDEV (//X38,"DEVIATION SUMS OF SQUARES AND CROSS ",      00035100
                           "PRODUCTS"//),                               00035200
               FVAR (//X47,"VARIANCE-COVARIANCE MATRIX"//),             00035300
               FCOR (//X51,"CORRELATION MATRIX"//),                     00035400
               FIPAR(14I1,I3,9I1);                                      00035500
LIST           LID  (FOR DX1 ~ 0 STEP 1 UNTIL 12 DO ID[DX1]),           00035600
               LIPAR(PR,P1,P2,T2,LIN2,P3,T3,LIN3,P4,T4,LIN4,P5,LIN5,INP,00035700
               PROB,LIN6,P6,T6,T7,T8,T9,CALC,LIN7,P7);                  00035800
PROCEDURE      MEANSANDSD ;                                             00035900
          BEGIN                                                         00036000
FILE IN        RAWMAT  2(2,100) ;                                       00036100
FORMAT         OK(100O) ;                                               00036200
FORMAT         FMEAN(//X10,"MEANS OF THIS GROUP"//),                    00036300
               FSDEV(//X10,"STANDARD DEVIATIONS OF THIS GROUP"//),      00036400
               FDATA(X5, 9(X2,R10.4)) ,                                 00036500
               FPUNC(7R10.4) ;                                          00036600
LIST           LMEAN(FOR DX1 ~ 1 STEP 1 UNTIL M DO SUMX[DX1]),          00036700
               LSDEV(FOR DX1 ~ 1 STEP 1 UNTIL M DO SD[DX1]),            00036800
               LDATA(FOR DX1 ~ 1 STEP 1 UNTIL M DO DATA[DX1]) ;         00036900
               FOR I ~ 1 STEP 1 UNTIL M DO                              00037000
          BEGIN                                                         00037100
               SUMX[I] ~ 0 ;                                            00037200
               FOR J ~ 1 STEP 1 UNTIL M DO                              00037300
               R[I,J] ~ 0 ;                                             00037400
          END ;                                                         00037500
               IF INP > 0 THEN                                          00037600
               READ(RAWMAT,OK,NCASES,M) ELSE                            00037700
          BEGIN                                                         00037800
               REINITIAL(FMTQ) ;                                        00037900
               OBJECTFMTGEN(FMTQ) ;                                     00038000
          END ;                                                         00038100
               FOR K ~ 1 STEP 1 UNTIL NCASES DO                         00038200
          BEGIN                                                         00038300
               IF INP > 0 THEN                                          00038400
               READ(RAWMAT,OK,LDATA) [FINIS]  ELSE                      00038500
               READ(CARD,FMTQ,LDATA)[FINIS] ;                           00038600
               FOR I ~ 1 STEP 1 UNTIL M DO                              00038700
          BEGIN                                                         00038800
               SUMX[I] ~ SUMX[I] + DATA[I] ;                            00038900
               FOR J ~ 1 STEP 1 UNTIL M DO                              00039000
               R[I,J] ~ R[I,J] + DATA[I] | DATA[J] ;                    00039100
          END ;                                                         00039200
          END ;                                                         00039300
               FOR I ~ 1 STEP 1 UNTIL M DO                              00039400
          BEGIN                                                         00039500
               SD[I] ~ R[I,I] - SUMX[I] * 2 / NCASES ;                  00039600
               SD[I] ~ SQRT(SD[I]/(NCASES - 1)) ;                       00039700
               SUMX[I] ~ SUMX[I] / NCASES ;                             00039800
          END ;                                                         00039900
               WRITE(LINE,FMEAN) ;                                      00040000
               WRITE(LINE,FDATA,LMEAN) ;                                00040100
               WRITE(LINE,FSDEV) ;                                      00040200
               WRITE(LINE,FDATA,LSDEV) ;                                00040300
               WRITE(LINE[PAGE]) ;                                      00040400
               IF P1 > 0 THEN                                           00040500
          BEGIN                                                         00040600
               WRITE(PUNCH,FMEAN) ;                                     00040700
               WRITE(PUNCH,FPUNC,LMEAN) ;                               00040800
               WRITE(PUNCH,FSDEV) ;                                     00040900
               WRITE(PUNCH,FPUNC,LSDEV) ;                               00041000
          END ;                                                         00041100
          END  OF MEANSANDSD ;                                          00041200
                                                                        00041300
PROCEDURE      DEVSUMSQ ;                                               00041400
          BEGIN                                                         00041500
               FOR I ~ 1 STEP 1 UNTIL M DO                              00041600
               FOR J ~ 1 STEP 1 UNTIL M DO                              00041700
               R[I,J] ~ R[I,J] - SUMX[I] | SUMX[J] | NCASES ;           00041800
          END  OF DEVSUMSQ ;                                            00041900
                                                                        00042000
PROCEDURE      VARCOVAR ;                                               00042100
          BEGIN                                                         00042200
               FOR I ~ 1 STEP 1 UNTIL M DO                              00042300
               FOR J ~ 1 STEP 1 UNTIL M DO                              00042400
               R[I,J] ~ R[I,J] /(NCASES - 1) ;                          00042500
          END  OF VARCOVAR ;                                            00042600
                                                                        00042700
PROCEDURE      CORRELMAT ;                                              00042800
          BEGIN                                                         00042900
               FOR I ~ 1 STEP 1 UNTIL M DO                              00043000
               FOR J ~ 1 STEP 1 UNTIL M DO                              00043100
               IF SD[I] = 0 OR SD[J] = 0 THEN R[I,J] ~ 0 ELSE           00043200
               R[I,J] ~ R[I,J] /(SD[I] | SD[J]);                        00043300
          END  OF CORRELMAT ;                                           00043400
COMMENT:       START OF CORPRO;                                         00043500
               WRITE(LINE,TITLE) ;                                      00043600
               READ(CARD,FID,LID)[FINIS] ;                              00043700
               READ(CARD,FIPAR,LIPAR)[FINIS] ;                          00043800
               T5 ~ 1;                                                  00043900
               WRITE(LINE,FID,LID) ;                                    00044000
               WRITE(LINE,FOUT,M,NCASES) ;                              00044100
               L ~ 0 ;                                                  00044200
               MEANSANDSD;                                              00044300
               IF PR > 0 THEN                                           00044400
               GO TO DUMMY ;                                            00044500
               IF P2 > 0 AND P2 < 3 THEN                                00044600
               MPUNCH (M,M,R,"B",P2,PROB) ;                             00044700
               IF T2 > 0 THEN                                           00044800
               MTAPE (M,M,R,SWFILE[L]) ;                                00044900
               IF LIN2 > 0 THEN                                         00045000
          BEGIN                                                         00045100
               WRITE(LINE,FRAW) ;                                       00045200
               MPRINT (M,M,R,"RAW SS") ;                                00045300
          END;                                                          00045400
               L ~ 1 ;                                                  00045500
               DEVSUMSQ ;                                               00045600
               IF P3 > 0 AND P3 < 3 THEN                                00045700
               MPUNCH (M,M,R,"D",P3,PROB) ;                             00045800
               IF T3 > 0 THEN                                           00045900
               MTAPE (M,M,R,SWFILE[L]) ;                                00046000
               IF LIN3 > 0 THEN                                         00046100
          BEGIN                                                         00046200
               WRITE(LINE,FDEV) ;                                       00046300
               MPRINT (M,M,R,"DEV SS") ;                                00046400
          END;                                                          00046500
               L ~ 2 ;                                                  00046600
               VARCOVAR ;                                               00046700
               IF P4 > 0 AND P4 < 3 THEN                                00046800
               MPUNCH (M,M,R,"C",P4,PROB) ;                             00046900
               IF T4 > 0 THEN                                           00047000
               MTAPE (M,M,R,SWFILE[L]) ;                                00047100
               IF LIN4 > 0 THEN                                         00047200
          BEGIN                                                         00047300
               WRITE(LINE,FVAR) ;                                       00047400
               MPRINT (M,M,R,"COVAR ") ;                                00047500
          END;                                                          00047600
               L ~ 3 ;                                                  00047700
               CORRELMAT ;                                              00047800
               IF P5 > 0 AND P5 < 3 THEN                                00047900
               MPUNCH (M,M,R,"R",P5,PROB) ;                             00048000
               IF T5 > 0 THEN                                           00048100
               MTAPE (M,M,R,SWFILE[L]) ;                                00048200
               REWIND(SWFILE[L]);                                       00048300
               IF LIN5 > 0 THEN                                         00048400
          BEGIN                                                         00048500
               WRITE(LINE,FCOR) ;                                       00048600
               MPRINT (M,M,R,"R MAT ") ;                                00048700
          END;                                                          00048800
DUMMY:    END  OF CORPRO;                                               00048900
               DATELINE("PARCOR");                                      00049000
               IF DATATYPE > 1 THEN GO TO HAVECOR;                      00049100
               CORPRO;                                                  00049200
               IF PR > 0 THEN GO TO FINIS;                              00049300
               GO TO TAPEIN;                                            00049400
HAVECOR:       READ(CARD,FID,LID);                                      00049500
               READ(CARD,FIPAR,LIPAR);                                  00049600
               WRITE(LINE,FID,LID);                                     00049700
               WRITE(LINE,FOUT,M,NCASES);                               00049800
               IF INDEVICE = 1 THEN GO TO TAPEIN;                       00049900
CARDIN:        REINITIAL(FMTQ);                                         00050000
               OBJECTFMTGEN(FMTQ);                                      00050100
               FOR I ~ 1 STEP 1 UNTIL M DO                              00050200
               READ(CARD,FMTQ,LDATA);                                   00050300
               GO TO STARTPAR;                                          00050400
TAPEIN:        READ(CORMAT,OK,M,M);                                     00050500
               FOR I ~ 1 STEP 1 UNTIL M DO                              00050600
               READ(CORMAT,OK,LDATA);                                   00050700
               CLOSE(CORMAT,PURGE);                                     00050800
STARTPAR:      S ~ 1;                                                   00050900
               K ~ 0;                                                   00051000
               FOR I ~ S STEP 1 UNTIL M DO                              00051100
               FOR J ~ S STEP 1 UNTIL M DO                              00051200
               VCOVAR[I,J] ~ CORMATT[I,J];                              00051300
REPEAT:        FOR I ~ S STEP 1 UNTIL M DO                              00051400
               FOR J ~ S STEP 1 UNTIL M DO                              00051500
               PCOVAR[I,J] ~ CORMATT[I,J];                              00051600
               S ~ S+1;                                                 00051700
               K ~ K+1;                                                 00051800
               IF CALC = 0 THEN GO TO PARCORREL;                        00051900
               FOR I ~ S STEP 1 UNTIL M DO                              00052000
               FOR J ~ S STEP 1 UNTIL M DO                              00052100
               VCOVAR[I,J] ~ VCOVAR[I,J] - VCOVAR[K,J] | VCOVAR[K,I];   00052200
               IF LIN7 > 0 THEN                                         00052300
               WRITE(LINE,PCVHEAD,K);                                   00052400
               MPRINT1(M,M,VCOVAR,"PCVMAT",S);                          00052500
               IF P7 > 0 AND P7 < 3 THEN                                00052600
               MPUNCH1(M,M,VCOVAR,"V",P7,PROB,S);                       00052700
PARCORREL:     FOR I ~ S STEP 1 UNTIL M DO                              00052800
               FOR J ~ S STEP 1 UNTIL M DO                              00052900
               PCOVAR[I,J] ~ PCOVAR[I,J] - PCOVAR[K,J] | PCOVAR[K,I];   00053000
               FOR I ~ S STEP 1 UNTIL M DO                              00053100
               PCOVAR[I,I] ~ SQRT(PCOVAR[I,I]);                         00053200
               FOR I ~ S STEP 1 UNTIL M DO                              00053300
               FOR J ~ S STEP 1 UNTIL M DO                              00053400
               IF PCOVAR[I,I] = 0 OR PCOVAR[J,J] = 0 THEN               00053500
               CORMATT[I,J] ~ 0 ELSE                                    00053600
               CORMATT[I,J] ~ PCOVAR[I,J] / (PCOVAR[I,I] | PCOVAR[J,J]);00053700
               FOR I ~ S STEP 1 UNTIL M DO                              00053800
               CORMATT[I,I] ~ CORMATT[I,I] | PCOVAR[I,I];               00053900
               IF LIN6 > 0 THEN                                         00054000
               WRITE(LINE,PCHEAD,K);                                    00054100
               MPRINT1 (M,M,CORMATT,"PC MAT",S);                        00054200
               IF P6 > 0 AND P6 < 3 THEN                                00054300
               MPUNCH1(M,M,CORMATT,"P",P6,PROB,S);                      00054400
               IF S > 5 THEN GO TO CHECK;                               00054500
               L ~ 0;                                                   00054600
               IF T6 = 1 AND S = 2 THEN                                 00054700
               MTAPE1 (M-S+1,M-S+1,CORMATT,SWFILE[L],S);                00054800
               L ~ 1;                                                   00054900
               IF T7 = 1 AND S = 3 THEN                                 00055000
               MTAPE1 (M-S+1,M-S+1,CORMATT,SWFILE[L],S);                00055100
               L ~ 2;                                                   00055200
               IF T8 = 1 AND S = 4 THEN                                 00055300
               MTAPE1 (M-S+1,M-S+1,CORMATT,SWFILE[L],S);                00055400
               L ~ 3;                                                   00055500
               IF T9 = 1 AND S = 5 THEN                                 00055600
               MTAPE1 (M-S+1,M-S+1,CORMATT,SWFILE[L],S);                00055700
CHECK:         IF K < NPARS THEN GO TO REPEAT;                          00055800
FINIS:         DATELINE(0);                                             00055900
          END;                                                          00056000
FINIS:    END  OF PARCOR.                                               00056100
                LAST CARD ON CRDIMG TAPE                                99999999
