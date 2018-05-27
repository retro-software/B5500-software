         BEGIN                                                          00000000
COMMENT        VARMAX/STATMAN;                                          00000100
COMMENT        VARIMAX ROTATION OF A FACTOR MATRIX: FROM CLARKS FACVAR  00000200
               JOHN H WACKWITZ   4/20/66  ;                             00000300
INTEGER       I,NVAR,Z,QZ,CAS,NRO,WRT,WRC;                              00000400
INTEGER        SFMT;                                                    00000500
INTEGER ARRAY ID[0:4] ;                                                 00000600
REAL          LIM ;                                                     00000700
LABEL         START,NOMO,LL1 ;                                          00000800
FORMAT IN     FORM1 (5A6,3I3,F5.2,I3,4I1);                              00000900
FORMAT         CHKFOR (A6);                                             00001000
FORMAT         OKTL(100O);                                              00001100
FORMAT         NOFRM ("ERROR: NO FORMAT SPECIFICATIONS FOR THIS RUN"//);00001200
FORMAT         ERRCNT ("ERROR: WRONG N INDICATED ON HEADER CARD" //);   00001300
LABEL          WRONGCOUNT;                                              00001400
FORMAT         SWF (//////////////////////////////////////////////////  00001500
                    //////////////////////////////////////////////////  00001600
                    //////////////////////////////////////////////////  00001700
                    //////////////////////////////////////////////////);00001800
FORMAT OUT    TITLE(X41,"VARIMAX ROTATION OF A FACTOR ANALYSIS"///),    00001900
              FRM1 (X45,5A6//X1,"CASE",I3/X1,"NUMBER OF VARIABLES",I4/  00002000
                    X1,"NUMBER OF FACTORS",I6),                         00002100
              FRM6 (/X1,"ROW",I3/(10F12.5)/),                           00002200
              FRM12(//X1,"FACTOR MATRIX"),                              00002300
              FRM13(/X1,"VARIABLE",I3/(10F12.5)/),                      00002400
              FRM16(//X1,"VALUE GIVEN TO LIMIT NUMBER OF FACTORS TO BE",00002500
                    " ROTATED",F9.4/),                                  00002600
              FRM17(//X1,"FACTOR MATRIX CAN NOT BE ROTATED, CHANGE THE",00002700
                    " LIMIT VALUE LIM"/),                               00002800
              FRM18(/X1,"NUMBER OF FACTORS ROTATED",I4/X1,"NUMBER OF ", 00002900
                    "ITERATION CYCLES",I3///X1,"ROTATED FACTOR MATRIX"),00003000
              FRM19(/X1,"ORIGINAL AND SUCCESSIVE VARIANCES"//X3,"CYCLE",00003100
                    " NO.",X10,"VARIANCES"),                            00003200
              FRM20(I8,X8,F15.7),                                       00003300
              FRM21(//X1,"CHECK ON COMMUNALITIES"//X3,"VARIABLES",X8,   00003400
                    "ORIGINAL",X9,"FINAL",X8,"DIFFERENCE"),             00003500
              FRM22(I8,X5,3F15.5) ;                                     00003600
FILE IN        CARD(2,10);                                              00003700
FILE IN        FACMAT 2(2,100);                                         00003800
DEFINE         CARDR=CARD#;                                             00003900
FILE OUT       PRINT  4(2,15);                                          00004000
DEFINE         LINE=PRINT#;                                             00004100
FILE OUT       PUNCH 0(2,10);                                           00004200
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00004300
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00004400
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00004500
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00004600
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00004700
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00004800
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00004900
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00005000
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00005100
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00005200
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00005300
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00005400
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00005500
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00005600
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00005700
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00005800
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00005900
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00006000
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300006100
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00006200
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00006300
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00006400
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00006500
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00006600
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00006700
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00006800
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00006900
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100007000
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00007100
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00007200
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00007300
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00007400
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00007500
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00007600
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00007700
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00007800
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00007900
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00008000
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00008100
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00008200
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00008300
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00008400
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00008500
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00008600
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00008700
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00008800
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00008900
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00009000
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00009100
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00009200
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00009300
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00009400
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00009500
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00009600
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00009700
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800009800
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00009900
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00010000
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00010100
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00010200
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00010300
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00010400
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00010500
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00010600
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00010700
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00010800
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00010900
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00011000
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00011100
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00011200
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00011300
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00011400
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00011500
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00011600
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00011700
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00011800
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00011900
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00012000
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00012100
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00012200
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00012300
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00012400
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00012500
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00012600
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00012700
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00012800
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00012900
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00013000
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00013100
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00013200
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00013300
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00013400
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00013500
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100013600
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00013700
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00013800
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00013900
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00014000
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00014100
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000014200
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00014300
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00014400
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00014500
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00014600
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00014700
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEA00014800
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",00014900
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"///),LAYT(//"EXECUTION TIM00015000
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///00015100
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=00015200
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAY00015300
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FE00015400
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.00015500
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];EN00015600
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THEN00015700
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,00015800
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       00015900
START:   BEGIN                                                          00016000
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NVAR,Z,CAS,        00016100
                    LIM,NRO,SFMT,QZ,WRT,WRC);                           00016200
              READ (CARDR,FORM1,HEAD) [NOMO] ;                          00016300
              WRITE(PRINT[PAGE]) ;                                      00016400
               DATELINE("VARMAX");                                      00016500
              WRITE(PRINT,TITLE);                                       00016600
               IF  QZ ! 1  THEN                                         00016700
          BEGIN                                                         00016800
               WRITE(LINE[DBL]);                                        00016900
               READ(CARD[NO], CHKFOR, SFMT)[NOMO];                      00017000
               IF SFMT ! "FORMAT"  THEN                                 00017100
          BEGIN                                                         00017200
               WRITE(LINE, NOFRM);                                      00017300
               GO TO NOMO;                                              00017400
          END  ELSE  OBJECTFMTGEN(SWF);                                 00017500
               WRITE(LINE[DBL]);                                        00017600
          END  ELSE READ(FACMAT,OKTL,NVAR,Z) ;                          00017700
         END ;                                                          00017800
LL1:     BEGIN                                                          00017900
INTEGER       EX ;                                                      00018000
REAL          NS,NSM1,RN ;                                              00018100
LABEL         LL2 ;                                                     00018200
              EX ~ NVAR + 50 ;                                          00018300
LL2:     BEGIN                                                          00018400
INTEGER       J,JJ,K,KK,L,LL,II,M,MM;                                   00018500
REAL          NORM,T1,T2,SYN,KOS,P,TA,TE,BE,W,S,MARY,B ;                00018600
REAL ARRAY    A[0:NVAR,0:Z],   DATA[0:EX],                              00018700
                                               SUMX[0:NVAR] ;           00018800
LABEL          LL25,LL26,LL27,LL35;                                     00018900
LIST          OUTT(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],CAS,NVAR,Z),       00019000
              FACT(I,FOR J ~ 1 STEP 1 UNTIL Z DO A[I,J]),               00019100
              FACTIN(FOR J ~ 1 STEP 1 UNTIL Z DO A[I,J]),               00019200
              ROTM(I,FOR J ~ 1 STEP 1 UNTIL NRO DO A[I,J]);             00019300
              WRITE(PRINT,FRM1,OUTT) ;                                  00019400
               IF QZ > 0 THEN                                           00019500
          BEGIN                                                         00019600
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00019700
               READ(FACMAT,OKTL,FACTIN);                                00019800
          END  ELSE                                                     00019900
          BEGIN                                                         00020000
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00020100
               READ(CARD,SWF,FACTIN);                                   00020200
          END;                                                          00020300
              WRITE(PRINT,FRM12) ;                                      00020400
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00020500
              WRITE(PRINT,FRM13,FACT) ;                                 00020600
              WRITE(PRINT,FRM16,LIM) ;                                  00020700
              IF LIM = 0 THEN                                           00020800
              GO TO LL26 ;                                              00020900
              FOR J ~ 1 STEP 1 UNTIL Z DO                               00021000
         BEGIN                                                          00021100
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00021200
         BEGIN                                                          00021300
              W ~ ABS(A[I,J]) ;                                         00021400
              IF W } LIM THEN                                           00021500
              GO TO LL25                                                00021600
         END ;                                                          00021700
               RN ~ J-1 ;                                               00021800
              GO TO LL27 ;                                              00021900
LL25:    END ;                                                          00022000
LL26:          RN ~ Z ;                                                 00022100
LL27:         IF (NRO=0) OR (NRO>RN) THEN                               00022200
              NRO ~ RN;                                                 00022300
              IF NRO { 1 THEN                                           00022400
         BEGIN                                                          00022500
              WRITE(PRINT,FRM17) ;                                      00022600
              GO TO START                                               00022700
         END ;                                                          00022800
COMMENT       ROTATE FACTOR MATRIX ;                                    00022900
LL35:    BEGIN                                                          00023000
REAL          TN4P,ST,CT,S2T,C2T,S4T,C4T,TN4T,CN4T ;                    00023100
LABEL         LL28,LL29,LL30,LL31,LL32 ;                                00023200
              T1 ~ 0.00116 ;                                            00023300
              JJ ~ 3 ;                                                  00023400
              P ~ 1.0 ;                                                 00023500
              KK ~ 0 ;                                                  00023600
              MM ~ (NRO|(NRO-1))/2 ;                                    00023700
              DATA[1] ~ 0.0 ;                                           00023800
              LL ~ NRO - 1 ;                                            00023900
              II ~ 1 ;                                                  00024000
              EX ~ 0 ;                                                  00024100
              T2 ~ NVAR ;                                               00024200
              TA ~ T2*2 ;                                               00024300
              W ~ 1.0 / SQRT(2.0) ;                                     00024400
              NS ~ 0.0001 ;                                             00024500
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00024600
              SUMX[I] ~ 0.0 ;                                           00024700
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00024800
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00024900
              SUMX[I] ~ SUMX[I] + A[I,J]|A[I,J] ;                       00025000
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00025100
         BEGIN                                                          00025200
              SUMX[I] ~ SQRT(SUMX[I]) ;                                 00025300
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00025400
              A[I,J] ~ A[I,J] / SUMX[I]                                 00025500
         END ;                                                          00025600
LL29:         II ~ II + 1 ;                                             00025700
              DATA[II] ~ 0.0 ;                                          00025800
              Z ~ II - 1 ;                                              00025900
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00026000
         BEGIN                                                          00026100
              RN ~ 0.0 ;                                                00026200
              BE ~ 0.0 ;                                                00026300
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00026400
         BEGIN                                                          00026500
              S ~ A[I,J]|A[I,J] ;                                       00026600
              RN ~ RN + S ;                                             00026700
              BE ~ BE + S|S                                             00026800
         END ;                                                          00026900
              DATA[II] ~ DATA[II] + (T2|BE - RN|RN)/TA                  00027000
         END ;                                                          00027100
              IF II } 50 THEN                                           00027200
              GO TO LL30 ;                                              00027300
              MARY ~ ABS(DATA[II] - DATA[Z]) - 1.0@-7 ;                 00027400
              IF MARY { 0 THEN                                          00027500
         BEGIN                                                          00027600
              EX ~ EX + 1 ;                                             00027700
              IF EX > JJ THEN                                           00027800
              GO TO LL30                                                00027900
         END ;                                                          00028000
              FOR J ~ 1 STEP 1 UNTIL LL DO                              00028100
         BEGIN                                                          00028200
              L ~ J + 1 ;                                               00028300
              FOR K ~ L STEP 1 UNTIL NRO DO                             00028400
         BEGIN                                                          00028500
              IF KK } MM THEN                                           00028600
              GO TO LL31 ;                                              00028700
              IF ABS(P) > NS THEN                                       00028800
              KK ~ 0 ;                                                  00028900
              RN ~ 0.0 ;                                                00029000
              BE ~ 0.0 ;                                                00029100
              S ~ 0.0 ;                                                 00029200
              NORM ~ 0.0 ;                                              00029300
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00029400
         BEGIN                                                          00029500
              MARY ~ (A[I,J] + A[I,K])|(A[I,J] - A[I,K]) ;              00029600
              TE ~ A[I,J]|A[I,K] ;                                      00029700
              TE ~ TE + TE ;                                            00029800
              S ~ S + (MARY + TE)|(MARY - TE) ;                         00029900
              NORM ~ NORM + 2.0|MARY|TE ;                               00030000
              RN ~ RN + MARY ;                                          00030100
              BE ~ BE + TE                                              00030200
         END ;                                                          00030300
              TE ~ NORM - 2.0|RN|BE/T2 ;                                00030400
              B ~ S - (RN|RN - BE|BE)/T2 ;                              00030500
              P ~ 0.25|ARCTAN(TE/B) ;                                   00030600
              IF ABS(P) { NS THEN                                       00030700
         BEGIN                                                          00030800
              KK ~ KK + 1 ;                                             00030900
              GO TO LL31                                                00031000
         END ;                                                          00031100
              TN4P ~ TE/B ;                                             00031200
              IF TE = B THEN                                            00031300
         BEGIN                                                          00031400
              IF TE + B < T1 THEN                                       00031500
              GO TO LL31 ;                                              00031600
              C4T ~ W ;                                                 00031700
              S4T ~ W ;                                                 00031800
              GO TO LL28                                                00031900
         END                                                            00032000
              ELSE IF TE < B THEN                                       00032100
         BEGIN                                                          00032200
              TN4T ~ ABS(TE)/ABS(B) ;                                   00032300
              IF TN4T } T1 THEN                                         00032400
         BEGIN                                                          00032500
              C4T ~ 1.0 / SQRT(1.0 + TN4T*2) ;                          00032600
              S4T ~ TN4T|C4T ;                                          00032700
              GO TO LL28                                                00032800
         END ;                                                          00032900
              IF B } 0 THEN                                             00033000
              GO TO LL31 ;                                              00033100
              SYN ~ W ;                                                 00033200
              KOS ~ W ;                                                 00033300
              GO TO LL32                                                00033400
         END                                                            00033500
              ELSE                                                      00033600
         BEGIN                                                          00033700
              CN4T ~ ABS(TE/B) ;                                        00033800
              IF CN4T } T1 THEN                                         00033900
         BEGIN                                                          00034000
              S4T ~ 1.0 / SQRT(1.0 + CN4T*2) ;                          00034100
              C4T ~ CN4T|S4T ;                                          00034200
              GO TO LL28                                                00034300
         END ;                                                          00034400
              C4T ~ 0.0 ;                                               00034500
              S4T ~ 1.0                                                 00034600
         END ;                                                          00034700
LL28:         C2T ~ SQRT((1.0 + C4T)/2.0) ;                             00034800
              S2T ~ S4T/(2.0|C2T) ;                                     00034900
              CT ~ SQRT((1.0 + C2T)/2.0) ;                              00035000
              ST ~ S2T/(2.0|CT) ;                                       00035100
              IF B > 0 THEN                                             00035200
         BEGIN                                                          00035300
              KOS ~ CT ;                                                00035400
              SYN ~ ST                                                  00035500
         END                                                            00035600
              ELSE                                                      00035700
         BEGIN                                                          00035800
              KOS ~ W|CT + W|ST ;                                       00035900
              SYN ~ ABS(W|CT - W|ST)                                    00036000
         END ;                                                          00036100
              IF TE { 0 THEN                                            00036200
              SYN ~ -SYN ;                                              00036300
LL32:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00036400
         BEGIN                                                          00036500
              RN ~ A[I,J]|KOS + A[I,K]|SYN ;                            00036600
              BE ~ -A[I,J]|SYN + A[I,K]|KOS ;                           00036700
              A[I,J] ~ RN ;                                             00036800
              A[I,K] ~ BE                                               00036900
         END                                                            00037000
         END ;                                                          00037100
LL31:    END ;                                                          00037200
              GO TO LL29 ;                                              00037300
LL30:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00037400
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00037500
              A[I,J] ~ A[I,J]|SUMX[I] ;                                 00037600
              EX ~ II - 2 ;                                             00037700
              WRITE(PRINT,FRM18,NRO,EX) ;                               00037800
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00037900
              WRITE(PRINT,FRM13,ROTM) ;                                 00038000
              WRITE(PRINT,FRM19) ;                                      00038100
              FOR I ~ 2 STEP 1 UNTIL II DO                              00038200
         BEGIN                                                          00038300
              EX ~ I - 2 ;                                              00038400
              WRITE(PRINT,FRM20,EX,DATA[I])                             00038500
         END ;                                                          00038600
              WRITE(PRINT,FRM21) ;                                      00038700
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            00038800
         BEGIN                                                          00038900
              SUMX[I] ~ SUMX[I]|SUMX[I] ;                               00039000
              BE ~ 0.0 ;                                                00039100
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             00039200
              BE ~ BE + A[I,J]|A[I,J] ;                                 00039300
              RN ~ BE - SUMX[I] ;                                       00039400
              WRITE(PRINT,FRM22,I,SUMX[I],BE,RN)                        00039500
          END;                                                          00039600
               IF WRT > 0 OR WRC > 0 THEN                               00039700
          BEGIN                                                         00039800
COMMENT        WRITE ROTATED FACTOR MATRIX ON TAPE AND CARDS;           00039900
FILE OUT       ROTMAT 2(2,100, SAVE 10) ;                               00040000
INTEGER        J1Q,J2Q,JSECQ;                                           00040100
FORMAT         OKTL(100O),                                              00040200
               PUNC(2I4,X2,10(X1,F6.4));                                00040300
LIST           FACP(I,JSECQ,FOR J ~ J1Q STEP 1 UNTIL J2Q DO A[I,J]),    00040400
               TAPO(FOR J ~ 1 STEP 1 UNTIL NRO DO A[I,J]);              00040500
LABEL          L77,L78,L79;                                             00040600
               IF WRT > 0 THEN BEGIN                                    00040700
               WRITE(ROTMAT,OKTL,NVAR,NRO) ;                            00040800
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00040900
               WRITE(ROTMAT,OKTL,TAPO) ;                                00041000
               REWIND(ROTMAT) ;                                         00041100
          END ;                                                         00041200
               IF WRC >0 THEN  BEGIN                                    00041300
L77:           FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00041400
          BEGIN                                                         00041500
               J1Q ~ 0;                                                 00041600
               J2Q ~ 0;                                                 00041700
               JSECQ ~ 0;                                               00041800
L78:           J1Q ~ J2Q+1;                                             00041900
               J2Q ~ J1Q+9;                                             00042000
               IF J2Q { NRO  THEN GO TO L79;                            00042100
               J2Q ~ NRO;                                               00042200
L79:           JSECQ ~ JSECQ+1;                                         00042300
               WRITE(PUNCH,PUNC,FACP);                                  00042400
               IF J2Q < NRO THEN GO TO L78;                             00042500
          END END;                                                      00042600
         END                                                            00042700
         END                                                            00042800
         END ;                                                          00042900
          END;                                                          00043000
              GO TO START ;                                             00043100
WRONGCOUNT:    WRITE(LINE, ERRCNT);                                     00043200
NOMO:          DATELINE(0);                                             00043300
          END.                                                          00043400
                LAST CARD ON CRDIMG TAPE                                99999999
