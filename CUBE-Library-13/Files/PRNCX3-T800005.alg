          BEGIN                                                         00000000
COMMENT        PRNCX3/STATMAN;                                          00000100
COMMENT        BASIC STRUCTURE SUCCESSIVE FACTOR METHODS                00000200
               SOLUTION WITHOUT RESIDUALS: FROM A FORTRAN II PROGRAM    00000300
               BY PAUL HORST   TRANSLATION BY JOHN WACKWITZ 5/9/66   ;  00000400
FILE IN CARD 0(2,10);     FILE OUT  LINE 4(2,15);                       00000500
FILE OUT PUNCH 0(2,10);                                                 00000600
FILE IN        CORMAT 2(2,100) ;                                        00000700
INTEGER        N, LIQ, LFQ,  M, WRT, WRC, INP,I ;                       00000800
REAL               P,RN,RA;                                             00000900
FORMAT IN      FORM1(10A6/3I4,3R8.5,3I1) ;                              00001000
INTEGER ARRAY  ID[0:9];                                                 00001100
FORMAT OUT     TITLE(//X34,"PRINCIPAL AXIS FACTOR ANALYSIS: ",          00001200
                        "SUCCESSIVE  SOLUTION"//),                      00001300
               CASE (//X30,10A6//X10,"EIGENVALUES EXTRACTED TO:  ",R6.3/00001400
                X10,"CASE:   ",I5  /X10,"NUMBER OF VARIABLES:",I5),     00001500
               EVAL (//X54,"EIGENVALUES"/(10R12.5)/),                   00001600
               FACT1(//X53,"FACTOR MATRIX"//),                          00001700
               FACT2(/X1,"VARIABLE",I3/(10R12.5)/),                     00001800
               ITTR(//X45,"NUMBER OF  ITERATIONS REQUIRED"/(10I12)/);   00001900
FORMAT         DATA(/////////////////////////////////////////////////   00002000
                    /////////////////////////////////////////////////   00002100
                    /////////////////////////////////////////////////   00002200
                    /////////////////////////////////////////////////); 00002300
FORMAT         FCM(100O) ;                                              00002400
FORMAT         EVEC1(X54,"EIGENVECTORS"//);                             00002500
FORMAT         EVEC2(/X1,"VECTOR",I3/(10R12.5)/);                       00002600
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
INTEGER        AR , EX ;                                                00015800
LABEL          LL1 ;                                                    00015900
LIST           HEAD(FOR I ~ 0 STEP 1 UNTIL 9 DO ID[I],N,LIQ,LFQ,P,      00016000
                    RN,RA,WRT,WRC,INP) ;                                00016100
               READ(CARD,FORM1,HEAD) [FINIS] ;                          00016200
               IF INP>1 THEN READ(CORMAT,FCM,N,N);                      00016300
               WRITE(LINE[PAGE]) ;                                      00016400
               DATELINE("PRNCX3") ;                                     00016500
               WRITE(LINE,TITLE) ;                                      00016600
               M ~ (N | (N+1))/2 ;                                      00016700
               EX ~ LFQ ;                                               00016800
               IF RA > 0 THEN AR ~ 1 ELSE AR ~ 0 ;                      00016900
LL1:      BEGIN                                                         00017000
FORMAT         FEIG(///"********",I6," TH APROX. OF THE",I6," TH EIGENV"00017100
               ,"ALUE WAS <0"///);                                      00017200
REAL ARRAY     R[0:M,[256]], A[0:EX,0:N], U[0:N], W[0:EX], EVQ[0:EX] ;  00017300
INTEGER ARRAY  KVQ[0:EX] ;                                              00017400
INTEGER  DX1;                                                           00017500
INTEGER        L, K, L1Q, J, IJQ, I1Q, Z ;                              00017600
REAL           E, S, E1Q ;                                              00017700
LIST LIST1  (FOR DX1 ~ 1 STEP 1 UNTIL N DO U[DX1]),                     00017800
     LIST2(FOR DX1 ~ 1 STEP 1 UNTIL M DO R[DX1]),                       00017900
     LIST3(I,FOR DX1~1 STEP 1 UNTIL L DO A[DX1,I]),                     00018000
     LIST4(FOR DX1 ~ 1 STEP 1 UNTIL L DO KVQ[DX1]),                     00018100
     LIST5(FOR DX1 ~ 1 STEP 1 UNTIL L DO EVQ[DX1]);                     00018200
LIST LIST6(L,FOR DX1 ~ 1 STEP 1 UNTIL N DO U[DX1]);                     00018300
LIST LIST7(FOR I ~ 0 STEP 1 UNTIL 9 DO ID[I],RN,AR,N) ;                 00018400
LABEL L9,L24,L27,L47,L51,L28;                                           00018500
               IF INP < 2 THEN BEGIN                                    00018600
               OBJECTFMTGEN(DATA) ;                                     00018700
               WRITE(LINE[DBL])  END ;                                  00018800
               IF INP > 0 THEN                                          00018900
          BEGIN                                                         00019000
               FOR I ~ 1 STEP 1 UNTIL N DO                              00019100
          BEGIN                                                         00019200
               IF INP > 1 THEN  READ(CORMAT,FCM,LIST1)  ELSE            00019300
               READ(CARD,DATA,LIST1);                                   00019400
               FOR J ~ I STEP 1 UNTIL N DO                              00019500
          BEGIN                                                         00019600
               Z ~ Z + 1 ;                                              00019700
               R[Z] ~ U[J] ;                                            00019800
          END ;                                                         00019900
          END ;                                                         00020000
          END  ELSE                                                     00020100
               READ(CARD,DATA,LIST2) ;                                  00020200
               IF INP=2 THEN CLOSE(CORMAT,RELEASE) ELSE CLOSE(CARD,     00020300
               RELEASE);                                                00020400
               WRITE(LINE,CASE,LIST7);                                  00020500
          E ~ 0;                                                        00020600
          L ~ 0;                                                        00020700
L9:       IF L=LFQ THEN GO TO L51;                                      00020800
          L ~ L+1;                                                      00020900
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00021000
            A[L,I] ~ 1  END;                                            00021100
          FOR K ~ 1, K+1 STEP 1 UNTIL LIQ DO BEGIN                      00021200
            IF L{1 THEN GO TO L24;                                      00021300
            L1Q ~ L-1;                                                  00021400
            FOR J ~ 1, J+1 STEP 1 UNTIL L1Q DO BEGIN                    00021500
              W[J] ~ 0;                                                 00021600
              FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                    00021700
                W[J] ~ W[J]+A[J,I]|A[L,I]  END;                         00021800
              END;                                                      00021900
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00022000
              U[I] ~ 0;                                                 00022100
              FOR J ~ 1, J+1 STEP 1 UNTIL L1Q DO BEGIN                  00022200
                U[I] ~ U[I]-A[J,I]|W[J]  END;                           00022300
              END;                                                      00022400
L24:        FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00022500
              IF L>1 THEN GO TO L27;                                    00022600
              U[I] ~ 0;                                                 00022700
L27:          FOR J ~ 1, J+1 STEP 1 UNTIL I DO BEGIN                    00022800
                IJQ ~ I+((J-1)|(N|2-J)) DIV 2;                          00022900
                U[I] ~ U[I]+R[IJQ]|A[L,J]  END;                         00023000
              I1Q ~ I+1;                                                00023100
               IF I1Q >N THEN GO TO L28;                                00023200
              FOR J ~ I1Q, J+1 STEP 1 UNTIL N DO BEGIN                  00023300
                IJQ ~ ((I-1)|(N|2-I)) DIV 2+J;                          00023400
                U[I] ~ U[I]+R[IJQ]|A[L,J]  END;                         00023500
L28:          END;                                                      00023600
            S ~ 0;                                                      00023700
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00023800
              S ~ S+U[I]|A[L,I]  END;                                   00023900
               IF S < 0 THEN                                            00024000
          BEGIN                                                         00024100
               WRITE(LINE,FEIG,K,L);                                    00024200
               GO TO L47 ;                                              00024300
          END ;                                                         00024400
            S ~ 1/SQRT(S);                                              00024500
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00024600
              A[L,I] ~ U[I]|S  END;                                     00024700
            S ~ 0;                                                      00024800
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00024900
              S ~ S+A[L,I]*2  END;                                      00025000
            E1Q ~ E;                                                    00025100
            E ~ SQRT(S);                                                00025200
            IF (ABS(E1Q/E-1))<P THEN GO TO L47;                         00025300
            END;                                                        00025400
L47:           IF S { RN THEN                                           00025500
          BEGIN                                                         00025600
               L ~ L - 1;                                               00025700
               GO TO L51 ;                                              00025800
          END ;                                                         00025900
          KVQ[L] ~ K;                                                   00026000
          EVQ[L] ~ S;                                                   00026100
               IF L=1 THEN WRITE(LINE,EVEC1);                           00026200
               FOR I ~ 1 STEP 1 UNTIL N DO                              00026300
               U[I] ~ A[L,I] / E ;                                      00026400
               WRITE(LINE,EVEC2,LIST6);                                 00026500
          GO TO L9;                                                     00026600
L51:           WRITE(LINE,FACT1);                                       00026700
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00026800
            WRITE(LINE,FACT2,LIST3);                                    00026900
            END;                                                        00027000
          WRITE(LINE,ITTR,LIST4);                                       00027100
          WRITE(LINE,EVAL,LIST5);                                       00027200
               IF WRT > 0 OR WRC > 0 THEN                               00027300
          BEGIN                                                         00027400
COMMENT        WRITE FACTOR MATRIX ON TAPE AND CARDS;                   00027500
FILE OUT       FACMAT 2(2,100, SAVE 10) ;                               00027600
INTEGER        J1Q,J2Q,JSECQ;                                           00027700
FORMAT         OKTL(100O),                                              00027800
               PUNC(2I4,X2,10F7.4);                                     00027900
LIST       FACP( I,JSECQ,FOR DX1 ~ J1Q STEP 1 UNTIL J2Q DO A[DX1,I]),   00028000
            TAPO(FOR DX1 ~ 1 STEP 1 UNTIL L  DO A[DX1,I]);              00028100
LABEL          L77,L78,L79;                                             00028200
               IF WRT > 0 THEN BEGIN                                    00028300
               WRITE(FACMAT,OKTL,N,L) ;                                 00028400
               FOR I ~  1  STEP 1 UNTIL N   DO                          00028500
               WRITE(FACMAT,OKTL,TAPO) ;                                00028600
               REWIND(FACMAT) ;                                         00028700
          END ;                                                         00028800
               IF WRC >0 THEN  BEGIN                                    00028900
L77:           FOR I ~  1  STEP 1 UNTIL N   DO                          00029000
          BEGIN                                                         00029100
               J1Q ~ 0;                                                 00029200
               J2Q ~ 0;                                                 00029300
               JSECQ ~ 0;                                               00029400
L78:           J1Q ~ J2Q+1;                                             00029500
               J2Q ~ J1Q+9;                                             00029600
               IF J2Q { L    THEN GO TO L79;                            00029700
               J2Q ~ L;                                                 00029800
L79:           JSECQ ~ JSECQ+1;                                         00029900
               WRITE(PUNCH,PUNC,FACP);                                  00030000
               IF J2Q <  L   THEN GO TO L78;                            00030100
          END;                                                          00030200
          END;                                                          00030300
          END;                                                          00030400
          END;                                                          00030500
          END;                                                          00030600
                                                                        00030700
COMMENT **************************  OUTER BLOCK  ************;          00030800
  XPR~QQ~0;                                                             00030900
FINIS:         DATELINE(0);                                             00031000
          END OF PROGRAM PRNCX3.                                        00031100
                LAST CARD ON CRDIMG TAPE                                99999999
