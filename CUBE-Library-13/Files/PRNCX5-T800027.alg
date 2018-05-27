          BEGIN                                                         00000100
COMMENT        BASIC STRUCTURE SUCCESSIVE FACTOR METHODS                00000200
               SOLUTION WITHOUT RESIDUALS: FROM A FORTRAN II PROGRAM    00000300
               BY PAUL HORST   TRANSLATION BY JOHN WACKWITZ 5/9/66      00000400
               ITERATIONS FOR COMMUNALITIES OPTION BY MARK FOSTER;      00000500
FILE IN CARD 0(2,10);     FILE OUT  LINE 4(2,15);                       00000600
FILE OUT PUNCH 0(2,10);                                                 00000700
FILE IN        CORMAT 2(2,100) ;                                        00000800
INTEGER        N, LIQ, LFQ,  M, WRT, WRC, INP,I,COM,RAT,Z1,FO2,FO3;     00000900
REAL           TOL,P,RN,RA;                                             00001000
FORMAT IN      FORM1(10A6/3I4,3R8.5,4I1,I3,R8.5,I1);                    00001100
INTEGER ARRAY  ID[0:9];                                                 00001200
FORMAT OUT     TITLE(//X34,"PRINCIPAL AXIS FACTOR ANALYSIS: ",          00001300
                        "SUCCESSIVE  SOLUTION"/                         00001400
               X34,"WITH ITERATIVE OPTION FOR COMMUNALITIES"//),        00001500
               CASE (//X30,10A6//X10,"EIGENVALUES EXTRACTED TO:  ",R6.3/00001600
                X10,"CASE:   ",I5  /X10,"NUMBER OF VARIABLES:",I5),     00001700
               EVAL (//X54,"EIGENVALUES"/(10R12.5)/),                   00001800
               FACT1(//X53,"FACTOR MATRIX"//),                          00001900
               FACT2(/X1,"VARIABLE",I3/(10R12.5)/),                     00002000
               ITTR(//X45,"NUMBER OF  ITERATIONS REQUIRED"/(10I12)/);   00002100
FORMAT         DATA(/////////////////////////////////////////////////   00002200
                    /////////////////////////////////////////////////   00002300
                    /////////////////////////////////////////////////   00002400
                    /////////////////////////////////////////////////); 00002500
FORMAT         FCM(100O) ;                                              00002600
FORMAT         EVEC1(//X54,"EIGENVECTORS"//);                           00002700
FORMAT         EVEC2(/X1,"VECTOR",I3/(10R12.5)/);                       00002800
FORMAT         EVEC3(/X20,"COMMUNALITIES - ITERATIONS= ",I3);           00002900
FORMAT         EVEC4(/10R12.5);                                         00003000
REAL  XPR;                                                              00003100
INTEGER  QQ;                                                            00003200
LABEL FINIS,START;                                                      00003300
PROCEDURE ERROR(N); VALUE N; INTEGER N; BEGIN                           00003400
  FORMAT F(///////"STOP / PAUSE  NO. ", I5);                            00003500
  WRITE (LINE,  F, N);  GO TO FINIS END;                                00003600
REAL PROCEDURE INT(ARG1); VALUE ARG1; REAL ARG1;                        00003700
    BEGIN INT~(SIGN(ARG1)|ENTIER(ABS(ARG1))) END;                       00003800
REAL PROCEDURE MAX(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00003900
  BEGIN MAX~(IF ARG1}ARG2 THEN ARG1 ELSE ARG2) END;                     00004000
REAL PROCEDURE MIN(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00004100
  BEGIN MIN~(IF ARG1{ARG2 THEN ARG1 ELSE ARG2) END;                     00004200
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
COMMENT **************************  MAINPRO  ****************;          00016000
START:    BEGIN                                                         00016100
INTEGER        AR , EX ;                                                00016200
LABEL          LL1 ;                                                    00016300
LIST           HEAD(FOR I ~ 0 STEP 1 UNTIL 9 DO ID[I],N,LIQ,LFQ,P,      00016400
                    RN,RA,WRT,WRC,INP,COM,RAT,TOL,FO3);                 00016500
               READ(CARD,FORM1,HEAD) [FINIS] ;                          00016600
               IF INP > 1 THEN READ(CORMAT,FCM,N) ;                     00016700
               WRITE(LINE[PAGE]) ;                                      00016800
               DATELINE("PRNCX1") ;                                     00016900
               WRITE(LINE,TITLE) ;                                      00017000
               M ~ (N | (N+1))/2 ;                                      00017100
               EX ~ LFQ ;                                               00017200
               IF RA > 0 THEN AR ~ 1 ELSE AR ~ 0 ;                      00017300
LL1:      BEGIN                                                         00017400
REAL ARRAY     B,C[0:N];                                                00017500
REAL ARRAY     R[0:M,[256]], A[0:EX,0:N], U[0:N], W[0:EX], EVQ[0:EX] ;  00017600
INTEGER ARRAY  KVQ[0:EX] ;                                              00017700
INTEGER  DX1;                                                           00017800
INTEGER        L, K, L1Q, J, IJQ, I1Q, Z ;                              00017900
REAL           E, S, E1Q ;                                              00018000
LIST LIST1  (FOR DX1 ~ 1 STEP 1 UNTIL N DO U[DX1]),                     00018100
     LIST2(FOR DX1 ~ 1 STEP 1 UNTIL M DO R[DX1]),                       00018200
     LIST3(I,FOR DX1~1 STEP 1 UNTIL L DO A[DX1,I]),                     00018300
     LIST4(FOR DX1 ~ 1 STEP 1 UNTIL L DO KVQ[DX1]),                     00018400
     LIST5(FOR DX1 ~ 1 STEP 1 UNTIL L DO EVQ[DX1]);                     00018500
LIST LIST6(L,FOR DX1 ~ 1 STEP 1 UNTIL N DO U[DX1]);                     00018600
LIST LIST7(FOR I ~ 0 STEP 1 UNTIL 9 DO ID[I],RN,AR,N) ;                 00018700
LIST           LIST8(FOR J~1 STEP 1 UNTIL N DO B[J]);                   00018800
LABEL L9,L24,L27,L47,L51,L28,L101;                                      00018900
               IF INP < 2 THEN BEGIN                                    00019000
               OBJECTFMTGEN(DATA) ;                                     00019100
               WRITE(LINE[DBL])  END ;                                  00019200
               IF INP > 0 THEN                                          00019300
          BEGIN                                                         00019400
               FOR I ~ 1 STEP 1 UNTIL N DO                              00019500
          BEGIN                                                         00019600
               IF INP > 1 THEN  READ(CORMAT,FCM,LIST1)  ELSE            00019700
               READ(CARD,DATA,LIST1);                                   00019800
               FOR J ~ I STEP 1 UNTIL N DO                              00019900
          BEGIN                                                         00020000
               Z ~ Z + 1 ;                                              00020100
               R[Z] ~ U[J] ;                                            00020200
          END ;                                                         00020300
          END ;                                                         00020400
          END  ELSE                                                     00020500
               READ(CARD,DATA,LIST2) ;                                  00020600
               WRITE(LINE,CASE,LIST7);                                  00020700
               FOR I~1 STEP 1 UNTIL N DO C[I]~1;                        00020800
               FO2~0;                                                   00020900
L101:                                                                   00021000
          E ~ 0;                                                        00021100
          L ~ 0;                                                        00021200
L9:       IF L=LFQ THEN GO TO L51;                                      00021300
          L ~ L+1;                                                      00021400
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00021500
            A[L,I] ~ 1  END;                                            00021600
          FOR K ~ 1, K+1 STEP 1 UNTIL LIQ DO BEGIN                      00021700
            IF L{1 THEN GO TO L24;                                      00021800
            L1Q ~ L-1;                                                  00021900
            FOR J ~ 1, J+1 STEP 1 UNTIL L1Q DO BEGIN                    00022000
              W[J] ~ 0;                                                 00022100
              FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                    00022200
                W[J] ~ W[J]+A[J,I]|A[L,I]  END;                         00022300
              END;                                                      00022400
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00022500
              U[I] ~ 0;                                                 00022600
              FOR J ~ 1, J+1 STEP 1 UNTIL L1Q DO BEGIN                  00022700
                U[I] ~ U[I]-A[J,I]|W[J]  END;                           00022800
              END;                                                      00022900
L24:        FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00023000
              IF L>1 THEN GO TO L27;                                    00023100
              U[I] ~ 0;                                                 00023200
L27:          FOR J ~ 1, J+1 STEP 1 UNTIL I DO BEGIN                    00023300
                IJQ ~ I+((J-1)|(N|2-J)) DIV 2;                          00023400
                U[I] ~ U[I]+R[IJQ]|A[L,J]  END;                         00023500
              I1Q ~ I+1;                                                00023600
               IF I1Q >N THEN GO TO L28;                                00023700
              FOR J ~ I1Q, J+1 STEP 1 UNTIL N DO BEGIN                  00023800
                IJQ ~ ((I-1)|(N|2-I)) DIV 2+J;                          00023900
                U[I] ~ U[I]+R[IJQ]|A[L,J]  END;                         00024000
L28:          END;                                                      00024100
            S ~ 0;                                                      00024200
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00024300
              S ~ S+U[I]|A[L,I]  END;                                   00024400
               IF S < 0 THEN                                            00024500
          BEGIN                                                         00024600
               WRITE(LINE,<///"********",I6," TH APROX. OF THE",I6,     00024700
                    " TH EIGENVALUE WAS < 0"///>,K,L) ;                 00024800
               IF FO3!1 THEN                                            00024900
          BEGIN                                                         00025000
               WRITE (LINE,EVEC3,FO2);  WRITE (LINE,EVEC4,LIST8);       00025100
          END;                                                          00025200
               COM~0;                                                   00025300
               GO TO L47 ;                                              00025400
          END ;                                                         00025500
            S ~ 1/SQRT(S);                                              00025600
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00025700
              A[L,I] ~ U[I]|S  END;                                     00025800
            S ~ 0;                                                      00025900
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00026000
              S ~ S+A[L,I]*2  END;                                      00026100
            E1Q ~ E;                                                    00026200
            E ~ SQRT(S);                                                00026300
            IF (ABS(E1Q/E-1))<P THEN GO TO L47;                         00026400
            END;                                                        00026500
L47:           IF S { RN THEN                                           00026600
          BEGIN                                                         00026700
               L ~ L - 1;                                               00026800
               GO TO L51 ;                                              00026900
          END ;                                                         00027000
          KVQ[L] ~ K;                                                   00027100
          EVQ[L] ~ S;                                                   00027200
               IF L=1 AND COM=0 THEN WRITE(LINE,EVEC1);                 00027300
               FOR I ~ 1 STEP 1 UNTIL N DO                              00027400
               U[I] ~ A[L,I] / E ;                                      00027500
               IF COM=0 THEN WRITE(LINE,EVEC2,LIST6);                   00027600
          GO TO L9;                                                     00027700
L51:           IF COM!0 THEN                                            00027800
          BEGIN                                                         00027900
               FOR I~1 STEP 1 UNTIL N DO B[I]~0;                        00028000
               C[0]~0;  FO2~FO2+1;                                      00028100
               FOR J~1 STEP 1 UNTIL N DO                                00028200
               FOR I~1 STEP 1 UNTIL L DO                                00028300
               B[J]~B[J]+A[I,J]*2;                                      00028400
               IF FO3=1 THEN                                            00028500
          BEGIN                                                         00028600
               WRITE (LINE,EVEC3,FO2);                                  00028700
               WRITE (LINE,EVEC4,LIST8);                                00028800
          END;                                                          00028900
               FOR I~1 STEP 1 UNTIL N DO                                00029000
               IF (ABS(C[I]-B[I]))>TOL THEN C[0]~C[0]+1;                00029100
               IF C[0]!0 THEN COM~1 ELSE                                00029200
          BEGIN                                                         00029300
               COM~0;                                                   00029400
               IF FO3!1 THEN                                            00029500
          BEGIN                                                         00029600
               WRITE (LINE,EVEC3,FO2);                                  00029700
               WRITE (LINE,EVEC4,LIST8);                                00029800
          END;                                                          00029900
          END;                                                          00030000
               IF FO2=(RAT-1) OR FO2=RAT THEN                           00030100
          BEGIN                                                         00030200
               IF FO3!1 AND C[0]!0 THEN                                 00030300
          BEGIN                                                         00030400
               WRITE (LINE,EVEC3,FO2); WRITE (LINE,EVEC4,LIST8);        00030500
          END;                                                          00030600
          END;                                                          00030700
               IF FO2}RAT THEN COM~0;                                   00030800
               FOR I~1 STEP 1 UNTIL N DO                                00030900
          BEGIN                                                         00031000
               Z1~((I-1)|(2|N-I))/2+I;                                  00031100
               R[Z1]~C[I]~B[I];                                         00031200
          END;                                                          00031300
               GO TO L101;                                              00031400
          END ELSE                                                      00031500
               WRITE(LINE,FACT1);                                       00031600
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00031700
            WRITE(LINE,FACT2,LIST3);                                    00031800
            END;                                                        00031900
          WRITE(LINE,ITTR,LIST4);                                       00032000
          WRITE(LINE,EVAL,LIST5);                                       00032100
               IF WRT > 0 OR WRC > 0 THEN                               00032200
          BEGIN                                                         00032300
COMMENT        WRITE FACTOR MATRIX ON TAPE AND CARDS;                   00032400
FILE OUT       FACMAT 2(2,100, SAVE 10) ;                               00032500
INTEGER        J1Q,J2Q,JSECQ;                                           00032600
FORMAT         OKTL(100O),                                              00032700
               PUNC(2I4,X2,10F7.4);                                     00032800
LIST       FACP( I,JSECQ,FOR DX1 ~ J1Q STEP 1 UNTIL J2Q DO A[DX1,I]),   00032900
            TAPO(FOR DX1 ~ 1 STEP 1 UNTIL L  DO A[DX1,I]);              00033000
LABEL          L77,L78,L79;                                             00033100
               IF WRT > 0 THEN BEGIN                                    00033200
               WRITE(FACMAT,OKTL,N,L) ;                                 00033300
               FOR I ~  1  STEP 1 UNTIL N   DO                          00033400
               WRITE(FACMAT,OKTL,TAPO) ;                                00033500
               REWIND(FACMAT) ;                                         00033600
          END ;                                                         00033700
               IF WRC >0 THEN  BEGIN                                    00033800
L77:           FOR I ~  1  STEP 1 UNTIL N   DO                          00033900
          BEGIN                                                         00034000
               J1Q ~ 0;                                                 00034100
               J2Q ~ 0;                                                 00034200
               JSECQ ~ 0;                                               00034300
L78:           J1Q ~ J2Q+1;                                             00034400
               J2Q ~ J1Q+9;                                             00034500
               IF J2Q { L    THEN GO TO L79;                            00034600
               J2Q ~ L;                                                 00034700
L79:           JSECQ ~ JSECQ+1;                                         00034800
               WRITE(PUNCH,PUNC,FACP);                                  00034900
               IF J2Q <  L   THEN GO TO L78;                            00035000
          END;                                                          00035100
          END;                                                          00035200
          END;                                                          00035300
          END;                                                          00035400
          END;                                                          00035500
                                                                        00035600
COMMENT **************************  OUTER BLOCK  ************;          00035700
  XPR~QQ~0;                                                             00035800
FINIS:         DATELINE(0);                                             00035900
          END OF PROGRAM PRNCX3.                                        00036000
                LAST CARD ON CRDIMG TAPE                                99999999
