BEGIN                                                                   00001000
%  BMD03S ---- BIOLOGICAL ASSAY: PROBIT ANALYSIS (ALGOL)                00001100
FILE IN CARD 0(2,10);                                                   00001200
FILE OUT PUNCH 0(2,10);                                                 00001300
FILE OUT PRINT 4(2,15);                                                 00001400
FILE TAPE1 2(2,15);                                                     00001600
FILE TAPE2 2(2,15);                                                     00001700
FILE TAPE3 2(2,15);                                                     00001800
FILE TAPE4 2(2,15);                                                     00001900
FILE TAPE5 0(2,10);                                                     00002000
FILE TAPE7 2(2,15);                                                     00002200
FILE TAPE8 2(2,15);                                                     00002300
FILE TAPE9 2(2,15);                                                     00002400
FILE TAPE10 2(2,15);                                                    00002500
FILE TAPE11 2(2,15);                                                    00002600
FILE TAPE13 2(2,15);                                                    00002800
FILE TAPE14 2(2,15);                                                    00002900
FILE TAPE15 2(2,15);                                                    00003000
FILE TAPE16 2(2,15);                                                    00003100
SWITCH FILE FILESW:=XXXXXX,TAPE1,TAPE2,TAPE3,TAPE4,TAPE5,TAPE6,TAPE7,   00003200
LABEL  FINIS;                                                           00003400
REAL ARRAY  DATA[0:63,0:511];    COMMENT USED WITH DATA STATEMENTS ONLY;00003500
REAL Q,XPR;    INTEGER K;                                               00003600
DEFINE B=BOOLEAN#;                                                      00003700
FORMAT F(//////"STOP / PAUSE  NO. ",I5), OKTL(256O);                    00003800
   INT:=SIGN(ARG1)|ENTIER(ABS(ARG1));                                   00004000
REAL PROCEDURE TANH(ARG1);        VALUE ARG1;        REAL ARG1;         00004100
   TANH:=((Q:=EXP(ARG1|2))-1)/(Q+1);                                    00004200
REAL PROCEDURE MAX(ARG1,ARG2);    VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00004300
   MAX:=IF ARG1 GEQ ARG2 THEN ARG1 ELSE ARG2;                           00004400
   MIN:=IF ARG1 LEQ ARG2 THEN ARG1 ELSE ARG2;                           00004600
REAL PROCEDURE DIM(ARG1,ARG2);    VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00004700
   DIM:=MAX(ARG1-ARG2,0);                                               00004800
REAL PROCEDURE TSIGN(ARG1,ARG2);  VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00004900
   TSIGN:=SIGN(IF ARG2=0 THEN 1 ELSE ARG2)|ABS(ARG1);                   00005000
   LOG:=LN(ARG1)/2.30258509298;                                         00005200
PROCEDURE ERROR(ARG1);            VALUE ARG1;        REAL ARG1;         00005300
BEGIN WRITE(PRINT,F,ARG1); GO TO FINIS END;                             00005400
REAL ARRAY R,SN,P,Z,X,YY,Y[0:1001],A[0:3],DC[0:9],FQQQQQQ[0:3],         00005500
DOSE,SMP,SMY[0:1001],CONST[0:64];                                       00005600
REAL ARRAY CD,DV,D[0:10],DC0,CD0[0:9];                                  00005800
INTEGER ARRAY ICODE[0:64];                                              00005900
INTEGER M,IBG,KQQQQQQ;                                                  00006000
REAL CODE,CG;                                                           00006100
INTEGER IPR;                                                            00006200
FORMAT FMT;                                                             00006400
          BEGIN                                                         00006500
STREAM PROCEDURE FORM(FT,CODE);                                         00006600
          BEGIN                                                         00006700
               DI~FT;                                                   00006800
               DS~15 WDS;                                               00007000
          END;                                                          00007100
LABEL RETURN,ERROR;                                                     00007200
FORMAT         F1(72A1);                                                00007300
        ARRAY  CODE[0:14];                                              00007400
INTEGER        I,K,REP,WIDTH,D,N;                                       00007600
INTEGER TENS,ONES,WT,WO,DEC0,DEC1,NN;                                   00007700
DEFINE    S=CODE[K].[1:1]#,                                             00007800
          C=CODE[K].[2:4]#,                                             00007900
          W=CODE[K].[6:6]#,                                             00008000
         D1=CODE[K].[20:4]#,                                            00008200
         W2=CODE[K].[24:4]#,                                            00008300
         W1=CODE[K].[28:4]#,                                            00008400
       SKIP=CODE[K].[32:6]#,                                            00008500
     REPEAT=CODE[K].[38:10]#,                                           00008600
            F[I]="6" OR F[I]="7" OR F[I]="8" OR F[I]="9" OR F[I]="0" #, 00008800
       CHAR=F[I]="A" OR F[I]="I" OR F[I]="X" OR F[I]="E" OR F[I]="F"    00008900
OR F[I] ="R" #;  INTEGER TR,L1,L2,L3,L4;                                00009000
BOOLEAN TRUTH;                                                          00009100
DEFINE CHECK= BEGIN L1~L1+1;                                            00009200
                    L3~L3+1;                                            00009400
                    L4~L4+1;                                            00009500
              END  # ;                                                  00009600
BOOLEAN FIRST;                                                          00009700
INTEGER R1,R2,R3,R4;                                                    00009800
FIRST~TRUE;                                                             00010000
               K~0;                                                     00010100
TRUTH~FALSE;                                                            00010200
L1~L2~L3~L4~TR~0;                                                       00010300
               FOR I~0 STEP 1 UNTIL 71 DO                               00010400
IF F[I]="(" THEN                                                        00010600
                BEGIN                                                   00010700
                     S~1; SKIP~1;CODE[K].[6:26]~0;                      00010800
C~4;                                                                    00010900
                     NN~I;                                              00011000
                            ELSE                                        00011200
                     BEGIN                                              00011300
                          I~I-1;                                        00011400
                          IF NUMBER                                     00011500
                          THEN                                          00011600
                                   I~I-1;                               00011800
                                   IF NUMBER                            00011900
                                   THEN BEGIN                           00012000
                                             TENS~F[I];                 00012100
                                             ONES~F[I+1];               00012200
                                   ELSE BEGIN                           00012400
                                             TENS~0;                    00012500
                                             ONES~F[I+1];               00012600
                                        END;                            00012700
                                   REP~10|TENS+ONES;                    00012800
                              END                                       00013000
                          ELSE REPEAT~0;                                00013100
                                   TRUTH~TRUE;                          00013200
                                   TR~TR+1;                             00013300
IF TR =1 THEN BEGIN L1~0;R1~REP END ;                                   00013400
IF TR =3 THEN BEGIN L3~0;R3~REP END ;                                   00013600
IF TR =4 THEN BEGIN L4~0;R4~REP END ;                                   00013700
                     END;                                               00013800
                          I~NN;                                         00013900
                          K~K+1;                                        00014000
FIRST~FALSE;                                                            00014200
                          GO TO NEXT;                                   00014300
                END;                                                    00014400
               IF F[I]="/" THEN                                         00014500
             BEGIN                                                      00014600
                  CODE[K].[6:26]~0; REPEAT~0;                           00014800
IF TRUTH THEN CHECK;                                                    00014900
                  K~K+1; GO TO NEXT;                                    00015000
             END;                                                       00015100
               IF F[I]="," THEN GO TO NEXT;                             00015200
IF F[I]=")" THEN                                                        00015400
              BEGIN                                                     00015500
LABEL DSK;                                                              00015600
                   IF NOT TRUTH THEN                                    00015700
                        BEGIN                                           00015800
                        CODE[K].[6:26]~0;                               00016000
                        REPEAT~1;                                       00016100
                        K~K+1;                                          00016200
                   END;                                                 00016300
IF TR = 1 THEN IF R1 !0 THEN GO TO DSK;                                 00016400
IF TR=3 THEN IF R3!0 THEN GO TO DSK;                                    00016600
IF TR=4 THEN IF R4!0 THEN GO TO DSK;                                    00016700
S~1; SKIP~0; C~6;                                                       00016800
CODE[K].[6:26]~0;                                                       00016900
REPEAT~1;                                                               00017000
IF TRUTH THEN CHECK;                                                    00017200
DSK:                                                                    00017300
                   S~1;C~0;SKIP~0;                                      00017400
                   CODE[K].[6:26]~0;                                    00017500
                   IF TRUTH THEN BEGIN IF TR=1 THEN REP~L1;             00017600
                                       IF TR=3 THEN REP~L3;             00017800
                                       IF TR=4 THEN REP~L4;             00017900
                                 END                                    00018000
                            ELSE REP~K;                                 00018100
TR~TR-1;                                                                00018200
                        REPEAT~REP;                                     00018400
                   K~K+1;                                               00018500
IF TRUTH THEN CHECK;                                                    00018600
                   GO TO NEXT                                           00018700
              END;                                                      00018800
             BEGIN                                                      00019000
NN~I;                                                                   00019100
                  I~I-1;                                                00019200
                  IF NUMBER THEN                                        00019300
                BEGIN                                                   00019400
                     I~I-1;                                             00019600
                        TENS~0;                                         00019700
IF I<0 THEN GO TO IMIN;                                                 00019800
                     IF NUMBER THEN                                     00019900
                   BEGIN                                                00020000
                        ONES~F[I+1];                                    00020200
                   END                                                  00020300
;                                                                       00020400
IMIN:BEGIN                                                              00020500
                        ONES~F[I+1];                                    00020600
                   END;                                                 00020800
I~I+1;                                                                  00020900
               END ELSE REP~1;                                          00021000
          IF REP=0 THEN REP~1;                                          00021100
I~I+2;                                                                  00021200
                   BEGIN                                                00021400
                        I~I+1;                                          00021500
                        IF NUMBER THEN                                  00021600
                      BEGIN                                             00021700
                           WT~F[I-1];                                   00021800
WIDTH~10|WT+WO;                                                         00022000
                      END                                               00022100
                      ELSE                                              00022200
                      BEGIN                                             00022300
                           WO~F[I-1];                                   00022400
          I~I-1;                                                        00022600
                      END                                               00022700
                   END                                                  00022800
                   ELSE                                                 00022900
                        GO TO ERROR;                                    00023000
                     IF F[I]="." THEN                                   00023200
                   BEGIN                                                00023300
                        I~I+1;                                          00023400
                        IF NUMBER THEN                                  00023500
                      BEGIN                                             00023600
                           IF NUMBER THEN                               00023800
                         BEGIN                                          00023900
                              DEC0~F[I-1];                              00024000
                              DEC1~F[I];                                00024100
D~10|DEC0+DEC1;                                                         00024200
                         ELSE                                           00024400
                         BEGIN                                          00024500
                              DEC0~0;                                   00024600
                              DEC1~F[I-1];                              00024700
D~DEC1;                                                                 00024800
                      END                                               00025000
                      ELSE GO TO ERROR;                                 00025100
                   END;                                                 00025200
I~NN;                                                                   00025300
                     IF F[I]="X" THEN                                   00025400
                        REPEAT~REP;                                     00025600
WO~WIDTH MOD 64;                                                        00025700
W~WO;                                                                   00025800
SKIP~WO;                                                                00025900
                        W1~WIDTH DIV 64;                                00026000
                        D1~0;                                           00026200
                        D2~0;                                           00026300
                        S~0;                                            00026400
                        C~2;                                            00026500
                        K~K+1;                                          00026600
                        GO TO NEXT;                                     00026800
                   END;                                                 00026900
                     IF F[I]="A" THEN                                   00027000
                   BEGIN                                                00027100
                        S~0;                                            00027200
                        REPEAT~REP;                                     00027400
                        W2~0;                                           00027500
                        D1~0;                                           00027600
                        D2~0;                                           00027700
                        W~WIDTH;                                        00027800
                      BEGIN                                             00028000
                           W1~WIDTH;                                    00028100
                           SKIP~0;                                      00028200
                      END                                               00028300
                      ELSE                                              00028400
                           W1~6;                                        00028600
                           SKIP~WIDTH-6;                                00028700
                      END;                                              00028800
                        K~K+1;                                          00028900
IF TRUTH THEN CHECK;                                                    00029000
                   END;                                                 00029200
                     IF F[I]="I" THEN                                   00029300
                   BEGIN                                                00029400
                        S~0;                                            00029500
                        C~6;                                            00029600
                        W~WIDTH;                                        00029800
                        D1~0;                                           00029900
                        D2~0;                                           00030000
                        IF WIDTH>16 THEN SKIP~WIDTH-16                  00030100
                                    ELSE SKIP~0;                        00030200
                      BEGIN                                             00030400
                           W1~WIDTH;                                    00030500
                           W2~0;                                        00030600
                      END                                               00030700
                      ELSE                                              00030800
                           W1~8;                                        00031000
                           W2~WIDTH-SKIP-8;                             00031100
                      END;                                              00031200
                        K~K+1;                                          00031300
IF TRUTH THEN CHECK;                                                    00031400
                   END;                                                 00031600
                    IF F[I]="F" OR F[I]="E"  OR F[I] = "R" THEN         00031700
                   BEGIN                                                00031800
                        S~0;                                            00031900
                       C~15;                                            00032000
                        W~WIDTH;                                        00032200
                        IF D>16 THEN GO TO ERROR;                       00032300
                       IF D{8 THEN                                      00032400
                      BEGIN                                             00032500
                           D1~D;                                        00032600
                      END                                               00032800
                      ELSE                                              00032900
                      BEGIN                                             00033000
                           D1~8;                                        00033100
                           D2~D-8;                                      00033200
                        IF (N~WIDTH-D-1)>16 THEN SKIP~WIDTH-D-17        00033400
                                            ELSE SKIP~0;                00033500
                        IF N>8 THEN                                     00033600
                      BEGIN                                             00033700
                           W1~8;                                        00033800
                      END                                               00034000
                      ELSE                                              00034100
                      BEGIN                                             00034200
                           W1~N;                                        00034300
                           W2~0;                                        00034400
                        K~K+1;                                          00034600
IF TRUTH THEN CHECK;                                                    00034700
                        GO TO NEXT;                                     00034800
                END;                                                    00034900
             END;                                                       00035000
               FORM(FMT,CODE[0]);                                       00035200
GO TO RETURN;                                                           00035300
ERROR:                                                                  00035400
GO TO FINIS;                                                            00035500
RETURN: END OF FIXFORM;                                                 00035600
  VALUE M,C;                                                            00035800
  INTEGER M;                                                            00035900
  REAL C;                                                               00036000
  REAL ARRAY X[0],YY[0],Z[0],P[0],R[0],SN[0],FQQQQQQ[0],D[0];           00036100
BEGIN                                                                   00036200
OWN REAL CQ,PCC,PP,QQQQQQQ,PQ,XLP,XLPP,PA,PB,PC,PAA,PAB,PBB,PAC,        00036400
 PBC;                                                                   00036500
COMMENT  DERIV2       SUBROUTINE DERIV2 FOR BMD03S                      00036600
DERIVATIVES WITH RESPECT TO A AND B.;                                   00036700
CQ:=1-C;                                                                00036800
DO BEGIN                                                                00037000
  D[I]:=0;                                                              00037100
  END UNTIL (I:=(I+1)) GTR 4;                                           00037200
FQQQQQQ[1]:=0;                                                          00037300
FQQQQQQ[2]:=0;                                                          00037400
I:=1;                                                                   00037600
DO BEGIN                                                                00037700
  PP:=C+P[I]|CQ;                                                        00037800
  QQQQQQQ:=1-PP;                                                        00037900
  PQ:=PP|QQQQQQQ;                                                       00038000
  XLPP:=-R[I]/PP*2-(SN[I]-R[I])/QQQQQQQ*2;                              00038200
  PA:=CQ|Z[I];                                                          00038300
  PB:=PA|X[I];                                                          00038400
  PC:=1-P[I];                                                           00038500
  PAA:=-PA|YY[I];                                                       00038600
  PBB:=PAB|X[I];                                                        00038800
  PAC:=-Z[I];                                                           00038900
  PBC:=PAC|X[I];                                                        00039000
  FQQQQQQ[1]:=FQQQQQQ[1]+XLP|PA;                                        00039100
  FQQQQQQ[2]:=FQQQQQQ[2]+XLP|PB;                                        00039200
  D[2]:=D[2]+XLPP|PA|PB+XLP|PAB;                                        00039400
  D[4]:=D[4]+XLPP|PB|PB+XLP|PBB;                                        00039500
  END UNTIL (I:=(I+1)) GTR M;                                           00039600
D[3]:=D[2];                                                             00039700
 END;                                                                   00039800
  VALUE M,C;                                                            00040000
  INTEGER M;                                                            00040100
  REAL C;                                                               00040200
  REAL ARRAY X[0],YY[0],Z[0],P[0],R[0],SN[0],FQQQQQQ[0],D[0];           00040300
BEGIN                                                                   00040400
OWN REAL CQ,PCC,PP,QQQQQQQ,PQ,XLP,XLPP,PA,PB,PC,PAA,PAB,PBB,PAC,        00040600
 PBC;                                                                   00040700
LABEL L13,L0;                                                           00040800
COMMENT  DERIV3     SUBROUTINE DERIV3 FOR BMD03S                        00040900
DERIVATIVES WITH RESPECT TO A, B, AND C.;                               00041000
I:=1;                                                                   00041200
DO BEGIN                                                                00041300
  FQQQQQQ[I]:=0;                                                        00041400
  D[I]:=0;                                                              00041500
  END UNTIL (I:=(I+1)) GTR 3;                                           00041600
DO BEGIN                                                                00041800
  D[I]:=0;                                                              00041900
  END UNTIL (I:=(I+1)) GTR 9;                                           00042000
PCC:=0;                                                                 00042100
M1:=M;                                                                  00042200
M1:=M+1;                                                                00042400
L13: I:=1;                                                              00042500
DO BEGIN                                                                00042600
  PP:=C+P[I]|CQ;                                                        00042700
  QQQQQQQ:=1-PP;                                                        00042800
  XLP:=(R[I]-PP|SN[I])/PQ;                                              00043000
  XLPP:=-R[I]/PP*2-(SN[I]-R[I])/QQQQQQQ*2;                              00043100
  PA:=CQ|Z[I];                                                          00043200
  PB:=PA|X[I];                                                          00043300
  PC:=1-P[I];                                                           00043400
  PAB:=-PB|YY[I];                                                       00043600
  PBB:=PAB|X[I];                                                        00043700
  PAC:=-Z[I];                                                           00043800
  PBC:=PAC|X[I];                                                        00043900
  FQQQQQQ[1]:=FQQQQQQ[1]+XLP|PA;                                        00044000
  FQQQQQQ[3]:=FQQQQQQ[3]+XLP|PC;                                        00044200
  D[1]:=D[1]+XLPP|PA|PA+XLP|PAA;                                        00044300
  D[2]:=D[2]+XLPP|PA|PB+XLP|PAB;                                        00044400
  D[5]:=D[5]+XLPP|PB|PB+XLP|PBB;                                        00044500
  D[3]:=D[3]+XLPP|PA|PC+XLP|PAC;                                        00044600
  D[9]:=D[9]+XLPP|PC|PC+XLP|PCC;                                        00044800
  END UNTIL (I:=(I+1)) GTR M1;                                          00044900
D[4]:=D[2];                                                             00045000
D[8]:=D[6];                                                             00045100
D[7]:=D[3];                                                             00045200
PROCEDURE PHI2(X,PHI,PP);                                               00045400
  VALUE X;                                                              00045500
  REAL X,PHI,PP;                                                        00045600
BEGIN                                                                   00045700
OWN REAL SQ2,Y,YY,P1,ET,PH;                                             00045800
COMMENT  PHI2  SUBROUTINE TO COMPUTE PHI(X) AND PHIPRIME(X).;           00046000
SQ2:=1.414213562;                                                       00046100
Y:=ABS(X)/SQ2;                                                          00046200
YY:=X|X/2;                                                              00046300
PP:=EXP(-YY)|.39894228;                                                 00046400
ET:=1/(1+.3275911|Y);                                                   00046600
PH:=1-((((.94064607|ET-1.287822453)|ET+1.25969513)|ET-.252128668)|      00046700
 ET+.225836846)|ET|P1;                                                  00046800
IF (XPR:=(X)) GTR 0 THEN GO TO L3 ELSE IF XPR=0 THEN GO TO L4;          00046900
PH:=-PH;                                                                00047000
GO TO L0;                                                               00047200
L4: PHI:=0;                                                             00047300
L0:  END;                                                               00047400
PROCEDURE DIFF;                                                         00047500
BEGIN                                                                   00047600
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  PHI2,DERIV2,DERIV3;        00047800
LABEL L300,L2050,L0;                                                    00047900
COMMENT  DIFF         SUBROUTINE DIFF FOR BMD03S                        00048000
DIFFERENTIAL COEFICIENTS AND FUNCTION AUXILIARY SUBROUTINE FOR NEWT     00048100
PREDICT PROBIT YY AND PROBABILITY P.;                                   00048200
DO BEGIN                                                                00048400
  Y[I]:=A[1]+A[2]|X[I];                                                 00048500
  YY[I]:=Y[I]-5;                                                        00048600
  PHI2(YY[I],P[I],Z[I]);                                                00048700
  END UNTIL (I:=(I+1)) GTR M;                                           00048800
P[M1]:=0;                                                               00049000
X[M1]:=0;                                                               00049100
IF CG LSS 0 THEN GO TO L300 ELSE GO TO L2050;                           00049200
COMMENT  C CONSTANT;                                                    00049300
L2050: DERIV2(X,YY,Z,P,R,SN,M,A[3],FQQQQQQ,DC);                         00049400
COMMENT  OPTIMIZE C;                                                    00049600
L300: DERIV3(X,YY,Z,P,R,SN,M,A[3],FQQQQQQ,DC);                          00049700
L0:  END;                                                               00049800
PROCEDURE INVERT(G,TEMP,NM,N,DET,ER);                                   00049900
    VALUE N,NM;                                                         00050000
    REAL DET,ER,NM;                                                     00050200
    REAL ARRAY G[0],TEMP[0];                                            00050300
BEGIN                                                                   00050400
OWN REAL ARRAY R[0:1001];                                               00050500
OWN INTEGER KQQQQQQ,I,J;                                                00050600
LABEL L10,L20,L30,L0,L80,L99;                                           00050800
SWITCH SWGO1:=L10,L20,L30;                                              00050900
COMMENT  INVERT  MATRIX OF DIMENSION 2 OR 3.;                           00051000
ER:=0;                                                                  00051100
KQQQQQQ:=N;                                                             00051200
L10: R[1]:=1/G[1];                                                      00051400
GO TO L0;                                                               00051500
L20: DET:=G[1]|G[4]-G[3]|G[2];                                          00051600
IF DET NEQ 0 THEN GO TO L80;                                            00051700
L99: ER:=1;                                                             00051800
L80: R[1]:=G[4]/DET;                                                    00052000
R[2]:=-G[2]/DET;                                                        00052100
R[3]:=-G[3]/DET;                                                        00052200
R[4]:=G[1]/DET;                                                         00052300
GO TO L0;                                                               00052400
R[4]:=-(G[4]|G[9]-G[6]|G[7]);                                           00052600
R[7]:=(G[4]|G[8]-G[5]|G[7]);                                            00052700
R[2]:=-(G[2]|G[9]-G[3]|G[8]);                                           00052800
R[5]:=(G[1]|G[9]-G[3]|G[7]);                                            00052900
R[8]:=-(G[1]|G[8]-G[2]|G[7]);                                           00053000
R[6]:=-(G[1]|G[6]-G[3]|G[4]);                                           00053200
R[9]:=(G[1]|G[5]-G[2]|G[4]);                                            00053300
DET:=R[1]|G[1]+R[4]|G[2]+R[7]|G[3];                                     00053400
IF DET=0 THEN GO TO L99;                                                00053500
I:=1;                                                                   00053600
  R[I]:=R[I]/DET END UNTIL (I:=(I+1)) GTR 9;                            00053800
GO TO L0;                                                               00053900
L0:                                                                     00054000
    J ~ 1;                                                              00054100
    FOR I ~ NM STEP 1 UNTIL NM + 8 DO                                   00054200
              TEMP[I] ~ R[J];                                           00054400
              J ~ J + 1;                                                00054500
         END;                                                           00054600
END OF INVERT;                                                          00054700
PROCEDURE LINREG(X,Y,M,A,B);                                            00054800
  INTEGER M;                                                            00055000
  REAL A,B;                                                             00055100
  REAL ARRAY X[0],Y[0];                                                 00055200
BEGIN                                                                   00055300
OWN INTEGER I;                                                          00055400
LABEL L0;                                                               00055600
COMMENT  LINREG       SUBROUTINE LINREG FOR BMD03S                      00055700
REGRESS Y AS A FUNCTION OF X.  Y=AX + B.;                               00055800
XM:=M;                                                                  00055900
SX:=0;                                                                  00056000
SXX:=0;                                                                 00056200
SXY:=0;                                                                 00056300
I:=1;                                                                   00056400
DO BEGIN                                                                00056500
  SXY:=SXY+X[I]|Y[I];                                                   00056600
  SY:=SY+Y[I];                                                          00056800
  SXX:=SXX+X[I]*2;                                                      00056900
  END UNTIL (I:=(I+1)) GTR M;                                           00057000
A:=(XM|SXY-SX|SY)/(XM|SXX-SX*2);                                        00057100
B:=(SY-A|SX)/XM;                                                        00057200
PROCEDURE MNVAR(X,N,XB,XV);                                             00057400
  VALUE N;                                                              00057500
  INTEGER N;                                                            00057600
  REAL XB,XV;                                                           00057700
  REAL ARRAY X[0];                                                      00057800
OWN INTEGER I;                                                          00058000
OWN REAL XN1,XN;                                                        00058100
COMMENT  MNVAR SUBROUTINE TO COMPUTE MEAN AND VARIANCE;                 00058200
XN1:=N-1;                                                               00058300
XN:=N;                                                                  00058400
XB:=0;                                                                  00058600
I:=1;                                                                   00058700
DO BEGIN                                                                00058800
  XB:=XB+X[I];                                                          00058900
  END UNTIL (I:=(I+1)) GTR N;                                           00059000
I:=1;                                                                   00059200
DO BEGIN                                                                00059300
  XV:=XV+(X[I]-XB)*2;                                                   00059400
  END UNTIL (I:=(I+1)) GTR N;                                           00059500
XV:=XV/XN1;                                                             00059600
PROCEDURE NEWT(DIFF,D,FQQQQQQ,N,NI,TEMP,EPS,ER,NPR,A);                  00059800
  VALUE N,EPS,NPR;                                                      00059900
  INTEGER N,NI,NPR;                                                     00060000
  REAL EPS,ER;                                                          00060100
  REAL ARRAY D[0],FQQQQQQ[0],TEMP[0],A[0];                              00060200
BEGIN                                                                   00060400
OWN INTEGER DX1;                                                        00060500
OWN INTEGER NM,NF,NN,IPR,KQQQQQQ,I,IJ,J,JL,JU;                          00060600
OWN REAL DET,DEL;                                                       00060700
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  INVERT;                    00060800
FL805("   ",7R16.6),                                                    00061000
FL801(" APPROXIMATION TO SOLUTION."),                                   00061100
FL802(" DIFFERENTIAL COEFFICIENT MATRIX."),                             00061200
FL806(/" CRITERION FOR CONVERGENCE IS NOT MET AFTER",I5," ITERATIONS"), 00061300
FL807(/                                                                 00061400
 " BE CONTINUED.");                                                     00061600
LIST LIST1(KQQQQQQ);                                                    00061700
LIST LIST2(FOR DX1:=1 STEP 1 UNTIL N DO FQQQQQQ[DX1]);                  00061800
LIST LIST3(FOR DX1:=1 STEP 1 UNTIL N DO A[DX1]);                        00061900
LIST LIST4(FOR DX1:=JL STEP 1 UNTIL JU DO D[DX1]);                      00062000
LABEL L8,L99,L18,L0;                                                    00062200
COMMENT  NEWT  NON-LINEAR SYSTEM SOLVER;                                00062300
ER:=0;                                                                  00062400
NM:=N+1;                                                                00062500
NF:=N;                                                                  00062600
IPR:=NI+1;                                                              00062800
IF NPR LEQ 0 THEN GO TO L8;                                             00062900
IPR:=1;                                                                 00063000
L8: KQQQQQQ:=1;                                                         00063100
DO BEGIN                                                                00063200
    INVERT(D,TEMP,NM,NF,DET,TEMP[20]);                                  00063400
  DEL:=0;                                                               00063500
  IF DET=0 THEN GO TO L99;                                              00063600
  I:=1;                                                                 00063700
  DO BEGIN                                                              00063800
    IJ:=I;                                                              00064000
    J:=1;                                                               00064100
    DO BEGIN                                                            00064200
      IJ:=IJ+N;                                                         00064300
      TEMP[I]:=TEMP[I]-FQQQQQQ[J]|TEMP[IJ];                             00064400
    DEL:=DEL+TEMP[I]*2;                                                 00064600
    A[I]:=A[I]+TEMP[I];                                                 00064700
    END UNTIL (I:=(I+1)) GTR N;                                         00064800
  IF KQQQQQQ NEQ IPR THEN GO TO L18;                                    00064900
  IPR:=IPR+NPR;                                                         00065000
  WRITE(TAPE6,FL805,LIST2);                                             00065200
  WRITE(TAPE6,FL801);                                                   00065300
  WRITE(TAPE6,FL805,LIST3);                                             00065400
  WRITE(TAPE6,FL802);                                                   00065500
  J:=1;                                                                 00065600
    JL:=N|(J-1)+1;                                                      00065800
    JU:=JL+N-1;                                                         00065900
    WRITE(TAPE6,FL805,LIST4);                                           00066000
    END UNTIL (J:=(J+1)) GTR N;                                         00066100
  L18: IF DEL LEQ EPS THEN GO TO L0;                                    00066200
WRITE(TAPE6,FL806,LIST5);                                               00066400
ER:=1;                                                                  00066500
GO TO L0;                                                               00066600
L99: WRITE(TAPE6,FL807);                                                00066700
ER:=-1;                                                                 00066800
GO TO L0;                                                               00067000
L0:  END;                                                               00067100
REAL PROCEDURE PHINV(P);                                                00067200
  VALUE P;                                                              00067300
  REAL P;                                                               00067400
OWN REAL QQQQQQQ,T,E,X;                                                 00067600
LABEL L10,L12,L0,L13,L122;                                              00067700
COMMENT  PHINV SUBROUTINE USINGS HASTINGS APPROXIMATION.;               00067800
IF (XPR:=(P-.5)) GTR 0 THEN GO TO L12 ELSE IF XPR LSS 0 THEN GO TO      00067900
 L10;                                                                   00068000
GO TO L0;                                                               00068200
L12: QQQQQQQ:=1-P;                                                      00068300
T:=-1;                                                                  00068400
GO TO L13;                                                              00068500
L10: QQQQQQQ:=P;                                                        00068600
L13: IF QQQQQQQ GTR 0 THEN GO TO L122;                                  00068800
PHINV:=-T|999;                                                          00068900
GO TO L0;                                                               00069000
L122: E:=SQRT(LN(1/QQQQQQQ*2));                                         00069100
X:=-E+((.010328|E+.802853)|E+2.515517)/(((.001308|E+.189269)|E+1.432788)00069200
PHINV:=T|X;                                                             00069400
L0:  END;                                                               00069500
PROCEDURE TPWD(NT1,NT2);                                                00069600
  INTEGER NT1,NT2;                                                      00069700
BEGIN                                                                   00069800
LABEL L40,L12,L19,L24,L0;                                               00070000
COMMENT  TPWD    SUBROUTINE TPWD FOR BMD03S         VERSION OF SEPT.    00070100
 26, 1963;                                                              00070200
IF (XPR:=(NT1)) GTR 0 THEN GO TO L12 ELSE IF XPR LSS 0 THEN GO TO       00070300
 L40;                                                                   00070400
L12: IF NT1=NT2 THEN GO TO L19;                                         00070600
IF NT2 GEQ 5 THEN GO TO L19;                                            00070700
REWIND(FILESW[NT2]);                                                    00070800
L19: IF NT1=5 THEN GO TO L24;                                           00070900
IF NT1=6 THEN GO TO L40;                                                00071000
L24: NT2:=NT1;                                                          00071200
GO TO L0;                                                               00071300
L40: WRITE(TAPE6,FL49);                                                 00071400
GO TO FINIS;                                                            00071500
L0:  END;                                                               00071600
    VALUE M,KQQQQQQ,IK;                                                 00071800
    INTEGER M,KQQQQQQ,IERROR,IK;                                        00071900
    INTEGER ARRAY ICODEE[0];                                            00072000
    REAL ARRAY DOSE[0],CONSTT[0],X[0];                                  00072100
BEGIN                                                                   00072200
OWN REAL ARRAY CONST[0:64];                                             00072400
OWN INTEGER J2,JK,JUMP,J;                                               00072500
OWN INTEGER I;                                                          00072600
OWN REAL FM,D,TD,A,SAMP,B;                                              00072700
FORMAT FL9980(/" TRANSFORMATION CANNOT BE PERFORMED. CODE =",I4,        00072800
LIST LIST1(JUMP,DOSE[J],D);                                             00073000
LABEL L8,L10,L20,L30,L40,L50,L60,L70,L80,L90,L110,L99,L12,L22,L52,      00073100
 L53,L0;                                                                00073200
SWITCH SWGO1:=L10,L20,L30,L40,L50,L60,L70,L80,L90,L110;                 00073300
COMMENT  TRNGEN SUBROUTINE TO TRANSFORM DOSE.;                          00073400
VALUE ARGX;                                                             00073600
REAL ARGX;                                                              00073700
ASN:=ARCTAN(ARGX/SQRT(1-ARGX*2));                                       00073800
J~1;                                                                    00073900
FOR I~IK STEP 1 UNTIL 64 DO BEGIN                                       00074000
J ~ J + 1;    END;                                                      00074200
IERROR:=0;                                                              00074300
J2:=1;                                                                  00074400
DO BEGIN                                                                00074500
  X[J2]:=DOSE[J2] END UNTIL (J2:=(J2+1)) GTR M;                         00074600
JK:=1;                                                                  00074800
DO BEGIN                                                                00074900
  JUMP:=ICODE[JK];                                                      00075000
  FM:=CONST[JK];                                                        00075100
  IF JUMP|(JUMP-11) GEQ 0 THEN GO TO L0;                                00075200
  DO BEGIN                                                              00075400
    D:=X[J];                                                            00075500
    GO TO SWGO1[JUMP];                                                  00075600
    COMMENT  1  SQRT(DOSE);                                             00075700
    L10: IF (XPR:=(D)) GTR 0 THEN GO TO L12 ELSE IF XPR LSS 0 THEN GO   00075800
    TD:=0;                                                              00076000
    GO TO L8;                                                           00076100
    L12: TD:=SQRT(D);                                                   00076200
    GO TO L8;                                                           00076300
    COMMENT  2  SQRT(DOSE) + SQRT(DOSE + 1);                            00076400
     TO L99;                                                            00076600
    TD:=1;                                                              00076700
    GO TO L8;                                                           00076800
    L22: TD:=SQRT(D)+SQRT(D+1);                                         00076900
    GO TO L8;                                                           00077000
    L30: IF D LEQ 0 THEN GO TO L99;                                     00077200
    TD:=LOG(D);                                                         00077300
    GO TO L8;                                                           00077400
    COMMENT  4  EXP(DOSE).;                                             00077500
    L40: TD:=EXP(D);                                                    00077600
    COMMENT  5  ARCSINE(DOSE);                                          00077800
    L50: IF (XPR:=(D)) GTR 0 THEN GO TO L52 ELSE IF XPR LSS 0 THEN GO   00077900
     TO L99;                                                            00078000
    TD:=0;                                                              00078100
    GO TO L8;                                                           00078200
TO L53;                                                                 00078400
    TD:=1.570796325;                                                    00078500
    GO TO L8;                                                           00078600
    L53: TD:=ASN(A);                                                    00078700
    GO TO L8;                                                           00078800
    L60: SAMP:=M;                                                       00079000
    A:=D/(SAMP+1);                                                      00079100
    B:=A+1/(SAMP+1);                                                    00079200
    IF A LSS 0 THEN GO TO L99;                                          00079300
    TD:=ASN(SQRT(A));                                                   00079400
    TD:=TD+ASN(SQRT(B));                                                00079600
    GO TO L8;                                                           00079700
    COMMENT  7   1/ DOSE;                                               00079800
    L70: IF D=0 THEN GO TO L99;                                         00079900
    TD:=1/D;                                                            00080000
    L80: TD:=D+FM;                                                      00080200
    GO TO L8;                                                           00080300
    COMMENT  9  DOSE X CONST;                                           00080400
    L90: TD:=D|FM;                                                      00080500
    GO TO L8;                                                           00080600
    L110: TD:=D*FM;                                                     00080800
    L8: X[J]:=TD END UNTIL (J:=(J+1)) GTR M;                            00080900
  END UNTIL (JK:=(JK+1)) GTR 8;                                         00081000
GO TO L0;                                                               00081100
L99: IERROR:=-999;                                                      00081200
GO TO L0;                                                               00081400
L0:                                                                     00081500
J~1; FOR I~IK STEP 1 UNTIL 64 DO BEGIN                                  00081600
CONSTT[I] ~ CONST[J];  ICODEE[I] ~ ICODE[J]; J~J+1; END;                00081700
END OF TRNGEN;                                                          00081800
  INTEGER NVF;                                                          00082000
BEGIN                                                                   00082100
FORMAT FL4000(/" ",X23,                                                 00082200
 "NUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFIED, ASSUMED ",     00082300
 "TO BE 1.");                                                           00082400
COMMENT  VFCHCK    SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE    00082600
 FORMAT CRDS;                                                           00082700
IF NVF GTR 0 THEN GO TO L20;                                            00082800
L10: WRITE(TAPE6,FL4000);                                               00082900
NVF:=1;                                                                 00083000
L20: IF NVF LEQ 10 THEN GO TO L0 ELSE GO TO L10;                        00083200
L0:  END;                                                               00083300
PROCEDURE READIN;                                                       00083400
BEGIN                                                                   00083500
OWN REAL ARRAY AA[0:3];                                                 00083600
OWN INTEGER INDEX1;                                                     00083800
OWN INTEGER NTAP,IC,    MTAP,IVF,M1,I,I2,I3,KODE,K1,                    00083900
 ITRG;                                                                  00084000
OWN REAL PPP,AAA,TRG,ER,PRBLM,SMALN,SMALP,QC;                           00084100
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  VFCHCK,TPWD,PHINV,REMOVE;  00084200
FORMAT FL11(                                                            00084400
 " BMD03S - BIOLOGICAL ASSAY, PROBIT ANALYSIS - VERSION OF JUNE  ",     00084500
 "1, 1964"/" HEALTH SCIENCES COMPUTING FACILITY, UCLA."/),              00084600
FL10(A6,A6,I1,R5.4,I4,4R6.0,X18,I2,2I1,2I2),                            00084700
FL19(A6,I1,8(I2,R6.0)),                                                 00084800
FL9990(" PROBLEM",X1,A6,"."),                                           00085000
FL9991(" CONTROL GROUP DATA. SAMPLE SIZE N=",R6.0,", RESPONSE R=",R6.0, 00085100
 ".",X3,3A6),                                                           00085200
FL9992(" UNTRANSFORMED DOSE."),                                         00085300
FL9993(X3,9R12.2),                                                      00085400
FL9996(" SAMPLE SIZE."),                                                00085600
FL9985(/" DATA INCORRECT,PROBLEM DELETED. "),                           00085700
FL38(/" ",X5,"END OF OUTPUT FOR PROBLEM ",A6),                          00085800
FL9981(" SAMPLE PROBIT"),                                               00085900
FL9982(" SAMPLE PROBABILITY"),                                          00086000
FL9997(/                                                                00086200
 " NON PROBLEM CARD ENCOUNTERED CHECK DATA DECK. EXECUTION DELETE",     00086300
 "D. "),                                                                00086400
FL9998(                                                                 00086500
 " THE NUMBER OF SPECIAL TRANSGENERATION CARDS IS ILLEGAL. PROBLE",     00086600
FL9999(/" ILLEGAL NUMBER OF DOSES SPECIFIED. EXECUTION DELETED."),      00086800
FL9989(/" CONTROL CARD ERROR. SPECTG CARD EXPECTED BUT A ",A6,          00086900
 " CARD WAS FOUND. EXECUTION DELETED.");                                00087000
LIST LIST1(PRBLM,CODE,IC,CG,M,R[1],SN[1],A[1],A[2],IBG,KQQQQQQ,IPR,     00087100
 MTAP,IVF);                                                             00087200
]);                                                                     00087400
LIST LIST4(CODE);                                                       00087500
LIST LIST5(SN[M1],R[M1],FOR INDEX1:=1 STEP 1 UNTIL 3 DO AA[INDEX1]);    00087600
LIST LIST6(FOR DX1:=1 STEP 1 UNTIL M DO DOSE[DX1]);                     00087700
LIST LIST7(FOR DX1:=1 STEP 1 UNTIL M DO R[DX1]);                        00087800
LIST LIST9(FOR DX1:=1 STEP 1 UNTIL M DO SMY[DX1]);                      00088000
LIST LIST10(FOR DX1:=1 STEP 1 UNTIL M DO SMP[DX1]);                     00088100
LIST LIST11(CODE,KQQQQQQ);                                              00088200
LIST LIST12(KODE);                                                      00088300
BEGIN                                                                   00088400
 L301,L303,L291,L32,L610,L612,L210,L25,L0,L992,L999,L889;               00088600
COMMENT  READIN       SUBROUTINE READIN FOR BMD03S            JUNE  1,  00088700
 1964;                                                                  00088800
PPP:=("PROBLM");                                                        00088900
AAA:=("FINISH");                                                        00089000
IF NTAP GTR 0 THEN GO TO L2;                                            00089200
L1: NTAP:=5;                                                            00089300
L2: ER:=0;                                                              00089400
READ(TAPE5,FL10,LIST1)[FINIS];                                          00089500
IF PRBLM ! PPP THEN GO TO L990;                                         00089600
M1:=M+1;                                                                00089800
COMMENT  R(M1) = R;                                                     00089900
R[M1]:=R[1];                                                            00090000
SN[M1]:=MAX(SN[1],0);                                                   00090100
KQQQQQQ:=MIN(KQQQQQQ,8);                                                00090200
I:=1;                                                                   00090400
DO BEGIN                                                                00090500
  I2:=1+8|(I-1);                                                        00090600
  I3:=I2+7;                                                             00090700
  READ(TAPE5,FL19,LIST2)[FINIS];                                        00090800
  L501: END UNTIL (I:=(I+1)) GTR KQQQQQQ;                               00091000
IF -KQQQQQQ GEQ 0 THEN GO TO L887;                                      00091100
VFCHCK(IVF);                                                            00091200
IVF:=IVF|12;                                                            00091300
FIXFORM(FMT);      %READ VARIABLE FORMAT *****************************  00091400
READ(TAPE5,FMT,LIST6)[FINIS];                                           00091600
READ(TAPE5,FMT,LIST7)[FINIS];                                           00091700
READ(TAPE5,FMT,LIST8)[FINIS];                                           00091800
COMMENT  GET SAMPLE P ( = R/S )                                         00091900
GET INVERSE NORMAL OF SAMPLE P.;                                        00092000
DO BEGIN                                                                00092200
  IF SN[I] LEQ 0 THEN GO TO L54;                                        00092300
  SMP[I]:=R[I]/SN[I];                                                   00092400
  IF SMP[I]|(SMP[I]-1) GTR 0 THEN GO TO L54;                            00092500
  END UNTIL (I:=(I+1)) GTR M;                                           00092600
IF ABS(CG) GEQ 1 THEN GO TO L28;                                        00092800
IF CG LSS 0 THEN GO TO L29;                                             00092900
IF IC=0 THEN GO TO L30;                                                 00093000
IF CG=0 THEN GO TO L28 ELSE GO TO L29;                                  00093100
COMMENT  WE GUESS C;                                                    00093200
SN[M1]:=0;                                                              00093400
R[M1]:=0;                                                               00093500
SMALN:=SN[1];                                                           00093600
SMALP:=SMP[1];                                                          00093700
GO TO L2821;                                                            00093800
SMALN:=SN[M1];                                                          00094000
L2821: I:=1;                                                            00094100
DO BEGIN                                                                00094200
  IF SMP[I] GEQ SMALP THEN GO TO L301;                                  00094300
  SMALN:=SN[I];                                                         00094400
  L301: END UNTIL (I:=(I+1)) GTR M;                                     00094600
A[3]:=SMALP;                                                            00094700
IF (XPR:=(A[3])) GTR 0 THEN GO TO L303 ELSE IF XPR LSS 0 THEN GO TO     00094800
 L54;                                                                   00094900
A[3]:=.5/SMALN;                                                         00095000
COMMENT  USER GUESS C;                                                  00095200
L29: A[3]:=ABS(CG);                                                     00095300
L291: AA[1]:=("OPTIMI");                                                00095400
AA[2]:=("ZE C. ");                                                      00095500
AA[3]:=("      ");                                                      00095600
GO TO L32;                                                              00095800
COMMENT  HOLD C CONSTANT;                                               00095900
L30: AA[1]:=("C HELD");                                                 00096000
AA[2]:=(" CONST");                                                      00096100
AA[3]:=("ANT.  ");                                                      00096200
L32: IF SN[M1] GTR 0 THEN GO TO L610;                                   00096400
SMP[M1]:=0;                                                             00096500
SMY[M1]:=5+PHINV(0);                                                    00096600
GO TO L612;                                                             00096700
L610: SMP[M1]:=R[M1]/SN[M1];                                            00096800
L612: QC:=1-A[3];                                                       00097000
I:=1;                                                                   00097100
DO BEGIN                                                                00097200
  SMP[I]:=(SMP[I]-A[3])/QC;                                             00097300
  SMY[I]:=5+PHINV(SMP[I]);                                              00097400
COMMENT  PRINT INPUT DATA;                                              00097600
L210: WRITE(TAPE6[PAGE]);                                               00097700
      WRITE(TAPE6,FL11);                                                00097800
WRITE(TAPE6,FL9990,LIST4);                                              00097900
WRITE(TAPE6,FL9991,LIST5);                                              00098000
WRITE(TAPE6,FL9992);                                                    00098200
WRITE(TAPE6,FL9993,LIST6);                                              00098300
WRITE(TAPE6,FL9994);                                                    00098400
WRITE(TAPE6,FL9993,LIST7);                                              00098500
WRITE(TAPE6,FL9996);                                                    00098600
IF ER=0 THEN GO TO L25;                                                 00098800
WRITE(TAPE6,FL9985);                                                    00098900
WRITE(TAPE6,FL38,LIST4);                                                00099000
GO TO L1;                                                               00099100
L25: WRITE(TAPE6,FL9981);                                               00099200
WRITE(TAPE6,FL9982);                                                    00099400
WRITE(TAPE6,FL9993,LIST10);                                             00099500
GO TO L0;                                                               00099600
L990:    IF PRBLM = AAA                                                 00099700
 THEN GO TO L999 ELSE GO TO L992;                                       00099800
L54: ER:=1;                                                             00100000
GO TO L210;                                                             00100100
COMMENT  LAST PROBLEM COMPLETED.;                                       00100200
L999: WRITE(TAPE6,FL9995);                                              00100300
L887: IF NTAP LEQ 5 THEN GO TO L889;                                    00100400
COMMENT  PROBLEM CARD ERROR;                                            00100600
L992: WRITE(TAPE6[PAGE]);                                               00100700
      WRITE(TAPE6,FL11);                                                00100800
WRITE(TAPE6,FL9997);                                                    00100900
GO TO L887;                                                             00101000
      WRITE(TAPE6,FL11);                                                00101200
WRITE(TAPE6,FL9998,LIST11);                                             00101300
GO TO L887;                                                             00101400
L993: WRITE(TAPE6[PAGE]);                                               00101500
      WRITE(TAPE6,FL11);                                                00101600
GO TO L887;                                                             00101800
L994: WRITE(TAPE6[PAGE]);                                               00101900
      WRITE(TAPE6,FL11);                                                00102000
WRITE(TAPE6,FL9989,LIST12);                                             00102100
KQQQQQQ:=-1001;                                                         00102200
L0:  END END;                                                           00102400
PROCEDURE MAINPRO;                                                      00102500
BEGIN                                                                   00102600
OWN REAL ARRAY                                                          00102700
RX[0:1000];                                                             00102800
OWN INTEGER JK,IK,IERROR,I1,I,M2,NPR,NK,NV,NIT,N1,                      00103000
 NN,IDF,M1,I3;                                                          00103100
OWN REAL ER,SAVC,XAV,XVAR,EPS,CG1,HAMU,ALPHAQQ,ALPHA1,HAMU0,            00103200
HASI,AAA,DET,CHI,RI,PP;                                                 00103300
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  READIN,TRNGEN,LINREG,      00103400
FORMAT FL9998(////" TRANSFORMED DOSE FOR TRANSFORMATION CARD NO ",I3,   00103600
 " OF",I3),                                                             00103700
FL9993(X3,9R12.2),                                                      00103800
FL9988(/" THE INITIAL VALUES AT START OF ITERATION ARE, ALPHA= ",R9.3,  00103900
 X3,"BETA=",R9.3,X3,"C=",R9.3),                                         00104000
FL9885(/                                                                00104200
 " THE FINAL VALUES OF THE PARAMETERS AND THE PARTIAL DERIVATIVES",     00104300
 " ARE"/X5,"FOR PROBIT LINE (ALPHA1+BETA*(X-XBAR))  ",X17,"*",X5,       00104400
 "FOR PROBIT LINE (ALPHA+BETA*(X))   "),                                00104500
FL9980(X5,"ALPHA1=",R8.3,X10,"DL/DA=",X3,R10.4,X13,"*",X5,"ALPHA =",    00104600
X5,"C",X5,"=",R8.3,X10,"DL/DC=",X3,R10.4,X13,"*",X5,"C",X5,"=",R8.3//   00104800
 " THE ESTIMATES MU-HAT AND SIGMA-HAT OF THE MEAN AND STANDARD DE",     00104900
 "VIATION OF THE TRANSFORMED DOSE TOLERANCE ARE AS FOLLOWS."/X5,        00105000
 "   MU-HAT=",X3,R9.4/X5,"SIGMA-HAT=",X3,R9.4),                         00105100
FL9981(/" THE COVARIANCE MATRICES ARE AS FOLLOWS"/X5,"FOR ALPHA1, BETA",00105200
FL9883(X3,2R12.4,X35,"*",2R12.4),                                       00105400
FL9884(X3,3R12.4,X23,"*",3R12.4),                                       00105500
FL9982(X5,"FOR MU, SIGMA",A6),                                          00105600
FL9983(X3,9R12.4),                                                      00105700
FL9984(/" CHI-SQUARE=",X3,R12.4,", WITH",I5," DEGREES OF FREEDOM."),    00105800
FL38(/" ",X5,"END OF OUTPUT FOR PROBLEM ",A6),                          00106000
FL9985(" THE MEAN AND VARIANCE OF THE TRANSFORMED DOSE ARE"/" MEAN= ",  00106100
 R8.4,"  VAR= ",R8.4);                                                  00106200
LIST LIST1(JK,KQQQQQQ);                                                 00106300
LIST LIST2(FOR DX1:=1 STEP 1 UNTIL M DO X[DX1]);                        00106400
LIST LIST4(ALPHA1,FQQQQQQ[1],ALPHAQQ,A[2],FQQQQQQ[2],A[2],A[3],FQQQQQQ[300106600
],A[3],HAMU0,HASI);                                                     00106700
LIST LIST5(AAA,AAA);                                                    00106800
LIST LIST6(FOR DX1:=I1 STEP 1 UNTIL I3 DO CD[DX1],FOR DX1:=I1 STEP 1    00106900
 UNTIL I3 DO CD0[DX1]);                                                 00107000
LIST LIST8(FOR DX1:=I1 STEP 1 UNTIL I3 DO D[DX1]);                      00107200
LIST LIST9(CHI,IDF);                                                    00107300
LIST LIST10(CODE);                                                      00107400
BEGIN                                                                   00107500
LABEL L998,L2098,L2000,L2001,L2210,L2201,L2041,L2042,L2043,L2377,       00107600
SWITCH SWGO1:=L3071,L3170,L3171;                                        00107800
COMMENT  ID   0901HS 15 150    $BMD03S BIOLOGICAL ASSAY, PROBIT         00107900
 ANALYSIS                                                               00108000
BMD03S       BIOLOGICAL ASSAY - PROBIT ANALYSIS      JUNE  1, 1964      00108100
DIFF;                                                                   00108200
COMMENT  READ IN DATA;                                                  00108400
L998: READIN;                                                           00108500
SAVC:=A[3];                                                             00108600
JK:=1;                                                                  00108700
DO BEGIN                                                                00108800
  IK:=1+8|(JK-1);                                                       00109000
  COMMENT  TRANSFORM DOSE.;                                             00109100
  WRITE(TAPE6,FL9998,LIST1);                                            00109200
TRNGEN(DOSE,CONST,IK,M,ICODE,1,IERROR,X);                               00109300
  IF IERROR NEQ 0 THEN GO TO L2098;                                     00109400
  IF A[2] LEQ 0 THEN GO TO L2000 ELSE GO TO L2001;                      00109600
  COMMENT  GET LEAST SQUARES FIT;                                       00109700
  L2000: I1:=1;                                                         00109800
  I:=1;                                                                 00109900
  DO BEGIN                                                              00110000
    IF SMP[I] LEQ 0 THEN GO TO L2210;                                   00110200
    TEMP[I1]:=SMY[I];                                                   00110300
    RX[I1]:=X[I];                                                       00110400
    M2:=I1;                                                             00110500
    I1:=I1+1;                                                           00110600
  LINREG(RX,TEMP,M2,A[2],A[1]);                                         00110800
  L2001: MNVAR(X,M,XAV,XVAR);                                           00110900
  WRITE(TAPE6,FL9988,LIST3);                                            00111000
  IF A[2] GTR 0 THEN GO TO L2201;                                       00111100
  WRITE(TAPE6,FL46);                                                    00111200
  L2201: I:=1;                                                          00111400
  DO BEGIN                                                              00111500
    X[I]:=X[I]-XAV END UNTIL (I:=(I+1)) GTR M;                          00111600
  A[1]:=A[1]+A[2]|XAV;                                                  00111700
  COMMENT  SOLVE FOR MAXIMUM LIKLIHOOD BY NEWTONS METHOD;               00111800
  NPR:=IPR;                                                             00112000
  COMMENT  FIRST TIME GET BEST APPROX TO ALPHA, BETA;                   00112100
  CG1:=CG;                                                              00112200
  CG:=1;                                                                00112300
  NK:=1;                                                                00112400
    IF CG GEQ 0 THEN GO TO L2041;                                       00112600
    NV:=3;                                                              00112700
    GO TO L2042;                                                        00112800
    L2041: NV:=2;                                                       00112900
    L2042: NIT:=50;                                                     00113000
    CG:=CG1;                                                            00113200
    IF (XPR:=(ER)) LSS 0 THEN GO TO L2098 ELSE IF XPR=0 THEN GO TO      00113300
     L2043;                                                             00113400
    L2043: IF CG1 GEQ 0 THEN GO TO L2377;                               00113500
    END UNTIL (NK:=(NK+1)) GTR 2;                                       00113600
DIFF;                                                                   00113800
  CG:=CG1;                                                              00113900
  HAMU:=(5-A[1])/A[2];                                                  00114000
  ALPHAQQ:=A[1]-A[2]|XAV;                                               00114100
  ALPHA1:=A[1];                                                         00114200
  HAMU0:=(5-A[1])/A[2];                                                 00114400
  HASI:=1/A[2];                                                         00114500
  COMMENT  COMPUTR COVARIANCE MATRICES;                                 00114600
  IF CG1 LSS 0 THEN GO TO L305 ELSE GO TO L207;                         00114700
  COMMENT  TWO DIMENSION CASE;                                          00114800
  NN:=4;                                                                00115000
  AAA:=(".     ");                                                      00115100
  IDF:=M-2;                                                             00115200
  DC[4]:=DC[5];                                                         00115300
  DC[3]:=DC[2];                                                         00115400
  DV[4]:=-(HAMU|(HAMU|DC[1]+2|DC[2])+DC[5])/HASI*4+2|(HAMU|FQQQQQQ[1]+FQ00115600
QQQQQ[2])/HASI*3;                                                       00115700
  DV[2]:=(HAMU|DC[1]+DC[2])/HASI*3;                                     00115800
  DV[3]:=DV[2];                                                         00115900
  DC0[1]:=-DC[1];                                                       00116000
  DC0[4]:=-DC[4]-2|DC[2]|XAV-DC[1]|XAV*2;                               00116200
  DC0[3]:=DC0[2];                                                       00116300
  GO TO L306;                                                           00116400
  COMMENT  THREE DIMENSION CASE;                                        00116500
  L305: N1:=3;                                                          00116600
  AAA:=(", C.  ");                                                      00116800
  IDF:=M-3;                                                             00116900
  IF SN[M+1] LEQ 0 THEN GO TO L3051;                                    00117000
  IDF:=IDF+1;                                                           00117100
  L3051: DV[1]:=-DC[1]/HASI*2;                                          00117200
QQQQQ[2])/HASI*3;                                                       00117400
  DV[9]:=-DC[9];                                                        00117500
  DV[2]:=(HAMU|DC[1]+DC[2])/HASI*3;                                     00117600
  DV[4]:=DV[2];                                                         00117700
  DV[3]:=-DC[3]/HASI;                                                   00117800
  DV[6]:=(HAMU|DC[3]+DC[6])/HASI*2;                                     00118000
  DV[8]:=DV[6];                                                         00118100
  DC0[1]:=-DC[1];                                                       00118200
  DC0[2]:=-DC[1]|XAV-DC[4];                                             00118300
  DC0[3]:=-DC[3];                                                       00118400
  DC0[6]:=-DC[3]|XAV-DC[6];                                             00118600
  DC0[9]:=-DC[9];                                                       00118700
  DC0[4]:=DC0[2];                                                       00118800
  DC0[7]:=DC0[3];                                                       00118900
  DC0[8]:=DC0[6];                                                       00119000
  DO BEGIN                                                              00119200
    DC[I]:=-DC[I] END UNTIL (I:=(I+1)) GTR NN;                          00119300
INVERT(DC,CD,1,N1,DET,TEMP[20]);                                        00119400
INVERT(DV,D,1, N1,DET,TEMP[20]);                                        00119500
INVERT(DC0,CD0,1,N1,DET,TEMP[20]);                                      00119600
  M1:=M;                                                                00119800
  IF CG1 GEQ 0 THEN GO TO L3081;                                        00119900
  M1:=M+1;                                                              00120000
  L3081: CHI:=0;                                                        00120100
  I:=1;                                                                 00120200
    RI:=R[I];                                                           00120400
    PP:=A[3]+P[I]|(1-A[3]);                                             00120500
    CHI:=CHI+(RI-SN[I]|PP)*2/(PP-PP*2)/SN[I];                           00120600
    END UNTIL (I:=(I+1)) GTR M;                                         00120700
  COMMENT  PRINT RESULTS;                                               00120800
  WRITE(TAPE6,FL9980,LIST4);                                            00121000
  WRITE(TAPE6,FL9981,LIST5);                                            00121100
  I:=1;                                                                 00121200
  DO BEGIN                                                              00121300
    I1:=1+N1|(I-1);                                                     00121400
    GO TO SWGO1[N1];                                                    00121600
    L3170: WRITE(TAPE6,FL9883,LIST6);                                   00121700
    GO TO L3071;                                                        00121800
    L3171: WRITE(TAPE6,FL9884,LIST6);                                   00121900
    L3071: END UNTIL (I:=(I+1)) GTR N1;                                 00122000
  I:=1;                                                                 00122200
  DO BEGIN                                                              00122300
    I1:=1+N1|(I-1);                                                     00122400
    I3:=N1|I;                                                           00122500
    WRITE(TAPE6,FL9983,LIST8);                                          00122600
  WRITE(TAPE6,FL9984,LIST9);                                            00122800
  L2098: WRITE(TAPE6,FL9989,LIST1);                                     00122900
  A[2]:=0;                                                              00123000
  END UNTIL (JK:=(JK+1)) GTR KQQQQQQ;                                   00123100
WRITE(TAPE6,FL38,LIST10);                                               00123200
COMMENT  NORMAL OUTPUT FORMAT;                                          00123400
COMMENT  ERROR OUTPUT FORMAT;                                           00123500
 END END;                                                               00123600
COMMENT  INITIALIZING BLOCK;                                            00123700
  XPR:=Q:=K:=0;                                                         00123800
END.                                                                    00124000
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
