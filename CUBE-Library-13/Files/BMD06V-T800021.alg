BEGIN                                                                   00001000
% BMD06V ---- GENERAL LINEAR HYPOTHESIS WITH CONTRASTS (ALGOL)          00001100
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
FORMAT FMT(A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6,A6);                        00003200
   TAPE8,TAPE9,TAPE10,TAPE11,TAPE12,TAPE13,TAPE14,TAPE15,TAPE16;        00003400
LABEL  FINIS;                                                           00003500
REAL ARRAY  DATA[0:63,0:511];    COMMENT USED WITH DATA STATEMENTS ONLY;00003600
REAL Q,XPR;    INTEGER K;                                               00003700
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
REAL ARRAY DATAQQQ[0:100],C[0:61,0:61],A[0:100,0:61],XM[0:61],B[0:61],  00005500
           CI[0:61,0:61],P[0:6];                                        00005600
INTEGER NEW,NT,N1,N2,N3,N4,ND,NTAPE,NC,NPP,NT1,NNT,MD,MMT,NCOV,NT2,NP,  00005800
        NO,NH;                                                          00005900
REAL OON,ON,PR,RD,RE;                                                   00006000
PROCEDURE  FIXFORM(FMT);                                                00006100
FORMAT FMT;                                                             00006200
STREAM PROCEDURE FORM(FT,CODE);                                         00006400
          BEGIN                                                         00006500
               DI~FT;                                                   00006600
               SI~CODE;                                                 00006700
               DS~15 WDS;                                               00006800
LABEL RETURN,ERROR;                                                     00007000
FORMAT         F1(72A1);                                                00007100
        ARRAY  CODE[0:14];                                              00007200
ALPHA ARRAY    F[0:71];                                                 00007300
INTEGER        I,K,REP,WIDTH,D,N;                                       00007400
DEFINE    S=CODE[K].[1:1]#,                                             00007600
          C=CODE[K].[2:4]#,                                             00007700
          W=CODE[K].[6:6]#,                                             00007800
         D2=CODE[K].[16:4]#,                                            00007900
         D1=CODE[K].[20:4]#,                                            00008000
         W1=CODE[K].[28:4]#,                                            00008200
       SKIP=CODE[K].[32:6]#,                                            00008300
     REPEAT=CODE[K].[38:10]#,                                           00008400
     NUMBER=F[I]="1" OR F[I]="2" OR F[I]="3" OR F[I]="4" OR F[I]="5" OR 00008500
            F[I]="6" OR F[I]="7" OR F[I]="8" OR F[I]="9" OR F[I]="0" #, 00008600
OR F[I] ="R" #;  INTEGER TR,L1,L2,L3,L4;                                00008800
BOOLEAN TRUTH;                                                          00008900
DEFINE CHECK= BEGIN L1~L1+1;                                            00009000
                    L2~L2+1;                                            00009100
                    L3~L3+1;                                            00009200
              END  # ;                                                  00009400
BOOLEAN FIRST;                                                          00009500
INTEGER R1,R2,R3,R4;                                                    00009600
READ(TAPE5,F1,FOR I ~ 0 STEP 1 UNTIL 71 DO F[I]);                       00009700
FIRST~TRUE;                                                             00009800
TRUTH~FALSE;                                                            00010000
L1~L2~L3~L4~TR~0;                                                       00010100
               FOR I~0 STEP 1 UNTIL 71 DO                               00010200
          BEGIN  LABEL NEXT;                                            00010300
IF F[I]="(" THEN                                                        00010400
                     S~1; SKIP~1;CODE[K].[6:26]~0;                      00010600
C~4;                                                                    00010700
                     NN~I;                                              00010800
IF FIRST THEN REPEAT ~0                                                 00010900
                            ELSE                                        00011000
                          I~I-1;                                        00011200
                          IF NUMBER                                     00011300
                          THEN                                          00011400
                              BEGIN                                     00011500
                                   I~I-1;                               00011600
                                   THEN BEGIN                           00011800
                                             TENS~F[I];                 00011900
                                             ONES~F[I+1];               00012000
                                        END                             00012100
                                   ELSE BEGIN                           00012200
                                             ONES~F[I+1];               00012400
                                        END;                            00012500
                                   REP~10|TENS+ONES;                    00012600
                                   REPEAT~REP;                          00012700
                              END                                       00012800
                                   TRUTH~TRUE;                          00013000
                                   TR~TR+1;                             00013100
IF TR =1 THEN BEGIN L1~0;R1~REP END ;                                   00013200
IF TR =2 THEN BEGIN L2~0;R2~REP END ;                                   00013300
IF TR =3 THEN BEGIN L3~0;R3~REP END ;                                   00013400
                     END;                                               00013600
                          I~NN;                                         00013700
                          K~K+1;                                        00013800
IF TRUTH THEN CHECK;                                                    00013900
FIRST~FALSE;                                                            00014000
                END;                                                    00014200
               IF F[I]="/" THEN                                         00014300
             BEGIN                                                      00014400
                 S~1; C~6; SKIP~1;                                      00014500
                  CODE[K].[6:26]~0; REPEAT~0;                           00014600
                  K~K+1; GO TO NEXT;                                    00014800
             END;                                                       00014900
               IF F[I]="," THEN GO TO NEXT;                             00015000
               IF F[I]=" " THEN GO TO NEXT;                             00015100
IF F[I]=")" THEN                                                        00015200
LABEL DSK;                                                              00015400
                   IF NOT TRUTH THEN                                    00015500
                        BEGIN                                           00015600
                        S~1; SKIP~0;C~6;                                00015700
                        CODE[K].[6:26]~0;                               00015800
                        K~K+1;                                          00016000
                   END;                                                 00016100
IF TR = 1 THEN IF R1 !0 THEN GO TO DSK;                                 00016200
IF TR = 2 THEN IF R2!0 THEN GO TO DSK;                                  00016300
IF TR=3 THEN IF R3!0 THEN GO TO DSK;                                    00016400
S~1; SKIP~0; C~6;                                                       00016600
CODE[K].[6:26]~0;                                                       00016700
REPEAT~1;                                                               00016800
K~K+1;                                                                  00016900
IF TRUTH THEN CHECK;                                                    00017000
                   S~1;C~0;SKIP~0;                                      00017200
                   CODE[K].[6:26]~0;                                    00017300
                   IF TRUTH THEN BEGIN IF TR=1 THEN REP~L1;             00017400
                                       IF TR=2 THEN REP~L2;             00017500
                                       IF TR=3 THEN REP~L3;             00017600
                                 END                                    00017800
                            ELSE REP~K;                                 00017900
TR~TR-1;                                                                00018000
IF TR=0 THEN TRUTH~FALSE;                                               00018100
                        REPEAT~REP;                                     00018200
IF TRUTH THEN CHECK;                                                    00018400
                   GO TO NEXT                                           00018500
              END;                                                      00018600
               IF CHAR THEN                                             00018700
             BEGIN                                                      00018800
                  I~I-1;                                                00019000
                  IF NUMBER THEN                                        00019100
                BEGIN                                                   00019200
LABEL IMIN;                                                             00019300
                     I~I-1;                                             00019400
IF I<0 THEN GO TO IMIN;                                                 00019600
                     IF NUMBER THEN                                     00019700
                   BEGIN                                                00019800
                        TENS~F[I];                                      00019900
                        ONES~F[I+1];                                    00020000
;                                                                       00020200
IMIN:BEGIN                                                              00020300
                        ONES~F[I+1];                                    00020400
REP~10|TENS+ONES;                                                       00020500
                   END;                                                 00020600
               END ELSE REP~1;                                          00020800
          IF REP=0 THEN REP~1;                                          00020900
I~I+2;                                                                  00021000
                     IF NUMBER THEN                                     00021100
                   BEGIN                                                00021200
                        IF NUMBER THEN                                  00021400
                      BEGIN                                             00021500
                           WT~F[I-1];                                   00021600
                           WO~F[I];                                     00021700
WIDTH~10|WT+WO;                                                         00021800
                      ELSE                                              00022000
                      BEGIN                                             00022100
                           WO~F[I-1];                                   00022200
WIDTH~WO;                                                               00022300
          I~I-1;                                                        00022400
                   END                                                  00022600
                   ELSE                                                 00022700
                        GO TO ERROR;                                    00022800
I~I+1;                                                                  00022900
                     IF F[I]="." THEN                                   00023000
                        I~I+1;                                          00023200
                        IF NUMBER THEN                                  00023300
                      BEGIN                                             00023400
                           I~I+1;                                       00023500
                           IF NUMBER THEN                               00023600
                              DEC0~F[I-1];                              00023800
                              DEC1~F[I];                                00023900
D~10|DEC0+DEC1;                                                         00024000
                         END                                            00024100
                         ELSE                                           00024200
                              DEC0~0;                                   00024400
                              DEC1~F[I-1];                              00024500
D~DEC1;                                                                 00024600
                         END;                                           00024700
                      END                                               00024800
                   END;                                                 00025000
I~NN;                                                                   00025100
                     IF F[I]="X" THEN                                   00025200
                   BEGIN                                                00025300
                        REPEAT~REP;                                     00025400
W~WO;                                                                   00025600
SKIP~WO;                                                                00025700
                        W1~WIDTH DIV 64;                                00025800
                        W2~0;                                           00025900
                        D1~0;                                           00026000
                        S~0;                                            00026200
                        C~2;                                            00026300
                        K~K+1;                                          00026400
IF TRUTH THEN CHECK;                                                    00026500
                        GO TO NEXT;                                     00026600
                     IF F[I]="A" THEN                                   00026800
                   BEGIN                                                00026900
                        S~0;                                            00027000
                        C~4;                                            00027100
                        REPEAT~REP;                                     00027200
                        D1~0;                                           00027400
                        D2~0;                                           00027500
                        W~WIDTH;                                        00027600
                        IF WIDTH<6 THEN                                 00027700
                      BEGIN                                             00027800
                           SKIP~0;                                      00028000
                      END                                               00028100
                      ELSE                                              00028200
                      BEGIN                                             00028300
                           W1~6;                                        00028400
                      END;                                              00028600
                        K~K+1;                                          00028700
IF TRUTH THEN CHECK;                                                    00028800
                        GO TO NEXT;                                     00028900
                   END;                                                 00029000
                   BEGIN                                                00029200
                        S~0;                                            00029300
                        C~6;                                            00029400
                        REPEAT~REP;                                     00029500
                        W~WIDTH;                                        00029600
                        D2~0;                                           00029800
                        IF WIDTH>16 THEN SKIP~WIDTH-16                  00029900
                                    ELSE SKIP~0;                        00030000
                        IF WIDTH<8 THEN                                 00030100
                      BEGIN                                             00030200
                           W2~0;                                        00030400
                      END                                               00030500
                      ELSE                                              00030600
                      BEGIN                                             00030700
                           W1~8;                                        00030800
                      END;                                              00031000
                        K~K+1;                                          00031100
IF TRUTH THEN CHECK;                                                    00031200
                        GO TO NEXT;                                     00031300
                   END;                                                 00031400
                   BEGIN                                                00031600
                        S~0;                                            00031700
                       C~15;                                            00031800
                        REPEAT~REP;                                     00031900
                        W~WIDTH;                                        00032000
                       IF D{8 THEN                                      00032200
                      BEGIN                                             00032300
                           D1~D;                                        00032400
                           D2~0;                                        00032500
                      END                                               00032600
                      BEGIN                                             00032800
                           D1~8;                                        00032900
                           D2~D-8;                                      00033000
                      END;                                              00033100
                        IF (N~WIDTH-D-1)>16 THEN SKIP~WIDTH-D-17        00033200
                        IF N>8 THEN                                     00033400
                      BEGIN                                             00033500
                           W1~8;                                        00033600
                           W2~N-8-SKIP;                                 00033700
                      END                                               00033800
                      BEGIN                                             00034000
                           W1~N;                                        00034100
                           W2~0;                                        00034200
                      END;                                              00034300
                        K~K+1;                                          00034400
                        GO TO NEXT;                                     00034600
                END;                                                    00034700
             END;                                                       00034800
NEXT:     END;                                                          00034900
               FORM(FMT,CODE[0]);                                       00035000
ERROR:                                                                  00035200
GO TO FINIS;                                                            00035300
RETURN: END OF FIXFORM;                                                 00035400
PROCEDURE VFCHCK(NVF);                                                  00035500
  INTEGER NVF;                                                          00035600
FORMAT FL4000(/" ",X23,                                                 00035800
 "NUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFIED, ASSUMED ",     00035900
 "TO BE 1.");                                                           00036000
LABEL L10,L20,L0;                                                       00036100
COMMENT  VFCHCK    SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE    00036200
IF NVF GTR 0 THEN GO TO L20;                                            00036400
L10: WRITE(TAPE6,FL4000);                                               00036500
NVF:=1;                                                                 00036600
GO TO L0;                                                               00036700
L20: IF NVF LEQ 10 THEN GO TO L0 ELSE GO TO L10;                        00036800
PROCEDURE TPWD(NT1,NT2);                                                00037000
  INTEGER NT1,NT2;                                                      00037100
BEGIN                                                                   00037200
FORMAT FL49(" ERROR ON TAPE ASSIGNMENT");                               00037300
LABEL L40,L12,L19,L24,L0;                                               00037400
 26, 1963;                                                              00037600
IF (XPR:=(NT1)) GTR 0 THEN GO TO L12 ELSE IF XPR LSS 0 THEN GO TO       00037700
 L40;                                                                   00037800
NT1:=5;                                                                 00037900
L12: IF NT1=NT2 THEN GO TO L19;                                         00038000
REWIND(FILESW[NT2]);                                                    00038200
GO TO L19;                                                              00038300
COMMENT  17 CALL REMOVE(NT2);                                           00038400
L19: IF NT1=5 THEN GO TO L24;                                           00038500
IF NT1=6 THEN GO TO L40;                                                00038600
L24: NT2:=NT1;                                                          00038800
GO TO L0;                                                               00038900
L40: WRITE(TAPE6,FL49);                                                 00039000
GO TO FINIS;                                                            00039100
L0:  END;                                                               00039200
BEGIN                                                                   00039400
OWN INTEGER DX1;                                                        00039500
OWN INTEGER I,J,L;                                                      00039600
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  VFCHCK;                    00039700
FORMAT FL900(/" TRANSFORMATION",X4,"VARIABLE NO.",X6,"TYPE OF",X9,      00039800
FL901("    CARD NO.",X9,"ASSIGNED",X5,"TRANSFORMATION",X4,"TRANSFORMED",00040000
X4,"OR  CONSTANT"),                                                     00040100
FL905(A6,R3.0,R2.0,R3.0,R6.0),                                          00040200
FL906(/" ERROR ON TRANSFORMATION CARD"),                                00040300
FL902(" ",X4,I4,X13,I4,X13,I3,X13,I3,X10,I6),                           00040400
FL912(12A6),                                                            00040600
FL904(/" "//" TRANSFORMATION OF THE TYPE",I3,"  CANNOT BE PERFORMED ON"/00040700
" VARIABLE",I3,"  CASE",I5,".  THE VALUE IS",R12.5),                    00040800
FL908(/" "//" ",X10,"INPUT  DATA  AS  READ  FROM  CARDS"/" CASE NO."),  00040900
FL909(I6,8R13.5),                                                       00041000
FL907(12I6);                                                            00041200
LIST LIST1(RD,FOR DX1:=1 STEP 1 UNTIL 4 DO A[I,DX1]);                   00041300
LIST LIST2(I,N1,N2,N3);                                                 00041400
LIST LIST3(I,N1,N2,N3,A[I,4]);                                          00041500
LIST LIST4(I,N1,N2,N3,N4);                                              00041600
LIST LIST5(FOR DX1:=1 STEP 1 UNTIL ND DO FMT[DX1]);                     00041800
LIST LIST55(FOR DX1 ~ 1 STEP 1 UNTIL NPP DO DATAQQQ[DX1]);              00041900
LIST LIST6(N2,N3,I,DATAQQQ[N3]);                                        00042000
LIST LIST7(N2,N4,I,DATAQQQ[N4]);                                        00042100
LIST LIST8(FOR DX1:=1 STEP 1 UNTIL NNT DO DATAQQQ[DX1],PR);             00042200
LIST LIST10(FOR DX1:=N1 STEP 1 UNTIL N2 DO DATAQQQ[DX1]);               00042400
BEGIN                                                                   00042500
LABEL L20,L18,L12,L14,L16,L75,L77,L81,L82,L84,L260,L250,L110,L140,      00042600
 L150,L160,L170,L180,L190,L200,L210,L220,L230,L240,L113,L115,L114,      00042700
 L118,L120,L130,L156,L163,L164,L166,L167,L242,L295,L1000,L298,L299,     00042800
SWITCH SWGO1:=L110,L110,L110,L140,L150,L160,L170,L180,L190,L200,L210,   00043000
 L220,L230,L240;                                                        00043100
SWITCH SWGO2:=L118,L120,L130;                                           00043200
COMMENT  READ      SUBROUTINE READ FOR BMD06V             DECEMBER 11,  00043300
 1963;                                                                  00043400
VALUE ARGRD;                                                            00043600
REAL ARGRD;                                                             00043700
ASN:=ARCTAN(ARGRD/SQRT(1-ARGRD*2));                                     00043800
NEW:=0;                                                                 00043900
IF NT LEQ 0 THEN GO TO L20;                                             00044000
WRITE(TAPE6,FL900);                                                     00044200
WRITE(TAPE6,FL901);                                                     00044300
PR:=("TRNGEN");                                                         00044400
I:=1;                                                                   00044500
DO BEGIN                                                                00044600
IF RD = PR THEN GO TO L12;                                              00044800
  WRITE(TAPE6,FL906);                                                   00044900
  GO TO FINIS;                                                          00045000
  L12: N1:=INT(A[I,1]);                                                 00045100
  N2:=INT(A[I,2]);                                                      00045200
  N4:=INT(A[I,4]);                                                      00045400
  IF N2 GEQ 8 THEN GO TO L14;                                           00045500
  WRITE(TAPE6,FL902,LIST2);                                             00045600
  GO TO L18;                                                            00045700
  L14: IF N2 GEQ 10 THEN GO TO L16;                                     00045800
  GO TO L18;                                                            00046000
  L16: WRITE(TAPE6,FL902,LIST4);                                        00046100
  L18: END UNTIL (I:=(I+1)) GTR NT;                                     00046200
L20: VFCHCK(ND);                                                        00046300
ND:=ND|12;                                                              00046400
READ(TAPE5,FL912,LIST5)[FINIS];                                         00046600
FIXFORM(FMT); % PROCEDURE TO READ FORMAT CARD ************************  00046700
J:=1;                                                                   00046800
DO BEGIN                                                                00046900
  I:=1;                                                                 00047000
    C[I,J]:=0 END UNTIL (I:=(I+1)) GTR NNT END UNTIL (J:=(J+1)) GTR     00047200
 NNT;                                                                   00047300
I:=1;                                                                   00047400
DO BEGIN                                                                00047500
  IF NTAPE GTR 0 THEN GO TO L75;                                        00047600
  GO TO L77;                                                            00047800
L75: READ(FILESW[NTAPE],FMT,LIST55)[FINIS];                             00047900
  L77: IF NC GTR 0 THEN GO TO L82;                                      00048000
  L81: PR:=1;                                                           00048100
  GO TO L84;                                                            00048200
  IF PR=0 THEN GO TO L81;                                               00048400
  L84: IF NT LEQ 0 THEN GO TO L260;                                     00048500
  J:=1;                                                                 00048600
  DO BEGIN                                                              00048700
    N1:=INT(A[J,1]);                                                    00048800
    N3:=INT(A[J,3]);                                                    00049000
    N4:=INT(A[J,4]);                                                    00049100
    GO TO SWGO1[N2];                                                    00049200
    L110: IF (XPR:=(DATAQQQ[N3])) GTR 0 THEN GO TO L115 ELSE IF XPR LSS 00049300
     0 THEN GO TO L113;                                                 00049400
    DATAQQQ[N1]:=1;                                                     00049600
    GO TO L250;                                                         00049700
    L113: WRITE(TAPE6,FL904,LIST6);                                     00049800
    NEW:=NEW+1;                                                         00049900
    L114: DATAQQQ[N1]:=0;                                               00050000
    L115: GO TO SWGO2[N2];                                              00050200
    L118: DATAQQQ[N1]:=SQRT(DATAQQQ[N3]);                               00050300
    GO TO L250;                                                         00050400
    L120: DATAQQQ[N1]:=SQRT(DATAQQQ[N3])+SQRT(DATAQQQ[N3]+1);           00050500
    GO TO L250;                                                         00050600
    GO TO L250;                                                         00050800
    L140: DATAQQQ[N1]:=EXP(DATAQQQ[N3]);                                00050900
    GO TO L250;                                                         00051000
    L150: IF DATAQQQ[N3] LSS 0 THEN GO TO L113;                         00051100
    RD:=SQRT(DATAQQQ[N3]);                                              00051200
     L156;                                                              00051400
    DATAQQQ[N1]:=ASN(RD);                                               00051500
    GO TO L250;                                                         00051600
    L156: DATAQQQ[N1]:=1.5708;                                          00051700
    GO TO L250;                                                         00051800
    RD:=SQRT(DATAQQQ[N3]/OON);                                          00052000
    RE:=SQRT((DATAQQQ[N3]+1)/OON);                                      00052100
    IF (XPR:=(RD-1)) GTR 0 THEN GO TO L113 ELSE IF XPR=0 THEN GO TO     00052200
     L163;                                                              00052300
    RD:=ASN(RD);                                                        00052400
    L163: RD:=1.5708;                                                   00052600
    L164: IF (XPR:=(RE-1)) GTR 0 THEN GO TO L113 ELSE IF XPR=0 THEN GO  00052700
     TO L166;                                                           00052800
    RE:=ASN(RE);                                                        00052900
    GO TO L167;                                                         00053000
    L167: DATAQQQ[N1]:=RD+RE;                                           00053200
    GO TO L250;                                                         00053300
    L170: IF DATAQQQ[N3]=0 THEN GO TO L113;                             00053400
    DATAQQQ[N1]:=1/DATAQQQ[N3];                                         00053500
    GO TO L250;                                                         00053600
    GO TO L250;                                                         00053800
    L190: DATAQQQ[N1]:=DATAQQQ[N3]|A[J,4];                              00053900
    GO TO L250;                                                         00054000
    L200: DATAQQQ[N1]:=DATAQQQ[N3]*N4;                                  00054100
    GO TO L250;                                                         00054200
    GO TO L250;                                                         00054400
    L220: DATAQQQ[N1]:=DATAQQQ[N3]-DATAQQQ[N4];                         00054500
    GO TO L250;                                                         00054600
    L230: DATAQQQ[N1]:=DATAQQQ[N3]|DATAQQQ[N4];                         00054700
    GO TO L250;                                                         00054800
    WRITE(TAPE6,FL904,LIST7);                                           00055000
    NEW:=NEW+1;                                                         00055100
    GO TO L250;                                                         00055200
    L242: DATAQQQ[N1]:=DATAQQQ[N3]/DATAQQQ[N4];                         00055300
    L250: END UNTIL (J:=(J+1)) GTR NT;                                  00055400
  DO BEGIN                                                              00055600
    XM[J]:=XM[J]+DATAQQQ[J];                                            00055700
    L:=J;                                                               00055800
    DO BEGIN                                                            00055900
      C[J,L]:=C[J,L]+DATAQQQ[J]|DATAQQQ[L]|PR END UNTIL (L:=(L+1)) GTR  00056000
  WRITE(FILESW[NT1],*,LIST8);                                           00056200
  END UNTIL (I:=(I+1)) GTR NO;                                          00056300
CLOSE(FILESW[NT1],*);                                                   00056400
REWIND(FILESW[NT1]);                                                    00056500
IF NEW GTR 0 THEN GO TO L295;                                           00056600
I:=1;                                                                   00056800
DO BEGIN                                                                00056900
  N2:=I+1;                                                              00057000
  J:=N2;                                                                00057100
  DO BEGIN                                                              00057200
 GTR N1;                                                                00057400
L295: IF MD LEQ 0 THEN GO TO L1000;                                     00057500
WRITE(TAPE6,FL908);                                                     00057600
IF NC GTR 0 THEN GO TO L298;                                            00057700
N3:=NNT;                                                                00057800
L298: N3:=NNT+1;                                                        00058000
L299: I:=1;                                                             00058100
DO BEGIN                                                                00058200
  READ(FILESW[NT1],*,LIST8)[FINIS];                                     00058300
  IF NC LEQ 0 THEN GO TO L305;                                          00058400
  L305: N1:=1;                                                          00058600
  N2:=0;                                                                00058700
  L310: IF (N3-N2) GTR 8 THEN GO TO L320;                               00058800
  N2:=N3;                                                               00058900
  GO TO L325;                                                           00059000
  L325: IF N1 GTR 1 THEN GO TO L335;                                    00059200
  WRITE(TAPE6,FL909,LIST9);                                             00059300
  GO TO L340;                                                           00059400
  L335: WRITE(TAPE6,FL910,LIST10);                                      00059500
  L340: IF N3 LEQ N2 THEN GO TO L350;                                   00059600
  GO TO L310;                                                           00059800
  L350: END UNTIL (I:=(I+1)) GTR NO;                                    00059900
REWIND(FILESW[NT1]);                                                    00060000
L1000: NTAPE:=NT1;                                                      00060100
 END END;                                                               00060200
  VALUE N;                                                              00060400
  INTEGER N;                                                            00060500
  REAL D;                                                               00060600
  INTEGER ARRAY L[0],M[0];                                              00060700
  REAL ARRAY A[0,0];                                                    00060800
OWN INTEGER KQQQQQQ,I,J;                                                00061000
OWN REAL BIGA,HOLD;                                                     00061100
LABEL L20,L35,L45,L55,L65,L75,L100,L120,L0;                             00061200
COMMENT  INVERT      SUBROUTINE INVERT FOR BMD06V                       00061300
 1 8-29-63;                                                             00061400
KQQQQQQ:=1;                                                             00061600
DO BEGIN                                                                00061700
  L[KQQQQQQ]:=KQQQQQQ;                                                  00061800
  M[KQQQQQQ]:=KQQQQQQ;                                                  00061900
  BIGA:=A[KQQQQQQ,KQQQQQQ];                                             00062000
  DO BEGIN                                                              00062200
    J:=KQQQQQQ;                                                         00062300
    DO BEGIN                                                            00062400
      IF ABS(BIGA) GEQ ABS(A[I,J]) THEN GO TO L20;                      00062500
      BIGA:=A[I,J];                                                     00062600
      M[KQQQQQQ]:=J;                                                    00062800
      L20: END UNTIL (J:=(J+1)) GTR N END UNTIL (I:=(I+1)) GTR N;       00062900
  J:=L[KQQQQQQ];                                                        00063000
  IF L[KQQQQQQ] LEQ KQQQQQQ THEN GO TO L35;                             00063100
  I:=1;                                                                 00063200
    HOLD:=-A[KQQQQQQ,I];                                                00063400
    A[KQQQQQQ,I]:=A[J,I];                                               00063500
    A[J,I]:=HOLD END UNTIL (I:=(I+1)) GTR N;                            00063600
  L35: I:=M[KQQQQQQ];                                                   00063700
  IF M[KQQQQQQ] LEQ KQQQQQQ THEN GO TO L45;                             00063800
  DO BEGIN                                                              00064000
    HOLD:=-A[J,KQQQQQQ];                                                00064100
    A[J,KQQQQQQ]:=A[J,I];                                               00064200
    A[J,I]:=HOLD END UNTIL (J:=(J+1)) GTR N;                            00064300
  L45: I:=1;                                                            00064400
    IF I=KQQQQQQ THEN GO TO L55;                                        00064600
    A[I,KQQQQQQ]:=A[I,KQQQQQQ]/(-A[KQQQQQQ,KQQQQQQ]);                   00064700
    L55: END UNTIL (I:=(I+1)) GTR N;                                    00064800
  I:=1;                                                                 00064900
  DO BEGIN                                                              00065000
    DO BEGIN                                                            00065200
      IF I=KQQQQQQ THEN GO TO L65;                                      00065300
      IF J=KQQQQQQ THEN GO TO L65;                                      00065400
      A[I,J]:=A[I,KQQQQQQ]|A[KQQQQQQ,J]+A[I,J];                         00065500
      L65: END UNTIL (J:=(J+1)) GTR N END UNTIL (I:=(I+1)) GTR N;       00065600
  DO BEGIN                                                              00065800
    IF J=KQQQQQQ THEN GO TO L75;                                        00065900
    A[KQQQQQQ,J]:=A[KQQQQQQ,J]/A[KQQQQQQ,KQQQQQQ];                      00066000
    L75: END UNTIL (J:=(J+1)) GTR N;                                    00066100
  D:=D|A[KQQQQQQ,KQQQQQQ];                                              00066200
  END UNTIL (KQQQQQQ:=(KQQQQQQ+1)) GTR N;                               00066400
KQQQQQQ:=N;                                                             00066500
L100: KQQQQQQ:=(KQQQQQQ-1);                                             00066600
IF KQQQQQQ LEQ 0 THEN GO TO L0;                                         00066700
I:=L[KQQQQQQ];                                                          00066800
J:=1;                                                                   00067000
DO BEGIN                                                                00067100
  HOLD:=A[J,KQQQQQQ];                                                   00067200
  A[J,KQQQQQQ]:=-A[J,I];                                                00067300
  A[J,I]:=HOLD END UNTIL (J:=(J+1)) GTR N;                              00067400
IF J LEQ KQQQQQQ THEN GO TO L100;                                       00067600
I:=1;                                                                   00067700
DO BEGIN                                                                00067800
  HOLD:=A[KQQQQQQ,I];                                                   00067900
  A[KQQQQQQ,I]:=-A[J,I];                                                00068000
GO TO L100;                                                             00068200
L0:  END;                                                               00068300
PROCEDURE ADJUST;                                                       00068400
BEGIN                                                                   00068500
OWN INTEGER DX1;                                                        00068600
FORMAT FL901(/" "//" ORIGINAL MEANS FOR DESIGN VARIABLES"),             00068800
FL902(/" VARIABLE NO.",I3,"  (COVARIATE)"),                             00068900
FL900(8R15.5),                                                          00069000
FL903(/" DEPENDENT VARIABLE"),                                          00069100
FL904(/" "//" CELL MEANS");                                             00069200
LIST LIST2(FOR DX1:=1 STEP 1 UNTIL MMMT DO DATAQQQ[DX1]);               00069400
LIST LIST3(FOR DX1:=1 STEP 1 UNTIL NNT DO DATAQQQ[DX1],PR);             00069500
LABEL L40,L45,L0;                                                       00069600
COMMENT  ADJUST      SUBROUTINE ADJUST FOR BMD06V                       00069700
 1 8-29-63;                                                             00069800
IF NCOV LEQ 0 THEN GO TO L40;                                           00070000
WRITE(TAPE6,FL901);                                                     00070100
I:=1;                                                                   00070200
DO BEGIN                                                                00070300
  KQQQQQQ:=NN[I];                                                       00070400
  DO BEGIN                                                              00070600
    DATAQQQ[J]:=C[J,KQQQQQQ]/C[J,J] END UNTIL (J:=(J+1)) GTR MMMT;      00070700
  WRITE(TAPE6,FL902,LIST1);                                             00070800
  WRITE(TAPE6,FL900,LIST2);                                             00070900
  END UNTIL (I:=(I+1)) GTR NCOV;                                        00071000
GO TO L45;                                                              00071200
L40: WRITE(TAPE6,FL904);                                                00071300
L45: J:=1;                                                              00071400
DO BEGIN                                                                00071500
  DATAQQQ[J]:=C[J,NNT]/C[J,J] END UNTIL (J:=(J+1)) GTR MMMT;            00071600
IF NCOV LEQ 0 THEN GO TO L0;                                            00071800
J:=1;                                                                   00071900
DO BEGIN                                                                00072000
  I:=1;                                                                 00072100
  DO BEGIN                                                              00072200
 NNT;                                                                   00072400
I:=1;                                                                   00072500
DO BEGIN                                                                00072600
  KQQQQQQ:=NN[I];                                                       00072700
  XM[KQQQQQQ]:=XM[KQQQQQQ]/ON END UNTIL (I:=(I+1)) GTR NCOV;            00072800
DO BEGIN                                                                00073000
  READ(FILESW[NT1],*,LIST3)[FINIS];                                     00073100
  J:=1;                                                                 00073200
  DO BEGIN                                                              00073300
    KQQQQQQ:=NN[J];                                                     00073400
   J+1)) GTR NCOV;                                                      00073600
  J:=1;                                                                 00073700
  DO BEGIN                                                              00073800
    L:=J;                                                               00073900
    DO BEGIN                                                            00074000
     NNT END UNTIL (J:=(J+1)) GTR NNT;                                  00074200
  WRITE(FILESW[NT2],*,LIST3);                                           00074300
  END UNTIL (I:=(I+1)) GTR NO;                                          00074400
REWIND(FILESW[NT1]);                                                    00074500
CLOSE(FILESW[NT2],*);                                                   00074600
NTAPE:=NT2;                                                             00074800
N1:=NNT-1;                                                              00074900
I:=1;                                                                   00075000
DO BEGIN                                                                00075100
  N2:=I+1;                                                              00075200
  DO BEGIN                                                              00075400
    C[J,I]:=C[I,J] END UNTIL (J:=(J+1)) GTR NNT END UNTIL (I:=(I+1))    00075500
 GTR N1;                                                                00075600
L0:  END;                                                               00075700
PROCEDURE MAINPRO;                                                      00075800
OWN REAL ARRAY FMT[0:120];                                              00076000
OWN INTEGER DX1;                                                        00076100
OWN INTEGER MTAPE,I,IJ,J,NDF,KQQQQQQ;                                   00076200
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  TPWD,READQQQ,ADJUST,       00076300
 INVERT;                                                                00076400
                   (A6,66R1.0/(X6,66R1.0)),                             00076600
                   (A6,33R2.0/(X6,33R2.0)),                             00076700
                   (A6,22R3.0/(X6,22R3.0)),                             00076800
                   (A6,16R4.0/(X6,16R4.0)),                             00076900
                   (A6,13R5.0/(X6,13R5.0)),                             00077000
FORMAT FL902(                                                           00077200
 " BMD06V - GENERAL LINEAR HYPOTHESIS WITH CONTRASTS - VERSION OF",     00077300
 " MAY 27, 1964/"," HEALTH SCIENCES COMPUTING FACILITY, UCLA"),         00077400
FL900(A6,A2,I2,I4,7I2,X42,I2),                                          00077500
FL901(" ERROR ON PROBLEM CARD OR DECK SET-UP"),                         00077600
I6//),                                                                  00077800
FL924(A6,33I2/(X6,33I2)),                                               00077900
FL904(A6,3I2,60I1),                                                     00078000
FL905(" ERROR ON CONTRAST CARD"),                                       00078100
FL906(/" "///" CONTRAST"/" CARD NO.",I3,X5,60I1),                       00078200
FL908(/" ROW",I3),                                                      00078400
FL909(8R15.5),                                                          00078500
FL910(/" "//" INVERTED CROSS PRODUCTS MATRIX"),                         00078600
FL921(/" "//" ",X19,"TABLE OF RESIDUALS"/" OBSERVATION",X5,"Y VALUE",X9,00078700
"Y ESTIMATE",X8,"RESIDUAL"),                                            00078800
FL911(/" "//" REGRESSION COEFFICIENTS"),                                00079000
FL912(/" STANDARD DEVIATIONS OF REGRESSION COEFFICIENTS"),              00079100
FL913(/" "//" VARIANCE OF ESTIMATE",R15.5),                             00079200
FL914(" STD. ERROR OF ESTIMATE",R13.5//),                               00079300
FL923(" DEGREES OF FREEDOM",X5,I6///),                                  00079400
FL920(/" PARAMETER",X7,I4,6(X11,I4)),                                   00079600
FL916(" ",I5,X3,7R15.4),                                                00079700
FL917(/" ESTIMATE ",7R15.5),                                            00079800
FL918(/" STANDARD"/" ERROR OF ",7R15.5),                                00079900
FL919(" ESTIMATE"//);                                                   00080000
LIST LIST2(RD,NNT,NO);                                                  00080200
LIST LIST3(RD,FOR DX1:=1 STEP 1 UNTIL NCOV DO NN[DX1]);                 00080300
LIST LIST4(RD,NT,ND,MD,FOR DX1:=1 STEP 1 UNTIL MMT DO NV[DX1]);         00080400
LIST LIST5(IJ,FOR DX1:=1 STEP 1 UNTIL MMT DO NV[DX1]);                  00080500
LIST LIST6(I);                                                          00080600
LIST LIST8(FOR DX1:=1 STEP 1 UNTIL NP DO CI[I,DX1]);                    00080800
LIST LIST9(FOR DX1:=1 STEP 1 UNTIL NNT DO DATAQQQ[DX1],PR);             00080900
LIST LIST10(I,DATAQQQ[NNT],RE,OON);                                     00081000
LIST LIST11(FOR DX1:=1 STEP 1 UNTIL NP DO B[DX1]);                      00081100
LIST LIST12(FOR DX1:=1 STEP 1 UNTIL NP DO DATAQQQ[DX1]);                00081200
LIST LIST14(RE);                                                        00081400
LIST LIST15(NDF);                                                       00081500
LIST LIST16(FOR DX1:=N1 STEP 1 UNTIL N2 DO NN[DX1]);                    00081600
LIST LIST17(MV[I],FOR DX1:=N1 STEP 1 UNTIL N2 DO A[DX1,I]);             00081700
LIST LIST18(FOR DX1:=N1 STEP 1 UNTIL N2 DO DATAQQQ[DX1]);               00081800
BEGIN                                                                   00082000
LABEL L10,L30,L20,L1000,L40,L45,L53,L170,L55,L60,L70,L98,L108,L105,     00082100
 L135,L145,L150,L1002;                                                  00082200
COMMENT  ID   0901HS 15 150    $BMD06V GENERAL LINEAR HYPOTHESIS WITH   00082300
 CONTRASTS                                                              00082400
NTAPE:=5;                                                               00082600
P[1]:=("66F1.0");                                                       00082700
P[2]:=("33F2.0");                                                       00082800
P[3]:=("22F3.0");                                                       00082900
P[4]:=("16F4.0");                                                       00083000
P[6]:=("11F6.0");                                                       00083200
NT1:=2;                                                                 00083300
NT2:=3;                                                                 00083400
REWIND(FILESW[NT1]);                                                    00083500
REWIND(FILESW[NT2]);                                                    00083600
ON:=("PROBLM");                                                         00083800
IF PR = ON THEN GO TO L30;                                              00083900
IF PR=("FINISH") THEN GO TO L1000;                                      00084000
L20: WRITE(TAPE6[PAGE]);                                                00084100
     WRITE(TAPE6,FL901);                                                00084200
L30: IF (MTAPE-NT1)|(MTAPE-NT2)=0 THEN GO TO L20;                       00084400
TPWD(MTAPE,NTAPE);                                                      00084500
NNT:=NP+NEW;                                                            00084600
MMT:=NNT-1;                                                             00084700
ON:=NO;                                                                 00084800
IF (NNT+NCOV)|(NNT+NCOV-61) GEQ 0 THEN GO TO L20;                       00085000
WRITE(TAPE6[PAGE]);                                                     00085100
WRITE(TAPE6,FL902);                                                     00085200
WRITE(TAPE6,FL903,LIST2);                                               00085300
IF NC LEQ 0 THEN GO TO L40;                                             00085400
GO TO L45;                                                              00085600
L40: NPP:=NP;                                                           00085700
L45: I:=1;                                                              00085800
DO BEGIN                                                                00085900
  XM[I]:=0 END UNTIL (I:=(I+1)) GTR NNT;                                00086000
IF NEW GTR 0 THEN GO TO L1000;                                          00086200
IF NCOV LEQ 0 THEN GO TO L53;                                           00086300
READ(TAPE5,FL924,LIST3)[FINIS];                                         00086400
L53: ADJUST;                                                            00086500
IJ:=1;                                                                  00086600
  READ(TAPE5,FL904,LIST4)[FINIS];                                       00086800
  IF RD=("CNTRST") THEN GO TO L60;                                      00086900
  L55: WRITE(TAPE6[PAGE]);                                              00087000
       WRITE(TAPE6,FL905);                                              00087100
  GO TO L170;                                                           00087200
  NP:=0;                                                                00087400
  I:=1;                                                                 00087500
  DO BEGIN                                                              00087600
    IF NV[I] LEQ 0 THEN GO TO L70;                                      00087700
    NP:=NP+1;                                                           00087800
    L70: END UNTIL (I:=(I+1)) GTR MMT;                                  00088000
  IF NP LEQ 0 THEN GO TO L55;                                           00088100
  J:=1;                                                                 00088200
  DO BEGIN                                                              00088300
    N1:=MV[J];                                                          00088400
    I:=1;                                                               00088600
    DO BEGIN                                                            00088700
      N2:=MV[I];                                                        00088800
      CI[I,J]:=C[N2,N1] END UNTIL (I:=(I+1)) GTR NP END UNTIL (J:=(     00088900
   J+1)) GTR NP;                                                        00089000
  I:=1;                                                                 00089200
  DO BEGIN                                                              00089300
    WRITE(TAPE6,FL908,LIST6);                                           00089400
    WRITE(TAPE6,FL909,LIST7);                                           00089500
    END UNTIL (I:=(I+1)) GTR NP;                                        00089600
  WRITE(TAPE6,FL910);                                                   00089800
  I:=1;                                                                 00089900
  DO BEGIN                                                              00090000
    WRITE(TAPE6,FL908,LIST6);                                           00090100
    WRITE(TAPE6,FL909,LIST8);                                           00090200
  J:=1;                                                                 00090400
  DO BEGIN                                                              00090500
    B[J]:=0;                                                            00090600
    I:=1;                                                               00090700
    DO BEGIN                                                            00090800
   UNTIL (J:=(J+1)) GTR NP;                                             00091000
  IF MD LEQ 0 THEN GO TO L98;                                           00091100
  WRITE(TAPE6,FL921);                                                   00091200
  L98: RD:=0;                                                           00091300
  NDF:=0;                                                               00091400
  DO BEGIN                                                              00091600
    READ(FILESW[NTAPE],*,LIST9)[FINIS];                                 00091700
    RE:=0;                                                              00091800
    J:=1;                                                               00091900
    DO BEGIN                                                            00092000
      RE:=RE+DATAQQQ[N1]|B[J] END UNTIL (J:=(J+1)) GTR NP;              00092200
    IF RE=0 THEN GO TO L108;                                            00092300
    OON:=DATAQQQ[NNT]-RE;                                               00092400
    NDF:=NDF+1;                                                         00092500
    IF MD LEQ 0 THEN GO TO L105;                                        00092600
    L105: RD:=RD+(OON*2)|PR;                                            00092800
    L108: END UNTIL (I:=(I+1)) GTR NO;                                  00092900
  REWIND(FILESW[NTAPE]);                                                00093000
  NDF:=NDF-NP;                                                          00093100
  RE:=NDF;                                                              00093200
  RE:=SQRT(RD);                                                         00093400
  I:=1;                                                                 00093500
  DO BEGIN                                                              00093600
    DATAQQQ[I]:=SQRT(RD|CI[I,I]) END UNTIL (I:=(I+1)) GTR NP;           00093700
  WRITE(TAPE6,FL911);                                                   00093800
  WRITE(TAPE6,FL912);                                                   00094000
  WRITE(TAPE6,FL909,LIST12);                                            00094100
  WRITE(TAPE6,FL913,LIST13);                                            00094200
  WRITE(TAPE6,FL914,LIST14);                                            00094300
  WRITE(TAPE6,FL923,LIST15);                                            00094400
  FMT[1]:=("(");                                                        00094600
COMMENT                                                                 00094700
  FMT[2]:=("A6,   ");                                                   00094800
COMMENT                                                                 00094900
  FMT[3]:=P[ND];                                                        00095000
  FMT[4]:=("/     ");                                                   00095200
COMMENT                                                                 00095300
  FMT[5]:=("(");                                                        00095400
COMMENT                                                                 00095500
  FMT[6]:=("6X,   ");                                                   00095600
  FMT[7]:=P[ND];                                                        00095800
COMMENT                                                                 00095900
  FMT[8]:=(",     ");                                                   00096000
COMMENT                                                                 00096100
  FMT[9]:=(")");                                                        00096200
  FMT[10]:=(")");                                                       00096400
  I:=1;                                                                 00096500
  DO BEGIN                                                              00096600
% READ IN DATA HERE ****************************************            00096700
READ(TAPE5,SF[ND],RE,FOR J~1 STEP 1 UNTIL NP DO A[I,J])[FINIS];         00096800
  I:=1;                                                                 00097000
  DO BEGIN                                                              00097100
    DATAQQQ[I]:=0;                                                      00097200
    J:=1;                                                               00097300
    DO BEGIN                                                            00097400
     NP END UNTIL (I:=(I+1)) GTR NT;                                    00097600
  I:=1;                                                                 00097700
  DO BEGIN                                                              00097800
    FMT[I]:=0;                                                          00097900
    KQQQQQQ:=1;                                                         00098000
      J:=1;                                                             00098200
      DO BEGIN                                                          00098300
        FMT[I]:=FMT[I]+A[I,KQQQQQQ]|A[I,J]|CI[J,KQQQQQQ] END UNTIL (    00098400
       J:=(J+1)) GTR NP END UNTIL (KQQQQQQ:=(KQQQQQQ+1)) GTR NP;        00098500
    FMT[I]:=SQRT(FMT[I]|RD) END UNTIL (I:=(I+1)) GTR NT;                00098600
  DO BEGIN                                                              00098800
    NN[I]:=I END UNTIL (I:=(I+1)) GTR NT;                               00098900
  WRITE(TAPE6,FL915);                                                   00099000
  N1:=1;                                                                00099100
  N2:=0;                                                                00099200
  N2:=N2+NT;                                                            00099400
  GO TO L150;                                                           00099500
  L145: N2:=N2+7;                                                       00099600
  L150: WRITE(TAPE6,FL920,LIST16);                                      00099700
  I:=1;                                                                 00099800
    WRITE(TAPE6,FL916,LIST17);                                          00100000
    END UNTIL (I:=(I+1)) GTR NP;                                        00100100
  WRITE(TAPE6,FL917,LIST18);                                            00100200
  WRITE(TAPE6,FL918,LIST19);                                            00100300
  WRITE(TAPE6,FL919);                                                   00100400
  IF NT LEQ 0 THEN GO TO L170;                                          00100600
  N1:=N1+7;                                                             00100700
  GO TO L135;                                                           00100800
  L170: END UNTIL (IJ:=(IJ+1)) GTR NH;                                  00100900
GO TO L10;                                                              00101000
REWIND(FILESW[NTAPE]);                                                  00101200
L1002: GO TO FINIS;                                                     00101300
 END END;                                                               00101400
COMMENT  INITIALIZING BLOCK;                                            00101500
  XPR:=Q:=K:=0;                                                         00101600
END.                                                                    00101800
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
