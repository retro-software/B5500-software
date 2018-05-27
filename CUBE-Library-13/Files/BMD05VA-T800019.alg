BEGIN                                                                   00000100
COMMENT        THE CUBE LIBRARY NUMBER IS T800019. THE PROGRAM NAME IS  00000200
               BMD05V/DISK. THIS VERSION IS DATED 7/5/68;               00000300
FILE IN CARD 0(2,10);                                                   00000400
FILE OUT PUNCH 0(2,10);                                                 00000500
FILE OUT PRINT 4(2,15);                                                 00000600
FILE XXXXXX 2(2,15);                                                    00000700
FILE TAPE1 2(2,15);                                                     00000800
FILE TAPE2 2(2,15);                                                     00000900
FILE TAPE3 2(2,15);                                                     00001000
FILE TAPE4 2(2,15);                                                     00001100
FILE TAPE5 2(2,15);                                                     00001200
FILE TAPE6 2(2,15);                                                     00001300
FILE TAPE7 2(2,15);                                                     00001400
FILE TAPE8 2(2,15);                                                     00001500
FILE TAPE9 2(2,15);                                                     00001600
FILE TAPE10 2(2,15);                                                    00001700
FILE TAPE11 2(2,15);                                                    00001800
FILE TAPE12 2(2,15);                                                    00001900
FILE TAPE13 2(2,15);                                                    00002000
FILE TAPE14 2(2,15);                                                    00002100
FILE TAPE15 2(2,15);                                                    00002200
FILE TAPE16 2(2,15);                                                    00002300
SWITCH FILE FILESW:=XXXXXX,TAPE1,TAPE2,TAPE3,TAPE4,TAPE5,TAPE6,TAPE7,   00002400
   TAPE8,TAPE9,TAPE10,TAPE11,TAPE12,TAPE13,TAPE14,TAPE15,TAPE16;        00002500
LABEL  FINIS;                                                           00002600
REAL ARRAY  DATA[0:63,0:511];    COMMENT USED WITH DATA STATEMENTS ONLY;00002700
REAL Q,XPR;    INTEGER K;                                               00002800
DEFINE B=BOOLEAN#;                                                      00002900
FORMAT F(//////"STOP / PAUSE  NO. ",I5), OKTL(256O);                    00003000
REAL PROCEDURE INT(ARG1);         VALUE ARG1;        REAL ARG1;         00003100
   INT:=SIGN(ARG1)|ENTIER(ABS(ARG1));                                   00003200
REAL PROCEDURE TANH(ARG1);        VALUE ARG1;        REAL ARG1;         00003300
   TANH:=((Q:=EXP(ARG1|2))-1)/(Q+1);                                    00003400
REAL PROCEDURE MAX(ARG1,ARG2);    VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00003500
   MAX:=IF ARG1 GEQ ARG2 THEN ARG1 ELSE ARG2;                           00003600
REAL PROCEDURE MIN(ARG1,ARG2);    VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00003700
   MIN:=IF ARG1 LEQ ARG2 THEN ARG1 ELSE ARG2;                           00003800
REAL PROCEDURE DIM(ARG1,ARG2);    VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00003900
   DIM:=MAX(ARG1-ARG2,0);                                               00004000
REAL PROCEDURE TSIGN(ARG1,ARG2);  VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00004100
   TSIGN:=SIGN(IF ARG2=0 THEN 1 ELSE ARG2)|ABS(ARG1);                   00004200
REAL PROCEDURE LOG(ARG1);         VALUE ARG1;        REAL ARG1;         00004300
   LOG:=LN(ARG1)/2.30258509298;                                         00004400
PROCEDURE ERROR(ARG1);            VALUE ARG1;        REAL ARG1;         00004500
BEGIN WRITE(PRINT,F,ARG1); GO TO FINIS END;                             00004600
PROCEDURE TPWD(NT1,NT2);                                                00004700
  INTEGER NT1,NT2;                                                      00004800
BEGIN                                                                   00004900
FORMAT FL49(" ERROR ON TAPE ASSIGNMENT");                               00005000
LABEL L40,L12,L19,L24,L0;                                               00005100
COMMENT  TPWD    SUBROUTINE TPWD FOR BMD05V         VERSION OF SEPT.    00005200
 26, 1963;                                                              00005300
IF (XPR:=(NT1)) GTR 0 THEN GO TO L12 ELSE IF XPR LSS 0 THEN GO TO       00005400
 L40;                                                                   00005500
NT1:=5;                                                                 00005600
L12: IF NT1=NT2 THEN GO TO L19;                                         00005700
IF NT2 GEQ 5 THEN GO TO L19;                                            00005800
REWIND(FILESW[NT2]);                                                    00005900
GO TO L19;                                                              00006000
COMMENT  17 CALL REMOVE(NT2);                                           00006100
L19: IF NT1=5 THEN GO TO L24;                                           00006200
IF NT1=6 THEN GO TO L40;                                                00006300
REWIND(FILESW[NT1]);                                                    00006400
L24: NT2:=NT1;                                                          00006500
GO TO L0;                                                               00006600
L40: WRITE(TAPE6,FL49);                                                 00006700
GO TO FINIS;                                                            00006800
L0:  END;                                                               00006900
PROCEDURE TRANS(NVG,ONNN,IR,IERROR);                                    00007000
  VALUE NVG,ONNN,IR;                                                    00007100
  INTEGER NVG,IR,IERROR;                                                00007200
  REAL ONNN;                                                            00007300
BEGIN                                                                   00007400
OWN INTEGER ARRAY NEWA[0:64],LCODE[0:61],LVA[0:61];                     00007500
OWN REAL ARRAY T[0:61,0:63],BNEW[0:61];                                 00007600
OWN INTEGER I,J,NTR,KQQQQQQ,II;                                         00007700
OWN REAL POWER,D,E;                                                     00007800
FORMAT FL900(/" ERROR OCCURRED DURING TRANS-GENERATION PASS NUMBER",I3, 00007900
 ", DESIGN NUMBER",I5,"."/" THIS VALUE IS ",R15.5,". ",                 00008000
 "THIS IS THE FIRST ERROR ENCOUNTERED."/                                00008100
 " PROGRAM WILL GO TO NEXT PROBLEM, IF ANY.");                          00008200
LIST LIST1(I,IR,T[1,KQQQQQQ]);                                          00008300
LABEL L200,L99,L7,L8,L10,L20,L30,L40,L50,L60,L70,L80,L90,L100,L110,     00008400
 L120,L130,L140,L32,L12,L18,L24,L29,L33,L0;                             00008500
SWITCH SWGO1:=L10,L20,L30,L40,L50,L60,L70,L80,L90,L100,L110,L120,       00008600
 L130,L140;                                                             00008700
COMMENT  TRANS        SUBROUTINE TRANS FOR BMD05V          OCTOBER 13,  00008800
 1964;                                                                  00008900
REAL PROCEDURE ASN(ARGXX);                                              00009000
VALUE ARGXX;                                                            00009100
REAL ARGXX;                                                             00009200
ASN:=ARCTAN(ARGXX/SQRT(1-ARGXX*2));                                     00009300
IERROR:=0;                                                              00009400
I:=1;                                                                   00009500
DO BEGIN                                                                00009600
  J:=NEWA[I];                                                           00009700
  NTR:=LCODE[I];                                                        00009800
  KQQQQQQ:=LVA[I];                                                      00009900
  IF NTR|(NTR-15) GEQ 0 THEN GO TO L99;                                 00010000
  IF NTR GEQ 11 THEN GO TO L7;                                          00010100
  POWER:=BNEW[I];                                                       00010200
  GO TO L8;                                                             00010300
  L7: II:=INT(BNEW[I]);                                                 00010400
  L8: GO TO SWGO1[NTR];                                                 00010500
  L10: IF (XPR:=(T[1,KQQQQQQ])) LSS 0 THEN GO TO L99 ELSE IF XPR=0 THEN 00010600
GO TO L32;                                                              00010700
  T[1,J]:=SQRT(T[1,KQQQQQQ]);                                           00010800
  GO TO L200;                                                           00010900
  L20: IF (XPR:=(T[1,KQQQQQQ])) GTR 0 THEN GO TO L12 ELSE IF XPR LSS 0  00011000
   THEN GO TO L99;                                                      00011100
  T[1,J]:=1;                                                            00011200
  GO TO L200;                                                           00011300
  L12: T[1,J]:=SQRT(T[1,KQQQQQQ])+SQRT(T[1,KQQQQQQ]+1);                 00011400
  GO TO L200;                                                           00011500
  L30: IF T[1,KQQQQQQ] LEQ 0 THEN GO TO L99;                            00011600
  T[1,J]:=LOG(T[1,KQQQQQQ]);                                            00011700
  GO TO L200;                                                           00011800
  L40: T[1,J]:=EXP(T[1,KQQQQQQ]);                                       00011900
  GO TO L200;                                                           00012000
  L50: IF (XPR:=(T[1,KQQQQQQ])) LSS 0 THEN GO TO L99 ELSE IF XPR=0 THEN 00012100
GO TO L32;                                                              00012200
  IF (XPR:=(T[1,KQQQQQQ]-1)) GTR 0 THEN GO TO L99 ELSE IF XPR LSS 0     00012300
   THEN GO TO L18;                                                      00012400
  T[1,J]:=3.14159265/2;                                                 00012500
  GO TO L200;                                                           00012600
  L18: D:=SQRT(T[1,KQQQQQQ]);                                           00012700
  T[1,J]:=ASN(D);                                                       00012800
  GO TO L200;                                                           00012900
  L60: D:=T[1,KQQQQQQ]/ONNN;                                            00013000
  E:=D+1/ONNN;                                                          00013100
  IF (XPR:=(D)) GTR 0 THEN GO TO L24 ELSE IF XPR LSS 0 THEN GO TO       00013200
   L99;                                                                 00013300
  IF (XPR:=(E)) LSS 0 THEN GO TO L99 ELSE IF XPR=0 THEN GO TO L32;      00013400
  T[1,J]:=ASN(SQRT(E));                                                 00013500
  GO TO L200;                                                           00013600
  L24: IF (XPR:=(E)) GTR 0 THEN GO TO L29 ELSE IF XPR LSS 0 THEN GO TO  00013700
   L99;                                                                 00013800
  T[1,J]:=ASN(SQRT(D));                                                 00013900
  GO TO L200;                                                           00014000
  L29: D:=SQRT(D);                                                      00014100
  E:=SQRT(E);                                                           00014200
  T[1,J]:=ASN(D)+ASN(E);                                                00014300
  GO TO L200;                                                           00014400
  L70: IF T[1,KQQQQQQ]=0 THEN GO TO L99;                                00014500
  T[1,J]:=1/T[1,KQQQQQQ];                                               00014600
  GO TO L200;                                                           00014700
  L80: T[1,J]:=T[1,KQQQQQQ]+POWER;                                      00014800
  GO TO L200;                                                           00014900
  L90: T[1,J]:=T[1,KQQQQQQ]|POWER;                                      00015000
  GO TO L200;                                                           00015100
  L100: IF T[1,KQQQQQQ] NEQ 0 THEN GO TO L33;                           00015200
  L32: T[1,J]:=0;                                                       00015300
  GO TO L200;                                                           00015400
  L33: T[1,J]:=T[1,KQQQQQQ]*POWER;                                      00015500
  GO TO L200;                                                           00015600
  L110: T[1,J]:=T[1,KQQQQQQ]+T[1,II];                                   00015700
  GO TO L200;                                                           00015800
  L120: T[1,J]:=T[1,KQQQQQQ]-T[1,II];                                   00015900
  GO TO L200;                                                           00016000
  L130: T[1,J]:=T[1,KQQQQQQ]|T[1,II];                                   00016100
  GO TO L200;                                                           00016200
  L140: IF T[1,II]=0 THEN GO TO L99;                                    00016300
  T[1,J]:=T[1,KQQQQQQ]/T[1,II];                                         00016400
  GO TO L200;                                                           00016500
  L99: WRITE(TAPE6,FL900,LIST1);                                        00016600
  IERROR:=-99;                                                          00016700
  GO TO L0;                                                             00016800
  L200: END UNTIL (I:=(I+1)) GTR NVG;                                   00016900
L0:  END;                                                               00017000
PROCEDURE VFCHCK(NVF);                                                  00017100
  INTEGER NVF;                                                          00017200
BEGIN                                                                   00017300
FORMAT FL4000(/" ",X23,                                                 00017400
 "NUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFIED, ASSUMED ",     00017500
 "TO BE 1.");                                                           00017600
LABEL L10,L20,L0;                                                       00017700
COMMENT  VFCHCK SUBROUTINE  VFCHCK FOR BMD05V    MAY 26,1964;           00017800
IF NVF GTR 0 THEN GO TO L20;                                            00017900
L10: WRITE(TAPE6,FL4000);                                               00018000
NVF:=1;                                                                 00018100
GO TO L0;                                                               00018200
L20: IF NVF LEQ 5 THEN GO TO L0 ELSE GO TO L10;                         00018300
L0:  END;                                                               00018400
PROCEDURE MATRIX(N1,N2);                                                00018500
  VALUE N1,N2;                                                          00018600
  INTEGER N1,N2;                                                        00018700
BEGIN                                                                   00018800
OWN REAL ARRAY T[0:61,0:63],B[0:61];                                    00018900
OWN INTEGER JUNK,I,IJ,NM,NP,NO,KQQQQQQ,KM,KP,KQ,J;                      00019000
OWN REAL BIG,CON,SUK,AA,POV;                                            00019100
LABEL L46,L50,L63,L49,L52,L55,L53,L66,L0;                               00019200
COMMENT  MATRIX       SUBROUTINE MATRIX FOR BMD05V         OCTOBER 13,  00019300
 1964;                                                                  00019400
JUNK:=0;                                                                00019500
I:=1;                                                                   00019600
DO BEGIN                                                                00019700
  B[I]:=0 END UNTIL (I:=(I+1)) GTR NP;                                  00019800
IJ:=0;                                                                  00019900
NM:=NP-1;                                                               00020000
L46: IJ:=IJ+1;                                                          00020100
NO:=IJ;                                                                 00020200
BIG:=ABS(T[IJ,IJ]);                                                     00020300
I:=IJ;                                                                  00020400
DO BEGIN                                                                00020500
  IF BIG GEQ ABS(T[I,IJ]) THEN GO TO L50;                               00020600
  NO:=I;                                                                00020700
  BIG:=ABS(T[I,IJ]);                                                    00020800
  L50: END UNTIL (I:=(I+1)) GTR NP;                                     00020900
IF BIG GTR CON THEN GO TO L63;                                          00021000
B[IJ]:=1;                                                               00021100
JUNK:=JUNK+1;                                                           00021200
GO TO L46;                                                              00021300
L49: JUNK:=JUNK+1;                                                      00021400
L52: KQQQQQQ:=NP;                                                       00021500
KM:=KQQQQQQ-1;                                                          00021600
I:=1;                                                                   00021700
DO BEGIN                                                                00021800
  SUK:=0;                                                               00021900
  KP:=KQQQQQQ-I;                                                        00022000
  IF B[KP] LSS 1 THEN GO TO L53;                                        00022100
  B[KP]:=0;                                                             00022200
  GO TO L55;                                                            00022300
  L53: KQ:=KP+1;                                                        00022400
  J:=KQ;                                                                00022500
  DO BEGIN                                                              00022600
    SUK:=SUK+T[KP,J]|B[J] END UNTIL (J:=(J+1)) GTR KQQQQQQ;             00022700
  B[KP]:=T[KP,N1]-SUK;                                                  00022800
  L55: END UNTIL (I:=(I+1)) GTR KM;                                     00022900
GO TO L0;                                                               00023000
L63: IF NO LEQ IJ THEN GO TO L66;                                       00023100
J:=IJ;                                                                  00023200
DO BEGIN                                                                00023300
  AA:=T[IJ,J];                                                          00023400
  T[IJ,J]:=T[NO,J];                                                     00023500
  T[NO,J]:=AA END UNTIL (J:=(J+1)) GTR N2;                              00023600
L66: POV:=T[IJ,IJ];                                                     00023700
J:=IJ;                                                                  00023800
DO BEGIN                                                                00023900
  T[IJ,J]:=T[IJ,J]/POV END UNTIL (J:=(J+1)) GTR N1;                     00024000
KQQQQQQ:=IJ+1;                                                          00024100
I:=KQQQQQQ;                                                             00024200
DO BEGIN                                                                00024300
  J:=KQQQQQQ;                                                           00024400
  DO BEGIN                                                              00024500
    T[I,J]:=T[I,J]-T[I,IJ]|T[IJ,J] END UNTIL (J:=(J+1)) GTR N1 END      00024600
 UNTIL (I:=(I+1)) GTR NP;                                               00024700
IF IJ LSS NM THEN GO TO L46;                                            00024800
IF ABS(T[NP,NP]) LEQ CON THEN GO TO L49;                                00024900
B[NP]:=T[NP,N1]/T[NP,NP];                                               00025000
GO TO L52;                                                              00025100
L0:  END;                                                               00025200
PROCEDURE PUNCHQQ;                                                      00025300
BEGIN                                                                   00025400
OWN REAL ARRAY T[0:61,0:63],A[0:63],P[0:2,0:511];                       00025500
OWN INTEGER DX1;                                                        00025600
OWN INTEGER NCO,IJ,NR,I,NNCO,MCO,J,L1,L2;                               00025700
OWN REAL ORQQQQQ;                                                       00025800
FORMAT FL900(/" "//" DESIGN",X3,"NO. OF",X8,"MEAN",X9,"STD. DEV."/X11,  00025900
 "REPS",X11,"Y",X14,"Y"),                                               00026000
FL903(" ",I4,X5,I4,R17.5,R15.5,R17.5,4R14.5),                           00026100
FL902(/" "//" DESIGN",X3,"NO. OF",X8,"MEAN",X9,"STD. DEV.",X10,         00026200
 "MEANS OF COVARIATES"/X11,"REPS",X11,"Y",X14,"Y",X4,5I14/(X46,5I14)),  00026300
FL904(" ",X45,R17.5,4R14.5);                                            00026400
LIST LIST1(NR);                                                         00026500
LIST LIST2(FOR DX1:=1 STEP 1 UNTIL NR DO P[(K:=(DX1)).[33:6],K.[39:9]]);00026600
LIST LIST3(IJ,NR,A[3],A[4]);                                            00026700
LIST LIST4(FOR DX1:=1 STEP 1 UNTIL NCO DO DX1);                         00026800
LIST LIST5(FOR DX1:=1 STEP 1 UNTIL NNCO DO P[(K:=(DX1)).[33:6],K.[39:9]]00026900
);                                                                      00027000
LIST LIST6(IJ,NR,A[3],A[4],FOR DX1:=L1 STEP 1 UNTIL L2 DO T[DX1,1]);    00027100
LIST LIST7(FOR DX1:=L1 STEP 1 UNTIL L2 DO T[DX1,1]);                    00027200
LABEL L40,L20,L30,L120,L115,L65,L75,L78,L85,L90,L100,L105;              00027300
COMMENT  PUNCH        SUBROUTINE PUNCH FOR BMD05V          OCTOBER 13,  00027400
 1964;                                                                  00027500
IF NCO GTR 0 THEN GO TO L40;                                            00027600
WRITE(TAPE6,FL900);                                                     00027700
IJ:=1;                                                                  00027800
DO BEGIN                                                                00027900
  READ(TAPE2,*,LIST1)[FINIS];                                           00028000
  READ(TAPE2,*,LIST2)[FINIS];                                           00028100
  IF NR GTR 1 THEN GO TO L20;                                           00028200
  A[3]:=P[0,1];                                                         00028300
  A[4]:=0;                                                              00028400
  GO TO L30;                                                            00028500
  L20: A[1]:=0;                                                         00028600
  A[2]:=0;                                                              00028700
  ORQQQQQ:=NR;                                                          00028800
  I:=1;                                                                 00028900
  DO BEGIN                                                              00029000
    A[1]:=A[1]+P[(K:=(I)).[33:6],K.[39:9]];                             00029100
    A[2]:=A[2]+P[(K:=(I)).[33:6],K.[39:9]]*2 END UNTIL (I:=(I+1)) GTR   00029200
   NR;                                                                  00029300
  A[3]:=A[1]/ORQQQQQ;                                                   00029400
  A[4]:=SQRT((ORQQQQQ|A[2]-A[1]*2)/(ORQQQQQ|(ORQQQQQ-1)));              00029500
  L30: WRITE(TAPE6,FL903,LIST3);                                        00029600
  END UNTIL (IJ:=(IJ+1)) GTR N;                                         00029700
GO TO L120;                                                             00029800
L40: NNCO:=NCO+1;                                                       00029900
WRITE(TAPE6,FL902,LIST4);                                               00030000
IJ:=1;                                                                  00030100
DO BEGIN                                                                00030200
  MCO:=NCO;                                                             00030300
  I:=1;                                                                 00030400
  DO BEGIN                                                              00030500
    T[I,1]:=0 END UNTIL (I:=(I+1)) GTR NCO;                             00030600
  A[1]:=0;                                                              00030700
  A[2]:=0;                                                              00030800
  READ(TAPE2,*,LIST1)[FINIS];                                           00030900
  I:=1;                                                                 00031000
  DO BEGIN                                                              00031100
    READ(TAPE2,*,LIST5)[FINIS];                                         00031200
    J:=1;                                                               00031300
    DO BEGIN                                                            00031400
      T[J,1]:=T[J,1]+P[(K:=(J)).[33:6],K.[39:9]] END UNTIL (J:=(J+1))   00031500
     GTR NCO;                                                           00031600
    A[1]:=A[1]+P[(K:=(NNCO)).[33:6],K.[39:9]];                          00031700
    A[2]:=A[2]+P[(K:=(NNCO)).[33:6],K.[39:9]]*2 END UNTIL (I:=(I+1))    00031800
   GTR NR;                                                              00031900
  IF NR GTR 1 THEN GO TO L65;                                           00032000
  A[3]:=A[1];                                                           00032100
  A[4]:=0;                                                              00032200
  GO TO L75;                                                            00032300
  L65: ORQQQQQ:=NR;                                                     00032400
  I:=1;                                                                 00032500
  DO BEGIN                                                              00032600
    T[I,1]:=T[I,1]/ORQQQQQ END UNTIL (I:=(I+1)) GTR NCO;                00032700
  A[3]:=A[1]/ORQQQQQ;                                                   00032800
  A[4]:=SQRT((ORQQQQQ|A[2]-A[1]*2)/(ORQQQQQ|(ORQQQQQ-1)));              00032900
  L75: L1:=1;                                                           00033000
  L2:=0;                                                                00033100
  L78: IF MCO GTR 5 THEN GO TO L85;                                     00033200
  L2:=L2+MCO;                                                           00033300
  GO TO L90;                                                            00033400
  L85: L2:=L2+5;                                                        00033500
  L90: IF L1 GTR 1 THEN GO TO L100;                                     00033600
  WRITE(TAPE6,FL903,LIST6);                                             00033700
  GO TO L105;                                                           00033800
  L100: WRITE(TAPE6,FL904,LIST7);                                       00033900
  L105: MCO:=MCO-5;                                                     00034000
  IF MCO LEQ 0 THEN GO TO L115;                                         00034100
  L1:=L1+5;                                                             00034200
  GO TO L78;                                                            00034300
  L115: END UNTIL (IJ:=(IJ+1)) GTR N;                                   00034400
L120: REWIND(TAPE2);                                                    00034500
 END;                                                                   00034600
PROCEDURE MAINPRO;                                                      00034700
BEGIN                                                                   00034800
OWN INTEGER ARRAY ND[0:61],MP[0:61],MA[0:61,0:64],ID[0:64],             00034900
 IFF[0:2,0:64],NEWA[0:64],LCODE[0:61],LVA[0:61];                        00035000
OWN REAL ARRAY S[0:61,0:63],T[0:61,0:63],A[0:63],B[0:61],               00035100
 SUM[0:64],X[0:61,0:63],COE[0:61,0:64],P[0:2,0:511],SU[0:64],           00035200
 AM[0:61,0:64],FMT[0:61],BNEW[0:61];                                    00035300
OWN INTEGER DX1;                                                        00035400
OWN INTEGER ASVNAB,ASVNNN;                                              00035500
OWN INTEGER MTAPE,IX,NVG,N,NP,NCO,NH,NA,NTAPE,L1,NCN,NTRGOF,I,NP1,      00035600
 KM,N1,NPP,JX,J,N2,IERROR,N5,IJ,N4,IA1,IA3,N3,NR,L2,IK,NO,M1,M2,        00035700
 NT,IA4,KK,MM,JUNK,JK;                                                  00035800
OWN REAL AA3,AA4,CON,AA1,PROB,ONNN,AA2;                                 00035900
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  TPWD,VFCHCK,TRANS,MATRIX,  00036000
 PUNCHQQ,REMOVE;                                                        00036100
FORMAT FL901(" BMD05V - GENERAL LINEAR HYPOTHESIS - VERSION ",          00036200
 "OF JULY 22, 1965"/" HEALTH SCIENCES COMPUTING FACILITY, UCLA"//),     00036300
FL900(A6,A2,I3,4I2,R6.0,I2,X41,I2,I2),                                  00036400
FL940(/" ERROR ON PROBLEM CARD"),                                       00036500
FL902(/" PROBLEM NUMBER  ",A2//" NUMBER OF DESIGN CARD SETS",I6),       00036600
FL937(/" ",X6,"TRANS-GENERATION CARD"),                                 00036700
FL938(/" CARD    NEW     TRANS    ORIG.   ORIG. VAR(B)"/                00036800
 "  NO. VARIABLE   CODE    VAR(A)   OR CONSTANT"),                      00036900
FL936(A6,I3,I2,I3,R6.0),                                                00037000
FL905(/" ERROR ON TRANS-GENERATION CARD",I4),                           00037100
FL939("  ",I2,I8,2I9,X4,R10.5),                                         00037200
FL920(/" ",X23,                                                         00037300
 "ALL TRANSGENERATION CARD VARIABLES MUST BE 1 FOR THE NO COVARIA",     00037400
 "TE CASE."/X26,                                                        00037500
 "THE ABOVE CARD IS INCORRECT. THE VARIABLES WILL BE SET EQUAL TO",     00037600
 " 1."),                                                                00037700
FL6007(/" ILLEGAL TRANSGENERATION CODE SPECIFIED ON CARD",I3),          00037800
FL915(/" PROGRAM WILL TERMINATE."),                                     00037900
FL931(12A6),                                                            00038000
FL903(/" NUMBER OF INDEPENDENT VARIABLES",I6//),                        00038100
FL933(/" DESIGN"//),                                                    00038200
FL904(A6,22R3.0),                                                       00038300
FL934(X1,I4,X3,30I3),                                                   00038400
FL935(X8,30I3),                                                         00038500
FL907(A6,66I1),                                                         00038600
FL909(/" ",X24,A6," CARD",I4,                                           00038700
 " MISPUNCHED OR OUT OF ORDER. PROGRAM CANNOT PROCEED."),               00038800
FL908(/" HYPOTHESES AND SUMS OF SQUARES EXPLAINED BY HYPOTHESES"//),    00038900
FL910(/" ESTIMATES OF COEFFICIENTS"/X35,"H Y P O T H E S I S"),         00039000
FL912(/" ",X4,6I16/(X5,6I16)),                                          00039100
FL913(" VARIABLE"),                                                     00039200
FL911(I6,X4,6R16.5),                                                    00039300
FL914(/" RESIDUAL"/" SUM SQS.",R17.5,5R16.5),                           00039400
FL916(/" DEGREES OF"),                                                  00039500
FL917(" FREEDOM OF",I10,5I16),                                          00039600
FL918(" RESIDUALS"),                                                    00039700
FL919(/" F TESTS  ",X32,4R16.5),                                        00039800
FL926(/" F TESTS  ",6R16.5),                                            00039900
FL921(" FREEDOM OF",X32,6(I9," ",I4,"  ")),                             00040000
FL927(" FREEDOM OF",6(I9," ",I4,"  ")),                                 00040100
FL922(" F TESTS"//" "),                                                 00040200
FL923(/" ACCURACY OF COEFFICIENTS"),                                    00040300
FL924(I6,X20,5R16.7),                                                   00040400
FL930(I6,X4,6R16.7),                                                    00040500
FL906(72I1);                                                            00040600
LIST LIST1(AA1,PROB,N,NP,NCO,NH,NVG,ONNN,NA,NTAPE,L1);                  00040700
LIST LIST2(PROB,N);                                                     00040800
LIST LIST3(AA1,NEWA[I],LCODE[I],LVA[I],BNEW[I]);                        00040900
LIST LIST4(I);                                                          00041000
LIST LIST5(I,NEWA[I],LCODE[I],LVA[I],BNEW[I]);                          00041100
LIST LIST6(FOR DX1:=1 STEP 1 UNTIL L1 DO FMT[DX1]);                     00041200
LIST LIST7(NP);                                                         00041300
LIST LIST8(IA1,A[1],FOR DX1:=1 STEP 1 UNTIL N4 DO P[(K:=(DX1)).[33:6],K.00041400
[39:9]]);                                                               00041500
LIST LIST9(IA1,FOR DX1:=N3 STEP 1 UNTIL N4 DO P[(K:=(DX1)).[33:6],K.[39:00041600
9]]);                                                                   00041700
LIST LIST10(IJ,FOR DX1:=1 STEP 1 UNTIL NP1 DO ND[DX1]);                 00041800
LIST LIST11(IJ,FOR DX1:=1 STEP 1 UNTIL 30 DO ND[DX1]);                  00041900
LIST LIST12(FOR DX1:=31 STEP 1 UNTIL NP1 DO ND[DX1]);                   00042000
LIST LIST13(NR);                                                        00042100
LIST LIST14(FOR DX1:=L1 STEP 1 UNTIL L2 DO P[(K:=(DX1)).[33:6],K.[39:9]]00042200
);                                                                      00042300
LIST LIST15(FOR DX1:=KM STEP 1 UNTIL L1 DO P[(K:=(DX1)).[33:6],K.[39:9]]00042400
);                                                                      00042500
LIST LIST16(AA1,FOR DX1:=1 STEP 1 UNTIL NPP DO MP[DX1]);                00042600
LIST LIST17(IA1,FOR DX1:=1 STEP 1 UNTIL NO DO MP[DX1]);                 00042700
LIST LIST18(IA4,NT);                                                    00042800
LIST LIST19(FOR DX1:=L1 STEP 1 UNTIL L2 DO DX1);                        00042900
LIST LIST20(I,FOR DX1:=L1 STEP 1 UNTIL L2 DO COE[I,DX1]);               00043000
LIST LIST21(FOR DX1:=L1 STEP 1 UNTIL L2 DO SUM[DX1]);                   00043100
LIST LIST22(FOR DX1:=L1 STEP 1 UNTIL L2 DO ID[DX1]);                    00043200
LIST LIST23(FOR DX1:=3 STEP 1 UNTIL L2 DO P[(K:=(DX1)).[33:6],K.[39:9]])00043300
 ;                                                                      00043400
LIST LIST24(FOR DX1:=3 STEP 1 UNTIL L2 DO [IFF[1,DX1],IFF[2,DX1]]);     00043500
LIST LIST25(FOR DX1:=L1 STEP 1 UNTIL L2 DO [IFF[1,DX1],IFF[2,DX1]]);    00043600
LIST LIST26(I,FOR DX1:=2 STEP 1 UNTIL L2 DO AM[I,DX1]);                 00043700
LIST LIST27(I,FOR DX1:=L1 STEP 1 UNTIL L2 DO AM[I,DX1]);                00043800
LIST LIST28(IA3,IJ);                                                    00043900
BEGIN                                                                   00044000
LABEL L5,L130,L10,L7,L500,L11,L313,L6313,L6008,L6006,L3125,L165,L35,    00044100
 L334,L550,L339,L3385,L345,L38,L39,L22,L2005,L34,L375,L380,L86,L157,    00044200
 L121,L99,L90,L70,L72,L145,L152,L200,L216,L225,L230,L243,L245,L253,     00044300
 L260,L270,L276,L285,L290,L300,L303,L504;                               00044400
SWITCH ASSIGN:=L130,L86,L121,L200;                                      00044500
COMMENT  ID   0901HS 15 150    $BMD05V GENERAL LINEAR HYPOTH. FOR       00044600
 MISSING DATA                                                           00044700
BD05V        GENERAL LINEAR HYPOTHESIS               JULY 22, 1965;     00044800
AA3:=("DESIGN");                                                        00044900
AA4:=("HYPOTH");                                                        00045000
MTAPE:=5;                                                               00045100
CON:=1@-6;                                                              00045200
REWIND(TAPE2);                                                          00045300
L5: ID[1]:=0;                                                           00045400
IX:=1;                                                                  00045500
MP[1]:=0;                                                               00045600
NVG:=0;                                                                 00045700
ASVNAB:=1;                                                              00045800
READ(TAPE5,FL900,LIST1)[FINIS];                                         00045900
AA2:=("PROBLM");                                                        00046000
IF B(AA1) AND ( NOT B(AA2)) OR B(AA2) AND ( NOT B(AA1))=0 THEN GO TO    00046100
 L10;                                                                   00046200
IF AA1=("FINISH") THEN GO TO L500;                                      00046300
L7: WRITE(TAPE6,FL940);                                                 00046400
GO TO L500;                                                             00046500
L10: IF NP|(NP-61) GEQ 0 THEN GO TO L7;                                 00046600
IF (NCO+NP)|(NCO+NP-61) GEQ 0 THEN GO TO L7;                            00046700
IF NH|(NH-58) GEQ 0 THEN GO TO L7;                                      00046800
IF NTAPE=2 THEN GO TO L7;                                               00046900
TPWD(NTAPE,MTAPE);                                                      00047000
WRITE(TAPE6[PAGE]);                                                     00047100
WRITE(TAPE6,FL901);                                                     00047200
WRITE(TAPE6,FL902,LIST2);                                               00047300
NCN:=NCO+1;                                                             00047400
IF NVG LEQ 0 THEN GO TO L11;                                            00047500
WRITE(TAPE6,FL937);                                                     00047600
WRITE(TAPE6,FL938);                                                     00047700
AA2:=("TRNGEN");                                                        00047800
NTRGOF:=0;                                                              00047900
I:=1;                                                                   00048000
DO BEGIN                                                                00048100
  READ(TAPE5,FL936,LIST3)[FINIS];                                       00048200
  IF B(AA1) AND ( NOT B(AA2)) OR B(AA2) AND ( NOT B(AA1))=0 THEN GO TO  00048300
   L6313;                                                               00048400
  WRITE(TAPE6,FL905,LIST4);                                             00048500
  GO TO L6008;                                                          00048600
  L6313: IF LCODE[1]|(LCODE[1]-15) GTR 0 THEN GO TO L6006;              00048700
  WRITE(TAPE6,FL939,LIST5);                                             00048800
  IF NCO GTR 0 THEN GO TO L313;                                         00048900
  IF -((NEWA[I]-1)|(LVA[I]-1))=0 THEN GO TO L3125;                      00049000
  WRITE(TAPE6,FL920);                                                   00049100
  NEWA[I]:=1;                                                           00049200
  LVA[I]:=1;                                                            00049300
  GO TO L313;                                                           00049400
  L3125: IF 10 GEQ LCODE[I] THEN GO TO L313;                            00049500
  BNEW[I]:=1;                                                           00049600
  GO TO L313;                                                           00049700
  L6006: WRITE(TAPE6,FL6007,LIST4);                                     00049800
  L6008: NTRGOF:=NTRGOF+1;                                              00049900
  WRITE(TAPE6,FL915);                                                   00050000
  L313: END UNTIL (I:=(I+1)) GTR NVG;                                   00050100
IF NTRGOF GTR 0 THEN GO TO L500;                                        00050200
L11: NP1:=NP;                                                           00050300
ONNN:=ONNN+1;                                                           00050400
KM:=NP+1;                                                               00050500
N1:=KM+NCO;                                                             00050600
NP:=N1-1;                                                               00050700
NPP:=NP;                                                                00050800
VFCHCK(L1);                                                             00050900
L1:=12|L1;                                                              00051000
READ(TAPE5,FL931,LIST6)[FINIS];                                         00051100
WRITE(TAPE6,FL903,LIST7);                                               00051200
JX:=NH+3;                                                               00051300
J:=1;                                                                   00051400
DO BEGIN                                                                00051500
  SUM[J]:=0 END UNTIL (J:=(J+1)) GTR JX;                                00051600
N2:=N1+1;                                                               00051700
J:=1;                                                                   00051800
DO BEGIN                                                                00051900
  I:=1;                                                                 00052000
  DO BEGIN                                                              00052100
    S[I,J]:=0 END UNTIL (I:=(I+1)) GTR NP END UNTIL (J:=(J+1)) GTR      00052200
 N1;                                                                    00052300
WRITE(TAPE6,FL933);                                                     00052400
L1:=N1;                                                                 00052500
IERROR:=0;                                                              00052600
N5:=21;                                                                 00052700
IF NP1 GEQ 21 THEN GO TO L165;                                          00052800
N5:=NP1;                                                                00052900
L165: IJ:=1;                                                            00053000
DO BEGIN                                                                00053100
  N4:=N5;                                                               00053200
  READ(TAPE5,FL904,LIST8)[FINIS];                                       00053300
  L334: IF IA1 NEQ IA3 THEN GO TO L550;                                 00053400
  IF N4 GEQ NP1 THEN GO TO L339;                                        00053500
  N3:=N4+1;                                                             00053600
  N4:=N4+22;                                                            00053700
  IF NP1 GEQ N4 THEN GO TO L3385;                                       00053800
  N4:=NP1;                                                              00053900
  L3385: READ(TAPE5,FL904,LIST9)[FINIS];                                00054000
  GO TO L334;                                                           00054100
  L339: IF IERROR LSS 0 THEN GO TO L345;                                00054200
  I:=1;                                                                 00054300
  DO BEGIN                                                              00054400
    ND[I]:=INT(P[(K:=(I)).[33:6],K.[39:9]]) END UNTIL (I:=(I+1)) GTR    00054500
   NP1;                                                                 00054600
  IF NP1 GTR 30 THEN GO TO L38;                                         00054700
  WRITE(TAPE6,FL934,LIST10);                                            00054800
  GO TO L39;                                                            00054900
  L38: WRITE(TAPE6,FL934,LIST11);                                       00055000
  WRITE(TAPE6,FL935,LIST12);                                            00055100
  L39: NR:=INT(A[1]);                                                   00055200
  WRITE(TAPE2,*,LIST13);                                                00055300
  ID[1]:=ID[1]+NR;                                                      00055400
  L345: IF NCO GTR 0 THEN GO TO L22;                                    00055500
  L2:=NP+NR;                                                            00055600
  COMMENT  SEE LINE 364 OF FORTRAN LISTING;                             00055700
  IF IERROR LSS 0 THEN GO TO L35;                                       00055800
  IF -NVG GEQ 0 THEN GO TO L2005;                                       00055900
  I:=L1;                                                                00056000
  DO BEGIN                                                              00056100
    T[1,1]:=P[(K:=(I)).[33:6],K.[39:9]];                                00056200
    TRANS(NVG,ONNN,IJ,IERROR);                                          00056300
    P[(K:=(I)).[33:6],K.[39:9]]:=T[1,1] END UNTIL (I:=(I+1)) GTR        00056400
   L2;                                                                  00056500
  L2005: WRITE(TAPE2,*,LIST14);                                         00056600
  I:=L1;                                                                00056700
  DO BEGIN                                                              00056800
    SUM[1]:=SUM[1]+P[(K:=(I)).[33:6],K.[39:9]]|P[(K:=(I)).[33:6],K.[39:900056900
]] END UNTIL (I:=(I+1)) GTR L2;                                         00057000
  J:=1;                                                                 00057100
  DO BEGIN                                                              00057200
    I:=1;                                                               00057300
    DO BEGIN                                                            00057400
      S[I,J]:=S[I,J]+(P[(K:=(I)).[33:6],K.[39:9]]|P[(K:=(J)).[33:6],K.[300057500
9:9]])|A[1] END UNTIL (I:=(I+1)) GTR NP END UNTIL (J:=(J+1)) GTR        00057600
   NP;                                                                  00057700
  I:=L1;                                                                00057800
  DO BEGIN                                                              00057900
    J:=1;                                                               00058000
    DO BEGIN                                                            00058100
      S[J,N1]:=S[J,N1]+P[(K:=(J)).[33:6],K.[39:9]]|P[(K:=(I)).[33:6],K.[00058200
39:9]] END UNTIL (J:=(J+1)) GTR NP END UNTIL (I:=(I+1)) GTR L2;         00058300
  GO TO L35;                                                            00058400
  L22: IK:=1;                                                           00058500
  DO BEGIN                                                              00058600
    COMMENT  SEE LINE 382 OF FORTRAN LISTING;                           00058700
    IF IERROR LSS 0 THEN GO TO L34;                                     00058800
    NO:=0;                                                              00058900
    I:=KM;                                                              00059000
    DO BEGIN                                                            00059100
      NO:=NO+1;                                                         00059200
      T[1,NO]:=P[(K:=(I)).[33:6],K.[39:9]] END UNTIL (I:=(I+1)) GTR     00059300
     L1;                                                                00059400
    IF NVG LEQ 0 THEN GO TO L375;                                       00059500
    TRANS(NVG,ONNN,IJ,IERROR);                                          00059600
    NO:=0;                                                              00059700
    I:=KM;                                                              00059800
    DO BEGIN                                                            00059900
      NO:=NO+1;                                                         00060000
      P[(K:=(I)).[33:6],K.[39:9]]:=T[1,NO] END UNTIL (I:=(I+1)) GTR     00060100
     L1;                                                                00060200
    L375: WRITE(TAPE2,*,LIST15);                                        00060300
    SUM[1]:=SUM[1]+P[(K:=(L1)).[33:6],K.[39:9]]|P[(K:=(L1)).[33:6],K.[3900060400
:9]];                                                                   00060500
    J:=1;                                                               00060600
    DO BEGIN                                                            00060700
      I:=1;                                                             00060800
      DO BEGIN                                                          00060900
        S[I,J]:=S[I,J]+P[(K:=(I)).[33:6],K.[39:9]]|P[(K:=(J)).[33:6],K.[00061000
39:9]] END UNTIL (I:=(I+1)) GTR NP END UNTIL (J:=(J+1)) GTR NP;         00061100
    J:=1;                                                               00061200
    DO BEGIN                                                            00061300
      S[J,N1]:=S[J,N1]+P[(K:=(J)).[33:6],K.[39:9]]|P[(K:=(L1)).[33:6],K.00061400
[39:9]] END UNTIL (J:=(J+1)) GTR NP;                                    00061500
    L34: END UNTIL (IK:=(IK+1)) GTR NR;                                 00061600
  L35: END UNTIL (IJ:=(IJ+1)) GTR N;                                    00061700
CLOSE(TAPE2,*);                                                         00061800
REWIND(TAPE2);                                                          00061900
IF IERROR GEQ 0 THEN GO TO L380;                                        00062000
J:=1;                                                                   00062100
DO BEGIN                                                                00062200
  READ(TAPE5,FL907,LIST16)[FINIS];                                      00062300
  END UNTIL (J:=(J+1)) GTR NH;                                          00062400
GO TO L5;                                                               00062500
L380: J:=1;                                                             00062600
DO BEGIN                                                                00062700
  SU[J]:=0;                                                             00062800
  I:=1;                                                                 00062900
  DO BEGIN                                                              00063000
    AM[I,J]:=0;                                                         00063100
    COE[I,J]:=0 END UNTIL (I:=(I+1)) GTR NP END UNTIL (J:=(J+1)) GTR    00063200
 JX;                                                                    00063300
I:=1;                                                                   00063400
DO BEGIN                                                                00063500
  S[I,N2]:=S[I,N1] END UNTIL (I:=(I+1)) GTR NP;                         00063600
J:=1;                                                                   00063700
DO BEGIN                                                                00063800
  I:=1;                                                                 00063900
  DO BEGIN                                                              00064000
    T[I,J]:=S[I,J] END UNTIL (I:=(I+1)) GTR NP END UNTIL (J:=(J+1)) GTR 00064100
 N2;                                                                    00064200
I:=1;                                                                   00064300
DO BEGIN                                                                00064400
  J:=1;                                                                 00064500
  DO BEGIN                                                              00064600
    X[I,J]:=T[I,J] END UNTIL (J:=(J+1)) GTR N1 END UNTIL (I:=(I+1)) GTR 00064700
 NP;                                                                    00064800
I:=1;                                                                   00064900
DO BEGIN                                                                00065000
  FMT[I]:=0 END UNTIL (I:=(I+1)) GTR 60;                                00065100
FMT[1]:=("(");                                                          00065200
FMT[2]:=("I6,6X,");                                                     00065300
L1:=NPP+3;                                                              00065400
FMT[L1]:=("F18.5,");                                                    00065500
L1:=L1+1;                                                               00065600
FMT[L1]:=(")");                                                         00065700
MATRIX(N1,N2);                                                          00065800
PUNCHQQ;                                                                00065900
I:=1;                                                                   00066000
DO BEGIN                                                                00066100
  MP[I]:=0 END UNTIL (I:=(I+1)) GTR NP;                                 00066200
ASVNNN:=2;                                                              00066300
GO TO L157;                                                             00066400
L86: I:=1;                                                              00066500
DO BEGIN                                                                00066600
  MP[I]:=1 END UNTIL (I:=(I+1)) GTR NP;                                 00066700
ASVNNN:=3;                                                              00066800
M1:=N1;                                                                 00066900
M2:=N2;                                                                 00067000
NT:=0;                                                                  00067100
GO TO L99;                                                              00067200
L90: NT:=NT+1;                                                          00067300
NO:=NPP;                                                                00067400
READ(TAPE5,FL907,LIST17)[FINIS];                                        00067500
IF IA1=IA4 THEN GO TO L70;                                              00067600
WRITE(TAPE6,FL909,LIST18);                                              00067700
GO TO L500;                                                             00067800
L70: NP:=0;                                                             00067900
I:=1;                                                                   00068000
DO BEGIN                                                                00068100
  IF MP[I] LEQ 0 THEN GO TO L72;                                        00068200
  NP:=NP+1;                                                             00068300
  ND[NP]:=I;                                                            00068400
  L72: END UNTIL (I:=(I+1)) GTR NO;                                     00068500
M1:=NP+1;                                                               00068600
ND[M1]:=N1;                                                             00068700
J:=1;                                                                   00068800
DO BEGIN                                                                00068900
  KK:=ND[J];                                                            00069000
  I:=1;                                                                 00069100
  DO BEGIN                                                              00069200
    MM:=ND[I];                                                          00069300
    T[I,J]:=S[MM,KK] END UNTIL (I:=(I+1)) GTR NP END UNTIL (J:=(J+1))   00069400
 GTR M1;                                                                00069500
M2:=M1+1;                                                               00069600
I:=1;                                                                   00069700
DO BEGIN                                                                00069800
  T[I,M2]:=T[I,M1] END UNTIL (I:=(I+1)) GTR NP;                         00069900
J:=1;                                                                   00070000
DO BEGIN                                                                00070100
  I:=1;                                                                 00070200
  DO BEGIN                                                              00070300
    X[I,J]:=T[I,J] END UNTIL (I:=(I+1)) GTR NP END UNTIL (J:=(J+1)) GTR 00070400
 M1;                                                                    00070500
MATRIX(M1,M2);                                                          00070600
L99: IX:=IX+1;                                                          00070700
I:=1;                                                                   00070800
DO BEGIN                                                                00070900
  SU[IX]:=SU[IX]+B[I]|X[I,M1] END UNTIL (I:=(I+1)) GTR NP;              00071000
SUM[IX]:=SUM[1]-SU[IX];                                                 00071100
ID[IX]:=ID[1]-(NP-JUNK);                                                00071200
IF NA LEQ 0 THEN GO TO L145;                                            00071300
I:=1;                                                                   00071400
DO BEGIN                                                                00071500
  A[I]:=0;                                                              00071600
  J:=1;                                                                 00071700
  DO BEGIN                                                              00071800
    A[I]:=A[I]+X[I,J]|B[J] END UNTIL (J:=(J+1)) GTR NP END UNTIL (      00071900
 I:=(I+1)) GTR NP;                                                      00072000
I:=1;                                                                   00072100
DO BEGIN                                                                00072200
  A[I]:=A[I]-X[I,M1] END UNTIL (I:=(I+1)) GTR NP;                       00072300
L145: KK:=0;                                                            00072400
I:=1;                                                                   00072500
DO BEGIN                                                                00072600
  IF MP[I] LEQ 0 THEN GO TO L152;                                       00072700
  KK:=KK+1;                                                             00072800
  AM[I,IX]:=A[KK];                                                      00072900
  COE[I,IX]:=B[KK];                                                     00073000
  L152: END UNTIL (I:=(I+1)) GTR NPP;                                   00073100
L157: I:=1;                                                             00073200
DO BEGIN                                                                00073300
  MA[I,IX]:=MP[I] END UNTIL (I:=(I+1)) GTR NPP;                         00073400
GO TO ASSIGN[ASVNNN];                                                   00073500
L121: IF NT LSS NH THEN GO TO L90;                                      00073600
GO TO ASSIGN[ASVNAB];                                                   00073700
L130: ASVNAB:=4;                                                        00073800
J:=2;                                                                   00073900
DO BEGIN                                                                00074000
  MP[J]:=0 END UNTIL (J:=(J+1)) GTR NPP;                                00074100
MP[1]:=1;                                                               00074200
NP:=1;                                                                  00074300
T[1,1]:=S[1,1];                                                         00074400
M1:=2;                                                                  00074500
M2:=3;                                                                  00074600
T[1,M1]:=S[1,N1];                                                       00074700
T[1,M2]:=S[1,N2];                                                       00074800
X[1,1]:=T[1,1];                                                         00074900
X[1,2]:=T[1,2];                                                         00075000
MATRIX(M1,M2);                                                          00075100
GO TO L99;                                                              00075200
L200: AA2:=ID[2];                                                       00075300
I:=3;                                                                   00075400
DO BEGIN                                                                00075500
  IFF[1,I]:=ID[I]-ID[2];                                                00075600
  IFF[2,I]:=ID[2];                                                      00075700
  AA1:=IFF[1,I];                                                        00075800
  P[(K:=(I)).[33:6],K.[39:9]]:=(AA2/AA1)|((SUM[I]-SUM[2])/SUM[2]) END   00075900
 UNTIL (I:=(I+1)) GTR IX;                                               00076000
WRITE(TAPE6[DBL]);                                                      00076100
WRITE(TAPE6,FL908);                                                     00076200
I:=1;                                                                   00076300
DO BEGIN                                                                00076400
  FMT[I+2]:=("I1,") END UNTIL (I:=(I+1)) GTR NPP;                       00076500
I:=1;                                                                   00076600
DO BEGIN                                                                00076700
  COMMENT  SEE LINE 519 OF FORTRAN LISTING;                             00076800
  END UNTIL (I:=(I+1)) GTR IX;                                          00076900
WRITE(TAPE6[DBL]);                                                      00077000
WRITE(TAPE6,FL910);                                                     00077100
L1:=1;                                                                  00077200
L2:=0;                                                                  00077300
JK:=IX;                                                                 00077400
L216: IF JK GTR 6 THEN GO TO L225;                                      00077500
L2:=L2+JK;                                                              00077600
GO TO L230;                                                             00077700
L225: L2:=L2+6;                                                         00077800
L230: WRITE(TAPE6,FL912,LIST19);                                        00077900
WRITE(TAPE6,FL913);                                                     00078000
I:=1;                                                                   00078100
DO BEGIN                                                                00078200
  WRITE(TAPE6,FL911,LIST20);                                            00078300
  END UNTIL (I:=(I+1)) GTR NPP;                                         00078400
WRITE(TAPE6,FL914,LIST21);                                              00078500
WRITE(TAPE6,FL916);                                                     00078600
WRITE(TAPE6,FL917,LIST22);                                              00078700
WRITE(TAPE6,FL918);                                                     00078800
IF L1 GTR 1 THEN GO TO L243;                                            00078900
WRITE(TAPE6,FL919,LIST23);                                              00079000
GO TO L245;                                                             00079100
L243: WRITE(TAPE6,FL926,LIST14);                                        00079200
L245: WRITE(TAPE6,FL916);                                               00079300
IF L1 GTR 1 THEN GO TO L253;                                            00079400
WRITE(TAPE6,FL921,LIST24);                                              00079500
GO TO L260;                                                             00079600
L253: WRITE(TAPE6,FL927,LIST25);                                        00079700
L260: WRITE(TAPE6,FL922);                                               00079800
JK:=JK-6;                                                               00079900
IF JK LEQ 0 THEN GO TO L270;                                            00080000
L1:=L1+6;                                                               00080100
GO TO L216;                                                             00080200
L270: IF NA LEQ 0 THEN GO TO L5;                                        00080300
WRITE(TAPE6,FL923);                                                     00080400
L1:=1;                                                                  00080500
L2:=0;                                                                  00080600
JK:=IX;                                                                 00080700
L276: IF JK GTR 6 THEN GO TO L285;                                      00080800
L2:=L2+JK;                                                              00080900
GO TO L290;                                                             00081000
L285: L2:=L2+6;                                                         00081100
L290: IF L1 GTR 1 THEN GO TO L300;                                      00081200
I:=1;                                                                   00081300
DO BEGIN                                                                00081400
  WRITE(TAPE6,FL924,LIST26);                                            00081500
  END UNTIL (I:=(I+1)) GTR NPP;                                         00081600
GO TO L303;                                                             00081700
L300: I:=1;                                                             00081800
DO BEGIN                                                                00081900
  WRITE(TAPE6,FL930,LIST27);                                            00082000
  END UNTIL (I:=(I+1)) GTR NPP;                                         00082100
L303: JK:=JK-6;                                                         00082200
IF JK LEQ 0 THEN GO TO L5;                                              00082300
L1:=L1+6;                                                               00082400
WRITE(TAPE6[DBL]);                                                      00082500
GO TO L276;                                                             00082600
L550: WRITE(TAPE6,FL909,LIST28);                                        00082700
GO TO L500;                                                             00082800
L500: IF MTAPE LEQ 5 THEN GO TO L504;                                   00082900
REMOVE(MTAPE);                                                          00083000
L504: GO TO FINIS;                                                      00083100
 END END;                                                               00083200
COMMENT  INITIALIZING BLOCK;                                            00083300
  XPR:=Q:=K:=0;                                                         00083400
MAINPRO; FINIS:                                                         00083500
END.                                                                    00083600
