FILE  5 = CARD, UNIT = READER, RECORD = 10, BUFFER = 2                  00000100
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00000200
CTPWD    SUBROUTINE TPWD FOR BMD05R         VERSION OF SEPT. 26, 1963   00000300
      SUBROUTINE TPWD(NT1,NT2)                                          00000400
      IF(NT1)40,10,12                                                   00000500
 10   NT1=5                                                             00000600
 12   IF(NT1-NT2)14,19,14                                               00000700
 14   IF(NT2-5)15,19,17                                                 00000800
   15 REWIND NT2                                                        00000900
      GO TO 19                                                          00001000
  17                                                                    00001100
   17 CONTINUE                                                          00001110
C  17 CALL REMOVE(NT2)                                                  00001200
   19 IF(NT1-5)18,24,18                                                 00001300
 18   IF(NT1-6)22,40,22                                                 00001400
 22   REWIND NT1                                                        00001500
 24   NT2=NT1                                                           00001600
 28   RETURN                                                            00001700
 40   WRITE            (6,49)                                           00001800
      CALL EXIT                                                         00001900
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              00002000
      END                                                               00002100
CVFCHCK    SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE FORMAT CRDS00002200
      SUBROUTINE VFCHCK(NVF)                                            00002300
      IF(NVF)10,10,20                                                   00002400
 10   WRITE            (6,4000)                                         00002500
      NVF=1                                                             00002600
 50   RETURN                                                            00002700
C                                                                       00002800
 20   IF(NVF-5)50,50,10                                                 00002900
C                                                                       00003000
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIF00003100
     XIED, ASSUMED TO BE 1.)                                            00003200
      END                                                               00003300
CMATINV SUBROUTINE FOR BMD05R                             10/3/63       00003400
      SUBROUTINE MATINV(A,N,B,D,T)                                      00003500
      DIMENSION A(11,11),B(10),U(10)                                    00003600
      DIMENSION IND(10),C(10)                                           00003700
      T=1.0                                                             00003800
      DO 50 I=1,N                                                       00003900
      IND(I)=0                                                          00004000
 50   C(I)=A(I,I)                                                       00004100
      D=1.                                                              00004200
      DO 8 L=1,N                                                        00004300
      X=0.                                                              00004400
      DO 4 I=1,N                                                        00004500
      IF(IND(I))4,2,4                                                   00004600
 2    IF(X-A(I,I)/C(I))3,4,4                                            00004700
    3 K=I                                                               00004800
      X=A(I,I)/C(I)                                                     00004900
    4 CONTINUE                                                          00005000
      T=AMIN1(T,X)                                                      00005100
   11 IND(K)=1                                                          00005200
      DO 5 I=K,N                                                        00005300
      U(I)=A(I,K)                                                       00005400
    5 A(I,K)=0.                                                         00005500
      Y=U(K)                                                            00005600
      D=D*Y                                                             00005700
      DO 6 I=1,K                                                        00005800
      U(I)=A(K,I)                                                       00005900
    6 A(K,I)=0.                                                         00006000
      V=B(K)                                                            00006100
      B(K)=0.                                                           00006200
      U(K)=1.                                                           00006300
      DO 8 I=1,N                                                        00006400
      DO 7 J=1,I                                                        00006500
    7 A(I,J)=A(I,J)-U(I)*U(J)/Y                                         00006600
    8 B(I)=B(I)-U(I)*V/Y                                                00006700
      RETURN                                                            00006800
      END                                                               00006900
CTRNGEN       SUBROUTINE TRNGEN FOR BMD05R             MAY 12, 1964     00007000
      SUBROUTINE TRNGEN(N,NT,M,X,LCODE,CONST)                           00007100
      ASNF(X)=ATAN (X/SQRT (1.0-X**2))                                  00007200
      DIMENSION X(500,11),LCODE(8),CONST(8)                             00007300
 4    DO 210 I=1,M                                                      00007400
      FM=CONST(I)                                                       00007500
      JUMP=LCODE(I)                                                     00007600
      DO 8 J=1,N                                                        00007700
      D=X(J,NT)                                                         00007800
      GO TO(10,20,30,40,50,60,70,80,90,110),JUMP                        00007900
   10 IF(D)99,11,12                                                     00008000
   11 X(J,NT)=0.0                                                       00008100
      GO TO 8                                                           00008200
   12 X(J,NT)=SQRT (D)                                                  00008300
      GO TO 8                                                           00008400
   20 IF(D)99,21,22                                                     00008500
   21 X(J,NT)=1.0                                                       00008600
      GO TO 8                                                           00008700
   22 X(J,NT)=SQRT (D)+SQRT (D+1.0)                                     00008800
      GO TO 8                                                           00008900
   30 IF(D)99,99,31                                                     00009000
   31 X(J,NT)=ALOG10(D)                                                 00009100
      GO TO 8                                                           00009200
   40 X(J,NT)=EXP (D)                                                   00009300
      GO TO 8                                                           00009400
   50 IF(D)99,51,52                                                     00009500
   51 X(J,NT)=0.0                                                       00009600
      GO TO 8                                                           00009700
   52 IF(D-1.0)53,54,99                                                 00009800
   54 X(J,NT)=3.14159265/2.0                                            00009900
      GO TO 8                                                           00010000
   53 A=SQRT (D)                                                        00010100
      X(J,NT)=ASNF(A)                                                   00010200
      GO TO 8                                                           00010300
   60 SAMP=N                                                            00010400
      A=D/(SAMP+1.0)                                                    00010500
      B=A+1.0/(SAMP+1.0)                                                00010600
      IF(A)99,61,62                                                     00010700
   61 IF(B)99,63,64                                                     00010800
   63 X(J,NT)=0.0                                                       00010900
      GO TO 8                                                           00011000
   64 X(J,NT)=ASNF(SQRT (B))                                            00011100
      GO TO 8                                                           00011200
   62 IF(B)99,65,66                                                     00011300
   65 X(J,NT)=ASNF(SQRT (A))                                            00011400
      GO TO 8                                                           00011500
   66 X(J,NT)=ASNF(SQRT (A))+ASNF(SQRT (B))                             00011600
      GO TO 8                                                           00011700
   70 IF(D)71,99,71                                                     00011800
   71 X(J,NT)=1.0/D                                                     00011900
      GO TO 8                                                           00012000
   80 X(J,NT)=D+FM                                                      00012100
      GO TO 8                                                           00012200
   90 X(J,NT)=D*FM                                                      00012300
      GO TO 8                                                           00012400
  110 IF(D)111,112,111                                                  00012500
  112 X(J,NT)=0.0                                                       00012600
      GO TO 8                                                           00012700
  111 X(J,NT)=D**FM                                                     00012800
    8 CONTINUE                                                          00012900
  210 CONTINUE                                                          00013000
 1000 RETURN                                                            00013100
 99   WRITE            (6,105)J,JUMP                                    00013200
      GO TO 8                                                           00013300
 105  FORMAT(18H0THE VALUE OF CASEI4,83H OF THE DEPENDENT VARIABLE VIOLA00013400
     1TED THE RESTRICTION FOR TRANSGENERATION OF THE TYPE, I3,1H./52H TH00013500
     2E PROGRAM CONTINUED LEAVING THIS VALUE UNCHANGED.)                00013600
      END                                                               00013700
CREPORT SUBROUTINE FOR BMD05R                           10/3/63         00013800
      SUBROUTINE REPORT (SSAR,SSDR,N1,N,NPP)                            00013900
      DIMENSION  XY(51,101),SS(10)                                      00014000
      COMMON XR,YR,XMAX,YMAX,XY,NCC,JX,XIJ,TC,TP,JY,YIJ,YMIN,IC,        00014100
     1 N2,NN,     SSDRM,TOTAL,SS                                        00014200
      IF(N1-100) 10, 30, 30                                             00014300
 10   NN=N-1                                                            00014400
      N2=NN-N1                                                          00014500
      D1=N1                                                             00014600
      D2=N2                                                             00014700
      SSARM=SSAR/D1                                                     00014800
      SSDRM=SSDR/D2                                                     00014900
      F=SSARM/SSDRM                                                     00015000
      TOTAL=SSAR+SSDR                                                   00015100
      IF(N1-1) 15, 15, 20                                               00015200
 15   WRITE            (6, 900)                                         00015300
      GO TO 21                                                          00015400
 20   WRITE            (6, 901) N1                                      00015500
 21   WRITE            (6, 902)                                         00015600
      WRITE            (6, 903)                                         00015700
      WRITE            (6, 904) N1,SSAR,SSARM,F                         00015800
      WRITE            (6, 905) N2,SSDR,SSDRM                           00015900
      WRITE            (6, 906) NN, TOTAL                               00016000
      NPP=NPP+1                                                         00016100
      SS(NPP)=SSAR                                                      00016200
      GO TO 70                                                          00016300
 30   N1=1                                                              00016400
      WRITE            (6, 907) NPP                                     00016500
      WRITE            (6, 921)                                         00016600
      WRITE            (6, 922)                                         00016700
      DO 60 I=1,NPP                                                     00016800
      GO TO (31,32,33,34,35,36,37,38,39,40), I                          00016900
 31   WRITE            (6, 911) N1,SS(1),SS(1)                          00017000
      GO TO 60                                                          00017100
 32   SSAR=SS(2)-SS(1)                                                  00017200
      IF(SSAR) 41, 42, 42                                               00017300
 41   SSAR=0.0                                                          00017400
 42   WRITE            (6, 912) N1,SSAR,SSAR                            00017500
      GO TO 60                                                          00017600
 33   SSAR=SS(3)-SS(2)                                                  00017700
      IF(SSAR) 43, 44, 44                                               00017800
 43   SSAR=0.0                                                          00017900
 44   WRITE            (6, 913) N1,SSAR,SSAR                            00018000
      GO TO 60                                                          00018100
 34   SSAR=SS(4)-SS(3)                                                  00018200
      IF(SSAR) 45, 46, 46                                               00018300
 45   SSAR=0.0                                                          00018400
 46   WRITE            (6, 914) N1,SSAR,SSAR                            00018500
      GO TO 60                                                          00018600
 35   SSAR=SS(5)-SS(4)                                                  00018700
      IF(SSAR) 47, 48, 48                                               00018800
 47   SSAR=0.0                                                          00018900
 48   WRITE            (6, 915) N1,SSAR,SSAR                            00019000
      GO TO 60                                                          00019100
 36   SSAR=SS(6)-SS(5)                                                  00019200
      IF(SSAR) 49, 50, 50                                               00019300
 49   SSAR=0.0                                                          00019400
 50   WRITE            (6, 916) N1,SSAR,SSAR                            00019500
      GO TO 60                                                          00019600
 37   SSAR=SS(7)-SS(6)                                                  00019700
      IF(SSAR) 52, 53, 53                                               00019800
 52   SSAR=0.0                                                          00019900
 53   WRITE            (6, 917) N1,SSAR,SSAR                            00020000
      GO TO 60                                                          00020100
 38   SSAR=SS(8)-SS(7)                                                  00020200
      IF(SSAR) 54, 55, 55                                               00020300
 54   SSAR=0.0                                                          00020400
 55   WRITE            (6, 918) N1,SSAR,SSAR                            00020500
      GO TO 60                                                          00020600
 39   SSAR=SS(9)-SS(8)                                                  00020700
      IF(SSAR) 56, 57, 57                                               00020800
 56   SSAR=0.0                                                          00020900
 57   WRITE            (6, 919) N1,SSAR,SSAR                            00021000
      GO TO 60                                                          00021100
 40   SSAR=SS(10)-SS(9)                                                 00021200
      IF(SSAR) 58, 59, 59                                               00021300
 58   SSAR=0.0                                                          00021400
 59   WRITE            (6, 920) N1,SSAR,SSAR                            00021500
 60   CONTINUE                                                          00021600
      WRITE            (6, 905) N2,SSDR,SSDRM                           00021700
      WRITE            (6, 906) NN,TOTAL                                00021800
 900  FORMAT(65H0               ANALYSIS OF VARIANCE FOR SIMPLE LINEAR R00021900
     1EGRESSION)                                                        00022000
 901  FORMAT(41H0                ANALYSIS OF VARIANCE FOR I4, 19H  DEGRE00022100
     1E POLYNOMIAL)                                                     00022200
 902  FORMAT(85H0   SOURCE OF VARIATION              DEGREE OF         S00022300
     1UM OF         MEAN          F)                                    00022400
 903  FORMAT(87H                                      FREEDOM          S00022500
     1QUARES       SQUARE       VALUE)                                  00022600
 904  FORMAT(37H0DUE TO REGRESSION.............      I6,F19.5,2F14.5)   00022700
 905  FORMAT(37H DEVIATION ABOUT REGRESSION....      I6,F19.5,F14.5)    00022800
 906  FORMAT(37H                      TOTAL....      I6,F19.5///)       00022900
 907  FORMAT(45H0              FINAL ANALYSIS OF VARIANCE FOR I4, 19H  D00023000
     1EGREE POLYNOMIAL)                                                 00023100
 911  FORMAT(37H0LINEAR TERM...................      I6,F19.5,F14.5)    00023200
 912  FORMAT(37H QUADRATIC TERM................      I6,F19.5,F14.5)    00023300
 913  FORMAT(37H CUBIC TERM....................      I6,F19.5,F14.5)    00023400
 914  FORMAT(37H QUARTIC TERM..................      I6,F19.5,F14.5)    00023500
 915  FORMAT(37H QUINTIC TERM..................      I6,F19.5,F14.5)    00023600
 916  FORMAT(37H SEXTIC TERM...................      I6,F19.5,F14.5)    00023700
 917  FORMAT(37H 7TH DEGREE TERM...............      I6,F19.5,F14.5)    00023800
 918  FORMAT(37H 8TH DEGREE TERM...............      I6,F19.5,F14.5)    00023900
 919  FORMAT(37H 9TH DEGREE TERM...............      I6,F19.5,F14.5)    00024000
 920  FORMAT(37H 10TH DEGREE TERM..............      I6,F19.5,F14.5)    00024100
 921  FORMAT(74H0   SOURCE OF VARIATION              DEGREE OF         S00024200
     1UM OF         MEAN)                                               00024300
 922  FORMAT(75H                                      FREEDOM          S00024400
     1QUARES       SQUARE)                                              00024500
 70   RETURN                                                            00024600
      END                                                               00024700
CSCALE        SUBROUTINE SCALE FOR SUB PLOTR          JULY 20, 1964     00024800
      SUBROUTINE SCALE(YMIN,YMAX,YINT,JY,TYMIN,TYMAX,YIJ,YRMA)          00024900
      DIMENSION C(10)                                                   00025000
       C(1)=  1.0                                                       00025100
       C(2)=1.5                                                         00025200
       C(3)=2.0                                                         00025300
       C(4)=3.0                                                         00025400
       C(5)=4.0                                                         00025500
      C(6)=5.0                                                          00025600
       C(7)=7.5                                                         00025700
       C(8)=10.0                                                        00025800
C     TEST=154400000000                                                 00025900
   50 YR=YMAX-YMIN                                                      00026000
      TT=YR/YINT                                                        00026100
      J=ALOG10(TT)                                                      00026200
      E=10.0**J                                                         00026300
      TT=TT/E                                                           00026400
      I=0                                                               00026500
      IF(TT-1.0)205,201,201                                             00026600
  205 TT=TT*10.0                                                        00026700
      E=E/10.0                                                          00026800
 201  I=I+1                                                             00026900
      IF(8-I)1,2,2                                                      00027000
    1 E=E*10.0                                                          00027100
      I=1                                                               00027200
    2 T3=TT-C(I)                                                        00027300
      IF (T3.GT. .000001 .OR. T3.LT.-.000001)  GO TO 252                00027400
      TT=INT(TT*1000000)/1000000                                        00027500
  252 IF(TT-C(I))233,202,201                                            00027600
  233 YIJ=C(I)*E                                                        00027700
      GO TO 203                                                         00027800
  202 Y=YMIN/C(I)                                                       00027900
      J=Y                                                               00028000
      T=J                                                               00028100
C     IF(0.0001-ABS (T-Y))204,233,233                                   00028200
      GO TO 233                                                         00028300
  204 YIJ=C(I+1)*E                                                      00028400
  203 X=((YMAX+YMIN)/YIJ-YINT )/2.0+.00001                              00028500
      K=X                                                               00028600
      IF(K)235,240,240                                                  00028700
  235 Y=K                                                               00028800
      IF(X-Y)236,240,236                                                00028900
  236 K=K-1                                                             00029000
  240 TYMIN=K                                                           00029100
      TYMIN=YIJ*TYMIN                                                   00029200
      TYMAX=TYMIN+YINT*YIJ                                              00029300
      TEST=YINT*(YIJ/10.0)                                              00029400
      IF(YRMA-TYMAX-TEST)10,10,201                                      00029500
   10 TT=YINT/10.                                                       00029600
      JY=TT+.000001                                                     00029700
      YIJ=TEST                                                          00029800
      J=TYMIN/ YIJ                                                      00029900
      IF (K)242,241,241                                                 00030000
  242 J=J-1                                                             00030100
  241 J=J*JY+JY-K                                                       00030200
      JY=J                                                              00030300
      RETURN                                                            00030400
      END                                                               00030500
CPLOTR        SUBROUTINE PLOTR                        JULY 20, 1964     00030600
      SUBROUTINE PLOTR(X,ZMIN,ZMAX,Y,SYM,WMIN,WMAX,NC,NP,YRMA,XRMA)     00030700
C                                                                       00030800
C     PLOTR WAS MODIFIED THIS DATE TO GIVE NICER SCALES.                00030900
C                                                                       00031000
      DIMENSION XY(51,101),Y(15),CLAB(12),XM(6),SYM(15),GF(10),F2T(10)  00031100
      COMMON XR,YR,XMAX,YMAX,XY,NCC,JX,XIJ,TC,TP,JY,YIJ,YMIN,IC         00031200
C                                                                       00031300
 100   FORMAT(1H 6X5(F12.3,8X),F12.3/17X,5(F12.3,8X))                   00031400
 101  FORMAT(1H F12.3,1X,A1,101A1 ,A1)                                  00031500
  102 FORMAT(1H 13X,A1,101A1,A1)                                        00031600
 1000 FORMAT(1H  14X,101A1)                                             00031700
 1001 FORMAT(15X,20(5H+....),1H+)                                       00031800
      BLANKS=(+6H      )                                                00031900
      BLANK=(+1H )                                                      00032000
C      XM(1)=770000000000                                               00032100
C        XM(2)=007700000000                                             00032200
C       XM(3)=000077000000                                              00032300
C        XM(4)=000000770000                                             00032400
C       XM(5)=000000007700                                              00032500
C       XM(6)=000000000077                                              00032600
      GF(1)=(+6H1X    )                                                 00032700
      GF(2)=(+6H2X    )                                                 00032800
      GF(3)=(+6H3X    )                                                 00032900
      GF(4)=(+6H4X    )                                                 00033000
      GF(5)=(+6H5X    )                                                 00033100
      GF(6)=(+6H6X    )                                                 00033200
      GF(7)=(+6H7X    )                                                 00033300
      GF(8)=(+6H8X    )                                                 00033400
      GF(9)=(+6H9X    )                                                 00033500
      GF(10)=(+6H10X   )                                                00033600
      F2T(1)=(+6H(17X  )                                                00033700
      F2T(2)=BLANKS                                                     00033800
      F2T(3)=BLANKS                                                     00033900
      F2T(4)=(+6H4(F12.)                                                00034000
      F2T(5)=(+6H3,8X),)                                                00034100
      F2T(6)=(+6HF12.3 )                                                00034200
      F2T(7)=(+6H/7X   )                                                00034300
      F2T(8)=BLANKS                                                     00034400
      F2T(9)=(+6H5(F12.)                                                00034500
      F2T(10)=(+6H3,8X)))                                               00034600
      IF(JY.EQ.0) GO TO 3002                                            00034700
      IF (JY.LT.11 .AND. JY .GT. 0) GO TO 3000                          00034800
      JY=1                                                              00034900
 3000 F2T(3)=GF(JY)                                                     00035000
      F2T(8)=GF(JY)                                                     00035100
 3002 IF(NCC)48,50,48                                                   00035200
   50 KL=0                                                              00035300
      TC=(+1H.)                                                         00035400
      TP=(+1H+)                                                         00035500
      CALL SCALE(WMIN,WMAX,100.0,JY,YMIN,YMAX,YIJ,YRMA)                 00035600
      YR=YMAX-YMIN                                                      00035700
  230 J=JY                                                              00035800
      IF(J*(J-10))204,201,201                                           00035900
  201 IF(KL)220,220,231                                                 00036000
  231 WRITE            (6,1001)                                         00036100
      IF(KL)250,250,220                                                 00036200
  220 CLAB(1)= YMIN                                                     00036300
      DO 222 I=2,11                                                     00036400
  222 CLAB(I)=CLAB(I-1)+YIJ                                             00036500
      WRITE            (6,100)(CLAB(I),I=1,11,2),(CLAB(J),J=2,10,2)     00036600
      IF(KL)231,231,14                                                  00036700
  204 IF(J-5)205,221,207                                                00036800
  207 J=J-5                                                             00036900
  205 JYT=5-J                                                           00037000
  221 CONTINUE                                                          00037100
      IF(KL)226,226,227                                                 00037200
  226 F2T(3)=GF(JY)                                                     00037300
  225 F2T(8)=GF(JY)                                                     00037400
      TT=JY                                                             00037500
      TT=TT*YIJ/10.                                                     00037600
      CLAB(1)= YMIN+TT                                                  00037700
      DO 223 I=2,10                                                     00037800
  223 CLAB(I)=CLAB(I-1) +YIJ                                            00037900
      WRITE            (6,F2T)(CLAB(I),I=2,10,2),(CLAB(J),J=1,9 ,2)     00038000
      IF(KL)227,227,14                                                  00038100
  227 IF(JY-5)208,209,208                                               00038200
  209 WRITE            (6,1001)                                         00038300
      IF(KL)250,250,226                                                 00038400
  208 WRITE            (6,1000)(TC,I=1,J ),((TP,(TC,I=1,4)),K=1,19),TP,(00038500
     1 TC,I=1,JYT)                                                      00038600
      IF(KL)250,250,226                                                 00038700
  250 CONTINUE                                                          00038800
      NCC=1                                                             00038900
      IC=0                                                              00039000
      IF(NP)80,11,11                                                    00039100
       DO 1 J=1,101                                                     00039200
   11 DO 1 I=1,51                                                       00039300
    1 XY(I,J)=BLANKS                                                    00039400
      CALL SCALE (ZMIN,ZMAX,50.,JX,XMIN,XMAX,XIJ,XRMA)                  00039500
      XR=XMAX-XMIN                                                      00039600
   48 IF(NC)52,13,49                                                    00039700
   49 IF(NP)80,10,10                                                    00039800
   10 DO 9 N=1,NC                                                       00039900
      SYMB=SYM(N)                                                       00040000
      XDIFFR=XMAX-X                                                     00040100
      IF(XDIFFR)105,106,106                                             00040200
  105 XDIFFR=0.0                                                        00040300
  106 YDIFFR=YMAX-Y(N)                                                  00040400
      IF(YDIFFR)107,108,108                                             00040500
  107 YDIFFR=0.0                                                        00040600
  108 L=51.-(50.*XDIFFR)/XR+.5                                          00040700
      K=101.-(100.*YDIFFR)/YR+.5                                        00040800
C      CALL FORM2(T,M,SYMB)                                             00040900
      XY(L,K)=SYM(N)                                                    00041000
 9     CONTINUE                                                         00041100
      GO TO 15                                                          00041200
   80 DO 86 I=1,101                                                     00041300
   86 XY(1,I)=BLANKS                                                    00041400
       L=1                                                              00041500
      DO 95 N=1,NC                                                      00041600
      SYMB=SYM(N)                                                       00041700
      YDIFFR=YMAX-Y(N)                                                  00041800
      IF(YDIFFR)860,865,865                                             00041900
  860 YDIFFR=0.0                                                        00042000
  865 K=101.-(100.*YDIFFR)/YR+.5                                        00042100
C      CALL FORM2(T,M,SYMB)                                             00042200
      IF (XY(L,K).EQ.BLANK) GO TO 3005                                  00042300
      XY(L,K)=(+1HB)                                                    00042400
      GO TO 95                                                          00042500
 3005 XY(L,K)=SYM(N)                                                    00042600
   95 CONTINUE                                                          00042700
      IF(MOD  (IC,5))97,96,97                                           00042800
   96 W=TP                                                              00042900
      GO TO 98                                                          00043000
   97 W=TC                                                              00043100
   98 WRITE            (6,101)X,W,(XY(1,N),N=1,101),W                   00043200
      IC=IC+1                                                           00043300
      GO TO 15                                                          00043400
   13 M=6-JX                                                            00043500
      LL=50+M                                                           00043600
      T=JX                                                              00043700
      IF(5-JX)131,131,135                                               00043800
  131 T=0.0                                                             00043900
  135 RLAB=XMAX-(T*XIJ)/5.0                                             00044000
      W=TC                                                              00044100
      K=52                                                              00044200
      DO 31 L=M,LL                                                      00044300
      K=K-1                                                             00044400
      I=MOD  (L,5)                                                      00044500
      IF(I-1)2,3,2                                                      00044600
    3 W=TP                                                              00044700
      WRITE            (6,101)RLAB,W,(XY(K,N),N=1,101),W                00044800
      RLAB=RLAB-XIJ                                                     00044900
      W=TC                                                              00045000
      GO TO 31                                                          00045100
    2 WRITE            (6,102)W,(XY(K,N),N=1,101),W                     00045200
   31 CONTINUE                                                          00045300
   52 KL=1                                                              00045400
      GO TO 230                                                         00045500
   14 NCC=0                                                             00045600
   15 RETURN                                                            00045700
      END                                                               00045800
CGRAPH        SUBROUTINE GRAPH FOR BMD05R           AUGUST 16, 1965     00045900
      SUBROUTINE GRAPH(XX,YY,A,B,N,NPP)                                 00046000
      DIMENSION XX(500),YY(500),B(10),PP(500),FMT(104)                  00046100
      DIMENSION SYM(15),YYY(15)                                         00046200
      DIMENSION  XY(51,101)                                             00046300
      COMMON XR,YR,XMAX,YMAX,XY,NCC,JX,XIJ,TC,TP,JY,YIJ,YMIN,IC         00046400
      DO 10 I=1,N                                                       00046500
      SUM=0.0                                                           00046600
      AA=1.0                                                            00046700
      DO 5 J=1,NPP                                                      00046800
      AA=AA*XX(I)                                                       00046900
    5 SUM=SUM+B(J)*AA                                                   00047000
      PP(I)=SUM+A                                                       00047100
   10 CONTINUE                                                          00047200
      WRITE            (6,100)                                          00047300
      WRITE            (6,101 )                                         00047400
      DO 15 I=1,N                                                       00047500
      DIFF=YY(I)-PP(I)                                                  00047600
   15 WRITE            (6,102)I,XX(I),YY(I),PP(I),DIFF                  00047700
      SS=10.0**10                                                       00047800
      FF=-10.0**9                                                       00047900
      XS=SS                                                             00048000
      XL=FF                                                             00048100
      DO 300 I=1,N                                                      00048200
      XS=AMIN1(XS,XX(I))                                                00048300
      XL=AMAX1(XL,XX(I))                                                00048400
      SS=AMIN1(SS,YY(I),PP(I))                                          00048500
  300 FF=AMAX1(FF,YY(I),PP(I))                                          00048600
      WRITE            (6,4000)                                         00048700
C     MXY AND LYNN ARE DUMMY VARIABLES FOR SUBR SCALE.                  00048800
      CALL SCALE(SS,FF,100.,MXY,S1,F1,LYNN,FF)                          00048900
 4000 FORMAT(1H0//)                                                     00049000
      MXY=N                                                             00049100
      LYNN=0                                                            00049200
  303 IF(MXY-50) 301,302,302                                            00049300
  301 LYNN=1                                                            00049400
  302 SYM(1)=(+1HO)                                                     00049500
      SYM(2)=(+1HP)                                                     00049600
      DO 50 I=1,N                                                       00049700
      X=XX(I)                                                           00049800
      YYY(1)=YY(I)                                                      00049900
      YYY(2)=PP(I)                                                      00050000
      CALL PLOTR(X ,XS,XL,YYY,SYM,SS,FF,2,-1,FF,XL)                     00050100
      IF(LYNN) 50,50,304                                                00050200
  304 WRITE            (6,307)                                          00050300
  307 FORMAT(1H )                                                       00050400
   50 CONTINUE                                                          00050500
      CALL PLOTR(X ,XS,XL,YYY,SYM,SS,FF,-1,-1,FF,XL)                    00050600
      WRITE            (6,4001)                                         00050700
 4001 FORMAT(12H0GRAPH CODES//13H O=OBSERVED Y/14H P=PREDICTED Y/21H B=O00050800
     XBSERVED=PREDICTED//)                                              00050900
      WRITE            (6,218)S1,F1                                     00051000
  100 FORMAT(1H15X,18HTABLE OF RESIDUALS)                               00051100
  101 FORMAT(/6H0 NO. 4X,7HX VALUE4X,7HY VALUE4X,11HY PREDICTED4X,8HRESI00051200
     1DUAL//)                                                           00051300
  102 FORMAT(I5,F13.4,F11.4,2F13.4)                                     00051400
  218 FORMAT(26H GRAPH SCALE EXTENDS FROM F10.4,4H TO F10.4)            00051500
      RETURN                                                            00051600
      END                                                               00051700
CID   0901HS 15 150    $BMD05R POLYNOMIAL REGRESSION                    00051800
CBMD05R       POLYNOMIAL REGRESSION                 AUGUST 16, 1965     00051900
      DIMENSION X(500,11),SD(11,11),AV(11),B(10),RR(11,11)              00052000
      DIMENSION SE(10),XX(500),YY(500),FMT(120),XXB(10),KODE(8),CONST(8)00052100
      DIMENSION  XY(51,101),SS(10)                                      00052200
      COMMON XR,YR,XMAX,YMAX,XY,NCC,JX,XIJ,TC,TP,JY,YIJ,YMIN,IC,        00052300
     1 N2,NN,     SSDRM,TOTAL,SS                                        00052400
      EQUIVALENCE (IA1,A1),(IA2,A2),(IA3,A3)                            00052500
C                                                                       00052600
  918 FORMAT(45H1BMD05R - POLYNOMIAL REGRESSION - VERSION OF ,          00052700
     1  15HAUGUST 16, 1965/                                             00052800
     X41H HEALTH SCIENCES COMPUTING FACILITY, UCLA//)                   00052900
C                                                                       00053000
      A1=(+6HPROBLM)                                                    00053100
      A2=(+6HFINISH)                                                    00053200
      A3=(+6HSPECTG)                                                    00053300
      NTAPE=5                                                           00053400
 5    READ          ( 5,900)ISUM,PROB,N,NP,IPLOT,NVG,NACC,MTAPE,KVR     00053500
      NPP=0                                                             00053600
      IF(ISUM-IA1)7,4,7                                                 00053700
 7    IF(ISUM-IA2)8,15,8                                                00053800
 8    WRITE            (6,919)                                          00053900
 15   IF(6-NTAPE)16,17,17                                               00054000
 16                                                                     00054100
   16 CONTINUE                                                          00054110
C16   CALL REMOVE(NTAPE)                                                00054200
 17   CALL EXIT                                                         00054300
 4    CALL TPWD(MTAPE,NTAPE)                                            00054400
      IF(NACC)748,748,749                                               00054500
 748  NACC=13                                                           00054600
 749  TOL=10.0**(-NACC)                                                 00054700
 19   WRITE            (6, 918)                                         00054800
      WRITE            (6, 902) PROB                                    00054900
      WRITE            (6, 903) N                                       00055000
      IF(NP*(NP-11))200,8,8                                             00055100
 200  IF((N-NP)*(N-501))205,8,8                                         00055200
 205  IERROR=0                                                          00055300
      NT=NP+1                                                           00055400
      ONO=0.0                                                           00055500
      ON=N                                                              00055600
      ONO=ON                                                            00055700
      DO 721 I=1,500                                                    00055800
      X(I,NT)=0.0                                                       00055900
 721  X(I,1)=0.0                                                        00056000
      DO 20 I=1,NT                                                      00056100
      AV(I)=0.0                                                         00056200
      DO 20 J=1,NT                                                      00056300
 20   SD(I,J)=0.0                                                       00056400
      CALL VFCHCK(KVR)                                                  00056500
      KVR=KVR*12                                                        00056600
      READ           (5,916)(FMT(I),I=1,KVR)                            00056700
      IF(-NVG)22,23,23                                                  00056800
 22   WRITE            (6, 901)                                         00056900
      READ           (5,920)ISUM,M,(KODE(I),CONST(I),I=1,M)             00057000
      IF(ISUM-IA3)210,220,210                                           00057100
 210  WRITE            (6, 921)                                         00057200
      IERROR=-501                                                       00057300
      GO TO 23                                                          00057400
 220  DO 230 I=1,M                                                      00057500
      WRITE            (6, 922)KODE(I),CONST(I)                         00057600
      IF(KODE(I)*(KODE(I)-11))230,225,225                               00057700
 225  WRITE            (6, 923)KODE(I)                                  00057800
 230  CONTINUE                                                          00057900
 23   READ           (5    ,FMT)(X(I,1),X(I,NT),I=1,N)                  00058000
      IF(NTAPE-5)10,11,10                                               00058100
 10   REWIND NTAPE                                                      00058200
 11   IF(NVG)25,25,24                                                   00058300
 24   CALL TRNGEN(N,NT,M,X,KODE,CONST)                                  00058400
      IF(IERROR) 5, 25, 25                                              00058500
 25   SUMY=0.0                                                          00058600
      SUMYY=0.0                                                         00058700
      SUMXY=0.0                                                         00058800
      SUMX=0.0                                                          00058900
      SUMXX=0.0                                                         00059000
      DO 27 I=1,N                                                       00059100
      YY(I)=X(I,NT)                                                     00059200
      XX(I)=X(I,1)                                                      00059300
      SUMX=SUMX+XX(I)                                                   00059400
      SUMY=SUMY+YY(I)                                                   00059500
      SUMXY=SUMXY+XX(I)*YY(I)                                           00059600
      SUMXX=SUMXX+XX(I)*XX(I)                                           00059700
 27   SUMYY=SUMYY+YY(I)*YY(I)                                           00059800
      R=(SUMXY-SUMX*SUMY/ON)/SQRT ((SUMXX-SUMX*SUMX/ON)*(SUMYY-SUMY*SUMY00059900
     1/ON))                                                             00060000
      RRR=R*R                                                           00060100
      IF(NP-1)1010,1010,1212                                            00060200
 1212 DO 30 J=2,NP                                                      00060300
      JJ=J-1                                                            00060400
      DO 30 I=1,N                                                       00060500
 30   X(I,J)=X(I,JJ)*X(I,1)                                             00060600
 1010 DO 36 J=1,NT                                                      00060700
      DO 35 I=1,N                                                       00060800
 35   AV(J)=AV(J)+X(I,J)                                                00060900
 36   AV(J)=AV(J)/ONO                                                   00061000
      DO 40 J=1,NT                                                      00061100
      DO 40 I=1,N                                                       00061200
 40   X(I,J)=X(I,J)-AV(J)                                               00061300
      DO 46 J=1,NT                                                      00061400
      DO 46 K=J,NT                                                      00061500
      DO 45 I=1,N                                                       00061600
 45   SD(J,K)=SD(J,K)+(X(I,J)*X(I,K))                                   00061700
 46   SD(K,J)=SD(J,K)                                                   00061800
      B(1)=SD(1,NT)/SD(1,1)                                             00061900
      A=AV(NT)-B(1)*AV(1)                                               00062000
      SE(1)=SQRT ((SUMYY-A*SUMY-B(1)*SUMXY)/(SD(1,1)*(ON-2.0)))         00062100
      WRITE            (6, 904)                                         00062200
      WRITE            (6, 905)                                         00062300
      WRITE            (6, 906) AV(1)                                   00062400
      WRITE            (6, 907) AV(NT)                                  00062500
      WRITE            (6, 908) A                                       00062600
      WRITE            (6, 909) B(1)                                    00062700
      WRITE            (6, 910) SE(1)                                   00062800
      WRITE            (6,911)R                                         00062900
      SSAR=RRR*SD(NT,NT)                                                00063000
      SSDR=(1.0-RRR)*SD(NT,NT)                                          00063100
      N1=1                                                              00063200
      CALL REPORT (SSAR,SSDR,N1,N,NPP)                                  00063300
      GSSDR=SSDR                                                        00063400
      IF(NP-1)50,50,1414                                                00063500
   50 XXB(1)=B(1)                                                       00063600
      AXX=A                                                             00063700
      NXX=1                                                             00063800
      GO TO 1313                                                        00063900
 1414 DO 73 IJ=2,NP                                                     00064000
      N1=IJ                                                             00064100
      DO 52 J=1,IJ                                                      00064200
       B(J)=SD(J,NT)                                                    00064300
       DO 52 I=1,IJ                                                     00064400
 52    RR(I,J)=SD(I,J)                                                  00064500
      CALL MATINV(RR,IJ,B,D,T)                                          00064600
      IF(T-TOL)66,191,191                                               00064700
 66   WRITE            (6, 917)                                         00064800
      SSDR=GSSDR                                                        00064900
      IF(NPP) 67, 67, 75                                                00065000
 67   NPP=1                                                             00065100
      GO TO 75                                                          00065200
 191  DO 192 I=1,IJ                                                     00065300
      RR(I,I)=-RR(I,I)                                                  00065400
 192  B(I)=-B(I)                                                        00065500
      SSAR=0.0                                                          00065600
      DO 60 I=1,IJ                                                      00065700
 60   SSAR=SSAR+B(I)*SD(I,NT)                                           00065800
      SSDR=SD(NT,NT)-SSAR                                               00065900
      DF=IJ+1                                                           00066000
      VARI=SSDR/(ON-DF)                                                 00066100
      DO 62 J=1,IJ                                                      00066200
      SUMY=VARI*RR(J,J)                                                 00066300
 62   SE(J)=SQRT (SUMY)                                                 00066400
      SU=0.0                                                            00066500
      DO 64 I=1,IJ                                                      00066600
 64   SU=SU+B(I)*AV(I)                                                  00066700
 69   GSSDR=SSDR                                                        00066800
      A=AV(NT)-SU                                                       00066900
      WRITE            (6, 912) N1                                      00067000
      WRITE            (6, 908) A                                       00067100
      WRITE            (6, 913)                                         00067200
      WRITE            (6, 915) (B(I), I=1,N1)                          00067300
      DO 70 I=1,N1                                                      00067400
 70   XXB(I)=B(I)                                                       00067500
      AXX=A                                                             00067600
      NXX=N1                                                            00067700
      WRITE            (6, 914)                                         00067800
      WRITE            (6, 915) (SE(I), I=1,N1)                         00067900
      WRITE            (6, 904)                                         00068000
 73   CALL REPORT (SSAR,SSDR,N1,N,NPP)                                  00068100
 75   N1=105                                                            00068200
      WRITE            (6, 904)                                         00068300
      CALL REPORT (SSAR,SSDR,N1,N,NPP)                                  00068400
 1313 IF(IPLOT-1)5,600,5                                                00068500
 600  CALL GRAPH (XX,YY,AXX,XXB,N,NXX)                                  00068600
      GO TO 5                                                           00068700
 900  FORMAT(A6,A2,I3,4I2,49X,2I2)                                      00068800
 901  FORMAT(21H0TRANSGENERATION CARD//16H CODE   CONSTANT//)           00068900
 902  FORMAT(15H0PROBLEM NO.   A2)                                      00069000
 903  FORMAT(12H SAMPLE SIZEI5)                                         00069100
 904  FORMAT(1H0)                                                       00069200
 905  FORMAT(38H0REGRESSION - ONE INDEPENDENT VARIABLE)                 00069300
 906  FORMAT(23H0XMEAN.....            F12.5)                           00069400
 907  FORMAT(23H YMEAN.....            F12.5)                           00069500
 908  FORMAT(23H0INTERCEPT (A VALUE)...F12.5)                           00069600
 909  FORMAT(23H REG. COEFFICIENT......F12.5)                           00069700
 910  FORMAT(25H STD. ERROR OF REG. COEF.F10.5)                         00069800
 911  FORMAT(23H0CORRELATION COEF......F12.5///)                        00069900
 912  FORMAT(32H0POLYNOMIAL REGRESSION OF DEGREEI3)                     00070000
 913  FORMAT(24H0REGRESSION COEFFICIENTS)                               00070100
 914  FORMAT(25H0STD. ERROR OF REG. COEF.)                              00070200
 915  FORMAT(10F12.5)                                                   00070300
 916  FORMAT(12A6)                                                      00070400
 917  FORMAT(71H0COMPUTATIONAL ACCURACY NOT SUFFICIENT FOR POLYNOMIALS O00070500
     1F HIGHER DEGREE)                                                  00070600
 919  FORMAT(22H0ERROR ON PROBLEM CARD)                                 00070700
 920  FORMAT(A6,I1,8(I2,F6.0))                                          00070800
 921  FORMAT(96H0SPECTG CARD MISPUNCHED OR OUT OF ORDER. PROGRAM WILL PR00070900
     1OCEED TO NEXT JOB, IF ANY, OR TERMINATE.)                         00071000
 922  FORMAT(1H I3,F12.5)                                               00071100
 923  FORMAT(29H0ILLEGAL TRANSGENERATION CODEI3,67H SPECIFIED. PROGRAM W00071200
     1ILL CONTINUE LEAVING OUT THIS TRANSGENERATION.)                   00071300
      END                                                               00071400
