FILE  5 = CARD, UNIT = READER, RECORD = 10,BUFFER = 2                   00000100
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00000200
FILE  3 = TAPE3, UNIT = TAPE, RECORD = 15, BUFFER = 2                   00000300
C                                                                       00000400
C     BMD03M ---- FACTOR ANALYSIS (FORTRAN)                             00000500
C                                                                       00000600
CMULTR   SUBROUTINE MULTR(A,N,U)                 JANUARY 1, 1966        00000700
      SUBROUTINE MULTR(A,N,U)                                           00000800
      DIMENSION V(80), U(80), A(80,80)                                  00000900
      DO 1 I=1,N                                                        00001000
      IF (A(I,I))1,1,16                                                 00001100
   16 K=I                                                               00001200
   1  V(I)=A(I,I)                                                       00001300
   2  DO 3 I=1,K                                                        00001400
      U(I)=A(K,I)                                                       00001500
   3  A(K,I)=0.                                                         00001600
      P=-U(K)                                                           00001700
      DO 4 I=K,N                                                        00001800
      U(I)=A(I,K)                                                       00001900
   4  A(I,K)=0.                                                         00002000
      U(K)=-1.                                                          00002100
      V(K)=-V(K)                                                        00002200
      H=0.                                                              00002300
      DO 8 I=1,N                                                        00002400
      Y=U(I)/P                                                          00002500
      DO 5 J=1,I                                                        00002600
   5  A(I,J)=A(I,J)+U(J)*Y                                              00002700
      IF (V(I))8,8,6                                                    00002800
   6  IF (A(I,I)-H)8,7,7                                                00002900
   7  H=A(I,I)                                                          00003000
      K=I                                                               00003100
   8  CONTINUE                                                          00003200
      IF (H-.00001)9,9,2                                                00003300
   9  DO 10 I=1,N                                                       00003400
      DO 10 J=1,I                                                       00003500
      A(I,J)=-A(I,J)                                                    00003600
   10 A(J,I)=A(I,J)                                                     00003700
      DO 15 K=1,N                                                       00003800
      IF (V(K))11,14,14                                                 00003900
   11 DO 13 I=1,N                                                       00004000
      IF (V(I))13,12,12                                                 00004100
   12 IF (A(I,I)-A(I,K)*A(I,K)/A(K,K)+.00001)14,14,13                   00004200
   13 CONTINUE                                                          00004300
      U(K)=1.-1./A(K,K)                                                 00004400
      GO TO 15                                                          00004500
   14 U(K)=-V(K)                                                        00004600
   15 CONTINUE                                                          00004700
      DO 17 I=1,N                                                       00004800
      DO 17 J=1,I                                                       00004900
      IF (V(I))18,19,19                                                 00005000
   18 IF (V(J))17,19,19                                                 00005100
   19 A(I,J)=0.                                                         00005200
      A(J,I)=0.                                                         00005300
   17 CONTINUE                                                          00005400
      RETURN                                                            00005500
      END                                                               00005600
CJACOBI     SUBROUTINE FOR BMD03M       7-1-64                          00005700
      SUBROUTINE JACOBI%A,B,N,ACC,IV,NR[                                00005800
      DIMENSION A%80,80[,B%80,80[,LK%80[,Q%80[                          00005900
      IF%IV-1[200,201,200                                               00006000
 201  DO 202 I#1,N                                                      00006100
      DO 204 J#1,N                                                      00006200
 203  B%I,J[#0.                                                         00006300
 204  B(J,I) = 0.                                                       00006400
 202  B%I,I[#1.                                                         00006500
 200  NR#0                                                              00006600
      Q%1[ # 0.                                                         00006700
      W#0.                                                              00006800
      H # .5 * A%1,1[ * A%1,1[                                          00006900
      DO 1 I#2,N                                                        00007000
      H#H&.5*A%I,I[*A%I,I[                                              00007100
      Q%I[#0.                                                           00007200
      I1#I-1                                                            00007300
      DO 2 J#1,I1                                                       00007400
      Z#ABS %A%I,J[[                                                    00007500
      H#H&Z*Z                                                           00007600
      IF%Z-Q%I[[2,2,3                                                   00007700
 3    Q%I[#Z                                                            00007800
      LK%I[#J                                                           00007900
 2    CONTINUE                                                          00008000
      IF%Q%I[-W[1,1,4                                                   00008100
 4    W#Q%I[                                                            00008200
      III#I                                                             00008300
 1    CONTINUE                                                          00008400
      H#ACC*SQRT %2.*H[/FLOAT %N[                                       00008500
 30   II#LK%III[                                                        00008600
      JJ#III                                                            00008700
      X#A%II,II[                                                        00008800
      Y#A%JJ,II[                                                        00008900
      Z#A%JJ,JJ[                                                        00009000
      W#X-Z                                                             00009100
      T#.5*%W&SQRT %W*W&4.*Y*Y[[/Y                                      00009200
      W#SQRT %1.&T*T[                                                   00009300
      S#T/W                                                             00009400
      C#1./W                                                            00009500
      CC#C*C                                                            00009600
      SS#S*S                                                            00009700
      SC#S*C*2.                                                         00009800
      Q1#0.                                                             00009900
      Q2#0.                                                             00010000
      W#0.                                                              00010100
      NR#NR&1                                                           00010200
      DO 27 I#1,N                                                       00010300
      IF%I-II[10,11,12                                                  00010400
 10   U#A%II,I[                                                         00010500
      V#A%JJ,I[                                                         00010600
      E#U*S&V*C                                                         00010700
      A%II,I[#E                                                         00010800
      IF%ABS %E[-Q1[15,15,14                                            00010900
 14   Q1#ABS %E[                                                        00011000
      I1#I                                                              00011100
 15   F#V*S-U*C                                                         00011200
      A%JJ,I[#F                                                         00011300
      IF%ABS %F[-Q2[9,9,16                                              00011400
 16   Q2#ABS %F[                                                        00011500
      I2#I                                                              00011600
      GO TO 9                                                           00011700
 11   A%II,I[#SS*X&SC*Y&CC*Z                                            00011800
      Q%I[#Q1                                                           00011900
      LK%I[#I1                                                          00012000
      GO TO 9                                                           00012100
 12   IF%I-JJ[17,18,19                                                  00012200
 17   U#A%I,II[                                                         00012300
      V#A%JJ,I[                                                         00012400
      E#S*U&C*V                                                         00012500
      A%I,II[#E                                                         00012600
      IF%ABS %E[-Q%I[[15,15,21                                          00012700
 21   LK%I[#II                                                          00012800
      Q%I[#ABS %E[                                                      00012900
      GO TO 15                                                          00013000
 18   A%JJ,I[#CC*X-SC*Y&SS*Z                                            00013100
      A%I,II[#0.                                                        00013200
      Q%I[#Q2                                                           00013300
      LK%I[#I2                                                          00013400
      GO TO 9                                                           00013500
 19   U#A%I,II[                                                         00013600
      V#A%I,JJ[                                                         00013700
      E#U*S&V*C                                                         00013800
      F#V*S-U*C                                                         00013900
      A%I,II[#E                                                         00014000
      A%I,JJ[#F                                                         00014100
      G#AMAX1%ABS %E[,ABS %F[[                                          00014200
      IF%G-Q%I[[9,9,13                                                  00014300
 13   Q%I[#G                                                            00014400
      IF%ABS %E[-ABS %F[[23,24,24                                       00014500
 24   LK%I[#II                                                          00014600
      GO TO 9                                                           00014700
 23   LK%I[#JJ                                                          00014800
 9    IF%Q%I[-W[40,25,25                                                00014900
 25   W#Q%I[                                                            00015000
      III#I                                                             00015100
 40   IF%IV[27,27,33                                                    00015200
 33   U#B%I,II[                                                         00015300
      V#B%I,JJ[                                                         00015400
      B%I,II[#U*S&V*C                                                   00015500
      B%I,JJ[#V*S-U*C                                                   00015600
 27   CONTINUE                                                          00015700
      IF%W-H[31,31,30                                                   00015800
 31   IF%IV[55,55,56                                                    00015900
 56   DO 50 I#1,N                                                       00016000
      U#-1.E20                                                          00016100
      DO 51 J#I,N                                                       00016200
      IF%A%J,J[-U[51,51,52                                              00016300
 52   U#A%J,J[                                                          00016400
      K#J                                                               00016500
 51   CONTINUE                                                          00016600
      IF%K-I[53,50,53                                                   00016700
 53   A%K,K[#A%I,I[                                                     00016800
      A%I,I[#U                                                          00016900
      DO 54 J#1,N                                                       00017000
      U#B%J,K[                                                          00017100
      B%J,K[#B%J,I[                                                     00017200
 54   B%J,I[#U                                                          00017300
 50   CONTINUE                                                          00017400
 55   RETURN                                                            00017500
      END                                                               00017600
CROTATE  SUBROUTINE ROTATE FOR BMD037            JANUARY 1, 1966        00017700
      SUBROUTINE ROTATE %A,N,L,H,TV[                                    00017800
      DIMENSION A%80,80[,TV%80[,H%80[                                   00017900
      DIMENSION P%80,80[,R%80,80[,SUMXX%80,80[,MM%80[                   00018000
C     COMMON A,P,R,SUMXX,MM,H,TV                                        00018100
      COMMON R,SUMXX,MM                                                 00018200
      EQUIVALENCE%SUMXX,P[                                              00018300
      EPS#0.00116                                                       00018400
      P%1,1[ # 1.0                                                      00018500
      NCOUNT#0                                                          00018600
      MM%1[ # %L*%L-1[[/2                                               00018700
      TV%1[#0.0                                                         00018800
      LL#L-1                                                            00018900
      NV#1                                                              00019000
      FN#N                                                              00019100
      FFN#FN**2                                                         00019200
      CONS#1.0/SQRT %2.0[                                               00019300
      ZERO#1.E-4                                                        00019400
      DO 3 I#1,N                                                        00019500
    3 H%I[#0.0                                                          00019600
      DO 4 I#1,N                                                        00019700
      DO 4 J#1,L                                                        00019800
    4 H%I[#H%I[&A%I,J[*A%I,J[                                           00019900
      DO 5 I#1,N                                                        00020000
      H%I[#SQRT %H%I[[                                                  00020100
      DO 5 J#1,L                                                        00020200
    5 A%I,J[#A%I,J[/H%I[                                                00020300
  222 NV#NV&1                                                           00020400
      TV%NV[#0.0                                                        00020500
      LV#NV-1                                                           00020600
      DO 88 J#1,L                                                       00020700
      AA#0.0                                                            00020800
      BB#0.0                                                            00020900
      DO 77 I#1,N                                                       00021000
      CC#A%I,J[*A%I,J[                                                  00021100
      AA#AA&CC                                                          00021200
   77 BB#BB&CC**2                                                       00021300
   88 TV%NV[#TV%NV[&%FN*BB-AA**2[/FFN                                   00021400
      IF%NV-50[9,999,999                                                00021500
 9    IF%ABS %TV%NV[-TV%LV[[-1.E-6[999,999,13                           00021600
   13 DO 500 J#1,LL                                                     00021700
      II#J&1                                                            00021800
      DO 500 K#II,L                                                     00021900
      IF%NCOUNT-MM%1[[32,500,500                                        00022000
   32 IF%ABS %P%1,1[-ZERO[[14,14,10                                     00022100
   10 NCOUNT#0                                                          00022200
   14 AA#0.0                                                            00022300
      BB#0.0                                                            00022400
      CC#0.0                                                            00022500
      DD#0.0                                                            00022600
      DO15I#1,N                                                         00022700
      U#%A%I,J[&A%I,K[[*%A%I,J[-A%I,K[[                                 00022800
      T#A%I,J[*A%I,K[                                                   00022900
      T#T&T                                                             00023000
      CC#CC&%U&T[*%U-T[                                                 00023100
      DD#DD&2.0*U*T                                                     00023200
      AA#AA&U                                                           00023300
   15 BB#BB&T                                                           00023400
      T#DD-2.0*AA*BB/FN                                                 00023500
      B#CC-%AA**2-BB**2[/FN                                             00023600
      P%1,1[ # 0.25 * ATAN %T/B[                                        00023700
      IF%ABS %P%1,1[-ZERO[[7,7,69                                       00023800
    7 NCOUNT#NCOUNT&1                                                   00023900
      GO TO 500                                                         00024000
   69 TAN4P#T/B                                                         00024100
      IF%T-B[1041,1433,1042                                             00024200
 1433 IF%T&B-EPS[500,1043,1043                                          00024300
 1043 COS4T#CONS                                                        00024400
      SIN4T#CONS                                                        00024500
      GO TO 5000                                                        00024600
 1041 TAN4T#ABS %T[/ABS %B[                                             00024700
      IF%TAN4T-EPS[8000,1100,1100                                       00024800
 1100 COS4T#1.0/SQRT %1.0&TAN4T**2[                                     00024900
      SIN4T#TAN4T*COS4T                                                 00025000
      GO TO 5000                                                        00025100
 8000 IF%B[1150,500,500                                                 00025200
 1150 SINP#CONS                                                         00025300
      COSP#CONS                                                         00025400
      GO TO 1000                                                        00025500
 1042 CTN4T#ABS %T/B[                                                   00025600
      IF%CTN4T-EPS[9000,1200,1200                                       00025700
 1200 SIN4T#1.0/SQRT %1.0&CTN4T**2[                                     00025800
      COS4T#CTN4T*SIN4T                                                 00025900
      GO TO 5000                                                        00026000
 9000 COS4T#0.0                                                         00026100
      SIN4T#1.0                                                         00026200
 5000 COS2T#SQRT %%1.0&COS4T[/2.0[                                      00026300
      SIN2T#SIN4T/%2.0*COS2T[                                           00026400
      COST#SQRT %%1.0&COS2T[/2.0[                                       00026500
      SINT#SIN2T/%2.0*COST[                                             00026600
      IF%B[1250,1250,1300                                               00026700
 1300 COSP#COST                                                         00026800
      SINP#SINT                                                         00026900
      GO TO 7000                                                        00027000
 1250 COSP#CONS*COST&CONS*SINT                                          00027100
      SINP#ABS %CONS*COST-CONS*SINT[                                    00027200
 7000 IF%T[1400,1400,1000                                               00027300
 1400 SINP#-SINP                                                        00027400
 1000 DO 100 I#1,N                                                      00027500
      AA#A%I,J[*COSP&A%I,K[*SINP                                        00027600
      BB#-A%I,J[*SINP&A%I,K[*COSP                                       00027700
      A%I,J[#AA                                                         00027800
  100 A%I,K[#BB                                                         00027900
  500 CONTINUE                                                          00028000
      GO TO 222                                                         00028100
  999 DO 6 I#1,N                                                        00028200
      DO 6 J#1,L                                                        00028300
    6 A%I,J[#A%I,J[*H%I[                                                00028400
      NC#NV-2                                                           00028500
      WRITE(6,901) L                                                    00028600
      WRITE(6,902) NC                                                   00028700
      WRITE(6,903)                                                      00028800
C     THE FOLLOWING CARDS THROUGH STATEMENT NUMBER 256 ARE USED TO      00028900
C     YEILD THE GREATEST NUMBER OF POSITIVE FACTORS CORRESPONDING TO    00029000
C     THE EIGENVECTORS.                                                 00029100
      DO 256 I3=1,L                                                     00029200
      NMINUS=0                                                          00029300
      DO 252 I1=1,N                                                     00029400
      IF (A(I1,I3))251,252,252                                          00029500
  251 NMINUS=NMINUS+1                                                   00029600
  252 CONTINUE                                                          00029700
      IF (N-2*NMINUS)253,256,256                                        00029800
  253 DO 254 I2=1,N                                                     00029900
  254 A(I2,I3)=-A(I2,I3)                                                00030000
  256 CONTINUE                                                          00030100
      DO 310 I#1,N                                                      00030200
      WRITE(6,910) I                                                    00030300
  310 WRITE(6,911) (A(I,J),J=1,L)                                       00030400
      WRITE(6,900)                                                      00030500
      WRITE(6,904)                                                      00030600
      WRITE(6,905)                                                      00030700
      DO 320 I#2,NV                                                     00030800
      NC#I-2                                                            00030900
  320 WRITE(6,906) NC,TV(I)                                             00031000
      WRITE(6,900)                                                      00031100
      WRITE(6,907)                                                      00031200
      WRITE(6,908)                                                      00031300
      DO 330 I#1,N                                                      00031400
      H%I[#H%I[*H%I[                                                    00031500
      BB#0.0                                                            00031600
      DO 322 J#1,L                                                      00031700
 322  BB#BB&A%I,J[**2                                                   00031800
      AA#BB-H%I[                                                        00031900
 330  WRITE (6,909) I,H%I[,BB,AA                                        00032000
 900  FORMAT%1H0[                                                       00032100
 901  FORMAT%26H0NUMBER OF FACTORS ROTATEDI4[                           00032200
 902  FORMAT%27H NUMBER OF ITERATION CYCLESI3//[                        00032300
 903  FORMAT%22H0ROTATED FACTOR MATRIX[                                 00032400
 904  FORMAT%34H0ORIGINAL AND SUCCESSIVE VARIANCES[                     00032500
 905  FORMAT%12H0  CYCLE NO.10X,9HVARIANCES[                            00032600
 906  FORMAT%I8,5X,F18.7[                                               00032700
 907  FORMAT%23H0CHECK ON COMMUNALITIES[                                00032800
 908  FORMAT%12H0  VARIABLES8X,8HORIGINAL9X,5HFINAL8X,10HDIFFERENCE[    00032900
 909  FORMAT%I8,5X,3F15.5[                                              00033000
 910  FORMAT%9H0VARIABLEI3[                                             00033100
 911  FORMAT%10F12.5[                                                   00033200
      RETURN                                                            00033300
      END                                                               00033400
CTPWD    SUBROUTINE TPWD FOR BMD03M         VERSION OF MARCH 3,1964     00033500
C     VERSION II ALLOWS ALTERNATE BINARY INPUT                          00033600
      SUBROUTINE TPWD%LT1,LT2[                                          00033700
      NT1#IABS %LT1[                                                    00033800
      NT2#IABS %LT2[                                                    00033900
      IF%NT1[12,10,12                                                   00034000
 10   LT1#5                                                             00034100
      NT1#5                                                             00034200
 12   IF%NT1-NT2[14,19,14                                               00034300
 14   IF%NT2-5[15,19,19                                                 00034400
   15 REWIND NT2                                                        00034500
   19 IF%NT1-5[18,24,18                                                 00034600
 18   IF%NT1-6[22,40,22                                                 00034700
 22   REWIND NT1                                                        00034800
 24   LT2#LT1                                                           00034900
 28   RETURN                                                            00035000
   40 WRITE(6,49)                                                       00035100
      CALL EXIT                                                         00035200
 49   FORMAT%25H ERROR ON TAPE ASSIGNMENT[                              00035300
      END                                                               00035400
CVFCHCK    SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE FORMAT CRDS00035500
      SUBROUTINE VFCHCK%NVF[                                            00035600
      IF%NVF[10,10,20                                                   00035700
  10  WRITE (6,4000)                                                    00035800
      NVF#1                                                             00035900
 50   RETURN                                                            00036000
C                                                                       00036100
  20  IF%NVF-6[50,50,10                                                 00036200
C                                                                       00036300
 4000 FORMAT%1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIF00036400
     XIED, ASSUMED TO BE 1.[                                            00036500
      END                                                               00036600
CBMD03M       FACTOR ANALYSIS                   JANUARY 1, 1966         00036700
      DIMENSION A%80,80[,P%80,80[,R%80,80[,MM%80[,SUMXX%80,80[,         00036800
     1SUMDV%80[,C%80[                                                   00036900
      DIMENSION LL%80[                                                  00037000
      DIMENSION TC%255[,XM%80[,SD%80[,FFMT%12[                          00037100
C     COMMON A,  R,SUMXX,MM,SUMDV,C                                     00037200
      COMMON A,  R,SUMXX,MM,C                                           00037300
      EQUIVALENCE%SUMXX,P[                                              00037400
C                                                                       00037500
 412  FORMAT(39H1BMD03M - FACTOR ANALYSIS - VERSION OF                  00037600
     X16HJANUARY  1, 1966/                                              00037700
     135H HEALTH SCIENCES COMPUTING FACILITY//                          00037800
     214H PROBLEM CODE A6,/                                             00037900
     318H NO. OF VARIABLES I3,/                                         00038000
     414H NO. OF CASES I5,//[                                           00038100
C                                                                       00038200
      MTAPE #5                                                          00038300
      A123#%&6HFINISH[                                                  00038400
      B123#%&6HPROBLM[                                                  00038500
      C123#%&6HCOMMUN[                                                  00038600
   10 READ(5,900) TODE,RNN,NV,N,INTYPE,IIDIAG,NRO,CNFLAM,ITF,N          00038700
     1FP,NTAPE,NN                                                       00038800
      LO#0                                                              00038900
      NJ#1                                                              00039000
      IF %TODE-A123[ 400,401,400                                        00039100
  409 WRITE(6,905)                                                      00039200
      GO TO 401                                                         00039300
  402 WRITE(6,404)                                                      00039400
 401  IF%NTAPE-5[371,371,370                                            00039500
C 370 CALL REMOVE%NTAPE[                                                00039600
  370 CONTINUE                                                          00039700
 371  CALL EXIT                                                         00039800
  400 IF %TODE - B123[ 402,403,402                                      00039900
 403  CALL TPWD%NTAPE,MTAPE[                                            00040000
      IF%NTAPE[476,402,405                                              00040100
  476 N12#IABS %NTAPE[                                                  00040200
      NJ#2                                                              00040300
      GO TO 307                                                         00040400
  405 CALL VFCHCK%NN[                                                   00040500
 408  NN#NN*12                                                          00040600
      READ (5,901) (SUMDV(I), I=1,NN)                                   00040700
  307 IF%%NV-1[*%NV-81[[406,409,409                                     00040800
  406 IF%INTYPE*%INTYPE-4[[ 407,409,409                                 00040900
  407 IF%IIDIAG-5[20,409,409                                            00041000
 20   ON#N                                                              00041100
      IF(-ITF)7192,7191,7191                                            00041200
 7192 IF(INTYPE-1)7193,7191,7193                                        00041300
 7193 WRITE(6,7194)                                                     00041400
      ITF = 0                                                           00041500
      READ (5,900) XAX                                                  00041600
 7191 ONN = ON-1.                                                       00041700
      WRITE(6,412) RNN,NV,N                                             00041800
      IF%N*%N-NV[[550,554,554                                           00041900
 550  IF%IIDIAG-2[554,552,554                                           00042000
 552  IIDIAG#1                                                          00042100
 554  GO TO %30,35,40[,INTYPE                                           00042200
   30 DO 25 I#1,NV                                                      00042300
      XM(I)=0.                                                          00042400
 553  DO 25 J#1,NV                                                      00042500
 25   SUMXX%I,J[#0.0                                                    00042600
      IF%ITF[ 3240,3240,3241                                            00042700
 3241 READ (5,901) FFMT                                                 00042800
      REWIND 3                                                          00042900
 3240 IF%N-9999[49,49,409                                               00043000
 49   IF%N[409,409,50                                                   00043100
 50   DO 100 I#1,N                                                      00043200
  55  READ (5    ,SUMDV) (C(J), J=1,NV)                                 00043300
      GO TO 80                                                          00043400
   60 READ (N12) (C(J),J=1,NV)                                          00043500
   80 H=I                                                               00043600
      H1=H*(H-1.)                                                       00043700
      DO 85 J=1,NV                                                      00043800
      SD(J)=(C(J)-XM(J))/H                                              00043900
      XM(J)=XM(J)+SD(J)                                                 00044000
      DD=SD(J)*H1                                                       00044100
      DO 85 K=1,J                                                       00044200
   85 SUMXX(K,J)=SUMXX(K,J)+DD*SD(K)                                    00044300
      IF%ITF[ 100,100,3280                                              00044400
 3280 DO 3202 J#1,NV                                                    00044500
      IF%LO-255[ 3200,3201,3201                                         00044600
 3201 WRITE (3) TC                                                      00044700
      LO#0                                                              00044800
 3200 LO#LO&1                                                           00044900
 3202 TC%LO[#C%J[                                                       00045000
 100  CONTINUE                                                          00045100
      IF%ITF[ 3281,3281,3283                                            00045200
 3283 WRITE (3) TC                                                      00045300
      ENDFILE 3                                                         00045400
      REWIND 3                                                          00045500
 3281 NJ#NV-1                                                           00045600
      DO 103 I#1,NJ                                                     00045700
      NK#I&1                                                            00045800
      DO 103 J#NK,NV                                                    00045900
 103  SUMXX%J,I[#SUMXX%I,J[                                             00046000
      WRITE (6,912)                                                     00046100
      WRITE(6,913) (XM(I),I=1,NV)                                       00046200
      DO 135 I#1,NV                                                     00046300
  135 SD(I)=SQRT(SUMXX(I,I)/ONN)                                        00046400
      WRITE (6,914)                                                     00046500
      WRITE(6,913) (SD(I), I=1,NV)                                      00046600
      DO 140 I#1,NV                                                     00046700
      DO 140 J#I,NV                                                     00046800
  140 R(I,J)=SUMXX(I,J)                                                 00046900
      DO 141 I#1,NJ                                                     00047000
      NK#I&1                                                            00047100
      DO 141 J#NK,NV                                                    00047200
 141  R%J,I[#R%I,J[                                                     00047300
      NN#NV-1                                                           00047400
 215  DO 220 I#1,NV                                                     00047500
  220 SUMDV(I)=SQRT(R(I,I))                                             00047600
      DO 225 I#1,NV                                                     00047700
      DO 225 J#I,NV                                                     00047800
 225  P%I,J[#R%I,J[/%SUMDV%I[*SUMDV%J[[                                 00047900
      DO 226 I#1,NN                                                     00048000
      NK#I&1                                                            00048100
      DO 226 J#NK,NV                                                    00048200
 226  P%J,I[#P%I,J[                                                     00048300
      GO TO 227                                                         00048400
   35 DO 142 I#1,NV                                                     00048500
      GO TO%145,150[,NJ                                                 00048600
  145 READ (NTAPE,SUMDV) (P(I,J), J=1,NV)                               00048700
      GO TO 142                                                         00048800
  150 READ (N12) (P(I,J), J=1,NV)                                       00048900
  142 CONTINUE                                                          00049000
  227 IF (ITF)7123,7123,7234                                            00049100
 7123 IF(IIDIAG-2)7345,7234,7345                                        00049200
 7234 DO 200 I=1,NV                                                     00049300
      DO 200 J=1,NV                                                     00049400
  200 R%I,J[#P%I,J[                                                     00049500
      CALL MULTR(R,NV,SUMDV)                                            00049600
 7345 GO TO (160,180,185,190),IIDIAG                                    00049700
  180 DO 230 I=1,NV                                                     00049800
  230 P(I,I)=SUMDV(I)                                                   00049900
      GO TO 160                                                         00050000
 185  DO 110 I#1,NV                                                     00050100
      P%I,I[#0.                                                         00050200
      DO 110 J#1,NV                                                     00050300
      IF%P%I,I[-ABS %P%I,J[[[115,110,110                                00050400
 115  P%I,I[#ABS %P%I,J[[                                               00050500
 110  CONTINUE                                                          00050600
      GO TO 160                                                         00050700
  950 FORMAT%A6,11F6.0[                                                 00050800
  190 DO 8888 JJ#1,NV,11                                                00050900
      KK# MIN0 %JJ&10,NV[                                               00051000
      READ (5,950) COMMUN,(P(J,J), J=JJ,KK)                             00051100
      IF %COMMUN-C123[ 402,8888,402                                     00051200
 8888 CONTINUE                                                          00051300
  160 WRITE (6,915)                                                     00051400
      DO 238 I#1,NV                                                     00051500
      WRITE(6,916) I                                                    00051600
  238 WRITE(6,917) (P(I,J), J=1,NV)                                     00051700
  241 CALL JACOBI(P,A,NV,1.E-7,1,NROT)                                  00051800
      LLL#0                                                             00051900
      NZ#0                                                              00052000
      DO 243 I#1,NV                                                     00052100
      SUMDV%I[#P%I,I[                                                   00052200
      DO 1724 J=1,NV                                                    00052300
 1724 P(I,J)=A(I,J)                                                     00052400
      IF%SUMDV%I[[ 2421,2422,2422                                       00052500
 2421 NZ#NZ&1                                                           00052600
      GO TO 242                                                         00052700
 2422 IF%SUMDV%I[-CNFLAM[ 242,243,243                                   00052800
 242  LLL#LLL&1                                                         00052900
 243  CONTINUE                                                          00053000
      NZ#NV-NZ                                                          00053100
  224 IF %NZ[10,10,500                                                  00053200
  500 WRITE(6,918)                                                      00053300
  918 FORMAT%12H0EIGENVALUES//[                                         00053400
  920 FORMAT%13H0EIGENVECTORS//[                                        00053500
      WRITE            (6,917)%SUMDV%I[,I#1,NV[                         00053600
      IF%NZ-NV[1775,1774,1775                                           00053700
 1775 WRITE(6,1789)                                                     00053800
 1789 FORMAT%95H0ONLY THE POSITIVE EIGENVALUES AND ASSOCIATED VECTORS WI00053900
     1LL BE USED IN THE FOLLOWING COMPUTATION[                          00054000
 1774 SUM#.0                                                            00054100
      FNV#NV                                                            00054200
      DO 250  I=1,NZ                                                    00054300
      SUM#SUM&SUMDV%I[                                                  00054400
 250  A%I,1[# SUM  /FNV                                                 00054500
      WRITE(6,919)                                                      00054600
      WRITE(6,917) (A(I,1), I=1,NZ)                                     00054700
      WRITE(6,920)                                                      00054800
      DO 255 J#1,NZ                                                     00054900
      WRITE(6,921) J                                                    00055000
  255 WRITE(6,917) (P(I,J), I=1,NV)                                     00055100
      DO 260 I#1,NZ                                                     00055200
      SUMDV%I[#SQRT %SUMDV%I[[                                          00055300
      DO 260 J#1,NV                                                     00055400
 260  A%J,I[#SUMDV%I[*P%J,I[                                            00055500
      WRITE(6,922)                                                      00055600
C     THE FOLLOWING CARDS THROUGH STATEMENT NUMBER 256 ARE USED TO      00055700
C     YEILD THE GREATEST NUMBER OF POSITIVE FACTORS CORRESPONDING TO    00055800
C     THE EIGENVECTORS.                                                 00055900
      DO 256 I3=1,NZ                                                    00056000
      NMINUS=0                                                          00056100
      DO 252 I1=1,NV                                                    00056200
      IF (A(I1,I3))251,252,252                                          00056300
  251 NMINUS=NMINUS+1                                                   00056400
  252 CONTINUE                                                          00056500
      IF (NV-2*NMINUS)253,256,256                                       00056600
  253 DO 254 I2=1,NV                                                    00056700
  254 A(I2,I3)=-A(I2,I3)                                                00056800
  256 CONTINUE                                                          00056900
      DO 265 I#1,NV                                                     00057000
      WRITE(6,923) I                                                    00057100
  265 WRITE(6,917) (A(I,J), J=1,NZ)                                     00057200
      IF%NV-NZ[ 269, 269, 266                                           00057300
 266  DO 267 I#1,NZ                                                     00057400
      DO 267 J#1,NZ                                                     00057500
      P(I,J)=0.0                                                        00057600
      DO 267 K#1,NV                                                     00057700
  267 P(I,J)=P(I,J)+A(K,I)*A(K,J)                                       00057800
      WRITE(6,911)                                                      00057900
      WRITE(6,926)                                                      00058000
      GO TO 278                                                         00058100
 269  DO 270 I#1,NV                                                     00058200
      DO 270 J#1,NV                                                     00058300
      P(I,J)=0.0                                                        00058400
      DO 270 K#1,NV                                                     00058500
  270 P(I,J)=P(I,J)+A(I,K)*A(J,K)                                       00058600
      WRITE(6,911)                                                      00058700
      WRITE(6,924)                                                      00058800
 278  DO 275 I#1,NZ                                                     00058900
      WRITE(6,916) I                                                    00059000
  275 WRITE(6,917) (P(I,J), J=1,NZ)                                     00059100
      NZ#NV-LLL                                                         00059200
      NRO# MIN0 %NRO,NZ[                                                00059300
      IF%NRO*%1-NRO[[ 325,320,320                                       00059400
  320 WRITE(6,911)                                                      00059500
      WRITE(6,927)                                                      00059600
      GO TO 10                                                          00059700
   40 DO 282 I#1,NV                                                     00059800
       GO TO %285,295[,NJ                                               00059900
  285 READ (NTAPE,SUMDV) (A(I,J), J=1,NRO)                              00060000
      GO TO 282                                                         00060100
  295 READ (N12) (A(I,J), J=1,NRO)                                      00060200
  282 CONTINUE                                                          00060300
      WRITE(6,922)                                                      00060400
      DO 305 I#1,NV                                                     00060500
      WRITE(6,923) I                                                    00060600
  305 WRITE(6,917) (A(I,J), J=1,NRO)                                    00060700
      IF%NRO-NV[325,325,409                                             00060800
  325 CALL ROTATE(A,NV,NRO,SUMDV,C)                                     00060900
      IF (-ITF)3212,10,10                                               00061000
 3212 IF (NFP)3310,3310,3311                                            00061100
 3310 NFP=NRO                                                           00061200
 3311 DO 5110 I=1,NRO                                                   00061300
      DO 5111 K=1,NV                                                    00061400
      SUMDV(K)=A(K,I)                                                   00061500
 5111 A(K,I)=0.0                                                        00061600
      DO 5110 J=1,NV                                                    00061700
      DO 5110 K=1,NV                                                    00061800
 5110 A(J,I)=A(J,I)+R(J,K)*SUMDV(K)                                     00061900
      WRITE(6,3314) ITF,FFMT                                            00062000
 3314 FORMAT%47H0FACTOR SCORES ARE COMPUTED AND WRITTEN ON TAPEI3,31H UN00062100
     XDER THE FORMAT PRINTED BELOW/1X,12A6[                             00062200
      LO#255                                                            00062300
      DO 3218 L#1,N                                                     00062400
      DO 3216 I#1,NV                                                    00062500
      IF%LO-255[ 3213,2314,2314                                         00062600
 2314 READ (3) TC                                                       00062700
      LO#0                                                              00062800
 3213 LO#LO&1                                                           00062900
 3216 C%I[#%TC%LO[-XM%I[[/SD%I[                                         00063000
      DO 3217 J#1,NRO                                                   00063100
      SUMDV%J[#0.0                                                      00063200
      DO 3217 I#1,NV                                                    00063300
 3217 SUMDV%J[#SUMDV%J[&A%I,J[*C%I[                                     00063400
 3218 WRITE(ITF,FFMT) L,(SUMDV(J),J=1,NFP)                              00063500
 3252 IF%ITF-6[7771,10,7771                                             00063600
 7771 ENDFILE ITF                                                       00063700
      REWIND ITF                                                        00063800
      GO TO 10                                                          00063900
 900  FORMAT%2A6,I2,I4,2I1,I2,F6.0,2I2,35X,I3,I2[                       00064000
 404  FORMAT%45H0CONTROL CARDS INCORRECTLY ORDERED OR PUNCHED[          00064100
 901  FORMAT%12A6[                                                      00064200
 902  FORMAT%12F6.0[                                                    00064300
 903  FORMAT%36I2[                                                      00064400
 904  FORMAT%23H1FACTOR ANALYSIS  %CASEI2,1H[/12H PROBLEM NO.A6[        00064500
 905  FORMAT%1H0,10X33HPROBLEM CARD INCORRECTLY PUNCHED.[               00064600
 906  FORMAT%20H0NUMBER OF VARIABLESI4/12H SAMPLE SIZE6X,I6[            00064700
 908  FORMAT%20H0DATA TRANSFORMATION50I2[                               00064800
 909  FORMAT%20X,50I2[                                                  00064900
 910  FORMAT%23H0NO DATA TRANSFORMATION[                                00065000
 911  FORMAT%1H0[                                                       00065100
 912  FORMAT%1H0//6H MEANS[                                             00065200
 913  FORMAT%8F15.5[                                                    00065300
 914  FORMAT%1H0//20H STANDARD DEVIATIONS[                              00065400
 915  FORMAT%1H0//25H CORRELATION COEFFICIENTS[                         00065500
 916  FORMAT%4H0ROWI3[                                                  00065600
 917  FORMAT%10F12.5[                                                   00065700
 919  FORMAT%40H0CUMULATIVE PROPORTION OF TOTAL VARIANCE[               00065800
 921  FORMAT%7H0VECTORI3[                                               00065900
 922  FORMAT%1H0//14H FACTOR MATRIX[                                    00066000
 923  FORMAT%9H0VARIABLEI3[                                             00066100
 924  FORMAT%47H0FACTOR CHECK MATRIX %CORRELATION COEFFICIENTS[[        00066200
 925  FORMAT%1H09X,62HCOMPUTATIONAL ACCURACY NOT SUFFICIENT FOR ANY MORE00066300
     1 EIGENVALUES[                                                     00066400
 926  FORMAT%45H0FACTOR CHECK MATRIX %EIGENVALUES DIAGONALLY[[          00066500
 927  FORMAT%61H0THE FACTOR MATRIX CANNOT BE ROTATED.  CHANGE THE CODE N00066600
     1UMBER[                                                            00066700
 929  FORMAT%61H0VALUE SPECIFIED TO LIMIT THE NUMBER OF FACTORS TO BE RO00066800
     1TATEDF9.4[                                                        00066900
 7194 FORMAT(56H0FACTOR SCORES CAN BE COMPUTED ONLY IF INPUT IS RAW DATA00067000
     X)                                                                 00067100
      STOP                                                              00067200
      END                                                               00067300
