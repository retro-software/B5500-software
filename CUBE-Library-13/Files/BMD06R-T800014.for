FILE  5 = CRA, UNIT = READER, RECORD = 10, BUFFER = 2                   00001000
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00001100
C                                                                       00001200
CBMD06R    ASYMPTOTIC REGRESSION - MAIN PROGRAM    SEPTEMBER 29, 1965   00001300
C                                                                       00001400
      DIMENSION CF(9)                                                   00001500
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00001600
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00001700
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00001800
     3,N1(1000),N2(1000)                                                00001900
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00002000
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200002100
     2,RS,  AS,BS,SUMSQS,AVE                                            00002200
     3,X2,Y2,Y3,I5,I6                                                   00002300
      EQUIVALENCE(X1,PRE,U)                                             00002400
C                                                                       00002500
C                                                                       00002600
C                                                                       00002700
C                                                                       00002800
  502 FORMAT(45H1BMD06R - ASYMPTOTIC REGRESSION - VERSION OF ,          00002900
     X18HSEPTEMBER 29,1965/                                             00003000
     1 40H HEALTH SCIENCES COMPUTING FACILITY,UCLA//)                   00003100
  304 FORMAT(45H0CONTROL CARDS INCORRECTLY ORDERED OR PUNCHED)          00003200
 501  FORMAT(2A6,I3,A2,A3,A3,I2,F6.0,4A3,I2,A3,F6.0, 14X, 2I2)          00003300
  503 FORMAT(13H PROBLEM CARD//)                                        00003400
  504 FORMAT(13H PROBLEM CODE6X,A6)                                     00003500
  505 FORMAT(14H0ORIGINAL DATA//)                                       00003600
  506 FORMAT(5H  NO.6X,7HX VALUE6X,7HY VALUE//)                         00003700
  507 FORMAT(1H I4,2F13.4)                                              00003800
  508 FORMAT(61H0NO Y VALUE FOR SOME X,PROGRAM WILL GO TO NEXT PROBLEM,I00003900
     1F ANY)                                                            00004000
  509 FORMAT(I2)                                                        00004100
  513 FORMAT(9H0FIT NO. I3)                                             00004200
  515 FORMAT(15H0ITERATION NO. I2)                                      00004300
  516 FORMAT(2H *62X,1H*)                                               00004400
  517 FORMAT(2H *3F20.8,7H  *  Y(I1,2H)=F20.8)                          00004500
  518 FORMAT(19H0INFORMATION MATRIX//)                                  00004600
  519 FORMAT(30H0INVERSE OF INFORMATION MATRIX//)                       00004700
  520 FORMAT(/23H0INITIAL ESTIMATE OF R=F10.4//)                        00004800
  521 FORMAT(/1H 25X,19HPARAMETER ESTIMATES)                            00004900
  522 FORMAT(14H ITERATION NO.8X,1HA14X,1HB14X,1HR9X,20HSUM(E(Y)-MEAN(Y)00005000
     1)**2//)                                                           00005100
  523 FORMAT(1H I9,F19.6,2F15.6,1X,F16.6)                               00005200
  524 FORMAT(/1H04X,5HFINAL6X,8HSTANDARD)                               00005300
  525 FORMAT(1H 4X,9HESTIMATES2X,10HDEVIATIONS//)                       00005400
  526 FORMAT(3H A=F11.4,2X,F10.4//)                                     00005500
  527 FORMAT(3H B=F11.4,2X,F10.4//)                                     00005600
  528 FORMAT(3H R=F11.4,2X,F10.4)                                       00005700
  529 FORMAT(20H1 TABLE OF RESIDUALS//)                                 00005800
  530 FORMAT(16H NO. OF X VALUES3X,I6,8X,14H INPUT PATTERN9X,A6)        00005900
 532  FORMAT(12H X RE-SCALED 10X,A6,8X,5H SORT17X,A6)                   00006000
 535  FORMAT(     16H PRINT RESIDUALS 6X, A3)                           00006100
  537 FORMAT(13H X TRANS CODE6X,I6,8X,12H OUTPUT DATA10X,A3)            00006200
  538 FORMAT(11H X CONSTANT8X,F11.5,3X,16H VARIABLE FORMAT3X,I6)        00006300
  630 FORMAT(69H0 NO.         X VALUE         Y VALUE     Y PREDICTED   00006400
     1     RESIDUAL//)                                                  00006500
  631 FORMAT(1H I4,4F16.4)                                              00006600
  701 FORMAT(20H REGRESSION EQUATION/1H015X,1HX/1H 4X,11HY = A + B.R//) 00006700
 923  FORMAT(11H0ITERATION=I5, 3X, 2HA=F12.6,3X,2HB=F12.6,3X,2HR=F12.6  00006800
     1,3X, 23HSUM(E(Y)-MEAN(Y))**2  =F12.6)                             00006900
 9309 FORMAT( 36H0CONVERGENCE CRITERION NOT MET AFTER  I5, 11H ITERATION00007000
     1S  )                                                              00007100
C                                                                       00007200
C                                                                       00007300
C                                                                       00007400
C                                                                       00007500
      I5=5                                                              00007600
      I6=6                                                              00007700
      A123=(+6HFINISH)                                                  00007800
      B123=(+6HPROBLM)                                                  00007900
      D123=(+3HYES)                                                     00008000
C                                                                       00008100
 100  READ           (5,501)TODE,PROB,NXY,TXY,RES,SPARE,                00008200
     1    LTRANX,CONSX,XSORT,GPLOT,STEP,XRED,MORE,OUT,RHO1,NTAPE,KVT    00008300
C                                                                       00008400
      IF (TODE-A123) 300,326,300                                        00008500
 300  IF (TODE-B123) 302,332,302                                        00008600
 302  CONTINUE                                                          00008700
      WRITE (6,502)                                                     00008800
      WRITE (6,304)                                                     00008900
  326 CALL EXIT                                                         00009000
 332  CONTINUE                                                          00009100
      WRITE (6,502)                                                     00009200
      WRITE (6,701)                                                     00009300
      WRITE (6,503)                                                     00009400
      WRITE            ( 6,504)PROB                                     00009500
      WRITE            ( 6,530)NXY,TXY                                  00009600
      WRITE            ( 6,532)XRED,XSORT                               00009700
      WRITE            ( 6,535)RES                                      00009800
      WRITE            ( 6,537)LTRANX,OUT                               00009900
      WRITE            ( 6,538)CONSX,KVT                                00010000
      IF((NXY-3)*(NXY-1000))305,302,302                                 00010100
 305  CALL VFCHCK(KVT)                                                  00010200
 3    CONTINUE                                                          00010300
      SUMSQS(1)=0.0                                                     00010400
      SWITCH=0                                                          00010500
      LTRANX=LTRANX+1                                                   00010600
      CALL READ                                                         00010700
      NCN=NS                                                            00010800
C                                                                       00010900
C CHECK INPUT DATA                                                      00011000
      DO 70 I=1,NXY                                                     00011100
   70 N2(I)=0                                                           00011200
      N2(1)=N1(1)                                                       00011300
      DO 71 I=2,NXY                                                     00011400
   71 N2(I)=N2(I-1)+N1(I)                                               00011500
      IF (OUT-D123) 6,4,6                                               00011600
                                                                        00011700
C PRINT INPUT DATA                                                      00011800
    4 WRITE  (6,505)                                                    00011900
      WRITE (6,506)                                                     00012000
      DO 5 I=1,NXY                                                      00012100
      NE=N2(I)                                                          00012200
      NS=NE-N1(I)+1                                                     00012300
      DO 72 L=NS,NE                                                     00012400
   72 WRITE            (I6,507)I,X1(I),Y2(L)                            00012500
    5 CONTINUE                                                          00012600
    6 DO 7 I=1,NXY                                                      00012700
      IF(N1(I))8,8,7                                                    00012800
    7 CONTINUE                                                          00012900
      GO TO 10                                                          00013000
    8 WRITE            (I6,508)                                         00013100
      IF (TRANS-D123) 100,9,100                                         00013200
    9 DO 200 I=1,MORE                                                   00013300
  200 READ            (5,509)                                           00013400
      GO TO 100                                                         00013500
                                                                        00013600
C TRANSFORM X AND SORT X AND Y                                          00013700
   10 IF(LTRANX-1)    302,73,74                                         00013800
   74 CALL TRANX                                                        00013900
      GO TO 75                                                          00014000
   73 DO 76 I=1,NXY                                                     00014100
   76 X2(I)=X1(I)                                                       00014200
   75 IF (XSORT-D123) 15,11,15                                          00014300
   11 CALL SORTX                                                        00014400
      GO TO 20                                                          00014500
   15 NF=0                                                              00014600
      DO 16 I=1,NXY                                                     00014700
      NE=N2(I)                                                          00014800
      NS=NE-N1(I)+1                                                     00014900
      DO 17 J=NS,NE                                                     00015000
      NF=NF+1                                                           00015100
   17 Y3(NF)=Y2(J)                                                      00015200
   16 CONTINUE                                                          00015300
  20  IF (XRED-D123) 600,601,600                                        00015400
                                                                        00015500
C SCALE X                                                               00015600
  601 RNXY=NXY-1                                                        00015700
      SMAL=X2(1)                                                        00015800
      RANGE=X2(NXY)-SMAL                                                00015900
      DO 602 I=1,NXY                                                    00016000
  602 X2(I)=((X2(I)-SMAL)/RANGE)*RNXY                                   00016100
  600 IF(MORE)  302,26,27                                               00016200
   26 MORE=1                                                            00016300
                                                                        00016400
C BEGIN REGRESSION LOOP                                                 00016500
   27 DO 1000 IMM=1,MORE                                                00016600
      WRITE           (  6,513)IMM                                      00016700
      NF=0                                                              00016800
                                                                        00016900
C SET UP Y FOR REGRESSION AND TRANSFORMATION                            00017000
      DO 29 I=1,NXY                                                     00017100
      NE=N2(I)                                                          00017200
      NS=NE-N1(I)+1                                                     00017300
      DO 28 L=NS,NE                                                     00017400
      NF=NF+1                                                           00017500
   28 Y2(NF)=Y3(L)                                                      00017600
   29 CONTINUE                                                          00017700
   21 CALL TRANSY                                                       00017800
                                                                        00017900
C GET START VALUE FOR R                                                 00018000
 25   CALL FINDR                                                        00018100
      XN = NXY                                                          00018200
      XB = 0.                                                           00018300
      DO 3993 I=1,NXY                                                   00018400
 3993 XB = XB + X2(I)                                                   00018500
      XB = XB/XN                                                        00018600
      I = XB                                                            00018700
      XB = I                                                            00018800
      DO 3994 I=1,NXY                                                   00018900
 3994 X2(I) = X2(I)-XB                                                  00019000
      IF(RHO1)2511,2511,2510                                            00019100
 2510 R0 = RHO1                                                         00019200
 2511 CONTINUE                                                          00019300
      WRITE             (I6,520)R0                                      00019400
      PRINT = 0.                                                        00019500
      IF (STEP-D123) 4777,4877,4777                                     00019600
 4777 CONTINUE                                                          00019700
      WRITE            (I6, 522 )                                       00019800
      PRINT = 1.                                                        00019900
 4877 CONTINUE                                                          00020000
C  INITIALIZE LOOP                                                      00020100
      ICOUNT=0                                                          00020200
      NIT = 100                                                         00020300
      RS(1)=R0                                                          00020400
      TY0=0.0                                                           00020500
      DO 445 I=1,NXY                                                    00020600
      TY0=TY0+AVE(I)*(FLOAT (N1(I)))                                    00020700
  445 CONTINUE                                                          00020800
      Y012(1)=TY0                                                       00020900
      R=R0                                                              00021000
      SUMSQ = 0.                                                        00021100
C BEGIN  ITERATION FOR ALPHA, BETA AND R.                               00021200
 996  CONTINUE                                                          00021300
      ICOUNT = ICOUNT + 1                                               00021400
      R0 = R                                                            00021500
      CALL SETUP                                                        00021600
      CALL INVERT(C1,3)                                                 00021700
      A=C1(1,1)*TY0+C1(1,2)*TY1+C1(1,3)*TY2                             00021800
      B=C1(1,2)*TY0+C1(2,2)*TY1+C1(2,3)*TY2                             00021900
      R=R0+(C1(1,3)*TY0+C1(2,3)*TY1+C1(3,3)*TY2)/B                      00022000
      R = ABS (R)                                                       00022100
 40   CONTINUE                                                          00022200
      SUMSQ=0.0                                                         00022300
      DO 45 I=1,NXY                                                     00022400
      YEST=A+B*(R**X2(I))                                               00022500
      DIF=AVE(I)-YEST                                                   00022600
      SUMSQ=SUMSQ+DIF**2                                                00022700
   45 CONTINUE                                                          00022800
      B = B*R**(-XB)                                                    00022900
                                                                        00023000
C PRINT ITERATION RESULTS IF DESIRED                                    00023100
      IF (PRINT )477,481,477                                            00023200
 477  CONTINUE                                                          00023300
      WRITE            ( 6, 523) ICOUNT,A, B, R, SUMSQ                  00023400
      GO TO 47                                                          00023500
 481  CONTINUE                                                          00023600
      WRITE            ( 6, 923) ICOUNT,A, B, R, SUMSQ                  00023700
      WRITE (6,518)                                                     00023800
      DO 39 I=1,3                                                       00023900
      WRITE (6,516)                                                     00024000
      K=I-1                                                             00024100
      WRITE            ( 6,517)(C(I,J),J=1,3),K,Y012(I)                 00024200
   39 WRITE  (6,516)                                                    00024300
      WRITE (6,519)                                                     00024400
      DO 49 I=1,3                                                       00024500
      WRITE (6,516)                                                     00024600
      K=I-1                                                             00024700
      WRITE            ( 6,517)(C1(I,J),J=1,3),K,Y012(I)                00024800
 49   WRITE (6,516)                                                     00024900
 47   IF(ABS (R-R0)-ABS (R)*1.E-5)60,60,308                             00025000
 308  IF (ICOUNT-NIT+1)99,99,309                                        00025100
 99   CONTINUE                                                          00025200
      GO TO 996                                                         00025300
                                                                        00025400
C END OF ITERATION FOR ALPHA, BETA, AND R.                              00025500
C PRINT RESULTS                                                         00025600
 309  CONTINUE                                                          00025700
      WRITE            ( 6,9309 )                                       00025800
     1 NIT                                                              00025900
 60   CONTINUE                                                          00026000
      DO 6111 I=1,NXY                                                   00026100
 6111 X2(I) = X2(I) +XB                                                 00026200
      CALL SETUP                                                        00026300
      CALL INVERT(C1,3)                                                 00026400
      WRITE (6,518)                                                     00026500
      DO 62 I=1,3                                                       00026600
      WRITE (6,516)                                                     00026700
      K=I-1                                                             00026800
      WRITE            ( 6,517)(C(I,J),J=1,3),K,Y012(I)                 00026900
   62 WRITE (6,516)                                                     00027000
      WRITE (6,519)                                                     00027100
      DO 63 I=1,3                                                       00027200
      WRITE (6,516)                                                     00027300
      K=I-1                                                             00027400
      WRITE            ( 6,517)(C1(I,J),J=1,3),K,Y012(I)                00027500
   63 WRITE (6,516)                                                     00027600
      WRITE (6,524)                                                     00027700
      WRITE (6,525)                                                     00027800
      N=0                                                               00027900
      SE=0.0                                                            00028000
      DO101 I=1,NXY                                                     00028100
      NE=N2(I)                                                          00028200
      NS=NE-N1(I)+1                                                     00028300
      PRE(I)=A+B*(R**X2(I))                                             00028400
      N=N+N1(I)                                                         00028500
      DO 101 J=NS,NE                                                    00028600
      SE=SE+(Y2(J)-PRE(I))**2                                           00028700
  101 CONTINUE                                                          00028800
      FN=N-3                                                            00028900
      AS(1) = A                                                         00029000
      BS(1) = B                                                         00029100
      RS(1) = R                                                         00029200
      SE=SE/FN                                                          00029300
      SA=SQRT (C1(1,1)*SE)                                              00029400
      SB=SQRT (C1(2,2)*SE)                                              00029500
      SR=(SQRT (C1(3,3)*SE))/ABS (B)                                    00029600
      WRITE            ( 6, 526)A,SA                                    00029700
      WRITE            ( 6, 527)B,SB                                    00029800
      WRITE            ( 6,528)R, SR                                    00029900
      CALL ANOVAR                                                       00030000
      IF (RES-D123) 641,640,641                                         00030100
 640  CONTINUE                                                          00030200
      WRITE            ( 6,529 )                                        00030300
      WRITE (6,630)                                                     00030400
      DO 64 I=1,NXY                                                     00030500
      NE=N2(I)                                                          00030600
      NS=NE-N1(I)+1                                                     00030700
      DO 65 J=NS,NE                                                     00030800
      DIFF=Y2(J)-PRE(I)                                                 00030900
   65 WRITE            ( 6,631)I,X2(I),Y2(J),PRE(I),DIFF                00031000
   64 CONTINUE                                                          00031100
 641  CONTINUE                                                          00031200
      IF (GPLOT-D123) 66,67,66                                          00031300
   67 CALL PLOT  (ICOUNT)                                               00031400
   66 TRANS=D123                                                        00031500
 1000 CONTINUE                                                          00031600
      GO TO 100                                                         00031700
      STOP                                                              00031800
      END                                                               00031900
CANOVAR       SUBROUTINE ANOVAR FOR BMD06R         SEPTEMBER 29,1965    00032000
      SUBROUTINE ANOVAR                                                 00032100
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00032200
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00032300
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00032400
     3,N1(1000),N2(1000)                                                00032500
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00032600
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200032700
     2,RS,  AS,BS,SUMSQS,AVE                                            00032800
     3,X2,Y2,Y3,I5,I6                                                   00032900
      EQUIVALENCE(X1,PRE,U)                                             00033000
      ADD=0.0                                                           00033100
      TOT=0.0                                                           00033200
      TMEAN=0.0                                                         00033300
      YHAT=0.0                                                          00033400
C N2(NXY) CONTAINS THE TOTAL NUMBER OF DEPENDENT VARIABLE VALUES AND    00033500
C TY0 CONTAINS THE SUM OF ALL DEPENDENT VARIABLE VALUES.                00033600
C SUM, THEREFORE, CONTAINS THE GRAND MEAN.                              00033700
      DIV=N2(NXY)                                                       00033800
       SUM=TY0/DIV                                                      00033900
      DO 10 I=1,NXY                                                     00034000
      NE=N2(I)                                                          00034100
      NS=NE-N1(I)+1                                                     00034200
      YHAT=YHAT+((AVE(I)-PRE(I))**2)*FLOAT (N1(I))                      00034300
      DO 7 J=NS,NE                                                      00034400
      ADD=ADD+(Y2(J)-SUM)**2                                            00034500
      TOT=TOT+(Y2(J)-AVE(I))**2                                         00034600
    7 TMEAN=TMEAN+(Y2(J)-PRE(I))**2                                     00034700
   10 CONTINUE                                                          00034800
      WRITE (6,100)                                                     00034900
      WRITE (6,101)                                                     00035000
      WRITE (6,106)                                                     00035100
      WRITE (6,102)   ADD                                               00035200
      WRITE (6,103)TOT                                                  00035300
      WRITE (6,104)TMEAN                                                00035400
      WRITE (6,105)YHAT                                                 00035500
      WRITE (6,106)                                                     00035600
  100 FORMAT(1H020X,20HANALYSIS OF VARIANCE//)                          00035700
  101 FORMAT(11H DEVIATIONS20X,14HSUM OF SQUARES)                       00035800
  102 FORMAT(11H FROM MEAN 20X,F13.4)                                   00035900
  103 FORMAT(16H FROM X(I) MEANS15X,F13.4//)                            00036000
  104 FORMAT(11H FROM CURVE20X,F13.4)                                   00036100
  105 FORMAT(25H OF X(I) MEANS FROM CURVE6X,F13.4)                      00036200
  106 FORMAT(1H 44(1H*))                                                00036300
      RETURN                                                            00036400
      END                                                               00036500
CEXPREG       SUBROUTINE EXPREG FOR BMD06R        FEBRUARY 21, 1964     00036600
      SUBROUTINEEXPREG(G,N,X,A,B,R,P,SUM)                               00036700
      DIMENSION G(1),X(1), P(1)                                         00036800
      X1 = 0.                                                           00036900
      X2 = 0.                                                           00037000
      N2 = N-2                                                          00037100
      D2 = G(2) - G(1)                                                  00037200
      D1 = D2                                                           00037300
      D2 = G(I+2) - G(I+1)                                              00037400
      DO 10 I=1,N2                                                      00037500
      FI = G(I+1) - G(I)                                                00037600
      FJ   = G(I+2) - G(I+1)                                            00037700
      X1 = X1 + FI**2                                                   00037800
      X2 = X2 + FI*FJ                                                   00037900
      WRITE            (6,999)FI,FJ,X1,X2,G(I)                          00038000
 10   CONTINUE                                                          00038100
      R = X2/X1                                                         00038200
      IF(R) 15,30,30                                                    00038300
 15   R1 = -R                                                           00038400
      DO 20 I=1,N                                                       00038500
      T = 3.1415927*X(I)                                                00038600
      P(I) = R1**X(I)*COS (T)                                           00038700
 20   CONTINUE                                                          00038800
      GO TO 40                                                          00038900
                                                                        00039000
 30   DO 35 I = 1,N                                                     00039100
 35   P(I)= R**X(I)                                                     00039200
 40   CONTINUE                                                          00039300
      SP = 0.                                                           00039400
      SPP = 0.                                                          00039500
      SPG = 0.                                                          00039600
      SG = 0.                                                           00039700
      DO 45 I=1,N                                                       00039800
      SP = SP + P(I)                                                    00039900
      SPP = SPP + P(I)**2                                               00040000
      SPG = SPG + P(I)*G(I)                                             00040100
      SG = SG + G(I)                                                    00040200
 45   CONTINUE                                                          00040300
      XN = N                                                            00040400
      DET = XN*SPP - SP**2                                              00040500
      X1 = SPP/DET                                                      00040600
      X2 = -SP/DET                                                      00040700
      X3 = X2                                                           00040800
      X4 = XN/DET                                                       00040900
      A = X1*SG + X2*SPG                                                00041000
      B = X3*SG + X4*SPG                                                00041100
      SUM = 0.                                                          00041200
      DO 50 I=1,N                                                       00041300
      SUM = SUM + (G(I)-A-B*P(I))**2                                    00041400
 50   CONTINUE                                                          00041500
      RETURN                                                            00041600
 999  FORMAT(3X,6F15.4)                                                 00041700
      END                                                               00041800
CFINDR      SUBROUTINE FINDR FOR BMD06R                           9-6-6300041900
      SUBROUTINE FINDR                                                  00042000
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00042100
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00042200
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00042300
     3,N1(1000),N2(1000)                                                00042400
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00042500
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200042600
     2,RS,  AS,BS,SUMSQS,AVE                                            00042700
     3,X2,Y2,Y3,I5,I6                                                   00042800
      EQUIVALENCE(X1,PRE,U)                                             00042900
      DIMENSIONCF(9)                                                    00043000
      DO 1 I=1,NXY                                                      00043100
      SUM=0.0                                                           00043200
      NE=N2(I)                                                          00043300
      NS=NE-N1(I)+1                                                     00043400
      DO 2 J=NS,NE                                                      00043500
 2    SUM = SUM + Y2(J)                                                 00043600
      AVE(I) = SUM/FLOAT (N1(I))                                        00043700
 1    CONTINUE                                                          00043800
      CALL REGRES( X2, AVE,NXY, CF)                                     00043900
      IF(ABS (CF(2))-.01)50,50,55                                       00044000
 50   R0 = .9999                                                        00044100
      GO TO 100                                                         00044200
 55   CONTINUE                                                          00044300
      IF(CF(5))60,65,60                                                 00044400
 65   CONTINUE                                                          00044500
      GO TO 50                                                          00044600
 60   CONTINUE                                                          00044700
      R0 = (CF(4)*(X2(2)-X2(NXY))+CF(5)*(X2(2)**2 -X2(NXY)**2))/        00044800
     1(CF(4)*(X2(1)-X2(NXY-1))+CF(5)*(X2(1)**2-X2(NXY-1)**2))           00044900
      IF(R0)50, 50,100                                                  00045000
 100  RETURN                                                            00045100
      END                                                               00045200
CINVERT      SUBROUTINE INVERT FOR BMD06R         FEBRUARY 21, 1964     00045300
      SUBROUTINE INVERT(A,N)                                            00045400
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00045500
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00045600
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00045700
     3,N1(1000),N2(1000)                                                00045800
      DIMENSION L(3),M(3),A(3,3)                                        00045900
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00046000
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200046100
     2,RS,  AS,BS,SUMSQS,AVE                                            00046200
     3,X2,Y2,Y3,I5,I6                                                   00046300
      EQUIVALENCE(X1,PRE,U)                                             00046400
      D=1.0                                                             00046500
      DO80 K=1,N                                                        00046600
      L(K)=K                                                            00046700
      M(K)=K                                                            00046800
      BIGA=A(K,K)                                                       00046900
      DO20 I=K,N                                                        00047000
      DO20 J=K,N                                                        00047100
      IF(ABS (BIGA)-ABS (A(I,J))) 10,20,20                              00047200
   10 BIGA=A(I,J)                                                       00047300
      L(K)=I                                                            00047400
      M(K)=J                                                            00047500
   20 CONTINUE                                                          00047600
C     INTERCHANGE ROWS                                                  00047700
      J=L(K)                                                            00047800
      IF(L(K)-K) 35,35,25                                               00047900
   25 DO30 I=1,N                                                        00048000
      HOLD=-A(K,I)                                                      00048100
      A(K,I)=A(J,I)                                                     00048200
   30 A(J,I)=HOLD                                                       00048300
C     INTERCHANGE COLUMNS                                               00048400
   35 I=M(K)                                                            00048500
      IF(M(K)-K) 45,45,37                                               00048600
   37 DO40 J=1,N                                                        00048700
      HOLD=-A(J,K)                                                      00048800
      A(J,K)=A(J,I)                                                     00048900
   40 A(J,I)=HOLD                                                       00049000
C     DIVIDE COLUMN BY MINUS PIVOT                                      00049100
   45 DO55 I=1,N                                                        00049200
   46 IF(I-K)50,55,50                                                   00049300
   50 A(I,K)=A(I,K)/(-A(K,K))                                           00049400
   55 CONTINUE                                                          00049500
C     REDUCE MATRIX                                                     00049600
      DO65 I=1,N                                                        00049700
      DO65 J=1,N                                                        00049800
   56 IF(I-K) 57,65,57                                                  00049900
   57 IF(J-K) 60,65,60                                                  00050000
   60 A(I,J)=A(I,K)*A(K,J)+A(I,J)                                       00050100
   65 CONTINUE                                                          00050200
C     DIVIDE ROW BY PIVOT                                               00050300
      DO75 J=1,N                                                        00050400
   68 IF(J-K)70,75,70                                                   00050500
   70 A(K,J)=A(K,J)/A(K,K)                                              00050600
   75 CONTINUE                                                          00050700
C     CONTINUED PRODUCT OF PIVOTS                                       00050800
      D=D*A(K,K)                                                        00050900
C     REPLACE PIVOT BY RECIPROCAL                                       00051000
      A(K,K)=1.0/A(K,K)                                                 00051100
   80 CONTINUE                                                          00051200
C     FINAL ROW AND COLUMN INTERCHANGE                                  00051300
      K=N                                                               00051400
  100 K=(K-1)                                                           00051500
      IF(K) 150,150,103                                                 00051600
  103 I=L(K)                                                            00051700
      IF(I-K) 120,120,105                                               00051800
  105 DO110 J=1,N                                                       00051900
      HOLD=A(J,K)                                                       00052000
      A(J,K)=-A(J,I)                                                    00052100
  110 A(J,I)=HOLD                                                       00052200
  120 J=M(K)                                                            00052300
      IF(J-K) 100,100,125                                               00052400
  125 DO130 I=1,N                                                       00052500
      HOLD=A(K,I)                                                       00052600
      A(K,I)=-A(J,I)                                                    00052700
  130 A(J,I)=HOLD                                                       00052800
      GO TO 100                                                         00052900
 150  CONTINUE                                                          00053000
      IF(D) 160,999,160                                                 00053100
 999  WRITE           (6,995)                                           00053200
 995  FORMAT(49H ERROR IN ATTEMPTING TO INVERT A SINGULAR MATRIX. )     00053300
 160  RETURN                                                            00053400
      END                                                               00053500
CPLOT         SUBROUTINE  PLOT FOR BMD06R             JULY 22, 1964     00053600
      SUBROUTINE PLOT (ICOUNT)                                          00053700
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00053800
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00053900
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00054000
     3,N1(1000),N2(1000) ,XY(51,101)                                    00054100
       DIMENSION FMT(120)                                               00054200
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00054300
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200054400
     2,RS,  AS,BS,SUMSQS,AVE                                            00054500
     3,X2,Y2,Y3,I5,I6                                                   00054600
     4,XR,YR,XMAX,XY,NCC,JX,XIJ,TC,TP,JY,YIJ,YMIN,IC,YMAX               00054700
      EQUIVALENCE(X1,PRE,U)                                             00054800
      A=AS(ICOUNT)                                                      00054900
      B=BS(ICOUNT)                                                      00055000
      R=RS(ICOUNT+1)                                                    00055100
      SS=10.0**10                                                       00055200
      FF=-10.0**10                                                      00055300
      XS=SS                                                             00055400
      XL=FF                                                             00055500
      DO 300 I=1,NXY                                                    00055600
      XS=AMIN1(XS,X2(I))                                                00055700
      XL=AMAX1(XL,X2(I))                                                00055800
      SS=AMIN1(SS,AVE(I),PRE(I))                                        00055900
  300 FF=AMAX1(FF,AVE(I),PRE(I))                                        00056000
      WRITE (6,100)                                                     00056100
      WRITE (6,201)                                                     00056200
C     MXY AND LYNN ARE DUMMY VARIABLES FOR SUBROUTINE SCALE.            00056300
      CALL SCALE(SS,FF,100.0,MXY,S1,F1,LYNN,FF)                         00056400
      IF(B)210,211,211                                                  00056500
  210 WRITE (6,204)                                                     00056600
      GO TO 220                                                         00056700
  211 WRITE            ( 6,215)                                         00056800
  220 WRITE            ( 6,218)S1,F1                                    00056900
      MXY=NXY                                                           00057000
      LYNN=0                                                            00057100
  303 IF(MXY-50) 301,302,302                                            00057200
  301 LYNN=LYNN+1                                                       00057300
      MXY=LYNN*MXY                                                      00057400
      GO TO 303                                                         00057500
      DIMENSION SYM(15),YYY(15)                                         00057600
  302 SYM(1)=(+6HA00000)                                                00057700
      SYM(2)=(+6HP00000)                                                00057800
      DO 50 I=1,NXY                                                     00057900
      XX=X2(I)                                                          00058000
      YYY(2)=PRE(I)                                                     00058100
      YYY(1)=AVE(I)                                                     00058200
      CALL PLOTR(XX,XS,XL,YYY,SYM,SS,FF,2,-1,FF,XL)                     00058300
      IF(LYNN) 50,50,304                                                00058400
  304 DO 305 J=1,LYNN                                                   00058500
  305 WRITE     (6,101)                                                 00058600
   50 CONTINUE                                                          00058700
      CALL PLOTR(XX,XS,XL,YYY,SYM,SS,FF,-1,-1,FF,XL)                    00058800
  100 FORMAT(12H1GRAPH CODES//13H A=AVERAGED Y/14H P=PREDICTED Y)       00058900
  101 FORMAT(1H )                                                       00059000
  201 FORMAT(48H0X AND Y VALUES ARE PLOTTED IN TRANSFORMED UNITS)       00059100
  204 FORMAT(19H ASYMPTOTE AT RIGHT)                                    00059200
  215 FORMAT(18H ASYMPTOTE AT LEFT)                                     00059300
  218 FORMAT(26H GRAPH SCALE EXTENDS FROM F10.4,4H TO F10.4)            00059400
  403 RETURN                                                            00059500
      END                                                               00059600
CPLOTR        SUBROUTINE PLOTR                        JULY 20, 1964     00059700
      SUBROUTINE PLOTR(X,ZMIN,ZMAX,Y,SYM,WMIN,WMAX,NC,NP,YRMA,XRMA)     00059800
      DIMENSION XY(51,101),Y(15),CLAB(12),XM(6),SYM(15),GF(10),F2T(10)  00059900
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00060000
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00060100
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00060200
     3,N1(1000),N2(1000)                                                00060300
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00060400
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200060500
     2,RS,  AS,BS,SUMSQS,AVE                                            00060600
     3,X2,Y2,Y3,I5,I6                                                   00060700
     4,XR,YR,XMAX,XY,NCC,JX,XIJ,TC,TP,JY,YIJ,YMIN,IC,YMAX               00060800
      EQUIVALENCE(X1,PRE,U)                                             00060900
C                                                                       00061000
C     PLOTR WAS MODIFIED THIS DATE TO GIVE BETTER SCALES.               00061100
C                                                                       00061200
C                                                                       00061300
 100   FORMAT(1H 6X5(F12.3,8X),F12.3/17X,5(F12.3,8X))                   00061400
  101 FORMAT(1H F12.3,1X,A1,101A1,A1)                                   00061500
  102 FORMAT(1H 13X,A1,101A1,A1)                                        00061600
 1000 FORMAT(1H  14X,101A1)                                             00061700
 1001 FORMAT(15X,20(5H+....),1H+)                                       00061800
      BLANKS=(+6H      )                                                00061900
      BLANK=(+1H )                                                      00062000
C      XM(1)=770000000000                                               00062100
C        XM(2)=007700000000                                             00062200
C       XM(3)=000077000000                                              00062300
C        XM(4)=000000770000                                             00062400
C       XM(5)=000000007700                                              00062500
C       XM(6)=000000000077                                              00062600
      GF(1)=(+6H1X    )                                                 00062700
      GF(2)=(+6H2X    )                                                 00062800
      GF(3)=(+6H3X    )                                                 00062900
      GF(4)=(+6H4X    )                                                 00063000
      GF(5)=(+6H5X    )                                                 00063100
      GF(6)=(+6H6X    )                                                 00063200
      GF(7)=(+6H7X    )                                                 00063300
      GF(8)=(+6H8X    )                                                 00063400
      GF(9)=(+6H9X    )                                                 00063500
      GF(10)=(+6H10X   )                                                00063600
      F2T(1)=(+6H (17X )                                                00063700
      F2T(2)=BLANKS                                                     00063800
      F2T(3)=BLANKS                                                     00063900
      F2T(4)=(+6H4(F12.)                                                00064000
      F2T(5)=(+6H3,8X),)                                                00064100
      F2T(6)=(+6HF12.3 )                                                00064200
      F2T(7)=(+6H/7X   )                                                00064300
      F2T(8)=BLANKS                                                     00064400
      F2T(9)=(+6H5(F12.)                                                00064500
      F2T(10)=(+6H3,8X)))                                               00064600
      IF (JY.EQ.0) GO TO 3002                                           00064700
      IF (JY.LT.11 .AND. JY.GT.0) GO TO 3000                            00064800
      JY=1                                                              00064900
 3000 F2T(3)=GF(JY)                                                     00065000
      F2T(8)=GF(JY)                                                     00065100
 3002 IF(NCC)48,50,48                                                   00065200
   50 KL=0                                                              00065300
      TC=(+1H.)                                                         00065400
      TP=(+1H+)                                                         00065500
      CALL SCALE(WMIN,WMAX,100.0,JY,YMIN,YMAX,YIJ,YRMA)                 00065600
      YR=YMAX-YMIN                                                      00065700
  230 J=JY                                                              00065800
      IF(J*(J-10))204,201,201                                           00065900
  201 IF(KL)220,220,231                                                 00066000
  231 WRITE            (6,1001  )                                       00066100
      IF(KL)250,250,220                                                 00066200
  220 CLAB(1)= YMIN                                                     00066300
      DO 222 I=2,11                                                     00066400
  222 CLAB(I)=CLAB(I-1)+YIJ                                             00066500
      WRITE            (6,100)(CLAB(I),I=1,11,2),(CLAB(K),K=2,10,2)     00066600
      IF(KL)231,231,14                                                  00066700
  204 IF(J-5)205,221,207                                                00066800
  207 J=J-5                                                             00066900
  205 JYT=5-J                                                           00067000
  221 CONTINUE                                                          00067100
      IF(KL)226,226,227                                                 00067200
  226 F2T(3)=GF(JY)                                                     00067300
  225 F2T(8)=GF(JY)                                                     00067400
      TT=JY                                                             00067500
      TT=TT*YIJ/10.                                                     00067600
      CLAB(1)= YMIN+TT                                                  00067700
      DO 223 I=2,10                                                     00067800
  223 CLAB(I)=CLAB(I-1) +YIJ                                            00067900
      WRITE            (6,F2T)(CLAB(I),I=2,10,2),(CLAB(K),K=1,9 ,2)     00068000
      IF(KL)227,227,14                                                  00068100
  227 IF(JY-5)208,209,208                                               00068200
  209 WRITE            (6,1001 )                                        00068300
      IF(KL)250,250,226                                                 00068400
  208 WRITE            (6,1000)(TC,I=1,J ),((TP,(TC,I=1,4)),K=1,19),TP,(00068500
     1 TC,I=1,JYT)                                                      00068600
      IF(KL)250,250,226                                                 00068700
  250 CONTINUE                                                          00068800
      NCC=1                                                             00068900
      IC=0                                                              00069000
      IF(NP)80,11,11                                                    00069100
   11 DO 1 I=1,51                                                       00069200
       DO 1 J=1,101                                                     00069300
   1  XY(I,J)=BLANK                                                     00069400
      CALL SCALE (ZMIN,ZMAX,50.,JX,XMIN,XMAX,XIJ,XRMA)                  00069500
      XR=XMAX-XMIN                                                      00069600
   48 IF(NC)52,13,49                                                    00069700
   49 IF(NP)80,10,10                                                    00069800
   10 DO 9 N=1,NC                                                       00069900
      SYMB=SYM(N)                                                       00070000
      XDIFFR=XMAX-X                                                     00070100
      IF(XDIFFR)105,106,106                                             00070200
  105 XDIFFR=0.0                                                        00070300
  106 YDIFFR=YMAX-Y(N)                                                  00070400
      IF(YDIFFR)107,108,108                                             00070500
  107 YDIFFR=0.0                                                        00070600
  108 L=51.-(50.*XDIFFR)/XR+.5                                          00070700
      K=101.-(100.*YDIFFR)/YR+.5                                        00070800
C     CALL FORM2(T,M,SYMB)                                              00070900
      XY(L,K)=SYM(N)                                                    00071000
 9     CONTINUE                                                         00071100
      GO TO 15                                                          00071200
   80 DO 86 I=1,101                                                     00071300
   86 XY(1,I)=BLANK                                                     00071400
       L=1                                                              00071500
      DO 95 N=1,NC                                                      00071600
      SYMB=SYM(N)                                                       00071700
      YDIFFR=YMAX-Y(N)                                                  00071800
      IF(YDIFFR)860,865,865                                             00071900
  860 YDIFFR=0.0                                                        00072000
  865 K=101.-(100.*YDIFFR)/YR+.5                                        00072100
C      CALL FORM2(T,M,SYMB)                                             00072200
      IF (XY(L,K).EQ.BLANK) GO TO 3005                                  00072300
      XY(L,K)=(+1HB)                                                    00072400
      GO TO 95                                                          00072500
 3005 XY(L,K)=SYM(N)                                                    00072600
   95 CONTINUE                                                          00072700
      IF( MOD (IC,5))97,96,97                                           00072800
   96 W=TP                                                              00072900
      GO TO 98                                                          00073000
   97 W=TC                                                              00073100
   98 WRITE            (6,101)X,W,(XY(1,N),N=1,101),W                   00073200
      IC=IC+1                                                           00073300
      GO TO 15                                                          00073400
   13 M=6-JX                                                            00073500
      LL=50+M                                                           00073600
      T=JX                                                              00073700
      IF(5-JX)131,131,135                                               00073800
  131 T=0.0                                                             00073900
  135 RLAB=XMAX-(T*XIJ)/5.0                                             00074000
      W=TC                                                              00074100
      K=52                                                              00074200
      DO 31 L=M,LL                                                      00074300
      K=K-1                                                             00074400
      I= MOD (L,5)                                                      00074500
      IF(I-1)2,3,2                                                      00074600
    3 W=TP                                                              00074700
      WRITE            (6,101)RLAB,W,(XY(K,N),N=1,101),W                00074800
      RLAB=RLAB-XIJ                                                     00074900
      W=TC                                                              00075000
      GO TO 31                                                          00075100
    2 WRITE            (6,102)W,(XY(K,N),N=1,101),W                     00075200
   31 CONTINUE                                                          00075300
   52 KL=1                                                              00075400
      GO TO 230                                                         00075500
   14 NCC=0                                                             00075600
   15 RETURN                                                            00075700
            END                                                         00075800
CREAD         SUBROUTINE READ FOR BMD06R               MAY 13, 1964     00075900
      SUBROUTINE READ                                                   00076000
      DIMENSION FMT(120),Z(1000)                                        00076100
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00076200
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00076300
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00076400
     3,N1(1000),N2(1000)                                                00076500
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00076600
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200076700
     2,RS,  AS,BS,SUMSQS,AVE                                            00076800
     3,X2,Y2,Y3,I5,I6                                                   00076900
      EQUIVALENCE (X1,PRE,U),(IA1,A1)                                   00077000
      A1=(+6HSAMSIZ)                                                    00077100
      G123=(+2HXY)                                                      00077200
      NS=0                                                              00077300
      NCARDS=(NXY+21)/22                                                00077400
      L2=0                                                              00077500
      DO 50 J=1,NCARDS                                                  00077600
      L1=L2+1                                                           00077700
      L2=L2+22                                                          00077800
      IF(NXY-L2)35,36,36                                                00077900
 35   L2=NXY                                                            00078000
 36   READ           (5,103)ISAM,(N1(I),I=L1,L2)                        00078100
      IF(ISAM-IA1)40,50,40                                              00078200
 40   WRITE            (6, 4000)A1                                      00078300
      NXY=-22                                                           00078400
 50   CONTINUE                                                          00078500
      IF(NXY)55,55,56                                                   00078600
 55   CALL EXIT                                                         00078700
 56   NTOT=0                                                            00078800
      DO 70 I=1,NXY                                                     00078900
      IF(N1(I))80,80,75                                                 00079000
 80   N1(I)= 1                                                          00079100
 75   NTOT=NTOT+N1(I)                                                   00079200
 70   CONTINUE                                                          00079300
      IF(NTOT-5000)4,4,300                                              00079400
    4 KVT=KVT*12                                                        00079500
      READ            (5,101)(FMT(I),I=1,KVT)                           00079600
      JUMP=1                                                            00079700
   5  IF (TXY-G123) 6,7,6                                               00079800
    6 JUMP=2                                                            00079900
    7 DO 25 I=1,NXY                                                     00080000
      NY=N1(I)                                                          00080100
      GO TO (9,11), JUMP                                                00080200
    9 READ           (I5,FMT)D,(Z(J),J=1,NY)                            00080300
      X1(I)=D                                                           00080400
      DO 19 J=1,NY                                                      00080500
   16 NS=NS+1                                                           00080600
      Y2(NS)=Z(J)                                                       00080700
   19 CONTINUE                                                          00080800
      GO TO 25                                                          00080900
   11 READ           ( 5,FMT)(Z(J),J=1,NY),D                            00081000
      X1(I)=D                                                           00081100
      DO 24 J=1,NY                                                      00081200
   26 NS=NS+1                                                           00081300
      Y2(NS)=Z(J)                                                       00081400
   24 CONTINUE                                                          00081500
   25 CONTINUE                                                          00081600
  101 FORMAT(12A6)                                                      00081700
  102 FORMAT(12F6.0)                                                    00081800
 103  FORMAT(A6,22I3)                                                   00081900
 4000 FORMAT(1H0,A6,33H CARD MISPUNCHED OR OUT OF ORDER.)               00082000
 4010 FORMAT(84H0TOTAL NUMBER OF VALUES FOR DEPENDENT VARIABLE EXCEEDS 500082100
     1000. PROGRAM CANNOT PROCEED.)                                     00082200
      RETURN                                                            00082300
 300  WRITE            (6, 4010)                                        00082400
      GO TO 55                                                          00082500
      END                                                               00082600
CREGRES       SUBROUTINE REGRES  FOR BMD06R                             00082700
      SUBROUTINE REGRES (X,  F,  N,  C)                                 00082800
      DIMENSIONA(9),B(3),C(9),F(1),X(1)                                 00082900
      DO 1 I=1,9                                                        00083000
 1    A(I)=0.                                                           00083100
      B(1) = 0.                                                         00083200
      B(2) = 0.                                                         00083300
      B(3) = 0.                                                         00083400
      DO 100 I=1,N                                                      00083500
      X2 = X(I)**2                                                      00083600
      A(2) = A(2)+X(I)                                                  00083700
      A(3) = A(3) +X2                                                   00083800
      A(6) = A(6) + X2*X(I)                                             00083900
      A(9) = A(9) + X2**2                                               00084000
      B(1) = B(1) + F(I)                                                00084100
      B(2) = B(2) +F(I)*X(I)                                            00084200
      B(3) = B(3)+ F(I)*X2                                              00084300
 100  CONTINUE                                                          00084400
      A(1) = N                                                          00084500
      A(4) = A(2)                                                       00084600
      A(5) = A(3)                                                       00084700
      A(7) = A(3)                                                       00084800
      A(8) = A(6)                                                       00084900
      D = A(1)*A(5)-A(2)**2                                             00085000
      C(1) = (A(5)*B(1)-A(2)*B(2))/D                                    00085100
      C(2) = (-A(2)*B(1)+A(1)*B(2))/D                                   00085200
      CALL INVERT(A,3)                                                  00085300
      C(3) = A(1)*B(1)+A(2)*B(2) +A(3)*B(3)                             00085400
      C(4) = A(4) *B(1) + A(5)*B(2) + A(6)*B(3)                         00085500
      C(5) = A(7) *B(1) + A(8)*B(2) + A(9)*B(3)                         00085600
      RETURN                                                            00085700
 99   FORMAT(3X,3F20.5)                                                 00085800
      END                                                               00085900
CSETUP   SUBROUTINE SETUP FOR BMD06R               SEPTEMBER 23,1965    00086000
      SUBROUTINE SETUP                                                  00086100
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00086200
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00086300
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00086400
     3,N1(1000),N2(1000)                                                00086500
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00086600
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200086700
     2,RS,  AS,BS,SUMSQS,AVE                                            00086800
     3,X2,Y2,Y3,I5,I6                                                   00086900
      EQUIVALENCE(X1,PRE,U)                                             00087000
      V=ALOG(R)                                                         00087100
      DO 10 I=1,3                                                       00087200
      DO 10 J=1,3                                                       00087300
   10 C(I,J)=0.0                                                        00087400
      TY1=0.0                                                           00087500
      TY2=0.0                                                           00087600
      DO 20 I=1,NXY                                                     00087700
      FN=N1(I)                                                          00087800
      C(1,1)=C(1,1)+FN                                                  00087900
      U(I)=EXP (X2(I)*V)                                                00088000
      U3=X2(I)*U(I)/R                                                   00088100
      U4=U(I)**2                                                        00088200
      U5=X2(I)*U4/R                                                     00088300
      U6=X2(I)*U5/R                                                     00088400
      C(1,2)=C(1,2)+(U(I)*FN)                                           00088500
      C(1,3)=C(1,3)+(U3*FN)                                             00088600
      C(2,2)=C(2,2)+(U4*FN)                                             00088700
      C(2,3)=C(2,3)+(U5*FN)                                             00088800
      C(3,3)=C(3,3)+(U6*FN)                                             00088900
      TY1=TY1+U(I)*AVE(I)*FN                                            00089000
   15 TY2=TY2+U3*AVE(I)*FN                                              00089100
   20 CONTINUE                                                          00089200
      C(3,1)=C(1,3)                                                     00089300
      C(3,2)=C(2,3)                                                     00089400
      C(2,1)=C(1,2)                                                     00089500
      Y012(2)=TY1                                                       00089600
      Y012(3)=TY2                                                       00089700
      DO 30 I=1,3                                                       00089800
      DO 30 J=1,3                                                       00089900
   30 C1(I,J)=C(I,J)                                                    00090000
      RETURN                                                            00090100
      END                                                               00090200
CSORTX      SUBROUTINE SORTX FOR BMD06R                           9-6-6300090300
      SUBROUTINE SORTX                                                  00090400
      DIMENSION  N3(1000),N4(1000)       ,N1(1000),N2(1000)             00090500
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00090600
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00090700
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00090800
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00090900
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200091000
     2,RS,  AS,BS,SUMSQS,AVE                                            00091100
     3,X2,Y2,Y3,I5,I6                                                   00091200
      EQUIVALENCE(X1,PRE,U)                                             00091300
      NF=0                                                              00091400
      DO 60 I=1,NXY                                                     00091500
   33 N3(I)=I                                                           00091600
   60 N4(I)=N1(I)                                                       00091700
      DO 10 I=1,NXY                                                     00091800
      DO 5 J=I,NXY                                                      00091900
      IF(X2(I)-X2(J)) 5,5,4                                             00092000
    4 XD=X2(I)                                                          00092100
      X2(I)=X2(J)                                                       00092200
      X2(J)=XD                                                          00092300
      K1=N3(I)                                                          00092400
      N3(I)=N3(J)                                                       00092500
      N3(J)=K1                                                          00092600
      K2=N1(I)                                                          00092700
      N1(I)=N1(J)                                                       00092800
      N1(J)=K2                                                          00092900
    5 CONTINUE                                                          00093000
   10 CONTINUE                                                          00093100
      DO 6 I=1,NXY                                                      00093200
      K1=N3(I)                                                          00093300
      NE=N2(K1)                                                         00093400
      NS=NE-N4(K1)+1                                                    00093500
      DO 3 K=NS,NE                                                      00093600
      NF=NF+1                                                           00093700
    3 Y3(NF)=Y2(K)                                                      00093800
    6 CONTINUE                                                          00093900
      DO 20 I=1,NXY                                                     00094000
   20 N2(I)=0                                                           00094100
      N2(1)=N1(1)                                                       00094200
      DO 30 I=2,NXY                                                     00094300
   30 N2(I)=N2(I-1)+N1(I)                                               00094400
      RETURN                                                            00094500
      END                                                               00094600
CTRANX        SUBROUTINE TRANX FOR BMD06R              MAY 13, 1964     00094700
      SUBROUTINE TRANX                                                  00094800
      ASNF(XX)=ATAN (XX/(SQRT (1.0-XX**2)))                             00094900
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00095000
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00095100
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00095200
     3,N1(1000),N2(1000)                                                00095300
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00095400
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200095500
     2,RS,  AS,BS,SUMSQS,AVE                                            00095600
     3,X2,Y2,Y3,I5,I6                                                   00095700
      EQUIVALENCE(X1,PRE,U)                                             00095800
      LTRANX=LTRANX-1                                                   00095900
      KTRANX=LTRANX                                                     00096000
      IF(LTRANX*(LTRANX-11))5,150,175                                   00096100
    5 DO 100 I=1,NXY                                                    00096200
      D=X1(I)                                                           00096300
      GO TO (10,20,30,40,50,60,70,80,90,200,170),LTRANX                 00096400
 10   IF(D)160,11,12                                                    00096500
 11   X2(I)=0.0                                                         00096600
      GO TO 100                                                         00096700
 12   X2(I)=SQRT (D)                                                    00096800
      GO TO 100                                                         00096900
 20   IF(D)160,21,22                                                    00097000
 21   X2(I)=1.0                                                         00097100
      GO TO 100                                                         00097200
 22   X2(I)=SQRT (D)+SQRT (D+1.0)                                       00097300
      GO TO 100                                                         00097400
 30   IF(D)160,160,31                                                   00097500
 31   X2(I)=ALOG10(D)                                                   00097600
      GO TO 100                                                         00097700
   40 X2(I)=EXP (D)                                                     00097800
      GO TO 100                                                         00097900
 50   IF(D)160,11,51                                                    00098000
 51   IF(D-1.0)52,53,160                                                00098100
 52   A=SQRT (D)                                                        00098200
      X2(I)=ASNF(A)                                                     00098300
      GO TO 100                                                         00098400
 53   X2(I)=1.57079633                                                  00098500
      GO TO 100                                                         00098600
   60 FN=NXY                                                            00098700
      A=D/(FN+1.0)                                                      00098800
      B=A+1.0/(FN+1.0)                                                  00098900
      IF(A)160,61,62                                                    00099000
 61   IF(B)160,11,64                                                    00099100
 62   IF(B)160,65,66                                                    00099200
 64   X2(I)=ASNF(SQRT (B))                                              00099300
      GO TO 100                                                         00099400
 65   X2(I)=ASNF(SQRT (A))                                              00099500
      GO TO 100                                                         00099600
 66   X2(I)=ASNF(SQRT (A))+ASNF(SQRT (B))                               00099700
      GO TO 100                                                         00099800
 70   IF(D)71,160,71                                                    00099900
 71   X2(I)=1.0/D                                                       00100000
      GO TO 100                                                         00100100
   80 X2(I)=D+CONSX                                                     00100200
      GO TO 100                                                         00100300
   90 X2(I)=D*CONSX                                                     00100400
      GO TO 100                                                         00100500
 200  IF(D)210,11,210                                                   00100600
 210  X2(I)=D**CONSX                                                    00100700
      GO TO 100                                                         00100800
 170  IF(D)155,155,171                                                  00100900
 171  X2(I) = ALOG(D)                                                   00101000
      GO TO 100                                                         00101100
 150  WRITE            (6, 4000)                                        00101200
 4000 FORMAT(108H0ILLEGAL TRANSGENERATION CODE FOR THE INDEPENDENT VARIA00101300
     1BLE. PROGRAM WILL PROCEED LEAVING VARIABLE UNCHANGED.)            00101400
      X2(I)=D                                                           00101500
      GO TO 100                                                         00101600
 155  KTRANX=17                                                         00101700
 160  WRITE            (6,4010)I,KTRANX                                 00101800
      X2(I)=D                                                           00101900
      GO TO 100                                                         00102000
 4010 FORMAT(4H0THEI4,94HTH. VALUE OF THE INDEPENDENT VARIABLE VIOLATES 00102100
     1THE RESTRICTION FOR TRANSGENERATION OF THE TYPEI3,1H./52H THE PROG00102200
     2RAM CONTINUES LEAVING THIS VALUE UNCHANGED.)                      00102300
 175  IF(LTRANX-17)150,176,150                                          00102400
 176  LTRANX=11                                                         00102500
      GO TO 5                                                           00102600
  100 CONTINUE                                                          00102700
      RETURN                                                            00102800
      END                                                               00102900
CTRANSY       SUBROUTINE TRANSY FOR BMD06R             MAY 13, 1964     00103000
      SUBROUTINE TRANSY                                                 00103100
      ASNF(XX)=ATAN (XX/(SQRT (1.0-XX**2)))                             00103200
      DIMENSION X1(1000),X2(1000),         Y2(5000),                NTL(00103300
     110),CONT(10),Y3(5000),C(3,3),C1(3,3),Y012(3),RS(51),U(1000),AS(50)00103400
     2,BS(50),SUMSQS(51),AVE(1000),PRE(1000)                            00103500
     3,N1(1000),N2(1000)                                                00103600
      COMMON NXY,TXY,NY,D123,        TRANS,LTRANX,CONST,CONSX,KVT,SWITCH00103700
     1,X1,Y1,N1,NS,N2,NTL,CONT,IPASS,IX,IY,YV,R0,C,C1,TY0,TY1,TY2,R,Y01200103800
     2,RS,  AS,BS,SUMSQS,AVE                                            00103900
     3,X2,Y2,Y3,I5,I6                                                   00104000
      EQUIVALENCE(X1,PRE,U)                                             00104100
      C123=(+6HSPECTG)                                                  00104200
      READ           (5,1101)TODE,NT,(NTL(I),CONT(I),I=1,NT)            00104300
      IF (C123-TODE) 203,300,203                                        00104400
  300 WRITE            ( 6,1102)                                        00104500
      WRITE (6,1103)                                                    00104600
      DO 114 I=1,NT                                                     00104700
  114 WRITE            ( 6,1104)NTL(I),CONT(I),I                        00104800
    9 DO 99 I=1,NT                                                      00104900
      JUMP=NTL(I)+1                                                     00105000
      FMULY=CONT(I)                                                     00105100
      IF(JUMP*(JUMP-19))200,203,203                                     00105200
 200  IF(11-JUMP)204,201,201                                            00105300
 204  IF(JUMP-18)203,202,203                                            00105400
 202  JUMP=12                                                           00105500
  201 DO 98 J=1,NXY                                                     00105600
      NE=N2(J)                                                          00105700
      NS=NE-N1(J)+1                                                     00105800
      GO TO(99,10,20,30,40,50,60,70,80,90,100,110),JUMP                 00105900
   10 DO 11 K=NS,NE                                                     00106000
      IF(Y2(K))88,13,14                                                 00106100
   13 Y2(K)=0.0                                                         00106200
      GO TO 11                                                          00106300
   14 Y2(K)=SQRT (Y2(K))                                                00106400
   11 CONTINUE                                                          00106500
      GO TO 98                                                          00106600
   20 DO 21 K=NS,NE                                                     00106700
      IF(Y2(K))88,22,23                                                 00106800
   22 Y2(K)=1.0                                                         00106900
      GO TO 21                                                          00107000
   23 Y2(K)=SQRT (Y2(K))+SQRT (Y2(K)+1.0)                               00107100
   21 CONTINUE                                                          00107200
      GO TO 98                                                          00107300
   30 DO 31 K=NS,NE                                                     00107400
      IF(Y2(K))88,88,31                                                 00107500
 31   Y2(K)=ALOG10(Y2(K))                                               00107600
      GO TO 98                                                          00107700
   40 DO 41 K=NS,NE                                                     00107800
   41 Y2(K)=EXP (Y2(K))                                                 00107900
      GO TO 98                                                          00108000
   50 DO 51 K=NS,NE                                                     00108100
      IF(Y2(K))88,52,53                                                 00108200
   52 Y2(K)=0.0                                                         00108300
      GO TO 51                                                          00108400
   53 IF(Y2(K)-1.0)54,55,88                                             00108500
   54 A=SQRT (Y2(K))                                                    00108600
      Y2(K)=ASNF(A)                                                     00108700
      GO TO 51                                                          00108800
   55 Y2(K)=3.14159265/2.0                                              00108900
   51 CONTINUE                                                          00109000
      GO TO 98                                                          00109100
   60 DIV=N1(J)                                                         00109200
      DO 61 K=NS,NE                                                     00109300
      A=Y2(K)/(DIV+1.0)                                                 00109400
      B=A+1.0/(DIV+1.0)                                                 00109500
      IF(A)88,62,63                                                     00109600
   62 IF(B)88,64,65                                                     00109700
   64 Y2(K)=0.0                                                         00109800
      GO TO 61                                                          00109900
   65 Y2(K)=ASNF(SQRT (B))                                              00110000
      GO TO 61                                                          00110100
   63 IF(B)88,66,67                                                     00110200
   66 Y2(K)=ASNF(SQRT (A))                                              00110300
      GO TO 61                                                          00110400
   67 Y2(K)=ASNF(SQRT (A))+ASNF(SQRT (B))                               00110500
   61 CONTINUE                                                          00110600
      GO TO 98                                                          00110700
   70 DO 71 K=NS,NE                                                     00110800
      IF(Y2(K)) 71,88,71                                                00110900
   71 Y2(K)=1.0/Y2(K)                                                   00111000
      GO TO 98                                                          00111100
   80 DO 81 K=NS,NE                                                     00111200
   81 Y2(K)=Y2(K)+FMULY                                                 00111300
      GO TO 98                                                          00111400
   90 DO 91 K=NS,NE                                                     00111500
   91 Y2(K)=Y2(K)*FMULY                                                 00111600
      GO TO 98                                                          00111700
  100 DO 101 K=NS,NE                                                    00111800
      IF(Y2(K))102,103,102                                              00111900
  103 Y2(K)=0.0                                                         00112000
      GO TO 101                                                         00112100
  102 Y2(K)=Y2(K)**FMULY                                                00112200
  101 CONTINUE                                                          00112300
      GO TO 98                                                          00112400
  110 DO 111 K=NS,NE                                                    00112500
      IF(Y2(K))88,88,111                                                00112600
  111 Y2(K)=ALOG(Y2(K))                                                 00112700
   98 CONTINUE                                                          00112800
   99 CONTINUE                                                          00112900
      GO TO 1000                                                        00113000
 203  WRITE            (6,4000)NTL(I)                                   00113100
      GO TO 99                                                          00113200
 4000 FORMAT(29H0ILLEGAL TRANSGENERATION CODEI3,93H SPECIFIED FOR THE DE00113300
     1PENDENT VARIABLE. THE PROGRAM WILL PROCEED WITHOUT THIS TRANSGENER00113400
     2ATION.)                                                           00113500
 88   N=K-NS+1                                                          00113600
      WRITE            (6,4010)N,J,NTL(I)                               00113700
      GO TO 98                                                          00113800
 4010 FORMAT(4H0THEI4,56HTH. VALUE OF THE DEPENDENT VARIABLE CORRESPONDI00113900
     1NG TO THEI4,37HTH. VALUE OF THE INDEPENDENT VARIABLE/57H VIOLATES 00114000
     2THE RESTRICTION FOR TRANSGENERATION OF THE TYPEI3,1H.52H THE PROGR00114100
     3AM CONTINUES LEAVING THIS VALUE UNCHANGED.)                       00114200
 1101 FORMAT(A6,I1,8(I2,F6.0))                                          00114300
 1102 FORMAT(20H0TRANSFORMATION CARD)                                   00114400
 1103 FORMAT(27H0 CODE   CONSTANT  PASS NO.)                            00114500
 1104 FORMAT(1H0I5,F13.4,I5)                                            00114600
 1000 RETURN                                                            00114700
      END                                                               00114800
CVFCHCK    SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE FORMAT CRDS00114900
      SUBROUTINE VFCHCK(NVF)                                            00115000
      IF(NVF)10,10,20                                                   00115100
 10   WRITE            (6,4000 )                                        00115200
      NVF=1                                                             00115300
 50   RETURN                                                            00115400
C                                                                       00115500
 20   IF(NVF-10)50,50,10                                                00115600
C                                                                       00115700
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIF00115800
     XIED, ASSUMED TO BE 1.)                                            00115900
C                                                                       00116000
      END                                                               00116100
CSCALE        SUBROUTINE SCALE FOR SUB PLOTR        AUGUST 18, 1964     00116200
      SUBROUTINE SCALE(YMIN,YMAX,YINT,JY,TYMIN,TYMAX,YIJ,YRMA)          00116300
      DIMENSION C(10)                                                   00116400
       C(1)=  1.0                                                       00116500
       C(2)=1.5                                                         00116600
       C(3)=2.0                                                         00116700
       C(4)=3.0                                                         00116800
       C(5)=4.0                                                         00116900
      C(6)=5.0                                                          00117000
       C(7)=7.5                                                         00117100
       C(8)=10.0                                                        00117200
C     TEST=154400000000                                                 00117300
   50 YR=YMAX-YMIN                                                      00117400
      TT=YR/YINT                                                        00117500
      J=ALOG10(TT)                                                      00117600
      E=10.0**J                                                         00117700
      TT=TT/E                                                           00117800
      I=0                                                               00117900
      IF(TT-1.0)205,201,201                                             00118000
  205 TT=TT*10.0                                                        00118100
      E=E/10.0                                                          00118200
 201  I=I+1                                                             00118300
      IF(8-I)1,2,2                                                      00118400
    1 E=E*10.0                                                          00118500
      I=1                                                               00118600
    2 T3 = TT - C(I)                                                    00118700
      IF (T3 .GT. .000001 .OR. T3 .LT. -.000001) GO TO 252              00118800
      TT = INT(TT* 1000000)/1000000                                     00118900
  252 IF (TT-C(I)) 233,202,201                                          00119000
  233 YIJ=C(I)*E                                                        00119100
      GO TO 203                                                         00119200
  202 Y=YMIN/C(I)                                                       00119300
      J=Y                                                               00119400
      T=J                                                               00119500
C     IF(0.0001-ABS (T-Y))204,233,233                                   00119600
      GO TO 233                                                         00119700
  204 YIJ=C(I+1)*E                                                      00119800
  203 X=((YMAX+YMIN)/YIJ-YINT )/2.0+.00001                              00119900
      K=X                                                               00120000
      IF(K)235,240,240                                                  00120100
  235 Y=K                                                               00120200
      IF(X-Y)236,240,236                                                00120300
  236 K=K-1                                                             00120400
  240 TYMIN=K                                                           00120500
      TYMIN=YIJ*TYMIN                                                   00120600
      TYMAX=TYMIN+ YINT*YIJ                                             00120700
      TEST = YINT *(YIJ/10.0)                                           00120800
      IF(YRMA-TYMAX-TEST)10,10,201                                      00120900
   10 TT=YINT/10.                                                       00121000
      JY=TT+.000001                                                     00121100
      YIJ = TEST                                                        00121200
      J=TYMIN/ YIJ                                                      00121300
      IF (K)242,241,241                                                 00121400
  242 J=J-1                                                             00121500
  241 J=J*JY+JY-K                                                       00121600
      JY=J                                                              00121700
      RETURN                                                            00121800
      END                                                               00121900
