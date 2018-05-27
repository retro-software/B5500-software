FILE  5 = CRA, UNIT = READER, RECORD = 10, BUFFER = 2                   00000100
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00000200
C                                                                       00000300
C     BMD01M ---- PRINCIPAL COMPONENT ANALYSIS (FORTRAN)                00000400
C                                                                       00000500
CVFCHCK    SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE FORMAT CRDS00000600
      SUBROUTINE VFCHCK(NVF)                                            00000700
      IF(NVF)10,10,20                                                   00000800
 10   WRITE            (6,4000)                                         00000900
      NVF=1                                                             00001000
 50   RETURN                                                            00001100
C                                                                       00001200
 20   IF(NVF-10)50,50,10                                                00001300
C                                                                       00001400
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIF00001500
     XIED, ASSUMED TO BE 1.)                                            00001600
      END                                                               00001700
CTRANS   SUBROUTINE TRANS FOR BMD01M   MAY 19,1964                      00001800
      SUBROUTINE TRANS(DATA,NVAR,NSAM,IERROR,NVG)                       00001900
      ASN (XX)=ATAN (XX/SQRT (1.0-XX**2))                               00002000
      DIMENSION COV(25,25),C123(400,25),Z123(25,25)                     00002100
      DIMENSION DATA(400,25)                                            00002200
      COMMON      COV,C123,Z123                                         00002300
      D123=(+6HTRNGEN)                                                  00002400
      MARY=0                                                            00002500
      FN=NSAM                                                           00002600
      WRITE            (6,1403)                                         00002700
      WRITE            (6,1400)                                         00002800
      DO1000 J=1,NVG                                                    00002900
      READ           (5,1100)TODE,NEWA,LCODE,LVA,BNEW                   00003000
      IF (D123-TODE)200,201,200                                         00003100
  200 NVAR=-NVAR                                                        00003200
      GO TO 1111                                                        00003300
  201 WRITE            (6,1402)J,NEWA,LCODE,LVA,BNEW                    00003400
      IF(LCODE*(15-LCODE)) 710,710,714                                  00003500
  710 WRITE            (6, 712)                                         00003600
  712 FORMAT(29HERROR ON TRANSGENERATION CODE)                          00003700
  714 IF(LCODE-10)4,5,5                                                 00003800
    5 NEWB=BNEW                                                         00003900
    4 DO3I=1,NSAM                                                       00004000
      D=DATA(I,LVA)                                                     00004100
      GOTO(10,20,30,40,50,60,70,80,90,100,110,120,130,140),LCODE        00004200
   10 IF(D)99,7,8                                                       00004300
    7 DATA(I,NEWA)=0.0                                                  00004400
      GO TO 3                                                           00004500
    8 DATA(I,NEWA)=SQRT (D)                                             00004600
      GO TO 3                                                           00004700
   20 IF(D)99,11,12                                                     00004800
   11 DATA(I,NEWA)=1.0                                                  00004900
      GO TO 3                                                           00005000
   12 DATA(I,NEWA)=SQRT (D)+SQRT (D+1.0)                                00005100
      GO TO 3                                                           00005200
   30 IF(D)99,99,14                                                     00005300
   14 DATA(I,NEWA)=ALOG10(D)                                            00005400
      GO TO 3                                                           00005500
   40 DATA(I,NEWA)=EXP (D)                                              00005600
      GO TO 3                                                           00005700
   50 IF(D)99,16,17                                                     00005800
   16 DATA(I,NEWA)=0.0                                                  00005900
      GO TO 3                                                           00006000
   17 IF(D-1.0)18,19,99                                                 00006100
   19 DATA(I,NEWA)=3.14159265/2.0                                       00006200
      GO TO 3                                                           00006300
   18 A=SQRT (D)                                                        00006400
      DATA(I,NEWA)=ASN (A)                                              00006500
      GO TO 3                                                           00006600
   60 A=D/(FN+1.0)                                                      00006700
      B=A+1.0/(FN+1.0)                                                  00006800
      IF(A)99,23,24                                                     00006900
   23 IF(B)99,26,27                                                     00007000
   26 DATA(I,NEWA)=0.0                                                  00007100
      GO TO 3                                                           00007200
   27 DATA(I,NEWA)= ASN (SQRT (B))                                      00007300
      GO TO 3                                                           00007400
   24 IF(B)99,28,29                                                     00007500
   28 DATA(I,NEWA)=ASN (SQRT (A))                                       00007600
      GO TO 3                                                           00007700
   29 A=SQRT (A)                                                        00007800
      B=SQRT (B)                                                        00007900
      DATA(I,NEWA)=ASN (A)+ASN (B)                                      00008000
      GO TO 3                                                           00008100
   70 IF(D)31,99,31                                                     00008200
   31 DATA(I,NEWA)=1.0/D                                                00008300
      GO TO 3                                                           00008400
   80 DATA(I,NEWA)=D+BNEW                                               00008500
      GO TO 3                                                           00008600
   90 DATA(I,NEWA)=D*BNEW                                               00008700
      GO TO 3                                                           00008800
  100 IF(D)99,32,33                                                     00008900
   32 DATA(I,NEWA)=0.0                                                  00009000
      GO TO 3                                                           00009100
   33 DATA(I,NEWA)=D**NEWB                                              00009200
      GO TO 3                                                           00009300
  110 DATA(I,NEWA)=D+DATA(I,NEWB)                                       00009400
      GO TO 3                                                           00009500
  120 DATA(I,NEWA)=D-DATA(I,NEWB)                                       00009600
      GO TO 3                                                           00009700
  130 DATA(I,NEWA)=D*DATA(I,NEWB)                                       00009800
      GO TO 3                                                           00009900
  140 IF(DATA(I,NEWB))34,99,34                                          00010000
   34 DATA(I,NEWA)=D/DATA(I,NEWB)                                       00010100
      GO TO 3                                                           00010200
   99 IF(MARY)43,44,44                                                  00010300
   44 MARY=-999                                                         00010400
      IERROR=-999                                                       00010500
      WRITE            (6,1404)J                                        00010600
   43 WRITE            (6,1405)I                                        00010700
    3 CONTINUE                                                          00010800
      IF(IERROR)42,1000,1000                                            00010900
 1000 CONTINUE                                                          00011000
 1100 FORMAT(A6,I3,I2,I3,F6.0)                                          00011100
 1400 FORMAT(46H0CARD     K      TRANS    ORIG.   ORIG. VAR(J)/45H  NO. 00011200
     1VARIABLE   CODE    VAR(I)   OR CONSTANT)                          00011300
 1401 FORMAT(41H0PROGRAM CANNOT CONTINUE FOR THIS PROBLEM)              00011400
 1402 FORMAT(2H  I2,I8,2I9,F15.5)                                       00011500
 1403 FORMAT(1H06X,23HTRANS GENERATOR CARD(S))                          00011600
 1404 FORMAT(30H0THE INSTRUCTIONS INDICATED ON/25H TRANS GENERATOR CARD 00011700
     1NO.I2,4H RE-/29H SULTED IN THE VIOLATION OF A/31H RESTRICTION FOR 00011800
     2THIS TRANSFOR-/31H MATION. THE VIOLATION OCCURRED/28H FOR THE ITEM00011900
     3S LISTED BELOW./)                                                 00012000
 1405 FORMAT(10H ITEM NO. I3)                                           00012100
      IF(IERROR)42,1111,1111                                            00012200
   42 WRITE            (6,1401)                                         00012300
 1111 RETURN                                                            00012400
      END                                                               00012500
CTPWD    SUBROUTINE TPWD FOR BMD01M         VERSION OF SEPT. 26, 1963   00012600
      SUBROUTINE TPWD(NT1,NT2)                                          00012700
      IF(NT1)40,10,12                                                   00012800
 10   NT1=5                                                             00012900
 12   IF(NT1-NT2)14,19,14                                               00013000
 14   IF(NT2-5)15,19,17                                                 00013100
   15 REWIND NT2                                                        00013200
      GO TO 19                                                          00013300
   17                                                                   00013400
   17 CONTINUE                                                          00013410
   19 IF(NT1-5)18,24,18                                                 00013500
 18   IF(NT1-6)22,40,22                                                 00013600
 22   REWIND NT1                                                        00013700
 24   NT2=NT1                                                           00013800
 28   RETURN                                                            00013900
 40   WRITE            (6,49)                                           00014000
      CALL EXIT                                                         00014100
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              00014200
      END                                                               00014300
CRDLBL                                                                  00014400
C     SUBROUTINE TO READ IN LABELS CARDS, STORE THEM IN ARRAY,          00014500
C     AND SUBSTITUTE NUMBERS FOR UNLABELED VARIABLES                    00014600
C     NVAR IS TOTAL NUMBER OF VARIABLES                                 00014700
C     NLBVAR IS NUMBER OF LABELED VARIABLES EXPECTED                    00014800
C                                                                       00014900
      SUBROUTINE RDLBL(NLBVAR,NVAR,ARRAY)                               00015000
C     EQUIVALENCE INTEGER AND FLOATING NAMES SO THAT INTEGER SUBTRACTION00015100
C     MAY BE USED TO TEST ALPHABETIC EQUALITY                           00015200
      EQUIVALENCE(LABEL,ALABEL)                                         00015300
      DIMENSION ARRAY(1),IDUM(7),DUMY(7)                                00015400
      ALABEL=(+3HLAB)                                                   00015500
C     NUMBER VARIABLES                                                  00015600
      DO 1 I=1,NVAR                                                     00015700
   1  ARRAY(I)=I                                                        00015800
C     IF NO LABELS, RETURN                                              00015900
      IF(NLBVAR) 9,9,2                                                  00016000
   2  N=0                                                               00016100
C     READ 1 LABELS CARD                                                00016200
  20  READ           (5,3)ITEST,(IDUM(J),DUMY(J),J=1,7)                 00016300
   3  FORMAT(A3,3X,7(I4,A6))                                            00016400
C     TEST FOR "LAB" IN FIRST 3 COLS.                                   00016500
      IF(ITEST-LABEL) 4,6,4                                             00016600
C     ERROR--PRINT MESSAGE AND QUIT                                     00016700
   4  WRITE            (6,5)                                            00016800
   5  FORMAT(36H0LABELS CARD NOT FOUND WHEN EXPECTED)                   00016900
      CALL EXIT                                                         00017000
C     EXAMINE 7 FIELDS                                                  00017100
   6  DO 8 J=1,7                                                        00017200
      K=IDUM(J)                                                         00017300
C     TEST INDEX.  IF 0, IGNORE.  IF ILLEGAL, PRINT MESSAGE AND         00017400
C     IGNORE EXCEPT TO COUNT                                            00017500
      IF(K) 11,8,10                                                     00017600
  10  IF(K-NVAR) 7,7,11                                                 00017700
  11  WRITE            (6,12)K,DUMY(J)                                  00017800
  12  FORMAT(18H0LABELS CARD INDEX,I7,18H INCORRECT. LABEL ,A6,9H IGNORE00017900
     1D.)                                                               00018000
      GO TO 13                                                          00018100
C     MOVE LABEL TO ARRAY                                               00018200
   7  ARRAY(K)=DUMY(J)                                                  00018300
C     STEP NUMBER OF VARIABLES                                          00018400
  13  N=N+1                                                             00018500
C     TEST FOR END. IF END, RETURN. IF NOT, SCAN OTHER FIELDS.          00018600
      IF(N-NLBVAR) 8,9,9                                                00018700
   8  CONTINUE                                                          00018800
      GO TO 20                                                          00018900
   9  RETURN                                                            00019000
      END                                                               00019100
CPATTY2      SUBROUTINE PATTY2 FOR BMD01M        JUNE 28, 1963          00019200
C SUBROUTINE PATTY2 FOR BM01M                   JUNE 28, 1963           00019300
      SUBROUTINE PATTY2(A,N,NAMES,JK)                                   00019400
      DIMENSION A(25,25),NAMES(25),NN(8)                                00019500
      IT=1                                                              00019600
      KK=0                                                              00019700
      K1=IT                                                             00019800
      K2= MIN0 (8,N)                                                    00019900
    5 KK=KK+8                                                           00020000
      IF(N-KK)3,3,4                                                     00020100
    4 IT=IT+1                                                           00020200
      GO TO 5                                                           00020300
    3 DO 50 JX=1,IT                                                     00020400
      LLL=K2-K1+1                                                       00020500
      LL=0                                                              00020600
      IF(JK)35,35,37                                                    00020700
   35 WRITE            (6,350)(IG,IG=1,LLL)                             00020800
      GO TO 45                                                          00020900
   37 DO 40 JJ=K1,K2                                                    00021000
      LL=LL+1                                                           00021100
   40 NN(LL)=NAMES(JJ)                                                  00021200
      WRITE            (6,300)(NN(II),II=1,LLL)                         00021300
   45 DO 10 I=1,N                                                       00021400
   10 WRITE            (6,20)NAMES(I),(A(I,J),J=K1,K2)                  00021500
      K1=K2+1                                                           00021600
      K2=K1+7                                                           00021700
      K2= MIN0 (K2,N)                                                   00021800
  300 FORMAT(1H013X,A6,7(8X,A6)/)                                       00021900
   20 FORMAT(1H A6,1X,8F14.4)                                           00022000
  350 FORMAT(1H017X,I2,7(12X,I2)/)                                      00022100
   50 CONTINUE                                                          00022200
      RETURN                                                            00022300
      END                                                               00022400
CEIGEN     SUBROUTINE EIGEN FOR BMD01M      JUNE 28, 1963               00022500
      SUBROUTINE EIGEN(VALU,N,M)                                        00022600
C                                                                       00022700
C     EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC MATRIX           00022800
C                                                                       00022900
      DIMENSION A(25,25), B(25,25), VALU(25), DIAG(25), SUPERD(24),     00023000
     X          Q(24), VALL(25), S(24), C(24), D(25), IND(25), U(25),   00023100
     X          DUMMY(10000)                                            00023200
      DIMENSION X123(400,25),Y123(400,25),DUMY1(400,25),DUMY2(625)      00023300
      COMMON X123,B,Y123,A                                              00023400
      EQUIVALENCE (X123,DUMY1),(B,DUMY2),(SUPERD,DUMMY(26)),(TAU,BETA), 00023500
     X            (VALL,D,DUMMY(50)),(Q,S,DUMMY(75)),(IND,U),(II,MATCH),00023600
     X            (DIAG,DUMMY,Y123),(ANORM,ANORM2),(P,PRODS),(T,SMALLD) 00023700
C                                                                       00023800
C     CALCULATE NORM OF MATRIX                                          00023900
C                                                                       00024000
    3 ANORM2=0.0                                                        00024100
    4 DO 6 I=1,N                                                        00024200
    5 DO 6 J=1,N                                                        00024300
    6 ANORM2=ANORM2+A(I,J)**2                                           00024400
    7 ANORM=SQRT (ANORM2)                                               00024500
C                                                                       00024600
C     GENERATE IDENTITY MATRIX                                          00024700
C                                                                       00024800
    9 IF (M) 10, 45, 10                                                 00024900
   10 DO 40 I=1,N                                                       00025000
   12 DO 40 J=1,N                                                       00025100
   20 IF(I-J) 35, 25, 35                                                00025200
   25 B(I,J)=1.0                                                        00025300
   30 GO TO 40                                                          00025400
   35 B(I,J)=0.0                                                        00025500
   40 CONTINUE                                                          00025600
C                                                                       00025700
C     PERFORM ROTATIONS TO REDUCE MATRIX TO JACOBI FORM                 00025800
C                                                                       00025900
   45 IEXIT=1                                                           00026000
   50 NN=N-2                                                            00026100
   52 IF (NN) 890, 170, 55                                              00026200
   55 DO 160 I=1,NN                                                     00026300
   60 II=I+2                                                            00026400
   65 DO 160 J=II,N                                                     00026500
   70 T1=A(I,I+1)                                                       00026600
   75 T2=A(I,J)                                                         00026700
   80 GO TO 900                                                         00026800
   90 DO 105 K=I,N                                                      00026900
   95 T2=COS*A(K,I+1)+SIN*A(K,J)                                        00027000
  100 A(K,J)=COS*A(K,J)-SIN*A(K,I+1)                                    00027100
  105 A(K,I+1)=T2                                                       00027200
  110 DO 125 K=I,N                                                      00027300
  115 T2=COS*A(I+1,K)+SIN*A(J,K)                                        00027400
  120 A(J,K)=COS*A(J,K)-SIN*A(I+1,K)                                    00027500
  125 A(I+1,K)=T2                                                       00027600
  128 IF (M) 130, 160, 130                                              00027700
  130 DO 150 K=1,N                                                      00027800
  135 T2=COS*B(K,I+1)+SIN*B(K,J)                                        00027900
  140 B(K,J)=COS*B(K,J)-SIN*B(K,I+1)                                    00028000
  150 B(K,I+1)=T2                                                       00028100
  160 CONTINUE                                                          00028200
C                                                                       00028300
C     MOVE JACOBI FORM ELEMENTS AND INITIALIZE EIGENVALUE BOUNDS        00028400
C                                                                       00028500
  170 DO 200 I=1,N                                                      00028600
  180 DIAG(I)=A(I,I)                                                    00028700
  190 VALU(I)=ANORM                                                     00028800
  200 VALL(I)=-ANORM                                                    00028900
  210 DO 230 I=2,N                                                      00029000
  220 SUPERD(I-1)=A(I-1,I)                                              00029100
  230 Q(I-1)=(SUPERD(I-1))**2                                           00029200
C                                                                       00029300
C     DETERMINE SIGNS OF PRINCIPAL MINORS                               00029400
C                                                                       00029500
  235 TAU=0.0                                                           00029600
  240 I=1                                                               00029700
  260 MATCH=0                                                           00029800
  270 T2=0.0                                                            00029900
  275 T1=1.0                                                            00030000
  277 DO 450 J=1,N                                                      00030100
  280 P=DIAG(J)-TAU                                                     00030200
  290 IF(T2) 300, 330, 300                                              00030300
  300 IF(T1) 310, 370, 310                                              00030400
  310 T=P*T1-Q(J-1)*T2                                                  00030500
  320 GO TO 410                                                         00030600
  330 IF(T1) 335, 350, 350                                              00030700
  335 T1=-1.0                                                           00030800
  340 T=-P                                                              00030900
  345 GO TO 410                                                         00031000
  350 T1=1.0                                                            00031100
  355 T=P                                                               00031200
  360 GO TO 410                                                         00031300
  370 IF(Q(J-1)) 380, 350, 380                                          00031400
  380 IF(T2) 400, 390, 390                                              00031500
  390 T=-1.0                                                            00031600
  395 GO TO 410                                                         00031700
  400 T=1.0                                                             00031800
C                                                                       00031900
C     COUNT AGREEMENTS IN SIGN                                          00032000
C                                                                       00032100
  410 IF(T1) 425, 420, 420                                              00032200
  420 IF(T) 440, 430, 430                                               00032300
  425 IF(T) 430, 440, 440                                               00032400
  430 MATCH=MATCH+1                                                     00032500
  440 T2=T1                                                             00032600
  450 T1=T                                                              00032700
C                                                                       00032800
C     ESTABLISH TIGHTER BOUNDS ON EIGENVALUES                           00032900
C                                                                       00033000
  460 DO 530 K=1,N                                                      00033100
  465 IF (K-MATCH) 470, 470, 520                                        00033200
  470 IF(TAU-VALL(K)) 530, 530, 480                                     00033300
  480 VALL(K)=TAU                                                       00033400
  490 GO TO 530                                                         00033500
  520 IF(TAU-VALU(K)) 525, 530, 530                                     00033600
  525 VALU(K)=TAU                                                       00033700
  530 CONTINUE                                                          00033800
  540 IF(VALU(I)-VALL(I)-5.0E-8) 570, 570, 550                          00033900
  550 IF(VALU(I)) 560, 580, 560                                         00034000
  560 IF(ABS (VALL(I)/VALU(I)-1.0)-5.0E-8) 570, 570, 580                00034100
  570 I=I+1                                                             00034200
  575 IF(I-N) 540, 540, 590                                             00034300
  580 TAU=(VALL(I)+VALU(I))/2.0                                         00034400
  585 GO TO 260                                                         00034500
C                                                                       00034600
C     JACOBI EIGENVECTORS BY ROTATIONAL TRIANGULARIZATION               00034700
C                                                                       00034800
  590 IF (M) 593, 890, 593                                              00034900
  593 IEXIT=2                                                           00035000
  595 DO 610 I=1,N                                                      00035100
  600 DO 610 J=1,N                                                      00035200
  610 A(I,J)=0.0                                                        00035300
  615 DO 850 I=1,N                                                      00035400
  620 IF (I-1) 625, 625, 621                                            00035500
  621 IF (VALU(I-1)-VALU(I)-5.0E-7) 730, 730, 622                       00035600
  622 IF (VALU(I-1)) 623, 625, 623                                      00035700
  623 IF (ABS (VALU(I)/VALU(I-1)-1.0)-5.0E-7) 730, 730, 625             00035800
  625 COS=1.0                                                           00035900
  628 SIN=0.0                                                           00036000
  630 DO 700 J=1,N                                                      00036100
  635 IF(J-1) 680, 680, 640                                             00036200
  640 GO TO 900                                                         00036300
  650 S(J-1)=SIN                                                        00036400
  660 C(J-1)=COS                                                        00036500
  670 D(J-1)=T1*COS+T2*SIN                                              00036600
  680 T1=(DIAG(J)-VALU(I))*COS-BETA*SIN                                 00036700
  690 T2=SUPERD(J)                                                      00036800
  700 BETA=SUPERD(J)*COS                                                00036900
  710 D(N)=T1                                                           00037000
  720 DO 725 J=1,N                                                      00037100
  725 IND(J)=0                                                          00037200
  730 SMALLD=ANORM                                                      00037300
  735 DO 780 J=1,N                                                      00037400
  740 IF (IND(J)-1) 750, 780, 780                                       00037500
  750 IF (ABS (SMALLD)-ABS (D(J)))780, 780, 760                         00037600
  760 SMALLD=D(J)                                                       00037700
  770 NN=J                                                              00037800
  780 CONTINUE                                                          00037900
  790 IND(NN)=1                                                         00038000
  800 PRODS=1.0                                                         00038100
  805 IF (NN-1) 810, 850, 810                                           00038200
  810 DO 840 K=2,NN                                                     00038300
  820 II=NN+1-K                                                         00038400
  830 A(II+1,I)=C(II)*PRODS                                             00038500
  840 PRODS=-PRODS*S(II)                                                00038600
  850 A(1,I)=PRODS                                                      00038700
C                                                                       00038800
C     FORM MATRIX PRODUCT OF ROTATION MATRIX WITH JACOBI VECTOR MATRIX  00038900
C                                                                       00039000
  855 DO 885 J=1,N                                                      00039100
  860 DO 865 K=1,N                                                      00039200
  865 U(K)=A(K,J)                                                       00039300
  870 DO 885 I=1,N                                                      00039400
  875 A(I,J)=0.0                                                        00039500
  880 DO 885 K=1,N                                                      00039600
  885 A(I,J)=B(I,K)*U(K)+A(I,J)                                         00039700
  890 GO TO 941                                                         00039800
C                                                                       00039900
C     CALCULATE SINE AND COSINE OF ANGLE OF ROTATION                    00040000
C                                                                       00040100
  900 IF (T2) 910, 940, 910                                             00040200
  910 T=SQRT (T1**2+T2**2)                                              00040300
  920 COS=T1/T                                                          00040400
  925 SIN=T2/T                                                          00040500
  930 GO TO (90,650), IEXIT                                             00040600
  940 GO TO (160,910), IEXIT                                            00040700
  941 RETURN                                                            00040800
      END                                                               00040900
CBMD01M       PRINCIPAL COMPONENT ANALYSIS          AUGUST 26, 1965     00041000
      DIMENSION X(400,25),XMEAN(25),COV(25,25),VALU(25),SCALE(25),      00041100
     1C(400,25),Z(25,25),FMT(120),NAMES(25)                             00041200
      COMMON X,COV,C,Z                                                  00041300
C                                                                       00041400
  209 FORMAT(42H1BMD01M - COMPONENT ANALYSIS - VERSION OF               00041500
     X15HAUGUST 26, 1965/                                               00041600
     140H HEALTH SCIENCES COMPUTING FACILITY,UCLA//                     00041700
     214H PROBLEM CODE A6,/                                             00041800
     321H NUMBER OF VARIABLES I3,/                                      00041900
     417H NUMBER OF CASES I6,/                                          00042000
     527H NUMBER OF VARIABLES ADDED I4,/                                00042100
     635H NUMBER OF TRANSGENERATION CARD(S) I4,/                        00042200
     7 35H NUMBER OF VARIABLE FORMAT CARD(S) I3,///)                    00042300
C                                                                       00042400
      A123=(+6HFINISH)                                                  00042500
      B123=(+6HPROBLM)                                                  00042600
      C123=(+3HYES)                                                     00042700
      NTAPE=5                                                           00042800
   10 READ           (5,901)TODE,NPROB,NV,N,RNCR,GCK,NADD,NVG           00042900
     1,NLV,MTAPE,KVR                                                    00043000
      IERROR=0                                                          00043100
      IF (A123-TODE)200,201,200                                         00043200
  202 WRITE            (6,204)                                          00043300
 201  IF(NTAPE-5)12,12,11                                               00043400
   11                                                                   00043500
   11 CONTINUE                                                          00043510
 12   CALL EXIT                                                         00043600
 200  IF (B123-TODE)202,203,202                                         00043700
  203 CALL TPWD(MTAPE,NTAPE)                                            00043800
    9 IF((NV-1)*(NV-26)) 205,202,202                                    00043900
  205 IF((N-2)*(N-401)) 206,202,202                                     00044000
  206 IF((NV+NADD-1)*(NV+NADD-26)) 207,202,202                          00044100
 207  CALL VFCHCK(KVR)                                                  00044200
  204 FORMAT(45H0CONTROL CARDS INCORRECTLY ORDERED OR PUNCHED)          00044300
  208 WRITE            (6,209)NPROB,NV,N,NADD,NVG,KVR                   00044400
  901 FORMAT(2A6,I2,I3,2A3,I3,2I2,38X,2I2)                              00044500
  211 NV1=NV+NADD                                                       00044600
      CALL RDLBL(NLV,NV1,NAMES)                                         00044700
   17 KVR=KVR*12                                                        00044800
      READ           (5,942)(FMT(I),I=1,KVR)                            00044900
      DO 13 I=1,N                                                       00045000
   13 READ           (NTAPE,FMT)(X(I,J),J=1,NV)                         00045100
   19 ON=N                                                              00045200
      IF(NVG) 202,600,601                                               00045300
  601 CALL TRANS (X,NV,N,IERROR,NVG)                                    00045400
      IF(IERROR)10,600,600                                              00045500
  600 IF(NV) 202,202,212                                                00045600
  212 NV=NV1                                                            00045700
      DO 21 J=1,NV                                                      00045800
      XMEAN(J)=0.0                                                      00045900
      DO 20 I=1,N                                                       00046000
  20  XMEAN(J)=XMEAN(J)+X(I,J)                                          00046100
  21  XMEAN(J)=XMEAN(J)/ON                                              00046200
      DO 22 I=1,NV                                                      00046300
      DO 22 J=1,NV                                                      00046400
      COV(I,J)=0.0                                                      00046500
      DO 22 K=1,N                                                       00046600
  22  COV(I,J)=COV(I,J)+(X(K,I)-XMEAN(I))*(X(K,J)-XMEAN(J))             00046700
      DO 23 I=1,NV                                                      00046800
  23  SCALE(I)=SQRT (COV(I,I))                                          00046900
      DO 24 I=1,NV                                                      00047000
      DO 24 J=1,NV                                                      00047100
  24  Z(I,J)=COV(I,J)/(SCALE(I)*SCALE(J))                               00047200
      WRITE (6,923)                                                     00047300
      WRITE (6,904)                                                     00047400
      CALL PATTY2(Z,NV,NAMES,1)                                         00047500
      CALL EIGEN(VALU,NV,NV)                                            00047600
      WRITE (6,923)                                                     00047700
      WRITE (6,907)                                                     00047800
      WRITE            (6,906)(VALU(I),I=1,NV)                          00047900
      RANK=0.0                                                          00048000
      DO 26 I=1,NV                                                      00048100
 26   RANK=RANK+VALU(I)                                                 00048200
      SMALL=0.0                                                         00048300
      DO 18 I=1,NV                                                      00048400
      SMALL=SMALL+VALU(I)                                               00048500
   18 VALU(I)=SMALL/RANK                                                00048600
      WRITE (6,937)                                                     00048700
      WRITE            (6, 938)(VALU(I),I=1,NV)                         00048800
      WRITE (6,923)                                                     00048900
      WRITE (6,908)                                                     00049000
      CALL PATTY2(Z,NV,NAMES,0)                                         00049100
      DO 29 J=1,NV                                                      00049200
      DO 29 I=1,N                                                       00049300
  29  X(I,J)=(X(I,J)-XMEAN(J))/SCALE(J)                                 00049400
      ONN=N-1                                                           00049500
      SQ=SQRT (ONN)                                                     00049600
      DO 43 I=1,N                                                       00049700
      DO 43 J=1,NV                                                      00049800
      C(I,J)=0.0                                                        00049900
      DO 42 K=1,NV                                                      00050000
   42 C(I,J)=C(I,J)+X(I,K)*Z(K,J)                                       00050100
   43 C(I,J)=C(I,J)*SQ                                                  00050200
      IF (GCK-C123) 57,41,57                                            00050300
  41  DO 51 J=1,NV                                                      00050400
      XMEAN(J)=0.0                                                      00050500
      DO 50 I=1,N                                                       00050600
  50  XMEAN(J)=XMEAN(J)+C(I,J)                                          00050700
  51  XMEAN(J)=XMEAN(J)/ON                                              00050800
      DO 52 I=1,NV                                                      00050900
      DO 52 J=1,NV                                                      00051000
      COV(I,J)=0.0                                                      00051100
      DO 525 K=1,N                                                      00051200
  525 COV(I,J)=COV(I,J)+(C(K,I)-XMEAN(I))*(C(K,J)-XMEAN(J))             00051300
   52 COV(I,J)=COV(I,J)/ONN                                             00051400
      WRITE (6,923)                                                     00051500
      WRITE (6,922)                                                     00051600
      CALL PATTY2(COV,NV,NAMES,1)                                       00051700
   57 IF (RNCR-C123)40,30,40                                            00051800
  30  WRITE (6,923)                                                     00051900
      WRITE (6,909)                                                     00052000
      WRITE (6,910)                                                     00052100
      SMALL=-(10.0**36.0)                                               00052200
      DO 39 II=1,NV                                                     00052300
      WRITE            (6,912) II                                       00052400
      DO 32 I=1,N                                                       00052500
      C(I,1)=0.0                                                        00052600
      C(I,2)=0.0                                                        00052700
      DO 31 K=1,NV                                                      00052800
  31  C(I,1)=C(I,1)+X(I,K)*Z(K,II)                                      00052900
   32 C(I,1)=C(I,1)*SQ                                                  00053000
      DO 39 I=1,N                                                       00053100
      RANK=SMALL                                                        00053200
      DO 38 J=1,N                                                       00053300
      IF(C(J,1)-RANK)38,38,36                                           00053400
  36  IF(C(J,2)-999.0)37,38,38                                          00053500
  37  RANK=C(J,1)                                                       00053600
      NJ=J                                                              00053700
  38  CONTINUE                                                          00053800
      C(NJ,2)=999.0                                                     00053900
      WRITE            (6,911)RANK,NJ                                   00054000
  39  CONTINUE                                                          00054100
  40  GO TO 10                                                          00054200
 902  FORMAT(19H0COMPONENT ANALYSIS)                                    00054300
 903  FORMAT(12H PROBLEM NO.I4)                                         00054400
 904  FORMAT(31H0CORRELATION COEFFICIENT MATRIX)                        00054500
 907  FORMAT(12H0EIGENVALUES)                                           00054600
 908  FORMAT(13H0EIGENVECTORS)                                          00054700
 909  FORMAT(48H0RANK ORDER OF EACH STANDARDIZED CASE ORDERED BY)       00054800
 910  FORMAT(44H SIZE OF EACH PRINCIPAL COMPONENT SEPARATELY)           00054900
  911 FORMAT(F18.6,I10)                                                 00055000
 912  FORMAT(16H0  COMPONENT NO.I3,12H    CASE NO.)                     00055100
 922  FORMAT(25H0EIGEN VALUE CHECK MATRIX)                              00055200
 923  FORMAT(1H0)                                                       00055300
 937  FORMAT(40H0CUMULATIVE PROPORTION OF TOTAL VARIANCE)               00055400
 938  FORMAT(1H F11.2,7F15.2)                                           00055500
  942 FORMAT(12A6)                                                      00055600
  906 FORMAT(6F16.7)                                                    00055700
      STOP                                                              00055800
      END                                                               00055900
