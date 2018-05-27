CID                     BMD04M                                                  
CID   0901HS 15 150    $BMD04M DISCRIMINANT ANALYSIS FOR TWO GROUPS     M0400000
CIBJOB         ALTIO                                                    M0400010
CIBFTC D04M    LIST,M94,XR7                                             M0400020
C             DISCRIMINANT ANALYSIS - TWO GROUPS      JUNE  9, 1966     M0400030
C        THIS IS A SIFTED VERSION OF BMD04M ORIGINALLY WRITTEN IN       M0400040
C        FORTRAN II. SOME MODIFICATIONS WERE MADE TO MAKE IT OPERABLE   M0400050
C        AND SLIGHTLY MORE EFFICIENT THAN THE SIFTED VERSION.           M0400060
      DIMENSION X(2,25,300),SUM(2,25,25),TOTAL(2,25),D(25),B(25),Z(2,300M0400070
     1), MMM(25),  N(2),LL(25),DIV(2),C(25,25),DD(25),ZBAR(2),VARZ(2),  M0400080
     2LLL(25),IRANK(2,300),LX(25),LY(25),FMT(120)                       M0400090
C                                                                       M0400100
  343 FORMAT(56H1BMD04M - DISCRIMINANT ANALYSIS-TWO GROUPS - VERSION OF M0400110
     118HJUNE  9, 1966     ,/                                           M0400120
     241H HEALTH SCIENCES COMPUTING FACILITY, UCLA)                     M0400130
C                                                                       M0400140
      DATA A2,FINISH,A3/6HPROBLM,6HFINISH,6HSELECT/                     M0400150
      MTAPE=5                                                           M0400160
 6    READ (5,310)A1,PROB,K,N(1),N(2),NVG,NADD,NTAPE,NUM,KVR            M0400170
      IF(A1.EQ.A2)GO TO 110                                             M0400180
      IF(A1.EQ.FINISH)GO TO 100                                         M0400190
  107 WRITE (6,345)                                                     M0400200
      GO TO 100                                                         M0400210
  110 CALL TPWD(NTAPE,MTAPE)                                            M0400220
 116  WRITE (6,343)                                                     M0400230
      IF((K-1)*(K-26))112,107,107                                       M0400240
  112 K1=K+NADD                                                         M0400250
      IF((K1-1)*(K1-26))114,107,107                                     M0400260
  114 WRITE (6,339)PROB,K1                                              M0400270
      DO 117 I=1,2                                                      M0400280
      IF((N(I)-K1+1)*(N(I)-301))117,107,107                             M0400290
  117 CONTINUE                                                          M0400300
      NTRAN=0                                                           M0400310
      IERROR=0                                                          M0400320
      IF(KVR.GT.0.AND.KVR.LE.10)GO TO 118                               M0400330
      KVR=1                                                             M0400340
      WRITE(6,4000)                                                     M0400350
  118 KVR=KVR*12                                                        M0400360
      READ (5,344)(FMT(I),I=1,KVR)                                      M0400370
      DO 410 I=1,2                                                      M0400380
      NN=N(I)                                                           M0400390
      DO 410 J=1,NN                                                     M0400400
  410 READ (MTAPE,FMT)(X(I,L,J), L=1,K)                                 M0400410
      IF(NVG)21,21,500                                                  M0400420
  500 CALL TRNGEN(X,K,N,IERROR,2,NVG)                                   M0400430
      IF(IERROR) 10, 21, 21                                             M0400440
   10 DO 15 I=1,NUM                                                     M0400450
   15 READ (5,340)A1                                                    M0400460
      GO TO 6                                                           M0400470
   21 K=K1                                                              M0400480
      NSEL=0                                                            M0400490
      DO 35 M=1,2                                                       M0400500
      DIV(M)=N(M)                                                       M0400510
      DO 30 I=1,K                                                       M0400520
      LL(I)=I                                                           M0400530
      TOTAL(M,I)=0.0                                                    M0400540
      DO 30 L=I,K                                                       M0400550
      C(I,L)=0.0                                                        M0400560
      SUM(M,I,L)=0.0                                                    M0400570
      NN=N(M)                                                           M0400580
      DO 25 J=1,NN                                                      M0400590
  25  SUM(M,I,L)=SUM(M,I,L)+X(M,I,J)*X(M,L,J)                           M0400600
  30  SUM(M,L,I)=SUM(M,I,L)                                             M0400610
      DO 35 I=1,K                                                       M0400620
      NN=N(M)                                                           M0400630
      DO 35 J=1,NN                                                      M0400640
  35  TOTAL(M,I)=TOTAL(M,I)+X(M,I,J)                                    M0400650
      DO 37 I=1,K                                                       M0400660
  37  D(I)=TOTAL(1,I)/DIV(1)-TOTAL(2,I)/DIV(2)                          M0400670
      DO 38 I=1,K                                                       M0400680
  38  DD(I)=D(I)                                                        M0400690
      WRITE (6,313)                                                     M0400700
      WRITE (6,321)                                                     M0400710
      DO 36 I=1,2                                                       M0400720
      DO 36 J=1,K                                                       M0400730
  36  Z(I,J)=TOTAL(I,J)/DIV(I)                                          M0400740
      DO 39 I=1,K                                                       M0400750
  39  WRITE (6,314)I,Z(1,I),Z(2,I),D(I)                                 M0400760
      DO 40 I=1,K                                                       M0400770
      DO 40 L=1,K                                                       M0400780
      C(I,L)=SUM(1,I,L)+SUM(2,I,L)-TOTAL(1,I)*TOTAL(1,L)/DIV(1)-TOTAL(2,M0400790
     1I)*TOTAL(2,L)/DIV(2)                                              M0400800
  40  SUM(1,I,L)=C(I,L)                                                 M0400810
      WRITE (6,301)                                                     M0400820
      DO 42 I=1,K                                                       M0400830
  42  WRITE (6,302)(SUM(1,I,L),L=1,K)                                   M0400840
  43  CALL INVERT (C,K,DET,LX,LY)                                       M0400850
      WRITE (6,323)                                                     M0400860
      DO 44 I=1,K                                                       M0400870
  44  WRITE (6,302)(C(I,J),J=1,K)                                       M0400880
      DO 45 I=1,K                                                       M0400890
      B(I)=0.0                                                          M0400900
      DO 45 L=1,K                                                       M0400910
  45  B(I)=B(I)+C(I,L)*D(L)                                             M0400920
      WRITE (6,303)                                                     M0400930
      WRITE (6,302)(B(I),I=1,K)                                         M0400940
      DSQ=0.0                                                           M0400950
      DO 48I=1,K                                                        M0400960
      DO 48 J=1,K                                                       M0400970
  48  DSQ=DSQ+D(I)*D(J)*C(I,J)                                          M0400980
      DSQ=DSQ*(DIV(1)+DIV(2)-2.0)                                       M0400990
      WRITE (6,320)DSQ                                                  M0401000
      IDF=N(1)+N(2)-K-1                                                 M0401010
      DK=IDF                                                            M0401020
      FK=K                                                              M0401030
      VAL=DIV(1)*DIV(2)*DK/(FK*(DIV(1)+DIV(2))*(DIV(1)+DIV(2)-2.0))     M0401040
      VAL=VAL*DSQ                                                       M0401050
      WRITE (6,341)K,IDF,VAL                                            M0401060
      DO 50 M=1,2                                                       M0401070
      NN=N(M)                                                           M0401080
      DO 50 J=1,NN                                                      M0401090
      Z(M,J)=0.0                                                        M0401100
      DO 50 I=1,K                                                       M0401110
      LI=LL(I)                                                          M0401120
  50  Z(M,J)=Z(M,J)+B(I)*X(M,LI,J)                                      M0401130
      WRITE (6,322)                                                     M0401140
      DO 52 M=1,2                                                       M0401150
      NN=N(M)                                                           M0401160
      SUMZ=0.0                                                          M0401170
      SUMZSQ=0.0                                                        M0401180
      ZBAR(M)=0.0                                                       M0401190
      DIVN=N(M)                                                         M0401200
      VARZ(M)=0.0                                                       M0401210
      DO 51 I=1,NN                                                      M0401220
      SUMZ=SUMZ+Z(M,I)                                                  M0401230
  51  SUMZSQ=SUMZSQ+Z(M,I)**2                                           M0401240
      ZBAR(M)=SUMZ/DIVN                                                 M0401250
      VARZ(M)=(SUMZSQ-SUMZ**2/DIVN)/(DIVN-1.0)                          M0401260
      STDVZ=SQRT(VARZ(M))                                               M0401270
  52  WRITE (6,319)M,NN,ZBAR(M),VARZ(M),STDVZ                           M0401280
      WRITE (6,304)                                                     M0401290
      WRITE (6,324)                                                     M0401300
      DO 53 I=1,2                                                       M0401310
      DO 53 J=1,300                                                     M0401320
  53  IRANK (I,J)=0                                                     M0401330
      NTOTAL=N(1)+N(2)                                                  M0401340
      DO 70 I=1,NTOTAL                                                  M0401350
      HOLD=-(10.0**35.0)                                                M0401360
      DO 59 M=1,2                                                       M0401370
      NN=N(M)                                                           M0401380
      DO 59 J=1,NN                                                      M0401390
      IF(Z(M,J)-HOLD)59,59,54                                           M0401400
  54  IF(IRANK(M,J))55,55,59                                            M0401410
  55  MM=M                                                              M0401420
      JJ=J                                                              M0401430
      HOLD=Z(M,J)                                                       M0401440
  59  CONTINUE                                                          M0401450
      IRANK (MM,JJ)=999                                                 M0401460
      IF(MM-1)60,60,61                                                  M0401470
  60  WRITE (6,305)I,HOLD,JJ                                            M0401480
      GO TO 70                                                          M0401490
  61  WRITE (6,325)I,HOLD,JJ                                            M0401500
  70  CONTINUE                                                          M0401510
  71  IF(NUM) 6, 6, 72                                                  M0401520
  72  NSEL=NSEL+1                                                       M0401530
      WRITE (6,317)NSEL                                                 M0401540
      READ (5,340)A1,K,(LL(I), I=1,25)                                  M0401550
      IF(A1.EQ.A3)GO TO 74                                              M0401560
  73  WRITE (6,346)NSEL                                                 M0401570
      NUM=NUM-1                                                         M0401580
      GO TO 71                                                          M0401590
  74  WRITE (6,309)                                                     M0401600
      WRITE (6,307)(LL(I),I=1,K)                                        M0401610
      DO 76 I=1,K                                                       M0401620
      LLI=LL(I)                                                         M0401630
      DO 75 L=1,K                                                       M0401640
      LLLL=LL(L)                                                        M0401650
  75  C(I,L)=SUM(1,LLI,LLLL)                                            M0401660
  76  D(I)=DD(LLI)                                                      M0401670
      NUM=NUM-1                                                         M0401680
      GO TO 43                                                          M0401690
 301  FORMAT(36H0 SUM OF PRODUCTS OF DEV. FROM MEANS)                   M0401700
 302  FORMAT(7F16.5)                                                    M0401710
 303  FORMAT(36H0 DISCRIMINANT FUNCTION COEFFICIENTS)                   M0401720
 304  FORMAT(69H0          FIRST GROUP      SECOND GROUP    FIRST GROUP M0401730
     1 SECOND GROUP)                                                    M0401740
 305  FORMAT(1H I4,F17.5,17X,I12)                                       M0401750
 307  FORMAT(10I5)                                                      M0401760
 309  FORMAT(28H  VARIABLES USED IN FUNCTION)                           M0401770
 310  FORMAT(A6,A2,I2,2I3,4I2,46X,I2)                                   M0401780
 313  FORMAT(49H0 VARIABLE MEANS BY GROUP AND DIFFERENCE IN MEANS)      M0401790
 314  FORMAT(I5,2X,3F16.5)                                              M0401800
 317  FORMAT(1H0//15H  SELECTION NO.I4)                                 M0401810
 319  FORMAT(I7,I14,F17.5,2F20.5)                                       M0401820
 320  FORMAT(22H0 MAHALANOBIS DSQUARE=F16.5)                            M0401830
 321  FORMAT(57H  VARIABLE      MEAN 1          MEAN 2         DIFFERENCM0401840
     1E)                                                                M0401850
 322  FORMAT(79H0 POP. NO.    SAMPLE SIZE      MEAN Z            VARIANCM0401860
     1E Z         STD. DEV. Z)                                          M0401870
 323  FORMAT(47H0 INVERSE OF SUM OF PRODUCTS OF DEV. FROM MEANS)        M0401880
 324  FORMAT(66H  RANK       VALUES           VALUES         ITEM NO.   M0401890
     1  ITEM NO.)                                                       M0401900
 325  FORMAT(1H I4,17X,F17.5,12X,I13)                                   M0401910
 339  FORMAT(15H0 PROBLEM NO.  A2/21H  NUMBER OF VARIABLESI4)           M0401920
 340  FORMAT(A6,26I2)                                                   M0401930
 341  FORMAT(4H0 F(I2,1H,I3,2H)=F16.5)                                  M0401940
 344  FORMAT(12A6)                                                      M0401950
 345  FORMAT(37H0ERROR ON PROBLEM CARD OR DECK SET-UP)                  M0401960
 346  FORMAT(40H0 ERROR ON SELECTION CARD OR DECK SET-UPI4)             M0401970
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFM0401980
     1IED, ASSUMED TO BE 1.)                                            M0401990
 100  IF(MTAPE-5)102,102,101                                            M0402000
  101 REWIND MTAPE                                                      M0402010
  102 STOP                                                              M0402020
      END                                                               M0402030
CIBFTC INVER   LIST,M94,XR7                                             M0402040
C             SUBROUTINE INVERT FOR BMD04M            JUNE  9, 1966     M0402050
      SUBROUTINE INVERT (A,N,D,L,M)                                     M0402060
C     PROGRAM FOR FINDING THE INVERSE OF A NXN MATRIX                   M0402070
      DIMENSION A(25,25),L(25),M(25)                                    M0402080
C     SEARCH FOR LARGEST ELEMENT                                        M0402090
      D=1.0                                                             M0402100
      DO80 K=1,N                                                        M0402110
      L(K)=K                                                            M0402120
      M(K)=K                                                            M0402130
      BIGA=A(K,K)                                                       M0402140
      DO20 I=K,N                                                        M0402150
      DO20 J=K,N                                                        M0402160
      IF(ABS(BIGA)-ABS(A(I,J))) 10,20,20                                M0402170
   10 BIGA=A(I,J)                                                       M0402180
      L(K)=I                                                            M0402190
      M(K)=J                                                            M0402200
   20 CONTINUE                                                          M0402210
C     INTERCHANGE ROWS                                                  M0402220
      J=L(K)                                                            M0402230
      IF(L(K)-K) 35,35,25                                               M0402240
   25 DO30 I=1,N                                                        M0402250
      HOLD=-A(K,I)                                                      M0402260
      A(K,I)=A(J,I)                                                     M0402270
   30 A(J,I)=HOLD                                                       M0402280
C     INTERCHANGE COLUMNS                                               M0402290
   35 I=M(K)                                                            M0402300
      IF(M(K)-K) 45,45,37                                               M0402310
   37 DO40 J=1,N                                                        M0402320
      HOLD=-A(J,K)                                                      M0402330
      A(J,K)=A(J,I)                                                     M0402340
   40 A(J,I)=HOLD                                                       M0402350
C     DIVIDE COLUMN BY MINUS PIVOT                                      M0402360
   45 DO55 I=1,N                                                        M0402370
   46 IF(I-K)50,55,50                                                   M0402380
   50 A(I,K)=A(I,K)/(-A(K,K))                                           M0402390
   55 CONTINUE                                                          M0402400
C     REDUCE MATRIX                                                     M0402410
      DO65 I=1,N                                                        M0402420
      DO65 J=1,N                                                        M0402430
   56 IF(I-K) 57,65,57                                                  M0402440
   57 IF(J-K) 60,65,60                                                  M0402450
   60 A(I,J)=A(I,K)*A(K,J)+A(I,J)                                       M0402460
   65 CONTINUE                                                          M0402470
C     DIVIDE ROW BY PIVOT                                               M0402480
      DO75 J=1,N                                                        M0402490
   68 IF(J-K)70,75,70                                                   M0402500
   70 A(K,J)=A(K,J)/A(K,K)                                              M0402510
   75 CONTINUE                                                          M0402520
C     CONTINUED PRODUCT OF PIVOTS                                       M0402530
      D=D*A(K,K)                                                        M0402540
C     REPLACE PIVOT BY RECIPROCAL                                       M0402550
      A(K,K)=1.0/A(K,K)                                                 M0402560
   80 CONTINUE                                                          M0402570
C     FINAL ROW AND COLUMN INTERCHANGE                                  M0402580
      K=N                                                               M0402590
  100 K=(K-1)                                                           M0402600
      IF(K) 150,150,103                                                 M0402610
  103 I=L(K)                                                            M0402620
      IF(I-K) 120,120,105                                               M0402630
  105 DO110 J=1,N                                                       M0402640
      HOLD=A(J,K)                                                       M0402650
      A(J,K)=-A(J,I)                                                    M0402660
  110 A(J,I)=HOLD                                                       M0402670
  120 J=M(K)                                                            M0402680
      IF(J-K) 100,100,125                                               M0402690
  125 DO130 I=1,N                                                       M0402700
      HOLD=A(K,I)                                                       M0402710
      A(K,I)=-A(J,I)                                                    M0402720
  130 A(J,I)=HOLD                                                       M0402730
      GO TO 100                                                         M0402740
  150 RETURN                                                            M0402750
      END                                                               M0402760
CIBFTC TPWDS   LIST,M94,XR7                                             M0402770
C             SUBROUTINE TPWD FOR BMD04M              JUNE  9, 1966     M0402780
      SUBROUTINE TPWD(NT1,NT2)                                          M0402790
      IF(NT1)40,10,12                                                   M0402800
 10   NT1=5                                                             M0402810
 12   IF(NT1-NT2)14,19,14                                               M0402820
   14 IF(NT2.EQ.5)GO TO 18                                              M0402830
   17 REWIND NT2                                                        M0402840
   19 IF(NT1-5)18,24,18                                                 M0402850
 18   IF(NT1-6)22,40,22                                                 M0402860
 22   REWIND NT1                                                        M0402870
 24   NT2=NT1                                                           M0402880
 28   RETURN                                                            M0402890
 40   WRITE (6,49)                                                      M0402900
       STOP                                                             M0402910
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              M0402920
      END                                                               M0402930
CIBFTC TRNGE   LIST,M94,XR7                                             M0402940
C             SUBROUTINE TRNGEN FOR BMD04M            JUNE  9, 1966     M0402950
      SUBROUTINE TRNGEN (DATA,NVAR,NSAM,IERROR,NGRP,NVG)                M0402960
      DIMENSION DATA(2,25,300),NSAM(2)                                  M0402970
      ASN(XX)=ATAN(XX/SQRT(1.0-XX**2))                                  M0402980
      DATA A2/6HTRNGEN/                                                 M0402990
      MARY=0                                                            M0403000
      WRITE (6,1403)                                                    M0403010
      WRITE (6,1400)                                                    M0403020
      DO1000J=1,NVG                                                     M0403030
      READ (5,1100)A1,NEWA,LCODE,LVA,BNEW                               M0403040
      IF(A1.EQ.A2)GO TO 250                                             M0403050
  200 WRITE (6,1401)J                                                   M0403060
      IERROR=-999                                                       M0403070
  250 WRITE (6,1402)J,NEWA,LCODE,LVA,BNEW                               M0403080
      IF(-IERROR)251,251,1000                                           M0403090
  251 IF(LCODE*(LCODE-15))255,500,500                                   M0403100
  255 IF(LCODE-10)4,5,5                                                 M0403110
    5 NEWB=BNEW                                                         M0403120
    4 DO3000JJ=1,NGRP                                                   M0403130
      NN=NSAM(JJ)                                                       M0403140
      DO 300 I=1,NN                                                     M0403150
      D=DATA(JJ,LVA,I)                                                  M0403160
      GOTO(10,20,30,40,50,60,70,80,90,100,110,120,130,140),LCODE        M0403170
   10 IF(D)99,7,8                                                       M0403180
    7 D2=0.0                                                            M0403190
      GOTO3                                                             M0403200
    8 D2=SQRT(D)                                                        M0403210
      GOTO3                                                             M0403220
   20 IF(D)99,11,12                                                     M0403230
   11 D2=1.0                                                            M0403240
      GOTO3                                                             M0403250
   12 D2=SQRT(D)+SQRT(D+1.0)                                            M0403260
      GOTO3                                                             M0403270
   30 IF(-D)14,99,99                                                    M0403280
   14 D2=ALOG10(D)                                                      M0403290
      GO TO 3                                                           M0403300
   40 D2=EXP(D)                                                         M0403310
      GOTO3                                                             M0403320
   50 IF(-D)17,7,99                                                     M0403330
   17 IF(D-1.0)18,19,99                                                 M0403340
   19 D2=3.14159265/2.0                                                 M0403350
      GOTO3                                                             M0403360
   18 D2=ASN(SQRT(D))                                                   M0403370
      GOTO3                                                             M0403380
   60 FN=NN                                                             M0403390
      A=D/(FN+1.0)                                                      M0403400
      B=A+1.0/(FN+1.0)                                                  M0403410
      IF(A)99,23,24                                                     M0403420
   23 IF(-B)27,7,99                                                     M0403430
   27 D2=ASN(SQRT(B))                                                   M0403440
      GOTO3                                                             M0403450
   24 IF(B)99,28,29                                                     M0403460
   28 D2=ASN(SQRT(A))                                                   M0403470
      GOTO3                                                             M0403480
   29 A=SQRT(A)                                                         M0403490
      B=SQRT(B)                                                         M0403500
      D2=ASN(A)+ASN(B)                                                  M0403510
      GOTO3                                                             M0403520
   70 IF(D)31,99,31                                                     M0403530
   31 D2=1.0/D                                                          M0403540
      GOTO3                                                             M0403550
   80 D2=D+BNEW                                                         M0403560
      GOTO3                                                             M0403570
   90 D2=D*BNEW                                                         M0403580
      GOTO3                                                             M0403590
  100 IF(-D)33,7,99                                                     M0403600
   33 D2=D**NEWB                                                        M0403610
      GOTO3                                                             M0403620
  110 D2=D+DATA(JJ,NEWB,I)                                              M0403630
      GOTO3                                                             M0403640
  120 D2=D-DATA(JJ,NEWB,I)                                              M0403650
      GOTO3                                                             M0403660
  130 D2=D*DATA(JJ,NEWB,I)                                              M0403670
      GOTO3                                                             M0403680
  140 IF(DATA(JJ,NEWB,I))34,99,34                                       M0403690
   34 D2=D/DATA(JJ,NEWB,I)                                              M0403700
      GOTO3                                                             M0403710
   99 IF(MARY)43,44,44                                                  M0403720
   44 MARY=-999                                                         M0403730
      IERROR=-999                                                       M0403740
      WRITE (6,1404)J                                                   M0403750
   43 WRITE (6,1405)I                                                   M0403760
      GO TO 300                                                         M0403770
    3 DATA(JJ,NEWA,I)=D2                                                M0403780
  300 CONTINUE                                                          M0403790
      IF(IERROR)45,3000,3000                                            M0403800
   45 WRITE (6,1406)JJ                                                  M0403810
 3000 CONTINUE                                                          M0403820
      IF(IERROR)42,1000,1000                                            M0403830
  500 WRITE (6,1408)NEWA                                                M0403840
 1000 CONTINUE                                                          M0403850
      IF(IERROR) 42,1111,1111                                           M0403860
   42 WRITE (6,1407)                                                    M0403870
 1100 FORMAT(A6,I3,I2,I3,F6.0)                                          M0403880
 1400 FORMAT(46H0CARD    NEW     TRANS    ORIG.   ORIG. VAR(B)/45H  NO. M0403890
     1VARIABLE   CODE    VAR(A)   OR CONSTANT)                          M0403900
 1401 FORMAT(30H0ERROR ON TRANSGENERATION CARDI4)                       M0403910
 1402 FORMAT(2H  I2,I8,2I9,F15.5)                                       M0403920
 1403 FORMAT(1H06X,21HTRANSGENERATION CARDS)                            M0403930
 1404 FORMAT(30H0THE INSTRUCTIONS INDICATED ON 25H TRANSGENERATION CARD M0403940
     1NO.I2,4H RE-/29H SULTED IN THE VIOLATION OF A 31H RESTRICTION FOR M0403950
     2THIS TRANSFOR-/31H MATION. THE VIOLATION OCCURED 29H FOR THE ITEM M0403960
     3S LISTED BELOW./)                                                 M0403970
 1405 FORMAT(10H ITEM NO. I3)                                           M0403980
 1406 FORMAT(32H0ITEMS ABOVE ARE FROM GROUP NO. I2//)                   M0403990
 1407 FORMAT(41H0PROGRAM CANNOT CONTINUE FOR THIS PROBLEM)              M0404000
 1408 FORMAT(1H0,35X,95HILLEGAL TRANSGENERATION CODE SPECIFIED ON PRECEDM0404010
     1ING CARD. PROGRAM WILL PROCEED LEAVING VARIABLE,I3,11H UNCHANGED.)M0404020
 1111 RETURN                                                            M0404030
      END                                                               M0404040
