CID                     BMD05M                                                  
CID   0901HS 15 150    $BMD05M DISCRIMINANT ANALYSIS FOR SEVERAL GROUPS M0500000
CIBJOB         ALTIO                                                    M0500010
CIBFTC D05M    LIST,M94,XR7                                             M0500020
C            DISCRIMINANT ANALYSIS - SEVERAL GROUPS   JUNE  9, 1966     M0500030
C        THIS IS A SIFTED VERSION OF BMD05M ORIGINALLY WRITTEN IN       M0500040
C        FORTRAN II. SOME MODIFICATIONS WERE MADE TO MAKE IT OPERABLE   M0500050
C        AND SLIGHTLY MORE EFFICIENT THAN THE SIFTED VERSION.           M0500060
      DIMENSION X(5,20,175),XMEAN(5,20),COV(20,20),SUMCOV(20,20),       M0500070
     1DENT(20,20),LB(20),M(20),N(5),C(20,20),NDENT(5,6)                 M0500080
      COMMON  K      , NN     , N      , LA     , KP     , X            M0500090
      COMMON  ONN    , XMEAN  , NTO    , M      , K1     , K2           M0500100
      DATA  A2,XZ/6HPROBLM,6HFINISH/                                    M0500110
  946 FORMAT(60H1BMD05M - DISCRIMINANT ANALYSIS-SEVERAL GROUPS - VERSIONM0500120
     1 OF ,18HJUNE  9, 1966     ,/                                      M0500130
     241H HEALTH SCIENCES COMPUTING FACILITY, UCLA)                     M0500140
C                                                                       M0500150
      NTAPE=5                                                           M0500160
 100  READ (5,900)A1,PROB,K,KP,NVG,NADD,MTAPE,(N(I),I=1,5), KVR         M0500170
      IF(A1  .EQ.  A2)     GO TO 15                                     M0500180
    5 IF(A1  .EQ.  XZ)   GO TO 80                                       M0500190
 10   WRITE (6,948)                                                     M0500200
      GO TO 80                                                          M0500210
   15 CALL TPWD(MTAPE,NTAPE)                                            M0500220
 21   JESSE=KP+NADD                                                     M0500230
      IF(KVR.GT.0.AND.KVR.LE.10)GO TO 210                               M0500240
      KVR=1                                                             M0500250
      WRITE (6,4000)                                                    M0500260
  210 WRITE (6,946)                                                     M0500270
      WRITE (6,929)PROB,JESSE                                           M0500280
      IF((K-1)*(K-6))20,10,10                                           M0500290
  20  IF((KP-K+1)*(KP-21))22,10,10                                      M0500300
  22  DO 23 I=1,K                                                       M0500310
      IF(N(I)-175)23,23,10                                              M0500320
  23  CONTINUE                                                          M0500330
      IF((JESSE-K+1)*(JESSE-21))25,10,10                                M0500340
  25  CALL READ(KVR,NVG,NTAPE,NADD)                                     M0500350
      KP=JESSE                                                          M0500360
      IF(-NVG)81,81,100                                                 M0500370
 81   DO 24 I=1,KP                                                      M0500380
      DO 24 J=1,KP                                                      M0500390
 24   SUMCOV(I,J)=0.0                                                   M0500400
      DO 30 I=1,K                                                       M0500410
      NN=N(I)                                                           M0500420
      DO 26 L=1,KP                                                      M0500430
      DO 26 LL=1,KP                                                     M0500440
      COV(L,LL)=0.0                                                     M0500450
      DO 26 J=1,NN                                                      M0500460
 26   COV(L,LL)=COV(L,LL)+((X(I,L,J)-XMEAN(I,L))*(X(I,LL,J)-XMEAN(I,LL))M0500470
     1)                                                                 M0500480
      DO 28 L=1,KP                                                      M0500490
      DO 28 LL=1,KP                                                     M0500500
 28   SUMCOV(L,LL)=SUMCOV(L,LL)+COV(L,LL)                               M0500510
      WRITE (6,930)                                                     M0500520
      WRITE (6,905)I                                                    M0500530
      DO 30 J=1,KP                                                      M0500540
      WRITE (6,906)J                                                    M0500550
 30   WRITE (6,907)(COV(J,L),L=1,KP)                                    M0500560
      DF=0.0                                                            M0500570
      DO 37 I=1,K                                                       M0500580
      DFF=N(I)                                                          M0500590
 37   DF=DF+DFF                                                         M0500600
      OK=K                                                              M0500610
      DF=DF-OK                                                          M0500620
      DO 38 I=1,KP                                                      M0500630
      DO 38 J=1,KP                                                      M0500640
      SUMCOV(I,J)=SUMCOV(I,J)/DF                                        M0500650
 38   COV(I,J)=SUMCOV(I,J)                                              M0500660
      WRITE (6,908)                                                     M0500670
      DO 39 I=1,KP                                                      M0500680
      WRITE (6,906)I                                                    M0500690
 39   WRITE (6,907)(SUMCOV(I,J),J=1,KP)                                 M0500700
      CALL INVERT(COV,KP,D,LB,M)                                        M0500710
      WRITE (6,930)                                                     M0500720
      WRITE (6,909)                                                     M0500730
      DO 40 I=1,KP                                                      M0500740
      WRITE (6,906)I                                                    M0500750
 40   WRITE (6,907)(COV(I,J),J=1,KP)                                    M0500760
      DO 41 I=1,KP                                                      M0500770
      DO 41 J=1,KP                                                      M0500780
      C(I,J)=0.0                                                        M0500790
      DO 41 II=1,KP                                                     M0500800
 41   C(I,J)=C(I,J)+SUMCOV(I,II)*COV(II,J)                              M0500810
      WRITE (6,930)                                                     M0500820
      WRITE (6,910)                                                     M0500830
      DO 42 I=1,KP                                                      M0500840
      WRITE (6,906)I                                                    M0500850
 42   WRITE (6,907)(C(I,J),J=1,KP)                                      M0500860
      CALL CHITES(N,COV,XMEAN,KP,K)                                     M0500870
      DO 43 I=1,K                                                       M0500880
      DO 43 J=1,KP                                                      M0500890
      SUMCOV(I,J)=0.0                                                   M0500900
      DO 43 II=1,KP                                                     M0500910
 43   SUMCOV(I,J)=SUMCOV(I,J)+XMEAN(I,II)*COV(II,J)                     M0500920
      DO 44 I=1,K                                                       M0500930
      DO 44 J=1,KP                                                      M0500940
 44   DENT(J,I)=XMEAN(I,J)*(-0.5)                                       M0500950
      DO 47 I=1,K                                                       M0500960
      DO 47 J=1,K                                                       M0500970
      C(I,J)=0.0                                                        M0500980
      DO 47 II=1,KP                                                     M0500990
 47   C(I,J)=C(I,J)+SUMCOV(I,II)*DENT(II,J)                             M0501000
      WRITE (6,930)                                                     M0501010
      DO 45 I=1,K                                                       M0501020
 45   M(I)=I                                                            M0501030
      WRITE (6,911)(M(I),I=1,K)                                         M0501040
      WRITE (6,912)                                                     M0501050
      DO 50 I=1,KP                                                      M0501060
 50   WRITE (6,913)I,(SUMCOV(J,I),J=1,K)                                M0501070
      WRITE (6,930)                                                     M0501080
      DO 48 I=1,K                                                       M0501090
      J=I                                                               M0501100
 48   C(20,I)=C(I,J)                                                    M0501110
      WRITE (6,914)(C(20,I), I=1,K)                                     M0501120
      DO 52 II=1,K                                                      M0501130
      NN=N(II)                                                          M0501140
      DO 52 I=1,NN                                                      M0501150
      DO 49 J=1,K                                                       M0501160
      DENT(1,J)=0.0                                                     M0501170
      DO 49 I1=1,KP                                                     M0501180
 49   DENT(1,J)=DENT(1,J)+X(II,I1,I)*SUMCOV(J,I1)                       M0501190
      DO 52 J=1,K                                                       M0501200
   52 X(II,J,I)=DENT(1,J)+C(20,J)                                       M0501210
      SMALL=-(10.0**36.0)                                               M0501220
      K1=K+1                                                            M0501230
      K2=K+2                                                            M0501240
      DO 55 I=1,K                                                       M0501250
      NN=N(I)                                                           M0501260
      DO 55 J=1,NN                                                      M0501270
      X(I,K1,J)=SMALL                                                   M0501280
      DO 55 JJ=1,K                                                      M0501290
      IF(X(I,K1,J)-X(I,JJ,J))54,55,55                                   M0501300
 54   X(I,K1,J)=X(I,JJ,J)                                               M0501310
      X(I,K2,J)=JJ                                                      M0501320
 55   CONTINUE                                                          M0501330
      DO 57 I=1,K                                                       M0501340
      NN=N(I)                                                           M0501350
      DO 57 J=1,NN                                                      M0501360
      SUM=0.0                                                           M0501370
      DO 56 JJ=1,K                                                      M0501380
      X(I,JJ,J)=EXP(X(I,JJ,J)-X(I,K1,J))                                M0501390
   56 SUM=SUM+X(I,JJ,J)                                                 M0501400
      X(I,K1,J)=1.0                                                     M0501410
      DO 57 JJ=1,K1                                                     M0501420
   57 X(I,JJ,J)=X(I,JJ,J)/SUM                                           M0501430
      WRITE (6,930)                                                     M0501440
      CALL CLASSI                                                       M0501450
      DO 70 I=1,K                                                       M0501460
      NN=N(I)                                                           M0501470
      DO 70 II=1,K                                                      M0501480
      NDENT(I,II)=0                                                     M0501490
      DO 70 J=1,NN                                                      M0501500
      NSUM=0                                                            M0501510
      DO 68 JJ=1,K                                                      M0501520
      IF(X(I,II,J)-X(I,JJ,J))68,68,67                                   M0501530
 67   NSUM=NSUM+1                                                       M0501540
 68   CONTINUE                                                          M0501550
      IF(NSUM-(K-1)) 70, 69, 70                                         M0501560
 69   NDENT(I,II)=NDENT(I,II)+1                                         M0501570
 70   CONTINUE                                                          M0501580
      WRITE (6,930)                                                     M0501590
      WRITE (6,924)                                                     M0501600
      GO TO (91,92,93,94,95),K                                          M0501610
 91   WRITE (6,941)(M(I),I=1,K)                                         M0501620
      GO TO 96                                                          M0501630
 92   WRITE (6,942)(M(I),I=1,K)                                         M0501640
      GO TO 96                                                          M0501650
 93   WRITE (6,943)(M(I),I=1,K)                                         M0501660
      GO TO 96                                                          M0501670
 94   WRITE (6,944)(M(I),I=1,K)                                         M0501680
      GO TO 96                                                          M0501690
 95   WRITE (6,945)(M(I),I=1,K)                                         M0501700
 96   WRITE (6,926)                                                     M0501710
      DO 75 I=1,K                                                       M0501720
      NDENT(I,K1)=0                                                     M0501730
      DO 72 J=1,K                                                       M0501740
 72   NDENT(I,K1)=NDENT(I,K1)+NDENT(I,J)                                M0501750
      WRITE (6,927)I,(NDENT(I,J), J=1,K1)                               M0501760
 75   WRITE (6,931)                                                     M0501770
      GO TO 100                                                         M0501780
 900  FORMAT(A6,A2,5I2,5I3,37X,I2)                                      M0501790
 905  FORMAT(52H0MATRIX OF CROSS PRODUCTS OF DEV. FROM MEANS - GROUPI2) M0501800
 906  FORMAT(4H ROWI3)                                                  M0501810
 907  FORMAT(1H F14.5,7F15.5)                                           M0501820
 908  FORMAT(18H0DISPERSION MATRIX)                                     M0501830
 909  FORMAT(29H0INVERSE OF DISPERSION MATRIX)                          M0501840
 910  FORMAT(46H0TEST OF ACCURACY OF DISPERSION MATRIX INVERSE)         M0501850
 911  FORMAT(9H0FUNCTIONI13,4I16)                                       M0501860
 912  FORMAT(12H0COEFFICIENT)                                           M0501870
 913  FORMAT(5X,I2,F20.5,4F16.5)                                        M0501880
 914  FORMAT(9H0CONSTANTF18.5,4F16.5)                                   M0501890
 924  FORMAT(22H0CLASSIFICATION MATRIX)                                 M0501900
 926  FORMAT(6H0GROUP)                                                  M0501910
 927  FORMAT(1H I3,8X,6I11)                                             M0501920
 929  FORMAT(14H0PROBLEM NO.  A2/20H NUMBER OF VARIABLESI4///)          M0501930
 930  FORMAT(1H0)                                                       M0501940
 931  FORMAT(1H )                                                       M0501950
 941  FORMAT(9H0FUNCTIONI14,12H       TOTAL)                            M0501960
 942  FORMAT(9H0FUNCTIONI14, I11,12H       TOTAL)                       M0501970
 943  FORMAT(9H0FUNCTIONI14,2I11,12H       TOTAL)                       M0501980
 944  FORMAT(9H0FUNCTIONI14,3I11,12H       TOTAL)                       M0501990
 945  FORMAT(9H0FUNCTIONI14,4I11,12H       TOTAL)                       M0502000
 948  FORMAT(37H0ERROR ON PROBLEM CARD OR DECK SET-UP)                  M0502010
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFM0502020
     1IED, ASSUMED TO BE 1.)                                            M0502030
 80   IF(NTAPE-5)83,83,82                                               M0502040
   82 REWIND NTAPE                                                      M0502050
   83 STOP                                                              M0502060
      END                                                               M0502070
CIBFTC CHITE   LIST,M94,XR7                                             M0502080
C             SUBROUTINE CHITES FOR BMD05M            JUNE  9, 1966     M0502090
      SUBROUTINE CHITES(N,COV,XMEAN,KP,K)                               M0502100
      DIMENSION XBAR(20),XMEAN(5,20),COV(20,20),N(5)                    M0502110
      DFSUM=0.0                                                         M0502120
      CHI=0.0                                                           M0502130
      DO 2 I=1,KP                                                       M0502140
    2 XBAR(I)=0.0                                                       M0502150
      DO 3 I=1,K                                                        M0502160
      XN=N(I)                                                           M0502170
    3 DFSUM=DFSUM+XN                                                    M0502180
      DO 4 I=1,KP                                                       M0502190
      DO 5 J=1,K                                                        M0502200
      TEMP=N(J)                                                         M0502210
    5 XBAR(I)=XBAR(I)+TEMP*XMEAN(J,I)                                   M0502220
    4 XBAR(I)=XBAR(I)/DFSUM                                             M0502230
      DO 6 I=1,KP                                                       M0502240
      DO 6 J=1,KP                                                       M0502250
      TEMP=0.0                                                          M0502260
      DO 7 L=1,K                                                        M0502270
      DNN=N(L)                                                          M0502280
    7 TEMP=TEMP+DNN*(XMEAN(L,I)-XBAR(I))*(XMEAN(L,J)-XBAR(J))           M0502290
    6 CHI=CHI+TEMP*COV(I,J)                                             M0502300
      NDF=KP*(K-1)                                                      M0502310
      WRITE (6,9)CHI                                                    M0502320
      WRITE (6,8)CHI,NDF,K,KP                                           M0502330
    8 FORMAT(11H0THE VALUE F10.5,31H CAN BE USED AS CHI-SQUARE WITHI3,/5M0502340
     16H DEGREES OF FREEDOM TO TEST THE HYPOTHESIS THAT THE MEAN/32H VALM0502350
     2UES ARE THE SAME IN ALL THE I3,18H GROUPS FOR THESE I2,/11H VARIABM0502360
     3LES.//)                                                           M0502370
    9 FORMAT(1H0/33H GENERALIZED MAHALANOBIS D-SQUAREF12.5)             M0502380
      RETURN                                                            M0502390
      END                                                               M0502400
CIBFTC CLASS   LIST,M94,XR7                                             M0502410
C             SUBROUTINE CLASSI FOR BMD05M            JUNE  9, 1966     M0502420
      SUBROUTINE CLASSI                                                 M0502430
      DIMENSION N(5),X(5,20,175),XMEAN(5,20),M(20)                      M0502440
      COMMON  K      , NN     , N      , LA     , KP     , X            M0502450
      COMMON  ONN    , XMEAN  , NTO    , M      , K1     , K2           M0502460
      WRITE (6,915)                                                     M0502470
      GO TO (81,82,83,84,85),K                                          M0502480
 81   WRITE (6,931)(M(II),II=1,K)                                       M0502490
      GO TO 86                                                          M0502500
 82   WRITE (6,932)(M(II),II=1,K)                                       M0502510
      GO TO 86                                                          M0502520
 83   WRITE (6,933)(M(II),II=1,K)                                       M0502530
      GO TO 86                                                          M0502540
 84   WRITE (6,934)(M(II),II=1,K)                                       M0502550
      GO TO 86                                                          M0502560
 85   WRITE (6,935)(M(II),II=1,K)                                       M0502570
 86   DO 66 I=1,K                                                       M0502580
      WRITE (6,917)I                                                    M0502590
      WRITE (6,918)                                                     M0502600
      NN=N(I)                                                           M0502610
      DO 66 J=1,NN                                                      M0502620
      NX=X(I,K2,J)                                                      M0502630
      GO TO (61,62,63,64,65),K                                          M0502640
 61   WRITE (6,919)J,(X(I,JJ,J),JJ=1,K1),NX                             M0502650
      GO TO 66                                                          M0502660
 62   WRITE (6,920)J,(X(I,JJ,J),JJ=1,K1),NX                             M0502670
      GO TO 66                                                          M0502680
 63   WRITE (6,921)J,(X(I,JJ,J),JJ=1,K1),NX                             M0502690
      GO TO 66                                                          M0502700
 64   WRITE (6,922)J,(X(I,JJ,J),JJ=1,K1),NX                             M0502710
      GO TO 66                                                          M0502720
 65   WRITE (6,923)J,(X(I,JJ,J),JJ=1,K1),NX                             M0502730
 66   CONTINUE                                                          M0502740
 915  FORMAT(53H0EVALUATION OF CLASSIFICATION FUNCTIONS FOR EACH CASE)  M0502750
  917 FORMAT(6H GROUPI3)                                                M0502760
 918  FORMAT(6H  CASE)                                                  M0502770
  919 FORMAT(I5,F21.5,F15.5,I12)                                        M0502780
  920 FORMAT(I5,F21.5,2F15.5,I12)                                       M0502790
  921 FORMAT(I5,F21.5,3F15.5,I12)                                       M0502800
  922 FORMAT(I5,F21.5,4F15.5,I12)                                       M0502810
  923 FORMAT(I5,F21.5,5F15.5,I12)                                       M0502820
  931 FORMAT(9H0FUNCTIONI12, 41H            LARGEST     FN.NO.FOR LARGESM0502830
     1T/32X,27HPROBABILITY     PROBABILITY)                             M0502840
  932 FORMAT(9H0FUNCTIONI12, I15,41H            LARGEST     FN.NO.FOR LAM0502850
     1RGEST/47X,27HPROBABILITY     PROBABILITY)                         M0502860
  933 FORMAT(9H0FUNCTIONI12,2I15,41H            LARGEST     FN.NO.FOR LAM0502870
     1RGEST/62X,27HPROBABILITY     PROBABILITY)                         M0502880
  934 FORMAT(9H0FUNCTIONI12,3I15,41H            LARGEST     FN.NO.FOR LAM0502890
     1RGEST/77X,27HPROBABILITY     PROBABILITY)                         M0502900
  935 FORMAT(9H0FUNCTIONI12,4I15,39H            LARGEST   FN.NO.FOR LARGM0502910
     1EST/92X,27HPROBABILITY     PROBABILITY)                           M0502920
      RETURN                                                            M0502930
      END                                                               M0502940
CIBFTC INVER   LIST,M94,XR7                                             M0502950
C             SUBROUTINE INVERT FOR BMD05M            JUNE  9, 1966     M0502960
      SUBROUTINE INVERT (A,N,D,L,M)                                     M0502970
C     PROGRAM FOR FINDING THE INVERSE OF A NXN MATRIX                   M0502980
      DIMENSION A(20,20),L(20),M(20)                                    M0502990
C     SEARCH FOR LARGEST ELEMENT                                        M0503000
      D=1.0                                                             M0503010
      DO80 K=1,N                                                        M0503020
      L(K)=K                                                            M0503030
      M(K)=K                                                            M0503040
      BIGA=A(K,K)                                                       M0503050
      DO20 I=K,N                                                        M0503060
      DO20 J=K,N                                                        M0503070
      IF(ABS(BIGA)-ABS(A(I,J))) 10,20,20                                M0503080
   10 BIGA=A(I,J)                                                       M0503090
      L(K)=I                                                            M0503100
      M(K)=J                                                            M0503110
   20 CONTINUE                                                          M0503120
C     INTERCHANGE ROWS                                                  M0503130
      J=L(K)                                                            M0503140
      IF(L(K)-K) 35,35,25                                               M0503150
   25 DO30 I=1,N                                                        M0503160
      HOLD=-A(K,I)                                                      M0503170
      A(K,I)=A(J,I)                                                     M0503180
   30 A(J,I)=HOLD                                                       M0503190
C     INTERCHANGE COLUMNS                                               M0503200
   35 I=M(K)                                                            M0503210
      IF(M(K)-K) 45,45,37                                               M0503220
   37 DO40 J=1,N                                                        M0503230
      HOLD=-A(J,K)                                                      M0503240
      A(J,K)=A(J,I)                                                     M0503250
   40 A(J,I)=HOLD                                                       M0503260
C     DIVIDE COLUMN BY MINUS PIVOT                                      M0503270
   45 DO55 I=1,N                                                        M0503280
   46 IF(I-K)50,55,50                                                   M0503290
   50 A(I,K)=A(I,K)/(-A(K,K))                                           M0503300
   55 CONTINUE                                                          M0503310
C     REDUCE MATRIX                                                     M0503320
      DO65 I=1,N                                                        M0503330
      DO65 J=1,N                                                        M0503340
   56 IF(I-K) 57,65,57                                                  M0503350
   57 IF(J-K) 60,65,60                                                  M0503360
   60 A(I,J)=A(I,K)*A(K,J)+A(I,J)                                       M0503370
   65 CONTINUE                                                          M0503380
C     DIVIDE ROW BY PIVOT                                               M0503390
      DO75 J=1,N                                                        M0503400
   68 IF(J-K)70,75,70                                                   M0503410
   70 A(K,J)=A(K,J)/A(K,K)                                              M0503420
   75 CONTINUE                                                          M0503430
C     CONTINUED PRODUCT OF PIVOTS                                       M0503440
      D=D*A(K,K)                                                        M0503450
C     REPLACE PIVOT BY RECIPROCAL                                       M0503460
      A(K,K)=1.0/A(K,K)                                                 M0503470
   80 CONTINUE                                                          M0503480
C     FINAL ROW AND COLUMN INTERCHANGE                                  M0503490
      K=N                                                               M0503500
  100 K=(K-1)                                                           M0503510
      IF(K) 150,150,103                                                 M0503520
  103 I=L(K)                                                            M0503530
      IF(I-K) 120,120,105                                               M0503540
  105 DO110 J=1,N                                                       M0503550
      HOLD=A(J,K)                                                       M0503560
      A(J,K)=-A(J,I)                                                    M0503570
  110 A(J,I)=HOLD                                                       M0503580
  120 J=M(K)                                                            M0503590
      IF(J-K) 100,100,125                                               M0503600
  125 DO130 I=1,N                                                       M0503610
      HOLD=A(K,I)                                                       M0503620
      A(K,I)=-A(J,I)                                                    M0503630
  130 A(J,I)=HOLD                                                       M0503640
      GO TO 100                                                         M0503650
  150 RETURN                                                            M0503660
      END                                                               M0503670
CIBFTC READS   LIST,M94,XR7                                             M0503680
C             SUBROUTINE READ FOR BMD05M              JUNE  9, 1966     M0503690
      SUBROUTINE READ (KVR,NVG,NTAPE,NADD)                              M0503700
      DIMENSIONN(5),X(5,20,175),XMEAN(5,20),M(20),FMT(120)              M0503710
      COMMON  K      , NN     , N      , LA     , KP     , X            M0503720
      COMMON  ONN    , XMEAN  , NTO    , M      , K1     , K2           M0503730
   11 KVR=KVR*12                                                        M0503740
      IERROR=0                                                          M0503750
      READ (5,937)(FMT(I), I=1,KVR)                                     M0503760
      DO 10 I=1,K                                                       M0503770
      NN=N(I)                                                           M0503780
      DO 10 J=1,NN                                                      M0503790
   10 READ (NTAPE,FMT)(X(I,L,J), L=1,KP)                                M0503800
      IF(NVG)60,60,70                                                   M0503810
   70 CALL TRNGEN (X,KP,N,IERROR,K,NVG)                                 M0503820
      IF(-IERROR)400,400,500                                            M0503830
  400 KP=KP+NADD                                                        M0503840
   60 DO21I=1,K                                                         M0503850
      NN=N(I)                                                           M0503860
      ONN=NN                                                            M0503870
      DO 21 L=1,KP                                                      M0503880
      XMEAN(I,L)=0.0                                                    M0503890
      DO 19 J=1,NN                                                      M0503900
   19 XMEAN(I,L)=XMEAN(I,L)+X(I,L,J)                                    M0503910
   21 XMEAN(I,L)=XMEAN(I,L)/ONN                                         M0503920
      NTO=0                                                             M0503930
      DO 22 I=1,K                                                       M0503940
      NTO=NTO+N(I)                                                      M0503950
   22 M(I)=I                                                            M0503960
      GO TO (121,122,123,124,125), K                                    M0503970
  121 WRITE (6,931)(M(I), I=1,K)                                        M0503980
      GO TO 129                                                         M0503990
  122 WRITE (6,932)(M(I), I=1,K)                                        M0504000
      GO TO 129                                                         M0504010
  123 WRITE (6,933)(M(I), I=1,K)                                        M0504020
      GO TO 129                                                         M0504030
  124 WRITE (6,934)(M(I), I=1,K)                                        M0504040
      GO TO 129                                                         M0504050
  125 WRITE (6,935)(M(I), I=1,K)                                        M0504060
  129 WRITE (6,902)(N(I),I=1,K),NTO                                     M0504070
      WRITE (6,903)                                                     M0504080
      DO 23 I=1,KP                                                      M0504090
   23 WRITE (6,904)I,(XMEAN(J,I),J=1,K)                                 M0504100
  902 FORMAT(7H0SAMPLEI17,5I16)                                         M0504110
  903 FORMAT(12H0MEAN SCORES)                                           M0504120
  904 FORMAT(I7,5H     5F16.5)                                          M0504130
  931 FORMAT(6H0GROUPI18,17H            TOTAL)                          M0504140
  932 FORMAT(6H0GROUPI18,I16,17H            TOTAL)                      M0504150
  933 FORMAT(6H0GROUPI18,2I16,17H            TOTAL)                     M0504160
  934 FORMAT(6H0GROUPI18,3I16,17H            TOTAL)                     M0504170
  935 FORMAT(6H0GROUPI18,4I16,17H            TOTAL)                     M0504180
  937 FORMAT(12A6)                                                      M0504190
  100 RETURN                                                            M0504200
  500 NVG=-175                                                          M0504210
      GO TO 100                                                         M0504220
      END                                                               M0504230
CIBFTC TPWDS   LIST,M94,XR7                                             M0504240
C             SUBROUTINE TPWD FOR BMD05M              JUNE  9, 1966     M0504250
      SUBROUTINE TPWD(NT1,NT2)                                          M0504260
      IF(NT1)40,10,12                                                   M0504270
 10   NT1=5                                                             M0504280
 12   IF(NT1-NT2)14,19,14                                               M0504290
   14 IF(NT2.EQ.5)GO TO 18                                              M0504300
   17 REWIND NT2                                                        M0504310
   19 IF(NT1-5)18,24,18                                                 M0504320
 18   IF(NT1-6)22,40,22                                                 M0504330
 22   REWIND NT1                                                        M0504340
 24   NT2=NT1                                                           M0504350
 28   RETURN                                                            M0504360
 40   WRITE (6,49)                                                      M0504370
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              M0504380
      STOP                                                              M0504390
      END                                                               M0504400
CIBFTC TRNGE   LIST,M94,XR7                                             M0504410
C             SUBROUTINE TRNGEN FOR BMD05M            JUNE  9, 1966     M0504420
      SUBROUTINE TRNGEN (DATA,NVAR,NSAM,IERROR,NGRP,NVG)                M0504430
      DIMENSION DATA(5,20,175),NSAM(5)                                  M0504440
      ASN(XX)=ATAN(XX/SQRT(1.0-XX**2))                                  M0504450
      DATA A2/6HTRNGEN/                                                 M0504460
      MARY=0                                                            M0504470
      WRITE (6,1403)                                                    M0504480
      WRITE (6,1400)                                                    M0504490
      DO1000J=1,NVG                                                     M0504500
      READ (5,1100)A1,NEWA,LCODE,LVA,BNEW                               M0504510
      IF(A1 .EQ. A2)    GO TO 2                                         M0504520
    1 WRITE (6,1408)J                                                   M0504530
      IERROR=-999                                                       M0504540
      GO TO 1000                                                        M0504550
    2 WRITE (6,1402)J,NEWA,LCODE,LVA,BNEW                               M0504560
      IF(LCODE*(LCODE-15))250,1,1                                       M0504570
  250 IF(LCODE-10)4,5,5                                                 M0504580
    5 NEWB=BNEW                                                         M0504590
    4 DO3000JJ=1,NGRP                                                   M0504600
      NN=NSAM(JJ)                                                       M0504610
      DO 300 I=1,NN                                                     M0504620
      D=DATA(JJ,LVA,I)                                                  M0504630
      GOTO(10,20,30,40,50,60,70,80,90,100,110,120,130,140),LCODE        M0504640
   10 IF(D)99,7,8                                                       M0504650
    7 D2=0.0                                                            M0504660
      GOTO3                                                             M0504670
    8 D2=SQRT(D)                                                        M0504680
      GOTO3                                                             M0504690
   20 IF(D)99,11,12                                                     M0504700
   11 D2=1.0                                                            M0504710
      GOTO3                                                             M0504720
   12 D2=SQRT(D)+SQRT(D+1.0)                                            M0504730
      GOTO3                                                             M0504740
   30 IF(D)99,99,14                                                     M0504750
   14 D2=ALOG10(D)                                                      M0504760
      GOTO3                                                             M0504770
   40 D2=EXP(D)                                                         M0504780
      GOTO3                                                             M0504790
   50 IF(-D)17,7,99                                                     M0504800
   17 IF(D-1.0)18,19,99                                                 M0504810
   19 D2=3.14159265/2.0                                                 M0504820
      GOTO3                                                             M0504830
   18 D2=ASN(SQRT(D))                                                   M0504840
      GOTO3                                                             M0504850
   60 FN=NN                                                             M0504860
      A=D/(FN+1.0)                                                      M0504870
      B=A+1.0/(FN+1.0)                                                  M0504880
      IF(A)99,23,24                                                     M0504890
   23 IF(-B)27,7,99                                                     M0504900
   27 D2=ASN(SQRT(B))                                                   M0504910
      GOTO3                                                             M0504920
   24 IF(B)99,28,29                                                     M0504930
   28 D2=ASN(SQRT(A))                                                   M0504940
      GOTO3                                                             M0504950
   29 A=SQRT(A)                                                         M0504960
      B=SQRT(B)                                                         M0504970
      D2=ASN(A)+ASN(B)                                                  M0504980
      GOTO3                                                             M0504990
   70 IF(D)31,99,31                                                     M0505000
   31 D2=1.0/D                                                          M0505010
      GOTO3                                                             M0505020
   80 D2=D+BNEW                                                         M0505030
      GOTO3                                                             M0505040
   90 D2=D*BNEW                                                         M0505050
      GOTO3                                                             M0505060
  100 IF(-D)33,7,99                                                     M0505070
   33 D2=D**NEWB                                                        M0505080
      GOTO3                                                             M0505090
  110 D2=D+DATA(JJ,NEWB,I)                                              M0505100
      GOTO3                                                             M0505110
  120 D2=D-DATA(JJ,NEWB,I)                                              M0505120
      GOTO3                                                             M0505130
  130 D2=D*DATA(JJ,NEWB,I)                                              M0505140
      GOTO3                                                             M0505150
  140 IF(DATA(JJ,NEWB,I))34,99,34                                       M0505160
   34 D2=D/DATA(JJ,NEWB,I)                                              M0505170
      GOTO3                                                             M0505180
   99 IF(MARY)43,44,44                                                  M0505190
   44 MARY=-999                                                         M0505200
      IERROR=-999                                                       M0505210
      WRITE (6,1404)J                                                   M0505220
   43 WRITE (6,1405)I                                                   M0505230
      GO TO 300                                                         M0505240
    3 DATA(JJ,NEWA,I)=D2                                                M0505250
  300 CONTINUE                                                          M0505260
      IF(IERROR)45,3000,3000                                            M0505270
   45 WRITE (6,1406)JJ                                                  M0505280
 3000 CONTINUE                                                          M0505290
      IF(IERROR)42,1000,1000                                            M0505300
 1000 CONTINUE                                                          M0505310
      IF(IERROR) 42,1111,1111                                           M0505320
   42 WRITE (6,1407)                                                    M0505330
 1100 FORMAT(A6,I3,I2,I3,F6.0)                                          M0505340
 1400 FORMAT(46H0CARD    NEW     TRANS    ORIG.   ORIG. VAR(B)/45H  NO. M0505350
     1VARIABLE   CODE    VAR(A)   OR CONSTANT)                          M0505360
 1402 FORMAT(2H  I2,I8,2I9,F15.5)                                       M0505370
 1403 FORMAT(1H06X,23HTRANS GENERATOR CARD(S))                          M0505380
 1404 FORMAT(30H0THE INSTRUCTIONS INDICATED ON/25H TRANS GENERATOR CARD M0505390
     1NO.I2,4H RE-/29H SULTED IN THE VIOLATION OF A/31H RESTRICTION FOR M0505400
     2THIS TRANSFOR-/31H MATION. THE VIOLATION OCCURRED/28H FOR THE ITEMM0505410
     3S LISTED BELOW./)                                                 M0505420
 1405 FORMAT(10H ITEM NO. I3)                                           M0505430
 1406 FORMAT(32H0ITEMS ABOVE ARE FROM GROUP NO. I2//)                   M0505440
 1407 FORMAT(41H0PROGRAM CANNOT CONTINUE FOR THIS PROBLEM)              M0505450
 1408 FORMAT(30H0ERROR ON TRANSGENERATION CARDI4)                       M0505460
 1111 RETURN                                                            M0505470
      END                                                               M0505480
