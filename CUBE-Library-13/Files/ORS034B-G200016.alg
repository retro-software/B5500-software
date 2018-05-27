 LABEL  0000000000XXXXXX0010000001
         BEGIN                                                          CORR   1
COMMENT       CORRELATION ANALYSIS                                      CORR   2
              CHARLES L. CLARK                                          CORR   3
              PROFESSIONAL SERVICES DIVISIONAL GROUP                    CORR   4
              BURROUGHS CORPORATION                                     CORR   5
              PROGRAM CONTAINS 139 CARDS                                CORR   6
              CARD SEQUENCE STARTS WITH CORR   1                        CORR   7
              FIRST RELEASE DATE JULY 25, 1964  ;                       CORR   8
INTEGER       I,J,NVAR,NSAM ;                                           CORR   9
INTEGER ARRAY ID[0:4] ;                                                 CORR  10
BOOLEAN       SUM,SSQ,VAR ;                                             CORR  11
LABEL         START,LL1,NOMO ;                                          CORR  12
FORMAT IN     FORM1(5A6,I3,I5,3L5),                                     CORR  13
              FORM2(12F6.1) ;                                           CORR  14
FORMAT OUT    TITLE(X50,"CORRELATION ANALYSIS"///X45,5A6//              CORR  15
                    X1,"NUMBER OF VARIABLES =",I5/                      CORR  16
                    X1,"NUMBER OF REPLICATIONS =",I6//),                CORR  17
              FRM3 (//X1,"SUMS"/),                                      CORR  18
              FRM4 ((8F14.4)/),                                         CORR  19
              FRM5 (//X1,"MEANS"/),                                     CORR  20
              FRM6 (//X1,"CROSS PRODUCT DEVIATIONS"),                   CORR  21
              FRM7 (//X8,"COL.",7(X10,"COL.")/X8,I3,7(X11,I3)/X1,"ROW"),CORR  22
              FRM8 (//X1,"STANDARD DEVIATIONS"/),                       CORR  23
              FRM9 (//X1,"VARIANCE-COVARIANCE MATRIX"),                 CORR  24
              FRM10(//X1,"CORRELATION MATRIX"),                         CORR  25
              FRM11(I4,F11.4,7F14.4) ;                                  CORR  26
FILE IN       CARDR(1,10) ;                                             CORR  27
FILE OUT      PRINT 1 (1,15) ;                                          CORR  28
START:   BEGIN                                                          CORR  29
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NVAR,NSAM,SUM,SSQ, CORR  30
                   VAR) ;                                               CORR  31
              READ (CARDR,FORM1,HEAD) [NOMO]                            CORR  32
         END ;                                                          CORR  33
LL1:     BEGIN                                                          CORR  34
INTEGER       K,L,M,II,JJ,KK,LL ;                                       CORR  35
INTEGER ARRAY NN[0:8] ;                                                 CORR  36
ARRAY         X[0:NVAR],SX[0:NVAR],SXX[0:NVAR],SXY[0:NVAR,0:NVAR] ;     CORR  37
LABEL         L1,L2,LYST ;                                              CORR  38
LIST          OUTT (FOR J ~ 0 STEP 1 UNTIL 4 DO ID[J],NVAR,NSAM),       CORR  39
              DATR (FOR I ~ 1 STEP 1 UNTIL NVAR DO X[I]),               CORR  40
              SUMS (FOR J ~ 1 STEP 1 UNTIL NVAR DO SX[J]),              CORR  41
              CAPS (NN[1],FOR JJ ~ 2 STEP 1 UNTIL LL DO NN[JJ]),        CORR  42
              ARAY (II,SXY[II,K],FOR JJ ~ K+1 STEP 1 UNTIL KK DO        CORR  43
                    SXY[II,JJ]) ;                                       CORR  44
FILE          TAPEA (2,NVAR+1) ;                                        CORR  45
              WRITE(PRINT[PAGE]) ;                                      CORR  46
              WRITE(PRINT,TITLE,OUTT) ;                                 CORR  47
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            CORR  48
         BEGIN                                                          CORR  49
              SX[I] ~ 0.0 ;                                             CORR  50
              SXX[I] ~ 0.0 ;                                            CORR  51
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            CORR  52
              SXY[I,J] ~ 0.0                                            CORR  53
         END ;                                                          CORR  54
              FOR J ~ 1 STEP 1 UNTIL NSAM DO                            CORR  55
         BEGIN                                                          CORR  56
              READ (CARDR,FORM2,DATR) ;                                 CORR  57
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            CORR  58
         BEGIN                                                          CORR  59
              SX[I] ~ SX[I] + X[I]                                      CORR  60
         END ;                                                          CORR  61
              WRITE(TAPEA,NVAR+1,X[*])                                  CORR  62
         END ;                                                          CORR  63
              REWIND(TAPEA) ;                                           CORR  64
              IF SUM THEN                                               CORR  65
         BEGIN                                                          CORR  66
              WRITE(PRINT,FRM3) ;                                       CORR  67
              WRITE(PRINT,FRM4,SUMS)                                    CORR  68
         END ;                                                          CORR  69
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            CORR  70
              X[I] ~ SX[I] / NSAM ;                                     CORR  71
              WRITE(PRINT,FRM5) ;                                       CORR  72
              WRITE(PRINT,FRM4,DATR) ;                                  CORR  73
              FOR I ~ 1 STEP 1 UNTIL NSAM DO                            CORR  74
         BEGIN                                                          CORR  75
              READ (TAPEA,NVAR+1,SX[*]) ;                               CORR  76
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            CORR  77
         BEGIN                                                          CORR  78
              SX[J] ~ SX[J] - X[J] ;                                    CORR  79
              SXX[J] ~ SXX[J] + SX[J]|SX[J] ;                           CORR  80
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            CORR  81
              SXY[J,K] ~ SXY[J,K] + SX[J]|SX[K]                         CORR  82
         END                                                            CORR  83
         END ;                                                          CORR  84
              REWIND(TAPEA) ;                                           CORR  85
              IF SSQ THEN                                               CORR  86
         BEGIN                                                          CORR  87
              WRITE(PRINT,FRM6) ;                                       CORR  88
              M ~ 1 ;                                                   CORR  89
              GO TO LYST                                                CORR  90
         END ;                                                          CORR  91
L1:           WRITE(PRINT,FRM8) ;                                       CORR  92
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            CORR  93
              X[I] ~ SQRT(SXX[I] / (NSAM-1.0)) ;                        CORR  94
              WRITE(PRINT,FRM4,DATR) ;                                  CORR  95
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            CORR  96
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            CORR  97
              SXY[I,J] ~ SXY[I,J] / (NSAM - 1.0) ;                      CORR  98
              IF VAR THEN                                               CORR  99
         BEGIN                                                          CORR 100
              WRITE(PRINT,FRM9) ;                                       CORR 101
              M ~ 2 ;                                                   CORR 102
              GO TO LYST                                                CORR 103
         END ;                                                          CORR 104
L2:           WRITE(PRINT,FRM10) ;                                      CORR 105
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            CORR 106
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            CORR 107
              SXY[I,J] ~ SXY[I,J] / (X[I]|X[J]) ;                       CORR 108
              M ~ 3 ;                                                   CORR 109
LYST:         K ~ 1 ;                                                   CORR 110
              IF NVAR < 8 THEN                                          CORR 111
              KK ~ NVAR                                                 CORR 112
              ELSE                                                      CORR 113
              KK ~ 8 ;                                                  CORR 114
              I ~ 1 + (NVAR - 1) DIV 8 ;                                CORR 115
              FOR J ~ 1 STEP 1 UNTIL I DO                               CORR 116
         BEGIN                                                          CORR 117
              LL ~ KK - K + 1 ;                                         CORR 118
              L ~ 0 ;                                                   CORR 119
              FOR JJ ~ K STEP 1 UNTIL KK DO                             CORR 120
         BEGIN                                                          CORR 121
              L ~ L + 1 ;                                               CORR 122
              NN[L] ~ JJ                                                CORR 123
         END ;                                                          CORR 124
              WRITE(PRINT,FRM7,CAPS) ;                                  CORR 125
              FOR II ~ 1 STEP 1 UNTIL NVAR DO                           CORR 126
              WRITE(PRINT,FRM11,ARAY) ;                                 CORR 127
              K ~ KK + 1 ;                                              CORR 128
              KK ~ K + 7 ;                                              CORR 129
              IF KK > NVAR THEN                                         CORR 130
              KK ~ NVAR                                                 CORR 131
         END ;                                                          CORR 132
              IF M = 1 THEN                                             CORR 133
              GO TO L1                                                  CORR 134
              ELSE IF M = 2 THEN                                        CORR 135
              GO TO L2                                                  CORR 136
         END ;                                                          CORR 137
              GO TO START ;                                             CORR 138
NOMO:    END PROGRAM.                                                   CORR 139
 LABEL  000000000CARDR  0010000001
PROF. SERV. DEPT. PROB.   3333 24   15 TRUE TRUE TRUE
   4.9   4.9   3.2   8.3   3.7   4.7   3.3   0.6   3.7   0.3   4.7   4.8
   2.7   4.1   2.7   3.6  10.2   4.8   1.8   2.8   2.5   1.0   2.4   1.4
   0.6   0.4   0.5   0.6   2.5   0.7   2.7   7.4   9.0   7.7   6.2   5.5
   5.0   8.9  11.9  13.0  10.5   6.6   7.2   5.4  12.9  14.5   7.8  10.6
   6.6   6.2  12.1  10.1  10.0  11.8  10.2   8.4  13.4   9.8  17.8  14.0
   0.4   0.6   1.1   0.4   0.3   0.2   0.8   0.4   0.3   0.3   0.4   3.9
   3.0   3.6   0.9   0.8   1.1   1.6   1.5   1.4   2.8   2.8   2.3   2.2
   0.8   3.0   6.0   1.7   1.7   4.1  11.7  13.6  12.2  10.0  13.0  13.9
   6.2  10.3  14.1   9.3   8.7   8.8   7.6  11.5   9.4  12.9  14.0  13.9
   8.9   9.4  11.0   2.6  13.2  11.3  11.0  12.0   5.7   9.7   8.2  10.7
  22.4  19.1  11.8  14.4  12.5   8.7   3.3  10.6   6.8   7.4   7.4   6.5
   7.0   9.9   7.2   5.6   6.9  15.8  18.9  16.3  11.7  25.4  30.6   9.8
   9.0  13.2  10.6   9.6   9.5   9.8  21.9  39.1  53.3  69.3  76.7  84.0
  73.0  58.2  36.6  83.6 100.3  88.2 127.0  47.9  56.9  47.6  79.2  40.3
  37.6  33.9  37.6  56.9  40.9  28.6  27.4  78.4  50.4  68.2  34.3  76.3
  26.9  34.8  24.4  34.5  23.8  23.2  17.5  33.2  33.7  54.2  12.2  25.0
  15.5  18.5  20.1  16.8  25.1  26.1  22.9  36.5  30.7  19.8  26.1  30.5
  30.8  23.3  16.0  30.0  23.6  28.1  84.4  61.6  74.7  85.5 108.1 135.9
 138.5  97.2  63.9  97.8  84.5  71.5 110.0 102.0  58.0  72.3 121.0 133.8
 122.0  36.5 116.0 110.0 105.0 123.0  66.7 126.0 118.0  88.5  79.8 117.0
   3.4   3.3   2.9   2.9   2.6   1.2   1.1   1.4   1.9   2.2   1.1   1.6
   0.7   2.2   1.8   1.3   2.0   2.5   2.4   1.8   1.7   2.1   0.6   1.8
   2.4   3.1   2.9   2.4   3.0   3.2   4.0  26.7  27.0  22.8  17.6  17.4
  21.3  30.8  33.9  25.6  26.8  18.1  22.3  23.1   4.3  10.9  19.0  24.4
  16.8   7.7  20.0   5.5  10.5  13.4  29.4  14.6  26.0  16.6  24.2  12.2
   1.4   1.2   2.7   0.8   5.5   0.7   0.7   0.9   0.8   0.9   0.7   0.7
   0.9   0.6   0.9   0.5   0.8   0.6   1.2   1.0   0.9   1.8   1.1   1.1
   0.9   0.5   0.9   0.8   2.1   2.2  15.1  16.1  10.6  14.4  13.6   5.9
  14.5  11.1   9.8   6.7  16.6  12.3  14.0  11.2  14.2  14.4  13.7  15.2
  15.3  18.7   7.4  11.7  14.5  15.5  12.7  13.9  16.1  14.5  14.4  12.9
