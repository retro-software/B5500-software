 LABEL  0000000000XXXXXX0010000001
         BEGIN                                                          BSWO   1
COMMENT       BASIC STATISTICAL ROUTINE WITH OMISSION OPTIONS           BSWO   2
              CHARLES L. CLARK                                          BSWO   3
              PROFESSIONAL SERVICES DIVISIONAL GROUP                    BSWO   4
              BURROUGHS CORPORATION                                     BSWO   5
              PROGRAM CONTAINS 105 CARDS                                BSWO   6
              CARD SEQUENCE BEGINS WITH BSWO   1                        BSWO   7
              FIRST RELEASE DATE JULY 25, 1964  ;                       BSWO   8
INTEGER       I,J,L,NSAM,NVAR,NCAS,NCOD ;                               BSWO   9
INTEGER ARRAY ID[0:4] ;                                                 BSWO  10
LABEL         START,L1,NOMO ;                                           BSWO  11
FORMAT IN     FORM1 (5A6,I5,3I3),                                       BSWO  12
              FORM2 (12F6.1),                                           BSWO  13
              FORM3 (12I6) ;                                            BSWO  14
FORMAT OUT    TITLE(X45,"BASIC STATISTICAL VALUES"///X42,5A6////        BSWO  15
                    X8,"VAR NO",X6,"MEAN",X8,"S.D.",X4,"S.E. OF MEAN",  BSWO  16
                    X2,"SAMPLE",X6,"MAXIMUM",X5,"MINIMUM",X6,"RANGE"//),BSWO  17
              FRM1 (X9,I4,X6,"NO DATA FOR THIS VARIABLE"),              BSWO  18
              FRM2 (X9,I4,F13.4,2F12.4,I7,X4,3F12.4),                   BSWO  19
              FRM3 (X9,I4,X6,"ONLY ONE VALUE FOR THIS VARIABLE") ;      BSWO  20
FILE IN       CARDR(1,10) ;                                             BSWO  21
FILE OUT      PRINT 1 (1,15) ;                                          BSWO  22
START:   BEGIN                                                          BSWO  23
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NSAM,NVAR,NCAS,    BSWO  24
                   NCOD) ;                                              BSWO  25
              READ (CARDR,FORM1,HEAD) [NOMO]                            BSWO  26
         END ;                                                          BSWO  27
L1:      BEGIN                                                          BSWO  28
BOOLEAN       NOMIS ;                                                   BSWO  29
INTEGER ARRAY NX[0:NVAR],S[0:NVAR] ;                                    BSWO  30
REAL          DEN,RANG,W ;                                              BSWO  31
ARRAY         CODE[0:NCOD],SUMX[0:NVAR],SUMXX[0:NVAR],MINX[0:NVAR],     BSWO  32
              MAXX[0:NVAR],XBAR[0:NVAR],SC[0:NVAR],X[0:NVAR] ;          BSWO  33
LIST          SCAL(FOR I ~ 1 STEP 1 UNTIL NVAR DO S[I]),                BSWO  34
              MISS(FOR I ~ 1 STEP 1 UNTIL NCOD DO CODE[I]),             BSWO  35
              DATA(FOR J ~ 1 STEP 1 UNTIL NVAR DO X[J]),                BSWO  36
              OUT1(FOR L ~ 0 STEP 1 UNTIL 4 DO ID[L]) ;                 BSWO  37
LABEL         NAXT ;                                                    BSWO  38
              READ (CARDR,FORM3,SCAL) ;                                 BSWO  39
              IF NCAS > 0 THEN                                          BSWO  40
              READ (CARDR,FORM2,MISS) ;                                 BSWO  41
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            BSWO  42
         BEGIN                                                          BSWO  43
              SC[I] ~ 10.0*S[I] ;                                       BSWO  44
              SUMX[I] ~ 0.0 ;                                           BSWO  45
              SUMXX[I] ~ 0.0 ;                                          BSWO  46
              MINX[I] ~ 0.9@49 ;                                        BSWO  47
              MAXX[I] ~ -0.9@49 ;                                       BSWO  48
              NX[I] ~ 0                                                 BSWO  49
         END ;                                                          BSWO  50
              FOR I ~ 1 STEP 1 UNTIL NSAM DO                            BSWO  51
         BEGIN                                                          BSWO  52
              READ (CARDR,FORM2,DATA) ;                                 BSWO  53
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            BSWO  54
         BEGIN                                                          BSWO  55
              W ~ X[J] ;                                                BSWO  56
              NOMIS ~ TRUE ;                                            BSWO  57
              IF NCAS = 1 THEN                                          BSWO  58
         BEGIN                                                          BSWO  59
              FOR L ~ 1 STEP 1 UNTIL NCOD DO                            BSWO  60
         BEGIN                                                          BSWO  61
              IF W = CODE[L] THEN                                       BSWO  62
         BEGIN                                                          BSWO  63
              NOMIS ~ FALSE ;                                           BSWO  64
              GO TO NAXT                                                BSWO  65
         END                                                            BSWO  66
         END                                                            BSWO  67
         END ;                                                          BSWO  68
              IF NOMIS THEN                                             BSWO  69
                                                                        BSWO  70
         BEGIN                                                          BSWO  71
              W ~ W|SC[J] ;                                             BSWO  72
              NX[J] ~ NX[J] + 1 ;                                       BSWO  73
              SUMX[J] ~ SUMX[J] + W ;                                   BSWO  74
              SUMXX[J] ~ SUMXX[J] + W|W ;                               BSWO  75
              IF MAXX[J] < W THEN                                       BSWO  76
              MAXX[J] ~ W ;                                             BSWO  77
              IF MINX[J] > W THEN                                       BSWO  78
              MINX[J] ~ W                                               BSWO  79
         END ;                                                          BSWO  80
NAXT:    END                                                            BSWO  81
         END ;                                                          BSWO  82
              WRITE(PRINT[PAGE]) ;                                      BSWO  83
              WRITE(PRINT,TITLE,OUT1) ;                                 BSWO  84
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            BSWO  85
         BEGIN                                                          BSWO  86
              DEN ~ NX[I] ;                                             BSWO  87
              IF DEN = 0 THEN                                           BSWO  88
              WRITE(PRINT,FRM1,I)                                       BSWO  89
              ELSE IF DEN = 1 THEN                                      BSWO  90
              WRITE(PRINT,FRM3,I)                                       BSWO  91
              ELSE                                                      BSWO  92
         BEGIN                                                          BSWO  93
              XBAR[I] ~ SUMX[I] / DEN ;                                 BSWO  94
              SUMXX[I] ~ (SUMXX[I]-(SUMX[I]|SUMX[I])/DEN)/(DEN-1.0) ;   BSWO  95
              SUMXX[I] ~ SQRT(SUMXX[I]) ;                               BSWO  96
              SUMX[I] ~ SUMXX[I]/SQRT(DEN) ;                            BSWO  97
              RANG ~ MAXX[I] - MINX[I] ;                                BSWO  98
              WRITE(PRINT,FRM2,I,XBAR[I],SUMXX[I],SUMX[I],DEN,MAXX[I],  BSWO  99
                   MINX[I],RANG)                                        BSWO 100
         END                                                            BSWO 101
         END ;                                                          BSWO 102
              GO TO START                                               BSWO 103
         END ;                                                          BSWO 104
NOMO:    END PROGRAM .                                                  BSWO 105
 LABEL  000000000CARDR  0010000001
PROF. SERV. DEPT. PROB.    170    5 16  1  3
-00002                  &00001
            -00001
       999.01000.0
 200.0  37.0  12.7   9.7   1.2  31.2 999.0         1.7   0.4  56.0   8.1
 550.0   2.3 900.0 900.0
        62.5   9.8   8.01000.0  33.4        12.5   3.2   0.8         9.0
 610.0 999.0 700.0
        45.0  13.0   8.6        32.7        16.2   5.5   1.2         6.5
 510.0       675.0
 999.0  45.0   8.2   5.8   2.6  34.8 999.0  13.7   4.6   0.1  37.61000.0

 475.0  53.5  11.5   9.2   5.9  32.11000.0  10.0   2.2   2.2  40.8   5.3
 650.01000.0 750.0 750.0
