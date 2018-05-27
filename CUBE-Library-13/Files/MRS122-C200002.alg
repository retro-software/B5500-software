 LABEL  0000000000XXXXXX0010000001
         BEGIN                                                          TEST   1
                                                                        TEST   2
COMMENT       PROCEDURE MULLER FINDS REAL AND COMPLEX ROOTS OF AN       TEST   3
              ARBITRARY FUNCTION.  P1, P2, AND P3 ARE STARTING VALUES.  TEST   4
              THE ROOTS NEAREST TO THESE POINTS WILL BE FOUND FIRST.    TEST   5
              MXM IS THE MAXIMUM NUMBER OF ITERATIONS TO BE MADE IN     TEST   6
              FINDING ANY ONE ROOT.  EP1 AND EP2 ARE CONVERGENCE        TEST   7
              CRITERION FACTORS.  IF ABS((X(I+1)-X(I))/X(I+1)) IS       TEST   8
              LESS THAN EP1 OR IF THE FUNCTION VALUE AND MODIFIED       TEST   9
              FUNCTION VALUE ARE BOTH LESS THAN EP2, A ROOT HAS BEEN    TEST  10
              FOUND.  SW1, SW2, SW3, AND SWR ARE BOOLEAN VARIABLES.     TEST  11
              IF  SW1 IS TRUE, THEN EACH ITERANT OF EACH ROOT WILL BE   TEST  12
              PRINTED.  IF SW2 IS TRUE THE VALUE OF EACH ROOT FOUND     TEST  13
              WILL BE PRINTED.  IF SW3 IS TRUE, THEN, WHEN APPLICABLE,  TEST  14
              THE COMPLEX CONJUGATE OF EACH ROOT FOUND WILL BE          TEST  15
              ADMITTED AS A ROOT.  IF SWR IS TRUE ONLY REAL ROOTS       TEST  16
              ARE FOUND. SWR SHOULD BE SET TRUE IF IT IS KNOWN THAT     TEST  17
              THE FUNCTION HAS ALL REAL ROOTS.  ARRAYS RRT AND IRT      TEST  18
              CONTAIN THE REAL AND IMAGINARY PARTS, RESPECTIVELY, OF    TEST  19
              THE ROOTS FOUND.  PROCEDURE FUNCTION IS THE FUNCTION      TEST  20
              GENERATOR AND PROCEDURE COMPLEX PERFORMS THE NECESSARY    TEST  21
              COMPLEX ARITHMETIC.                                       TEST  22
                                                                        TEST  23
              R. D. RODMAN,                                             TEST  24
              (PROFESSIONAL SERVICES DIVISIONAL GROUP),                 TEST  25
                                                                        TEST  26
              CARD SEQUENCE BEGINS WITH MULL0001,                       TEST  27
              FIRST RELEASE 5/1/63  ;                                   TEST  28
                                                                        TEST  29
INTEGER       TMXM, TNRTS, I ;                                          TEST  30
REAL          TP1, TP2, TP3, TEP1, TEP2 ;                               TEST  31
BOOLEAN       TSW1, TSW2, TSW3, TSWR ;                                  TEST  32
LABEL         START ;                                                   TEST  33
REAL ARRAY    TRRT, TIRT[0:100] ;                                       TEST  34
FILE IN       TIN1(1,10) ;                                              TEST  35
FILE OUT      TOUT1(8,15) ;                                             TEST  36
FORMAT IN     TF1(3F8.3,2I3,2E9.2,4L6) ;                                TEST  37
                                                                        TEST  38
FORMAT OUT    TF2(//X46,"REAL",X12,"IMAGINARY"),                        TEST  39
              TF4(X37,E18.11,X4,E18.11) ;                               TEST  40
                                                                        TEST  41
PROCEDURE     FUNCTION(REALE, IMAG, REVAL, IEVAL) ;                     TEST  42
                                                                        TEST  43
VALUE         REALE, IMAG ;                                             TEST  44
REAL          REALE, IMAG, REVAL, IEVAL ;                               TEST  45
                                                                        TEST  46
         BEGIN                                                          TEST  47
                                                                        TEST  48
COMMENT       THE USER MUST INSERT THE CODING FOR THIS PROCEDURE.       TEST  49
              "REALE" AND "IMAG" ARE THE REAL AND IMAGINARY PARTS OF    TEST  50
              THE DEPENDENT VARIABLE. THE REAL AND IMAGINARY PARTS OF   TEST  51
              THE FUNCTION EVALUATED AT THIS POINT MUST BE "REVAL" AND  TEST  52
              "IEVAL", RESPECTIVELY ;                                   TEST  53
                                                                        TEST  54
REAL     T1, IT1, T2, IT2, T3, IT3, T4, IT4 ;                           TEST  55
                                                                        TEST  56
COMMENT       THIS FUNCTION IS THE EIGHTH DEGREE POLYNOMIAL             TEST  57
              SHOWN IN THE SECOND TEST CASE ;                           TEST  58
                                                                        TEST  59
         T1 ~ REALE | REALE - IMAG | IMAG + REALE + 1 ;                 TEST  60
         IT1 ~ 2 | REALE | IMAG + IMAG ;  T2 ~ T1 -2-REALE ;            TEST  61
         IT2 ~ IT1 - IMAG ;  T3 ~ T1 | T2 - IT1 | IT2 + 2 ;             TEST  62
         IT3 ~ T1 | IT2 + T2 | IT1 ;  T1 ~ T1-REALE ;  T2 ~ T1+1 ;      TEST  63
         T4 ~ T1 | T2 - IT2 | IT2 ;  IT4 ~ T1 | IT2 + IT2 | T2 ;        TEST  64
         REVAL ~ T3 | T4 - IT3 | IT4 - 1 ;                              TEST  65
         IEVAL ~ T3 | IT4 + IT3 | T4                                    TEST  66
         END ;                                                          TEST  67
                                                                        TEST  68
PROCEDURE     MULLER(P1, P2, P3, MXM, NRTS, EP1, EP2, SW1, SW2, SW3,    MULL   1
                     SWR, RRT, IRT, OT1 ) ;                             MULL   2
                                                                        MULL   3
VALUE         P1, P2, P3, MXM, NRTS, EP1, EP2, SW1, SW2, SW3, SWR ;     MULL   4
INTEGER       MXM, NRTS ;                                               MULL   5
BOOLEAN       SW1, SW2, SW3, SWR ;                                      MULL   6
REAL          P1, P2, P3, EP1, EP2 ;                                    MULL   7
REAL ARRAY    RRT, IRT[0] ;                                             MULL   8
FILE          OT1 ;                                                     MULL   9
                                                                        MULL  10
         BEGIN                                                          MULL  11
BOOLEAN       BOOL;                                                     MULL  12
INTEGER       C1, RTC, I, ITC ;                                         MULL  13
                                                                        MULL  14
REAL          RX1, RX2, RX3, IX1, IX2, IX3, RROOT, IROOT, RDNR, IDNR,   MULL  15
              T1, IT1, FRROOT, FIROOT, RFX1, RFX2, RFX3, IFX1, IFX2,    MULL  16
              IFX3, RH, IH, RLAM, ILAM, RDEL, IDEL, T2, IT2, T3, IT3,   MULL  17
              T4, IT4, RG, IG, RDEN, IDEN, RFUNC, IFUNC ;               MULL  18
                                                                        MULL  19
LABEL         M0, M1, M2, M3, M4, M9, M8, M6, M7, FIN1, FIN2, FIN3, M10,MULL  20
              M12, M11, EXIT ;                                          MULL  21
                                                                        MULL  22
SWITCH        J ~ M2, M3, M4, M7, M11 ;                                 MULL  23
                                                                        MULL  24
FORMAT OUT    F2(  X46,"REAL", X12,"IMAGINARY" / X37, E18.11, X4, E18.11MULL  25
              /  X39,"THE FUNCTION EVALUATED AT THIS POINT IS"  /X46,   MULL  26
              "REAL",X12,"IMAGINARY"/X37,E18.11,X4,E18.11 /X35,"THE MOD"MULL  27
              "IFIED FUNCTION EVALUATED AT THIS POINT IS"/ X46,"REAL",  MULL  28
              X12,"IMAGINARY"/X37,E18.11,X4,E18.11  ),                  MULL  29
              F4(///X29,I3," ITERATIONS HAVE BEEN MADE.  THE VALUE OF " MULL  30
              "THE ITERANT IS NOW"),                                    MULL  31
              F6(//X37,"SUCCESSIVE ITERANTS MEET CONVERGENCE CRITERION"/MULL  32
              X39,"AFTER",I3," ITERATIONS.  THE ROOT FOUND IS"),        MULL  33
              F8(//X33,"THE FUNCTION VALUES OF THE LAST ITERANT ARE"    MULL  34
              " SUFFICIENTLY"/X33,"NEAR ZERO. ",I4," ITERATIONS WERE MA"MULL  35
              "DE.  THE ROOT FOUND IS"),                                MULL  36
              F10(//X35,I3," ITERATIONS COMPLETED AND SUCCESSFUL CONVE" MULL  37
              "RGENCE"/X41,"HAS NOT OCCURRED.  THE LAST ITERANT IS"),   MULL  38
              F12(//X40,"THE PREVIOUS ROOT FOUND WAS COMPLEX.  THE"/X40,MULL  39
              "CONJUGATE OF THIS VALUE IS ALSO A ROOT.") ;              MULL  40
                                                                        MULL  41
PROCEDURE     COMPLEX(A, IA, B, IB, K, C, IC) ;                         MULL  42
                                                                        MULL  43
VALUE         A, IA, B, IB, K ;                                         MULL  44
INTEGER       K ;                                                       MULL  45
REAL          A, IA, B, IB, C, IC ;                                     MULL  46
                                                                        MULL  47
         BEGIN                                                          MULL  48
REAL          TEMP ;                                                    MULL  49
LABEL         MPY, DVD, SQT, EXIT ;                                     MULL  50
SWITCH        JUNCTION ~ MPY, DVD, SQT ;                                MULL  51
              GO TO JUNCTION[K] ;                                       MULL  52
                                                                        MULL  53
MPY:          C ~ A|B -IA|IB ;  IC ~ A|IB + IA|B ;  GO TO EXIT ;        MULL  54
                                                                        MULL  55
DVD:          IF B = 0 AND IB = 0 THEN                                  MULL  56
         BEGIN                                                          MULL  57
              C~1 ;  IC~0 ;  GO TO EXIT                                 MULL  58
         END ;                                                          MULL  59
              TEMP ~ B|B + IB|IB ;  C ~ (A|B + IA|IB) / TEMP ;          MULL  60
              IC ~ (IA|B - A|IB) / TEMP ;  GO TO EXIT ;                 MULL  61
                                                                        MULL  62
SQT:          IF (IA=0) AND (A<0) THEN                                  MULL  63
         BEGIN                                                          MULL  64
              C ~ 0 ;  IC ~ SQRT(-A)                                    MULL  65
         END                                                            MULL  66
              ELSE IF IA=0 THEN                                         MULL  67
         BEGIN                                                          MULL  68
              C ~ SQRT(A) ;  IC~0                                       MULL  69
         END                                                            MULL  70
              ELSE                                                      MULL  71
         BEGIN                                                          MULL  72
              TEMP ~ SQRT(A|A + IA|IA) ;                                MULL  73
              C ~ SQRT((TEMP+A)/2) ;                                    MULL  74
              IC ~ IF (TEMP -A) < 0 THEN 0 ELSE SQRT((TEMP-A)/2)        MULL  75
         END ;                                                          MULL  76
              IF((B+C)|(B+C)+(IB+IC)|(IB+IC))<((B-C)|(B-C)+(IB-IC)|     MULL  77
                  (IB-IC)) THEN                                         MULL  78
         BEGIN                                                          MULL  79
              C ~ B-C ;  IC ~ IB-IC                                     MULL  80
         END                                                            MULL  81
              ELSE                                                      MULL  82
         BEGIN                                                          MULL  83
              C ~ B+C ;  IC ~ IB+IC                                     MULL  84
         END ;                                                          MULL  85
                                                                        MULL  86
EXIT:    END ;                                                          MULL  87
                                                                        MULL  88
              FOR I ~ 1 STEP 1 UNTIL NRTS DO RRT[I]~IRT[I]~0 ; RTC~0 ;  MULL  89
                                                                        MULL  90
M0:           IX1~IX2~IX3~C1~IROOT~ITC~0 ;  RROOT~P1 ;  BOOL ~ FALSE ;  MULL  91
                                                                        MULL  92
M1:           C1~C1+1 ;   RDNR ~ 1 ;  IDNR ~ 0 ;                        MULL  93
              FOR I ~ 1 STEP 1 UNTIL RTC DO                             MULL  94
         BEGIN                                                          MULL  95
              COMPLEX(RDNR,IDNR,RROOT-RRT[I],IROOT-IRT[I],1,T1,IT1) ;   MULL  96
              RDNR ~ T1 ;  IDNR ~ IT1                                   MULL  97
         END ;                                                          MULL  98
              FUNCTION(RROOT, IROOT, T1, IT1) ;                         MULL  99
              COMPLEX(T1, IT1, RDNR, IDNR, 2, FRROOT, FIROOT) ;         MULL 100
              GO TO J[C1] ;                                             MULL 101
                                                                        MULL 102
M2:           RFX1~FRROOT ;  IFX1~FIROOT ;  RROOT~P2 ;  GO TO M1 ;      MULL 103
                                                                        MULL 104
M3:           RFX2~FRROOT ;  IFX2~FIROOT ;  RROOT~P3 ;  GO TO M1 ;      MULL 105
                                                                        MULL 106
M4:           RFX3~FRROOT ;  IFX3~FIROOT ;  RX1~P1 ;  RX2~P2 ;  RX3~P3 ;MULL 107
              RH ~ RX3-RX2 ;  IH ~ IX3-IX2  ;                           MULL 108
              COMPLEX(RH,IH, RX2-RX1, IX2-IX1, 2, RLAM,ILAM) ;          MULL 109
              RDEL ~ RLAM + 1 ;  IDEL ~ ILAM ;                          MULL 110
                                                                        MULL 111
M9:           IF(RFX1=RFX2)AND(RFX2=RFX3)AND(IFX1=IFX2)AND(IFX2=IFX3)   MULL 112
         THEN BEGIN                                                     MULL 113
              RLAM ~ 1 ;  ILAM ~ 0 ;  GO TO M8                          MULL 114
         END ;                                                          MULL 115
              COMPLEX(RFX1, IFX1, RLAM, ILAM, 1, T1, IT1) ;             MULL 116
              COMPLEX(RFX2, IFX2, RDEL, IDEL, 1, T2, IT2) ;             MULL 117
              T1 ~ T1-T2+RFX3 ;  IT1 ~ IT1-IT2+IFX3 ;                   MULL 118
              COMPLEX(RDEL, IDEL, RLAM, ILAM, 1, T2, IT2) ;             MULL 119
              COMPLEX(T1,IT1,T2,IT2,1,T3,IT3) ;                         MULL 120
              COMPLEX(RFX3,IFX3,T3,IT3,1,T1,IT1) ;                      MULL 121
              T1 ~ -4|T1 ;  IT1 ~ -4|IT1 ;                              MULL 122
              COMPLEX(RFX3,IFX3,RLAM+RDEL,ILAM+IDEL,1,T2,IT2) ;         MULL 123
              COMPLEX(RDEL|RDEL-IDEL|IDEL,2|RDEL|IDEL,RFX2,IFX2,1,T3,   MULL 124
              IT3) ;  COMPLEX(RLAM|RLAM-ILAM|ILAM,2|RLAM|ILAM,RFX1,IFX1,MULL 125
              1,T4,IT4) ;  RG~T4-T3+T2 ;  IG~IT4-IT3+IT2 ;              MULL 126
              IF SWR AND ((RG|RG+T1)<0) THEN                            MULL 127
         BEGIN                                                          MULL 128
              RDEN ~ RG ;  IDEN ~ IG ~ 0                                MULL 129
         END  ELSE                                                      MULL 130
              COMPLEX(RG|RG-IG|IG+T1,2|RG|IG+IT1,RG,IG,3,RDEN,IDEN) ;   MULL 131
              COMPLEX(-2|RFX3, -2|IFX3, RDEL, IDEL, 1, T1, IT1) ;       MULL 132
              COMPLEX(T1, IT1, RDEN, IDEN, 2, RLAM, ILAM) ;             MULL 133
                                                                        MULL 134
M8:           ITC ~ ITC + 1 ;                                           MULL 135
              RX1~RX2 ;  RX2~RX3 ;  RFX1~RFX2 ;  RFX2~RFX3 ;            MULL 136
              IX1~IX2 ;  IX2~IX3 ;  IFX1~IFX2 ;  IFX2~IFX3 ;            MULL 137
              COMPLEX(RLAM,ILAM,RH,IH,1,T1,IT1) ;  RH~T1 ;  IH~IT1 ;    MULL 138
                                                                        MULL 139
M6:           RDEL~RLAM+1 ;  IDEL~ILAM ;  RX3~RX2+RH ;  IX3~IX2+IH ;    MULL 140
              C1~3 ;  RROOT~RX3 ;  IROOT~IX3 ;  GO TO M1 ;              MULL 141
                                                                        MULL 142
M7:           RFX3~FRROOT ;  IFX3~FIROOT ;                              MULL 143
              FUNCTION(RX3,IX3, RFUNC,IFUNC) ;                          MULL 144
              COMPLEX(RFX3, IFX3, RFX2, IFX2, 2, T1, IT1) ;             MULL 145
              IF (T1|T1 + IT1|IT1) > 100 THEN                           MULL 146
         BEGIN                                                          MULL 147
              RLAM~RLAM/2 ; RH~RH/2 ; ILAM~ILAM/2 ; IH~IH/2 ;  GO TO M6 MULL 148
         END ;                                                          MULL 149
              IF SW1 THEN                                               MULL 150
         BEGIN                                                          MULL 151
              WRITE(OT1,F4,ITC) ;                                       MULL 152
              WRITE(OT1,F2,RX3,IX3,RFUNC,IFUNC,RFX3,IFX3)               MULL 153
         END ;                                                          MULL 154
              T1 ~ RX3-RX2 ;  IT1 ~ IX3-IX2 ;                           MULL 155
              COMPLEX(T1, IT1, RX2, IX2, 2, T2, IT2) ;                  MULL 156
              IF SQRT(T2|T2+IT2|IT2){EP1 THEN GO TO FIN1 ;              MULL 157
              IF(SQRT(RFUNC|RFUNC+IFUNC|IFUNC){EP2) AND                 MULL 158
              (SQRT(RFX3|RFX3+IFX3|IFX3){EP2) THEN GO TO FIN2 ;         MULL 159
              IF ITC}MXM THEN GO TO FIN3 ELSE GO TO M9 ;                MULL 160
                                                                        MULL 161
FIN1:         IF (NOT SW2) THEN GO TO M12 ELSE WRITE(OT1,F6,ITC) ;      MULL 162
              GO TO M10 ;                                               MULL 163
                                                                        MULL 164
FIN2:         IF (NOT SW2) THEN GO TO M12 ELSE WRITE(OT1,F8,ITC) ;      MULL 165
              GO TO M10 ;                                               MULL 166
                                                                        MULL 167
FIN3:         BOOL ~ TRUE ;                                             MULL 168
              IF (NOT SW2) THEN GO TO M12 ELSE WRITE(OT1,F10,ITC) ;     MULL 169
                                                                        MULL 170
M10:          WRITE(OT1,F2,RX3,IX3,RFUNC,IFUNC,RFX3,IFX3) ;             MULL 171
                                                                        MULL 172
M12:          RTC ~ RTC+1 ;  RRT[RTC] ~ RX3 ;  IRT[RTC] ~ IX3 ;         MULL 173
              IF RTC } NRTS THEN GO TO EXIT ;                           MULL 174
              IF(ABS(IX3)>EP1) AND (SW3) AND (NOT BOOL) THEN            MULL 175
         BEGIN                                                          MULL 176
              IX3~-IX3 ;  FUNCTION(RX3,IX3,RFUNC,IFUNC) ;               MULL 177
              RROOT ~ RX3 ;  IROOT ~ IX3 ;  C1 ~ 4 ;  GO TO M1 ;        MULL 178
                                                                        MULL 179
M11:          IF SW2 THEN                                               MULL 180
         BEGIN                                                          MULL 181
              WRITE(OT1,F12) ;                                          MULL 182
              WRITE(OT1,F2,RX3,IX3,RFUNC,IFUNC,FRROOT,FIROOT)           MULL 183
         END ;                                                          MULL 184
              RTC ~ RTC+1 ;  RRT[RTC] ~ RX3 ;  IRT[RTC] ~ IX3           MULL 185
         END ELSE GO TO M0 ;                                            MULL 186
              IF RTC < NRTS THEN GO TO M0 ;                             MULL 187
                                                                        MULL 188
EXIT:    END ;                                                          MULL 189
                                                                        TEST  69
COMMENT       TEST PROGRAM BEGINS HERE ;                                TEST  70
                                                                        TEST  71
START:        READ(TIN1,TF1,TP1,TP2,TP3,TMXM,TNRTS,TEP1,TEP2,TSW1,TSW2, TEST  72
              TSW3,TSWR) ;                                              TEST  73
              FOR I ~ 0 STEP 1 UNTIL 100 DO TRRT[I] ~ TIRT[I] ~ 0 ;     TEST  74
              MULLER(TP1,TP2,TP3,TMXM,TNRTS,TEP1,TEP2,TSW1,TSW2,TSW3,   TEST  75
              TSWR,TRRT,TIRT, TOUT1 ) ;                                 TEST  76
              WRITE(TOUT1,TF2) ;                                        TEST  77
              WRITE(TOUT1,TF4, FOR I~1 STEP 1 UNTIL TNRTS DO [TRRT[I],  TEST  78
                    TIRT[I]]) ;                                         TEST  79
              GO TO START                                               TEST  80
         END.                                                           TEST  81

 LABEL  000000000CARDSIN0010000001
  -1.       .1     1.    30  8  .1 @- 9  .1 @- 9  TRUE  TRUE  TRUE FALSE
  -1.       .1     1.    30  8  .1 @- 9  .1 @- 9 FALSE  TRUE  TRUE FALSE

