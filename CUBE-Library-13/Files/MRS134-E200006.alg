 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                          UNIT   1
                                                                        UNIT   2
COMMENT       LEAST SQUARES POLYNOMIAL CURVE-FITTING WITH WEIGHT        UNIT   3
              OPTIONS. THE OMEGA VALUES MAY EITHER BE CONSTANTS, OR THEYUNIT   4
              MAY START WITH ANY GIVEN INITIAL VALUE AND BE ITERATED TO UNIT   5
              SELFCONSISTENCY BY ASSIGNING A FUNCTION OF THE SQUARES    UNIT   6
              OF THE NORMAL DIFFERENCES AS OMEGA.                       UNIT   7
                                                                        UNIT   8
              THE FOLLOWING BOOLEAN OPTIONS, IF TRUE, RESULT IN :       UNIT   9
                   STEPN ~ FITTING IS CARRIED OUT FOR ALL ORDERS,       UNIT  10
                           UP TO M { 10.                                UNIT  11
                   INTEG ~ VALUES OF FIRST AND SECOND INTEGRAL ARE      UNIT  12
                           COMPUTED FOR GRAPHS GOING THROUGH THE ORIGIN.UNIT  13
                   DIFFL ~ FIRST AND SECOND DERIVATIVES ARE COMPUTED.   UNIT  14
                   SELFC ~ WEIGHTS ARE ITERATED TO SELFCONSISTENCY FOR  UNIT  15
                           AN INPUT THRESHOLD VALUE.                    UNIT  16
                   FOLLW ~ SEQUENTIAL DATA SETS ARE PROCESSED.          UNIT  17
                                                                        UNIT  18
              DR EDGAR L. EICHHORN [ BURROUGHS PROFESSIONAL SERVICES].  UNIT  19
                                                                        UNIT  20
              CARD SEQUENCE STARTS WITH UNIT-0000.                      UNIT  21
              NEW VERSION, 05-25-1964.     ;                            UNIT  22
                                                                        UNIT  23
                                                                        UNIT  24
INTEGER       I,J,K,P,Q,S,N,M,CT,COUNT  ;                               UNIT  25
                                                                        UNIT  26
REAL          RT,SQUSUM,CALC,DIFF,DIFF2,FIRST,SECND,FACT1,FACT2,THOLD,  UNIT  27
              OMS,OMGSUM,WSUM,XQ,YQ  ;                                  UNIT  28
                                                                        UNIT  29
REAL ARRAY    PART[0:20],EL[0:13,0:14,0:13],DEN,                        UNIT  30
              SUB,SOL[0:13],X,Y,OMEGA,PREV[0:250]  ;                    UNIT  31
                                                                        UNIT  32
BOOLEAN       STEPN,INTEG,DIFFL,SELFC,FOLLW  ;                          UNIT  33
                                                                        UNIT  34
LIST          ORDER(P,M),                                               UNIT  35
              DETER(FACT1,FACT2),                                       UNIT  36
              THC(THOLD,CT),                                            UNIT  37
              BOOLEEN(STEPN,INTEG,DIFFL,SELFC,FOLLW),                   UNIT  38
              INDATA(X[Q],Y[Q],OMEGA[Q]),                               UNIT  39
              CN(SOL[Q],(Q-2)),                                         UNIT  40
              LISTING(Q,X[Q],Y[Q],OMEGA[Q],CALC,DIFF,FIRST,SECND),      UNIT  41
              VARIANCE((SQUSUM/OMGSUM),((100.0)|(1.0 - (SQUSUM/WSUM))));UNIT  42
                                                                        UNIT  43
FORMAT OUT    HEADLINE(X2,"Q",X14,"X",X9,"Y[OBS]",X10,"OMEGA",X8,       UNIT  44
                       "Y[CALC]",X10,"DIFF."),                          UNIT  45
              DIFFLINE(X2,"Q",X14,"X",X9,"Y[OBS]",X10,"OMEGA",X8,       UNIT  46
                       "Y[CALC]",X10,"DIFF.",X10,"SLOPE",X10,"CURV."),  UNIT  47
              INTGLINE(X2,"Q",X14,"X",X9,"Y[OBS]",X10,"OMEGA",X8,       UNIT  48
                       "Y[CALC]",X10,"DIFF.",X5,"FIRST INT.",X4,        UNIT  49
                       "SECOND INT."),                                  UNIT  50
              CONF(E14.7," | (X*",I2,")"),                              UNIT  51
              LISTF(I3,X3,7(E12.5,X3)),                                 UNIT  52
              CNTF("ITERATION NO. ",I2),                                UNIT  53
              SUMF("VARIANCE = ",E12.5,X3,"GOODNESS-OF-FIT = ",         UNIT  54
                   F10.5,X1,"PERCENT")  ;                               UNIT  55
                                                                        UNIT  56
FILE IN       BUFFIN (1,10)  ;                                          UNIT  57
                                                                        UNIT  58
FILE OUT      BUFFOUT 1 (1,15)  ;                                       UNIT  59
                                                                        UNIT  60
FORMAT IN     FMIN0(5L6),                                               UNIT  61
              FMIN1(3F12.5),                                            UNIT  62
              FMIN2(I2,X1,I2),                                          UNIT  63
              FMIN3(2F12.7),                                            UNIT  64
              FMIN4(F12.7,X1,I2)  ;                                     UNIT  65
                                                                        UNIT  66
LABEL         ONSET,START,SUMS,AAAAA,SECOND,THIRD,FOURTH,               UNIT  67
              FIFTH, SIXTH, SEVENTH, EIGHTH, NINTH, FINISH  ;           UNIT  68
                                                                        UNIT  69
                                                                        UNIT  70
ONSET:        Q ~ 1  ;                                                  UNIT  71
              READ(BUFFIN,FMIN0,BOOLEEN)  ;                             UNIT  72
              IF (INTEG OR DIFFL) THEN READ(BUFFIN,FMIN3,DETER)  ;      UNIT  73
              IF SELFC THEN READ(BUFFIN,FMIN4,THC)  ;                   UNIT  74
                                                                        UNIT  75
START  :      READ(BUFFIN,FMIN1,INDATA)  ;                              UNIT  76
              IF X[Q] = 0.0 AND Y[Q] = 0.0 THEN GO TO AAAAA  ;          UNIT  77
              PREV[Q] ~ 0.0  ;                                          UNIT  78
              Q ~ Q + 1  ;  GO TO START  ;                              UNIT  79
                                                                        UNIT  80
AAAAA  :      S ~ Q - 1  ;  COUNT ~ 0  ;                                UNIT  81
              READ(BUFFIN,FMIN2,ORDER)  ;                               UNIT  82
              IF NOT STEPN THEN                                         UNIT  83
         BEGIN                                                          UNIT  84
              N ~ P  ;  GO TO SUMS                                      UNIT  85
         END ;                                                          UNIT  86
                                                                        UNIT  87
              FOR N ~ P STEP 1 UNTIL M DO                               UNIT  88
         BEGIN                                                          UNIT  89
              IF SELFC THEN                                             UNIT  90
              FOR Q ~ 1 STEP 1 UNTIL S DO OMEGA[Q] ~ 1.0  ;             UNIT  91
                                                                        UNIT  92
SUMS  :       IF S { N THEN GO TO FINISH  ;                             UNIT  93
              COUNT ~ COUNT + 1  ;                                      UNIT  94
                                                                        UNIT  95
              FOR I ~ (2|N) STEP -1 UNTIL 0 DO PART[I] ~ 0.0  ;         UNIT  96
              FOR I ~ 1 STEP 1 UNTIL (N+1) DO EL[I,(N+2),1] ~ 0.0  ;    UNIT  97
              OMGSUM ~ WSUM ~ 0.0  ;                                    UNIT  98
                                                                        UNIT  99
              FOR Q ~ 1 STEP 1 UNTIL S DO                               UNIT 100
         BEGIN                                                          UNIT 101
              XQ ~ X[Q]  ;  YQ ~ Y[Q]  ;  OMS ~ OMEGA[Q]|OMEGA[Q]  ;    UNIT 102
              OMGSUM ~ OMS + OMGSUM  ;  WSUM ~ OMS|YQ|YQ + WSUM  ;      UNIT 103
                                                                        UNIT 104
              FOR J ~ (2|N) STEP -1 UNTIL 0 DO                          UNIT 105
              PART[J] ~ OMS|(XQ*J) + PART[J]  ;                         UNIT 106
              FOR I ~ 1 STEP 1 UNTIL (N+1) DO                           UNIT 107
              EL[I,(N+2),1] ~ - OMS|YQ|(XQ*(I-1)) + EL[I,(N+2),1]  ;    UNIT 108
         END  ;                                                         UNIT 109
                                                                        UNIT 110
SECOND  :     FOR I ~ 1 STEP 1 UNTIL (N+1) DO                           UNIT 111
              FOR J ~ 1 STEP 1 UNTIL (N+1) DO EL[I,J,1] ~ PART[I+N-J]  ;UNIT 112
                                                                        UNIT 113
THIRD:        Q ~ N + 1  ;  K ~ 1  ;                                    UNIT 114
                                                                        UNIT 115
FOURTH:       FOR I ~ 1 STEP 1 UNTIL Q DO                               UNIT 116
              DEN[I] ~ EL[I,1,K]  ;                                     UNIT 117
                                                                        UNIT 118
              FOR I ~ 1 STEP 1 UNTIL Q DO                               UNIT 119
              FOR J ~ 1 STEP 1 UNTIL (Q+1) DO                           UNIT 120
              EL[I,J,(K+1)] ~ EL[I,J,K]/DEN[I]  ;                       UNIT 121
                                                                        UNIT 122
              FOR J ~ 1 STEP 1 UNTIL (Q+1) DO                           UNIT 123
              SUB[J] ~ EL[1,J,(K+1)]  ;                                 UNIT 124
                                                                        UNIT 125
              FOR I ~ 1 STEP 1 UNTIL Q DO                               UNIT 126
              FOR J ~ 1 STEP 1 UNTIL (Q+1) DO                           UNIT 127
              EL[I,J,(K+1)] ~ EL[I,J,(K+1)] - SUB[J]  ;                 UNIT 128
                                                                        UNIT 129
              FOR I ~ 1 STEP 1 UNTIL Q DO                               UNIT 130
              FOR J ~ 1 STEP 1 UNTIL (Q+1) DO                           UNIT 131
              EL[I,J,(K+1)] ~ EL[(I+1),(J+1),(K+1)]  ;                  UNIT 132
                                                                        UNIT 133
              IF Q = 1 THEN GO TO FIFTH  ;                              UNIT 134
              Q ~ Q - 1  ;  K ~ K + 1  ;  GO TO FOURTH  ;               UNIT 135
                                                                        UNIT 136
FIFTH:        SOL[1] ~ 1.0  ;                                           UNIT 137
                                                                        UNIT 138
              FOR I ~ 2 STEP 1 UNTIL (N+2) DO                           UNIT 139
         BEGIN                                                          UNIT 140
              RT ~ 0.0  ;                                               UNIT 141
              FOR J ~ 1 STEP 1 UNTIL (I-1) DO                           UNIT 142
              RT ~ RT + EL[(I-1),(J+1),(N+3-I)] | SOL[I-J]  ;           UNIT 143
              SOL[I] ~ - RT/EL[(I-1),1,(N+3-I)]  ;                      UNIT 144
         END ;                                                          UNIT 145
                                                                        UNIT 146
SIXTH  :      FOR Q ~ 2 STEP 1 UNTIL (N+2) DO WRITE(BUFFOUT,CONF,CN)  ; UNIT 147
              IF S > 30 THEN WRITE(BUFFOUT[PAGE])  ;                    UNIT 148
              WRITE(BUFFOUT[DBL])  ;  WRITE(BUFFOUT,CNTF,COUNT)  ;      UNIT 149
              WRITE(BUFFOUT[DBL])  ;                                    UNIT 150
              IF INTEG THEN WRITE(BUFFOUT,INTGLINE) ELSE                UNIT 151
              IF DIFFL THEN WRITE(BUFFOUT,DIFFLINE) ELSE                UNIT 152
              WRITE(BUFFOUT,HEADLINE)  ;  WRITE(BUFFOUT[DBL])  ;        UNIT 153
                                                                        UNIT 154
SEVENTH:      SQUSUM ~ 0.0  ;                                           UNIT 155
                                                                        UNIT 156
              FOR Q ~ 1 STEP 1 UNTIL S DO                               UNIT 157
         BEGIN                                                          UNIT 158
              CALC ~ FIRST ~ SECND ~ 0.0  ;                             UNIT 159
              FOR I ~ 1 STEP 1 UNTIL (N+1) DO                           UNIT 160
              CALC ~ CALC + (X[Q] * (I - 1)) | SOL[I+1]  ;              UNIT 161
              DIFF ~ Y[Q] - CALC  ;  DIFF2 ~ DIFF|DIFF  ;               UNIT 162
                                                                        UNIT 163
              IF INTEG THEN                                             UNIT 164
              FOR I ~ 1 STEP 1 UNTIL (N+1) DO                           UNIT 165
         BEGIN                                                          UNIT 166
              FIRST ~ FIRST + (SOL[I+1]/I) | (X[Q] * I)  ;              UNIT 167
              SECND ~ SECND + (SOL[I+1]/(I | (I + 1))) | (X[Q] * (I+1)) UNIT 168
         END ELSE                                                       UNIT 169
                                                                        UNIT 170
              IF DIFFL THEN                                             UNIT 171
         BEGIN                                                          UNIT 172
              FOR I ~ 2 STEP 1 UNTIL (N+1) DO                           UNIT 173
              FIRST ~ FIRST + (SOL[I+1]) | (I - 1) | (X[Q] * (I - 2))  ;UNIT 174
              FOR I ~ 3 STEP 1 UNTIL (N+1) DO                           UNIT 175
              SECND ~ SECND + (SOL[I+1]) | (I - 1) | ( I - 2) |         UNIT 176
                              (X[Q] * (I - 3))  ;                       UNIT 177
              FACT1 ~ FACT2 ~ 0.0  ;                                    UNIT 179
         END ELSE                                                       UNIT 178
                                                                        UNIT 180
              FIRST ~ FIRST | FACT1  ;  SECND ~ SECND | FACT2  ;        UNIT 181
              WRITE(BUFFOUT,LISTF,LISTING)  ;                           UNIT 182
              PREV[Q] ~ OMEGA[Q]  ;  OMEGA[Q] ~ EXP(- DIFF2)  ;         UNIT 183
              SQUSUM ~ DIFF2|PREV[Q]|PREV[Q] + SQUSUM  ;                UNIT 184
         END ;                                                          UNIT 185
              WRITE(BUFFOUT[DBL])  ;  WRITE(BUFFOUT[DBL])  ;            UNIT 186
                                                                        UNIT 187
EIGHTH  :     WRITE(BUFFOUT,SUMF,VARIANCE)  ;  WRITE(BUFFOUT[PAGE])  ;  UNIT 188
              IF SELFC AND COUNT < CT THEN                              UNIT 189
         BEGIN                                                          UNIT 190
              FOR Q ~ 1 STEP 1 UNTIL S DO                               UNIT 191
              IF ABS(OMEGA[Q] - PREV[Q]) > THOLD THEN GO TO SUMS        UNIT 192
         END  ;                                                         UNIT 193
              COUNT ~ 0  ;                                              UNIT 194
              IF NOT STEPN THEN GO TO FINISH  ;                         UNIT 195
                                                                        UNIT 196
NINTH:        END ;                                                     UNIT 197
                                                                        UNIT 198
              IF FOLLW THEN                                             UNIT 199
         BEGIN                                                          UNIT 200
              WRITE(BUFFOUT[PAGE])  ;  GO TO ONSET  ;                   UNIT 201
         END  ;                                                         UNIT 202
                                                                        UNIT 203
FINISH : END.                                                           UNIT 204
