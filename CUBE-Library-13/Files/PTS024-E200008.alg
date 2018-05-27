         BEGIN                                                          NLCF   1

COMMENT       LEAST SQUARES CURVE FIT FOR EVALUATING CONSTANTS IN       NLCF   2
              NONLINEAR EQUATIONS.                                      NLCF   3
              A. PAUL OLESON                                            NLCF   4
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORP.              NLCF   5
              CARD SEQUENCE STARTS WITH NLCF-0001.                      NLCF   6
              FIRST RELEASE 2-30-63;                                    NLCF   7

INTEGER       I, J, K, M, N, P, S, V, W, Q, COUNT, ITER ;               NLCF   8

BOOLEAN       P1, P2, RELTV, LVNBG ;                                    NLCF   9

REAL          TOLA, TOLB ;                                              NLCF  10

FILE IN       CARD1(1,10) ;                                             NLCF  11

FILE OUT      CARD4(1,15) ;                                             NLCF  12

FORMAT IN     CARD2(9I6,2E11.2) ;                                       NLCF  13

LIST          INTGR(N, M, P, Q, P1, P2, RELTV, LVNBG, ITER, TOLA, TOLB);NLCF  14

LABEL         START ;                                                   NLCF  15

COMMENT       READ DATA GIVING PROBLEM SIZE ;                           NLCF  16

START:        READ (CARD1, CARD2, INTGR) ;                              NLCF  17
         BEGIN                                                          NLCF  18

REAL ARRAY    G, F, T[0:N], L, U, C, CC, B, BB[0:M], A, R[0:M,0:M],     NLCF  19
              X[0:P,0:N], H[0:M,0:N], PH[0:M,0:M,0:N] ;                 NLCF  20

REAL          LI, RKI, RR, SD, SE, SS, TEST, TT, OMEGA, Y, TRS1, TRS2,  NLCF  21
              TRS3, TRS4, X1S, X2S, X3S ;                               NLCF  22

LABEL         FUNC, INT, PRINT ;                                        NLCF  23

FORMAT IN     CARD3 (5E15.6) ;                                          NLCF  24

LIST          FNCTN (FOR S ~ 1 STEP 1 UNTIL N DO F[S]),                 NLCF  25
              VARBL (FOR S ~ 1 STEP 1 UNTIL N DO X[V,S]),               NLCF  26
              CONST (FOR I ~ 1 STEP 1 UNTIL M DO B[I]),                 NLCF  27
              OUT1(COUNT, TT),                                          NLCF  28
              OUT2 (FOR I ~ 1 STEP 1 UNTIL M DO [I, B[I]]),             NLCF  29
              OUT3 (FOR S ~ 1 STEP 1 UNTIL N DO [F[S], G[S], T[S]]),    NLCF  30
              OUT4 (SD, SE, RR) ;                                       NLCF  31

FORMAT OUT    HEAD (X15, "ITERATION NO.",X10,"SUM OF SQUARES",X20,"I",  NLCF  32
              X15,"B[I]"/),                  FMT1 (X20, I2, X15, E15.8),NLCF  33
              FMT2 (X72, I1, X8, E15.8),                                NLCF  34
              FMT3 (/X30, "OBSERVED",X15,"CALCULATED",X15,"DIFFERENCE"),NLCF  35
              FMT4 (X26, E15.8, 2(X10, E15.8)),                         NLCF  36
              FMT5 (X10, "STANDARD DEVIATION =", E15.8, X5,             NLCF  37
              "STANDARD ERROR =", E15.8, X5, "R SQUARE =", E15.8, I2//);NLCF  38
              READ (CARD1, CARD3, FNCTN) ;                              NLCF  39
              FOR V ~ 1 STEP 1 UNTIL P DO READ (CARD1, CARD3, VARBL) ;  NLCF  40
              READ (CARD1, CARD3, CONST) ;                              NLCF  41
              IF P1 THEN WRITE (CARD4, HEAD) ;                          NLCF  42

COMMENT       THE ROUTINE FOR COMPUTING G[S], ITS FIRST DERIVATIVES,    NLCF  43
              H[I,S], AND SECOND DERIVATIVES, PH[I,J,S], WITH RESPECT TONLCF  44
              EACH OF THE COEFFICIENTS, B[I], MUST FOLLOW THE DATA READ NLCF  45
              IN.  THE ROUTINE LABEL IS FUNC ;                          NLCF  46

FUNC :        FOR S ~ 1 STEP 1 UNTIL N DO                               FUNC   1

COMMENT       THIS IS THE ROUTINE FOR THE SAMPLE PROBLEM. IT IS REPLACEDFUNC   2
              BY A SUITABLE ROUTINE FOR EACH TYPE OF CORRELATION USED ; FUNC   3

         BEGIN                                                          FUNC   4
              TT ~ X[1,S] ;                  TRS1 ~ B[3] | TT + B[2] ;  FUNC   5
              TRS2 ~ - 1.0 / TRS1 ;          TRS3 ~ EXP(TRS2) ;         FUNC   6
              TRS4 ~ TRS2 | TRS2 ;           Y ~ G[S] ~ B[1] | TRS3 ;   FUNC   7
              H[1,S] ~ TRS3 ;                TRS2 ~ H[2,S] ~ Y | TRS4 ; FUNC   8
              H[3,S] ~ TT | TRS2 ;           PH[1,1,S] ~ 0.0 ;          FUNC   9
              TRS2 ~ PH[1,2,S] ~ PH[2,1,S] ~ TRS3 | TRS4 ;              FUNC  10
              PH[1,3,S] ~ PH[3,1,S] ~ TT | TRS2 ;                       FUNC  11
              TRS3 ~ PH[2,2,S] ~ - (TRS1 | 2.0 - 1.0) | Y | TRS4 | TRS4;FUNC  12
              PH[2,3,S] ~ PH[3,2,S] ~ TT | TRS3 ;                       FUNC  13
              PH[3,3,S] ~ TT | TT | TRS3                                FUNC  14
         END ;                                                          FUNC  15

              COUNT ~ COUNT + 1 ;                                       NLCF  47
              IF Q ! 0 THEN                                             NLCF  48
         BEGIN                                                          NLCF  49
              TRS1 ~ TRS2 ~ 0.0 ;                                       NLCF  50
              IF RELTV THEN FOR S ~ 1 STEP 1 UNTIL N DO                 NLCF  51
         BEGIN                                                          NLCF  52
              TRS3 ~ G[S] / F[S] ;         TRS1 ~ TRS1 + TRS3 ;         NLCF  53
              TRS2 ~ TRS3*2 + TRS2                                      NLCF  54
         END                                                            NLCF  55
              ELSE FOR S ~ 1 STEP 1 UNTIL N DO                          NLCF  56
         BEGIN                                                          NLCF  57
              TRS3 ~ G[S] ;         TRS1 ~ F[S] | TRS3 + TRS1 ;         NLCF  58
              TRS2 ~ TRS3*2 + TRS2                                      NLCF  59
         END ;                                                          NLCF  60
              OMEGA ~ TRS1 / TRS2 ;                                     NLCF  61
              FOR I ~ 1 STEP 1 UNTIL Q DO B[I] ~ B[I] | OMEGA ;         NLCF  62
              FOR S ~ 1 STEP 1 UNTIL N DO                               NLCF  63
         BEGIN                                                          NLCF  64
              G[S] ~ G[S] | OMEGA ;                                     NLCF  65
              FOR I ~ Q + 1 STEP 1 UNTIL M DO                           NLCF  66
         BEGIN                                                          NLCF  67
              FOR J ~ Q + 1 STEP 1 UNTIL M DO                           NLCF  68
              PH[I,J,S] ~ PH[I,J,S] | OMEGA ;                           NLCF  69
              H[I,S] ~ H[I,S] | OMEGA                                   NLCF  70
         END                                                            NLCF  71
         END                                                            NLCF  72
         END ;                                                          NLCF  73
              TT ~ 0.0 ;               FOR S ~ 1 STEP 1 UNTIL N DO      NLCF  74
         BEGIN                                                          NLCF  75
              SS ~ T[S] ~ F[S] - G[S] ;                                 NLCF  76
              IF RELTV THEN                                             NLCF  77
         BEGIN                                                          NLCF  78
              TRS4 ~ 1.0 / F[S] ;                                       NLCF  79
              SS ~ T[S] ~ SS | TRS4 ;                                   NLCF  80
              FOR I ~ 1 STEP 1 UNTIL M DO                               NLCF  81
         BEGIN                                                          NLCF  82
              FOR J ~ I STEP 1 UNTIL M DO PH[I,J,S] ~ PH[I,J,S] | TRS4 ;NLCF  83
              H[I,S] ~ H[I,S] | TRS4                                    NLCF  84
         END                                                            NLCF  85
         END ;                                                          NLCF  86
              TT ~ SS | SS + TT                                         NLCF  87
         END  ;                                                         NLCF  88

COMMENT       TEST FOR CONVERGENCE. CORRECT DIVERGENCE BY DOUBLING      NLCF  89
              DIAGONAL ELEMENTS IN THE COEFFICIENT MATRIX PER LEVENBERG;NLCF  90

              IF (NOT LVNBG) OR (TEST = 0.0) OR (TT < TEST) THEN        NLCF  91
         BEGIN                                                          NLCF  92
              FOR I ~ 1 STEP 1 UNTIL M DO FOR J ~ I STEP 1 UNTIL M DO   NLCF  93
         BEGIN                                                          NLCF  94
              SS ~ 0.0 ;               FOR S ~ 1 STEP 1 UNTIL N DO      NLCF  95
              A[I,J] ~ SS ~ - PH[I,J,S] | T[S] + H[I,S] | H[J,S] + SS   NLCF  96
         END ;                                                          NLCF  97

COMMENT       TEST MATRIX FOR SINGULARITY ;                             NLCF  98

              IF A[1,1] = 0.0 THEN                                      NLCF  99
         BEGIN                                                          NLCF 100
              I ~ 1 ;          FOR I ~ I WHILE A[I,I] = 0.0 DO I ~ I+1 ;NLCF 101
              A[1,1] ~ A[I,I]                                           NLCF 102
         END ;                                                          NLCF 103
              FOR I ~ 2 STEP 1 UNTIL M DO IF A[I,I] = 0.0 THEN          NLCF 104
              A[I,I] ~ A[I-1,I-1] ;                                     NLCF 105

COMMENT       CALCULATE THE ELEMENTS OF THE RIGHT HAND SIDE ;           NLCF 106

              FOR I ~ 1 STEP 1 UNTIL M DO                               NLCF 107
         BEGIN                                                          NLCF 108
              SS ~ 0.0 ;         FOR S ~ 1 STEP 1 UNTIL N DO            NLCF 109
              SS ~ H[I,S] | T[S] + SS ;          C[I] ~ SS              NLCF 110
         END                                                            NLCF 111
         END                                                            NLCF 112
              ELSE                                                      NLCF 113
         BEGIN                                                          NLCF 114
              FOR I ~ 1 STEP 1 UNTIL M DO                               NLCF 115
         BEGIN                                                          NLCF 116
              A[I,I] ~ A[I,I] | 2.0 ;            B[I] ~ B[I] - BB[I]    NLCF 117
         END ;                                                          NLCF 118
              TT ~ TEST                                                 NLCF 119
         END  ;                                                         NLCF 120

COMMENT       THE CORRECTIONS TO THE CONSTANTS ARE CALCULATED BY CROUTS NLCF 121
              REDUCTION FOR A SYMMETRIC MATRIX ;                        NLCF 122

              FOR I ~ 1 STEP 1 UNTIL M DO                               NLCF 123
         BEGIN                                                          NLCF 124
              W ~ I - 1 ;          SS ~ A[I,I] ;          R[I,I] ~ 1.0 ;NLCF 125
              FOR K ~ 1 STEP 1 UNTIL W DO                               NLCF 126
         BEGIN                                                          NLCF 127
              RKI ~ R[K,I] ;       SS ~ - (U[K] ~ L[K] | RKI) | RKI + SSNLCF 128
         END ;                                                          NLCF 129
              L[I] ~ SS ;          LI ~ 1.0 / SS ;                      NLCF 130
              FOR J ~ I + 1 STEP 1 UNTIL M DO                           NLCF 131
         BEGIN                                                          NLCF 132
              SS ~ A[I,J] ;        FOR K ~ 1 STEP 1 UNTIL W DO          NLCF 133
              SS ~ - R[K,J] | U[K] + SS ;          R[I,J] ~ SS | LI     NLCF 134
         END ;                                                          NLCF 135
              SS ~ C[I] ;          FOR K ~ 1 STEP 1 UNTIL W DO          NLCF 136
              SS ~ - U[K] | CC[K] + SS ;            CC[I] ~ SS | LI     NLCF 137
         END ;                                                          NLCF 138
              BB[M] ~ CC[M] ;      FOR I ~ M - 1 STEP -1 UNTIL 1 DO     NLCF 139
         BEGIN                                                          NLCF 140
              SS ~ CC[I] ;         FOR K ~ I + 1 STEP 1 UNTIL M DO      NLCF 141
              SS ~ - R[I,K] | BB[K] + SS ;          BB[I] ~ SS          NLCF 142
         END ;                                                          NLCF 143

COMMENT       TEST ADJUSTMENTS IN CONSTANTS AND THE LAST CHANGE IN THE  NLCF 144
              SUM OF SQUARES OF DIFFERENCES ;                           NLCF 145

              IF (TEST = TT) OR (ABS(TEST-TT)/TT > TOLA) THEN GO TO INT;NLCF 146
              FOR I ~ 1 STEP 1 UNTIL M DO IF ABS(BB[I]/B[I]) > TOLB THENNLCF 147
              GO TO INT ;                    GO TO PRINT ;              NLCF 148

INT :         IF P1 THEN                                                NLCF 149
         BEGIN                                                          NLCF 150
              WRITE (CARD4, FMT1, OUT1) ;                               NLCF 151
              WRITE (CARD4, FMT2, OUT2)                                 NLCF 152
         END ;                                                          NLCF 153
              IF (COUNT { ITER) OR (LVNBG AND (TEST = TT)) THEN         NLCF 154
         BEGIN                                                          NLCF 155
              IF (NOT LVNBG) OR (LVNBG AND ((TEST = 0.0) OR TEST > TT)) NLCF 156
              THEN TEST ~ TT ;          FOR I ~ 1 STEP 1 UNTIL M DO     NLCF 157
              B[I] ~ B[I] + BB[I] ;          GO TO FUNC                 NLCF 158
         END  ;                                                         NLCF 159

PRINT :       IF RELTV THEN                                             NLCF 160
         BEGIN                                                          NLCF 161
              TT ~ 0.0 ;                     FOR S ~ 1 STEP 1 UNTIL N DONLCF 162
         BEGIN                                                          NLCF 163
              T[S] ~ T[S] | F[S] ;           TT ~ T[S] | T[S] + TT      NLCF 164
         END                                                            NLCF 165
         END  ;                                                         NLCF 166
              IF P2 THEN                                                NLCF 167
         BEGIN                                                          NLCF 168
              WRITE (CARD4, FMT3) ;          WRITE (CARD4, FMT4, OUT3)  NLCF 169
         END ;                                                          NLCF 170
              TEST ~ SS ~ 0.0 ;              FOR S ~ 1 STEP 1 UNTIL N DONLCF 171
         BEGIN                                                          NLCF 172
              X2S ~ F[S] ;                   SS ~ X2S + SS ;            NLCF 173
              TEST ~ X2S | X2S + TEST                                   NLCF 174
         END ;                                                          NLCF 175
              SS ~ SS*2 ;          SE ~ SQRT(TT / (N - M)) ;            NLCF 176
              SD ~ SQRT((N | TEST - SS) / ((N - 1) | N));               NLCF 177
              RR ~ - (TT / (SD*2 | (N - 1))) + 1.0 ;                    NLCF 178
              WRITE (CARD4, HEAD) ;          WRITE (CARD4, FMT1, OUT1) ;NLCF 179
              WRITE (CARD4, FMT2, OUT2) ;    WRITE (CARD4, FMT5, OUT4) ;NLCF 180
              FOR I ~ 1 STEP 1 UNTIL M DO BB[I] ~ 0.0 ;                 NLCF 181
              TEST ~ COUNT ~ 0.0                                        NLCF 182
         END ;                                                          NLCF 183
              GO TO START ;                                             NLCF 184
         END .                                                          NLCF 185
