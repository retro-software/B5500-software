         BEGIN                                                          MRCA   1
COMMENT       MULTIPLE REGRESSION AND CORRELATION ANALYSIS              MRCA   2
              BURROUGHS CORPORATION                                     MRCA   5
              PROGRAM CONTAINS 416 CARDS                                MRCA   6
              FIRST RELEASE DATE 12-1-64  ;                             MRCA   7
INTEGER       H, I, M, N, Q ;                                           MRCA   8
INTEGER ARRAY ID[0:4] ;                                                 MRCA   9
BOOLEAN       TR,MEAN,VAR,NORM,INV,RES,PCC,CC,CCINV,SWICH ;             MRCA  10
LABEL         START, LL1,NOMO ;                                         MRCA  11
FORMAT IN     FORM1 (5A6, 4I5, 6L5/3L5) ,                               MRCA  12
              FORM2 (26I3) ,                                            MRCA  13
              FORM3 (5E13.6) ;                                          MRCA  14
FORMAT OUT    TITLE(X34,"GENERAL MULTIPLE REGRESSION AND ",             MRCA  15
                    "CORRELATION ANALYSIS"///X45,5A6//                  MRCA  16
                    X1,"SAMPLE SIZE",X2,I5/                             MRCA  17
                    X1,"NO. OF INDEPENDENT VARIABLES",X2,I3/            MRCA  18
                    X1,"NO. OF DEPENDENT VARIABLES",X4,I3//) ,          MRCA  19
              FRM1 (X1,"NORMAL MATRIX"/) ,                              MRCA  20
              FRM2 (X1,"ROW",I3/(6(X1,I3,X1,E15.8)/)) ,                 MRCA  21
              FRM3 (//X1,"MEANS OF INDEPENDENT AND " ,                  MRCA  22
                    "DEPENDENT VARIABLES"/) ,                           MRCA  23
              FRM4 ((6(X1,I3,X1,E15.8)/)/) ,                            MRCA  24
              FRM5 (/X1,"VARIANCES OF INDEPENDENT AND " ,               MRCA  25
                    "DEPENDENT VARIABLES"/) ,                           MRCA  26
              FRM6 (/X1,"THE INVERSE MATRIX"/) ,                        MRCA  27
              FRM7 (/X1,"PRODUCT OF NORMAL AND INVERSE " ,              MRCA  28
                    "MATRICES"/) ,                                      MRCA  29
              FRM8 (/X47,"DEPENDENT VARIABLE NO.",I3/) ,                MRCA  30
              FRM9 (X1,"REGRESSION COEFFICIENTS"//                      MRCA  31
                    X1,"THE CONSTANT TERM IS",X3,E15.8) ,               MRCA  32
              FRM10(/X1,"THE ERROR SUM SQUARE IS",X3,E15.8//            MRCA  33
                    X1,"THE ERROR MEAN SQUARE IS",X2,E15.8//            MRCA  34
                    X1,"THE STANDARD ERROR OF ESTIMATE IS" ,            MRCA  35
                    X3,E15.8//                                          MRCA  36
                    X1,"THE COEFFICIENT OF DETERMINATION " ,            MRCA  37
                    "IS",X6,E15.8//                                     MRCA  38
                    X1,"THE MULTIPLE CORRELATION " ,                    MRCA  39
                    "COEFFICIENT IS",X2,E15.8//) ,                      MRCA  40
              FRM11(X1,"STANDARD DEVIATIONS OF THE " ,                  MRCA  41
                    "REGRESSION COEFFICIENTS"/) ,                       MRCA  42
              FRM12(/X1,"T VALUE OF REGRESSION " ,                      MRCA  43
                    "COEFFICIENTS"/) ,                                  MRCA  44
              FRM13(/X36,"ANALYSIS OF VARIANCE"/X29,                    MRCA  45
                    "FOR THE MULTIPLE LINEAR REGRESSION"//              MRCA  46
                    X7,"SOURCE OF VARIATION",X7,"D.F.",X8,              MRCA  47
                   "SUM OF",X13,"MEAN",X15,"F"/X45,                     MRCA  48
                    "SQUARES",X11,"SQUARES",X11,"RATIO"/                MRCA  49
                    X1,"DUE TO REGRESSION",X14,I5,X3,E15.8,             MRCA  50
                    X3,E15.8,X3,E15.8/                                  MRCA  51
                    X1,"DEVIATION ABOUT REGRESSION",X5,I5,              MRCA  52
                    X3,E15.8,X3,E15.8/                                  MRCA  53
                    X22,"TOTAL",X5,I5,X3,E15.8//) ,                     MRCA  54
              FRM14(/X51,"TABLE OF RESIDUALS"//                         MRCA  55
                    X25,"OBSERVATION",X8,"Y CALCULATED",X9,             MRCA  56
                    "Y OBSERVED",X11,"RESIDUAL") ,                      MRCA  57
              FRM15(X28,I5,X9,E15.8,X5,E15.8,X5,E15.8) ,                MRCA  58
              FRM16(//X25,"TEST OF EXTREME RESIDUALS"//                 MRCA  59
                    X25,"RANGE OF RESIDUALS",X20,E15.8/                 MRCA  60
                    X25,"RANGE / STANDARD ERROR OF " ,                  MRCA  61
                    "ESTIMATE",X4,E15.8//) ,                            MRCA  62
              FRM17(X1,"CORRELATION COEFFICIENTS"/) ,                   MRCA  63
              FRM18(/X1,"INVERSE OF CORRELATION MATRIX"/) ,             MRCA  64
              FRM19(/X1,"PARTIAL CORRELATION COEFFICIENTS"/) ;          MRCA  65
FILE IN       CARDR(1,10) ;                                             MRCA  66
FILE OUT      PRINT 1 (1,15) ;                                          MRCA  67
START:   BEGIN                                                          MRCA  68
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],M,N,Q,             MRCA  69
                   H,TR,MEAN,VAR,NORM,INV,RES,PCC,CC,CCINV);            MRCA  70
              READ (CARDR,FORM1,HEAD) [NOMO] ;                          MRCA  71
         END ;                                                          MRCA  72
              SWICH ~ FALSE ;                                           MRCA  73
LL1:     BEGIN                                                          MRCA  74
INTEGER       J, K, L, P ;                                              MRCA  75
ARRAY         X[0:Q],YSQ[0:Q-N],A[0:N+1,0:Q],B[0:H],SB[0:N] ,           MRCA  76
              T[0:Q] ;                                                  MRCA  77
REAL          C, RSS, RMS, F, ERS, EMS, SE, CD, MCC, HI, LO, R, S ;     MRCA  78
LABEL         RERED, RESDU ;                                            MRCA  79
LIST          TRAN (FOR J ~ 1 STEP 1 UNTIL Q DO T[J]) ,                 MRCA  80
              DATA (FOR I ~ 1 STEP 1 UNTIL H DO B[I]) ,                 MRCA  81
              OUT1 (FOR J ~ 0 STEP 1 UNTIL 4 DO ID[J], M, N,            MRCA  82
                    L) ,                                                MRCA  83
              OUT2 (I, FOR J ~ I STEP 1 UNTIL N DO                      MRCA  84
                    [J, A[I,J]]) ,                                      MRCA  85
              OUT3 (FOR I ~ 1 STEP 1 UNTIL N DO [I, B[I]]) ,            MRCA  86
              OUT4 (FOR I ~ 1 STEP 1 UNTIL N DO [I, SB[I]]),            MRCA  87
              OUT5 (FOR I ~ 1 STEP 1 UNTIL N DO [I, X[I]]) ,            MRCA  88
              OUT6 (I, FOR J ~ I STEP 1 UNTIL N DO                      MRCA  89
                    [J, X[J]]) ;                                        MRCA  90
PROCEDURE     INVERT (N, A) ;                                           MRCA  91
VALUE         N ;                                                       MRCA  92
INTEGER       N ;                                                       MRCA  93
ARRAY         A[0,0] ;                                                  MRCA  94
COMMENT       THIS PROCEDURE INVERTS A SYMMETRIC MATRIX                 MRCA  95
              A[I,J] OF WHICH THE UPPER HALF IS GIVEN ;                 MRCA  96
         BEGIN                                                          MRCA  97
INTEGER       I, J, K, L ;                                              MRCA  98
ARRAY         Z[0:N] ;                                                  MRCA  99
REAL          DG, R ;                                                   MRCA 100
              FOR K ~ 1 STEP 1 UNTIL N-1 DO                             MRCA 101
         BEGIN                                                          MRCA 102
              DG ~ A[K,K] ;                                             MRCA 103
              FOR I ~ K+1 STEP 1 UNTIL N DO                             MRCA 104
         BEGIN                                                          MRCA 105
              R ~ A[K,I] / DG ;                                         MRCA 106
         FOR J ~ I STEP 1 UNTIL N DO                                    MRCA 107
              A[I,J] ~ A[I,J] - A[K,J] | R ;                            MRCA 108
              FOR L ~ 1 STEP 1 UNTIL K-1 DO                             MRCA 109
              A[L,I] ~ A[L,I] - A[L,K] | R ;                            MRCA 110
              A[K,I] ~ -R                                               MRCA 111
         END                                                            MRCA 112
         END ;                                                          MRCA 113
COMMENT       BACKWARD PASS ;                                           MRCA 114
              FOR K ~ 1 STEP 1 UNTIL N DO                               MRCA 115
         BEGIN                                                          MRCA 116
              FOR J ~ K+1 STEP 1 UNTIL N DO                             MRCA 117
              Z[J] ~ A[K,J] / A[J,J] ;                                  MRCA 118
              Z[K] ~ 1 / A[K,K] ;                                       MRCA 119
              FOR I ~ K STEP 1 UNTIL N DO                               MRCA 120
         BEGIN                                                          MRCA 121
         FOR J ~ I+1 STEP 1 UNTIL N DO                                  MRCA 122
              Z[I] ~ Z[I] + Z[J] | A[I,J] ;                             MRCA 123
              A[K,I] ~ Z[I]                                             MRCA 124
         END                                                            MRCA 125
         END                                                            MRCA 126
         END  INVERT ;                                                  MRCA 127
LABEL         NAXT, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10,            MRCA 128
              L11, L12, L13, L14, L15 ;                                 MRCA 129
SWITCH        U ~ L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L1  ;  MRCA 130
FILE          TAPEA (2,Q+1) ;                                           MRCA 131
              IF TR THEN                                                MRCA 132
              READ (CARDR,FORM2,TRAN) ;                                 MRCA 133
              FOR I ~ 0 STEP 1 UNTIL N DO                               MRCA 134
              FOR J ~ I STEP 1 UNTIL Q DO                               MRCA 135
              A[I,J] ~ 0 ;                                              MRCA 136
              L ~ Q-N ;                                                 MRCA 137
              FOR P ~ 1 STEP 1 UNTIL L DO                               MRCA 138
              YSQ[P] ~ 0 ;                                              MRCA 139
              FOR L ~ 1 STEP 1 UNTIL M DO                               MRCA 140
         BEGIN                                                          MRCA 141
              READ (CARDR,FORM3,DATA) ;                                 MRCA 142
              FOR J ~ 1 STEP 1 UNTIL Q DO                               MRCA 143
         BEGIN                                                          MRCA 144
              IF TR THEN                                                MRCA 145
         BEGIN                                                          MRCA 146
              GO TO U[T[J]] ;                                           MRCA 147
L1:           X[J] ~ B[J] ;                                             MRCA 148
              GO TO NAXT ;                                              MRCA 149
L2:           X[J] ~ B[J] | B[J] ;                                      MRCA 150
              GO TO NAXT ;                                              MRCA 151
L3:           X[J] ~ 1 / B[J] ;                                         MRCA 152
              GO TO NAXT ;                                              MRCA 153
L4:           X[J] ~ SQRT (B[J]) ;                                      MRCA 154
              GO TO NAXT ;                                              MRCA 155
L5:           X[J] ~ EXP (B[J]) ;                                       MRCA 156
              GO TO NAXT ;                                              MRCA 157
L6:           X[J] ~ LN (B[J]) ;                                        MRCA 158
              GO TO NAXT ;                                              MRCA 159
L7:           X[J] ~ SIN (B[J]) ;                                       MRCA 160
              GO TO NAXT ;                                              MRCA 161
L8:           X[J] ~ COS (B[J]) ;                                       MRCA 162
              GO TO NAXT ;                                              MRCA 163
L9:           X[J] ~ B[J+1] ;                                           MRCA 164
              GO TO NAXT ;                                              MRCA 165
L10:          X[J] ~ B[J+2] ;                                           MRCA 166
              GO TO NAXT ;                                              MRCA 167
L11:          X[J] ~ B[J+3] ;                                           MRCA 168
              GO TO NAXT ;                                              MRCA 169
L12:          X[J] ~ B[J+4] ;                                           MRCA 170
              GO TO NAXT ;                                              MRCA 171
L13:          X[J] ~ B[J+5] ;                                           MRCA 172
              GO TO NAXT ;                                              MRCA 173
L14:          X[J] ~ B[J+6] ;                                           MRCA 174
              GO TO NAXT ;                                              MRCA 175
L15:          X[J] ~ B[J+7] ;                                           MRCA 176
              GO TO NAXT ;                                              MRCA 177
NAXT:    END                                                            MRCA 178
              ELSE                                                      MRCA 179
              X[J] ~ B[J]                                               MRCA 180
         END ;                                                          MRCA 181
              WRITE(TAPEA,Q+1,X[*])                                     MRCA 182
         END;                                                           MRCA 183
              REWIND(TAPEA) ;                                           MRCA 184
RERED:        FOR L ~ 1 STEP 1 UNTIL M DO                               MRCA 185
         BEGIN                                                          MRCA 186
              READ (TAPEA,Q+1,X[*]) ;                                   MRCA 187
              IF SWICH THEN                                             MRCA 188
         BEGIN                                                          MRCA 189
              F ~ B[0] ;                                                MRCA 190
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 191
              F ~ F + B[I] | X[I] ;                                     MRCA 192
              C ~ F - X[P] ;                                            MRCA 193
              WRITE (PRINT,FRM15,L,F,X[P],C) ;                          MRCA 194
              IF HI < C THEN                                            MRCA 195
              HI ~ C ;                                                  MRCA 196
              IF LO > C THEN                                            MRCA 197
              LO ~ C                                                    MRCA 198
         END                                                            MRCA 199
              ELSE                                                      MRCA 200
         BEGIN                                                          MRCA 201
              FOR K ~ 1 STEP 1 UNTIL Q DO                               MRCA 202
         BEGIN                                                          MRCA 203
              C ~ X[K] ;                                                MRCA 204
              A[0,K] ~ A[0,K] + C ;                                     MRCA 205
              IF K { N THEN                                             MRCA 206
         BEGIN                                                          MRCA 207
              FOR J ~ K STEP 1 UNTIL Q DO                               MRCA 208
              A[K,J] ~ A[K,J] + C | X[J]                                MRCA 209
         END                                                            MRCA 210
         END ;                                                          MRCA 211
              FOR J ~ N+1 STEP 1 UNTIL Q DO                             MRCA 212
              YSQ[J-N] ~ YSQ[J-N] + X[J] | X[J]                         MRCA 213
         END                                                            MRCA 214
         END ;                                                          MRCA 215
              IF SWICH THEN                                             MRCA 216
              GO TO RESDU ;                                             MRCA 217
              FOR I ~ 1 STEP 1 UNTIL Q DO                               MRCA 218
         BEGIN                                                          MRCA 219
              R ~ A[0,I] / M ;                                          MRCA 220
              IF I { N THEN                                             MRCA 221
         BEGIN                                                          MRCA 222
              C ~ A[I,I] - A[0,I] | R ;                                 MRCA 223
              A[I,I] ~ C ;                                              MRCA 224
              A[I,0] ~ C ;                                              MRCA 225
              FOR J ~ I+1 STEP 1 UNTIL N DO                             MRCA 226
         BEGIN                                                          MRCA 227
              C ~ A[I,J] - A[0,J]|R ;                                   MRCA 228
              A[I,J] ~ C ;                                              MRCA 229
              A[J,I] ~ C                                                MRCA 230
         END ;                                                          MRCA 231
              FOR J ~ N+1 STEP 1 UNTIL Q DO                             MRCA 232
              A[I,J] ~ A[I,J] - A[0,J]|R                                MRCA 233
         END                                                            MRCA 234
              ELSE                                                      MRCA 235
              YSQ[I-N] ~ YSQ[I-N] - A[0,I] | R ;                        MRCA 236
              A[0,I] ~ R                                                MRCA 237
         END ;                                                          MRCA 238
              L ~ Q-N ;                                                 MRCA 239
              WRITE(PRINT[PAGE]) ;                                      MRCA 240
              WRITE(PRINT,TITLE,OUT1) ;                                 MRCA 241
COMMENT       PRINTOUT OF NORMAL MATRIX ;                               MRCA 242
              IF NORM THEN                                              MRCA 243
         BEGIN                                                          MRCA 244
              WRITE (PRINT,FRM1) ;                                      MRCA 245
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 246
              WRITE (PRINT,FRM2,OUT2)                                   MRCA 247
         END ;                                                          MRCA 248
COMMENT       COMPUTATION OF MEANS AND VARIANCES ;                      MRCA 249
              IF MEAN THEN                                              MRCA 250
         BEGIN                                                          MRCA 251
              FOR I ~ 1 STEP 1 UNTIL Q DO                               MRCA 252
              X[I] ~ A[0,I] ;                                           MRCA 253
              WRITE (PRINT,FRM3) ;                                      MRCA 254
              L ~ N ;                                                   MRCA 255
              N ~ Q ;                                                   MRCA 256
              WRITE (PRINT,FRM4,OUT5) ;                                 MRCA 257
              N ~ L                                                     MRCA 258
         END ;                                                          MRCA 259
              IF VAR THEN                                               MRCA 260
         BEGIN                                                          MRCA 261
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 262
              X[I] ~ A[I,0] / (M-1) ;                                   MRCA 263
              FOR I ~ N+1 STEP 1 UNTIL Q DO                             MRCA 264
              X[I] ~ YSQ[I-N] / (M-1) ;                                 MRCA 265
              WRITE (PRINT,FRM5) ;                                      MRCA 266
              L ~ N ;                                                   MRCA 267
              N ~ Q ;                                                   MRCA 268
              WRITE (PRINT,FRM4,OUT5) ;                                 MRCA 269
              N ~ L                                                     MRCA 270
         END ;                                                          MRCA 271
              INVERT (N, A) ;                                           MRCA 272
COMMENT       PRINTOUT OF INVERSE MATRIX ;                              MRCA 273
              IF INV THEN                                               MRCA 274
         BEGIN                                                          MRCA 275
              WRITE (PRINT,FRM6) ;                                      MRCA 276
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 277
              WRITE (PRINT,FRM2,OUT2) ;                                 MRCA 278
COMMENT       PRODUCT OF NORMAL AND INVERSE MATRICES ;                  MRCA 279
              WRITE (PRINT,FRM7) ;                                      MRCA 280
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 281
         BEGIN                                                          MRCA 282
              FOR J ~ I STEP 1 UNTIL N DO                               MRCA 283
         BEGIN                                                          MRCA 284
              X[J] ~ 0 ;                                                MRCA 285
              FOR L ~ 1 STEP 1 UNTIL I-1 DO                             MRCA 286
              X[J] ~ X[J] + A[L,I] | A[J,L] ;                           MRCA 287
              FOR L ~ I STEP 1 UNTIL N DO                               MRCA 288
         BEGIN                                                          MRCA 289
              IF L < J THEN                                             MRCA 290
              S ~ A[J,L]                                                MRCA 291
              ELSE IF L > J THEN                                        MRCA 292
              S ~ A[L,J]                                                MRCA 293
              ELSE                                                      MRCA 294
              S ~ A[J,0] ;                                              MRCA 295
              X[J] ~ X[J] + S | A[I,L]                                  MRCA 296
         END                                                            MRCA 297
         END ;                                                          MRCA 298
              WRITE (PRINT,FRM2,OUT6)                                   MRCA 299
         END                                                            MRCA 300
         END ;                                                          MRCA 301
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 302
         BEGIN                                                          MRCA 303
              R ~ A[I,0] ;                                              MRCA 304
              FOR J ~ I+1 STEP 1 UNTIL N DO                             MRCA 305
              A[J,I] ~ A[J,I] / SQRT (R | A[J,0])                       MRCA 306
         END ;                                                          MRCA 307
COMMENT       CONSIDER EACH DEPENDENT VARIABLE SEPARATELY ;             MRCA 308
              FOR P ~ N+1 STEP 1 UNTIL Q DO                             MRCA 309
         BEGIN                                                          MRCA 310
COMMENT       COMPUTATION OF REGRESSION COEFFICIENTS ;                  MRCA 311
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 312
         BEGIN                                                          MRCA 313
              B[I] ~ 0 ;                                                MRCA 314
              FOR L ~ 1 STEP 1 UNTIL I-1 DO                             MRCA 315
              B[I] ~ B[I] + A[L,I] | A[L,P] ;                           MRCA 316
              FOR L ~ I STEP 1 UNTIL N DO                               MRCA 317
              B[I] ~ B[I] + A[I,L] | A[L,P]                             MRCA 318
         END ;                                                          MRCA 319
              B[0] ~ A[0,P] ;                                           MRCA 320
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 321
              B[0] ~ B[0] - B[I] | A[0,I] ;                             MRCA 322
COMMENT       COMPUTATION OF REGRESSION SUM OF SQUARES ;                MRCA 323
              RSS ~ 0 ;                                                 MRCA 324
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 325
              RSS ~ RSS + B[I] | A[I,P] ;                               MRCA 326
              RMS ~ RSS / N ;                                           MRCA 327
COMMENT       COMPUTATION OF ERROR SUM SQUARE ;                         MRCA 328
              C ~ YSQ[P-N] ;                                            MRCA 329
              ERS ~ C - RSS ;                                           MRCA 330
COMMENT       COMPUTATION OF ERROR MEAN SQUARE AND                      MRCA 331
              STANDARD ERROR OF ESTIMATE ;                              MRCA 332
              K ~ M-1 ;                                                 MRCA 333
              L ~ K-N ;                                                 MRCA 334
              EMS ~ ERS / L ;                                           MRCA 335
              SE ~ SQRT (EMS) ;                                         MRCA 336
COMMENT       COMPUTATION OF COEFFICIENT OF DETERMINATION ;             MRCA 337
              CD ~ RSS / C ;                                            MRCA 338
COMMENT       MULTIPLE CORRELATION COEFFICIENT ;                        MRCA 339
              MCC ~ SQRT (CD) ;                                         MRCA 340
COMMENT       COMPUTATION OF STANDARD DEVIATIONS AND T                  MRCA 341
              VALUES OF REGRESSION COEFFICIENTS ;                       MRCA 342
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 343
         BEGIN                                                          MRCA 344
              S ~ SQRT (EMS | A[I,I]) ;                                 MRCA 345
              SB[I] ~ S ;                                               MRCA 346
              X[I] ~ B[I] / S                                           MRCA 347
         END ;                                                          MRCA 348
              I ~ P-N ;                                                 MRCA 349
              WRITE (PRINT,FRM8,I) ;                                    MRCA 350
              WRITE (PRINT,FRM9,B[0]) ;                                 MRCA 351
              WRITE (PRINT,FRM4,OUT3) ;                                 MRCA 352
              WRITE (PRINT,FRM10,ERS,EMS,SE,CD,MCC) ;                   MRCA 353
              WRITE (PRINT,FRM11) ;                                     MRCA 354
              WRITE (PRINT,FRM4,OUT4) ;                                 MRCA 355
              WRITE (PRINT,FRM12) ;                                     MRCA 356
              WRITE (PRINT,FRM4,OUT5) ;                                 MRCA 357
COMMENT       ANALYSIS OF VARIANCE ;                                    MRCA 358
              F ~ RMS / EMS ;                                           MRCA 359
              WRITE (PRINT,FRM13,N,RSS,RMS,F,L,ERS,EMS,K,C);            MRCA 360
COMMENT       COMPUTATION OF RESIDUES ;                                 MRCA 361
              IF RES THEN                                               MRCA 362
         BEGIN                                                          MRCA 363
              REWIND(TAPEA) ;                                           MRCA 364
              SWICH ~ TRUE ;                                            MRCA 365
              WRITE (PRINT,FRM14) ;                                     MRCA 366
              HI ~ -0.9@+49 ;                                           MRCA 367
              LO ~  0.9@+49 ;                                           MRCA 368
              GO TO RERED ;                                             MRCA 369
RESDU:        CD ~ HI - LO ;                                            MRCA 370
              F ~ CD / SE ;                                             MRCA 371
              WRITE (PRINT,FRM16,CD,F)                                  MRCA 372
         END                                                            MRCA 373
         END ;                                                          MRCA 374
              IF PCC THEN                                               MRCA 375
         BEGIN                                                          MRCA 376
COMMENT       CONSIDER EACH DEPENDENT VARIABLE SEPARATELY ;             MRCA 377
              FOR P ~ N+1 STEP 1 UNTIL Q DO                             MRCA 378
         BEGIN                                                          MRCA 379
COMMENT       GET AUGMENTED CORRELATION COEFFICIENT MATRIX ;            MRCA 380
MONITOR  PRINT(A, L);
              L ~ P-N ;                                                 MRCA 389
              FOR I ~ 1 STEP 1 UNTIL N+1 DO                             MRCA 381
              A[I,I] ~ 1 ;                                              MRCA 382
              FOR K ~ 1 STEP 1 UNTIL N DO                               MRCA 383
         BEGIN                                                          MRCA 384
              FOR J ~ K+1 STEP 1 UNTIL N DO                             MRCA 385
              A[K,J] ~ A[J,K] ;                                         MRCA 386
              A[K,N+1] ~ A[K,N+1] / SQRT (A[K,0] | YSQ[L])              MRCA 387
         END ;                                                          MRCA 388
              WRITE (PRINT,FRM8,L) ;                                    MRCA 390
              N ~ N+1 ;                                                 MRCA 391
              IF CC THEN                                                MRCA 392
         BEGIN                                                          MRCA 393
              WRITE (PRINT,FRM17) ;                                     MRCA 394
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 395
              WRITE (PRINT,FRM2,OUT2)                                   MRCA 396
         END ;                                                          MRCA 397
COMMENT       INVERT AUGMENTED CORRELATION MATRIX ;                     MRCA 398
              INVERT (N, A) ;                                           MRCA 399
              IF CCINV THEN                                             MRCA 400
         BEGIN                                                          MRCA 401
              WRITE (PRINT,FRM18) ;                                     MRCA 402
              FOR I ~ 1 STEP 1 UNTIL N DO                               MRCA 403
              WRITE (PRINT,FRM2,OUT2)                                   MRCA 404
         END ;                                                          MRCA 405
COMMENT       DETERMINE PARTIAL CORRELATION COEFFICIENTS ;              MRCA 406
              R ~ A[N,N] ;                                              MRCA 407
              N ~ N-1 ;                                                 MRCA 408
              FOR J ~ 1 STEP 1 UNTIL N DO                               MRCA 409
              X[J] ~ -A[J,N+1] / SQRT (R | A[J,J]) ;                    MRCA 410
              WRITE (PRINT,FRM19) ;                                     MRCA 411
              WRITE (PRINT,FRM4,OUT5) ;                                 MRCA 412
COMMENT       MOVE COLUMN FOR NEXT DEPENDENT VARIABLE ;                 MRCA 413
              IF P+1 { Q THEN                                           MRCA 414
         BEGIN                                                          MRCA 415
              FOR J ~ 1 STEP 1 UNTIL N+1 DO                             MRCA 416
              A[J,N+1] ~ A[J,P+1]                                       MRCA 417
         END                                                            MRCA 418
         END                                                            MRCA 419
         END                                                            MRCA 420
         END ;                                                          MRCA 421
              GO TO START ;                                             MRCA 422
NOMO:    END  PROGRAM.                                                  MRCA 423
MULT. REGRESSION & CORRELATION    9    2    4    4FALSE TRUE TRUE TRUE TRUE TRUE
 TRUE TRUE TRUE
 +.130000,+01 +.158000,+02 +.134000,+02 +.109000,+02
 +.200000,+01 +.206000,+02 +.181000,+02 +.114500,+02
 +.270000,+01 +.238000,+02 +.167000,+02 +.120000,+02
 +.240000,+01 +.217000,+02 +.192000,+02 +.121000,+02
 +.330000,+01 +.214000,+02 +.151000,+02 +.124500,+02
 +.290000,+01 +.215000,+02 +.148000,+02 +.123000,+02
 +.280000,+01 +.181000,+02 +.117000,+02 +.123500,+02
 +.370000,+01 +.238000,+02 +.156000,+02 +.129000,+02
 +.260000,+01 +.239000,+02 +.170000,+02 +.118500,+02
