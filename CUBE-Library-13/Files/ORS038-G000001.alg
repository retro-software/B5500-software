         BEGIN                                                          FAN    1
COMMENT       FACTOR ANALYSIS                                           FAN    2
              CHARLES L. CLARK                                          FAN    3
              PROFESSIONAL SERVICES DIVISIONAL GROUP                    FAN    4
              BURROUGHS CORPORATION                                     FAN    5
              PROGRAM CONTAINS 712 CARDS                                FAN    6
              FIRST RELEASE DATE DEC. 1, 1964  ;                        FAN    7
INTEGER       I,NVAR,NSAM,CAS,NRO ;                                     FAN    8
INTEGER ARRAY ID[0:4] ;                                                 FAN    9
BOOLEAN       TR ;                                                      FAN   10
REAL          LIM ;                                                     FAN   11
LABEL         START,NOMO,LL1 ;                                          FAN   12
FORMAT IN     FORM1 (5A6,I3,I5,I3,F5.0,I3,L5) ,                         FAN   13
              FORM2 (12I6),                                             FAN   14
              FORM3 (26I3),                                             FAN   15
              FORM4 (12F6.1) ;                                          FAN   16
FORMAT OUT    TITLE(X52,"FACTOR ANALYSIS"///),                          FAN   17
              FRM1 (X45,5A6//X1,"CASE",I3/X1,"NUMBER OF VARIABLES",I4/  FAN   18
                    X1,"SAMPLE SIZE",I6),                               FAN   19
              FRM2 (//X1,"MEAN VALUE OF VARIABLES"),                    FAN   20
              FRM3 (8F15.5),                                            FAN   21
              FRM4 (//X1,"STANDARD DEVIATIONS"),                        FAN   22
              FRM5 (//X1,"CORRELATION COEFFICIENTS"),                   FAN   23
              FRM6 (/X1,"ROW",I3/(10F12.5)/),                           FAN   24
              FRM7 (//X1,"EIGENVALUES"),                                FAN   25
              FRM8 (//X7,"ACCURACY INSUFFICIENT FOR MORE EIGENVALUES"), FAN   26
              FRM9 (//X1,"CUMULATIVE PROPORTION OF TOTAL VARIANCE"),    FAN   27
              FRM10(//X1,"EIGENVECTORS"),                               FAN   28
              FRM11(/X1,"VECTOR",I3/(10F12.5)/),                        FAN   29
              FRM12(//X1,"FACTOR MATRIX"),                              FAN   30
              FRM13(/X1,"VARIABLE",I3/(10F12.5)/),                      FAN   31
              FRM14(//X1,"CHECK MATRIX - EIGENVALUES ON DIAGONAL"),     FAN   32
              FRM15(//X1,"CHECK MATRIX - CORRELATION COEFFICIENTS"),    FAN   33
              FRM16(//X1,"VALUE GIVEN TO LIMIT NUMBER OF FACTORS TO BE",FAN   34
                    " ROTATED",F9.4/),                                  FAN   35
              FRM17(//X1,"FACTOR MATRIX CAN NOT BE ROTATED, CHANGE THE",FAN   36
                    " LIMIT VALUE LIM"/),                               FAN   37
              FRM18(/X1,"NUMBER OF FACTORS ROTATED",I4/X1,"NUMBER OF ", FAN   38
                    "ITERATION CYCLES",I3///X1,"ROTATED FACTOR MATRIX"),FAN   39
              FRM19(/X1,"ORIGINAL AND SUCCESSIVE VARIANCES"//X3,"CYCLE",FAN   40
                    " NO.",X10,"VARIANCES"),                            FAN   41
              FRM20(I8,X8,F15.7),                                       FAN   42
              FRM21(//X1,"CHECK ON COMMUNALITIES"//X3,"VARIABLES",X8,   FAN   43
                    "ORIGINAL",X9,"FINAL",X8,"DIFFERENCE"),             FAN   44
              FRM22(I8,X5,3F15.5) ;                                     FAN   45
FILE IN       CARDR   (1,10) ;                                          FAN   46
FILE OUT      PRINT 1 (1,15) ;                                          FAN   47
START:   BEGIN                                                          FAN   48
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NVAR,NSAM,CAS,     FAN   49
                   LIM,NRO,TR) ;                                        FAN   50
              READ (CARDR,FORM1,HEAD) [NOMO] ;                          FAN   51
              WRITE(PRINT[PAGE]) ;                                      FAN   52
              WRITE(PRINT,TITLE)                                        FAN   53
         END ;                                                          FAN   54
LL1:     BEGIN                                                          FAN   55
INTEGER       EX ;                                                      FAN   56
REAL          NS,NSM1,RN ;                                              FAN   57
LABEL         LL2 ;                                                     FAN   58
              NS ~ NSAM ;                                               FAN   59
              NSM1 ~ NS - 1.0 ;                                         FAN   60
              EX ~ NVAR + 50 ;                                          FAN   61
              RN ~ 0.0 ;                                                FAN   62
LL2:     BEGIN                                                          FAN   63
INTEGER       J,JJ,K,KK,L,LL,II,M,MM,Z ;                                FAN   64
INTEGER ARRAY T[0:99],SC[0:NVAR] ;                                      FAN   65
REAL          NORM,T1,T2,SYN,KOS,P,TA,TE,BE,W,S,MARY,B ;                FAN   66
REAL ARRAY    A[0:NVAR,0:NVAR],DATA[0:EX],SCA[0:NVAR,0:NVAR],           FAN   67
              SUMXX[0:NVAR,0:NVAR],R[0:NVAR,0:NVAR],DIAG[0:NVAR],       FAN   68
              VAL[0:NVAR],SD[0:NVAR],X[0:NVAR],SUMX[0:NVAR] ;           FAN   69
LABEL         NAXT,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,  FAN   70
              LL3,LL23,LL24,LL25,LL26,LL27,LL33,LL34,LL35 ;             FAN   71
LIST          SCAL(FOR I ~ 1 STEP 1 UNTIL NVAR DO SC[I]),               FAN   72
              TRAN(FOR J ~ 1 STEP 1 UNTIL NVAR DO T[J]),                FAN   73
              DATR(FOR L ~ 1 STEP 1 UNTIL NVAR DO DATA[L]),             FAN   74
              OUTT(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],CAS,NVAR,NSAM),    FAN   75
              AVEX(FOR I ~ 1 STEP 1 UNTIL NVAR DO SUMX[I]),             FAN   76
              SDEV(FOR K ~ 1 STEP 1 UNTIL NVAR DO A[K,1]),              FAN   77
              CORR(I,FOR J ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]),          FAN   78
              EVAL(FOR I ~ 1 STEP 1 UNTIL Z DO SUMX[I]),                FAN   79
              CUMP(FOR I ~ 1 STEP 1 UNTIL Z DO A[I,1]),                 FAN   80
              EVEC(J,FOR I ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]),          FAN   81
              FACT(I,FOR J ~ 1 STEP 1 UNTIL Z DO A[I,J]),               FAN   82
              CHEK(I,FOR J ~ 1 STEP 1 UNTIL Z DO R[I,J]),               FAN   83
              ROTM(I,FOR J ~ 1 STEP 1 UNTIL NRO DO A[I,J]),             FAN   84
              CAS4(FOR J ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]) ;           FAN   85
SWITCH        U ~ L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15 ;  FAN   86
FILE          TAPEB 2 (1,10) ;                                          FAN   87
              WRITE(PRINT,FRM1,OUTT) ;                                  FAN   88
              READ (CARDR,FORM2,SCAL) ;                                 FAN   89
              IF CAS = 4 THEN                                           FAN   90
         BEGIN                                                          FAN   91
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN   92
         BEGIN                                                          FAN   93
              READ (CARDR,FORM4,CAS4) ;                                 FAN   94
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN   95
              SCA[I,J] ~ SCA[I,J]|10.0*SC[J]                            FAN   96
         END ;                                                          FAN   97
              GO TO LL33                                                FAN   98
         END ;                                                          FAN   99
              IF CAS = 1 THEN                                           FAN  100
              RN ~ 1.0 ;                                                FAN  101
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  102
         BEGIN                                                          FAN  103
              A[I,2] ~ 0.0 ;                                            FAN  104
              A[I,3] ~ 0.0 ;                                            FAN  105
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  106
              SUMXX[I,J] ~ 0.0                                          FAN  107
         END ;                                                          FAN  108
              IF TR THEN                                                FAN  109
              READ (CARDR,FORM3,TRAN) ;                                 FAN  110
              FOR M ~ 1 STEP 1 UNTIL NSAM DO                            FAN  111
         BEGIN                                                          FAN  112
              READ (CARDR,FORM4,DATR) ;                                 FAN  113
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  114
         BEGIN                                                          FAN  115
              X[J] ~ DATA[J]|10.0*SC[J] ;                               FAN  116
              DATA[J] ~ X[J]                                            FAN  117
         END ;                                                          FAN  118
              IF TR THEN                                                FAN  119
         BEGIN                                                          FAN  120
              FOR J ~ NVAR STEP -1 UNTIL 1 DO                           FAN  121
         BEGIN                                                          FAN  122
              GO TO U[T[J]] ;                                           FAN  123
L1:           GO TO NAXT ;                                              FAN  124
L2:           DATA[J] ~ X[J]|X[J] ;                                     FAN  125
              GO TO NAXT ;                                              FAN  126
L3:           DATA[J] ~ 1 / X[J] ;                                      FAN  127
              GO TO NAXT ;                                              FAN  128
L4:           DATA[J] ~ SQRT(X[J]) ;                                    FAN  129
              GO TO NAXT ;                                              FAN  130
L5:           DATA[J] ~ EXP(X[J]) ;                                     FAN  131
              GO TO NAXT ;                                              FAN  132
L6:           DATA[J] ~ LN(X[J]) ;                                      FAN  133
              GO TO NAXT ;                                              FAN  134
L7:           DATA[J] ~ SIN(X[J]) ;                                     FAN  135
              GO TO NAXT ;                                              FAN  136
L8:           DATA[J] ~ COS(X[J]) ;                                     FAN  137
              GO TO NAXT ;                                              FAN  138
L9:           DATA[J] ~ X[J+1] ;                                        FAN  139
              GO TO NAXT ;                                              FAN  140
L10:          DATA[J] ~ X[J+2] ;                                        FAN  141
              GO TO NAXT ;                                              FAN  142
L11:          DATA[J] ~ X[J+3] ;                                        FAN  143
              GO TO NAXT ;                                              FAN  144
L12:          DATA[J] ~ X[J+4] ;                                        FAN  145
              GO TO NAXT ;                                              FAN  146
L13:          DATA[J] ~ X[J+5] ;                                        FAN  147
              GO TO NAXT ;                                              FAN  148
L14:          DATA[J] ~ X[J+6] ;                                        FAN  149
              GO TO NAXT ;                                              FAN  150
L15:          DATA[J] ~ X[J+7] ;                                        FAN  151
              GO TO NAXT ;                                              FAN  152
NAXT:    END                                                            FAN  153
         END ;                                                          FAN  154
              FOR L ~ 1 STEP 1 UNTIL NVAR DO                            FAN  155
         BEGIN                                                          FAN  156
              A[L,2] ~ A[L,2] + DATA[L] ;                               FAN  157
              A[L,3] ~ A[L,3] + DATA[L]|DATA[L]                         FAN  158
         END ;                                                          FAN  159
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  160
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            FAN  161
              SUMXX[J,K] ~ SUMXX[J,K] + DATA[J]|DATA[K]                 FAN  162
         END ;                                                          FAN  163
              JJ ~ NVAR - 1 ;                                           FAN  164
              FOR I ~ 1 STEP 1 UNTIL JJ DO                              FAN  165
         BEGIN                                                          FAN  166
              KK ~ I + 1 ;                                              FAN  167
              FOR J ~ KK STEP 1 UNTIL NVAR DO                           FAN  168
              SUMXX[J,I] ~ SUMXX[I,J]                                   FAN  169
         END ;                                                          FAN  170
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  171
              SUMX[I] ~ A[I,2]/NS ;                                     FAN  172
              WRITE(PRINT,FRM2) ;                                       FAN  173
              WRITE(PRINT,FRM3,AVEX) ;                                  FAN  174
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  175
         BEGIN                                                          FAN  176
              SUMX[J] ~ A[J,3] - ((A[J,2]|A[J,2])/NS) ;                 FAN  177
              A[J,1] ~ SQRT(SUMX[J]/NSM1)                               FAN  178
         END ;                                                          FAN  179
              WRITE(PRINT,FRM4) ;                                       FAN  180
              WRITE(PRINT,FRM3,SDEV) ;                                  FAN  181
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  182
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  183
              R[I,J] ~ SUMXX[I,J] - ((A[I,2]|A[J,2])/NS) ;              FAN  184
              FOR K ~ 1 STEP 1 UNTIL JJ DO                              FAN  185
         BEGIN                                                          FAN  186
              KK ~ K+1 ;                                                FAN  187
              FOR L ~ KK STEP 1 UNTIL NVAR DO                           FAN  188
              R[L,K] ~ R[K,L]                                           FAN  189
         END ;                                                          FAN  190
              LL ~ NVAR - 1 ;                                           FAN  191
              IF CAS < 3 THEN                                           FAN  192
              GO TO LL3 ;                                               FAN  193
              FOR II ~ 1 STEP 1 UNTIL NVAR DO                           FAN  194
         BEGIN                                                          FAN  195
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  196
              T[I] ~ I ;                                                FAN  197
              FOR I ~ II STEP 1 UNTIL LL DO                             FAN  198
              T[I] ~ T[I+1] ;                                           FAN  199
              T[NVAR] ~ II ;                                            FAN  200
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  201
         BEGIN                                                          FAN  202
              JJ ~ T[J] ;                                               FAN  203
              FOR I ~ 1 STEP 1 UNTIL J DO                               FAN  204
         BEGIN                                                          FAN  205
              KK ~ T[I] ;                                               FAN  206
              SUMXX[I,J] ~ R[KK,JJ]                                     FAN  207
         END                                                            FAN  208
         END ;                                                          FAN  209
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  210
              A[1,J] ~ SUMXX[1,J] ;                                     FAN  211
              FOR I ~ 1 STEP 1 UNTIL LL DO                              FAN  212
         BEGIN                                                          FAN  213
              FOR J ~ I STEP 1 UNTIL NVAR DO                            FAN  214
              SCA[I,J] ~ A[I,J]/A[I,I] ;                                FAN  215
              MM ~ I+1 ;                                                FAN  216
              FOR J ~ MM STEP 1 UNTIL NVAR DO                           FAN  217
         BEGIN                                                          FAN  218
              S ~ 0.0 ;                                                 FAN  219
              FOR M ~ 1 STEP 1 UNTIL I DO                               FAN  220
              S ~ S + SCA[M,MM]|A[M,J] ;                                FAN  221
              A[MM,J] ~ SUMXX[MM,J] - S                                 FAN  222
         END                                                            FAN  223
         END ;                                                          FAN  224
              DATA[II] ~ 0.0 ;                                          FAN  225
              FOR I ~ 1 STEP 1 UNTIL LL DO                              FAN  226
              DATA[II] ~ DATA[II] + A[I,NVAR]|SCA[I,NVAR] ;             FAN  227
              DATA[II] ~ DATA[II] / SUMXX[NVAR,NVAR]                    FAN  228
         END ;                                                          FAN  229
LL3:          FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  230
              SUMX[I] ~ SQRT(SUMX[I]) ;                                 FAN  231
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  232
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  233
              SCA[I,J] ~ R[I,J] / (SUMX[I]|SUMX[J]) ;                   FAN  234
              FOR I ~ 1 STEP 1 UNTIL LL DO                              FAN  235
         BEGIN                                                          FAN  236
              KK ~ I+1 ;                                                FAN  237
              FOR J ~ KK STEP 1 UNTIL NVAR DO                           FAN  238
              SCA[J,I] ~ SCA[I,J]                                       FAN  239
         END ;                                                          FAN  240
              IF CAS = 3 THEN                                           FAN  241
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  242
              SCA[I,I] ~ DATA[I] ;                                      FAN  243
LL33:         WRITE(PRINT,FRM5) ;                                       FAN  244
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  245
              WRITE(PRINT,FRM6,CORR) ;                                  FAN  246
COMMENT       CALCULATE EIGENVALUES AND EIGENVECTORS ;                  FAN  247
LL34:    BEGIN                                                          FAN  248
REAL ARRAY    IND[0:NVAR] ;                                             FAN  249
LABEL         LL4,LL5,LL6,LL7,LL8,LL9,LL10,LL11,LL12,LL13,LL14,LL15,    FAN  250
              LL16,LL17,LL18,LL19,LL20,LL21,LL22,LL36 ;                 FAN  251
              NORM ~ 0.0 ;                                              FAN  252
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  253
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  254
              NORM ~ NORM + SCA[I,J]*2 ;                                FAN  255
              NORM ~ SQRT(NORM) ;                                       FAN  256
              IF NVAR = 0 THEN                                          FAN  257
              GO TO LL4 ;                                               FAN  258
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  259
         BEGIN                                                          FAN  260
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  261
              R[I,J] ~ 0.0 ;                                            FAN  262
              R[I,I] ~ 1.0                                              FAN  263
         END ;                                                          FAN  264
LL4:          EX ~ 1 ;                                                  FAN  265
              LL ~ NVAR - 2 ;                                           FAN  266
              IF LL < 0 THEN                                            FAN  267
              GO TO LL23 ;                                              FAN  268
              IF LL = 0 THEN                                            FAN  269
              GO TO LL5 ;                                               FAN  270
              FOR I ~ 1 STEP 1 UNTIL LL DO                              FAN  271
         BEGIN                                                          FAN  272
              II ~ I + 2 ;                                              FAN  273
              FOR J ~ II STEP 1 UNTIL NVAR DO                           FAN  274
         BEGIN                                                          FAN  275
              T1 ~ SCA[I,I+1] ;                                         FAN  276
              T2 ~ SCA[I,J] ;                                           FAN  277
LL20:         IF T2 ! 0 THEN                                            FAN  278
         BEGIN                                                          FAN  279
LL18:         TE ~ SQRT(T1|T1 + T2|T2) ;                                FAN  280
              KOS ~ T1 / TE ;                                           FAN  281
              SYN ~ T2 / TE ;                                           FAN  282
              IF EX = 1 THEN                                            FAN  283
              GO TO LL22                                                FAN  284
              ELSE                                                      FAN  285
              GO TO LL19                                                FAN  286
         END ;                                                          FAN  287
              IF EX = 1 THEN                                            FAN  288
              GO TO LL36                                                FAN  289
              ELSE                                                      FAN  290
              GO TO LL18 ;                                              FAN  291
LL22:         FOR K ~ I STEP 1 UNTIL NVAR DO                            FAN  292
         BEGIN                                                          FAN  293
              T2 ~ KOS | SCA[K,I+1] + SYN | SCA[K,J] ;                  FAN  294
              SCA[K,J] ~ KOS | SCA[K,J] - SYN | SCA[K,I+1] ;            FAN  295
              SCA[K,I+1] ~ T2                                           FAN  296
         END ;                                                          FAN  297
              FOR K ~ I STEP 1 UNTIL NVAR DO                            FAN  298
         BEGIN                                                          FAN  299
              T2 ~ KOS | SCA[I+1,K] + SYN | SCA[J,K] ;                  FAN  300
              SCA[J,K] ~ KOS | SCA[J,K] - SYN | SCA[I+1,K] ;            FAN  301
              SCA[I+1,K] ~ T2                                           FAN  302
         END ;                                                          FAN  303
              IF NVAR ! 0 THEN                                          FAN  304
         BEGIN                                                          FAN  305
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            FAN  306
         BEGIN                                                          FAN  307
              T2 ~ KOS | R[K,I+1] + SYN | R[K,J] ;                      FAN  308
              R[K,J] ~ KOS | R[K,J] - SYN | R[K,I+1] ;                  FAN  309
              R[K,I+1] ~ T2                                             FAN  310
         END                                                            FAN  311
         END ;                                                          FAN  312
LL36:    END                                                            FAN  313
         END ;                                                          FAN  314
LL5:          FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  315
         BEGIN                                                          FAN  316
              DIAG[I] ~ SCA[I,I] ;                                      FAN  317
              SUMX[I] ~ NORM ;                                          FAN  318
              VAL[I] ~ -NORM                                            FAN  319
         END ;                                                          FAN  320
              FOR I ~ 2 STEP 1 UNTIL NVAR DO                            FAN  321
         BEGIN                                                          FAN  322
              SD[I-1] ~ SCA[I-1,I] ;                                    FAN  323
              X[I-1] ~ SD[I-1]*2                                        FAN  324
         END ;                                                          FAN  325
              TA ~ 0.0 ;                                                FAN  326
              I ~ 1 ;                                                   FAN  327
LL13:         M ~ 0 ;                                                   FAN  328
              T2 ~ 0.0 ;                                                FAN  329
              T1 ~ 1.0 ;                                                FAN  330
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  331
         BEGIN                                                          FAN  332
              P ~ DIAG[J] - TA ;                                        FAN  333
              IF T2 = 0 THEN                                            FAN  334
              GO TO LL6 ;                                               FAN  335
              IF T1 = 0 THEN                                            FAN  336
              GO TO LL7 ;                                               FAN  337
              TE ~ P|T1 - X[J-1]|T2 ;                                   FAN  338
              GO TO LL8 ;                                               FAN  339
LL6:          IF T1 < 0 THEN                                            FAN  340
         BEGIN                                                          FAN  341
              T1 ~ -1.0 ;                                               FAN  342
              TE ~ -P ;                                                 FAN  343
              GO TO LL8                                                 FAN  344
         END ;                                                          FAN  345
LL9:          T1 ~ 1.0 ;                                                FAN  346
              TE ~ P ;                                                  FAN  347
              GO TO LL8 ;                                               FAN  348
LL7:          IF X[J-1] = 0 THEN                                        FAN  349
              GO TO LL9 ;                                               FAN  350
              IF T2 } 0 THEN                                            FAN  351
         BEGIN                                                          FAN  352
              TE ~ -1.0 ;                                               FAN  353
              GO TO LL8                                                 FAN  354
         END ;                                                          FAN  355
              TE ~ 1.0 ;                                                FAN  356
LL8:          IF ((T1 < 0) AND (TE < 0)) OR ((T1 } 0) AND (TE } 0)) THENFAN  357
              M ~ M+1 ;                                                 FAN  358
              T2 ~ T1 ;                                                 FAN  359
              T1 ~ TE                                                   FAN  360
         END ;                                                          FAN  361
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            FAN  362
         BEGIN                                                          FAN  363
              IF K { M THEN                                             FAN  364
         BEGIN                                                          FAN  365
              IF TA > VAL[K] THEN                                       FAN  366
              VAL[K] ~ TA                                               FAN  367
         END                                                            FAN  368
              ELSE                                                      FAN  369
         BEGIN                                                          FAN  370
              IF TA < SUMX[K] THEN                                      FAN  371
              SUMX[K] ~ TA                                              FAN  372
         END                                                            FAN  373
         END ;                                                          FAN  374
LL12:         W ~ SUMX[I] - VAL[I] - 5.0@-8 ;                           FAN  375
              IF W { 0 THEN                                             FAN  376
              GO TO LL10 ;                                              FAN  377
              IF SUMX[I] = 0 THEN                                       FAN  378
              GO TO LL11 ;                                              FAN  379
              W ~ ABS(VAL[I] / SUMX[I] - 1.0) - 5.0@-8 ;                FAN  380
              IF W > 0 THEN                                             FAN  381
              GO TO LL11 ;                                              FAN  382
LL10:         I ~ I + 1 ;                                               FAN  383
              IF I > NVAR THEN                                          FAN  384
              GO TO LL14                                                FAN  385
              ELSE                                                      FAN  386
              GO TO LL12 ;                                              FAN  387
LL11:         TA ~ (VAL[I] + SUMX[I]) / 2.0 ;                           FAN  388
              GO TO LL13 ;                                              FAN  389
LL14:         EX ~ 2 ;                                                  FAN  390
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  391
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  392
              SCA[I,J] ~ 0.0 ;                                          FAN  393
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  394
         BEGIN                                                          FAN  395
              IF I = 1 THEN                                             FAN  396
              GO TO LL21 ;                                              FAN  397
              W ~ SUMX[I-1] - SUMX[I] - 5.0@-7 ;                        FAN  398
              IF W { 0 THEN                                             FAN  399
              GO TO LL16 ;                                              FAN  400
              IF SUMX[I-1] ! 0 THEN                                     FAN  401
         BEGIN                                                          FAN  402
              W ~ ABS(SUMX[I] / SUMX[I-1] - 1.0) - 5.0@-7 ;             FAN  403
              IF W { 0 THEN                                             FAN  404
              GO TO LL16                                                FAN  405
         END ;                                                          FAN  406
LL21:         KOS ~ 1.0 ;                                               FAN  407
              SYN ~ 0.0 ;                                               FAN  408
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  409
         BEGIN                                                          FAN  410
              IF J = 1 THEN                                             FAN  411
              GO TO LL15                                                FAN  412
              ELSE                                                      FAN  413
              GO TO LL20 ;                                              FAN  414
LL19:         X[J-1] ~ SYN ;                                            FAN  415
              DATA[J-1] ~ KOS ;                                         FAN  416
              VAL[J-1] ~ T1|KOS + T2|SYN ;                              FAN  417
LL15:         T1 ~ (DIAG[J]-SUMX[I])|KOS - TA|SYN ;                     FAN  418
              T2 ~ SD[J] ;                                              FAN  419
              TA ~ SD[J]|KOS                                            FAN  420
         END ;                                                          FAN  421
              VAL[NVAR] ~ T1 ;                                          FAN  422
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  423
              IND[J] ~ 0 ;                                              FAN  424
LL16:         TE ~ NORM ;                                               FAN  425
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  426
         BEGIN                                                          FAN  427
              IF IND[J] } 1 THEN                                        FAN  428
              GO TO LL17 ;                                              FAN  429
              IF ABS(TE) { ABS(VAL[J]) THEN                             FAN  430
              GO TO LL17 ;                                              FAN  431
              TE ~ VAL[J] ;                                             FAN  432
              LL ~ J ;                                                  FAN  433
LL17:    END ;                                                          FAN  434
              IND[LL] ~ 1 ;                                             FAN  435
              P ~ 1.0 ;                                                 FAN  436
              IF LL ! 1 THEN                                            FAN  437
         BEGIN                                                          FAN  438
              FOR K ~ 2 STEP 1 UNTIL LL DO                              FAN  439
         BEGIN                                                          FAN  440
              M ~ LL+1-K ;                                              FAN  441
              SCA[M+1,I] ~ DATA[M]|P ;                                  FAN  442
              P ~ -P|X[M]                                               FAN  443
         END                                                            FAN  444
         END ;                                                          FAN  445
              SCA[1,I] ~ P                                              FAN  446
         END ;                                                          FAN  447
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  448
         BEGIN                                                          FAN  449
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            FAN  450
              IND[K] ~ SCA[K,J] ;                                       FAN  451
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  452
         BEGIN                                                          FAN  453
              SCA[I,J] ~ 0.0 ;                                          FAN  454
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            FAN  455
              SCA[I,J] ~ R[I,K]|IND[K] + SCA[I,J]                       FAN  456
         END                                                            FAN  457
         END                                                            FAN  458
         END ;                                                          FAN  459
LL23:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  460
         BEGIN                                                          FAN  461
              IF SUMX[I] { RN THEN                                      FAN  462
         BEGIN                                                          FAN  463
              Z ~ I-1 ;                                                 FAN  464
              GO TO LL24                                                FAN  465
         END                                                            FAN  466
         END ;                                                          FAN  467
              Z ~ NVAR ;                                                FAN  468
LL24:         WRITE(PRINT,FRM7) ;                                       FAN  469
              WRITE(PRINT,FRM3,EVAL) ;                                  FAN  470
              IF NVAR > Z THEN                                          FAN  471
              WRITE(PRINT,FRM8) ;                                       FAN  472
COMMENT       CALCULATE CUMULATIVE PROPORTIONS OF TOTAL VARIANCE ;      FAN  473
              A[1,1] ~ SUMX[1] ;                                        FAN  474
              FOR I ~ 2 STEP 1 UNTIL Z DO                               FAN  475
              A[I,1] ~ A[I-1,1] + SUMX[I] ;                             FAN  476
              FOR I ~ 1 STEP 1 UNTIL Z DO                               FAN  477
              A[I,1] ~ A[I,1] / (NVAR | A[Z,1]) ;                       FAN  478
              WRITE(PRINT,FRM9) ;                                       FAN  479
              WRITE(PRINT,FRM3,CUMP) ;                                  FAN  480
              WRITE(PRINT,FRM10) ;                                      FAN  481
              FOR J ~ 1 STEP 1 UNTIL Z DO                               FAN  482
              WRITE(PRINT,FRM11,EVEC) ;                                 FAN  483
              FOR I ~ 1 STEP 1 UNTIL Z DO                               FAN  484
         BEGIN                                                          FAN  485
              SUMX[I] ~ SQRT(SUMX[I]) ;                                 FAN  486
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  487
              A[J,I] ~ SUMX[I]|SCA[J,I]                                 FAN  488
         END ;                                                          FAN  489
              WRITE(PRINT,FRM12) ;                                      FAN  490
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  491
              WRITE(PRINT,FRM13,FACT) ;                                 FAN  492
              IF NVAR > Z THEN                                          FAN  493
         BEGIN                                                          FAN  494
              FOR I ~ 1 STEP 1 UNTIL Z DO                               FAN  495
              FOR J ~ 1 STEP 1 UNTIL Z DO                               FAN  496
         BEGIN                                                          FAN  497
              R[I,J] ~ 0.0 ;                                            FAN  498
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            FAN  499
              R[I,J] ~ R[I,J] + A[K,I]|A[K,J]                           FAN  500
         END ;                                                          FAN  501
              WRITE(PRINT,FRM14)                                        FAN  502
         END                                                            FAN  503
              ELSE                                                      FAN  504
         BEGIN                                                          FAN  505
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  506
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            FAN  507
         BEGIN                                                          FAN  508
              R[I,J] ~ 0.0 ;                                            FAN  509
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            FAN  510
              R[I,J] ~ R[I,J] + A[I,K]|A[J,K]                           FAN  511
         END ;                                                          FAN  512
              WRITE(PRINT,FRM15)                                        FAN  513
         END ;                                                          FAN  514
              FOR I ~ 1 STEP 1 UNTIL Z DO                               FAN  515
              WRITE(PRINT,FRM6,CHEK) ;                                  FAN  516
              WRITE(PRINT,FRM16,LIM) ;                                  FAN  517
              IF LIM = 0 THEN                                           FAN  518
              GO TO LL26 ;                                              FAN  519
              FOR J ~ 1 STEP 1 UNTIL Z DO                               FAN  520
         BEGIN                                                          FAN  521
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  522
         BEGIN                                                          FAN  523
              W ~ ABS(A[I,J]) ;                                         FAN  524
              IF W } LIM THEN                                           FAN  525
              GO TO LL25                                                FAN  526
         END ;                                                          FAN  527
              NRO ~ J-1 ;                                               FAN  528
              GO TO LL27 ;                                              FAN  529
LL25:    END ;                                                          FAN  530
              NRO ~ Z ;                                                 FAN  531
              GO TO LL27 ;                                              FAN  532
LL26:         IF (NRO=0) OR (NRO>Z) THEN                                FAN  533
              NRO ~ Z ;                                                 FAN  534
LL27:         IF NRO { 1 THEN                                           FAN  535
         BEGIN                                                          FAN  536
              WRITE(PRINT,FRM17) ;                                      FAN  537
              GO TO START                                               FAN  538
         END ;                                                          FAN  539
COMMENT       ROTATE FACTOR MATRIX ;                                    FAN  540
LL35:    BEGIN                                                          FAN  541
REAL          TN4P,ST,CT,S2T,C2T,S4T,C4T,TN4T,CN4T ;                    FAN  542
LABEL         LL28,LL29,LL30,LL31,LL32 ;                                FAN  543
              T1 ~ 0.00116 ;                                            FAN  544
              JJ ~ 3 ;                                                  FAN  545
              P ~ 1.0 ;                                                 FAN  546
              KK ~ 0 ;                                                  FAN  547
              MM ~ (NRO|(NRO-1))/2 ;                                    FAN  548
              DATA[1] ~ 0.0 ;                                           FAN  549
              LL ~ NRO - 1 ;                                            FAN  550
              II ~ 1 ;                                                  FAN  551
              EX ~ 0 ;                                                  FAN  552
              T2 ~ NVAR ;                                               FAN  553
              TA ~ T2*2 ;                                               FAN  554
              W ~ 1.0 / SQRT(2.0) ;                                     FAN  555
              NS ~ 0.0001 ;                                             FAN  556
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  557
              SUMX[I] ~ 0.0 ;                                           FAN  558
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  559
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             FAN  560
              SUMX[I] ~ SUMX[I] + A[I,J]|A[I,J] ;                       FAN  561
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  562
         BEGIN                                                          FAN  563
              SUMX[I] ~ SQRT(SUMX[I]) ;                                 FAN  564
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             FAN  565
              A[I,J] ~ A[I,J] / SUMX[I]                                 FAN  566
         END ;                                                          FAN  567
LL29:         II ~ II + 1 ;                                             FAN  568
              DATA[II] ~ 0.0 ;                                          FAN  569
              Z ~ II - 1 ;                                              FAN  570
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             FAN  571
         BEGIN                                                          FAN  572
              RN ~ 0.0 ;                                                FAN  573
              BE ~ 0.0 ;                                                FAN  574
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  575
         BEGIN                                                          FAN  576
              S ~ A[I,J]|A[I,J] ;                                       FAN  577
              RN ~ RN + S ;                                             FAN  578
              BE ~ BE + S|S                                             FAN  579
         END ;                                                          FAN  580
              DATA[II] ~ DATA[II] + (T2|BE - RN|RN)/TA                  FAN  581
         END ;                                                          FAN  582
              IF II } 50 THEN                                           FAN  583
              GO TO LL30 ;                                              FAN  584
              MARY ~ ABS(DATA[II] - DATA[Z]) - 1.0@-7 ;                 FAN  585
              IF MARY { 0 THEN                                          FAN  586
         BEGIN                                                          FAN  587
              EX ~ EX + 1 ;                                             FAN  588
              IF EX > JJ THEN                                           FAN  589
              GO TO LL30                                                FAN  590
         END ;                                                          FAN  591
              FOR J ~ 1 STEP 1 UNTIL LL DO                              FAN  592
         BEGIN                                                          FAN  593
              L ~ J + 1 ;                                               FAN  594
              FOR K ~ L STEP 1 UNTIL NRO DO                             FAN  595
         BEGIN                                                          FAN  596
              IF KK } MM THEN                                           FAN  597
              GO TO LL31 ;                                              FAN  598
              IF ABS(P) > NS THEN                                       FAN  599
              KK ~ 0 ;                                                  FAN  600
              RN ~ 0.0 ;                                                FAN  601
              BE ~ 0.0 ;                                                FAN  602
              S ~ 0.0 ;                                                 FAN  603
              NORM ~ 0.0 ;                                              FAN  604
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  605
         BEGIN                                                          FAN  606
              MARY ~ (A[I,J] + A[I,K])|(A[I,J] - A[I,K]) ;              FAN  607
              TE ~ A[I,J]|A[I,K] ;                                      FAN  608
              TE ~ TE + TE ;                                            FAN  609
              S ~ S + (MARY + TE)|(MARY - TE) ;                         FAN  610
              NORM ~ NORM + 2.0|MARY|TE ;                               FAN  611
              RN ~ RN + MARY ;                                          FAN  612
              BE ~ BE + TE                                              FAN  613
         END ;                                                          FAN  614
              TE ~ NORM - 2.0|RN|BE/T2 ;                                FAN  615
              B ~ S - (RN|RN - BE|BE)/T2 ;                              FAN  616
              P ~ 0.25|ARCTAN(TE/B) ;                                   FAN  617
              IF ABS(P) { NS THEN                                       FAN  618
         BEGIN                                                          FAN  619
              KK ~ KK + 1 ;                                             FAN  620
              GO TO LL31                                                FAN  621
         END ;                                                          FAN  622
              TN4P ~ TE/B ;                                             FAN  623
              IF TE = B THEN                                            FAN  624
         BEGIN                                                          FAN  625
              IF TE + B < T1 THEN                                       FAN  626
              GO TO LL31 ;                                              FAN  627
              C4T ~ W ;                                                 FAN  628
              S4T ~ W ;                                                 FAN  629
              GO TO LL28                                                FAN  630
         END                                                            FAN  631
              ELSE IF TE < B THEN                                       FAN  632
         BEGIN                                                          FAN  633
              TN4T ~ ABS(TE)/ABS(B) ;                                   FAN  634
              IF TN4T } T1 THEN                                         FAN  635
         BEGIN                                                          FAN  636
              C4T ~ 1.0 / SQRT(1.0 + TN4T*2) ;                          FAN  637
              S4T ~ TN4T|C4T ;                                          FAN  638
              GO TO LL28                                                FAN  639
         END ;                                                          FAN  640
              IF B } 0 THEN                                             FAN  641
              GO TO LL31 ;                                              FAN  642
              SYN ~ W ;                                                 FAN  643
              KOS ~ W ;                                                 FAN  644
              GO TO LL32                                                FAN  645
         END                                                            FAN  646
              ELSE                                                      FAN  647
         BEGIN                                                          FAN  648
              CN4T ~ ABS(TE/B) ;                                        FAN  649
              IF CN4T } T1 THEN                                         FAN  650
         BEGIN                                                          FAN  651
              S4T ~ 1.0 / SQRT(1.0 + CN4T*2) ;                          FAN  652
              C4T ~ CN4T|S4T ;                                          FAN  653
              GO TO LL28                                                FAN  654
         END ;                                                          FAN  655
              C4T ~ 0.0 ;                                               FAN  656
              S4T ~ 1.0                                                 FAN  657
         END ;                                                          FAN  658
LL28:         C2T ~ SQRT((1.0 + C4T)/2.0) ;                             FAN  659
              S2T ~ S4T/(2.0|C2T) ;                                     FAN  660
              CT ~ SQRT((1.0 + C2T)/2.0) ;                              FAN  661
              ST ~ S2T/(2.0|CT) ;                                       FAN  662
              IF B > 0 THEN                                             FAN  663
         BEGIN                                                          FAN  664
              KOS ~ CT ;                                                FAN  665
              SYN ~ ST                                                  FAN  666
         END                                                            FAN  667
              ELSE                                                      FAN  668
         BEGIN                                                          FAN  669
              KOS ~ W|CT + W|ST ;                                       FAN  670
              SYN ~ ABS(W|CT - W|ST)                                    FAN  671
         END ;                                                          FAN  672
              IF TE { 0 THEN                                            FAN  673
              SYN ~ -SYN ;                                              FAN  674
LL32:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  675
         BEGIN                                                          FAN  676
              RN ~ A[I,J]|KOS + A[I,K]|SYN ;                            FAN  677
              BE ~ -A[I,J]|SYN + A[I,K]|KOS ;                           FAN  678
              A[I,J] ~ RN ;                                             FAN  679
              A[I,K] ~ BE                                               FAN  680
         END                                                            FAN  681
         END ;                                                          FAN  682
LL31:    END ;                                                          FAN  683
              GO TO LL29 ;                                              FAN  684
LL30:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  685
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             FAN  686
              A[I,J] ~ A[I,J]|SUMX[I] ;                                 FAN  687
              EX ~ II - 2 ;                                             FAN  688
              WRITE(PRINT,FRM18,NRO,EX) ;                               FAN  689
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  690
              WRITE(PRINT,FRM13,ROTM) ;                                 FAN  691
              WRITE(PRINT,FRM19) ;                                      FAN  692
              FOR I ~ 2 STEP 1 UNTIL II DO                              FAN  693
         BEGIN                                                          FAN  694
              EX ~ I - 2 ;                                              FAN  695
              WRITE(PRINT,FRM20,EX,DATA[I])                             FAN  696
         END ;                                                          FAN  697
              WRITE(PRINT,FRM21) ;                                      FAN  698
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            FAN  699
         BEGIN                                                          FAN  700
              SUMX[I] ~ SUMX[I]|SUMX[I] ;                               FAN  701
              BE ~ 0.0 ;                                                FAN  702
              FOR J ~ 1 STEP 1 UNTIL NRO DO                             FAN  703
              BE ~ BE + A[I,J]|A[I,J] ;                                 FAN  704
              RN ~ BE - SUMX[I] ;                                       FAN  705
              WRITE(PRINT,FRM22,I,SUMX[I],BE,RN)                        FAN  706
         END                                                            FAN  707
         END                                                            FAN  708
         END                                                            FAN  709
         END ;                                                          FAN  710
              GO TO START ;                                             FAN  711
NOMO:              END.                                                      712
 LABEL  000000000CARDR  0010000001
PROF. SERV. DEPT. PROB.    173  6       4        FALSE
-00001-00001-00001-00001-00001-00001
   7.4   7.2   7.5   4.9   4.2   2.8
   7.2   7.2   7.8   4.2   3.6   2.4
   7.5   7.8   8.9   3.5   3.0   2.0
   4.9   4.2   3.5   4.9   4.2   2.8
   4.2   3.6   3.0   4.2   3.6   2.4
   2.8   2.4   2.0   2.8   2.4   1.6
