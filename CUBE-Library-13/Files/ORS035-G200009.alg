 LABEL  0000000000XXXXXX0010000001
         BEGIN                                                          SMR    1
COMMENT       STEPWISE MULTIPLE REGRESSION                              SMR    2
              CHARLES L. CLARK                                          SMR    3
              PROFESSIONAL SERVICES DIVISIONAL GROUP                    SMR    4
              BURROUGHS CORPORATION                                     SMR    5
              PROGRAM CONTAINS 405 CARDS                                SMR    6
              FIRST RELEASE DATE AUGUST 25, 1964  ;                     SMR    7
INTEGER       I,NOVA,NSAM,NAD ;                                         SMR    8
INTEGER ARRAY ID[0:4] ;                                                 SMR    9
BOOLEAN       WT,STAP,SSQ,MEAN,RESD,COEF,PRED,CNST,TR  ;                SMR   10
REAL          TOL,FIN,FOUT,CONS ;                                       SMR   11
LABEL         START,NOMO,LL1 ;                                          SMR   12
FORMAT IN     FORM1 (5A6,3F10.5,2I5,2L5/7L5,I2),                        SMR   13
              FORM2 (12I6),                                             SMR   14
              FORM3 (26I3),                                             SMR   15
              FORM4 (12F6.1) ;                                          SMR   16
FORMAT OUT    TITLE(X41,"STEPWISE MULTIPLE REGRESSION ANALYSIS"///),    SMR   17
              FRM2 (X45,5A6//X1,"NO. OF DATA =",I5/                     SMR   18
                    X1,"NO. OF VARIABLES =",I5/                         SMR   19
                    X1,"WEIGHTED DEGREES OF FREEDOM =",F12.2/           SMR   20
                    X1,"F LEVEL TO ENTER VARIABLE =",F10.3/             SMR   21
                    X1,"F LEVEL TO REMOVE VARIABLE =",F9.3//),          SMR   22
              FRM3 (//X1,"SUM OF VARIABLES"//                           SMR   23
                    (X6,"SUM X(",I2,") =",F12.4)),                      SMR   24
              FRM4 (X6,"SUM   Y   =",F12.4///X1,"SUM OF SQUARES AND ",  SMR   25
                    "CROSS PRODUCTS"//),                                SMR   26
              FRM5 (X6,"X(",I2,") VS X(",I2,") =",F15.6),               SMR   27
              FRM6 (X6,"X(",I2,") VS   Y   =",F15.6),                   SMR   28
              FRM7 (X6,"  Y   VS   Y   =",F15.6),                       SMR   29
              FRM8 (//X1,"MEAN VALUE OF VARIABLES"//                    SMR   30
                    (X5,"MEAN X(",I2,") =",F12.4)),                     SMR   31
              FRM9 (X5,"MEAN   Y   =",F12.4//),                         SMR   32
              FRM10(X1,"RESIDUAL SUMS OF SQUARES AND CROSS PRODUCTS"/), SMR   33
              FRM11(X1,"VARIABLE",I4," IS CONSTANT"/),                  SMR   34
              FRM12(//X1,"CORRELATION COEFFICIENTS"/),                  SMR   35
              FRM13(//X1,"SQUARE NON-POSITIVE,END STEP",I4/),           SMR   36
              FRM14(//X1,"NO MORE DEGREES OF FREEDOM, STEP ",I4/),      SMR   37
              FRM15(//X1,"SQUARE OF X",I4," IS NEGATIVE, END OF STEP ", SMR   38
                    I4/),                                               SMR   39
              FRM16(//X1,"STANDARD ERROR 6F Y = ",F12.6/),              SMR   40
              FRM17(//"STEP NO.",I4/X1,"VARIABLE REMOVED",I5),          SMR   41
              FRM18(//"STEP NO.",I4/X1,"VARIABLE ENTERING",I5),         SMR   42
              FRM19(X1,"F LEVEL ",F12.4/X1,"STANDARD ERROR OF ESTIMATE",SMR   43
                    X1,F12.4/X1,"CONSTANT ",F13.5/X1,"VARIABLE",X3,     SMR   44
                    "COEFFICIENT",X3,"STD ERROR OF COEF"//              SMR   45
                    ((X3,"X",I3,X5,F10.5,X5,F10.5)/)),                  SMR   46
              FRM20(//"STEPS OF REGRESSIO5 COMPLETED",I5/),             SMR   47
              FRM21(//"DIAGONAL ELEMENTS OF INVERSE OF CORRELATION ",   SMR   48
                    "MATRIX"//X4,"VAR. NO."X5,"VALUE"//(I7,F16.6)/),    SMR   49
              FRM22(///X19,"PREDICTED VS ACTUAL VALUES"/X10,"RUN NO.",  SMR   50
                    X4,"ACTUAL",X7,"PREDICTED",X5,"DEVIATION"/),        SMR   51
              FRM23(X8,I6,X2,F12.5,X3,F12.5,X2,F12.5) ;                 SMR   52
FILE IN       CARDR(1,10) ;                                             SMR   53
FILE OUT      PRINT 1 (1,15) ;                                          SMR   54
START:   BEGIN                                                          SMR   55
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],TOL,FIN,FOUT,NOVA, SMR   56
                   NSAM,WT,STAP,SSQ,MEAN,RESD,COEF,PRED,CNST,TR,NAD) ;  SMR   57
              READ (CARDR,FORM1,HEAD) [NOMO] ;                          SMR   58
              WRITE(PRINT[PAGE]) ;                                      SMR   59
              WRITE(PRINT,TITLE)                                        SMR   60
         END ;                                                          SMR   61
LL1:     BEGIN                                                          SMR   62
INTEGER       NIN,K,NENT,NMIN,NMAX,NVP1,MARY,NEWV,NVA1 ;                SMR   63
REAL          VAR,FLEV ;                                                SMR   64
LABEL         LL2 ;                                                     SMR   65
              K ~ 0 ;                                                   SMR   66
              NENT ~ 0 ;                                                SMR   67
              NMIN ~ 0 ;                                                SMR   68
              NMAX ~ 0 ;                                                SMR   69
              NVA1 ~ NOVA + 1 ;                                         SMR   70
              NVP1 ~ NOVA + 1 + NAD ;                                   SMR   71
              MARY ~ NOVA + 1 + ABS(NAD) ;                              SMR   72
              NEWV ~ NOVA + NAD ;                                       SMR   73
LL2:     BEGIN                                                          SMR   74
INTEGER       N,J,L,VMI,VPL,NSTEP,IP1,VM2,STM1 ;                        SMR   75
BOOLEAN       SWICH ;                                                   SMR   76
INTEGER ARRAY T[0:99],SC[0:NVA1],INDX[0:NVP1] ;                         SMR   77
REAL          W,DF,SIGY,VMIN,VMAX,YPRE,DEV ;                            SMR   78
REAL ARRAY    VEC[0:NVP1,0:NVP1],DATA[0:MARY],X[0:MARY],AVE[0:NVP1],    SMR   79
              SIG[0:NVP1],COEN[0:NVP1],SIGC[0:NVP1] ;                   SMR   80
LABEL         LL6,LL8,LL9,LL10,LL11,LL12,LL13,LL14,LL15,LL16,NAXT,      SMR   81
              L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15 ;      SMR   82
LIST          SCAL(FOR I ~ 1 STEP 1 UNTIL NVA1 DO SC[I]),               SMR   83
              OUTT(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NSAM,NOVA,         SMR   84
                   VEC[VPL,VPL],FIN,FOUT),                              SMR   85
              TRAN(FOR J ~ 1 STEP 1 UNTIL NEWV DO T[J]),                SMR   86
              DATR(FOR L ~ 1 STEP 1 UNTIL NOVA DO DATA[L]),             SMR   87
              DATE(FOR L ~ 1 STEP 1 UNTIL NOVA DO DATA[L],W),           SMR   88
              SUMX(FOR I ~ 1 STEP 1 UNTIL VMI DO [I,VEC[I,VPL]]),       SMR   89
              SSQX(FOR J ~ I STEP 1 UNTIL VMI DO [I,J,VEC[I,J]]),       SMR   90
              SSYX(FOR I ~ 1 STEP 1 UNTIL VMI DO [I,VEC[I,NOVA]]),      SMR   91
              AVEX(FOR I ~ 1 STEP 1 UNTIL VMI DO [I,AVE[I]]),           SMR   92
              CORR(FOR J ~ IP1 STEP 1 UNTIL VMI DO [I,J,VEC[I,J]]),     SMR   93
              KOEF(FLEV,SIGY,CONS,FOR J ~ 1 STEP 1 UNTIL NIN DO         SMR   94
                   [INDX[J],COEN[J],SIGC[J]]),                          SMR   95
              DIAG(FOR L ~ 1 STEP 1 UNTIL VMI DO [L,VEC[L,L]]) ;        SMR   96
SWITCH        U ~ L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15 ;  SMR   97
FILE          TAPEB (2,NEWV+1) ;                                        SMR   98
              FOR I ~ 1 STEP 1 UNTIL NVP1 DO                            SMR   99
              FOR J ~ 1 STEP 1 UNTIL NVP1 DO                            SMR  100
              VEC[I,J] ~ 0.0 ;                                          SMR  101
              READ (CARDR,FORM2,SCAL) ;                                 SMR  102
              IF TR THEN                                                SMR  103
              READ (CARDR,FORM3,TRAN) ;                                 SMR  104
              FOR N ~ 1 STEP 1 UNTIL NSAM DO                            SMR  105
         BEGIN                                                          SMR  106
              IF WT THEN                                                SMR  107
              READ (CARDR,FORM4,DATE)                                   SMR  108
              ELSE                                                      SMR  109
              READ (CARDR,FORM4,DATR) ;                                 SMR  110
              FOR J ~ 1 STEP 1 UNTIL NOVA DO                            SMR  111
         BEGIN                                                          SMR  112
              X[J] ~ DATA[J]|10.0*SC[J] ;                               SMR  113
              DATA[J] ~ X[J]                                            SMR  114
         END ;                                                          SMR  115
COMMENT       TRANSFORMATIONS ;                                         SMR  116
              IF TR THEN                                                SMR  117
         BEGIN                                                          SMR  118
              FOR J ~ NEWV STEP -1 UNTIL 1 DO                           SMR  119
         BEGIN                                                          SMR  120
              GO TO U[T[J]] ;                                           SMR  121
L1:           DATA[J] ~ X[J] ;                                          SMR  122
              GO TO NAXT ;                                              SMR  123
L2:           DATA[J] ~ X[J]|X[J] ;                                     SMR  124
              GO TO NAXT ;                                              SMR  125
L3:           DATA[J] ~ 1 / X[J] ;                                      SMR  126
              GO TO NAXT ;                                              SMR  127
L4:           DATA[J] ~ SQRT(X[J]) ;                                    SMR  128
              GO TO NAXT ;                                              SMR  129
L5:           DATA[J] ~ EXP(X[J]) ;                                     SMR  130
              GO TO NAXT ;                                              SMR  131
L6:           DATA[J] ~ LN(X[J]) ;                                      SMR  132
              GO TO NAXT ;                                              SMR  133
L7:           DATA[J] ~ SIN(X[J]) ;                                     SMR  134
              GO TO NAXT ;                                              SMR  135
L8:           DATA[J] ~ COS(X[J]) ;                                     SMR  136
              GO TO NAXT ;                                              SMR  137
L9:           DATA[J] ~ X[J+1] ;                                        SMR  138
              GO TO NAXT ;                                              SMR  139
L10:          DATA[J] ~ X[J+2] ;                                        SMR  140
              GO TO NAXT ;                                              SMR  141
L11:          DATA[J] ~ X[J+3] ;                                        SMR  142
              GO TO NAXT ;                                              SMR  143
L12:          DATA[J] ~ X[J+4] ;                                        SMR  144
              GO TO NAXT ;                                              SMR  145
L13:          DATA[J] ~ X[J+5] ;                                        SMR  146
              GO TO NAXT ;                                              SMR  147
L14:          DATA[J] ~ X[J+6] ;                                        SMR  148
              GO TO NAXT ;                                              SMR  149
L15:          DATA[J] ~ X[J+7] ;                                        SMR  150
              GO TO NAXT ;                                              SMR  151
NAXT:    END                                                            SMR  152
         END ;                                                          SMR  153
              IF WT THEN                                                SMR  154
              W ~ W|10.0*SC[NVA1] ;                                     SMR  155
              WRITE(TAPEB,NEWV+1,DATA[*]) ;                             SMR  156
              FOR I ~ 1 STEP 1 UNTIL NEWV DO                            SMR  157
         BEGIN                                                          SMR  158
              L ~ NEWV + 1 ;                                            SMR  159
              IF WT THEN                                                SMR  160
              DF ~ DATA[I]|W                                            SMR  161
              ELSE                                                      SMR  162
              DF ~ DATA[I] ;                                            SMR  163
              VEC[I,L] ~ VEC[I,L] + DF ;                                SMR  164
              FOR J ~ I STEP 1 UNTIL NEWV DO                            SMR  165
              VEC[I,J] ~ VEC[I,J] + DF|DATA[J]                          SMR  166
         END ;                                                          SMR  167
              IF NOT WT THEN                                            SMR  168
              W ~ 1.0 ;                                                 SMR  169
              VEC[NVP1,NVP1] ~ VEC[NVP1,NVP1] + W                       SMR  170
         END ;                                                          SMR  171
              NOVA ~ NEWV ;                                             SMR  172
              VMI ~ NOVA - 1 ;                                          SMR  173
              VPL ~ NOVA + 1 ;                                          SMR  174
              WRITE(PRINT,FRM2,OUTT) ;                                  SMR  175
              IF SSQ THEN                                               SMR  176
         BEGIN                                                          SMR  177
              WRITE(PRINT,FRM3,SUMX) ;                                  SMR  178
              WRITE(PRINT,FRM4,VEC[NOVA,VPL]) ;                         SMR  179
              FOR I ~ 1 STEP 1 UNTIL VMI DO                             SMR  180
              WRITE(PRINT,FRM5,SSQX) ;                                  SMR  181
              WRITE(PRINT,FRM6,SSYX) ;                                  SMR  182
              WRITE(PRINT,FRM7,VEC[NOVA,NOVA])                          SMR  183
         END ;                                                          SMR  184
              IF CNST THEN                                              SMR  185
              FOR I ~ 1 STEP 1 UNTIL NOVA DO                            SMR  186
              FOR J ~ 1 STEP 1 UNTIL NOVA DO                            SMR  187
              VEC[I,J] ~ VEC[I,J]-(VEC[I,VPL]|VEC[J,VPL]/VEC[VPL,VPL]) ;SMR  188
              FOR I ~ 1 STEP 1 UNTIL NOVA DO                            SMR  189
              AVE[I] ~ VEC[I,VPL] / VEC[VPL,VPL] ;                      SMR  190
              IF MEAN THEN                                              SMR  191
         BEGIN                                                          SMR  192
              WRITE(PRINT,FRM8,AVEX) ;                                  SMR  193
              WRITE(PRINT,FRM9,AVE[NOVA])                               SMR  194
         END ;                                                          SMR  195
              IF RESD THEN                                              SMR  196
         BEGIN                                                          SMR  197
              WRITE(PRINT,FRM10) ;                                      SMR  198
              FOR I ~ 1 STEP 1 UNTIL VMI DO                             SMR  199
              WRITE(PRINT,FRM5,SSQX) ;                                  SMR  200
              WRITE(PRINT,FRM6,SSYX) ;                                  SMR  201
              WRITE(PRINT,FRM7,VEC[NOVA,NOVA])                          SMR  202
         END ;                                                          SMR  203
              NSTEP ~ -1 ;                                              SMR  204
              SWICH ~ FALSE ;                                           SMR  205
              DF ~ VEC[VPL,VPL] - 1.0 ;                                 SMR  206
              FOR I ~ 1 STEP 1 UNTIL NOVA DO                            SMR  207
         BEGIN                                                          SMR  208
              IF VEC[I,I] = 0 THEN                                      SMR  209
         BEGIN                                                          SMR  210
              WRITE(PRINT,FRM11,I) ;                                    SMR  211
              SIG[I] ~ 1.0                                              SMR  212
         END                                                            SMR  213
              ELSE                                                      SMR  214
              SIG[I] ~ SQRT(VEC[I,I]) ;                                 SMR  215
              VEC[I,I] ~ 1.0                                            SMR  216
         END ;                                                          SMR  217
              FOR I ~ 1 STEP 1 UNTIL VMI DO                             SMR  218
         BEGIN                                                          SMR  219
              IP1 ~ I + 1 ;                                             SMR  220
              FOR J ~ IP1 STEP 1 UNTIL NOVA DO                          SMR  221
         BEGIN                                                          SMR  222
              VEC[I,J] ~ VEC[I,J] / (SIG[I]|SIG[J]) ;                   SMR  223
              VEC[J,I] ~ VEC[I,J]                                       SMR  224
         END                                                            SMR  225
         END ;                                                          SMR  226
              IF COEF THEN                                              SMR  227
         BEGIN                                                          SMR  228
              WRITE(PRINT,FRM12) ;                                      SMR  229
              VM2 ~ VMI - 1 ;                                           SMR  230
              FOR I ~ 1 STEP 1 UNTIL VM2 DO                             SMR  231
         BEGIN                                                          SMR  232
              IP1 ~ I + 1 ;                                             SMR  233
              WRITE(PRINT,FRM5,CORR)                                    SMR  234
         END ;                                                          SMR  235
              WRITE(PRINT,FRM6,SSYX)                                    SMR  236
         END ;                                                          SMR  237
COMMENT       COMPUTE NEXT REGRESSION EQUATION ;                        SMR  238
LL12:         NSTEP ~ NSTEP + 1 ;                                       SMR  239
              IF VEC[NOVA,NOVA] { 0 THEN                                SMR  240
         BEGIN                                                          SMR  241
              STM1 ~ NSTEP - 1 ;                                        SMR  242
              WRITE(PRINT,FRM13,STM1) ;                                 SMR  243
              GO TO LL14                                                SMR  244
         END ;                                                          SMR  245
              SIGY ~ SIG[NOVA] | SQRT(VEC[NOVA,NOVA] / DF) ;            SMR  246
              DF ~ DF - 1.0 ;                                           SMR  247
              IF DF { 0 THEN                                            SMR  248
         BEGIN                                                          SMR  249
              WRITE(PRINT,FRM14,NSTEP) ;                                SMR  250
              GO TO LL14                                                SMR  251
         END ;                                                          SMR  252
              VMIN ~ 0.0 ;                                              SMR  253
              VMAX ~ 0.0 ;                                              SMR  254
              NIN ~ 0 ;                                                 SMR  255
              FOR I ~ 1 STEP 1 UNTIL VMI DO                             SMR  256
         BEGIN                                                          SMR  257
              IF VEC[I,I] < 0 THEN                                      SMR  258
         BEGIN                                                          SMR  259
              WRITE(PRINT,FRM15,NSTEP) ;                                SMR  260
              GO TO LL14                                                SMR  261
         END                                                            SMR  262
              ELSE IF VEC[I,I] = 0 THEN                                 SMR  263
              GO TO LL8                                                 SMR  264
              ELSE IF VEC[I,I] - TOL < 0 THEN                           SMR  265
              GO TO LL8                                                 SMR  266
              ELSE                                                      SMR  267
              VAR ~ VEC[I,NOVA] | VEC[NOVA,I] / VEC[I,I] ;              SMR  268
              IF VAR < 0 THEN                                           SMR  269
         BEGIN                                                          SMR  270
              NIN ~ NIN + 1 ;                                           SMR  271
              INDX[NIN] ~ I ;                                           SMR  272
              COEN[NIN] ~ VEC[I,NOVA] | SIG[NOVA] / SIG[I] ;            SMR  273
              SIGC[NIN] ~ (SIGY / SIG[I]) | SQRT(VEC[I,I]) ;            SMR  274
COMMENT       CHOOSE NEXT ENTERING VARIABLE AND LEAVING VARIABLE ;      SMR  275
              IF VMIN > 0 THEN                                          SMR  276
         BEGIN                                                          SMR  277
              REWIND(TAPEB) ;                                           SMR  278
              GO TO START                                               SMR  279
         END                                                            SMR  280
              ELSE IF VMIN = 0 THEN                                     SMR  281
         BEGIN                                                          SMR  282
LL6:          VMIN ~ VAR ;                                              SMR  283
              NMIN ~ I ;                                                SMR  284
              GO TO LL8                                                 SMR  285
         END                                                            SMR  286
              ELSE IF VAR - VMIN > 0 THEN                               SMR  287
              GO TO LL6                                                 SMR  288
              ELSE                                                      SMR  289
              GO TO LL8                                                 SMR  290
         END                                                            SMR  291
              ELSE IF VAR = 0 THEN                                      SMR  292
              GO TO LL8                                                 SMR  293
              ELSE                                                      SMR  294
         BEGIN                                                          SMR  295
              IF VAR - VMAX { 0 THEN                                    SMR  296
              GO TO LL8                                                 SMR  297
              ELSE                                                      SMR  298
         BEGIN                                                          SMR  299
              VMAX ~ VAR ;                                              SMR  300
              NMAX ~ I                                                  SMR  301
         END                                                            SMR  302
         END ;                                                          SMR  303
LL8:     END ;                                                          SMR  304
              IF NIN < 0 THEN                                           SMR  305
         BEGIN                                                          SMR  306
              REWIND(TAPEB) ;                                           SMR  307
              GO TO START                                               SMR  308
         END                                                            SMR  309
              ELSE IF NIN = 0 THEN                                      SMR  310
         BEGIN                                                          SMR  311
              WRITE(PRINT,FRM16,SIGY) ;                                 SMR  312
              GO TO LL10                                                SMR  313
         END                                                            SMR  314
              ELSE IF CNST THEN                                         SMR  315
              CONS ~ AVE[NOVA]                                          SMR  316
              ELSE                                                      SMR  317
         BEGIN                                                          SMR  318
              CONS ~ 0.0 ;                                              SMR  319
              GO TO LL9                                                 SMR  320
         END ;                                                          SMR  321
              FOR I ~ 1 STEP 1 UNTIL NIN DO                             SMR  322
         BEGIN                                                          SMR  323
              J ~ INDX[I] ;                                             SMR  324
              CONS ~ CONS - (COEN[I] | AVE[J])                          SMR  325
         END ;                                                          SMR  326
LL9:          IF STAP THEN                                              SMR  327
         BEGIN                                                          SMR  328
              IF NENT { 0 THEN                                          SMR  329
              WRITE(PRINT,FRM17,NSTEP,K)                                SMR  330
              ELSE                                                      SMR  331
              WRITE(PRINT,FRM18,NSTEP,K) ;                              SMR  332
              WRITE(PRINT,FRM19,KOEF)                                   SMR  333
         END ;                                                          SMR  334
COMMENT       TESTS TO REMOVE OR ENTER VARIABLE ;                       SMR  335
              IF SWICH THEN                                             SMR  336
              GO TO LL15 ;                                              SMR  337
              FLEV ~ VMIN | DF / VEC[NOVA,NOVA] ;                       SMR  338
              IF FOUT + FLEV > 0 THEN                                   SMR  339
         BEGIN                                                          SMR  340
              K ~ NMIN ;                                                SMR  341
              NENT ~ 0 ;                                                SMR  342
              GO TO LL16                                                SMR  343
         END ;                                                          SMR  344
LL10:         FLEV ~ VMAX | DF / (VEC[NOVA,NOVA] - VMAX) ;              SMR  345
              IF (FIN > FLEV) OR ((FIN = FLEV) AND (FIN { 0)) THEN      SMR  346
              GO TO LL13 ;                                              SMR  347
              K ~ NMAX ;                                                SMR  348
              NENT ~ K ;                                                SMR  349
COMMENT       COMPUTE NEXT MATRIX ;                                     SMR  350
LL16:         IF K { 0 THEN                                             SMR  351
         BEGIN                                                          SMR  352
              REWIND(TAPEB) ;                                           SMR  353
              GO TO START                                               SMR  354
         END ;                                                          SMR  355
              FOR I ~ 1 STEP 1 UNTIL NOVA DO                            SMR  356
         BEGIN                                                          SMR  357
              IF I = K THEN                                             SMR  358
              GO TO LL11 ;                                              SMR  359
              FOR J ~ 1 STEP 1 UNTIL NOVA DO                            SMR  360
         BEGIN                                                          SMR  361
              IF J ! K THEN                                             SMR  362
              VEC[I,J] ~ VEC[I,J] - VEC[I,K] | VEC[K,J] / VEC[K,K]      SMR  363
         END ;                                                          SMR  364
LL11:    END ;                                                          SMR  365
              FOR I ~ 1 STEP 1 UNTIL NOVA DO                            SMR  366
         BEGIN                                                          SMR  367
              IF I ! K THEN                                             SMR  368
              VEC[I,K] ~ - VEC[I,K] / VEC[K,K]                          SMR  369
         END ;                                                          SMR  370
              FOR J ~ 1 STEP 1 UNTIL NOVA DO                            SMR  371
         BEGIN                                                          SMR  372
              IF J ! K THEN                                             SMR  373
              VEC[K,J] ~ VEC[K,J] / VEC[K,K]                            SMR  374
         END ;                                                          SMR  375
              VEC[K,K] ~ 1.0 / VEC[K,K] ;                               SMR  376
              GO TO LL12 ;                                              SMR  377
LL13:         WRITE(PRINT,FRM20,NSTEP) ;                                SMR  378
LL14:         IF STAP THEN                                              SMR  379
              GO TO LL15 ;                                              SMR  380
              SWICH ~ TRUE ;                                            SMR  381
              GO TO LL9 ;                                               SMR  382
LL15:         WRITE(PRINT,FRM21,DIAG) ;                                 SMR  383
              IF PRED THEN                                              SMR  384
         BEGIN                                                          SMR  385
              REWIND(TAPEB) ;                                           SMR  386
              WRITE(PRINT,FRM22) ;                                      SMR  387
              FOR N ~ 1 STEP 1 UNTIL NSAM DO                            SMR  388
         BEGIN                                                          SMR  389
              READ (TAPEB,NOVA+1,DATA[*]) ;                             SMR  390
              YPRE ~ CONS ;                                             SMR  391
              FOR I ~ 1 STEP 1 UNTIL NIN DO                             SMR  392
         BEGIN                                                          SMR  393
              K ~ INDX[I] ;                                             SMR  394
              YPRE ~ YPRE + COEN[I] | DATA[K]                           SMR  395
         END ;                                                          SMR  396
              DEV ~ DATA[NOVA] - YPRE ;                                 SMR  397
              WRITE(PRINT,FRM23,N,DATA[NOVA],YPRE,DEV)                  SMR  398
         END                                                            SMR  399
         END ;                                                          SMR  400
              REWIND(TAPEB) ;                                           SMR  401
         END                                                            SMR  402
         END ;                                                          SMR  403
              GO TO START ;                                             SMR  404
NOMO:    END PROGRAM.                                                   SMR  405
 LABEL  000000000CARDR  0010000001
PROF. SERV. DEPT. PROB.   90010000.001000002.500000002.50000    4   15FALSE TRUE
 TRUE TRUE TRUE TRUE TRUE TRUEFALSE

  32.0  48.0  54.0  15.0
  36.0  33.0  19.0  16.0
   3.0  28.0  30.0  14.0
  12.0  33.0  64.0  22.0
  36.0  34.0  60.0  24.0
  24.0  36.0  53.0  19.0
  19.0  42.0  29.0  13.0
  20.0  33.0  55.0  15.0
  27.0  36.0  62.0  23.0
  15.0  22.0  33.0  12.0
  45.0  46.0  68.0  25.0
   9.0  28.0  42.0  17.0
  11.0  32.0  45.0  18.0
  33.0  34.0  39.0  19.0
  21.0  45.0  39.0  18.0
PROF. SERV. DEPT. PROB.   9032   0.001     2.5       0.2        4   15 TRUE TRUE
 TRUE TRUE TRUE TRUE TRUEFALSE TRUE-1

  4  9  9
  32.0  48.0  54.0  15.0   0.1
  36.0  33.0  19.0  16.0   1.0
   3.0  28.0  30.0  14.0   1.0
  12.0  33.0  64.0  22.0   1.0
  36.0  34.0  60.0  24.0   0.4
  24.0  36.0  53.0  19.0   3.0
  19.0  42.0  29.0  13.0   1.0
  20.0  33.0  55.0  15.0   0.2
  27.0  36.0  62.0  23.0   1.0
  15.0  22.0  33.0  12.0   0.3
  45.0  46.0  68.0  25.0   2.0
   9.0  28.0  42.0  17.0   1.0
  11.0  32.0  45.0  18.0   1.0
  33.0  34.0  39.0  19.0   1.0
  21.0  45.0  39.0  18.0   1.0
