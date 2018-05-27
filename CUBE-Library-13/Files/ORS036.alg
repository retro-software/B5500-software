 LABEL  000000000PUNCH  00165064010000000000000000000000        0MAKCAST
         BEGIN                                                          CTAB   1
COMMENT       CROSS TABULATION WITH CORRELATION AND CHI SQUARE          CTAB   2
              C. L. CLARK                                               CTAB   3
              PROFESSIONAL SERVICES DIVISIONAL GROUP                    CTAB   4
              BURROUGHS CORPORATION                                     CTAB   5
              PROGRAM CONTAINS     CARDS                                CTAB   6
              CARD SEQUENCE STARTS WITH CTAB  1                         CTAB   7
              FIRST RELEASE DATE           ;                            CTAB   8
INTEGER       NVAR,NREP,NSEL,I,J,K,L ;                                  CTAB   9
INTEGER ARRAY ID[0:4] ;                                                 CTAB  10
BOOLEAN       ZROW ;                                                    CTAB  11
LABEL         START,NOMO,LL1 ;                                          CTAB  12
FORMAT IN     FORM1(5A6,3I6,2L5),                                       CTAB  13
              FORM2(14I5),                                              CTAB  14
              FORM3(12I6),                                              CTAB  15
              FORM4(I3,I2,22I3) ;                                       CTAB  16
FORMAT OUT    TITLE(X52,"CROSS TABULATION"///X45,5A6//),                CTAB  17
              FRM0 (X1,"VARIABLE",I4,"AND",I4,"RANGES ARE GREATER THAN",CTAB  18
                   " 34"//),                                            CTAB  19
              FRM1 (//////X1,"VARIABLE",I4," IS CROSS TABULATED WITH ", CTAB  20
                    "VARIABLE",I4//                                     CTAB  21
                    X1,"NUMBER OF REPLICATIONS IS",I5//                 CTAB  22
                    X1,"VARIABLE MAXIMUM MINIMUM"/                      CTAB  23
                    X1,I6,2I8/X1,I6,2I8//),                             CTAB  24
              FRM2 (X1,"NO ENTRY IN THE TABLE"//),                      CTAB  25
              FRM3 (" ROW",X7,"(",I2,")"/"TOTAL",X6,"RANGE"),           CTAB  26
              FRM4 (I4,X7,I4," *",34I3),                                CTAB  27
              FRM5 (X16,"*********************************************",CTAB  28
                   "**************************************************",CTAB  29
                   "********"),                                         CTAB  30
              FRM6 (X11," (",I2,")",I4,17I6),                           CTAB  31
              FRM7 (X11," RANGE",18I6),                                 CTAB  32
              FRM8 (/X11,"COLUMN",I4,17I6),                             CTAB  33
              FRM9 (X11," TOTAL",18I6),                                 CTAB  34
              FRM10(/"GRAND TOTAL =",I6//),                             CTAB  35
              FRM11(X1,"CORRELATION COEFFICIENT IS",F8.4//),            CTAB  36
              FRM12(X1,"CHI SQUARE IS NOT COMPUTABLE"//),               CTAB  37
              FRM13(X1,"CHI SQUARE =",F8.3,X5,"DF =",I5//),             CTAB  38
              FRM14(X1,"OVERFLOW FREQUENCIES"/X4,"ROW  COLUMN   COUNT"/ CTAB  39
                   (X4,I3,X4,I3,X5,I5/)),                               CTAB  40
              FRM15(//X1,"VALUES NOT ENTERED",I4),                      CTAB  41
              FRM16(///X4,"CASE NO.",X5,"VARIABLE",I3,X5,"VARIABLE",I3/ CTAB  42
                   (X4,I5,X9,I6,X10,I6)) ;                              CTAB  43
FILE IN       CARDR(1,10) ;                                             CTAB  44
FILE OUT      PRINT 1 (1,15) ;                                          CTAB  45
START:   BEGIN                                                          CTAB  46
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NVAR,NREP,NSEL,    CTAB  47
                   ZROW) ;                                              CTAB  48
              READ (CARDR,FORM1,HEAD) [NOMO]                            CTAB  49
         END ;                                                          CTAB  50
LL1:     BEGIN                                                          CTAB  51
INTEGER       NNN,NAXT,NUM,MAXX,MINX,NEWX,MUCH,OVER,LIMX,LIMY,EVEN,ODD, CTAB  52
              MAXY,MINY,NEWY,DIF,IT,LOV,JUNX,JUNXY,TOTL,LL ;            CTAB  53
INTEGER ARRAY DATA[0:NVAR,0:NREP],MAX[0:NVAR],MIN[0:NVAR],              CTAB  54
              LEV[0:22],X[0:NREP],Y[0:NREP],RANGX[0:40],RANGY[0:100],   CTAB  55
              OUTX[0:50],OUTY[0:50],ITEM[0:50],PT[0:100,0:40],          CTAB  56
              OPT[0:200,0:3],MARY[0:100],MARX[0:40] ;                   CTAB  57
BOOLEAN       LOK,CROS ;                                                CTAB  58
REAL          SUMX,SUMY,SUMXX,SUMXY,SUMYY,RXY,TOP,BOT,CHI,DEN,R,S ;     CTAB  59
LABEL         L1,CROSS,CHANG,CORR,OFLO,L2,DROP,SKIP ;                   CTAB  60
LIST          RANG(FOR I ~ 1 STEP 1 UNTIL NVAR DO [MAX[I],MIN[I]]),     CTAB  61
              DATR(FOR K ~ 1 STEP 1 UNTIL NVAR DO DATA[K,J]),           CTAB  62
              SEL (NAXT,NUM,FOR I ~ 1 STEP 1 UNTIL 22 DO LEV[I]),       CTAB  63
              OUTT(FOR J ~ 0 STEP 1 UNTIL 4 DO ID[J]),                  CTAB  64
              ROW (MARY[K],RANGY[K],                                    CTAB  65
              FOR J ~ 1 STEP 1 UNTIL LIMX DO PT[K,J]),                  CTAB  66
              ABSO(NEWX,RANGX[1],                                       CTAB  67
                   FOR I ~ 1 STEP 1 UNTIL ODD DO RANGX[2|I+1]),         CTAB  68
              ABSE(FOR I ~ 1 STEP 1 UNTIL EVEN DO RANGX[2|I]),          CTAB  69
              CTLO(MARX[1],FOR I ~ 1 STEP 1 UNTIL ODD DO MARX[2|I+1]),  CTAB  70
              CTLE(FOR I ~ 1 STEP 1 UNTIL EVEN DO MARX[2|I]),           CTAB  71
              OFRE(FOR I ~ 1 STEP 1 UNTIL OVER DO [OPT[I,1],OPT[I,2],   CTAB  72
                   OPT[I,3]]),                                          CTAB  73
              OMIT(NEWY,NEWX,FOR I ~ 1 STEP 1 UNTIL JUNX DO [ITEM[I],   CTAB  74
                   OUTY[I],OUTX[I]]) ;                                  CTAB  75
              WRITE(PRINT[PAGE]) ;                                      CTAB  76
              WRITE(PRINT,TITLE,OUTT) ;                                 CTAB  77
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            CTAB  78
              FOR J ~ 1 STEP 1 UNTIL NREP DO                            CTAB  79
              DATA[I,J] ~ 0 ;                                           CTAB  80
COMMENT       READ MAX AND MIN LIMITS FOR EACH VARIABLE ;               CTAB  81
              READ(CARDR,FORM2,RANG) ;                                  CTAB  82
              FOR J ~ 1 STEP 1 UNTIL NREP DO                            CTAB  83
              READ(CARDR,FORM3,DATR) ;                                  CTAB  84
              NNN ~ 1 ;                                                 CTAB  85
L2:           IF NNN > NSEL THEN                                        CTAB  86
              GO TO START ;                                             CTAB  87
              READ(CARDR,FORM4,SEL) ;                                   CTAB  88
              MAXX ~ MAX[NAXT] ;                                        CTAB  89
              MINX ~ MIN[NAXT] ;                                        CTAB  90
              NEWX ~ NAXT ;                                             CTAB  91
              K ~ 0 ;                                                   CTAB  92
              FOR J ~ 1 STEP 1 UNTIL NREP DO                            CTAB  93
              X[J] ~ DATA[NAXT,J] ;                                     CTAB  94
              MUCH ~ 1 ;                                                CTAB  95
L1:           IF MUCH > NUM THEN                                        CTAB  96
         BEGIN                                                          CTAB  97
              NNN ~ NNN +1 ;                                            CTAB  98
              GO TO L2                                                  CTAB  99
         END ;                                                          CTAB 100
              LOV ~ LEV[MUCH] ;                                         CTAB 101
              FOR L ~ 1 STEP 1 UNTIL NREP DO                            CTAB 102
              Y[L] ~ DATA[LOV,L] ;                                      CTAB 103
              LOK ~ FALSE ;                                             CTAB 104
              MAXY ~ MAX[LOV] ;                                         CTAB 105
              MINY ~ MIN[LOV] ;                                         CTAB 106
              NEWY ~ LOV ;                                              CTAB 107
              DIF ~ MAXX - MINX ;                                       CTAB 108
              IF DIF { 34 THEN                                          CTAB 109
              GO TO CROSS ;                                             CTAB 110
              DIF ~ MAXY - MINY ;                                       CTAB 111
              IF DIF { 34 THEN                                          CTAB 112
         BEGIN                                                          CTAB 113
              LOK ~ TRUE ;                                              CTAB 114
              CROS ~ TRUE ;                                             CTAB 115
              GO TO CHANG                                               CTAB 116
         END ;                                                          CTAB 117
              WRITE(PRINT,FRM0,NEWY,NEWX) ;                             CTAB 118
              LOK ~ FALSE ;                                             CTAB 119
              GO TO CHANG ;                                             CTAB 120
COMMENT       CROSS TABULATE ;                                          CTAB 121
CROSS:        RANGX[1] ~ MINX ;                                         CTAB 122
              RANGY[1] ~ MINY ;                                         CTAB 123
              LIMX ~ MAXX - MINX + 1 ;                                  CTAB 124
              LIMY ~ MAXY - MINY + 1 ;                                  CTAB 125
              FOR I ~ 2 STEP 1 UNTIL LIMX DO                            CTAB 126
              RANGX[I] ~ RANGX[I-1] + 1 ;                               CTAB 127
              FOR I ~ 2 STEP 1 UNTIL LIMY DO                            CTAB 128
              RANGY[I] ~ RANGY[I-1] + 1 ;                               CTAB 129
COMMENT       COMPUTE FREQUENCY MATRIX AND LIST OF EXTREME VALUES ;     CTAB 130
              JUNX ~ 0 ;                                                CTAB 131
              JUNXY ~ 0 ;                                               CTAB 132
              FOR I ~ 1 STEP 1 UNTIL LIMY DO                            CTAB 133
              FOR J ~ 1 STEP 1 UNTIL LIMX DO                            CTAB 134
              PT[I,J] ~ 0 ;                                             CTAB 135
              FOR L ~ 1 STEP 1 UNTIL NREP DO                            CTAB 136
         BEGIN                                                          CTAB 137
              IF (X[L]>MAXX)OR(X[L]<MINX)OR(Y[L]>MAXY)OR(Y[L]<MINY) THENCTAB 138
         BEGIN                                                          CTAB 139
              JUNXY ~ JUNXY + 1 ;                                       CTAB 140
              IF JUNXY { 49 THEN                                        CTAB 141
         BEGIN                                                          CTAB 142
              JUNX ~ JUNX + 1 ;                                         CTAB 143
              OUTX[JUNX] ~ X[L] ;                                       CTAB 144
              OUTY[JUNX] ~ Y[L] ;                                       CTAB 145
              ITEM[JUNX] ~ L                                            CTAB 146
         END                                                            CTAB 147
         END                                                            CTAB 148
              ELSE                                                      CTAB 149
         BEGIN                                                          CTAB 150
              I ~ X[L] - MINX + 1 ;                                     CTAB 151
              J ~ Y[L] - MINY + 1 ;                                     CTAB 152
              PT[J,I] ~ PT[J,I] + 1                                     CTAB 153
         END                                                            CTAB 154
         END ;                                                          CTAB 155
COMMENT       COMPUTE OVERFLOW MATRIX ;                                 CTAB 156
              OVER ~ 0 ;                                                CTAB 157
              FOR J ~ 1 STEP 1 UNTIL LIMY DO                            CTAB 158
              FOR I ~ 1 STEP 1 UNTIL LIMX DO                            CTAB 159
         BEGIN                                                          CTAB 160
              IF PT[J,I] = 999 THEN                                     CTAB 161
         BEGIN                                                          CTAB 162
              OVER ~ OVER + 1 ;                                         CTAB 163
              OPT[OVER,1] ~ RANGY[J] ;                                  CTAB 164
              OPT[OVER,2] ~ RANGX[I] ;                                  CTAB 165
              OPT[OVER,3] ~ PT[J,I] - 999 ;                             CTAB 166
              PT[J,I] ~ PT[J,I] - OPT[OVER,3]                           CTAB 167
         END                                                            CTAB 168
         END ;                                                          CTAB 169
              FOR I ~ 1 STEP 1 UNTIL LIMY DO                            CTAB 170
              MARY[I] ~ 0 ;                                             CTAB 171
              FOR J ~ 1 STEP 1 UNTIL LIMX DO                            CTAB 172
              MARX[J] ~ 0 ;                                             CTAB 173
              FOR I ~ 1 STEP 1 UNTIL LIMY DO                            CTAB 174
              FOR J ~ 1 STEP 1 UNTIL LIMX DO                            CTAB 175
         BEGIN                                                          CTAB 176
              MARY[I] ~ MARY[I] + PT[I,J] ;                             CTAB 177
              MARX[J] ~ MARX[J] + PT[I,J]                               CTAB 178
         END ;                                                          CTAB 179
              WRITE(PRINT,FRM1,NEWY,NEWX,NREP,NEWY,MAXY,MINY,NEWX,MAXX, CTAB 180
                   MINX) ;                                              CTAB 181
              TOTL ~ 0 ;                                                CTAB 182
              FOR I ~ 1 STEP 1 UNTIL LIMX DO                            CTAB 183
              TOTL ~ TOTL + MARX[I] ;                                   CTAB 184
              IF TOTL { 0 THEN                                          CTAB 185
         BEGIN                                                          CTAB 186
              WRITE(PRINT,FRM2) ;                                       CTAB 187
              GO TO CHANG                                               CTAB 188
         END ;                                                          CTAB 189
              WRITE(PRINT,FRM3,NEWY) ;                                  CTAB 190
              IF ZROW THEN                                              CTAB 191
         BEGIN                                                          CTAB 192
              LL ~ 0 ;                                                  CTAB 193
              FOR I ~ 1 STEP 1 UNTIL LIMY DO                            CTAB 194
         BEGIN                                                          CTAB 195
              IF MARY[I] = 0 THEN                                       CTAB 196
              GO TO DROP ;                                              CTAB 197
              LL ~ LL + 1 ;                                             CTAB 198
              RANGY[LL] ~ RANGY[I] ;                                    CTAB 199
              FOR J ~ 1 STEP 1 UNTIL LIMX DO                            CTAB 200
              PT[LL,J] ~ PT[I,J] ;                                      CTAB 201
              MARY[LL] ~ MARY[I] ;                                      CTAB 202
DROP:    END ;                                                          CTAB 203
              LIMY ~ LL ;                                               CTAB 204
              LL ~ 0 ;                                                  CTAB 205
              FOR I ~ 1 STEP 1 UNTIL LIMX DO                            CTAB 206
         BEGIN                                                          CTAB 207
              IF MARX[I] = 0 THEN                                       CTAB 208
              GO TO SKIP ;                                              CTAB 209
              LL ~ LL + 1 ;                                             CTAB 210
              RANGX[LL] ~ RANGX[I] ;                                    CTAB 211
              FOR J ~ 1 STEP 1 UNTIL LIMY DO                            CTAB 212
              PT[J,LL] ~ PT[J,I] ;                                      CTAB 213
              MARX[LL] ~ MARX[I] ;                                      CTAB 214
SKIP:    END ;                                                          CTAB 215
              LIMX ~ LL                                                 CTAB 216
         END ;                                                          CTAB 217
              FOR L ~ 1 STEP 1 UNTIL LIMY DO                            CTAB 218
         BEGIN                                                          CTAB 219
              K ~ LIMY + 1 - L ;                                        CTAB 220
              WRITE(PRINT,FRM4,ROW)                                     CTAB 221
         END ;                                                          CTAB 222
              WRITE(PRINT,FRM5) ;                                       CTAB 223
              IF LIMX > 2|(LIMX DIV 2) THEN                             CTAB 224
         BEGIN                                                          CTAB 225
              ODD ~ LIMX DIV 2 ;                                        CTAB 226
              EVEN ~ ODD                                                CTAB 227
         END                                                            CTAB 228
              ELSE                                                      CTAB 229
         BEGIN                                                          CTAB 230
              ODD ~ LIMX DIV 2 - 1 ;                                    CTAB 231
              EVEN ~ ODD + 1                                            CTAB 232
         END ;                                                          CTAB 233
              WRITE(PRINT,FRM6,ABSO) ;                                  CTAB 234
              WRITE(PRINT,FRM7,ABSE) ;                                  CTAB 235
              WRITE(PRINT,FRM8,CTLO) ;                                  CTAB 236
              WRITE(PRINT,FRM9,CTLE) ;                                  CTAB 237
              WRITE(PRINT,FRM10,TOTL) ;                                 CTAB 238
COMMENT       COMPUTE CORRELATION COEFFICIENT ;                         CTAB 239
CORR:         SUMX ~ 0.0 ;                                              CTAB 240
              SUMY ~ 0.0 ;                                              CTAB 241
              SUMXX ~ 0.0 ;                                             CTAB 242
              SUMXY ~ 0.0 ;                                             CTAB 243
              SUMYY ~ 0.0 ;                                             CTAB 244
              FOR I ~ 1 STEP 1 UNTIL NREP DO                            CTAB 245
         BEGIN                                                          CTAB 246
              R ~ X[I] ;                                                CTAB 247
              S ~ Y[I] ;                                                CTAB 248
              SUMX ~ SUMX + R ;                                         CTAB 249
              SUMY ~ SUMY + S ;                                         CTAB 250
              SUMXX ~ SUMXX + R|R ;                                     CTAB 251
              SUMXY ~ SUMXY + R|S ;                                     CTAB 252
              SUMYY ~ SUMYY + S|S                                       CTAB 253
         END ;                                                          CTAB 254
              TOP ~ NREP|SUMXY - SUMX|SUMY ;                            CTAB 255
              BOT ~ SQRT((NREP|SUMXX-SUMX|SUMX)|(NREP|SUMYY-SUMY|SUMY));CTAB 256
              IF BOT = 0.0 THEN                                         CTAB 257
         BEGIN                                                          CTAB 258
              WRITE(PRINT,FRM12) ;                                      CTAB 259
              GO TO OFLO                                                CTAB 260
         END ;                                                          CTAB 261
              RXY ~ TOP / BOT ;                                         CTAB 262
              WRITE(PRINT,FRM11,RXY) ;                                  CTAB 263
COMMENT       COMPUTE CHI SQUARE ;                                      CTAB 264
              CHI ~ 0.0 ;                                               CTAB 265
              FOR I ~ 1 STEP 1 UNTIL LIMY DO                            CTAB 266
              FOR J ~ 1 STEP 1 UNTIL LIMX DO                            CTAB 267
         BEGIN                                                          CTAB 268
              BOT ~ MARY[I] | MARX[J] ;                                 CTAB 269
              IF BOT = 0.0 THEN                                         CTAB 270
         BEGIN                                                          CTAB 271
              WRITE(PRINT,FRM12) ;                                      CTAB 272
              GO TO OFLO                                                CTAB 273
         END ;                                                          CTAB 274
              TOP ~ PT[I,J] - BOT/TOTL ;                                CTAB 275
              CHI ~ CHI + TOP|TOP|TOTL/BOT                              CTAB 276
         END ;                                                          CTAB 277
              IT ~ (LIMY-1)|(LIMX-1) ;                                  CTAB 278
              WRITE(PRINT,FRM13,CHI,IT) ;                               CTAB 279
COMMENT       LIST OVERFLOW FREQUENCIES ;                               CTAB 280
OFLO:         IF OVER > 0 THEN                                          CTAB 281
              WRITE(PRINT,FRM14,OFRE) ;                                 CTAB 282
COMMENT       LIST VALUES NOT ENTERED ;                                 CTAB 283
              IF JUNXY > 0 THEN                                         CTAB 284
         BEGIN                                                          CTAB 285
              WRITE(PRINT,FRM15,JUNXY) ;                                CTAB 286
              IF JUNXY { 50 THEN                                        CTAB 287
         BEGIN                                                          CTAB 288
              WRITE(PRINT,FRM16,OMIT)                                   CTAB 289
         END                                                            CTAB 290
         END ;                                                          CTAB 291
              CROS ~ FALSE ;                                            CTAB 292
CHANG:        IF LOK THEN                                               CTAB 293
         BEGIN                                                          CTAB 294
              IT ~ NEWY ;                                               CTAB 295
              NEWY ~ NEWX ;                                             CTAB 296
              NEWX ~ IT ;                                               CTAB 297
              IT ~ MAXX ;                                               CTAB 298
              MAXX ~ MAXY ;                                             CTAB 299
              MAXY ~ IT ;                                               CTAB 300
              IT ~ MINX ;                                               CTAB 301
              MINX ~ MINY ;                                             CTAB 302
              MINY ~ IT ;                                               CTAB 303
              FOR L ~ 1 STEP 1 UNTIL NREP DO                            CTAB 304
         BEGIN                                                          CTAB 305
              IT ~ X[L] ;                                               CTAB 306
              X[L] ~ Y[L] ;                                             CTAB 307
              Y[L] ~ IT                                                 CTAB 308
         END ;                                                          CTAB 309
              IF CROS THEN                                              CTAB 310
              GO TO CROSS                                               CTAB 311
         END ;                                                          CTAB 312
              MUCH ~ MUCH + 1 ;                                         CTAB 313
              GO TO L1                                                  CTAB 314
         END ;                                                          CTAB 315
NOMO:    END PROGRAM .                                                  CTAB 316
 LABEL  000000000CARDR  0010000001
PROF. SERV. DEPT. PROB.     70     4   129     2 TRUEFALSE
   10    0   15    0   10    0   20    0
     1     1     1     1
     1     2     1     1
     2     3     1     2
     2     4     1     2
     3     5     1     2
     3     6     2     3
     4     7     2     3
     4     8     2     3
     5     9     2     3
     5    10     2     4
     6    11     2     4
     6    12     2     4
     7    13     2     4
     7    14     2     4
     8    15     3     5
     8    16     3     5
     9    17     3     5
     9    18     3     8
    10    19     3     5
    10    20     3     5
     3     1     3     6
     3     2     3     6
     4     3     3     6
     4     4     3     6
     5     5     3     6
     5     6     3     6
     6     7     3     6
     6     8     4     7
     7     9     4     7
     7    10     4     7
     8    11     4     7
     8    12     4     7
     9    13     4     7
     9    14     4     7
    10    15     4     7
    10    16     4     7
     1     5     4     8
     1     6     4     8
     2     7     4     8
     2     8     4     8
     3     9     4     8
     3    10     4     8
     4    11     4     8
     4    12     4     8
     5    13     5     9
     5    14     5     9
     6    15     5     9
     6    16     5     9
     7    17     5     9
     7    18     5     9
     8    19     5     9
     8    20     5     9
     7     1     5     9
     7     2     5     9
     8     3     5    10
     8     4     5    10
     9     5     5    10
     9     6     5    10
    10     7     5    10
    10     8     5    10
     9     1     5    10
     9     2     5    10
    10     3     5    10
    10     4     5    10
    10     1     5    10
    10     1     6    11
    10     1     6    11
    10     1     6    11
     1    10     6    11
     1    11     6    11
     2    12     6    11
     2    13     6    11
     3    14     6    11
     3    15     6    11
     4    16     6    11
     4    17     6    11
     5    18     6    12
     5    19     6    12
     6    20     6    12
     1    13     6    12
     1    14     6    12
     2    15     6    12
     2    16     6    12
     3    17     6    12
     3    18     6    12
     4    19     6    12
     4    20     7    13
     1    16     7    13
     1    17     7    13
     2    18     7    13
     2    19     7    13
     3    20     7    13
     1    20     7    13
     1    20     7    13
     1    20     7    13
     1    20     7    14
    10     1     7    14
    10     2     7    14
     9     3     7    14
     9     4     7    14
     8     5     7    14
     8     6     7    14
     7     7     7    14
     7     8     8    15
     6     9     8    15
     6    10     8    15
     5    11     8    15
     5    12     8    15
     4    13     8    15
     4    14     8    15
     3    15     8    16
     3    16     8    16
     2    17     8    16
     2    18     8    16
     1    19     8    16
     1    20     8    16
     3    17     9    17
     8    13     9    17
     6     1     9    17
     2     6     9    17
     5    14     9    17
     7     2     9    18
     3     5     9    18
     1    19     9    18
     9    13     9    18
    10    12    10    19
     2     7    10    19
     4     3    10    19
     5    11    10    20
  3 2  2  4
  4 1  1
 LABEL  000000000PUNCH  00165064010000000000000000000000        0MAKCAST
