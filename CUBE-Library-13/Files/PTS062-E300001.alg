         BEGIN
COMMENT       ED SEIDMAN ;
         BEGIN                                                          SRFT   1
COMMENT       MULTIDIMENSIONAL SURFACE FIT PROGRAM. THIS METHOD USES    SRFT   2
              ORTHOGONAL POLYNOMIALS WHICH PERMIT TRUNCATION WITHOUT    SRFT   3
              RECOMPUTING THE COEFFICIENTS. A NEW VALUE FOR THE ERROR ISSRFT   4
              COMPUTED FOR EACH TRUNCATION PASS. ;                      SRFT   5
BOOLEAN       SW, ST ;                                                  SRFT   6
FILE IN       CARD (2,10) ;                                             SRFT   8
FILE OUT      PAPER 1(3,15),                                            SRFT   9
              PUNCH 0(2,10) ;                                           SRFT  10
INTEGER       KMAX, PMAX, V, N ;                                        SRFT  16
LIST          SIZESI (KMAX, PMAX, V, N, ST, SW) ;                       SRFT  17
FORMAT IN     CFI (6I10) ;                                              SRFT  18
              READ (CARD, CFI, SIZESI) ;                                SRFT  19
         BEGIN                                                          SRFT  20
INTEGER       I, J, D, M, X, T, T1, T2, T3, KM ;                        SRFT  21
INTEGER ARRAY NAME[0:V], K[0:V], P[0:V], DEGREE[0:V], DEG[0:V],         SRFT  22
              NEWP[0:V], TOTAL[0:V], TALLY[0:V] ;                       SRFT  23
REAL          ALPHATEMP, BETATEMP, DIFF, RHOTEMP, SUM, SUMS ;           SRFT  24
REAL ARRAY    TT[0:V], DATA[0:KMAX], POWERS[0:KMAX], ALEPH[0:PMAX],     SRFT  25
              BETA[0:PMAX], PHI[0:N], RHO[0:V, 0:PMAX+1],               SRFT  26
              POLY[0:V | KMAX | (PMAX+1)], COEFF[0:3 | (PMAX+1)] ;      SRFT  27
LIST          SIZESF (FOR M ~ 1 STEP 1 UNTIL V DO                       SRFT  33
              [NAME[M], K[M], P[M]]),                                   SRFT  34
              POINTS (FOR D ~ 1 STEP 1 UNTIL K[M] DO                    SRFT  35
              DATA[D]),                                                 SRFT  36
              HO (M),                                                   SRFT  37
              ALPHABETA (ALEPH[I], BETA[I]),                            SRFT  38
              RHOVALUES (FOR D ~ 1 STEP 1 UNTIL P[M]+1 DO               SRFT  39
              RHO[M,D]),                                                SRFT  40
              POLYS (FOR D ~ T3 STEP 1 UNTIL K[M]+T3-1 DO               SRFT  41
              POLY[D]),                                                 SRFT  42
              PABO (M, P[M], FOR D ~ 1 STEP 1 UNTIL P[M] DO             SRFT  43
              [ALEPH[D], BETA[D]]) ;                                    SRFT  44
FORMAT IN     CFF (3I10),                                               SRFT  66
              CF2 (4E20.11) ;                                           SRFT  67
FORMAT OUT    HEAD (X42, "MULTIDIMENSIONAL SURFACE FIT PROGRAM" //),    SRFT  68
              HF (// X50, "VARIABLE", I5 //),                           SRFT  69
              TITLEAB (X61, "ALPHA[I]", X13, "BETA[I]" //),             SRFT  70
              OOT (X50, 2E20.11 /),                                     SRFT  71
              TITLER (/ X50, "RHO[I]" //),                              SRFT  72
              CF3 (6E20.11),                                            SRFT  73
              TITLEP (// X50, "POLYNOMIALS" //),                        SRFT  74
              PABF (2I5 / (2E20.11)) ;                                  SRFT  75
              READ (CARD, CFF, SIZESF) ;                                SRFT  78
              WRITE (PAPER[PAGE], OOT) ;
              WRITE (PAPER, HEAD) ;                                     SRFT  79
              D ~ 1 ;                                                   SRFT  28
              FOR I ~ 1 STEP 1 UNTIL V DO                               SRFT  29
              D ~ (P[I] + 1) | D ;                                      SRFT  30
         BEGIN                                                          SRFT  31
REAL ARRAY    COEFFICIENTS[0:D] ;                                       SRFT  32
LIST          MAG (ALPHATEMP, BETATEMP, D, DIFF, I, J, M, N, V,         SRFT  45
              PMAX, RHOTEMP, SUM, SUMS, T, T1, T2, T3, X, KMAX, KM,     SRFT  46
              FOR I ~ 1 STEP 1 UNTIL V DO                               SRFT  47
              [DEG[I], DEGREE[I], K[I], NAME[I], NEWP[I], P[I], TT[I],  SRFT  48
              TALLY[I], TOTAL[I]],                                      SRFT  49
              FOR I ~ 1 STEP 1 UNTIL KMAX DO                            SRFT  50
              [DATA[I], POWERS [I]],                                    SRFT  51
              FOR I ~ 1 STEP 1 UNTIL PMAX DO                            SRFT  52
              [ALEPH[I], BETA[I]],                                      SRFT  53
              FOR I ~ 1 STEP 1 UNTIL V DO                               SRFT  54
              FOR J ~ 1 STEP 1 UNTIL PMAX+1 DO                          SRFT  55
              RHO[I,J],                                                 SRFT  56
              FOR I ~ 1 STEP 1 UNTIL 3 | (PMAX+1) DO                    SRFT  59
              COEFF[I]),                                                SRFT  60
              MAGPO (FOR I ~ 1 STEP 1 UNTIL X-1 DO                      SRFT  57
              POLY[I]),                                                 SRFT  58
              MAGC (FOR I ~ 1 STEP 1 UNTIL T3 DO                        SRFT  61
              COEFFICIENTS[I]),                                         SRFT  62
              MAGPH (FOR I ~ 1 STEP 1 UNTIL N DO                        SRFT  63
              PHI[I]) ;                                                 SRFT  64
LABEL         VARIABLE, MAJORLOOP, RESET, LAST, FURTHER, ONE, TRUNCATE, SRFT  76
              COEFFICIENTC, THEEND, REPLACE ;                           SRFT  77
              IF SW THEN GO TO REPLACE ;                                SRFT  14
              TOTAL[1] ~ X ~ I ~ T3 ~ 1 ;                               SRFT  80
              FOR M ~ 2 STEP 1 UNTIL V DO                               SRFT  81
              TOTAL[M] ~ (P[M-1] + 1) | K[M-1] + TOTAL[M-1] ;           SRFT  82
              M ~ 1 ;                                                   SRFT  83
COMMENT       THE FOLLOWING SECTION COMPUTES THE RECU-SIVE COEFFICIENTS SRFT  84
              NEEDED FOR THE ORTHOGONAL POLYNOMIALS, AND THEN THE       SRFT  85
              POLYNOMIALS ;                                             SRFT  86
VARIABLE :    WRITE (PAPER, HF, HO) ;                                   SRFT  87
              WRITE (PAPER, TITLEAB) ;                                  SRFT  88
              READ (CARD, CF2, POINTS) ;                                SRFT  89
              BETA[1] ~ 0 ;          KM ~ K[M] ;                        SRFT  90
              FOR J ~ 1 STEP 1 UNTIL KM DO                              SRFT  91
         BEGIN                                                          SRFT  92
              POWERS[J] ~ POLY[X] ~ 1.0 ;          X ~ X + 1            SRFT  93
         END ;                                                          SRFT  94
              RHO[M,1] ~ RHOTEMP ~ KM ;          SUM ~ 0 ;              SRFT  95
              FOR J ~ 1 STEP 1 UNTIL KM DO                              SRFT  96
              SUM ~ DATA[J] + SUM ;                                     SRFT  97
              ALEPH[1] ~ ALPHATEMP ~ -SUM / RHOTEMP ;                   SRFT  98
              FOR J ~ 1 STEP 1 UNTIL KM DO                              SRFT  99
         BEGIN                                                          SRFT 100
              POLY[X] ~ DATA[J] + ALPHATEMP ;          X ~ X + 1        SRFT 101
         END ;                                                          SRFT 102
MAJORLOOP :   RHOTEMP ~ 0 ;                                             SRFT 103
              WRITE (PAPER, OOT, ALPHABETA) ;                           SRFT 104
              I ~ I + 1 ;         T ~ X - KM ;                          SRFT 105
              FOR J ~ 1 STEP 1 UNTIL KM DO                              SRFT 106
         BEGIN                                                          SRFT 107
              POWERS[J] ~ POWERS[J] | DATA[J] ;                         SRFT 108
              RHOTEMP ~ POWERS[J] | POLY[T] + RHOTEMP ;                 SRFT 109
              T ~ T + 1                                                 SRFT 110
         END ;                                                          SRFT 111
COMMENT       THE FOLLOWING TESTS FOR ZERO VALUES OF RHO[M,I] WHICH     SRFT 112
              WOULD CAUSE ARITHMETIC OVERFLOW LATER ON. THE FIT FOR     SRFT 113
              VARIABLE M IS TERMINATED AT THIS POINT AND P[M] IS        SRFT 114
              ADJUSTED ACCORDINGLY ;                                    SRFT 115
              IF RHOTEMP = 0 THEN                                       SRFT 116
         BEGIN                                                          SRFT 117
              P[M] ~ I - 2 ;          GO TO RESET                       SRFT 118
         END ;                                                          SRFT 119
              RHO[M,I] ~ RHOTEMP ;                                      SRFT 120
              IF I = P[M] + 1 THEN GO TO RESET ;                        SRFT 121
              SUM ~ 0 ;          T ~ X - KM ;                           SRFT 122
              FOR J ~ 1 STEP 1 UNTIL KM DO                              SRFT 123
         BEGIN                                                          SRFT 124
              SUM ~ POLY[T]*2 | DATA[J] + SUM ;                         SRFT 125
              T ~ T + 1                                                 SRFT 126
         END ;                                                          SRFT 127
              ALEPH[I] ~ ALPHATEMP ~ -SUM / RHOTEMP ;                   SRFT 128
              BETA[I] ~ BETATEMP ~ -RHOTEMP / RHO[M,I-1] ;              SRFT 129
              T ~ X ;          T1 ~ X - KM ;          T2 ~ T1 - KM ;    SRFT 130
              FOR J ~ 1 STEP 1 UNTIL KM DO                              SRFT 131
         BEGIN                                                          SRFT 132
              POLY[T] ~ (DATA[J] + ALPHATEMP) | POLY[T1] +              SRFT 133
              POLY[T2] | BETATEMP ;                                     SRFT 134
              T ~ T + 1 ;         T1 ~ T1 + 1 ;          T2 ~ T2 + 1    SRFT 135
         END ;                                                          SRFT 136
              X ~ X + KM ;          GO TO MAJORLOOP ;                   SRFT 137
RESET :       WRITE (PAPER, TITLER) ;                                   SRFT 138
              WRITE (PAPER, CF3, RHOVALUES) ;                           SRFT 139
              WRITE (PAPER, TITLEP) ;                                   SRFT 140
              FOR T2 ~ 1 STEP 1 UNTIL P[M]+1 DO                         SRFT 141
         BEGIN                                                          SRFT 142
              WRITE (PAPER, CF3, POLYS) ;                               SRFT 143
              T3 ~ T3 + KM                                              SRFT 144
         END ;                                                          SRFT 145
              WRITE (PUNCH, PABF, PABO) ;                               SRFT 146
COEFFICIENTC :                                                          SRFT 147
         BEGIN                                                          SRFT 148
INTEGER       J2 ;                                                      SRFT 149
LABEL         TESTI ;                                                   SRFT 150
LIST          ID (NAME[M], P[M]),                                       SRFT 151
              CIJO (FOR T ~ J2+1 STEP 1 UNTIL J2+I DO                   SRFT 152
              COEFF[T]) ;                                               SRFT 153
FORMAT OUT    IDENT (// X40, "POWER COEFFICIENTS", 2I5 //),             SRFT 154
              OUTPOLY (6E20.11) ;                                       SRFT 155
              J ~ PMAX + 1 ;          J2 ~ J + J ;                      SRFT 156
              WRITE (PAPER, IDENT, ID) ;                                SRFT 157
              FOR D ~ 1 STEP 1 UNTIL J2+J DO                            SRFT 158
              COEFF[D] ~ 0 ;                                            SRFT 159
              I ~ 1 ;          COEFF[J+2] ~ COEFF[J2+1] ~ ALEPH[1] ;    SRFT 160
              WRITE (PAPER, OUTPOLY, CIJO) ;                            SRFT 161
              COEFF[1] ~ COEFF[J+3] ~ 1.0 ;                             SRFT 162
TESTI :       IF I = P[M] THEN GO TO LAST ;                             SRFT 163
              I ~ I + 1 ;          T ~ J + 1 ;                          SRFT 164
              FOR D ~ J2+1 STEP 1 UNTIL J2+I DO                         SRFT 165
         BEGIN                                                          SRFT 166
              COEFF[D] ~ ALEPH[I] | COEFF[T+1] + COEFF[T] +             SRFT 167
              BETA[I] | COEFF[T-J] ;                                    SRFT 168
              T ~ T + 1                                                 SRFT 169
         END ;                                                          SRFT 170
              WRITE (PAPER, OUTPOLY, CIJO) ;                            SRFT 171
              T ~ J + 1 ;                                               SRFT 172
              FOR D ~ J+2 STEP 1 UNTIL J+I+1 DO                         SRFT 173
         BEGIN                                                          SRFT 174
              COEFF[T-J] ~ COEFF[D] ;                                   SRFT 175
              COEFF[D] ~ COEFF[T+J] ;          T ~ T + 1                SRFT 176
         END ;                                                          SRFT 177
              COEFF[D] ~ 1.0 ;          GO TO TESTI                     SRFT 178
         END COEFFICIENTC ;                                             SRFT 179
LAST :        IF M = V THEN GO TO FURTHER ;                             SRFT 180
              M ~ M + 1 ;          I ~ 1 ;          GO TO VARIABLE ;    SRFT 181
COMMENT       THE FOLLOWING SECTION COMPUTES THE COEFFICIENTS OF THE    SRFT 182
              ORTHOGONAL POLYNOMIALS ;                                  SRFT 183
FURTHER :                                                               SRFT 184
         BEGIN                                                          SRFT 185
LABEL         BEG1, BACK1, HERE1, REPEAT1, CHECK1, TOWRITE ;            SRFT 186
LIST          DEPENDENTS (FOR D ~ 1 STEP 1 UNTIL N DO                   SRFT 187
              PHI[D]),                                                  SRFT 188
              OUTCOEFF (FOR J ~ 1 STEP 1 UNTIL T3 DO                    SRFT 189
              COEFFICIENTS[J]) ;                                        SRFT 190
FORMAT IN     CFD (4E20.11) ;                                           SRFT 191
FORMAT OUT    ACOEFF (// X50, "ORTHOGONAL COEFFICIENTS" //),            SRFT 192
              FORMCOEFF (4E20.11) ;                                     SRFT 193
              READ (CARD, CFD, DEPENDENTS) ;                            SRFT 194
              CLOSE (CARD, RELEASE) ;
              WRITE (PAPER, ACOEFF) ;                                   SRFT 195
              FOR M ~ 1 STEP 1 UNTIL V DO                               SRFT 196
         BEGIN                                                          SRFT 197
              DEGREE[M] ~ TOTAL[M] ;                                    SRFT 198
              TT[M] ~ 0 ;          DEG[M] ~ 1                           SRFT 199
         END ;                                                          SRFT 200
              T ~ 1 ;                                                   SRFT 201
              SUM ~ 0 ;
BEG1 :        D ~ 1 ;                                                   SRFT 202
              T2 ~ TOTAL[V] + K[V] | DEG[V] - 1 ;                       SRFT 203
              T1 ~ -K[V] + T2 + 1 ;                                     SRFT 204
BACK1 :       FOR I ~ T1 STEP 1 UNTIL T2 DO                             SRFT 205
         BEGIN                                                          SRFT 206
              SUM   ~ SUM   + PHI[D] | POLY[I] ;                        SRFT 207
              D ~ D + 1                                                 SRFT 208
         END ;                                                          SRFT 209
              TT[V] ~ SUM ;          SUM ~ 0 ;
              IF V = 1 THEN GO TO REPEAT1 ;                             SRFT 210
              M ~ V - 1 ;                                               SRFT 211
HERE1 :       TT[M] ~ POLY[K[M]|(DEG[M]-1)+DEGREE[M]] | TT[M+1] + TT[M];SRFT 212
              TT[M+1] ~ 0 ;                                             SRFT 213
              IF DEGREE[M] ~ DEGREE[M] + 1 > TOTAL[M] + K[M] - 1 THEN   SRFT 214
         BEGIN                                                          SRFT 215
              IF M = 1 THEN GO TO REPEAT1 ;                             SRFT 216
              DEGREE[M] ~ TOTAL[M] ;                                    SRFT 217
              M ~ M - 1 ;          GO TO HERE1                          SRFT 218
         END ;                                                          SRFT 219
              GO TO BACK1 ;                                             SRFT 220
REPEAT1 :     FOR M ~ 1 STEP 1 UNTIL V DO                               SRFT 221
              TT[1] ~ TT[1] / RHO[M,DEG[M]] ;                           SRFT 222
              COEFFICIENTS[T] ~ TT[1] ;          T ~ T + 1 ;            SRFT 223
              DEGREE[1] ~ TOTAL[1] ;          M ~ V ;                   SRFT 224
CHECK1 :      IF DEG[M] ! P[M] + 1 THEN                                 SRFT 225
         BEGIN                                                          SRFT 226
              DEG[M] ~ DEG[M] + 1 ;          TT[1] ~ 0 ;                SRFT 227
              GO TO BEG1                                                SRFT 228
         END ;                                                          SRFT 229
              IF M = 1 THEN GO TO TOWRITE ;                             SRFT 230
              DEG[M] ~ 1 ;          M ~ M - 1 ;                         SRFT 231
              GO TO CHECK1 ;                                            SRFT 232
TOWRITE :     T3 ~ T - 1 ;                                              SRFT 233
              WRITE (PAPER, FORMCOEFF, OUTCOEFF) ;                      SRFT 234
              WRITE (PUNCH, FORMCOEFF, OUTCOEFF) ;                      SRFT 235
              CLOSE (PUNCH, RELEASE) ;
         END FURTHER ;                                                  SRFT 236
COMMENT       WRITE MEMORY ON MAGNETIC TAPE FOR LATER TRUNCATION ;      SRFT 237
         BEGIN                                                          SRFT 238
FILE OUT      TAPE (2, 1023) ;                                          SRFT 239
              WRITE (TAPE, *, MAG) ;                                    SRFT 241
              WRITE (TAPE, *, MAGPO) ;
              WRITE (TAPE, *, MAGC) ;
              WRITE (TAPE, *, MAGPH) ;
              GO TO ONE                                                 SRFT 242
         END ;                                                          SRFT 243
ONE :                                                                   SRFT 244
         BEGIN                                                          SRFT 245
LABEL         BEG2, BACK2, HERE2, REPEAT2, CHECK2 ;                     SRFT 246
LIST          DIFFERENCES (PHI[T], TT[1], DIFF),                        SRFT 247
              FINALSUM (SUMS, FOR M ~ 1 STEP 1 UNTIL V DO               SRFT 248
              P[M]) ;                                                   SRFT 249
FORMAT OUT    DF (X35, 3E20.11 /),                                      SRFT 250
              FF (X40, "VARIANCE", E20.11, X5, 10I3 //),                SRFT 251
              ERRORTITLE (// X42, "ORIGINAL", X11, "CALCULATED", X10,   SRFT 252
              "DIFFERENCE" //) ;                                        SRFT 253
              WRITE (PAPER[PAGE], DF) ;
              WRITE (PAPER, ERRORTITLE) ;                               SRFT 254
              FOR M ~ 1 STEP 1 UNTIL V DO                               SRFT 255
         BEGIN                                                          SRFT 256
              TALLY[M] ~ K[M] | P[M] ;                                  SRFT 257
              DEGREE[M] ~ TOTAL[M] ;                                    SRFT 258
              DEG[M] ~ 1 ;          TT[M] ~ 0                           SRFT 259
         END ;                                                          SRFT 260
              SUMS ~ 0 ;          T ~ 1 ;                               SRFT 261
              SUM ~ 0 ;
BEG2 :        D ~ 1 ;                                                   SRFT 262
              T1 ~ TOTAL[V] + DEG[V] - 1 ;                              SRFT 263
              T2 ~ TALLY[V] + T1 ;                                      SRFT 264
BACK2 :       FOR I ~ T1 STEP K[V] UNTIL T2 DO                          SRFT 265
         BEGIN                                                          SRFT 266
              SUM   ~ SUM   + COEFFICIENTS[D] | POLY[I] ;               SRFT 267
              D ~ D + 1                                                 SRFT 268
         END ;                                                          SRFT 269
              TT[V] ~ SUM ;          SUM ~ 0 ;
              IF V = 1 THEN GO TO REPEAT2 ;                             SRFT 270
              M ~ V - 1 ;                                               SRFT 271
HERE2 :       TT[M] ~ POLY[DEGREE[M]+DEG[M]-1] | TT[M+1] + TT[M] ;      SRFT 272
              TT[M+1] ~ 0 ;                                             SRFT 273
              IF DEGREE[M] ~ DEGREE[M] + K[M] > TOTAL[M] + TALLY[M] THENSRFT 274
         BEGIN                                                          SRFT 275
              IF M = 1 THEN GO TO REPEAT2 ;                             SRFT 276
              DEGREE[M] ~ TOTAL[M] ;                                    SRFT 277
              M ~ M - 1 ;          GO TO HERE2                          SRFT 278
         END ;                                                          SRFT 279
              GO TO BACK2 ;                                             SRFT 280
REPEAT2 :     DIFF ~ PHI[T] - TT[1] ;                                   SRFT 281
              WRITE (PAPER, DF, DIFFERENCES) ;                          SRFT 282
              SUMS ~ DIFF | DIFF + SUMS ;                               SRFT 283
              DEGREE[1] ~ TOTAL[1] ;                                    SRFT 284
              T ~ T + 1 ;          M ~ V ;                              SRFT 285
CHECK2 :      IF DEG[M] < K[M] THEN                                     SRFT 286
         BEGIN                                                          SRFT 287
              DEG[M] ~ DEG[M] + 1 ;          TT[1] ~ 0 ;                SRFT 288
              GO TO BEG2                                                SRFT 289
         END ;                                                          SRFT 290
              IF M = 1 THEN                                             SRFT 291
         BEGIN                                                          SRFT 292
              SUMS ~ SUMS / N ;                                         SRFT 293
              WRITE (PAPER, FF, FINALSUM) ;                             SRFT 294
              IF ST THEN GO TO TRUNCATE ;                               SRFT 295
              GO TO THEEND                                              SRFT 296
         END ;                                                          SRFT 297
              DEG[M] ~ 1 ;          M ~ M - 1 ;                         SRFT 298
              GO TO CHECK2                                              SRFT 299
         END ONE ;                                                      SRFT 300
COMMENT       REPLACE MEMORY FROM PREVIOUSLY WRITTEN MAGNETIC TAPE ;    SRFT 301
REPLACE :                                                               SRFT 302
         BEGIN                                                          SRFT 303
FILE IN       TAPE (2, 1023) ;                                          SRFT 304
              READ (TAPE, *, MAG) ;                                     SRFT 306
              READ (TAPE, *, MAGPO) ;
              READ (TAPE, *, MAGC) ;
              READ (TAPE, *, MAGPH) ;
         END REPLACE ;                                                  SRFT 307
COMMENT       THE FOLLOWING BLOCK TRUNCATES THE ORTHOGONAL COEFFICIENTS SRFT 308
              BY OVERLAYING AND THEN THE ERROR IS RECOMPUTED.  THIS IS  SRFT 309
              DONE ACCORDING TO NEW VALUES OF P[M] THAT ARE INPUT. ;    SRFT 310
TRUNCATE :                                                              SRFT 311
         BEGIN                                                          SRFT 312
LABEL         TRUNC, DEGE, REPEAT ;                                     SRFT 313
LIST          NEW (FOR M ~ 1 STEP 1 UNTIL V DO                          SRFT 314
              NEWP[M]) ;                                                SRFT 315
              READ (CARD, CFI, NEW) [THEEND] ;                          SRFT 316
              FOR M ~ 1 STEP 1 UNTIL V DO                               SRFT 317
              DEGREE[M] ~ 0 ;                                           SRFT 318
              T ~ NEWP[V] + 1 ;          I ~ 1 ;                        SRFT 319
              DEG[V] ~ P[V] + 1 ;                                       SRFT 320
              IF V = 1 THEN GO TO REPEAT ;                              SRFT 321
              FOR M ~ V-1 STEP -1 UNTIL 1 DO                            SRFT 322
              DEG[M] ~ DEG[M+1] | (P[M] + 1) ;                          SRFT 323
COMMENT       COMPUTE THE NUMBER OF UNWANTED COEFFICIENTS TO BE JUMPED  SRFT 324
              OVER ;                                                    SRFT 325
TRUNC :       J ~ 0 ;                                                   SRFT 326
              FOR M ~ 1 STEP 1 UNTIL V-1 DO                             SRFT 327
              J ~ DEG[M+1] | DEGREE[M] + J ;                            SRFT 328
              FOR D ~ 1 STEP 1 UNTIL T DO                               SRFT 329
         BEGIN                                                          SRFT 330
              T1 ~ J + D ;                                              SRFT 331
              COEFFICIENTS[I] ~ COEFFICIENTS[T1] ;                      SRFT 332
              I ~ I + 1                                                 SRFT 333
         END ;                                                          SRFT 334
              M ~ V - 1 ;                                               SRFT 335
DEGE :        DEGREE[M] ~ DEGREE[M] + 1 ;                               SRFT 336
              IF DEGREE[M] ! NEWP[M] + 1 THEN GO TO TRUNC ;             SRFT 337
              IF M = 1 THEN                                             SRFT 338
         BEGIN                                                          SRFT 339
REPEAT :      FOR M ~ 1 STEP 1 UNTIL V DO                               SRFT 340
              P[M] ~ NEWP[M] ;                                          SRFT 341
              GO TO ONE                                                 SRFT 342
         END ;                                                          SRFT 343
              DEGREE[M] ~ 0 ;          M ~ M - 1 ;          GO TO DEGE  SRFT 344
         END TRUNCATE ;                                                 SRFT 345
THEEND : END                                                            SRFT 346
         END                                                            SRFT 347
         END ;                                                          SRFT 349
         END .
         9         6         3       567         0         0    INITIAL DATA
      1111         9         6                                       VARIABLE 1
      2222         7         6                                       VARIABLE 2
      3333         9         6                                       VARIABLE 3
   0.1          ,  3   0.2          ,  3   0.3          ,  3   0.4          ,  3
   0.5          ,  3   0.75         ,  3   1.0          ,  3   1.6          ,  3
   2.0          ,  3                             VARIABLE 1
   0.16         ,  2   0.3          ,  2   0.4          ,  2   0.5          ,  2
   0.6          ,  2   0.8          ,  2   1.0          ,  2         VARIABLE 2
   0.0          ,      0.5          ,  2   1.0          ,  2   1.5          ,  2
   2.0          ,  2   3.0          ,  2   4.0          ,  2   5.0          ,  2
   6.0          ,  2                             VARIABLE 3
    .48000000000, 04    .52000000000, 04    .56300000000, 04    .61000000000, 04
    .65600000000, 04    .75500000000, 04    .86100000000, 04    .97600000000, 04
    .10960000000, 05    .84900000000, 04    .91500000000, 04    .98400000000, 04
    .10530000000, 05    .11250000000, 05    .12780000000, 05    .14550000000, 05
    .16470000000, 05    .18570000000, 05    .10800000000, 05    .11640000000, 05
    .12560000000, 05    .13440000000, 05    .14360000000, 05    .16280000000, 05
    .18560000000, 05    .21000000000, 05    .23640000000, 05    .13150000000, 05
    .14150000000, 05    .15200000000, 05    .16250000000, 05    .17400000000, 05
    .19850000000, 05    .22500000000, 05    .25500000000, 05    .28700000000, 05
    .15600000000, 05    .16740000000, 05    .18000000000, 05    .19200000000, 05
    .20520000000, 05    .23400000000, 05    .26520000000, 05    .29940000000, 05
    .33660000000, 05    .20000000000, 05    .21600000000, 05    .23200000000, 05
    .24880000000, 05    .26620000000, 05    .30080000000, 05    .34160000000, 05
    .38480000000, 05    .43760000000, 05    .24500000000, 05    .26500000000, 05
    .28500000000, 05    .30700000000, 05    .32900000000, 05    .37500000000, 05
    .42700000000, 05    .48000000000, 05    .53900000000, 05    .47200000000, 04
    .51500000000, 04    .56000000000, 04    .60600000000, 04    .65300000000, 04
    .75200000000, 04    .85900000000, 04    .96800000000, 04    .10930000000, 05
    .82900000000, 04    .89700000000, 04    .96600000000, 04    .10410000000, 05
    .11160000000, 05    .12780000000, 05    .14550000000, 05    .16410000000, 05
    .18450000000, 05    .10560000000, 05    .11400000000, 05    .12360000000, 05
    .13240000000, 05    .14200000000, 05    .16240000000, 05    .18480000000, 05
    .20840000000, 05    .23480000000, 05    .12900000000, 05    .13850000000, 05
    .14900000000, 05    .15950000000, 05    .17050000000, 05    .19600000000, 05
    .22250000000, 05    .25250000000, 05    .28500000000, 05    .15180000000, 05
    .16280000000, 05    .17460000000, 05    .18780000000, 05    .20100000000, 05
    .23040000000, 05    .26160000000, 05    .29700000000, 05    .33600000000, 05
    .19760000000, 05    .21200000000, 05    .22560000000, 05    .24160000000, 05
    .25920000000, 05    .29820000000, 05    .33920000000, 05    .38480000000, 05
    .43520000000, 05    .24000000000, 05    .25700000000, 05    .27610000000, 05
    .29500000000, 05    .31600000000, 05    .36100000000, 05    .41360000000, 05
    .47300000000, 05    .53400000000, 05    .46400000000, 04    .50700000000, 04
    .55200000000, 04    .59800000000, 04    .64600000000, 04    .74600000000, 04
    .85000000000, 04    .96200000000, 04    .10880000000, 05    .81000000000, 04
    .88200000000, 04    .95200000000, 04    .10320000000, 05    .11100000000, 05
    .12720000000, 05    .14430000000, 05    .16350000000, 05    .18450000000, 05
    .10250000000, 05    .11200000000, 05    .12120000000, 05    .13000000000, 05
    .14000000000, 05    .16080000000, 05    .18400000000, 05    .20800000000, 05
    .23400000000, 05    .12380000000, 05    .13500000000, 05    .14500000000, 05
    .15550000000, 05    .16750000000, 05    .19300000000, 05    .22200000000, 05
    .25100000000, 05    .28500000000, 05    .14420000000, 05    .15520000000, 05
    .16800000000, 05    .18000000000, 05    .19380000000, 05    .22380000000, 05
    .25800000000, 05    .29400000000, 05    .33420000000, 05    .18400000000, 05
    .20000000000, 05    .21390000000, 05    .23050000000, 05    .24930000000, 05
    .29050000000, 05    .33200000000, 05    .38000000000, 05    .43120000000, 05
    .22260000000, 05    .24200000000, 05    .26110000000, 05    .28100000000, 05
    .30220000000, 05    .35000000000, 05    .40360000000, 05    .46380000000, 05
    .52590000000, 05    .45900000000, 04    .50400000000, 04    .54900000000, 04
    .59500000000, 04    .64200000000, 04    .74200000000, 04    .84800000000, 04
    .96200000000, 04    .10860000000, 05    .77400000000, 04    .85200000000, 04
    .93000000000, 04    .10110000000, 05    .10950000000, 05    .12630000000, 05
    .14400000000, 05    .16350000000, 05    .18420000000, 05    .98400000000, 04
    .10760000000, 05    .11720000000, 05    .12680000000, 05    .13720000000, 05
    .15840000000, 05    .18200000000, 05    .20720000000, 05    .23400000000, 05
    .11750000000, 05    .12900000000, 05    .13950000000, 05    .15050000000, 05
    .16300000000, 05    .19000000000, 05    .21950000000, 05    .25000000000, 05
    .28250000000, 05    .13640000000, 05    .14800000000, 05    .16020000000, 05
    .17400000000, 05    .18900000000, 05    .21900000000, 05    .25380000000, 05
    .29100000000, 05    .33130000000, 05    .17090000000, 05    .18740000000, 05
    .20360000000, 05    .22140000000, 05    .24000000000, 05    .28230000000, 05
    .32480000000, 05    .37380000000, 05    .42560000000, 05    .20570000000, 05
    .22690000000, 05    .24730000000, 05    .26830000000, 05    .29060000000, 05
    .34010000000, 05    .39500000000, 05    .45390000000, 05    .51800000000, 05
    .44800000000, 04    .49600000000, 04    .54100000000, 04    .59000000000, 04
    .63800000000, 04    .73900000000, 04    .84300000000, 04    .96000000000, 04
    .10850000000, 05    .75900000000, 04    .83700000000, 04    .91800000000, 04
    .10020000000, 05    .10860000000, 05    .12600000000, 05    .14400000000, 05
    .16260000000, 05    .18300000000, 05    .96800000000, 04    .10520000000, 05
    .11480000000, 05    .12480000000, 05    .13600000000, 05    .15800000000, 05
    .18160000000, 05    .20640000000, 05    .23360000000, 05    .11500000000, 05
    .12600000000, 05    .13750000000, 05    .14900000000, 05    .16100000000, 05
    .18750000000, 05    .21600000000, 05    .24800000000, 05    .28200000000, 05
    .13140000000, 05    .14400000000, 05    .15660000000, 05    .17100000000, 05
    .18600000000, 05    .21600000000, 05    .25020000000, 05    .28800000000, 05
    .32840000000, 05    .16420000000, 05    .17980000000, 05    .19700000000, 05
    .21600000000, 05    .23480000000, 05    .27520000000, 05    .31890000000, 05
    .36690000000, 05    .41920000000, 05    .19370000000, 05    .21540000000, 05
    .23710000000, 05    .25920000000, 05    .28230000000, 05    .33260000000, 05
    .38800000000, 05    .44500000000, 05    .50800000000, 05    .44000000000, 04
    .48600000000, 04    .53300000000, 04    .57900000000, 04    .62600000000, 04
    .72800000000, 04    .84200000000, 04    .96000000000, 04    .10800000000, 05
    .75000000000, 04    .82500000000, 04    .90600000000, 04    .98700000000, 04
    .10680000000, 05    .12330000000, 05    .14160000000, 05    .16110000000, 05
    .18210000000, 05    .95600000000, 04    .10400000000, 05    .11360000000, 05
    .12320000000, 05    .13360000000, 05    .15440000000, 05    .17800000000, 05
    .20280000000, 05    .23000000000, 05    .11200000000, 05    .12250000000, 05
    .13450000000, 05    .14650000000, 05    .15850000000, 05    .18400000000, 05
    .21200000000, 05    .24250000000, 05    .27700000000, 05    .12600000000, 05
    .14100000000, 05    .15300000000, 05    .16620000000, 05    .18060000000, 05
    .21120000000, 05    .24300000000, 05    .27900000000, 05    .32100000000, 05
    .16100000000, 05    .17360000000, 05    .18980000000, 05    .20780000000, 05
    .22660000000, 05    .26540000000, 05    .30720000000, 05    .35200000000, 05
    .40480000000, 05    .18100000000, 05    .20000000 00, 05    .22200000000, 05
    .24610000000, 05    .27000000000, 05    .31700000000, 05    .37000000000, 05
    .42600000000, 05    .49000000000, 05    .42700000000, 04    .47000000000, 04
    .51700000000, 04    .56600000000, 04    .61400000000, 04    .71800000000, 04
    .83200000000, 04    .95000000000, 04    .10770000000, 05    .72000000000, 04
    .79800000000, 04    .87900000000, 04    .96000000000, 04    .10440000000, 05
    .12150000000, 05    .13980000000, 05    .15960000000, 05    .18120000000, 05
    .90000000000, 04    .10000000000, 05    .10960000000, 05    .11960000000, 05
    .13000000000, 05    .15160000000, 05    .17480000000, 05    .20000000000, 05
    .22800000000, 05    .10600000000, 05    .11750000000, 05    .12900000000, 05
    .14100000000, 05    .15350000000, 05    .18000000000, 05    .20800000000, 05
    .23900000000, 05    .27250000000, 05    .11640000000, 05    .13020000000, 05
    .14400000000, 05    .15840000000, 05    .17280000000, 05    .20580000000, 05
    .24000000000, 05    .27480000000, 05    .31500000000, 05    .14000000000, 05
    .15760000000, 05    .17600000000, 05    .19380000000, 05    .21440000000, 05
    .25600000000, 05    .30000000000, 05    .34720000000, 05    .40000000000, 05
    .16300000000, 05    .18300000000, 05    .20410000000, 05    .22710000000, 05
    .25300000000, 05    .30500000000, 05    .36000000000, 05    .42000000000, 05
    .48200000000, 05    .41100000000, 04    .45600000000, 04    .50200000000, 04
    .55200000000, 04    .60300000000, 04    .71200000000, 04    .82400000000, 04
    .93800000000, 04    .10640000000, 05    .66900000000, 04    .75000000000, 04
    .84000000000, 04    .92400000000, 04    .10140000000, 05    .11940000000, 05
    .13800000000, 05    .15720000000, 05    .17820000000, 05    .83600000000, 04
    .94000000000, 04    .10400000000, 05    .11480000000, 05    .12640000000, 05
    .15000000000, 05    .17360000000, 05    .19800000000, 05    .22400000000, 05
    .10000000000, 05    .11150000000, 05    .12300000000, 05    .13650000000, 05
    .14900000000, 05    .17550000000, 05    .20450000000, 05    .23500000000, 05
    .26850000000, 05    .11160000000, 05    .12240000000, 05    .13800000000, 05
    .15240000000, 05    .16680000000, 05    .19920000000, 05    .23340000000, 05
    .27000000000, 05    .30900000000, 05    .13520000000, 05    .15200000000, 05
    .17030000000, 05    .18800000000, 05    .20800000000, 05    .24800000000, 05
    .29040000000, 05    .33680000000, 05    .38800000000, 05    .15700000000, 05
    .17370000000, 05    .19800000000, 05    .22390000000, 05    .24790000000, 05
    .29280000000, 05    .34370000000, 05    .40400000000, 05    .46600000000, 05
    .40000000000, 04    .44500000000, 04    .49100000000, 04    .54200000000, 04
    .59200000000, 04    .70200000000, 04    .81600000000, 04    .93600000000, 04
    .10620000000, 05    .65100000000, 04    .72000000000, 04    .79800000000, 04
    .88200000000, 04    .96900000000, 04    .11460000000, 05    .13350000000, 05
    .15450000000, 05    .17580000000, 05    .76000000000, 04    .85600000000, 04
    .95600000000, 04    .10640000000, 05    .11760000000, 05    .14080000000, 05
    .16560000000, 05    .19080000000, 05    .21880000000, 05    .85500000000, 04
    .97000000000, 04    .10850000000, 05    .12150000000, 05    .13500000000, 05
    .16250000000, 05    .19250000000, 05    .22500000000, 05    .26050000000, 05
    .93000000000, 04    .10680000000, 05    .12120000000, 05    .13500000000, 05
    .15060000000, 05    .18420000000, 05    .21900000000, 05    .25800000000, 05
    .30180000000, 05    .10800000000, 05    .12800000000, 05    .14640000000, 05
    .16560000000, 05    .18670000000, 05    .22720000000, 05    .27200000000, 05
    .32320000000, 05    .38080000000, 05    .12400000000, 05    .14500000000, 05
    .16860000000, 05    .19190000000, 05    .21600000000, 05    .26800000000, 05
    .32500000000, 05    .38900000000, 05    .46100000000, 05
