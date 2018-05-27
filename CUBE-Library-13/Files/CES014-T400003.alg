BEGIN
COMMENT       A B-5000 PROGRAM FOR THE DESIGN OF REINFORCED CONCRETE
              TIED COLUMNS.

              VICTOR BERMAN                                                 0003
              (PROFESSIONAL SERVICES, BURROUGHS CORP.)                      0004

              CARD SEQUENCE CODE STARTS WITH CCLM-0001                      0005
              FIRST RELEASE DATE JUME 29, 1964. ;

REAL          ABAR, ABAR1, ABAR2, AGSZ, AL, AS, ART, BC, BL, B1, BY, BD,    0007
              COH, COH1, COV, COVH, COVV, CR, CTR, CTR1, D, DMAX, DC,       0008
              DEPT,  DIAM, DIAMR, DIAM1, DIAM2, DIFMX, DIFMN, DIFT,         0009
              DDL, DIFX, DIFY, DL, D1, D2, D3, E, EP, EP1, EP2, FIN, FS,    0010
              FX, FY, INC, INTX, INTY,                                      0011
              INCRH, INCRV, K, KTE, KTT, KT1, KT2, KU, K1, K2, K3, KT,      0012
              LD, LENTH, LIMP, LL, LLL, MAXD, MAXW, MIND, MINW, MP, M,      0013
              MU1, MU2, M1DL, M1LL, M1W, M2DL, M2LL, M2W, P, PU, PUL,       0014
              RECES, REQB, ROOT, SC, SFC, SFT, SFXC, SFXT, SFYC, SFYT,      0015
              SPA1, SPLIC, SS, SSB, SSC, TU, TC, TIE, TISP, UD, SU, SPA,    0016
              TRMX, TRMY, WIDT, WIND, WINDL ;                               0017

INTEGER       G, H, I, II, J, JJ, KK, LIMN, LIMN1, LIMN2, LSBD, LSBD1,      0018
              LSBD2, NBAR, NBAR1, NC, NCOLS, NCON, NLDC, NM, NM1, NUM,      0019
              SBD, SBD1, SBD2, TNBAR ;                                      0020

ALPHA         WD, WIDTH, DEPTH ;                                            0021

BOOLEAN       FXD, LIMIT, SND, TMP, TRY ;                                   0022

REAL ARRAY    X, Y[0:40] ;                                                  0023

LABEL         L51, EXIT ;                                                   0024

FILE          FILE1 (1, 10),
              FILE2 1(2, 15) ;

FORMAT        FORC(I5, 6F10.1),
              FORD(7F10.1, L5),                                             0028
              FORE(6F10.1),                                                 0029
              FORF(7F10.1),                                                 0030
              CHGD("THE ", A5, " OF THE COLUMN MUST BE INCREASED TO ",      0031
              "PLACE ALL THE BARS"/ "FINAL COLUMN ", A5,  " = ", F5.1,      0032
              " INCHES"/),                                                  0033
              FOR5("THE SECTION OF COLUMN",  I3, " HAS THE FOLLOWING ",     0035
              "DIMENSIONS "//  "WIDTH = ", F5.1, " INCHES"//"DEPTH = ",     0036
              F5.1, " INCHES"//"USE", I3, " BARS IN EACH FACE, OF THE ",    0037
              "COLUMN PARALLEL TO PRINCIPAL AXIS OF BENDING"//              0038
              "STANDARD BAR DESIGNATION NUMBER", I3, ", ", F5.2,            0039
              " INCHES IN DIAMETER"//"SPACED", F5.1, " INCHES CENTER",      0040
              " TO CENTER"//"CENTER OF EXTERIOR BARS IS AT", F5.1,          0041
              " INCHES FROM COLUMN FACES"//"CONCRETE COVER IS", F5.1,       0042
              " INCHES"//"USE TIES, STANDARD BAR DESIGNATION NUMBER",       0043
              I3, ", ", F5.2, " INCHES IN DIAMETER"//"SPACED", F5.1,        0044
              " INCHES CENTER TO CENTER"/),                                 0045
              FOR6("USE", I3, " BARS, IN EACH FACE OF THE COLUMN ",         0047
              "PERPENDICULAR TO PRINCIPAL AXIS OF BENDING"//"STANDARD ",    0048
              "BAR DESIGNATION NUMBER", I3, ", ", F5.2, " INCHES IN ",      0049
              "DIAMETER"//"SPACED", F5.1, " INCHES CENTER TO CENTER"//      0050
              "CENTER OF EXTERIOR BARS IS AT", F5.1, " INCHES FROM ",       0051
              "COLUMN FACES"//"CONCRETE COVER IS", F5.1, " INCHES"/),       0052
              DEFST("DISREGARD THIS DESIGN, THE AREA OF STEEL IS", F5.1,    0053
              " PERCENT OF THE SECTION AREA"/ "THE AREA OF STEEL ",         0054
              "SHOULD BE WITHIN 1 AND 8 PERCENT OF THE SECTION AREA"),      0055
              FOR7("BAR SPLICES SHOULD BE", I3, " INCHES LONG"/),
              ULDF("THE COLUMN CAN CARRY AN ULTIMATE LOAD OF ",
              I4, " KIPS") ;

LIST          DATAC(NUM, LL, WIND, DL, M1LL, M1W, M1DL),                    0059
              DATAD(M2LL, M2W, M2DL, LENTH, SC, SS, AGSZ, LIMIT),           0060
              DATAE(MINW, MAXW, MIND, MAXD, COVH, COVV),                    0061
              DATAF(LIMN1, LSBD1, LIMN2, LSBD2, LIMP, INCRH, INCRV),        0062
              CHND(WD, WD, TC),                                             0063
              CHNB(WD, WD, BC),                                             0064
              OUT5(NUM, BC, TC, NBAR1, SBD1, DIAM1, SPA1, COH1, CTR1,       0066
              SBD2, DIAM2, TISP),                                           0067
              OUT6(NBAR, SBD, DIAM, SPA, COH, CTR),                         0068
              OUT8(WD,BC),                                                  0070
              OUT9(WD, B1) ;

PROCEDURE     BDIAM ;                                                       0074
         BEGIN                                                              0075

COMMENT       THIS PROCEDURE CALCULATES THE DIAMETER OF BARS,               0076
              GIVEN THE STANDARD BAR DESIGNATION NUMBER. ;                  0077
              DIAM ~ 0.125 | SBD ;                                          0078
              IF SBD > 8 THEN                                               0079
         BEGIN                                                              0080
              IF SBD = 9 THEN DIAM ~ DIAM + 0.005                           0081
              ELSE IF SBD = 10 THEN DIAM ~ DIAM + 0.02                      0082
              ELSE DIAM ~ DIAM + 0.035                                      0083
         END ;                                                              0084
         END BDIAM ;                                                        0085

PROCEDURE     ULTLD ;                                                       0086
         BEGIN                                                              0087

COMMENT       THE ULTIMATE LOADS AND MOMENTS ARE COMPUTED BY MULTIPLYING    0088
              THE ACTUAL LOADS AND MOMENTS BY THE LOAD FACTORS GIVEN IN     0089
              THE CODE (A.C.I - A604).  THE MAXIMUM VALUE OBTAINED FROM     0090
              THESE EQUATIONS FOR EACH COMBINATION OF LIVE, WIND AND        0091
              DEAD LOADS IS THE ULTIMATE LOAD OR MOMENT. ;                  0092
              UD  ~ 1.2 | DDL + 2.4 | LLL + 0.6 | WINDL ;                   0093
              KTE ~ 1.2 | DDL + 0.6 | LLL + 2.4 | WINDL ;                   0094
              IF KTE > UD THEN UD ~ KTE ;                                   0095
              KTE ~ K | (DDL + LLL + 0.5 | WINDL) ;                         0096
              IF KTE > UD THEN UD ~ KTE ;                                   0097
              KTE ~ K | (DDL + 0.5 | LLL + WINDL) ;                         0098
              IF KTE > UD THEN UD ~ KTE                                     0099
         END ULTLD ;                                                        0100

PROCEDURE     BALND ;                                                       0101
         BEGIN                                                              0102
REAL          AG ;                                                          0103

COMMENT       DESIGN A BALANCED SQUARE COLUMN, ASSUME THAT THE DISTANCE     0104
              FROM THE CENTER OF THE BARS TO THE EDGE OF THE COLUMN IS      0105
              2.7 INCHES  (A.C.I. - A609(B), EQUATION (A8)). ;              0106
              K1 ~ 0.85 ;                                                   0107
              IF SC > 5.0 THEN K1 ~ K1 - 0.5 | (SC - 5.0) ;                 0108
              K2 ~ 0.5 | K1 ;    BD ~ PU / (0.85 | K1 | KU | SC) ;          0109

COMMENT       THE CODE FORBIDS THAT THE COLUMN SECTION SHOULD BE SMALLER    0110
              THAN THE AREA REQUIRED TO CARRY THE AXIAL LOAD WITH           0111
              4 PERCENT OF STEEL. ;                                         0112
              AG ~ (LL + WIND + DL) / (0.18 | SC + 0.0128 | SS) ;           0113
              IF AG > BD THEN BD ~ AG ;                                     0114

COMMENT      A.C.I.-1101 - THE AREA OF CONCRETE CANNOT BE LESS THAN         0115
              120 SQUARE INCHES. ;                                          0116
              IF BD < 120.0 THEN BD ~ 120.0 ;                               0117
              IF (((MINW = 0) AND (MIND = 0)) OR                            0118
              ((MINW !0) AND (MIND ! 0))) THEN                              0119
         BEGIN                                                              0120
              ROOT ~ SQRT(ABS(7.3 + 4 | BD)) ;                              0121
              BC ~ TC ~ 0.5 | (2.7 + ROOT) ;     DC ~ TC - 2.7              0122
         END                                                                0123
              ELSE IF (MINW ! 0) AND (MIND = 0) THEN                        0124
         BEGIN                                                              0125
              BC ~ MINW ;     DC ~ BD / BC                                  0126
         END                                                                0127
              ELSE                                                          0128
         BEGIN                                                              0129
              DC ~ MIND - 2.7 ;     BC ~ BD / DC                            0130
         END ;                                                              0131
              TC ~ DC + 2.7 ;                                               0132
              IF  (MINW = MAXW) AND (MIND = MAXD) AND                       0133
              (MAXD ! 0) AND (MAXW ! 0) THEN FXD ~ TRUE ;                   0134
         END BALND ;                                                        0135

PROCEDURE     COMP ;                                                        0136
         BEGIN                                                              0137
REAL          TERMA, TERMB ;                                                0138

COMMENT       CALCULATION OF AREA OF REINFORCEMENT WHEN CONCRETE            0139
              SECTION CONTROLS ( EQUATION (A11) OF THE A.C.I. CODE) ;       0140
              TERMB ~ ((3 | D2 | EP) / ((D3)*2)) + 1.18 ;                   0141
              TERMB ~ PU - (D1 | D2 | SC / TERMB) ;                         0142
              TERMA ~ (EP / (2 | D3 - D2)) + 0.5 ;                          0143
              AS ~ TERMA | TERMB / SS                                       0144
         END COMP ;                                                         0145

PROCEDURE     BRSZ  ;                                                       0146
         BEGIN                                                              0147
REAL          DIAMR ;                                                       0148
LABEL         L52, L52A ;                                                   0149

COMMENT       THIS PROCEDURE CALCULATES THE BAR SIZE NECESSARY TO OBTAIN    0150
              THE AREA OF STEEL FOR A GIVEN NUMBER OF BARS. ;               0151
              IF LSBD ! 0 THEN                                              0152
         BEGIN                                                              0153
              SBD ~ LSBD ;     GO TO L52A                                   0154
         END ;                                                              0155
              IF AS { 0 THEN                                                0156
         BEGIN                                                              0157
              SBD ~ 4 ;     GO TO L52                                       0158
         END ;                                                              0159
              DIAMR ~ SQRT(AS / (0.785 | NBAR)) ;                           0160
              FOR SBD ~ SBD STEP -1 WHILE DIAM > DIAMR DO                   0161
         BEGIN                                                              0162

COMMENT       A.C.I. - 1104 - A - THE MINIMUM BAR SIZE TO BE USED           0163
              IN COLUMNS IS NUMBER 5 BARS. ;                                0164
              IF SBD < 5 THEN GO TO L52 ;                                   0165
              BDIAM                                                         0167
         END ;                                                              0168
L52:          SBD ~ SBD + 1 ;                                               0169
L52A:         BDIAM ;                                                       0170
              REQB ~ 2.0 | NBAR | DIAM + 1.5 | AGSZ | (NBAR - 1) +          0171
              2.0 | COV + SU                                                0172
         END BRSZ ;                                                         0173

PROCEDURE     TENS ;                                                        0174
         BEGIN                                                              0175
REAL          A, Z ;                                                        0176

COMMENT       CALCULATION OF AREA OF STEEL WHEN IT CONTROLS THE DESIGN ;    0177
              A ~ PU / (0.85 | SC | D1) ;                                   0178
              Z ~ EP - 0.5 | (D2 - A) ;                                     0179
              AS ~ PU | Z / ((2 | D3 - D2) | SS)                            0180
         END TENS ;                                                         0181

PROCEDURE     REINF ;                                                       0182
         BEGIN                                                              0183

COMMENT       THIS PROCEDURE FINDS THE NUMBER OF BARS OF A GIVEN SIZE       0184
              NEEDED TO GET THE NECESSARY AREA OF STEEL AND THE             0185
              COLUMN WIDTH NECESSARY TO ACCOMMODATE THEM. ;                 0186
              BDIAM ;                                                       0187
              NBAR ~ ENTIER (AS / (0.785 | (DIAM*2))) + 1 ;                 0188
              IF NBAR < 2 THEN NBAR ~ 2 ;                                   0189
              REQB ~ 2 | NBAR | DIAM + 1.5 | AGSZ | (NBAR - 1) +            0190
              2 | COV + SU                                                  0191
         END REINF ;                                                        0192

PROCEDURE     NBR ;                                                         0193
         BEGIN                                                              0194

COMMENT       THIS PROCEDURE SHIFTS CONTROL TO THE PROCEDURE THAT           0195
              CALCULATES THE NUMBER OF BARS THAT GIVE THE REQUIRED AREA     0196
              OF STEEL.  SHOULD THE NUMBER OF BARS BE FIXED, IT WILL        0197
              FIND THE REQUIRED BAR SIZE. ;                                 0198
              IF LSBD = 0 THEN SBD ~ 11                                     0199
              ELSE SBD ~ LSBD ;                                             0200
              BDIAM ;                                                       0201
              IF COV ! 0 THEN COV ~ COV                                     0202
              ELSE IF SBD < 6 THEN COV ~ 1.5                                0203
              ELSE COV ~ 2.0 ;                                              0204
              IF LIMN ! 0 THEN                                              0205
         BEGIN                                                              0206
              NBAR ~ LIMN ;                                                 0207
              BRSZ                                                          0208
         END                                                                0209
              ELSE REINF                                                    0210
         END NBR ;                                                          0211

PROCEDURE     SPACN ;                                                       0212
         BEGIN                                                              0213

REAL          KT, SPA3, SPA4 ;                                              0214
COMMENT       A.C.I. - 1103 - B - CENTER TO CENTER SPACING OF BARS SHALL    0215
              NOT BE LESS THAN 2 1/2 THE DIAMETER OF ROUND BARS.            0216
              THE CLEAR SPACING BETWEEN PAIRS OF BARS AT LAPPED SPLICES     0217
              SHALL NOT BE LESS THAN 1 1/2 INCHES OR 1 1/2 TIMES THE        0218
              MAXIMUM SIZE OF COARSE AGGREGATE. ;                           0219
              B1 ~ 0 ;                                                      0220
              KT ~ KT1 ~ 2.5 | DIAM ;     KT2 ~ 1.5 | AGSZ + 2 | DIAM ;     0221
              IF KT2 > KT THEN KT ~ KT2 ;                                   0222
              CTR ~ COV + 0.5 | DIAM ;                                      0223
              IF SND THEN COV ~ 1.5 | AGSZ ;                                0224
              SPA ~ SPA3~ (D1 - 2 | COV - DIAM - SU) / (NBAR - 1) ;         0225
              SPA4 ~ ((D1 - 2 | (COV + NBAR | DIAM) - SU) /                 0226
              (NBAR - 1)) + 2 | DIAM ;                                      0227
              IF SPA4 < SPA THEN SPA ~ SPA4 ;
              IF SPA < KT THEN
         BEGIN                                                              0230
              IF SND THEN INC ~ INCRV ELSE INC ~ INCRH ;
              IF KT = KT1 THEN                                              0233
              B1 ~ INC | (ENTIER((2 | COV + DIAM + (NBAR - 1) | KT1 +       0234
              SU) / INC) + 1.0)                                             0235
              ELSE                                                          0236
              B1 ~ INC | (ENTIER((2 | (COV + DIAM) + (NBAR - 1) | KT2 +     0237
              SU) / INC) + 1.0)  ;                                          0238
         END                                                                0239
         END SPACN ;                                                        0240

PROCEDURE     CORT ;                                                        0241
         BEGIN                                                              0242
LABEL         L53, L54 ;                                                    0243

COMMENT       THIS PROCEDURE INCREASE OR DECREASES THE WIDTH AND DEPTH      0244
              OF THE COLUMN WHEN NECESSARY. ;                               0245
              IF (BC { MINW) AND (MINW!0) AND (INC < 0) THEN GO TO L53;     0246
              IF (BC } MAXW) AND (MAXW!0) AND (INC > 0) THEN GO TO L53 ;    0247
              BC ~ BC + INC ;                                               0248
L53:          IF (TC { MIND) AND (MIND !0) AND (INC < 0) THEN GO TO L54;    0249
              IF (TC } MAXD) AND (MAXD !0) AND (INC > 0) THEN GO TO L54;    0250
              DC ~ DC + INC ;     TC ~ TC + INC ;                           0251
L54:     END CORT ;                                                         0252

PROCEDURE     DIM1 ;                                                        0253
         BEGIN                                                              0254
COMMENT       THE MAIN DIMENSION, D1, IS THE WIDTH. ;                       0255

              D1 ~ BC ;     D2 ~ TC ;     D3 ~ TC - CTR1                    0256
         END DIM1 ;                                                         0257
PROCEDURE     DIM2 ;                                                        0258

         BEGIN                                                              0259

COMMENT       THE MAIN DIMENSION, D1, IS THE DEPTH. ;                       0260
              D1 ~ TC ;     D2 ~ BC ;     D3 ~ BC - CTR                     0261
         END DIM2 ;                                                         0262
PROCEDURE     FRBR ;                                                        0263

         BEGIN                                                              0264
REAL          YT ;                                                          0265

COMMENT       THIS PROCEDURE CALCULATES THE FORCE THAT EACH BAR EXERTS.     0266
              IT ACCUMULATES THE SUM OF TENSILE AND COMPRESSIVE             0267
              FORCES AND THE MOMENTS PRODUCED BY THEM. ;                    0268
              INTX ~ (Y[I] - M | X[I] - BY) / (MP - M) ;                    0269
              INTY ~ MP | INTX + BY ;                                       0270
              D ~ SQRT((X[I] - INTX)*2 + (Y[I] - INTY)*2) ;                 0271

COMMENT       FIND IF THE BAR IS IN TENSION OR COMPRESSION ZONE. ;          0272
              YT ~ -X[I] | MP + Y[I] ;     FS ~ FIN | D  ;                  0273
              IF FS > SS THEN FS ~ SS ;                                     0274
              FS ~ FS | ABAR ;                                              0275
              FX ~ FS | (BC - X[I]) ;     FY ~ FS | (TC - Y[I]) ;           0276
              IF YT < BY THEN                                               0277
         BEGIN                                                              0278
              SFT ~ SFT + FS ;                                              0279
              SFXT ~ SFXT + FX ;     SFYT ~ SFYT + FY                       0280
         END                                                                0281
              ELSE                                                          0282
         BEGIN                                                              0283
              SFC ~ SFC + FS ;                                              0284
              SFXC ~ SFXC + FX ;     SFYC ~ SFYC + FY                       0285
         END                                                                0286
         END FRBR ;                                                         0287

PROCEDURE     ULCLD ;                                                       0288
         BEGIN                                                              0289
REAL          AR, BR, XX, AA, BB ;                                          0290

COMMENT       THIS PROCEDURE CHECKS IF THERE IS MOMENT EQUILIBRIUM          0291
              AND FINDS THE ULTIMATE LOAD THAT THE COLUMN CAN CARRY. ;      0292
              SFT ~ SFC ~ SFXT ~ SFXC ~ SFYT ~ SFYC ~ 0 ;                   0293
              AR ~ BC - AL ;     BR ~ TC - BL ;     MP ~ -BL / AL ;         0294
              M ~ -1.0 / MP ;     BY ~ BR - MP | BC ;                       0295
              INTX ~ (M | BC - TC + BY) / (M - MP)  ;                       0296
              INTY ~ MP | INTX + BY ;      DMAX ~ 0 ;                       0297

COMMENT       THE DISTANCE TO THE FARTHEST APART COMPRESSIVE CONCRETE       0298
              FIBER D2, AND TO THE FARTHEST APART BAR DMAX, ARE             0299
              CALCULATED. ;                                                 0300
              D2 ~ SQRT((BC - INTX)*2 + (TC - INTY)*2) ;     DMAX ~ 0 ;     0301
              FOR I ~ 1 STEP 1 UNTIL TNBAR DO                               0302
         BEGIN                                                              0303
              INTX ~ (Y[I] - M | X[I] - BY) / (MP - M) ;                    0304
              INTY ~ MP | INTX + BY ;                                       0305
              D ~ SQRT((INTX - X[I])*2 + (INTY - Y[I])*2) ;                 0306
              IF D > DMAX THEN DMAX ~ D                                     0307
         END ;                                                              0308
              IF (D2 / DMAX) > RECES THEN FIN ~ 90.0 / D2                   0309
              ELSE FIN ~ 60.0 / DMAX ;                                      0310
              ABAR ~ ABAR1 ;                                                0311
              FOR I ~ 1 STEP 1 UNTIL II DO FRBR ;                           0312
              ABAR ~ ABAR2 ;                                                0313
              FOR I ~ II + 1 STEP 1 UNTIL TNBAR DO FRBR ;                   0314
              AA ~ 0.85 | AL ;     BB ~ 0.85 | BL ;                         0315
              ART ~ 0.5 | AA | BB ;                                         0316
              TRMX ~ 0.333 | AA | ART ;     TRMY ~ 0.333 | BB | ART ;       0317
              IF AA > BC THEN                                               0318
         BEGIN                                                              0319
              XX ~ (AA - BC) | BB / AA ;                                    0320
              DIFT ~ 0.5 | (AA - BC) | XX ;     ART ~ ART - DIFT ;          0321
              TRMX ~ TRMX - DIFT | (BC + 0.333 | (AA - BC)) ;               0322
              TRMY ~ TRMY - DIFT | (0.333 | XX)                             0323
         END ;                                                              0324
              IF BB > TC THEN                                               0325
         BEGIN                                                              0326
              XX ~ (BB - TC) | AA / BB ;                                    0327
              DIFT ~ 0.5 | (BB - TC) | XX ;     ART ~ ART - DIFT ;          0328
              TRMX ~ TRMX - DIFT | (0.333 | XX) ;                           0329
              TRMY ~ TRMY - DIFT | (TC + 0.333 | (BB - TC))                 0330
         END ;                                                              0331
              DIFX ~ 0.85 | SC | TRMX - SFXT + SFXC + PU | EP2 ;            0332
              DIFY ~ 0.85 | SC | TRMY - SFYT + SFYC + PU | EP1 ;            0333
              PUL ~ 0.85 | SC | ART + SFC - SFT                             0334
         END ULCLD ;                                                        0335

PROCEDURE     DIFR ;                                                        0336
         BEGIN                                                              0337

COMMENT       THIS PROCEDURE SHIFTS THE NEUTRAL AXIS TRYING TO              0338
              EQUILIBRATE THE MOMENTS PRODUCED BY THE INTERNAL AND          0339
              EXTERNAL FORCES. ;                                            0340
              IF DIFX > DIFMX THEN AL ~ AL - CR                             0341
              ELSE IF DIFX < DIFMN THEN AL ~ AL + CR                        0342
              ELSE AL ~ AL ;                                                0343
              IF ABS(AL) < 0.05 THEN AL ~ AL + 0.2 | CR ;
              IF DIFY > DIFMX THEN BL ~ BL - CR                             0344
              ELSE IF DIFY < DIFMN THEN BL ~ BL + CR                        0345
              ELSE BL ~ BL ;                                                0346
              IF ABS(BL) < 0.05 THEN BL ~ BL + 0.2 | CR ;
              ULCLD                                                         0347
         END  DIFR ;                                                        0348

L51:          READ(FILE1, FORC, DATAC)[EXIT] ;
              MINW ~ MAXW ~ MIND ~ MAXD ~ 0.0 ;
              WIDTH ~ DEPTH ~ INCRH ~ INCRV ~ 0.0 ;
              COVH ~ COVV ~ COV ~ LIMP ~ SU ~ 0 ;
              LIMN1 ~ LSBD1 ~ LIMN2 ~ LSBD2 ~ 0 ;                           0366
              SND ~ TRY ~ FXD ~ FALSE ;                                     0367
              READ(FILE1, FORD, DATAD) ;                                    0350
         BEGIN                                                              0351
REAL          P1, P2, EXC ;                                                 0352
LABEL         L52B, L53B, L53C, L53E, L53G, L54, L54B, L54C, L54E,          0353
              L55, L59, L61, L61A, L62, L63, L64 ;                          0354

COMMENT       A MAXIMUM STEEL STRAIN OF 0.004 IN. PER IN. IS ASSUMED        0355
              WHILE THE MAXIMUM CONCRETE STRAIN IS 0.003 IN. PER IN. ;      0356
              RECES ~ 0.75 ;     K ~ 2.0 ;                                  0357
              IF LIMIT THEN                                                 0360
         BEGIN                                                              0361
              READ(FILE1, FORE, DATAE) ;                                    0362
              READ(FILE1, FORF, DATAF)                                      0363
         END ;                                                              0364
              IF INCRH = 0 THEN INCRH ~ 2.0 ;                               0370
              IF INCRV = 0 THEN INCRV ~ 2.0 ;                               0371
              CTR ~ CTR1 ~ 2.7 ;                                            0372
              KTE ~ CTR1 - COVV - 0.7 ;
              IF KTE < 0 THEN CTR1 ~ CTR1 - KTE ;                           0374
              KTE ~ CTR - COVH - 0.7 ;                                      0375
              IF KTE < 0 THEN CTR ~ CTR - KTE ;                             0376
              DDL ~ DL ;     LLL ~ LL ;     WINDL ~ WIND ;                  0377
              ULTLD ;                                                       0378
              LD ~ PU ~ UD ;                                                0379
              DDL ~ M1DL ;     LLL ~ M1LL ;     WINDL ~ M1W ;               0380
              ULTLD ;                                                       0381
              MU1 ~ UD ;                                                    0382
              DDL ~ M2DL ;     LLL ~ M2LL ;     WINDL ~ M2W ;               0383
              ULTLD ;                                                       0384
              MU2 ~ UD ;                                                    0385
              IF AGSZ < 1.0 THEN AGSZ ~ 1.0 ;                               0386
              KU ~ 90.0 / (90.0 + SS) ;                                     0387
              LIMN ~ LIMN1 ;     LSBD ~ LSBD1 ;                             0388
              BALND ;                                                       0389
              J ~ 0 ;                                                       0390

COMMENT       THE MINIMUM ALLOWABLE ECCENTRICITY FOR TIED COLUMNS IS        0391
              ONE-TENTH OF THE SECTION DEPTH. ;                             0392
              EP2 ~ EP ~ 12 | MU2 / PU ;     KTE ~ 0.1 | BC ;               0393
              IF EP < KTE THEN EP ~ KTE ;                                   0394
              E ~ EP + 0.5 | BC - CTR1 ;                                    0395
              DIM2 ;                                                        0396
              GO TO L53C ;                                                  0397

L53B:         EP1 ~ EP ~ 12 | MU1 / PU ;     KTE ~ 0.1 | TC ;               0398
              IF EP < KTE THEN EP ~ KTE ;                                   0399
              E ~ EP + 0.5 | TC - CTR ;                                     0400
              DIM1 ;                                                        0401
              J ~ 1 ;                                                       0402

L53C:         AS ~ (PU | E - 0.85 | K1 | SC | D1 | ((D3)*2) | KU | (1.0-    0403
              0.5 | K1 | KU)) / ((SS - 0.85 | SC) | (D3 - CTR)) ;           0404
              IF J = 1 THEN GO TO L53E ;                                    0405
              P2 ~ AS / (D1 | D3) ;     GO TO L53B ;                        0406

L53E:         P1 ~ AS / (D1 | D3) ;     P ~ P1 + P2 ;     COV ~ COVV ;      0407

COMMENT       THE TOTAL AREA OF STEEL NEEDED TO RESIST BENDING IS           0408
              CALCULATED.  IF THIS AREA IS MORE THAN 8 PERCENT OF THE       0409
              AREA OF CONCRETE, OR ANY OTHER MAXIMUM FIXED BY THE           0410
              DESIGNER, THE COLUMN MUST BE DESIGNED FOR A TENSILE           0411
              FAILURE.  THE SECTION OF THE COLUMN IS INCREASED UNTIL THE    0412
              AREA OF STEEL IS WITHIN THE ALLOWABLE RATIO. ;                0413
              LIMP ~ 0.01 | LIMP ;                                          0414
              IF LIMP = 0 THEN LIMP ~ 0.08 ;                                0415
              K2 ~ 0.5 | LIMP ;     K1 ~ P1 | K2 / P ;                      0416
              K3 ~ 0.005 | P1 / P ;                                         0417
              IF FXD THEN                                                   0418
         BEGIN                                                              0419
              BC ~ MINW ;     TC ~ MIND ;     DC ~ TC - CTR1 ;              0420
              IF (((BC | DC) > BD) OR (P > K2)) THEN TRY ~ TRUE ;           0421
              GO TO L54E                                                    0422
         END ;                                                              0423
              BC ~ INCRH | (ENTIER(BC / INCRH) + 1.0) ;
              TC ~ INCRV | (ENTIER(TC / INCRV) + 1.0) ;
              DC ~ TC - CTR1 ;                                              0426
              IF P < K2 THEN                                                0427
         BEGIN                                                              0428
              REQB ~ 0 ;

L53G:         IF REQB < BC THEN                                             0430
         BEGIN                                                              0431
              IF (BC { MINW) AND (MINW ! 0) THEN                            0432
         BEGIN                                                              0433
              BC ~ MINW ;     GO TO L54C                                    0434
         END ;                                                              0435
              INC ~ -INCRH ;      CORT ;      DIM1 ;

COMMENT       A.C.I. - A611 - WHEN THE UNSUPPORTED LENGTH OF THE COLUMN     0438
              IS GREATER THAN 15 TIMES THE LEAST LATERAL DIMENSION,         0439
              THE ULTIMATE LOAD ACTING ON THE COLUMN MUST BE INCREASED      0440
              BY THE FACTOR GIVEN IN EQUATION (A14) AND THE SECTION         0441
              COLUMN MUST BE INCREASED. ;                                   0442
              IF (LENTH  / TC) > 1.25 THEN                                  0443
              PU ~ LD / (1.6 - 0.48 | LENTH / TC) ;                         0444
              COMP ;                                                        0445
              P1 ~ AS / (D1 | D3) ;                                         0446
              NBR ;                                                         0447
              IF P1 > K1 THEN GO TO L54 ;                                   0448
              GO TO L53G                                                    0449
         END ;                                                              0450

L54:          INC ~ INCRH ;                                                 0451
              CORT ;                                                        0452
              IF (BC|DC) > BD THEN TRY ~ TRUE                               0453
         END                                                                0454
              ELSE                                                          0455
         BEGIN                                                              0456
              TRY ~ TRUE ;     REQB ~ 1000.0 ;

L54B:         IF REQB > BC THEN                                             0458
         BEGIN                                                              0459
              IF (BC } MAXW) AND (MAXW ! 0) THEN                            0460
         BEGIN                                                              0461
              WD ~ "WIDTH" ;     BC ~ MAXW ;                                0462
              WRITE(FILE2, CHGD, CHND) ;     GO TO L54C                     0463
         END ;                                                              0464
              INC ~ INCRH ;      CORT ;      DIM1 ;
              IF (LENTH / TC) > 1.25 THEN                                   0467
              PU ~ LD / (1.6 - (0.48 | LENTH / TC)) ;                       0468
              TENS ;                                                        0469
              P1 ~ AS / (BC | DC) ;                                         0470
              NBR ;                                                         0471
              IF P1 < K3 THEN                                               0472
         BEGIN                                                              0473
              INC ~ -INCRH ;                                                0474
              CORT ;                                                        0475
              IF (BC | DC) < BD THEN TRY ~ FALSE
         END ;                                                              0477
              GO TO L54B                                                    0478
         END                                                                0479
         END ;                                                              0480

L54C:         IF (BC < MINW) AND (MINW ! 0) THEN BC ~ MINW ;                0481
              IF (BC > MAXW) AND (MAXW ! 0) THEN BC ~ MAXW ;                0482
              IF (TC < MIND) AND (MIND ! 0) THEN TC ~ MIND ;                0483
              IF (TC > MAXD) AND (MAXD ! 0) THEN TC ~ MAXD ;                0484

COMMENT       A.C.I. 1101 - THE MINIMUM ALLOWABLE COLUMN THICKNESS          0485
              IS 8.0 INCHES. ;                                              0486

L54E:         IF BC < 8.0 THEN BC ~ 8.0 ;                                   0487
              IF TC < 8.0 THEN TC ~ 8.0 ;                                   0488
              TMP ~ FALSE ;                                                 0489

L55:          DIM1 ;                                                        0490
              IF (LENTH / BC) > 1.25 THEN                                   0491
              PU ~ LD / (1.6 - (0.48 | LENTH / BC)) ;                       0492
              IF TRY THEN TENS                                              0493
              ELSE COMP ;                                                   0494
              IF MU2 > MU1 THEN SU ~ 2.82 + 3 | AGSZ ;
              NBR ;                                                         0496
              BRSZ ;                                                        0497
              DIAM2 ~ 0.5 ;                                                 0498
              SPACN ;                                                       0499
              IF B1 > BC THEN                                               0500
         BEGIN                                                              0501
              TMP ~ TRUE ;     D1 ~ B1 ~ BC ~ BC + INCRH ;     GO TO L55    0502
         END ;                                                              0503
              IF TMP THEN                                                   0504
         BEGIN                                                              0505
              WD ~"WIDTH" ;     TMP ~ FALSE ;                               0506
              IF (BC > MAXW) AND (MAXW ! 0) THEN                            0507
              WRITE(FILE2, CHGD, CHND)                                      0508
         END ;                                                              0509
              NBAR1 ~ NBAR ;     CTR1 ~ CTR ;                               0510
              DIAM1 ~ DIAM ;     SBD1 ~ SBD ;                               0511
              IF COVV ! 0 THEN CTR1 ~ COVV + 0.5 | DIAM1 ;                  0512

COMMENT       IT IS ASSUMED THAT EACH TIE CONFINES FOUR LONGITUDINAL        0513
              BARS.  THE AREA OF A TIE IS AROUND 2 PERCENT OF THE           0514
              AREA OF THE BARS IT CONFINES. ;                               0515
              ABAR ~ 0 ;     TIE ~ 0.0628 | (DIAM1*2) ;                     0516

COMMENT       A.C.I.-1104 - C - LATERAL TIES SHOULD BE AT LEAST             0517
              1/4 INCHES IN DIAMETER. ;                                     0518
              FOR SBD ~ 2 STEP 1 WHILE ABAR < TIE DO
         BEGIN                                                              0520
              BDIAM ;                                                       0521
              ABAR ~ 0.785 | (DIAM*2)                                       0522
         END  ;                                                             0523

COMMENT       A.C.I. - 1104 - C - LATERAL TIES SHALL BE SPACED NOT OVER     0524
              16 BAR DIAMETERS. 48 TIE DIAMETERS OR, THE LEAST              0525
              DIMENSION OF THE COLUMN. ;                                    0526
              DIAM2 ~ DIAM ;     SBD2 ~ SBD ;                               0527
              TISP ~ 16 | DIAM1 ;     KTE ~ 48 | DIAM2 ;                    0528
              IF KTE < TISP THEN TISP ~ KTE ;
              IF BC < TISP THEN TISP ~ BC ;
              IF TC < TISP THEN TISP ~ TC ;

COMMENT       A.C.I. - 1103 - C - 1 - THE LENGTH OF SPLICES IN VERTICAL     0532
              BARS IS 20 BAR DIAMETERS INCREASED BY ONE FOR EACH 1000       0533
              PSI. THAT THE WORKING STRENGTH OF STEEL EXCEEDS 20000         0534
              PSI.  INCREASE THE LENGTH OF SPLICES BY ONE-THIRD IF          0535
              CONCRETE STRENGTH IS BELOW 3000 PSI. ;                        0536
              EXC ~ SS - 50.0 ;                                             0537
              IF EXC < 0 THEN EXC ~ 0 ;                                     0538
              SPLIC ~ (20.0 + 0.4 | EXC) | DIAM1 ;                          0539
              IF SC < 3.0 THEN SPLIC ~ 1.33 | SPLIC ;                       0540
              SPLIC ~ ENTIER(SPLIC) + 1.0 ;

COMMENT       THE CONCRETE COVER SPECIFIED BY THE DESIGNER IS THE           0542
              COVER FROM THE EDGE OF THE BARS.  HOWEVER, THE COVER OF       0543
              THE TIES SHOULD BE AT LEAST 1.5 INCHES. ;                     0544
              IF COVH ! 0 THEN COH1 ~ COVH + 0.5 | DIAM1                    0545
              ELSE COH1 ~ CTR1 ;                                            0546
              KTE ~ COH1 - 0.5 | DIAM1 - DIAM2 - 1.5 ;                      0547
              IF KTE < 0 THEN COH1 ~ COH1 - KTE ;                           0548
              COH1 ~ COH1 + 0.5 | SU ;
              SPA1 ~ (BC - 2 | COH1) / (NBAR1 - 1) ;                        0550
              EP ~ EP2 ;                                                    0551
              IF EP < (0.1 | BC) THEN EP ~ 0.1 |BC ;                        0552

L59:          DIM2 ;                                                        0553
              SND ~ TRUE;      COV ~ COVH ;                                 0554
              IF TRY THEN TENS                                              0555
              ELSE COMP ;                                                   0556
              IF SU = 0 THEN SU ~ 2 | DIAM1 + 3 | AGSZ                      0557
              ELSE SU ~ 0 ;                                                 0558
              LSBD ~ LSBD2 ;     LIMN ~ LIMN2;                              0559
              NBR;                                                          0560
              BRSZ ;                                                        0561
              SPACN ;                                                       0562
              IF B1 > TC THEN                                               0563
         BEGIN                                                              0564
              TMP ~ TRUE;      D1 ~ B1 ~ TC ~ TC + INCRV ;     GO TO L59    0565
         END ;                                                              0566
              IF TMP THEN                                                   0567
         BEGIN                                                              0568
              WD ~ "DEPTH"  ;  TMP ~ FALSE ;                                0569
              SPACN ;                                                       0570
              WRITE(FILE2, CHGD, CHND)                                      0571
         END ;                                                              0572
              IF COVV ! 0 THEN COH ~ COVV + 0.5 | DIAM                      0573
              ELSE COH ~ CTR ;                                              0574
              COH ~ COH + 0.5 | SU ;                                        0575
              SPA ~ (TC - 2 | COH) / (NBAR - 1) ;                           0576

COMMENT       THE LOAD THAT THE DESIGNED SECTION CAN CARRY AT FAILURE
              IS CALCULATED IN ORDER TO TEST THE SAFETY OF THE DESIGN.
              IF THE LOAD IS LOWER THAN THE ULTIMATE LOAD, THE
              AREA OF STEEL IS INCREASED. ;

L61:          EP1 ~ EP1 - 0.5 | TC ;     EP2 ~ EP2 - 0.5 | BC ;
              AL ~ BC ;     BL ~ TC ;

L61A:         II ~ 2 | NBAR1 ;      TNBAR ~ 2 | (NBAR + NBAR1) ;

COMMENT       FIND THE COORDINATES OF EACH BAR. ;
              FOR I ~ 1 STEP 1 UNTIL NBAR1 DO
         BEGIN
              J ~ NBAR1 + I ;
              X[I] ~ X[J] ~ CTR1 + SPA1 | (I - 1) ;
              Y[I] ~ CTR1 ;     Y[J] ~ TC - CTR1
         END ;
              JJ ~ II + NBAR ;
              FOR I ~ II + 1 STEP 1 UNTIL JJ DO
         BEGIN
              J ~ NBAR + I ;
              X[I] ~ CTR ;     X[J] ~ BC - CTR ;
              Y[I] ~ Y[J] ~ CTR + SPA | (I - II - 1)
         END ;

L62:          ABAR1 ~ 0.785 | DIAM1*2 ;      ABAR2 ~ 0.785 | DIAM*2 ;
              ULCLD ;
              CR ~ 2.0 ;      DIFMX ~ 1000.0 ;      DIFMN ~ -1000.0 ;
              FOR KK ~ 1 STEP 1 UNTIL 6 DO
         BEGIN
              FOR G ~ 1 STEP 1 WHILE
              (ABS(DIFX) > DIFMX) OR (ABS(DIFY) > DIFMX) DO DIFR ;
              DIFMX ~ 0.5 | DIFMX ;     DIFMN ~ 0.5 | DIFMN ;
              CR ~ 0.5 | CR
         END ;
              SND ~ TRUE ;
              IF PUL < PU THEN
         BEGIN
              IF ((SBD1 < 11) AND (LSBD1 = 0)) THEN
         BEGIN
              SBD1 ~ 11 ;     DIAM1 ~ 1.41
         END
              ELSE IF ((SBD < 11) AND (LSBD2 = 0)) THEN
         BEGIN
              SBD ~ 11 ;      DIAM ~ 1.41
         END
              ELSE
         BEGIN
              KT ~ 2.82 + 1.5 | AGSZ ;
              IF SND THEN
         BEGIN
              IF KT > (SPA1 | (NBAR1 - 1) / NBAR1) THEN GO TO L63 ;
              NBAR1 ~ NBAR1 + 1 ;     SND ~ FALSE ;
              SPA1 ~ SPA1 | (NBAR1 - 1) / NBAR1
         END
              ELSE

L63:     BEGIN
              IF (KT > (SPA | (NBAR - 1) / NBAR)) AND SND THEN
         BEGIN
              BC ~ BC + INCRH ;     NBAR ~ NBAR - 1 ;
              TC ~ TC + INCRV ;     NBAR1 ~ NBAR1 - 1 ;     GO TO L54E
         END ;
              NBAR ~ NBAR + 1 ;
              SPA ~ SPA | (NBAR - 1) / NBAR ;     SND ~ TRUE
         END ;
              GO TO L61A
         END ;
              GO TO L62
         END ;

L64:          WRITE(FILE2, FOR5, OUT5) ;                                    0578
              WRITE(FILE2, FOR6, OUT6) ;                                    0579
              WRITE(FILE2, FOR7, SPLIC) ;

COMMENT       A.C.I. - 1104 - A, B - THE AREA OF STEEL SHOULD BE WITHIN     0581
              1 AND 8 PERCENT OF THE COLUMN AREA. ;                         0582
              P ~ 200.0 | (ABAR1 | NBAR1 + ABAR2 | NBAR)  / (BC | TC) ;     0583
              IF P < 1.0 THEN WRITE(FILE2, DEFST, P) ;
              WRITE(FILE2[PAGE], ULDF, PUL) ;
              GO TO L51 ;                                                   0584
              END ;                                                         0585
EXIT:         END.                                                          0586
