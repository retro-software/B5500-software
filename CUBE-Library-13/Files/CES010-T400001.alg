 LABEL  0000000000XXXXXX0010000001
              MMM, DI, SIDE[0:50], IRN[0:200] ;                                1
                                                                               2
                                                                               3
         BEGIN                                                                 5
COMMENT       TRAVERSE CLOSURE PROGRAM FOR THE BURROUGHS                       6
              B-5000.                                                          7
              VICTOR BERMAN                                                    8
              (PROFESSIONAL SERVICES, BURROUGHS CORP.)                         9
              FIRST RELEASE JULY 1, 1962.                                     10
              THIS PROGRAM IS DESIGNED TO DO THE DIFFERENT                    11
              CALCULATIONS THAT ARISE IN CLOSING TRAVERSES                    12
              SUCH AS: CALCULATION OF MISSING LENGTHS                         13
              AND BEARINGS, TRAVERSE ADJUSTMENT WHEN ALL                      14
              THE ELEMENTS OF THE TRAVERSE ARE KNOWN,                         15
              COMPUTATION OF THE AREAS OF CLOSED TRAVERSES                    16
              AND AREAS OF THE LOTS WHEN THEIR BOUNDARIES                     17
              DO NOT COINCIDE WITH THE TRAVERSE  ;                            18
LABEL         T30, T31, T32, T33, T35, T42, T43, T44, T45,                    19
              T46, T47, T51, T53, T50 ;
SWITCH        SW3 ~ T31, T32, T33, T33  ;                                     21
SWITCH        SW4 ~ T53, T42, T43, T44, T45, T46, T47  ;                      22
REAL          ADAZM, ADAZR, ADAZS, ANC, ANCP, ANCPR, ARCIR,                   23
              ANCR, ANGLE, ANK, ALER, ANU, ANU1R, ANU2R,                      24
              ANCIR, ARE, AREAC, AREAT, AREATA, BR, BRG1,                     25
              BRG2, BR1, COREL, CORE1, CORNL, CORN1, DCP, D1, D2,             26
              DEP1, DEP2, DIS, DMD, ERCL1, ERDEP, ERLAT,                      27
                     INT, IRRAR, KTE, KT1, KT2, K1, K2, KTT,                  28
              K3, K4, LU, LU1, LU2, L1, L2, MMR,                              29
              PHI, PHIR, R, SADEP, SALAT, SDEP, SDIST,                        30
              SLAT, SSX, SUMEV, SUMOD, TES1R, TES2R,                          31
              UNAA, UNART, UNAT, TPI, PI, RAD, RADI  ;                        32
INTEGER       I, II, III, J, JJ, JJJ, KK, M, NP, CASE, G,                     33
              NS, NNS, ADJMET, DDX, MMX, LANDD ;                              34
REAL ARRAY    SS, DIST, LA, DE, NC, EC, CORLA,                                35
              CORDE[0:250], BRG, BRGR, LAT, DEP[0:200],                       36
              OFS, INTER[0:50],                                               37
              SSS, DISTA, LATI, DEPI[0:50] ;                                  38
INTEGER ARRAY DD, MM, DIR, DIRNN, MEDIR[0:250], DDD,                          39
              MMM, DI, SIDE[0:50], IRN[0:200] ;
FILE IN       IN1(1, 10) ;                                                    40
FILE OUT      OUT1 1(1, 15) ;                                                 41
FORMAT IN     DATAF(4F12.2, 2I5, F6.2),                                       44
              DATJF(I10),                                                     45
              DATBF(2I4, F6.1, I6,F12.2, I4) ;                                46
LIST          DATA(CORN1, CORE1, CORNL, COREL, ADJMET, M, ALER),              47
              DATJ(G),                                                        48
              DATB([DD[I], MM[I], SS[I], DIR[I], DIST[I], MEDIR[I]]),         49
              RES1(ERCL1),                                                    50
              RES2(FOR I ~ 1 STEP 1 UNTIL M + NNS DO                          51
              [I, DD[I], MM[I], SS[I], DIRNN[I], DIST[I],                     52
              CORLA[I], CORDE[I], NC[I], EC[I]]),                             53
              RES3(ARE, AREAC),                                               5U
              RES18(I, IRRAR),                                                55
              RES4(DCP),                                                      56
              RES5(DDX, MMX, SSX, DIRNN[I]),                                  57
              RES6(DCP, DDX, MMX, SSX, DIRNN[I]),                             58
              RES7(LU),                                                       59
              RES10(LU1, LU2)  ;                                              60
FORMAT OUT    REF1(//"ERROR OF CLOSURE =", F8.4, X1,                          61
              "FEET"///),                                                     62
              REFJ("J O B   N U M B E R", I10),                               63
              EJC(/),                                                         64
              TIT1("COURSE NUMBER", X14, "BEARING", X16,                      65
              "LENGTH", X7, "LATITUDE", X5, "DEPARTURE",                      66
              X12, "C O O R D I N A T E S"// X51, "FEET",                     67
              X10, "FEET", X9, "FEET", X16, "NORTH", X9,                      68
              "EAST"////),                                                    69
              REF2(X4, I3, X6, I3, X1, "DEG", X1, I3, X1,                     70
              "MIN", F6.1, " SEC", X1, A4, F12.2,                             71
              F14.2, F14.2, F20.2, F14.2/),                                   72
              TIT2("THE ERROR OF CLOSURE OF THE TRAVERSE ",                   73
              "IS LARGER THAN ALLOWABLE"/),                                   74
              REF3(//"AREA = ", F10.2, X1, "SQUARE FEET"//                    75
              "AREA = ", F10.4, X1, "ACRES"//),                               76
              REF18("AREA OF IRREGULAR PORTION", I2,                          77
              " = ", F15.2, X1, "SQUARE FEET"/),                              78
              TIT20(X40, "T R A V E R S E  P O L Y G O N"//),                 79
              TIT30(X45, "L O T  P O L Y G O N"//),                           80
              REF4(X10, "THE UNKNOWN LENGTH = ",  F15.2, " FT"),              81
              REF5(X10, "THE UNKNOWN BEARING = ", I3, " DEG", I3,             82
              " MIN", F5.1, " SEC", X2, A4),                                  83
              REF6(X10, "THE UNKNOWN LENGTH = ", F15.2, " FT"/                84
              "THE UNKNOWN BEARING = ", I2, " DEG", I3, " MIN", F5.1,         85
              " SEC", X2, A4),                                                86
              REF7(X10, "THE UNKNOWN DISTANCE =", F15.2, " FT"),              87
              REF8(X10, "FIRST UNKNOWN BEARING = ", I2, " DEG", I3,           88
              " MIN", F5.1, " SEC", X2, A4),                                  89
              REF9(X10, "SECOND UNKNOWN BEARING = ", I2, " DEG", I3,          90
              " MIN", F5.1, " SEC", X2, A4),                                  91
              REF10(X10, "FIRST UNKNOWN LENGTH", F15.2, " FT"/                92
              X10, "SECOND UNKNOWN LENGTH" , F15.2, " FT") ;                  93
COMMENT       BEARINGS AND DEFLECTIONS ARE CONVERTED INTO                     94
              AZIMUTHS.  THE METHOD USED TO READ DIRECTIONS                   95
              IS STATED BY,                                                   96
              AZIMUTH                  MEDIR[I] = 1                           97
              BEARING                  MEDIR[I] = 2                           98
              DEFLECTION ANGLE         MEDIR[I] = 3                           99
              BACK ANGLE               MEDIR[I] = 4  ;                       100
PROCEDURE     DEFL  ;                                                        101
         BEGIN                                                               102
LABEL         TA  ;                                                          103
COMMENT       TO CONVERT DEFLECTION ANGLES INTO AZIMUTHS,                    104
              FOR DEFLECTION ANGLES TO THE RIGHT DIR[I] = 1,                 105
              FOR DEFLECTION ANGLES TO THE LEFT DIR[I] = 2,                  106
              THE DEFLECTION ANGLE OF THE FIRST LINE                         107
              INDICATES ITS AZIMUTH FROM NORTH  ;                            108
              IF DIR[I] } 10 THEN GO TO TA  ;                                109
              IF DIR[I] = 2 THEN ANGLE ~ -ANGLE  ;                           110
              BRG[I] ~ BRG[I - 1] + ANGLE  ;                                 111
              IF MEDIR[I] = 4 THEN BRG[I] ~ BRG[I] + 180.0  ;                112
              IF BRG[I] } 360.0 THEN                                         113
              BRG[I] ~ BRG[I] - 360.0 ;                                      114
              IF BRG[I] < 0 THEN BRG[I] ~ BRG[I] + 360.0 ;                   115
TA:      END ;                                                               116
PROCEDURE     AZMTH ;                                                        117
COMMENT       THE VALUES GIVEN TO DIR[I] ARE FOR,                            118
              NORTHEAST  DIR[I] = 1                                          119
              SOUTHEAST  DIR[I] = 2                                          120
              NORTHWEST  DIR[I] = 3                                          121
              NORTHWEST  DIR[I] = 4 ;                                        122
         BEGIN                                                               123
LABEL         T1, T2, T3, T4, T8 ;                                           124
SWITCH        SW1 ~ T1, T2, T3, T4 ;                                         125
              IF DIR[I] } 10 THEN GO TO T8 ;                                 126
              GO TO SW1[DIR[I]] ;                                            127
T1:           BRG[I] ~ ANGLE ;                                               128
              GO TO T8 ;                                                     129
T2:           BRG[I] ~ 180.0 - ANGLE ;                                       130
              GO TO T8 ;                                                     131
T3:           BRG[I] ~ 180.0 + ANGLE ;                                       132
              GO TO T8 ;                                                     133
T4:           BRG[I] ~ 360.0 - ANGLE ;                                       134
T8:      END ;                                                               135
PROCEDURE     LADE ;                                                         136
COMMENT       THIS  SUBROUTINE CALCULATES LATITUDE AND                       137
              DEPARTURE OF EACH COURSE  ;                                    138
         BEGIN                                                               139
LABEL         T9  ;                                                          140
              SLAT ~ CORN1  ;                                                141
              SDEP ~ CORE1  ;                                                142
              FOR I ~ 1 STEP 1 UNTIL M DO                                    143
         BEGIN                                                               144
              IF DIR[I] } 10 THEN GO TO T9  ;                                145
              BRGR[I] ~ BRG[I] | RADI  ;                                     146
              LAT[I] ~ DIST[I] | COS(BRGR[I])  ;                             147
              DEP[I] ~ DIST[I] | SIN(BRGR[I])  ;                             148
              SLAT ~ LAT[I] + SLAT  ;                                        149
              SDEP ~ DEP[I] + SDEP  ;                                        150
T9:      END                                                                 151
         END  ;                                                              152
PROCEDURE     CLPO  ;                                                        153
COMMENT       COMPUTATION OF AZIMUTH AND LENGTH OF THE                       154
              LINE CLOSING THE POLYGON OF COURSES OF                         155
              KNOWN ELEMENTS  ;                                              156
         BEGIN                                                               157
              DCP ~ SQRT((SLAT - CORNL)*2 +                                  158
              (SDEP - COREL)*2)  ;                                           159
              KTE ~ (COREL - SDEP)/DCP ;                                     160
              ANCPR ~ ARCTAN(KTE/SQRT(1.0 - KTE*2)) ;                        161
              IF CORNL - SLAT < 0 THEN ANCPR ~ PI - ANCPR  ;                 162
              IF ANCPR < 0 THEN ANCPR ~ ANCPR + TPI                          163
              ELSE IF ANCPR > TPI THEN ANCPR ~ ANCPR - TPI  ;                164
              ANCP ~ ANCPR | RAD                                             165
         END  ;                                                              166
PROCEDURE     SIMPS  ;                                                       167
         BEGIN                                                               168
COMMENT       OFFSET LINES ARE AT REGULAR INTERVALS,                         169
              THE NUMBER OF INTERVALS MUST BE EVEN.                          170
              THE DATA WILL SHOW ZEROS FOR THE VALUES                        171
              OF DIST[J]  ;                                                  172
              SUMOD ~ SUMEV ~ IRRAR ~ 0  ;                                   173
              II ~ NP - 1  ;                                                 174
              FOR J ~ 2 STEP 2 UNTIL II DO                                   175
         BEGIN                                                               176
              SUMOD ~ OFS[J + 1] + SUMOD  ;                                  177
              SUMEV ~ OFS[J] + SUMEV                                         178
         END  ;                                                              179
              IRRAR ~ (OFS[I] - OFS[NP] + 2.0 | SUMOD +                      180
              4.0 | SUMEV) | INT/3.0  ;                                      181
              WRITE(OUT1, REF18, RES18)                                      182
         END  ;                                                              183
PROCEDURE     CORD  ;                                                        184
         BEGIN                                                               185
COMMENT       THE AREA IS CALCULATED BY METHOD OF                            186
              COORDINATES WHEN OFFSET LINES ARE AT                           187
              UNEQUAL INTERVALS.  THE DISTANCE FROM THE                      188
              FIRST POINT OF THE LINE TO EACH OFFSET                         189
              LINE IS RECORDED AS INTER[J].  THE DATA                        190
              WILL SHOW ZERO FOR THE VALUE OF INT.  ;                        191
              IRRAR ~ 0  ;                                                   192
              II ~ NP - 1  ;                                                 193
              FOR J ~ 1 STEP 1 UNTIL II DO                                   194
              IRRAR ~ (OFS[J + 1] + OFS[J]) |                                195
              (INTER[J + 1] - INTER[J]) + IRRAR  ;                           196
              IRRAR ~ 0.5 | IRRAR  ;                                         197
              WRITE(OUT1, REF18, RES18)                                      198
         END  ;                                                              199
PROCEDURE     CONV  ;                                                        200
         BEGIN                                                               201
LABEL         T19A, T19B, T19C, T19D, T19E  ;                                202
SWITCH        SW6 ~ T19A, T19B, T19C, T19D  ;                                203
              IF PHI ! 0 THEN GO TO T19E ;                                   204
              J ~ ENTIER(ANU/90) + 1 ;  ANU ~ ANU MOD 90 ;                   206
              GO TO SW6[J]  ;                                                210
T19A:         DIRNN[I] ~ "N.E."  ;     DIR[I] ~ 1 ;                          211
              GO TO T19E  ;                                                  212
T19B:         DIRNN[I] ~ "S.E."  ;     DIR[I] ~ 2 ;                          213
              ANU ~ 90.0 - ANU  ;                                            214
              GO TO T19E  ;                                                  215
T19C:         DIRNN[I] ~ "S.W."  ;     DIR[I] ~ 3 ;                          216
              GO TO T19E  ;                                                  217
T19D:         DIRNN[I] ~ "N.W."  ;     DIR[I] ~ 4 ;                          218
              ANU ~ 90.0 - ANU  ;                                            219
T19E:         DD[I] ~ DDX ~ ENTIER(ANU)  ;                                   220
              MMR ~ (ANU - DDX) | 60.0  ;                                    221
              MM[I] ~ MMX ~ ENTIER(MMR)  ;                                   222
              SS[I] ~ SSX ~ (MMR - MMX) | 60.0 ;                             223
              IF SSX > 59.9 THEN                                             224
         BEGIN                                                               225
              SS[I] ~ SSX ~ 0 ;      MM[I] ~ MMX ~ MMX + 1                   226
         END                                                                 227
         END  ;                                                              228
PROCEDURE     IRAR  ;                                                        229
         BEGIN                                                               230
COMMENT       CALCULATION OF TOTAL AREA INCLUDING PORTIONS                   231
              WHOSE BOUNDARIES ARE LOCATED BY OFFSET LINES  ;                232
INTEGER       I ;                                                            233
FORMAT IN     DAF(I4, F10.2),                                                234
              DBF(2F10.2) ;                                                  235
LIST          DA(NP, INT),                                                   236
              DB(FOR I ~ 1 STEP 1 UNTIL NP DO                                237
              [OFS[I], INTER[I]])  ;                                         238
              READ(IN1, DAF, DA) ;                                           239
              READ(IN1, DBF, DB) ;                                           240
              IF INT = 0 THEN CORD                                           241
              ELSE SIMPS  ;                                                  242
              AREAT ~ AREAT + IRRAR                                          243
         END  ;                                                              244
PROCEDURE     CIRC  ;                                                        245
         BEGIN                                                               246
COMMENT       CALCULATION OF AREA OF CIRCULAR SEGMENT,                       247
              RADIUS AND CENTRAL ANGLE OF ARC  ;                             248
FORMAT OUT    REF35("CIRCULAR SEGMENT", I4//"RADIUS ",                       249
              "OF CIRCLE =", F10.2, X1, "FEET"//                             250
              "AREA OF CIRCULAR SEGMENT = ", F10.2, X1,                      251
              "SQUARE FEET"/),                                               252
              REF36("CENTRAL ANGLE = ", I3, " DEG", I4, " MIN", F5.1,        253
              " SEC"//)  ;                                                   254
LIST          RES35(I, R, ARCIR),                                            255
              RES36(DDX, MMX, SSX)  ;                                        256
              ANU ~ PHI ~ ABS(BRG[I + 1] - BRG[I - 1])  ;                    257
              PHIR ~ 0.5 | PHI | RADI  ;                                     258
              R ~ 0.5 | DIST[I] / SIN(PHIR)  ;                               259
              ARCIR ~ PHIR | R*2-                                            260
              (0.5 | DIST[I] | R | COS(PHIR))  ;                             261
              CONV  ;                                                        262
              AREAT ~ AREAT + ARCIR  ;                                       263
              WRITE(OUT1, REF35, RES35)  ;                                   264
              WRITE(OUT1, REF36, RES36)  ;                                   265
              PHI ~ 0                                                        266
         END  ;                                                              267
PROCEDURE     AREAS  ;                                                       268
         BEGIN                                                               269
LABEL         T20  ;                                                         270
FORMAT OUT    REF11("AREA INCLUDING IRREGULAR PROTIONS =",                   271
              F10.2, X1, "SQUARE FEET"//                                     272
              "AREA INCLUDING IRREGULAR PORTIONS =",                         273
              F10.4, X1, "ACRES"///)  ;                                      274
LIST          RES11(AREAT, AREATA)  ;                                        275
              CORDE[0] ~ DMD ~ ARE ~ 0  ;                                    276
              II ~ M + NNS  ;                                                277
COMMENT       COMPUTATION OF AREA BY DOUBLE-MERIDIAN-                        278
              DISTANCE METHOD  ;                                             279
              FOR I ~ 1 STEP 1 UNTIL II DO                                   280
         BEGIN                                                               281
              DMD ~ CORDE[I - 1] + CORDE[I] + DMD  ;                         282
              ARE ~ CORLA[I] | DMD + ARE                                     283
         END  ;                                                              284
              AREAT ~ ARE ~ ABS(0.5 | ARE)  ;                                285
              AREAC ~ ARE/43560.0  ;                                         286
              WRITE(OUT1, REF3, RES3) ;                                      287
              FOR I ~ 1 STEP 1 UNTIL M DO                                    288
         BEGIN                                                               289
              IF (IRN[I] } 100) AND (IRN[I] < 1000) THEN                     290
              IRAR  ;                                                        291
              IF IRN[I] } 1000 THEN CIRC                                     292
         END  ;                                                              293
              FOR I ~ 1 STEP 1 UNTIL M DO                                    294
              IF IRN[I] } 100 THEN                                           295
         BEGIN                                                               296
              AREATA ~ AREAT/43560.0  ;                                      297
              WRITE(OUT1, REF11, RES11)  ;                                   298
              GO TO T20                                                      299
         END  ;                                                              300
T20:     END  ;                                                              301
PROCEDURE     ADBLC ;                                                        302
         BEGIN                                                               303
COMMENT       CALCULATION OF ADJUSTED BEARINGS, LENGTHS AND COORDINATES;     304
              DIST[I] ~ SQRT(CORLA[I]*2 + CORDE[I]*2)  ;                     305
              ADAZR ~ ARCTAN(CORDE[I]/CORLA[I]) ;                            306
              ADAZS ~ ABS(ADAZR) | RAD  ;                                    307
              IF CORLA[I] < 0 THEN ADAZR ~ PI + ADAZR  ;                     308
              IF ADAZR > TPI THEN ADAZR ~ ADAZR - TPI  ;                     309
              IF ADAZR < 0 THEN ADAZR ~ ADAZR + TPI  ;                       310
              ANU ~ ADAZM ~ RAD | ADAZR ;                                    311
              CONV ;                                                         312
              NC[I] ~ NC[I - 1] + CORLA[I]  ;                                313
              EC[I] ~ EC[I - 1] + CORDE[I]  ;                                314
         END ;                                                               315
PROCEDURE     AJUST ;                                                        316
COMMENT       GIVEN ALL THE ELEMENTS OF THE TRAVERSE, THE                    317
              ERROR OF CLOSURE IS CALCULATED AND THE                         318
              TRAVERSE IS ADJUSTED  ;                                        319
         BEGIN                                                               320
LABEL         T10, T15, T16, T17 ;                                           321
              IF NNS ! 0 THEN GO TO T17  ;                                   322
              ERLAT ~ SLAT - CORNL  ;                                        323
              ERDEP ~ SDEP - COREL  ;                                        324
              ERCL1 ~ SQRT(ERLAT*2 + ERDEP*2)  ;                             325
COMMENT       THE SOLUTION OF THE PROBLEM MAY BE STOPPED                     326
              AT THIS POINT IF THE ERROR OF CLOSURE IS                       327
              LARGER THAN THE ALLOWABLE, (THE ALLOWABLE                      328
              ERROR IS NOT ZERO)  ;                                          329
              IF ALER = 0 THEN GO TO T10  ;                                  330
              IF ALER < ERCL1 THEN                                           331
         BEGIN                                                               332
              WRITE(OUT1 [PAGE], TIT2)  ;                                    333
              GO TO T30                                                      334
         END  ;                                                              335
T10:          IF ADJMET = 2 THEN GO TO T15  ;                                336
COMMENT       ADJUSTMENT BY COMPASS RULE  ;                                  337
              SDIST ~ 0  ;                                                   338
              FOR I ~ 1 STEP 1 UNTIL M DO                                    339
              SDIST ~ DIST[I] + SDIST  ;                                     340
              K1 ~ ERLAT/SDIST  ;                                            341
              K2 ~ ERDEP/SDIST  ;                                            342
              FOR I ~ 1 STEP 1 UNTIL M DO                                    343
         BEGIN                                                               344
              CORLA[I] ~ -DIST[I] | K1 + LAT[I]  ;                           345
              CORDE[I] ~ -DIST[I] | K2 + DEP[I]                              346
         END  ;                                                              347
              GO TO T16  ;                                                   348
COMMENT       ADJUSTMENT BY TRANSIT RULE  ;                                  349
T15:          SALAT ~ SADEP ~ 0  ;                                           350
              FOR I ~ 1 STEP 1 UNTIL M DO                                    351
         BEGIN                                                               352
              SALAT ~ ABS(LAT[I]) + SALAT  ;                                 353
              SADEP ~ ABS(DEP[I]) + SADEP                                    354
         END  ;                                                              355
              K3 ~ ERLAT/SALAT  ;                                            356
              K4 ~ ERDEP/SADEP  ;                                            357
              FOR I ~ 1 STEP 1 UNTIL M DO                                    358
         BEGIN                                                               359
              CORLA[I] ~ -ABS(LAT[I]) | K3 + LAT[I]  ;                       360
              CORDE[I] ~ -ABS(DEP[I]) | K4 + DEP[I]                          361
         END  ;                                                              362
T16:          NC[0] ~ CORN1  ;                                               363
              EC[0] ~ CORE1  ;                                               364
              FOR I ~ 1 STEP 1 UNTIL M DO                                    365
T17:          ADBLC ;                                                        366
              WRITE(OUT1, TIT20)  ;                                          367
              WRITE(OUT1, REF1, RES1)                                        368
         END  ;                                                              369
PROCEDURE     LAND  ;                                                        370
         BEGIN                                                               371
COMMENT       CALCULATION OF COORDINATES OF POINTS                           372
              OUTSIDE OF THE TRAVERSE, WHEN THESE POINTS                     373
              ARE LOCATED BY THE TRANSIT  ;                                  374
LABEL         T21, T22, T23, T24, T25, T26, T27, T28, T29 ;                  375
SWITCH        SW2 ~ T24, T21, T22, T23  ;                                    376
FORMAT IN     DATA2F(I10),                                                   377
              DATA2AF(2I4, F6.1, I6, F12.2, 2I4) ;                           378
FORMAT OUT    TIT30(X45, "L O T  P O L Y G O N") ;                           379
LIST          DATA2(NS),                                                     380
              DATA2A(FOR J ~ 1 STEP 1 UNTIL NS DO                            381
              [DDD[J], MMM[J], SSS[J], DI[J], DISTA[J],                      382
              MEDIR[J], SIDE[J]]) ;                                          383
              IF NNS ! 0 THEN GO TO T28  ;                                   384
              READ(IN1, DATA2F, DATA2)  ;                                    385
              READ(IN1, DATA2AF, DATA2A) ;                                   386
              FOR J ~ 1 STEP 1 UNTIL NS DO                                   387
         BEGIN                                                               388
              BR ~ MMM[J]/60.0 + SSS[J]/3600.0 + DDD[J]  ;                   389
              IF MEDIR[J] = 2 THEN                                           400
         BEGIN                                                               401
              GO TO SW2[DI[J]]  ;                                            402
T21:          BR ~ 180.0 - BR  ;                                             403
              GO TO T24  ;                                                   404
T22:          BR ~ 180.0 + BR  ;                                             405
              GO TO T24  ;                                                   406
T23:          BR ~ 360.0 - BR  ;                                             407
T24:     END  ;                                                              408
              IF MEDIR[J] > 2 THEN                                           409
         BEGIN                                                               410
              JJ ~ SIDE[J]  ;                                                411
              IF DI[J] = 2 THEN BR ~ -BR  ;                                  412
              BR ~ BRG[JJ] + BR  ;                                           413
              IF MEDIR[J] = 4 THEN BR ~ BR + 180.0                           414
         END  ;                                                              415
              KTE ~ DISTA[J]  ;                                              416
              KTT ~ BR | RADI  ;                                             417
              LATI[J] ~ COS(KTT) | KTE  ;                                    418
              DEPI[J] ~ SIN(KTT) | KTE                                       419
         END  ;                                                              420
              I ~ 1 ;      III ~ 0 ;                                         421
              FOR II ~ 1 STEP 1 UNTIL M DO                                   422
              IF SIDE[I] = II THEN                                           424
         BEGIN                                                               425
T26:          III ~ III + 1 ;                                                426
              LA[III] ~ CORLA[II] + LATI[I]  ;                               427
              DE[III] ~ CORDE[II] + DEPI[I]  ;                               428
              IF SIDE[I] = SIDE[I - 1] THEN                                  429
         BEGIN                                                               430
              LA[III] ~ -LA[III - 1] + LA[III]  ;                            431
              DE[III] ~-DE[III - 1] + DE[III]                                432
         END  ;                                                              433
              IF SIDE[I + 1] ! SIDE[I] THEN                                  434
         BEGIN                                                               435
              CORLA[II + 1] ~ CORLA[II + 1] - LATI[I]  ;                     436
              CORDE[II + 1] ~ CORDE[II + 1] - DEPI[I]                        437
         END  ;                                                              438
              I ~ I + 1  ;                                                   439
              IF SIDE[I] = II THEN GO TO T26                                 439
         END                                                                 440
              ELSE                                                           441
         BEGIN                                                               442
              III ~ III + 1  ;                                               443
              LA[III] ~ CORLA[II]  ;                                         444
              DE[III] ~ CORDE[II]                                            445
         END  ;                                                              446
T27:          NNS ~ 0  ;                                                     449
              FOR J ~ 1 STEP 1 UNTIL NS DO                                   450
              IF SIDE[J + 1] = SIDE[J] THEN NNS ~ NNS + 1 ;                  452
              II ~ M + NNS  ;                                                454
              FOR I ~ 1 STEP 1 UNTIL II DO                                   455
         BEGIN                                                               456
              CORLA[I] ~ LA[I]  ;                                            457
              CORDE[I] ~ DE[I]                                               458
         END  ;                                                              459
T28:          FOR I ~1 STEP 1 WHILE (I { (M + NNS)) DO                       460
              ADBLC ;                                                        461
T29:          WRITE(OUT1 [PAGE], TIT30)  ;                                   462
              WRITE(OUT1, TIT1)  ;                                           463
              WRITE(OUT1, REF2, RES2)  ;                                     464
              IF (CORN1 = CORE1) AND (CORNL = COREL) THEN                    465
              AREAS                                                          466
         END  ;                                                              467
              RAD ~ 57.295779 ;      PI ~ 3.1415927 ;     TPI ~ 2 | PI ;     468
              RADI ~ 1.0/RAD ;                                               469
T30:          READ(IN1, DATJF, DATJ) ;                                       470
              WRITE(OUT1 [PAGE], EJC) ;                                      471
              WRITE(OUT1, REFJ, DATJ) ;                                      472
              READ(IN1, DATAF, DATA) ;                                       473
              PHI ~ NNS ~ LANDD ~ 0 ;                                        474
              FOR I ~ 1 STEP 1 UNTIL M DO                                    475
              READ(IN1, DATBF, DATB) ;                                       477
              IF ADJMET } 10 THEN                                            482
         BEGIN                                                               483
              LANDD ~ ADJMET  ;                                              484
              ADJMET ~ ADJMET - 10                                           485
         END  ;                                                              486
              FOR I ~ 1 STEP 1 UNTIL M DO                                    487
         BEGIN                                                               488
              IRN[I] ~ 0  ;                                                  489
              IF DIR[I] } 1000 THEN                                          490
         BEGIN                                                               491
              IRN[I] ~ DIR[I]  ;                                             492
              DIR[I] ~ DIR[I] - 1000                                         493
         END  ;                                                              494
              IF DIR[I] } 100 THEN                                           495
         BEGIN                                                               496
              IRN[I] ~ DIR[I]  ;                                             497
              DIR[I] ~ DIR[I] - 100                                          498
         END                                                                 499
         END  ;                                                              500
              FOR I ~ 1 STEP 1 UNTIL M DO                                    501
         BEGIN                                                               502
              IF DIR[I] } 10 THEN GO TO T35  ;                               503
              ANGLE ~ DD[I] + MM[I]/60.0 + SS[I]/3600.0  ;                   504
              GO TO SW3[MEDIR[I]]  ;                                         505
T31:          BRG[I] ~ ANGLE  ;                                              506
              GO TO T35  ;                                                   507
T32:          AZMTH  ;                                                       508
              GO TO T35  ;                                                   509
T33:          DEFL  ;                                                        510
T35:     END  ;                                                              511
              JJ ~ KK ~ 0  ;                                                 512
COMMENT       TO FIND IF ANY ELEMENT OF THE TRAVERSE IS                      513
              MISSING AND IF SO, ENTER THE CASE THAT                         514
              SOLVES THE UNKNOWN(S)  ;                                       515
              FOR I ~ 1 STEP 1 UNTIL M DO                                    516
         BEGIN                                                               517
              IF DIST[I] = 0 THEN JJ ~ JJ + 1  ;                             518
              IF DIR[I] } 10 THEN KK ~ KK + 1                                519
         END  ;                                                              520
              IF (JJ = 0) AND (KK = 0) THEN CASE ~ 1                         521
              ELSE IF (JJ = 1) AND (KK = 0) THEN CASE ~ 2                    522
              ELSE IF (JJ = 0) AND (KK = 1) THEN CASE ~ 3                    523
              ELSE IF (JJ = 1) AND (KK = 1) THEN                             524
         BEGIN                                                               525
              II ~ 0  ;                                                      526
              FOR II ~ II + 1 WHILE DIST[II] ! 0 DO  ;                       527
              IF DIR[II] } 10 THEN CASE ~ 4                                  528
              ELSE CASE ~ 5                                                  529
         END                                                                 530
              ELSE IF (JJ = 0) AND (KK = 2) THEN CASE ~ 6                    531
              ELSE IF (JJ = 2) AND (KK = 0) THEN CASE ~ 7  ;                 532
              LADE  ;                                                        533
              IF CASE = 1 THEN AJUST                                         534
              ELSE CLPO  ;                                                   535
              GO TO SW4[CASE]  ;                                             536
COMMENT       ONE LENGTH UNKNOWN  ;                                          537
T42:          III ~ I ~ 0  ;                                                 538
              FOR III ~ I ~ I + 1 WHILE DIST[I] ! 0 DO  ;                    539
              DIST[III] ~ DCP  ;                                             540
              WRITE(OUT1 [PAGE], REF4, RES4)  ;                              541
              GO TO T50  ;                                                   542
COMMENT       ONE BEARING UNKNOWN  ;                                         543
T43:          III ~ I ~ 0  ;                                                 544
              FOR III ~ I ~ I + 1 WHILE DIR[I] < 10 DO  ;                    545
              BRG[III] ~ ANU ~ ANCP  ;                                       546
              CONV  ;                                                        547
              WRITE(OUT1 [PAGE], REF5, RES5)  ;                              548
              GO TO T50  ;                                                   549
COMMENT       BEARING AND LENGTH OF SAME COURSE UNKNOWN  ;                   550
T44:          III ~ I ~ 0  ;                                                 551
              FOR III ~ I ~ I + 1 WHILE DIST[I] ! 0 DO  ;                    552
              BRG[I] ~ ANU ~ ANCP  ; DIST[I] ~ DCP  ;                        553
              CONV  ;                                                        554
              WRITE(OUT1 [PAGE], REF6, RES6)  ;                              555
              GO TO T50  ;                                                   556
COMMENT       UNKNOWN BEARING AND LENGTH OF DIFFERENT                        557
              COURSES  ;                                                     558
T45:          JJJ ~ J ~ III ~ I ~ 0  ;                                       559
              FOR JJJ ~ J ~ J + 1 WHILE DIST[J] ! 0 DO  ;                    560
              BR1 ~ BRG[J]  ;                                                561
              FOR III ~ I ~ I + 1 WHILE DIR[I] < 10 DO  ;                    562
              DIS ~ DIST[I]  ;                                               563
              IF BR1 > ANCP + 180.0 THEN                                     564
              ANK ~ 360.0 - BR1 + ANCP                                       565
              ELSE IF BR1 < ANCP - 180.0 THEN                                566
              ANK ~ 360.0 + BR1 - ANCP                                       567
              ELSE ANK ~ ABS(BR1 - ANCP)  ;                                  568
              IF ANK > 180.0 THEN ANK ~ ANK - 180.0  ;                       569
              KTE ~ DIS/SIN(ANK | RADI)  ;                                   570
              KTT ~ DCP/ KTE ;                                               571
              UNAA ~ RAD | ABS(ARCTAN(KTT/SQRT(1.0 - KTT*2))) ;              572
COMMENT       IF THE ANGLE BETWEEN THE TWO UNKNOWN                           573
              COURSES IS BETWEEN 90 AND 270 DEGREES                          574
              DIR[I] OF UNKNOWN BEARING IS 12  ;                             575
              IF DIR[I] = 12 THEN UNAA ~ 180.0 - UNAA  ;                     576
              UNAT ~ 180.0 - UNAA - ANK  ;                                   577
              DIST[JJJ] ~ LU ~ KTE | ABS(SIN(UNAT | RADI))  ;                578
              WRITE(OUT1, REF7, RES7)  ;                                     579
              LADE  ;      CLPO ;                                            580
              GO TO T43  ;                                                   581
COMMENT       TWO BEARINGS ARE UNKNOWN  ;                                    582
T46:          I ~ 0  ;                                                       583
              FOR III ~ I ~ I + 1 WHILE DIR[I] < 10 DO  ;                    584
              L1 ~ DIST[I]  ;  J ~ I + 1  ;                                  585
              FOR JJJ ~ J ~ J + 1  WHILE DIR[J] < 10 DO  ;                   586
              L2 ~ DIST[J]  ;                                                587
              KTE ~ (L2*2 + DCP*2 - L1*2)/(2 | L2 | DCP ) ;                  588
              ANU1R ~ ARCTAN(SQRT(1.0 - KTE*2)/KTE) ;                        589
              KTE ~ (L1 * 2 + DCP*2 - L2*2)/(2 | L1 | DCP) ;                 590
              ANU2R ~ ARCTAN(SQRT(1.0 - KTE*2)/KTE) ;                        591
COMMENT       THE LINE OF UNKNOWN BEARING LOCATED IN AN                      592
              ANGLE COUNTERCLOCKWISE FROM THE LINE                           593
              CLOSING THE POLYGON OF KNOWN SIDES WILL                        594
              SHOW DIR[I] = 12  ;                                            595
              IF DIR[III] = 12         THEN ANU2R ~ -ANU2R  ;                596
              IF DIR[JJJ] = 12         THEN ANU1R ~ -ANU1R  ;                597
              TES2R ~ ANCPR + ANU2R  ;                                       598
              TES1R ~ ANCPR + ANU1R  ;                                       599
              ANU ~ BRG[III] ~ TES2R | RAD  ;                                600
              CONV  ;                                                        601
              WRITE(OUT1, REF8, RES5)  ;                                     602
              I ~ JJJ  ;                                                     603
              ANU ~ BRG[JJJ] ~ TES1R | RAD  ;                                604
              CONV  ;                                                        605
              WRITE(OUT1, REF9, RES5)  ;                                     606
              GO TO T50  ;                                                   607
COMMENT       TWO LENGTHS ARE UNKNOWN  ;                                     608
T47:          III ~ I ~ 0  ;                                                 609
              FOR III ~ I ~ I + 1 WHILE DIST[I] ! 0 DO  ;                    610
              BRG1 ~ BRG[I]  ;                                               611
              J ~ I  ;                                                       612
              FOR JJJ ~ J ~ I + 1 WHILE DIST[J] ! 0 DO  ;                    613
              BRG2 ~ BRG[J]  ;                                               614
              ANCR ~ ABS(BRG2 - BRG1) | RADI  ;                              615
              KTE ~ DCP/SIN(ANCR)  ;                                         616
              KT1 ~ BRG1 - ANCP  ;                                           617
              KT2 ~ BRG2 - ANCP  ;                                           618
              IF KT1 > 180.0 THEN KT1 ~ 360.0 - KT1  ;                       619
              IF KT2 > 180.0 THEN KT2 ~ 360.0 - KT2  ;                       620
              LU1 ~ KTE | SIN(ABS(KT2) | RADI)  ;                            621
              LU2 ~ KTE | SIN(ABS(KT1) | RADI)  ;                            622
              DIST[III] ~ LU1  ;                                             623
              DIST[JJJ] ~ LU2  ;                                             624
              WRITE(OUT1, REF10, RES10)  ;                                   625
              WRITE(OUT1, TIT20)  ;                                          626
COMMENT       COMPUTATION OF LATITUDE AND DEPARTURE OF                       627
              ONE SIDE IF ANY OF ITS ELEMENTS WAS UNKNOWN  ;                 628
T50:          BRGR[III] ~ BRG[III] | RADI  ;                                 629
              LAT[III] ~ DIST[III] | COS(BRGR[III])  ;                       630
              DEP[III] ~ DIST[III] | SIN(BRGR[III])  ;                       631
              IF CASE { 4 THEN GO TO T51  ;                                  632
COMMENT       COMPUTATION OF LATITUDE AND DEPARTURE OF                       633
              A SECOND COURSE IF ANY OF ITS ELEMENTS                         634
              WAS UNKNOWN  ;                                                 635
              BRGR[JJJ] ~ BRG[JJJ] | RADI  ;                                 636
              LAT[JJJ] ~ DIST[JJJ] | COS(BRGR[JJJ])  ;                       637
              DEP[JJJ] ~ DIST[JJJ] | SIN(BRGR[JJJ])  ;                       638
T51:          FOR I ~ 1 STEP 1 UNTIL M DO                                    639
         BEGIN                                                               640
              CORLA[I] ~ LAT[I]  ;                                           641
              CORDE[I] ~ DEP[I]                                              642
         END  ;                                                              643
              NC[0] ~ CORN1  ;                                               644
              EC[0] ~ CORE1  ;                                               645
              II ~ M + NNS  ;                                                646
              FOR I ~ 1 STEP 1 UNTIL II DO                                   647
         BEGIN                                                               648
              IF MEDIR[I] = 1 THEN DIRNN[I] ~ 0                              649
              ELSE IF DIR[I] = 1 THEN DIRNN[I] ~ "N.E."                      650
              ELSE IF DIR[I] = 2 THEN DIRNN[I] ~ "S.E."                      651
              ELSE IF DIR[I] = 3 THEN DIRNN[I] ~ "S.W."                      652
              ELSE DIRNN[I] ~ "N.W."  ;                                      653
              NC[I] ~ NC[I - 1] + CORLA[I]  ;                                654
              EC[I] ~ EC[I - 1] + CORDE[I]                                   655
         END  ;                                                              656
T53:          WRITE(OUT1, TIT1)  ;                                           657
              WRITE(OUT1, REF2, RES2)  ;                                     658
              IF (CORN1 = CORNL) AND (CORE1 = COREL) THEN                    659
              AREAS  ;                                                       660
              IF LANDD ! 0 THEN LAND  ;                                      661
              GO TO T30  ;                                                   662
         END.                                                                663
 LABEL  000000000IN1    0010000001
         1
   297555.55   734617.29   411809.16   756914.78    1    8  0.00
   0   0   0.0    12    21042.91   2
  20  38  31.0     4        0.00   2
  34  30  24.0     4    34091.87   2
 311  58  56.0     1    27209.27   1
  23  14  28.0     1    21223.40   2
 339  59   3.0     1    24559.51   1
  32  31  16.0     1    27871.87   2
  48  49  30.0     2    58182.52   2
         2
         .0          .0          .0          .0     1    7   .0
  80  10    .0     1      100.0    2
  35  25    .0     1      200.0    2
  85  20    .0  1001      200.0    2
  25  40    .0   102      300.0    2
  30  30    .0     3      400.0    2
  25   0    .0     4      200.0    2
  47  20    .0   104      350.0    2
   7     50.00
      5.00      0.0
     10.0       0.0
     12.0       0.0
      8.0       0.0
      3.0       0.0
     20.0       0.0
      0.0       0.0
   6      0.00
      0.0       0.0
     30.2      50.0
     12.2     120.0
     57.4     221.0
      5.0     302.0
      0.0     350.0
         3
         .0          .0          .0          .0    11    7   .0
  80  10    .0     1      100.0    2
  35  25    .0     1      200.0    2
  85  20    .0     1      200.0    2
  25  40    .0     2      300.0    2
  30  30    .0     3      400.0    2
  25   0    .0     4      200.0    2
  47  20    .0     4      350.0    2
         5
 270   0    .0     1       50.0    1   2
  10   0    .0     4       35.1    2   2
  50   0    .0     4       27.3    2   3
  80   0    .0     1       29.9    2   3
  75   0    .0     3       53.4    2   5

