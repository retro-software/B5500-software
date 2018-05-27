         BEGIN

              FOR THE BURROUGHS B-5000 COMPUTER.

COMMENT       DESIGN OF REINFORCED CONCRETE FIXED END BEAMS PROGRAM
              VICTOR BERMAN

              PROGRAM CARD SEQUENCE STARTS WITH FEBC-0001
              FIRST RELEASE JULY 1, 1963.
              THIS PROGRAM DESIGNS THE REINFORCEMENT OF A FIXED END
              BEAM IN ORDER TO RESIST THE IMPOSED LOADS AND BENDING
              MOMENTS. ;

REAL          DIAM, DISLD, LENTH, SHRF, REAC, REAC3, DIS, MMAX,             0002
              INC, KT1, KTY, ALCBN, CUTI,                                   00A2
              KTA, KTB, KTC, KTT, UD, BU, DU, SC, ROOT, Q, P, SS,           0003
              AS, PER, SHRMX, ALBON, UNPER, NBARS, ABAR, AGSZ, REQB,        0004
              WIDTH, COH, SPA, Y, SPAST, SHRS, SHRCN, K, ALSHR, ML,         0005
              MR, MULL, MULW, MULD, MURL, MURW, MURD, ABOT, TU, DEPTH,      0006
              WT, LLL, LL, DDL, DL, WINDL, WIND, MUR, MUL, MP,              0007
              TLOAD, REAB, SHRL, DISR, SHRR, DISRT, DIAM1, CH, COH1,        0008
              SPA1, CV, COV, COV1, BOND, KTE, KTX, COMP, IN1, ATOP,REQQ,
              REBB, SPAC, SPLIC, SHEAR, N, KD, JD, TT, STSTB, CNSTB,        0010
              TU1, SM, DEFC, DEFM, DEFD, DEFF, ALDE, MCR, TERM1, TERM3,     0011
              MU, APS, CT, REQ, FD, EXTN, EXTN1, SHRF1, ANCT,               0012
              CTI, ABOTT, ATOPP, TERM2, F1, F2, SF, Z, STTOP, STSZ,         0013
              INT, STSTR  ;                                                 0014

ALPHA         SIDE, RIGHT, CENTER, LEFT, UPDN1, TOP, BOTTOM ;               0015

INTEGER       SBD, NCONC, I, II, J, JJ, KK, NBAR, NBAR1, SBD1,              0016
              CUTF, NUM,                                                     A16
              NBOT, NBEND, NADL, NST, NXTND, EXTF, NXTNT, NDLB, CTF,        0017
              NSTR, SBDS ;                                                  0018

BOOLEAN       TRY, SBDF, SKIP, POS, TIME, TRLD, TMP ;                       0019

LABEL         L103, L140 ;

FILE          FILE1(1, 10),                                                 0020
              FILE2 1(1, 15) ;

REAL ARRAY    CONLD, DIST, COLL, CODL, COW[0:10],                             25
              PTI1, PTI2[0:20], INCH[0:40]  ;                                 26

INTEGER ARRAY FT[0:40], PTF1, PTF2[0:20]  ;                                   27

FORMAT IN     F1A(I5, 3F10.1, I5, F10.1),
              F1B(6F10.1),
              F1C(7F10.1, L5),
              F2A(4F10.1) ;

FORMAT OUT    FOR50(X25, "THE SECTION OF BEAM NUMBER ", I4,                 0030
              " HAS THE FOLLOWING DIMENSIONS"//"WIDTH = ", F5.1,            0031
              " INCHES" //"DEPTH = ", F5.1, " INCHES" /),                   0032
              DEF50("THE MOMENT AT THE CENTER OF THE BEAM IS NEGATIVE"),    0033
              DEF51(X10, "THE WIDTH OF THE BEAM SHOULD BE ",F5.1,           0034
              " INCHES, IN ORDER TO PLACE" //X10, "THE CENTER "             0035
              "BOTTOM BARS" /),                                             0036
              FOR51("PLACE ", I3, X1, A6, " BARS AT ", A6, " OF THE ",      0037
              "BEAM", " S.B.D. NUMBER", I4, // "AT ", F5.1, " INCHES ",     0038
              "FROM THE ", A6, " SPACED ", F5.1, " INCHES CENTER TO ",      0039
              "CENTER"// "CENTER OF EXTERIOR BARS IS AT", F5.1,             0040
              " INCHES FROM THE FACES OF THE BEAM" /),                      0041
              FOR51A(I4, " BARS ARE EMBEDDED 6 INCHES INTO SUPPORTS"/),     0042
              DEF52(X10,"THE SECTION MUST BE INCREASED, TO KEEP ",          0043
              "SHEAR STRESS WITHIN ALLOWABLE LIMITS" /),                    0044
              DEF53(X10, "THE SIZE OF CENTER BOTTOM BARS SHOULD BE ",       0045
              "REDUCED TO REDUCE BOND STRESS" /),                           0046
              TIT53("BEND A BOTTOM BAR TO A 45 DEGREE ANGLE AT A ",         0047
              "DISTANCE FROM" // X31, " LEFT SUPPORT", X21,
              "RIGHT SUPPORT" /),
              FOR53(X10, 2(X15, I3, " FEET", F5.0, " INCHES ") /),          0050
              FOR54("BEND THESE BARS AT ", F5.1," INCHES FROM THE TOP ",    0051
              "AND RUN THEM ", F5.1, " INCHES INTO THE SUPPORTS" /),        0052
              FOR55("A SECOND ROW OF BARS IS NEEDED AT THE ", A6, X1,       0053
              A5, " SUPPORT"// "CLEAR DISTANCE BETWEEN ROWS IS 1 IN."//     0054
              "PLACE ", I3, " BARS, S.B.D. NUMBER ", I3/),                  0055
              FOR56(I4, " STRAIGHT BARS ARE PLACED AT THE ", A6, X1, A5,    0056
              " SUPPORT S.B.D. NUMBER ", I3 /),                             0057
              TIT56("PLACE TOP BARS DIRECTLY OVER THE BOTTOM BARS" /),      0058
              FOR57(I3, "ADDITIONAL BOTTOM BAR(S), S.B.D. NUMBER", I3,      0A58
              " MUST BE PLACED AT THE ", A5 // "CUT THE BAR(S) AT", I3,
              " FEET", F5.0, " INCHES FROM THE SUPPORT" /),                 0C58
              FOR60("CUT ", I3, X1, A6, " STRAIGHT BAR(S) AT", I3,          0059
              " FEET",F5.0, " INCHES FROM ", A5, " SUPPORT" /),             0060
              DEF61(X10, "THE WIDTH OF THE BEAM SHOULD BE INCREASED TO",    0061
              F5.1, " INCHES"//X10,"TO PLACE THE ", A6, " BARS AT THE ",    0062
              A5, " SUPPORT"/),                                             0063
              FOR63("BOTTOM BARS AT THE ", A5, " SUPPORT ARE EMBEDDED",     0A63
              F5.0, " INCHES INTO THE COLUMN"/),                            0B63
              FOR64A("NUMBER",I4, " BARS ARE USED AS STIRRUPS "//           0064
              "DISTANCE OF STIRRUPS FROM LEFT SUPPORT" /),                  0065
              FOR64(I6, " FEET", F5.0, " INCHES" /),                        0077
              FOR65("MAXIMUM STRESSES AT THE CENTER OF THE SPAN" /),        0067
              FOR66("MAXIMUM STRESSES AT THE ", A5, " SUPPORT"/),
              FOR67("CONCRETE BENDING STRESS = ", F16.0, " P.S.I. " //      0069
              "BOTTOM STEEL BENDING STRESS = ", F12.0, " P.S.I."//          0070
              "BOTTOM STEEL BOND STRESS = ", F15.0, " P.S.I."//"SHEAR ",    0072
              "STRESS = ", F27.0, " P.S.I." /),                             0073
              FOR68("TOP STEEL BENDING STRESS = ", F15.0, " P.S.I."//       0074
              "TOP STEEL BOND STRESS = ", F18.0, " P.S.I."/),               0075
              FOR69("DEFLECTION AT THE CENTER OF THE SPAN = ", F5.2,        0076
              " INCHES "// "ALLOWABLE DEFLECTION = ", F21.2," INCHES"/);    0077

LIST          DAT1A(NUM, LL, WIND, DL, NCONC, LENTH),
              DAT1B(MULL, MULW, MULD, MURL, MURW, MURD),
              DAT1C(SC, SS, AGSZ, WIDTH, DEPTH, CH, CV, TRLD),
              DATA2(COLL[I], COW[I], CODL[I], DIST[I]),                     0080
              OUT50(NUM, WIDTH, DEPTH),                                     0081
              OUT51(NBAR, UPDN1, SIDE, SBD, COV1, UPDN1, SPA1, COH1),       0082
              OUT51A(NBOT),                                                 0083
              OUT51B(REQB),                                                 0084
              OUT53(PTF1[J], PTI1[J], PTF2[J], PTI2[J]),                    0085
              OUT54(COV1, SPLIC),                                           0086
              OUT55(UPDN1, SIDE, NADL, SBD),                                0087
              OUT56(NST, UPDN1, SIDE, SBD),                                 0088
              OUT57(NDLB, SBD, SIDE, CTF, CTI),                             0A88
              OUT59(NXTNT, UPDN1, CUTF, CUTI, SIDE),                        0089
              OUT60(NXTND, UPDN1, EXTF, EXTN, SIDE),                        0090
              OUT61(REQB, UPDN1, SIDE),                                     0091
              OUT63(SIDE, ANCT),                                            0A91
              OUT64(FT[I], INCH[I]),                                        0092
              OUT64A(SBDS),                                                 0093
              OUT66(SIDE),                                                  0094
              OUT67(CNSTB, STSTB, BOND, SHEAR),                             0095
              OUT68(STTOP, BOND),                                           0096
              OUT69(DEFF, ALDE) ;                                           0097

PROCEDURE     BDIAM ;                                                       0098

         BEGIN                                                              0099
COMMENT       THIS PROCEDURE CALCULATES THE DIAMETER OF STEEL BARS GIVEN    0100
              THE STANDARD BAR DESIGNATION NUMBER. ;                        0101
              DIAM ~ SBD | 0.125 ;                                          0102
              IF SBD > 8 THEN                                               0103
         BEGIN                                                              0104
              IF SBD = 9 THEN DIAM ~ DIAM + 0.005                           0105
              ELSE IF SBD = 10 THEN DIAM ~ DIAM + 0.02                      0106
              ELSE DIAM ~ DIAM + 0.035                                      0107
         END                                                                0108
         END BDIAM ;                                                        0109

PROCEDURE     REACM ;                                                       0110

         BEGIN                                                               112
REAL          REAC1, REAC2 ;                                                0111
LABEL         L1, L1A ;                                                     0112

COMMENT       THIS PROCEDURE CALCULATES THE REACTIONS AND MAXIMUM           0113
              POSITIVE MOMENT.;                                             0114
              IF TRLD THEN KTC ~ 0.25
              ELSE KTC ~ 0.5 ;
              REAC1 ~ -KTC | DISLD | LENTH ;      REAC2 ~ 0 ;               0115
              FOR I ~ 1 STEP 1 UNTIL NCONC DO                               0116
              REAC2 ~ (DIST[I] - LENTH) | CONLD[I] + REAC2 ;                0117
              REAC2 ~ REAC2 / LENTH ;                                       0118
              SHRF ~ REAC ~ REAC1 + REAC2 + REAC3 ;                         0119

COMMENT       FIND THE POINT OF ZERO SHEAR THAT CORRESPONDS TO THE          0120
              POINT OF MAXIMUM MOMENT. ;                                    0121
              I ~ 1 ;           KTX ~ 0 ;                                   0122
              KTA ~ 2 | DISLD / LENTH ;     KTB ~ 0.5 | LENTH ;             0123
              FOR DIS ~ 0 STEP INC WHILE SHRF > 0 DO                        0124
         BEGIN
              INC ~ SHRF / REAC ;                                           0125
              IF INC < 0.1 THEN INC ~ 0.1 ;                                 0126
              IF (I { NCONC) AND (DIST[I] { DIS) THEN                       0128
         BEGIN                                                              0129
              SHRF ~ SHRF + CONLD[I] ;                                      0130
              IF SHRF { 0 THEN
         BEGIN
              DIS ~ DIST[I] ;      GO TO L1A
         END ;
              I ~ I + 1                                                     0131
         END ;                                                              0132
              IF TRLD THEN                                                  0133
         BEGIN                                                              0134
              IF DIS > KTB THEN KTT ~ DISLD | ( 1.0 - (DIS - KTB) / KTB)    0135
              ELSE KTT ~ DIS | KTA ;                                        0136
              SHRF ~ SHRF + 0.5 | (KTX + KTT) | INC                         0137
         END                                                                0138
              ELSE SHRF ~ SHRF + DISLD | INC ;                              0139
              KTX ~ KTT                                                     0140
         END ;                                                              0141

L1A:          IF TRLD THEN                                                  0142
         BEGIN                                                              0143
              MMAX ~ REAC | DIS + 0.1666 | KTA | (DIS*3) ;                  0144
              IF DIS > KTB THEN                                             0145
              MMAX ~ MMAX - 0.333 | (DIS | KTA - DISLD) | (DIS - KTB)*2     0146
         END                                                                0147
              ELSE  MMAX ~ REAC | DIS + 0.5 | DISLD | DIS*2 ;               0148
              FOR J ~ 1 STEP 1 WHILE DIST[J] { DIS DO                       0149
         BEGIN                                                              0150
              IF J > NCONC THEN GO TO L1 ;                                  0151
              MMAX ~ MMAX + CONLD[J] | (DIS - DIST[J]) ;     J ~ J + 1      0152
         END ;                                                              0153

L1:      END REACM ;                                                        0154

PROCEDURE     ULTLD ;                                                       0156

         BEGIN                                                              0157

COMMENT       ULTIMATE LOADS ARE CALCULATED BY MULTIPLYING THE LOADS BY     0158
              THE LOAD FACTORS GIVEN IN A.C.I. CODE (A604). ;               0159
              UD ~  1.2 | DDL + 2.4 | LLL + 0.6 | WINDL ;                   0161
              KTB ~ 1.2 | DDL + 0.6 | LLL + 2.4 | WINDL ;                   0163
              IF ABS(KTB) > ABS(UD) THEN UD ~ KTB ;                         0164
              KTC ~ (DDL + LLL + 0.5 | WINDL) | K ;                         0165
              IF ABS(KTC) > ABS(UD) THEN UD ~ KTC ;                         0166
              KTT ~ (DDL + 0.5 | LLL + WINDL) | K ;                         0167
              IF ABS(KTT) > ABS(UD) THEN UD ~ KTT                           0168
         END ULTLD ;                                                        0169

PROCEDURE     DOBLR ;                                                       0282

         BEGIN                                                              0283

COMMENT       COMPUTATION OF AREA OF STEEL REQUIRED AT TOP AND BOTTOM OF    0284
              DOUBLE REINFORCED BEAMS. ;                                    0285
REAL          MULS, MRES ;                                                  0286
              MULS ~ 0.306 | KTA / 12.0 ;     MRES ~ MU - MULS ;            0287
              APS ~ 12.0 | MRES / (SS | (2.0 | DU - TU)) ;                  0288
              AS ~ (0.4 | SC / SS) | BU | DU                                0289
         END DOBLR ;

PROCEDURE     ULSTR ;                                                       0170

         BEGIN                                                              0172
REAL          Q1, Q2, DIF1, DIF2 ;                                          0171
LABEL         L11, L12 ;                                                    0173

COMMENT       CALCULATION OF AREA OF STEEL NEEDED TO RESIST THE             0174
              ULTIMATE MOMENT. ;                                            0175
              KTA ~ BU | DU*2 | SC ; KTB ~ 12.0 | MU / KTA ;                0176
              IF KTB > 0.306 THEN GO TO L11 ;                               0177
              ROOT ~ SQRT(1.0 - 2.36 | KTB) ;                               0178
              Q1 ~ (1.0 - ROOT) / 1.18 ; Q2 ~ (1.0 + ROOT) / 1.18 ;         0179
              DIF1 ~ ABS(MU - (KTA | Q1 | (1.0 - 0.59 | Q1) / 12.0)) ;      0180
              DIF2 ~ ABS(MU - (KTA | Q2 | (1.0 - 0.59 | Q2) / 12.0)) ;      0181
              IF DIF1 < DIF2 THEN Q ~ Q1                                    0183
              ELSE Q ~ Q2 ;                                                 0184
              IF Q > 0.4 THEN GO TO L11 ;                                   0185
              P ~ Q | SC / SS ;                                             0186

COMMENT       A.C.I-702-E-1. WHEN POSITIVE REINFORCEMENT IS INDICATED       0187
              BY ANALYSIS, THE AMOUNT PROVIDED MUST BE NOT LESS THAN        0188
              0.005 TIMES THE SECTION AREA. ;                               0189
              IF (P < 0.005) AND POS THEN P ~ 0.005 ;                       0190
              AS ~ P | BU | DU ; GO TO L12 ;                                0191

L11:          DOBLR  ;                                                      0192

L12:     END ULSTR ;                                                        0193

PROCEDURE     BARDS ;                                                       0224

         BEGIN                                                              0226
REAL          SPAC1, CR ;                                                   0225

COMMENT       THE NUMBER OF BARS NEEDED TO OBTAIN NECESSARY AREA OF         0227
              REINFORCEMENT AND BAR SPACING ARE CALCULATED. ;               0228
              ABAR ~ DIAM*2 | 0.785 ; NBARS ~ AS / ABAR ;                   0229
              IF TRY THEN NBARS ~ NBARS | 0.5 ;                             0230
              NBAR ~ ENTIER(NBARS) + 1.0 ;                                  0231

COMMENT       A.C.I. -505-A. THE CLEAR DISTANCE BETWEEN BARS SHALL BE       0232
              NOT LESS THAN THE NOMINAL BAR DIAMETER, 1.33 TIMES THE        0233
              MAXIMUM SIZE OF COARSE AGGREGATE OR 1 IN.  THE CLEAR          0234
              DISTANCE BETWEEN ROWS SHALL BE 1 IN. AT LEAST ;               0235
              SPAC ~ 1.33 | AGSZ  ;                                         0236
              IF SPAC < DIAM THEN SPAC ~ DIAM ;                             0237
              IF SPAC < 1.0 THEN SPAC ~ 1.0 ;                               0238
COMMENT       IF THE DESIGNER HAS NOT FIXED THE MINIMUM CONCRETE COVER      0239

              2 INCHES OF CONCRETE COVER IS PLACED ON EACH SIDE  IT IS      0240
              INITIALLY ASSUMED THAT NUMBER 4 BARS ARE USED AS STIRRUPS,    0241
              TO FIND THE MINIMUM REQUIRED WIDTH OF SECTION. ;              0242
              IF CH = 0 THEN CR ~ 5.0                                       0243
              ELSE  CR ~ CH | 2.0 + 1.0 ;                                   0244
              REQB ~ (NBAR + 1.0) | DIAM + (NBAR - 1.0) | SPAC + CR         0245
         END BARDS ;                                                        0246

PROCEDURE     BONST ;                                                       0194

         BEGIN                                                              0196
LABEL         L2, L3, L4 ;                                                  0195
REAL          NNPER, UNAR, UNPER, RQPR ;                                    0197

COMMENT       THIS PROCEDURE CALCULATES THE SIZE OF BARS THAT GIVE AN       0198
              ADEQUATE PERIMETER AND AREA OF STEEL WITH A MINIMUM NUMBER    0199
              OF BARS ;                                                     0200
              IF SKIP THEN GO TO L2 ;                                       0201
              PER ~ SHRMX / (0.875 | ALBON | DU) ;                          0202
              IF POS THEN PER ~ 2.0 | PER ;                                 0203

L2:           SBD ~ 12 ;      UNPER ~ 4.43 ;      UNAR ~ 1.56 ;             0203
              NNPER ~ PER / UNPER ;      NBARS ~ AS / UNAR ;                0207
              IF NNPER > NBARS THEN NBARS ~ NNPER ;                         0208
              NBAR ~ ENTIER(NBARS) + 1.0 ;                                  0209
              IF NBAR < 2 THEN NBAR ~ 2 ;                                   0210
              RQPR ~ PER / NBAR ;     ABAR ~ AS / NBAR ;                    0211

L3:           SBD ~ SBD - 1 ;                                               0212
              BDIAM ;                                                       0213
              UNPER ~ 3.14 | DIAM ;      UNAR ~ DIAM*2 | 0.785 ;            0214
              IF ((SBD > 4) AND (UNPER > RQPR) AND (UNAR > ABAR)) THEN      0215
              GO TO L3 ;                                                    0216
              SBD ~ SBD + 1  ;                                              0219
              BDIAM ;                                                       0220
              KTE ~ NBAR | 0.785 | DIAM*2 ;
              IF AS < KTE THEN AS ~ KTE ;
              IF SKIP THEN GO TO L4 ;                                       0221
              BARDS ;                                                       0222

L4:      END BONST ;                                                        0223

PROCEDURE     SHL ;                                                         0260

         BEGIN                                                              0261

COMMENT       THIS PROCEDURE CALCULATES THE SHEAR FORCE ACTING AT           0262
              DIFFERENT SECTIONS ALONG THE SPAN OF THE BEAM. ;              0263
              SHRF ~ DISLD | SPAST + SHRF ;                                 0264
              IF (J { NCONC) AND (DIST[J] { Y) AND (DIST[J] ! 0) THEN       0265
         BEGIN                                                              0266
              SHRF ~ CONLD[J] + SHRF ;     J ~ J + 1                        0267
         END ;                                                              0268
              SHRS ~ ABS(SHRF) - SHRCN                                      0269
         END SHL ;                                                          0270

PROCEDURE     STIRP ;                                                       0271

         BEGIN                                                              0272

COMMENT       THE POSITION OF EACH STIRRUP IS GIVEN BY ITS DISTANCE FROM    0273
              ITS LEFT SUPPORT. ;                                           0274
              FT[I] ~ ENTIER(Y) ;                                           0275
              INCH[I] ~ ENTIER(24.0 | (Y - FT[I])) | 0.5 + 1.0 ;            0276
              IF INCH[I] > 11.0 THEN                                        0277
         BEGIN                                                              0278
              INCH[I] ~ 0 ;     FT[I] ~ FT[I] + 1.0                         0279
         END                                                                0280
         END ;                                                              0281

L103:         READ(FILE1, F1A, DAT1A)[L140] ;
              READ(FILE1, F1B, DAT1B)[L140] ;
              READ(FILE1, F1C, DAT1C)[L140] ;
              TRLD ~ FALSE ;                                                0284
         BEGIN

LABEL         L104A, L105A, L105B, L105, L106 ;
              K ~ 1.8 ;                                                     0290
              IF SC } 3.5 THEN ALBON ~ 0.35                                 0292
              ELSE ALBON ~ 0.1 | SC ;                                       0293
              IF SC } 3.0 THEN ALSHR ~ 0.24                                 0294
              ELSE ALSHR ~ 0.08 | SC ;                                      0295
              FOR I ~ 1 STEP 1 UNTIL NCONC DO                               0303
         BEGIN                                                              0304
              READ(FILE1, F2A, DATA2)[:L140] ;
              LLL ~ COLL[I] ;     DDL ~ CODL[I]    ; WINDL ~ COW[I] ;       0306
              ULTLD ;                                                       0307
              CONLD[I] ~ UD                                                 0308
         END ;                                                              0309
              ML ~ MULL + MULW + MULD ;     MR ~ MURL + MURW + MURD ;       0296
              LLL ~ MULL ;     DDL ~ MULD ;     WINDL ~ MULW ;              0311
              ULTLD ;                                                       0312
              MUL ~ UD ;                                                    0313
              LLL ~ MURL ;     DDL ~ MURD ;     WINDL ~ MURW ;              0314
              ULTLD ;                                                       0315
              MUR ~ UD ;                                                    0316
              SIDE ~ "CENTER" ;     ABOT ~ 0 ;      TRY ~ FALSE ;           0297
              BU ~ WIDTH ;     TU ~ DEPTH ;     DU ~ TU - 3.0 ;             0298
              WT ~ -0.00104 | BU | TU ;                                     0299
              LLL ~ LL ;     DDL ~ DL + WT ;     WINDL ~ WIND ;             0300
              ULTLD ;                                                       0301
              DISLD ~ UD ;                                                  0302

COMMENT       REAC3 ARE THE REACTIONS THAT BALANCE THE DIFFERENCE IN        0317
              END MOMENTS. ;                                                0318
              REAC3 ~ (MUL - MUR) / LENTH ;                                 0319
              POS ~ TRUE ;     REACM ;                                      0321
              MU ~ MP ~ MMAX - MUL ;                                        0322

COMMENT       THE BEAM IS ASSUMED TO BE SINGLE REINFORCED IN THE            0323
              CENTRAL PORTION. ;                                            0324
              WRITE(FILE2[PAGE]) ;                                          A324
              WRITE(FILE2, FOR50, OUT50) ;                                  A325
              IF MP < 0 THEN WRITE(FILE2[PAGE], DEF50) ;                    B325
              ULSTR ;                                                       0326

COMMENT       THE SIZE OF BARS IS OBTAINED SO THAT THE BOND STRESS IS       0327
              WITHIN THE ALLOWABLE VALUE.  THIS STRESS IS STRESS IS         0328
              CALCULATED AT THE THEORETICAL CUT-OFF POINTS OF THE BARS.;    0329
              KTA ~ MUL ;      KTB ~ MUR ;                                  0330
              DISR ~ DIS | (1.0 - SQRT(MP / (MP + KTA))) ;                  A330
              DISRT ~ DIS + (LENTH - DIS) | (SQRT(MP / (MP + KTB))) ;       B330

L104A:        DISLD ~ LL + WIND + DL + WT ;     TLOAD ~ DISLD | LENTH ;     0331
              FOR I ~ 1 STEP 1 UNTIL NCONC DO                               0332
         BEGIN                                                              0333
              CONLD[I] ~ COLL[I] + COW[I] + CODL[I] ;                       0334
              TLOAD ~ CONLD[I] + TLOAD                                      0335
         END ;                                                              0336
              REAC3 ~ (ML - MR) / LENTH ;                                   0337
              REACM ;                                                       0338
              REAB ~ -(TLOAD + REAC) ;                                      0339
              IF TRLD THEN                                                  A340
         BEGIN                                                              B340
              KTE ~ 2 | DISLD / LENTH ;     KTA ~ 0.5 | KTE | DISR          C340
         END                                                                D340
              ELSE KTA ~ DISLD ;                                            E340
              SHRL ~ REAC + KTA | DISR ;                                    F340
              FOR I ~ 1 STEP 1 WHILE DIST[I] { DISR DO                      0341
         BEGIN                                                              0342
              IF I > NCONC THEN GO TO L105A ;                               0343
              SHRL ~ CONLD[I] + SHRL                                        0344
         END ;                                                              0345

L105A:        IF TRLD THEN                                                  A346
         BEGIN                                                              B346
              KTT ~ 0.5 | KTE | LENTH ;     KTB ~ 0.5 | LENTH ;             C346
              SHRR ~ SHRL + 0.5 | ((KTT + 2 | KTA) | (KTB - DISR) +         D346
              (2 | KTT - (DISRT - KTB) | KTE) | (DISRT - KTB))              E346
         END                                                                F346
              ELSE SHRR ~ SHRL + DISLD | (DISRT - DISR) ;                   G346
              FOR I ~ I STEP 1 WHILE DIST[I] { DISRT DO                     0347
         BEGIN                                                              0348
              IF I > NCONC THEN GO TO L105B ;                               0349
              SHRR ~ CONLD[I] + SHRR                                        0350
         END ;                                                              0351

L105B:        IF ABS(SHRL) > ABS(SHRR) THEN SHRMX ~ ABS(SHRL)               0352
              ELSE SHRMX ~ ABS(SHRR) ;                                      0353
              BONST ;                                                       0354

L105:         IF (REQB - WIDTH) > 0.25 THEN                                 0356
         BEGIN                                                              0357
              IF SBD < 11 THEN
         BEGIN
              SBD ~ SBD + 1  ;
              BDIAM ;
              BARDS ;
              GO TO L105
         END ;
              WRITE(FILE2, DEF51, OUT51B) ;                                 0358
              BU ~ WIDTH ~ ENTIER(REQB) + 1.0                               0359
         END;                                                               0360
              NBAR1 ~ NBAR ;     DIAM1 ~ DIAM ;     SBD1 ~ SBD ;            0361
              MP ~ MMAX - ML ;                                              0363

COMMENT       IF THE MINIMUM COVER IS NOT SPECIFIED, IT IS ASSUMED THAT     0364
              NUMBER 4 BARS ARE USED AS STIRRUPS AND 2 IN. OF COVER         0365
              ARE LEFT ON EACH SIDE. ;                                      0366
              IF CH = 0 THEN CH ~ 2.0 ;                                     0367
              COH1 ~ CH + (1.0 + DIAM1) | 0.5 ;                             0368
              SPA1 ~ (BU - 2.0 | COH1) / (NBAR1 - 1.0) ;                    0369
              IF CV ! 0 THEN CV ~ CV                                        0370
              ELSE IF SBD = 6 THEN CV ~ 1.5                                 0371
              ELSE CV ~ 2.0 ;                                               0372
              COV1 ~ CV + (1.0 + DIAM1) | 0.5 ;     DU ~ TU - COV1 ;        0373
              UPDN1 ~ "BOTTOM" ;
              WRITE(FILE2, FOR51, OUT51) ;                                  0374

COMMENT       A.C.I. 902-B.  OF THE POSITIVE REINFORCEMENT IN CONTINUOUS    0375
              BEAMS AT LEAST ONE-FOURTH OF THE AREA SHALL BE EXTENDED       0376
              ALONG THE SAME FACE OF THE BEAM INTO THE SUPPORTS             0377
              A DISTANCE OF 6 IN.  IF BOND STRESS AT THE SUPPORTS           0378
              EXCEEDS THE ALLOWABLE A LARGER NUMBER OF BARS IS EXTENDED;    0379
              NBOT ~ ENTIER(0.24 | NBAR) + 1.0 ;                            0380
              IF NBOT < 2 THEN NBOT ~ 2 ;                                   0381
              BOND ~ 0.364 | SHRMX / (DU | NBOT | DIAM1) ;                  0382
              FOR NBOT ~ NBOT STEP 1 WHILE BOND > ALBON DO                  0383
         BEGIN                                                              0384
              IF NBOT > NBAR1 THEN                                          0385
         BEGIN                                                              0386
              WRITE(FILE2, DEF53) ;     SBD ~ SBD - 2 ;                     0387
              BDIAM ;                                                       0388
              BARDS ;                                                       0389
              GO TO L105                                                    0390
         END ;                                                              0391
              BOND ~ BOND | (NBOT - 1) / NBOT                               A391
         END ;                                                              B391
              ABAR ~ DIAM1*2 | 0.785 ;     NBEND ~ NBAR1 - NBOT ;           0392
              ABOT ~ NBOT | ABAR ;     ATOP ~ NBEND | ABAR ;                0393
              REQQ ~ (DIAM + SPAC) | NBEND ;                                0394
              REBB ~ (DIAM + SPAC) | NBOT ;                                 0395
              IF DIAM1 < 1.0 THEN SPLIC ~ 12.0                              0396
              ELSE SPLIC ~ ENTIER (DIAM1 | 12.0) + 1 ;

COMMENT       BARS ARE EXTENDE 12 BAR DIAMETERS BEYOND THE THEORETICAL      0398
              CUT-OFF POINT BEFORE BENDING THEM TO A 45 DEGREE ANGLE. ;     0399
              IF NBEND { 0 THEN GO TO L106 ;                                0400
              WRITE(FILE2, TIT53) ;                                         0401
              KTA ~ SQRT(NBAR1) ;                                           0402
              KTE ~ (DIS - DISR) / KTA ;     KTT ~ (DISRT - DIS) / KTA ;    0403
              FOR J ~ 1 STEP 1 UNTIL NBEND DO                               0404
         BEGIN                                                              0405
              KTB ~ DIS - (SQRT(J) | KTE + DIAM1) ;                         0406
              PTI1[J] ~ (KTB - ENTIER(KTB)) | 12.0 ;                        0408
              IF PTI1[J] > 11.0 THEN
         BEGIN
              KTB ~ KTB + 1 ;      PTI1[J] ~ 0
         END ;
              PTF1[J] ~ ENTIER(KTB) ;                                       0407
              KTB ~ LENTH - DIS - (SQRT(J) | KTT + DIAM1) ;                 0409
              PTI2[J] ~ (KTB - ENTIER(KTB)) | 12.0 ;                        0411
              IF PTI2[J] > 11.0 THEN
         BEGIN
              KTB ~ KTB + 1 ;      PTI2[J] ~ 0
         END ;
              PTF2[J] ~ ENTIER(KTB) ;                                       0410
              WRITE(FILE2, FOR53, OUT53)                                    0412
         END ;                                                              0413

              WRITE(FILE2, FOR54, OUT54) ;                                  0414
L106:         WRITE(FILE2, FOR51A, OUT51A) ;                                0415
              SHEAR ~ 1000.0 | SHRMX / (0.875 | BU | DU) ;                  0416
              BOND ~ 1000.0 | BOND ;                                        0417
              N ~ 30.0 / SC  ;     KTE ~ N | NBAR | ABAR ;                  0418
              IF TRY THEN KTE ~ 2.0 | KTE  ;                                0419
              ROOT ~ SQRT(KTE*2 + 2.0 | KTE | BU | DU) ;                    0420
              KD ~ (-KTE + ROOT) / BU ;     JD ~ DU - 0.333 | KD ;          0421
              TT ~ 12000.0 | MP / JD ;     STSTB ~ TT / (NBAR | ABAR) ;     0422
              IF TRY THEN STSTB ~ 0.5 | STSTB ;                             0423
              CNSTB ~ 2 | TT / (KD | BU) ;     DIS ~ 0.5 | LENTH ;          0424

COMMENT       DEFLECTION ANALYSIS AT THE CENTER OF THE BEAM. ;
              DEFC ~ 0 ;     TU1 ~ TU ;                                     0425
              IN1 ~ DU*3 | BU / 12.0 ;     KTE ~ IN1 | LENTH / 1728.0 ;
              FOR I ~ 1 STEP 1 UNTIL NCONC DO                               0427
         BEGIN                                                              0428
              KTX ~ COLL[I] + COW[I] ;                                      0429
              COMP ~ LENTH - DIST[I] ;                                      0430
              KTY ~ KTX / (1000.0 | SC) + CODL[I] / 750.0 ;                 A430
              IF DIS < DIST[I] THEN                                         0431
         BEGIN                                                              0432
              KTB ~ LENTH*2 - DIS*2 - COMP*2 ;                              0433
              DEFC ~ KTY | COMP | DIS | KTB / 6.0 + DEFC                    0434
         END                                                                0436
              ELSE IF DIS > DIST[I] THEN                                    0437
         BEGIN                                                              0438
              KTB ~ LENTH | ((DIS - DIST[I])*3) / COMP +                    0439
              (LENTH*2 - COMP*2) | DIS - DIS*3 ;                            0440
              DEFC ~ KTY | COMP  | KTB / 6.0 + DEFC                         0441
         END                                                                A441
              ELSE DEFC ~ KTY | ((COMP | DIST[I])*2) / 3.0 + DEFC           0443
         END ;                                                              0444
              DEFC ~ DEFC / KTE ;                                           0447

COMMENT       A REDUCED MODULES OF ELASTICITY OF 750000 P.S.I., IS USED     0448
              TO CALCULATE THE DEFLECTION PRODUCED BY DEAD LOADS. ;         0449
              KTC ~ 72.0 | DIS | (LENTH*3 - 2.0 | LENTH | DIS*2 +           0450
              DIS*3) / IN1 ;                                                0451
              DEFD ~ ((DL + WT) / 750.0 +
              (LL + WIND) / (1000.0 |SC)) | KTC ;                           0453
              SM ~ ABS((MULL + MULW - MURL - MURW) / (1000.0 | SC) +        0454
              (MULD - MURD) / 750.0) ;                                      0455
              DEFM ~ 288.0 | SM | (LENTH - DIS) | (2.0 | LENTH | (LENTH-    0455
              DIS) + DIS*2) / (LENTH | IN1) ;                               0456
              DEFF ~ ABS(DEFC + DEFD + DEFM) ;                              0457
              ALDE ~ ((12.0 | LENTH)*2) / (10000.0 | TU) ;                  0458
              WRITE(FILE2, FOR65) ;                                         0459
              WRITE(FILE2, FOR67, OUT67) ;                                  0460
              WRITE(FILE2, FOR69, OUT69)                                    0461
         END ;
         BEGIN

LABEL         L112, L109, L110, L110B, L110C, L110E, L110G, L114, L115,
              L115A, L128,
              L125, L127, L129, L129A,L130, L130A, L21, L21A, L22 ;

COMMENT       DESIGN REINFORCEMENT AT THE SUPPORTS.                         0462
              SHOULD DOUBLE REINFORCEMENT BE REQUIRED IT IS CALCULATED      0463
              BY EQUATION (A3) OF A.C.I. CODE. ;                            0464

L112:         SHRMX ~ REAC ;     SIDE ~ " LEFT" ;                           0467

L109:         POS ~ TIME ~ FALSE ;           MU ~ MUL ;                     0468

L110:         DU ~ TU - COV1 ;     UPDN1 ~ "   TOP" ;                       0469

COMMENT       CALCULATION OF SIZE AND NUMBER OF ADDITIONAL BARS NEEDED      0471
              AT THE SUPPORTS. ;                                            0472
              TERM1 ~ 0.59 | SS / (SC | BU | DU) ;                          0466
              KT1 ~ (SS - 0.85 | SC) | (DU - COV1) ;                        0474
              TERM3 ~ (12 | MU - ABOT | KT1) / (SS | DU) ;                  0476
              TMP ~ SKIP ~ FALSE ;     AS ~ 0 ;                             0477

L110B:        KTE ~ TERM1 | AS*2 - AS + TERM3 ;                             0478
              IF SKIP THEN GO TO L110C ;                                    0479
              IF KTE < 0 THEN TMP ~ TRUE ;                                  0480
              SKIP ~ TRUE ;                                                 0481

L110C:        IF TMP THEN                                                   0482
         BEGIN                                                              0483
              IF KTE > 0 THEN GO TO L110E ;                                 0484
              AS ~ AS - 0.1                                                 0485
         END                                                                0486
              ELSE                                                          0487
         BEGIN                                                              0488
              IF KTE < 0 THEN GO TO L110E ;                                 0489
              AS ~ AS + 0.1                                                 0490
         END ;                                                              0491
              GO TO L110B ;                                                 0492

L110E:        AS ~ AS + ABOT - ATOP ;                                       0493
              IF AS < 0 THEN AS ~ 0 ;                                       0478

L110G:        APS ~ CT ~ 0 ;     TRY ~FALSE ;                               A478

COMMENT       THE ALLOWABLE BOND STRESS OF TOP BARS WHEN MORE THAN          0479
              12 INCHES OF CONCRETE IS CAST BELOW THEM IS 7 PERCENT         0480
              OF THE STRENGTH OF THE CONCRETE. ;                            0481
              ALCBN ~ 0.7 | ALBON ;                                         0482
              PER ~ SHRMX / (0.875 | ALCBN | DU) - NBEND | (3.14 |          0483
              DIAM1) ;                                                      0484
              NADL ~ 0 ;     NST ~ NBAR1 - NBEND ;                          0485

COMMENT       ADDITIONAL TOP STRAIGHT BARS MAY BE NEEDED TO REDUCE          0486
              THE BENDING AND/OR BOND STRESSES AT THE SUPPORTS.  TOP        0487
              STRAIGHT BARS ARE PLACED IN THE SAME POSITION THAT THE        0488
              BOTTOM BARS THAT WERE EXTENDED TO THE SUPPORTS.  TWO ROWS     0489
              OF TOP BARS MAY BE NECESSARY IF WIDTH IS INSUFFICIENT. ;      0490

L114:         SKIP ~ TRUE ;                                                 0491
              BONST ;                                                       0492
              SKIP ~ FALSE ;                                                0493

L115:         IF TRY THEN REQ ~ 0                                           0494
              ELSE REQ ~ REQQ ;                                             0495
              NADL ~ NBAR - NST ;
              IF NADL > 0 THEN                                              A496
         BEGIN                                                              B496
              NBAR ~ NADL ;     TRY ~ TRUE ;                                C496

L115A:        BARDS ;                                                       D496
              REQB ~ REQB + REQ ;                                           0497
              IF REQB > BU THEN                                             0498
         BEGIN                                                              0499
              IF SBD < 11 THEN                                              0500
         BEGIN                                                              0501
              SBD ~ SBD + 1 ;                                               0502
              BDIAM ;                                                       0503
              GO TO L115A                                                   0504
         END ;                                                              0505
              WRITE(FILE2, DEF61, OUT61)
         END                                                                A509
         END ;                                                              0510
              FD ~ DIAM ;                                                   0516
              IF CV ! 0 THEN CV ~ CV                                        0517
              ELSE IF SBD < 6 THEN CV ~ 1.5                                 0518
              ELSE CV ~ 2.0 ;                                               0519
              COV ~ CV + 0.5 | DIAM ;     DU ~ TU - COV ;                   0520
              WRITE(FILE2, FOR56, OUT56) ;                                  0522
              IF NADL > 0 THEN
         BEGIN
              DU ~ DU - 0.5 | (DIAM + 1.0) ;
              WRITE(FILE2, FOR55, OUT55)
         END ;
              WRITE(FILE2, TIT56) ;                                         0524

COMMENT       A.C.I. 902-A. TENSILE NEGATIVE REINFORCEMENT OF A             0525
              CONTINUOUS BEAM SHOULD BE A DEQUATELY ANCHORED BY BOND.       0526
              EVERY BAR SHALL BE EXTENDED AT LEAST 12 BAR DIAMETERS         0527
              BEYOND THE POINT AT WHICH THEY ARE NOT LONGER NEEDED. ;       0528
              IF TIME THEN CUTI ~ LENTH - DISRT + DIAM                      0529
              ELSE CUTI ~ DISR + DIAM ;                                     0530
              CUTF ~ ENTIER(CUTI) ; CUTI ~ (CUTI - CUTF) | 12.0 ;           0531
              IF CUTI > 11.0 THEN
         BEGIN
              CUTF ~ CUTF + 1 ;      CUTI ~ 0
         END ;

COMMENT       A.C.I. 902-A. AT LEAST LEAST ONE-THIRD OF THE TOTAL           0532
              REINFORCEMENT PROVIDED FOR NEGATIVE MOMENT AT THE SUPPORTS    0533
              SHALL BE EXTENDED BEYOND THE INFLECTION POINT A DISTANCE      0534
              SUFFICIENT TO DEVELOP BY BOND ONE-HALF THE ALLOWABLE BOND     0535
              STRESS, NOT LESS THAN 1/16 OF THE CLEAR SPAN NOR LESS THAN    0536
              THE DEPTH OF THE MEMBER. ;                                    0537
              NXTND ~ ENTIER((NADL + NST + NBEND) | 0.33) + 1.0 -NBEND;     0538
              IF NXTND { 0 THEN GO TO L128 ;                                0539
              EXTN ~ LENTH | 0.0625 ;     EXTN1 ~ DEPTH | 0.0833 ;          0540
              IF EXTN1 > EXTN THEN EXTN ~ EXTN1 ;                           0541
              IF TIME THEN EXTN ~ DISRT - EXTN                              0542
              ELSE EXTN ~ DISR + EXTN ;                                     0543
              SHRF ~ DISLD | EXTN + REAC ;                                  0544
              FOR I ~ 1 STEP 1 WHILE DIST[I] < EXTN DO                      0545
         BEGIN                                                              0546
              IF I > NCONC THEN GO TO L125 ;                                0547
              SHRF ~ CONLD[I] + SHRF                                        0548
         END ;                                                              0549

L125:         BOND ~ ABS(0.364 | SHRF / (DU | NXTND | DIAM)) ;              0550
              IF BOND > (0.5 | ALBON) THEN                                  0551
         BEGIN                                                              0552
              IF TIME THEN                                                  0553
         BEGIN                                                              0554
              INT ~ -0.2 ;     I ~ I - 1 ;     J ~ -1                       0555
         END                                                                0556
              ELSE                                                          0557
         BEGIN                                                              0558
              INT ~ 0.2 ;     J ~ 1                                         0559
         END ;                                                              0560
              FOR EXTN ~ EXTN STEP INT WHILE BOND > (0.5 | ALBON) DO        0561
         BEGIN                                                              0562
              SHRF1 ~ SHRF ;                                                0563
              IF (EXTN < 0) OR (EXTN > LENTH) THEN GO TO L127 ;             0564
              EXTN ~ EXTN + INT ;                                           1564
              SHRF ~ INT | DISLD + SHRF ;                                   0565
              IF ((DIST[I] > EXTN) AND TIME) OR ((DIST[I] < EXTN) AND       0566
              NOT TIME) THEN                                                0567
         BEGIN                                                              0568
              SHRF ~ CONLD[I] + SHRF ;     I ~ J + I                        0569
         END ;                                                              0570
              BOND ~ SHRF | BOND / SHRF1                                    0571
         END                                                                0572
         END ;                                                              0573

L127:         IF TIME THEN EXTN ~ LENTH - EXTN ;                            0574
              EXTF ~ ENTIER(EXTN) ;     EXTN ~ (EXTN - EXTF) | 12.0 ;       0575
              IF EXTN > 11.0 THEN
         BEGIN
              EXTF ~ EXTF + 1 ;      EXTN ~ 0
         END ;
              WRITE(FILE2, FOR60, OUT60) ;                                  0576

L128:         NXTNT ~ NADL + NST - NXTND ;                                  0577
              WRITE(FILE2, FOR60, OUT59) ;                                  0578

COMMENT       DESIGN OF NEGATIVE COMPRESSION REINFORCEMENT.                 0579
              IF THE BARS EXTENDED FROM THE CENTER TO THE SUPPORTS GIVE     0580
              MORE AREA OF STEEL THAN NEEDED, ADDITIONAL BOTTOM BARS ARE    0581
              NOT REQUIRED. ;                                               0582
              ANCT ~ NDLB ~ 0 ;    AS ~ APS ~ APS - ABOT ;                  0583
              PER ~ (SHRMX / (0.875 | ALBON | DU)) - NBOT | DIAM1 |3.14;    0585
              IF (APS > 0) OR (PER > 0) THEN                                0587
         BEGIN                                                              0588
              UPDN1 ~ "BOTTOM";      SKIP ~ TRUE ;                          0589
              BONST ;                                                       0590
              SKIP ~ FALSE ;                                                0591

L129:         BARDS ;                                                       0592
              REQB ~ REBB + REQB ;                                          0593
              IF REQB > WIDTH THEN                                          0594
         BEGIN                                                              0595
              IF SBD < 11 THEN                                              0596
         BEGIN                                                              0597
              SBD ~ SBD + 1 ;                                               0598
              BDIAM ;                                                       0599
              GO TO L129                                                    0600
         END
              ELSE WRITE(FILE2, DEF61, OUT61)                               0602
         END ;                                                              0603
              IF TIME THEN CTI ~ LENTH - DISRT
              ELSE CTI ~ DISR ;                                             0605
              CTI ~ CTI + DIAM ;                                            0606
              CTF ~ ENTIER(CTI) ;     CTI ~ (CTI - CTF) | 12.0 ;            0607
              NDLB ~ NBAR ;     ANCT ~ DIAM | 24.0 ;                        0608
              WRITE(FILE2, FOR57, OUT57) ;
              WRITE(FILE2, FOR63, OUT63)                                    0610
         END ;                                                              0611
              ABOTT ~  DIAM*2 | 0.785 | NDLB + ABOT ;                       0612
              ATOPP ~ FD*2 | 0.785 | (NADL + NST) + ABOT | NBEND / NBOT;    0613
              KTA ~ ABOTT | 9.0 ;     KTB ~ ATOPP | 10.0 ;                  0614
              TERM1 ~ BU | 0.5 ;     TERM2 ~ KTA + KTB ;                    0615
              TERM3 ~ -KTA | COV1 - KTB | DU ;                              0616
              KD ~ (-TERM2 + SQRT((TERM2)*2 - 4.0 | TERM1 | TERM3)) /       0617
              (2.0 | TERM1) ;                                               0618
              KTE ~ (KD - COV1) / KD ;                                      0619
              F1 ~ TERM1 | KD ;     F2 ~ KTA | KTE ;     SF ~ F1 + F2 ;     0620
              SM ~ KD | F1 | 0.333 + COV1 | F2 ;     Z ~ SM / SF ;          0621
              JD ~ TU - COV - Z ;                                           0622
              IF TIME THEN                                                  0623
         BEGIN                                                              0624
              TT ~ 12000.0 | MR / JD ;     SHRMX ~ REAB                     0625
         END                                                                0626
              ELSE                                                          0627
         BEGIN                                                              0628
              TT ~ 12000.0 | ML / JD ;     SHRMX ~ REAC                     0629
         END ;                                                              0630
              CNSTB ~ TT / SF ;     STSTB ~ (1.0 - KTE) | CNSTB | N ;       0631
              STTOP ~ TT / ATOPP ;                                          0632
              SHEAR ~ 1000.0 | SHRMX / (0.875 | BU | DU) ;                  0633
              KTE ~ 364.0 | SHRMX / DU ;                                    A633
              BOND ~ KTE / (NBOT | DIAM1 + NDLB | DIAM) ;                   0637
              WRITE(FILE2, FOR66, OUT66) ;                                  0635
              WRITE(FILE2, FOR67, OUT67) ;                                  0636
              BOND ~ KTE /((NST + NADL) | FD) ;                             0634
              WRITE(FILE2, FOR68, OUT68) ;                                  0638

L129A:        IF TIME THEN GO TO L130 ;                                     0639
              TIME ~ TRUE ;     MU ~ MUR ;                                  0640
              SHRMX ~ REAB ;     SIDE ~ "RIGHT" ;                           0641
              GO TO L110 ;                                                  0642

COMMENT       STIRRUP DESIGN.                                               0643
              A.C.I - 807 THE AMOUNT OF WEB REINFORCEMENT SHALL NOT BE      0644
              LESS THAN 0.15 PERCENT OF THE PRODUCT OF THE WIDTH OF THE     0645
              MEMBER AND THE HORIZONTAL SPACING OF WEB REINFORCEMENT        0646
              THE MAXIMUM SPACING OF BARS CANNOT BE MORE THAN ONE-HALF      0647
              THE DEPTH OF THE SECTION,  THE SIZE OF STIRRUPS IS KEPT       0648
              CONSTANT, VARYING THE SPACING. ;                              0649

L130:         STSZ ~ BU | DU | 0.00075 ;     NSTR ~ ABAR ~ 0 ;              0650
              FOR SBD ~ 2 STEP 1 WHILE ABAR < STSZ DO                       0651
         BEGIN                                                              0652
              IF SBD > 9 THEN GO TO L130A ;                                 0653
              BDIAM ;                                                       0654
              ABAR ~ DIAM*2 | 0.785                                         0655
         END ;                                                              0656

L130A:        SBDS ~ SBD ;     I ~ J ~ JJ ~ 1 ;                             0657
              SHRF ~ REAC - INT | DISLD | 0.5 ;                             0658
              SHRCN ~ SC | BU | DU | 0.03 ;                                 0659
              IF SHRF < SHRCN THEN GO TO L22 ;                              0660
              DISLD ~ LL + WIND + DL + WT ;                                 0661
              STSTR ~ SS | ABAR | DU | 0.35 ;                               0662
              SPAST ~ INT ~ 0.0416 | DU ;                                   A662
              FOR Y ~ -0.0208 | DU STEP SPAST WHILE Y < LENTH DO            0663
         BEGIN                                                              0664
              SHL ;                                                         0665
              IF SHRS < 0 THEN
         BEGIN                                                              0667
              IF Y > LENTH THEN GO TO L21 ;                                 0670
              SPAST ~ INT ;   II ~ I ;     I ~ I + 4 ;     KTE ~ Y ;        0668
              KTT ~ Y ;                                                     0669
              GO TO L21A
         END ;                                                              0672
              SPAST ~ STSTR / (SHRS | 12.0) ;                               0673
              IF SPAST > INT THEN SPAST ~ INT ;                             0674

L21:          STIRP ;                                                       0675
              I ~ I + 1 ;

L21A:    END ;                                                              0676
              IF II < 2 THEN II ~ 2 ;     NSTR ~ I - 2 ;                    0678
              I ~ II ;     Y ~ KTE ;                                        0679
              STIRP ;                                                       0680
              I ~ I + 1 ;     Y ~ Y + INT ;                                 0681
              STIRP ;                                                       0682
              Y ~ KTT - 2.0 | INT ;     I ~ I + 1 ;                         0683
              STIRP ;                                                       0684
              I ~ I + 1 ;     Y ~ Y + INT ;                                 0685
              STIRP ;                                                       0686

L22:          IF NSTR > 0 THEN                                              0687
         BEGIN                                                              0688
              WRITE(FILE2, FOR64A, OUT64A) ;                                0689
              FOR I ~ 2 STEP 1 UNTIL NSTR DO                                0690
              WRITE(FILE2, FOR64, OUT64)                                    0691
         END
         END ;
              GO TO L103 ;                                                  0693
L140:   END.
