 LABEL  0000000000XXXXXX0010000001
                                                                        CELL0001
                                                                        CELL0002
COMMENT       PROGRAM FOR THE COMPUTATION OF BASIC                      CELL0003
              CRYSTALLOGRAPHIC UNIT CELL DATA.                          CELL0004
                                                                        CELL0005
                   PART A)  RECIPROCAL CELL CONSTANTS FROM              CELL0006
                            THE MEASURED DIRECT CELL.                   CELL0007
                   PART B)  PREPARATION OF CARDS AND LISTING            CELL0008
                            OF ALL INDEX COMBINATIONS                   CELL0009
                            POSSIBLE WITHIN THE GIVEN                   CELL0010
                            LIMITING SPHERE OF LAMBDA,                  CELL0011
                            SUBJECT TO SPACEGROUP CONSTRAINTS.          CELL0012
                                                                        CELL0013
              BOOLEAN EXPRESSIONS FOR PROGRAM CONTROL OPTIONS :         CELL0014
                      BOOL0 TRUE = TRIGGER IS READ.                     CELL0015
                      BOOL1 TRUE = PROGRAM WILL STOP AFTER THE          CELL0016
                                   COMPUTATION OF CELL CONSTANTS.       CELL0017
                      BOOL2 TRUE = CARD PUNCHING IS BYPASSED.           CELL0018
                      BOOL3 TRUE = WRITES OUT THE VALUE OF LS AND       CELL0019
                                   STOPS AFTER LAYER IS DONE.           CELL0020
                      BOOL4 TRUE = BYPASS CELL PRINTOUT.                CELL0021
                      BOOL5 TRUE = CELL INPUT CONSISTS OF               CELL0022
                                   RECIPROCAL CELL DATA.                CELL0023
                                                                        CELL0024
              AN AVERAGE HIGHER LAYER IN A TRICLINIC SPACEGROUP WAS     CELL0025
              COMPUTED IN 3.3 MINUTES PER 1000 VALID INDEX              CELL0026
              COMBINATIONS, USING A HIGH-SPEED PRINTER FOR OUTPUT.      CELL0027
                                                                        CELL0028
              BY EDGAR L. EICHHORN AND STEPHEN MATZNER,                 CELL0029
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION).           CELL0030
                                                                        CELL0031
              CARD SEQUENCE CODE STARTS WITH CELL-0001.                 CELL0032
              FIELD TESTING COMPLETED JANUARY 1964  ;                   CELL0033
                                                                        CELL0034
                                                                        CELL0035
         BEGIN                                                          CELL0036
INTEGER       H, K, L, HMAX, KMAX, LMAX, HS, KS, LS, WH, WK,            CELL0037
              TRIGG, CLOSA, SUM1, SUM2, SUM3, SUM4, P, Q, R,            CELL0038
              SUMT1, SUMT2, SUMT3, SUMT4, SUMPL,LANE,SECTOR  ;          CELL0039
REAL          LAMBDA, SINA, SINB, SINC, COSA, COSB, COSC, A, B, C,      CELL0040
              SINAR, SINBR, SINCR, COSAR, COSBR, COSCR, VOL, VOLR,      CELL0041
              REV, CHH, CKK, CLL, CHK, CHL, CKL, ALEPH, BETA, GAMMA,    CELL0042
              ALEPHR, BETAR, GAMMAR, SST, AR, BR, CR, ZERO,             CELL0043
              HMXF, KMXF, LMXF  ;                                       CELL0044
                                                                        CELL0045
BOOLEAN       BOOL0, BOOL1, BOOL2, BOOL3, BOOL4, BOOL5  ;               CELL0046
                                                                        CELL0047
FILE IN       FIN(1,10)  ;                                              CELL0048
                                                                        CELL0049
FILE OUT      PRINTER 1 (1,15),                                         CELL0050
              PUNCH 0 (1,10)  ;                                         CELL0051
                                                                        CELL0052
COMMENT       INPUT LIST DECLARATIONS ARE STARTED  ;                    CELL0053
                                                                        CELL0054
LIST          TRIGGER(TRIGG),                                           CELL0055
              BOOLEEN(BOOL0,BOOL1,BOOL2,BOOL3,BOOL4,BOOL5),             CELL0056
              CELL(A,B,C,COSA,COSB,COSC),                               CELL0057
              RAD(LAMBDA)  ;                                            CELL0058
                                                                        CELL0059
COMMENT       OUTPUT LIST DECLARATIONS ARE STARTED  ;                   CELL0060
                                                                        CELL0061
LIST          LAYERS(ABS(LS),SUM1,SUM2,SUM3,SUM4),                      CELL0062
              RADT(LAMBDA),                                             CELL0063
              CARDP(H,K,L,SST,ZERO,ZERO),                               CELL0064
              SSTGEN(CHH,CKK,CLL,CHK,CHL,CKL),                          CELL0065
              DIRECT(A,B,C,VOL),                                        CELL0066
              ANGSIN(SINA,SINB,SINC),                                   CELL0067
              ANGCOS(COSA,COSB,COSC),                                   CELL0068
              RECP(AR,BR,CR,VOLR),                                      CELL0069
              ANGSINR(SINAR,SINBR,SINCR),                               CELL0070
              ANGCOSR(COSAR,COSBR,COSCR),                               CELL0071
              DEGRD(ALEPH,BETA,GAMMA),                                  CELL0072
              DEGRR(ALEPHR,BETAR,GAMMAR),                               CELL0073
              LLIST(H,K,L,SST),                                         CELL0074
              TOTAL(SUMT1,SUMT2,SUMT3,SUMT4)  ;                         CELL0075
                                                                        CELL0076
FORMAT IN     FMIN1(6L5),                                               CELL0077
              FMIN2(6F7.5),                                             CELL0078
              FMIN3(F7.5),                                              CELL0079
              FMIN4(F10.6)  ;                                           CELL0080
                                                                        CELL0081
FORMAT OUT    STRUCTURE("MONOCALCIUMPHOSPHITE.MONOHYDRATE"/,/),         CELL0082
              FGEN(X5,6(F10.7,X1)),                                     CELL0083
              FIRST("DIRECT CELL",/,/),                                 CELL0084
              SECOND(X13,"ALPHA",X8,"BETA",X9,"GAMMA",/,/),             CELL0085
              THIRD("RECIPROCAL CELL",/,/),                             CELL0086
              GENF("CHH =",F10.7,X2,"CKK =",F10.7,X2,"CLL =",           CELL0087
                  F10.7,X2,"CHK =",F10.7,X2,"CHL =",F10.7,X2,           CELL0088
                  "CKL =",F10.7,/),                                     CELL0089
              DIRF("A =",F9.5,X5,"B =",F9.5,X5,"C =",F9.5,X5,           CELL0090
                  "V =",F12.5,/,/),                                     CELL0091
              DIRQ("A =",F7.3,X7,"B =",F7.3,X7,"C =",F7.3,X7,           CELL0092
                  "V =",F10.3,/,/),                                     CELL0093
              ANGF1("SIN",X8,3(F8.5,X5),/),                             CELL0094
              ANGF2("COS",X8,3(F8.5,X5),/,/),                           CELL0095
              LAYF(X10,5(I5,X2),/),                                     CELL0096
              DEGRF("ANGLE",X5,3(F6.2,X7),/),                           CELL0097
              PRODF(X50,"ALL",X2,4(I5,X2),/,/),                         CELL0098
              SSTCON("SST GENERATORS",/),                               CELL0099
              RADF("LAMBDA = ",F8.5,/,/),                               CELL0100
              CARDFT(X3,3(I3,X2),X3,3(F8.5,X1)),                        CELL0101
              ERROR1("SST IS LESS THAN ZERO"),                          CELL0102
              LISTA(X20,3(I3,X2),X5,F9.5,/)  ;                          CELL0103
                                                                        CELL0104
LABEL         AA,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,CELL0105
              A17,P1,P2,P3,P4,P5,P6,Q1,Q2,Q3,Q4,LOOP,OVER,LLLL  ;       CELL0106
                                                                        CELL0107
SWITCH        SECTOO ~ Q1, Q2, Q3, Q4  ;                                CELL0108
SWITCH        LANN ~ P1, P2, P3, P4, P5, P6  ;                          CELL0109
                                                                        CELL0110
AA:           TRIGG ~ 0  ;                                              CELL0111
              READ(FIN,FMIN1,BOOLEEN)  ;                                CELL0112
              IF BOOL0 THEN READ(FIN,FMIN4,TRIGGER)  ;                  CELL0113
              READ(FIN,FMIN2,CELL)  ;                                   CELL0114
              READ(FIN,FMIN3,RAD)  ;                                    CELL0115
              ZERO ~ SUMT1 ~ SUMT2 ~ SUMT3 ~ SUMT4 ~ 0  ;               CELL0116
                                                                        CELL0117
A1:           HMXF ~ 2 | A/LAMBDA ;                                     CELL0118
              KMXF ~ 2 | B/LAMBDA  ;                                    CELL0119
              LMXF ~ 2 | C/LAMBDA  ;                                    CELL0120
                                                                        CELL0121
              SINA ~ SQRT(-COSA | COSA + 1)  ;                          CELL0122
              SINB ~ SQRT(-COSB | COSB + 1)  ;                          CELL0123
              SINC ~ SQRT(-COSC | COSC + 1)  ;                          CELL0124
                                                                        CELL0125
A2:           COSAR ~ (COSB | COSC - COSA)/(SINB | SINC)  ;             CELL0126
              COSBR ~ (COSA | COSC - COSB)/(SINA | SINC)  ;             CELL0127
              COSCR ~ (COSA | COSB - COSC)/(SINA | SINB)  ;             CELL0128
                                                                        CELL0129
              SINAR ~ SQRT(-COSAR | COSAR + 1)  ;                       CELL0130
              SINBR ~ SQRT(-COSBR | COSBR + 1)  ;                       CELL0131
              SINCR ~ SQRT(-COSCR | COSCR + 1)  ;                       CELL0132
                                                                        CELL0133
A3:           VOL ~ A | B | C | SINA | SINB | SINCR  ;                  CELL0134
              VOLR ~ 1/VOL  ;                                           CELL0135
                                                                        CELL0136
              AR ~ B | C | SINA/VOL  ;                                  CELL0137
              BR ~ A | C | SINB/VOL  ;                                  CELL0138
              CR ~ A | B | SINC/VOL  ;                                  CELL0139
                                                                        CELL0140
              IF BOOL5 THEN                                             CELL0141
         BEGIN                                                          CELL0142
              REV ~ A  ;  A ~ AR  ;  AR ~ REV  ;                        CELL0143
              REV ~ B  ;  B ~ BR  ;  BR ~ REV  ;                        CELL0144
              REV ~ C  ;  C ~ CR  ;  CR ~ REV  ;                        CELL0145
              REV ~ COSA  ;  COSA ~ COSAR  ;  COSAR ~ REV  ;            CELL0146
              REV ~ COSB  ;  COSB ~ COSBR  ;  COSBR ~ REV  ;            CELL0147
              REV ~ COSC  ;  COSC ~ COSCR  ;  COSCR ~ REV  ;            CELL0148
              REV ~ SINA  ;  SINA ~ SINAR  ;  SINAR ~ REV  ;            CELL0149
              REV ~ SINB  ;  SINB ~ SINBR  ;  SINBR ~ REV  ;            CELL0150
              REV ~ SINC  ;  SINC ~ SINCR  ;  SINCR ~ REV  ;            CELL0151
              REV ~ VOL  ;  VOL ~ VOLR  ;  VOLR ~ REV  ;                CELL0152
              HMXF ~ HMXF | A/AR  ;                                     CELL0153
              KMXF ~ KMXF | B/BR  ;                                     CELL0154
              LMXF ~ LMXF | C/CR  ;                                     CELL0155
         END ;                                                          CELL0156
                                                                        CELL0157
              CHH ~ AR | AR | LAMBDA | LAMBDA/4  ;                      CELL0158
              CKK ~ BR | BR | LAMBDA | LAMBDA/4  ;                      CELL0159
              CLL ~ CR | CR | LAMBDA | LAMBDA/4  ;                      CELL0160
              CHK ~ 2 | AR | BR | COSCR | LAMBDA | LAMBDA/4  ;          CELL0161
              CHL ~ 2 | AR | CR | COSBR | LAMBDA | LAMBDA/4  ;          CELL0162
              CKL ~ 2 | BR | CR | COSAR | LAMBDA | LAMBDA/4  ;          CELL0163
                                                                        CELL0164
              ALEPH ~ 57.2958 | ARCTAN(SQRT(- COSA | COSA + 1)/COSA)  ; CELL0165
              IF COSA < 0 THEN ALEPH ~ 180 + ALEPH  ;                   CELL0166
              BETA  ~ 57.2958 | ARCTAN(SQRT(- COSB | COSB + 1)/COSB)  ; CELL0167
              IF COSB < 0 THEN BETA  ~ 180 + BETA   ;                   CELL0168
              GAMMA ~ 57.2958 | ARCTAN(SQRT(- COSC | COSC + 1)/COSC)  ; CELL0169
              IF COSC < 0 THEN GAMMA ~ 180 + GAMMA  ;                   CELL0170
              ALEPHR ~ 57.2958 | ARCTAN(SQRT(- COSAR | COSAR +          CELL0171
              1)/COSAR)  ;                                              CELL0172
              IF COSAR < 0 THEN ALEPHR ~ 180 + ALEPHR  ;                CELL0173
              BETAR  ~ 57.2958 | ARCTAN(SQRT(- COSBR | COSBR +          CELL0174
              1)/COSBR)  ;                                              CELL0175
              IF COSBR < 0 THEN BETAR  ~ 180 + BETAR   ;                CELL0176
              GAMMAR ~ 57.2958 | ARCTAN(SQRT(- COSCR | COSCR +          CELL0177
              1)/COSCR)  ;                                              CELL0178
              IF COSCR < 0 THEN GAMMAR ~ 180 + GAMMAR  ;                CELL0179
                                                                        CELL0180
A4:           IF BOOL4 THEN GO TO OVER  ;                               CELL0181
              WRITE(PUNCH,FGEN,SSTGEN)  ;                               CELL0182
              WRITE(PRINTER,STRUCTURE)  ;                               CELL0183
              WRITE(PRINTER,FIRST)  ;                                   CELL0184
              WRITE(PRINTER,DIRQ,DIRECT)  ;                             CELL0185
              WRITE(PRINTER,SECOND)  ;                                  CELL0186
              WRITE(PRINTER,DEGRF,DEGRD)  ;                             CELL0187
              WRITE(PRINTER,ANGF1,ANGSIN)  ;                            CELL0188
              WRITE(PRINTER,ANGF2,ANGCOS)  ;                            CELL0189
              WRITE(PRINTER,THIRD)  ;                                   CELL0190
              WRITE(PRINTER,DIRF,RECP)  ;                               CELL0191
              WRITE(PRINTER,SECOND)  ;                                  CELL0192
              WRITE(PRINTER,DEGRF,DEGRR) ;                              CELL0193
              WRITE(PRINTER,ANGF1,ANGSINR)  ;                           CELL0194
              WRITE(PRINTER,ANGF2,ANGCOSR)  ;                           CELL0195
              WRITE(PRINTER,SSTCON)  ;                                  CELL0196
              WRITE(PRINTER,RADF,RADT)  ;                               CELL0197
              WRITE(PRINTER,GENF,SSTGEN)  ;                             CELL0198
                                                                        CELL0199
OVER:         HMAX ~ HMXF  ;  KMAX ~ KMXF  ; LMAX ~ LMXF  ;             CELL0200
              IF BOOL1 THEN GO TO A17  ;                                CELL0201
                                                                        CELL0202
A5:           IF HMAX } KMAX THEN GO TO A6 ;                            CELL0203
              IF HMAX } LMAX THEN GO TO A8 ;                            CELL0204
              IF KMAX } LMAX THEN GO TO A9 ;                            CELL0205
              LANE ~ 6  ;  GO TO A11  ;                                 CELL0206
                                                                        CELL0207
A6:           IF HMAX } LMAX THEN GO TO A7  ;                           CELL0208
              LANE ~ 1  ;  GO TO A11  ;                                 CELL0209
                                                                        CELL0210
A7:           IF LMAX } KMAX THEN GO TO A10  ;                          CELL0211
              LANE ~ 2  ;  GO TO A11  ;                                 CELL0212
                                                                        CELL0213
A8:           LANE ~ 3  ;  GO TO A11  ;                                 CELL0214
                                                                        CELL0215
A9:           LANE ~ 4  ;  GO TO A11  ;                                 CELL0216
                                                                        CELL0217
A10:          LANE ~ 5  ;                                               CELL0218
                                                                        CELL0219
A11:          LS ~ TRIGG  ;                                             CELL0220
              SUMT1 ~ SUMT2 ~ SUMT3 ~ SUMT4 ~ 0  ;                      CELL0221
              SECTOR ~ WH ~ WK ~ 1  ;                                   CELL0222
              SUMPL ~ HS ~ KS ~ 0  ;                                    CELL0223
                                                                        CELL0224
              IF LS = 0 THEN HS ~ 1  ;                                  CELL0225
                                                                        CELL0226
LOOP:         GO TO LANN[LANE]  ;                                       CELL0227
                                                                        CELL0228
P1:           L ~ HS  ;  H~ KS  ;  K ~ LS  ;                            CELL0229
              P ~ LMAX  ;  Q ~ HMAX  ;  R ~ KMAX  ;                     CELL0230
              GO TO A12  ;                                              CELL0231
                                                                        CELL0232
P2:           H ~ HS  ;  K ~ KS  ;  L ~ LS  ;                           CELL0233
              P ~ HMAX  ;  Q ~ KMAX  ;  R ~ LMAX  ;                     CELL0234
              GO TO A12  ;                                              CELL0235
P3:           K ~ HS  ;  H ~ KS  ;  L ~ LS  ;                           CELL0236
              P ~ KMAX  ;  Q ~ HMAX  ;  R ~ LMAX  ;                     CELL0237
              GO TO A12  ;                                              CELL0238
                                                                        CELL0239
P4:           K ~ HS  ;  L ~ KS  ;  H ~ LS  ;                           CELL0240
              P ~ KMAX  ;  Q ~ LMAX  ;  R ~ HMAX  ;                     CELL0241
              GO TO A12  ;                                              CELL0242
P5:           H ~ HS  ; L ~ KS  ;  K ~ LS  ;                            CELL0243
              P ~ HMAX  ;  Q ~ LMAX  ;  R ~ KMAX  ;                     CELL0244
              GO TO A12  ;                                              CELL0245
                                                                        CELL0246
P6:           L ~ HS  ;  K ~ KS  ;  H ~ LS  ;                           CELL0247
              P ~ LMAX  ;  Q ~ KMAX  ;  R ~ HMAX  ;                     CELL0248
                                                                        CELL0249
A12:          SST ~ CHH | H | H + CKK | K | K + CLL | L | L +           CELL0250
                    CHK | H | K + CHL | H | L + CKL | K | L  ;          CELL0251
              IF SST < 0 THEN WRITE(PRINTER,ERROR1)  ;                  CELL0252
              IF SST > 1 THEN GO TO A13  ;                              CELL0253
              IF BOOL2 THEN GO TO LLLL  ;                               CELL0254
              WRITE(PUNCH,CARDFT,CARDP)  ;                              CELL0255
                                                                        CELL0256
LLLL:         WRITE(PRINTER,LISTA,LLIST)  ;                             CELL0257
              SUMPL ~ SUMPL + 1  ;  CLOSA ~ 0  ;                        CELL0258
                                                                        CELL0259
A13:          IF ABS(HS) = P THEN GO TO A14  ;                          CELL0260
              HS ~ HS + WH  ;  GO TO LOOP  ;                            CELL0261
                                                                        CELL0262
A14:          IF ABS(KS) = Q THEN GO TO A15  ;                          CELL0263
              HS ~ 0  ;  KS ~ KS + WK  ;                                CELL0264
              IF SECTOR = 2 THEN HS ~ -1  ;                             CELL0265
              IF SECTOR = 4 THEN HS ~ 1  ;                              CELL0266
              IF CLOSA = 1 THEN GO TO LOOP  ;                           CELL0267
              CLOSA ~ 1  ;  GO TO LOOP  ;                               CELL0268
                                                                        CELL0269
A15:          GO TO SECTOO[SECTOR]  ;                                   CELL0270
                                                                        CELL0271
Q1:           SUM1 ~ SUMPL  ;  SUMPL ~ 0  ;                             CELL0272
              SECTOR ~ SECTOR + 1  ;                                    CELL0273
              HS ~ - 1 ;  KS ~ 0  ;  WH ~ - 1  ;                        CELL0274
              IF LS = 0 THEN KS ~ 1  ;                                  CELL0275
              IF CLOSA = 1 THEN GO TO LOOP  ;                           CELL0276
              CLOSA ~ 1  ;  GO TO LOOP  ;                               CELL0277
Q2:           SUM2 ~ SUMPL  ;  SUMPL ~ 0  ;                             CELL0278
              SECTOR ~ SECTOR + 1  ;                                    CELL0279
              IF LS = 0 THEN GO TO Q4  ;                                CELL0280
              HS ~ 0  ;  KS ~ - 1  ;  WH ~ 1  ;  WK ~ - 1  ;            CELL0281
              IF CLOSA = 1 THEN GO TO LOOP  ;                           CELL0282
              CLOSA ~ 1  ;  GO TO LOOP  ;                               CELL0283
                                                                        CELL0284
Q3:           SUM3 ~ SUMPL  ;  SUMPL ~ 0  ;                             CELL0285
              SECTOR ~ SECTOR + 1  ;                                    CELL0286
              HS ~ KS ~ WH ~ WK ~ 1  ;  LS ~ - LS  ;                    CELL0287
              IF CLOSA = 1 THEN GO TO LOOP  ;                           CELL0288
              CLOSA ~ 1  ;  GO TO LOOP  ;                               CELL0289
                                                                        CELL0290
Q4:           SUM4 ~ SUMPL  ;  SUMPL ~ 0  ;                             CELL0291
              SECTOR ~ 1  ;                                             CELL0292
              WRITE(PRINTER,LAYF,LAYERS)  ;                             CELL0293
                                                                        CELL0294
              SUMT1 ~ SUMT1 + SUM1  ;  SUM1 ~ 0  ;                      CELL0295
              SUMT2 ~ SUMT2 + SUM2  ;  SUM2 ~ 0  ;                      CELL0296
              SUMT3 ~ SUMT3 + SUM3  ;  SUM3 ~ 0  ;                      CELL0297
              SUMT4 ~ SUMT4 + SUM4  ;  SUM4 ~ 0  ;                      CELL0298
                                                                        CELL0299
              IF ABS(LS) = R THEN GO TO A16  ;                          CELL0300
              LS ~ ABS(LS) + 1  ;                                       CELL0301
              KS ~ HS ~ 0  ;  WH ~ WK ~ 1  ;                            CELL0302
              IF NOT BOOL3 THEN GO TO LOOP  ;                           CELL0303
              WRITE(PRINTER,FMIN4,LS)  ;  GO TO A17  ;                  CELL0304
                                                                        CELL0305
A16:          WRITE(PRINTER,PRODF,TOTAL)  ;                             CELL0306
                                                                        CELL0307
A17:     END .                                                          CELL0308
 LABEL  000000000FIN    0010000001
FALSEFALSE TRUE TRUEFALSE TRUE
0.162060.133250.148500.053500.114640.46920
0.71070
