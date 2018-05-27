 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                          LSPL0000
COMMENT       LEAST SQUARES PLANE THROUGH A SET OF MAXIMALLY            LSPL0001
              ONE HUNDRED POINTS, GIVEN IN FRACTIONAL,                  LSPL0002
              THREE-DIMENSIONAL COORDINATES.                            LSPL0000
              PROGRAM CONVERTS THESE COORDINATES INTO THE               LSPL0003
              ORTHOGONAL AXIS VALUES.                                   LSPL0004
                                                                        LSPL0005
              THE FOLLOWING BOOLEAN OPTIONS EXIST:                      LSPL0006
              BOOL0 = TRUE : CONTROL PRINTOUT OF LAGRANGIAN.            LSPL0007
              BOOL1 = TRUE : START NEW DATA SET.                        LSPL0008
              BOOL2 = TRUE : ALL WEIGHTS UNITY ON SECOND RUN.           LSPL0009
              BOOL3 = TRUE : NONSTOP WEIGHTS ITERATION.                 LSPL0010
                                                                        LSPL0011
              BY EDGAR L. EICHHORN AND STEPHEN MATZNER.                 LSPL0012
                                                                        LSPL0013
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION).           LSPL0014
                                                                        LSPL0015
              CARD SEQUENCE STARTS WITH CODE LSPL-0000.                 LSPL0016
              FIELD-TESTING COMPLETED 02-17-1964  .  ;                  LSPL0017
                                                                        LSPL0018
FILE IN       FIN(1,10)  ;                                              LSPL0019
                                                                        LSPL0020
FILE OUT      FOU 1 (1,15)  ;                                           LSPL0021
INTEGER       I, J, K, R, TRIGG, NORTR  ;                               LSPL0022
                                                                        LSPL0023
REAL          SUMXC, SUMYC, SUMZC, SUMOM, XCM, YCM, ZCM, ALEPH, BEITH,  LSPL0024
              GIMEL, LAGRM, RES, KAPPA, DLAG, NORM, TEMP, F, AR, BR, CR,LSPL0025
              COSAR,COSBR, COSCR, SINAR, SINBR, SINCR, VOL,D,           LSPL0026
              SINA, SINB, SINC, A, B, C, COSA, COSB, COSC,              LSPL0027
              OMICRON, EPSILON, DIFF, EPSMEAN  ;                        LSPL0028
                                                                        LSPL0029
BOOLEAN       BOOL0, BOOL1, BOOL2, BOOL3  ;                             LSPL0030
                                                                        LSPL0031
ARRAY         X, Y, Z, XC, YC, ZC,T, OMEGA[0:101], M, COMP, MR,         LSPL0032
              MNOR[0:3], EL, ONE[0:3,0:3]  ;                            LSPL0033
                                                                        LSPL0034
LIST          CONSTANTS(A,B,C,COSA,COSB,COSC),                          LSPL0035
              BOOLEEN(BOOL0,BOOL1,BOOL2,BOOL3),                         LSPL0036
              POINTS(X[I],Y[I],Z[I],OMEGA[I]),                          LSPL0037
              COORD(I,XC[I],YC[I],ZC[I],T[I],OMEGA[I]),                 LSPL0038
              PARAM(I,X[I],Y[I],Z[I],OMEGA[I]),                         LSPL0039
              EPS(EPSMEAN,OMICRON),                                     LSPL0040
              LAGR(LAGRM),                                              LSPL0041
              DLOUT(DLAG),                                              LSPL0042
              MALL(M[1],M[2],M[3],D),                                   LSPL0043
              MNORM(MNOR[1],MNOR[2],MNOR[3],F),                         LSPL0044
              MRALL(MR[1],MR[2],MR[3],D)  ;                             LSPL0045
                                                                        LSPL0046
FORMAT IN     FMIN1(6F10.5),                                            LSPL0047
              FMIN2(3F8.4,I2),                                          LSPL0048
              FMIN3(4L5)  ;                                             LSPL0049
                                                                        LSPL0050
FORMAT OUT    COORF(I3,X8,3(X5,F10.5),X9,F10.5,X7,F9.6,/),              LSPL0051
              COMPOUND("TRANS BETA-IONYLIDENE CROTONIC ACID.",/),       LSPL0052
              MFF("M[1] = ",F10.5,X3,"M[2] = ",F10.5,X3,"M[3] = "       LSPL0053
              F10.5,X7,"D = ",F10.5,/),                                 LSPL0054
              MRF("H[1] = ",F10.5,X3,"H[2] = ",F10.5,X3,"H[3] = "       LSPL0055
              F10.5,X7,"D = ",F10.5,/),                                 LSPL0056
              MNF("N[1] = ",F10.5,X3,"N[2] = ",F10.5,X3,"N[3] = "       LSPL0057
              F10.5,X7,"D = ",F10.5,/),                                 LSPL0058
              PARM("NUMBER",X13,"X-PARAMETER",X4,"Y-PARAMETER",X4,      LSPL0059
              "Z-PARAMETER",X23,"WEIGHT",/),                            LSPL0060
              PARF(I3,X8,3(X5,F10.5),X26,F9.6,/),                       LSPL0061
              EPSF("EPSMEAN = ",F12.7,X5,"OMICRON = ",F12.7,/),         LSPL0062
              LAGFF(X70,F12.8,/),                                       LSPL0063
              DLFFF(X60,"DLAG = ",X3,F14.8,/),                          LSPL0064
              LAGRF(X40,"LAGR = ",F12.8,/)  ;                           LSPL0065
                                                                        LSPL0066
LABEL         START, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, LSPL0067
              A13, A14, NORA, NORB, NORC, NORD, REFINE, B12, B14,FINISH;LSPL0068
                                                                        LSPL0069
                                                                        LSPL0070
START:        I ~ 1  ;                                                  LSPL0071
              READ(FIN,FMIN3,BOOLEEN)  ;                                LSPL0072
              READ(FIN,FMIN1,CONSTANTS)  ;                              LSPL0073
              WRITE(FOU,COMPOUND)  ;                                    LSPL0074
              WRITE(FOU[DBL])  ;                                        LSPL0075
                                                                        LSPL0076
A1:           READ(FIN,FMIN2,POINTS)  ;                                 LSPL0077
              IF X[I] = 9.9 THEN GO TO A2  ;                            LSPL0078
              I ~ I + 1  ;  GO TO A1  ;                                 LSPL0079
                                                                        LSPL0080
A2:           R ~ I  ;  I ~ 1  ;                                        LSPL0081
              WRITE(FOU,PARM)  ;                                        LSPL0082
              WRITE(FOU[DBL])  ;                                        LSPL0083
                                                                        LSPL0084
              SINA ~ SQRT(1 - COSA | COSA)  ;                           LSPL0085
              SINB ~ SQRT(1 - COSB | COSB)  ;                           LSPL0086
              SINC ~ SQRT(1 - COSC | COSC)  ;                           LSPL0087
                                                                        LSPL0088
              COSAR ~ (COSB | COSC - COSA)/(SINB | SINC)  ;             LSPL0089
              COSBR ~ (COSA | COSC - COSB)/(SINA | SINC)  ;             LSPL0090
              COSCR ~ (COSA | COSB - COSC)/(SINA | SINB)  ;             LSPL0091
                                                                        LSPL0092
              SINAR ~ SQRT(1 - COSAR | COSAR)  ;                        LSPL0093
              SINBR ~ SQRT(1 - COSBR | COSBR)  ;                        LSPL0094
              SINCR ~ SQRT(1 - COSCR | COSCR)  ;                        LSPL0095
                                                                        LSPL0096
              VOL ~ A | B | C | SINA | SINB | SINCR  ;                  LSPL0097
              AR ~ B | C | SINA/VOL  ;                                  LSPL0098
              BR ~ A | C | SINB/VOL  ;                                  LSPL0099
              CR ~ A | B | SINC/VOL  ;                                  LSPL0100
A3:           FOR I ~ 1 STEP 1 UNTIL (R - 1) DO                         LSPL0101
         BEGIN                                                          LSPL0102
              XC[I] ~ A | X[I] | SINC - C | Z[I] | SINA | COSBR  ;      LSPL0103
              YC[I] ~ A | X[I] | COSC + B | Y[I] + C | Z[I] | COSA  ;   LSPL0104
              ZC[I] ~ C | Z[I] | SINA | SINBR  ;                        LSPL0105
              WRITE(FOU,PARF,PARAM)  ;                                  LSPL0106
         END ;                                                          LSPL0107
                                                                        LSPL0108
              WRITE(FOU[DBL])  ;                                        LSPL0109
              WRITE(FOU[DBL])  ;                                        LSPL0110
                                                                        LSPL0111
A4:           FOR J ~ 1 STEP 1 UNTIL 3 DO                               LSPL0112
              FOR K ~ 1 STEP 1 UNTIL 3 DO                               LSPL0113
              ONE[J,K] ~ 0.0  ;                                         LSPL0114
              SUMXC ~ SUMYC ~ SUMZC ~ SUMOM ~ 0.0  ;                    LSPL0115
                                                                        LSPL0116
A5:           FOR I ~ 1 STEP 1 UNTIL (R - 1) DO                         LSPL0117
         BEGIN                                                          LSPL0118
              ONE[1,1] ~ ONE[1,1] + OMEGA[I] | XC[I] | XC[I]  ;         LSPL0119
              ONE[2,2] ~ ONE[2,2] + OMEGA[I] | YC[I] | YC[I]  ;         LSPL0120
              ONE[3,3] ~ ONE[3,3] + OMEGA[I] | ZC[I] | ZC[I]  ;         LSPL0121
                                                                        LSPL0122
              ONE[1,2] ~ ONE[1,2] + OMEGA[I] | XC[I] | YC[I]  ;         LSPL0123
              ONE[1,3] ~ ONE[1,3] + OMEGA[I] | XC[I] | ZC[I]  ;         LSPL0124
              ONE[2,3] ~ ONE[2,3] + OMEGA[I] | YC[I] | ZC[I]  ;         LSPL0125
                                                                        LSPL0126
              SUMXC ~ XC[I] | OMEGA[I] + SUMXC  ;                       LSPL0127
              SUMYC ~ YC[I] | OMEGA[I] + SUMYC  ;                       LSPL0128
              SUMZC ~ ZC[I] | OMEGA[I] + SUMZC  ;                       LSPL0129
                                                                        LSPL0130
              SUMOM ~ OMEGA[I] + SUMOM  ;                               LSPL0131
         END ;                                                          LSPL0132
                                                                        LSPL0133
A6:           XCM ~ SUMXC/SUMOM  ;                                      LSPL0134
              YCM ~ SUMYC/SUMOM  ;                                      LSPL0135
              ZCM ~ SUMZC/SUMOM  ;                                      LSPL0136
                                                                        LSPL0137
              EL[1,1] ~ ONE[1,1] - XCM | XCM | SUMOM  ;                 LSPL0138
              EL[2,2] ~ ONE[2,2] - YCM | YCM | SUMOM  ;                 LSPL0139
              EL[3,3] ~ ONE[3,3] - ZCM | ZCM | SUMOM  ;                 LSPL0140
                                                                        LSPL0141
              EL[1,2] ~ ONE[1,2] - XCM | YCM | SUMOM  ;                 LSPL0142
              EL[1,3] ~ ONE[1,3] - XCM | ZCM | SUMOM  ;                 LSPL0143
              EL[2,3] ~ ONE[2,3] - YCM | ZCM | SUMOM  ;                 LSPL0144
                                                                        LSPL0145
A7:           ALEPH ~ EL[1,1] + EL[2,2] + EL[3,3]  ;                    LSPL0146
              BEITH ~ - EL[1,1] | EL[2,2] - EL[1,1] | EL[3,3]           LSPL0147
                      - EL[2,2] | EL[3,3] + EL[1,2] | EL[1,2]           LSPL0148
                      + EL[1,3] | EL[1,3] + EL[2,3] | EL[2,3]  ;        LSPL0149
              GIMEL ~ EL[1,1] | EL[2,2] | EL[3,3] + (2.0) |             LSPL0150
                      EL[1,2] | EL[1,3] | EL[2,3] -                     LSPL0151
                      EL[1,1] | EL[2,3] | EL[2,3] -                     LSPL0152
                      EL[2,2] | EL[1,3] | EL[1,3] -                     LSPL0153
                      EL[3,3] | EL[1,2] | EL[1,2]  ;                    LSPL0154
                                                                        LSPL0155
A8:           LAGRM ~ - GIMEL/BEITH  ;                                  LSPL0156
                                                                        LSPL0157
A9:           RES ~ - LAGRM | LAGRM | LAGRM + ALEPH | LAGRM | LAGRM +   LSPL0158
                      BEITH | LAGRM + GIMEL  ;                          LSPL0159
              IF BOOL0 THEN WRITE(FOU,LAGFF,LAGR)  ;                    LSPL0160
                                                                        LSPL0161
COMMENT       CHANGE KAPPA TO SUIT REQUIREMENTS (NEXT CARD)  ;          LSPL0162
                                                                        LSPL0163
              KAPPA ~ 2.0 | 10 @ -7  ;                                  LSPL0164
                                                                        LSPL0165
              DLAG ~ - RES/(- (3.0) | LAGRM | LAGRM + (2.0) | ALEPH |   LSPL0166
                       LAGRM + BEITH)  ;                                LSPL0167
              IF BOOL0 THEN WRITE(FOU,DLFFF,DLOUT)  ;                   LSPL0168
              IF ABS(DLAG) { KAPPA THEN GO TO A10  ;                    LSPL0169
              LAGRM ~ LAGRM + DLAG  ;  GO TO A9  ;                      LSPL0170
                                                                        LSPL0171
A10:          WRITE(FOU[PAGE])  ;                                       LSPL0172
              WRITE(FOU,PARM)  ;                                        LSPL0173
              WRITE(FOU[DBL])  ;                                        LSPL0174
                                                                        LSPL0175
              COMP[1]  ~ EL[1,3] | (EL[2,2] - LAGRM) -                  LSPL0176
                         EL[1,2] | EL[2,3]  ;                           LSPL0177
              COMP[2] ~ EL[2,3] | (EL[1,1] - LAGRM) -                   LSPL0178
                        EL[1,3] | EL[1,2]  ;                            LSPL0179
              COMP[3] ~ EL[1,2] | EL[1,2] - (EL[1,1] - LAGRM) |         LSPL0180
                        (EL[2,2] - LAGRM)  ;                            LSPL0181
                                                                        LSPL0182
              NORM ~ COMP[1] | COMP[1] + COMP[2] | COMP[2] +            LSPL0183
                     COMP[3] | COMP[3]  ;                               LSPL0184
              FOR K ~ 1 STEP 1 UNTIL 3 DO                               LSPL0185
              M[K] ~ COMP[K]/SQRT(NORM)  ;                              LSPL0186
              D ~ XCM | M[1] + YCM | M[2] + ZCM | M[3]  ;               LSPL0187
                                                                        LSPL0188
A11:          FOR I ~ 1 STEP 1 UNTIL (R - 1) DO                         LSPL0189
         BEGIN                                                          LSPL0190
              T[I] ~ XC[I] | M[1] + YC[I] | M[2] + ZC[I] | M[3] - D  ;  LSPL0191
              WRITE(FOU,COORF,COORD)  ;                                 LSPL0192
         END ;                                                          LSPL01 3
                                                                        LSPL0194
              MR[1] ~ A | (M[1] | SINC + M[2] | COSC) ;                 LSPL0195
              MR[2] ~ B | M[2]  ;                                       LSPL0196
              MR[3] ~ C | (M[3] | SINA | SINBR + M[2] | COSA -          LSPL0197
                      M[1] | SINA | COSBR)  ;                           LSPL0198
              WRITE(FOU[DBL])  ;                                        LSPL0199
              WRITE(FOU,LAGRF,LAGR)  ;                                  LSPL0200
              WRITE(FOU[DBL])  ;                                        LSPL0201
              WRITE(FOU,MFF,MALL)  ;                                    LSPL0202
              WRITE(FOU[DBL])  ;                                        LSPL0203
              WRITE(FOU,MRF,MRALL)  ;                                   LSPL0204
              WRITE(FOU[DBL])  ;                                        LSPL0205
                                                                        LSPL0206
NORA:         J ~ 1  ;                                                  LSPL0207
                                                                        LSPL0208
NORB:         I ~ 1  ; NORTR ~ 0  ;                                     LSPL0209
                                                                        LSPL0210
NORC:         MNOR[I] ~ MR[I]/MR[J]  ;                                  LSPL0211
              IF MNOR[I] > 1000.0 THEN GO TO NORD  ;                    LSPL0212
              IF MNOR[I] = 0 THEN NORTR ~ I  ;                          LSPL0213
              I ~ I + 1  ;                                              LSPL0214
              IF I { 3 THEN GO TO NORC  ;                               LSPL0215
              IF ABS(MNOR[1]) } ABS(MNOR[2]) THEN TEMP ~ ABS(MNOR[1])   LSPL0216
              ELSE TEMP ~ ABS(MNOR[2])  ;                               LSPL0217
              IF ABS(MNOR[3]) > TEMP THEN TEMP ~ ABS(MNOR[3])  ;        LSPL0218
              IF NORTR ! 0 AND TEMP = 1.0 AND MNOR[1] ! MNOR[2] AND     LSPL0219
              MNOR[1] ! MNOR[3] AND MNOR[2] ! MNOR[3] THEN GO TO NORD  ;LSPL0220
              F ~ SQRT((1.0)/((MNOR[1] | AR)*2 + (MNOR[2] | BR)*2 +     LSPL0221
                  (2.0) | AR | CR | COSBR | MNOR[1] | MNOR[3] +         LSPL0222
                  (2.0) | BR | CR | COSAR | MNOR[2] | MNOR[3])) ;       LSPL0223
              WRITE(FOU,MNF,MNORM)  ;                                   LSPL0224
                                                                        LSPL0225
NORD:         J ~ J + 1  ;                                              LSPL0226
              IF J { 3 THEN GO TO NORB  ;                               LSPL0227
                                                                        LSPL0228
A12:          WRITE(FOU[PAGE])  ;                                       LSPL0229
              IF BOOL2 OR BOOL3 THEN GO TO REFINE  ;                    LSPL0230
                                                                        LSPL0231
B12:          IF BOOL1 THEN GO TO START  ;                              LSPL0232
                                                                        LSPL0233
REFINE:       IF BOOL2 THEN GO TO A13  ;                                LSPL0234
              IF NOT BOOL3 THEN GO TO FINISH  ;                         LSPL0235
                                                                        LSPL0236
              BOOL3 ~ FALSE  ;                                          LSPL0237
              FOR I ~ 1 STEP 1 UNTIL (R - 1) DO                         LSPL0238
         BEGIN                                                          LSPL0239
              OMEGA[R+I] ~ OMEGA[I]  ;                                  LSPL0240
              OMEGA[I] ~ EXP(- ABS(T[I]))  ;                            LSPL0241
         END ;                                                          LSPL0242
                                                                        LSPL0243
COMMENT       CHANGE OMICRON TO SUIT REQUIREMENTS(NEXT CARD)  ;         LSPL0244
                                                                        LSPL0245
              OMICRON ~ 5.0 | 10 @ -6  ;                                LSPL0246
              TRIGG ~ 0  ;  EPSILON ~ 0.0  ;                            LSPL0247
                                                                        LSPL0248
              FOR I ~ 1 STEP 1 UNTIL (R - 1) DO                         LSPL0249
         BEGIN                                                          LSPL0250
              DIFF ~ ABS(OMEGA[I] - OMEGA[R+I])  ;                      LSPL0251
              IF DIFF } OMICRON THEN TRIGG ~ 1  ;                       LSPL0252
              EPSILON ~ EPSILON + DIFF  ;                               LSPL0253
         END ;                                                          LSPL0254
                                                                        LSPL0255
              IF TRIGG = 0 THEN                                         LSPL0256
         BEGIN                                                          LSPL0257
              EPSMEAN ~ EPSILON/(R - 1)  ;  GO TO A14  ;                LSPL0258
         END ;                                                          LSPL0259
                                                                        LSPL0260
              GO TO A4  ;                                               LSPL0261
                                                                        LSPL0262
A13:          FOR I ~ 1 STEP 1 UNTIL (R - 1) DO                         LSPL0263
              OMEGA[I] ~ 1.0  ;  BOOL2 ~ FALSE  ;  GO TO A4  ;          LSPL0264
                                                                        LSPL0265
A14:          WRITE(FOU,EPSF,EPS)  ;                                    LSPL0266
                                                                        LSPL0267
B14:          IF BOOL1 THEN GO TO START  ;                              LSPL0268
                                                                        LSPL0269
FINISH:  END .                                                          LSPL0270
 LABEL  000000000FIN    0010000001
 TRUE TRUE TRUEFALSE
  10.48     13.63      7.67     -0.32722  -0.61566   0.55339
  0.7352  0.3240  0.8038 9
  0.8351  0.1644  1.0070 9
  0.7506  0.1998  0.7852 6
  0.8660  0.1339  0.7031 6
  0.7891  0.1349  0.4626 6
  0.6383  0.0909  0.3186 6
  0.5313  0.1274  0.4079 6
  0.3597  0.0983  0.2287 9
  0.5739  0.1776  0.6095 6
  0.4513  0.2116  0.6729 6
  0.4545  0.2749  0.8460 6
  0.3256  0.3033  0.8926 6
  0.1657  0.2645  0.7065 9
  0.3496  0.3625  1.0845 6
  0.2372  0.3911  1.1550 6
  0.2722  0.4449  1.3538 6
  0.1575  0.4693  1.4204 6
  0.2008  0.5282  1.6153 8
  0.0279  0.4378  1.3022 8
 -0.7352  0.676   2.1962 9
 -0.8351  0.8356  1.993  9
 -0.7506  0.8002  2.2148 6
 -0.866   0.8661  2.2969 6
 -0.7891  0.8651  2.5374 6
 -0.6383  0.9091  2.6814 6
 -0.5313  0.8726  2.5921 6
 -0.3597  0.9017  2.7713 9
 -0.5739  0.8224  2.3905 6
 -0.4513  0.7884  2.3271 6
 -0.4545  0.7251  2.1540 6
 -0.3256  0.6967  2.1074 6
 -0.1657  0.7354  2.2935 9
 -0.3496  0.6375  1.9155 6
 -0.2372  0.6089  1.8450 6
 -0.2722  0.5551  1.6462 6
 -0.1575  0.5307  1.5796 6
 -0.2008  0.4718  1.3847 8
 -0.0279  0.5622  1.6978 8
  9.9     0.0     0.0    0
 TRUE TRUE TRUE TRUE
  10.48     13.63      7.67     -0.32722  -0.61566   0.55339
  0.7352  0.3240  0.8038 9
  0.8351  0.1644  1.0070 9
  0.7506  0.1998  0.7852 6
  0.8660  0.1339  0.7031 6
  0.7891  0.1349  0.4626 6
  0.6383  0.0909  0.3186 6
  0.5313  0.1274  0.4079 6
  0.3597  0.0983  0.2287 9
  0.5739  0.1776  0.6095 6
  0.4513  0.2116  0.6729 6
  0.4545  0.2749  0.8460 6
  0.3256  0.3033  0.8926 6
  0.1657  0.2645  0.7065 9
  0.3496  0.3625  1.0845 6
  0.2372  0.3911  1.1550 6
  0.2722  0.4449  1.3538 6
  0.1575  0.4693  1.4204 6
  0.2008  0.5282  1.6153 8
  0.0279  0.4378  1.3022 8
 -0.7352  0.676   2.1962 9
 -0.8351  0.8356  1.993  9
 -0.7506  0.8002  2.2148 6
 -0.866   0.8661  2.2969 6
 -0.7891  0.8651  2.5374 6
 -0.6383  0.9091  2.6814 6
 -0.5313  0.8726  2.5921 6
 -0.3597  0.9017  2.7713 9
 -0.5739  0.8224  2.3905 6
 -0.4513  0.7884  2.3271 6
 -0.4545  0.7251  2.1540 6
 -0.3256  0.6967  2.1074 6
 -0.1657  0.7354  2.2935 9
 -0.3496  0.6375  1.9155 6
 -0.2372  0.6089  1.8450 6
 -0.2722  0.5551  1.6462 6
 -0.1575  0.5307  1.5796 6
 -0.2008  0.4718  1.3847 8
 -0.0279  0.5622  1.6978 8
  9.9     0.0     0.0    0
 TRUEFALSEFALSE TRUE
  10.48     13.63      7.67     -0.32722  -0.61566   0.55339
  0.7352  0.3240  0.8038 9
  0.8351  0.1644  1.0070 9
  0.7506  0.1998  0.7852 6
  0.8660  0.1339  0.7031 6
  0.7891  0.1349  0.4626 6
  0.6383  0.0909  0.3186 6
  0.5313  0.1274  0.4079 6
  0.3597  0.0983  0.2287 9
  0.5739  0.1776  0.6095 6
  0.4513  0.2116  0.6729 6
  0.4545  0.2749  0.8460 6
  0.3256  0.3033  0.8926 6
  0.1657  0.2645  0.7065 9
  0.3496  0.3625  1.0845 6
  0.2372  0.3911  1.1550 6
  0.2722  0.4449  1.3538 6
  0.1575  0.4693  1.4204 6
  0.2008  0.5282  1.6153 8
  0.0279  0.4378  1.3022 8
 -0.7352  0.676   2.1962 9
 -0.8351  0.8356  1.993  9
 -0.7506  0.8002  2.2148 6
 -0.866   0.8661  2.2969 6
 -0.7891  0.8651  2.5374 6
 -0.6383  0.9091  2.6814 6
 -0.5313  0.8726  2.5921 6
 -0.3597  0.9017  2.7713 9
 -0.5739  0.8224  2.3905 6
 -0.4513  0.7884  2.3271 6
 -0.4545  0.7251  2.1540 6
 -0.3256  0.6967  2.1074 6
 -0.1657  0.7354  2.2935 9
 -0.3496  0.6375  1.9155 6
 -0.2372  0.6089  1.8450 6
 -0.2722  0.5551  1.6462 6
 -0.1575  0.5307  1.5796 6
 -0.2008  0.4718  1.3847 8
 -0.0279  0.5622  1.6978 8
  9.9     0.0     0.0    0
