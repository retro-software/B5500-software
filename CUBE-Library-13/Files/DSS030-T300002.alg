 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
              ESTIMATED STANDARD DEVIATIONS AND MOST PROBABLE ERRORS    ESDV0000
              OF BOND DISTANCES AND ANGLES,                             ESDV0001
              FOR A MAXIMUM OF 100 ATOMS PER EVALUATED SET.             ESDV0002
                                                                        ESDV0003
              BOOLEAN EXPRESSIONS FOR PROGRAM CONTROL OPTIONS :         ESDV0004
                                                                        ESDV0005
                   BOOL0 = TRUE : SELECTED BONDS ARE LISTED.            ESDV0006
                   BOOL1 = TRUE : ANGLES ARE NOT COMPUTED.              ESDV0007
                                                                        ESDV0008
              BY EDGAR L. EICHHORN, AND STEPHEN MATZNER.                ESDV0009
                                                                        ESDV0010
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION).           ESDV0011
                                                                        ESDV0012
              CARD SEQUENCE CODE STARTS WITH ESDV-0000.                 ESDV0013
              FIELD TESTING COMPLETED 02 - 12 - 1964    ;               ESDV0014
                                                                        ESDV0015
                                                                        ESDV0016
         BEGIN                                                          ESDV0017
INTEGER       I, J, K, R, Q, CYCLE  ;                                   ESDV0018
                                                                        ESDV0019
REAL          A, B, C, COSA, COSB, COSC, SUMH, SUMK, SUML, PSI,         ESDV0020
              SIGMAPSI, DISTMAXH, DISTMINH, ONEMAXS, ONEMINS, TWOMAXS,  ESDV0021
              TWOMINS, PSIMAX, PSIMIN, HYPOT, LEG1, LEG2, SIGHYP,       ESDV0022
              SIGLEG1, SIGLEG2, RAD, ARGMAX, ARGMIN  ;                  ESDV0023
                                                                        ESDV0024
ARRAY         X, Y, Z, XXDC, YYDC, ZZDC, SIGMAX, SIGMAY, SIGMAZ[0:100], ESDV0025
              DELTAX, DELTAY, DELTAZ, DIST, SIGDIST, SIGX, SIGY,        ESDV0026
              SIGZ[0:100,0:100]  ;                                      ESDV0027
                                                                        ESDV0028
BOOLEAN       BOOL0, BOOL1  ;                                           ESDV0029
                                                                        ESDV0030
LABEL         ONSET, SETI, A2, A3, B3, A4, DISTSELECT,                  ESDV0031
              A5, PSISELECT, ALEPH, BETA, GAMMA, DELTA, ETA, FINISH  ;  ESDV0032
                                                                        ESDV0033
LIST          CELL(A,B,C,COSA,COSB,COSC),                               ESDV0034
              ATOMSS(X[I],SIGMAX[I],Y[I],SIGMAY[I],Z[I],SIGMAZ[I]),     ESDV0035
              DATADIST(I,J),                                            ESDV0036
              DATAPSI(I,J,K),                                           ESDV0037
              OUTX(I,X[I],SIGMAX[I],Y[I],SIGMAY[I],Z[I],SIGMAZ[I]),     ESDV0038
              OUTDIST(I,J,DIST[I,J],SIGDIST[I,J]),                      ESDV0039
              PSILIST(I,J,K,PSI,SIGMAPSI)  ;                            ESDV0040
                                                                        ESDV0041
FORMAT IN     FMIN1(X6,6F9.5),                                          ESDV0042
              FMIN2(2L5),                                               ESDV0043
              FMIN3(I2),                                                ESDV0044
              FMIN4(2I2),                                               ESDV0045
              FMIN5(3I2)  ;                                             ESDV0046
                                                                        ESDV0047
FORMAT OUT    STRUCTURE(X20,"4-HYDROXYPYRIDINE-N-OXIDE AFTER CYCLE NO.",ESDV0048
              I2,///),                                                  ESDV0049
              FORMX(I2,3(X7,2F9.5)),                                    ESDV0050
              COORDINATES(X1,"I",X12,"X",X5,"SIGMA-X",X12,"Y",X5,       ESDV0051
                          "SIGMA-Y",X12,"Z",X5,"SIGMA-Z",///),          ESDV0052
              DISTANCES(//,X1,"I",X4,"J",X9,"DISTANCE",X5,"ERROR",//),  ESDV0053
              ANGLES(//,X1,"I",X4,"J",X4,"K",X10,"ANGLE",X3,"ERROR",//),ESDV0054
              ERROR1("PSI VALUES INCORRECT"),                           ESDV0055
              FORMDIST(2(I2,X3),X4,2F10.5),                             ESDV0056
              FORMPSI(3(I2,X3),X4,2F8.2)  ;                             ESDV0057
                                                                        ESDV0058
FILE IN       FIN(1,10)  ;                                              ESDV0059
                                                                        ESDV0060
FILE OUT      FOU 1 (1,15)  ;                                           ESDV0061
                                                                        ESDV0062
PROCEDURE     STANDARD  ;                                               ESDV0063
         BEGIN                                                          ESDV0064
              DISTMAXH ~ SIGHYP | (0.25) + HYPOT  ;                     ESDV0065
              DISTMINH ~ - SIGHYP | (0.25) + HYPOT  ;                   ESDV0066
              ONEMAXS ~ - SIGLEG1 | (0.25) + LEG1  ;                    ESDV0067
              ONEMINS ~ + SIGLEG1 | (0.25) + LEG1  ;                    ESDV0068
              TWOMAXS ~ - SIGLEG2 | (0.25) + LEG2  ;                    ESDV0069
              TWOMINS ~ + SIGLEG2 | (0.25) + LEG2  ;                    ESDV0070
              ARGMAX ~ (ONEMAXS | ONEMAXS + TWOMAXS | TWOMAXS -         ESDV0071
                        DISTMAXH | DISTMAXH)/((2.0) | ONEMAXS |TWOMAXS);ESDV0072
              ARGMIN ~ (ONEMINS | ONEMINS + TWOMINS | TWOMINS -         ESDV0073
                        DISTMINH | DISTMINH)/((2.0) | ONEMINS |TWOMINS);ESDV0074
         END  STANDARD  ;                                               ESDV0075
                                                                        ESDV0076
REAL PROCEDURE     ARCCOS(X1)  ;                                        ESDV0077
                                                                        ESDV0078
VALUE         X1  ;                                                     ESDV0079
                                                                        ESDV0080
REAL          X1  ;                                                     ESDV0081
                                                                        ESDV0082
         BEGIN                                                          ESDV0083
REAL          PIC,MC,X,T,Z,PI2  ;                                       ESDV0084
                                                                        ESDV0085
LABEL         L1  ;                                                     ESDV0086
                                                                        ESDV0087
              IF ABS(X1) < 1@-6 THEN                                    ESDV0088
         BEGIN                                                          ESDV0089
              ARCCOS ~-X1 + 1.57079632679  ;                            ESDV0090
              GO TO L1                                                  ESDV0091
         END ;                                                          ESDV0092
              PIC ~ 0  ;                                                ESDV0093
              MC ~ 1  ;                                                 ESDV0094
              Z ~ SIGN(X1)  ;                                           ESDV0095
              X ~ ABS(X1)  ;                                            ESDV0096
              IF X > 0.92387953231 THEN                                 ESDV0097
         BEGIN                                                          ESDV0098
              X ~ SQRT((1-X)/2)  ;                                      ESDV0099
              PIC ~ 1.57079632679  ;                                    ESDV0100
              MC ~ -2                                                   ESDV0101
         END                                                            ESDV0102
              ELSE                                                      ESDV0103
         BEGIN                                                          ESDV0104
              IF X > 0.70710678119 THEN                                 ESDV0105
         BEGIN                                                          ESDV0106
              X ~2|(X*2) - 1  ;                                         ESDV0107
              PIC ~ 0.785398163397  ;                                   ESDV0108
              MC ~ 0.5                                                  ESDV0109
         END                                                            ESDV0110
         END ;                                                          ESDV0111
              T ~ X*2   ;                                               ESDV0112
              ARCCOS ~-((0.364541120348 - 4.22649415434/                ESDV0113
              (T - 8.66648256098 - 6.1228848016/                        ESDV0114
              (T - 3.2316720226 - 0.312873861283/                       ESDV0115
              (T - 1.63902626905 - 0.0268477822258/                     ESDV0116
              (T - 1.16535753774))))) | MC | X + PIC) | Z               ESDV0117
              + 1.57079632679  ;                                        ESDV0118
                                                                        ESDV0119
L1:      END ;                                                          ESDV0120
                                                                        ESDV0121
                                                                        ESDV0122
ONSET:        READ(FIN,FMIN1,CELL)  ;                                   ESDV0123
              READ(FIN,FMIN2,BOOL0,BOOL1)  ;                            ESDV0124
              READ(FIN,FMIN3,CYCLE)  ;                                  ESDV0125
              SUMH ~ SUMK ~ SUML ~ 0  ;  RAD ~ 57.2957795056  ;         ESDV0126
                                                                        ESDV0127
SETI:         I ~ 1  ;                                                  ESDV0128
                                                                        ESDV0129
A2:           READ(FIN,FMIN1,ATOMSS)  ;                                 ESDV0130
              IF ABS(X[I]) = 999.9 THEN GO TO A3  ;                     ESDV0131
              I ~ I + 1  ;  GO TO A2  ;                                 ESDV0132
                                                                        ESDV0133
A3:           R ~ I  ;                                                  ESDV0134
              WRITE(FOU,STRUCTURE,CYCLE)  ;                             ESDV0135
              WRITE(FOU,COORDINATES)  ;                                 ESDV0136
                                                                        ESDV0137
COMMENT       INPUT PARAMETERS ARE CHECKED BY LISTING THEM FIRST  ;     ESDV0138
                                                                        ESDV0139
B3:           FOR I ~ 1 STEP 1 UNTIL R DO                               ESDV0140
         BEGIN                                                          ESDV0141
              WRITE(FOU,FORMX,OUTX)  ;                                  ESDV0142
         END ;                                                          ESDV0143
              WRITE(FOU,DISTANCES)  ;                                   ESDV0144
                                                                        ESDV0145
COMMENT       ALL DISTANCES [I,J] ARE COMPUTED FOR THE STRUCTURE,       ESDV0146
              FOR THE N|(N-1)/2 PERMISSIBLE PERMUTATIONS  ;             ESDV0147
                                                                        ESDV0148
A4:           FOR I ~ 1 STEP 1 UNTIL (R - 2) DO                         ESDV0149
         BEGIN                                                          ESDV0150
              Q ~ I + 1  ;                                              ESDV0151
              FOR J ~ Q STEP 1 UNTIL (R - 1) DO                         ESDV0152
         BEGIN                                                          ESDV0153
              DELTAX[I,J] ~ X[I] - X[J]  ;                              ESDV0154
              DELTAY[I,J] ~ Y[I] - Y[J]  ;                              ESDV0155
              DELTAZ[I,J] ~ Z[I] - Z[J]  ;                              ESDV0156
              DIST[I,J] ~ SQRT((DELTAX[I,J] | A)*2 + (DELTAY[I,J] |     ESDV0157
                          B)*2 + (DELTAZ[I,J] | C)*2 +DELTAX[I,J] |     ESDV0158
                          DELTAY[I,J] | A | B | (2.0) | COSC +          ESDV0159
                          DELTAX[I,J] | DELTAZ[I,J] | A | C | (2.0) |   ESDV0160
                          COSB + DELTAY[I,J] | DELTAZ[I,J] | B | C |    ESDV0161
                          (2.0) | COSA)  ;                              ESDV0162
              SIGX[I,J] ~ A | SQRT(SIGMAX[I]*2 + SIGMAX[J]*2)  ;        ESDV0163
              SIGY[I,J] ~ A | SQRT(SIGMAY[I]*2 + SIGMAY[J]*2)  ;        ESDV0164
              SIGZ[I,J] ~ A | SQRT(SIGMAZ[I]*2 + SIGMAZ[J]*2)  ;        ESDV0165
              SIGDIST[I,J] ~ (0.4549) | SQRT(SIGX[I,J] | SIGX[I,J] +    ESDV0166
                             SIGY[I,J] | SIGY[I,J] +                    ESDV0167
                             SIGZ[I,J] | SIGZ[I,J] +                    ESDV0168
                             SIGX[I,J] | SIGY[I,J] | (2.0) | COSC +     ESDV0169
                             SIGX[I,J] | SIGZ[I,J] | (2.0) | COSB +     ESDV0170
                             SIGY[I,J] | SIGZ[I,J] | (2.0) | COSA)  ;   ESDV0171
              IF NOT BOOL0 THEN WRITE(FOU,FORMDIST,OUTDIST)  ;          ESDV0172
         END                                                            ESDV0173
         END ;                                                          ESDV0174
                                                                        ESDV0175
              IF NOT BOOL0 THEN GO TO A5  ;                             ESDV0176
                                                                        ESDV0177
COMMENT       BY BOOL0 OPTION ONLY THOSE DISTANCES ARE SELECTED FOR     ESDV0178
              WHICH THE INDICES [I,J] ARE SPECIFIED BY INPUT  ;         ESDV0179
                                                                        ESDV0180
DISTSELECT:   READ(FIN,FMIN4,DATADIST)  ;                               ESDV0181
              IF I = 99 THEN GO TO A5  ;                                ESDV0182
              WRITE(FOU,FORMDIST,OUTDIST)  ;                            ESDV0183
              GO TO DISTSELECT  ;                                       ESDV0184
                                                                        ESDV0185
A5:           IF BOOL1 THEN GO TO FINISH  ;                             ESDV0186
              WRITE(FOU,ANGLES)  ;                                      ESDV0187
                                                                        ESDV0188
COMMENT       THE ANGLES [I,J,K] ARE COMPUTED FROM                      ESDV0189
              SPECIFIED INDICES, THE LARGEST ANGLE OF THE               ESDV0190
              TRIANGLE BEING SELECTED BY CONVENTION  ;                  ESDV0191
                                                                        ESDV0192
PSISELECT:    READ(FIN,FMIN5,DATAPSI)  ;                                ESDV0193
              IF I = 99 THEN GO TO FINISH  ;                            ESDV0194
              IF  DIST[I,J] } DIST[I,K] THEN GO TO ALEPH  ;             ESDV0195
              IF  DIST[I,J] } DIST[J,K] OR                              ESDV0196
                  DIST[I,K] } DIST[J,K] THEN GO TO BETA  ;              ESDV0197
              GO TO GAMMA  ;                                            ESDV0198
                                                                        ESDV0199
ALEPH:        IF DIST[I,J] } DIST[J,K] THEN GO TO DELTA  ;              ESDV0200
              GO TO GAMMA  ;                                            ESDV0201
                                                                        ESDV0202
BETA:         HYPOT ~ DIST[I,K]  ;  SIGHYP ~ SIGDIST[I,K]  ;            ESDV0203
              LEG1 ~ DIST[J,K]  ;  SIGLEG1 ~ SIGDIST[J,K]  ;            ESDV0204
              LEG2 ~ DIST[I,J]  ;  SIGLEG2 ~ SIGDIST[I,J]  ;            ESDV0205
                                                                        ESDV0206
              STANDARD  ;  GO TO ETA  ;                                 ESDV0207
                                                                        ESDV0208
GAMMA:        HYPOT ~ DIST[J,K]  ;  SIGHYP ~ SIGDIST[I,K]  ;            ESDV0209
              LEG1 ~ DIST[I,K]  ;  SIGLEG1 ~ SIGDIST[J,K]  ;            ESDV0210
              LEG2 ~ DIST[I,J]  ;  SIGLEG2 ~ SIGDIST[I,J]  ;            ESDV0211
                                                                        ESDV0212
              STANDARD  ;  GO TO ETA  ;                                 ESDV0213
                                                                        ESDV0214
DELTA:        HYPOT ~ DIST[I,J]  ;  SIGHYP ~ SIGDIST[I,K]  ;            ESDV0215
              LEG1 ~ DIST[J,K]  ;  SIGLEG1 ~ SIGDIST[J,K]  ;            ESDV0216
              LEG2 ~ DIST[I,K]  ;  SIGLEG2 ~ SIGDIST[I,J]  ;            ESDV0217
                                                                        ESDV0218
              STANDARD  ;                                               ESDV0219
                                                                        ESDV0220
ETA:          PSIMAX ~ RAD | ARCCOS(ARGMAX)  ;                          ESDV0221
              PSIMIN ~ RAD | ARCCOS(ARGMIN)  ;                          ESDV0222
                                                                        ESDV0223
              PSI ~ (0.5) | (PSIMAX + PSIMIN)  ;                        ESDV0224
              SIGMAPSI ~ (0.5) | (PSIMAX - PSIMIN)  ;                   ESDV0225
              WRITE(FOU,FORMPSI,PSILIST)  ;                             ESDV0226
              GO TO PSISELECT  ;                                        ESDV0227
                                                                        ESDV0228
FINISH:       END.                                                      ESDV0229
 LABEL  000000000FIN    0010000001
        3.900    6.512   13.215   -0.46468 -0.51786  0.07359
FALSEFALSE
28
         .07569   .01285   .68016   .00879   .37631   .00447
         .09407   .01575   .44268   .00928   .34302   .00520
        -.02166   .02004   .26518   .01239   .20517   .00685
         .00568   .01460   .59605   .00942   .17178   .00484
        -.01106   .01720   .73511   .00920   .27802   .00627
        -.01767   .01324   .36355   .00854   .13425   .00491
        -.11704   .01234   .20296   .00784  -.00150   .00470
         .05457   .01322   .81811   .00798   .48351   .00470
      999.9      0.0      0.0      0.0      0.0      0.0
 1 2 3
 1 2 5
 1 2 8
 1 4 5
 1 5 8
 2 3 6
 3 4 6
 3 6 7
 4 6 7
 4 5 6
99 0 0
