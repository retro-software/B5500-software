         BEGIN                                                          BDIS0001
COMMENT       BATCH DISTILLATION PROGRAM                                BDIS0002
              BY DONALD L. VOSS                                         BDIS0003
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       BDIS0004
              CARD SEQUENCE CODE STARTS WITH BDIS0001.                  BDIS0005
              FIRST RELEASE DATE     10-28-64     ;                     BDIS0006
                                                                        BDIS0007
REAL          P, TDIFF, STOP, QM, POIR1, R, DELQ1, DELQM, SUMD, SUMR,   BDIS0008
              DELD ;                                                    BDIS0009
INTEGER       M, N, KEY, MAXIT, I, J, IDNO, STIME1, DAY, YEAR, IPOIR1,  BDIS0010
              ELMIN, ELSEC, PRMIN, PRSEC, IOMIN, IOSEC, STIME2, STIME3, BDIS0011
              READF, READH, READIE, READCD, POPD, POCD, POIR, POIR2 ;   BDIS0012
ALPHA         MONTH ;                                                   BDIS0013
LABEL         BD1, EOFCARD ;                                            BDIS0014
REAL ARRAY    TOL[0:3], LIMIT[0:2], SPEC[0:3] ;                         BDIS0015
BOOLEAN ARRAY CALCDHV[0:3] ;                                            BDIS0016
ALPHA ARRAY   ID[0:12] ;                                                BDIS0017
FILE IN       FIN1 (2, 10) ;                                            BDIS0018
FILE OUT      FOUT1 1 (2, 15) ;                                         BDIS0019
FORMAT IN     FNAME (2A6, 3F20.10),                                     BDIS0020
              FKH  (E17.8, 3E16.8, X1, A2, 2A6) ;                       BDIS0021
FORMAT OUT    FID1 ("BATCH DISTILLATION PROGRAM" // "INPUT DATA PRINT-",BDIS0022
                   "OUT" // "PROBLEM NUMBER", I15                       BDIS0023
                   ///  6A6 // 6A6 /// "TOTAL NUMBER OF TRAYS", I11,    BDIS0024
                   X7,"COLUMN PRESSURE, PSIA",F15.3//"TOTAL NUMBER OF ",BDIS0025
                   "COMPONENTS",I6,X7,"TEMPERATURE TOLERANCE, F.",      BDIS0026
                   F11.3//"KEY COMPONENT NUMBER",I12,X7,"MOL FRACTION ",BDIS0027
                   "TOLERANCE, %",F11.3//"MAX. NO. OF ITERATIONS",I10,X7BDIS0028
                   , "HOLDUP TOLERANCE, %", F17.3//"LOWER LIMIT ON T, ",BDIS0029
                   "F.", X7, I5, X7, "UPPER LIMIT ON T, F.", X9, I7 /// BDIS0030
                   "PROBLEM SPECIFICATIONS:    % OF INITIAL FEED WITH", BDIS0031
                   "DRAWN", F8.1, // X27, "REFLUX RATIO, L1/D", F17.1), BDIS0032
              FID3 (X27, "MAXIMUM REFLUX RATIO, L1/D", F9.1 // X27,     BDIS0033
                   "SPECIFICATION COMPONENT NO.", I6),                  BDIS0034
              FID4 (X27, "% OF INITIAL FEED WITHDRAWN", F8.1 // X27,    BDIS0035
                   "NO. OF CYCLES", I20),                               BDIS0036
              FID5 (// "FEED MOLS, TRAY HOLDUP (CU. FT.), AND INITIAL ",BDIS0037
                   "ESTIMATES:"//X4,"HUM[M] =",F10.4," MOLS    T[1] =", BDIS0038
                   F6.1, " F.     T[M] = ", F6.1, " F." // X6, "I", X9, BDIS0039
                   "F[I]", X11, "X[M,I]", X9, "J", X7, "HUV[J]"),       BDIS0040
              FID6 (I7, F16.4, E16.5, I8, F14.5),                       BDIS0041
              FID7 (I7, F16.4, E16.5),                                  BDIS0042
              FID8 (X39, I8, F14.5),                                    BDIS0043
              FID9 ("COMPONENT K-RATIO AND ENTHALPY DATA:" //           BDIS0044
                   (2A6, X6, "TC[I] =", F9.3, X5, "PC[I] =", F9.3, X5,  BDIS0045
                   "S[I] =", F10.5 /  7(4E16.8, X1, A2, 2A6 / )         BDIS0046
                   4E16.8, X1, A2, 2A6 /  4E16.8, X1, A2, 2A6  // )),   BDIS0047
              FERR (// "THIS PROBLEM WAS TERMINATED DUE TO THE",        BDIS0048
                   " FOLLOWING REASON:" / ),                            BDIS0049
              FERR1 (/ "THE MATRIX USED IN THE STARTUP PROCEDURE IS ",  BDIS0050
                   "SINGULAR OR ILL-CONDITIONED" / ),                   BDIS0051
              FERR2 (/ "THE MAXIMUM NUMBER OF ITERATIONS ALLOWED IN ",  BDIS0052
                   "THE STARTUP PROCEDURE WAS EXCEEDED" / ),            BDIS0053
              FERR3 ( / "THE TEMPERATURE LIMITS WERE EXCEEDED IN THE ", BDIS0054
                   "TRANSIENT PROCEDURE"),                              BDIS0055
              FSS1 ("BATCH DISTILLATION PROGRAM" // "PRINT-OUT OF ",    BDIS0056
                   "STEADY-STATE CONDITIONS AT TOTAL REFLUX" //         BDIS0057
                   "PROBLEM NUMBER", I15 /// 6A6 // 6A6 /// "T[J]:" /   BDIS0058
                   (8F10.2)),                                           BDIS0059
              FSS2 (// "HUM[J]:" / (8F10.4)),                           BDIS0060
              FSS3 (// "X[J,I]:"),                                      BDIS0061
              FSS4 (5E15.7),                                            BDIS0062
              FFP1 ("BATCH DISTILLATION PROGRAM" // "PRINT-OUT OF ",    BDIS0063
                   "FINAL RESULTS AT END OF DISTILLATION" //            BDIS0064
                   "PROBLEM NUMBER", I15 /// 6A6 // 6A6 ///             BDIS0065
                   "DATE", X17, A6, I3, ",", I5 // "ELAPSED TIME",      BDIS0066
                   I10, " MIN.,", I3, " SEC." // "PROCESSOR TIME", I8,  BDIS0067
                   " MIN.," I3, " SEC." // "I/O TIME", I14, " MIN.,",   BDIS0068
                   I3, " SEC." //                                       BDIS0069
                                                                        BDIS0070
                                                                        BDIS0071
                   /" TRAY     TEMPERATURE   TRAY LIQUID    TRAY LIQUI",BDIS0072
                   "D     X SUMMATION   Y SUMMATION" / X10, "OF TRAY, ",BDIS0073
                   "F.   HOLDUP, MOLS   HOLDUP,CU.FT.   ON TRAY",       BDIS0074
                   "       ON TRAY"),                                   BDIS0075
              FFP2 (I4, F15.2, X1, 2F14.4, X2, 2F14.7),                 BDIS0076
              FFP3 (/// "TOTAL MOLS OF DISTILLATE WITHDRAWN: ", F15.4,  BDIS0077
                   /// " COMPONENT MOL FRACTIONS OF TOTAL DISTILLATE ", BDIS0078
                   "WITHDRAWN FROM COLUMN:" / (5E15.7)),                BDIS0079
              FFP4 (/// " COMPONENT MOLS OF TOTAL DISTILLATE WITHDRAWN",BDIS0080
                   " FROM COLUMN:" / (5F15.3)),                         BDIS0081
              FFP5 (/// " TRAY LIQUID MOL FRACTIONS, X[J,I]:"),         BDIS0082
              FFP6 (5E15.7),                                            BDIS0083
              FFP7 (/// " TRAY VAPOR MOL FRACTIONS, Y[J,I]:") ;         BDIS0084
                                                                        BDIS0085
PROCEDURE     DATE (MONTH, DAY, YEAR) ;                                 BDIS0086
COMMENT       THIS PROCEDURE CALCULATES THE MONTH, DAY OF THE MONTH,    BDIS0087
              AND YEAR FROM THE TIME(0) FUNCTION.  MONTH IS ALPHA.  DAY BDIS0088
              AND YEAR ARE INTEGERS. ;                                  BDIS0089
ALPHA         MONTH ;                                                   BDIS0090
INTEGER       DAY, YEAR ;                                               BDIS0091
              BEGIN                                                     BDIS0092
INTEGER            T1, T6, T7, T9, DAZ, X ;                             BDIS0093
INTEGER ARRAY MESS[0:13] ;                                              BDIS0094
ALPHA ARRAY   MO [1:12] ;                                               BDIS0095
INTEGER PROCEDURE ICV1 (SO, CH, SGN, LGTH) ;                            BDIS0096
VALUE         CH, SGN, LGTH ;                                           BDIS0097
INTEGER       CH, SGN, LGTH ;                                           BDIS0098
REAL          SO ;                                                      BDIS0099
         BEGIN                                                          BDIS0100
INTEGER       EDE ;                                                     BDIS0101
INTEGER STREAM PROCEDURE IC8 (SO, CH, LGTH) ;                           BDIS0102
VALUE         CH, LGTH ;                                                BDIS0103
         BEGIN                                                          BDIS0104
              DI ~ LOC IC8 ;      SI ~ SO ;     SI ~ SI + CH ;          BDIS0105
              DS ~ LGTH OCT                                             BDIS0106
         END  OF IC8 PROCEDURE ;                                        BDIS0107
              ICV1 ~ IC8 (SO, CH, LGTH)                                 BDIS0108
         END  OF ICV1 PROCEDURE ;                                       BDIS0109
              FILL MO[*] WITH "  JAN.", "  FEB.", " MARCH", " APRIL",   BDIS0110
              "   MAY", "  JUNE", "  JULY", "AUGUST", " SEPT.",         BDIS0111
              "  OCT.", "  NOV.", "  DEC." ;                            BDIS0112
              T6 ~ TIME(0) ;                                            BDIS0113
              T6 ~ ICV1 (T6, 3, 0, 5) ;                                 BDIS0114
              T1 ~ T6 MOD 1000 ;                                        BDIS0115
              IF T1 > 368 THEN T1 ~ 1 ;                                 BDIS0116
              T9 ~ T6 DIV 1000 ;                                        BDIS0117
              X ~ IF (T9 MOD 4) = 0 THEN 1 ELSE 0 ;                     BDIS0118
              MESS[0] ~ 0 ;                                             BDIS0119
              MESS[1] ~ 31 ;     MESS[2] ~ 59 + X ;                     BDIS0120
              MESS[3] ~ 90 + X ;     MESS[4] ~ 120 + X ;                BDIS0121
              MESS[5] ~ 151 + X ;     MESS[6] ~ 181 + X ;               BDIS0122
              MESS[7] ~ 212 + X ;     MESS[8] ~ 243 + X ;               BDIS0123
              MESS[9] ~ 273 + X ;     MESS[10] ~ 304 + X ;              BDIS0124
              MESS[11] ~ 334 + X ;     MESS[12] ~ 367 ;                 BDIS0125
              MESS[13] ~ 1000 ;                                         BDIS0126
              X ~ 0 ;   WHILE T1 > MESS[X] DO                           BDIS0127
         BEGIN                                                          BDIS0128
              X ~ X + 1 ;     T7 ~ X                                    BDIS0129
         END ;                                                          BDIS0130
              DAZ ~ T1 - MESS[X-1] ;                                    BDIS0131
              MONTH ~ MO[T7] ;     DAY ~ DAZ ;     YEAR ~ T9 + 1900     BDIS0132
         END  OF DATE PROCEDURE ;                                       BDIS0133
                                                                        BDIS0134
COMMENT       DECLARATIONS AND FILL STATEMENTS FOR THE GLOBAL ARRAYS    BDIS0135
              OF THE HVPCOR PROCEDURE ;                                 BDIS0136
REAL ARRAY    HVC3, HVC4, HVC7, HVC8[0:2,0:6], HVC5, HVC9[0:49],        BDIS0137
              TR1S3, TR1S4[0:2] ;                                       BDIS0138
INTEGER ARRAY HVC2[0:2] ;                                               BDIS0139
              FILL HVC2[*] WITH   0,         6,             6          ;BDIS0140
              FILL HVC3[1,*] WITH 0.0,      -1.1671875,    -1.4894278,  BDIS0141
               -1.1559073,    -1.3616489,   -1.2883117,    -1.3995702  ;BDIS0142
              FILL HVC3[2,*] WITH 0.0,      -1.2935283,    -2.9748991,  BDIS0143
               -2.4260566,    -1.8061761,   -1.5492552,    -1.4278796  ;BDIS0144
              FILL HVC4[1,*] WITH 0.0,       0.0,          -0.10587647, BDIS0145
               -0.15860289,   -0.15248362,  -0.11896855,   -0.12173253 ;BDIS0146
              FILL HVC4[2,*] WITH 0.0,       0.0,          -1.7678550,  BDIS0147
               -1.8686137,   -1.2165188,    -0.67716468,   -0.61318349 ;BDIS0148
              FILL HVC5[*] WITH 0.0,        -0.68264275,   -1.0872225,  BDIS0149
                0.047697762,  -0.024306088, -0.0090858597, -0.001729015,BDIS0150
               -0.0027521429, -2.2596067,    0.11921812,   -0.046204775,BDIS0151
                0.014351991,  -0.023163497,  0.025471511,   0.013133272,BDIS0152
                0.78097570,   -0.17094139,   0.098209577,  -0.044902667,BDIS0153
                0.015565173,  -0.010983720,  0.013622787,   0.28893313, BDIS0154
               -0.25370399,    0.0052142570, 0.071616235,   0.046127269,BDIS0155
               -0.11245569,   -0.067557734, -2.5307361,    -0.039864924,BDIS0156
               -0.12607210,    0.13765330,  -0.039550505,  0.0075315843,BDIS0157
               -0.21354763,    2.2361128,   -0.36080301,   0.49691013,  BDIS0158
               -0.41863117,   -0.076349532,  0.43597890,  -0.40507180,  BDIS0159
                0.26338300,   0.0083735620, -0.016716827, -0.016240225, BDIS0160
                0.10507060,   -0.12106818,   0.025741676 ;              BDIS0161
              FILL HVC7[1,*] WITH 0.0,      -1.2729166,    -1.6239677,  BDIS0162
               -1.5317194,    -1.4788939,   -1.4345722,    -1.5801600  ;BDIS0163
              FILL HVC7[2,*] WITH 0.0,      -6.5718750,   -12.149681,   BDIS0164
              -11.497100,    -11.065928,   -10.278189,    -10.667091   ;BDIS0165
              FILL HVC8[1,*] WITH 0.0,       0.0,          -0.090954100,BDIS0166
               -0.056326597,  -0.075512113, -0.071434341,  -0.063708461;BDIS0167
              FILL HVC8[2,*] WITH 0.0,       0.0,         -41.217490,   BDIS0168
              -22.312134,    -21.043415,   -20.693912,    -26.268748   ;BDIS0169
              FILL HVC9[*]   WITH 0.0,      39.745103,      6.3744973,  BDIS0170
              -0.11410040, -0.0036340168, -8.2198154@-5, -2.1580705@-5, BDIS0171
                2.9001251@-6, -44.671381,    -6.7048192,  0.073453717,  BDIS0172
               -0.0066641733,  0.0017309322, -9.824585@-5, 1.343254@-5, BDIS0173
               41.351011,      4.5915409,   -0.029361153,  0.0018827038,BDIS0174
               -0.0040032069,  7.6001770@-4, -9.750253@-5, -27.356096,  BDIS0175
                0.97045128,   -0.28080321,   0.032749281,  0.0074142633,BDIS0176
               -0.0017002214,  4.3344771@-4, 39.904305,    -4.4069227,  BDIS0177
                0.52546214,   -0.092967052,  0.0057307519, 0.0034063759,BDIS0178
               -1.2990048@-4, 46.719260,     8.5852725,    -0.62558932, BDIS0179
               -0.011829404,  -0.041450650,  0.014043180, -9.8770289@-4,BDIS0180
               -3.7232051,    -0.30920320,  -0.0049008792, 0.0011831959,BDIS0181
                9.1663621@-4, -1.8376908@-4, 3.8674416@-5              ;BDIS0182
                                                                        BDIS0183
COMMENT       READ IN DATA SET 1 ;                                      BDIS0184
BD1:          STIME1 ~ TIME(1) ;     STIME2 ~ TIME(2) ;                 BDIS0185
              STIME3 ~ TIME(3) ;                                        BDIS0186
              READ (FIN1, /, FOR I ~ 1 STEP 1 UNTIL 12 DO ID[I],        BDIS0187
              IDNO, M, N, KEY, MAXIT, P,  TOL[1], TOL[2], TOL[3],       BDIS0188
              LIMIT[1], LIMIT[2], POPD, POCD, POIR, IPOIR1, POIR2, QM,  BDIS0189
              SPEC[1], SPEC[2] ) [EOFCARD] ;                            BDIS0190
                                                                        BDIS0191
         BEGIN                                                          BDIS0192
COMMENT       START OF BLOCK 2 OF PROGRAM ;                             BDIS0193
OWN REAL ARRAY F, S, TC, PC[0:N],                                       BDIS0194
              HUM, HUV, DELL, DELV, T[0:M],                             BDIS0195
              X, Y[0:M,0:N] ;                                           BDIS0196
OWN ALPHA ARRAY NAME1, NAME2[0:N], AHV, AHL[0:N,0:3], AK[0:N,0:7,0:3] ; BDIS0197
LABEL         BD2, ERR1, ERR2, ERR3 ;                                   BDIS0198
                                                                        BDIS0199
COMMENT       GLOBAL ARRAYS FOR THE KNGAA PROCEDURE ;                   BDIS0200
OWN REAL ARRAY KC1[0:N,0:28], KC15[0:N,0:4], KC16[0:7] ;                BDIS0201
                                                                        BDIS0202
COMMENT       GLOBAL ARRAY FOR THE HVPCOR PROCEDURE ;                   BDIS0203
OWN REAL ARRAY HVC1[0:N,0:4] ;                                          BDIS0204
                                                                        BDIS0205
COMMENT       GLOBAL ARRAY FOR THE HLIQ PROCEDURE ;                     BDIS0206
OWN REAL ARRAY HLC5[0:N,0:4] ;                                          BDIS0207
                                                                        BDIS0208
COMMENT       GLOBAL ARRAYS FOR THE PHIPART PROCEDURE ;                 BDIS0209
INTEGER ARRAY TI1S1, TI1S2[0:3], TI1S3[0:2] ;                           BDIS0210
                                                                        BDIS0211
COMMENT       GLOBAL ARRAY FOR THE PHIPART AND TRANSIENT PROCEDURES ;   BDIS0212
REAL ARRAY    TR1S2 [0: IF (M>N-1 AND M>2) THEN M ELSE IF N-1>2 THEN    BDIS0213
              N-1 ELSE 2] ;                                             BDIS0214
                                                                        BDIS0215
COMMENT       GLOBAL ARRAY FOR THE KNGAA, PHIPART, AND TRANSIENT        BDIS0216
              PROCEDURES ;                                              BDIS0217
REAL ARRAY    TR1S1 [0:IF (M>N-1 AND M>14) THEN M ELSE IF 14>N-1 THEN 14BDIS0218
              ELSE N-1] ;                                               BDIS0219
                                                                        BDIS0220
COMMENT       GLOBAL ARRAYS FOR THE STARTUP PROCEDURE ;                 BDIS0221
REAL ARRAY    PHUMT[0:M,0:M], PHUMX[0:M,0:N], A[0:M+N,0:M+N], B,        BDIS0222
              C[0:M+N] ;                                                BDIS0223
                                                                        BDIS0224
COMMENT       GLOBAL ARRAYS FOR THE TRANSIENT PROCEDURE ;               BDIS0225
REAL ARRAY    HV, HL, PHLT[0:M], PHLX[0:M,0:N], D[0:N] ;                BDIS0226
                                                                        BDIS0227
COMMENT       GLOBAL ARRAYS FOR THE STARTUP AND TRANSIENT PROCEDURES ;  BDIS0228
REAL ARRAY    K, PKT[0:M,0:N], SBAR[0:M] ;                              BDIS0229
                                                                        BDIS0230
COMMENT                            .               .                    BDIS0231
                                   .               .                    BDIS0232
                                   .               .                    BDIS0233
                                   .               .                    BDIS0234
                                   .               .                    BDIS0235
              THE KNGAA, PHIPART, HLIQ, HVPCOR, AND SOLVE PROCEDURES    BDIS0236
              MUST BE INSERTED HERE, IN THAT ORDER.                     BDIS0237
                                   .               .                    BDIS0238
                                   .               .                    BDIS0239
                                   .               .                    BDIS0240
                                   .               .                    BDIS0241
                                   .               .                   ;BDIS0242
                                                                        BDIS0243
PROCEDURE     STARTUP (M, N, KEY, P, F, S, HUV, TOL, LIMIT, MAXIT, POIR,BDIS0244
              T, HUM, X, ERR1, ERR2) ;                                  BDIS0245
                                                                        BDIS0246
COMMENT       THIS PROCEDURE CALCULATES THE STEADY-STATE CONDITIONS OF  BDIS0247
              A BATCH DISTILLATION COLUMN OPERATING AT TOTAL REFLUX.    BDIS0248
              THE TEMPERATURE, COMPONENT MOL FRACTIONS, AND MOLS OF     BDIS0249
              LIQUID HOLDUP ON EACH TRAY ARE CALCULATED BY MEANS OF A   BDIS0250
              NEWTON-RAPHSON TECHNIQUE.                                 BDIS0251
                                                                        BDIS0252
              THE INPUT PARAMETERS ARE                                  BDIS0253
              M    - TOTAL NUMBER OF TRAYS  - INTEGER                   BDIS0254
              N    - TOTAL NUMBER OF COMPONENTS  - INTEGER              BDIS0255
              KEY  - KEY COMPONENT NUMBER  - INTEGER                    BDIS0256
              P    - COLUMN PRESSURE, PSIA  - REAL                      BDIS0257
              F    - COMPONENT FEED MOLS  - REAL ARRAY [0:N], WHERE     BDIS0258
                   F[0] = TOTAL MOLS OF FEED.                           BDIS0259
              S    - COMPONENT LIQUID MOLAR VOLUMES, CU. FT./MOL  -     BDIS0260
                   REAL ARRAY[0:N]                                      BDIS0261
              HUV  - LIQUID HOLDUP ON TRAYS, CU. FT. - REAL ARRAY[0:M-1]BDIS0262
              TOL  - TOLERANCES ON VARIABLES  - REAL ARRAY [0:3]        BDIS0263
                   TOL[1] = TRAY TEMPERATURES, DEG. F.                  BDIS0264
                   TOL[2] = REBOILER LIQUID MOL FRACTIONS, PER CENT     BDIS0265
                   TOL[3] = REBOILER LIQUID MOLAR HOLDUP, PER CENT      BDIS0266
              LIMIT - TEMPERATURE LIMITS - REAL ARRAY [0:2]             BDIS0267
                   LIMIT[1] = MINIMUM T, F.                             BDIS0268
                   LIMIT[2] = MAXIMUM T, F.                             BDIS0269
              MAXIT - MAXIMUM NUMBER OF ITERATIONS ALLOWED  - INTEGER   BDIS0270
              POIR - INTEGER.  IF = 1, PRINT INTERMEDIATE RESULTS.      BDIS0271
              T    - INITIAL ESTIMATES OF THE TRAY TEMPERATURES,        BDIS0272
                   DEG. F.  - REAL ARRAY [0:M]                          BDIS0273
              HUM[M] - INITIAL ESTIMATE OF REBOILER LIQUID HOLDUP, MOLS BDIS0274
                   (SEE OUTPUT PARAMETERS)                              BDIS0275
              X[M,I] - INITIAL ESTIMATES OF REBOILER LIQUID MOL         BDIS0276
                   MOL FRACTIONS (SEE OUTPUT PARAMETERS)                BDIS0277
                                                                        BDIS0278
              IN ADDITION, THE REAL ARRAYS A[0:M+N,0:M+N], B, C[0:M+N], BDIS0279
              K, PKT[0:M,0:N], PHUMT[0:M,0:M], PHUMX[0:M,0:N], AND      BDIS0280
              SBAR[0:M] PLUS THE GLOBAL ARRAYS OF KNGAA ARE GLOBAL      BDIS0281
              TO STARTUP.                                               BDIS0282
                                                                        BDIS0283
              ALSO, THE OUTPUT FILE, FOUT1, MUST BE DECLARED GLOBAL TO  BDIS0284
              STARTUP.  IT IS USED TO PRINT THE INTERMEDIATE RESULTS.   BDIS0285
                                                                        BDIS0286
              THE OUTPUT PARAMETERS ARE                                 BDIS0287
              T    - TRAY TEMPERATURES, DEG. F. - REAL ARRAY [0:M]      BDIS0288
              HUM  - LIQUID HOLDUP ON TRAYS, MOLS - REAL ARRAY [0:M]    BDIS0289
              X    - LIQUID TRAY COMPOSITIONS, MOL FRACTIONS  -         BDIS0290
                   REAL ARRAY [0:M,0:N]                                 BDIS0291
                                                                        BDIS0292
              THE REFERENCE PARAMETERS ARE                              BDIS0293
              ERR1 - REFERENCE STATEMENT LABEL TO WHICH CONTROL IS      BDIS0294
                   CHANGED IF THE MATRIX USED IN STARTUP IS SINGULAR OR BDIS0295
                   ILL-CONDITIONED.                                     BDIS0296
              ERR2 - REFERENCE STATEMENT LABEL TO WHICH CONTROL IS      BDIS0297
                   CHANGED IF MAXIT IS EXCEEDED.                        BDIS0298
                                                                        BDIS0299
              THE PROCEDURES SOLVE AND KNGAA MUST BE DECLARED PRIOR TO  BDIS0300
              STARTUP.  CARDS 0230 AND 0250 OF SOLVE WERE MODIFIED TO   BDIS0301
              REMOVE THE OWN TYPE.  ;                                   BDIS0302
                                                                        BDIS0303
VALUE         N, M, KEY, P, F, S, HUV, TOL, LIMIT, MAXIT, POIR ;        BDIS0304
REAL          P ;                                                       BDIS0305
INTEGER       N, M, KEY, MAXIT ;                                        BDIS0306
INTEGER       POIR ;                                                    BDIS0307
REAL ARRAY    F, S, HUV, TOL, LIMIT, T, HUM[0], X[0,0] ;                BDIS0308
LABEL         ERR1, ERR2 ;                                              BDIS0309
                                                                        BDIS0310
         BEGIN                                                          BDIS0311
REAL          SUMX, SUMSX, PXM, EPS, CORR, TCORR, MAXDT, MAXDH,         BDIS0312
              MAXDX, LHFAC, UHFAC, LXFAC, UXFAC ;                       BDIS0313
INTEGER       I, J, Q, MPN, MPI, G, MPG, L, ITER ;                      BDIS0314
BOOLEAN       CALCPDC, RECYCLE, IR2, IR3, IR4, IR5, IR6, IR7, IR13,     BDIS0315
              IR14, IR15, IR16 ;                                        BDIS0316
LABEL         S1, S2, S3, S4, S5 ;                                      BDIS0317
FORMAT OUT    FIR1 (//"CORRECTIONS MULTIPLIED BY", F8.4),               BDIS0318
              FIR2 (  "X[M,I] SET TO LOWER LIMIT"),                     BDIS0319
              FIR3 (  "SUMX > 1 NORMALIZED" ),                          BDIS0320
              FIR4 (  "T SET TO LOWER LIMIT" ),                         BDIS0321
              FIR5 (  "T SET TO UPPER LIMIT" ),                         BDIS0322
              FIR6 (  "DELTA T SET TO MAXDT"),                          BDIS0323
              FIR7 (  "HUM[M] SET TO LOWER LIMIT"),                     BDIS0324
              FIR8 (/ "PRINT-OUT OF INTERMEDIATE RESULTS IN STARTUP"),  BDIS0325
              FIR9 (  "HUM[M] = ", F10.4 / "X[M,I]:" / (8E15.5) ),      BDIS0326
              FIR10 (  "T[J]:" / (12F10.2) ),                           BDIS0327
              FIR11(   "OVER-ALL MATERIAL UNBALANCE =", E14.5           BDIS0328
                   /   "COMPONENT MATERIAL UNBALANCE:" / (8E15.5)),     BDIS0329
              FIR12 (  "EQUILIBRIUM CONDITION UNBALANCES:" / (8E15.5)), BDIS0330
              FIR14 ( "X[M,I] SET TO UPPER LIMIT"),                     BDIS0331
              FIR15 ( "DELTA X[M,I] SET TO MAXDX"),                     BDIS0332
              FIR16 ( "DELTA HUM[M] SET TO MAXDH") ;                    BDIS0333
                                                                        BDIS0334
              RECYCLE ~ TRUE ;         ITER ~ 1 ;                       BDIS0335
                                                                        BDIS0336
COMMENT       PRINT OUT INTERMEDIATE RESULTS HEADING IF POIR = 1 ;      BDIS0337
              IF POIR = 1 THEN                                          BDIS0338
         BEGIN                                                          BDIS0339
              FOR I ~ 1 STEP 1 UNTIL M + N DO  C[I] ~ 0 ;               BDIS0340
              IR2 ~ IR3 ~ IR4 ~ IR5 ~ IR6 ~ IR7 ~ FALSE ;               BDIS0341
              WRITE (FOUT1[PAGE]) ;     WRITE (FOUT1, FIR8)             BDIS0342
         END  OF HEADING PRINTOUT ;                                     BDIS0343
                                                                        BDIS0344
COMMENT       SET LIMITS ON CORRECTIONS TO HUM[M], X[M,I], AND T[J] ;   BDIS0345
              MAXDT ~ 20.0 ;                                            BDIS0346
              MAXDX ~ 1.0 ;     LXFAC ~ 1.0@-4 ;     UXFAC ~ 0.9999 ;   BDIS0347
              LHFAC ~ 0.2 ;     UHFAC ~ 0.9999 ;     MAXDH ~ 0.5 | F[0];BDIS0348
                                                                        BDIS0349
COMMENT       CALCULATE K-RATIOS AND T DERIVATIVES ;                    BDIS0350
              CALCPDC ~ TRUE ;                                          BDIS0351
S1:           FOR J ~ 1 STEP 1 UNTIL M DO                               BDIS0352
         BEGIN                                                          BDIS0353
              IF J = 2 THEN  CALCPDC ~ FALSE ;                          BDIS0354
              KNGAA (T[J], P, N, CALCPDC, TRUE, FALSE, K[J,*], PKT[J,*],BDIS0355
              C)                                                        BDIS0356
         END  OF K AND PKT CALCULATIONS ;                               BDIS0357
                                                                        BDIS0358
COMMENT       CALCULATE LIQUID MOL FRACTIONS OF ALL PLATES EXCEPT M ;   BDIS0359
              FOR J ~ M-1 STEP -1 UNTIL 1 DO                            BDIS0360
              FOR I ~ 1 STEP 1 UNTIL N DO                               BDIS0361
              IF X[J,I] ~ (K[J+1,I] | X[J+1,I]) < 0 THEN                BDIS0362
         BEGIN                                                          BDIS0363
              X[J,I] ~ 1.0@-20 ;     IR2 ~ TRUE                         BDIS0364
         END  OF X CALCULATIONS ;                                       BDIS0365
                                                                        BDIS0366
COMMENT       CALCULATE TRAY AVERAGE SPECIFIC VOLUME AND TRAY MOLAR     BDIS0367
              LIQUID HOLDUP ;                                           BDIS0368
              FOR J ~ 1 STEP 1 UNTIL M DO                               BDIS0369
         BEGIN                                                          BDIS0370
              SUMSX ~ 0 ;                                               BDIS0371
              FOR I ~ 1 STEP 1 UNTIL N DO                               BDIS0372
              SUMSX ~ X[J,I] | S[I] + SUMSX ;                           BDIS0373
              SBAR[J] ~ SUMSX ;                                         BDIS0374
              IF J ! M THEN     HUM[J] ~ HUV[J] / SBAR[J]               BDIS0375
         END  OF SBAR AND HUM CALCULATIONS ;                            BDIS0376
                                                                        BDIS0377
COMMENT       CALCULATE PHUMT ( PARTIAL DERIVATIVE OF TRAY MOLAR LIQUID BDIS0378
              HOLDUP WITH RESPECT TO EACH TRAY TEMPERATURE), A1, A3,    BDIS0379
              AND A7, WHERE THE COEFFICIENT MATRIX, A, IS STRUCTURED    BDIS0380
              AS FOLLOWS: . A1,M|M      . A2,M|(N-1)     . A3,M|1     . BDIS0381
                          ............................................. BDIS0382
                          . A4,(N-1)|M  . A5,(N-1)|(N-1) . A6,(N-1)|1 . BDIS0383
                          ............................................. BDIS0384
                          . A7,1|M      . A8,1|(N-1)     . A9,1|1     .;BDIS0385
              MPN ~ M + N ;                                             BDIS0386
              FOR J ~ 1 STEP 1 UNTIL M DO                               BDIS0387
         BEGIN                                                          BDIS0388
              A[J,MPN] ~ 0 ;                                            BDIS0389
              FOR Q ~ 1 STEP 1 UNTIL M DO                               BDIS0390
         BEGIN                                                          BDIS0391
              IF J = 1 THEN   A[MPN,Q] ~ 0 ;                            BDIS0392
              IF Q > J THEN                                             BDIS0393
         BEGIN                                                          BDIS0394
              IF Q ! 1 AND J ! M THEN                                   BDIS0395
         BEGIN                                                          BDIS0396
              SUMX ~ 0 ;                                                BDIS0397
              FOR I ~ 1 STEP 1 UNTIL N DO    IF I ! KEY THEN            BDIS0398
              SUMX ~ (X[J,I] | PKT[Q,I] | S[I] / (X[M,I] | K[Q,I]) -    BDIS0399
              X[J,KEY] | PKT[Q,KEY] | S[KEY] / (X[M,KEY] | K[Q,KEY])) | BDIS0400
              X[M,I] + SUMX ;                                           BDIS0401
              PHUMT[J,Q] ~ - (X[J,KEY] | PKT[Q,KEY] | S[KEY] / (X[M,KEY]BDIS0402
              | K[Q,KEY]) + SUMX) | HUM[J] / SBAR[J] ;                  BDIS0403
              A[MPN,Q] ~ A[MPN,Q] - PHUMT[J,Q]                          BDIS0404
         END  OF PHUMT AND A7 CALCULATIONS FOR Q GREATER THEN J ;       BDIS0405
              GO TO S2                                                  BDIS0406
         END  OF A1 CALCULATIONS FOR Q GREATER THAN J                   BDIS0407
              ELSE IF Q = J THEN                                        BDIS0408
         BEGIN                                                          BDIS0409
              PHUMT[J,Q] ~ 0 ;                                          BDIS0410
              GO TO S2                                                  BDIS0411
         END  OF PHUMT AND A1 CALCULATIONS FOR Q EQUAL TO J             BDIS0412
              ELSE                                                      BDIS0413
         BEGIN                                                          BDIS0414
              PHUMT[J,Q] ~ 0 ;     A[J,Q] ~ 0                           BDIS0415
         END  OF PHUMT AND A1 CALCULATIONS FOR Q LESS THAN J ;          BDIS0416
              GO TO S3 ;                                                BDIS0417
S2:           SUMX ~ 0 ;                                                BDIS0418
              FOR I ~ 1 STEP 1 UNTIL N DO     IF I ! KEY THEN           BDIS0419
              SUMX ~ X[J,I] | K[J,I] | PKT[Q,I] / K[Q,I] - X[J,KEY] |   BDIS0420
              K[J,KEY] | X[M,I] | PKT[Q,KEY] / (X[M,KEY] | K[Q,KEY]) +  BDIS0421
              SUMX ;                                                    BDIS0422
              A[J,Q] ~ X[J,KEY] | K[J,KEY] | PKT[Q,KEY] / (X[M,KEY] |   BDIS0423
              K[Q,KEY]) + SUMX ;                                        BDIS0424
S3:      END  OF A3 CALCULATIONS                                        BDIS0425
         END  OF PHUMT A1 A3 AND A7 CALCULATIONS ;                      BDIS0426
                                                                        BDIS0427
COMMENT       CALCULATE A2, A4, A6, A8, AND PHUMX (PARTIAL DERIVATIVE   BDIS0428
              OF TRAY MOLAR LIQUID HOLDUP WITH RESPECT TO REBOILER MOL  BDIS0429
              FRACTION) ;                                               BDIS0430
              FOR I ~ 1 STEP 1 UNTIL N DO   IF I ! KEY THEN             BDIS0431
         BEGIN                                                          BDIS0432
              IF I < KEY THEN   MPI ~ M + I   ELSE   MPI ~ M + I - 1 ;  BDIS0433
              A[MPN,MPI] ~ 0 ;                                          BDIS0434
              A[MPI,MPN] ~ - X[M,I] ;                                   BDIS0435
              FOR Q ~ M STEP -1 UNTIL 1 DO                              BDIS0436
         BEGIN                                                          BDIS0437
              A[Q,MPI] ~ K[Q,I] | X[Q,I] / X[M,I] - K[Q,KEY] | X[Q,KEY] BDIS0438
              / X[M,KEY];                                               BDIS0439
              IF Q ! 1 THEN                                             BDIS0440
         BEGIN                                                          BDIS0441
              SUMX ~ 0 ;                                                BDIS0442
              FOR J ~ Q - 1 STEP -1 UNTIL 1 DO                          BDIS0443
              SUMX ~ HUM[J] | X[J,I] | PKT[Q,I] / K[Q,I] + X[J,I] |     BDIS0444
              PHUMT[J,Q] + SUMX ;                                       BDIS0445
              A[MPI,Q] ~ - SUMX                                         BDIS0446
         END  OF A4 CALCULATIONS                                        BDIS0447
              ELSE   A[MPI,Q] ~ 0 ;                                     BDIS0448
              PHUMX[Q,I] ~ (S[KEY] | X[Q,KEY] / X[M,KEY] - S[I] | X[Q,I]BDIS0449
              / X[M,I]) | HUM[Q] / SBAR[Q] ;                            BDIS0450
              A[MPN,MPI] ~ A[MPN,MPI] - PHUMX[Q,I]                      BDIS0451
         END  OF PHUMX A2 AND A8 CALCULATIONS                           BDIS0452
         END  OF PHUMX A2 A4 A6 AND A8 CALCULATIONS ;                   BDIS0453
                                                                        BDIS0454
COMMENT       CALCULATE A5 ;                                            BDIS0455
              FOR I ~ 1 STEP 1 UNTIL N DO   IF I ! KEY THEN             BDIS0456
         BEGIN                                                          BDIS0457
              IF I < KEY THEN   MPI ~ M + I   ELSE   MPI ~ M + I - 1 ;  BDIS0458
              FOR G ~ 1 STEP 1 UNTIL N DO   IF G ! KEY THEN             BDIS0459
         BEGIN                                                          BDIS0460
              SUMX ~ 0 ;                                                BDIS0461
              FOR J ~ M STEP -1 UNTIL 1 DO                              BDIS0462
         BEGIN                                                          BDIS0463
              IF I ! G THEN   PXM ~ 0   ELSE   PXM ~ X[J,I] / X[M,I] ;  BDIS0464
              SUMX ~ X[J,I] | PHUMX[J,G] + HUM[J] | PXM + SUMX          BDIS0465
         END ;                                                          BDIS0466
              IF G < KEY THEN   MPG ~ M + G   ELSE   MPG ~ M + G - 1 ;  BDIS0467
              A[MPI,MPG] ~ - SUMX                                       BDIS0468
         END                                                            BDIS0469
         END  OF A5 CALCULATIONS ;                                      BDIS0470
                                                                        BDIS0471
COMMENT       CALCULATE A9 ;                                            BDIS0472
              A[MPN,MPN] ~ -1.0 ;                                       BDIS0473
                                                                        BDIS0474
COMMENT       CALCULATE THE CONSTANT VECTOR ;                           BDIS0475
              C[MPN] ~ 0 ;                                              BDIS0476
              FOR L ~ 1 STEP 1 UNTIL MPN DO                             BDIS0477
              IF L { M THEN                                             BDIS0478
         BEGIN                                                          BDIS0479
              C[MPN] ~ HUM[L] + C[MPN] ;      SUMX ~ 0 ;                BDIS0480
              FOR I ~ 1 STEP 1 UNTIL N DO                               BDIS0481
              SUMX ~ K[L,I] | X[L,I] + SUMX ;                           BDIS0482
              C[L] ~ 1.0 - SUMX                                         BDIS0483
         END                                                            BDIS0484
              ELSE   IF Q ~ L - M ! KEY THEN                            BDIS0485
         BEGIN                                                          BDIS0486
              SUMX ~ 0 ;     FOR J ~ 1 STEP 1 UNTIL M DO                BDIS0487
              SUMX ~ X[J,Q] | HUM[J] + SUMX ;                           BDIS0488
              G ~ IF Q < KEY THEN   L   ELSE   L - 1 ;                  BDIS0489
              C[G] ~ SUMX - F[Q]                                        BDIS0490
         END ;                                                          BDIS0491
              C[MPN] ~ C[MPN] - F[0] ;                                  BDIS0492
                                                                        BDIS0493
COMMENT       PRINT OUT INTERMEDIATE RESULTS IF POIR = 1 ;              BDIS0494
              IF POIR = 1 THEN                                          BDIS0495
         BEGIN                                                          BDIS0496
              IF CORR ! 0 THEN                                          BDIS0497
              WRITE (FOUT1, FIR1, CORR) ;                               BDIS0498
              WRITE (FOUT1, FIR9, HUM[M], FOR I ~ 1 STEP 1 UNTIL N DO   BDIS0499
              X[M,I] ) ;                                                BDIS0500
              WRITE (FOUT1, FIR10, FOR J ~ 1 STEP 1 UNTIL M DO T[J] ) ; BDIS0501
              WRITE (FOUT1, FIR11, C[M+N], FOR I ~ 1 STEP 1 UNTIL N-1 DOBDIS0502
              C[M+I] ) ;                                                BDIS0503
              WRITE (FOUT1, FIR12, FOR J ~ 1 STEP 1 UNTIL M DO C[J] ) ; BDIS0504
              IF IR2   THEN  WRITE (FOUT1, FIR2) ;                      BDIS0505
              IF IR3   THEN  WRITE (FOUT1, FIR3) ;                      BDIS0506
              IF IR4   THEN  WRITE (FOUT1, FIR4) ;                      BDIS0507
              IF IR5   THEN  WRITE (FOUT1, FIR5) ;                      BDIS0508
              IF IR6   THEN  WRITE (FOUT1, FIR6) ;                      BDIS0509
              IF IR7   THEN  WRITE (FOUT1, FIR7) ;                      BDIS0510
              IF IR14  THEN  WRITE (FOUT1, FIR14) ;                     BDIS0511
              IF IR15  THEN  WRITE (FOUT1, FIR15) ;                     BDIS0512
              IF IR16  THEN  WRITE (FOUT1, FIR16) ;                     BDIS0513
              IR2 ~ IR3 ~ IR4 ~ IR5 ~ IR6 ~ IR7 ~ IR13 ~ IR14 ~ IR15 ~  BDIS0514
              IR16 ~ FALSE                                              BDIS0515
         END  OF INTERMEDIATE PRINT OUT ;                               BDIS0516
                                                                        BDIS0517
COMMENT       EXIT IF TOLERANCES ARE MET ;                              BDIS0518
              IF NOT RECYCLE THEN   GO TO S5 ;                          BDIS0519
                                                                        BDIS0520
COMMENT       EXIT TO ERR2 IF MAXIT HAS BEEN EXCEEDED ;                 BDIS0521
              IF ITER ~ ITER + 1 > MAXIT + 1 THEN   GO TO ERR2 ;        BDIS0522
                                                                        BDIS0523
COMMENT       ENTER THE SOLVE PROCEDURE TO CALCULATE THE CORRECTIONS TO BDIS0524
              T[], X[M,], AND HUM[M] ;                                  BDIS0525
              EPS ~ (M + N) / 2.0 | 1.0 @ -10 ;                         BDIS0526
              SOLVE (MPN, A, C, FALSE, 0, 0, EPS, B, ERR1, S4) ;        BDIS0527
                                                                        BDIS0528
COMMENT       ADD CORRECTIONS TO PREVIOUS VALUES AND TEST IF TOLERANCES BDIS0529
              ARE MET ;                                                 BDIS0530
S4:           CORR ~ 1.0 ;     RECYCLE ~ FALSE ;                        BDIS0531
              FOR L ~ 1 STEP 1 UNTIL MPN DO                             BDIS0532
              IF L { M THEN                                             BDIS0533
         BEGIN                                                          BDIS0534
              IF TOL[1] < ABS(B[L]) THEN   RECYCLE ~ TRUE ;             BDIS0535
              IF ABS(B[L]) > MAXDT  THEN                                BDIS0536
         BEGIN                                                          BDIS0537
              IF TCORR ~ ABS(MAXDT / B[L]) < CORR THEN   CORR ~ TCORR ; BDIS0538
              IR6 ~ TRUE                                                BDIS0539
         END  OF MAXDT TEST ;                                           BDIS0540
              IF T[L] + B[L] < LIMIT[1] THEN                            BDIS0541
         BEGIN                                                          BDIS0542
              IF TCORR ~ ABS((T[L] - LIMIT[1]) / B[L]) < CORR THEN      BDIS0543
              CORR ~ TCORR ;     IR4 ~ TRUE                             BDIS0544
         END  OF TEST ON LOWER LIMIT OF T                               BDIS0545
              ELSE IF T[L] + B[L] > LIMIT[2] THEN                       BDIS0546
         BEGIN                                                          BDIS0547
              IF TCORR ~ ABS((LIMIT[2] - T[L]) / B[L]) < CORR THEN      BDIS0548
              CORR ~ TCORR ;     IR5 ~ TRUE                             BDIS0549
         END  OF TEST ON UPPER LIMIT OF T                               BDIS0550
         END  OF TOLERANCE TEST ON T                                    BDIS0551
              ELSE   IF Q ~ L - M ! KEY THEN                            BDIS0552
         BEGIN                                                          BDIS0553
              G ~ IF Q < KEY THEN   L   ELSE   L - 1 ;                  BDIS0554
              IF ABS(B[G]) > MAXDX THEN                                 BDIS0555
         BEGIN                                                          BDIS0556
              IF TCORR ~ ABS(MAXDX / B[G]) < CORR THEN   CORR ~ TCORR ; BDIS0557
              IR15 ~ TRUE                                               BDIS0558
         END  OF MAXDX TEST;                                            BDIS0559
              IF X[M,Q] + B[G] { 0 THEN                                 BDIS0560
         BEGIN                                                          BDIS0561
              IF TCORR ~ ABS(X[M,Q] | (1.0 - LXFAC) / B[G]) < CORR THEN BDIS0562
              CORR ~ TCORR ;     IR2 ~ TRUE                             BDIS0563
         END  OF TEST FOR NEGATIVE X VALUES ;                           BDIS0564
              IF X[M,Q] + B[G] } 1.0 THEN                               BDIS0565
         BEGIN                                                          BDIS0566
              IF TCORR ~ ABS((1.0 - X[M,Q]) | UXFAC / B[G]) < CORR THEN BDIS0567
              CORR ~ TCORR ;     IR14 ~ TRUE                            BDIS0568
         END  OF TEST FOR X GREATER THAN OR EQUAL TO 1 ;                BDIS0569
              IF TOL[2] < ABS(B[G] / X[M,Q] | 100.0) THEN RECYCLE ~ TRUEBDIS0570
         END  OF TOLERANCE TEST ON X ;                                  BDIS0571
              IF ABS(B[MPN]) > MAXDH THEN                               BDIS0572
         BEGIN                                                          BDIS0573
              IF TCORR ~ ABS(MAXDH / B[MPN]) < CORR THEN                BDIS0574
              CORR ~ TCORR ;     IR16 ~ TRUE                            BDIS0575
         END  OF MAXDH TEST ;                                           BDIS0576
              IF HUM[M] + B[MPN] { 0 THEN                               BDIS0577
         BEGIN                                                          BDIS0578
              IF TCORR ~ ABS(HUM[M] | (1.0 - LHFAC) / B[MPN]) < CORR    BDIS0579
              THEN   CORR ~ TCORR ;     IR7 ~ TRUE                      BDIS0580
         END  OF TEST FOR NEGATIVE HUM ;                                BDIS0581
              IF TOL[3] < ABS (B[MPN] / HUM[M] | 100.0) THEN            BDIS0582
              RECYCLE ~ TRUE ;                                          BDIS0583
              FOR L ~ 1 STEP 1 UNTIL MPN DO                             BDIS0584
              IF L { M THEN   T[L] ~ T[L] + B[L] | CORR                 BDIS0585
              ELSE   IF Q ~ (L-M) ! KEY THEN                            BDIS0586
         BEGIN                                                          BDIS0587
              G ~ IF Q < KEY THEN L ELSE L-1 ;                          BDIS0588
              X[M,Q] ~ X[M,Q] + B[G] | CORR                             BDIS0589
         END ;                                                          BDIS0590
              HUM[M] ~ HUM[M] + B[MPN] | CORR ;                         BDIS0591
                                                                        BDIS0592
COMMENT       CALCULATE X[M,KEY] ;                                      BDIS0593
              SUMX ~ 0 ;                                                BDIS0594
              FOR I ~ 1 STEP 1 UNTIL N DO     IF I ! KEY THEN           BDIS0595
              SUMX ~ X[M,I] + SUMX ;                                    BDIS0596
              IF SUMX > 1.0 THEN                                        BDIS0597
         BEGIN                                                          BDIS0598
              X[M,KEY] ~ 1.0@-20 ;     IR3 ~ TRUE ;                     BDIS0599
              FOR I ~ 1 STEP 1 UNTIL N DO     IF I ! KEY THEN           BDIS0600
              X[M,I] ~ X[M,I] / SUMX                                    BDIS0601
         END                                                            BDIS0602
              ELSE   X[M,KEY] ~ 1.0 - SUMX ;                            BDIS0603
              GO TO S1 ;                                                BDIS0604
                                                                        BDIS0605
S5:      END  OF STARTUP PROCEDURE ;                                    BDIS0606
PROCEDURE     TRANSIENT (M, N, KEY, P, TC, PC, S, LIMIT, STOP, QM,      BDIS0607
              POIR1, POIR2, R, HUM, T, X, Y, DELL, DELV, DELQ1, DELQM,  BDIS0608
              DELD, D, SUMD, SUMR, SBAR, ERR1) ;                        BDIS0609
                                                                        BDIS0610
COMMENT       THIS PROCEDURE CALCULATES THE TRANSIENT CONDITIONS OF A   BDIS0611
              BATCH DISTILLATION COLUMN OPERATING AT CONSTANT REFLUX OR BDIS0612
              AT VARIABLE REFLUX IN WHICH THE DISTILLATE COMPOSITION OF BDIS0613
              A SPECIFIED COMPONENT IS MAINTAINED AT A CONSTANT VALUE.  BDIS0614
                                                                        BDIS0615
              THE INPUT PARAMETERS ARE                                  BDIS0616
              M    - TOTAL NUMBER OF TRAYS  - INTEGER                   BDIS0617
              N    - TOTAL NUMBER OF COMPONENTS  - INTEGER              BDIS0618
              KEY  - KEY COMPONENT NUMBER  - INTEGER                    BDIS0619
              P    - COLUMN PRESSURE, PSIA  - REAL                      BDIS0620
              TC   - COMPONENT CRITICAL TEMPERATURES, DEG. R.  - REAL   BDIS0621
                   ARRAY[0:N]                                           BDIS0622
              PC   - COMPONENT CRITICAL PRESSURES, PSIA  - REAL         BDIS0623
                   ARRAY [0:N]                                          BDIS0624
              S    - COMPONENT LIQUID MOLAR VOLUMES, CU. FT./MOL  -     BDIS0625
                   REAL ARRAY[0:N]                                      BDIS0626
              LIMIT - TEMPERATURE LIMITS  - REAL ARRAY [0:2]            BDIS0627
                   LIMIT[1] = MINIMUM T, F.                             BDIS0628
                   LIMIT[2] = MAXIMUM T, F.                             BDIS0629
              STOP - STOP = THE MOLS OF DISTILLATE TO BE WITHDRAWN,     BDIS0630
                   UNLESS THE COLUMN IS TO OPERATE AT TOTAL REFLUX.     BDIS0631
                   THEN, STOP = THE MOLS OF REFLUX TO BE RETURNED TO    BDIS0632
                   THE COLUMN.  - REAL                                  BDIS0633
              QM   - IF THE REBOILER HEAT DUTY IS HELD CONSTANT.        BDIS0634
                   THROUGHOUT THE DISTILLATION, IT MAY BE SPECIFIED     BDIS0635
                   HERE AS BTU/HOUR TO ALLOW CALCULATION OF THE         BDIS0636
                   CONDENSER DUTY AND THE LIQUID AND VAPOR RATES.  SET  BDIS0637
                   TO ZERO OTHERWISE.                                   BDIS0638
              POIR1 - INCREMENTAL VALUE OF D OR L1 (TOTAL REFLUX) USED  BDIS0639
                   TO INDICATE THE POINT(S) OF INTERMEDIATE PRINTOUT,   BDIS0640
                   MOLS - REAL.  TO OMIT INTERMEDIATE PRINTOUT, SET     BDIS0641
                   POIR1 > TOTAL MOLS WITHDRAWN OR REFLUXED (TOTAL      BDIS0642
                   REFLUX).                                             BDIS0643
              POIR2 - INTEGER.  IF = 1, PRINT X[J,I] WITH THE           BDIS0644
                   INTERMEDIATE RESULTS.                                BDIS0645
              R    - REFLUX RATIO, L1/D.  SET R > 1.0@10 FOR TOTAL      BDIS0646
                   REFLUX OPERATION  - REAL                             BDIS0647
              HUM  - INITIAL LIQUID HOLDUP ON TRAYS, MOLS  - REAL       BDIS0648
                   ARRAY [0:M]                                          BDIS0649
              T    - INITIAL TRAY TEMPERATURES, F.  - REAL ARRAY [0:M]  BDIS0650
              X    - INITIAL LIQUID TRAY COMPOSITIONS, MOL FRACTIONS  - BDIS0651
                   REAL ARRAY [0:M,0:N]                                 BDIS0652
                                                                        BDIS0653
              THE OUTPUT PARAMETERS ARE                                 BDIS0654
              HUM  - FINAL LIQUID HOLDUP ON TRAYS,MOLS - REAL ARRAY[0:M]BDIS0655
              T    - FINAL TRAY TEMPERATURES, DEG. F.  - REAL ARRAY[0:M]BDIS0656
              X    - FINAL LIQUID TRAY COMPOSITIONS, MOL FRACTIONS  -   BDIS0657
                   REAL ARRAY [0:M,0:N]                                 BDIS0658
              Y    - FINAL VAPOR TRAY COMPOSITIONS, MOL FRACTIONS  -    BDIS0659
                   REAL ARRAY [0:M,0:N]                                 BDIS0660
              DELL - FINAL INCREMENTAL LIQUID TRAY FLOWS, MOLS  - REAL  BDIS0661
                   ARRAY [0:M]                                          BDIS0662
              DELV - FINAL INCREMENTAL VAPOR TRAY FLOWS, MOLS  - REAL   BDIS0663
                   ARRAY [0:M]                                          BDIS0664
              DELQ1 - FINAL INCREMENTAL CONDENSER DUTY, BTU  - REAL     BDIS0665
              DELQM - FINAL INCREMENTAL REBOILER DUTY, BTU  - REAL      BDIS0666
              DELD - FINAL INCREMENTAL DISTILLATE FLOW, MOLS  -REAL     BDIS0667
              D    - COMPONENT TOTAL MOLS WITHDRAWN AS DISTILLATE  -    BDIS0668
                   REAL ARRAY [0:N]                                     BDIS0669
              SUMD - TOTAL MOLS OF DISTILLATE WITHDRAWN  - REAL         BDIS0670
              SUMR - TOTAL MOLS OF REFLUX RETURNED TO COLUMN - REAL     BDIS0671
              SBAR - AVERAGE LIQUID MOLAR VOLUME ON TRAY, CU. FT./MOL - BDIS0672
                   REAL ARRAY [0:M]                                     BDIS0673
                                                                        BDIS0674
              THE REFERENCE PARAMETER IS                                BDIS0675
              ERR1 - REFERENCE STATEMENT LABEL TO WHICH CONTROL IS      BDIS0676
                   CHANGED IF THE TEMPERATURE LIMITS ARE EXCEEDED.      BDIS0677
              IN ADDITION, THE REAL ARRAYS HV, HL, PHLT, SBAR[0:M],     BDIS0679
              K, PKT, PHLX[0:M,0:N], TR1S1, TR2S2[0: IF (M>N-1 AND M>14)BDIS0680
              THEN M ELSE IF 14>N-1 THEN 14 ELSE N-1], AND THE GLOBAL   BDIS0681
              ARRAYS OF KNGAA, HLIQ, HVPCOR, AND PHIPART ARE GLOBAL TO  BDIS0682
              THE TRANSIENT PROCEDURE.                                  BDIS0683
                                                                        BDIS0684
              ALSO, THE OUTPUT FILE, FOUT1, MUST BE DECLARED GLOBAL TO  BDIS0685
              TRANSIENT.  IT IS USED TO PRINT THE RESULTS.              BDIS0686
                                                                        BDIS0687
              THE PROCEDURES PHIPART, HVPCOR, HLIQ, AND KNGAA           BDIS0688
              MUST BE DECLARED PRIOR TO TRANSIENT.  PHIPART MUST BE     BDIS0689
              DECLARED PRIOR TO HVPCOR ;                                BDIS0690
                                                                        BDIS0691
VALUE         M, N, KEY, P, TC, PC, S, LIMIT, STOP, QM, POIR1, POIR2, R;BDIS0692
REAL          P, STOP, QM, R, DELQ1, DELQM, DELD, SUMD, SUMR, POIR1 ;   BDIS0693
INTEGER       M, N, KEY ;                                               BDIS0694
INTEGER       POIR2 ;                                                   BDIS0695
REAL ARRAY    TC, PC, S, LIMIT, HUM, T, D, DELL, SBAR, DELV[0], X,      BDIS0696
              Y[0,0] ;                                                  BDIS0697
LABEL         ERR1 ;                                                    BDIS0698
                                                                        BDIS0699
         BEGIN                                                          BDIS0700
REAL          DELT, DELHUM, TRS1, SUM, SUM1, SUM2, SUM3, SKEY, KKEY, DL,BDIS0701
              DV, MHUMJ, ANI, XJI, KJI, HLJ, PRNT, MAXXIJ ;             BDIS0702
INTEGER       I, J, L, NM1, JP1, JM1, MM1, NP1 ;                        BDIS0703
BOOLEAN       CALCPDC ;                                                 BDIS0704
LABEL         TR1, TR2 ;                                                BDIS0705
FORMAT OUT    FIR1 (/ "PRINT-OUT OF INTERMEDIATE RESULTS IN THE ",      BDIS0706
                   "TRANSIENT PROCEDURE" /),                            BDIS0707
              FIR3 ("COLUMN IS RUN AT TOTAL REFLUX" // "MOLS OF REFLUX",BDIS0708
                   " TO BE RETURNED ", F15.2 / ),                       BDIS0709
              FIR4 ("COLUMN IS RUN AT CONSTANT REFLUX" // "REFLUX ",    BDIS0710
                   "RATIO, L1/D ", F26.2 // "MOLS OF DISTILLATE TO BE ",BDIS0711
                   "WITHDRAWN ", F10.2 / ),                             BDIS0712
              FIR6 ("REBOILER HEAT DUTY, BTU/HOUR ", F16.2 /),          BDIS0713
              FIR7 ("X[J,I]:"),                                         BDIS0714
              FIR8 (8E15.5),                                            BDIS0715
              FIR9 ("HUM[J]:"),                                         BDIS0716
              FIR10 ("T[J]:"),                                          BDIS0717
              FIR11 ("DELL[J]:"),                                       BDIS0718
              FIR12 ("L[J]:"),                                          BDIS0719
              FIR13 ("DELV[J]:"),                                       BDIS0720
              FIR14 ("V[J]:"),                                          BDIS0721
              FIR15 ("DELQ1 = ", E15.5, "   DELQM = ", E15.5),          BDIS0722
              FIR16 ("Q1 = ", E15.5, "   QM = ", E15.5, X6),            BDIS0723
              FIR17 (X49, " SUMD = ", E12.5, " SUMR = ", E12.5,         BDIS0724
                   " R = ", E12.5) ;                                    BDIS0725
                                                                        BDIS0726
COMMENT       PRINT OUT PROBLEM SPECIFICATIONS IF POIR1 ! 0 ;           BDIS0727
              IF POIR1 ! 0 THEN                                         BDIS0728
         BEGIN                                                          BDIS0729
              WRITE (FOUT1 [PAGE]) ;                                    BDIS0730
              WRITE (FOUT1, FIR1) ;                                     BDIS0731
              IF R > 1.0@10 THEN WRITE (FOUT1, FIR3, STOP)              BDIS0732
              ELSE WRITE (FOUT1, FIR4, R, STOP) ;                       BDIS0733
              IF QM ! 0 THEN   WRITE (FOUT1, FIR6, QM)                  BDIS0734
         END  OF PROBLEM SPECIFICATIONS PRINT OUT ;                     BDIS0735
                                                                        BDIS0736
COMMENT       CALCULATE K-RATIOS, ENTHALPIES, AVERAGE TRAY SPECIFIC     BDIS0737
              VOLUMES, AND VAPOR MOL FRACTIONS ;                        BDIS0738
              SUMD ~ SUMR ~ PRNT ~ 0 ;                                  BDIS0739
              FOR I ~ 1 STEP 1 UNTIL N DO  D[I] ~ 0 ;                   BDIS0740
              MAXXIJ ~ 0.01 ;     DELD ~ DELL[1] ~ 0.1 ;                BDIS0741
              CALCPDC ~ TRUE ;     NM1 ~ N - 1 ;                        BDIS0742
              NP1 ~ N + 1 ;     MM1 ~ M - 1 ;     SKEY ~ S[KEY] ;       BDIS0743
TR1:          FOR J ~ 1 STEP 1 UNTIL M DO                               BDIS0744
         BEGIN                                                          BDIS0745
              IF J = 2 THEN   CALCPDC ~ FALSE ;                         BDIS0746
              KNGAA (T[J], P, N, CALCPDC, TRUE, FALSE, K[J,*], PKT[J,*],BDIS0747
              C) ;                                                      BDIS0748
              HLIQ (N, T[J], X[J,*], HL[J], PHLX[J,*], PHLT[J]) ;       BDIS0749
              FOR I ~ 1 STEP 1 UNTIL N DO     IF I ! KEY THEN           BDIS0750
              PHLX[J,I] ~ PHLX[J,I] - PHLX[J,KEY] ;                     BDIS0751
              IF J ! M THEN                                             BDIS0752
         BEGIN                                                          BDIS0753
              SUM ~ 0 ;     FOR I ~ 1 STEP 1 UNTIL N DO                 BDIS0754
              SUM ~ X[J,I] | S[I] + SUM ;                               BDIS0755
              SBAR[J] ~ SUM                                             BDIS0756
         END  OF SBAR CALCULATION ;                                     BDIS0757
              IF J ! 1 THEN                                             BDIS0758
         BEGIN                                                          BDIS0759
              SUM ~ 0 ;     FOR I ~ 1 STEP 1 UNTIL N DO                 BDIS0760
              SUM ~ SUM + Y[J,I] ~ (K[J,I] | X[J,I]) ;                  BDIS0761
              CALCDHV[1] ~ CALCDHV[2] ~ CALCDHV[3] ~ FALSE ;            BDIS0762
              HVPCOR (N, T[J], P, Y[J,*], CALCDHV, HV[J], TRS1, TRS1,   BDIS0763
              TR1S1)                                                    BDIS0764
         END                                                            BDIS0765
         END  OF DATA CALCULATIONS ;                                    BDIS0766
                                                                        BDIS0767
COMMENT       CONDENSER CALCULATIONS ;                                  BDIS0768
              TRS1 ~ HUM[2] / (900.0 | (1.0 + R) | MAXXIJ) ;            BDIS0769
              IF R { 1.0@10 THEN                                        BDIS0770
         BEGIN                                                          BDIS0771
              IF TRS1 > 1.1 | DELD THEN   DELD ~ 1.1 | DELD             BDIS0772
              ELSE   DELD ~ TRS1 ;                                      BDIS0773
              SUMD ~ SUMD + DELD ;     DELL[1] ~ R | DELD ;             BDIS0774
              FOR I ~ 1 STEP 1 UNTIL N DO   D[I] ~ D[I] + X[1,I] | DELD BDIS0775
         END                                                            BDIS0776
              ELSE                                                      BDIS0777
         BEGIN                                                          BDIS0778
              IF TRS1 > 1.1 | DELL[1] THEN   DELL [1] ~ 1.1 | DELL[1]   BDIS0779
              ELSE   DELL[1] ~ TRS1 ;     DELD ~ 0                      BDIS0780
         END ;                                                          BDIS0781
              MAXXIJ ~ 0 ;                                              BDIS0782
              SUM ~ 0 ;     SUMR ~ DELL[1] + SUMR ;                     BDIS0783
              FOR I ~ 1 STEP 1 UNTIL N DO     IF I ! KEY THEN           BDIS0784
              SUM ~ (Y[2,I] - X[1,I]) | (S[I] - SKEY) + SUM ;           BDIS0785
              DELHUM ~ - (DELD + DELL[1]) | SUM / (SBAR[1] + SUM) ;     BDIS0786
              DELV[2] ~ DELD + DELL[1] + DELHUM ;                       BDIS0787
              SUM ~ SUM1 ~ SUM2 ~ SUM3 ~ 0 ;                            BDIS0788
              FOR I ~ 1 STEP 1 UNTIL N DO                               BDIS0789
         BEGIN                                                          BDIS0790
              IF I ! KEY THEN                                           BDIS0791
         BEGIN                                                          BDIS0792
              SUM ~ SUM + TR1S1[I] ~ ((Y[2,I] - X[1,I]) | DELV[2] /     BDIS0793
              HUM[1]) ;                                                 BDIS0794
              SUM1 ~ TR1S1[I] | (K[1,KEY] - K[1,I]) + SUM1 ;            BDIS0795
              SUM3 ~ PHLX[1,I] | TR1S1[I] + SUM3                        BDIS0796
         END ;                                                          BDIS0797
              SUM2 ~ PKT[1,I] | X[1,I] + SUM2                           BDIS0798
         END ;                                                          BDIS0799
              TR1S1[KEY] ~ - SUM ;     DELT ~ SUM1 / SUM2 ;             BDIS0800
              DELQ1 ~ DELV[2] | (HV[2] - HL[1]) - HUM[1] | PHLT[1] |    BDIS0801
              DELT - HUM[1] | SUM3 ;                                    BDIS0802
              HUM[1] ~ HUM[1] + DELHUM ;     T[1] ~ T[1] + DELT ;       BDIS0803
              FOR I ~ 1 STEP 1 UNTIL N DO   X[1,I] ~ X[1,I] + TR1S1[I] ;BDIS0804
                                                                        BDIS0805
COMMENT       CALCULATIONS FOR TRAYS 2, 3, - - -, M-1 ;                 BDIS0806
              FOR J ~ 2 STEP 1 UNTIL MM1 DO                             BDIS0807
         BEGIN                                                          BDIS0808
              KKEY ~ K[J,KEY] ;     DV ~ DELV[J] ;     MHUMJ ~ - HUM[J];BDIS0809
              JP1 ~ J + 1 ;     JM1 ~ J - 1 ;     DL ~ DELL[JM1] ;      BDIS0810
              SUM3 ~ 0 ;     FOR I ~ 1 STEP 1 UNTIL N DO                BDIS0811
              SUM3 ~ X[J,I] | PKT[J,I] + SUM3 ;                         BDIS0812
              TRS1 ~ PHLT[J] / SUM3 ;                                   BDIS0813
              SUM ~ SUM1 ~ 0 ;     HLJ ~ HL[J] ;                        BDIS0814
              FOR I ~ 1 STEP 1 UNTIL N DO    IF I ! KEY THEN            BDIS0815
         BEGIN                                                          BDIS0816
              XJI ~ X[J,I] ;     KJI ~ K[J,I] ;                         BDIS0817
              ANI ~ (PHLX[J,I] + (KKEY - KJI) | TRS1) | MHUMJ ;         BDIS0818
              SUM ~ (TR1S1[I] ~ ((XJI - X[JM1,I] + TR1S1[I]) | DL +     BDIS0819
              (KJI - 1.0) | DV | XJI)) | ANI + SUM ;                    BDIS0820
              SUM1 ~ (TR1S2[I] ~ (Y[JP1,I] - XJI)) | ANI + SUM1         BDIS0821
         END ;                                                          BDIS0822
              DELV[JP1] ~ ((HV[J] - HLJ) | DV + (HLJ - HL[JM1]) | DL -  BDIS0823
              SUM / MHUMJ) / ((HV[JP1] - HLJ) - SUM1 / MHUMJ) ;         BDIS0824
              SUM ~ 0 ;                                                 BDIS0825
              FOR I ~ 1 STEP 1 UNTIL N DO     IF I ! KEY THEN           BDIS0826
         BEGIN                                                          BDIS0827
              SUM ~ SUM + TR1S1[I] ~ ((TR1S1[I] - TR1S2[I] | DELV[JP1]) BDIS0828
              / MHUMJ) ;                                                BDIS0829
              X[J,I] ~ X[J,I] + TR1S1[I]                                BDIS0830
         END ;                                                          BDIS0831
              X[J,KEY] ~ X[J,KEY] + TR1S1[KEY] ~ (-SUM) ;               BDIS0832
              SUM ~ SUM1 ~ 0 ;                                          BDIS0833
              FOR I ~ 1 STEP 1 UNTIL N DO                               BDIS0834
         BEGIN                                                          BDIS0835
              IF TR1S1[I] > MAXXIJ  THEN   MAXXIJ ~ TR1S1[I] ;          BDIS0836
              IF I ! KEY THEN                                           BDIS0837
         BEGIN                                                          BDIS0838
              SUM ~ TR1S1[I] | (S[I] - SKEY) + SUM ;                    BDIS0839
              SUM1 ~ TR1S1[I] | (KKEY - K[J,I]) + SUM1                  BDIS0840
         END                                                            BDIS0841
         END ;                                                          BDIS0842
              DELL[J] ~ HUM[J] | SUM / SBAR[J] + DELV[JP1] + DL - DV ;  BDIS0843
              HUM[J] ~ HUM[J] + DELV[JP1] - DELL[J] - DV + DL ;         BDIS0844
              T[J] ~ T[J] + SUM1 / SUM3                                 BDIS0845
         END  OF TRAY CALCULATIONS ;                                    BDIS0846
                                                                        BDIS0847
COMMENT       REBOILER CALCULATIONS ;                                   BDIS0848
              DELHUM ~ DELL[MM1] - DELV[M] ;                            BDIS0849
              SUM ~ SUM3 ~ 0 ;                                          BDIS0850
              FOR I ~ 1 STEP 1 UNTIL N DO                               BDIS0851
         BEGIN                                                          BDIS0852
              SUM3 ~ X[M,I] | PKT[M,I] + SUM3 ;                         BDIS0853
              IF I ! KEY THEN                                           BDIS0854
         BEGIN                                                          BDIS0855
              SUM ~ SUM + TR1S1[I] ~ (((X[MM1,I] - TR1S1[I] - X[M,I]) | BDIS0856
              DELL[MM1] + (1.0 - K[M,I]) | X[M,I] | DELV[M]) / HUM[M]) ;BDIS0857
              X[M,I] ~ X[M,I] + TR1S1[I]                                BDIS0858
         END                                                            BDIS0859
         END ;                                                          BDIS0860
              X[M,KEY] ~ X[M,KEY] - SUM ;                               BDIS0861
              SUM ~ SUM1 ~ 0 ;                                          BDIS0862
              FOR I ~ 1 STEP 1 UNTIL N DO     IF I ! KEY THEN           BDIS0863
         BEGIN                                                          BDIS0864
              SUM ~ (K[M,KEY] - K[M,I]) | TR1S1[I] + SUM ;              BDIS0865
              SUM1 ~ PHLX[M,I] | TR1S1[I] + SUM1                        BDIS0866
         END ;                                                          BDIS0867
              T[J] ~ T[J] + DELT ~ (SUM / SUM3) ;                       BDIS0868
              DELQM ~ (PHLT[M] | DELT + SUM1) | HUM[M] + HV[M] | DELV[M]BDIS0869
              + HL[M] | DELHUM - HL[MM1] | DELL[MM1] ;                  BDIS0870
              HUM[M] ~ HUM[M] + DELHUM ;                                BDIS0871
                                                                        BDIS0872
COMMENT       PRINT OUT INTERMEDIATE RESULTS IF POIR1 ! 0 ;             BDIS0873
              IF POIR1 ! 0 THEN                                         BDIS0874
              IF (PRNT ~ (PRNT + (IF R{1.0@10 THEN DELD ELSE DELL[1]))) BDIS0875
              } POIR1 THEN                                              BDIS0876
         BEGIN                                                          BDIS0877
              WRITE (FOUT1 [DBL]) ;                                     BDIS0878
              PRNT ~ 0 ;                                                BDIS0879
              IF POIR2 = 1 THEN                                         BDIS0880
         BEGIN                                                          BDIS0881
              WRITE (FOUT1, FIR7) ;     FOR J ~ 1 STEP 1 UNTIL M DO     BDIS0882
              WRITE (FOUT1, FIR8, FOR I ~ 1 STEP 1 UNTIL N DO X[J,I])   BDIS0883
         END ;                                                          BDIS0884
              TRS1 ~ DELQ1 | QM / DELQM ;                               BDIS0885
              IF QM ! 0 THEN     WRITE (FOUT1[NO], FIR16, TRS1, QM)     BDIS0886
              ELSE   WRITE (FOUT1[NO], FIR15, DELQ1, DELQM) ;           BDIS0887
              WRITE (FOUT1, FIR17, SUMD, SUMR, R) ;                     BDIS0888
              WRITE (FOUT1, FIR9) ;                                     BDIS0889
              WRITE (FOUT1, FIR8, FOR J ~ 1 STEP 1 UNTIL M DO  HUM[J]) ;BDIS0890
              WRITE (FOUT1, FIR10) ;                                    BDIS0891
              WRITE (FOUT1, FIR8, FOR J ~ 1 STEP 1 UNTIL M DO  T[J]) ;  BDIS0892
              FOR J ~ 1 STEP 1 UNTIL M DO                               BDIS0893
              IF QM ! 0 THEN                                            BDIS0894
         BEGIN                                                          BDIS0895
              TR1S1[J] ~ DELL[J] | QM / DELQM ;                         BDIS0896
              TR1S2[J] ~ DELV[J] | QM / DELQM                           BDIS0897
         END                                                            BDIS0898
              ELSE                                                      BDIS0899
         BEGIN                                                          BDIS0900
              TR1S1[J] ~ DELL[J] ;                                      BDIS0901
              TR1S2[J] ~ DELV[J]                                        BDIS0902
         END ;                                                          BDIS0903
              IF QM ! 0 THEN   WRITE (FOUT1, FIR12)                     BDIS0904
              ELSE   WRITE (FOUT1, FIR11) ;                             BDIS0905
              WRITE (FOUT1, FIR8, FOR J ~ 1 STEP 1 UNTIL M DO TR1S1[J]);BDIS0906
              IF QM ! 0 THEN   WRITE (FOUT1, FIR14)                     BDIS0907
              ELSE   WRITE (FOUT1, FIR13) ;                             BDIS0908
              WRITE (FOUT1, FIR8, FOR J ~ 1 STEP 1 UNTIL M DO TR1S2[J]) BDIS0909
         END  OF INTERMEDIATE PRINT OUT ;                               BDIS0910
                                                                        BDIS0911
COMMENT       TEST FOR EXIT FROM THE PROCEDURE ;                        BDIS0912
              IF R { 1.0@10 THEN   SUM ~ SUMD   ELSE   SUM ~ SUMR ;     BDIS0913
              IF SUM } STOP THEN   GO TO TR2                            BDIS0914
              ELSE   GO TO TR1 ;                                        BDIS0915
                                                                        BDIS0916
TR2:     END OF TRANSIENT PROCECURE ;                                   BDIS0917
                                                                        BDIS0918
COMMENT       READ IN COMPONENT FEED MOLS ;                             BDIS0919
              READ (FIN1, /, READF) ;                                   BDIS0920
              IF READF = 1 THEN                                         BDIS0921
              READ (FIN1, /, FOR I ~ 1 STEP 1 UNTIL N DO [F[I]] ) ;     BDIS0922
                                                                        BDIS0923
COMMENT       READ IN TRAY HOLDUP VOLUMES ;                             BDIS0924
              READ (FIN1, /, READH) ;                                   BDIS0925
              IF READH = 1 THEN                                         BDIS0926
              READ (FIN1, /, FOR J ~ 1 STEP 1 UNTIL M - 1 DO [HUV[J]]); BDIS0927
                                                                        BDIS0928
COMMENT       READ IN INITIAL ESTIMATES ;                               BDIS0929
              READ (FIN1, /, READIE) ;                                  BDIS0930
              IF READIE = 1 THEN                                        BDIS0931
              READ (FIN1, /, HUM[M], FOR I ~ 1 STEP 1 UNTIL N DO        BDIS0932
              [X[M,I]], T[1], T[M] ) ;                                  BDIS0933
                                                                        BDIS0934
COMMENT       ASSIGN VALUES TO DATA WHICH ARE NOT SPECIFIED ;           BDIS0935
              IF MAXIT = 0 THEN   MAXIT ~ 10 ;                          BDIS0936
              IF TOL[1] = 0 THEN   TOL[1] ~ 0.2 ;                       BDIS0937
              IF TOL[2] = 0 THEN   TOL[2] ~ 0.1 ;                       BDIS0938
              IF TOL[3] = 0 THEN   TOL[3] ~ 0.1 ;                       BDIS0939
              IF LIMIT[1] = 0 AND LIMIT[2] = 0 THEN                     BDIS0940
         BEGIN                                                          BDIS0941
              LIMIT[1] ~ -100.0 ;     LIMIT[2] ~ 500.0                  BDIS0942
         END ;                                                          BDIS0943
              F[0] ~ 0 ;      FOR I ~ 1 STEP 1 UNTIL N DO               BDIS0944
              F[0] ~ F[I] + F[0] ;                                      BDIS0945
              TDIFF ~ (T[M] - T[1]) / (M - 1) ;                         BDIS0946
              FOR J ~ M - 1 STEP - 1 UNTIL 1 DO   T[J] ~ T[J+1] - TDIFF;BDIS0947
                                                                        BDIS0948
COMMENT       PRINT OUT PROBLEM DATA IF POPD = 1 ;                      BDIS0949
              IF POPD = 1 THEN                                          BDIS0950
         BEGIN                                                          BDIS0951
              WRITE (FOUT1[PAGE]);                                      BDIS0952
              WRITE (FOUT1,FID1, IDNO, FOR I ~ 1 STEP 1 UNTIL 12 DO     BDIS0953
              ID[I], M, P, N, TOL[1], KEY, TOL[2], MAXIT, TOL[3],       BDIS0954
              LIMIT[1], LIMIT[2], SPEC[1], SPEC[2]) ;                   BDIS0955
              WRITE (FOUT1, FID5, HUM[M], T[1], T[M] ) ;                BDIS0956
              I ~ 1 ;                                                   BDIS0957
BD2:          IF I { N AND I { M THEN                                   BDIS0958
              WRITE (FOUT1, FID6, I, F[I], X[M,I], I, HUV[I] )          BDIS0959
              ELSE IF I { N AND I > M THEN                              BDIS0960
              WRITE (FOUT1, FID7, I, F[I], X[M,I] )                     BDIS0961
              ELSE      WRITE (FOUT1, FID8, I, HUV[I] ) ;               BDIS0962
              IF I ~ I + 1 { N OR I { M-1 THEN   GO TO BD2              BDIS0963
         END ;                                                          BDIS0964
                                                                        BDIS0965
COMMENT       READ IN COMPONENT PHYSICAL PROPERTY DATA ;                BDIS0966
              READ (FIN1, /, READCD) ;                                  BDIS0967
              IF READCD = 1 THEN                                        BDIS0968
         BEGIN                                                          BDIS0969
              READ (FIN1, FNAME, FOR I ~ 1 STEP 1 UNTIL N DO            BDIS0970
              [NAME1[I], NAME2[I], TC[I], PC[I], S[I] ] ) ;             BDIS0971
              READ (FIN1, FKH, FOR I ~ 1 STEP 1 UNTIL N DO              BDIS0972
              FOR J ~ 1,2,3,4,5,6,7 DO [KC1[I,4|J-3], KC1[I,4|J-2],     BDIS0973
              KC1[I,4|J-1], KC1[I,4|J], AK[I,J,1], AK[I,J,2],AK[I,J,3]],BDIS0974
              FOR I ~ 1 STEP 1 UNTIL N DO [FOR J~1,2,3,4 DO  HVC1[I,J], BDIS0975
              AHV[I,1], AHV[I,2], AHV[I,3] ],                           BDIS0976
              FOR I ~ 1 STEP 1 UNTIL N DO [FOR J~1,2,3,4 DO  HLC5[I,J], BDIS0977
              AHL[I,1], AHL[I,2], AHL[I,3] ])                           BDIS0978
         END ;                                                          BDIS0979
                                                                        BDIS0980
COMMENT       PRINT OUT COMPONENT DATA IF POCD = 1 ;                    BDIS0981
              IF POCD = 1 THEN                                          BDIS0982
         BEGIN                                                          BDIS0983
              WRITE (FOUT1, FID9, FOR I ~ 1 STEP 1 UNTIL N DO           BDIS0984
              [NAME1[I], NAME2[I], TC[I], PC[I], S[I],                  BDIS0985
              FOR J ~ 1,2,3,4,5,6,7 DO [KC1[I,4|J-3], KC1[I,4|J-2],     BDIS0986
              KC1[I,4|J-1], KC1[I,4|J], AK[I,J,1], AK[I,J,2],AK[I,J,3]],BDIS0987
              FOR J ~ 1,2,3,4 DO HVC1[I,J], AHV[I,1], AHV[I,2],AHV[I,3],BDIS0988
              FOR J ~ 1,2,3,4 DO HLC5[I,J], AHL[I,1],AHL[I,2],AHL[I,3]])BDIS0989
         END ;                                                          BDIS0990
                                                                        BDIS0991
COMMENT       ENTER THE STARTUP PROCEDURE TO CALCULATE THE STEADY-STATE BDIS0992
              CONDITIONS AT TOTAL REFLUX ;                              BDIS0993
              STARTUP (M, N, KEY, P, F, S, HUV, TOL, LIMIT, MAXIT, POIR,BDIS0994
              T, HUM, X, ERR1, ERR2) ;                                  BDIS0995
                                                                        BDIS0996
COMMENT       PRINT THE STEADY-STATE CONDITIONS AT TOTAL REFLUX ;       BDIS0997
              WRITE (FOUT1 [PAGE] ) ;                                   BDIS0998
              WRITE (FOUT1, FSS1, IDNO, FOR I ~ 1 STEP 1 UNTIL 12 DO    BDIS0999
              ID[I], FOR J ~ 1 STEP 1 UNTIL M DO   T[J] ) ;             BDIS1000
              WRITE (FOUT1, FSS2, FOR J ~ 1 STEP 1 UNTIL M DO HUM[J] ) ;BDIS1001
              WRITE (FOUT1, FSS3) ;     FOR J ~ 1 STEP 1 UNTIL M DO     BDIS1002
              WRITE (FOUT1, FSS4, FOR I ~ 1 STEP 1 UNTIL N DO X[J,I]);  BDIS1003
                                                                        BDIS1004
COMMENT       PRELIMINARY CALCULATIONS FOR THE TRANSIENT CONDITIONS ;   BDIS1005
              STOP ~ SPEC[1] | F[0] | 0.01 ;                            BDIS1006
              R ~ SPEC[2] ;                                             BDIS1007
              POIR1 ~ STOP / (IPOIR1 + 0.99) ;                          BDIS1008
                                                                        BDIS1009
COMMENT       ENTER THE TRANSIENT PROCEDURE TO CALCULATE THE TRANSIENT  BDIS1010
              CONDITIONS ;                                              BDIS1011
              TRANSIENT (M, N, KEY, P, TC, PC, S, LIMIT, STOP, QM,      BDIS1012
              POIR1, POIR2, R, HUM, T, X, Y, DELL, DELV, DELQ1, DELQM,  BDIS1013
              DELD, D, SUMD, SUMR, SBAR, ERR3) ;                        BDIS1014
                                                                        BDIS1015
COMMENT       PRINT THE FINAL RESULTS ;                                 BDIS1016
              WRITE (FOUT1 [PAGE] ) ;                                   BDIS1017
              DATE (MONTH, DAY, YEAR) ;                                 BDIS1018
              ELMIN ~ (ELSEC ~ (TIME(1) - STIME1)) DIV 3600 ;           BDIS1019
              ELSEC ~ (ELSEC DIV 60) MOD 60 ;                           BDIS1020
              PRMIN ~ (PRSEC ~ (TIME(2) - STIME2)) DIV 3600 ;           BDIS1021
              PRSEC ~ (PRSEC DIV 60) MOD 60 ;                           BDIS1022
              IOMIN ~ (IOSEC ~ (TIME(3) - STIME3)) DIV 3600 ;           BDIS1023
              IOSEC ~ (IOSEC DIV 60) MOD 60 ;                           BDIS1024
              WRITE (FOUT1, FFP1, IDNO, FOR I ~ 1 STEP 1 UNTIL 12 DO    BDIS1025
              ID[I], MONTH, DAY, YEAR, ELMIN, ELSEC, PRMIN, PRSEC,      BDIS1026
              IOMIN, IOSEC) ;                                           BDIS1027
              FOR J ~ 1 STEP 1 UNTIL M DO                               BDIS1028
         BEGIN                                                          BDIS1029
              TR1S1[1] ~ TR1S1[2] ~ 0 ;                                 BDIS1030
              TR1S2[J] ~ HUM[J] | SBAR[J] ;                             BDIS1031
              FOR I ~ 1 STEP 1 UNTIL N DO                               BDIS1032
         BEGIN                                                          BDIS1033
              TR1S1[1] ~ TR1S1[1] + X[J,I] ;                            BDIS1034
              TR1S1[2] ~ TR1S1[2] + Y[J,I]                              BDIS1035
         END ;                                                          BDIS1036
              WRITE ( FOUT1, FFP2, J, T[J], HUM[J], TR1S2[J], TR1S1[1], BDIS1037
              TR1S1[2])                                                 BDIS1038
         END ;                                                          BDIS1039
              TR1S1[0] ~ 0 ;     FOR I ~ 1 STEP 1 UNTIL N DO            BDIS1040
              TR1S1[0] ~ TR1S1[0] + D[I] ;                              BDIS1041
              FOR I ~ 1 STEP 1 UNTIL N DO                               BDIS1042
              TR1S1[I] ~ D[I] / TR1S1[0] ;                              BDIS1043
              WRITE (FOUT1, FFP3, SUMD, FOR I ~ 1 STEP 1 UNTIL N DO     BDIS1044
              TR1S1[I]) ;                                               BDIS1045
              WRITE (FOUT1, FFP4, FOR I ~ 1 STEP 1 UNTIL N DO D[I] ) ;  BDIS1046
              WRITE (FOUT1, FFP5) ;                                     BDIS1047
              FOR J ~ 1 STEP 1 UNTIL M DO                               BDIS1048
              WRITE (FOUT1, FFP6, FOR I ~ 1 STEP 1 UNTIL N DO X[J,I] ); BDIS1049
              WRITE (FOUT1, FFP7) ;                                     BDIS1050
              FOR J ~ 1 STEP 1 UNTIL M DO                               BDIS1051
              WRITE (FOUT1, FFP6, FOR I ~ 1 STEP 1 UNTIL N DO Y[J,I]);  BDIS1052
                                                                        BDIS1053
COMMENT       RECYCLE TO READ IN NEXT PROBLEM ;                         BDIS1054
              GO TO BD1 ;                                               BDIS1055
                                                                        BDIS1056
COMMENT       PRINT OUT ERROR MESSAGES;                                 BDIS1057
ERR1:         WRITE (FOUT1, FERR) ;     WRITE (FOUT1, FERR1) ;          BDIS1058
              GO TO BD1 ;                                               BDIS1059
ERR2:         WRITE (FOUT1, FERR) ;     WRITE (FOUT1, FERR2) ;          BDIS1060
              GO TO BD1 ;                                               BDIS1061
ERR3:         WRITE (FOUT1, FERR) ;     WRITE (FOUT1, FERR3) ;          BDIS1062
              GO BD1                                                    BDIS1063
                                                                        BDIS1064
         END  OF BLOCK 2 OF PROGRAM ;                                   BDIS1065
                                                                        BDIS1066
COMMENT       THIS IS THE END OF THE BATCH DISTILLATION PROGRAM ;       BDIS1067
EOFCARD: END.                                                           BDIS1068
"PROBLEM USED IN MEADOWS PAPER       "ID1,    /  DATA SET 1, PROBLEM DATA
"7 TRAYS,  4 COMPONENTS              " ID2,
 1 PRONO,  7 M, 4 N, 2 KEY,20 MAXIT, 100.0 P, 0 TOLT, 0 TOLX, 0 TOLH,
0 LLIMIT, 0 ULIMIT, 1 POPD, 1 POCD, 1 POIR, 3 POIR1, 1 POIR2, 0 QM,
 80.43 SPEC1: % WITHDRAWN, 5.0 SPEC2: L1/D,
 1 READF,                                     /  DATA SET 2, FEED DATA
 50.0, 50.0, 50.0, 50.0,                      /  COMPONENT FEED MOLS
 1 READH,                                     /  DATA SET 3, TRAY DATA
 10.5, 10.5, 10.5, 10.5, 10.5, 10.5 ,            TRAY HOLDUP VOLUME, CU. FT.
 1 READIE,                                    /  DATA SET 4, INITIAL ESTIMATES
160.0 HUM[M],
 0.1, 0.2, 0.3, 0.4,                          /  X[M,I]
 100.0 T[1],  200.0 T[M],
 1 READCD,                                    /  DATA SET5, COMPONENT DATA
    NC4           765.22              550.7                 1.6
    NC5           845.52              489.5                 1.84
    IC5           829.6               483.0                 1.86
     C6           914.1               439.7                 2.08
  -0.14609524@ 03  0.17442830@ 04 -0.88504047@ 04  0.23172909@ 05*CP# 5000NC4  1
  -0.32904241@ 05  0.24047149@ 05 -0.70717229@ 04  0.51791307@ 03*CP# 5000NC4  2
  -0.60967471@ 04  0.30280354@ 05 -0.77393885@ 05  0.10670547@ 06*CP# 5000NC4  3
  -0.75224715@ 05  0.21217935@ 05 -0.63796025@ 03  0.75662458@ 04*CP# 5000NC4  4
  -0.36863956@ 05  0.92089775@ 05 -0.12320518@ 06  0.83414815@ 05*CP# 5000NC4  5
  -0.22305445@ 05  0.28216774@ 03 -0.33359564@ 04  0.16069058@ 05*CP# 5000NC4  6
  -0.39576618@ 05  0.51906262@ 05 -0.34121151@ 05  0.87318732@ 04*CP# 5000NC4  7
  -0.10371977@ 03  0.10966453@ 04 -0.50722388@ 04  0.11754639@ 05*CP# 5000NC5  1
  -0.14127402@ 05  0.81845946@ 04 -0.16956204@ 04  0.24380242@ 03*CP# 5000NC5  2
  -0.21492877@ 04  0.78892828@ 04 -0.11607237@ 05  0.14031742@ 04*CP# 5000NC5  3
   0.11581226@ 05 -0.75607267@ 04 -0.10736680@ 03  0.10561029@ 03*CP# 5000NC5  4
   0.47752449@ 04 -0.28152337@ 05  0.65978843@ 05 -0.70069589@ 05*CP# 5000NC5  5
   0.27856453@ 05 -0.24754430@ 02  0.93370388@ 03 -0.75623480@ 04*CP# 5000NC5  6
   0.28002344@ 05 -0.53335958@ 05  0.50426510@ 05 -0.18653917@ 05*CP# 5000NC5  7
  -0.18305484@ 03  0.21777317@ 04 -0.10945974@ 05  0.28287761@ 05*CP# 5000IC5  1
  -0.39608830@ 05  0.28559400@ 05 -0.82940554@ 04  0.56296145@ 03*CP# 5000IC5  2
  -0.65170005@ 04  0.31723596@ 05 -0.78882897@ 05  0.10528309@ 06*CP# 5000IC5  3
  -0.71581396@ 05  0.19388801@ 05 -0.52509215@ 03  0.58763885@ 04*CP# 5000IC5  4
  -0.26930887@ 05  0.61721460@ 05 -0.73163112@ 05  0.41517839@ 05*CP# 5000IC5  5
  -0.83467070@ 04  0.15132424@ 03 -0.15333466@ 04  0.61208267@ 04*CP# 5000IC5  6
  -0.11000576@ 05  0.72440636@ 04  0.17474641@ 04 -0.28406992@ 04*CP# 5000IC5  7
  -0.38164575@ 03  0.46951509@ 04 -0.23870067@ 05  0.62677434@ 05*CP# 5000NC6  1
  -0.89570741@ 05  0.66094338@ 05 -0.19686499@ 05  0.14660147@ 04*CP# 5000NC6  2
  -0.18043894@ 05  0.90946557@ 05 -0.23662909@ 06  0.33485551@ 06*CP# 5000NC6  3
  -0.24446629@ 06  0.72025953@ 05 -0.18285795@ 04  0.22547523@ 05*CP# 5000NC6  4
  -0.11260287@ 06  0.29003841@ 06 -0.40580315@ 06  0.29239302@ 06*CP# 5000NC6  5
  -0.84860607@ 05  0.76121690@ 03 -0.93390473@ 04  0.46228269@ 05*CP# 5000NC6  6
  -0.11790527@ 06  0.16309981@ 06 -0.11591202@ 06  0.33082319@ 05*CP# 5000NC6  7
   0.15675143@ 05  0.21864464@ 02  0.16089416@-01 -0.17931083@-05 HVC1 - N-C4H10
   0.19066847@ 05  0.26819218@ 02  0.20174027@-01 -0.24417899@-05 HVC1 - N-C5H12
   0.18635740@ 05  0.26220114@ 02  0.21312870@-01 -0.28625929@-05 HVC1 ISO-C5H12
   0.21965245@ 05  0.31947241@ 02  0.24589594@-01 -0.34057931@-05 HVC1  -  C6H14
   0.56381802@ 04  0.32156941@ 02  0.18464268@-01 -0.66078112@-05 HLC5 - N-C4H10
   0.66969614@ 04  0.38053567@ 02  0.22532208@-01 -0.66454438@-05
   0.66009918@ 04  0.37564035@ 02  0.21591715@-01 -0.56235172@-05 HLC5 ISO-C5H12
   0.76972475@ 04  0.44516063@ 02  0.27030713@-01 -0.75881413@-05 HLC5  -  C6H14
