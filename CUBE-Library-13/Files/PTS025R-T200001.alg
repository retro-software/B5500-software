                                              00000000         BEGIN
                                      BWRE   100000000COMMENT       BENEDICT-WEB
EQUATION OF STATE FOR                 BWRE   20000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000
              COMPUTATION OF K-RATIOS AND ENTHALPIES ;                  BWRE   3

INTEGER       I, N ;                                                    BWRE   4

REAL          MOLES, TEMP, PRESS ;                                      BWRE   5

FILE IN       CARD1 (2,10) ;                                            BWRE   6

FORMAT IN     CARD2 (X10, I5, 3F10.5) ;                                 BWRE   7

LIST          INITIAL (N, MOLES, PRESS, TEMP) ;                         BWRE   8

LABEL         READDATA, DONE ;                                          BWRE  A8
COMMENT       READ INITIAL DATA ;                                       BWRE   9
READDATA :    READ (CARD1, CARD2, INITIAL) [DONE] ;                     BWRE  10
         BEGIN                                                          BWRE  11
REAL ARRAY    FEED[0:N], X[0:N], Y[0:N], IBO[0:N], IAO[0:N], ICO[0:N],  BWRE  12
              IB[0:N], IA[0:N], IC[0:N], IALF[0:N], IGAM[0:N], K[0:N],  BWRE  13
              KO[0:N], XFUG[0:N], YFUG[0:N], H[0:N], S[0:N] ;           BWRE  14

REAL          RT, T2, R, DT2, RT3, LDEN, VDEN, LBO, VBO, LAO, VAO, LCO, BWRE  15
              VCO, LB, VB, LA, VA, LC, VC, LALF, VALF, LGAM, VGAM, XI,  BWRE  16
              YI, HL, HV, SL, SV, DL, DV, ENTHALPYL, ENTHALPYV,         BWRE  17
              ENTROPYL, ENTROPYV ;                                      BWRE  18

LABEL         ERROR, ENTRANCE ;                                         BWRE  19

FORMAT IN     CARD3 (E20.11, 2(/ 4E20.11) / 2E20.11) ;                  BWRE  20

LIST          DATA (FOR I ~ 1 STEP 1 UNTIL N DO                         BWRE  21
              [ FEED[I], IBO[I], IAO[I], ICO[I], IB[I], IA[I], IC[I],   BWRE  22
              IALF[I], IGAM[I], H[I], S[I] ]) ;                         BWRE  23

FILE OUT      PAPER 1(2,15) ;                                           BWRE  24

LIST          OUTDENS (LDEN, VDEN),                                     BWRE  25
              OUTENTH (ENTHALPYL, ENTHALPYV),                           BWRE  26
              OUTENTR (ENTROPYL, ENTROPYV),                             BWRE  27
              OUTCOMP (I, FEED[I], K[I], X[I], XFUG[I], Y[I], YFUG[I]) ;BWRE  28

FORMAT OUT    TITLE (X25, "THERMODYNAMIC PROPERTIES AND ",              BWRE  29
              "PHASE BEHAVIOR OF LIGHT HYDROCARBON ",                   BWRE  30
              "MIXTURES" / X33, "CALCULATED FROM THE ",                 BWRE  31
              "BENEDICT-WEBB-RUBIN EQUATION OF STATE" //),              BWRE  32
              INITIALF ("NO. OF COMPONENTS = ", I3, X5,                 BWRE  33
              "TOTAL MOLES = ", F10.5, X7, "PRESSURE, PSIA = ", F10.5,  BWRE  34
              X5, "TEMPERATURE, DEG R = ", F8.3 //),                    BWRE  35
              PHASE (X46, "LIQUID", X17, "VAPOR" //),                   BWRE  36
              PHASEDENS (X20, "DENSITY", X13, E17.11, X8, E17.11 //),   BWRE  37
              PHASEENTH (X20, "ENTHALPY", X12, E17.11, X8, E17.11 //),  BWRE  38
              PHASEENTR (X20, "ENTROPY", X13, E17.11, X8, E17.11 //),   BWRE  39
              ERR ("ISOTHERMAL FLASH RATIO L/F EXCEEDS ",               BWRE  40
              "LIMITS, 0>L/F>1" //),                                    BWRE  41
              COMP (X1, "COMP", X9, "FEED", X13, "K-RATIO",             BWRE  42
              X13, "LIQUID", X13, "LIQFUG", X13, "VAPOR",               BWRE  43
              X14, "VAPFUG" //),                                        BWRE  44
              COMPDATA (X1, I3, X2, 6E19.11 //) ;                       BWRE  45

PROCEDURE     IFLASH ;                                                  BWRE  46
COMMENT       THIS ISOTHERMAL FLASH ROUTINE WILL CALCULATE THE PHASE    BWRE  47
              COMPOSITION OF TWO PHASE MIXTURES.  IF THE MIXTURE IS ALL BWRE  48
              LIQUID OR ALL VAPOR, THE ROUTINE EXITS WITH THE           BWRE  49
              CORRESPONDING RESULTS.  FOR TWO PHASES, A NEWTON-RAPHSON  BWRE  50
              METHOD IS USED ;                                          BWRE  51
         BEGIN                                                          BWRE  52
REAL          TVS1, TVS2, TVS3, GF, PGF, KI ;                           BWRE  53

LABEL         I1, I2, I3, I4, EXIT ;                                    BWRE  54
COMMENT       DETERMINE IF TWO PHASES EXIST ;                           BWRE  55
              TVS1 ~ TVS2 ~ 0 ;                                         BWRE  56
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE  57
         BEGIN                                                          BWRE  58
              KI ~ K[I] ;                                               BWRE  59
              IF KI = 0 THEN                                            BWRE  60
         BEGIN                                                          BWRE  61
              TVS1 ~ 1.1 ;          GO TO I1                            BWRE  62
         END ;                                                          BWRE  63
              TVS1 ~ FEED[I] / KI + TVS1 ;                              BWRE  64

I1 :          TVS2 ~ FEED[I] | KI + TVS2                                BWRE  65
         END ;                                                          BWRE  66
              IF TVS1 > 1.0 THEN GO TO I2 ;                             BWRE  67
COMMENT       1. VAPOR PHASE ONLY ;                                     BWRE  68
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE  69
         BEGIN                                                          BWRE  70
              X[I] ~ 0 ;          Y[I] ~ FEED[I]                        BWRE  71
         END ;                                                          BWRE  72
              GO TO EXIT ;                                              BWRE  73

I2 :          IF TVS2 > 1.0 THEN GO TO I3 ;                             BWRE  74
COMMENT       2. LIQUID PHASE ONLY ;                                    BWRE  75
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE  76
         BEGIN                                                          BWRE  77
              Y[I] ~ 0 ;          X[I] ~ FEED[I]                        BWRE  78
         END ;                                                          BWRE  79
              GO TO EXIT ;                                              BWRE  80
COMMENT       START CALCULATIONS WITH L/F = 0.5 ;                       BWRE  81

I3 :          TVS1 ~ 0.5 ;                                              BWRE  82
COMMENT       ITERATE TO DETERMINE L/F ;                                BWRE  83

I4 :          GF ~ PGF ~ 0 ;                                            BWRE  84
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE  85
         BEGIN                                                          BWRE  86
              TVS2 ~ 1.0 - K[I] ;                                       BWRE  87
              TVS3 ~ 1.0 / (TVS1 | TVS2 + K[I]) ;                       BWRE  88
              GF ~ FEED[I] | TVS2 | TVS3 + GF ;                         BWRE  89
              PGF ~ - (TVS2 | TVS3) * 2 | FEED[I] + PGF                 BWRE  90
         END ;                                                          BWRE  91
              TVS2 ~ GF / PGF ;                                         BWRE  92
              TVS1 ~ TVS1 - TVS2 ;                                      BWRE  93
              IF TVS1 < 0 THEN TVS1 ~ 0                                 BWRE  9
              ELSE IF TVS1 > 1.0 THEN TVS1 ~ 1.0 ;                      BWRE  95
              IF ABS (TVS2) > 1@-6 THEN GO TO I4 ;                      BWRE  96
COMMENT       CALCULATE PHASE COMPOSITIONS ;                            BWRE  97
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE  98
         BEGIN                                                          BWRE  99
              X[I] ~ FEED[I] / ((1.0 - TVS1) | K[I] + TVS1) ;           BWRE 100
              Y[I] ~ X[I] | K[I]                                        BWRE 101
         END ;                                                          BWRE 102

EXIT :   END IFLASH ;                                                   BWRE 103

PROCEDURE     DENFUGTY ( BO, AO, CO, B, A, C, ALF, GAM, D, FUG ) ;      BWRE 104

VALUE         BO, AO, CO, B, A, C, ALF, GAM ;                           BWRE 105

REAL          BO, AO, CO, B, A, C, ALF, GAM, D ;                        BWRE 106

REAL ARRAY    FUG[0] ;                                                  BWRE 107
COMMENT       DENSITY AND FUGACITY ARE CALCULATED FOR                   BWRE 108
              EITHER THE LIQUID OR VAPOR PHASE ;                        BWRE 109
         BEGIN                                                          BWRE 110
REAL          A3, C3, BAC, BA, AALF, GAM2, DD, D2, D3, DRT, DRT3, GD2,  BWRE 111
              TAU, CD, PHI, PART, TG, KAO, KCO, KB, KA, KK, KAA, KALF,  BWRE 112
              KC, KGAM ;                                                BWRE 113

LABEL         EVAL, CALCFUG ;                                           BWRE 114
COMMENT       COMMON TERMS ARE EVALUATED FOR SPEED ;                    BWRE 115
              A3 ~ A * 2 | A ;          C3 ~ C * 2 | C ;                BWRE 116
              BAC ~ BO | RT - AO * 2 - CO * 2 | DT2 ;                   BWRE 117
              BA ~ B * 2 | B | RT - A3 ;          D ~ 1.0 ;             BWRE 118
              AALF ~ ALF * 2 | ALF | A3 ;                               BWRE 119
              GAM2 ~ GAM * 2 ;          DD ~ 1@-5 ;                     BWRE 120
COMMENT       BEGIN DENSITY ITERATION ;                                 BWRE 121

EVAL :        D2 ~ D * 2 ;          D3 ~ D | D2 ;                       BWRE 122
              DRT ~ D / RT ;          DRT3 ~ D / RT3 ;                  BWRE 123
              GD2 ~ GAM2 | D2 ;          TAU ~ EXP (-GD2) ;             BWRE 124
COMMENT       COMPARE DELTA D WITH THE TOLERANCE ;                      BWRE 125
              IF ABS (DD) < 1@-6 THEN GO TO CALCFUG ;                   BWRE 126
              CD ~ (1 + GD2) | TAU | C3 | D2 | DT2 ;                    BWRE 127
COMMENT       CALCULATE THE PRESSURE FROM THE BWR EQUATION              BWRE 128
              AND ITS PARTIAL WITH RESPECT TO DENSITY ;                 BWRE 129
              PHI ~ (((D3 | AALF + BA) | D + BAC) | D + RT + CD) | D ;  BWRE 130
              PART ~ ((((-GAM2 * 2 | DT2 | TAU | 2.0 | C3) | D3 + 6.0 | BWRE 131
              AALF | D2) | D + 3.0 | BA) | D + 2.0 | BAC) | D + RT +    BWRE 132
              3.0 | CD ;                                                BWRE 133
COMMENT       EVALUATE DELTA D ;                                        BWRE 134
              DD ~ -(PHI - PRESS) / PART ;                              BWRE 135
              D ~ D + DD ;          GO TO EVAL ;                        BWRE 136
COMMENT       COMMON TERMS ARE EVALUATED FOR SPEED ;                    BWRE 137

CALCFUG :     TG ~ (1 - TAU) / GD2 ;                                    BWRE 138
              KAO ~ (DRT + DRT) | AO ;                                  BWRE 139
              KCO ~ (DRT3 + DRT3) | CO ;                                BWRE 140
              KB ~ (D | B) * 2 | 1.5 ;                                  BWRE 141
              KA ~ 1.5 | DRT | A | A | D ;                              BWRE 142
              KK ~ (D2 | A | ALF) * 2 | 0.6 | DRT ;                     BWRE 143
              KAA ~ KK | ALF ;          KALF ~ KK | A ;                 BWRE 144
              KC ~ (TG - 0.5 | TAU) | 3.0 | DRT3 | D | C | C ;          BWRE 145
              KGAM ~ (TG - TAU - 0.5 | GD2 | TAU) | 2.0 | DRT3 | D |    BWRE 146
              C3 / GAM ;                                                BWRE 147
COMMENT       COMPUTE THE FUGACITY FOR THE INDIVIDUAL COMPONENTS ;      BWRE 148
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE 149
         BEGIN                                                          BWRE 150
              FUG[I] ~ (IBO[I] + BO) | D - IAO[I] | KAO - ICO[I] | KCO +BWRE 151
              IB[I] | KB + (KAA - KA) | IA[I] + IALF[I] | KALF +        BWRE 152
              IC[I] | KC - IGAM[I] | KGAM ;                             BWRE 153
              FUG[I] ~ EXP (FUG[I]) | RT | D                            BWRE 154
         END                                                            BWRE 155
         END DENFUGTY ;                                                 BWRE 156

PROCEDURE     HS ( D, BO, AO, CO, B, A, C, ALF, GAM, HE, SE ) ;         BWRE 157
COMMENT       COMPUTE THE ENTHALPY AND ENTROPY CORRECTIONS              BWRE 158
              FOR THE LIQUID AND VAPOR PHASES ;                         BWRE 159

VALUE         D, BO, AO, CO, B, A, C, ALF, GAM ;                        BWRE 160

REAL          D, BO, AO, CO, B, A, C, ALF, GAM, HE, SE ;                BWRE 161
         BEGIN                                                          BWRE 162
REAL          D2, GD2, TAU ;                                            BWRE 163
              D2 ~ D | D ;          GD2 ~ GAM * 2 | D2 ;                BWRE 164
              TAU ~ EXP (-GD2) ;                                        BWRE 165
COMMENT       COMPUTE THE ENTHALPY CORRECTIONS ;                        BWRE 166
              HE ~ (1.987 / R) | (RT | (BO | D + B * 2 | B | D2) -      BWRE 167
              AO * 2 | D | 2.0 - CO * 2 | D | DT2 | 4.0 + (ALF * 2 |    BWRE 168
              ALF | D | D2 | 0.4 - 0.5) | A * 2 | A | D2 | 3.0 + (3.0 | BWRE 169
              (1.0 - TAU) / GD2 + (GD2 - 0.5) | TAU) | C * 2 | C |      BWRE 170
              DT2 | D2) ;                                               BWRE 171
COMMENT       COMPUTE THE ENTROPY CORRECTIONS ;                         BWRE 172
              SE ~ (1.987 / R) | (D | (BO | R + 2.0 | CO * 2 | DT2 /    BWRE 173
              TEMP) + B * 2 | B | R | D2 | 0.5 - (C * 2 | C | DT2 |     BWRE 174
              2.0 | D2 / TEMP) | ((1.0 - TAU) / GD2 - 0.5 | TAU))       BWRE 175
         END HS ;                                                       BWRE 176
COMMENT       READ DATA AND GENERATE THE INITIAL VALUES ;               BWRE 177

              READ (CARD1, CARD3, DATA) ;                               BWRE 178
              R ~ 10.7335 ;          RT ~ R | TEMP ;                    BWRE 179
              T2 ~ TEMP * 2 ;         DT2 ~ 1.0 / T2 ;                  BWRE 180
              RT3 ~ RT | T2 ;                                           BWRE 181
              K[1] ~ 10 * ((N - 1.0) / 2.0) ;                           BWRE 182
              FOR I ~ 2 STEP 1 UNTIL N DO                               BWRE 183
              K[I] ~ K[I-1] | 0.1 ;                                     BWRE 184
COMMENT       BREAK DOWN FEED INTO LIQUID AND VAPOR ;                   BWRE 185

ENTRANCE :    IFLASH ;                                                  BWRE 186
COMMENT       EVALUATE THE PHASE COEFFICIENTS FOR THE TWO PHASES ;      BWRE 187
              LBO ~ VBO ~ LAO ~ VAO ~ LCO ~ VCO ~ LB ~ VB ~ 0 ;         BWRE 188
              LA ~ VA ~ LC ~ VC ~ LALF ~ VALF ~ LGAM ~ VGAM ~ 0 ;       BWRE 189
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE 190
         BEGIN                                                          BWRE 191
              KO[I] ~ K[I] ;                                            BWRE 192
              XI ~ X[I] ;          YI ~ Y[I] ;                          BWRE 193
              LBO ~ IBO[I] | XI + LBO ;                                 BWRE 194
              VBO ~ IBO[I] | YI + VBO ;                                 BWRE 195
              LAO ~ IAO[I] | XI + LAO ;                                 BWRE 196
              VAO ~ IAO[I] | YI + VAO ;                                 BWRE 197
              LCO ~ ICO[I] | XI + LCO ;                                 BWRE 198
              VCO ~ ICO[I] | YI + VCO ;                                 BWRE 199
              LB ~ IB[I] | XI + LB ;                                    BWRE 200
              VB ~ IB[I] | YI + VB ;                                    BWRE 201
              LA ~ IA[I] | XI + LA ;                                    BWRE 202
              VA ~ IA[I] | YI + VA ;                                    BWRE 203
              LC ~ IC[I] | XI + LC ;                                    BWRE 204
              VC ~ IC[I] | YI + VC ;                                    BWRE 205
              LALF ~ IALF[I] | XI + LALF ;                              BWRE 206
              VALF ~ IALF[I] | YI + VALF ;                              BWRE 207
              LGAM ~ IGAM[I] | XI + LGAM ;                              BWRE 208
              VGAM ~ IGAM[I] | YI + VGAM                                BWRE 209
         END ;                                                          BWRE 210
COMMENT       EVALUATE THE LIQUID DENSITY AND FUGACITY ;                BWRE 211
              DENFUGTY ( LBO, LAO, LCO, LB, LA, LC, LALF,               BWRE 212
              LGAM, LDEN, XFUG ) ;                                      BWRE 213
COMMENT       EVALUATE THE VAPOR DENSITY AND FUGACITY ;                 BWRE 214
              DENFUGTY ( VBO, VAO, VCO, VB, VA, VC, VALF,               BWRE 215
              VGAM, VDEN, YFUG ) ;                                      BWRE 216
COMMENT       COMPUTE THE K-RATIOS FOR THE INDIVIDUAL COMPONENTS ;      BWRE 217
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE 218
              K[I] ~ XFUG[I] / YFUG[I] ;                                BWRE 219
COMMENT       REPEAT THE ENTIRE ITERATION IF ANY OF THE                 BWRE 220
              K(I) DIFFERENCES EXCEED THE TOLERANCE ;                   BWRE 221
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE 222
              IF ABS (KO[I] - K[I]) > 5@-6 THEN                         BWRE 223
              GO TO ENTRANCE ;                                          BWRE 224
COMMENT       COMPUTE THE UNCORRECTED ENTHALPY AND ENTROPY              BWRE 225
              FOR THE TWO PHASES ;                                      BWRE 226
              HL ~ HV ~ SL ~ SV ~ 0 ;                                   BWRE 227
              DL ~ RT | LDEN ;          DV ~ RT | VDEN ;                BWRE 228
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE 229
         BEGIN                                                          BWRE 230
              XI ~ X[I] ;          YI ~ Y[I] ;                          BWRE 231
              HL ~ H[I] | XI + HL ;         HV ~ H[I] | YI + HV ;       BWRE 232
              SL ~ (-1.987 | LN (DL | XI) + S[I]) | XI + SL ;           BWRE 233
              SV ~ (-1.987 | LN (DV | YI) + S[I]) | YI + SV             BWRE 234
         END ;                                                          BWRE 235
COMMENT       EVALUATE THE LIQUID, THEN VAPOR, ENTHALPY                 BWRE 236
              AND ENTROPY CORRECTIONS ;                                 BWRE 237
              HS ( LDEN, LBO, LAO, LCO, LB, LA, LC, LALF,               BWRE 238
              LGAM, ENTHALPYL, ENTROPYL ) ;                             BWRE 239
              HS ( VDEN, VBO, VAO, VCO, VB, VA, VC, VALF,               BWRE 240
              VGAM, ENTHALPYV, ENTROPYV ) ;                             BWRE 241
              ENTHALPYL ~ HL + ENTHALPYL ;                              BWRE 242
              ENTHALPYV ~ HV + ENTHALPYV ;                              BWRE 243
              ENTROPYL ~ SL - ENTROPYL ;                                BWRE 244
              ENTROPYV ~ SV - ENTROPYV ;                                BWRE 245
COMMENT       PRINT THE FINAL RESULTS ;                                 BWRE 246
              WRITE (PAPER [PAGE], COMPDATA) ;                          BWRE246A
              WRITE (PAPER, TITLE) ;                                    BWRE 247
              WRITE (PAPER, INITIALF, INITIAL) ;                        BWRE 248
              WRITE (PAPER, PHASE) ;                                    BWRE 249
              WRITE (PAPER, PHASEDENS, OUTDENS) ;                       BWRE 250
              WRITE (PAPER, PHASEENTH, OUTENTH) ;                       BWRE 251
              WRITE (PAPER, PHASEENTR, OUTENTR) ;                       BWRE 252
              WRITE (PAPER, COMP) ;                                     BWRE 253
              FOR I ~ 1 STEP 1 UNTIL N DO                               BWRE 254
              WRITE (PAPER, COMPDATA, OUTCOMP) ;                        BWRE 255
              GO TO READDATA                                            BWRE 256
         END ;                                                          BWRE 257
DONE :   END ;                                                          BWRE 258
         END .

 LABEL  000000000CARD1  0010000001
              5   1.0    1297.0     560.0
   0.7057       @
   0.682401     @     83.6376       @      1.66061      @  4   0.953644     @
  14.397        @      7.92684      @  2   0.799567     @      1.24082      @
   2.5083111    @  3  44.6859       @
   0.0669       @
   1.00554      @      1.25183      @  2   4.6843       @  4   1.41845      @
  27.5235       @      1.85788      @  3   1.00015      @      1.74009      @
   3.0184134    @  3  55.2734       @
   0.0413       @
   1.55884      @      1.60983      @  2   7.88031      @  4   1.79396      @
  38.5408       @      2.9336       @  3   1.35644      @      2.37597      @
   3.7455293    @  3  65.2090       @
   0.0508       @
   1.99211      @      1.95012      @  2   1.10139      @  5   2.1732       @
  48.4464       @      3.95625      @  3   1.65425      @      2.95372      @
   4.9527562    @  3  75.0172       @
   0.1353       @
   2.51096      @      2.1431       @  2   1.605067     @  5   2.57853      @
  62.6707       @      5.44349      @  3   1.9522       @      3.49122      @
   6.0071195    @  3  84.5055       @
