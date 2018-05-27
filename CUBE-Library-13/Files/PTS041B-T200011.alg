PROCEDURE     KPICKW (T, P, PICKK, CALCPT, CALCPP, CALCPDC, K, PKT, PKP,KPWP0001
              PVW, XW, PPVWT, PXWT) ;                                   KPWP0002
                                                                        KPWP0003
COMMENT       THIS PROCEDURE TIES TOGETHER THE PROCEDURES KNGAA, KIDEAL,KPWP0004
              KPOLY, PHIPART, AND KWATER FOR THE PURPOSE OF CALCULATING KPWP0005
              K-RATIOS AND THEIR DERIVATIVES.                           KPWP0006
              BY DONALD L. VOSS                                         KPWP0007
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KPWP0008
              CARD SEQUENCE CODE STARTS WITH KPWP0001.                  KPWP0009
              FIRST RELEASE DATE      10-15-63                          KPWP0010
                                                                        KPWP0011
              THE INPUT PARAMETERS ARE                                  KPWP0012
              T    - TEMPERATURE, F  - REAL                             KPWP0013
              P    - PRESSURE, PSIA  - REAL                             KPWP0014
              PICKK - NO. OF COMPONENTS FOR EACH OPTION - INTEGER ARRAY KPWP0015
                   [0:7].                                               KPWP0016
                   PICKK[0] - TOTAL NO.  OF COMPONENTS                  KPWP0017
                   PICKK[1] - NO. USING KNGAA                           KPWP0018
                   PICKK[2] - NO. USING KIDEAL                          KPWP0019
                   PICKK[3] - NO. USING KPOLY                           KPWP0020
                   PICKK[4] - NO. USING PHIPART                         KPWP0021
                   PICKK[5] - NO. WHOSE K VALUES ARE GIVEN IN KC12[]    KPWP0022
                   PICKK[6] - =1 TO CALCULATE FOR WATER DISSOLVED IN A  KPWP0023
                              HYDROCARBON PHASE.  =0 IF NOT.            KPWP0024
                   PICKK[7] - NO. USING THE INTERPOLATION METHOD.  SEE  KPWP0025
                              KC13 AND KC14.                            KPWP0026
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKT.                KPWP0027
              CALCPP - BOOLEAN.  IF TRUE, CALCULATE PKP.                KPWP0028
              CALCPDC - BOOLEAN.  IF TRUE, CALCULATE THE PRESSURE       KPWP0029
                   DEPENDENT COEFFICIENTS IN KNGAA.  FOR CALCPDC = TRUE,KPWP0030
                   P(LAST ENTRY) MUST EQUAL P(PRESENT ENTRY).           KPWP0031
                                                                        KPWP0032
              IN ADDITION, THE KC- VARIABLES ARE NON-LOCAL TO KPICK.    KPWP0033
              KC1  - N.G.A.A. COEFFICIENTS FOR KNGAA  - REAL ARRAY [0:  KPWP0034
                   PICKK[1],0:28]                                       KPWP0035
              KC2  - VAPOR PRESSURE COEFFICIENTS FOR KIDEAL  - REAL     KPWP0036
                   ARRAY[0:PICKK[2],0:3]                                KPWP0037
              KC3  - WATER VAPOR PRESSURE COEFFICIENTS FOR KWATER  -    KPWP0038
                   REAL ARRAY [0:3]                                     KPWP0039
              KC6  - COEFFICIENTS FOR THE SOLUBILITY OF WATER IN        KPWP0040
                   HYDROCARBONS FOR KWATER  - REAL ARRAY [0:3]          KPWP0041
              KC4  - REFERENCE COMPONENT K COEFFICIENTS FOR KPOLY  -    KPWP0042
                   REAL ARRAY [0:5]                                     KPWP0043
              KC5  - RELATIVE VOLATILITY COEFFICIENTS FOR KPOLY  - REAL KPWP0044
                   ARRAY [0:PICKK[3],0:3]                               KPWP0045
              KC7  - COMPONENT V VALUES IN PHIPART  - INTEGER ARRAY     KPWP0046
                   [0:PICKK[4]]                                         KPWP0047
              KC8  - COMPONENT ALEPH VALUES IN PHIPART  - REAL ARRAY    KPWP0048
                   [0:PICKK[4],0:MAX KC7[],0:MAX KC11[,] ]              KPWP0049
              KC9  - COMPONENT BETA VALUES IN PHIPART  - REAL ARRAY     KPWP0050
                   [0:PICKK[4],0:MAX KC7[],0:MAX KC11[,] ]              KPWP0051
              KC10 - COMPONENT COEFF VALUES IN PHIPART  - REAL ARRAY    KPWP0052
                   [0:PICKK[4],0:MAX (KC11[,1]+1)......(KC11[,V]+1) ]   KPWP0053
              KC11 - COMPONENT P VALUES IN PHIPART  - INTEGER ARRAY     KPWP0054
                   [0:PICKK[4],0:MAX KC7[] ]                            KPWP0055
              IN PHIPART, THE INDEPENDENT VARIABLES ARE T OR T AND P.   KPWP0056
              KC12 - GIVEN VALUES OF COMPONENT K RATIOS  - REAL ARRAY   KPWP0057
                   [0:PICKK[5] ]                                        KPWP0058
              KC13 -  1/T  VALUES FOR INTERPOLATION METHOD  - REAL      KPWP0059
                   ARRAY [0:PICKK[7],0:MAX. M], WHERE KC13[I,0] = M AND KPWP0060
                   M = TOTAL NUMBER OF 1/T VALUES FOR COMPONENT I.  THE KPWP0061
                   VALUES MUST BE STORED IN ORDER OF INCREASING T.  T   KPWP0062
                   IS DEGREES RANKINE.                                  KPWP0063
              KC14 - LN K VALUES CORRESPONDING TO KC13  - REAL ARRAY    KPWP0064
                   [0:PICKK[7],0:MAX. M]                                KPWP0065
                                                                        KPWP0066
              THE OUTPUT PARAMETERS ARE                                 KPWP0067
              K    - COMPONENT K-RATIOS (Y/X)  - REAL ARRAY [0:N],      KPWP0068
                   WHERE N IS THE TOTAL NUMBER OF COMPONENTS.           KPWP0069
              PKT  - COMPONENT T DERIVATIVES OF K  - REAL ARRAY [0:N]   KPWP0070
              PKP  - COMPONENT P DERIVATIVES OF K  - REAL ARRAY [0:N] - KPWP0071
                   (NOT INCLUDED FOR KPOLY AND INTERPOLATION METHOD)    KPWP0072
              PVW  - VAPOR PRESSURE OF WATER, PSIA  - REAL              KPWP0073
              XW   - MOLE FRACTION OF WATER DISSOLVED IN HYDROCARBON    KPWP0074
                   PHASE  - REAL                                        KPWP0075
              PPVWT - T DERIVATIVE OF PVW  - REAL                       KPWP0076
              PXWT - T DERIVATIVE OF XW  - REAL                         KPWP0077
                                                                        KPWP0078
              THE INPUT AND OUTPUT COMPONENT ARRAYS ARE ARRANGED        KPWP0079
                   ACCORDING TO THE PICKK[] ORDER.                      KPWP0080
              THE PROCEDURES KNGAA, KIDEAL, KPOLY, PHIPART, ANT1, AND   KPWP0081
                   KWATER MUST BE DECLARED PRIOR TO KPICKW.             KPWP0082
              THE PROCEDURE ANT1 MUST BE DECLARED PRIOR TO KIDEAL AND   KPWP0083
                   KWATER.  ;                                           KPWP0084
                                                                        KPWP0085
VALUE         T, P, CALCPT, CALCPP, CALCPDC ;                           KPWP0086
REAL          T, P, PVW, XW, PPVWT, PXWT ;                              KPWP0087
BOOLEAN       CALCPT, CALCPP, CALCPDC ;                                 KPWP0088
ARRAY         K[0], PKT[0], PKP[0] ;                                    KPWP0089
INTEGER ARRAY PICKK[0] ;                                                KPWP0090
                                                                        KPWP0091
         BEGIN                                                          KPWP0092
INTEGER       NF1, NF2, NF3, NF4, NF5, NF6, I, E, J, M, U, KC7E, KC11E ;KPWP0093
REAL          TR, T1, T2, K1, K2, SLOPE ;                               KPWP0094
LABEL         KP1 ;                                                     KPWP0095
BOOLEAN ARRAY S[0:2] ;                                                  KPWP0096
REAL ARRAY    DATA, SUM[0:2] ;                                          KPWP0097
                                                                        KPWP0098
COMMENT       SET UP ITERATION LIST VARIABLES ;                         KPWP0099
              NF1 ~ PICKK[1] + PICKK[2] ;        NF2 ~ PICKK[3] + NF1 ; KPWP0100
              NF3 ~ PICKK[4] + NF2 ;             NF4 ~ PICKK[5] + NF3 ; KPWP0101
              NF5 ~ PICKK[6] + NF4 ;             NF6 ~ PICKK[7] + NF5 ; KPWP0102
                                                                        KPWP0103
COMMENT       LINEAR INTERPOLATION OF LN K VERSUS 1/T VALUES ;          KPWP0104
              IF PICKK[7] > 0  THEN                                     KPWP0105
         BEGIN                                                          KPWP0106
              TR ~ 1/(T + 459.6) ;                                      KPWP0107
              FOR I ~ NF5 + 1 STEP 1 UNTIL NF6   DO                     KPWP0108
         BEGIN                                                          KPWP0109
              E ~ I - NF5 ;                                             KPWP0110
              M ~ KC13[E,0] ;                                           KPWP0111
              FOR J ~ 2 STEP 1 UNTIL M  DO                              KPWP0112
              IF TR > KC13[E,J]  THEN   GO TO KP1 ;                     KPWP0113
              J ~ M ;                                                   KPWP0114
KP1:          K1 ~ KC14[E,J-1] ;     K2 ~ KC14[E,J] ;                   KPWP0115
              T1 ~ KC13[E,J-1] ;     T2 ~ KC13[E,J] ;                   KPWP0116
              SLOPE ~ (K2 - K1)/(T2 - T1) ;                             KPWP0117
              K[I] ~ EXP (K1 + SLOPE | (TR - T1)) ;                     KPWP0118
              IF CALCPT  THEN                                           KPWP0119
              PKT[I] ~ - K[I] | SLOPE | TR*2                            KPWP0120
         END                                                            KPWP0121
         END ;                                                          KPWP0122
                                                                        KPWP0123
COMMENT       KWATER VALUES ;                                           KPWP0124
              IF PICKK[6] = 1 THEN                                      KPWP0125
              KWATER (T, P, CALCPT, CALCPP, K[NF5], PVW, XW, PKT[NF5],  KPWP0126
              PKP[NF5], PPVWT, PXWT) ;                                  KPWP0127
                                                                        KPWP0128
COMMENT       K-RATIOS GIVEN IN KC12[] ;                                KPWP0129
              IF PICKK[5] > 0 THEN                                      KPWP0130
              FOR I ~ NF3+1 STEP 1 UNTIL NF4 DO                         KPWP0131
         BEGIN                                                          KPWP0132
              E ~ I - NF3 ;     K[I] ~ KC12[E]                          KPWP0133
         END  ;                                                         KPWP0134
                                                                        KPWP0135
COMMENT       PHIPART VALUES ;                                          KPWP0136
              IF PICKK[4] > 0 THEN                                      KPWP0137
         BEGIN                                                          KPWP0138
              DATA[1] ~ T ;     DATA[2] ~ P ;                           KPWP0139
              S[1] ~ S[2] ~ FALSE ;                                     KPWP0140
              IF CALCPT  THEN  S[1] ~ TRUE ;                            KPWP0141
              IF CALCPP  THEN  S[2] ~ TRUE ;                            KPWP0142
              FOR I ~ NF2 + 1 STEP 1 UNTIL NF3  DO                      KPWP0143
         BEGIN                                                          KPWP0144
              E ~ I - NF2 ;     KC7E ~ KC7[E] ;                         KPWP0145
              IF  KC11[E,1] > KC11[E,2]  THEN   KC11E ~ KC11[E,1]       KPWP0146
              ELSE   KC11E ~ KC11[E,2] ;                                KPWP0147
         BEGIN                                                          KPWP0148
REAL ARRAY    ALEPH, BETA[0:2,0:KC11E] ;                                KPWP0149
              FOR J ~ 1 STEP 1 UNTIL KC7E  DO                           KPWP0150
              FOR  U ~ 1 STEP 1 UNTIL KC11E  DO                         KPWP0151
         BEGIN                                                          KPWP0152
              ALEPH[J,U] ~ KC8[E,J,U] ;     BETA[J,U] ~ KC9[E,J,U]      KPWP0153
         END ;                                                          KPWP0154
              PHIPART (KC7[E], DATA, ALEPH, BETA, KC10[E,*], KC11[E,*], KPWP0155
              S, K[I], SUM) ;                                           KPWP0156
              IF CALCPT  THEN  PKT[I] ~ SUM[1] ;                        KPWP0157
              IF CALCPP  THEN  PKP[I] ~ SUM[2]                          KPWP0158
         END                                                            KPWP0159
         END                                                            KPWP0160
         END  ;                                                         KPWP0161
                                                                        KPWP0162
COMMENT       KPOLY VALUES ;                                            KPWP0163
              IF PICKK[3] > 0 THEN                                      KPWP0164
         BEGIN                                                          KPWP0165
              KPOLY (T, PICKK[3], CALCPT, K, PKT) ;                     KPWP0166
              FOR I ~ NF1+1 STEP 1 UNTIL NF2 DO                         KPWP0167
         BEGIN                                                          KPWP0168
              E ~ I - NF1 ;     K[I] ~ K[E] ;                           KPWP0169
              IF CALCPT  THEN  PKT[I] ~ PKT[E]                          KPWP0170
         END                                                            KPWP0171
         END  ;                                                         KPWP0172
                                                                        KPWP0173
COMMENT       KIDEAL VALUES ;                                           KPWP0174
              IF PICKK[2] > 0 THEN                                      KPWP0175
         BEGIN                                                          KPWP0176
              KIDEAL (T, P, PICKK[2], CALCPT, CALCPP, K, PKT, PKP) ;    KPWP0177
              FOR I ~ PICKK[1]+1 STEP 1 UNTIL NF1 DO                    KPWP0178
         BEGIN                                                          KPWP0179
              E ~ I - PICKK[1] ;     K[I] ~ K[E] ;                      KPWP0180
              IF CALCPT  THEN  PKT[I] ~ PKT[E] ;                        KPWP0181
              IF CALCPP  THEN  PKP[I] ~ PKP[E]                          KPWP0182
         END                                                            KPWP0183
         END  ;                                                         KPWP0184
                                                                        KPWP0185
COMMENT       KNGAA VALUES ;                                            KPWP0186
              IF PICKK[1] > 0 THEN                                      KPWP0187
              KNGAA (T, P, PICKK[1], CALCPDC, CALCPT, CALCPP, K, PKT,   KPWP0188
              PKP)                                                      KPWP0189
                                                                        KPWP0190
         END  KPICKW ;                                                  KPWP0191
