PROCEDURE     KPICK (T, P, PICKK, CALCPT, CALCPP, CALCPDC, K, PKT, PKP);KPKP0001
                                                                        KPKP0002
COMMENT       THIS PROCEDURE TIES TOGETHER THE PROCEDURES KNGAA, KIDEAL,KPKP0003
              KPOLY, AND PHIPART FOR THE PURPOSE OF CALCULATING K       KPKP0004
              RATIOS AND THEIR DERIVATIVES.                             KPKP0005
              BY DONALD L. VOSS                                         KPKP0006
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KPKP0007
              CARD SEQUENCE CODE STARTS WITH KPKP0001.                  KPKP0008
              FIRST RELEASE DATE      10-15-63                          KPKP0009
                                                                        KPKP0010
              THE INPUT PARAMETERS ARE                                  KPKP0011
              T    - TEMPERATURE, F  - REAL                             KPKP0012
              P    - PRESSURE, PSIA  - REAL                             KPKP0013
              PICKK - NO. OF COMPONENTS FOR EACH OPTION - INTEGER ARRAY KPKP0014
                   [0:7].                                               KPKP0015
                   PICKK[0] - TOTAL NO.  OF COMPONENTS                  KPKP0016
                   PICKK[1] - NO. USING KNGAA                           KPKP0017
                   PICKK[2] - NO. USING KIDEAL                          KPKP0018
                   PICKK[3] - NO. USING KPOLY                           KPKP0019
                   PICKK[4] - NO. USING PHIPART                         KPKP0020
                   PICKK[5] - NO. WHOSE K VALUES ARE GIVEN IN KC12[]    KPKP0021
                   PICKK[7] - NO. USING THE INTERPOLATION METHOD.  SEE  KPKP0022
                              KC13 AND KC14.                            KPKP0023
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKT.                KPKP0024
              CALCPP - BOOLEAN.  IF TRUE, CALCULATE PKP.                KPKP0025
              CALCPDC - BOOLEAN.  IF TRUE, CALCULATE THE PRESSURE       KPKP0026
                   DEPENDENT COEFFICIENTS IN KNGAA.  FOR CALCPDC = TRUE,KPKP0027
                   P(LAST ENTRY) MUST EQUAL P(PRESENT ENTRY).           KPKP0028
                                                                        KPKP0029
              IN ADDITION, THE KC- VARIABLES ARE NON-LOCAL TO KPICK.    KPKP0030
              KC1  - N.G.A.A. COEFFICIENTS FOR KNGAA  - REAL ARRAY [0:  KPKP0031
                   PICKK[1],0:28]                                       KPKP0032
              KC2  - VAPOR PRESSURE COEFFICIENTS FOR KIDEAL  - REAL     KPKP0033
                   ARRAY[0:PICKK[2],0:3]                                KPKP0034
              KC4  - REFERENCE COMPONENT K COEFFICIENTS FOR KPOLY  -    KPKP0035
                   REAL ARRAY [0:5]                                     KPKP0036
              KC5  - RELATIVE VOLATILITY COEFFICIENTS FOR KPOLY  - REAL KPKP0037
                   ARRAY [0:PICKK[3],0:3]                               KPKP0038
              KC7  - COMPONENT V VALUES IN PHIPART  - INTEGER ARRAY     KPKP0039
                   [0:PICKK[4]]                                         KPKP0040
              KC8  - COMPONENT ALEPH VALUES IN PHIPART  - REAL ARRAY    KPKP0041
                   [0:PICKK[4],0:MAX KC7[],0:MAX KC11[,] ]              KPKP0042
              KC9  - COMPONENT BETA VALUES IN PHIPART  - REAL ARRAY     KPKP0043
                   [0:PICKK[4],0:MAX KC7[],0:MAX KC11[,] ]              KPKP0044
              KC10 - COMPONENT COEFF VALUES IN PHIPART  - REAL ARRAY    KPKP0045
                   [0:PICKK[4],0:MAX (KC11[,1]+1)......(KC11[,V]+1) ]   KPKP0046
              KC11 - COMPONENT P VALUES IN PHIPART  - INTEGER ARRAY     KPKP0047
                   [0:PICKK[4],0:MAX KC7[] ]                            KPKP0048
              IN PHIPART, THE INDEPENDENT VARIABLES ARE T OR T AND P.   KPKP0049
              KC12 - GIVEN VALUES OF COMPONENT K RATIOS  - REAL ARRAY   KPKP0050
                   [0:PICKK[5] ]                                        KPKP0051
              KC13 -  1/T  VALUES FOR INTERPOLATION METHOD  - REAL      KPKP0052
                   ARRAY [0:PICKK[7],0:MAX. M], WHERE KC13[I,0] = M AND KPKP0053
                   M = TOTAL NUMBER OF 1/T VALUES FOR COMPONENT I.  THE KPKP0054
                   VALUES MUST BE STORED IN ORDER OF INCREASING T.  T   KPKP0055
                   IS DEGREES RANKINE.                                  KPKP0056
              KC14 - LN K VALUES CORRESPONDING TO KC13  - REAL ARRAY    KPKP0057
                   [0:PICKK[7],0:MAX. M]                                KPKP0058
                                                                        KPKP0059
              THE OUTPUT PARAMETERS ARE                                 KPKP0060
              K    - COMPONENT K-RATIOS (Y/X)  - REAL ARRAY [0:N],      KPKP0061
                   WHERE N IS THE TOTAL NUMBER OF COMPONENTS.           KPKP0062
              PKT  - COMPONENT T DERIVATIVES OF K  - REAL ARRAY [0:N]   KPKP0063
              PKP  - COMPONENT P DERIVATIVES OF K  - REAL ARRAY [0:N] - KPKP0064
                   (NOT INCLUDED FOR KPOLY AND INTERPOLATION METHOD)    KPKP0065
                                                                        KPKP0066
              THE INPUT AND OUTPUT COMPONENT ARRAYS ARE ARRANGED        KPKP0067
                   ACCORDING TO THE PICKK[] ORDER.                      KPKP0068
              THE PROCEDURES KNGAA, KIDEAL, KPOLY, PHIPART, AND ANT1    KPKP0069
                   MUST BE DECLARED PRIOR TO KPICK.                     KPKP0070
              THE PROCEDURE ANT1 MUST BE DECLARED PRIOR TO KIDEAL.  ;   KPKP0071
                                                                        KPKP0072
VALUE         T, P, CALCPT, CALCPP, CALCPDC ;                           KPKP0073
REAL          T, P ;                                                    KPKP0074
BOOLEAN       CALCPT, CALCPP, CALCPDC ;                                 KPKP0075
ARRAY         K[0], PKT[0], PKP[0] ;                                    KPKP0076
INTEGER ARRAY PICKK[0] ;                                                KPKP0077
                                                                        KPKP0078
         BEGIN                                                          KPKP0079
INTEGER       NF1, NF2, NF3, NF4, NF5, I, E, J, M, U, KC7E, KC11E ;     KPKP0080
REAL          TR, T1, T2, K1, K2, SLOPE ;                               KPKP0081
LABEL         KP1 ;                                                     KPKP0082
BOOLEAN ARRAY S[0:2] ;                                                  KPKP0083
REAL ARRAY    DATA, SUM[0:2] ;                                          KPKP0084
                                                                        KPKP0085
COMMENT       SET UP ITERATION LIST VARIABLES ;                         KPKP0086
              NF1 ~ PICKK[1] + PICKK[2] ;        NF2 ~ PICKK[3] + NF1 ; KPKP0087
              NF3 ~ PICKK[4] + NF2 ;             NF4 ~ PICKK[5] + NF3 ; KPKP0088
              NF5 ~ PICKK[7] + NF4 ;                                    KPKP0089
                                                                        KPKP0090
COMMENT       LINEAR INTERPOLATION OF LN K VERSUS 1/T VALUES ;          KPKP0091
              IF PICKK[7] > 0  THEN                                     KPKP0092
         BEGIN                                                          KPKP0093
              TR ~ 1/(T + 459.6) ;                                      KPKP0094
              FOR I ~ NF4 + 1 STEP 1 UNTIL NF5  DO                      KPKP0095
         BEGIN                                                          KPKP0096
              E ~ I - NF4 ;                                             KPKP0097
              M ~ KC13[E,0] ;                                           KPKP0098
              FOR J ~ 2 STEP 1 UNTIL M  DO                              KPKP0099
              IF TR > KC13[E,J]  THEN   GO TO KP1 ;                     KPKP0100
              J ~ M ;                                                   KPKP0101
KP1:          K1 ~ KC14[E,J-1] ;     K2 ~ KC14[E,J] ;                   KPKP0102
              T1 ~ KC13[E,J-1] ;     T2 ~ KC13[E,J] ;                   KPKP0103
              SLOPE ~ (K2 - K1)/(T2 - T1) ;                             KPKP0104
              K[I] ~ EXP (K1 + SLOPE | (TR - T1)) ;                     KPKP0105
              IF CALCPT  THEN                                           KPKP0106
              PKT[I] ~ - K[I] | SLOPE | TR*2                            KPKP0107
         END                                                            KPKP0108
         END ;                                                          KPKP0109
                                                                        KPKP0110
COMMENT       K-RATIOS GIVEN IN KC12[] ;                                KPKP0111
              IF PICKK[5] > 0 THEN                                      KPKP0112
              FOR I ~ NF3+1 STEP 1 UNTIL NF4 DO                         KPKP0113
         BEGIN                                                          KPKP0114
              E ~ I - NF3 ;     K[I] ~ KC12[E]                          KPKP0115
         END  ;                                                         KPKP0116
                                                                        KPKP0117
COMMENT       PHIPART VALUES ;                                          KPKP0118
              IF PICKK[4] > 0 THEN                                      KPKP0119
         BEGIN                                                          KPKP0120
              DATA[1] ~ T ;     DATA[2] ~ P ;                           KPKP0121
              S[1] ~ S[2] ~ FALSE ;                                     KPKP0122
              IF CALCPT  THEN  S[1] ~ TRUE ;                            KPKP0123
              IF CALCPP  THEN  S[2] ~ TRUE ;                            KPKP0124
              FOR I ~ NF2 + 1 STEP 1 UNTIL NF3  DO                      KPKP0125
         BEGIN                                                          KPKP0126
              E ~ I - NF2 ;     KC7E ~ KC7[E]  ;                        KPKP0127
              IF  KC11[E,1] > KC11[E,2]  THEN   KC11E ~ KC11[E,1]       KPKP0128
              ELSE   KC11E ~ KC11[E,2]  ;                               KPKP0129
         BEGIN                                                          KPKP0130
REAL ARRAY    ALEPH, BETA[0:2,0:KC11E]  ;                               KPKP0131
              FOR  J ~ 1 STEP 1 UNTIL KC7E  DO                          KPKP0132
              FOR  U ~ 1 STEP 1 UNTIL KC11E  DO                         KPKP0133
         BEGIN                                                          KPKP0134
              ALEPH[J,U] ~ KC8[E,J,U]  ;    BETA[J,U] ~ KC9[E,J,U]      KPKP0135
         END ;                                                          KPKP0136
              PHIPART (KC7[E], DATA, ALEPH, BETA, KC10[E,*], KC11[E,*], KPKP0137
              S, K[I], SUM) ;                                           KPKP0138
              IF CALCPT  THEN  PKT[I] ~ SUM[1] ;                        KPKP0139
              IF CALCPP  THEN  PKP[I] ~ SUM[2]                          KPKP0140
         END                                                            KPKP0141
         END                                                            KPKP0142
         END  ;                                                         KPKP0143
                                                                        KPKP0144
COMMENT       KPOLY VALUES ;                                            KPKP0145
              IF PICKK[3] > 0 THEN                                      KPKP0146
         BEGIN                                                          KPKP0147
              KPOLY (T, PICKK[3], CALCPT, K, PKT) ;                     KPKP0148
              FOR I ~ NF1+1 STEP 1 UNTIL NF2 DO                         KPKP0149
         BEGIN                                                          KPKP0150
              E ~ I - NF1 ;     K[I] ~ K[E] ;                           KPKP0151
              IF CALCPT  THEN  PKT[I] ~ PKT[E]                          KPKP0152
         END                                                            KPKP0153
         END  ;                                                         KPKP0154
                                                                        KPKP0155
COMMENT       KIDEAL VALUES ;                                           KPKP0156
              IF PICKK[2] > 0 THEN                                      KPKP0157
         BEGIN                                                          KPKP0158
              KIDEAL (T, P, PICKK[2], CALCPT, CALCPP, K, PKT, PKP) ;    KPKP0159
              FOR I ~ PICKK[1]+1 STEP 1 UNTIL NF1 DO                    KPKP0160
         BEGIN                                                          KPKP0161
              E ~ I - PICKK[1] ;     K[I] ~ K[E] ;                      KPKP0162
              IF CALCPT  THEN  PKT[I] ~ PKT[E] ;                        KPKP0163
              IF CALCPP  THEN  PKP[I] ~ PKP[E]                          KPKP0164
         END                                                            KPKP0165
         END  ;                                                         KPKP0166
                                                                        KPKP0167
COMMENT       KNGAA VALUES ;                                            KPKP0168
              IF PICKK[1] > 0 THEN                                      KPKP0169
              KNGAA (T, P, PICKK[1], CALCPDC, CALCPT, CALCPP, K, PKT,   KPKP0170
              PKP)                                                      KPKP0171
                                                                        KPKP0172
         END  KPICK ;                                                   KPKP0173
