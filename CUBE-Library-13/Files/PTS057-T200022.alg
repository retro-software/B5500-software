PROCEDURE     GFLASH (Z, PICKK, PICKGF, LIMITGF, CALCPDC, T, P, VDF, X, GFLP0001
              Y, ERRORGF) ;                                             GFLP0002
                                                                        GFLP0003
COMMENT       GENERAL NON-ADIABATIC FLASH PROCEDURE                     GFLP0004
              BY DONALD L. VOSS                                         GFLP0005
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       GFLP0006
              CARD SEQUENCE CODE STARTS WITH GFLP0001.                  GFLP0007
              FIRST RELEASE DATE     11-1-63                            GFLP0008
                                                                        GFLP0009
              THIS PROCEDURE DETERMINES THE PHASE COMPOSITIONS OF       GFLP0010
              VAPOR-LIQUID MIXTURES IN WHICH EQUILIBRIUM EXISTS BETWEEN GFLP0011
              THE LIQUID AND VAPOR.  GIVEN TWO VALUES OF T, P, OR VDF,  GFLP0012
              THIS PROCEDURE WILL CALCULATE THE THIRD VALUE.  AN        GFLP0013
              ASSUMED VALUE OF THE THIRD PARAMETER MUST BE GIVEN.  BY   GFLP0014
              SPECIFYING VDF = 1.0 OR 0, DEW-POINT OR BUBBLE-POINT      GFLP0015
              CALCULATIONS WILL BE MADE, RESPECTIVELY.  THE KPICK       GFLP0016
              PROCEDURE IS USED TO CALCULATE THE K-RATIOS.              GFLP0017
                                                                        GFLP0018
              THE INPUT PARAMETERS ARE                                  GFLP0019
              T    - GIVEN OR ASSUMED TEMPERATURE, F  - REAL            GFLP0020
              P    - GIVEN OR ASSUMED PRESSURE, PSIA  - REAL            GFLP0021
              VDF  - GIVEN OR ASSUMED V/F  - REAL                       GFLP0022
              Z    - MOLE FRACTIONS OF FEED COMPONENTS  - REAL ARRAY    GFLP0023
                   [0:N], WHERE N IS THE TOTAL NUMBER OF COMPONENTS.    GFLP0024
                   THE ORDER OF COMPONENTS MUST BE THE SAME AS THE      GFLP0025
                   PICKK[] ORDER.                                       GFLP0026
              PICKK - NUMBER OF COMPONENTS USING EACH K-RATIO PROCEDURE GFLP0027
                   - INTEGER ARRAY [0:7]                                GFLP0028
                   PICKK[0] - TOTAL NUMBER OF COMPONENTS                GFLP0029
                   PICKK[1] - NO. USING KNGAA                           GFLP0030
                   PICKK[2] - NO. USING KIDEAL                          GFLP0031
                   PICKK[3] - NO. USING KPOLY                           GFLP0032
                   PICKK[4] - NO. USING PHIPART                         GFLP0033
                   PICKK[5] - NO. WHOSE K-RATIOS ARE GIVEN IN KC12[].   GFLP0034
                   PICKK[7] - NO. USING THE INTERPOLATION METHOD.       GFLP0035
              PICKGF - SPECIFIES PROBLEM TYPE  - INTEGER                GFLP0036
                   =1 TO CALCULATE T, GIVEN P AND VDF.                  GFLP0037
                   =2 TO CALCULATE P, GIVEN T AND VDF.                  GFLP0038
                   =3 TO CALCULATE VDF, GIVEN T AND P.                  GFLP0039
              LIMITGF - T AND P LIMITS  -  REAL ARRAY [0:4]             GFLP0040
                   LIMITGF[1] - MINIMUM T, F.                           GFLP0041
                   LIMITGF[2] - MAXIMUM T, F.                           GFLP0042
                   LIMITGF[3] - MINIMUM P, PSIA                         GFLP0043
                   LIMITGF[4] - MAXIMUM P, PSIA                         GFLP0044
              CALCPDC - BOOLEAN.  IF TRUE, CALCULATE THE PRESSURE       GFLP0045
                   DEPENDENT COEFFICIENTS IN THE KNGAA PROCEDURE.  FOR  GFLP0046
                   CALCPDC = FALSE, P OF THE PRESENT ENTRY MUST EQUAL   GFLP0047
                   P OF THE LAST ENTRY TO KNGAA.                        GFLP0048
              IN ADDITION, THE VARIABLES KC1, KC2, KC4, KC5, AND KC7 TO GFLP0049
              KC14 ARE NON-LOCAL TO GFLASH.  THESE VARIABLES ARE        GFLP0050
              DEFINED IN THE KPICK PROCEDURE (PTS-41).                  GFLP0051
                                                                        GFLP0052
              THE OUTPUT PARAMETERS ARE                                 GFLP0053
              T    - GIVEN OR CALCULATED TEMPERATURE, F  - REAL         GFLP0054
              P    - GIVEN OR CALCULATED PRESSURE, PSIA  - REAL         GFLP0055
              VDF  - GIVEN OR CALCULATED V/F  - REAL                    GFLP0056
              X    - COMPONENT LIQUID MOL FRACTIONS  - REAL ARRAY [0:N] GFLP0057
              Y    - COMPONENT VAPOR MOL FRACTIONS  - REAL ARRAY [0:N]  GFLP0058
                                                                        GFLP0059
              THE REFERENCE PARAMETER IS                                GFLP0060
              ERRORGF - REFERENCE STATEMENT LABEL TO WHICH CONTROL IS   GFLP0061
                   CHANGED IF THE LIMITS OF LIMITGF[] ARE EXCEEDED.     GFLP0062
                                                                        GFLP0063
              THE PROCEDURES KNGAA, KIDEAL, KPOLY, PHIPART, ANT1, AND   GFLP0064
                   KPICK MUST BE DECLARED PRIOR TO GFLASH.              GFLP0065
              THE PROCEDURES KNGAA, KIDEAL, KPOLY, PHIPART, AND ANT1    GFLP0066
                   MUST BE DECLARED PRIOR TO KPICK.                     GFLP0067
              THE PROCEDURE ANT1 MUST BE DECLARED PRIOR TO KIDEAL.  ;   GFLP0068
                                                                        GFLP0069
VALUE         PICKGF, CALCPDC ;                                         GFLP0070
REAL          T, P, VDF ;                                               GFLP0071
INTEGER       PICKGF ;                                                  GFLP0072
BOOLEAN       CALCPDC ;                                                 GFLP0073
LABEL         ERRORGF ;                                                 GFLP0074
REAL ARRAY    Z[0], LIMITGF[0], X[0], Y[0] ;                            GFLP0075
INTEGER ARRAY PICKK[0] ;                                                GFLP0076
                                                                        GFLP0077
         BEGIN                                                          GFLP0078
REAL          SUM1, SUM2, LLM, ULM, AVAL, CVAL, ZI, KI1V1, KI1, TOL ;   GFLP0079
INTEGER       I, N ;                                                    GFLP0080
BOOLEAN       CALCT, CALCP, CALCVDF, OLEX, CALCPT, CALCPP ;             GFLP0081
REAL ARRAY    K, PKTP2[0:PICKK[0]] ;                                    GFLP0082
LABEL         GF1, GF2, GF3, GF4, GF5, GF6, GF7, GF8, GF9 ;             GFLP0083
                                                                        GFLP0084
COMMENT       INITIAL CALCULATIONS ;                                    GFLP0085
              CALCPT ~ CALCPP ~ OLEX ~ FALSE ;      N ~ PICKK[0] ;      GFLP0086
              CALCT ~ (PICKGF = 1) ;                                    GFLP0087
              CALCP ~ (PICKGF = 2) ;                                    GFLP0088
              CALCVDF ~ (PICKGF = 3) ;                                  GFLP0089
                                                                        GFLP0090
COMMENT       SET-UP OF VARIABLES FOR T CALCULATION IF CALCT = TRUE,    GFLP0091
              FOR P CALCULATION IF CALCP = TRUE, OR FOR VDF CALCULATION GFLP0092
              IF CALCVDF = TRUE ;                                       GFLP0093
              IF CALCT  THEN                                            GFLP0094
         BEGIN                                                          GFLP0095
              CALCPT ~ TRUE ;                                           GFLP0096
              LLM ~ LIMITGF[1] ;   ULM ~ LIMITGF[2] ;                   GFLP0097
              AVAL ~ T ;     TOL ~ 0.001                                GFLP0098
         END                                                            GFLP0099
              ELSE IF CALCP  THEN                                       GFLP0100
         BEGIN                                                          GFLP0101
              CALCPP ~ TRUE ;                                           GFLP0102
              LLM ~ LIMITGF[3] ;   ULM ~ LIMITGF[4] ;                   GFLP0103
              AVAL ~ P ;     TOL ~ 0.001                                GFLP0104
         END                                                            GFLP0105
              ELSE                                                      GFLP0106
         BEGIN                                                          GFLP0107
              LLM ~ 0 ;   ULM ~ 1.0 ;   AVAL ~ VDF ;   TOL ~ 1.0@-6     GFLP0108
         END  ;                                                         GFLP0109
                                                                        GFLP0110
COMMENT       ITERATE HERE TO CALCULATE T OR P ;                        GFLP0111
GF1:          KPICK (T, P, PICKK, CALCPT, CALCPP, CALCPDC, K, PKTP2,    GFLP0112
              PKTP2) ;                                                  GFLP0113
              IF PICKGF ! 2  THEN  CALCPDC ~ FALSE ;                    GFLP0114
                                                                        GFLP0115
COMMENT       DETERMINE IF TWO PHASES EXIST IN VDF CALCULATION ;        GFLP0116
              IF CALCVDF  THEN                                          GFLP0117
         BEGIN                                                          GFLP0118
              SUM1 ~ SUM2 ~ 0 ;                                         GFLP0119
              FOR I ~ 1 STEP 1 UNTIL N  DO                              GFLP0120
         BEGIN                                                          GFLP0121
              SUM1 ~ Z[I]/K[I] + SUM1 ;                                 GFLP0122
              SUM2 ~ Z[I] | K[I] + SUM2 ;                               GFLP0123
              PKTP2[I] ~ -(K[I] - 1.0)*2                                GFLP0124
         END  ;                                                         GFLP0125
              IF SUM1 > 1.0  THEN  GO TO GF2 ;                          GFLP0126
                                                                        GFLP0127
COMMENT       VAPOR PHASE ONLY ;                                        GFLP0128
              VDF ~ 1.0 ;          GO TO GF9 ;                          GFLP0129
GF2:          IF SUM2 > 1.0  THEN  GO TO GF3 ;                          GFLP0130
                                                                        GFLP0131
COMMENT       LIQUID PHASE ONLY ;                                       GFLP0132
              VDF ~ 0 ;          GO TO GF9                              GFLP0133
         END ;                                                          GFLP0134
                                                                        GFLP0135
COMMENT       ITERATE HERE TO CALCULATE VDF, ALSO CONTINUE T AND P      GFLP0136
              CALCULATION.  NEWTON-RAPHSON METHOD IS USED IN THE        GFLP0137
              CONVERGENCE SCHEME. ;                                     GFLP0138
GF3:          SUM1 ~ SUM2 ~ 0 ;                                         GFLP0139
              FOR I ~ 1 STEP 1 UNTIL N  DO                              GFLP0140
         BEGIN                                                          GFLP0141
              ZI ~ Z[I] ;   KI1 ~ K[I] - 1.0 ;                          GFLP0142
              KI1V1 ~ KI1 | VDF + 1.0 ;                                 GFLP0143
              SUM1 ~ ZI | KI1/KI1V1 + SUM1 ;                            GFLP0144
              SUM2 ~ PKTP2[I] | ZI/KI1V1*2 + SUM2                       GFLP0145
         END  ;                                                         GFLP0146
              CVAL ~ AVAL - SUM1/SUM2 ;                                 GFLP0147
              IF CVAL < LLM  THEN  GO TO GF5 ;                          GFLP0148
              IF CVAL > ULM  THEN  GO TO GF7 ;                          GFLP0149
                                                                        GFLP0150
COMMENT       TEST IF TOLERENCE IS MET.  RECYCLE IF NOT ;               GFLP0151
              IF TOL > ABS(SUM1)  THEN  GO TO GF8 ;                     GFLP0152
              AVAL ~ CVAL ;                                             GFLP0153
GF4:          IF CALCT  THEN  T ~ AVAL                                  GFLP0154
              ELSE IF CALCP  THEN  P ~ AVAL                             GFLP0155
              ELSE                                                      GFLP0156
         BEGIN                                                          GFLP0157
              VDF ~ AVAL ;     GO TO GF3                                GFLP0158
         END  ;                                                         GFLP0159
              GO TO GF1 ;                                               GFLP0160
                                                                        GFLP0161
COMMENT       TEST IF T, P, OR VDF EXCEEDS ITS LIMITS ;                 GFLP0162
GF5:          IF AVAL = LLM  THEN                                       GFLP0163
         BEGIN                                                          GFLP0164
              IF NOT CALCP  THEN  GO TO ERRORGF ;                       GFLP0165
              IF OLEX  THEN  GO TO ERRORGF ;                            GFLP0166
              OLEX ~ TRUE ;     AVAL ~ ULM - 50.0 ;     GO TO GF4       GFLP0167
         END  ;                                                         GFLP0168
GF6:          AVAL ~ LLM ;     GO TO GF4 ;                              GFLP0169
GF7:          IF AVAL = ULM  THEN                                       GFLP0170
         BEGIN                                                          GFLP0171
              IF NOT CALCP  THEN  GO TO ERRORGF ;                       GFLP0172
              IF OLEX  THEN  GO TO ERRORGF ;                            GFLP0173
              OLEX ~ TRUE ;     AVAL ~ LLM + 50.0 ;     GO TO GF4       GFLP0174
         END  ;                                                         GFLP0175
              AVAL ~ ULM ;     GO TO GF4 ;                              GFLP0176
                                                                        GFLP0177
COMMENT       CALCULATE COMPOSITIONS ;                                  GFLP0178
GF8:          IF CALCT  THEN  T ~ CVAL                                  GFLP0179
              ELSE IF CALCP  THEN  P ~ CVAL                             GFLP0180
              ELSE  VDF ~ CVAL ;                                        GFLP0181
GF9:          FOR I ~ 1 STEP 1 UNTIL N  DO                              GFLP0182
         BEGIN                                                          GFLP0183
              IF VDF = 1.0  THEN  X[I] ~ 0                              GFLP0184
              ELSE  X[I] ~ Z[I]/((K[I] - 1.0) | VDF + 1.0) ;            GFLP0185
              IF VDF = 0  THEN  Y[I] ~ 0                                GFLP0186
              ELSE IF VDF = 1.0  THEN  Y[I] ~ Z[I]                      GFLP0187
              ELSE  Y[I] ~ K[I] | X[I]                                  GFLP0188
         END                                                            GFLP0189
                                                                        GFLP0190
         END  GFLASH ;                                                  GFLP0191
