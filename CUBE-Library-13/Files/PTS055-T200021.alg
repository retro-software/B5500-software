PROCEDURE     DEWBUB (Z, PICKK, CALCDEW, CALCT, LIMITDB, CALCPDC, T, P, DBPP0001
              ERRORDB) ;                                                DBPP0002
                                                                        DBPP0003
COMMENT       DEW-POINT AND BUBBLE-POINT PROCEDURE                      DBPP0004
              BY DONALD L. VOSS                                         DBPP0005
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       DBPP0006
              CARD SEQUENCE CODE STARTS WITH DBPP0001.                  DBPP0007
              FIRST RELEASE DATE     11-1-63                            DBPP0008
                                                                        DBPP0009
              THIS PROCEDURE PERFORMS DEW-POINT AND BUBBLE-POINT        DBPP0010
              CALCULATIONS ON MULTICOMPONENT MIXTURES.  EITHER          DBPP0011
              TEMPERATURE OR PRESSURE CAN BE DETERMINED.  THE KPICK     DBPP0012
              PROCEDURE IS USED TO DETERMINE THE VAPOR-LIQUID           DBPP0013
              EQUILIBRIUM RATIOS (Y/X).                                 DBPP0014
                                                                        DBPP0015
              THE INPUT PARAMETERS ARE                                  DBPP0016
              T    - GIVEN OR ASSUMED TEMPERATURE, F  - REAL            DBPP0017
              P    - GIVEN OR ASSUMED PRESSURE, PSIA  - REAL            DBPP0018
              Z    - COMPONENT MOLE FRACTIONS - REAL ARRAY [0:N], WHERE DBPP0019
                   N IS THE TOTAL NUMBER OF COMPONENTS.  THE ORDER OF   DBPP0020
                   COMPONENTS MUST BE THE SAME AS THE PICKK[] ORDER.    DBPP0021
              PICKK - NO. OF COMPONENTS FOR EACH K-RATIO PROCEDURE  -   DBPP0022
                   INTEGER ARRAY [0:7]                                  DBPP0023
                   PICKK[0] - TOTAL NUMBER OF COMPONENTS                DBPP0024
                   PICKK[1] - NO. USING KNGAA                           DBPP0025
                   PICKK[2] - NO. USING KIDEAL                          DBPP0026
                   PICKK[3] - NO. USING KPOLY                           DBPP0027
                   PICKK[4] - NO. USING PHIPART                         DBPP0028
                   PICKK[5] - NO. WHOSE K-RATIOS ARE GIVEN IN KC12[].   DBPP0029
                   PICKK[7] - NO. USING THE INTERPOLATION METHOD.       DBPP0030
              CALCDEW - BOOLEAN.  IF TRUE, CALCULATE FOR A DEW-POINT    DBPP0031
                   CONDITION.  IF FALSE, CALCULATE FOR A BUBBLE-POINT   DBPP0032
                   CONDITION.                                           DBPP0033
              CALCT - BOOLEAN.  IF TRUE, CALCULATE T.  IF FALSE,        DBPP0034
                   CALCULATE P.                                         DBPP0035
              LIMITDB - T AND P LIMITS  - REAL ARRAY [0:4]              DBPP0036
                   LIMITDB[1] - MINIMUM T, F.                           DBPP0037
                   LIMITDB[2] - MAXIMUM T, F.                           DBPP0038
                   LIMITDB[3] - MINIMUM P, PSIA                         DBPP0039
                   LIMITDB[4] - MAXIMUM P, PSIA                         DBPP0040
              CALCPDC - BOOLEAN.  IF TRUE, CALCULATE THE PRESSURE       DBPP0041
                   DEPENDENT COEFFICIENTS IN THE KNGAA PROCEDURE.  FOR  DBPP0042
                   CALCPDC = FALSE, P OF PRESENT ENTRY MUST EQUAL P OF  DBPP0043
                   LAST ENTRY TO THE KNGAA PROCEDURE.                   DBPP0044
                                                                        DBPP0045
              IN ADDITION, THE VARIABLES KC1, KC2, KC4, KC5, AND KC7 TO DBPP0046
              KC14 ARE NON-LOCAL TO DEWBUB.  THESE VARIABLES ARE        DBPP0047
              DEFINED IN THE KPICK PROCEDURE (PTS-41).                  DBPP0048
                                                                        DBPP0049
              THE OUTPUT PARAMETERS ARE                                 DBPP0050
              T    - GIVEN OR CALCULATED TEMPERATURE, F.                DBPP0051
              P    - GIVEN OR CALCULATED PRESSURE, PSIA                 DBPP0052
                                                                        DBPP0053
              THE REFERENCE PARAMETER IS                                DBPP0054
              ERRORDB - REFERENCE STATEMENT LABEL TO WHICH CONTROL IS   DBPP0055
                   CHANGED IF THE CALCULATED T OR P VIOLATE THE LIMITS  DBPP0056
                   OF LIMITDB[].                                        DBPP0057
                                                                        DBPP0058
              THE PROCEDURES KNGAA, KIDEAL, KPOLY, PHIPART, ANT1, AND   DBPP0059
                   KPICK MUST BE DECLARED PRIOR TO DEWBUB.              DBPP0060
              THE PROCEDURES KNGAA, KIDEAL, KPOLY, PHIPART, AND ANT1    DBPP0061
                   MUST BE DECLARED PRIOR TO KPICK.                     DBPP0062
              THE PROCEDURE ANT1 MUST BE DECLARED PRIOR TO KIDEAL. ;    DBPP0063
                                                                        DBPP0064
VALUE         CALCDEW, CALCT, CALCPDC ;                                 DBPP0065
REAL          T, P ;                                                    DBPP0066
BOOLEAN       CALCDEW, CALCT, CALCPDC ;                                 DBPP0067
INTEGER ARRAY PICKK[0] ;                                                DBPP0068
REAL ARRAY    Z[0], LIMITDB[0] ;                                        DBPP0069
LABEL         ERRORDB ;                                                 DBPP0070
                                                                        DBPP0071
         BEGIN                                                          DBPP0072
REAL          TRS1, TRS2, TRS3, TRS4, TRS5, TRS6, TRS7, TRS8 ;          DBPP0073
INTEGER       I, N ;                                                    DBPP0074
BOOLEAN       TBS1, CALCPT, CALCPP ;                                    DBPP0075
REAL ARRAY    K, TR1S1[0:PICKK[0]] ;                                    DBPP0076
LABEL         D1, D2, D3, D4, D5, D6 ;                                  DBPP0077
                                                                        DBPP0078
              CALCPT ~ CALCPP ~ TBS1 ~ FALSE ;      N ~ PICKK[0] ;      DBPP0079
                                                                        DBPP0080
COMMENT       SET-UP OF VARIABLES FOR T CALCULATION IF CALCT = TRUE OR  DBPP0081
              FOR P CALCULATION IF CALCP = TRUE ;                       DBPP0082
              IF CALCT THEN                                             DBPP0083
         BEGIN                                                          DBPP0084
              TRS3 ~ LIMITDB[1] ;     TRS4 ~ LIMITDB[2] ;               DBPP0085
              TRS5 ~ T ;     CALCPT ~ TRUE                              DBPP0086
         END                                                            DBPP0087
              ELSE                                                      DBPP0088
         BEGIN                                                          DBPP0089
              TRS3 ~ LIMITDB[3] ;     TRS4 ~ LIMITDB[4] ;               DBPP0090
              TRS5 ~ P ;     CALCPP ~ TRUE                              DBPP0091
         END  ;                                                         DBPP0092
                                                                        DBPP0093
COMMENT       ITERATE HERE TO CALCULATE T OR P.  ENTER THE KPICK        DBPP0094
              PROCEDURE TO CALCULATE THE K-RATIOS. ;                    DBPP0095
D1:           KPICK (T, P, PICKK, CALCPT, CALCPP, CALCPDC, K, TR1S1,    DBPP0096
              TR1S1) ;                                                  DBPP0097
              IF CALCT THEN CALCPDC ~ FALSE ;                           DBPP0098
              TRS6 ~ TRS7 ~ 0 ;                                         DBPP0099
              FOR I ~ 1 STEP 1 UNTIL N DO                               DBPP0100
         BEGIN                                                          DBPP0101
                                                                        DBPP0102
COMMENT       CALCULATIONS FOR DEW-POINT IF CALCDEW = TRUE OR FOR       DBPP0103
              BUBBLE-POINT IF CALCDEW = FALSE ;                         DBPP0104
              IF CALCDEW THEN                                           DBPP0105
         BEGIN                                                          DBPP0106
              TRS1 ~ Z[I]/K[I] ;     TRS2 ~ -TR1S1[I] | TRS1/K[I]       DBPP0107
         END                                                            DBPP0108
              ELSE                                                      DBPP0109
         BEGIN                                                          DBPP0110
              TRS1 ~ K[I] | Z[I] ;     TRS2 ~ Z[I] | TR1S1[I]           DBPP0111
         END  ;                                                         DBPP0112
              TRS6 ~ TRS6 + TRS1 ;     TRS7 ~ TRS7 + TRS2               DBPP0113
         END  ;                                                         DBPP0114
                                                                        DBPP0115
COMMENT       NEWTON-RAPHSON METHOD IS USED IN CONVERGENCE SCHEME. ;    DBPP0116
              TRS6 ~ TRS6 - 1.0 ;     TRS8 ~ -TRS6/TRS7 + TRS5 ;        DBPP0117
              IF TRS8 < TRS3 THEN  GO TO D3 ;                           DBPP0118
              IF TRS8 > TRS4 THEN  GO TO D5 ;                           DBPP0119
                                                                        DBPP0120
COMMENT       TEST IF TOLERANCE IS MET.  RECYCLE IF NOT. ;              DBPP0121
              TRS5 ~ TRS8 ;                                             DBPP0122
              IF 0.001 > ABS(TRS6) THEN                                 DBPP0123
         BEGIN                                                          DBPP0124
              IF CALCT THEN  T ~ TRS5  ELSE  P ~ TRS5 ;      GO TO D6   DBPP0125
         END  ;                                                         DBPP0126
D2:           IF CALCT  THEN  T ~ TRS5  ELSE  P ~ TRS5 ;     GO TO D1 ; DBPP0127
                                                                        DBPP0128
COMMENT       TEST IF T OR P EXCEED THEIR LIMITS ;                      DBPP0129
D3:           IF TRS5 = TRS3 THEN                                       DBPP0130
         BEGIN                                                          DBPP0131
              IF CALCT THEN  GO TO ERRORDB ;                            DBPP0132
              IF TBS1 THEN  GO TO ERRORDB ;                             DBPP0133
              TBS1 ~ TRUE ;     TRS5 ~ TRS4 - 50.0 ;     GO TO D2       DBPP0134
         END  ;                                                         DBPP0135
D4:           TRS5 ~ TRS3 ;     GO TO D2 ;                              DBPP0136
D5:           IF TRS5 = TRS4 THEN                                       DBPP0137
         BEGIN                                                          DBPP0138
              IF CALCT THEN  GO TO ERRORDB ;                            DBPP0139
              IF TBS1 THEN  GO TO ERRORDB ;                             DBPP0140
              TBS1 ~ TRUE ;     TRS5 ~ TRS3 + 50.0 ;     GO TO D2       DBPP0141
         END  ;                                                         DBPP0142
              TRS5 ~ TRS4 ;      GO TO D2 ;                             DBPP0143
                                                                        DBPP0144
D6:      END  DEWBUB ;                                                  DBPP0145
