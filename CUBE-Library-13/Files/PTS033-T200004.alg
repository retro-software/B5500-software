PROCEDURE     KNGAA (T, P, N, CALCPDC, CALCPT, CALCPP, K, PKT, PKP) ;   KNGP0001

COMMENT       N.G.A.A. K-RATIO PROCEDURE.                               KNGP0002
              BY A. PAUL OLESON AND DONALD L. VOSS                      KNGP0003
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KNGP0004
              FIRST RELEASE DATE:  5-5-63                               KNGP0005

              THIS PROCEDURE CALCULATES VAPOR-LIQUID EQUILIBRIUM RATIOS KNGP0006
              (Y/X) FOR LIGHT HYDROCARBONS BY MEANS OF THE N.G.A.A.     KNGP0007
              CORRELATION.  ALSO CALCULATED, BY OPTION, ARE THE         KNGP0008
              TEMPERATURE AND PRESSURE DERIVATIVES OF THE K-RATIOS.     KNGP0009
              COMPONENTS MAY BE REFERENCED TO ANY COMPONENT IN THE      KNGP0010
              SYSTEM BY K = K[R](A + B | T + C | T*2), WHERE            KNGP0011
                   A = KC1[I,2] | P + KC1[I,3],                         KNGP0012
                   B = KC1[I,4] | P + KC1[I,5],                         KNGP0013
                   C = KC1[I,6] | P + KC1[I,7],                         KNGP0014
              AND T IS DEGREES RANKINE/1000.                            KNGP0015

              THE INPUT PARAMETERS ARE                                  KNGP0016
              T    - TEMPERATURE, F - REAL                              KNGP0017
              P    - PRESSURE, PSIA - REAL                              KNGP0018
              N    - NUMBER OF COMPONENTS - INTEGER                     KNGP0019
              CALCPDC - BOOLEAN.  IF TRUE, CALCULATE P DEPENDENT        KNGP0020
                     COEFFICIENTS.  FOR CALCPDC = FALSE, P OF THE LAST  KNGP0021
                     ENTRY MUST EQUAL P OF THE PRESENT ENTRY.           KNGP0022
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE THE T DERIVATIVE    KNGP0023
                     OF K.                                              KNGP0024
              CALCPP - BOOLEAN.  IF TRUE, CALCULATE THE P DERIVATIVE    KNGP0025
                     OF K.                                              KNGP0026

              ALSO, THE VARIABLE KC1 IS NON-LOCAL.                      KNGP0027
              KC1  - N.G.A.A. COEFFICIENTS - REAL ARRAY [0:N,0:28],     KNGP0028
                     WHERE THE 28 COEFFICIENTS ARE ALTERNATELY 4 SETS   KNGP0029
                     OF 7 P COEFFICIENTS.  IF KC1[I,1] = 0, COMPONENT   KNGP0030
                     I IS REFERENCED TO COMPONENT R AND KC1[I,8] = R.   KNGP0031
                     R IS REAL AND MUST BE GREATER THAN I.              KNGP0032

              THE OUTPUT PARAMETERS ARE                                 KNGP0033
              K    - COMPONENT VAPOR-LIQUID EQUILIBRIUM RATIOS (Y/X) -  KNGP0034
                     REAL ARRAY [0:N]                                   KNGP0035
              PKT  - COMPONENT T DERIVATIVES OF K - REAL ARRAY [0:N]    KNGP0036
              PKP  - COMPONENT P DERIVATIVES OF K - REAL ARRAY [0:N] ;  KNGP0037

VALUE         T, P, N, CALCPDC, CALCPT, CALCPP ;                        KNGP0038
INTEGER       N ;                                                       KNGP0039
BOOLEAN       CALCPDC, CALCPT, CALCPP ;                                 KNGP0040
REAL          T, P ;                                                    KNGP0041
ARRAY         K[0], PKT[0], PKP[0] ;                                    KNGP0042

         BEGIN                                                          KNGP0043
LABEL         KNG1, KNG2, KNG3, KNG4, KNG5, KNG6, KNG7, KNG8, KNG9 ;    KNGP0044
INTEGER       I, J, U, E, G, TIS1, Q ;                                  KNGP0045
REAL          TRS1, TR1, TR12 ;                                         KNGP0046
OWN REAL      LOGP ;                                                    KNGP0047
ARRAY         TR1S1[0:4] ;                                              KNGP0048
OWN ARRAY     PDC[0:N,0:4], LOGP1[0:7] ;                                KNGP0049

COMMENT       CALCULATE PRESSURE DEPENDENT COEFFICIENTS IF CALCPDC = 1 ;KNGP0050
              IF CALCPDC THEN                                           KNGP0051
         BEGIN                                                          KNGP0052
              LOGP ~ LN(P) | 0.1 ;      LOGP1[1] ~ 1.0 ;                KNGP0053
              FOR U ~ 2 STEP 1 UNTIL 7 DO                               KNGP0054
              LOGP1[U] ~ LOGP1[U-1] | LOGP ;                            KNGP0055
              LOGP ~ 10.0 | LOGP ;                                      KNGP0056
              FOR I ~ 1 STEP 1 UNTIL N DO                               KNGP0057
         BEGIN                                                          KNGP0058
              IF KC1[I,1] = 0 THEN GO TO KNG1 ;                         KNGP0059
              FOR J ~ 1, 8, 15, 22 DO                                   KNGP0060
         BEGIN                                                          KNGP0061
              Q ~ J + 6 ;                                               KNGP0062
              E ~ Q DIV 7 ;                                             KNGP0063
              PDC[I,E] ~ 0 ;                                            KNGP0064
              TIS1 ~ 1 - J ;                                            KNGP0065
              FOR U ~ J STEP 1 UNTIL Q DO                               KNGP0066
         BEGIN                                                          KNGP0067
              G ~ U + TIS1 ;                                            KNGP0068
              PDC[I,E] ~ KC1[I,U] | LOGP1[G] + PDC[I,E]                 KNGP0069
         END                                                            KNGP0070
         END  ;                                                         KNGP0071
              GO TO KNG2 ;                                              KNGP0072

COMMENT       CALCULATE PRESSURE DEPENDENT COEFFICIENTS OF REFERENCED   KNGP0073
              COMPONENTS ;                                              KNGP0074
KNG1:         PDC[I,1] ~ 0 ;                                            KNGP0075
              PDC[I,2] ~ KC1[I,2] | P + KC1[I,3] ;                      KNGP0076
              PDC[I,3] ~ KC1[I,4] | P + KC1[I,5] ;                      KNGP0077
              PDC[I,4] ~ KC1[I,6] | P + KC1[I,7] ;                      KNGP0078
KNG2:    END                                                            KNGP0079
         END  ;                                                         KNGP0080

COMMENT       CALCULATE K-RATIOS ;                                      KNGP0081
KNG3:         TR1 ~ (T + 459.7) | 0.001 ;  TR12 ~ TR1*2 ;               KNGP0082
              FOR I ~ N STEP -1 UNTIL 1 DO                              KNGP0083
         BEGIN                                                          KNGP0084
              IF PDC[I,1] = 0 THEN GO TO KNG4 ;                         KNGP0085
              K[I] ~ EXP (PDC[I,4] | TR12 + PDC[I,3] | TR1 +            KNGP0086
              PDC[I,1]/TR1 + PDC[I,2] - LOGP) ;                         KNGP0087
              GO TO KNG5 ;                                              KNGP0088

COMMENT       CALCULATE K-RATIOS OF REFERENCED COMPONENTS ;             KNGP0089
KNG4:         K[I] ~ K[KC1[I,8]] | (PDC[I,3] | TR1 + PDC[I,2] +         KNGP0090
              PDC[I,4] | TR12) ;                                        KNGP0091
KNG5:    END  ;                                                         KNGP0092

COMMENT       CALCULATE TEMPERATURE DERIVATIVES OF K-RATIOS, IF CALCPT  KNGP0093
              IS TRUE ;                                                 KNGP0094
              IF CALCPT THEN                                            KNGP0095
              FOR I ~ N STEP -1 UNTIL 1 DO                              KNGP0096
         BEGIN                                                          KNGP0097
              IF PDC[I,1] = 0 THEN GO TO KNG6 ;                         KNGP0098
              PKT[I] ~ (-PDC[I,1]/TR12 + 2.0 | PDC[I,4] | TR1 +         KNGP0099
              PDC[I,3] ) | 0.001 | K[I] ;                               KNGP0100
              GO TO KNG7 ;                                              KNGP0101

COMMENT       CALCULATE TEMPERATURE DERIVATIVES OF K-RATIOS FOR         KNGP0102
              REFERENCED COMPONENTS ;                                   KNGP0103
KNG6:         E ~ KC1[I,8] ;                                            KNGP0104
              PKT[I] ~ K[I]/K[E] | PKT[E] + (PDC[I,4] | 2.0 | TR1 +     KNGP0105
              PDC[I,3] ) | K[E] | 0.001 ;                               KNGP0106
KNG7:    END  ;                                                         KNGP0107

COMMENT       CALCULATE PRESSURE DERIVATIVES OF K-RATIOS, IF CALCPP IS  KNGP0108
              TRUE ;                                                    KNGP0109
              IF CALCPP THEN                                            KNGP0110
              FOR I ~ N STEP -1 UNTIL 1 DO                              KNGP0111
         BEGIN                                                          KNGP0112
              IF PDC[I,1] = 0 THEN GO TO KNG8 ;                         KNGP0113
              FOR J ~ 1, 8, 15, 22 DO                                   KNGP0114
         BEGIN                                                          KNGP0115
              Q ~ J + 5 ;                                               KNGP0116
              E ~ (J + 6) DIV 7 ;                                       KNGP0117
              TR1S1[E] ~ 0 ;  TRS1 ~ 1.0 ;                              KNGP0118
              TIS1 ~ 1 - J ;                                            KNGP0119
              FOR U ~ J STEP 1 UNTIL Q DO                               KNGP0120
         BEGIN                                                          KNGP0121
              G ~ U + TIS1 ;                                            KNGP0122
              TR1S1[E] ~ KC1[I,U+1] | LOGP1[G] | TRS1 + TR1S1[E] ;      KNGP0123
              TRS1 ~ TRS1 + 1.0                                         KNGP0124
         END                                                            KNGP0125
         END  ;                                                         KNGP0126
              PKP[I] ~ (TR1S1[4] | TR12 + TR1S1[3] | TR1 + TR1S1[1]/TR1 KNGP0127
              + TR1S1[2] - 10.0) | 0.1 | K[I]/P ;                       KNGP0128
              GO TO KNG9 ;                                              KNGP0129

COMMENT       CALCULATE PRESSURE DERIVATIVES OF K-RATIOS FOR REFERENCED KNGP0130
              COMPONENTS ;                                              KNGP0131
KNG8:         E ~ KC1[I,8] ;                                            KNGP0132
              PKP[I] ~ (KC1[I,6] | TR12 + KC1[I,4] | TR1 + KC1[I,2] ) | KNGP0133
              K[E] + (PDC[I,4] | TR12 + PDC[I,3] | TR1 + PDC[I,2] ) |   KNGP0134
              PKP[E] ;                                                  KNGP0135
KNG9:    END                                                            KNGP0136

         END  KNGAA ;                                                   KNGP0137
