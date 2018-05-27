 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                          TEST   1
                                                                        TEST   2
COMMENT       TEST PROGRAM FOR FLASHW PROCEDURE.                        TEST   3
              BY A. PAUL OLESON                                         TEST   4
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORP.              TEST   5
              CARD SEQUENCE STARTS WITH TEST0001.                       TEST   6
              DATE OF FIRST ISSUE 12-1-63 ;                             TEST   7
FILE IN       FLIN (1,10) ;                                             TEST   8
FILE OUT      FIL1 (1,15) ;                                             TEST   9
BOOLEAN       CALCPDC ;                                                 TEST  10
BOOLEAN ARRAY B[0:10] ;                                                 TEST  11
INTEGER       I, J, N ;                                                 TEST  12
INTEGER ARRAY PICKK[0:8], KC7[0:20], KC11[0:20,0:2]  ;                  TEST  13
REAL          T, P, VDF, S ;                                            TEST  14
REAL ARRAY    KC1[0:20,0:28], KC2[0:20,0:3], KC3[0:3], KC4[0:5],        TEST  15
              KC5[0:2,0:3], KC6[0:3],KC8[0:20,0:2,0:2],                 TEST  16
              KC9[0:20,0:2,0:2], KC10[0:20,0:5], KC12[0:20], K[0:20],   TEST  17
              PK[0:20], Z[0:20], X[0:20], Y[0:20], LIMITFW[0:4] ;       TEST  18
                                                                        TEST  19
LABEL         ENDPROB, ERRORFW ;                                        TEST  20
FORMAT IN     F2 (E17.8,3E16.8) ,                                       TEST  21
              F3 (X4,2F6.1,F4.1,3F6.3,4I2) ;                            TEST  22
                                                                        TEST  23
FORMAT OUT    FO1 (2F9.5,2E16.8) ,                                      TEST  24
              FO2 (3E16.8) ;                                            TEST  25
                                                                        TEST  26
LIST          LI1(FOR I ~ 1,2 DO FOR J ~ 1 STEP 1 UNTIL 28 DO           TEST  27
              KC1[I,J]),                                                TEST  28
              LI2(T, P, VDF, Z[1], Z[2], Z[3], B[1], B[2], B[3], B[4]), TEST  29
              LI3(T, P, VDF, S) ,                                       TEST  30
              L4 (FOR I ~ 1,2,3 DO [Z[I], X[I], Y[I]]) ;                TEST  31
REAL          LOGP ;                                                    TEST  32
COMMENT       GLOBAL ARRAYS FOR KNGAA PROCEDURE ;                       TEST  33
ARRAY         TR1S1[0:4], PDC[0:8,0:4], LOGP1[0:7] ;                    TEST  34
COMMENT       GLOBAL ARRAYS FOR PHIPART PROCEDURE ;                     TEST  35
INTEGER ARRAY TADD, ADD[0:5], DEGREE[0:4] ;                             TEST  36
ARRAY         POLY[0:25], TT[0:4] ;                                     TEST  37
                                                                        TEST  38
PROCEDURE     KNGAA (T, P, N, CALCPDC, CALCPT, CALCPP, K, PKT, PKP) ;   KNGP001

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
PROCEDURE     PHIPART (V, DATA, ALEPH, BETA, COEFF, P, S, PHI, SUM) ;   SFSB   1
COMMENT       THIS PROCEDURE COMPUTES POINTS ON AN N-DIMENSIONAL SURFACESFSB   2
              AND, BY OPTION, THE PARTIAL DERIVATIVES OF ANY OF THE     SFSB   3
              VARIABLES AT THE POINTS IN QUESTION ;                     SFSB   4
VALUE         V ;                                                       SFSB   5
INTEGER       V ;                                                       SFSB   6
INTEGER ARRAY P[0] ;                                                    SFSB   7
BOOLEAN ARRAY S[0] ;                                                    SFSB   8
REAL ARRAY    DATA[0], ALEPH[0,0], BETA[0,0], COEFF[0], SUM[0] ;        SFSB   9
REAL          PHI ;                                                     SFSB  10
         BEGIN                                                          SFSB  11
INTEGER       KV, M, T, I, D, PV ;                                      SFSB  12
BOOLEAN       B ;                                                       SFSB  14
REAL          DATUM ;                                                   SFSB  15
COMMENT       THE SIZE OF POLY IS SUM(P[I]+1), I ~ 1 STEP 1 UNTIL V. IT SFSB  17
              HAS BEEN SET TO A VALUE SO AS NOT TO REQUIRE CALCULATION  SFSB  18
              WITH EACH PASS THROUGH PHIPART, BUT IT MAY BE ALTERED. ;  SFSB  19
LABEL         POLYCOMPUTE, PARTCOMPUTE, D2, BACK, HERE, FINI, DEPENDENT;SFSB  20
              TADD[1] ~ ADD[1] ~ T ~ 1 ;                                SFSB  21
              FOR M ~ 1 STEP 1 UNTIL V DO                               SFSB  22
         BEGIN                                                          SFSB  23
              DATUM ~ DATA[M] ;          SUM[M] ~ 0 ;                   SFSB  24
COMMENT       EVALUATE THE POLYNOMIALS ;                                SFSB  25
POLYCOMPUTE :                                                           SFSB  26
         BEGIN                                                          SFSB  27
              POLY[T] ~ 1.0 ;                                           SFSB  28
              POLY[T+1] ~ DATUM + ALEPH[M,1] ;          T ~ T + 2 ;     SFSB  29
              FOR I ~ 2 STEP 1 UNTIL P[M] DO                            SFSB  30
         BEGIN                                                          SFSB  31
              POLY[T] ~ (ALEPH[M,I] + DATUM) | POLY[T-1] +              SFSB  32
              BETA[M,I] | POLY[T-2] ;                                   SFSB  33
              T ~ T + 1                                                 SFSB  34
         END                                                            SFSB  35
         END POLYCOMPUTE ;                                              SFSB  36
              TADD[M+1] ~ ADD[M+1] ~ T                                  SFSB  37
         END ;                                                          SFSB  38
              B ~ FALSE ;                                               SFSB  39
COMMENT       COMPUTE THE DEPENDENT VARIABLE OR THE PARTIAL ;           SFSB  40
DEPENDENT :                                                             SFSB  41
         BEGIN                                                          SFSB  42
              D ~ 1 ;                                                   SFSB  43
              FOR M ~ 1 STEP 1 UNTIL V DO                               SFSB  44
         BEGIN                                                          SFSB  45
              DEGREE[M] ~ TADD[M] ;          TT[M] ~ 0                  SFSB  46
         END ;                                                          SFSB  47
              PV ~ TADD[V] + P[V] ;                                     SFSB  48
BACK :        FOR I ~ TADD[V] STEP 1 UNTIL PV DO                        SFSB  49
         BEGIN                                                          SFSB  50
              TT[V] ~ COEFF[D] | POLY[I] + TT[V] ;                      SFSB  51
              D ~ D + 1                                                 SFSB  52
         END ;                                                          SFSB  53
              IF V = 1 THEN GO TO FINI ;                                SFSB  54
              M ~ V - 1 ;                                               SFSB  55
HERE :        TT[M] ~ POLY[DEGREE[M]] | TT[M+1] + TT[M] ;               SFSB  56
              TT[M+1] ~ 0 ;                                             SFSB  57
              DEGREE[M] ~ DEGREE[M] + 1 ;                               SFSB  58
              IF DEGREE[M] { TADD[M] + P[M] THEN GO TO BACK ;           SFSB  60
              IF M = 1 THEN GO TO FINI ;                                SFSB  61
              DEGREE[M] ~ TADD[M] ;          M ~ M - 1 ;                SFSB  62
              GO TO HERE ;                                              SFSB  63
FINI :        IF B THEN                                                 SFSB  64
         BEGIN                                                          SFSB  65
              SUM[KV] ~ TT[1] ;          GO TO D2                       SFSB  66
         END ;                                                          SFSB  67
              PHI ~ TT[1]                                               SFSB  68
         END DEPENDENT ;                                                SFSB  69
              FOR KV ~ 1 STEP 1 UNTIL V DO                              SFSB  70
         BEGIN                                                          SFSB  71
              M ~ KV ;                                                  SFSB  72
              IF S[M] THEN                                              SFSB  73
         BEGIN                                                          SFSB  74
              DATUM ~ DATA[M] ;                                         SFSB  75
COMMENT       EVALUATE THE PARTIAL POLYNOMIALS ;                        SFSB  76
PARTCOMPUTE :                                                           SFSB  77
         BEGIN                                                          SFSB  78
              POLY[T] ~ 0 ;          POLY[T+1] ~ 1.0 ;                  SFSB  79
              T ~ T + 2 ;                                               SFSB  80
              FOR I ~ 2 STEP 1 UNTIL P[M] DO                            SFSB  81
         BEGIN                                                          SFSB  82
              POLY[T] ~ (ALEPH[M,I] + DATUM) | POLY[T-1] +              SFSB  83
              POLY[ADD[M]+I-1] + BETA[M,I] | POLY[T-2] ;                SFSB  84
              T ~ T + 1                                                 SFSB  85
         END                                                            SFSB  86
         END PARTCOMPUTE ;                                              SFSB  87
              T ~ TADD[M] ~ ADD[V+1] ;          B ~ TRUE ;              SFSB  88
              GO TO DEPENDENT ;                                         SFSB  89
D2 :          TADD[KV] ~ ADD[KV]                                        SFSB  90
         END                                                            SFSB  91
         END                                                            SFSB  92
         END PHIPART ;                                                  SFSB  93
PROCEDURE     ANT1 (X, CALCPX, ANTC, ANT, PANTX) ;                      ANTP   1

COMMENT       ANTOINE EQUATION PROCEDURE.                               ANTP   2
              BY DONALD L. VOSS                                         ANTP   3
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       ANTP   4
              FIRST RELEASE DATE:  11-1-62                              ANTP   5

              THIS PROCEDURE EVALUATES THE 3-CONSTANT ANTOINE EQUATION. ANTP   6

              THE INPUT PARAMETERS ARE                                  ANTP   7
              X    - INDEPENDENT VARIABLE - REAL                        ANTP   8
              CALCPX - BOOLEAN.  IF TRUE, CALCULATE PANTX.              ANTP   9
              ANTC - ANTOINE EQUATION COEFFICIENTS - REAL ARRAY [0:3]   ANTP  10

              THE OUTPUT PARAMETERS ARE                                 ANTP  11
              ANT  - VALUE OF THE ANTOINE EQUATION AT X - REAL          ANTP  12
              PANTX - X DERIVATIVE OF ANT - REAL ;                      ANTP  13

VALUE         X, CALCPX ;                                               ANTP  14
BOOLEAN       CALCPX ;                                                  ANTP  15
REAL          X, ANT, PANTX ;                                           ANTP  16
ARRAY         ANTC[0] ;                                                 ANTP  17

         BEGIN                                                          ANTP  18
REAL          TRS1 ;                                                    ANTP  19

COMMENT       EVALUATE THE ANTOINE EQUATION ;                           ANTP  20
              TRS1 ~ ANTC[3] + X ;                                      ANTP  21
              ANT ~ EXP (-ANTC[2]/TRS1 + ANTC[1] ) ;                    ANTP  22

COMMENT       IF CALCPX IS TRUE, CALCULATE THE DERIVATIVE OF THE        ANTP  23
              ANTOINE EQUATION WITH RESPECT TO X. ;                     ANTP  24
              IF CALCPX THEN                                            ANTP  25
              PANTX ~ ANTC[2]/TRS1*2 | ANT                              ANTP  26
         END  ANT1 ;                                                    ANTP  27
PROCEDURE     KIDEAL (T, P, N, CALCPT, CALCPP, K, PKT, PKP) ;           KIDP0001

COMMENT       IDEAL K-RATIO PROCEDURE.                                  KIDP0002
              BY DONALD L. VOSS                                         KIDP0003
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KIDP0004
              FIRST RELEASE DATE:  5-5-63                               KIDP0005

              THIS PROCEDURE CALCULATES VAPOR-LIQUID EQUILIBRIUM RATIOS KIDP0006
              (Y/X), AND THEIR DERIVATIVES WITH RESPECT TO TEMPERATURE  KIDP0007
              AND PRESSURE.  IDEAL LIQUID AND VAPOR BEHAVIOR IS ASSUMED.KIDP0008

              THE INPUT PARAMETERS ARE                                  KIDP0009
              T    - TEMPERATURE, F - REAL                              KIDP0010
              P    - PRESSURE, PSIA - REAL                              KIDP0011
              N    - NUMBER OF COMPONENTS - INTEGER                     KIDP0012
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKT.                KIDP0013
              CALCPP - BOOLEAN.  IF TRUE, CALCULATE PKP.                KIDP0014

              ALSO, THE VARIABLE KC2 IS NON-LOCAL.                      KIDP0015
              KC2  - COMPONENT VAPOR PRESSURE COEFFICIENTS - REAL ARRAY KIDP0016
                     [0:N,0:3].  KC2 IS USED AS ANTC IN THE ANT1        KIDP0017
                     PROCEDURE.                                         KIDP0018

              THE OUTPUT PARAMETERS ARE                                 KIDP0019
              K    - COMPONENT VAPOR-LIQUID EQUILIBRIUM RATIOS (Y/X) -  KIDP0020
                     REAL ARRAY [0:N]                                   KIDP0021
              PKT  - COMPONENT T DERIVATIVES OF K - REAL ARRAY [0:N]    KIDP0022
              PKP  - COMPONENT P DERIVATIVES OF K - REAL ARRAY [0:N]    KIDP0023

              THE PROCEDURE ANT1 MUST BE DECLARED PRIOR TO KIDEAL. ;    KIDP0024

VALUE         T, P, N, CALCPT, CALCPP ;                                 KIDP0025
INTEGER       N ;                                                       KIDP0026
BOOLEAN       CALCPT, CALCPP ;                                          KIDP0027
REAL          T, P ;                                                    KIDP0028
ARRAY         K[0], PKT[0], PKP[0] ;                                    KIDP0029

         BEGIN                                                          KIDP0030
INTEGER       I ;                                                       KIDP0031
REAL          TRS1 ;                                                    KIDP0032

              TRS1 ~ 1.0/P ;                                            KIDP0033
              FOR  I ~ 1 STEP 1 UNTIL N DO                              KIDP0034
         BEGIN                                                          KIDP0035

COMMENT       ENTER THE ANT1 PROCEDURE TO CALCULATE THE VAPOR PRESSURE  KIDP0036
              AND ITS TEMPERATURE DERIVATIVE FOR EACH COMPONENT. ;      KIDP0037
              ANT1 (T, CALCPT, KC2[I,*], K[I], PKT[I] ) ;               KIDP0038

COMMENT       CALCULATE THE COMPONENT K-RATIOS. ;                       KIDP0039
              K[I] ~ K[I] | TRS1 ;                                      KIDP0040

COMMENT       IF CALCPT IS TRUE, CALCULATE THE COMPONENT T DERIVATIVES  KIDP0041
              OF THE K-RATIOS. ;                                        KIDP0042
              IF CALCPT THEN                                            KIDP0043
              PKT[I] ~ PKT[I] | TRS1 ;                                  KIDP0044

COMMENT       IF CALCPP IS TRUE, CALCULATE THE COMPONENT P DERIVATIVES  KIDP0045
              OF THE K-RATIOS. ;                                        KIDP0046
              IF CALCPP THEN                                            KIDP0047
              PKP[I] ~ -K[I] | TRS1                                     KIDP0048
         END                                                            KIDP0049

         END  KIDEAL ;                                                  KIDP0050
PROCEDURE     KPOLY (T, N, CALCPT, K, PKT) ;                            KPYP0001

COMMENT       RELATIVE VOLATILITY K-RATIO PROCEDURE.                    KPYP0002
              BY DONALD L. VOSS                                         KPYP0003
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KPYP0004
              CARD SEQUENCE CODE STARTS WITH KPYP0001                   KPYP0005
              FIRST RELEASE DATE:  5-5-63                               KPYP0006

              THIS PROCEDURE CALCULATES VAPOR-LIQUID EQUILIBRIUM        KPYP0007
              K-RATIOS (Y/X) AND THEIR DERIVATIVES WITH RESPECT TO      KPYP0008
              TEMPERATURE BY MEANS OF A RELATIVE VOLATILITY EQUATION.   KPYP0009
              THE REFERENCE COMPONENT K-RATIO IS CALCULATED BY MEANS    KPYP0010
              OF A FOURTH DEGREE POLYNOMIAL IN TEMPERATURE.  A SECOND   KPYP0011
              DEGREE POLYNOMIAL IS USED FOR THE RELATIVE VOLATILITIES   KPYP0012
              OF THE OTHER COMPONENTS.                                  KPYP0013

              THE INPUT PARAMETERS ARE                                  KPYP0014
              T    - TEMPERATURE, F.  - REAL                            KPYP0015
              N    - NUMBER OF COMPONENTS  - INTEGER                    KPYP0016
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKT.                KPYP0017

              IN ADDITION, THE VARIABLES KC4 AND KC5 ARE NON-LOCAL TO   KPYP0018
              KPOLY.                                                    KPYP0019
              KC4  - REFERENCE COMPONENT K-RATIO COEFFICIENTS  - REAL   KPYP0020
                   ARRAY [0:5]                                          KPYP0021
              KC5  - RELATIVE VOLATILITY COEFFICIENTS OF COMPONENTS  -  KPYP0022
                   REAL ARRAY [0:N,0:3]                                 KPYP0023

              THE OUTPUT PARAMETERS ARE                                 KPYP0024
              K    - COMPONENT K-RATIO  - REAL ARRAY [0:N]              KPYP0025
              PKT  - T DERIVATIVE OF K  - REAL ARRAY [0:N]  ;           KPYP0026

VALUE         T, N, CALCPT ;                                            KPYP0027
INTEGER       N ;                                                       KPYP0028
BOOLEAN       CALCPT ;                                                  KPYP0029
REAL          T ;                                                       KPYP0030
ARRAY         K[0], PKT[0] ;                                            KPYP0031

         BEGIN                                                          KPYP0032
INTEGER       I ;                                                       KPYP0033
REAL          TRS1, KR, PKRT1, RV ;                                     KPYP0034

COMMENT       CALCULATE THE K-RATIO OF THE REFERENCE COMPONENT, KR.  ;  KPYP0035
              TRS1 ~ 0.01 | T ;                                         KPYP0036
              KR ~ (((KC4[5] | TRS1 + KC4[4]) | TRS1 + KC4[3]) | TRS1 + KPYP0037
              KC4[2]) | TRS1 + KC4[1] ;                                 KPYP0038

COMMENT       CALCULATE THE TEMPERATURE DERIVATIVE OF KR, MULTIPLIED BY KPYP0039
              100.0, IF CALCPT IS TRUE.  ;                              KPYP0040
              IF CALCPT THEN                                            KPYP0041
              PKRT1 ~ ((KC4[5] | TRS1 | 4.0 + KC4[4] | 3.0) | TRS1 +    KPYP0042
              KC4[3] | 2.0) | TRS1 + KC4[2] ;                           KPYP0043

COMMENT       CALCULATE THE COMPONENT K-RATIOS AND, IF CALCPT IS TRUE,  KPYP0044
              THEIR TEMPERATURE DERIVATIVES.  ;                         KPYP0045
              FOR I ~ 1 STEP 1 UNTIL N DO                               KPYP0046
         BEGIN                                                          KPYP0047
              RV ~ (KC5[I,3] | TRS1 + KC5[I,2]) | TRS1 + KC5[I,1] ;     KPYP0048
              K[I] ~ KR | RV ;                                          KPYP0049
              IF CALCPT THEN                                            KPYP0050
              PKT[I] ~ ((KC5[I,3] | TRS1 | 2.0 + KC5[I,2]) | KR +       KPYP0051
              RV | PKRT1) | 0.01                                        KPYP0052
         END                                                            KPYP0053

         END  KPOLY ;                                                   KPYP0054
PROCEDURE     KWATER (T, P, CALCPT, CALCPP, KW, PVW, XW, PKWT, PKWP,    KWRP0001
              PPVWT, PXWT) ;                                            KWRP0002

COMMENT       PROCEDURE FOR K-RATIO OF WATER DISSOLVED IN HYDROCARBONS. KWRP0003
              BY A. PAUL OLESON AND DONALD L. VOSS                      KWRP0004
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KWRP0005
              FIRST RELEASE DATE:  5-5-63                               KWRP0006

              THIS PROCEDURE CALCULATES THE FOLLOWING QUANTITIES FOR    KWRP0007
              WATER: VAPOR-LIQUID EQUILIBRIUM RATIO (KW OR Y/X), VAPOR  KWRP0008
              PRESSURE, MOLE FRACTION OF WATER DISSOLVED IN THE HYDRO-  KWRP0009
              CARBON PHASE, THE DERIVATIVES OF THESE QUANTITIES WITH    KWRP0010
              RESPECT TO TEMPERATURE, AND THE DERIVATIVE OF KW WITH     KWRP0011
              RESPECT TO PRESSURE.  THE K-RATIO OF WATER IS CALCULATED  KWRP0012
              BY ASSUMING IDEAL VAPOR BEHAVIOR AND USING AN EMPIRICAL   KWRP0013
              CORRELATION FOR THE WATER SOLUBILITY.                     KWRP0014

              THE INPUT PARAMETERS ARE                                  KWRP0015
              T    - TEMPERATURE, F. - REAL                             KWRP0016
              P    - PRESSURE, PSIA - REAL                              KWRP0017
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKWT, PPVWT, AND    KWRP0018
                     PXWT.                                              KWRP0019
              CALCPP - BOOLEAN.  IF TRUE, CALCULATE PKWP.               KWRP0020

              ALSO, THE VARIABLES KC3 AND KC6 ARE NON-LOCAL.            KWRP0021
              KC3  - WATER VAPOR PRESSURE COEFFIENTS - REAL ARRAY [0:3].KWRP0022
                     KC3 IS USED AS ANTC IN THE ANT1 PROCEDURE.         KWRP0023
              KC6  - COEFFICIENTS FOR THE SOLUBILITY OF WATER IN HYDRO- KWRP0024
                     CARBONS - REAL ARRAY [0:3].  KC6 IS USED AS ANTC   KWRP0025
                     IN THE ANT1 PROCEDURE.                             KWRP0026

              THE OUTPUT PARAMETERS ARE                                 KWRP0027
              KW   - VAPOR-LIQUID EQUILIBRIUM RATIO (Y/X) OF WATER -    KWRP0028
                     REAL                                               KWRP0029
              PVW  - VAPOR PRESSURE OF WATER, PSIA                      KWRP0030
              XW   - MOLE FRACTION OF WATER DISSOLVED IN HYDROCARBON    KWRP0031
                     PHASE - REAL                                       KWRP0032
              PKWT - T DERIVATIVE OF KW - REAL                          KWRP0033
              PKWP - P DERIVATIVE OF KW - REAL                          KWRP0034
              PPVWT - T DERIVATIVE OF PVW - REAL                        KWRP0035
              PXWT - T DERIVATIVE OF XW - REAL                          KWRP0036

              THE ANT1 PROCEDURE MUST BE DECLARED PRIOR TO KWATER ;     KWRP0037

VALUE         T, P, CALCPT, CALCPP ;                                    KWRP0038
BOOLEAN       CALCPT, CALCPP ;                                          KWRP0039
REAL          T, P, KW, PVW, XW, PKWT, PKWP, PPVWT, PXWT ;              KWRP0040

         BEGIN                                                          KWRP0041

COMMENT       ENTER THE ANT1 PROCEDURE TO CALCULATE THE VAPOR PRESSURE  KWRP0042
              OF WATER, AND ITS TEMPERATURE DERIVATIVE. ;               KWRP0043
              ANT1 (T, CALCPT, KC3, PVW, PPVWT) ;                       KWRP0044

COMMENT       ENTER THE ANT1 PROCEDURE TO CALCULATE THE SOLUBILITY OF   KWRP0045
              WATER IN HYDROCARBONS, AND ITS TEMPERATURE DERIVATIVE. ;  KWRP0046
              ANT1 (T, CALCPT, KC6, XW, PXWT) ;                         KWRP0047

COMMENT       CALCULATE THE K-RATIO OF WATER. ;                         KWRP0048
              KW ~ PVW/(P | XW) ;                                       KWRP0049

COMMENT       IF CALCPT IS TRUE, CALCULATE THE T DERIVATIVE OF KW. ;    KWRP0050
              IF CALCPT THEN                                            KWRP0051
              PKWT ~ (-PXWT/XW + PPVWT/PVW) | KW ;                      KWRP0052

COMMENT       IF CALCPP IS TRUE, CALCULATE THE P DERIVATIVE OF KW. ;    KWRP0053
              IF CALCPP THEN                                            KWRP0054
              PKWP ~ -KW/P                                              KWRP0055

         END  KWATER ;                                                  KWRP0056
PROCEDURE     KPICKW (T, P, PICKK, CALCPT, CALCPP, CALCPDC, K, PKT, PKP,KPWP0001
              PVW, XW, PPVWT, PXWT) ;                                   KPWP0002

COMMENT       THIS PROCEDURE TIES TOGETHER THE PROCEDURES KNGAA, KIDEAL,KPWP0003
              KPOLY, PHIPART, AND KWATER FOR THE PURPOSE OF CALCULATING KPWP0004
              K-RATIOS AND THEIR DERIVATIVES.                           KPWP0005
              BY DONALD L. VOSS                                         KPWP0006
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       KPWP0007
              CARD SEQUENCE CODE STARTS WITH KPWP0001.                  KPWP0008

              FIRST RELEASE DATE    5-5-63                              KPWP0009
              THE INPUT PARAMETERS ARE                                  KPWP0010
              T    - TEMPERATURE, F  - REAL                             KPWP0011
              P    - PRESSURE, PSIA  - REAL                             KPWP0012
              PICKK - NO. OF COMPONENTS FOR EACH OPTION - INTEGER ARRAY KPWP0013
                   [0:6].                                               KPWP0014
                   PICKK[1] - NO. USING KNGAA                           KPWP0015
                   PICKK[2] - NO. USING KIDEAL                          KPWP0016
                   PICKK[3] - NO. USING KPOLY                           KPWP0017
                   PICKK[4] - NO. USING PHIPART                         KPWP0018
                   PICKK[5] - NO. WHOSE K VALUES ARE GIVEN IN KC12[]    KPWP0019
                   PICKK[6] - =1 TO CALCULATE FOR WATER DISSOLVED IN A  KPWP0020
                              HYDROCARBON PHASE.  =0 IF NOT.            KPWP0021
              CALCPT - BOOLEAN.  IF TRUE, CALCULATE PKT.                KPWP0022
              CALCPP - BOOLEAN.  IF TRUE, CALCULATE PKP.                KPWP0023
              CALCPDC - BOOLEAN.  IF TRUE, CALCULATE THE PRESSURE       KPWP0024
                   DEPENDENT COEFFICIENTS IN KNGAA.  FOR CALCPDC = TRUE,KPWP0025
                   P(LAST ENTRY) MUST EQUAL P(PRESENT ENTRY).           KPWP0026

              IN ADDITION, THE KC- VARIABLES ARE NON-LOCAL TO KPICK.    KPWP0027
              KC1  - N.G.A.A. COEFFICIENTS FOR KNGAA  - REAL ARRAY [0:  KPWP0028
                   PICKK[1],0:28]                                       KPWP0029
              KC2  - VAPOR PRESSURE COEFFICIENTS FOR KIDEAL  - REAL     KPWP0030
                   ARRAY[0:PICKK[2],0:3]                                KPWP0031
              KC3  - WATER VAPOR PRESSURE COEFFICIENTS FOR KWATER  -    KPWP0032
                   REAL ARRAY [0:3]                                     KPWP0033
              KC6  - COEFFICIENTS FOR THE SOLUBILITY OF WATER IN        KPWP0034
                   HYDROCARBONS FOR KWATER  - REAL ARRAY [0:3]          KPWP0035
              KC4  - REFERENCE COMPONENT K COEFFICIENTS FOR KPOLY  -    KPWP0036
                   REAL ARRAY [0:5]                                     KPWP0037
              KC5  - RELATIVE VOLATILITY COEFFICIENTS FOR KPOLY  - REAL KPWP0038
                   ARRAY [0:PICKK[3],0:3]                               KPWP0039
              KC7  - COMPONENT V VALUES IN PHIPART  - INTEGER ARRAY     KPWP0040
                   [0:PICKK[4]]                                         KPWP0041
              KC8  - COMPONENT ALEPH VALUES IN PHIPART  - REAL ARRAY    KPWP0042
                   [0:PICKK[4],0:MAX KC7[],0:MAX KC11[] ]               KPWP0043
              KC9  - COMPONENT BETA VALUES IN PHIPART  - REAL ARRAY     KPWP0044
                   [0:PICKK[4],0:MAX KC7[],0:MAX KC11[] ]               KPWP0045
              KC10 - COMPONENT COEFF VALUES IN PHIPART  - REAL ARRAY    KPWP0046
                   [0:PICKK[4],0:MAX (KC11[I]+1) ... (KC11[V]+1) ]      KPWP0047
              KC11 - COMPONENT P VALUES IN PHIPART  - INTEGER ARRAY     KPWP0048
                   [0:PICKK[4],0:MAX KC7[] ]                            KPWP0049
              IN PHIPART, THE INDEPENDENT VARIABLES ARE T OR T AND P.   KPWP0050
              KC12 - GIVEN VALUES OF COMPONENT K RATIOS  - REAL ARRAY   KPWP0051
                   [0:PICKK[5] ]                                        KPWP0052

              THE OUTPUT PARAMETERS ARE                                 KPWP0053
              K    - COMPONENT K-RATIOS (Y/X)  - REAL ARRAY [0:N],      KPWP0054
                   WHERE N IS THE TOTAL NUMBER OF COMPONENTS.           KPWP0055
              PKT  - COMPONENT T DERIVATIVES OF K  - REAL ARRAY [0:N]   KPWP0056
              PKP  - COMPONENT P DERIVATIVES OF K  - REAL ARRAY [0:N] - KPWP0057
                   (NOT INCLUDED FOR KPOLY)                             KPWP0058
              PVW  - VAPOR PRESSURE OF WATER, PSIA  - REAL              KPWP0059
              XW   - MOLE FRACTION OF WATER DISSOLVED IN HYDROCARBON    KPWP0060
                   PHASE  - REAL                                        KPWP0061
              PPVWT - T DERIVATIVE OF PVW  - REAL                       KPWP0062
              PXWT - T DERIVATIVE OF XW  - REAL                         KPWP0063

              THE INPUT AND OUTPUT COMPONENT ARRAYS ARE ARRANGED        KPWP0064
                   ACCORDING TO THE PICKK[] ORDER.                      KPWP0065
              THE PROCEDURES KNGAA, KIDEAL, KPOLY, PHIPART, ANT1, AND   KPWP0066
                   KWATER MUST BE DECLARED PRIOR TO KPICKW.             KPWP0067
              THE PROCEDURE ANT1 MUST BE DECLARED PRIOR TO KIDEAL AND   KPWP0068
                   KWATER.  ;                                           KPWP0069

VALUE         T, P, CALCPT, CALCPP, CALCPDC ;                           KPWP0070
REAL          T, P, PVW, XW, PPVWT, PXWT ;                              KPWP0071
BOOLEAN       CALCPT, CALCPP, CALCPDC ;                                 KPWP0072
ARRAY         K[0], PKT[0], PKP[0] ;                                    KPWP0073
INTEGER ARRAY PICKK[0] ;                                                KPWP0074

         BEGIN                                                          KPWP0075
INTEGER       NF1, NF2, NF3, NF4, NF5, I, E, J, U, KC7E, KC11E ;        KPWP0076
BOOLEAN ARRAY S[0:2] ;                                                  KPWP0077
REAL ARRAY    DATA, SUM[0:2] ;                                          KPWP0078

COMMENT       SET UP ITERATION LIST VARIABLES ;                         KPWP0079
              NF1 ~ PICKK[1] + PICKK[2] ;        NF2 ~ PICKK[3] + NF1 ; KPWP0080
              NF3 ~ PICKK[4] + NF2 ;             NF4 ~ PICKK[5] + NF3 ; KPWP0081
              NF5 ~ PICKK[6] + NF4 ;                                    KPWP0082

COMMENT       KWATER VALUES ;                                           KPWP0083
              IF PICKK[6] = 1 THEN                                      KPWP0084
              KWATER (T, P, CALCPT, CALCPP, K[NF5], PVW, XW, PKT[NF5],  KPWP0085
              PKP[NF5], PPVWT, PXWT) ;                                  KPWP0086

COMMENT       K-RATIOS GIVEN IN KC12[] ;                                KPWP0087
              IF PICKK[5] > 0 THEN                                      KPWP0088
              FOR I ~ NF3+1 STEP 1 UNTIL NF4 DO                         KPWP0089
         BEGIN                                                          KPWP0090
              E ~ I - NF3 ;     K[I] ~ KC12[E]                          KPWP0091
         END  ;                                                         KPWP0092

COMMENT       PHIPART VALUES ;                                          KPWP0093
              IF PICKK[4] > 0 THEN                                      KPWP0094
         BEGIN                                                          KPWP0095
              DATA[1] ~ T ;     DATA[2] ~ P ;                           KPWP0096
              S[1] ~ S[2] ~ FALSE ;                                     KPWP0097
              IF CALCPT  THEN  S[1] ~ TRUE ;                            KPWP0098
              IF CALCPP  THEN  S[2] ~ TRUE ;                            KPWP0099
              FOR I ~ NF2 + 1 STEP 1 UNTIL NF3  DO                      KPWP0100
         BEGIN                                                          KPWP0101
              E ~ I - NF2 ;     KC7E ~ KC7[E] ;                         KPWP0102
              IF  KC11[E,1] > KC11[E,2]  THEN   KC11E ~ KC11[E,1]       KPWP0103
              ELSE   KC11E ~ KC11[E,2] ;                                KPWP0104
         BEGIN                                                          KPWP0105
REAL ARRAY    ALEPH, BETA[0:2,0:KC11E] ;                                KPWP0106
              FOR J ~ 1 STEP 1 UNTIL KC7E  DO                           KPWP0107
              FOR  U ~ 1 STEP 1 UNTIL KC11E  DO                         KPWP0108
         BEGIN                                                          KPWP0109
              ALEPH[J,U] ~ KC8[E,J,U] ;     BETA[J,U] ~ KC9[E,J,U]      KPWP0110
         END ;                                                          KPWP0111
              PHIPART (KC7[E], DATA, ALEPH, BETA, KC10[E,*], KC11[E,*], KPWP0112
              S, K[I], SUM) ;                                           KPWP0113
              IF CALCPT  THEN  PKT[I] ~ SUM[1] ;                        KPWP0114
              IF CALCPP  THEN  PKP[I] ~ SUM[2]                          KPWP0115
         END                                                            KPWP0116
         END                                                            KPWP0117
         END  ;                                                         KPWP0118

COMMENT       KPOLY VALUES ;                                            KPWP0119
              IF PICKK[3] > 0 THEN                                      KPWP0120
         BEGIN                                                          KPWP0121
              KPOLY (T, PICKK[3], CALCPT, K, PKT) ;                     KPWP0122
              FOR I ~ NF1+1 STEP 1 UNTIL NF2 DO                         KPWP0123
         BEGIN                                                          KPWP0124
              E ~ I - NF1 ;     K[I] ~ K[E] ;                           KPWP0125
              IF CALCPT  THEN  PKT[I] ~ PKT[E]                          KPWP0126
         END                                                            KPWP0127
         END  ;                                                         KPWP0128

COMMENT       KIDEAL VALUES ;                                           KPWP0129
              IF PICKK[2] > 0 THEN                                      KPWP0130
         BEGIN                                                          KPWP0131
              KIDEAL (T, P, PICKK[2], CALCPT, CALCPP, K, PKT, PKP) ;    KPWP0132
              FOR I ~ PICKK[1]+1 STEP 1 UNTIL NF1 DO                    KPWP0133
         BEGIN                                                          KPWP0134
              E ~ I - PICKK[1] ;     K[I] ~ K[E] ;                      KPWP0135
              IF CALCPT  THEN  PKT[I] ~ PKT[E] ;                        KPWP0136
              IF CALCPP  THEN  PKP[I] ~ PKP[E]                          KPWP0137
         END                                                            KPWP0138
         END  ;                                                         KPWP0139

COMMENT       KNGAA VALUES ;                                            KPWP0140
              IF PICKK[1] > 0 THEN                                      KPWP0141
              KNGAA (T, P, PICKK[1], CALCPDC, CALCPT, CALCPP, K, PKT,   KPWP0142
              PKP)                                                      KPWP0143

         END  KPICKW ;                                                  KPWP0144

PROCEDURE     FLASHW (Z, PICKK, B, LIMITFW, CALCPDC, T, P, VDF, S,      FLHW   1
              X, Y) ;                                                   FLHW   2
                                                                        FLHW   3
COMMENT       PROCEDURE FOR GENERAL NONADIABATIC FLASH CALCULATIONS     FLHW   4
              FOR HYDROCARBON MIXTURES CONTAINING WATER. A SEPARATE     FLHW   5
              PURE LIQUID WATER PHASE IS CONSIDERED.                    FLHW   6
              THIS PROCEDURE COMPUTES THE TEMPERATURE OR PRESSURE OR    FLHW   7
              THE VAPOR-TO-FEED MOL RATIO FOR SPECIFIED VALUES OF THE   FLHW   8
              OTHER TWO VARIABLES. THE FRACTION OF LIQUID WATER, S,     FLHW   9
              IS CALCULATED AS A FUNCTION OF THESE VARIABLES.           FLHW  10
                                                                        FLHW  11
              A. PAUL OLESON                                            FLHW  12
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORP.              FLHW  13
              CARD SEQUENCE STARTS WITH FLHW0001.                       FLHW  14
              DATE OF FIRST ISSUE 12-1-63 .                             FLHW  15
                                                                        FLHW  16
              THE INPUT PARAMETERS ARE                                  FLHW  17
                                                                        FLHW  18
              T       - TEMPERATURE, F., REAL.                          FLHW  19
              P       - PRESSURE, PSIA, REAL.                           FLHW  20
              VDF     - VAPOR TO FEED MOL RATIO, REAL.                  FLHW  21
              Z       - FEED COMPOSITION, MOL FRACTIONS, REAL ARRAY.    FLHW  22
              PICKK   - SELECTORS OF K-RATIO CORRELATIONS, INTEGER ARRAYFLHW  23
                   SEE KPICKW PROCEDURE, PTS-   .                       FLHW  24
              CALCPDC - INSTRUCTION TO CALCULATE PRESSURE DEPENDENT     FLHW  25
                   COEFFICIENTS IN NGAA K-RATIO PROCEDURE (PTS-   ),    FLHW  26
                   BOOLEAN. TRUE IF REQUIRED.                           FLHW  27
              B       - PROBLEM TYPE, BOOLEAN ARRAY,                    FLHW  28
                   B[1] - TRUE IF WATER IS PRESENT,                     FLHW  29
                   B[2] - TRUE IF CALCULATING TEMPERATURE.              FLHW  30
                   B[3] - TRUE IF CALCULATING PRESSURE.                 FLHW  31
                   B[4] - TRUE IF CALCULATING VAPOR TO FEED RATIO.      FLHW  32
              LIMITFW - BOUNDS ON TEMPERATURE AND PRESSURE, ARRAY       FLHW  33
                   LIMITFW[1] - MIN. T                                  FLHW  34
                   LIMITFW[2] - MAX. T                                  FLHW  35
                   LIMITFW[3] - MIN. P                                  FLHW  36
                   LIMITFW[4] - MAX. P                                  FLHW  37
                                                                        FLHW  38
              GLOBAL INPUT PARAMETERS INCLUDE                           FLHW  39
              KC1 THROUGH KC12, COEFFIENTS FOR THE VARIOUS K-RATIO      FLHW  40
              CORRELATIONS. SEE PTS-041 FOR DETAILS.                    FLHW  41
                                                                        FLHW  42
              OUTPUT PARAMETERS ARE                                     FLHW  43
              T       - TEMPERATURE, F., REAL,                          FLHW  44
              P       - PRESSURE, PSIA, REAL.                           FLHW  45
              VDF     - VAPOR TO FEED MOL RATIO, REAL.                  FLHW  46
              S       - MOL RATIO OF PURE LIQUID WATER TO FEED.         FLHW  47
              X       - COMPOSITION OF LIQUID HYDROCARBON PHASE, MOL    FLHW  48
                   FRACTION, REAL ARRAY.                                FLHW  49
              Y       - COMPOSITION OF VAPOR PHASE, MOL FRACTIONS, REAL FLHW  50
                   ARRAY.                                               FLHW  51
              THE REFERENCE LABEL IS                                    FLHW  52
              ERRORFW - LABEL OUTSIDE PROCEDURE ;                       FLHW  53
                                                                        FLHW  54
VALUE         Z, PICKK, B, LIMITFW, CALCPDC ;                           FLHW  55
BOOLEAN       CALCPDC ;                                                 FLHW  56
BOOLEAN ARRAY B[0] ;                                                    FLHW  57
INTEGER ARRAY PICKK[0] ;                                                FLHW  58
REAL          T, P, VDF, S ;                                            FLHW  59
ARRAY         Z, LIMITFW, X, Y[0] ;                                     FLHW  60
                                                                        FLHW  61
         BEGIN                                                          FLHW  62
LABEL         FLW1, L1, L2, L3, FLW2, FLW3 ;                            FLHW  63
INTEGER       NW, I, NL ;                                               FLHW  64
BOOLEAN       B0, B1, B2, B3, B4, B5, B6, B7, B8 ;                      FLHW  65
REAL          STS1, STS2, XW, XW1, PVW, PVW1, G, ER, E, PGE, PGS, PSE,  FLHW  66
                                                                        FLHW  67
              PXWT ;                                                    FLHW  68
ARRAY         HK, PKT, PKP, K[0:20] ;                                   FLHW  69
                                                                        FLHW  70
REAL PROCEDURE  FT (A) ;                                                FLHW  71
VALUE         A ;                                                       FLHW  72
REAL          A ;                                                       FLHW  73
         BEGIN                                                          FLHW  74
              FT ~ - KC3[2] / (LN(A) - KC3[1]) - KC3[3]                 FLHW  75
         END  FT ;                                                      FLHW  76
                                                                        FLHW  77
REAL PROCEDURE FUNC (I,EA,EB,EC,HK,Z,VDF,S) ;                           FLHW  78
VALUE         I, EA, EB, EC, Z, HK, VDF, S ;                            FLHW  79
INTEGER       I, EA, EB, EC ;                                           FLHW  80
REAL          VDF, S ;                                                  FLHW  81
ARRAY         Z, HK[0] ;                                                FLHW  82
         BEGIN                                                          FLHW  83
              FUNC ~ HK[I]*EA | Z[I]*EC / (HK[I] | VDF + 1.0 - S)*EB    FLHW  84
         END FUNC ;                                                     FLHW  85
                                                                        FLHW  86
COMMENT       SUM NUMBER OF COMPONENTS FROM PICKK ARRAY ;               FLHW  87
                                                                        FLHW  88
              NW ~ 0 ;  FOR I ~ 1 STEP 1 UNTIL 6 DO NW ~ PICKK[I] + NW ;FLHW  89
              B0 ~ (VDF = 0.0) ;  B1 ~ (VDF = 1.0) ;                    FLHW  90
              B2 ~ B3 ~ B4 ~ B5 ~ B6 ~ B7 ~ B8 ~ FALSE ;                FLHW  91
FLW1:         KPICKW (T, P, PICKK, B[2], B[3], CALCPDC, K, PK, PK,      FLHW  92
              PVW, XW, STS1, PXWT) ;                                    FLHW  93
              IF B[2] THEN CALCPDC ~ FALSE ELSE CALCPDC ~ TRUE ;        FLHW  94
              XW1 ~ 1.0 - XW ;                                          FLHW  95
              FOR I ~ 1 STEP 1 UNTIL NW DO HK[I] ~ K[I] - 1.0 ;         FLHW  96
                                                                        FLHW  97
COMMENT       IF CALCULATING VDF, CHECK IF 0 { VDF { (1 - S) ;          FLHW  98
                                                                        FLHW  99
L1 :          IF B[4] THEN                                              FLHW 100
         BEGIN                                                          FLHW 101
              STS1 ~ STS2 ~ 0 ;                                         FLHW 102
              FOR I ~ 1 STEP 1 UNTIL NW DO                              FLHW 103
         BEGIN                                                          FLHW 104
              STS1 ~ Z[I] / K[I] + STS1 ;  STS2 ~ Z[I] | K[I] + STS2    FLHW 105
         END  ;                                                         FLHW 106
              IF B[1] AND (Z[NW] > XW) THEN                             FLHW 107
         BEGIN                                                          FLHW 108
              S ~ (Z[NW] - XW) / XW1 ;                                  FLHW 109
              STS2 ~ -HK[NW] | S + STS2                                 FLHW 110
         END                                                            FLHW 111
              ELSE S ~ 0 ;                                              FLHW 112
              B0 ~ (STS2 { 1.0) ;  B1 ~ (STS1 { 1.0) ;                  FLHW 113
L2 :          IF B1 THEN                                                FLHW 114
         BEGIN                                                          FLHW 115
                                                                        FLHW 116
COMMENT       TEST IF ONLY VAPOR AND LIQUID WATER ARE PRESENT ;         FLHW 117
                                                                        FLHW 118
L3 :          IF B[1] AND (Z[NW] > (PVW / P)) THEN                      FLHW 119
         BEGIN                                                          FLHW 120
              STS1 ~ HK[NW] | (Z[NW] | P - PVW) / (K[NW] | (P - PVW))   FLHW 121
              + STS1 ;                                                  FLHW 122
              B2 ~ B1 ~ (STS1 < 1.0) ;                                  FLHW 123
              IF B1 THEN                                                FLHW 124
         BEGIN                                                          FLHW 125
              S ~ (Z[NW] | P - PVW) / (P - PVW) ; VDF ~ 1.0 - S         FLHW 126
         END                                                            FLHW 127
                                                                        FLHW 128
COMMENT       OTHERWISE LIQUID WATER CANNOT BE PRESENT ;                FLHW 129
                                                                        FLHW 130
         END                                                            FLHW 131
              ELSE                                                      FLHW 132
         BEGIN                                                          FLHW 133
              S ~ 0 ; VDF ~ 1.0                                         FLHW 134
         END  ;                                                         FLHW 135
                                                                        FLHW 136
COMMENT       TEST IF VAPOR IS NOT PRESENT ;                            FLHW 137
                                                                        FLHW 138
         END  ;                                                         FLHW 139
              IF B0 THEN VDF ~ 0.0 ;                                    FLHW 140
                                                                        FLHW 141
COMMENT       END L2 ;                                                  FLHW 142
                                                                        FLHW 143
              IF (B1 OR B0) THEN GO TO FLW3                             FLHW 144
         END  L1 ;                                                      FLHW 145
                                                                        FLHW 146
COMMENT       COMPUTE MOL RATIO OF LIQUID WATER TO FEED ;               FLHW 147
                                                                        FLHW 148
FLW2 :        IF B1 OR NOT B[1] THEN S ~ 0 ELSE                         FLHW 149
              S ~ (-(HK[NW] | VDF + 1.0) | XW + Z[NW]) / XW1 ;          FLHW 150
              B8 ~ (S > 0) ;  IF NOT B8 THEN S ~ 0 ;                    FLHW 151
              IF (NOT B2 AND NOT B[4] AND ((S + VDF) > 1.0)) THEN       FLHW 152
         BEGIN                                                          FLHW 153
              S ~ 1.0 - VDF ;                                           FLHW 154
              IF B[2] THEN T ~ FT ((Z[NW] - S) | P / VDF)               FLHW 155
              ELSE  P ~ VDF | PVW / (Z[NW] - S) ;                       FLHW 156
              B2 ~ TRUE ; GO TO FLW1                                    FLHW 157
         END  ;                                                         FLHW 158
                                                                        FLHW 159
COMMENT       EVALUATE FUNCTION G ;                                     FLHW 160
                                                                        FLHW 161
              G ~ 0 ;                                                   FLHW 162
              FOR I ~ 1 STEP 1 UNTIL NW DO G ~ FUNC(I,1,1,1,HK,Z,VDF,S) FLHW 163
              + G ;                                                     FLHW 164
              IF B8 THEN G ~ - FUNC(NW,1,1,0,HK,Z,VDF,S) | S + G ;      FLHW 165
                                                                        FLHW 166
COMMENT       TEST IF TWO OR THREE PHASES ARE PRESENT ;                 FLHW 167
                                                                        FLHW 168
              IF B2 THEN                                                FLHW 169
         BEGIN                                                          FLHW 170
              IF G > 0 THEN                                             FLHW 171
         BEGIN                                                          FLHW 172
              B1 ~ TRUE ;  GO TO FLW3                                   FLHW 173
         END                                                            FLHW 174
              ELSE  B2 ~ FALSE                                          FLHW 175
         END  ;                                                         FLHW 176
                                                                        FLHW 177
COMMENT       TEST IF TOLERANCE ON G IS MET ;                           FLHW 178
                                                                        FLHW 179
              IF B[4] THEN ER ~ 10.0*(-6) ELSE ER ~ 10.0*(-3) ;         FLHW 180
              IF ABS(G) < ER THEN                                       FLHW 181
         BEGIN                                                          FLHW 182
                                                                        FLHW 183
COMMENT       COMPARE DEW POINTS WITH RESPECT TO LIQUID WATER AND       FLHW 184
              LIQUID HYDROCARBON PHASES. SELECT LOWER PRESSURE OR       FLHW 185
              HIGHER TEMPERATURE ;                                      FLHW 186
                                                                        FLHW 187
              IF B[1] AND B1 THEN                                       FLHW 188
         BEGIN                                                          FLHW 189
              IF B[2] THEN                                              FLHW 190
         BEGIN                                                          FLHW 191
              T ~ FT (Z[NW] | P) ;                                      FLHW 192
              IF T < E THEN T ~ E                                       FLHW 193
         END                                                            FLHW 194
              ELSE                                                      FLHW 195
         BEGIN                                                          FLHW 196
              P ~ PVW / Z[NW] ;  IF P > E THEN P ~ E                    FLHW 197
         END                                                            FLHW 198
         END  ;                                                         FLHW 199
              GO TO FLW3                                                FLHW 200
         END  ;                                                         FLHW 201
                                                                        FLHW 202
COMMENT       ADJUST E USING THE NEWTON-RAPHSON METHOD. CALCULATE THE   FLHW 203
              DERIVATIVES FIRST ;                                       FLHW 204
                                                                        FLHW 205
              PGE ~ 0 ;                                                 FLHW 206
              IF B[4] THEN                                              FLHW 207
         BEGIN                                                          FLHW 208
              E ~ VDF ;                                                 FLHW 209
              FOR I ~ 1 STEP 1 UNTIL NW DO                              FLHW 210
              PGE ~ - FUNC(I,2,2,1,HK,Z,VDF,S) + PGE                    FLHW 211
         END                                                            FLHW 212
              ELSE                                                      FLHW 213
         BEGIN                                                          FLHW 214
              IF B[2] THEN E ~ T ELSE E ~ P ;                           FLHW 215
              FOR I ~ 1 STEP 1 UNTIL NW DO                              FLHW 216
              PGE ~ FUNC(I,0,2,1,HK,Z,VDF,S) | PK[I] + PGE ;            FLHW 217
              PGE ~ (1.0 - S) | PGE                                     FLHW 218
         END  ;                                                         FLHW 219
                                                                        FLHW 220
COMMENT       ADJUST PGE FOR S-DEPENDENT TERMS ;                        FLHW 221
                                                                        FLHW 222
              IF B8 THEN                                                FLHW 223
         BEGIN                                                          FLHW 224
              PGS ~ 0 ;                                                 FLHW 225
              FOR I ~ 1 STEP 1 UNTIL NW DO                              FLHW 226
              PGS ~ FUNC(I,1,2,1,HK,Z,VDF,S) + PGS ;                    FLHW 227
              PGS ~ - FUNC(NW,1,2,0,HK,Z,VDF,S) | (HK[NW] | VDF + 1.0)  FLHW 228
              + PGS ;                                                   FLHW 229
              IF B[4] THEN                                              FLHW 230
         BEGIN                                                          FLHW 231
              PSE ~ -HK[NW] | XW / XW1 ;                                FLHW 232
              PGE ~ FUNC(NW,2,2,0,HK,Z,VDF,S) | S + PGE                 FLHW 233
         END                                                            FLHW 234
              ELSE                                                      FLHW 235
         BEGIN                                                          FLHW 236
              PGE ~ FUNC(NW,0,2,0,HK,Z,VDF,S) | PK[NW] | (S - 1.0) | S  FLHW 237
              + PGE ;                                                   FLHW 238
              PSE ~ -PK[NW] | XW | VDF / XW1 ;                          FLHW 239
              IF B[2] THEN PSE ~ -(HK[NW] | VDF + 1.0 - S) |PXWT / XW1  FLHW 240
              + PSE                                                     FLHW 241
         END  ;                                                         FLHW 242
                                                                        FLHW 243
COMMENT       ADJUST DERIVATIVE OF G ;                                  FLHW 244
                                                                        FLHW 245
              PGE ~ PGS | PSE  + PGE                                    FLHW 246
         END  ;                                                         FLHW 247
                                                                        FLHW 248
COMMENT       ADJUST VALUE OF E FOR NEXT ITERATION ;                    FLHW 249
                                                                        FLHW 250
              E ~ E - G / PGE ;                                         FLHW 251
                                                                        FLHW 252
COMMENT       TEST LIMITS. IF EXCEEDED EXIT TO REFERENCE LABEL ERRORFLW FLHW 253
              IN MAIN PROGRAM ;                                         FLHW 254
                                                                        FLHW 255
              IF NOT B[4] THEN                                          FLHW 256
         BEGIN                                                          FLHW 257
              IF B[2] THEN NL ~ 0 ELSE NL ~ 2 ;                         FLHW 258
              IF E < LIMITFW[NL+1] THEN                                 FLHW 259
         BEGIN                                                          FLHW 260
              IF B7 THEN GO TO ERRORFW ;                                FLHW 261
              E ~ LIMITFW[NL+1] ;                                       FLHW 262
              IF B[3] AND B4 THEN E ~ LIMITFW[4] - 50.0 ;               FLHW 263
              IF B[2] OR B4 THEN B7 ~ B4 ~ TRUE                         FLHW 264
         END  ;                                                         FLHW 265
                                                                        FLHW 266
COMMENT       TEST IF UPPER LIMITS ARE EXCEEDED ;                       FLHW 267
                                                                        FLHW 268
              IF E > LIMITFW[NL+2] THEN                                 FLHW 269
         BEGIN                                                          FLHW 270
              IF B5 THEN GO TO ERRORFW ;                                FLHW 271
              E ~ LIMITFW[NL+2] ;                                       FLHW 272
              IF B[3] AND B6 THEN E ~ LIMITFW[3] + 50.0 ;               FLHW 273
              IF B6 OR B[2] THEN B5 ~ B6 ~ TRUE                         FLHW 274
         END                                                            FLHW 275
         END  ;                                                         FLHW 276
                                                                        FLHW 277
COMMENT       RETURN TO FLW1 OR FLW2 TO CONTINUE ITERATIONS IF THE      FLHW 278
              FUNCTION G IS NOT WITHIN TOLERANCE ;                      FLHW 279
                                                                        FLHW 280
              IF B[2] THEN T ~ E                                        FLHW 281
              ELSE IF B[3] THEN P ~ E                                   FLHW 282
              ELSE                                                      FLHW 283
         BEGIN                                                          FLHW 284
              VDF ~ E ;  GO TO FLW2                                     FLHW 285
         END  ;                                                         FLHW 286
              GO TO FLW1 ;                                              FLHW 287
                                                                        FLHW 288
COMMENT       COMPUTE COMPOSITIONS OF PHASES PRESENT AT SOLUTION ;      FLHW 289
                                                                        FLHW 290
FLW3 :        FOR I ~ 1 STEP 1 UNTIL NW DO                              FLHW 291
         BEGIN                                                          FLHW 292
              X[I] ~ FUNC(I,0,1,1,HK,Z,VDF,S) ;  Y[I] ~ K[I] | X[I]     FLHW 293
         END  ;                                                         FLHW 294
              IF B0 OR B1 THEN                                          FLHW 295
              FOR I ~ 1 STEP 1 UNTIL NW DO                              FLHW 296
         BEGIN                                                          FLHW 297
              IF B1 THEN X[I] ~ 0 ELSE Y[I] ~ 0                         FLHW 298
         END  ;                                                         FLHW 299
              IF B[1] AND NOT B1 THEN                                   FLHW 300
         BEGIN                                                          FLHW 301
              X[NW] ~ - FUNC(NW,0,1,0,HK,Z,VDF,S) | S + X[NW] ;         FLHW 302
              IF NOT B0 THEN Y[NW] ~ K[NW] | X[NW]                      FLHW 303
         END  ;                                                         FLHW 304
              IF B2 THEN  Y[NW] ~ Y[NW] - S / VDF ;                     FLHW 305
         END FLASHW  ;                                                  FLHW 306
                                                                        TEST  39
              KC3[1] ~ 14.40555 ;   KC3[2] ~ 6936.5635 ;                TEST  40
              KC3[3] ~ 379.92103 ;   KC6[1] ~ 6.1556551 ;               TEST  41
              KC6[2] ~ 6691.9779 ;   KC6[3] ~ 420.42673 ;               TEST  42
              FOR I ~ 1 STEP 1 UNTIL 8 DO PICKK[I] ~ 0 ;                TEST  43
              PICKK[1] ~ 2 ;  PICKK[6] ~ 1 ;                            TEST  44
              LIMITFW[1] ~ 0.0 ;  LIMITFW[2] ~ 500.0 ;                  TEST  45
              LIMITFW[3] ~ 10.0 ;  LIMITFW[4] ~ 1600.0 ;                TEST  46
              READ (FLIN, F2, LI1)[ENDPROB] ;                           TEST  47
              CALCPDC ~ TRUE ;                                          TEST  48
                                                                        TEST  49
              FOR N ~ 1 STEP 1 UNTIL 19 DO                              TEST  50
         BEGIN                                                          TEST  51
              READ (FLIN,F3,LI2) ;                                      TEST  52
              FLASHW (Z, PICKK, B, LIMITFW, CALCPDC, T, P, VDF, S,      TEST  53
              X, Y) ;                                                   TEST  54
              WRITE (FIL1,FO1,LI3) ;                                    TEST  55
              WRITE (FIL1, FO2, L4) ;                                   TEST  56
         END  ;                                                         TEST  57
                                                                        TEST  58
ENDPROB:      ;                                                         TEST  59
ERRORFW: END  .                                                         TEST  60
                                                                        TEST  61
 LABEL  000000000FILE1  0010000001
  -0.14609524@ 03  0.17442830@ 04 -0.88504047@ 04  0.23172909@ 05*CP# 5000NC4  1
  -0.32904241@ 05  0.24047149@ 05 -0.70717229@ 04  0.51791307@ 03*CP# 5000NC4  2
  -0.60967471@ 04  0.30280354@ 05 -0.77393885@ 05  0.10670547@ 06*CP# 5000NC4  3
  -0.75224715@ 05  0.21217935@ 05 -0.63796025@ 03  0.75662458@ 04*CP# 5000NC4  4
  -0.36863956@ 05  0.92089775@ 05 -0.12320518@ 06  0.83414815@ 05*CP# 5000NC4  5
  -0.22305445@ 05  0.28216774@ 03 -0.33359564@ 04  0.16069058@ 05*CP# 5000NC4  6
  -0.39576618@ 05  0.51906262@ 05 -0.34121151@ 05  0.87318732@ 04*CP# 5000NC4  7
  -0.21348786@ 03  0.25474323@ 04 -0.12798200@ 05  0.33160386@ 05*CP# 5000NC7  1
  -0.46617734@ 05  0.33733453@ 05 -0.98179094@ 04  0.75259770@ 03*CP# 5000NC7  2
  -0.90661037@ 04  0.45105376@ 05 -0.11567362@ 06  0.16082237@ 06*CP# 5000NC7  3
  -0.11494396@ 06  0.33049039@ 05 -0.83874040@ 03  0.10204559@ 05*CP# 5000NC7  4
  -0.50087083@ 05  0.12659472@ 06 -0.17312184@ 06  0.12128248@ 06*CP# 5000NC7  5
  -0.34048056@ 05  0.31865699@ 03 -0.38575857@ 04  0.18652295@ 05*CP# 5000NC7  6
  -0.46388321@ 05  0.62285187@ 05 -0.42635868@ 05  0.11615321@ 05*CP# 5000NC7  7
     280.0  75.0 0.5 0.250 0.250 0.500 1 0 0 1
     220.0  50.0 0.5 0.250 0.250 0.500 1 0 0 1
     150.0  75.0 0.5 0.490 0.490 0.020 1 0 0 1
     175.0  75.0 0.5 0.300 0.300 0.400 1 0 0 1
     225.0  75.0 0.5 0.299 0.699 0.002 1 0 0 1
     200.0  80.0 0.0 0.400 0.400 0.200 1 1 0 0
     200.0  60.0 0.0 0.499 0.499 0.002 1 1 0 0
     200.0  70.0 1.0 0.300 0.500 0.200 1 1 0 0
     200.0 100.0 1.0 0.150 0.150 0.700 1 1 0 0
     200.0  50.0 0.4 0.100 0.100 0.800 1 1 0 0
     200.0  75.0 0.5 0.300 0.300 0.400 1 1 0 0
     200.0  75.0 0.5 0.499 0.499 0.002 1 1 0 0
     100.0 100.0 0.0 0.600 0.200 0.200 1 0 1 0
     160.0 100.0 0.0 0.500 0.496 0.004 1 0 1 0
     300.0 100.0 1.0 0.100 0.700 0.200 1 0 1 0
     200.0 100.0 1.0 0.100 0.100 0.800 1 0 1 0
     200.0 100.0 0.4 0.100 0.100 0.800 1 0 1 0
     120.0 100.0 0.6 0.500 0.200 0.300 1 0 1 0
     250.0 100.0 0.5 0.290 0.690 0.020 1 0 1 0
