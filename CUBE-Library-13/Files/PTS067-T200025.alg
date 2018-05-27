 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                          TEST   1
                                                                        TEST   2
COMMENT       TEST PROGRAM FOR ABSACT (ACTUAL ABSORBER) PROCEDURE.      TEST   3
              MARK N. LAYMAN AND A. PAUL OLESON                         TEST   4
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION        TEST   5
              FIRST RELEASE DATE 12-1-63 ;                              TEST   6
                                                                        TEST   7
INTEGER       ITER, M, N, OILS, GASES, TYPE, VAPDRWS, LIQDRWS, KEY, I,  TEST   8
              J, U ;                                                    TEST   9
                                                                        TEST  10
BOOLEAN       CALCPDC, WATER ;                                          TEST  11
BOOLEAN ARRAY PRV[0:3];                                                 TEST  12
REAL          PVWC1, PVWC2, PVWC3, SOLWC1, SOLWC2, SOLWC3, P, RCVR,     TEST  13
              HVFM, V1, T1, HV1, LM, TM, HLM, RECOVERY, LOGP ;          TEST  14
                                                                        TEST  15
FILE IN       FLIN(1, 10) ;                                             TEST  16
FILE OUT      FIL1 1 (1,15) ;                                           TEST  17
FORMAT IN     FRIN1(9I4, L8, 3E12.4),           FRIN2(3E26.8) ;         TEST  18
LIST          LIN1(TYPE, ITER, M, N, OILS, GASES, VAPDRWS, LIQDRWS, KEY,TEST  19
              WATER, P, HVFM, RCVR),                                    TEST  20
              LIN2(PVWC1, PVWC2, PVWC3),                                TEST  21
              LIN3(SOLWC1, SOLWC2, SOLWC3);                             TEST  22
                                                                        TEST  23
LABEL         START, ENDPROB ;                                          TEST  24
START:        READ (FLIN, FRIN1, LIN1)[ENDPROB] ;                       TEST  25
              IF WATER THEN                                             TEST  26
         BEGIN                                                          TEST  27
              READ(FLIN, FRIN2, LIN2) ;        READ(FLIN, FRIN2, LIN3)  TEST  28
         END  ;                                                         TEST  29
         BEGIN                                                          TEST  30
ALPHA ARRAY   NAMES[0:N] ;                                              TEST  31
INTEGER ARRAY LFP[0:OILS], VFP[0:GASES], LDP[0:LIQDRWS], VDP[0:VAPDRWS],TEST  32
              HVC2[0:2] ;                                               TEST  33
                                                                        TEST  34
ARRAY         HVC3, HVC4, HVC7, HVC8[0:2,0:6], HVC5, HVC9[0:49], HVC1,  TEST  35
              HLC5[0:N,0:4], KC1[0:N,0:28], TC, PC, Z, YV1, XLM, E[0:N],TEST  36
              Q[0:M], LF, TLF, HLF[0:OILS], VF, TVF, HVF[0:GASES], LD,  TEST  37
              TLD, HLD[0:LIQDRWS], VD, TVD, HVD[0:VAPDRWS],             TEST  38
              XLD[0:LIQDRWS,0:N], YVD[0:VAPDRWS,0:N], XLF[0:OILS,0:N],  TEST  39
              YVF[0:GASES,0:N], PKT, PKP[0:N] ;                         TEST  40
                                                                        TEST  41
COMMENT       GLOBAL ARRAYS FOR KNGAA PROCEDURE ;                       TEST  42
ARRAY         TR1S1[0:4], PDC[0:N,0:4], LOGP1[0:7] ;                    TEST  43
                                                                        TEST  44
COMMENT       GLOBAL ARRAYS FOR PHIPART PROCEDURE ;                     TEST  45
INTEGER ARRAY TADD, ADD[0:4], DEGREE[0:3] ;                             TEST  46
ARRAY         POLY[0:25], TT[0:3] ;                                     TEST  47
                                                                        TEST  48
COMMENT       GLOBAL ARRAYS FOR HVPCOR PROCEDURE ;                      TEST  49
ARRAY         TR1S3, TR1S4[0:2] ;                                       TEST  50
                                                                        TEST  51
COMMENT       GLOBAL ARRAYS FOR ABSACT PROCEDURE ;                      TEST  52
BOOLEAN ARRAY PRV[0:3] ;                                                TEST  53
ARRAY         L, V, T, HV, PHVT, HL, PHLT[0:M+1], K, S[0:M+1,0:N] ;     TEST  54
FORMAT IN     FRIN1(8E10.3),                                            TEST  55
              FRIN2(A6,2F30.2),                                         TEST  56
              FRIN2A(E17.8,3E16.8,X15),                                 TEST  57
              FRIN3(I6,2E35.8),     FRIN3A(3F20.6),     FRIN4(I6,E35.8);TEST  58
                                                                        TEST  59
LIST          LIN1(FOR J ~ 1 STEP 1 UNTIL M DO Q[J]),                   TEST  60
              LIN1A(FOR I ~ 1 STEP 1 UNTIL N DO E[I]),                  TEST  61
              LIN2(NAMES[I], TC[I], PC[I]),                             TEST  62
              LIN2A(FOR J ~ 1,2,3,4 DO HVC1[I,J]),                      TEST  63
              LIN2B(FOR J ~ 1,2,3,4 DO HLC5[I,J]),                      TEST  64
              LIN2C(FOR J ~ U STEP 1 UNTIL U+3 DO KC1[I,J]),            TEST  65
              LIN3(LFP[J], LF[J], TLF[J]),                              TEST  66
              LIN3A(FOR I ~ 1 STEP 1 UNTIL N DO XLF[J,I]),              TEST  67
              LIN4(VFP[J], VF[J], TVF[J]),                              TEST  68
              LIN4A(FOR I ~ 1 STEP 1 UNTIL N DO YVF[J,I]),              TEST  69
              LIN5(LDP[J], LD[J]),             LIN6(VDP[J], VD[J]) ;    TEST  70
                                                                        TEST  71
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
         END  KNGAA ;

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

PROCEDURE     HVPCOR ( N, T, P, Y, CALCDHV, HV, PHVT, PHVP, PHVY ) ;    HVPC   1
                                                                        HVPC   2
COMMENT       THIS PROCEDURE COMPUTES THE ENTHALPY OF A GASEOUS MIXTURE HVPC   3
              AND, WITH SEPARATE OPTIONS, ITS PARTIAL DERIVATIVES       HVPC   4
              WITH RESPECT TO TEMPERATURE, PRESSURE AND MOL FRACTIONS . HVPC   5
                                                                        HVPC   6
              MARK N. LAYMAN                                            HVPC   7
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       HVPC   8
                                                                        HVPC   9
              CARD SEQUENCE STARTS WITH HVPC-0001                       HVPC  10
              FIRST RELEASE DATE 11-15-63.                              HVPC  11
                                                                        HVPC  12
              THE INPUT PARAMETERS ARE                                  HVPC  13
              N    - NUMBER OF COMPONENTS - INTEGER                     HVPC  14
              T    - TEMPERATURE, F - REAL                              HVPC  15
              P    - PRESSURE, PSIA - REAL                              HVPC  16
              Y    - COMPONENT MOL FRACTIONS - REAL ARRAY [0:N]         HVPC  17
              CALCDHV - BOOLEAN ARRAY [0:3].  IF CALCDHV[1], [2] AND [3]HVPC  18
                        ARE TRUE, THEN PHVT, PHVP AND PHVY WILL BE      HVPC  19
                        COMPUTED, RESPECTIVELY .                        HVPC  20
              IN ADDITION, ARRAYS TR1S3, TR1S4, TC, PC, HVC1 - 5        HVPC  21
              AND HVC7 - 9 ARE NON-LOCAL TO THIS PROCEDURE .            HVPC  22
                                                                        HVPC  23
              THE OUTPUT PARAMETERS ARE                                 HVPC  24
              HV   - VAPOR ENTHALPY, BTU/MOL - REAL                     HVPC  25
              PHVT - VAPOR HEAT CAPACITY, BTU/MOL/DEG.F - REAL          HVPC  26
              PHVP - PARTIAL DERIVATIVE OF VAPOR ENTHALPY WITH          HVPC  27
                     RESPECT TO PRESSURE, BTU/MOL/PSIA - REAL           HVPC  28
              PHVY - PARTIAL DERIVATIVE OF VAPOR ENTHALPY WITH RESPECT  HVPC  29
                     TO MOL FRACTION, BTU/MOL - REAL ARRAY [0:N] ;      HVPC  30
                                                                        HVPC  31
VALUE         N, T, P ;                                                 HVPC  32
INTEGER       N ;                                                       HVPC  33
REAL          T, P, HV, PHVT, PHVP ;                                    HVPC  34
BOOLEAN ARRAY CALCDHV[0] ;                                              HVPC  35
ARRAY         Y, PHVY[0] ;                                              HVPC  36
                                                                        HVPC  37
         BEGIN                                                          HVPC  38
INTEGER       I ;                                                       HVPC  39
BOOLEAN       CALCPHVT, CALCPHVP, CALCPHVY ;                            HVPC  40
REAL          TPC, PPC, TPR, PPR, YI, PHOT, TRS1, TRS2, TRS3, TRS4 ;    HVPC  41
                                                                        HVPC  42
              HV ~ PHOT ~ TPC ~ PPC ~ 0.0 ;                             HVPC  43
              CALCPHVT ~ CALCDHV[1] ;        CALCPHVP ~ CALCDHV[2] ;    HVPC  44
              CALCPHVY ~ CALCDHV[3] ;                                   HVPC  45
              IF CALCPHVY THEN CALCDHV[1] ~ CALCDHV[2] ~ TRUE ;         HVPC  46
                                                                        HVPC  47
COMMENT       COMPUTE IDEAL GAS ENTHALPY AND HEAT CAPACITY              HVPC  48
              AND PSEUDO-CRITICAL TEMPERATURE AND PRESSURE ;            HVPC  49
                                                                        HVPC  50
              TRS1 ~ T | T ;                                            HVPC  51
              FOR I ~ 1 STEP 1 UNTIL N DO                               HVPC  52
         BEGIN                                                          HVPC  53
              YI ~ Y[I] ;                    TRS2 ~ HVC1[I,2] ;         HVPC  54
              TRS3 ~ HVC1[I,3] | T ;         TRS4 ~ HVC1[I,4] | TRS1 ;  HVPC  55
              PHVY[I] ~ (TRS4 + TRS3 + TRS2) | T + HVC1[I,1] ;          HVPC  56
              HV ~ PHVY[I] | YI + HV ;       IF CALCPHVT THEN           HVPC  57
              PHOT ~ (TRS4 | 3 + TRS3 + TRS3 + TRS2) | YI + PHOT ;      HVPC  58
              TPC ~ TC[I] | YI + TPC ;       PPC ~ PC[I] | YI + PPC     HVPC  59
         END  ;                                                         HVPC  60
              TR1S3[1] ~ TPR ~ (T + 459.6) / TPC ;       PPR ~ P / PPC ;HVPC  61
                                                                        HVPC  62
COMMENT       TEST PSEUDO-REDUCED PRESSURE TO CHOOSE PROPER SURFACE FIT;HVPC  63
                                                                        HVPC  64
              IF PPR { 1.0 THEN                                         HVPC  65
         BEGIN                                                          HVPC  66
              TR1S3[2] ~ - LN(PPR) ;                                    HVPC  67
                                                                        HVPC  68
COMMENT       COMPUTE THE PRESSURE CORRECTION AND ITS                   HVPC  69
              PARTIAL DERIVATIVES USING THE LOW RANGE FIT ;             HVPC  70
                                                                        HVPC  71
              PHIPART (2,TR1S3,HVC3,HVC4,HVC5,HVC2,CALCDHV,TRS1,TR1S4) ;HVPC  72
              TRS1 ~ EXP(TRS1) ;             TRS2 ~ - TPC | TRS1 ;      HVPC  73
              IF CALCPHVT THEN PHVT ~ - TR1S4[1] | TRS1 ;               HVPC  74
              IF CALCPHVP THEN PHVP ~ - TR1S4[2] | TRS2 / P ;           HVPC  75
              IF CALCPHVY THEN                                          HVPC  76
         BEGIN                                                          HVPC  77
              TRS3 ~ (TR1S4[1] | TPR - 1.0) | TRS1 ;                    HVPC  78
              TRS4 ~ - TR1S4[2] | TPC | TRS1 / PPC                      HVPC  79
         END                                                            HVPC  80
         END                                                            HVPC  81
              ELSE                                                      HVPC  82
         BEGIN                                                          HVPC  83
              TR1S3[2] ~ PPR ;                                          HVPC  84
                                                                        HVPC  85
COMMENT       COMPUTE THE PRESSURE CORRECTION AND ITS                   HVPC  86
              PARTIAL DERIVATIVES USING THE HIGH RANGE FIT ;            HVPC  87
                                                                        HVPC  88
              PHIPART (2,TR1S3,HVC7,HVC8,HVC9,HVC2,CALCDHV,TRS1,TR1S4) ;HVPC  89
              TRS2 ~ - TPC | TRS1 / PPR ;                               HVPC  90
              IF CALCPHVT THEN PHVT ~ - TR1S4[1] / PPR ;                HVPC  91
              IF CALCPHVP THEN PHVP ~ - (TR1S4[2] | TPC + TRS2) / P ;   HVPC  92
              IF CALCPHVY THEN                                          HVPC  93
         BEGIN                                                          HVPC  94
              TRS3 ~ (TR1S4[1] | TPR - TRS1) / PPR ;                    HVPC  95
              TRS4 ~ (TR1S4[2] / PPC - TRS1/P) | TPC ;                  HVPC  96
         END                                                            HVPC  97
         END  ;                                                         HVPC  98
                                                                        HVPC  99
COMMENT       COMPUTE THE TOTAL PRESSURE CORRECTED ENTHALPY             HVPC 100
              AND ITS PARTIAL DERIVATIVES ;                             HVPC 101
                                                                        HVPC 102
              HV ~ HV + TRS2 ;               PHVT ~ PHVT + PHOT ;       HVPC 103
              IF CALCPHVY THEN FOR I ~ 1 STEP 1 UNTIL N DO              HVPC 104
              PHVY[I] ~ TC[I] | TRS3 + PC[I] | TRS4 + PHVY[I] ;         HVPC 105
         END HVPCOR ;                                                   HVPC 106

PROCEDURE     HLIQ ( N, T, Z, HL, PHLX, PHLT ) ;                        HLIQ   1

COMMENT       THIS PROCEDURE COMPUTES THE TOTAL ENTHALPY, PARTIAL       HLIQ   2
              MOLAL ENTHALPIES, AND THE HEAT CAPACITY OF A LIQUID       HLIQ   3
              MIXTURE, USING EXTENDED LIQUID LINES.                     HLIQ   4
              A. PAUL OLESON                                            HLIQ   5
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       HLIQ   6
              CARD SEQUENCE STARTS WITH HLIQ-0001.                      HLIQ   7
              FIRST RELEASE 3-20-63.                                    HLIQ   8

              THE INPUT PARAMETERS ARE                                  HLIQ   9
              N    - NUMBER OF COMPONENTS - INTEGER                     HLIQ  10
              T    - TEMPERATURE, F - REAL                              HLIQ  11
              Z    - MOL FRACTIONS - REAL ARRAY [0:N]                   HLIQ  12

              THE OUTPUT PARAMETERS ARE                                 HLIQ  13
              HL   - LIQUID ENTHALPY, BTU/MOL - REAL                    HLIQ  14
              PHLX - PARTIAL MOLAL ENTHALPY, BTU/MOL - REAL ARRAY [0:N] HLIQ  15
              PHLT - LIQUID HEAT CAPACITY, BTU/MOL/DEGREE F - REAL ;    HLIQ  16

VALUE         N, T ;                                                    HLIQ  17

INTEGER       N ;                                                       HLIQ  18

REAL          T, HL, PHLT ;                                             HLIQ  19

ARRAY         Z[0], PHLX[0] ;                                           HLIQ  20

         BEGIN                                                          HLIQ  21

INTEGER       I ;                                                       HLIQ  22

REAL          ZI, T2, B, CT, DT2 ;                                      HLIQ  23

              HL ~ PHLT ~ 0.0 ;              T2 ~ T | T ;               HLIQ  24
              FOR I ~ 1 STEP 1 UNTIL N DO                               HLIQ  25
         BEGIN                                                          HLIQ  26
              ZI ~ Z[I] ;                    B ~ HLC5[I,2] ;            HLIQ  27
              CT ~ HLC5[I,3] | T ;           DT2 ~ HLC5[I,4] | T2 ;     HLIQ  28
              PHLX[I] ~ HLC5[I,1] + ( B + CT + DT2 ) | T ;              HLIQ  29
              HL ~ PHLX[I] | ZI + HL ;                                  HLIQ  30
              PHLT ~ ( DT2 | 3.0 + CT | 2.0 + B ) | ZI + PHLT           HLIQ  31
         END                                                            HLIQ  32
         END HLIQ ;                                                     HLIQ  33
PROCEDURE     ABSACT (TYPE, ITER, M, P, N, OILS, GASES, VAPDRWS,        ASRB   1
              LIQDRWS, NAMES, LFP, LF, XLF, TLF, VFP, VF, YVF, TVF,     ASRB   2
              HVFM, LDP, LD, VDP, VD, Q, WATER, KEY, RCVR, E, V1, YV1,  ASRB   3
              T1, HV1, LM, XLM, TM, HLM, XLD, TLD, HLD, YVD, TVD,       ASRB   4
              HVD, RECOVERY) ;                                          ASRB   5
                                                                        ASRB   6
COMMENT       CALCULATIONS FOR MULTIPLE FEED AND DRAW ABSORBERS AND     ASRB   7
              STRIPPERS WITH ACTUAL TRAYS. THE TRAY MODEL IS BASED ON   ASRB   8
              THE MURPHREE OVER-ALL LIQUID TRAY EFFICIENCY. INTERCOOLERSASRB   9
              MAY BE USED AT ANY TRAY .                                 ASRB  10
                                                                        ASRB  11
              A. PAUL OLESON AND MARK N. LAYMAN                         ASRB  12
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORP.              ASRB  13
                                                                        ASRB  14
              CARD SEQUENCE STARTS WITH ASRB-0001.                      ASRB  15
              FIRST RELEASE 10-1-63.                                    ASRB  16
                                                                        ASRB  17
              THE INPUT PARAMETERS ARE                                  ASRB  18
              TYPE - TYPE OF PROBLEM, INTEGER.  IF TYPE =               ASRB  19
                     0 - CALCULATE PERFORMANCE                          ASRB  20
                     1 - CALCULATE LEAN OIL FLOW TO OBTAIN SPECIFIED    ASRB  21
                         RECOVERY OF KEY COMPONENT.                     ASRB  22
                     2 - CALCULATE WET GAS OR STRIPPING MEDIA TO OBTAIN ASRB  23
                         SPECIFIED STRIPPING OF KEY COMPONENT FROM FEED ASRB  24
              ITER - NUMBER OF ITERATIONS ALLOWED, INTEGER.             ASRB  25
              M    - NUMBER OF THEORETICAL TRAYS, INTEGER.              ASRB  26
                         OIL.                                           ASRB  27
              P    - PRESSURE, PSIA - REAL.                             ASRB  28
              N    - NUMBER OF COMPONENTS, INTEGER.                     ASRB  29
              OILS - NUMBER OF OIL FEEDS, INTEGER.                      ASRB  30
              GASES - NUMBER OF GAS FEEDS, INTEGER.                     ASRB  31
              VAPDRWS - NUMBER OF VAPOR DRAWS, INTEGER.                 ASRB  32
              LIQDRWS - NUMBER OF LIQUID DRAWS, INTEGER.                ASRB  33
              NAMES - NAMES OF COMPONENTS, ALPHA ARRAY[0:N].            ASRB  34
              LFP  - NUMBER OF TRAYS TO WHICH OIL IS FED, INTEGER ARRAY ASRB  35
                     [0:OILS].                                          ASRB  36
              LF   - OIL FEED FLOWS, MOLS - REAL ARRAY[0:OILS].         ASRB  37
              XLF  - COMPOSITION OF OIL FEEDS, MOL FRACTIONS -          ASRB  38
                     REAL ARRAY[0:OILS,0:N].                            ASRB  39
              TLF  - TEMPERATURES OF OIL FEEDS, F - REAL ARRAY[0:OILS]. ASRB  40
              VFP  - NUMBERS OF TRAYS TO WHICH GAS IS FED, INTEGER ARRAYASRB  41
                     [0:GASES].                                         ASRB  42
              VF   - GAS FEED FLOWS, MOLS - REAL ARRAY[0:GASES].        ASRB  43
              YVF  - COMPOSITIONS OF GAS FEEDS, MOL FRACTIONS -         ASRB  44
                     REAL ARRAY[0:GASES,0:N].                           ASRB  45
              TVF  - GAS FEED TEMPERATURES, F - REAL ARRAY[0:GASES].    ASRB  46
              HVFM - ENTHALPY OF STEAM IF USED, BTU/MOL - REAL.         ASRB  47
              LDP  - NUMBER OF TRAYS FROM WHICH LIQUID SIDE STREAMS ARE ASRB  48
                     DRAWN, INTEGER ARRAY[0:LIQDRWS].                   ASRB  49
              LD   - SPECIFIED LIQUID DRAWS, MOLS - REAL ARRAY          ASRB  50
                     [0:LIQDRWS].                                       ASRB  51
              VDP  - NUMBER OF TRAYS FROM WHICH VAPOR SIDE STREAMS ARE  ASRB  52
                     DRAWN, INTEGER ARRAY[0:VAPDRWS].                   ASRB  53
              VD   - SPECIFIED VAPOR DRAWS, MOLS - REAL ARRAY[0:VAPDRWS]ASRB  54
              Q    - HEAT REMOVED FROM EACH TRAY, BTU - REAL ARRAY[0:M].ASRB  55
              WATER - BOOLEAN VARIABLE. TRUE IF PRESENT.                ASRB  56
              KEY  - NUMBER OF KEY COMPONENT SPECIFIED, INTEGER.        ASRB  57
              RCVR - SPECIFIED RECOVERY OF KEY COMPONENT, FRACTION.     ASRB  58
              E    - MURPHREE LIQUID OVER-ALL PLATE EFFICIENCIES,       ASRB  59
                     REAL ARRAY[0:N].                                   ASRB  60
                                                                        ASRB  61
              THE OUTPUT PARAMETERS ARE                                 ASRB  62
              V1   - FLOW OF DRY GAS, MOLS - REAL.                      ASRB  63
              YV1  - COMPOSITION OF DRY GAS, MOL FRACTIONS -            ASRB  64
                     REAL ARRAY[0:N].                                   ASRB  65
              T1   - DRY GAS TEMPERATURE, F - REAL.                     ASRB  66
              HV1  - ENTHALPY OF DRY GAS, BTU - REAL.                   ASRB  67
              LM   - RICH OIL FLOW, MOLS - REAL.                        ASRB  68
              XLM  - COMPOSITION OF RICH OIL, MOL FRACTIONS -           ASRB  69
                     REAL ARRAY[0:N].                                   ASRB  70
              TM   - RICH OIL TEMPERATURE, F - REAL.                    ASRB  71
              HLM  - ENTHALPY OF RICH OIL, BTU - REAL.                  ASRB  72
              XLD  - COMPOSITION OF SIDE STREAM LIQUID DRAWS,           ASRB  73
                     MOL FRACTIONS - REAL ARRAY[0:LIQDRWS,0:N].         ASRB  74
              TLD  - TEMPERATURES OF SIDE STREAM LIQUID DRAWS, F -      ASRB  75
                     REAL ARRAY[0:LIQDRWS].                             ASRB  76
              HLD  - ENTHALPIES OF SIDE STREAM LIQUID DRAWS, BTU -      ASRB  77
                     REAL ARRAY[0:LIQDRWS].                             ASRB  78
              YVD  - COMPOSITIONS OF SIDE STREAM VAPOR DRAWS,           ASRB  79
                     MOL FRACTIONS, REAL ARRAY[0:VAPDRWS, 0:N].         ASRB  80
              TVD  - TEMPERATURES OF SIDE STREAM VAPOR DRAWS, F -       ASRB  81
                     REAL ARRAY[0:VAPDRWS].                             ASRB  82
              HVD  - ENTHALPIES OF SIDE STREAM VAPOR DRAWS, BTU -       ASRB  83
                     REAL ARRAY[0:VAPDRWS].                             ASRB  84
              RECOVERY - CALCULATED RECOVERY OF KEY COMPONENT, FRACTION;ASRB  85
                                                                        ASRB  86
VALUE         ITER, M, P, N, OILS, TYPE, VAPDRWS, LIQDRWS, GASES, KEY,  ASRB  87
              RCVR ;                                                    ASRB  88
INTEGER       ITER, M, N, OILS, GASES, TYPE, VAPDRWS, LIQDRWS, KEY ;    ASRB  89
BOOLEAN       WATER ;                                                   ASRB  90
REAL          P, RCVR, V1, T1, HV1, LM, TM, HLM, RECOVERY, HVFM ;       ASRB  91
ALPHA ARRAY   NAMES[0] ;                                                ASRB  92
INTEGER ARRAY LFP, VFP, LDP, VDP[0] ;                                   ASRB  93
                                                                        ASRB  94
ARRAY         LF, TLF, VF, TVF, LD, VD, Q, E[0], YV1, XLM, TLD, HLD,    ASRB  95
              TVD, HVD[0], XLF, YVF, XLD, YVD[0,0] ;                    ASRB  96
                                                                        ASRB  97
         BEGIN                                                          ASRB  98
INTEGER       I, J, J0, J1, J2, U, COUNT, NUM ;                         ASRB  99
BOOLEAN       MHBAL, HINGE ;                                            ASRB 100
REAL          R, RR, TOLT, TOLL, ASPEC, SSPEC, KEPT, TRS1, TRS2, TRS3,  ASRB 101
              TRS4, TRS5, INTER, DELTA ;                                ASRB 102
                                                                        ASRB 103
LABEL         REDO, FLOWS, HEAT, ACOM, ADJUST, TRIDIAG, MASS, KREMSER,  ASRB 104
              ITERA, NOVUM, DOOR, SOLUTION ;                            ASRB 105
                                                                        ASRB 106
COMMENT       CALCULATE ENTHALPIES OF FEED STREAMS ;                    ASRB 107
                                                                        ASRB 108
                                                                        ASRB 109
ARRAY         PVW, SOLW, TRA1, TRA2, TRA3, Z, LNET, VNET, TX, LP, DA,   ASRB 110
              DB, DC, DD, GAM[0:M+1], LL, VV[0:M+1,0:N], TRA[0:N] ;     ASRB 111
                                                                        ASRB 112
REDO :        FOR J ~ 1 STEP 1 UNTIL OILS DO                            ASRB 113
              HLIQ (N, TLF[J], XLF[J,*], HLF[J], TRA, R) ;              ASRB 114
              IF HVFM = 0.0 THEN U ~ GASES  ELSE                        ASRB 115
         BEGIN                                                          ASRB 116
              U ~ GASES - 1 ;                   HVF[GASES] ~ HVFM       ASRB 117
         END  ;                                                         ASRB 118
              PRV[1] ~ PRV[2] ~ PRV[3] ~ FALSE ;                        ASRB 119
              FOR J ~ 1 STEP 1 UNTIL U DO                               ASRB 120
              HVPCOR(N, TVF[J], P, YVF[J,*], PRV, HVF[J], R, TRS1, TRA);ASRB 121
                                                                        ASRB 122
COMMENT       ESTIMATE FLOW RATES AND TEMPERATURES ;                    ASRB 123
                                                                        ASRB 124
              FOR J ~ 1 STEP 1 UNTIL M DO V[J] ~ L[J] ~ 0.0 ;           ASRB 125
              FOR J ~ 1 STEP 1 UNTIL OILS DO L[LFP[J]] ~ LF[J] ;        ASRB 126
              FOR J ~ 1 STEP 1 UNTIL GASES DO V[VFP[J]] ~ VF[J] ;       ASRB 127
              FOR J ~ 1 STEP 1 UNTIL LIQDRWS DO                         ASRB 128
              L[LDP[J] + 1] ~ L[LDP[J] + 1] - LD[J] ;                   ASRB 129
              FOR J ~ 1 STEP 1 UNTIL VAPDRWS DO                         ASRB 130
              V[VDP[J] - 1] ~ V[VDP[J] - 1] - VD[J] ;                   ASRB 131
              FOR J ~ M-1 STEP -1 UNTIL 1 DO V[J] ~ V[J + 1] + V[J] ;   ASRB 132
              FOR J ~ 2 STEP 1 UNTIL M DO L[J] ~ L[J - 1] + L[J] ;      ASRB 133
              IF V[M] = 0 THEN                                          ASRB 134
         BEGIN                                                          ASRB 135
              R ~ V[VFP[GASES]] | 0.01 ;                                ASRB 136
              FOR J ~ VFP[GASES] + 1 STEP 1 UNTIL M DO                  ASRB 137
         BEGIN                                                          ASRB 138
              V[J] ~ R ;  L[J-1] ~ L[J-1] + R                           ASRB 139
         END  ;                                                         ASRB 140
              L[M] ~ L[M] + R                                           ASRB 141
         END  ;                                                         ASRB 142
              T1 ~ 0.0 ;                                                ASRB 143
              FOR J ~ 1 STEP 1 UNTIL OILS DO T1 ~ TLF[J] + T1 ;         ASRB 144
              FOR J ~ 1 STEP 1 UNTIL GASES DO T1 ~ TVF[J] + T1 ;        ASRB 145
              T1 ~ T1 / ( OILS + GASES ) ;                              ASRB 146
              FOR J ~ 1 STEP 1 UNTIL M DO T[J] ~ T1 ;                   ASRB 147
              IF WATER THEN NUM ~ N - 1  ELSE NUM ~ N ;                 ASRB 148
                                                                        ASRB 149
COMMENT       ITERATIVE CALCULATION OF FLOWS AND TEMPERATURES ;         ASRB 150
                                                                        ASRB 151
FLOWS:        FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 152
         BEGIN                                                          ASRB 153
              IF WATER THEN                                             ASRB 154
         BEGIN                                                          ASRB 155
              PVW[J] ~ EXP(-PVWC2 / (PVWC3 + T[J]) + PVWC1) ;           ASRB 156
              SOLW[J] ~ EXP(-SOLWC2 / (SOLWC3 + T[J]) + SOLWC1) ;       ASRB 157
              K[J,N] ~ PVW[J] / (SOLW[J] | P)                           ASRB 158
         END  ;                                                         ASRB 159
              CALCPDC ~ (COUNT = 0) ;     R ~ V[J] / L[J] ;             ASRB 160
              IF (COUNT > 0) OR (J = 1) THEN                            ASRB 161
              KNGAA (T[J],P,NUM,CALCPDC,FALSE,FALSE,K[J,*],PKT,PKP) ;   ASRB 162
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 163
              IF COUNT = 0 THEN S[J,I] ~ K[1,I] | R                     ASRB 164
              ELSE S[J,I] ~ K[J,I] | R                                  ASRB 165
         END  ;                                                         ASRB 166
                                                                        ASRB 167
COMMENT       FORM MATRIX FOR COMPONENT MATERIAL BALANCES TO CALCULATE  ASRB 168
              LIQUID FLOW RATES ;                                       ASRB 169
                                                                        ASRB 170
              FOR J ~ 0 STEP 1 UNTIL M DO                               ASRB 171
              TRA1[J] ~ TRA2[J] ~ Z[J] ~ 0.0 ;                          ASRB 172
              MHBAL ~ FALSE ;                                           ASRB 173
              FOR J ~ 1 STEP 1 UNTIL LIQDRWS DO TRA1[LDP[J]+1] ~ -LD[J];ASRB 174
              FOR J ~ 1 STEP 1 UNTIL VAPDRWS DO Z[VDP[J]-1] ~ - VD[J] ; ASRB 175
              FOR J ~ 1 STEP 1 UNTIL OILS DO                            ASRB 176
              TRA2[LFP[J]] ~ TRA1[LFP[J]] + LF[J] ;                     ASRB 177
              TRA1[1] ~ 1.0 ;                TRA2[1] ~ L[1] / TRA2[1] ; ASRB 178
              Z[1] ~ Z[1] / V[2] + 1.0 ;                                ASRB 179
              FOR J ~ 2 STEP 1 UNTIL M DO                               ASRB 180
         BEGIN                                                          ASRB 181
              J0 ~ J - 1 ;                   J1 ~ J + 1 ;               ASRB 182
              TRA1[J] ~ TRA1[J] / L[J0] + 1.0 ;                         ASRB 183
              TRA2[J] ~ L[J] / (L[J0] + TRA2[J]) ;                      ASRB 184
              IF J = M THEN Z[J] ~ 1.0  ELSE                            ASRB 185
              Z[J] ~ Z[J] / V[J1] + 1.0                                 ASRB 186
         END  ;                                                         ASRB 187
                                                                        ASRB 188
MASS :        FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 189
         BEGIN                                                          ASRB 190
              IF MHBAL THEN GO TO TRIDIAG ;                             ASRB 191
              TRS2 ~ E[I] ;                                             ASRB 192
              TRS5 ~ 0.0 ;                                              ASRB 193
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 194
         BEGIN                                                          ASRB 195
              J0 ~ J - 1 ;                                              ASRB 196
              TRS3 ~ S[J,I] / TRS2 ;                                    ASRB 197
              TRS4 ~ (1.0 - TRS2) | TRA2[J] | TRS3 ;                    ASRB 198
              DA[J] ~ - (TRS4 + 1.0) | TRA1[J] ;                        ASRB 199
              DB[J0] ~ TRA1[J] | Z[J0] | TRS4 + TRS5 + 1.0 ;            ASRB 200
              DC[J0] ~ - Z[J0] | TRS3;                                  ASRB 201
              TRS5 ~ TRS3 ;                                             ASRB 202
              DD[J] ~ 0.0                                               ASRB 203
         END  ;                                                         ASRB 204
              DB[0] ~ 0.0 ;                                             ASRB 205
              DB[M] ~ TRS5 + 1.0 ;                                      ASRB 206
              FOR J ~ 1 STEP 1 UNTIL GASES DO                           ASRB 207
              DD[VFP[J]] ~ YVF[J,I] | VF[J] ;                           ASRB 208
              FOR J ~ 1 STEP 1 UNTIL OILS DO                            ASRB 209
         BEGIN                                                          ASRB 210
              J1 ~ LFP[J] ;                  TRS1 ~ XLF[J,I] | LF[J] ;  ASRB 211
              J0 ~ J1 - 1 ;                                             ASRB 212
              TRS3 ~ S[J1,I] / TRS2 ;                                   ASRB 213
              TRS4 ~ (1.0 - TRS2) | TRA2[J1] | TRS3 ;                   ASRB 214
              DD[J1] ~ (TRS4 + 1.0) | TRS1 + DD[J1] ;                   ASRB 215
              DD[J0] ~ - Z[J0] | TRS4 | TRS1 + DD[J0]                   ASRB 216
         END  ;                                                         ASRB 217
                                                                        ASRB 218
TRIDIAG :     GAM[1] ~ - DC[1] / DB[1] ;    TX[1] ~ DD[1] / DB[1] ;     ASRB 219
              FOR J ~ 2 STEP 1 UNTIL M DO                               ASRB 220
         BEGIN                                                          ASRB 221
              INTER ~ GAM[J - 1] | DA[J] + DB[J] ;                      ASRB 222
              GAM[J] ~ - DC[J] / INTER ;                                ASRB 223
              TX[J] ~ ( - TX[J - 1] | DA[J] + DD[J] ) / INTER           ASRB 224
         END  ;                                                         ASRB 225
              FOR J ~ M STEP -1 UNTIL 2 DO                              ASRB 226
              TX[J - 1] ~ GAM[J - 1] | TX[J] + TX[J - 1] ;              ASRB 227
              IF MHBAL THEN GO TO HEAT ;                                ASRB 228
                                                                        ASRB 229
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 230
              IF TX[J] > 0.0 THEN LL[J,I] ~ TX[J] ELSE LL[J,I] ~ 0.0    ASRB 231
         END  ;                                                         ASRB 232
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 233
         BEGIN                                                          ASRB 234
              V[J] ~ L[J] ~ 0.0 ;                                       ASRB 235
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 236
         BEGIN                                                          ASRB 237
              L[J] ~ LL[J,I] + L[J] ;        VV[J,I] ~ 0.0              ASRB 238
         END                                                            ASRB 239
         END  ;                                                         ASRB 240
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 241
         BEGIN                                                          ASRB 242
              FOR J ~ 1 STEP 1 UNTIL OILS DO                            ASRB 243
              VV[LFP[J],I] ~ XLF[J,I] | LF[J] ;                         ASRB 244
              FOR J ~ 1 STEP 1 UNTIL GASES DO                           ASRB 245
              VV[VFP[J],I] ~ YVF[J,I] | VF[J] + VV[VFP[J],I] ;          ASRB 246
              FOR J ~ M STEP -1 UNTIL 1 DO                              ASRB 247
         BEGIN                                                          ASRB 248
              VV[J,I] ~ VV[J+1,I] | Z[J] + LL[J-1,I] | TRA1[J] - LL[J,I]ASRB 249
              + VV[J,I] ;  IF VV[J,I] > 0.0 THEN V[J] ~ VV[J,I] + V[J]  ASRB 250
              ELSE VV[J,I] ~ 0.0                                        ASRB 251
         END                                                            ASRB 252
         END  ;                                                         ASRB 253
              V1 ~ V[1] ;          LM ~ L[M] ;                          ASRB 254
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 255
         BEGIN                                                          ASRB 256
              YV1[I] ~ VV[1,I] / V1 ;  XLM[I] ~ LL[M,I] / LM ;          ASRB 257
         END  ;                                                         ASRB 258
                                                                        ASRB 259
COMMENT       THIS COMPLETES THE MATERIAL BALANCES.  THE HEAT BALANCES  ASRB 260
              AND ADJUSTMENTS IN TRAY TEMPERATURES FOLLOW. ENTHALPIES   ASRB 261
              AND SPECIFIC HEATS OF ALL STREAMS ARE CALCULATED FIRST ;  ASRB 262
                                                                        ASRB 263
              PRV[1] ~ TRUE ;                                           ASRB 264
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 265
         BEGIN                                                          ASRB 266
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 267
         BEGIN                                                          ASRB 268
              TRA[I] ~ LL[J,I] / L[J] ;   PKT[I] ~ VV[J,I] / V[J]       ASRB 269
         END  ;                                                         ASRB 270
              HLIQ ( N, T[J], TRA, HL[J], PKP, PHLT[J] ) ;              ASRB 271
              HVPCOR(N, T[J], P, PKT, PRV, HV[J], PHVT[J], R, TRA) ;    ASRB 272
              VNET[J] ~ LNET[J] ~ 0.0                                   ASRB 273
         END  ;                                                         ASRB 274
              FOR J ~ 1 STEP 1 UNTIL LIQDRWS DO LNET[LDP[J]] ~ - LD[J] ;ASRB 275
              FOR J ~ 1 STEP 1 UNTIL VAPDRWS DO VNET[VDP[J]] ~ - VD[J] ;ASRB 276
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 277
         BEGIN                                                          ASRB 278
              HV[J] ~ HV[J] | V[J] ;  PHVT[J] ~ PHVT[J] | V[J] ;        ASBR 279
              HL[J] ~ HL[J] | L[J] ;  PHLT[J] ~ PHLT[J] | L[J] ;        ASBR 280
              LNET[J] ~ LNET[J] + L[J] ;  VNET[J] ~ VNET[J] + V[J] ;    ASBR 281
         END  ;                                                         ASRB 282
              HL[0] ~ HV[M+1] ~ 0.0 ;  HV1 ~ HV[1] ;  HLM ~ HL[M] ;     ASBR 283
              FOR J ~ 1 STEP 1 UNTIL OILS DO                            ASRB 284
              TRA1[LFP[J]] ~ HLF[J] | LF[J] ;                           ASRB 285
              FOR J ~ 1 STEP 1 UNTIL GASES DO                           ASRB 286
              TRA2[VFP[J]] ~ HVF[J]  | VF[J] ;                          ASRB 287
              L[0] ~ V[M+1] ~ 1.0 ;                                     ASRB 288
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 289
         BEGIN                                                          ASRB 290
              J0 ~ J - 1 ;                   J1 ~ J + 1 ;               ASRB 291
              DA[J] ~ PHLT[J0] | LNET[J0] / L[J0] ;                     ASRB 292
              DB[J] ~ - PHVT[J] - PHLT[J] ;                             ASRB 293
              DC[J] ~ PHVT[J1] | VNET[J1] / V[J1] ;                     ASRB 294
              DD[J] ~ HL[J0] | LNET[J0] / L[J0] + HV[J1] | VNET[J1]     ASRB 295
              / V[J1] - HL[J] - HV[J] + TRA1[J] + TRA2[J] + Q[J]        ASRB 296
         END  ;                                                         ASRB 297
              DA[1] ~ DC[M] ~ 0.0 ;                                     ASRB 298
              MHBAL ~ TRUE ;                 GO TO MASS ;               ASRB 299
                                                                        ASRB 300
HEAT:         TOLT ~ 0.2 ;  TOLL ~ 0.001 ;                              ASRB 301
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 302
         BEGIN                                                          ASRB 303
              IF ABS(TX[J]) > TOLT THEN GO TO ADJUST ;                  ASRB 304
              IF LP[J] = 0.0 THEN GO TO ADJUST ;                        ASRB 305
              IF ABS(LP[J] / L[J] - 1.0) > TOLL THEN GO TO ADJUST       ASRB 306
         END  ;                                                         ASRB 307
                                                                        ASRB 308
              IF TYPE = 0 THEN GO TO SOLUTION ;                         ASRB 309
                                                                        ASRB 310
                                                                        ASRB 311
COMMENT       ADJUST LEAN OIL RATE FOR SPECIFIED RECOVERY ;             ASRB 312
                                                                        ASRB 313
              TRS1 ~ M ;  KEPT ~ RCVR ;  HINGE ~ FALSE ;                ASRB 314
                                                                        ASRB 315
KREMSER :     TRS2 ~ 1.0 ;                                              ASRB 316
                                                                        ASRB 317
COMMENT       FIND ABSORPTION OR STRIPPING FACTOR SATISFYING            ASRB 318
              THE KREMSER EQUATION. ENTER WITH TRS1 = NUMBER OF         ASRB 319
              IDEAL STAGES AND KEPT = RECOVERY OF KEY COMPONENT.        ASRB 320
              LEAVE WITH TRS2 = CALCULATED FACTOR ;                     ASRB 321
                                                                        ASRB 322
              TRS3 ~ TRS1 / (TRS1 + 1.0) ;                              ASRB 323
              DELTA ~ (KEPT / TRS3) | 2.0 - 2.0 ;                       ASRB 324
              IF DELTA > 0.0 THEN GO TO NOVUM ELSE TRS2 ~ 0.5 ;         ASRB 325
                                                                        ASRB 326
ITERA :       TRS4 ~ TRS2*(TRS1 + 1.0 ) ;                               ASRB 327
              TRS3 ~ (TRS4 - TRS2) / (TRS4 - 1.0) ;                     ASRB 328
              DELTA ~ (KEPT - TRS3) | (TRS4 - 1.0) / ((TRS1 + 1.0) |    ASRB 329
              (1.0 - TRS3) | (TRS4 / TRS2) - 1.0) ;                     ASRB 330
                                                                        ASRB 331
NOVUM :       IF ABS(DELTA) < 0.00001 THEN GO TO DOOR ;                 ASRB 332
              TRS2 ~ TRS2 + DELTA ;           GO TO ITERA ;             ASRB 333
                                                                        ASRB 334
DOOR :        IF HINGE THEN GO TO ACOM ;                                ASRB 335
              R ~ RR ~ 0.0 ;                                            ASRB 336
              FOR U ~ 1 STEP 1 UNTIL OILS DO                            ASRB 337
              R ~ XLF[U,KEY] | LF[U] + R ;                              ASRB 338
              FOR U ~ 1 STEP 1 UNTIL GASES DO                           ASRB 339
              RR ~ YVF[U,KEY] | VF[U] + RR ;                            ASRB 340
              IF TYPE = 1 THEN                                          ASRB 341
         BEGIN                                                          ASRB 342
              ASPEC ~ TRS2 ;               KEPT ~ (LL[M,KEY] - R) / RR  ASRB 343
         END                                                            ASRB 344
              ELSE                                                      ASRB 345
         BEGIN                                                          ASRB 346
              SSPEC ~ TRS2 ;               KEPT ~ (VV[1,KEY] - RR) / R  ASRB 347
         END ;                                                          ASRB 348
              IF ABS (KEPT - RCVR) < 0.001 THEN GO TO SOLUTION ;        ASRB 349
              HINGE ~ TRUE ;           GO TO KREMSER ;                  ASRB 350
                                                                        ASRB 351
ACOM:         IF TYPE = 1 THEN                                          ASRB 352
         BEGIN                                                          ASRB 353
              LF[1] ~ LF[1] | ASPEC / TRS2 ;                            ASRB 354
              GO TO REDO                                                ASRB 355
         END  ;                                                         ASRB 356
              VF[GASES] ~ VF[GASES] | SSPEC / TRS2 ;                    ASRB 357
              GO TO REDO ;                                              ASRB 358
                                                                        ASRB 359
ADJUST:       FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 360
         BEGIN                                                          ASRB 361
              T[J] ~ T[J] - TX[J] ;          LP[J] ~ L[J]               ASRB 362
         END  ;                                                         ASRB 363
              TM ~ T[M] ;              COUNT ~ COUNT + 1 ;              ASRB 364
              IF COUNT > ITER THEN GO TO SOLUTION ;  GO TO FLOWS ;      ASRB 365
                                                                        ASRB 366
SOLUTION:BEGIN                                                          ASRB 367
REAL          TJ, LJ, XJ, YJ, VJ, HLJ, HVJ ;                            ASRB 368
LABEL         HED ;                                                     ASRB 369
FORMAT OUT    FRMTA(X20,"PRESSURE, PSIA =",E12.5,X5,"COUNT =",I3),      ASRB 370
              FRMTB1(//X5,"CALCULATION OF PERFORMANCE"),                ASRB 371
              FRMTB2(//X5,"CALCULATION OF OIL RATE"),                   ASRB 372
              FRMTB3(//X5,"CALCULATION OF GAS FEED RATE"),              ASRB 373
              FRMTC1(//X5,"SPECIFIED RECOVERY OF ",A6," =",E12.5),      ASRB 374
              FRMTC2(//X5,"CALCULATED RECOVERY OF ",A6," =",E12.5),     ASRB 375
              FRMTD(//X5,"TRAY",X4,"T[J]",X7,"V[J]",X7,"L[J]",X7,       ASRB 376
                   "VD[J]",X6,"LD[J]",X6,"VF[J]",X6"LF[J]",X6,"Q[J]"),  ASRB 377
              FRMTE(/X6,I2 ,8E11.4),  FRMTG(//X50,"VAPOR",X19,"LIQUID"),ASRB 378
              FRMTF(////X30,"TRAY",I3,X5,"TEMPERATURE F = ",E12.4),     ASRB 379
              FRMTH(/X30,"ENTHALPY, BTU",E15.5,X8,E15.5),               ASRB 380
              FRMTI(/X30,"COMPONENT",X6,"MOLS",X10,"Y",X10,"MOLS",X10,  ASRB 381
              "X"),           FRMTJ(X32,A6,X4,E12.5,E12.5,E13.5,E12.5), ASRB 382
              FRMTK(//X30,"TOTALS",X6,E12.5,X13,E12.5),                 ASRB 383
              FRMTL(////X30,"OIL TO TRAY ",I3,X5,                       ASRB 384
              "TEMPERATURE F =",E12.5),                                 ASRB 385
              FRMTM(////X30,"GAS TO TRAY ",I3,X5,                       ASRB 386
              "TEMPERATURE F =",E12.5),                                 ASRB 387
              FRMTN(////X30,"VAPOR DRAW FROM TRAY ",I2,X5,              ASRB 388
              "TEMPERATURE F =",E12.5),                                 ASRB 389
              FRMTO(////X30,"LIQUID DRAW FROM TRAY ",I2,X5,             ASRB 400
              " TEMPERATURE F =",E12.5),                                ASRB 401
              FRMTP(////X45,"COMPONENT  EFFICIENCY"),                   ASRB 402
              FRMTQ(/X45,A6,F12.3) ;                                    ASRB 403
                                                                        ASRB 404
LIST          LIST1(P, COUNT),          LIST2(NAMES[KEY], RCVR),        ASRB 405
              LIST3(J, T[J], V[J], L[J], Z[2], KEPT, RR, R, Q[J]),      ASRB 406
              LIST4(J, TJ),         LIST5(HVJ, HLJ),         LIST8(LJ), ASRB 407
              LIST6(NAMES[I], VJ, YJ, LJ, XJ),         LIST7(VJ, LJ),   ASRB 408
              LIST9(FOR I ~ 1 STEP 1 UNTIL N DO [NAMES[I], E[I]]) ;     ASRB 409
                                                                        ASRB 410
              WRITE(FIL1[PAGE]) ;                                       ASRB 411
              WRITE(FIL1, FRMTA, LIST1) ;                               ASRB 412
              IF TYPE = 2 THEN WRITE(FIL1,FRMTB3) ;                     ASRB 413
              IF TYPE = 1 THEN WRITE(FIL1,FRMTB2) ;                     ASRB 414
              IF TYPE = 0 THEN                                          ASRB 415
         BEGIN                                                          ASRB 416
              WRITE(FIL1,FRMTB1) ;  GO TO HED                           ASRB 417
         END  ;                                                         ASRB 418
              WRITE(FIL1,FRMTC1,LIST2) ;                                ASRB 419
              RCVR ~ RECOVERY ~ KEPT ;  WRITE(FIL1,FRMTC2,LIST2) ;      ASRB 420
HED:          WRITE(FIL1,FRMTD) ;                                       ASRB 421
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 422
         BEGIN                                                          ASRB 423
              R ~ RR ~ Z[1] ~ Z[2] ~ KEPT ~ 0.0 ;                       ASRB 424
              FOR U ~ 1 STEP 1 UNTIL OILS DO                            ASRB 425
              IF J = LFP[U] THEN R ~ LF[U] ;                            ASRB 426
              FOR U ~ 1 STEP 1 UNTIL GASES DO                           ASRB 427
              IF J = VFP[U] THEN RR ~ VF[U] ;                           ASRB 428
              FOR U ~ 1 STEP 1 UNTIL LIQDRWS DO                         ASRB 429
              IF J = LDP[U] THEN KEPT ~ LD[U] ;                         ASRB 430
              FOR U ~ 1 STEP 1 UNTIL VAPDRWS DO                         ASRB 431
              IF J = VDP[U] THEN Z[2] ~ VD[U] ;                         ASRB 432
              WRITE(FIL1,FRMTE,LIST3)                                   ASRB 433
         END  ;                                                         ASRB 434
              WRITE(FIL1, FRMTP) ;           WRITE(FIL1,FRMTQ,LIST9) ;  ASRB 435
              FOR J ~ 1 STEP 1 UNTIL M DO                               ASRB 436
         BEGIN                                                          ASRB 437
              HLJ ~ HL[J] ; HVJ ~ HV[J] ; TJ ~ T[J] ; WRITE(FIL1[PAGE]);ASRB 438
              WRITE(FIL1, FRMTF, LIST4) ;           WRITE(FIL1,FRMTG) ; ASRB 439
              WRITE(FIL1,FRMTH,LIST5) ;  WRITE(FIL1,FRMTI) ;            ASRB 440
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 441
         BEGIN                                                          ASRB 442
              LJ ~ LL[J,I] ;  XJ ~ LJ / L[J] ;  VJ ~ VV[J,I] ;          ASRB 443
              YJ ~ VJ / V[J] ;  WRITE(FIL1,FRMTJ,LIST6)                 ASRB 444
         END  ;                                                         ASRB 445
              VJ ~ V[J] ;  LJ ~ L[J] ;  WRITE(FIL1,FRMTK,LIST7)         ASRB 446
         END  ;                                                         ASRB 447
                                                                        ASRB 448
COMMENT       PRINT OUT FEEDS ;                                         ASRB 449
                                                                        ASRB 450
              FOR U ~ 1 STEP 1 UNTIL OILS DO                            ASRB 451
         BEGIN                                                          ASRB 452
              J ~ LFP[U] ;  TJ ~ TLF[U] ;  WRITE(FIL1[PAGE]) ;          ASRB 453
              WRITE(FIL1, FRMTL, LIST4) ;           WRITE(FIL1,FRMTG) ; ASRB 454
              HLJ ~ HLF[U] | LF[U] ;  HVJ ~ VJ ~ YJ ~ 0.0 ;             ASRB 455
              WRITE(FIL1,FRMTH,LIST5) ;          WRITE(FIL1,FRMTI) ;    ASRB 456
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 457
         BEGIN                                                          ASRB 458
              XJ ~ XLF[U,I] ;  LJ ~ LF[U] | XJ ;                        ASRB 459
              WRITE(FIL1,FRMTJ,LIST6)                                   ASRB 460
         END  ;                                                         ASRB 461
              LJ ~ LF[U] ;  VJ ~ 0.0 ;  WRITE(FIL1,FRMTK,LIST7)         ASRB 462
         END  ;                                                         ASRB 463
              FOR U ~ 1 STEP 1 UNTIL GASES DO                           ASRB 464
         BEGIN                                                          ASRB 465
              J ~ VFP[U] ;  TJ ~ TVF[U] ;  WRITE(FIL1[PAGE]) ;          ASRB 466
              WRITE(FIL1, FRMTM, LIST4) ;           WRITE(FIL1,FRMTG) ; ASRB 467
              HLJ ~ 0.0 ;  HVJ ~ HVF[U] | VF[U] ;  LJ ~ XJ ~ 0.0 ;      ASRB 468
              WRITE(FIL1,FRMTH,LIST5) ;  WRITE(FIL1,FRMTI) ;            ASRB 469
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 470
         BEGIN                                                          ASRB 471
              YJ ~ YVF[U,I] ;  VJ ~ VF[U] | YJ ;                        ASRB 472
              WRITE(FIL1,FRMTJ,LIST6)                                   ASRB 473
         END  ;                                                         ASRB 474
              VJ ~ VF[U] ;  LJ ~ 0.0 ;  WRITE(FIL1,FRMTK,LIST7)         ASRB 475
         END  ;                                                         ASRB 476
                                                                        ASRB 477
COMMENT       PRINT OUT SIDE STREAM DRAWS ;                             ASRB 478
                                                                        ASRB 479
              FOR U ~ 1 STEP 1 UNTIL VAPDRWS DO                         ASRB 480
         BEGIN                                                          ASRB 481
              HLJ ~ 0.0 ;  J ~ VDP[U] ;  TJ ~ TVD[U] ~ T[J] ;           ASRB 482
              HVJ ~ HVD[U] ~ HV[J] | VD[U] / V[J] ;                     ASRB 483
              WRITE(FIL1[PAGE]) ;  WRITE(FIL1, FRMTN, LIST4) ;          ASRB 484
              WRITE(FIL1, FRMTH, LIST5) ;                               ASRB 485
              WRITE(FIL1,FRMTI) ;  LJ ~ XJ ~ 0.0 ;                      ASRB 486
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 487
         BEGIN                                                          ASRB 488
              YJ ~ YVD[U,I] ~ VV[J,I] / V[J] ;  VJ ~ VD[U] | YJ ;       ASRB 489
              WRITE(FIL1,FRMTJ,LIST6)                                   ASRB 490
         END  ;                                                         ASRB 491
              VJ ~ VD[U] ;  WRITE(FIL1,FRMTK,LIST7)                     ASRB 492
         END  ;                                                         ASRB 493
              VJ ~ YJ ~ HVJ ~ 0.0 ;                                     ASRB 494
              FOR U ~ 1 STEP 1 UNTIL LIQDRWS DO                         ASRB 495
         BEGIN                                                          ASRB 496
              J ~ LDP[U] ;  HLJ ~ HLD[U] ~ HL[J] | LD[U]/L[J] ;         ASRB 497
              TJ ~ TLD[U] ~ T[J] ;  WRITE(FIL1[PAGE]) ;                 ASRB 498
              WRITE(FIL1, FRMTO, LIST4) ;                               ASRB 499
              WRITE(FIL1,FRMTH,LIST5) ;  WRITE(FIL1,FRMTI) ;            ASRB 500
              VJ ~ YJ ~ 0.0 ;                                           ASRB 501
              FOR I ~ 1 STEP 1 UNTIL N DO                               ASRB 502
         BEGIN                                                          ASRB 503
              XJ ~ XLD[U,I] ~ LL[J,I] / L[J] ;  LJ ~ LD[U] | XJ ;       ASRB 504
              WRITE(FIL1,FRMTJ,LIST6)                                   ASRB 505
         END  ;                                                         ASRB 506
              LJ ~ LD[U] ;  WRITE(FIL1,FRMTK,LIST7)                     ASRB 507
         END                                                            ASRB 508
         END                                                            ASRB 509
         END ABSACT ;                                                   ASRB 510
                                                                        TEST  72
              FILL HVC2[*] WITH   0,         6,             6          ;TEST  73
              FILL HVC3[1,*] WITH 0.0,      -1.1671875,    -1.4894278,  TEST  74
               -1.1559073,    -1.3616489,   -1.2883117,    -1.3995702  ;TEST  75
              FILL HVC3[2,*] WITH 0.0,      -1.2935283,    -2.9748991,  TEST  76
               -2.4260566,    -1.8061761,   -1.5492552,    -1.4278796  ;TEST  77
              FILL HVC4[1,*] WITH 0.0,       0.0,          -0.10587647, TEST  78
               -0.15860289,   -0.15248362,  -0.11896855,   -0.12173253 ;TEST  79
              FILL HVC4[2,*] WITH 0.0,       0.0,          -1.7678550,  TEST  80
               -1.8686137,   -1.2165188,    -0.67716468,   -0.61318349 ;TEST  81
              FILL HVC5[*] WITH 0.0,        -0.68264275,   -1.0872225,  TEST  82
                0.047697762,  -0.024306088, -0.0090858597, -0.001729015,TEST  83
               -0.0027521429, -2.2596067,    0.11921812,   -0.046204775,TEST  84
                0.014351991,  -0.023163497,  0.025471511,   0.013133272,TEST  85
                0.78097570,   -0.17094139,   0.098209577,  -0.044902667,TEST  86
                0.015565173,  -0.010983720,  0.013622787,   0.28893313, TEST  87
               -0.25370399,    0.0052142570, 0.071616235,   0.046127269,TEST  88
               -0.11245569,   -0.067557734, -2.5307361,    -0.039864924,TEST  89
               -0.12607210,    0.13765330,  -0.039550505,  0.0075315843,TEST  90
               -0.21354763,    2.2361128,   -0.36080301,   0.49691013,  TEST  91
               -0.41863117,   -0.076349532,  0.43597890,  -0.40507180,  TEST  92
                0.26338300,   0.0083735620, -0.016716827, -0.016240225, TEST  93
                0.10507060,   -0.12106818,   0.025741676 ;              TEST  94
              FILL HVC7[1,*] WITH 0.0,      -1.2729166,    -1.6239677,  TEST  95
               -1.5317194,    -1.4788939,   -1.4345722,    -1.5801600  ;TEST  96
              FILL HVC7[2,*] WITH 0.0,      -6.5718750,   -12.149681,   TEST  97
              -11.497100,    -11.065928,   -10.278189,    -10.667091   ;TEST  98
              FILL HVC8[1,*] WITH 0.0,       0.0,          -0.090954100,TEST  99
               -0.056326597,  -0.075512113, -0.071434341,  -0.063708461;TEST 100
              FILL HVC8[2,*] WITH 0.0,       0.0,         -41.217490,   TEST 101
              -22.312134,    -21.043415,   -20.693912,    -26.268748   ;TEST 102
              FILL HVC9[*]   WITH 0.0,      39.745103,      6.3744973,  TEST 103
              -0.11410040, -0.0036340168, -8.2198154@-5, -2.1580705@-5, TEST 104
                2.9001251@-6, -44.671381,    -6.7048192,  0.073453717,  TEST 105
               -0.0066641733,  0.0017309322, -9.824585@-5, 1.343254@-5, TEST 106
               41.351011,      4.5915409,   -0.029361153,  0.0018827038,TEST 107
               -0.0040032069,  7.6001770@-4, -9.750253@-5, -27.356096,  TEST 108
                0.97045128,   -0.28080321,   0.032749281,  0.0074142633,TEST 109
               -0.0017002214,  4.3344771@-4, 39.904305,    -4.4069227,  TEST 110
                0.52546214,   -0.092967052,  0.0057307519, 0.0034063759,TEST 111
               -1.2990048@-4, 46.719260,     8.5852725,    -0.62558932, TEST 112
               -0.011829404,  -0.041450650,  0.014043180, -9.8770289@-4,TEST 113
               -3.7232051,    -0.30920320,  -0.0049008792, 0.0011831959,TEST 114
                9.1663621@-4, -1.8376908@-4, 3.8674416@-5              ;TEST 115
              READ (FLIN, FRIN1, LIN1) ;                                TEST 116
              READ (FLIN, FRIN1, LIN1A) ;                               TEST 117
              FOR I ~ 1 STEP 1 UNTIL N DO                               TEST 118
         BEGIN                                                          TEST 119
              READ (FLIN, FRIN2, LIN2) ;                                TEST 120
              READ(FLIN, FRIN2A, LIN2A ) ;                              TEST 121
              READ(FLIN, FRIN2A, LIN2B) ;                               TEST 122
              IF NOT ((WATER) AND (I = N)) THEN                         TEST 123
              FOR U ~ 1 STEP 4 UNTIL 25 DO                              TEST 124
              READ(FLIN, FRIN2A, LIN2C )                                TEST 125
         END  ;                                                         TEST 126
              FOR J ~ 1 STEP 1 UNTIL OILS DO                            TEST 127
         BEGIN                                                          TEST 128
              READ (FLIN, FRIN3, LIN3) ;       READ (FLIN,FRIN3A,LIN3A) TEST 129
         END  ;                                                         TEST 130
              FOR J ~ 1 STEP 1 UNTIL GASES DO                           TEST 131
         BEGIN                                                          TEST 132
              READ (FLIN, FRIN3, LIN4) ;       READ (FLIN,FRIN3A,LIN4A) TEST 133
         END  ;                                                         TEST 134
              FOR J ~ 1 STEP 1 UNTIL LIQDRWS DO READ (FLIN,FRIN4,LIN5) ;TEST 135
              FOR J ~ 1 STEP 1 UNTIL VAPDRWS DO READ (FLIN,FRIN4,LIN6) ;TEST 136
              ABSACT (TYPE, ITER, M, P, N, OILS, GASES, VAPDRWS,        TEST 137
              LIQDRWS, NAMES, LFP, LF, XLF, TLF, VFP, VF, YVF, TVF,     TEST 138
              HVFM, LDP, LD, VDP, VD, Q, WATER, KEY, RCVR, E, V1, YV1,  TEST 139
              T1, HV1, LM, XLM, TM, HLM, XLD, TLD, HLD, YVD, TVD,       TEST 140
              HVD, RECOVERY)                                            TEST 141
         END  ;                                                         TEST 142
              GO TO START ;                                             TEST 143
ENDPROB:      ;                                                         TEST 144
         END .                                                          TEST 145
 LABEL  000000000FLIN   0010000001
   0  10  30   8   1   1   0   0   0   FALSE  0.75  @ 02  0.0   @ 00  0.0   @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
 0.35 @ 00 0.35 @ 00 0.35 @ 00 0.35 @ 00 0.35 @ 00 0.35 @ 00 0.35 @ 00 0.35 @ 00
    C1                        343.1                         673.1
   0.48407973@ 04  0.86105586@ 01  0.22874472@-02  0.66456101@-06 HVC1       CH4
   0.29218565@ 04  0.13904192@ 02 -0.21724234@-02  0.11627389@-05 HLC5   -   CH4
   0.15966963@ 03 -0.18614131@ 04  0.84954508@ 04 -0.19802467@ 05*CP#20000 C1  1
   0.24963880@ 05 -0.16218149@ 05  0.42658278@ 04 -0.78521050@ 03*CP#20000 C1  2
   0.92213509@ 04 -0.42156812@ 05  0.98402830@ 05 -0.12423939@ 06*CP#20000 C1  3
   0.80865825@ 05 -0.21312737@ 05  0.12485995@ 04 -0.14502591@ 05*CP#20000 C1  4
   0.66404332@ 05 -0.15518010@ 06  0.19614320@ 06 -0.12782316@ 06*CP#20000 C1  5
   0.33732023@ 05 -0.61317660@ 03  0.71202509@ 04 -0.32611418@ 05*CP#20000 C1  6
   0.76188815@ 05 -0.96250684@ 05  0.62685495@ 05 -0.16529954@ 05*CP#20000 C1  7
    C2                        549.69                        708.3
   0.90900849@ 04  0.12121772@ 02  0.72767200@-02 -0.20287102@-06 HVC1   -  C2H6
   0.37330894@ 04  0.20525908@ 02  0.72608871@-02 -0.52406469@-05 HLC5   -  C2H6
   0.48316040@ 02 -0.70378298@ 03  0.38581939@ 04 -0.10866799@ 05*CP# 5000 C2  1
   0.16603603@ 05 -0.13025065@ 05  0.41050208@ 04 -0.29929847@ 03*CP# 5000 C2  2
   0.42677506@ 04 -0.23505100@ 05  0.66623855@ 05 -0.10254466@ 06*CP# 5000 C2  3
   0.81156646@ 05 -0.25811161@ 05  0.54286080@ 03 -0.74356827@ 04*CP# 5000 C2  4
   0.41052431@ 05 -0.11670769@ 06  0.18021311@ 06 -0.14318856@ 06*CP# 5000 C2  5
   0.45748093@ 05 -0.28787791@ 03  0.39216850@ 04 -0.21702094@ 05*CP# 5000 C2  6
   0.61847762@ 05 -0.95723944@ 05  0.76257711@ 05 -0.24438408@ 05*CP# 5000 C2  7
    C3                        665.86                        617.4
   0.12493845@ 05  0.16452783@ 02  0.12352731@-01 -0.12255693@-05 HVC1   -  C3H8
   0.46205824@ 04  0.25737553@ 02  0.15111391@-01 -0.73387156@-05 HLC5   -  C3H8
   0.98394520@ 02 -0.14131624@ 04  0.77343068@ 04 -0.21917360@ 05*CP# 5000 C3  1
   0.33988235@ 05 -0.27311043@ 05  0.88847254@ 04 -0.54296680@ 03*CP# 5000 C3  2
   0.76823994@ 04 -0.42234210@ 05  0.12022665@ 06 -0.18727989@ 06*CP# 5000 C3  3
   0.15116042@ 06 -0.49341154@ 05  0.87703780@ 03 -0.12165055@ 05*CP# 5000 C3  4
   0.67149294@ 05 -0.19192601@ 06  0.30022436@ 06 -0.24339113@ 06*CP# 5000 C3  5
   0.79786537@ 05 -0.42805960@ 03  0.59335825@ 04 -0.32867401@ 05*CP# 5000 C3  6
   0.94266741@ 05 -0.14799351@ 06  0.12044690@ 06 -0.39641890@ 05*CP# 5000 C3  7
   IC4                        734.56                        529.1
   0.15009188@ 05  0.21334487@ 02  0.17513610@-01 -0.24812564@-05 HVC1 ISO-C4H10
   0.56378857@ 04  0.32111714@ 02  0.16887125@-01 -0.48987662@-05 HLC5 ISO-C4H10
  -0.14795526@ 03  0.18579370@ 04 -0.98731107@ 04  0.27043347@ 05*CP# 5000IC4  1
  -0.40199194@ 05  0.30789112@ 05 -0.95008845@ 04  0.47628510@ 03*CP# 5000IC4  2
  -0.59918219@ 04  0.31925121@ 05 -0.87518622@ 05  0.12971382@ 06*CP# 5000IC4  3
  -0.98675923@ 05  0.30170681@ 05 -0.49339930@ 03  0.63834073@ 04*CP# 5000IC4  4
  -0.34238686@ 05  0.94267059@ 05 -0.13956783@ 06  0.10541852@ 06*CP# 5000IC4  5
  -0.31835094@ 05  0.17674242@ 03 -0.23147318@ 04  0.12527393@ 05*CP# 5000IC4  6
  -0.34711909@ 05  0.51405016@ 05 -0.38545959@ 05  0.11471507@ 05*CP# 5000IC4  7
   NC4                        765.22                        550.7
   0.15675143@ 05  0.21864464@ 02  0.16089416@-01 -0.17931083@-05 HVC1 - N-C4H10
   0.56381802@ 04  0.32156941@ 02  0.18464268@-01 -0.66078112@-05 HLC5 - N-C4H10
  -0.14609524@ 03  0.17442830@ 04 -0.88504047@ 04  0.23172909@ 05*CP# 5000NC4  1
  -0.32904241@ 05  0.24047149@ 05 -0.70717229@ 04  0.51791307@ 03*CP# 5000NC4  2
  -0.60967471@ 04  0.30280354@ 05 -0.77393885@ 05  0.10670547@ 06*CP# 5000NC4  3
  -0.75224715@ 05  0.21217935@ 05 -0.63796025@ 03  0.75662458@ 04*CP# 5000NC4  4
  -0.36863956@ 05  0.92089775@ 05 -0.12320518@ 06  0.83414815@ 05*CP# 5000NC4  5
  -0.22305445@ 05  0.28216774@ 03 -0.33359564@ 04  0.16069058@ 05*CP# 5000NC4  6
  -0.39576618@ 05  0.51906262@ 05 -0.34121151@ 05  0.87318732@ 04*CP# 5000NC4  7
   IC5                        829.6                         483.0
   0.18635740@ 05  0.26220114@ 02  0.21312870@-01 -0.28625929@-05 HVC1 ISO-C5H12
   0.66009918@ 04  0.37564035@ 02  0.21591715@-01 -0.56235172@-05 HLC5 ISO-C5H12
  -0.18305484@ 03  0.21777317@ 04 -0.10945974@ 05  0.28287761@ 05*CP# 5000IC5  1
  -0.39608830@ 05  0.28559400@ 05 -0.82940554@ 04  0.56296145@ 03*CP# 5000IC5  2
  -0.65170005@ 04  0.31723596@ 05 -0.78882897@ 05  0.10528309@ 06*CP# 5000IC5  3
  -0.71581396@ 05  0.19388801@ 05 -0.52509215@ 03  0.58763885@ 04*CP# 5000IC5  4
  -0.26930887@ 05  0.61721460@ 05 -0.73163112@ 05  0.41517839@ 05*CP# 5000IC5  5
  -0.83467070@ 04  0.15132424@ 03 -0.15333466@ 04  0.61208267@ 04*CP# 5000IC5  6
  -0.11000576@ 05  0.72440636@ 04  0.17474641@ 04 -0.28406992@ 04*CP# 5000IC5  7
   NC5                        845.52                        489.5
   0.19066847@ 05  0.26819218@ 02  0.20174027@-01 -0.24417899@-05 HVC1 - N-C5H12
   0.66969614@ 04  0.38053567@ 02  0.22532208@-01 -0.66454438@-05 HLC5 - N-C5H12
  -0.10371977@ 03  0.10966453@ 04 -0.50722388@ 04  0.11754639@ 05*CP# 5000NC5  1
  -0.14127402@ 05  0.81845946@ 04 -0.16956204@ 04  0.24380242@ 03*CP# 5000NC5  2
  -0.21492877@ 04  0.78892828@ 04 -0.11607237@ 05  0.14031742@ 04*CP# 5000NC5  3
   0.11581226@ 05 -0.75607267@ 04 -0.10736680@ 03  0.10561029@ 03*CP# 5000NC5  4
   0.47752449@ 04 -0.28152337@ 05  0.65978843@ 05 -0.70069589@ 05*CP# 5000NC5  5
   0.27856453@ 05 -0.24754430@ 02  0.93370388@ 03 -0.75623480@ 04*CP# 5000NC5  6
   0.28002344@ 05 -0.53335958@ 05  0.50426510@ 05 -0.18653917@ 05*CP# 5000NC5  7
   OIL                       1020.0                         362.0
   0.37896887@ 05  0.5808404 @ 02  0.45822738@-01 -0.67897556@-05
   0.12295581@ 05  0.73970919@ 02  0.55131703@-01 -0.13660414@-04
   0.15966963@ 03 -0.18614131@ 04  0.84954508@ 04 -0.19802467@ 05*CP#20000 OIL 1
   0.24963880@ 05 -0.16218149@ 05  0.42658278@ 04 -0.79990650@ 03*CP#20000 OIL 2
   0.92213509@ 04 -0.42156812@ 05  0.98402830@ 05 -0.12423939@ 06*CP#20000 OIL 3
   0.80865825@ 05 -0.21312737@ 05  0.12485995@ 04 -0.14502591@ 05*CP#20000 OIL 4
   0.66404332@ 05 -0.15518010@ 06  0.19614320@ 06 -0.12782316@ 06*CP#20000 OIL 5
   0.33732023@ 05 -0.61317660@ 03  0.71202509@ 04 -0.32611418@ 05*CP#20000 OIL 6
   0.76188815@ 05 -0.96250684@ 05  0.62685495@ 05 -0.16529954@ 05*CP#20000 OIL 7
     1                     0.534     @ 03                     0.8       @ 02
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 1.0
    30                     0.1975    @ 04                     0.8       @ 02
            0.83                0.084               0.048
            0.009               0.017               0.008
            0.004               0.0
   0  10   8   8   2   2   1   1   0   FALSE  0.75  @ 02  0.0   @ 00  0.0   @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00-0.5  @ 06 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01
    C1                        343.1                         673.1
   0.48407973@ 04  0.86105586@ 01  0.22874472@-02  0.66456101@-06 HVC1       CH4
   0.29218565@ 04  0.13904192@ 02 -0.21724234@-02  0.11627389@-05 HLC5   -   CH4
   0.15966963@ 03 -0.18614131@ 04  0.84954508@ 04 -0.19802467@ 05*CP#20000 C1  1
   0.24963880@ 05 -0.16218149@ 05  0.42658278@ 04 -0.78521050@ 03*CP#20000 C1  2
   0.92213509@ 04 -0.42156812@ 05  0.98402830@ 05 -0.12423939@ 06*CP#20000 C1  3
   0.80865825@ 05 -0.21312737@ 05  0.12485995@ 04 -0.14502591@ 05*CP#20000 C1  4
   0.66404332@ 05 -0.15518010@ 06  0.19614320@ 06 -0.12782316@ 06*CP#20000 C1  5
   0.33732023@ 05 -0.61317660@ 03  0.71202509@ 04 -0.32611418@ 05*CP#20000 C1  6
   0.76188815@ 05 -0.96250684@ 05  0.62685495@ 05 -0.16529954@ 05*CP#20000 C1  7
    C2                        549.69                        708.3
   0.90900849@ 04  0.12121772@ 02  0.72767200@-02 -0.20287102@-06 HVC1   -  C2H6
   0.37330894@ 04  0.20525908@ 02  0.72608871@-02 -0.52406469@-05 HLC5   -  C2H6
   0.48316040@ 02 -0.70378298@ 03  0.38581939@ 04 -0.10866799@ 05*CP# 5000 C2  1
   0.16603603@ 05 -0.13025065@ 05  0.41050208@ 04 -0.29929847@ 03*CP# 5000 C2  2
   0.42677506@ 04 -0.23505100@ 05  0.66623855@ 05 -0.10254466@ 06*CP# 5000 C2  3
   0.81156646@ 05 -0.25811161@ 05  0.54286080@ 03 -0.74356827@ 04*CP# 5000 C2  4
   0.41052431@ 05 -0.11670769@ 06  0.18021311@ 06 -0.14318856@ 06*CP# 5000 C2  5
   0.45748093@ 05 -0.28787791@ 03  0.39216850@ 04 -0.21702094@ 05*CP# 5000 C2  6
   0.61847762@ 05 -0.95723944@ 05  0.76257711@ 05 -0.24438408@ 05*CP# 5000 C2  7
    C3                        665.86                        617.4
   0.12493845@ 05  0.16452783@ 02  0.12352731@-01 -0.12255693@-05 HVC1   -  C3H8
   0.46205824@ 04  0.25737553@ 02  0.15111391@-01 -0.73387156@-05 HLC5   -  C3H8
   0.98394520@ 02 -0.14131624@ 04  0.77343068@ 04 -0.21917360@ 05*CP# 5000 C3  1
   0.33988235@ 05 -0.27311043@ 05  0.88847254@ 04 -0.54296680@ 03*CP# 5000 C3  2
   0.76823994@ 04 -0.42234210@ 05  0.12022665@ 06 -0.18727989@ 06*CP# 5000 C3  3
   0.15116042@ 06 -0.49341154@ 05  0.87703780@ 03 -0.12165055@ 05*CP# 5000 C3  4
   0.67149294@ 05 -0.19192601@ 06  0.30022436@ 06 -0.24339113@ 06*CP# 5000 C3  5
   0.79786537@ 05 -0.42805960@ 03  0.59335825@ 04 -0.32867401@ 05*CP# 5000 C3  6
   0.94266741@ 05 -0.14799351@ 06  0.12044690@ 06 -0.39641890@ 05*CP# 5000 C3  7
   IC4                        734.56                        529.1
   0.15009188@ 05  0.21334487@ 02  0.17513610@-01 -0.24812564@-05 HVC1 ISO-C4H10
   0.56378857@ 04  0.32111714@ 02  0.16887125@-01 -0.48987662@-05 HLC5 ISO-C4H10
  -0.14795526@ 03  0.18579370@ 04 -0.98731107@ 04  0.27043347@ 05*CP# 5000IC4  1
  -0.40199194@ 05  0.30789112@ 05 -0.95008845@ 04  0.47628510@ 03*CP# 5000IC4  2
  -0.59918219@ 04  0.31925121@ 05 -0.87518622@ 05  0.12971382@ 06*CP# 5000IC4  3
  -0.98675923@ 05  0.30170681@ 05 -0.49339930@ 03  0.63834073@ 04*CP# 5000IC4  4
  -0.34238686@ 05  0.94267059@ 05 -0.13956783@ 06  0.10541852@ 06*CP# 5000IC4  5
  -0.31835094@ 05  0.17674242@ 03 -0.23147318@ 04  0.12527393@ 05*CP# 5000IC4  6
  -0.34711909@ 05  0.51405016@ 05 -0.38545959@ 05  0.11471507@ 05*CP# 5000IC4  7
   NC4                        765.22                        550.7
   0.15675143@ 05  0.21864464@ 02  0.16089416@-01 -0.17931083@-05 HVC1 - N-C4H10
   0.56381802@ 04  0.32156941@ 02  0.18464268@-01 -0.66078112@-05 HLC5 - N-C4H10
  -0.14609524@ 03  0.17442830@ 04 -0.88504047@ 04  0.23172909@ 05*CP# 5000NC4  1
  -0.32904241@ 05  0.24047149@ 05 -0.70717229@ 04  0.51791307@ 03*CP# 5000NC4  2
  -0.60967471@ 04  0.30280354@ 05 -0.77393885@ 05  0.10670547@ 06*CP# 5000NC4  3
  -0.75224715@ 05  0.21217935@ 05 -0.63796025@ 03  0.75662458@ 04*CP# 5000NC4  4
  -0.36863956@ 05  0.92089775@ 05 -0.12320518@ 06  0.83414815@ 05*CP# 5000NC4  5
  -0.22305445@ 05  0.28216774@ 03 -0.33359564@ 04  0.16069058@ 05*CP# 5000NC4  6
  -0.39576618@ 05  0.51906262@ 05 -0.34121151@ 05  0.87318732@ 04*CP# 5000NC4  7
   IC5                        829.6                         483.0
   0.18635740@ 05  0.26220114@ 02  0.21312870@-01 -0.28625929@-05 HVC1 ISO-C5H12
   0.66009918@ 04  0.37564035@ 02  0.21591715@-01 -0.56235172@-05 HLC5 ISO-C5H12
  -0.18305484@ 03  0.21777317@ 04 -0.10945974@ 05  0.28287761@ 05*CP# 5000IC5  1
  -0.39608830@ 05  0.28559400@ 05 -0.82940554@ 04  0.56296145@ 03*CP# 5000IC5  2
  -0.65170005@ 04  0.31723596@ 05 -0.78882897@ 05  0.10528309@ 06*CP# 5000IC5  3
  -0.71581396@ 05  0.19388801@ 05 -0.52509215@ 03  0.58763885@ 04*CP# 5000IC5  4
  -0.26930887@ 05  0.61721460@ 05 -0.73163112@ 05  0.41517839@ 05*CP# 5000IC5  5
  -0.83467070@ 04  0.15132424@ 03 -0.15333466@ 04  0.61208267@ 04*CP# 5000IC5  6
  -0.11000576@ 05  0.72440636@ 04  0.17474641@ 04 -0.28406992@ 04*CP# 5000IC5  7
   NC5                        845.52                        489.5
   0.19066847@ 05  0.26819218@ 02  0.20174027@-01 -0.24417899@-05 HVC1 - N-C5H12
   0.66969614@ 04  0.38053567@ 02  0.22532208@-01 -0.66454438@-05 HLC5 - N-C5H12
  -0.10371977@ 03  0.10966453@ 04 -0.50722388@ 04  0.11754639@ 05*CP# 5000NC5  1
  -0.14127402@ 05  0.81845946@ 04 -0.16956204@ 04  0.24380242@ 03*CP# 5000NC5  2
  -0.21492877@ 04  0.78892828@ 04 -0.11607237@ 05  0.14031742@ 04*CP# 5000NC5  3
   0.11581226@ 05 -0.75607267@ 04 -0.10736680@ 03  0.10561029@ 03*CP# 5000NC5  4
   0.47752449@ 04 -0.28152337@ 05  0.65978843@ 05 -0.70069589@ 05*CP# 5000NC5  5
   0.27856453@ 05 -0.24754430@ 02  0.93370388@ 03 -0.75623480@ 04*CP# 5000NC5  6
   0.28002344@ 05 -0.53335958@ 05  0.50426510@ 05 -0.18653917@ 05*CP# 5000NC5  7
   OIL                       1020.0                         362.0
   0.37896887@ 05  0.5808404 @ 02  0.45822738@-01 -0.67897556@-05
   0.12295581@ 05  0.73970919@ 02  0.55131703@-01 -0.13660414@-04
   0.15966963@ 03 -0.18614131@ 04  0.84954508@ 04 -0.19802467@ 05*CP#20000 OIL 1
   0.24963880@ 05 -0.16218149@ 05  0.42658278@ 04 -0.79990650@ 03*CP#20000 OIL 2
   0.92213509@ 04 -0.42156812@ 05  0.98402830@ 05 -0.12423939@ 06*CP#20000 OIL 3
   0.80865825@ 05 -0.21312737@ 05  0.12485995@ 04 -0.14502591@ 05*CP#20000 OIL 4
   0.66404332@ 05 -0.15518010@ 06  0.19614320@ 06 -0.12782316@ 06*CP#20000 OIL 5
   0.33732023@ 05 -0.61317660@ 03  0.71202509@ 04 -0.32611418@ 05*CP#20000 OIL 6
   0.76188815@ 05 -0.96250684@ 05  0.62685495@ 05 -0.16529954@ 05*CP#20000 OIL 7
     1                     0.430     @ 03                     0.8       @ 02
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 1.0
     4                     0.104     @ 03                     0.8       @ 02
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 1.0
     8                     0.1580    @ 04                     0.8       @ 02
            0.83                0.084               0.048
            0.009               0.017               0.004
            0.008               0.0
     6                     0.3950    @ 03                     0.8       @ 02
            0.83                0.084               0.048
            0.009               0.017               0.004
            0.008               0.0
     6                     0.120     @ 03
     4                     0.350     @ 03
   1  10   6   8   1   1   0   0   5   FALSE  0.75  @ 02  0.0   @ 00  0.8   @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01
    C1                        343.1                         673.1
   0.48407973@ 04  0.86105586@ 01  0.22874472@-02  0.66456101@-06 HVC1       CH4
   0.29218565@ 04  0.13904192@ 02 -0.21724234@-02  0.11627389@-05 HLC5   -   CH4
   0.15966963@ 03 -0.18614131@ 04  0.84954508@ 04 -0.19802467@ 05*CP#20000 C1  1
   0.24963880@ 05 -0.16218149@ 05  0.42658278@ 04 -0.78521050@ 03*CP#20000 C1  2
   0.92213509@ 04 -0.42156812@ 05  0.98402830@ 05 -0.12423939@ 06*CP#20000 C1  3
   0.80865825@ 05 -0.21312737@ 05  0.12485995@ 04 -0.14502591@ 05*CP#20000 C1  4
   0.66404332@ 05 -0.15518010@ 06  0.19614320@ 06 -0.12782316@ 06*CP#20000 C1  5
   0.33732023@ 05 -0.61317660@ 03  0.71202509@ 04 -0.32611418@ 05*CP#20000 C1  6
   0.76188815@ 05 -0.96250684@ 05  0.62685495@ 05 -0.16529954@ 05*CP#20000 C1  7
    C2                        549.69                        708.3
   0.90900849@ 04  0.12121772@ 02  0.72767200@-02 -0.20287102@-06 HVC1   -  C2H6
   0.37330894@ 04  0.20525908@ 02  0.72608871@-02 -0.52406469@-05 HLC5   -  C2H6
   0.48316040@ 02 -0.70378298@ 03  0.38581939@ 04 -0.10866799@ 05*CP# 5000 C2  1
   0.16603603@ 05 -0.13025065@ 05  0.41050208@ 04 -0.29929847@ 03*CP# 5000 C2  2
   0.42677506@ 04 -0.23505100@ 05  0.66623855@ 05 -0.10254466@ 06*CP# 5000 C2  3
   0.81156646@ 05 -0.25811161@ 05  0.54286080@ 03 -0.74356827@ 04*CP# 5000 C2  4
   0.41052431@ 05 -0.11670769@ 06  0.18021311@ 06 -0.14318856@ 06*CP# 5000 C2  5
   0.45748093@ 05 -0.28787791@ 03  0.39216850@ 04 -0.21702094@ 05*CP# 5000 C2  6
   0.61847762@ 05 -0.95723944@ 05  0.76257711@ 05 -0.24438408@ 05*CP# 5000 C2  7
    C3                        665.86                        617.4
   0.12493845@ 05  0.16452783@ 02  0.12352731@-01 -0.12255693@-05 HVC1   -  C3H8
   0.46205824@ 04  0.25737553@ 02  0.15111391@-01 -0.73387156@-05 HLC5   -  C3H8
   0.98394520@ 02 -0.14131624@ 04  0.77343068@ 04 -0.21917360@ 05*CP# 5000 C3  1
   0.33988235@ 05 -0.27311043@ 05  0.88847254@ 04 -0.54296680@ 03*CP# 5000 C3  2
   0.76823994@ 04 -0.42234210@ 05  0.12022665@ 06 -0.18727989@ 06*CP# 5000 C3  3
   0.15116042@ 06 -0.49341154@ 05  0.87703780@ 03 -0.12165055@ 05*CP# 5000 C3  4
   0.67149294@ 05 -0.19192601@ 06  0.30022436@ 06 -0.24339113@ 06*CP# 5000 C3  5
   0.79786537@ 05 -0.42805960@ 03  0.59335825@ 04 -0.32867401@ 05*CP# 5000 C3  6
   0.94266741@ 05 -0.14799351@ 06  0.12044690@ 06 -0.39641890@ 05*CP# 5000 C3  7
   IC4                        734.56                        529.1
   0.15009188@ 05  0.21334487@ 02  0.17513610@-01 -0.24812564@-05 HVC1 ISO-C4H10
   0.56378857@ 04  0.32111714@ 02  0.16887125@-01 -0.48987662@-05 HLC5 ISO-C4H10
  -0.14795526@ 03  0.18579370@ 04 -0.98731107@ 04  0.27043347@ 05*CP# 5000IC4  1
  -0.40199194@ 05  0.30789112@ 05 -0.95008845@ 04  0.47628510@ 03*CP# 5000IC4  2
  -0.59918219@ 04  0.31925121@ 05 -0.87518622@ 05  0.12971382@ 06*CP# 5000IC4  3
  -0.98675923@ 05  0.30170681@ 05 -0.49339930@ 03  0.63834073@ 04*CP# 5000IC4  4
  -0.34238686@ 05  0.94267059@ 05 -0.13956783@ 06  0.10541852@ 06*CP# 5000IC4  5
  -0.31835094@ 05  0.17674242@ 03 -0.23147318@ 04  0.12527393@ 05*CP# 5000IC4  6
  -0.34711909@ 05  0.51405016@ 05 -0.38545959@ 05  0.11471507@ 05*CP# 5000IC4  7
   NC4                        765.22                        550.7
   0.15675143@ 05  0.21864464@ 02  0.16089416@-01 -0.17931083@-05 HVC1 - N-C4H10
   0.56381802@ 04  0.32156941@ 02  0.18464268@-01 -0.66078112@-05 HLC5 - N-C4H10
  -0.14609524@ 03  0.17442830@ 04 -0.88504047@ 04  0.23172909@ 05*CP# 5000NC4  1
  -0.32904241@ 05  0.24047149@ 05 -0.70717229@ 04  0.51791307@ 03*CP# 5000NC4  2
  -0.60967471@ 04  0.30280354@ 05 -0.77393885@ 05  0.10670547@ 06*CP# 5000NC4  3
  -0.75224715@ 05  0.21217935@ 05 -0.63796025@ 03  0.75662458@ 04*CP# 5000NC4  4
  -0.36863956@ 05  0.92089775@ 05 -0.12320518@ 06  0.83414815@ 05*CP# 5000NC4  5
  -0.22305445@ 05  0.28216774@ 03 -0.33359564@ 04  0.16069058@ 05*CP# 5000NC4  6
  -0.39576618@ 05  0.51906262@ 05 -0.34121151@ 05  0.87318732@ 04*CP# 5000NC4  7
   IC5                        829.6                         483.0
   0.18635740@ 05  0.26220114@ 02  0.21312870@-01 -0.28625929@-05 HVC1 ISO-C5H12
   0.66009918@ 04  0.37564035@ 02  0.21591715@-01 -0.56235172@-05 HLC5 ISO-C5H12
  -0.18305484@ 03  0.21777317@ 04 -0.10945974@ 05  0.28287761@ 05*CP# 5000IC5  1
  -0.39608830@ 05  0.28559400@ 05 -0.82940554@ 04  0.56296145@ 03*CP# 5000IC5  2
  -0.65170005@ 04  0.31723596@ 05 -0.78882897@ 05  0.10528309@ 06*CP# 5000IC5  3
  -0.71581396@ 05  0.19388801@ 05 -0.52509215@ 03  0.58763885@ 04*CP# 5000IC5  4
  -0.26930887@ 05  0.61721460@ 05 -0.73163112@ 05  0.41517839@ 05*CP# 5000IC5  5
  -0.83467070@ 04  0.15132424@ 03 -0.15333466@ 04  0.61208267@ 04*CP# 5000IC5  6
  -0.11000576@ 05  0.72440636@ 04  0.17474641@ 04 -0.28406992@ 04*CP# 5000IC5  7
   NC5                        845.52                        489.5
   0.19066847@ 05  0.26819218@ 02  0.20174027@-01 -0.24417899@-05 HVC1 - N-C5H12
   0.66969614@ 04  0.38053567@ 02  0.22532208@-01 -0.66454438@-05 HLC5 - N-C5H12
  -0.10371977@ 03  0.10966453@ 04 -0.50722388@ 04  0.11754639@ 05*CP# 5000NC5  1
  -0.14127402@ 05  0.81845946@ 04 -0.16956204@ 04  0.24380242@ 03*CP# 5000NC5  2
  -0.21492877@ 04  0.78892828@ 04 -0.11607237@ 05  0.14031742@ 04*CP# 5000NC5  3
   0.11581226@ 05 -0.75607267@ 04 -0.10736680@ 03  0.10561029@ 03*CP# 5000NC5  4
   0.47752449@ 04 -0.28152337@ 05  0.65978843@ 05 -0.70069589@ 05*CP# 5000NC5  5
   0.27856453@ 05 -0.24754430@ 02  0.93370388@ 03 -0.75623480@ 04*CP# 5000NC5  6
   0.28002344@ 05 -0.53335958@ 05  0.50426510@ 05 -0.18653917@ 05*CP# 5000NC5  7
   OIL                       1020.0                         362.0
   0.37896887@ 05  0.5808404 @ 02  0.45822738@-01 -0.67897556@-05
   0.12295581@ 05  0.73970919@ 02  0.55131703@-01 -0.13660414@-04
   0.15966963@ 03 -0.18614131@ 04  0.84954508@ 04 -0.19802467@ 05*CP#20000 OIL 1
   0.24963880@ 05 -0.16218149@ 05  0.42658278@ 04 -0.79990650@ 03*CP#20000 OIL 2
   0.92213509@ 04 -0.42156812@ 05  0.98402830@ 05 -0.12423939@ 06*CP#20000 OIL 3
   0.80865825@ 05 -0.21312737@ 05  0.12485995@ 04 -0.14502591@ 05*CP#20000 OIL 4
   0.66404332@ 05 -0.15518010@ 06  0.19614320@ 06 -0.12782316@ 06*CP#20000 OIL 5
   0.33732023@ 05 -0.61317660@ 03  0.71202509@ 04 -0.32611418@ 05*CP#20000 OIL 6
   0.76188815@ 05 -0.96250684@ 05  0.62685495@ 05 -0.16529954@ 05*CP#20000 OIL 7
     1                     0.534     @ 03                     0.8       @ 02
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 1.0
     6                     0.1975    @ 04                     0.8       @ 02
            0.83                0.084               0.048
            0.009               0.017               0.004
            0.008               0.0
   2  15   8   9   1   1   0   0   7    TRUE  0.15  @ 03  0.2567@ 05  0.98  @ 00
            0.14405550@ 02            0.69365635@ 04            0.37992103@ 03
            0.61556551@ 01            0.66919779@ 04            0.42042673@ 03
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01 0.1  @ 01
 0.1  @ 01
    C1                        343.1                         673.1
   0.48407973@ 04  0.86105586@ 01  0.22874472@-02  0.66456101@-06 HVC1       CH4
   0.29218565@ 04  0.13904192@ 02 -0.21724234@-02  0.11627389@-05 HLC5   -   CH4
   0.15966963@ 03 -0.18614131@ 04  0.84954508@ 04 -0.19802467@ 05*CP#20000 C1  1
   0.24963880@ 05 -0.16218149@ 05  0.42658278@ 04 -0.78521050@ 03*CP#20000 C1  2
   0.92213509@ 04 -0.42156812@ 05  0.98402830@ 05 -0.12423939@ 06*CP#20000 C1  3
   0.80865825@ 05 -0.21312737@ 05  0.12485995@ 04 -0.14502591@ 05*CP#20000 C1  4
   0.66404332@ 05 -0.15518010@ 06  0.19614320@ 06 -0.12782316@ 06*CP#20000 C1  5
   0.33732023@ 05 -0.61317660@ 03  0.71202509@ 04 -0.32611418@ 05*CP#20000 C1  6
   0.76188815@ 05 -0.96250684@ 05  0.62685495@ 05 -0.16529954@ 05*CP#20000 C1  7
    C2                        549.69                        708.3
   0.90900849@ 04  0.12121772@ 02  0.72767200@-02 -0.20287102@-06 HVC1   -  C2H6
   0.37330894@ 04  0.20525908@ 02  0.72608871@-02 -0.52406469@-05 HLC5   -  C2H6
   0.48316040@ 02 -0.70378298@ 03  0.38581939@ 04 -0.10866799@ 05*CP# 5000 C2  1
   0.16603603@ 05 -0.13025065@ 05  0.41050208@ 04 -0.29929847@ 03*CP# 5000 C2  2
   0.42677506@ 04 -0.23505100@ 05  0.66623855@ 05 -0.10254466@ 06*CP# 5000 C2  3
   0.81156646@ 05 -0.25811161@ 05  0.54286080@ 03 -0.74356827@ 04*CP# 5000 C2  4
   0.41052431@ 05 -0.11670769@ 06  0.18021311@ 06 -0.14318856@ 06*CP# 5000 C2  5
   0.45748093@ 05 -0.28787791@ 03  0.39216850@ 04 -0.21702094@ 05*CP# 5000 C2  6
   0.61847762@ 05 -0.95723944@ 05  0.76257711@ 05 -0.24438408@ 05*CP# 5000 C2  7
    C3                        665.86                        617.4
   0.12493845@ 05  0.16452783@ 02  0.12352731@-01 -0.12255693@-05 HVC1   -  C3H8
   0.46205824@ 04  0.25737553@ 02  0.15111391@-01 -0.73387156@-05 HLC5   -  C3H8
   0.98394520@ 02 -0.14131624@ 04  0.77343068@ 04 -0.21917360@ 05*CP# 5000 C3  1
   0.33988235@ 05 -0.27311043@ 05  0.88847254@ 04 -0.54296680@ 03*CP# 5000 C3  2
   0.76823994@ 04 -0.42234210@ 05  0.12022665@ 06 -0.18727989@ 06*CP# 5000 C3  3
   0.15116042@ 06 -0.49341154@ 05  0.87703780@ 03 -0.12165055@ 05*CP# 5000 C3  4
   0.67149294@ 05 -0.19192601@ 06  0.30022436@ 06 -0.24339113@ 06*CP# 5000 C3  5
   0.79786537@ 05 -0.42805960@ 03  0.59335825@ 04 -0.32867401@ 05*CP# 5000 C3  6
   0.94266741@ 05 -0.14799351@ 06  0.12044690@ 06 -0.39641890@ 05*CP# 5000 C3  7
   IC4                        734.56                        529.1
   0.15009188@ 05  0.21334487@ 02  0.17513610@-01 -0.24812564@-05 HVC1 ISO-C4H10
   0.56378857@ 04  0.32111714@ 02  0.16887125@-01 -0.48987662@-05 HLC5 ISO-C4H10
  -0.14795526@ 03  0.18579370@ 04 -0.98731107@ 04  0.27043347@ 05*CP# 5000IC4  1
  -0.40199194@ 05  0.30789112@ 05 -0.95008845@ 04  0.47628510@ 03*CP# 5000IC4  2
  -0.59918219@ 04  0.31925121@ 05 -0.87518622@ 05  0.12971382@ 06*CP# 5000IC4  3
  -0.98675923@ 05  0.30170681@ 05 -0.49339930@ 03  0.63834073@ 04*CP# 5000IC4  4
  -0.34238686@ 05  0.94267059@ 05 -0.13956783@ 06  0.10541852@ 06*CP# 5000IC4  5
  -0.31835094@ 05  0.17674242@ 03 -0.23147318@ 04  0.12527393@ 05*CP# 5000IC4  6
  -0.34711909@ 05  0.51405016@ 05 -0.38545959@ 05  0.11471507@ 05*CP# 5000IC4  7
   NC4                        765.22                        550.7
   0.15675143@ 05  0.21864464@ 02  0.16089416@-01 -0.17931083@-05 HVC1 - N-C4H10
   0.56381802@ 04  0.32156941@ 02  0.18464268@-01 -0.66078112@-05 HLC5 - N-C4H10
  -0.14609524@ 03  0.17442830@ 04 -0.88504047@ 04  0.23172909@ 05*CP# 5000NC4  1
  -0.32904241@ 05  0.24047149@ 05 -0.70717229@ 04  0.51791307@ 03*CP# 5000NC4  2
  -0.60967471@ 04  0.30280354@ 05 -0.77393885@ 05  0.10670547@ 06*CP# 5000NC4  3
  -0.75224715@ 05  0.21217935@ 05 -0.63796025@ 03  0.75662458@ 04*CP# 5000NC4  4
  -0.36863956@ 05  0.92089775@ 05 -0.12320518@ 06  0.83414815@ 05*CP# 5000NC4  5
  -0.22305445@ 05  0.28216774@ 03 -0.33359564@ 04  0.16069058@ 05*CP# 5000NC4  6
  -0.39576618@ 05  0.51906262@ 05 -0.34121151@ 05  0.87318732@ 04*CP# 5000NC4  7
   IC5                        829.6                         483.0
   0.18635740@ 05  0.26220114@ 02  0.21312870@-01 -0.28625929@-05 HVC1 ISO-C5H12
   0.66009918@ 04  0.37564035@ 02  0.21591715@-01 -0.56235172@-05 HLC5 ISO-C5H12
  -0.18305484@ 03  0.21777317@ 04 -0.10945974@ 05  0.28287761@ 05*CP# 5000IC5  1
  -0.39608830@ 05  0.28559400@ 05 -0.82940554@ 04  0.56296145@ 03*CP# 5000IC5  2
  -0.65170005@ 04  0.31723596@ 05 -0.78882897@ 05  0.10528309@ 06*CP# 5000IC5  3
  -0.71581396@ 05  0.19388801@ 05 -0.52509215@ 03  0.58763885@ 04*CP# 5000IC5  4
  -0.26930887@ 05  0.61721460@ 05 -0.73163112@ 05  0.41517839@ 05*CP# 5000IC5  5
  -0.83467070@ 04  0.15132424@ 03 -0.15333466@ 04  0.61208267@ 04*CP# 5000IC5  6
  -0.11000576@ 05  0.72440636@ 04  0.17474641@ 04 -0.28406992@ 04*CP# 5000IC5  7
   NC5                        845.52                        489.5
   0.19066847@ 05  0.26819218@ 02  0.20174027@-01 -0.24417899@-05 HVC1 - N-C5H12
   0.66969614@ 04  0.38053567@ 02  0.22532208@-01 -0.66454438@-05 HLC5 - N-C5H12
  -0.10371977@ 03  0.10966453@ 04 -0.50722388@ 04  0.11754639@ 05*CP# 5000NC5  1
  -0.14127402@ 05  0.81845946@ 04 -0.16956204@ 04  0.24380242@ 03*CP# 5000NC5  2
  -0.21492877@ 04  0.78892828@ 04 -0.11607237@ 05  0.14031742@ 04*CP# 5000NC5  3
   0.11581226@ 05 -0.75607267@ 04 -0.10736680@ 03  0.10561029@ 03*CP# 5000NC5  4
   0.47752449@ 04 -0.28152337@ 05  0.65978843@ 05 -0.70069589@ 05*CP# 5000NC5  5
   0.27856453@ 05 -0.24754430@ 02  0.93370388@ 03 -0.75623480@ 04*CP# 5000NC5  6
   0.28002344@ 05 -0.53335958@ 05  0.50426510@ 05 -0.18653917@ 05*CP# 5000NC5  7
   OIL                       1020.0                         362.0
   0.37896887@ 05  0.5808404 @ 02  0.45822738@-01 -0.67897556@-05
   0.12295581@ 05  0.73970919@ 02  0.55131703@-01 -0.13660414@-04
  -0.63913800@ 01 -0.25115198@ 03  0.26152836@ 04 -0.10959084@ 05*CP# 5000NC10 1
   0.22746904@ 05 -0.22944245@ 05  0.89273270@ 04  0.12932001@ 03*CP# 5000NC10 2
  -0.84750691@ 03 -0.97208763@ 03  0.19054218@ 05 -0.56047726@ 05*CP# 5000NC10 3
   0.66561791@ 05 -0.28343977@ 05 -0.22656780@ 03  0.19075648@ 04*CP# 5000NC10 4
  -0.25396522@ 04 -0.16231953@ 05  0.63682320@ 05 -0.82830330@ 05*CP# 5000NC10 5
   0.36899066@ 05  0.10590165@ 03 -0.95123876@ 03  0.16287371@ 04*CP# 5000NC10 6
   0.62006918@ 04 -0.27600105@ 05  0.37157188@ 05 -0.16863828@ 05*CP# 5000NC10 7
   H20                       1165.0                        3206.0
   0.23303156@ 05  0.79278327@ 01  0.37993942@-03  0.19084349@-06 HVC1   -   H20
   0.35700447@ 04  0.17526054@ 02 -0.10883193@-02  0.72949013@-05 HLC5   -   H20
     1                     0.5555    @ 03                     0.358     @ 03
            0.021166            0.012473            0.023182
            0.011123            0.028438            0.012653
            0.027016            0.863940            0.0
     8                     0.36      @ 03                     0.358     @ 03
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 0.0                 1.0

