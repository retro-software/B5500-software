 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                          TEST   1
                                                                        TEST   2
COMMENT       TEST PROGRAM FOR ABSORBER PROCEDURE.                      TEST   3
              MARK N. LAYMAN AND A. PAUL OLESON                         TEST   4
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION        TEST   5
              FIRST RELEASE DATE 12-1-63 ;                              TEST   6
                                                                        TEST   7
INTEGER       ITER, M, N, OILS, GASES, TYPE, VAPDRWS, LIQDRWS, KEY, I,  TEST   8
              J, U ;                                                    TEST   9
                                                                        TEST  10
BOOLEAN       CALCPDC, WATER ;                                          TEST  11
                                                                        TEST  12
REAL          PVWC1, PVWC2, PVWC3, SOLWC1, SOLWC2, SOLWC3, P, RCVR,     TEST  13
              HVFM, V1, T1, HV1, LM, TM, HLM, RECOVERY ;                TEST  14
                                                                        TEST  15
FORMAT IN     FRIN1(9I4, L8, 3E12.4),           FRIN2(3E26.8) ;         TEST  16
FILE OUT      FIL1 1(1,15) ;                                            TEST  17
FILE IN       FLIN(1, 10) ;                                             TEST  18
                                                                        TEST  19
LIST          LIN1(TYPE, ITER, M, N, OILS, GASES, VAPDRWS, LIQDRWS, KEY,TEST  20
              WATER, P, HVFM, RCVR),                                    TEST  21
              LIN2(PVWC1, PVWC2, PVWC3),                                TEST  22
              LIN3(SOLWC1, SOLWC2, SOLWC3);                             TEST  23
LABEL         START, ENDPROB ;                                          TEST  24
                                                                        TEST  25
START:        READ (FLIN, FRIN1, LIN1)[ENDPROB] ;                       TEST  26
              IF WATER THEN                                             TEST  27
         BEGIN                                                          TEST  28
              READ(FLIN, FRIN2, LIN2) ;        READ(FLIN, FRIN2, LIN3)  TEST  29
         END  ;                                                         TEST  30
                                                                        TEST  31
         BEGIN                                                          TEST  32
ALPHA ARRAY   NAMES[0:N] ;                                              TEST  33
                                                                        TEST  34
INTEGER ARRAY LFP[0:OILS], VFP[0:GASES], LDP[0:LIQDRWS], VDP[0:VAPDRWS],TEST  35
              HVC2[0:2] ;                                               TEST  36
                                                                        TEST  37
ARRAY         HVC3, HVC4, HVC7, HVC8[0:2,0:6], HVC5, HVC9[0:49], TC,    TEST  38
              PC, Z, YV1, XLM[0:N], Q[0:M], LF, TLF[0:OILS], TLD, HLD,  TEST  39
              LD[0:LIQDRWS], TVD, HVD, VD[0:VAPDRWS], VF, TVF[0:GASES], TEST  40
              HVC1, HLC5[0:N,0:4], KC1[0:N,0:28], XLD[0:LIQDRWS,0:N],   TEST  41
              YVD[0:VAPDRWS,0:N], XLF[0:OILS,0:N], YVF[0:GASES,0:N],    TEST  42
              HLF[0:OILS], HVF[0:GASES] ;                               TEST  43
                                                                        TEST  44
COMMENT       GLOBAL ARRAYS FOR KNGAA PROCEDURE ;                       TEST  45
ARRAY         TR1S1[0:4], PDC[0:N,0:4], LOGP1[0:7] ;                    TEST  46
                                                                        TEST  47
COMMENT       GLOBAL ARRAYS FOR PHIPART PROCEDURE ;                     TEST  48
INTEGER ARRAY TADD, ADD[0:4], DEGREE[0:3] ;                             TEST  49
ARRAY         POLY[0:25], TT[0:3] ;                                     TEST  50
                                                                        TEST  51
COMMENT       GLOBAL ARRAYS FOR HVPCOR PROCEDURE ;                      TEST  52
ARRAY         TR1S3, TR1S4[0:2] ;                                       TEST  53
                                                                        TEST  54
COMMENT       GLOBAL ARRAYS FOR ABSORBER PROCEDURE ;                    TEST  55
ARRAY         L, V, T, HV, PHVT, HL, PHLT[0:M+1], K, S[0:M+1,0:N] ;     TEST  56
BOOLEAN ARRAY PRV[0:3] ;                                                TEST  57
                                                                        TEST  58
FORMAT IN     FRIN1(8E10.3),                                            TEST  59
              FRIN2(A6,2F30.2),                                         TEST  60
              FRIN2A(E17.8,3E16.8,X15),                                 TEST  61
              FRIN3(I6,2E35.8),     FRIN3A(3F20.6),     FRIN4(I6,E35.8);TEST  62
                                                                        TEST  63
LIST          LIN1(FOR J ~ 1 STEP 1 UNTIL M DO Q[J]),                   TEST  64
              LIN2(NAMES[I], TC[I], PC[I]),                             TEST  65
              LIN2A(FOR J ~ 1,2,3,4 DO HVC1[I,J]),                      TEST  66
              LIN2B(FOR J ~ 1,2,3,4 DO HLC5[I,J]),                      TEST  67
              LIN2C(FOR J ~ U STEP 1 UNTIL U+3 DO KC1[I,J]),            TEST  68
              LIN3(LFP[J], LF[J], TLF[J]),                              TEST  69
              LIN3A(FOR I ~ 1 STEP 1 UNTIL N DO XLF[J,I]),              TEST  70
              LIN4(VFP[J], VF[J], TVF[J]),                              TEST  71
              LIN4A(FOR I ~ 1 STEP 1 UNTIL N DO YVF[J,I]),              TEST  72
              LIN5(LDP[J], LD[J]),             LIN6(VDP[J], VD[J]) ;    TEST  73
                                                                        TEST  74
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
PROCEDURE     ABSORBER ( TYPE, ITER, M, P, N, OILS, GASES, VAPDRWS,     SORB   1
              LIQDRWS, NAMES, LFP, LF, XLF, TLF, VFP, VF, YVF, TVF,     SORB   2
              HVFM, LDP, LD, VDP, VD, Q, WATER, KEY, RCVR, V1, YV1,     SORB   3
              T1, HV1, LM, XLM, TM, HLM, XLD, TLD, HLD, YVD, TVD,       SORB   4
              HVD, RECOVERY) ;                                          SORB   5
                                                                        SORB   6
COMMENT       THEORETICAL TRAY CALCULATIONS FOR ABSORBERS AND STRIPPERS SORB   7
              WITH MULTIPLE FEEDS AND DRAWS.                            SORB   8
                                                                        SORB   9
              A. PAUL OLESON AND MARK N. LAYMAN                         SORB  10
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORP.              SORB  11
                                                                        SORB  12
              CARD SEQUENCE STARTS WITH SORB-0001.                      SORB  13
              FIRST RELEASE 12-1-63.                                    SORB  14
                                                                        SORB  15
                                                                        SORB  16
              THE INPUT PARAMETERS ARE                                  SORB  17
              TYPE - TYPE OF PROBLEM, INTEGER.  IF TYPE =               SORB  18
                     0 - CALCULATE PERFORMANCE                          SORB  19
                     1 - CALCULATE LEAN OIL FLOW TO OBTAIN SPECIFIED    SORB  20
                         RECOVERY OF KEY COMPONENT.                     SORB  21
                     2 - CALCULATE WET GAS OR STRIPPING MEDIA TO OBTAIN SORB  22
                         SPECIFIED STRIPPING OF KEY COMPONENT FROM FEED SORB  23
                         OIL.                                           SORB  24
              ITER - NUMBER OF ITERATIONS ALLOWED, INTEGER.             SORB  25
              M    - NUMBER OF THEORETICAL TRAYS, INTEGER.              SORB  26
              P    - PRESSURE, PSIA - REAL.                             SORB  27
              N    - NUMBER OF COMPONENTS, INTEGER.                     SORB  28
              OILS - NUMBER OF OIL FEEDS, INTEGER.                      SORB  29
              GASES - NUMBER OF GAS FEEDS, INTEGER.                     SORB  30
              VAPDRWS - NUMBER OF VAPOR DRAWS, INTEGER.                 SORB  31
              LIQDRWS - NUMBER OF LIQUID DRAWS, INTEGER.                SORB  32
              NAMES - NAMES OF COMPONENTS, ALPHA ARRAY[0:N].            SORB  33
              LFP  - NUMBER OF TRAYS TO WHICH OIL IS FED, INTEGER ARRAY SORB  34
                     [0:OILS].                                          SORB  35
              LF   - OIL FEED FLOWS, MOLS - REAL ARRAY[0:OILS].         SORB  36
              XLF  - COMPOSITION OF OIL FEEDS, MOL FRACTIONS -          SORB  37
                     REAL ARRAY[0:OILS,0:N].                            SORB  38
              TLF  - TEMPERATURES OF OIL FEEDS, F - REAL ARRAY[0:OILS]. SORB  39
              VFP  - NUMBERS OF TRAYS TO WHICH GAS IS FED, INTEGER ARRAYSORB  40
                     [0:GASES].                                         SORB  41
              VF   - GAS FEED FLOWS, MOLS - REAL ARRAY[0:GASES].        SORB  42
              YVF  - COMPOSITIONS OF GAS FEEDS, MOL FRACTIONS -         SORB  43
                     REAL ARRAY[0:GASES,0:N].                           SORB  44
              TVF  - GAS FEED TEMPERATURES, F - REAL ARRAY[0:GASES].    SORB  45
              HVFM - ENTHALPY OF STEAM IF USED, BTU/MOL - REAL.         SORB  46
              LDP  - NUMBER OF TRAYS FROM WHICH LIQUID SIDE STREAMS ARE SORB  47
                     DRAWN, INTEGER ARRAY[0:LIQDRWS].                   SORB  48
              LD   - SPECIFIED LIQUID DRAWS, MOLS - REAL ARRAY          SORB  49
                     [0:LIQDRWS].                                       SORB  50
              VDP  - NUMBER OF TRAYS FROM WHICH VAPOR SIDE STREAMS ARE  SORB  51
                     DRAWN, INTEGER ARRAY[0:VAPDRWS].                   SORB  52
              VD   - SPECIFIED VAPOR DRAWS, MOLS - REAL ARRAY[0:VAPDRWS]SORB  53
              Q    - HEAT REMOVED FROM EACH TRAY, BTU - REAL ARRAY[0:M].SORB  54
              WATER - BOOLEAN VARIABLE. TRUE IF PRESENT.                SORB  55
              KEY  - NUMBER OF KEY COMPONENT SPECIFIED, INTEGER.        SORB  56
              RCVR - SPECIFIED RECOVERY OF KEY COMPONENT, FRACTION.     SORB  57
                                                                        SORB  58
              THE OUTPUT PARAMETERS ARE                                 SORB  59
              V1   - FLOW OF DRY GAS, MOLS - REAL.                      SORB  60
              YV1  - COMPOSITION OF DRY GAS, MOL FRACTIONS -            SORB  61
                     REAL ARRAY[0:N].                                   SORB  62
              T1   - DRY GAS TEMPERATURE, F - REAL.                     SORB  63
              HV1  - ENTHALPY OF DRY GAS, BTU - REAL.                   SORB  64
              LM   - RICH OIL FLOW, MOLS - REAL.                        SORB  65
              XLM  - COMPOSITION OF RICH OIL, MOL FRACTIONS -           SORB  66
                     REAL ARRAY[0:N].                                   SORB  67
              TM   - RICH OIL TEMPERATURE, F - REAL.                    SORB  68
              HLM  - ENTHALPY OF RICH OIL, BTU - REAL.                  SORB  69
              XLD  - COMPOSITION OF SIDE STREAM LIQUID DRAWS,           SORB  70
                     MOL FRACTIONS - REAL ARRAY[0:LIQDRWS,0:N].         SORB  71
              TLD  - TEMPERATURES OF SIDE STREAM LIQUID DRAWS, F -      SORB  72
                     REAL ARRAY[0:LIQDRWS].                             SORB  73
              HLD  - ENTHALPIES OF SIDE STREAM LIQUID DRAWS, BTU -      SORB  74
                     REAL ARRAY[0:LIQDRWS].                             SORB  75
              YVD  - COMPOSITIONS OF SIDE STREAM VAPOR DRAWS,           SORB  76
                     MOL FRACTIONS, REAL ARRAY[0:VAPDRWS, 0:N].         SORB  77
              TVD  - TEMPERATURES OF SIDE STREAM VAPOR DRAWS, F -       SORB  78
                     REAL ARRAY[0:VAPDRWS].                             SORB  79
              HVD  - ENTHALPIES OF SIDE STREAM VAPOR DRAWS, BTU -       SORB  80
                     REAL ARRAY[0:VAPDRWS].                             SORB  81
              RECOVERY - CALCULATED RECOVERY OF KEY COMPONENT, FRACTION;SORB  82
                                                                        SORB  83
VALUE         ITER, M, P, N, OILS, TYPE, VAPDRWS, LIQDRWS, GASES, KEY,  SORB  84
              RCVR ;                                                    SORB  85
INTEGER       ITER, M, N, OILS, GASES, TYPE, VAPDRWS, LIQDRWS, KEY ;    SORB  86
BOOLEAN       WATER ;                                                   SORB  87
REAL          P, RCVR, V1, T1, HV1, LM, TM, HLM, RECOVERY, HVFM ;       SORB  88
ALPHA ARRAY   NAMES[0] ;                                                SORB  89
INTEGER ARRAY LFP, VFP, LDP, VDP[0] ;                                   SORB  90
ARRAY         LF, TLF, VF, TVF, LD, VD, Q, YV1[0], XLM, TLD, HLD, TVD,  SORB  91
              HVD[0], XLF, YVF, XLD, YVD[0,0] ;                         SORB  92
                                                                        SORB  93
         BEGIN                                                          SORB  94
INTEGER       I, J, U, COUNT, NUM ;                                     SORB  95
REAL          R, RR, TOLT, TOLL, ASPEC, SSPEC, KEPT, TRS1, TRS2, TRS3,  SORB  96
              TRS4, INTER, DELTA ;                                      SORB  97
                                                                        SORB  98
ARRAY         PVW, SOLW, TX, LP, Z, DA, DB, DC, DD, GAM[0:M+1], PKT,    SORB  99
              PKP, TRA[0:N], LL, VV[0:M+1,0:N] ;                        SORB 100
                                                                        SORB 101
BOOLEAN       MHBAL, HINGE ;                                            SORB 102
LABEL         REDO, FLOWS, HEAT, ACOM, ADJUST, SOLUTION, TRIDIAG, MASS, SORB 103
              KREMSER, ITERA, NOVUM, DOOR ;                             SORB 104
                                                                        SORB 105
                                                                        SORB 106
COMMENT       CALCULATE ENTHALPIES OF FEED STREAMS ;                    SORB 107
                                                                        SORB 108
REDO :        FOR J ~ 1 STEP 1 UNTIL OILS DO                            SORB 109
              HLIQ ( N, TLF[J], XLF[J,*], HLF[J], TRA, R) ;             SORB 110
              IF HVFM = 0.0 THEN U ~ GASES  ELSE                        SORB 111
         BEGIN                                                          SORB 112
              U ~ GASES - 1 ;                   HVF[GASES] ~ HVFM       SORB 113
         END  ;                                                         SORB 114
              PRV[1] ~ PRV[2] ~ PRV[3] ~ FALSE ;                        SORB 115
              FOR J ~ 1 STEP 1 UNTIL U DO                               SORB 116
              HVPCOR(N, TVF[J], P, YVF[J,*], PRV, HVF[J], R, TRS1, TRA);SORB 117
                                                                        SORB 118
COMMENT       ESTIMATE FLOW RATES AND TEMPERATURES ;                    SORB 119
                                                                        SORB 120
              FOR J ~ 1 STEP 1 UNTIL M DO V[J] ~ L[J] ~ 0.0 ;           SORB 121
              FOR J ~ 1 STEP 1 UNTIL OILS DO L[LFP[J]] ~ LF[J] ;        SORB 122
              FOR J ~ 1 STEP 1 UNTIL GASES DO V[VFP[J]] ~ VF[J] ;       SORB 123
              FOR J ~ 1 STEP 1 UNTIL LIQDRWS DO                         SORB 124
              L[LDP[J] + 1] ~ L[LDP[J] + 1] - LD[J] ;                   SORB 125
              FOR J ~ 1 STEP 1 UNTIL VAPDRWS DO                         SORB 126
              V[VDP[J] - 1] ~ V[VDP[J] - 1] - VD[J] ;                   SORB 127
              FOR J ~ M-1 STEP -1 UNTIL 1 DO V[J] ~ V[J + 1] + V[J] ;   SORB 128
              FOR J ~ 2 STEP 1 UNTIL M DO L[J] ~ L[J - 1] + L[J] ;      SORB 129
              IF V[M] = 0 THEN                                          SORB 130
         BEGIN                                                          SORB 131
              R ~ V[VFP[GASES]] | 0.01 ;                                SORB 132
              FOR J ~ VFP[GASES] + 1 STEP 1 UNTIL M DO                  SORB 133
         BEGIN                                                          SORB 134
              V[J] ~ R ;  L[J-1] ~ L[J-1] + R                           SORB 135
         END  ;                                                         SORB 136
              L[M] ~ L[M] + R                                           SORB 137
         END  ;                                                         SORB 138
              T1 ~ 0.0 ;                                                SORB 139
              FOR J ~ 1 STEP 1 UNTIL OILS DO T1 ~ TLF[J] + T1 ;         SORB 140
              FOR J ~ 1 STEP 1 UNTIL GASES DO T1 ~ TVF[J] + T1 ;        SORB 141
              T1 ~ T1 / ( OILS + GASES ) ;                              SORB 142
              FOR J ~ 1 STEP 1 UNTIL M DO T[J] ~ T1 ;                   SORB 143
              IF WATER THEN NUM ~ N - 1  ELSE NUM ~N ;                  SORB 144
                                                                        SORB 145
                                                                        SORB 146
COMMENT       ITERATIVE CALCULATION OF FLOWS AND TEMPERATURES ;         SORB 147
FLOWS:        FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 148
         BEGIN                                                          SORB 149
              IF WATER THEN                                             SORB 150
         BEGIN                                                          SORB 151
              PVW[J] ~ EXP(-PVWC2 / (PVWC3 + T[J]) + PVWC1) ;           SORB 152
              SOLW[J] ~ EXP(-SOLWC2 / (SOLWC3 + T[J]) + SOLWC1) ;       SORB 153
              K[J,N] ~ PVW[J] / (SOLW[J] | P)                           SORB 154
         END  ;                                                         SORB 155
              CALCPDC ~ ( COUNT = 0 ) ;  R ~ V[J] / L[J] ;              SORB 156
              IF (COUNT > 0) OR (J = 1) THEN                            SORB 157
              KNGAA (T[J],P,NUM,CALCPDC,FALSE,FALSE,K[J,*],PKT,PKP) ;   SORB 158
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 159
              IF COUNT = 0 THEN S[J,I] ~ K[1,I] | R                     SORB 160
              ELSE S[J,I] ~ K[J,I] | R                                  SORB 161
         END  ;                                                         SORB 162
                                                                        SORB 163
COMMENT       FORM MATRIX FOR COMPONENT MATERIAL BALANCES TO CALCULATE  SORB 164
              LIQUID FLOW RATES ;                                       SORB 165
                                                                        SORB 166
              FOR J ~ 1 STEP 1 UNTIL M DO DA[J] ~ Z[J] ~ - 1.0 ;        SORB 167
              DA[1] ~ Z[M] ~ 0.0 ;  MHBAL ~ FALSE ;                     SORB 168
              FOR J ~ 1 STEP 1 UNTIL LIQDRWS DO                         SORB 169
              DA[LDP[J] + 1] ~ LD[J] / L[LDP[J]] - 1.0 ;                SORB 170
              FOR J ~ 1 STEP 1 UNTIL VAPDRWS DO                         SORB 171
              Z[VDP[J] - 1] ~ VD[J] / V[VDP[J]] - 1.0 ;                 SORB 172
                                                                        SORB 173
MASS:         FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 174
         BEGIN                                                          SORB 175
              IF MHBAL THEN GO TO TRIDIAG ;                             SORB 176
              FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 177
         BEGIN                                                          SORB 178
              DB[J] ~ S[J,I] + 1.0 ;  DD[J] ~ 0.0 ;                     SORB 179
              DC[J] ~ S[J + 1, I] | Z[J]                                SORB 180
         END  ;                                                         SORB 181
              FOR J ~ 1 STEP 1 UNTIL OILS DO                            SORB 182
              DD[LFP[J]] ~ XLF[J,I] | LF[J] ;                           SORB 183
              FOR J ~ 1 STEP 1 UNTIL GASES DO                           SORB 184
              DD[VFP[J]] ~ YVF[J,I] | VF[J] + DD[VFP[J]] ;              SORB 185
                                                                        SORB 186
TRIDIAG :     GAM[1] ~ - DC[1] / DB[1] ;      TX[1] ~ DD[1] / DB[1] ;   SORB 187
              FOR J ~ 2 STEP 1 UNTIL M DO                               SORB 188
         BEGIN                                                          SORB 189
              INTER ~ GAM[J - 1] | DA[J] + DB[J] ;                      SORB 190
              GAM[J] ~ - DC[J] / INTER ;                                SORB 191
              TX[J] ~ ( - TX[J - 1] | DA[J] + DD[J] ) / INTER           SORB 192
         END  ;                                                         SORB 193
              FOR J ~ M STEP -1 UNTIL 2 DO                              SORB 194
              TX[J - 1] ~ GAM[J - 1] | TX[J] + TX[J - 1] ;              SORB 195
              IF MHBAL THEN GO TO HEAT ;                                SORB 196
              FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 197
              IF TX[J] > 0.0 THEN LL[J,I] ~ TX[J] ELSE LL[J,I] ~ 0.0    SORB 198
         END  ;                                                         SORB 199
              FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 200
         BEGIN                                                          SORB 201
              V[J] ~ L[J] ~ 0.0 ;                                       SORB 202
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 203
         BEGIN                                                          SORB 204
              L[J] ~ LL[J,I] + L[J] ;                                   SORB 205
              VV[J,I] ~ S[J,I] | LL[J,I] ;  V[J] ~ VV[J,I] + V[J]       SORB 206
         END                                                            SORB 207
         END  ;                                                         SORB 208
              V1 ~ V[1] ;          LM ~ L[M] ;                          SORB 209
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 210
         BEGIN                                                          SORB 211
              YV1[I] ~ VV[1,I] / V1 ;  XLM[I] ~ LL[M,I] / LM ;          SORB 212
         END  ;                                                         SORB 213
                                                                        SORB 214
COMMENT       THIS COMPLETES THE MATERIAL BALANCES.  THE HEAT BALANCES  SORB 215
              AND ADJUSTMENTS IN TRAY TEMPERATURES FOLLOW. ENTHALPIES   SORB 216
              AND SPECIFIC HEATS OF ALL STREAMS ARE CALCULATED FIRST ;  SORB 217
                                                                        SORB 218
              PRV[1] ~ TRUE ;                                           SORB 219
              FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 220
         BEGIN                                                          SORB 221
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 222
         BEGIN                                                          SORB 223
              TRA[I] ~ LL[J,I] / L[J] ;  PKT[I] ~ VV[J,I] / V[J]        SORB 224
         END  ;                                                         SORB 225
              HLIQ ( N, T[J], TRA, HL[J], PKP, PHLT[J] ) ;              SORB 226
              HVPCOR(N, T[J], P, PKT, PRV, HV[J], PHVT[J], R, PKP) ;    SORB 227
              HL[J] ~ HL[J] | L[J] ;  PHLT[J] ~ PHLT[J] | L[J] ;        SORB 228
              HV[J] ~ HV[J] | V[J] ;  PHVT[J] ~ PHVT[J] | V[J] ;        SORB 229
         END  ;                                                         SORB 230
              HL[0] ~ HV[M+1] ~ 0.0 ;  HV1 ~ HV[1] ;  HLM ~ HL[M] ;     SOR2 231
                                                                        SORB 232
              FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 233
         BEGIN                                                          SORB 234
              DA[J] ~ - PHLT[J-1] ;  DB[J] ~ PHLT[J] + PHVT[J] ;        SORB 235
              DC[J] ~ - PHVT[J+1] ;                                     SORB 236
              DD[J] ~ - HV[J+1] - HL[J-1] + Q[J] + HV[J] + HL[J]        SORB 237
         END  ;                                                         SORB 238
              FOR U ~ 1 STEP 1 UNTIL OILS DO                            SORB 239
         BEGIN                                                          SORB 240
              J ~ LFP[U] ;  DD[J] ~ -HLF[U] | LF[U] + DD[J]             SORB 241
         END  ;                                                         SORB 242
              FOR U ~ 1 STEP 1 UNTIL GASES DO                           SORB 243
         BEGIN                                                          SORB 244
              J ~ VFP[U] ;  DD[J] ~ - HVF[U] | VF[U] +  DD[J]           SORB 245
         END  ;                                                         SORB 246
                                                                        SORB 247
              FOR U ~ 1 STEP 1 UNTIL VAPDRWS DO                         SORB 248
         BEGIN                                                          SORB 249
              J ~ VDP[U] ;  R ~ VD[U] / V[J] ;                          SORB 250
              DC[J-1] ~ PHVT[J] | R + DC[J-1] ;                         SORB 251
              DD[J-1] ~ HV[J] | R + DD[J-1]                             SORB 252
         END  ;                                                         SORB 253
                                                                        SORB 254
              FOR U ~ 1 STEP 1 UNTIL LIQDRWS DO                         SORB 255
         BEGIN                                                          SORB 256
              J ~ LDP[U] ;  R ~ LD[U] / L[J] ;                          SORB 257
              DA[J+1] ~ PHLT[J] | R + DA[J+1] ;                         SORB 258
              DD[J+1] ~ HL[J] | R + DD[J+1]                             SORB 259
         END  ;                                                         SORB 260
              DA[1] ~ DC[M] ~ 0.0 ;                                     SORB 261
              MHBAL ~ TRUE ;           GO TO MASS ;                     SORB 262
                                                                        SORB 263
HEAT:         TOLT ~ 0.2 ;  TOLL ~ 0.001 ;                              SORB 264
                                                                        SORB 265
              FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 266
         BEGIN                                                          SORB 267
              IF ABS(TX[J]) > TOLT THEN GO TO ADJUST ;                  SORB 268
              IF LP[J] = 0.0 THEN GO TO ADJUST ;                        SORB 269
              IF ABS(LP[J] / L[J] - 1.0) > TOLL THEN GO TO ADJUST       SORB 270
         END  ;                                                         SORB 271
              IF TYPE = 0 THEN GO TO SOLUTION ;                         SORB 272
                                                                        SORB 273
COMMENT       ADJUST LEAN OIL RATE FOR SPECIFIED RECOVERY ;             SORB 274
                                                                        SORB 275
              TRS1 ~ M ;  KEPT ~ RCVR ;  HINGE ~ FALSE ;                SORB 276
                                                                        SORB 277
KREMSER :     TRS2 ~ 1.0 ;                                              SORB 278
                                                                        SORB 279
COMMENT       FIND ABSORPTION OR STRIPPING FACTOR SATISFYING            SORB 280
              THE KREMSER EQUATION. ENTER WITH TRS1 = NUMBER OF         SORB 281
              IDEAL STAGES AND KEPT = RECOVERY OF KEY COMPONENT.        SORB 282
              LEAVE WITH TRS2 = CALCULATED FACTOR ;                     SORB 283
                                                                        SORB 284
              TRS3 ~ TRS1 / (TRS1 + 1.0) ;                              SORB 285
              DELTA ~ (KEPT / TRS3) | 2.0 - 2.0 ;                       SORB 286
              IF DELTA > 0.0 THEN GO TO NOVUM ELSE TRS2 ~ 0.5 ;         SORB 287
                                                                        SORB 288
ITERA :       TRS4 ~ TRS2*(TRS1 + 1.0 ) ;                               SORB 289
              TRS3 ~ (TRS4 - TRS2) / (TRS4 - 1.0) ;                     SORB 290
              DELTA ~ (KEPT - TRS3) | (TRS4 - 1.0) / ((TRS1 + 1.0) |    SORB 291
              (1.0 - TRS3) | (TRS4 / TRS2) - 1.0) ;                     SORB 292
                                                                        SORB 293
NOVUM :       IF ABS(DELTA) < 0.00001 THEN GO TO DOOR ;                 SORB 294
              TRS2 ~ TRS2 + DELTA ;           GO TO ITERA ;             SORB 295
                                                                        SORB 296
DOOR :        IF HINGE THEN GO TO ACOM ;                                SORB 297
              R ~ RR ~ 0.0 ;                                            SORB 298
              FOR U ~ 1 STEP 1 UNTIL OILS DO                            SORB 299
              R ~ XLF[U,KEY] | LF[U] + R ;                              SORB 300
              FOR U ~ 1 STEP 1 UNTIL GASES DO                           SORB 301
              RR ~ YVF[U,KEY] | VF[U] + RR ;                            SORB 302
              IF TYPE = 1 THEN                                          SORB 303
         BEGIN                                                          SORB 304
              ASPEC ~ TRS2 ;               KEPT ~ (LL[M,KEY] - R) / RR  SORB 305
         END                                                            SORB 306
              ELSE                                                      SORB 307
         BEGIN                                                          SORB 308
              SSPEC ~ TRS2 ;               KEPT ~ (VV[1,KEY] - RR) / R  SORB 309
         END ;                                                          SORB 310
              IF ABS (KEPT - RCVR) < 0.001 THEN GO TO SOLUTION ;        SORB 311
              HINGE ~ TRUE ;           GO TO KREMSER ;                  SORB 312
                                                                        SORB 313
ACOM:         IF TYPE = 1 THEN                                          SORB 314
         BEGIN                                                          SORB 315
              LF[1] ~ LF[1] | ASPEC / TRS2 ;                            SORB 316
              GO TO REDO                                                SORB 317
         END  ;                                                         SORB 318
              VF[GASES] ~ VF[GASES] | SSPEC / TRS2 ;                    SORB 319
              GO TO REDO ;                                              SORB 320
                                                                        SORB 321
ADJUST:       FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 322
         BEGIN                                                          SORB 323
              T[J] ~ T[J] - TX[J] ;          LP[J] ~ L[J]               SORB 324
         END  ;                                                         SORB 325
              TM ~ T[M] ;              COUNT ~ COUNT + 1 ;              SORB 326
              IF COUNT > ITER THEN GO TO SOLUTION ;  GO TO FLOWS ;      SORB 327
                                                                        SORB 328
SOLUTION:BEGIN                                                          SORB 329
REAL          TJ, LJ, XJ, YJ, VJ, HLJ, HVJ ;                            SORB 330
LABEL         HED ;                                                     SORB 331
                                                                        SORB 332
FORMAT OUT    FRMTA(X20,"PRESSURE, PSIA =",E12.5,X5,"COUNT =",I3),      SORB 333
              FRMTB1(//X5,"CALCULATION OF PERFORMANCE"),                SORB 334
              FRMTB2(//X5,"CALCULATION OF OIL RATE"),                   SORB 335
              FRMTB3(//X5,"CALCULATION OF GAS FEED RATE"),              SORB 336
              FRMTC1(//X5,"SPECIFIED RECOVERY OF ",A6," =",E12.5),      SORB 337
              FRMTC2(//X5,"CALCULATED RECOVERY OF ",A6," =",E12.5),     SORB 338
              FRMTD(//X5,"TRAY",X4,"T[J]",X7,"V[J]",X7,"L[J]",X7,       SORB 339
                   "VD[J]",X6,"LD[J]",X6,"VF[J]",X6"LF[J]",X6,"Q[J]"),  SORB 340
              FRMTE(/X6,I2 ,8E11.4),  FRMTG(//X50,"VAPOR",X19,"LIQUID"),SORB 341
              FRMTF(////X30,"TRAY",I3,X5,"TEMPERATURE F = ",E12.4),     SORB 342
              FRMTH(/X30,"ENTHALPY, BTU",E15.5,X8,E15.5),               SORB 343
              FRMTI(/X30,"COMPONENT",X6,"MOLS",X10,"Y",X10,"MOLS",X10,  SORB 344
              "X"),           FRMTJ(X32,A6,X4,E12.5,E12.5,E13.5,E12.5), SORB 345
              FRMTK(//X30,"TOTALS",X6,E12.5,X13,E12.5),                 SORB 346
              FRMTL(////X30,"OIL TO TRAY ",I3,X5,                       SORB 347
              "TEMPERATURE F =",E12.5),                                 SORB 348
              FRMTM(////X30,"GAS TO TRAY ",I3,X5,                       SORB 349
              "TEMPERATURE F =",E12.5),                                 SORB 350
              FRMTN(////X30,"VAPOR DRAW FROM TRAY ",I2,X5,"TEMPERATURE",SORB 351
              " F =",E12.5),          FRMTO(////X30,"LIQUID DRAW FROM ",SORB 352
              "TRAY ",I2,X5,"TEMPERATURE F =",E12.5) ;                  SORB 353
                                                                        SORB 354
LIST          LIST1(P, COUNT),          LIST2(NAMES[KEY], RCVR),        SORB 355
              LIST3(J, T[J], V[J], L[J], Z[2], KEPT, RR, R, Q[J]),      SORB 356
              LIST4(J, TJ),         LIST5(HVJ, HLJ),         LIST8(LJ), SORB 357
              LIST6(NAMES[I], VJ, YJ, LJ, XJ),          LIST7(VJ, LJ) ; SORB 358
                                                                        SORB 359
              WRITE(FIL1[PAGE]) ;                                       SORB 360
              WRITE(FIL1, FRMTA, LIST1) ;                               SORB 361
              IF TYPE = 2 THEN WRITE(FIL1,FRMTB3) ;                     SORB 362
              IF TYPE = 1 THEN WRITE(FIL1,FRMTB2) ;                     SORB 363
              IF TYPE = 0 THEN                                          SORB 364
         BEGIN                                                          SORB 365
              WRITE(FIL1,FRMTB1) ;  GO TO HED                           SORB 366
         END  ;                                                         SORB 367
              WRITE(FIL1,FRMTC1,LIST2) ;                                SORB 368
              RCVR ~ RECOVERY ~ KEPT ;  WRITE(FIL1,FRMTC2,LIST2) ;      SORB 369
                                                                        SORB 370
HED:          WRITE(FIL1,FRMTD) ;                                       SORB 371
              FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 372
         BEGIN                                                          SORB 373
              R ~ RR ~ Z[1] ~ Z[2] ~ KEPT ~ 0.0 ;                       SORB 374
              FOR U ~ 1 STEP 1 UNTIL OILS DO                            SORB 375
              IF J = LFP[U] THEN R ~ LF[U] ;                            SORB 376
              FOR U ~ 1 STEP 1 UNTIL GASES DO                           SORB 377
              IF J = VFP[U] THEN RR ~ VF[U] ;                           SORB 378
              FOR U ~ 1 STEP 1 UNTIL LIQDRWS DO                         SORB 379
              IF J = LDP[U] THEN KEPT ~ LD[U] ;                         SORB 380
              FOR U ~ 1 STEP 1 UNTIL VAPDRWS DO                         SORB 381
              IF J = VDP[U] THEN Z[2] ~ VD[U] ;                         SORB 382
              WRITE(FIL1,FRMTE,LIST3)                                   SORB 383
         END  ;                                                         SORB 384
              FOR J ~ 1 STEP 1 UNTIL M DO                               SORB 385
         BEGIN                                                          SORB 386
              HLJ ~ HL[J] ;  HVJ ~ HV[J] ;  WRITE(FIL1[PAGE]) ;         SORB 387
              TJ ~ T[J] ; WRITE(FIL1,FRMTF,LIST4) ; WRITE(FIL1,FRMTG) ; SORB 388
              WRITE(FIL1,FRMTH,LIST5) ;  WRITE(FIL1,FRMTI) ;            SORB 389
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 400
         BEGIN                                                          SORB 401
              LJ ~ LL[J,I] ;  XJ ~ LJ / L[J] ;  VJ ~ VV[J,I] ;          SORB 402
              YJ ~ VJ / V[J] ;  WRITE(FIL1,FRMTJ,LIST6)                 SORB 403
         END  ;                                                         SORB 404
              VJ ~ V[J] ;  LJ ~ L[J] ;  WRITE(FIL1,FRMTK,LIST7)         SORB 405
         END  ;                                                         SORB 406
                                                                        SORB 407
COMMENT       PRINT OUT FEEDS ;                                         SORB 408
                                                                        SORB 409
              FOR U ~ 1 STEP 1 UNTIL OILS DO                            SORB 410
         BEGIN                                                          SORB 411
              J ~ LFP[U] ;  TJ ~ TLF[U] ;  WRITE(FIL1[PAGE]) ;          SORB 412
              WRITE(FIL1, FRMTL, LIST4) ;           WRITE(FIL1,FRMTG) ; SORB 413
              HLJ ~ HLF[U] | LF[U] ;  HVJ ~ VJ ~ YJ ~ 0.0 ;             SORB 414
              WRITE(FIL1,FRMTH,LIST5) ;          WRITE(FIL1,FRMTI) ;    SORB 415
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 416
         BEGIN                                                          SORB 417
              XJ ~ XLF[U,I] ;  LJ ~ LF[U] | XJ ;                        SORB 418
              WRITE(FIL1,FRMTJ,LIST6)                                   SORB 419
         END  ;                                                         SORB 420
              LJ ~ LF[U] ;  VJ ~ 0.0 ;  WRITE(FIL1,FRMTK,LIST7)         SORB 421
         END  ;                                                         SORB 422
                                                                        SORB 423
              FOR U ~ 1 STEP 1 UNTIL GASES DO                           SORB 424
         BEGIN                                                          SORB 425
              J ~ VFP[U] ;  TJ ~ TVF[U] ;  WRITE(FIL1[PAGE]) ;          SORB 426
              WRITE(FIL1, FRMTM, LIST4) ;           WRITE(FIL1,FRMTG) ; SORB 427
              HLJ ~ 0.0 ;  HVJ ~ HVF[U] | VF[U] ;  LJ ~ XJ ~ 0.0 ;      SORB 428
              WRITE(FIL1,FRMTH,LIST5) ;  WRITE(FIL1,FRMTI) ;            SORB 429
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 430
         BEGIN                                                          SORB 431
              YJ ~ YVF[U,I] ;  VJ ~ VF[U] | YJ ;                        SORB 432
              WRITE(FIL1,FRMTJ,LIST6)                                   SORB 433
         END  ;                                                         SORB 434
              VJ ~ VF[U] ;  LJ ~ 0.0 ;  WRITE(FIL1,FRMTK,LIST7)         SORB 435
         END  ;                                                         SORB 436
                                                                        SORB 437
COMMENT       PRINT OUT SIDE STREAM DRAWS ;                             SORB 438
                                                                        SORB 439
              FOR U ~ 1 STEP 1 UNTIL VAPDRWS DO                         SORB 440
         BEGIN                                                          SORB 441
              HLJ ~ 0.0 ;  J ~ VDP[U] ;  TJ ~ TVD[U] ~ T[J] ;           SORB 442
              HVJ ~ HVD[U] ~ HV[J] | VD[U] / V[J] ;                     SORB 443
              WRITE(FIL1[PAGE]) ;  WRITE(FIL1,FRMTN,LIST4) ;            SORB 444
              WRITE(FIL1, FRMTH, LIST5) ;                               SORB 445
              WRITE(FIL1,FRMTI) ;  LJ ~ XJ ~ 0.0 ;                      SORB 446
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 447
         BEGIN                                                          SORB 448
              YJ ~ YVD[U,I] ~ VV[J,I] / V[J] ;  VJ ~ VD[U] | YJ ;       SORB 449
              WRITE(FIL1,FRMTJ,LIST6)                                   SORB 450
         END  ;                                                         SORB 451
              VJ ~ VD[U] ;  WRITE(FIL1,FRMTK,LIST7)                     SORB 452
         END  ;                                                         SORB 453
              VJ ~ YJ ~ HVJ ~ 0.0 ;                                     SORB 454
                                                                        SORB 455
              FOR U ~ 1 STEP 1 UNTIL LIQDRWS DO                         SORB 456
         BEGIN                                                          SORB 457
              J ~ LDP[U] ;  HLJ ~ HLD[U] ~ HL[J] | LD[U]/L[J] ;         SORB 458
              TJ ~ TLD[U] ;  WRITE(FIL1[PAGE]) ;                        SORB 459
              WRITE(FIL1, FRMTO, LIST4) ;                               SORB 460
              WRITE(FIL1,FRMTH,LIST5) ;  WRITE(FIL1,FRMTI) ;            SORB 461
              VJ ~ YJ ~ 0.0 ;                                           SORB 462
              FOR I ~ 1 STEP 1 UNTIL N DO                               SORB 463
         BEGIN                                                          SORB 464
              XJ ~ XLD[U,I] ~ LL[J,I] / L[J] ;  LJ ~ LD[U] | XJ ;       SORB 465
              WRITE(FIL1,FRMTJ,LIST6)                                   SORB 466
         END  ;                                                         SORB 467
              LJ ~ LD[U] ;  WRITE(FIL1,FRMTK,LIST7)                     SORB 468
         END                                                            SORB 469
         END                                                            SORB 470
                                                                        SORB 471
         END ABSORBER ;                                                 SORB 472
                                                                        SORB 473
              FILL HVC2[*] WITH   0,         6,             6          ;TEST  75
              FILL HVC3[1,*] WITH 0.0,      -1.1671875,    -1.4894278,  TEST  76
               -1.1559073,    -1.3616489,   -1.2883117,    -1.3995702  ;TEST  77
              FILL HVC3[2,*] WITH 0.0,      -1.2935283,    -2.9748991,  TEST  78
               -2.4260566,    -1.8061761,   -1.5492552,    -1.4278796  ;TEST  79
              FILL HVC4[1,*] WITH 0.0,       0.0,          -0.10587647, TEST  80
               -0.15860289,   -0.15248362,  -0.11896855,   -0.12173253 ;TEST  81
              FILL HVC4[2,*] WITH 0.0,       0.0,          -1.7678550,  TEST  82
               -1.8686137,   -1.2165188,    -0.67716468,   -0.61318349 ;TEST  83
              FILL HVC5[*] WITH 0.0,        -0.68264275,   -1.0872225,  TEST  84
                0.047697762,  -0.024306088, -0.0090858597, -0.001729015,TEST  85
               -0.0027521429, -2.2596067,    0.11921812,   -0.046204775,TEST  86
                0.014351991,  -0.023163497,  0.025471511,   0.013133272,TEST  87
                0.78097570,   -0.17094139,   0.098209577,  -0.044902667,TEST  88
                0.015565173,  -0.010983720,  0.013622787,   0.28893313, TEST  89
               -0.25370399,    0.0052142570, 0.071616235,   0.046127269,TEST  90
               -0.11245569,   -0.067557734, -2.5307361,    -0.039864924,TEST  91
               -0.12607210,    0.13765330,  -0.039550505,  0.0075315843,TEST  92
               -0.21354763,    2.2361128,   -0.36080301,   0.49691013,  TEST  93
               -0.41863117,   -0.076349532,  0.43597890,  -0.40507180,  TEST  94
                0.26338300,   0.0083735620, -0.016716827, -0.016240225, TEST  95
                0.10507060,   -0.12106818,   0.025741676 ;              TEST  96
              FILL HVC7[1,*] WITH 0.0,      -1.2729166,    -1.6239677,  TEST  97
               -1.5317194,    -1.4788939,   -1.4345722,    -1.5801600  ;TEST  98
              FILL HVC7[2,*] WITH 0.0,      -6.5718750,   -12.149681,   TEST  99
              -11.497100,    -11.065928,   -10.278189,    -10.667091   ;TEST 100
              FILL HVC8[1,*] WITH 0.0,       0.0,          -0.090954100,TEST 101
               -0.056326597,  -0.075512113, -0.071434341,  -0.063708461;TEST 102
              FILL HVC8[2,*] WITH 0.0,       0.0,         -41.217490,   TEST 103
              -22.312134,    -21.043415,   -20.693912,    -26.268748   ;TEST 104
              FILL HVC9[*]   WITH 0.0,      39.745103,      6.3744973,  TEST 105
              -0.11410040, -0.0036340168, -8.2198154@-5, -2.1580705@-5, TEST 106
                2.9001251@-6, -44.671381,    -6.7048192,  0.073453717,  TEST 107
               -0.0066641733,  0.0017309322, -9.824585@-5, 1.343254@-5, TEST 108
               41.351011,      4.5915409,   -0.029361153,  0.0018827038,TEST 109
               -0.0040032069,  7.6001770@-4, -9.750253@-5, -27.356096,  TEST 110
                0.97045128,   -0.28080321,   0.032749281,  0.0074142633,TEST 111
               -0.0017002214,  4.3344771@-4, 39.904305,    -4.4069227,  TEST 112
                0.52546214,   -0.092967052,  0.0057307519, 0.0034063759,TEST 113
               -1.2990048@-4, 46.719260,     8.5852725,    -0.62558932, TEST 114
               -0.011829404,  -0.041450650,  0.014043180, -9.8770289@-4,TEST 115
               -3.7232051,    -0.30920320,  -0.0049008792, 0.0011831959,TEST 116
                9.1663621@-4, -1.8376908@-4, 3.8674416@-5              ;TEST 117
              READ (FLIN, FRIN1, LIN1) ;                                TEST 118
              FOR I ~ 1 STEP 1 UNTIL N DO                               TEST 119
         BEGIN                                                          TEST 120
              READ (FLIN, FRIN2, LIN2) ;                                TEST 121
              READ(FLIN, FRIN2A, LIN2A ) ;                              TEST 122
              READ(FLIN, FRIN2A, LIN2B) ;                               TEST 123
              IF NOT ((WATER) AND (I = N)) THEN                         TEST 124
              FOR U ~ 1 STEP 4 UNTIL 25 DO                              TEST 125
              READ(FLIN, FRIN2A, LIN2C )                                TEST 126
         END  ;                                                         TEST 127
              FOR J ~ 1 STEP 1 UNTIL OILS DO                            TEST 128
         BEGIN                                                          TEST 129
              READ (FLIN, FRIN3, LIN3) ;       READ (FLIN,FRIN3A,LIN3A) TEST 130
         END  ;                                                         TEST 131
              FOR J ~ 1 STEP 1 UNTIL GASES DO                           TEST 132
         BEGIN                                                          TEST 133
              READ (FLIN, FRIN3, LIN4) ;       READ (FLIN,FRIN3A,LIN4A) TEST 134
         END  ;                                                         TEST 135
              FOR J ~ 1 STEP 1 UNTIL LIQDRWS DO READ (FLIN,FRIN4,LIN5) ;TEST 136
              FOR J ~ 1 STEP 1 UNTIL VAPDRWS DO READ (FLIN,FRIN4,LIN6) ;TEST 137
              ABSORBER ( TYPE, ITER, M, P, N, OILS, GASES, VAPDRWS,     TEST 138
              LIQDRWS, NAMES, LFP, LF, XLF, TLF, VFP, VF, YVF, TVF,     TEST 139
              HVFM, LDP, LD, VDP, VD, Q, WATER, KEY, RCVR, V1, YV1,     TEST 140
              T1, HV1, LM, XLM, TM, HLM, XLD, TLD, HLD, YVD, TVD,       TEST 141
              HVD, RECOVERY)                                            TEST 142
         END  ;                                                         TEST 143
              GO TO START ;                                             TEST 144
ENDPROB:      ;                                                         TEST 145
         END .                                                          TEST 146
 LABEL  000000000FLIN   0010000001
   0  10   8   8   2   2   1   1   0   FALSE  0.75  @ 02  0.0   @ 00  0.0   @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.5  @ 06 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
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
     1                     0.430     @ 03                     0.8       @ 02
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 1.0
     4                     0.104     @ 03                     0.8       @ 02
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 1.0
     6                     0.395     @ 03                     0.8       @ 02
            0.83                0.084               0.048
            0.009               0.017               0.004
            0.008               0.0
     8                     0.158     @ 04                     0.8       @ 02
            0.83                0.084               0.048
            0.009               0.017               0.004
            0.008               0.0
     6                     0.120     @ 03
     4                     0.350     @ 03
   1  15   6   8   1   1   0   0   5   FALSE  0.75  @ 02  0.0   @ 00  0.8   @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
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
     1                     0.100     @ 04                     0.8       @ 02
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 1.0
     6                     0.1975    @ 04                     0.8       @ 02
            0.83                0.084               0.048
            0.009               0.017               0.004
            0.008               0.0
   0  10   6   8   1   1   0   0   0   FALSE  0.75  @ 02  0.0   @ 00  0.0   @ 00
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
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
   0  10   6   9   1   1   0   0   0    TRUE  0.15  @ 03  0.2567@ 05  0.0   @ 00
            0.14405550@ 02            0.69365635@ 04            0.37992103@ 03
            0.61556551@ 01            0.66919779@ 04            0.42042673@ 03
 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00 0.0  @ 00
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
     6                     0.25      @ 03                     0.358     @ 03
            0.0                 0.0                 0.0
            0.0                 0.0                 0.0
            0.0                 0.0                 1.0
