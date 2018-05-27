INTEGER ARRAY HVC2[0:2] ;
ARRAY         TR1S3, TR1S4[0:2], HVC5, HVC9[0:49], HVC3, HVC4, HVC7,
              HVC8[0:2, 0:6] ;
ARRAY         TC, PC[0:N], HVC1[0:N, 0:4] ;
              FILL HVC2[*] WITH   0,         6,             6          ;
              FILL HVC3[1,*] WITH 0.0,      -1.1671875,    -1.4894278,
               -1.1559073,    -1.3616489,   -1.2883117,    -1.3995702  ;
              FILL HVC3[2,*] WITH 0.0,      -1.2935283,    -2.9748991,
               -2.4260566,    -1.8061761,   -1.5492552,    -1.4278796  ;
              FILL HVC4[1,*] WITH 0.0,       0.0,          -0.10587647,
               -0.15860289,   -0.15248362,  -0.11896855,   -0.12173253 ;
              FILL HVC4[2,*] WITH 0.0,       0.0,          -1.7678550,
               -1.8686137,   -1.2165188,    -0.67716468,   -0.61318349 ;
              FILL HVC5[*] WITH 0.0,        -0.68264275,   -1.0872225,
                0.047697762,  -0.024306088, -0.0090858597, -0.001729015,
               -0.0027521429, -2.2596067,    0.11921812,   -0.046204775,
                0.014351991,  -0.023163497,  0.025471511,   0.013133272,
                0.78097570,   -0.17094139,   0.098209577,  -0.044902667,
                0.015565173,  -0.010983720,  0.013622787,   0.28893313,
               -0.25370399,    0.0052142570, 0.071616235,   0.046127269,
               -0.11245569,   -0.067557734, -2.5307361,    -0.039864924,
               -0.12607210,    0.13765330,  -0.039550505,  0.0075315843,
               -0.21354763,    2.2361128,   -0.36080301,   0.49691013,
               -0.41863117,   -0.076349532,  0.43597890,  -0.40507180,
                0.26338300,   0.0083735620, -0.016716827, -0.016240225,
                0.10507060,   -0.12106818,   0.025741676 ;
              FILL HVC7[1,*] WITH 0.0,      -1.2729166,    -1.6239677,
               -1.5317194,    -1.4788939,   -1.4345722,    -1.5801600  ;
              FILL HVC7[2,*] WITH 0.0,      -6.5718750,   -12.149681,
              -11.497100,    -11.065928,   -10.278189,    -10.667091   ;
              FILL HVC8[1,*] WITH 0.0,       0.0,          -0.090954100,
               -0.056326597,  -0.075512113, -0.071434341,  -0.063708461;
              FILL HVC8[2,*] WITH 0.0,       0.0,         -41.217490,
              -22.312134,    -21.043415,   -20.693912,    -26.268748   ;
              FILL HVC9[*]   WITH 0.0,      39.745103,      6.3744973,
              -0.11410040, -0.0036340168, -8.2198154@-5, -2.1580705@-5,
                2.9001251@-6, -44.671381,    -6.7048192,  0.073453717,
               -0.0066641733,  0.0017309322, -9.824585@-5, 1.343254@-5,
               41.351011,      4.5915409,   -0.029361153,  0.0018827038,
               -0.0040032069,  7.6001770@-4, -9.750253@-5, -27.356096,
                0.97045128,   -0.28080321,   0.032749281,  0.0074142633,
               -0.0017002214,  4.3344771@-4, 39.904305,    -4.4069227,
                0.52546214,   -0.092967052,  0.0057307519, 0.0034063759,
               -1.2990048@-4, 46.719260,     8.5852725,    -0.62558932,
               -0.011829404,  -0.041450650,  0.014043180, -9.8770289@-4,
               -3.7232051,    -0.30920320,  -0.0049008792, 0.0011831959,
                9.1663621@-4, -1.8376908@-4, 3.8674416@-5              ;
PROCEDURE     HVPCOR ( N, T, P, Z, CALCDHV, HV, PHVT, PHVP, PHVY ) ;    HVPC   1

COMMENT       THIS PROCEDURE COMPUTES THE ENTHALPY OF A GASEOUS MIXTURE HVPC   2
              AND, WITH SEPARATE OPTIONS, ITS PARTIAL DERIVATIVES WITH  HVPC   3
              RESPECT TO TEMPERATURE, PRESSURE AND MOL FRACTIONS .      HVPC   4

              MARK N. LAYMAN                                            HVPC   5
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       HVPC   6

              CARD SEQUENCE STARTS WITH HVPC-0001.                      HVPC   7
              FIRST RELEASE 3-20-63.                                    HVPC   8

              THE INPUT PARAMETERS ARE                                  HVPC   9
              N    - NUMBER OF COMPONENTS -INTEGER                      HVPC  10
              T    - TEMPERATURE, F - REAL                              HVPC  11
              P    - PRESSURE, PSIA - REAL                              HVPC  12
              Z    - COMPONENT MOLS OR MOL FRACTIONS - REAL ARRAY [0:N] HVPC  13
              CALCDHV - BOOLEAN ARRAY [0:3]. IF CALCDHV[1], [2] AND [3] HVPC  14
                        ARE TRUE, THEN PHVT, PHVP AND PHVY WILL BE      HVPC  15
                        COMPUTED, RESPECTIVELY .                        HVPC  16
              IN ADDITION, ARRAYS TC, PC, HVC1 - 5 AND HVC7 - 9 ARE     HVPC  17
              NON-LOCAL TO THIS PROCEDURE .                             HVPC  18


              THE OUTPUT PARAMETERS ARE                                 HVPC  19
              HV   - VAPOR ENTHALPY, BTU (Z = MOLS) OR BTU/MOL (Z = MOL HVPC  20
                     FRACTIONS) - REAL                                  HVPC  21
              PHVT - VAPOR HEAT CAPACITY, BTU/DEG.F (Z = MOLS) OR       HVPC  22
                     BTU/MOL/DEG.F (Z = MOL FRACTIONS) - REAL           HVPC  23
              PHVP - PARTIAL DERIVATIVE OF VAPOR ENTHALPY WITH RESPECT  HVPC  24
                     TO PRESSURE, BTU/PSIA (Z = MOLS) OR BTU/MOL/PSIA   HVPC  25
                     (Z = MOL FRACTIONS) - REAL                         HVPC  26
              PHVY - PARTIAL DERIVATIVE OF VAPOR ENTHALPY WITH RESPECT  HVPC  27
              TO MOL FRACTION, BTU/MOL - REAL ARRAY[0:N] ;              HVPC  28
VALUE         N, T, P ;                                                 HVPC  29

INTEGER       N ;                                                       HVPC  30

REAL          T, P, HV, PHVT, PHVP ;                                    HVPC  31

BOOLEAN ARRAY CALCDHV[0] ;                                              HVPC  32

ARRAY         Z[0], PHVY[0] ;                                           HVPC  33

         BEGIN                                                          HVPC  34

INTEGER       I ;                                                       HVPC  35

BOOLEAN       CALCPHVT, CALCPHVP, CALCPHVY ;                            HVPC  36

REAL          TPC, PPC, TPR, PPR, Y, SUMZ, SUMZ1, ZI, PHI, TRS1, TRS2,  HVPC  37
              B, CT, DT2, HO, THO, PHOT, DHT, T2 ;                      HVPC  39
ARRAY         DATA, DPHI[0:2] ;                                         HVPC  40
              TPC ~ PPC ~ THO ~ PHOT ~ SUMZ ~ DPHI[0] ~ 0.0 ;           HVPC  41
              FOR I ~ 1 STEP 1 UNTIL N DO SUMZ ~ Z[I] + SUMZ ;          HVPC  42
              SUMZ1 ~ 1.0 / SUMZ ;               CALCPHVY ~ CALCDHV[3]; HVPC  43
              CALCPHVT ~ CALCDHV[1] ;            CALCPHVP ~ CALCDHV[2]; HVPC  44
              FOR I ~ 1 STEP 1 UNTIL N DO                               HVPC  45
         BEGIN                                                          HVPC  46
              Y ~ Z[I] | SUMZ1 ;                                        HVPC  47
              TPC ~ TC[I] | Y + TPC ;            PPC ~ PC[I] | Y + PPC  HVPC  48
         END ;                                                          HVPC  49
              DATA[1] ~ TPR ~ (T + 459.6) / TPC ;        PPR ~ P / PPC ;HVPC  50
              IF CALCPHVY THEN CALCDHV[1] ~ CALCDHV[2] ~ TRUE ;         HVPC  51

COMMENT       TEST PSEUDO-REDUCED PRESSURE TO CHOOSE PROPER SURFACE FIT;HVPC  52

              IF PPR { 1.0 THEN                                         HVPC  53
         BEGIN                                                          HVPC  54
              DATA[2] ~ - LN(PPR) ;                                     HVPC  55

COMMENT       COMPUTE THE PRESSURE CORRECTION AND ITS PARTIAL           HVPC  56
              DERIVATIVES USING THE LOW RANGE FIT ;                     HVPC  57

              PHIPART (2,DATA,HVC3,HVC4,HVC5,HVC2,CALCDHV,  PHI,DPHI) ; HVPC  58
              PHI ~ EXP(PHI) ;                 DHT ~ - TPC | PHI ;      HVPC  59
              IF CALCPHVT THEN PHVT ~ - DPHI[1] | PHI ;                 HVPC  60
              IF CALCPHVP THEN PHVP ~ - DPHI[2] | DHT / P ;             HVPC  61
              IF CALCPHVY THEN                                          HVPC  62
         BEGIN                                                          HVPC  63
              TRS1 ~ (DPHI[1] | TPR - 1.0) | PHI ;                      HVPC  64
              TRS2 ~ - DPHI[2] | TPC | PHI / PPC                        HVPC  65
         END                                                            HVPC  66
         END                                                            HVPC  67
              ELSE                                                      HVPC  68
         BEGIN                                                          HVPC  69
              DATA[2] ~ PPR ;                                           HVPC  70

COMMENT       COMPUTE THE PRESSURE CORRECTION AND ITS PARTIAL           HVPC  71
              DERIVATIVES USING THE HIGH RANGE FIT ;                    HVPC  72

              PHIPART (2,DATA,HVC7,HVC8,HVC9,HVC2,CALCDHV,  PHI,DPHI) ; HVPC  73
              DHT ~ - TPC | PHI / PPR ;                                 HVPC  74
              IF CALCPHVT THEN PHVT ~ - DPHI[1] / PPR ;                 HVPC  75
              IF CALCPHVP THEN PHVP ~ - (DPHI[2] | TPC + DHT) / P ;     HVPC  76
              IF CALCPHVY THEN                                          HVPC  77
         BEGIN                                                          HVPC  78
              TRS1 ~ (DPHI[1] | TPR - PHI) / PPR ;                      HVPC  79
              TRS2 ~ (DPHI[2] / PPC - PHI / P) | TPC                    HVPC  80
         END                                                            HVPC  81
         END  ;                                                         HVPC  82
              T2 ~ T | T ;                                              HVPC  83
              FOR I ~ 1 STEP 1 UNTIL N DO                               HVPC  84
         BEGIN                                                          HVPC  85
              ZI ~ Z[I] ;                   B ~ HVC1[I,2] ;             HVPC  86
              CT ~ HVC1[I,3] | T ;          DT2 ~ HVC1[I,4] | T2 ;      HVPC  87

COMMENT       COMPUTE IDEAL GAS STATE ENTHALPY AND HEAT CAPACITY ;      HVPC  88

              HO ~ HVC1[I,1] + (B + CT + DT2) | T ;                     HVPC  89
              THO ~ HO | ZI + THO ;                                     HVPC  90
              IF CALCPHVT THEN                                          HVPC  91
              PHOT ~ (DT2 | 3.0 + CT | 2.0 + B) | ZI + PHOT ;           HVPC  92
              IF CALCPHVY THEN                                          HVPC  93
              PHVY[I] ~ TC[I] | TRS1 + PC[I] | TRS2 + HO                HVPC  94
         END ;                                                          HVPC  95

COMMENT       COMPUTE THE TOTAL PRESSURE CORRECTED ENTHALPY AND ITS     HVPC  96
              PARTIAL DERIVATIVES ;                                     HVPC  97

              HV ~ DHT | SUMZ + THO ;                                   HVPC  98
              IF CALCPHVT THEN PHVT ~ PHVT | SUMZ + PHOT ;              HVPC  99
              IF CALCPHVP THEN PHVP ~ PHVP | SUMZ ;                     HVPC 100
         END HVPCOR ;                                                   HVPC 101
