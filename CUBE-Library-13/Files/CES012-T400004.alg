         BEGIN                                                               100
COMMENT       PIPE STRESS ANALYSIS.                                          200
              VICTOR BERMAN                                                  300
              (PROFESSIONAL SERVICES, BURROUGHS CORP.                        400
              CARD SEQUENCE CODE STARTS PSAN-0001                            500
              FIRST RELEASE DATE :                                           600
              THE PROGRAM HANDLES PIPING SYSTEMS MADE OF ANY MATERIAL.       700
              THE PHYSICAL PROPERTIES OF THE FOLLOWING MATERIALS HAVE        800
              BEEN STORED IN THE COMPUTER MEMORY                             900
              LCS = LOW CARBON STEEL (LESS 3% CARBON)                       1000
              HCS = HIGH CARBON STEEL (MORE 3% CARBON)                      1100
              LCM = LOW-CHROME MOLYBDENUM STEEL (LESS 3% CHROME)            1200
              ICM = INTEMEDIATE MOLY-CHROME STEEL (5 CR MO THRU 9 CR MO)    1300
              AUS = AUSTENITIC STAINLESS STEEL (12 CR, 17 CR AND 27 CR)     1400
              MON = MONEL (67% NI, 30% CU)                                  1500
              KMO = K-MONEL (66% NI, 29% CU.AL.)                            1600
              ALU = ALUMINUM                                                1700
              GCI = GRAY CAST IRON                                          1800
              BRO = BRONZE                                                  1900
              WIR = WROUGHT IRON                                            2000
              KON = COPPER NICKEL (70% CU, 30% NI)                          2100
              FOR ANY OTHER MATERIAL, ITS MODULUS OF ELASTICITY AND         2200
              THERMAL COEFFICIENT OF EXPANSION MUST BE GIVEN                2300
              A NUMBER OF FITTINGS CAN BE USED TO CONNECT PIPES, THE        2400
              KIND OF FITTING MUST BE SPECIFIED BY THE PROPER IDENTIFIER    2500
              MIT = MITER BEND (CLOSELY OR WIDELY SPACED)                   2600
              WTE = WELDING TEE                                             2700
              RTE = REINFORCED FABRICATED TEE, WITH PAD OR SADDLE           2800
              TEE = UNREINFORCED FABRICATED TEE                             2900
              BWJ = BUTT-WELDED JOINT, REDUCER, OR WELDING NECK             3000
              DWF = DOUBLE WELDED SLIP-ON FLANGE                            3100
              FWJ = FILLET WELDED JOINT (SINGLE WELDED), SOCKER             3200
                    WELDED FLANGE, OR SINGLE WELDED SLIP-ON FLANGE          3300
              LJF = LAP JOINT FLANGE                                        3400
              SPJ = SCREWED PIPE JOINT OR SCREWED FLANGE  ;                 3500
BOOLEAN       TMP, SNPR, SKB ;                                              3600
REAL          A, AAX, AAY, AAZ, ALPH, AL1A, AL1B, AL1C, AL1D, AL1JA,        3700
              AL1PR, ANLR, B, C, CA, CAA, CAB, CALPH, CB, CBB, CH, CON,     3800
              CONEX, COSB, COSPH, COSSQB, CQ, CU, CU0,                      3900
              CV, CV0, DIH, DIRR, DIST, DIV1,HYP, K, KTA, KTE, KTE1,        4000
              KTX, KTY, KTZ, L, LLL, ME, P, PHALR, PHIR, PHIRA, PI, Q,      4100
              QX, RAD, RM, S, SA, SAAC2V, SABC2Q, SB, SBBC2U, SINB,         4200
              SINPH, SINSQB, SPA, SPAA, SPAB, SPB, SPBB, SX, SXL, TAH,      4300
              TAV, TB, TC, TE, U, UP0, UP00VP00, UX, UY, UZ, U0, U00V00,    4400
              V, VP0, VX, VY, VZ, V0, W, WX, WY, WZ, XA, XC, YA, YC, ZA,    4500
              KH, ZETAR, STRSMX, BET,                                       4600
              MB, MPB, MTOR, MBM, MPBM, MTORM, MBEN, MBENM, KTT,            4700
              SINM, SINN, SINC, COSN, COSM, COSC, AL1MR, STOR,              4800
              STORM, SBEN, SEQ, SBENM, SEQM,                                4900
              ZC, DB ;                                                      5000
INTEGER       BP, BPP, CNS, G, GG, GGG, GH, H, HH, HHH, I, II, III, J,      5100
              NL, PT,                                                       5200
              JJ, JJJ, KK, KKK, MPR, NAN, NB, NBAS, NM, NN, NNN, NPR,LP;    5300
ALPHA         LCS, HCS, LCM, ICM, AUS, SCS, MON, KMO, ALU, GCI, BRO,        5400
              BRS, WIR, KON, ANK, BRP, SKW, MIT, WTE, RTE, TEE, BWJ,        5500
              DWF, FWJ, LJF, SPJ, MI1, MI2, MI3, MI4, MI5, MI6, MI7,        5600
              FM, MT, FMMT, OPTION, LAST, LOP, PRK, RIG ;                   5700
FILE IN       FILE1(1,10) ;                                                 5800
FILE OUT      FILE2 1(1,15) ;                                               5900
REAL ARRAY    AL1R, ARMX, ARMY, ARMZ, CHKX, CHKY, CHKZ[0:150],              6000
              CNC, TEMP[0:60, 0:120], CNCT[0:120, 0:60],                    6100
              D[0:6, 0:6, 0:60], DE[0:6, 0:60], DEL[0:120],                 6200
              DI, EC, EPS, HAPHR, IN1, R[0:150],                            6300
              RFX, RFY, RFZ[0:60], MATE[0:280], BMAT, TR[0:6, 0:6],         6400
              T, TH, X, Y, Z[0:150], MF, TEMPR[0:460] ;                     6500
ALPHA ARRAY   TYPE, FIT, MAT[0:150] ;                                       6600
BOOLEAN ARRAY PRN[0:60] ;                                                   6700
ALPHA ARRAY   IDENT[0:12] ;                                                 6800
INTEGER ARRAY BASBR, BOL[0:60], BRA, PL, PREV[0:150],                       6900
              TRAV[0:20, 0:60], TRAVT[0:60, 0:20] ;                         7000
FORMAT        INA(A4, X1, I5),                                              7100
              INB(I4, X3, 6F8.3, I5, 2F6.2, 2(X1, A3)),                     7200
              INC(I4, X3, 6F8.3, I5, 3(X1, A3)),                            7300
              INC1(X9, 6F8.5),                                              7400
              INE1(16I5),                                                   7500
              IDENF(13A6),                                                  7600
              TI("MEMBER", X2, "ORIGIN", X7, "X", X12, "Y", X12, "Z",       7700
              X6, "O.D.", X7, "THK", X7, "R", X7, "TEMP", X7,               7800
              "METAL", X5, "TYPE", X3, "FITTING"////),                      7900
              TIA(X87, "MOD ELAS   EXP COEF"////),                          8000
              FMAT("EC = ", F6.2, "|10 @ 6 P.S.I.    EPS = ", F6.2,         8100
              "|10 @ -6 IN. / IN."///),                                     8200
              FOR3(I4, X4, I4, 3F13.3, 3F9.3, I9, X3, 3(X6, A3)///),        8300
              FOR4(I4, X4, I4, 3F13.3, 3F9.3, I9, 2F7.2,                    8400
              2(X5, A3)///),                                                8500
              FOR5("COORDINATES   X = ", F7.3, "  Y = ", F7.3,              8600
              "  Z = ", F7.3///),                                           8700
              SNGLR(X10, "SINGULAR MATRIX"),                                8800
              ERIN(X10, "INPUT DATA ERROR"),                                8900
              FOR9("CONSTRAIN", I7, 5I11/) ,                                9000
              FOR11(I4, X1, 6I11, 3I14/),                                   9100
              FOR12("MIDPOINT", X63, 3I14/),                                9200
              SPAC(////),                                                   9300
              TI10(X35, "FORCES,  MOMENTS  AND  STRESSES"////               9400
              X17, "FORCES IN POUNDS", X15,  "MOMENTS IN ",                 9500
              "FOOT-POUNDS", X16, "STRESSES IN P.S.I."///  "POINT", X9,     9600
              "FX", X9, "FY",X9, "FZ", X9, "MX", X9, "MY", X9, "MZ", X6,    9700
              "   BENDING     TORSIONAL     EQUIVALENT"///),                9800
              TI12(X35, "ANCHOR AND  CONSTRAINT  REACTIONS"//// X25,        9900
              "FORCES IN POUNDS", X25, "MOMENTS IN ",                      10000
              "FOOT-POUNDS"///"ANCHOR", X11, "FX", X9, "FY", X9, "FZ",     10100
              X9, "F", X9, "MX", X9, "MY", X9, "MZ", X9, "M" ///),         10200
              FOR14(I4, X4, 8I11/),                                        10300
              FOR16(X5, "FURNISH PROPERTIES OF PIPE NO.", I4),             10400
              FOR17(X35, "THAT WAS THE LAST PROBLEM"),                     10500
              FOR18(X5, "ERROR IN SPECIFYING MATERIAL OF PIPE NO.", I4),   10600
              FOR19(X35, "THE MAXIMUM STRESS IS", I7,                      10700
              " P.S.I. AT POINT", I4),                                     10800
              FOR20(X10, "LOOP AT MEMBER NO." I4, "CANNOT BE DEFINED"),    10900
              TI9(X30, "FREE EXPANSIONS OF INTERCONNECTED BRANCHES") ;     11000
LIST          DATAA(OPTION, TC),                                           11100
              DATAB(PREV[J], X[J], Y[J], Z[J], DI[J], T[J], R[J], TH[J],   11200
              EC[J], EPS[J], TYPE[J], FIT[J]),                             11300
              DATAC(PREV[J], X[J], Y[J], Z[J], DI[J], T[J], R[J], TH[J],   11400
              MAT[J], TYPE[J], FIT[J]),                                    11500
              DATAC1(FOR G ~ 1 STEP 1 UNTIL 6 DO [DE[G,I]]),               11600
              IDEN(FOR G ~ 0 STEP 1 UNTIL 12 DO [IDENT[G]]),               11700
              OUTB(J, PREV[J], X[J], Y[J], Z[J], DI[J], T[J], R[J],        11800
              TH[J], EC[J], EPS[J], TYPE[J], FIT[J]),                      11900
              OUTC(J, PREV[J], X[J], Y[J], Z[J], DI[J], T[J],              12000
              R[J], TH[J], MAT[J], TYPE[J], FIT[J]),                       12100
              OUT9(FOR KK ~ 14 STEP 1 UNTIL 16 DO [TEMPR[KK]],             12200
              FOR KK ~ 11 STEP 1 UNTIL 13 DO [TEMPR[KK]]),                 12300
              OUT11(HH, FOR KK ~ 14 STEP 1 UNTIL 16 DO [TEMPR[KK]],        12400
              FOR KK ~ 11 STEP 1 UNTIL 13 DO [TEMPR[KK]], SBEN,            12500
              STOR, SEQ),                                                  12600
              OUT12(SBENM, STORM, SEQM),                                   12700
             OUT14(TEMPR[50+I], FOR G ~ 1 STEP 1 UNTIL 8 DO                12800
              [CNCT[I,G]]),                                                12900
              OUTMAT(EC[J], EPS[J]),                                       13000
              OUT19(STRSMX, PT) ;                                          13100
PROCEDURE     JE(A, N, Z) ;                                                13200
VALUE         N ;                                                          13300
ARRAY         A[0, 0], Z[0] ;                                              13400
INTEGER       N ;                                                          13500
         BEGIN                                                             13600
REAL          Z1, D ;                                                      13700
INTEGER       I, J, K ;                                                    13800
              IF A[1,1] = 0 THEN                                           13900
         BEGIN                                                             14000
              FOR I ~ 1 STEP 1 WHILE A[I,1] = 0 DO                         14100
              IF I > N THEN WRITE(FILE2, SNGLR) ;                          14200
              FOR K ~ N + 1 STEP -1 UNTIL 1 DO                             14300
         BEGIN                                                             14400
              D ~ A[1,K] ;    A[1,K] ~ A[I,K] ;      A[I,K] ~ D            14500
         END                                                               14600
         END  ;                                                            14700
              FOR K ~ N + 1 STEP -1 UNTIL 1 DO                             14800
         BEGIN                                                             14900
              FOR I ~ 1 STEP 1 UNTIL N DO Z[I] ~ A[I, 1] ; Z1 ~ Z[1] ;     15000
              FOR J ~ 2 STEP 1 UNTIL K DO                                  15100
         BEGIN                                                             15200
              D ~ A[1, J] / Z1 ;                                           15300
              FOR I ~ 2 STEP 1 UNTIL N DO                                  15400
              A[I - 1, J - 1] ~ A[I, J] - Z[I] | D ;                       15500
              A[N, J - 1] ~ D                                              15600
         END                                                               15700
         END                                                               15800
         END JE ;                                                          15900
REAL PROCEDURE     ARCSIN(X1)  ;                                           16000
VALUE X1  ; REAL X1  ;                                                     16100
         BEGIN                                                             16200
REAL          PIC,MC,X,T,Z  ;                                              16300
LABEL         L1  ;                                                        16400
              IF ABS(X1) < 1@-6 THEN                                       16500
         BEGIN                                                             16600
              ARCSIN ~ X1  ;                                               16700
              GO TO L1                                                     16800
         END ;                                                             16900
              PIC ~ 0  ;                                                   17000
              MC ~ 1  ;                                                    17100
              Z ~ SIGN(X1)  ;                                              17200
              X ~ ABS(X1)  ;                                               17300
              IF X > 0.92387953231 THEN                                    17400
         BEGIN                                                             17500
              X ~ SQRT((1-X)/2)  ;                                         17600
              PIC ~ 1.57079632679  ;                                       17700
              MC ~ -2                                                      17800
         END                                                               17900
              ELSE                                                         18000
         BEGIN                                                             18100
              IF X > 0.70710678119 THEN                                    18200
         BEGIN                                                             18300
              X ~2|(X*2) - 1  ;                                            18400
              PIC ~ 0.785398163397  ;                                      18500
              MC ~ 0.5                                                     18600
         END ;                                                             18700
         END ;                                                             18800
              T ~ X*2   ;                                                  18900
              ARCSIN ~ ((0.364541120348 - 4.22649415434/                   19000
              (T - 8.66648256098 - 6.1228848016/                           19100
              (T - 3.2316720226 - 0.312873861283/                          19200
              (T - 1.63902626905 - 0.0268477822258/                        19300
              (T - 1.16535753774))))) | MC | X + PIC) | Z  ;               19400
L1:      END ;                                                             19500
REAL PROCEDURE     ARCCOS(X1)  ;                                           19600
VALUE X1  ; REAL X1  ;                                                     19700
         BEGIN                                                             19800
REAL          PIC,MC,X,T,Z,PI2  ;                                          19900
LABEL         L1  ;                                                        20000
              IF ABS(X1) < 1@-6 THEN                                       20100
         BEGIN                                                             20200
              ARCCOS ~ X1 + 1.57079632679  ;                               20300
              GO TO L1                                                     20400
         END ;                                                             20500
              PIC ~ 0  ;                                                   20600
              MC ~ 1  ;                                                    20700
              Z ~ SIGN(X1)  ;                                              20800
              X ~ ABS(X1)  ;                                               20900
              IF X > 0.92387953231 THEN                                    21000
         BEGIN                                                             21100
              X ~ SQRT((1-X)/2)  ;                                         21200
              PIC ~ 1.57079632679  ;                                       21300
              MC ~ -2                                                      21400
         END                                                               21500
              ELSE                                                         21600
         BEGIN                                                             21700
              IF X > 0.70710678119 THEN                                    21800
         BEGIN                                                             21900
              X ~2|(X*2) - 1  ;                                            22000
              PIC ~ 0.785398163397  ;                                      22100
              MC ~ 0.5  ;                                                  22200
         END ;                                                             22300
         END ;                                                             22400
              T ~ X*2  ;                                                   22500
              ARCCOS ~ ((0.364541120348 - 4.22649415434/                   22600
              (T - 8.66648256098 - 6.1228848016/                           22700
              (T - 3.2316720226 - 0.312873861283/                          22800
              (T - 1.63902626905 - 0.0268477822258/                        22900
              (T - 1.16535753774))))) | MC | X + PIC) | Z                  23000
              - 1.57079632679  ;                                           23100
L1:      END ;                                                             23200
PROCEDURE     FE ;                                                         23300
COMMENT       THE EXPANSIONS THAT WOULD TAKE PLACE IF EACH BRANCH WOULD    23400
              BE FREE TO EXPAND ARE CALCULATED.                            23500
              THE PRODUCT OF INERTIA, EI, HAS BEEN ASSUMED TO BE 1@8,      23600
              ON THE OTHER HAND, THE DATA OF THERMAL EXPANSION IS 1@6      23700
              TIMES THE ACTUAL VALUE. THEREFORE, CON = (1@8 / 1@6) / 144   23800
              OR 0.6944 ;                                                  23900
         BEGIN                                                             24000
REAL          CONRO ;                                                      24100
              CON ~ 0.6944444 ;                                            24200
              CONEX ~ 694444.44 ;                                          24300
              CONRO ~ CONEX ;                                              24400
              FOR G ~ 1 STEP 1 UNTIL 3 DO                                  24500
              FOR I ~ 1 STEP 1 UNTIL NB DO DE[G,I] ~ DE[G,I] | CONRO ;     24600
              FOR G ~ 4 STEP 1 UNTIL 6 DO                                  24700
              FOR I ~ 1 STEP 1 UNTIL NB DO DE[G,I] ~ DE[G,I] | CONEX ;     24800
              FOR J ~ 1 STEP 1 UNTIL NM DO                                 24900
         BEGIN                                                             25000
              I ~ ABS (BRA[J]) ;                                           25100
              KTE ~ (TH[J] - TC) | EPS[J] | CON ;                          25200
              DE[4,I] ~ DE[4,I] + X[J] | KTE ;                             25300
              DE[5,I] ~ DE[5,I] + Y[J] | KTE ;                             25400
              DE[6,I] ~ DE[6,I] + Z[J] | KTE                               25500
         END ;                                                             25600
              J ~ 6 | NBAS ;                                               25700
              FOR I ~ 1 STEP 1 UNTIL J DO DEL[I] ~ 0 ;                     25800
              FOR II ~ 1 STEP 1 UNTIL NBAS DO                              25900
              FOR I ~ 1 STEP 1 UNTIL NB DO                                 26000
              IF TRAV[II,I] ! 0 THEN                                       26100
         BEGIN                                                             26200
              HH ~ 6 | II - 6 ;                                            26300
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                  26400
              DEL[HH+G] ~ DEL[HH+G] + DE[G,I]                              26500
         END ;                                                             26600
         END FE ;                                                          26700
PROCEDURE     TRIGF ;                                                      26800
         BEGIN                                                             26900
              IF PL[J] = 1 OR PL[J] = 4 THEN                               27000
         BEGIN                                                             27100
              HYP ~ SQRT (AAY * 2 + AAZ * 2) ;                             27200
              ANLR ~ ARCSIN (AAZ / HYP) ;                                  27300
              IF AAY < 0 THEN ANLR ~ PI - ANLR                             27400
         END                                                               27500
              ELSE IF PL[J] = 2 THEN                                       27600
         BEGIN                                                             27700
              HYP ~ SQRT (AAX * 2 + AAZ * 2) ;                             27800
              ANLR ~ ARCSIN (AAX / HYP) ;                                  27900
              IF AAZ < 0 THEN ANLR ~ PI - ANLR                             28000
         END                                                               28100
              ELSE IF PL[J] = 3 THEN                                       28200
         BEGIN                                                             28300
              HYP ~ SQRT (AAX * 2 + AAY * 2) ;                             28400
              ANLR ~ ARCSIN (AAY / HYP) ;                                  28500
              IF AAX < 0 THEN ANLR ~ PI - ANLR                             28600
         END ;                                                             28700
         END TRIGF ;                                                       28800
PROCEDURE     ANGLE ;                                                      28900
         BEGIN                                                             29000
              IF R[J] ! 0 THEN                                             29100
         BEGIN                                                             29200
              AAX ~ X[J-1] ;      AAY ~ Y[J-1] ;                           29300
              AAZ ~ Z[J-1] ;                                               29400
              TRIGF ;                                                      29500
              AL1R[J-1] ~ ANLR ;                                           29600
              AAX ~ X[J+1] ;        AAY ~ Y[J+1] ;                         29700
              AAZ ~ Z[J+1] ;                                               29800
              TRIGF ;                                                      29900
              AL1R[J+1] ~ ANLR                                             30000
         END                                                               30100
              ELSE                                                         30200
         BEGIN                                                             30300
              AAX ~ X[J] ;         AAY ~ Y[J] ;         AAZ ~ Z[J] ;       30400
              TRIGF ;                                                      30500
              AL1R[J] ~ ANLR                                               30600
         END                                                               30700
         END ANGLE ;                                                       30800
PROCEDURE     SKEW ;                                                       30900
         BEGIN                                                             31000
LABEL         E250 ;                                                       31100
COMMENT       A ROTATION OF COORDINATE AXES MUST BE PERFORMED SO THAT      31200
              THE BRANCH SHOULD RUN ALONG THE Z AXIS ;                     31300
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                  31400
              FOR H ~ 1 STEP 1 UNTIL 6 DO TR[G,H] ~ 0 ;                    31500
              IF SKB THEN GO TO E250 ;                                     31600
              LLL ~ SQRT (X[JJJ] * 2 + Y[JJJ] * 2 + Z[JJJ] * 2) ;          31700
              WX ~ X[JJJ] / LLL ;         WY ~ Y[JJJ] / LLL ;              31800
              WZ ~ Z[JJJ] / LLL ;                                          31900
              VX ~ 1 / SQRT ((WX / WZ) * 2 + 1) ;                          32000
              VY ~ 0 ;         VZ ~ -VX | WX / WZ ;                        32100
              UX ~ -VZ | WY / (VX * 2 + VZ * 2) ;         UY ~ WX / VZ ;   32200
              UZ ~ -VX | UX / VZ ;                                         32300
E250 :        TEMP[1,1] ~ UX ;    TEMP[1,2] ~ UY ;     TEMP[1,3] ~ UZ ;    32400
              TEMP[2,1] ~ VX ;     TEMP[2,2] ~ VY ;     TEMP[2,3] ~ VZ ;   32500
              TEMP[3,1] ~ WX ;     TEMP[3,2] ~ WY ;     TEMP[3,3] ~ WZ ;   32600
              FOR G ~ 1, 2, 3 DO FOR H ~ 1, 2, 3 DO                        32700
              TR[G+3,H+3] ~ TR[G,H] ~ TEMP[G,H]                            32800
         END SKEW ;                                                        32900
PROCEDURE     SKEB ;                                                       33000
         BEGIN                                                             33100
LABEL         EA, EB ;                                                     33200
              TEMPR[1] ~ X[J-1] ;      TEMPR[2] ~ Y[J-1] ;                 33300
              TEMPR[3] ~ Z[J-1] ;      TEMPR[4] ~ X[J+1] ;                 33400
              TEMPR[5] ~ Y[J+1] ;      TEMPR[6] ~ Z[J+1] ;                 33500
              KTE ~ 0 ;                                                    33600
              FOR G ~ 1, 2, 3 DO KTE ~ TEMPR[G] * 2 + KTE ;                33700
              KTE ~ SQRT (KTE) ;                                           33800
              FOR G ~ 1, 2, 3 DO TEMPR[G] ~ TEMPR[G] / KTE ;    KTE ~ 0;   33900
              FOR G ~ 4, 5, 6 DO KTE ~ TEMPR[G] * 2 + KTE ;                34000
              KTE ~ SQRT (KTE) ;                                           34100
              FOR G ~ 4, 5, 6 DO TEMPR[G] ~ TEMPR[G] / KTE ; TMP ~ TRUE;   34200
              FOR G ~ 1 STEP 1 UNTIL 6 DO IF TEMPR[G] = 0 THEN             34300
         BEGIN                                                             34400
              GG ~ (G + 1) MOD 3 ;      IF GG = 0 THEN GG ~ 3 ;            34500
              GGG ~ (G + 2 ) MOD 3 ;      IF GGG =0 THEN GGG ~ 3 ;         34600
              TMP ~ FALSE ;      GO TO EA                                  34700
         END ;                                                             34800
              IF TMP THEN GO TO EB ;                                       34900
EA :          IF TEMPR[GG] = 0 THEN                                        35000
         BEGIN                                                             35100
              TEMPR[20+GG] ~ 10.0 ;      TEMPR[20+GGG] ~ 0                 35200
         END                                                               35300
              ELSE IF TEMPR[GGG] = 0 THEN                                  35400
         BEGIN                                                             35500
              TEMPR[20+GGG] ~ 10.0 ;      TEMPR[20+GG] ~ 0                 35600
         END                                                               35700
              ELSE                                                         35800
         BEGIN                                                             35900
              TEMPR[20+GG] ~ 10.0 ;                                        36000
              TEMPR[20+GGG] ~ -10.0 | TEMPR[GGG]/ TEMPR[GG]                36100
         END ;                                                             36200
              IF G > 3 THEN H ~ -3 ELSE H ~ 3 ;     KTE ~ 0 ;              36300
              TEMPR[20+G]   ~ -(TEMPR[GG+H] | TEMPR[20+GG] +               36400
              TEMPR[GGG+H] | TEMPR[20+GGG]) / TEMPR[G+H] ;                 36500
              FOR G ~ 21, 22, 23 DO KTE ~ TEMPR[G] * 2 + KTE ;             36600
              KTE ~ SQRT (KTE) ;                                           36700
              FOR G ~ 21, 22, 23 DO TEMPR[G] ~ TEMPR[G] / KTE ;            36800
              UX ~ TEMPR[21];     UY ~ TEMPR[22] ;      UZ ~ TEMPR[23] ;   36900
EB :          WX ~ TEMPR[4] ;      WY ~ TEMPR[5] ;      WZ ~ TEMPR[6] ;    37000
              IF TMP THEN                                                  37100
         BEGIN                                                             37200
              KTX ~ TEMPR[1] ;    KTY ~ TEMPR[2] ;     KTZ ~ TEMPR[3] ;    37300
              KTE ~ WY | KTZ - WZ | KTY ;                                  37400
              UY ~ 10 | (-WX | KTZ + WZ | KTX ) / KTE ;                    37500
              UZ ~10 | (-WY | KTX + WX | KTY) / KTE ;                      37600
              LLL ~ SQRT (100.0 + UY * 2 + UZ * 2) ;                       37700
              UX ~10 / LLL ;      UY ~ UY / LLL ;      UZ ~ UZ / LLL       37800
         END ;                                                             37900
              VX ~ WY | UZ - WZ | UY ;                                     38000
              VY ~ WZ | UX - WX | UZ ;                                     38100
              VZ ~ WX | UY - WY | UX ;                                     38200
              SKB ~ TRUE ;      SKEW ;      SKB ~ FALSE ;                  38300
         END SKEB ;                                                        38400
PROCEDURE     TRANSF ;                                                     38500
         BEGIN                                                             38600
              TEMPR[1] ~ X[J+G] ;      TEMPR[2] ~ Y[J+G] ;                 38700
              TEMPR[3] ~ Z[J+G] ;                                          38800
              FOR GG ~ 1, 2, 3 DO                                          38900
         BEGIN                                                             39000
              KTE ~ 0 ;                                                    39100
              FOR HH ~ 1, 2, 3 DO KTE ~ TR[GG,HH] | TEMPR[HH] + KTE ;      39200
              IF ABS (KTE) < 0.0001 THEN KTE ~ 0 ; TEMPR[5+GG] ~ KTE       39300
         END ;                                                             39400
              X[J+G]~ TEMPR[6] ;      Y[J+G] ~ TEMPR[7] ;                  39500
              Z[J+G] ~ TEMPR[8]  ;                                         39600
         END TRANSF ;                                                      39700
PROCEDURE     MITCL ;                                                      39800
         BEGIN                                                             39900
COMMENT       THIS PROCEDURE CALCULATES THE FLEXIBILITY AND STRESS         40000
              INTENSIFICATION FACTOR OF MITER BENDS. ;                     40100
              G ~ FIT[J].[42:6] ;     ZETAR ~ AL1PR / (2 | G) ;            40200
              KTE ~ SIN (ZETAR) / COS (ZETAR) ;                            40300
              S ~ 2 | R[J] | KTE ;                                         40400
              IF S < (RM | (1 + KTE)) THEN                                 40500
              KH ~ T[J] | S / (KTE | 2 | RM * 2)                           40600
              ELSE KH ~ (1 + 1 / KTE) | T[J] / (2 | RM) ;                  40700
              K ~ 1.52 / (KH * 0.8333)                                     40800
         END MITCL ;                                                       40900
PROCEDURE     SHC ;                                                        41000
         BEGIN                                                             41100
LABEL         E1, E2, E3, E100, E10, E4, E5, E6, E7, E11, E12, E13, E14,   41200
              E20,                                                         41300
              E200, E21, E22, E23, E111 ;                                  41400
SWITCH        SWCH1 ~ E1, E2, E3, E1 ;                                     41500
SWITCH        SWCH2 ~ E4, E5, E6, E4 ;                                     41600
SWITCH        SWCH3 ~ E11, E12, E13, E11 ;                                 41700
SWITCH        SWCH4 ~ E21, E22, E23, E21 ;                                 41800
              PI ~ 3.1415927 ;                                             41900
              RAD ~ 57.295779 ;                                            42000
              FOR J ~ 1 STEP 1 UNTIL NM DO                                 42100
         BEGIN                                                             42200
              IF TYPE[J] = "RIG" THEN GO TO E111 ;                         42300
              I ~ JJ ~ ABS (BRA[J]) ;                                      42400
              XA ~ RFX[I] ;     YA ~ RFY[I] ;     ZA ~ RFZ[I] ;            42500
COMMENT       THE PLANE IN WHICH EACH MEMBER LIES IS DETERMINED ;          42600
              IF X[J] = 0 THEN PL[J] ~ 1                                   42700
              ELSE IF Y[J] = 0 THEN PL[J] ~ 2                              42800
              ELSE IF Z[J] = 0 THEN PL[J] ~ 3                              42900
              ELSE                                                         43000
         BEGIN                                                             43100
              PL[J] ~ 4 ;      IF BOL[I] = 0 THEN BOL[I] ~ J ;             43200
              IF R[J] = 0 THEN                                             43300
         BEGIN                                                             43400
              JJJ ~ J ;      SKEW ;      G ~ 0 ;      TRANSF               43500
         END                                                               43600
              ELSE                                                         43700
         BEGIN                                                             43800
              SKEB ;                                                       43900
              FOR G ~ -1, 0, 1 DO TRANSF                                   44000
         END                                                               44100
         END ;                                                             44200
              IF TYPE[J] = "PRK" THEN                                      44300
         BEGIN                                                             44400
              PRN[I] ~ TRUE ;    D[2,2,I] ~ ENTIER (R[J]) ;                44500
              IF T[J] ! 0 THEN D[1,1,I] ~ 694.4444 / T[J] ;                44600
              GO TO E111                                                   44700
         END ;                                                             44800
              IN1[J]~ (DI[J] * 4 - (DI[J] - ABS (2 | T[J])) * 4) |         44900
              PI / 64 ;                                                    45000
              Q ~ 100 / (EC[J] | IN1[J])  ;                                45100
              RM ~ (DI[J] - ABS (T[J])) | 0.5 ;                            45200
              IF R[J] ! 0 THEN GO TO E10 ;                                 45300
COMMENT       FLEXIBILITY COEFFIECIENTS OF STRAIGHT MEMBERS ;              45400
              K ~ 1.0 ;                                                    45500
              ANGLE ;                                                      45600
COMMENT       LOCATING THE MIDPOINT OF THE STRAIGHT MEMBERS ;              45700
              G ~ PREV[J] ;      IF G < 1 THEN G ~ J - 1 ;                 45800
              XC ~ CHKX[G] + 0.5 | X[J] ;                                  45900
              YC ~ CHKY[G] + 0.5 | Y[J] ;                                  46000
              ZC ~ CHKZ[G] + 0.5 | Z[J] ;      JJ ~ J ;                    46100
COMMENT       IF ANY BRANCH IS SKEWED A ROTATION OF COORDINATES IS         46200
              PERFORMED TO PLACE THE BRANCH IN THE Z PLANE ;               46300
              GO TO SWCH1[PL[J]] ;                                         46400
E1 :          A ~ YC - YA ;         B ~ ZC - ZA ;         C ~ XC - XA ;    46500
              L ~ SQRT (Y[J] * 2 + Z[J] * 2) ;                             46600
              GO TO E100 ;                                                 46700
E2 :          A ~ ZC - ZA ;         B ~ XC - XA ;         C ~ YC - YA ;    46800
              L ~ SQRT (X[J] * 2 + Z[J] * 2) ;                             46900
              GO TO E100 ;                                                 47000
E3 :          A ~ XC - XA ;         B ~ YC - YA ;         C ~ ZC - ZA ;    47100
              L ~ SQRT (X[J] * 2 + Y[J] * 2) ;                             47200
COMMENT       PARAMETERS NEEDED TO OBTAIN THE SHAPE COEFFICIENTS OF        47300
              STRAIGHT MEMBERS ;                                           47400
E100 :        SX ~ K | Q | L ;         SA ~ A | SX ;                       47500
              SB ~ B | SX ;                                                47600
              SINB ~ SIN (AL1R[J]) ;         COSB ~ COS (AL1R[J]) ;        47700
              SINSQB ~ SINB * 2 ;         COSSQB ~ COSB * 2 ;              47800
              SXL ~ L * 2 | SX / 12 ;                                      47900
              QX ~ (1.3 - K) | Q | L | COSB | SINB ;                       48000
              CQ ~ C | QX ;                                                48100
              UX ~ (K | COSSQB + 1.3 | SINSQB) | Q | L ;                   48200
              U0 ~ A | UX - B | QX ;         CU ~ C | UX ;                 48300
              VX ~ (K | SINSQB + 1.3 | COSSQB) | Q | L ;                   48400
              V0 ~ B | VX - A | QX ;         CV ~ C | VX ;                 48500
              P ~ RM * 2 | Q | L / 144 ;         S ~ 2.6 | P ;             48600
              U ~ (2.6 | COSSQB + 0.5 | SINSQB) | P ;                      48700
              V ~ (2.6 | SINSQB + 0.5 | COSSQB) | P ;                      48800
              W ~ 2.1 | P | SINB | COSB ;                                  48900
              SABC2Q ~ B | SA + SXL | COSB | SINB + C | CQ + W ;           49000
              CU0 ~ C | U0 ;         CV0 ~ C | V0 ;                        49100
              SAAC2V ~ A | SA + SXL | COSSQB + C | CV + U ;                49200
              SBBC2U ~ B | SB + SXL | SINSQB + C | CU + V ;                49300
              U00V00 ~ A | U0 + B | V0 + SXL + S ;                         49400
              GO TO E200 ;                                                 49500
E10 :         ANGLE ;                                                      49600
              GO TO SWCH2[PL[J]] ;                                         49700
E4 :          DIST ~ SQRT (Y[J] * 2 + Z[J] * 2) ;                          49800
              GO TO E7 ;                                                   49900
E5 :          DIST ~ SQRT (X[J] * 2 + Z[J] * 2) ;                          50000
              GO TO E7 ;                                                   50100
E6 :          DIST ~ SQRT (X[J] * 2 + Y[J] * 2) ;                          50200
E7 :          PHIR ~ 2 | ARCSIN (DIST | 0.5 / R[J]) ;                      50300
              PHIRA ~ ABS (PHIR) ;         HAPHR[J] ~ 0.5 | PHIRA ;        50400
              ME ~  R[J] | ((1.0 /  COS(HAPHR[J]))  - 1.0) ;               50500
              TE ~  SIN (HAPHR[J]) / COS (HAPHR[J]) | R[J] ;               50600
              KTA ~ COS (HAPHR[J]) | R[J] ;                                50700
              GO TO SWCH3[PL[J]] ;                                         50800
E11 :         TAH ~ Y[J-1] | TE / L ;                                      50900
              TAV ~ Z[J-1] | TE / L ;                                      51000
              DIH ~ 0.5 | Y[J] - TAH ;                                     51100
              DIV1~ 0.5 | Z[J] - TAV ;                                     51200
              DIRR ~ SQRT (DIH * 2 + DIV1 * 2) ;                           51300
              A ~ KTA | DIH / DIRR - YA + CHKY[J-1] + Y[J] | 0.5 ;         51400
              B ~ KTA | DIV1/ DIRR - ZA + CHKZ[J-1] + Z[J] | 0.5 ;         51500
              C ~ CHKX[J-1] - XA ;                                         51600
              TB ~ Z[J] / Y[J] ;         CH ~ Y[J-1] ;                     51700
              CV ~ Z[J-1] ;         ARMX[J] ~ CHKX[J-1] ;                  51800
              ARMY[J] ~ ME | DIH / DIRR + TAH + CHKY[J-1] ;                51900
              ARMZ[J] ~ ME | DIV1/ DIRR + TAV + CHKZ[J-1] ;                52000
              GO TO E14 ;                                                  52100
E12 :         TAH ~ Z[J-1] | TE / L ;                                      52200
              TAV ~ X[J-1] | TE / L ;                                      52300
              DIH ~ 0.5 | Z[J] - TAH ;                                     52400
              DIV1~ 0.5 | X[J] - TAV ;                                     52500
              DIRR ~ SQRT (DIH * 2 + DIV1 * 2);                            52600
              A ~ KTA | DIH / DIRR - ZA + CHKZ[J-1] + Z[J] | 0.5 ;         52700
              B ~ KTA | DIV1/ DIRR - XA + CHKX[J-1] + X[J] | 0.5 ;         52800
              C ~ CHKY[J-1] - YA ;                                         52900
              TB ~ X[J] / Z[J] ;         CH ~ Z[J-1] ;                     53000
              CV ~ X[J-1] ;         ARMY[J] ~ CHKY[J-1] ;                  53100
              ARMX[J] ~ ME | DIV1/ DIRR + TAV + CHKX[J-1] ;                53200
              ARMZ[J] ~ ME | DIH / DIRR + TAH + CHKZ[J-1] ;                53300
              GO TO E14 ;                                                  53400
E13 :         TAH ~ X[J-1] | TE / L ;                                      53500
              TAV ~ Y[J-1] | TE / L ;                                      53600
              DIH ~ 0.5 | X[J] - TAH ;                                     53700
              DIV1~ 0.5 | Y[J] - TAV ;                                     53800
              DIRR ~ SQRT (DIH * 2 + DIV1 * 2);                            53900
              A ~ KTA | DIH / DIRR - XA + CHKX[J-1] + X[J] | 0.5 ;         54000
              B ~ KTA | DIV1/ DIRR - YA + CHKY[J-1] + Y[J] | 0.5 ;         54100
              C ~ CHKZ[J-1] - ZA ;                                         54200
              TB ~ Y[J] / X[J] ;         CH ~ X[J-1] ;                     54300
              CV ~ Y[J-1] ;          ARMZ[J] ~ CHKZ[J-1] ;                 54400
              ARMX[J] ~ ME | DIH / DIRR + TAH + CHKX[J-1] ;                54500
              ARMY[J] ~ ME | DIV1/ DIRR + TAV + CHKY[J-1] ;                54600
E14 :         AL1JA ~ AL1R[J-1] | RAD ;         AL1B ~ AL1JA / 90 ;        54700
              AL1C ~ ENTIER (AL1B) ;                                       54800
              AL1D ~ ABS (AL1B - AL1C) ;         AL1A ~ 90 | AL1D ;        54900
              IF 90 - AL1A < 1@-4 THEN AL1A ~ 0 ;                          55000
              IF TB > 0 THEN                                               55100
         BEGIN                                                             55200
              IF CH < 0 THEN AL1A ~ AL1A + 180 ;                           55300
              IF CH = 0 AND CV > 0 THEN AL1A ~ AL1A + 180                  55400
         END ;                                                             55500
              IF TB < 0 THEN                                               55600
         BEGIN                                                             55700
              IF CV > 0 THEN AL1A ~ AL1A + 90 ;                            55800
              IF CV = 0 AND CH > 0 THEN AL1A ~ AL1A + 90 ;                 55900
              IF CV < 0 THEN AL1A ~ AL1A + 270 ;                           56000
              IF CV = 0 AND CH < 0 THEN AL1A ~ AL1A + 270                  56100
         END ;                                                             56200
              AL1PR ~ AL1A / RAD ;                                         56300
              PHALR ~ AL1PR + PHIRA ;                                      56400
              SINB ~ SIN (AL1PR) ;         COSB ~ COS (AL1PR) ;            56500
              SINPH ~ SIN (PHALR) ;         COSPH ~ COS (PHALR) ;          56600
              IF FIT[J].[30:12] = "MI" THEN MITCL                          56700
              ELSE K ~ 0.1375 | RM * 2 / (T[J] | R[J]) ;                   56800
              IF K < 1 THEN K ~ 1 ;                                        56900
              CA ~ COSB - COSPH ;         CB ~ SINB - SINPH ;              57000
              CAB ~ (COSPH * 2 - SINPH * 2 - COSB * 2 + SINB * 2)|0.25 ;   57100
              CAA ~ (PHIR - SINPH | COSPH + SINB | COSB) | 0.5 ;           57200
              CBB ~ (PHIR + SINPH | COSPH - SINB | COSB) | 0.5 ;           57300
              KTE ~ R[J] * 2 | K | Q ;         SPA ~ KTE | CA ;            57400
              SPB ~ KTE | CB ;         SPAB ~ R[J] | KTE | CAB ;           57500
              SPAA ~ R[J] | KTE | CAA ;                                    57600
              SPBB ~ R[J] | KTE | CBB ;                                    57700
              KTE1 ~ R[J] * 2 | Q | 1.3 ;                                  57800
              UP0 ~ KTE1 | CA ;         VP0 ~ KTE1 | CB ;                  57900
              UP00VP00 ~ R[J] | KTE1 | PHIR ;                              58000
              SX ~ R[J] | PHIR | K | Q ;                                   58100
              SA ~ A | SX + SPA ;         SB ~ B | SX + SPB ;              58200
              QX ~ (K - 1.3) | R[J] | Q | CAB ;         CQ ~ C | QX ;      58300
              UX ~ (K | CBB + 1.3 | CAA) | R[J] | Q ;                      58400
              U0 ~ UX | A + UP0 - QX | B ;         CU ~ C | UX ;           58500
              VX ~ (K | CAA + 1.3 | CBB) | R[J] | Q ;                      58600
              V0 ~ VX | B + VP0 - QX | A ;         CV ~ C | VX ;           58700
              P ~ RM * 2 | R[J] | Q / 144 ;         S ~ 2.6 | P | PHIR ;   58800
              U ~ (2.6 | CBB + 0.5 | CAA) | P ;                            58900
               V ~ (2.6 | CAA + 0.5 | CBB) | P ;                           59000
              W ~ -2.1 | P | CAB ;                                         59100
              SABC2Q ~ SX | A | B + SPA | B + SPB | A + SPAB +             59200
              CQ | C + W ;                                                 59300
              CU0 ~ C | U0 ;         CV0 ~ C | V0 ;                        59400
              SAAC2V ~ 2 | A | SPA + A * 2 | SX + CV | C + SPAA + U ;      59500
              SBBC2U ~ 2 | B | SPB + B * 2 | SX + CU | C + SPBB + V ;      59600
              U00V00 ~ UP00VP00 + 2 | A | UP0 + 2 | B | VP0 +              59700
              A * 2 | UX + B * 2 | VX - 2 | A | B | QX + S ;               59800
E200 :        I ~ ABS (BRA[J]) ;                                           59900
              IF PL[J] = 4 THEN                                            60000
              FOR G ~ -1, 0, 1 DO                                          60100
         BEGIN                                                             60200
              IF (J + G) > NM OR (J + G) < 1 THEN GO TO E20 ;              60300
              HHH ~ PREV[J+G] ;      IF HHH { 0 THEN HHH ~ J + G - 1 ;     60400
              X[J+G] ~ CHKX[J+G] - CHKX[HHH] ;                             60500
              Y[J+G] ~ CHKY[J+G] - CHKY[HHH] ;                             60600
              Z[J+G] ~ CHKZ[J+G] - CHKZ[HHH] ;                             60700
E20 :    END ;                                                             60800
              GO TO SWCH4[PL[J]] ;                                         60900
E21 :         D[1,1,I] ~ D[1,1,I] + SX ;                                   61000
              D[1,5,I] ~ D[1,5,I] + SB ;                                   61100
              D[1,6,I] ~ D[1,6,I] - SA ;                                   61200
              D[2,2,I] ~ D[2,2,I] + VX ;                                   61300
              D[2,3,I] ~ D[2,3,I] + QX ;                                   61400
              D[2,4,I] ~ D[2,4,I] - V0 ;                                   61500
              D[2,5,I] ~ D[2,5,I] - CQ ;                                   61600
              D[2,6,I] ~ D[2,6,I] + CV ;                                   61700
              D[3,3,I] ~ D[3,3,I] + UX ;                                   61800
              D[3,4,I] ~ D[3,4,I] + U0 ;                                   61900
              D[3,5,I] ~ D[3,5,I] - CU ;                                   62000
              D[3,6,I] ~ D[3,6,I] + CQ ;                                   62100
              D[4,4,I] ~ D[4,4,I] + U00V00 ;                               62200
              D[4,5,I] ~ D[4,5,I] - CU0 ;                                  62300
              D[4,6,I] ~ D[4,6,I] - CV0 ;                                  62400
              D[5,5,I] ~ D[5,5,I] + SBBC2U ;                               62500
              D[5,6,I] ~ D[5,6,I] - SABC2Q ;                               62600
              D[6,6,I] ~ D[6,6,I] + SAAC2V ;                               62700
              GO TO E111 ;                                                 62800
E22 :         D[1,1,I] ~ D[1,1,I] + UX ;                                   62900
              D[1,3,I] ~ D[1,3,I] + QX ;                                   63000
              D[1,4,I] ~ D[1,4,I] + CQ ;                                   63100
              D[1,5,I] ~ D[1,5,I] + U0 ;                                   63200
              D[1,6,I] ~ D[1,6,I] - CU ;                                   63300
              D[2,2,I] ~ D[2,2,I] + SX ;                                   63400
              D[2,4,I] ~ D[2,4,I] - SA ;                                   63500
              D[2,6,I] ~ D[2,6,I] + SB ;                                   63600
              D[3,3,I] ~ D[3,3,I] + VX ;                                   63700
              D[3,4,I] ~ D[3,4,I] + CV ;                                   63800
              D[3,5,I] ~ D[3,5,I] - V0 ;                                   63900
              D[3,6,I] ~ D[3,6,I] - CQ ;                                   64000
              D[4,4,I] ~ D[4,4,I] + SAAC2V ;                               64100
              D[4,5,I] ~ D[4,5,I] - CV0 ;                                  64200
              D[4,6,I] ~ D[4,6,I] - SABC2Q ;                               64300
              D[5,5,I] ~ D[5,5,I] + U00V00 ;                               64400
              D[5,6,I] ~ D[5,6,I] - CU0 ;                                  64500
              D[6,6,I] ~ D[6,6,I] + SBBC2U ;                               64600
              GO TO E111 ;                                                 64700
E23 :         D[1,1,I] ~ D[1,1,I] + VX ;                                   64800
              D[1,2,I] ~ D[1,2,I] + QX ;                                   64900
              D[1,4,I] ~ D[1,4,I] - CQ ;                                   65000
              D[1,5,I] ~ D[1,5,I] + CV ;                                   65100
              D[1,6,I] ~ D[1,6,I] - V0 ;                                   65200
              D[2,2,I] ~ D[2,2,I] + UX ;                                   65300
              D[2,4,I] ~ D[2,4,I] - CU ;                                   65400
              D[2,5,I] ~ D[2,5,I] + CQ ;                                   65500
              D[2,6,I] ~ D[2,6,I] + U0 ;                                   65600
              D[3,3,I] ~ D[3,3,I] + SX ;                                   65700
              D[3,4,I] ~ D[3,4,I] + SB ;                                   65800
              D[3,5,I] ~ D[3,5,I] - SA ;                                   65900
              D[4,4,I] ~ D[4,4,I] + SBBC2U ;                               66000
              D[4,5,I] ~ D[4,5,I] - SABC2Q ;                               66100
              D[4,6,I] ~ D[4,6,I] - CU0 ;                                  66200
              D[5,5,I] ~ D[5,5,I] + SAAC2V ;                               66300
              D[5,6,I] ~ D[5,6,I] - CV0 ;                                  66400
              D[6,6,I] ~ D[6,6,I] + U00V00 ;                               66500
E111 :   END ;                                                             66600
              FOR I ~ 1 STEP 1 UNTIL NB DO                                 66700
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                  66800
              FOR H ~ G + 1 STEP 1 UNTIL 6 DO D[H, G, I] ~ D[G, H, I]      66900
         END SHC ;                                                         67000
PROCEDURE     BM ;                                                         67100
         BEGIN                                                             67200
              BMAT[1,5] ~ -KTZ ;                                           67300
              BMAT[1,6] ~ KTY ;                                            67400
              BMAT[2,4] ~ KTZ ;                                            67500
              BMAT[2,6] ~ -KTX ;                                           67600
              BMAT[3,4] ~ -KTY ;                                           67700
              BMAT[3,5] ~ KTX ;                                            67800
         END BM ;                                                          67900
PROCEDURE     LOOP ;                                                       68000
         BEGIN                                                             68100
LABEL         E115, E116, E117, E118 ;                                     68200
COMMENT       THIS PROCEDURE FINDS THE BRANCHES THAT FORM A LOOP. ;        68300
              NL ~ NL + 1 ;     JJ ~ 0 ;                                   68400
              BASBR[II] ~ BRA[J] ;                                         68500
E115 :        JJ ~ JJ + 1 ;      IF JJ > J THEN GO TO E117 ;               68600
              KTX ~ ABS (CHKX[J] - CHKX[JJ] + X[JJ]) ;                     68700
              KTY ~ ABS (CHKY[J] - CHKY[JJ] + Y[JJ]) ;                     68800
              KTZ ~ ABS (CHKZ[J] - CHKZ[JJ] + Z[JJ]) ;                     68900
              IF KTX < 0.005 AND KTY < 0.005 AND KTZ < 0.005               69000
              THEN GGG ~ BRA[JJ]                                           69100
              ELSE GO TO E115 ;                                            69200
              HHH ~ GGG - 1 ;                                              69300
              FOR GGG ~ 1 STEP 1 UNTIL HHH DO                              69400
              TRAV[II,GGG] ~ TRAVT[GGG,II] ~ 0 ;                           69500
              JJJ ~ J ;                                                    69600
E116 :        G ~ BRA[JJJ] ;                                               69700
              IF TYPE[JJJ] = "ANK" OR TYPE[JJJ] = "PRK" THEN               69800
              TRAV[II,G] ~ 0 ELSE TRAV[II,G] ~ 1 ;                         69900
              TRAVT[G,II] ~ TRAV[II,G] ;                                   70000
              IF PREV[JJJ] < 1 THEN JJJ ~ JJJ - 1 ELSE JJJ ~ PREV[JJJ] ;   70100
              IF JJJ < JJ THEN GO TO E118 ;                                70200
              GO TO E116 ;                                                 70300
E117 :        WRITE(FILE2, FOR20, J) ;                                     70400
E118 :        II ~ II + 1 ;                                                70500
         END LOOP ;                                                        70600
PROCEDURE     MOMEN ;                                                      70700
         BEGIN                                                             70800
LABEL         E501, E502, E503, E505, E507 ;                               70900
              IF OPTION.[24:12] = "FM" THEN SNPR ~ TRUE                    71000
              ELSE SNPR ~ FALSE ;                                          71100
              SKB ~ FALSE ;                                                71200
              FOR II ~ 1 STEP 1 UNTIL NBAS DO IF PRN[BASBR[II]] THEN       71300
         BEGIN                                                             71400
              III ~ BASBR[II] ;      G ~ 6 | II - 4 ;                      71500
              GG ~ ENTIER (D[2,2,III]) ;                                   71600
              FOR J ~ NNN STEP -1 UNTIL G DO MF[J+5] ~ MF[J] ;             71700
              KTE ~ MF[G-1] ;     G ~ G - 2 ;     NNN ~ NNN + 5 ;          71800
              FOR J ~ 1 STEP 1 UNTIL 6 DO   MF[G+J] ~ 0 ;                  71900
              MF[G+GG] ~ KTE                                               72000
         END ;                                                             72100
              WRITE(FILE2[PAGE]) ;      WRITE(FILE2, TI10) ;               72200
COMMENT       COMPUTATION OF FORCES AND MOMENTS AT BRANCHING POINTS. ;     72300
              FOR I ~ 1 STEP 1 UNTIL NB DO                                 72400
         BEGIN                                                             72500
              FOR KK ~ 1 STEP 1 UNTIL 6 DO CNC[I,KK] ~ 0 ;                 72600
              FOR II ~ 1 STEP 1 UNTIL NBAS DO                              72700
              IF TRAV[II,I] ! 0 THEN                                       72800
         BEGIN                                                             72900
              G ~ BASBR[II] ;      KTX ~ RFX[G] - RFX[I] ;                 73000
              KTY ~ RFY[G] - RFY[I] ;      KTZ ~ RFZ[G] - RFZ[I];          73100
              BM ;      GG ~ 6 | II - 6 ;                                  73200
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                  73300
         BEGIN                                                             73400
              KTE ~ 0 ;                                                    73500
              FOR H ~ 1 STEP 1 UNTIL 6 DO                                  73600
              KTE ~ BMAT[G,H] | MF[GG+H] + KTE ;                           73700
              CNC[I,G] ~ CNC[I,G] + KTE                                    73800
         END                                                               73900
         END                                                               74000
         END ;                                                             74100
              NAN ~ 0 ;                                                    74200
              FOR I ~ 1 STEP 1 UNTIL NB DO                                 74300
         BEGIN                                                             74400
              J ~ 0 ;                                                      74500
E501 :        J ~ J + 1 ;                                                  74600
              IF BRA[J] > I OR J > NM THEN GO TO E503 ;                    74700
              IF BRA[J] < I THEN GO TO E501                                74800
              ELSE IF BRA[J] = I THEN                                      74900
         BEGIN                                                             75000
              IF SKB THEN FOR G ~ -2, -1, 0 DO                             75100
         BEGIN                                                             75200
              HHH ~ PREV[J+G] ;     IF HHH { 0 THEN HHH ~ J + G - 1 ;      75300
              X[J+G] ~ CHKX[J+G] - CHKX[HHH] ;                             75400
              Y[J+G] ~ CHKY[J+G] - CHKY[HHH] ;                             75500
              Z[J+G] ~ CHKZ[J+G] - CHKZ[HHH] ;                             75600
         END ;                                                             75700
              SKB ~ FALSE ;                                                75800
              RM ~ (DI[J] - T[J]) | 0.5 ;     TMP ~ FALSE ;                75900
              IF TYPE[J]  = "PRK" THEN                                     76000
         BEGIN                                                             76100
              TMP ~ TRUE ;      GO TO E502                                 76200
         END ;                                                             76300
              KTX ~ RFX[I] - CHKX[J] + X[J] ;                              76400
              KTY ~ RFY[I] - CHKY[J] + Y[J] ;                              76500
              KTZ ~ RFZ[I] - CHKZ[J] + Z[J] ;                              76600
              IF PL[J] = 4 AND R[J] ! 0 THEN                               76700
         BEGIN                                                             76800
              SKEB ;     FOR G ~ -1, 0, 1 DO TRANSF ;      SKB ~ TRUE      76900
         END ;                                                             77000
              ANGLE ;                                                      77100
              IF R[J] ! 0 THEN                                             77200
         BEGIN                                                             77300
              AL1MR ~ (AL1R[J+1] + AL1R[J-1]) | 0.5 ;                      77400
              SINM ~ SIN (AL1R[J-1]) ;    COSM ~ COS (AL1R[J-1]) ;         77500
              SINN ~ SIN (AL1R[J+1]) ;    COSN ~ COS (AL1R[J+1]) ;         77600
              SINC ~ SIN (AL1MR) ;     COSC ~ COS (AL1MR)                  77700
         END                                                               77800
              ELSE                                                         77900
         BEGIN                                                             78000
              SINN ~ SIN (AL1R[J]) ;      COSN ~ COS (AL1R[J])             78100
         END ;                                                             78200
              IF TYPE[J-1] = "PRK" THEN GO TO E502 ;                       78300
              IF PREV[J] = -1 AND TYPE[J-1] ! "BRP" THEN GO TO E507        78400
              ELSE WRITE(FILE2, SPAC) ;                                    78500
E502 :        IF TMP THEN HH ~ J                                           78600
              ELSE                                                         78700
         BEGIN                                                             78800
              IF PREV[J] = -1 THEN HH ~ J - 1                              78900
              ELSE  HH ~ PREV[J]                                           79000
         END ;                                                             79100
              BM ;                                                         79200
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                  79300
         BEGIN                                                             79400
              KTE ~ 0 ;                                                    79500
              FOR H ~ 1 STEP 1 UNTIL 6 DO                                  79600
              KTE ~ BMAT[G,H] | CNC[I,H] + KTE ;                           79700
              TEMPR[10+G] ~ KTE                                            79800
         END ;                                                             79900
              IF HH = 0 THEN GO TO E505 ;                                  80000
COMMENT       FORCES AND MOMENTS AT THE ANCHORS ARE STORED IN MEMORY       80100
              IN ORDER TO PRINT THEIR VALUES LATER ON. ;                   80200
              IF (TYPE[J] = "ANK" OR TYPE[J] = "PRK") AND TMP THEN         80300
         BEGIN                                                             80400
              NAN ~ NAN + 1 ;      HH ~ J ;                                80500
E505 :        KTE ~ 0 ;      TEMPR[50+NAN] ~ HH ;                          80600
              FOR G ~ 14 STEP 1 UNTIL 16 DO                                80700
         BEGIN                                                             80800
              IF HH = 0 THEN CNCT[NAN,G-13] ~ TEMPR[G] ELSE                80900
              CNCT[NAN,G-13] ~ -TEMPR[G] ;                                 81000
              KTE ~ TEMPR[G] * 2 + KTE                                     81100
         END ;                                                             81200
              CNCT[NAN,4]~ SQRT (KTE) ;     KTE ~ 0 ;                      81300
              FOR G ~ 11 STEP 1 UNTIL 13 DO                                81400
         BEGIN                                                             81500
              IF HH = 0 THEN CNCT[NAN,G-6] ~ TEMPR[G] ELSE                 81600
              CNCT[NAN,G-6] ~ -TEMPR[G] ;                                  81700
              KTE ~ TEMPR[G] * 2 + KTE                                     81800
         END ;                                                             81900
              CNCT[NAN,8] ~ SQRT (KTE) ;                                   82000
         END ;                                                             82100
              IF TYPE[J] = "PRK" THEN                                      82200
         BEGIN                                                             82300
              WRITE(FILE2, FOR9, OUT9) ;      GO TO E501                   82400
         END ;                                                             82500
              G ~ PL[J] ;      IF G = 4 THEN G ~ 1 ;                       82600
              GG ~ (G+1) MOD 3 + 11 ;      JJ ~ G MOD 3 + 11 ;             82700
              IF (NOT TMP) AND R[J] ! 0 THEN                               82800
         BEGIN                                                             82900
              SINN ~ SINM ;      COSN ~ COSM                               83000
         END ;                                                             83100
              MB ~ TEMPR[G+10] ;                                           83200
              MPB ~ TEMPR[GG] | COSN - TEMPR[JJ] | SINN ;                  83300
              MTOR ~ -TEMPR[GG] | SINN - TEMPR[JJ] | COSN ;                83400
              IF R[J] ! 0 AND TMP THEN                                     83500
         BEGIN                                                             83600
              KTX ~ RFX[I] - ARMX[J] ;                                     83700
              KTY ~ RFY[I] - ARMY[J] ;                                     83800
              KTZ ~ RFZ[I] - ARMZ[J] ;     BM ;                            83900
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                  84000
         BEGIN                                                             84100
              KTE ~ 0 ;                                                    84200
              FOR H ~ 1 STEP 1 UNTIL 6 DO                                  84300
              KTE ~ BMAT[G,H] | CNC[I,H] + KTE ;                           84400
              TEMPR[20+G] ~ KTE                                            84500
         END ;                                                             84600
              G ~ PL[J] ;  IF G = 4 THEN G ~ 1 ;  MBM ~ TEMPR[G+20] ;      84700
              GG ~ (G+1) MOD 3 + 21 ;      JJ ~ G MOD 3 + 21 ;             84800
              MPBM ~ TEMPR[GG] | COSC - TEMPR[JJ] | SINC ;                 84900
              MTORM ~ -TEMPR[GG] | SINC - TEMPR[JJ] | COSC                 85000
         END ;                                                             85100
              IF (TYPE[J] = "BRP" AND TMP) OR (TYPE[J] = "SKW")            85200
              OR (PREV[J] > 0 AND (NOT TMP))                               85300
              OR (TYPE[J-1] = "BRP" AND (NOT TMP))                         85400
              OR (TYPE[J+1] = "PRK" AND TMP)                               85500
              OR (TYPE[J-1] = "PRK" AND (NOT TMP)) THEN                    85600
         BEGIN                                                             85700
              KH ~ 1.0 ;                                                   85800
              IF FIT[J] = "TEE" THEN KH ~ T[J] / RM                        85900
              ELSE IF FIT[J] = "WTE" THEN KH ~ 4.4 | T[J] / RM             86000
              ELSE IF FIT[J] = "RTE" THEN KH ~                             86100
              (T[J] | 1.5) * 2.5 / (RM | T[J] * 1.5) ;                     86200
              IF FIT[J] = "DWF" THEN BET ~ 1.2                             86300
              ELSE IF FIT[J] = "FWJ" THEN BET ~ 1.3                        86400
              ELSE IF FIT[J] = "LJF" THEN BET ~ 1.6                        86500
              ELSE IF FIT[J] = "SPJ" THEN BET ~ 2.3                        86600
              ELSE BET ~ 0.9 / (KH * 0.66667) ;                            86700
              IF BET < 1.0 THEN BET ~ 1.0                                  86800
         END                                                               86900
              ELSE BET ~ 1.0 ;                                             87000
              KTT ~ 6 | BET | DI[J] / IN1[J] ;                             87100
              IF R[J] ! 0 AND TMP THEN                                     87200
         BEGIN                                                             87300
              IF FIT[J].[30:12] = "MI" THEN MITCL                          87400
              ELSE KH ~ 0.1375 | RM * 2 / (T[J] | R[J]) ;                  87500
              BET ~ 0.9 / (KH * 0.66667) ;                                 87600
              IF BET < 1.0 THEN BET ~ 1.0 ;                                87700
              KTT ~ 6 | BET | DI[J] / IN1[J] ;                             87800
              STORM ~ 0.5 | KTT | MTORM / BET ;                            87900
              SBENM ~ KTT | SQRT (MBM * 2 + MPBM * 2) ;                    88000
              SEQM ~ SQRT (SBENM * 2 + 4 | STORM * 2) ;                    88100
              IF SEQM > STRSMX THEN                                        88200
         BEGIN                                                             88300
              STRSMX ~ SEQM ;     PT ~ HH                                  88400
         END ;                                                             88500
              IF SNPR THEN WRITE (FILE2, FOR12, OUT12)                     88600
         END ;                                                             88700
              STOR ~ 0.5 | KTT | MTOR / BET ;                              88800
              SBEN ~ KTT | SQRT (MB * 2 + MPB * 2) ;                       88900
              SEQ ~ SQRT (SBEN * 2 + 4 | STOR * 2) ;                       89000
              IF SEQ > STRSMX THEN                                         89100
         BEGIN                                                             89200
              STRSMX ~ SEQ ;      PT ~ HH                                  89300
         END ;                                                             89400
              IF SNPR THEN WRITE (FILE2, FOR11, OUT11) ;                   89500
              IF TMP THEN GO TO E501 ;                                     89600
E507 :        KTX ~ RFX[I] - CHKX[J] ;                                     89700
              KTY ~ RFY[I] - CHKY[J] ;                                     89800
              KTZ ~ RFZ[I] - CHKZ[J] ;                                     89900
              TMP ~ TRUE ;     GO TO E502                                  90000
         END ;                                                             90100
E503 :   END                                                               90200
         END MOMEN ;                                                       90300
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                  90400
              FOR H ~ 1 STEP 1 UNTIL 6 DO                                  90500
         BEGIN                                                             90600
              IF G = H THEN BMAT[G,H] ~ 1                                  90700
              ELSE BMAT[G,H] ~ 0                                           90800
         END ;                                                             90900
              CHKX[0] ~ CHKY[0] ~ CHKZ[0] ~ 0 ;                            91000
              FILL MATE[*] WITH 0.0,                                       91100
              5.07, 5.35, 5.65, 5.90, 6.13, 6.38, 6.60, 6.82, 7.02,7.23,   91200
              7.44, 7.65, 7.84, 7.97, 8.12, 8.19, 8.28, 8.36, 0.0, 27.9,   91300
              5.07, 5.35, 5.65, 5.90, 6.13, 6.38, 6.60, 6.82, 7.02,7.23,   91400
              7.44, 7.65, 7.84, 7.97, 8.12, 8.19, 8.28, 8.36, 0.0, 29.9,   91500
              5.07, 5.35, 5.65, 5.90, 6.13, 6.38, 6.60, 6.82, 7.02,7.23,   91600
              7.44, 7.65, 7.84, 7.97, 8.12, 8.19, 8.28, 8.36, 0.0, 29.9,   91700
              4.77, 5.05, 5.32, 5.56, 5.79, 6.04, 6.19, 6.34, 6.50,6.66,   91800
              6.80, 6.96, 7.10, 7.22, 7.32, 7.41, 7.49, 7.55, 0.0, 27.4,   91900
              8.21, 8.47, 8.75, 8.98, 9.16, 9.34, 9.47, 9.59, 9.70,9.82,   92000
              9.92,10.05,10.16,10.29,10.39,10.48,10.54,10.60, 0.0, 28.3,   92100
              4.36, 4.59, 4.85, 5.10, 5.29, 5.50, 5.66, 5.81, 5.96,6.13,   92200
              6.26, 6.39, 6.52, 6.63, 6.72, 6.78, 6.85, 6.90, 0.0, 29.2,   92300
              5.72, 6.40, 6.95, 7.28, 7.55, 7.84, 8.02, 8.20, 8.40,8.58,   92400
              8.78, 8.96, 9.16, 9.34, 9.52, 9.70, 9.88,10.04, 0.0, 26.0,   92500
              0.0, 0.0, 0.0, 0.0, 0.0, 7.48, 7.68, 7.90, 8.09, 8.30,       92600
              8.50, 8.70, 8.90, 9.10, 9.30, 9.50, 9.70, 9.89, 0.0, 26.0,   92700
              10.04,10.61,11.25, 11.86,12.39,12.95,13.28,13.60,13.90,      92800
              14.20, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 10.6,    92900
              0.0, 0.0, 0.0, 0.0, 0.0, 5.75, 5.93, 6.10, 6.28, 6.47,       93000
              6.65, 6.83, 7.00, 7.19, 0.0, 0.0, 0.0, 0.0, 0.0, 13.4,       93100
              8.45, 8.65, 8.95, 9.32,9.66,10.03,10.12,10.23,10.32,10.44,   93200
              10.52,10.62,10.72,10.80,10.90,11.00, 0.0, 0.0, 0.0, 13.4,    93300
              8.24, 8.41, 8.73, 9.11, 9.42,9.76,10.00,10.23,10.47,10.69,   93400
              10.92,11.16,11.40,11.63,11.85,12.09, 0.0, 0.0, 0.0, 14.0,    93500
              5.79, 6.13, 6.48, 6.78, 7.04, 7.32, 7.48, 7.61, 7.73,7.88,   93600
              8.01, 8.13, 8.29, 8.39, 0.0, 0.0, 0.0, 0.0, 0.0, 29.5,       93700
              6.76, 7.19, 7.60, 7.94, 8.24, 8.54, 8.71, 8.90, 0.0,0.0,     93800
              0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 29.5 ;          93900
         BEGIN                                                             94000
LABEL         E101, E102, E103, E104, E105,                                94100
              E109, E110, E110A, E110B, E123, E128 ;                       94200
PROCEDURE     PREVJ ;                                                      94300
         BEGIN                                                             94400
LABEL         E107 ;                                                       94500
              G ~ PREV[J] ;                                                94600
              I ~ I + 1 ;     III ~ BRA[G] ;     BRA[J] ~ I ;              94700
              RFX[I] ~ CHKX[G] ;    RFY[I] ~ CHKY[G] ; RFZ[I] ~ CHKZ[G];   94800
              FOR HH ~ 1 STEP 1 WHILE TRAV[HH,III] = 0 DO                  94900
              IF HH > II THEN GO TO E107 ;                                 95000
              FOR GG ~ 1 STEP 1 UNTIL III DO                               95100
              TRAV[II,GG] ~ TRAVT[GG,II] ~ TRAV[HH,GG];                    95200
E107 :        TRAV[II,I] ~ TRAVT[I,II] ~ 1                                 95300
         END PREVJ ;                                                       95400
PROCEDURE     ANKR ;                                                       95500
         BEGIN                                                             95600
              WRITE(FILE2, FOR5, CHKX[J], CHKY[J], CHKZ[J]) ;              95700
              BASBR[II] ~ I ;      II ~ II + 1 ;                           95800
              READ(FILE1, INC1, DATAC1) ;     NAN ~ NAN + 1                95900
         END ANKR ;                                                        96000
PROCEDURE     BRPT ;                                                       96100
         BEGIN                                                             96200
              I ~ I + 1 ;     TRAV[II,I] ~ TRAVT[I,II] ~ 1 ;               96300
              RFX[I] ~ CHKX[G] ;   RFY[I] ~ CHKY[G] ;  RFZ[I] ~ CHKZ[G];   96400
         END BRPT ;                                                        96500
E109 :         READ(FILE1, IDENF, IDEN) ;    WRITE(FILE2, IDENF, IDEN) ;   96600
              WRITE(FILE2, SPAC) ;                                         96700
              READ(FILE1, INA, DATAA) ;     WRITE(FILE2, TI) ;             96800
              FOR G ~ 1 STEP 1 UNTIL 6 DO DE[G,1] ~ -DE[G,1] ;             96900
              IF OPTION = "LAST" THEN GO TO E128 ;                         97000
              IF OPTION.[36:12] = "MT" THEN TMP ~ TRUE ELSE TMP ~ FALSE;   97100
              SKB ~ FALSE ;                                                97200
              IF (NOT TMP) THEN WRITE(FILE2, TIA) ;                        97300
              FOR I ~ 1 STEP 1 UNTIL NB DO                                 97400
         BEGIN                                                             97500
              BOL[I] ~ 0 ;      PRN[I] ~ FALSE                             97600
         END ;                                                             97700
              FOR G ~ 1 STEP 1 UNTIL 7 DO FOR H ~ 1 STEP 1 UNTIL 7 DO      97800
              TRAV[G,H] ~ TRAVT[H,G] ~ 0 ;                                 97900
              TRAV[1,1] ~ TRAVT[1,1] ~ BRA[1] ~ 1 ;                        98000
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                  98100
              FOR H ~ 1 STEP 1 UNTIL 20 DO DE[G,H] ~ 0 ;                   98200
              J ~ NAN ~ NL ~ STRSMX ~ 0 ;     I ~ H ~ II ~ 1 ;             98300
              READ(FILE1, INC1, DATAC1) ;                                  98400
E110 :        J ~ J + 1 ;                                                  98500
              IF TMP THEN                                                  98600
         BEGIN                                                             98700
              READ(FILE1, INC, DATAC) ;                                    98800
              IF PREV[J] = 999 THEN GO TO E110A ;                          98900
              IF MAT[J] = "   " THEN                                       99000
         BEGIN                                                             99100
              EC[J] ~ EC[J-1] ;      EPS[J] ~ EPS[J-1] ;                   99200
              GO TO E110B                                                  99300
         END ;                                                             99400
              IF MAT[J] = "LCS" THEN HH~ 1                                 99500
              ELSE IF MAT[J] = "HCS" THEN HH~ 2                            99600
              ELSE IF MAT[J] = "LCM" THEN HH~ 3                            99700
              ELSE IF MAT[J] = "ICM" THEN HH~ 4                            99800
              ELSE IF MAT[J] = "AUS" THEN HH~ 5                            99900
              ELSE IF MAT[J] = "SCS" THEN HH~ 6                           100000
              ELSE IF MAT[J] = "MON" THEN HH~ 7                           100100
              ELSE IF MAT[J] = "KMO" THEN HH~ 8                           100200
              ELSE IF MAT[J] = "ALU" THEN HH~ 9                           100300
              ELSE IF MAT[J] = "GCI" THEN HH~ 10                          100400
              ELSE IF MAT[J] = "BRO" THEN HH~ 11                          100500
              ELSE IF MAT[J] = "BRS" THEN HH~ 12                          100600
              ELSE IF MAT[J] = "WIR" THEN HH~ 13                          100700
              ELSE IF MAT[J] = "KON" THEN HH~ 14                          100800
              ELSE WRITE(FILE2, FOR18, J) ;                               100900
              HH ~ 20 | HH ;      EC[J] ~MATE[HH] ;                       101000
              HH ~ HH - 20 ;                                              101100
              KTE ~ 0.01 | TH[J] ;                                        101200
              GG ~ ENTIER (KTE) ;     JJ ~ ABS (GG) + 1 ;                 101300
              IF GG < 0 THEN JJ ~ -JJ ;                                   101400
              KTX ~ MATE[HH+GG+4] ;                                       101500
              KTY ~ MATE[HH+JJ+4] ;                                       101600
              IF KTX = 0 OR KTY = 0 THEN                                  101700
         BEGIN                                                            101800
              WRITE(FILE2, FOR17, J) ;     GO TO E110                     101900
         END ;                                                            102000
              EPS[J]~ KTX + (KTY - KTX) | ABS (KTE - GG) ;                102100
              WRITE(FILE2, FMAT, OUTMAT)                                  102200
         END                                                              102300
              ELSE READ(FILE1, INB, DATAB) ;                              102400
E110B :       IF PREV[J] = 999 THEN GO TO E110A ;                         102500
              HHH ~ PREV[J] ;      IF HHH { 0 THEN HHH ~ J - 1 ;          102600
              CHKX[J] ~ CHKX[HHH] + X[J] ;                                102700
              CHKY[J] ~ CHKY[HHH] + Y[J] ;                                102800
              CHKZ[J] ~ CHKZ[HHH] + Z[J] ;                                102900
              IF DI[J] = 0 THEN DI[J] ~ DI[J-1] ;                         103000
              IF T[J] = 0 THEN                                            103100
         BEGIN                                                            103200
              IF TYPE[J] ! "PRK" THEN T[J] ~ T[J-1] ;                     103300
              IF TYPE[J-1] = "RIG" THEN T[J] ~ T[J-2]                     103400
         END ;                                                            103500
              IF TH[J] = 0 THEN TH[J] ~ TH[J-1] ;                         103600
              IF TYPE[J] = "RIG" THEN T[J] ~ 0.5 | DI[J] ;                103700
              IF TMP THEN WRITE(FILE2, FOR3, OUTC)                        103800
              ELSE WRITE(FILE2, FOR4, OUTB) ;                             103900
              BRA[J] ~ I ;                                                104000
              IF J > 1 AND PREV[J]  = 0 THEN PREV[J]  ~ - 1 ;             104100
              IF PREV[J] > 0 THEN GO TO E101                              104200
              ELSE IF (X[J] ! 0 AND Y[J] ! 0 AND Z[J] ! 0) OR             104300
              (X[J-1] ! 0 AND Y[J-1] ! 0 AND Z[J-1] ! 0) THEN GO TO E102  104400
              ELSE IF TYPE[J] = "BRP" OR TYPE[J] = "PRK" THEN GO TO E103  104500
              ELSE IF TYPE[J] = "ANK" THEN GO TO E104                     104600
              ELSE IF TYPE[J] = "LOP" THEN GO TO E105 ELSE GO TO E110 ;   104700
              IF PREV[J] > 0 THEN                                         104800
E101 :   BEGIN                                                            104900
              IF TYPE[J-1] = "PRK" THEN                                   105000
              FOR H ~ 1 STEP 1 UNTIL I DO TRAV[II,H] ~ TRAVT[H,II] ~ 0 ;  105100
              PREVJ ;                                                     105200
              IF (X[J] ! 0 AND Y[J] ! 0 AND Z[J] ! 0) THEN                105300
E102 :   BEGIN                                                            105400
              G ~ PREV[J] ;                                               105500
              IF G < 1 THEN G ~ J - 1 ;                                   105600
              IF TYPE[G] = "BRP" OR G ! J - 1 THEN I ~ I - 1 ;            105700
              BRPT ;      BRA[J] ~ I                                      105800
         END ;                                                            105900
              IF (X[J-1] ! 0 AND Y[J-1] ! 0 AND Z[J-1] ! 0 AND            106000
              PREV[J] < 1 AND TYPE[J] = "   ") THEN                       106100
         BEGIN                                                            106200
              BRPT ;      BRA[J] ~ I                                      106300
         END ;                                                            106400
E103 :        IF TYPE[J] = "PRK" THEN                                     106500
         BEGIN                                                            106600
              IF TYPE[J-1] = "BRP" THEN I ~ I - 1 ;                       106700
              H ~ PREV[J] ;      IF H < 1 THEN PREV[J] ~ J - 1 ;          106800
              PREVJ ;      PREV[J] ~ H ;                                  106900
              NAN ~ NAN + 1 ;      BASBR[II] ~ I ;      II ~ II + 1 ;     107000
              FOR G ~ 1 STEP 1 UNTIL I - 1 DO TRAV[II,G] ~                107100
              TRAVT[G,II] ~ TRAV[II-1,G] ;      I ~ I + 1 ;               107200
              TRAV[II,I] ~ TRAVT[I,II] ~ 1                                107300
         END ;                                                            107400
              IF TYPE[J] = "BRP" THEN                                     107500
         BEGIN                                                            107600
              G ~ J ;      BRPT                                           107700
         END ;                                                            107800
E104 :        IF TYPE[J] = "ANK" THEN ANKR ;                              107900
E105 :        IF TYPE[J] = "LOP" THEN LOOP                                108000
         END ;                                                            108100
              GO TO E110 ;                                                108200
E110A :       NB ~ I ;                                                    108300
              RFX[1] ~ RFX[2] ;  RFY[1] ~ RFY[2] ; RFZ[1] ~ RFZ[2] ;      108400
              NBAS ~ NAN + NL ;      NM ~ J - 1 ;                         108500
              NN ~ 6 | NB ;         NNN ~ 6 | NBAS ;                      108600
              FE ;                                                        108700
              FOR G ~ 1 STEP 1 UNTIL 6 DO FOR H ~ 1 STEP 1 UNTIL 6 DO     108800
              FOR I ~ 1 STEP 1 UNTIL NB DO D[G,H,I]~0.0 ;                 108900
              SHC ;                                                       109000
COMMENT       GENERATING THE CONNECTION MATRIX ;                          109100
              FOR G ~ 1 STEP 1 UNTIL NN DO                                109200
              FOR H ~ 1 STEP 1 UNTIL NNN DO CNC[G,H] ~ 0 ;                109300
              FOR II ~ 1 STEP 1 UNTIL NBAS DO                             109400
         BEGIN                                                            109500
              III ~ BASBR[II] ;                                           109600
              FOR I ~ 1 STEP 1 UNTIL NB DO                                109700
              IF TRAV[II,I] ! 0 THEN                                      109800
         BEGIN                                                            109900
              KTX ~ RFX[III] - RFX[I] ;                                   110000
              KTY ~ RFY[III] - RFY[I] ;                                   110100
              KTZ ~ RFZ[III] - RFZ[I] ;                                   110200
              BM ;                                                        110300
              KK ~ 6 | I - 6 ;         KKK ~ 6 | II - 6 ;                 110400
              IF BOL[I] ! 0 THEN                                          110500
         BEGIN                                                            110600
              TMP ~ FALSE ;      JJJ ~ J ~ BOL[I] ;                       110700
              IF R[J] ! 0 THEN SKEB ELSE SKEW ;                           110800
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                 110900
              FOR H ~ 1 STEP 1 UNTIL 6 DO                                 111000
         BEGIN                                                            111100
              KTE ~ 0 ;                                                   111200
              FOR HHH ~ 1 STEP 1 UNTIL 6 DO                               111300
              KTE ~ TR[G,HHH] | BMAT[HHH,H] + KTE ;                       111400
              CNC[KK+G,KKK+H] ~ KTE                                       111500
         END                                                              111600
         END                                                              111700
              ELSE                                                        111800
         BEGIN                                                            111900
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                 112000
              FOR H ~ 1 STEP 1 UNTIL 6 DO                                 112100
              CNC[KK+G,KKK+H] ~ BMAT[G,H] ;                               112200
         END                                                              112300
         END                                                              112400
         END ;                                                            112500
COMMENT       GENERATION OF CONNECTION MATRIX FOR PARTIALLY               112600
              CONSTRAINED SYSTEMS. ;                                      112700
              FOR G ~ 1 STEP 1 UNTIL NNN DO TEMPR[G] ~ 1.0 ;    KK ~ 0 ;  112800
              FOR II ~ 1 STEP 1 UNTIL NBAS DO                             112900
         BEGIN                                                            113000
              III ~ BASBR[II] ;                                           113100
              IF PRN[III] THEN                                            113200
         BEGIN                                                            113300
              GG ~ ENTIER (D[2,2,III]) ;     GGG ~ 6 | II  - 6 ;          113400
              FOR J ~ 1 STEP 1 UNTIL 6 DO IF J ! GG THEN TEMPR[GGG+J] ~0  113500
         END                                                              113600
         END ;                                                            113700
              FOR H ~ 1 STEP 1 UNTIL NNN DO                               113800
              IF TEMPR[H] = 0 THEN KK ~ KK + 1                            113900
              ELSE IF KK > 0 THEN                                         114000
         BEGIN                                                            114100
              DEL[H-KK] ~ DEL[H] ;                                        114200
              FOR G ~ 1 STEP 1 UNTIL NN DO                                114300
              CNC[G,H-KK] ~ CNC[G,H]                                      114400
         END ;                                                            114500
              NNN ~ NNN - KK ;                                            114600
              FOR G ~ 1 STEP 1 UNTIL NN DO TEMPR[G] ~ 1.0 ;    KK ~ 0 ;   114700
              FOR I ~ 1 STEP 1 UNTIL NB DO                                114800
              IF PRN[I] THEN                                              114900
         BEGIN                                                            115000
              GG ~ ENTIER (D[2,2,I]) ;      GGG ~ 6 | I - 6 ;             115100
              FOR J ~ 1 STEP 1 UNTIL 6 DO IF J ! GG THEN TEMPR[GGG+J] ~0  115200
         END ;                                                            115300
              FOR G ~ 1 STEP 1 UNTIL NN DO                                115400
              IF TEMPR[G] = 0 THEN KK ~ KK + 1                            115500
              ELSE IF KK > 0 THEN FOR H ~ 1 STEP 1 UNTIL NNN DO           115600
              CNC[G-KK,H] ~ CNC[G,H] ;      NN ~ NN - KK ;                115700
COMMENT       THE TRANSPOSED MATRIX ;                                     115800
              FOR G ~ 1 STEP 1 UNTIL NN DO                                115900
              FOR H ~ 1 STEP 1 UNTIL NNN DO                               116000
              CNCT[H,G] ~ CNC[G,H] ;                                      116100
              KK ~ 0 ;                                                    116200
              FOR I ~ 1 STEP 1 UNTIL NB DO                                116300
         BEGIN                                                            116400
              GGG ~ 6 | I - 6 - 5 | KK ;                                  116500
              IF PRN[I] THEN                                              116600
         BEGIN                                                            116700
              GGG ~ GGG + 1 ;     KK ~ KK + 1 ;                           116800
              KTE ~ D[1,1,I] ;                                            116900
              FOR GG ~ 1 STEP 1 UNTIL NNN DO                              117000
              TEMP[GG,GGG] ~ CNCT[GG,GGG] | KTE ;                         117100
         END                                                              117200
              ELSE FOR H ~ 1 STEP 1 UNTIL 6 DO                            117300
              FOR GG ~ 1 STEP 1 UNTIL NNN DO                              117400
         BEGIN                                                            117500
              KTE ~ 0 ;                                                   117600
              FOR G ~ 1 STEP 1 UNTIL 6 DO                                 117700
              KTE ~ CNCT[GG,GGG+G] | D[G,H,I] + KTE ;                     117800
              TEMP[GG,GGG+H] ~ KTE                                        117900
         END                                                              118000
         END ;                                                            118100
              FOR G ~ 1 STEP 1 UNTIL NNN DO                               118200
              FOR H ~ 1 STEP 1 UNTIL NNN DO                               118300
         BEGIN                                                            118400
              KTE ~ 0 ;                                                   118500
              FOR HH ~ 1 STEP 1 UNTIL NN DO                               118600
              KTE ~ TEMP[G,HH] | CNC[HH,H] + KTE ;                        118700
              CNCT[G,H] ~ KTE                                             118800
         END ;                                                            118900
              H ~ NNN + 1 ;                                               119000
              FOR G ~ 1 STEP 1 UNTIL NNN DO CNCT[G,H] ~ DEL[G] ;          119100
              JE(CNCT, NNN, MF) ;                                         119200
              MOMEN ;                                                     119300
              WRITE(FILE2[PAGE]) ;     WRITE(FILE2, TI12) ;               119400
              FOR I ~ 0 STEP 1 UNTIL NAN DO WRITE(FILE2, FOR14, OUT14) ;  119500
              WRITE(FILE2[PAGE]) ;      WRITE(FILE2[PAGE],FOR19,OUT19);   119600
              GO TO E109 ;                                                119700
E128 :        WRITE (FILE2[PAGE]) ;      WRITE(FILE2, FOR16)              119800
         END ;                                                            119900
         END.                                                             120000
