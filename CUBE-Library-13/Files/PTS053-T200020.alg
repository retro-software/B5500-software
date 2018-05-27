PROCEDURE     IFLASH (SUMF, N, Z, K, X, Y, L, V, SUML, SUMV) ;          IFLP0001

COMMENT       ISOTHERMAL-ISOBARIC FLASH PROCEDURE                       IFLP0002
              BY DONALD L. VOSS                                         IFLP0003
              PROFESSIONAL SERVICES GROUP,  BURROUGHS CORPORATION       IFLP0004
              CARD SEQUENCE CODE STARTS WITH IFLP0001.                  IFLP0005
              FIRST RELEASE DATE   5-5-63                               IFLP0006

              THIS PROCEDURE CALCULATES THE PHASE COMPOSITIONS OF       IFLP0007
              VAPOR-LIQUID MIXTURES IN WHICH EQUILIBRIUM EXISTS BETWEEN IFLP0008
              THE LIQUID AND VAPOR.  IF THE CONDITIONS ARE SUCH THAT    IFLP0009
              THE MIXTURE IS ALL LIQUID OR ALL VAPOR, THE PROCEDURE     IFLP0010
              EXITS WITH THE APPROPRIATE OUTPUT RESULTS.  FOR TWO PHASE IFLP0011
              MIXTURES, A NEWTON-RAPHSON METHOD IS USED TO DETERMINE    IFLP0012
              THE PHASE COMPOSITIONS.                                   IFLP0013

              THE INPUT PARAMETERS ARE                                  IFLP0014
              SUMF - MOLES OF FEED  - REAL                              IFLP0015
              N    - TOTAL NUMBER OF COMPONENTS  - INTEGER              IFLP0016
              Z    - MOLE FRACTIONS OF FEED COMPONENTS  - REAL ARRAY    IFLP0017
                   [0:N]                                                IFLP0018
              K    - COMPONENT K-RATIOS  - REAL ARRAY [0:N]             IFLP0019

              THE OUTPUT PARAMETERS ARE                                 IFLP0020
              X    - COMPONENT LIQUID MOLE FRACTIONS  - REAL ARRAY [0:N]IFLP0021
              Y    - COMPONENT VAPOR MOLE FRACTIONS  - REAL ARRAY [0:N] IFLP0022
              L    - COMPONENT LIQUID MOLES  - REAL ARRAY [0:N]         IFLP0023
              V    - COMPONENT VAPOR MOLES  - REAL ARRAY [0:N]          IFLP0024
              SUML - TOTAL MOLES OF LIQUID  - REAL                      IFLP0025
              SUMV - TOTAL MOLES OF VAPOR  - REAL  ;                    IFLP0026

VALUE         SUMF, N ;                                                 IFLP0027
REAL          SUMF, SUML, SUMV ;                                        IFLP0028
INTEGER       N ;                                                       IFLP0029
ARRAY         Z[0], K[0], X[0], Y[0], L[0], V[0] ;                      IFLP0030

         BEGIN                                                          IFLP0031
REAL          TRS1, TRS2, TRS3, NRF, PNRF ;                             IFLP0032
INTEGER       I ;                                                       IFLP0033
LABEL         I1, I2, I3, I4, I5 ;                                      IFLP0034

COMMENT       DETERMINE IF TWO PHASES EXIST ;                           IFLP0035
              TRS1 ~ TRS2 ~ 0 ;                                         IFLP0036
              FOR I ~ 1 STEP 1 UNTIL N DO                               IFLP0037
         BEGIN                                                          IFLP0038
              IF K[I] = 0 THEN                                          IFLP0039
         BEGIN                                                          IFLP0040
              TRS1 ~ 1.1 ;     GO TO I1                                 IFLP0041
         END  ;                                                         IFLP0042
              TRS1 ~ Z[I]/K[I] + TRS1 ;                                 IFLP0043
I1:           TRS2 ~ Z[I] | K[I] + TRS2                                 IFLP0044
         END  ;                                                         IFLP0045
              IF TRS1 > 1.0 THEN  GO TO I2 ;                            IFLP0046

COMMENT       VAPOR PHASE ONLY ;                                        IFLP0047
              TRS1 ~ 0 ;     GO TO I5 ;                                 IFLP0048
I2:           IF TRS2 > 1.0 THEN  GO TO I3 ;                            IFLP0049

COMMENT       LIQUID PHASE ONLY ;                                       IFLP0050
              TRS1 ~ 1.0 ;     GO TO I5 ;                               IFLP0051

COMMENT       START CALCULATIONS WITH SUML/SUMF = 0.5 ;                 IFLP0052
I3:           TRS1 ~ 0.5 ;                                              IFLP0053

COMMENT       ITERATE TO DETERMINE SUML/SUMF ;                          IFLP0054
I4:           NRF ~ PNRF ~ 0 ;                                          IFLP0055
              FOR I ~ 1 STEP 1 UNTIL N DO                               IFLP0056
         BEGIN                                                          IFLP0057
              TRS2 ~ -K[I] + 1.0 ;                                      IFLP0058
              TRS3 ~ K[I] + TRS1 | TRS2 ;                               IFLP0059
              NRF ~ Z[I] | TRS2/TRS3 + NRF ;                            IFLP0060
              PNRF ~ -Z[I] | (TRS2/TRS3)*2 + PNRF                       IFLP0061
         END  ;                                                         IFLP0062
              TRS2 ~ NRF/PNRF ;     TRS1 ~ TRS1 - TRS2 ;                IFLP0063
              IF TRS1 < 0 THEN  TRS1 ~ 0                                IFLP0064
              ELSE IF TRS1 > 1.0 THEN  TRS1 ~ 1.0 ;                     IFLP0065
              IF ABS(TRS2) > 1.0@-6  THEN  GO TO I4 ;                   IFLP0066

COMMENT       CALCULATE PHASE COMPOSITIONS ;                            IFLP0067
I5:           SUML ~ TRS1 | SUMF ;     SUMV ~ SUMF - SUML ;             IFLP0068
              FOR I ~ 1 STEP 1 UNTIL N DO                               IFLP0069
         BEGIN                                                          IFLP0070
              IF TRS1 ! 0  THEN                                         IFLP0071
              X[I] ~ Z[I] / (K[I] | (1.0 - TRS1) + TRS1)                IFLP0072
              ELSE  X[I] ~ 0 ;                                          IFLP0073
              L[I] ~ X[I] | SUML ;                                      IFLP0074
              V[I] ~ Z[I] | SUMF - L[I] ;                               IFLP0075
              IF TRS1 ! 1.0 THEN  Y[I] ~ V[I]/SUMV                      IFLP0076
              ELSE  Y[I] ~ 0                                            IFLP0077
         END                                                            IFLP0078

         END IFLASH ;                                                   IFLP0079
