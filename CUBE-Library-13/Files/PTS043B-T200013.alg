PROCEDURE     PHBD(N, RT, EX, V, DELTA, GAMMA) ;                        PHBD   1
                                                                        PHBD   2
COMMENT       THIS PROCEDURE COMPUTES ACTIVITY COEFFICIENTS OF MULTI-   PHBD   3
              COMPONENT SYSTEMS, USING THE HILDEBRAND EQUATION,         PHBD   4
                                                                        PHBD   5
              CHIEN-SHIH LU                                             PHBD   6
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION,       PHBD   7
              FIRST RELEASE DATE: 12-1-63                               PHBD   8
                                                                        PHBD   9
              THE INPUT PARAMETERS ARE                                  PHBD  10
              N   - NUMBER OF COMPONENTS, INTEGER                       PHBD  11
              RT  - R|T  WHERE  R = UNIVERSAL GAS CONSTANT              PHBD  12
                                T = TEMPERATURE , REAL                  PHBD  13
              EX  - MOL FRACTIONS, REAL ARRAY                           PHBD  14
              V   - MOLAL VOLUME, REAL ARRAY                            PHBD  15
              DELTA - SOLUBILITY PARAMETER, REAL ARRAY                  PHBD  16
                                                                        PHBD  17
              THE OUTPUT PARAMETER IS                                   PHBD  18
              GAMMA - ACTIVITY COEFFICIENTS, REAL ARRAY ;               PHBD  19
                                                                        PHBD  20
VALUE         N, RT ;                                                   PHBD  21
INTEGER       N ;                                                       PHBD  22
REAL          RT ;                                                      PHBD  23
ARRAY         EX, V, DELTA, GAMMA[0] ;                                  PHBD  24
                                                                        PHBD  25
         BEGIN                                                          PHBD  26
                                                                        PHBD  27
INTEGER       J ;                                                       PHBD  28
REAL          DELA, TRS1, TRS2 ;                                        PHBD  29
                                                                        PHBD  30
              TRS2 ~ DELA ~ 0 ;
                                                                        PHBD  33
              FOR  J ~ 1  STEP 1 UNTIL N  DO                            PHBD  34
         BEGIN                                                          PHBD  35
              TRS1 ~ EX[J] | V[J] ;                                     PHBD  36
              DELA ~ DELTA[J] | TRS1 + DELA ;                           PHBD  37
              TRS2 ~ TRS1 + TRS2 ;                                      PHBD  38
         END ;                                                          PHBD  39
              DELA ~ DELA / TRS2 ;                                      PHBD  40
                                                                        PHBD  41
              FOR  J ~ 1  STEP 1 UNTIL N  DO                            PHBD  42
              GAMMA[J] ~ EXP(V[J] | (DELTA[J] - DELA) * 2 / RT) ;       PHBD  43
         END  PHBD ;                                                    PHBD  44
