PROCEDURE     PEMW(M, E, A, C, D, EX, G) ;                              PEMW   1
                                                                        PEMW   2
COMMENT       THIS PROCEDURE COMPUTES ACTIVITY COEFFICIENTS OF BINARY,  PEMW   3
              TERNARY, AND QUATERNARY SYSTEMS, USING EXTENDED MARGULES  PEMW   4
              EQUATION (BY K. WOHL),                                    PEMW   5
                                                                        PEMW   6
              CHIEN-SHIH LU                                             PEMW   7
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION,       PEMW   8
              FIRST RELEASE DATE: 12-1-63                               PEMW   9
                                                                        PEMW  10
              THE INPUT PARAMETERS ARE                                  PEMW  11
              M  - NUMBER OF COMPONENTS, INTEGER                        PEMW  12
              E  - COEFFICIENT  FOR THE MARGULES EQUATION, REAL         PEMW  13
              A  - COEFFICIENTS FOR THE MARGULES EQUATION, REAL ARRAY   PEMW  14
              C  - COEFFICIENTS FOR THE MARGULES EQUATION, REAL ARRAY   PEMW  15
                   THE COEFFICIENT C[I,I,J,K] IS STORED IN C[10-(I+J+K),PEMW  16
                   I],                                                  PEMW  17
              D  - COEFFICIENTS FOR THE MARGULES EQUATION, REAL ARRAY   PEMW  18
              EX - MOLE FRACTIONS, REAL ARRAY                           PEMW  19
                                                                        PEMW  20
              THE OUTPUT PARAMETER IS                                   PEMW  21
              G - ACTIVITY COEFFICIENTS, REAL ARRAY ;                   PEMW  22
                                                                        PEMW  23
VALUE         M, E, A, C, D, EX ;                                       PEMW  24
INTEGER       M ;                                                       PEMW  25
REAL          E ;                                                       PEMW  26
REAL ARRAY    A, C, D[0,0], EX, G[0] ;                                  PEMW  27
                                                                        PEMW  28
         BEGIN                                                          PEMW  29
                                                                        PEMW  30
INTEGER       I, J, K, II, MM, JJ ;                                     PEMW  31
REAL          TRS, TRS1, TRS2 ;                                         PEMW  32
LABEL         L1, L2, L3, L4, L5 ;                                      PEMW  33
              IF  M = 3  THEN  MM ~ 6  ELSE  MM ~ 10 ;                  PEMW  34
                                                                        PEMW  35
              FOR  I ~ 1  STEP 1 UNTIL M  DO                            PEMW  36
         BEGIN                                                          PEMW  37
              TRS ~ 0 ;                                                 PEMW  38
                                                                        PEMW  39
              FOR  J ~ 1  STEP 1 UNTIL M  DO                            PEMW  40
         BEGIN                                                          PEMW  41
              IF  J = I  THEN  GO TO L1 ;                               PEMW  42
              TRS1 ~ A[I,J] + 2 | EX[I] | (A[J,I] - A[I,J] - D[I,J]) +  PEMW  43
              3 | EX[I] | EX[I] | D[I,J] ;                              PEMW  44
              TRS ~ TRS + EX[J] | EX[J] | TRS1 ;                        PEMW  45
L1:      END ;                                                          PEMW  46
              IF  M = 2  THEN                                           PEMW  48
         BEGIN                                                          PEMW  49
              G[I] ~ TRS ;   GO TO L2                                   PEMW  50
         END ;                                                          PEMW  52
                                                                        PEMW  53
              FOR  J ~ 1  STEP 1 UNTIL M  DO                            PEMW  54
         BEGIN                                                          PEMW  55
              IF  J = 1  THEN  GO TO L3 ;                               PEMW  56
                                                                        PEMW  57
              FOR  K ~ J  STEP 1 UNTIL M  DO                            PEMW  58
         BEGIN                                                          PEMW  59
              IF  K = J  OR  K = I  THEN  GO TO L4 ;                    PEMW  60
              II ~ MM - I - J - K ;                                     PEMW  61
              TRS1 ~ 0 ;                                                PEMW  62
                                                                        PEMW  63
              FOR  JJ ~ I, J, K  DO                                     PEMW  64
              TRS1 ~ TRS1 + EX[JJ] | C[II,JJ] ;                         PEMW  66
              TRS2 ~ 0.5 | (A[I,J] + A[J,I] + A[I,K] + A[K,I] - A[J,K] -PEMW  68
              A[K,J]) + EX[I] | (A[J,I] - A[I,J] + A[K,I] -             PEMW  69
              A[I,K]) + (EX[K] - EX[J]) | (A[K,J] - A[J,K]) -           PEMW  70
              EX[I] | C[II,I] - (1 - 3 | EX[I]) | TRS1 ;                PEMW  71
              TRS1 ~ EX[J] | EX[K] ;                                    PEMW  72
              TRS ~ TRS + (TRS2 + 3 | TRS1 | D[J,K]) | TRS1 ;           PEMW  73
L4:      END ;                                                          PEMW  74
L3:      END ;                                                          PEMW  75
              IF  M = 3  THEN                                           PEMW  76
         BEGIN                                                          PEMW  77
              G[I] ~ TRS ;   GO TO L2 ;                                 PEMW  78
         END ;                                                          PEMW  80
              TRS1 ~ 1 ;   TRS2 ~ 0 ;                                   PEMW  81
                                                                        PEMW  83
              FOR  J ~ 1  STEP 1 UNTIL M  DO                            PEMW  84
         BEGIN                                                          PEMW  85
              IF  I = J  THEN  GO TO L5 ;                               PEMW  86
              TRS1 ~ TRS1 | EX[J] ;                                     PEMW  87
              TRS2 ~ TRS2 + C[ I,J] | EX[J] ;                           PEMW  88
L5:      END ;                                                          PEMW  89
              G[I] ~ TRS + TRS1 | (3 | TRS2 - (1 - 3 | EX[I]) | E) ;    PEMW  90
L2:           G[I] ~ EXP(G[I] | 2.302585) ;                             PEMW  91
         END ;                                                          PEMW  92
         END  PEMW ;                                                    PEMW  93
