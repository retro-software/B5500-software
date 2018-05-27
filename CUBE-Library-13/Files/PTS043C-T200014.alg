 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN

COMMENT       THE REAL ARRAYS, TR2S1, TR2S2[0:N,0:N], TR1S1, TR1S2[0:N],
              MUST BE DECLARED IN THE MAIN PROGRAM ;

REAL ARRAY    TR1S1, TR1S2[0:10], TR2S1, TR2S2[0:10, 0:10] ;

PROCEDURE     PMVL(NN, MM, N, A, C, SQ, EX, GAMMA) ;                    PMVL   1
                                                                        PMVL   2
COMMENT       THIS PROCEDURE COMPUTES ACTIVITY COEFFICIENTS OF MULTI-   PMVL   3
              COMPONENT SYSTEMS, USING MODIFIED VAN LAAR EQUATION (BY   PMVL   4
              CLINE BLACK),                                             PMVL   5
                                                                        PMVL   6
              CHIEN-SHIH LU                                             PMVL   7
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION,       PMVL   8
              FIRST RELEASE DATE: 12-1-63                               PMVL   9
                                                                        PMVL  10
              THE INPUT PARAMETERS ARE                                  PMVL  11
              NN  - NUMBER OF CLASSES, INTEGER                          PMVL  12
              MM  - NUMBER OF COMPONENTS, INTEGER                       PMVL  13
              N   - CLASS IDENTIFIER, INTEGER ARRAY,                    PMVL  14
                    FOR QUATERNARY SYSTEM OF THREE CLASSES, THE ARRAY   PMVL  15
                    1,3,4  INDICATES THAT THE FIRST AND THE SECOND COM- PMVL  16
                    PONENTS BELONG TO THE CLASS ONE, THE THIRD COMPONENTPMVL  17
                    BELONGS TO THE CLASS TWO, AND THE FOURTH COMPONENT  PMVL  18
                    BELONGS TO THE CLASS THREE,                         PMVL  19
              A   - VAN LAAR COEFFICIENTS, REAL ARRAY                   PMVL  20
              C   - MODIFICATION COEFFICIENTS, REAL ARRAY               PMVL  21
              SQ  - EMPIRICAL COEFFICIENTS, REAL ARRAY                  PMVL  22
                    THE NUMERICAL VALUES OF SQ[I] MAY BE OBTAINED BY THEPMVL  23
                    RELATIONSHIP                                        PMVL  24
                           SQ[I] ~ (A[I,J] - C[I,J]) / (A[J,I] - C[I,J) PMVL  25
                    WHERE J DENOTE THE KEY COMPONENT AND SQ[J] ~ 1,     PMVL  26
              EX  - MOL FRACTIONS, REAL ARRAY                           PMVL  27
                                                                        PMVL  28
              THE OUTPUT PARAMETER IS                                   PMVL  29
              GAMMA - ACTIVITY COEFFICIENTS, REAL ARRAY ;               PMVL  30
                                                                        PMVL  31
VALUE         NN, MM, N, A, C, SQ, EX ;                                 PMVL  32
              GAMMA - ACTIVITY COEFFICIENTS, REAL ARRAY ;               PMVL  30
                                                                        PMVL  31
VALUE    NN,MM, N, A, C, SQ, EX  ;                                      PMVL  32
INTEGER       NN, MM ;                                                  PMVL  33
INTEGER ARRAY N[0] ;                                                    PMVL  34
REAL ARRAY    A, C[0,0], SQ, EX, GAMMA[0] ;                             PMVL  35
                                                                        PMVL  36
         BEGIN                                                          PMVL  37
                                                                        PMVL  38
INTEGER       I, J, K, M, R, S, R1, S1, SK, SM, SR, SS, M1 ;            PMVL  39
REAL          Q, TRS1, TRS2, TRS3, TRS4, TRS5, TRS6, CESI ;             PMVL  40
LABEL         L1, L2, L3 ;                                              PMVL  42
                                                                        PMVL  43
              Q ~ 0 ;                                                   PMVL  44

              FOR  I ~ 1  STEP 1 UNTIL MM  DO                           PMVL  45
              Q ~ Q + SQ[I] | EX[I] ;                                   PMVL  46
                                                                        PMVL  47
              FOR  I ~ 1  STEP 1 UNTIL MM  DO                           PMVL  48
         BEGIN                                                          PMVL  49
                                                                        PMVL  50
              FOR  J ~ 1  STEP 1 UNTIL MM  DO                           PMVL  51
         BEGIN                                                          PMVL  52
              TR2S1[I,J] ~ A[I,J] - C[I,J]  ;                           PMVL  53
              TR2S2[I,J] ~ TR2S1[I,J] / SQ[I]                           PMVL  54
         END ;                                                          PMVL  55
              TR1S1[I] ~ SQ[I] | EX[I] / Q ;                            PMVL  56
         END ;                                                          PMVL  57
                                                                        PMVL  58
              FOR  I ~ 1  STEP 1 UNTIL NN  DO                           PMVL  59
         BEGIN                                                          PMVL  60
              TRS1 ~ 0 ;                                                PMVL  61
                                                                        PMVL  62
              FOR  J ~ 1  STEP 1 UNTIL (N[I] - N[I-1])  DO              PMVL  63
              TRS1 ~ TRS1 + EX[N[I-1]+J] ;                              PMVL  64
              TR1S2[I] ~ TRS1                                           PMVL  65
         END ;                                                          PMVL  66
              S ~ 1 ;   K ~ N[S] ;                                      PMVL  67
                                                                        PMVL  69
              FOR  I ~ 1  STEP 1 UNTIL MM  DO                           PMVL  70
         BEGIN                                                          PMVL  71
              IF  I > K  THEN                                           PMVL  72
         BEGIN                                                          PMVL  73
              S ~ S + 1 ;    K ~ N[S]                                   PMVL  74
         END ;                                                          PMVL  76
              TRS1 ~ TRS2 ~ TRS3 ~ 0 ;                                  PMVL  77
                                                                        PMVL  78
              FOR  R ~ 1  STEP 1 UNTIL NN  DO                           PMVL  79
         BEGIN                                                          PMVL  80
              IF  R ! S  THEN                                           PMVL  81
         BEGIN                                                          PMVL  82
              TRS4 ~ TRS5 ~  0 ;                                        PMVL  83
                                                                        PMVL  84
              FOR  R1 ~ 1  STEP 1 UNTIL (N[R] - N[R-1])  DO             PMVL  85
         BEGIN                                                          PMVL  86
              SR ~ R1 + N[R-1] ;                                        PMVL  87
              TRS4 ~ TRS4 + C[I,SR] | EX[SR] ;                          PMVL  88
                                                                        PMVL  89
              FOR  S1 ~ 1  STEP 1 UNTIL (N[S] - N[S-1])  DO             PMVL  90
         BEGIN                                                          PMVL  91
              SS ~ S1 + N[S-1] ;                                        PMVL  92
              TRS5 ~ TRS5 + C[SS,SR] | EX[SS] | EX[SR]                  PMVL  93
         END ;                                                          PMVL  94
         END ;                                                          PMVL  95
              TRS6 ~ TR1S2[S] - TR1S2[R] ;                              PMVL  96
              TRS1 ~ TRS6 | TRS6 | TRS4 + TRS1 ;                        PMVL  97
              TRS2 ~ TRS2 + TRS6 | (2 - 3 | TRS6) | TRS5 ;              PMVL  98
                                                                        PMVL  99
              FOR  M ~ 1  STEP 1 UNTIL NN  DO                           PMVL 100
         BEGIN                                                          PMVL 101
              IF  M = R  OR  M = S  THEN  GO TO L1 ;                    PMVL 102
              TRS6 ~ 0 ;                                                PMVL 103
                                                                        PMVL 104
              FOR  R1 ~ 1  STEP 1 UNTIL (N[R] - N[R-1])  DO             PMVL 105
         BEGIN                                                          PMVL 106
              SR ~ R1 + N[R-1] ;                                        PMVL 107
                                                                        PMVL 108
              FOR  M1 ~ 1  STEP 1 UNTIL (N[M] - N[M-1])  DO             PMVL 109
         BEGIN                                                          PMVL 110
              SM ~ M1 + N[M-1] ;                                        PMVL 111
              TRS6 ~ TRS6 + C[SR,SM] | EX[SR] | EX[SM]                  PMVL 112
         END ;                                                          PMVL 113
         END ;                                                          PMVL 114
              TRS3 ~ TRS3 + (TR1S2[R] - TR1S2[M]) * 2 | TRS6 ;          PMVL 115
L1:      END ;                                                          PMVL 116
         END ;                                                          PMVL 117
         END ;                                                          PMVL 118
              CESI ~ TRS1 + TRS2 - 3 | TRS3 / 2 ;                       PMVL 119
              TRS1 ~ TRS2 ~ 0 ;                                         PMVL 120
                                                                        PMVL 121
              FOR  J ~ 1  STEP 1 UNTIL MM  DO                           PMVL 122
         BEGIN                                                          PMVL 123
              TRS1 ~ TRS1 + TR2S2[I,J] | TR1S1[J] * 2 ;                 PMVL 124
              TRS3 ~ 0 ;                                                PMVL 125
                                                                        PMVL 126
              FOR  SK ~ 1  STEP 1 UNTIL MM  DO                          PMVL 127
         BEGIN                                                          PMVL 128
              IF  SK = J  THEN  GO TO L3 ;                              PMVL 129
              TRS3 ~ TRS3 + TR1S1[J] | TR1S1[SK] | (TR2S2[I,J] +        PMVL 130
              TR2S2[I,SK] - TR2S2[J,SK]) ;                              PMVL 131
L3:      END ;                                                          PMVL 132
              TRS2 ~ TRS2 + TRS3 ;                                      PMVL 133
L2:      END ;                                                          PMVL 134
              GAMMA[I] ~ SQ[I] | (TRS1 + 0.5 | TRS2) + CESI ;           PMVL 135
              GAMMA[I] ~ EXP(GAMMA[I] | 2.302585) ;                     PMVL 136
         END ;                                                          PMVL 137
         END  PMVL ;                                                    PMVL 138
         END.
