 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
         BEGIN                                                          CMNT0000
                                                                        CMNT0001
COMMENT       THIS PROCEDURE EVALUATES THE ELLIPTIC INTEGRALS           CMNT0002
              OF THE FIRST AND SECOND KIND.                             CMNT0003
                                                                        CMNT0004
              STEPHEN MATZNER                                           CMNT0005
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION).           CMNT0006
                                                                        CMNT0007
              CARD SEQUENCE BEGINS WITH ELLI0001,                       CMNT0008
              FIRST RELEASE 01-02-1964  ;                               CMNT0009
                                                                        CMNT0010
                                                                        ELLI0001
REAL PROCEDURE INCOMPLETEF(A,P)  ;                                      ELLI0002
VALUE         A, P  ;                                                   ELLI0003
REAL          A, P  ;                                                   ELLI0004
                                                                        ELLI0005
         BEGIN                                                          ELLI0006
REAL          K, A1, B, B1, SI  ;                                       ELLI0007
                                                                        ELLI0008
INTEGER       N, M  ;                                                   ELLI0009
                                                                        ELLI0010
              B ~ ABS(COS(A))  ;  B1 ~ ABS(SIN(A))  ;                   ELLI0011
              IF B1 < 0.9539 THEN                                       ELLI0012
         BEGIN                                                          ELLI0013
              K  ~ 1 ;  SI ~ COS(P)/SIN(P)  ;  N ~ 0  ;  A ~ B  ;       ELLI0014
                                                                        ELLI0015
              FOR M ~ 1, 2, 3 DO                                        ELLI0016
         BEGIN                                                          ELLI0017
              SI ~ (SI - A/SI)/2  ;                                     ELLI0018
              N ~ 2 | N + (1 - SIGN(SI))/2  ;                           ELLI0019
              K ~ (K +B)/2  ;  B ~ SQRT(A)  ;  A ~ K | B  ;             ELLI0020
         END ;                                                          ELLI0021
              SI ~ (SI - A/SI)/2  ;  N ~ 2 | N + 1 - SIGN(SI)  ;        ELLI0022
              K ~ (K + B)/2  ;                                          ELLI0023
              INCOMPLETEF ~ ((2 | ARCTAN(K/SI) + N |                    ELLI0024
              3.14159265359)/K)/32  ;                                   ELLI0025
         END                                                            ELLI0026
              ELSE                                                      ELLI0027
         BEGIN                                                          ELLI0028
              K ~ 1  ;  B ~ B1  ;  A1 ~ B | B  ;                        ELLI0029
              SI ~ COS(P)/SIN(P)  ;                                     ELLI0030
                                                                        ELLI0031
              FOR M ~ 1, 2 DO                                           ELLI0032
         BEGIN                                                          ELLI0033
              A ~ K | B  ;                                              ELLI0034
              SI ~ K | SI + SQRT(K | K |(SI | SI + 1) - A1)  ;          ELLI0035
              K ~ (K + B)/2  ;  SI ~ SI/(K | 2)  ;                      ELLI0036
              B ~ SQRT(A)  ;  A1 ~ A  ;                                 ELLI0037
         END ;                                                          ELLI0038
              SI ~ K | SI + SQRT(K | K | (SI | SI + 1) - A)  ;          ELLI0039
              K ~ (K + B)/2  ;  SI ~ SI/(K | 2)  ;                      ELLI0040
              INCOMPLETEF ~ LN((1 + SQRT(1 + SI | SI))/SI)/K  ;         ELLI0041
         END ;                                                          ELLI0042
         END INCOMPLETEF  ;                                             ELLI0043
                                                                        ELLI0044
REAL PROCEDURE INCOMPLETEE(A,P)  ;                                      ELLI0045
VALUE         A, P  ;                                                   ELLI0046
REAL          A, P  ;                                                   ELLI0047
         BEGIN                                                          ELLI0048
REAL          U, V  ;                                                   ELLI0049
                                                                        ELLI0050
LABEL         L1, L2  ;                                                 ELLI0051
                                                                        ELLI0052
              U ~ ABS(COS(A))  ;  V ~ ABS(SIN(A))  ;                    ELLI0053
              IF V } 0.9539 THEN GO TO L1  ;                            ELLI0054
         BEGIN                                                          ELLI0055
REAL          K, A1, B, S1, S2, SI, CO  ;                               ELLI0056
                                                                        ELLI0057
INTEGER       N, M  ;                                                   ELLI0058
                                                                        ELLI0059
              B ~ U  ;  K ~ 1  ;  CO ~ COS(P)/SIN(P)  ;                 ELLI0060
              S1 ~ S2 ~ N ~ 0  ;  A1 ~ B | B  ;                         ELLI0061
                                                                        ELLI0062
              FOR M ~ 1, 2, 3, 4 DO                                     ELLI0063
         BEGIN                                                          ELLI0064
              A ~ K | B  ;  CO ~ (CO - A/CO)/2  ;                       ELLI0065
              N ~ 2 | N + (1 - SIGN(CO))/2  ;                           ELLI0066
              P ~ K | K  ;  K ~ (K + B)/2  ;  P ~ P - A1  ;             ELLI0067
              S2 ~ S2 + P | SIGN(1.5 - N + (N DIV 4) | 4)/              ELLI0068
              SQRT(K | K + CO | CO)  ;                                  ELLI0069
              S1 ~ S1/2 + P  ;  A1 ~ A  ;  B ~ SQRT(A)  ;               ELLI0070
         END ;                                                          ELLI0071
              INCOMPLETEE ~ (((2 | ARCTAN(K/CO) + (N + (1 -             ELLI0072
              SIGN(CO))/2) | 3.14159265359) | (0.25 - S1)/K)/2 + S2)/4 ;ELLI0073
              GO TO L2  ;                                               ELLI0074
         END ;                                                          ELLI0075
                                                                        ELLI0076
L1:      BEGIN                                                          ELLI0077
                                                                        ELLI0078
REAL          K, S1, CO, SI  ;                                          ELLI0079
                                                                        ELLI0080
REAL ARRAY    B[0:3], SINE[0:3]  ;                                      ELLI0081
                                                                        ELLI0082
INTEGER       M  ;                                                      ELLI0083
                                                                        ELLI0084
              K ~ 1  ;  S1  ~ U | U  ;  B[0] ~ V  ;  B[3] ~ 1 + V  ;    ELLI0085
              SI ~ SIN(P)  ;  CO ~ COS(P)/SI  ;  SINE[0] ~ SI  ;        ELLI0086
              CO ~ (CO + SQRT(CO | CO + 1 - V | V))/B[3]  ;             ELLI0087
              SI ~ CO | CO  ;                                           ELLI0088
                                                                        ELLI0089
              FOR M ~ 0, 1, 2 DO                                        ELLI0090
         BEGIN                                                          ELLI0091
              SINE[M + 1] ~ 1/SQRT(1 + SI)  ;  A ~ K | B[M]  ;          ELLI0092
              K ~ B[3]/2  ;  B[M + 1] ~ SQRT(A)  ;  P ~ K | K  ;        ELLI0093
              S1 ~ S1/2 + P - A  ;  B[3] ~ K + B[M + 1]  ;              ELLI0094
              CO ~ (K | CO + SQRT(P | (SI + 1) - A))/B[3]  ;            ELLI0095
              SI ~ CO | CO  ;                                           ELLI0096
         END ;                                                          ELLI0097
              SI ~ SQRT(1 + SI)  ;  P ~ 1/SI  ;  U ~ 0  ;  B[3] ~ K  ;  ELLI0098
                                                                        ELLI0099
              FOR M ~ 3, 2, 1, 0 DO                                     ELLI0100
              U ~ 2 | U + B[M] | (P - SINE[M])  ;                       ELLI0101
              INCOMPLETEE~ 4 | S1 | LN((1 + SI)/CO)/K + P + U  ;        ELLI0102
         END ;                                                          ELLI0103
                                                                        ELLI0104
L2:      END INCOMPLETEE  ;                                             ELLI0105
                                                                        ELLI0106
REAL PROCEDURE COMPLETEF(A)  ;                                          ELLI0107
VALUE         A  ;                                                      ELLI0108
REAL          A  ;                                                      ELLI0109
         BEGIN                                                          ELLI0110
REAL          A1, A2, B, K1  ;                                          ELLI0111
                                                                        ELLI0112
INTEGER       N  ;                                                      ELLI0113
                                                                        ELLI0114
              B ~ ABS(SIN(A))  ;                                        ELLI0115
              IF B } 0.9539 THEN                                        ELLI0116
         BEGIN                                                          ELLI0117
              A1 ~ (1 + B)/2  ;  B ~ SQRT(B)  ;  A2 ~ (A1 + B)/2  ;     ELLI0118
              K1 ~ COS(A)  ;  B ~ SQRT(A1 | B)  ;                       ELLI0119
              COMPLETEF ~ LN(128 | (A2 + B) | A2 |                      ELLI0120
              A1*2/K1*4)/(A2 + B)/2  ;                                  ELLI0121
         END                                                            ELLI0122
              ELSE                                                      ELLI0123
         BEGIN                                                          ELLI0124
              B ~ ABS(COS(A))  ;  A1 ~ 1  ;                             ELLI0125
                                                                        ELLI0126
                                                                        ELLI0127
              FOR N ~ 1, 2, 3 DO                                        ELLI0128
         BEGIN                                                          ELLI0129
              A2 ~ (A1 + B)/2  ;  B ~ SQRT(A1 | B)  ;  A1 ~ A2  ;       ELLI0130
         END ;                                                          ELLI0131
              COMPLETEF ~ 3.14159265359/(A1 + B)  ;                     ELLI0132
         END                                                            ELLI0133
         END COMPLETEF  ;                                               ELLI0134
                                                                        ELLI0135
REAL PROCEDURE COMPLETEE(A)  ;                                          ELLI0136
VALUE         A  ;                                                      ELLI0137
REAL          A  ;                                                      ELLI0138
         BEGIN                                                          ELLI0139
REAL          A1, A2, B, S, K1  ;                                       ELLI0140
                                                                        ELLI0141
INTEGER       N  ;                                                      ELLI0142
                                                                        ELLI0143
              B ~ ABS(SIN(A))  ;  K1 ~ ABS(COS(A))  ;                   ELLI0144
              IF B } 0.9539 THEN                                        ELLI0145
         BEGIN                                                          ELLI0146
              A1 ~ (1 + B)/2  ;S ~ K1 | K1/2 + A1 | A1 - B  ;           ELLI0147
              B ~ SQRT(B)  ;  A2 ~ (A1 + B)/2  ;  A ~ A1 | B  ;         ELLI0148
              S ~ S/2 + A2 | A2 - A  ;  B ~ SQRT(A)  ;                  ELLI0149
              COMPLETEE ~ (A2 + B)/2 + (S/(A2 + B)) |                   ELLI0150
              LN(128 | (A2 + B) | A2 | A1 | A1/(K1*4))  ;               ELLI0151
         END                                                            ELLI0152
              ELSE                                                      ELLI0153
         BEGIN                                                          ELLI0154
              B ~ K1  ;  A1 ~ 1  ;  S ~ 1 + B | B  ;                    ELLI0155
                                                                        ELLI0156
              FOR N ~ 1, 2, 3 DO                                        ELLI0157
         BEGIN                                                          ELLI0158
              A2 ~ (A1 + B)/2  ;  A ~ A1 | B  ;  A1 ~ A2  ;             ELLI0159
              S ~ S/2 - A1 | A1 + A  ;  B ~ SQRT(A)  ;                  ELLI0160
         END ;                                                          ELLI0161
              COMPLETEE ~ 12.5663706144 | S/(A1 + B)  ;                 ELLI0162
         END COMPLETEF  ;                                               ELLI0163
         END ;                                                          ELLI0164
                                                                        ELLI0165
REAL PROCEDURE ELLI(A,P,INT)  ;                                         ELLI0166
VALUE         A, P, INT  ;                                              ELLI0167
REAL          A, P  ;                                                   ELLI0168
INTEGER       INT  ;                                                    ELLI0169
         BEGIN                                                          ELLI0170
              ELLI ~ IF P ! 333 AND INT = 1 THEN INCOMPLETEE(A,P)       ELLI0171
              ELSE IF P ! 333 THEN INCOMPLETEF(A,P) ELSE                ELLI0172
              IF INT = 1 THEN COMPLETEE(A) ELSE COMPLETEF(A)  ;         ELLI0173
         END ;                                                          ELLI0174
FILE OUT      FOU 1 (1,15)  ;                                           CMNT0011
FORMAT        FMOU1("E[ ] =", E20.10),                                  CMNT0012
              FMOU2("F[ ] =", E20.10)  ;                                CMNT0013
              WRITE(FOU,FMOU1,ELLI(0.01745329252,0.01745329252,1)) ;    CMNT0014
              WRITE(FOU,FMOU2,ELLI(0.01745329252,0.01745329252,2)) ;    CMNT0015
              WRITE(FOU,FMOU1,ELLI(0.69813170080,0.69813170080,1)) ;    CMNT0016
              WRITE(FOU,FMOU2,ELLI(0.69813170080,0.69813170080,2)) ;    CMNT0017
              WRITE(FOU,FMOU1,ELLI(1.55334303428,1.55334303428,1)) ;    CMNT0018
              WRITE(FOU,FMOU2,ELLI(1.55334303428,1.55334303428,2)) ;    CMNT0019
              WRITE(FOU,FMOU1,ELLI(0.01745329252,0.69813170080,1)) ;    CMNT0020
              WRITE(FOU,FMOU2,ELLI(0.01745329252,0.69813170080,2)) ;    CMNT0021
              WRITE(FOU,FMOU1,ELLI(0.69813170080,1.55334303428,1)) ;    CMNT0022
              WRITE(FOU,FMOU2,ELLI(0.69813170080,1.55334303428,2)) ;    CMNT0023
              WRITE(FOU,FMOU1,ELLI(1.55334303428,0.01745329252,1)) ;    CMNT0024
              WRITE(FOU,FMOU2,ELLI(1.55334303428,0.01745329252,2)) ;    CMNT0025
              WRITE(FOU,FMOU1,ELLI(0.01745329252,1.55334303428,1)) ;    CMNT0026
              WRITE(FOU,FMOU2,ELLI(0.01745329252,1.55334303428,2)) ;    CMNT0027
              WRITE(FOU,FMOU1,ELLI(0.69813170080,0.01745329252,1)) ;    CMNT0028
              WRITE(FOU,FMOU2,ELLI(0.69813170080,0.01745329252,2)) ;    CMNT0029
              WRITE(FOU,FMOU1,ELLI(1.55334303428,0.69813170080,1)) ;    CMNT0030
              WRITE(FOU,FMOU2,ELLI(1.55334303428,0.69813170080,2)) ;    CMNT0031
         END .                                                          CMNT0032
