         BEGIN                                                          PROG0001
COMMENT       PROCEDURE TO PERFORM COMPLEX ARITHMETIC                   PROG0002
              AND OTHER COMPLEX OPERATIONS.                             PROG0003
                                                                        PROG0004
              A. LYNN MEYER                                             PROG0005
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION)            PROG0006
                                                                        PROG0007
              CARD SEQUENCE CODE STARTS WITH CPLX0001                   PROG0008
              FIRST RELEASE DATE 12-2-63                               ;PROG0009
                                                                        PROG0010
PROCEDURE     COMPLEX(A,CONVECT)  ;                                     CPLX0001
REAL ARRAY    A[0,0]  ;                                                 CPLX0002
INTEGER ARRAY CONVECT[0]  ;                                             CPLX0003
                                                                        CPLX0004
         BEGIN                                                          CPLX0005
REAL PROCEDURE     SINH(X1)  ;                                          CPLX0006
VALUE         X1  ;                                                     CPLX0007
REAL          X1  ;                                                     CPLX0008
                                                                        CPLX0009
         BEGIN                                                          CPLX0010
REAL          Y  ;                                                      CPLX0011
              Y ~ EXP(X1)  ;                                            CPLX0012
              SINH ~ (Y - 1.0/Y) | 0.5                                  CPLX0013
         END ;                                                          CPLX0014
REAL PROCEDURE     COSH(X1)  ;                                          CPLX0015
VALUE         X1  ;                                                     CPLX0016
REAL          X1  ;                                                     CPLX0017
                                                                        CPLX0018
         BEGIN                                                          CPLX0019
REAL          Y  ;                                                      CPLX0020
              Y ~ EXP(X1)  ;                                            CPLX0021
              COSH ~ (Y + 1.0/Y) | 0.5                                  CPLX0022
         END ;                                                          CPLX0023
STREAM PROCEDURE   ILLOP(MMMMMMMMMM,A,E,I)  ;                           CPLX0024
         BEGIN                                                          CPLX0025
              DI ~ A  ;  DS ~ 26 LIT "*ILLEGAL OP CODE* ENTRY - "  ;    CPLX0026
              SI ~ E  ;  DS ~ 4 DEC  ;  DS ~ 7 LIT " LOC - "  ;         CPLX0027
              SI ~ I  ;  DS ~ 4 DEC  ;  DS ~ LIT "~"  ;                 CPLX0028
              SI ~ LOC A  ;  DI ~ MMMMMMMMMM  ;                         CPLX0029
              DS ~ 5 LIT "{|000"  ;  DS ~ 3 RESET  ;                    CPLX0030
              SI ~ SI + 5  ;  SKIP 3 SB  ;                              CPLX0031
              3(IF SB THEN DS ~ SET ELSE DS ~ RESET  ;  SKIP 1 SB)  ;   CPLX0032
              DS ~ 2 CHR  ;                                             CPLX0033
              RELEASE(MMMMMMMMMM)  ;                                    CPLX0034
         END ;                                                          CPLX0035
FILE OUT      MMMMMMMMMM 2(1,10)  ;                                     CPLX0036
OWN INTEGER   E  ;                                                      CPLX0037
REAL ARRAY    DD[0:10]  ;                                               CPLX0038
INTEGER       I,J,I1,I2,I3  ;                                           CPLX0039
LABEL         NEXT,STORE,ADD,SUB,MUL,DVD,LOAD,ARG,ROOT,EXPC,SINC,COSC,  CPLX0040
              MODC,POLY,FIN,NEXT1  ;                                    CPLX0041
                                                                        CPLX0042
SWITCH        TYPE ~ STORE,ADD,SUB,MUL,DVD,LOAD,ARG,ROOT,EXPC,SINC,COSC,CPLX0043
              MODC,POLY  ;                                              CPLX0044
                                                                        CPLX0045
REAL          DIVD,Y  ;                                                 CPLX0046
                                                                        CPLX0047
COMMENT       MATRIX A -                                                CPLX0048
                 ROW 0 - REAL PART                                      CPLX0049
                 ROW 1 - IMAGINARY PART                                 CPLX0050
                 A[0,0] - REAL PART PSUEDO ACCUMULATOR                  CPLX0051
                 A[1,0] - IMAGINARY PART PSUEDO ACCUMULATOR             CPLX0052
              VECTOR CONVECT - CONTROL VECTOR                           CPLX0053
                   IN THE FOLLOWING I,J AND K ON THE RIGHT              CPLX0054
                   REFER TO A[I],A[J] AND A[K] RESPECTIVELY             CPLX0055
                 1,I - STORE PSUEDO ACCUMULATOR IN I                    CPLX0056
                 2,I,J - ADD I + J                                      CPLX0057
                 3,I,J - SUBTRACT I - J                                 CPLX0058
                 4,I,J - MULTIPLY I | J                                 CPLX0059
                 5,I,J - DIVIDE I/J                                     CPLX0060
                 6,I - LOAD I INTO PSUEDO ACCUMULATOR                   CPLX0061
                 7,I - ARG(I)                                           CPLX0062
                 8,I - SQRT(I)                                          CPLX0063
                 9,I - EXP(I)                                           CPLX0064
                 10,I - SIN(I)                                          CPLX0065
                 11,I - COS(I)                                          CPLX0066
                 12,I - ABS(I)                                          CPLX0067
                 13,I,J,K - POLYNOMIAL CALCULATION                      CPLX0068
                        LET X = A[K]                                    CPLX0069
                   ((...A[J] | X + A[J - 1]) | X +...A[I]              ;CPLX0070
                                                                        CPLX0071
              I ~ -2  ;  E ~ E + 1  ;                                   CPLX0072
NEXT1:        I ~ I + 2  ;                                              CPLX0073
              I1 ~ CONVECT[I+ 1]  ;                                     CPLX0074
              GO TO TYPE[ABS(CONVECT[I])]  ;                            CPLX0075
              ILLOP(MMMMMMMMMM,DD,E,I)  ;                               CPLX0076
              E ~ E/0  ;                                                CPLX0077
                                                                        CPLX0078
STORE:        A[0,I1] ~ A[0,0]  ;                                       CPLX0079
              A[1,I1] ~ A[1,0]  ;                                       CPLX0080
              GO TO NEXT  ;                                             CPLX0081
ADD:          I2 ~ CONVECT[I+ 2]  ;                                     CPLX0082
              A[0,0] ~ A[0,I1] + A[0,I2]  ;                             CPLX0083
              A[1,0] ~ A[1,I1] + A[1,I2]  ;                             CPLX0084
              I ~ I + 1  ;  GO TO NEXT  ;                               CPLX0085
                                                                        CPLX0086
SUB:          I2 ~ CONVECT[I + 2]  ;                                    CPLX0087
              A[0,0] ~ A[0,I1] - A[0,I2]  ;                             CPLX0088
              A[1,0] ~ A[1,I1] - A[1,I2]  ;                             CPLX0089
              I ~ I + 1  ;  GO TO NEXT  ;                               CPLX0090
                                                                        CPLX0091
MUL:          I2 ~ CONVECT[I + 2]  ;                                    CPLX0092
              Y ~ A[0,I1] | A[0,I2] - A[1,I1] | A[1,I2]  ;              CPLX0093
              A[1,0] ~ A[0,I1] | A[1,I2] + A[0,I2] | A[1,I1]  ;         CPLX0094
              A[0,0] ~ Y  ;                                             CPLX0095
              I ~ I + 1  ;  GO TO NEXT  ;                               CPLX0096
                                                                        CPLX0097
DVD:          I2 ~ CONVECT[I + 2]  ;                                    CPLX0098
              DIVD ~ A[0,I2]*2 + A[1,I2]*2  ;                           CPLX0099
              Y ~ (A[0,I1] | A[0,I2] + A[1,I1] | A[1,I2])/DIVD  ;       CPLX0100
              A[1,0] ~ (A[1,I1] | A[0,I2] - A[0,I1] | A[1,I2])/DIVD  ;  CPLX0101
              A[0,0] ~ Y  ;                                             CPLX0102
              I ~ I + 1  ;  GO TO NEXT  ;                               CPLX0103
                                                                        CPLX0104
LOAD:         A[0,0] ~ A[0,I1]  ;                                       CPLX0105
              A[1,0] ~ A[1,I1]  ;                                       CPLX0106
              GO TO NEXT  ;                                             CPLX0107
                                                                        CPLX0108
ARG:          IF A[0,I1] > 0 THEN                                       CPLX0109
              A[0,0] ~ ARCTAN(A[1,I1]/A[0,I1])                          CPLX0110
              ELSE                                                      CPLX0111
         BEGIN                                                          CPLX0112
              IF A[0,I1] = 0 THEN                                       CPLX0113
              A[0,0] ~ SIGN(A[1,I1]) | 1.57079632679                    CPLX0114
              ELSE                                                      CPLX0115
              A[0,0] ~ ARCTAN(A[1,I1]/A[0,I1]) +                        CPLX0116
              SIGN(A[1,I1]) | 3.14159265358  ;                          CPLX0117
         END ;                                                          CPLX0118
              A[1,0] ~ 0  ;                                             CPLX0119
              GO TO NEXT  ;                                             CPLX0120
                                                                        CPLX0121
ROOT:         IF A[1,I1] = 0 THEN                                       CPLX0122
         BEGIN                                                          CPLX0123
              IF A[0,I1] < 0 THEN                                       CPLX0124
         BEGIN                                                          CPLX0125
              A[1,0] ~ SQRT(ABS(A[0,I1]))     ;                         CPLX0126
              A[0,0] ~ 0                                                CPLX0127
         END                                                            CPLX0128
              ELSE                                                      CPLX0129
         BEGIN                                                          CPLX0130
              A[1,0] ~ 0  ;                                             CPLX0131
              A[0,0] ~ SQRT(A[0,I1])  ;                                 CPLX0132
         END ;                                                          CPLX0133
              GO TO NEXT  ;                                             CPLX0134
         END ;                                                          CPLX0135
              Y ~ SQRT((SQRT(A[0,I1]*2 + A[1,I1]*2) + A[0,I1])/2.0)  ;  CPLX0136
              A[1,0] ~ A[1,I1]/(2.0 | Y | SIGN(A[1,I1]))  ;             CPLX0137
              A[0,0] ~ Y  ;                                             CPLX0138
              GO TO NEXT  ;                                             CPLX0139
                                                                        CPLX0140
EXPC:         Y ~ EXP(A[0,I1])  ;                                       CPLX0141
              A[0,0] ~ COS(A[1,I1]) | Y  ;                              CPLX0142
              A[1,0] ~ SIN(A[1,I1]) | Y  ;                              CPLX0143
              GO TO NEXT  ;                                             CPLX0144
                                                                        CPLX0145
SINC:         Y ~ SIN(A[0,I1]) | COSH(A[1,I1])  ;                       CPLX0146
              A[1,0] ~ COS(A[0,I1]) | SINH(A[1,I1])  ;                  CPLX0147
              A[0,0] ~ Y  ;                                             CPLX0148
              GO TO NEXT  ;                                             CPLX0149
                                                                        CPLX0150
COSC:         Y ~ COS(A[0,I1]) | COSH(A[1,I1])  ;                       CPLX0151
              A[1,0] ~ -SIN(A[0,I1]) | SINH(A[1,I1])  ;                 CPLX0152
              A[0,0] ~ Y  ;                                             CPLX0153
              GO TO NEXT  ;                                             CPLX0154
                                                                        CPLX0155
MODC:         A[0,0] ~ SQRT(A[0,I1]*2 + A[1,I1]*2)  ;                   CPLX0156
              A[1,0] ~ 0  ;                                             CPLX0157
              GO TO NEXT  ;                                             CPLX0158
                                                                        CPLX0159
POLY:         I3 ~ CONVECT[I + 3]  ;                                    CPLX0160
              Y ~ A[0,I2]  ;  DIVD ~ A[1,I2]  ;                         CPLX0161
              FOR J ~ I2 - 1 STEP -1 UNTIL I1 DO                        CPLX0162
         BEGIN                                                          CPLX0163
              Y ~ Y | A[0,I3] - DIVD | A[1,I3] + A[0,J]  ;              CPLX0164
              DIVD ~ Y | A[1,I3] + A[0,I3] | DIVD + A[1,J]  ;           CPLX0165
         END ;                                                          CPLX0166
              A[0,0] ~ Y  ;  A[1,0] ~ DIVD  ;                           CPLX0167
              I ~ I + 1  ;  GO TO NEXT  ;                               CPLX0168
NEXT:         IF CONVECT[I] } 0 THEN GO TO NEXT1  ;                     CPLX0169
                                                                        CPLX0170
FIN:     END ;                                                          CPLX0171
                                                                        PROG0011
         END .                                                          PROG0012
