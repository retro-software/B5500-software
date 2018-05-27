
COMMENT       A TEST PROGRAM USING PROCEDURE BESSEL WHICH CALCULATES    TEST0001
              THE VALUES OF THE REGULAR AND MODIFIED BESSEL FUNCTIONS   TEST0002
              OF THE FIRST AND SECOND KINDS FOR COMPLEX ARGUMENTS AND   TEST0003
              POSITIVE INTEGRAL ORDERS.                                 TEST0004
                                                                        TEST0005
              J. K. KONDO                                               TEST0006
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION).     TEST0007
                                                                        TEST0008
              PROCEDURE CARD SEQUENCE START WITH BLCX0001.              TEST0009
              FIRST RELEASE DATE 04-01-1964  ;                          TEST0010
                                                                        TEST0011
                                                                        TEST0012
         BEGIN                                                          TEST0013
                                                                        TEST0014
LABEL         START, EOP  ;                                             TEST0015
REAL          X, Y, SUM, ISUM  ;                                        TEST0016
INTEGER       N  ;                                                      TEST0017
ALPHA         FUNCTION  ;                                               TEST0018
                                                                        TEST0019
FORMAT IN     FMIN(I3,X5,A1,X5,2E19.12)  ;                              TEST0020
FORMAT OUT    COMT(X30,"BESSEL FUNCTION"///"FUNCTION",X2,"N",X10,"X",   TEST0021
              X20,"YI",X15,"REAL PART",X12,"IMAGINARY PART"),           TEST0022
              FRMT(X3,A1,X5,I2,4E21.12)  ;                              TEST0023
                                                                        TEST0024
FILE IN       CARD(1,10)  ;                                             TEST0025
FILE OUT      PRINTER(1,15)  ;                                          TEST0026
                                                                        TEST0027
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
COMMENT       THE INPUT PARAMETERS FOR PROCEDURE BESSELN ARE            BLCX0001
                   FUNCTION - AN ALPHANUMERIC OF ONE CHARACTER WHICH    BLCX0002
                              DETERMINES THE FUNCTION TO BE EVALUATED,  BLCX0003
                              IT CAN ASSUME THE FOLLOWING VALUES:       BLCX0004
                              J - REGULAR BESSEL OF THE FIRST KIND      BLCX0005
                              Y - REGULAR BESSEL OF THE SECOND KIND     BLCX0006
                              I - MODIFIED BESSEL OF THE FIRST KIND     BLCX0007
                              K - MODIFIED BESSEL OF THE SECOND KIND.   BLCX0008
                   X        - REAL PART OF THE ARGUMENT.                BLCX0009
                   Y        - IMAGINARY PART OF THE ARGUMENT            BLCX0010
                   N        - THE ORDER OF THE FUNCTION TO BE EVALUATED BLCX0011
              THE OUTPUT PARAMETERS ARE:                                BLCX0012
                   SUM      - REAL PART OF THE EVALUATED FUNCTION.      BLCX0013
                   ISUM     - IMAGINARY PART OF THE EVALUATED FUNCTION ;BLCX0014
                                                                        BLCX0015
PROCEDURE     BESSELN(X,Y,N,FUNCTION,SUM,ISUM)  ;                       BLCX0016
                                                                        BLCX0017
VALUE         X, Y, N, FUNCTION  ;                                      BLCX0018
REAL          X, Y, SUM, ISUM  ;                                        BLCX0019
INTEGER       N  ;                                                      BLCX0020
ALPHA         FUNCTION  ;                                               BLCX0021
                                                                        BLCX0022
         BEGIN                                                          BLCX0023
ARRAY         A[0:1,0:5]  ;                                             BLCX0024
LABEL         L1,EXIT  ;                                                BLCX0025
                                                                        BLCX0026
PROCEDURE     BESSELCOM(X,Y,ORDER,FUNCTION,SUM,ISUM)  ;                 BLCX0027
                                                                        BLCX0028
VALUE         X, Y, FUNCTION, ORDER  ;                                  BLCX0029
REAL          X, Y, SUM, ISUM  ;                                        BLCX0030
INTEGER       ORDER  ;                                                  BLCX0031
ALPHA         FUNCTION  ;                                               BLCX0032
                                                                        BLCX0033
         BEGIN                                                          BLCX0034
INTEGER ARRAY CONVECT[0:14]  ;                                          BLCX0035
ARRAY         A[0:1,0:6]  ;                                             BLCX0036
BOOLEAN       BOOL  ;                                                   BLCX0037
INTEGER       N  ;                                                      BLCX0038
REAL          R, I, TOL, Z1, Z2  ;                                      BLCX0039
LABEL         EXIT, SECOND  ;                                           BLCX0040
              IF ORDER = 1 THEN BOOL ~ TRUE ELSE BOOL ~ FALSE  ;        BLCX0041
              TOL ~ 1.0@-13  ;                                          BLCX0042
              R ~ (X*2 - Y*2) | 0.25  ;  I ~ X | Y | 0.5  ;             BLCX0043
              IF FUNCTION = "J" OR FUNCTION = "Y" THEN                  BLCX0044
         BEGIN                                                          BLCX0045
              R ~ -R  ;  I ~ -I                                         BLCX0046
         END  ;                                                         BLCX0047
                                                                        BLCX0048
         BEGIN                                                          BLCX0049
COMMENT       THE VALUE OF THE J OR THE I FUNCTION IS CALCULATED IN THISBLCX0050
              BLOCK  ;                                                  BLCX0051
LABEL         L1  ;                                                     BLCX0052
INTEGER       NN  ;                                                     BLCX0053
              A[0,1] ~ R  ;  A[1,1] ~ I  ;                              BLCX0054
              IF BOOL THEN                                              BLCX0055
         BEGIN                                                          BLCX0056
              A[0,3] ~ A[0,2] ~ X | 0.5  ;                              BLCX0057
              A[1,2] ~ A[1,3] ~ Y | 0.5  ;                              BLCX0058
              Z1 ~ (X*2 + Y*2) | 0.25  ;                                BLCX0059
              N ~ 0                                                     BLCX0060
         END                                                            BLCX0061
              ELSE                                                      BLCX0062
         BEGIN                                                          BLCX0063
              A[0,3] ~ R  ;                                             BLCX0064
              A[1,2] ~ A[1,3] ~ I  ;                                    BLCX0065
              A[0,2] ~ 1.0 + R  ;                                       BLCX0066
              Z1 ~ I*2 + A[0,2]*2  ;                                    BLCX0067
              N ~ 1                                                     BLCX0068
         END  ;                                                         BLCX0069
                                                                        BLCX0070
L1:           N ~ N + 1  ;                                              BLCX0071
              NN ~ N | N  ;                                             BLCX0072
              IF BOOL THEN NN ~ NN + N  ;                               BLCX0073
              A[0,3] ~ A[0,3]/NN;  A[1,3] ~ A[1,3]/NN  ;                BLCX0074
              FILL CONVECT[*] WITH 4,1,3,1,3,2,0,2,-1,4  ;              BLCX0075
              COMPLEX(A,CONVECT)  ;                                     BLCX0076
              Z2 ~ A[0,4]*2 + A[1,4]*2  ;                               BLCX0077
              IF ABS(Z2 - Z1) > TOL THEN                                BLCX0078
         BEGIN                                                          BLCX0079
              A[0,2] ~ A[0,4]  ;  A[1,2] ~ A[1,4]  ;                    BLCX0080
              Z1 ~ Z2  ;                                                BLCX0081
              GO TO L1                                                  BLCX0082
         END  ;                                                         BLCX0083
              IF FUNCTION = "Y" OR FUNCTION = "K" THEN GO TO SECOND  ;  BLCX0084
              SUM ~ A[0,2]  ;  ISUM ~ A[1,2]  ;                         BLCX0085
              GO TO EXIT                                                BLCX0086
         END  CALCULATION OF BESSEL FUNCTIONS OF THE FIRST KIND  ;      BLCX0087
                                                                        BLCX0088
SECOND:  BEGIN                                                          BLCX0089
COMMENT       THE VALUE OF THE Y OR THE K FUNCTION IS CALCULATED IN THISBLCX0090
              BLOCK  ;                                                  BLCX0091
                                                                        BLCX0092
PROCEDURE     LNCOM(X,Y,RLN,ILN)  ;                                     BLCX0093
                                                                        BLCX0094
VALUE         X,Y  ;                                                    BLCX0095
REAL          RLN,ILN,X,Y  ;                                            BLCX0096
                                                                        BLCX0097
         BEGIN                                                          BLCX0098
              RLN ~ 0.5 | LN(X*2 + Y*2)  ;                              BLCX0099
              IF X ! 0 THEN ILN ~ ARCTAN(Y/X)                           BLCX0100
              ELSE IF Y > 0 THEN ILN ~ 1.57079632679                    BLCX0101
              ELSE ILN ~ -1.57079632679  ;                              BLCX0102
              IF X < 0 THEN IF Y < 0 THEN ILN ~ ILN - 3.14159265359     BLCX0103
              ELSE ILN ~ ILN + 3.14159265359                            BLCX0104
              ELSE IF Y < 0 THEN ILN ~ ILN + 6.28318530718  ;           BLCX0105
         END  ;                                                         BLCX0106
                                                                        BLCX0107
              LNCOM(X/2,Y/2,A[0,1],A[1,1])  ;                           BLCX0108
              A[0,1] ~ A[0,1] + 0.577215664902  ;                       BLCX0109
              FILL CONVECT[*] WITH 4,1,2,-1,6  ;                        BLCX0110
              COMPLEX(A,CONVECT)  ;                                     BLCX0111
                                                                        BLCX0112
         BEGIN                                                          BLCX0113
REAL          NN, NNN  ;                                                BLCX0114
LABEL         L1  ;                                                     BLCX0115
              A[0,1] ~ R  ;  A[1,1] ~ I  ;                              BLCX0116
              IF BOOL THEN                                              BLCX0117
         BEGIN                                                          BLCX0118
              A[0,3] ~ 1.0  ;                                           BLCX0119
              A[0,5] ~ A[0,2] ~ X | 0.5  ;                              BLCX0120
              A[1,5] ~ A[1,2] ~ Y | 0.5  ;                              BLCX0121
              Z1 ~ (X*2 + Y*2) | 0.25  ;                                BLCX0122
         END                                                            BLCX0123
              ELSE                                                      BLCX0124
         BEGIN                                                          BLCX0125
              A[0,5] ~ 1.0  ;                                           BLCX0126
              A[1,5] ~ A[0,3] ~ A[0,2] ~ A[1,2] ~ Z1 ~ 0  ;             BLCX0127
         END  ;                                                         BLCX0128
              A[1,3] ~ N ~ 0  ;                                         BLCX0129
                                                                        BLCX0130
L1:           N ~ N + 1  ;                                              BLCX0131
              NN ~ 1.0/N  ;                                             BLCX0132
              NNN ~ IF BOOL THEN 1/(N+1.0) ELSE NN  ;                   BLCX0133
              A[0,3] ~ A[0,3] + NN  ;                                   BLCX0134
              IF BOOL THEN A[0,3] ~ A[0,3] + NNN  ;                     BLCX0135
              NN ~ NN | NNN  ;                                          BLCX0136
              A[0,5] ~ A[0,5] | NN  ;  A[1,5] ~ A[1,5] | NN  ;          BLCX0137
              FILL CONVECT[*] WITH 4,5,1,1,5,4,0,3,2,0,2,-1,4  ;        BLCX0138
              COMPLEX(A,CONVECT)  ;                                     BLCX0139
              Z2 ~ A[0,4]*2 + A[1,4]*2  ;                               BLCX0140
              IF ABS(Z2 - Z1) > TOL THEN                                BLCX0141
         BEGIN                                                          BLCX0142
              A[0,2] ~ A[0,4]  ;  A[1,2] ~ A[1,4]  ;                    BLCX0143
              Z1 ~ Z2  ;                                                BLCX0144
              GO TO L1                                                  BLCX0145
         END  ;                                                         BLCX0146
         END  ;                                                         BLCX0147
              IF BOOL THEN                                              BLCX0148
         BEGIN                                                          BLCX0149
              A[0,1] ~ 1.0  ;  A[1,1] ~ 0  ;                            BLCX0150
              A[0,3] ~ X  ;  A[1,3] ~ Y  ;                              BLCX0151
              FILL CONVECT[*] WITH 5,1,3,-1,1  ;                        BLCX0152
              COMPLEX(A,CONVECT)                                        BLCX0153
         END  ;                                                         BLCX0154
              A[0,4] ~ 2.0  ;  A[1,4] ~ 0  ;                            BLCX0155
              IF FUNCTION = "Y" THEN                                    BLCX0156
         BEGIN                                                          BLCX0157
              A[0,3] ~ 3.14159265359  ;  A[1,3] ~ 0  ;                  BLCX0158
              IF BOOL THEN                                              BLCX0159
              FILL CONVECT[*] WITH 3,6,1,4,0,4,3,0,2,5,0,3,-1,1         BLCX0160
              ELSE                                                      BLCX0161
              FILL CONVECT[*] WITH 3,6,2,4,0,4,5,0,3,-1,1  ;            BLCX0162
         END                                                            BLCX0163
              ELSE                                                      BLCX0164
              IF BOOL THEN                                              BLCX0165
              FILL CONVECT[*] WITH 5,2,4,3,1,0,2,0,6,-1,1               BLCX0166
              ELSE                                                      BLCX0167
              FILL CONVECT[*] WITH 3,2,6,-1,1  ;                        BLCX0168
              COMPLEX(A,CONVECT)  ;                                     BLCX0169
              SUM ~ A[0,1]  ;  ISUM ~ A[1,1]  ;                         BLCX0170
         END  CALCULATION OF BESSEL FUNCTIONS OF THE SECOND KIND  ;     BLCX0171
EXIT:    END  PROCEDURE BESSELCOM  ;                                    BLCX0172
                                                                        BLCX0173
PROCEDURE     RECURSIVE(A,FUNCTION,N)  ;                                BLCX0174
                                                                        BLCX0175
VALUE         FUNCTION, N  ;                                            BLCX0176
ALPHA         FUNCTION  ;                                               BLCX0177
INTEGER       N  ;                                                      BLCX0178
ARRAY         A[0,0]  ;                                                 BLCX0179
                                                                        BLCX0180
         BEGIN                                                          BLCX0181
                                                                        BLCX0182
COMMENT       THE INPUT PARAMETERS TO PROCEDURE RECURSIVE ARE:          BLCX0183
                   A - AN ARRAY WHICH CONTAINS IN                       BLCX0184
                       COL 1 - THE COMPLEX ARGUMENT                     BLCX0185
                       COL 2 - THE VALUE OF THE FUNCTION OF ORDER ZERO  BLCX0186
                       COL 3 - THE VALUE OF THE FUNCTION OF ORDER ONE   BLCX0187
                   FUNCTION - THE FUNCTION BEING EVALUATED              BLCX0188
                   N - THE ORDER OF THE FUNCTION                        BLCX0189
              THE VALUE OF THE FUNCTION OF ORDER N IS STORED IN A[0,5]  BLCX0190
                   AND A[1,5]  ;                                        BLCX0191
                                                                        BLCX0192
INTEGER ARRAY CONVECT[0:11]  ;                                          BLCX0193
INTEGER       J, NN  ;                                                  BLCX0194
              NN ~ N - 1  ;                                             BLCX0195
              A[0,4] ~ 2.0  ;  A[1,4] ~ 0  ;                            BLCX0196
              FILL CONVECT[*] WITH 5,4,1,-1,1  ;                        BLCX0197
              COMPLEX(A,CONVECT)  ;                                     BLCX0198
                                                                        BLCX0199
              FOR J ~ 1 STEP 1 UNTIL NN DO                              BLCX0200
         BEGIN                                                          BLCX0201
              A[0,4] ~ J  ;                                             BLCX0202
              FILL CONVECT[*] WITH 4,4,1,4,0,3,3,0,2,-1,5  ;            BLCX0203
              IF FUNCTION = "K" THEN CONVECT[6] ~ 2                     BLCX0204
              ELSE IF FUNCTION = "I" THEN                               BLCX0205
         BEGIN                                                          BLCX0206
              CONVECT[7] ~ 2  ;  CONVECT[8] ~ 0                         BLCX0207
         END  ;                                                         BLCX0208
              COMPLEX(A,CONVECT)  ;                                     BLCX0209
              A[0,2] ~ A[0,3]  ;  A[1,2] ~ A[1,3] ;                     BLCX0210
              A[0,3] ~ A[0,5]  ;  A[1,3] ~ A[1,5]  ;                    BLCX0211
         END  ;                                                         BLCX0212
         END  PROCEDURE RECURSIVE  ;                                    BLCX0213
                                                                        BLCX0214
              IF (X*2 + Y*2) > 100 THEN GO TO EXIT  ;                   BLCX0215
              IF N = 1 THEN GO TO L1  ;                                 BLCX0216
              BESSELCOM(X,Y,0,FUNCTION,A[0,2],A[1,2])  ;                BLCX0217
              IF N = 0 THEN                                             BLCX0218
         BEGIN                                                          BLCX0219
              SUM ~ A[0,2]  ;  ISUM ~ A[1,2]  ;                         BLCX0220
              GO TO EXIT                                                BLCX0221
         END  ;                                                         BLCX0222
                                                                        BLCX0223
                                                                        BLCX0224
L1:           BESSELCOM(X,Y,1,FUNCTION,A[0,3],A[1,3])  ;                BLCX0225
              IF N = 1 THEN                                             BLCX0226
         BEGIN                                                          BLCX0227
              SUM ~ A[0,3]  ;  ISUM ~ A[1,3]  ;                         BLCX0228
              GO TO EXIT                                                BLCX0229
         END  ;                                                         BLCX0230
              A[0,1] ~ X  ;  A[1,1] ~ Y  ;                              BLCX0231
              RECURSIVE(A,FUNCTION,N)  ;                                BLCX0232
              SUM ~ A[0,5]  ;   ISUM ~ A[1,5]  ;                        BLCX0233
EXIT:    END  PROCEDURE BESSELN  ;                                      BLCX0234
                                                                        TEST0028
              WRITE(PRINTER,COMT)  ;                                    TEST0029
START:        READ(CARD,FMIN,N,FUNCTION,X,Y)[EOP]  ;                    TEST0030
              BESSELN(X,Y,N,FUNCTION,SUM,ISUM)  ;                       TEST0031
              WRITE(PRINTER[DBL],FRMT,FUNCTION,N,X,Y,SUM,ISUM)  ;       TEST0032
              GO TO START  ;                                            TEST0033
                                                                        TEST0034
EOP:     END  .                                                         TEST0035

