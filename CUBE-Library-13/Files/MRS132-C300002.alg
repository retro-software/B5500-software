
COMMENT       THIS PROCEDURE WILL DETERMINE THE VALUE OF BESSEL FUNCTIONTEST0001
              OF THE FIRST "JN" AND/OR SECOND "YN" KIND(S) FOR REAL     TEST0002
              VARIABLE "X" AND POSITIVE INTEGER ORDER "N".  THE VARIABLETEST0003
              "B" DETERMINES THE FUNCTION(S) TO BE EVALUATED: J FUNCTIONTEST0004
              ONLY (B = -1), Y FUNCTION ONLY (B = 1), BOTH FUNCTIONS    TEST0005
              (B = 0).                                                  TEST0006
                                                                        TEST0007
              J. K. KONDO                                               TEST0008
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION)      TEST0009
                                                                        TEST0010
              PROCEDURE CARD SEQUENCE BEGINS WITH RGBS0001              TEST0011
              FIRST RELEASE DATE 2/14/1964  ;                           TEST0012
                                                                        TEST0013
         BEGIN                                                          TEST0014
                                                                        TEST0015
FILE IN       CARD(1,10)  ;                                             TEST0016
FILE OUT      PRINTER(1,15)  ;                                          TEST0017
                                                                        TEST0018
FORMAT IN     FRMT(2I3, E16.8)  ;                                       TEST0019
                                                                        TEST0020
FORMAT OUT    FORM(" N = ", I2, X5, "X = ", E15.8, X5, "JN = ", E19.12, TEST0021
                   X5, "YN = ", E19.12)  ;                              TEST0022
                                                                        TEST0023
INTEGER       N, B  ;                                                   TEST0024
REAL          X, YN, JN  ;                                              TEST0025
LABEL         START, PROGRAMEND  ;                                      TEST0026
                                                                        TEST0027
PROCEDURE     BESSEL(N,X,B,JN,YN)  ;                                    RGBS0001
                                                                        RGBS0002
VALUE         N,X,B  ;                                                  RGBS0003
INTEGER       N,B  ;                                                    RGBS0004
REAL          JN,YN,X  ;                                                RGBS0005
                                                                        RGBS0006
         BEGIN                                                          RGBS0007
                                                                        RGBS0008
REAL          J0, J1, Y0, Y1, W, Y, Z, LPE  ;                           RGBS0009
BOOLEAN       BOOL  ;                                                   RGBS0010
LABEL         ASYM, RJN, RJ1, EXIT  ;                                   RGBS0011
                                                                        RGBS0012
REAL PROCEDURE RECURSIVE(N,X,AH,BH)  ;                                  RGBS0013
                                                                        RGBS0014
VALUE         N, X  ;                                                   RGBS0015
REAL          X, AH, BH  ;                                              RGBS0016
INTEGER       N  ;                                                      RGBS0017
                                                                        RGBS0018
         BEGIN                                                          RGBS0019
INTEGER       NN, I  ;                                                  RGBS0020
REAL          XH, CH  ;                                                 RGBS0021
              NN ~ N - 2  ;                                             RGBS0022
              XH ~ 2.0 / X  ;                                           RGBS0023
                                                                        RGBS0024
              FOR I ~ 1 STEP 1 UNTIL NN DO                              RGBS0025
         BEGIN                                                          RGBS0026
              CH ~ I | XH | BH - AH  ;                                  RGBS0027
              AH ~ BH  ;   BH ~ CH                                      RGBS0028
         END  ;                                                         RGBS0029
              RECURSIVE ~ (N - 1) | XH | BH - AH                        RGBS0030
         END  ;                                                         RGBS0031
                                                                        RGBS0032
              IF B = -1 THEN YN ~ 0  ;                                  RGBS0033
              IF B = 1 THEN JN ~ 0  ;                                   RGBS0034
              IF X = 0 THEN                                             RGBS0035
         BEGIN                                                          RGBS0036
              IF B ! 1 THEN IF N = 0 THEN JN ~ 1.0 ELSE JN ~ 0  ;       RGBS0037
              IF B ! -1 THEN YN ~ 1@63  ;   GO TO EXIT                  RGBS0038
         END  ;                                                         RGBS0039
              IF N < 2 THEN BOOL ~ TRUE ELSE BOOL ~ FALSE  ;            RGBS0040
              IF X > 8.0 THEN GO TO ASYM  ;                             RGBS0041
              Y ~ X | X  ;                                              RGBS0042
              W ~ 0.25 | Y  ;                                           RGBS0043
              Z ~ 0.5 | X  ;                                            RGBS0044
              IF B ! -1 THEN LPE ~ 0.577215664902 + LN(Z)  ;            RGBS0045
              IF N = 1 THEN GO TO RJ1  ;                                RGBS0046
              J0 ~ (((((((((((0.139498225113@-24 | Y                    RGBS0047
                   - 0.129716288484@-21) | Y + 0.704016687605@-19) | Y  RGBS0048
                   - 0.288339653048@-16) | Y + 0.937980951236@-14) | Y  RGBS0049
                   - 0.240261374626@-11) | Y + 0.470946090727@-9) | Y   RGBS0050
                   - 0.678167794818@-7) | Y + 0.678168346068@-5) | Y    RGBS0051
                   - 0.434027774627@-3) | Y + 0.156249999909@-1) | Y    RGBS0052
                   - 0.249999999990 ) | Y + 0.999999999998  ;           RGBS0053
              IF BOOL AND B ! 1 THEN                                    RGBS0054
         BEGIN                                                          RGBS0055
              JN ~ J0  ;                                                RGBS0056
              IF B = -1 THEN GO TO EXIT                                 RGBS0057
         END                                                            RGBS0058
              ELSE IF B = -1 THEN GO TO RJ1  ;                          RGBS0059
              Y0 ~ (((((((((((- 0.150467970109@-16 | W                  RGBS0060
                   + 0.333454743015@-14) | W - 0.434406477930@-12) | W  RGBS0061
                   + 0.428118263077@-10) | W - 0.334207822365@-8) | W   RGBS0062
                   + 0.204138961079@-6) | W - 0.945211598389@-5) | W    RGBS0063
                   + 0.317129505745@-3) | W - 0.723379609259@-2) | W    RGBS0064
                   + 0.101851851679) | W - 0.749999999942) | W          RGBS0065
                   + 2.0) | W  ;                                        RGBS0066
              Y0 ~ ( LPE | J0 | 2.0 + Y0) / 3.14159265359  ;            RGBS0067
              IF BOOL THEN                                              RGBS0068
         BEGIN                                                          RGBS0069
              YN ~ Y0  ;                                                RGBS0070
              GO TO EXIT                                                RGBS0071
         END  ;                                                         RGBS0072
                                                                        RGBS0073
RJ1:          J1 ~ ((((((((((( -0.335054238791@-23 | Y                  RGBS0074
                   + 0.285610094526@-20) | Y - 0.140863026183@-17) | Y  RGBS0075
                   + 0.519086007244@-15) | Y - 0.150082378490@-12) | Y  RGBS0076
                   + 0.336368378924@-10) | Y - 0.565136013887@-8) | Y   RGBS0077
                   + 0.678167921918@-6) | Y - 0.542534690611@-4) | Y    RGBS0078
                   + 0.260416665592@-2) | Y - 0.624999999857@-1) | Y    RGBS0079
                   + 0.499999999997) | X  ;                             RGBS0080
              IF BOOL AND B ! 1 THEN                                    RGBS0081
         BEGIN                                                          RGBS0082
              JN ~ J1  ;                                                RGBS0083
              IF B = -1 THEN GO TO EXIT                                 RGBS0084
         END                                                            RGBS0085
              ELSE IF B = -1 THEN GO TO RJN  ;                          RGBS0086
              Y1 ~ (((((((((((( -0.115388588882@-17 | W                 RGBS0087
                   + 0.279827840463@-15) | W - 0.400177983019@-13) | W  RGBS0088
                   + 0.435457257162@-11) | W - 0.378894734385@-9) | W   RGBS0089
                   + 0.261320433612@-7) | W - 0.138966667187@-5) | W    RGBS0090
                   + 0.547839147141@-4) | W - 0.151620361683@-2) | W    RGBS0091
                   + 0.0271990739475) | W - 0.277777777678) | W         RGBS0092
                   + 1.24999999997) | W - 0.999999999997) | Z  ;        RGBS0093
              Y1 ~ (LPE | J1 | 2.0 - 2.0 / X + Y1) / 3.14159265359  ;   RGBS0094
              IF BOOL THEN                                              RGBS0095
         BEGIN                                                          RGBS0096
              YN ~ Y1  ;                                                RGBS0097
              GO TO EXIT                                                RGBS0098
         END  ;                                                         RGBS0099
                                                                        RGBS0100
RJN:          IF B ! 1 THEN JN ~ RECURSIVE(N,X,J0,J1)  ;                RGBS0101
              IF B ! -1 THEN YN ~ RECURSIVE(N,X,Y0,Y1)  ;               RGBS0102
              GO TO EXIT  ;                                             RGBS0103
                                                                        RGBS0104
ASYM:    BEGIN                                                          RGBS0105
REAL          V, P0, P1, Q0, Q1  ;                                      RGBS0106
LABEL         ASYM1  ;                                                  RGBS0107
                                                                        RGBS0108
              Z ~ 0.797884560803 / SQRT(X)  ;                           RGBS0109
              W ~ 8.0 / X  ;   Y ~ W | W  ;                             RGBS0110
              V ~ X - 0.785398163397  ;                                 RGBS0111
              IF N = 1 THEN GO TO ASYM1  ;                              RGBS0112
              P0 ~ ((((-0.404853006966@-7 | Y + 0.309937051275@-6) | Y  RGBS0113
                   - 0.216059947887@-5) | Y + 0.273758976168@-4) | Y    RGBS0114
                   - 0.109863241355@-2) | Y + 0.999999999995  ;         RGBS0115
              Q0 ~ (((((((0.249700044314@-8 | Y - 0.153070993743@-7) | YRGBS0116
                   + 0.533266393704@-7) | Y - 0.177457027803@-6) | Y    RGBS0117
                   + 0.822906006470@-6) | Y - 0.693067950798@-5) | Y    RGBS0118
                   + 0.143051142768@-3) | Y - 0.015625) | W  ;          RGBS0119
              IF B ! 1 THEN                                             RGBS0120
         BEGIN                                                          RGBS0121
              J0 ~ (P0 | COS(V) - Q0 | SIN(V)) | Z  ;                   RGBS0122
              IF BOOL THEN                                              RGBS0123
         BEGIN                                                          RGBS0124
              JN ~ J0  ;                                                RGBS0125
              IF B = -1 THEN GO TO EXIT                                 RGBS0126
         END                                                            RGBS0127
              ELSE IF B = -1 THEN GO TO ASYM1                           RGBS0128
         END  ;                                                         RGBS0129
              Y0 ~ (P0 | SIN(V) + Q0 | COS(V)) | Z  ;                   RGBS0130
              IF NOT BOOL THEN GO TO ASYM1  ;                           RGBS0131
              YN ~ Y0  ;  GO TO EXIT  ;                                 RGBS0132
                                                                        RGBS0133
 ASYM1:       V ~ X - 2.35619449019  ;                                  RGBS0134
              P1 ~ (((( 0.453881173876@-7 | Y - 0.353073384912@-6) | Y  RGBS0135
                   + 0.255533559242@-5) | Y - 0.351985020881@-4) | Y    RGBS0136
                   + 0.00183105424791) | Y + 1.00000000001  ;           RGBS0137
              Q1 ~ (((((( 0.727261614074@-8| Y - 0.458123808979@-7) | Y RGBS0138
                   + 0.189865102935@-6) | Y - 0.946595508569@-6) | Y    RGBS0139
                   + 0.847035746808@-5) | Y - 0.200271571572@-3) | Y    RGBS0140
                   + 0.0468749999997) | W  ;                            RGBS0141
              IF B ! 1 THEN                                             RGBS0142
         BEGIN                                                          RGBS0143
              J1 ~ (P1 | COS(V) - Q1 | SIN(V)) | Z  ;                   RGBS0144
              IF BOOL THEN                                              RGBS0145
         BEGIN                                                          RGBS0146
              JN ~ J1  ;                                                RGBS0147
              IF B = -1 THEN GO TO EXIT                                 RGBS0148
         END                                                            RGBS0149
              ELSE IF B = -1 THEN GO TO RJN                             RGBS0150
         END  ;                                                         RGBS0151
              Y1 ~ (P1 | SIN(V) + Q1 | COS(V)) | Z  ;                   RGBS0152
              IF NOT BOOL THEN GO TO RJN  ;                             RGBS0153
              YN ~ Y1                                                   RGBS0154
         END  ;                                                         RGBS0155
EXIT:    END  ;                                                         RGBS0156
                                                                        TEST0028
START:        READ(CARD,FRMT,N,B,X)[PROGRAMEND]  ;                      TEST0029
              BESSEL(N,X,B,JN,YN)  ;                                    TEST0030
              WRITE(PRINTER[DBL],FORM,N,X,JN,YN)  ;                     TEST0031
              GO TO START  ;                                            TEST0032
                                                                        TEST0033
PROGRAMEND:   END  .                                                    TEST0034

