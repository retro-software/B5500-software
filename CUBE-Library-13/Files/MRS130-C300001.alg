
COMMENT       PROCEDURE MODBES DETERMINES THE VALUE OF THE MODIFIED     TEST0001
              BESSEL FUNCTIONS OF THE FIRST AND SECOND KINDS FOR REAL   TEST0002
              ARGUMENTS AND INTEGRAL ORDERS.                            TEST0003
              THE INPUT PARAMETERS TO MODBES ARE:                       TEST0004
                 B  - AN INTEGER WHOSE VALUE DETERMINES THE FUNCTION(S) TEST0005
                      TO BE EVALUATED:                                  TEST0006
                      K FUNCTION ONLY  B = 1                            TEST0007
                      I FUNCTION ONLY  B = -1                           TEST0008
                      K AND I FUNCTIONS B = 0                           TEST0009
                 N   - THE ORDER FOR THE FUNCTIONS(S) DESIRED           TEST0010
                 X   - THE ARGUMENT FOR THE FUNCTION(S) DESIRED.        TEST0011
              OUTPUT PARAMETERS:                                        TEST0012
                 BI  - THE VALUE OF THE N-TH ORDER OF I(X)              TEST0013
                 BK  - THE VALUE OF THE N-TH ORDER OF K(X).             TEST0014
                                                                        TEST0015
              J. K. KONDO                                               TEST0016
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION),     TEST0017
                                                                        TEST0018
              PROCEDURE CARD SEQUENCE BEGINS WITH MODB0001.             TEST0019
              FIRST RELEASE DATE 11-15-63.  ;                           TEST0020
                                                                        TEST0021
         BEGIN                                                          TEST0022
                                                                        TEST0023
FILE IN       FILE2(1,10)  ;                                            TEST0024
FILE          FILE1 1 (1,15)  ;                                         TEST0025
INTEGER       N,B  ;                                                    TEST0026
REAL          X,BI,BK  ;                                                TEST0027
LABEL         START, EOP  ;                                             TEST0028
                                                                        TEST0029
FORMAT IN     FRMT(2I3, E16.8)  ;                                       TEST0030
FORMAT OUT    FORM("N = ",I2,X5,"X = ",E15.8,X5,"IN(X) = ", E19.12, X5, TEST0031
                   "KN(X) = ",E19.12)  ;                                TEST0032
                                                                        TEST0033
PROCEDURE     MODBES (N,X,B,BI,BK)  ;                                   MODB0001
VALUE         N, X, B  ;                                                MODB0002
INTEGER       N, B  ;                                                   MODB0003
REAL          X, BI, BK  ;                                              MODB0004
                                                                        MODB0005
         BEGIN                                                          MODB0006
                                                                        MODB0007
REAL          I0, I1, K0, K1, F0, F1, Y, W, Z, ZZ  ;                    MODB0008
BOOLEAN       BOOL  ;                                                   MODB0009
LABEL         ASYM, EXIT, ONE, RECUR  ;                                 MODB0010
                                                                        MODB0011
              IF B = -1 THEN BK ~ 0  ;                                  MODB0012
              IF B = 1 THEN BI ~ 0  ;                                   MODB0013
              IF X = 0 THEN                                             MODB0014
         BEGIN                                                          MODB0015
              IF B ! 1 THEN IF N = 0 THEN BI ~ 1.0 ELSE BI ~ 0  ;       MODB0016
              IF B ! -1 THEN BK ~ 1@63  ;   GO TO EXIT                  MODB0017
         END  ;                                                         MODB0018
              IF N < 2 THEN BOOL ~ TRUE ELSE BOOL ~ FALSE  ;            MODB0019
              IF X > 8.0 THEN GO TO ASYM  ;                             MODB0020
              W ~ X | 0.5  ;    Y ~ X | X  ;                            MODB0021
              IF N = 1 THEN GO TO ONE  ;                                MODB0022
              I0 ~ (((((((((( 0.268019175490@-21 | Y                    MODB0023
                   + 0.535216907976@-19) | Y + 0.305979194094@-16) | Y  MODB0024
                   + 0.930022182581@-14) | Y + 0.240569384057@-11) | Y  MODB0025
                   + 0.470887594931@-9) | Y + 0.678177034377@-7) | Y    MODB0026
                   + 0.678167681927@-5) | Y + 0.434027811441@-3) | Y    MODB0027
                   + 0.156249999240@-1) | Y + 0.250000000062) | Y       MODB0028
                   + 0.999999999993  ;                                  MODB0029
              IF BOOL AND B ! 1 THEN                                    MODB0030
         BEGIN                                                          MODB0031
              BI ~ I0  ;                                                MODB0032
              IF B = -1 THEN GO TO EXIT                                 MODB0033
         END                                                            MODB0034
              ELSE IF B = -1 THEN GO TO ONE  ;                          MODB0035
              ZZ ~ X/8  ;  Z ~ ZZ | ZZ  ;                               MODB0036
              K0 ~ ((((((((((( 0.00264711864490 | Z                     MODB0037
                   + 0.00779199933693) | Z + 0.0872236307790) | Z       MODB0038
                   + 0.444560728015) | Z + 1.99795311175) | Z           MODB0039
                   + 6.64831493414) | Z + 15.7452737568) | Z            MODB0040
                   + 23.2886583327) | Z + 13.6332513798) | Z            MODB0041
                   - 14.8112164763) | Z - 29.6646416017) | Z            MODB0042
                   - 15.4161604173) | Z - 1.96351002602 - LN(ZZ) | I0  ;MODB0043
              IF BOOL THEN                                              MODB0044
         BEGIN                                                          MODB0045
              BK ~ K0  ;                                                MODB0046
              GO TO EXIT                                                MODB0047
         END  ;                                                         MODB0048
                                                                        MODB0049
ONE:          I1 ~ ((((((((((( 0.108550788103@-22 | Y                   MODB0050
                   + 0.253259771373@-20) | Y + 0.151626286590@-17) | Y  MODB0051
                   + 0.517720064571@-15) | Y + 0.150307094262@-12) | Y  MODB0052
                   + 0.336362767529@-10) | Y + 0.565144802895@-8) | Y   MODB0053
                   + 0.678167997710@-6) | Y + 0.542534743123@-4) | Y    MODB0054
                   + 0.260416666133@-2) | Y + 0.0625000000051) | Y      MODB0055
                   + 0.499999999999) | X  ;                             MODB0056
              IF BOOL AND B ! 1 THEN                                    MODB0057
         BEGIN                                                          MODB0058
              BI ~ I1  ;                                                MODB0059
              IF B = -1 THEN GO TO EXIT                                 MODB0060
         END                                                            MODB0061
              ELSE IF B = -1 THEN GO TO RECUR  ;                        MODB0062
              K1 ~ ((((((((((( 0.00778344530446 | Z + 0.0198236610206)|ZMODB0063
                   + 0.208272452744) | Z + 0.934186538782) | Z          MODB0064
                   + 3.66626441485) | Z + 10.3133591072) | Z            MODB0065
                   + 19.5725210307) | Z + 20.0086018471) | Z            MODB0066
                   - 0.588972633205) | Z - 25.3306343153) | Z           MODB0067
                   - 22.8323208143) | Z - 5.85404010414) | ZZ  ;        MODB0068
              K1 ~ LN(ZZ) | I1 + 1.0/X - K1  ;                          MODB0069
              IF BOOL THEN                                              MODB0070
         BEGIN                                                          MODB0071
              BK ~ K1  ;                                                MODB0072
              GO TO EXIT                                                MODB0073
         END  ;                                                         MODB0074
                                                                        MODB0075
RECUR:   BEGIN                                                          MODB0076
                                                                        MODB0077
INTEGER       I, NN  ;                                                  MODB0078
LABEL         LA  ;                                                     MODB0079
                                                                        MODB0080
              NN ~ N - 2  ;                                             MODB0081
              Z ~ 2.0/X  ;                                              MODB0082
              IF B = 1 THEN GO TO LA  ;                                 MODB0083
                                                                        MODB0084
              FOR I ~ 1 STEP 1 UNTIL NN DO                              MODB0085
         BEGIN                                                          MODB0086
              BI ~ -I1 | I | Z + I0  ;                                  MODB0087
              I0 ~ I1  ;   I1 ~ BI                                      MODB0088
         END  ;                                                         MODB0089
                                                                        MODB0090
              BI ~ -(N-1) | Z | I1 + I0  ;                              MODB0091
              IF B = -1 THEN GO TO EXIT  ;                              MODB0092
                                                                        MODB0093
LA:           FOR I ~ 1 STEP 1 UNTIL NN DO                              MODB0094
         BEGIN                                                          MODB0095
              BK ~ I | Z | K1 + K0  ;                                   MODB0096
              K0 ~ K1  ;   K1 ~ BK                                      MODB0097
         END  ;                                                         MODB0098
                                                                        MODB0099
              BK ~ (N-1) | Z | K1 + K0                                  MODB0100
         END  ;                                                         MODB0101
              GO TO EXIT  ;                                             MODB0102
                                                                        MODB0103
ASYM:    BEGIN                                                          MODB0104
                                                                        MODB0105
LABEL         AONE   ;                                                  MODB0106
DEFINE        F0 = ((((((((((((( - 0.102629925965@-6 | Y                MODB0107
                   - 0.116287636538@-6) | Y + 0.270977633015@-6) | Y    MODB0108
                   + 0.358404518514@-6) | Y - 0.151493975036@-6) | Y    MODB0109
                   - 0.218733131831@-6) | Y + 0.213257727517@-6) |Y     MODB0110
                   + 0.448815551623@-6) | Y + 0.859033133932@-6) | Y    MODB0111
                   + 0.274083014220@-5) | Y + 0.109244143943@-4) | Y    MODB0112
                   + 0.570712435028@-4) | Y + 0.438291045055@-3) | Y    MODB0113
                   + 0.623347307930@-2) | Y + 0.398942280402  #,        MODB0114
              F1 = ((((((((((((( 0.111806692501@-6 | Y                  MODB0115
                   + 0.122939865332@-6) | Y - 0.300027912496@-6) | Y    MODB0116
                   - 0.383190078678@-6) | Y + 0.174915128201@-6) | Y    MODB0117
                   + 0.231430039467@-6) | Y - 0.246689337550@-6) | Y    MODB0118
                   - 0.508729084839@-6) | Y - 0.101354745725@-5) | Y    MODB0119
                   - 0.335329334259@-5) | Y - 0.140459219128@-4) | Y    MODB0120
                   - 0.79899080447@-4) | Y - 0.730485068521@-3) | Y     MODB0121
                   - 0.0187004193374) | Y + 0.398942280401  #  ;        MODB0122
                                                                        MODB0123
              Y ~ 8.0 / X  ;    Z ~ 1.0 / SQRT(X)  ;                    MODB0124
              IF N = 1 THEN GO TO AONE  ;                               MODB0125
              IF B ! 1 THEN                                             MODB0126
         BEGIN                                                          MODB0127
              I0 ~ F0  ;                                                MODB0128
              I0 ~ EXP(X) | Z | I0  ;                                   MODB0129
              IF BOOL THEN                                              MODB0130
         BEGIN                                                          MODB0131
              BI ~ I0  ;                                                MODB0132
              IF B = -1 THEN GO TO EXIT                                 MODB0133
         END  ;                                                         MODB0134
              IF B = -1 THEN GO TO AONE                                 MODB0135
         END  ;                                                         MODB0136
              Y ~ -Y  ;                                                 MODB0137
              K0 ~ F0  ;                                                MODB0138
              K0 ~ EXP(-X) | Z | K0 | 3.14159265359  ;                  MODB0139
              IF NOT BOOL THEN GO TO AONE  ;                            MODB0140
              BK ~ K0  ;    GO TO EXIT  ;                               MODB0141
AONE:         Y ~ ABS(Y)  ;                                             MODB0142
              IF B ! 1 THEN                                             MODB0143
         BEGIN                                                          MODB0144
              I1 ~ F1  ;                                                MODB0145
              I1 ~ EXP(X) | Z | I1  ;                                   MODB0146
              IF BOOL THEN                                              MODB0147
         BEGIN                                                          MODB0148
              BI ~ I1  ;                                                MODB0149
              IF B = -1 THEN GO TO EXIT                                 MODB0150
         END  ;                                                         MODB0151
              IF B = -1 THEN GO TO RECUR                                MODB0152
         END  ;                                                         MODB0153
              Y ~ -Y  ;                                                 MODB0154
              K1 ~ F1  ;                                                MODB0155
              K1 ~ EXP(-X) | Z | K1 | 3.14159265359  ;                  MODB0156
              IF NOT BOOL THEN GO TO RECUR  ;                           MODB0157
              BK ~ K1                                                   MODB0158
         END  ;                                                         MODB0159
                                                                        MODB0160
EXIT:    END  ;                                                         MODB0161
                                                                        TEST0034
START:        READ(FILE2,FRMT,N,B,X)[EOP]  ;                            TEST0035
              MODBES (N,X,B,BI,BK)  ;                                   TEST0036
              WRITE(FILE1[DBL],FORM,N,X,BI,BK)  ;                       TEST0037
              GO TO START  ;                                            TEST0038
                                                                        TEST0039
EOP:     END  .                                                         TEST0040

