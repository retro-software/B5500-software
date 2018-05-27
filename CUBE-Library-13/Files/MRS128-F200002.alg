COMMENT       PROCEDURE GIVENS CALCULATES THE EIGENVALUES               TEST0001
              OF A REAL SYMMETRIC MATRIX BY GIVENS METHOD.              TEST0002
                                                                        TEST0003
              GERARD F. DIETZEL                                         TEST0004
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       TEST0005
                                                                        TEST0006
              PROCEDURE CARD SEQUENCE BEGINS WITH GIVS-0001             TEST0007
              FIRST RELEASE DATE 8-1-1963  ;                            TEST0008
                                                                        TEST0009
         BEGIN                                                          TEST0010
                                                                        TEST0011
INTEGER       N  ;                                                      TEST0012
LIST          ORDER(N)  ;                                               TEST0013
FORMAT IN     FIN1(I1)  ;                                               TEST0014
FILE IN       CARD(1,10)  ;                                             TEST0015
FILE OUT      PRINT(1,15)  ;                                            TEST0016
                                                                        TEST0017
              READ(CARD,FIN1,ORDER)  ;                                  TEST0018
                                                                        TEST0019
         BEGIN                                                          TEST0020
                                                                        TEST0021
INTEGER       I,J  ;                                                    TEST0022
ARRAY         A[0:N,0:N],A1,A2,EPSIL,EV[0:N]  ;                         TEST0023
                                                                        TEST0024
LIST          MATRIX(FOR J ~ I STEP 1 UNTIL N DO A[I,J]),               TEST0025
              MATRIN(I, FOR J ~ 1 STEP 1 UNTIL N DO [J,A[I,J]]),        TEST0026
              TRID(FOR I ~ 1 STEP 1 UNTIL N-1 DO [A1[I],A2[I+1]],A1[N]),TEST0027
              EIGVAL(EV[I],EPSIL[I])  ;                                 TEST0028
                                                                        TEST0029
FORMAT IN     FIN2(8(F6.1,X2))  ;                                       TEST0030
                                                                        TEST0031
FORMAT OUT    FOUT1(X15,"ROW",I3/X15,"COLUMN"/(X15,5(I4,F15.8))),       TEST0032
              FOUT2(X19,F15.8,F19.8),                                   TEST0033
              FOUT3(X19,F15.8,F19.10),                                  TEST0034
              HEADING1(//X55,"INPUT MATRIX A"//),                       TEST0035
              HEADING2(//X25,"ELEMENTS OF TRIDIAGONAL FORM"/            TEST0036
                       X26,"DIAGONAL",X7,"OFF-DIAGONAL"//),             TEST0037
              HEADING3(//X24,"EIGENVALUE",X12,"EPSILON"//)  ;           TEST0038
                                                                        TEST0039
PROCEDURE     GIVENS(N,A,A1,A2,EV,EPSIL)  ;                             GIVS0001
                                                                        GIVS0002
VALUE         N  ;                                                      GIVS0003
INTEGER       N  ;                                                      GIVS0004
ARRAY         A[0,0],A1,A2,EV,EPSIL[0]  ;                               GIVS0005
                                                                        GIVS0006
         BEGIN                                                          GIVS0007
                                                                        GIVS0008
INTEGER       I,J,J1,C,INT,CON,COIN  ;                                  GIVS0009
REAL          ALPHA1,BETA,LAMBDA,GAMMA,BOUND,BASE,EPS,SUM,V,BND  ;      GIVS0010
LABEL         L1,L2,L3,L4,L5,L6,L7  ;                                   GIVS0011
                                                                        GIVS0012
COMMENT       PROCEDURE TRIDIAG REDUCES A REAL SYMMETRIC MATRIX A OF    HOUS0001
              ORDER N TO TRIDIAGONAL FORM BY HOUSEHOLDERS METHOD.       HOUS0002
                                                                        HOUS0003
              GERARD F. DIETZEL                                         HOUS0004
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       HOUS0005
                                                                        HOUS0006
              PROCEDURE CARD SEQUENCE BEGINS WITH HOUS-0001             HOUS0007
              FIRST RELEASE DATE 9-01-1963  ;                           HOUS0008
                                                                        HOUS0009
PROCEDURE     TRIDIAG(N,A,A1,A2)  ;                                     HOUS0010
                                                                        HOUS0011
VALUE         N  ;                                                      HOUS0012
INTEGER       N  ;                                                      HOUS0013
ARRAY         A1,A2[0],A[0,0]  ;                                        HOUS0014
                                                                        HOUS0015
         BEGIN                                                          HOUS0016
                                                                        HOUS0017
INTEGER       I,J,K  ;                                                  HOUS0018
REAL          S,SI,Y,KAP  ;                                             HOUS0019
LABEL         L  ;                                                      HOUS0020
                                                                        HOUS0021
              FOR I ~ 1 STEP 1 UNTIL N-2 DO                             HOUS0022
         BEGIN                                                          HOUS0023
              S ~ 0  ;                                                  HOUS0024
              FOR J ~ I+1 STEP 1 UNTIL N DO  S ~ S + A[I,J]*2  ;        HOUS0025
              S ~ SQRT(S)  ;                                            HOUS0026
                                                                        HOUS0027
              IF A[I,I+1] = 0 THEN SI ~ 1.0  ELSE SI ~ SIGN(A[I,I+1])  ;HOUS0028
              IF S = 0 THEN GO TO L  ;                                  HOUS0029
                                                                        HOUS0030
              A1[I+1] ~ SQRT(0.5|(1 + SI|A[I,I+1]/S))  ;                HOUS0031
              Y ~ 2.0 | SI | A1[I+1] | S  ;                             HOUS0032
              FOR J ~ I+2 STEP 1 UNTIL N DO  A1[J] ~ A[I,J] / Y  ;      HOUS0033
                                                                        HOUS0034
              FOR J ~ I+1 STEP 1 UNTIL N DO                             HOUS0035
         BEGIN                                                          HOUS0036
              A2[J] ~ 0  ;                                              HOUS0037
              FOR K ~ I+1 STEP 1 UNTIL N DO A2[J] ~ A2[J] + A[J,K]|A1[K]HOUS0038
         END  ;                                                         HOUS0039
                                                                        HOUS0040
              KAP ~ 0  ;                                                HOUS0041
              FOR J ~ I+1 STEP 1 UNTIL N DO  KAP ~ KAP + A2[J]|A1[J]  ; HOUS0042
                                                                        HOUS0043
              FOR J ~ I+1 STEP 1 UNTIL N DO  A2[J] ~ A2[J] - KAP|A1[J] ;HOUS0044
                                                                        HOUS0045
              FOR J ~ I+1 STEP 1 UNTIL N DO                             HOUS0046
              FOR K ~ J STEP 1 UNTIL N DO                               HOUS0047
              A[J,K] ~ A[K,J] ~ A[K,J] - 2.0|(A1[J]|A2[K] +             HOUS0048
              A1[K]|A2[J])  ;                                           HOUS0049
                                                                        HOUS0050
              FOR J ~ I+1 STEP 1 UNTIL N DO  A[I,J] ~ A1[J]  ;          HOUS0051
                                                                        HOUS0052
L:            A2[I+1] ~ -SI | S  ;  A1[I] ~ A[I,I]                      HOUS0053
         END  ;                                                         HOUS0054
                                                                        HOUS0055
              A1[N-1] ~ A[N-1,N-1]  ;  A1[N] ~ A[N,N]  ;                HOUS0056
              A2[1] ~ 0  ;  A2[N] ~ A[N-1,N]  ;                         HOUS0057
                                                                        HOUS0058
COMMENT       THE REDUCTION TO TRIDIAGONAL FORM IS COMPLETE.            HOUS0059
              THE ARRAY A1 CONTAINS THE DIAGONAL ELEMENTS WHILE THE     HOUS0060
              ARRAY A2 CONTAINS THE OFF-DIAGONAL ELEMENTS.              HOUS0061
              THE ROWS OF THE UPPER TRIANGLE OF A CONTAIN THE VECTORS   HOUS0062
              WHICH HAVE BEEN CONSTRUCTED FOR THE REDUCTION  ;          HOUS0063
         END  ;                                                         HOUS0064
                                                                        GIVS0013
COMMENT       GIVEN A REAL N BY N SYMMETRIC MATRIX S OF TRIDIAGONAL     STRM0001
              FORM, I.E. S[I,I] = A[I], S[I,I+1] = S[I+1,I] = B[I+1],   STRM0002
              AND S[I,J] = 0 FOR ABS(I-J) > 1. B[1] = 0.                STRM0003
              WITH THE AID OF A STURM SEQUENCE PROCEDURE STURM WILL     STRM0004
              DETERMINE, FOR ANY LAMBDA, THE NUMBER OF EIGENVALUES      STRM0005
              THAT ARE GREATER THAN OR EQUAL TO LAMBDA. THIS NUMBER     STRM0006
              IS GIVEN BY C.                                            STRM0007
                                                                        STRM0008
              ROBERT M. COLLINGE                                        STRM0009
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       STRM0010
                                                                        STRM0011
              PROCEDURE CARD SEQUENCE BEGINS WITH STRM-0001             STRM0012
              FIRST RELEASE DATE 8-1-1963  ;                            STRM0013
                                                                        STRM0014
PROCEDURE     STURM(LAMBDA,A,B,N,C)  ;                                  STRM0015
                                                                        STRM0016
VALUE         LAMBDA,N  ;                                               STRM0017
INTEGER       N,C  ;                                                    STRM0018
REAL          LAMBDA  ;                                                 STRM0019
ARRAY         A,B[0]  ;                                                 STRM0020
                                                                        STRM0021
         BEGIN                                                          STRM0022
                                                                        STRM0023
INTEGER       I,SF1,SF2,SF3  ;                                          STRM0024
REAL          F1,F2,F3,ALPHA1,BETA  ;                                   STRM0025
LABEL         A1  ;                                                     STRM0026
                                                                        STRM0027
              SF2 ~ 1  ;  C ~ 0  ;                                      STRM0028
                                                                        STRM0029
              FOR I ~ 1 STEP 1 UNTIL N DO                               STRM0030
         BEGIN                                                          STRM0031
              ALPHA1 ~ A[I] - LAMBDA  ;                                 STRM0032
                                                                        STRM0033
              IF B[I] = 0 THEN                                          STRM0034
         BEGIN                                                          STRM0035
              F3 ~ ALPHA1 | SF2  ;  GO TO A1                            STRM0036
         END  ;                                                         STRM0037
                                                                        STRM0038
              BETA ~ B[I] * 2  ;                                        STRM0039
                                                                        STRM0040
              IF B[I-1] = 0 THEN F3 ~ ALPHA1|F2 - BETA|SF1              STRM0041
              ELSE F3 ~ ALPHA1 | F2 - BETA|F1  ;                        STRM0042
                                                                        STRM0043
A1:           IF F3 ! 0 THEN SF3 ~ SIGN(F3)  ELSE SF3 ~ SF2  ;          STRM0044
                                                                        STRM0045
              IF SF3 | SF2 > 0 THEN C ~ C+1  ;                          STRM0046
                                                                        STRM0047
              F1 ~ F2  ;  F2 ~ F3  ;                                    STRM0048
              SF1 ~ SF2  ;  SF2 ~ SF3                                   STRM0049
         END                                                            STRM0050
         END  ;                                                         STRM0051
                                                                        GIVS0014
              TRIDIAG(N,A,A1,A2)  ;                                     GIVS0015
                                                                        GIVS0016
COMMENT       A IS REDUCED TO TRIDIAGONAL FORM  ;                       GIVS0017
                                                                        GIVS0018
COMMENT       TAKE THE MAXIMUM ROW SUM OF THE TRIDIAGONAL MATRIX        GIVS0019
              AS A BOUND FOR THE EIGENVALUES  ;                         GIVS0020
                                                                        GIVS0021
              BND ~ ABS(A1[1]) + ABS(A2[2])  ;                          GIVS0022
              FOR I ~ 2 STEP 1 UNTIL N-1 DO                             GIVS0023
         BEGIN                                                          GIVS0024
              SUM ~ ABS(A1[I]) + ABS(A2[I]) + ABS(A2[I+1])  ;           GIVS0025
              IF SUM > BND THEN BND ~ SUM                               GIVS0026
         END  ;                                                         GIVS0027
              SUM ~ ABS(A1[N]) + ABS(A2[N])  ;                          GIVS0028
              IF SUM > BND THEN BND ~ SUM  ;                            GIVS0029
                                                                        GIVS0030
COMMENT       THE INTERVAL [-BND,BND] CONTAINS ALL THE EIGENVALUES  ;   GIVS0031
                                                                        GIVS0032
              BOUND ~ ALPHA1 ~ BND  ;                                   GIVS0033
                                                                        GIVS0034
              FOR I ~ 1 STEP 1 UNTIL N DO                               GIVS0035
         BEGIN                                                          GIVS0036
                                                                        GIVS0037
COMMENT       SUCCESSIVE BISECTION IN THE INTERVAL [-BND,BOUND]  ;      GIVS0038
                                                                        GIVS0039
              CON ~ 1  ;                                                GIVS0040
L1:           LAMBDA ~ -BND  ;                                          GIVS0041
L2:           STURM(LAMBDA,A1,A2,N,C)  ;                                GIVS0042
                                                                        GIVS0043
              IF C > I THEN                                             GIVS0044
         BEGIN                                                          GIVS0045
              IF ABS(LAMBDA - BOUND) < 1.0 THEN                         GIVS0046
         BEGIN                                                          GIVS0047
              IF CON = 1 THEN                                           GIVS0048
         BEGIN                                                          GIVS0049
              ALPHA1 ~ LAMBDA  ;  BETA ~ BOUND  ;                       GIVS0050
                                                                        GIVS0051
COMMENT       THERE IS AT LEAST ONE EIGENVALUE IN THE INTERVAL          GIVS0052
              [ALPHA1,BETA]  ;                                          GIVS0053
                                                                        GIVS0054
              GAMMA ~ ABS(BETA)  ;                                      GIVS0055
              IF ABS(ALPHA1) > GAMMA THEN GAMMA ~ ABS(ALPHA1)  ;        GIVS0056
                                                                        GIVS0057
COMMENT       SUCH AN EIGENVALUE HAS AT MOST THE SAME NUMBER OF DIGITS  GIVS0058
              BEFORE THE DECIMAL POINT AS GAMMA  ;                      GIVS0059
                                                                        GIVS0060
              BASE ~ 0.1  ;  INT ~ 0  ;                                 GIVS0061
L3:           BASE ~ 10.0 | BASE  ;                                     GIVS0062
              IF GAMMA } BASE THEN                                      GIVS0063
         BEGIN                                                          GIVS0064
              INT ~ INT + 1  ;  GO TO L3                                GIVS0065
         END  ;                                                         GIVS0066
                                                                        GIVS0067
COMMENT       THE NUMBER OF DIGITS BEFORE THE DECIMAL POINT OF GAMMA    GIVS0068
              IS GIVEN BY INT  ;                                        GIVS0069
                                                                        GIVS0070
              EPS ~ 10.0 * (INT - 10)  ;  EPSIL[I] ~ EPS                GIVS0071
         END  ;                                                         GIVS0072
                                                                        GIVS0073
COMMENT       THE BISECTION PROCESS IS TERMINATED WHEN                  GIVS0074
              ABS(LAMBDA - BOUND) { EPS  ;                              GIVS0075
                                                                        GIVS0076
              IF ABS(LAMBDA - BOUND) { EPS THEN                         GIVS0077
         BEGIN                                                          GIVS0078
              COIN ~ C - I + 1  ;  J1 ~ COIN - 1  ;                     GIVS0079
                                                                        GIVS0080
COMMENT       THE NUMBER OF EIGENVALUES LOCATED IN THE INTERVAL         GIVS0081
              [LAMBDA,BOUND] IS GIVEN BY COIN  ;                        GIVS0082
                                                                        GIVS0083
              EV[I] ~ LAMBDA  ;                                         GIVS0084
              FOR J ~ 1 STEP 1 UNTIL J1 DO                              GIVS0085
         BEGIN                                                          GIVS0086
              EV[I+J] ~ LAMBDA  ;                                       GIVS0087
              EPSIL[I+J] ~ EPS                                          GIVS0088
         END  ;                                                         GIVS0089
                                                                        GIVS0090
              I ~ I + J1  ;                                             GIVS0091
              GO TO L7                                                  GIVS0092
         END  ;                                                         GIVS0093
              CON ~ 0                                                   GIVS0094
         END  ;                                                         GIVS0095
              LAMBDA ~ 0.5 | (BOUND + LAMBDA)  ;                        GIVS0096
              GO TO L2                                                  GIVS0097
         END  ;                                                         GIVS0098
                                                                        GIVS0099
              BOUND ~ LAMBDA  ;                                         GIVS0100
                                                                        GIVS0101
              IF C < I THEN GO TO L1  ;                                 GIVS0102
                                                                        GIVS0103
              IF C = I THEN                                             GIVS0104
         BEGIN                                                          GIVS0105
              IF CON = 1 THEN BETA ~ ALPHA1  ;                          GIVS0106
              ALPHA1 ~ LAMBDA  ;                                        GIVS0107
                                                                        GIVS0108
COMMENT       THERE IS ONLY ONE EIGENVALUE IN THE INTERVAL              GIVS0109
              [ALPHA1,BETA]  ;                                          GIVS0110
                                                                        GIVS0111
L4:           IF ABS(ALPHA1 - BETA) } 1.0 THEN                          GIVS0112
         BEGIN                                                          GIVS0113
              V ~ 0.5 | (ALPHA1 + BETA)  ;                              GIVS0114
              STURM(V,A1,A2,N,C)  ;                                     GIVS0115
              IF C = I THEN ALPHA1 ~ V  ELSE BETA ~ V  ;                GIVS0116
              GO TO L4                                                  GIVS0117
         END  ;                                                         GIVS0118
                                                                        GIVS0119
              GAMMA ~ ABS(BETA)  ;                                      GIVS0120
              IF ABS(ALPHA1) > GAMMA THEN GAMMA ~ ABS(ALPHA1)  ;        GIVS0121
                                                                        GIVS0122
COMMENT       SEE PREVIOUS COMMENTS  ;                                  GIVS0123
                                                                        GIVS0124
              BASE ~ 0.1  ;  INT ~ 0  ;                                 GIVS0125
L5:           BASE ~ 10.0 | BASE  ;                                     GIVS0126
              IF GAMMA } BASE THEN                                      GIVS0127
         BEGIN                                                          GIVS0128
              INT ~ INT + 1  ;  GO TO L5                                GIVS0129
         END  ;                                                         GIVS0130
              EPS ~ 10.0 * (INT - 10)  ;  EPSIL[I] ~ EPS  ;             GIVS0131
                                                                        GIVS0132
COMMENT       THE BISECTION PROCESS IS TERMINATED WHEN                  GIVS0133
              ABS(ALPHA1 - BETA) { EPS  ;                               GIVS0134
                                                                        GIVS0135
L6:           IF ABS(ALPHA1 - BETA) > EPS THEN                          GIVS0136
         BEGIN                                                          GIVS0137
              V ~ 0.5 | (ALPHA1 + BETA)  ;                              GIVS0138
              STURM(V,A1,A2,N,C)  ;                                     GIVS0139
              IF C = I THEN ALPHA1 ~ V  ELSE BETA ~ V  ;                GIVS0140
              GO TO L6                                                  GIVS0141
         END  ;                                                         GIVS0142
                                                                        GIVS0143
              EV[I] ~ ALPHA1                                            GIVS0144
         END  ;                                                         GIVS0145
L7:      END  ;                                                         GIVS0146
                                                                        GIVS0147
COMMENT       THE CALCULATION OF THE EIGENVALUES IS COMPLETED.          GIVS0148
              ARRAY EV CONTAINS THE EIGENVALUES  ;                      GIVS0149
         END  ;                                                         GIVS0150
                                                                        TEST0040
              FOR I ~ 1 STEP 1 UNTIL N DO  READ(CARD,FIN2,MATRIX)  ;    TEST0041
              FOR I ~ 1 STEP 1 UNTIL N DO                               TEST0042
              FOR J ~ I+1 STEP 1 UNTIL N DO                             TEST0043
              A[J,I] ~ A[I,J]  ;                                        TEST0044
                                                                        TEST0045
              WRITE(PRINT,HEADING1)  ;                                  TEST0046
              FOR I ~ 1 STEP 1 UNTIL N DO  WRITE(PRINT,FOUT1,MATRIN)  ; TEST0047
                                                                        TEST0048
              GIVENS(N,A,A1,A2,EV,EPSIL)  ;                             TEST0049
                                                                        TEST0050
              WRITE(PRINT,HEADING2)  ;                                  TEST0051
              WRITE(PRINT,FOUT2,TRID)  ;                                TEST0052
              WRITE(PRINT,HEADING3)  ;                                  TEST0053
              FOR I ~ 1 STEP 1 UNTIL N DO  WRITE(PRINT,FOUT3,EIGVAL)    TEST0054
         END                                                            TEST0055
         END.                                                           TEST0056
