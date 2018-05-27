COMMENT       THIS PROCEDURE CALCULATES THE EIGENVALUES                 TEST0001
              AND EIGENVECTORS OF A REAL SYMMETRIC MATRIX               TEST0002
              BY THE JACOBI METHOD.                                     TEST0003
                                                                        TEST0004
              GERARD F. DIETZEL                                         TEST0005
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       TEST0006
                                                                        TEST0007
              PROCEDURE CARD SEQUENCE BEGINS WITH JACB-0001             TEST0008
              FIRST RELEASE DATE 8-1-1963  ;                            TEST0009
                                                                        TEST0010
         BEGIN                                                          TEST0011
                                                                        TEST0012
INTEGER       N  ;                                                      TEST0013
LIST          ORDER(N)  ;                                               TEST0014
FORMAT IN     FIN1(I1)  ;                                               TEST0015
FILE IN       CARD(1,10)  ;                                             TEST0016
FILE OUT      PRINT(1,15)  ;                                            TEST0017
                                                                        TEST0018
              READ(CARD,FIN1,ORDER)  ;                                  TEST0019
                                                                        TEST0020
         BEGIN                                                          TEST0021
                                                                        TEST0022
INTEGER       I,J,K,CON  ;                                              TEST0023
REAL          VF,EPS  ;                                                 TEST0024
ARRAY         AX[0:N],A,AA,U[0:N,0:N]  ;                                TEST0025
LABEL         FINISH,L  ;                                               TEST0026
                                                                        TEST0027
LIST          ACCURACY(EPS),                                            TEST0028
              MATRIX(FOR J ~ I STEP 1 UNTIL N DO A[I,J]),               TEST0029
              MATRIN(I, FOR J ~ 1 STEP 1 UNTIL N DO [J,A[I,J]]),        TEST0030
              EIGVAL(FOR I ~ 1 STEP 1 UNTIL N DO A[I,I]),               TEST0031
              EIGVECT1(FOR I ~ 1 STEP 1 UNTIL N DO                      TEST0032
                       FOR J ~ K-4 STEP 1 UNTIL K DO U[I,J]),           TEST0033
              EIGVECT2(FOR I ~ 1 STEP 1 UNTIL N DO                      TEST0034
                       FOR J ~ K-4 STEP 1 UNTIL N DO U[I,J]),           TEST0035
              VFINAL(VF),                                               TEST0036
              ACCUR(EPS)  ;                                             TEST0037
                                                                        TEST0038
FORMAT IN     FIN2(E8.1),                                               TEST0039
              FIN3(8(F6.1,X2))  ;                                       TEST0040
                                                                        TEST0041
FORMAT OUT    FOUT1(X15, "ROW",I3/X15,"COLUMN"/(X15,5(I4,F15.8))),      TEST0042
              FOUT2(X14,5F19.8),                                        TEST0043
              FOUT3(X14,3F19.8),                                        TEST0044
              FOUT4(/X15,"FINAL THRESHOLD = VF"//X15,E8.1,I1),          TEST0045
              FOUT5(/X15,"ACCURACY = EPS "//X15,E8.1,I1),               TEST0046
              FOUT6(/),                                                 TEST0047
              HEADING1(X55,"INPUT MATRIX A"/),                          TEST0048
              HEADING2(/X57,"EIGENVALUES"/),                            TEST0049
              HEADING3(/X56,"EIGENVECTORS"),                            TEST0050
              HEADING4(//X56,"DIFFERENCE AX - LAMBDA.X"/)  ;            TEST0051
                                                                        TEST0052
PROCEDURE     JACOBI(N,VF,EPS,A,U)  ;                                   JACB0001
                                                                        JACB0002
VALUE         N,EPS  ;                                                  JACB0003
INTEGER       N  ;                                                      JACB0004
REAL          VF,EPS  ;                                                 JACB0005
ARRAY         A,U[0,0]  ;                                               JACB0006
                                                                        JACB0007
         BEGIN                                                          JACB0008
                                                                        JACB0009
INTEGER       I,I0,I1,I2,J,J0,J1,CON  ;                                 JACB0010
REAL          NORM,V,T,C,C1,S,S1,TEMP,ARG,T1  ;                         JACB0011
LABEL         L1,L2  ;                                                  JACB0012
                                                                        JACB0013
COMMENT       SET MATRIX U = IDENTITY MATRIX  ;                         JACB0014
                                                                        JACB0015
              FOR I ~ 1 STEP 1 UNTIL N DO                               JACB0016
         BEGIN                                                          JACB0017
              FOR J ~ I+1 STEP 1 UNTIL N DO                             JACB0018
              U[J,I] ~ U[I,J] ~ 0  ;                                    JACB0019
              U[I,I] ~ 1.0                                              JACB0020
         END  ;                                                         JACB0021
                                                                        JACB0022
COMMENT       CALCULATE THE OFF-DIAGONAL NORM OF A  ;                   JACB0023
                                                                        JACB0024
              NORM ~ 0  ;                                               JACB0025
              FOR I ~ 2 STEP 1 UNTIL N DO                               JACB0026
              FOR J ~ 1 STEP 1 UNTIL I-1 DO                             JACB0027
              NORM ~ NORM + 2.0|A[I,J]*2  ;                             JACB0028
              NORM ~ SQRT(NORM)  ;                                      JACB0029
                                                                        JACB0030
COMMENT       CALCULATE THE FINAL THRESHOLD VF  ;                       JACB0031
                                                                        JACB0032
              ARG ~ N*2 - N  ;  VF ~ EPS / SQRT(ARG)  ;                 JACB0033
                                                                        JACB0034
              V ~ NORM ; CON ~ 0  ;                                     JACB0035
                                                                        JACB0036
L1:           V ~ V / N  ;                                              JACB0037
                                                                        JACB0038
COMMENT       CONSIDER THE LOWER TRIANGULAR ELEMENTS ONLY.              JACB0039
              EXAMINE THE OFF-DIAGONAL ELEMENTS ROW-WISE IN SYSTEMATIC  JACB0040
              ORDER AND ROTATE IF THE MAGNITUDE IS GREATER THAN         JACB0041
              THE CURRENT THRESHOLD V  ;                                JACB0042
                                                                        JACB0043
L2:           FOR I0 ~ 2 STEP 1 UNTIL N DO                              JACB0044
              FOR J0 ~ 1 STEP 1 UNTIL I0-1 DO                           JACB0045
         BEGIN                                                          JACB0046
              IF ABS(A[I0,J0]) > V THEN                                 JACB0047
         BEGIN                                                          JACB0048
              CON ~ 1   ;                                               JACB0049
                                                                        JACB0050
              IF A[I0,I0] = A[J0,J0] THEN T ~ 1.0  ELSE                 JACB0051
              T ~ 2.0 | A[I0,J0] | SIGN(A[J0,J0] - A[I0,I0]) /          JACB0052
              (ABS(A[J0,J0] - A[I0,I0]) +                               JACB0053
              SQRT((A[J0,J0] - A[I0,I0])*2 + 4.0 | A[I0,J0]*2))  ;      JACB0054
                                                                        JACB0055
              C ~ 1 / SQRT(1 + T*2)  ;  S ~ T | C  ;                    JACB0056
              C1 ~ C*2  ;  S1 ~ S*2  ;  T1 ~ T*2  ;                     JACB0057
              TEMP ~ A[I0,I0]  ;                                        JACB0058
              A[I0,I0] ~ C1 | (A[I0,I0] - 2.0 | T | A[I0,J0] +          JACB0059
              T1 | A[J0,J0])  ;                                         JACB0060
              A[J0,J0] ~ C1 | (A[J0,J0] + 2.0 | T | A[I0,J0] +          JACB0061
              T1 | TEMP)  ;                                             JACB0062
              A[I0,J0] ~ 0  ;                                           JACB0063
                                                                        JACB0064
              J1 ~ J0 - 1  ;                                            JACB0065
              FOR J ~ 1 STEP 1 UNTIL J1 DO                              JACB0066
         BEGIN                                                          JACB0067
              TEMP ~ -S | A[J0,J] + C | A[I0,J]  ;                      JACB0068
              A[J0,J] ~ C | A[J0,J] + S | A[I0,J]  ;                    JACB0069
              A[I0,J] ~ TEMP                                            JACB0070
         END  ;                                                         JACB0071
                                                                        JACB0072
              I1 ~ J0 + 1  ;  I2 ~ I0 - 1  ;                            JACB0073
              FOR I ~ I1 STEP 1 UNTIL I2 DO                             JACB0074
         BEGIN                                                          JACB0075
              TEMP ~ -S | A[I,J0] + C | A[I0,I]  ;                      JACB0076
              A[I,J0] ~ C | A[I,J0] + S | A[I0,I]  ;                    JACB0077
              A[I0,I] ~ TEMP                                            JACB0078
         END  ;                                                         JACB0079
                                                                        JACB0080
              I1 ~ I0 + 1  ;                                            JACB0081
              FOR I ~ I1 STEP 1 UNTIL N DO                              JACB0082
         BEGIN                                                          JACB0083
              TEMP ~ -S | A[I,J0] + C | A[I,I0]  ;                      JACB0084
              A[I,J0] ~ C | A[I,J0] + S | A[I,I0]  ;                    JACB0085
              A[I,I0] ~ TEMP                                            JACB0086
         END  ;                                                         JACB0087
                                                                        JACB0088
              FOR I ~ 1 STEP 1 UNTIL N DO                               JACB0089
         BEGIN                                                          JACB0090
              TEMP ~ C | U[I,I0] - S | U[I,J0]  ;                       JACB0091
              U[I,J0] ~ S | U[I,I0] + C | U[I,J0]  ;                    JACB0092
              U[I,I0] ~ TEMP                                            JACB0093
         END                                                            JACB0094
         END                                                            JACB0095
         END  ;                                                         JACB0096
                                                                        JACB0097
COMMENT       TEST &F CU--ENT TH-ESHOLD V EXCEEDED AND,                 JACB00 8
              IF NOT, WHETHER FINAL THRESHOLD EPS REACHED  ;            JACB0099
                                                                        JACB0100
              IF CON = 1 THEN                                           JACB0101
         BEGIN                                                          JACB0102
              CON ~ 0  ;  GO TO L2                                      JACB0103
         END  ;                                                         JACB0104
                                                                        JACB0105
              IF V } VF THEN GO TO L1  ELSE                             JACB0106
              FOR I ~ 2 STEP 1 UNTIL N DO                               JACB0107
              FOR J ~ 1 STEP 1 UNTIL I-1 DO                             JACB0108
              A[J,I] ~ A[I,J]                                           JACB0109
         END  ;                                                         JACB0110
                                                                        TEST0053
              READ(CARD,FIN2,ACCURACY)  ;                               TEST0054
              FOR I ~ 1 STEP 1 UNTIL N DO  READ(CARD,FIN3,MATRIX)  ;    TEST0055
                                                                        TEST0056
              FOR I ~ 1 STEP 1 UNTIL N DO                               TEST0057
              FOR J ~ I+1 STEP 1 UNTIL N DO                             TEST0058
              AA[I,J] ~ AA[J,I] ~ A[J,I] ~ A[I,J]  ;                    TEST0059
              FOR I ~ 1 STEP 1 UNTIL N DO AA[I,I] ~ A[I,I]  ;           TEST0060
                                                                        TEST0061
              WRITE(PRINT,HEADING1)  ;                                  TEST0062
              FOR I ~ 1 STEP 1 UNTIL N DO  WRITE(PRINT,FOUT1,MATRIN)  ; TEST0063
                                                                        TEST0064
              JACOBI(N,VF,EPS,A,U)  ;                                   TEST0065
                                                                        TEST0066
              WRITE(PRINT,FOUT4,VFINAL)  ;                              TEST0067
              WRITE(PRINT,FOUT5,ACCUR)  ;                               TEST0068
              WRITE(PRINT,HEADING2)  ;                                  TEST0069
              WRITE(PRINT,FOUT2,EIGVAL)  ;                              TEST0070
              WRITE(PRINT,HEADING3)  ;                                  TEST0071
              CON ~ 0  ;                                                TEST0072
                                                                        TEST0073
L:            FOR K ~ 5 STEP 5 UNTIL N DO                               TEST0074
         BEGIN                                                          TEST0075
              WRITE(PRINT,FOUT6)  ;                                     TEST0076
              WRITE(PRINT,FOUT2,EIGVECT1)                               TEST0077
         END  ;                                                         TEST0078
              WRITE(PRINT,FOUT6)  ;                                     TEST0079
              IF K-5 < N THEN  WRITE(PRINT,FOUT3,EIGVECT2)  ;           TEST0080
              IF CON = 1 THEN GO TO FINISH  ;                           TEST0081
                                                                        TEST0082
              FOR J ~ 1 STEP 1 UNTIL N DO                               TEST0083
         BEGIN                                                          TEST0084
              FOR I ~ 1 STEP 1 UNTIL N DO                               TEST0085
         BEGIN                                                          TEST0086
              AX[I] ~ 0  ;                                              TEST0087
              FOR K ~ 1 STEP 1 UNTIL N DO AX[I] ~ AX[I] + AA[I,K]|U[K,J]TEST0088
         END  ;                                                         TEST0089
              FOR K ~ 1 STEP 1 UNTIL N DO U[K,J] ~ AX[K] - A[J,J]|U[K,J]TEST0090
         END  ;                                                         TEST0091
              WRITE(PRINT,HEADING4)  ;                                  TEST0092
              CON ~ 1  ;  GO TO L  ;                                    TEST0093
                                                                        TEST0094
FINISH:  END                                                            TEST0095
         END.                                                           TEST0096

