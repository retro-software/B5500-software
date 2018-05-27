
COMMENT       PROCEDURE TRIDIAG REDUCES A REAL SYMMETRIC MATRIX A       TEST0001
              OF ORDER N TO TRIDIAGONAL FORM (UT)AU (UT = TRANSPOSE     TEST0002
              OF U) BY GIVENS METHOD.                                   TEST0003
              ALSO, THE MATRIX U IS CALCULATED.                         TEST0004
                                                                        TEST0005
              GERARD F. DIETZEL                                         TEST0006
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.       TEST0007
                                                                        TEST0008
              PROCEDURE CARD SEQUENCE BEGINS WITH TRID-0001             TEST0009
              FIRST RELEASE DATE 5-14-1962  ;                           TEST0010
                                                                        TEST0011
         BEGIN                                                          TEST0012
                                                                        TEST0013
INTEGER       N  ;                                                      TEST0014
LIST          ORDER(N)  ;                                               TEST0015
FORMAT IN     FIN1(I1)  ;                                               TEST0016
FILE IN       CARD(1,10)  ;                                             TEST0017
FILE OUT      PRINT(1,15)  ;                                            TEST0018
                                                                        TEST0019
              READ(CARD,FIN1,ORDER)  ;                                  TEST0020
                                                                        TEST0021
         BEGIN                                                          TEST0022
                                                                        TEST0023
INTEGER       I,J  ;                                                    TEST0024
ARRAY         A,U[0:N,0:N]  ;                                           TEST0025
                                                                        TEST0026
LIST          MATRIX(FOR J ~ I STEP 1 UNTIL N DO A[I,J]),               TEST0027
              MATRIN(I, FOR J ~ 1 STEP 1 UNTIL N DO [J,A[I,J]]),        TEST0028
              MATRUTAU(I, FOR J ~ 1 STEP 1 UNTIL N DO [J,A[I,J]]),      TEST0029
              MATRU(I, FOR J ~ 1 STEP 1 UNTIL N DO [J,U[I,J]])  ;       TEST0030
                                                                        TEST0031
FORMAT IN     FIN2(4(F4.1,X2))  ;                                       TEST0032
                                                                        TEST0033
FORMAT OUT    FOUT(X15,"ROW",I3/X15,"COLUMN"/(X15,5(I4,F15.8))),        TEST0034
              HEADING1(//X55,"INPUT MATRIX A"//),                       TEST0035
              HEADING2(//X55,"TRIDIAGONAL MATRIX (UT)AU"//),            TEST0036
              HEADING3(//X55,"MATRIX U"//)  ;                           TEST0037
                                                                        TEST0038
PROCEDURE     TRIDIAG(N,A,U)  ;                                         TRID0001
                                                                        TRID0002
VALUE         N  ;                                                      TRID0003
INTEGER       N  ;                                                      TRID0004
ARRAY         A,U[0,0]  ;                                               TRID0005
                                                                        TRID0006
         BEGIN                                                          TRID0007
INTEGER       I,J,J1,J2,J3,J4,K,N1  ;                                   TRID0008
REAL          FACT,C1,C2,LOC1,LOC2,TEMP  ;                              TRID0009
LABEL         L  ;                                                      TRID0010
                                                                        TRID0011
COMMENT       SET MATRIX U = IDENTITY MATRIX OF ORDER N  ;              TRID0012
              FOR I ~ 1 STEP 1 UNTIL N DO                               TRID0013
         BEGIN                                                          TRID0014
              FOR J ~ I+1 STEP 1 UNTIL N DO                             TRID0015
              U[I,J] ~ U[J,I] ~ 0  ;                                    TRID0016
              U[I,I] ~ 1.0                                              TRID0017
         END  ;                                                         TRID0018
                                                                        TRID0019
COMMENT       THE REDUCTION OF THE MATRIX A BEGINS HERE.                TRID0020
              ONLY THE UPPER TRIANGULAR ELEMENTS OF A ARE USED          TRID0021
              IN THE COMPUTATION  ;                                     TRID0022
                                                                        TRID0023
              N1 ~ N - 2  ;                                             TRID0024
              FOR I ~ 1 STEP 1 UNTIL N1 DO                              TRID0025
         BEGIN                                                          TRID0026
                                                                        TRID0027
              J1 ~ I + 1  ;  J2 ~ I + 2  ;                              TRID0028
              FOR J ~ J2 STEP 1 UNTIL N DO                              TRID0029
         BEGIN                                                          TRID0030
              IF A[I,J] = 0 THEN GO TO L  ;                             TRID0031
              FACT ~ 1 / SQRT(A[I,J1]*2 + A[I,J]*2)  ;                  TRID0032
              C1 ~ FACT | A[I,J1]  ;  C2 ~ FACT | A[I,J]  ;             TRID0033
              LOC1 ~ A[J1,J1]  ;  LOC2 ~ A[J1,J]  ;                     TRID0034
              A[J1,J1] ~ C1*2|LOC1 + 2.0|C1|C2|LOC2 + C2*2|A[J,J]  ;    TRID0035
              A[J1,J] ~ -C1|C2|LOC1 + (C1*2 - C2*2)|LOC2 + C1|C2|A[J,J];TRID0036
              A[J,J] ~ C2*2|LOC1 - 2.0|C1|C2|LOC2 + C1*2|A[J,J]  ;      TRID0037
                                                                        TRID0038
              J3 ~ J + 1  ;                                             TRID0039
              FOR K ~ J3 STEP 1 UNTIL N DO                              TRID0040
         BEGIN                                                          TRID0041
              TEMP ~ A[J1,K]  ;                                         TRID0042
              A[J1,K] ~ C1|TEMP + C2|A[J,K]  ;                          TRID0043
              A[J,K] ~ -C2|TEMP + C1|A[J,K]                             TRID0044
         END  ;                                                         TRID0045
                                                                        TRID0046
              J4 ~ J - 1  ;                                             TRID0047
              FOR K ~ J2 STEP 1 UNTIL J4 DO                             TRID0048
         BEGIN                                                          TRID0049
              TEMP ~ A[J1,K]  ;                                         TRID0050
              A[J1,K] ~ C1|TEMP + C2|A[K,J]  ;                          TRID0051
              A[K,J] ~ -C2|TEMP + C1|A[K,J]                             TRID0052
         END  ;                                                         TRID0053
                                                                        TRID0054
              A[I,J1] ~ C1|A[I,J1] + C2|A[I,J]  ;                       TRID0055
              A[I,J] ~ 0  ;                                             TRID0056
                                                                        TRID0057
              FOR K ~ 1 STEP 1 UNTIL N DO                               TRID0058
         BEGIN                                                          TRID0059
              TEMP ~ U[K,J1]  ;                                         TRID0060
              U[K,J1] ~ C1|TEMP + C2|U[K,J]  ;                          TRID0061
              U[K,J] ~ -C2|TEMP + C1|U[K,J]                             TRID0062
         END  ;                                                         TRID0063
                                                                        TRID0064
L:       END                                                            TRID0065
                                                                        TRID0066
         END  ;                                                         TRID0067
                                                                        TRID0068
              FOR I ~ 1 STEP 1 UNTIL N DO                               TRID0069
              FOR J ~ I+1 STEP 1 UNTIL N DO                             TRID0070
              A[J,I] ~ A[I,J]                                           TRID0071
         END  ;                                                         TRID0072
                                                                        TEST0039
              FOR I ~ 1 STEP 1 UNTIL N DO  READ(CARD,FIN2,MATRIX)  ;    TEST0040
              WRITE(PRINT,HEADING1)  ;                                  TEST0041
              FOR I ~ 1 STEP 1 UNTIL N DO                               TEST0042
              FOR J ~ I+1 STEP 1 UNTIL N DO                             TEST0043
              A[J,I] ~ A[I,J]  ;                                        TEST0044
              FOR I ~ 1 STEP 1 UNTIL N DO WRITE(PRINT,FOUT,MATRIN)  ;   TEST0045
                                                                        TEST0046
              TRIDIAG(N,A,U)  ;                                         TEST0047
                                                                        TEST0048
              WRITE(PRINT,HEADING2)  ;                                  TEST0049
              FOR I ~ 1 STEP 1 UNTIL N DO  WRITE(PRINT,FOUT,MATRUTAU)  ;TEST0050
              WRITE(PRINT,HEADING3)  ;                                  TEST0051
              FOR I ~ 1 STEP 1 UNTIL N DO  WRITE(PRINT,FOUT,MATRU)      TEST0052
                                                                        TEST0053
         END                                                            TEST0054
                                                                        TEST0055
         END.                                                           TEST0056
