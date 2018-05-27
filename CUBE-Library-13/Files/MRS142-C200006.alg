 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
COMMENT       ROOTS OF A POLYNOMIAL BY THE SECANT METHOD.               TEST0001
                                                                        TEST0002
              J. K. KONDO                                               TEST0003
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION)  .   TEST0004
                                                                        TEST0005
              PROCEDURE CARD SEQUENCE START WITH SCAT0001.              TEST0006
              FIRST RELEASE DATE 04-15-1964  ;                          TEST0007
                                                                        TEST0008
                                                                        TEST0009
         BEGIN                                                          TEST0010
                                                                        TEST0011
FILE IN       CARD (1,10)  ;                                            TEST0012
FILE          PRINTER 1 (1,15)  ;                                       TEST0013
                                                                        TEST0014
LABEL         START, EOP  ;                                             TEST0015
REAL          LOWER, UPPER  ;                                           TEST0016
INTEGER       N  ;                                                      TEST0017
                                                                        TEST0018
FORMAT IN     FRMT(I3, 2F8.4)  ;                                        TEST0019
START:        READ(CARD, FRMT, N, LOWER, UPPER)[EOP]  ;                 TEST0020
                                                                        TEST0021
         BEGIN                                                          TEST0022
                                                                        TEST0023
ARRAY         A, RESULT[0:N]  ;                                         TEST0024
INTEGER       J  ;                                                      TEST0025
FORMAT IN     FIN (4E20.11)  ;                                          TEST0026
FORMAT        FOUT(X30, "ORIGINAL COEFFICIENTS"/(10F12.4)),             TEST0027
              FORM(X30, "ROOTS OF POLYNOMIAL"/(6E20.7))  ;              TEST0028
                                                                        TEST0029
PROCEDURE     ROOTS(N, A, LOWER, UPPER, RESULT)  ;                      SCAT0001
VALUE         N, LOWER, UPPER  ;                                        SCAT0002
INTEGER       N  ;                                                      SCAT0003
REAL          LOWER, UPPER  ;                                           SCAT0004
ARRAY         A, RESULT[0]  ;                                           SCAT0005
                                                                        SCAT0006
         BEGIN                                                          SCAT0007
                                                                        SCAT0008
COMMENT       PROCEDURE ROOTS DETERMINES THE ROOTS OF A POLYNOMIAL BY   SCAT0009
              THE SECANT METHOD.  IT IS ASSUMED THAT THE POLYNOMIAL HAS SCAT0010
              NO MULTIPLE ROOTS.  THE PARAMETERS OF THE PROCEDURE ROOTS:SCAT0011
                   N - THE DEGREE OF THE POLYNOMIAL                     SCAT0012
                   A - THE COEFFICIENTS OF THE POLYNOMIAL               SCAT0013
                   LOWER - THE LOWER BOUND OF THE ROOTS.                SCAT0014
                   UPPER - THE UPPER BOUND OF THE ROOTS.                SCAT0015
                   RESULT - THE COMPUTED ROOTS OF THE POLYNOMIAL.  ;    SCAT0016
                                                                        SCAT0017
INTEGER       I, J  ;                                                   SCAT0018
REAL          FACT  ;                                                   SCAT0019
ARRAY         ROOT, C[0:N]  ;                                           SCAT0020
                                                                        SCAT0021
REAL PROCEDURE SECANT(C, J, A, B)  ;                                    SCAT0022
VALUE         J, A, B  ;                                                SCAT0023
INTEGER       J  ;                                                      SCAT0024
REAL          A, B  ;                                                   SCAT0025
ARRAY         C[0]  ;                                                   SCAT0026
                                                                        SCAT0027
         BEGIN                                                          SCAT0028
                                                                        SCAT0029
COMMENT       PROCEDURE SECANT DETERMINES THE ROOT OF THE POLYNOMIAL    SCAT0030
              LOCATED IN THE INTERVAL (A,B). AN APPROXIMATION IS ACCEPT-SCAT0031
              ED AS A ROOT IF THE RELATIVE DIFFERENCE BETWEEN IT AND THESCAT0032
              PREVIOUS APPROXIMATION IS LESS THAN EPS.  IF THE APPROXI- SCAT0033
              MATION DOES NOT CONVERGE WITHIN A GIVEN NUMBER OF ITERA-  SCAT0034
              TIONS MAX, THE ROOT IS SET EQUAL TO THE LAST APPROXIMA-   SCAT0035
              TION .  ;                                                 SCAT0036
                                                                        SCAT0037
INTEGER       MAX, I  ;                                                 SCAT0038
REAL          C0, C1, FA, FB, FC1, EPS  ;                               SCAT0039
LABEL         IT  ;                                                     SCAT0040
                                                                        SCAT0041
REAL PROCEDURE FUNC(X, J, F)  ;                                         SCAT0042
VALUE         X, J  ;                                                   SCAT0043
REAL          X  ;                                                      SCAT0044
INTEGER       J  ;                                                      SCAT0045
ARRAY         F[0]  ;                                                   SCAT0046
         BEGIN                                                          SCAT0047
INTEGER       M  ;                                                      SCAT0048
ARRAY         P[0:J]  ;                                                 SCAT0049
              P[0] ~ F[0]  ;                                            SCAT0050
              FOR M ~ 1 STEP 1 UNTIL J DO                               SCAT0051
              P[M] ~ P[M-1] | X + F[M]  ;                               SCAT0052
              FUNC ~ P[J]  ;                                            SCAT0053
         END  ;                                                         SCAT0054
                                                                        SCAT0055
              EPS ~ @-10  ;  MAX ~ 50  ;                                SCAT0056
              I ~ 1  ;                                                  SCAT0057
              C0 ~ B  ;                                                 SCAT0058
              FB ~ FUNC(B, J, C)  ;                                     SCAT0059
              FA ~ FUNC(A, J, C)  ;                                     SCAT0060
                                                                        SCAT0061
IT:           C1 ~ (A | FB - B | FA) / (FB - FA)  ;                     SCAT0062
                                                                        SCAT0063
              IF ABS(C1 - C0) } ABS(C0) | EPS AND I < MAX THEN          SCAT0064
         BEGIN                                                          SCAT0065
              I ~ I + 1  ;                                              SCAT0066
              C0 ~ C1  ;                                                SCAT0067
              FC1 ~ FUNC(C1, J, C)  ;                                   SCAT0068
              IF SIGN(FC1) = SIGN(FB) THEN                              SCAT0069
         BEGIN                                                          SCAT0070
              FB ~ FC1  ;  B ~ C1                                       SCAT0071
         END                                                            SCAT0072
              ELSE                                                      SCAT0073
         BEGIN                                                          SCAT0074
              FA ~ FC1  ;  A ~ C1                                       SCAT0075
         END  ;                                                         SCAT0076
              GO TO IT                                                  SCAT0077
         END  ;                                                         SCAT0078
                                                                        SCAT0079
              SECANT ~ C1                                               SCAT0080
         END  PROCEDURE SECANT  ;                                       SCAT0081
                                                                        SCAT0082
              FACT ~ 1  ;                                               SCAT0083
              FOR J ~ 2 STEP 1 UNTIL N DO                               SCAT0084
         BEGIN                                                          SCAT0085
              FACT ~ FACT | J  ;                                        SCAT0086
              C[N-J] ~ A[N-J] | FACT                                    SCAT0087
         END  ;                                                         SCAT0088
              C[N-1] ~ A[N-1]  ;  C[N] ~ A[N]  ;                        SCAT0089
              ROOT[0] ~ LOWER  ;                                        SCAT0090
              ROOT[1] ~ -A[1] / (N | A[0])  ;                           SCAT0091
              ROOT[2] ~ UPPER  ;                                        SCAT0092
              C[0] ~ C[0] / 2.0  ;                                      SCAT0093
              RESULT[1] ~ SECANT(C,2,ROOT[0],ROOT[1])  ;                SCAT0094
              RESULT[2] ~ SECANT(C,2,ROOT[1],ROOT[2])  ;                SCAT0095
                                                                        SCAT0096
              FOR J ~ 3 STEP 1 UNTIL N DO                               SCAT0097
         BEGIN                                                          SCAT0098
              C[0] ~ C[0] / J  ;                                        SCAT0099
              FOR I ~ 1 STEP 1 UNTIL J - 1 DO                           SCAT0100
         BEGIN                                                          SCAT0101
              C[I] ~ C[I] / (J-I)  ;                                    SCAT0102
              ROOT[I] ~ RESULT[I]  ;                                    SCAT0103
         END  ;                                                         SCAT0104
              ROOT[J] ~ UPPER  ;                                        SCAT0105
              FOR I ~ 1 STEP 1 UNTIL J DO                               SCAT0106
              RESULT[I] ~ SECANT(C, J, ROOT[I-1],ROOT[I])               SCAT0107
         END                                                            SCAT0108
         END  PROCEDURE ROOTS  ;                                        SCAT0109
                                                                        TEST0030
              READ(CARD, FIN, FOR J ~ 0 STEP 1 UNTIL N DO A[J])  ;      TEST0031
              WRITE(PRINTER[DBL])  ;                                    TEST0032
              WRITE(PRINTER[DBL],FOUT,FOR J ~ 0 STEP 1 UNTIL N DO A[J]);TEST0033
              ROOTS(N, A, LOWER, UPPER, RESULT)  ;                      TEST0034
              WRITE(PRINTER, FORM, FOR J ~ 1 STEP 1 UNTIL N DO          TEST0035
                   RESULT[J])  ;                                        TEST0036
              GO TO START  ;                                            TEST0037
         END  ;                                                         TEST0038
EOP:     END  .                                                         TEST0039
 LABEL  000000000CARD   0010000001
  4  1.9     8.00
   1.0          @ 00  -1.975        @ 01   1.2930       @ 02  -3.138525     @ 02
   2.523339     @ 02
  4  0.5     4.5
   1.0          @ 00  -1.00         @ 01   3.50         @ 01  -5.0          @ 01
   2.40         @ 01
  3 -3.0     2.0
   1.0          @ 00   0.0                -7.0          @ 00   7.0          @ 00
