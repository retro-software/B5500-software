
 LABEL  0000000000XXXXXX0010000001
$ CARD
COMMENT       PROCEDURE TO EVALUATE THE COEFFICIENTS OF                 TEST0000
              EMPIRICAL FORMULAS.                                       TEST0001
                                                                        TEST0002
              STEPHEN MATZNER                                           TEST0003
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION)      TEST0004
                                                                        TEST0005
              PROCEDURE CARD SEQUENCE BEGINS WITH EMPI-0000             TEST0006
              FIRST RELEASE DATE 04 - 08 - 64  ;                        TEST0007
                                                                        TEST0008
         BEGIN                                                          TEST0009
PROCEDURE     EMPFOR(P,Y,A,B)  ;                                        EMPI0000
                                                                        EMPI0001
VALUE         P, Y  ;                                                   EMPI0002
REAL ARRAY    A, B, Y[0]  ;                                             EMPI0003
INTEGER       P  ;                                                      EMPI0004
                                                                        EMPI0005
         BEGIN                                                          EMPI0006
INTEGER       I, J, L, Z, E, D, TEM, F, PH, Q, PQ, PQ1  ;               EMPI0007
REAL          M, K, CON  ;                                              EMPI0008
REAL ARRAY    C, G[0:100]  ;                                            EMPI0009
                                                                        EMPI0010
              FOR I ~ 0 STEP 1 UNTIL P-1 DO                             EMPI0011
              A[I] ~ B[I] ~ 0  ;                                        EMPI0012
              PH ~ P | 0.5  ;  PQ ~ P | 0.25  ;  PQ1 ~ PQ - 1  ;        EMPI0013
              D ~ 1  ;  L ~ 90  ;  TEM ~ -D  ;  Q ~ 1  ;                EMPI0014
                                                                        EMPI0015
              FOR F ~ 1 STEP 1 UNTIL P-1 DO                             EMPI0016
         BEGIN                                                          EMPI0017
              IF F > PH THEN E ~ F - PH ELSE E ~ F  ;                   EMPI0018
              D ~ -D  ;  L ~ -L  ;  TEM ~ -TEM  ;                       EMPI0019
                                                                        EMPI0020
              FOR J ~ 1 STEP 1 UNTIL PQ1 DO                             EMPI0021
         BEGIN                                                          EMPI0022
              Z ~ 1  ;  M ~ E | J | TEM | 45 + L  ;                     EMPI0023
              CON ~ J | E | 45  ;  K ~ 0  ;                             EMPI0024
                                                                        EMPI0025
              FOR I ~ 1 STEP 1 UNTIL PQ1 DO                             EMPI0026
         BEGIN                                                          EMPI0027
              M ~ M + K | D  ;                                          EMPI0028
              K ~ CON/(I + Z + 1)  ;  Z ~ Z + I + 1  ;                  EMPI0029
         END ;                                                          EMPI0030
              C[PH+J] ~ SIN(M | 0.01745329252)  ;                       EMPI0031
         END ;                                                          EMPI0032
              IF F { PH THEN                                            EMPI0033
         BEGIN                                                          EMPI0034
              C[PH] ~ D | C[0] ~ 1  ;                                   EMPI0035
                                                                        EMPI0036
              FOR J ~ 1 STEP 1 UNTIL PQ1 DO                             EMPI0037
         BEGIN                                                          EMPI0038
              C[PH-J] ~ C[PH+J]  ;                                      EMPI0039
              C[P-J] ~ C[J] ~ D | C[PH-J]  ;                            EMPI0040
         END ;                                                          EMPI0041
         END                                                            EMPI0042
         ELSE                                                           EMPI0043
         BEGIN                                                          EMPI0044
              C[PH] ~ C[0] ~ 0  ;                                       EMPI0045
                                                                        EMPI0046
              FOR J ~ 1 STEP 1 UNTIL PQ1 DO                             EMPI0047
         BEGIN                                                          EMPI0048
              C[PH-J] ~ -C[PH+J]  ;                                     EMPI0049
              C[P-J] ~ -C[J] ~ D | C[PH-J]  ;                           EMPI0050
         END ;                                                          EMPI0051
         END ;                                                          EMPI0052
              IF D = -1 THEN C[PQ] ~ C[3|PQ] ~ 0 ELSE                   EMPI0053
              IF F { PH THEN                                            EMPI0054
              C[3|PQ] ~ C[PQ] ~ Q ~ -Q ELSE                             EMPI0055
              C[3|PQ] ~ -C[PQ] ~ Q ~ -Q  ;                              EMPI0056
                                                                        EMPI0057
              FOR I ~ 0 STEP 1 UNTIL P-1 DO                             EMPI0058
              IF F > PH THEN                                            EMPI0059
              B[E] ~ Y[I] | C[I] | 2/P + B[E] ELSE                      EMPI0060
              A[E] ~ Y[I] | C[I] | 2/P + A[E]  ;                        EMPI0061
              IF F = PH THEN                                            EMPI0062
         BEGIN                                                          EMPI0063
              D ~ -D  ;  L ~ 0  ;  TEM ~ 1  ;  Q ~ -1  ;  A[E] ~ A[E]/2 EMPI0064
         END ;                                                          EMPI0065
         END ;                                                          EMPI0066
                                                                        EMPI0067
              FOR I ~ 0 STEP 1 UNTIL P-1 DO                             EMPI0068
              A[0] ~ Y[I]/P + A[0]  ;                                   EMPI0069
         END ;                                                          EMPI0070
         BEGIN                                                          TEST0010
                                                                        TEST0011
INTEGER       I, P, PH, R, J  ;                                         TEST0012
REAL          PER  ;                                                    TEST0013
REAL ARRAY    A,B,X,Y,G[0:100]  ;                                       TEST0014
ALPHA ARRAY   S[0:100]  ;                                               TEST0015
                                                                        TEST0016
FILE IN       FIN(1,10)  ;                                              TEST0017
FILE OUT      FOU 1 (1,15)  ;                                           TEST0018
                                                                        TEST0019
FORMAT IN     FM1(I2,F5.3),                                             TEST0020
              FM2(I2),                                                  TEST0021
              FM3(8F10.5)  ;                                            TEST0022
                                                                        TEST0023
FORMAT OUT    FM4(F9.4," : ",F9.4),                                     TEST0024
              TIT1("Y = ",F9.4,X2,),                                    TEST0025
              TIT2(X15,4(A3,F9.4," COS ",I2," X",X2)/),                 TEST0026
              TIT3(X15,4(A3,F9.4," SIN ",I2," X",X2)/)  ;               TEST0027
                                                                        TEST0028
LIST          DATAY(FOR I ~ 0 STEP 1 UNTIL P-1 DO Y[I]),                TEST0029
              TITLI1(FOR I ~ 1 STEP 1 UNTIL PH DO [S[I],ABS(A[I]),I]),  TEST0030
              TITLI2(FOR I ~ 1 STEP 1 UNTIL PH-1 DO [S[I],ABS(B[I]),I]),TEST0031
              DATAX(FOR I ~ 1 STEP 1 UNTIL R DO X[I])  ;                TEST0032
                                                                        TEST0033
              READ(FIN,FM2,P)  ;                                        TEST0034
              READ(FIN,FM3,DATAY)  ;                                    TEST0035
              READ(FIN,FM1,R,PER)  ;                                    TEST0036
              READ(FIN,FM3,DATAX)  ;                                    TEST0037
              EMPFOR(P,Y,A,B)  ;                                        TEST0038
              PH ~ P | 0.5  ;                                           TEST0039
                                                                        TEST0040
COMMENT       PRINT OUT OF EMPIRICAL FORMULA  ;                         TEST0041
                                                                        TEST0042
              WRITE(FOU[NO],TIT1,A[0])  ;                               TEST0043
                                                                        TEST0044
              FOR I ~ 1 STEP 1 UNTIL PH DO                              TEST0045
              IF A[I] } 0 THEN S[I] ~ " + " ELSE S[I] ~ " - "  ;        TEST0046
              WRITE(FOU,TIT2,TITLI1)  ;                                 TEST0047
                                                                        TEST0048
              FOR I ~ 1 STEP 1 UNTIL PH-1 DO                            TEST0059
              IF B[I] } 0 THEN S[I] ~ " + " ELSE S[I] ~ " - "  ;        TEST0050
              WRITE(FOU,TIT3,TITLI2)  ;                                 TEST0051
              WRITE(FOU[DBL])  ;                                        TEST0052
                                                                        TEST0053
COMMENT       EVALUATION OF EMPIRICAL FORMULA  ;                        TEST0054
                                                                        TEST0055
              FOR I ~ 1 STEP 1 UNTIL R DO                               TEST0056
         BEGIN                                                          TEST0057
              G[I] ~ 0  ;  X[I] ~ X[I] | PER  ;                         TEST0058
                                                                        TEST0059
              FOR J ~ 1 STEP 1 UNTIL PH DO                              TEST0060
              G[I] ~ COS(0.01745329252|X[I]|J) | A[J] + G[I]  ;         TEST0061
                                                                        TEST0062
              FOR J ~ 1 STEP 1 UNTIL PH-1 DO                            TEST0063
              G[I] ~ SIN(0.01745329252|X[I]|J) | B[J] + G[I]  ;         TEST0064
              G[I] ~ A[0] + G[I]  ;                                     TEST0065
              WRITE(FOU,FM4,X[I],G[I])  ;                               TEST0066
         END ;                                                          TEST0067
         END ;                                                          TEST0068
         END .                                                          TEST0069
 LABEL  000000000FIN    0010000001
24
 149.0     137.0     128.0     126.0     128.0     135.0     159.0     178.0
 189.0     191.0     189.0     187.0     178.0     170.0     177.0     183.0
 181.0     179.0     179.0     185.0     182.0     176.0     166.0     160.0
 61.0
  50.0     100.0     150.0     200.0     250.0     300.0
