COMMENT       SIXTH ORDER RUNGE-KUTTA-NYSTROM STARTING PROCEDURE        TEST0001
                                                                        TEST0002
              A. F. PIXLEY AND ANNE G. MACEK                            TEST0003
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION)      TEST0004
                                                                        TEST0005
              PROCEDURE CARD SEQUENCE BEGINS WITH RKST-0001             TEST0006
              FIRST RELEASE DATE 06-25-63  ;                            TEST0007
                                                                        TEST0008
         BEGIN                                                          TEST0009
                                                                        TEST0010
INTEGER       K,NF,J,I  ;                                               TEST0011
REAL          X1,H  ;                                                   TEST0012
ARRAY         Y[0:10,0:8]  ;                                            TEST0013
                                                                        TEST0014
LIST          IN1(X1,H,K,NF),                                           TEST0015
              IN2(FOR J ~ 1 STEP 1 UNTIL K DO Y[0,J]),                  TEST0016
              OUTH(H),                                                  TEST0017
              OUT1(I,I|H+X1,FOR J ~ 1 STEP 1 UNTIL K DO Y[I,J])  ;      TEST0018
                                                                        TEST0019
FORMAT IN     FORM1(2E20.10,2I10),                                      TEST0020
              FORM2(4E20.10)  ;                                         TEST0021
                                                                        TEST0022
FORMAT OUT    FOUT1("INCREMENT =",E20.10),                              TEST0023
              FOUT2("EQ. NO.",X10,"X",X16,"Y[I,1]",X14,"Y[I,2]",X14,    TEST0024
                   "Y[I,3]",X14,"Y[I,4]"),                              TEST0025
              FOUT3(I7,5E20.10)  ;                                      TEST0026
                                                                        TEST0027
FILE IN       RDR(1,10)  ;                                              TEST0028
FILE OUT      PRINTER(1,15)  ;                                          TEST0029
                                                                        TEST0030
PROCEDURE     F(X,Y,DX)  ;                                              TEST0031
                                                                        TEST0032
VALUE         X  ;                                                      TEST0033
REAL          X  ;                                                      TEST0034
ARRAY         Y[0],DX[0]  ;                                             TEST0035
                                                                        TEST0036
         BEGIN                                                          TEST0037
              DX[1] ~ Y[1] | 2.0 - Y[2] - Y[3] - Y[4]  ;                TEST0038
              DX[2] ~ Y[1] | 3.0 - Y[2] | 2.0 - Y[3] - Y[4]  ;          TEST0039
              DX[3] ~ Y[1] | 2.0 - Y[3] | 2.0 - Y[4]  ;                 TEST0040
              DX[4] ~ Y[1] | 2.0 - Y[2] - Y[4] | 2.0                    TEST0041
         END  ;                                                         TEST0042
                                                                        TEST0043
PROCEDURE     RKSTARTS(K,NF,X1,H,Y,F)  ;                                RKST0001
                                                                        RKST0002
VALUE         K,NF,H  ;                                                 RKST0003
REAL          X1,H  ;                                                   RKST0004
INTEGER       K,NF  ;                                                   RKST0005
ARRAY         Y[0,0]  ;                                                 RKST0006
PROCEDURE     F  ;                                                      RKST0007
                                                                        RKST0008
         BEGIN                                                          RKST0009
INTEGER       I,J  ;                                                    RKST0010
REAL          X  ;                                                      RKST0011
ARRAY         DX,TEMPY,K1,K2,K3,K4,K5,K6[0:K]  ;                        RKST0012
                                                                        RKST0013
              FOR I ~ 0 STEP 1 UNTIL (NF - 1) DO                        RKST0014
         BEGIN                                                          RKST0015
              X ~ H | I + X1  ;                                         RKST0016
              F(X,Y[I,*],DX)  ;                                         RKST0017
                                                                        RKST0018
              FOR J ~ 1 STEP 1 UNTIL K DO                               RKST0019
         BEGIN                                                          RKST0020
              K1[J] ~ DX[J] | H  ;                                      RKST0021
              TEMPY[J] ~ K1[J]/3.0 + Y[I,J]                             RKST0022
         END  ;                                                         RKST0023
              F(H/3.0 + X,TEMPY,DX)  ;                                  RKST0024
                                                                        RKST0025
              FOR J ~ 1 STEP 1 UNTIL K DO                               RKST0026
         BEGIN                                                          RKST0027
              K2[J] ~ DX[J] | H  ;                                      RKST0028
              TEMPY[J] ~ (K2[J] | 6.0 + K1[J] | 4.0)/25.0 + Y[I,J]      RKST0029
         END  ;                                                         RKST0030
              F((H | 2.0)/5.0 + X,TEMPY,DX)  ;                          RKST0031
                                                                        RKST0032
              FOR J ~ 1 STEP 1 UNTIL K DO                               RKST0033
         BEGIN                                                          RKST0034
              K3[J] ~ DX[J] | H  ;                                      RKST0035
              TEMPY[J] ~ (K3[J] | 15.0 - K2[J] | 12.0 + K1[J])/4.0      RKST0036
                   + Y[I,J]                                             RKST0037
         END  ;                                                         RKST0038
              F(H + X,TEMPY,DX)  ;                                      RKST0039
                                                                        RKST0040
              FOR J ~ 1 STEP 1 UNTIL K DO                               RKST0041
         BEGIN                                                          RKST0042
              K4[J] ~ DX[J] | H  ;                                      RKST0043
              TEMPY[J] ~ (K4[J] | 8.0 - K3[J] | 50.0 + K2[J] | 90.0 +   RKST0044
                   K1[J] | 6.0)/81.0 + Y[I,J]                           RKST0045
         END  ;                                                         RKST0046
              F((H | 2.0)/3.0 + X,TEMPY,DX)  ;                          RKST0047
                                                                        RKST0048
              FOR J ~ 1 STEP 1 UNTIL K DO                               RKST0049
         BEGIN                                                          RKST0050
              K5[J] ~ DX[J] | H  ;                                      RKST0051
              TEMPY[J] ~ (K4[J] | 8.0 + K3[J] | 10.0 + K2[J] | 36.0 +   RKST0052
                   K1[J] | 6.0)/75.0 + Y[I,J]                           RKST0053
         END  ;                                                         RKST0054
              F((H | 4.0)/5.0 + X,TEMPY,DX)  ;                          RKST0055
                                                                        RKST0056
              FOR J ~ 1 STEP 1 UNTIL K DO                               RKST0057
         BEGIN                                                          RKST0058
              K6[J] ~ DX[J] | H  ;                                      RKST0059
              Y[I+1,J] ~ (K1[J] | 23.0 + K3[J] | 125.0 - K5[J] | 81.0 + RKST0060
                   K6[J] | 125.0)/192.0 + Y[I,J]                        RKST0061
         END                                                            RKST0062
         END                                                            RKST0063
         END  ;                                                         RKST0064
                                                                        TEST0044
              READ(RDR,FORM1,IN1)  ;  READ(RDR,FORM2,IN2)  ;            TEST0045
              RKSTARTS(K,NF,X1,H,Y,F)  ;                                TEST0046
              WRITE(PRINTER,FOUT1,OUTH)  ;  WRITE(PRINTER,FOUT2)  ;     TEST0047
                                                                        TEST0048
              FOR I ~ 0 STEP 1 UNTIL NF DO WRITE(PRINTER,FOUT3,OUT1)    TEST0049
                                                                        TEST0050
         END.                                                           TEST0051
