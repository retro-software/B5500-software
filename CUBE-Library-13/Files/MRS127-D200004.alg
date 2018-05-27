 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
COMMENT       THE CLIPPINGER DIMSDALE METHOD FOR SOLVING A SYSTEM OF    TEST0001
              FIRST ORDER DIFFERENTIAL EQUATIONS.                       TEST0002
                                                                        TEST0003
              ANNE G. MACEK                                             TEST0004
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION)            TEST0005
                                                                        TEST0006
              CARD SEQUENCE CODE STARTS WITH CDIM 0001                  TEST0007
              FIRST ISSUE 01/17/64                                     ;TEST0008
                                                                        TEST0009
         BEGIN                                                          TEST0010
                                                                        TEST0011
REAL          H,X,XMAX,EMAX,EMIN,E  ;                                   TEST0012
INTEGER       NOEQ,NPRINT,I  ;                                          TEST0013
ARRAY         Y[0:3]  ;                                                 TEST0014
                                                                        TEST0015
LIST          IN1(H,X,XMAX,EMAX,EMIN,E,NPRINT),                         TEST0016
              IN2(NOEQ,FOR I ~ 1 STEP 1 UNTIL 3 DO Y[I])  ;             TEST0017
                                                                        TEST0018
                                                                        TEST0019
FORMAT IN     FIN1(3E12.5,3E10.3,I4),                                   TEST0020
              FIN2(I3,3E15.8)  ;                                        TEST0021
                                                                        TEST0022
FILE IN       READER(1,10)  ;                                           TEST0023
FILE OUT      PRINTER(1,15)  ;                                          TEST0024
                                                                        TEST0025
PROCEDURE     F(X,Y,DY)  ;                                              FNCT0001
                                                                        FNCT0002
VALUE         X  ;                                                      FNCT0003
REAL          X  ;                                                      FNCT0004
ARRAY         Y[0],DY[0]  ;                                             FNCT0005
                                                                        FNCT0006
         BEGIN                                                          FNCT0007
COMMENT       FUNCTION ROUTINE  ;                                       FNCT0008
                                                                        FNCT0009
              DY[1] ~ Y[2] | 2.0 - Y[3] | 3.0  ;                        FNCT0010
              DY[2] ~ Y[1] - Y[2] - Y[3]  ;                             FNCT0011
              DY[3] ~ Y[1] - Y[3] | 2.0                                 FNCT0012
         END  ;                                                         FNCT0013
                                                                        FNCT0014
PROCEDURE     PRINT(X,H,NOEQ,Y,DY)  ;                                   PRNT0001
                                                                        PRNT0002
VALUE         X,H,NOEQ  ;                                               PRNT0003
INTEGER       X,H,NOEQ  ;                                               PRNT0004
ARRAY         Y,DY[0]  ;                                                PRNT0005
                                                                        PRNT0006
         BEGIN                                                          PRNT0007
INTEGER       I  ;                                                      PRNT0008
                                                                        PRNT0009
LIST          XH(X,H),                                                  PRNT0010
              RESULTS(I,Y[I],DY[I])  ;                                  PRNT0011
                                                                        PRNT0012
FORMAT OUT    FORMXH("X = ",E15.8,"  H = ",E15.8),                      PRNT0013
              FORMTITLE(X9,"I",X10,"Y",X17,"DY"),                       PRNT0014
              FORMR(I10,2E19.10)  ;                                     PRNT0015
                                                                        PRNT0016
              WRITE(PRINTER[DBL],FORMXH,XH)  ;                          PRNT0017
              WRITE(PRINTER,FORMTITLE)  ;                               PRNT0018
              FOR I ~ 1 STEP 1 UNTIL NOEQ DO                            PRNT0019
              WRITE(PRINTER,FORMR,RESULTS)  ;                           PRNT0020
              WRITE(PRINTER[DBL])                                       PRNT0021
         END  ;                                                         PRNT0022
                                                                        PRNT0023
PROCEDURE     CLIPDIM(H,X,NOEQ,Y,XMAX,EMAX,EMIN,E,NPRINT,F,PRINT)  ;    CDIM0001
                                                                        CDIM0002
VALUE         NOEQ,XMAX,EMAX,EMIN,E,NPRINT  ;                           CDIM0003
INTEGER       NOEQ,NPRINT  ;                                            CDIM0004
REAL          X,H,XMAX,EMAX,EMIN,E  ;                                   CDIM0005
ARRAY         Y[0]  ;                                                   CDIM0006
PROCEDURE     F,PRINT  ;                                                CDIM0007
                                                                        CDIM0008
         BEGIN                                                          CDIM0009
INTEGER       I,J,N  ;                                                  CDIM0010
REAL          CO  ;                                                     CDIM0011
ARRAY         DY,Y1,DY1,Y1C,DY1C,HY,DHY,HYC[0:NOEQ]  ;                  CDIM0012
BOOLEAN       BOOL,BOOLH  ;                                             CDIM0013
LABEL         L1,L1A,L2,L3,L4,L5,RET  ;                                 CDIM0014
                                                                        CDIM0015
              N ~ 0  ;                                                  CDIM0016
              F(X,Y,DY)  ;                                              CDIM0017
                                                                        CDIM0018
L1:           PRINT(X,H,NOEQ,Y,DY)  ;                                   CDIM0019
                                                                        CDIM0020
L1A:          BOOL ~ BOOLH ~ FALSE  ;                                   CDIM0021
              IF (X+H) > XMAX THEN GO TO RET  ;                         CDIM0022
                                                                        CDIM0023
L2:           FOR I ~ 1 STEP 1 UNTIL NOEQ DO                            CDIM0024
              Y1[I] ~ DY[I] | H + Y[I]  ;                               CDIM0025
              F(X+H,Y1,DY1)  ;                                          CDIM0026
              FOR I ~ 1 STEP 1 UNTIL NOEQ DO                            CDIM0027
              HY[I] ~ (Y[I] + Y1[I]) | (0.5) + (DY[I] - DY1[I]) | H |   CDIM0028
                   (0.125)  ;                                           CDIM0029
                                                                        CDIM0030
L3:           F(H|0.5+X,HY,DHY)  ;                                      CDIM0031
              FOR I ~ 1 STEP 1 UNTIL NOEQ DO                            CDIM0032
              Y1C[I] ~ ((DHY[I] | 4.0 + DY[I] + DY1[I]) | H)/6.0 + Y[I];CDIM0033
              F(H+X,Y1C,DY1C)  ;                                        CDIM0034
              FOR I ~ 1 STEP 1 UNTIL NOEQ DO                            CDIM0035
              HYC[I] ~ (Y[I] + Y1C[I]) | (0.5) + (DY[I] - DY1C[I]) | H  CDIM0036
                   | (0.125)  ;                                         CDIM0037
                                                                        CDIM0038
              FOR I ~ 1 STEP 1 UNTIL NOEQ DO                            CDIM0039
         BEGIN                                                          CDIM0040
              CO ~ ABS((Y1C[I] - Y1[I])/Y1[I])  ;                       CDIM0041
              IF BOOL THEN GO TO L5   ;                                 CDIM0042
              IF CO > EMAX THEN                                         CDIM0043
         BEGIN                                                          CDIM0044
              BOOLH ~ TRUE  ;                                           CDIM0045
              H ~ H | 0.5  ;  GO TO L2                                  CDIM0046
         END  ;                                                         CDIM0047
              IF BOOLH THEN GO TO L5  ;                                 CDIM0048
                                                                        CDIM0049
              IF (I = 1) AND (CO < EMIN) AND ((H|2+X) < XMAX) THEN      CDIM0050
         BEGIN                                                          CDIM0051
              FOR J ~ 2 STEP 1 UNTIL NOEQ DO                            CDIM0052
         BEGIN                                                          CDIM0053
              CO ~ ABS((Y1C[J] - Y1[J])/Y1[J])  ;                       CDIM0054
              IF CO } EMIN THEN                                         CDIM0055
         BEGIN                                                          CDIM0056
              I ~ J  ;  GO TO L4                                        CDIM0057
         END                                                            CDIM0058
         END  ;                                                         CDIM0059
              H ~ H + H  ;  GO TO L2  ;                                 CDIM0060
L4:      END  ;                                                         CDIM0061
              BOOL ~ TRUE  ;                                            CDIM0062
                                                                        CDIM0063
L5:           IF CO > E THEN                                            CDIM0064
         BEGIN                                                          CDIM0065
              FOR J ~ 1 STEP 1 UNTIL NOEQ DO                            CDIM0066
         BEGIN                                                          CDIM0067
              HY[J] ~ HYC[J]  ;  Y1[J] ~ Y1C[J]  ;  DY1[J] ~ DY1C[J]    CDIM0068
         END  ;                                                         CDIM0069
              GO TO L3                                                  CDIM0070
         END                                                            CDIM0071
         END  ;                                                         CDIM0072
                                                                        CDIM0073
              X ~ X + H  ;                                              CDIM0074
              FOR I ~ 1 STEP 1 UNTIL NOEQ DO                            CDIM0075
         BEGIN                                                          CDIM0076
              Y[I] ~ Y1C[I]  ;  DY[I] ~ DY1C[I]                         CDIM0077
         END  ;                                                         CDIM0078
              N ~ N + 1  ;                                              CDIM0079
              IF N < NPRINT THEN GO TO L1A  ;                           CDIM0080
              N ~ 0  ;  GO TO L1  ;                                     CDIM0081
                                                                        CDIM0082
RET:     END  ;                                                         CDIM0083
                                                                        TEST0026
              READ(READER,FIN1,IN1)  ;                                  TEST0027
              READ(READER,FIN2,IN2)  ;                                  TEST0028
              CLIPDIM(H,X,NOEQ,Y,XMAX,EMAX,EMIN,E,NPRINT,F,PRINT)  ;    TEST0029
                                                                        TEST0030
         END.                                                           TEST0031

 LABEL  000000000READER 0010000001
  3+2.00000000@+00+1.00000000@+00+0.00000000@+00

+0.10000@-01+0.00000@+00+1.00000@+01+0.100@-06+0.100@-08+0.100@-07 1600
