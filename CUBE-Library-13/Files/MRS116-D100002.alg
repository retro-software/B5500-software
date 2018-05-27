
 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
COMMENT       WEDDLES RULE USED RECURSIVEY TO APPROXIMATE THE INTEGRAL  TEST0001
              OF THE FUNCTION FKT BETWEEN THE LIMITS A AND B.           TEST0002
                                                                        TEST0003
              ANNE G. MACEK                                             TEST0004
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION)      TEST0005
                                                                        TEST0006
              PROCEDURE CARD SEQUENCE BEGINS WITH WDDL-0001             TEST0007
              FIRST RELEASE DATE 04-01-63  ;                            TEST0008
                                                                        TEST0009
         BEGIN                                                          TEST0010
                                                                        TEST0011
REAL          A,B,EPS,Z  ;                                              TEST0012
                                                                        TEST0013
LIST          BOUNDS(A,B,EPS),                                          TEST0014
              INTEGRAL(Z,A,B,EPS)  ;                                    TEST0015
                                                                        TEST0016
FORMAT IN     CARD(3E16.8)  ;                                           TEST0017
FORMAT OUT    FORMOUT("INTEGRAL =",E20.10//"BETWEEN BOUNDS X =",        TEST0018
                   E15.8," AND X =",E15.8//"TOLERANCE =",E15.8)  ;      TEST0019
                                                                        TEST0020
FILE IN       READER(1,10)  ;                                           TEST0021
FILE OUT      PRINTER(1,15)  ;                                          TEST0022
                                                                        TEST0023
REAL PROCEDURE  FKT(X)  ;                                               TEST0024
                                                                        TEST0025
VALUE         X  ;                                                      TEST0026
REAL           X  ;                                                     TEST0027
                                                                        TEST0028
              FKT ~ X*3 - X|5  ;                                        TEST0029
                                                                        TEST0030
REAL PROCEDURE WEDDLEINT(FKT,A,B,EPS)  ;                                WDDL0001
                                                                        WDDL0002
VALUE         A,B,EPS  ;                                                WDDL0003
REAL          A,B,EPS  ;                                                WDDL0004
REAL PROCEDURE FKT  ;                                                   WDDL0005
                                                                        WDDL0006
         BEGIN                                                          WDDL0007
                                                                        WDDL0008
INTEGER       LEVEL,J  ;                                                WDDL0009
REAL          INT  ;                                                    WDDL0010
ARRAY         F[0:5],C[0:5]  ;                                          WDDL0011
                                                                        WDDL0012
REAL PROCEDURE  WEDDLE(FKT,A,INT,FA,FB,F,ABSAREA,EST,EPS,C,LEVEL)  ;    WDDL0013
                                                                        WDDL0014
VALUE         A,INT,FA,FB,EST,EPS  ;                                    WDDL0015
INTEGER       LEVEL  ;                                                  WDDL0016
REAL          A,INT,FA,FB,ABSAREA,EST,EPS  ;                            WDDL0017
ARRAY         F[0],C[0]  ;                                              WDDL0018
REAL PROCEDURE  FKT  ;                                                  WDDL0019
                                                                        WDDL0020
         BEGIN                                                          WDDL0021
                                                                        WDDL0022
INTEGER       I  ;                                                      WDDL0023
REAL          DX,DDX,X1,X2,EST1,EST2,EST3,IDDX,SUM,WEDX  ;              WDDL0024
ARRAY         E,G,H[0:5]  ;                                             WDDL0025
                                                                        WDDL0026
              DX ~ INT/3.0  ;  DDX ~ DX/6.0  ;                          WDDL0027
              X1 ~ A + DX  ;  X2 ~ X1 + DX  ;                           WDDL0028
              EST1 ~ FA + F[2]  ;  EST2 ~ F[2] + F[4]  ;                WDDL0029
              EST3 ~ F[4] + FB  ;                                       WDDL0030
                                                                        WDDL0031
              FOR I ~ 1 STEP 1 UNTIL 5 DO                               WDDL0032
         BEGIN                                                          WDDL0033
              IDDX ~ I | DDX  ;                                         WDDL0034
              E[I] ~ FKT(A + IDDX) | C[I]  ;                            WDDL0035
              EST1 ~ E[I] + EST1  ;                                     WDDL0036
              G[I] ~ FKT(X1 + IDDX) | C[I]  ;                           WDDL0037
              EST2 ~ G[I] + EST2  ;                                     WDDL0038
              H[I] ~ FKT(X2 + IDDX) | C[I]  ;                           WDDL0039
              EST3 ~ H[I] + EST3                                        WDDL0040
         END  ;                                                         WDDL0041
                                                                        WDDL0042
              WEDX ~ DDX | 0.3  ;                                       WDDL0043
              EST1 ~ EST1 | WEDX  ;                                     WDDL0044
              EST2 ~ EST2 | WEDX  ;                                     WDDL0045
              EST3 ~ EST3 | WEDX  ;                                     WDDL0046
              ABSAREA ~ ABSAREA - ABS(EST) + ABS(EST1) + ABS(EST2) +    WDDL0047
                   ABS(EST3)  ;                                         WDDL0048
              SUM ~ EST1 + EST2 + EST3  ;                               WDDL0049
              LEVEL ~ LEVEL + 1  ;                                      WDDL0050
                                                                        WDDL0051
              WEDDLE ~ IF (ABS(EST-SUM) { EPS|ABSAREA AND EST ! 1) OR   WDDL0052
                   LEVEL } 7 THEN SUM                                   WDDL0053
              ELSE WEDDLE(FKT,A,DX,FA,F[2],E,ABSAREA,EST1,EPS/3.0,C,    WDDL0054
                   LEVEL) + WEDDLE(FKT,X1,DX,F[2],F[4],G,ABSAREA,EST2,  WDDL0055
                   EPS/3.0,C,LEVEL) + WEDDLE(FKT,X2,DX,F[4],FB,H,       WDDL0056
                   ABSAREA,EST3,EPS/3.0,C,LEVEL)  ;                     WDDL0057
                                                                        WDDL0058
              LEVEL ~ LEVEL - 1                                         WDDL0059
         END  ;                                                         WDDL0060
                                                                        WDDL0061
              LEVEL ~ 1  ;                                              WDDL0062
              INT ~ B - A  ;                                            WDDL0063
              FILL C[*] WITH 0,5.0,1.0,6.0,1.0,5.0  ;                   WDDL0064
              FOR J ~ 1 STEP 1 UNTIL 5 DO                               WDDL0065
              F[J] ~ FKT(A + (INT | J)/6.0 | C[J]) ;                    WDDL0066
                                                                        WDDL0067
              WEDDLEINT ~ WEDDLE(FKT,A,INT,FKT(A),FKT(B),F,1.0,1.0,EPS, WDDL0068
                   C,LEVEL)                                             WDDL0069
         END  ;                                                         WDDL0070
                                                                        TEST0031
              READ(READER,CARD,BOUNDS)  ;                               TEST0032
              Z ~ WEDDLEINT(FKT,A,B,EPS)  ;                             TEST0033
              WRITE(PRINTER,FORMOUT,INTEGRAL)                           TEST0034
                                                                        TEST0035
         END.                                                           TEST0036
