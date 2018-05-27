COMMENT       THE CLIPPINGER DIMSDALE METHOD FOR SOLVING A FIRST ORDER  TEST0001
              ORDINARY DIFFERENTIAL EQUATION                            TEST0002
                                                                        TEST0003
              ANNE G. MACEK                                             TEST0004
              (PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION)      TEST0005
              PROCEDURE CARD SEQUENCE BEGINS WITH CDIM-0001             TEST0007
                                                                        TEST0006
              FIRST RELEASE DATE 04-01-63  ;                            TEST0008
                                                                        TEST0009
         BEGIN                                                          TEST0010
                                                                        TEST0011
INTEGER       N,NPRINT  ;                                               TEST0012
REAL          X0,Y0,H,XMAX,EMAX,EMIN,E  ;                               TEST0013
LIST          INPUT(X0,Y0,H,XMAX,EMAX,EMIN,E,NPRINT)  ;                 TEST0014
FORMAT IN     DATA(4E11.3,3E10.2,I4)  ;                                 TEST0015
FILE IN       READER(1,10)  ;                                           TEST0016
FILE OUT      PRINTER(1,15)  ;                                          TEST0017
                                                                        TEST0018
REAL PROCEDURE F(X,Y)  ;                                                TEST0019
                                                                        TEST0020
VALUE         X,Y  ;                                                    TEST0021
REAL          X,Y  ;                                                    TEST0022
                                                                        TEST0023
              F ~ -Y*2 | X | 2.0  ;                                     TEST0024
                                                                        TEST0025
                                                                        TEST0026
PROCEDURE     PRINT(X,H,Y,DY,LIN)  ;                                    TEST0027
                                                                        TEST0028
VALUE         X,H,Y,DY  ;                                               TEST0029
INTEGER       LIN  ;                                                    TEST0030
REAL          X,H,Y,DY  ;                                               TEST0031
                                                                        TEST0032
         BEGIN                                                          TEST0033
                                                                        TEST0034
LIST          OUTPUT(X,H,Y,DY,1/(1 + X*2))  ;                           TEST0035
FORMAT OUT    FORMH(X8,"X",X15,"H",X15,"Y",X15,"DY"),                   TEST0036
              FORM(5F16.8)  ;                                           TEST0037
                                                                        TEST0038
              IF LIN = 0 THEN                                           TEST0039
         BEGIN                                                          TEST0040
              WRITE(PRINTER[PAGE])  ;                                   TEST0041
              WRITE(PRINTER[DBL],FORMH)                                 TEST0042
         END  ;                                                         TEST0043
              WRITE(PRINTER,FORM,OUTPUT)  ;                             TEST0044
              LIN ~ LIN + 1 ;                                           TEST0045
              IF LIN > 50 THEN LIN ~ 0                                  TEST0046
         END  ;                                                         TEST0047
                                                                        TEST0048
PROCEDURE     CLIPDIM(X,Y,H,XMAX,EMAX,EMIN,E,NPRINT,F,PRINT)  ;         CDIM0001
                                                                        CDIM0002
VALUE         XMAX,EMAX,EMIN,E,NPRINT  ;                                CDIM0003
INTEGER       NPRINT  ;                                                 CDIM0004
REAL          X,Y,H,XMAX,EMAX,EMIN,E  ;                                 CDIM0005
REAL PROCEDURE F  ;                                                     CDIM0006
PROCEDURE     PRINT  ;                                                  CDIM0007
                                                                        CDIM0008
         BEGIN                                                          CDIM0009
                                                                        CDIM0010
INTEGER       N,LIN  ;                                                  CDIM0011
REAL          DY,Y1,DY1,Y1C,DY1C,HY,DHY,HYC,CO  ;                       CDIM0012
BOOLEAN       BOOL,BOOLH  ;                                             CDIM0013
LABEL         L1,L1A,L2,L3,L4,RET  ;                                    CDIM0014
                                                                        CDIM0015
              N ~ LIN ~ 0  ;                                            CDIM0016
              DY ~ F(X,Y)  ;                                            CDIM0017
                                                                        CDIM0018
L1:           PRINT(X,H,Y,DY,LIN)  ;                                    CDIM0019
                                                                        CDIM0020
L1A:          BOOL ~ BOOLH ~ FALSE  ;                                   CDIM0021
              IF (X+H) > XMAX THEN GO TO RET  ;                         CDIM0022
                                                                        CDIM0023
L2:           Y1 ~ Y + H | DY  ;                                        CDIM0024
              DY1 ~ F(X+H,Y1)  ;                                        CDIM0025
              HY ~ 0.5 | (Y + Y1) + 0.125 | H | (DY - DY1)  ;           CDIM0026
                                                                        CDIM0027
L3:           DHY ~ F(X + 0.5 | H,HY)  ;                                CDIM0028
              Y1C ~ Y + H | (DY + DHY | 4.0 + DY1)/6.0  ;               CDIM0029
              DY1C ~ F(X+H,Y1C)  ;                                      CDIM0030
              HYC ~ 0.5 | (Y + Y1C) + 0.125 | H | (DY - DY1C)  ;        CDIM0031
              CO ~ ABS((Y1C - Y1)/Y1)  ;                                CDIM0032
              IF BOOL THEN GO TO L4  ;                                  CDIM0033
              IF CO > EMAX THEN                                         CDIM0034
         BEGIN                                                          CDIM0035
              BOOLH ~ TRUE  ;                                           CDIM0036
              H ~ 0.5 | H  ;  GO TO L2                                  CDIM0037
         END  ;                                                         CDIM0038
              IF BOOLH THEN GO TO L4  ;                                 CDIM0039
              IF CO < EMIN THEN                                         CDIM0040
         BEGIN                                                          CDIM0041
              IF (X + H | 2.0) > XMAX THEN GO TO L4  ;                  CDIM0042
              H ~ H + H  ;  GO TO L2                                    CDIM0043
         END  ;                                                         CDIM0044
              BOOL ~ TRUE  ;                                            CDIM0045
                                                                        CDIM0046
L4:           IF CO > E THEN                                            CDIM0047
         BEGIN                                                          CDIM0048
              HY ~ HYC  ;  Y1 ~ Y1C  ;  DY1 ~ DY1C  ;                   CDIM0049
              GO TO L3                                                  CDIM0050
         END  ;                                                         CDIM0051
              X ~ X + H  ;                                              CDIM0052
              Y ~ Y1C  ;  DY ~ DY1C  ;                                  CDIM0053
              N ~ N + 1  ;                                              CDIM0054
              IF N < NPRINT THEN GO TO L1A  ;                           CDIM0055
              N ~ 0  ;  GO TO L1  ;                                     CDIM0056
                                                                        CDIM0057
RET:     END  ;                                                         CDIM0058
                                                                        TEST0049
              READ(READER,DATA,INPUT)  ;                                TEST0050
              CLIPDIM(X0,Y0,H,XMAX,EMAX,EMIN,E,NPRINT,F,PRINT)  ;       TEST0051
                                                                        TEST0052
         END.                                                           TEST0053
