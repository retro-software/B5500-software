 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
COMMENT       HAMMINGS MODIFIED MILNE METHOD WITH AUTOMATIC STEP SIZE   TEST0001
              ADJUSTMENT FOR SOLVING A SYSTEM OF FIRST ORDER ORDINARY   TEST0002
              DIFFERENTIAL EQUATIONS                                    TEST0003
                                                                        TEST0004
              A. F. PIXLEY AND ANNE G. MACEK                            TEST0005
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION)            TEST0006
                                                                        TEST0007
              PROCEDURE CARD SEQUENCE BEGINS WITH HAMS-0001             TEST0008
              FIRST RELEASE 08/01/62                                   ;TEST0009
                                                                        TEST0010
         BEGIN                                                          TEST0011
                                                                        TEST0012
INTEGER       K,NP,I,J  ;                                               TEST0013
REAL          XF,HMAX,HMIN,H,X  ;                                       TEST0014
ARRAY         CMAX[0:4],CMIN[0:4],Y[0:7,0:4]  ;                         TEST0015
                                                                        TEST0016
LIST          INPUT(K,NP,XF,HMAX,HMIN,H,X),                             TEST0017
              CM(FOR I ~ 1 STEP 1 UNTIL 4 DO [CMAX[I],CMIN[I]]),        TEST0018
              Y0(FOR I ~ 1 STEP 1 UNTIL 4 DO Y[J,I]),                   TEST0019
              TITLE(K,X,XF,H,HMIN,HMAX,FOR I ~ 1 STEP 1 UNTIL 4 DO      TEST0020
                   [I,CMIN[I],CMAX[I]])  ;                              TEST0021
                                                                        TEST0022
FORMAT IN     FINPUT(I3,I5,5E12.5),                                     TEST0023
              FCM(8E10.3),                                              TEST0024
              FY0(4E20.10)  ;                                           TEST0025
                                                                        TEST0026
FORMAT OUT    FORMT("HAMMING METHOD"//"NO. EQUATIONS =",I3,".  INITIAL",TEST0027
                   " X =",E15.8,", FINAL X =",E15.8,"."/"STEP SIZE- ",  TEST0028
                   "INITIAL =",E15.8,", MIN =",E15.8,", MAX =",E15.8,   TEST0029
                   "."//"PREDICTOR-CORRECTOR DIFFERENCES C -"/X20,"MIN",TEST0030
                   X15,"MAX"/4("I =",I3,2E18.8/))  ;                    TEST0031
                                                                        TEST0032
FILE IN       READER(1,10)  ;                                           TEST0033
FILE OUT      PRINTER(1,15)  ;                                          TEST0034
                                                                        TEST0035
PROCEDURE     F(X,Y,DY)  ;                                              FNCT0001
                                                                        FNCT0002
VALUE         X  ;                                                      FNCT0003
REAL          X  ;                                                      FNCT0004
ARRAY         Y[0],DY[0]  ;                                             FNCT0005
                                                                        FNCT0006
         BEGIN                                                          FNCT0007
              DY[1] ~ Y[1] | 2.0 - Y[2] - Y[3] - Y[4]  ;                FNCT0008
              DY[2] ~ Y[1] | 3.0 - Y[2] | 2.0 - Y[3] - Y[4]  ;          FNCT0009
              DY[3] ~ Y[1] | 2.0 - Y[3] | 2.0 - Y[4]  ;                 FNCT0010
              DY[4] ~ Y[1] | 2.0 - Y[2] - Y[4] | 2.0                    FNCT0011
         END  ;                                                         FNCT0012
                                                                        FNCT0013
PROCEDURE     PRINTR(K,N,X,Y,C,DY)  ;                                   PNTR0001
                                                                        PNTR0002
VALUE         K,N,X  ;                                                  PNTR0003
INTEGER       K,N  ;                                                    PNTR0004
REAL          X  ;                                                      PNTR0005
ARRAY         Y[0],C[0],DY[0]  ;                                        PNTR0006
                                                                        PNTR0007
         BEGIN                                                          PNTR0008
INTEGER       I  ;                                                      PNTR0009
                                                                        PNTR0010
LIST          HEAD(N,X),                                                PNTR0011
              RESULTS(I,Y[I],C[I],DY[I])  ;                             PNTR0012
                                                                        PNTR0013
FORMAT OUT    FORMH("N =",I4,X2,"X =",E15.8),                           PNTR0014
              FORMR("I =",I3,X2,"YI =",E15.8,X2,"CI =",E15.8,X2,"DYI =",PNTR0015
                   E15.8)  ;                                            PNTR0016
                                                                        PNTR0017
              WRITE(PRINTER[DBL],FORMH,HEAD)  ;                         PNTR0018
              FOR I ~ 1 STEP 1 UNTIL K DO WRITE(PRINTER,FORMR,RESULTS) ;PNTR0019
              WRITE(PRINTER[DBL])                                       PNTR0020
         END  ;                                                         PNTR0021
                                                                        PNTR0022
PROCEDURE     PRINTC(K,N,H,C)  ;                                        PNTC0001
                                                                        PNTC0002
VALUE         K,N,H  ;                                                  PNTC0003
INTEGER       K,N  ;                                                    PNTC0004
REAL          H  ;                                                      PNTC0005
ARRAY         C[0]  ;                                                   PNTC0006
                                                                        PNTC0007
         BEGIN                                                          PNTC0008
INTEGER       I  ;                                                      PNTC0009
                                                                        PNTC0010
LIST          OUT1(N,H),                                                PNTC0011
              OUT2(I,C[I])  ;                                           PNTC0012
                                                                        PNTC0013
FORMAT OUT    FORM1("N =",I4,X3,"H =",E15.8),                           PNTC0014
              FORM2("I =",I3,X3,"CI =",E15.8)  ;                        PNTC0015
                                                                        PNTC0016
              WRITE(PRINTER[DBL],FORM1,OUT1)  ;                         PNTC0017
              FOR I ~ 1 STEP 1 UNTIL K DO WRITE(PRINTER,FORM2,OUT2)  ;  PNTC0018
              WRITE(PRINTER[DBL])                                       PNTC0019
         END  ;                                                         PNTC0020
                                                                        PNTC0021
PROCEDURE     HAMMINGS(K,NP,XF,HMAX,HMIN,CMAX,CMIN,H,X,Y,F,PRINTR,      HAMS0001
                   PRINTC)  ;                                           HAMS0002
                                                                        HAMS0003
VALUE         K,NP,XF,HMAX,HMIN  ;                                      HAMS0004
REAL          X,H,XF,HMAX,HMIN  ;                                       HAMS0005
INTEGER       K,NP  ;                                                   HAMS0006
ARRAY         Y[0,0],CMAX[0],CMIN[0]  ;                                 HAMS0007
PROCEDURE     F,PRINTR,PRINTC  ;                                        HAMS0008
                                                                        HAMS0009
         BEGIN                                                          HAMS0010
INTEGER       I,N,L,J  ;                                                HAMS0011
REAL          X1  ;                                                     HAMS0012
ARRAY         DY[0:7,0:K],C[0:K],P[0:K],DC[0:K]  ;                      HAMS0013
LABEL         STPP,VAL,RESTORE,HALVE,RETURN  ;                          HAMS0014
                                                                        HAMS0015
              X1 ~ X  ;                                                 HAMS0016
              FOR I ~ 1 STEP 1 UNTIL K DO C[I] ~ 0.0  ;                 HAMS0017
              FOR N ~ 1 STEP 1 UNTIL 4 DO                               HAMS0018
         BEGIN                                                          HAMS0019
              X ~ H | (N-1) + X1  ;                                     HAMS0020
              F(X,Y[N,*],DY[N,*])  ;                                    HAMS0021
              PRINTR(K,N,X,Y[N,*],C,DY[N,*])                            HAMS0022
         END  ;                                                         HAMS0023
              N ~ 4  ;  L ~ 1  ;  GO TO STPP ;                          HAMS0024
                                                                        HAMS0025
RESTORE:      FOR J ~ 1 STEP 1 UNTIL 4 DO FOR I ~ 1 STEP 1 UNTIL K DO   HAMS0026
         BEGIN                                                          HAMS0027
              Y[J,I] ~ Y[L+J,I]  ;                                      HAMS0028
              DY[J,I] ~ DY[L+J,I]                                       HAMS0029
         END  ;                                                         HAMS0030
              L ~ 1  ;                                                  HAMS0031
                                                                        HAMS0032
STPP:         N ~ N + 1  ;  X ~ X + H  ;                                HAMS0033
              FOR I ~ 1 STEP 1 UNTIL K DO                               HAMS0034
         BEGIN                                                          HAMS0035
              P[I] ~ (DY[L+1,I] | 2.0 - DY[L+2,I] + DY[L+3,I] | 2.0)    HAMS0036
                   | H | 4.0/3.0 + Y[L,I]  ;                            HAMS0037
              C[I] ~ P[I] - C[I] | 112.0/121.0                          HAMS0038
         END  ;                                                         HAMS0039
              F(X,C,DC)  ;                                              HAMS0040
              FOR I ~ 1 STEP 1 UNTIL K DO                               HAMS0041
         BEGIN                                                          HAMS0042
              Y[L+4,I] ~ ((DC[I] + DY[L+3,I] | 2.0 - DY[L+2,I]) | H |   HAMS0043
                   3.0 + Y[L+3,I] | 9.0 - Y[L+1,I])/8.0  ;              HAMS0044
              C[I] ~ P[I] - Y[L+4,I]                                    HAMS0045
         END  ;                                                         HAMS0046
              FOR I ~ 1 STEP 1 UNTIL K DO                               HAMS0047
              IF ABS(C[I]) > CMAX[I] THEN GO TO HALVE  ;                HAMS0048
                                                                        HAMS0049
VAL:          FOR I ~ 1 STEP 1 UNTIL K DO                               HAMS0050
              Y[L+4,I] ~ C[I] | 9.0/121.0 + Y[L+4,I]  ;                 HAMS0051
              F(X,Y[L+4,*],DY[L+4,*])  ;                                HAMS0052
              IF ((N-4) MOD NP) = 0 THEN                                HAMS0053
              PRINTR(K,N,X,Y[L+4,*],C,DY[L+4,*])  ;                     HAMS0054
              IF (XF-X) | H  < 0 THEN GO TO RETURN  ;                   HAMS0055
              IF ABS(H | 2.0) > ABS(HMAX) THEN GO TO RESTORE  ;         HAMS0056
              FOR I ~ 1 STEP 1 UNTIL K DO                               HAMS0057
              IF ABS(C[I]) } CMIN[I] THEN GO TO RESTORE  ;              HAMS0058
              IF L = 3 THEN                                             HAMS0059
         BEGIN                                                          HAMS0060
              FOR J ~ 1 STEP 1 UNTIL 4 DO FOR I ~ 1 STEP 1 UNTIL K DO   HAMS0061
         BEGIN                                                          HAMS0062
              Y[J,I] ~ Y[J|2-1,I]  ;                                    HAMS0063
              DY[J,I] ~ DY[J|2-1,I]                                     HAMS0064
         END  ;                                                         HAMS0065
              H ~ H | 2.0  ;                                            HAMS0066
              PRINTC(K,N,H,C) ;                                         HAMS0067
              FOR I ~ 1 STEP 1 UNTIL K DO C[I] ~ C[I] | 32.0  ;         HAMS0068
              L ~ 1   ;  GO TO STPP                                     HAMS0069
         END  ;                                                         HAMS0070
              L ~ L + 1  ;  GO TO STPP  ;                               HAMS0071
                                                                        HAMS0072
HALVE:        IF ABS(H|(0.5)) < ABS(HMIN) THEN GO TO VAL  ;             HAMS0073
              PRINTC(K,N,H,C)  ;                                        HAMS0074
              FOR I ~ 1 STEP 1 UNTIL K DO                               HAMS0075
         BEGIN                                                          HAMS0076
              P[I] ~ ((-DY[L+3,I] | 9.0 + DY[L+2,I] | 36.0 + DY[L+1,I] |HAMS0077
                   3.0) | H + Y[L+3,I] | 45.0 + Y[L+2,I] | 72.0 +       HAMS0078
                   Y[L+1,I] | 11.0)/128.0  ;                            HAMS0079
              C[I] ~ (-(DY[L+3,I] | 9.0 + DY[L+2,I] | 36.0 + DY[L+1,I] |HAMS0080
                   9.0) | H + Y[L+3,I] | 11.0 + Y[L+2,I] | 72.0 +       HAMS0081
                   Y[L+1,I] | 45.0)/128.0                               HAMS0082
         END  ;                                                         HAMS0083
              FOR I ~ 1 STEP 1 UNTIL K DO                               HAMS0084
         BEGIN                                                          HAMS0085
              Y[1,I] ~ C[I]  ;  C[I] ~ 0.0  ;                           HAMS0086
              Y[2,I] ~ Y[L+2,I]  ;  DY[2,I] ~ DY[L+2,I]  ;              HAMS0087
              Y[4,I] ~ Y[L+3,I]  ;  DY[4,I] ~ DY[L+3,I]                 HAMS0088
         END  ;                                                         HAMS0089
              F(X-H|(2.5),Y[1,*],DY[1,*])  ;                            HAMS0090
              F(X-H|(1.5),Y[3,*],DY[3,*])  ;                            HAMS0091
              X ~ X - H  ;  H ~ H/2  ;  N ~ N - 1  ;                    HAMS0092
              GO TO STPP  ;                                             HAMS0093
                                                                        HAMS0094
RETURN:  END  ;                                                         HAMS0095
                                                                        TEST0036
              READ(READER,FINPUT,INPUT)  ;                              TEST0037
              READ(READER,FCM,CM)  ;                                    TEST0038
              FOR J ~ 1 STEP 1 UNTIL 4 DO READ(READER,FY0,Y0)  ;        TEST0039
              WRITE(PRINTER,FORMT,TITLE)  ;                             TEST0040
              HAMMINGS(K,NP,XF,HMAX,HMIN,CMAX,CMIN,H,X,Y,F,PRINTR,      TEST0041
                   PRINTC)  ;                                           TEST0042
                                                                        TEST0043
         END.                                                           TEST0044


 LABEL  000000000READER 0010000001
  4   20+1.50000@+01+0.10000@+00+0.10000@-02+0.10000@-01+0.00000@+00
+0.100@-05+0.100@-07+0.100@-05+0.100@-07+0.100@-05+0.100@-07+0.100@-05+0.100@-07
   +1.0000000000@+00   +0.0000000000@+00   +1.0000000000@+00   +0.0000000000@+00
   +1.0099004979@+00   +1.9850664175@-02   +9.9999999958@-01   +1.9801161683@-02
   +1.0196039669@+00   +3.9405293599@-02   +9.9999999345@-01   +3.9209253864@-02
   +1.0291133331@+00   +5.8667799507@-02   +9.9999996703@-01   +5.8231099017@-02
