 LABEL  0000000000XXXXXX0010000001
$ CARD LIST
COMMENT       MILNES METHOD WITH CORRECTOR STABILIZATION FOR            TEST0001
              A SYSTEM OF FIRST ORDER ORDINARY DIFFERENTIAL EQUATIONS   TEST0002
                                                                        TEST0003
              A. F. PIXLEY AND ANNE G. MACEK                            TEST0004
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION)            TEST0005
                                                                        TEST0006
              CARD SEQUENCE CODE STARTS WITH MLNS-0001                  TEST0007
              FIRST ISSUE 01/17/64                                     ;TEST0008
                                                                        TEST0009
         BEGIN                                                          TEST0010
                                                                        TEST0011
INTEGER       NF,NS,NP,K,N,I,LINES  ;                                   TEST0012
BOOLEAN       B  ;                                                      TEST0013
REAL          X,H,PEC,PES,E  ;                                          TEST0014
ARRAY         Y[0:4,0:10]  ;                                            TEST0015
                                                                        TEST0016
LIST          INPUT(X,H,NF,NS,NP,B,PEC,PES,E,K),                        TEST0017
              YRK(FOR I ~ 1 STEP 1 UNTIL 4 DO Y[N,I]),                  TEST0018
              TITLE(K,B,E,H,NF,NS,PES,PEC)  ;                           TEST0019
                                                                        TEST0020
FORMAT IN     FORMIN(2E10.3,I5,2I3,L6,3E10.3,I3),                       TEST0021
                                                                        TEST0023
              FRK(4E20.10)  ;                                           TEST0022
FORMAT OUT    FORMT("MILNES METHOD"/"NO. EQUATIONS =",I3/"CORRECTOR ",  TEST0024
                   "ITERATION INDICATOR =",L6," WITH MAX. DIFF. =",     TEST0025
                   ", WITH STABILIZATION EVERY ",I4," STEPS"/           TEST0027
                   "PERMISSIBLE DIFFERENCE BETWEEN CORRECTOR AND ",     TEST0028
                   "STABILIZER =",F14.8/"PERMISSIBLE DIFFERENCE ",      TEST0029
                   F14.8,"."/"STEP SIZE = ",F14.8/"NO. OF STEPS =",I5,  TEST0026
                   "BETWEEN PREDICTOR AND CORRECTOR =",F14.8)  ;        TEST0030
                                                                        TEST0031
FILE IN       READER(1,10)  ;                                           TEST0032
FILE OUT      PRINTER(1,15)  ;                                          TEST0033
                                                                        TEST0034
COMMENT       SOLUTION OF THE DIFFERENTIAL EQUATIONS                    FNCT0001
                   DY1 = 2Y1 -  Y2 -  Y3 -  Y4                          FNCT0002
                   DY2 = 3Y1 - 2Y2 -  Y3 -  Y4                          FNCT0003
                   DY3 = 2Y1       - 2Y3 -  Y4                          FNCT0004
                   DY4 = 2Y1 -  Y2       - 2Y4                          FNCT0005
              WITH INITIAL CONDITIONS X = 0, Y1 = Y3 = 1, Y2 = Y4 = 0  ;FNCT0006
                                                                        FNCT0007
PROCEDURE     F(X,Y,DY)  ;                                              FNCT0008
                                                                        FNCT0009
VALUE         X  ;                                                      FNCT0010
REAL          X  ;                                                      FNCT0011
ARRAY         Y,DY[0]  ;                                                FNCT0012
                                                                        FNCT0013
         BEGIN                                                          FNCT0014
              DY[1] ~ Y[1] | 2.0 - Y[2] - Y[3] - Y[4]  ;                FNCT0015
              DY[2] ~ Y[1] | 3.0 - Y[2] | 2.0 - Y[3] - Y[4]  ;          FNCT0016
              DY[3] ~ Y[1] | 2.0 - Y[3] | 2.0 - Y[4]  ;                 FNCT0017
              DY[4] ~ Y[1] | 2.0 - Y[2] - Y[4] | 2.0                    FNCT0018
         END  ;                                                         FNCT0019
                                                                        FNCT0020
PROCEDURE     PRINT(K,N,X,Y,P,UNSY,DY,J,BE)  ;                          PRNT0001
                                                                        PRNT0002
VALUE         K,N,X,J,BE  ;                                             PRNT0003
INTEGER       K,N,J  ;                                                  PRNT0004
BOOLEAN       BE  ;                                                     PRNT0005
REAL          X  ;                                                      PRNT0006
ARRAY         Y,P,UNSY,DY[0]  ;                                         PRNT0007
                                                                        PRNT0008
         BEGIN                                                          PRNT0009
INTEGER       I  ;                                                      PRNT0010
                                                                        PRNT0011
LIST          HEAD(N,X,J),                                              PRNT0012
              RESULTS(I,Y[I],P[I]-Y[I],DY[I],Y[I]-UNSY[I])  ;           PRNT0013
                                                                        PRNT0014
FORMAT OUT    FORMH("N =",I4,"  X =",F14.8,"  NO. OF CORRECTOR ITER. =",PRNT0015
                   I3),                                                 PRNT0016
              FORMR("I =",I3,"  YI = ",F14.8,"  PI-YI =",F14.8,"  DY =",PRNT0017
                   F14.8,"  YIS-YI =",F14.8),                           PRNT0018
              FORME(X90,"ERROR")  ;                                     PRNT0019
                                                                        PRNT0020
              IF LINES = 7 THEN                                         PRNT0021
         BEGIN                                                          PRNT0022
              WRITE(PRINTER[PAGE])  ;  LINES ~ 0                        PRNT0023
         END  ;                                                         PRNT0024
              WRITE(PRINTER[DBL],FORMH,HEAD)  ;                         PRNT0025
              FOR I ~ 1 STEP 1 UNTIL K DO WRITE(PRINTER,FORMR,RESULTS) ;PRNT0026
              IF BE THEN WRITE(PRINTER,FORME)  ;                        PRNT0027
              WRITE(PRINTER[DBL])  ;   LINES ~ LINES + 1                PRNT0028
         END  ;                                                         PRNT0029
                                                                        PRNT0030
PROCEDURE     MILNES(K,NF,H,E,PEC,PES,NS,NP,X,Y,B,F,PRINT)  ;           MLNS0001
                                                                        MLNS0002
VALUE         K,NF,H,E,PEC,PES,NS,NP  ;                                 MLNS0003
INTEGER       K,NS,NP,NF  ;                                             MLNS0004
BOOLEAN       B  ;                                                      MLNS0005
REAL          X,H,E,PEC,PES  ;                                          MLNS0006
ARRAY         Y[0,0]  ;                                                 MLNS0007
PROCEDURE     F,PRINT  ;                                                MLNS0008
                                                                        MLNS0009
         BEGIN                                                          MLNS0010
INTEGER       N,J,I,J1  ;                                               MLNS0011
BOOLEAN       BE,B1,BI  ;                                               MLNS0012
REAL          X0  ;                                                     MLNS0013
ARRAY         DY[0:5,0:K],P[0:K],YS[0:K]  ;                             MLNS0014
LABEL         STPP,CORR,R1,R2,R3,RETURN  ;                              MLNS0015
                                                                        MLNS0016
              X0 ~ X  ;                                                 MLNS0017
              FOR N ~ 0 STEP 1 UNTIL 3 DO                               MLNS0018
         BEGIN                                                          MLNS0019
              X ~ N | H + X0  ;                                         MLNS0020
              F(X,Y[N,*],DY[N,*])  ;                                    MLNS0021
              PRINT(K,N,X,Y[N,*],Y[N,*],Y[N,*],DY[N,*],0,FALSE)         MLNS0022
         END  ;                                                         MLNS0023
              N ~ 3  ;                                                  MLNS0024
                                                                        MLNS0025
STPP:         N ~ N + 1  ;                                              MLNS0026
              BE ~ FALSE  ;                                             MLNS0027
              X ~ X + H  ;                                              MLNS0028
              J ~ 0  ;                                                  MLNS0029
              FOR I ~ 1 STEP 1 UNTIL K DO                               MLNS0030
              P[I] ~ (DY[1,I] | 2.0 - DY[2,I] + DY[3,I] | 2.0) | H      MLNS0031
                   | 4.0/3.0 + Y[0,I]  ;                                MLNS0032
              F(X,P,DY[4,*])  ;                                         MLNS0033
              B1 ~ B  ;  BI ~ FALSE  ;                                  MLNS0034
                                                                        MLNS0035
CORR:         FOR I ~ 1 STEP 1 UNTIL K DO                               MLNS0036
              Y[4,I] ~ (DY[4,I] + DY[3,I] | 4.0 + DY[2,I]) | H/3.0      MLNS0037
                   + Y[2,I]  ;                                          MLNS0038
              F(X,Y[4,*],DY[4,*])  ;                                    MLNS0039
                                                                        MLNS0040
              IF BI THEN                                                MLNS0041
              FOR I ~ 1 STEP 1 UNTIL K DO                               MLNS0042
              IF ABS(YS[I] - Y[4,I]) > E THEN                           MLNS0043
         BEGIN                                                          MLNS0044
              B1 ~ TRUE  ;  GO TO R1                                    MLNS0045
         END  ;                                                         MLNS0046
                                                                        MLNS0047
R1:           IF B1 THEN                                                MLNS0048
         BEGIN                                                          MLNS0049
              B1 ~ FALSE  ;  BI ~ TRUE  ;                               MLNS0050
              FOR I ~ 1 STEP 1 UNTIL K DO YS[I] ~ Y[4,I]  ;             MLNS0051
              J ~ J + 1  ;                                              MLNS0052
              GO TO CORR                                                MLNS0053
         END  ;                                                         MLNS0054
                                                                        MLNS0055
              FOR I ~ 1 STEP 1 UNTIL K DO                               MLNS0056
              IF ABS(P[I] - Y[4,I]) > PEC THEN                          MLNS0057
         BEGIN                                                          MLNS0058
              BE ~ TRUE  ;  GO TO R2                                    MLNS0059
         END  ;                                                         MLNS0060
                                                                        MLNS0061
R2:           IF (N MOD NS) = 0 THEN                                    MLNS0062
         BEGIN                                                          MLNS0063
              FOR I ~ 1 STEP 1 UNTIL K DO                               MLNS0064
              YS[I] ~ ((DY[4,I] + DY[3,I] | 3.0 + DY[2,I] | 3.0 +       MLNS0065
                   DY[1,I]) | H | 3.0/8.0 + Y[4,I] + Y[1,I]) | 0.5   ;  MLNS0066
              F(X,YS,DY[4,*])  ;                                        MLNS0067
              FOR I ~ 1 STEP 1 UNTIL K DO                               MLNS0068
              IF ABS(YS[I] - Y[4,I]) > PES THEN                         MLNS0069
         BEGIN                                                          MLNS0070
              BE ~ TRUE  ;  GO TO R3                                    MLNS0071
         END                                                            MLNS0072
         END  ;                                                         MLNS0073
                                                                        MLNS0074
R3:           IF ((N MOD NP = 0) OR BE) THEN                            MLNS0075
              IF (N MOD NS) = 0 THEN                                    MLNS0076
         BEGIN                                                          MLNS0077
              PRINT(K,N,X,YS,P,Y[4,*],DY[4,*],J,BE)  ;                  MLNS0078
              FOR I ~ 1 STEP 1 UNTIL K DO Y[4,I] ~ YS[I]                MLNS0079
         END                                                            MLNS0080
              ELSE PRINT(K,N,X,Y[4,*],P,Y[4,*],DY[4,*],J,BE)  ;         MLNS0081
              IF N = NF THEN GO TO RETURN  ;                            MLNS0082
              FOR J1 ~ 1 STEP 1 UNTIL 4 DO                              MLNS0083
              FOR I ~ 1 STEP 1 UNTIL K DO                               MLNS0084
         BEGIN                                                          MLNS0085
              Y[J1-1,I] ~ Y[J1,I]  ;                                    MLNS0086
              DY[J1-1,I] ~ DY[J1,I]                                     MLNS0087
         END  ;                                                         MLNS0088
              GO TO STPP  ;                                             MLNS0089
                                                                        MLNS0090
RETURN:  END  ;                                                         MLNS0091
                                                                        TEST0035
              READ(READER,FORMIN,INPUT)  ;                              TEST0036
              FOR N ~ 0 STEP 1 UNTIL 3 DO                               TEST0037
              READ(READER,FRK,YRK)  ;                                   TEST0038
              WRITE(PRINTER[DBL],FORMT,TITLE)  ;  WRITE(PRINTER[DBL])  ;TEST0039
              LINES ~ 2  ;                                              TEST0040
              MILNES(K,NF,H,E,PEC,PES,NS,NP,X,Y,B,F,PRINT)  ;           TEST0041
                                                                        TEST0042
         END.                                                           TEST0043

 LABEL  000000000READER 0010000001
+0.000@+00+0.100@-01 1500 50 20  TRUE+0.100@-02+0.100@-03+0.100@-05  4
   +1.0000000000@+00   +0.0000000000@+00   +1.0000000000@+00    0.0000000000#+00
   +1.0099004979@+00   +1.9850664175@-02   +9.9999999958@-01    1.9801161683#-02
   +1.0196039669@+00   +3.9405293599@-02   +9.9999999345@-01    3.9209253864#-02
   +1.0291133331@+00   +5.8667799507@-02   +9.9999996703@-01    5.8231099017#-02

+0.000@+00+0.500@+00  100 20 10 FALSE+0.100@-02+0.100@-03+0.100@-05  4
   +1.0000000000@+00   +0.0000000000@+00   +1.0000000000@+00   +0.0000000000@+00
   +1.3015137373@+00   +6.9498304758@-01   +9.9824837743@-01   +6.1916671512@-01
   +1.3488912843@+00   +9.8101184313@-01   +981011843133@-01   +7.9707212254@-01
   +1.2690527858@+00   +1.0459226257@+00   +9.3435754561@-01   +7.9490119552@-01

