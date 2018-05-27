         BEGIN                                                          CMNT0001
COMMENT       THIS PROCEDURE DETERMINES ALL THE ZEROS OF A              CMNT0002
              POLYNOMIAL P(X) PROVIDED THE ZEROS ARE REAL               CMNT0003
              AND UNEQUAL.                                              CMNT0004
                                                                        CMNT0005
              JOANNE K. KONDO                                           CMNT0006
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION).           CMNT0007
                                                                        CMNT0008
              PROCEDURE CARD SEQUENCE CODE STARTS WITH NWTN0001.        CMNT0009
              FIRST RELEASE 07/01/1963  ;                               CMNT0010
                                                                        CMNT0011
                                                                        CMNT0012
PROCEDURE     NEWTONMAEHLY (A, N, EPS, Z)  ;                            NWTN0001
VALUE         N, EPS  ;                                                 NWTN0002
INTEGER       N  ;                                                      NWTN0003
REAL          EPS  ;                                                    NWTN0004
ARRAY         A, Z[0]  ;                                                NWTN0005
                                                                        NWTN0006
         BEGIN                                                          NWTN0007
                                                                        NWTN0008
REAL          AA, PP, QQ, X0, X1  ;                                     NWTN0009
INTEGER       I, M, S  ;                                                NWTN0010
LABEL         ITERATION;                                                NWTN0011
ARRAY         B, Q[0:N-1], P[0:N]  ;                                    NWTN0012
                                                                        NWTN0013
PROCEDURE     HORNER(P,Q,N,X,PP,QQ)  ;                                  NWTN0014
VALUE         N, X  ;                                                   NWTN0015
ARRAY         P, Q[0]  ;                                                NWTN0016
REAL          PP, X, QQ  ;                                              NWTN0017
INTEGER       N  ;                                                      NWTN0018
                                                                        NWTN0019
         BEGIN                                                          NWTN0020
                                                                        NWTN0021
REAL          S, S1  ;                                                  NWTN0022
INTEGER       I  ;                                                      NWTN0023
                                                                        NWTN0024
              S ~ S1 ~ 0  ;                                             NWTN0025
              FOR I ~ 0 STEP 1 UNTIL N-1 DO                             NWTN0026
         BEGIN                                                          NWTN0027
              S ~ S | X + P[I]  ;                                       NWTN0028
              S1 ~ S1 | X + Q[I]                                        NWTN0029
         END  ;                                                         NWTN0030
              PP ~ S | X + P[N]  ;  QQ ~ S1  ;                          NWTN0031
         END  ;                                                         NWTN0032
                                                                        NWTN0033
COMMENT       THE FIRST APPROXIMATION OF THE LARGEST ZERO IS DETERMINED;NWTN0034
                                                                        NWTN0035
              P[0] ~ AA ~ A[0]  ;  X0 ~ PP ~ 0  ;                       NWTN0036
              S ~ SIGN(A[0])  ;                                         NWTN0037
              FOR I ~  1 STEP 1 UNTIL N DO                              NWTN0038
              IF S | A[I] < 0 THEN                                      NWTN0039
         BEGIN                                                          NWTN0040
              IF PP = 0 THEN PP ~ I  ;                                  NWTN0041
              IF X0  < ABS(A[I]) THEN X0 ~ ABS(A[I])                    NWTN0042
         END  ;                                                         NWTN0043
              X0 ~ IF PP = 0 THEN 0 ELSE 1 + (X0/AA)*(1/PP)  ;          NWTN0044
              FOR I ~ 0 STEP 1 UNTIL N-1 DO                             NWTN0045
              B[I] ~ (N-I) | A[I]  ;                                    NWTN0046
              FOR M ~ 1 STEP 1 UNTIL N DO                               NWTN0047
         BEGIN                                                          NWTN0048
                                                                        NWTN0049
COMMENT       THE APPROXIMATION IS IMPROVED UNTIL THE ACCEPTANCE        NWTN0050
              CRITERION IS MET  ;                                       NWTN0051
                                                                        NWTN0052
ITERATION:    HORNER(A,B,N,X0,PP,QQ)  ;                                 NWTN0053
              X1 ~ X0 - PP/QQ  ;                                        NWTN0054
              IF ABS(X1-X0) > EPS | ABS(X1) THEN                        NWTN0055
         BEGIN                                                          NWTN0056
              X0 ~ X1  ;  GO TO ITERATION                               NWTN0057
         END  ;                                                         NWTN0058
                                                                        NWTN0059
COMMENT       Z[M] = X1 IS THE M-TH ZERO OF THE POLYNOMIAL  ;           NWTN0060
                                                                        NWTN0061
              Z[M] ~ X1  ;  Q[0] ~ PP ~ B[0] ~ B[0] - AA  ;             NWTN0062
              IF M < N THEN                                             NWTN0063
         BEGIN                                                          NWTN0064
              FOR I ~ 1 STEP 1 UNTIL N-1 DO                             NWTN0065
         BEGIN                                                          NWTN0066
              PP ~ P[I] ~ X1 | P[I-1] + A[I]  ;                         NWTN0067
              PP ~ B[I] ~ B[I] - PP  ;                                  NWTN0068
              Q[I] ~ X1 | Q[I-1] + PP                                   NWTN0069
         END  ;                                                         NWTN0070
              HORNER(P,Q,N-1,X1,PP,QQ)  ;                               NWTN0071
              X0 ~ X1 - PP/QQ                                           NWTN0072
                                                                        NWTN0073
COMMENT       XO IS THE FIRST APPROXIMATION FOR THE NEXT ZERO  ;        NWTN0074
                                                                        NWTN0075
         END                                                            NWTN0076
         END                                                            NWTN0077
         END      ;                                                     NWTN0078
         END  .                                                         CMNT0013

