         BEGIN                                                          CMNT0001
COMMENT       THIS PROCEDURE WILL PERFORM A ONE DIMENSIONAL             CMNT0002
              INTERPOLATION.  "X" IS A VECTOR CONTAINING THE            CMNT0003
              INDEPENDENT VARIABLE AND "Y" A VECTOR CONTAINING          CMNT0004
              CORRESPONDING VALUES OF THE DEPENDENT VARIABLE.  "N" IS   CMNT0005
              THE NUMBER OF POINTS, AND "XBAR" IS THE VALUE FOR WHICH   CMNT0006
              THE INTERPOLATION TAKES PLACE.   EPS IS A TOLERANCE       CMNT0007
              PARAMETER.  ITS USE IS EXPLAINED IN DETAIL IN THE         CMNT0008
              CORRESPONDING TECHNICAL BULLETIN.                         CMNT0009

              R.D. RODMAN,                                              CMNT0010
              PROFESSIONAL SERVICES DIVISIONAL GROUP,                   CMNT0011

              CARD SEQUENCE BEGINS WITH UVAR0001                        CMNT0012
              FIRST RELEASE 8/63  ;                                     CMNT0013

REAL PROCEDURE INTERPOLATE(N, X, Y, XBAR, EPS) ;                        UVAR0001
VALUE         N, XBAR, EPS ;                                            UVAR0002
INTEGER       N ;                                                       UVAR0003
REAL          XBAR, EPS ;                                               UVAR0004
REAL ARRAY    X, Y[0] ;                                                 UVAR0005

         BEGIN                                                          UVAR0006

LABEL         EXIT ;                                                    UVAR0007
INTEGER       I, J, K, I1 ;                                             UVAR0008
REAL          T1, T2 ;                                                  UVAR0009
REAL ARRAY    A[0:N,0:N], B[0:N] ;                                      UVAR0010

              FOR I ~ 1 STEP 1 UNTIL N DO                               UVAR0011
         BEGIN                                                          UVAR0012
              B[I] ~ X[I] - XBAR ;                                      UVAR0013
              A[I,1] ~ Y[I]                                             UVAR0014
         END ;                                                          UVAR0015
              FOR I ~ 1 STEP 1 UNTIL N DO FOR J ~ I+1 STEP 1 UNTIL N DO UVAR0016
         BEGIN                                                          UVAR0017
              IF ABS(B[I]) > ABS(B[J]) THEN                             UVAR0018
         BEGIN                                                          UVAR0019
              T1 ~ B[I] ;  B[I] ~ B[J] ;  B[J] ~ T1 ;                   UVAR0020
              T1 ~ A[I,1] ;  A[I,1] ~ A[J,1] ;  A[J,1] ~ T1             UVAR0021
         END                                                            UVAR0022
         END ;                                                          UVAR0023
              FOR I ~ 2 STEP 1 UNTIL N DO                               UVAR0024
         BEGIN                                                          UVAR0025
              I1 ~ I-1 ;                 T2 ~ A[I1,I1] ;                UVAR0026
              FOR J ~ I STEP 1 UNTIL N DO                               UVAR0027
              A[I,J] ~ (T2 | B[J] - A[J,I1] | B[I1]) / (B[J] - B[I1]) ; UVAR0028
              T1 ~ A[I,I] ;                                             UVAR0029
              IF ABS((T1 - T2) / T1) { EPS THEN                         UVAR0030
         BEGIN                                                          UVAR0031
              INTERPOLATE ~ T1 ;  GO TO EXIT                            UVAR0032
         END                                                            UVAR0033
         END ;                                                          UVAR0034
              INTERPOLATE ~ T1 ;                                        UVAR0035

EXIT:    END ;                                                          UVAR0036

         END .                                                          CMNT0014
