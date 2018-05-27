         BEGIN                                                          CMNT0001
COMMENT       THIS PROCEDURE PERFORMS A TWO DIMENSIONAL INTERPOLATION.  CMNT0002
              THE VECTOR "X" OF LENGTH "M",AND THE VECTOR "Y" OF LENGTH CMNT0003
              "N" CONTAIN THE INDEPENDENT VARIABLES.  THE M|N ARRAY     CMNT0004
              "FXY" CONTAINS THE DEPENDENT VARIABLE.  "XBAR" AND "YBAR" CMNT0005
              ARE THE VALUES OVER WHICH THE INTERPOLATION TAKES PLACE.  CMNT0006
              "EPS" IS THE SAME AS IN THE UNIVARIATE INTERPOLATION.     CMNT0007

              R.D. RODMAN,                                              CMNT0008
              PROFESSIONAL SERVICES DIVISIONAL GROUP,                   CMNT0009

              CARD SEQUENCE BEGINS WITH BVAR0001,                       CMNT0010
              FIRST RELEASE 8/63  ;                                     CMNT0011

REAL PROCEDURE BIVARIATE(M, N, X, Y, FXY, XBAR,YBAR, EPS) ;             BVAR0001
VALUE         M, N, XBAR, YBAR, EPS ;                                   BVAR0002
INTEGER       M, N ;                                                    BVAR0003
REAL          XBAR, YBAR, EPS ;                                         BVAR0004
REAL ARRAY    X, Y[0], FXY[0,0] ;                                       BVAR0005

         BEGIN                                                          BVAR0006

INTEGER       I ;                                                       BVAR0007
REAL ARRAY    FTEMP[0:M] ;                                              BVAR0008
REAL PROCEDURE INTERPOLATE(N, X, Y, XBAR, EPS) ;                        BVAR0009
VALUE         N, XBAR, EPS ;                                            BVAR0010
INTEGER       N ;                                                       BVAR0011
REAL          XBAR, EPS ;                                               BVAR0012
REAL ARRAY    X, Y[0] ;                                                 BVAR0013

         BEGIN                                                          BVAR0014

LABEL         EXIT ;                                                    BVAR0015
INTEGER       I, J, K, I1 ;                                             BVAR0016
REAL          T1, T2 ;                                                  BVAR0017
REAL ARRAY    A[0:N,0:N], B[0:N] ;                                      BVAR0018

              FOR I ~ 1 STEP 1 UNTIL N DO                               BVAR0019
         BEGIN                                                          BVAR0020
              B[I] ~ X[I] - XBAR ;                                      BVAR0021
              A[I,1] ~ Y[I]                                             BVAR0022
         END ;                                                          BVAR0023
              FOR I ~ 1 STEP 1 UNTIL N DO FOR J ~ I+1 STEP 1 UNTIL N DO BVAR0024
         BEGIN                                                          BVAR0025
              IF ABS(B[I]) > ABS(B[J]) THEN                             BVAR0026
         BEGIN                                                          BVAR0027
              T1 ~ B[I] ;  B[I] ~ B[J] ;  B[J] ~ T1 ;                   BVAR0028
              T1 ~ A[I,1] ;  A[I,1] ~ A[J,1] ;  A[J,1] ~ T1             BVAR0029
         END                                                            BVAR0030
         END ;                                                          BVAR0031
              FOR I ~ 2 STEP 1 UNTIL N DO                               BVAR0032
         BEGIN                                                          BVAR0033
              I1 ~ I-1 ;                 T2 ~ A[I1,I1] ;                BVAR0034
              FOR J ~ I STEP 1 UNTIL N DO                               BVAR0035
              A[I,J] ~ (T2 | B[J] - A[J,I1] | B[I1]) / (B[J] - B[I1]) ; BVAR0036
              T1 ~ A[I,I] ;                                             BVAR0037
              IF ABS((T1 - T2) / T1) { EPS THEN                         BVAR0038
         BEGIN                                                          BVAR0039
              INTERPOLATE ~ T1 ;  GO TO EXIT                            BVAR0040
         END                                                            BVAR0041
         END ;                                                          BVAR0042
              INTERPOLATE ~ T1 ;                                        BVAR0043

EXIT:    END ;                                                          BVAR0044

              FOR I ~ 1 STEP 1 UNTIL M DO                               BVAR0045
              FTEMP[I] ~   INTERPOLATE(N, X, FXY[I,*], XBAR, EPS) ;     BVAR0046
              BIVARIATE ~ INTERPOLATE(M, Y, FTEMP, YBAR, EPS) ;         BVAR0047
         END ;                                                          BVAR0048

         END.                                                           CMNT0012
