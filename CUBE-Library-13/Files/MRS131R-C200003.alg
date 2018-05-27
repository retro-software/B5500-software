         BEGIN
INTEGER       I, N ;
REAL          E, F, INCRE, G ;
ARRAY         A, C, D, ER, EI[0:50] ;
FILE          CARD(1,10), PRINTER 1 (1,15) ;
FORMAT        F1(I4,(12F6.3)), F2(X51,"ROOTS OF THE POLYNOMIAL"//X46,
              "REAL", X12, "IMAGINARY"/(X37, E18.11, X4, E18.11)),
              F4(///X51,"ERROR OF THE POLYNOMIAL"//X46,"REAL", X12,
              "IMAGINARY"/(X37, E18.11, X4, E18.11)) ;
PROCEDURE RPOLY(E, F, INCRE, G, N, A, C, D, ER, EI) ;
COMMENT       PROCEDURE RPOLY FINDS THE ROOTS OF A POLYNOMIAL, THE
              COEFFICIENTS OF WHICH ARE REAL NUMBERS.
              CARD SEQUENCE BEGINS WITH RPLY0000
              FIRST RELEASE DATE 07/01/64 ;
VALUE         E, F, INCRE, G, N ;
INTEGER       N ;
REAL          E, F, INCRE, G ;
REAL ARRAY    A, C, D, ER, EI[0] ;
         BEGIN
INTEGER       I, N1, RCOUNT, K, L ;
REAL          H, U, V, W, R, S, PR, PS, UMIN, VMIN, T, T1, T2, T3, P,Q ;
LABEL         S9, START, EXIT, ROOT ;
ARRAY         B[0:N] ;
PROCEDURE     ORPOL(N, A, X, Y, T, IT) ;
VALUE         N, X, Y ;
INTEGER       N ;
REAL          X, Y, T, IT ;
REAL ARRAY    A[0] ;
         BEGIN
REAL          P ;
INTEGER       I ;
              T ~ A[N] ; IT ~ 0 ;
              FOR I ~ N-1 STEP -1 UNTIL 0 DO
         BEGIN
              P ~ T | X - IT | Y + A[I] ;
              IT ~ IT | X + T | Y ;
              T ~ P
         END
         END ;
              FOR I ~ 0 STEP 1 UNTIL N DO B[I] ~ A[I] ; N1 ~ N ;
              RCOUNT ~ 1 ;
START:        H ~ INCRE ;  U ~ E ;  V ~ F ;
S9:           W ~ 9.9@60 ;
              R ~ U-H ;
              FOR K ~ 1 STEP 1 UNTIL 3 DO
         BEGIN
              S ~ V-H ;
              FOR L ~ 1 STEP 1 UNTIL 3 DO
         BEGIN
              ORPOL(N1, B, R, S, PR, PS) ;
              IF (ABS(PR) + ABS(PS)) < W THEN
         BEGIN
              UMIN ~ R ;  VMIN ~ S ;  W ~ ABS(PR) + ABS(PS)
         END ;
              S ~ S+H
         END ;
              R ~ R+H
         END ;
              IF ABS(U-UMIN) < (1.0@-10) | ABS(U) AND
              ABS(V-VMIN) < (1.0@-10) | ABS(V) THEN
         BEGIN
              IF SQRT(U*2 + V*2) | 1.0@-12 } H THEN GO TO ROOT ;
              H ~ H/G ;
              GO TO S9
         END ;
              U ~ UMIN ;  V ~ VMIN ;  GO TO S9 ;
ROOT:         IF ABS(V) < H|1000 THEN V ~ 0 ;
              C[RCOUNT] ~ U ;  D[RCOUNT] ~ V ;
              ORPOL(N, A,U, V,ER[RCOUNT],EI[RCOUNT]) ;
              RCOUNT ~ RCOUNT + 1 ;
              IF V ! 0 THEN
         BEGIN
              C[RCOUNT] ~ U ;  D[RCOUNT] ~ -V ;
              ORPOL(N, A,U,-V,ER[RCOUNT],EI[RCOUNT]) ;
              RCOUNT ~ RCOUNT + 1
         END ;
              IF RCOUNT > N THEN GO TO EXIT ;
              IF V = 0 THEN
         BEGIN
              T ~ B[N1-1] ;
              B[N1-1] ~ B[N1] ;
              FOR I ~ N1-2 STEP -1 UNTIL 0 DO
         BEGIN
              S ~ T + U | B[I+1] ;
              T ~ B[I] ;  B[I] ~ S
         END ;
              N1 ~ N1-1 ;
         END
              ELSE
         BEGIN
              P ~ -2 | U ;  Q ~ U+2 + V*2 ;
              T1 ~ B[N1-2] ;  T2 ~ B[N1-3] ;
              B[N1-2] ~ B[N1] ;
              B[N1-3] ~ B[N1-1] - P | B[N1-2] ;
              FOR I ~ N1-4 STEP -1 UNTIL 0 DO
         BEGIN
              T3 ~ B[I] ;
              B[I] ~ T1 - P | B[I+1] - Q | B[I+2] ;
              T1 ~ T2 ;  T2 ~ T3
         END ;
              N1 ~ N1-2
END;
              GO TO START ;
EXIT:    END ;
              READ(CARD,F1,N, FOR I ~0 STEP 1 UNTIL N DO A[I]) ;
              WRITE(PRINTER, F1, N, FOR I ~ 0 STEP 1 UNTIL N DO A[I]) ;
              E ~ F ~ 0.5 ; INCRE ~ 1 ; G ~ 5 ;
              RPOLY(E, F, INCRE, G, N, A, C, D, ER, EI) ;
              WRITE(PRINTER, F2, FOR I ~ 1 STEP 1 UNTIL N DO
                   [C[I], D[I]]) ;
              WRITE(PRINTER, F4, FOR I ~ 1 STEP 1 UNTIL N DO
                   [ER[I], EI[I]])
              END.
   3 2.    0.   -1.    1.
