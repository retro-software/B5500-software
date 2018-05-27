         BEGIN
INTEGER       I, N ;
REAL          E, F, INCRE, G ;
ARRAY         A, C, D, ER, EI[0:50] ;
ARRAY         Z[0:50] ;
FILE          CARD(1,10), PRINTER 1 (1,15) ;
FORMAT        F1(I4,(12F6.3)), F2(X51,"ROOTS OF THE POLYNOMIAL"//X46,
              "REAL", X12, "IMAGINARY"/(X37, E18.11, X4, E18.11)),
              F4(///X51,"ERROR OF THE POLYNOMIAL"//X46,"REAL", X12,
              "IMAGINARY"/(X37, E18.11, X4, E18.11)) ;
PROCEDURE CPOLY(E, F, INCRE, G, N, A, Z, C, D, ER, EI) ;
COMMENT       PROCEDURE CPOLY DETERMINES THE ROOTS OF A POLYNOMIAL,
              THE COEFFICIENTS OF WHICH ARE COMPLEX NUMBERS.
              CARD SEQUENCE BEGINS WITH CPLY0000
              FIRST RELEASE DATE 07/01/64 ;
VALUE         E, F, INCRE, G, N ;
INTEGER       N ;
REAL          E, F, INCRE, G ;
REAL ARRAY    A, Z, C, D, ER, EI[0] ;
         BEGIN
INTEGER       I, N1, RCOUNT, K, L ;
REAL          H, U, V, W, R, S, PR, PS, UMIN, VMIN, T, T1, T2, T3, P,Q ;
REAL          IT, IS ;
LABEL         S9, START, EXIT, ROOT ;
ARRAY         B, BZ[0:N] ;
PROCEDURE ORPOL(N, A, Z, X, Y, T, IT) ;
VALUE         N, X, Y ;
INTEGER       N ;
REAL          X, Y, T, IT ;
REAL ARRAY    A, Z[0] ;
         BEGIN
REAL          P ;
INTEGER       I ;
              T ~ A[N] ; IT ~ Z[N] ;
              FOR I ~ N-1 STEP -1 UNTIL 0 DO
         BEGIN
              P ~ T | X - IT | Y + A[I] ;
              IT ~ IT | X + T | Y + Z[I] ;
              T ~ P
         END
         END ;
              FOR I ~ 0 STEP 1 UNTIL N DO
         BEGIN
              B[I] ~ A[I] ; BZ[I] ~ Z[I]
         END ;
              N1 ~ N ; RCOUNT ~ 1 ;
START:        H ~ INCRE ; U ~ E ; V ~ F ;
S9:           W ~ 9.9@60 ;
              R ~ U-H ;
              FOR K ~ 1 STEP 1 UNTIL 3 DO
         BEGIN
              S ~ V-H ;
              FOR L ~ 1 STEP 1 UNTIL 3 DO
         BEGIN
              ORPOL(N1, B, BZ, R, S, PR, PS) ;
              IF (ABS(PR) + ABS(PS)) < W THEN
         BEGIN
              UMIN ~ R ; VMIN ~ S ; W ~ ABS(PR) + ABS(PS)
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
              GO TO S9;
         END ;
              U ~ UMIN ; V ~ VMIN ; GO TO S9 ;
ROOT:         IF ABS(V) < H | 10 THEN V ~ 0 ;
              C[RCOUNT] ~ U ; D[RCOUNT] ~ V ;
              ORPOL(N, A, Z, U, V, ER[RCOUNT], EI[RCOUNT]) ;
              RCOUNT ~ RCOUNT + 1 ;
              IF RCOUNT > N THEN GO TO EXIT ;
              T ~ B[N1-1] ; IT ~ BZ[N1-1] ;
              B[N1-1] ~ B[N1] ; BZ[N1-1] ~ BZ[N1] ;
              FOR I ~ N1-2 STEP -1 UNTIL 0 DO
         BEGIN
              S ~ T + U | B[I+1] - V | BZ[I+1] ;
              IS ~ IT + V | B[I+1] + U | BZ[I+1] ;
              T ~ B[I] ; IT ~ BZ[I] ; B[I] ~ S ; BZ[I] ~ IS ;
         END ;
              N1 ~ N1-1 ;
              GO TO START ;
EXIT :   END ;
              READ(CARD,F1,N, FOR I ~ 0 STEP 1 UNTIL N DO A[I]) ;
              READ(CARD, F1, N, FOR I ~ 0 STEP 1 UNTIL N DO Z[I]) ;
              WRITE(PRINTER, F1, N, FOR I ~ 0 STEP 1 UNTIL N DO A[I]) ;
              WRITE(PRINTER, F1, N, FOR I ~ 0 STEP 1 UNTIL N DO Z[I]) ;
              E ~ F ~ 0.5 ; INCRE ~ 1 ; G ~ 3 ;
              CPOLY(E, F, INCRE, G, N, A, Z, C, D, ER, EI) ;
              WRITE(PRINTER, F2, FOR I ~ 1 STEP 1 UNTIL N DO
                             [C[I], D[I]]) ;
                        WRITE(PRINTER, F4, FOR I ~ 1 STEP 1 UNTIL N DO
                             [ER[I], EI[I]])
         END .
   4-4.8   3.2   -.2   -.3    .1
   4 8.4  -3.8  -1.     .4    .
