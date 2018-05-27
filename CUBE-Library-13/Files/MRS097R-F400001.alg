COMMENT       THIS PROCEDURE SOLVES A SET OF N BY N SIMULTANEOUS LINEAR SLVE0010
              EQUATIONS USING THE CROUT REDUCTION.  PROVISION IS MADE   SLVE0020
              FOR EASILY SOLVING SEVERAL SETS OF SUCH EQUATIONS, THE    SLVE0030
              COEFFICIENT MATRIX OF WHICH REMAINS THE SAME.  PROVISION  SLVE0040
              IS ALSO MADE FOR ITERATING THE SOLUTION TO REDUCE ROUND-  SLVE0050
              OFF ERROR.  THIS FEATURE IS OPTIONAL.  THE ORIGINAL       SLVE0060
              EQUATIONS MAY BE PERMUTED, BUT OTHERWISE REMAIN INTACT.   SLVE0070

              R.D. RODMAN,                                              SLVE0080
              (PROFESSIONAL SERVICES DIVISIONAL GROUP),                 SLVE0090

              CARD SEQUENCE STARTS WITH SLVE0010.                       SLVE0100
              FIRST RELEASED 12/01/62.  ;                               SLVE0110

PROCEDURE     SOLVE(N, A, C, RSW, E, K1, EPS, X, E1, E2) ;              SLVE0130
VALUE         N, RSW, E, K1, EPS ;                                      SLVE0140
INTEGER       N, K1 ;                                                   SLVE0150
REAL          E, EPS ;                                                  SLVE0160
BOOLEAN       RSW ;                                                     SLVE0170
REAL ARRAY    A[0,0], C, X[0] ;                                         SLVE0180
LABEL         E1, E2 ;                                                  SLVE0190
         BEGIN                                                          SLVE0200
INTEGER       I, J, K, J1, K2, L ;                                      SLVE0210
REAL          BIG, TEMP, DIAG, NORM, Q ;                                SLVE0220
OWN INTEGER ARRAY F[0:N] ;                                              SLVE0230
REAL ARRAY    D[0:N] ;                                                  SLVE0240
OWN REAL ARRAY B[0:N, 0:N] ;                                            SLVE0250
LABEL         S1, S2, S3, S4, S5, S6, REP, S7, S8, S9, IT1, S10, S11,   SLVE0260
              S12, S13, S14, S15, EXIT ;                                SLVE0270
S1:           IF RSW THEN GO TO REP ;                                   SLVE0290

COMMENT       THE COEFFICIENT MATRIX IS TRIANGULARIZED.  ;              SLVE0300

              FOR I ~ 1 STEP 1 UNTIL N DO                               SLVE0310
              FOR J ~ 1 STEP 1 UNTIL N DO                               SLVE0320
              B[I,J] ~ A[I,J] ;                                         SLVE0330
S2:           FOR I ~ 1 STEP 1 UNTIL N DO                               SLVE0340
         BEGIN                                                          SLVE0350
              L ~ I-1 ;                                                 SLVE0360
              FOR J ~ I STEP 1 UNTIL N DO                               SLVE0370
         BEGIN                                                          SLVE0380
              Q~0 ;                                                     SLVE0390
              FOR K ~ 1 STEP 1 UNTIL L DO Q ~ B[J,K] | B[K,I] + Q ;     SLVE0400
              B[J,I] ~ B[J,I] - Q                                       SLVE0410
         END ;                                                          SLVE0420
              BIG ~ 0 ;  K2 ~ I ;                                       SLVE0430
S3:           FOR K ~ I STEP 1 UNTIL N DO                               SLVE0440
         BEGIN                                                          SLVE0450
              IF ABS(B[K,I]) > BIG THEN                                 SLVE0460
         BEGIN                                                          SLVE0470
              BIG ~ ABS(B[K,I]) ;  K2 ~ K                               SLVE0480
         END                                                            SLVE0490
         END ;                                                          SLVE0500

COMMENT       E1 IS THE NON-LOCAL LABEL TO WHICH AN EXIT IS MADE IF THE SLVE0510
              COEFFICIENT MATRIX IS SINGULAR.  ;                        SLVE0520

S4:           IF BIG { EPS THEN GO TO E1 ;  F[I] ~ K2 ;                 SLVE0530
              IF K2 ! I THEN                                            SLVE0540
S5:           FOR K ~ 1 STEP 1 UNTIL N DO                               SLVE0550
         BEGIN                                                          SLVE0560
              TEMP ~ A[K2,K] ;  A[K2,K] ~ A[I,K] ;  A[I,K] ~ TEMP ;     SLVE0570
              TEMP ~ B[K2,K] ;  B[K2,K] ~ B[I,K] ;  B[I,K] ~ TEMP ;     SLVE0580
         END ;                                                          SLVE0590
              DIAG ~ B[I,I] ;                                           SLVE0600
S6:           FOR J ~ I+1 STEP 1 UNTIL N DO                             SLVE0610
         BEGIN                                                          SLVE0620
              Q~0 ;                                                     SLVE0630
              FOR K ~ 1 STEP 1 UNTIL L DO Q ~ B[I,K] | B[K,J] + Q ;     SLVE0640
              B[I,J] ~ (B[I,J]-Q)/DIAG                                  SLVE0650
         END                                                            SLVE0660
         END ;                                                          SLVE0670

COMMENT       THE REDUCED "C" VECTOR IS COMPUTED.  ;                    SLVE0680

REP:          FOR I ~ 1 STEP 1 UNTIL N DO                               SLVE0690
         BEGIN                                                          SLVE0700
              TEMP ~ C[F[I]] ;  C[F[I]] ~ C[I] ;  D[I] ~ C[I] ~ TEMP    SLVE0710
         END ;                                                          SLVE0720

COMMENT       THE BACKWARD PASS, GIVING THE DESIRED SOLUTION,           SLVE0730
              IS EXECUTED.  ;                                           SLVE0740

              FOR I ~ 1 STEP 1 UNTIL N DO                               SLVE0750
         BEGIN                                                          SLVE0760
              L ~ I-1 ;  Q~0 ;                                          SLVE0770
S7:           FOR K ~ 1 STEP 1 UNTIL L DO Q ~ B[I,K] | D[K] + Q ;       SLVE0780
              D[I] ~ (D[I]-Q)/B[I,I]                                    SLVE0790
         END ;                                                          SLVE0800
S8:           FOR I ~ N STEP -1 UNTIL 1 DO                              SLVE0810
         BEGIN                                                          SLVE0820
              Q~0 ;                                                     SLVE0830
              FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ B[I,K] | X[K] + Q ;     SLVE0840
              X[I] ~ D[I] - Q                                           SLVE0850
         END ;                                                          SLVE0860
S9:           IF E = 0 THEN GO TO EXIT ;                                SLVE0870

COMMENT       THE SOLUTION IS ITERATED AND TESTED FOR ACCURACY.  ;      SLVE0880

              J1 ~ 0 ;                                                  SLVE0890
IT1:          IF J1 } K1 THEN GO TO E2 ;                                SLVE0900
              NORM ~ 0 ;                                                SLVE0910
              FOR I ~ 1 STEP 1 UNTIL N DO                               SLVE0920
         BEGIN                                                          SLVE0930
              Q~0 ;  L ~ I-1 ;                                          SLVE0940
S10:          FOR K ~ 1 STEP 1 UNTIL N DO Q ~ A[I,K] | X[K] + Q ;       SLVE0950
              D[I] ~ C[I] - Q ;                                         SLVE0960
S11:          NORM ~ ABS(D[I]) + NORM ;                                 SLVE0970
              Q~0 ;                                                     SLVE0980
S12:          FOR K ~ 1 STEP 1 UNTIL L DO Q ~ B[I,K] | D[K] + Q ;       SLVE0990
              D[I] ~ (D[I]-Q)/B[I,I]                                    SLVE1000
         END ;                                                          SLVE1010
              FOR I ~ N STEP -1 UNTIL 1 DO                              SLVE1020
         BEGIN                                                          SLVE1030
              Q~0 ;                                                     SLVE1040
S13:          FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ B[I,K] | D[K] + Q ;     SLVE1050
              X[I] ~ X[I] + D[I] - Q                                    SLVE1060
         END ;                                                          SLVE1070
S14:          J1 ~ J1 + 1 ;                                             SLVE1080
S15:          IF N | E < NORM THEN GO TO IT1 ;                          SLVE1090
EXIT:    END ;                                                          SLVE1100
