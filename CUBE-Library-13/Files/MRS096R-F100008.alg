COMMENT       THIS PROCEDURE WILL FIND THE INVERSE OF A MATRIX GIVEN    ITIV0010
              THE ORIGINAL MATRIX AND AN APPROXIMATION TO THE DESIRED   ITIV0020
              INVERSE.  IF THE APPROXIMATION IS NOT NEAR ENOUGH, AN     ITIV0030
              EXIT IS MADE TO THE NON-LOCAL LABEL E1.  THE FINAL        ITIV0040
              INVERSE REPLACES THE APPROXIMATION IN MEMORY.             ITIV0050

              R.D. RODMAN,                                              ITIV0060
              (PROFESSIONAL SERVICES DIVISIONAL GROUP),                 ITIV0070

              CARD SEQUENCE BEGINS WITH ITIV0010,                       ITIV0080
              FIRST RELEASE 12/01/62.  ;                                ITIV0090

PROCEDURE     ITINV(N, A, B, E, K1, E1, E2) ;                           ITIV0100
VALUE         N, E, K1 ;                                                ITIV0110
INTEGER       N, K1 ;                                                   ITIV0120
REAL          E ;                                                       ITIV0130
REAL ARRAY    A, B[0,0] ;                                               ITIV0140
LABEL         E1, E2 ;                                                  ITIV0150
         BEGIN                                                          ITIV0160
INTEGER       I, J, K, J1, L ;                                          ITIV0170
REAL          NORM, TEST, Q ;                                           ITIV0180
REAL ARRAY    C[0:N], B1[0:N, 0:N] ;                                    ITIV0190
LABEL         S3, EXIT ;                                                ITIV0200

COMMENT       "A" CONTAINS THE ORIGINAL MATRIX AND "B" AN APPROXIMATION ITIV0210
              TO "A INVERSE".  "NORM" IS COMPUTED IN ORDER TO DETERMINE ITIV0220
              IF CONVERGENCE WILL OCCUR.  ;                             ITIV0230

              FOR I ~ 1 STEP 1 UNTIL N DO                               ITIV0240
              FOR J ~ 1 STEP 1 UNTIL N DO                               ITIV0250
         BEGIN                                                          ITIV0260
              Q~0 ;                                                     ITIV0270
              FOR K ~ 1 STEP 1 UNTIL N DO Q ~ A[I,K] | B[K,J] + Q ;     ITIV0280
              B1[I,J] ~ IF I=J THEN 1-Q ELSE -Q                         ITIV0290
         END ;                                                          ITIV0300
              FOR I ~ 1 STEP 1 UNTIL N DO                               ITIV0310
         BEGIN                                                          ITIV0320
              C[I] ~ 0 ;                                                ITIV0330
              FOR J ~ 1 STEP 1 UNTIL N DO                               ITIV0340
              C[I] ~ ABS(B1[I,J]) + C[I]                                ITIV0350
         END ;                                                          ITIV0360
              NORM ~ C[1] ;                                             ITIV0370
              FOR I ~ 2 STEP 1 UNTIL N DO                               ITIV0380
              IF NORM < C[I] THEN NORM ~ C[I] ;                         ITIV0390
              IF NORM } 1.0 THEN GO TO E1 ;                             ITIV0400
              J1 ~ 1 ;                                                  ITIV0410

COMMENT       AN ITERATION IS PERFORMED.  THE ACCURACY OF THE INVERSE   ITIV0420
              AND THE NUMBER OF ITERATIONS ARE TESTED AGAINST RELEVANT  ITIV0430
              INPUT PARAMETERS.  ;                                      ITIV0440

S3:           TEST ~ 0 ;                                                ITIV0450
              FOR I ~ 1 STEP 1 UNTIL N DO                               ITIV0460
              FOR J ~ 1 STEP 1 UNTIL N DO                               ITIV0470
         BEGIN                                                          ITIV0480
              Q~0 ;                                                     ITIV0490
              FOR K ~ 1 STEP 1 UNTIL N DO Q ~ A[I,K] | B[K,J] + Q ;     ITIV0500
              IF I=J THEN                                               ITIV0510
         BEGIN                                                          ITIV0520
              B1[I,J] ~ 2-Q ;  TEST ~ ABS(1-Q) + TEST                   ITIV0530
         END                                                            ITIV0540
              ELSE                                                      ITIV0550
         BEGIN                                                          ITIV0560
              B1[I,J] ~-Q ;  TEST ~ ABS(Q) + TEST                       ITIV0570
         END                                                            ITIV0580
         END ;                                                          ITIV0590
              IF TEST < E THEN GO TO EXIT ;                             ITIV0600
              FOR I ~ 1 STEP 1 UNTIL N DO                               ITIV0610
         BEGIN                                                          ITIV0620
              FOR J ~ 1 STEP 1 UNTIL N DO                               ITIV0630
         BEGIN                                                          ITIV0640
              Q~0 ;                                                     ITIV0650
              FOR K ~ 1 STEP 1 UNTIL N DO Q ~ B[I,K] | B1[K,J] + Q ;    ITIV0660
              C[J] ~ Q                                                  ITIV0670
         END ;                                                          ITIV0680
              FOR J ~ 1 STEP 1 UNTIL N DO                               ITIV0690
              B[I,J] ~ C[J]                                             ITIV0700
         END ;                                                          ITIV0710
              IF J1 } K1 THEN GO TO E2 ;                                ITIV0720
              J1 ~ J1 + 1 ;  GO TO S3 ;                                 ITIV0730
EXIT:    END ;                                                          ITIV0740
