COMMENT       THIS PROCEDURE WILL COMPUTE THE INVERSE OF A SYMMETRIC    ISYM0010
              MATRIX.  THE PROCEDURE WILL FAIL IF ANY "TRAILING MINOR"  ISYM0020
              IS ZERO.   THIS CONDITION CANNOT EXITS IF THE MATRIX IS   ISYM0030
              "DEFINITE".  UPON ENTERING THE PROCEDURE THE DIAGONAL     ISYM0040
              ELEMENTS OF THE ORIGINAL MATRIX ARE PLACED IN A VECTOR    ISYM0050
              CALLED "DIAGONAL".  THE INVERSE MATRIX IS PLACED IN THE   ISYM0060
              UPPER TRIANGULAR HALF, INCLUDING THE MAIN DIAGONAL, OF    ISYM0070
              THE ORIGINAL ARRAY.                                       ISYM0080

              R.D. RODMAN,                                              ISYM0090
              (PROFESSIONAL SERVICES GROUP),                            ISYM0100

              CARD SEQUENCE STARTS WITH "ISYM0010",                     ISYM0110
              FIRST RELEASE 12/01/62.  ;                                ISYM0120

PROCEDURE     INVPDS(N, A, DIAGONAL) ;                                  ISYM0130
VALUE         N ;                                                       ISYM0140
INTEGER       N ;                                                       ISYM0150
REAL ARRAY    A[0,0], DIAGONAL[0] ;                                     ISYM0160
         BEGIN                                                          ISYM0170
INTEGER       I, J, K, I1, L ;                                          ISYM0180
REAL          DIAG, Q ;                                                 ISYM0190
REAL ARRAY    TEMP[0:N] ;                                               ISYM0200
LABEL         IN1, IN2, IN3 ;                                           ISYM0210

COMMENT       THE ORIGINAL MATRIX IS DECOMPOSED INTO THE PRODUCT OF A   ISYM0220
              UNIT LOWER TRIANGULAR MATRIX, A DIAGONAL MATRIX, AND A    ISYM0230
              UNIT UPPER TRIANGULAR MATRIX WHICH IS THE TRANSPOSE OF    ISYM0240
              THE UNIT LOWER TRIANGULAR MATRIX.  ;                      ISYM0250

IN1:          FOR I ~ N STEP -1 UNTIL 1 DO                              ISYM0260
         BEGIN                                                          ISYM0270
              DIAGONAL[I] ~ A[I,I] ;                                    ISYM0280
              FOR K ~ I+1 STEP 1 UNTIL N DO                             ISYM0290
              TEMP[K] ~ A[I,K] | A[K,K] ;                               ISYM0300
              FOR J ~ I STEP -1 UNTIL 1 DO                              ISYM0310
         BEGIN                                                          ISYM0320
              Q~0 ;                                                     ISYM0330
              FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ A[J,K] | TEMP[K] + Q ;  ISYM0340
              IF I=J THEN A[J,I] ~ DIAG ~ A[J,I] - Q  ELSE              ISYM0350
                   A[J,I] ~ (A[J,I]-Q)/DIAG                             ISYM0360
         END                                                            ISYM0370
         END ;                                                          ISYM0380

COMMENT       THESE THREE MATRICES ARE INVERTED.  ;                     ISYM0390

IN2:          FOR I ~ N STEP -1 UNTIL 1 DO                              ISYM0400
         BEGIN                                                          ISYM0410
              I1 ~ I+1 ;  A[I,I] ~ 1.0 / A[I,I] ;                       ISYM0420
              FOR J ~ N STEP -1 UNTIL I1 DO                             ISYM0430
         BEGIN                                                          ISYM0440
              Q~0 ;  L ~ J-1 ;                                          ISYM0450
              FOR K ~ I1 STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;    ISYM0460
              A[I,J] ~ -A[I,J] - Q                                      ISYM0470
         END                                                            ISYM0480
         END ;                                                          ISYM0490

COMMENT       THE INVERTED MATRICES ARE MULTIPLIED, IN REVERSE ORDER,   ISYM0500
              TO GIVE THE DESIRED INVERSE.  ;                           ISYM0510

IN3:          FOR I ~ N STEP -1 UNTIL 1  DO                             ISYM0520
         BEGIN                                                          ISYM0530
              I1 ~ I-1 ;  DIAG ~ A[I,I] ;                               ISYM0540
              FOR K ~ 1 STEP 1 UNTIL I1 DO                              ISYM0550
              TEMP[K] ~ A[K,K] | A[K,I] ;                               ISYM0560
              FOR J ~ N STEP -1 UNTIL I DO                              ISYM0570
         BEGIN                                                          ISYM0580
              Q~0 ;                                                     ISYM0590
              FOR K ~ 1 STEP 1 UNTIL I1 DO Q ~ A[K,J] | TEMP[K] + Q ;   ISYM0600
              A[I,J] ~ IF I=J THEN A[I,J] + Q ELSE A[I,J] | DIAG + Q    ISYM0610
         END                                                            ISYM0620
         END                                                            ISYM0630
         END ;                                                          ISYM0640
