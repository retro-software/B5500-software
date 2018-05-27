COMMENT       THIS PROCEDURE COMPUTES THE DETERMINANT OF A MATRIX.      DETR0010
              N IS THE ORDER       A IS THE MATRIX        EPS IS A      DETR0020
              TOLERANCE PARAMETER OF THE ORDER 1.0 @ -10                DETR0030
              D HOLDS THE VALUE OF THE DETERMINANT.                     DETR0040

              R. D. RODMAN                                              DETR0050
              (PROFESSIONAL SERVICES DIVISIONAL GROUP)                  DETR0060

              CARD SEQUENCE BEGINS WITH DETR0010.                       DETR0070
              FIRST RELEASE 6/01/63 ;                                   DETR0080

PROCEDURE DETERMINANT(N, A, EPS, D) ;                                   DETR0090
VALUE         N, EPS ;                                                  DETR0100
INTEGER       N ;                                                       DETR0110
REAL          EPS, D ;                                                  DETR0120
REAL ARRAY    A[0,0] ;                                                  DETR0130
         BEGIN                                                          DETR0140
INTEGER       I, J, K, K2, L ;                                          DETR0150
REAL          BIG, DIAG, TEMP, Q ;                                      DETR0160
LABEL         D1, D2, D3, EXIT ;                                        DETR0170

COMMENT       THE INPUT MATRIX IS DECOMPOSED INTO TWO TRIANGULAR        DETR0180
              MATRICES USING ROW PIVOTS. ;                              DETR0190

              D ~ 1.0 ;                                                 DETR0200
D1:           FOR I ~ 1 STEP 1 UNTIL N DO                               DETR0210
         BEGIN                                                          DETR0220
              L ~ I-1 ;                                                 DETR0230
              FOR J ~ I STEP 1 UNTIL N DO                               DETR0240
         BEGIN                                                          DETR0250
              Q~0 ;                                                     DETR0260
              FOR K ~ 1 STEP 1 UNTIL L DO Q ~ A[J,K] | A[K,I] + Q ;     DETR0270
              A[J,I] ~ A[J,I] - Q                                       DETR0280
         END ;                                                          DETR0290
              BIG ~ 0 ;  K2 ~ I ;                                       DETR0300
D2:           FOR K ~ I STEP 1 UNTIL N DO                               DETR0310
         BEGIN                                                          DETR0320
              IF ABS(A[K,I]) > BIG THEN                                 DETR0330
         BEGIN                                                          DETR0340
              BIG ~ ABS(A[K,I]) ;  K2 ~ K                               DETR0350
         END                                                            DETR0360
         END ;                                                          DETR0370
              IF BIG < EPS THEN                                         DETR0380
         BEGIN                                                          DETR0390
              D ~ 0 ;  GO TO EXIT                                       DETR0400
         END ;                                                          DETR0410

COMMENT       "BIG" IS THE LARGEST ELEMENT IN THE COLUMN JUST COMPUTED. DETR0420
              IF NECESSARY, A ROW EXCHANGE IS MADE SO THAT "BIG" IS     DETR0430
              ON THE MAIN DIAGONAL ;                                    DETR0440

              IF K2 ! I THEN                                            DETR0450
         BEGIN                                                          DETR0460
              D ~ -D ;                                                  DETR0470
              FOR K ~ 1 STEP 1 UNTIL N DO                               DETR0480
         BEGIN                                                          DETR0490
              TEMP ~ A[I,K] ;  A[I,K] ~ A[K2,K] ;  A[K2,K] ~ TEMP       DETR0500
         END                                                            DETR0510
         END ;                                                          DETR0520

COMMENT       THE VALUE OF THE DETERMINANT, "D", IS THE PRODUCT OF THE  DETR0530
              DIAGONAL ELEMENTS OF THE DECOMPOSED MATRIX. ;             DETR0540

D3:           DIAG ~ A[I,I] ;  D ~ D | DIAG ;                           DETR0550

COMMENT       A ROW OF THE DECOMPOSED MATRIX IS NOW CALCULATED. ;       DETR0560

              FOR J ~ I+1 STEP 1 UNTIL N DO                             DETR0570
         BEGIN                                                          DETR0580
              Q~0 ;                                                     DETR0590
              FOR K ~ 1 STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;     DETR0600
              A[I,J] ~ (A[I,J]-Q)/DIAG                                  DETR0610
         END                                                            DETR0620
         END ;                                                          DETR0630
EXIT:    END  ;                                                         DETR0640
