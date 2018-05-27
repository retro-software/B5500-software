COMMENT       THIS PROCEDURE COMPUTES THE INVERSE OF A MATRIX.  IF THE  INVS0010
              MATRIX TO BE INVERTED IS SINGULAR, AN EXIT IS MADE TO THE INVS0020
              NON-LOCAL LABEL "SINGULAR".  THE INVERSE MATRIX REPLACES  INVS0030
              THE ORIGINAL MATRIX IN MEMORY.                            INVS0040

              R.D. RODMAN,                                              INVS0050
              (PROFESSIONAL SERVICES DIVISIONAL GROUP),                 INVS0060

              CARD SEQUENCE BEGINS WITH INVS0010.                       INVS0070
              FIRST RELEASE 12/01/62.  ;                                INVS0080

PROCEDURE     INVERSE(N, A, EPS, SINGULAR) ;                            INVS0090
VALUE         N, EPS ;                                                  INVS0100
INTEGER       N ;                                                       INVS0110
REAL          EPS ;                                                     INVS0120
REAL ARRAY    A[0,0] ;                                                  INVS0130
LABEL         SINGULAR ;                                                INVS0140
         BEGIN                                                          INVS0150
INTEGER       I, J, K, II, N1, K2, L ;                                  INVS0160
REAL          BIG, TEMP, DIAG, Q ;                                      INVS0170
INTEGER ARRAY F[0:N] ;                                                  INVS0180
LABEL         I2, I3, I4, I5, I6, SK3 ;                                 INVS0190

COMMENT       THE MATRIX IS TRIANGULARIZED USING ROW PIVOTS.  IF THE    INVS0210
              LARGEST ELEMENT IN A NEWLY CALCULATED COLUMN IS, IN       INVS0220
              ABSOLUTE VALUE, LESS THAN EPS, AN EXIT IS MADE TO THE     INVS0230

              NON-LOCAL LABEL "SINGULAR". ;                             INVS0240
I2:           FOR I ~ 1 STEP 1 UNTIL N DO                               INVS0250
         BEGIN                                                          INVS0260
              II ~ I-1 ;                                                INVS0270
              FOR J ~ I STEP 1 UNTIL N DO                               INVS0280
         BEGIN                                                          INVS0290
              Q ~ 0 ;                                                   INVS0300
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[J,K] | A[K,I] + Q ;    INVS0310
              A[J,I] ~ A[J,I] - Q                                       INVS0320
         END ;                                                          INVS0330
              BIG ~ 0 ;  K2 ~ I ;                                       INVS0340
I3:           FOR K ~ I STEP 1 UNTIL N DO                               INVS0350
         BEGIN                                                          INVS0360
              IF ABS(A[K,I]) > BIG THEN                                 INVS0370
         BEGIN                                                          INVS0380
              BIG ~ ABS(A[K,I]) ;  K2 ~ K                               INVS0390
         END                                                            INVS0400
         END ;                                                          INVS0410
              IF BIG { EPS THEN GO TO SINGULAR ;                        INVS0420
              F[I] ~ K2 ;                                               INVS0430
              IF K2 ! I THEN                                            INVS0440
I4:           FOR K ~ 1 STEP 1 UNTIL N DO                               INVS0450
         BEGIN                                                          INVS0460
              TEMP ~ A[I,K] ;  A[I,K] ~ A[K2,K] ;  A[K2,K] ~ TEMP       INVS0470
         END ;                                                          INVS0480
              DIAG ~ A[I,I] ;                                           INVS0490

COMMENT       A PIVOT WAS MADE.  A ROW IS NOW CALCULATED.  ;            INVS0500

              FOR J ~ I+1 STEP 1 UNTIL N DO                             INVS0510
         BEGIN                                                          INVS0520
              Q ~ 0 ;                                                   INVS0530
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;    INVS0540
              A[I,J] ~ (A[I,J] - Q) / DIAG                              INVS0550
         END                                                            INVS0560
         END ;                                                          INVS0570

COMMENT       THE LOWER TRIANGULAR MATRIX IS INVERTED.  ;               INVS0580

I5:           FOR I ~ 1 STEP 1 UNTIL N DO                               INVS0590
         BEGIN                                                          INVS0600
              II ~ I-1 ;  DIAG ~ A[I,I] ;                               INVS0610
              FOR J ~ 1 STEP 1 UNTIL I DO                               INVS0620
         BEGIN                                                          INVS0630
              IF I=J THEN A[I,J] ~ 1/DIAG ELSE                          INVS0640
         BEGIN                                                          INVS0650
              Q ~ 0 ;                                                   INVS0660
              FOR K ~ J STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;    INVS0670
              A[I,J] ~ -Q/DIAG                                          INVS0680
         END                                                            INVS0690
         END                                                            INVS0700
         END ;                                                          INVS0710
              N1 ~ N-1 ;                                                INVS0720

COMMENT       THE UPPER TRIANGULAR MATRIX IS INVERTED.  ;               INVS0730

              FOR I ~ N1 STEP -1 UNTIL 1 DO                             INVS0740
         BEGIN                                                          INVS0750
              II ~ I+1 ;                                                INVS0760
              FOR J ~ N STEP -1 UNTIL II DO                             INVS0770
         BEGIN                                                          INVS0780
              Q~0 ;  L ~ J-1 ;                                          INVS0790
              FOR K ~ II STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;    INVS0800
              A[I,J] ~ -A[I,J] - Q                                      INVS0810
         END                                                            INVS0820
         END ;                                                          INVS0830

COMMENT       THE INVERTED TRIANGULAR MATRICES ARE MULTIPLIED  TOGETHER.INVS0840
              THEIR PRODUCT IS THE DESIRED INVERSE.  ;                  INVS0850

              FOR I ~ 1 STEP 1 UNTIL N1 DO                              INVS0860
              FOR J ~ 1 STEP 1 UNTIL N DO                               INVS0870
         BEGIN                                                          INVS0880
              Q ~ 0 ;                                                   INVS0890
              IF I}J THEN                                               INVS0900
         BEGIN                                                          INVS0910
              FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;   INVS0920
              A[I,J] ~ A[I,J] + Q                                       INVS0930
         END                                                            INVS0940
              ELSE                                                      INVS0950
         BEGIN                                                          INVS0960
              FOR K ~ J STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;     INVS0970
              A[I,J] ~ Q                                                INVS0980
         END                                                            INVS0990
         END ;                                                          INVS1000

COMMENT       THE COLUMNS OF THE PRODUCT ARE NOW PERMUTED IN SUCH A WAY INVS1010
              AS TO COMPENSATE FOR THE ROW PIVOTS MADE IN THE COURSE OF INVS1020
              TRIANGULARIZATION.  ;                                     INVS1030

I6:           FOR J ~ N STEP -1 UNTIL 1 DO                              INVS1040
         BEGIN                                                          INVS1050
              K2 ~ F[J] ;                                               INVS1070
              IF F[J] = J THEN GO TO SK3 ;                              INVS1060
              FOR K ~ 1 STEP 1 UNTIL N DO                               INVS1080
         BEGIN                                                          INVS1090
              TEMP ~ A[K,K2] ;  A[K,K2] ~ A[K,J] ;  A[K,J] ~ TEMP       INVS1100
         END ;                                                          INVS1110
SK3:     END                                                            INVS1120
         END ;                                                          INVS1130
