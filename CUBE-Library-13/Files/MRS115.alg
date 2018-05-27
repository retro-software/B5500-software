         BEGIN                                                          MAIN0010
                                                                        MAIN0020
COMMENT       ROBERT R. HAMMERS ,                                       MAIN0030
              (PROFESSIONAL SERVICES GROUP) ,                           MAIN0040
              BURROUGHS CORPORATION , PASADENA                          MAIN0050
                                                                        MAIN0060
                                                                        MAIN0070
              CARD SEQUENCE BEGINS WITH MAIN0010 FOR MAIN PROGRAM       MAIN0080
              CARD SEQUENCE BEGINS WITH CONT0010 FOR THE PROCEDURE      MAIN0090
                                                                        MAIN0/T0
                                                                        MAIN0110
              FIRST RELEASE 5/1/64  ;                                   MAIN0120
                                                                        MAIN0130
                                                                        MAIN0140
FILE IN       F (1,10)  ;                                               MAIN0150
FILE OUT      P 1(1,15)  ;                                              MAIN0160
                                                                        MAIN0170
REAL          C,D,R  ;                                                  MAIN0180
INTEGER       N,NN,N1,J,J1  ;                                           MAIN0190
BOOLEAN       BOOL  ;                                                   MAIN0200
LABEL         START,L,FINISH  ;                                         MAIN0210
                                                                        MAIN0220
FORMAT IN     F1 (I10)  ,  F2 (4E20.12)  ;                              MAIN0230
FORMAT OUT    P1 (//"THE DEGREE OF EACH INPUT POLYNOMIAL IS",I4/        MAIN0240
                    "THE COEFFICIENTS OF THE INPUT POLYNOMIALS ARE")   ,MAIN0250
              P2("A[",I2," ]=",F15.8,X10,"B[",I2," ]=",F15.8,I1)       ,MAIN0260
              P3("FOLLOWING ARE THE COEFFICIENTS OF THE CONTINUED"     ,MAIN0270
                    " FRACTION"/X9,"C",X9,"D")                         ,MAIN0280
              P35 ("   R  U  N     C  O  M  P  L  E  T  E")            ,MAIN0290
              P4 (3F15.10)                                             ,MAIN0300
              P5("A DIVISION BY ZERO HAS OCCURED IN ONE OF THE"        ,MAIN0310
                    " TRANSFORMATIONS")                                ;MAIN0320
                                                                        MAIN0330
COMMENT       WHEN ALL THE DATA SETS ARE COMPLETED A BLANK IS READ      MAIN0340
              SETTING N TO 0 AND " RUN COMPLETE " IS PRINTED OUT   ;    MAIN0350
                                                                        MAIN0360
START:        READ (F,F1,N)  ;  IF N { 0 THEN GO TO FINISH ELSE         MAIN0370
              WRITE (P,P1,N)  ;                                         MAIN0380
                                                                        MAIN0390
         BEGIN                                                          MAIN0400
                                                                        MAIN0410
REAL ARRAY    A,B[0:N]  ;                                               MAIN0420
                                                                        MAIN0430
PROCEDURE     CONTINUED (A,B,L)  ;                                      CONT0010
                                                                        CONT0020
REAL ARRAY    A,B[0]   ;                                                CONT0030
LABEL         L         ;                                               CONT0040
                                                                        CONT0050
         BEGIN                                                          CONT0060
                                                                        CONT0070
COMMENT       IF B[NN] = 0 THEN A DIVISION BY ZERO WOULD BE NECESSARY TOCONT0080
              CONTINUE , THEREFORE A BRANCH IS MADE , THE CONDITION IS  CONT0090
              PRINTED OUT AND THE NEXT DATA SET READ IN  ;              CONT0/T0
                                                                        CONT0110
              IF (R ~ B[NN]) = 0 THEN GO TO L ELSE R ~ 1/R   ;          CONT0120
              C ~ A[NN]|R  ;                                            CONT0130
              D ~ A[N1~NN-1] ~ A[N1]-C|B[N1]   ;                        CONT0140
              D ~ R|D  ;                                                CONT0150
                                                                        CONT0160
              FOR J ~ N1 STEP -1 UNTIL 1 DO                             CONT0170
         BEGIN                                                          CONT0180
              R ~ A[J1~J-1] ~ A[J1]-C|B[J1]   ;                         CONT0190
              B[J] ~ D|B[J]-R                                           CONT0200
         END  ;                                                         CONT0210
              B[0] ~ D|B[0]                                             CONT0220
         END  ;                                                         CONT0230
                                                                        MAIN0440
COMMENT       ALTERNATE COEFFICIENTS OF A AND B ARE READ STARTING WITH  MAIN0450
              A[0],B[0],A[1], ........ ,A[N],B[N]  ;                    MAIN0460
                                                                        MAIN0470
              READ (F,F2,FOR J ~ 0 STEP 1 UNTIL N DO [A[J],B[J]])      ;MAIN0480
                                                                        MAIN0490
              BOOL ~ TRUE  ;                                            MAIN0500
                                                                        MAIN0510
              FOR J ~ 0 STEP 1 UNTIL N DO                               MAIN0520
              WRITE (P,P2,J,A[J],J,B[J])   ;                            MAIN0530
              WRITE (P,P3)  ;                                           MAIN0540
                                                                        MAIN0550
              FOR NN ~ N STEP -1 UNTIL 1 DO                             MAIN0560
         BEGIN                                                          MAIN0570
              IF BOOL THEN                                              MAIN0580
         BEGIN                                                          MAIN0590
              CONTINUED (A,B,L)  ;  BOOL ~ FALSE                        MAIN0600
         END  ELSE                                                      MAIN0610
         BEGIN                                                          MAIN0620
              CONTINUED (B,A,L)  ;  BOOL ~ TRUE                         MAIN0630
         END  ;                                                         MAIN0640
                                                                        MAIN0650
              WRITE (P,P4,C,D)                                          MAIN0660
         END  ;                                                         MAIN0670
              C ~ IF BOOL THEN A[0]/B[0] ELSE B[0]/A[0]  ;              MAIN0680
              WRITE (P,P4,C)  ;  GO TO START                            MAIN0690
         END  ;                                                         MAIN0700
                                                                        MAIN0710
L:            WRITE (P,P5)  ;  GO TO START  ;                           MAIN0720
                                                                        MAIN0730
FINISH:       WRITE (P,P35)                                             MAIN0740
                                                                        MAIN0750
         END  .                                                         MAIN0760
