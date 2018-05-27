         BEGIN                                                          MAIN0010
                                                                        MAIN0020
COMMENT       THIS PROGRAM USES PROCEDURE TWOVAR TO EVALUATE A          MAIN0030
              HYPERGEOMETRIC SERIES IN TWO VARIABLES .                  MAIN0040
                                                                        MAIN0050
              ROBERT R. HAMMERS ,                                       MAIN0060
              BURROUGHS CORPORATION ,                                   MAIN0070
              (PROFESSIONAL SERVICES) ,                                 MAIN0080
              PASADENA , CALIFORNIA .                                   MAIN0090
                                                                        MAIN0/T0
              CARD SEQUENCE FOR MAIN PROGRAM BEGINS WITH MAIN0010 .     MAIN0110
              CARD SEQUENCE FOR PROCEDURE BEGINS WITH PROC0010 .        MAIN0120
                                                                        MAIN0130
              FIRST RELEASE 10 - 1 - 64                                ;MAIN0140
                                                                        MAIN0150
                                                                        MAIN0160
FILE IN       RDR (1,10)  ;                                             MAIN0170
FILE OUT      PRNTR 1(2,15)  ;                                          MAIN0180
REAL          SUM,X,Y,EPS,SECS  ;                                       MAIN0190
INTEGER       NMAX,MMAX,NP,TESTNO,FLAG,CASE,I  ;                        MAIN0200
REAL ARRAY    PARAM[0:5]  ;                                             MAIN0210
INTEGER ARRAY INDEX[0:5]  ;                                             MAIN0220
LABEL         START,FINISH  ;                                           MAIN0230
                                                                        MAIN0240
FORMAT IN     FIN (5I4,3E20.12/(4(E18.11,I2)))  ;                       MAIN0250
FORMAT OUT    OUT0 ("THE INPUT PARAMETERS FOR TEST CASE NUMBER " ,I2,   MAIN0260
                    " ARE AS FOLLOWS:"//X10,"X = ",F7.2,",Y = ",F7.2,   MAIN0270
                    ",EPS = ",E8.1,",MMAX = ",I3,",NMAX = ",I3,         MAIN0280
                    ",TESTNO = ",I2,",THE NUMBER OF PARAMETERS IS ",    MAIN0290
                      I1/(X10,"P[",I1,"] = ",F7.2,X10,                  MAIN0300
                      "INDEX[",I1,"] = ",I2))  ,                        MAIN0310
              OUT1 (X10,"THE TIME TO PROCESS TEST CASE ",I1," IS ",     MAIN0320
                     F8.3," SECONDS , SUM = ",E20.12//)  ,              MAIN0325
              OUT2 ("SERIES CONVERGES TO 0 , SUM =",E25.12//)  ,        MAIN0330
              OUT3 ("SERIES DIVERGES"//)  ,                             MAIN0340
              OUT4 ("ARGUMENT OUT OF RANGE"//)  ,                       MAIN0350
              OUT5 ("MMAX TERMS ARE EXCEEDED , SUM = ",E25.11//)  ,     MAIN0360
              OUT6 ("NMAX TERMS ARE EXCEEDED , SUM = ",E25.11//)  ;     MAIN0370
                                                                        MAIN0380
PROCEDURE     TWOVAR (X,Y,P,INDEX,NMAX,MMAX,EPS,SUM,NP,TESTNO,FLAG)  ;  PROC0010
                                                                        PROC0020
COMMENT       X      - ONE OF THE TWO VARIABLES  .                      PROC0030
              Y      - THE OTHER VARIABLE .                             PROC0040
              P      - ARRAY CONTAINING PARAMETERS FOR HYPERGEOMETRIC   PROC0050
                       SERIES STARTING AT CELL 1 .                      PROC0060
              INDEX  - AN INTEGER ARRAY WHICH INDICATES WHAT TYPE OF    PROC0070
                       INDEX THE CORRISPONDING P ARRAY ELEMENTS HAVE    PROC0080
                       ACCORDING TO THE FOLLOWING METHOD:               PROC0090
                                                                        PROC0/T0
                     TYPE OF        **     VALUE IN INDEX ARRAY ******* PROC0110
                      INDEX         ** (NUMERATOR  **** (DENOMINATOR ** PROC0120
                                          PARAMETER****    PARAMETER)** PROC0130
                        M                   1                   11      PROC0140
                        N                   2                   12      PROC0150
                        M+N                 3                   13      PROC0160
                        2M+N                4                   14      PROC0170
                        M-N                 5                   15      PROC0180
                        2M-N                6                   16      PROC0190
                        N-M                 7                   17      PROC0200
                        2N-M                8                   18      PROC0210
                                                                        PROC0220
              SUM    - THE VALUE OF THE HYPERGEOMETRIC SERIES .         PROC0230
              NMAX   - THE PROCESS DISCONTINUES IF INDEX N EXCEEDS NMAX.PROC0240
              MMAX   - THE PROCESS DISCONTINUES IF INDEX M EXCEEDS MMAX.PROC0250
              EPS    - THE RELATIVE ACCUARCY DESIRED FOR BOTH M AND N   PROC0260
                       SUMMATIONS. SATISFIED WHEN NTERM / SUM < EPS .   PROC0270
              NP     - THE NUMBER OF PARAMETERS AND CORRESPONDING INDEX.PROC0280
              TESTNO - VALUE FROM 1-34 WHICH DETERMINES WHICH TEST ON   PROC0290
                         ARGUEMENTS SHOULD APPLY .                      PROC0300
              FLAG   - INDICATES WHY PROCESS TERMINATED :               PROC0310
                         FLAG = 0    NORMAL SOLUTION STORED IN SUM .    PROC0320
                         FLAG = 10   TERMS CONVERGE TO 0 , SUM EXACT .  PROC0330
                         FLAG = 20   SERIES DIVERGES .                  PROC0340
                         FLAG = 30   M BECOMES GREATER THAN MMAX .      PROC0350
                         FLAG = 40   N BECOMES GREATER THAN NMAX .      PROC0360
                         FLAG = 50   ARGUEMENT(S) OUT OF REGION OF CONV;PROC0370
                                                                        PROC0380
VALUE         X,Y,NMAX,MMAX,EPS,NP,TESTNO  ;                            PROC0390
INTEGER       NMAX,MMAX,NP,TESTNO,FLAG  ;                               PROC0400
REAL          X,Y,EPS,SUM  ;                                            PROC0410
REAL ARRAY    P[0]  ;                                                   PROC0420
INTEGER ARRAY INDEX[0]  ;                                               PROC0430
                                                                        PROC0440
         BEGIN                                                          PROC0450
                                                                        PROC0460
LABEL         L1,L3,L4,L5,L6,L7,L8,LN2,LN5,LN8,ENDN,ENDN1,IEND,END1,NRECPROC0470
              ,MREC,EXIT,NSUM,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13PROC0480
              ,T14,ERR50,T7A  ;                                         PROC0490
SWITCH        SW ~ L1,END1,L3,L4,L5,L6,L7,L8  ;                         PROC0500
SWITCH        SWIT ~ ENDN1,LN2,LN2,LN2,LN5,LN5,LN2,LN8  ;               PROC0510
SWITCH        SWA ~ T1,T2,T1,T3,T4,T14,T14,T4,T14,T4,T4,T2,T1,T14,T5,T4,PROC0520
              T14,T4,T4,T4,T14,T14,T6,T7,T8,T13,T13,T9,T13,T13,T10,T13, PROC0530
              T12,T11  ;                                                PROC0540
INTEGER       I,IND,M,N  ;                                              PROC0550
REAL          TEMPN,TEMP,MSUM,MTERM,NTERM,MFAC,NFAC,AX,AY  ;            PROC0560
BOOLEAN       BOOL  ;                                                   PROC0570
                                                                        PROC0580
REAL PROCEDURE EVALU (BOOL,TERM)  ;                                     PROC0590
VALUE         BOOL,TERM  ;                                              PROC0600
BOOLEAN       BOOL  ;                                                   PROC0610
REAL          TERM  ;                                                   PROC0620
              EVALU ~ IF BOOL THEN TERM ELSE 1/TERM  ;                  PROC0630
                                                                        PROC0640
              AX ~ ABS(X)  ;  AY ~ ABS(Y)  ;                            PROC0650
              GO TO SWA [TESTNO]  ;                                     PROC0660
ERR50:        FLAG ~ 50  ;  GO TO EXIT  ;                               PROC0670
T1:           IF AX } 1 OR AY } 1 THEN GO TO ERR50  ;  GO TO T14  ;     PROC0680
T2:           IF AX + AY > 1 THEN GO TO ERR50  ;  GO TO T14  ;          PROC0690
T3:           IF SQRT(AX) + SQRT(AY) > 1 THEN GO TO ERR50  ;  GO TO T14;PROC0700
T4:           IF AX > 1 THEN GO TO ERR50  ;  GO TO T14  ;               PROC0710
T5:           IF AX | (1+AY) > 1 THEN GO TO ERR50  ;                    PROC0720
T6:           IF AY > 1 THEN GO TO ERR50  ;  GO TO T14  ;               PROC0730
T7:           IF AX + (AY-.5) * 2 > .25 THEN GO TO ERR50  ;             PROC0740
T7A:          IF AX > .25 THEN GO TO ERR50  ;  GO TO T6  ;              PROC0750
T8:           IF (2-AY) | AY + 4 | AX > 1 THEN GO TO ERR50 ; GO TO T7A ;PROC0760
T9:           IF (4 | AX | AY + 2-AY) | AY > 1 THEN GO TO ERR50  ;      PROC0770
              GO TO T7A  ;                                              PROC0780
T10:          IF (AX + 1) | AY * 2 > 1 THEN GO TO ERR50  ;  GO TO T7A  ;PROC0790
T11:          IF ABS(9 | AY - 4 | AX) | 4 | AX + ABS(AY * 2 | AX | 27 + PROC0800
              8 | AX-AY) > 1 THEN GO TO ERR50  ;  GO TO T7A  ;          PROC0810
T12:          IF AX * 2 | AY * 2 | 27 + 18 | AX | AY + ABS(AX-AY) | 4 >1PROC0820
              OR AY > .25 THEN GO TO ERR50  ;                           PROC0830
T13:          IF AX > .25 THEN GO TO ERR50  ;                           PROC0840
T14:          FOR I ~ NP STEP -1 UNTIL 1 DO                             PROC0850
              IF (TEMP ~ P[I]) = ENTIER(TEMP) AND TEMP { 0 AND          PROC0860
              INDEX[I] } 5 THEN                                         PROC0870
         BEGIN                                                          PROC0880
              FLAG ~ 20  ;  GO TO EXIT                                  PROC0890
         END  ;                                                         PROC0900
                                                                        PROC0910
              TEMP ~ MSUM ~ TEMPN ~ M ~ MFAC ~NFAC ~ 1  ;               PROC0920
              FLAG ~ SUM ~ N ~ 0  ;  GO TO MREC  ;                      PROC0930
                                                                        PROC0940
COMMENT       DENOMINATOR PARAMETERS ARE GREATER THAN 10  ;             PROC0950
                                                                        PROC0960
NREC:         FOR I ~ NP STEP -1 UNTIL 1 DO                             PROC0970
         BEGIN                                                          PROC0980
              BOOL ~ IF (IND ~ INDEX[I]) > 10 THEN FALSE ELSE TRUE  ;   PROC0990
              GO TO SWIT[IND MOD 10]  ;                                 PROC1000
LN2:          IND ~ N - 1  ;  GO TO ENDN  ;                             PROC1010
LN5:          BOOL ~ NOT BOOL  ;  IND ~ N - 1  ;  GO TO ENDN  ;         PROC1020
LN8:          IND ~ 2|N - 2  ;  TEMPN ~ TEMPN | EVALU (BOOL,P[I]+IND)  ;PROC1030
              IND ~ IND + 1  ;                                          PROC1040
ENDN:         TEMPN ~ TEMPN | EVALU (BOOL,P[I]+IND)  ;                  PROC1050
ENDN1:   END  ;                                                         PROC1060
                                                                        PROC1070
              MSUM ~ TEMP ~ TEMPN  ;  M ~ MFAC ~ 1  ;                   PROC1080
                                                                        PROC1090
MREC:         FOR I ~ NP STEP -1 UNTIL 1 DO                             PROC1100
         BEGIN                                                          PROC1110
              BOOL ~ IF(IND ~ INDEX[I])> 10 THEN FALSE ELSE TRUE  ;     PROC1120
              GO TO SW[IND MOD 10]  ;                                   PROC1130
L1:           IND ~ M - 1  ;  GO TO IEND  ;                             PROC1140
L3:           IND ~ M + N - 1  ;  GO TO IEND  ;                         PROC1150
L4:           IND ~ 2 | M + N - 2  ;                                    PROC1160
              TEMP ~ TEMP | EVALU (BOOL,P[I]+IND)  ;                    PROC1170
              IND ~ IND + 1  ;  GO TO IEND  ;                           PROC1180
L5:           IND ~ IF M{ N THEN N-M ELSE M-N-1  ;  GO TO IEND  ;       PROC1190
L6:           IF (IND ~ 2|M-N) = 1 THEN                                 PROC1200
         BEGIN                                                          PROC1210
              TEMP ~ TEMP | EVALU (BOOL,P[I]*2)  ;  GO TO END1          PROC1220
         END  ;                                                         PROC1230
              IND ~ IF IND { 0 THEN ABS(IND) + 1 ELSE IND - 1  ;        PROC1240
              TEMP ~ TEMP | EVALU (BOOL,P[I] + IND)  ;                  PROC1250
              IND ~ IND - 1  ;  GO TO IEND  ;                           PROC1260
L7:           BOOL ~ NOT BOOL  ;                                        PROC1270
              IND ~ IF M{ N THEN N-M ELSE M-N-1  ;  GO TO IEND  ;       PROC1280
L8:           BOOL ~ NOT BOOL  ;                                        PROC1290
              IF (IND ~ 2|N-M) < 0 THEN IND ~ ABS(IND) - 1  ;           PROC1300
IEND:         TEMP ~ TEMP | EVALU(BOOL,P[I]+IND)  ;                     PROC1310
END1:    END  ;                                                         PROC1320
                                                                        PROC1330
              IF TEMP = 0 THEN GO TO NSUM  ;                            PROC1340
              MFAC ~ M | MFAC  ;                                        PROC1350
              MTERM ~ TEMP | X * M / MFAC  ;                            PROC1360
              MSUM ~ MSUM + MTERM  ;                                    PROC1370
              IF (M ~ M+1) > MMAX THEN                                  PROC1380
         BEGIN                                                          PROC1390
              FLAG ~ 30  ;  GO TO EXIT                                  PROC1400
         END  ;                                                         PROC1410
              IF MSUM = 0 THEN GO TO MREC  ;                            PROC1420
              IF ABS(MTERM/MSUM) > EPS THEN GO TO MREC  ;               PROC1430
                                                                        PROC1440
NSUM:         NTERM ~ MSUM | Y * N / NFAC  ;                            PROC1450
              SUM ~ SUM + NTERM  ;                                      PROC1460
              IF (N ~ N+1) > NMAX THEN                                  PROC1470
         BEGIN                                                          PROC1480
              FLAG ~ 40  ;  GO TO EXIT                                  PROC1490
         END  ;                                                         PROC1500
              NFAC ~ NFAC | N  ;                                        PROC1510
              IF SUM = 0 THEN GO TO NREC  ;                             PROC1520
              IF ABS (NTERM / SUM) > EPS THEN GO TO NREC  ;             PROC1530
EXIT:    END  ;                                                         PROC1540
                                                                        MAIN0390
                                                                        MAIN0400
START:        READ (RDR,FIN,CASE,NP,TESTNO,NMAX,MMAX,X,Y,EPS,FOR I ~ 1  MAIN0410
                    STEP 1 UNTIL NP DO [PARAM[I],INDEX[I]]) [FINISH]  ; MAIN0420
                                                                        MAIN0430
              WRITE (PRNTR,OUT0,CASE,X,Y,EPS,MMAX,NMAX,TESTNO,NP,FOR I ~MAIN0440
                     1 STEP 1 UNTIL NP DO [I,PARAM[I],I,INDEX[I]])  ;   MAIN0450
                                                                        MAIN0460
              SECS ~ TIME(1)  ;                                         MAIN0465
              TWOVAR (X,Y,PARAM,INDEX,NMAX,MMAX,EPS,SUM,NP,TESTNO,FLAG);MAIN0470
              SECS ~ (TIME(1) - SECS)/60  ;                             MAIN0475
                                                                        MAIN0480
              IF FLAG = 0 THEN                                          MAIN0490
         BEGIN                                                          MAIN0500
              WRITE (PRNTR,OUT1,CASE,SECS,SUM)  ;  GO TO START          MAIN0510
         END  ;                                                         MAIN0520
              IF FLAG = 10 THEN                                         MAIN0530
         BEGIN                                                          MAIN0540
              WRITE (PRNTR,OUT2,SUM)  ;  GO TO START                    MAIN0550
         END  ;                                                         MAIN0560
              IF FLAG = 20 THEN                                         MAIN0570
         BEGIN                                                          MAIN0580
              WRITE (PRNTR,OUT3)  ;  GO TO START                        MAIN0590
         END  ;                                                         MAIN0600
              IF FLAG = 30 THEN                                         MAIN0610
         BEGIN                                                          MAIN0620
              WRITE (PRNTR,OUT5,SUM)  ;  GO TO START                    MAIN0630
         END  ;                                                         MAIN0640
              IF FLAG = 40 THEN                                         MAIN0650
         BEGIN                                                          MAIN0660
              WRITE (PRNTR,OUT6,SUM)  ;  GO TO START                    MAIN0670
         END  ;                                                         MAIN0680
              WRITE (PRNTR,OUT4)  ;  GO TO START  ;                     MAIN0690
FINISH:  END  .                                                         MAIN0700
