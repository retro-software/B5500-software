          BEGIN                                                         00000100
COMMENT        CUBE LIBRARY NUMBER IS G200013.  THE PROGRAM NAME IS     00000200
               "B0309B/TTY".  THIS VERSION DATED 5/3/68;                00000300
COMMENT        CORRELATION ANALYSIS                                     00000400
               CHARLES L. CLARK                                         00000500
               PROFESSIONAL SERVICES DIVISIONAL GROUP                   00000600
               BURROUGHS CORPORATION                                    00000700
               FIRST RELEASE DATE JULY 25, 1964                         00000800
               CONVERTED TO TELETYPE JULY 28, 1967 BY                   00000900
               F L LUCAS AND C A WILLIAMS;                              00001000
INTEGER        STA,NCHAR,NKAR,NSAM,NVAR,I,J;                            00001100
INTEGER ARRAY  ID[0:4] ;                                                00001200
BOOLEAN        SUM,SSQ,VAR ;                                            00001300
LABEL          NOMO;                                                    00001400
FILE IN        FID DISK SERIAL "AAAAAAA" "BBBBBBB" (2,10,30);           00001500
ALPHA FILE IN  TTIN 14 (2,8);                                           00001600
ALPHA FILE OUT TTOUT 14 (2,8);                                          00001700
DEFINE    RTT=READ(TTIN(STA)#, WTT=WRITE(TTOUT(STA)#, PRINT=TTOUT(STA)#;00001800
FORMAT IN      F1(X8,*A1),  F2(X8,I*),  F4(X8,A*), F3(X8,A1,A6);        00001900
FORMAT IN      F5(X8,L*);                                               00002000
FORMAT IN      F(8F6.1);                                                00002100
ALPHA          ID1,ID2,AA1,AA2,BB1,BB2,ANOTH;                           00002200
ARRAY          TBUF[0:7];                                               00002300
INTEGER ARRAY  BB[0:30];                                                00002400
FORMAT OUT     FSTR(X8,"{!ENTER YOUR PROB. ID-UP TO 30 CHAR{!~"),       00002500
               FNV(X8,"{!NUMBER OF VARIABLES = ~"),                     00002600
               FNS(X8,"{!NUMBER OF SAMPLES = ~"),                       00002700
               FSUPR(X8,"{!SUMS TO BE PRINTED TRUE OR FALSE  ~"),       00002800
               FCRPRPR(X8,"{!CROSS-PRODUCT DEVIATIONS TO BE PRINTED TRUE00002900
 OR FALSE{!~"),                                                         00003000
               FVACOVPR(X8,"{!VAR-COVAR MATRIX TO BE PRINTED TRUE OR FAL00003100
SE ~"),                                                                 00003200
               TITL(X8,"{!!!CORRELATION ANALYSIS{!!!~"),                00003300
               FRM3(X9,"{!!SUMS{!!~"),                                  00003400
               FRM3A(X9,"{!!SUMS OF SQUARES{!!~"),                      00003500
               FRM4(X8,4F12.2"{!~"),                                    00003600
               FRM5(X9,"{!!MEANS{!!~"),                                 00003700
               FRM6(X9,"{!!CROSS PRODUCT DEVIATIONS{!!~"),              00003800
               FRM7(X16,"!!COL.",3(X8,"COL."),"{!!~"),                  00003900
               FRM7A(X16,I3,3(X9,I3),"{!~"),                            00004000
               FRM7B(X9,"ROW{!~"),                                      00004100
               FRM8(X9,"{!!STANDARD DEVIATIONS{!!~"),                   00004200
               FRM9(X9,"{!!VARIANCE-COVARIANCE MATRIX{!!~"),            00004300
               FRM10(X9,"{!!CORRELATION MATRIX{!!~"),                   00004400
               FCRLF(X8,"{!~"),                                         00004500
               FNXT(X8,"{!IF YOU WANT TO DO ANOTHER SET TYPE YES{!~"),  00004600
               FRM11(X8,I4,F11.4,3F12.4,"{!~"),                         00004700
               FDN(X8,"{!ENTER DATA NAMES~"),                           00004800
               FPX(X8,"{!PREFIX = ~"),                                  00004900
               FSX(X8,"{!SUFFIX = ~");                                  00005000
PROCEDURE      BLANKIT(XX);                                             00005100
ALPHA          XX;                                                      00005200
          BEGIN                                                         00005300
            IF XX.[12:6]="~" THEN XX.[12:36]~"      " ELSE              00005400
            IF XX.[18:6]="~" THEN XX.[18:30]~"     " ELSE               00005500
            IF XX.[24:6]="~" THEN XX.[24:24]~"    " ELSE                00005600
            IF XX.[30:6]="~" THEN XX.[30:18]~"   " ELSE                 00005700
            IF XX.[36:6]="~" THEN XX.[36:12]~"  " ELSE                  00005800
            IF XX.[42:6]="~" THEN XX.[42:06]~" " ;                      00005900
          END OF BLANKIT;                                               00006000
INTEGER STREAM PROCEDURE FINDGP(B);                                     00006100
          BEGIN                                                         00006200
LABEL          HERE;                                                    00006300
               SI~B; 40(IF SC="~" THEN JUMP OUT 1 TO HERE;              00006400
               SI~SI+1; TALLY~TALLY+1);                                 00006500
HERE:          FINDGP~TALLY;                                            00006600
          END;                                                          00006700
LABEL          START,LL1;                                               00006800
START:    BEGIN                                                         00006900
LIST           HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NVAR,NSAM,SUM,SSQ,00007000
                    VAR) ;                                              00007100
               STA~STATUS(TBUF[*]);                                     00007200
               STA.[9:9]~TBUF[0].[9:9];                                 00007300
               WTT,FSTR);  RTT,8,TBUF[*]);                              00007400
               NCHAR~FINDGP(TBUF[1]);                                   00007500
               NKAR~NCHAR-1;                                            00007600
              READ(TBUF[*],F1,NCHAR,FOR I~0 STEP 1 UNTIL NKAR DO BB[I]);00007700
               WTT,FNV); RTT,4,TBUF[*]);                                00007800
               NCHAR~FINDGP(TBUF[1]);                                   00007900
               READ(TBUF[*],F2,NCHAR,NVAR);                             00008000
               WTT,FNS); RTT,4,TBUF[*]);                                00008100
               NCHAR~FINDGP(TBUF[1]);                                   00008200
               READ(TBUF[*],F2,NCHAR,NSAM);                             00008300
               WTT,FSUPR); RTT,4,TBUF[*]);                              00008400
               NCHAR~FINDGP(TBUF[1]);                                   00008500
               READ(TBUF[*],F5,NCHAR,SUM);                              00008600
               WTT,FCRPRPR); RTT,4,TBUF[*]);                            00008700
               NCHAR~FINDGP(TBUF[1]);                                   00008800
               READ(TBUF[*],F5,NCHAR,SSQ);                              00008900
               WTT,FVACOVPR); RTT,4,TBUF[*]);                           00009000
               NCHAR~FINDGP(TBUF[1]);                                   00009100
               READ(TBUF[*],F5,NCHAR,VAR);                              00009200
          END ;                                                         00009300
LL1:      BEGIN                                                         00009400
INTEGER        K,L,M,II,JJ,KK,LL ;                                      00009500
INTEGER ARRAY  NN[0:8] ;                                                00009600
ARRAY          X[0:NVAR],SX[0:NVAR],SXX[0:NVAR],SXY[0:NVAR,0:NVAR] ;    00009700
ARRAY          SXSQ[0:NVAR];                                            00009800
LABEL          L1,L2,LYST,TRIAGAIN;                                     00009900
LIST           OUTT (FOR J ~ 0 STEP 1 UNTIL 4 DO ID[J],NVAR,NSAM),      00010000
               DATR (FOR I ~ 1 STEP 1 UNTIL NVAR DO X[I]),              00010100
               SUMS (FOR J ~ 1 STEP 1 UNTIL NVAR DO SX[J]),             00010200
               SUMSQ(FOR J~1 STEP 1 UNTIL NVAR DO SXSQ[J]),             00010300
               CAPS (NN[1],FOR JJ ~ 2 STEP 1 UNTIL LL DO NN[JJ]),       00010400
               ARAY (II,SXY[II,K],FOR JJ ~ K+1 STEP 1 UNTIL KK DO       00010500
                    SXY[II,JJ]) ;                                       00010600
FILE           TAPEA DISK SERIAL [1:100] (2,NVAR+1);                    00010700
               WTT,FDN);                                                00010800
TRIAGAIN:      WTT,FPX); RTT,F3,AA1,AA2);                               00010900
               ID1.[6:6]~AA1;  ID1.[12:36]~AA2;                         00011000
               BLANKIT(ID1);                                            00011100
               WTT,FSX); RTT,F3,BB1,BB2);                               00011200
               ID2.[6:6]~BB1;  ID2.[12:36]~BB2;                         00011300
               BLANKIT(ID2);                                            00011400
               FILL FID WITH ID1,ID2;                                   00011500
               SEARCH(FID,BB[*]);                                       00011600
               IF BB[0]<2 THEN                                          00011700
          BEGIN                                                         00011800
               WTT,<X8,"{!YOUR FILE NOT FOUND. TRY AGAIN.{!~">);        00011900
               GO TO TRIAGAIN;                                          00012000
          END;                                                          00012100
               WTT,TITL);                                               00012200
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00012300
          BEGIN                                                         00012400
               SX[I] ~ 0.0 ;                                            00012500
               SXX[I] ~ 0.0 ;                                           00012600
               FOR J ~ 1 STEP 1 UNTIL NVAR DO                           00012700
               SXY[I,J] ~ 0.0                                           00012800
          END ;                                                         00012900
               FOR J ~ 1 STEP 1 UNTIL NSAM DO                           00013000
          BEGIN                                                         00013100
               READ(FID,F,DATR);                                        00013200
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00013300
          BEGIN                                                         00013400
               SX[I] ~ SX[I] + X[I];                                    00013500
               SXSQ[I] ~ SXSQ[I] + X[I]|X[I];                           00013600
          END ;                                                         00013700
               WRITE(TAPEA,NVAR+1,X[*])                                 00013800
          END ;                                                         00013900
               REWIND(TAPEA) ;                                          00014000
               IF SUM THEN                                              00014100
          BEGIN                                                         00014200
               WTT,FRM3);                                               00014300
               WTT,FRM4,SUMS);                                          00014400
               WTT,FRM3A);                                              00014500
               WTT,FRM4,SUMSQ);                                         00014600
          END ;                                                         00014700
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00014800
               X[I] ~ SX[I] / NSAM ;                                    00014900
               WTT,FRM5);                                               00015000
               WTT,FRM4,DATR);                                          00015100
               FOR I ~ 1 STEP 1 UNTIL NSAM DO                           00015200
          BEGIN                                                         00015300
               READ (TAPEA,NVAR+1,SX[*]) ;                              00015400
               FOR J ~ 1 STEP 1 UNTIL NVAR DO                           00015500
          BEGIN                                                         00015600
               SX[J] ~ SX[J] - X[J] ;                                   00015700
               SXX[J] ~ SXX[J] + SX[J]|SX[J] ;                          00015800
               FOR K ~ 1 STEP 1 UNTIL NVAR DO                           00015900
               SXY[J,K] ~ SXY[J,K] + SX[J]|SX[K]                        00016000
          END                                                           00016100
          END ;                                                         00016200
                                                                        00016300
               IF SSQ THEN                                              00016400
          BEGIN                                                         00016500
               WTT,FRM6);                                               00016600
               M ~ 1 ;                                                  00016700
               GO TO LYST                                               00016800
          END ;                                                         00016900
L1:            WTT,FRM8);                                               00017000
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00017100
               X[I] ~ SQRT(SXX[I] / (NSAM-1.0)) ;                       00017200
               WTT,FRM4,DATR);                                          00017300
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00017400
               FOR J ~ 1 STEP 1 UNTIL NVAR DO                           00017500
               SXY[I,J] ~ SXY[I,J] / (NSAM - 1.0) ;                     00017600
               IF VAR THEN                                              00017700
          BEGIN                                                         00017800
               WTT,FRM9);                                               00017900
               M ~ 2 ;                                                  00018000
               GO TO LYST                                               00018100
          END ;                                                         00018200
L2:            WTT,FRM10);                                              00018300
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00018400
               FOR J ~ 1 STEP 1 UNTIL NVAR DO                           00018500
               SXY[I,J] ~ SXY[I,J] / (X[I]|X[J]) ;                      00018600
               M ~ 3 ;                                                  00018700
LYST:          K ~ 1 ;                                                  00018800
               IF NVAR < 4 THEN                                         00018900
               KK ~ NVAR                                                00019000
               ELSE                                                     00019100
               KK ~ 4 ;                                                 00019200
               I ~ 1 + (NVAR - 1) DIV 4 ;                               00019300
               FOR J ~ 1 STEP 1 UNTIL I DO                              00019400
          BEGIN                                                         00019500
               LL ~ KK - K + 1 ;                                        00019600
               L ~ 0 ;                                                  00019700
               FOR JJ ~ K STEP 1 UNTIL KK DO                            00019800
          BEGIN                                                         00019900
               L ~ L + 1 ;                                              00020000
               NN[L] ~ JJ                                               00020100
          END ;                                                         00020200
               WTT,FRM7);                                               00020300
          BEGIN                                                         00020400
               WTT,FRM7A,CAPS);                                         00020500
               IF (JJ+1)}LL THEN                                        00020600
               WTT,FCRLF);                                              00020700
               WTT,FRM7B);                                              00020800
          END;                                                          00020900
               FOR II ~ 1 STEP 1 UNTIL NVAR DO                          00021000
          BEGIN                                                         00021100
               WTT,FRM11,ARAY);                                         00021200
               IF (K+2)}KK THEN                                         00021300
               WTT,FCRLF);                                              00021400
          END;                                                          00021500
               K ~ KK + 1 ;                                             00021600
               KK ~ K + 3 ;                                             00021700
               IF KK > NVAR THEN                                        00021800
               KK ~ NVAR                                                00021900
          END ;                                                         00022000
               IF M = 1 THEN                                            00022100
               GO TO L1                                                 00022200
               ELSE IF M = 2 THEN                                       00022300
               GO TO L2                                                 00022400
          END ;                                                         00022500
               FOR I~0 STEP 1 UNTIL 5 DO WTT,FCRLF);                    00022600
               WTT,FNXT);                                               00022700
               RTT,<X8,A3>,ANOTH);                                      00022800
               REWIND(FID);                                             00022900
               IF ANOTH = "YES" THEN GO TO START ELSE GO TO NOMO;       00023000
NOMO:     END PROGRAM.                                                  00023100
