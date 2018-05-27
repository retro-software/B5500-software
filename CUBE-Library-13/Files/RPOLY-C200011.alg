          BEGIN                                                         00000100
COMMENT        CUBE LIBRARY NUMBER IS C200011.  THE PROGRAM NAME IS     00000200
               "RPOLY/TTY".  THIS VERSION DATED 5/3/68;                 00000300
INTEGER        I,N,STA,GM,CNTCOM;                                       00000400
ALPHA          CR,LF,LA,COMA;                                           00000500
REAL           E,F, INCRE, G ;                                          00000600
ALPHA FILE IN  TTIN 14 (2,8);                                           00000700
ALPHA FILE OUT TTOUT 14 (2,8);                                          00000800
DEFINE         RTT=READ(TTIN(STA)#, WTT=WRITE(TTOUT(STA)#;              00000900
ARRAY          TBUF,TBUF1[0:7];                                         00001000
ARRAY          A, C, D, ER, EI[0:50] ;                                  00001100
                                                                        00001200
                                                                        00001300
FORMAT         F2(X18,"ROOTS OF THE POLYNOMIAL{!!~"),                   00001400
               FF2(X18,"REAL",X12,"IMAGINARY{!~"),                      00001500
               FFF2(X9,E18.11,X4,E18.11,"{!~"),                         00001600
               F4(X18,"ERROR OF THE POLYNOMIAL{!!~"),                   00001700
               FF4(X18,"REAL",X12,"IMAGINARY{!~"),                      00001800
               FFF4(X9,E18.11,X4,E18.11,"{!~"),                         00001900
               FCRLF(X8,"{!~");                                         00002000
LABEL          TRYAGAIN;                                                00002100
INTEGER STREAM PROCEDURE FINDGM(B,LA);                                  00002200
          BEGIN                                                         00002300
LABEL          HERE ;                                                   00002400
               SI~B; 40(IF SC="~" THEN JUMP OUT 1 TO HERE;              00002500
               SI~SI+1; TALLY~TALLY+1);                                 00002600
HERE:          FINDGM~TALLY;                                            00002700
          END;                                                          00002800
INTEGER STREAM PROCEDURE COUNTCOMMAS(B,NDG,COMA);                       00002900
VALUE          NDG;                                                     00003000
          BEGIN                                                         00003100
               SI~B; DI~COMA; DI~DI+7;                                  00003200
               NDG(IF SC=DC THEN TALLY~TALLY+1; DI~DI-1);               00003300
               COUNTCOMMAS~TALLY;                                       00003400
          END OF COUNTCOMMAS;                                           00003500
STREAM PROCEDURE MOVER(B,T,N);                                          00003600
VALUE          N;                                                       00003700
          BEGIN                                                         00003800
               SI~B; DI~T; DS~N CHR;                                    00003900
          END OF MOVER;                                                 00004000
BOOLEAN STREAM PROCEDURE COMMA(B,NDG);                                  00004100
VALUE          NDG;                                                     00004200
          BEGIN                                                         00004300
               SI~B;  SI~SI+NDG;  IF SC="," THEN TALLY~1;               00004400
               COMMA~TALLY;                                             00004500
          END OF COMMA;                                                 00004600
STREAM PROCEDURE PUTONEIN(B,NDG,COMA);                                  00004700
VALUE          NDG;                                                     00004800
          BEGIN                                                         00004900
               DI~B; SI~COMA; SI~SI+7; DI~DI+NDG;                       00005000
               DS~1 CHR;                                                00005100
          END OF PUTONEIN;                                              00005200
                                                                        00005300
PROCEDURE RPOLY(E, F, INCRE, G, N, A, C, D, ER, EI) ;                   00005400
                                                                        00005500
COMMENT        PROCEDURE RPOLY FINDS THE ROOTS OF A POLYNOMIAL, THE     00005600
               COEFFICIENTS OF WHICH ARE REAL NUMBERS.                  00005700
                                                                        00005800
               R. D. RODMAN,                                            00005900
               (PROFESSIONAL SERVICES DIVISIONAL GROUP)                 00006000
                                                                        00006100
               CARD SEQUENCE BEGINS WITH RPLY0000                       00006200
               FIRST RELEASE DATE 07/01/64 ;                            00006300
                                                                        00006400
VALUE          E, F, INCRE, G, N ;                                      00006500
INTEGER        N ;                                                      00006600
REAL           E, F, INCRE, G ;                                         00006700
REAL ARRAY     A, C, D, ER, EI[0] ;                                     00006800
                                                                        00006900
          BEGIN                                                         00007000
INTEGER        I, N1, RCOUNT, K, L ;                                    00007100
REAL           H, U, V, W, R, S, PR, PS, UMIN, VMIN, T, T1, T2, T3, P,Q;00007200
LABEL          S9, START, EXIT, ROOT ;                                  00007300
ARRAY          B[0:N] ;                                                 00007400
                                                                        00007500
PROCEDURE      ORPOL(N, A, X, Y, T, IT) ;                               00007600
VALUE          N, X, Y ;                                                00007700
INTEGER        N ;                                                      00007800
REAL           X, Y, T, IT ;                                            00007900
REAL ARRAY     A[0] ;                                                   00008000
                                                                        00008100
          BEGIN                                                         00008200
REAL           P ;                                                      00008300
INTEGER        I ;                                                      00008400
                                                                        00008500
               T ~ A[N] ; IT ~ 0 ;                                      00008600
               FOR I ~ N-1 STEP -1 UNTIL 0 DO                           00008700
          BEGIN                                                         00008800
               P~ T | X - IT | Y + A[I] ;                               00008900
               IT ~ IT | X + T | Y ;                                    00009000
               T ~ P                                                    00009100
          END                                                           00009200
          END ;                                                         00009300
               FOR I ~ 0 STEP 1 UNTIL N DO B[I] ~ A[I] ; N1 ~ N ;       00009400
               RCOUNT ~ 1 ;                                             00009500
                                                                        00009600
START:         H ~ INCRE ; U ~ E ; V ~ F ;                              00009700
                                                                        00009800
S9:            W ~ 9.9@60 ;                                             00009900
               R ~ U-H ;                                                00010000
               FOR K ~ 1 STEP 1 UNTIL 3 DO                              00010100
          BEGIN                                                         00010200
               S ~ V-H ;                                                00010300
               FOR L ~ 1 STEP 1 UNTIL 3 DO                              00010400
          BEGIN                                                         00010500
               ORPOL(N1, B, R, S, PR, PS) ;                             00010600
               IF (ABS(PR) + ABS(PS)) < W THEN                          00010700
          BEGIN                                                         00010800
               UMIN ~ R ; VMIN ~ S ; W ~ ABS(PR) + ABS(PS)              00010900
          END ;                                                         00011000
               S ~ S+H                                                  00011100
          END ;                                                         00011200
               R ~ R+H                                                  00011300
          END;                                                          00011400
               IF ABS(U-UMIN) < (1.0@-10) | ABS(U) AND                  00011500
               ABS(V-VMIN) < (1.0@-10) | ABS(V) THEN                    00011600
          BEGIN                                                         00011700
               IF SQRT(U*2 + V*2) | 1.0@-12 } H THEN GO TO ROOT ;       00011800
               H ~ H/G ;                                                00011900
               GO TO S9                                                 00012000
          END ;                                                         00012100
               U ~ UMIN ; V ~ VMIN ; GO TO S9 ;                         00012200
                                                                        00012300
ROOT:          IF ABS(V) < H|1000 THEN V ~ 0 ;                          00012400
               C[RCOUNT] ~ U ; D[RCOUNT] ~ V ;                          00012500
               ORPOL(N, A,U, V,ER[RCOUNT],EI[RCOUNT]) ;                 00012600
               RCOUNT ~ RCOUNT + 1 ;                                    00012700
               IF V ! 0 THEN                                            00012800
          BEGIN                                                         00012900
               C[RCOUNT] ~ U ; D[RCOUNT] ~ -V ;                         00013000
               ORPOL(N, A,U,-V,ER[RCOUNT],EI[RCOUNT]) ;                 00013100
               RCOUNT ~ RCOUNT + 1                                      00013200
          END ;                                                         00013300
               IF RCOUNT > N THEN GO TO EXIT ;                          00013400
               IF V = 0 THEN                                            00013500
          BEGIN                                                         00013600
               T ~ B[N1-1] ;                                            00013700
               B[N1-1] ~ B[N1] ;                                        00013800
               FOR I ~ N1-2 STEP -1 UNTIL 0 DO                          00013900
          BEGIN                                                         00014000
               S ~ T + U | B[I+1] ;                                     00014100
               T ~ B[I] ; B[I] ~ S                                      00014200
          END ;                                                         00014300
               N1 ~ N1-1 ;                                              00014400
          END                                                           00014500
               ELSE                                                     00014600
          BEGIN                                                         00014700
               P ~ -2 | U ; Q ~ U*2 + V*2 ;                             00014800
               T1 ~ B[N1-2] ; T2 ~ B[N1-3] ;                            00014900
               B[N1-2] ~ B[N1] ;                                        00015000
               B[N1-3] ~ B[N1-1] - P | B[N1-2] ;                        00015100
               FOR I ~ N1-4 STEP -1 UNTIL 0 DO                          00015200
          BEGIN                                                         00015300
               T3 ~ B[I] ;                                              00015400
               B[I] ~ T1 - P | B[I+1] - Q | B[I+2] ;                    00015500
               T1 ~ T2 ; T2 ~ T3                                        00015600
          END ;                                                         00015700
               N1 ~ N1-2                                                00015800
          END ;                                                         00015900
               GO TO START ;                                            00016000
                                                                        00016100
EXIT:     END ;                                                         00016200
                                                                        00016300
               STA~STATUS(TBUF[*]);                                     00016400
               STA.[9:9]~TBUF[0].[9:9];                                 00016500
               CR~47;  LF~60;  LA~31;  COMA~58;                         00016600
TRYAGAIN:      WTT,<X8,"ALL NUMBERS ENTERED MUST BE FOLLOWED BY A COMMA{00016700
!~">);                                                                  00016800
               WTT,<X8," ENTER THE NUMBER OF COEFFICIENTS{!~">);        00016900
               WTT,FCRLF);                                              00017000
               READ(TTIN(STA),8,TBUF[*]);                               00017100
               WTT,FCRLF);                                              00017200
               GM~FINDGM(TBUF[1],LA);                                   00017300
               IF NOT COMMA(TBUF[1],GM-1) THEN                          00017400
          BEGIN                                                         00017500
               PUTONEIN(TBUF[1],GM,COMA);                               00017600
               GM~GM+1;                                                 00017700
          END;                                                          00017800
               MOVER(TBUF[1],TBUF1[0],GM);                              00017900
               READ(TBUF1[*],/,N);                                      00018000
               WTT,<X8," ENTER THE COEFFICIENTS{!~">);                  00018100
               WTT,FCRLF);                                              00018200
               READ(TTIN(STA),8,TBUF[*]);                               00018300
               WTT,FCRLF);                                              00018400
               GM~FINDGM(TBUF[1],LA);                                   00018500
               IF NOT COMMA(TBUF[1],GM-1) THEN                          00018600
          BEGIN                                                         00018700
               PUTONEIN(TBUF[1],GM,COMA);                               00018800
               GM~GM+1;                                                 00018900
          END;                                                          00019000
               MOVER(TBUF[1],TBUF1[0],GM);                              00019100
               READ(TBUF1[*],/,FOR I~0 STEP 1 UNTIL N DO A[I]);         00019200
               CNTCOM~COUNTCOMMAS(TBUF1[0],GM,COMA)-1;                  00019300
               IF CNTCOM  ! N   THEN                                    00019400
          BEGIN                                                         00019500
               WTT,<X8,"TYPING ERROR. PLEASE TRY AGAIN{!~">);           00019600
               GO TO TRYAGAIN;                                          00019700
          END;                                                          00019800
               E ~ F ~ 0.5 ; INCRE ~ 1 ; G ~ 5 ;                        00019900
               RPOLY(E, F, INCRE, G, N, A, C, D, ER, EI) ;              00020000
               WTT,FCRLF);                                              00020100
               WTT,F2); WTT,FF2);                                       00020200
               WTT,FFF2,FOR I~1 STEP 1 UNTIL N DO  [C[I],D[I]]);        00020300
               WTT,FCRLF);                                              00020400
               WTT,F4); WTT,FF4);                                       00020500
               WTT,FFF4,FOR I~1 STEP 1 UNTIL N DO [ER[I], EI[I]]);      00020600
          END  .                                                        00020700
