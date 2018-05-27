          BEGIN                                                         00000100
COMMENT        CUBE LIBRARY NUMBER IS C200010.  PROGRAM NAME IS         00000200
               "CPLOY/TTY".  THIS VERSION DATED 5/3/68;                 00000300
INTEGER        I,N,STA,GM,CNTCOM;                                       00000400
ALPHA FILE IN  TTIN 14 (2,8);                                           00000500
ALPHA FILE OUT TTOUT 14 (2,8);                                          00000600
DEFINE         RTT=READ(TTIN(STA)#, WTT=WRITE(TTOUT(STA)#;              00000700
REAL           E, F, INCRE, G ;                                         00000800
ARRAY          A, C, D, ER, EI[0:50] ;                                  00000900
ARRAY          Z[0:50] ;                                                00001000
ARRAY          TBUF,TBUF1[0:7];                                         00001100
ALPHA          CR,LF,LA,COMA;                                           00001200
LABEL          TRYAGAIN;                                                00001300
                                                                        00001400
FORMAT         F2(X23,"ROOTS OF THE POLYNOMIAL{!!~"),                   00001500
               FF2(X18,"REAL",X12,"IMAGINARY{!~"),                      00001600
               FFF2(X9,E18.11,X4,E18.11,"{!~"),                         00001700
               F4(X23,"!!!ERROR OF THE POLYNOMIAL{!!~"),                00001800
               FF4(X18,"REAL",X12,"IMAGINARY{!~"),                      00001900
               FFF4(X9,E18.11,X4,E18.11,"{!~"),                         00002000
               FCRLF(X8,"{!~");                                         00002100
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
PROCEDURE CPOLY(E, F, INCRE, G, N, A, Z, C, D, ER, EI) ;                00005400
                                                                        00005500
COMMENT        PROCEDURE CPOLY DETERMINES THE ROOTS OF A POLYNOMIAL,    00005600
               THE COEFFICIENTS OF WHICH ARE COMPLEX NUMBERS.           00005700
                                                                        00005800
               R. D. RODMAN,                                            00005900
               (PROFESSIONAL SERVICES DIVISIONAL GROUP)                 00006000
                                                                        00006100
               CARD SEQUENCE BEGINS WITH CPLY0000                       00006200
               FIRST RELEASE DATE 07/01/64 ;                            00006300
                                                                        00006400
VALUE          E, F, INCRE, G, N  ;                                     00006500
INTEGER        N ;                                                      00006600
REAL           E, F, INCRE, G ;                                         00006700
REAL ARRAY     A, Z, C, D, ER, EI[0] ;                                  00006800
                                                                        00006900
          BEGIN                                                         00007000
INTEGER        I, N1, RCOUNT, K, L ;                                    00007100
REAL           H, U, V, W, R, S, PR, PS, UMIN, VMIN, T, T1, T2, T3, P,Q;00007200
REAL           IT, IS ;                                                 00007300
LABEL          S9, START, EXIT, ROOT ;                                  00007400
ARRAY          B, BZ[0:N] ;                                             00007500
                                                                        00007600
PROCEDURE ORPOL(N, A, Z, X, Y, T, IT) ;                                 00007700
                                                                        00007800
VALUE          N, X, Y ;                                                00007900
INTEGER        N ;                                                      00008000
REAL           X, Y, T, IT ;                                            00008100
REAL ARRAY     A, Z[0] ;                                                00008200
                                                                        00008300
          BEGIN                                                         00008400
REAL           P ;                                                      00008500
INTEGER        I ;                                                      00008600
                                                                        00008700
               T ~ A[N] ; IT ~ Z[N] ;                                   00008800
               FOR I ~ N-1 STEP -1 UNTIL 0 DO                           00008900
          BEGIN                                                         00009000
               P ~ T | X - IT | Y + A[I] ;                              00009100
               IT ~ IT | X + T | Y + Z[I] ;                             00009200
               T ~ P                                                    00009300
          END                                                           00009400
          END  ;                                                        00009500
               FOR I ~ 0 STEP 1 UNTIL N DO                              00009600
          BEGIN                                                         00009700
               B[I] ~ A[I] ; BZ[I] ~ Z[I]                               00009800
          END  ;                                                        00009900
               N1 ~ N ; RCOUNT ~ 1 ;                                    00010000
                                                                        00010100
START:         H ~ INCRE ; U ~ E ; V ~ F ;                              00010200
                                                                        00010300
S9:            W ~ 9.9@60 ;                                             00010400
               R ~ U-H ;                                                00010500
               FOR K ~ 1 STEP 1 UNTIL 3 DO                              00010600
          BEGIN                                                         00010700
               S ~ V-H ;                                                00010800
               FOR L ~ 1 STEP 1 UNTIL 3 DO                              00010900
          BEGIN                                                         00011000
               ORPOL(N1, B, BZ, R, S, PR, PS) ;                         00011100
               IF (ABS(PR) + ABS(PS)) < W THEN                          00011200
          BEGIN                                                         00011300
               UMIN ~ R ; VMIN ~ S ; W ~ ABS(PR) + ABS(PS)              00011400
          END  ;                                                        00011500
               S ~ S+H                                                  00011600
          END  ;                                                        00011700
               R ~ R+H                                                  00011800
          END  ;                                                        00011900
               IF ABS(U-UMIN) < (1.0@-10) | ABS(U) AND                  00012000
               ABS(V-VMIN) < (1.0@-10) | ABS(V) THEN                    00012100
          BEGIN                                                         00012200
               IF SQRT(U*2 + V*2) | 1.0@-12 } H THEN GO TO ROOT ;       00012300
               H ~ H/G ;                                                00012400
               GO TO S9                                                 00012500
          END  ;                                                        00012600
               U ~ UMIN ; V ~ VMIN ; GO TO S9 ;                         00012700
                                                                        00012800
ROOT:          IF ABS(V) < H | 10 THEN V ~ 0 ;                          00012900
               C[RCOUNT] ~ U ; D[RCOUNT] ~ V ;                          00013000
               ORPOL(N, A, Z, U, V, ER[RCOUNT], EI[RCOUNT]) ;           00013100
               RCOUNT ~ RCOUNT + 1 ;                                    00013200
               IF RCOUNT > N THEN GO TO EXIT ;                          00013300
               T ~ B[N1-1] ; IT ~ BZ[N1-1] ;                            00013400
               B[N1-1] ~ B[N1] ; BZ[N1-1] ~ BZ[N1] ;                    00013500
               FOR I ~ N1-2 STEP -1 UNTIL 0 DO                          00013600
          BEGIN                                                         00013700
               S ~ T + U | B[I+1] - V | BZ[I+1] ;                       00013800
               IS ~ IT + V | B[I+1] + U | BZ[I+1] ;                     00013900
               T ~ B[I] ; IT ~ BZ[I] ; B[I] ~ S ; BZ[I] ~ IS ;          00014000
          END  ;                                                        00014100
               N1 ~ N1-1 ;                                              00014200
               GO TO START ;                                            00014300
                                                                        00014400
EXIT:     END  ;                                                        00014500
                                                                        00014600
               STA~STATUS(TBUF[*]);                                     00014700
               STA.[9:9]~TBUF[0].[9:9];                                 00014800
               CR~47;  LF~60;  LA~31;  COMA~58;                         00014900
TRYAGAIN:      WTT,<X8,"ALL NUMBERS ENTERED MUST BE FOLLOWED BY A COMMA{00015000
!~">);                                                                  00015100
               WTT,<X8,"ENTER THE NUMBER OF COEFFICIENTS AND THE REAL{!~00015200
">);                                                                    00015300
               WTT,<X8,"COEFFICIENTS{!!~">);                            00015400
               READ(TTIN(STA),8,TBUF[*]);                               00015500
               WTT,FCRLF);                                              00015600
               GM~FINDGM(TBUF[1],LA);                                   00015700
               IF NOT COMMA(TBUF[1],GM-1) THEN                          00015800
          BEGIN                                                         00015900
               PUTONEIN(TBUF[1],GM,COMA);                               00016000
               GM~GM+1;                                                 00016100
          END;                                                          00016200
               N~N-1;                                                   00016300
               CNTCOM~COUNTCOMMAS(TBUF[1],GM,COMA)-1;                   00016400
               IF CNTCOM  ! N+1 THEN                                    00016500
          BEGIN                                                         00016600
               WTT,<X8,"TYPING ERROR. PLEASE TRY AGAIN{!~">);           00016700
               GO TO TRYAGAIN;                                          00016800
          END;                                                          00016900
               MOVER(TBUF[1],TBUF1[0],GM);                              00017000
               READ(TBUF1[*],/,N,FOR I~0 STEP 1 UNTIL N DO A[I]);       00017100
               WTT,<X8,6F6.1>,N,FOR I~0 STEP 1 UNTIL N DO A[I]);        00017200
               WTT,<X8,"{!ENTER THE NUMBER OF COEFFICIENTS AND THE{!~">)00017300
;                                                                       00017400
               WTT,<X8,"IMAGINARY COEFFICIENTS{!!~">);                  00017500
               READ(TBUF1[*],/,N,FOR I~0 STEP 1 UNTIL N DO Z[I]);       00017600
               WTT,<X8,6F6.1>,N,FOR I~0 STEP 1 UNTIL N DO Z[I]);        00017700
               E ~ F ~ 0.5 ; INCRE ~ 1 ; G ~ 3 ;                        00017800
               CPOLY(E, F, INCRE, G, N, A, Z, C, D, ER, EI) ;           00017900
               WTT,FCRLF);                                              00018000
               WTT,F2); WTT,FF2);                                       00018100
               WTT,FFF2,FOR I~1 STEP 1 UNTIL N DO [C[I],D[I]]);         00018200
               WTT,F4); WTT,FF4);                                       00018300
               WTT,FFF4,FOR I~1 STEP 1 UNTIL N DO [ER[I],EI[I]]);       00018400
          END  .                                                        00018500
