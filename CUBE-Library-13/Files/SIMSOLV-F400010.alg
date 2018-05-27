          BEGIN                                                         00000100
COMMENT        CUBE LIBRARY NUMBER IS F400010.  THE PROGRAM NAME IS     00000200
               "SIM/SOLVE".  THIS VERSION DATED 5/3/68;                 00000300
COMMENT        MICHAEL MALCOLM, SIMULTANEOUS EQUATION SOLVER;           00000400
ALPHA FILE IN  TTIN 14(1,8);                                            00000500
ALPHA FILE OUT TTOUT 14(1,8);                                           00000600
FORMAT         F1(X8,2A1,"HOW MANY EQUATIONS DO YOU HAVE.",3A1),        00000700
               F2(X8,2A1,"TYPE IN COEFFICIENT MATRIX",3A1),             00000800
               F3(X8,2A1,"SEPARATE EACH VALUE BY A COMMA",2A1),         00000900
               F4(X8,2A1,"USE AS MANY LINES AS YOU NEED ",2A1),         00001000
               F5(X8,2A1,"BUT START EACH ROW ON A NEW LINE.",4A1),      00001100
               F6(X8,2A1,"TYPE IN RIGHT HAND SIDE TOP TO BOTTOM",3A1),  00001200
               F7(X8,2A1,"SOLUTION:",3A1),                              00001300
               F8(X8,2A1,"T[",I1,"]=",E15.8,2A1),                       00001400
               F9(X8,2A1,"T[",I2,"]=",E15.8,2A1),                       00001500
               F10(X8,2A1,"THE COEFFICIENT MATRIX IS SINGULAR",3A1),    00001600
               F11(X8,2A1,"ACCURACY NOT ACHIEVED",3A1),                 00001700
               F12(X8,2A1,"IS THIS ALL FOR NOW.",3A1);                  00001800
INTEGER        N,STA,I,J;                                               00001900
REAL           E;                                                       00002000
ALPHA          CR,LF,LA,DCIM,COMA;                                      00002100
ARRAY          RD[0:7],CO[0:25,0:25],RHS,T[0:25];                       00002200
SAVE ARRAY     RD1,RD2[0:11];                                           00002300
LABEL          SING,MORE,DUNN,RANA,MAYBE;                               00002400
DEFINE         CRLFIT=CR,LF,CR,LF,LA#,                                  00002500
               CRLF=CR,LF,CR,LA#;                                       00002600
BOOLEAN        TRI,USED;                                                00002700
                                                                        00002800
PROCEDURE      SOLVE(N,A,C,RSW,E,K1,EPS,X,E1,E2);                       00002900
VALUE          N,RSW,E,K1,EPS;                                          00003000
INTEGER        N,K1;                                                    00003100
REAL           E,EPS;                                                   00003200
BOOLEAN        RSW;                                                     00003300
REAL ARRAY     A[0,0], C    ,X[0];                                      00003400
LABEL          E1,E2;                                                   00003500
          BEGIN                                                         00003600
INTEGER        I,J,K,J1,K2,L;                                           00003700
REAL           BIG,TEMP,DIAG,NORM,Q;                                    00003800
OWN INTEGER ARRAY F[0:N];                                               00003900
REAL ARRAY     D, RC, CC[0:N];                                          00004000
OWN REAL ARRAY B[0:N,0:N];                                              00004100
LABEL          S1, S2, S3, S4, S5, S6, REP, S7, S8, S9 , IT1, S10, S11, 00004200
               S12, S13, S14, S15, EXIT;                                00004300
S1:            IF RSW THEN                                              00004400
               GO TO REP;                                               00004500
COMMENT        THE COEFFICIENT MATRIX IS TRIANGULARIZED;                00004600
               FOR I:= 1 STEP 1 UNTIL N DO                              00004700
               FOR J:= 1 STEP 1 UNTIL N DO                              00004800
               B[I,J]:= A[I,J];                                         00004900
S2:            FOR I:= 1 STEP 1 UNTIL N DO                              00005000
          BEGIN                                                         00005100
               L:= I - 1;                                               00005200
               FOR J ~ I STEP 1 UNTIL N DO                              00005300
          BEGIN                                                         00005400
               Q:= 0;                                                   00005500
               FOR K:= 1 STEP 1 UNTIL L DO Q:= B[J,K] | B[K,I] + Q;     00005600
               B[J,I] :=B[J,I] - Q                                      00005700
          END;                                                          00005800
               BIG:= 0;  K2:= I;                                        00005900
S3:            FOR K:= I STEP 1 UNTIL N DO                              00006000
          BEGIN                                                         00006100
               IF ABS(B[K,I]) GTR BIG THEN                              00006200
          BEGIN                                                         00006300
               BIG:= ABS(B[K,I]); K2:= K                                00006400
          END                                                           00006500
          END;                                                          00006600
COMMENT        E1 IS THE NON-LOCAL LABEL TO WHICH AN EXIT IS MADE IF THE00006700
               COEFFICIENT MATRIX IS SINGULAR. ;                        00006800
S4:            IF BIG LEQ EPS THEN GO TO E1; F[I]:= K2;                 00006900
               IF K2 NEQ I THEN                                         00007000
S5:            FOR K:= 1 STEP 1 UNTIL N DO                              00007100
          BEGIN                                                         00007200
               TEMP:= A[K2,K]; A[K2,K]:= A[I,K]; A[I,K]:= TEMP;         00007300
               TEMP:= B[K2,K]; B[K2,K]:= B[I,K]; B[I,K]:= TEMP;         00007400
          END;                                                          00007500
               DIAG:= B[I,I];                                           00007600
S6:            FOR J:= I+1 STEP 1 UNTIL N DO                            00007700
          BEGIN                                                         00007800
               Q:= 0;                                                   00007900
               FOR K:= 1 STEP 1 UNTIL L DO Q:= B[I,K] | B[K,J] + Q;     00008000
               B[I,J]:= (B[I,J] - Q)/DIAG                               00008100
          END                                                           00008200
          END;                                                          00008300
COMMENT        THE REDUCED "C" VECTOR IS COMPUTED. ;                    00008400
REP:           FOR I:= 1 STEP 1 UNTIL N DO                              00008500
          BEGIN                                                         00008600
               TEMP:= C[F[I]]; C[F[I]]:= C[I]; D[I]:= C[I]:= TEMP       00008700
          END;                                                          00008800
COMMENT        THE BACKWARD PASS,GIVING THE DESIRED SOLUTION,           00008900
               IS EXECUTED. ;                                           00009000
               FOR I:= 1 STEP 1 UNTIL N DO                              00009100
          BEGIN                                                         00009200
               L:= I - 1; Q:= 0;                                        00009300
S7:            FOR K:= 1 STEP 1 UNTIL L DO Q:= B[I,K] | D[K] + Q;       00009400
               D[I]:= (D[I] -  Q)/B[I,I]                                00009500
          END;                                                          00009600
S8:            FOR I:= N STEP -1 UNTIL 1 DO                             00009700
          BEGIN                                                         00009800
               Q:= 0;                                                   00009900
               FOR K:= I+1 STEP 1 UNTIL N DO Q:= B[I,K] | X[K] + Q;     00010000
               X[I]:= D[I] - Q                                          00010100
          END;                                                          00010200
S9:            IF E = 0 THEN GO TO EXIT;                                00010300
COMMENT        THE SOLUTION IS ITERATED AND TESTED FOR ACCURACY. ;      00010400
               J1:= 0;                                                  00010500
IT1:           IF J1 GEQ K1 THEN GO TO E2;                              00010600
               NORM:= 0;                                                00010700
               FOR I:= 1 STEP 1 UNTIL N DO                              00010800
          BEGIN                                                         00010900
               Q:= 0; L:= I - 1;                                        00011000
S10:           FOR K:= 1 STEP 1 UNTIL N DO Q:= A[I,K] | X[K] + Q ;      00011100
               D[I]:= C[I] - Q;                                         00011200
S11:           NORM:= ABS(D[I]) + NORM ;                                00011300
               Q:= 0;                                                   00011400
S12:           FOR K:= 1 STEP 1 UNTIL L DO Q:= B[I,K] | D[K] + Q ;      00011500
               D[I]:= (D[I]-Q)/B[I,I]                                   00011600
          END;                                                          00011700
               FOR I:= N STEP -1 UNTIL 1 DO                             00011800
          BEGIN                                                         00011900
               Q:= 0;                                                   00012000
S13:           FOR K:= I+1 STEP 1 UNTIL N DO Q:= B[I,K] | D[K] + Q;     00012100
               X[I]:= X[I] + D[I] - Q                                   00012200
          END;                                                          00012300
S14:           J1:= J1 + 1;                                             00012400
S15:           IF N | E LSS NORM THEN GO TO IT1;                        00012500
EXIT:     END;                                                          00012600
                                                                        00012700
INTEGER STREAM PROCEDURE FINDGP(B,LA);                                  00012800
          BEGIN                                                         00012900
COMMENT        THE NUMBER OF CHARACTERS PRECEDING THE GROUP MARK IS     00013000
               FOUND;                                                   00013100
LABEL          FOUNDIT;                                                 00013200
               DI~LA;  DI~DI+7;                                         00013300
               SI:=B;   63(IF SC=DC THEN JUMP OUT 1 TO FOUNDIT;         00013400
               DI:=DI-1; TALLY:= TALLY +1);                             00013500
FOUNDIT:       FINDGP:=TALLY;                                           00013600
          END  OF FINDGP;                                               00013700
                                                                        00013800
BOOLEAN STREAM PROCEDURE DECIMAL(B,NDG,DCIM);                           00013900
VALUE          NDG;                                                     00014000
          BEGIN                                                         00014100
COMMENT        TRUE IF THE NUMBER CONTAINS A DECIMAL POINT;             00014200
               SI:=B; DI:=DCIM;  DI:=DI+7;                              00014300
               NDG(IF SC=DC THEN TALLY := 1; DI:=DI-1);                 00014400
               DECIMAL:=TALLY;                                          00014500
          END OF DECIMAL;                                               00014600
                                                                        00014700
STREAM PROCEDURE PUTITIN(B,DCIM,NDG);                                   00014800
VALUE          NDG;                                                     00014900
          BEGIN                                                         00015000
COMMENT        PUTS DECIMAL IN AFTER INTEGER PART;                      00015100
               DI:=B;  SI:=DCIM;  SI:=SI+7;  DI:=DI+NDG;                00015200
               DS:=1 CHR; DS:= 20 LIT "0";                              00015300
          END OF PUTITIN;                                               00015400
                                                                        00015500
STREAM PROCEDURE MOVE(F,T,N);                                           00015600
VALUE          N;                                                       00015700
          BEGIN                                                         00015800
COMMENT        PREPARES T TO BE READ IN AN R20.10 FORMAT;               00015900
               SI:=F;DI:=T;DS:=N CHR;DS:=20 LIT "0";                    00016000
          END  OF MOVE;                                                 00016100
                                                                        00016200
STREAM PROCEDURE PUTONEIN(B,NDG,COMA);                                  00016300
VALUE          NDG;                                                     00016400
          BEGIN                                                         00016500
               DI:=B; SI:=COMA; SI:=SI+7; DI:=DI+NDG;                   00016600
               DS:=1 CHR;                                               00016700
          END OF PUTONEIN     ;                                         00016800
                                                                        00016900
INTEGER STREAM PROCEDURE COUNTCOMMAS(B,NDG,COMA);                       00017000
VALUE          NDG;                                                     00017100
          BEGIN                                                         00017200
               SI:=B; DI:=COMA; DI:=DI+7;                               00017300
               NDG(IF SC=DC THEN TALLY:=TALLY+1; DI:=DI-1);             00017400
               COUNTCOMMAS:=TALLY;                                      00017500
          END  OF COUNTCOMMAS;                                          00017600
                                                                        00017700
BOOLEAN PROCEDURE YES;                                                  00017800
          BEGIN                                                         00017900
FORMAT         FIN(X8,A2),                                              00018000
               FOUT(X8,2A1,"PLEASE TYPE YES OR NO."3A1);                00018100
ALPHA          YESNO;                                                   00018200
LABEL          GOOFED;                                                  00018300
GOOFED:        READ(TTIN(STA),FIN,YESNO);                               00018400
               IF YESNO = "YE" THEN YES := TRUE ELSE                    00018500
               IF YESNO = "NO" THEN YES := FALSE ELSE                   00018600
          BEGIN                                                         00018700
               WRITE(TTOUT(STA),FOUT,CRLFIT);                           00018800
               GO TO GOOFED                                             00018900
          END;                                                          00019000
          END OF YES;                                                   00019100
                                                                        00019200
BOOLEAN STREAM PROCEDURE COMMA(B,NDG);                                  00019300
VALUE          NDG;                                                     00019400
          BEGIN                                                         00019500
               SI:=B; SI:=SI+NDG; IF SC="," THEN TALLY:=1;              00019600
               COMMA:=TALLY;                                            00019700
          END OF COMMA;                                                 00019800
                                                                        00019900
STREAM PROCEDURE MOVER(B,T,N);                                          00020000
VALUE          N;                                                       00020100
          BEGIN                                                         00020200
               SI:=B; DI:=T; DS:=N CHR;                                 00020300
          END OF MOVER;                                                 00020400
                                                                        00020500
PROCEDURE      READVAL(V,STATION);                                      00020600
VALUE          STATION;                                                 00020700
INTEGER        STATION;                                                 00020800
REAL           V;                                                       00020900
          BEGIN                                                         00021000
COMMENT        THE VALUE OF V IS READ FROM THE REMOTE STATION THRU THE  00021100
               USE OF A MOVE AND EDIT;                                  00021200
INTEGER        GP;                                                      00021300
FORMAT         F1(R20.10),                                              00021400
               FSO(X8,2A1,"TYPING ERROR.  START OVER",3A1);             00021500
LABEL          RER,DUNN,TRYAGAIN;                                       00021600
TRYAGAIN:      READ(TTIN(STATION),8,RD1[*]);                            00021700
               GP:= FINDGP(RD1[1],LA);                                  00021800
               IF NOT DECIMAL(RD1[1],GP,DCIM) THEN                      00021900
          BEGIN                                                         00022000
               PUTITIN(RD1[1],DCIM,GP);                                 00022100
          GP := GP + 1;                                                 00022200
          END;                                                          00022300
               MOVE(RD1[1],RD2[0],GP);                                  00022400
               READ(RD2[*],F1,V)[:RER];  GO TO DUNN;                    00022500
RER:           WRITE(TTOUT(STATION),FSO,CR,LF,LF,CR,LA);                00022600
               GO TO TRYAGAIN;                                          00022700
DUNN:     END  OF READVAL;                                              00022800
                                                                        00022900
PROCEDURE      READROW;                                                 00023000
          BEGIN                                                         00023100
INTEGER        GP,CCM,J,K;                                              00023200
LABEL          MORE,R,ALL;                                              00023300
FORMAT         FS(X8,2A1,"TYPING ERROR,TYPE LINE AGAIN",3A1),           00023400
               FD(X8,3A1);                                              00023500
               J:=1;                                                    00023600
MORE:          READ(TTIN(STA),8,RD1[*]);                                00023700
               WRITE(TTOUT(STA),FD,CR,LF,LA);                           00023800
               GP:= FINDGP(RD1[1],LA);                                  00023900
               IF NOT   COMMA(RD1[1],GP-1) THEN                         00024000
          BEGIN                                                         00024100
               PUTONEIN(RD1[1],GP,COMA);                                00024200
               GP:=GP+1                                                 00024300
          END;                                                          00024400
               CCM:= COUNTCOMMAS(RD1[1],GP,COMA)-1   ;                  00024500
               MOVER(RD1[1],RD2[0],GP);                                 00024600
               READ(RD2[*],/,FOR K:=J STEP 1 UNTIL J+CCM DO CO[I,K])[:R]00024700
              ;J:=K;                                                    00024800
               IF J LEQ N THEN GO TO MORE ELSE GO TO ALL;               00024900
R:             WRITE(TTOUT(STA),FS,CRLFIT);                             00025000
               GO TO MORE;                                              00025100
ALL:      END OF READROW;                                               00025200
                                                                        00025300
               TRI:=FALSE;   USED:=FALSE;                               00025400
               CR:=47; LF:=60; LA:=31; DCIM:=26; COMA:=58;              00025500
               STA~STATUS(RD[*]);                                       00025600
               STA.[9:9]~RD[0].[9:9];                                   00025700
MORE:          WRITE(TTOUT(STA),F1,CRLFIT);                             00025800
               READVAL(N,STA);                                          00025900
               E:=0.5|N|@-10;                                           00026000
               WRITE(TTOUT(STA),F2,CRLFIT);                             00026100
               IF NOT USED THEN                                         00026200
          BEGIN                                                         00026300
               WRITE(TTOUT(STA),F3,CRLF);                               00026400
               WRITE(TTOUT(STA),F4,CRLF);                               00026500
               WRITE(TTOUT(STA),F5,CR,LF,CR,LF,LF,LA);                  00026600
          END;                                                          00026700
               USED:=TRUE;                                              00026800
               FOR I:= 1 STEP 1 UNTIL N DO READROW;                     00026900
               WRITE(TTOUT(STA),F6,CRLFIT);                             00027000
               I:=0; READROW;                                           00027100
               FOR J:=1 STEP 1 UNTIL N DO RHS[J]:= CO[0,J];             00027200
               SOLVE(N,CO,RHS,TRI,0,0,E,T,SING,RANA);                   00027300
               FOR I:=1 STEP 1 UNTIL N DO                               00027400
               IF I LEQ 9 THEN                                          00027500
               WRITE(TTOUT(STA),F8,CR,LF,I,T[I],CR,LA) ELSE             00027600
               WRITE(TTOUT(STA),F9,CR,LF,I,T[I],CR,LA);                 00027700
MAYBE:         WRITE(TTOUT(STA),F12,CRLFIT);                            00027800
               IF YES THEN GO TO DUNN ELSE GO TO MORE;                  00027900
SING:          WRITE(TTOUT(STA),F10,CRLFIT); GO TO MAYBE;               00028000
RANA:          WRITE(TTOUT(STA),F11,CRLFIT); GO TO MAYBE;               00028100
DUNN:     END  OF PROGRAM.                                              00028200
