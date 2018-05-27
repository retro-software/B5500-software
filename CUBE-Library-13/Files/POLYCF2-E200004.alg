COMMENT        PROGRAM - POLYNOMIAL CURVE FIT FOR MULT. CASES,          00000100
               CUBE LIBRARY NUMBER IS E200004                           00000200
               THIS VERSION DATED 2/1/67;                               00000300
BEGIN                                                                   00000400
COMMENT  LEAST SQUARE CURVE FIT;                                        00000500
LABEL Q;                                                                00000600
INTEGER J;                                                              00000700
INTEGER I;                                                              00000800
FILE IN FILE1 (1,10);                                                   00000900
FILE OUT F1 1 (1,15);                                                   00001000
INTEGER N,D,CASE;                                                       00001100
FORMAT IN FORMAT2 (I4,X4,I2,X4,I2);                                     00001200
READ (FILE1,FORMAT2,CASE);                                              00001300
Q:                                                                      00001400
READ (FILE1,FORMAT2,N,D);                                               00001500
BEGIN                                                                   00001600
REAL ARRAY AOE [1:D+1];                                                 00001700
REAL ARRAY A[1:(D+1),1:1],X[1:N],Y[1:1,1:N],YY[1:N],YYY[1:N];           00001800
LIST LIST2 (FOR I~1 STEP 1 UNTIL N DO Y[1,I]);                          00001900
FORMAT IN FORMAT1 (3(F15.8,X5));                                        00002000
LIST LIST1 (FOR I~1 STEP 1 UNTIL N DO X[I]);                            00002100
REAL YYYY,STD;                                                          00002200
LIST L3 (FOR I~1 STEP 1 UNTIL (D+1) DO A[I,1]);                         00002300
LIST L1 (FOR I~1 STEP 1 UNTIL N DO [X[I],Y[1,I],YY[I],YYY[I]]);         00002400
LIST L6 (FOR I~1 STEP 1 UNTIL D+1 DO AOE[I]);                           00002500
FORMAT OUT FOR6 ( "A0E=",E15.8,X4,"A1E=",E15.8,X4,"A2E=",E15.8,X4,      00002600
"A3E=",E15.8,X4,"A4E=",E15.8, / "A5E=",E15.8,X4,"A6E=",E15.8,X4,        00002700
"A7E=",E15.8,X4,"A8E=",E15.8,X4,"A9E=",E15.8,// "A10E=",E15.8);         00002800
FORMAT OUT FOR2 ( "A0=",E15.8,X4,"A1=",E15.8,X4,"A2=",E15.8,X4,         00002900
"A3=",E15.8,X4,"A4=",E15.8,X4,//,"A5=",E15.8,X4,"A6=",E15.8,X4,         00003000
"A7=",E15.8,X4,"A8=",E15.8,X4,"A9=",E15.8,X4,//,"A10=",E15.8);          00003100
FORMAT OUT FOR3 (X8,"X",X18,"Y",X18,"YB",X16,"Y-YB",//);                00003200
FORMAT OUT FOR4 ("STANDARD DEVIATION=",E15.8);                          00003300
FORMAT OUT FOR5 (4(E15.8,X4));                                          00003400
REAL ARRAY C[1:(D+1),1:(D+1)];                                          00003500
PROCEDURE LS (N,D,X,Y,A);                                               00003600
VALUE N,D;                                                              00003700
INTEGER N,D;                                                            00003800
REAL ARRAY X[1],Y[1,1],A[1,1];                                          00003900
BEGIN                                                                   00004000
PROCEDURE MATATB (A,B,N1,N2,N4,C);                                      00004100
VALUE N1,N2,N4;                                                         00004200
INTEGER N1,N2,N4;                                                       00004300
REAL ARRAY A,B,C[1,1];                                                  00004400
BEGIN                                                                   00004500
INTEGER I,J,K;                                                          00004600
REAL ARRAY E[1:N2,1:N1];                                                00004700
FOR I~1 STEP 1 UNTIL N1 DO                                              00004800
FOR J~1 STEP 1 UNTIL N2 DO                                              00004900
E[J,I] ~ A[I,J];                                                        00005000
FOR K~1 STEP 1 UNTIL N4 DO                                              00005100
FOR I~1 STEP 1 UNTIL N2 DO BEGIN                                        00005200
C[I,K] ~ 0;                                                             00005300
FOR J~1 STEP 1 UNTIL N1 DO                                              00005400
C[I,K] ~ E[I,J] | B[J,K] + C[I,K] END;                                  00005500
END;                                                                    00005600
PROCEDURE MATTP (A,N1,N2,C);                                            00005700
VALUE N1,N2;                                                            00005800
INTEGER N1,N2;                                                          00005900
REAL ARRAY A,C[1,1];                                                    00006000
BEGIN                                                                   00006100
INTEGER I,J;                                                            00006200
FOR I~1 STEP 1 UNTIL N1 DO                                              00006300
FOR J~1 STEP 1 UNTIL N2 DO                                              00006400
C[J,I] ~ A[I,J];                                                        00006500
END;                                                                    00006600
COMMENT       THIS PROCEDURE WILL COMPUTE THE INVERSE OF A SYMMETRIC    00006700
              MATRIX.  THE PROCEDURE WILL FAIL IF ANY "TRAILING MINOR"  00006800
              IS ZERO.   THIS CONDITION CANNOT EXITS IF THE MATRIX IS   00006900
              "DEFINITE".  UPON ENTERING THE PROCEDURE THE DIAGONAL     00007000
              ELEMENTS OF THE ORIGINAL MATRIX ARE PLACED IN A VECTOR    00007100
              CALLED "DIAGONAL".  THE INVERSE MATRIX IS PLACED IN THE   00007200
              UPPER TRIANGULAR HALF, INCLUDING THE MAIN DIAGONAL, OF    00007300
              THE ORIGINAL ARRAY.                                       00007400
                                                                        00007500
              R.D. RODMAN,                                              00007600
              (PROFESSIONAL SERVICES GROUP),                            00007700
                                                                        00007800
              CARD SEQUENCE STARTS WITH "ISYM0010",                     00007900
              FIRST RELEASE 12/01/62.  ;                                00008000
                                                                        00008100
PROCEDURE     INVPDS(N, A, DIAGONAL) ;                                  00008200
VALUE         N ;                                                       00008300
INTEGER       N ;                                                       00008400
REAL ARRAY    A[1,1], DIAGONAL[1] ;                                     00008500
         BEGIN                                                          00008600
INTEGER       I, J, K, I1, L ;                                          00008700
REAL          DIAG, Q ;                                                 00008800
REAL ARRAY    TEMP[0:N] ;                                               00008900
LABEL         IN1, IN2, IN3 ;                                           00009000
                                                                        00009100
COMMENT       THE ORIGINAL MATRIX IS DECOMPOSED INTO THE PRODUCT OF A   00009200
              UNIT LOWER TRIANGULAR MATRIX, A DIAGONAL MATRIX, AND A    00009300
              UNIT UPPER TRIANGULAR MATRIX WHICH IS THE TRANSPOSE OF    00009400
              THE UNIT LOWER TRIANGULAR MATRIX.  ;                      00009500
                                                                        00009600
IN1:          FOR I ~ N STEP -1 UNTIL 1 DO                              00009700
         BEGIN                                                          00009800
              DIAGONAL[I] ~ A[I,I] ;                                    00009900
              FOR K ~ I+1 STEP 1 UNTIL N DO                             00010000
              TEMP[K] ~ A[I,K] | A[K,K] ;                               00010100
              FOR J ~ I STEP -1 UNTIL 1 DO                              00010200
         BEGIN                                                          00010300
              Q~0 ;                                                     00010400
              FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ A[J,K] | TEMP[K] + Q ;  00010500
              IF I=J THEN A[J,I] ~ DIAG ~ A[J,I] - Q  ELSE              00010600
                   A[J,I] ~ (A[J,I]-Q)/DIAG                             00010700
         END                                                            00010800
         END ;                                                          00010900
                                                                        00011000
COMMENT       THESE THREE MATRICES ARE INVERTED.  ;                     00011100
                                                                        00011200
IN2:          FOR I ~ N STEP -1 UNTIL 1 DO                              00011300
         BEGIN                                                          00011400
              I1 ~ I+1 ;  A[I,I] ~ 1.0 / A[I,I] ;                       00011500
              FOR J ~ N STEP -1 UNTIL I1 DO                             00011600
         BEGIN                                                          00011700
              Q~0 ;  L ~ J-1 ;                                          00011800
              FOR K ~ I1 STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;    00011900
              A[I,J] ~ -A[I,J] - Q                                      00012000
         END                                                            00012100
         END ;                                                          00012200
                                                                        00012300
COMMENT       THE INVERTED MATRICES ARE MULTIPLIED, IN REVERSE ORDER,   00012400
              TO GIVE THE DESIRED INVERSE.  ;                           00012500
IN3:          FOR I ~ N STEP -1 UNTIL 1  DO                             00012600
         BEGIN                                                          00012700
              I1 ~ I-1 ;  DIAG ~ A[I,I] ;                               00012800
              FOR K ~ 1 STEP 1 UNTIL I1 DO                              00012900
              TEMP[K] ~ A[K,K] | A[K,I] ;                               00013000
              FOR J ~ N STEP -1 UNTIL I DO                              00013100
         BEGIN                                                          00013200
              Q~0 ;                                                     00013300
              FOR K ~ 1 STEP 1 UNTIL I1 DO Q ~ A[K,J] | TEMP[K] + Q ;   00013400
       A[J,I]~A[I,J] ~ IF I=J THEN A[I,J] + Q ELSE A[I,J] | DIAG + Q    00013500
         END                                                            00013600
         END                                                            00013700
         END ;                                                          00013800
PROCEDURE MATMPY (A,B,N1,N2,N4,C);                                      00013900
VALUE N1,N2,N4;                                                         00014000
INTEGER N1,N2,N4;                                                       00014100
REAL ARRAY A,B,C[1,1];                                                  00014200
BEGIN                                                                   00014300
INTEGER I,J,K;                                                          00014400
FOR K~1 STEP 1 UNTIL N4 DO                                              00014500
FOR I~1 STEP 1 UNTIL N1 DO BEGIN                                        00014600
C[I,K] ~ 0;                                                             00014700
FOR J~1 STEP 1 UNTIL N2 DO                                              00014800
C[I,K] ~ A[I,J] | B[J,K] + C[I,K] END;                                  00014900
END;                                                                    00015000
INTEGER R,V,L,I,J,DSQMQ;                                                00015100
REAL XS,ZS;                                                             00015200
LABEL VV;                                                               00015300
ARRAY FACT,MAT[0:(D+1)|(D+1)];                                          00015400
REAL ARRAY BB[0:D,0:0];                                                 00015500
LABEL L4,L6;                                                            00015600
ARRAY XSS,ZSS[0:D];                                                     00015700
REAL ARRAY T[1:N];                                                      00015800
REAL ARRAY B[1:N,1:(D+1)],                   Z[1:(D+1),1:1],W[1:N,1:1]; 00015900
REAL ARRAY DIAGONAL[1:D+1];                                             00016000
XS~(X[1]+X[N])/2.;                                                      00016100
ZS~X[N]-XS;                                                             00016200
FOR I~1 STEP 1 UNTIL N DO                                               00016300
T[I]~(X[I]-XS)/ZS;                                                      00016400
L4:FOR I~1 STEP 1 UNTIL N DO                                            00016500
BEGIN                                                                   00016600
B[I,1]~1.0;                                                             00016700
FOR J~2 STEP 1 UNTIL (D+1) DO                                           00016800
B[I,J]~T[I]|B[I,(J-1)];                                                 00016900
END;                                                                    00017000
L6:MATATB (B,B,N,D+1,D+1,C);                                            00017100
MATTP (Y,1,N,W);                                                        00017200
MATATB (B,W,N,D+1,1,Z);                                                 00017300
INVPDS ((D+1),C,DIAGONAL);                                              00017400
MATMPY (C,Z,D+1,D+1,1,BB);                                              00017500
DSQMQ~(D|D)+(2|D);                                                      00017600
FOR R~0 STEP 1 UNTIL D DO FACT[R]~1;                                    00017700
FOR R~(D+1) STEP 1 UNTIL DSQMQ DO FACT[R]~0;                            00017800
V~2;                                                                    00017900
FOR R~D STEP (D+1) UNTIL DSQMQ DO                                       00018000
BEGIN                                                                   00018100
FOR  L ~ V STEP  1 UNTIL  ( D + 1 ) DO                                  00018200
FACT [R+L]~FACT[R-D+L-2]+FACT[R+L-1];                                   00018300
V~V+1;                                                                  00018400
END;                                                                    00018500
XSS[0]~1;                                                               00018600
XSS[1]~-XS;                                                             00018700
FOR I~1 STEP 1 UNTIL D DO XSS[I]~-XS|XSS[I-1];                          00018800
ZSS[0]~1;                                                               00018900
ZSS[1]~ZS;                                                              00019000
IF D<2 THEN GO TO VV;                                                   00019100
FOR I~2 STEP 1 UNTIL D DO ZSS[I]~ZS|ZSS[I-1];                           00019200
VV:V~0;                                                                 00019300
FOR R~0 STEP (D+1) UNTIL DSQMQ DO BEGIN                                 00019400
FOR L~V STEP 1 UNTIL D DO                                               00019500
MAT[R+L]~FACT[R+L]|XSS[L-V]/ZSS[L];                                     00019600
V~V+1;                                                                  00019700
END;                                                                    00019800
J~0;                                                                    00019900
V~0;                                                                    00020000
FOR R~0 STEP (D+1) UNTIL DSQMQ DO                                       00020100
BEGIN                                                                   00020200
J~J+1;                                                                  00020300
A[J,1]~0;                                                               00020400
FOR I~V STEP 1 UNTIL D DO                                               00020500
A[J,1]~A[J,1]+MAT[R+I]|BB[I,0];                                         00020600
V~V+1 END;                                                              00020700
END;                                                                    00020800
READ (FILE1,FORMAT1,LIST1);                                             00020900
READ (FILE1,FORMAT1,LIST2);                                             00021000
LS (N,D,X,Y,A);                                                         00021100
FOR I~ 1 STEP 1 UNTIL N DO BEGIN YY[I]~0; FOR J~1 STEP 1 UNTIL D DO     00021200
YY[I]~A[J+1,1]|X[I] *J + YY[I]; YY[I]~YY[I]+A[1,1] END;                 00021300
YYYY~0;                                                                 00021400
FOR I~1 STEP 1 UNTIL N DO                                               00021500
BEGIN                                                                   00021600
YYY[I]~Y[1,I]-YY[I];                                                    00021700
YYYY~YYYY+YYY[I]*2;                                                     00021800
END;                                                                    00021900
YYYY~YYYY/(N-D);                                                        00022000
STD~SQRT(YYYY);                                                         00022100
FOR I~1 STEP 1 UNTIL D+1 DO BEGIN                                       00022200
AOE[I]~SQRT (C[I,I]); AOE[I]~AOE[I]-STD END;                            00022300
WRITE (F1,FOR2,L3);                                                     00022400
WRITE (F1[4]);                                                          00022500
WRITE (F1,FOR3);                                                        00022600
WRITE (F1,FOR5,L1);                                                     00022700
WRITE (F1[DBL]);                                                        00022800
WRITE (F1,FOR4,STD);                                                    00022900
WRITE (F1[4]);                                                          00023000
WRITE (F1,FOR6,L6);                                                     00023100
WRITE (F1[PAGE]);                                                       00023200
CASE ~CASE-1;                                                           00023300
IF CASE > 0 THEN GO TO Q;                                               00023400
END;                                                                    00023500
END.                                                                    00023600
