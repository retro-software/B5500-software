COMMENT        PROCEDURE - GREATEST PRIME FACTOR,                       00000100
               CUBE LIBRARY NUMBER IS Z200003.                          00000200
               THIS VERSION DATED 2/1/67;                               00000300
 INTEGER PROCEDURE GPFACTOR(SOURCE);                                    00000400
  INTEGER SOURCE;                                                       00000500
  BEGIN                                                                 00000600
   INTEGER DIVIDEND,DIVISOR,QUOTIENT,N;                                 00000700
   LABEL       EXIT,FACTOR;                                             00000800
   DIVIDEND~ABS(SOURCE);                                                00000900
   IF DIVIDEND{1 THEN                                                   00001000
    BEGIN                                                               00001100
     GPFACTOR~DIVIDEND;                                                 00001200
     GO TO EXIT                                                         00001300
    END;                                                                00001400
   N~1;                                                                 00001500
   DIVISOR~2;                                                           00001600
 FACTOR: QUOTIENT~DIVIDEND DIV DIVISOR;                                 00001700
   IF QUOTIENT|DIVISOR!DIVIDEND THEN                                    00001800
    BEGIN                                                               00001900
     DIVISOR~DIVISOR+N;                                                 00002000
     N~2                                                                00002100
    END ELSE                                                            00002200
    IF QUOTIENT=1 THEN                                                  00002300
     BEGIN                                                              00002400
      GPFACTOR~DIVIDEND;                                                00002500
      GO TO EXIT                                                        00002600
     END ELSE                                                           00002700
      DIVIDEND~QUOTIENT;                                                00002800
   GO TO FACTOR;                                                        00002900
 EXIT: END;                                                             00003000
