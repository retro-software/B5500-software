BEGIN                                                                   00000100
COMMENT        PROGRAM - TAPEDUMP,                                      00000200
               CUBE LIBRARY NUMBER IS K400001.                          00000300
               THIS VERSION DATED 6/20/67;                              00000400
FILE LP 4 (2,15);                                                       00000500
ARRAY F[0:1022]; FILE FF(1,1023);                                       00000600
INTEGER COUNT,I;                                                        00000700
LABEL TOPP,EOF,PAR;                                                     00000800
ARRAY LINE[0:14];                                                       00000900
FORMAT BAD(2(60("*")),/"IRRECOVERABLE PARITY ERROR. "                   00001000
               "HERE COMES THE BAD RECORD",/2(60("*")));                00001100
FORMAT GOOD("THERE ARE",I6," WORDS IN THIS RECORD");                    00001200
STREAM PROCEDURE BLANK(LINE);                                           00001300
BEGIN DI~LINE; 15(DS~8 LIT " "); END;                                   00001400
STREAM PROCEDURE FILLF(F);                                              00001500
BEGIN                                                                   00001600
     DI~F; 20(51(DS~ 8 LIT""")); 3(DS~8 LIT """);                       00001700
END OF FILLF;                                                           00001800
INTEGER STREAM PROCEDURE COUNTDOWN(F);                                  00001900
BEGIN                                                                   00002000
     LOCAL ONE,TEMP1,FAREND,QUOTES;                                     00002100
     LABEL TOP,DOWN;                                                    00002200
     DI~LOC COUNTDOWN; DS~4 LIT "0";                                    00002300
     DI~LOC ONE;    DS~4 LIT "0001";                                    00002400
     DI~LOC FAREND; DS~4 LIT "1023";                                    00002500
     DI~LOC QUOTES; DS~8 LIT """;                                       00002600
     DI~LOC QUOTES; SI~F;                                               00002700
TOP: IF 8 SC=DC THEN GO TO DOWN;                                        00002800
     TEMP1~SI; SI~ LOC ONE; DI~ LOC COUNTDOWN; DS~ 4 ADD;               00002900
     DI~DI-4; SI~ LOC FAREND; IF 4 SC=DC THEN GO TO DOWN;               00003000
     SI~TEMP1; DI~LOC QUOTES;                                           00003100
     GO TO TOP;                                                         00003200
DOWN:SI~ LOC COUNTDOWN; DI~ LOC TEMP1; DS~ 4 OCT;                       00003300
     SI~ LOC TEMP1; DI~ LOC COUNTDOWN; DS~ 1 WDS;                       00003400
END OF COUNTDOWN;                                                       00003500
PROCEDURE PRINTIT;                                                      00003600
BEGIN                                                                   00003700
     PROCEDURE RUBITOUT(N); VALUE N; INTEGER N;                         00003800
     BEGIN                                                              00003900
          STREAM PROCEDURE BLANCIT(L,A,B,A1,B1 ); VALUE A,B,A1,B1;      00004000
          BEGIN                                                         00004100
               DI~L; B(DI~DI+63; DI~DI+1); DI~DI+A;                     00004200
               B1(8(DS~ 8 LIT " "));                                    00004300
               A1(DS~ 1 LIT " ");                                       00004400
          END;                                                          00004500
          INTEGER J;                                                    00004600
          J~120-20|N;                   COMMENT START HERE;             00004700
          N~N|20;                       COMMENT BLANK OUT THIS MANY;    00004800
          BLANCIT(LINE,J,J DIV 64,N,N DIV 64);                          00004900
     END OF RUBITOUT;                                                   00005000
     STREAM PROCEDURE WOPITOUT(F,L);                                    00005100
     BEGIN                                                              00005200
          SI~F; DI~L;                                                   00005300
                                                                        00005400
         6(8(DS~3 RESET;3(IF SB THEN DS~1 SET ELSE DS~1 RESET;SKIP SB));00005500
          DS~ 1 LIT " ";                                                00005600
          8(DS~3 RESET;3(IF SB THEN DS~1 SET ELSE DS~1 RESET;SKIP SB)); 00005700
          DS~3 LIT " ";);                                               00005800
     END;                                                               00005900
BLANK (LINE);WRITE (LP,15,LINE[*]);                                     00006000
WRITE(LP,GOOD,COUNT);                                                   00006100
WRITE (LP,15,LINE[*]);                                                  00006200
FOR I~0 STEP 6 UNTIL COUNT-1 DO                                         00006300
     BEGIN                                                              00006400
          BLANK(LINE); WOPITOUT(F[I],LINE);                             00006500
          IF I+6>COUNT THEN RUBITOUT(6-COUNT+I);                        00006600
          WRITE (LP,15,LINE[*]);                                        00006700
     END;                                                               00006800
END OF PRINTIT;                                                         00006900
TOPP: FILLF(F);                                                         00007000
     READ(FF,1023,F[*])[EOF:PAR];                                       00007100
     COUNT~COUNTDOWN(F);                                                00007200
     PRINTIT;                                                           00007300
     GO TO TOPP;                                                        00007400
PAR:  WRITE(LP,BAD);   GO TO TOPP;                                      00007500
EOF:                                                                    00007600
END.                                                                    00007700
