          BEGIN                                                         00000100
COMMENT        PROGRAM - EDITOR,                                        00000200
               CUBE LIBRARY NUMBER IS Q000002.                          00000300
               THIS VERSION DATED 6/20/67;                              00000400
COMMENT                                                                 00000500
                                                                        00000600
    PROGRAMMED BY ARTHUR F KAUPE JR    8 NOVEMBER 1964                 ;00000700
                                                                        00000800
  COMMENT REVISED 26 NOVEMBER 1964 SO AS TO USE REWIND OPERATION        00000900
          WHERE POSSIBLE AND TO DISCONTINUE USE OF THE BLANK AFTER      00001000
          THE CODES VOID, RSEQ, AND STEP;                               00001100
                                                                        00001200
  COMMENT REVISED 4 DECEMBER 1964 TO PUT SAVE FACTOR IN OUTPUT FILE;    00001300
                                                                        00001400
FILE                                                                    00001500
    EDITCRD(2,10),      % CONTROL AND SYMBOLIC FILE                     00001600
    TAPE0(2,56,10),TAPE1(2,56,10),TAPE2(2,56,10),TAPE3(2,56,10),        00001700
    TAPE4(2,56,10),TAPE5(2),TAPE6(2),TAPE7(2),TAPE8(2),TAPE9(2);        00001800
FILE OUT                                                                00001900
    NEWTAPE(2,56,10,SAVE 1),    %  OUTPUT FILE                          00002000
    PRINTER 4 (5,15);   % PROGRAM LISTING                               00002100
DEFINE NOOFOPTIONS= 8#;                                                 00002200
SAVE ARRAY CARD[0:14];                                                  00002300
ARRAY SAVENO        ,VOIDTHRU[0:0];                                     00002400
ARRAY TAPE[0:9],PRINT,BLANKS[0:14],                                     00002500
    CONTROLOPTIONS[0:NOOFOPTIONS];                                      00002600
DEFINE CARD9=CARD[9]#,PRINT9=PRINT[9]#,TAPEX=TAPE[9]#;                  00002700
INTEGER SEQNO,SEQINCR,TAPENUMBER,FIRSTPOSITION;                         00002800
SWITCH FILE TAPEN~                                                      00002900
           TAPE0,TAPE1,TAPE2,TAPE3,TAPE4,TAPE5,TAPE6,TAPE7,TAPE8,TAPE9; 00003000
BOOLEAN STREAM PROCEDURE PERCENT(A);                                    00003100
  BEGIN SI~A; IF SC="%" THEN TALLY~1; PERCENT~TALLY END;                00003200
BOOLEAN STREAM PROCEDURE COLLATE(A,B);                                  00003300
  BEGIN SI~A; DI~B; IF 8 SC{DC THEN TALLY~1; COLLATE~TALLY END;         00003400
BOOLEAN STREAM PROCEDURE EQUAL(A,B);                                    00003500
  BEGIN SI~A; DI~B; IF 8 SC=DC THEN TALLY~1; EQUAL~TALLY END;           00003600
STREAM PROCEDURE MOVE(A,B); BEGIN SI~A; DI~B; DS~10 WDS END;            00003700
BOOLEAN STREAM PROCEDURE EMPTY(A,B);                                    00003800
  BEGIN SI~A; DI~B; IF 20 SC=DC THEN IF 52 SC=DC THEN TALLY~1;          00003900
  EMPTY~TALLY END;                                                      00004000
DEFINE BLANKCARD=EMPTY(PRINT,BLANKS)#;                                  00004100
STREAM PROCEDURE MARKPARITY(A);                                         00004200
  BEGIN DI~A; DS~27 LIT "PARITY ERROR WHEN READ | | |" END;             00004300
STREAM PROCEDURE BLANKOUT(A);                                           00004400
  BEGIN DI~A; DS~8 LIT " "; SI~A; DS~14 WDS END;                        00004500
STREAM PROCEDURE NUMBER(A,B); VALUE B;                                  00004600
  BEGIN DI~A; SI~LOC B; DS~8 DEC END;                                   00004700
BOOLEAN TAPEMODE,CARDMODE,PARITY,PARITYMARK,RESEQ,SINGLE;               00004800
LABEL PICKCARD,CONTROLCARD,FINIS,INITIALIZATION,GET,PICKTAPE,EOT,EOF,   00004900
    PART,EOT1,PART1,PROCESS;                                            00005000
INITIALIZATION:                                                         00005100
  BEGIN                                                                 00005200
  INTEGER STREAM PROCEDURE GETADDRESS(A);                               00005300
    BEGIN SI~A; GETADDRESS~SI END;                                      00005400
  FIRSTPOSITION~GETADDRESS(CARD);                                       00005500
  BLANKOUT(CARD);                                                       00005600
  BLANKOUT(PRINT); BLANKOUT(BLANKS);                                    00005700
  FILL CONTROLOPTIONS[*] WITH                                           00005800
      "%%%%    ","TAPE    ","SNGL    ","VOID    ",                      00005900
      "RSEQ    ","STEP    ","DBLE    ","NSEQ    ";                      00006000
PARITY~PARITYMARK~TAPEMODE~FALSE;                                       00006100
CARDMODE~RESEQ~SINGLE~TRUE;                                             00006200
  SEQNO~0; SEQINCR~1000;                                                00006300
  READ(EDITCRD,10,CARD[*])                                              00006400
  END*;                                                                 00006500
GO PICKCARD;                                                            00006600
CONTROLCARD:                                                            00006700
  BEGIN                                                                 00006800
  INTEGER STREAM PROCEDURE DEBLANK(A,B); VALUE A,B;                     00006900
    BEGIN LABEL LOOP,FINIS;                                             00007000
    SI~A; SI~SI+B; LOOP: IF SC!" " THEN GO FINIS; SI~SI+1; GO LOOP;     00007100
    FINIS: DEBLANK~SI                                                   00007200
    END;                                                                00007300
  INTEGER STREAM PROCEDURE SCAN(A,B); VALUE A;                          00007400
    BEGIN SI~A; DI~B; TALLY~1;                                          00007500
    NOOFOPTIONS(IF 4 SC=DC THEN JUMP OUT; SI~SI-4; DI~DI+4; TALLY~TALLY 00007600
        +1); SCAN~TALLY END;                                            00007700
  INTEGER STREAM PROCEDURE TAPENO(A); VALUE A;                          00007800
    BEGIN SI~A; SI~SI+4; DI~LOC TAPENO; DI~DI+7; DS~1 CHR END;          00007900
  STREAM PROCEDURE MOVESEQNO(A,B); VALUE A;                             00008000
    BEGIN SI~A; SI~SI+4; DI~B; DS~8 CHR END;                            00008100
  INTEGER STREAM PROCEDURE GETSEQNO(A); VALUE A;                        00008200
    BEGIN SI~A; SI~SI+4; DI~LOC GETSEQNO; DS~8 OCT END;                 00008300
  FILE OUT ERRFILE 1 "CONTROL"" ERROR " (1,15);                         00008400
  FORMAT OUT GOOF(X72,"SEGUENCE ERROR", 17(" |"));                      00008500
  STREAM PROCEDURE MOVE1(A,B); BEGIN SI~A; DI~B; DS~1 WDS END;          00008600
  INTEGER POSITION,TEMP;                                                00008700
  BOOLEAN VOIDMODE,ERROR;                                               00008800
  LABEL LOOP,TAPEOP,PAR,SNGLOP,DBLEOP,VOIDOP,STRTOP,STEPOP,NSEQOP,EOC,  00008900
      BADCARD,VOIDLOOP,PARI,ONT,CLOSELOOP;                              00009000
  DEFINE ENDMARK = CONTROLOPTIONS #;                                    00009100
  SWITCH OPTION~EOC,TAPEOP,SNGLOP,VOIDOP,STRTOP,STEPOP,DBLEOP,NSEQOP,   00009200
      BADCARD;                                                          00009300
  VOIDMODE~ERROR~FALSE;                                                 00009400
  POSITION~DEBLANK(FIRSTPOSITION,1);                                    00009500
  MOVE1(CARD9,SAVENO); MOVE1(ENDMARK,CARD9);                            00009600
LOOP:                                                                   00009700
  GO TO OPTION[SCAN(POSITION,CONTROLOPTIONS)];                          00009800
TAPEOP:                                                                 00009900
  TEMP~TAPENO(POSITION); POSITION~DEBLANK(POSITION,5);                  00010000
  IF TAPEMODE THEN                                                      00010100
    IF TAPENUMBER=TEMP THEN                                             00010200
      REWIND(TAPEN[TAPENUMBER])                                         00010300
    ELSE                                                                00010400
      CLOSE(TAPEN[TAPENUMBER],RELEASE);                                 00010500
  IF (TAPEMODE~(TAPENUMBER~TEMP)}0 AND TAPENUMBER{9) THEN               00010600
    BEGIN READ(TAPEN[TAPENUMBER],10,TAPE[*])[:PAR];                     00010700
    PARITY~FALSE; GO LOOP;                                              00010800
PAR:                                                                    00010900
    PARITY~TRUE;                                                        00011000
    END;                                                                00011100
  GO LOOP;                                                              00011200
SNGLOP:                                                                 00011300
  SINGLE~TRUE; POSITION~DEBLANK(POSITION,4); GO LOOP;                   00011400
DBLEOP:                                                                 00011500
  SINGLE~FALSE; POSITION~DEBLANK(POSITION,4); GO LOOP;                  00011600
VOIDOP:                                                                 00011700
  MOVESEQNO(POSITION,VOIDTHRU);                                         00011800
  VOIDMODE~TRUE;                                                        00011900
  POSITION~DEBLANK(POSITION,12); GO LOOP;                               00012000
STRTOP:                                                                 00012100
  IF (ERROR~(SEQNO-SEQINCR)>(SEQNO~GETSEQNO(POSITION))) THEN            00012200
    WRITE(PRINTER,GOOF);                                                00012300
  RESEQ~TRUE;                                                           00012400
  POSITION~DEBLANK(POSITION,12); GO LOOP;                               00012500
STEPOP:                                                                 00012600
  IF (SEQINCR~GETSEQNO(POSITION)){0 THEN                                00012700
    BEGIN CLOSELOOP: READ(EDITCRD,10,CARD[*])[FINIS]; GO CLOSELOOP END; 00012800
  POSITION~DEBLANK(POSITION,12); GO LOOP;                               00012900
NSEQOP:                                                                 00013000
  RESEQ~FALSE; POSITION~DEBLANK(POSITION,4); GO LOOP;                   00013100
EOC:                                                                    00013200
  IF ERROR THEN                                                         00013300
BADCARD:                                                                00013400
    BEGIN MOVE1(SAVENO,CARD9); WRITE(ERRFILE,15,CARD[*]) END;           00013500
  IF VOIDMODE THEN                                                      00013600
    IF TAPEMODE THEN                                                    00013700
VOIDLOOP:                                                               00013800
      IF COLLATE(TAPEX,VOIDTHRU) THEN                                   00013900
        BEGIN                                                           00014000
        READ(TAPEN[TAPENUMBER],10,TAPE[*])[ONT:PARI];                   00014100
        PARITY~FALSE; GO VOIDLOOP;                                      00014200
PARI:                                                                   00014300
        PARITY~TRUE; GO VOIDLOOP;                                       00014400
ONT:                                                                    00014500
        CLOSE(TAPEN[TAPENUMBER],SAVE);                                  00014600
        TAPEMODE~FALSE                                                  00014700
        END;                                                            00014800
  BLANKOUT(CARD);                                                       00014900
GO PICKCARD                                                             00015000
  END CONTROLCARD;                                                      00015100
GET:                                                                    00015200
IF CARDMODE THEN                                                        00015300
  IF TAPEMODE THEN                                                      00015400
    IF COLLATE(CARD9,TAPEX) THEN                                        00015500
      BEGIN                                                             00015600
      IF EQUAL(CARD9,TAPEX) THEN                                        00015700
        BEGIN                                                           00015800
        READ(TAPEN[TAPENUMBER],10,TAPE[*])[EOT:PART];                   00015900
        PARITY~FALSE                                                    00016000
        END;                                                            00016100
PICKCARD:                                                               00016200
      IF PERCENT(CARD) THEN GO CONTROLCARD;                             00016300
      MOVE(CARD,PRINT);                                                 00016400
      READ(EDITCRD,10,CARD[*])[EOF];                                    00016500
      END                                                               00016600
    ELSE                                                                00016700
      BEGIN                                                             00016800
PICKTAPE:                                                               00016900
      MOVE(TAPE,PRINT);                                                 00017000
      PARITYMARK~PARITY;                                                00017100
      READ(TAPEN[TAPENUMBER],10,TAPE[*])[EOT1:PART1];                   00017200
      PARITY~FALSE                                                      00017300
      END                                                               00017400
  ELSE                                                                  00017500
    GO PICKCARD                                                         00017600
ELSE                                                                    00017700
  BEGIN                                                                 00017800
  IF TAPEMODE THEN GO PICKTAPE;                                         00017900
  GO FINIS;                                                             00018000
EOT:                                                                    00018100
  CLOSE(TAPEN[TAPENUMBER],SAVE);                                        00018200
  TAPEMODE~FALSE;                                                       00018300
  GO PICKCARD;                                                          00018400
PART:                                                                   00018500
  PARITY~TRUE;                                                          00018600
  GO PICKCARD;                                                          00018700
EOF:                                                                    00018800
  CLOSE(EDITCRD,RELEASE);                                               00018900
  CARDMODE~FALSE;                                                       00019000
  GO PROCESS;                                                           00019100
EOT1:                                                                   00019200
  CLOSE(TAPEN[TAPENUMBER],SAVE);                                        00019300
  TAPEMODE~FALSE;                                                       00019400
  GO PROCESS;                                                           00019500
PART1:                                                                  00019600
  PARITY~TRUE                                                           00019700
  END;                                                                  00019800
PROCESS:                                                                00019900
IF BLANKCARD THEN GO GET;                                               00020000
IF RESEQ THEN                                                           00020100
  BEGIN NUMBER(PRINT9,SEQNO); SEQNO~SEQNO+SEQINCR END;                  00020200
IF PARITYMARK THEN BEGIN MARKPARITY(PRINT[11]); PARITYMARK~FALSE END;   00020300
IF SINGLE THEN                                                          00020400
  WRITE(PRINTER,15,PRINT[*])                                            00020500
ELSE                                                                    00020600
  WRITE(PRINTER[DBL],15,PRINT[*]);                                      00020700
WRITE(NEWTAPE,10,PRINT[*]);                                             00020800
BLANKOUT(PRINT);                                                        00020900
GO GET;                                                                 00021000
FINIS:                                                                  00021100
LOCK(NEWTAPE,RELEASE);                                                  00021200
END.                                                                    00021300
