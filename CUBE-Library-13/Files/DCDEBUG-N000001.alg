BEGIN                                                                   00000100
COMMENT        PROGRAM - DATACOM/DEBUG,                                 00000200
               CUBE LIBRARY NUMBER IS N000001.                          00000300
               THIS VERSION DATED 5/31/67;                              00000400
COMMENT     C O V I L L / D E B U G              D. L. COVILL           00000500
                                                 15 FEB 1967 ;          00000600
ALPHA FILE DCIM 14 (1,10);                                              00000700
ALPHA FILE OUT DCOM 14 (2,8);                                           00000800
FILE SPO 11 (1,10);                                                     00000900
FILE PRT 17 (1,15);                                                     00001000
ARRAY MSG[0:9], INSINK[0:9];                                            00001100
REAL TU, I, PARAMS;                                                     00001200
LABEL LOOP, READREC, WRITEREC, GRPMARK, GETSTATUS, QUIT;                00001300
SWITCH SW1 ~ READREC,WRITEREC,GRPMARK,GETSTATUS,QUIT;                   00001400
% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 00001500
INTEGER PROCEDURE NEXT;                                                 00001600
BEGIN                                                                   00001700
  ALPHA CODE;  REAL I;  LABEL DONE;                                     00001800
  FORMAT F (A3,A6);                                                     00001900
  ARRAY M[0:6];                                                         00002000
    FILL M[*] WITH 0,"RED","WRT","MRK","STS","QUT";                     00002100
    READ (SPO,F,CODE,PARAMS);                                           00002200
    FOR I ~ 1 STEP 1 UNTIL 6 DO IF CODE=M[I] THEN BEGIN                 00002300
      NEXT ~ I;  GO TO DONE;  END;                                      00002400
  DONE:                                                                 00002500
END NEXT;                                                               00002600
% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 00002700
PROCEDURE TYPESTATUS (S);                                               00002800
VALUE S;  REAL S;                                                       00002900
BEGIN                                                                   00003000
  FORMAT F("STATUS= ",8L1);                                             00003100
  LIST L (BOOLEAN(S.[23:1]),                                            00003200
          BOOLEAN(S.[24:1]),                                            00003300
          BOOLEAN(S.[25:1]),                                            00003400
          BOOLEAN(S.[26:1]),                                            00003500
          BOOLEAN(S.[27:1]),                                            00003600
          BOOLEAN(S.[28:1]),                                            00003700
          BOOLEAN(S.[29:1]),                                            00003800
          BOOLEAN(S.[30:1]));                                           00003900
  WRITE (SPO, F, L);                                                    00004000
END TYPESTATUS;                                                         00004100
% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 00004200
PROCEDURE INITIALIZE;                                                   00004300
BEGIN                                                                   00004400
  TU ~ 0 & 1[9:44:4];                                                   00004500
  TYPESTATUS (STATUS(TU,1));                                            00004600
END INITIALIZE;                                                         00004700
% = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 00004800
  FILL MSG[*] WITH 0, "{!AAAAAA",                                       00004900
          "BBBBBBBB", "CCCCCCCC",                                       00005000
          "DDDDXXXX", "EEEEEEEE",                                       00005100
          "FFFFFFFF", "GGGGGGGG",                                       00005200
          "YYYYYYYY", "ZZZZZZZZ";                                       00005300
  INITIALIZE;                                                           00005400
LOOP:                                                                   00005500
  GO TO SW1[NEXT];                                                      00005600
READREC:                                                                00005700
         COMMENT ENTER BY TYPING "RED" OR "REDW2"                       00005800
         IF "RED" THEN WAIT PART WILL BE EMPTY.                         00005900
         IF "REDW2" THEN WAIT PART WILL CONTAIN 2;                      00006000
  BEGIN                                                                 00006100
  BOOLEAN W;   INTEGER S;                                               00006200
  LABEL R1, R2, NOINPUT, ABNORM;                                        00006300
  FORMAT F1 ("READREC ",A1), F2 ("NOINPUT"), F3 ("ABNORMAL");           00006400
    W ~ IF PARAMS.[12:6] = "W" THEN TRUE ELSE FALSE;                    00006500
    IF W THEN S ~ PARAMS.[18:6];                                        00006600
    WRITE (SPO,F1,IF W THEN "W" ELSE " ");                              00006700
    IF W THEN GO TO R2;                                                 00006800
  R1:                                                                   00006900
    READ (DCIM(TU),10,INSINK[*])[NOINPUT:ABNORM];                       00007000
    TYPESTATUS (INSINK[0]);                                             00007100
    GO TO LOOP;                                                         00007200
  R2:                                                                   00007300
    READ (DCIM(TU,S),10,INSINK[*])[NOINPUT:ABNORM];                     00007400
    TYPESTATUS (INSINK[0]);                                             00007500
    GO TO LOOP;                                                         00007600
  NOINPUT:                                                              00007700
    WRITE (SPO,F2);  GO TO LOOP;                                        00007800
  ABNORM:                                                               00007900
    WRITE (SPO, F3);  GO TO LOOP;                                       00008000
  END OF READREC BLOCK;                                                 00008100
WRITEREC:                                                               00008200
  BEGIN                                                                 00008300
         COMMENT ENTER BY TYPING "WRTWSN"  (WRT IS BASIC COMMAND)       00008400
         IF "W" WAIT PART WILL BE SET TO "S",                           00008500
         "N" WORDS WILL BE WRITTEN;                                     00008600
  BOOLEAN W;    INTEGER S, N;                                           00008700
  LABEL W1, W2, CANTDOIT, BROKE;                                        00008800
  FORMAT F1 ("WRITEREC"), F2 ("CANTDOIT"), F3 ("BROKE");                00008900
    WRITE (SPO,F1);                                                     00009000
    W ~ IF PARAMS.[12:6] = "W" THEN TRUE ELSE FALSE;                    00009100
    S ~ PARAMS.[18:6];                                                  00009200
    N ~ PARAMS.[24:6];                                                  00009300
    IF W THEN GO TO W2;                                                 00009400
  W1:                                                                   00009500
    WRITE (DCOM(TU),N,MSG[*])[CANTDOIT:BROKE];                          00009600
    TYPESTATUS (STATUS(TU,0));                                          00009700
    GO TO LOOP;                                                         00009800
  W2:                                                                   00009900
    WRITE (DCOM(TU,S),N,MSG[*])[CANTDOIT:BROKE];                        00010000
    TYPESTATUS (STATUS(TU,0));                                          00010100
    GO TO LOOP;                                                         00010200
  CANTDOIT:                                                             00010300
    WRITE (SPO,F2);  GO TO LOOP;                                        00010400
  BROKE:                                                                00010500
    WRITE (SPO,F3);  GO TO LOOP;                                        00010600
  END OF WRITEREC BLOCK;                                                00010700
GRPMARK:                                                                00010800
  BEGIN                                                                 00010900
  FORMAT F ("GRPMARK");                                                 00011000
INTEGER C, SR;                                                          00011100
  STREAM PROCEDURE SETIT (MSG,C,SR);                                    00011200
  VALUE C;                                                              00011300
  BEGIN                                                                 00011400
    DI ~ MSG;  DI ~ DI + C;                                             00011500
    SI ~ SR;  SI ~ SI + 7;                                              00011600
    DS ~ 1 CHR;                                                         00011700
    DI ~ DI - 1;  SI ~ SI - 1;                                          00011800
    IF SC = "S" THEN DS ~ 1 LIT "~";                                    00011900
  END SETIT;                                                            00012000
    WRITE (SPO,F);                                                      00012100
    C ~ PARAMS.[12:6]|10 + PARAMS.[18:6];                               00012200
    C ~ C - 1;                                                          00012300
    SR ~ PARAMS.[24:6];                                                 00012400
    SETIT (MSG[1],C,SR);                                                00012500
    GO TO LOOP;                                                         00012600
  END OF GRPMARK BLOCK;                                                 00012700
GETSTATUS:                                                              00012800
  BEGIN                                                                 00012900
         COMMENT ENTER BY TYPING "STSA"                                 00013000
         IF "A" THEN WILL BE AN ACTIVE INTERROGATE, OTHERWISE PASSIVE;  00013100
  BOOLEAN ACTIVE;                                                       00013200
  FORMAT F ("GETSTATUS");                                               00013300
    WRITE (SPO,F);                                                      00013400
    ACTIVE ~ IF PARAMS.[12:6] = "A" THEN TRUE ELSE FALSE;               00013500
    TYPESTATUS (STATUS(TU,IF ACTIVE THEN 1 ELSE 0));                    00013600
    GO TO LOOP;                                                         00013700
  END OF GETSTATUS BLOCK;                                               00013800
QUIT:                                                                   00013900
END PROGRAM.                                                            00014000
