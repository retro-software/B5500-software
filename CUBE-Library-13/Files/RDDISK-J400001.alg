          BEGIN                                                         00000100
COMMENT        PROGRAM - RDDISK,                                        00000200
               CUBE LIBRARY NUMBER IS J400001.                          00000300
               THIS VERSION DATED 6/20/67;                              00000400
COMMENT        RDDISK, READS A CARD-IMAGE FILE FROM DISK. FIRST NAME    00000500
               OF FILE STARTS IN COL 1 & 2ND IN COL 9 OF THE ONLY       00000600
               DATA CARD.  FILE IS LISTED ON PRINTER & IF COL 19-23     00000700
               SAYS "CARDS" THEN A CARD DECK IS ALSO PRODUCED;          00000800
FILE IN        CARD(1,10);                                              00000900
FILE IN        DF DISK SERIAL "NAME1" "NAME2" (2,10,30);                00001000
FILE OUT       LINE 4(2,15);                                            00001100
FILE OUT       PCH 0(2,10);                                             00001200
FILE OUT       TPCARD 2 "0CRDIMG" (2,56,10,SAVE 600);                   00001300
ARRAY          A[0:9];                                                  00001400
BOOLEAN        CARDOUT;                                                 00001500
BOOLEAN        TAPEOUT;                                                 00001600
STREAM PROCEDURE MOVE(F,T);                                             00001700
          BEGIN                                                         00001800
               SI~F; DI~T; DI~DI+1; DS~7 CHR;                           00001900
          END;                                                          00002000
LABEL          NEXT,LAST;                                               00002100
ALPHA          ID1,ID2;                                                 00002200
FORMAT         F1("LISTING OF DISK FILE ",A1,A6,"/",A1,A6/);            00002300
               READ(CARD,10,A[*]);                                      00002400
               CLOSE(CARD,RELEASE);                                     00002500
               MOVE(A[0],ID1);                                          00002600
               MOVE(A[1],ID2);                                          00002700
               WRITE(LINE,F1,ID1.[6:6],ID1,ID2.[6:6],ID2);              00002800
               FILL DF WITH ID1,ID2;                                    00002900
               MOVE(A[2],ID1);                                          00003000
               IF ID1.[18:30]="CARDS" THEN CARDOUT~TRUE                 00003100
                 ELSE CARDOUT~FALSE;                                    00003200
               IF ID1.[18:30] = "TAPE " THEN TAPEOUT ~ TRUE             00003300
               ELSE TAPEOUT ~ FALSE;                                    00003400
NEXT:          READ(DF,10,A[*])[LAST];                                  00003500
               WRITE(LINE,10,A[*]);                                     00003600
               IF CARDOUT THEN WRITE(PCH,10,A[*]);                      00003700
               IF TAPEOUT THEN WRITE(TPCARD,10,A[*]);                   00003800
               GO TO NEXT;                                              00003900
LAST:     END.                                                          00004000
