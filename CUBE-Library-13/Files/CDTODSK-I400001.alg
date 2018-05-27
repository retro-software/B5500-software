          BEGIN                                                         00000100
COMMENT        PROGRAM CDTODSK, CUBE LIBRARY NUMBER IS I400001.         00000200
               THIS VERSION DATED 6/2/67;                               00000300
COMMENT        CREATES A CARD-IMAGE FILE ON DISK.                       00000400
               CARDS ARE NUMBERED IN STEPS OF 100 IN COLS 73-80.        00000500
               FIRST CARD READ CONTAINS NAME OF FILE, IN COLS 1-7.      00000600
               FILE CREATED IS CALLED  NAMEIT/SAVE, WHERE NAMEIT        00000700
               IS THE NAME READ FROM CARD.  THIS FILE IS COMPATIBLE     00000800
               WITH TEXT/EDITOR AND MUST BE READ LIKE CARD IMAGE,       00000900
               WITH FORMATTING OR ARRAY ROW;                            00001000
COMMENT        THE FILE WILL BE AN EXACT CARD IMAGE, AND SPECIAL        00001100
               CHARACTERS !{}~<> WHICH CANNOT BE SENT TO TELETYPES      00001200
               MUST NOT BE PRESENT IF FILE IS TO BE USED FOR            00001300
               TEXT/EDITOR. PROGRAM ALGTOTE SHOULD BE USED FOR THESE;   00001400
FILE IN        CARD(2,10);                                              00001500
FILE OUT       NEWDF DISK SERIAL[5:201]"NAMEIT" "SAVE"(2,10,30,SAVE 99);00001600
COMMENT                                                                 00001700
FILE OUT      NEWDF DISK SERIAL[5:201]"NAMEIT""SOURCE"(2,10,150,SAVE 9);00001800
ARRAY          A[0:9];                                                  00001900
INTEGER        I;                                                       00002000
ALPHA          ID1;                                                     00002100
STREAM PROCEDURE MOVE(F,T);                                             00002200
          BEGIN                                                         00002300
               SI~F; DI~T; DI~DI+1; DS~7 CHR;                           00002400
          END  MOVE;                                                    00002500
STREAM PROCEDURE OCV(OCTL,CHRS);                                        00002600
          BEGIN                                                         00002700
               SI~OCTL; DI~CHRS; DS~8 DEC;                              00002800
          END  OCV;                                                     00002900
LABEL          NEXT,EXIT;                                               00003000
               READ(CARD,10,A[*]);                                      00003100
               MOVE(A[0],ID1);                                          00003200
               FILL NEWDF WITH ID1,*;                                   00003300
               I~0;                                                     00003400
NEXT:          READ(CARD,10,A[*])[EXIT];                                00003500
               I~I+100;                                                 00003600
               OCV(I,A[9]);                                             00003700
               WRITE(NEWDF,10,A[*]);                                    00003800
               GO TO NEXT;                                              00003900
EXIT:          LOCK(NEWDF,RELEASE);                                     00004000
          END.                                                          00004100
