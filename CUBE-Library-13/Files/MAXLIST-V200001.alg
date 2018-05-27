COMMENT        PROCEDURE - MAX,                                         00000100
               CUBE LIBRARY NUMBER IS V200001.                          00000200
               THIS VERSION DATED 6/20/67;                              00000300
REAL PROCEDURE MAX(LISTID) ;                                            00000400
LIST LISTID;                                                            00000500
COMMENT                                                                 00000600
    MAX RETURNS THE MAXIMUM VALUE OF THE VALUES ON THE LIST LISTID.     00000700
THE PROCEDURE CAN BE USED TO FIND THE MINIMUM VALUE ON THE LIST BY      00000800
CHANGING THE SIGN > TO < ON CARD #31;                                   00000900
BEGIN                                                                   00001000
   INTEGER I,J,DUMMY,SAVER;                                             00001100
   INTEGER K;                                                           00001200
   BOOLEAN OK;                                                          00001300
   STREAM PROCEDURE MOVE(A)"TO"(B);                                     00001400
      BEGIN                                                             00001500
         DI~B;SI~A;DS~WDS;                                              00001600
      END MOVE;                                                         00001700
   STREAM PROCEDURE RSETTING(POINTER);                                  00001800
      BEGIN                                                             00001900
         SI~POINTER;                                                    00002000
         SI~SI-39;                                                      00002100
         DI~POINTER;                                                    00002200
         DS~ 5 LIT "Q0000";                                             00002300
         DS~ 3 RESET;                                                   00002400
         9(IF SB THEN DS~SET ELSE DS~RESET; SKIP 1 SB;);                00002500
         DS~ 1 LIT "C";                                                 00002600
         SKIP 3 SB;                                                     00002700
         SI~SI+5;                                                       00002800
         DI~DI+8;                                                       00002900
         DS~WDS;                                                        00003000
      END RSETTING;                                                     00003100
   RSETTING(I);                                                         00003200
   SAVER~I;                                                             00003300
   MOVE (K,I);                                                          00003400
   K~DUMMY;                                                             00003500
   OK~TRUE;                                                             00003600
   WHILE OK DO                                                          00003700
      IF J~ DUMMY > K AND OK~ I! -1 THEN K ~ J;                         00003800
   MOVE(SAVER,I);                                                       00003900
   MAX~K;                                                               00004000
END PROCEDURE MAX;                                                      00004100
