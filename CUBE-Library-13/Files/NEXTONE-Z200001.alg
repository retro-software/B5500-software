COMMENT        PROCEDURE - NEXTONE,                                     00000100
               CUBE LIBRARY NUMBER IS Z200001.                          00000200
               THIS VERSION DATED 6/20/67;                              00000300
REAL PROCEDURE NEXTONE (LISTID,FINI,SAVEIT);                            00000400
INTEGER SAVEIT;                                                         00000500
LIST LISTID;                                                            00000600
BOOLEAN FINI;                                                           00000700
COMMENT NEXTONE RETURNS THE VALUE OF THE"NEXT"PARAMETER IN THE          00000800
    DECLARED LIST LISTID. FINI AND SAVEIT ARE DECLARED LOCAL TO THE     00000900
    SEGMENT CALLING NEXTONE. THE FIRST ENTRY MUST BE WITH THE VALUE     00001000
    OF FINI = TRUE. IF THERE IS AT LEAST ONE MORE ELEMENT IN            00001100
    THE LIST TO BE RETURNED, FINI WILL BE SET = FALSE. WHEN THE         00001200
    LIST IS EXHAUSTED A MINUS ONE IS RETURNED AND FINI IS SET=TRUE.     00001300
    THE INFORMATION REQUIRED TO GET THE NEXT ITEM IN LISTID IS          00001400
    STORED IN SAVEIT AND FINI. THEREFORE EACH ACTUAL PARAMETER          00001500
    USED AS A LISTID MUST HAVE ITS OWN SAVEIT AND FINI.  THIS IS        00001600
    PARTICULARLY IMPORTANT IF SWITCH LIST CONSTRUCTS ARE USED.          00001700
    IT IS RECOMMENDED THAT THE PROGRAMMER DECLARE PARALLEL BOOLEAN AND  00001800
    INTEGER ARRAYS TO HOLD FINI AND SAVEIT FOR CORRESPONDING LISTS      00001900
    IN THE SWITCH LIST. IF A GIVEN LIST IS EXHAUSTED THEN ITS FINI      00002000
    AND SAVEIT MAY BE USED BY ANOTHER LIST.  A LIST MAY BE RESTARTED AT 00002100
    ANY TIME BY SETTING FINI=TRUE.   A TYPICAL CALL WILL BE             00002200
        LIST ITEMLIST(I,J,K).                                           00002300
        INTEGER SAVER.                                                  00002400
        BOOLEAN DONE.                                                   00002500
        REAL A.                                                         00002600
        LABEL L1,EXIT.                                                  00002700
        DONE~TRUE.                                                      00002800
        L1:  A~NEXTONE(ITEMLIST,DONE,SAVER).                            00002900
            IF DONE THEN GO TO EXIT                                     00003000
            ELSE PROCESSNEWITEM.  GO TO L1.                             00003100
        EXIT: ;                                                         00003200
COMMENT   VARIABLE A HAS A VALUE = -1 AT EXIT;                          00003300
BEGIN                                                                   00003400
INTEGER I,J;    REAL DUMMY;                                             00003500
STREAM PROCEDURE RSETTING (POINTER);                                    00003600
BEGIN                                                                   00003700
COMMENT  THIS PROCEDURE RETURNS THE SETTING OF THE R REGISTER;          00003800
SI ~ POINTER;   SI ~ SI - 55;                                           00003900
COMMENT  BACK UP POINTER TO POINT TO MARK STACK CONTROL WORD.  THE      00004000
NEXT NINE BITS IN THIS STRING ARE THE NINE BITS OF THE R REGISTER;      00004100
DI ~ POINTER;                                                           00004200
DS ~ 5 LIT "Q0000";   DS ~ 3 RESET;                                     00004300
9 (IF SB THEN DS ~ SET ELSE DS ~ RESET; SKIP 1 SB;);                    00004400
DS ~ 1 LIT "C";                                                         00004500
END RSETTING;                                                           00004600
STREAM PROCEDURE XFER(A);                                               00004700
BEGIN                                                                   00004800
SI ~ A;   SI ~ SI - 48; SI ~ SI - 16;                                   00004900
DI ~ A;   DS ~ WDS;                                                     00005000
END XFER;                                                               00005100
STREAM PROCEDURE MOVE (A)"TO"(B)"AFTER PUTTING B IN"(C);                00005200
BEGIN                                                                   00005300
SI ~ B;  DI ~ C;  DS ~ WDS;                                             00005400
SI ~ A;  DI ~ B;  DS ~ WDS;                                             00005500
END MOVE;                                                               00005600
RSETTING (I);                                                           00005700
IF FINI THEN                                                            00005800
   BEGIN                                                                00005900
   FINI ~ FALSE;                                                        00006000
   MOVE (FINI,I,J);                                                     00006100
   END                                                                  00006200
ELSE MOVE(SAVEIT,I,J);                                                  00006300
XFER(DUMMY);                                                            00006400
COMMENT   EXCHANGE LIST DESCRIPTOR AND DUMMY TO FOOL MACHINE.           00006500
REFERENCE TO DUMMY ACTUALLY EXECUTES LIST;                              00006600
NEXTONE ~ DUMMY;                                                        00006700
MOVE (J,I,SAVEIT);                                                      00006800
IF SAVEIT = -1 THEN FINI ~ TRUE;                                        00006900
END PROCEDURE NEXTONE;                                                  00007000
