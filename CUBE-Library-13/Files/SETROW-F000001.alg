COMMENT        PROCEDURE - SETROWSIZE,                                  00000100
               CUBE LIBRARY NUMBER IS F000001.                          00000200
               THIS VERSION DATED 6/20/67;                              00000300
PROCEDURE      SETROWSIZE (ROW, SIZE); VALUE SIZE;                      00000400
INTEGER        SIZE; ARRAY ROW[0];                                      00000500
           BEGIN COMMENT SETROWSIZE SIMULTANEOUSLY CAUSES MCP TO        00000600
                             DEALLOCATE STORAGE FOR THE ARRAY "ROS"     00000700
                             AND SETS THE SIZE FIELD (WORD COUNT) IN    00000800
                             THE DESCRIPTOR FOR THAT ROW TO "SIZE".     00000900
                             USER MAY LATER ACCESS THAT ROW, CAUSING    00001000
                             MCP TO RE-ALLOCATE STORAGE (ACCORDING TO   00001100
                             THE NEW WORD COUNT) AND TO FILL IT WITH    00001200
                             ZEROS, AS USUAL. THE ARRAY "ROW" DOES      00001300
                             NOT HAVE TO BE PRESENT WHEN SETROWSIZE     00001400
                             IS CALLED TO ACT ON IT. ;                  00001500
REAL           POINT; ARRAY DUMMY[0:SIZE-1];                            00001600
STREAM PROCEDURE DIDL(ROW, POINT, DUMMY);                               00001700
          BEGIN LOCAL T2, T1, T3, T4, KEEPINCORE, ENCORE;               00001800
          COMMENT "ROW" MAY OR MAY NOT BE PRESENT WHEN DIDL             00001900
               IS CALLED.  "DUMMY" WILL NOT BE PRESENT.  WE WILL BUILD  00002000
              THE NEW DESCRIPTORS WITHIN THE STREAM PROCEDURE STACK, AND00002100
          COMMUNICATE THEM TO THE OUTSIDE WORLD WITH A "DS~WDS"         00002200
          XFER; SI ~ LOC ROW;  SKIP 2 SB;  IF SB THEN                   00002300
          BEGIN COMMENT  IT IS PRESENT, LETS KEEP IT THAT WAY;          00002400
              SI ~ ROW ;  KEEPINCORE ~ SI; TALLY ~ 1;                   00002500
              SI ~ POINT; SI ~ SI + 8; T3 ~ SI                          00002600
          END; COMMENT IF ROW WAS PRESENT, WE SAVED OFF THE             00002700
          "OUTSIDE" STACK ADDRESS OF "DUMMY" FOR LATER FIXUP;           00002800
          ENCORE ~ TALLY; COMMENT SET USABLE COUNTER TO EITHER          00002900
          ZERO OR ONE, DEPENDING ON THE PRESENCE OF "ROW";              00003000
          SI ~ LOC ROW; DI ~ LOC T1; DS ~ WDS; SI ~ SI + 8;             00003100
          DS ~ WDS; COMMENT T1 IS COPY OF "ROW" DESCRIPTOR,             00003200
          T2 IS COPY OF "DUMMY" DESCRIPTOR;                             00003300
          SI ~ LOC ROW; DI ~ LOC T2; SI ~ SI + 3; DI ~ DI + 3;          00003400
          DS ~ 2 CHR; 3(IF SB THEN DS ~ SET ELSE DS ~ RESET;            00003500
          SKIP SB); SI ~ LOC DUMMY; DI ~ LOC T1; SI ~ SI + 3;           00003600
          DI ~ DI + 3; DS ~ 2 CHR; 3(IF SB THEN DS ~ SET ELSE           00003700
          DS ~ RESET; SKIP SB); COMMENT THIS CODE INTERCHANGED          00003800
          THE F-FIELDS OF THE DESCRIPTORS.  I NOW HAVE GOOD             00003900
          COPIES IN THE STREAM PROCEDURE STACK;                         00004000
          SI ~ LOC ROW ; DI ~ LOC T4; DI ~ DI + 5; SKIP 3 DB;           00004100
               SI ~ SI + 3; 15(IF SB THEN DS ~ SET ELSE DS ~ RESET;     00004200
          SKIP SB); COMMENT FIND ADDRESS OF MOTHER FOR "ROW";           00004300
          SI ~ LOC T1; DI ~ POINT; DI ~ DI + 8; DS ~ WDS;               00004400
          COMMENT THAT TAKES CARE OF DUMMY;                             00004500
          DI ~ T4; DS ~ WDS;                                            00004600
          COMMENT AND THAT TAKES CARE OF ROW;                           00004700
          ENCORE(DI ~ KEEPINCORE; DI ~ DI - 8; SI ~ LOC T3;             00004800
          DS ~ WDS); COMMENT THIS FIXES JP THE MEMORY LINK,             00004900
          IF ROW WAS IN CORE TO BEGIN WITH;                             00005000
          END DIDL;                                                     00005100
          DIDL(ROW,POINT,DUMMY);                                        00005200
          END SETROWSIZE;                                               00005300
