BEGIN                                                                   00000100
 FILE IN      OLD DISK SERIAL (2, 10, 150) ;                            00000200
 SAVE FILE OUT NEW DISK SERIAL [20:300] (2,10,150, SAVE 99) ;           00000300
 FILE IN      CARD (2,10) ;                                             00000400
 FILE OUT     LINE 18 (2,15) ;                                          00000500
 FORMAT       X72I8 (X72, I8),                                          00000600
              X90I8 (X90, I8) ;                                         00000700
 ARRAY        C, D [0:9] ;                                              00000800
 INTEGER      CS, DS, I ;                                               00000900
 DEFINE       READCARD =                                                00001000
              IF CARDTOG THEN                                           00001100
              BEGIN                                                     00001200
                 READ (CARD, 10, C [*]) [EOC] ;                         00001300
                 READ (C [*], X72I8, CS) ;                              00001400
              END#,                                                     00001500
              READOLD =                                                 00001600
              IF OLDTOG THEN                                            00001700
              BEGIN                                                     00001800
                 READ (OLD, 10, D [*]) [EOD] ;                          00001900
                 READ (D [*], X72I8, DS) ;                              00002000
              END#,                                                     00002100
              WRITENEWC =                                               00002200
              BEGIN                                                     00002300
                 WRITE (LINE [NO], X90I8, I ~ I + 1) ;                  00002400
                 WRITE (NEW, 10, C [*]) ;                               00002500
                 WRITE (LINE, 10, C [*]) ;                              00002600
              END#,                                                     00002700
              WRITENEWD =                                               00002800
              BEGIN                                                     00002900
                 WRITE (LINE [NO], X90I8, I ~ I + 1) ;                  00003000
                 WRITE (NEW, 10, D [*]) ;                               00003100
                 WRITE (LINE, 10, D [*]) ;                              00003200
              END# ;                                                    00003300
 BOOLEAN      CARDTOG, OLDTOG ;                                         00003400
 BOOLEAN      READOLDTOO ;                                              00003500
 LABEL        START, STOP, EOC, EOD ;                                   00003600
 BOOLEAN STREAM PROCEDURE NOTALLBLANK (C) ;                             00003700
 BEGIN                                                                  00003800
 LABEL        EXIT ;                                                    00003900
              SI ~ C ;                                                  00004000
              2 (36 (IF SC ! " " THEN                                   00004100
                     BEGIN                                              00004200
                       TALLY ~ 1 ;                                      00004300
                       JUMP OUT 2 TO EXIT ;                             00004400
                     END;  SI ~ SI + 1)) ;                              00004500
 EXIT:                                                                  00004600
              NOTALLBLANK ~ TALLY ;                                     00004700
 END          NOTALLBLANK ;                                             00004800
              CARDTOG ~ OLDTOG ~ TRUE ;                                 00004900
              READCARD ;                                                00005000
              READOLD ;                                                 00005100
 START:                                                                 00005200
              READOLDTOO ~ FALSE ;                                      00005300
              IF CS LSS DS THEN                                         00005400
              BEGIN                                                     00005500
                 WRITENEWC ;                                            00005600
                 READCARD ;                                             00005700
              END ELSE                                                  00005800
              IF CS = DS THEN                                           00005900
              BEGIN                                                     00006000
                 IF NOTALLBLANK (C) THEN                                00006100
                    WRITENEWC ;                                         00006200
                 READOLDTOO ~ TRUE ;                                    00006300
                 READCARD ;                                             00006400
                 READOLD ;                                              00006500
              END ELSE                                                  00006600
              BEGIN                                                     00006700
                 WRITENEWD ;                                            00006800
                 READOLD ;                                              00006900
              END ;                                                     00007000
              GO TO START ;                                             00007100
 EOC:                                                                   00007200
              IF NOT OLDTOG THEN GO TO STOP ;                           00007300
              CARDTOG ~ FALSE ;                                         00007400
              CS ~ 99999999 ;                                           00007500
              IF READOLDTOO THEN READOLD ;                              00007600
              GO TO START ;                                             00007700
 EOD:                                                                   00007800
              IF NOT CARDTOG THEN GO TO STOP ;                          00007900
              OLDTOG ~ FALSE ;                                          00008000
              DS ~ 99999999 ;                                           00008100
              GO TO START ;                                             00008200
 STOP:                                                                  00008300
              CLOSE (OLD) ;                                             00008400
              LOCK (NEW) ;                                              00008500
END.                                                                    00008600
END;END.        LAST CARD ON 0CRDING TAPE                               00008700
