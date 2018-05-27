COMMENT        LOCATIONS:       1    2    3                             00000100
                                4    5    6                             00000200
                                7    8    9                             00000300
***********************************************************************;00000400
          BEGIN                                                         00000500
COMMENT        CUBE LIBRARY NUMBER IS Z200010.  THE PROGRAM NAME IS     00000600
               "TICTAC/TTY".  THIS VERSION DATED 5/14/68;               00000700
ALPHA FILE IN  TTIN 14 (2,8);                                           00000800
ALPHA FILE OUT TTOUT 14 (2,8);                                          00000900
DEFINE         RTT=READ(TTIN(STA)#, WTT=WRITE(TTOUT(STA)#;              00001000
BOOLEAN        B1;                                                      00001100
FORMAT OUT     F01(X8,3(X2,A1),"{!~");                                  00001200
FORMAT IN      FIN(X8,I1);                                              00001300
ARRAY          TBUF[0:7];                                               00001400
ALPHA ARRAY    A[0:10];                                                 00001500
INTEGER ARRAY  TEMP,TEMP1,TEMP2[1:24];                                  00001600
INTEGER        I,J,K,CT,YOU,TYPE,MOVE,YORI,ALT,STA;                     00001700
LABEL          YOU1,MACH,MACH1,YOU2,CORN,INSD,CNTR,FIRMOV,SECMOV,PRINT, 00001800
               PRINT1,WIN,YOULOOSE,AGAIN,START,MOUSE,CONT,CONT1,CHECK1, 00001900
               AG1,FINISH;                                              00002000
SWITCH         SW ~ AGAIN,START;                                        00002100
SWITCH         SW1 ~ YOU1,MACH;                                         00002200
SWITCH         SW2 ~ MACH1,YOU2;                                        00002300
SWITCH         SW3 ~ CORN,INSD,CNTR;                                    00002400
FORMAT OUT     FO5(X8,"SORRY, YOU LOOSE - BETTER LUCK NEXT TIME{!~"),   00002500
               FO6(X8,"CAT AND MOUSE, GIVE UP IF YOU CAN NOT WIN{!~"),  00002600
               FO7(X8,"CONGRATULATIONS - YOU DID IT - YOU WON{!~"),     00002700
               FO8(X8,"YOU GOOFED A MARK IS IN THAT FRAME - TRY AGAIN{!~00002800
"),            FCRLF(X8,"{!~"),                                         00002900
               FO9(X9,"{!~"),                                           00003000
               FMA1(X8,"   YOU MOVE FIRST{!~"),                         00003100
               FMA2(X8,"ONLY DIGITS  1- 9 MAY BE USED - TRY AGAIN{!~"), 00003200
               FCRLF2(X8,"{!!~"),                                       00003300
               FO10(X8,"THE BOARD IS NUMBERED AS FOLLOWS{!~"),          00003400
               FO11(X8,"1 2 3 {!~"),                                    00003500
               FO12(X8,"4 5 6{!~"),                                     00003600
               FO13(X8,"7 8 9{!~"),                                     00003700
               FO14(X8,"TYPE 0 WHEN YOU WISH TO STOP{!~"),              00003800
               FMA3(X8," YOUR MOVE{!~");                                00003900
LIST           LT(A[1],A[2],A[3]), LT1(A[4],A[5],A[6]), LT2(A[7],A[8],  00004000
               A[9]);                                                   00004100
               STA~STATUS(TBUF[*]);                                     00004200
               STA.[9:9]~TBUF[0].[9:9];                                 00004300
               WTT,FCRLF);                                              00004400
               WTT,FO10);                                               00004500
               WTT,FCRLF);                                              00004600
               WTT,FO11);                                               00004700
               WTT,FO12);                                               00004800
               WTT,FO13);                                               00004900
               WTT,FCRLF);                                              00005000
               WTT,FO14);                                               00005100
               CT~-1;                                                   00005200
               FILL TEMP[*] WITH 1,1,1,1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,5, 00005300
                                 5,6,7,7,8;                             00005400
               FILL TEMP1[*] WITH 2,3,4,5,7,9,3,5,8,5,6,7,9,5,6,7,6,7,8,00005500
                                  9,9,8,9,9;                            00005600
               FILL TEMP2[*] WITH 3,2,7,9,4,5,1,8,5,7,9,5,6,6,5,1,4,3,2,00005700
                                  1,3,9,8,7;                            00005800
               YORI~2;                                                  00005900
START:         FOR I ~ 1 STEP 1 UNTIL 10 DO A[I]~ "*";                  00006000
               MOVE ~0; GO TO SW1[YORI];                                00006100
MACH:          CT~CT+2; IF CT>9 THEN CT~CT-9; A[CT]~"X"; YORI~1;        00006200
               GO TO PRINT;                                             00006300
YOU1:          YORI~2; WTT,FMA1); MOVE~MOVE+1; GO TO AG1;               00006400
AGAIN:         B1~FALSE; MOVE~MOVE+1; WTT,FMA3);                        00006500
AG1:           RTT,FIN,YOU);                                            00006600
               WTT,FCRLF);                                              00006700
               IF YOU = 0 THEN GO TO FINISH ELSE                        00006800
               IF YOU<1 OR YOU>9 THEN                                   00006900
          BEGIN WTT,FMA2); GO TO AG1                                    00007000
          END;                                                          00007100
               IF A[YOU] !"*" THEN BEGIN WTT,FO8);                      00007200
               GO TO AG1 END;                                           00007300
               A[YOU]~"0";                                              00007400
               FOR I~1 STEP 1 UNTIL 24 DO                               00007500
          BEGIN IF A[TEMP[I]]=A[TEMP1[I]] THEN                          00007600
          BEGIN IF A[TEMP[I]]="X" THEN                                  00007700
          BEGIN IF (A[TEMP2[I]]="X") OR (A[TEMP2[I]]="0") THEN GO TO    00007800
               CONT ELSE A[TEMP2[I]] ~"X"; GO TO YOULOOSE;              00007900
           END; END;  CONT: END;                                        00008000
               FOR I~1 STEP 1 UNTIL 24 DO                               00008100
          BEGIN IF A[TEMP[I]]=A[TEMP1[I]] THEN                          00008200
          BEGIN IF A[TEMP[I]]="0" THEN                                  00008300
          BEGIN IF (A[TEMP2[I]]="X") OR (A[TEMP2[I]]="0")               00008400
               THEN GO TO CONT1 ELSE                                    00008500
          BEGIN B1~ TRUE; A[TEMP2[I]]~"X" END;                          00008600
               GO TO CHECK1; END END; CONT1: END;                       00008700
               GO TO SW2[YORI];                                         00008800
YOU2:          IF MOVE =1 THEN GO TO FIRMOV ;                           00008900
               IF MOVE =2 THEN GO TO SW3[TYPE];                         00009000
               IF B1 THEN GO TO CHECK1 ELSE GO TO MACH1;                00009100
FIRMOV:        FOR I ~1,3,7,9 DO IF A[I] !"*" THEN BEGIN                00009200
               TYPE~1; J~1; A[5] ~"X"; GO TO PRINT END;                 00009300
               FOR I ~ 2,4,6,8 DO IF A[I]!"*" THEN BEGIN                00009400
               TYPE~2; J~1; A[5]~"X"; GO TO PRINT END;                  00009500
               TYPE~3; A[3]~"X"; GO TO PRINT;                           00009600
CORN:          IF A[10-J]!"*" THEN BEGIN                                00009700
               IF J=1 OR J=3 THEN A[2]~"X" ELSE A[8] ~"X"; GO TO PRINT; 00009800
           END;                                                         00009900
               IF J>5 THEN A[16-J] ~"X" ELSE A[4-J] ~"X"; GO TO PRINT;  00010000
INSD:          IF J=2 THEN BEGIN A[1]~"X"; GO TO PRINT END;             00010100
               IF J=4 THEN BEGIN A[7]~"X"; GO TO PRINT END;             00010200
               IF J=6 THEN BEGIN A[3]~"X"; GO TO PRINT END;             00010300
CNTR:          A[9]~"X"; GO TO PRINT;                                   00010400
MACH1:         FOR I~1,3,7,9 DO                                         00010500
               IF A[I]="*" THEN BEGIN A[I]~"X"; GO TO CHECK1 END;       00010600
               IF A[5]="*" THEN BEGIN A[5]~"X"; GO TO CHECK1 END;       00010700
               FOR I~ 2,4,6,8 DO                                        00010800
               IF A[I]="*" THEN BEGIN A[I]~"X"; GO TO CHECK1 END;       00010900
CHECK1:        FOR I ~1 STEP 1 UNTIL 24 DO                              00011000
               IF A[TEMP[I]]=A[TEMP1[I]] THEN                           00011100
          BEGIN IF A[TEMP[I]]=A[TEMP2[I]] THEN                          00011200
          BEGIN IF A[TEMP[I]]!"*" THEN GO TO WIN END END;               00011300
               FOR I ~1 STEP 1 UNTIL 9 DO                               00011400
               IF A[I]="*" THEN GO TO PRINT; GO TO MOUSE;               00011500
WIN:           IF A[TEMP[I]]="X" THEN GO TO YOULOOSE;                   00011600
               WTT,FO7); K~2; GO TO PRINT1;                             00011700
YOULOOSE:      WTT,FO5); K~2; GO TO PRINT1;                             00011800
MOUSE:         WTT,FO6); K~2; GO TO PRINT1;                             00011900
PRINT:         K~1;                                                     00012000
PRINT1:                                                                 00012100
               WTT,FCRLF);                                              00012200
               WTT,F01,LT);                                             00012300
               WTT,F01,LT1);                                            00012400
               WTT,F01,LT2);                                            00012500
               WTT,FCRLF2);                                             00012600
               GO TO SW[K];                                             00012700
FINISH:                                                                 00012800
           END.                                                         00012900
