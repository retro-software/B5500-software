CARDPRINT   000108AA                                                    00000100
                                                                        00000200
   PRINTS CARD-IMAGES ON A LINE-PRINTER, DOUBLE-SPACED.                 00000300
   INPUT MAY BE FROM A CARD-READER OR FROM A DISK FILE                  00000400
   BLOCKED (X,10,30). IF MORE THAN ONE COPY IS DESIRED, THEN            00000500
   INCLUDE A COMMON CARD WITH COMMON EQUAL TO THE NUMBER OF             00000600
   COPIES DESIRED.                                                      00000700
                                                                        00000800
  **********************************************************************00000900
          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00001000
      NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.     00001100
  **********************************************************************00001200
                                                                        00001300
%                                                                       00001400
BEGIN                                                                   00001500
   INTEGER  INDEX;                                                      00001600
   INTEGER  I,J;                                                        00001700
   ARRAY A[0:9];                                                        00001800
   FILE IN CARD(5,10,30);                                               00001900
   FILE OUT LINE 18(2,15);                                              00002000
   LABEL  EOF;                                                          00002100
%                                                                       00002200
   IF INDEX=0 THEN INDEX := 1;                                          00002300
   FOR J:=1 STEP 1 UNTIL INDEX DO                                       00002400
   BEGIN                                                                00002500
    WRITE(LINE[PAGE]); WRITE(LINE[DBL]); WRITE(LINE[DBL]);              00002600
    WHILE TRUE DO                                                       00002700
     BEGIN                                                              00002800
      READ(CARD,10,A[*]) [EOF];                                         00002900
      WRITE(LINE[DBL],10,A[*]);                                         00003000
     END;                                                               00003100
EOF:CLOSE(CARD,RELEASE);                                                00003200
   END;                                                                 00003300
END.                                                                    00003400
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
