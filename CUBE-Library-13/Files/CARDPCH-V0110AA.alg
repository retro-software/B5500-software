CARDPUNCH   000110AA                                                    00000100
                                                                        00000200
   PUNCHES A DECK OF CARDS USING AS INPUT EITHER A                      00000300
   CARD-READER OR A DISK FILE BLOCKED (X,10,30). IF MORE                00000400
   THAN ONE DECK NEEDS TO BE PUNCHED, THEN INCLUDE A COMMON             00000500
   CARD WITH COMMON EQUAL TO THE NUMBER OF DECKS DESIRED.               00000600
                                                                        00000700
  **********************************************************************00000800
          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00000900
      NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.     00001000
  **********************************************************************00001100
                                                                        00001200
%                                                                       00001300
BEGIN                                                                   00001400
   INTEGER  INDEX;                                                      00001500
   INTEGER  I,J;                                                        00001600
   ARRAY A[0:9];                                                        00001700
   FILE IN CARD(5,10,30);                                               00001800
   FILE OUT PUNCH 0(5,10);                                              00001900
   LABEL  EOF;                                                          00002000
%                                                                       00002100
   IF INDEX=0 THEN INDEX := 1;                                          00002200
   FOR J:=1 STEP 1 UNTIL INDEX DO                                       00002300
   BEGIN                                                                00002400
    WHILE TRUE DO                                                       00002500
     BEGIN                                                              00002600
      READ(CARD,10,A[*]) [EOF];                                         00002700
      WRITE(PUNCH,10,A[*]);                                             00002800
     END;                                                               00002900
EOF:CLOSE(CARD,RELEASE);                                                00003000
    CLOSE(PUNCH,RELEASE);                                               00003100
   END;                                                                 00003200
END.                                                                    00003400
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
