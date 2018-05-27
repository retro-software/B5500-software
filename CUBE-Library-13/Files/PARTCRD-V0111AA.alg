PARTCARD    000111AA                                                    00000100
                                                                        00000200
   PRINTS 72 COLUMNS OF 80 COLUMN CARD-IMAGES ON A LINE-PRINTER.        00000300
   INPUT MAY BE FROM A CARD-READER OR FROM A DISK FILE                  00000400
   BLOCKED (X,10,30). IF MORE THAN ONE COPY IS DESIRED, THEN            00000500
   INCLUDE A COMMON CARD WITH COMMON EQUAL TO THE NUMBER OF             00000600
   COPIES DESIRED. LISTINGS ARE DOUBLE-SPACED.                          00000700
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
   ARRAY A[0:11];                                                       00001800
   FILE IN CARD(5,10,30);                                               00001900
   FILE OUT LINE 18(2,15);                                              00002000
   LIST LIS(FOR I:=0 STEP 1 UNTIL 11 DO A[I]);                          00002100
   LABEL  EOF;                                                          00002200
   FORMAT CFORM(12A6);                                                  00002300
   FORMAT LFORM(X5,12A6);                                               00002400
%                                                                       00002500
   IF INDEX=0 THEN INDEX := 1;                                          00002600
   FOR J:=1 STEP 1 UNTIL INDEX DO                                       00002700
   BEGIN                                                                00002800
    WRITE(LINE[PAGE]); WRITE(LINE[DBL]); WRITE(LINE[DBL]);              00002900
    WHILE TRUE DO                                                       00003000
     BEGIN                                                              00003100
      READ(CARD,CFORM,LIS) [EOF];                                       00003200
      WRITE(LINE[DBL],LFORM,LIS);                                       00003300
     END;                                                               00003400
EOF:CLOSE(CARD,RELEASE);                                                00003500
   END;                                                                 00003600
END.                                                                    00003700
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
