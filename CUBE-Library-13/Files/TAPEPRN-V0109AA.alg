TAPEPRINT   000109AA                                                    00000100
                                                                        00000200
   LISTS ON A LINE-PRINTER, DOUBLE-SPACED, A TAPE FILE WHICH IS         00000300
   BLOCKED (X,56,10).                                                   00000400
                                                                        00000500
  **********************************************************************00000600
          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASUSUMES           00000700
      NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.     00000800
  **********************************************************************00000900
                                                                        00001000
BEGIN                                                                   00001100
   FILE IN TAPE(2,56,10);                                               00001200
   FILE OUT PRINT 18(2,15);                                             00001300
   ARRAY  A[0:9];                                                       00001400
   LABEL  EOF;                                                          00001500
%                                                                       00001600
   WHILE TRUE DO                                                        00001700
   BEGIN                                                                00001800
      READ(TAPE,10,A[*]) [EOF];                                         00001900
      WRITE(PRINT[DBL],10,A[*]);                                        00002000
   END;                                                                 00002100
EOF: END.                                                               00002200
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
