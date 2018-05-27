COMMENT        PROCEDURE - BINBCD, CUBE LIBRARY NUMBER IS M200003.      00000100
               THIS VERSION DATED 2/1/67;                               00000200
         STREAM PROCEDURE BINBCD(BINARY,BCD);                           00000300
 COMMENT THE INPUT TO THIS PROCEDURE IS A POSITIVE INTEGER BINARY       00000400
         THE OUTPUT IS THE BCD REPRESENTATION OF  BINARY;               00000500
          BEGIN                                                         00000600
          SI~BINARY;                                                    00000700
          DI~BCD;                                                       00000800
          DS~8 DEC;                                                     00000900
          END OF BINBCD;                                                00001000
