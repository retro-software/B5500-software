PBDPBT      000124AA                                                    00000100
                                                                        00000200
************************************************************************00000300
          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00000400
     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00000500
************************************************************************00000600
                                                                        00000700
   PROGRAM TO MAKE A PRINTER BACK-UP TAPE FROM A PRINTER                00000800
   BACK-UP DISK FILE. COMMON IS THE SUFFIX OF THE PBD FILE.             00000900
   THIS VALUE IS ENTERED BY USING A COMMON CARD EQUAL TO THE            00001000
   PBD FILE ID. - JAMES HARLEY, GEORGIA TECH                            00001100
                                                                        00001200
BEGIN                                                                   00001300
 REAL COMMON;                                                           00001400
 SAVE FILE PBT "PBTMCP" "BACK-UP" (4,90,SAVE 10);                       00001500
 SAVE FILE PBD DISK SERIAL "PBD" " " (4,90);                            00001600
 LABEL REED,EOF;                                                        00001700
 SAVE ARRAY A[0:89];                                                    00001800
%                                                                       00001900
 STREAM PROCEDURE CONVERT(I,Z);                                         00002000
 VALUE I;                                                               00002100
 BEGIN                                                                  00002200
  DI:=Z;                                                                00002300
  SI:=LOC I;                                                            00002400
  DS:=8 DEC;                                                            00002500
 END CONVERT;                                                           00002600
%                                                                       00002700
 CONVERT(COMMON,A);                                                     00002800
 FILL PBD WITH *, A[0];                                                 00002900
 REED: READ(PBD,90,A[*]) [EOF];                                         00003000
 WRITE(PBT,90,A[*]);                                                    00003100
 GO REED;                                                               00003200
EOF: END.                                                               00003300
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
