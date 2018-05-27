DCMAP       000101AA                                                    00000100
                                                                        00000200
                                                                        00000300
  **********************************************************************00000400
            PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES          00000500
      NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.     00000600
  **********************************************************************00000700
%                                                                       00000800
THIS PROGRAM WILL PROVIDE INFORMATION ABOUT A SPECIFIED DISK FILE TO    00000900
                        A USER AT A REMOTE TERMINAL.                    00001000
%                                                                       00001100
         BEGIN                                                          00001200
ALPHA FILE OUT TWXOUT 14(2,8);                                          00001300
ALPHA FILE IN TWXIN 14(2,8);                                            00001400
ARRAY         A[0:2], B[0:6];                                           00001500
FORMAT        F1 (X8,A1,A6),                                            00001600
              F2 (X8,"TYPE MULTI FILE ID",A3),                          00001700
              F3 (X8,"MFID",X10,A1,A6,A3,),                             00001800
              F4 (X8,"FID",X11,A1,A6,A3),                               00001900
              F5 (X8,"RECORD LENGTH",I7," WORDS",A3),                   00002000
              F6 (X8,"BLOCK LENGTH",X1,I7," WORDS",A3),                 00002100
              F7 (X8,"EOF",X10,I7," RECORDS",A3),                       00002200
              F8 (X8,"OPEN COUNT",X3,I7,A3),                            00002300
              F9 (X8,"TYPE FILE ID",A3),                                00002400
              F10(X8,"USER TYPE",X4,I7,A3),                             00002500
              F11(X8,"FILE NOT PRESENT",A3),                            00002600
              F12(X8,"NOT A VALID USER OF THIS FILE",A3),               00002700
              F13(X8,A1,A6,A3),                                         00002800
              F14(X8,A3);                                               00002900
FILE HUNTED DISK "MFID""FID"(1,30);                                     00003000
ALPHA         USEDID, TEMP1, TEMP2;                                     00003100
STREAM PROCEDURE FIX(A,B);                                              00003200
         BEGIN                                                          00003300
              DI:=B; DI:=DI+1; DS:=7 LIT "       "; DI:=DI-7;           00003400
              SI:=A; SI:=SI+7; IF SC EQL ALPHA THEN DS:=CHR;            00003500
              SI:=SI+2; 6(IF SC EQL ALPHA THEN DS:=CHR);                00003600
         END  STREAM PROCEDURE FIX;                                     00003700
              FILL A[*] WITH OCT5774, OCT577437;                        00003800
              USEDID.[2:5]:=STATUS(B[*]);                               00003900
              USEDID := USEDID & B[1][6:6:42];                          00004000
              A[2] := A[2] & B[0][9:9:9];                               00004100
              SEEK(TWXIN(A[2]));                                        00004200
              WRITE(TWXOUT(A[2],5),F2,A[1]);                            00004300
              READ(TWXIN(A[2],10),F1,TEMP1,TEMP2);                      00004400
              WRITE(TWXOUT(A[2],5),F14,A[1]);                           00004500
              FIX(TEMP1,B[1]);                                          00004600
              WRITE(TWXOUT(A[2],5),F9,A[1]);                            00004700
              READ(TWXIN(A[2],10),F1,TEMP1,TEMP2);                      00004800
              WRITE(TWXOUT(A[2],5),F14,A[1]);                           00004900
              FIX(TEMP1,B[2]);                                          00005000
              WRITE(TWXOUT(A[2],5),F3,B[1].[6:6],B[1].[12:36],A[1]);    00005100
              WRITE(TWXOUT(A[2],5),F4,B[2].[6:6],B[2].[12:36],A[1]);    00005200
              FILL HUNTED WITH B[1].[6:42], B[2].[6:42];                00005300
              SEARCH(HUNTED,B[*]);                                      00005400
              IF B[0] EQL 0 THEN                                        00005500
         BEGIN                                                          00005600
              WRITE(TWXOUT(A[2],5),F13,USEDID.[6:6],                    00005700
                                       USEDID.[12:36],A[1]);            00005800
              WRITE(TWXOUT(A[2],5),F12,A[1]);                           00005900
         END;                                                           00006000
              IF B[0] EQL -1 THEN WRITE(TWXOUT(A[2],5),F11,A[1]);       00006100
              IF B[0] GTR 0 THEN                                        00006200
         BEGIN                                                          00006300
              WRITE(TWXOUT(A[2],5),F5,B[3],A[1]);                       00006400
              WRITE(TWXOUT(A[2],5),F6,B[4],A[1]);                       00006500
              WRITE(TWXOUT(A[2],5),F7,B[5],A[1]);                       00006600
              WRITE(TWXOUT(A[2],5),F8,B[6],A[1]);                       00006700
              WRITE(TWXOUT(A[2],5),F10,B[0],A[1]);                      00006800
         END;                                                           00006900
         END.                                                           00007000
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
