DECKSEQ     000103AA                                                    00000100
                                                                        00000200
                                                                        00000300
************************************************************************00000400
           PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES           00000500
     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00000600
***********************************************************************;00000700
%                                                                       00000800
COMMENT    ALGOL, FORTRAN, AND COBOL SOURCE DECK SEQUENCER.             00000900
         THIS PROGRAM REQUIRES ONE CONTROL CARD:                        00001000
         COLUMN 1: "A" OR "C" (ALGOL OR COBOL)                          00001100
           IF ALGOL OR FORTRAN, THEN PUNCH INTO                         00001200
             COLUMNS 2-4 ANY ALPHAMERIC ID WHICH WILL BE GANG-PUNCHED   00001300
                         INTO COLUMNS 73-75 WITH SEQUENCE NUMBERS IN    00001400
                         COLUMNS 76-80.                                 00001500
           IF COBOL, THEN PUNCH INTO                                    00001600
             COLUMNS 5-12 ANY ALPHAMERIC ID WHICH WILL BE GANG-PUNCHED  00001700
                          INTO COLUMNS 73-80 WITH SEQUENCE NUMBERS IN   00001800
                          COLUMNS 1-6;                                  00001900
%                                                                       00002000
BEGIN                                                                   00002100
   FILE IN SEQIN (2,10);                                                00002200
COMMENT  INSERT TO DETERMINE FILE DECLARATIONS                          00002300
FILE IN SEQIN(2,56,10);                                                 00002400
   FILE OUT SEQOUT 0 (2,10);                                            00002500
COMMENT  INSERT TO DETERMINE FILE DECLARATIONS                          00002600
FILE OUT SEQOUT 2(2,56,10,SAVE 1);                                      00002700
COMMENT  INSERT TO DETERMINE FILE DECLARATIONS                          00002800
FILE OUT SEQOUT 2(2,10,SAVE 1);                                         00002900
   DEFINE CARD = SEQIN #, PUNCH = SEQOUT #;                             00003000
   FORMAT IN INFMTA (A1, A3, A3, A5),                                   00003100
   INFMTB (12A6);                                                       00003200
   FORMAT OUT OTFMT (12A6,A3,A5);                                       00003300
   LABEL FINISH;                                                        00003400
   ALPHA CODE;                                                          00003500
   INTEGER I, COUNT, ICODE, J, INC, IND;                                00003600
   ALPHA ARRAY INFO[0:13], ID[0:2];                                     00003700
   LIST LST (FOR I ~ 0 STEP 1 UNTIL 11 + J DO INFO[I]);                 00003800
   STREAM PROCEDURE CONV (B, A);                                        00003900
      BEGIN                                                             00004000
      SI ~ B;                                                           00004100
      DI ~ A;                                                           00004200
      DI ~ DI + 2;                                                      00004300
      DS ~ 6 DEC                                                        00004400
      END;                                                              00004500
   READ (CARD, INFMTA, CODE, ID[0], ID[1], ID[2]);                      00004600
   IF CODE = "A" THEN ICODE ~ 0 ELSE IF CODE = "C" THEN ICODE ~ 1 ELSE  00004700
      BEGIN                                                             00004800
      FORMAT OUT ERRFMTA ("NON-EXISTENT CODE");                         00004900
      WRITE (PUNCH, ERRFMTA);                                           00005000
      GO TO FINISH                                                      00005100
      END;                                                              00005200
   INC ~ 10 * ICODE | 10;                                               00005300
   COUNT ~ INC;                                                         00005400
   IND ~ 13 | (1 - ICODE);                                              00005500
   FOR I ~ 0 STEP 1 UNTIL ICODE DO INFO[12 + I] ~ ID[I + ICODE];        00005600
   WHILE TRUE DO                                                        00005700
      BEGIN                                                             00005800
      J ~ 0;                                                            00005900
      READ (CARD, INFMTB, LST) [FINISH];                                00006000
      CONV (COUNT, INFO[IND]);                                          00006100
      J ~ 2;                                                            00006200
      WRITE (PUNCH, OTFMT, LST);                                        00006300
      COUNT ~ COUNT + INC                                               00006400
      END;                                                              00006500
   FINISH:END.                                                          00006600
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
