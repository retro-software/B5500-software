BEGIN                                                                   00000100
COMMENT        PROGRAM - CARDIN,                                        00000200
               CUBE LIBRARY NUMBER IS I900001.                          00000300
               THIS VERSION DATED 6/20/67;                              00000400
COMMENT                                                                 00000500
                                  "CARDIN"                              00000600
                                                                        00000700
DISCLAIMER                                                              00000800
DISCLAIMER:  THE AUTHORS OF THIS PROGRAM, THE CUBE ORGANIZATION,        00000900
     AND THE BURROUGHS CORPORATION, BELIEVE THIS PROGRAM MATERIAL       00001000
     TO BE CORRECT, HOWEVER, THEY BEAR NO RESPONSIBILITY,               00001100
     FINANCIAL OR OTHERWISE, FOR ERROR RESULTING FROM ITS USE, NOR      00001200
     ACCEPT ANY RESPONSIBILITY FOR ITS MAINTENANCE.                     00001300
                                                                        00001400
AUTHOR                                                                  00001500
     MORAG D. BATSON     UNIVERSITY OF VIRGINIA                         00001600
                                                                        00001700
ABSTRACT                                                                00001800
THIS PRODEDURE FACILITATES THE READING OF OVERPUNCHED CARDS.            00001900
                                                                        00002000
SYSTEM                                                                  00002100
MINIMUM B5000.                                                          00002200
                                                                        00002300
                                                                        00002400
                             CARDIN                                     00002500
                                                                        00002600
     "CARDIN" IS AN TYPE INTEGER PROCEDURE DESIGNED TO FACILITATE THE   00002700
READING OF OVERPUNCHED CARDS.  IT EVALUATES A NUMERIC FIELD FROM 1 TO 8 00002800
COLUMNS IN WIDTH, WITH THE PROCEDURE ITSELF YIELDING THE VALUE AS A     00002900
<FUNCTIONAL DESIGNATOR>.  ITS USE IS ILLUSTRATED BY THE FOLLOWING       00003000
EXAMPLE:                                                                00003100
                                                                        00003200
                       A~CARDIN(B,C,D);                                 00003300
COMMENT                                                                 00003400
                                                                        00003500
       WHERE  A IS A <VARIABLE>,                                        00003600
              B IS A <FILE IDENTIFIER>,                                 00003700
              C AND D ARE <ARITHMETIC EXPRESSION>S SUCH THAT D{8,       00003800
                   C{80, AND C+D{81.                                    00003900
                                                                        00004000
     THE IDENTIFIER B IS ASSUMED TO NAME A FILE WHOSE ASSOCIATED        00004100
BUFFER(S) CONTAIN CARD IMAGE(S).  THE EXPRESSIONS C AND D ARE EVALUATED 00004200
TO YIELD INTEGERS. THE VALUE OF C DESIGNATES THE COLUMN OF THE BEGINNING00004300
OF THE CARD IMAGE FIELD. THE VALUE OF D DESIGNATES THE WIDTH OF THE     00004400
FIELD, IN COLUMNS.  EACH COLUMN MUST CONTAIN EITHER A NUMBER OR A SPACE,00004500
WITH THE EXCEPTION OF THE FIRST COLUMN.                                 00004600
                                                                        00004700
    IF THE FIRST COLUMN CONTAINS THE CHARACTER "-" OR AN OVERPUNCHED    00004800
ELEVEN, THE NUMERIC VALUE OF THE FIELD IS TAKEN AS NEGATIVE.  IF THE    00004900
FIRST COLUMN CONTAINS ANYTHING ELSE (IN PARTICULAR AN OVERPUNCHED "+")  00005000
THE FIELD IS TAKEN AS POSITIVE. A SPACE IN ANY COLUMN IS INTERPRETED AS 00005100
A ZERO.                                                                 00005200
                                                                        00005300
    THE USER MUST HANDLE THE FILLING AND RELEASING OF FILES AT THE      00005400
PROPER TIME, SINCE "CARDIN" PERFORMS ONLY THE EDITING AND NOT THE INPUT 00005500
FUNCTION.                                                               00005600
ADDITIONAL DOCUMENTATION.                                               00005700
     1. THE LESS-THAN-OR-OR-EQUAL-TO CHARACTER IN THE THE FIRST         00005800
     COLUMN OF A FIELD IS INTERPRETED AS OVERPUNCHED NEGATIVE.          00005900
     THIS IS BECAUSE OF THE THE VIRGINIA-STANFORD CARD CODE             00006000
     FOR THAT CHARACTER, WHICH IS  AN 11-0 PUNCH.                       00006100
     2.  IF A LETTER OCCURS IN ANY COLUMN BUT THE FIRST OF              00006200
     A FIELD, ITS ZONE BITS WILL BE STRIPPED OFF, LEAVING THE           00006300
     UNDERPUNCHED NUMERIC VALUE.                                        00006400
                                                                        00006500
************************************************************************00006600
                                                                       ;00006700
     FILE OUT LP 1(1,15);    FILE IN CR(1,10);                          00006800
     INTEGER I,J,X;                                                     00006900
FORMAT FOUT(I11);                                                       00007000
INTEGER PROCEDURE CARDIN(FYLE,COL,WIDTH);                               00007100
VALUE COL,WIDTH ;                                                       00007200
FILE FYLE ;                                                             00007300
INTEGER COL,WIDTH ;                                                     00007400
BEGIN                                                                   00007500
COMMENT LIMITATIONS OF INPUT PARAMETERS ARE: COL{8 & COL + WIDTH { 81 ; 00007600
     STREAM PROCEDURE XXX(WORD,CHAR,WIDTH,OU );                         00007700
     VALUE CHAR,WIDTH ;                                                 00007800
     BEGIN                                                              00007900
          LOCAL COPY ;                                                  00008000
          SI ~ WORD ;                                                   00008100
          SI ~ SI   + CHAR ;                                            00008200
          IF SC=">" THEN BEGIN DI~WORD;DI~DI+CHAR;DS~ 1 LIT "|" END;    00008300
          DI ~ LOC COPY ;                                               00008400
          DS ~ WIDTH CHR ;                                              00008500
          SI ~ SI - WIDTH ;                                             00008600
          DI ~ DI - 1 ;                                                 00008700
          DS ~ 1 ZON ;                                                  00008800
          SI ~ SI - 1 ;                                                 00008900
          DI ~ DI - WIDTH ;                                             00009000
          WIDTH(IF SC = ALPHA THEN DI ~ DI+1 ELSE DS ~ 6 RESET;SI~SI+1);00009100
          SI ~ LOC COPY ;                                               00009200
          DI ~ OU  ;                                                    00009300
          DS ~ WIDTH OCT ;                                              00009400
     END ;                                                              00009500
     INTEGER TEMP , INDEX1 , INDEX2 ;                                   00009600
     INDEX1 ~ ( COL - 1 ) DIV 8 ;                                       00009700
     INDEX2 ~ ( COL - 1 ) MOD 8 ;                                       00009800
     XXX(FYLE(INDEX1),INDEX2,WIDTH,TEMP);                               00009900
     CARDIN ~ TEMP ;                                                    00010000
END OF PROCEDURE CARDIN ;                                               00010100
     FOR I~1,2 DO                                                       00010200
          BEGIN                                                         00010300
          FOR J~1 STEP 8 UNTIL 73 DO                                    00010400
               BEGIN                                                    00010500
               X~CARDIN(CR,J, 8);                                       00010600
               WRITE(LP,FOUT,X);                                        00010700
               END;                                                     00010800
          RELEASE (CR); END;                                            00010900
END.                                                                    00011000
