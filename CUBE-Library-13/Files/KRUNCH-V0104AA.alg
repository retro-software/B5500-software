KRUNCH      000104AA                                                    00000100
                                                                        00000200
************************************************************************00000300
           PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES           00000400
     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00000500
***********************************************************************;00000600
%                                                                       00000700
COMMENT    KRUNCH/UTILITY PACKS AN ALGOL SOURCE PROGRAM SO THAT IT FILLS00000800
         72 COLUMNS OF EACH CARD COMPRISING THE KRUNCHED SOURCE DECK.  ;00000900
      BEGIN                                                             00001000
      ALPHA CH, T, IDENT, CHAR, SAVE1, TUF1, TUF2;                      00001100
      BOOLEAN ID, ID1, ID2, FIRSTCARD, EOF;                             00001200
      INTEGER I, WIN, WOUT, CIN, COUT, SQ;                              00001300
      LABEL K, L, M, N, ENDIT, N1;                                      00001400
      ALPHA ARRAY BUFIMG[0:9];                                          00001500
      DEFINE CR = BUFIMG #;                                             00001600
      FILE IN KRUCARD (1,10);                                           00001700
      FILE OUT KRUPNCH 0 (1,10);                                        00001800
      DEFINE CP = KRUPNCH #;                                            00001900
      STREAM PROCEDURE SEQUENCE (FO, SQ, IDENT);                        00002000
         BEGIN                                                          00002100
         DI ~ FO;                                                       00002200
         SI ~ IDENT;                                                    00002300
         SI ~ SI + 5;                                                   00002400
         DS ~ 3 CHR;                                                    00002500
         SI ~ SQ;                                                       00002600
         DS ~ 5 DEC                                                     00002700
         END;                                                           00002800
      BOOLEAN STREAM PROCEDURE ALPHACHECK (T);                          00002900
         BEGIN                                                          00003000
         SI ~ T;                                                        00003100
         SI ~ SI + 7;                                                   00003200
         DI ~ LOC ALPHACHECK;                                           00003300
         DS ~ 8 LIT "0";                                                00003400
         DI ~ DI - 1;                                                   00003500
         SKIP 5 DB;                                                     00003600
         IF SC = ALPHA THEN DS ~ 1 SET;                                 00003700
         END;                                                           00003800
      ALPHA STREAM PROCEDURE GNCH (FI, CIN);                            00003900
      VALUE CIN;                                                        00004000
         BEGIN                                                          00004100
         SI ~ FI;                                                       00004200
         SI ~ SI + CIN;                                                 00004300
         DI ~ LOC GNCH;                                                 00004400
         DI ~ DI + 7;                                                   00004500
         DS ~ CHR;                                                      00004600
         END GNCH;                                                      00004700
      ALPHA PROCEDURE GETCHAR;                                          00004800
         BEGIN                                                          00004900
         IF WIN } 9 THEN                                                00005000
            BEGIN                                                       00005100
            LABEL L1, L2;                                               00005200
            FORMAT FMT (13A6,A2);                                       00005300
            ALPHA ARRAY INFO[0:13];                                     00005400
            STREAM PROCEDURE CONCAT (INBUF, OUTBUF);                    00005500
               BEGIN                                                    00005600
               SI ~ INBUF;                                              00005700
               DI ~ OUTBUF;                                             00005800
               13 (SI ~ SI + 2;                                         00005900
               DS ~ 6 CHR);                                             00006000
               SI ~ SI + 6;                                             00006100
               DS ~ 2 CHR;                                              00006200
               END;                                                     00006300
            STREAM PROCEDURE GETIDENT (IDENT, BUFIMG);                  00006400
               BEGIN                                                    00006500
               SI ~ BUFIMG;                                             00006600
               DI ~ IDENT;                                              00006700
               DI ~ DI + 5;                                             00006800
               DS ~ 3 CHR                                               00006900
               END;                                                     00007000
            INTEGER I;                                                  00007100
            READ (KRUCARD, FMT, FOR I ~ 0 STEP 1 UNTIL 13 DO INFO[I]) [ 00007200
                 L1];                                                   00007300
            CONCAT (INFO[0], BUFIMG[0]);                                00007400
            IF FIRSTCARD THEN                                           00007500
               BEGIN                                                    00007600
               GETIDENT (IDENT, BUFIMG[9]);                             00007700
               FIRSTCARD ~ FALSE                                        00007800
               END;                                                     00007900
            GO TO L2;                                                   00008000
            L1:EOF ~ TRUE;                                              00008100
            L2:CIN ~ WIN ~ 0;                                           00008200
            END;                                                        00008300
         TUF1 ~ TUF2;                                                   00008400
         GETCHAR ~ TUF2 ~ GNCH (CR[WIN], CIN);                          00008500
         ID1 ~ ID;                                                      00008600
         ID ~ ALPHACHECK (TUF2) AND (ID OR TUF2 } 10);                  00008700
         CIN ~ CIN + 1;                                                 00008800
         IF CIN } 8 THEN                                                00008900
            BEGIN                                                       00009000
            WIN ~ WIN + 1;                                              00009100
            CIN ~ CIN - 8                                               00009200
            END;                                                        00009300
         END CHAR;                                                      00009400
      STREAM PROCEDURE PPCH (FO, COUT, CH);                             00009500
      VALUE COUT;                                                       00009600
         BEGIN                                                          00009700
         SI ~ CH;                                                       00009800
         SI ~ SI + 7;                                                   00009900
         DI ~ FO;                                                       00010000
         DI ~ DI + COUT;                                                00010100
         DS ~ CHR;                                                      00010200
         END;                                                           00010300
      PROCEDURE CHAROUT (CH);                                           00010400
      VALUE CH;                                                         00010500
      ALPHA CH;                                                         00010600
         BEGIN                                                          00010700
         IF WOUT } 9 THEN                                               00010800
            BEGIN                                                       00010900
            SEQUENCE (CP (9), SQ, IDENT);                               00011000
            SQ ~ SQ + 10;                                               00011100
            RELEASE (CP);                                               00011200
            COUT ~ WOUT ~ 0;                                            00011300
            END;                                                        00011400
         PPCH (CP (WOUT), COUT, CH);                                    00011500
         COUT ~ COUT + 1;                                               00011600
         IF COUT } 8 THEN                                               00011700
            BEGIN                                                       00011800
            WOUT ~ WOUT + 1;                                            00011900
            COUT ~ COUT - 8;                                            00012000
            END;                                                        00012100
         END CHAROUT;                                                   00012200
      PROCEDURE CLEAR;                                                  00012300
         BEGIN                                                          00012400
         INTEGER I;                                                     00012500
         FOR I ~ 0 WHILE WOUT < 9 DO CHAROUT (" ");                     00012600
         END;                                                           00012700
      PROCEDURE COMMENTCHECK (L);                                       00012800
      LABEL L;                                                          00012900
         BEGIN                                                          00013000
         LABEL NOPE;                                                    00013100
         INTEGER PTR, PWD;                                              00013200
         PTR ~ CIN;                                                     00013300
         PWD ~ WIN;                                                     00013400
         SAVE1 ~ T;                                                     00013500
         FOR CH ~ "O", "M", "M", "E", "N", "T" DO                       00013600
            BEGIN                                                       00013700
            IF WIN } 9 THEN GO TO NOPE;                                 00013800
            CHAR ~ GETCHAR;                                             00013900
            IF EOF THEN GO TO ENDIT;                                    00014000
            IF CH ! CHAR THEN GO TO NOPE;                               00014100
            END;                                                        00014200
         CHAR ~ GETCHAR;                                                00014300
         IF EOF THEN GO TO ENDIT;                                       00014400
         T ~ CHAR;                                                      00014500
         IF NOT ALPHACHECK (T) THEN FOR CH ~ T WHILE CHAR ! ";" DO      00014600
            BEGIN                                                       00014700
            CHAR ~ GETCHAR;                                             00014800
            IF EOF THEN GO TO ENDIT;                                    00014900
            T ~ CHAR                                                    00015000
            END                                                         00015100
         ELSE GO TO NOPE;                                               00015200
         GO TO L;                                                       00015300
         NOPE:WIN ~ PWD;                                                00015400
         CIN ~ PTR;                                                     00015500
         ID ~ ID1 ~ TRUE;                                               00015600
         T ~ SAVE1;                                                     00015700
         END;                                                           00015800
      WIN ~ 10;                                                         00015900
      FIRSTCARD ~ TRUE;                                                 00016000
      EOF ~ FALSE;                                                      00016100
      TUF1 ~ TUF2 ~ ";";                                                00016200
      SQ ~ 10;                                                          00016300
      L:CHAR ~ GETCHAR;                                                 00016400
      IF EOF THEN GO TO ENDIT;                                          00016500
      T ~ CHAR;                                                         00016600
      IF T = """ THEN                                                   00016700
         BEGIN                                                          00016800
         N:CHAR ~ GETCHAR;                                              00016900
         IF EOF THEN GO TO ENDIT;                                       00017000
         T ~ CHAR;                                                      00017100
         IF T = """ THEN                                                00017200
            BEGIN                                                       00017300
            CHAR ~ GETCHAR;                                             00017400
            IF EOF THEN GO TO ENDIT;                                    00017500
            IF CHAR ! """ THEN                                          00017600
               BEGIN                                                    00017700
               ENDIT:CLEAR;                                             00017800
               SEQUENCE (CP (9), SQ, IDENT);                            00017900
               RELEASE (CP);                                            00018000
               GO TO N1;                                                00018100
               END;                                                     00018200
            FOR I ~ 1,2,3 DO CHAROUT (""");                             00018300
            GO TO L;                                                    00018400
            END;                                                        00018500
         CHAROUT (""");                                                 00018600
         CHAROUT (T);                                                   00018700
         K:CHAR ~ GETCHAR;                                              00018800
         IF EOF THEN GO TO ENDIT;                                       00018900
         T ~ CHAR;                                                      00019000
         CHAROUT (T);                                                   00019100
         IF T ! """ THEN GO TO K;                                       00019200
         CHAR ~ GETCHAR;                                                00019300
         IF EOF THEN GO TO ENDIT;                                       00019400
         T ~ CHAR;                                                      00019500
         END;                                                           00019600
      IF T = " " THEN                                                   00019700
         BEGIN                                                          00019800
         ID2 ~ ID1;                                                     00019900
         M:CHAR ~ GETCHAR;                                              00020000
         IF EOF THEN GO TO ENDIT;                                       00020100
         T ~ CHAR;                                                      00020200
         IF T = " " THEN GO TO M;                                       00020300
         IF ID2 AND ALPHACHECK (T) THEN CHAROUT (" ");                  00020400
         IF T = """ THEN GO TO N;                                       00020500
         END;                                                           00020600
      IF T = "$" AND 8 | WIN + CIN = 1 THEN                             00020700
         BEGIN                                                          00020800
         IF 8 | WOUT + COUT ! 0 THEN                                    00020900
            BEGIN                                                       00021000
            CLEAR;                                                      00021100
            WOUT ~ 10                                                   00021200
            END;                                                        00021300
         CHAROUT ("$");                                                 00021400
         FOR I ~ 1 STEP 1 UNTIL 71 DO                                   00021500
            BEGIN                                                       00021600
            CHAR ~ GETCHAR;                                             00021700
            IF EOF THEN GO TO ENDIT;                                    00021800
            CHAROUT ((T ~ CHAR));                                       00021900
            END;                                                        00022000
         GO TO L;                                                       00022100
         END;                                                           00022200
      IF T = "C" AND NOT ALPHACHECK (TUF1) THEN COMMENTCHECK (L);       00022300
      CHAROUT (T);                                                      00022400
      GO TO L;                                                          00022500
      N1:END.                                                           00022600
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
