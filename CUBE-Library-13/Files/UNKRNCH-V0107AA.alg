UNKRUNCH    000107AA                                                    00000100
                                                                        00000200
                                                                        00000300
************************************************************************00000400
           PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES           00000500
     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00000600
***********************************************************************;00000700
% UNKRUNCH/UTILITY PRODUCES A NEW SOURCE DECK OF ALGOL, REFORMATTED     00000800
% FOR BETTER READABILITY.                                               00000900
% THE CONTROL CARD FOR THIS PROGRAM IS FREE FIELD AND SETS THE          00001000
%        SWITCH FILES FOR THE INPUT AND OUTPUT.                         00001100
%        THE FIRST NUMBER IS FOR THE INPUT.                             00001200
%        INPUT = 0 THEN A CARD DECK IS EXPECTED AFTER THE CONTROL CARD  00001300
%        INPUT = 1 THEN A 0CRDIMG TAPE IS EXPECTED                      00001400
%        OUTPUT = 0 THEN THE CARD PUNCH WILL MAKE A NEW SEQUENCED DECK  00001500
%        OUTPUT = 1 THEN A NEW RESEQUENCED 0CRDIMG TAPE IS MADE         00001600
%        OUTPUT = 2 THE OUTPUT APPEARS ON THE LINE PRINTER.             00001700
%                                                                       00001800
BEGIN                                                                   00001900
   FILE CARD(5,10,30),                                                  00002000
        TAPE "0CRDIMG" (2,56,10),                                       00002100
        LINE 18(2,15),                                                  00002200
        PUNCH 0(5,10),                                                  00002300
        NEWTAPE "0CRDIMG" (2,56,10,SAVE 1);                             00002400
   INTEGER I, J, PAREN, WIN, CIN, WBUF, CBUF, WOUT, COUT, SQ, SCOL,     00002500
        TEMP;                                                           00002600
   ALPHA PARM, T, FILER, CHAR, BLKBGN, BLKEND, ELSER, SEMICO, COMMA,    00002700
        RTPAREN, LISTR, FRMATR, LFPAREN;                                00002800
   ARRAY BUFIMG [0 : 9], CP [0 : 9], BUF [0 : 9];                       00002900
   BOOLEAN COMA, DECL, NUMSW, FLORMAT, SEMI, ENDR;                      00003000
   SWITCH FILE INPUT := CARD,                                           00003100
        TAPE;                                                           00003200
   SWITCH FILE OUTPUT := PUNCH,                                         00003300
        NEWTAPE,                                                        00003400
        LINE;                                                           00003500
   LABEL ENDIT, ST;                                                     00003600
   BOOLEAN STREAM PROCEDURE BLANK (CP);                                 00003700
   BEGIN                                                                00003800
      LABEL L7;                                                         00003900
      TALLY ~ 0;                                                        00004000
      SI ~ CP;                                                          00004100
      2 (36 (IF SC ! " " THEN JUMP OUT 2 TO L7;                         00004200
      SI ~ SI + 1));                                                    00004300
      TALLY ~ 1;                                                        00004400
      L7: BLANK ~ TALLY;                                                00004500
   END BLANK;                                                           00004600
   ALPHA STREAM PROCEDURE GNCH (CR, CIN);                               00004700
   VALUE CIN;                                                           00004800
   BEGIN                                                                00004900
      SI ~ CR;                                                          00005000
      SI ~ SI + CIN;                                                    00005100
      DI ~ LOC GNCH;                                                    00005200
      DI ~ DI + 7;                                                      00005300
      DS ~ CHR;                                                         00005400
   END GNCH;                                                            00005500
   ALPHA PROCEDURE GETCHAR;                                             00005600
   BEGIN                                                                00005700
      IF CIN < 0 THEN                                                   00005800
      BEGIN                                                             00005900
         CIN ~ CIN + 8;                                                 00006000
         WIN ~ WIN - 1                                                  00006100
      END;                                                              00006200
      IF WIN > 8 THEN                                                   00006300
      BEGIN                                                             00006400
         READ (INPUT [I], 10, BUFIMG [*]) [ENDIT];                      00006500
         WIN ~ CIN ~ 0;                                                 00006600
      END;                                                              00006700
      GETCHAR ~ GNCH (BUFIMG [WIN], CIN);                               00006800
      CIN ~ CIN + 1;                                                    00006900
      IF CIN > 7 THEN                                                   00007000
      BEGIN                                                             00007100
         WIN ~ WIN + 1;                                                 00007200
         CIN ~ CIN - 8;                                                 00007300
      END;                                                              00007400
   END GETCHAR;                                                         00007500
   STREAM PROCEDURE BUFTOOUT (BUF, TEMP, CP, COUT);                     00007600
   VALUE TEMP, COUT;                                                    00007700
   BEGIN                                                                00007800
      SI ~ BUF;                                                         00007900
      DI ~ CP;                                                          00008000
      DI ~ DI + COUT;                                                   00008100
      DS ~ TEMP CHR;                                                    00008200
   END BUFTOOUT;                                                        00008300
   BOOLEAN STREAM PROCEDURE COMEQUAL (BUF);                             00008400
   BEGIN                                                                00008500
      LOCAL NIC;                                                        00008600
      TALLY ~ 0;                                                        00008700
      SI ~ BUF;                                                         00008800
      DI ~ LOC NIC;                                                     00008900
      DS ~ 8 LIT "COMMENT ";                                            00009000
      DI ~ DI - 8;                                                      00009100
      IF 8 SC = DC THEN TALLY ~ 1;                                      00009200
      COMEQUAL ~ TALLY;                                                 00009300
   END COMEQUAL;                                                        00009400
   BOOLEAN STREAM PROCEDURE EQUAL (BUF, COUNT, T);                      00009500
   VALUE COUNT;                                                         00009600
   BEGIN                                                                00009700
      LABEL L1;                                                         00009800
      TALLY ~ 0;                                                        00009900
      SI ~ BUF;                                                         00010000
      DI ~ T;                                                           00010100
      DI ~ DI + 2;                                                      00010200
      COUNT (IF SC ! DC THEN JUMP OUT 1 TO L1);                         00010300
      TALLY ~ 1;                                                        00010400
      L1: EQUAL ~ TALLY;                                                00010500
   END EQUAL;                                                           00010600
   STREAM PROCEDURE CB (T, BUF, CBUF);                                  00010700
   VALUE CBUF;                                                          00010800
   BEGIN                                                                00010900
      SI ~ T;                                                           00011000
      SI ~ SI + 7;                                                      00011100
      DI ~ BUF;                                                         00011200
      DI ~ DI + CBUF;                                                   00011300
      DS ~ CHR;                                                         00011400
   END CB;                                                              00011500
   PROCEDURE CHARTOBUF (T);                                             00011600
   VALUE T;                                                             00011700
   ALPHA T;                                                             00011800
   BEGIN                                                                00011900
      CB (T, BUF [WBUF], CBUF);                                         00012000
      CBUF ~ CBUF + 1;                                                  00012100
      IF CBUF > 7 THEN                                                  00012200
      BEGIN                                                             00012300
         WBUF ~ WBUF + 1;                                               00012400
         CBUF ~ CBUF - 8;                                               00012500
      END;                                                              00012600
   END CHARTOBUF;                                                       00012700
   BOOLEAN STREAM PROCEDURE ALPHACHECK (T);                             00012800
   BEGIN                                                                00012900
      SI ~ T;                                                           00013000
      SI ~ SI + 7;                                                      00013100
      IF SC = ALPHA THEN TALLY ~ 1 ELSE TALLY ~ 0;                      00013200
      ALPHACHECK ~ TALLY;                                               00013300
   END ALPHACHECK;                                                      00013400
   STREAM PROCEDURE CLEARBUF (BUF);                                     00013500
   BEGIN                                                                00013600
      DI ~ BUF;                                                         00013700
      2 (DS ~ 40 LIT " ");                                              00013800
   END CLEARBUF;                                                        00013900
   STREAM PROCEDURE SEQUENCE (FO, SQ);                                  00014000
   BEGIN                                                                00014100
      DI ~ FO;                                                          00014200
      2 (DI ~ DI + 36);                                                 00014300
      SI ~ SQ;                                                          00014400
      DS ~ 8 DEC;                                                       00014500
   END SEQUENCE;                                                        00014600
   PROCEDURE DROP;                                                      00014700
   BEGIN                                                                00014800
      IF NOT BLANK (CP[0]) THEN                                         00014900
      BEGIN                                                             00015000
         SEQUENCE (CP [0], SQ);                                         00015100
         SQ ~ SQ + 100;                                                 00015200
         WRITE (OUTPUT[J] [DBL], 10, CP[ *]);                           00015300
         CLEARBUF (CP);                                                 00015400
      END;                                                              00015500
      WOUT ~ SCOL DIV 8;                                                00015600
      COUT ~ SCOL MOD 8;                                                00015700
   END DROP;                                                            00015800
   PROCEDURE PERCENT;                                                   00015900
   BEGIN                                                                00016000
      INTEGER NUMCHAR, REMCHAR;                                         00016100
      STREAM PROCEDURE MVCHAR (NUMB, SOURCE, SSKIP, DEST, DSKIP);       00016200
      VALUE NUMB, SSKIP, DSKIP;                                         00016300
      BEGIN                                                             00016400
         SI ~ SOURCE;                                                   00016500
         SI ~ SI + SSKIP;                                               00016600
         DI ~ DEST;                                                     00016700
         DI ~ DI + DSKIP;                                               00016800
         DS ~ NUMB CHR;                                                 00016900
      END MVCHAR;                                                       00017000
      IF (CIN ~ CIN - 1) < 0 THEN                                       00017100
      BEGIN                                                             00017200
         CIN ~ CIN + 8;                                                 00017300
         WIN ~ WIN - 1;                                                 00017400
      END;                                                              00017500
      DROP;                                                             00017600
      NUMCHAR ~ 72 - (8 | WIN + CIN);                                   00017700
      IF (REMCHAR ~ NUMCHAR - 40) > 0 THEN                              00017800
      BEGIN                                                             00017900
         MVCHAR (40, BUFIMG [WIN], CIN, CP [WIN], CIN);                 00018000
         MVCHAR (REMCHAR, BUFIMG [WIN + 5], CIN, CP [WIN + 5], CIN);    00018100
      END                                                               00018200
      ELSE MVCHAR (NUMCHAR, BUFIMG [WIN], CIN, CP[WIN], CIN);           00018300
      DROP;                                                             00018400
      WIN ~ 9;                                                          00018500
      T ~ " ";                                                          00018600
   END PERCENT;                                                         00018700
   PROCEDURE COMSCAN;                                                   00018800
COMMENT   THIS PROCECURE HANDLES THE OUTPUTTING OF COMMENTS THAT        00018900
          APPEAR IN THE PROGRAM, PLACING THEM IN THE SAME POSITION      00019000
          THAT THEY HAD IN THE ORIGINAL CARD;                           00019100
   BEGIN                                                                00019200
      LABEL FOO;                                                        00019300
      INTEGER COL;                                                      00019400
      COL ~ SCOL;                                                       00019500
      SCOL ~ 0;                                                         00019600
      CLEARBUF (CP);                                                    00019700
      WIN ~ WIN - 1;                                                    00019800
      COUT ~ CIN;                                                       00019900
      WOUT ~ WIN;                                                       00020000
      FOO:T ~ GETCHAR;                                                  00020100
      IF WOUT > 8 THEN DROP;                                            00020200
      CB (T, CP [WOUT], COUT);                                          00020300
      COUT ~ COUT + 1;                                                  00020400
      WOUT ~ WOUT + COUT DIV 8;                                         00020500
      COUT ~ COUT MOD 8;                                                00020600
      IF T ! ";" THEN GO TO FOO;                                        00020700
      T ~ GETCHAR;                                                      00020800
      SCOL ~ COL;                                                       00020900
      DROP;                                                             00021000
   END COMSCAN;                                                         00021100
   PROCEDURE CREAD;                                                     00021200
   BEGIN                                                                00021300
      LABEL N1, N2, DONE, C1, Q1, ST;                                   00021400
      CBUF ~ WBUF ~ 0;                                                  00021500
      CLEARBUF (BUF);                                                   00021600
      CIN ~ CIN - 1;                                                    00021700
      ST: T ~ GETCHAR;                                                  00021800
      IF ALPHACHECK (T) THEN                                            00021900
      BEGIN                                                             00022000
         IF T < 10 THEN                                                 00022100
         BEGIN                                                          00022200
            NUMSW ~ TRUE;                                               00022300
            N1: CHARTOBUF (T);                                          00022400
            T ~ GETCHAR;                                                00022500
            IF ALPHACHECK (T) AND T > 9 AND NUMSW AND NOT FLORMAT THEN  00022600
            BEGIN                                                       00022700
               NUMSW ~ FALSE;                                           00022800
               GO TO DONE;                                              00022900
            END;                                                        00023000
            IF ALPHACHECK (T) THEN GO TO N1;                            00023100
            IF T = "." OR T = ":" OR T = "," THEN GO TO N1;             00023200
            IF T = "@" THEN                                             00023300
            BEGIN                                                       00023400
               CHARTOBUF (T);                                           00023500
               T ~ GETCHAR;                                             00023600
               N2: CHARTOBUF (T);                                       00023700
               T ~ GETCHAR;                                             00023800
               IF T < 10 THEN GO TO N2;                                 00023900
            END;                                                        00024000
            GO TO DONE;                                                 00024100
         END;                                                           00024200
         C1:CHARTOBUF (T);                                              00024300
         T ~ GETCHAR;                                                   00024400
         IF ALPHACHECK (T) OR T = "." OR T = "[" OR T = ":" THEN GO TO  00024500
              C1;                                                       00024600
         IF T = " " THEN GO TO DONE;                                    00024700
         IF T = "." THEN                                                00024800
         BEGIN                                                          00024900
            CHARTOBUF (".");                                            00025000
            T ~ GETCHAR;                                                00025100
         END;                                                           00025200
         IF T = ":" THEN                                                00025300
         BEGIN                                                          00025400
            CHARTOBUF (":");                                            00025500
            T ~ GETCHAR;                                                00025600
         END;                                                           00025700
         GO TO DONE;                                                    00025800
      END;                                                              00025900
      IF T = """ THEN                                                   00026000
      BEGIN                                                             00026100
         T ~ GETCHAR;                                                   00026200
         IF T = """ THEN                                                00026300
         BEGIN                                                          00026400
            T ~ GETCHAR;                                                00026500
            IF T ! """ THEN GO TO ENDIT;                                00026600
            CHARTOBUF (""");                                            00026700
            CHARTOBUF (""");                                            00026800
            CHARTOBUF (""");                                            00026900
            T ~ GETCHAR;                                                00027000
            GO TO DONE;                                                 00027100
         END;                                                           00027200
         CHARTOBUF (""");                                               00027300
         Q1: CHARTOBUF (T);                                             00027400
         T ~ GETCHAR;                                                   00027500
         IF T ! """ THEN GO TO Q1;                                      00027600
         CHARTOBUF (""");                                               00027700
         T ~ GETCHAR;                                                   00027800
         GO TO DONE;                                                    00027900
      END;                                                              00028000
      IF T = "%" THEN PERCENT;                                          00028100
      IF T = " " THEN GO TO ST;                                         00028200
      IF T = "," OR T = ";" OR T = ")" OR T = "]" THEN COUT ~ COUT - 1; 00028300
      IF COUT < 0 THEN                                                  00028400
      BEGIN                                                             00028500
         COUT ~ COUT + 8;                                               00028600
         WOUT ~ WOUT - 1;                                               00028700
      END;                                                              00028800
      CHARTOBUF (T);                                                    00028900
      PARM ~ GETCHAR;                                                   00029000
      IF T = ";" THEN FLORMAT ~ FALSE;                                  00029100
      IF T ! "." AND T ! "[" AND T ! "(" THEN DONE : CHARTOBUF (" ");   00029200
   END CREAD;                                                           00029300
%***********************************************************************00029400
   READ (CARD, /, I, J);                                                00029500
   IF I = 1 THEN CLOSE (CARD, RELEASE);                                 00029600
   FILER ~ "FILE  ";                                                    00029700
   BLKBGN ~ "BEGIN ";                                                   00029800
   BLKEND ~ "END . ";                                                   00029900
   ELSER ~ "ELSE  ";                                                    00030000
   SEMICO ~ ";     ";                                                   00030100
   COMMA ~ ",     ";                                                    00030200
   RTPAREN ~ ")     ";                                                  00030300
   LFPAREN ~ "(     ";                                                  00030400
   LISTR ~ "LIST  ";                                                    00030500
   FRMATR ~ "FORMAT";                                                   00030600
   SEMI ~ COMA ~ DECL ~ FALSE;                                          00030700
   PAREN ~ 0;                                                           00030800
   WOUT ~ WBUF ~ CBUF ~ 0;                                              00030900
   CIN ~ 1;                                                             00031000
   WIN ~ 10;                                                            00031100
   CLEARBUF (CP);                                                       00031200
   CLEARBUF (BUF);                                                      00031300
   SQ ~ 100;                                                            00031400
   ST: CREAD;                                                           00031500
   TEMP ~ 8 | WBUF + CBUF;                                              00031600
   IF EQUAL (BUF, 6, BLKBGN) THEN                                       00031700
   BEGIN                                                                00031800
      DROP;                                                             00031900
      IF SEMI THEN SCOL ~ SCOL - 2 ELSE SCOL ~ SCOL + 3;                00032000
      SEMI ~ FALSE;                                                     00032100
   END;                                                                 00032200
   IF EQUAL (BUF, TEMP, BLKEND) THEN                                    00032300
   BEGIN                                                                00032400
      IF SEMI THEN SCOL ~ SCOL - 5;                                     00032500
      SCOL ~ SCOL - 3;                                                  00032600
      DROP;                                                             00032700
      SEMI ~ FALSE;                                                     00032800
      ENDR ~ TRUE;                                                      00032900
   END;                                                                 00033000
   IF ENDR AND EQUAL (BUF, TEMP, ELSER) THEN DROP;                      00033100
   IF TEMP + COUT + 8 | WOUT > 72 THEN                                  00033200
   BEGIN                                                                00033300
      IF NOT SEMI THEN SCOL ~ SCOL + 5;                                 00033400
      SEMI ~ TRUE;                                                      00033500
      DROP;                                                             00033600
   END;                                                                 00033700
   BUFTOOUT (BUF, TEMP, CP [WOUT], COUT);                               00033800
   COUT ~ COUT + TEMP;                                                  00033900
   WOUT ~ WOUT + COUT DIV 8;                                            00034000
   COUT ~ COUT MOD 8;                                                   00034100
   IF COMEQUAL (BUF) THEN COMSCAN;                                      00034200
   IF EQUAL (BUF, TEMP, LISTR) THEN DECL ~ TRUE;                        00034300
   IF EQUAL (BUF, 6, FRMATR) THEN DECL ~ FLORMAT ~ TRUE;                00034400
   IF EQUAL (BUF, TEMP, FILER) THEN DECL ~ TRUE;                        00034500
   IF DECL THEN                                                         00034600
   BEGIN                                                                00034700
      IF NOT SEMI THEN SCOL ~ SCOL + 5;                                 00034800
      SEMI ~ TRUE;                                                      00034900
      IF NOT COMA THEN SCOL ~ SCOL + 3;                                 00035000
      COMA ~ TRUE;                                                      00035100
      IF EQUAL (BUF, TEMP, LFPAREN) THEN PAREN ~ PAREN + 1;             00035200
      IF EQUAL (BUF, TEMP, RTPAREN) THEN PAREN ~ PAREN - 1;             00035300
      IF PAREN = 0 AND EQUAL (BUF, TEMP, COMMA) THEN                    00035400
      BEGIN                                                             00035500
         SCOL ~ SCOL - 3;                                               00035600
         COMA ~ FALSE;                                                  00035700
         DROP;                                                          00035800
      END;                                                              00035900
   END;                                                                 00036000
   IF EQUAL (BUF, TEMP, SEMICO) THEN                                    00036100
   BEGIN                                                                00036200
      IF SEMI THEN SCOL ~ SCOL - 5;                                     00036300
      IF COMA THEN SCOL ~ SCOL - 3;                                     00036400
      COMA ~ DECL ~ FLORMAT ~ ENDR ~ SEMI ~ FALSE;                      00036500
      DROP;                                                             00036600
   END;                                                                 00036700
   IF EQUAL (BUF, 6, BLKBGN) THEN DROP;                                 00036800
   GO TO ST;                                                            00036900
   ENDIT: DROP;                                                         00037000
END .                                                                   00037100
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
