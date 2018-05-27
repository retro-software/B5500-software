COPARE      000100AA                                                    00000100
                                                                        00000200
                                                                        00000300
  **********************************************************************00000400
            PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES          00000500
      NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.     00000600
  **********************************************************************00000700
%                                                                       00000800
   THIS PROGRAM COMPARES TWO MULTIFILE TAPES TO SEE IF THEY ARE         00000900
IDENTICAL. ALL OF THE FILES ON THE MASTER TAPE MUST BE LABEL-EQUATED    00001000
TO A "T" FILE AND ALL OF THE FILES ON THE TAPE BEING CHECKED MUST       00001100
BE LABEL-EQUATED TO AN "N" FILE. FOR EXAMPLE:                           00001200
      FILE T1=A/B; FILE N1=A/C                                          00001300
      FILE T2=B/B; FILE N2=B/C                                          00001400
       ETC.                                                             00001500
ONE DATA CARD MUST BE INCLUDED AS INPUT. IT SHOULD BE IN FREE-FIELD     00001600
FORMAT. THE FIRST NUMBER MUST BE THE NUMBER OF FILES ON THE TAPE BEING  00001700
CHECKED, FOLLOWED BY A COMMA. AND THE SECOND NUMBER MUST BE ONE         00001800
OR ZERO, FOLLOWED BY A COMMA. IF THE SECOND NUMBER IS A ZERO,           00001900
COPARE WILL SIMPLY COMPARE THE TAPES TO DETERMINE IF THEY ARE IDENTICAL.00002000
IF THE SECOND NUMBER IS A ONE, THEN COPARE WILL FIRST DUPLICATE THE     00002100
MASTER TAPE, AND THEN WILL COMPARE THE MASTER AND THE NEWLY             00002200
CREATED TAPE TO DETERMINE IF THEY ARE IDENTICAL.                        00002300
%                                                                       00002350
BEGIN                                                                   00002400
   FILE IN C (1,12);                                                    00002500
   LABEL RETRY;                                                         00002600
   REAL II;                                                             00002700
   LABEL IRA;                                                           00002800
   FILE IN T1 (2,56);                                                   00002900
   FILE IN T2 (2,56);                                                   00003000
   FILE IN T3 (2,56);                                                   00003100
   FILE IN T4 (2,56);                                                   00003200
   FILE IN T5 (2,56);                                                   00003300
   FILE IN T6 (2,56);                                                   00003400
   FILE IN T7 (2,56);                                                   00003500
   FILE IN T8 (2,56);                                                   00003600
   FILE IN T9 (2,56);                                                   00003700
   FILE IN T10 (2,56);                                                  00003800
   FILE IN T11 (2,56);                                                  00003900
   FILE IN T12 (2,56);                                                  00004000
   FILE IN T13 (2,56);                                                  00004100
   FILE IN T14 (2,56);                                                  00004200
   FILE IN T15 (2,56);                                                  00004300
   FILE OUT N1 (2,56, SAVE 100);                                        00004400
   FILE OUT N2 (2,56, SAVE 100);                                        00004500
   FILE OUT N3 (2,56, SAVE 100);                                        00004600
   FILE OUT N4 (2,56, SAVE 100);                                        00004700
   FILE OUT N5 (2,56, SAVE 100);                                        00004800
   FILE OUT N6 (2,56, SAVE 100);                                        00004900
   FILE OUT N7 (2,56, SAVE 100);                                        00005000
   FILE OUT N8 (2,56, SAVE 100);                                        00005100
   FILE OUT N9 (2,56, SAVE 100);                                        00005200
   FILE OUT N10 (2,56, SAVE 100);                                       00005300
   FILE OUT N11 (2,56, SAVE 100);                                       00005400
   FILE OUT N12 (2,56, SAVE 100);                                       00005500
   FILE OUT N13 (2,56, SAVE 100);                                       00005600
   FILE OUT N14 (2,56, SAVE 100);                                       00005700
   FILE OUT N15 (2,56, SAVE 100);                                       00005800
   SWITCH FILE T ~ C,                                                   00005900
        T1,                                                             00006000
        T2,                                                             00006100
        T3,                                                             00006200
        T4,                                                             00006300
        T5,                                                             00006400
        T6,                                                             00006500
        T7,                                                             00006600
        T8,                                                             00006700
        T9,                                                             00006800
        T10,                                                            00006900
        T11,                                                            00007000
        T12,                                                            00007100
        T13,                                                            00007200
        T14,                                                            00007300
        T15;                                                            00007400
   SWITCH FILE N ~ C,                                                   00007500
        N1,                                                             00007600
        N2,                                                             00007700
        N3,                                                             00007800
        N4,                                                             00007900
        N5,                                                             00008000
        N6,                                                             00008100
        N7,                                                             00008200
        N8,                                                             00008300
        N9,                                                             00008400
        N10,                                                            00008500
        N11,                                                            00008600
        N12,                                                            00008700
        N13,                                                            00008800
        N14,                                                            00008900
        N15;                                                            00009000
   INTEGER I, J, COPY;                                                  00009100
   ARRAY A[0:55];                                                       00009200
   LABEL L, EOF;                                                        00009300
   PROCEDURE COMPARETAPES;                                              00009400
   BEGIN                                                                00009500
      FORMAT OA6A2 (A6, A2 / 10 (A6, A2)),                              00009600
           IA6A2 (56 (A6, A2));                                         00009700
      ARRAY Y[0:111];                                                   00009800
      LIST X (FOR I ~ 0 STEP 1 UNTIL 111 DO Y[I]);                      00009900
      PROCEDURE REPOSITION;                                             00010000
      BEGIN                                                             00010100
         SPACE (T[II], - 1);                                            00010200
         CLOSE (T[II], *);                                              00010300
         SPACE (N[II], - 1);                                            00010400
         CLOSE (N[II], *);                                              00010500
      END;                                                              00010600
      FORMAT BAD ("***COPARE/UTILITY: TAPES DID NOT COMPARE ON FILES",  00010700
              I2);                                                      00010800
      FORMAT GOOD ("***COPARE/UTILITY: TAPES COMPARED A-OK");           00010900
      FILE SPO 11 (1,10);                                               00011000
      FILE LP 4 (1,15);                                                 00011100
      LABEL EOF, L, BOMBOUT;                                            00011200
      ARRAY B[0:55];                                                    00011300
      FORMAT FMT (O);                                                   00011400
      BOOLEAN STREAM PROCEDURE BADCOMPARE (A, B);                       00011500
      BEGIN                                                             00011600
         SI ~ A;                                                        00011700
         DI ~ B;                                                        00011800
         8 (IF 56 SC ! DC THEN                                          00011900
         BEGIN                                                          00012000
            TALLY ~ 1;                                                  00012100
            JUMP OUT;                                                   00012200
         END;                                                           00012300
        );                                                              00012400
         BADCOMPARE ~ TALLY;                                            00012500
      END;                                                              00012600
      FOR I ~ 1 STEP 1 UNTIL J DO CLOSE (N[I], SAVE);                   00012700
      SPACE (N[1], 1);                                                  00012800
      SPACE (N[1], - 1);                                                00012900
      FOR I ~ 1 STEP 1 UNTIL J DO CLOSE (T[I], SAVE);                   00013000
      FOR I ~ 1 STEP 1 UNTIL J DO                                       00013100
      BEGIN                                                             00013200
         L: READ (T[I], 56, A[ *]) [EOF];                               00013300
         READ (N[I], 56, B[ *]) [EOF];                                  00013400
         IF BADCOMPARE (A, B) THEN                                      00013500
         BEGIN                                                          00013600
            II ~ I;                                                     00013700
            WRITE (SPO, BAD, I);                                        00013800
            BEGIN                                                       00013900
               READ (A[ *], IA6A2, X);                                  00014000
               WRITE (LP, OA6A2, X);                                    00014100
            END;                                                        00014200
            WRITE (LP[DBL]);                                            00014300
            WRITE (LP[DBL]);                                            00014400
            BEGIN                                                       00014500
               READ (B[ *], IA6A2, X);                                  00014600
               WRITE (LP, OA6A2, X);                                    00014700
            END;                                                        00014800
            CLOSE (LP, RELEASE);                                        00014900
            REPOSITION;                                                 00015000
            GO TO RETRY;                                                00015100
         END;                                                           00015200
         GO TO L;                                                       00015300
         EOF: CLOSE (N[I], *);                                          00015400
         IF I < J THEN                                                  00015500
         BEGIN                                                          00015600
            SPACE (N[I + 1], 1);                                        00015700
            SPACE (N[I + 1], - 1);                                      00015800
         END;                                                           00015900
         CLOSE (T[I], *);                                               00016000
      END;                                                              00016100
      WRITE (SPO, GOOD);                                                00016200
      BOMBOUT:                                                          00016300
   END COMPARETAPES;                                                    00016400
   READ (C, /, J, COPY);                                                00016500
   CLOSE (C, RELEASE);                                                  00016600
   II ~ 1;                                                              00016700
   IF COPY = 0 THEN GO TO IRA;                                          00016800
   RETRY: FOR I ~ II STEP 1 UNTIL J DO                                  00016900
   BEGIN                                                                00017000
      L: READ (T[I], 56, A[ *]) [EOF];                                  00017100
      WRITE (N[I], 56, A[ *]);                                          00017200
      GO TO L;                                                          00017300
      EOF: IF I < J THEN                                                00017400
      BEGIN                                                             00017500
         CLOSE (T[I], *);                                               00017600
         CLOSE (N[I], *);                                               00017700
      END;                                                              00017800
   END;                                                                 00017900
   IRA: COMPARETAPES;                                                   00018000
   END.                                                                 00018100
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
