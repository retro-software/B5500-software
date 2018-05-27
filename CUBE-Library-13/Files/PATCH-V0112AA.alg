PATCH       000112AA                                                    00000100
                                                                        00000200
   ELIMINATES THE NEED OF INTERPRETING, COLLATING, AND MERGING          00000300
   PATCHES TO SOFTWARE RECEIVED FROM BURROUGHS FOR THE B 5500.          00000400
   SIMPLY TAKE THE ENTIRE PATCH DECK, PRECEDE IT WITH AN "EXECUTE"      00000500
   CARD AND A "DATA CRD" CARD, AND FOLLOW IT WITH AN "END" CARD.        00000600
   THIS PROGRAM WILL ACCEPT THE PATCH DECK AS INPUT, LIST IT            00000700
   ACCORDING TO PATCH NUMBER, NUMBER OF CARDS PER PATCH, AND TOTAL      00000800
   NUMBER OF CARDS IN THE PATCH DECK. THEN IT WILL SORT THE             00000900
   PATCH DECK ACCORDING TO SEQUENCE NUMBER AND PATCH NUMBER.            00001000
   THE PROGRAM WILL THEN LOOK FOR A SYMBOLIC TAPE FILE LABELED          00001100
   "CII". THIS FILE NAME SHOULD BE LABEL-EQUATED TO THE SOFTWARE        00001200
   TAPE FILE BEING PATCHED. FOR EXAMPLE:                                00001300
      EXECUTE PATCH/UTILITY                                             00001400
      FILE CII=SYMBOL/DCESPSY                                           00001500
      DATA CRD                                                          00001600
      .................PATCH DECK                                       00001700
      END                                                               00001800
                                                                        00001900
   THE ABOVE DECK SETUP WOULD BE USED TO UPDATE THE MCP WITH            00002000
   AN UPDATED PATCH DECK. THE PROGRAM WILL CREATE A NEW SYMBOL          00002100
   TAPE, THEN IT WILL COMPARE THE ORIGINAL AND THE NEW SYMBOL           00002200
   TAPE AND LIST ALL CARDS ADDED, DELETED, OR CHANGED.                  00002300
   THE PROGRAM WILL ACCEPT A PATCH DECK FROM DISK IF IT HAS             00002400
   BEEN WRITTEN ON DISK WITH THE FILE DECLARATION PARAMETERS            00002500
                      (X,10,30)                                         00002600
                                                                        00002700
                                                                        00002800
  **********************************************************************00002900
          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00003000
       NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.    00003100
  **********************************************************************00003200
                                                                        00003300
                                                                        00003400
         BEGIN %  DISK SYSTEM MAINTENANCE             B.L.LANGE         00003500
FILE CRD(2,10,30);                                                      00003600
FILE PBT 18(2,15);                                                      00003700
FILE CII   (2,56,10);                                                   00003800
FILE CIO  2(2,56,10,SAVE 1);                                            00003900
FILE DFO  DISK SERIAL [20:1000] (2,15,30);                              00004000
FORMAT HD1( 9("- - - - "),"SEQUENCE.  INPUT.  PATCH.  CARDS.  NOTES."); 00004100
ALPHA ARRAY AR[0:9], A, B, C, D[0:14];                                  00004200
INTEGER STREAM PROCEDURE COMPARE (KEY1,KEY2) ;                          00004300
         BEGIN                                                          00004400
               SI ~ KEY1 ; DI ~ KEY2 ;                                  00004500
               IF 8 SC > DC THEN TALLY ~ 2 ELSE                         00004600
         BEGIN                                                          00004700
               SI ~ SI - 8 ; DI ~ DI - 8 ;                              00004800
               IF 8 SC < DC THEN TALLY ~ 0 ELSE TALLY ~ 1 ;             00004900
         END; COMPARE ~ TALLY ;                                         00005000
         END COMPARE PROCEDURE ;                                        00005100
ALPHA STREAM PROCEDURE ALF(I);  BEGIN                                   00005200
              SI~I; DI~LOC ALF; DS~4LIT".   ";DS~4DEC; END;             00005300
INTEGER STREAM PROCEDURE COMPAR2 (KEY1,KEY2) ;                          00005400
         BEGIN                                                          00005500
               SI ~ KEY1 ; DI ~ KEY2 ;                                  00005600
               IF 16 SC > DC THEN TALLY ~ 2 ELSE                        00005700
         BEGIN                                                          00005800
               SI ~ SI -16 ; DI ~ DI -16 ;                              00005900
               IF 16 SC < DC THEN TALLY ~ 0 ELSE TALLY ~ 1 ;            00006000
         END; COMPAR2 ~ TALLY ;                                         00006100
         END COMPAR2 PROCEDURE ;                                        00006200
INTEGER STREAM PROCEDURE CRDIMG (OLD,NEW);                              00006300
         BEGIN SI ~ OLD; DI ~ NEW; TALLY ~ 0;                           00006400
               9(IF 8 SC ! DC THEN TALLY ~ 2); IF 8 SC ! DC THEN        00006500
               TALLY ~ TALLY + 1;  CRDIMG ~ TALLY;                      00006600
         END OF CRDIMG ;                                                00006700
STREAM PROCEDURE MOVE(N,A,B);       VALUE N;                            00006800
         BEGIN SI~A; DI~B; DS~ N WDS;                                   00006900
         END OF MOVE;                                                   00007000
PROCEDURE PATCH (P,F,M); FILE P,F,M;                                    00007100
    BEGIN     INTEGER C;  LABEL MG,EP,EF,EX;                            00007200
              B[10] ~ D[10] ~                                           00007300
              B[11] ~ B[12] ~ B[13] ~ B[14] ~                           00007400
              D[11] ~ D[12] ~ D[14] ~         AR[1];                    00007500
              D[13] ~ AR[5];                                            00007600
               WRITE(PBT[PAGE]);  WRITE(PBT[DBL],HD1);                  00007700
               READ( F ,10,D[*]);                                       00007800
              C~1;                                                      00007900
    MG:       IF C{1 THEN READ(P,10,A[*])[EP];                          00008000
              IF C}1 THEN BEGIN                                         00008100
               MOVE(10,D[0],B[0]); READ(F,10,D[*])[EF];                 00008200
               IF COMPARE(B[9],D[9]) } 1 THEN                           00008300
                BEGIN WRITE(PBT,15,B[*]); WRITE(PBT[DBL],15,D[*]);      00008400
                END; END;                                               00008500
              IF C}0 AND C{2 THEN C~COMPARE(A[9],B[9]);                 00008600
              IF C{1 THEN IF COMPARE(A[0],AR[8])=1 THEN C~9;            00008700
              IF C>1 AND C<9 THEN WRITE(M,10,B[*]);                     00008800
              IF C{1 THEN WRITE(M,10,A[*]);                             00008900
              IF C=9 THEN IF COMPARE(A[2], B[9])<1 THEN C~0;            00009000
              GO TO MG;                                                 00009100
    EP:       IF C<0 THEN GO TO EX; IF C<1 THEN WRITE(M,10,B[*]);       00009200
              C~3; GO TO MG;                                            00009300
    EF:       IF C>2 THEN GO TO EX;                                     00009400
              C~-1; GO TO MG;                                           00009500
    EX: END OF PATCH;                                                   00009600
BOOLEAN PROCEDURE INPUT (TEMP);  ARRAY TEMP[0];                         00009700
         BEGIN     LABEL RD,EF,EX;                                      00009800
              OWN INTEGER I, J, K;                                      00009900
RD:           READ(CRD,10,TEMP[*])[EF];                                 00010000
                                         TEMP[13]~TEMP[14]~AR[1];       00010100
              IF COMPARE(TEMP[9],AR[1]) < 1 THEN                        00010200
              BEGIN                                                     00010300
                   WRITE(PBT[PAGE]);                                    00010400
                   WRITE(PBT[ DBL],HD1);  J~J+1;  K~0;                  00010500
                   TEMP[10]~TEMP[11]~TEMP[12]~AR[1];                    00010600
                   WRITE(PBT[DBL],15,TEMP[*]);                          00010700
                   GO TO RD;                                            00010800
              END;                                                      00010900
               I~I+1; TEMP[10]~ALF(I); TEMP[11]~ALF(J);                 00011000
               K~K+1;  TEMP[12]~ALF(K);                                 00011100
              WRITE(PBT,15,TEMP[*]);  GO TO EX;                         00011200
EF:         INPUT ~ TRUE; CLOSE(CRD,RELEASE);                           00011300
EX:      END OF INPUT;                                                  00011400
PROCEDURE OUTPUT (EOF,B); VALUE EOF; ARRAY B[0]; BOOLEAN EOF;           00011500
         BEGIN     LABEL SE,EX; OWN BOOLEAN SW;                         00011600
              IF EOF THEN GO TO SE;                                     00011700
              IF NOT SW THEN BEGIN SW ~ TRUE;                           00011800
               WRITE(PBT[PAGE]);  WRITE(PBT[DBL],HD1);                  00011900
              MOVE(15,B[0],A[0]); GO TO EX; END;                        00012000
              IF COMPARE(A[9],B[9])!0 THEN BEGIN A[13] ~ AR[6];         00012100
          WRITE(PBT,15,A[*]); WRITE(PBT[DBL],15,B[*]); END              00012200
         ELSE WRITE(DFO,15,A[*]);                                       00012300
         MOVE(15,B[0],A[0]); GO TO EX;                                  00012400
SE:      WRITE(DFO,15,A[*]); REWIND(DFO);                               00012500
EX:      END OF OUTPUT;                                                 00012600
BOOLEAN PROCEDURE SORTA (A,B);  ARRAY A,B[0];                           00012700
              SORTA ~ COMPAR2(A[9],B[9]) { 1;                           00012800
PROCEDURE HIVALU (A);  ARRAY A[0];                                      00012900
              FILL A[*] WITH 0,0,0,0,0,0,0,0,0,OCT1414141414141414;     00013000
INTEGER X;                                                              00013100
LABEL RO, RN, LS, RP, RX, OM;                                           00013200
              FILL AR[*] WITH "~       ", ".       ", ".  NOP  ",       00013300
                              ".  ADD  ", ".  DIF  ", ".  SEQ  ",       00013400
                              ".  DUP  ", ". ERROR ", "$$  OMIT",       00013500
                              ".  OMIT ";                               00013600
              SORT(OUTPUT,INPUT,0,HIVALU,SORTA,15);                     00013700
              PATCH(DFO,CII,CIO);                                       00013800
              CLOSE(CII,SAVE); REWIND(CIO); REWIND(DFO);                00013900
               WRITE(PBT[PAGE]);  WRITE(PBT[DBL],HD1);                  00014000
                                 B[14]~AR[1];                           00014100
               READ(DFO,15,C[*])[RO];                                   00014200
    RO:        READ (CII,10,A[*])[RN];                                  00014300
    RN:        READ (CIO,10,B[*])[LS];                                  00014400
               IF CRDIMG(A[0],C[0]) = 0 THEN BEGIN C[13]~AR[2];         00014500
                    WRITE(PBT,15,C[*]);                                 00014600
                    READ(DFO,15,C[*])[RX];  END;                        00014700
    RX:        IF X ~ CRDIMG(A[0],B[0]) = 0 THEN GO TO RO;              00014800
               B[13] ~ IF X=3 THEN AR[3]  ELSE                          00014900
                       IF X=2 THEN AR[4] ELSE AR[5];                    00015000
               IF CRDIMG(C[0],B[0])!0 THEN B[13]~AR[7];                 00015100
               MOVE(3,C[10],B[10]);                                     00015200
               IF COMPARE(AR[8],C[0])=1 THEN  BEGIN                     00015300
                   A[10]~AR[9];                                         00015400
    OM:            WRITE(PBT,15,A[*]);                                  00015500
                   READ(CII,10,A[*]);                                   00015600
                   IF COMPARE(A[9],C[2]){1 THEN GO TO OM;               00015700
                   GO TO RX;  END;                                      00015800
               READ(DFO,15,C[*])[RP];  RP:                              00015900
               WRITE (PBT,15,B[*]);IF X=3 THEN GO RN; GO RO;            00016000
    LS:  END.                                                           00016100
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
