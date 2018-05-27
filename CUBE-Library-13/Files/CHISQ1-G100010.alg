 BEGIN                                                                  00000100
COMMENT        CUBE LIBRARY NUMBER IS G100010.  THE PROGRAM NAME IS     00000200
               "CHI/SQUARE".  THIS VERSION DATED 5/3/68;                00000300
ALPHA FILE IN DATACOM 14 (1,8);                                         00000400
ALPHA FILE OUT REMOTE 14 (1,8);                                         00000500
INTEGER     J,UNIT,CRLF,CANC,CR,LF,LA,TR,NUM;                           00000600
ARRAY     D[0:9];                                                       00000700
ALPHA     B;                                                            00000800
 BOOLEAN     OK, HELP,STOP;                                             00000900
FORMAT     FTS(X8,A6),                                                  00001000
           FT1(X8,A2," YOU BLEW IT THE PROGRAM ENDED",4A1,A3),          00001100
           FT2(X8,A2," LOG OUT AND PRESS CLR BUTTON",A3);               00001200
LIST     FORM( CRLF, CANC);                                             00001300
LABEL     EXT;                                                          00001400
     PROCEDURE READUNIT;                                                00001500
BEGIN                                                                   00001600
LABEL  ER,EXIT,RD;                                                      00001700
FORMAT     OKANS(X8, A3),                                               00001800
     FTERR(X8,A2,"TYPING ERRER TRY AGAIN",A3);                          00001900
  RD:  READ(DATACOM(UNIT),8,D[*])[:ER];                                 00002000
     WRITE(REMOTE(UNIT),OKANS, CANC);                                   00002100
     FILL D[*] WITH "     "; GO TO EXIT;                                00002200
ER:  WRITE(REMOTE(UNIT),FTERR,FORM);                                    00002300
    GO TO RD;                                                           00002400
EXIT:    END OF READUNIT;                                               00002500
                                                                        00002600
     PROCEDURE DATAOK (NEED);                                           00002700
INTEGER     NEED;                                                       00002800
BEGIN                                                                   00002900
INTEGER NO, I;                                                          00003000
ALPHA ARRAY     A[0:60];                                                00003100
LABEL     CONT;                                                         00003200
FORMAT     F1(X8, 56A1),                                                00003300
     ER1(X8,A2, "INV RECORD", A3);                                      00003400
LIST     L1( FOR I := 1 STEP 1 UNTIL 56 DO A[I] ),                      00003500
     FORM1(CRLF, CANC);                                                 00003600
COMMENT THIS PROGRAM FINDS THE NUMBER OF VALUES IN A INPUT              00003700
AND THEN CHECKS TO SEE IF IT IS ENOUGH OR TELLS THE PROGRAM;            00003800
     READ( D[*], F1, L1);                                               00003900
     STOP:=FALSE;                                                       00004000
     NO := 0; NEED := ABS(NEED); HELP := FALSE;                         00004100
I:=1; DO IF A[I] EQL 31 THEN GO TO CONT ELSE                            00004200
 IF A[I] EQL "H" AND A[I+1] EQL "E" AND A[I+2] EQL "L"                  00004300
 THEN HELP := TRUE ELSE                                                 00004400
     IF A[I] EQL "," THEN NO := NO + 1                                  00004500
ELSE IF A[I] EQL "S" AND A[I+1] EQL "T" AND A[I+2] EQL"O"               00004600
     AND A[I+3] EQL "P" THEN STOP:=TRUE                                 00004700
     UNTIL I := I + 1 EQL 57;                                           00004800
CONT: IF NEED EQL 3 THEN NEED := NO;                                    00004900
     IF NO GEQ NEED AND NO NEQ 0 THEN OK :=FALSE                        00005000
     ELSE OK := TRUE;                                                   00005100
     IF OK THEN WRITE(REMOTE(UNIT),ER1,FORM1);                          00005200
END OF DATAOK;                                                          00005300
     PROCEDURE CHISQUARE;                                               00005400
BEGIN                                                                   00005500
ARRAY     A[0:40,0:7];                                                  00005600
REAL     TI, TJ, CHI;                                                   00005700
INTEGER     I,J,NUM,NI,NJ;                                              00005800
LABEL    OVER,L1,L2;                                                    00005900
DEFINE     FORI = FOR I:= 1 STEP 1 UNTIL NI DO#,                        00006000
     FORJ = FOR J:= 1 STEP 1 UNTIL NJ DO#,                              00006100
     FE = (( A[0,J] | A[I,0] ) / TJ ) #,                                00006200
     ALLOK = IF HELP THEN GO TO OVER ELSE                               00006300
     IF STOP THEN GO TO EXT #,                                          00006400
     WR = WRITE(REMOTE(UNIT), #;                                        00006500
FORMAT FTC(X8,A2,"NO OF COLUMNS =",A1),                                 00006600
     FTR(X8,"NO OF ROWS =",A1),                                         00006700
     F1(X8,A2,"THIS PROGRAM FINDS THE CHI SQUARE FOR A TABLE",A3),      00006800
 F4(X8,A2,"TYPE IN ONE LINE OF TABLE IDENTIFICATION",3A1,A3),           00006900
     F1A(X8," TO RESTART PROGRAM TYPE HELP",A3),                        00007000
     F1B(X8," TO END PROGRAM TYPE STOP",A3),                            00007100
     F5(X8, A2,"TYPE IN TABLE BY ROWS",A3),                             00007200
     F6(X8, A2,"THE TABLE AS RECEIVED BY ROWS",A3),                     00007300
     FF(X8, A2, A3),                                                    00007400
     F7(X8, *F8.1, A3 ),                                                00007500
     F9(X8, A2,"ERROR TJ=",F8.1," TI=",F8.1, A3),                       00007600
    F10(X8,A2,"THE CHI SQUARE VALUE =", R12.6,A3),                      00007700
     F12(X8,A2, "DEGREES OF FREEDOM =",I4, 4A1,A3),                     00007800
     F13(X8,A2,"ERROR MAX NO. COL= 6 NO. ROWS=40",A3),                  00007900
     F1C(X8," ALL NUMBERS MUST BE FOLLOWED BY A COMMA",A3);             00008000
COMMENT THIS PROCEDURE FINDS THE CHI SQUARE FOR A TABLE                 00008100
     OF VALUES READ IN BY ROW WRITTEN BY DAVID MACURDY;                 00008200
     WR F1, FORM);                                                      00008300
WR F1A,CANC); WR F1B,CANC); WR F1C, CANC);                              00008400
OVER: NI:=40; NJ:=7;  TI:= TJ:= 0;                                      00008500
FORI FORJ A[I-1,J-1] := 0;                                              00008600
NUM:=1; L1: WR FTC,CRLF,LA); READUNIT; DATAOK(NUM);                     00008700
ALLOK; IF OK THEN GO TO L1; READ(D[*],/,NJ);                            00008800
L2: WR FTR,LA); READUNIT; DATAOK(NUM); ALLOK;                           00008900
IF OK THEN GO TO L2; READ(D[*],/,NI);                                   00009000
IF NI GTR 40 OR NJ GTR 6 THEN BEGIN                                     00009100
WR F13, FORM); GO TO OVER; END;                                         00009200
WR F5, FORM);                                                           00009300
BEGIN                                                                   00009400
LABEL  LR;                                                              00009500
INTEGER  DUMMY;                                                         00009600
 FORI BEGIN                                                             00009700
LR: READUNIT; DATAOK(NJ); ALLOK; IF OK THEN GO TO LR;                   00009800
READ( D[*],/,FORJ A[I,J] );                                             00009900
 END OF READ IN OF TABLE;                                               00010000
 END OF BLOCK TO READ IN DATA;                                          00010100
 WR F4, CRLF,LF,LF,LF,CANC); READUNIT; WR FF, FORM);                    00010200
FORI WR F7, NJ, FORJ A[I,J], CANC );                                    00010300
COMMENT THIS SECTION FINDS CHI SQUARE;                                  00010400
     FORI FORJ A[I,0] := A[I,0] + A[I,J] ;                              00010500
FORJ FORI A[0,J] := A[0,J] + A[I,J];                                    00010600
FORJ TJ := TJ + A[0,J];                                                 00010700
FORI TI := TI + A[I,0];                                                 00010800
IF TJ NEQ TI THEN WR F9,CRLF,TJ,TI,CANC);                               00010900
CHI := 0.0;                                                             00011000
FORI FORJ                                                               00011100
CHI := CHI + ((( A[I,J] - FE)*2)/FE);                                   00011200
 WR F10, CRLF, CHI, CANC);                                              00011300
NUM := (NI-1) | (NJ-1) ;                                                00011400
 WR F12, CRLF, NUM, LF,LF,LF,LF, CANC );                                00011500
GO TO OVER;                                                             00011600
END OF CHISQUARE;                                                       00011700
CRLF:=3068; CANC:=196383; CR:=47; LF:=60; LA:=31;                       00011800
               UNIT~STATUS(D[*]);                                       00011900
               UNIT.[9:9]~D[0].[9:9];                                   00012000
     CHISQUARE;                                                         00012100
EXT: WRITE(REMOTE(UNIT),FT1,CRLF,LF,LF,LF,LF,CANC);                     00012200
 WRITE(REMOTE(UNIT),FT2,FORM);                                          00012300
    END.                                                                00012400
