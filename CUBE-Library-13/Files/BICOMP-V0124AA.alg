BICOMP      000124AA                                                    00000100
                                                                        00000200
************************************************************************00000300
          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00000400
     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00000500
************************************************************************00000600
                                                                        00000700
   A BINARY CARD READ AND COMPARE PROGRAM. THE DATA FILE IS A CARD-     00000800
   READER. THE FIRST CARD TO BE READ MUST BE A BLANK CARD. THE          00000900
   CARD WITH WHICH ALL OTHERS WILL BE COMPARED MUST BE THE SECOND       00001000
   CARD IN THE CARD FILE. WHEN AN INCORRECT COMPARISON IS MADE          00001100
   A MESSAGE WILL BE OUTPUTTED ON THE SPO, AND AN INPUT OF ANY          00001200
   INTEGER NUMBER WILL BE EXPECTED. BEFORE INPUTTING THE NUMBER,        00001300
   THE OFFENDING CARD SHOULD BE REMOVED FROM THE CARD-READER. IT        00001400
   WILL BE THE LAST CARD READ IN THE STACKER. DUE TO THE FACT THAT      00001500
   THE PROGRAM WILL READ THE END CARD IN BINARY ALSO, THERE MUST        00001600
   BE TWO QUESTION MARK END CARDS FOLLOWING THE DATA DECK. WHEN         00001700
   THE JOB COMPLETES, IT WILL OUTPUT A MESSAGE ON THE SPO INDICATING    00001800
   HOW MANY CORRECT AND INCORRECT CARDS WERE PROCESSED.                 00001900
   15 JANUARY 1968 - A. RABENAU                                         00002000
                                                                        00002100
BEGIN                                                                   00002200
 FILE CARDZ(1,20);                                                      00002300
 FILE SPO 11(1,10);                                                     00002400
 LABEL AGIN,CEOF;                                                       00002500
 FORMAT F2("CORRECT = ",I5," INCORRECT = ",I5);                         00002600
 ARRAY A,B[0:19];                                                       00002700
 INTEGER I,J,L;                                                         00002800
%                                                                       00002900
 STREAM PROCEDURE FIX(F);                                               00003000
 BEGIN                                                                  00003100
  DI := F; SKIP 21 DB; DS := SET;                                       00003200
 END STREAM PROCEDURE FIX;                                              00003300
%                                                                       00003400
 STREAM PROCEDURE MOVE(F,A);                                            00003500
 BEGIN                                                                  00003600
  SI:=F; DI:=A; DS:=20 WDS;                                             00003700
 END STREAM PROCEDURE MOVE;                                             00003800
%                                                                       00003900
 INTEGER STREAM PROCEDURE COMPARE(A,B);                                 00004000
 BEGIN                                                                  00004100
  LABEL L1;                                                             00004200
  LOCAL Q;                                                              00004300
  DI:=LOC Q; DS:=10 LIT"2200|+++||";                                    00004400
  SI:=A; DI:=B; TALLY:=0;                                               00004500
  8(20(IF SC NEQ DC THEN                                                00004600
  BEGIN                                                                 00004700
   SI:=LOC Q; DI:=B;                                                    00004800
   IF 10 SC EQL DC THEN                                                 00004900
   BEGIN                                                                00005000
    TALLY:=2; JUMP OUT 2 TO L1;                                         00005100
   END;                                                                 00005200
   TALLY:=1; JUMP OUT 2 TO L1;                                          00005300
  END; ));                                                              00005400
 L1: COMPARE:=TALLY;                                                    00005500
 END BOOLEAN STREAM PROCEDURE COMPARE;                                  00005600
%                                                                       00005700
 READ(CARDZ[NO]) [CEOF];                                                00005800
 FIX(CARDZ);                                                            00005900
 RELEASE(CARDZ);                                                        00006000
 MOVE(CARDZ(0),A);                                                      00006100
 AGIN: RELEASE(CARDZ);                                                  00006200
 MOVE(CARDZ(0),B);                                                      00006300
 IF(L:=COMPARE(A,B)) EQL 1 THEN                                         00006400
 BEGIN                                                                  00006500
  FORMAT F1("BAD CARD...REMOVE.~");                                     00006600
  FORMAT F3(I5);                                                        00006700
  INTEGER K;                                                            00006800
  WRITE(SPO,F1);                                                        00006900
  READ(SPO,F3,K);                                                       00007000
  I:=I+1;                                                               00007100
 END ELSE IF L EQL 2 THEN GO TO CEOF;                                   00007200
 J:=J+1;                                                                00007300
 GO TO AGIN;                                                            00007400
 CEOF: CLOSE(CARDZ,RELEASE);                                            00007500
 J:=J-1;                                                                00007600
 WRITE(SPO,F2,J,I);                                                     00007700
END OF PROGRAM.                                                         00007800
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
