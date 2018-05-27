BEGIN                                                                   00000300
     COMMENT   THIS PROGRAM ALLOWS A REMOTE USER TO SELECTIVELY         00000400
               PRINT A FILE THAT IS ON DISK. THE FILE IS DUMPED         00000500
               TO A REMOTE BY STARTING AT ANY RECORD DESIGNATED         00000600
               AND SPECIFING THE NO. OF RECS TO DUMP. A PORTION         00000700
               OF THE RECORD IS SPECIFIED BY STARTING CH POSITION       00000800
               AND NO. OF CHS PER LINE. THE FOLLOWING OPTION ARE INCL:  00000900
* QUIT--- GO TO END OF JOB                                              00001000
* DATA--- IF INVALID REMOTE CHS ARE ON FILE "*" IS INSERTED             00001100
* FILE--- ALLOWS NEW FILE TO BE SELECTED                                00001200
* SPEC--- SETS SPECIAL MODE ALLOWING RECORDS TO BE SPECIFIED BY         00001300
          A KEY (63 CHS OR LSS) INSTEAD OF REC NO.                      00001400
* NORM--- RESETS SPECIAL MODE                                           00001500
* SRCE--- RESETS "DATA" MODE                                            00001600
               NOTE MAX CHS PER LINE = 72 MAX REC SIZE 1023             00001700
   KEY---SETS FIELD SCAN TO INC OF THE NR FOLLOWING KEY                 00001705
           WHEN IN SPEC MODE I.E.--* KEY 1 SETS THE                     00001710
           SCANNER TO 1 STARTING AT SELECTED FIELD.                     00001715
          KEY 0 SETS SCANNER TO FIXED FIELD.                            00001717
   INC---SETS REC COUNTER TO THE INC NR FOLLOWING                       00001720
         WHEN EITHER IN SPEC OR NORM MODE.                              00001725
     WRITTEN BY  EDWARD WARSHAWER    APT BRANCH  FT. MON,NJ    ;        00001800
                                                                        00001805
%-------------------------------------------------------------          00001810
DEFINE                                                                  00001815
   DCINCHAR = 56#,    % BUFFER INPUT SZ                                 00001820
   DCOUTCHAR =56#,   % BUFFER OUTPUT SZ                                 00001825
  BUFINSIZE =8#,                                                        00001830
  BUFOUTSIZE = 8#;                                                      00001835
LABEL AXEPT,AGAIN,QUITIT ,NOIN   ;                                      00001900
INTEGER INC  ,KEYINC ;                                                  00002000
INTEGER RECSIZE,BLKSIZE,NRREC, NRCHRLINE,MAXLINES,LC,          R,J;     00002100
BOOLEAN DATAFILE;                                                       00002200
BOOLEAN CHKMESS,SPEC;                                                   00002300
REAL PREF,SUFF;                                                         00002400
FILE SEARCHER(1,30);                                                    00002500
ARRAY                                                                   00002600
      KEY[0:8]              ;                                           00002700
ARRAY B[0:6];                                                           00002800
INTEGER RECNR,WORDS,CHARS,         I,NRRECD;                            00002900
INTEGER STLINE,  STKEY,                                                 00003000
                NRKEY,                                                  00003100
                  STWD,  STCHR                      ;                   00003200
STREAM PROCEDURE MOVE(A,ANS,W,C,  S);                                   00003300
VALUE W,C,S;                                                            00003400
BEGIN                                                                   00003500
LABEL L1;                                                               00003502
     LOCAL TEMP;                                                        00003600
      SI := A;  SI := SI + S;                                           00003700
   DI := ANS; W(DS := 8 CHR);  DS := C CHR;                             00003701
  TEMP:= DI;  DI := ANS; SI:= TEMP; SI := SI-1;                         00003702
  C(IF SC NEQ " " THEN JUMP OUT 1 TO L1                                 00003704
      ELSE SI := SI -1);                                                00003706
     W(8(IF SC NEQ " " THEN JUMP OUT 2 TO L1                            00003708
           ELSE SI := SI -1));                                          00003710
L1:SI := SI + 1;                                                        00003712
    TEMP:= SI;  DI := TEMP ;                                            00003714
                                             DS := 3 LIT "{!~";         00003800
END;                                                                    00003900
STREAM PROCEDURE EDITDATA(A,ANS,W,C,  S);                               00004000
VALUE W,C,S;                                                            00004100
BEGIN                                                                   00004200
LABEL L1;                                                               00004202
     LABEL L,L2;                                                        00004300
     LOCAL TEMP;                                                        00004400
      SI := A;  SI := SI + S;                                           00004500
      DI := ANS;                                                        00004600
         C(IF SC= ALPHA THEN DS:= 1 CHR                                 00004700
         ELSE                                                           00004800
           BEGIN                                                        00004900
                  IF SC ="~" THEN DS:= 1 LIT "*"                        00005000
               ELSE IF SC = "!" THEN DS := 1 LIT "*"                    00005100
                 ELSE IF SC = "{" THEN DS := 1 LIT "*"                  00005200
                   ELSE IF SC = "}" THEN DS := 1 LIT "*"                00005300
                   ELSE IF SC = ">" THEN DS := 1 LIT "*"                00005400
                   ELSE IF SC = "<" THEN DS := 1 LIT "*"                00005500
                     ELSE BEGIN DS := 1 CHR; GO TO L; END;              00005600
             SI := SI + 1;                                              00005700
           END;                                                         00005800
                       L: );                                            00005900
       W(8(IF SC= ALPHA THEN DS:= 1 CHR                                 00006000
         ELSE                                                           00006100
           BEGIN                                                        00006200
                  IF SC ="~" THEN DS:= 1 LIT "*"                        00006300
               ELSE IF SC = "!" THEN DS := 1 LIT "*"                    00006400
                 ELSE IF SC = "{" THEN DS := 1 LIT "*"                  00006500
                   ELSE IF SC = "}" THEN DS := 1 LIT "*"                00006600
                   ELSE IF SC = ">" THEN DS := 1 LIT "*"                00006700
                   ELSE IF SC = "<" THEN DS := 1 LIT "*"                00006800
                     ELSE BEGIN DS := 1 CHR; GO TO L2; END;             00006900
             SI := SI + 1;                                              00007000
           END;                                                         00007100
                        L2: ); );                                       00007200
   TEMP := DI;DI:= ANS; SI := TEMP; SI := SI-1;                         00007202
   C(IF SC NEQ " " THEN JUMP OUT 1 TO L1                                00007204
        ELSE SI := SI -1);                                              00007206
   W(8(IF SC NEQ " " THEN JUMP OUT 2 TO L1                              00007208
              ELSE SI := SI -1 ));                                      00007210
      L1:  SI := SI +1;                                                 00007212
    TEMP:= SI;DI := TEMP;                                               00007214
     DS := 3 LIT "{!~";                                                 00007300
END EDITDATA;                                                           00007400
STREAM PROCEDURE  MOVENAME  (M,P,S,ANS);                                00007500
COMMENT ACCEPT FILE NAME AND MOVE TO OUTPUT ;                           00007600
BEGIN                                                                   00007700
     DI := P; DS := 8 LIT " ";                                          00007800
     SI := M; DI := P; DS := 1 LIT "0";                                 00007900
     5(IF SC = " " THEN SI := SI+1  ELSE JUMP OUT);                     00008000
     7(IF SC = ALPHA THEN DS := CHR ELSE JUMP OUT);                     00008100
     SI := SI+1; DI := S; DS := 8 LIT " ";                              00008200
     DI := S; DS := 1 LIT "0";                                          00008300
     7(IF SC = ALPHA THEN DS := CHR ELSE JUMP OUT);                     00008400
     SI := P; DI := ANS; DS := 4 LIT " ** ";                            00008500
     SI := SI + 1;  7(IF SC = ALPHA THEN DS := CHR ELSE JUMP OUT);      00008600
     SI := S; SI := SI+1; DS := 1 LIT "/";                              00008700
     7(IF SC = ALPHA THEN DS := CHR ELSE JUMP OUT);                     00008800
     DS := 7 LIT " **{!!~";                                             00008900
END MOVENAME;                                                           00009000
INTEGER STREAM PROCEDURE ACTION(M);                                     00009100
BEGIN  LABEL L1 ,L2 ,L3 ; LOCAL S, TEMP;                                00009200
     SI := M;  5(IF SC = " " THEN SI := SI+1 ELSE JUMP OUT);            00009300
     IF SC ! "*" THEN GO TO L1 ELSE SI:= SI+1;                          00009400
     5(IF SC = " " THEN SI:=SI+1 ELSE JUMP OUT);  S := SI;              00009500
     IF SC = "Q" THEN                                                   00009600
       BEGIN SI:= SI+1; IF SC = "U" THEN                                00009700
        BEGIN SI:= SI+1; IF SC = "I" THEN                               00009800
          BEGIN SI:= SI+1; IF SC = "T" THEN                             00009900
            BEGIN TALLY := 1; GO TO L1; END; END; END; END;             00010000
     SI := S;                                                           00010100
     IF SC = "D" THEN                                                   00010200
       BEGIN SI := SI+1; IF SC = "A" THEN                               00010300
          BEGIN SI := SI+1; IF SC = "T" THEN                            00010400
             BEGIN SI := SI+1; IF SC = "A" THEN                         00010500
               BEGIN TALLY := 2; GO TO L1; END; END; END; END;          00010600
     SI := S;                                                           00010700
     IF SC = "F" THEN                                                   00010800
       BEGIN                                                            00010900
           SI := SI+1; IF SC = "I" THEN                                 00011000
            BEGIN SI:= SI+1; IF SC = "L" THEN                           00011100
               BEGIN SI := SI+1; IF SC = "E" THEN                       00011200
                 BEGIN TALLY := 3; GO TO L1; END; END; END; END;        00011300
     SI := S;                                                           00011400
     IF SC = "S" THEN                                                   00011500
       BEGIN SI:=SI+1; IF SC = "P" THEN                                 00011600
          BEGIN SI:= SI+1; IF SC = "E" THEN                             00011700
            BEGIN SI := SI+1; IF SC = "C" THEN                          00011800
               BEGIN TALLY := 4; GO TO L1; END; END; END; END;          00011900
     SI := S;                                                           00012000
     IF SC = "N" THEN                                                   00012100
       BEGIN SI :=SI+1; IF SC = "O" THEN                                00012200
          BEGIN SI:= SI+1; IF SC = "R" THEN                             00012300
            BEGIN SI := SI+1; IF SC = "M" THEN                          00012400
              BEGIN TALLY := 5; GO TO L1; END; END; END; END;           00012500
     SI := S;                 % START SRCE = 6                          00012600
     IF SC = "S" THEN                                                   00012700
       BEGIN SI := SI+1; IF SC = "R" THEN                               00012800
          BEGIN SI := SI+1; IF SC = "C" THEN                            00012900
            BEGIN SI := SI+1; IF SC = "E" THEN                          00013000
               BEGIN TALLY := 6; GO TO L1; END; END; END; END;          00013100
SI := S;                                                                00013102
IF SC = "I" THEN                                                        00013104
  BEGIN                                                                 00013106
     SI:=SI+1; IF SC ="N" THEN                                          00013108
    BEGIN SI := SI+1; IF SC = "C" THEN BEGIN                            00013110
      SI:=SI+1;  5(IF SC =" " THEN SI:=SI+1 ELSE JUMP OUT);             00013112
      TALLY:= 1;                                                        00013114
      IF SC = ALPHA THEN SI:=SI+1 ELSE GO TO L2;                        00013116
      4(IF SC = ALPHA THEN TALLY:=TALLY+1                               00013118
    ELSE JUMP OUT; SI:= SI +1);                                         00013120
L2:      TEMP := TALLY;  SI := SI - TEMP;                               00013122
     DI := M               ;                                            00013124
    DS:= TEMP OCT;   TALLY := 7;                                        00013126
       GO TO L1;                                                        00013128
      END;                                                              00013130
  END; END;                                                             00013132
SI := S;                                                                00013140
IF SC = "K" THEN                                                        00013142
  BEGIN SI := SI +1; IF SC = "E" THEN                                   00013144
    BEGIN SI := SI +1; IF SC = "Y" THEN                                 00013146
      BEGIN                                                             00013148
        SI := SI + 1;                                                   00013150
        5(IF SC = " " THEN SI := SI +1 ELSE JUMP OUT);                  00013152
        TALLY := 1  ;                                                   00013153
        IF SC= ALPHA THEN SI := SI + 1 ELSE GO TO L3;                   00013154
        4(IF SC = ALPHA THEN TALLY := TALLY+1                           00013156
    ELSE JUMP OUT; SI := SI +1);                                        00013158
L3:     TEMP := TALLY;SI := SI -TEMP;                                   00013160
        DI := M; DS := TEMP OCT; TALLY := 8; GO TO L1;                  00013162
END; END; END;                                                          00013164
L1:                                                                     00013200
     ACTION := TALLY;                                                   00013300
END ACTION;                                                             00013400
STREAM PROCEDURE SLOSHER(M);                                            00013500
BEGIN                                                                   00013600
     DI:= M;  10(DS := 8 LIT ",");                                      00013700
END SLOSHER;                                                            00013800
BOOLEAN STREAM PROCEDURE COMP(A, CH, NR, K);                            00014600
VALUE CH,NR;                                                            00014700
BEGIN                                                                   00014800
     SI := A;  DI := K;  SI := SI + CH;                                 00014900
     IF NR SC = DC THEN TALLY := 1;                                     00015000
     COMP := TALLY;                                                     00015100
END COMP;                                                               00015200
STREAM PROCEDURE FILLKEY(K, M, NR)   ;                                  00015300
VALUE NR ;                                                              00015400
BEGIN                                                                   00015500
LABEL L;                                                                00015600
     SI := M;                                                           00015700
     63(IF SC ! "~" THEN SI := SI + 1                                   00015800
          ELSE JUMP OUT TO L );                                         00015900
     17(IF SC ! "~" THEN SI := SI + 1                                   00016000
          ELSE JUMP OUT ) ;                                             00016100
L:   SI := SI - NR;  DI := K;                                           00016200
     DS := NR CHR;  DS := 4 LIT "{!!~";                                 00016300
END FILLKEY;                                                            00016400
FORMAT FM1(                               "** ENTER FILE **{!!~"),      00016500
       FM2(                 "** FILE NOT ON DISK.RE-ENTER. **{!!~"),    00016600
       FM3("RECSIZE = ",I6," BLKSIZE = ",I6," NR OF RECS =",I6,"{!!~"), 00016700
       FM4 ("  KEY NOT FOUND.{!!~"),                                    00016800
     FM5("{!** ENTER PARAMETERS **{!!~"),                               00016900
FM6("--PROCESS TIME = ",I3," MIN",I3," SEC","--I/O = ",I3," MIN",I3,    00016902
             " SEC{!~"),                                                00016904
                                                                        00017000
      FM8("** INVALID USER OF FILE. PLEASE RETYPE **{!!~"),             00017100
       FM10("KEY FOUND. RECORD NR IS ",I6,"{!!~"),                      00017200
    FM11("~"),                                                          00017300
       FM7("** PARAMETER ERROR. PLEASE CHECK AND RETYPE **{!!~");       00017400
LIST L1(RECSIZE,BLKSIZE,NRRECD),                                        00017500
     L2(RECNR,NRREC,NRCHRLINE,MAXLINES,STLINE),                         00017600
L4(I DIV 3600,(I MOD 3600)DIV 60,J DIV 3600,(J MOD 3600) DIV 60),       00017650
     L3(RECNR,                                                          00017699
        NRREC,NRCHRLINE,MAXLINES,STLINE,STKEY,NRKEY); % SPEC            00017700
SWITCH LIST SWL := L2, L3 ;                                             00017800
SWITCH FORMAT SWF :=                                                    00017900
     ("TYPE IN RECNR,NR RECS,NR CHR PER LN,MAX LNS PER REC,",           00018000
          "START CHR LINE{!!~"),           %  SW = 0                    00018100
     ("REC#,"      ,                                                    00018199
            "#RECS,#CH-LN,MAX-LNS-REC,ST CHR LN,",                      00018200
                  "ST KEY,NR CHR KEY,KEY{!!~"); % SW = 1                00018300
DEFINE INNG =    NOT INPUT THEN GO TO QUITIT#,                          00018400
  RECSZCH =STWD#,STKY2=STCHR#,RECNR2=J#,                                00018420
WHATNOW =                                                               00018450
CHKMESS := TRUE;                                                        00018452
WHILE CHKMESS DO                                                        00018454
BEGIN IF INNG; BREAK := FALSE;                                          00018456
CASE ACTION(MESSAGE) OF BEGIN                                           00018458
 CHKMESS := FALSE; GO TO QUITIT; DATAFILE := TRUE;                      00018460
GO TO AGAIN;                                                            00018462
BEGIN SPEC := TRUE;GO TO REPEATREC;END;                                 00018464
BEGIN SPEC := FALSE; GO TO REPEATREC;END;                               00018466
DATAFILE := FALSE;                                                      00018468
     INC := MESSAGE[0];                                                 00018470
  KEYINC := MESSAGE[0];                                                 00018472
  END; END#,                                                            00018474
       WHATNEXT =                                                       00018500
          CHKMESS := TRUE;                                              00018600
          WHILE CHKMESS DO                                              00018700
BEGIN                                                                   00018800
IF INNG;                                                                00018900
BREAK := FALSE;                                                         00018950
CASE ACTION(MESSAGE) OF                                                 00019000
            BEGIN                                                       00019100
               CHKMESS := FALSE;                                        00019200
     GO TO QUITIT;                                                      00019300
           DATAFILE := TRUE;                                            00019400
               BEGIN CHKMESS := FALSE; GO TO AXEPT; END;                00019500
               SPEC := TRUE;                                            00019600
               SPEC := FALSE;                                           00019700
                DATAFILE := FALSE;      % SRCE  = 6                     00019800
        INC := MESSAGE[0];                                              00019810
  KEYINC := MESSAGE[0];                                                 00019812
     END;                                                               00019900
                    END#,                                               00020000
       OUTNG =    NOT OUTPUT THEN GO TO QUITIT#;                        00020100
DEFINE                                                                  00020200
                     NBUFIN = 1 #,                                      00020300
    NBUFOUT = 1 #,                                                      00020400
    MSGSIZE  =  80#,                                                    00020500
    ANSSIZE  = 120#,                                                    00020600
    WAITTIME = 180#;                                                    00020700
ARRAY                                                                   00020800
    MESSAGE[0:(MSGSIZE-1) DIV 8], ANSWER[0:(ANSSIZE-1) DIV 8];          00020900
ALPHA FILE IN  DCIN  14 ( NBUFIN, BUFINSIZE);                           00021000
ALPHA FILE OUT DCOUT 14 (NBUFOUT,BUFOUTSIZE);                           00021100
REAL USER;                                                              00021200
BOOLEAN INUSE, BREAK  ;                                                 00021300
                                                                        00021400
                                                                        00021500
BOOLEAN PROCEDURE OUTPUT;                                               00021600
BEGIN                                                                   00021700
    LABEL LOOP, NOGO, BROKE, BADNEWS, EXIT   ;                          00021800
    INTEGER  CNT, Q, R, TIMEX;                                          00021900
    BOOLEAN GPMK;                                                       00022000
    REAL S;                                                             00022100
    ARRAY TANK[0:BUFOUTSIZE-1];                                         00022200
    STREAM PROCEDURE STOPPER(A);                                        00022300
        BEGIN DI~A; DI~DI+7; DS~LIT "~"  END STOPPER;                   00022400
    BOOLEAN STREAM PROCEDURE MOVE (A,B,C);  VALUE C;                    00022500
        BEGIN SI:= A;  SI:=SI+C;  TALLY:=0;                             00022600
                       DI := B;                                         00022700
            DCOUTCHAR(IF SC="~" THEN TALLY~1; DS~CHR);                  00022800
            MOVE~TALLY                                                  00022900
        END MOVE;                                                       00023000
    IF NOT INUSE THEN GO TO EXIT;                                       00023100
    STOPPER(ANSWER[(ANSSIZE-1) DIV 8]);                                 00023200
OUTPUT~TRUE;                                                            00023300
TIMEX ~ TIME(1);                                                        00023400
LOOP:   Q~CNT DIV 8;  R~CNT MOD 8;                                      00023500
    GPMK ~ MOVE(ANSWER[Q],TANK[1],R);                                   00023600
    WRITE(DCOUT(USER,5),BUFOUTSIZE,TANK[*])[NOGO:BROKE];                00023700
    IF GPMK THEN GO TO EXIT;                                            00023800
    CNT ~ CNT + DCOUTCHAR;                                              00023900
    GO TO LOOP;                                                         00024000
BROKE:   BREAK ~ TRUE;  GO TO EXIT;                                     00024100
NOGO:  S ~ STATUS(USER,0);                                              00024200
    IF S.[30:1]=1 THEN GO TO BADNEWS;                                   00024300
     IF TIME(1) - TIMEX GTR WAITTIME | 60 THEN GO TO BADNEWS;           00024400
    IF S.[23:2]!0 OR S.[28:1]=1 THEN WHEN(5);                           00024500
%   WAIT IN PREVIOUS STATEMENT IF THE WRITE DID NOT TIME OUT            00024600
    GO TO LOOP;                                                         00024700
BADNEWS:   CLOSE(DCIN,RELEASE);  CLOSE(DCOUT,RELEASE);                  00024800
    OUTPUT ~ INUSE ~ FALSE; USER ~ 0;                                   00024900
EXIT:   END OUTPUT;                                                     00025000
                                                                        00025100
                                                                        00025200
BOOLEAN PROCEDURE INPUT;                                                00025300
BEGIN                                                                   00025400
    SWITCH FORMAT FLAGS ~ ("!{~"),                                      00025500
        ("!{  MESSAGE WAS TOO LONG...PLEASE REENTER!{~"),               00025600
        ("!{  MESSAGE WAS PARTLY LOST...PLEASE REENTER!{~");            00025700
    ARRAY SINK[0:BUFINSIZE-1];                                          00025800
    INTEGER CNT, TIMEX, FLAG, Q, R, N;                                  00025900
    LABEL LOOP, NOGO, ABNORM, CHECKIT, EXIT, BADNEWS;                   00026000
    REAL S;                                                             00026100
    INTEGER STREAM PROCEDURE SIZE(A);                                   00026200
        BEGIN  SI~A;  TALLY~1;                                          00026300
            DCINCHAR(IF SC="~" THEN JUMP OUT; SI~SI+1; TALLY~TALLY+1);  00026400
            SIZE ~ TALLY                                                00026500
        END SIZE;                                                       00026600
    STREAM PROCEDURE MOVE(A,B,C,D);  VALUE C,D;                         00026700
        BEGIN SI~A; DI~B; DI~DI+D; DS~C CHR  END MOVE;                  00026800
    IF NOT INUSE THEN                                                   00026900
        BEGIN  READ(DCIN(0,WAITTIME),BUFINSIZE,SINK[*])[EXIT];          00027000
            USER ~ SINK[0];  READ SEEK (DCIN(USER));                    00027100
        END LOOKING FOR A USER;                                         00027200
    INPUT ~ TRUE;                                                       00027300
     TIMEX := TIME(1);                                                  00027400
LOOP:   Q~ CNT DIV 8;  R ~ CNT MOD 8;                                   00027500
    IF INUSE                                                            00027600
            THEN READ(DCIN(USER,5),BUFINSIZE,SINK[*])[NOGO:ABNORM]      00027700
            ELSE  INUSE~ TRUE;                                          00027800
    TIMEX ~ TIME(1);                                                    00027900
    IF SINK[0].[25:1]=0   % GM                                          00028000
        THEN  N ~ SIZE(SINK[1])                                         00028100
     ELSE N := DCINCHAR;                                                00028200
    IF CNT~CNT+N > MSGSIZE                                              00028300
        THEN  FLAG ~ 1                                                  00028400
        ELSE  MOVE(SINK[1],MESSAGE[Q],N,R);                             00028500
    IF SINK[0].[25:1]=0                                                 00028600
        THEN  GO TO CHECKIT                                             00028700
        ELSE  GO TO LOOP;                                               00028800
ABNORM:   S ~ STATUS(USER,0);                                           00028900
    IF S.[23:2]=3 OR S.[30:1]=1 THEN GO TO BADNEWS;                     00029000
    READ(DCIN(USER,0),BUFINSIZE,SINK[*])[NOGO:NOGO];                    00029100
NOGO:   S ~ STATUS(USER,0);                                             00029200
    IF   S.[30:1]=1 THEN GO TO BADNEWS;                                 00029300
    IF S.[28:1] = 1 THEN                                                00029400
            BEGIN S~STATUS(USER,1); FLAG~2; GO TO CHECKIT END;          00029500
    IF   S.[27:1]=1 THEN                                                00029600
            BEGIN                                                       00029700
                WRITE(ANSWER[*],FLAGS[0]);                              00029800
                IF NOT OUTPUT THEN GO TO BADNEWS;                       00029900
        END;                                                            00030000
     IF TIME(1) - TIMEX LSS WAITTIME | 60 THEN GO TO LOOP;              00030100
BADNEWS:                                                                00030200
    INPUT := FALSE  ;                                                   00030300
    GO TO EXIT;                                                         00030400
CHECKIT:   WRITE(ANSWER[*],FLAGS[FLAG]);                                00030500
    IF NOT OUTPUT THEN GO TO BADNEWS;                                   00030600
    IF FLAG!0 THEN                                                      00030700
        IF INPUT THEN GO TO EXIT ELSE GO TO BADNEWS;                    00030800
EXIT:   END INPUT;                                                      00030900
  INC := 1;                                                             00031000
    DATAFILE := TRUE    ;                                               00031050
 I:= STATUS(B[*]);                                                      00031100
IF I LEQ 0 THEN GO TO NOIN ELSE                                         00031102
BEGIN INUSE:=TRUE;USER := B[0];END ;                                    00031104
AGAIN:                                                                  00031200
WRITE(ANSWER[*],FM1);         % CODE 1 "ENTER FILE"                     00031300
IF OUTNG;                                                               00031400
AXEPT:                                                                  00031500
     WHATNEXT;                                                          00031600
      MOVENAME(MESSAGE,PREF,SUFF,ANSWER);    % FILE ACCEPTED CASE =0    00031700
IF OUTNG;                                                               00031800
FILL SEARCHER WITH PREF,SUFF;                                           00031900
SEARCH(SEARCHER,B[*]);                                                  00032000
IF B[0] LSS 0 THEN                                                      00032100
     BEGIN                                                              00032200
          WRITE(ANSWER[*],FM2);   % FILE NOT FOUND                      00032300
          IF OUTNG;                                                     00032400
          GO TO AXEPT;                                                  00032500
     END                                                                00032600
  ELSE                                                                  00032700
IF B [0] = 0 THEN                                                       00032800
     BEGIN                                                              00032900
       WRITE(ANSWER[*],FM8);                                            00033000
       IF OUTNG;                                                        00033100
       GO TO AXEPT;                                                     00033200
     END;                                                               00033300
RECSIZE := B[3];                                                        00033400
BLKSIZE := B[4]; NRRECD := B[5]+1;                                      00033500
WRITE(ANSWER[*],FM3,L1);        % OUTPUT PARAMETERS                     00033600
IF OUTNG;                                                               00033625
BEGIN   % INNER BLK                                                     00033650
FILE IN DISKIN DISK SERIAL (1,RECSIZE,BLKSIZE);                         00033652
LABEL NEXTBLK,   SPEC1,    PAR, NOKEY;                                  00033654
LABEL QUITIT    ,   OK , NXTREC  ;                                      00033655
LABEL REED,  PARERR,   REPEATREC,    REPEAT,ASK;                        00033656
INTEGER STKYWD,  STKYCH;                                                00033658
ARRAY                                                                   00033660
     A[0:IF RECSIZE { 1023 THEN RECSIZE-1 ELSE 1022]  ;                 00033662
FILL DISKIN WITH PREF  ,SUFF;                                           00033664
                                                                        00033700
REPEATREC:                                                              00033800
WRITE(ANSWER[*],SWF[REAL(SPEC)]);                                       00033900
IF OUTNG;                                                               00034000
REPEAT:                                                                 00034100
SLOSHER(MESSAGE);             % MOVE COMMA                              00034200
WHATNOW;                                                                00034300
READ(MESSAGE[*],/,SWL[REAL(SPEC)]);                                     00034400
IF NRREC ! 0 THEN                                                       00034500
BEGIN                                                                   00034600
   IF   NRCHRLINE ! 0 THEN                                              00034700
BEGIN                                                                   00034800
      IF MAXLINES ! 0 THEN                                              00034900
BEGIN                                                                   00035000
         IF   NRCHRLINE { 72 THEN                                       00035100
BEGIN                                                                   00035200
           IF STLINE > 0 AND STLINE { (RECSIZE | 8) THEN                00035300
BEGIN                                                                   00035400
               LC := ((RECSIZE | 8) - (STLINE -1)) DIV NRCHRLINE;       00035500
               R := ((RECSIZE | 8) - (STLINE -1)) MOD NRCHRLINE;        00035600
               IF MAXLINES {        LC THEN                             00035700
                BEGIN                                                   00035800
                  LC := MAXLINES;                                       00035900
                  R := 0;                                               00036000
                END;                                                    00036100
     END                                                                00036200
          ELSE GO TO PARERR;                                            00036300
END                                                                     00036400
          ELSE GO TO  PARERR;                                           00036500
END                                                                     00036600
      ELSE GO TO  PARERR;                                               00036700
END                                                                     00036800
   ELSE GO TO  PARERR;                                                  00036900
END                                                                     00037000
ELSE GO TO  PARERR;                                                     00037100
       IF RECNR LSS 0 THEN RECNR:= ABS(RECNR)  ;                        00037110
      IF RECNR EQL 0 THEN RECNR := RECNR + 1;                           00037120
     IF SPEC THEN                                                       00037200
       BEGIN                                                            00037300
         IF STKEY > 0 AND ((STKEY + NRKEY) { ((RECSIZE | 8) +1)) THEN   00037400
           BEGIN                                                        00037500
                   IF NRKEY > 0 AND NRKEY { 63 THEN                     00037600
               BEGIN                                                    00037700
                   FILLKEY(KEY,MESSAGE,      NRKEY)      ;              00037800
              WRITE(ANSWER[*], 9,KEY[*]);                               00037900
IF OUTNG;                                                               00038000
                    GO TO SPEC1           ;                             00038100
               END ELSE GO TO PARERR;                                   00038200
           END ELSE GO TO PARERR;                                       00038300
       END;                                                             00038400
NEXTBLK:                                                                00039350
     FOR I := RECNR -1 STEP INC WHILE I LSS RECNR+((NRREC-1)|INC) DO    00039400
        BEGIN                                                           00039500
PAR:     READ(DISKIN[I],RECSIZE,A[*])[ASK:PAR];                         00039600
          WORDS := NRCHRLINE DIV 8 ;                                    00039650
        CHARS := NRCHRLINE MOD 8;                                       00039655
    STWD := (STLINE - 1) DIV 8 ;                                        00039700
    STCHR := (STLINE - 1) MOD 8;                                        00039800
           FOR J := 1 STEP 1 UNTIL LC   DO                              00039900
              BEGIN                                                     00040000
           IF DATAFILE THEN EDITDATA(A[STWD],ANSWER,WORDS,CHARS,STCHR)  00040100
  ELSE                                                                  00040200
           MOVE(A[STWD],ANSWER,WORDS,CHARS,  STCHR);                    00040300
                 IF OUTNG;                                              00040400
                 IF BREAK THEN                                          00040500
                    BEGIN BREAK := FALSE; GO TO ASK; END;               00040600
                STWD := ((STWD | 8) + STCHR + NRCHRLINE) DIV 8;         00040700
               STCHR := ((STWD | 8) + STCHR + NRCHRLINE)MOD 8;          00040800
              END;                                                      00040900
           IF R ! 0 THEN                                                00041000
            BEGIN                                                       00041100
          WORDS := R DIV 8;                                             00041200
          CHARS := R MOD 8;                                             00041300
           IF DATAFILE THEN EDITDATA(A[STWD],ANSWER,WORDS,CHARS,STCHR)  00041400
  ELSE                                                                  00041500
           MOVE(A[STWD],ANSWER,WORDS,CHARS,  STCHR);                    00041600
              IF OUTNG;                                                 00041700
              IF BREAK THEN                                             00041800
                BEGIN BREAK := FALSE; GO TO ASK;  END;                  00041900
        END;                                                            00042000
     END;                                                               00042100
                                                                        00042200
ASK:                                                                    00042300
WRITE(ANSWER[*],FM5);      %   DO U WANT A REC;                         00042400
IF OUTNG;                                                               00042500
GO TO REPEAT;                                                           00042550
PARERR:                                                                 00043300
WRITE(ANSWER[*],FM7);                                                   00043400
IF OUTNG;                                                               00043500
GO TO REPEAT;                                                           00043600
SPEC1:                                                                  00043700
RECNR2 := RECNR-1;                                                      00043705
  IF KEYINC = 0 THEN KEYINC := RECSIZE | 8;                             00043707
  RECSZCH := RECSIZE |8;                                                00043712
REED:   FOR I := 1 STEP 1 UNTIL 10 DO                                   00043714
   BEGIN                                                                00043716
     READ(DISKIN[RECNR2] ,RECSIZE,A[*])[NOKEY:REED];                    00043718
     STKY2 := STKEY-1;                                                  00043720
     WHILE TRUE DO                                                      00043722
      BEGIN                                                             00043724
        STKYWD := STKY2 DIV 8;                                          00043726
        STKYCH := STKY2 MOD 8;                                          00043728
        IF COMP(A[STKYWD],STKYCH,NRKEY,KEY) THEN GO TO OK;              00043730
        STKY2 := STKY2 + KEYINC;                                        00043732
        IF (STKY2 + NRKEY) GTR RECSZCH THEN GO TO NXTREC;               00043734
     END;                                                               00043736
NXTREC:   RECNR2:= RECNR2 + INC;                                        00043738
END;                                                                    00043740
     READ(DCIN(USER,0), BUFINSIZE,MESSAGE[*])[REED];                    00045400
            GO TO ASK;                                                  00045500
OK:                                                                     00045600
WRITE(ANSWER[*],FM10, RECNR := RECNR2 +1);                              00045700
IF OUTNG;                                                               00045800
                         GO TO NEXTBLK;                     %  4        00045900
NOKEY:                                                                  00046000
WRITE(ANSWER[*],FM4);                                                   00046100
IF OUTNG;                                                               00046200
GO TO ASK;                                                              00046300
QUITIT:                                                                 00046350
END;                    % END INNER BLOCK                               00046400
QUITIT:                                                                 00046500
I := TIME(2);    J:= TIME(3);                                           00046550
WRITE(ANSWER[*],FM6,L4);                                                00046552
IF OUTNG;                                                               00046554
NOIN:                                                                   00046556
END.                                                                    00046600
 I:= STATUS(B[*]); IF  I LEQ 0 THEN GO TO NOIN                          00311100
ELSE BEGIN                                                              00311102
   USER.[9:9] := B[0].[9:9];                                            00311104
 INUSE := TRUE;                                                         00311106
  END;                                                                  00311108
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
