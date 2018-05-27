              REMOTE / CARD                                             00000100
  WRITTEN BY RON BRODY;  BURROUGHS CORP.; PAOLI, PA.                    00000200
BEGIN                                                                   00000300
 DEFINE VERSION = 75# ; % AUGUST 7, 1968.                               00000400
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00000500
 ALPHA FILE IN TWXINPUT 14 (3, 8) ;                                     00000600
 DEFINE TWXIN = TWXINPUT (0, 300)# ;                                    00000700
 ALPHA FILE OUT TWXOUTPUT 14 (4, 5) ;                                   00000800
 DEFINE TWXOUT = TWXOUTPUT (STATION)# ;                                 00000900
 ALPHA ARRAY RECORD [0 : 9] ;                                           00001100
 INTEGER N,                                                             00001300
   AT,                                                                  00001400
   D ;                                                                  00001500
 REAL ARRAY LINKLIST [0 : 31, 0 : 255] ;                                00001600
 DEFINE LEFTSIDE = [35 : 5]#,                                           00001700
   RIGHTSIDE = [40 : 8]# ;                                              00001800
 DEFINE LINKLISTSUBD = LINKLIST [D.LEFTSIDE, D.RIGHTSIDE]#,             00001900
   LINKLISTSUBAT = LINKLIST [AT.LEFTSIDE, AT.RIGHTSIDE]# ;              00002000
 DEFINE S = [1 : 21]#,                                                  00002100
   SF = 1 : 27 : 21#,                                                   00002200
   F = [22 : 13]#,                                                      00002300
   FF = 22 : 35 : 13#,                                                  00002400
   T = [35 : 13]#,                                                      00002500
   TF = 35 : 35 : 13# ;                                                 00002600
 DEFINE INFINITY = 2097151# ;   % MAX SEQ NO. = 2*21 - 1.               00002700
 DEFINE MAXFILELENGTH = 8191# ;   % = 2*13 - 1.                         00002800
 INTEGER M,                                                             00002900
   INC ;                                                                00003000
 INTEGER I ;                                                            00003100
 BOOLEAN BOOL ;                                                         00003200
 DEFINE B = BOOL.[47 : 1]#,                                             00003300
   INLINETOG = BOOL.[46 : 1]#,                                          00003400
    EXTRALFCR = BOOL . [45:1]#,                                         00003450
   PRINTING = BOOL.[44 : 1]#,                                           00003500
   TRANSLATING = BOOL.[43 : 1]#,                                        00003550
   ALREADYSEQ = BOOL.[42 : 1]#,                                         00003560
   QUICK = BOOL.[41 : 1]#,                                              00003570
   SCANTOG = BOOL.[40 : 1]#,                                            00003580
   NUM0 = BOOL.[39 : 1]#,                                               00003581
   NUM1 = BOOL.[38 : 1]#,                                               00003582
   NUM2 = BOOL.[37 : 1]#,                                               00003583
   NUM3 = BOOL.[36 : 1]#,                                               00003584
   NUM4 = BOOL.[35 : 1]#,                                               00003585
   EMPTY0 = BOOL.[34 : 1]#,                                             00003586
   EMPTY1 = BOOL.[33 : 1]#,                                             00003587
   EMPTY2 = BOOL.[32 : 1]#,                                             00003588
   EMPTY3 = BOOL.[31 : 1]#,                                             00003589
   EMPTY4 = BOOL.[30 : 1]#,                                             00003590
   FILESECURITY = BOOL.[29 : 1]#,                                       00003591
   CHANGETOG = BOOL.[28 : 1]#,                                          00003592
   EDITTOG = BOOL.[27 : 1]#,                                            00003593
   POSTING = BOOL.[26 : 1]#,                                            00003594
   PUNCHING = BOOL.[25 : 1]#,                                           00003595
   ZIPPING = BOOL.[24:1]#,                                              00003596
   MOREINPUT = BOOL.[23:1]#,                                            00003597
   NOSTAR = BOOL.[22:1]#,                                               00003598
   INORDER = BOOL.[1 : 1]# ;                                            00003600
 DEFINE CONTROLTOP = 34# ;                                              00003650
 ARRAY CONTROLS [0 : CONTROLTOP] ;                                      00003700
 DEFINE                                                                 00003710
   STRINGJ = CONTROLS [34]#,                                            00003725
   VN = CONTROLS [33]#,                                                 00003730
   PREWHERE = CONTROLS [32]#,                                           00003740
   STRING = CONTROLS [17]#,   % THRU CONTROLS [31].                     00003750
   PAGELENGTH = CONTROLS [14]#,                                         00003760
   LINECOUNT = CONTROLS [16]#,                                          00003770
   SAVEFACTOR = CONTROLS [15]#,                                         00003780
   TIME1 = CONTROLS [13]#,                                              00003800
   STRINGI = CONTROLS [12]#,                                            00003810
   STATION = CONTROLS [11]#,                                            00003900
   PREFIX = CONTROLS [10]#,                                             00004000
   SUFFIX = CONTROLS [9]#,                                              00004100
   TABAMOUNT = CONTROLS [8]#,                                           00004200
   FILETYPE = CONTROLS [7]#,                                            00004300
   RSWDL = CONTROLS [6]#,                                               00004400
   USERCODE = CONTROLS [5]# ;                                           00004500
 ARRAY IMAGE [0 : 9] ;                                                  00004600
 DEFINE RSWDM = 27# ;                                                   00004700
 DEFINE READONLYLIMIT = 6# ;                                            00004750
 DEFINE SETRSWDL =                                                      00004800
  BEGIN                                                                 00004900
    RSWDL := 13 ;                                                       00005000
  END # ;                                                               00005200
 ARRAY RSWD [0 : RSWDM] ;                                               00005300
 DEFINE                                                                 00005310
                                                                        00005320
   RWTEACH = RSWD [RSWDM - 3]#,                                         00005330
   RWTEACHINDEX = RSWDM - 3# ;                                          00005335
 LABEL NEXT,                                                            00005400
   TIMING,                                                              00005500
   STOP ;                                                               00005600
                                                                        00005700
                                                                        00005800
                                                                        00005820
                                                                        00005830
                                                                        00005840
                                                                        00005850
                                                                        00005860
                                                                        00005870
                                                                        00005880
                                                                        00005900
                                                                        00005910
                                                                        00005950
                                                                        00005960
                                                                        00006000
                                                                        00006100
                                                                        00006200
                                                                        00006300
                                                                        00006400
                                                                        00006500
                                                                        00006600
                                                                        00006700
                                                                        00006800
                                                                        00006900
                                                                        00007000
                                                                        00007100
                                                                        00007200
                                                                        00007250
                                                                        00007300
                                                                        00007400
                                                                        00007500
                                                                        00007600
                                                                        00007610
                                                                        00007620
                                                                        00007630
                                                                        00007700
                                                                        00007800
                                                                        00007880
                                                                        00007900
                                                                        00008000
                                                                        00008010
                                                                        00008020
                                                                        00008030
                                                                        00008040
                                                                        00008050
                                                                        00008100
                                                                        00008200
                                                                        00008300
                                                                        00008350
                                                                        00008360
                                                                        00008400
                                                                        00008500
                                                                        00008600
                                                                        00008800
                                                                        00008900
                                                                        00008950
                                                                        00009000
 SAVE FILE SCRATCH DISK SERIAL [20 : 90] (1, 10,  30, SAVE 7) ;         00009100
 FILE DISC DISK SERIAL (2, 10, 30) ;                                    00009200
 FILE FEEDBACK DISK SERIAL (1, 15, 30) ;                                00009300
 FILE LIBRARY DISK SERIAL (2, 10, 30) ;                                 00009400
 FILE RECOVER DISK SERIAL (2, 256) ;                                    00009450
 ARRAY ZIPPY [0 : 19] ;                                                 00009500
 FORMAT                                                                 00009600
   ZIPPER ("CC COMPILE ", A1, A6, "/", A1, A6, " WITH ", A1, A6,        00009700
           " LIBRARY; ALGOL FILE CARD=", A1, A6,                        00009800
           "/", A1, A6, " SERIAL; ALGOL FILE LINE=",                    00009900
           "LINE/", A1, A6, " SERIAL; END."),                           00010000
   X96I8 (X96, I8),                                                     00010400
   X116I4 (X116, I4),                                                   00010410
   SEGMENT (X8, " SEGMENT = ", I4, "; ~"),                              00010420
   RELADDR (X8, " REL. ADDR. = ", I4, ".~"),                            00010430
   X108I4 (X108, I4),                                                   00010440
   X96I4 (X96, I4),                                                     00010450
   X101I4 (X101, I4),                                                   00010460
   X84I4 (X84, I4),                                                     00010470
   X91I4 (X91, I4),                                                     00010480
   X92A4 (X92, A4),                                                     00010490
   A1 (A1),                                                             00010500
   X5I6 (X5,I6),                                                        00010600
   I6 (I6),                                                             00010700
   X72I8 (X72, I8),                                                     00010800
   SEQC (X8, "{!", I6, "~"),                                            00010810
   SEQA (X8, "{!", I8, ":~"),                                           00010820
   SEQCO (X8, "{!", A6, "~"),                                           00010830
   SEQAO (X8, "{!", O, ":~"),                                           00010840
   TABIT (X8, X *, "~"),                                                00010900
   EOJ (X8, "{!", I3, " MINS", I3, " SECS ", A6, "~"),                  00011000
   WAIT (X8, "WAIT...~"),                                               00011100
   SEQERR (X8, "SEQ ERR -", I8, " WAIT...~"),                           00011200
   XON (X8, ">~"),                                                      00011300
   TEACH1 (X8, "{!THE VALID REQUESTS ARE:~"),                           00011400
   TEACH2 (X8, "* ", A1, A6, "   ~"),                                   00011500
   TEACH3 (X8, "FOR SYNTAX OF A VERB~"),                                00011510
   TEACH4 (X8, " (E.G. TAB), INPUT: * TEACH~"),                         00011520
   TEACH5 (X8, " VERB. (E.G. * TEACH TAB)~"),                           00011530
   ALLNINES (X8, "{!{99999999:~"),                                      00011550
   NEWPAGE (X8, "{!!PAGE ", I3, ".{!!---{!~"),                          00011560
   DATE (X52," LISTED AT", I3, ":", I2, " ON ", A5, " BY", X40),        00011570
   LFCR (X8, "{!~") ;                                                   00011600
 LIST LISTN (N),                                                        00011700
   TEACHOUT (RSWD [I] . [6:6], RSWD [I]),                               00011750
   TIMEOUT (I DIV 3600, I DIV 60 MOD 60, M) ;                           00011800
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00011900
 STREAM PROCEDURE OCTDEC (N, I9) ;                                      00012000
 VALUE N ;                                                              00012050
  BEGIN                                                                 00012100
    SI := LOC N ;                                                       00012200
    DI := I9 ;                                                          00012300
    DS := 8 DEC ;                                                       00012400
  END OCTDEC ;                                                          00012500
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00012600
 STREAM PROCEDURE COBOLSEQ (N, D, T, S, M) ;                            00012700
 VALUE M ;                                                              00012800
  BEGIN                                                                 00012900
    SI := N ;                                                           00013000
    DI := T ;                                                           00013100
    DS := 6 DEC ;                                                       00013200
    SI := D ;                                                           00013300
    M (SI := SI + 6) ;              %IF ALREADY SEQ THEN SKIP.          00013400
    DS := 33 CHR ;                                                      00013500
    DS := 33 CHR ;                                                      00013600
    SI := S ;                                                           00013700
    SI := SI + 1 ;                                                      00013800
    DS := 1 LIT " " ;                                                   00013810
    DS := 7 CHR ;                                                       00013820
    SI := T ;                                                           00013900
    DI := D ;                                                           00014000
    DS := 10 WDS ;                                                      00014100
  END COBOLSEQ ;                                                        00014200
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00014300
 DEFINE SEQUENCE = IF FILETYPE LEQ 1 THEN                               00014500
    OCTDEC (IF N = INFINITY - 1 THEN 99999999 ELSE N, IMAGE [9])        00014600
  ELSE IF FILETYPE = 2 THEN                                             00014700
    COBOLSEQ (N, IMAGE, RECORD, SUFFIX, ALREADYSEQ)# ;                  00014800
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00014900
 STREAM PROCEDURE SHIFTLEFT (N, A, B) ;                                 00015000
 VALUE N ;                                                              00015100
  BEGIN                                                                 00015200
    SI := A ;                                                           00015300
    SI := SI + N ;                                                      00015400
    DI := B ;                                                           00015500
    2 (DS := 33 CHR) ;                                                  00015600
    N (DS := 1 LIT " ") ;                                               00015650
    SI := B ;                                                           00015700
    DI := A ;                                                           00015800
    2 (DS := 33 CHR) ;                                                  00015900
  END SHIFTLEFT ;                                                       00016000
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00016100
 STREAM PROCEDURE MOVE (S, D, SKPS, SKPD, N) ;                          00016200
 VALUE SKPS,                                                            00016300
   SKPD,                                                                00016400
   N ;                                                                  00016500
  BEGIN                                                                 00016600
    SI := S ;                                                           00016700
    DI := D ;                                                           00016800
    SI := SI + SKPS ;                                                   00016900
    DI := DI + SKPD ;                                                   00017000
    DS := N CHR ;                                                       00017100
  END MOVE ;                                                            00017200
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00017300
 STREAM PROCEDURE INSERT (N, S, D) ;                                    00017400
 VALUE N,                                                               00017500
   S ;                                                                  00017600
  BEGIN                                                                 00017700
    SI := LOC S ;                                                       00017800
    SI := SI + 7 ;                                                      00017900
    DI := D ;                                                           00018000
    DS := 1 CHR ;                                                       00018100
    N (SI := SI - 1 ;                                                   00018200
    DS := 1 CHR) ;                                                      00018300
  END INSERT ;                                                          00018305
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00018310
 STREAM PROCEDURE BLANKOUTSPECIALCHARACTERS (S,D,N) ; VALUE N ;         00018315
  BEGIN                                                                 00018320
 LABEL YES, NO, NEXT ;                                                  00018325
    DI := D ;                                                           00018330
    DS := 8 LIT "        " ;                                            00018335
    SI := D ;                                                           00018340
    DS := 8 WDS ;                                                       00018345
    SI := S ;                                                           00018350
    DI := D ;                                                           00018355
    2(36(IF SC = " " THEN                                               00018360
         BEGIN                                                          00018365
           N(SI := SI - 1 ;                                             00018370
             IF SC = " " THEN DI := DI - 1 ;                            00018375
             SI := SI + 1) ;                                            00018380
           GO TO NO ;                                                   00018385
         END ;                                                          00018390
         IF SC = ALPHA THEN GO TO NO ;                                  00018395
         IF SC GTR "R" THEN GO TO YES ;    % NEQ                        00018400
         IF SC GEQ "+" THEN GO TO NO ;                                  00018405
         IF SC GTR ":" THEN GO TO YES ;    % GTR, GEQ                   00018410
         IF SC GEQ "-" THEN GO TO NO ;                                  00018415
         IF SC GTR ";" THEN GO TO YES ;    % LEQ                        00018420
         IF SC GEQ "&" THEN GO TO NO ;                                  00018425
         IF SC GTR "(" THEN GO TO YES ;    % LSS, LEFT-ARROW            00018430
         GO TO NO ;                                                     00018435
 YES:                                                                   00018440
         DS := 1 LIT "$" ;                                              00018445
         SI := SI + 1 ;                                                 00018446
         GO TO NEXT ;                                                   00018450
 NO:                                                                    00018455
         DS := 1 CHR ;                                                  00018460
 NEXT:                                                                  00018465
         )) ;                                                           00018470
    SI := D ;                                                           00018475
    DI := S ;                                                           00018480
    DS := 9 WDS ;                                                       00018485
 END BLANKOUTSPECIALCHARACTERS ;                                        00018490
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00018500
 BOOLEAN STREAM PROCEDURE ALLBLANK (S, SKP, N) ;                        00018600
 VALUE SKP,                                                             00018700
   N ;                                                                  00018800
  BEGIN                                                                 00018900
 LABEL GRPMKIT ;                                                        00019000
    SI := S ;                                                           00019100
    SI := SI + SKP ;                                                    00019200
    SI := SI + N ;                                                      00019300
    N (SI := SI - 1 ;                                                   00019400
    IF SC NEQ " " THEN                                                  00019500
      JUMP OUT TO GRPMKIT) ;                                            00019600
    TALLY := 1 ;                                                        00019700
    SI := SI - 1 ;                                                      00019800
 GRPMKIT:                                                               00019900
    SI := SI + 1 ;                                                      00020000
    N := SI ;                                                           00020100
    DI := N ;                                                           00020200
    DS := 1 LIT "~" ;                                                   00020300
    ALLBLANK := TALLY ;                                                 00020400
  END ALLBLANK ;                                                        00020500
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00020600
  PROCEDURE WRITEALINE (K) ;                                            00020810
  VALUE K ; INTEGER K ;                                                 00020820
  BEGIN                                                                 00020830
  INTEGER NN ;                                                          00020840
    IF (LINECOUNT := LINECOUNT + 1) MOD PAGELENGTH = 0 THEN             00020850
    BEGIN                                                               00020860
      NN := N ;                                                         00020870
      N := LINECOUNT DIV PAGELENGTH ;                                   00020880
      WRITE (TWXOUT, NEWPAGE, LISTN) ;                                  00020890
      N := NN ;                                                         00020900
    END ;                                                               00020910
    IF K = 1 THEN                                                       00020920
      WRITE (TWXOUT, LFCR)                                              00020930
    ELSE IF K = 3 THEN                                                  00020931
    BEGIN                                                               00020932
      OCTDEC (NN:= N, N) ;                                              00020933
      IF NN = INFINITY - 1 THEN WRITE (TWXOUT, ALLNINES)                00020934
      ELSE IF FILETYPE = 2 THEN WRITE (TWXOUT, SEQCO, LISTN)            00020935
      ELSE WRITE (TWXOUT, SEQAO, LISTN) ;                               00020936
      N := NN ;                                                         00020937
    END                                                                 00020938
    ELSE IF K = 2 THEN                                                  00020940
      IF N = INFINITY - 1 THEN                                          00020950
        WRITE (TWXOUT, ALLNINES)                                        00020960
      ELSE                                                              00020970
        IF FILETYPE = 2 THEN                                            00020975
          WRITE (TWXOUT, SEQC, LISTN)                                   00020980
        ELSE WRITE (TWXOUT, SEQA, LISTN) ;                              00020985
  END WRITEALINE ;                                                      00020990
  DEFINE ANOTHERLINE = WRITEALINE (0)#,                                 00021000
    WRITELFCR = WRITEALINE (1)#,                                        00021010
    WRITESEQ = WRITEALINE (2)#,                                         00021020
    WRITEOLDSEQ = WRITEALINE (3)# ;                                     00021030
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00021100
 SWITCH FORMAT ERRS := (X8, "{!", 2 (A1, A6), "~"),                     00021200
   (X8, "{!INPUT LOST (TOO LONG).~", 4A1),                              00021300
   (X8, "{!BAD NAME  ", A1, A6, "/", A1, A6, "~"),                      00021400
   (X8, "{!",  A1, A6, " INVALID(*", A1, A6, ")~"),                     00021500
   (X8, "{!DUP. FILE ", A1, A6, "/", A1, A6, "~"),                      00021600
   (X8, "{!NO   FILE ", A1, A6, "/", A1, A6, "~"),                      00021700
   (X8, "{!NUMERIC EXPECTED- ", A1, A6, "~", 2A1),                      00021800
   (X8, "{!BAD FILE ", A1, A6, "/", A1, A6, "~"),                       00021900
   (X8, "{!INLINE TYPE ", 2 (A1, A6), "~"),                             00022300
   (X8, "{!INV USER: ", A1, A6, "/", A1, A6, "~") ;                     00022310
 DEFINE ERRF = ERRS [IF K = -1 THEN 0 ELSE K]# ;                        00022400
 PROCEDURE ERROR (K, A, B) ;                                            00022500
 VALUE K,                                                               00022600
   A,                                                                   00022700
   B ;                                                                  00022800
 INTEGER K ;                                                            00022900
 REAL  A,                                                               00023000
   B ;                                                                  00023100
  BEGIN                                                                 00023200
    ANOTHERLINE ;                                                       00023210
    IF A = "#000000" THEN                                               00023220
      A := "       " ;                                                  00023230
    IF B = "#000000" THEN                                               00023240
      B := "       " ;                                                  00023250
    WRITE (TWXOUT, ERRF, A.[6 : 6], A, B.[6 : 6], B) ;                  00023300
    IF K GEQ 0 THEN                                                     00023325
    MOREINPUT := FALSE ;                                                00023350
    GO TO NEXT ;                                                        00023400
  END ERROR ;                                                           00023500
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00023600
 PROCEDURE READIN ;                                                     00023700
  BEGIN                                                                 00023800
 INTEGER STREAM PROCEDURE CHRSBEFOREGRPMK (S) ;                         00023900
    BEGIN                                                               00024000
      SI := S ;                                                         00024100
      56 (IF SC = "~" THEN                                              00024200
        JUMP OUT ;                                                      00024300
      SI := SI + 1 ;                                                    00024400
      TALLY := TALLY + 1) ;                                             00024500
      CHRSBEFOREGRPMK := TALLY ;                                        00024600
    END CHRSBEFOREGRPMK ;                                               00024700
 BOOLEAN STREAM PROCEDURE MORE (IMAGE, INPUT) ;                         00024800
 BEGIN                                                                  00024805
 LOCAL QUOTES, ENDQUOTE, TEMP ;                                         00024810
 LABEL NOTHINGYET, BUMP, FOUNDQUOTE, FOUNDSEMICOLON, LOOP, XIT ;        00024815
    SI := IMAGE ;                                                       00024820
    DI := LOC QUOTES ;                                                  00024825
    DS := 2 LIT """ ;                                                   00024830
    DS := 4 LIT "()[]" ;                                                00024835
    2 ( 40 ( CI := CI + MORE ;                                          00024840
      GO TO NOTHINGYET ;                                                00024845
      GO TO FOUNDSEMICOLON ;                                            00024850
      GO TO FOUNDQUOTE ;                                                00024855
 NOTHINGYET:                                                            00024860
      IF SC = ALPHA THEN                                                00024865
        GO TO BUMP ;                                                    00024870
      IF SC = " " THEN                                                  00024875
        GO TO BUMP ;                                                    00024880
      DI := LOC QUOTES ;                                                00024885
      3 (IF SC = DC THEN JUMP OUT ;                                     00024890
         SI := SI - 1 ;                                                 00024895
         DI := DI + 1) ;                                                00024900
      IF TOGGLE THEN                                                    00024905
      BEGIN                                                             00024910
        TEMP := SI ;                                                    00024915
        ENDQUOTE := DI ;                                                00024920
        DI := LOC ENDQUOTE ;                                            00024925
        SI := ENDQUOTE ;                                                00024930
        DS := 1 CHR ;                                                   00024935
        TALLY := 2 ;                                                    00024940
        MORE := TALLY ;                                                 00024945
        SI := TEMP ;                                                    00024950
        GO TO LOOP ;                                                    00024955
      END ;                                                             00024960
      IF SC = "." THEN                                                  00024965
        JUMP OUT 2 TO XIT ;                                             00024970
      IF SC = ";" THEN                                                  00024975
      BEGIN                                                             00024980
        TALLY := 1 ;                                                    00024985
        MORE := TALLY ;                                                 00024990
        TEMP := SI ;                                                    00024995
        SI := INPUT ;                                                   00025000
        DI := INPUT ;                                                   00025005
        DS := 8 LIT "        " ;                                        00025010
        DS := 9 WDS ;                                                   00025015
        SI := TEMP ;                                                    00025020
        DI := INPUT ;                                                   00025025
      END ;                                                             00025030
 BUMP:                                                                  00025035
      SI := SI + 1 ;                                                    00025040
      GO TO LOOP ;                                                      00025045
 FOUNDSEMICOLON:                                                        00025050
      DS := 1 CHR ;                                                     00025055
      GO TO LOOP ;                                                      00025060
 FOUNDQUOTE:                                                            00025065
      DI := LOC ENDQUOTE ;                                              00025070
      IF SC = DC THEN                                                   00025075
      BEGIN                                                             00025080
        TALLY := 0 ;                                                    00025085
        MORE := TALLY ;                                                 00025090
      END ;                                                             00025095
 LOOP: )) ;                                                             00025100
 XIT:                                                                   00025105
 END MORE ;                                                             00025110
 STREAM PROCEDURE FIX (IM, TAB, N, Z) ;                                 00025115
 VALUE TAB, N ;                                                         00025120
 BEGIN                                                                  00025125
 LABEL XIT ;                                                            00025130
    SI := Z ;                                                           00025135
    DI := Z ;                                                           00025140
    DS := 8 LIT "        " ;                                            00025145
    DS := 9 WDS ;                                                       00025150
    SI := IM ;                                                          00025155
    DI := Z ;                                                           00025160
    DI := DI + TAB ;                                                    00025165
    N (2 (DS := 40 CHR) ;  JUMP OUT TO XIT) ;                           00025170
    2 (40 (IF SC = "~" THEN JUMP OUT 2 TO XIT ; DS := 1 CHR)) ;         00025175
 XIT:                                                                   00025180
    SI := Z ;                                                           00025185
    DI := IM ;                                                          00025190
    DS := 10 WDS ;                                                      00025195
 END FIX ;                                                              00025200
 BOOLEAN STREAM PROCEDURE STAR (S) ;                                    00025500
    BEGIN                                                               00025600
      SI := S ;                                                         00025700
      IF SC = "*" THEN                                                  00025800
        TALLY := 1 ;                                                    00025900
      STAR := TALLY ;                                                   00026000
    END STAR ;                                                          00026100
 INTEGER C,                                                             00026200
   CHRS ;                                                               00026300
 OWN ALPHA ARRAY INPUT [0 : 9] ;                                        00026350
 LABEL AGAIN, EXIT ;                                                    00026400
   IF MOREINPUT THEN                                                    00026401
   BEGIN                                                                00026402
     READ (INPUT [*], 10, IMAGE [*]) ;                                  00026403
     GO TO EXIT ;                                                       00026404
   END ;                                                                00026405
   IF FILESECURITY THEN                                                 00026406
   BEGIN                                                                00026410
    SEARCH (RECOVER, IMAGE [*]) ;                                       00026411
    IF IMAGE [0] = -1 THEN                                              00026412
    BEGIN                                                               00026413
      FILL SCRATCH WITH USERCODE, USERCODE ;                            00026414
      WRITE (SCRATCH, *, LISTN) ;                                       00026415
      LOCK (SCRATCH) ;                                                  00026416
    END ;                                                               00026417
    READ SEEK (RECOVER [0]) ;                                           00026418
    WRITE (RECOVER, RSWDM + 1, RSWD [*]) ;                              00026420
    CONTROLS [0] := N ;                                                 00026425
    CONTROLS [1] := AT & D [22 : 35 : 13] ;                             00026430
    CONTROLS [2] := M ;                                                 00026435
    CONTROLS [3] := INC ;                                               00026440
    CONTROLS [4] := REAL (BOOL) ;                                       00026445
    WRITE (RECOVER, CONTROLTOP + 1, CONTROLS [*]) ;                     00026450
    IF RSWDL LEQ READONLYLIMIT THEN                                     00026455
      FOR I := D DIV 256 STEP -1 UNTIL 0 DO                             00026460
        WRITE (RECOVER, 256, LINKLIST [I, *]) ;                         00026465
    CLOSE (DISC) ;  PREWHERE := -1 ;                                    00026470
    CLOSE (RECOVER) ;                                                   00026475
   END ;                                                                00026480
    INSERT (39, " ", IMAGE [0]) ;                                       00026500
    INSERT (39, " ", IMAGE [5]) ;                                       00026510
    EMPTY1 := FALSE ;                                                   00026550
 AGAIN:                                                                 00026600
    IF C := STATUS (INPUT [*]) GTR 2 THEN                               00026700
      FOR C := 0, 2 DO                                                  00026800
      IF STATION.[9 : 9] NEQ INPUT [C].[9 : 9] THEN                     00026900
    BEGIN                                                               00027000
      WRITE (TWXOUTPUT (INPUT [C]), ERRS [0], "U", "SE EXE", "C",       00027100
        "UTE.  ") ;                                                     00027200
      RELEASE (INPUT [C]) ;                                             00027300
      GO TO AGAIN ;                                                     00027400
    END ;                                                               00027500
    IF C = 0 THEN                                                       00027510
      GO TO STOP ;                                                      00027520
    INSERT (63, "~", INPUT [0]) ;                                       00027600
    READ (TWXIN, 8, INPUT [*]) [STOP] ;                                 00027700
    C := CHRSBEFOREGRPMK (INPUT [1]) ;                                  00027800
    IF C = 32 THEN                                                      00027900
      IF INPUT [0].[25 : 1] = 1 THEN                                    00028000
      C := 28 ;                                                         00028100
                                                                        00028200
                                                                        00028300
                                                                        00028400
    IF CHRS + C GTR 80 OR CHRS GTR 63 THEN                              00028500
      ERROR (1, 0, 0) ;                                                 00028600
    MOVE (INPUT [1], IMAGE [0], 0, CHRS, C + 1) ;                       00028700
    CHRS := CHRS + C ;                                                  00028800
    IF INPUT [0].[25 : 1] NEQ 0 THEN                                    00028900
      GO TO AGAIN ;                                                     00029000
 EXIT:                                                                  00029010
    NOSTAR := NOT STAR (IMAGE) ;                                        00029015
    IF NOSTAR THEN                                                      00029020
    BEGIN                                                               00029030
      FIX (IMAGE, TABAMOUNT, INLINETOG, ZIPPY) ;                        00029040
      MOREINPUT := FALSE                                                00029050
    END                                                                 00029060
    ELSE                                                                00029070
      MOREINPUT := MORE (IMAGE, INPUT) ;                                00029080
  END READIN ;                                                          00029100
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00029110
 PROCEDURE RDISC (WHERE, RECAGE) ;                                      00029120
 VALUE WHERE ;                                                          00029130
 INTEGER WHERE ;                                                        00029140
 ARRAY RECAGE [0] ;                                                     00029150
 BEGIN                                                                  00029160
    IF PREWHERE + 1 NEQ PREWHERE := WHERE THEN                          00029170
      READ (DISC [WHERE]) ;                                             00029180
    READ (DISC, 10, RECAGE [*]) ;                                       00029190
 END RDISC ;                                                            00029195
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00029200
 PROCEDURE WRITEAT ;                                                    00029300
  BEGIN                                                                 00029400
 INTEGER I ;                                                            00029500
 LABEL BLANKLINE ;                                                      00029600
    RDISC (AT - 2, RECORD) ;                                            00029700
                                                                        00029800
    N := LINKLISTSUBAT.S ;                                              00029900
    IF FILETYPE NEQ 2 THEN                                              00030000
      WRITESEQ                                                          00030100
    ELSE                                                                00030200
    BEGIN                                                               00030205
      WRITELFCR ;                                                       00030210
      COBOLSEQ (N, RECORD, IMAGE, SUFFIX, TRUE) ;                       00030220
    END ;                                                               00030230
    EXTRALFCR := FALSE ;                                                00030250
    BLANKOUTSPECIALCHARACTERS (RECORD,IMAGE,QUICK) ;                    00030275
    IF ALLBLANK (RECORD [7], 0, 16) THEN                                00030300
      IF ALLBLANK (RECORD [3], 4, 28) THEN                              00030400
      IF ALLBLANK (RECORD [0], 0, 28) THEN                              00030500
      GO TO BLANKLINE ;                                                 00030600
    IF NOT ALLBLANK (RECORD [8], 0, 8) THEN                             00030700
      IF FILETYPE NEQ 2 THEN                                            00030800
      BEGIN                                                             00030825
        EXTRALFCR := TRUE ;                                             00030850
      WRITELFCR ;                                                       00030900
       END ;                                                            00030950
    INSERT (7, "~", IMAGE [4]) ;                                        00031000
    FOR I := 0 STEP 1 UNTIL 2 DO                                        00031100
    BEGIN                                                               00031200
      MOVE (RECORD [3 | I], IMAGE [1], 4 | I, 0, 28) ;                  00031300
      WRITE (TWXOUT, 5, IMAGE [*]) [: NEXT] ;                           00031400
    END ;                                                               00031500
 BLANKLINE:                                                             00031600
  END WRITEAT ;                                                         00031700
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00031800
 BOOLEAN PROCEDURE POSTMAN ;                                            00031900
 BEGIN                                                                  00031925
 BOOLEAN STREAM PROCEDURE POSTFROM (SENDER, MESSAGE, Z) ;               00031950
 BEGIN                                                                  00031960
 LABEL OK, EXIT ;                                                       00031970
                                                                        00031975
    SI := Z ;                                                           00031980
    DI := Z ;                                                           00031990
    DS := 8 LIT "        " ;                                            00032000
    DS := 8 WDS ;                                                       00032010
    SI := MESSAGE ;                                                     00032020
                                                                        00032025
    20 (IF SC = ":" THEN                                                00032030
          JUMP OUT TO OK ;                                              00032040
        SI := SI + 1) ;                                                 00032050
    TALLY := 1 ;                                                        00032060
    POSTFROM := TALLY ;                                                 00032070
                                                                        00032075
    GO TO EXIT ;                                                        00032080
 OK:                                                                    00032090
    SI := SI + 1 ;                                                      00032100
    DI := Z ;                                                           00032110
    63 (IF SC = "~" THEN                                                00032120
                                                                        00032125
          JUMP OUT ;                                                    00032130
        DS := 1 CHR) ;                                                  00032140
    DS := 1 LIT "-" ;                                                   00032150
    SI := SENDER ;                                                      00032160
    SI := SI + 1 ;                                                      00032170
                                                                        00032175
    DS := 7 CHR ;                                                       00032180
 EXIT:                                                                  00032190
 END POSTFROM ;                                                         00032200
                                                                        00032225
                                                                        00032250
                                                                        00032275
                                                                        00032300
                                                                        00032325
                                                                        00032350
                                                                        00032375
    IF NUM2 THEN                                                        00032400
    BEGIN                                                               00032425
      OCTDEC (CONTROLS [2], M) ;                                        00032450
      CONTROLS [2] := M ;                                               00032475
    END ;                                                               00032500
  FILL LIBRARY WITH "MAIL % ", IF EMPTY1 THEN USERCODE ELSE CONTROLS[2];00032525
  SEARCH (LIBRARY, ZIPPY [*]) ;                                         00032550
  IF EMPTY1 THEN                                                        00032575
  BEGIN                                                                 00032600
    IF ZIPPY [0] = -1 THEN                                              00032625
      ERROR (0, "SORRY, ", "NO MAIL") ;                                 00032650
    CONTROLS [1] := "MAIL % " ;                                         00032675
    EMPTY1 := FALSE ;                                                   00032700
    NUM1 := FALSE ;                                                     00032725
    CONTROLS [2] := USERCODE ;                                          00032750
    EMPTY2 := FALSE ;                                                   00032775
    NUM2 := FALSE ;                                                     00032800
    NUM3 := FALSE ;                                                     00032825
    POSTMAN := TRUE ;                                                   00032850
  END ELSE                                                              00032875
  BEGIN                                                                 00032900
    IF CONTROLS [1] NEQ "TO     " THEN                                  00032925
      ERROR (0, "MISSING", " TO.   ") ;                                 00032950
    IF POSTFROM (USERCODE, IMAGE, RECORD) THEN                          00032975
      ERROR (0, "MISSING", " COLON.") ;                                 00033000
    IF ZIPPY [0] = -1 THEN                                              00033025
    BEGIN                                                               00033050
      I := STATUS (STATION, 3) ;                                        00033075
      FILL SCRATCH WITH "MAIL % ", CONTROLS [2] ;                       00033100
      WRITE (SCRATCH, 10, RECORD [*]) ;                                 00033125
      LOCK (SCRATCH) ;                                                  00033150
      I := STATUS (STATION, 4) ;                                        00033175
    END ELSE                                                            00033200
    BEGIN                                                               00033225
      WRITE (LIBRARY [ZIPPY [5] + 1], 10, RECORD [*]) ;                 00033250
      CLOSE (LIBRARY) ;                                                 00033275
    END ;                                                               00033300
    MOREINPUT := FALSE ;                                                00033302
    POSTMAN := FALSE ;                                                  00033325
  END ;                                                                 00033350
 END POSTMAN ;                                                          00033375
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00033400
                                                                        00033500
                                                                        00033600
                                                                        00033700
                                                                        00033800
                                                                        00033900
 BOOLEAN PROCEDURE ITSOLD (N) ;                                         00034000
 VALUE N ;                                                              00034100
 INTEGER N ;                                                            00034200
  BEGIN                                                                 00034300
    WHILE N LSS LINKLISTSUBAT.S DO                                      00034400
      AT := LINKLISTSUBAT.F ;                                           00034500
    WHILE N GTR LINKLISTSUBAT.S DO                                      00034600
      AT := LINKLISTSUBAT.T ;                                           00034700
    ITSOLD := N = LINKLISTSUBAT.S ;                                     00034800
  END ITSOLD ;                                                          00034900
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00035000
 STREAM PROCEDURE STAR (S, D) ;                                         00035100
  BEGIN                                                                 00035200
 LOCAL N,                                                               00035300
   K ;                                                                  00035310
 LABEL DEBLANK,                                                         00035400
   NALPHA,                                                              00035410
   BLANK,                                                               00035420
   NUMALPHA ;                                                           00035450
    SI := S ;                                                           00035500
                                                                        00035600
                                                                        00035700
      DI := S ;                                                         00035800
      2 (DI := DI + 36) ;                                               00035900
      DS := 1 LIT """ ;                                                 00036000
      DI := D ;                                                         00036100
      5 (DS := 8 LIT "0#000000") ;                                      00036200
      DI := D ;                                                         00036300
      5 (TALLY := 0 ;                                                   00036400
      K := TALLY ;                                                      00036410
 DEBLANK:                                                               00036500
      TALLY := 0 ;                                                      00036510
      SI := SI + 1 ;                                                    00036600
      IF SC = " " THEN                                                  00036700
        GO TO DEBLANK ;                                                 00036800
      IF SC = ALPHA THEN ELSE                                           00036802
      BEGIN                                                             00036804
      IF SC = "." THEN                                                  00036810
        JUMP OUT ;                                                      00036820
      IF SC = ";" THEN                                                  00036830
        JUMP OUT ;                                                      00036840
      IF SC = """ THEN                                                  00036900
        JUMP OUT ;                                                      00036920
      IF SC = "(" THEN                                                  00036940
        JUMP OUT ;                                                      00036960
      IF SC = "[" THEN                                                  00036980
        JUMP OUT ;                                                      00037000
      IF SC = "/" THEN                                                  00037010
        TALLY := 1 ;                                                    00037020
      K := TALLY ;                                                      00037030
      GO TO DEBLANK ;                                                   00037040
      END ;                                                             00037050
      IF SC GEQ "0" THEN                                                00037100
      BEGIN                                                             00037200
        K (JUMP OUT TO NALPHA) ;                                        00037210
        K := SI ;                                                       00037220
        8 (IF SC LSS "0" THEN                                           00037300
          JUMP OUT ;                                                    00037400
        TALLY := TALLY + 1 ;                                            00037500
        SI := SI + 1) ;                                                 00037600
        N := TALLY ;                                                    00037700
        IF TOGGLE THEN                                                  00037705
        BEGIN                                                           00037710
          IF SC = ALPHA THEN                                            00037715
            GO TO NUMALPHA ;                                            00037720
 BLANK:   IF SC = " " THEN BEGIN SI := SI + 1 ; GO TO BLANK ; END ;     00037722
          IF SC = "/" THEN                                              00037725
          BEGIN                                                         00037730
 NUMALPHA:                                                              00037735
            SI := K ;                                                   00037740
            GO TO NALPHA ;                                              00037745
          END ;                                                         00037750
        END ;                                                           00037755
        SI := K ;                                                       00037800
        DS := N OCT ;                                                   00037900
        SI := SI - 1 ;                                                  00037950
      END                                                               00038000
      ELSE                                                              00038100
      BEGIN                                                             00038200
 NALPHA:                                                                00038250
        DS := 1 LIT "+" ;                                               00038300
        7 (IF SC = ALPHA THEN                                           00038400
          DS := 1 CHR                                                   00038500
        ELSE DS := 1 LIT " ") ;                                         00038600
        SI := SI - 1 ;                                                  00038650
      END) ;                                                            00038700
                                                                        00038800
                                                                        00038900
                                                                        00039000
                                                                        00039100
  END STAR ;                                                            00039200
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00039300
 BOOLEAN STREAM PROCEDURE INLINEEDIT (S, D, T, INITIAL) ;               00039400
   VALUE INITIAL ;                                                      00039450
  BEGIN                                                                 00039500
 LABEL SEARCH,                                                          00039600
   INSERT,                                                              00039700
   DELETE,                                                              00039800
   REPLACE,                                                             00039900
   WRAPUP,                                                              00040000
   LOOP,                                                                00040100
   IDR,                                                                 00040150
   XIT ;                                                                00040200
 LOCAL BIDR ;                                                           00040300
    DI := LOC BIDR ;                                                    00040325
    DS := 4 LIT " IDR" ;                                                00040350
    DI := T ;                                                           00040400
    DS := 8 LIT "        " ;                                            00040500
    SI := T ;                                                           00040600
    DS := 9 WDS ;                                                       00040700
    TALLY := INITIAL ;                                                  00040725
    INLINEEDIT := TALLY ;                                               00040750
    TALLY := 0 ;                                                        00040775
    2 (36 (CI := CI + INLINEEDIT ;                                      00040800
    GO TO SEARCH ;                                                      00040900
    GO TO IDR ;                                                         00041000
    GO TO IDR ;                                                         00041100
    GO TO IDR ;                                                         00041200
    GO TO WRAPUP ;                                                      00041300
 SEARCH:                                                                00041400
    SI := D ;                                                           00041500
    DI := T ;                                                           00041600
    DS := 1 CHR ;                                                       00041700
    D := SI ;                                                           00041800
    T := DI ;                                                           00041900
    SI := S ;                                                           00042000
    DI := LOC BIDR ;                                                    00042100
    4 (IF SC = DC THEN                                                  00042200
         JUMP OUT ;                                                     00042300
       SI := SI - 1 ;                                                   00042400
       TALLY := TALLY + 1) ;                                            00042500
    IF TOGGLE THEN ELSE                                                 00042600
    BEGIN                                                               00042700
      TALLY := 1 ;                                                      00042800
      JUMP OUT 2 TO XIT ;                                               00042900
    END ;                                                               00043000
                                                                        00043100
                                                                        00043200
                                                                        00043300
                                                                        00043400
                                                                        00043500
                                                                        00043600
                                                                        00043700
                                                                        00043800
                                                                        00043900
                                                                        00044000
    INLINEEDIT := TALLY ;                                               00044100
    TALLY := 0 ;                                                        00044200
                                                                        00044300
    S := SI ;                                                           00044400
    GO TO LOOP ;                                                        00044500
 IDR:                                                                   00044600
    SI := S ;                                                           00044700
    IF SC = "~" THEN                                                    00044800
    BEGIN                                                               00044810
      SI := D ;                                                         00044820
      DI := T ;                                                         00044830
      TALLY := 4 ;                                                      00044840
      INLINEEDIT := TALLY ;                                             00044850
 WRAPUP:                                                                00044860
      DS := 1 CHR ;                                                     00044870
      GO TO LOOP ;                                                      00044880
    END ;                                                               00044890
                                                                        00044900
    CI := CI + INLINEEDIT ;                                             00044905
    GO TO WRAPUP ;                                                      00044910
    GO TO INSERT ;                                                      00044915
    GO TO DELETE ;                                                      00044920
    GO TO REPLACE ;                                                     00044925
 INSERT:                                                                00044930
    DI := T ;                                                           00045000
    DS := 1 CHR ;                                                       00045100
    S := SI ;                                                           00045200
    T := DI ;                                                           00045300
    GO TO LOOP ;                                                        00045400
 DELETE:                                                                00045500
                                                                        00045600
                                                                        00045700
                                                                        00045800
    DI := D ;                                                           00045900
    DI := DI + 1 ;                                                      00046000
    D := DI ;                                                           00046100
    SI := SI + 1 ;                                                      00046200
    S := SI ;                                                           00046300
    GO TO LOOP ;                                                        00046400
 REPLACE:                                                               00046500
                                                                        00046600
                                                                        00046700
                                                                        00046800
    DI := T ;                                                           00046900
    DS := 1 CHR ;                                                       00047000
    S := SI ;                                                           00047100
    T := DI ;                                                           00047200
    SI := D ;                                                           00047300
    SI := SI + 1 ;                                                      00047400
    D := SI ;                                                           00047500
    GO TO LOOP ;                                                        00047600
                                                                        00047700
                                                                        00047800
                                                                        00047900
                                                                        00048000
                                                                        00048100
                                                                        00048200
                                                                        00048300
                                                                        00048400
 LOOP:                                                                  00048500
   )) ;                                                                 00048600
    TALLY := 0 ;                                                        00048700
 XIT:                                                                   00048800
    INLINEEDIT := TALLY ;                                               00048900
  END INLINE ;                                                          00049000
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00049002
 STREAM PROCEDURE TRANSLATE (RECORD, SCRATCH) ;                         00049004
  BEGIN                                                                 00049006
 LOCAL N ;                                                              00049008
    SI := SCRATCH ;                                                     00049012
    DI := SCRATCH ;                                                     00049014
    DS := 8 LIT "        " ;                                            00049016
    DS := 8 WDS ;                                                       00049018
    SI := RECORD ;                                                      00049020
    DI := SCRATCH ;                                                     00049022
    2(36(                                                               00049024
    IF TOGGLE THEN                                                      00049028
    BEGIN                                                               00049030
      N := TALLY ;                                                      00049032
      TALLY := 1 ;                                                      00049034
      IF SC = "-" THEN DS := 1 LIT "~"                                  00049036
      ELSE IF SC = "*" THEN DS := 1 LIT "*"                             00049037
      ELSE IF SC = "=" THEN DS := 1 LIT "!"                             00049038
      ELSE IF SC = "(" THEN DS := 1 LIT "<"                             00049040
      ELSE IF SC = ")" THEN DS := 1 LIT ">"                             00049042
      ELSE IF SC = "[" THEN DS := 1 LIT "{"                             00049044
      ELSE IF SC = "]" THEN DS := 1 LIT "}"                             00049046
    ELSE IF SC = "%" THEN BEGIN DS:=2 RESET; DS:=2 SET; DS:=2 RESET; END00049047
      ELSE                                                              00049048
      BEGIN                                                             00049050
        TALLY := N ;                                                    00049052
        DS := 1 LIT "%" ;                                               00049054
        DS := 1 CHR ;                                                   00049066
        SI := SI - 1 ;                                                  00049068
      END ;                                                             00049070
      IF SC = "%" THEN IF SC = "A" THEN ; % THIS RESETS TOGGLE.         00049072
      SI := SI + 1 ;                                                    00049074
    END                                                                 00049076
    ELSE IF SC = "%" THEN                                               00049078
      SI := SI + 1                                                      00049080
    ELSE DS := 1 CHR)) ;                                                00049082
    N := TALLY ;                                                        00049084
    N (SI := SCRATCH ;                                                  00049086
    DI := RECORD ;                                                      00049088
    DS := 9 WDS) ;                                                      00049090
  END TRANSLATE ;                                                       00049092
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00049100
 PROCEDURE ADJUSTFORNUMERICIDS ;                                        00049200
  BEGIN                                                                 00049300
    IF NUM1 THEN                                                        00049400
    BEGIN                                                               00049500
      OCTDEC (CONTROLS [1], M) ;                                        00049600
      CONTROLS [1] := M ;                                               00049700
    END ;                                                               00049800
    IF NUM2 THEN                                                        00049900
    BEGIN                                                               00050000
      OCTDEC (CONTROLS [2], M) ;                                        00050100
      CONTROLS [2] := M ;                                               00050200
    END ;                                                               00050300
  END ADJUSTFORNUMERICIDS ;                                             00050400
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00050401
 PROCEDURE FINDSEGRELADDR ;                                             00050402
  BEGIN                                                                 00050403
 LABEL EOF ; FILE LINE 15 (2,15) ; FILL LINE WITH "LINE   ", USERCODE ; 00050404
    FILL FEEDBACK WITH "LINE   ", USERCODE ;                            00050405
    SEARCH (FEEDBACK, IMAGE [*]) ;                                      00050406
    IF IMAGE [0] LEQ 0 THEN                                             00050407
      ERROR (9 + 4 | IMAGE [0], "LINE   ", USERCODE) ;                  00050408
    IF CONTROLS [1] = "ALGOL  " THEN                                    00050409
      I := 0                                                            00050410
    ELSE IF CONTROLS [1] = "FORTRAN" THEN                               00050411
      I := 1                                                            00050412
    ELSE IF CONTROLS [1] = "COBOL  " THEN                               00050413
      I := 2                                                            00050414
    ELSE IF NOT EMPTY1 THEN                                             00050415
      ERROR (7, "TYPE:  ", CONTROLS [1]) ;                              00050416
    WRITE (TWXOUT, WAIT) ; CONTROLS [0] := N ;                          00050417
    IF NUM2 AND NUM3 AND NUM4 THEN                                      00050418
    BEGIN                                                               00050419
    B := TRUE ;                                                         00050420
    WHILE B DO                                                          00050421
    BEGIN                                                               00050422
      READ (FEEDBACK, 15, ZIPPY [*]) [EOF] ;                            00050423
      IF I = 0 THEN                                                     00050424
      BEGIN                                                             00050425
        READ (ZIPPY [*], X116I4, LISTN) ;                               00050426
        IF N = 0 THEN                                                   00050427
          N := M                                                        00050428
        ELSE                                                            00050429
        BEGIN                                                           00050430
          M := N ;                                                      00050431
          N := 0 ;                                                      00050432
        END ;                                                           00050433
      END ELSE                                                          00050434
      IF I = 1 THEN                                                     00050435
      BEGIN                                                             00050436
        READ (ZIPPY [*], X92A4, LISTN) ;                                00050437
        IF N NEQ "LONG" THEN                                            00050438
        BEGIN                                                           00050439
          READ (ZIPPY [*], X91I4, LISTN) ;                              00050440
          IF N = 0 THEN                                                 00050441
            N := M                                                      00050442
          ELSE                                                          00050443
          BEGIN                                                         00050444
            M := N ;                                                    00050445
            N := 0 ;                                                    00050446
          END ;                                                         00050447
        END ELSE                                                        00050448
          N := 0 ;                                                      00050449
      END ELSE                                                          00050460
        READ (ZIPPY [*], X96I4, LISTN) ;                                00050461
      IF N = CONTROLS [2] THEN                                          00050462
      BEGIN                                                             00050463
        IF I = 0 THEN                                                   00050464
          READ (ZIPPY [*], X108I4, LISTN)                               00050465
        ELSE IF I = 1 THEN                                              00050466
          READ (ZIPPY [*], X84I4, LISTN)                                00050467
        ELSE READ (ZIPPY [*], X101I4, LISTN) ;                          00050468
          B := N LEQ CONTROLS [4] ;                                     00050469
          IF CONTROLS [3] LEQ N AND B THEN                              00050470
        BEGIN                                                           00050471
          IMAGE [0] := N ;                                              00050472
          IF I = 0 THEN                                                 00050473
            READ (ZIPPY [*], X96I8, LISTN)                              00050474
          ELSE IF I = 1 THEN                                            00050475
            READ (ZIPPY [*], X72I8, LISTN)                              00050476
          ELSE READ (ZIPPY [*], X5I6, LISTN) ;                          00050477
          WRITESEQ ;                                                    00050478
          N := CONTROLS [2] ;                                           00050479
          WRITE (TWXOUT, SEGMENT, LISTN) [:EOF] ;                       00050480
          N := IMAGE [0] ;                                              00050481
          WRITE (TWXOUT, RELADDR, LISTN) [:EOF] ;                       00050482
        END ;                                                           00050483
      END ;                                                             00050484
    END ;                                                               00050485
    END ELSE                                                            00050486
    IF CONTROLS [2] . [6:30] = "ERROR" OR CONTROLS [2] = "SYNTAX " THEN 00050487
    BEGIN                                                               00050488
      M := I ;                                                          00050489
                                                                        00050490
      WHILE TRUE DO                                                     00050491
      BEGIN                                                             00050492
        READ (FEEDBACK, 15, ZIPPY [*]) [EOF] ;                          00050493
        IF M = 2 THEN                                                   00050494
        BEGIN                                                           00050495
          READ (ZIPPY [*], A1, LISTN) ;                                 00050496
          IF N = " " OR N = "[" THEN                                    00050497
            READ (ZIPPY [*], X5I6, LISTN)                               00050498
          ELSE                                                          00050499
            N := 0 ;                                                    00050500
        END ELSE                                                        00050501
        IF M = 1 THEN                                                   00050502
          READ (ZIPPY [*], X72I8, LISTN)                                00050503
        ELSE                                                            00050504
          READ (ZIPPY [*], X96I8, LISTN) ;                              00050505
        IF N NEQ 0 THEN                                                 00050506
          I := N                                                        00050507
        ELSE                                                            00050508
        BEGIN                                                           00050509
          N := I ;                                                      00050510
          WRITESEQ ;                                                    00050511
          BLANKOUTSPECIALCHARACTERS (ZIPPY,IMAGE,1) ;                   00050512
          IF ALLBLANK (ZIPPY [7], 0, 16) THEN                           00050513
            IF ALLBLANK (ZIPPY [3], 4, 28) THEN                         00050514
            B := ALLBLANK (ZIPPY [0], 0, 28) ;                          00050515
          INSERT (7, "~", IMAGE [4]) ;                                  00050516
          FOR I := 0 STEP 1 UNTIL 2 DO                                  00050517
          BEGIN                                                         00050518
            MOVE (ZIPPY [3 | I], IMAGE [1], 4 | I, 0, 28) ;             00050519
            WRITE (TWXOUT, 5, IMAGE [*]) [:EOF] ;                       00050520
          END ;                                                         00050521
        END ;                                                           00050522
      END ;                                                             00050523
    END ELSE                                                            00050524
    IF EMPTY1 THEN                                                      00050525
      WHILE TRUE DO                                                     00050526
      BEGIN                                                             00050527
        READ (FEEDBACK, 15, ZIPPY [*]) [EOF] ;                          00050528
        WRITE (LINE [DBL], 15, ZIPPY [*]) ;                             00050529
      END ELSE                                                          00050530
      ERROR (6, "3 TIMES", 0) ;                                         00050531
 EOF:                                                                   00050532
    CLOSE (FEEDBACK) ;                                                  00050533
    N := CONTROLS [0] ;                                                 00050534
 END FINDSEGRELADDR ;                                                   00050535
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00050536
 PROCEDURE VERIFAX (XEROX) ;                                            00050537
 VALUE XEROX ;                                                          00050600
 BOOLEAN XEROX ;                                                        00050700
  BEGIN                                                                 00050800
 FILE COPY DISK SERIAL [20 : 15 | ((D + 14) DIV 15)] (2, IF PRINTING    00050900
      THEN                                                              00051000
      15                                                                00051100
    ELSE 10, IF PRINTING THEN                                           00051200
      15                                                                00051300
    ELSE 150, SAVE SAVEFACTOR) ;                                        00051400
 DEFINE SAVED = COPY# ;                                                 00051700
 DEFINE ALREADYSEQ = TRUE# ;                                            00051800
 INTEGER AT ;                                                           00051900
 REAL I ;                                                               00052000
 DEFINE IMAGE = ZIPPY # ;                                               00052050
 REAL ARRAY LINCLIST [0 : D DIV 256, 0 : 255] ;                         00052100
 DEFINE LINKLISTSUBD = LINCLIST [D.LEFTSIDE, D.RIGHTSIDE]# ;            00052200
                                                                        00052210
                                                                        00052220
                                                                        00052230
                                                                        00052240
                                                                        00052250
                                                                        00052260
                                                                        00052270
                                                                        00052280
                                                                        00052290
    IF XEROX THEN                                                       00052300
    BEGIN   % XEROX                                                     00052400
      FILL COPY WITH PREFIX, SUFFIX, *, *, *, 12 ;                      00052500
      WRITE (TWXOUT, WAIT) ;                                            00052600
      D := 0 ;                                                          00052700
      LINKLISTSUBD := 2 ;                                               00052800
      D := 1 ;                                                          00052900
      WHILE AT := LINKLISTSUBAT.T NEQ 1 DO                              00053000
      BEGIN                                                             00053100
        RDISC (AT - 2, IMAGE) ;                                         00053200
                                                                        00053300
        N := LINKLISTSUBAT.S ;                                          00053400
        SEQUENCE ;                                                      00053500
        D := D + 1 ;                                                    00053600
        LINKLISTSUBD := 0 & N [SF] & (D - 1) [FF] & (D + 1) [TF] ;      00053700
        WRITE (COPY, 10, IMAGE [*]) ;                                   00053800
      END ;                                                             00053900
      LINKLISTSUBD.T := 1 ;                                             00054000
      RELEASE (STATION) ;                                               00054025
      CLOSE (DISC, PURGE) ; PREWHERE := -1 ;                            00054100
      LOCK (COPY) ;                                                     00054200
      SEEK (TWXINPUT (STATION)) ;                                       00054225
      IF RSWDL = 0 THEN                                                 00054300
        FOR I := D  DIV 256 STEP - 1 UNTIL 0 DO                         00054500
        WRITE (LINKLIST [I, *], 256, LINCLIST [I, *]) ;                 00054600
      AT := 1 ;                                                         00054700
      INORDER := TRUE ;                                                 00054800
      LINKLISTSUBAT := 0 & INFINITY [SF] & D [FF] & 1 [TF] ;            00054900
      AT := 2 ;                                                         00055000
      LINKLISTSUBAT.F := 0 ;                                            00055100
      N := N + INC ;                                                    00055150
      IF D = 1 THEN                                                     00055200
        SETRSWDL ;                                                      00055300
   IF FILESECURITY THEN                                                 00055306
   BEGIN                                                                00055308
    SEARCH (RECOVER, IMAGE [*]) ;                                       00055310
    IF IMAGE [0] = -1 THEN                                              00055312
    BEGIN                                                               00055314
      FILL SCRATCH WITH USERCODE, USERCODE ;                            00055316
      WRITE (SCRATCH, *, LISTN) ;                                       00055318
      LOCK (SCRATCH) ;                                                  00055320
    END ;                                                               00055322
    READ SEEK (RECOVER [0]) ;                                           00055324
    WRITE (RECOVER, RSWDM + 1, RSWD [*]) ;                              00055326
    READ (CONTROLS [*], 5, IMAGE [*]) ;                                 00055327
    CONTROLS [0] := N ;                                                 00055328
    CONTROLS [1] := AT & D [22 : 35 : 13] ;                             00055330
    CONTROLS [2] := M ;                                                 00055332
    CONTROLS [3] := INC ;                                               00055334
    CONTROLS [4] := REAL (BOOL) ;                                       00055336
    WRITE (RECOVER, CONTROLTOP + 1, CONTROLS [*]) ;                     00055338
    WRITE (CONTROLS [*], 5, IMAGE [*]) ;                                00055339
    IF RSWDL LEQ READONLYLIMIT THEN                                     00055340
      FOR I := D DIV 256 STEP -1 UNTIL 0 DO                             00055342
        WRITE (RECOVER, 256, LINKLIST [I, *]) ;                         00055344
    CLOSE (DISC) ;  PREWHERE := -1 ;                                    00055346
    CLOSE (RECOVER) ;                                                   00055348
   END ;                                                                00055350
    END XEROX                                                           00055400
    ELSE                                                                00055500
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00055600
    BEGIN   % THERMOFAX                                                 00055700
      FILL SAVED WITH CONTROLS [1], CONTROLS [2], *, *, *, IF PRINTING  00055800
        THEN                                                            00055900
        IF PUNCHING THEN 0 ELSE 15                                      00056000
      ELSE 12 ;                                                         00056100
      IF NOT PRINTING THEN                                              00056200
      BEGIN                                                             00056300
        SEARCH (SAVED, IMAGE [*]) ;                                     00056400
        IF IMAGE [0] NEQ - 1 THEN                                       00056500
          ERROR (4, CONTROLS [1], CONTROLS [2]) ;                       00056600
        CONTROLS [3] := -1 ;                                            00056625
        CONTROLS [4] := INFINITY ;                                      00056650
      END ELSE                                                          00056700
      IF NOT NUM4 THEN                                                  00056710
      BEGIN                                                             00056720
        CONTROLS [4] := INFINITY ;                                      00056730
        IF NOT NUM3 THEN                                                00056740
          CONTROLS [3] := -1 ;                                          00056750
      END ;                                                             00056760
      WRITE (TWXOUT, WAIT) ;                                            00056800
      IF PRINTING AND NOT PUNCHING THEN                                 00056805
      BEGIN                                                             00056810
        M := TIME (0) ;                                                 00056815
        WRITE (IMAGE [*], DATE, TIMEOUT) ;                              00056820
        IMAGE [4] := PREFIX ;                                           00056825
        INSERT (0, " ", IMAGE [4]) ;                                    00056830
        IMAGE [5] := SUFFIX ;                                           00056835
        INSERT (0, "/", IMAGE [5]) ;                                    00056840
        IMAGE [10] := USERCODE ;                                        00056845
        INSERT (0, " ", IMAGE [10]) ;                                   00056850
        WRITE (SAVED [DBL], 15, IMAGE [*]) ;                            00056855
        INSERT (7, " ", IMAGE [10]) ;                                   00056860
      END ;                                                             00056865
      I := N ;                                                          00056900
      AT := N := M := 0 ;                                               00057000
      WHILE AT := LINKLISTSUBAT.T NEQ 1 DO                              00057100
      IF CONTROLS[3] LEQ N:=LINKLISTSUBAT.S AND N LEQ CONTROLS[4] THEN  00057150
      BEGIN                                                             00057200
        RDISC (AT - 2, IMAGE) ;                                         00057300
                                                                        00057400
        OCTDEC (M := M + 1, IMAGE [14]) ;                               00057500
        IF NOT ZIPPING THEN                                             00058000
        SEQUENCE ;                                                      00058100
        IF PRINTING AND NOT PUNCHING AND CONTROLS [2] = "DOUBLE " THEN  00058110
          WRITE (SAVED [DBL], 15, IMAGE [*])                            00058120
        ELSE                                                            00058130
        WRITE (SAVED, 15, IMAGE [*]) ;                                  00058200
      END ELSE M := M + 1 ;                                             00058300
      IF ZIPPING THEN                                                   00058325
        ZIP WITH SAVED                                                  00058350
      ELSE                                                              00058375
      LOCK (SAVED) ;                                                    00058400
      N := I ;                                                          00058500
    END THERMOFAX ;                                                     00058600
  END VERIFAX ;                                                         00058700
                                                                        00058750
 DEFINE XEROX = VERIFAX (TRUE)# ;                                       00058800
 DEFINE THERMOFAX = VERIFAX (FALSE)# ;                                  00058900
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00059000
 PROCEDURE WDISC ;                                                      00059002
 BEGIN                                                                  00059004
 LABEL EOT ;                                                            00059006
    IF FALSE THEN                                                       00059008
    BEGIN                                                               00059010
 EOT:                                                                   00059012
      AT := LINKLISTSUBD . F ;                                          00059014
      LINKLISTSUBAT . T := AT := LINKLISTSUBD . T ;                     00059016
      LINKLISTSUBAT . F := LINKLISTSUBD . F ;                           00059018
      D := D - 1 ;                                                      00059020
      READ SEEK (DISC [0]) ;                                            00059022
      PREWHERE := -1 ;                                                  00059024
      XEROX ;                                                           00059026
    END ;                                                               00059028
    WHILE N GTR LINKLISTSUBAT.S DO                                      00059030
      AT := LINKLISTSUBAT.T ;                                           00059032
    WHILE N LSS LINKLISTSUBAT.S DO                                      00059034
      AT := LINKLISTSUBAT.F ;                                           00059036
    IF N NEQ LINKLISTSUBAT.S THEN                                       00059038
    BEGIN                                                               00059040
      IF D := D + 1 GTR MAXFILELENGTH THEN                              00059042
        ERROR (0, "FILE TO", " LONG. ") ;                               00059044
      LINKLISTSUBD := 0 & N [SF] & AT [FF] & (LINKLISTSUBAT.T) [TF] ;   00059046
      LINKLISTSUBAT.T := D ;                                            00059048
      AT := LINKLISTSUBD.T ;                                            00059050
      IF AT NEQ 1 THEN                                                  00059052
        INORDER := FALSE ;                                              00059054
      LINKLISTSUBAT.F := D ;                                            00059056
      AT := D ;                                                         00059058
    END ;                                                               00059060
    SEQUENCE ;                                                          00059062
    IF PREWHERE + 1 NEQ PREWHERE := AT - 2 THEN                         00059064
      WRITE (DISC [PREWHERE], 10, IMAGE [*]) [EOT]                      00059066
    ELSE                                                                00059068
      WRITE (DISC, 10, IMAGE [*]) [EOT] ;                               00059070
 END WDISC ;                                                            00059072
                                                                        00059100
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00059200
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00059400
 PROCEDURE INITIALIZE ;                                                 00059420
 BEGIN                                                                  00059440
 DEFINE SAVESPACE = # ;                                                 00059460
   B := TRUE ;                                                          00059480
  TIME1 := TIME (1) ;                                                   00059500
  INC := STATUS (CONTROLS [*]) ;                                        00059600
  STATION := CONTROLS [0] ;                                             00059700
  SEEK (TWXINPUT (STATION)) ;                                           00059800
  USERCODE := CONTROLS [1] ;                                            00059900
                                                                        00059920
  IF USERCODE = -1 THEN                                                 00060000
    USERCODE := "T  B   " & (STATION . [9:4] DIV 10) [12:42:6]          00060100
                & ENTIER (STATION . [9:4] MOD 10) [18:42:6]             00060200
                & (STATION . [14:4] DIV 10) [30:42:6]                   00060300
                & ENTIER(STATION . [14:4] MOD 10) [36:42:6] ;           00060400
  FILESECURITY := TRUE ;                                                00060402
  BEGIN                                                                 00060404
    FILL RECOVER WITH USERCODE, USERCODE ;                              00060406
    SEARCH (RECOVER, IMAGE [*]) ;                                       00060408
    IF IMAGE [0] = -1 THEN                                              00060410
    BEGIN                                                               00060412
      FILL SCRATCH WITH USERCODE, USERCODE ;                            00060414
      WRITE (SCRATCH, *, LISTN) ;                                       00060416
      LOCK (SCRATCH) ;                                                  00060418
    END ELSE                                                            00060420
    IF IMAGE [0] = 7 AND IMAGE [3] = 256 AND IMAGE [4] = 256 THEN       00060422
    BEGIN                                                               00060424
      READ (RECOVER, RSWDM + 1, RSWD [*]) ;                             00060426
      I := STATION ;                                                    00060428
      READ (RECOVER, CONTROLTOP + 1, CONTROLS [*]) ;                    00060430
      STATION := I ;                                                    00060432
      N := CONTROLS [0] ;                                               00060434
      AT := CONTROLS [1] . [35:13] ;                                    00060436
      D := CONTROLS [1] . [22:13] ;                                     00060438
      M := CONTROLS [2] ;                                               00060440
      INC := CONTROLS [3] ;                                             00060442
      BOOL := BOOLEAN (CONTROLS [4]) ;                                  00060444
      IF RSWDL LEQ READONLYLIMIT THEN                                   00060446
        FOR I := D DIV 256 STEP -1 UNTIL 0 DO                           00060448
          READ (RECOVER, 256, LINKLIST [I,*]) ;                         00060450
      CLOSE (RECOVER) ;                                                 00060452
      IF RSWDL GTR READONLYLIMIT THEN                                   00060454
       IF VN = VERSION THEN                                             00060455
        ERROR (0, "HELLO  ", USERCODE) ;                                00060456
      FILL DISC WITH PREFIX, SUFFIX ;                                   00060458
      SEARCH (DISC, IMAGE [*]) ;                                        00060460
      IF IMAGE [0] NEQ 7 THEN                                           00060462
        IF IMAGE [0] LEQ 0 OR RSWDL = 0 THEN                            00060464
        BEGIN                                                           00060466
          SETRSWDL ;                                                    00060468
          INORDER := TRUE ;                                             00060469
          IF VN = VERSION THEN                                          00060470
            ERROR (IF IMAGE [0] = -1 THEN 5 ELSE 9,                     00060471
                 PREFIX, SUFFIX) ;                                      00060472
        END ;                                                           00060474
      IF VN = VERSION THEN ERROR (0, PREFIX, SUFFIX) ;                  00060475
      IF NOT INORDER THEN XEROX ;                                       00060476
      B := TRUE ;                                                       00060477
    END ELSE IF IMAGE [5] NEQ 0                                         00060478
    OR IMAGE [0] NEQ 7 OR IMAGE [3] NEQ 10 OR IMAGE [4] NEQ 30 THEN     00060479
    BEGIN                                                               00060480
      FILESECURITY := FALSE ;                                           00060482
      B := FALSE ;                                                      00060484
    END ;                                                               00060486
  END ;                                                                 00060488
  INC := 100 ;                                                          00060500
  INORDER := TRUE ;                                                     00060600
  SAVEFACTOR := 7 ;                                                     00060660
  PAGELENGTH := 10000 ;                                                 00060670
  VN := VERSION ;                                                       00060680
  SETRSWDL ;                                                            00060700
  FILL RSWD [*] WITH                                                    00060800
    "DITTO  ", "COPY   ", "INLINE ", "ZIP    ", "CHANGE ", "EDIT   ",   00060850
    "SAVE   ", "RESEQ  ", "PUNCH  ", "PRINT  ", "DELETE ", "CLOSE  ",   00060860
    "COMPILE",                                                          00060870
    "PAGE   ", "TIME   ", "SCAN   ", "LISTING", "INC    ", "TAB    ",   00060900
    "PERCENT", "QUICK  ", "LIST   ", "OPEN   ",                         00061000
    "MAIL   ", "TEACH  ", "REMOVE ", "REPLACE", "END    " ;             00061100
  IF NOT B THEN                                                         00061110
    ERROR (9 - 2 | REAL (IMAGE [0] = 7), USERCODE, USERCODE) ;          00061120
  FILL LIBRARY WITH "MAIL % ", "ALL    " ;                              00061121
  SEARCH (LIBRARY, IMAGE [*]) ;                                         00061122
  M := IMAGE [5] ;                                                      00061123
  IF IMAGE [0] GEQ 2 THEN                                               00061124
  BEGIN                                                                 00061125
    FILL SCRATCH WITH "MAIL % ", USERCODE ;                             00061126
    SEARCH (SCRATCH, IMAGE [*]) ;                                       00061127
    IF IMAGE [0] = -1 THEN                                              00061128
    BEGIN N := STATUS (STATION, 3) ;                                    00061129
      FOR N := 0 STEP 1 UNTIL M DO                                      00061130
      BEGIN                                                             00061131
        READ (LIBRARY, 10, IMAGE [*]) ;                                 00061132
        WRITE (SCRATCH, 10, IMAGE [*]) ;                                00061133
      END ;                                                             00061134
      LOCK (SCRATCH) ;  N := STATUS (STATION, 4) ;                      00061135
    END ELSE                                                            00061136
    IF IMAGE [0] GEQ 3 THEN                                             00061137
    BEGIN                                                               00061138
      FILL DISC WITH "MAIL % ", USERCODE ;                              00061139
      SPACE (DISC, IMAGE [5] + 1) ;                                     00061140
      FOR N := 0 STEP 1 UNTIL M DO                                      00061141
      BEGIN                                                             00061142
        READ (LIBRARY, 10, IMAGE [*]) ;                                 00061143
        WRITE (DISC, 10, IMAGE [*]) ;                                   00061144
      END ;                                                             00061145
      CLOSE (DISC) ;                                                    00061146
    END ;                                                               00061147
    CLOSE (LIBRARY) ;                                                   00061148
  END ;                                                                 00061149
  FILL LIBRARY WITH "MAIL % ", USERCODE ;                               00061150
  SEARCH (LIBRARY, IMAGE [*]) ;                                         00061151
  IF IMAGE [0] GEQ 3 THEN                                               00061152
    ERROR (0, " MAIL% ", USERCODE) ;                                    00061160
  ERROR (0, "HELLO  ", USERCODE) ;                                      00061200
 END INITIALIZE ;                                                       00061250
   INITIALIZE ;                                                         00061275
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00061300
 NEXT:                                                                  00061301
 BEGIN                                                                  00061302
 LABEL MAIL,                                                            00061303
   COPY,                                                                00061304
   LISTING,                                                             00061305
   PAGED,                                                               00061306
   TIMING,                                                              00061307
   SCAN,                                                                00061308
   CHANGESTRING,                                                        00061309
   EDIT,                                                                00061310
   PUNCH,                                                               00061311
   PRINT,                                                               00061312
   ZIPIT,                                                               00061313
   PERCENT,                                                             00061314
   QUICKLIST,                                                           00061315
   LISTIT,                                                              00061316
   OPEN,                                                                00061317
   CLOSEIT,                                                             00061318
   INCCHANGE,                                                           00061319
   RESEQ,                                                               00061320
   TAB,                                                                 00061321
   INLINE,                                                              00061322
   SAVEIT,                                                              00061323
   DELETE,                                                              00061324
   COMPILE,                                                             00061325
   DITTO,                                                               00061326
   REMOVE,                                                              00061327
   TEACH,                                                               00061328
   REPLACE,                                                             00061329
   STOP ;                                                               00061330
 SWITCH TYPE := DITTO,                                                  00061331
   COPY,                                                                00061332
   INLINE,                                                              00061333
   ZIPIT,                                                               00061334
   CHANGESTRING,                                                        00061335
   EDIT,                                                                00061336
   SAVEIT,                                                              00061337
   RESEQ,                                                               00061338
   PUNCH,                                                               00061339
   PRINT,                                                               00061340
   DELETE,                                                              00061341
   CLOSEIT,                                                             00061342
   COMPILE,                                                             00061343
   PAGED,                                                               00061344
   TIMING,                                                              00061345
   SCAN,                                                                00061346
   LISTING,                                                             00061347
   INCCHANGE,                                                           00061348
   TAB,                                                                 00061349
   PERCENT,                                                             00061350
   QUICKLIST,                                                           00061351
   LISTIT,                                                              00061352
   OPEN,                                                                00061353
   MAIL,                                                                00061354
   TEACH,                                                               00061355
   REMOVE,                                                              00061356
   REPLACE,                                                             00061357
   STOP ;                                                               00061358
                                                                        00061400
 BEGIN                                                                  00061410
 LABEL                                                                  00061420
   INLINEOK,                                                            00061422
   INLINEESCAPE,                                                        00061424
   VERBEXIT ;                                                           00061430
  ZIPPING := FALSE ;                                                    00061448
  QUICK := FALSE ;                                                      00061450
  SCANTOG := FALSE ;                                                    00061452
  CHANGETOG := FALSE ;                                                  00061454
  EDITTOG := FALSE ;                                                    00061456
  POSTING := FALSE ;                                                    00061458
  IF RSWDL GTR READONLYLIMIT THEN                                       00061460
    N := VERSION ;                                                      00061470
  IF N GEQ INFINITY THEN                                                00061500
  BEGIN                                                                 00061600
    N := INFINITY - 1 ;                                                 00061700
    ERROR (0, "SEQ. OV", "ER-FLOW") ;                                   00061800
  END ELSE IF N LEQ 0 THEN                                              00061900
    N := 1 ;                                                            00061925
 IF NOT MOREINPUT THEN                                                  00061926
 BEGIN                                                                  00061927
  IF RSWDL LEQ READONLYLIMIT THEN                                       00061930
  BEGIN                                                                 00061935
    IF ITSOLD (N) THEN                                                  00061940
      WRITEOLDSEQ                                                       00061945
    ELSE                                                                00061950
      WRITESEQ ;                                                        00061955
  END ELSE                                                              00061960
  WRITESEQ ;                                                            00062000
  IF INLINETOG AND EXTRALFCR THEN WRITELFCR ;                           00062050
  IF TABAMOUNT NEQ 0 THEN                                               00062100
    WRITE (TWXOUT, TABIT, TABAMOUNT) ;                                  00062200
  WRITE (TWXOUT, XON) ;                                                 00062300
 END ;                                                                  00062350
  READIN ;                                                              00062400
  CONTROLS [0] := 0 ;                                                   00062500
  IF INLINETOG THEN                                                     00062600
  BEGIN                                                                 00062700
    IF TRANSLATING THEN                                                 00062710
      TRANSLATE (IMAGE, ZIPPY) ;                                        00062720
    IF NOT INLINEEDIT (IMAGE, RECORD, ZIPPY,                            00062800
          IF M := ABS (M) LEQ 3 THEN M ELSE 0) THEN                     00062825
    BEGIN                                                               00062850
      WRITE (IMAGE [*], 10, ZIPPY [*]) ;                                00062900
      GO TO INLINEOK ;                                                  00062925
    END                                                                 00062950
    ELSE IF NOSTAR THEN                                                 00063000
      ERROR ( 8, "NOT I,R", " OR D. ") ;                                00063100
    GO TO INLINEESCAPE ;                                                00063125
  END ;                                                                 00063200
  IF NOSTAR THEN                                                        00063300
  BEGIN                                                                 00063400
    IF RSWDL = READONLYLIMIT THEN                                       00063410
      ERROR (0, "READ ON", "LY FILE") ;                                 00063420
    IF RSWDL NEQ 0 THEN                                                 00063500
    BEGIN                                                               00063550
      EMPTY1 := TRUE ;                                                  00063560
      I := RWTEACHINDEX ;                                               00063600
      GO TO VERBEXIT ;                                                  00063610
    END ;                                                               00063620
 INLINEOK:                                                              00063625
    IF TRANSLATING THEN                                                 00063650
      TRANSLATE (IMAGE, ZIPPY) ;                                        00063660
                                                                        00063700
    WDISC ;                                                             00063800
                                                                        00063900
                                                                        00064000
                                                                        00064100
    IF INLINETOG THEN                                                   00064200
    BEGIN                                                               00064300
      WRITEAT ;                                                         00064400
      INLINETOG := FALSE ;                                              00064500
    END ;                                                               00064600
    N := INC | ((N + INC) DIV INC) ;                                    00064700
    GO TO NEXT ;                                                        00064800
  END ;                                                                 00064900
 INLINEESCAPE:                                                          00064925
  INLINETOG := FALSE ;                                                  00065000
  STAR (IMAGE, CONTROLS) ;                                              00065001
  EMPTY0 := CONTROLS [0] = "#000000" ;                                  00065002
  EMPTY1 := CONTROLS [1] = "#000000" ;                                  00065004
  EMPTY2 := CONTROLS [2] = "#000000" ;                                  00065006
  EMPTY3 := CONTROLS [3] = "#000000" ;                                  00065008
  EMPTY4 := CONTROLS [4] = "#000000" ;                                  00065010
  NUM0 := NOT (BOOLEAN (CONTROLS [0] . [1 : 1]) OR EMPTY0) ;            00065012
  NUM1 := NOT (BOOLEAN (CONTROLS [1] . [1 : 1]) OR EMPTY1) ;            00065014
  NUM2 := NOT (BOOLEAN (CONTROLS [2] . [1 : 1]) OR EMPTY2) ;            00065016
  NUM3 := NOT (BOOLEAN (CONTROLS [3] . [1 : 1]) OR EMPTY3) ;            00065018
  NUM4 := NOT (BOOLEAN (CONTROLS [4] . [1 : 1]) OR EMPTY4) ;            00065020
  FOR I := 0 STEP 1 UNTIL 4 DO                                          00065100
    IF BOOLEAN (CONTROLS [I] . [1 : 1]) THEN                            00065200
      CONTROLS [I] . [1 : 1] := 0                                       00065225
    ELSE                                                                00065250
    IF CONTROLS [I] GEQ INFINITY THEN                                   00065300
    CONTROLS [I] := INFINITY - 1                                        00065400
  ELSE IF CONTROLS [I] = 0 THEN                                         00065500
    CONTROLS [I] := 1 ;                                                 00065600
  IF NUM0 THEN                                                          00065700
    IF RSWDL GTR READONLYLIMIT THEN                                     00065710
    BEGIN                                                               00065720
      OCTDEC (N, CONTROLS [0]) ;                                        00065730
      ERROR (3, CONTROLS [0], RWTEACH) ;                                00065740
    END ELSE                                                            00065750
    N := CONTROLS [0]                                                   00065800
  ELSE                                                                  00065900
  BEGIN                                                                 00066000
    FOR I := RSWDL STEP 1 UNTIL RSWDM DO                                00066100
      IF CONTROLS [0] = RSWD [I] THEN                                   00066200
      GO TO VERBEXIT ;                                                  00066300
    ERROR (3, CONTROLS [0], RWTEACH) ;                                  00066400
  END ;                                                                 00066500
  IF NOT MOREINPUT THEN                                                 00066550
  IF ITSOLD (N) THEN                                                    00066600
    WRITEAT ;                                                           00066700
  GO TO NEXT ;                                                          00066800
 VERBEXIT:                                                              00066810
 END ;                                                                  00066820
 GO TO TYPE [I + 1] ;                                                   00066830
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00066900
 QUICKLIST:                                                             00066910
  QUICK := TRUE ;                                                       00066920
  GO TO LISTIT ;                                                        00066930
 SCAN:                                                                  00066940
  SCANTOG := TRUE ;                                                     00066950
  GO TO LISTIT ;                                                        00066960
 CHANGESTRING:                                                          00066970
  CHANGETOG := TRUE ;                                                   00066980
  GO TO LISTIT ;                                                        00066982
 EDIT:                                                                  00066984
  IF NOT (NUM1 AND NUM2 AND NUM3) THEN                                  00066986
    ERROR (6, "3 TIMES", 0) ;                                           00066988
  IF NOT ITSOLD (CONTROLS [3]) THEN                                     00066990
    ERROR (0, "MISSING", " FORMAT") ;                                   00066992
  EDITTOG := TRUE ;                                                     00066994
  RDISC (AT - 2, ZIPPY) ;                                               00066996
  IF FILETYPE = 2 THEN SHIFTLEFT (6, ZIPPY, IMAGE) ;                    00066997
  WRITE (TWXOUT, WAIT) ;                                                00066998
 LISTIT:                                                                00067000
 BEGIN                                                                  00067001
 BOOLEAN STREAM PROCEDURE PRESENT (S, R, I, T, N, J, Z) ;               00067002
 VALUE I, J, T, N ;                                                     00067003
  BEGIN                                                                 00067004
 LABEL XIT, LOOP1, LOOP2 ;  LOCAL K, M ;                                00067005
    DI := Z ;  SI := Z ;  DS := 8 LIT "        " ;  DS := 9 WDS ;       00067006
    SI := LOC T ;                                                       00067007
    DI := LOC K ;                                                       00067008
    SI := SI + 6 ;                                                      00067009
    DI := DI + 7 ;                                                      00067010
    DS := 1 CHR ;                                                       00067011
                                                                        00067012
K(2(32(M(TALLY:=M;TALLY:=TALLY+63;M:=TALLY;TALLY:=1;JUMP OUT TO LOOP1) ;00067013
 SI:=S; DI:=R; IF I SC=DC THEN BEGIN TALLY:=1; N(JUMP OUT 4 TO XIT);    00067014
 R:=DI; DI:=Z; DS:=J CHR; Z:=DI; TALLY:=I; TALLY:=TALLY+63 ;            00067015
 M:=TALLY;TALLY:=1;END ELSE BEGIN DI:=DI-I; N(DI:=DI+1; R:=DI;          00067016
 JUMP OUT TO LOOP1) ; R:=DI; SI:=R; DI:=Z; DS:=CHR;                     00067017
 R:=SI; Z:=DI; END ; LOOP1:)));                                         00067018
 T(M(TALLY:=M; TALLY:=TALLY+63; M:=TALLY; TALLY:=1;JUMP OUT TO LOOP2) ; 00067019
 SI:=S; DI:=R; IF I SC=DC THEN BEGIN TALLY:=1; N(JUMP OUT 2 TO XIT) ;   00067020
 R:=DI; DI:=Z; DS:=J CHR; Z:=DI; TALLY:=I; TALLY:=TALLY+63;             00067021
 M:=TALLY;TALLY:=1; END ELSE BEGIN DI:=DI-I; N(DI:=DI+1; R:=DI;         00067022
 JUMP OUT TO LOOP2) ; R:=DI; SI:=R; DI:=Z; DS:=CHR;                     00067023
 R:=SI; Z:=DI; END; LOOP2:) ;                                           00067024
                                                                        00067025
                                                                        00067026
                                                                        00067027
                                                                        00067028
 XIT:                                                                   00067029
    PRESENT := TALLY ;                                                  00067030
 END PRESENT ;                                                          00067031
 INTEGER STREAM PROCEDURE ISOLATE (S, D, N) ;  VALUE N ;                00067032
  BEGIN                                                                 00067033
 LOCAL STOPCHR ;                                                        00067034
 LABEL OK, XIT ;                                                        00067035
    SI := S ;                                                           00067036
    N (DI := LOC STOPCHR ;                                              00067037
       63 (SI := SI + 1 ;                                               00067038
        IF SC = """ THEN                                                00067039
        BEGIN                                                           00067040
          DS := 1 CHR ;                                                 00067041
          JUMP OUT TO OK ;                                              00067042
        END ;                                                           00067043
        IF SC = "(" THEN                                                00067044
        BEGIN                                                           00067045
          DS := 1 LIT ")" ;                                             00067046
          SI := SI + 1 ;                                                00067047
          JUMP OUT TO OK ;                                              00067048
        END ;                                                           00067049
        IF SC = "[" THEN                                                00067050
        BEGIN                                                           00067051
          DS := 1 LIT "]" ;                                             00067052
          SI := SI + 1 ;                                                00067053
          JUMP OUT TO OK ;                                              00067054
        END ; IF SC="." THEN JUMP OUT; IF SC=";" THEN JUMP OUT) ;       00067055
    JUMP OUT TO XIT ;                                                   00067056
 OK:                                                                    00067057
    63 (DI := LOC STOPCHR ;                                             00067058
        IF SC = DC THEN                                                 00067059
          JUMP OUT ;                                                    00067060
        SI := SI - 1 ;                                                  00067061
        DI := D ;                                                       00067062
        DS := 1 CHR ;                                                   00067063
        D := DI ;                                                       00067064
        TALLY := TALLY + 1) ;  N := TALLY ; TALLY := 0) ;  TALLY := N ; 00067065
 XIT:                                                                   00067066
    ISOLATE := TALLY ;                                                  00067067
 END ISOLATE ;                                                          00067068
 STREAM PROCEDURE EDITS (F, S, D, N) ;  VALUE N ;                       00067069
 BEGIN                                                                  00067070
    DI := D ;  DS := 8 LIT " " ;  SI := D ;  DS := 9 WDS ;  DI := D ;   00067071
    2 (N  (SI := F ;                                                    00067072
      IF SC = "@" THEN                                                  00067073
      BEGIN                                                             00067074
        SI := SI + 1 ;  F := SI ;  SI := S ;  DS := CHR ;  S := SI ;    00067075
      END ELSE                                                          00067076
      IF SC = "#" THEN                                                  00067077
      BEGIN                                                             00067078
        SI := SI + 1 ;  F := SI ;  SI := S ;  SI := SI + 1 ;  S := SI ; 00067079
      END ELSE                                                          00067080
      BEGIN                                                             00067081
        DS := CHR ;  F := SI ;                                          00067082
      END)) ;                                                           00067083
 END EDITS ;                                                            00067084
  IF SCANTOG OR CHANGETOG THEN                                          00067085
  BEGIN                                                                 00067086
    IF TRANSLATING THEN                                                 00067087
      TRANSLATE (IMAGE, ZIPPY) ;                                        00067088
    IF I := ISOLATE (IMAGE, STRING, 1) = 0 THEN                         00067089
      I := STRINGI                                                      00067090
    ELSE                                                                00067091
      STRINGI := I ;                                                    00067092
    IF I = 0 THEN                                                       00067093
      ERROR (0, "MISSING", " STRING") ;                                 00067094
    IF CHANGETOG THEN BEGIN STRINGJ:=ISOLATE(IMAGE,STRING,2) ;          00067095
     IF STRINGJ=0 THEN ERROR (0, "MISSING", " STRING") ;                00067096
     IF 72 DIV STRINGI | STRINGJ GTR 160 THEN ERROR (0, "SIZ DIF",      00067097
       " EXCESS") ; ALREADYSEQ := TRUE ; END ;  WRITE (TWXOUT, WAIT) ;  00067098
  END ;                                                                 00067099
  IF NUM1 AND (NUM2 OR EMPTY2) THEN                                     00067100
  BEGIN                                                                 00067200
    N := CONTROLS [1] ;                                                 00067300
    IF NUM2 THEN                                                        00067400
      M := CONTROLS [2] + 1                                             00067500
    ELSE IF SCANTOG OR EDITTOG THEN                                     00067600
      M := INFINITY                                                     00067610
    ELSE                                                                00067620
      M := N ;                                                          00067630
  END                                                                   00067700
  ELSE IF NOT EMPTY1 THEN                                               00067800
  BEGIN                                                                 00067900
    ADJUSTFORNUMERICIDS ;                                               00067910
    FILL SCRATCH WITH CONTROLS [1], CONTROLS [2] ;                      00068000
    SEARCH (SCRATCH, IMAGE [*]) ;                                       00068100
    IF IMAGE [0] LEQ 0 THEN                                             00068200
      ERROR (9 + 4 | IMAGE [0], CONTROLS [1], CONTROLS [2]) ;           00068300
    BEGIN                                                               00068400
 FILE RO DISK SERIAL (2, IMAGE [3], IMAGE [4]) ;                        00068500
  INTEGER K ;                                                           00068550
 LABEL MORE,                                                            00068600
   EOF ;                                                                00068700
      K := N ;                                                          00068750
      FILL RO WITH IMAGE [1], IMAGE [2] ;                               00068800
      N := 0 ;                                                          00068900
      M := IMAGE [5]  + 1 ;                                             00068910
      IF NUM3 THEN                                                      00068920
      BEGIN                                                             00068930
        SPACE (RO, N := CONTROLS [3] - 1) [EOF] ;                       00068940
        IF NUM4 THEN                                                    00068960
          M := CONTROLS [4] ;                                           00068970
      END ELSE IF NOT EMPTY3 THEN                                       00068980
        POSTING := TRUE ;                                               00068990
 MORE:                                                                  00069000
     B := TRUE ;                                                        00069005
      IF N := N + 1 GTR M THEN                                          00069010
        GO TO EOF ;                                                     00069020
      READ (RO, 10, ZIPPY [*]) [EOF] ;                                  00069100
      IF SCANTOG THEN                                                   00069110
        IF NOT PRESENT (STRING, ZIPPY, STRINGI, 73 - STRINGI,           00069120
                        1, STRINGJ, IMAGE) THEN                         00069121
          GO TO MORE ;                                                  00069130
     IF NOT POSTING THEN                                                00069190
      WRITESEQ ;                                                        00069200
      BLANKOUTSPECIALCHARACTERS (ZIPPY,IMAGE,QUICK) ;                   00069250
      IF ALLBLANK (ZIPPY [7], 0, 16) THEN                               00069300
        IF ALLBLANK (ZIPPY [3], 4, 28) THEN                             00069400
          IF ALLBLANK (ZIPPY [0], 0, 28) THEN                           00069500
            GO TO MORE ;                                                00069525
      IF NOT ALLBLANK (ZIPPY [8], 0, 8) OR POSTING THEN                 00069550
        WRITELFCR ;                                                     00069560
      INSERT (7, "~", IMAGE [4]) ;                                      00069600
      B := FALSE ;                                                      00069620
      FOR I := 0 STEP 1 UNTIL 2 DO                                      00069700
      BEGIN                                                             00069800
        MOVE (ZIPPY [3 | I], IMAGE [1], 4 | I, 0, 28) ;                 00069900
        WRITE (TWXOUT, 5, IMAGE [*]) [: EOF] ;                          00070000
      END ;                                                             00070100
      IF POSTING AND CONTROLS [1] = "MAIL % " THEN                      00070110
      BEGIN                                                             00070120
        SPACE (RO, -1) ;                                                00070130
        INSERT (39, " ", IMAGE [0]) ;                                   00070140
        INSERT (39, " ", IMAGE [5]) ;                                   00070150
        WRITE (RO, 10, IMAGE [*]) ;                                     00070160
      END ;                                                             00070170
      GO TO MORE ;                                                      00070200
 EOF:                                                                   00070300
     IF POSTING AND B AND CONTROLS [1] = "MAIL % " THEN                 00070320
       CLOSE (RO, PURGE) ;                                              00070340
      N := K ;                                                          00070360
    END ;                                                               00070400
    GO TO NEXT ;                                                        00070500
  END                                                                   00070600
  ELSE                                                                  00070700
  BEGIN                                                                 00070800
   IF NOT SCANTOG THEN                                                  00070850
    N := 1 ;                                                            00070900
    M := INFINITY ;                                                     00071000
  END ;                                                                 00071100
  IF RSWDL GTR READONLYLIMIT THEN                                       00071200
    ERROR (5, " FOR   ", CONTROLS [0]) ;                                00071300
  B := ITSOLD (N) ;                                                     00071400
  IF LINKLISTSUBAT.S LEQ M THEN                                         00071500
    IF AT GEQ 2 THEN                                                    00071600
    DO                                                                  00071700
  BEGIN                                                                 00071800
    IF SCANTOG OR CHANGETOG THEN                                        00071810
    BEGIN                                                               00071820
      RDISC (AT - 2, RECORD) ;                                          00071830
                                                                        00071835
      IF PRESENT (STRING, RECORD, STRINGI, IF SCANTOG THEN 73 - STRINGI 00071840
                  ELSE 72, SCANTOG, STRINGJ, ZIPPY) THEN                00071841
      BEGIN                                                             00071845
        IF SCANTOG THEN                                                 00071850
        BEGIN                                                           00071851
        WRITEAT ;                                                       00071855
        AT := LINKLISTSUBAT . T ;                                       00071860
        N := LINKLISTSUBAT . S ;                                        00071865
        GO TO NEXT ;                                                    00071870
        END ;                                                           00071871
        N := LINKLISTSUBAT . S ; WRITE (IMAGE [*], 10, ZIPPY [*]) ;     00071872
        SEQUENCE ; SPACE (DISC, -1) ; WRITE (DISC, 10, IMAGE [*]) ;     00071873
      END ;                                                             00071875
    END ELSE                                                            00071880
    IF EDITTOG THEN                                                     00071882
    BEGIN                                                               00071884
      RDISC (AT - 2, RECORD) ;                                          00071886
      IF FILETYPE = 2 THEN SHIFTLEFT (6, RECORD, IMAGE) ;               00071887
      EDITS (ZIPPY, RECORD, IMAGE, IF FILETYPE = 3 THEN 40 ELSE 36) ;   00071888
      N := LINKLISTSUBAT . S ;                                          00071889
      SEQUENCE ;                                                        00071890
      SPACE (DISC, -1) ;                                                00071892
      WRITE (DISC, 10, IMAGE [*]) ;                                     00071894
    END ELSE                                                            00071896
    WRITEAT ;                                                           00071900
    AT := LINKLISTSUBAT.T ;                                             00072000
  END UNTIL LINKLISTSUBAT.S GEQ M ;                                     00072100
  AT := LINKLISTSUBAT.F ;                                               00072200
  N := INC | (LINKLISTSUBAT.S DIV INC + 1) ;                            00072300
  IF SCANTOG THEN                                                       00072310
    ERROR (0, "EOF NO ", "STRING.") ;                                   00072340
  ALREADYSEQ := FALSE ;                                                 00072350
  GO TO NEXT ;                                                          00072400
 END ;                                                                  00072450
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00072500
 MAIL:                                                                  00072520
  IF POSTMAN THEN                                                       00072530
  BEGIN                                                                 00072540
    POSTING := TRUE ;                                                   00072550
    GO TO QUICKLIST ;                                                   00072560
  END ;                                                                 00072570
  GO TO NEXT ;                                                          00072580
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00072590
 COPY:                                                                  00072600
  ADJUSTFORNUMERICIDS ;                                                 00072700
  FILL LIBRARY WITH CONTROLS [1], CONTROLS [2] ;                        00072800
  SEARCH (LIBRARY, IMAGE [*]) ;                                         00072900
  IF IMAGE [0] LSS 2 THEN                                               00073000
    ERROR (9 + 4 | IMAGE [0], CONTROLS [1], CONTROLS [2]) ;             00073100
  IF IMAGE [3] NEQ 10 OR IMAGE [4] MOD 30 NEQ 0 THEN                    00073200
    ERROR (7, CONTROLS [1], CONTROLS [2]) ;                             00073300
  IF NUM3 THEN                                                          00073400
  BEGIN                                                                 00073500
    I := CONTROLS [3] - 1 ;                                             00073600
    IF NUM4 THEN                                                        00073700
    BEGIN                                                               00073800
      M := CONTROLS [4] - 1 ;                                           00073900
      IF M GTR IMAGE [5] THEN                                           00074000
        M := IMAGE [5] ;                                                00074100
    END                                                                 00074200
    ELSE M := I ;                                                       00074300
  END                                                                   00074400
  ELSE                                                                  00074500
  BEGIN                                                                 00074600
    I := 0 ;                                                            00074700
    M := IMAGE [5] ;                                                    00074800
  END ;                                                                 00074900
  ALREADYSEQ := TRUE ;                                                  00074950
  SPACE (LIBRARY, I) ;                                                  00075000
   IF M - I GTR 10 THEN                                                 00075050
     WRITE (TWXOUT, WAIT) ;                                             00075060
  FOR I := I STEP 1 UNTIL M DO                                          00075100
  BEGIN                                                                 00075200
    READ (LIBRARY, 10, IMAGE [*]) ;                                     00075300
                                                                        00075350
                                                                        00075400
    WDISC ;                                                             00075450
                                                                        00075500
                                                                        00075600
                                                                        00075700
    N := N + INC ;                                                      00075800
  END ;                                                                 00075900
  ALREADYSEQ := FALSE ;                                                 00075950
  CLOSE (LIBRARY) ;                                                     00076000
  GO TO NEXT ;                                                          00076100
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00076200
 PUNCH:                                                                 00076225
  PUNCHING := TRUE ;                                                    00076250
 PRINT:                                                                 00076300
  ADJUSTFORNUMERICIDS ;                                                 00076310
  PRINTING := TRUE ;                                                    00076400
  I := TIME (1) DIV 60 ;                                                00076450
  THERMOFAX ;                                                           00076500
  PRINTING := FALSE ;                                                   00076600
  PUNCHING := FALSE ;                                                   00076625
  GO TO NEXT ;                                                          00076700
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00076702
 ZIPIT:                                                                 00076704
  IF NOT INORDER THEN XEROX ;                                           00076705
  CONTROLS [0] := N ;                                                   00076706
  AT := M := 0 ;                                                        00076708
  B := TRUE ;                                                           00076710
  WHILE AT := LINKLISTSUBAT . T NEQ 1 DO                                00076712
  BEGIN                                                                 00076714
    RDISC (AT - 2, IMAGE) ;                                             00076716
    READ (IMAGE [*], A1, LISTN) ;                                       00076718
    IF N = 12 THEN                                                      00076720
    BEGIN                                                               00076722
      B := FALSE ;                                                      00076724
      RDISC (M, IMAGE) ;                                                00076726
      IMAGE [9] := AT - 2 ;                                             00076728
      WRITE (DISC [M], 10, IMAGE [*]) ;                                 00076730
      PREWHERE := M - 1 ;                                               00076731
      M := AT - 2 ;                                                     00076732
    END ELSE                                                            00076734
    IF B THEN                                                           00076736
                                                                        00076737
      ERROR (0, "INV FIR", "ST CARD") ;                                 00076738
                                                                        00076739
  END ;                                                                 00076740
  IF N NEQ 12 THEN                                                      00076742
                                                                        00076743
    ERROR (0, "INV LAS", "T CARD.") ;                                   00076744
                                                                        00076745
  RDISC (M, IMAGE) ;                                                    00076746
  IMAGE [9] := M ;                                                      00076748
  WRITE (DISC [M], 10, IMAGE [*]) ;                                     00076750
  IF NOT EMPTY1 THEN                                                    00076752
  BEGIN                                                                 00076754
    PREWHERE := M - 1 ;                                                 00076756
    ZIPPING := TRUE ;                                                   00076758
    ADJUSTFORNUMERICIDS ;                                               00076760
                                                                        00076761
    THERMOFAX ;                                                         00076762
    N := CONTROLS [0] ;                                                 00076763
  END ELSE                                                              00076764
  BEGIN                                                                 00076766
    SETRSWDL ;                                                          00076768
    PREWHERE := -1 ;                                                    00076770
    ZIP WITH DISC ;                                                     00076772
  END ;                                                                 00076774
  GO TO NEXT ;                                                          00076776
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00076800
 CLOSEIT:                                                               00076825
  SETRSWDL ;                                                            00076850
  IF NOT INORDER THEN                                                   00076875
    XEROX ;                                                             00076900
  CLOSE (DISC) ; PREWHERE := -1 ;                                       00076925
  GO TO NEXT ;                                                          00076950
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00076975
 OPEN:                                                                  00077000
 BEGIN                                                                  00077025
 DEFINE SAVESPACE = # ;                                                 00077050
  SETRSWDL ;                                                            00077060
  IF NOT INORDER THEN                                                   00077100
    XEROX ;                                                             00077200
  CLOSE (DISC) ; PREWHERE := -1 ;                                       00077300
  ADJUSTFORNUMERICIDS ;                                                 00077500
  IF CONTROLS [3] = "ALGOL  " THEN                                      00077600
    FILETYPE := 0                                                       00077700
  ELSE IF CONTROLS [3] = "FORTRAN" THEN                                 00077800
    FILETYPE := 1                                                       00077900
  ELSE IF CONTROLS [3] = "COBOL  " THEN                                 00078000
    FILETYPE := 2                                                       00078100
  ELSE IF CONTROLS [3] = "DATA   " THEN                                 00078200
    FILETYPE := 3                                                       00078300
  ELSE ERROR (7, "TYPE:  ", CONTROLS [3]) ;                             00078400
  IF CONTROLS [4] = "NEW    " THEN                                      00078500
  BEGIN                                                                 00078600
    FILL SCRATCH WITH CONTROLS [1], CONTROLS [2] ;                      00078700
    SEARCH (SCRATCH, IMAGE [*]) ;                                       00078800
    IF IMAGE [0] NEQ - 1 THEN                                           00078900
      ERROR (4, CONTROLS [1], CONTROLS [2]) ;                           00079000
    WRITE (SCRATCH, 10, IMAGE [*]) ;                                    00079100
    LOCK (SCRATCH) ;                                                    00079200
    RSWDL := 0 ;                                                        00079210
  END                                                                   00079300
  ELSE                                                                  00079400
  BEGIN                                                                 00079500
    FILL DISC WITH CONTROLS [1], CONTROLS [2] ;                         00079600
    SEARCH (DISC, IMAGE [*]) ;                                          00079700
    IF IMAGE [0] LEQ 0 THEN                                             00079800
      ERROR (9 + 4 | IMAGE [0], CONTROLS [1], CONTROLS [2]) ;           00079900
    IF IMAGE [3] NEQ 10 OR IMAGE [4] MOD 30 NEQ 0 THEN                  00080000
      ERROR (7, CONTROLS [1], CONTROLS [2]) ;                           00080200
    IF IMAGE [6] NEQ 0 THEN                                             00080300
      ERROR (0, "FILE IN", " USE.  ") ;                                 00080400
    RSWDL := READONLYLIMIT | REAL (IMAGE [0] NEQ 7) ;                   00080500
  END ;                                                                 00080600
  PREFIX := CONTROLS [1] ;                                              00080700
  SUFFIX := CONTROLS [2] ;                                              00080800
  IF CONTROLS [4] = "NEW    " THEN                                      00080900
  BEGIN                                                                 00081000
    FILL DISC WITH CONTROLS [1], CONTROLS [2] ;                         00081100
    READ SEEK (DISC [0]) ;                                              00081200
    D := 0 ;                                                            00081300
    LINKLISTSUBD := 1 ;                                                 00081400
    D := 1 ;                                                            00081500
    LINKLISTSUBD := 0 & INFINITY [SF] & 1 [TF] ;                        00081600
    AT := 0 ;                                                           00081700
    N := INC ;                                                          00081800
    GO TO NEXT ;                                                        00081900
  END ;                                                                 00082000
  D := IMAGE [5] + 2 ;                                                  00082100
  IF D GTR MAXFILELENGTH THEN                                           00082200
  BEGIN                                                                 00082210
    SETRSWDL ;                                                          00082220
    ERROR (0, "FILE TO", " LONG. ") ;                                   00082300
  END ;                                                                 00082310
  READ SEEK (DISC [0]) ;                                                00082400
  N := 0 ;                                                              00082500
  FOR AT := 2 STEP 1 UNTIL D DO                                         00082600
    LINKLISTSUBAT := 0 & (N := N + INC) [SF] & (AT - 1) [FF] & (AT + 1) 00082700
    [TF] ;                                                              00082800
  LINKLISTSUBD.T := 1 ;                                                 00082900
  AT := 0 ;                                                             00083000
  LINKLISTSUBAT := 2 ;                                                  00083100
  AT := 1 ;                                                             00083200
  LINKLISTSUBAT := 0 & INFINITY [SF] & D [FF] & 1 [TF] ;                00083300
  AT := 2 ;                                                             00083400
  LINKLISTSUBAT.F := 0 ;                                                00083500
  N := N + INC ;                                                        00083600
  IF CONTROLS [4] = "OLD    " OR FILETYPE = 3 THEN                      00083700
  BEGIN                                                                 00083800
    INORDER := FILETYPE = 3 OR RSWDL = READONLYLIMIT ;                  00083900
    IF RSWDL = READONLYLIMIT THEN                                       00083910
      ERROR (-1, "READ ON", "LY FILE") ;                                00083920
    GO TO NEXT ;                                                        00084000
  END ;                                                                 00084100
  WRITE (TWXOUT, WAIT) ;                                                00084400
  FILL LIBRARY WITH CONTROLS [1], CONTROLS [2] ;                        00084450
  AT := 0 ;                                                             00084500
  M := 0 ;                                                              00084600
  WHILE AT := LINKLISTSUBAT.T NEQ 1 DO                                  00084700
  BEGIN                                                                 00084800
    IF FILETYPE NEQ 2 THEN                                              00084900
      READ (LIBRARY, X72I8, LISTN)                                      00085000
    ELSE READ (LIBRARY, I6, LISTN) ;                                    00085100
    IF N LEQ 0 THEN                                                     00085300
      N := 1                                                            00085400
    ELSE IF N GEQ INFINITY THEN                                         00085500
      N := INFINITY - 1 ;                                               00085600
    IF M GTR M := N THEN                                                00085700
    BEGIN                                                               00085800
      WRITELFCR ;                                                       00085900
      WRITE (TWXOUT, SEQERR, LISTN) ;                                   00086000
    END ;                                                               00086100
    LINKLISTSUBAT.S := N ;                                              00086200
  END ;                                                                 00086300
  CLOSE (LIBRARY) ;                                                     00086350
  N := INC | ((N + INC) DIV INC) ;                                      00086400
  IF RSWDL = READONLYLIMIT THEN                                         00086410
    ERROR (-1, "READ ON", "LY FILE") ;                                  00086420
  GO TO NEXT ;                                                          00086500
 END ;                                                                  00086550
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00086600
 INCCHANGE:                                                             00086700
  IF NOT NUM1 THEN                                                      00086800
    ERROR (6, CONTROLS [0], CONTROLS [1]) ;                             00086900
  INC := CONTROLS [1] ;                                                 00087000
  N := INC | ((N + INC - 1) DIV INC) ;                                  00087100
  IF N = 0 THEN                                                         00087200
    N := INC ;                                                          00087300
  GO TO NEXT ;                                                          00087400
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00087500
 RESEQ:                                                                 00087600
  IF NUM2 THEN                                                          00087602
  BEGIN                                                                 00087604
    IF NOT NUM1 THEN                                                    00087606
      ERROR (0, CONTROLS [1], "INVALID") ;                              00087608
    IF NUM4 THEN                                                        00087610
      INC := CONTROLS [4] ;                                             00087612
    IF NUM3 THEN                                                        00087614
      N := CONTROLS [3] - INC                                           00087616
    ELSE                                                                00087618
      N := CONTROLS [1] - INC ;                                         00087620
    B := ITSOLD (CONTROLS [1]) ;                                        00087622
    M := CONTROLS [2] ;                                                 00087624
    WHILE LINKLISTSUBAT . S LSS M DO                                    00087626
    BEGIN                                                               00087628
      LINKLISTSUBAT . S := N := N + INC ;                               00087630
      AT := LINKLISTSUBAT . T ;                                         00087632
    END ;                                                               00087634
    IF LINKLISTSUBAT . S = M THEN                                       00087636
      LINKLISTSUBAT . S := N := N + INC ;                               00087638
  END ELSE                                                              00087640
  BEGIN                                                                 00087642
  IF NUM1 THEN                                                          00087700
    INC := CONTROLS [1] ;                                               00087800
  N := 0 ;                                                              00087900
  AT := 0 ;                                                             00088000
  WHILE AT := LINKLISTSUBAT.T NEQ 1 DO                                  00088100
    LINKLISTSUBAT.S := N := N + INC ;                                   00088200
  END ;                                                                 00088202
  N := N + INC ;                                                        00088300
  IF FILETYPE NEQ 3 THEN                                                00088400
    INORDER := RSWDL = READONLYLIMIT ;                                  00088500
  GO TO NEXT ;                                                          00088600
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00088700
 TAB:                                                                   00088800
  IF NOT NUM1 THEN                                                      00088900
    TABAMOUNT := 1                                                      00089000
  ELSE IF TABAMOUNT := CONTROLS [1] GTR 28 THEN                         00089100
    TABAMOUNT := 28 ;                                                   00089200
  IF FILETYPE = 2 THEN                                                  00089300
    TABAMOUNT := TABAMOUNT - 6 ;                                        00089400
  IF TABAMOUNT := TABAMOUNT - 1 LSS 0 THEN                              00089500
    TABAMOUNT := 0 ;                                                    00089600
  GO TO NEXT ;                                                          00089700
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00089800
 SAVEIT:                                                                00089900
  IF EMPTY2 THEN                                                        00090000
  BEGIN                                                                 00090100
    IF NUM1 THEN                                                        00090200
      SAVEFACTOR := CONTROLS [1] ;                                      00090250
    IF NOT FILESECURITY THEN                                            00090260
    IF NOT INORDER THEN                                                 00090300
      XEROX                                                             00090400
    ELSE CLOSE (DISC) ; PREWHERE := -1 ;                                00090500
  END                                                                   00090600
  ELSE                                                                  00090700
  BEGIN                                                                 00090800
    ADJUSTFORNUMERICIDS ;                                               00090900
    IF NUM3 THEN                                                        00090910
      SAVEFACTOR := CONTROLS [3] ;                                      00090920
    THERMOFAX ;                                                         00091000
  END ;                                                                 00091100
  GO TO NEXT ;                                                          00091200
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00091300
 DELETE:                                                                00091400
  IF NOT NUM1 THEN                                                      00091500
    ERROR (6, CONTROLS [0], CONTROLS [1]) ;                             00091600
  INORDER := RSWDL = READONLYLIMIT ;                                    00091700
  N := CONTROLS [1] ;                                                   00091800
  IF NOT NUM2 THEN                                                      00091900
    M := N                                                              00092000
  ELSE IF M := CONTROLS [2] LSS N THEN                                  00092100
    M := N ;                                                            00092200
  I := D ;                                                              00092300
  B := ITSOLD (N) ;                                                     00092400
  D := LINKLISTSUBAT.F ;                                                00092500
  IF ITSOLD (M) THEN                                                    00092600
    AT := LINKLISTSUBAT.T ;                                             00092700
  LINKLISTSUBD.T := AT ;                                                00092800
  LINKLISTSUBAT.F := D ;                                                00092900
  N := LINKLISTSUBD.S + INC ;                                           00093000
  D := I ;                                                              00093100
  GO TO NEXT ;                                                          00093200
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00093300
 COMPILE:                                                               00093400
 BEGIN                                                                  00093500
                                                                        00093505
                                                                        00093510
                                                                        00093515
                                                                        00093520
                                                                        00093525
                                                                        00093530
                                                                        00093535
                                                                        00093540
                                                                        00093545
                                                                        00093550
 LABEL AGAIN ;                                                          00093600
                                                                        00093700
                                                                        00093800
    IF EMPTY2 THEN                                                      00093900
      ERROR (2, CONTROLS [1], CONTROLS [2]) ;                           00094000
    ADJUSTFORNUMERICIDS ;                                               00094100
    FILL FEEDBACK WITH "LINE   ", USERCODE ;                            00094110
    FILL SCRATCH WITH CONTROLS [1], CONTROLS [2] ;                      00094120
                                                                        00094200
                                                                        00094210
                                                                        00094220
                                                                        00094230
                                                                        00094240
                                                                        00094250
                                                                        00094260
                                                                        00094270
                                                                        00094300
                                                                        00094305
    IF RSWDL GTR READONLYLIMIT THEN                                     00094310
      ERROR (5, " FOR   ", CONTROLS [0]) ;                              00094320
    IF FILETYPE = 3 AND EMPTY3 THEN                                     00094400
      ERROR (7, PREFIX, SUFFIX) ;                                       00094500
      IF NOT EMPTY3 THEN                                                00094510
      BEGIN                                                             00094520
        FILL LIBRARY WITH CONTROLS [3], "DISK   " ;                     00094530
        SEARCH (LIBRARY, IMAGE [*]) ;                                   00094540
        IF IMAGE [0] LEQ 0 THEN                                         00094550
          ERROR (9 + 4 | IMAGE [0], IMAGE [1], IMAGE [2]) ;             00094560
        I := CONTROLS [3] . [6:6] ;                                     00094570
        M := CONTROLS [3] . [12:36] ;                                   00094580
      END ;                                                             00094590
    SEARCH (FEEDBACK, IMAGE [*]) ;                                      00094600
    IF IMAGE [0] NEQ -1 THEN                                            00094610
      ERROR (4, "LINE   ", USERCODE) ;                                  00094620
    SEARCH (SCRATCH, IMAGE [*]) ;                                       00094700
    IF IMAGE [0] NEQ - 1 THEN                                           00094800
      ERROR (4, CONTROLS [1], CONTROLS [2]) ;                           00094900
    IF NOT INORDER THEN                                                 00095000
      XEROX ;                                                           00095100
    IF EMPTY3 THEN                                                      00095105
    IF FILETYPE = 0 THEN                                                00095110
    BEGIN                                                               00095120
      I := "A" ;                                                        00095130
      M := "LGOL  " ;                                                   00095140
    END ELSE                                                            00095150
    IF FILETYPE = 1 THEN                                                00095160
    BEGIN                                                               00095170
      I := "F" ;                                                        00095180
      M := "ORTRAN" ;                                                   00095190
    END ELSE                                                            00095200
    BEGIN                                                               00095210
      I := "C" ;                                                        00095220
      M := "OBOL  " ;                                                   00095230
    END ;                                                               00095240
    WRITE (ZIPPY [*], ZIPPER, CONTROLS [1].[6 : 6], CONTROLS [1],       00095300
      CONTROLS [2].[6:6], CONTROLS [2], I, M, PREFIX.[6:6],PREFIX,      00095400
      SUFFIX.[6 : 6], SUFFIX, USERCODE.[6 : 6], USERCODE) ;             00095500
    ZIP WITH ZIPPY [*] ;                                                00095600
                                                                        00095700
    CLOSE (DISC) ; PREWHERE := -1 ;                                     00095800
    WRITE (TWXOUT, WAIT) ;                                              00095850
 AGAIN:                                                                 00095900
    SEARCH (SCRATCH, IMAGE [*]) ;                                       00096000
    IF IMAGE [0] NEQ -1 THEN                                            00096100
      ERROR (0, "COMPILE", "D OK.  ") ;                                 00096200
    SEARCH (FEEDBACK, IMAGE [*]) ;                                      00096300
    IF IMAGE [0] NEQ -1 THEN                                            00096400
    BEGIN                                                               00096500
      SEARCH (SCRATCH, IMAGE [*]) ;                                     00096600
      IF IMAGE [0] NEQ -1 THEN                                          00096700
                                                                        00096710
                                                                        00096720
                                                                        00096760
        ERROR (0, "COMPILE", "D OK.  ") ;                               00096800
      CONTROLS [1] := M & I [6:42:6] ;                                  00096900
      CONTROLS [2] := "ERRORS " ;                                       00097000
                                                                        00097010
                                                                        00097020
                                                                        00097030
                                                                        00097040
                                                                        00097050
                                                                        00097060
                                                                        00097070
                                                                        00097080
                                                                        00097085
                                                                        00097090
                                                                        00097095
      FINDSEGRELADDR ;                                                  00097100
    END ELSE                                                            00097200
    BEGIN                                                               00097250
      IF NOT MOREINPUT THEN                                             00097275
      READ (TWXINPUT (0, 5), 1, IMAGE [*]) [AGAIN] ;                    00097300
      SETRSWDL ;                                                        00097350
    END ;                                                               00097375
 END COMPILE ;                                                          00097400
                                                                        00097500
                                                                        00097600
                                                                        00097700
                                                                        00097750
                                                                        00097800
                                                                        00097900
                                                                        00098000
                                                                        00098100
                                                                        00098200
                                                                        00098300
                                                                        00098400
                                                                        00098500
                                                                        00098600
                                                                        00098700
                                                                        00098800
                                                                        00098900
                                                                        00099000
                                                                        00099010
                                                                        00099020
                                                                        00099030
                                                                        00099040
                                                                        00099050
                                                                        00099060
                                                                        00099100
                                                                        00099200
                                                                        00099250
                                                                        00099260
                                                                        00099270
                                                                        00099280
                                                                        00099300
                                                                        00099400
                                                                        00099500
                                                                        00099600
                                                                        00099700
                                                                        00099800
                                                                        00099900
                                                                        00100000
                                                                        00100100
                                                                        00100200
                                                                        00100300
                                                                        00100400
                                                                        00100500
                                                                        00100600
                                                                        00100700
                                                                        00100750
                                                                        00100800
  GO TO NEXT ;                                                          00100810
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00100900
 DITTO:                                                                 00101000
  CLOSE (DISC) ;                                                        00101050
  IF NOT NUM1 THEN                                                      00101100
    ERROR (6, CONTROLS [0], CONTROLS [1]) ;                             00101200
  FILL LIBRARY WITH PREFIX, SUFFIX, *, *, *, 10 ;                       00101250
  I := CONTROLS [1] ;                                                   00101300
  IF NUM2 THEN                                                          00101400
    M := CONTROLS [2] + 1                                               00101500
  ELSE M := I ;                                                         00101600
  ALREADYSEQ := TRUE ;                                                  00101650
  B := ITSOLD (I) ;                                                     00101700
  CONTROLS [1] := D ;                                                   00101750
  IF LINKLISTSUBAT.S LEQ M THEN                                         00101800
    DO                                                                  00101900
  BEGIN                                                                 00102000
    READ (LIBRARY [AT - 2], 10, IMAGE [*]) ;                            00102100
    READ SEEK (LIBRARY [LINKLISTSUBAT . T - 2]) ;                       00102150
    I := AT ;                                                           00102200
                                                                        00102300
    WDISC ;                                                             00102400
                                                                        00102500
                                                                        00102600
                                                                        00102700
    AT := I ;                                                           00102800
                                                                        00102850
                                                                        00102900
    N := N + INC ;                                                      00103000
    AT := LINKLISTSUBAT.T ;                                             00103100
  END UNTIL LINKLISTSUBAT.S GEQ M OR AT GTR CONTROLS [1] ;              00103200
  ALREADYSEQ := FALSE ;                                                 00103250
  N := INC | ((N + INC - 1) DIV INC) ;                                  00103300
  CLOSE (LIBRARY) ;                                                     00103325
  FILL LIBRARY WITH *, *, *,*, *, 12 ;                                  00103350
  GO TO NEXT ;                                                          00103400
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00103500
 REMOVE:                                                                00103600
  ADJUSTFORNUMERICIDS ;                                                 00103700
  IF CONTROLS [1] = PREFIX THEN                                         00103800
    IF CONTROLS [2] = SUFFIX AND RSWDL = 0 THEN                         00103900
  BEGIN                                                                 00104000
    READ (DISC [0], 1, CONTROLS [*]) ;                                  00104050
    CLOSE (DISC, PURGE) ; PREWHERE := -1 ;                              00104100
    SETRSWDL ;                                                          00104200
    INORDER := TRUE ;                                                   00104300
    GO TO NEXT ;                                                        00104400
  END ;                                                                 00104500
  FILL FEEDBACK WITH CONTROLS [1], CONTROLS [2] ;                       00104600
  SEARCH (FEEDBACK, IMAGE [*]) ;                                        00104700
  IF IMAGE [0] LEQ 0 THEN                                               00104800
    ERROR (5, CONTROLS [1], CONTROLS [2]) ;                             00104900
  IF IMAGE [0] LEQ 3 THEN                                               00105000
    ERROR (9, CONTROLS [1], CONTROLS [2]) ;                             00105100
  IF IMAGE [6] NEQ 0 THEN                                               00105200
    ERROR (0, "FILE IN", " USE.  ") ;                                   00105300
  READ (FEEDBACK, 10, IMAGE [*]) ;                                      00105400
  CLOSE (FEEDBACK, PURGE) ;                                             00105500
  GO TO NEXT ;                                                          00105600
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00105700
 LISTING:                                                               00105710
  FINDSEGRELADDR ;                                                      00105720
  GO TO NEXT ;                                                          00105730
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00105740
 INLINE:                                                                00105800
  IF NUM1 THEN                                                          00105900
  BEGIN                                                                 00106000
    N := CONTROLS [1] ;                                                 00106100
    IF NOT ITSOLD (N) THEN                                              00106200
      ERROR ( 8, "- NO SU", "CH LINE") ;                                00106300
    IF NOT MOREINPUT THEN                                               00106350
    WRITEAT ;                                                           00106400
    I := 2 ;                                                            00106450
  END                                                                   00106500
  ELSE                                                                  00106600
  BEGIN                                                                 00106700
    B := ITSOLD (N) ;                                                   00106800
    AT := LINKLISTSUBAT.F ;                                             00106900
    N := LINKLISTSUBAT.S ;                                              00107000
    I := 1 ;                                                            00107050
  END ;                                                                 00107100
  RDISC (AT - 2, RECORD) ;                                              00107200
  INLINETOG := TRUE ;                                                   00107300
  IF FILETYPE = 2 THEN                                                  00107400
    SHIFTLEFT (6, RECORD, IMAGE) ;                                      00107500
  IF I := CONTROLS [I] . [6:6] = "I" THEN M := 1                        00107510
  ELSE IF I = "D" THEN M := 2                                           00107520
  ELSE IF I = "R" THEN M := 3                                           00107530
  ELSE M := 0 ;                                                         00107540
  GO TO NEXT ;                                                          00107600
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00107700
 PAGED:                                                                 00107710
  IF NUM2 THEN                                                          00107720
    LINECOUNT := (CONTROLS [2] - 1) | PAGELENGTH ;                      00107730
  IF NUM1 THEN                                                          00107740
    LINECOUNT := LINECOUNT DIV PAGELENGTH | PAGELENGTH := CONTROLS [1]  00107750
  ELSE                                                                  00107760
    LINECOUNT := (LINECOUNT DIV PAGELENGTH + 1) | PAGELENGTH - 1 ;      00107770
  GO TO NEXT ;                                                          00107780
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00107790
 TEACH:                                                                 00107800
  IF NOT EMPTY1 THEN                                                    00107802
  BEGIN                                                                 00107804
    M := -1 ;                                                           00107806
    FOR I := RSWDM STEP -1 UNTIL 0 DO                                   00107808
      IF CONTROLS [1] = RSWD [I] THEN                                   00107810
      BEGIN                                                             00107812
        M := I ;                                                        00107814
        I := -1 ;                                                       00107816
      END ;                                                             00107818
    IF M = -1 THEN                                                      00107820
      ERROR (3, CONTROLS [1], RWTEACH) ;                                00107822
    OCTDEC (VERSION, CONTROLS [2]) ;                                    00107824
    FILL LIBRARY WITH CONTROLS [1] := "TEACHER", CONTROLS [2] ;         00107826
    SEARCH (LIBRARY, IMAGE [*]) ;                                       00107828
    IF IMAGE [0] LEQ 0 THEN                                             00107830
      ERROR (5, CONTROLS [1], CONTROLS [2]) ;                           00107832
    SPACE (LIBRARY, M) ;                                                00107834
    I := N ;                                                            00107836
    READ (LIBRARY, I6, LISTN) ;                                         00107838
    CLOSE (LIBRARY) ;                                                   00107840
    CONTROLS [3] := N DIV 1000 ;                                        00107842
    NUM3 := TRUE ;                                                      00107843
    CONTROLS [4] := N MOD 1000 ;                                        00107844
    NUM4 := TRUE ;                                                      00107845
    N := I ;                                                            00107846
    POSTING := TRUE ;                                                   00107847
    GO TO QUICKLIST ;                                                   00107848
  END ;                                                                 00107850
  ANOTHERLINE ;                                                         00107890
  WRITE (TWXOUT, TEACH1) ;                                              00107900
  FOR I := RSWDL STEP 1 UNTIL RSWDM DO                                  00108000
  BEGIN                                                                 00108010
    IF (I - RSWDL) MOD 6 = 0 THEN                                       00108020
      WRITELFCR ;                                                       00108030
    WRITE (TWXOUT, TEACH2, TEACHOUT) [: NEXT] ;                         00108100
  END ;                                                                 00108110
  WRITELFCR ;                                                           00108120
  WRITE (TWXOUT, TEACH3) ;                                              00108130
  WRITE (TWXOUT, TEACH4) ;                                              00108140
  WRITE (TWXOUT, TEACH5) ;                                              00108150
  GO TO NEXT ;                                                          00108200
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00108210
 PERCENT:                                                               00108220
  TRANSLATING := CONTROLS [1] = "ON     " ;                             00108230
  IF NOT TRANSLATING THEN                                               00108240
    IF CONTROLS [1] NEQ "OFF    " THEN                                  00108250
      ERROR (2, CONTROLS [1], " 0N-OFF") ;                              00108260
  GO TO NEXT ;                                                          00108270
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00108300
 REPLACE:                                                               00108305
   FOR I := 0 STEP 1 UNTIL RSWDM DO                                     00108310
     IF RSWD [I] = CONTROLS [2] THEN                                    00108315
        ERROR (0, "DUPL.  ", CONTROLS [2]) ;                            00108320
   FOR I := 0 STEP 1 UNTIL RSWDM DO                                     00108325
     IF RSWD [I] = CONTROLS [1] THEN                                    00108330
     BEGIN                                                              00108335
       RSWD [I] := CONTROLS [2] ;                                       00108340
       GO TO NEXT ;                                                     00108345
     END ;                                                              00108350
   ERROR (0, "NO VERB", CONTROLS [1]) ;                                 00108355
 % * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *00108360
 STOP:                                                                  00108400
  B := FALSE ;                                                          00108402
  IF FALSE THEN                                                         00108404
 TIMING:                                                                00108406
    B := TRUE ;                                                         00108408
 END ;                                                                  00108410
  IF B THEN                                                             00108412
    GO TO TIMING ;                                                      00108414
 STOP:                                                                  00108420
  RSWDL := RSWDM ;                                                      00108450
  IF NOT INORDER THEN                                                   00108500
    XEROX ;                                                             00108600
  B := FALSE ;                                                          00108605
  IF FILESECURITY THEN                                                  00108610
  BEGIN                                                                 00108620
    READ (RECOVER, *, LISTN) ;                                          00108630
    CLOSE (RECOVER, PURGE) ;                                            00108640
  END ;                                                                 00108650
  IF EMPTY1 THEN                                                        00108652
  BEGIN                                                                 00108654
    IF FALSE THEN                                                       00108656
 TIMING:                                                                00108660
      B := TRUE ;                                                       00108662
  ANOTHERLINE ;                                                         00108670
  I := TIME (2) ;                                                       00108700
  M := "PROC. " ;                                                       00108800
  WRITE (TWXOUT, EOJ, TIMEOUT) ;                                        00108900
  ANOTHERLINE ;                                                         00108950
  I := TIME (3) ;                                                       00109000
  M := "I / O." ;                                                       00109100
  WRITE (TWXOUT, EOJ, TIMEOUT) ;                                        00109200
  ANOTHERLINE ;                                                         00109250
  I := TIME (1) - TIME1 ;                                               00109300
  M := "TOTAL." ;                                                       00109400
  WRITE (TWXOUT, EOJ, TIMEOUT) ;                                        00109500
  END ;                                                                 00109525
  IF B THEN                                                             00109550
    GO TO NEXT ;                                                        00109560
END.                                                                    00109600
                LAST CARD ON CRDIMG TAPE                                99999999
