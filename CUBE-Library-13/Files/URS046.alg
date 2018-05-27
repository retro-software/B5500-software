 LABEL  0000000000XXXXXX0010000001
$ CARD
$ CARD LIST
BEGIN                                                                   00000000
COMMENT                                                                 00000100
                                                                        00000200
              A L G O L   C R O S S   R E F E R E N C E                 00000300
                                                                        00000400
              PROGRAMMED BY HARRY N. BROOMALL  DECEMBER 1963            00000500
                                                                        00000600
              SYSTEMS IMPROVEMENT GROUP                                 00000700
              AUTOMATIC PROGRAMMING - PASADENA                          00000800
                                                                        00000900
;                                                                       00001000
FILE OUT TAPE2"X-CROSS"(2,15);                                          00001200
FILE IN CARD "0CARD  " (2,10);                                          00001300
FILE IN TAPE10 "0CRDIMG" (2,10);                                        00001305
FILE IN TAPE15 "0CRDIMG" (2,15);                                        00001310
FILE IN TAPEBLK "0CRDIMG" (2,56,10);                                    00001312
    FILE IN NAMES(1,10);   COMMENT CARD READ BUFFER;                    00001400
INTEGER SWF; COMMENT SWITCH FILE INDICATOR;                             00001320
SWITCH FILE TAPE ~ CARD,TAPE10,TAPE15,TAPEBLK;                          00001315
    FILE OUT PRINT 1 (2,15);    COMMENT  PRINTER OUTPUT BUFFER;         00001500
BOOLEAN  LISTOG,SORTOG,NEWTOG,DOLLAR,DILLY,LASTUSED,EOFTOG;             00001600
BOOLEAN NULLTOG;                                                        00001700
INTEGER NEWSEQ;                                                         00001800
ALPHA BLANK;                                                            00001900
INTEGER IDENTCT; COMMENT NUMBER OF IDENTIFIERS TO X-REF;                00002100
INTEGER CLASS; COMMENT 0 - NOT FOUND,1 - FOUND, 3,4,5,6 RESERVED        00002200
                       7- NUMBER, 8- SPECHR;                            00002300
DEFINE COMMENTV = 2 #,                                                  00002400
       BEGINV  = 3 #,                                                   00002500
       ENDV   = 4 #,                                                    00002600
       ELSEV  = 5 #,                                                    00002700
       UNTILV = 6# ;                                                    00002800
INTEGER WD; COMMENT NO OF WORDS OF INPUT;                               00002900
INTEGER W2; COMMENT W + 2;                                              00003000
INTEGER SEQCT; COMMENT DUMMY SEQ NO IN CASE OF BLANK FIELD;             00003100
ALPHA    Q   ;   COMMENT TEMPORARY ACCUM[1];                            00003200
BOOLEAN PRINTOG;                                                        00003300
BOOLEAN MAYBE;                                                          00003310
BOOLEAN ENDTOG;                                                         00003400
BOOLEAN BUILDTOG;                                                       00003405
BOOLEAN CARDI; COMMENT CARD IMAGE FORMAT;                               00003500
INTEGER INDENT; COMMENT INDENT FOR VARIABLE LENGTH IDENTIFIERS;         00003600
INTEGER W; COMMENT W IS THE SIZE OF INPUT RECORD IN WORDS;              00003700
FORMAT OUT TITLE(X40,"CROSS-REFERENCE FOR ALGOL PROGRAMS",56" ");       00003800
REAL SEQ1,SEQ; COMMENT SEQ1 TO ILLIMINATE FLAG DATAOF SEQUENCE NO;      00003900
INTEGER I; COMMENT USED TO KEEP TRACK OF LINE OF SEQ NO;                00004000
REAL    P;      COMMENT INFO POINTER;                                   00004100
INTEGER PTEMP; COMMENT TEMPORARY P;                                     00004200
REAL L; COMMENT LINK OR ELBAT WORD USED IN SEARCH OR BUILDXREF;         00004300
INTEGER NEXT,PRES;    COMMENT USED TO SEARCH THRU LINK LIST;            00004400
INTEGER RESULT;ALPHA CHR;                                               00004500
INTEGER WORDS; COMMENT NUMBER OF LOCATIONS TAKEN UP BY 5-8-8;           00004600
INTEGER COUNT   ; COMMENT NUMBER OF CHARACTERS IN IDENTIFIER;           00004700
INTEGER SCRAM;     COMMENT SCRAMBLE INDEX;                              00004800
INTEGER LCR,NCR;                                                        00004900
ARRAY STKH[0:125];   COMMENT STACKHEAD;                                 00005000
ARRAY INFO[0:1022];  COMMENT TALBLE OF LINKED IDENTIFIERS;              00005100
SAVE ARRAY GLOBE[0:14]; COMMENT COMMON READ IN AREA;                    00005200
INTEGER ARRAY XREF[0:255,0:255];      COMMENT 32,768;                   00005400
ARRAY ACCUM[0:9]; COMMENT IDENTIFIERS ARE BUILT HERE;                   00005500
STREAM PROCEDURE RESERVED(PRINT,ACCUM,COUNT);VALUE COUNT;               00005501
BEGIN                                                                   00005502
    DI ~ PRINT; DS~18 LIT "R E S E R V E D -  ";                        00005503
    SI ~ ACCUM ; SI ~ SI + 3 ; DS ~ COUNT CHR                           00005504
END;                                                                    00005505
STREAM PROCEDURE DUPLICATE(PRINT,ACCUM,COUNT);VALUE COUNT;              00005506
BEGIN                                                                   00005507
    DI ~ PRINT; DS~30 LIT"D U P L I C A T E   N A M E - ";              00005508
    SI ~ ACCUM; SI ~ SI+ 3; DS ~ COUNT CHR;                             00005509
END;                                                                    00005510
STREAM PROCEDURE TOOMANY(PRINT,ACCUM,COUNT);VALUE COUNT ;               00005511
BEGIN DI ~ PRINT; DS ~38 LIT"**** TOO MANY NAMES-LAST ACCEPTED WAS " ;  00005512
       SI ~ ACCUM; SI ~ SI + 3; DS ~ COUNT CHR;                         00005513
END;
STREAM PROCEDURE ZOT(RET,CHT,LCR); VALUE CHT,LCR;                       00005600
    BEGIN SI ~LCR; DI ~ RET ; DS ~ 1 WDS;                               00005700
      DI ~ LCR; SI ~ LOC LCR; SI ~ SI - 1; DS ~ CHR; END ZOT;           00005800
REAL STREAM PROCEDURE MKABS(A); BEGIN DI ~A;MKABS~DI END MKABS;         00005900
    STREAM PROCEDURE MOVEWORDS(FROM,TOWARDS,NWDCOUNT);VALUE NWDCOUNT;   00006000
    BEGIN SI ~FROM; DI ~ TOWARDS; DS ~ NWDCOUNT WDS END MOVEWORDS;      00006100
STREAM PROCEDURE CLEARB(A);                                             00006200
    BEGIN  DI ~ A; DS ~ 8 LIT "  "; SI ~ A; DS~ 14 WDS; END CLEARB;     00006300
STREAM PROCEDURE SEQM(F,T);                                             00006320
    BEGIN SI ~F;SI ~ SI + 8; DI ~ T; DS ~ WDS; END;                     00006330
  STREAM PROCEDURE MOVESEQ(BLANK,F,T);                                  00006400
    BEGIN SI ~ F; DI ~ T; DS ~ 1 WDS;                                   00006500
          SI ~ BLANK; DI ~ F; DS ~ WDS;                                 00006600
         END MOVESEQ;                                                   00006700
STREAM PROCEDURE CLEARSEQ(SEQ1); BEGIN DI ~SEQ1;DS~16 LIT"00"; END;     00006800
PROCEDURE READATAPE;                                                    00006900
    BEGIN LABEL EOF, XIT ,PAR ;                                         00007000
         DEFINE DONT = WHILE FALSE DO # ;                               00007010
BOOLEAN STREAM PROCEDURE BLKTEST(BLANK,NEWSEQ,SEQFIELD);                00007100
         BEGIN   SI ~ BLANK; DI ~ SEQFIELD;                             00007200
                 IF 8 SC = DC THEN                                      00007300
                   BEGIN SI ~ NEWSEQ ; DI ~ SEQFIELD ;DS ~ 2 LIT"##";   00007400
                          DS ~ 6 DEC ; TALLY ~ 1;                       00007500
                   END ;                                                00007600
                  BLKTEST ~ TALLY;                                      00007700
         END BLKTEST;                                                   00007800
    IF LASTUSED THEN                                                    00007900
         BEGIN IF NEWTOG THEN                                           00008000
              BEGIN IF PRINTOG AND LISTOG THEN                          00008100
                   BEGIN MOVEWORDS(PRINT(0),TAPE2(0),15);               00008200
                         RELEASE(TAPE2)                                 00008300
                   END;                                                 00008400
                   WRITE(TAPE2); COMMENT GIVES SPACING AND CLEARS;      00008500
              END;                                                      00008600
                IF LISTOG THEN                                          00008700
              BEGIN IF PRINTOG THEN                                     00008800
                   BEGIN RELEASE(PRINT) END;                            00008900
                   WRITE(PRINT)                                         00009000
              END;                                                      00009100
PAR:     READ (TAPE[SWF],WD,GLOBE[*] ) [EOF:PAR] ;                      00009200
       CLEARB(PRINT(0));                                                00009300
         CLEARSEQ(SEQ1);                                                00009400
          IF BLKTEST(BLANK,NEWSEQ,GLOBE[W])THEN NEWSEQ ~ NEWSEQ + 1 ;   00009500
     IF LISTOG OR NEWTOG                                                00009600
                        THEN MOVEWORDS(GLOBE[0],PRINT(0),WD);           00009700
    IF CARDI   THEN IF LISTOG OR NEWTOG THEN                            00009800
               MOVESEQ(BLANK,PRINT(9),PRINT(10));                       00009900
              PRINTOG ~ TRUE;                                           00010000
         END ELSE READ(NAMES,10,GLOBE[*])[EOF] ;                        00010100
    GO TO XIT ;                                                         00010200
EOF: EOFTOG ~ TRUE;                                                     00010300
DONT BEGIN                                                              00010310
XIT: LCR ~ MKABS(GLOBE[W]);NCR~LCR-W; ZOT(SEQ,"%",LCR);                 00010400
    END;                                                                00010410
END READATAPE;                                                          00010500
REAL STREAM PROCEDURE EXAMINE(NCR); VALUE NCR;                          00010600
  BEGIN SI ~ NCR; DI ~ LOC EXAMINE; DI ~ DI + 7; DS ~ CHR END EXAMINE;  00010700
INTEGER     XC,XR;                                                      00010800
REAL XP;                                                                00010900
REAL TIMEX;                                                             00010905
INTEGER XCOUNT;    COMMENT NO OF X- REF;                                00010910
INTEGER INFOL;     COMMENT REMAINING SPACE IN INFO;                     00010920
DEFINE   LINKX = [31:9] # ,                                             00011100
         LINKY = [40:8] # ,                                             00011200
         LINKI = [38:10] # ,                                            00011300
         LINKC =[21:9] #,                                               00011400
         LINKR =[30:8]#,                                                00011500
         LINKCR=[21:17]#;                                               00011600
PROCEDURE BUILDXREF;                                                    00011700
  BEGIN LABEL INSERT,XIT,PERMUTE;                                       00011800
   DEFINE LINKAUX  = [3:17] # ,                                         00011900
          LINKAUXC = [3:9 ] # ,                                         00012000
          LINKAUXR = [12:8] # ;                                         00012100
    IF L.LINKCR = 0 THEN BEGIN  INFO[PRES].LINKCR ~ XP;                 00012200
                                INFO[PRES].LINKAUX ~ XP;                00012300
INSERT: XP ~XP + 1;                                                     00012400
        SEQM(SEQ1,XREF[XP.LINKX,XP.LINKY]);                             00012500
    XP ~ XP + 1 ;                                                       00012600
    GO TO XIT END;                                                      00012700
PERMUTE: XREF[L.LINKAUXC,L.LINKAUXR].LINKCR ~XP;                        00012800
    INFO [PRES].LINKAUX~ XP;                                            00012900
    GO TO INSERT ;                                                      00013000
XIT:                                                                    00013100
  END BUILDXREF;                                                        00013200
STREAM PROCEDURE MOVECHRS(A,A1,B,B1,COUNT);VALUE A1,B1,COUNT;           00013300
    BEGIN SI~A; SI ~ SI + A1; DI ~ B; DI ~ DI + B1 ;                    00013400
          DS ~ COUNT CHR END MOVECHRS;                                  00013500
PROCEDURE PRINTID(TRU); VALUE TRU; BOOLEAN TRU;                         00013800
   BEGIN                                                                00013900
    MOVECHRS (INFO[P+1],3,PRINT(0),0,COUNT);                            00014000
        IF TRU THEN BEGIN RELEASE(PRINT);                               00014100
         IF NEWTOG THEN                                                 00014200
              BEGIN MOVEWORDS(PRINT(0),TAPE2(0),15);                    00014300
                      RELEASE(TAPE2);                                   00014400
              END;                                                      00014500
                 CLEARB(PRINT(0));                                      00014600
          END;                                                          00014700
     END PRINTID;                                                       00014800
BOOLEAN STREAM PROCEDURE EQUAL(A,B,COUNT); VALUE COUNT;                 00014900
    BEGIN   SI ~ A; DI ~ B; TALLY ~ 1;IF COUNT SC = DC                  00015000
              THEN EQUAL ~ TALLY; END EQUAL;                            00015100
PROCEDURE PRINTSEQ;                                                     00015200
  BEGIN                                                                 00015300
   STREAM PROCEDURE FORM(A,B,I,INDENT); VALUE I,INDENT;                 00015400
      BEGIN SI ~A;DI~B; DI~DI+2 ; COMMENT COL 11;                       00015500
        INDENT(SI~SI +8;DI ~DI +10);                                    00015600
          I (DS~8CHR;DS~ 2 LIT"  ");                                    00015700
     END FORM;                                                          00015800
         FORM(GLOBE[1],PRINT(1),I-INDENT,INDENT);                       00015900
   INDENT~ 0;                                                           00016000
      RELEASE(PRINT);                                                   00016100
    IF NEWTOG THEN BEGIN                                                00016200
                   MOVEWORDS(PRINT(0),TAPE2(0),15);                     00016300
                   RELEASE(TAPE2);                                      00016400
              END;                                                      00016500
              CLEARB(PRINT(0));                                         00016600
      END PRINTSEQ;                                                     00016700
PROCEDURE SEARCHINFO;                                                   00016800
    BEGIN LABEL LOOP,FOUND,NOTFOUND,XIT;                                00016900
  DEFINE CLASSV = [11:4] # ;                                            00017000
    L~STKH[SCRAM~( Q MOD 125 )];                                        00017100
LOOP:NEXT ~ L.LINKI ;                                                   00017200
     IF NEXT = 0 THEN GO TO NOTFOUND ;                                  00017300
     PRES ~ NEXT; L ~ INFO[PRES];                                       00017400
     IF INFO[PRES + 1]!  Q THEN GO TO LOOP;                             00017500
     IF COUNT { 5 THEN                                                  00017600
FOUND: BEGIN  CLASS ~ IF L < 0 THEN  L.CLASSV ELSE 1; GO TO XIT END;    00017700
     IF NOT EQUAL(INFO[PRES+2],ACCUM[2],COUNT-5)                        00017800
         THEN GO LOOP ELSE GO TO FOUND;                                 00017900
NOTFOUND: CLASS ~ 0;                                                    00018000
XIT: END SEARCHINFO;                                                    00018100
STREAM PROCEDURE SCAN(NCR,COUNTV,ACCUM,COMCOUNT,RESULT,RESULTV,         00018200
         COUNT,ST2,NCRV,ST1);                                           00018300
         VALUE COUNTV,COMCOUNT,RESULTV,ST2,NCRV,ST1;                    00018400
         BEGIN                                                          00018500
         LABEL DEBLANK,NUMBERS,IDBLDR,GNC,K,EXIT,FINIS,L,ERROR,         00018600
              COMMENTS,COMMANTS;                                        00018700
         DI ~ RESULT; DI ~ DI+7;                                        00018800
         SI ~ NCRV;                                                     00018900
COMMENT SETUP DI FOR CHANGE IN RESULT AND SI FOR A LOOK AT BUFFER;      00019000
         CI ~ CI+RESULTV;   COMMENT SWITCH ON VALUE OF RESULT;          00019100
         GO TO DEBLANK;   COMMENT 0 IS INITIAL CODE;                    00019200
         GO TO IDBLDR;   COMMENT 1 IS ID CODE;                          00019300
         GO TO FINIS;     COMMENT 2 IS SPECIAL CHARACTER CODE;          00019400
         GO TO NUMBERS;   COMMENT 3 IS NUMBER CODE;                     00019500
         GO TO FINIS;     COMMENT 4 IS ERROR CODE;                      00019600
         GO TO GNC;     COMMENT 5 IS GET NEXT CHARACTOR CODE;           00019700
         GO TO COMMANTS; COMMENT 6 IS COMMENT CODE;                     00019800
                   COMMENT 7 IS DEBLANK ONLY CODE;                      00019900
              IF SC=" " THEN                                            00020000
         BEGIN K:  SI ~ SI+1;                                           00020100
              IF SC =" "  THEN GO TO K                                  00020200
         END;                                                           00020300
              GO TO FINIS;                                              00020400
DEBLANK:   IF SC = " " THEN                                             00020500
                   BEGIN L: SI ~SI+1; IF SC = " " THEN GO TO L END;     00020600
COMMENT  IF WE ARRIVE HERE WE HAVE A NON-BLANK CHARACTOR;               00020700
              NCRV ~ SI;                                                00020800
              IF SC } "0" THEN GO TO NUMBERS;                           00020900
              IF SC = ALPHA THEN GO TO IDBLDR;                          00021000
COMMENT       IF WE ARRIVE HERE WE HAVE A SPECIAL CHARACTOR (OR GNC);   00021100
GNC:          DS ~ LIT "2";                                             00021200
              TALLY ~ 1; SI ~ SI +1; GO TO EXIT;                        00021300
COMMANTS:     IF SC ! ";" THEN BEGIN                                    00021400
COMMENTS:     SI ~ SI+1;                                                00021500
              IF SC > "%" THEN GO TO COMMENTS;                          00021600
              IF SC < ";" THEN GO TO COMMENTS;                          00021700
COMMENT       CHARACTORS BETWEEN % AND SEMICOLON ARE HANDLED BY WORD    00021800
              MODE PART OF COMMENT ROUTINE;                             00021900
              END;                                                      00022000
              GO TO FINIS;                                              00022100
IDBLDR:       TALLY ~ 63; DS ~ LIT "1";                                 00022200
              COMCOUNT( TALLY~ TALLY+1;                                 00022300
                   IF SC = ALPHA THEN SI~SI+1 ELSE JUMP OUT TO EXIT);   00022400
              TALLY ~ TALLY+1; IF SC = ALPHA THEN BEGIN                 00022500
ERROR:        DI ~ DI-1; DS ~ LIT "4"; GO TO EXIT;                      00022600
              END ELSE GO TO EXIT;                                      00022700
COMMENT       IF WE ARRIVE AT ERROR WE HAVE MORE THAN 63 CHARACTORS     00022800
              IN AN IDENTIFIER OR NUMBER;                               00022900
NUMBERS:      TALLY ~ 63; DS ~ LIT "3";                                 00023000
                   COMCOUNT( TALLY ~ TALLY+1;                           00023100
                   IF SC <"0"THEN JUMP OUT TO EXIT; SI~SI+1);           00023200
              GO TO ERROR;                                              00023300
EXIT:         ST1 ~ TALLY;     COMMENT ST1 CONTAINS NUMBER OF CHAR-     00023400
                             ACTORS WE ARE GOING TO MOVE INTO THE       00023500
                             ACCUMULATOR;                               00023600
              TALLY ~ TALLY+COUNTV; ST2 ~ TALLY;                        00023700
              DI ~ COUNT; SI ~ LOC ST2; DS ~ WDS;                       00023800
COMMENT       THIS CODE UPDATED COUNT;                                  00023900
              DI ~ ACCUM; SI ~ SI-3; DS ~ 3 CHR;                        00024000
COMMENT       THIS CODE PLACES COUNT IN ACCUM AS WELL;                  00024100
         DI ~ DI+COUNTV;   COMMENT POSITION DI PAST CHARACTORS          00024200
                             ALREADY IN THE ACCUMULATOR, IF ANY;        00024300
         SI ~ NCRV; DS ~ ST1 CHR;                                       00024400
COMMENT  MOVE CHARACTORS INTO ACCUM;                                    00024500
FINIS:        DI ~ NCR; ST1 ~ SI; SI ~ LOC ST1; DS ~ WDS;               00024600
COMMENT       RESET NCR TO LOCATION OF NEXT CHARACTOR TO BE SCANNED;    00024700
              END OF SCAN;                                              00024800
PROCEDURE SCANNER;                                                      00024900
BEGIN LABEL L1;                                                         00025000
L1:                                                                     00025100
SCAN(NCR,COUNT,ACCUM[1],63-COUNT,RESULT,RESULT,COUNT,0,NCR,0);          00025200
Q~ACCUM[1];                                                             00025300
IF NCR = LCR THEN BEGIN READATAPE; GO TO L1 END END OF SCANNER;         00025400
PROCEDURE TABLE;                                                        00025500
    BEGIN                                                               00025600
LABEL IDENT,SPECHR,NUMBER,SCANAGAIN,QUOTE,XIT;                          00025700
SWITCH S~ IDENT,SPECHR,NUMBER;                                          00025800
SCANAGAIN: RESULT~COUNT~ACCUM[1]~0;                                     00025900
    SCANNER; IF EOFTOG THEN BEGIN CLASS ~ 1; GO XIT END;                00026000
     GO TO S[RESULT];                                                   00026100
NUMBER: CLASS ~ 7; GO TO SCANAGAIN;                                     00026200
SPECHR: CHR~  Q.[18:6];  CLASS ~ 8;                                     00026300
    IF CHR = "%" THEN BEGIN READATAPE; GO TO SCANAGAIN END;             00026400
     ENDTOG ~ FALSE;                                                    00026500
    IF CHR ! """ THEN GO TO SCANAGAIN;                                  00026600
      RESULT ~ 5 ; COUNT ~ 0 ; SCANNER;                                 00026700
      DO BEGIN                                                          00026800
QUOTE: RESULT ~ 5 ; COUNT ~ 0 ; SCANNER; CHR ~ Q.[18:6] END             00026900
   UNTIL CHR =   """;                                                   00027000
      GO TO SCANAGAIN;                                                  00027100
IDENT: SEARCHINFO; IF NOT ENDTOG AND CLASS = 2 THEN                     00027200
    BEGIN WHILE EXAMINE(NCR) ! ";"  DO                                  00027300
      BEGIN RESULT ~ 6; COUNT ~ 0; SCANNER END;                         00027400
      END;                                                              00027500
XIT: END TABLE;                                                         00027600
BEGIN    COMMENT  INITIALIZE - BUILDINFO,START OF PROGRAM;              00027700
    PROCEDURE BUILDINFO;                                                00027800
    BEGIN INTEGER WORDS;                                                00027900
    IF NOT BUILDTOG THEN                                                00027910
         BEGIN                                                          00027920
         IDENTCT ~ IDENTCT + 1 ;                                        00028000
         INFO[P] ~ STKH[SCRAM];                                         00028100
             STKH[SCRAM] ~ P;                                           00028400
              WORDS ~ IF COUNT { 5 THEN 1 ELSE ((COUNT-6)DIV 8 + 2);    00028500
         MOVEWORDS(ACCUM[1],INFO[P~P+1],WORDS);                         00028600
      IF P~ P+ WORDS > 1012 THEN BEGIN BUILDTOG ~TRUE ;                 00028700
                       CLEARB(PRINT(0));                                00028712
                        TOOMANY(PRINT(0),ACCUM[1],COUNT);               00028713
                        IF NEWTOG THEN                                  00028714
                           BEGIN MOVEWORDS(PRINT(0),TAPE2(0),15);       00028715
                                  RELEASE(TAPE2);                       00028716
                                  WRITE(TAPE2);                         00028717
                            END ;                                       00028718
                        RELEASE(PRINT); WRITE(PRINT);                   00028719
                        END;                                            00028719
         END;                                                           00028720
    END BUILDINFO;                                                      00028800
    STREAM PROCEDURE FIXBLK(BLANK);                                     00028900
    BEGIN DI~BLANK; DS ~8 LIT"  "; END FIXBLK ;                         00029000
    TIMEX ~ TIME(1);                                                    00029010
FILL INFO [*] WITH                                                      00029100
         0,                                                             00029200
         OCT2000200000000000, "7COMME","NT      ",                      00029300
         OCT2000300000000000,"5BEGIN",                                  00029400
         OCT2000400000000000,"3END00",                                  00029500
         OCT2000500000000000,"4ELSE0",                                  00029600
         OCT2000600000000000,"5UNTIL" ;                                 00029700
    STKH[0]~1; COMMENT  COMMENT ;                                       00029800
    STKH[2]~4; COMMENT BEGIN ;                                          00029900
    STKH[6] ~6; COMMENT END;                                            00030000
    STKH[16]~8; COMMENT ELSE;                                           00030100
    STKH[86]~10; COMMENT UNTIL;                                         00030200
                                                                        00030300
    MAYBE ~ FALSE;                                                      00030310
    LISTOG ~ SORTOG~NEWTOG~DOLLAR~LASTUSED~PRINTOG~DILLY~FALSE;         00030400
EOFTOG ~FALSE; ENDTOG ~ TRUE;  CARDI ~ TRUE;                            00030500
    FIXBLK(BLANK);                                                      00030600
    W ~ 10; COMMENT SCAN 80 COL;    P ~ 12;  XP ~ 2;                    00030700
    NEWSEQ ~ 1; SWF ~ IDENTCT ~ 0; NULLTOG ~ BUILDTOG ~ FALSE;          00030800
    READATAPE;                                                          00030900
         IF EXAMINE(NCR)="$" THEN                                       00031000
    BEGIN  LABEL PERCENT ;                                              00031100
         DO BEGIN COUNT ~ RESULT ~ ACCUM[1] ~ 0; SCANNER;               00031200
                   IF Q = "6TAPE1" THEN BEGIN SWF ~2;CARDI ~ FALSE; END;00031300
                   IF Q = "4CARD0" THEN SWF ~ 0;                        00031305
                   IF Q = "4TAPE0" THEN SWF ~ 1 ;                       00031310
                   IF Q = "7TAPEB" THEN SWF ~ 3;                        00031315
                   IF Q = "4LIST0" THEN LISTOG ~ TRUE;                  00031400
                   IF Q = "3NEW00" THEN BEGIN ACCUM[1] ~ COUNT ~ 0;     00031500
                                           RESULT ~ 0; NEWTOG ~ TRUE;   00031510
                                           SCANNER;                     00031520
                                       END;                             00031530
                   IF Q = "4SORT0" THEN SORTOG~TRUE;                    00031600
                   IF Q ="5DILLY" THEN DILLY ~ TRUE  ;                  00031700
                   IF Q = "1%0000"  THEN GO TO PERCENT ;                00031800
         END UNTIL FALSE     ;                                          00031900
    PERCENT: READATAPE;                                                 00032000
    END ELSE                                                            00032100
         BEGIN  FORMAT F1(" MISSING CONTROL CARD ");                    00032200
              WRITE(PRINT,F1);                                          00032300
              CARDI ~ SORTOG~ LISTOG ~ TRUE  ;                          00032400
                   SWF ~ 0 ;                                            00032410
         END;                                                           00032500
    DO BEGIN                                                            00032600
         TABLE; IF CLASS ! 0 THEN                                       00032700
              BEGIN                                                     00032705
                 IF NOT EOFTOG THEN                                     00032710
                   BEGIN                                                00032715
                        CLEARB(PRINT(0));                               00032720
                        IF CLASS =1                                     00032725
                          THEN DUPLICATE(PRINT(0),ACCUM[1],COUNT)       00032730
                          ELSE RESERVED(PRINT(0),ACCUM[1],COUNT);       00032735
                        IF NEWTOG THEN                                  00032740
                          BEGIN MOVEWORDS(PRINT(0),TAPE2(0),15) ;       00032745
                          RELEASE(TAPE2);WRITE(TAPE2);                  00032750
                          END;                                          00032755
                        RELEASE(PRINT);WRITE(PRINT);                    00032760
                   END;                                                 00032765
              END                                                       00032800
           ELSE BUILDINFO;                                              00032900
        END UNTIL EOFTOG;                                               00033000
     CLOSE(NAMES,SAVE);    EOFTOG~FALSE;                                00033100
                                                                        00033200
  IF CARDI   THEN BEGIN  WD ~ 10; W~9; W2~12 END                        00033300
             ELSE BEGIN  WD~15; W2 ~14; W ~ 12 END;                     00033400
ENDTOG ~ FALSE;                                                         00033500
LASTUSED ~ TRUE;  READATAPE ;                                           00033600
DO TABLE UNTIL CLASS = BEGINV   ;                                       00033700
END  INFTIALIZE  ;                                                      00033800
BEGIN                                                                   00033900
STREAM PROCEDURE MSEQ(F,T);                                             00034000
BEGIN SI ~F;SI ~SI + 8; DI ~ T; DS~WDS END MSEQ;                        00034100
PROCEDURE BLOCK;                                                        00034200
    BEGIN REAL BEGINV1,BEGINV;                                          00034300
      LABEL LOOP,XIT,LEND,LBEGIN,LELSE,LUNTIL,FOUND;                    00034400
      SWITCH S ~ FOUND,LOOP,LBEGIN,LEND,LELSE,LUNTIL;                   00034500
    MSEQ(SEQ1,BEGINV);                                                  00034600
LOOP: TABLE;                                                            00034700
         IF EOFTOG THEN GO TO XIT;                                      00034710
      GO TO S[CLASS];                                                   00034800
      GO TO LOOP;                                                       00034900
FOUND: BUILDXREF; GO TO LOOP;                                           00035000
LBEGIN: IF NOT ENDTOG THEN BEGIN BLOCK ; GO LOOP END ; GO LOOP ;        00035100
LELSE :                                                                 00035200
LUNTIL: ENDTOG  ~ FALSE; GO LOOP ;                                      00035300
LEND:   MSEQ(BEGINV1,PRINT(W2));                                        00035400
        ENDTOG ~ TRUE ;                                                 00035500
        PRINTOG ~ FALSE;                                                00035600
        IF LISTOG THEN RELEASE(PRINT)       ;                           00035700
        IF LISTOG AND NEWTOG                                            00035790
                  THEN BEGIN MOVEWORDS(PRINT(0),TAPE2(0),15);           00035800
                          RELEASE(TAPE2);                               00035900
                        END ;                                           00036000
        CLEARB(PRINT(0));                                               00036100
XIT: END   BLOCK ;                                                      00036200
    BLOCK;                                                              00036300
    XCOUNT ~ XP DIV 2 - 1 ;                                             00036310
    INFOL ~ 1023 - P; NEWSEQ ~ NEWSEQ - 1;                              00036320
END;                                                                    00036400
IF DILLY THEN                                                           00036500
    BEGIN  LABEL EOF,L; WRITE(PRINT[PAGE]);                             00036600
           L: READ(TAPE[SWF],WD,GLOBE[*])[EOF];                         00036700
         IF LISTOG THEN WRITE(PRINT,15,GLOBE[*]);                       00036800
         IF NEWTOG THEN WRITE (TAPE2,15,GLOBE[*]);                      00036900
         GO TO L;                                                       00037000
    EOF: END;                                                           00037100
    BEGIN LABEL XIT, LOOP;                                              00037200
         DEFINE NXT= [38:10]#,NWD= [10:4]#,CHS= [12:6]#;                00037300
    P~ 12;                                                              00037400
    LOOP: COUNT ~ INFO[P+1]. CHS ;                                      00037500
    IF COUNT = 0 THEN GO TO XIT   ;                                     00037600
                   I ~ IF COUNT { 5 THEN 2 ELSE                         00037700
              (COUNT-6) DIV 8 + 3;                                      00037800
                   INFO[P].NXT ~ P+I ; P~P+I; GO TO LOOP;               00037900
    XIT: INFO [P-I].NXT ~ 0;                                            00038000
    END ;                                                               00038100
IF SORTOG THEN BEGIN INTEGER ARRAY VECTOR [0:IDENTCT  ];                00038200
           INTEGER BASE,TOP;                                            00038300
                      INTEGER V,HIGH,SYMBOL,N,N1,N2;DEFINE IBASE=12#;   00038400
         INTEGER PROCEDURE PRE;                                         00038500
    BEGIN                                                               00038600
                     V~ -1; P ~ IBASE;                                  00038700
               DO BEGIN   VECTOR[V~V+1] ~ P;                            00038800
                   END UNTIL  P ~ INFO[P].LINKI = 0;                    00038900
      PRE ~ V;                                                          00039000
              END PRE ;                                                 00039100
                                                                        00039200
         PROCEDURE POST;                                                00039300
              BEGIN INTEGER TEMP;                                       00039400
              TEMP ~ VECTOR[0]; V ~ 0;                                  00039500
              DO BEGIN                                                  00039600
                   INFO[TEMP]~INFO[TEMP]&VECTOR[V+1][38:38:10] ;        00039700
              END UNTIL TEMP ~ VECTOR[V~V+1]=0 ;                        00039800
         PTEMP ~ VECTOR[0];                                             00039900
              END POST ;                                                00040000
                                                                        00040100
         BOOLEAN PROCEDURE COLLATE(S1,S2);VALUE S1,S2; INTEGER S1,S2;   00040200
              COMMENT S1 AND S2 ARE INDEXES INTO INFO. THEY ARE         00040300
              POINTER WHICH ARE HELD IN VECTOR [ ], OR SYMBOL ;         00040400
         BEGIN                                                          00040500
              INTEGER STREAM PROCEDURE COMPARE(A,B,N);VALUE N ;         00040600
              BEGIN  COMMENT  A > B  2, A=B  1 , A < B  0;              00040700
              LABEL  LOW;                                               00040800
                                                                        00040900
                   SI ~ A; DI ~ B; SI ~ SI + 11; DI ~ DI +11 ;          00041000
                   IF N SC < DC THEN GO TO LOW;                         00041100
                   SI ~ SI-N; DI ~ DI - N ;                             00041200
                   IF  N SC = DC THEN TALLY ~ 1  ELSE TALLY ~ 2;        00041300
              LOW: COMPARE ~ TALLY ;                                    00041400
              END COMPARE;                                              00041500
         LABEL HIGH,EQL, LOW, XIT;                                      00041600
              SWITCH S ~ EQL,HIGH;                                      00041700
                                                                        00041800
              N ~ IF N1~INFO[S1+1].[12:6]<N2~INFO[S2+1].[12:6]          00041900
                   THEN N1 ELSE N2;                                     00042000
              GO TO S[COMPARE(INFO[S1],INFO[S2],N)];                    00042100
    LOW:  COLLATE ~ TRUE; GO TO XIT;                                    00042200
    HIGH:  COLLATE ~ FALSE; GO TO XIT;                                  00042300
    EQL : IF N1<N2 THEN  COLLATE ~ TRUE ELSE COLLATE ~ FALSE;           00042400
    XIT:END  COLLATE;                                                   00042500
         PROCEDURE SORT (BASE,TOP);VALUE BASE,TOP;                      00042600
                                  INTEGER BASE,TOP;                     00042700
         BEGIN  INTEGER LOW;                                            00042800
                LABEL  UP,DOWN,LAST;                                    00042900
              WHILE TOP> BASE DO                                        00043000
              BEGIN  LOW ~ BASE; HIGH ~ TOP;                            00043100
                   SYMBOL ~ VECTOR [LOW] ;                              00043200
    DOWN:     WHILE LOW < HIGH DO                                       00043300
                 IF COLLATE(VECTOR[HIGH],SYMBOL)                        00043400
                 THEN BEGIN  VECTOR [LOW] ~ VECTOR [HIGH] ;             00043500
                             LOW ~ LOW + 1 ; GO TO UP;                  00043600
                      END   ELSE  HIGH~HIGH-1;                          00043700
                 GO TO LAST;                                            00043800
    UP:       WHILE  LOW < HIGH DO                                      00043900
                      IF COLLATE(SYMBOL,VECTOR[LOW])                    00044000
                 THEN BEGIN VECTOR [HIGH] ~ VECTOR [LOW] ;              00044100
                        HIGH ~ HIGH - 1; GO TO DOWN ;                   00044200
                       END ELSE  LOW ~ LOW + 1;                         00044300
    LAST:     VECTOR[LOW] ~ SYMBOL;                                     00044400
              IF (LOW-BASE) { (TOP - LOW ) THEN                         00044500
                BEGIN SORT(BASE,LOW-1);BASE ~LOW+1 END                  00044600
           ELSE BEGIN SORT (LOW+1,TOP);TOP ~ LOW-1 END ;                00044700
              END                                                       00044800
         END  SORT;                                                     00044900
         TOP ~ PRE; BASE ~ 0;                                           00045000
         SORT(BASE,TOP);                                                00045100
         POST;                                                          00045200
    END  SORTOG BLOCK ;                                                 00045300
IF LISTOG THEN BEGIN CLEARB(PRINT(0)); WRITE(PRINT[PAGE]) END;          00045400
  WRITE(PRINT,TITLE);  WRITE(PRINT); CLEARB(PRINT(0));                  00045500
IF NEWTOG THEN WRITE(TAPE2,TITLE);                                      00045501
BEGIN PROCEDURE PRINTX(TOG);BOOLEAN TOG;                                00045600
    BEGIN                                                               00045700
    LABEL  LOOP,LINE,LINE1,L;                                           00045800
  LABEL EOS;                                                            00045900
         P ~ IF SORTOG THEN PTEMP ELSE 12;                              00046000
LOOP: XP ~ INFO[P]; COUNT ~ INFO[  P+1].[12:6] ;                        00046100
    IF COUNT = 0 OR P = 0 THEN GO TO EOS ;                              00046200
    IF XP.LINKCR = 0 THEN                                               00046300
         BEGIN                                                          00046400
    NULLTOG ~ TRUE;                                                     00046500
    IF MAYBE THEN PRINTID(TRUE);                                        00046600
L:  P ~ INFO[P].LINKI ;                                                 00046700
           GO TO LOOP                                                   00046800
         END;                                                           00046900
    IF TOG  THEN BEGIN                                                  00047000
    PRINTID(FALSE);                                                     00047100
      I ~     (INDENT~COUNT DIV 10) + 1; GO TO LINE1;                   00047200
LINE: I ~ 1;                                                            00047300
LINE1:       XC ~ XP.LINKC ; XR ~ XP.LINKR;                             00047400
       MOVEWORDS(XREF[XC,XR+1],GLOBE[I], 1) ;                           00047500
         IF XP ~ XREF[XC,XR]  = 0                                       00047600
         THEN BEGIN  PRINTSEQ;GO TO L END;                              00047700
         IF I = 11                                                      00047800
           THEN BEGIN PRINTSEQ ; GO TO LINE END;                        00047900
         I ~ I + 1 ;                                                    00048000
       GO TO LINE1 ;                                                    00048100
    END;   GO TO L;                                                     00048200
EOS:END PRINTX;                                                         00048300
PRINTX(TRUE);                                                           00048400
IF NULLTOG THEN BEGIN                                                   00048500
    FORMAT F(" NO CROSS REFERENCE FOR THE FOLLOWING IDENTIFIERS");      00048600
   WRITE (PRINT[PAGE]);                                                 00048700
   WRITE (PRINT[DBL],F);                                                00048800
    CLEARB(PRINT(0));                                                   00048900
         IF NEWTOG THEN WRITE(TAPE2,F);                                 00048910
    MAYBE ~ TRUE;                                                       00048920
   PRINTX(FALSE);                                                       00049000
  END;                                                                  00049100
END;                                                                    00049200
    BEGIN                                                               00049300
    STREAM PROCEDURE MESG1(SEQ1,NEWSEQ,PRINT);                          00049305
    BEGIN DI ~ PRINT; SI ~ SEQ1 ; SI ~ SI + 8 ; DS ~ WDS ;              00049400
         DS ~ 41 LIT " WAS THE LAST SEQUENCE FIELD. THERE WERE ";       00049500
         SI ~ NEWSEQ; DS ~ 4 DEC;                                       00049600
         DS ~ 13 LIT " BLANK FIELDS";                                   00049700
    END;                                                                00049800
    FORMAT OUT MESG2 (I3, " NAMES WERE CROSS REFERENCED. ",I5,          00049900
                          " REFERENCES ARE LISTED ABOVE."),             00050000
         MESG3("TABLE SPACE LEFT FOR ADDITIONAL NAMES IS " I4 ,         00050100
                ".AT LEAST TWO SPACES ARE REQUIRED PER NAME."),         00050110
         MESG4( "ELAPSED TIME WAS " ,F8.1, " SECONDS");                 00050200
         MESG1(SEQ1,NEWSEQ,PRINT(0));                                   00050300
         RELEASE (PRINT);                                               00050400
         WRITE(PRINT,MESG2,IDENTCT,XCOUNT);                             00050500
         WRITE(PRINT,MESG3,INFOL);                                      00050600
         WRITE(PRINT,MESG4,(TIME(1)-TIMEX) / 60);                       00050700
           IF NOT EOFTOG  AND SWF  = 0 THEN DO READATAPE UNTIL EOFTOG;  00050800
END END.                                                                00050900
 LABEL  000000000NAMES  0010000001
$ TAPEBLK SORT
AUS,      TBLK,SL,SW,TP,XLABEL,LMAX,DXMAX,P,UNDEF,NSV,SV,FN,SR,FM,LS,LB,
DA,PR,XP,MD,EQ,FXP,FLP,BOO,DBL,CMP,CMAX,DBMAX,CARD,PPUNCH,PRINT,TENIR,
PUNCH,XBWRD,HASH,BOXI,BOXL,BOXP,BOXC,JV,XF,NOM,NOM1,NOM2,NOM3,NOM4,
KX,ACOUNT,CARTE,PGCT,SARG,CCT,IPHASE,CHCT,TPOSIT,CHAR,FWCT,NHOLL,LJSTCT,
WH,STNXT,STANO,COLMN1,TYPUS,MODUS,CTOTAL,KPASS,JST,FLGX,FLGZ,NHMX,CARTX,
RSVX,RSVZ,DRPX,DRPZ,EQUMAX,GANZ,NULLEN,BRUCH,POTENZ,LISTCT,SPCT,PNAME,
CNOM,JTAB,ARGMAX,JSWGO,KCT,JXLS,BOXA,BOXMD,BOXE,RG1,RG2,RG3,RG4,PNT,
PSCALE,SVL,NSRG,RESULT,BOXJ,ALTO,BASSO,DFCT,DXCT,IXCT,DATACT,BLKDCT,ID,
LOW,ADR,INFO,SENSW,SENSL,OUTB,KEY,DESCR,RBAND,READH,CONVT,ALFA,D,WHOLD,
WX,WZ,WB,ASTACK,LSTX,NHOLD,E,SVCH,INDEX,XSTACK,LXSAVE,CSTACK,BSTACK,
SVSW,TBSW,PCOSW,PPSW,BWX,HEADSW,SRCHSW,XCSW,XESW,BRKUSW,GLOBSW,CONSW,
CBDECL,CBNOM,CBSIZE,ASNVAR,DOTAB,DOJ,DOM2,DOM3,RELTAB,RELSYM,DCT,OKTLSW,
DFSW,FCNSW,SUBSW,BGNSW,LISTSW,FALSCH,WAHR,CQSW,FXPSW,DFCNSW,OUTSW,
CTRLSW,LINESW,DATASW,BLKDSW,VSPACE,ALTER,USE,MODE,CLASS,LENGTH,ARGS,
LCLASS,CPUT,MODE,MPUT,CB,ADDRESS,ZPUT,INTG,INTGW,DFALT,PURGE,TBSCH,
LAVER,NFIX,EMBED,SARG,VPROC,LOOKUP,EFFACE,SQUEEZE,SVDCMP,SVQ,MAX,MIN,
HEADER,EJECT,LACHER,DESCRP,DSET,RSET,WSET,RRV,RWTNR,DCLZ,GNZ,LIRE,
SCRIRE,    SCAN,RESCAN,    CLZ,STACKZ,STACK,STACK2,PASS2,ZCMNT,POSIT,
LJUST,MARGIN,INDENT,SECURE,ZSEMI,BRI,ZAHLX,ZAHL1,ZAHL2,EQLDEC,QUERY,
FMTST,SJUST,ALFAN,READC,OPEN,PASS,     ZAHL,ERRMSG,CUTBACK,
NPROC,BPROC,LPROC,LPULL,ADJZ,VERIFY,NDUMP,DUMPH,SKAN,PNSCAN,RSWDZ,
BEXP,NSEXP,FMPULL,FLPULL,DXQ,NSSV,SIMPEL,LCURSE,LISTEL,LISTP,LISTPZ,GT,
DATAPR,TPROC,GTO,THGTO,ZGTO,JCHECK,DMN,DIMEN,RETOUR,STEND,MONTOR,
XTYPE,ARITHOP,DFPROC,LFNAME,LFPROC,MAXMIN,MODF,SFPROC,FNPR,STRPR,
SCREEN,PASSJ,SVPASS,ARGBCT,SVPASS,ARGCT,SVPR,SPEW,NSCAN,NXDMP,DXCHECK,
TERM,      ARTHX,ARTHI,RVSG,NOPE,BOP,RELOP,TVALUE,ARTHREL,BCONTIN,BTERM,
FMTSPEC,FMT,CGOTO,DFCN,IFGO,BIFURC,TRFORK,ARTHIF,LOGIF,IFSTA,DOST,ASGN,
AGOTO,ASSIGN,SRCALL,SRDUMP,PDUMP,BCALL,OVERFL,SLITE,DVCHK,CALL,CLIST,
COMMON,RETURN,RGOTO,ELIST,EQUIV,STOP,BACKSP,ENDFILE,ROULER,ENSTA,DATA,
DFLIST,RCD,ETYPE,PRC,BLOCKDATA,TYPDEC,LP3,LP2,LP1,BLCOM,LPAR,BLANK,
COMMA,EQUALS,SLASH,DOLLAR,STMNT,PRTETE,FNCHECK,STNMBR,NEXT,RECOVER,
ENDSR,              PRLK4,      GNC,SSCAN,PNX,CQCLR,LMASK,FDECL,MDECL,
PFX,PUSH,STASH,CQDX,ECALC,SVDX,LNGTH,RNIT2,WARN,PCHK2,PCHK1,CHK1,
DCHECK,EQPROC,CBK,CBK1,CDECL,SRHEAD,COMDEC,DELE,    REDUCE,EPROC2,
CQTAB,LOCDEC,DXDECL,FMTHD,LDECL,PROC1,NDUMP,SCRP,PWFLD,CQPR2,CQPR1,
