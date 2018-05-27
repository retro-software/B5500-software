                                                                        00000100
                                                                        00000200
BEGIN COMMENT DABTIME                                                   00000300
    TO OBTAIN LISTING MERGE IN BLANK CARD WITH SEQUENCE NUMBER 00000030;00000400
COMMENT        THE CUBE LIBRARY NUMBER IS Q000003.  THE PROGRAM NAME IS 00000500
               "DABTIME".  THIS VERSION DATED 6/28/68;                  00000600
PROCEDURE TIMEIT(A);                                                    00000700
FILE A;                                                                 00000800
BEGIN                                                                   00000900
    OWN BOOLEAN B;                                                      00001000
    INTEGER E,F,G,H,I;                                                  00001100
    STREAM PROCEDURE C(A,B,D,E);                                        00001200
    VALUE A;                                                            00001300
    BEGIN                                                               00001400
        SI~ LOC A;                                                      00001500
        SI~ SI+3;                                                       00001600
        DI~ B;                                                          00001700
        DS~ 2 OCT;                                                      00001800
        DI~ D;                                                          00001900
        DS~ 3 OCT                                                       00002000
    END;                                                                00002100
    STREAM PROCEDURE D(A,B,C,E,F,G,H,I,J,K);                            00002200
    VALUE A,B,C,E,F,G,H,I,J;                                            00002300
    BEGIN                                                               00002400
        LABEL L, M, N, O, P, Q;                                         00002500
        DI~ K;                                                          00002600
COMMENT*****************************************************************00002700
        SKIP 8 DB;                                                      00002800
COMMENT*****************************************************************00002900
        DS~ 6 RESET;                                                    00003000
COMMENT*****************************************************************00003100
        DS~ 4 SET;                                                      00003200
COMMENT*****************************************************************00003300
        DS~ RESET;                                                      00003400
COMMENT*****************************************************************00003500
        SKIP 8 DB;                                                      00003600
COMMENT*****************************************************************00003700
        DS~ 2 SET;                                                      00003800
COMMENT*****************************************************************00003900
        DS~ 4 RESET;                                                    00004000
COMMENT*****************************************************************00004100
        DI~ LOC K;                                                      00004200
COMMENT*****************************************************************00004300
        SI~ K;                                                          00004400
COMMENT*****************************************************************00004500
        DS~ WDS;                                                        00004600
COMMENT*****************************************************************00004700
        DI~ K;                                                          00004800
        DS~ 8 LIT" ";                                                   00004900
        SI~ K;                                                          00005000
        DS~ 14 WDS;                                                     00005100
        DI~ K;                                                          00005200
        SI~ LOC A;                                                      00005300
        DS~ 2 DEC;                                                      00005400
        2(DS~ LIT":"; DS~ 2 DEC);                                       00005500
        DI~ DI+1;                                                       00005600
        CI~ CI+E;                                                       00005700
        DS~ 3 LIT"SUN"; GO L; SKIP SB;                                  00005800
        DS~ 3 LIT"MON"; GO L; SKIP SB;                                  00005900
        DS~ 4 LIT"TUES"; GO L; SKIP SB;                                 00006000
        DS~ 6 LIT"WEDNES"; GO L;                                        00006100
        DS~ 5 LIT"THURS"; GO L;                                         00006200
        DS~ 3 LIT"FRI"; GO L; SKIP SB;                                  00006300
        DS~ 5 LIT"SATUR";                                               00006400
L:                                                                      00006500
        DS~ 4 LIT"DAY,";                                                00006600
        DI~ DI+1;                                                       00006700
        CI~ CI+F;                                                       00006800
        DS~ 3 LIT"JAN"; GO M; SKIP SB;                                  00006900
        DS~ 4 LIT"FEBR"; GO M; SKIP SB;                                 00007000
        DS~ 5 LIT"MARCH"; GO O;                                         00007100
        DS~ 5 LIT"APRIL"; GO O;                                         00007200
        DS~ 3 LIT"MAY"; GO O; SKIP SB;                                  00007300
        DS~ 4 LIT"JUNE"; GO O; SKIP SB;                                 00007400
        DS~ 4 LIT"JULY"; GO O; SKIP SB;                                 00007500
        DS~ 6 LIT"AUGUST"; GO O;                                        00007600
        DS~ 6 LIT"SEPTEM"; GO N;                                        00007700
        DS~ 4 LIT"OCTO"; GO N; SKIP SB;                                 00007800
        DS~ 5 LIT"NOVEM"; GO N;                                         00007900
        DS~ 5 LIT"DECEM"; GO N;                                         00008000
M:                                                                      00008100
        DS~ 4 LIT"UARY"; GO O;                                          00008200
N:                                                                      00008300
        DS~ 3 LIT"BER";                                                 00008400
O:                                                                      00008500
        DI~ DI+1;                                                       00008600
        A~ DI;                                                          00008700
        DI~ LOC B;                                                      00008800
        SI~ LOC G;                                                      00008900
        DS~ 2 DEC;                                                      00009000
        DI~ A;                                                          00009100
        SI~ LOC B;                                                      00009200
        IF SC="0" THEN                                                  00009300
            SI~ SI+1                                                    00009400
        ELSE                                                            00009500
            DS~ CHR;                                                    00009600
        DS~ CHR;                                                        00009700
        DS~ 4 LIT", 19";                                                00009800
        SI~ LOC H;                                                      00009900
        DS~ 2 DEC;                                                      00010000
        IF SC!"+" THEN                                                  00010100
            BEGIN                                                       00010200
                DI~ DI+4;                                               00010300
                DS~ 16 LIT"PROCESSOR TIME =";                           00010400
                DI~ DI+1;                                               00010500
                A~ CI;                                                  00010600
                GO P;                                                   00010700
                DI~ DI+4;                                               00010800
                DS~ 10 LIT"I/O TIME =";                                 00010900
                DI~ DI+1;                                               00011000
                SI~ LOC J;                                              00011100
                A~ CI;                                                  00011200
                GO P;                                                   00011300
                GO Q;                                                   00011400
P:                                                                      00011500
                B~ DI;                                                  00011600
                DI~ LOC C;                                              00011700
                DS~ 8 DEC;                                              00011800
                SI~ LOC C;                                              00011900
                DI~ B;                                                  00012000
                5(IF TOGGLE THEN IF SC="0" THEN SI~ SI+1 ELSE DS~ CHR   00012100
                  ELSE DS~ CHR);                                        00012200
                DS~ CHR;                                                00012300
                DS~ LIT".";                                             00012400
                DS~ 2 CHR;                                              00012500
                DS~ 8 LIT" SECONDS";                                    00012600
                CI~ A;                                                  00012700
Q:                                                                      00012800
            END                                                         00012900
    END;                                                                00013000
ARRAY X[0:14];COMMENT**************************************************;00013100
    C(TIME(0),E,F,A(0));                                                00013200
    G~ ((((E-1)DIV 4)+E+F)MOD 7)|5;                                     00013300
    IF F<60 THEN                                                        00013400
        F~ F-1                                                          00013500
    ELSE                                                                00013600
        IF E MOD 4=0 THEN                                               00013700
            F~ F-1;                                                     00013800
    IF F> 212 THEN                                                      00013900
        F~(F+31)DIV 61+F                                                00014000
    ELSE                                                                00014100
        IF F>59 THEN                                                    00014200
            F~ (F+62)DIV 61+F;                                          00014300
    H~ TIME(1)|.016667;                                                 00014400
    D(H DIV 3600, I~H DIV 60 MOD 60, I~ H MOD 60,                       00014500
      G,(F DIV 31)|5, H~ F MOD 31+1, E,                                 00014600
      IF B THEN H~ TIME(2)|1.6667 ELSE-1, IF B THEN H~ TIME(3)|1.6667   00014700
COMMENT*****************************************************************00014800
      ELSE-1,A);                                                        00014900
ELSE-1,X[0]);COMMENT***************************************************;00015000
    B~ TRUE;                                                            00015100
COMMENT*****************************************************************00015200
    RELEASE(A,15)                                                       00015300
;WRITE(A[DBL],15,X[*])COMMENT******************************************;00015400
END;                                                                    00015500
BOOLEAN LISTOG,TAPETOG,SOURCETOG,INDEXTOG,FLOWTOG;                      00015600
BOOLEAN TIME;                                                           00015700
 BOOLEAN TAPIN;                                                         00015800
   ARRAY A, B[0:30];                                                    00015900
 BOOLEAN NONSPACERTOG;                                                  00016000
LABEL LA,LC;                                                            00016100
INTEGER SCRAM;                                                          00016200
STREAM PROCEDURE PRINTHEN(L,A,I); VALUE I;                              00016300
 BEGIN LOCAL T; DI~LOC T; SI~A; DS ~ WDS; SI ~ SI+8; DI ~ L;            00016400
60(DS~2LIT" ");DI~L;DS~WDS;DI~DI+4;I(DS~2LIT": ");DS~ T CHR;END;        00016500
ALPHA Q;                                                                00016600
   REAL I,J,T,C,GT1,GT2,GT3,GT4          ,COUNT,TCOUNT,RESULT,NXTELBT,  00016700
              PREVL, RECORDCOUNT, RCF,                                  00016800
    FCR,NCR,LCR,LASTUSED,AVAIL;                                         00016900
 BOOLEAN NST,DOKLUDGE,SPENST;                                           00017000
INTEGER ELCLASS    ;                                                    00017100
   ARRAY SPECIAL[0:31], INFO[0:31,0:255], ACCUM[0:10],ELBAT[0:75],      00017200
         STACKHEAD[0:125], TEN[-46:69];                                 00017300
BOOLEAN STREAM PROCEDURE EQUALBLANKSEQ(A,B);VALUE A;                    00017400
BEGIN SI~A; DI~B; IF 8 SC=DC THEN TALLY~1;EQUALBLANKSEQ~TALLY END;      00017500
STREAM PROCEDURE SEQIT(A,B);VALUE A,B;                                  00017600
BEGIN DI~A;DS~2LIT"*";SI~LOC B;DS~6DEC END;                             00017700
              DEFINE MON   =[ 1: 1]#,                                   00017800
                     CLASS =[ 2: 7]#,                                   00017900
                     FORMAL=[ 9: 1]#,                                   00018000
                     VO    =[10: 1]#,                                   00018100
                     LVL   =[11: 5]#,                                   00018200
                    ADDRESS=[16:11]#,                                   00018300
                     INCR  =[27: 8]#,                                   00018400
                     LINK  =[35:13]#,                                   00018500
                     LINKR =[35: 5]#,                                   00018600
                     LINKC =[40: 8]#;                                   00018700
 COMMENT THESE DEFINES ARE USED TO PICK APART THE ELBAT WORD.           00018800
              MON IS THE BIT WHICH IS ON IF THE QUANTITY IS MONITORED.  00018900
              CLASS IS THE PRINCIPAL IDENTIFICATION OF A GIVEN          00019000
                  QUANTITY.                                             00019100
              FORMAL IS THE BIT WHICH IS ON IF THE QUANTITY IS A FORMAL 00019200
                  NEXT ITEM IN THE STACK. ZERO IS AN END FLAG.          00019300
              LINKR AND LINKC ARE SUBDIVISIONS OF LINK.;                00019400
COMMENT CLASSES FOR IDENTIFIERS;                                        00019500
     DEFINE        UNKNOWNID      =00#,                                 00019600
                   STLABID        =01#,                                 00019700
                   LOCLID         =02#,                                 00019800
                   DEFINEDID      =03#,                                 00019900
                   LISTID         =04#,                                 00020000
                   FRMTID         =05#,                                 00020100
                   FILEID         =06#,                                 00020200
                   SWITCHID       =07#,                                 00020300
                   PROCID         =08#,                                 00020400
                   INTRNSICPROCID =09#,                                 00020500
                   STRPROCID      =10#,                                 00020600
                   BOOSTRPROCID   =11#,                                 00020700
                   REALSTRPROCID  =12#,                                 00020800
                   ALFASTRPROCID  =13#,                                 00020900
                   INTSTRPROCID   =14#,                                 00021000
                   BOOPROCID      =15#,                                 00021100
                   REALPROCID     =16#,                                 00021200
                   ALFAPROCID     =17#,                                 00021300
                   INTPROCID      =18#,                                 00021400
                   BOOID          =19#,                                 00021500
                   REALID         =20#,                                 00021600
                   ALFAID         =21#,                                 00021700
                   INTID          =22#,                                 00021800
                   BOOARRAYID     =23#,                                 00021900
                   REALARRAYID    =24#,                                 00022000
                   ALFAARRAYID    =25#,                                 00022100
                   INTARRAYID     =26#,                                 00022200
                   LABELID        =27#,                                 00022300
                   TRUTHV         =28#,                                 00022400
                   LITNO          =29#,                                 00022500
                   NONLITNO       =30#,                                 00022600
                   STRNGCON       =31#,                                 00022700
                   LEFTPAREN      =32#,                                 00022800
COMMENT CLASSES FOR THOSE SYNTACICAL QUANTITIES WHICH CAN               00022900
              BEGIN A STATEMENT;                                        00023000
                   READV          =33#,                                 00023100
                   WRITEV         =34#,                                 00023200
                   FORV           =35#,                                 00023300
                   WHILEV         =36#,                                 00023400
                   DOV            =37#,                                 00023500
                   UNTILV         =38#,                                 00023600
                   ELSEV          =39#,                                 00023700
                   ENDV           =40#,                                 00023800
    SEMICOLON=41#,                                                      00023900
    FILLV=42#,                                                          00024000
       IFV      =  43#,                                                 00024100
       GOV      =  44#,                                                 00024200
       RELEASEV =  45#,                                                 00024300
                   BEGINV         =46#,                                 00024400
COMMENT CLASS   FOR DECLARATORS;                                        00024500
                   DECLARATOR     =47#,                                 00024600
COMMENT STREAM RESERVED WORDS;                                          00024700
                   SIV            =48#,                                 00024800
                   DIQ            =49#,                                 00024900
                   CIV            =50#,                                 00025000
                   TALLYV         =51#,                                 00025100
                   DSV            =52#,                                 00025200
                   SKIPV          =53#,                                 00025300
                   JUMPV          =54#,                                 00025400
                   DBV            =55#,                                 00025500
                   SBV            =56#,                                 00025600
                   TOGGLEV        =57#,                                 00025700
                   SCV            =58#,                                 00025800
                   LOCV           =59#,                                 00025900
                   DCV            =60#,                                 00026000
                   LOCALV         =61#,                                 00026100
                   LITV           =62#,                                 00026200
                   TRNSFER        =63#,                                 00026300
COMMENT MISC.  SYNTACTICAL QUANTITIES;                                  00026400
                   COMMENTV       =64#,                                 00026500
                   LFTBRKET       =65#,                                 00026600
                   VALUEV         =66#,                                 00026700
                   COMMA          =67#,                                 00026800
                   PERIOD         =68#,                                 00026900
                   STEPV          =69#,                                 00027000
                   COLON          =70#,                                 00027100
                   RTBRKET        =71#,                                 00027200
                   RTPAREN        =72#,                                 00027300
                   WITHV          =73#,                                 00027400
                   THENV          =74#,                                 00027500
                   TOV            =75#,                                 00027600
                   CROSSHATCH     =76#,                                 00027700
COMMENT SCAN ERROR CLASS;                                               00027800
                   SCANERR        =77#,                                 00027900
COMMENT CLASSES FOR OPERATORS;                                          00028000
                   NOTOP          =78#,                                 00028100
                   ASSIGNOP       =79#,                                 00028200
                   AMPERSAND      =80#,                                 00028300
                   EQVOP          =81#,                                 00028400
                   IMPOP          =82#,                                 00028500
                   OROP           =83#,                                 00028600
                   ANDOP          =84#,                                 00028700
                   RELOP          =85#,                                 00028800
                   ADOP           =86#,                                 00028900
                   MULOP          =87#,                                 00029000
                   FACTOP         =88#;                                 00029100
 COMMENT SUBCLASSES FOR DECLARATORS;                                    00029200
    DEFINE         OWNV           =01#,                                 00029300
                   SAVEV          =02#,                                 00029400
                   BOOV           =03#,                                 00029500
                   REALV          =04#,                                 00029600
                   ALFAV          =05#,                                 00029700
                   INTV           =06#,                                 00029800
                   LABELV         =07#,                                 00029900
                   DUMPV          =08#,                                 00030000
                   LISTV          =09#,                                 00030100
                   OUTV           =10#,                                 00030200
                   INV            =11#,                                 00030300
                   MONITORV       =12#,                                 00030400
                   SWITCHV        =13#,                                 00030500
                   PROCV          =14#,                                 00030600
                   ARRAYV         =15#,                                 00030700
                   FORMATV        =16#,                                 00030800
                   FILEV          =17#,                                 00030900
                   STREAMV        =18#,                                 00031000
                   FORWARDV       =19#,                                 00031100
                   DEFINEV        =20#;                                 00031200
   LABEL LB,EXIT;                                                       00031300
ARRAY CURRSEQA[0:2];                                                    00031400
 DEFINE CURRSEQ = CURRSEQA[0]#;                                         00031500
BOOLEAN PAGETOG;                                                        00031600
 STREAM PROCEDURE BLANKOUT(L);                                          00031700
    BEGIN DI ~ L; DS ~ 8 LIT " " END;                                   00031800
 REAL IMC;                                                              00031900
 FILE IN CARD(4, 10, 30);                                               00032000
ARRAY LINE[0:15];                                                       00032100
 SAVE FILE ALDOC(2,120,16,SAVE 1);                                      00032200
FILE CODE DISK SERIAL[20:900](1, 20, 1380);                             00032300
 FILE IN TAPE(2, 60, 10);                                               00032400
FILE EDOC DISK SERIAL[20:900](1, 20, 1380);                             00032500
FILE FLOW DISK SERIAL[20:990](1, 14, 1260);                             00032600
FILE CODAL DISK SERIAL[20:384](2,2,96);                                 00032700
FILE OUT BUG 4 (4, 15);                                                 00032800
BOOLEAN ENDTOG;                                                         00032900
INTEGER DELTA; ARRAY BEGINARRAY[0:100];                                 00033000
LABEL L101,L102,L103,L104,L105,L106;                                    00033100
FILE TEMPTAPE DISK SERIAL[20:3000](1, 16, 1200);                        00033200
DEFINE TTAPE = TEMPTAPE#;                                               00033300
STREAM PROCEDURE ZOT(SEQ      ,      L); VALUE   L;                     00033400
    BEGIN DI ~ SEQ; SI~L; SI~L; DS~WDS; DI~L; DS ~ LIT "%" END;         00033500
 STREAM PROCEDURE PRINT(LINE,NCR); VALUE NCR;                           00033600
              BEGIN                                                     00033700
                DI ~ LINE; DS ~ 16 LIT " ";                             00033800
                SI ~ NCR; DS ~ 9 WDS;                                   00033900
                DS ~ 8 LIT " ";                                         00034000
                DS ~ WDS;                                               00034100
                SI ~ LINE; DS ~2WDS END;                                00034200
 INTEGER STREAM PROCEDURE SETUP(TAPE,LCR,CARDSEQ,TAPESEQ);              00034300
              BEGIN                                                     00034400
                SI ~ CARDSEQ; DI ~ TAPESEQ;                             00034500
                IF 8 SC > DC THEN TALLY~1                               00034600
                ELSE BEGIN SI~SI-8; DI~DI-8;TAPESEQ ~ SI;               00034700
                  IF 8 SC = DC THEN TALLY ~2 END ;                      00034800
                 SETUP ~ TALLY;                                         00034900
                DI ~ TAPESEQ; TAPESEQ ~ DI;                             00035000
                SI ~ LOC TAPESEQ; DI ~ LCR; DS ~ WDS END;               00035100
REAL STREAM PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;                    00035200
              BEGIN                                                     00035300
                   SI~ ACCUM; SI~SI+SKP;SI~SI+3;DI~LOC CONV;DS~N OCT    00035400
              END;                                                      00035500
 STREAM PROCEDURE MOVE(W)"WORDS FROM"(A)"TO"(B); VALUE W;               00035600
              BEGIN SI ~ A; DI ~ B; DS ~ W WDS END;                     00035700
 BOOLEAN STREAM PROCEDURE EQUAL(COUNT,A,B); VALUE COUNT;                00035800
              BEGIN TALLY ~ 1; SI~A; DI~B;                              00035900
                   IF COUNT SC = DC THEN EQUAL ~ TALLY END;             00036000
 REAL STREAM PROCEDURE EXAMIN (NCR); VALUE NCR;                         00036100
              BEGIN SI ~ NCR; DI ~ LOC EXAMIN; DI ~ DI+7; DS ~ CHR END; 00036200
 STREAM PROCEDURE SCAN(NCR,COUNTV,ACCUM,COMCOUNT,RESULT,RESULTV,        00036300
                  COUNT,ST2,NCRV,ST1);                                  00036400
              VALUE COUNTV, COMCOUNT,RESULTV,ST2,NCRV,ST1;              00036500
              BEGIN                                                     00036600
              LABEL DEBLANK,NUMBERS,IDBLDR,GNC,K,EXIT,FINIS,L,ERROR;    00036700
              DI ~ RESULT; DI ~ DI+7;                                   00036800
              SI ~ NCRV;                                                00036900
              CI ~ CI+RESULTV;                                          00037000
              GO TO DEBLANK;  COMMENT 0 IS INITIAL CODE;                00037100
              GO TO IDBLDR;   COMMENT 1 IS ID CODE;                     00037200
              GO TO FINIS;    COMMENT 2 IS SPECIAL CHARACTER CODE;      00037300
              GO TO NUMBERS;  COMMENT 3 IS NUMBER CODE;                 00037400
              GO TO FINIS;    COMMENT 4 IS ERROR CODE;                  00037500
              GO TO GNC;      COMMENT 5 IS GET NEXT CHARACTOR CODE;     00037600
                    IF SC=" " THEN                                      00037700
              BEGIN K:    SI~SI+1;                                      00037800
                    IF SC =" "  THEN GO TO K                            00037900
              END;                                                      00038000
                    GO TO FINIS;                                        00038100
    DEBLANK:  IF SC = " " THEN                                          00038200
                   BEGIN L: SI~SI+1; IF SC = " " THEN GO TO L END;      00038300
              NCRV ~ SI;                                                00038400
              IF SC } "0" THEN GO TO NUMBERS;                           00038500
              IF SC = ALPHA THEN GO TO IDBLDR;                          00038600
     GNC:     DS ~ LIT "2";                                             00038700
              TALLY ~ 1; SI ~ SI +1; GO TO EXIT;                        00038800
 IDBLDR:      TALLY ~ 63; DS ~ LIT "1";                                 00038900
              COMCOUNT( TALLY~ TALLY+1;                                 00039000
                   IF SC = ALPHA THEN SI~SI+1 ELSE JUMP OUT TO EXIT);   00039100
 ERROR:       DI~DI-1; DS ~ LIT "4"; GO TO EXIT;                        00039200
 NUMBERS:     TALLY ~ 63; DS ~ LIT "3";                                 00039300
                   COMCOUNT( TALLY~ TALLY+1;                            00039400
                   IF SC <"0"THEN JUMP OUT TO EXIT; SI~SI+1);           00039500
              GO TO ERROR;                                              00039600
 EXIT:        ST1 ~ TALLY;                                              00039700
              TALLY ~ TALLY+COUNTV; ST2 ~ TALLY;                        00039800
              DI ~ COUNT; SI ~ LOC ST2; DS ~ WDS;                       00039900
              DI ~ ACCUM; SI ~ COUNT; SI~SI+5; DS ~ 3 CHR; DI~DI+COUNTV;00040000
              SI ~ NCRV; DS ~ ST1 CHR;                                  00040100
 FINIS:       DI ~ NCR; ST1 ~ SI; SI ~ LOC ST1; DS ~ WDS END;           00040200
REAL STREAM PROCEDURE MKABS(A);  BEGIN SI ~A; MKABS~SI END;             00040300
 PROCEDURE READACARD;                                                   00040400
    BEGIN                                                               00040500
         IF SOURCETOG THEN                                              00040600
                    IF LINE[15] = 0 THEN                                00040700
                   BEGIN IF TAPETOG THEN WRITE(TTAPE,16,LINE[*]);       00040800
                   IF LISTOG THEN                                       00040900
                        BEGIN BLANKOUT(LINE[0]);                        00041000
IF NOT TIME THEN TIMEIT(BUG); TIME~ TRUE                             ;  00041100
                             WRITE(BUG[DBL],15,LINE[*]);                00041200
                        END;                                            00041300
                   END;                                                 00041400
         IF LASTUSED } 0 THEN                                           00041500
              BEGIN IF LASTUSED = 1 THEN SPACE(TAPE,1)                  00041600
                   ELSE READ(CARD);                                     00041700
                   LASTUSED ~ SETUP(TAPE,LCR,CARD(9),TAPE(9));          00041800
                   IF LASTUSED = 2 THEN                                 00041900
BEGIN READ(TAPE);                                                       00042000
                             LASTUSED ~ 0;                              00042100
                        END;                                            00042200
END ELSE BEGIN IF LASTUSED=-1 THEN READ(CARD);                          00042300
                   LCR ~ MKABS(CARD(9));                                00042400
              END;                                                      00042500
         FCR ~NCR ~ LCR-9;                                              00042600
IF EQUALBLANKSEQ(LCR,CURRSEQA[2]) THEN                                  00042700
  SEQIT(LCR,CURRSEQA[1]~CURRSEQA[1]+1);                                 00042800
         PRINT(LINE[0],FCR);                                            00042900
          LINE[15]~ 0;                                                  00043000
         LINE[0] ~ IMC ~ IMC+1;                                         00043100
         ZOT(CURRSEQ,LCR);                                              00043200
    END;                                                                00043300
REAL PROCEDURE CONVERT;                                                 00043400
              BEGIN REAL T;                                             00043500
                   T~0;                                                 00043600
                   IF COUNT-TCOUNT>8 THEN T~CONV(ACCUM[1],TCOUNT,       00043700
                         TCOUNT~COUNT-TCOUNT-8)|100000000;              00043800
                   CONVERT~T+CONV(ACCUM[1],TCOUNT,COUNT-TCOUNT)         00043900
              END;                                                      00044000
 PROCEDURE ENTERIT;                                                     00044100
              BEGIN                                                     00044200
                   IF GT1 ~((GT2 ~(COUNT+10).[42:3]) +AVAIL).LINKR      00044300
                       ! AVAIL.LINKR THEN AVAIL ~ 256 | GT1;            00044400
                   ACCUM[0] ~ ACCUM[0] & STACKHEAD [SCRAM] [35:35:13];  00044500
                   ACCUM[1] ~ ACCUM[1] & (AVAIL-PREVL) [4:40:8];        00044600
                   MOVE(GT2+1)"WORDS FROM"(ACCUM)"TO"(INFO[AVAIL.       00044700
                       LINKR,AVAIL.LINKC]);                             00044800
                   PREVL ~ STACKHEAD[SCRAM] ~ AVAIL;                    00044900
                   AVAIL ~ AVAIL+GT2+1 END;                             00045000
 PROCEDURE SCANNER;                                                     00045100
              BEGIN LABEL L;                                            00045200
          L:  SCAN(NCR,COUNT,ACCUM[1],63-COUNT,RESULT,RESULT,COUNT,     00045300
                   0,NCR,0);                                            00045400
              IF NCR = LCR THEN BEGIN                                   00045500
                   READACARD;                                           00045600
                   GO TO L END END;                                     00045700
 INTEGER PROCEDURE TABLE(P); VALUE P; INTEGER P;                        00045800
              BEGIN                                                     00045900
 LABEL PERCENT,SPECIALCHAR,COMPLETE,COLON,DOT,ATSIGN,QUOTE,IPART,       00046000
   SCANAGAIN, FPART,EPART,IDENT,ROSE,COMPOST,DOLLAR,RTPAREN;            00046100
 SWITCH  SPECIALSWITCH ~ PERCENT,DOLLAR,DOT,ATSIGN,COLON,QUOTE,RTPAREN; 00046200
 SWITCH RESULTSWITCH~  IDENT, SPECIALCHAR,IPART;                        00046300
STREAM PROCEDURE COMMENTSCAN(NCR,NCRV,T); VALUE NCRV;                   00046400
BEGIN LABEL L;                                                          00046500
SI ~ NCRV;                                                              00046600
IF SC ! ";" THEN BEGIN                                                  00046700
L: SI ~ SI+1;                                                           00046800
IF SC > "%" THEN GO TO L;                                               00046900
IF SC < ";" THEN GO TO L;                                               00047000
END;                                                                    00047100
NCRV ~ SI;                                                              00047200
DI ~ T; DI ~ DI+7; DS ~ CHR;                                            00047300
DI ~ NCR; SI ~ LOC NCRV; DS ~ WDS END;                                  00047400
                WHILE P } NXTELBT                                       00047500
                  DO BEGIN                                              00047600
 SCANAGAIN:        COUNT ~ RESULT ~ ACCUM[1] ~ 0; SCANNER;              00047700
                   IF RESULT ! 2 THEN SPENST ~ FALSE;                   00047800
                   GO TO RESULTSWITCH[RESULT];                          00047900
 SPECIALCHAR: GT1 ~ ACCUM[1].[18:6] - 2;                                00048000
ENDTOG ~ GT1 = 57 AND ENDTOG;                                           00048100
              T ~ SPECIAL[GT1&GT1[42:41:3]];                            00048200
              IF T.[1:1] = 0 THEN NST ~ TRUE;                           00048300
              IF SPENST THEN T ~  ABS(T);                               00048400
              SPENST ~ T.[1:1] = 0;                                     00048500
                   IF GT1 ~ T.INCR ! 0 THEN GO TO SPECIALSWITCH [GT1];  00048600
                GO TO COMPLETE;                                         00048700
    COLON:    RESULT~6;    SCANNER;                                     00048800
              IF EXAMIN (NCR) = "=" THEN BEGIN RESULT ~0; SCANNER;      00048900
                 T ~ SPECIAL[13] END;                                   00049000
              GO TO COMPLETE;                                           00049100
 DOT:         IF EXAMIN (NCR) >9 THEN GO TO COMPLETE;                   00049200
              C ~ 0; GO TO FPART;                                       00049300
 ATSIGN:      C ~ 1; GO TO EPART;                                       00049400
    QUOTE:    DO   BEGIN  RESULT~5; SCANNER END UNTIL EXAMIN(NCR)=""";  00049500
              RESULT~0;                                                 00049600
              SCANNER   ;                                               00049700
              Q~  ACCUM[1];                                             00049800
                   T.CLASS ~ STRNGCON ; GO TO COMPLETE;                 00049900
 DOLLAR: LINE[15] ~ 10;                                                 00050000
         COUNT ~ RESULT ~ ACCUM[1] ~ 0; SCANNER; Q ~ ACCUM[1];          00050100
         IF Q = "4TAPE0" THEN BEGIN LASTUSED ~ 0; TAPIN ~ TRUE END      00050200
         ELSE IF Q ! "4CARD0" THEN GO TO PERCENT ELSE LASTUSED ~ -1;    00050300
PAGETOG~                                                                00050400
         LISTOG ~ INDEXTOG ~ FLOWTOG ~ SOURCETOG ~ FALSE;               00050500
         DO BEGIN COUNT ~ RESULT ~ ACCUM[1] ~ 0; SCANNER; Q ~ ACCUM[1]; 00050600
              IF Q = "4LIST0" THEN LISTOG ~ TRUE ELSE                   00050700
              IF Q = "5INDEX" THEN INDEXTOG ~ TRUE ELSE                 00050800
              IF Q = "4FLOW0" THEN FLOWTOG ~ TRUE ELSE                  00050900
              IF Q = "6SOURC" THEN SOURCETOG ~ TRUE ELSE                00051000
IF Q="4PAGE0" THEN PAGETOG~ TRUE ELSE                                   00051100
              IF Q = "4TAPE0" THEN TAPETOG ~ TRUE ELSE GO TO PERCENT    00051200
        END UNTIL FALSE;                                                00051300
 PERCENT:     READACARD;                                                00051400
              GO TO SCANAGAIN;                                          00051500
    RTPAREN:RESULT~6;    SCANNER;                                       00051600
              IF EXAMIN(NCR)="""  THEN                                  00051700
              BEGIN                                                     00051800
                RESULT~0;   SCANNER;                                    00051900
                DO BEGIN RESULT~5;  SCANNER END UNTIL EXAMIN(NCR)=""";  00052000
                RESULT~0;    SCANNER;                                   00052100
                RESULT~6;    SCANNER;                                   00052200
                IF EXAMIN(NCR) !"("   THEN                              00052300
                  BEGIN                                                 00052400
                  END;                                                  00052500
                RESULT~0;    SCANNER;    Q~ACCUM[1];                    00052600
                T~SPECIAL[24]                                           00052700
              END;                                                      00052800
              GO TO COMPLETE;                                           00052900
    IPART:         TCOUNT~0;                                            00053000
                   C~CONVERT;                                           00053100
                   IF   EXAMIN(NCR)="." THEN                            00053200
              BEGIN RESULT~0;                                           00053300
                   SCANNER;                                             00053400
    FPART:         TCOUNT~COUNT;                                        00053500
                   IF    EXAMIN(NCR){9 THEN                             00053600
              BEGIN RESULT~0;                                           00053700
                   SCANNER;                                             00053800
                   C~C+CONVERT|TEN[TCOUNT-COUNT];                       00053900
              END                                                       00054000
              END;                                                      00054100
                   IF EXAMIN(NCR)="@" THEN                              00054200
              BEGIN RESULT~0;                                           00054300
                   SCANNER;                                             00054400
    EPART:         TCOUNT~COUNT;                                        00054500
                   IF T~EXAMIN(NCR)>9 THEN                              00054600
              BEGIN RESULT~0;                                           00054700
                   SCANNER;                                             00054800
                   TCOUNT~COUNT;                                        00054900
              END;  RESULT~0;                                           00055000
                   SCANNER;                                             00055100
                   C~C|TEN[IF T="-" THEN -CONVERT ELSE CONVERT]         00055200
              END;                                                      00055300
              Q~  ACCUM[1];                                             00055400
                   T ~0;                                                00055500
                   T.CLASS ~ NONLITNO;  GO TO COMPLETE;                 00055600
        IDENT:     T ~ STACKHEAD[SCRAM ~ (Q~ACCUM[1]) MOD 125];         00055700
        ROSE :     GT1 ~ T.LINKR;                                       00055800
                   IF(GT2 ~ T.LINKC)+GT1= 0 THEN                        00055900
                     BEGIN T ~ 0; GO TO COMPLETE END;                   00056000
                   T ~ INFO[GT1,GT2];                                   00056100
                   IF INFO[GT1,GT2+1]&0[1:1:11] ! Q THEN GO TO ROSE;    00056200
                   IF COUNT { 5 THEN GO TO COMPOST ;                    00056300
                   IF NOT EQUAL(COUNT-5,ACCUM[2],INFO[GT1,GT2+2])       00056400
                     THEN GO TO ROSE;                                   00056500
        COMPOST:   T ~ T&GT1[35:43:5]&GT2[40:40:8];                     00056600
IF NOT ENDTOG THEN                                                      00056700
                   IF T.CLASS = COMMENTV THEN BEGIN                     00056800
GT1 ~ 0;                                                                00056900
DO BEGIN COMMENTSCAN(NCR,NCR,GT1); IF NCR = LCR THEN READACARD END UNTIL00057000
 GT1 =";";                                                              00057100
              RESULT~0;    SCANNER;                                     00057200
                        GO TO SCANAGAIN END ELSE                        00057300
 IF T.CLASS = BEGINV THEN                                               00057400
    BEGIN BEGINARRAY[DELTA] ~ IMC;                                      00057500
         DELTA ~ DELTA+1 END;                                           00057600
 COMPLETE:    NONSPACERTOG ~ T.[1:1] = 0;                               00057700
              ELBAT[NXTELBT] ~ ABS(T);                                  00057800
              IF NXTELBT ~ NXTELBT+1 > 74                               00057900
                THEN BEGIN                                              00058000
                   MOVE(10,ELBAT[65],ELBAT);                            00058100
                   I~ I-65;                                             00058200
                   P~ P-65;                                             00058300
                   NXTELBT ~ 10 END END;                                00058400
              ELBAT[NXTELBT+1] ~ REAL( NOT FALSE);                      00058500
              TABLE ~ ELBAT[P].CLASS END;                               00058600
         INTEGER IDCTR;                                                 00058700
         FORMAT FC(I8,A8,"  CLASS=",I3,";  ADDRESS =",I5,";  INCR =",I4,00058800
                   ";  LINK =",I5,".");                                 00058900
 REAL PROCEDURE TAKE(L); VALUE L; REAL L;                               00059000
              TAKE ~ INFO[L.LINKR,L.LINKC];                             00059100
 REAL PROCEDURE STEPI;                                                  00059200
              STEPI ~ ELCLASS ~ TABLE(I~I+1);                           00059300
 PROCEDURE STEPIT; ELCLASS ~ TABLE(I~I+1);                              00059400
 PROCEDURE PUT(A)"INTO"(B); VALUE A,B; REAL A,B;                        00059500
              INFO[B.LINKR,B.LINKC] ~ A;                                00059600
 PROCEDURE PUTSEQ(S,I); VALUE I; REAL S,I;                              00059700
              MOVE(1,S,INFO[I.LINKR,I.LINKC]);                          00059800
 REAL TYPE; INTEGER INDENT; DEFINE IDMAX = LABELID#;                    00059900
                PROCEDURE ENDCOMMENT; BEGIN ENDTOG~TRUE;                00060000
         DELTA ~ DELTA-1;                                               00060100
         A[0] ~ BEGINARRAY[DELTA];                                      00060200
         MOVE(1,CURRSEQ,A[1]);                                          00060300
              WRITE(CODAL,2,A[*]);                                      00060400
DO UNTIL STEPI } UNTILV AND ELCLASS { ENDV OR NOT ENDTOG; ENDTOG~FALSE  00060500
END;                                                                    00060600
 ARRAY HOLDER[0:20];                                                    00060700
 DEFINE                                                                 00060800
    QA = HOLDER[ 0]#,                                                   00060900
    QB = HOLDER[ 1]#,                                                   00061000
    QC = HOLDER[ 2]#,                                                   00061100
    QD = HOLDER[ 3]#,                                                   00061200
    QE = HOLDER[ 4]#,                                                   00061300
    QF = HOLDER[ 5]#,                                                   00061400
    QG = HOLDER[ 6]#,                                                   00061500
    QH = HOLDER[ 7]#,                                                   00061600
    QI = HOLDER[ 8]#,                                                   00061700
    QJ = HOLDER[10]#,                                                   00061800
    QK = HOLDER[11]#,                                                   00061900
   QL = HOLDER[12]#,                                                    00062000
   QM = HOLDER[13]#,                                                    00062100
   QN = HOLDER[15]#,                                                    00062200
    QP = HOLDER[17]#,                                                   00062300
    QQ = HOLDER[18]#;                                                   00062400
 REAL PROCEDURE MAX(A,B); VALUE A,B; REAL A,B;                          00062500
    MAX ~ IF A > B THEN A ELSE B;                                       00062600
 REAL PROCEDURE MIN(A,B); VALUE A,B; REAL A,B;                          00062700
    MIN ~ IF A < B THEN A ELSE B;                                       00062800
 ARRAY FLOWARRAY[0:13];                                                 00062900
 REAL FLOWCTR,LL,RL,LLBOO,RLBOO,CENTER,OLL,ORL,LINEPLACE,OLDLINEPLACE;  00063000
 BOOLEAN CENTERTOG,BLANKTOG,STRMTOG,BOOTOG;                             00063100
REAL FUDGE;                                                             00063200
SAVE ARRAY LINEACCUM[0:1022];                                           00063300
 PROCEDURE WRITERECORD(LINES,L,R,Q,N); VALUE LINES,Q,N;                 00063400
    BOOLEAN LINES; REAL L,R,Q,N;                                        00063500
   BEGIN                                                                00063600
    STREAM PROCEDURE MOVECHAR(L,Q,N,F); VALUE Q,N;                      00063700
         BEGIN LOCAL Q1;                                                00063800
              DI ~ LOC Q1; DI ~ DI+7;                                   00063900
              SI ~ LOC Q;  SI ~ SI+6; DS ~ CHR;                         00064000
              SI ~ L; Q1(SI ~ SI+32;SI~SI+32;); SI ~ SI+Q;              00064100
              DI ~ F; DS ~ N CHR;                                       00064200
         END;                                                           00064300
        IF DOKLUDGE THEN R ~ RLBOO ~ MAX(R,N DIV 2 + CENTER + 4|RL - 1);00064400
      FLOWARRAY[0] ~ CENTER-(N-1) DIV 2-1;                              00064500
      FLOWARRAY[1] ~ N;                                                 00064600
         IF LINES THEN BEGIN                                            00064700
      FLOWARRAY[2] ~ LLBOO;                                             00064800
      FLOWARRAY[3] ~ RLBOO;                                             00064900
      IF LLBOO } 0 THEN FLOWARRAY[2] ~+(LL~LL-1)|4+LLBOO+1;             00065000
      IF RLBOO } 0 THEN FLOWARRAY[3] ~-(RL~RL-1)|4+RLBOO-1;             00065100
         END;                                                           00065200
    IF FLOWTOG THEN BEGIN                                               00065300
          IF LINES THEN                                                 00065400
      FLOWARRAY[4] ~ REAL(LL!OLL)|2+REAL(RL!ORL);                       00065500
         MOVECHAR(LINEACCUM[0],Q,N,FLOWARRAY[5]);                       00065600
WRITE (FLOW, 14, FLOWARRAY[*]);                                         00065700
      FLOWCTR ~ FLOWCTR+1;                                              00065800
    END;                                                                00065900
         IF LINES THEN      BEGIN                                       00066000
      OLL ~ LL; ORL ~ RL;                                               00066100
      LLBOO ~ RLBOO ~ -1;                                               00066200
           END;                                                         00066300
      L ~ MIN(L,CENTER-(N-1) DIV 2 - 4|LL-1);                           00066400
      R ~ MAX(R,CENTER+N DIV 2 + 4|RL-1);                               00066500
   END WRITERECORD;                                                     00066600
 PROCEDURE WRITELINE(LMAR,RMAR); REAL LMAR,RMAR;                        00066700
    BEGIN INTEGER T1,T2,Q;                                              00066800
         DEFINE M = T1#,N = T2#;                                        00066900
         REAL STREAM PROCEDURE FIND(L,Q,M); VALUE Q,M;                  00067000
              BEGIN LOCAL Q1; LABEL K;                                  00067100
                   DI ~ LOC Q1; DI ~ DI+7;                              00067200
                   SI ~ LOC Q; SI ~ SI+6; DS ~ CHR;                     00067300
                   TALLY ~ 0; SI ~ L; SI ~ SI-1;                        00067400
                   Q1(SI ~ SI+32; SI ~ SI+32); SI ~ SI+Q; SI~ SI+M;     00067500
              K:   IF SC = ALPHA THEN                                   00067600
                        BEGIN TALLY ~ TALLY+1; SI ~ SI-1; GO TO K END;  00067700
                   FIND ~ TALLY END;                                    00067800
       IF CENTERTOG THEN                                                00067900
          BEGIN CENTERTOG ~ FALSE;                                      00068000
             T1 ~ (MAX(LINEPLACE,OLDLINEPLACE)-REAL(BOOTOG)|12) DIV 8;  00068100
             BOOTOG ~ FALSE;                                            00068200
             T2 ~ 2|(LL-RL)+60-CENTER;                                  00068300
             CENTER ~ MIN(T1,ABS(T2))|SIGN(T2)+CENTER                   00068400
          END;                                                          00068500
       IF BLANKTOG THEN                                                 00068600
          BEGIN CENTER ~ 2|(LL-RL+FUDGE)+60;                            00068700
             LMAR ~ MIN(LMAR,CENTER+4|LL);                              00068800
             RMAR ~ MAX(RMAR,CENTER-4|RL);                              00068900
          END;                                                          00069000
             FUDGE ~ 0;                                                 00069100
   M ~ MIN(120-4|(LL+RL),56);                                           00069200
    M ~ MIN(LINEPLACE DIV (LINEPLACE DIV M + 1) +5,M);                  00069300
    DO BEGIN IF Q+M < LINEPLACE THEN                                    00069400
         BEGIN N ~-FIND(LINEACCUM,Q,M)+M;                               00069500
              IF N { 0 THEN N ~ M;                                      00069600
              FLOWARRAY[2] ~ FLOWARRAY[3] ~ -1;                         00069700
              FLOWARRAY[4] ~ 0;                                         00069800
              WRITERECORD(FALSE,LMAR,RMAR,Q,N);                         00069900
              Q ~ Q+N;                                                  00070000
         END ELSE BEGIN                                                 00070100
                  WRITERECORD(TRUE,LMAR,RMAR,Q,LINEPLACE-Q);            00070200
                     Q ~ Q+M END END UNTIL Q } LINEPLACE;               00070300
      BLANKTOG ~ FALSE;                                                 00070400
       OLDLINEPLACE ~ LINEPLACE;                                        00070500
       LINEPLACE ~ 0;                                                   00070600
   END WRITELINE;                                                       00070700
 PROCEDURE PUSH(Q); REAL Q;                                             00070800
    BEGIN                                                               00070900
       REAL STREAM PROCEDURE P(Q,N,L); VALUE N;                         00071000
          BEGIN LOCAL N1,C;                                             00071100
             DI ~ LOC N1; DI ~ DI+7; SI ~ LOC N; SI ~ SI+6; DS ~ CHR;   00071200
             SI ~ Q; SI ~ SI+2; DI ~ LOC P; DI ~ DI+7; DS ~ CHR;        00071300
             DI ~ L; N1(DI ~ DI+32; DI ~ DI+32); DI ~ DI+N; DS ~ P CHR; 00071400
          END;                                                          00071500
IF LINEPLACE+Q.[12:6]<8184 THEN                                         00071600
       LINEPLACE ~ P(Q,LINEPLACE,LINEACCUM)+LINEPLACE;                  00071700
    END PUSH;                                                           00071800
 PROCEDURE PUSHANDWR(Q,L,R); REAL Q,L,R;                                00071900
    BEGIN PUSH(Q); WRITELINE(L,R); END PUSHANDWR;                       00072000
 PROCEDURE ORDINARY(S); VALUE S; REAL S;                                00072100
    BEGIN REAL ELSECTR;  LABEL L;                                       00072200
REAL PCOUNT;                                                            00072300
       WHILE(ELCLASS < UNTILV OR ELCLASS > SEMICOLON)                   00072400
AND ELCLASS!S OR PCOUNT>0 DO                                            00072500
   L:     BEGIN PUSH(ACCUM[1]);                                         00072600
             IF ELCLASS = IFV THEN ELSECTR ~ ELSECTR+1;                 00072700
IF ELCLASS=FORV THEN PCOUNT~PCOUNT+1 ELSE IF ELCLASS=DOV THEN PCOUNT~   00072800
PCOUNT-1;                                                               00072900
              NST ~ NONSPACERTOG;                                       00073000
              STEPIT;                                                   00073100
              IF NST AND NONSPACERTOG THEN PUSH(QE);                    00073200
          END;                                                          00073300
         IF ELCLASS = ELSEV THEN ELSECTR ~ ELSECTR-1;                   00073400
              IF ELSECTR } 0AND ELCLASS = ELSEV THEN      GO TO L;      00073500
IF NOT(NST AND NONSPACERTOG) AND LINEPLACE ! 0 THEN PUSH(QE);           00073600
    END ORDINARY;                                                       00073700
 PROCEDURE TIL(V); VALUE V; REAL V;                                     00073800
    BEGIN REAL ELSECTR;                                                 00073900
    DO BEGIN                                                            00074000
       WHILE ELCLASS ! V DO                                             00074100
          BEGIN PUSH(ACCUM[1]);                                         00074200
              NST ~ NONSPACERTOG;                                       00074300
              STEPIT;                                                   00074400
              IF NST AND NONSPACERTOG THEN PUSH(QE);                    00074500
              IF ELCLASS = IFV THEN ELSECTR ~ ELSECTR+1;                00074600
              IF ELCLASS = ELSEV THEN ELSECTR ~ ELSECTR-1;              00074700
          END;                                                          00074800
       ELCLASS ~ 0;                                                     00074900
    END UNTIL ELSECTR = 0 ;                                             00075000
       STEPIT;                                                          00075100
     END TIL;                                                           00075200
 PROCEDURE STMT(L,R); REAL L,R; FORWARD;                                00075300
 PROCEDURE BOOSTMT(L,R,V); VALUE V; REAL L,R,V;                         00075400
    BEGIN                                                               00075500
      STEPIT;                                                           00075600
      PUSH(QI);                                                         00075700
      TIL(V);                                                           00075800
      RL ~ RL+1;                                                        00075900
      CENTERTOG ~ BOOTOG ~ TRUE;                                        00076000
      PUSHANDWR(QC,L,R);                                                00076100
      CENTERTOG ~ BOOTOG ~ TRUE;                                        00076200
      PUSHANDWR(QB,L,R);                                                00076300
      PUSHANDWR(QJ,L,R);                                                00076400
      PUSHANDWR(QB,L,R);                                                00076500
      STMT(L,R);                                                        00076600
    END BOOSTMT;                                                        00076700
 PROCEDURE WHILESTMT(L,R); REAL L,R;                                    00076800
    BEGIN REAL TL,TR,T;                                                 00076900
      LL ~ LL+1;      TL ~ 120;                                         00077000
       PUSHANDWR(QK,TL,TR);                                             00077100
       PUSHANDWR(QB,TL,TR);                                             00077200
       BOOSTMT(TL,TR,DOV);                                              00077300
       LLBOO ~ TL;                                                      00077400
       PUSHANDWR(QB,TL,TR);                                             00077500
       FUDGE ~ REAL(BLANKTOG ~ TRUE);                                   00077600
       PUSHANDWR(QE,TL,TR);                                             00077700
       RLBOO ~ TR;                                                      00077800
       PUSHANDWR(QB,TL,TR);                                             00077900
       PUSHANDWR(QB,TL,TR);                                             00078000
       PUSHANDWR(QH,TL,TR);                                             00078100
       L ~ MIN(L,TL);                                                   00078200
       R ~ MAX(R,TR);                                                   00078300
    END WHILESTMT;                                                      00078400
 PROCEDURE FILLSTMT(L,R); REAL L,R;                                     00078500
    BEGIN TIL(WITHV);                                                   00078600
       WRITELINE(L,R);                                                  00078700
       PUSHANDWR(QB,L,R);                                               00078800
       I ~ I-1;                                                         00078900
DO IF STEPI=ADOP THEN STEPIT UNTIL STEPI}UNTILV AND ELCLASS{SEMICOLON;  00079000
    END FILLSTMT;                                                       00079100
 PROCEDURE FORSTMT(L,R); REAL L,R;                                      00079200
    BEGIN REAL TL,TR;                                                   00079300
       REAL C;                                                          00079400
       TL ~ 120;                                                        00079500
       TIL(DOV); CENTERTOG ~ TRUE;                                      00079600
       WRITELINE(L,R);                                                  00079700
       CENTERTOG ~ TRUE;                                                00079800
       PUSHANDWR(QB,L,R);                                               00079900
       RL ~ RL+1; LL ~ LL+1;                                            00080000
       PUSHANDWR(QB,TL,TR);                                             00080100
       PUSHANDWR(QB,TL,TR);                                             00080200
       C ~ CENTER;                                                      00080300
       STMT(TL,TR);                                                     00080400
       LLBOO ~ TL; RLBOO ~ TR;                                          00080500
       PUSHANDWR(QB,TL,TR);                                             00080600
       CENTER ~ C;                                                      00080700
       PUSHANDWR(QB,TL,TR);                                             00080800
       L ~ MIN(L,TL);                                                   00080900
       R ~ MAX(R,TR);                                                   00081000
    END FORSTMT;                                                        00081100
 PROCEDURE LOOPSTMT(L,R); REAL L,R;                                     00081200
    BEGIN LABEL EL;                                                     00081300
       REAL TL,TR,C;                                                    00081400
       TL ~ 120;                                                        00081500
       PUSH(QL);                                                        00081600
       PUSH(ACCUM[1]);                                                  00081700
       PUSH(QE);                                                        00081800
       CENTERTOG ~ TRUE;                                                00081900
       PUSHANDWR(QM,L,R);                                               00082000
       CENTERTOG ~ TRUE;                                                00082100
       PUSHANDWR(QB,L,R);                                               00082200
       LL ~ LL+1; RL ~ RL+1;                                            00082300
       PUSHANDWR(QB,TL,TR);                                             00082400
       PUSHANDWR(QB,TL,TR);                                             00082500
       C ~ CENTER;                                                      00082600
       STEPIT; STEPIT;                                                  00082700
       STEPIT;                                                          00082800
       LLBOO ~ TL; RLBOO ~ TR;                                          00082900
       PUSHANDWR(QB,TL,TR);                                             00083000
       CENTER ~ C;                                                      00083100
       PUSHANDWR(QB,L,R);                                               00083200
       L ~ MIN(L,TL);                                                   00083300
       R ~ MAX(R,TR);                                                   00083400
    END LOOPSTMT;                                                       00083500
 PROCEDURE DOSTMT(L,R); REAL L,R;                                       00083600
    BEGIN REAL TL,TR;                                                   00083700
       TL ~ 120;                                                        00083800
       STEPIT;                                                          00083900
       RL ~ RL+1;                                                       00084000
       PUSHANDWR(QA,TL,TR);                                             00084100
       PUSHANDWR(QB,TL,TR);                                             00084200
       STMT(TL,TR);                                                     00084300
       STEPIT;                                                          00084400
       PUSH(QI);                                                        00084500
       ORDINARY(300);                                                   00084600
       PUSH(QC);                                                        00084700
       RLBOO ~ TR; DOKLUDGE ~ TRUE;                                     00084800
       WRITELINE(TL,TR);                                                00084900
       CENTERTOG ~ BOOTOG ~ TRUE;                                       00085000
       DOKLUDGE  ~ FALSE ;                                              00085100
       PUSHANDWR(QB,L,R);                                               00085200
       PUSHANDWR(QJ,L,R);                                               00085300
       PUSHANDWR(QB,L,R);                                               00085400
       L ~ MIN(L,TL);                                                   00085500
       R ~ MAX(R,TR);                                                   00085600
    END BOSTMT;                                                         00085700
 PROCEDURE COMPOUNDTAIL(L,R); REAL L,R;                                 00085800
    BEGIN LABEL EL;                                                     00085900
 EL:   STMT(L,R);                                                       00086000
       IF ELCLASS = SEMICOLON THEN                                      00086100
          BEGIN STEPIT;                                                 00086200
             GO TO EL                                                   00086300
          END;                                                          00086400
       ENDCOMMENT;                                                      00086500
    END;                                                                00086600
 PROCEDURE LABELR(L,R); REAL L,R;                                       00086700
    BEGIN                                                               00086800
      IF NOT STRMTOG THEN                                               00086900
         BEGIN GT1 ~ TAKE(ELBAT[I]);                                    00087000
            GT1 ~ GT1.INCR+ELBAT[I].LINK;                               00087100
              PUTSEQ(CURRSEQ,GT1);                                      00087200
         END;                                                           00087300
      PUSH(ACCUM[1]);                                                   00087400
      STEPIT;                                                           00087500
      PUSH(ACCUM[1]);                                                   00087600
      STEPIT;                                                           00087700
      PUSH(QN);                                                         00087800
      GT4 ~ LINEPLACE;                                                  00087900
      FOR GT1 ~ 2 STEP 1 UNTIL GT4 DO PUSH(QE);                         00088000
      WRITELINE(L,R);                                                   00088100
      PUSHANDWR(QB,L,R);                                                00088200
    END LABELR;                                                         00088300
 PROCEDURE IFSTMT(L,R); REAL L,R;                                       00088400
    BEGIN REAL TL1,TR1,TL2,TR2;                                         00088500
      BOOLEAN B;                                                        00088600
       TL1 ~ TL2 ~ 120;                                                 00088700
       BOOSTMT(TL1,TR1,THENV);                                          00088800
       IF ELCLASS ! ELSEV AND NOT BLANKTOG THEN                         00088900
          BEGIN RLBOO ~ TR1;                                            00089000
             PUSHANDWR(QA,TL1,TR1);                                     00089100
          END                                                           00089200
       ELSE BEGIN                                                       00089300
             B ~ NOT BLANKTOG;                                          00089400
          IF NOT BLANKTOG THEN                                          00089500
             BEGIN LL ~ LL+1;                                           00089600
                PUSHANDWR(QB,TL1,TR1);                                  00089700
                TL2 ~ CENTER -4|LL+1;                                   00089800
             END;                                                       00089900
       FUDGE ~ REAL(BLANKTOG ~ TRUE);                                   00090000
              PUSHANDWR(QE,TL2,TR1);                                    00090100
              RLBOO ~ TR1;                                              00090200
              PUSHANDWR(QB,TL1,TR1);                                    00090300
              PUSHANDWR(QB,TL2,TR2);                                    00090400
              PUSHANDWR(QH,TL2,TR2);                                    00090500
              IF ELCLASS = ELSEV THEN                                   00090600
                BEGIN STEPIT;                                           00090700
                   STMT(TL2,TR2);                                       00090800
                      IF B THEN BEGIN                                   00090900
                   LLBOO ~ TL2;                                         00091000
                   PUSHANDWR(QK,TL2,TR2);                               00091100
                      END;                                              00091200
                END;                                                    00091300
              TL1 ~ MIN(TL1,TL2);                                       00091400
              TR1 ~ MAX(TR1,TR2);                                       00091500
      END;                                                              00091600
      IF NOT BLANKTOG THEN PUSHANDWR(QB,TL1,TR1);                       00091700
      L ~ MIN(L,TL1);                                                   00091800
      R ~ MAX(R,TR1);                                                   00091900
   END IFSTMT;                                                          00092000
 PROCEDURE GOSTMT(L,R); REAL L,R;                                       00092100
    BEGIN REAL T,T1,T2,GT1;                                             00092200
 REAL STREAM PROCEDURE CONVERT(N); VALUE N;                             00092300
     BEGIN SI ~ LOC N; DI ~ LOC N; DS ~ 8 DEC;                          00092400
           DI ~ LOC CONVERT; DI ~ DI+2; DS ~ LIT "2";                   00092500
           SI ~ LOC N; SI ~ SI+6;                                       00092600
           IF SC = "0" THEN                                             00092700
              BEGIN DS ~ LIT " "; SI ~ SI+1; DS ~ CHR END               00092800
           ELSE DS ~ 2 CHR;                                             00092900
     END;                                                               00093000
       IF STEPI = TOV THEN STEPIT;                                      00093100
       IF ELCLASS ! SWITCHID THEN                                       00093200
          BEGIN PUSHANDWR(QB,L,R);                                      00093300
             PUSH(QF);                                                  00093400
             ORDINARY(300);                                             00093500
             PUSHANDWR(QG,L,R);                                         00093600
             PUSHANDWR(QE,L,R);                                         00093700
             PUSHANDWR(QE,L,R);                                         00093800
             BLANKTOG ~ TRUE;                                           00093900
          END ELSE BEGIN T ~ ELBAT[I].LINK+TAKE(ELBAT[I]).INCR+2;       00094000
        PUSH(ACCUM[1]); STEPIT; T2~ 1;                                  00094100
             ORDINARY(300);                                             00094200
             CENTERTOG ~ TRUE;                                          00094300
             WRITELINE(L,R);                                            00094400
             CENTERTOG ~ TRUE;                                          00094500
             WHILE GT1 ~ TAKE(T) ! 0 DO                                 00094600
                BEGIN PUSHANDWR(QB,L,R);                                00094700
                   PUSH(QF);                                            00094800
                   PUSH(INFO[GT1.LINKR, GT1.LINKC+1]);                  00094900
                   PUSH(QP);                                            00095000
                   GT1 ~ CONVERT(T2);                                   00095100
                   PUSH(GT1);                                           00095200
                   GT4 ~ LINEPLACE;                                     00095300
                   FOR GT1 ~ 8 STEP 1 UNTIL GT4 DO                      00095400
                      PUSH(QE);                                         00095500
                   WRITELINE(L,R);                                      00095600
                   T ~ T+1; T2 ~ T2+1;                                  00095700
                   PUSHANDWR(QB,L,R);                                   00095800
                END;                                                    00095900
             PUSHANDWR(QB,L,R);                                         00096000
             PUSHANDWR(QH,L,R);                                         00096100
          END END GOSTMT;                                               00096200
 PROCEDURE BLOCK(A,B,C,D); VALUE A,B; REAL A,C,D; BOOLEAN B; FORWARD;   00096300
 PROCEDURE STMT(L,R); REAL L,R;                                         00096400
    BEGIN LABEL AGAIN,EL,EXIT;                                          00096500
       REAL T;                                                          00096600
 AGAIN:   IF ELCLASS = IFV THEN BEGIN IFSTMT(L,R); GO EXIT END;         00096700
          IF ELCLASS = GOV THEN BEGIN GOSTMT(L,R); GO EXIT END;         00096800
          IF ELCLASS = DOV THEN BEGIN DOSTMT(L,R); GO EXIT END;         00096900
          IF ELCLASS = WHILEV THEN BEGIN WHILESTMT(L,R); GO EXIT END;   00097000
          IF ELCLASS = FORV THEN BEGIN FORSTMT(L,R); GO EXIT END;       00097100
          IF ELCLASS = FILLV THEN BEGIN FILLSTMT(L,R); GO EXIT END;     00097200
          IF ELCLASS = LABELID THEN                                     00097300
 EL:         BEGIN LABELR(L,R); GO AGAIN END;                           00097400
          IF ELCLASS = BEGINV THEN                                      00097500
             BEGIN IF TABLE(I+1) = DECLARATOR THEN                      00097600
                IF TABLE(I-1) = COLON THEN                              00097700
                   BEGIN PUT(TAKE(ELBAT[I-2])&2[16:37:11],ELBAT[I-2]);  00097800
                      BLOCK(ELBAT[I-2],FALSE,L,R);                      00097900
                   END ELSE BLOCK(0,FALSE,L,R)                          00098000
                ELSE BEGIN STEPIT; COMPOUNDTAIL(L,R) END;               00098100
                GO TO EXIT END;                                         00098200
          IF STRMTOG THEN                                               00098300
             BEGIN RESULT ~ 6;                                          00098400
                SCANNER;                                                00098500
                IF GT1 ~ EXAMIN(NCR) = ":" THEN GO TO EL;               00098600
                IF GT1 = "(" AND ELCLASS ! RELEASEV THEN                00098700
                   BEGIN LOOPSTMT(L,R); GO EXIT END;                    00098800
                T ~ ELCLASS;                                            00098900
                ORDINARY(RTPAREN);                                      00099000
                IF T = RELEASEV THEN                                    00099100
                   BEGIN PUSH(ACCUM[1]); STEPIT; PUSH(QE) END;          00099200
             END ELSE ORDINARY(300);                                    00099300
          IF LINEPLACE ! 0 THEN                                         00099400
             BEGIN LINEPLACE ~ LINEPLACE-1;                             00099500
                WRITELINE(L,R);                                         00099600
                PUSHANDWR(QB,L,R) END;                                  00099700
 EXIT: END STMT;                                                        00099800
REAL                                                                    00099900
 STREAM PROCEDURE PRINTNDX(ALF,TYPE,SEQ,LINE,INDENT);                   00100000
              BEGIN                                                     00100100
              LOCAL L,A;                                                00100200
       DI~ INDENT; SI ~ LINE; DS ~ WDS; SI ~ SEQ; DS~WDS; L ~ DI;       00100300
                   DI ~ LOC  A; DI ~ DI+7; SI~ ALF; SI~ SI+10; DS ~CHR; 00100400
TALLY~TALLY+A;                                                          00100500
                   DI ~ L; DS ~ A CHR; DS ~ 1 LIT "("; L ~ DI;          00100600
                   DI ~ LOC A; DI ~DI+7; SI~TYPE; SI~ SI+10; DS ~ CHR;  00100700
TALLY~TALLY+A;                                                          00100800
                   DI ~ L; DS ~ A CHR; DS ~ 1 LIT ")";                  00100900
   TALLY ~ TALLY+2; PRINTNDX~ TALLY END;                                00101000
 PROCEDURE PURGE(BACKTO,P,PRNT); VALUE BACKTO,P,PRNT;                   00101100
              REAL BACKTO,P; BOOLEAN PRNT;                              00101200
              BEGIN                                                     00101300
PROCEDURE ALPHALATE(P); VALUE P; REAL P;                                00101400
BEGIN REAL STREAM PROCEDURE COMPARE(A,B,COUNT); VALUE COUNT;            00101500
              BEGIN SI ~A; SI~SI+11; DI~B;DI~DI+11;                     00101600
                 IF COUNT SC < DC THEN TALLY~1 ELSE BEGIN SI~SI-COUNT;  00101700
                       DI~DI-COUNT; IF COUNT SC = DC THEN TALLY ~2 END ;00101800
                 COMPARE~TALLY END;                                     00101900
              REAL CP,C,CG,GL,PGL,B; LABEL L;                           00102000
                 PGL~4; GL~ TAKE(PGL).LINK;                             00102100
              CP ~ TAKE(P.LINK+1).[12:6] ;                              00102200
         L:   CG~ TAKE(GL+1).[12:6];                                    00102300
              C ~ IF CP > CG THEN CG ELSE CP;                           00102400
              B ~ COMPARE(INFO[P.LINKR,P.LINKC],                        00102500
                       INFO[GL.LINKR,GL.LINKC],C);                      00102600
              IF B = 1 OR ( B=2 AND CP < CG) THEN                       00102700
                   BEGIN PUT(TAKE(PGL)&P [35:35:13],PGL);               00102800
                         PUT(TAKE(P  )&GL[35:35:13],P  );   END         00102900
                   ELSE BEGIN PGL ~ GL; GL ~ TAKE(GL).LINK; GO TO L;    00103000
              END END ;                                                 00103100
                    FILL INFO  [0,*] WITH OCT20040000,OCT21400000000,0, 00103200
                     "5BLOCK",6,"1 0000",0,OCT21400000000;              00103300
                 GT1 ~ TAKE(P);                                         00103400
                 GT2 ~ GT1.INCR+P.LINK;                                 00103500
               IF PREVL > BACKTO THEN BEGIN                             00103600
              IF PRNT AND INDEXTOG THEN                                 00103700
       BEGIN   PUT(RECORDCOUNT,GT2+1);  RCF~RECORDCOUNT; END;           00103800
                 WHILE PREVL > BACKTO DO BEGIN                          00103900
                   GT1 ~ TAKE(PREVL);                                   00104000
                   GT2 ~ GT1.INCR+ PREVL;                               00104100
                   IF PRNT THEN                                         00104200
                        IF INDEXTOG THEN ALPHALATE(PREVL);              00104300
                   GT2 ~ TAKE(PREVL+1);                                 00104400
                   GT3 ~ GT2.[4:8];                                     00104500
                   GT2 ~ GT2&0[1:1:11];                                 00104600
                   STACKHEAD[GT2 MOD 125] ~ GT1.LINK;                   00104700
                   AVAIL ~ PREVL;                                       00104800
                   PREVL ~ PREVL - GT3  END;                            00104900
         IF INDEXTOG THEN                                               00105000
              IF PRNT THEN BEGIN J ~4; J~TAKE(J);                       00105100
                  WHILE TAKE(J).LINK ! 0 DO BEGIN GT1~ TAKE(J);         00105200
                      GT4~TAKE(GT1).LINK;                               00105300
                    GT2~GT1.INCR+J;                                     00105400
     GT3~            PRINTNDX(                                          00105500
                    INFO[J.LINKR,J.LINKC],                              00105600
                   INFO[GT1.[16:3],GT1.[19: 8]],                        00105700
                   INFO[GT2.LINKR,GT2.LINKC],                           00105800
          INFO[(GT2+1).LINKR,(GT2+1).LINKC],A[1]);                      00105900
      A[0] ~ GT3+512|GT4; RECORDCOUNT ~ RECORDCOUNT+1;                  00106000
WRITE(CODE, 20, A[*]);                                                  00106100
               J~GT1.LINK;                                              00106200
     END END ;                                                          00106300
                 INDENT ~ INDENT-4 END;      END;                       00106400
 PROCEDURE ENTER;                                                       00106500
              BEGIN                                                     00106600
                ACCUM[0] ~ STACKHEAD[SCRAM]&(COUNT+18)[27:37:8]         00106700
                   &1 [2:41:7] & TYPE[16:37:11];                        00106800
                ENTERIT;                                                00106900
              PUTSEQ(CURRSEQ,AVAIL);                                    00107000
                  PUT(0,AVAIL+1);                                       00107100
                AVAIL ~ AVAIL +2 END;                                   00107200
 PROCEDURE WRITEFLOW(V); VALUE V; REAL V;                               00107300
    BEGIN REAL I,A;                                                     00107400
       DEFINE FINAL = LINEACCUM#, MASK = LINEACCUM[16]#;                00107500
       REAL C,N;                                                        00107600
       BOOLEAN L,R;                                                     00107700
       STREAM PROCEDURE CENTER(C,N,FLOW); VALUE C,N;                    00107800
          BEGIN DI ~ C; SI ~ FLOW; DS ~ N CHR END;                      00107900
       STREAM PROCEDURE PLACE(P,W); VALUE P,W;                          00108000
          BEGIN DI ~ P; SI ~ LOC W; SI ~ SI+7; DS ~ CHR END;            00108100
  REAL STREAM PROCEDURE LEFTLINE(S); VALUE S;                           00108200
          BEGIN LABEL L;                                                00108300
             SI ~ S; DI ~ S; DI ~ DI+1;                                 00108400
          L: SI ~ SI-1; DI ~ DI-2;                                      00108500
             IF SC = " " THEN BEGIN DS ~ LIT "."; GO TO L END;          00108600
             LEFTLINE~SI END;                                           00108700
  REAL STREAM PROCEDURE RIGHTLINE(S); VALUE S;                          00108800
          BEGIN LABEL L;                                                00108900
             SI ~ S; DI ~ S;                                            00109000
       L:    IF SC = " " THEN                                           00109100
                BEGIN SI ~ SI+1; DS ~ LIT "."; GO TO L END;             00109200
             RIGHTLINE ~ SI END;                                        00109300
  REAL STREAM PROCEDURE P(N); VALUE N;                                  00109400
          BEGIN LOCAL L,Q;                                              00109500
             DI ~ LOC L; SI ~ LOC N; DI ~ DI+7; SI ~ SI+6; DS ~ CHR;    00109600
             SI ~ Q; L(SI ~ SI+32; SI ~ SI+32); SI ~ SI+N; P ~ SI END;  00109700
  REAL STREAM PROCEDURE MKABS(A);                                       00109800
          BEGIN DI ~ A; MKABS ~ DI; 4(DI ~ DI+32);                      00109900
              15(DS ~ 8 LIT " ") END;                                   00110000
    LINE[15] ~ 1;                                                       00110100
IF SOURCETOG AND LISTOG AND NOT TIME THEN BEGIN TIME~TRUE;TIMEIT(BUG)END00110200
;IF SOURCETOG THEN                                                      00110300
 BEGIN IF TAPETOG THEN WRITE(TTAPE,16,LINE[*]);                         00110400
              IF LISTOG THEN                                            00110500
                   BEGIN BLANKOUT(LINE[0]);                             00110600
                        WRITE(BUG[PAGE],15,LINE[*]);                    00110700
                   END;                                                 00110800
         END;                                                           00110900
    IF FLOWTOG THEN BEGIN                                               00111000
       PUSHANDWR(QB,0,0);                                               00111100
       PUSHANDWR(QB,0,0);                                               00111200
             N ~ FLOWARRAY[1];                                          00111300
             LLBOO ~ FLOWARRAY[2];                                      00111400
             RLBOO ~ FLOWARRAY[3];                                      00111500
             L ~ BOOLEAN(FLOWARRAY[4] DIV 2);                           00111600
             R ~ BOOLEAN(FLOWARRAY[4]);                                 00111700
             IF LLBOO } 0 THEN PLACE(P(LLBOO)+A+16,".");                00111800
             IF RLBOO } 0 THEN PLACE(P(RLBOO)+A+16,".");                00111900
             MOVE(15,MASK,FINAL);                                       00112000
             IF L THEN LL ~ LEFTLINE(P(C)+A);                           00112100
             IF R THEN RL ~ RIGHTLINE(P(C+N)+A);                        00112200
             CENTER  (P(C)+A,N,FLOWARRAY[5]);                           00112300
             WRITE(CODE,15,FINAL[*]);                                   00112400
             IF LLBOO < 0 AND L THEN PLACE(LL+16," ");                  00112500
             IF RLBOO < 0 AND R THEN PLACE(RL+16," ");                  00112600
          END;                                                          00112700
       FOR I ~ - 2 STEP 1 UNTIL V DO                                    00112800
          BEGIN READ REVERSE (CODE,15,LINE[*]);                         00112900
             LINE[15] ~ 2 + REAL(V = I);                                00113000
              IF TAPETOG THEN WRITE(TTAPE,16,LINE[*]);                  00113100
IF LISTOG THEN IF PAGETOG THEN WRITE(BUG, 15, LINE[*]) ELSE             00113200
                        WRITE (BUG,15,LINE[*]) ;                        00113300
          END;                                                          00113400
                   IF LISTOG THEN WRITE (BUG[PAGE]) ;                   00113500
                                                                        00113600
    END;                                                                00113700
    END WRITEFLOW;                                                      00113800
 PROCEDURE PROCEDUREDEC;                                                00113900
    BEGIN INTEGER FLOWCTROLD,CENTEROLD,LLOLD,RLOLD,OLDOLDLINEPLACE,T;   00114000
       LABEL L;                                                         00114100
       FLOWCTROLD ~ FLOWCTR;                                            00114200
       CENTEROLD ~ CENTER;                                              00114300
       LLOLD ~ LL;                                                      00114400
       RLOLD ~ RL;                                                      00114500
       OLDOLDLINEPLACE ~ OLDLINEPLACE;                                  00114600
       CENTER ~ 60;                                                     00114700
       OLL ~ LL ~ ORL ~ RL ~ OLDLINEPLACE ~ 0;                          00114800
       PUSHANDWR(ACCUM[1],0,0);                                         00114900
       FOR T ~ 1 STEP 1 UNTIL OLDLINEPLACE DO PUSH(QB);                 00115000
       WRITELINE(0,0);                                                  00115100
       PUSHANDWR(QB,0,0);                                               00115200
       PUSHANDWR(QB,0,0);                                               00115300
       ENTER;                                                           00115400
       IF ELBAT[I-2].ADDRESS = STREAMV THEN                             00115500
          BEGIN STRMTOG ~ TRUE;                                         00115600
             DO UNTIL STEPI = BEGINV;                                   00115700
             WHILE STEPI = DECLARATOR OR ACCUM[1] = "5LOCAL" DO         00115800
                DO UNTIL STEPI = SEMICOLON;                             00115900
             COMPOUNDTAIL(120,0);                                       00116000
             STRMTOG ~ FALSE;                                           00116100
          END ELSE                                                      00116200
             BEGIN DO UNTIL STEPI = SEMICOLON;                          00116300
                IF STEPI = VALUEV THEN                                  00116400
                   BEGIN DO UNTIL STEPI = SEMICOLON; STEPIT END;        00116500
                      IF ELCLASS = DECLARATOR THEN                      00116600
                   BEGIN I ~ I-1;                                       00116700
             L:       BLOCK(PREVL,TRUE,120,0);                          00116800
                   END ELSE BEGIN                                       00116900
                     IF ELCLASS = BEGINV THEN                           00117000
                       IF TABLE(I+1) = DECLARATOR THEN GO TO L;         00117100
                     T ~ PREVL;                                         00117200
                     STMT(0120,0);                                      00117300
                     PURGE(T,T,TRUE);                                   00117400
                    END END;                                            00117500
       IF ELBAT[I-1].ADDRESS ! FORWARDV                                 00117600
          THEN WRITEFLOW(FLOWCTR-FLOWCTROLD)                            00117700
         ELSE IF FLOWTOG THEN SPACE(FLOW,FLOWCTROLD-FLOWCTR);           00117800
       FLOWCTR ~ FLOWCTROLD;                                            00117900
       CENTER ~ CENTEROLD;                                              00118000
       LL ~ OLL ~ LLOLD;                                                00118100
       RL ~ ORL ~ RLOLD;                                                00118200
       OLDLINEPLACE ~ OLDOLDLINEPLACE;                                  00118300
    END PROCEDUREDEC;                                                   00118400
PROCEDURE BLOCK(P,SOP,L,R); VALUE P,SOP; REAL P,L,R; BOOLEAN SOP;       00118500
              BEGIN                                                     00118600
              INTEGER PARENCNT,BRACKCNT,BEGINCNT, BACKTO;               00118700
              LABEL START,ARRAYL,DEFINEL,PROCEDUREL,FORWARDL,EXIT,LEBAL,00118800
                   SWITCHL,IOL,IOL1,REALETC,MISCEL;                     00118900
              SWITCH DECSWTCH ~                                         00119000
                   MISCEL,                                              00119100
                   MISCEL,                                              00119200
              REALETC,                                                  00119300
              REALETC,                                                  00119400
              REALETC,                                                  00119500
              REALETC,                                                  00119600
              LEBAL,                                                    00119700
              MISCEL,                                                   00119800
              IOL1,                                                     00119900
              IOL,                                                      00120000
              IOL,                                                      00120100
              MISCEL,                                                   00120200
              SWITCHL,                                                  00120300
              PROCEDUREL,                                               00120400
              ARRAYL,                                                   00120500
              IOL1,                                                     00120600
              IOL1,                                                     00120700
              MISCEL,                                                   00120800
              FORWARDL,                                                 00120900
              DEFINEL;                                                  00121000
                 IF P=0 THEN BEGIN TYPE ~ 2;                  COUNT~1;  00121100
            ACCUM[1]~ "190000"  + "30000";                              00121200
                                             SCRAM ~ ACCUM[1] MOD 125;  00121300
           ENTER; P ~ PREVL END ;                                       00121400
           BACKTO ~ PREVL; INDENT ~ INDENT+4;                           00121500
             IF ELCLASS = BEGINV THEN SOP ~ FALSE;                      00121600
    START: IF STEPI = DECLARATOR THEN                                   00121700
              BEGIN WHILE STEPI = DECLARATOR DO;                        00121800
              TYPE ~ ELBAT[I-1];                                        00121900
              GO TO DECSWTCH[TYPE.ADDRESS] END;                         00122000
              IF ELCLASS = BEGINV THEN IF TABLE(I+1)=DECLARATOR AND SOP 00122100
                   THEN BEGIN SOP~ FALSE; GO TO START END;              00122200
                   IF SOP THEN STMT(L,R) ELSE COMPOUNDTAIL(L,R);        00122300
              PURGE(BACKTO,P,TRUE); GO TO EXIT;                         00122400
    FORWARDL: PURGE(BACKTO-1,P,FALSE); GO TO EXIT;                      00122500
    MISCEL:   WHILE STEPI ! SEMICOLON DO; GO TO START;                  00122600
    DEFINEL:  ENTER; WHILE STEPI ! CROSSHATCH DO;                       00122700
              IF STEPI = SEMICOLON THEN GO TO START;                    00122800
              STEPIT;                                                   00122900
              GO TO DEFINEL;                                            00123000
    PROCEDUREL:  IF SOP THEN GO TO REALETC;                             00123100
              PROCEDUREDEC; GO TO START;                                00123200
    REALETC:  ENTER;                                                    00123300
              IF STEPI = SEMICOLON THEN GO TO START;                    00123400
                STEPIT;                                                 00123500
                GO TO REALETC;                                          00123600
       LEBAL: ENTER; PUT(TAKE(PREVL)&LABELID[2:41:7],PREVL);            00123700
              IF STEPI = SEMICOLON THEN GO TO START;                    00123800
              STEPIT; GO TO LEBAL;                                      00123900
    ARRAYL:   ENTER;                                                    00124000
              IF STEPI = COMMA THEN BEGIN STEPIT; GO TO ARRAYL END;     00124100
              BRACKCNT ~ 0;                                             00124200
              DO BEGIN                                                  00124300
                 IF ELCLASS = LFTBRKET                                  00124400
                   THEN BRACKCNT ~ BRACKCNT+1                           00124500
                   ELSE IF ELCLASS = RTBRKET                            00124600
                     THEN BRACKCNT ~ BRACKCNT-1;                        00124700
                 STEPIT END UNTIL BRACKCNT = 0 ;                        00124800
              IF ELCLASS = SEMICOLON THEN GO TO START;                  00124900
                STEPIT;                                                 00125000
                GO TO ARRAYL;                                           00125100
    IOL:      TYPE ~ ELBAT[I-2];                                        00125200
    IOL1:     ENTER;                                                    00125300
              IF ELBAT[I-2].ADDRESS = SWITCHV AND TABLE(I+1) ! COMMA    00125400
                 THEN GO TO MISCEL;                                     00125500
              PARENCNT ~ 0;                                             00125600
              IF STEPI = COMMA THEN BEGIN STEPIT; GO TO IOL1 END;       00125700
              IF ELCLASS = SEMICOLON THEN GO TO START; I ~ I-1;         00125800
    DO UNTIL STEPI = LEFTPAREN; I ~ I -1;                               00125900
              DO IF STEPI   =LEFTPAREN                                  00126000
                   THEN PARENCNT ~ PARENCNT+1                           00126100
                   ELSE IF ELCLASS = RTPAREN THEN PARENCNT ~ PARENCNT-1 00126200
                UNTIL PARENCNT = 0;                                     00126300
              IF STEPI = SEMICOLON THEN GO TO START;                    00126400
              STEPIT;                                                   00126500
              GO TO IOL1;                                               00126600
    SWITCHL:  IF SOP THEN GO TO IOL1; ENTER;                            00126700
              PUT(TAKE(PREVL)&SWITCHID[2:41:7],PREVL);                  00126800
              WHILE STEPI ! SEMICOLON DO                                00126900
                BEGIN STEPIT;                                           00127000
                   PUT(ELBAT[I],AVAIL);                                 00127100
                   AVAIL ~ AVAIL+1                                      00127200
                END;                                                    00127300
              PUT(0,AVAIL);                                             00127400
              AVAIL ~ AVAIL+1;                                          00127500
              GO TO START;                                              00127600
    EXIT:  END BLOCK;                                                   00127700
              FILL INFO[0,*] WITH 0, OCT011400000000,0;                 00127800
              LASTUSED ~ -2;                                            00127900
              READACARD;                                                00128000
              LISTOG ~ INDEXTOG ~ FLOWTOG ~ SOURCETOG ~ FALSE;          00128100
              AVAIL ~ 451; PREVL ~ 450;                                 00128200
              NXTELBT ~ 1;                                              00128300
FILL CURRSEQA[*] WITH 0,0,"        ";                                   00128400
 FILL HOLDER[*] WITH                                                    00128500
     "3 O<00",              % QA= 0                                     00128600
     "1.0000",              % QB= 1                                     00128700
     "59.NO.",              % QC= 2                                     00128800
     "1;0000",              % QD= 3                                     00128900
     "1 0000",              % QE= 4                                     00129000
     "2~~000",              % QF= 5                                     00129100
     "2  000",              % QG= 6                                     00129200
     "1V0000",              % QH= 7                                     00129300
     "6     ", " 0000000",  % QI= 8                                     00129400
     "3YES00",              % QJ=10                                     00129500
     "3>O 00",              % QK=11                                     00129600
     "4FOR 0",              % QL=12                                     00129700
     "6LOOPS", " 0000000",  % QM=13                                     00129800
     "6....>", "O0000000",  % QN=15                                     00129900
     "3~.~00",              % QP=17                                     00130000
     "1|0000",              % QQ = 18                                   00130100
     0;                                                                 00130200
              FILL TEN[*] WITH                                          00130300
                           OCT1771110463422054,                         00130400
                           OCT1761332600326467,                         00130500
                           OCT1751621340414205,                         00130600
                           OCT1742165630517247,                         00130700
                           OCT1732623176643120,                         00130800
                           OCT1723370036413744,                         00130900
                           OCT1714266046116735,                         00131000
                           OCT1705343457542525,                         00131100
                           OCT1676634373473252,                         00131200
                           OCT1651040347241213,                         00131300
                           OCT1641250441111455,                         00131400
                           OCT1631522551333770,                         00131500
                           OCT1622047303622767,                         00131600
                           OCT1612461164567564,                         00131700
                           OCT1603175421725521,                         00131800
                           OCT1574034726313046,                         00131900
                           OCT1565044113775657,                         00132000
                           OCT1556255136775233,                         00132100
                           OCT1547730366574502,                         00132200
                           OCT1521171646433362,                         00132300
                           OCT1511430220142257,                         00132400
                           OCT1501736264172732,                         00132500
                           OCT1472325741231521,                         00132600
                           OCT1463013331500045,                         00132700
                           OCT1453616220020057,                         00132800
                           OCT1444561664024072,                         00132900
                           OCT1435716241031111,                         00133000
                           OCT1427301711237333,                         00133100
                           OCT1401116227350722,                         00133200
                           OCT1371341675243107,                         00133300
                           OCT1361632254513731,                         00133400
                           OCT1352200727636717,                         00133500
                           OCT1342641115606502,                         00133600
                           OCT1333411341150223,                         00133700
                           OCT1324313631402270,                         00133800
                           OCT1315376577702746,                         00133900
                           OCT1306676337663537,                         00134000
                           OCT1261045602764047,                         00134100
                           OCT1251257143561061,                         00134200
                           OCT1241532774515275,                         00134300
                           OCT1232061573640554,                         00134400
                           OCT1222476132610706,                         00134500
                           OCT1213215561353071,                         00134600
                           OCT1204061115645707,                         00134700
                           OCT1175075341217270,                         00134800
                           OCT1166314631463146,                         00134900
                           OCT1141000000000000,                         00135000
                           OCT1131200000000000,                         00135100
                            OCT1121440000000000,                        00135200
                           OCT1111750000000000,                         00135300
                           OCT1102342000000000,                         00135400
                           OCT1073032400000000,                         00135500
                           OCT1063641100000000,                         00135600
                           OCT1054611320000000,                         00135700
                           OCT1045753604000000,                         00135800
                           OCT1037346545000000,                         00135900
                           OCT1011124027620000,                         00136000
                           OCT0001351035564000,                         00136100
                           OCT0011643245121000,                         00136200
                           OCT0022214116345200,                         00136300
                           OCT0032657142036440,                         00136400
                           OCT0043432772446150,                         00136500
                           OCT0054341571157602,                         00136600
                           OCT0065432127413543,                         00136700
                           OCT0076740555316473,                         00136800
                           OCT0111053071060221,                         00136900
                           OCT0121265707274266,                         00137000
                           OCT0131543271153343,                         00137100
                           OCT0142074147406234,                         00137200
                           OCT0152513201307703,                         00137300
                           OCT0163236041571663,                         00137400
                           OCT0174105452130240,                         00137500
                           OCT0205126764556310,                         00137600
                           OCT0216354561711772,                         00137700
                           OCT0231004771627437,                         00137800
                           OCT0241206170175347,                         00137900
                           OCT0251447626234641,                         00138000
                           OCT0261761573704011,                         00138100
                           OCT0272356132665013,                         00138200
                           OCT0303051561442216,                         00138300
                           OCT0313664115752661,                         00138400
                           OCT0324641141345435,                         00138500
                           OCT0336011371636745,                         00138600
                           OCT0347413670206536,                         00138700
                           OCT0361131664625027,                         00138800
                           OCT0371360241772234,                         00138900
                           OCT0401654312370703,                         00139000
                           OCT0412227375067064,                         00139100
                           OCT0422675274304701,                         00139200
                           OCT0433454553366062,                         00139300
                           OCT0444367706263476,                         00139400
                           OCT0455465667740415,                         00139500
                           OCT0467003245730521,                         00139600
                           OCT0501060411731665,                         00139700
                           OCT0511274514320242,                         00139800
                           OCT0521553637404312,                         00139900
                           OCT0532106607305375,                         00140000
                           OCT0542530351166674,                         00140100
                           OCT0553256443424453,                         00140200
                           OCT0564132154331566,                         00140300
                           OCT0575160607420123,                         00140400
                           OCT0606414751324150,                         00140500
                           OCT0621012014361120,                         00140600
                           OCT0631214417455344,                         00140700
                           OCT0641457523370635,                         00140800
                           OCT0651773450267005,                         00140900
                           OCT0662372362344606,                         00141000
                           OCT0673071057035747,                         00141100
                           OCT0703707272645341,                         00141200
                           OCT0714671151416632,                         00141300
                           OCT0726047403722400,                         00141400
                           OCT0737461304707100,                         00141500
                           OCT0751137556607072,                         00141600
                           OCT0761367512350710,                         00141700
                           OCT0771665435043072;                         00141800
              FILL INFO[1,*] WITH                                       00141900
OCT0,                                                                   00142000
             OCT1000000000000602,                                       00142100
              "7COMME",                                                 00142200
              "NT000000",                                               00142300
             OCT0340000010000000,                                       00142400
              "4TRUE0",                                                 00142500
             OCT0340000000000427,                                       00142600
              "5FALSE",                                                 00142700
             OCT1160000000000000,                                       00142800
              "3NOT00",                                                 00142900
             OCT0530000000000000,                                       00143000
              "2IF000",                                                 00143100
             OCT0540000000000416,                                       00143200
              "2GO000",                                                 00143300
             OCT0410000000000000,                                       00143400
              "4READ0",                                                 00143500
             OCT0420000000000441,                                       00143600
              "5WRITE",                                                 00143700
             OCT0550000000000000,                                       00143800
              "7RELEA",                                                 00143900
              "SE000000",                                               00144000
             OCT0430000000000000,                                       00144100
              "3FOR00",                                                 00144200
             OCT0440000000000520,                                       00144300
              "5WHILE",                                                 00144400
             OCT0450000000000000,                                       00144500
              "2DO000",                                                 00144600
             OCT0460000000000000,                                       00144700
              "5UNTIL",                                                 00144800
             OCT0470000000000000,                                       00144900
              "4ELSE0",                                                 00145000
             OCT0500000000000000,                                       00145100
              "3END00",                                                 00145200
    OCT0520000000000000,                                                00145300
              "4FILL0",                                                 00145400
             OCT0560000000000000,                                       00145500
              "5BEGIN",                                                 00145600
             OCT0570000010000000,                                       00145700
              "3OWN00",                                                 00145800
             OCT0570000020000000,                                       00145900
              "4SAVE0",                                                 00146000
             OCT0570000050000000,                                       00146100
              "5ALPHA",                                                 00146200
             OCT0570000040000000,                                       00146300
              "4REAL0",                                                 00146400
             OCT0570000030000523,                                       00146500
              "7BOOLE",                                                 00146600
              "AN000000",                                               00146700
             OCT0570000060000000,                                       00146800
              "7INTEG",                                                 00146900
              "ER000000",                                               00147000
              0,                                                        00147100
              "6DOUBL",                                                 00147200
              "E0000000",                                               00147300
             OCT0570000070000000,                                       00147400
              "5LABEL",                                                 00147500
             OCT0570000100000000,                                       00147600
              "4DUMP0",                                                 00147700
             OCT0570000110000000,                                       00147800
              "4LIST0",                                                 00147900
             OCT0570000120000000,                                       00148000
              "3OUT00",                                                 00148100
             OCT0570000130000000,                                       00148200
              "2IN000",                                                 00148300
             OCT0570000140000000,                                       00148400
              "7MONIT",                                                 00148500
              "OR000000",                                               00148600
             OCT0570000150000000,                                       00148700
              "6SWITC",                                                 00148800
              "H0000000",                                               00148900
             OCT0570000160000451,                                       00149000
              "9PROCE",                                                 00149100
              "DURE0000",                                               00149200
             OCT0570000170000406,                                       00149300
              "5ARRAY",                                                 00149400
             OCT0570000200000542,                                       00149500
              "6FORMA",                                                 00149600
              "T0000000",                                               00149700
             OCT0570000210000000,                                       00149800
              "4FILE0",                                                 00149900
             OCT0570000230000000,                                       00150000
              "7FORWA",                                                 00150100
              "RD000000",                                               00150200
             OCT0570000240000000,                                       00150300
              "6DEFIN",                                                 00150400
              "E0000000",                                               00150500
             OCT1020000000000000,                                       00150600
              "5VALUE",                                                 00150700
             OCT1050000000000000,                                       00150800
              "4STEP0",                                                 00150900
             OCT1110000000000000,                                       00151000
              "4WITH0",                                                 00151100
             OCT1120000000000000,                                       00151200
              "4THEN0",                                                 00151300
             OCT1130000000000000,                                       00151400
              "2TO000",                                                 00151500
             OCT1210002030000000,                                       00151600
              "3EQV00",                                                 00151700
             OCT1220000000000000,                                       00151800
              "3IMP00",                                                 00151900
             OCT1230000430000554,                                       00152000
              "2OR000",                                                 00152100
             OCT1240001030000000,                                       00152200
              "3AND00",                                                 00152300
             OCT1270006000000000,                                       00152400
              "3DIV00",                                                 00152500
             OCT1270016000000000,                                       00152600
              "3MOD00",                                                 00152700
             OCT0570000220000000,                                       00152800
              "6STREA",                                                 00152900
              "M0000000",                                               00153000
             OCT0110000000040000,                                       00153100
              "3SIN00",                                                 00153200
              12,                                                       00153300
             OCT0110000000040000,                                       00153400
              "3COS00",                                                 00153500
              13,                                                       00153600
             OCT0110000002040000,                                       00153700
              "4SIGN0",                                                 00153800
              8,                                                        00153900
             OCT0110000003060000,                                       00154000
              "6ENTIE",                                                 00154100
              "R0000000",                                               00154200
              9,                                                        00154300
             OCT0110000001040606,                                       00154400
              "3ABS00",                                                 00154500
              7,                                                        00154600
             OCT0110000000040470,                                       00154700
              "4SQRT0",                                                 00154800
              11,                                                       00154900
             OCT0110000000060000,                                       00155000
              "6ARCTA",                                                 00155100
              "N0000000",                                               00155200
              14,                                                       00155300
             OCT0110000000040000,                                       00155400
              "2LN000",                                                 00155500
              15,                                                       00155600
             OCT0110000004040000,                                       00155700
              "3ZIP00",                                                 00155800
              10,                                                       00155900
             OCT0110000000040000,                                       00156000
              "3EXP00",                                                 00156100
              16,                                                       00156200
             OCT0600000600040000,                                       00156300
              "2SI000",                                                 00156400
             OCT0610001040040000,                                       00156500
              "2DI000",                                                 00156600
             OCT0620001460040000,                                       00156700
              "2CI000",                                                 00156800
             OCT0630001630040000,                                       00156900
              "5TALLY",                                                 00157000
             OCT0640000530040000,                                       00157100
              "2DS000",                                                 00157200
             OCT0650000150040000,                                       00157300
              "4SKIP0",                                                 00157400
             OCT0660001620040000,                                       00157500
              "4JUMP0",                                                 00157600
             OCT0670000740040000,                                       00157700
              "2DB000",                                                 00157800
             OCT0700000500040000,                                       00157900
              "2SB000",                                                 00158000
             OCT0710000260060000,                                       00158100
              "6TOGGL",                                                 00158200
              "E0000000",                                               00158300
             OCT0720000730040000,                                       00158400
              "2SC000",                                                 00158500
             OCT0730001160040000,                                       00158600
              "3LOC00",                                                 00158700
             OCT0740001170040000,                                       00158800
              "2DC000",                                                 00158900
             OCT0750001430040000,                                       00159000
              "5LOCAL",                                                 00159100
             OCT0760000340040000,                                       00159200
              "3LIT00",                                                 00159300
             OCT0770001036440000,                                       00159400
              "3SET00",                                                 00159500
             OCT0770001066540000,                                       00159600
              "5RESET",                                                 00159700
             OCT0770001020540000,                                       00159800
              "3WDS00",                                                 00159900
             OCT0770001357740000,                                       00160000
              "3CHR00",                                                 00160100
             OCT0770001057340000,                                       00160200
              "3ADD00",                                                 00160300
             OCT0770001617240000,                                       00160400
              "3SUB00",                                                 00160500
             OCT0770000727640000,                                       00160600
              "3ZON00",                                                 00160700
             OCT0770000417440000,                                       00160800
              "3NUM00",                                                 00160900
             OCT0770000766740000,                                       00161000
              "3OCT00",                                                 00161100
             OCT0770000176600000,                                       00161200
              "3DEC00",                                                 00161300
              OCT0;                                                     00161400
              FILL SPECIAL[*] WITH                                      00161500
              OCT3140000000000000,  COMMENT #;                          00161600
              OCT2000000000100000,  COMMENT @;                          00161700
             OCT0000000000000000,  COMMENT  ;                           00161800
              OCT3060000000120000,  COMMENT :;                          00161900
             OCT1250000450002763,  COMMENT >;                           00162000
             OCT1250000250002662,  COMMENT };                           00162100
              OCT3260000000000000,  COMMENT +;                          00162200
             OCT0000000000000000,  COMMENT  ;                           00162300
              OCT3040000000060000,  COMMENT .;                          00162400
              OCT3010000000000000,  COMMENT [;                          00162500
              OCT3200000000000000,  COMMENT &;                          00162600
              OCT2400000000000000,  COMMENT (;                          00162700
             OCT1250010450003571,  COMMENT <;                           00162800
             OCT1170000000000000,  COMMENT ~;                           00162900
          OCT3270000000000000,  COMMENT |;                              00163000
             OCT0000000000000000,  COMMENT  ;                           00163100
             OCT0000000000040000,  COMMENT $;                           00163200
              OCT3300000000000000,  COMMENT *;                          00163300
              OCT3260000000000000,  COMMENT -;                          00163400
              OCT3100000000160000,  COMMENT );                          00163500
        OCT2510000000000000, COMMENT SEMICOLON;                         00163600
             OCT1250010250003470,  COMMENT {;                           00163700
             OCT0000000000000000,  COMMENT  ;                           00163800
              OCT3270000000000000,  COMMENT /;                          00163900
              OCT3030000000000000,  COMMENT ,;                          00164000
             OCT0000000000020000,  COMMENT %;                           00164100
             OCT1250001050002561,  COMMENT !;                           00164200
             OCT1250011050002460,  COMMENT =;                           00164300
              OCT3070000000000000,  COMMENT ];                          00164400
             OCT0000000000140000,  COMMENT "; OCT0, OCT0;               00164500
                   FILL  STACKHEAD[*] WITH  257,                        00164600
318,291,393,326,  0,287,277,293,304,356,350,  0,  0,  0,362,285,        00164700
358,  0,  0,314,  0,373,  0,  0,  0,329,348,  0,  0,  0,  0,  0,        00164800
  0,  0,  0,380,301,  0,  0,  0,  0,  0,  0,  0,  0,  0,396,299,        00164900
367,331,  0,260,  0,  0,  0,383,  0,  0,  0,  0,  0,  0,  0,316,        00165000
281,  0,  0,  0,  0,  0,  0,  0,  0,  0,376,  0,344,323,  0,  0,        00165100
  0,264,  0,  0,  0,283,  0,268,  0,  0,266,  0,  0,320,  0,  0,        00165200
  0,370,  0,342,295,334,  0,  0,360,352,  0,  0,  0,310,  0,  0,        00165300
  0,  0,  0,  0,346,  0,274,  0,  0,  0,272,  0;                        00165400
LISTOG~ SOURCETOG~ FLOWTOG~ INDEXTOG~ TRUE;                             00165500
WRITE(CODAL,*,-1,-1);                                                   00165600
QC ~ QC +"30000";                                                       00165700
WRITE(CODE, 20, A[*]);                                                  00165800
FLOWCTR ~ 0;                                                            00165900
LLBOO ~ RLBOO ~ -1;                                                     00166000
CENTER ~ 60;                                                            00166100
WRITE(FLOW, 14, A[*]);                                                  00166200
             RECORDCOUNT~1;                                             00166300
IF LASTUSED=-2 THEN LASTUSED~-1;                                        00166400
              DO UNTIL STEPI = BEGINV; STMT(120,0);                     00166500
WRITEFLOW(FLOWCTR);                                                     00166600
IF TAPIN  THEN CLOSE(TAPE,RELEASE);                                     00166700
CLOSE(CARD,RELEASE);                                                    00166800
REWIND(FLOW);                                                           00166900
LINE[0] ~ IMC+2; LINE[15] ~ 9;                                          00167000
IF TAPETOG THEN BEGIN                                                   00167100
WRITE(TTAPE,16,LINE[*]);                                                00167200
REWIND(TTAPE);                                                          00167300
END;                                                                    00167400
A[0] ~ IMC+2;                                                           00167500
WRITE(CODE, 20, A[*]);                                                  00167600
DELTA ~ 0;                                                              00167700
         DELTA ~ DELTA - 2;                                             00167800
         IF DELTA < 0 THEN GO TO L101;                                  00167900
         GO TO L102;                                                    00168000
    END;                                                                00168100
IF A[0] > BEGINARRAY[DELTA] THEN                                        00168200
DELTA ~ DELTA+2;                                                        00168300
MOVE(2,A,BEGINARRAY[DELTA]);                                            00168400
    GO TO L103;                                                         00168500
L101:REWIND(CODAL);                                                     00168600
L104: READ REVERSE(CODE, 20, A[*]);                                     00168700
    GT1 ~ 0;                                                            00168800
L105: IF TAPETOG THEN                                                   00168900
BEGIN READ(  TTAPE, 16, B[*]);                                          00169000
    IF B[15] = 9 THEN GO TO L106;                                       00169100
    IF B[15] < 2 THEN                                                   00169200
         BEGIN IF A[0] = B[0] THEN MOVE(1,A[1],B[14]);                  00169300
               IF A[0] < B[0] THEN GT1 ~ 2;                             00169400
               BLANKOUT(B[0]);                                          00169500
         END;                                                           00169600
    WRITE(ALDOC,16,B[*]);                                               00169700
IF LISTOG AND NOT TIME THEN BEGIN TIME~ TRUE; TIMEIT(BUG) END;          00169800
         FOR I ~ 1 STEP 1 UNTIL RCF DO                                  00169900
BEGIN READ(CODE, 20, A[*]);                                             00170000
WRITE(EDOC, 20, A[*]);                                                  00170100
              END;                                                      00170200
    LINE[15] ~ 4;                                                       00170300
    RECORDCOUNT ~ RCF-1;                                                00170400
    INDENT ~ 2;                                                         00170500
    GT1 ~ 1;                                                            00170600
LB: IF INDENT=2 THEN READ(CODE,20,A[*])                                 00170700
         ELSE BEGIN RECORDCOUNT ~ RECORDCOUNT + 1;                      00170800
READ(EDOC, 20, A[*]);                                                   00170900
              END;                                                      00171000
    RCF ~ A[1];                                                         00171100
    GT1 ~ A[0] DIV 512;                                                 00171200
PRINTHEN(LINE[0], A[0], INDENT DIV 2);                                  00171300
    IF TAPETOG THEN WRITE(ALDOC,16,LINE[*]);                            00171400
IF LISTOG THEN WRITE(BUG, 15, LINE[*]);                                 00171500
    IF RCF = 0 THEN                                                     00171600
LC:      BEGIN IF GT1 ! 0 THEN GO TO LB;                                00171700
         END ELSE BEGIN B[INDENT] ~ RECORDCOUNT;                        00171800
              B[INDENT+1] ~ GT1;                                        00171900
              GT1 ~ 1;                                                  00172000
              RCF ~ RCF-1;                                              00172100
              INDENT ~ INDENT+2;                                        00172200
         END;                                                           00172300
    SPACE(EDOC,RCF-RECORDCOUNT);                                        00172400
    RECORDCOUNT ~ RCF;                                                  00172500
    GO TO LC;                                                           00172600
END;                                                                    00172700
EXIT:IF TIME THEN TIMEIT(BUG);      END.                                00172800
                                                                        00172900
