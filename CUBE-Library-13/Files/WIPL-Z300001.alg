BEGIN COMMENT  WIPL - WISCONSIN INTERACTIVE PROBLEM-SOLVING LANGUAGE    10001000
                      BY  ED HARRIS  AND  BOB JANOSKI                   10003000
                         UNIV. OF WISCONSIN CUMPUTING CENTER  (MADISON) 10005000
VERSION 1.50 (9/1/70)                                                   10007000
THE DIAGRAM BELOW SKETCHES THE PATHS OF CONTROL  (SHOWN BY = AND H)     10009000
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 10011000
                                                                       .10013000
    OUTER BLOCK            BLOCK DIAGRAM                                10015000
 * * * * * * * * * *         -----------               SCANNER          10017000
 *  INITIALIZATION  *                        # = =* * * * * * * * * *   10019000
 *                  *                        H    * PRODUCE SYNTAX  *   10021000
 *  CREATE SAVFILE  *                        H    *    ARRAYS       *   10023000
 *  OR RESTART OR   *                        H    * * * * * * * * * *   10025000
 *  H/L RECOVERY    *                        H                          10027000
 *                  *         SCHEDULER      H           ACCEPT         10029000
 *  CALL SCHEDULER  =====* * * * * * * *     H   # = * * * * * * * * *  10031000
 *                  *    *---D.C.I.O.----*   H   H   * READ UNDEFINED*  10033000
 *  CLEANUP         *    *---------------*   H   H   *   VALUES      *  10035000
 * * * * * * * * * *     * CALL SCANNER  ====#   H   * * * * * * * * *  10037000
                         *               *       H                      10039000
                         * CALL ACCEPT   = = = = #    TRANSLATOR        10041000
                         * OR TRANSLATOR = = = = = * * * * * * * * * * *10043000
                         *               *         *--STMT PROCEDURES--*10045000
                         * CALL RUNNERS  *         *--VERB PROCEDURES--*10047000
      RUNNERS            * * * * H * * * *         *--CLEANUPCODE------*10049000
      -------                    H                 *--LINKUP-----------*10051000
    S:EXECUTE CODE = = = = = = = #                 *-------------------*10053000
    E:WRITE MESSAGE= = = = = = = #                 *PRODUCE CODE ARRAY *10055000
    I:INPUT FROM DISK  = = = = = #                 * * * * * * * * * * *10057000
    D:SAVE ON DISK = = = = = = = #                                      10059000
    B:BEGIN NEW USER = = = = = = #                                      10061000
    P:WRITE ONE LINE = = = = = = #                                      10063000
    L:LIST A LINE  = = = = = = = #                                      10065000
    H:LIST HELP CARD = = = = = = #                                      10067000
    K:LIST KEYWORD CARD  = = = = #                                      10069000
    R:RESTART USER = = = = = = = #                                      10071000
    Q:QUIT = = = = = = = = = = = #                                      10073000
   EL:(E&L)  = = = = = = = = = = #                                      10075000
   RL:(R&L)  = = = = = = = = = = #                                      10077000
. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 10079000
          SEQUENCE OF OPERATION:                                        10081000
          1.   INITIALIZATION (IN OUTER BLOCK)                          10083000
          2.   A CALL TO SCHEDULER.                                     10085000
               3.   CALLS ON DCIO TO LOAD ONE CARD IMAGE INTO           10087000
                    SYNTAX[0] ARRAY.                                    10089000
               3A   A CALL ON SCANNER TO PRODUCE THREE ARRAYS:          10091000
                         SCANCODE, WHICH HAS A 6 BIT CLASS AND A 6 BIT  10093000
                              KEY FOR EACH SYMBOL ON THE CARD           10095000
                         IDENT, WHICH CONTAINS THE ALPHA CHARACTERS     10097000
                              OF THE IDENTIFIERS, AN ARRAY FLAG, AND    10099000
                              AND THE VALUE OF THE IDENTIFIER.          10101000
                              THE VALUE OF THE IDENTIFIER IS SET        10103000
                              TO "UNDEFINED".                           10105000
                         SCANCON, WHICH IS AN INTERMEDIATE LIST OF      10107000
                              CONSTANTS AND STRINGS TO BE USED LATER.   10109000
               4.   A CALL ON ONE OF THE ANALYSIS PROCEDURES, DETERMINDD10111000
                         BY SCAN (WHICH PRODUCES CODE IN THE TEMPCODE   10113000
                         ARRAY).                                        10115000
               5.   A CALL ON THE CLEANUP ROUTINE WHICH MOVES CODE      10117000
                    AND CONSTANTS AND STRINGS INTO THE CODE ARRAY.      10119000
               6.   A CALL ON STORE IF PROGRAM MODE ELSE A CALL ON      10121000
                    EXECUTE IF CALCULATOR MODE.                         10123000
               7.   IF EXECUTE IS SUSPENDED (AWAITING I/O) AN ENTRY     10125000
                    IS MADE IN THE RUNNERS ARRAY FOR SCHEDULING LATER9  10127000
               8.   IF NO RUNNERS ARE ACTIVE, IT SLEEPS ON INPUT.       10129000
          9.   IF ANY REALLOCATION IS REQUIRED, THEN SCHEDULER EXITS    10131000
               TO THE OUTER BLOCK AND IS RECALLED AGAIN WITH NEW ARRAY  10133000
               DIMENSIONS.                                              10135000
          ;                                                             10137000
DEFINE MAX = 6#;  COMMENT  SPECIFIES 7 POSSIBLE USERS OF WIPL;          10139000
          COMMENT  SEE FILE DECLARATIONS IN SCHEDULER;                  10141000
DEFINE MAXINPUTBUFFERSIZE = 5#;                                         10143000
     COMMENT  USE "5" IF ALL INPUT BUFFERS ARE 28 CHARACTERS,           10145000
              USE "8" IF ANY INPUT BUFFERS ARE 56 CHARACTERS;           10147000
DEFINE TIMEOUT = 240#;  COMMENT  4 MINUTE IDLE TIMEOUT;                 10149000
DEFINE SYNTAXERRSTART = 75#;                                            10151000
DEFINE MAXERRNUMBER = 106#; COMMENT SYNTAXERRSTART+LAST SYNTAX ERROR;   10151100
COMMENT                                                                 10153000
     LAST SYNTAX ERROR USED IS 31.                                      10153100
     LAST EXECUTION ERROR USED IS 49;                                   10153200
BOOLEAN DEBUG;  COMMENT  COMMON=1 TURNS ON DEBUG OPTION;                10155000
               COMMENT  COMMON>1 SPECIFIES ALTERNATE SAVFILE;           10157000
INTEGER MAXUSER;    COMMENT  THE INDEX IF THE MAXIMUM LOGGED-IN USER;   10159000
DEFINE SAVEOPTION =  SAVE #;  COMMENT  EXCHANGE OVERLAYABLE CORE FOR    10161000
          BETTER RESPONSE IN A HEAVY MIX;                               10163000
SAVEOPTION                                                              10165000
ARRAY USERID[0:MAX];  COMMENT   THIS ARRAY DETERMINES LOGGED-IN USERS;  10167000
DEFINE INFMAX = 36#;                                                    10169000
ARRAY INFO[0:MAX, 0:INFMAX];                                            10171000
COMMENT  THE FOLLOWING DEFINES REFER TO INDEX VALUES INTO THE INFO      10173000
          ARRAY;                                                        10175000
DEFINE IOBUF=0#;                                                        10177000
DEFINE DICTMAX=10#; COMMENT  POINTER TO LAST IDENTIFIER IN IDENTIFIERS  10179000
                              ARRAY;                                    10181000
DEFINE CODEMAX=11#; COMMENT  POINTER TO LAST ELEMENT IN CODE ARRAY;     10183000
DEFINE CMAX = CODEMAX#;                                                 10185000
DEFINE LASTRAND=12#;                                                    10187000
DEFINE FILEID=13#;                                                      10189000
DEFINE CINDX=14#, LINDX=15#, SINDX=16#;                                 10191000
          COMMENT  C,S AND L REGISTERS FOR EXECUTE PROCEDURE;           10193000
DEFINE CBASEINDX=17#;                                                   10195000
DEFINE MODE =18#;   COMMENT  IF NON-ZERO, THEN IT IS ACCEPT INDEX;      10197000
DEFINE FILELIMITS = 19#;                                                10199000
DEFINE FILENAME = 19#;                                                  10201000
DEFINE LOADLINK = 20#;                                                  10203000
DEFINE HELPLINK = 20#;                                                  10205000
DEFINE ALPHACCEPT = 21].[46:1#;                                         10207000
DEFINE LOADLIST = 21].[45:1#;                                           10209000
DEFINE ACCEPTFLAG=21].[44:1#;                                           10211000
DEFINE ARRAYFLAG=21].[43:1#;                                            10213000
DEFINE MODEFLAG=21].[42:1#;                                             10215000
DEFINE PUBLICFLAG = 21].[41:1#;                                         10217000
DEFINE FORMFLAG = 21].[40:1#;                                           10219000
DEFINE FORMINDEX = 21].[33:7#;                                          10221000
DEFINE FORMPOINTER = 21].[20:13#;                                       10223000
DEFINE SIGNIFICANCE = 21].[19:1#;                                       10225000
DEFINE DATAST = 21].[18:1#;                                             10227000
DEFINE DATAINDEX = 21].[11:7#;                                          10229000
DEFINE DATASTART = 21].[1:10#;                                          10231000
DEFINE RUNLIMIT=22#,RUNCOUNT=23#;                                       10233000
DEFINE LASTUSERARRAY=24#;                                               10235000
DEFINE BUFINDEX = 25#;                                                  10237000
DEFINE DATAPOINTER = 26#;                                               10239000
DEFINE ALTBUF = 27#;                                                    10241000
COMMENT                                                                 10243000
          INFO[USER,*] CONTAINS ALL THE FLAGS AND COUNTERS FOR EACH USER10245000
               FORMAT:                                                  10247000
                    0-8       72 CHARACTER I/O BUFFER.                  10249000
                    9         SCRATCH.                                  10251000
                    10-26     AS NOTED ABOVE BY DEFINES                 10253000
                    26-36     BUFFER FOR FORMATTED OUTPUT;              10255000
DEFINE HELPSIZE = 900#,                                                 10257000
       HELPARRAYSTART = 301#,                                           10259000
       ERRORSTART = 331#,                                               10261000
       DIRSPACE = 381#,                                                 10263000
       FILESTART = 427#;                                                10265000
COMMENT                                                                 10267000
     IF IT IS NECESSARY TO INCREASE THE SIZE OF THE HELPFILE, HELPSIZE  10269000
     IS THE NUMBER OF CARDS IN THE HELPFILE, HELPARRAYSTART IS HELPSIZE 10271000
     /3+1, ERRORSTART IS HELPARRAYSTART+HELPSIZE/30, DIRSPACE IS        10273000
     ERRORSTART+50, FILESTART IS DIRSPACE+46;                           10275000
ARRAY HELPTABLE[0:HELPSIZE];                                            10277000
BOOLEAN RESTART;                                                        10279000
BOOLEAN READDATA;                                                       10281000
DEFINE ADD1TOP = SI1~SI; DI1~DI; DI~LOC T; SI~P; DS~WDS; SI~T; SI~SI+8; 10283000
     T~SI; SI~LOC T; DI~P; DS~WDS; SI~SI1; DI~DI1#;                     10285000
INTEGER USER;  COMMENT  THE GLOBAL SUBSCRIPT OF USER;                   10287000
DEFINE IDENTIFIERSARRAYSIZE = 128#;                                     10289000
ARRAY  IDENTIFIERS[0:MAX,0:IDENTIFIERSARRAYSIZE];                       10291000
COMMENT                                                                 10293000
          IDENTIFIERS[USER,*] CONTAINS TWO WORDS FOR EACH VARIABLE:     10295000
               FIRST WORD     [1:1]          1 IF ARRAY IDENTIFIER.     10297000
                              [2:4]          # CHARACTERS IN NAME.      10299000
                              [6:42]         FIRST 7 CHARACTERS.        10301000
               SECOND WORD    VALUE, IF SIMPLE VARIABLE, ELSE:          10303000
                              [1:2]          # OF DIMENSIONS.           10305000
                              [3:8],[11:8],[19:8],[27:8]    SIZES OF DIM10307000
                              [38:10]        USERARRAY[USER,*] BASEADDR.10309000
                              ;                                         10311000
DEFINE STACKARRAYSIZE = 50#;                                            10313000
INTEGER MAXCODESIZE;                                                    10315000
INTEGER MAXUSERARRAY;                                                   10317000
ARRAY DICT[0:110];  COMMENT  DICTIONARY OF RESERVED WORDS;              10319000
          COMMENT  USED BY  SCAN  AND  HELPLOADER  ONLY;                10321000
          DEFINE DICTSIZEMINUS63=47#;                                   10323000
SAVEOPTION                                                              10325000
REAL ARRAY RUNNER[0:MAX];  COMMENT  THE SCHEDULE BY USER;               10327000
ARRAY SYNTAX[0:215];                                                    10329000
DEFINE TEMPCODE=11#, SCANCODE=79#, SCANCON=147#;                        10331000
COMMENT                                                                 10333000
          SYNTAX IS THE ARRAY THAT INTERFACES BETWEEN DCIO, SCANNER,    10335000
          AND TRANSLATOR.                                               10337000
          0-9       IS INPUT IMAGE.                                     10339000
          TEMPCODE  IS TRANSLATOR PRODUCED CODE CONTAINING AN OP CODE   10341000
                    AND MODIFIER FOR EACH PSEUDO INSTRUCTION GENERATED. 10343000
          SCANCODE  IS SCANNER PRODUCED CODE CONTAINING A CLASS AND A   10345000
                    KEY FIELD FOR EACH SYMBOL ON THE INPUT CARD.        10347000
                    (SEE INITIALIZE COMMENTS).                          10349000
          SCANCON   IS AN INTERMEDIATE ALPHA REPRESENTATION OF CONSTANTS10351000
                    AND STRINGS.  FOR NUMBERS, CHARACTER 1 IS THE # OF  10353000
                    CHARACTERS IN THE MANTISSA, CHARACTERS 2-N ARE THE  10355000
                    ALPHA CHARACTERS OF THE DIGITS AND DEC. POINT, AND  10357000
                    A FULL WORD (BEGINNING WITH "E" ) IS ADDED FOR      10359000
                    EXPONENTS.     ;                                    10361000
DEFINE CLASSFIELD = [36:6]#;                                            10363000
DEFINE SBIT = [1:1]#;                                                   10365000
DEFINE KEYFIELD = [42:6]#;                                              10367000
DEFINE CDISKADDR = [2:13]#,  CSNUMB = [15:17]#, CLINK = [32:10]#,       10369000
          CSIZE = [42:6]#;                                              10371000
DEFINE CPARTNUMB = [15:7]#, CSTEPNUMB = [22:10]#;                       10373000
COMMENT                                                                 10375000
          THE FORMAT OF THE CODE ARRAY (DECLARED OWN IN SCHEDULER) IS:  10377000
               0         INITIAL LINK WORD TO FIRST EXECUTABLE STMNT.   10379000
               1         LINK WORD FOR FIRST CARD.                      10381000
               2-N       CODE SYLLABLES FOR FIRST CARD.                 10383000
               N+1 TO M  IN-LINE CONSTANTS AND STRINGS.                 10385000
               M+1       LINK WORD FOR SECOND CARD, ETC.                10387000
          LINK FORMAT:                                                  10389000
               SBIT      1 = ERASED CARD.                               10391000
               CDISKADDR:CARD IMAGE # IN SAVEFILE ON DISK.              10393000
               CSNUMB    BINARY PART AND STEP NUMBER.                   10395000
               CLINK     SUBSCRIPT IN CODE[USER,*] OF NEXT LOGICAL PROG.10397000
                         STATEMENT.  "0" IS THE STOPPER.                10399000
               CSIZE     1 = REMARK CARD (WITH NO CODE SYLLABLES) ELSE  10401000
               CSIZE     # OF CODE WORDS FOR THE CARD.                ; 10403000
BOOLEAN ERRFLAG;                                                        10405000
LABEL FINISH;                                                           10407000
LABEL FINISH1;                                                          10409000
LABEL ENDEND;                                                           10411000
BOOLEAN FLAG;                                                           10413000
BOOLEAN OK;                                                             10415000
BOOLEAN ENTERDICT;  COMMENT  TRUE IF NEW VARIABLES SHOULD BE ENTERED  ; 10417000
BOOLEAN ACTIVE;     COMMENT  FLAG FOR ANY ACTIVE RUNNERS;               10419000
BOOLEAN PROGMODE;   COMMENT  PROGRAM/CALCULATOR MODE FLAG;              10421000
BOOLEAN REALLOCATE;       COMMENT  FLAG TO RE-ALLOCATE USER ARRAYS;     10423000
BOOLEAN PFLAG;                                                          10425000
REAL  T, T1, T2;                                                        10427000
REAL T3;                                                                10429000
REAL T4;                                                                10431000
REAL T5;                                                                10433000
REAL T6,T7;                                                             10435000
REAL S1,S2;                                                             10437000
INTEGER I;                                                              10439000
INTEGER ERRNUMBER;                                                      10441000
REAL INTG, FRACTION, TENS;                                              10443000
INTEGER NC,C;                                                           10445000
INTEGER CBASE;                                                          10447000
INTEGER EVENTS;                                                         10449000
REAL STNOMASK;                                                          10451000
REAL UNDEFINED;                                                         10453000
FILE DSK DISK RANDOM "WIPLS" "SAVFILE" (1,30,30);                       10455000
COMMENT                                                                 10457000
          SAVE FILE FORMAT:                                             10459000
          SEG. 0              FIRST DIRECTORY SEG. AND RESTART FLAGS.   10461000
          SEG. 1, ETC.        HELP CARD IMAGES.                         10463000
          SEG. HELPARRAYSTART, ETC.     COPY OF HELPARRAY[*].           10465000
          SEG. ERRORSTART, ETC.    ERROR MESSAGES.                      10467000
          SEG. DIRSPACE, ETC.  SPACE FOR MORE DIRECTORY SEGMENTS.       10469000
          SEG. FILESTART, ETC.     USER CARD IMAGES AND DIRECTORY SEGS.;10471000
COMMENT                                                                 10473000
          THE SAVE FILE IS CREATED BY READING THE FILES ERRORS/CARDS    10475000
          AND HELPFIL/CARDS FROM USER DISK.  THESE FILES MAY BE MODIFIED10477000
          IN TEXT EDITOR-STYLE USING WIPL SINCE THEY CONSIST OF         10479000
          REMARK CARDS.  EXECUTING  "CC REMOVE WIPLS/SAVFILE."  FORCES  10481000
          AN UPDATE OF THE LOCAL FILE;                                  10483000
INTEGER PTMASK;                                                         10485000
SAVEOPTION                                                              10487000
ARRAY DISKBUF[0:29];     COMMENT  CORE COPY OF DISK-WRITE BUFFER;       10489000
INTEGER DISKPOINTER;     COMMENT  CARD NUMBER BEING WRITTEN;            10491000
ARRAY DIRBUF[0:29];      COMMENT  CORE COPY OF CURRENT DIRECTORY SEG.;  10493000
INTEGER DLOC;  COMMENT  LOCATION OF CURRENT DIRECTORY SEGMENT;          10495000
INTEGER DENTRY;     COMMENT  ENTRY # WITHIN DIRECTORY SEGMENT;          10497000
SAVEOPTION                                                              10499000
ARRAY LISTBUF[0:29];     COMMENT  CORE COPY OF COMMON READ-BACK BUFFER; 10501000
INTEGER LISTPOINTER;                                                    10503000
ARRAY SAVEBUF[0:29];                                                    10505000
FILE SAVER DISK SERIAL [20:45] "X" "CARDS"  (1,10,30, SAVE 5);          10507000
FILE IN LOADFILE DISK SERIAL "X" "CARDS" (2,10,30);                     10509000
COMMENT  DE BUG CODE;                                                   10511000
FILE OUT PR 15 (2,17);                                                  10513000
FORMAT E ("   SCANCODE:  ", 15(A6,X1)),                                 10515000
      EE ("   SCANCON:  ",10(O,X1)),                                    10517000
     DMPCODE ("  TEMPCODE:  ", 15(A6,X1)),                              10519000
     DMPIDS (" IDENTIFIERS AND VALUES "),                               10521000
     FMTC(" CODE:  ",14(O,X1)),                                         10523000
     H1(14(O,X1)),                                                      10525000
     XFMT  ("EXECUTE:  ", "C =  ", I4,  "  L =  ", I1,   "  OP = ",     10527000
          A2,  "  MODIFIER = " , A2, "  S =  ", I4, "  STACK =   ");    10529000
COMMENT   THE FOLLOWING STEPS COULD BE TAKEN TO OPTIMIZE WIPL:          10531000
          1.   CHANGE THE DATA COMM READ SO THAT IT CHECKS FOR INPUT    10533000
               MORE THAN ONCE EVERY SECOND.                             10535000
          2.   CHANGE THE DICTIONARY SEARCH IN SCANNER TO KEY ITS       10537000
               SEARCH ON THE CHARACTER COUNT, RATHER THAN SERIALLY      10539000
               SEARCH THE ENTIRE TABLE.                                 10541000
          3.   IMPROVE LOAD-ING SPEED BY ELIMINATING UNNECESSARY DISK   10543000
               REFERENCES.                                              10545000
          4.   MODIFY THE DYNAMIC OWN ARRAYS FOR EACH USER TO BE OF     10547000
               VARIABLE ROW SIZE;                                       10549000
COMMENT        COMPILE BY:                                              10551000
          CC USER = <USERID>                                            10553000
          CC COMPILE WIPL/WIPL ALGOL LIBRARY                            10555000
          CC ALGOL FILE TAPE = WIPL/SOURCE [SERIAL]                     10557000
          CC PRIORITY=4                                                 10559000
          CC CORE=5500                                                  10561000
          CC STACK=400                                                  10563000
          CC DATA CARD                                                  10565000
          $TAPE LIST CHECK                                              10567000
                                                               99999999 10569000
          CC PUBLIC WIPL/WIPL                                           10571000
          CC  END                                                       10573000
          ;                                                             10575000
COMMENT   TO ADD VERBS:                                                 10577000
          1.   MAKE AN ENTRY IN DICT[*].  (EXTEND ITS SIZE IF NEC.)     10579000
          2.   CHANGE THE DEFINES "EITHERINDX" AND "PROGINDX" IF NEC.   10581000
          3.   ADD AN ENTRY IN THE CASE STATEMENT IN TRANSLATOR;        10583000
COMMENT   TO ADD FUNCTIONS:                                             10585000
          1.   REPEAT 1. ABOVE.                                         10587000
          2.   MAKE AN ENTRY IN THE FUNCTION CASE STATEMENT IN EXECUTE; 10589000
% ********************************************************************  20001000
% ********************************************************************  20003000
% *************** S C A N N E R *************************************** 20005000
% ********************************************************************  20007000
% ********************************************************************  20009000
          COMMENT                                                       20011000
          SCANNER PRODUCES THREE ARRAYS:                                20013000
               SCANCODE ARRAY   WHICH CONTAINS A CLASS AND KEY FOR      20015000
                              EACH SYMBOL ON THE CARD,                  20017000
               SCANCON ARRAY   WHICH CONTAINS THE INTERMEDIATE BREAKDOWN20019000
                              OF CONSTANTS AND STRINGS,                 20021000
               IDLIST ARRAY    WHICH IS THE LIST OF IDENTIFIERS         20023000
                              AND THEIR VALUES;                         20025000
     COMMENT                                                            20027000
          SCANNER ENTERS VARIABLE NAMES INTO THE IDENTIFIERS ARRAY.     20029000
          SCANNER ALSO DOES CONSIDERABLE ERROR CHECKING.                20031000
          ;                                                             20033000
COMMENT  THE FOLLOWING DEFINES ARE ENTRYS IN  THE DICT ARRAY;           20035000
DEFINE CONV = 1#;                                                       20037000
DEFINE EITHER = 2#;                                                     20039000
DEFINE PROG = 3#;                                                       20041000
DEFINE CONVINDX = 0#;                                                   20043000
DEFINE EITHERINDX = 9#;                                                 20045000
DEFINE PROGINDX = 15#;                                                  20047000
DEFINE REMARKINDX = 12#;                                                20047100
DEFINE DATAINDX = 23#;                                                  20049000
DEFINE RESTOREINDX = 24#;                                               20051000
DEFINE REPLINDX = 9#;                                                   20053000
DEFINE ARRAYID = 6#;                                                    20055000
DEFINE REMLETR1="2"#, REMLETR2="3"#;                                    20057000
DEFINE FORMLETR1 = "3"#, FORMLETR2 = "7"#;                              20059000
DEFINE IDLETTER = "5"#;                                                 20061000
DEFINE ARRAYIDLETTER="6"#;                                              20063000
DEFINE FUNCTLETR1 = "@"#;                                               20065000
DEFINE OPR = 10#;                                                       20067000
DEFINE ENDR = 00#;                                                      20069000
DEFINE DRBRK = "91"#;                                                   20071000
DEFINE DLPAREN = "40"#;                                                 20073000
DEFINE STRNG = 15#;                                                     20075000
DEFINE DRPAREN = "90"#;                                                 20077000
DEFINE CON = 7#;                                                        20079000
DEFINE ADSUBOP = 8#;                                                    20081000
DEFINE LISTLETRS = "16"#;                                               20083000
INTEGER PROCEDURE SCANNER (INFO, IDLIST);    ARRAY INFO[0], IDLIST[0];  20085000
     BEGIN                                                              20087000
          COMMENT  SCANNER RETURNS AN INDEX FOR THE CASE STATEMENT      20089000
               IN TRANSLATOR;                                           20091000
          LABEL XIT, TWOOPS;                                            20093000
          LABEL BRKERR, PARENERR, CLASSERR;                             20095000
          LABEL CLASSERR1,CLASSERR2;                                    20097000
INTEGER STREAM PROCEDURE STRNGLENGTH(STR);                              20099000
BEGIN LABEL EXIT; TALLY ~1; SI~STR;8(8(IF SC=""" THEN JUMP OUT 2 TO EXIT20101000
     ;SI~SI+1); TALLY~TALLY+1); EXIT:STRNGLENGTH~TALLY; END STRNGLENGTH;20103000
BOOLEAN STREAM PROCEDURE SCAN(ERRLIST, CLEAROUT, IDLIST, CONLIST,       20105000
          DICT, CODELIST, BUF);                                         20107000
     BEGIN                                                              20109000
          LOCAL     T, T1, T2, ERRBUF;                                  20111000
         LABEL      NEXT, DEBLANK, ERR, NUMB, NUMBR, STRING, SERCH,     20113000
                    DFOUND, ERRL, XIT, EXPO, EXP, E1, E2, A, SLBRK,     20115000
                    IDSERCH, ERRQ, ERRQM;                               20117000
          DI ~ CLEAROUT;  DS ~ 8 LIT "00";  SI ~ CLEAROUT;              20119000
          3(DS~63 WDS); DS~14 WDS; %CLEAR SYNTAX ARRAY                  20121000
          DS ~ 1 LIT "E";  DI ~ ERRLIST;                                20123000
          DS ~ 40 LIT " ";  COMMENT BLANK OUT ERROR BUFFER;             20125000
          TALLY ~ 1;  SCAN ~ TALLY;  COMMENT  SET PROCEDURE TRUE;       20127000
          DI ~ BUF;  DI ~ DI + 32;  DI ~ DI + 40;  DS ~ LIT "~";        20129000
               DS ~ LIT """;  DS ~ 2 LIT " ~";  COMMENT SET END OF CARD;20131000
          SI ~ BUF;  2(36(IF SC > "9" THEN JUMP OUT 2 TO ERRQM ELSE     20133000
               IF SC ="~" THEN ELSE SI ~ SI+1));                        20135000
NEXT:     SI ~ CONLIST;  IF SC ! "0" THEN GO TO ERRL;                   20137000
          SI ~ CODELIST;  IF SC ! "0" THEN GO TO ERRL;                  20139000
          DI ~ LOC T;  DS ~ 8 LIT "0"; COMMENT  CLEAR NEW SYMBOL;       20141000
          SI ~ BUF;  COMMENT  LOOK AT NEXT SYMBOL;                      20143000
DEBLANK:  IF SC = " " THEN BEGIN SI ~ SI + 1; GO TO DEBLANK; END;       20145000
          IF SC = "~" THEN GO TO XIT;                                   20147000
                    COMMENT  ~  TERMINATES CARD IMAGE;                  20149000
          BUF ~ SI; COMMENT  MARK START OF NEW SYMBOL;                  20151000
          ERRBUF ~ SI;                                                  20153000
          IF SC = "." THEN GO TO NUMB;                                  20155000
          IF SC = ALPHA THEN IF SC } "0" THEN BEGIN                     20157000
               COMMENT  PROCESSOR FOR A NUMBER STRING;                  20159000
          NUMB:     SI ~ SI + 1;  TALLY ~ 1;                            20161000
               15(  IF SC = "." THEN GO TO NUMBR;                       20163000
                    IF SC } "0" THEN BEGIN                              20165000
                    NUMBR:    SI ~ SI + 1;  TALLY ~ TALLY + 1;          20167000
                    END ELSE BEGIN                                      20169000
                    T2 ~ TALLY;  SI ~ LOC T2;  SI ~ SI + 7;             20171000
                    DI ~ CONLIST;  DS ~ 1 CHR;  SI ~ BUF;               20173000
                    DS ~ T2 CHR;COMMENT  CHARACTER COUNT AND STRING PKD;20175000
                    BUF ~ SI; COMMENT  UPDATE SCAN POINTER;             20177000
                    SI ~ CONLIST;  SI ~ SI + 16;  CONLIST ~ SI;         20179000
                    DI ~ CODELIST;   DI ~ DI + 6;  DS ~ 2 LIT "NO";     20181000
                    CODELIST ~ DI;  COMMENT  UPDATE CODE POINTER;       20183000
                    SI ~ BUF;                                           20185000
                    JUMP OUT 1 TO EXPO; END );    GO TO ERR;            20187000
                                                                        20189000
EXPO:     IF SC = "@" THEN GO TO EXP;                                   20191000
                    IF SC = "E" THEN BEGIN                              20193000
                    EXP: SI ~ SI +1;                                    20195000
                    E1:  IF SC = " " THEN BEGIN SI ~ SI + 1;            20197000
                              GO TO E1; END;                            20199000
                         IF SC = "+" THEN GO TO E2;                     20201000
                         IF SC } "0" THEN GO TO E2;                     20203000
                         IF SC = "-" THEN BEGIN                         20205000
                              E2:  DI ~ CONLIST;  DS ~ 1 LIT "E";       20207000
                                   DS ~ 1 CHR;                          20209000
                                   DI ~ DI + 6;  CONLIST ~ DI;  DI ~ DI 20211000
                                   - 6;  3(IF SC } "0" THEN DS ~ CHR);  20213000
                                   DS ~ 1 LIT ".";  BUF ~ SI;           20215000
                                   END;                                 20217000
                    END PACKING EXPONENTIAL;  GO TO NEXT;               20219000
          END NUMBER STRING ELSE BEGIN                                  20221000
                    COMMENT  ALPHA STRING;                              20223000
                    TALLY ~ 1;  SI ~ SI + 1;                            20225000
               7(   IF SC = ALPHA THEN BEGIN SI ~ SI + 1;  TALLY ~ TALLY20227000
                    + 1; END ELSE BEGIN                                 20229000
               A:   T2 ~ TALLY;  SI ~ LOC T2; SI ~ SI + 7;              20231000
                    DI ~ LOC T;  DS ~ 1 CHR;  SI ~ BUF;  DS ~ T2 CHR;   20233000
                    BUF ~ SI;  JUMP OUT 1 TO SERCH; END);               20235000
               8(   IF SC = ALPHA THEN BEGIN SI ~ SI +1; TALLY~TALLY    20237000
                    + 1;  END ELSE GO TO A);  GO TO ERR;                20239000
          END PROCESSING NUMBER OR ALPHA STRING;                        20241000
          IF SC = """ THEN BEGIN SI ~ SI + 1;                           20243000
                    DI ~ CODELIST;  DI ~ DI + 6;  DS ~ 2 LIT "ST";      20245000
                    CODELIST ~ DI;  COMMENT  ENTER STRING CONTROL WORD; 20247000
          STRING:   DI ~ CONLIST;  DS ~ 1 LIT "H";  DI ~ DI + 7;        20249000
                    CONLIST ~ DI;  DI ~ DI - 7;                         20251000
               7(   IF SC = "~" THEN JUMP OUT 1 TO ERRQ;                20253000
                    IF SC = """ THEN BEGIN DS ~ 1 CHR; BUF ~ SI;        20255000
                    JUMP OUT 1 TO NEXT; END ELSE DS ~ 1 CHR);           20257000
                    GO TO STRING;  COMMENT  MANY 7  CHARACTER ENTRIES ; 20259000
                    END STRING;                                         20261000
          COMMENT  FOUND A NON-ALPHA CHARACTER;                         20263000
          DI ~ LOC T;  DS ~ 1 LIT "1";  DS ~ 1 CHR;                     20265000
          SI~SI-1; IF SC="*" THEN BEGIN SI~SI+1; IF SC="*" THEN DS~1CHR;20267000
               END ELSE SI~SI+1;                                        20269000
          SI~SI-1;  IF SC=":" THEN BEGIN SI~SI+1;  IF SC="=" THEN       20271000
               DS~1 CHR; END ELSE SI~SI+1;                              20273000
          BUF~SI;                                                       20275000
SERCH:    SI ~ BUF;  COMMENT  SEARCH DICT AND ID LIST FOR SYMBOL;       20277000
SLBRK:    IF SC = " " THEN BEGIN SI ~ SI + 1; GO TO SLBRK; END;         20279000
          IF SC = "[" THEN BEGIN                                        20281000
               DI ~ LOC T;  SKIP 1 DB;  DS ~ 1 SET;                     20283000
               COMMENT  MARK ARRAY IDENTIFIER;                          20285000
               SI ~ SI + 1;  BUF ~ SI;  END  LEFT BRACKET;              20287000
               SI ~ DICT;                                               20289000
               63(  DI ~ LOC T;  IF 6 SC = DC THEN JUMP OUT TO DFOUND   20291000
                    ELSE SI ~ SI + 2);                                  20293000
               COMMENT   LENGTH OF DICT IS NEEDED HERE;                 20295000
               DICTSIZEMINUS63                                          20297000
                 (DI~LOC T;  IF 6 SC = DC THEN JUMP OUT TO DFOUND       20299000
                         ELSE SI ~ SI + 2);  GO TO IDSERCH;             20301000
DFOUND:   DI ~ CODELIST;   DI ~ DI + 6;  DS ~ 2 CHR;  CODELIST ~ DI;    20303000
          SI ~ SI - 2;  IF SC = REMLETR1 THEN BEGIN SI ~ SI + 1;        20305000
               IF SC = REMLETR2 THEN GO TO XIT; END;                    20307000
               COMMENT  CHECK FOR COMMENT CODE;                         20309000
          IF SC = FORMLETR1 THEN BEGIN SI~SI+1;                         20311000
               IF SC = FORMLETR2 THEN BEGIN                             20313000
                    SI~CODELIST;  SI~SI-10;  IF SC="N" THEN GO TO XIT;  20315000
                    END SKIPPING FORM STATEMENT;                        20317000
               END;                                                     20319000
          IF SC = FUNCTLETR1 THEN BEGIN                                 20321000
               COMMENT  CHECK FOR <FCN NAME> (  FOR A FUNCT NAME;       20323000
               SI ~ BUF;                                                20325000
               63(IF SC ! " " THEN IF SC = "(" THEN JUMP OUT 1 TO NEXT  20327000
                    ELSE JUMP OUT ELSE SI ~ SI +1);                     20329000
               DI ~ CODELIST;  DI ~ DI - 8;  CODELIST ~ DI;             20331000
          END FUNCTION ELSE GO TO NEXT;                                 20333000
IDSERCH:  SI ~ LOC T;  SI ~ SI + 1;  IF SC = ALPHA THEN ELSE GO TO ERR; 20335000
               COMMENT  IDENTIFIER MUST START WITH ALPHA;               20337000
          TALLY ~ 0;  SI ~ IDLIST;                                      20339000
          63(  IF SC = "0" THEN BEGIN                                   20341000
                    T1 ~ SI;  DI ~ T1;  SI ~ LOC T;  DS ~ 8 CHR;        20343000
                    DS ~ 8 LIT "E";  SI ~ SI - 8;  END ENTERING ID;     20345000
               DI ~ LOC T;  IF 8 SC = DC THEN BEGIN                     20347000
                    T2 ~ TALLY;  DI ~ CODELIST;  DI ~ DI + 6;           20349000
                    SI~BUF; SI~SI-1; IF SC="[" THEN DS~1 LIT            20351000
                    ARRAYIDLETTER ELSE DS~1 LIT IDLETTER; SI~LOC T2;    20353000
                    SI~SI+7;                                            20355000
                    DS ~ 1 CHR;  CODELIST ~ DI;  JUMP OUT 1 TO NEXT;    20357000
               END ELSE BEGIN TALLY ~ TALLY + 1;  SI ~ SI + 8; END);    20359000
               DI ~ ERRLIST;                                            20361000
               DS ~ 36 LIT "MORE THAN 63 IDENTIFIERS, ERASE SOME";      20363000
               DS ~ 1 LIT "~";                                          20365000
               TALLY ~ 0;  SCAN ~ TALLY;  GO TO XIT;                    20367000
ERRQ:     DI ~ ERRLIST;  DS ~ 20 LIT "QUOTES DO NOT MATCH~";            20369000
          TALLY ~ 0;  SCAN ~ TALLY;  GO TO XIT;                         20371000
ERRQM:    DI ~ ERRLIST;  DS ~ 29 LIT "QUESTION MARK IS NOT ALLOWED~";   20373000
          TALLY ~ 0;  SCAN ~ TALLY;  GO TO XIT;                         20375000
ERRL:     DI ~ ERRLIST;  DS ~ 17 LIT "TOO MANY SYMBOLS~";               20377000
          TALLY ~ 0;  SCAN ~ TALLY;  GO TO XIT;                         20379000
ERR:      DI ~ ERRLIST;  DI ~ DI+8;  DS ~ 15 LIT" ";  DI ~ ERRLIST;     20381000
          DS~8 LIT " ERROR: ";  T2~TALLY;  SI~LOC T2;  SI~SI+7;         20383000
          IF SC="0" THEN BEGIN SI~ERRBUF;  DS~CHR;  END ELSE BEGIN      20385000
          SI~ERRBUF;  DS~T2 CHR;  END;                                  20387000
          TALLY ~ 0;  DS ~ 1 LIT"~";  SCAN ~ TALLY;                     20389000
XIT:                                                                    20391000
END SCAN; %------------------------------------- ---------------------- 20393000
          IF SCAN(INFO[IOBUF],SYNTAX[TEMPCODE],IDLIST[0],               20395000
               SYNTAX[SCANCON], DICT[0], SYNTAX[SCANCODE], SYNTAX[0])   20397000
          THEN BEGIN                                                    20399000
               I ~ SCANCODE - 1; T1 ~ 0;  T2 ~ 0;  T3 ~ 0;              20401000
               FLAG ~ FALSE;                                            20403000
               WHILE T ~ SYNTAX[I ~ I + 1] ! ENDR DO BEGIN              20405000
                    COMMENT   LOOK AT EACH SYMBOL FROM SCAN;            20407000
                    IF T.CLASSFIELD = ARRAYID THEN T1 ~ T1 + 1;         20409000
                         COMMENT   COUNT LEFT BRACKETS;                 20411000
                    IF T = DRBRK THEN IF T1 ~ T1 - 1 < 0 THEN GO TO     20413000
                         BRKERR;  COMMENT  BRACKETS DONT MATCH;         20415000
                    IF T = DLPAREN THEN T2 ~ T2 + 1;                    20417000
                    IF T=DRPAREN THEN IF T2~T2-1<0 THEN GO TO PARENERR; 20419000
                    IF T = "NO" THEN BEGIN  COMMENT  SET # REL. ADDR. ; 20421000
                         SYNTAX[I] ~ CON | 64 + T3;                     20423000
                         IF SYNTAX[SCANCON + (T3~T3+2)].[1:5] = "E"     20425000
                              THEN T3 ~ T3 + 1;  END  NUMBER;           20427000
                    IF T="ST" THEN BEGIN SYNTAX[I]~STRNG|64+T3;         20429000
                    T3~T3+STRNGLENGTH(SYNTAX[SCANCON+T3]); END STRING;  20431000
                    IF T.CLASSFIELD = OPR OR T.CLASSFIELD = ADSUBOP     20433000
                         THEN IF FLAG THEN GO TO TWOOPS                 20435000
                              ELSE FLAG ~ TRUE ELSE FLAG ~ FALSE;       20437000
                    END WHILE;                                          20439000
               IF T1 ! 0 THEN GO TO BRKERR;                             20441000
               IF T2 ! 0 THEN GO TO PARENERR;                           20443000
               PROGMODE ~ FALSE;                                        20445000
          IF INFO[MODE] = 0 THEN BEGIN  COMMENT  OK TO TRANSLATE;       20447000
               IF T1 ~ ( T ~ SYNTAX[I ~ SCANCODE]).CLASSFIELD = CON     20449000
                    THEN BEGIN PROGMODE ~ TRUE;                         20451000
                    IF T1 ~ (T ~ SYNTAX[I ~ I + 1]). CLASSFIELD = CONV  20453000
                    THEN GO TO CLASSERR1;  END PROGRAM MODE SETUP       20455000
               ELSE IF T1 ~ T.CLASSFIELD = PROG THEN GO TO CLASSERR2;   20457000
               T2 ~ -100;                                               20459000
               IF T1 = PROG THEN T2 ~ PROGINDX;                         20461000
               IF T1 = CONV THEN T2 ~ CONVINDX;                         20463000
               IF T1 = EITHER THEN T2 ~ EITHERINDX;                     20465000
               COMMENT CHECK FOR DATA AND RESTORE.                      20467000
                    ":0" IS THE CODE FOR =     ;                        20469000
               IF T2 < 0 THEN IF T1 = IDLETTER THEN                     20471000
               BEGIN                                                    20473000
                    IF (T4 ~ IDLIST[(T3 ~ T.KEYFIELD)+T3]).[1:29] =     20475000
                         "4DATA" THEN IF SYNTAX[I+1] ! ":0" THEN        20477000
                    BEGIN                                               20479000
                         SCANNER ~ DATAINDX;                            20481000
                         GO TO XIT;                                     20483000
                    END;                                                20485000
                    IF T4.[1:29] = "4GOTO" THEN IF SYNTAX[I+1]!":0" THEN20485100
                    BEGIN                                               20485200
                         SCANNER ~ PROGINDX; COMMENT GO TO IS PROG,0;   20485300
                         GO TO XIT;                                     20485400
                    END;                                                20485500
                    IF T4.[1:35] = "8CONTI" THEN IF SYNTAX[I+1]=0 THEN  20485600
                    BEGIN                                               20485700
                         SCANNER ~ REMARKINDX;                          20485800
                         GO TO XIT;                                     20485900
                    END;                                                20486000
                    IF T4.[1:35] = "7RESTO" THEN IF SYNTAX[I+1] = 0 THEN20487000
                    BEGIN                                               20489000
                         SCANNER ~ RESTOREINDX;                         20491000
                         GO TO XIT;                                     20493000
                    END;                                                20495000
               END;                                                     20497000
                    IF T2 < 0 THEN IF T1 = IDLETTER OR T1 = ARRAYID     20499000
                         THEN SCANNER ~ REPLINDX ELSE                   20501000
                         IF SYNTAX[SCANCODE] = ENDR THEN SCANNER ~ 1    20501100
                         ELSE GO TO CLASSERR;                           20501200
                         COMMENT SCANNER = 1 DENOTES NULL STMT;         20501300
                    IF T2}0 THEN SCANNER~T2+T.KEYFIELD;                 20503000
                    IF T1=IDLETTER OR T1=ARRAYID THEN SCANNER~REPLINDX; 20505000
          END ELSE SCANNER ~ -1;   COMMENT  ACCEPT STATEMENT INPUT;     20507000
          END  SCAN RETURNED TRUE;                                      20509000
          GO TO XIT;                                                    20511000
BRKERR:   ERRNUMBER~1; GO TO XIT;                                       20513000
          COMMENT BRACKETS DO NOT MATCH;                                20515000
PARENERR: ERRNUMBER~2; GO TO XIT;                                       20517000
          COMMENT PARENTHESES DO NOT MATCH;                             20519000
CLASSERR: ERRNUMBER~3; GO TO XIT;                                       20521000
          COMMENT UNRECOGNIZABLE STATEMENT TYPE;                        20523000
CLASSERR1:ERRNUMBER~4; GO TO XIT;                                       20525000
          COMMENT STATEMENT IS ILLEGAL IN PROGRAM MODE;                 20527000
CLASSERR2:ERRNUMBER~5; GO TO XIT;                                       20529000
          COMMENT STATEMENT IS ILLEGAL IN CALCULATOR MODE;              20531000
TWOOPS:   ERRNUMBER~6;                                                  20533000
          COMMENT ADJACENT OPERATORS;                                   20535000
XIT:                                                                    20537000
IF DEBUG THEN BEGIN                                                     20539000
WRITE (PR[DBL], 10, SYNTAX[*]);                                         20541000
WRITE (PR[DBL], E, FOR T ~ SCANCODE STEP 1 UNTIL SCANCODE +9            20543000
       DO SYNTAX[T]);                                                   20545000
WRITE(PR[DBL],EE,FOR T~SCANCON STEP 1 UNTIL SCANCON+9 DO SYNTAX[T]);    20547000
WRITE (PR, DMPIDS);                                                     20549000
WRITE (PR[DBL], 17, IDLIST[*]);                                         20551000
END DEBUG;                                                              20553000
END SCANNER;                                                            20555000
% ********************************************************************  30001000
% ********************************************************************  30003000
% ************* E N T E R D I R E C T O R Y  *************************  30005000
% ********************************************************************  30007000
% ********************************************************************  30009000
COMMENT                                                                 30011000
          ENTER DIRECTORY INSERTS THREE CONTROL WORDS INTO THE FILE     30013000
          DIRECTORY FOR EACH FILE;                                      30015000
COMMENT                                                                 30017000
          DIRECTORY SEGMENTS ARE IDENTIFIED BY WORD 0 AND ARE CHAINED   30019000
          BY WORD 1.  THUS THERE ARE 9 ENTRIES PER SEGMENT.  (THE       30021000
          SECOND WORD IS UNUSED);                                       30023000
COMMENT                                                                 30025000
     DIRECTORY FORMAT:                                                  30027000
          WORD 0:   FILE SECURITY USER ID IF > 0, OR IF -1 THEN THE     30029000
                    FILE HAS BEEN PURGED AFTER A RESTART OR A QUIT.     30031000
          WORD 1:   DISK POINTER TO BEGINNING OF FILE.                  30033000
          WORD 2:   CREATION TIME AND DATE.                             30035000
                    [22:17] TIME OF DAY / 3600                          30037000
                    [39:9] DATE (1 - 366);                              30039000
COMMENT                                                                 30041000
          WORDS 3-5 OF RECORD 0 ARE RESERVED, SEE INITIALIZE;           30043000
PROCEDURE ENTERDIRECTORY(INFO); ARRAY INFO[0];                          30045000
BEGIN DEFINE SEGMENT=#; COMMENT NEW SEGMENT HERE;                       30047000
     IF DENTRY~DENTRY+3=30 THEN                                         30049000
     BEGIN                                                              30051000
          T~DLOC;                                                       30053000
          DLOC~(IF DLOC=0 THEN DIRSPACE ELSE DLOC+1);   DENTRY~3;       30055000
          DIRBUF[1]~DLOC;                                               30057000
          WRITE(DSK[T],30,DIRBUF[*]);                                   30059000
          FOR T~1 STEP 1 UNTIL 29 DO DIRBUF[T]~0;                       30061000
     END STARTING A NEW DIRECTORY SEGMENT;                              30063000
     DIRBUF[DENTRY]~USERID[USER]; COMMENT WORD 0 = USERID;              30065000
     DIRBUF[DENTRY+1]~DISKPOINTER; COMMENT STARTING CARD LOCATION;      30067000
     T7~TIME(0);                                                        30069000
     DIRBUF[DENTRY+2]~ENTIER(T7.[30:6]|100+T7.[36:6]|10+T7.[42:6])&     30071000
          (TIME(1) DIV 3600) [22:31:17];                                30073000
          COMMENT CREATION TIME AND DATE;                               30075000
     IF DLOC=0 AND DENTRY=6 THEN DIRBUF[3]~-1;                          30077000
     WRITE (DSK[DLOC],30,DIRBUF[*]);                                    30079000
          COMMENT SAVE DIRECTORY ON DISK ALSO;                          30081000
     IF RESTART THEN IF REAL(RESTART)=1 THEN                            30083000
     BEGIN                                                              30085000
          READ(DSK[0],30,DIRBUF[*]);                                    30087000
          DIRBUF[3]~-1;                                                 30089000
          WRITE(DSK[0],30,DIRBUF[*]);                                   30091000
          READ(DSK[DLOC],30,DIRBUF[*]);                                 30093000
          RESTART~BOOLEAN(3);                                           30095000
     END;                                                               30097000
     INFO[FILEID]~DISKPOINTER&(DLOC|32+DENTRY)[3:18:30];                30099000
END ENTERDIRECTORY;                                                     30101000
% ********************************************************************  30103000
% ********************************************************************  30105000
% *************************** D I R E C T O R Y  ********************** 30107000
% ********************************************************************  30109000
% ********************************************************************  30111000
COMMENT                                                                 30113000
          DIRECTORY SEARCH ROUTINE.  THIS ROUTINE SEARCHES THE DIRECTORY30115000
          FOR AN ENTRY WITH A MATCHING USER ID.  IF AN ENTRY IS FOUND,  30117000
          THE PROCEDURE IS SET TO TRUE AND D1 AND D2 ARE SET TO WORDS   30119000
          1 AND 2 OF THE CORRESPONDING DIRECTORY ENTRY;                 30121000
BOOLEAN PROCEDURE DIRECTORY(UID,D1,D2); VALUE UID; REAL UID,D1,D2;      30123000
BEGIN ARRAY DBUF[0:29]; LABEL XIT;                                      30125000
     T6~0;                                                              30127000
     T7~6;                                                              30129000
     DO BEGIN                                                           30131000
          READ(DSK[T6],30,DBUF[*]);                                     30133000
          DO BEGIN                                                      30135000
               IF DBUF[T7]=UID THEN                                     30137000
               BEGIN                                                    30139000
                    DIRECTORY~TRUE;                                     30141000
                    D1~DBUF[T7+1];                                      30143000
                    D2~DBUF[T7+2];                                      30145000
                    GO TO XIT;                                          30147000
               END A SUCCESSFUL DIRECTORY SEARCH;                       30149000
          END UNTIL T7~T7+3=30;                                         30151000
          T7~3;                                                         30153000
     END UNTIL T6~DBUF[1]=0;                                            30155000
XIT:                                                                    30157000
END DIRECTORY;                                                          30159000
% ********************************************************************  40001000
% ********************************************************************  40003000
%  ************************** L O A D E R  *****************************40005000
% ********************************************************************  40007000
% ********************************************************************  40009000
COMMENT                                                                 40011000
          LOADER READS CARDS FROM THE SYSTEM DISK.  IT PUTS CARD IMAGES 40013000
          INTO THE SYNTAX[0] ARRAY.  LOADLINK IS THE FILE NAME;         40015000
BOOLEAN PROCEDURE LOADER(INFO); ARRAY INFO[0];                          40017000
BEGIN                                                                   40019000
LABEL XIT;                                                              40021000
STREAM PROCEDURE MOVEG(D);                                              40023000
BEGIN  LOCAL T;                                                         40025000
     SI~D;  SI~SI-1;                                                    40027000
     63(IF SC ! " " THEN JUMP OUT ELSE SI~SI-1);                        40029000
     SI~SI+1;  T~SI;  DI~T;  DS~LIT "~";                                40031000
END MOVEG;                                                              40033000
     LOADER~TRUE;                                                       40035000
     IF T~INFO[FILELIMITS].[33:15]=0 THEN                               40037000
     BEGIN                                                              40039000
          COMMENT ABOUT TO LOAD FIRST CARD;                             40041000
          FILL LOADFILE WITH INFO[LOADLINK];                            40043000
          SEARCH(LOADFILE,SYNTAX[*]);                                   40045000
          IF SYNTAX[0] { 0 THEN                                         40047000
          BEGIN                                                         40049000
               LOADER~FALSE;                                            40051000
               GO TO XIT;                                               40053000
          END NO FILE PRESENT;                                          40055000
          INFO[FILELIMITS].[18:15]~SYNTAX[5]; COMMENT EOF POINTER;      40057000
     END SEARCH AND INITIALIZE;                                         40059000
     IF T>INFO[FILELIMITS].[18:15] THEN                                 40061000
     BEGIN                                                              40063000
          LOADER~FALSE;                                                 40065000
          CLOSE(LOADFILE);                                              40067000
     END ELSE                                                           40069000
     BEGIN                                                              40071000
          READ(LOADFILE,10,SYNTAX[*]);                                  40073000
          MOVEG(SYNTAX[9]);                                             40075000
     END LOADING CARD IMAGE;                                            40077000
     INFO[FILELIMITS].[33:15]~T+1;                                      40079000
          COMMENT BUMP RECORD POINTER;                                  40081000
XIT:                                                                    40083000
END LOADER;                                                             40085000
% ********************************************************************  40087000
% ********************************************************************  40089000
% ***********************  H E L P L O A D E R  **********************  40091000
% ********************************************************************  40093000
% ********************************************************************  40095000
COMMENT  HELPLOADER IS CALLED BY INITIALIZE IF NO SAVFILE IS PRESENT.   40097000
          IT READS THE DISK FILE  HELPFIL/CARDS AND RE-FORMATS IT;      40099000
COMMENT  CARD FORMAT:                                                   40101000
     A.   LEFT JUSTIFIED STATEMENT NUMBER.                              40103000
     B.   ONE TO FIVE BLANKS.                                           40105000
     C.   "REM"  OPTIONAL  (OR "REM ").                                 40107000
     D.   *<KEYWORD>   OPTIONAL  (<KEYWORD> = DO, IF, ETC.).            40109000
     E.   INFORMATION;                                                  40111000
PROCEDURE HELPLOADER;                                                   40113000
          BEGIN                                                         40115000
          LABEL XIT;  ARRAY LOADBUF[0:9];                               40117000
          DEFINE HELP = LOADFILE#, TEMPDISKBUF = DISKBUF#;              40119000
          DEFINE INFO = SYNTAX#;                                        40121000
          LABEL LOOP;                                                   40123000
STREAM PROCEDURE SETCOL73(D);                                           40125000
          BEGIN LOCAL T;                                                40127000
          SI ~ D;  SI ~ SI+32;  SI ~ SI+40;                             40129000
          63(SI ~ SI-1;  IF SC ! " " THEN JUMP OUT);                    40131000
          SI ~ SI+1;  T ~ SI;  DI ~ T;                                  40133000
          DS ~ LIT "~";   END SET COL 73;                               40135000
REAL STREAM PROCEDURE FORMATHELP(S, D, PART, STP);                      40137000
          BEGIN LOCAL T, REM, T2;                                       40139000
          DI ~ D;  DS ~ 8 LIT " ";  8(DS ~ 8 LIT "~");                  40141000
          SI ~ S;  DI ~ PART;  SI ~ SI+1;                               40143000
          IF SC = "." THEN BEGIN SI ~ S;  DS ~ 1 OCT; END ELSE          40145000
               BEGIN SI ~ S;  DS ~ 2 OCT;  END;                         40147000
          T ~ SI;  SI ~ PART;  DI ~ D;  DS ~ 2 DEC;  DS ~ 1 LIT ".";    40149000
          DI ~ D;  DS ~ 2 FILL;                                         40151000
          SI ~ T;  SI ~ SI+1;DI ~ D;  DI ~ DI+3;                        40153000
          3(IF SC = " " THEN DS ~ LIT "0" ELSE DS ~ CHR);               40155000
          T ~ SI;                                                       40157000
          SI ~ D;  SI ~ SI+3;  DI ~ STP;  DS ~ 3 OCT;                   40159000
          SI ~ D;  SI ~ SI+4;  2(IF SC = "0" THEN SI ~ SI+1 ELSE        40161000
          BEGIN  DI ~ D;  DS ~ 6 LIT " ";  JUMP OUT;  END);             40163000
          SI ~ T;  SI ~ SI+1;    5(IF SC = " " THEN SI ~ SI+1);         40165000
          DI ~ LOC REM;  DS ~ 3 LIT "REM";                              40167000
          DI ~ DI - 3;                                                  40169000
          IF 3 SC = DC THEN  ELSE SI ~ SI-3;                            40171000
          T ~ SI;                                                       40173000
          2(IF SC = "*" THEN BEGIN                                      40175000
               TALLY ~ 0;  SI ~ SI+1;                                   40177000
               DI ~ LOC FORMATHELP;  DI ~ DI+1;                         40179000
          6( IF SC = ALPHA THEN BEGIN DS ~ CHR; TALLY ~ TALLY+1;END);   40181000
               9(IF SC = ALPHA THEN BEGIN TALLY ~ TALLY+1;  SI~SI+1;    40183000
                    END);                                               40185000
               T2 ~ TALLY;  T~SI;                                       40187000
               SI ~ LOC T2;  SI ~ SI+7;  DI ~ LOC FORMATHELP; DS ~ CHR; 40189000
          JUMP OUT; END ELSE SI ~ SI+1);                                40191000
          SI ~ T;  DI ~ D;  DI ~ DI+5;                                  40193000
          63 (IF SC ! "~" THEN DS ~ CHR ELSE JUMP OUT);                 40195000
          END FORMATHELP;                                               40197000
%         -----                                                         40199000
               FILL HELP WITH "HELPFIL";                                40201000
               SEARCH(HELP, LOADBUF[*]);                                40203000
               IF LOADBUF[0] < 0 THEN GO TO XIT;  COMMENT  NO FILE;     40205000
               INFO[FILELIMITS] ~ 0&LOADBUF[5] [18:33:15];              40207000
               INFO[FILELIMITS].[3:15] ~ 3;                             40209000
LOOP:                                                                   40211000
          I~INFO[FILELIMITS].[33:15];                                   40213000
          READ(HELP,10,LOADBUF[*]);                                     40215000
          SETCOL73(LOADBUF[0]);                                         40217000
          HELPTABLE[I] ~ HELPTABLE[I+1] ~ 0;                            40219000
          HELPTABLE[I+1].CPARTNUMB ~ 99;                                40221000
          IF T4 ~ FORMATHELP(LOADBUF[0], TEMPDISKBUF[T6~(T5~INFO        40223000
               [FILELIMITS].[3:15]) MOD 3 | 10],                        40225000
               T1, T2) ! 0 THEN BEGIN  COMMENT  FOUND A KEYWORD;        40227000
               T ~ 1;                                                   40229000
               T4 ~ T4.[1:35];                                          40231000
               WHILE (T3~DICT[T].[1:35]) ! 0 AND T3 ! T4 DO T~T+1;      40233000
               IF T3=T4 THEN HELPTABLE[I] ~ DICT[T].[36:12];            40235000
               IF T4 = "4DATA0" THEN HELPTABLE[I] ~ "38";               40237000
               END KEYWORD SEARCH;                                      40239000
          HELPTABLE[I].CDISKADDR ~ T5;                                  40241000
          HELPTABLE[I].CPARTNUMB ~ T1;                                  40243000
          HELPTABLE[I].CSTEPNUMB ~ T2;                                  40245000
          IF DEBUG THEN WRITE (PR[DBL],17,TEMPDISKBUF[*]);              40247000
          IF T6=20 THEN WRITE(DSK[T5 DIV 3], 30, TEMPDISKBUF[*]);       40249000
          INFO[FILELIMITS].[3:15] ~ T5+1;                               40251000
          IF (I~I+1) > INFO[FILELIMITS].[18:15] THEN BEGIN              40253000
               CLOSE(HELP);                                             40255000
               WRITE (DSK[ T5 DIV 3    ], 30, TEMPDISKBUF[*]);          40257000
               FOR I ~ 0 STEP 1 UNTIL (HELPSIZE-29) DIV 30 DO           40259000
                    BEGIN   FOR T2 ~ 0 STEP 1 UNTIL 29 DO               40261000
                         SYNTAX[T2] ~ HELPTABLE[I|30+T2] ;              40263000
                    WRITE(DSK[I+HELPARRAYSTART],30,SYNTAX[*]); END;     40265000
          END ELSE BEGIN INFO[FILELIMITS].[33:15] ~ I;                  40267000
               GO TO LOOP; END;                                         40269000
     XIT:                                                               40271000
          IF DEBUG THEN WRITE(PR[DBL],H1,FOR I~0 STEP 1 UNTIL HELPSIZE  40273000
               DO HELPTABLE[I]);                                        40275000
          END HELPLOADER;                                               40277000
COMMENT                                                                 40279000
     HELP:          TYPES ALL THE STATEMENTS IN PART 1 AND ALL LINES    40281000
                    WITH STEP NUMBER = 0.                               40283000
     HELP  5:       TYPES ALL OF THE STATEMENTS IN PART 5.              40285000
     HELP <KEYWORD> TYPES ALL THE STATEMENTS CONTAINING *<KEYWORD>  ;   40287000
% ******************* TRANSLATOR DECLARATIONS   *********************   50001000
COMMENT  THE FOLLOWING LIST DEFINES THE OP-CODES FOR EXECUTE;           50003000
DEFINE LITERAL = 1#;                                                    50005000
DEFINE IDFETCH = 2#;                                                    50007000
DEFINE CONSTANT = 3#;                                                   50009000
DEFINE ARRAYFETCH = 4#;                                                 50011000
DEFINE ARITHOPR=5#;                                                     50013000
DEFINE RELTEST=6#;                                                      50015000
DEFINE FUNCTCALL=7#;                                                    50017000
DEFINE NDSTORE=8#;                                                      50019000
DEFINE DSTORE=9#;                                                       50021000
DEFINE NDSTOREA=10#;                                                    50023000
DEFINE DSTOREA=11#;                                                     50025000
DEFINE DOPART=12#;                                                      50027000
DEFINE GOTO=13#;                                                        50029000
DEFINE BRANCHBACK=14#;                                                  50031000
DEFINE DECRSTACKANDSKIP=15#;                                            50033000
DEFINE INTRINSIC=16#;                                                   50035000
DEFINE NEXTLINE=17#;                                                    50037000
DEFINE IFNOTRUSKIP=18#;                                                 50039000
DEFINE PRINTSTRING=19#;                                                 50041000
DEFINE PRINTEXP=20#;                                                    50043000
DEFINE STOP=21#;                                                        50045000
DEFINE IDACCEPT=22#;                                                    50047000
DEFINE ARRAYACCEPT=23#;                                                 50049000
DEFINE FORTRANIF = 24#;                                                 50051000
DEFINE FORM = 25#;                                                      50053000
DEFINE STRINGFETCH = 26#;                                               50055000
DEFINE IDREADDATA = 27#;                                                50057000
DEFINE ARRAYREADDATA = 28#;                                             50059000
COMMENT  END EXECUTE DEFINES;                                           50061000
BOOLEAN INHIBITCODE;                                                    50063000
INTEGER SAV;                                                            50065000
INTEGER INDEX;                                                          50067000
DEFINE PDS=22#;                                                         50069000
DEFINE HOLD=47#;                                                        50071000
DEFINE OPSTACK=72#;                                                     50073000
INTEGER PDSINDEX,HOLDINDEX,OSINDEX;                                     50075000
DEFINE NEXT=I~I+1#;                                                     50077000
DEFINE BACKUP=I~I-1#;                                                   50079000
INTEGER TEMPCODEINDEX;                                                  50081000
DEFINE UNSIGNNUM=SYNTAX[I].[36:6]="7"#;                                 50083000
DEFINE ARITHOP=SYNTAX[I].[36:6]=10 OR SYNTAX[I].[36:6]="8"#;            50085000
DEFINE RELOP=SYNTAX[I].[36:6]=":"#;                                     50087000
DEFINE IDEN=SYNTAX[I].[36:6]="5"#;                                      50089000
DEFINE ARRAYIDEN=SYNTAX[I].[36:6]="6"#;                                 50091000
DEFINE FNCIDEN=SYNTAX[I].[36:6]="@"#;                                   50093000
DEFINE STRING=SYNTAX[I].[36:6]=15#;                                     50095000
DEFINE STMTNR=SYNTAX[I].[36:6]="7"#;                                    50097000
DEFINE QRIGHTBRACKET=SYNTAX[I]="91"#,                                   50099000
       ENDCODE      =SYNTAX[I]="00"#,                                   50101000
       QTHEN        =SYNTAX[I]=">5"#,                                   50103000
       QPLUS        =SYNTAX[I]="80"#,                                   50105000
       QMINUS       =SYNTAX[I]="81"#,                                   50107000
       QLEFTPAREN   =SYNTAX[I]="40"#,                                   50109000
       QGO          =SYNTAX[I]="30"#,                                   50111000
       QREAD        =SYNTAX[I]="21"#,                                   50113000
       QPRINT       =SYNTAX[I]="22"#,                                   50115000
       QIF          =SYNTAX[I]="36"#,                                   50117000
       QDO          =SYNTAX[I]="32"#,                                   50119000
       QWHILE       =SYNTAX[I]="33"#,                                   50121000
       QUNTIL       =SYNTAX[I]="34"#,                                   50123000
       QDECLARE     =SYNTAX[I]="35"#,                                   50125000
       QCOMMA       =SYNTAX[I]=">0"#,                                   50127000
       QEQUAL       =SYNTAX[I]=":0"#,                                   50129000
       QELSE        =SYNTAX[I]=">2"#,                                   50131000
       QTO          =SYNTAX[I]=">4"#,                                   50133000
       QPART        =SYNTAX[I]=">#"#,                                   50135000
       QTIMES       =SYNTAX[I]=">@"#,                                   50137000
       QFOR         =SYNTAX[I]=">:"#,                                   50139000
       QBY          =SYNTAX[I]=">1"#,                                   50141000
       QSTOP        =SYNTAX[I]="31"#,                                   50143000
       QSET         =SYNTAX[I]="20"#,                                   50145000
       QSTEP        =SYNTAX[I]=">6"#,                                   50147000
       QYES         =SYNTAX[I]=">7"#,                                   50149000
       QNO          =SYNTAX[I]=">}"#,                                   50151000
       QFORM = SYNTAX[I]="37"#,                                         50153000
       QRIGHTPAREN  =SYNTAX[I]="90"#;                                   50155000
DEFINE PART=">#"#;                                                      50157000
DEFINE QPLOT=IDEN AND IDENTIFIERS[USER,(T1~SYNTAX[I].                   50157100
     [42:6])+T1].[1:29]="4PLOT"#;                                       50157200
DEFINE QTAB=IDEN AND IDENTIFIERS[USER,(T1~SYNTAX[I].                    50157300
     [42:6])+T1].[1:23]="3TAB"#;                                        50157400
DEFINE QREPEAT = IDEN AND IDENTIFIERS[USER,(T1~SYNTAX[I].[42:6])+T1].   50157500
     [1:35] = "6REPEA"#;                                                50157600
DEFINE ELSECODE=">2"#;                                                  50159000
ARRAY STACK[0:110];                                                     50161000
COMMENT  THIS IS THE TRANSLATOR STACK, NOT STACK[USER,*];               50163000
% ********************************************************************  50165000
% ********************************************************************  50167000
% ********************* T R A N S L A T O R  ***************************50169000
% ********************************************************************  50171000
% ********************************************************************  50173000
PROCEDURE TRANSLATOR(INFO, IDENT, CODE, USERARRAY);                     50175000
          ARRAY INFO[0], IDENT[0], CODE[0], USERARRAY[0];               50177000
          COMMENT    THIS PROCEDURE PRODUCES CODE IN THE CODE ARRAY;    50179000
          COMMENT   THE TRANSLATION ALGORITHM IS BASICLY RECURSIVE      50181000
                    DESCENT;                                            50183000
          BEGIN                                                         50185000
          LABEL XIT;                                                    50187000
          LABEL EXPERR;   MONITOR EXPOVR;                               50189000
                                                                        50191000
          DEFINE LASTLINK = T4#;                                        50193000
PROCEDURE STORE(INFO, CODECELL);  ARRAY INFO[0];  REAL CODECELL;        50195000
          FORWARD;                                                      50197000
% ------------ HIERARCHY ------------------------                       50199000
INTEGER PROCEDURE HIERARCHY(N);                                         50201000
INTEGER N;                                                              50203000
BEGIN                                                                   50205000
     FOR INDEX~0 STEP 2 UNTIL 20 DO IF STACK[INDEX] = N                 50207000
    THEN HIERARCHY~STACK[INDEX+1];                                      50209000
END HIERARCHY;                                                          50211000
%------------- PUSHDOWNI ------------------------                       50213000
PROCEDURE PUSHDOWNI;                                                    50215000
BEGIN STACK[(PDSINDEX~PDSINDEX+1)-1]~I END;                             50217000
%------------- POPUPI ---------------------------                       50219000
INTEGER PROCEDURE POPUPI;                                               50221000
BEGIN POPUPI~STACK[PDSINDEX~PDSINDEX-1] END;                            50223000
%------------- SAVEI ----------------------------                       50225000
PROCEDURE SAVEI;                                                        50227000
BEGIN STACK[(HOLDINDEX~HOLDINDEX+1)-1]~I END;                           50229000
%------------- RESTORE --------------------------                       50231000
INTEGER PROCEDURE RESTORE;                                              50233000
BEGIN RESTORE~STACK[HOLDINDEX~HOLDINDEX-1] END;                         50235000
%------------- GENCODE --------------------------                       50237000
PROCEDURE GENCODE(TYPE,CODEWORD);                                       50239000
VALUE CODEWORD;                                                         50241000
INTEGER TYPE,CODEWORD;                                                  50243000
BEGIN                                                                   50245000
    IF NOT INHIBITCODE THEN                                             50247000
    IF TYPE!5 THEN SYNTAX[(TEMPCODEINDEX~TEMPCODEINDEX+1)-1]~0&         50249000
    TYPE[36:42:6]&CODEWORD[42:42:6] ELSE                                50251000
    BEGIN                                                               50253000
        IF CODEWORD.[36:6]=10 THEN CODEWORD.[42:6]~CODEWORD.[42:6]+2;   50255000
        SYNTAX[(TEMPCODEINDEX~TEMPCODEINDEX+1)-1]~0&TYPE[36:42:6]&      50257000
        CODEWORD[42:42:6];                                              50259000
    END;                                                                50261000
END GENCODE;                                                            50263000
%------------- NCR ------------------------------                       50265000
INTEGER STREAM PROCEDURE NCR(S,N); VALUE N;                             50267000
BEGIN SI~S; SI~SI+N; DI~LOC NCR; DI~DI+7; DS~1 CHR; END;                50269000
     COMMENT RETURNS 1 CHARACTER;                                       50271000
%------------- SYNTAXERR ------------------------                       50273000
PROCEDURE SYNTAXERR(KEY); VALUE KEY; INTEGER KEY;                       50275000
     IF NOT ERRFLAG THEN BEGIN ERRFLAG~TRUE; ERRNUMBER~SYNTAXERRSTART+  50277000
     KEY; END;                                                          50279000
%------------- GETDIM, SETDIM, OLDSIZE, SETTOUNDEFINED, MOVEARRAY ----- 50281000
INTEGER PROCEDURE GETDIM(L); VALUE L; INTEGER L;                        50283000
COMMENT                                                                 50285000
     THIS PROCEDURE IS USED BY ARRAYDECL TO RETURN AN 8-BIT             50287000
     INTEGER USING AN INDEX INTO THE SCANCON TABLE;                     50289000
BEGIN                                                                   50291000
     INTG~0;                                                            50293000
     FOR C~1 STEP 1 UNTIL NCR(SYNTAX[SCANCON+L],0) DO                   50295000
          INTG~INTG|10+NCR(SYNTAX[SCANCON+L],C);                        50297000
     GETDIM~INTG;                                                       50299000
% ****    ADD A CHECK FOR INTEGERS                                      50301000
END GETDIM;                                                             50303000
PROCEDURE SETDIM(ARRAYWORD,SIZE);                                       50305000
VALUE SIZE; REAL ARRAYWORD,SIZE;                                        50307000
IF NOT ERRFLAG THEN                                                     50309000
BEGIN                                                                   50311000
     IF T1=4 THEN COMMENT MORE THAN 4 DIMENSIONS; SYNTAXERR(2) ELSE     50313000
     BEGIN                                                              50315000
          IF T1=0 THEN ARRAYWORD.[3:8]~SIZE ELSE                        50317000
          IF T1=1 THEN ARRAYWORD.[11:8]~SIZE ELSE                       50319000
          IF T1=2 THEN ARRAYWORD.[19:8]~SIZE ELSE                       50321000
          ARRAYWORD.[27:8]~SIZE;                                        50323000
          T3~T3|SIZE;                                                   50325000
          T1~T1+1;                                                      50327000
     END;                                                               50329000
END SETDIM;                                                             50331000
INTEGER PROCEDURE OLDSIZE(WORD); VALUE WORD; INTEGER WORD;              50333000
OLDSIZE~(IF T5~WORD.[1:2]=0 THEN WORD.[3:8] ELSE IF T5=1 THEN WORD.[3:8]50335000
     |WORD.[11:8] ELSE IF T5=2 THEN WORD.[3:8]|WORD.[11:8]|WORD.[19:8]  50337000
     ELSE WORD.[3:8]|WORD.[11:8]|WORD.[19:8]|WORD.[27:8]);              50339000
STREAM PROCEDURE SETTOUNDEFINED(M,N,D); VALUE M,N;                      50341000
BEGIN DI~D; M(32(DS~8 LIT "E")); N(DS~8 LIT "E"); END;                  50343000
STREAM PROCEDURE MOVEARRAY(M,N,S,D); VALUE M,N;                         50345000
BEGIN SI~S;  DI~D;  M(DS~ 32 WDS);   N(DS~WDS);  END;                   50347000
BOOLEAN PROCEDURE EXPRESSION   ; FORWARD;                               50349000
%                                                                       50351000
%***********************************************************************50353000
%                                                                       50355000
COMMENT                                                                 50357000
          <VARIABLE>     ::=  <IDEN>                                    50359000
                              <SUBSVAR>                                 50361000
                              <STRING>                               %%%50363000
          <SUBSVAR>      ::=  <ARRAYIDEN> <SUBSLIST> ]                  50365000
          <SUBSLIST>     ::=  <EXPRESSION>                              50367000
                              <EXPRESSION> , <SUBSLIST>                 50369000
          ;                                                             50371000
BOOLEAN PROCEDURE VARIABLE;                                             50373000
BEGIN REAL T,T1;                                                        50375000
BOOLEAN PROCEDURE SUBSLIST(T,T1); REAL T,T1;                            50377000
BEGIN                                                                   50379000
     IF EXPRESSION THEN                                                 50381000
     BEGIN                                                              50383000
          T1~T1+1;                                                      50385000
          NEXT;                                                         50387000
          IF QCOMMA THEN                                                50389000
          BEGIN                                                         50391000
               NEXT;                                                    50393000
               IF SUBSLIST(T,T1) THEN SUBSLIST~TRUE ELSE SUBSLIST~FALSE;50395000
          END ELSE                                                      50397000
          BEGIN                                                         50399000
               BACKUP;                                                  50401000
               SUBSLIST~TRUE;                                           50403000
          END;                                                          50405000
     END ELSE SUBSLIST~FALSE;                                           50407000
END SUBSLIST;                                                           50409000
BOOLEAN PROCEDURE SUBSVAR(T,T1); REAL T,T1;                             50411000
BEGIN                                                                   50413000
     IF ARRAYIDEN THEN                                                  50415000
     BEGIN                                                              50417000
          SAVEI;                                                        50419000
          IF T~IDENTIFIERS[USER,SYNTAX[I].KEYFIELD|2+1]=UNDEFINED THEN  50421000
               SYNTAXERR(0); COMMENT ARRAY NOT DECLARED;                50423000
          T1~-1;                                                        50425000
          STACK[(OSINDEX~OSINDEX+1)-1]~"99";                            50427000
               COMMENT MARK OPERATOR STACK;                             50429000
          NEXT;                                                         50431000
          IF SUBSLIST(T,T1) THEN                                        50433000
          BEGIN                                                         50435000
               NEXT;                                                    50437000
               IF NOT ERRFLAG THEN IF T.[1:2]!T1 THEN SYNTAXERR(1);     50439000
                    COMMENT NR. OF DIMENSIONS DOES NOT AGREE WITH DECL; 50441000
               IF QRIGHTBRACKET THEN                                    50443000
               BEGIN                                                    50445000
                    GENCODE(ARRAYFETCH,SYNTAX[RESTORE]);                50447000
                    OSINDEX~OSINDEX-1;                                  50449000
                    SUBSVAR~TRUE;                                       50451000
               END ELSE SUBSVAR~FALSE;                                  50453000
          END ELSE SUBSVAR~FALSE;                                       50455000
     END ELSE SUBSVAR~FALSE;                                            50457000
END SUBSVAR;                                                            50459000
     IF IDEN THEN                                                       50461000
     BEGIN                                                              50463000
          GENCODE(IDFETCH,SYNTAX[I]);                                   50465000
          VARIABLE~TRUE;                                                50467000
     END ELSE IF STRING THEN                                            50469000
     BEGIN                                                              50471000
          GENCODE(STRINGFETCH, SYNTAX[I]);                              50473000
          VARIABLE~TRUE;                                                50475000
     END ELSE IF SUBSVAR(T,T1) THEN VARIABLE~TRUE ELSE VARIABLE~FALSE;  50477000
END VARIABLE;                                                           50479000
%                                                                       50481000
%***********************************************************************50483000
%                                                                       50485000
COMMENT                                                                 50487000
          <EXPRESSION>   ::=  <UNARYOP> <EXPR BODY>                     50489000
                              <EXPR BODY>                               50491000
          <EXPR BODY>    ::=  <PRIMARY>                                 50493000
                              <PRIMARY> <ARITHOP> <EXPR BODY>           50495000
          <PRIMARY>      ::=  <UNSIGNNUM>                               50497000
                              ( <EXPRESSION> )                          50499000
                              <FNCDESIG>                                50501000
                              <VARIABLE>                                50503000
          <FNCDESIG>     ::=  <FNCIDEN> ( <EXPRESSION> )                50505000
          ;                                                             50507000
BOOLEAN PROCEDURE EXPRESSION;                                           50509000
BEGIN BOOLEAN OPERATORFLAG,PRIMARYFLAG; LABEL LOOP;                     50511000
BOOLEAN PROCEDURE FNCDESIG;                                             50513000
BEGIN                                                                   50515000
     IF FNCIDEN THEN                                                    50517000
     BEGIN                                                              50519000
          SAVEI;                                                        50521000
          NEXT;                                                         50523000
          COMMENT SKIP (;                                               50525000
                                                                        50527000
               STACK[(OSINDEX~OSINDEX+1)-1]~"99"; COMMENT MARK OPERATOR 50529000
                    STACK;                                              50531000
               NEXT;                                                    50533000
               IF EXPRESSION THEN                                       50535000
               BEGIN                                                    50537000
                    NEXT;                                               50539000
                    IF QRIGHTPAREN THEN                                 50541000
                    BEGIN                                               50543000
                         GENCODE(FUNCTCALL,SYNTAX[RESTORE]);            50545000
                         OSINDEX~OSINDEX-1;                             50547000
                         FNCDESIG~TRUE;                                 50549000
                    END ELSE                                            50551000
                    BEGIN                                               50553000
                         SYNTAXERR(9);                                  50555000
                         COMMENT MISSING ) IN FUNCTION CALL;            50557000
                         FNCDESIG~FALSE;                                50559000
                    END;                                                50561000
               END ELSE FNCDESIG~FALSE;                                 50563000
     END ELSE FNCDESIG~FALSE;                                           50577000
END FNCDESIG;                                                           50579000
BOOLEAN PROCEDURE PRIMARY;                                              50581000
BEGIN                                                                   50583000
     IF UNSIGNNUM THEN                                                  50585000
     BEGIN                                                              50587000
          PRIMARY~TRUE;                                                 50589000
          GENCODE(CONSTANT,SYNTAX[I]);                                  50591000
     END ELSE                                                           50593000
     IF QLEFTPAREN THEN                                                 50595000
     BEGIN                                                              50597000
          NEXT;                                                         50599000
          STACK[(OSINDEX~OSINDEX+1)-1]~"99";COMMENT MARK OPERATOR STACK;50601000
          IF EXPRESSION THEN                                            50603000
          BEGIN                                                         50605000
               NEXT;                                                    50607000
               IF QRIGHTPAREN THEN                                      50609000
               BEGIN                                                    50611000
                    PRIMARY~TRUE;                                       50613000
                    WHILE STACK[OSINDEX-1]!"99" DO GENCODE(ARITHOPR,    50615000
                         STACK[OSINDEX~OSINDEX-1]);                     50617000
                    OSINDEX~OSINDEX-1;COMMENT ELIMINATE MARK STACK;     50619000
               END ELSE                                                 50621000
               BEGIN                                                    50623000
                    SYNTAXERR(8);                                       50625000
                    COMMENT MISSING );                                  50627000
                    PRIMARY~FALSE;                                      50629000
               END;                                                     50631000
          END ELSE PRIMARY~FALSE;                                       50633000
     END ELSE                                                           50635000
     IF FNCDESIG OR VARIABLE THEN PRIMARY~TRUE ELSE PRIMARY~FALSE;      50637000
END PRIMARY;                                                            50639000
     IF QMINUS THEN STACK[(OSINDEX~OSINDEX+1)-1]~"#5"; COMMENT PLACE    50641000
          UNARY MINUS SIGN IN OPERATOR STACK;                           50643000
     IF NOT(QPLUS OR QMINUS) THEN                                       50645000
     IF ARITHOP THEN                                                    50647000
     BEGIN                                                              50649000
          SYNTAXERR(6);                                                 50651000
          COMMENT DANGLING OPERATOR;                                    50653000
          EXPRESSION~FALSE;                                             50655000
     END ELSE BACKUP;                                                   50657000
     OPERATORFLAG~TRUE;                                                 50659000
LOOP:NEXT;                                                              50661000
     IF ARITHOP THEN                                                    50663000
     BEGIN                                                              50665000
          OPERATORFLAG~PRIMARYFLAG~TRUE;                                50667000
          WHILE OSINDEX!OPSTACK AND STACK[OSINDEX-1]!"99" AND HIERARCHY(50669000
               SYNTAX[I]){HIERARCHY(STACK[OSINDEX-1]) DO GENCODE(       50671000
               ARITHOPR,STACK[ OSINDEX~OSINDEX-1]);                     50673000
          STACK[(OSINDEX~OSINDEX+1)-1]~SYNTAX[I];                       50675000
          COMMENT PLACE THE CURRENT OPERATOR ON THE STACK AFTER FIRST   50677000
               REMOVING ALL OPERATORS WITH EQUAL OR GREATER HIERARCHY;  50679000
          GO TO LOOP;                                                   50681000
     END HANDLING AN OPERATOR;                                          50683000
     IF OPERATORFLAG THEN IF PRIMARY THEN                               50693000
     BEGIN                                                              50695000
          PRIMARYFLAG~TRUE;                                             50697000
          OPERATORFLAG~FALSE;                                           50699000
          GO TO LOOP;                                                   50701000
     END HANDLING A VALID PRIMARY;                                      50703000
     WHILE OSINDEX!OPSTACK AND STACK[OSINDEX-1]!"99" DO GENCODE(ARITHOPR50705000
          ,STACK[OSINDEX~OSINDEX-1]);                                   50707000
     IF NOT PRIMARYFLAG THEN EXPRESSION~FALSE ELSE                      50709000
     IF OPERATORFLAG THEN                                               50711000
     BEGIN                                                              50713000
          SYNTAXERR(6);                                                 50715000
          COMMENT DANGLING OPERATOR;                                    50717000
          EXPRESSION~FALSE;                                             50719000
     END ELSE                                                           50721000
     BEGIN                                                              50723000
          EXPRESSION~TRUE;                                              50725000
          BACKUP;                                                       50727000
     END;                                                               50729000
END EXPRESSION;                                                         50731000
%                                                                       50733000
%***********************************************************************50735000
%                                                                       50737000
COMMENT                                                                 50739000
          <GOSTMT>       ::=  GO <STMTNR>                               50741000
                              GO TO <STMTNR>                            50743000
                              GO TO STEP <STMTNR>                       50745000
          ;                                                             50747000
BOOLEAN PROCEDURE GOSTMT;                                               50749000
BEGIN                                                                   50751000
     INHIBITCODE~FALSE;                                                 50753000
     IF QGO OR (IDEN AND IDENTIFIERS[USER,(T1~SYNTAX[I].[42:6])+T1].    50755000
          [1:29] = "4GOTO") THEN                                        5075510 
     BEGIN                                                              50757000
          NEXT;                                                         50759000
          IF QTO THEN NEXT;                                             50761000
          IF QSTEP THEN NEXT;                                           50763000
          IF STMTNR THEN                                                50765000
          BEGIN                                                         50767000
               GENCODE(GOTO,SYNTAX[I]);                                 50769000
               GENCODE(1,1);                                            50771000
               GENCODE(NEXTLINE,0);                                     50773000
               GOSTMT~TRUE;                                             50775000
          END ELSE                                                      50777000
          BEGIN                                                         50779000
               SYNTAXERR(11); COMMENT MISSING STATEMENT NUMBER;         50781000
               GOSTMT~FALSE;                                            50783000
          END;                                                          50785000
     END ELSE GOSTMT~FALSE;                                             50787000
END GOSTMT;                                                             50789000
%                                                                       50791000
%***********************************************************************50793000
%                                                                       50795000
BOOLEAN PROCEDURE REPLSTMT;                                             50797000
BEGIN                                                                   50799000
BOOLEAN PROCEDURE LEFTPARTLIST;                                         50801000
BEGIN                                                                   50803000
     PUSHDOWNI;                                                         50805000
     IF VARIABLE THEN                                                   50807000
     BEGIN                                                              50809000
          NEXT;                                                         50811000
          IF QCOMMA THEN                                                50813000
          BEGIN                                                         50815000
               NEXT;                                                    50817000
               IF LEFTPARTLIST THEN LEFTPARTLIST~TRUE                   50819000
                    ELSE LEFTPARTLIST~FALSE;                            50821000
          END ELSE                                                      50823000
          BEGIN                                                         50825000
               BACKUP;                                                  50827000
               LEFTPARTLIST~TRUE;                                       50829000
          END;                                                          50831000
     END ELSE                                                           50833000
     BEGIN                                                              50835000
          SYNTAXERR(26);  COMMENT  INVALID REPLACEMENT STMT;            50837000
          LEFTPARTLIST~FALSE;                                           50839000
     END;                                                               50841000
END LEFTPARTLIST;                                                       50843000
IF I { SCANCODE+1 THEN                                                  50845000
IF IDEN THEN                                                            50847000
BEGIN                                                                   50849000
     NEXT;                                                              50851000
     IF NOT (QEQUAL OR QCOMMA) THEN IF NOT ERRFLAG THEN                 50853000
     BEGIN                                                              50855000
          ERRNUMBER~3;                                                  50857000
          ERRFLAG~TRUE;                                                 50859000
     END ELSE ELSE BACKUP;                                              50861000
END CHECKING FOR POSSIBLE MISSPELLING OF A KEYWORD;                     50863000
IF QSET THEN NEXT;                                                      50865000
INHIBITCODE~TRUE;                                                       50867000
IF LEFTPARTLIST THEN                                                    50869000
BEGIN                                                                   50871000
     NEXT;                                                              50873000
     IF QEQUAL THEN                                                     50875000
     BEGIN                                                              50877000
          INHIBITCODE~FALSE;                                            50879000
          NEXT;                                                         50881000
          IF EXPRESSION THEN                                            50883000
          BEGIN                                                         50885000
               SAVEI;                                                   50887000
               REPLSTMT~TRUE;                                           50889000
               WHILE PDSINDEX!PDS DO BEGIN                              50891000
                    I~POPUPI;                                           50893000
                    OK~VARIABLE;                                        50895000
                    SYNTAX[TEMPCODEINDEX-1].[36:6]~                     50897000
                         (IF SYNTAX[TEMPCODEINDEX-1].[36:6]=IDFETCH     50899000
                         THEN NDSTORE ELSE NDSTOREA);                   50901000
                    COMMENT  GENERATE THE ID AND ARRAY STORES IN        50903000
                         REVERSE ORDER BY GENERATING FETCHES AND        50905000
                         CHANGING TO STORES;                            50907000
               END;                                                     50909000
               SYNTAX[TEMPCODEINDEX-1].[36:6]~SYNTAX[TEMPCODEINDEX-1].  50911000
                    [36:6]+1;                                           50913000
               GENCODE(NEXTLINE,0);                                     50915000
               I~RESTORE;                                               50917000
          END ELSE                                                      50919000
          BEGIN                                                         50921000
               REPLSTMT~FALSE;                                          50923000
               SYNTAXERR(26);                                           50925000
          END;                                                          50927000
     END ELSE                                                           50929000
     BEGIN                                                              50931000
          REPLSTMT~FALSE;                                               50933000
          ERRFLAG ~ TRUE; ERRNUMBER ~ 3; COMMENT UNRECOGNIZABLE STMT;   50935000
     END;                                                               50937000
END ELSE REPLSTMT~FALSE;                                                50939000
END REPLSTMT;                                                           50941000
%                                                                       50943000
%***********************************************************************50945000
%                                                                       50947000
COMMENT                                                                 50949000
          <DOSTMT>       ::=  <DOPART>                                  50951000
                              <DOPART> <TIMESCLAUSE>                    50953000
                              <DOPART> <FORCLAUSE>                      50955000
                              <DOPART> <WHILECLAUSE>                    50957000
                              <DOPART> <UNTILCLAUSE>                    50959000
          <DOPART>       ::=  DO PART <STMTNR>                          50961000
                              DO PART <STMTNR> ,                        50963000
          <TIMESCLAUSE>  ::=  <EXPRESSION> TIMES                        50965000
          <FORCLAUSE>    ::=  FOR <FORBODY>                             50967000
                              <FORBODY>                                 50969000
          <FORBODY>      ::=  <REPLSTMT> <TO> <EXPRESSION>              50971000
                              <REPLSTMT> <TO> <EXPRESSION> <BY> <EXPR.> 50973000
          <TO>           ::=  ,                                         50975000
                              TO                                        50977000
                              UNTIL                                     50979000
          <BY>           ::=  ,                                         50981000
                              BY                                        50983000
                              STEP                                      50985000
          <WHILECLAUSE>  ::=  WHILE <RELATION>                          50987000
          <RELATION>     ::=  <EXPRESSION> <RELOP> <EXPRESSION>         50989000
          <UNTILCLAUSE>  ::=  UNTIL <RELATION>                          50991000
          ;                                                             50993000
BOOLEAN PROCEDURE DOSTMT;                                               50995000
BEGIN                                                                   50997000
BOOLEAN PROCEDURE DOPRT;                                                50999000
BEGIN                                                                   51001000
     IF QDO THEN                                                        51003000
     BEGIN                                                              51005000
          NEXT;                                                         51007000
          IF QPART THEN                                                 51009000
          BEGIN                                                         51011000
               NEXT;                                                    51013000
               IF STMTNR THEN                                           51015000
               BEGIN                                                    51017000
                    SAVEI;                                              51019000
                    NEXT;                                               51021000
                    IF NOT QCOMMA THEN BACKUP;                          51023000
                    DOPRT~TRUE;                                         51025000
               END ELSE                                                 51027000
               BEGIN                                                    51029000
                    SYNTAXERR(14); COMMENT MISSING PART NUMBER;         51031000
                    DOPRT~FALSE;                                        51033000
               END;                                                     51035000
          END ELSE                                                      51037000
          BEGIN                                                         51039000
               SYNTAXERR(15); COMMENT "PART" MISSING;                   51041000
               DOPRT~FALSE;                                             51043000
          END;                                                          51045000
     END ELSE DOPRT~FALSE;                                              51047000
END DOPRT;                                                              51049000
BOOLEAN PROCEDURE RELATION;                                             51051000
BEGIN                                                                   51053000
     INHIBITCODE~FALSE;                                                 51055000
     IF EXPRESSION THEN                                                 51057000
     BEGIN                                                              51059000
          NEXT;                                                         51061000
          IF RELOP THEN                                                 51063000
          BEGIN                                                         51065000
               SAVEI;                                                   51067000
               NEXT;                                                    51069000
               IF EXPRESSION THEN                                       51071000
               BEGIN                                                    51073000
                    RELATION~TRUE;                                      51075000
                    GENCODE(RELTEST,SYNTAX[RESTORE]);                   51077000
               END ELSE RELATION~FALSE;                                 51079000
        END ELSE                                                        51081000
          BEGIN                                                         51083000
               SYNTAXERR(16); COMMENT RELATIONAL OPERATOR IS MISSING;   51085000
               RELATION~FALSE;                                          51087000
          END;                                                          51089000
     END ELSE RELATION~FALSE;                                           51091000
END RELATION;                                                           51093000
BOOLEAN PROCEDURE TIMESCLAUSE;                                          51095000
BEGIN                                                                   51097000
     IF EXPRESSION THEN                                                 51099000
     BEGIN                                                              51101000
          NEXT;                                                         51103000
          IF QTIMES THEN TIMESCLAUSE~TRUE ELSE TIMESCLAUSE~FALSE;       51105000
     END ELSE TIMESCLAUSE~FALSE;                                        51107000
END TIMESCLAUSE;                                                        51109000
BOOLEAN PROCEDURE WHILECLAUSE;                                          51111000
BEGIN                                                                   51113000
     IF QWHILE THEN                                                     51115000
     BEGIN                                                              51117000
          NEXT;                                                         51119000
          IF RELATION THEN WHILECLAUSE~TRUE ELSE WHILECLAUSE~FALSE;     51121000
     END ELSE WHILECLAUSE~FALSE;                                        51123000
END WHILECLAUSE;                                                        51125000
BOOLEAN PROCEDURE UNTILCLAUSE;                                          51127000
BEGIN                                                                   51129000
     IF QUNTIL THEN                                                     51131000
     BEGIN                                                              51133000
          NEXT;                                                         51135000
          IF RELATION THEN UNTILCLAUSE~TRUE ELSE UNTILCLAUSE~FALSE;     51137000
     END ELSE UNTILCLAUSE~FALSE;                                        51139000
END UNTILCLAUSE;                                                        51141000
BOOLEAN PROCEDURE FORCLAUSE;                                            51143000
BEGIN                                                                   51145000
     IF QFOR THEN                                                       51147000
          NEXT;                                                         51149000
          T1~I;                                                         51151000
          IF REPLSTMT THEN                                              51153000
          BEGIN                                                         51155000
               SAV~TEMPCODEINDEX~TEMPCODEINDEX-1;                       51157000
               T~I;                                                     51159000
               I~T1;                                                    51161000
               OK~VARIABLE;                                             51163000
               I~T;                                                     51165000
               NEXT;                                                    51167000
               IF QCOMMA OR QTO OR QUNTIL THEN NEXT;                    51169000
               IF EXPRESSION THEN                                       51171000
               BEGIN                                                    51173000
                    NEXT;                                               51175000
                    IF QCOMMA OR QBY OR QSTEP THEN NEXT;                51177000
                    T~I;                                                51179000
                    IF T7~SYNTAX[I]=ENDR OR T7=ELSECODE OR EXPRESSION   51181000
                         THEN                                           51183000
                    BEGIN                                               51185000
                         IF T7~SYNTAX[T]=ENDR OR T7=ELSECODE THEN       51187000
                              GENCODE(LITERAL,1);                       51189000
                         GENCODE(LITERAL,0);                            51191000
                         GENCODE(RELTEST,4);                            51193000
                         GENCODE(IFNOTRUSKIP,4);                        51195000
                         GENCODE(RELTEST,2);                            51197000
                         GENCODE(IFNOTRUSKIP,5);                        51199000
                         GENCODE(NEXTLINE,0);                           51201000
                         GENCODE(RELTEST,3);                            51203000
                         GENCODE(IFNOTRUSKIP,2);                        51205000
                         GENCODE(NEXTLINE,0);                           51207000
                         GENCODE(DOPART,SYNTAX[RESTORE]);               51209000
                         GENCODE(1,1);                                  51211000
                         I~T1;                                          51213000
                         OK~VARIABLE;                                   51215000
                         I~T;                                           51217000
                         IF T7~SYNTAX[I]=ENDR OR T7=ELSECODE THEN       51219000
                              GENCODE(LITERAL,1) ELSE OK~EXPRESSION;    51221000
                         GENCODE(ARITHOPR,0);                           51223000
                         I~T1;                                          51225000
                         OK~VARIABLE;                                   51227000
                         SYNTAX[TEMPCODEINDEX-1].[36:6]~(IF SYNTAX[     51229000
                              TEMPCODEINDEX-1].[36:6]=IDFETCH THEN      51231000
                              DSTORE ELSE DSTOREA);                     51233000
                         GENCODE(BRANCHBACK,TEMPCODEINDEX-SAV);         51235000
                         I~T;                                           51237000
                         IF T7~SYNTAX[I]=ENDR OR T7=ELSECODE THEN I~I-1 51239000
                         ELSE BEGIN                                     51241000
                              INHIBITCODE~TRUE;                         51243000
                              OK~EXPRESSION;                            51245000
                         END;                                           51247000
                         FORCLAUSE~TRUE;                                51249000
                    END ELSE                                            51251000
                    BEGIN                                               51253000
                         SYNTAXERR(17); COMMENT INVALID FOR CLAUSE;     51255000
                         FORCLAUSE~FALSE;                               51257000
                    END;                                                51259000
               END ELSE                                                 51261000
               BEGIN                                                    51263000
                    SYNTAXERR(17); COMMENT INVALID FOR CLAUSE;          51265000
                    FORCLAUSE~FALSE;                                    51267000
               END;                                                     51269000
          END ELSE FORCLAUSE~FALSE;                                     51271000
END FORCLAUSE;                                                          51273000
     IF DOPRT THEN                                                      51275000
     BEGIN                                                              51277000
          NEXT;                                                         51279000
          T2~I;                                                         51281000
          T4~TEMPCODEINDEX;                                             51283000
          INHIBITCODE~FALSE;                                            51285000
          IF T7~SYNTAX[I]=ENDR OR T7=ELSECODE THEN                      51287000
          BEGIN                                                         51289000
               GENCODE(DOPART,SYNTAX[RESTORE]);                         51291000
               GENCODE(1,1);                                            51293000
               GENCODE(NEXTLINE,0);                                     51295000
               BACKUP;                                                  51297000
               DOSTMT~TRUE;                                             51299000
          END ELSE                                                      51301000
          IF TIMESCLAUSE THEN                                           51303000
          BEGIN                                                         51305000
               GENCODE(INTRINSIC,1);                                    51307000
               GENCODE(DECRSTACKANDSKIP,0);                             51309000
               GENCODE(NEXTLINE,0);                                     51311000
               GENCODE(DOPART,SYNTAX[RESTORE]);                         51313000
               GENCODE(1,1);                                            51315000
               GENCODE(BRANCHBACK,4);                                   51317000
               DOSTMT~TRUE;                                             51319000
          END ELSE                                                      51321000
          BEGIN                                                         51323000
               I~T2;                                                    51325000
               TEMPCODEINDEX~T4;                                        51327000
               IF FORCLAUSE THEN DOSTMT~TRUE ELSE                       51329000
               BEGIN                                                    51331000
                    I~T2;                                               51333000
                    IF ERRNUMBER = SYNTAXERRSTART+26 THEN               51335000
                         BEGIN                                          51337000
                         ERRNUMBER~0;  ERRFLAG~FALSE;   COMMENT         51339000
                         FORCLAUSE CALL TO REPLSTMT CAUSES ERRFLAG;     51341000
                         END;                                           51343000
                    SAV~TEMPCODEINDEX;                                  51345000
                    INHIBITCODE~FALSE;                                  51347000
                    GENCODE(DOPART,SYNTAX[RESTORE]);                    51349000
                    GENCODE(1,1);                                       51351000
                    IF WHILECLAUSE THEN                                 51353000
                    BEGIN                                               51355000
                         GENCODE(IFNOTRUSKIP,2);                        51357000
                         GENCODE(BRANCHBACK,TEMPCODEINDEX-SAV);         51359000
                         GENCODE(NEXTLINE,0);                           51361000
                         DOSTMT~TRUE;                                   51363000
                    END ELSE                                            51365000
                    BEGIN                                               51367000
                         I~T2;                                          51369000
                         IF UNTILCLAUSE THEN                            51371000
                         BEGIN                                          51373000
                              GENCODE(IFNOTRUSKIP,2);                   51375000
                              GENCODE(NEXTLINE,0);                      51377000
                              GENCODE(BRANCHBACK,TEMPCODEINDEX-SAV);    51379000
                              DOSTMT~TRUE;                              51381000
                         END ELSE                                       51383000
                         BEGIN                                          51385000
                              SYNTAXERR(18); COMMENT ILLEGAL FORM OF THE51387000
                              DO STATEMENT HAS BEEN USED;               51389000
                              DOSTMT~FALSE;                             51391000
                         END;                                           51393000
                    END;                                                51395000
               END;                                                     51397000
          END;                                                          51399000
     END ELSE DOSTMT~FALSE;                                             51401000
END DOSTMT;                                                             51403000
%                                                                       51405000
%***********************************************************************51407000
%                                                                       51409000
COMMENT                                                                 51411000
          <INPUTSTMT>    ::=  READ <VARLIST>                            51413000
                              READ DATA <VARLIST>                       51415000
                              READ                                      51417000
          <VARLIST>      ::=  <VARIABLE>                                51419000
                              <VARIABLE> , <VARLIST>                    51421000
          ;                                                             51423000
BOOLEAN PROCEDURE INPUTSTMT(INFO); ARRAY INFO[0];                       51425000
BEGIN                                                                   51427000
BOOLEAN PROCEDURE VARLIST;                                              51429000
BEGIN                                                                   51431000
     IF VARIABLE THEN                                                   51433000
     BEGIN                                                              51435000
          SYNTAX[TEMPCODEINDEX-1].[36:6]~IF READDATA THEN(IF SYNTAX[    51437000
               TEMPCODEINDEX-1].[36:6] = IDFETCH THEN IDREADDATA ELSE   51439000
               ARRAYREADDATA) ELSE (IF SYNTAX[TEMPCODEINDEX-1].[36:6] = 51441000
               IDFETCH THEN IDACCEPT ELSE ARRAYACCEPT);                 51443000
          COMMENT CHANGE ID FETCH GENERATED BY VARIABLE TO AN ID ACCEPT;51445000
          NEXT;                                                         51447000
          IF QCOMMA THEN                                                51449000
          BEGIN                                                         51451000
               NEXT;                                                    51453000
               IF VARLIST THEN VARLIST~TRUE ELSE VARLIST~FALSE;         51455000
          END ELSE                                                      51457000
          BEGIN                                                         51459000
               BACKUP;                                                  51461000
               VARLIST~TRUE;                                            51463000
          END;                                                          51465000
     END ELSE VARLIST~FALSE;                                            51467000
END VARLIST;                                                            51469000
     INHIBITCODE~FALSE;                                                 51471000
     READDATA~FALSE;                                                    51473000
     IF QREAD THEN                                                      51475000
     BEGIN                                                              51477000
          NEXT;                                                         51479000
          IF ENDCODE OR QELSE THEN                                      51481000
          BEGIN                                                         51483000
               INPUTSTMT~TRUE;                                          51485000
               GENCODE(INTRINSIC,2);                                    51487000
               GENCODE(NEXTLINE,0);                                     51489000
          END READALPHA ELSE                                            51491000
          BEGIN                                                         51493000
               COMMENT CHECK FOR READ DATA <VARIABLE>;                  51495000
               IF IDEN AND IDENTIFIERS[USER,(T~SYNTAX[I].[42:6])+T].[   51497000
                    1:29] = "4DATA" AND (T~SYNTAX[I+1].[36:6]="5" OR    51499000
                    T="6") THEN                                         51501000
               BEGIN                                                    51503000
                    NEXT;                                               51505000
                    INFO[DATAST]~1;                                     51507000
                    READDATA~TRUE;                                      51509000
               END;                                                     51511000
               IF VARLIST THEN                                          51513000
               BEGIN                                                    51515000
                    GENCODE(NEXTLINE,0);                                51517000
                    INPUTSTMT~TRUE;                                     51519000
               END ELSE INPUTSTMT~FALSE;                                51521000
          END;                                                          51523000
     END ELSE INPUTSTMT~FALSE;                                          51525000
END INPUTSTMT;                                                          51527000
%                                                                       51529000
%***********************************************************************51531000
%                                                                       51533000
COMMENT                                                                 51535000
          <UNTILSTMT>    ::=  UNTIL <RELATION> <DOCLAUSE>               51537000
          <DOCLAUSE>     ::=  , DO PART <STMTNR>                        51539000
                              DO PART <STMTNR>                          51541000
          <WHILESTMT>    ::=  WHILE <RELATION> <DOCLAUSE>               51543000
          ;                                                             51545000
BOOLEAN PROCEDURE RELATION;                                             51547000
BEGIN                                                                   51549000
     INHIBITCODE~FALSE;                                                 51551000
     IF EXPRESSION THEN                                                 51553000
     BEGIN                                                              51555000
          NEXT;                                                         51557000
          IF RELOP THEN                                                 51559000
          BEGIN                                                         51561000
               SAVEI;                                                   51563000
               NEXT;                                                    51565000
               IF EXPRESSION THEN                                       51567000
               BEGIN                                                    51569000
                    RELATION~TRUE;                                      51571000
                    GENCODE(RELTEST,SYNTAX[RESTORE]);                   51573000
               END ELSE RELATION~FALSE;                                 51575000
          END ELSE                                                      51577000
          BEGIN                                                         51579000
               SYNTAXERR(16); COMMENT MISSING RELATIONAL OPERATOR;      51581000
               RELATION~FALSE;                                          51583000
          END;                                                          51585000
     END ELSE RELATION~FALSE;                                           51587000
END RELATION;                                                           51589000
BOOLEAN PROCEDURE DOCLAUSE;                                             51591000
BEGIN                                                                   51593000
     IF QCOMMA THEN NEXT;                                               51595000
     IF QDO THEN                                                        51597000
     BEGIN                                                              51599000
          NEXT;                                                         51601000
          IF QPART THEN                                                 51603000
          BEGIN                                                         51605000
               NEXT;                                                    51607000
               IF STMTNR THEN                                           51609000
               BEGIN                                                    51611000
                    GENCODE(DOPART,SYNTAX[I]);                          51613000
                    GENCODE(1,1);                                       51615000
                    GENCODE(BRANCHBACK,TEMPCODEINDEX-SAV);              51617000
                    DOCLAUSE~TRUE;                                      51619000
               END ELSE                                                 51621000
               BEGIN                                                    51623000
                    SYNTAXERR(14); COMMENT MISSING PART NUMBER;         51625000
                    DOCLAUSE~FALSE;                                     51627000
               END;                                                     51629000
          END ELSE                                                      51631000
          BEGIN                                                         51633000
               SYNTAXERR(15); COMMENT "PART" MISSING;                   51635000
               DOCLAUSE~FALSE;                                          51637000
          END;                                                          51639000
     END ELSE                                                           51641000
     BEGIN                                                              51643000
          SYNTAXERR(19); COMMENT "DO" MISSING;                          51645000
          DOCLAUSE~FALSE;                                               51647000
     END;                                                               51649000
END DOCLAUSE;                                                           51651000
BOOLEAN PROCEDURE UNTILSTMT;                                            51653000
BEGIN                                                                   51655000
     SAV~TEMPCODEINDEX;                                                 51657000
     IF QUNTIL THEN                                                     51659000
     BEGIN                                                              51661000
          NEXT;                                                         51663000
          IF RELATION THEN                                              51665000
          BEGIN                                                         51667000
               NEXT;                                                    51669000
               GENCODE(IFNOTRUSKIP,2);                                  51671000
               GENCODE(NEXTLINE,0);                                     51673000
               IF DOCLAUSE THEN UNTILSTMT~TRUE ELSE UNTILSTMT~FALSE;    51675000
          END ELSE UNTILSTMT~FALSE;                                     51677000
     END ELSE UNTILSTMT~FALSE;                                          51679000
END UNTILSTMT;                                                          51681000
BOOLEAN PROCEDURE WHILESTMT;                                            51683000
BEGIN                                                                   51685000
     SAV~TEMPCODEINDEX;                                                 51687000
     IF QWHILE THEN                                                     51689000
     BEGIN                                                              51691000
          NEXT;                                                         51693000
          IF RELATION THEN                                              51695000
          BEGIN                                                         51697000
               NEXT;                                                    51699000
               GENCODE(IFNOTRUSKIP,4);                                  51701000
               IF DOCLAUSE THEN                                         51703000
               BEGIN                                                    51705000
                    GENCODE(NEXTLINE,0);                                51707000
                    WHILESTMT~TRUE;                                     51709000
               END ELSE WHILESTMT~FALSE;                                51711000
          END ELSE WHILESTMT~FALSE;                                     51713000
     END ELSE WHILESTMT~FALSE;                                          51715000
END WHILESTMT;                                                          51717000
%                                                                       51719000
%***********************************************************************51721000
%                                                                       51723000
COMMENT                                                                 51725000
          <PRINTSTMT>    ::=  PRINT FORM <STMTNR> <EXPRLIST>            51727000
                              PRINT FORM <STMTNR> , <EXPRLIST>          51727100
                              PRINT <PRINTLIST>                         51729000
          <EXPRLIST>     ::=  <EXPRESSION>                              51731000
                              <EXPRESSION> , <EXPRLIST>                 51733000
          <PRINTLIST>    ::=  <PRINTELEM>                               51735000
                              <PRINTELEM> , <PRINTLIST>                 51737000
          <PRINTELEM>    ::=  <EXPRESSION>                              51739000
                              <STRING>                                  51741000
                              PLOT ( <EXPRESSION> , <STRING> )          51741100
                              PLOT ( <STRING> , <EXPRESSION> )          51741200
                              TAB ( <EXPRESSION> )                      51741300
                              REPEAT ( <EXPRESSION> , <STRING> )        51741400
                              REPEAT ( <STRING> , <EXPRESSION> )        51741500
          ;                                                             51743000
BOOLEAN PROCEDURE PRINTSTMT;                                            51745000
BEGIN                                                                   51747000
BOOLEAN PROCEDURE PRINTELEM;                                            51749000
BEGIN                                                                   51751000
     IF STRING THEN                                                     51753000
     BEGIN                                                              51755000
          GENCODE(PRINTSTRING,SYNTAX[I]);                               51757000
          PRINTELEM~TRUE;                                               51759000
     END ELSE IF QTAB AND SYNTAX[I+1]="40" THEN                         51759020
     BEGIN                                                              51759040
          NEXT; NEXT;                                                   51759060
          IF EXPRESSION THEN                                            51759080
          BEGIN                                                         51759100
               NEXT;                                                    51759120
               IF QRIGHTPAREN THEN                                      51759140
               BEGIN                                                    51759160
                    GENCODE(FUNCTCALL,19);                              51759180
                    PRINTELEM ~ TRUE;                                   51759200
               END ELSE PRINTELEM ~ FALSE;                              51759220
          END ELSE PRINTELEM ~ FALSE;                                   51759240
     END ELSE IF QPLOT OR QREPEAT AND SYNTAX[I+1] = "40" THEN           51759260
     BEGIN                                                              51759280
          T4~IF QPLOT THEN 18 ELSE 20;                                  51759300
          NEXT; NEXT;                                                   51759320
          IF STRING THEN                                                51759340
          BEGIN                                                         51759360
               GENCODE(STRINGFETCH,SYNTAX[I]);                          51759380
               NEXT;                                                    51759400
               IF QCOMMA THEN                                           51759420
               BEGIN                                                    51759440
                    NEXT;                                               51759460
                    IF EXPRESSION THEN                                  51759480
                    BEGIN                                               51759500
                         NEXT;                                          51759520
                         IF QRIGHTPAREN THEN                            51759540
                         BEGIN                                          51759560
                              GENCODE(FUNCTCALL,T4);                    51759580
                              PRINTELEM ~ TRUE;                         51759600
                         END ELSE PRINTELEM~FALSE;                      51759620
                    END ELSE PRINTELEM ~ FALSE;                         51759640
               END ELSE PRINTELEM~FALSE;                                51759660
          END ELSE                                                      51759680
          BEGIN                                                         51759700
               INHIBITCODE ~ TRUE;                                      51759720
               T1 ~ I;                                                  51759740
               IF EXPRESSION THEN                                       51759760
               BEGIN                                                    51759780
                    NEXT;                                               51759800
                    IF QCOMMA THEN                                      51759820
                    BEGIN                                               51759840
                         NEXT;                                          51759860
                         IF STRING THEN                                 51759880
                         BEGIN                                          51759900
                              INHIBITCODE ~ FALSE;                      51759920
                              GENCODE(STRINGFETCH,SYNTAX[I]);           51759940
                              T~I;                                      51759960
                              I ~ T1;                                   51759980
                              OK ~ EXPRESSION;                          51760000
                              I ~ T;                                    51760020
                              NEXT;                                     51760040
                              IF QRIGHTPAREN THEN                       51760060
                              BEGIN                                     51760080
                                    GENCODE(FUNCTCALL,T4);              51760100
                                   PRINTELEM ~ TRUE;                    51760120
                              END ELSE PRINTELEM ~ FALSE;               51760140
                         END ELSE PRINTELEM ~ FALSE;                    51760160
                    END ELSE PRINTELEM ~ FALSE;                         51760180
               END ELSE PRINTELEM ~ FALSE;                              51760200
          END;                                                          51760220
     END ELSE IF EXPRESSION THEN                                        51761000
     BEGIN                                                              51763000
          GENCODE(PRINTEXP,0);                                          51765000
          PRINTELEM~TRUE;                                               51767000
     END ELSE                                                           51769000
     BEGIN                                                              51771000
          SYNTAXERR(20); COMMENT INVALID PRINT STATEMENT;               51773000
          PRINTELEM~FALSE;                                              51775000
     END;                                                               51777000
END PRINTELEM;                                                          51779000
BOOLEAN PROCEDURE EXPRLIST;                                             51781000
BEGIN                                                                   51783000
     IF EXPRESSION THEN                                                 51785000
     BEGIN                                                              51787000
          GENCODE(PRINTEXP,0);                                          51789000
          NEXT;                                                         51791000
          IF QCOMMA THEN                                                51793000
          BEGIN                                                         51795000
               NEXT;                                                    51797000
               IF EXPRLIST THEN EXPRLIST ~ TRUE                         51799000
                    ELSE EXPRLIST ~ FALSE;                              51801000
          END ELSE                                                      51803000
          BEGIN                                                         51805000
               BACKUP;                                                  51807000
               EXPRLIST ~ TRUE;                                         51809000
          END;                                                          51811000
     END ELSE                                                           51813000
     BEGIN                                                              51815000
          SYNTAXERR(20); COMMENT INVALID PRINT STMT;                    51817000
          EXPRLIST ~ FALSE;                                             51819000
     END;                                                               51821000
END EXPRLIST;                                                           51823000
BOOLEAN PROCEDURE PRINTLIST;                                            51825000
BEGIN                                                                   51827000
     IF PRINTELEM THEN                                                  51829000
     BEGIN                                                              51831000
          NEXT;                                                         51833000
          IF QCOMMA THEN                                                51835000
          BEGIN                                                         51837000
               NEXT;                                                    51839000
               IF PRINTLIST THEN PRINTLIST~TRUE ELSE PRINTLIST~FALSE;   51841000
          END ELSE                                                      51843000
          BEGIN                                                         51845000
               BACKUP;                                                  51847000
               PRINTLIST~TRUE;                                          51849000
          END;                                                          51851000
     END ELSE PRINTLIST~FALSE;                                          51853000
END PRINTLIST;                                                          51855000
     INHIBITCODE~FALSE;                                                 51857000
     IF QPRINT THEN                                                     51859000
     BEGIN                                                              51861000
          NEXT;                                                         51863000
          IF QFORM THEN IF NOT PROGMODE THEN                            51865000
          BEGIN                                                         51867000
               SYNTAXERR(25); COMMENT FORM CANNOT BE USED IN CALC. MODE;51869000
               PRINTSTMT~FALSE;                                         51871000
          END ELSE BEGIN                                                51873000
               NEXT;                                                    51875000
               GENCODE(FORM, SYNTAX[I]);  GENCODE(1,1);                 51877000
               IF STMTNR THEN                                           51879000
               BEGIN                                                    51881000
                    NEXT;                                               51883000
                    IF QCOMMA THEN NEXT;                                51883100
                    IF ENDCODE OR QELSE THEN                            51883200
                    BEGIN                                               51883300
                         GENCODE(INTRINSIC,0);                          51883400
                         GENCODE(NEXTLINE,0);                           51883500
                         PRINTSTMT~TRUE;                                51883600
                         BACKUP;                                        51883700
                    END NULL LIST ELSE                                  51883800
                    IF EXPRLIST THEN                                    51885000
                    BEGIN                                               51887000
                         GENCODE(INTRINSIC,0);                          51889000
                         GENCODE(NEXTLINE,0);                           51891000
                         PRINTSTMT~TRUE;                                51893000
                    END ELSE PRINTSTMT~FALSE;                           51895000
               END ELSE                                                 51897000
               BEGIN                                                    51899000
                    SYNTAXERR(24);  COMMENT  MISSING OR INVALID FORM #; 51901000
                    PRINTSTMT~FALSE;                                    51903000
               END;                                                     51905000
          END ELSE                                                      51907000
          IF ENDCODE OR QELSE THEN                                      51909000
          BEGIN                                                         51911000
               GENCODE(INTRINSIC,0);                                    51913000
               GENCODE(NEXTLINE,0);                                     51915000
               PRINTSTMT~TRUE;                                          51917000
          END NULL LIST ELSE                                            51919000
          IF STRING AND PROGMODE AND SYNTAX[I+1]=ENDR AND I<4 THEN      51921000
          BEGIN                                                         51923000
          COMMENT  TREAT TYPE "XX"  AS SPECIAL CASE TO REDUCE CODE SIZE;51925000
               GENCODE(INTRINSIC,3);                                    51927000
               GENCODE(NEXTLINE,0);                                     51929000
               PRINTSTMT~TRUE;                                          51931000
          END ELSE                                                      51933000
          IF PRINTLIST THEN                                             51935000
          BEGIN                                                         51937000
               GENCODE(INTRINSIC,0);                                    51939000
               GENCODE(NEXTLINE,0);                                     51941000
               PRINTSTMT~TRUE;                                          51943000
          END ELSE PRINTSTMT~FALSE;                                     51945000
     END ELSE PRINTSTMT~FALSE;                                          51947000
END PRINTSTMT;                                                          51949000
%                                                                       51951000
%***********************************************************************51953000
%                                                                       51955000
COMMENT                                                                 51957000
          <DATASTMT>   ::=  DATA <DATALIST>                             51959000
          <DATALIST>   ::=  <DATAITEM>                                  51961000
                            <DATAITEM> , <DATALIST>                     51963000
          <DATAITEM>   ::=  <UNSIGNNUM>                                 51965000
                            + <UNSIGNNUM>                               51967000
                            - <UNSIGNNUM>                               51969000
                            <STRING>                                    51971000
;                                                                       51973000
BOOLEAN PROCEDURE DATASTMT(INFO); ARRAY INFO[0];                        51975000
BEGIN                                                                   51977000
BOOLEAN PROCEDURE DATAITEM;                                             51979000
BEGIN                                                                   51981000
     IF QPLUS OR QMINUS THEN NEXT;                                      51983000
     IF UNSIGNNUM OR STRING THEN DATAITEM~TRUE ELSE                     51985000
     BEGIN                                                              51987000
          SYNTAXERR(28); COMMENT INVALID DATA ITEM;                     51989000
          DATAITEM~FALSE;                                               51991000
     END;                                                               51993000
END DATAITEM;                                                           51995000
BOOLEAN PROCEDURE DATALIST;                                             51997000
BEGIN                                                                   51999000
     IF DATAITEM THEN                                                   52001000
     BEGIN                                                              52003000
          NEXT;                                                         52005000
          IF QCOMMA THEN                                                52007000
          BEGIN                                                         52009000
               NEXT;                                                    52011000
               IF SYNTAX[I] = ENDR THEN DATALIST ~ TRUE ELSE            52011100
               IF DATALIST THEN DATALIST~TRUE ELSE DATALIST~FALSE;      52013000
          END ELSE IF NOT ENDCODE THEN                                  52015000
          BEGIN                                                         52017000
               SYNTAXERR(29); COMMENT INVALID DATA LIST;                52019000
               DATALIST~FALSE;                                          52021000
          END ELSE                                                      52023000
          BEGIN                                                         52025000
               BACKUP;                                                  52027000
               DATALIST~TRUE;                                           52029000
          END;                                                          52031000
     END ELSE DATALIST~FALSE;                                           52033000
END DATALIST;                                                           52035000
     INHIBITCODE~FALSE;                                                 52037000
     NEXT;                                                              52039000
     IF DATALIST THEN                                                   52041000
     BEGIN                                                              52043000
          GENCODE(INTRINSIC,4); COMMENT THIS WILL BE USED IN LINKUP TO  52045000
               LINK TOGETHER DATA STATEMENTS;                           52047000
          GENCODE(NEXTLINE,0);                                          52049000
          DATASTMT~TRUE;                                                52051000
          INFO[DATAST]~1;                                               52053000
     END ELSE DATASTMT~FALSE;                                           52055000
END DATASTMT;                                                           52057000
%                                                                       52059000
%***********************************************************************52061000
%                                                                       52063000
COMMENT                                                                 52065000
          <RESTORESTMT>::=  RESTORE                                     52067000
     ;                                                                  52069000
BOOLEAN PROCEDURE RESTORESTMT;                                          52071000
BEGIN                                                                   52073000
     IF IDEN AND IDENTIFIERS[USER,(T1~SYNTAX[I].[42:6])+T1].[1:35] =    52075000
               "7RESTO" AND (T1~SYNTAX[I+1] = 0 % END OF CARD           52077000
          OR T1 = ">2")   % ELSE                                        52079000
          THEN                                                          52081000
          BEGIN                                                         52083000
               GENCODE(INTRINSIC,5);                                    52085000
               GENCODE(NEXTLINE,0);                                     52087000
               RESTORESTMT~TRUE;                                        52089000
          END ELSE RESTORESTMT~FALSE;                                   52091000
END RESTORESTMT;                                                        52093000
%                                                                       52095000
%***********************************************************************52097000
%                                                                       52099000
COMMENT                                                                 52101000
          <CONDSTMT>     ::=  IF ( <EXPRESSION> ) <STMTNRLIST>          52103000
                              <IFSTMT>                                  52105000
                              <IFSTMNT> ELSE <NONIFSTMT>                52107000
          <STMTNRLIST>   ::=  <STMTNR>                                  52109000
                              <STMTNR> , <STMTNR>                       52111000
                              <STMTNR> , <STMTNR> , <STMTNR>            52113000
          <IFSTMT>       ::=  <IFCLAUSE> <NONIFSTMT>                    52115000
          <IFCLAUSE>     ::=  IF <RELATION> THEN                        52117000
                              IF <RELATION> ,                           52117100
                              IF <RELATION>                             52117200
          <NONIFSTMT>    ::=  <GOSTMT>                                  52119000
                              <PRINTSTMT>                               52121000
                              <DOSTMT>                                  52123000
                              <INPUTSTMT>                               52125000
                              <UNTILSTMT>                               52127000
                              <WHILESTMT>                               52129000
                              STOP                                      52131000
                              <REPLSTMT>                                52133000
          ;                                                             52135000
BOOLEAN PROCEDURE CONDSTMT(INFO); ARRAY INFO[0];                        52137000
BEGIN OWN INTEGER SAV;                                                  52139000
LABEL XIT;                                                              52141000
BOOLEAN PROCEDURE NONIFSTMT(INFO); ARRAY INFO[0];                       52143000
BEGIN                                                                   52145000
     IF GOSTMT OR PRINTSTMT OR DOSTMT OR INPUTSTMT(INFO) OR UNTILSTMT   52147000
          OR WHILESTMT OR RESTORESTMT THEN NONIFSTMT~TRUE ELSE          52149000
     IF QSTOP THEN                                                      52151000
     BEGIN                                                              52153000
          GENCODE(STOP,0);                                              52155000
          NONIFSTMT~TRUE;                                               52157000
     END ELSE                                                           52159000
     IF REPLSTMT THEN NONIFSTMT~TRUE ELSE                               52161000
     BEGIN                                                              52163000
          ERRNUMBER ~ SYNTAXERRSTART+21; COMMENT INVALID STATEMENT TYPE 52165000
               FOLLOWING "THEN" OR "ELSE". CANNOT USE SYNTAXERR PROC.   52167000
               HERE SINCE REPLSTMT WILL HAVE ALREADY GENERATED A        52167100
               DIAGNOSTIC;                                              52167200
          NONIFSTMT~FALSE;                                              52169000
     END;                                                               52171000
END NONIFSTMT;                                                          52173000
BOOLEAN PROCEDURE IFCLAUSE;                                             52175000
BEGIN                                                                   52177000
     IF QIF THEN                                                        52179000
     BEGIN                                                              52181000
          NEXT;                                                         52183000
          IF RELATION THEN                                              52185000
          BEGIN                                                         52187000
               NEXT;                                                    52189000
               IFCLAUSE ~ TRUE;                                         52191000
               IF NOT (QCOMMA OR QTHEN) THEN BACKUP;                    52193000
          END ELSE IFCLAUSE~FALSE;                                      52201000
     END ELSE IFCLAUSE~FALSE;                                           52203000
END IFCLAUSE;                                                           52205000
BOOLEAN PROCEDURE IFSTMT(INFO); ARRAY INFO[0];                          52207000
BEGIN                                                                   52209000
     IF IFCLAUSE THEN                                                   52211000
     BEGIN                                                              52213000
          SAV~TEMPCODEINDEX;                                            52215000
          GENCODE(IFNOTRUSKIP,0);                                       52217000
               COMMENT GENERATE SKIP INSTRUCTION WITH SYLLABLE COUNT TO 52219000
                    BE FILLED IN AFTER FURTHER ANALYSIS;                52221000
          NEXT;                                                         52223000
          IF NONIFSTMT(INFO) THEN                                       52225000
          BEGIN                                                         52227000
               IFSTMT~TRUE;                                             52229000
               SYNTAX[SAV].[42:6]~TEMPCODEINDEX-SAV;                    52231000
          END ELSE IFSTMT~FALSE;                                        52233000
     END ELSE IFSTMT~FALSE;                                             52235000
END IFSTMT;                                                             52237000
     SAVEI;                                                             52239000
     IF QIF THEN BEGIN                                                  52241000
          NEXT;                                                         52243000
          IF QLEFTPAREN THEN BEGIN                                      52245000
               NEXT;                                                    52247000
               INHIBITCODE~TRUE;                                        52249000
               IF EXPRESSION THEN BEGIN                                 52251000
                    NEXT;                                               52253000
                    IF QRIGHTPAREN THEN BEGIN                           52255000
                         NEXT;                                          52257000
                         IF STMTNR THEN BEGIN                           52259000
                              COMMENT  IT IS A FORTRAN TYPE IF;         52261000
                              I~RESTORE+2;                              52263000
                              INHIBITCODE~FALSE;                        52265000
                              CONDSTMT~EXPRESSION;                      52267000
                              NEXT;  NEXT;                              52269000
                              SAV~TEMPCODEINDEX;                        52271000
                              GENCODE(FORTRANIF,0);                     52273000
                              T~0;                                      52275000
                              DO BEGIN                                  52277000
                                   IF STMTNR THEN BEGIN                 52279000
                                        GENCODE(GOTO,SYNTAX[I]);        52281000
                                        GENCODE(1,1);                   52283000
                                       T~T+1; COMMENT  COUNT BRANCHES;  52285000
                                   END ELSE IF NOT QCOMMA THEN BEGIN    52287000
                                        SYNTAXERR(23);                  52289000
                                        COMMENT  IMPROPER STMT# IN IF;  52291000
                                        CONDSTMT~FALSE;                 52293000
                                        GO TO XIT;                      52295000
                                        END INVALID STMT;               52297000
                                   NEXT;                                52299000
                                   END UNTIL ENDCODE OR T=3;            52301000
                              SYNTAX[SAV].KEYFIELD ~ ENTIER(T);         52303000
                              COMMENT  INSERT # BRANCHES IN SYLLABLE;   52305000
                              GENCODE(NEXTLINE,0);                      52307000
                              GO TO XIT;                                52309000
                              END PROCESSING FORTRAN IF                 52311000
          END  END  END  END;                                           52313000
          COMMENT  ALGOL TYPE IF STMT;                                  52315000
     I~RESTORE;                                                         52317000
     IF IFSTMT(INFO) THEN                                               52319000
     BEGIN                                                              52321000
          NEXT;                                                         52323000
          IF QELSE THEN                                                 52325000
          BEGIN                                                         52327000
               NEXT;                                                    52329000
               IF NONIFSTMT(INFO) THEN CONDSTMT~TRUE ELSE CONDSTMT~FALSE52331000
                    ;                                                   52333000
          END ELSE                                                      52335000
          BEGIN                                                         52337000
               BACKUP;                                                  52339000
               CONDSTMT~TRUE;                                           52341000
               GENCODE(NEXTLINE,0);                                     52343000
          END;                                                          52345000
     END ELSE CONDSTMT~FALSE;                                           52347000
XIT:                                                                    52349000
END CONDSTMT;                                                           52351000
%                                                                       52353000
%***********************************************************************52355000
%                                                                       52357000
COMMENT                                                                 52359000
          <ARRAYDECL>    ::=  DECLARE <ARRAYLIST>                       52361000
          <ARAYLIST>     ::=  <ARRAYELEM>                               52363000
                              <ARRAYELEM> , <ARRAYLIST>                 52365000
          <ARRAYELEM>    ::=  <ARRAYIDEN> <BOUNDLIST> ]                 52367000
          <BOUNDLIST>    ::=  <UNSIGNNUM>                               52369000
                              <UNSIGNNUM> , <BOUNDLIST>                 52371000
          ;                                                             52373000
BOOLEAN PROCEDURE ARRAYDECL(INFO,USERARRAY); ARRAY INFO,USERARRAY[0];   52375000
BEGIN                                                                   52377000
BOOLEAN PROCEDURE BOUNDLIST;                                            52379000
BEGIN                                                                   52381000
     IF UNSIGNNUM THEN                                                  52383000
     BEGIN                                                              52385000
          IF T2~GETDIM(SYNTAX[I].KEYFIELD)<256 THEN SETDIM(T4,T2) ELSE  52387000
               SYNTAXERR(3);                                            52389000
          NEXT;                                                         52391000
          IF QCOMMA THEN                                                52393000
          BEGIN                                                         52395000
               NEXT;                                                    52397000
               IF BOUNDLIST THEN BOUNDLIST~TRUE ELSE BOUNDLIST~FALSE;   52399000
          END ELSE                                                      52401000
          BEGIN                                                         52403000
               BACKUP;                                                  52405000
               BOUNDLIST~TRUE;                                          52407000
          END;                                                          52409000
     END ELSE                                                           52411000
     BEGIN                                                              52413000
          SYNTAXERR(13); COMMENT DIMENSION MUST BE A NUMBER;            52415000
          BOUNDLIST~FALSE;                                              52417000
     END;                                                               52419000
          COMMENT DIMENSION EXCEEDS 256;                                52421000
END BOUNDLIST;                                                          52423000
BOOLEAN PROCEDURE ARRAYELEM(INFO,USERARRAY); ARRAY INFO,USERARRAY[0];   52425000
BEGIN                                                                   52427000
     IF ARRAYIDEN THEN                                                  52429000
     BEGIN                                                              52431000
          T4~0;  COMMENT THE ARRAY DESCRIPTOR IS BUILT IN T4;           52433000
          T3~1; COMMENT TOTAL ARRAY SIZE;                               52435000
          T1~0; COMMENT NUMBER OF DIMENSIONS;                           52437000
          T~SYNTAX[I].KEYFIELD;                                         52439000
          T~T+T+1; COMMENT INDEX INTO IDENTIFIERS ARRAY;                52441000
          NEXT;                                                         52443000
          IF BOUNDLIST THEN                                             52445000
          BEGIN                                                         52447000
               NEXT;                                                    52449000
               IF QRIGHTBRACKET THEN                                    52451000
               BEGIN                                                    52453000
                    IF T3=0 THEN T1~IDENTIFIERS[USER,T].[1:2]+1;        52455000
                    IF NOT ERRFLAG THEN IF T2~IDENTIFIERS[USER,T]=      52457000
                         UNDEFINED THEN                                 52459000
                    BEGIN                                               52461000
                         IF (T5~INFO[LASTUSERARRAY])+T3{MAXUSERARRAY    52463000
                         THEN                                           52465000
                         BEGIN                                          52467000
                              IDENTIFIERS[USER,T]~0&(T1-1)[1:46:2]&     52469000
                              T4[3:3:32]&T5[38:38:10];                  52471000
                              USERARRAY[T5+T3-1]~USERARRAY[T5+T3-1]; %%%52473000
                              SETTOUNDEFINED(T3 DIV 32,ENTIER(T3 MOD    52475000
                              32),USERARRAY[T5]);                       52477000
                              INFO[LASTUSERARRAY]~T5+T3;                52479000
                              ARRAYELEM~TRUE;                           52481000
                         END ELSE IF T5+T3<1023 THEN                    52483000
                         BEGIN                                          52485000
                              MAXUSERARRAY~T5+T3;                       52487000
                              REALLOCATE ~ TRUE;                        52489000
                                   ARRAYELEM~FALSE;                     52491000
                         END ELSE                                       52493000
                         BEGIN                                          52495000
                              SYNTAXERR(4);                             52497000
                              COMMENT TOTAL AREA FOR ARRAYS EXCEEDED;   52499000
                              ARRAYELEM~FALSE;                          52501000
                         END;                                           52503000
                    END PROVIOUSLY UNDEFINED ARRAY ELSE                 52505000
                    BEGIN                                               52507000
                         IF T1!T2.[1:2]+1 THEN                          52509000
                         BEGIN                                          52511000
                              SYNTAXERR(5);                             52513000
                              COMMENT ATTEMPT TO RE-DIMENSION WITH A    52515000
                                   DIFFERENT NR. OF DIMENSIONS;         52517000
                              ARRAYELEM~FALSE;                          52519000
                         END ELSE                                       52521000
                         BEGIN                                          52523000
                              INTEGER OLDLENG, T8;                      52525000
                              INTEGER DIF;                              52527000
                              DEFINE NEWSIZE=T3#,IDINDEX=T#, NEWDESC=T4#52529000
                                   ;                                    52531000
                              FORMAT F(X15, A6, X5, I1, X3, I3, X1,     52533000
                              I3, X1, I3, X1, I3, X5, I4, X5, A6);      52535000
                              NEWDESC.[1:2]~T1-1;                       52537000
                              OLDLENG~OLDSIZE(T2);                      52539000
                              IF NEWSIZE<OLDLENG THEN                   52541000
                              BEGIN                                     52543000
                                   COMMENT   SIZE DECREASE;             52545000
                                   DIF~OLDLENG-NEWSIZE;                 52547000
                                   FOR T6~IDINDEX+2 STEP 2 WHILE T5~    52549000
                                   IDENTIFIERS[USER,T6-1]!0 DO          52551000
                                   IF T5<0 THEN IF IDENTIFIERS[USER,T6] 52553000
                                   !UNDEFINED THEN                      52555000
                                                IDENTIFIERS[USER,T6].[3852557000
                                   :10]~IDENTIFIERS[USER,T6].[38:10]    52559000
                                   -DIF;                                52561000
                                   T7~IDENTIFIERS[USER,IDINDEX].[38:10];52563000
                                   T6~T7+NEWSIZE;                       52565000
                                   T5~INFO[LASTUSERARRAY];              52567000
                                   FOR T7~T7+OLDLENG STEP 1 UNTIL T5-1  52569000
                                   DO BEGIN                             52571000
                                        USERARRAY[T6]~USERARRAY[T7];    52573000
                                        T6~T6+1;                        52575000
                                   END CRUNCHING AN ARRAY;              52577000
                                   INFO[LASTUSERARRAY]~                 52579000
                                        INFO[LASTUSERARRAY]-DIF;        52581000
                                   IDENTIFIERS[USER,IDINDEX].[1:37]~    52583000
                                        NEWDESC.[1:37];                 52585000
                                   ARRAYELEM~TRUE;                      52587000
                              END DECREASE ELSE                         52589000
                              IF NEWSIZE>OLDLENG THEN                   52591000
                              BEGIN                                     52593000
                                   DIF~NEWSIZE-OLDLENG;                 52595000
                                   IF DIF+INFO[LASTUSERARRAY]>          52597000
                                        MAXUSERARRAY THEN               52599000
                                   BEGIN                                52601000
                                        IF DIF+INFO[LASTUSERARRAY] >    52603000
                                        1022 THEN                       52605000
                                        BEGIN                           52607000
                                             SYNTAXERR(4);              52609000
                                             COMMENT  TOTAL AREA EXCEED;52611000
                                             ARRAYELEM~FALSE;           52613000
                                        END ELSE                        52615000
                                        BEGIN                           52617000
                                             REALLOCATE~TRUE;           52619000
                                             MAXUSERARRAY~ DIF+         52621000
                                                  INFO[LASTUSERARRAY];  52623000
                                             ARRAYELEM~FALSE;           52625000
                                             COMMENT  BEGIN BACK OUT &  52627000
                                                       REALLOCATION;    52629000
                                        END SIZE EXTENSION;             52631000
                                   END NOT ENOUGH SPACE ELSE            52633000
                                   BEGIN                                52635000
                                        T6~127;                         52637000
                                        DO                              52639000
                                        BEGIN                           52641000
                                             IF IDENTIFIERS[USER,T6-1]<052643000
                                             THEN IF IDENTIFIERS[USER,  52645000
                                             T6]!UNDEFINED              52647000
                                             THEN IDENTIFIERS[USER,T6]. 52649000
                                             [38:10]~IDENTIFIERS[USER,  52651000
                                             T6].[38:10]+DIF;           52653000
                                        END UNTIL T6~T6-2=IDINDEX ;     52655000
                                        T7~IDENTIFIERS[USER,IDINDEX].   52657000
                                        [38:10];                        52659000
                                        T8~T6~T7+NEWSIZE;               52661000
                                        T5~INFO[LASTUSERARRAY];         52663000
                                        FOR T7~T7+OLDLENG STEP 1 UNTIL  52665000
                                        T5-1 DO                         52667000
                                        BEGIN                           52669000
                                             USERARRAY[T6]~USERARRAY[T7]52671000
                                             ;    IF T7<T8 THEN         52673000
                                             USERARRAY[T7]~UNDEFINED;   52675000
                                             T6~T6+1;                   52677000
                                        END EXPANSION ;                 52679000
                                        INFO[LASTUSERARRAY]~            52681000
                                             INFO[LASTUSERARRAY]+DIF;   52683000
                                        IDENTIFIERS[USER,IDINDEX].      52685000
                                        [1:37]~NEWDESC.[1:37];          52687000
                                        ARRAYELEM~TRUE;                 52689000
                                   END;                                 52691000
                              END INCREASE ELSE ARRAYELEM~TRUE;         52693000
                              IF DEBUG THEN                             52695000
                              BEGIN                                     52697000
                                   FOR T7~0 STEP 2 WHILE T6~            52699000
                                   IDENTIFIERS[USER,T7]!0 DO            52701000
                                   IF T6<0 THEN IF IDENTIFIERS[USER,    52703000
                                   T7+1]!UNDEFINED THEN                 52705000
                                   WRITE(PR,F,T6.[6:36],                52707000
                                   (T5~IDENTIFIERS[USER,T7+1]).[1:2],   52709000
                                   T5.[3:8], T5.[11:8], T5.[19:8],      52711000
                                   T5.[27:8], T5~T5.[38:10],            52713000
                                   USERARRAY[T5]);                      52715000
                              END DEBUG;                                52717000
                         END SIZE CHANGE BLOCK;                         52719000
                    END PREVIOUSLY DECLARED ARRAY ELSE ARRAYELEM~FALSE; 52721000
               END ELSE ARRAYELEM~FALSE;                                52723000
          END ELSE ARRAYELEM~FALSE;                                     52725000
     END ELSE                                                           52727000
     BEGIN                                                              52727100
          ARRAYELEM ~ FALSE;                                            52727200
          IF IDEN THEN                                                  52727300
          BEGIN                                                         52727400
               NEXT;                                                    52727500
               IF QLEFTPAREN THEN SYNTAXERR(10);                        52727600
               COMMENT USE [ ] TO ENCLOSE SUBSCRIPTS - [ = SHIFT K, ] = 52727700
                    SHIFT M;                                            52727800
          END;                                                          52727900
     END CHECKING IF PAREN INSTEAD OF BRACKETS;                         52728000
END ARRAYELEM;                                                          52729000
BOOLEAN PROCEDURE ARRAYLIST(INFO,USERARRAY); ARRAY INFO,USERARRAY[0];   52731000
BEGIN                                                                   52733000
    IF ARRAYELEM(INFO,USERARRAY) THEN                                   52735000
    BEGIN                                                               52737000
        NEXT;                                                           52739000
        IF QCOMMA THEN                                                  52741000
        BEGIN                                                           52743000
            NEXT;                                                       52745000
            IF ARRAYLIST(INFO,USERARRAY) THEN ARRAYLIST~TRUE ELSE       52747000
            ARRAYLIST~FALSE;                                            52749000
        END ELSE                                                        52751000
        BEGIN                                                           52753000
            BACKUP;                                                     52755000
            ARRAYLIST~TRUE;                                             52757000
        END;                                                            52759000
    END ELSE ARRAYLIST~FALSE;                                           52761000
END ARRAYLIST;                                                          52763000
    IF QDECLARE THEN                                                    52765000
    BEGIN                                                               52767000
        NEXT;                                                           52769000
        IF ARRAYLIST(INFO,USERARRAY) THEN                               52771000
        BEGIN                                                           52773000
            ARRAYDECL~TRUE;                                             52775000
            GENCODE(NEXTLINE,0);                                        52777000
        END ELSE ARRAYDECL~FALSE;                                       52779000
    END ELSE ARRAYDECL~FALSE;                                           52781000
IF DEBUG THEN WRITE (PR[DBL], 17, IDENTIFIERS[USER,*]);                 52783000
END ARRAYDECL;                                                          52785000
%  -------------------------------------------------------------------  52787000
REAL STREAM PROCEDURE BLANKFILL(A);  VALUE A;                           52789000
          BEGIN LOCAL T;                                                52791000
          DI ~ LOC T;  SI ~ LOC A;  DI ~ DI + 7;  DS ~ 1 CHR;           52793000
          SI ~ LOC A;  SKIP 2 SB;  IF SB THEN BEGIN  DI~DI-1;           52795000
          DS ~ LIT "7";  COMMENT  TRUNCATE NAME TO 7 CHARACTERS; END;   52797000
          SI ~ SI + 1;                                                  52799000
          DI ~ LOC BLANKFILL;  DS ~ 8 LIT"0       ";  DI ~ DI - 7;      52801000
          SI ~ LOC A;  SI ~ SI + 1;  DS ~ T CHR;  END BLANK FILL;       52803000
STREAM PROCEDURE MOVE9(S,D); BEGIN SI ~ S;  DI ~ D;  DS ~ 9 WDS;  END;  52805000
     STREAM PROCEDURE GETNUMB (INTG, FRACTION, TENS, BCL, ERR);         52807000
          BEGIN LOCAL C, T2;                                            52809000
          LABEL PERIOD; LOCAL T;                                        52811000
          SI ~ BCL;                                                     52813000
          DI ~ LOC C;  DI ~ DI + 7;  DS ~ 1 CHR;  T2 ~ SI;              52815000
          C(IF SC="." THEN JUMP OUT TO PERIOD; SI~SI+1); T~SI; DI~T;    52817000
          DS~1 LIT "."; TALLY~C; TALLY~TALLY+1; C~TALLY;                52819000
PERIOD:   SI ~ T2;  TALLY ~ 0;  T ~ SI;                                 52821000
          DI ~ ERR;  DS ~ 8 LIT " ";                                    52823000
          DI ~ ERR;  SI ~ T2;  DS ~ C CHR;  SI ~ T;  DS ~ LIT "~";      52825000
          C(   IF SC ! "." THEN BEGIN TALLY ~ TALLY + 1;                52827000
               SI ~ SI + 1; END ELSE BEGIN                              52829000
                    T2 ~ TALLY;                                         52831000
                    SI ~ T;  DI ~ INTG;                                 52833000
                    DS ~ T2 OCT;                                        52835000
                    TALLY ~ 0;  SI ~ SI + 1;  T ~ SI;                   52837000
                    END);                                               52839000
          T2 ~ TALLY;  SI ~ T;  DI ~ FRACTION;  DS ~ T2 OCT;            52841000
          SI ~ LOC T2;  DI ~ TENS;  DS ~ 1 WDS;                         52843000
          END STREAM;                                                   52845000
INTEGER STREAM PROCEDURE STRNGLENGTH(STR);                              52847000
BEGIN LABEL EXIT; TALLY ~1; SI~STR;8(8(IF SC=""" THEN JUMP OUT 2 TO EXIT52849000
     ;SI~SI+1); TALLY~TALLY+1); EXIT:STRNGLENGTH~TALLY; END STRNGLENGTH;52851000
ALPHA STREAM PROCEDURE JUSTIFY(S);                                      52853000
          BEGIN LOCAL T; DI~LOC JUSTIFY; DI~DI+2; DS~6 LIT " ";         52855000
          SI~S; SI~SI+1; 6(IF SC ! """ THEN BEGIN SI~SI+1;              52857000
          TALLY~TALLY+1; END); DI~DI-6; T~TALLY; SI~S;                  52859000
          SI~SI+1; DS~T CHR; END JUSTIFY;                               52861000
% ----------------------- L O A D V E R B  -------------------------    52863000
COMMENT                                                                 52865000
          LOADVERB SCHEDULES THE LOADING OF DISK FILES FROM THE         52867000
          SYSTEM DISK FILE;                                             52869000
PROCEDURE LOADVERB(XIT,INFO,IDENT); LABEL XIT; ARRAY INFO,IDENT[0];     52871000
BEGIN                                                                   52873000
     INFO[LOADLIST]~0;                                                  52875000
     T1~0; I~I-1;                                                       52877000
     WHILE T~SYNTAX[I~I+1]!ENDR DO                                      52879000
     BEGIN                                                              52881000
          IF T=LISTLETRS THEN INFO[LOADLIST]~1;                         52883000
          IF T.CLASSFIELD=IDLETTER THEN T1~BLANKFILL(IDENT[T.KEYFIELD   52885000
               |2]);                                                    52887000
     END;                                                               52889000
     IF T1=0 THEN                                                       52891000
     BEGIN                                                              52893000
          ERRNUMBER~7;                                                  52895000
          RUNNER[USER]~"E";                                             52897000
          INFO[LOADLINK]~0;                                             52899000
          GO TO XIT;                                                    52901000
     END NO NAME GIVEN;                                                 52903000
     INFO[LOADLINK]~T1;                                                 52905000
     INFO[FILELIMITS]~0;                                                52907000
     RUNNER[USER]~"I";                                                  52909000
     GO TO XIT;                                                         52911000
END LOADVERB;                                                           52913000
% ------------------------- G E T S T N U M B  ----------------------   52915000
          COMMENT                                                       52917000
               THIS PROCEDURE RETURNS AN 18 BIT STATEMENT NUMBER        52919000
               USING AN INDEX INTO THE TEMPCON TABLE   ;                52921000
INTEGER PROCEDURE GETSTNUMB(C,INFO); VALUE C; INTEGER C; ARRAY INFO[0]; 52923000
          BEGIN                                                         52925000
          INTG ~ FRACTION ~ TENS ~ 0;                                   52927000
          GETNUMB(INTG,FRACTION,TENS,SYNTAX[SCANCON+C],INFO[IOBUF]);    52929000
          IF SYNTAX[SCANCON+C+2].[1:5] = "E"  OR   INTG = 0  OR         52931000
          INTG > 99 OR TENS>3 THEN BEGIN OK~FALSE;GETSTNUMB~0 END ELSE  52933000
                 IF GETSTNUMB ~ (FRACTION ~ FRACTION | 10*(3-TENS))&    52935000
                         INTG[31:41:7] = 0 THEN OK ~ FALSE;             52937000
          IF NOT OK THEN IF ERRNUMBER=0 THEN ERRNUMBER~8;               52939000
               COMMENT INVALID STATEMENT NUMBER;                        52941000
          END GETSTNUMB;                                                52943000
% ----------------- C O N V E R T C O N  ------------------------       52945000
          COMMENT                                                       52947000
               THIS PROCEDURE USES AN INDEX INTO THE SCANCON ARRAY TO   52949000
               CONVERT A CONSTANT TO BINARY.  THE SIGN IS NEGATIVE      52951000
               IF THE RANGE IS 0-63;                                    52953000
INTEGER STREAM PROCEDURE EXPONENT(E);  VALUE E;                         52955000
          BEGIN LOCAL T;                                                52957000
          SI ~ LOC E;  IF SC = "E" THEN BEGIN                           52959000
               TALLY ~ 0;  SI ~ SI + 1;  IF SC < "0" THEN SI ~ SI + 1;  52961000
               3(IF SC="." THEN JUMP OUT ELSE TALLY~TALLY+1; SI~SI+1);  52963000
               T ~ TALLY;  SI ~ SI - T;  DI ~ LOC EXPONENT;             52965000
               DS ~ T OCT;  DI ~ LOC EXPONENT;  SKIP 1 DB;              52967000
               SI ~ LOC E;  SI ~ SI + 1;  IF SC = "-" THEN DS ~ 1 SET;  52969000
               END EXPONENT PART PRESENT;                               52971000
          END EXPONENT;                                                 52973000
REAL PROCEDURE CONVERTCON(L); VALUE L; INTEGER L;                       52975000
          BEGIN                                                         52977000
          INTG ~ 0;                                                     52979000
          FRACTION ~ 0;                                                 52981000
          PFLAG ~ FALSE;                                                52983000
          TENS ~ 0;                                                     52985000
          FOR C ~ 1 STEP 1 UNTIL NCR(SYNTAX[SCANCON+L], 0)              52987000
          DO IF NC ~ NCR(SYNTAX[SCANCON+L], C) = "."                    52989000
               THEN BEGIN                                               52991000
               PFLAG ~ TRUE;                                            52993000
               INTG ~ FRACTION;                                         52995000
               FRACTION ~ TENS ~ 0;                                     52997000
          END ELSE BEGIN                                                52999000
               TENS ~ TENS + 1;                                         53001000
               FRACTION ~ FRACTION | 10 + NC;                           53003000
               END NEXT CHARACTER;                                      53005000
          IF PFLAG THEN S1~(INTG+FRACTION/10*TENS)|                     53007000
          10*EXPONENT(SYNTAX[SCANCON+L+2]) ELSE S1~FRACTION             53009000
          |10*EXPONENT(SYNTAX[SCANCON+L+2]);                            53011000
          IF S1<64 THEN IF S1 MOD 1=0 THEN CONVERTCON~-S1 ELSE          53013000
               CONVERTCON~S1 ELSE CONVERTCON~S1;                        53015000
          END CONVERTCON;                                               53017000
% ------------------------  C L E A N U P C O D E  -------------------  53019000
     COMMENT                                                            53021000
          THIS PROCEDURE PACKS CODE FROM THE TEMPCODE ARRAY INTO THE    53023000
          CODE ARRAY.  IN THE PROCESS IT FIXES CONSTANTS (OR LITERALS)  53025000
          STRINGS, AND BRANCH TO STATEMENT NUMBER;                      53027000
STREAM PROCEDURE CODESTORE(CODE,SYNTAX,T4);                             53029000
VALUE SYNTAX,T4; BEGIN SI~LOC SYNTAX;SI~SI+6;DI~CODE;2(DI~DI+T4);       53031000
     DS~2 CHR; END;                                                     53033000
PROCEDURE CLEANUPCODE(INFO, CODE, XIT);                                 53035000
          ARRAY INFO[0], CODE[0];   LABEL  XIT;                         53037000
          BEGIN                                                         53039000
          CODE[INFO[CMAX]]~0;                                           53041000
          T ~ TEMPCODE - 1;  T1 ~ 0;                                    53043000
          WHILE I ~ SYNTAX[T~T+1] ! ENDR DO                             53045000
               IF I ~ I.CLASSFIELD = GOTO OR I = DOPART OR I = FORM     53047000
                    THEN BEGIN                                          53049000
                    T ~ T+1;  T1 ~ T1 + 2;  END ELSE T1 ~ T1 + 1;       53051000
                         COMMENT  COUNT NUMBER OF INSTRUCTIONS,         53053000
                              SKIP OVER STATEMENT NUMBERS;              53055000
          T1~(T1+3) DIV 4 + 1; COMMENT FOUR INSTRUCTIONS/WORD;          53057000
          T ~ TEMPCODE - 1;                                             53059000
          T5 ~ INFO[CODEMAX] + 1;  COMMENT  SKIP OVER CONTROL WORD;     53061000
          FLAG ~ TRUE;                                                  53063000
          T4 ~ 0;                                                       53065000
          WHILE I ~ SYNTAX[T~T+1] ! ENDR AND OK DO BEGIN                53067000
               IF T5+T1 } MAXCODESIZE THEN BEGIN                        53069000
                    IF MAXCODESIZE~MAXCODESIZE+50>1022 THEN BEGIN       53071000
                         MAXCODESIZE~MAXCODESIZE-50;                    53073000
                         ERRNUMBER~36;                                  53075000
                         RUNNER[USER]~"E";                              53077000
                         COMMENT  PROGRAM TOO LARGE, TRY SAVE, ERASE,   53079000
                              LOAD;                                     53081000
                         GO TO XIT;  END HANDLING CODE OVERFLOW 1022;   53083000
                    REALLOCATE ~ TRUE;                                  53085000
                    GO TO XIT;  END CODE OVERFLOW;                      53087000
               IF FLAG THEN BEGIN                                       53089000
                    IF T6~I.CLASSFIELD=GOTO OR T6=FORM                  53091000
                         THEN BEGIN                                     53093000
                         OK ~ TRUE;                                     53095000
                         T2 ~ GETSTNUMB(I.KEYFIELD,INFO[*]);            53097000
                    IF NOT OK THEN ERRNUMBER~8;                         53099000
                         COMMENT INVALID STATEMENT NUMBER;              53101000
                         SYNTAX[T].KEYFIELD ~ T2.[30:6];                53103000
                         SYNTAX[T+1] ~ T2;                              53105000
                         FLAG ~ FALSE;                                  53107000
                         END INSERTION OF 18 BIT STATEMENT NUMBER;      53109000
                    IF I.CLASSFIELD = DOPART THEN BEGIN                 53111000
                         T2~ENTIER(ABS(CONVERTCON(I.KEYFIELD)));        53113000
                         IF T2>0 AND T2{99 THEN BEGIN                   53115000
                              T2~T2|1024;                               53117000
                              SYNTAX[T].KEYFIELD ~ T2.[30:6];           53119000
                              SYNTAX[T+1] ~ T2;                         53121000
                              FLAG ~ FALSE;                             53123000
                         END ELSE BEGIN OK~FALSE; ERRNUMBER~9; END;     53125000
                         COMMENT INVALID PART NUMBER;                   53127000
                         END INSERTION OF PART NUMBER;                  53129000
                    IF I.CLASSFIELD = CONSTANT THEN BEGIN               53131000
                         IF T2 ~ CONVERTCON(I.KEYFIELD) >0 THEN BEGIN   53133000
                              CODE[INFO[CODEMAX]+(T1~T1+1)-1] ~ T2;     53135000
                              SYNTAX[T].KEYFIELD ~ T1 - 1;              53137000
                         END ELSE                                       53139000
                              SYNTAX[T] ~ LITERAL | 64 + ABS(T2);       53141000
                         END CONVERSION OF CONSTANT;                    53143000
                    IF I.CLASSFIELD= PRINTSTRING THEN BEGIN             53145000
                         SYNTAX[T].KEYFIELD~T1;                         53147000
                         T6~STRNGLENGTH(SYNTAX[SCANCON+I.KEYFIELD])-1;  53149000
                         IF T5+T1+T6{MAXCODESIZE THEN FOR T3~0 STEP 1   53151000
                         UNTIL T6 DO CODE[INFO[CODEMAX]+(T1~T1+1)-1]    53153000
                         ~SYNTAX[SCANCON+I.KEYFIELD+T3] ELSE T1~T1+T6;  53155000
                         END INSERTION OF STRING CONSTANTS;             53157000
                    IF I.CLASSFIELD = STRINGFETCH THEN BEGIN            53159000
                         CODE[INFO[CODEMAX]+(T1~T1+1)-1]~               53161000
                              JUSTIFY(SYNTAX[SCANCON+I.KEYFIELD]);      53163000
                         SYNTAX[T].KEYFIELD~ENTIER(T1-1);               53165000
                         END INSERTING 7 CHAR  ARITHMETIC STRING;       53167000
               END ELSE FLAG ~ TRUE;  COMMENT  SKIP OVER ST #;          53169000
               CODESTORE(CODE[T5],SYNTAX[T],T4);                        53171000
               IF (T4~T4+1) = 4 THEN BEGIN T4 ~ 0;  T5 ~ T5 + 1; END;   53173000
               END PACKING SYLLABLES INTO CODE ARRAY;                   53175000
          IF SYNTAX[TEMPCODE].CLASSFIELD=NEXTLINE THEN T1~1;            53177000
               COMMENT  REMARKS ARE ONLY ENTERED AS A LINK WORD;        53179000
          CODE[INFO[CMAX]+T1] ~ 0;  COMMENT  ZERO NEXT ELEMENT;         53181000
          CODE[INFO[CMAX]].CSIZE ~ T1;  COMMENT  ENTER SIZE OF SEGMENT; 53183000
          IF PROGMODE THEN BEGIN                                        53185000
               CODE[INFO[CMAX]].CSNUMB ~ I ~ GETSTNUMB(0,INFO[*]);      53187000
                         COMMENT  INSERT STNUMB INTO CONTROL WORD;      53189000
               IF OK THEN BEGIN                                         53191000
               STORE(INFO[*], CODE[INFO[CMAX]]);                        53193000
               INFO[CMAX] ~ INFO[CMAX] + T1;  COMMENT  UPDATE CMAX      53195000
                    TO MAKE IT A PERMANENT ENTRY;                       53197000
               END OK                                                   53199000
          END PROGRAM MODE ELSE BEGIN                                   53201000
               INFO[CBASEINDX] ~ INFO[CMAX];                            53203000
               INFO[CINDX] ~ INFO[CMAX] + 1;                            53205000
               INFO[BUFINDEX] ~ 0;                                      53205100
               INFO[LINDX] ~ 0;                                         53207000
               INFO[SINDX] ~ 0; COMMENT  SET REGISTERS FOR EXECUTE;     53209000
               INFO[RUNCOUNT]~0;  INFO[RUNLIMIT]~1000;                  53211000
               RUNNER[USER] ~ "S";                                      53213000
               INFO[MODEFLAG]~1;                                        53215000
               END CALC MODE;                                           53217000
          END CLEANUP CODE;                                             53219000
% ------------------------ S T O R E  ----------------------------      53221000
COMMENT                                                                 53223000
          STORE PACKS A CARD IMAGE AND WRITES SEGMENTS ON THE DISK.  IT 53225000
          ERASES DISK AREAS AHEAD OF IT BY ERASING A LARGE CHUNK AT     53227000
          A TIME;                                                       53229000
COMMENT                                                                 53231000
          WORD 1-9: 72 CHARACTER IMAGE.                                 53233000
          WORD 10:  FILEID=FIRST CARD DISKPOINTER;                      53235000
PROCEDURE STORE(INFO,CODECELL); ARRAY INFO[0]; REAL CODECELL;           53237000
IF RUNNER[USER].[36:12]="R" THEN CODECELL.CDISKADDR~INFO[FILELIMITS].   53239000
     [18:15] ELSE                                                       53241000
BEGIN                                                                   53243000
     IF INFO[FILEID]=0 THEN ENTERDIRECTORY(INFO[*]);                    53245000
     CODECELL.CDISKADDR~DISKPOINTER;                                    53247000
     T~DISKPOINTER MOD 3 | 10;                                          53249000
     DISKBUF[T+9]~INFO[FILEID].[33:15];                                 53251000
     MOVE9(SYNTAX[0],DISKBUF[T]);                                       53253000
     IF RUNNER[USER].[36:12] ! "I" OR DISKPOINTER MOD 3 = 2 OR          53253100
          (T3 ~ INFO[FILELIMITS]).[18:15] < T3.[33:15] THEN             53253200
     WRITE(DSK[T~DISKPOINTER DIV 3],30,DISKBUF[*]);                     53255000
     IF(DISKPOINTER~DISKPOINTER+1) MOD 30 = 0 THEN                      53257000
     BEGIN                                                              53259000
          DISKBUF[0]~"EOF"; COMMENT ERASE NEXT SECTION OF DISK;         53261000
          FOR T2~1 STEP 1 UNTIL 29 DO DISKBUF[T2]~0;                    53263000
          FOR T2~T+1 STEP 1 UNTIL T+10 DO WRITE(DSK[T2],30,DISKBUF[*]); 53265000
          IF DISKPOINTER MOD 540 = 0 THEN                               53267000
          BEGIN                                                         53269000
               WRITE(DSK[(DISKPOINTER+539) DIV 3],30,DISKBUF[*]);       53271000
               CLOSE(DSK);                                              53273000
               COMMENT ON A NEW ROW, UPDATE EOF POINTER BY WRITING LAST 53275000
                    RECORD IN THE NEW ROW AND CLOSING THE FILE;         53277000
          END;                                                          53279000
     END ERASING NEXT SECTION OF DISK;                                  53281000
          IF DISKPOINTER MOD 3=0 THEN DISKBUF[19]~DISKBUF[29]~"NUL";    53283000
END STORE;                                                              53285000
% ------------------------- L I N K D A T A ----------------------------53287000
PROCEDURE LINKDATA(INFO,CODE); ARRAY INFO,CODE[0];                      53289000
BEGIN                                                                   53291000
     COMMENT THIS PROCEDURE LINKS TOGETHER DATA STATEMENTS VIA THE      53293000
          [38:10] FIELD OF THE WORD FOLLOWING THE LINK WORD OF THE DATA 53295000
          STATEMENT.  A DATA STATEMENT WILL UNIQUELY HAVE A CALL TO     53297000
          INTRINSIC 4 ("+4") AS THE FIRST SYLLABLE IN THE CODE;         53299000
     T~C~INFO[DATAPOINTER]~0;                                           53301000
     INFO[DATAINDEX]~0;                                                 53303000
     INFO[DATASTART]~0;                                                 53305000
     COMMENT CHECK FOR DATA STATEMENT;                                  53307000
     DO BEGIN                                                           53309000
          C~CODE[C].CLINK;                                              53311000
     IF CODE[C+1].[1:11] = "+4" THEN                                    53313000
     BEGIN                                                              53315000
          IF INFO[DATASTART] = 0 THEN INFO[DATASTART]~C;                53317000
          IF T ! 0 THEN CODE[T+1].[38:10]~C;                            53319000
          T~C;                                                          53321000
     END END UNTIL C = 0;                                               53323000
     IF T ! 0 THEN CODE[T+1].[38:10]~0;                                 53325000
END LINKDATA;                                                           53327000
% ------------------ L I N K U P  ----------------------------------    53329000
STREAM PROCEDURE CRUNCH(S, D, N1, N32);  VALUE N1, N32;                 53331000
          BEGIN  SI~S;  DI~D;  DS~N1 WDS;  N32(DS~32 WDS);  END CRUNCH ;53333000
PROCEDURE LINKUP(FCN,INFO,CODE,XIT);                                    53335000
          VALUE FCN; INTEGER FCN;  ARRAY INFO[0], CODE[0];              53337000
          LABEL XIT;                                                    53339000
          BEGIN                                                         53341000
          DEFINE SEGMENT = #;                                           53343000
          S1 ~ 0;  COMMENT  INITIAL STATEMENT # = 0;                    53345000
          S2 ~ 131071;  COMMENT  TERMINAL STATEMENT = INFINITY;         53347000
          I ~ SCANCODE;                                                 53349000
          WHILE T~SYNTAX[I~I+1]!ENDR DO BEGIN                           53351000
               OK~TRUE;                                                 53353000
               IF T=PART THEN IF ABS(S1~CONVERTCON((T~SYNTAX[I~I+1]).   53355000
                    KEYFIELD)) >0 AND S1{99 THEN BEGIN                  53357000
                         S1~0&S1[31:41:7];S2~0&S1[31:31:7]&999[38:38:10]53359000
                         ;END ELSE BEGIN                                53361000
                         COMMENT INVALID PART NUMBER;                   53363000
                         ERRNUMBER~9; RUNNER[USER]~"E"; GO TO           53365000
                         XIT; END ELSE                                  53367000
               IF T.CLASSFIELD=CON THEN IF S1=0 THEN S1~GETSTNUMB(      53369000
                    T.KEYFIELD,INFO[*]) ELSE S2~GETSTNUMB(T.KEYFIELD,   53371000
                         INFO[*]);                                      53373000
               COMMENT  SET UP STATEMENT NUMBER RANGE;                  53375000
          IF NOT OK THEN BEGIN RUNNER[USER]~"E"; GO TO XIT; END END;    53377000
          IF S1!0 AND S2=131071 THEN S2~S1;                             53379000
          C~1;                                                          53381000
          CODE[LASTLINK~0]~0;      COMMENT (LASTLINK DEFINED AS T4);    53383000
          COMMENT  BEGIN INITIAL PASS TO FIND STMNT NUMBERS IN RANGE    53385000
               AND REMOVE ERASED CODE;                                  53387000
          WHILE C<INFO[CMAX] DO BEGIN                                   53389000
               WHILE T~CODE[C]<0 DO BEGIN                               53391000
                    COMMENT  CRUNCH ERASED CODE;                        53393000
                    CRUNCH(CODE[C+(T3~T.CSIZE)], CODE[C],               53395000
                         ENTIER((T2~INFO[CMAX]-C) MOD 32), T2 DIV 32);  53397000
                    CODE[INFO[CMAX]~INFO[CMAX]-T3]~0;                   53399000
                    END COMPRESSING CODE ARRAY;                         53401000
               IF T~T.CSNUMB}S1 AND T{S2 AND T!0 THEN BEGIN             53403000
                    COMMENT  FORM LINKED LIST OF THOSE STMNTS IN RANGE; 53405000
                    CODE[LASTLINK].CLINK~C;                             53407000
                    LASTLINK~C;                                         53409000
                    END STMNT IN RANGE;                                 53411000
                    IF T~CODE[C].CSIZE>0 THEN C~C+T ELSE                53413000
                         IF C<INFO[CMAX] THEN BEGIN                     53415000
                         RUNNER[USER] ~ "E";                            53417000
                         SYNTAXERR(27);  GO TO XIT;  END ARRAY INVALID; 53419000
               END INITIAL PASS;                                        53421000
          COMMENT  LINKED LIST IS NOW SERIAL THRU CODE ARRAY, STARTING  53423000
                    AT LINK WORD 0 AND LINKING FORWARD UNTIL THE CELL   53425000
                    CODE[LASTLINK];                                     53427000
          IF LASTLINK=0 THEN BEGIN                                      53429000
               IF S1=0 THEN ERRNUMBER~10 ELSE ERRNUMBER~11;             53431000
               RUNNER[USER]~"E";                                        53433000
               GO TO XIT;                                               53435000
               END NO STATEMENTS PRESENT OR IN RANGE;                   53437000
          CODE[LASTLINK].CLINK~0;  COMMENT  SET STOPPER LINK;           53439000
          T6~522242;  COMMENT  MAX POSSIBLE PAIR SWAPS;                 53441000
          DO BEGIN  COMMENT   FORM LINKED-LIST BY REPEATED PAIR SWAPS;  53443000
               FLAG~TRUE;                                               53445000
               I~T5~0;                                                  53447000
               COMMENT  I IS 1 STMNT AHEAD OF C,  T5 IS 1 STMNT BEHIND  53449000
                         C SO THE PREVIOUS FORWARD POINTER MAY BE       53451000
                         MODIFIED;                                      53453000
               DO BEGIN                                                 53455000
                    C~I;  COMMENT  STEP C AND I 1 STMNT;                53457000
                    I~CODE[C].CLINK;                                    53459000
                    IF (S1~CODE[C].CSNUMB) = (S2~CODE[I].CSNUMB)        53461000
                         THEN BEGIN                                     53463000
                              COMMENT  LINK AROUND A DUPLICATE STMNT AND53465000
                                   VOID IT;                             53467000
                              CODE[T5].CLINK~I;                         53469000
                              CODE[C].SBIT~1;                           53471000
                         END DUPLICATE ELSE IF S1>S2 THEN BEGIN         53473000
                              COMMENT  A SWAP IS NECESSARY;             53475000
                              T3~CODE[T5].CLINK;                        53477000
                              CODE[T5].CLINK~CODE[C].CLINK;             53479000
                              CODE[C].CLINK~CODE[I].CLINK;              53481000
                              CODE[I].CLINK~T3;                         53483000
                              COMMENT  THREE WAY SWAP REQUIRED BECAUSE  53485000
                                   LIST IS SPECIFIED BY FORWARD LINKAGE;53487000
                              T5~I;                                     53489000
                              IF I!LASTLINK THEN I~C;                   53491000
                              COMMENT  C NOW BECOMES THE HIGHER         53493000
                                  STATEMENT NUMBER, RATHER THAN I.      53495000
                                   (BUT DONT PASS LASTLINK.);           53497000
                              FLAG~FALSE;                               53499000
                              COMMENT  MUST MAKE ANOTHER PASS;          53501000
                              END SWAP  ELSE T5~C;                      53503000
               IF T6~T6-1=0 THEN BEGIN SYNTAXERR(27);  RUNNER[USER]~"E";53505000
               GO TO XIT;  END;                                         53507000
               COMMENT  INTEGRITY OF ARRAYS HAS BEEN DESTROYED;         53509000
               END INNER DO LOOP UNTIL I=LASTLINK;                      53511000
               IF CODE[C]>0 THEN                                        53513000
               LASTLINK~C;                                              53515000
               COMMENT  BACK UP LASTLINK UNTIL NO MORE SWAPS NEEDED;    53517000
          END UNTIL FLAG;                                               53519000
          INFO[CBASEINDX] ~ CODE[0].CLINK;                              53521000
          IF FCN = "L" THEN RUNNER[USER] ~ "L";                         53523000
               COMMENT  SET LIST OPTION;                                53525000
          IF FCN = "R" THEN BEGIN                                       53527000
               IF INFO[DATAST] = 1 THEN LINKDATA(INFO,CODE);            53529000
               INFO[CINDX] ~ CODE[0].CLINK + 1;                         53531000
               INFO[LINDX] ~ 0;  INFO[SINDX] ~ 0;                       53533000
               COMMENT  SET UP REGISTERS FOR EXECUTE;                   53535000
               RUNNER[USER] ~ "S";  COMMENT  ENTER RUN SCHEDULE;        53537000
               INFO[RUNLIMIT] ~ 1000;   COMMENT  LIMIT TO 1000 STMNTS;  53539000
               INFO[RUNCOUNT]~0;                                        53541000
               INFO[MODEFLAG]~0;                                        53543000
               INFO[BUFINDEX]~0;                                        53545000
               END RUN;                                                 53547000
          IF FCN = "E" THEN BEGIN                                       53549000
               C ~ 0;                                                   53551000
               WHILE C ~ CODE[C].CLINK ! 0 DO CODE[C].SBIT ~ 1;         53553000
               STORE(INFO[*],C);                                        53555000
               END ERASING LINES OF CODE;                               53557000
          IF FCN = "D" THEN RUNNER[USER] ~ "D";                         53559000
               COMMENT   WRITE CARDS ON A DISK FILE FOR SAVE VERB;      53561000
          IF DEBUG THEN WRITE (PR[DBL], 16,CODE[*]);                    53563000
          GO TO XIT;                                                    53565000
          END LINKUP;                                                   53567000
%  -------------------- H E L P V E R B  -----------------------------  53569000
PROCEDURE HELPVERB(INFO, XIT);  ARRAY INFO[0];  LABEL XIT;              53571000
          BEGIN                                                         53573000
          IF HELPTABLE[0] = 0 THEN BEGIN                                53575000
               ERRNUMBER~12;                                            53577000
               RUNNER[USER] ~ "E";                                      53579000
               GO TO XIT;  END NO HELP;                                 53581000
          T2~T1~0;                                                      53583000
          OK ~ TRUE;                                                    53585000
          IF T3~(T~SYNTAX[I+1]).CLASSFIELD = CON THEN T1 ~ GETSTNUMB(   53587000
               T.KEYFIELD,INFO[*])                                      53589000
               ELSE IF T3  ! IDLETTER AND T3 ! ARRAYIDLETTER THEN T2 ~T;53591000
          IF T3 = IDLETTER THEN IF IDENTIFIERS[USER,(T4 ~ T.KEYFIELD)+  53593000
               T4].[1:29] = "4DATA" THEN T2 ~ "38";                     53595000
          IF NOT OK THEN BEGIN RUNNER[USER] ~ "E" ; GO TO XIT; END;     53597000
          INFO[HELPLINK] ~ 0;                                           53599000
          IF T2 ! 0 THEN BEGIN                                          53601000
               INFO[HELPLINK] ~ T2;                                     53603000
               RUNNER[USER] ~ "K";                                      53605000
               GO TO XIT;                                               53607000
               END KEYWORD REQUEST;                                     53609000
          I ~ 0;                                                        53611000
          T1.[38:10] ~ 0;  COMMENT  STEP # = 0;                         53613000
          IF T1 ! 0 THEN WHILE HELPTABLE[I].CSNUMB < T1 DO I ~ I+1;     53615000
               COMMENT  SKIP UP TO FIRST LINE OF A PART;                53617000
          INFO[HELPLINK].[38:10] ~ I;                                   53619000
          INFO[HELPLINK].CSNUMB ~ T1;                                   53621000
          RUNNER[USER] ~ "H";                                           53623000
          GO TO XIT;                                                    53625000
          END HELPVERB;                                                 53627000
% ------------------- R E M O V E V E R B  ---------------------------  53629000
PROCEDURE REMOVEVERB(IDENT,INFO,XIT); ARRAY IDENT,INFO[0]; LABEL XIT;   53631000
BEGIN                                                                   53633000
FILE SAVER DISK SERIAL "FILENAM" "CARDS" (1,10,30);                     53635000
     IF (T~SYNTAX[I+1]).CLASSFIELD!IDLETTER THEN BEGIN                  53637000
          ERRNUMBER~7;                                                  53639000
          RUNNER[USER]~"E";                                             53641000
          GO TO XIT;                                                    53643000
     END NO FILE NAME;                                                  53645000
     T1~BLANKFILL(IDENT[T.KEYFIELD|2]);                                 53647000
     FILL SAVER WITH T1;                                                53649000
     SEARCH(SAVER,INFO[*]);                                             53651000
     IF INFO[0]<0 THEN BEGIN                                            53653000
          ERRNUMBER~13;                                                 53655000
          RUNNER[USER]~"E";                                             53657000
          GO TO XIT;                                                    53659000
     END FILE NOT PRESENT;                                              53661000
     IF INFO[0]!7 THEN BEGIN                                            53663000
          ERRNUMBER ~ 45;                                               53665000
          RUNNER[USER]~"E";                                             53667000
          GO TO XIT;                                                    53669000
          END NOT PRIMARY USER;                                         53671000
     IF INFO[6] ! 0 THEN BEGIN                                          53671100
          ERRNUMBER ~ 48;                                               53671200
          RUNNER[USER] ~ "E";                                           53671300
          GO TO XIT;                                                    53671400
     END FILE IN USE;                                                   53671500
     READ(SAVER,10,INFO[*]);                                            53673000
     CLOSE (SAVER,PURGE);                                               53675000
     ERRNUMBER ~ 14; COMMENT "PURGED" MESSAGE;                          53677000
     RUNNER[USER] ~ "E";                                                53677100
     GO TO XIT;                                                         53679000
END REMOVEVERB;                                                         53681000
% --------------------------- S A V E V E R B  ---------------------    53683000
COMMENT                                                                 53685000
          SAVEVERB SCHEDULES THE CREATION OF A DISK FILE;               53687000
PROCEDURE SAVEVERB(XIT,INFO,IDENT,CODE); LABEL XIT; ARRAY INFO,IDENT,   53689000
     CODE[0];                                                           53691000
BEGIN                                                                   53693000
     IF INFO[FILEID]=0 THEN                                             53695000
     BEGIN                                                              53697000
          ERRNUMBER~10;                                                 53699000
          RUNNER[USER]~"E";                                             53701000
          GO TO XIT;                                                    53703000
     END NO STATEMENTS;                                                 53705000
     INFO[PUBLICFLAG] ~ 0;                                              53707000
     T1~0;                                                              53709000
     I~SCANCODE;                                                        53711000
     WHILE T~SYNTAX[I~I+1]!ENDR DO                                      53713000
          IF T.CLASSFIELD = IDLETTER THEN                               53715000
               IF (T2~IDENT[T.KEYFIELD|2]).[1:35] = "6PUBLI"            53717000
                    THEN INFO[PUBLICFLAG]~1 ELSE T1~BLANKFILL(T2);      53719000
     IF T1=0 THEN                                                       53721000
     BEGIN                                                              53723000
          ERRNUMBER~7;                                                  53725000
          RUNNER[USER]~"E";                                             53727000
          GO TO XIT;                                                    53729000
     END FILE NAME MISSING;                                             53731000
     INFO[FILENAME]~T1;                                                 53733000
     LINKUP("D",INFO,CODE,XIT);                                         53735000
END SAVEVERB;                                                           53737000
% ------------------- O P T I O N V E R B S  -------------------------  53739000
PROCEDURE OPTIONVERBS(TF,IDENT,INFO); VALUE TF; BOOLEAN TF;             53741000
     ARRAY IDENT[0],INFO[0];                                            53743000
          BEGIN                                                         53745000
          WHILE T~SYNTAX[I~I+1].CLASSFIELD!ENDR DO                      53747000
               IF T=IDLETTER THEN BEGIN                                 53749000
                    IF T~IDENT[SYNTAX[I].KEYFIELD|2].[6:42]="SIGNIFI"   53751000
                         THEN GENCODE(INTRINSIC,13+(1-REAL(TF))|2);     53753000
                          COMMENT  SET/CLEAR FREE FIELD TYPE PLACES;    53755000
                    IF T = "TRACE00" THEN                               53757000
                         GENCODE(INTRINSIC,14+(1-REAL(TF))|2);          53757100
                         COMMENT  SET/CLEAR DEBUG TOGGLE;               53759000
                    IF T = "CLOSE00" THEN GENCODE(INTRINSIC,17);        53761000
                         COMMENT  PRINT DEBUG FILE;                     53763000
                    COMMENT   <SPACE FOR MORE OPTIONS> ;                53765000
                    END SETTTING AND CLEARING OPTIONS;                  53767000
          GENCODE(NEXTLINE,0); OK~TRUE;                                 53769000
END OPTIONVERBS;                                                        53771000
% -------------------------------------------------------------------   53773000
COMMENT CHECK FOR NULL STATEMENT;                                       53773100
          IF SYNTAX[SCANCODE] = ENDR THEN GO TO XIT;                    53773200
          EXPOVR ~ EXPERR;  COMMENT  MONITOR EXP. FAULT;                53775000
          HOLDINDEX~HOLD;PDSINDEX~PDS;OSINDEX~OPSTACK;                  53777000
          TEMPCODEINDEX ~ TEMPCODE;                                     53779000
               T ~ I;                                                   53781000
          INDEX~SAV~0;  INHIBITCODE~OK~ERRFLAG~FALSE;                   53783000
          IF PROGMODE THEN I ~ SCANCODE + 1 ELSE I ~ SCANCODE;          53785000
COMMENT   THE INDEX FOR THIS CASE STATEMENT WAS OBTAINED FROM THE ENTRY 53787000
          NUMBER OF DICT[*];                                            53789000
          CASE T OF BEGIN                                               53791000
               BEGIN END;                                               53793000
               SAVEVERB(XIT, INFO, IDENT, CODE);                        53795000
               LOADVERB(XIT,INFO,IDENT);                                53797000
               BEGIN RUNNER[USER]~"Q";  GO TO XIT;                      53799000
                    END QUIT;                                           53801000
               LINKUP("R",INFO[*],CODE[*],XIT);                         53803000
               BEGIN IF SYNTAX[I+1]=ENDR THEN BEGIN                     53805000
                    FOR T~0 STEP 2 WHILE IDENT[T]!0 DO BEGIN            53807000
                         IDENT[T]~0; IDENT[T+1]~UNDEFINED; END;         53809000
                    INFO[LASTUSERARRAY]~0;                              53811000
                    INFO[DATAST]~0;                                     53813000
                    INFO[CMAX]~1;                                       53815000
                    CODE[0]~CODE[1]~0;                                  53817000
                    IF INFO[FILEID]!0 THEN BEGIN   COMMENT  CLOSE FILE; 53819000
                         IF T1~INFO[FILEID].[3:25]=DLOC THEN BEGIN      53821000
                              COMMENT  ENTRY IS IN CURRENT DLOC SEG.;   53823000
                              DIRBUF[INFO[FILEID].[28:5]].SBIT~1;       53825000
                              WRITE(DSK[DLOC],30,DIRBUF[*]);            53827000
                         END ELSE BEGIN  COMMENT  DIFFERENT SEGMENT;    53829000
                              READ(DSK[T1],30,LISTBUF[*]);              53831000
                              LISTBUF[INFO[FILEID].[28:5]].SBIT~1;      53833000
                              WRITE(DSK[T1],30,LISTBUF[*]);             53835000
                              LISTPOINTER~0;                            53837000
                         END ;                                          53839000
                         INFO[FILEID]~0;                                53841000
                         END MARKING USER FILE CLOSED;                  53843000
                    INFO[DICTMAX]~0;                                    53845000
                    GO TO XIT;                                          53847000
                    END;                                                53849000
                    IF SYNTAX[I+1].CLASSFIELD=IDLETTER THEN BEGIN       53851000
                         FOR T~I+1 STEP 1 WHILE SYNTAX[T]!ENDR DO       53853000
                         IF SYNTAX[T].CLASSFIELD=IDLETTER THEN IDENT[   53855000
                         SYNTAX[T].KEYFIELD|2+1]~UNDEFINED;GO TO XIT;   53857000
                         END ELSE LINKUP("E",INFO[*],CODE[*],XIT);      53859000
                    END ERASE;                                          53861000
               LINKUP("L",INFO[*],CODE[*],XIT);                         53863000
               BEGIN HELPVERB(INFO[*], XIT);  GO TO XIT;  END HELP;     53865000
               REMOVEVERB(IDENT[*], INFO[*], XIT);                      53867000
               OK~REPLSTMT;                                             53873000
               OK ~ INPUTSTMT(INFO);                                    53875000
               OK~PRINTSTMT;                                            53877000
               BEGIN SYNTAX[TEMPCODE]~NEXTLINE|64; OK~TRUE; END REMARK; 53879000
          OPTIONVERBS(TRUE,IDENT,INFO); COMMENT USE;                    53879100
          OPTIONVERBS(FALSE,IDENT,INFO); COMMENT CLEAR;                 53879200
               OK~GOSTMT;                                               53881000
               BEGIN SYNTAX[TEMPCODE]~STOP|64; OK~TRUE; END STOP;       53883000
               OK~DOSTMT;                                               53885000
               OK~WHILESTMT;                                            53887000
               OK~UNTILSTMT;                                            53889000
               BEGIN OK~ARRAYDECL(INFO,USERARRAY);  IF REALLOCATE THEN  53891000
                    GO TO XIT;  END ARRAYDECL;                          53893000
               OK ~ CONDSTMT(INFO);                                     53895000
               BEGIN  SYNTAX[TEMPCODE]~NEXTLINE|64;  OK~TRUE; END FORM; 53897000
               OK~DATASTMT(INFO);                                       53899000
               OK ~ RESTORESTMT;                                        53901000
               END CASE T;                                              53903000
               IF DEBUG THEN WRITE (PR[DBL], DMPCODE,                   53905000
                    FOR T~TEMPCODE STEP 1 UNTIL TEMPCODE+29 DO          53907000
               SYNTAX[T]);                                              53909000
               IF OK AND NOT ERRFLAG THEN BEGIN                         53911000
                    NEXT; IF SYNTAX[I]!ENDR THEN BEGIN                  53913000
                         SYNTAXERR(12); COMMENT MISSING OPERATOR OR     53915000
                         SEPARATOR HAS CAUSED THEN SYNTAX ANALYZER TO   53917000
                         TERMINATE PREMATURELY;                         53919000
                         RUNNER[USER]~"E"; GO TO XIT; END;              53921000
                    CLEANUPCODE(INFO[*],CODE[*],XIT);                   53923000
                    IF DEBUG THEN WRITE(PR[DBL],15,CODE[*]);            53925000
                    IF OK THEN ENTERDICT~TRUE ELSE RUNNER[USER]~"E";    53927000
               END OK ELSE BEGIN                                        53929000
                    IF ERRNUMBER=0 THEN ERRNUMBER~16;                   53931000
                         COMMENT IF NO SPECIFIC ERROR HAS BEEN GENERATED53933000
                         SET UP "SYNTAX ERROR" MESSAGE;                 53935000
                    RUNNER[USER]~"E"; END;                              53937000
               GO TO XIT;                                               53939000
EXPERR:   ERRNUMBER~35;  RUNNER[USER]~"E";                              53941000
          COMMENT        EXPONENT OVERFLOW;                             53943000
XIT:                                                                    53945000
END TRANSLATOR;                                                         53947000
% ********************************************************************  60001000
% ********************************************************************  60003000
% *********************** A C C E P T  *************************        60005000
% ********************************************************************  60007000
% ********************************************************************  60009000
     COMMENT                                                            60011000
          THIS PROCEDURE HANDLES CONVERSATIONAL REQUESTS .              60013000
          IT WILL HANDLE REQUESTS FOR ENTRY OF VALUES FOR UNDEFINED     60015000
          VARIABLES AS WELL AS OTHER FUNCTIONS;                         60017000
PROCEDURE ACCEPT(INFO,IDENT,STACK,USERARRAY);                           60019000
          ARRAY INFO,IDENT,STACK,USERARRAY[0];                          60021000
          BEGIN LABEL EXPERR,XIT;  MONITOR EXPOVR;                      60023000
INTEGER STREAM PROCEDURE NCR(S,N); VALUE N;                             60025000
          BEGIN SI ~ S;  SI ~ SI + N;  DI ~ LOC NCR;  DI ~ DI + 7;      60027000
               DS ~ 1 CHR; END;   COMMENT  RETURNS 1 CHARACTER ;        60029000
% ----------------- C O N V E R T C O N  ------------------------       60031000
          COMMENT                                                       60033000
               THIS PROCEDURE USES AN INDEX INTO THE SCANCON ARRAY TO   60035000
               CONVERT A CONSTANT TO BINARY;                            60037000
INTEGER STREAM PROCEDURE EXPONENT(E);  VALUE E;                         60039000
          BEGIN LOCAL T;                                                60041000
          SI ~ LOC E;  IF SC = "E" THEN BEGIN                           60043000
               TALLY ~ 0;  SI ~ SI + 1;  IF SC < "0" THEN SI ~ SI + 1;  60045000
               3(IF SC="." THEN JUMP OUT ELSE TALLY~TALLY+1; SI~SI+1);  60047000
               T ~ TALLY;  SI ~ SI -T;  DI ~ LOC EXPONENT;              60049000
               DS ~ T OCT;  DI ~ LOC EXPONENT;  SKIP 1 DB;              60051000
               SI ~ LOC E;  SI ~ SI + 1;  IF SC = "-" THEN DS ~ 1 SET;  60053000
               END EXPONENT PART PRESENT;                               60055000
          END EXPONENT;                                                 60057000
REAL PROCEDURE CONVERTCON(L); VALUE L; INTEGER L;                       60059000
          BEGIN                                                         60061000
          INTG ~ 0;                                                     60063000
          FRACTION ~ 0;                                                 60065000
          PFLAG ~ FALSE;                                                60067000
          TENS ~ 0;                                                     60069000
          FOR C~1 STEP 1 UNTIL SYNTAX[SCANCON+L].[1:5]                  60071000
          DO IF NC ~ NCR(SYNTAX[SCANCON+L], C) = "."                    60073000
               THEN BEGIN                                               60075000
               PFLAG ~ TRUE;                                            60077000
               INTG ~ FRACTION;                                         60079000
               FRACTION ~ TENS ~ 0;                                     60081000
          END ELSE BEGIN                                                60083000
               TENS ~ TENS + 1;                                         60085000
               FRACTION ~ FRACTION | 10 + NC;                           60087000
               END NEXT CHARACTER;                                      60089000
          IF PFLAG THEN CONVERTCON~(INTG+FRACTION/10*TENS)|10*EXPONENT( 60091000
               SYNTAX[SCANCON+L+2]) ELSE CONVERTCON~FRACTION            60093000
               |10*EXPONENT(SYNTAX[SCANCON+L+2]);                       60095000
          END CONVERTCON;                                               60097000
ALPHA STREAM PROCEDURE JUSTIFY(S);                                      60097100
BEGIN LOCAL T; DI~LOC JUSTIFY; DI~DI+2; DS~6 LIT " ";                   60097200
     SI~S; SI~SI+1; 6(IF SC ! """ THEN BEGIN SI~SI+1;                   60097300
     TALLY~TALLY+1; END); DI~DI-6; T~TALLY; SI~S;                       60097400
     SI~SI+1; DS~T CHR;                                                 60097500
END JUSTIFY;                                                            60097600
% --------------------------------------------------------------        60099000
          EXPOVR~EXPERR;                                                60101000
          I~SCANCODE;  IF QSTOP THEN BEGIN                              60103000
               INFO[FORMFLAG]~0;                                        60105000
               INFO[ACCEPTFLAG]~RUNNER[USER]~INFO[MODE]~INFO[BUFINDEX]  60107000
               ~0;  GO TO XIT;                                          60109000
               END ELSE                                                 60111000
          IF INFO[MODE] > 0 THEN BEGIN                                  60113000
               IF SYNTAX[I~SCANCODE].CLASSFIELD = ADSUBOP THEN I~I+1;   60115000
               IF ((T1~(T~SYNTAX[I]).CLASSFIELD) = CON OR T1 = STRNG)   60117000
                    AND SYNTAX[I+1] = ENDR THEN BEGIN                   60119000
                    IF T1 = CON THEN                                    60119100
                    T1~(IF I!SCANCODE THEN IF SYNTAX[I-1].KEYFIELD="1"  60121000
                    %CHECK FOR MINUS SIGN                               60123000
                    THEN -CONVERTCON(T.KEYFIELD) ELSE CONVERTCON(T.     60125000
                    KEYFIELD) ELSE CONVERTCON(T.KEYFIELD)) ELSE         60127000
                    T1 ~ JUSTIFY(SYNTAX[SCANCON+T.KEYFIELD]);           60127100
                    IF INFO[ARRAYFLAG]=0 THEN IDENT[INFO[MODE]]~T1      60129000
                    ELSE BEGIN INFO[ARRAYFLAG]~0;USERARRAY[INFO[MODE]-1]60131000
                    ~T1; END;                                           60133000
               COMMENT PUT CONSTANT VALUE IN IDENT OR USER ARRAY;       60135000
                    IF STACK[INFO[SINDX]] = UNDEFINED THEN              60137000
                         STACK[INFO[SINDX]] ~ T1;  COMMENT  INSERT      60139000
                                   IN STACK ALSO;                       60141000
                    RUNNER[USER] ~ "S"; COMMENT  RE-SCHEDULE;           60143000
                    INFO[MODE] ~ 0;                                     60145000
                    GO TO XIT;                                          60147000
               END ELSE BEGIN                                           60149000
                    ERRNUMBER~17; COMMENT NOT A NUMBER OR LITERAL;      60151000
                    RUNNER[USER]~"E";                                   60153000
                    GO TO XIT;                                          60155000
                    END DIAGNOSTIC FOR NO CONSTANT;                     60157000
               END ACCEPTING A VALUE ELSE BEGIN                         60159000
                    INFO[RUNCOUNT]~0;                                   60161000
                    IF QYES THEN BEGIN                                  60163000
                              INFO[MODE] ~ 0;                           60165000
                              INFO[RUNLIMIT]~INFO[RUNLIMIT]|2;          60167000
                              RUNNER[USER]~"S"; GO TO XIT; END ELSE     60169000
                    IF QNO THEN BEGIN INFO[MODE]~0; GO TO XIT; END ELSE 60171000
                    BEGIN                                               60173000
                         ERRNUMBER~18; COMMENT NOT YES OR NO;           60175000
                         RUNNER[USER] ~ "E";                            60177000
                         GO TO XIT;                                     60179000
                         END ;                                          60181000
               END ACCEPTING ALPHA CONTROL;                             60183000
EXPERR:   ERRNUMBER~19; RUNNER[USER]~"E"; COMMENT EXPONENT OVERFLOW     60185000
               IN ENTERED NUMBER;                                       60187000
XIT:      END ACCEPT;                                                   60189000
% ********************************************************************  70001000
% ********************************************************************  70003000
% **************** E X E C U T E   C O D E  *************************   70005000
% ********************************************************************  70007000
% ********************************************************************  70009000
COMMENT                                                                 70011000
         OP CODE         MODIFIER MEANING        FUNCTION               70013000
         *******         ****************        ********               70015000
         LITERAL         VALUE (6 BITS)          NUMPER TO TOP OF STACK 70017000
         IDFETCH         REL. ADDR. (ID)         NUMBER TO TOP OF STACK 70019000
         CONSTANT        REL. ADDR. (CODE)       NUMBER TO TOP OF STACK 70021000
         ARRAYFETCH      REL. ADDR (ID)          ARRAY ELEMENT FETCH    70023000
%        ARITHOP         OPERATOR #              PERFORM ARITH. OP.     70025000
%        RELTEST         OPERATOR #              PERFORM RELATIONAL TEST70027000
%        FUNCTCALL       FUNCTION #              PERFORM ARITH FUNCTION 70029000
         NDSTORE         REL. ADDR. (ID)         NON-DESTRUCTIVE STORE  70031000
         DSTORE          REL. ADDR. (ID)         DESTRUCTIVE STORE      70033000
         NDSTOREA        REL. ADDR. (ID)         ARRAY NDSTORE          70035000
         DSTOREA         REL. ADDR. (ID)         ARRAY DSTORE           70037000
%       DOPART           PARTNUMBER              DO PART                70039000
%        GOTO            STATEMENT #             GO TO                  70041000
%        BRANCHBACK      SYLLABLE COUNT          BRANCH BACKWARD        70043000
%        DECRSTACKANDSKIP                        DECREMENT TOS AND SKIP 70045000
%        INTRINSIC       INTRINSIC #             CALL ON LOCAL PROCEDURE70047000
         NEXTLINE                                GO TO NEXT CARD        70049000
          IFNOTRUSKIP    SYLLABLE COUNT           BRANCH FWD IF FALSE   70051000
%       PRINTSTRING      REL. ADDR. (CODE)       LITERAL TO IOBUF       70053000
%         PRINTEXP                               TOP OF STACK TO IOBUF  70055000
%        STOP                                    STOP EXECUTION         70057000
%         IDACCEPT       REL. ADDR.(ID)          ACCEPT IDENTIFIER VALUE70059000
%         ARRAYACCEPT    REL. ADDR. (ID)         ACCEPT ARRAY VALUE     70061000
%         FORTRANIF      # OF BRANCHES           1,2,3 BRANCH IF        70063000
%         FORM           STATEMENT #             PROCESS FORMAT         70065000
         ;                                                              70067000
SAVEOPTION                                                              70069000
ARRAY LINE[0:3];                                                        70071000
PROCEDURE EXECUTECODE (CODE, IDENT, INFO, USERARRAY,STACK);             70073000
          ARRAY CODE[0], IDENT[0], INFO[0], USERARRAY[0],STACK[0];      70075000
     BEGIN                                                              70077000
          LABEL NEXT;                                                   70079000
          LABEL CHECKSTACK;                                             70081000
          LABEL NEXTONE;                                                70083000
          LABEL CASOP;                                                  70085000
         LABEL BRANCH;                                                  70087000
          LABEL CASENEXTLINE;                                           70089000
               LABEL SLEEP, XIT;                                        70091000
INTEGER OP, MODIFIER;                                                   70093000
INTEGER C,S,L;                                                          70095000
REAL SNUMB;                                                             70097000
INTEGER CYCLE;                                                          70099000
FORMAT F1(F*.*), E1(E*.*), R1(R*.*);                                    70101000
MONITOR EXPOVR,INTOVR,ZERO,INDEX;                                       70103000
LABEL EXPERR,INTERR,ZEROERR,INDEXERR;                                   70105000
          INTEGER LASTI;                                                70107000
DEFINE GOTOSOLVER = SYNTAX#;  COMMENT  USE FIRST 21 WORDS OF SYNTAX     70109000
          ARRAY FOR COMPUTING AND STACKING BRANCH INDECES;              70111000
INTEGER STREAM PROCEDURE MOVELIT(S,D,N32,N1); VALUE N32, N1;            70113000
          BEGIN                                                         70115000
          SI~S;  SI~SI+1;  DI~D;  N32(DI~DI+32);  DI~DI+N1;             70117000
          7(IF SC = """ THEN JUMP OUT; TALLY~TALLY+1;  DS~CHR);         70119000
          MOVELIT~TALLY;                                                70121000
          END MOVELIT;                                                  70123000
STREAM PROCEDURE INSERTARROW(S,D,N32,N1);  VALUE N32, N1;               70125000
          BEGIN LABEL XIT;                                              70127000
          DI~S;  N32(DI~DI+32);  DI~DI+N1;  DS~ LIT "~";  SI~S;  DI~D;  70129000
          2(36(IF SC = "~" THEN JUMP OUT 2 TO XIT;  DS~CHR));           70131000
     XIT: DS~ LIT "~";                                                  70133000
          END INSERTARROW;                                              70135000
STREAM PROCEDURE MOVESTRINGONLY(S,D);                                   70137000
          BEGIN LABEL LOOP;                                             70139000
          SI~S;  63(IF SC!""" THEN SI~SI+1 ELSE JUMP OUT);  SI~SI+1;    70141000
          DI~D;                                                         70143000
     LOOP:IF SC =""" THEN DS~LIT "~" ELSE BEGIN DS~CHR;  GO TO LOOP;    70145000
              END COPY TO QUOTE;                                        70147000
          END MOVE STRING ONLY;                                         70149000
STREAM PROCEDURE MOVECHARS(S,S32,S1,N,D,D32,D1);  VALUE S32,S1,N,D32,D1;70151000
          BEGIN                                                         70153000
          SI~S;  S32(SI~SI+32);  SI~SI+S1;  DI~D;  D32(DI~DI+32);       70155000
          DI~DI+D1;  DS~N CHR;                                          70157000
          END MOVECHARS;                                                70159000
STREAM PROCEDURE MOVEALPHA(D,T,S); VALUE T;                             70159100
     BEGIN DI~D; SI~S; SI~SI+2; DS~T CHR; END;                          70159200
STREAM PROCEDURE MOVEBLANKS(N32,N1,D,D32,D1);                           70159300
     VALUE N32,N1,D32,D1;                                               70159400
BEGIN DI~D; D32(DI~DI+32); DI~DI+D1; N32(DS~32 LIT " ");                70159500
     N1(DS~LIT " ");                                                    70159600
END MOVEBLANKS;                                                         70159700
STREAM PROCEDURE REPEATCHAR(D,D32,D1,N32,N1,C); VALUE D32,D1,N32,N1,C;  70159800
     BEGIN SI~LOC C; SI~SI+2; DI~D; D32(DI~DI+32); DI~DI+D1;            70159900
          N32(32(DS~CHR; SI~SI-1)); N1(DS~CHR; SI~SI-1); END;           70160000
STREAM PROCEDURE MOVEBLANK(D,D32,D1); VALUE D32,D1;                     70161000
          BEGIN DI~D;  D32(DI~DI+32);  DI~DI+D1;  DS~LIT " ";  END;     70163000
STREAM PROCEDURE REQUEST (S,QMARK,D,STACK,N); VALUE QMARK,N;            70165000
BEGIN LOCAL T,T1,T2;                                                    70167000
     SI~S;  SKIP 2 SB;  DI~LOC T;  DI~DI+7;  SKIP 2 DB;  4(IF SB THEN   70169000
          DS~1 SET ELSE DS~1 RESET;  SKIP 1 SB);                        70171000
     SI~LOC T;  SI~SI+7;  SKIP 2 SB;  IF SB THEN                        70173000
     BEGIN                                                              70175000
          DI~D;  DS~15 LIT ".";  SI~S;  SI~SI+1;  DI~D;                 70177000
          DS~7 CHR;  DI~D;  DI~DI+T;                                    70179000
     END ELSE                                                           70181000
     BEGIN                                                              70183000
          DI~D;  SI~S;  SI~SI+1;  DS~T CHR;                             70185000
     END;                                                               70187000
     SI~S; SKIP 1 SB; IF SB THEN                                        70189000
     BEGIN                                                              70191000
          DS~1 LIT "[";  SI~STACK;  T2~SI;  N(T1~DI;  SI~T2;  DI~LOC T; 70193000
          DI~DI+5;  DS~3 DEC;  T2~SI;  SI~LOC T;  SI~SI+5;  DI~T1;      70195000
          IF SC = "0" THEN BEGIN SI~SI+1; IF SC = "0" THEN BEGIN SI~SI+170197000
               ;  DS~1 CHR;  END ELSE DS~2 CHR;  END ELSE DS~3 CHR;     70199000
          DS~1 LIT ",");  DI~DI-1;  DS~1 LIT "]";  END;                 70201000
     DS~1 LIT "=";  SI~LOC QMARK;  SI~SI+7;  DS~1 CHR;  DS~1 LIT "~";   70203000
END REQUEST;                                                            70205000
STREAM PROCEDURE INSERTQMARK(D,Q);  VALUE Q;                            70207000
          BEGIN  DI~D;  SI~LOC Q;  SI~SI+7;  DS~CHR;  DS~LIT"~";  END;  70209000
REAL PROCEDURE RAND(V,INFO); VALUE V; INTEGER V; ARRAY INFO[0];         70211000
BEGIN                                                                   70213000
     DEFINE FORCESEG = #;                                               70214000
     IF V!0 THEN BEGIN                                                  70215000
          INFO[LASTRAND]~T1~V;                                          70217000
          T2~25;                                                        70219000
     END ELSE BEGIN                                                     70221000
          T1~INFO[LASTRAND];                                            70223000
          T2~1;                                                         70225000
     END;                                                               70227000
    FOR T3~1 STEP 1 UNTIL T2 DO T1~REAL(BOOLEAN(274877906943) AND       70229000
          BOOLEAN(T1+0&T1[35:10:13]&T1[10:23:25]));                     70231000
    INFO[LASTRAND]~T1;                                                  70233000
    RAND~T1/274877906943;                                               70235000
          COMMENT RANDOM NUMBER GENERATOR USING THE POWER RESIDUE METHOD70237000
               274877906943 IS 2*38-1. INITIALLY V IS AN ODD INTEGER,   70239000
               AND AFTER THAT IT IS 0. WHENEVER A NEW STARTING VALUE FOR70241000
               V IS ENTERED, 25 RANDOM NUMBERS ARE GENERATED AND        70243000
               DISCARDED;                                               70245000
END RAND;                                                               70247000
INTEGER PROCEDURE GETDIM(WORD,KEY); VALUE WORD,KEY; INTEGER WORD,KEY;   70249000
     IF KEY=0 THEN GETDIM~WORD.[3:8] ELSE                               70251000
     IF KEY=1 THEN GETDIM~WORD.[11:8] ELSE                              70253000
     IF KEY=2 THEN GETDIM~WORD.[19:8] ELSE                              70255000
     GETDIM~WORD.[27:8];                                                70257000
PROCEDURE ERRCARD(INFO,CODE); ARRAY INFO,CODE[0];                       70259000
IF INFO[MODEFLAG]=0 THEN BEGIN                                          70261000
     COMMENT ERROR OCCURRED IN PROGRAM MODE;                            70263000
     RUNNER[USER]~"EL";                                                 70265000
     CODE[CBASE].CLINK~0;                                               70267000
     INFO[CBASEINDX]~CBASE;                                             70269000
     COMMENT SET STOPPER LINKS FOR LISTING OF BAD CARD;                 70271000
END ELSE RUNNER[USER]~"E";                                              70273000
INTEGER PROCEDURE ARRAYLOC(L,IDENT,STACK,S,CODE,INFO);VALUE L;          70275000
REAL S,L; ARRAY IDENT,STACK,CODE,INFO[0];                               70277000
BEGIN                                                                   70279000
     DEFINE FORCESEG = #;                                               70280000
     T~IDENT[L].[38:10];                                                70281000
     T1~IDENT[L].[1:2];                                                 70283000
     FOR T2~T1 STEP -1 UNTIL 0 DO                                       70285000
     BEGIN                                                              70287000
     T3~1;                                                              70289000
          IF STACK[S]>GETDIM(IDENT[L],T2) OR STACK[S] < 1 THEN          70291000
          BEGIN                                                         70293000
               ERRNUMBER~20; COMMENT SUBSCRIPT OUT OF RANGE;            70295000
               ERRCARD(INFO,CODE);                                      70297000
               T~-1;                                                    70299000
          END ELSE                                                      70301000
          BEGIN                                                         70303000
               STACK[S] ~ ENTIER(STACK[S]);                             70303100
               FOR T4~T2+1 STEP 1 UNTIL T1 DO T3~T3|GETDIM(IDENT[L],T4);70305000
               IF T}0 THEN                                              70307000
               T~T+(STACK[(S~S-1)+1]-1)|T3;                             70309000
          END;                                                          70311000
     END;                                                               70313000
     ARRAYLOC~T;                                                        70315000
END ARRAYLOC;                                                           70317000
INTEGER STREAM PROCEDURE COLUMN(S, O32, O1);  VALUE O32, O1;            70319000
          BEGIN  SI~S;  O32(SI~SI+32);  SI~SI+O1;  DI~LOC COLUMN;       70321000
          DI~DI+2; DS~CHR; DS~5 LIT " "; END;                           70323000
INTEGER STREAM PROCEDURE SCANR(S, ITEM);  VALUE ITEM;                   70325000
          BEGIN LOCAL CHAR1, COUNT;  LABEL XIT, ERR;                    70327000
          TALLY~6; SI~LOC ITEM; SI~SI+7; 5(IF SC = " " THEN BEGIN       70329000
            TALLY~TALLY+63; SI~SI-1; END ELSE JUMP OUT);                70331000
          COUNT~TALLY; SI~LOC ITEM; SI~SI+2; CHAR1~SI;                  70333000
          SI~S; TALLY~0;                                                70335000
          2(36(DI~CHAR1;  TALLY~TALLY+1;  IF SC="~" THEN JUMP OUT 2 TO  70337000
               ERR ELSE IF SC=DC THEN BEGIN                             70339000
               SI~SI-1; DI~DI-1;  IF COUNT SC=DC THEN JUMP OUT 2 TO XIT 70341000
               ELSE BEGIN SI~SI-COUNT;  SI~SI+1; END END ONE MATCHED)); 70343000
     ERR: TALLY~0;  COMMENT  0 MEANS  NO MATCH FOUND;                   70345000
     XIT: SCANR~TALLY;                                                  70347000
          END SCANR;                                                    70349000
INTEGER STREAM PROCEDURE FINDFORM(S);                                   70351000
          BEGIN LOCAL T;                                                70353000
          DI~LOC T;  DS~4 LIT "FORM";                                   70355000
          SI~S;                                                         70357000
          56(IF SC=ALPHA THEN IF SC<"0" THEN BEGIN                      70359000
               DI~LOC T;  IF 4 SC=DC THEN TALLY~TALLY+4 ELSE TALLY~56;  70361000
               JUMP OUT;  END FOUND THE FIRST LETTER;                   70363000
          SI~SI+1;  TALLY~TALLY+1);                                     70365000
          IF SC="A" THEN BEGIN  TALLY~TALLY+2;  SI~SI+2; END FORMAT;    70367000
          IF SC=" " THEN TALLY~TALLY+1;  COMMENT  SKIP A BLANK SEP.  ;  70369000
          FINDFORM~TALLY;   COMMENT  VALUE > 55 MEANS  FORM NOT FOUND;  70371000
          END FINDFORM;                                                 70373000
DEFINE REALCODE = "&"#;                                                 70375000
DEFINE SCIENTIFIC = "@"#;                                               70377000
DEFINE ALPHACODE = """#;                                                70377100
STREAM PROCEDURE FORMCRACK(S, S1, S32, FCN, SZ1, SZ2);                  70379000
          VALUE S1, S32;                                                70381000
          BEGIN LOCAL T,T1;                                             70383000
          COMMENT  FORMCRACK RETURNS A FUNCTION CODE AND ONE OR TWO     70385000
               FIELD SIZES.   IT BEGIN SCANNING BUFFER S AT S1+32|S32,  70387000
               THE SOURCE POINTER SHOULD BE STEPPED BY SZ1+SZ2 BEFORE   70389000
               THEN NEXT CALL.;                                         70391000
          LABEL SCI,REALPRE,REALSUF,ERROR,XIT;                          70393000
          LABEL AL1;                                                    70393100
          DI~S;  DI~DI+32;  DI~DI+40;  DS~LIT"~";                       70395000
          SI~S;  SI~SI+S1;  S32(SI~SI+32);                              70397000
          DI~SZ1;  DS~8 LIT"0";  DI~SZ2;  DS~8 LIT "0";                 70399000
          IF SC="~" THEN BEGIN  DI~FCN;  DS~8 LIT "0000000~";           70401000
               GO TO XIT;  END RETURN ENDFORM STATUS;                   70403000
          IF SC = SCIENTIFIC THEN BEGIN DI~FCN;  DS~8 LIT "0000000S";   70405000
               20(TALLY~TALLY+1;  SI~SI+1;  IF SC!SCIENTIFIC THEN       70407000
                    JUMP OUT 1 TO SCI);   GO TO ERROR;                  70409000
               SCI: T~TALLY;  SI~LOC T;  DI~SZ1;  DS~WDS;  GO TO XIT;   70411000
               END HANDLING SCIENTIFIC FIELD;                           70413000
          IF SC = ALPHACODE THEN BEGIN DI~FCN; DS~8 LIT "0000000L";     70413100
               6(TALLY~TALLY+1; SI~SI+1; IF SC ! ALPHACODE THEN         70413200
               JUMP OUT 1 TO AL1); GO TO ERROR;                         70413300
          AL1: T~TALLY; SI~LOC T; DI~SZ1; DS~WDS; GO TO XIT;            70413400
               END HANDLING ALPHA FIELD;                                70413500
          IF SC=REALCODE THEN BEGIN  DI~FCN;  DS~8 LIT "0000000F";      70415000
               15(TALLY~TALLY+1;  SI~SI+1;  IF SC ! REALCODE THEN JUMP  70417000
                    OUT 1 TO REALPRE);  GO TO ERROR;                    70419000
               REALPRE:  T1~SI;  T~TALLY;  DI~SZ1;  SI~ LOC T;  DS~ WDS;70421000
                    SI~T1;  IF SC="." THEN BEGIN                        70423000
                    TALLY~0;                                            70425000
                    11(SI~SI+1;  TALLY~TALLY+1;  IF SC ! REALCODE THEN  70427000
                         JUMP OUT 1 TO REALSUF);  GO TO ERROR;          70429000
                    REALSUF:  T~TALLY;  DI~SZ2;  SI~LOC T;  DS~ WDS;    70431000
                    END REAL SUFFIX;                                    70433000
               GO TO XIT;                                               70435000
               END REAL;                                                70437000
          TALLY~0;  DI~FCN;  DS~8 LIT "0000000A";  COMMENT  ALPHA DATA; 70439000
          63(TALLY~TALLY+1;  SI~SI+1;  IF SC="~" THEN JUMP OUT;         70441000
               IF SC = SCIENTIFIC THEN JUMP OUT;                        70443000
               IF SC = ALPHACODE THEN JUMP OUT;                         70443100
               IF SC = REALCODE THEN JUMP OUT);                         70445000
          T~TALLY;  DI~SZ1;  SI~LOC T;  DS~1 WDS;                       70447000
          GO TO XIT;                                                    70449000
ERROR:    DI~FCN;  DS~8 LIT "0000000E";  COMMENT  RETURN ERROR CODE;    70451000
XIT:      END FORMCRACK;                                                70453000
%-------------------------- G E T N E X T D A T A ----------------------70455000
REAL PROCEDURE GETNEXTDATA(V,INFO,CODE); REAL V;                        70457000
     ARRAY INFO,CODE[0];                                                70459000
BEGIN                                                                   70461000
INTEGER STREAM PROCEDURE FINDDATA(S);                                   70463000
BEGIN                                                                   70465000
     SI~S;                                                              70467000
     63(IF SC = ALPHA THEN IF SC < "0" THEN JUMP OUT; TALLY~TALLY+1;    70469000
          SI~SI+1);                                                     70471000
     TALLY~TALLY+4;                                                     70473000
     FINDDATA~TALLY;                                                    70475000
END FINDDATA;                                                           70477000
INTEGER STREAM PROCEDURE GETDATA(S,N32,N1,D,P); VALUE N1,N32;           70479000
BEGIN                                                                   70481000
LABEL XIT,DEBLANK,DEB,STRING,LOOP,NR ,EXPO,EXP,E1,E2,ENDSTR,S1;         70483000
LOCAL T,T1,T2,SI1,DI1;                                                  70485000
     DI~P; DS~8 LIT "0";                                                70487000
     SI~S; N32(SI~SI+32); SI~SI+N1;                                     70489000
     DI~D; DS~24 LIT "0";                                               70491000
     IF SC = "~" THEN GO TO XIT;                                        70493000
DEBLANK:                                                                70495000
     IF SC = " " THEN BEGIN DEB: ADD1TOP; SI~SI+1; GO TO DEBLANK END;   70497000
     IF SC = "," THEN GO TO DEB;                                        70499000
     IF SC = "~" THEN GO TO XIT;                                        70501000
     IF SC = "+" THEN GO TO DEB;                                        70503000
     IF SC = "-" THEN                                                   70505000
     BEGIN                                                              70507000
          DI~LOC GETDATA; DS~LIT "+";                                   70509000
          COMMENT SET RETURNED VALUE NEGATIVE;                          70511000
          GO TO DEB;                                                    70513000
     END;                                                               70515000
     IF SC = """ THEN GO TO STRING;                                     70517000
     COMMENT NUMBER;                                                    70519000
     T2~SI; SI~SI+1; TALLY~1; ADD1TOP;                                  70521000
LOOP:                                                                   70523000
     IF SC = "." THEN GO TO NR;                                         70525000
     IF SC } "0" THEN                                                   70527000
     BEGIN                                                              70529000
     NR:  SI~SI+1; ADD1TOP; TALLY~TALLY+1; GO TO LOOP;                  70531000
     END ELSE                                                           70533000
     BEGIN                                                              70535000
          T1~TALLY; SI~LOC T1; SI~SI+7; DI~D; DS~CHR; SI~T2;            70537000
          DS~T1 CHR; T2~SI; GO TO EXPO;                                 70539000
     END;                                                               70541000
EXPO:IF SC = " " THEN BEGIN SI~SI+1; ADD1TOP; GO TO EXPO; END;          70543000
     IF SC = "@" THEN GO TO EXP;                                        70545000
     IF SC = "E" THEN                                                   70547000
     BEGIN                                                              70549000
     EXP: SI~SI+1; ADD1TOP;                                             70551000
     E1:  IF SC = " " THEN BEGIN SI~SI+1; ADD1TOP; GO TO E1; END;       70553000
          IF SC = "+" THEN GO TO E2;                                    70555000
          IF SC = "-" THEN GO TO E2;                                    70557000
          IF SC } "0" THEN                                              70559000
          BEGIN                                                         70561000
          E2:  DI~D; DI~DI+16; DS~LIT "E"; DS~CHR;                      70563000
               3(IF SC } "0" THEN BEGIN DS~CHR; ADD1TOP;END);DS~LIT ".";70565000
          END;                                                          70567000
     ADD1TOP;                                                           70569000
     END;                                                               70571000
     DI~LOC GETDATA; DI~DI+7; DS~LIT "2";                               70573000
     GO TO XIT;                                                         70575000
STRING:                                                                 70577000
     SI~SI+1; ADD1TOP; TALLY~0; T1~SI;                                  70579000
     6(IF SC = """ THEN JUMP OUT TO ENDSTR ELSE BEGIN SI~SI+1;          70581000
          TALLY~TALLY+1; ADD1TOP; END);                                 70583000
S1:  IF SC ! """ THEN BEGIN SI~SI+1; ADD1TOP; GO TO S1; END;            70585000
ENDSTR:                                                                 70587000
     SI~SI+1; ADD1TOP;                                                  70589000
     DI~D; DI~DI+2; DS~6 LIT " "; DI~DI-6; T~TALLY;                     70591000
     SI~T1; DS~T CHR;                                                   70591100
     DI~LOC GETDATA; DI~DI+7; DS~LIT "1";                               70593000
XIT:                                                                    70595000
END GETDATA;                                                            70597000
INTEGER STREAM PROCEDURE EXPONENT(E); VALUE E;                          70599000
BEGIN LOCAL T;                                                          70601000
     SI~LOC E; IF SC = "E" THEN                                         70603000
     BEGIN                                                              70605000
          TALLY~0; SI~SI+1; IF SC < "0" THEN SI~SI+1;                   70607000
          3(IF SC = "." THEN JUMP OUT ELSE TALLY~TALLY+1; SI~SI+1);     70609000
          T~TALLY; SI~SI-T; DI~LOC EXPONENT; DS~T OCT;                  70611000
          DI~LOC EXPONENT; SKIP 1 DB; SI~LOC E; SI~SI+1;                70613000
          IF SC = "-" THEN DS~1 SET;                                    70615000
     END EXPONENT PART PRESENT;                                         70617000
END EXPONENT;                                                           70619000
INTEGER STREAM PROCEDURE NCR(S,N); VALUE N;                             70621000
BEGIN SI~S; SI~SI+N; DI~LOC NCR; DI~DI+7; DS~CHR; END;                  70623000
REAL PROCEDURE CONVERTCON(INFO); ARRAY INFO[0];                         70625000
BEGIN                                                                   70627000
     DEFINE FORCESEG = #;                                               70628000
     INTG~FRACTION~TENS~0;                                              70629000
     PFLAG~FALSE;                                                       70631000
     FOR I~1 STEP 1 UNTIL NCR(INFO[ALTBUF],0) DO                        70633000
          IF NC~NCR(INFO[ALTBUF],I) = "." THEN                          70635000
          BEGIN                                                         70637000
               PFLAG~TRUE;                                              70639000
               INTG~FRACTION;                                           70641000
               FRACTION~TENS~0;                                         70643000
          END ELSE                                                      70645000
          BEGIN                                                         70647000
               TENS~TENS+1;                                             70649000
               FRACTION~FRACTION|10+NC;                                 70651000
          END NEXT CHARACTER;                                           70653000
     IF PFLAG THEN CONVERTCON~(INTG+FRACTION/10*TENS)|10*               70655000
          EXPONENT(INFO[ALTBUF+2]) ELSE                                 70657000
          CONVERTCON~FRACTION|10*EXPONENT(INFO[ALTBUF+2]);              70659000
END CONVERTCON;                                                         70661000
%                                                                       70663000
LABEL XIT,AGAIN;                                                        70665000
BOOLEAN SLICE;                                                          70667000
     SLICE ~ FALSE;                                                     70669000
     IF INFO[DATASTART] = 0 THEN                                        70671000
     BEGIN                                                              70673000
          GETNEXTDATA~0;                                                70675000
          GO TO XIT;                                                    70677000
     END;                                                               70679000
     IF T1~INFO[DATAPOINTER] = 0 THEN T1~INFO[DATAPOINTER]~             70681000
          INFO[DATASTART];                                              70683000
AGAIN:                                                                  70685000
     T2~CODE[T1].CDISKADDR;                                             70687000
     IF DISKPOINTER DIV 3 ! T2 DIV 3 THEN                               70689000
     BEGIN                                                              70691000
          IF (T1 ~ T2 DIV 3) ! LISTPOINTER THEN                         70693000
          BEGIN                                                         70695000
               READ(DSK[LISTPOINTER ~ T1],30,LISTBUF[*]);               70697000
               SLICE ~ TRUE;                                            70699000
          END;                                                          70701000
          IF INFO[DATAINDEX] = 0 THEN INFO[DATAINDEX] ~ FINDDATA(       70703000
               LISTBUF[   T2 MOD 3|10]);                                70705000
          T~GETDATA(LISTBUF[T2 MOD 3|10],(T1~INFO[DATAINDEX]) DIV 32,   70707000
               ENTIER(T1 MOD 32),INFO[ALTBUF],T3);                      70709000
     END ELSE                                                           70711000
     BEGIN                                                              70713000
          IF INFO[DATAINDEX] = 0 THEN INFO[DATAINDEX] ~ FINDDATA(       70715000
               DISKBUF[   T2 MOD 3|10]);                                70717000
          T~GETDATA(DISKBUF[T2 MOD 3|10],(T1~INFO[DATAINDEX]) DIV 32,   70719000
               ENTIER(T1 MOD 32),INFO[ALTBUF],T3);                      70721000
     END;                                                               70723000
     INFO[DATAINDEX]~INFO[DATAINDEX]+T3;                                70725000
     IF T2~ABS(T) = 0 THEN                                              70727000
     BEGIN                                                              70729000
     COMMENT DATA STATEMENT EXHAUSTED;                                  70731000
          IF T1~CODE[INFO[DATAPOINTER]+1].[38:10] = 0 THEN              70733000
          BEGIN                                                         70735000
               GETNEXTDATA~0;                                           70737000
               GO TO XIT;                                               70739000
          END;                                                          70741000
          INFO[DATAPOINTER] ~ T1;                                       70743000
          INFO[DATAINDEX] ~ 0;                                          70745000
          GO TO AGAIN;                                                  70747000
     END;                                                               70749000
     GETNEXTDATA ~ IF SLICE THEN -1 ELSE 1;                             70751000
          COMMENT SET FOR TIME SLICE ON READ;                           70753000
     IF T2 = 1 THEN                                                     70755000
     BEGIN COMMENT STRING;                                              70757000
          V~INFO[ALTBUF]|SIGN(T);                                       70759000
          GO TO XIT;                                                    70761000
     END;                                                               70763000
     V~CONVERTCON(INFO)|SIGN(T);                                        70765000
XIT:                                                                    70767000
END GETNEXTDATA;                                                        70769000
PROCEDURE MOVEFIELD(INFO,CODE,XIT);  LABEL XIT;  ARRAY INFO, CODE[0];   70771000
          DO BEGIN                                                      70773000
     DEFINE FORCESEG = #;                                               70774000
          COMMENT  THIS PROCEDURE MOVES THE NEXT ALPHA FIELD FROM A FORM70775000
               STATEMENT INTO THE OUTPUT BUFFER;                        70777000
          FORMCRACK(LISTBUF[T~INFO[FORMPOINTER] MOD 3|10],              70779000
               ENTIER((T1~INFO[FORMINDEX]) MOD 32),T1 DIV 32,T2,T3,T4); 70781000
          IF T2="E" THEN                                                70783000
               BEGIN                                                    70785000
               ERRNUMBER~40;  COMMENT  FIELD TOO LARGE;                 70787000
               ERRCARD(INFO,CODE);                                      70789000
               GO TO XIT;                                               70791000
               END;                                                     70793000
          IF T2="A" THEN                                                70795000
               BEGIN                                                    70797000
               IF INFO[BUFINDEX]+T3 < 73 THEN                           70799000
               MOVECHARS(LISTBUF[T],T1 DIV 32,ENTIER(T1 MOD 32),T3,     70801000
                    INFO[ALTBUF],(T2~INFO[BUFINDEX]) DIV 32, ENTIER(    70803000
                    T2 MOD 32));                                        70805000
               INFO[BUFINDEX]~T2+T3;                                    70807000
               INFO[FORMINDEX]~T1+T3;                                   70809000
               END;                                                     70811000
          END UNTIL T3<63;                                              70813000
STREAM PROCEDURE STOPMSG(P,S,D);  VALUE P,S;                            70815000
BEGIN LOCAL T,T1;                                                       70817000
     DI~D;  SI~LOC P;  DS~18 LIT "STOP AT STATEMENT ";  T1~DI;          70819000
     DI~LOC T;  DS~2DEC;  SI~LOC T;  DI~T1;  IF SC="0" THEN BEGIN       70821000
        SI~SI+1;  DS~CHR;  END ELSE DS~2 CHR;  DS~LIT ".";  T1~DI;      70823000
     DI~LOC T;  SI~LOC S;  DS~3 DEC;  SI~LOC T;  DI~T1;  DS~CHR;        70825000
     IF SC!"0" THEN BEGIN DS~CHR;  IF SC!"0" THEN DS~CHR;  END          70827000
     ELSE BEGIN SI~SI+1;  IF SC!"0" THEN BEGIN SI~SI-1;  DS~2 CHR;  END;70829000
     END;  DS~LIT "~";                                                  70831000
END STOPMSG;                                                            70833000
STREAM PROCEDURE OUTOFDATAMSG(P,S,D);                                   70835000
     VALUE P,S;                                                         70837000
BEGIN LOCAL T,T1;                                                       70839000
     DI~D; SI~LOC P; DS~23 LIT "OUT OF DATA, STATEMENT "; T1~DI;        70841000
     DI~LOC T; DS~2 DEC; SI~LOC T; DI~T1;                               70843000
     IF SC = "0" THEN                                                   70845000
     BEGIN                                                              70847000
          SI~SI+1; DS~CHR;                                              70849000
     END ELSE DS~2 CHR;                                                 70851000
     DS~LIT "."; T1~DI; DI~LOC T; SI~LOC S; DS~3 DEC; SI~LOC T;         70853000
     DI~T1; DS~CHR;                                                     70855000
     IF SC ! "0" THEN                                                   70857000
     BEGIN                                                              70859000
          DS~CHR; IF SC ! "0" THEN DS~CHR;                              70861000
     END ELSE                                                           70863000
     BEGIN                                                              70865000
          SI~SI+1; IF SC ! "0" THEN                                     70867000
          BEGIN                                                         70869000
               SI~SI-1; DS~2 CHR;                                       70871000
          END;                                                          70873000
     END;                                                               70875000
     DS~LIT "~";                                                        70877000
END OUTOFDATAMSG;                                                       70879000
          STREAM PROCEDURE NXT (C,L,OP,M); VALUE L;                     70881000
          BEGIN                                                         70883000
          SI~C;SI~SI+L; DI~OP; DI~DI+7;                                 70885000
          DS ~ 1 CHR;  DI ~ M;  DI ~ DI + 7;  DS ~ 1 CHR; END NXT;      70887000
DEFINE READNEXTINSTRUCTION =                                            70889000
          NXT(CODE[C], L+L, OP, MODIFIER);                              70891000
          IF L ~ L + 1 = 4 THEN BEGIN L ~ 0; C ~ C + 1; END STEPPING C; 70893000
          #;                                                            70895000
% ----------------------------------------------------------------      70897000
          INTOVR~INTERR;                                                70899000
          EXPOVR~EXPERR;                                                70901000
          ZERO~ZEROERR;                                                 70903000
          INDEX~INDEXERR;                                               70905000
          CBASE ~ INFO[CBASEINDX];                                      70907000
          C ~ INFO[CINDX];                                              70909000
          L ~ INFO[LINDX];                                              70911000
          S ~ INFO[SINDX];  COMMENT  SET UP REGISTERS FOR EXECUTE;      70913000
          CYCLE ~ 100/ (MAXUSER+1);   COMMENT  SET TIME SLICE COUNTER;  70915000
          LASTI ~ 0;                                                    70917000
NEXT:     IF (INFO[RUNCOUNT]~INFO[RUNCOUNT]+1)>INFO[RUNLIMIT] THEN BEGIN70919000
               RUNNER[USER]~"E"; INFO[MODE]~-1; ERRNUMBER~21;           70921000
                    COMMENT TYPE "DO YOU WISH TO CONTINUE...";          70923000
               GO TO SLEEP; END TOO MUCH EXECUTE;IF CYCLE~CYCLE-1=0     70925000
               THEN BEGIN RUNNER[USER]~"S";GO TO SLEEP;END FORCED       70927000
               TIME SLICING;                                            70929000
          IF CODE[CBASE].CSIZE = 1 THEN GO TO CASENEXTLINE;             70931000
NEXTONE:  READNEXTINSTRUCTION;                                          70933000
          IF DEBUG THEN WRITE (PR,XFMT, C, L, OP, MODIFIER, S);         70935000
CASOP:    CASE OP OF BEGIN  COMMENT  SWITCH ON OP CODE  ----------;     70937000
               BEGIN RUNNER[USER]~"E"; GO TO XIT; END OP CODE 0;        70939000
%LITERAL                                                                70941000
               STACK[S ~ S + 1] ~ MODIFIER;  COMMENT  LITERAL;          70943000
%IDFETCH                                                                70945000
               BEGIN T ~ STACK[S~S+1] ~ IDENT[MODIFIER + MODIFIER+ 1];  70947000
                    IF T  = UNDEFINED THEN BEGIN INFO[MODE] ~           70949000
                              T1 ~ MODIFIER+ MODIFIER+ 1;               70951000
                              IF BOOLEAN(INFO[ACCEPTFLAG])THEN S~S-1;   70953000
                              REQUEST(IDENT[T1-1],12,INFO[IOBUF],INFO[  70955000
                              IOBUF],0);                                70957000
                              RUNNER[USER] ~"E";                        70959000
                              GO TO SLEEP;                              70961000
                              END;  END HANDLING AN UNDEFINED IDENT     70963000
                              ON AN IDENTIFIER FETCH;                   70965000
%CONSTANT                                                               70967000
               STACK[S~S+1] ~ CODE[CBASE+MODIFIER];  COMMENT  CONSTANT; 70969000
%ARRAYFETCH                                                             70971000
               BEGIN IF T1~ARRAYLOC(T5~MODIFIER+MODIFIER+1,IDENT,STACK, 70973000
                    S,CODE,INFO)<0 THEN %INVALID SUBSCRIPT              70975000
                    GO TO XIT; IF T~USERARRAY[T1]=                      70977000
                    UNDEFINED THEN BEGIN                                70979000
                         INFO[MODE]~T1+1;                               70981000
                         REQUEST(IDENT[T5-1],12,INFO[IOBUF],STACK[S+1], 70983000
                              IDENT[T5].[1:2]+1);                       70985000
                         INFO[ARRAYFLAG]~1;                             70987000
                         RUNNER[USER]~"E";                              70989000
                         STACK[(S~S+1)]~UNDEFINED;                      70991000
                         GO TO SLEEP;                                   70993000
                    END HANDLING AN UNDEFINED ARRAY ELEMENT ELSE        70995000
                    STACK[S~S+1]~T; COMMENT ARRAY FETCH; END;           70997000
%ARITHOP                                                                70999000
               CASE MODIFIER OF BEGIN    COMMENT  ARITH OPERATION;      71001000
                    STACK[S~S-1]~STACK[S]+STACK[S+1]; COMMENT  ADD;     71003000
                    STACK[S~S-1]~STACK[S]-STACK[S+1]; COMMENT  SUB;     71005000
                    STACK[S~S-1]~STACK[S]|STACK[S+1];  COMMENT MPY;     71007000
                    STACK[S~S-1]~STACK[S]/STACK[S+1]; COMMENT RDV;      71009000
                    BEGIN COMMENT A**B;                                 71011000
                         IF STACK[S-1]<0 AND STACK[S] MOD 1 !0 THEN     71013000
                         BEGIN ERRNUMBER~22; ERRCARD(INFO,CODE);        71015000
                              GO TO XIT; END NEGATIVE A ELSE            71017000
                         IF STACK[S-1]=0 THEN STACK[S~S-1]~0 ELSE       71019000
                         IF ABS(STACK[S]|LN(ABS(STACK[S-1])))>158 THEN  71021000
                         BEGIN ERRNUMBER~23; ERRCARD(INFO,CODE);        71023000
                              COMMENT A**B OUT OF RANGE;                71025000
                              GO TO XIT; END ELSE                       71027000
                         STACK[S~S-1]~STACK[S]*STACK[S+1];              71029000
                    END;                                                71031000
                    STACK[S~S-1]~STACK[S] DIV STACK[S+1];               71033000
                    STACK[S~S-1]~STACK[S] MOD STACK[S+1];               71035000
                    STACK[S] ~ -STACK[S];  COMMENT CHANGESIGN;          71037000
                    END CASE ARITHOP;                                   71039000
%RELTEST                                                                71041000
               CASE MODIFIER OF BEGIN   COMMENT  RELATIONAL OP;         71043000
                    STACK[S~S-1] ~ REAL(STACK[S] = STACK[S+1]);         71045000
                    STACK[S~S-1] ~ REAL(STACK[S] ! STACK[S+1]);         71047000
                    STACK[S~S-1] ~ REAL(STACK[S] > STACK[S+1]);         71049000
                    STACK[S~S-1] ~ REAL(STACK[S] < STACK[S+1]);         71051000
                    STACK[S~S-1] ~ REAL(STACK[S] } STACK[S+1]);         71053000
                    STACK[S~S-1] ~ REAL(STACK[S] { STACK[S+1]);         71055000
                    END CASE  OF RELATIONAL OPERATOR;                   71057000
%FUNCTCALL                                                              71059000
               CASE MODIFIER OF BEGIN  COMMENT   FUNCTIONS;             71061000
                    STACK[S] ~ ENTIER   (STACK[S]);                     71063000
                    BEGIN COMMENT LN;                                   71065000
                         IF STACK[S]{0 THEN                             71067000
                         BEGIN ERRNUMBER~24; ERRCARD(INFO,CODE);        71069000
                              GO TO XIT; END NEGATIVE ARGUMENT ELSE     71071000
                         STACK[S]~LN(STACK[S]);                         71073000
                    END;                                                71075000
                    STACK[S] ~ SIN      (STACK[S]);                     71077000
                    STACK[S] ~ COS      (STACK[S]);                     71079000
                    STACK[S] ~ SIN      (STACK[S])/COS (STACK[S]);      71081000
                    STACK[S] ~ ABS      (STACK[S]);                     71083000
                    BEGIN COMMENT EXP;                                  71085000
                         IF ABS(STACK[S])>158 THEN                      71087000
                         BEGIN ERRNUMBER~25; ERRCARD(INFO,CODE);        71089000
                              GO TO XIT; END EXP OUT OF RANGE ELSE      71091000
                         STACK[S]~EXP(STACK[S]);                        71093000
                    END;                                                71095000
                    STACK[S] ~ RAND(STACK[S],INFO[*]);                  71097000
                    STACK[S] ~ ARCTAN   (STACK[S]);                     71099000
                    BEGIN COMMENT SQRT;                                 71101000
                         IF STACK[S]<0 THEN                             71103000
                         BEGIN ERRNUMBER~26; ERRCARD(INFO,CODE);        71105000
                              GO TO XIT; END NEGATIVE ARGUMENT ELSE     71107000
                         STACK[S]~SQRT(STACK[S]);                       71109000
                    END;                                                71111000
                    STACK[S] ~ SIGN     (STACK[S]);                     71113000
                    BEGIN COMMENT LOG10;                                71115000
                         IF STACK[S]{0 THEN                             71117000
                         BEGIN ERRNUMBER~24; ERRCARD(INFO,CODE);        71119000
                              GO TO XIT; END NEGATIVE ARGUMENT ELSE     71121000
                         STACK[S]~LN(STACK[S])/2.30258509298;           71123000
                    END;                                                71125000
                    BEGIN COMMENT  NULL FUNCTION (12);  END;            71127000
                    IF STACK[S]>0 THEN                                  71129000
                    STACK[S]~COLUMN(INFO[ALTBUF],(T1~ENTIER(STACK[S]-1))71131000
                         DIV 32, ENTIER(T1 MOD 32));                    71133000
                    STACK[S]~SCANR(INFO[ALTBUF],ENTIER(STACK[S]));      71135000
                    BEGIN COMMENT (15); END;                            71135020
                    BEGIN COMMENT (16); END;                            71135040
                    BEGIN COMMENT (17); END;                            71135060
                    BEGIN COMMENT (18) PLOT FUNCTION;                   71135080
                         IF T1~ENTIER(STACK[(S~S-1)+1])}1 AND T1{72 THEN71135100
                         BEGIN                                          71135120
                              IF T1>(T2~INFO[BUFINDEX]) THEN            71135140
                              BEGIN                                     71135160
                                   MOVEBLANKS((T3~(T1-T2)) DIV 32,      71135180
                                   ENTIER(T3 MOD 32),INFO[ALTBUF],T2 DIV71135200
                                   32,ENTIER(T2 MOD 32));               71135220
                                   INFO[BUFINDEX] ~ T1;                 71135240
                              END;                                      71135260
                              MOVECHARS(STACK[(S~S-1)+1],0,2,1,INFO[    71135280
                                   ALTBUF],(T1-1) DIV 32,ENTIER((T1-1)  71135300
                                   MOD 32));                            71135320
                         END MOVING PLOT CHAR INTO BUFFER ELSE          71135340
                         BEGIN                                          71135360
                              ERRNUMBER ~ 37; ERRCARD(INFO,CODE);       71135380
                              GO TO XIT;                                71135400
                         END ARGUMENT OF PLOT NOT IN RANGE 1 TO 72;     71135420
                    END PLOT FUNCTION;                                  71135440
                    BEGIN COMMENT (19) TAB FUNCTION;                    71135460
                         IF T1~ENTIER(STACK[(S~S-1)+1])}1 AND T1{72 THEN71135480
                         IF T1~T1-1 > (T2~INFO[BUFINDEX]) THEN          71135500
                         BEGIN                                          71135520
                              MOVEBLANKS((T3~(T1-T2)) DIV 32,ENTIER(T3  71135540
                                   MOD 32),INFO[ALTBUF],T2 DIV 32,      71135560
                                   ENTIER(T2 MOD 32));                  71135580
                              INFO[BUFINDEX] ~ T1;                      71135600
                         END ELSE ELSE                                  71135620
                         BEGIN                                          71135640
                              ERRNUMBER~49; ERRCARD(INFO,CODE);         71135660
                              GO TO XIT;                                71135680
                         END ARGUMENT OF TAB NOT IN RANGE 1 TO 72;      71135700
                    END TAB FUNCTION;                                   71135720
                    BEGIN COMMENT (20) REPEAT FUNCTION;                 71135740
                         T2 ~ INFO[BUFINDEX];                           71135760
                         T1 ~ MIN(72-T2,STACK[(S~S-1)+1]);              71135780
                         IF 0 > T1 THEN T1 ~ 0;                         71135800
                         REPEATCHAR(INFO[ALTBUF],T2 DIV 32,ENTIER(T2 MOD71135820
                              32),T1 DIV 32, ENTIER(T1 MOD 32),STACK[(S~71135840
                              S-1)+1]);                                 71135860
                         INFO[BUFINDEX] ~ T2+T1;                        71135880
                    END REPEAT FUNCTION;                                71135900
                    END CASE ARITH FUNCTION CALL;                       71137000
%NDSTORE                                                                71139000
               IDENT[MODIFIER + MODIFIER + 1] ~ STACK[S]; COMMENT NON-  71141000
                              DESTRUCTIVE STORE;                        71143000
%DSTORE                                                                 71145000
               IDENT[MODIFIER + MODIFIER + 1] ~ STACK[(S~S-1)+1];       71147000
                                   COMMENT   DESTRUCTIVE STORE;         71149000
%NDSTOREA                                                               71151000
               BEGIN IF T1~ARRAYLOC(MODIFIER+MODIFIER+1,IDENT,STACK,    71153000
                    S,CODE,INFO)<0 THEN GO TO XIT;%INVALID SUBSCRIPT    71155000
                    USERARRAY[T1]~STACK[S];END;                         71157000
                         COMMENT ARRAY NON DESTRUCTIVE STORE;           71159000
%DSTOREA                                                                71161000
               BEGIN IF T1~ARRAYLOC(MODIFIER+MODIFIER+1,IDENT,STACK,    71163000
                    S,CODE,INFO)<0 THEN GO TO XIT;% INVALID SUBSCRIPT   71165000
                    USERARRAY[T1]~STACK[(S~S-1)+1];END;                 71167000
                         COMMENT ARRAY DESTRUCTIVE STORE;               71169000
%DOPART                                                                 71171000
               BEGIN                                                    71173000
                    SNUMB~MODIFIER;                                     71175000
                    READNEXTINSTRUCTION;                                71177000
                    SNUMB~0&OP[20:42:2]&SNUMB[15:43:5];                 71179000
                    STACK[S+1]~0&63[3:42:6]&SNUMB[15:15:7]&CBASE[26:38: 71181000
                         10]&C[36:38:10]&L[46:46:2];                    71183000
                    COMMENT PUT RETURN CONTROL WORD IN STACK;           71185000
                    C~CODE[0].CLINK;                                    71187000
                    COMMENT LOOK FOR 1ST STATEMENT IN PART;             71189000
                    DO BEGIN                                            71191000
                         T~CODE[C];                                     71193000
                         IF REAL(BOOLEAN(T) AND BOOLEAN(PTMASK))=SNUMB  71195000
                         THEN BEGIN                                     71197000
                              S~S+1;                                    71199000
                              CBASE~C;                                  71201000
                              C~C+1;                                    71203000
                              L~0;                                      71205000
                              GO TO NEXT;                               71207000
                         END ELSE C~T.CLINK;                            71209000
                    END UNTIL T.CLINK=0;                                71211000
                    C~1;                                                71213000
                    WHILE C<INFO[CMAX] DO BEGIN T~CODE[C];              71215000
                         IF T.SBIT!1 THEN IF REAL(BOOLEAN(T) AND        71217000
                         BOOLEAN(PTMASK))=SNUMB THEN BEGIN ERRNUMBER~27;71219000
                         ERRCARD(INFO,CODE); GO TO XIT; END             71221000
                         PART PRESENT BUT NOT IN RANGE;                 71223000
                    C~T.CSIZE+C;                                        71225000
                    END ;                                               71227000
                    ERRNUMBER~28; COMMENT PART NOT PRESENT;             71229000
                    ERRCARD(INFO,CODE);                                 71231000
                    GO TO XIT;                                          71233000
               END DOPART;                                              71235000
%GO TO                                                                  71237000
               GO TO BRANCH;    COMMENT  UNCONDITIONAL GO TO;           71239000
%BRANCHBACK                                                             71241000
               BEGIN L~L-1-MODIFIER; WHILE L<0 DO BEGIN                 71243000
                    L~L+4;C~C-1;END;END BRANCH BACKWARDS;               71245000
%DECRSTACKANDSKIP                                                       71247000
               IF (STACK[S]~STACK[S]-1)}0 THEN BEGIN                    71249000
                    READNEXTINSTRUCTION; GO TO NEXT; END ELSE           71251000
                    BEGIN S~S-1; GO TO NEXT; END;%END DECREMENT TOP OF  71253000
                   %STACK AND SKIP IF NOT NEGATIVE;                     71255000
%INTRINSIC                                                              71257000
               CASE MODIFIER OF BEGIN                                   71259000
                    BEGIN INSERTARROW(INFO[ALTBUF], INFO[IOBUF],        71261000
                         (T1~INFO[BUFINDEX]) DIV 32,ENTIER(T1 MOD 32)); 71263000
                         COMMENT PUT ARROW AT END OF PRINT BUFFER;      71265000
                         INFO[BUFINDEX]~0; RUNNER[USER]~"P";            71267000
                         INFO[FORMFLAG]~0;                              71269000
                         INFO[RUNCOUNT] ~ 0;                            71269100
                         GO TO SLEEP; END PRINT INTRINSIC;              71271000
                    STACK[S]~ENTIER(STACK[S]); COMMENT TOS CONTAINS     71273000
                         THE NUMBER OF ITERATIONS ON A TIMES CLAUSE IN  71275000
                         A DO STATEMENT.  THIS INTRINSIC CONVERTS IT TO 71277000
                         AN INTEGER TO FORCE THE [3:6] FIELD TO 0;      71279000
                    BEGIN COMMENT  (2)  READ ALPHA CONSTRUCT;           71281000
                         INFO[ALPHACCEPT]~1;                            71283000
                         INSERTQMARK(INFO[IOBUF],12);                   71285000
                         RUNNER[USER]~"E";                              71287000
                         GO TO SLEEP;                                   71289000
                         END ALPHAREAD;                                 71291000
                    BEGIN   COMMENT  (3)     TYPE STRING ONLY;          71293000
                         IF T1~(T~CODE[CBASE].CDISKADDR) DIV 3 =        71295000
                              DISKPOINTER DIV 3 THEN MOVESTRINGONLY(    71297000
                              DISKBUF[T MOD 3 | 10], INFO[IOBUF])       71299000
                         ELSE BEGIN COMMENT  NOT IN DISKBUF;            71301000
                              IF LISTPOINTER!T1 THEN                    71303000
                                   READ(DSK[LISTPOINTER~T1],30,         71305000
                                   LISTBUF[*]);                         71307000
                              MOVESTRINGONLY(LISTBUF[T MOD 3 | 10],     71309000
                                   INFO[IOBUF]);                        71311000
                              END MOVING FROM LISTBUF;                  71313000
                         RUNNER[USER]~"P";                              71315000
                         GO TO SLEEP;                                   71317000
                         END TYPESTRING;                                71319000
                    BEGIN COMMENT(4) IDENTIFIES DATA STMT ONLY; END;    71321000
                    BEGIN COMMENT (5) RESTORE STATEMENT;                71323000
                         INFO[DATAPOINTER]~0; INFO[DATAINDEX]~0; END;   71325000
                    BEGIN COMMENT (6); END;                             71325100
                    BEGIN COMMENT (7); END;                             71325200
                    BEGIN COMMENT (8); END;                             71325300
                    BEGIN COMMENT (9); END;                             71325400
                    BEGIN COMMENT (10); END;                            71325500
                    BEGIN COMMENT (11); END;                            71325600
                    BEGIN COMMENT (12); END;                            71325700
                    BEGIN COMMENT (13) USE SIGNIFICANCE INTRINSIC;      71325800
                         INFO[SIGNIFICANCE] ~ 1; END;                   71325900
                    BEGIN COMMENT (14) USE TRACE INTRINSIC;             71326000
                         DEBUG ~ TRUE; END;                             71326100
                    BEGIN COMMENT (15) CLEAR SIGNIFICANCE INTR;         71326200
                         INFO[SIGNIFICANCE] ~ 0; END;                   71326300
                    BEGIN COMMENT (16) CLEAR TRACE INTRINSIC;           71326400
                         DEBUG ~ FALSE; END;                            71326500
                    BEGIN COMMENT (17) CLOSE TRACE INTRINSIC;           71326600
                         CLOSE(PR,SAVE); COMMENT PRINT DEBUG FILE;END;  71326700
                    END INTRINSIC;                                      71327000
%NEXTLINE                                                               71329000
               BEGIN CASENEXTLINE:                                      71331000
                    INFO[ACCEPTFLAG]~0;                                 71333000
                    T1~CBASE;                                           71335000
                    CBASE~CODE[CBASE].CLINK;                            71337000
                    IF CBASE = 0 OR CODE[CBASE].CPARTNUMB!              71339000
                    CODE[T1].CPARTNUMB THEN IF S!0 AND                  71341000
                    STACK[S].[3:6]!0 THEN BEGIN                         71343000
                         T1~STACK[(S~S-1)+1];                           71345000
                         CBASE~T1.[26:10];                              71347000
                         C~T1.[36:10];                                  71349000
                         L~T1.[46:2];                                   71351000
                         GO TO NEXT; END                                71353000
                    ELSE IF CBASE=0 THEN BEGIN RUNNER[USER]~0;GO TO XIT;71355000
                         END;                                           71357000
                    COMMENT  LINK 0 MEANS END OF STATEMENTS;            71359000
                    L ~ 0;                                              71361000
                    C ~ CBASE + 1;                                      71363000
                    GO TO NEXT;                                         71365000
                    END HANDLING NEXT LINE;                             71367000
%IFNOTRUSKIP                                                            71369000
               IF STACK[(S~S-1)+1] = 0 THEN BEGIN                       71371000
                    L ~ L + MODIFIER - 1;  WHILE L}4 DO BEGIN           71373000
                         L ~ L - 4; C ~ C + 1; END;  END IF NOT BFWD;   71375000
%PRINTSTRING                                                            71377000
               DO BEGIN                                                 71379000
                    IF T1~INFO[BUFINDEX]<72 THEN                        71381000
                    BEGIN                                               71383000
                         T2~MOVELIT(CODE[CBASE+MODIFIER],INFO[ALTBUF],  71385000
                              T1 DIV 32,ENTIER(T1 MOD 32));             71387000
                         IF T1+T2<72 THEN                               71389000
                         BEGIN                                          71391000
                              INFO[BUFINDEX]~T1+T2;                     71393000
                              IF T2=7 THEN MODIFIER~MODIFIER+1;         71395000
                         END ELSE INFO[BUFINDEX]~72;                    71397000
                    END;                                                71399000
               END UNTIL T2!7 OR INFO[BUFINDEX]}72;                     71401000
%PRINTEXP                                                               71403000
               BEGIN                                                    71405000
                    DEFINE SEGMENT = #;                                 71407000
                    IF INFO[FORMFLAG]=0 THEN                            71409000
                    BEGIN                                               71411000
                         T2~6;                                          71413000
                         T3~4;                                          71415000
                         IF ABS(T1~STACK[S]){549755813887 THEN IF T1=   71417000
                         ENTIER(T1) THEN                                71419000
                         BEGIN                                          71421000
                              COMMENT  INTEGER;                         71423000
                              FOR T2~3 STEP 3 WHILE ABS(T1)}10*T2 DO ;  71425000
                              IF ABS(T1)}10*(T2-1)AND T1<0 THEN T2~T2+3;71427000
                              T3~0;                                     71429000
                         END;                                           71431000
                        IF T3!0 THEN IF BOOLEAN(INFO[SIGNIFICANCE])     71433000
                              THEN BEGIN T2~12;  T3~9;  END MORE PLACES 71435000
                              ELSE IF ABS(T1)>999999.99 THEN T2~T2+6;   71437000
                         IF T3=0 THEN WRITE(LINE[*],F1,T2+1,0,          71439000
                              STACK[(S~S-1)+1]) ELSE WRITE(LINE[*],R1,  71441000
                              T2+T3, T3-1,                              71443000
                              STACK[(S~S-1)+1]);                        71445000
                    END NO FORM ELSE                                    71447000
                    BEGIN                                               71449000
                         IF T2~INFO[FORMPOINTER] DIV 3! LISTPOINTER     71451000
                              THEN READ(DSK[LISTPOINTER~T2],30,         71453000
                                   LISTBUF[*]);                         71455000
                         FORMCRACK (LISTBUF[T~INFO[FORMPOINTER] MOD 3   71457000
                              |10],ENTIER((T1~INFO[FORMINDEX]) MOD 32), 71459000
                              T1 DIV 32, T4, T2, T3);                   71461000
                         IF T4 = "~" THEN                               71463000
                         BEGIN                                          71465000
                              ERRNUMBER~41;  COMMENT  TOO FEW FIELDS    71467000
                                   IN A FORM STATEMENT;                 71469000
                              ERRCARD(INFO,CODE);                       71471000
                              GO TO XIT;                                71473000
                         END;                                           71475000
                         IF T4="E" THEN BEGIN                           71477000
                              ERRNUMBER~40;  COMMENT  FIELD TOO LARGE;  71479000
                              ERRCARD(INFO,CODE);                       71481000
                              GO TO XIT;                                71483000
                              END FORMCRACK RETURNED ERROR;             71485000
                         INFO[FORMINDEX]~T1+T2+T3;                      71487000
                         IF T4="F" THEN WRITE(LINE[*],F1,T2+T3+(IF T3=0 71489000
                         THEN 1 ELSE 0), IF T3=0                        71491000
                              THEN 0 ELSE T3-1,STACK[(S~S-1)+1]) ELSE   71493000
                    IF T4 = "L" THEN MOVEALPHA(LINE[*],T2,STACK[(S~S-1) 71493100
                         +1]) ELSE                                      71493200
                              WRITE(LINE[*],E1,T2+T3,IF T2<8 THEN 0 ELSE71495000
                              T2-7,STACK[(S~S-1)+1]);                   71497000
                    END;                                                71499000
                    IF INFO[BUFINDEX]+T2+T3<72 THEN                     71501000
                    BEGIN                                               71503000
                         MOVECHARS(LINE[*],0,0,ENTIER(T2+T3),INFO[ALTBUF71505000
                              ],                                        71507000
                         (T1~INFO[BUFINDEX]) DIV 32,ENTIER(T1 MOD 32)); 71509000
                         INFO[BUFINDEX]~T1+T2+T3;                       71511000
                    END;                                                71513000
                    IF INFO[FORMFLAG]=1 THEN MOVEFIELD(INFO,CODE,XIT)   71515000
                    ELSE IF T1~INFO[BUFINDEX]<72 THEN                   71517000
                    BEGIN                                               71519000
                         MOVEBLANK(INFO[ALTBUF],T1 DIV 32,ENTIER(T1 MOD 71521000
                              32));                                     71523000
                         INFO[BUFINDEX]~T1+1;                           71525000
                    END MOVING IN A BLANK TO SEPARATE FIELDS;           71527000
               END PRINTEXP;                                            71529000
%STOP                                                                   71531000
               BEGIN                                                    71533000
                    STOPMSG((T1~CODE[CBASE]).CPARTNUMB,T1.CSTEPNUMB,    71535000
                         INFO[IOBUF]);                                  71537000
                    RUNNER[USER]~"E";                                   71539000
                    GO TO XIT;                                          71541000
               END STOP;                                                71543000
%IDACCEPT                                                               71545000
               BEGIN                                                    71547000
                    INFO[ACCEPTFLAG]~1;                                 71549000
                    INFO[MODE]~T1~MODIFIER+MODIFIER+1;                  71551000
                    REQUEST(IDENT[T1-1],12,INFO[IOBUF],INFO[IOBUF],0);  71553000
                    RUNNER[USER]~"E";                                   71555000
                    GO TO SLEEP;                                        71557000
               END IDACCEPT;                                            71559000
%ARRAYACCEPT                                                            71561000
               BEGIN                                                    71563000
                    IF T1~ARRAYLOC(T5~MODIFIER+MODIFIER+1,IDENT,STACK,  71565000
                    S,CODE,INFO)<0 THEN GO TO XIT;%INVALID SUBSCRIPT    71567000
                    INFO[MODE]~T1+1;                                    71569000
                    REQUEST(IDENT[T5-1],12,INFO[IOBUF],STACK[S+1],      71571000
                         IDENT[T5].[1:2]+1);                            71573000
                    INFO[ACCEPTFLAG]~1;                                 71575000
                    INFO[ARRAYFLAG]~1;                                  71577000
                    RUNNER[USER]~"E";                                   71579000
                    GO TO SLEEP;                                        71581000
               END ARRAYACCEPT;                                         71583000
%FORTRANIF                                                              71585000
               BEGIN COMMENT  1,2, OR 3 BRANCH IF;                      71587000
                    T~STACK[(S~S-1)+1];                                 71589000
                    IF MODIFIER<3 THEN                                  71591000
                         IF T=0 THEN L~L+2 ELSE                         71593000
                    ELSE                                                71595000
                         IF T<0 THEN                                    71597000
                         ELSE IF T=0 THEN L~L+2 ELSE L~L+4;             71599000
                    IF L>3 THEN BEGIN L~L-4;  C~C+1;  END;              71601000
               END FORTRANIF;                                           71603000
%FORM                                                                   71605000
               BEGIN                                                    71607000
                    DEFINE SEGMENT = YES#;                              71609000
                    INFO[FORMPOINTER]~0;                                71611000
                    INFO[FORMFLAG]~1;                                   71613000
                    SNUMB~MODIFIER;                                     71615000
                    READNEXTINSTRUCTION;                                71617000
                    SNUMB~0&MODIFIER[26:42:6]&OP[20:42:6]&SNUMB[15:43:5]71619000
                    ;  T1 ~ CODE[0].CLINK;                              71621000
                    DO BEGIN                                            71623000
                         T~CODE[T1];                                    71625000
                         IF T2~REAL(BOOLEAN(T) AND BOOLEAN(STNOMASK))   71627000
                              = SNUMB THEN INFO[FORMPOINTER]~T.CDISKADDR71629000
                              ELSE T1~T.CLINK;                          71631000
                    END UNTIL T.CLINK = 0 OR T2 } SNUMB;                71633000
                    IF T1~INFO[FORMPOINTER]=0 THEN                      71635000
                    BEGIN                                               71637000
                         ERRNUMBER~38;  COMMENT  MISSING FORM STMNT;    71639000
                         ERRCARD(INFO,CODE);                            71641000
                         GO TO XIT;                                     71643000
                    END;                                                71645000
                    READ(DSK[LISTPOINTER~T1 DIV 3],30,LISTBUF[*]);      71647000
                    T2~FINDFORM(LISTBUF[T1 MOD 3|10]);                  71649000
                    INFO[FORMINDEX]~T2;                                 71651000
                    IF T2 > 55 THEN                                     71653000
                    BEGIN                                               71655000
                         ERRNUMBER~39;  COMMENT  NOT A FORM;            71657000
                         ERRCARD(INFO,CODE);                            71659000
                         GO TO XIT;                                     71661000
                    END;                                                71663000
                    MOVEFIELD(INFO,CODE,XIT);                           71665000
                         COMMENT  MOVE FIRST ALPHA FIELD INTO BUFFER;   71667000
               END FORM INITIALIZATION;                                 71669000
%STRINGFETCH                                                            71671000
               STACK[S~S+1]~CODE[CBASE+MODIFIER];  COMMENT  STRING;     71673000
%IDREADDATA                                                             71675000
          BEGIN                                                         71677000
          IF T5 ~ GETNEXTDATA(T1,INFO,CODE) = 0 THEN                    71679000
          BEGIN                                                         71681000
               OUTOFDATAMSG((T~CODE[CBASE]).CPARTNUMB,T.CSTEPNUMB,      71683000
                    INFO[IOBUF]);                                       71685000
               RUNNER[USER] ~ "E";                                      71687000
               GO TO XIT;                                               71689000
          END ELSE IDENT[MODIFIER+MODIFIER+1] ~ T1;                     71691000
          IF T5 < 0 THEN BEGIN RUNNER[USER] ~ "S"; GO TO SLEEP;         71693000
               END END IDREADDATA;                                      71695000
%ARRAYREADDATA                                                          71697000
          BEGIN                                                         71699000
          IF T5 ~ GETNEXTDATA(T6,INFO,CODE) = 0 THEN                    71701000
          BEGIN                                                         71703000
               OUTOFDATAMSG((T~CODE[CBASE]).CPARTNUMB,T.CSTEPNUMB,      71705000
                    INFO[IOBUF]);                                       71707000
               RUNNER[USER] ~ "E";                                      71709000
               GO TO XIT;                                               71711000
          END ELSE IF T~ARRAYLOC(MODIFIER+MODIFIER+1,IDENT,STACK,S,CODE,71713000
               INFO) < 0 THEN GO TO XIT % INVALID SUBSCRIPT             71715000
          ELSE USERARRAY[T] ~ T6;                                       71717000
          IF T5 < 0 THEN BEGIN RUNNER[USER] ~ "S"; GO TO SLEEP;         71719000
               END END ARRAYREADDATA;                                   71721000
          END CASE OP CODE;                                             71723000
          IF DEBUG THEN WRITE (PR[DBL], 17, STACK[*]);                  71725000
          GO TO NEXTONE;                                                71727000
BRANCH:   SNUMB~MODIFIER;                                               71729000
          T6~CBASE;                                                     71731000
          READNEXTINSTRUCTION;                                          71733000
          SNUMB ~ 0&MODIFIER[26:42:6]&OP[20:42:6]&SNUMB[15:43:5];       71735000
          COMMENT  BUILD 18 BIT STATEMENT # FROM 2 INSTRUCTIONS;        71737000
          IF LASTI ! 0 THEN                                             71739000
               FOR I ~ LASTI-1 STEP -1 UNTIL 0 DO                       71741000
                    IF REAL(BOOLEAN(GOTOSOLVER[I]) AND BOOLEAN(STNOMASK)71743000
                              ) = SNUMB THEN BEGIN                      71745000
                         C ~ GOTOSOLVER[I].[33:15];                     71747000
                         L ~ 0;                                         71749000
                         CBASE ~ C - 1;                                 71751000
                         GO TO CHECKSTACK;                              71753000
                         END TABLE LOOKUP SUCCESSFUL;                   71755000
          C ~ CODE[0].CLINK;                                            71757000
          DO BEGIN T~CODE[C];                                           71759000
               COMMENT  LINK THRU STATEMENTS UNTIL FOUND;               71761000
               IF REAL(BOOLEAN(T) AND BOOLEAN(STNOMASK)) = SNUMB THEN   71763000
                    BEGIN                                               71765000
                    CBASE~C;                                            71767000
                    GOTOSOLVER[LASTI] ~ (C~C+1)&SNUMB[15:15:17];        71769000
                    COMMENT  SAVE ST# AND C IN TABLE;                   71771000
                    IF LASTI ~ LASTI + 1 = 20 THEN                      71773000
                         FOR LASTI ~ 0 STEP 1 UNTIL 14 DO               71775000
                              GOTOSOLVER[LASTI] ~GOTOSOLVER[LASTI+5];   71777000
                              COMMENT   PUSH DOWN LOOKUP TABLE;         71779000
                         IF DEBUG THEN WRITE (PR[DBL], 17, GOTOSOLVER[*]71781000
                              );                                        71783000
                         L~0;  GO TO CHECKSTACK; %BRANCH FOUND          71785000
                    END ELSE C ~ T.CLINK;                               71787000
               END UNTIL T.CLINK=0;                                     71789000
          C~1;                                                          71791000
          WHILE C<INFO[CMAX] DO BEGIN T~CODE[C];                        71793000
               IF T.SBIT!1 THEN IF REAL(BOOLEAN(T) AND BOOLEAN(STNOMASK)71795000
               )=SNUMB THEN BEGIN                                       71797000
                    ERRNUMBER~29; ERRCARD(INFO,CODE); GO TO XIT;        71799000
               END STATEMENT PRESENT BUT NOT IN RANGE;                  71801000
               C~T.CSIZE+C;                                             71803000
          END;                                                          71805000
          ERRNUMBER~30;  COMMENT ST # SEARCH FAILED;                    71807000
          ERRCARD(INFO,CODE);                                           71809000
          GO TO XIT;                                                    71811000
CHECKSTACK:                                                             71813000
     IF S!0 AND STACK[S].[1:8]=63 THEN IF CODE[CBASE].CPARTNUMB         71815000
          !CODE[T6].CPARTNUMB THEN                                      71817000
     DO IF STACK[S].CPARTNUMB ! CODE[CBASE].CPARTNUMB THEN              71829100
     BEGIN                                                              71829110
          S~S-1;                                                        71829120
          IF S ! 0 THEN IF STACK[S].[1:8] ! 63 THEN S~S-1;              71829130
     END ELSE GO TO NEXT                                                71829140
     UNTIL S=0;                                                         71829150
     GO TO NEXT;                                                        71829160
INTERR:   BEGIN ERRNUMBER~31; ERRCARD(INFO,CODE); GO TO XIT; END;       71831000
               COMMENT INTEGER OVERFLOW IN ENTIER FUNCTION;             71833000
EXPERR:   BEGIN ERRNUMBER~32; ERRCARD(INFO,CODE); GO TO XIT; END;       71835000
               COMMENT EXPONENT OVERFLOW;                               71837000
ZEROERR:  BEGIN ERRNUMBER~33; ERRCARD(INFO,CODE); GO TO XIT; END;       71839000
               COMMENT ATTEMPTED DIVIDE BY ZERO;                        71841000
INDEXERR:BEGIN IF S}STACKARRAYSIZE THEN ERRNUMBER~34 ELSE ERRNUMBER~102;71843000
               ERRCARD(INFO,CODE);  GO TO XIT;  END INVALID INDEX;      71845000
               COMMENT STACK OVERFLOW PROBABLY CAUSED BY RECURSIVE      71847000
               PART CALLS;                                              71849000
SLEEP:    INFO[CINDX] ~C;  INFO[LINDX]~L;  INFO[SINDX]~S;               71851000
               INFO[CBASEINDX] ~ CBASE;COMMENT  SAVE REGISTERS AND      71853000
               SUSPEND TASK;                                            71855000
XIT:                                                                    71857000
IF DEBUG THEN WRITE (PR[DBL], 17, IDENT[*]);                            71859000
     END PROCEDURE EXECUTECODE;                                         71861000
% ********************************************************************  80001000
% ********************************************************************  80003000
% ****************** S C H E D U L E R  ****************************    80005000
% ********************************************************************  80007000
% ********************************************************************  80009000
% ********************************************************************  80011000
     COMMENT                                                            80013000
          SCHEDULER CONTROLS ALL DATACOM I/O, SELECTION AND SCHEDULING. 80015000
          IT USES THE ARRAY  USERID  TO DETERMINE LOGGED IN USERS AND   80017000
          USES THE ARRAY RUNNER AS SCHEDULE INFORMATION.                80019000
          DECLARATION OF THE ARRAYS FOR EACH USER ARE CONTROLLED BY     80021000
          PASSING ARRAY SIZES INTO THIS PROCEDURE.  USER ARRAYS ARE     80023000
          DECLARED DOUBLY SUBSCRIPTED HERE.  ROWS ARE PASSED AS SINGLY  80025000
          DIMENSIONED ARRAYS TO PROCEDURES CALLED BY SCHEDULER.         80027000
          THE RUNNER ARRAY SERVES AS AN INTERLOCK AS WELL AS A SCHEDULE.80029000
          THE RUNNER VALUE MAY SPECIFY (1) IDLE,  (2) INPUT IN PROGRESS,80031000
          (3) OUTPUT IN PROGRESS, OR  (4) A TASK NEEDS TO BE PERFORMED. 80033000
          ITS FORMAT IS:                                                80035000
               [1:1]     IF ON, WRITE INFO[USER,IOBUF] TO TERMINAL.     80037000
               [12:12]   CHAR POSITION OF NEXT INPUT BUF LOAD (0,28,56).80039000
               [24:12]   CHAR POSITION OF NEXT OUTPUT BUF LOAD          80041000
               [36:12]   FUNCTION CODE FOR RUNNERS LOOP.                80043000
          WHEN A RUNNER IS ACTIVE, [1:35] IS 0 AND [36:12] CONTAINS     80045000
          THE FUNCTION (WHICH MAY BE MODIFIED BY A RUNNER);             80047000
DEFINE DCWDS=5#,  BUFLOAD =27#;                                         80049000
ALPHA FILE IN DCI 14 (2|(MAX+1), MAXINPUTBUFFERSIZE);                   80051000
ALPHA FILE OUT DCO1 14 (2,DCWDS);                                       80053000
ALPHA FILE OUT DCO2 14 (2,DCWDS);                                       80055000
ALPHA FILE OUT DCO3 14 (2,DCWDS);                                       80057000
ALPHA FILE OUT DCO4 14 (2,DCWDS);                                       80059000
ALPHA FILE OUT DCO5 14 (2,DCWDS);                                       80061000
ALPHA FILE OUT DCO6 14 (2,DCWDS);                                       80063000
ALPHA FILE OUT DCO7 14 (2,DCWDS);                                       80065000
SWITCH FILE DCO~DCO1,DCO2,DCO3,DCO4,DCO5,DCO6,DCO7;                     80067000
SAVEOPTION                                                              80069000
ARRAY DCBUF[0:(IF MAX<3 THEN 8 ELSE (MAX+1)|2+2)];                      80071000
          COMMENT  INPUT-OUTPUT  AND STATUS BUFFER FOR DATACOM;         80073000
SAVEOPTION                                                              80075000
ARRAY DCTUBUF[0:MAX];  COMMENT    TU AND BUF # FOR USERS;               80077000
INTEGER CYCLE;                                                          80079000
BOOLEAN IDLE;                                                           80081000
INTEGER SAVEINTERLOCK;                                                  80083000
INTEGER LOADINTERLOCK;                                                  80085000
DEFINE LOADBUF=SAVEBUF#;                                                80087000
PROCEDURE SCHEDULER(CODEMAXSIZE,USERARRAYMAXSIZE);                      80089000
          VALUE CODEMAXSIZE, USERARRAYMAXSIZE;                          80091000
          INTEGER CODEMAXSIZE, USERARRAYMAXSIZE;                        80093000
          BEGIN                                                         80095000
          LABEL WRITER, WRITEANDCLEAR;                                  80097000
          LABEL NEXT;                                                   80099000
OWN ARRAY STACK[0:MAX, 0:STACKARRAYSIZE],                               80101000
          CODE[0:MAX, 0:CODEMAXSIZE],                                   80103000
          USERARRAY[0:MAX, 0:USERARRAYMAXSIZE];                         80105000
          COMMENT  DYNAMIC ARRAY DECLARATION FOR USER ARRAYS;           80107000
          LABEL RUNNERS;                                                80109000
          LABEL ALLOCATE;                                               80111000
          LABEL ENDRUN;                                                 80113000
          LABEL ENDLOOP;                                                80115000
% ************************** D C I O  *******************************   80117000
COMMENT                                                                 80119000
          DCIO DRIVES THE REMOTE TERMINALS.  IT USES :                  80121000
               INFO[USER,IOBUF] FOR INPUT AND OUTPUT AREAS,             80123000
               RUNNER[USER] FOR FUNCTION AND OFFSET INFORMATION,        80125000
               DCTUBUF[USER]  FOR THE [9:9] DATACOM ADDRESS,            80127000
               USERID[USER]  FOR THE FILE SECURITY CODE,                80129000
               DCBUF[*]  FOR ACTUAL I/O.                                80131000
          DCIO HAS FOUR FUNCTIONS:                                      80133000
               1.   DISPENSE QUEUED WRITES.                             80135000
               2.   ATTACH NEW USERS.                                   80137000
               3.   ATTEMPT TO READ FROM TERMINALS.                     80139000
               4.   TIMEOUT IF IDLE TOO LONG.                           80141000
          DCIO CHANGES THE USER INDEX ON ATTACHMENT OR COMPLETED INPUT  80143000
          LINE;                                                         80145000
BOOLEAN PROCEDURE DCIO;                                                 80147000
          BEGIN                                                         80149000
          INTEGER TIMER;                                                80151000
          LABEL E, WNEXT, NO, XIT;                                      80153000
          LABEL DATACOM;                                                80155000
          LABEL ATTACH, DETACH, RESTORE;                                80157000
          LABEL RNEXT;                                                  80159000
          LABEL TIMESUP;                                                80161000
          LABEL SEEKIT;                                                 80163000
          DEFINE  STATE = T4#, U = T3#;                                 80165000
          DEFINE  WRITEOFFSET = [24:12]#, READOFFSET = [12:12]#;        80167000
BOOLEAN STREAM PROCEDURE MOVETOGPMK(S,OFFSET, D);                       80169000
          VALUE  OFFSET;                                                80171000
          BEGIN                                                         80173000
          SI ~ S;  SI ~ SI+OFFSET;  DI ~ D;                             80175000
          27(IF SC!"~" THEN DS~CHR ELSE BEGIN DS~CHR;                   80177000
               TALLY ~ 1;  MOVETOGPMK ~ TALLY;  JUMP OUT;  END);        80179000
          DS~LIT"~";    COMMENT  OUTPUT IN 27 CHAR RECORDS;             80181000
          END MOVETOGPMK;                                               80183000
INTEGER STREAM PROCEDURE MOVEIN(S, D, OFFSET);  VALUE OFFSET;           80185000
          BEGIN LOCAL SAVSOURCE;  LABEL FIFTYSIX, XIT;                  80187000
          SI~S;  DI~D;  DI~DI+OFFSET;   TALLY~28;                       80189000
          28(IF SC!"~" THEN DS~CHR ELSE BEGIN TALLY~0;  DS~CHR;         80191000
               JUMP OUT TO XIT;  END);   COMMENT  LESS THAN 28 CHAR ;   80193000
          SAVSOURCE~SI;  COMMENT  NOW CHECK FOR 28/56 BUFFERS;          80195000
          11(IF SC="0" THEN SI~SI+1 ELSE JUMP OUT TO FIFTYSIX);         80197000
          IF SC>"9" THEN GO TO XIT;  COMMENT  WAS A 28 CHAR BUFFER;     80199000
FIFTYSIX: SI~SAVSOURCE;                                                 80201000
          TALLY~56;  28(IF SC!"~" THEN DS~CHR ELSE BEGIN  DS~CHR;       80203000
               TALLY~0;  JUMP OUT TO XIT;  END);                        80205000
XIT:      MOVEIN~TALLY;  COMMENT  EITHER OFFSET VALUE OR 0 FOR ~ ;      80207000
          END MOVEIN;                                                   80209000
BOOLEAN STREAM PROCEDURE GPMRK(S);                                      80211000
BEGIN                                                                   80213000
     DI~LOC GPMRK;  DI~DI+7;  DS~ LIT "0";  SI~S;                       80215000
     17(IF SC = "~" THEN BEGIN DI~DI-1;  DS~LIT "1";  JUMP OUT;  END;   80217000
     SI~SI+1);                                                          80219000
END GPMRK;                                                              80221000
STREAM PROCEDURE LINETOOLONG(D);                                        80223000
BEGIN DI ~ D;  DS ~ 48 LIT                                              80225000
     "{!AN INPUT LINE IS RESTRICTED TO 72 CHARACTERS~";                 80227000
     END LINETOOLONG;                                                   80229000
STREAM PROCEDURE TIMEOUTMSG(D);                                         80231000
          BEGIN                                                         80233000
          DI ~ D;  DS ~ 25 LIT "TIMEOUT IN 15 SECONDS{!:~";             80235000
          END STREAM;                                                   80237000
STREAM PROCEDURE MOVEBRK(D);                                            80239000
          BEGIN                                                         80241000
          DI ~ D;  DS ~ 11 LIT "{!BREAK{!:~";                           80243000
          END MOVEBRK;                                                  80245000
STREAM PROCEDURE MOVE9(S,D);                                            80247000
          BEGIN SI ~ S;  DI ~ D;  DS ~ 9WDS;  END STREAM;               80249000
STREAM PROCEDURE ZEROOUT(D1, D2);                                       80251000
          BEGIN                                                         80253000
          DI ~ D2;  DS ~ 8 LIT"0";  SI ~ D2;  4(DS ~ 32 WDS);           80255000
          DI ~ D1;  SI ~ D2;  DS ~ INFMAX WDS;  END  STREAM;            80257000
STREAM PROCEDURE MOVEC(S, D, D2);                                       80259000
          BEGIN  SI ~ S;  DI ~ D;  DS ~ 9 WDS;                          80261000
          DI ~ D2;  DS ~ 3 LIT "{!~";  END MOVEC;                       80263000
PROCEDURE TOOMANY(S);  VALUE S;  INTEGER S;                             80265000
          BEGIN                                                         80267000
               ALPHA FILE OUT DCO99 14(1,DCWDS);                        80269000
               COMMENT  FIND THE NEW STATION BY ELIMINATION;            80271000
               FOR T~0 STEP 2 UNTIL S-1 DO BEGIN                        80273000
                    T2~1;                                               80275000
                    FOR T1~0 STEP 1 UNTIL MAX DO                        80277000
                    IF DCBUF[T]=0 OR DCBUF[T]=DCTUBUF[T1] THEN T2~0;    80279000
                    IF BOOLEAN(T2) THEN BEGIN  FILL SYNTAX[*] WITH      80281000
                         0, "SORRY, T", "OO MANY ", "USERS{!~";         80283000
                    WRITE(DCO99(SYNTAX[0]~DCBUF[T],0),4,SYNTAX[*]);     80285000
                    RELEASE(DCBUF[T]);                                  80287000
                    END WRITE;                                          80289000
                    END LOOKING;                                        80291000
         END TOOMANY;                                                   80293000
%         --------                                                      80295000
COMMENT   1.                                                            80297000
          CYCLE THRU ALL USERS LOOKING FOR A QUEUED LINE OF OUTPUT.     80299000
          IF OUTPUT IS QUEUED, CHECK THE STATUS (PASSIVE) OF THE TERMNAL80301000
               IF BREAK IS UP, CHANGE THE OUTPUT LINE TO READ "BREAK"   80303000
               AND SHUT OFF THE RUNNER (SPEC. ACTION FOR LOADS).        80305000
               MOVE A BUFFER LOAD INTO DCBUF[1] AND WRITE IT.           80307000
               IF A GROUP MARK WAS WRITTEN, SHUT OFF OUTPUT QUEUE AND   80309000
               RESET OFFSET.                                            80311000
                    IF THE RUNNER IS STILL ACTIVE, TURN OFF IDLE FLAG   80313000
                    AND INSURE THAT ACTIVE IS TURNED BACK ON.           80315000
;                                                                       80317000
DATACOM:  U~USER;                                                       80319000
          DO BEGIN                                                      80321000
               IF RUNNER[U].SBIT=1 THEN                                 80323000
               BEGIN                                                    80325000
                    STATE~STATUS(DCTUBUF[U],0);                         80327000
                    IF STATE.[26:1]=1 THEN                              80329000
                    BEGIN COMMENT BREAK;                                80331000
                         MOVEBRK(INFO[U,IOBUF]);                        80333000
                         IF RUNNER[U].[36:12]="I" THEN                  80335000
                         BEGIN                                          80337000
                              CLOSE(LOADFILE);                          80339000
                              LOADINTERLOCK~-1;                         80341000
                              INFO[U,LOADLIST]~0;                       80343000
                         END BREAK ON A LOAD;                           80345000
                         RUNNER[U].[24:24]~0;                           80347000
                    END HANDLING BREAK;                                 80349000
                    IF STATE.[30:1]=1 THEN GO TO DETACH;                80351000
                    COMMENT   CHECK FOR DISCONNECT;                     80353000
                    OK~MOVETOGPMK(INFO[U,IOBUF],T1~RUNNER[U].WRITEOFFSET80355000
                    ,DCBUF[1]);                                         80357000
                    WRITE(DCO[U](DCBUF[0]~DCTUBUF[U],0),DCWDS,          80359000
                         DCBUF[*])[E];                                  80361000
                         IF OK THEN                                     80363000
                         BEGIN                                          80365000
                              RUNNER[U].WRITEOFFSET~0;                  80367000
                              IF RUNNER[U].[36:12]!0 THEN               80369000
                              BEGIN                                     80371000
                                   IDLE~FALSE;                          80373000
                                   ACTIVE~TRUE;                         80375000
                              END;                                      80377000
                              RUNNER[U].SBIT~0;                         80379000
                              EVENTS~EVENTS+1;                          80381000
                         END ELSE RUNNER[U].WRITEOFFSET~T1+BUFLOAD;     80383000
          END;                                                          80385000
E:                                                                      80387000
WNEXT:    IF U~U+1>MAXUSER THEN U~0;                                    80389000
          END UNTIL U=USER;                                             80391000
COMMENT      2.                                                         80393000
          NOW LOOK FOR A NEW USER BY MATCHING THE SYSTEM LIST OF        80395000
          ATTACHED TERMINALS WITH THE CURRENT LIST.                     80397000
          IF A NEW USER IS FOUND:                                       80399000
               A.  FIND AN INDEX SLOT FOR HIM.                          80401000
               B.  SETUP TERMINAL AND USER ID.                          80403000
               C.  CLEAR HIS INFO AND IDENTIFIERS.                      80405000
               D.  SCHEDULE HIM FOR ATTACHMENT MESSAGE.                 80407000
               E.  ADJUST MAXUSER AND FLAGS.                            80409000
;                                                                       80411000
          IF TIMER MOD 5 = 1 THEN GO TO ATTACH;                         80413000
                                                             %VOID      80415000
          EVENTS~EVENTS+1;                                              80417000
          IF EVENTS MOD 5 ! 0 THEN GO TO RNEXT;                         80419000
ATTACH:                                                                 80421000
          IF STATE ~ STATUS(DCBUF[*]) { 0 THEN GO TO ENDEND;            80423000
          FOR I~0 STEP 2 UNTIL STATE-2 DO BEGIN                         80425000
               FOR U ~ 0 STEP 1 UNTIL MAXUSER DO IF DCBUF[I]=DCTUBUF[U] 80427000
                    THEN IF DCBUF[I+1]=USERID[U] THEN BEGIN             80429000
                         COMMENT  FOUND A MATCH;                        80431000
                         DCBUF[I]~0;  USERID[U]~-USERID[U];             80433000
                         COMMENT USERIDS ARE MARKED NEG. TEMPORARILY;   80435000
                         END SCRATCHING ENTRY FROM BOTH LISTS;          80437000
               IF DCBUF[I]!0 THEN BEGIN  COMMENT  UNIDENTIFIED USER;    80439000
                    FOR U~0 STEP 1 UNTIL MAXUSER DO IF I!U THEN         80441000
                    IF DCBUF[I]=DCTUBUF[U] THEN BEGIN                   80443000
DETACH:             RUNNER[U]~"Q";                                      80445000
                    INFO[U,CMAX]~0;   COMMENT  DONT GIVE EOJ MSG;       80447000
                    IDLE~FALSE;  ACTIVE~TRUE;                           80449000
          RESTORE:  FOR U~0 STEP 1 UNTIL MAXUSER DO                     80451000
                         USERID[U]~ABS(USERID[U]);                      80453000
                    GO TO RNEXT;                                        80455000
                    END TERMINAL HAS DISAPPEARED;                       80457000
               T~0;  WHILE DCTUBUF[T]!0 DO IF T~T+1>MAX THEN BEGIN      80459000
                    TOOMANY(STATE);  GO TO RESTORE;  END TOO MANY;      80461000
                    DCTUBUF[T]~DCBUF[I];  COMMENT  SET UP TERMINAL ID;  80463000
                    IF USERID[T]~-DCBUF[I+1]=0 THEN GO TO DETACH;       80465000
                    ZEROOUT(INFO[T,1],                                  80467000
                         IDENTIFIERS[T,0]);                             80469000
                    CODE[T,0]~CODE[T,1]~0;                              80471000
                    RUNNER[T] ~ "B";                                    80473000
                    CYCLE ~ T;                                          80475000
                    ACTIVE ~ TRUE;                                      80477000
                    IF INFO[0,RUNLIMIT]=0 THEN  % TRUE IF FIRST USER;   80479000
                    READ(DCI(DCTUBUF[T],0),5,DCBUF[*])[SEEKIT:SEEKIT];  80481000
               SEEKIT: COMMENT FILEOPEN BEFORE SEEK NEEDED BY MK8 MOD21;80483000
                    SEEK(DCI(DCTUBUF[T]));                              80485000
                    IDLE ~ FALSE;                                       80487000
                    MAXUSER ~ MAX;                                      80489000
                    WHILE DCTUBUF[MAXUSER] = 0 DO MAXUSER~MAXUSER-1;    80491000
                    END ATTACHING A NEW USER;                           80493000
               END LOOKING FOR NEW USERS;                               80495000
          FOR U~0 STEP 1 UNTIL MAXUSER DO IF USERID[U]>0 THEN GO TO     80497000
               DETACH ELSE USERID[U]~-USERID[U];                        80499000
COMMENT     3.                                                          80501000
          NOW DO A GENERAL READ.  IF INPUT IS READ, IDENTIFY THE USER.  80503000
               IF PAST COL 73, GIVE A DIAGNOSTIC.                       80505000
               IF A COMPLETE INPUT LINE:                                80507000
                    A.  MOVE CARD IMAGE INTO SYNTAX[0].                 80509000
                    B.  ADJUST FLAGS.                                   80511000
                    C.   WRITE "{!".                                    80513000
                    D.  EXIT  DCIO=TRUE                                 80515000
;                                                                       80517000
          RNEXT:    DCBUF[5]~12;  COMMENT  USE QUESTMARK FOR 28/56 TEST;80519000
                    READ(DCI(0,0),8,DCBUF[*])[NO:NO];                   80521000
                    U~-1;                                               80523000
                    FOR T~0 STEP 1 UNTIL MAXUSER DO                     80525000
                         IF DCBUF[0].[9:9]=DCTUBUF[T].[9:9] THEN U~T;   80527000
                    IF U<0 THEN GO TO RNEXT;                            80529000
                    IF RUNNER[U].[36:12]!0 THEN GO TO RNEXT;            80531000
                    IF T2~RUNNER[U].READOFFSET = 56 THEN                80533000
                    IF NOT GPMRK(DCBUF[1]) THEN                         80535000
                    BEGIN                                               80537000
                         ACTIVE~TRUE;                                   80539000
                         LINETOOLONG(INFO[U,IOBUF]);                    80541000
                         RUNNER[U]~"E";                                 80543000
                         USER ~ CYCLE ~ U;                              80545000
                         GO TO XIT;                                     80547000
                    END LINE TOO LONG;                                  80549000
                    IF T1~MOVEIN(DCBUF[1], INFO[U,IOBUF], T2~RUNNER[U]. 80551000
                         READOFFSET)=0 THEN BEGIN                       80553000
                         MOVEC(INFO[U,IOBUF], SYNTAX[0], DCBUF[1]);     80555000
                         RUNNER[U].READOFFSET ~ 0;                      80557000
                         IDLE ~ ACTIVE ~ FALSE;                         80559000
                         DCIO ~ TRUE;                                   80561000
                         USER ~ U;                                      80563000
                         EVENTS~EVENTS+1;                               80565000
                         WRITE(DCO[U](DCBUF[0]~DCTUBUF[U],1),2,DCBUF[*])80567000
                         [XIT:XIT];                                     80569000
                         GO TO XIT;                                     80571000
                    END COMPLETED A READ OPERATION ELSE BEGIN           80573000
                         RUNNER[U].READOFFSET~T2+T1;                    80575000
                         GO TO RNEXT; END;                              80577000
          NO:                                                           80579000
          IF IDLE THEN BEGIN                                            80581000
               IF TIMER~TIMER+1=(TIMEOUT+20)|2 THEN BEGIN               80583000
          TIMESUP:  ACTIVE~TRUE;                                        80585000
                    FOR U~0 STEP 1 UNTIL MAXUSER DO IF DCTUBUF[U]!0 THEN80587000
                         RUNNER[U]~"Q";                                 80589000
                    GO TO XIT;  END TIMEOUT OF ALL USERS;               80591000
               IF TIMER!TIMEOUT|2 THEN T~DELAY(513,0,0.5)  %HALF SEC DEL80593000
                    ELSE FOR I~0 STEP 1 UNTIL MAXUSER DO                80595000
                         IF DCTUBUF[I]!0 THEN BEGIN                     80597000
                              RUNNER[I].SBIT~1;                         80599000
                              RUNNER[I].WRITEOFFSET~0;                  80601000
                              TIMEOUTMSG(INFO[I,IOBUF]);                80603000
                              END WARNING MESSAGE ;                     80605000
               GO TO DATACOM;  END IDLE LOOP;                           80607000
XIT:      END DCIO;                                                     80609000
% *******************************************************************   80611000
STREAM PROCEDURE SETSEQ(D, PART, STP );  VALUE PART, STP;               80613000
          BEGIN  DI ~ D;  DS ~ 3 LIT "0";  SI ~ LOC PART;  DS ~ 2 DEC;  80615000
          SI ~ LOC STP;   DS ~ 3 DEC; END SETSEQ;                       80617000
STREAM PROCEDURE MOVEG(S,D);                                            80619000
          BEGIN                                                         80621000
          SI ~ S;  DI ~ D;                                              80623000
         9(8(IF SC ! "~" THEN DS ~ CHR ELSE DS ~ 1 LIT " "));           80625000
          END DELETE GROUP MARK AND BLANK FILL FOR TEXT EDITOR ;        80627000
STREAM PROCEDURE MOVE9(S,D);  BEGIN SI ~ S;  DI ~ D;  DS ~ 9 WDS;  END; 80629000
STREAM PROCEDURE MSG(V, D);  VALUE V;                                   80631000
          BEGIN DI ~ D;  SI ~ LOC V;  SI ~ SI + 7;                      80633000
          IF SC = "S" THEN DS ~ 5 LIT "SAVED";                          80635000
          IF SC = "L" THEN DS ~ 6 LIT"LOADED";                          80637000
          IF SC = "N" THEN DS ~ 43 LIT                                  80639000
               "THERE IS NO FILE WITH THAT NAME ON THE DISK";           80641000
          IF SC = "F" THEN DS ~ 19 LIT "DUPLICATE FILE NAME";           80643000
          IF SC = "B" THEN DS ~ 49 LIT                                  80645000
               "WIPL VERSION 1.5{!TYPE HELP IF YOU HAVE QUESTIONS";     80647000
          IF SC = "D" THEN DS ~ 44 LIT                                  80649000
               "SOMEONE ELSE IS LOGGED-IN UNDER YOUR USER-ID";          80651000
          IF SC = "R" THEN DS ~ 31 LIT                                  80653000
               "YOUR PROGRAM IS BEING RESTARTED";                       80655000
          IF SC = "C" THEN DS ~ 30 LIT "YOUR LAST VALID STATEMENT WAS:";80657000
          DS ~ 1 LIT "~";                                               80659000
          END STREAM;                                                   80661000
STREAM PROCEDURE SIGNAL(D);  BEGIN  DI ~ D;  DS ~ 2 LIT ":~";  END;     80663000
STREAM PROCEDURE CARRIAGE(A, B);   VALUE B;                             80665000
          BEGIN LOCAL T; LABEL L;                                       80667000
          SI ~ A;  9(8(IF SC = "~" THEN JUMP OUT 2 TO L ELSE SI~SI+1)); 80669000
          L:   T ~ SI;  DI ~ T;  SI ~ SI - 1;  IF SC { "9" THEN         80671000
               DS ~ 2 LIT "{!" ELSE DS ~ LIT "!";                       80673000
          B(DS ~ LIT ":");  DS ~ LIT "~";  END CARRIAGE;                80675000
STREAM PROCEDURE SPACEONE(D);                                           80677000
          BEGIN LOCAL T;  LABEL L;                                      80679000
          SI ~ D; 9(8(IF SC!"~" THEN SI~SI+1 ELSE JUMP OUT 2 TO L));    80681000
          L:  T~SI;  DI~T;  DS ~ 3 LIT "{!~";                           80683000
          END SPACEONE;                                                 80685000
STREAM PROCEDURE EOJMESSAGE(D,T1,T2); VALUE T1,T2;                      80687000
BEGIN DI ~ D;  SI ~ LOC T1;  DS ~ 7 LIT "EOJ AT ";  DS ~ 2 DEC;         80689000
     SI ~ LOC T2;  DS ~ 2 DEC;  DS ~ LIT "~";                           80691000
END EOJMESSAGE;                                                         80693000
BOOLEAN STREAM PROCEDURE WAITMSG(D);                                    80695000
          BEGIN LOCAL T;                                                80697000
          DI~LOC T;  DS~6 LIT "PLEASE";  DI~D;  SI~LOC T;               80699000
          IF 6 SC=DC THEN TALLY~1 ELSE BEGIN  DI~D;                     80701000
               DS~15 LIT "PLEASE WAIT...~"; END;                        80703000
          WAITMSG~TALLY;                                                80705000
          END WAITMSG;                                                  80707000
STREAM PROCEDURE PUBLICSAVE(D,N);  VALUE N;                             80709000
          BEGIN                                                         80711000
          DI~D;  DS~10 LIT "CC PUBLIC ";  SI~LOC N;  SI~SI+1; DS~7 CHR; 80713000
          DS~11 LIT "/CARDS;END.";                                      80715000
          END PUBLICSAVE;                                               80717000
BOOLEAN STREAM PROCEDURE STOP(S);                                       80719000
          BEGIN LOCAL T;                                                80721000
          DI~LOC T;  DS~5 LIT "STOP~";  SI~S;  DI~LOC T;                80723000
          IF 5 SC=DC THEN TALLY~1;  STOP~TALLY;                         80725000
          END STOP;                                                     80727000
BOOLEAN STREAM PROCEDURE FLAGBIT(S);                                    80729000
BEGIN  SI~S;  IF SB THEN BEGIN TALLY~1;  FLAGBIT~TALLY;  END; END;      80731000
PROCEDURE GETERRMESSAGE;                                                80733000
IF ERRNUMBER!0 THEN                                                     80735000
BEGIN                                                                   80737000
     T1~ERRORSTART|3+ERRNUMBER;                                         80739000
     IF (T2~T1 DIV 3) ! LISTPOINTER THEN                                80741000
          READ(DSK[LISTPOINTER~T2],30,LISTBUF[*]);                      80743000
          COMMENT READ ERROR FROM DISK;                                 80745000
     MOVE9(LISTBUF[T1 MOD 3 | 10],INFO[USER,IOBUF]);                    80747000
     ERRNUMBER~0;                                                       80749000
END GETERRMESSAGE;                                                      80751000
DEFINE CHARGETHEUSER = T1~STATUS(DCTUBUF[USER],0)#;                     80753000
% --------------------------------------------------------------------  80755000
% ----------------- M A I N   L O O P  --------------------------       80757000
% --------------------------------------------------------------------  80759000
COMMENT                                                                 80761000
          THE MAIN LOOP FUNCTIONS AS FOLLOWS:                           80763000
          1.   IF ARRAYS HAVE BEEN REALLOCATED, THE CARD IMAGE IS       80765000
               RE-PROCESSED, OTHERWISE DCIO IS CALLED TO ATTACH THE     80767000
               FIRST USER AND THE RUNNER "B" IS EXECUTED.               80769000
          2.   THE RUNNER LOOP MAKES A COMPLETE CYCLE THRU ALL          80771000
               LOGGED-IN USERS STARTING WITH THE CURRENT VALUE OF USER, 80773000
               STEPPING UP TO MAXUSER, RETURNS TO 0, THEN STOPS WHEN    80775000
               USER = CYCLE.                                            80777000
          3.   FOR EACH RUNNER A CHECK IS MADE FOR READ/WRITE INTERLOCK 80779000
               OR IDLE.                                                 80781000
               4.   USER ACCOUNTING IS TURNED ON.                       80783000
               5.   A FUNCTION IS PERFORMED  (S, E, L ETC.).            80785000
               6.   IF A FUNCTION BRANCHES TO WRITER, ONE LINE OF       80787000
                    OUTPUT IS QUEUED UP.                                80789000
               7.   IF A FUNCTION BRANCHES TO ENDRUN, A CHANGE TO       80791000
                    INACTIVE IS HANDLED BY TYPING A ":".                80793000
          8.   EACH RUNNER TERMINATES AT ENDLOOP,                       80795000
          9.   A ROUND OF I/O IS DONE BY CALLING DCIO.                  80797000
         10.   IF AN INPUT LINE IS ENCOUNTERED, A BRANCH TO NEXT IS     80799000
               MADE.                                                    80801000
              11.   NEXT PERFORMS THE FOLLOWING:                        80803000
                    A.  A SCANNER CALL TO PRODUCE THE SYNTAX ARRAY      80805000
                    B.  AN ACCEPT OR TRANSLATOR CALL.                   80807000
                    C.  UPDATE IDENTIFIERS IF ALL IS WELL.              80809000
                    D.  TAKE ACTION FOR LOAD-LIST.                      80811000
                    E.  ENTER RUNNERS LOOP.                             80813000
;                                                                       80815000
          IF REALLOCATE THEN BEGIN                                      80817000
               REALLOCATE ~ FALSE;                                      80819000
               GO TO NEXT;                                              80821000
          END REALLOCATION AND RECOMPILATION ELSE BEGIN                 80823000
               DO ACTIVE ~ DCIO UNTIL RUNNER[0] ! 0;                    80825000
               GO TO RUNNERS;                                           80827000
               END ATTACHING FIRST USER;                                80829000
NEXT:     ENTERDICT ~ FALSE;                                            80831000
          CHARGETHEUSER;                                                80833000
         IF INFO[USER,ALPHACCEPT]=0 THEN BEGIN                          80835000
          IF I ~ SCANNER(INFO[USER,*], IDENTIFIERS[USER,*]) = 0         80837000
               THEN RUNNER[USER ] ~ "E" ELSE                            80839000
          IF I < 0 THEN ACCEPT(INFO[USER,*], IDENTIFIERS[USER,*],       80841000
               STACK[USER,*],USERARRAY[USER,*])                         80843000
          ELSE TRANSLATOR(INFO[USER,*], IDENTIFIERS[USER,*],            80845000
                         CODE[USER,*], USERARRAY[USER,*]);              80847000
         END ELSE BEGIN  COMMENT  ALPHABETIC READ;                      80849000
               MOVE9(INFO[USER,IOBUF], INFO[USER,ALTBUF]);              80851000
               INFO[USER,ALPHACCEPT]~0;                                 80853000
               IF STOP(INFO[USER,IOBUF]) THEN RUNNER[USER]~0 ELSE       80855000
                    RUNNER[USER]~"S";                                   80857000
               END ALPHAREAD;                                           80859000
          IF REALLOCATE THEN GO TO ALLOCATE;                            80861000
          T ~ INFO[USER,DICTMAX];                                       80863000
          IF ENTERDICT THEN BEGIN                                       80865000
               WHILE IDENTIFIERS[USER,T]!0 DO T~T+2;                    80867000
               INFO[USER, DICTMAX] ~ T;                                 80869000
               COMMENT  IF NEW SYMBOLS CAN BE ACCEPTED, THEN UPDATEIDS; 80871000
          END ELSE                                                      80873000
               WHILE IDENTIFIERS[USER,T]!0 DO                           80875000
                    IDENTIFIERS[USER,(T~T+2)-2]~0;                      80877000
               COMMENT  DELETE NEW SYMBOLS FROM ID ARRAY;               80879000
          IF USER = LOADINTERLOCK THEN BEGIN                            80881000
               IF RUNNER[USER].[36:12]!"I"                              80883000
               THEN BEGIN                                               80885000
                    CLOSE(LOADFILE);                                    80887000
                    LOADINTERLOCK~-1;                                   80889000
                    INFO[USER,LOADLIST]~0;                              80891000
                    MOVE9(SYNTAX[0],INFO[USER,IOBUF]);                  80893000
                    CYCLE~USER;                                         80895000
                    GO TO WRITER;                                       80897000
                    END ERROR IN LOADING;                               80899000
               IF INFO[USER, LOADLIST] = 1 THEN BEGIN                   80901000
               MOVE9(SYNTAX[0], INFO[USER,IOBUF]);                      80903000
               CYCLE ~ USER;                                            80905000
               GO TO WRITER;  END LOAD AND LIST;                        80907000
               IF INFO[USER,FILELIMITS].[45:3]!0 THEN GO TO RUNNERS;    80909000
               COMMENT  TIME SLICE EVERY 8 CARDS ON A LOAD;             80911000
               ACTIVE~TRUE;  CYCLE~USER;  GO TO ENDLOOP;                80913000
               END LOAD IN PROGRESS;                                    80915000
          IF RUNNER[USER] = 0  THEN BEGIN CYCLE ~ USER;  GO TO ENDRUN;  80917000
               END THE RUNNER CHANGE TO IDLE;                           80919000
          IF RUNNER[USER] = "R" THEN IF USER~USER+1>MAXUSER THEN USER~0;80921000
RUNNERS:  CYCLE ~ USER;                                                 80923000
          ACTIVE ~ FALSE;  COMMENT   FLAG FOR ANY ACTIVE RUNNERS;       80925000
          IF DEBUG THEN WRITE (PR[ DBL], MAX, RUNNER[*]);               80927000
          DO BEGIN                                                      80929000
               IF T ~ RUNNER[USER] > 0 AND T < 9999 THEN BEGIN          80931000
                    CHARGETHEUSER;                                      80933000
                    IF T = "S" THEN BEGIN  EXECUTECODE(CODE[USER, *],   80935000
                         IDENTIFIERS[USER,*], INFO[USER,*],             80937000
                         USERARRAY[USER,*], STACK[USER,*]);             80939000
                         GO TO ENDRUN;  END EXECUTE;                    80941000
                    IF T = "E" THEN BEGIN                               80943000
                         GETERRMESSAGE;                                 80945000
                         GO TO WRITEANDCLEAR;                           80947000
                         END LOADING AN ERROR MESSAGE FROM THE DISK;    80949000
                    IF T = "EL" THEN BEGIN                              80951000
                         GETERRMESSAGE;                                 80953000
                         RUNNER[USER] ~ "L" ;                           80955000
                         GO TO WRITER;                                  80957000
                         END ERR MSG AND CARD LIST;                     80959000
                    IF T = "P" THEN BEGIN                               80961000
                         RUNNER[USER] ~ "S";                            80963000
                         GO TO WRITER;  END PRINT;                      80965000
                    IF T = "L" THEN BEGIN                               80967000
                         T1 ~ (T3~CODE[USER, INFO[USER,CBASEINDX]]).    80969000
                              CDISKADDR;                                80971000
                         IF DISKPOINTER DIV 3! T1 DIV 3 THEN BEGIN      80973000
                         IF (T2 ~ T1 DIV 3) ! LISTPOINTER THEN          80975000
                              READ(DSK[LISTPOINTER ~ T2], 30, LISTBUF[*]80977000
                              );   COMMENT  READ ENTRY FROM DISK     ;  80979000
                              MOVE9(LISTBUF[T1 MOD 3 | 10],             80981000
                                   INFO[USER,IOBUF]);                   80983000
                              END ELSE MOVE9(DISKBUF[T1 MOD 3 | 10],    80985000
                                   INFO[USER, IOBUF]);                  80987000
                         IF T ~ T3.CLINK = 0 THEN RUNNER[USER] ~ 0      80989000
                                   ELSE INFO[USER,CBASEINDX] ~ T;       80991000
                         IF T3.CPARTNUMB!CODE[USER,T]. CPARTNUMB THEN   80993000
                              SPACEONE(INFO[USER,IOBUF]);               80995000
                         GO TO WRITER;                                  80997000
                         END LIST OPTION;                               80999000
                    IF T = "H" THEN BEGIN                               81001000
                         I ~ INFO[USER, HELPLINK].[38:10];              81003000
                         IF T3 ~INFO[USER,HELPLINK].CPARTNUMB!0         81005000
                              THEN T1 ~ HELPTABLE[I] ELSE               81007000
                         WHILE (T1~HELPTABLE[I]).CSTEPNUMB ! 0 AND      81009000
                              T1.CPARTNUMB > 1 DO I~I+1;                81011000
                         IF T2~T1.CPARTNUMB=99 AND T3=0 THEN BEGIN      81013000
                              RUNNER[USER] ~ 0;                         81015000
                              GO TO ENDRUN; END;                        81017000
                         IF T3 ~ INFO[USER, HELPLINK].CPARTNUMB = 0     81019000
                              OR T3 = T2 THEN BEGIN                     81021000
                              IF (T2~T1.CDISKADDR DIV 3) ! LISTPOINTER  81023000
                              THEN READ(DSK[LISTPOINTER~T2], 30,        81025000
                                   LISTBUF[*]);                         81027000
                              MOVE9(LISTBUF[T1.CDISKADDR MOD 3 | 10],   81029000
                                   INFO[USER, IOBUF]);                  81031000
                              INFO[USER, HELPLINK].[38:10] ~ I+1;       81033000
                              GO TO WRITER;                             81035000
                         END ELSE RUNNER[USER] ~ 0;                     81037000
                         GO TO ENDRUN;                                  81039000
                         END HELP;                                      81041000
                    IF T = "K" THEN BEGIN  COMMENT  KEYWORD FROM HELP;  81043000
                         T2 ~ INFO[USER, HELPLINK].[36:12];             81045000
                         I ~ INFO[USER, HELPLINK].[18:15];              81047000
                         WHILE (T1~HELPTABLE[I]).[36:12] ! T2 DO        81049000
                                   IF T1.CPARTNUMB = 99 THEN BEGIN      81051000
                              RUNNER[USER] ~ 0;                         81053000
                              GO TO ENDRUN;                             81055000
                         END ELSE I ~ I+1;                              81057000
                         IF (T2~T1.CDISKADDR DIV 3) ! LISTPOINTER       81059000
                              THEN READ(DSK[LISTPOINTER~T2], 30,        81061000
                              LISTBUF[*]);                              81063000
                         MOVE9(LISTBUF[T1.CDISKADDR MOD 3 | 10] ,       81065000
                              INFO[USER,IOBUF]);                        81067000
                         INFO[USER, HELPLINK].[18:15] ~ I+1;            81069000
                         GO TO WRITER;                                  81071000
                         END KEY WORD HELPER;                           81073000
                    IF T = "I" THEN BEGIN                               81075000
                         IF LOADINTERLOCK<0 THEN LOADINTERLOCK~USER     81077000
                         ELSE IF USER!LOADINTERLOCK THEN BEGIN          81079000
                              INFO[LOADINTERLOCK,LOADLIST]~0;           81081000
                              IF WAITMSG(INFO[USER,IOBUF]) THEN         81083000
                              GO TO ENDRUN ELSE GO TO WRITER;           81085000
                              END LOAD FILE CONFLICT;                   81087000
                         IF LOADER(INFO[USER,*]) THEN GO TO NEXT        81089000
                         ELSE BEGIN MSG("L",INFO[USER,IOBUF]);          81091000
                         IF INFO[USER,FILELIMITS] = 0 THEN MSG("N",     81093000
                         INFO[USER,IOBUF]); COMMENT NO FILE PRESENT;    81095000
                         LOADINTERLOCK~-1;                              81097000
                         INFO[USER,LOADLIST]~0;                         81099000
                         GO TO WRITEANDCLEAR; END END LOADER;           81101000
                    IF T = "B" THEN BEGIN                               81103000
                         INFO[USER,CMAX]~1;  COMMENT SET CODE INDEX;    81105000
                         FOR I ~ 0 STEP 1 UNTIL MAXUSER DO              81107000
                         IF USER!I THEN IF USERID[I]=USERID[USER] THEN  81109000
                              BEGIN RUNNER[USER]~"Q";                   81111000
                              MSG("D", INFO[USER,IOBUF]);               81113000
                              GO TO WRITER;  END DUPLICATE USER ID;     81115000
                         IF NOT DIRECTORY(USERID[USER],T1,T2) THEN BEGIN81117000
                              COMMENT NOT A RESTART;                    81119000
                              MSG("B",INFO[USER,IOBUF]);                81121000
                              GO TO WRITEANDCLEAR;                      81123000
                              END HANDSHAKE;                            81125000
                         READ(DSK[T1 DIV 3],30,LOADBUF[*]);             81127000
                         IF LOADBUF[(T1 MOD 3 | 10)+9]!T1 THEN BEGIN    81129000
                              COMMENT NO CARDS ON DISK;                 81131000
                              MSG("B",INFO[USER,IOBUF]);                81133000
                              GO TO WRITEANDCLEAR;                      81135000
                              END HANDSHAKE;                            81137000
                         INFO[USER,LOADLINK]~INFO[USER,FILELIMITS]~T1;  81139000
                         INFO[USER,FILEID]~T1&(T6|32+T7)[3:18:30];      81141000
                         RUNNER[USER]~"R";                              81143000
                         MSG("R",INFO[USER,IOBUF]);                     81145000
                         READ(DSK[T6],30,LOADBUF[*]);                   81147000
                         LOADBUF[T7].SBIT~1;                            81149000
                         COMMENT  CLOSE FILE DURING H/L RECOVERY;       81151000
                         WRITE(DSK[T6],30,LOADBUF[*]);                  81153000
                         IF DEBUG THEN WRITE(PR[DBL],17,LOADBUF[*]);    81155000
                         GO TO WRITER;                                  81157000
                         END HANDSHAKE OR INITIALIZING A RESTART;       81159000
                    IF T = "R" THEN BEGIN LABEL NXT, NXT1;              81161000
                         NXT:                                           81163000
                         IF T~INFO[USER,LOADLINK]}DISKPOINTER THEN BEGIN81165000
                              MSG("C",INFO[USER,IOBUF]);                81167000
                              RUNNER[USER]~"RL";                        81169000
                              READ(DSK[T1~INFO[USER,FILEID].[3:25]],    81171000
                                   30,LOADBUF[*]);                      81173000
                              LOADBUF[INFO[USER,FILEID].[28:5]].SBIT~0; 81175000
                              COMMENT  REOPEN FILE AFTER SUCCESS;       81177000
                              WRITE(DSK[T1],30,LOADBUF[*]);             81179000
                              GO TO WRITER;                             81181000
                         END RESTARTING PROGRAM ELSE                    81183000
                         BEGIN                                          81185000
                              READ(DSK[T DIV 3],30,LOADBUF[*]);         81187000
                              NXT1:                                     81189000
                              IF LOADBUF[(T1~T MOD 3|10)+9]=INFO[USER,  81191000
                                   FILELIMITS].[33:15] THEN BEGIN       81193000
                                   INFO[USER,FILELIMITS].[18:15]~T;     81195000
                                   MOVE9(LOADBUF[T1],SYNTAX[0]);        81197000
                                   INFO[USER,LOADLINK]~T+1;             81199000
                                   GO TO NEXT;                          81201000
                              END PASSING CARD IMAGE;                   81203000
                              IF T MOD 3 ! 2 THEN BEGIN T~T+1;          81205000
                                   GO TO NXT1;  END CHECKING WHOLE SEG; 81207000
                         END;                                           81209000
                         INFO[USER,LOADLINK]~T+1;                       81211000
                         GO TO NXT; END LOADING A CARD IMAGE;           81213000
                    IF T = "RL" THEN BEGIN                              81215000
                         READ(DSK[(T~INFO[USER,FILELIMITS].[18:15])     81217000
                              DIV 3],30,LOADBUF[*]);                    81219000
                         MOVE9(LOADBUF[T MOD 3 | 10],INFO[USER,IOBUF]); 81221000
                         GO TO WRITEANDCLEAR;                           81223000
                         END LISTING LAST CARD AFTER A RESTART;         81225000
                    IF T = "D" THEN BEGIN                               81227000
                         IF SAVEINTERLOCK<0 THEN BEGIN                  81229000
                              FILL SAVER WITH INFO[USER,FILENAME];      81231000
                              SEARCH(SAVER,INFO[USER,*]);               81233000
                              IF INFO[USER,0] } 0 THEN BEGIN            81235000
                                   MSG("F", INFO[USER,IOBUF]);          81237000
                                   GO TO WRITEANDCLEAR;                 81239000
                                   END DUP FILE NAME;                   81241000
                              SAVEINTERLOCK ~ USER;  END                81243000
                         ELSE IF USER!SAVEINTERLOCK THEN GO TO ENDRUN;  81245000
                         T1 ~ (T3~CODE[USER,INFO[USER,CBASEINDX]]).     81247000
                              CDISKADDR;                                81249000
                         IF DISKPOINTER DIV 3 ! T1 DIV 3 THEN BEGIN     81251000
                              IF (T2~T1 DIV 3) ! LISTPOINTER THEN       81253000
                              READ(DSK[LISTPOINTER~T2], 30, LISTBUF[*]);81255000
                              MOVEG(LISTBUF[T1 MOD 3 | 10], SAVEBUF[0]);81257000
                         END ELSE MOVEG(DISKBUF[T1 MOD 3 |10],          81259000
                              SAVEBUF[0]);                              81261000
                         SETSEQ(SAVEBUF[9], T3.CPARTNUMB, T3.CSTEPNUMB);81263000
                         WRITE (SAVER, 10, SAVEBUF[*]);                 81265000
                         IF T ~ T3.CLINK = 0 THEN BEGIN                 81267000
                              LOCK(SAVER);                              81269000
                              MSG("S", INFO[USER,IOBUF]);               81271000
                              SAVEINTERLOCK ~ -1;                       81273000
                              IF INFO[USER,PUBLICFLAG]=1 THEN BEGIN     81275000
                                   PUBLICSAVE(SAVEBUF[0],INFO[USER,     81277000
                                        FILENAME]);                     81279000
                                   ZIP WITH SAVEBUF[*];                 81281000
                                   END MAKING PUBLIC FILE;              81283000
                              GO TO WRITEANDCLEAR;                      81285000
                         END LAST CARD ELSE INFO[USER,CBASEINDX] ~ T;   81287000
                         GO TO ENDRUN;                                  81289000
                         END SAVING CARDS ON DISK;                      81291000
                    IF T = "Q" THEN BEGIN                               81293000
                         IF INFO[USER,CMAX] ! 0 THEN                    81295000
                              BEGIN                                     81297000
                              INFO[USER,CMAX] ~ 0;                      81299000
                              EOJMESSAGE(INFO[USER,IOBUF],(T1~TIME(1)/  81301000
                              3600) DIV 60,ENTIER(T1 MOD 60));          81303000
                              GO TO WRITER; %TYPE EOJ MESSAGE           81305000
                              END RESPONSE TO QUIT;                     81307000
                         READ(DSK[T1~INFO[USER,FILEID].[3:25]],30,      81309000
                              LISTBUF[*]);                              81311000
                              LISTPOINTER~0;                            81313000
                         IF T~INFO[USER,FILEID].[28:5]!0 THEN LISTBUF[  81315000
                              T]~-1;                                    81317000
                              COMMENT SET USERID NEGATIVE TO INDICATE   81319000
                                FILE CLOSED;                            81321000
                         WRITE(DSK[T1],30,LISTBUF[*]);                  81323000
                         RELEASE(DCTUBUF[USER]);                        81325000
                         RUNNER[USER]~USERID[USER]~DCTUBUF[USER]~0;     81327000
                         ACTIVE ~ TRUE;                                 81329000
                         MAXUSER ~ MAX;                                 81331000
                         WHILE DCTUBUF[MAXUSER] = 0 AND MAXUSER > 0 DO  81333000
                              MAXUSER ~ MAXUSER-1;                      81335000
                         FOR I ~ 0 STEP 1 UNTIL MAXUSER DO              81337000
                         IF USERID[I]!0 THEN BEGIN CYCLE~I;             81339000
                              GO TO ENDLOOP; END;                       81341000
                         GO TO FINISH;  COMMENT  XIT FROM SCHEDULER;    81343000
                         END QUIT ROUTINE;                              81345000
               END THE RUNNER WAS ACTIVE ELSE GO TO ENDLOOP;            81347000
ENDRUN:   IF RUNNER[USER] = 0 THEN BEGIN                                81349000
               SIGNAL(INFO[USER,IOBUF]);                                81351000
               RUNNER[USER].SBIT ~ 1;                                   81353000
               GO TO ENDLOOP;                                           81355000
          END THE RUNNER CHANGED TO INACTIVE ELSE BEGIN                 81357000
               IF RUNNER[USER].SBIT = 0 THEN                            81359000
               ACTIVE ~ TRUE;                                           81361000
               GO TO ENDLOOP;                                           81363000
          END THE RUNNER IS STILL ACTIVE;                               81365000
WRITEANDCLEAR: RUNNER[USER] ~ 0;                                        81367000
WRITER:   CARRIAGE(INFO[USER, IOBUF], REAL(RUNNER[USER].[36:12] = 0));  81369000
          IF DEBUG THEN WRITE(PR[DBL],9,INFO[USER,*]);                  81371000
          RUNNER[USER].SBIT ~ 1;                                        81373000
          IF RUNNER[USER].[36:12] ! 0 THEN ACTIVE ~ TRUE;               81375000
ENDLOOP:  IF USER ~ USER+1 > MAXUSER THEN USER ~ 0;                     81377000
          END UNTIL USER = CYCLE;                                       81379000
COMMENT   **********  END OF RUNNER SELECTION LOOP  *********;          81381000
          IF DCIO THEN GO TO NEXT;                                      81383000
          IF ACTIVE THEN GO TO RUNNERS ELSE                             81385000
               DO  IDLE ~ TRUE UNTIL DCIO OR ACTIVE;                    81387000
          IF ACTIVE THEN GO TO RUNNERS ELSE GO TO NEXT;                 81389000
ALLOCATE: COMMENT  EXIT BLOCK TO RE-ALLOCATE ARRAYS;                    81391000
END SCHEDULER;                                                          81393000
% **********************************************************************90001000
STREAM PROCEDURE STRTMSG(D, QMARKS, TF);  VALUE QMARKS, TF;             90003000
          BEGIN                                                         90005000
          DI~D;  DS~12 LIT "PLEASE TRY{!";  SI ~ LOC QMARKS;  SI~SI+6;  90007000
          DS~2 CHR;  DS~11 LIT "RUN WIPL{!~";                           90009000
          DI~D;  TF(DS~18 LIT "PLEASE WAIT....{!~" ); END STREAM;       90011000
PROCEDURE STARTUPMSG(TF);  VALUE TF;  BOOLEAN TF;                       90013000
          BEGIN                                                         90015000
          IF STATUS(DCBUF[*]) { 0 THEN GO TO ENDEND;                    90017000
               COMMENT   IF THERE ARE NO REMOTE USERS, THEN QUIT;       90019000
          STRTMSG(DCBUF[1], 12&12[36:42:6], TF);                        90021000
          IF DCBUF[0]!0 THEN WRITE(DCO1(DCBUF[0],0),DCWDS,DCBUF[*]);    90023000
          END STARTUP MSG;                                              90025000
     BOOLEAN STREAM PROCEDURE ENDOFILE(S);                              90027000
     BEGIN LOCAL T; LABEL BAD,GOOD;                                     90029000
     COMMENT IF THE FLAGBIT IS ON IN WORD 9, OR IF WORD 0 = "EOF" OR    90031000
          WORD 9="NUL", OR IF THERE IS NO "~" IN THE FIRST 9 WORDS THEN 90033000
          RETURN TRUE TO INDICATE THE END OF THE LOCAL FILE;            90035000
          SI ~ S;                                                       90037000
          DI~LOC T;  DS~8 LIT "00000EOF"; DI~LOC T;                     90039000
          IF 8 SC=DC THEN GO TO BAD;                                    90041000
          SI~SI+40;  SI~SI+24;  IF SB THEN GO TO BAD;                   90043000
          DI~LOC T;  DS~8 LIT "00000NUL";  DI~LOC T;                    90045000
          IF 8 SC=DC THEN GO TO BAD;                                    90047000
          GO TO GOOD;                                                   90049000
     BAD: TALLY~1; ENDOFILE~TALLY;                                      90051000
     GOOD:END ENDOFILE;                                                 90053000
% ********************************************************************  90055000
% ********************************************************************  90057000
% ********************** I N I T I A L   C O D E  ********************  90059000
% ********************************************************************  90061000
% ********************************************************************  90063000
FILL DICT[*] WITH                                                       90065000
COMMENT       CLASSIFICATIONS PRODUCED BY SCAN                          90067000
                  NUMB.      CODE NAME       MEANING                    90069000
                   **        *********       *******                    90071000
                   0         ENDR      END OF STATEMENT                 90073000
                   1         CONV      CONVERSATIONAL-ONLY STATEMENT    90075000
                   2         EITHER    CONVERSATIONAL OR STORED PROGRAM 90077000
                   3         PROG      STORED PROGRAM CONTROL WORD      90079000
                   4         LPAREN    LEFT PAREN, LEFT BRACKET         90081000
                   5         IDENT     IDETIFIER                        90083000
                   6         ARRAYID   ARRAY IDENTIFIER                 90085000
                   7         CON       CONSTANT                         90087000
                   8         ADSUB     ADD, SUBTRACT OPERATOR           90089000
                   9         RPAREN    RIGHT PAREN, RIGHT BRACKET       90091000
                   10        OPR       OPERATOR                         90093000
                   11        FUNC      FUNCTION NAME                    90095000
                   13        REL       RELATIONAL OPERATOR              90097000
                   14        SEP       SEPARATOR                        90099000
                             %     NAME          CLASS     KEY          90101000
                             %     ****          *****     ***          90103000
     ; 0,                                                               90105000
     "4SAVE011",             %     4SAVE         CONV      1            90107000
     "4LOAD012",             %     4LOAD         CONV      2            90109000
     "4QUIT013",             %     4QUIT         CONV      3            90111000
     "3RUN0014",             %     3RUN          CONV      4            90113000
     "5ERASE15",             %     5ERASE        CONV      5            90115000
     "3ZAP0015",             %     3ZAP          CONV      5            90117000
     "6DELET15",             %     6DELET        CONV      5            90119000
     "4LIST016",             %     4LIST         CONV      6            90121000
     "4HELP017",             %     4HELP         CONV      7            90123000
     "5PURGE18",             %     5PURGE        CONV      8            90125000
     "6REMOV18",             %     6REMOV        CONV      8            90127000
     "3SET0020",             %     3SET          EITHER    0            90133000
     "3LET0020",             %     3LET          EITHER    0            90135000
     "4READ021",             %     4READ         EITHER    1            90137000
     "5INPUT21",             %     5INPUT        EITHER    1            90139000
     "6ACCEP21",             %     6ACCEP        EITHER    1            90141000
     "6DEMAN21",             %     6DEMAN        EITHER    1            90143000
     "4TYPE022",             %     4TYPE         EITHER    2            90145000
     "5PRINT22",             %     5PRINT        EITHER    2            90147000
     "5WRITE22",             %     5WRITE        EITHER    2            90149000
     "3REM0023",             %     3REM          EITHER    3            90151000
     "6REMAR23",             %     6REMAR        EITHER    3            90153000
     "7COMME23",             %     7COMME        EITHER    3            90155000
     "3USE0024",                                                        90155100
     "5CLEAR25",                                                        90155200
     "2GO00030",             %     2GO           PROG      0            90157000
     "4STOP031",             %     4STOP         PROG      1            90159000
     "2DO00032",             %     2DO           PROG      2            90161000
     "5WHILE33",             %     5WHILE        PROG      3            90163000
     "5UNTIL34",             %     5UNTIL        PROG      4            90165000
     "9DIMEN35",             %     9DIMEN        PROG      5            90167000
     "3DIM0035",             %     3DIM          PROG      5            90169000
     "7DECLA35",             %     7DECLA        PROG      5            90171000
     "5ARRAY35",             %     5ARRAY        PROG      5            90173000
     "2IF00036",             %     2IF           PROG      6            90175000
     "4FORM037",             %     4FORM         PROG      7            90177000
     "6FORMA37",             %     6FORMAT       PROG      7            90179000
% "4DATA038" NOT A RESERVED WORD                                        90181000
% "7RESTO39"   NOT A RESERVED WORD                                      90183000
     "1(000040",             %     1(            LPAREN    0            90185000
     "1[000041",             %     1[            LPAREN    1            90187000
     "1+000080",             %     1+            ADSUB     0            90189000
     "1-000081",             %     1-            ADSUB     1            90191000
     "1)000090",             %     1)            RPAREN    0            90193000
     "1]000091",             %     1]            RPAREN    1            90195000
     "1|0000#0",             %     1|            OPR       0            90197000
     "1*0000#0",             %     1*            OPR       0            90199000
     "1/0000#1",             %     1/            OPR       1            90201000
     "1**000#2",             %     1**           OPR       2            90203000
     "3DIV00#3",             %     3DIV          OPR       3            90205000
     "3MOD00#4",             %     3MOD          OPR       4            90207000
     "2IP000@0",             %     2IP           FUNC      0            90209000
     "3INT00@0",             %     3INT          FUNC      0            90211000
     "3LOG00@1",             %     3LOG          FUNC      1            90213000
     "2LN000@1",             %     2LN           FUNC      1            90215000
     "3SIN00@2",             %     3SIN          FUNC      2            90217000
     "4SINF0@2",             %     4SINF         FUNC      2            90219000
     "3COS00@3",             %     3COS          FUNC      3            90221000
     "4COSF0@3",             %     4COSF         FUNC      3            90223000
     "4TANF0@4",             %     4TANF         FUNC      4            90225000
     "3TAN00@4",             %     3TAN          FUNC      4            90227000
     "3ABS00@5",             %     3ABS          FUNC      5            90229000
     "3EXP00@6",             %     3EXP          FUNC      6            90231000
     "4EXPF0@6",             %     4EXPF         FUNC      6            90233000
     "4RAND0@7",             %     4RAND         FUNC      7            90235000
     "4ATAN0@8",             %     4ATAN         FUNC      8            90237000
     "3ATN00@8",             %     3ATN          FUNC      8            90239000
     "3SQR00@9",             %     3SQR          FUNC      9            90241000
     "4SQRT0@9",             %     4SQRT         FUNC      9            90243000
     "4SIGN0@#",             %     4SIGN         FUNC      10           90245000
     "5LOG10@@",             %     5LOG10        FUNC      11           90247000
     "6COLUM@:",             %     6COLUMN       FUNC      13           90249000
     "4SCAN0@>",             %     4SCAN         FUNC      14           90251000
%    "4PLOT0@B"     NOT A RESERVED WORD                                 90252300
%    "3TAB00@C"     NOT A RESERVED WORD                                 90252400
%    "6REPEA@D"     NOT A RESERVED WORD                                 90252500
     "1=0000:0",             %     1=            REL       0            90253000
     "1:=000:0",             %     1:=           REL       0            90255000
     "2EQ000:0",             %     2EQ           REL       0            90257000
     "3EQU00:0",             %     3EQU          REL       0            90259000
     "3EQL00:0",             %     3EQL          REL       0            90261000
     "1!0000:1",             %     1!            REL       1            90263000
     "2NE000:1",             %     2NE           REL       1            90265000
     "3NEQ00:1",             %     3NEQ          REL       1            90267000
     "1>0000:2",             %     1>            REL       2            90269000
     "2GT000:2",             %     2GT           REL       2            90271000
     "3GTR00:2",             %     3GTR          REL       2            90273000
     "1<0000:3",             %     1<            REL       3            90275000
     "2LT000:3",             %     2LT           REL       3            90277000
     "3LSS00:3",             %     3LSS          REL       3            90279000
     "1}0000:4",             %     1}            REL       4            90281000
     "2GE000:4",             %     2GE           REL       4            90283000
     "3GEQ00:4",             %     3GEQ          REL       4            90285000
     "1{0000:5",             %     1{            REL       5            90287000
     "2LE000:5",             %     2LE           REL       5            90289000
     "3LEQ00:5",             %     3LEQ          REL       5            90291000
     "1,0000>0",             %     1,            MISC      0            90293000
     "2BY000>1",             %     2BY           MISC      1            90295000
     "4ELSE0>2",             %     4ELSE         MISC      2            90297000
     "4THRU0>3",             %     4THRU         MISC      3            90299000
     "2TO000>4",             %     2TO           MISC      4            90301000
     "4THEN0>5",             %     4THEN         MISC      5            90303000
     "4STEP0>6",             %     4STEP         MISC      6            90305000
     "3YES00>7",             %     3YES          MISC      7            90307000
     "2AS000>8",             %     2AS           MISC      8            90309000
     "4PART0>#",             %     4PART         MISC      10           90311000
     "4TIME0>@",             %     4TIME         MISC      11           90313000
     "5TIMES>@",             %     5TIMES        MISC      11           90315000
     "3FOR00>:",             %     3FOR          MISC      13           90317000
     "4FROM0>>",             %     4FROM         MISC      14           90319000
     "2NO000>}",             %     2NO           MISC      15           90321000
     "4DISK0>+",             %     4DISK         MISC      16           90323000
0;   COMMENT   FIRST AND LAST WORD MUST BE ZERO;                        90325000
FILL STACK[*] WITH ":0",0,"99",1,"80",2,"81",2,"#5",2,"#0",3,"#1",3,    90327000
     "#3",3,"#4",3,"#2",4,"90",5;                                       90329000
COMMENT "99" IS MARKSTACK;                                              90331000
IF REAL(DEBUG) > 1 THEN FILL DSK WITH "TEMP";                           90333000
     SEARCH(DSK,INFO[0,*]);  COMMENT LOOK AT DISK DIRECTORY;            90335000
IF INFO[0,6] } 1 THEN BEGIN                                             90337000
          STARTUPMSG(FALSE);  GO TO ENDEND;  END SAVFILE IS BEING USED; 90339000
IF INFO[0,0]}0 THEN BEGIN   READ (DSK[0],30,DISKBUF[*]);                90341000
          IF DISKBUF[2]>TIME(1) THEN BEGIN COMMENT  IF FILE IS PRESENT  90343000
               AND TIME OF LAST USE GREATER THAN PRES TIME, THEN REMAKE;90345000
               INFO[0,0]~-1;  CLOSE(DSK,PURGE);  WHEN(1);  END DAILY PG;90347000
               END POSSIBLY RE CREATING WIPLS SAVFILE;                  90349000
IF INFO[0,0]<0 THEN                                                     90351000
% ---------------- C R E A T E  S A V E F I L E  ------------ -------   90353000
BEGIN                                                                   90355000
     COMMENT FILE NOT PRESENT;                                          90357000
     FILE MAKER DISK RANDOM[20:180]"WIPLS""SAVFILE"(1,30,30,SAVE 999);  90359000
     DEFINE ERRORS = LOADFILE#;                                         90361000
     LABEL EOF;                                                         90363000
STREAM PROCEDURE FORMATERRORS(S,S1,D);                                  90365000
BEGIN LOCAL T;                                                          90367000
     SI~S1;  SI~SI-1;                                                   90369000
     63(IF SC ! " " THEN JUMP OUT ELSE SI~SI-1);                        90371000
     SI~SI+1;  T~SI;  DI~T;  DS~LIT "~";                                90373000
     SI~S;  15(IF SC = "M" THEN JUMP OUT ELSE SI~SI+1);                 90375000
     SI~SI+2;  DI~D;  DS~63 CHR;                                        90377000
END FORMATTING ERRORS;                                                  90379000
          COMMENT OPEN AND CLOSE FILE IN THIS BLOCK;                    90381000
     IF REAL(DEBUG) > 1 THEN FILL MAKER WITH "TEMP";                    90383000
     STARTUPMSG(TRUE);                                                  90385000
          COMMENT WRITE WAIT MESSAGE FOR FIRST USER;                    90387000
     DIRBUF[0]~"DIR";                                                   90389000
     DENTRY~3;                                                          90391000
     DISKBUF[0]~"EOF";                                                  90393000
     FOR I~1 STEP 1 UNTIL 10 DO WRITE(MAKER[I],30,SYNTAX[*]);           90395000
     FOR I~HELPARRAYSTART STEP 1 UNTIL ERRORSTART-1 DO WRITE(MAKER[I],  90397000
          30,HELPTABLE[*]);                                             90399000
     FOR I~FILESTART STEP 1 UNTIL FILESTART+12 DO WRITE(MAKER[I],30,    90401000
          DISKBUF[*]);                                                  90403000
     WRITE(MAKER[359],30,DISKBUF[*]);                                   90405000
          COMMENT WRITE LAST RECORD IN ROW;                             90407000
     DISKPOINTER~FILESTART|3;                                           90409000
     DIRBUF[3]~DISKPOINTER&DENTRY[13:43:5];                             90411000
     WRITE(MAKER[0],30,DIRBUF[*]);                                      90413000
          COMMENT INITIALIZE DISK AND RESERVE SEGMENT 0 FOR FIRST       90415000
               DIRECTORY ENTRY;                                         90417000
     LOCK(MAKER);                                                       90419000
     DO SEARCH(MAKER,INFO[0,*]) UNTIL INFO[0,0]}0;                      90421000
     FILL INFO[0,*] WITH "CC FREE ","WIPLS/SA","VFILE;EN","D.      ";   90423000
     IF REAL(DEBUG){1 THEN ZIP WITH INFO[0,*];                          90425000
     WHEN(1);                                                           90427000
     HELPLOADER;                                                        90429000
          COMMENT LOAD HELPFIL FROM DISK;                               90431000
     FILL LOADFILE WITH "ERRORS ";                                      90433000
     I~(ERRORSTART|3)-1;                                                90435000
     T1~-1;                                                             90437000
     SEARCH(LOADFILE,SYNTAX[*]);                                        90439000
     IF SYNTAX[0] < 0 THEN BEGIN  COMMENT  NO ERROR FILE IN DIRECTRY;   90441000
          DISKBUF[0]~DISKBUF[10]~DISKBUF[20]~"R~  "&"ERRO"[1:25:23];    90443000
          FOR T~ERRORSTART STEP 1 UNTIL ERRORSTART+MAXERRNUMBER/3 DO    90445000
               WRITE(DSK[T],30,DISKBUF[*]);                             90447000
     END NO FILE ELSE BEGIN                                             90449000
     WHILE TRUE DO BEGIN                                                90451000
          READ(ERRORS[T1~T1+1], 10, SYNTAX[*])[EOF];                    90453000
          FORMATERRORS(SYNTAX[0],SYNTAX[9],DISKBUF[T~(I~I+1)MOD 3|10]); 90455000
          IF T = 20 THEN WRITE(DSK[I DIV 3],30,DISKBUF[*]);             90457000
     END LOADING ERROR FILE;                                            90459000
EOF: IF T ! 20 THEN WRITE(DSK[I DIV 3],30,DISKBUF[*]);                  90461000
     CLOSE(ERRORS);                                                     90463000
     END FILE PRESENT;                                                  90465000
END INITIALIZING A CLEAN DISK ELSE                                      90467000
% -------------------- U S E  O L D   F I L E --------------------------90469000
BEGIN                                                                   90471000
     I~HELPARRAYSTART-1;                                                90473000
     DO BEGIN                                                           90475000
          READ(DSK[I~I+1],30,SYNTAX[*]);                                90477000
          T~(I-HELPARRAYSTART)|30;                                      90479000
          T2~-1;                                                        90481000
          FOR T1~T STEP 1 UNTIL T+29 DO HELPTABLE[T1]~SYNTAX[T2~T2+1];  90483000
     END UNTIL SYNTAX[0]=0 OR I=ERRORSTART-1;                           90485000
          COMMENT LOAD HELPTABLE;                                       90487000
     READ(DSK[0],30,DIRBUF[*]);                                         90489000
     RESTART~DIRBUF[4]!0;                                               90491000
          COMMENT  SELECT NORMAL/RESTART MODE;                          90493000
     IF T~DIRBUF[3]>0 THEN                                              90495000
     BEGIN                                                              90497000
          COMMENT NORMAL RESTART;                                       90499000
          DENTRY~T.[13:5];                                              90501000
          DLOC~T.[18:15];                                               90503000
          DISKPOINTER~T.[33:15];                                        90505000
          READ(DSK[DLOC],30,DIRBUF[*]);                                 90507000
          READ(DSK[DISKPOINTER DIV 3],30,DISKBUF[*]);                   90509000
          COMMENT  POINTERS AND BUFFERS ARE READY TO GO NOW;            90511000
     END HANDLING A NORMAL RESTART ELSE                                 90513000
% ---------------------- H A L T   L O A D   R E C O V E R Y ---------  90515000
     BEGIN                                                              90517000
          STARTUPMSG(TRUE);                                             90519000
               COMMENT WRITE WAIT MESSAGE;                              90521000
          DIRBUF[4]~-1;                                                 90523000
          DIRBUF[2]~TIME(1);  COMMENT  RECORD TIME OF DAY OF LAST USE;  90525000
          WRITE(DSK[0],30,DIRBUF[*]);                                   90527000
          COMMENT  SET ABORT FLAG IN SAVFILE;                           90529000
          DLOC~0;                                                       90531000
          DO READ(DSK[DLOC],30,DIRBUF[*]) UNTIL DLOC~DIRBUF[1]=0;       90533000
               COMMENT CHAIN THROUGH DIRECTORY SEGMENTS UNTIL LAST ONE; 90535000
          DENTRY~3;                                                     90537000
          RESTART~TRUE;                                                 90539000
          FOR I~0 STEP 1 UNTIL 9 DO                                     90541000
          IF DENTRY!27 THEN IF DIRBUF[DENTRY+3]!0 THEN DENTRY~DENTRY+3; 90543000
               COMMENT FIND LAST DIRECTORY ENTRY;                       90545000
          DISKPOINTER~IF DLOC=0 AND DENTRY=3 THEN FILESTART|3 ELSE      90547000
          DIRBUF[DENTRY+1];                                             90549000
               COMMENT PICK STARTING POINT TO SEARCH DIRECTORY;         90551000
          DO BEGIN                                                      90553000
               IF T~DISKPOINTER MOD 3 = 0 THEN READ(DSK[DISKPOINTER     90555000
                    DIV 3],30,DISKBUF[*]);                              90557000
               DISKPOINTER~DISKPOINTER+1;                               90559000
          END UNTIL ENDOFILE(DISKBUF[T|10]);                            90561000
          DISKPOINTER~DISKPOINTER-1;                                    90563000
     END SETTING POINTERS OF AN ABORT RESTART;                          90565000
END RESTART;                                                            90567000
          STNOMASK.CSNUMB ~ 131071;                                     90569000
          PTMASK.CPARTNUMB~127;                                         90571000
          UNDEFINED ~ "EEEE" &"EEEE" [1:25:23];                         90573000
          MAXCODESIZE~MAXUSERARRAY~22;  COMMENT  GUESS AT ARRAY SIZES;  90575000
          SAVEINTERLOCK~LOADINTERLOCK~-1;                               90577000
% ********************************************************************  90579000
     BEGIN LABEL NEWSEGMENT;                                            90581000
     WHILE TRUE DO SCHEDULER(MAXCODESIZE, MAXUSERARRAY);                90583000
     NEWSEGMENT:  END CALLING SCHEDULER AND REALLOCATING ARRAYS;        90585000
% ********************************************************************  90587000
FINISH:                                                                 90589000
     IF RESTART THEN                                                    90591000
     BEGIN                                                              90593000
          RESTART~FALSE;                                                90595000
          T1~0;                                                         90597000
          T2~6;                                                         90599000
          DO BEGIN                                                      90601000
               READ(DSK[T1],30,DIRBUF[*]);                              90603000
               DO BEGIN                                                 90605000
                    IF T3~DIRBUF[T2]=0 THEN                             90607000
                    BEGIN                                               90609000
                         WRITE(DSK[T1],30,DIRBUF[*]);                   90611000
                         GO TO FINISH1;                                 90613000
                    END;                                                90615000
                    IF T3>0 THEN                                        90617000
                    BEGIN                                               90619000
                         T3~TIME(0);                                    90621000
                         IF (T4~T3.[30:6]|100+T3.[36:6]|10+T3.[42:6])>  90623000
                              (T5~DIRBUF[T2+2].[39:9])+1 OR (T4<T5 AND  90625000
                              T4=2) OR T4!T5 THEN DIRBUF[T2].[1:2]~-3   90627000
                              ELSE RESTART~TRUE;                        90629000
                         COMMENT  PURGE EXPIRED FILE THE NEXT DAY;      90631000
                    END;                                                90633000
               END UNTIL T2~T2+3=30;                                    90635000
               WRITE(DSK[T1],30,DIRBUF[*]);                             90637000
               T2~3;                                                    90639000
          END UNTIL T1~DIRBUF[1]=0;                                     90641000
     END RESTART;                                                       90643000
FINISH1:                                                                90645000
     READ(DSK[0],30,DIRBUF[*]);                                         90647000
     IF NOT RESTART THEN                                                90649000
     BEGIN                                                              90651000
          DIRBUF[4]~0;                                                  90653000
          DIRBUF[3]~ENTIER(FILESTART|3)&3[13:43:5];                     90655000
          FOR I~5 STEP 1 UNTIL 29 DO DIRBUF[I]~0;                       90657000
          DISKBUF[0]~"EOF";                                             90659000
          FOR I~FILESTART STEP 1 UNTIL FILESTART+12 DO WRITE(DSK[I],    90661000
               30,DISKBUF[*]);                                          90663000
          COMMENT RESET POINTERS TO INDICATE A NULL FILE;               90665000
     END ELSE                                                           90667000
          DIRBUF[3]~DISKPOINTER&DLOC[18:33:15]&DENTRY[13:43:5];         90669000
     DIRBUF[2]~TIME(1);  COMMENT  RECORD TIME OF DAY OF LAST USE;       90671000
     WRITE(DSK[0],30,DIRBUF[*]);                                        90673000
ENDEND :                                                                90675000
END.                                                                    90677000
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
