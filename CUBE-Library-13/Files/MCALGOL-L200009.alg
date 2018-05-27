                                                                        00000000
                                                                        00000010
                                                                        00000020
                                                                        00000030
                                                                        00000040
                                                                        00000050
BEGIN                                                                   00000097
DEFINE LANGUAGELEVEL = 3#;                                              00000098
DEFINE DATE = "/16/68"&"01"[1:37:11]#;                                  00000099
COMMENT   SUMMARY OF PATCHES...................                         00000100
G01 GENERAL REPAIR AND IMPROVEMENTS                                 %G0100000101
;                                                                       00000999
$ INCLUDE 00004001, IF ADVANCED;                                        00003000
 $ SET INTERMEDIATE = TRUE;                                             00004000
INTEGER                                                                 00005000
        ERRCT,      % # OF SYNTAX ERRORS, "COMMON" ON STARTUP           00006000
        RCCNT;      % CURRENT INPUT IMAGE "LINE"                        00007000
$ INCLUDE 00008001, IF INTERMEDIATE;                                    00007999
REAL    CAT1,CAT2;  % FIXED LOCATIONS FOR ISOCAT                        00008000
$ INCLUDE 00015000, IF COUNTING;                                        00010000
ARRAY   COUNTING[0:19];                                                 00011000
DEFINE  ASCT        = COUNTING[ 0] #,                                   00011200
        BSCT        = COUNTING[ 1] #,                                   00011400
        SSCT        = COUNTING[ 2] #,                                   00011600
        ESCT        = COUNTING[ 3] #,                                   00011800
        STEPCT      = COUNTING[ 4] #,                                   00012000
        INDCT       = COUNTING[ 5] #,                                   00012200
        STKCT       = COUNTING[ 6] #,                                   00012400
        STKMX       = COUNTING[ 7] #,                                   00012600
        STKDP       = COUNTING[ 8] #,                                   00012800
        PSEU        = COUNTING[ 9] #,                                   00013000
        MSTCT       = COUNTING[10] #,                                   00013200
        PUTCT       = COUNTING[11] #,                                   00013400
        GETCT       = COUNTING[12] #,                                   00013600
        MOVCT       = COUNTING[13] #,                                   00013800
        PTRCT       = COUNTING[14] #,                                   00014000
        PTCHCT      = COUNTING[15] #;                                   00014050
BOOLEAN                                                                 00100000
$ INCLUDE 00100101, IF DEBUGGING;                                       00100099
        ACMLST,     % LISTING OF ACM REQUESTED ("ACCUM")                00100100
        ATOG,       % ARITHMETIC EXPRESSION TOGGLE                      00101000
        BLOCK,      % BLOCK LEVEL LISTING REQUESTED ("BLOCK")           00102000
        BOOLEANEXTOG,% COMPILING A BOOLEAN EXPRESSION                   00103000
        CCDATATOG,  % PROCESSING DATA SECTION OF CCHANDLER              00104000
        CCLISTOG,   % LISTING OF CONTROL DECK REQUESTED                 00105000
        CMT,        % PROCESSING A COMMENT                              00106000
        COND,       % COMPILING A CONDITIONAL STMT                      00107000
        CONVERTTOG, % INTERPRETIVE MSTRING CONVERSION REQUIRED          00108000
        DBLINE,     % DOUBLE SPACING REQUESTED ("DOUBLE")               00109000
$ INCLUDE 00110001, IF DEBUGGING;                                       00109999
        DBUGN,      % LISTING OF COMPILED CODE REQUESTED ("DEBUGN")     00110000
        DEC,        % PROCESSING A DECLARATION                          00111000
        DECOMTOG,   % COMPILING A DECOMPOSITION STMT                    00112000
        DJTOG,      % DECLARATION JUGGLING INTO OUTPUT STRING           00113000
        DOLLARSIGN, % "$" IMAGE GOING INTO OUTPUT STRING                00114000
        ENDT,       % PROCESSING AN END COMMENT                         00115000
        EQVTOG,     % NEED NAME OF A MACRO W/O INTERPRETATION           00116000
        EX,         % "EXPRESSION" BEING SCANNED BY ESCAN               00117000
        FIELDTOG,   % INTERPRETING AN MFIELD                            00118000
        FLUSHD,     % LAST OUTPUT STRING IMAGE FLUSHED: POB IS CLEAN    00119000
        FMTTOG,     % FORMAT BEING PROCESSED...SCAN DIFFERENTLY         00120000
        INDEXING,   % INDEX REQUESTED ("INDEX")                         00121000
        INTERLIST,  % OUTPUT STRING LISTING REQUESTED ("INTER")         00122000
        ITOG,       % INTEGER MSTRING BEING PROCESSED                   00123000
        LENGTHTOG,  % INTERPRETING PARTIAL MSTRING                      00124000
        LUOK,       % LASTUSED OK TO PRINT INPUT IMAGE                  00125000
        MACROTOG,   % COMPILING A MACRO DECLARATION                     00126000
$ INCLUDE 00127001, IF DEBUGGING;                                       00126999
        MONITER,    % INTERPRER MONITORING REQUESTED ("MONITOR")        00127000
        NEWFILE,    % NEW DISK FILE REQUESTED ("NEW DISK")              00128000
        NEWRESEQ,   % RESEQUENCING IN NEW DISK REQUESTED ("SEQ")        00129000
        NOJUMP,     % JUMP NOT ALLOWED FOLLOWING PREVIOUS SYLLABLE      00130000
        NOREAD,     % PREVENT SCANNING BEYOND END OF PSEUDOREADER       00131000
        PACKLAST,   % SETPACK CALLED LAST                               00131100
        PREV,       % PREVIOUS ITEM PACKED WAS A SPECIAL CHR            00132000
        PRINT,      % LISTING OF INPUT REQUESTED ("LIST")               00133000
        PROCTOG,    % PROCESSING PROCEDURE DECLARATION                  00134000
$ INCLUDE 00135001, IF DEBUGGING;                                       00134999
        PSEUDOTOG,  % MONITORING OF PSEUDORDRS REQUESTED("PSEUDO")      00135000
$ INCLUDE 00136001, IF INTERMEDIATE;                                    00135999
        PWT,        % PARTIAL WORD TOGGLE                               00136000
        RESCANTOG,  % ALLOWS INTERPRETATION OF SCANNED ITEMS            00137000
        SPCT,       % PROCESSING PROCEDURE SPECIFICATIONS PART          00138000
$ INCLUDE 00139001, IF DEBUGGING;                                       00138999
        SRT,        % ADDRESS ASSIGNMENTS REQUESTED ("PRT")             00139000
        STREAMTOG,  % PROCESSING A STREAM PROCEDURE                     00140000
        SVT,        % SAVE TOGGLE FOR LOCKING CODE FILE                 00141000
        TOGGLE,     % INTERPRETATION SUCCESS TOGGLE FOR MACROS          00142000
        TRACE,      % ENTRY-EXIT OF MACROS REQUESTED ("TRACE")          00143000
        UNPACK,     % UNPACKED OUTPUT STRING REQUESTED ("UNPACK")       00144000
$ INCLUDE 00144101, IF ADVANCED;                                        00144099
        USERTOG,    % USER TOGGLE FOR CONDITIONAL MACROS                00144100
        VOIDING,    % PROCESSING A MCALGOL $ VOID CARD                  00145000
        WARNING,    % WARNING MESSAGES HAVE OCCURRED IN PROGRAM         00146000
        WARNTOG,    % WARNING MESSAGES REQUESTED ("WARN")               00147000
        WRITEIT;    % A LISTING HAS BEEN PRODUCED                       00148000
ALPHA   CHAR,       % LAST SCANNED SPECIAL CHARACTER                    00149000
        DATANAME,   % TIME-DERIVED NAME FOR CC DATA FILES               00150000
        F,          % SUFFIX OF CODE FILE                               00151000
        LANG,       % NAME OF LANGUAGE FILE                             00152000
        MF,         % PREFIX OF CODE FILE                               00153000
        V,          % LAST GET VALUE                                    00154000
        W;          % LAST ELBAT WORD FROM INDICT                       00155000
INTEGER A,          % GLOBAL TEMP                                       00156000
        ALGOLDICT,  % POINTER TO START OF ALGOL RESWDS IN DICT          00157000
        BASE,       % SUBTRACTION BASE FOR SCANCOUNT                    00158000
        BC,         % BFR CHR POINTER                                   00159000
        BECT,       % SERIAL NUMBER FOR DECLARATION IDS                 00160000
        BELB,       % BASE POSITION OF PHYSICAL ELBAT ARRAY             00161000
        BFRLNG,     % LEFTMOST CHR POS OF CURRENT PSEUDOREADER          00162000
        BLOCKLEVEL, % CURRENT BLOCK NESTING DEPTH                       00163000
        BLVL,       % CURRENT BEGIN NESTING DEPTH                       00164000
        BNO,        % SERIAL NUMBER OF CURRENT "BEGIN" FOR MATCHING     00165000
        BNOMAX,     % NEXT BNO TO ASSIGN                                00166000
        BW,         % BFR WORD POINTER                                  00167000
        BWMAX,      % RIGHTMOST CHR POS OF CURRENT PSEUDOREADER         00168000
        CARDREC,    % RECORD # OF CURRENT CARD IMAGE                    00169000
        CCC,        % CONTROL CARD COUNTER (* CARDS)                    00170000
        CFIELD,     % COMPILE-TIME FIELD WORD                           00171000
        CIN,        % PSEUDOREADER STACKING INDEX                       00172000
        CODEREC,    % SERIAL # OF CODE FILE IMAGE                       00173000
        CONP,       % NEXT AVAILABLE WORD IN CONSTANT POOL              00174000
        COUNT,      % CHR COUNTER USED BY SCANNERS                      00175000
        DECP,       % INDEX INTO DECL                                   00176000
        DISKREC,    % RECORD # OF CURRENT DISK IMAGE                    00177000
        DOLLARDICT, % POINTER TO MCALGOL DOLLAR CARD WORDS              00178000
        DSTK,       % NESTING DEPTH OF DISKSTACK                        00178100
        ELB,        % ELBAT WORD OF CURRENTLY SCANNED ITEM              00179000
        EP,         % SYLLABLE OF MACRO CODE BEING INTERPRETED          00180000
        FC,         % FIRST CHR OF PARTIAL MSTRING(USUALLY 0)           00181000
        GADR,       % CURRENT INTERPRETING OPERATOR                     00182000
        GLAT,       % NEXT GLOBAL ALPHA MSTRING ADDRESS                 00183000
        GLIT,       % NEXT GLOBAL INTEGER MSTRING ADDRESS               00184000
        GLOT,       % NEXT GLOBAL MPOINTER ADDRESS                      00185000
        GOP,        % CURRENT INTERPRETING OPERATOR                     00186000
        HM,         % CHR COUNT (+3) OF ACCM                            00187000
        I,          % GLOBAL TEMP                                       00188000
        IACCM,      % INTEGER ACCUMULATOR                               00189000
        IACCM1,     % SIDE IACCM FOR RELATIONS                          00190000
        II,         % GLOBAL TEMP FOR PSEUDOREADING                     00191000
        INDENT,     % 3|RECURSION DEPTH OF MACROS FOR TRACING           00192000
        INXR,       % INDEX RECORD COUNTER                              00193000
        J,          % GLOBAL TEMP                                       00194000
        LACT,       % NEXT LOCAL ALPHA MSTRING ADDRESS                  00195000
        LAD,        % LAST ADINFO FIELD FROM INDICT                     00196000
        LASTDICT,   % LAST ASSIGNED POSITION IN DICT                    00197000
        LASTUSED,   % INPUT MEDIA SWITCH IN READACARD                   00198000
        LICT,       % NEXT LOCAL INTEGER MSTRING ADDRESS                00199000
        LIMT,       % LENGTH OF LAST INTEGER MSTRING                    00200000
        LOCT,       % NEXT LOCAL MPOINTER ADDRESS                       00201000
        LSTUDICT,   % LAST ASSIGNED POISTION IN UDICT                   00202000
        MACRODICT,  % POINTER TO MACRO RESWDS IN DICT                   00203000
        MACT,       % RECURSION DEPTH OF MACROS                         00204000
        MADDR,      % DICT ADDRESS OF CURRENT MACRO FOR TRACING         00205000
        MCBL,       % BLOCK LEVEL AT WHICH MACRO WAS DECLARED           00206000
        MEL,        % INDEX INTO TELBAT                                 00207000
        ML,         % CODE ADDRESS DURING MACRO COMPILING               00208000
        MSTORP,     % CURRENT ROWS IN MDECL AND MCODE                   00209000
        NEST,       % PROCEDURE NESTING LEVEL INDEX                     00210000
        NEWINC,     % SEQUENCE INCREMENT FOR NEWDISK FILE               00211000
        NEWSEQ,     % SEQUENCE # FOR NEWDISK FILE                       00212000
        NEXTDICT,   % NEXT AVAILABLE DICT ENTRY LOCATION                00213000
$ INCLUDE 00214001, IF INTERMEDIATE;                                    00213999
        NOWCODE,    % CURRENT RECORD OF ISOCODE IN CODRAY               00214000
        NWCT,       % CHR POS IN DECL FOR PACKET                        00215000
        NXTCODE,    % NEXT RECORD & REL-ADR FOR MFIELD CODE             00216000
        NXTELB,     % SERIAL # OF NEXT ELBAT WORD                       00217000
        NXTPTCHREC, % NEXT AVAILABLE RECORD IN BACKPATCHES              00218000
        NXTPTCHX,   % INDEX INTO PATCHSPACE                             00219000
        NXTUDICT,   % NEXT AVAILABLE UDICT LOCATION                     00220000
        OHM,        % CHR COUNT (+3) IN OA                              00221000
        OP,         % CURRENT COMPILE TIME OPERATOR                     00222000
        OUTC,       % CHR POS IN POB FOR CODE RECORD PACKING            00223000
        OUTERLIMIT, % BEGIN NESTING LEVEL OF OUTPUT STRING              00224000
        PANK,       % INDEX INTO TANK                                   00225000
        PL,         % PARTIAL MFIELD LENGTH                             00226000
        PT,         % INDEX INTO MCODE                                  00227000
        SAVEA,      % SAVED LAST MSTRING ADDRESS                        00228000
        SAVEML,     % ML FOR LISTING                                    00229000
        SAVEOP,     % TEMP OP                                           00230000
%                                                                       00231000
        SEQ,        % CURRENT SEQUENCE # IN CODE FILE (STEP 100)        00232000
        SSTK,       % STATEMENT PATCH STACK INDEX                       00233000
        STMTSTKLVL, % OUTER BLOCK BEGIN NESTING LEVEL                   00234000
        STREAMDICT, % POINTER TO STREAM RESWDS IN DICT                  00235000
        TABLET,     % THE "I" USED IN STEPI                             00236000
        TEMP,       % NEED I SAY MORE                                   00237000
        TFC,        % TEMPORARY STORAGE FOR PARTIAL MFIELD FC           00238000
        TNWCT,      % OVERFLOW NWCT FOR PACKET                          00239000
        TOGGLECT,   % COUNT OF TOGGLE SETTING OPERATIONS                00239200
        TPANK,      % TTANK INDEX                                       00240000
        TYPE;       % CLASS OF LAST ITEM FROM TABLE                     00241000
REAL    FIELD;      % MFIELD DESCRIPTOR                                 00242000
                                                                        00243000
DEFINE          % ELBAT FIELDS                                          00300000
        DIFF      = [ 1: 8] #,    % BACK TRACE LINK                     00301000
        BLKLVL    = [ 9: 9] #,    % BLOCK LEVEL                         00302000
        SUB       = [18: 9] #,    % SUBCLASS                            00303000
        CLASS     = [27: 6] #,    % CLASS                               00304000
        ADINFO    = [33:15] #,    % ADDITIONAL INFO                     00305000
        TODIFF    =  1:40: 8#,                                          00306000
        TOBLKLVL  =  9:39: 9#,                                          00307000
        TOSUB     = 18:39: 9#,                                          00308000
        TOCLASS   = 27:42: 6#,                                          00309000
        TOCLSUB   = 18:33:15#,                                          00310000
        CLUBCLUB  = 18:18:15#,                                          00311000
        TOADINFO  = 33:33:15#,                                          00312000
                % INTEGER MSTRING ADINFO FIELDS                         00313000
        OFFSET    = [ 9: 9] #,    % DICTIONARY DISPLACEMENT             00314000
        REF       = [18:15] #,    % REFERENCE ADDRESS                   00315000
        LIM       = [33:15] #,    % SIZE LIMIT                          00316000
        TOOFFSET  =  9:39: 9#,                                          00317000
        OFFTOOFF  =  3: 3:15#,                                          00318000
        REFTOREF  = 18:18:15#,                                          00318500
        TOREF     = 18:33:15#,                                          00319000
        TOLIM     = 33:33:15#,                                          00320000
                % MACRO ADINFO FIELDS                                   00321000
        MCIF      = [ 2: 6] #,    % CONDITIONAL INTERROGATION FLAG      00321900
        MLICT     = [ 8:10] #,    % # LOCAL INTEGER MSTRINGS            00322000
        MLACT     = [18:10] #,    % # LOCAL ALPHA MSTRINGS              00323000
        MPOOL     = [28:10] #,    % SIZE OF CONSTANT POOL               00324000
        MML       = [38:10] #,    % SIZE OF CODE STRING                 00325000
        MSMAP     = [13:10] #,    % SIZE OF SPACEMAP ENTRY              00326000
        MLOCT     = [23:10] #,    % # LOCAL MPOINTERS                   00327000
        MPT       = [33:15] #,    % LOCATION OF CODE STRING             00328000
        TOMCIF    =  2:42: 6#,                                          00328900
        TOMLICT   =  8:38:10#,                                          00329000
        TOMLACT   = 18:38:10#,                                          00330000
        TOMPOOL   = 28:38:10#,                                          00331000
        TOMML     = 38:38:10#,                                          00332000
        TOMSMAP   = 13:38:10#,                                          00333000
        TOMLOCT   = 23:38:10#,                                          00334000
        TOMPT     = 33:33:15#,                                          00335000
                % PSEUDOREADER FIELDS                                   00336000
        SENDT     = [ 4: 1] #,    % END COMMENT TOGGLE                  00337000
        SRSC      = [ 5: 1] #,    % RESCANTOG                           00338000
        SLST      = [ 6: 3] #,    % LASTUSED                            00339000
        SMAX      = [ 9:13] #,    % BWMAX                               00340000
        SLNG      = [22:13] #,    % BFRLNG                              00341000
        SBC       = [35: 3] #,    % BC                                  00342000
        SBW       = [38:10] #,    % BW                                  00343000
        TOSENDT   =  4:47: 1#,                                          00344000
        TOSRSC    =  5:47: 1#,                                          00345000
        TOSLST    =  6:45: 3#,                                          00346000
        TOSMAX    =  9:35:13#,                                          00347000
        TOSLNG    = 22:35:13#,                                          00348000
        TOSBC     = 35:45: 3#,                                          00349000
        TOSBW     = 38:38:10#,                                          00350000
                % BLIST FIELDS FOR CUTBACK                              00351000
        BBLKL     = [ 3:15] #,    % BLOCK LEVEL                         00352000
        BNXTD     = [18:15] #,    % NEXTDICT                            00353000
        BBNO      = [33:15] #,    % DECLARATION BNO                     00354000
        TOBBLKL   =  3:33:15#,                                          00355000
        TOBNXTD   = 18:33:15#,                                          00356000
        TOBBNO    = 33:33:15#,                                          00357000
                % PATCH FIELDS                                          00358000
        PNOTT     = [ 1: 1] #,    % NOT TOUCHED OR FOUND                00359000
        PLINK     = [ 2: 1] #,    % PATCH LINK FLAG                     00360000
        PSEQN     = [ 9:29] #,    % SEQUENCE # OF LINK RECORD           00361000
        PBASE     = [38:10] #,    % BASE RECORD IN PATCH FILE           00362000
        PUSED     = [ 3:15] #,    % # RECORD USED                       00363000
        PSIZE     = [18:15] #,    % REQUESTED SIZE                      00364000
        PCODE     = [33:15] #,    % CODEREC OF LINK RECORD              00365000
        PLVL      = [18:15] #,    % OUTERLIMIT OF PATCH                 00366000
        PLOC      = [33:15] #,    % LOCATION OF PATCH                   00367000
        TOPNOTT   =  1:47: 1#,                                          00368000
        TOPLINK   =  2:47: 1#,                                          00369000
        TOPSEQN   =  9:19:29#,                                          00370000
        TOPBASE   = 38:38:10#,                                          00371000
        TOPUSED   =  3:33:15#,                                          00372000
        TOPSIZE   = 18:33:15#,                                          00373000
        TOPCODE   = 33:33:15#,                                          00374000
        TOPLVL    = 18:33:15#,                                          00375000
        TOPLOC    = 33:33:15#,                                          00376000
                % SCAN RESTORATION FIELDS                               00377000
        RPANK     = [ 8:10] #,    % TANK POINTER                        00378000
        RT        = [18:10] #,    % T OF STEPI FAME                     00379000
        RNELB     = [28:10] #,    % NXTELB                              00380000
        RX        = [38:10] #,    % CURRENTLY UNUSED                    00381000
        TORPANK   =  8:38:10#,                                          00382000
        TORT      = 18:38:10#,                                          00383000
        TORNELB   = 28:38:10#,                                          00384000
        TORX      = 38:38:10#,                                          00385000
                % PARAMETRIC DEFINE FIELDS                              00386000
        PDC1      = [ 2: 2] #,    % CONSTANT CLASS, PART 1              00387000
        PDC2      = [ 4: 2] #,    %    "      "       "  2              00388000
        PDP1      = [ 6: 6] #,    % PARAMETER NO,     "  1              00389000
        PDP2      = [12: 6] #,    %    "      "       "  2              00390000
        PDF1      = [18:15] #,    % CONSTANT FIELD,   "  1              00391000
        PDF2      = [33:15] #,    %    "      "       "  2              00392000
        TOPDC1    =  2:46: 2#,                                          00393000
        TOPDC2    =  4:46: 2#,                                          00394000
        TOPDP1    =  6:42: 6#,                                          00395000
        TOPDP2    = 12:42: 6#,                                          00396000
        TOPDF1    = 18:33:15#,                                          00397000
        TOPDF2    = 33:33:15#,                                          00398000
                % LOADSCAN FIELDS                                       00399000
        LSLNK     = [ 6: 6] #,    % LINK TO NEXT ENTRY                  00400000
        LSCTR     = [12: 6] #,    % COUNTER FIELD                       00401000
        LSCMP     = [12:36] #,    % COMPARISON FIELD                    00402000
        LSBAK     = [18:15] #,    % BACK LINK TO LAST ENTRY             00403000
        LSNXT     = [33:15] #,    % INDEX OF NEXT ENTRY                 00404000
                % OPERATOR TEST FIELDS                                  00405000
        MOVM      = [15: 1] #,    % MOVE CHR INTO MSTRING               00406000
        DBMN      = [16: 1] #,    % DEBUG USING MNEMON                  00407000
        DBA       = [17: 1] #,    % DEBUG IN ALPHA                      00408000
        SCMP      = [20: 1] #,    % STRING COMPARISON                   00409000
        MINT      = [21: 1] #,    % INTEGER OPERATOR                    00410000
        MCON      = [22: 1] #,    % MAY TAKE A CONSTANT                 00411000
        ZACM      = [23: 1] #,    % CLEAR ACCM                          00412000
                % MFIELD FIELDS                                         00413000
        MFDG      = [ 1: 1] #,    % MFIELD DESIGNATOR IN LDFT           00414000
        MFVN      = [ 2: 1] #,    % MFIELD VALUE IN NEXT LDFT WORD      00415000
        MFINT     = [ 3: 1] #,    % INTEGER MFIELD                      00416000
        MFDCT     = [ 4: 1] #,    % DICT LIMIT PRESENT                  00416050
        MFLIM     = [ 6:12] #,    % ALPHA SIZE LIMIT                    00416150
        MFOFF     = [18: 6] #,    % OFFSET FOR DICTIONARY               00417000
        MFBGN     = [24:12] #,    % START POSITION                      00418000
        MFLNG     = [36:12] #,    % LENGTH                              00419000
        CROW      = [24:12] #,    % CODE ROW                            00420000
        CRAD      = [36:12] #,    % CODE RELATIVE ADDRESS               00421000
        TOMFDG    =  1:47: 1#,                                          00422000
        TOMFVN    =  2:47: 1#,                                          00423000
        TOMFINT   =  3:47: 1#,                                          00424000
        TOMFDCT   =  4:47: 1#,                                          00424050
        TOMFLIM   = 6:36:12#,                                           00424150
        TOMFOFF   = 18:42: 6#,                                          00425000
        TOMFBGN   = 24:36:12#,                                          00426000
        TOMFLNG   = 36:36:12#,                                          00427000
        TOCROW    = 24:36:12#,                                          00428000
        TOCRAD    = 36:36:12#,                                          00429000
                % USER DICTIONARY FIELDS                                00430000
        USIZ      = [18: 9] #,    % REQUESTED SIZE OF ENTRY             00431000
        UXXX      = [27: 6] #,    % OPEN                                00432000
        UQUAL     = [33:15] #,    % QUALIFIER                           00433000
        UDCT      = [ 2: 1] #,    % DICTIONARY ADDRESS                  00433050
        TOUSIZ    = 18:39: 9#,                                          00434000
        TOUXXX    = 27:42: 6#,                                          00435000
        TOUQUAL   = 33:33:15#,                                          00436000
        TOUDCT    =  2:47: 1#,                                          00436050
                % CHARACTER FIELDS                                      00437000
        C0        = [ 1: 5] #,  TOC0 =  1:43: 5#,                       00438000
        C1        = [ 6: 6] #,  TOC1 =  6:42: 6#,                       00439000
        C2        = [12: 6] #,  TOC2 = 12:42: 6#,                       00440000
        C3        = [18: 6] #,  TOC3 = 18:42: 6#,                       00441000
        C4        = [24: 6] #,  TOC4 = 24:42: 6#,                       00442000
        C5        = [30: 6] #,  TOC5 = 30:42: 6#,                       00443000
        C6        = [36: 6] #,  TOC6 = 36:42: 6#,                       00444000
        C7        = [42: 6] #,  TOC7 = 42:42: 6#,                       00445000
        C12       = [ 6:12] #,  TOC12 =  6:36:12#,                      00446000
        C23       = [12:12] #,  TOC23 = 12:36:12#,                      00447000
        C34       = [18:12] #,  TOC34 = 18:36:12#,                      00448000
                % MISCELLANY                                            00449000
        CTR       = [ 1:17] #,    % MSTRING COUNT FIELD                 00450000
        TOCTR     =  1:31:17#,                                          00451000
        IR        = [33: 7] #,    % INDEX ROW                           00452000
        IC        = [40: 8] #,    % INDEX COLUMN                        00453000
        XIR       = [ 3: 7] #,    % X-FIELD INDEX ROW                   00454000
        XIC       = [10: 8] #,    % X-FIELD INDEX COLUMN                00455000
        XF        = [ 3:15] #,    % X-TRA FIELD                         00456000
        TOXF      =  3:33:15#,                                          00457000
        IRTOIR    = 33:33: 7#,                                          00458000
        WRD       = [35:10] #,    % WORD INDEX (DIV 8)                  00459000
        CHR       = [45: 3] #,    % CHR INDEX  (MOD 8)                  00460000
        SADD      = [33:15] #,    % STREAM ADDRESS                      00461000
        SCHR      = [30: 3] #,    % STREAM CHR POS                      00462000
        TOWRD     = 35:38:10#,                                          00463000
        TOCHR     = 45:45: 3#,                                          00464000
        TOSADD    = 33:33:15#,                                          00465000
        TOSCHR    = 30:45: 3#,                                          00466000
        JADR      = [37:11] #,    % JUMPING ADDRESS                     00467000
        STA       = [ 9: 9] #,    % STATION FROM STATUS WORD            00468000
        NEXT      = [18:15] #,    % NEXT BASE COUNT                     00469000
        CUR       = [33:15] #,    % CURRENT BASE COUNTER                00470000
        TONEXT    = 18:33:15#,                                          00471000
        TOCUR     = 33:33:15#,                                          00472000
        D2        = [37:10] #,    % DIV 2                               00473000
        D4        = [36:10] #,    % DIV 4                               00474000
        D32       = [33:10] #,    % DIV 32                              00475000
        D512      = [33: 6] #,    % DIV 512                             00476000
        M4        = [46: 2] #,    % MOD 4                               00477000
        M32       = [43: 5] #,    % MOD 32                              00478000
        CFX8      = 30:33:15#;    % C-FIELD | 8                         00479000
                                                                        00480000
DEFINE          % MACRO ADDRESS STRUCTURES                              00481000
        LALFBAS =    0 #,                                               00482000
        LINTBAS =  256 #,                                               00483000
        LPTRBAS =  768 #,                                               00484000
        GALFBAS = 1280 #,                                               00485000
        GINTBAS = 1792 #,                                               00486000
        GPTRBAS = 2816 #,                                               00487000
        CONSBAS = 3840 #,                                               00488000
        LALFMAX =  255 #,                                               00489000
        LINTMAX =  767 #,                                               00490000
        LPTRMAX = 1279 #,                                               00491000
        GALFMAX = 1791 #,                                               00492000
        GINTMAX = 2815 #,                                               00493000
        GPTRMAX = 3839 #,                                               00494000
        CONSMAX = 4095 #,                                               00495000
                                                                        00496000
        ELBMAX  =   90 #,                                               00497000
        TANKMAX =  180 #,                                               00498000
        BFRMAX  = 350 #, % SIZE OF BFR                                  00498100
        IDMAX   =   90 #, % MAX CHR IN SCANNED ITEM (NOT A STRING)      00499000
        MSMAX   = 1950 #, % MAX CHR IN MSTRING                          00500000
        CONVBAS = 1011 #,                                               00501000
        CREV    =    6 #, % REVERSE COMPARE                             00502000
        PATCHMAX= 1000 #, % MAX BACKPATCHES                             00502050
        PTCHMAX =  268 #, % MAX RESERVES | 2                            00502100
        MLMAX   = 2043 #; % MAX CODE ADDRESS                            00503000
                                                                        00504000
DEFINE          % MACRO OPERATORS                                       00505000
        ONOPP = 00 #, % NO OPERATION                                    00506000
        OOPSU = 01 #, % OPEN PSEUDOREADER                               00507000
        OCPSU = 02 #, % CLOSE PSEUDOREADER                              00508000
        OSEQL = 03 #, % STRING COMPARE EQUAL                            00509000
        OSNEQ = 04 #, % STRING COMPARE NOT EQUAL                        00510000
        OSLSS = 05 #, % STRING COMPARE LESS                             00511000
        OSLEQ = 06 #, % STRING COMPARE LESS THAN OR EQUAL               00512000
        OSGTR = 07 #, % STRING COMPARE GREATER                          00513000
        OSGEQ = 08 #, % STRING COMPARE GREATER THAN OR EQUAL            00514000
        OOPDC = 09 #, % MSTRING OPERAND CALL..CAT                       00515000
        OOPDS = 10 #, % MSTRING OPERAND CALL..SPACE                     00516000
        ONSTR = 11 #, % NON-DESTRUCTIVE MSTRING STORE                   00517000
        ODSTR = 12 #, % DESTRUCTIVE MSTRING STORE                       00518000
        OISTR = 13 #, % INTEGER STORE                                   00519000
        OIOPC = 14 #, % INTEGER MSTRING OPERAND CALL                    00520000
        OADDI = 15 #, % ADD INTEGER MSTRING                             00521000
        OSUBI = 16 #, % SUBTRACT INTEGER MSTRING                        00522000
        OMULI = 17 #, % MULTIPLY INTEGER MSTRING                        00523000
        ODIVI = 18 #, % DIVIDE INTEGER MSTRING                          00524000
        OOP19 = 19 #,                                                   00525000
        OOP20 = 20 #,                                                   00526000
        OILIT = 21 #, % INTEGER LITERAL CALL                            00527000
        OADDL = 22 #, % ADD LITERAL                                     00528000
        OSUBL = 23 #, % SUBTRACT LITERAL                                00529000
        OMULL = 24 #, % MULTIPLY LITERAL                                00530000
        ODIVL = 25 #, % DIVIDE LITERAL                                  00531000
        OLT1C = 26 #, % 1-LIT..CAT                                      00532000
        OLT2C = 27 #, % 2-LIT..CAT                                      00533000
        OLT2S = 28 #, % 2-LIT..SPACE                                    00534000
        OOP29 = 29 #,                                                   00535000
        OOP30 = 30 #,                                                   00536000
        OOP31 = 31 #,                                                   00537000
        OOP32 = 32 #,                                                   00538000
        OOP33 = 33 #,                                                   00539000
        OOP34 = 34 #,                                                   00540000
        OOP35 = 35 #,                                                   00541000
        OINPT = 36 #, % INPUT SEQUENCE                                  00542000
        OOTPT = 37 #, % OUTPUT SEQUENCE                                 00543000
        OERRO = 38 #, % ERROR, WARNING, NOTE SEQUENCE                   00544000
        OOP39 = 39 #,                                                   00545000
        OOP40 = 40 #,                                                   00546000
        OLDFT = 41 #, % LOAD FOR TEST                                   00547000
        OSUNC = 42 #, % SCAN UNTIL..CAT                                 00548000
        OSUNS = 43 #, % SCAN UNTIL..SPACE                               00549000
        OSTHC = 44 #, % SCAN THRU..CAT                                  00550000
        OSTHS = 45 #, % SCAN THRU..SPACE                                00551000
        OENTR = 46 #, % USERDICT ENTRY                                  00552000
        OFIND = 47 #, % FIND USERDICT ENTRY                             00553000
        OSRCH = 48 #, % SEARCH USERDICT                                 00554000
        OOP49 = 49 #,                                                   00555000
        OOP50 = 50 #,                                                   00556000
        OTYPC = 51 #, % SCAN TYPE..CAT                                  00557000
        OTYPS = 52 #, % SCAN TYPE..SPACE                                00558000
        OSETL = 53 #, % SET LENGTH                                      00559000
        OCONV = 54 #, % CONVERT                                         00560000
        OINDC = 55 #, % INDIRECT ADDRESS CALL                           00561000
        OICMP = 56 #, % INTEGER COMPARE                                 00562000
        OTOGL = 57 #, % TOGGLE SET RESET OPERATIONS                     00563000
        OOP58 = 58 #,                                                   00564000
        OOP59 = 59 #,                                                   00565000
        OOP60 = 60 #,                                                   00566000
        OJMPC = 61 #, % JUMP CONDITIONAL                                00567000
        OJUMP = 62 #, % JUMP UNCONDITIONAL                              00568000
        OINT1 = 63 #, % INTRINSIC 1 (CASES FOLLOW (+64))                00569000
        OBGNL = 64 #, % BEGINLEVEL (+OP)                                00570000
        OBLKL = 69 #, % BLOCKLEVEL (+OP)                                00571000
        ODIAL = 74 #, % DIAL FIRST CHARACTER                            00572000
        OLINC = 75 #, % LINE..CAT                                       00573000
        OLINS = 76 #, % LINE..SPACE                                     00574000
        OENTI = 77 #, % ENTIER                                          00575000
        OLENG = 78 #, % LENGTH                                          00576000
        ORDCD = 79 #, % READACARD                                       00577000
        ODELE = 80 #, % DELETE                                          00578000
        OSTCK = 81 #, % STACK                                           00579000
        ONSTK = 82 #, % UNSTACK                                         00580000
        OSCNT = 83 #, % SCAN COUNT                                      00581000
        OLEVL = 88 #, % UDICT LEVEL                                     00582000
        OQUAL = 89 #, % UDICT QUALIFIER                                 00583000
        ONAME = 90 #, % UDICT ENTRY NAME                                00584000
        OEXIT = 91 #; % EXIT MACRO                                      00585000
                                                                        00586000
                                                                        00587000
DEFINE          % SCAN CLASSES                                          00588000
        VSPECIAL    = 00 #, % SPECIAL CHARACTER                         00589000
        VNUMBER     = 01 #, % REAL OR INTEGER NUMBER                    00590000
        VOCTAL      = 02 #, % WRL OCTAL CONSTANT                        00591000
        VSTRING     = 03 #, % QUOTED STRING                             00592000
        VTORF       = 04 #, % TRUE OR FALSE                             00593000
        VNOTDEC     = 05 #, % NON RESCANNED ID                          00594000
        VMFIELDID   = 13 #, % MFIELD FUNCTION                           00595000
            % 0 => ALPHA                                                00596000
            % 1 => INTEGER                                              00597000
        VMACRONAME  = 14 #, % MACRO IDENTIFIER                          00598000
            % SEE VOTHERS SUBCLASSES                                    00599000
        VPARAMID    = 15 #, % PARAMETRIC DEFINE PARAMETER               00600000
            % N = PARAMETER #                                           00601000
        VSTRINGNAME = 16 #, % MSTRING, MPOINTER ID                      00602000
            % 0 => ALPHA MSTRING                                        00603000
            % 1 => INTEGER MSTRING                                      00604000
            % 2 => ALPHA MPOINTER                                       00605000
            % 3 => INTEGER MPOINTER                                     00606000
            % 4 => DICTIONARY ALPHA MSTRING                             00607000
            % 5 => DICTIONARY INTEGER MSTRING                           00608000
            % 6 => DICTIONARY MPOINTER                                  00609000
        VALGOLIDENT = 17 #, % ALGOL DECLARED ID                         00610000
            % SEE VOTHERS SUBCLASSES                                    00611000
        VDEFINEDID  = 18 #, % DEFINE ID                                 00612000
            % 0 => 1 IDENTIFIER                                         00613000
            % 1 => MANY THINGS, LAST ITEM = ID                          00614000
            % 2 => 1 SPCHR                                              00615000
            % 3 => MANY THINGS, LAST ITEM = SPCHR                       00616000
            % 4+=> PARAMETRIC                                           00617000
        VDOLLARID   = 19 #, % MCALGOL DOLLAR CARD RESERVED WORD         00618000
            % 00 => CARD                                                00619000
            % 01 => DISK                                                00620000
            % 02 => NEW                                                 00621000
            % 03 => SEQ                                                 00622000
            % 04 => DUMP                                                00623000
            % 05 => VOID                                                00624000
            % 06 => LIST                                                00625000
            % 07 => DOUBLE                                              00626000
            % 08 => MONITOR                                             00627000
            % 09 => TRACE                                               00628000
            % 10 => DEBUGN                                              00629000
            % 11 => ACCUM                                               00630000
            % 12 => UNPACK                                              00631000
            % 13 => INTER                                               00632000
            % 14 => PRT                                                 00633000
            % 15 => BLOCK                                               00634000
            % 16 => WARN                                                00635000
            % 17 => PAGE                                                00636000
            % 18 => INDEX                                               00637000
            % 19 => PSEUDO                                              00638000
        VSTREAMRES  = 20 #, % STREAM RESERVED WORD                      00639000
        VMACRORES   = 21 #, % MACRO RESERVED WORD                       00640000
            % SEE VMACRORES SUBCLASSES                                  00641000
        VOTHERS     = 22 #, % ALGOL RESERVED? WORD                      00642000
            % SEE VOTHERS SUBCLASSES                                    00643000
        VDECLRN     = 23 #, % DECLARATION POINTER                       00644000
        VPATCHID    = 24 #; % PATCH IDENTIFIER                          00645000
                                                                        00646000
DEFINE          % VOTHERS SUBCLASSES                                    00647000
        OWNV        = 01 #,                                             00648000
        SAVEV       = 02 #,                                             00649000
        BOOLEANV    = 03 #,                                             00650000
        REALV       = 04 #,                                             00651000
        ALPHAV      = 05 #,                                             00652000
        INTEGERV    = 06 #,                                             00653000
        LABELV      = 07 #,                                             00654000
        DUMPV       = 08 #,                                             00655000
        MONITORV    = 09 #,                                             00656000
        OUTV        = 10 #,                                             00657000
        INV         = 11 #,                                             00658000
        SWITCHV     = 12 #,                                             00659000
        STREAMV     = 13 #,                                             00660000
        PROCEDUREV  = 14 #,                                             00661000
        ARRAYV      = 15 #,                                             00662000
        STRINGV     = 16 #,                                             00663000
        FILEV       = 17 #,                                             00664000
        LISTV       = 18 #,                                             00665000
        FORMATV     = 19 #,                                             00666000
        MACROV      = 20 #,                                             00667000
        DEFINEV     = 21 #,                                             00668000
        MFIELDV     = 22 #,                                             00669000
        DICTIONARYV = 23 #,                                             00670000
        MPOINTERV   = 24 #,                                             00671000
        NOV         = 31 #,                                             00672000
        PAGEV       = 32 #,                                             00673000
        DBLV        = 33 #,                                             00674000
        READV       = 36 #,                                             00675000
        WRITEV      = 37 #,                                             00676000
        BEGINV      = 42 #,                                             00677000
        FORV        = 43 #,                                             00678000
        THENV       = 44 #,                                             00679000
        DOV         = 45 #,                                             00680000
        UNTILV      = 46 #,                                             00681000
        ELSEV       = 47 #,                                             00682000
        ENDV        = 48 #,                                             00683000
        FILLV       = 49 #,                                             00684000
        TOV         = 50 #,                                             00685000
        IFV         = 51 #,                                             00686000
        GOV         = 52 #,                                             00687000
        RELEASEV    = 53 #,                                             00688000
        DOUBLEV     = 54 #,                                             00689000
        REVERSEV    = 55 #,                                             00690000
        MODV        = 56 #,                                             00691000
        DIVV        = 57 #,                                             00692000
        ENTIERV     = 58 #,                                             00693000
        COSV        = 59 #,                                             00694000
        EXPV        = 60 #,                                             00695000
        LNV         = 61 #,                                             00696000
        ARCTANV     = 62 #,                                             00697000
        SINV        = 63 #,                                             00698000
        SQRTV       = 64 #,                                             00699000
        SIGNV       = 65 #,                                             00700000
        ABSV        = 66 #,                                             00701000
        COMMENTV    = 71 #,                                             00702000
        FORWARDV    = 72 #,                                             00703000
        STEPV       = 73 #,                                             00704000
        WHILEV      = 74 #,                                             00705000
        REPLACEV    = 75 #,                                             00706000
        VALUEV      = 76 #,                                             00707000
        WITHV       = 77 #,                                             00708000
        NOTV        = 85 #,                                             00709000
        EQVV        = 88 #,                                             00710000
        IMPV        = 89 #,                                             00711000
        ORV         = 90 #,                                             00712000
        ANDV        = 91 #;                                             00713000
                                                                        00714000
DEFINE          % VMACRORES SUBCLASSES                                  00715000
                % STATEMENT BEGINNERS                                   00716000
        INPUTM          = 00 #,                                         00717000
        RESCANM         = 01 #,                                         00718000
        OUTPUTM         = 02 #,                                         00719000
        ERRORM          = 03 #,                                         00720000
        WARNINGM        = 04 #,                                         00721000
        POSTM           = 05 #,                                         00722000
        DELETEM         = 06 #,                                         00723000
        PATCHM          = 07 #,                                         00724000
        SETM            = 08 #,                                         00725000
        RESETM          = 09 #,                                         00726000
                % INTEGER SEQUENCE ELEMENTS ONLY                        00727000
        RESERVEM        = 25 #,                                         00728000
        RESERVESTMTM    = 26 #,                                         00729000
        ENTERM          = 27 #,                                         00730000
        FINDM           = 28 #,                                         00731000
        SEARCHM         = 29 #,                                         00732000
        LEVELM          = 30 #,                                         00733000
        QUALIFIERM      = 31 #,                                         00734000
                % INTEGER OR ALPHA SEQUENCE ELEMENTS                    00735000
        LINEM           = 50 #,                                         00736000
        ENTIERM         = 51 #,                                         00737000
        LENGTHM         = 52 #,                                         00738000
                    % + OPERATORS                                       00739000
        BEGINLEVELM     = 60 #,                                         00740000
        BLOCKLEVELM     = 61 #,                                         00741000
        SCANCOUNTM      = 62 #,                                         00742000
                % ALPHA SEQUENCE ELEMENTS ONLY                          00743000
        UNTILM          = 75 #,                                         00744000
        THRUM           = 76 #,                                         00745000
        QUOTEM          = 77 #,                                         00746000
        NAMEM           = 78 #,                                         00747000
                % MISC                                                  00748000
        NULLM           = 97 #,                                         00748050
        EQUIVM          = 98 #,                                         00748100
        TOGGLEM         = 99 #;                                         00749000
                                                                        00750000
DEFINE          % VMACRORES SCAN TYPES SUBCLASSES                       00751000
                % ALPHA ONLY                                            00752000
        BEXPRESSIONM    = 100 #,                                        00753000
        CHARACTERM      = 101 #,                                        00754000
        ELEMENTM        = 102 #,                                        00755000
        EXPRESSIONM     = 103 #,                                        00756000
        IDENTIFIERM     = 104 #,                                        00757000
        STATEMENTM      = 105 #,                                        00758000
        VARIABLEM       = 106 #,                                        00759000
        DECLARATIONM    = 107 #,                                        00760000
        BPRIMARYM       = 108 #,                                        00761000
        PRIMARYM        = 109 #,                                        00762000
                % INTEGER OR ALPHA                                      00763000
        NUMBERM         = 125 #,                                        00764000
        INTGERM         = 126 #,                                        00765000
                % TOGGLE CONDITIONS                                     00766000
        DECLAREM        = 200 #,                                        00767000
        COMMENTM        = 201 #,                                        00768000
        ENDCOMMENTM     = 202 #,                                        00769000
        INHIBITM        = 203 #,                                        00770000
        USERM           = 255 #;                                        00771000
                                                                        00772000
DEFINE                                                                  00773000
        T = TABLET #,                                                   00774000
        STEPI = TYPE ~ TABLE(TABLET~TABLET+1)#;                         00775000
ARRAY   ACCM[0:255],COMMENT ALPHA SEQUENCE ACCUMULATOR. FORMAT FOR      00800000
                    ALL ALPHA MSTRINGS IS 3 CHR FOR BINARY COUNTER      00801000
                    FIELD FOLLOWED BY CHARACTERS FOLLOWED BY SPACE;     00802000
        BFR[0:BFRMAX-1], COMMENT PSEUDO & INPUT IMAGE BUFFER TO         00803000
                    CONTAIN "INPUT STRING". INPUT IMAGES ARE PLACED     00804000
                    IN THE RIGHTMOST 10 WORDS WITH A SCANNER            00805000
                    TERMINATING "%" IN COLUMN 73.  PSEUDO INPUT IS      00806000
                    "STACKED" TO THE LEFT OF THE NEXT ITEM TO BE        00807000
                    SCANNED. A "%" IS PLACED AT THE RIGHT OF EACH       00808000
                    PSEUDOREADER TO CAUSE UNSTACKING AND/OR             00809000
                    PREVENT OVER-SCANNING ON DECOMPOSITION;             00810000
        BLIST[0:249], COMMENT BEGIN-END AND BLOCK CONTROL               00811000
                    CUT BACK INFO FOR DICTIONARY ENTRIES.               00812000
                    MCALGOL DICT LIST IN 0-124,                         00813000
                    USER DICT LIST IN 125-249;                          00814000
        COMCON[0:255], COMMENT MACRO COMPILE-TIME CONSTANTS             00815000
                    STORAGE AREA;                                       00816000
        DECL[0:255], COMMENT DECLARATION TIME STORAGE SPACE FOR         00817000
                    PACKED ELEMENTS OF A DECLARATION;                   00818000
        DFN[0:255], COMMENT USED TO EXPAND DEFINES;                     00819000
        DICT[0:63,0:255], COMMENT MCALGOL DICTIONARY.                   00820000
                    WORD[0] CONTAINS SCRAMBLE LINK WORD.                00821000
                        =-1 IF END OF CHAIN.                            00822000
                    WORD[1] CONTAINS ELBAT WORD.                        00823000
                    WORDS[2-N] CONTAIN IDENTIFIER IN ALPHA              00824000
                        MSTRING FORMAT.                                 00825000
                        N IS (CTR+10)DIV 8-2 WORDS FOR THE              00826000
                        CHR REMAINING AFTER FIRST FIVE.                 00827000
                    WORD[1].ADINFO HAS THE FOLLOWING USES:              00828000
                    MSTRINGS, MPOINTERS: WORD[N+1]                      00829000
                        LIM = LENGTH IN CHR                             00830000
                        REF = SYMBOLIC ADDRESS                          00831000
                    MACROS: WORD[N+1]                                   00832000
                        MLICT = # LOCAL INTEGERS                        00833000
                        MLACT = # LOCAL ALPHAS                          00834000
                        MPOOL = # WORDS IN CONSTANT POOL                00835000
                        MML   = # WORDS OF CODE                         00836000
                      WORD[N+2]:                                        00837000
                        MLOCT = # LOCAL MPOINTERS                       00838000
                        MPT   = LOCATION OF CODE IN MCODE               00839000
                    MFIELDS: WORD[N+1] = CONSTANT POOL                  00840000
                        ENTRY FOR MFIELD DESCRIPTOR.                    00841000
                    DECLARATIONS: MDECL ADDRESS WHERE                   00842000
                        ALPHA CAN BE FOUND.                             00843000
                    IDENTIFIERS: TANK ADDRESS OF ORIGINAL               00844000
                        ALPHA.                                          00845000
                    SPECIAL CHARACTERS: THE CHARACTER.                  00846000
                    PARAMETRIC DEFINES: WORD[N+1]                       00847000
                        LIM = # RECONSTRUCTION PARTS                    00848000
                        REF = MDECL ADDRESS OF RECON INFO               00849000
                      WORDS[N+2->...] SPACE FOR PARAMETER               00850000
                        POINTERS DURING PROCESSING                      00851000
                      ELBAT.SUB = 4+# OF PARAMETERS                     00852000
                    ;                                                   00853000
        DLIST[0:249], COMMENT SCRAMBL STACKHEAD.                        00854000
                    MCALGOL DICT IN 0-124. USER DICT IN 125-249;        00855000
        ELBAT[0:269], COMMENT CONTAINS ELBAT WORDS FOR SCANNED          00856000
                    ITEMS. NXTELB IS SERIAL. BELB IS CURRENT BASE       00857000
                    INDEX OF ELBAT[0]. OVERFLOW KEEPS LAST 10 ITEMS;    00858000
        ERB[0:14],  COMMENT ERROR BUFFER AND GLOBAL ALPHA TEMP;         00859000
        GINT[0:255],COMMENT GLOBAL INTEGER MSTRING SPACE;               00860000
        GLOBAL[0:63,0:255], COMMENT GLOBAL ALPHA MSTRINGS. ROW          00861000
                    SIZES ARE FORESHORTENED AT DECLARATION TIME.        00862000
                    MAX SIZE IS 512, BUT NO ONE HAS YET USED OR         00863000
                    EXCEEDED 64;                                        00864000
        GPTR[0:255], COMMENT GLOBAL MPOINTERS;                          00865000
        IDENT[0:255], COMMENT LAST SCANNED IDENTIFIER OR                00866000
                    QUOTED STRING;                                      00867000
        LINKS[0:3], COMMENT SEQUENCE FIELDS OF INPUT AND PREVIOUS       00868000
                    ERROR IMAGES;                                       00869000
        LOCAL[0:63,0:255], COMMENT LOACL STORAGE FOR MACROS. ROWS OF    00870000
    LOCAL CONTAIN:                                                      00871000
        0 - INTEGER MSTRINGS, 1 WORD PER MSTRING                        00872000
        1 - CONSTANT POOL                                               00873000
        2 - CODE STRING                                                 00874000
        3 - ALPHA COMPARISON SPACE & "JUNK"                             00875000
        4 - SCAN POOL FOR LOAD-FOR-TEST SEQUENCE                        00876000
        5 - TEMP ACCM STORAGE WHILE LOADING..NOT STACKED TO DISK        00877000
        6 - MPOINTERS                                                   00878000
        7-N - ALPHA MSTRINGS, 4 UNITS OF 64 WORDS EACH;                 00879000
                                                                        00880000
        MAXINFO[0:99], COMMENT PSEUDOREADER STACKING INFO;              00883000
        MC[0:255],  COMMENT MACRO COMPILE-TIME CODE ARRAY;              00884000
        MCODE[0:255], COMMENT STORAGE FOR COMPILED CODE,                00885000
                    CONSTANTS, AND SIZING INFO;                         00886000
        MDECL[0:255], COMMENT STORAGE FOR PACKED                        00887000
                    DECLARATIONS AND DEFINES;                           00888000
        MNEMON[0:99], COMMENT CONTAINS DEBUGGING MNEMONICS FOR          00889000
                    OPCODES AND THE INTERPRETATION-TIME PROPERTIES      00890000
                    THEREOF. 0-63 ARE ACTUAL OPCODES. }64 ARE           00891000
                    OINT1 CASE SWITCH OPERATORS(+64);                   00892000
        OA[0:1022],COMMENT OUTPUT ACCUMULATOR FOR OUTPUT OF A MACRO.    00893000
                    SEQUENCES ARE APPENDED SERIALLY AT END.             00894000
                    OA WILL BE PSEUDOREAD UPON TERMINATION              00895000
                    OF MACRO;                                           00896000
$ INCLUDE 00899001, IF INTERMEDIATE;                                    00896999
        PATCHSPACE[0:PTCHMAX+1],COMMENT LOCATOR SPACE FOR PATCH RECORDS.00897000
                    TWO WORD ENTRIES KEEP TRACK OF REQUESTED AND        00898000
                    USED SPACE;                                         00899000
        POB[0:14],  COMMENT PACKED OUTPUT BUFFER FOR OUTPUT STRING;     00900000
        PPB[0:14],  COMMENT PACKED PATCH BUFFER FOR BACKPATCHES;        00901000
        PROCT[0:63], COMMENT PROCEDURE NESTING AND BEGIN-END            00902000
                    COUNTER;                                            00903000
        RELOP[0:11], COMMENT RELATION AND COUNTER RELATION              00904000
                    OPTIMIZATION ARRAY;                                 00905000
        SPACEMAP[0:23], COMMENT BIT MAP OF SPACE ASSIGNMENTS            00906000
                    IN GLOBAL & LOCAL;                                  00907000
        STMTSTACK[0:127], COMMENT RESERVE STATEMENT STACKED             00908000
                    OCCURENCE STORAGE;                                  00909000
        TANK[0:179], COMMENT CIRCULAR FILE OF IDENTIFIER ALPHA          00910000
                    POINTED TO BY ELBAT WORDS;                          00911000
        TELBAT[0:89], COMMENT TEMPORARY STORAGE FOR ELBAT WORDS         00912000
                    ALREADY PRESCANNED AT THE TIME OF A PSEUDOREAD.     00913000
                    THEY WILL BE RECALLED WHEN THE PSEUDOREADER         00914000
                    IS CLOSED;                                          00915000
        TTANK[0:179], COMMENT CIRCULAR FILE OF ALPHA FOR ITEMS          00916000
                    IN TELBAT;                                          00917000
$ INCLUDE 00917101, IF DEBUGGING;                                       00917049
        TYPENAME[0:63],  COMMENT  NAMES OF SCANTYPE PROCEDURES          00917050
                    USED IN TRACING;                                    00917100
$ INCLUDE 00926001, IF ADVANCED;                                        00917999
        UDICT[0:63,0:255]; COMMENT USER DICTIONARY.                     00918000
                    WORD[0] IS DLIST LINK.                              00919000
                    WORD[1] HAS THE FOLLOWING FIELDS:                   00920000
                                                                        00921000
                                                                        00922000
                                                                        00923000
                                                                        00924000
                                                                        00925000
                    ;                                                   00926000
$ OMIT 00926051, IF ADVANCED;                                           00926049
        UDICT[0:0];     % DUMMY                                         00926050
BOOLEAN ARRAY                                                           00927000
        ERRMASK[0:31]; COMMENT A BIT IS SET IN ERRMASK FOR EACH         00928000
                    ERROR AND WARNING MESSAGE ENCOUNTERED. THIS         00929000
                    GUIDES THE ERROR MESSAGE LISTER AT THE END          00930000
                    OF THE PROGRAM;                                     00931000
    FILE OUT CODE DISK SERIAL[20:IF ERRCT=0 THEN 120 ELSE(ERRCT+299)    00950000
        DIV 300|15](1,10,150,SAVE 780);                                 00951000
    FILE OUT PRINTER 4(1,15);                                           00952000
    FILE OUT CC DISK SERIAL [20:24](1,10,60);                           00953000
    FILE IN DISK DISK SERIAL (1,150,10);                                00954000
    FILE IN CARD (1,10,150);                                            00955000
    FILE OUT NEWDISK DISK SERIAL [20:(ERRCT+299)DIV 300|15]             00956000
        (1,10,150,SAVE 780);                                            00957000
    FILE DISKSTACK DISK RANDOM [20:20](2,270);                          00958000
$ INCLUDE 00959101, IF INTERMEDIATE;                                    00958999
    FILE BACKPATCHES DISK RANDOM[20:(PATCHMAX+199)DIV 200|10]           00959000
        (1,9,90);                                                       00959100
    FILE OUT INDEX DISK SERIAL [20:60](1,10,150,SAVE 1);                00960000
$ INCLUDE 00961001, IF INTERMEDIATE;                                    00960999
    FILE ISOCODE DISK RANDOM [20: 5] (1,120);                           00961000
    FILE MSTOR DISK RANDOM [20:15] (1,256);                             00962000
    FILE ERRORS DISK RANDOM (1,10,30);                                  00963000
    FORMAT                                                              00975000
        MON1(A5"["I5"] "A1,X1,O" ("3I6")"A7,X1,A1),                     00976000
        MON2("CAD="I4" OP="A4" ADD="V4" T="I5" TOG="L1" HM="I4,         00977000
            " OHM="I4" IACCM="I9" IACCM1="I9),                          00978000
        MON3("SLIPWORDS"2I5),                                           00979000
        MON4(A2,I5,L6);                                                 00980000
    FORMAT                                                              00981000
        FLUSH("FLUSHED");                                               00982000
$ INCLUDE 00984001, IF DEBUGGING;                                       00982999
    FORMAT                                                              00983000
        DEBUG(I5" OP="A4" ADDR="V4);                                    00984000
$ INCLUDE 01014001, IF INTERMEDIATE;                                    00999999
    INTEGER SPOTISOCAT; % MOVE NOT THESE 3 DECLARATIONS                 01000000
    REAL PROCEDURE ISOCAT(A); VALUE A; INTEGER A;                       01001000
    BEGIN REAL B; ISOCAT ~ B END;                                       01002000
    SAVE ARRAY CODRAY[0:119];                                           01003000
PROCEDURE STARTUPISOCAT; BEGIN                                          01004000
    STREAM PROCEDURE FIXISOCAT(SCAT);                                   01005000
    BEGIN                                                               01006000
        SI~SCAT; SI~SI+21;                                              01007000
        DI~SCAT; DI~DI+8; DS~5 LIT "=0000"; DS~3 CHR;                   01008000
        DI~DI-3; DS~3 RESET;                                            01009000
    END;                                                                01010000
    NOWCODE ~ REAL(NOT FALSE);                                          01011000
    CODRAY[0] ~ REAL(<55400662310055>); % F-1,LFU                       01012000
    FIXISOCAT(SPOTISOCAT);                                              01013000
END STARTUPISOCAT;                                                      01014000
BEGIN % END SETUP                                                       01100000
    LABEL DP,EOF,ENDDD;                                                 01101000
PROCEDURE READACARD; FORWARD;                                           01102000
PROCEDURE PACKET; FORWARD;                                              01103000
PROCEDURE SLIPWORDS(A,B); VALUE A,B; INTEGER A,B; FORWARD;              01104000
PROCEDURE WRAPBLOCK(T); VALUE T; BOOLEAN T; FORWARD;                    01105000
PROCEDURE MACRON(QQ,Q); VALUE Q,QQ; REAL Q,QQ; FORWARD;                 01106000
PROCEDURE MACRO(Q); VALUE Q; REAL Q; FORWARD;                           01107000
PROCEDURE DECLARATION; FORWARD;                                         01108000
INTEGER PROCEDURE TABLE(P); VALUE P; INTEGER P; FORWARD;                01109000
$ INCLUDE 01110001, IF INTERMEDIATE;                                    01109999
REAL PROCEDURE COMPILEFIELD(A,B); VALUE A,B; INTEGER A,B; FORWARD;      01110000
BOOLEAN PROCEDURE MDOLLAR(B); VALUE B; BOOLEAN B; FORWARD;              01111000
        COMMENT  TIMEIT POSTS THE TIME LINE AT THE BEGINNING AND        01112000
    ENDING OF THE LISTING;                                              01113000
PROCEDURE TIMEIT(A);                                                    01114000
    FILE A;                                                             01115000
    BEGIN                                                               01116000
        OWN BOOLEAN B;                                                  01117000
        INTEGER E, F, G, H, I;                                          01118000
        STREAM PROCEDURE C(A,B,D); VALUE A; BEGIN                       01119000
            SI~LOC A; SI~SI+3; DI~B; DS~2 OCT; DI~D; DS~3 OCT END;      01120000
        STREAM PROCEDURE D(A, B, C, E, F, G, H, I, J, K);               01121000
        VALUE A, B, C, E, F, G, H, I, J;                                01122000
        BEGIN                                                           01123000
            LABEL L, M, N, O, P, Q;                                     01124000
            DI~K; DS~8 LIT " "; SI~K; DS~14 WDS; DI~K; SI~LOC A;        01125000
        DS~2 DEC; 2(DS~LIT ":"; DS~2 DEC); DI~DI+1;                     01126000
        CI~CI+E; % 5 SYLLABLE HOPS                                      01127000
        DS~3 LIT "SUN";    GO L; SKIP SB;                               01128000
        DS~3 LIT "MON";    GO L; SKIP SB;                               01129000
        DS~4 LIT "TUES";   GO L; SKIP SB;                               01130000
        DS~6 LIT "WEDNES"; GO L;                                        01131000
        DS~5 LIT "THURS";  GO L;                                        01132000
        DS~3 LIT "FRI";    GO L; SKIP SB;                               01133000
        DS~5 LIT "SATUR";                                               01134000
L:      DS~4 LIT "DAY,"; DI~DI+1;                                       01135000
        CI~CI+F; % 5 SYLLABLE HOPS                                      01136000
        DS~3 LIT "JAN";    GO M; SKIP SB;                               01137000
        DS~4 LIT "FEBR";   GO M; SKIP SB;                               01138000
        DS~5 LIT "MARCH";  GO O;                                        01139000
        DS~5 LIT "APRIL";  GO O;                                        01140000
        DS~3 LIT "MAY";    GO O; SKIP SB;                               01141000
        DS~4 LIT "JUNE";   GO O; SKIP SB;                               01142000
        DS~4 LIT "JULY";   GO O; SKIP SB;                               01143000
        DS~6 LIT "AUGUST"; GO O;                                        01144000
        DS~6 LIT "SEPTEM"; GO N;                                        01145000
        DS~4 LIT "OCTO";   GO N; SKIP SB;                               01146000
        DS~5 LIT "NOVEM";  GO N;                                        01147000
        DS~5 LIT "DECEM";  GO N;                                        01148000
M:      DS~4 LIT "UARY";   GO O;                                        01149000
N:      DS~3 LIT "BER";                                                 01150000
O:      DI~DI+1; A~DI; DI~LOC B; SI~LOC G; DS~2 DEC; DI~A;              01151000
        SI~LOC B; IF SC="0" THEN SI~SI+1 ELSE DS~CHR; DS~CHR;           01152000
        DS~4 LIT ", 19"; SI~LOC H; DS~2 DEC;                            01153000
        IF SC!"+" THEN BEGIN                                            01154000
            DI~DI+4; DS~16 LIT "PROCESSOR TIME ="; DI~DI+1;             01155000
            A~CI; GO P;                                                 01156000
            DI~DI+4; DS~10 LIT "I/O TIME ="; DI~DI+1;                   01157000
            SI~LOC J; A~CI; GO P; GO Q;                                 01158000
P:          B~DI; DI~LOC C; DS~8 DEC; SI~LOC C; DI~B;                   01159000
            5(IF TOGGLE THEN IF SC="0" THEN SI~SI+1 ELSE                01160000
                DS~CHR ELSE DS~CHR);                                    01161000
            DS~CHR; DS~LIT "."; DS~2 CHR; DS~8 LIT " SECONDS";          01162000
            CI~A;                                                       01163000
Q:      END;                                                            01164000
        END;                                                            01165000
        ARRAY J[0:14];                                                  01166000
        C(TIME(0), E, F);                                               01167000
        G~((((E-1) DIV 4)+E+F) MOD 7)|5;                                01168000
        IF F<60 THEN                                                    01169000
            F~ F-1                                                      01170000
        ELSE                                                            01171000
            IF E MOD 4=0 THEN                                           01172000
                F~ F-1;                                                 01173000
        IF F>212 THEN                                                   01174000
            F~(F+31) DIV 61+F                                           01175000
        ELSE                                                            01176000
            IF F>59 THEN                                                01177000
                F~(F+62) DIV 61+F;                                      01178000
        H~ TIME(1)|.016667;                                             01179000
        D(H DIV 3600,I~H DIV 60 MOD 60,I~H MOD 60,G,(F DIV 31)|5,       01180000
          H~F MOD 31+1,E,IF B THEN H~TIME(2)|1.6667 ELSE-1,IF B         01181000
          THEN H~ TIME(3)|1.6667 ELSE-1, J[0]);                         01182000
        B~ TRUE;                                                        01183000
        WRITE(A[DBL], 15, J[*])                                         01184000
    END;                                                                01185000
        COMMENT  SETSIZE CHANGES THE DESCRIPTOR SIZE FIELD OF           01200000
    AN ARRAY OR VECTOR. L MUST NOT BE ZERO OR A NAME DESCRIPTOR         01201000
    WILL RESULT;                                                        01202000
STREAM PROCEDURE SETSIZE(L,A,M,N);                                      01203000
    VALUE L, M, N;                                                      01204000
    BEGIN                                                               01205000
        SI~LOC L; SI~SI+6; DI~A;                                        01206000
        N(DI~DI+8); M(16(DI~DI+32));                                    01207000
        DI~DI+1; DS~NUM; DS~CHR;                                        01208000
    END;                                                                01209000
        COMMENT SIZE OBTAINS THE ARRAY SIZE FROM A ROW                  01210000
    DESCRIPTOR;                                                         01211000
INTEGER STREAM PROCEDURE SIZE(A);                                       01212000
    BEGIN                                                               01213000
        SI~LOC A; SI~SI+1; DI~LOC SIZE; DI~DI+6;                        01214000
        DS~NUM; DS~CHR;                                                 01215000
    END;                                                                01216000
        COMMENT  MOVEDEC CREATES A DECIMAL STRING 8 CHR WIDE FROM A,    01217000
    F LEADING ZEROES FILLED, AT B +C CHR. E CHR ARE TRANSFERRED         01218000
    STARTING AT CHR D. A BLANK IS ALWAYS EMITTED TO THE RIGHT;          01219000
STREAM PROCEDURE MOVEDEC(A,B,C,D,E,F);                                  01220000
    VALUE A,C,D,E,F;                                                    01221000
    BEGIN                                                               01222000
        SI~LOC A; DI~LOC A; DS~8 DEC; DI~DI-8; DS~F FILL;               01223000
        SI~SI-8; DI~B; DI~DI+C; SI~SI+D; DS~E CHR; DS~LIT " ";          01224000
    END;                                                                01225000
        COMMENT  MASK SETS A BIT IN THE ERROR MASK FOR SYNTAX           01226000
    ERRORS;                                                             01227000
BOOLEAN STREAM PROCEDURE MASK(X); VALUE X; BEGIN                        01228000
    DI~LOC MASK; DI~DI+2; SKIP X DB; DS~SET END;                        01229000
        COMMENT  CR OBTAINS A CHR FROM THE WORD & POSITION GIVEN;       01230000
ALPHA STREAM PROCEDURE CR(S,N);                                         01231000
    VALUE N;                                                            01232000
    BEGIN                                                               01233000
        SI~S; SI~SI+N; DI~LOC CR; DI~DI+7; DS~CHR;                      01234000
    END;                                                                01235000
        COMMENT  DIALB DIALS BLANKS INTO A SEQUENCE. USUALLY USED       01236000
    FOR FILLING PARTIAL FIELDS WHEN SEQUENCE IS NOT LONG ENOUGH;        01237000
STREAM PROCEDURE DIALB(N,M,A,P); VALUE N,M,P;                           01238000
    BEGIN                                                               01239000
        DI~ A;  DI~ DI+ P;                                              01240000
        M(DS~ LIT " ");                                                 01241000
        N(32(DS~2 LIT " "));                                            01242000
    END;                                                                01243000
        COMMENT  CONSTANTCONVERT TAKES A SIGNED OR UNSIGNED NUMBER      01244000
    L CHR LONG AND CONVERTS IT TO A BINARY QUANTITY;                    01245000
INTEGER STREAM PROCEDURE CONSTANTCONVERT(S,L);                          01246000
    VALUE L;                                                            01247000
    BEGIN                                                               01248000
        SI~ S;                                                          01249000
        SI~ SI+3;                                                       01250000
        IF SC="-" THEN BEGIN                                            01251000
            TALLY~1; SI~ SI+1; END ELSE                                 01252000
        IF SC="+" THEN SI~ SI+1;                                        01253000
        DI~ LOC CONSTANTCONVERT;                                        01254000
        DS~ L OCT;                                                      01255000
        DI~ LOC CONSTANTCONVERT;                                        01256000
        L~ TALLY;   L(DS~ LIT "+");                                     01257000
    END;                                                                01258000
STREAM PROCEDURE BLANK(D,S1,S2); VALUE S1,S2;                           01259000
        COMMENT  BLANK BLANKS OUT (64|S1 + S2 + 1) WORDS OF AN          01260000
    ARRAY ROW;                                                          01261000
    BEGIN                                                               01262000
        DI~D; DS~8 LIT " "; SI~D;                                       01263000
        S1(DS~32 WDS; DS~32 WDS);                                       01264000
        DS~S2 WDS;                                                      01265000
    END;                                                                01266000
        COMMENT  MOVE MOVES UP TO 4095 WORDS FROM S TO D;               01267000
STREAM PROCEDURE MOVE(N,S,D);                                           01268000
    VALUE N;                                                            01269000
    BEGIN                                                               01270000
        LOCAL M;                                                        01271000
        SI~LOC N; SI~SI+6; DI~LOC M; DI~DI+7; DS~CHR;                   01272000
        SI~S; DI~D; DS~N WDS; M(DS~32 WDS; DS~32 WDS);                  01273000
    END MOVE;                                                           01274000
        COMMENT  GET GETS WORD A OUT OF THE DICTIONARY;                 01275000
REAL PROCEDURE GET(A);                                                  01276000
    VALUE A;                                                            01277000
    INTEGER A;                                                          01278000
    BEGIN                                                               01279000
$ INCLUDE 01279520, IF COUNTING;                                        01279500
        GETCT ~ *+1;                                                    01279510
        GET ~ V ~ DICT[A.IR,A.IC];                                      01280000
$ INCLUDE 01283001, IF DEBUGGING;                                       01280999
        IF MONITER AND PRINT THEN BEGIN LABEL L; L:                     01281000
            WRITE(PRINTER,MON1," DICT",A.ADINFO,"=",V,                  01282000
                V.CLASS,V.SUB,V.ADINFO)END;                             01283000
    END;                                                                01284000
        COMMENT  PUT PUTS A WORD INTO THE DICTIONARY;                   01285000
PROCEDURE PUT(A,V);                                                     01286000
    VALUE A, V;                                                         01287000
    INTEGER A;                                                          01288000
    REAL V;                                                             01289000
    BEGIN                                                               01290000
$ INCLUDE 01290520, IF COUNTING;                                        01290500
        PUTCT ~ *+1;                                                    01290510
        DICT[A.IR,A.IC] ~ V;                                            01291000
$ INCLUDE 01294001, IF DEBUGGING;                                       01291999
        IF MONITER AND PRINT THEN BEGIN LABEL L; L:                     01292000
            WRITE(PRINTER,MON1," DICT",A.ADINFO,"~",V,                  01293000
                V.CLASS,V.SUB,V.ADINFO)END;                             01294000
    END;                                                                01295000
        COMMENT ERRORLINK LISTS ERRORS FROM STANDARD                    01295020
    LINKED ERROR FILES: "LANGUAG"/"ERRORS" AND "MCALGOL"/"ERRORS";      01295040
PROCEDURE ERRORLINK(L); VALUE L; INTEGER L;                             01295060
BEGIN                                                                   01295080
    INTEGER STREAM PROCEDURE NEXTONE(A,B,C); VALUE B; BEGIN             01295100
        SI~A; DI~LOC NEXTONE; DS~4 OCT; DI~C; DS~4 OCT;                 01295120
        SI~SI-4; DI~A; DS~4 CHR; DS~4 LIT " ";                          01295140
        B(DI~A; DS~4 LIT " ");                                          01295160
    END;                                                                01295180
    LABEL WRONG;                                                        01295200
    INTEGER I,J,K;                                                      01295220
    READ(ERRORS[L],10,ERB[*])[WRONG];                                   01295240
    WHILE J~NEXTONE(ERB,I,K) ! 0 OR (I=0 AND L!K) DO                    01295260
        IF L!K THEN BEGIN                                               01295280
            I ~ -1;                                                     01295300
WRONG:      WRITE(ERB[*],<"00009999ERROR IN ERROR FILE,"                01295320
                " PLEASE NOTIFY SYSTEMS GROUP">);                       01295340
        END ELSE BEGIN                                                  01295360
            WRITE(PRINTER,10,ERB[*]);                                   01295380
            READ(ERRORS[L~J],10,ERB[*]);                                01295400
            I ~ 1;                                                      01295420
        END;                                                            01295440
    WRITE(PRINTER,10,ERB[*]);                                           01295460
END ERRORLINK;                                                          01295480
        COMMENT  ERROR POSTS MCALGOL ERROR & WARNING MESSAGES;          01296000
PROCEDURE ERROR(CODE);                                                  01297000
    VALUE CODE;                                                         01298000
    INTEGER CODE;                                                       01299000
    BEGIN                                                               01300000
        ARRAY TERB[0:14];                                               01301000
        LABEL NOPRINT;                                                  01302000
        INTEGER X, Y;                                                   01303000
STREAM PROCEDURE ADERR(S,SS,W,C,D,E,F);                                 01304000
    VALUE SS,W,C,E,F;                                                   01305000
    BEGIN                                                               01306000
        DI~D; DS~8 LIT " "; SI~D; DS~10 WDS;                            01307000
        DS~8 LIT "*"; SI~SI+8; DS~3 WDS; DI~D; SI~S;                    01308000
        SI~SI+SS; W(DS~8 CHR); DS~C CHR;                                01309000
        SI~LOC F; SI~SI+3; DS~LIT "~"; DS~5 CHR; SI~LOC E; DS~4 DEC;    01310000
        DS~6 LIT " ?"; DI~DI-10; DS~3 FILL;                             01311000
    END;                                                                01312000
        IF CODE<0 THEN BEGIN                                            01313000
            IF NOT WARNTOG THEN GO NOPRINT;                             01314000
            WARNING~ TRUE;                                              01315000
            CODE~ABS(CODE);                                             01316000
            END                                                         01317000
        ELSE                                                            01318000
        ERRCT~ ERRCT+1;                                                 01319000
        IF X~CODE.REF ! 0 THEN BEGIN                                    01320000
            CODE ~ CODE & 0[TOREF];                                     01321000
            ADERR(DICT[Y~X.IR,X~X.IC+2],3,0,DICT[Y,X].CTR,TERB,         01322000
              CODE,IF WARNING THEN "NOTE " ELSE "ERROR");               01323000
        END ELSE                                                        01324000
        IF LASTUSED>0 THEN                                              01325000
            ADERR(ERB,0,BW-BFRMAX+10,BC,TERB,CODE,IF WARNING THEN       01326000
                "NOTE " ELSE "ERROR")                                   01327000
        ELSE                                                            01328000
            BEGIN                                                       01329000
                IF Y~ BW|8+BC-X~ BFRLNG>72 THEN                         01330000
                    X~ X+Y-Y~ 72;                                       01331000
                ADERR(BFR[X.WRD],X.CHR,Y.WRD,Y.CHR,TERB,                01332000
                CODE,IF WARNING THEN "NOTE " ELSE "ERROR");             01333000
            END;                                                        01334000
        MOVE(1, LINKS[1], TERB[11]);                                    01335000
        MOVE(1, LINKS[0], LINKS[1]);                                    01336000
        WRITE(PRINTER[DBL], 15, TERB[*]);                               01337000
        ERRMASK[CODE.[38:5]] ~ ERRMASK[CODE.[38:5]] OR MASK(BOOLEAN     01338000
          (CODE) AND <37>);                                             01339000
        IF ERRCT}100 THEN                                               01340000
            BEGIN                                                       01341000
                WRITE(PRINTER,FLUSH);                                   01342000
                BLVL~ 0;                                                01343000
                GO EOF;                                                 01344000
            END;                                                        01345000
NOPRINT:                                                                01346000
    END;                                                                01347000
        COMMENT  OVRFL CALLS ADOVR TO POST MSTRING OVERFLOWS;           01348000
PROCEDURE OVRFL;                                                        01349000
    BEGIN                                                               01350000
        ARRAY ERB[0:14];                                                01351000
STREAM PROCEDURE ADOVR(D,E,L,M,A);                                      01352000
    VALUE E, L;                                                         01353000
    BEGIN                                                               01354000
        DI~ D;                                                          01355000
        DS~ 21 LIT "MSTRING OVERFLOW>>>>>";                             01356000
        SI~M; SI~SI+3; DS~L CHR; DS~2 LIT " +";                         01357000
        SI~LOC E; DS~4 DEC; DI~A; DS~8 LIT "*";                         01358000
        SI~A; DS~3 WDS;                                                 01359000
    END ADOVR;                                                          01360000
        BLANK(ERB,0,14);                                                01361000
        ERRCT~ ERRCT+1;                                                 01362000
        ADOVR(ERB,EP,GET(MADDR).CTR,DICT[MADDR.IR,MADDR.IC],            01363000
          ERB[11]);                                                     01364000
        MOVE(1, LINKS[1], ERB[11]);                                     01365000
        MOVE(1, LINKS[0], LINKS[1]);                                    01366000
        WRITE(PRINTER[DBL], 15, ERB[*]);                                01367000
    END;                                                                01368000
        COMMENT  SETINUSE SETS A BIT IN SPACEMAP FOR THE                01369000
    FIRST AREA OF AN MSTRING;                                           01370000
STREAM PROCEDURE SETINUSE(MAP,BIT); VALUE BIT; BEGIN                    01371000
    DI~MAP; DI~DI+1; SKIP BIT DB; DS~SET END;                           01372000
        COMMENT  INUSE CHECKS TO SEE IF A SECTOR OF MSTRING             01373000
    SPACE CONTAINS A COUNT FIELD;                                       01374000
BOOLEAN STREAM PROCEDURE INUSE(MAP,BIT); VALUE BIT; BEGIN               01375000
    SI~MAP; SI~SI+1; SKIP BIT SB;                                       01376000
    IF SB THEN BEGIN TALLY~1; INUSE~TALLY END;                          01377000
END;                                                                    01378000
        COMMENT  HOWLONG COUNTS CONTIGUOUS MSTRING SECTORS;             01379000
INTEGER STREAM PROCEDURE HOWLONG(MAP,BIT); VALUE BIT; BEGIN             01380000
    SI~MAP; SI~SI+1; SKIP BIT SB;                                       01381000
    3(SKIP SB; IF SB THEN JUMP OUT ELSE TALLY~TALLY+1);                 01382000
    TALLY~TALLY+1; HOWLONG~TALLY;                                       01383000
END;                                                                    01384000
        COMMENT  M MOVES AN ARBITRARY SEQUENCE FROM ONE ARBITRARY       01385000
    PLACE TO ANOTHER. A TERMINATING SPACE IS ALSO PLACED;               01386000
STREAM PROCEDURE M(S,S1,D,D1,N1,N2,N3);                                 01387000
    VALUE S1, D1, N1, N2, N3;                                           01388000
    BEGIN                                                               01389000
        SI~ S;                                                          01390000
        DI~ D;                                                          01391000
        SI~ SI+S1;                                                      01392000
        DI~ DI+D1;                                                      01393000
        N1(4(32(DS~ 32 CHR)));                                          01394000
        N2(DS~ 32 CHR;DS~ 32 CHR);                                      01395000
        DS~ N3 CHR;                                                     01396000
        SI~LOC N1; IF SC!"+" THEN DS~LIT " ";                           01397000
    END;                                                                01398000
        COMMENT  MOVER CHECKS FOR OVERFLOW, MAINTAINS THE CHR           01399000
    COUNT, AND SHUFFLES SEQUENCES INTO MSTRINGS;                        01400000
INTEGER PROCEDURE MOVER(N,S,SS,D,DS,SZ);                                01401000
    VALUE N,SS,DS,SZ;                                                   01402000
    INTEGER N,SS,DS,SZ;                                                 01403000
    ARRAY S, D[0];                                                      01404000
    BEGIN                                                               01405000
$ INCLUDE 01405520, IF COUNTING;                                        01405500
        MOVCT ~ *+N;                                                    01405510
        IF DS<0 OR N+DS>8|SIZE(D)-1 OR N+DS>8|SZ!0 THEN BEGIN           01406000
            OVRFL;                                                      01407000
            MOVER ~ DS;                                                 01408000
        END ELSE BEGIN                                                  01409000
                MOVER~ DS+N;                                            01410000
                M(S[SS.WRD],SS.CHR,D[DS.WRD],DS.CHR,                    01411000
                  (N~N).C5,N.C6,N);                                     01412000
            END                                                         01413000
    END;                                                                01414000
        COMMENT  P PUTS A CHR & A BLANK INTO AN MSTRING;                01415000
STREAM PROCEDURE P(C,D,D1);                                             01416000
    VALUE C, D1;                                                        01417000
    BEGIN                                                               01418000
        SI~LOC C; DI~D; SI~SI+7; DI~DI+D1; DS~CHR; DS~ LIT " ";         01419000
    END;                                                                01420000
        COMMENT  PUTTER CHECKS FOR OVERFLOW, MAINTAINS THE CHR COUNT    01421000
    AND PLACES ONE CHR INTO AN MSTRING;                                 01422000
INTEGER PROCEDURE PUTTER(C,D,DS);                                       01423000
    VALUE C, DS;                                                        01424000
    INTEGER C, DS;                                                      01425000
    ARRAY D[0];                                                         01426000
    BEGIN                                                               01427000
$ INCLUDE 01427520, IF COUNTING;                                        01427500
        PTRCT ~ *+1;                                                    01427510
        IF DS+1>SIZE(D)|8-2 THEN BEGIN                                  01428000
            OVRFL;                                                      01429000
            PUTTER ~ DS;                                                01430000
        END ELSE BEGIN                                                  01431000
                P(C,D[DS.WRD],DS.CHR);                                  01432000
                PUTTER ~ DS+1;                                          01433000
            END                                                         01434000
    END;                                                                01435000
        COMMENT  INSERT PLACES N CHR, COUNT CHR OVER INTO IDN. NO       01436000
    SPACE FOLLOWS THE INSERTION;                                        01437000
STREAM PROCEDURE INSERT(COUNT,IDN,SPC,N);                               01438000
    VALUE N,COUNT,SPC;                                                  01439000
    BEGIN                                                               01440000
        SI~LOC N; SI~SI-N; DI~IDN; DI~DI+COUNT; DS~N CHR;               01441000
    END INSERT;                                                         01442000
        COMMENT  COMPARE COMPARES (4096|P + 64|M + N) CHR OF TWO        01443000
    MSTRINGS. EQUALITY HOLDS UNTIL FIRST UNEQUAL CHR. RELATION IS       01444000
    THEN MADE ON THESE 2 CHR.                                           01445000
        0 = LOW                                                         01446000
        1 = EQUAL                                                       01447000
        2 = HIGH;                                                       01448000
INTEGER STREAM PROCEDURE COMPARE(S,D,P,M,N);                            01449000
    VALUE P, M, N;                                                      01450000
    BEGIN                                                               01451000
        LABEL L, EX;                                                    01452000
        SI~S; SI~SI+3; DI~D; DI~DI+3;                                   01453000
        P(4(32(32(IF SC!DC THEN JUMP OUT 4 TO L))));                    01454000
        M(2(32(IF SC!DC THEN JUMP OUT 3 TO L)));                        01455000
        N(IF SC!DC THEN JUMP OUT 1 TO L);                               01456000
        TALLY~ 1;                                                       01457000
        GO EX;                                                          01458000
L:      SI~ SI-1;                                                       01459000
        DI~ DI-1;                                                       01460000
        IF SC<DC THEN                                                   01461000
            TALLY~ 0                                                    01462000
        ELSE                                                            01463000
            TALLY~ 2;                                                   01464000
EX:     COMPARE~ TALLY;                                                 01465000
    END COMPARE;                                                        01466000
        COMMENT  SEQN PUTS AN 8 CHR DECIMAL # IN D FROM S;              01467000
STREAM PROCEDURE SEQN(S,D);                                             01468000
    VALUE S;                                                            01469000
    BEGIN                                                               01470000
        SI~LOC S; DI~D; DS~8 DEC;                                       01471000
    END SEQN;                                                           01472000
        COMMENT  SHAFT SHIFTS LEFT UP TO 63 CHR. USED TO RECOVER        01473000
    FROM ASSUMED OCTAL CONSTANT WHICH WAS A RELATION;                   01474000
STREAM PROCEDURE SHAFT(W,C);                                            01475000
    VALUE C;                                                            01476000
    BEGIN                                                               01477000
        SI~W; SI~SI+4; DI~W; DI~DI+3; DS~C CHR; DS~4 LIT " ";           01478000
    END SHAFT;                                                          01479000
        COMMENT IN4 CONVERTS 4 DIGIT DECLARATION # BACK TO BINARY;      01480000
INTEGER STREAM PROCEDURE IN4(A);                                        01481000
    VALUE A;                                                            01482000
    BEGIN                                                               01483000
        SI~LOC A; SI~SI+4; DI~LOC IN4; DS~4 OCT;                        01484000
    END;                                                                01485000
        COMMENT  STACKIT PUSHES AN ARRAY INTO DISKSTACK;                01486000
PROCEDURE STACKIT(S,A); VALUE S; INTEGER S; ARRAY A[0];                 01487000
BEGIN                                                                   01488000
    WRITE(DISKSTACK[DSTK],S,A[*]);                                      01489000
    DSTK ~ DSTK + 1;                                                    01490000
$ INCLUDE 01490530, IF COUNTING;                                        01490500
        STKCT ~ *+1;                                                    01490510
        IF STKDP~*+1>STKMX THEN STKMX~STKDP;                            01490520
END;                                                                    01491000
        COMMENT  UNSTACKIT POPS AN ARRAY OUT OF THE DISKSTACK           01492000
    AND RELOADS THE BUFFER;                                             01493000
PROCEDURE UNSTACKIT(S,A); VALUE S; INTEGER S; ARRAY A[0];               01494000
BEGIN                                                                   01495000
    READ(DISKSTACK[DSTK~DSTK-1],S,A[*]);                                01496000
    IF DSTK > 0 THEN                                                    01497000
        READ SEEK(DISKSTACK[DSTK-1]);                                   01498000
$ INCLUDE 01498530, IF COUNTING;                                        01498500
        STKCT ~ *+1;                                                    01498510
        STKDP ~ *-1;                                                    01498520
END;                                                                    01499000
        COMMENT WRITIT MOVES L WORDS FROM AN MSTRING INTO A TWO-        01500000
    DIMENSIONAL STORAGE SPACE, SPLITTING ACROSS ROWS IF REQD. P IS A    01501000
    CALL BY NAME TO KEEP TRACK OF NEXT FREE SPOT IN F;                  01502000
PROCEDURE WRITIT(A,L,P,F);                                              01503000
    VALUE L;                                                            01504000
    ARRAY A[0],F[0];                                                    01505000
    INTEGER L, P;                                                       01506000
    BEGIN LABEL NP;                                                     01507000
        II~ 0;                                                          01508000
        IF P.REF = 0 THEN IF MSTORP.PDF1 ! P.IR THEN BEGIN              01509000
            IF BOOLEAN(MSTORP).PDP1 THEN BEGIN                          01509050
                WRITE(MSTOR[MSTORP.PDF1],256,F[*]);                     01509100
$ INCLUDE 01509151, IF COUNTING;                                        01509149
                MSTCT ~ * + 1;                                          01509150
                MSTORP.PDP1 ~ 0;                                        01509200
            END;                                                        01509250
            READ(MSTOR[P.IR],256,F[*])[NP] ;                            01510000
$ INCLUDE 01510120, IF COUNTING;                                        01510100
            MSTCT ~ *+1;                                                01510110
        END ELSE ELSE                                                   01510200
        IF P.REF = 1 THEN IF MSTORP.PDF2 ! P.IR THEN BEGIN              01511000
            IF BOOLEAN(MSTORP).PDP2 THEN BEGIN                          01511050
                WRITE(MSTOR[MSTORP.PDF2+150],256,F[*]);                 01511100
$ INCLUDE 01511151, IF COUNTING;                                        01511149
                MSTCT ~ MSTCT + 1;                                      01511150
                MSTORP.PDP2 ~ 0;                                        01511200
            END;                                                        01511250
            READ(MSTOR[P.IR+150],256,F[*])[NP];                         01512000
$ INCLUDE 01512120, IF COUNTING;                                        01512100
            MSTCT ~ *+1;                                                01512110
        END;                                                            01512200
NP:     WHILE L>0 DO                                                    01513000
            BEGIN                                                       01514000
                IF J ~ 256-P.IC > L THEN                                01515000
                    J~ L;                                               01516000
                MOVE(J,A[II],F[P.IC]);                                  01517000
                CASE P.REF OF BEGIN                                     01518000
                BEGIN % MCODE                                           01519000
                    MSTORP.PDF1 ~ P.IR;                                 01520000
                    IF P.IC + J = 256 THEN BEGIN                        01520100
$ INCLUDE 01520520, IF COUNTING;                                        01520500
                    MSTCT ~ *+1;                                        01520510
                    WRITE(MSTOR[P.IR],256,F[*]);                        01521000
                    END ELSE                                            01521100
                    MSTORP.PDP1 ~ 1;                                    01521200
                END;                                                    01522000
                BEGIN % MDECL                                           01523000
                    MSTORP.PDF2 ~ P.IR;                                 01524000
                    IF P.IC + J = 256 THEN BEGIN                        01524100
$ INCLUDE 01524520, IF COUNTING;                                        01524500
                    MSTCT ~ *+1;                                        01524510
                    WRITE(MSTOR[P.IR+150],256,F[*]);                    01525000
                    END ELSE                                            01525100
                    MSTORP.PDP2 ~ 1;                                    01525200
                END;                                                    01526000
                ; % MISC MOVES                                          01527000
                END;                                                    01528000
                P~ P+J;                                                 01529000
                II~ II+J;                                               01530000
                L~ L-J                                                  01531000
            END                                                         01532000
    END;                                                                01533000
        COMMENT READIT RECOVERS L WORDS FROM A 2-DIMENSIONAL ARRAY      01534000
    INTO A 1-DIM ARRAY ROW(MSTRING). P IS NAME TO ALLOW SEQUENTIAL      01535000
    RECOVERIES;                                                         01536000
PROCEDURE READIT(A,L,P,F);                                              01537000
    VALUE L;                                                            01538000
    ARRAY A[0],F[0];                                                    01539000
    INTEGER L, P;                                                       01540000
    BEGIN INTEGER II;                                                   01541000
        II~ 0;                                                          01542000
        WHILE L>0 DO                                                    01543000
            BEGIN                                                       01544000
                IF J ~ 256-P.IC > L THEN                                01545000
                    J~ L;                                               01546000
                CASE P.REF OF BEGIN                                     01547000
                IF MSTORP.PDF1 ! P.IR THEN BEGIN % MCODE                01548000
                    IF BOOLEAN(MSTORP).PDP1 THEN BEGIN                  01548050
                        WRITE(MSTOR[MSTORP.PDF1],256,F[*]);             01548100
                        MSTORP.PDP1 ~ 0;                                01548150
                    END;                                                01548200
                    MSTORP.PDF1 ~ P.IR;                                 01549000
$ INCLUDE 01549520, IF COUNTING;                                        01549500
                    MSTCT ~ *+1;                                        01549510
                    READ(MSTOR[P.IR],256,F[*]);                         01550000
                  END;                                                  01551000
                IF MSTORP.PDF2 ! P.IR THEN BEGIN % MDECL                01552000
                    IF BOOLEAN(MSTORP).PDP2 THEN BEGIN                  01552050
                        WRITE(MSTOR[MSTORP.PDF2+150],256,F[*]);         01552100
                        MSTORP.PDP2 ~ 0;                                01552150
                    END;                                                01552200
                    MSTORP.PDF2 ~ P.IR;                                 01553000
$ INCLUDE 01553520, IF COUNTING;                                        01553500
                    MSTCT ~ *+1;                                        01553510
                    READ(MSTOR[P.IR+150],256,F[*]);                     01554000
                  END;                                                  01555000
                ; % MISC MOVES                                          01556000
                END;                                                    01557000
                MOVE(J,F[P.IC],A[II]);                                  01558000
                P~ P+J;                                                 01559000
                II~ II+J;                                               01560000
                L~ L-J                                                  01561000
            END                                                         01562000
    END;                                                                01563000
        COMMENT  SAVEIT DISK STACKS THE TANK & ELBAT PREPARITORY TO     01564000
    SCANNING "INVISIBLE" DATA SUCH AS DECLARATIONS OR PROCESSING        01565000
    MACROS. MACRO SCANNING IS INVISIBLE TO THE OUTPUT STRING. MACRO     01566000
    OUTPUT WILL LATER BE PSEUDOREAD & SCANNED ON ITS OWN;               01567000
PROCEDURE SAVEIT;                                                       01568000
    BEGIN LABEL L; L:                                                   01569000
        ELBAT[0]~ BELB;                                                 01570000
        ELBAT[1] ~ 0 & (NXTELB-BELB)[TORNELB] & (T-BELB)[TORT]          01571000
            & PANK[TORPANK];                                            01572000
        T~ T-1;                                                         01573000
        MOVE(TANKMAX,TANK,ELBAT[ELBMAX]);                               01574000
        STACKIT(270,ELBAT[*]);                                          01575000
    END;                                                                01576000
        COMMENT  FOLLOWING SECTION CONTAINS THE SCANNERS & MISC;        01577000
        COMMENT  RECALLIT UNSTACKS & RESTORES TANK & ELBAT;             01578000
PROCEDURE RECALLIT;                                                     01579000
    BEGIN LABEL L; L:                                                   01580000
        UNSTACKIT(270,ELBAT[*]);                                        01581000
        MOVE(TANKMAX,ELBAT[ELBMAX],TANK);                               01582000
        NXTELB ~ (BELB~ELBAT[0])+(T~ELBAT[1]).RNELB;                    01583000
        PANK ~ T.RPANK;                                                 01584000
        T ~ T.RT + BELB;                                                01585000
    END;                                                                01586000
COMMENT  SCANNER AND I/O PROCESSING PORTION;                            05000000
        COMMENT  SHIFT SHIFTS AN ELBAT WORD INTO THE ELBAT ARRAY.       05001000
    IF THE ITEM IS ALPHA, THE ALPHA IS MOVED INTO TANK & A POINTER      05002000
    IS LEFT IN ADINFO. IF ELBAT WOULD OVERFLOW, 11 WORDS ARE            05003000
    ROTATED OUT THE BOTTOM;                                             05004000
PROCEDURE SHIFT(W,C,S);                                                 05005000
    VALUE W, C, S;                                                      05006000
    INTEGER W, C, S;                                                    05007000
    BEGIN LABEL L;                                                      05008000
        IF W=0 THEN                                                     05009000
            W ~ 0&C[TOCLASS]&S[TOSUB]                                   05010000
        ELSE                                                            05011000
            BEGIN                                                       05012000
                C~ W.CLASS;                                             05013000
                S~ W.SUB                                                05014000
            END;                                                        05015000
        IF ENDT AND W="." THEN                                          05016000
            IF CIN>0 THEN                                               05017000
                MAXINFO[CIN].SLST ~ 5                                   05018000
            ELSE                                                        05019000
                LASTUSED~ 5;                                            05020000
     IF ENDT THEN ENDT~(NOT(UNTILV{S{ENDV AND C!VOTHERS)AND 0!C!63);    05021000
        IF NOT(ENDT OR CMT) THEN                                        05022000
            BEGIN                                                       05023000
                IF W>63 THEN                                            05024000
                    BEGIN                                               05025000
                        IF(C~(IDENT[1].CTR+10).WRD)+PANK >              05026000
                          TANKMAX THEN                                  05027000
                            PANK~ 0;                                    05028000
                        IF C > TANKMAX THEN BEGIN                       05029000
                            ERROR(103);                                 05030000
                            GO L;                                       05031000
                        END;                                            05032000
                        MOVE(C,IDENT[1],TANK[W~W&PANK[TOADINFO]]);      05033000
                        PANK~ PANK+C                                    05034000
                    END;                                                05035000
L:              IF NXTELB-BELB = ELBMAX THEN                            05036000
                    BEGIN                                               05037000
                        MOVE(ELBMAX-11, ELBAT[11], ELBAT);              05038000
                        BELB~ BELB+11                                   05039000
                    END;                                                05040000
                IF ELBAT[NXTELB-BELB]~ ABS(W)=";" THEN                  05041000
                    IF NEST>0 AND NOT (DEC OR MACROTOG) THEN            05042000
                        IF PROCT[NEST]=0 THEN                           05043000
                            BEGIN                                       05044000
                                IF STREAMTOG AND NOT PROCTOG THEN       05045000
                                    FOR I~ 0 STEP 1 UNTIL 124 DO        05046000
                                    WHILE A~ DLIST[I]<MACRODICT AND     05047000
                                      A}0 DO                            05048000
                                        DLIST[I] ~ DICT[A.IR,A.IC];     05049000
                                NEST~ NEST-1;                           05050000
                                SPCT~ STREAMTOG~ PROCTOG~ FALSE;        05051000
                                WRAPBLOCK(FALSE)                        05052000
                            END;                                        05053000
$ INCLUDE 05056001, IF DEBUGGING;                                       05053999
                IF MONITER AND PRINT THEN                               05054000
                    WRITE(PRINTER,MON1,"ELBAT",NXTELB,"~",W,            05055000
                        W.CLASS,W.SUB,W.ADINFO,IDENT[1],CHAR);          05056000
                NXTELB~ NXTELB+1;                                       05057000
            END;                                                        05058000
        IF CMT THEN CMT ~ ABS(W) ! ";" AND C ! 63;                      05059000
    END;                                                                05060000
        COMMENT  CCHANDLER PROCESSES "*" CARDS & STASHES THEM AWAY      05061000
    IN THE CONTROL CARD FILE. THIS FILE WILL BE ZIPPED AT THE END       05062000
    OF A SUCCESSFUL MCALGOL RUN;                                        05063000
PROCEDURE CCHANDLER;                                                    05064000
    BEGIN                                                               05065000
        REAL X;                                                         05066000
        ARRAY CCA[0:9];                                                 05067000
        BOOLEAN STREAM PROCEDURE DATA(ERB,L); VALUE L; BEGIN            05068000
            DI~LOC L; DS~8 LIT "DATALIST"; DI~DI-8;                     05069000
            SI~ERB; L1: IF SC=" " THEN BEGIN SI~SI+1; GO L1 END;        05070000
            IF 4 SC=DC THEN TALLY~1 ELSE GO XIT;                        05071000
            L2: IF SC=" " THEN BEGIN SI~SI+1; GO L2 END;                05072000
            IF 4 SC=DC THEN TALLY ~ 3;                                  05073000
XIT:            DATA ~ TALLY END;                                       05074000
        FORMAT                                                          05075000
            CC1("?="A1,A6),                                             05076000
            CC2("?ALGOL FILE CARD="A1,A6"/"A1,A6" SERIAL"),             05077000
            CC3("?ALGOL PROCESS="I8),                                   05078000
            CC4("?DATA "I7),                                            05079000
            CC5("?FILE READER= "I7);                                    05080000
        IF CCC=0 THEN                                                   05081000
            BEGIN                                                       05082000
                X~TIME(5);                                              05082850
                    ERROR(112)                                          05082900
                ;                                                       05082950
                WRITE(CC,CC1,X.C1,X);                                   05083000
                X ~ MOVER(71,ERB,0,CCA,1,0);                            05084000
                INSERT(0, CCA, "?", 1);                                 05085000
                DATANAME ~ TIME(1);                                     05086000
                WRITE(CC, 10, CCA[*]);                                  05087000
                WRITE(CC,CC2,MF.C1,MF,F.C1,F);                          05088000
                WRITE(CC,CC5,DATANAME);                                 05089000
                IF X~TIME(10) > 10 THEN % REQUESTED PROCESS IN SECONDS  05089950
                    WRITE(CC,CC3,X/60);                                 05090000
                CCC ~ 4;                                                05091000
            END                                                         05092000
        ELSE                                                            05093000
            BEGIN                                                       05094000
                IF CCDATATOG ~ DATA(ERB,0) THEN BEGIN                   05095000
                    CCLISTOG ~ CCDATATOG.[46:1] OR CCLISTOG;            05096000
                    WRITE(CC,CC4,DATANAME);                             05097000
                    DATANAME ~ TIME(1);                                 05098000
                    READACARD;                                          05099000
                    WHILE CR(LINKS,7)!"*" AND LASTUSED!5 DO BEGIN       05100000
                        WRITE(CC,10,ERB[*]);                            05101000
                        CCC ~ CCC + 1;                                  05102000
                        READACARD;                                      05103000
                    END;                                                05104000
                    CCDATATOG ~ FALSE;                                  05105000
                END;                                                    05106000
                IF LASTUSED ! 5 THEN BEGIN                              05107000
                X ~ MOVER(71,ERB,0,CCA,1,0);                            05108000
                INSERT(0, CCA, "?", 1);                                 05109000
                CCC ~ CCC + 1;                                          05110000
                WRITE(CC, 10, CCA[*])                                   05111000
                END;                                                    05112000
            END;                                                        05113000
    END;                                                                05114000
        COMMENT  OUTPUT PROCESSES AN OUTPUT STRING IMAGE ALREADY        05115000
    FORMATTED THRU FLUSHIT. INTERLISTING OCCURS IN THE LISTING          05116000
    ASYNCHRONOUSLY WITH THE INPUT DUE TO MACRO PROCESSING. THE          05117000
    OUTPUT IS LISTED ONLY AS EACH RECORD REACHES THE OUTER BLOCK;       05118000
PROCEDURE OUTPUT;                                                       05119000
    BEGIN                                                               05120000
        IF ERRCT=0 AND NOT SVT.[46:1] THEN                              05121000
            WRITE(CODE,10,POB[*]);                                      05121100
        CODEREC ~ CODEREC + 1;                                          05122000
        IF INTERLIST AND PRINT THEN BEGIN                               05123000
            BLANK(DECL,0,14);                                           05124000
            MOVE(10,POB,DECL[3]);                                       05125000
            IF DBLINE THEN                                              05126000
                WRITE(PRINTER[DBL],15,DECL[*])                          05127000
            ELSE                                                        05128000
                WRITE(PRINTER,15,DECL[*]);                              05129000
        END;                                                            05130000
        BLANK(POB,0,14);                                                05131000
        SEQN(SEQ~SEQ+100,POB[9]);                                       05132000
        OUTC ~ 0;                                                       05133000
        FLUSHD ~ TRUE;                                                  05134000
    END;                                                                05135000
        COMMENT  READACARD IS RESPONSIBLE FOR KEEPING INPUT IMAGES      05200000
    IN BFR & FLUSHING ACROSS THE ENDS OF PSEUDOREADERS INTO THE         05201000
    NEXT INPUT AREA. FLUSH OF INPUT IMAGE CAUSES READING & MERGING      05202000
    FROM CARD & DISK FILES WITH VOIDING;                                05203000
PROCEDURE READACARD;                                                    05204000
    BEGIN                                                               05205000
        REAL X,Y,A;                                                     05206000
        LABEL CARDS,ACARD,ADISK,COMPARE,EXIT,CARDL,DISKL,EQUAL,         05207000
          DISKS,EOF0,START,BOTH,SET,EOF1,EOF2,EOF3,EOF4,EOF5,           05208000
          EOF6,LOOP;                                                    05209000
        SWITCH WHICH ~ CARDS,ACARD,ADISK,DISKS,EOF0,BOTH;               05210000
        SWITCH WITCH ~ CARDL,DISKL,EQUAL;                               05211000
        INTEGER STREAM PROCEDURE COMPAR(C, D);                          05212000
        BEGIN SI~C; DI~D;                                               05213000
            IF 8 SC{DC THEN BEGIN                                       05214000
                SI~C; DI~D;                                             05215000
                IF 8 SC<DC THEN TALLY~1 ELSE TALLY~3;                   05216000
            END ELSE TALLY~2;                                           05217000
            COMPAR~TALLY;                                               05218000
        END COMPAR;                                                     05219000
        INTEGER STREAM PROCEDURE MKABS(A); BEGIN SI~A; MKABS~SI END;    05220000
        BOOLEAN STREAM PROCEDURE NOTPRES(A); BEGIN                      05221000
            SI~A; SKIP 2 SB; IF SB THEN ELSE TALLY~1;                   05222000
            NOTPRES~TALLY;                                              05223000
        END;                                                            05224000
        INTEGER STREAM PROCEDURE LINK(A); BEGIN LOCAL B;                05225000
            DI~LOC B; DS~5 LIT "?LINK"; DI~DI-5; SI~A;                  05226000
            IF SC!DC THEN GO X;                                         05227000
L1:         IF SC=" " THEN BEGIN SI~SI+1; GO L1 END;                    05228000
            IF 4 SC!DC THEN GO X;                                       05229000
L2:         IF SC=" " THEN BEGIN SI~SI+1; GO L2 END;                    05230000
            DI~LOC B; IF SC="+" THEN BEGIN DS~CHR;                      05231000
L3:             IF SC=" " THEN BEGIN SI~SI+1; GO L3 END;                05232000
            END ELSE DS~LIT "0";                                        05233000
            8(IF SC }"0" THEN BEGIN SI~SI+1; TALLY~TALLY+1 END          05234000
                ELSE JUMP OUT);                                         05235000
            A~TALLY; DI~LOC LINK; SI~SI-A; DS~A OCT;                    05236000
            DI~DI-8; SI~LOC B; DS~CHR;                                  05237000
X:      END LINK;                                                       05238000
START: LOOP: IF LASTUSED>0 THEN                                         05239000
            BEGIN                                                       05240000
                RCCNT~ RCCNT+1;                                         05241000
                GO WHICH[LASTUSED]                                      05242000
            END;                                                        05243000
        BW ~ (X~MAXINFO[CIN~CIN-1]).SBW;                                05244000
        BC ~ X.SBC;                                                     05245000
        BFRLNG ~ X.SLNG;                                                05246000
        BWMAX ~ X.SMAX;                                                 05247000
        RESCANTOG ~ BOOLEAN(X).SRSC;                                    05248000
        ENDT ~ BOOLEAN(X).SENDT OR ENDT;                                05249000
        IF LASTUSED ~ X.SLST=5 AND MACT=0 THEN                          05250000
            GO TO EOF;                                                  05251000
        IF NOREAD THEN X~TELBAT[MEL] ELSE                               05251900
        FOR I~ MEL-X~ TELBAT[MEL] STEP 1 UNTIL MEL-1 DO                 05252000
            BEGIN                                                       05253000
                IF(A~ TELBAT[I]).CLASS!0 THEN                           05254000
                    MOVE((TTANK[A].CTR+10).WRD,TTANK[A],IDENT[1]);      05255000
                SHIFT(A, 0, 0)                                          05256000
            END;                                                        05257000
        MEL ~ MEL-X-1; CHAR ~ " ";                                      05258000
        GO TO EXIT;                                                     05259000
CARDS:  IF NOTPRES(CARD) THEN                                           05260000
            BEGIN                                                       05261000
                WAIT(MKABS(CARD), REAL(<1000000004000000>));            05262000
                IF NOTPRES(CARD) THEN                                   05263000
                    BEGIN                                               05264000
                        LASTUSED~ 5;                                    05265000
                        GO TO EOF0;                                     05266000
                    END                                                 05267000
            END;                                                        05268000
        IF I~LINK(CARD(0))!0 THEN BEGIN                                 05269000
            CARDREC ~ IF I<0 THEN CARDREC-I ELSE I;                     05270000
            READ(CARD[CARDREC]);                                        05271000
            GO CARDS;                                                   05272000
        END;                                                            05273000
        MOVE(10,CARD(0),BFR[BFRMAX-10]);                                05274000
        CARDREC~ CARDREC+1;                                             05275000
        READ(CARD)[EOF1];                                               05276000
EOF1:   GO TO SET;                                                      05277000
ACARD:  IF NOTPRES(CARD) THEN                                           05278000
            BEGIN                                                       05279000
                WAIT(MKABS(CARD), REAL(<1000000004000000>));            05280000
                IF NOTPRES(CARD) THEN                                   05281000
                    BEGIN                                               05282000
                        LASTUSED~ 4;                                    05283000
                        GO TO DISKS                                     05284000
                    END                                                 05285000
            END;                                                        05286000
        IF I~LINK(CARD(0))!0 THEN BEGIN                                 05287000
            CARDREC ~ IF I<0 THEN CARDREC-I ELSE I;                     05288000
            READ(CARD[CARDREC]);                                        05289000
            GO ACARD;                                                   05290000
        END;                                                            05291000
        GO TO COMPARE;                                                  05292000
ADISK:  IF NOTPRES(DISK) THEN                                           05293000
            BEGIN                                                       05294000
                WAIT(MKABS(DISK), REAL(<1000000004000000>));            05295000
                IF NOTPRES(DISK) THEN                                   05296000
                    BEGIN                                               05297000
                        LASTUSED~ 1;                                    05298000
                        GO TO CARDS;                                    05299000
                    END                                                 05300000
            END;                                                        05301000
        IF I~LINK(DISK(0))!0 THEN BEGIN                                 05302000
            DISKREC ~ IF I<0 THEN DISKREC-I ELSE I;                     05303000
            READ(DISK[DISKREC]);                                        05304000
            GO ADISK;                                                   05305000
        END;                                                            05306000
        GO TO COMPARE;                                                  05307000
DISKS:  IF NOTPRES(DISK) THEN                                           05308000
            BEGIN                                                       05309000
                WAIT(MKABS(DISK), REAL(<1000000004000000>));            05310000
                IF NOTPRES(DISK) THEN                                   05311000
                    BEGIN                                               05312000
                        LASTUSED~ 5;                                    05313000
                        GO TO EOF0;                                     05314000
                    END                                                 05315000
            END;                                                        05316000
        IF I~LINK(DISK(0))!0 THEN BEGIN                                 05317000
            DISKREC ~ IF I<0 THEN DISKREC-I ELSE I;                     05318000
            READ(DISK[DISKREC]);                                        05319000
            GO DISKS;                                                   05320000
        END;                                                            05321000
        MOVE(10,DISK(0),BFR[BFRMAX-10]);                                05322000
        DISKREC~ DISKREC+1;                                             05323000
        READ(DISK)[EOF2];                                               05324000
EOF2:   GO TO SET;                                                      05325000
BOTH:   IF NOTPRES(DISK) THEN                                           05326000
            BEGIN                                                       05327000
                WAIT(MKABS(DISK), REAL(<1000000004000000>));            05328000
                IF NOTPRES(DISK) THEN                                   05329000
                    BEGIN                                               05330000
                        LASTUSED~ 1;                                    05331000
                        GO TO CARDS                                     05332000
                    END                                                 05333000
            END;                                                        05334000
        IF I~LINK(DISK(0))!0 THEN BEGIN                                 05335000
            DISKREC ~ IF I<0 THEN DISKREC-I ELSE I;                     05336000
            READ(DISK[DISKREC]);                                        05337000
            GO BOTH;                                                    05338000
        END;                                                            05339000
        GO TO ACARD;                                                    05340000
COMPARE: GO TO WITCH[COMPAR(CARD(9), DISK(9))];                         05341000
CARDL:  MOVE(10,CARD(0),BFR[BFRMAX-10]);                                05342000
        CARDREC~ CARDREC+1;                                             05343000
        READ(CARD)[EOF3];                                               05344000
EOF3:   LASTUSED~ 2;                                                    05345000
        GO TO SET;                                                      05346000
DISKL:  MOVE(10,DISK(0),BFR[BFRMAX-10]);                                05347000
        DISKREC~ DISKREC+1;                                             05348000
        READ(DISK)[EOF4];                                               05349000
EOF4:   LASTUSED~ 3;                                                    05350000
        GO TO SET;                                                      05351000
EQUAL:  MOVE(10,CARD(0),BFR[BFRMAX-10]);                                05352000
        CARDREC~ CARDREC+1; DISKREC~ DISKREC+1;                         05353000
        LASTUSED~ 6;                                                    05354000
        READ(CARD)[EOF5];                                               05355000
EOF5:   READ(DISK)[EOF6];                                               05356000
EOF6:   GO TO SET;                                                      05357000
EOF0:   IF MACT=0 THEN                                                  05358000
            GO EOF;                                                     05359000
        BLANK(ERB,0,14);                                                05360000
        BW~ 1013;                                                       05361000
        BC~ 0;                                                          05362000
        ERROR(48);                                                      05363000
        GO EOF;                                                         05364000
SET:    MOVE(1,BFR[BFRMAX-1],LINKS);                                    05365000
        IF VOIDING THEN                                                 05366000
            IF COMPAR(LINKS[0],LINKS[2])}2 THEN                         05367000
                VOIDING~ FALSE                                          05368000
            ELSE                                                        05369000
                GO START;                                               05370000
        IF NEWFILE THEN BEGIN LABEL L; L:                               05371000
            MOVE(10,BFR[BFRMAX-10],NEWDISK(0));                         05372000
            IF NEWRESEQ THEN BEGIN                                      05373000
                SEQN(NEWSEQ~NEWSEQ+NEWINC,NEWDISK(9));                  05374000
                IF X~CR(LINKS,7)="$" OR X="*" THEN                      05375000
                    INSERT(7,NEWDISK(9),X,1);                           05376000
                MOVE(1,NEWDISK(9),LINKS) END;                           05377000
            WRITE(NEWDISK);                                             05378000
        END;                                                            05379000
        BLANK(ERB,0,14);                                                05380000
        MOVE(10,BFR[BW~BFRMAX-10],ERB);                                 05381000
        INSERT(0,BFR[BFRMAX-1],"%",1);                                  05382000
        BC~ 0;                                                          05383000
        BWMAX ~ 8|BFRMAX - 8;                                           05384000
        LUOK~ TRUE;                                                     05385000
EXIT:   IF LASTUSED>0 THEN                                              05386000
            SAVEML~ ML;                                                 05387000
        IF X~ CR(LINKS[0], 7)="$" THEN                                  05388000
            BEGIN                                                       05389000
                IF MDOLLAR(FALSE) THEN;                                 05390000
                GO LOOP                                                 05391000
            END;                                                        05392000
        IF Y~CR(BFR[BFRMAX-10],0) = "$" THEN                            05392050
            IF CR(BFR[BFRMAX-10],1) = "*" THEN                          05392100
                IF NOT MDOLLAR(TRUE) THEN                               05392150
                    GO LOOP;                                            05392200
        IF X="*" OR CCDATATOG THEN LUOK ~ FALSE;                        05393000
        IF PRINT AND LUOK THEN BEGIN                                    05394000
$ INCLUDE 05395021, IF NOT INTERMEDIATE AND NOT ADVANCED;               05394999
                FORMAT TOP(X46"WRL MCALGOL BASIC VERSION OF "           05395000
                    O". LANGUAGE IS "A1,A6);                            05395020
$ INCLUDE 05395051, IF INTERMEDIATE AND NOT ADVANCED;                   05395030
                FORMAT TOP(X46"WRL MCALGOL INTERMEDIATE VERSION OF "    05395040
                    O". LANGUAGE IS "A1,A6);                            05395050
$ INCLUDE 05395081, IF INTERMEDIATE AND ADVANCED;                       05395060
                FORMAT TOP(X46"WRL MCALGOL ADVANCED VERSION OF "        05395070
                O". LANGUAGE IS "A1,A6);                                05395080
                IF NOT WRITEIT THEN                                     05396000
                    BEGIN                                               05397000
                    WRITE(PRINTER[NO],TOP,DATE,LANG.C1,LANG);           05398000
                    TIMEIT(PRINTER);                                    05399000
                    WRITEIT ~ TRUE;                                     05400000
                    END;                                                05401000
                LUOK~ FALSE;                                            05402000
                MOVEDEC(RCCNT,ERB[9],0,1,7,7);                          05403000
                MOVEDEC(SEQ, ERB[13], 7, 0, 8, 0);                      05404000
                MOVE(1, LINKS[0], ERB[10]);                             05405000
                MOVEDEC(SAVEML, ERB[11], 1, 4, 4, 8);                   05406000
                IF 3{LASTUSED{4 THEN BEGIN                              05407000
                    MOVEDEC(DISKREC,ERB[11],7,3,5,7);                   05408000
                    INSERT(4,ERB[12],"D",1);                            05409000
                    END                                                 05410000
                ELSE BEGIN                                              05411000
                    MOVEDEC(CARDREC,ERB[11],7,3,5,7);                   05412000
                    INSERT(5,ERB[12],"C",1);                            05413000
                    END;                                                05414000
                IF DBLINE THEN                                          05415000
                    WRITE(PRINTER[DBL], 15, ERB[*])                     05416000
                ELSE                                                    05417000
                    WRITE(PRINTER, 15, ERB[*]);                         05418000
            END;                                                        05419000
        IF Y = "$" THEN BEGIN                                           05420000
%                                                                       05421000
                IF OUTC>0 THEN                                          05425000
                    OUTPUT;                                             05426000
                MOVE(9,BFR[BFRMAX-10],POB);                             05427000
                OUTPUT;                                                 05428000
                GO START;                                               05429000
            END;                                                        05430000
        IF X="*" AND NOT CCDATATOG THEN                                 05431000
            BEGIN                                                       05432000
                CCHANDLER;                                              05433000
                GO TO START                                             05434000
            END;                                                        05435000
    END;                                                                05436000
        COMMENT  PSEUDOREAD IS RESPONSIBLE FOR STACKING -C- CHRS        05500000
    FROM MSTRING A. ANY PRESCANNED ITEMS ARE SAVED IN TELBAT &          05501000
    TANK TO BE RESTORED WHEN THIS PSEUDOREADER IS FLUSHED;              05502000
PROCEDURE PSEUDOREAD(A,C,Z);                                            05503000
    VALUE C,Z;                                                          05504000
    INTEGER C,Z;                                                        05505000
    ARRAY A[0];                                                         05506000
    BEGIN                                                               05507000
        FORMAT F1("PSEUDOREADER"I4"@"I4"["I4"-"I4"]U="I1,               05508000
                2L1";SZ="I4" I="I3);                                    05509000
        STREAM PROCEDURE BWBC(BWC,A); VALUE BWC; BEGIN                  05510000
            DI~A; DS~8 LIT " "; SI~A; DS~14 WDS;                        05511000
            SI~LOC BWC; DI~A; DS~4 DEC END;                             05512000
        IF MEL+I~NXTELB-T-1 > 255 THEN                                  05514000
            OVRFL                                                       05515000
        ELSE                                                            05516000
            BEGIN                                                       05517000
$ INCLUDE 05520001, IF DEBUGGING;                                       05517999
            IF PSEUDOTOG AND PRINT THEN                                 05518000
                WRITE(PRINTER,F1,CIN,8|BW+BC,BFRLNG,BWMAX,              05519000
                    LASTUSED,RESCANTOG,ENDT,C,I);                       05520000
                FOR II~ 1 STEP 1 UNTIL I DO                             05521000
                    IF(W~ ELBAT[T+II-BELB]).CLASS!0 THEN                05522000
                        BEGIN                                           05523000
                            IF(J~(TANK[W].CTR+10).WRD)+TPANK >          05524000
                              TANKMAX THEN                              05525000
                                TPANK~ 0;                               05526000
                            MOVE(J, TANK[W], TTANK[TPANK]);             05527000
                            TELBAT[MEL+II] ~ W&TPANK[TOADINFO];         05528000
                            TPANK~ TPANK+J                              05529000
                        END                                             05530000
                    ELSE                                                05531000
                        TELBAT[MEL+II]~ W;                              05532000
                NXTELB~ T+1;                                            05533000
                IF I<0 THEN I~0;                                        05534000
                TELBAT[MEL~ MEL+I+1]~ I                                 05535000
            END;                                                        05536000
        IF CIN>255 THEN                                                 05537000
            BEGIN                                                       05538000
            ERROR(56);WRITE(PRINTER[DBL]);WRITE(PRINTER,FLUSH);         05539000
            GO EOF;                                                     05540000
            END;                                                        05541000
        MAXINFO[CIN] ~ BW&BC[TOSBC]&BFRLNG[TOSLNG]&BWMAX[TOSMAX]&       05542000
       LASTUSED[TOSLST]&REAL(RESCANTOG)[TOSRSC]&REAL(ENDT)[TOSENDT];    05543000
        LASTUSED~ 0;                                                    05544000
        BWMAX ~ MOVER(C-1,A,8|Z+3,BFR,BW~BW|8+BC-1-C,0)+1;              05545000
        BC~(BFRLNG~ BW)-(BW~ BW.WRD)|8;                                 05546000
        INSERT(BWMAX.CHR,BFR[BWMAX.WRD],"%",1);                         05547000
$ INCLUDE 05547520, IF COUNTING;                                        05547500
    IF BW < PSEU THEN PSEU ~ BW;                                        05547510
$ INCLUDE 05560001, IF DEBUGGING;                                       05547999
        IF PSEUDOTOG AND PRINT THEN BEGIN                               05548000
            INTEGER R,L,B;                                              05548100
                R ~ (B~8|BFRMAX-7-8|BW-BC) DIV 104;                     05549000
                B ~ B MOD 104;                                          05550000
                BWBC(8|BW+BC,PPB);                                      05551000
                M(BFR[BW],BC,PPB[(L~112-B).WRD],L.CHR+1,0,              05552000
                    (B~B-1).C6,B);                                      05553000
                WRITE(PRINTER,15,PPB[*]);                               05554000
                FOR L~1 STEP 1 UNTIL R DO BEGIN                         05555000
                    BWBC((B~BFRMAX-14-(R-L)|13)|8,PPB);                 05556000
                    MOVE(13,BFR[B],PPB[1]);                             05557000
                    WRITE(PRINTER,15,PPB[*]);                           05558000
                END;                                                    05559000
            END;                                                        05560000
        CIN~ CIN+1;                                                     05561000
        ENDT~ RESCANTOG~ FALSE;                                         05562000
    END;                                                                05563000
        COMMENT  INDICT LOOKS UP IDENTIFIER IN IDENT AND RETURNS        05600000
    LOCATION IF FOUND, NEGATIVE # FOR SPECIAL ACTION OR NOT FOUND.      05601000
    W CONTAINS ELBAT WORD. LAD CONTAINS ADINFO FIELD;                   05602000
INTEGER PROCEDURE INDICT;                                               05603000
    BEGIN                                                               05604000
        LABEL TEST,TEST2,FOUND,DEFYN,NOPE,EXIT;                         05605000
        INTEGER NW, D;                                                  05606000
        BOOLEAN BOOL;                                                   05607000
NW~ DLIST[IDENT[1] MOD 125];                                            05608000
$ INCLUDE 05608520, IF COUNTING;                                        05608500
INDCT ~ *+1;                                                            05608510
IF NW}0 THEN DO                                                         05609000
%                                                                       05610000
    IF DICT[NW.IR,NW.IC+2] = D~IDENT[1] THEN                            05611000
        IF COMPARE(DICT[NW.IR,NW.IC+2],IDENT[1],                        05612000
              0,(D~D.CTR).C6,D) = 1 THEN BEGIN % MATCH                  05613000
    LAD ~ (W~DICT[NW.IR,NW.IC+1].[9:39]).ADINFO;                        05614000
    IF W.CLASS = VDEFINEDID AND MACROTOG THEN                           05615000
        GO DEFYN;                                                       05616000
    IF LASTUSED > 0 OR RESCANTOG THEN                                   05617000
        IF EX EQV (DEC OR MACROTOG)THEN                                 05618000
            GO TEST;                                                    05619000
    IF VDEFINEDID ! W.CLASS ! VMACRONAME THEN                           05620000
        GO TEST2;                                                       05621000
    IF EQVTOG THEN                                                      05622000
        GO FOUND;                                                       05623000
    GO NOPE;                                                            05624000
TEST:   IF W.CLASS = VDEFINEDID THEN BEGIN                              05625000
DEFYN:      IF CMT OR ENDT THEN BEGIN % FORGETIT                        05626000
                INDICT ~ -1;                                            05627000
                GO EXIT;                                                05628000
            END;                                                        05629000
            IF W.SUB > 4 THEN BEGIN % PARAMETRIC, LET TABLE DO IT       05630000
                INDICT ~ -5;                                            05631000
                GO EXIT;                                                05632000
            END;                                                        05633000
%                                                                       05634000
            LAD.REF ~ 1;                                                05638000
            READIT(DFN,1,LAD,MDECL);                                    05638100
            LAD ~ LAD - 1;                                              05638200
            D ~ DFN[0].CTR + 10;                                        05639000
            READIT(DFN,D.WRD,LAD,MDECL);                                05640000
            LAD ~ W.ADINFO;                                             05641000
            IF W.SUB = 0 THEN BEGIN % 1 ID                              05642000
                MOVE(D.WRD,DFN,IDENT[1]);                               05643000
                INDICT ~ INDICT; % RECURSIVE LOOKUP                     05644000
                GO EXIT;                                                05645000
            END;                                                        05646000
            BOOL ~ LASTUSED > 0 OR RESCANTOG;                           05647000
            IF CHAR ! " " THEN % BACK UP BFR 1 CHR                      05648000
                BC ~ -(BW~(NW~8|BW+BC-1).WRD)|8 + NW;                   05649000
            IF W.SUB = 2 THEN BEGIN % 1 SP CHR                          05650000
                CHAR ~ CR(DFN,3);                                       05651000
                INDICT ~ -4;                                            05652000
                GO EXIT;                                                05653000
            END;                                                        05654000
            PSEUDOREAD(DFN,D-9,0);                                      05655000
            RESCANTOG ~ BOOL;                                           05656000
            NXTELB ~ NXTELB - 1;                                        05657000
            INDICT ~ -3;                                                05658000
            GO EXIT;                                                    05659000
        END;                                                            05660000
TEST2:                                                                  05661000
$ INCLUDE 05666998, IF ADVANCED;                                        05661099
        IF W.CLASS=VOTHERS AND ELB.SUB=REPLACEV THEN BEGIN              05661100
            D ~ (DICT[W.IR,W.IC].CTR+10).WRD;                           05662000
            MOVE(D,DICT[W.IR,W.IC],IDENT[1]);                           05663000
            INDICT ~ -2;                                                05664000
            GO EXIT;                                                    05665000
        END;                                                            05666000
    IF W.CLASS = VMACRONAME THEN                                        05666050
        IF D~DICT[W.IR,W.IC].MCIF ! 0 THEN                              05666100
            IF REAL(MASK(D-1)AND USERTOG)=0 THEN                        05666150
                GO NOPE;                                                05666200
FOUND:  INDICT ~ NW + 1;                                                05667000
        GO EXIT;                                                        05668000
NOPE: END UNTIL NW ~ DICT[NW.IR,NW.IC] < 0;                             05669000
        INDICT ~ -1;                                                    05670000
EXIT:   END INDICT;                                                     05671000
        COMMENT  ADDICT ENTERS IDENTIFIER IN IDENT INTO DICT WITH       05672000
    AN ELBAT WORD CREATED FROM THE CLASS & SUBCLASS PARAMETERS.         05673000
    TWO WORDS BEYOND ENTRY ARE ZEROED TO PREVENT FLAG BITS FROM OLD     05674000
    CUTBACK ENTRIES;                                                    05675000
INTEGER PROCEDURE ADDICT(CODEWORD,SUBCODE);                             05676000
    VALUE CODEWORD, SUBCODE;                                            05677000
    INTEGER CODEWORD, SUBCODE;                                          05678000
    BEGIN                                                               05679000
        INTEGER LENGTH,NW,I,J;                                          05680000
        IF CODEWORD=VOTHERS AND SUBCODE=REPLACEV THEN                   05681000
            LENGTH~ IDENT[0]                                            05682000
        ELSE                                                            05683000
            LENGTH ~ (IDENT[1].CTR+10).WRD;                             05684000
        NW~ DLIST[I~ IDENT[1] MOD 125];                                 05685000
        IF (J~NEXTDICT).IC>254-LENGTH THEN BEGIN                        05686000
            NEXTDICT ~ 0&(NEXTDICT+256)[IRTOIR];                        05687000
            IF BLIST[BLVL].BNXTD = J THEN                               05688000
                BLIST[BLVL].BNXTD ~ NEXTDICT;                           05689000
            END;                                                        05690000
        J~ NEXTDICT-LASTDICT;                                           05691000
        LASTDICT~ NEXTDICT;                                             05692000
        DLIST[I]~ NEXTDICT;                                             05693000
        PUT(NEXTDICT, NW);                                              05694000
        ADDICT~ NEXTDICT+1;                                             05695000
        MOVE(LENGTH,IDENT[1],DICT[NEXTDICT.IR,NEXTDICT.IC+2]);          05696000
        PUT(NEXTDICT+1,(NEXTDICT~ NEXTDICT+LENGTH+2)&(64|SUBCODE+       05697000
          CODEWORD)[TOCLSUB]&BLVL[TOBLKLVL]&J[TODIFF]);                 05698000
        DICT[NEXTDICT.IR,NEXTDICT.IC] ~ DICT[(NW~NEXTDICT+1)            05699000
            .IR,NW.IC] ~ 0;                                             05700000
    END ADDICT;                                                         05701000
        COMMENT  SETPACK PUTS  TANKED ALPHA INTO ACCM DURING A SCAN;    05702000
PROCEDURE SETPACK;                                                      05703000
BEGIN                                                                   05703100
    PACKLAST ~ TRUE;                                                    05703200
    HM ~ MOVER(TANK[ELB].CTR,TANK,3&ELB[CFX8],ACCM,HM,0)+1;             05704000
END;                                                                    05704100
        COMMENT  PACKET PACKS TANKED ALPHA INTO DECL. CONTAINS          05705000
    PACKING ALGORITHM TO KEEP IDENTIFIERS FROM BREAKING ACROSS          05706000
    "CARDS" AND KEEP QUESTION MARKS OUT OF "COLUMN 1". DECLARATIONS,    05707000
    UNLESS PROGRAMMATICALLY DECOMPOSED & ALTERED, ARE LEFT PACKED       05708000
    GOING INTO THE OUTPUT STRING;                                       05709000
PROCEDURE PACKET;                                                       05710000
    BEGIN                                                               05711000
        INTEGER I, J;                                                   05712000
        INTEGER K,L,M,N,O;                                              05713000
        BOOLEAN OK;                                                     05714000
        LABEL MOVITOUT;                                                 05715000
        IF(NWCT+(I~IF ELB>63 THEN TANK[ELB].CTR ELSE 1)+7).WRD >        05716000
          255 THEN                                                      05717000
MOVITOUT:   BEGIN                                                       05718000
                J ~ (NWCT+6).WRD;                                       05719000
                WRITIT(DECL, J, DECP, MDECL);                           05720000
                TNWCT~ 8|J+TNWCT;                                       05721000
                BLANK(DECL,3,63);                                       05722000
                NWCT~ 4;                                                05723000
            END;                                                        05724000
        IF PREV AND ELB<64 AND NWCT>4 THEN                              05725000
            NWCT~ NWCT-1;                                               05726000
        IF ELB>63 THEN BEGIN                                            05727000
            IF J~(NWCT-3) DIV 72 ! K~(NWCT+I-3) DIV 72 THEN BEGIN       05728000
                IF K-J = 1 THEN                                         05729000
                    NWCT ~ K|72 + 3                                     05730000
                ELSE BEGIN                                              05731000
                    M ~ 75;                                             05732000
                    N ~ ELB.ADINFO;                                     05733000
                    DO BEGIN                                            05734000
                        OK ~ TRUE;                                      05735000
                        FOR L~M STEP 72 UNTIL I+3 DO                    05736000
                            IF CR(TANK[(O~N|8+L).WRD],O.CHR)=12         05737000
                            THEN BEGIN                                  05738000
                                OK ~ FALSE;                             05739000
                                L ~ I+3;                                05740000
                            END;                                        05741000
                    END UNTIL OK OR (M~M-1){3;                          05742000
                    IF ((NWCT~(J+1)|72+78-M)+I+7).WRD > 255 THEN        05743000
                        GO MOVITOUT;                                    05744000
            END END;                                                    05745000
            NWCT ~ MOVER(I,TANK,3&ELB[CFX8],DECL,NWCT,0)                05746000
        END ELSE                                                        05747000
            NWCT~ PUTTER(ELB,DECL,NWCT);                                05748000
        NWCT~ NWCT+REAL(PREV~ELB>63);                                   05749000
    END PACKET;                                                         05750000
        COMMENT SETINSERT PUTS SPECIAL CHR IN ACCM DURING SCANS;        05751000
PROCEDURE SETINSERT;                                                    05752000
BEGIN                                                                   05752100
    HM ~ HM - REAL(PACKLAST);                                           05752200
    PACKLAST ~ FALSE;                                                   05752300
    HM ~ PUTTER(ELB,ACCM,HM);                                           05753000
END;                                                                    05753100
        COMMENT  SCA IS THE NORMAL DEBLANKING ITEM SCANNER. NO          05754000
    LOCALS NOR JUMPS >63 SYLLABLES MAY BE PATCHED INTO THIS             05755000
    ROUTINE AND HAVE IT WORK RIGHT. THE FINAL STREAM ADDRESS IS         05756000
    PLACED ON TOP OF LITC 0 IN CALLS ON SCA TO DETERMINE LENGTH         05757000
    OF ITEM. IF CHR TO RIGHT!" ", CHAR IS SET TO IT FOR LOOK-AHEAD;     05758000
INTEGER STREAM PROCEDURE SCA(BW,AW,TYPE,BC,AC,TYPEV,CT,D);              05759000
    VALUE BC, AC, TYPEV;                                                05760000
    BEGIN                                                               05761000
        LABEL DEBL, INT, IDEN, GETI, L, IDEN1, GETA;                    05762000
        SI~BW; SCA~SI; SI~SI+BC; DI~AW; DI~DI+AC;                       05763000
        CI~ CI+TYPEV;                                                   05764000
        GO TO DEBL;                                                     05765000
        GO TO INT;                                                      05766000
        GO TO IDEN1;                                                    05767000
        GO TO DEBL;                                                     05768000
        GO TO GETA;                                                     05769000
        GO TO GETI;                                                     05770000
GETI:   IF SC=" " THEN BEGIN SI~SI+1; GO GETI END;                      05771000
        IF SC}"0" THEN BEGIN BC~SI; SI~D; SI~SI+7;                      05772000
            TALLY~TALLY+1; DS~CHR; SI~BC; END;                          05773000
        GO TO INT;                                                      05774000
GETA:   IF SC=" " THEN BEGIN SI~SI+1; GO GETA END;                      05775000
        IF SC=ALPHA THEN BEGIN BC~SI; SI~D; SI~SI+7;                    05776000
            TALLY~TALLY+1; DS~CHR; SI~BC; END;                          05777000
IDEN1:  GO TO IDEN;                                                     05778000
DEBL:   IF SC=" " THEN BEGIN SI~SI+1; GO DEBL END;                      05779000
        IF SC}"0" THEN                                                  05780000
            BEGIN                                                       05781000
INT:            63(IF SC}"0" THEN BEGIN DS~CHR;TALLY~TALLY+1;END        05782000
                  ELSE JUMP OUT);                                       05783000
                BC~ TALLY;                                              05784000
                TALLY~ 1;                                               05785000
                DS~ 4 LIT " ";                                          05786000
            END                                                         05787000
        ELSE                                                            05788000
            IF SC=ALPHA THEN                                            05789000
                BEGIN                                                   05790000
IDEN:               63(IF SC=ALPHA THEN BEGIN DS~ CHR;TALLY~ TALLY+1    05791000
                      END ELSE JUMP OUT);                               05792000
                    BC~ TALLY;                                          05793000
                    TALLY~ 2;                                           05794000
                    DS~ 4 LIT " ";                                      05795000
                END ELSE BEGIN                                          05796000
                    BC~TALLY; TALLY~3;                                  05797000
                END;                                                    05798000
        TYPEV~TALLY; DI~D; DS~7 LIT "0";                                05799000
        IF SC=ALPHA THEN DS~LIT " " ELSE DS~CHR;                        05800000
        AC~SI; SI~LOC BC; DI~CT; DS~WDS;                                05801000
        DI~LOC SCA; DI~DI-24; DS~WDS; % TRICK.....                      05802000
        DI~TYPE; DS~WDS;                                                05803000
    END;                                                                05804000
        COMMENT  MDOLLAR PROCESSES MCALGOL DOLLAR CARDS;                06000000
BOOLEAN PROCEDURE MDOLLAR(PARAM); VALUE PARAM; BOOLEAN PARAM;           06001000
BEGIN                                                                   06002000
        COMMENT  MFF LEFT JUSTIFIES SCANNED ID TO MAKE A FILE ID;       06003000
REAL STREAM PROCEDURE MFF(I,L);                                         06004000
    VALUE L;                                                            06005000
    BEGIN                                                               06006000
        DI~LOC MFF; DS~8 LIT "0       "; DI~DI-7;                       06007000
        SI~I; SI~SI+10; DS~L CHR;                                       06008000
    END;                                                                06009000
        FORMAT                                                          06010000
            MD1(I5,2I4,I6,2I3,I4,I6,I8),                                06011000
            MD2(10(I6"="I4));                                           06012000
        INTEGER S, CT;                                                  06013000
        OWN INTEGER TYPE;                                               06014000
        BOOLEAN OK,ADTOG;                                               06015000
        ARRAY I[0:(COUNT+10).WRD];                                      06016000
        LABEL EXIT,AGAIN,EOF,ALFA,SPCHR;                                06017000
        LABEL ASEQ,BOF;                                                 06018000
        PROCEDURE SCANIT(S,CT,I);                                       06019000
        ARRAY I[0];                                                     06020000
        INTEGER S,CT;                                                   06021000
        BEGIN                                                           06022000
            TYPE~ 0;                                                    06023000
            BC ~ (S~0-SCA(BFR[BW],I,TYPE,BC,11,TYPE,CT,CHAR)).SCHR;     06024000
            BW ~ BW + S.SADD;                                           06025000
            S ~ I[1] ~ I[1]&CT[TOC2];                                   06026000
        END;                                                            06027000
        SWITCH T ~ ALFA,ALFA,SPCHR;                                     06028000
        SLIPWORDS(DOLLARDICT,STREAMDICT);                               06029000
        MOVE((COUNT+10).WRD,IDENT,I);                                   06030000
        I[1].CTR ~ COUNT;                                               06031000
        EQVTOG ~ ADTOG ~ TRUE;                                          06032000
    DO BEGIN                                                            06033000
        SCANIT(S,CT,IDENT);                                             06034000
        GO T[TYPE];                                                     06035000
        GO EXIT;                                                        06036000
EOF:    LASTUSED~ 1;                                                    06037000
        GO SPCHR;                                                       06038000
ALFA:                                                                   06039000
        IF INDICT < 0 OR W.CLASS ! VDOLLARID THEN BEGIN                 06040000
            IF OK THEN BEGIN                                            06041000
                OK~ FALSE;                                              06042000
                FILL CODE WITH MF,F~MFF(IDENT,IF CT>7 THEN              06043000
                  7 ELSE CT);GO SPCHR;                                  06044000
            END;                                                        06045000
        IF CHAR="/" AND SEQ=0 AND CCC=0 THEN                            06046000
            BEGIN                                                       06047000
                MF~ MFF(IDENT,IF CT>7 THEN 7 ELSE CT);                  06048000
                IF NOT SVT.[46:1] THEN SVT ~ OK ~ TRUE;                 06049000
            END;                                                        06050000
            GO EXIT;                                                    06051000
        END;                                                            06052000
        CASE W.SUB OF BEGIN                                             06053000
        BEGIN                                         % CARD            06054000
            IF LASTUSED ! 5 THEN CLOSE(DISK);                           06055000
            LASTUSED ~ IF LASTUSED = 4 THEN 5 ELSE 1;                   06056000
            PRINT ~ FALSE;                                              06057000
        END;                                                            06058000
        BEGIN                                         % DISK            06059000
            IF LASTUSED = 1 THEN READ(DISK[NO])[EOF];                   06060000
            LASTUSED ~ 2;                                               06061000
            PRINT ~ FALSE;                                              06062000
        END;                                                            06063000
        BEGIN                                         % NEW             06064000
            SCANIT(S,CT,IDENT);                                         06065000
            IF S ! "4DISK " THEN GO EXIT;                               06066000
            NEWFILE ~ ADTOG;                                            06067000
        END;                                                            06068000
        BEGIN                                         % SEQ             06069000
            IF NOT (NEWRESEQ~ADTOG) THEN GO SPCHR;                      06070000
            IF CHAR = "+" THEN GO ASEQ;                                 06071000
            IF CHAR ! " " THEN GO SPCHR;                                06072000
            SCANIT(S,CT,IDENT);                                         06073000
            IF TYPE = 1 THEN BEGIN                                      06074000
                NEWSEQ ~ CONSTANTCONVERT(IDENT[1],CT);                  06075000
                IF CHAR = " " THEN SCANIT(S,CT,IDENT);                  06076000
            END ELSE                                                    06077000
            IF TYPE = 2 THEN GO ALFA;                                   06078000
            IF CHAR ! "+" THEN                                          06079000
                IF TYPE = 3 THEN GO SPCHR ELSE GO ALFA;                 06080000
ASEQ:       SCANIT(S,CT,I);                                             06081000
            IF TYPE = 1 THEN                                            06082000
                NEWSEQ ~ NEWSEQ-NEWINC~CONSTANTCONVERT(IDENT[1],CT)     06083000
            ELSE                                                        06084000
            IF TYPE = 2 THEN GO ALFA;                                   06085000
        END;                                                            06086000
        BEGIN                                         % DUMP            06087000
            SCANIT(S,CT,IDENT);                                         06088000
            IF S = "4ZERO " THEN                                        06089000
                RCCNT ~ -1                                              06090000
            ELSE                                                        06091000
            IF S = "6LOCAL" THEN BEGIN                                  06092000
                FORMAT LF0("LOCAL VARIABLES:"),                         06093000
                  LF1("CONVERTTOG ="L5/"FIELDTOG ="L5/                  06094000
                    "LENGTH ="I5/"DIAL ="I5/                            06095000
                    "ISOCODE ="2I5/"BASE ="2I5),                        06096000
                  LF2("OUTPUT ACCUMULATOR:"I5" CHR"),                   06097000
                  LF3("SEQUENCE ACCUMULATOR:"I5" CHR"),                 06098000
                  LF4("INTEGER MSTRINGS:"),                             06099000
                  LF5(4(I6":"I8,X2,O)),                                 06100000
                  LF6("CONSTANT POOL:"),                                06101000
                  LF7("CODE:"),                                         06102000
                  LF8("ROW"I4";"I5" CHR:"),                             06103000
                  LF9("MPOINTERS:"),                                    06104000
                  LF10("MSTRING"I4":"I4" CHR");                         06105000
                PROCEDURE LISTIT(A,W,T); VALUE W,T;                     06106000
                INTEGER W,T;                                            06107000
                ARRAY A[0];                                             06108000
                BEGIN                                                   06109000
                    INTEGER S;                                          06110000
                    BLANK(DFN,0,14);                                    06111000
                    FOR S~0 STEP 8 UNTIL T DO BEGIN                     06112000
                        MOVE(8,A[S+W],DFN[3]);                          06113000
                        MOVEDEC(8|S,DFN[2],0,3,5,7);                    06114000
                        WRITE(PRINTER,15,DFN[*]);                       06115000
                END END;                                                06116000
                INTEGER S1,S2;                                          06117000
                WRITE(PRINTER[PAGE]);                                   06118000
                WRITE(PRINTER,LF0);                                     06119000
                WRITE(PRINTER,MON2,EP,S~MNEMON[GOP],                    06120000
                    IF BOOLEAN(S).DBA THEN "A" ELSE "I",                06121000
                    IF BOOLEAN(S).DBMN THEN MNEMON[GADR] ELSE GADR,     06122000
                    TABLET,TOGGLE,HM,OHM,IACCM,IACCM1);                 06123000
                WRITE(PRINTER,LF1,CONVERTTOG,FIELDTOG,PL,TFC,           06124000
                    FIELD.CROW,FIELD.CRAD,BASE.REF,BASE.LIM);           06125000
                IF OHM > 3 THEN BEGIN                                   06126000
                    WRITE(PRINTER,LF2,OHM-3);                           06127000
                    LISTIT(OA,0,(OHM+10).WRD);                          06128000
                END;                                                    06129000
                IF HM > 3 THEN BEGIN                                    06130000
                    WRITE(PRINTER,LF3,HM-3);                            06131000
                    LISTIT(ACCM,0,(HM+10).WRD);                         06132000
                END;                                                    06133000
                S1 ~ LOCAL[0,255].MLICT - 1;                            06134000
                WRITE(PRINTER,LF4);                                     06135000
                WRITE(PRINTER,LF5,FOR S~0 STEP 1 UNTIL S1 DO            06136000
                    [S+LINTBAS,LOCAL[0,S],LOCAL[0,S]]);                 06137000
                WRITE(PRINTER,LF6);                                     06138000
                LISTIT(LOCAL[1,*],0,LOCAL[0,255].MPOOL);                06139000
                WRITE(PRINTER,LF7);                                     06140000
                LISTIT(LOCAL[2,*],0,LOCAL[0,255].MML);                  06141000
                IF S1~LOCAL[3,0].CTR ! 0 THEN BEGIN                     06142000
                    WRITE(PRINTER,LF8,3,S1);                            06143000
                    LISTIT(LOCAL[3,*],0,(S1+7).WRD);                    06144000
                END;                                                    06145000
                IF S1~LOCAL[4,0].CTR ! 0 THEN BEGIN                     06146000
                    WRITE(PRINTER,LF8,4,S1);                            06147000
                    LISTIT(LOCAL[4,*],0,(S1+7).WRD);                    06148000
                END;                                                    06149000
                IF S1~LOCAL[0,254].MLOCT-1 ! 0 THEN BEGIN               06150000
                    WRITE(PRINTER,LF9);                                 06150500
                    WRITE(PRINTER,LF5,FOR S~0 STEP 1 UNTIL S1 DO        06151000
                        [S+LPTRBAS,LOCAL[6,S],LOCAL[6,S]]);             06152000
                END;                                                    06153000
%               WRITE(PRINTER,15,SPACEMAP[*]);                          06154000
                S1 ~ LOCAL[0,255].MLACT;                                06155000
                FOR S~28 STEP 1 UNTIL S1 DO                             06156000
                    IF INUSE(SPACEMAP[S.D32],S.M32) THEN BEGIN          06157000
                        WRITE(PRINTER,LF10,S,S2~LOCAL[S.D4,S.M4|64].    06158000
                          CTR);                                         06159000
                        LISTIT(LOCAL[S.D4,*],S.M4|64,(S2+7).WRD);       06160000
                    END;                                                06161000
                WRITE(PRINTER[PAGE]);                                   06162000
                GO EXIT;                                                06163000
            END ELSE                                                    06164000
            IF S = "6TABLE" THEN BEGIN                                  06165000
                INTEGER S1,S2;                                          06166000
                FORMAT                                                  06167000
                    DF0("CMT="L5/"CONVERTTOG="L5/"DEC="L5/              06168000
                      "ENDT="L5/"EX="L5/"MACROTOG="L5/                  06169000
                      "NOJUMP="L5/"PROCTOG="L5/"RESCANTOG="L5/),        06170000
                    DF1("MCALGOL DICTIONARY"/"SCRAMBLE LIST:"),         06171000
                    DF2("ADDR DIFF SCR LINK BLK CL SUB +INFO   ",       06172000
                        "# CHR ALPHA"),                                 06173000
$ INCLUDE 06176001, IF INTERMEDIATE;                                    06173999
                    DF3("USER DICTIONARY"/"SCRAMBLE LIST:"),            06174000
                    DF4("ADDR DIFF SCR LINK BLK SZ XXX OWNER   ",       06175000
                        "# CHR ALPHA...ENTRY"),                         06176000
                    DF5("BEGIN-END NESTING"/" LEVEL DICT  BGN #"),      06177000
                    DF6(3I6),                                           06178000
                    DF7(//I5" PENDING STATEMENT PATCHES"/,              06179000
                        I5" NEST LEVEL OF LAST STMT PATCH"/             06180000
                        I5" OUTER BLOCK NESTED BEGINS"),                06181000
                    DF8("STORED MACRO CODE,"I6" WORDS:"),               06182000
                    DF9("STORED DECLARATIONS,"I6" WORDS:"),             06183000
                    DF10("GLOBAL INTEGER, MPOINTER, ALPHA VALUES"),     06184000
                    DF11( 5(I6":"I8));                                  06185000
                IF PRINT THEN WRITE(PRINTER[PAGE]);                     06186000
                WRITE(PRINTER,DF0,CMT,CONVERTTOG,DEC,ENDT,              06187000
                    EX,MACROTOG,NOJUMP,PROCTOG,RESCANTOG);              06188000
                WRITE(PRINTER,DF1);                                     06189000
                WRITE(PRINTER,MD2,FOR S~0 STEP 1 UNTIL 124 DO           06190000
                  [S,DLIST[S]]);                                        06191000
                WRITE(PRINTER[PAGE]);                                   06192000
                WRITE(PRINTER,DF2);                                     06193000
                S ~ (CT~NEXTDICT) - LASTDICT;                           06194000
                WHILE S > 0 DO BEGIN                                    06195000
                    S~(W~DICT[S1~(CT~CT-S).IR,(S2~CT.IC)+1]).DIFF;      06196000
                    WRITE(DFN[*],MD1,CT,S,DICT[S1,S2+2]MOD 125,         06197000
                        DICT[S1,S2],W.BLKLVL,W.CLASS,W.SUB,             06198000
                        W.ADINFO,W~DICT[S1,S2+2].CTR);                  06199000
                    M(DICT[S1,S2+2],3,DFN[5],4,0,0,W);                  06200000
                    WRITE(PRINTER,15,DFN[*]);                           06201000
                END;                                                    06202000
                WRITE(PRINTER[PAGE]);                                   06203000
$ INCLUDE 06221001, IF ADVANCED;                                        06203999
                IF NXTUDICT > 0 THEN BEGIN                              06204000
                WRITE(PRINTER,DF3);                                     06205000
                WRITE(PRINTER,MD2,FOR S~125 STEP 1 UNTIL 249 DO         06206000
                  [S-125,DLIST[S]]);                                    06207000
                WRITE(PRINTER[PAGE]);                                   06208000
                WRITE(PRINTER,DF4);                                     06209000
                S ~ (CT~NXTUDICT) - LSTUDICT;                           06210000
                WHILE S > 0 DO BEGIN                                    06211000
                    S~(W~UDICT[S1~(CT~CT-S).IR,(S2~CT.IC)+1]).DIFF;     06212000
                    WRITE(DFN[*],MD1,CT,S,UDICT[S1,S2+2]MOD 125,        06213000
                        UDICT[S1,S2],W.BLKLVL,W.USIZ,W.UXXX,            06214000
                        W.UQUAL,A~UDICT[S1,S2+2].CTR);                  06215000
                    MOVE(MIN((A+10).WRD+W.USIZ,11),                     06216000
                      UDICT[S1,S2+2],DFN[5]);                           06217000
                    WRITE(PRINTER,15,DFN[*]);                           06218000
                END;                                                    06219000
                WRITE(PRINTER[PAGE]);                                   06220000
                END;                                                    06221000
                IF BLVL > 1 THEN BEGIN                                  06222000
                    WRITE(PRINTER,DF5);                                 06223000
                    FOR S~1 STEP 1 UNTIL BLVL DO                        06224000
                      WRITE(PRINTER,DF6,(W~BLIST[S]).BBLKL,             06225000
                        W.BNXTD,W.BBNO);                                06226000
                END;                                                    06227000
                WRITE(PRINTER[DBL],DF7,SSTK,STMTSTKLVL,OUTERLIMIT);     06228000
                IF PT > 0 THEN BEGIN                                    06229000
                WRITE(PRINTER[PAGE]);                                   06230000
                WRITE(PRINTER,DF8,PT);                                  06231000
                BLANK(DFN,0,14);                                        06232000
                S1 ~ 0;                                                 06233000
                READIT(DFN,1,0,MCODE);                                  06234000
%               CLEAR UPDATED CODE                                      06235000
                FOR S~0 STEP  8 UNTIL PT DO BEGIN                       06236000
                    IF S.IR ! S1 THEN                                   06237000
                        READ(MSTOR[S1~S1+1],256,MCODE[*]);              06238000
                    MOVE(8,MCODE[S.IC],DFN[3]);                         06239000
                    MOVEDEC(S,DFN[2],0,3,5,7);                          06240000
                    WRITE(PRINTER,15,DFN[*]);                           06241000
                END;                                                    06242000
                MSTORP.PDF1 ~ S1;                                       06243000
                END;                                                    06244000
                IF DECP.LIM > 0 THEN BEGIN                              06245000
                WRITE(PRINTER[PAGE]);                                   06246000
                WRITE(PRINTER,DF9,DECP.LIM);                            06247000
                BLANK(DFN,0,14);                                        06248000
                S1 ~ 0;                                                 06249000
                READIT(DFN,1,0&1[TOREF],MDECL);                         06250000
%               CLEAR UPDATED DECLARATIONS                              06251000
                FOR S~0 STEP 8 UNTIL DECP.LIM DO BEGIN                  06252000
                    IF S.IR ! S1 THEN                                   06253000
                        READ(MSTOR[150+S1~S1+1],256,MDECL[*]);          06254000
                    MOVE(8,MDECL[S.IC],DFN[3]);                         06255000
                    MOVEDEC(S,DFN[2],0,3,5,7);                          06256000
                    WRITE(PRINTER,15,DFN[*]);                           06257000
                END;                                                    06258000
                MSTORP.PDF2 ~ S1;                                       06259000
                END;                                                    06260000
                IF GLIT > GINTBAS OR GLOT > GPTRBAS OR                  06261000
                  GLAT > GALFBAS THEN BEGIN                             06262000
                WRITE(PRINTER[PAGE]);                                   06263000
                WRITE(PRINTER,DF10);                                    06264000
                WRITE(PRINTER,DF11,FOR S~GINTBAS STEP 1 UNTIL GLIT      06265000
                  DO [S,GINT[S-GINTBAS]]);                              06266000
                WRITE(PRINTER[DBL]);                                    06267000
                WRITE(PRINTER,DF11,FOR S~GPTRBAS STEP 1 UNTIL GLOT      06268000
                  DO [S,GPTR[S-GPTRBAS]]);                              06269000
                FOR S~GALFBAS STEP 1 UNTIL GLAT DO                      06270000
                  IF INUSE(SPACEMAP[(S1~S-GALFBAS).D32+8],S1.M32)       06271000
                THEN BEGIN                                              06272000
                    WRITE(PRINTER,DF11,S);                              06273000
                    CT ~ GLOBAL[S1.D4,TYPE~S1.M4|64].CTR;               06274000
                    W ~ 3;                                              06275000
                    WHILE CT>0 DO BEGIN                                 06276000
                        BLANK(DFN,0,14);                                06277000
                        M(GLOBAL[S1.D4,TYPE+W.WRD],W.CHR,DFN[2],0,0,    06278000
                          (S1~MIN(CT,80)).C6,S1);                       06279000
                        WRITE(PRINTER,15,DFN[*]);                       06280000
                        CT ~ CT - S1;                                   06281000
                        W ~ W + S1;                                     06282000
                    END;                                                06283000
                END;                                                    06284000
                WRITE(PRINTER[PAGE]);                                   06285000
                END;                                                    06286000
                GO EXIT;                                                06287000
            END DUMP TABLES;                                            06288000
            FOR S~0 STEP 1 UNTIL 124 DO                                 06289000
                WHILE DOLLARDICT { A~DLIST[S] < STREAMDICT DO           06290000
                    DLIST[S] ~ DICT[A.IR,A.IC];                         06291000
            GO DP;                                                      06292000
                                                                        06293000
        END DUMP;                                                       06294000
        BEGIN                                         % VOID            06295000
            VOIDING ~ TRUE;                                             06296000
            SCANIT(S,CT,IDENT);                                         06297000
            LINKS[2] ~ 0;                                               06298000
            M(IDENT,11,LINKS[2],8-CT,0,0,CT);                           06299000
            IF NEWFILE THEN SPACE(NEWDISK,-1)[BOF];                     06300000
            GO EXIT;                                                    06301000
BOF:        CLOSE(NEWDISK);                                             06302000
            GO EXIT;                                                    06303000
        END;                                                            06304000
        PRINT ~ ADTOG;                                % LIST            06305000
        DBLINE ~ ADTOG;                               % DOUBLE          06306000
$ INCLUDE 06310001, IF DEBUGGING;                                       06306999
        MONITER ~ ADTOG;                              % MONITOR         06307000
        TRACE ~ ADTOG;                                % TRACE           06308000
        DBUGN ~ ADTOG;                                % DEBUGN          06309000
        ACMLST ~ ADTOG;                               % ACCUM           06310000
$ OMIT 06310101, IF DEBUGGING;                                          06310099
        ;;;;                                                            06310100
        UNPACK ~ ADTOG;                               % UNPACK          06311000
        IF INTERLIST ~ ADTOG THEN UNPACK ~ TRUE;      % INTER           06312000
$ INCLUDE 06313001, IF DEBUGGING;                                       06312999
        SRT ~ ADTOG;                                  % PRT             06313000
        BLOCK ~ ADTOG;                                % BLOCK           06314000
        WARNTOG ~ ADTOG;                              % WARN            06315000
        IF PRINT THEN WRITE(PRINTER[PAGE]);           % PAGE            06316000
        IF SEQ=0 OR NOT ADTOG THEN INDEXING ~ ADTOG;  % INDEX           06317000
$ INCLUDE 06318001, IF DEBUGGING;                                       06317999
        PSEUDOTOG ~ ADTOG;                            % PSEUDO          06318000
$ OMIT 06318101, IF DEBUGGING;                                          06318099
        ;                                                               06318100
        END ALFA CASES;                                                 06319000
SPCHR:  IF CHAR = "-" THEN ADTOG ~ FALSE ELSE                           06320000
        IF CHAR = "+" THEN ADTOG ~ TRUE;                                06321000
    END UNTIL CHAR = "%" OR CHAR = ";";                                 06322000
EXIT: MDOLLAR ~ CHAR = ";" AND PARAM;                                   06323000
    FOR S~0 STEP 1 UNTIL 124 DO                                         06324000
        WHILE DOLLARDICT { A~DLIST[S] < STREAMDICT DO                   06325000
            DLIST[S] ~ DICT[A.IR,A.IC];                                 06326000
        MOVE(((COUNT~I[1].CTR)+10).WRD,I,IDENT);                        06327000
        EQVTOG ~ FALSE;                                                 06328000
END MDOLLAR;                                                            06329000
        COMMENT  ASCAN DRIVES THE STANDARD SCANNER SCA AND CHECKS       06500000
    FOR END OF INPUT ("%") FLAGS TO FLUSH TO NEXT INPUT IMAGE.          06501000
    ESTABLISHES TYPE OF ITEM SCANNED;                                   06502000
PROCEDURE ASCAN;                                                        06503000
    BEGIN                                                               06504000
        REAL T, CT;                                                     06505000
        IF TYPE=0 THEN                                                  06506000
            COUNT~ 0;                                                   06507000
        DO                                                              06508000
            BEGIN                                                       06509000
                BC ~ (T~0-SCA(BFR[BW],IDENT[(T~COUNT+11).WRD],          06510000
                  TYPE,BC,T.CHR,TYPE,CT,CHAR)).SCHR;                    06511000
                IF(BW~(BW+T.SADD))|8+BC>BWMAX THEN                      06512000
                    IF LASTUSED=0 THEN                                  06513000
                        BEGIN                                           06514000
                            IF(COUNT~ COUNT+CT)=0 THEN                  06515000
                                TYPE~ 4;                                06516000
                            CT~ 0                                       06517000
                        END                                             06518000
                    ELSE                                                06519000
                        BEGIN                                           06520000
                            READACARD;                                  06521000
                            COUNT~ COUNT+CT-63;                         06522000
                            CT~ 63;                                     06523000
                        END;                                            06524000
                IF COUNT ~ COUNT+CT > IDMAX THEN                        06525000
                    COUNT ~ IDMAX;                                      06526000
            END                                                         06527000
        UNTIL CT<63;                                                    06528000
        IF TYPE!3 THEN                                                  06529000
            IDENT[1].CTR ~ COUNT;                                       06530000
$ INCLUDE 06530520, IF COUNTING;                                        06530500
    ASCT ~ *+1;                                                         06530510
    END ASCAN;                                                          06531000
        COMMENT  BSCAN DEBLANKS AND ONLY RETURNS A SPECIAL CHR IF       06600000
    PRESENT. USES PATCH OVER LITC 0 TRICK;                              06601000
PROCEDURE BSCAN;                                                        06602000
    BEGIN                                                               06603000
        INTEGER STREAM PROCEDURE SCB(BW, BC, T, D);                     06604000
        VALUE BC, T;                                                    06605000
        BEGIN                                                           06606000
            LABEL L;                                                    06607000
            SI~BW; SCB~SI; SI~SI+BC;                                    06608000
L:          IF SC=" " THEN BEGIN SI~SI+1; GO L END; DI~D;               06609000
            DS~7 LIT "0"; IF SC=ALPHA THEN DS~LIT " " ELSE DS~CHR;      06610000
            T~SI; SI~LOC T; DI~LOC SCB; DI~DI-24; % TRICK......         06611000
            DS~ WDS;                                                    06612000
        END SCB;                                                        06613000
        REAL T;                                                         06614000
        LABEL AGAIN;                                                    06615000
AGAIN:  BC~(T~0-SCB(BFR[BW],BC,0,CHAR)).SCHR;                           06616000
        IF(BW~(BW+T.SADD))|8+BC>BWMAX THEN                              06617000
            IF LASTUSED=0 THEN                                          06618000
                TYPE~ 4                                                 06619000
            ELSE                                                        06620000
                BEGIN                                                   06621000
                    READACARD;                                          06622000
                    GO TO AGAIN;                                        06623000
                END;                                                    06624000
$ INCLUDE 06624520, IF COUNTING;                                        06624500
    BSCT ~ *+1;                                                         06624510
    END BSCAN;                                                          06625000
        COMMENT  SSCAN IS USED TO PROCESS ARBITRARY LENGTH QUOTED       06700000
    STRINGS. IF "%" IS FOUND, A CHECK IS MADE TO SEE IF IT WAS A        06701000
    SENTINEL OR NOT. STRINGS SCANNED WHILE COMPILING MACROS ARE         06702000
    STRIPPED OF QUOTES FOR USE AS CONSTANTS. STRINGS >SMAX LONG         06703000
    ARE TOO ARBITRARY AND ARE TREATED AS SYNTAX ERRORS;                 06704000
PROCEDURE SSCAN;                                                        06705000
    BEGIN                                                               06706000
        LABEL AGAIN, EXIT, OK;                                          06707000
        REAL T, CT;                                                     06708000
        INTEGER STREAM PROCEDURE SCS(BW,AW,BC,AC,CT,D);                 06709000
        VALUE BC, AC;                                                   06710000
        BEGIN                                                           06711000
            SI~BW; SCS~SI; SI~SI+BC; DI~AW; DI~DI+AC;                   06712000
            63(IF SC=""" THEN JUMP OUT;IF SC!"%" THEN BEGIN DS~ CHR;    06713000
              TALLY~ TALLY+1;END ELSE JUMP OUT);                        06714000
            DS~4 LIT " "; DI~D; DS~7 LIT "0"; DS~CHR;                   06715000
            AC~SI; BC~TALLY; SI~LOC BC; DI~CT; DS~WDS;                  06716000
            DI~LOC SCS; DI~DI-24; DS~WDS;                               06717000
        END SCS;                                                        06718000
        GO AGAIN;                                                       06719000
OK:     INSERT((COUNT+3).CHR,IDENT[(COUNT+11).WRD],CHAR,1);             06720000
        COUNT~ COUNT+1;                                                 06721000
AGAIN:  BC ~ (T~0-SCS(BFR[BW],IDENT[(T~COUNT+11).WRD],BC,               06722000
          T.CHR,CT,CHAR)).SCHR;                                         06723000
        BW ~ BW+T.SADD;                                                 06724000
        IF COUNT~COUNT+CT > MSMAX THEN                                  06725000
            BEGIN                                                       06726000
                COUNT~ 0;                                               06727000
                ERROR(0);                                               06728000
            END;                                                        06729000
        IF BW|8+BC>BWMAX THEN                                           06730000
            BEGIN                                                       06731000
                IF LASTUSED=0 THEN                                      06732000
                    BEGIN                                               06733000
                        TYPE~ 4;                                        06734000
                        GO EXIT                                         06735000
                    END;                                                06736000
                READACARD;                                              06737000
                GO TO AGAIN;                                            06738000
            END;                                                        06739000
        IF CHAR="%" THEN                                                06740000
            GO TO OK;                                                   06741000
        IF COUNT+REAL(MACROTOG AND<1>)=1 THEN                           06742000
            GO TO OK;                                                   06743000
        IF CT=63 AND CHAR!""" THEN                                      06744000
            GO OK;                                                      06745000
EXIT:                                                                   06746000
$ INCLUDE 06746520, IF COUNTING;                                        06746500
    SSCT ~ *+1;                                                         06746510
    END SSCAN;                                                          06747000
        COMMENT  ESCAN IS USED TO SCAN OFF "EXPRESSIONS" OF THE         06800000
    TYPE FOUND IN ARRAY BOUND PAIRS AND FORMAT, LIST, & PROCEDURE       06801000
    DECLARATIONS BETWEEN "[ ]" & "( )";                                 06802000
PROCEDURE ESCAN;                                                        06803000
    BEGIN                                                               06804000
        INTEGER PARNPAIR, BRACPAIR;                                     06805000
        LABEL OK,ERR,CONTINUE,COLON,COMMA,LPN,LBC,RPN,RBC,EXIT;         06806000
        SWITCH SWHICH ~ OK,ERR,COLON,OK,OK,OK,ERR,ERR,ERR,ERR,          06807000
          ERR,ERR,ERR,ERR,ERR,OK,LBC,OK,LPN,OK,OK,OK,ERR,               06808000
          ERR,ERR,ERR,ERR,ERR,ERR,ERR,ERR,ERR,OK,OK,RPN,                06809000
          EXIT,OK,OK,OK,ERR,ERR,ERR,ERR,ERR,ERR,ERR,ERR,                06810000
          COMMA,ERR,OK,OK,RBC;                                          06811000
%                                                                       06812000
        GO TO CONTINUE;                                                 06813000
OK:     PACKET;                                                         06814000
CONTINUE: IF STEPI=63 THEN                                              06815000
            GO ERR;                                                     06816000
        IF TYPE!0 THEN                                                  06817000
            BEGIN                                                       06818000
                PACKET;                                                 06819000
                GO TO CONTINUE                                          06820000
            END;                                                        06821000
        GO TO SWHICH[ELB-10];                                           06822000
ERR:    ERROR(1);                                                       06823000
        ELB~ ";";                                                       06824000
        GO TO EXIT;                                                     06825000
LPN:    IF BRACPAIR+PARNPAIR~PARNPAIR+1 = 1 THEN                        06826000
            IF NOT FMTTOG THEN EX ~ TRUE;                               06826050
        GO TO OK;                                                       06827000
LBC:    IF PARNPAIR+BRACPAIR~BRACPAIR+1 = 1 THEN                        06828000
            IF NOT FMTTOG THEN EX ~ TRUE;                               06828050
        GO TO OK;                                                       06829000
RPN:    IF(PARNPAIR~ PARNPAIR-1)<0 THEN                                 06830000
            GO TO EXIT;                                                 06831000
        IF PARNPAIR + BRACPAIR = 0 THEN EX ~ FALSE;                     06831050
        GO TO OK;                                                       06832000
RBC:    IF(BRACPAIR~ BRACPAIR-1)<0 THEN                                 06833000
            GO TO EXIT;                                                 06834000
        IF BRACPAIR + PARNPAIR = 0 THEN EX ~ FALSE;                     06834050
        GO TO OK;                                                       06835000
COLON:  IF BRACPAIR=0 THEN                                              06836000
            GO TO EXIT;                                                 06837000
        GO TO OK;                                                       06838000
COMMA:  IF BRACPAIR+PARNPAIR=0 THEN                                     06839000
            GO TO EXIT;                                                 06840000
        GO TO OK;                                                       06841000
EXIT:   EX ~ FALSE;                                                     06842000
$ INCLUDE 06842520, IF COUNTING;                                        06842500
    ESCT ~ *+1;                                                         06842510
    END ESCAN;                                                          06843000
        COMMENT  SLIPWORDS IS USED TO SLIP ONE CLASS OF                 06850000
    RESERVED WORDS INTO DICT. THESE WILL BE REMOVED BY WRAPBLOCK        06851000
    MDOLLAR OR MACRO;                                                   06852000
PROCEDURE SLIPWORDS(B,E);                                               06853000
    VALUE B,E;                                                          06854000
    INTEGER B,E;                                                        06855000
    BEGIN                                                               06856000
        INTEGER I;                                                      06857000
$ INCLUDE 06859001, IF DEBUGGING;                                       06857999
        IF MONITER AND PRINT THEN                                       06858000
            WRITE(PRINTER,MON3,B,E);                                    06859000
        E ~ -DICT[E.IR,E.IC+1].DIFF + E;                                06860000
        DO BEGIN                                                        06861000
         DICT[E.IR,E.IC] ~ DLIST[I~DICT[E.IR,E.IC+2]MOD 125];           06862000
         DLIST[I] ~ E;                                                  06863000
        END UNTIL E~-(I~DICT[E.IR,E.IC+1].DIFF)+E<B OR I=0;             06864000
    END SLIPWORDS;                                                      06865000
        COMMENT  STARTBLOCK STACKS BLOCKLEVEL CUTBACK INFO &            06866000
    MAINTAINS BEGINLEVEL CONTROL. A CALL OF TRUE IS FOR A "BEGIN".      06867000
    A CALL OF FALSE IS TO PROVIDE A PSEUDOLEVEL FOR HIDING FORMAL       06868000
    PARAMETERS AND MACRO WORKINGS;                                      06869000
PROCEDURE STARTBLOCK(T);                                                06870000
    VALUE T;                                                            06871000
    BOOLEAN T;                                                          06872000
    BEGIN                                                               06873000
        FORMAT SP(X95"BEGIN"I4"("I2")");                                06874000
$ INCLUDE 06876001, IF DEBUGGING;                                       06874999
        IF MONITER AND PRINT THEN                                       06875000
            WRITE(PRINTER,MON4,"SB",NEXTDICT,T);                        06876000
        BLIST[BLVL]~ BNO&NEXTDICT[TOBNXTD]&BLOCKLEVEL[TOBBLKL];         06877000
        BLIST[BLVL+50] ~ NXTUDICT;                                      06878000
        IF T THEN                                                       06879000
            BEGIN                                                       06880000
                IF PROCTOG AND STREAMTOG THEN                           06881000
                    SLIPWORDS(STREAMDICT,MACRODICT);                    06882000
                SPCT~ PROCTOG~ FALSE;                                   06883000
                IDENT[1]~ "5?0001";                                     06884000
                IF NEST>0 THEN                                          06885000
                    PROCT[NEST] ~ *+1;                                  06886000
                SHIFT(0, VOTHERS, BEGINV);                              06887000
                BNO~ BNOMAX~ BNOMAX+1;                                  06888000
                IF BLOCK AND PRINT THEN                                 06889000
                    WRITE(PRINTER,SP,BNO,BLVL-NEST);                    06889100
            END                                                         06890000
        ELSE                                                            06891000
            PROCT[NEST~ NEST+1]~ 0;                                     06892000
        BLVL~ BLVL+1                                                    06893000
    END STARTBLOCK;                                                     06894000
        COMMENT  WRAPBLOCK CUTS BACK THE DICT TO THE PREVIOUS LEVEL.    06895000
    STORAGE SPACE FOR DECLARATIONS AND MACRO CODE ARE RETURNED.         06896000
    UNPLACED MACRO LABELS ARE FLAGGED;                                  06897000
PROCEDURE WRAPBLOCK(T);                                                 06898000
    VALUE T;                                                            06899000
    BOOLEAN T;                                                          06900000
    BEGIN                                                               06901000
        INTEGER NW, I;                                                  06902000
        REAL A,WD;                                                      06903000
        FORMAT SP(X96"END "I4"("I2")");                                 06904000
        NW~ NEXTDICT;                                                   06905000
$ INCLUDE 06907001, IF DEBUGGING;                                       06905999
        IF MONITER AND PRINT THEN                                       06906000
            WRITE(PRINTER,MON4,"WB",NW,T);                              06907000
        IF BLVL{1 AND T THEN                                            06908000
            ERROR(49);                                                  06909000
        NEXTDICT ~ BLIST[BLVL~BLVL-REAL(BLVL>0)].BNXTD;                 06910000
        BLOCKLEVEL ~ BLIST[BLVL].BBLKL;                                 06911000
        IF NW>NEXTDICT AND(NOT MACROTOG OR MCBL>BLVL) THEN              06912000
            FOR I~ 0 STEP 1 UNTIL 124 DO                                06913000
                WHILE NW~ DLIST[I]}NEXTDICT DO                          06914000
                BEGIN                                                   06915000
                    IF MACROTOG THEN                                    06916000
                        IF(A~DICT[NW.IR,NW.IC+1]).CLASS=                06917000
                          VALGOLIDENT AND A.SUB=LABELV THEN             06918000
                            IF BOOLEAN((A~GET(A.ADINFO)).PNOTT)THEN     06919000
                            BEGIN % UNPLACED LABEL                      06920000
                                ERROR(58&NW[TOREF]);                    06921000
                                ERRCT ~ ERRCT + 1;                      06922000
                            END;                                        06923000
                    IF (WD~DICT[NW.IR,NW.IC+1]).CLASS=VDECLRN THEN      06924000
                        IF WD.ADINFO < DECP.ADINFO THEN                 06925000
                            DECP.ADINFO ~ WD;                           06926000
                    IF WD.CLASS=VMACRONAME AND WD.SUB!EQVV THEN         06927000
                        IF (WD~DICT[(WD~ WD.ADINFO+1).IR,               06928000
                          WD.IC])<PT THEN                               06929000
                            PT~ WD;                                     06930000
                    DLIST[I] ~ DICT[NW.IR,NW.IC];                       06931000
                END                                                     06932000
        ELSE                                                            06933000
            NEXTDICT~ NW;                                               06934000
        IF NW ~ DICT[(I~NEXTDICT+1).IR,I.IC].DIFF!0 THEN                06935000
            LASTDICT~ NEXTDICT -NW;                                     06936000
$ INCLUDE 06947001, IF ADVANCED;                                        06936999
        NW ~ NXTUDICT;                                                  06937000
        NXTUDICT ~ BLIST[BLVL+50];                                      06938000
        IF NW > NXTUDICT THEN                                           06939000
            FOR I~125 STEP 1 UNTIL 249 DO                               06940000
                WHILE NW~DLIST[I] } NXTUDICT DO                         06941000
                    DLIST[I] ~ UDICT[NW.IR,NW.IC]                       06942000
        ELSE                                                            06943000
            NXTUDICT ~ NW;                                              06944000
        IF NW ~ UDICT[(I~NXTUDICT+1).IR,I.IC].DIFF ! 0 THEN             06945000
            LSTUDICT ~ NXTUDICT - NW;                                   06946000
        UDICT[I.IR,I.IC] ~ 0;                                           06947000
        DICT[I.IR,I.IC]~ 0;                                             06948000
        IF T THEN                                                       06949000
            BEGIN                                                       06950000
                COUNT~ 5;                                               06951000
                IDENT[1]~ "5?0000";                                     06952000
                SHIFT(0, VOTHERS, ENDV);                                06953000
                ENDT~ TRUE;                                             06954000
                IF BLOCK AND PRINT THEN                                 06955000
                    WRITE(PRINTER,SP,BNO,BLVL-NEST);                    06955100
                IF NEST>0 THEN                                          06956000
                    PROCT[NEST] ~ *-1                                   06957000
            END;                                                        06958000
        BNO ~ BLIST[BLVL].BBNO;                                         06959000
    END WRAPBLOCK;                                                      06960000
        COMMENT  TABLE IS THE HEART OF SCANNER ACTIVITY. THE RESULT     07000000
    IS TO RETURN ELBAT WORDS IN ELB, CLASS FIELD IN TABLE & TYPE.       07001000
    IF ITEM REQUESTED BY "STEPI" HAS ALREADY BEEN SCANNED, ONLY         07002000
    ELBAT WORD RETURNED. OTHERWISE, SCANNERS ARE ACTIVATED &            07003000
    RECOGNITION ATTEMPTS MADE. MACRO IDS CAUSE ACTIVATION OF            07004000
    INTERPRETER (MACRON). DECLARATORS CAUSE IMMEDIATE PROCESSING        07005000
    BY DECLARATION. DECOMPOSITION EOF CONDITION FLAGGED BY SHIFTING     07006000
    IMPOSSIBLE CLASS OF 63 INTO ELB;                                    07007000
INTEGER PROCEDURE TABLE(P);                                             07008000
    VALUE P;                                                            07009000
    INTEGER P;                                                          07010000
    BEGIN                                                               07011000
        INTEGER SPC, SGN, TTEM, A;                                      07012000
    STREAM PROCEDURE CONVERT(IDN,C); VALUE C; BEGIN                     07013000
        DI~IDN; DI~DI+3; SI~LOC C; SI~SI+7; SKIP 5 SB;                  07014000
        IF SB THEN DS~3 RESET; SI~IDN; SI~SI+4;                         07015000
        C(SKIP 3 SB;3(IF SB THEN DS~SET ELSE DS~RESET;SKIP SB));        07016000
        DS~4 LIT " ";                                                   07017000
    END CONVERT;                                                        07018000
        LABEL E51;                                                      07019000
        LABEL IPART,FPART,EPART,NONE,DONE,IDEN,SPCHR,PSEUDO,            07020000
          EXIT,FOUND,ONE,LOTHERS,LDEC,LOOP;                             07021000
        SWITCH WITCH~ IPART, IDEN, SPCHR, PSEUDO;                       07022000
$ INCLUDE 07022520, IF COUNTING;                                        07022500
        STEPCT ~ *+1;                                                   07022510
        WHILE P}NXTELB DO                                               07023000
            BEGIN                                                       07024000
                TYPE~ 0;                                                07025000
                ASCAN;                                                  07026000
                GO TO WITCH[TYPE];                                      07027000
IPART:          IF(W~ IDENT[1]).C23="5?" THEN                           07028000
                    BEGIN                                               07029000
                        IF W~ IN4(W)=0 THEN                             07030000
                            SHIFT(0,VOTHERS,ENDV)                       07031000
                        ELSE                                            07032000
                            IF W=1 THEN                                 07033000
                                SHIFT(0,VOTHERS,BEGINV)                 07034000
                            ELSE                                        07035000
                                IF INDICT>0 THEN                        07036000
                                  SHIFT(W,0,0)                          07037000
                                ELSE BEGIN ERROR(73);                   07038000
                                    SHIFT(0,VNOTDEC,0);END;             07039000
                        GO SPCHR                                        07040000
                    END;                                                07041000
                IF W.C23="4?" THEN                                      07042000
                    BEGIN                                               07043000
                    SHIFT(0,VPATCHID,0);                                07044000
                    GO SPCHR;                                           07045000
                    END;                                                07046000
                IF CHAR="." THEN                                        07047000
                    GO TO FPART;                                        07048000
                IF CHAR="@" THEN                                        07049000
                    GO TO EPART;                                        07050000
                SPC ~ 1023; % INTEGER FLAG                              07051000
                GO TO DONE;                                             07052000
FPART:          SPC~ CHAR;                                              07053000
                TYPE~ 5;                                                07054000
                A~ COUNT;                                               07055000
                ASCAN;                                                  07056000
                IF A=COUNT THEN                                         07057000
                    BEGIN                                               07058000
                        IF COUNT=0 THEN                                 07059000
                            GO TO NONE;                                 07060000
ONE:                    SHIFT(0,VNUMBER,REALV);                         07061000
                        GO TO NONE                                      07062000
                    END;                                                07063000
                IF CHAR="@" THEN                                        07064000
                    GO TO EPART;                                        07065000
                GO TO DONE;                                             07066000
EPART:          SPC~ CHAR;                                              07067000
                TYPE~ 5;                                                07068000
                A~ COUNT;                                               07069000
                ASCAN;                                                  07070000
                IF A!COUNT THEN                                         07071000
                    GO TO DONE;                                         07072000
                IF CHAR!"+" THEN                                        07073000
                    IF CHAR!"-" THEN                                    07074000
                        BEGIN                                           07075000
                            IF COUNT=0 THEN                             07076000
                                GO TO NONE;                             07077000
                            GO TO ONE                                   07078000
                        END;                                            07079000
                SGN~ SPC;                                               07080000
                SPC~ CHAR;                                              07081000
                TYPE~ 5;                                                07082000
                COUNT~ A~ 1+COUNT;                                      07083000
                ASCAN;                                                  07084000
                IF A!COUNT THEN                                         07085000
                    BEGIN                                               07086000
                        INSERT((A+2).CHR,IDENT[(A+10).WRD],SGN,1);      07087000
                        GO TO DONE;                                     07088000
                    END;                                                07089000
                SHIFT(0,VNUMBER,REALV);                                 07090000
                SHIFT(SGN, 0, 0);                                       07091000
NONE:           SHIFT(SPC, 0, 0);                                       07092000
                GO TO SPCHR;                                            07093000
DONE:           SHIFT(0,VNUMBER,IF SPC=1023 THEN INTEGERV ELSE          07094000
                    REALV);                                             07095000
                GO TO SPCHR;                                            07096000
IDEN:         IF A~INDICT < 0 THEN BEGIN                                07097000
    CASE ABS(A)-1 OF BEGIN                                              07098000
                                                                        07099000
        SHIFT(0,VNOTDEC,1);                           % NOT FOUND       07100000
        SHIFT(0,VNOTDEC,2);                           % REPLACEMENT     07101000
        GO EXIT;                                      % PSEUDOREADER    07102000
        ;                                             % SP CHR          07103000
        BEGIN                                         % PARA DEFINE     07104000
        INTEGER HLD,I,CL,FLD,PRM,DCT,D,LAD;                             07105000
        ARRAY HOLD[0:255];                                              07106000
        ARRAY INST[0:63],CON[0:127];                                    07107000
        REAL A;                                                         07108000
        BOOLEAN BOOL;                                                   07109000
        LABEL AGAIN;                                                    07110000
        IF CHAR = " " THEN BSCAN;                                       07111000
        IF NOT(CHAR="(" OR CHAR="[") THEN BEGIN                         07112000
            ERROR(87);                                                  07113000
            GO EXIT;                                                    07114000
        END;                                                            07115000
        SAVEIT;                                                         07116000
        IF BOOL ~ NWCT ! 3 THEN BEGIN                                   07117000
            DECL[0].CTR ~ NWCT - 3;                                     07118000
            STACKIT(CL~(NWCT+10).WRD,DECL[*]);                          07119000
            NWCT ~ 3;                                                   07120000
        END;                                                            07121000
        LAD ~ W.ADINFO;                                                 07122000
        HLD ~ 0 & 2[TOREF];                                             07123000
        D ~ W.SUB - 4;                                                  07124000
        DEC ~ TRUE;                                                     07125000
        FOR I~1 STEP 1 UNTIL D DO BEGIN                                 07126000
            DECL[(NWCT~3).WRD] ~ "     ";                               07127000
            WHILE STEPI ! VSPECIAL DO                                   07128000
AGAIN:          PACKET;                                                 07129000
            IF ELB = "(" OR ELB = "[" THEN                              07130000
                A ~ A + 1                                               07131000
            ELSE                                                        07132000
            IF ELB = ")" OR ELB = "]" THEN                              07133000
                A ~ A - 1;                                              07134000
            IF A > 0 THEN GO AGAIN;                                     07135000
            IF A = 0 THEN BEGIN                                         07136000
                IF I = D THEN GO AGAIN;                                 07137000
                IF ELB ! "," THEN GO AGAIN;                             07138000
            END ELSE BEGIN                                              07139000
                IF I ! D THEN                                           07140000
                    ERROR(88);                                          07141000
                IF ELB ! ")" AND ELB ! "]" THEN                         07142000
                    ERROR(89);                                          07143000
            END;                                                        07144000
            PUT(LAD+I,HLD);                                             07145000
            DECL[0].CTR ~ NWCT-3;                                       07146000
            WRITIT(DECL,(NWCT+10).WRD,HLD,HOLD);                        07147000
        END LOADING PARAMS;                                             07148000
        IF BOOL THEN BEGIN                                              07149000
            UNSTACKIT(CL,DECL[*]);                                      07150000
            NWCT ~ DECL[0].CTR + 3;                                     07151000
        END;                                                            07152000
        RECALLIT;                                                       07153000
        D ~ GET(LAD);                                                   07154000
        I ~ D.REF & 1[TOREF];                                           07155000
        READIT(INST,(D.LIM-1).D2+1,I,MDECL);                            07156000
        READIT(CON,D.OFFSET,I,MDECL);                                   07157000
        DEC ~ FALSE;                                                    07158000
        I ~ 0;                                                          07159000
        DCT ~ 3;                                                        07160000
        DO BEGIN                                                        07161000
            IF BOOLEAN(I~I+1) THEN BEGIN                                07162000
                CL ~ (A~INST[(I-1).D2]).PDC1;                           07163000
                DFN[255] ~ FLD ~ A.PDF1;                                07164000
                PRM ~ A.PDP1;                                           07165000
            END ELSE BEGIN                                              07166000
                CL ~ A.PDC2;                                            07167000
                DFN[255] ~ FLD ~ A.PDF2;                                07168000
                PRM ~ A.PDP2;                                           07169000
            END;                                                        07170000
            CASE CL OF BEGIN                                            07171000
                ; % NOT USED                                            07172000
                DCT ~ MOVER(1,DFN,2046,DFN,DCT,0) + 1; % 1 LIT          07173000
                DCT ~ MOVER(2,DFN,2046,DFN,DCT,0) + 1; % 2 LIT          07174000
                DCT ~ MOVER(CON[FLD].CTR,CON,8|FLD+3,                   07175000
                    DFN,DCT,0)+1; % SEQUENCE                            07176000
            END CASE STMT;                                              07177000
            IF PRM ! 0 THEN                                             07178000
                DCT ~ MOVER(HOLD[FLD~GET(LAD+PRM)].CTR,                 07179000
                    HOLD,8|FLD+3,DFN,DCT,0)+1;                          07180000
        END UNTIL CL=0=PRM;                                             07181000
        BOOL ~ LASTUSED > 0 OR RESCANTOG;                               07182000
        PSEUDOREAD(DFN,DCT,0);                                          07183000
        RESCANTOG ~ BOOL;                                               07184000
        NXTELB ~ NXTELB - 1;                                            07185000
        GO EXIT;                                                        07186000
        END PARAMETRIC DEFINE;                                          07187000
    END OUT OF DICT CASES;                                              07188000
                        GO SPCHR;                                       07189000
                    END;                                                07190000
                IF ENDT THEN ENDT~NOT(SGN~IDENT[1]="3END  " OR          07191000
                    SGN="4ELSE " OR SGN="5UNTIL");                      07192000
                IF SGN~ W.CLASS=VOTHERS THEN                            07193000
                    GO LOTHERS                                          07194000
                ELSE                                                    07195000
                    IF CMT OR ENDT THEN                                 07196000
                        GO FOUND                                        07197000
                    ELSE                                                07198000
                        IF SGN=VSTREAMRES AND W.SUB=1 THEN              07199000
                            BEGIN                                       07200000
                                SGN~ LABELV;                            07201000
                                GO LDEC                                 07202000
                            END                                         07203000
                        ELSE                                            07204000
                            IF SGN!VMACRONAME OR EQVTOG THEN            07205000
                                GO FOUND;                               07206000
                IF CHAR!" " THEN                                        07207000
                    BC ~ -(BW~(TTEM~ BW|8+BC-1).WRD)|8+TTEM;            07208000
                MACRON(A, W.ADINFO);                                    07209000
                GO TO EXIT;                                             07210000
LOTHERS:        IF SGN~ W.SUB!ENDV AND ENDT OR NOT EX EQV DEC OR CMT    07211000
                  THEN                                                  07212000
                    GO FOUND;                                           07213000
                IF SGN>MACROV THEN                                      07214000
                    IF SGN{MPOINTERV OR SGN=VALUEV THEN GO LDEC ELSE    07215000
                    IF SGN=BEGINV THEN BEGIN                            07216000
                        STARTBLOCK(TRUE);                               07217000
                        GO SPCHR;                                       07218000
                    END ELSE                                            07219000
                    IF SGN=ENDV THEN BEGIN                              07220000
                        WRAPBLOCK(TRUE);                                07221000
                        GO SPCHR;                                       07222000
                    END ELSE                                            07223000
                    IF SGN=COMMENTV THEN BEGIN                          07224000
                        CMT ~ NOT ENDT;                                 07225000
                        GO EXIT;                                        07226000
                    END ELSE                                            07227000
                    GO FOUND;                                           07228000
LDEC:           SHIFT(0, VNOTDEC, 0);                                   07229000
                IF ENDT THEN                                            07230000
                    GO SPCHR;                                           07231000
                SPC~ NXTELB;                                            07232000
LOOP:           IF DEC THEN                                             07233000
                    GO SPCHR;                                           07234000
                IF CHAR=" " THEN                                        07235000
                    BSCAN;                                              07236000
                IF TYPE=4 AND NOT NOREAD THEN                           07237000
                    BEGIN                                               07238000
                        READACARD;                                      07239000
                        TYPE~ CHAR~ " ";                                07240000
                        IF SPC=NXTELB THEN                              07241000
                            GO TO LOOP                                  07242000
                    END;                                                07243000
                IF CHAR!" " THEN                                        07244000
     IF SGN!INTEGERV OR NOT MACROTOG OR (SGN!SAVEV OR CHAR!")") THEN    07245000
                    IF CHAR!"(" THEN                                    07246000
                        GO E51                                          07247000
                    ELSE                                                07248000
                        IF SGN=BOOLEANV OR SGN=REALV THEN               07249000
                            GO SPCHR                                    07250000
                        ELSE                                            07251000
E51:                        BEGIN                                       07252000
                                ERROR(51);                              07253000
                                GO SPCHR                                07254000
                            END                                         07255000
                        ELSE                                            07256000
                            BC ~ -(BW~(TTEM~BW|8+BC-1).WRD)             07257000
                              |8+TTEM;                                  07258000
                IF SPC!NXTELB THEN                                      07259000
                    IF ELBAT[SPC-BELB].CLASS=0 THEN                     07260000
                        GO TO EXIT;                                     07261000
                ELBAT[SPC-1-BELB] ~ (*)&W[CLUBCLUB];                    07262000
                DECLARATION;                                            07263000
                GO TO EXIT;                                             07264000
FOUND:          SHIFT(W, 0, 0);                                         07265000
SPCHR:                                                                  07266000
                TTEM ~ COUNT;                                           07267000
                IF CHAR=":" THEN BEGIN                                  07268000
                    BSCAN;                                              07269000
                    IF CHAR="=" THEN CHAR~"~" ELSE                      07270000
                        SHIFT(":",0,0);                                 07271000
                    END;                                                07272000
                IF CHAR="%" THEN                                        07273000
                    GO PSEUDO;                                          07274000
                IF CHAR=" " THEN                                        07275000
                    GO TO EXIT;                                         07276000
                COUNT~ 0;                                               07277000
                IF CHAR="." AND NOT ENDT THEN                           07278000
                    GO TO FPART;                                        07279000
                IF CHAR="@" THEN                                        07280000
                    GO TO EPART;                                        07281000
                IF CHAR=""" THEN                                        07282000
                    BEGIN                                               07283000
                        IF NOT MACROTOG THEN                            07284000
                            BEGIN                                       07285000
                                INSERT(3, IDENT[1], CHAR, 1);           07286000
                                COUNT~ 1;                               07287000
                            END;                                        07288000
                        SSCAN;                                          07289000
                        IF NOT MACROTOG THEN                            07290000
                            BEGIN                                       07291000
                                INSERT((COUNT+3).CHR,IDENT[(COUNT+      07292000
                                  11).WRD],CHAR,1);                     07293000
                                COUNT~ COUNT+1;                         07294000
                            END;                                        07295000
                        IDENT[1].CTR ~ COUNT;                           07296000
                        SHIFT(0, VSTRING, 0);                           07297000
                        GO TO EXIT;                                     07298000
                    END;                                                07299000
                IF CHAR="<" THEN                                        07300000
                    BEGIN                                               07301000
                        SPC~ CHAR;                                      07302000
                        TYPE~ 5;                                        07303000
                        ASCAN;                                          07304000
                        IF TYPE=1 AND CHAR=" " THEN                     07305000
                            BSCAN;                                      07306000
                        IF CHAR!">" THEN                                07307000
                            BEGIN                                       07308000
                               FMTTOG~FMTTOG OR (SPC~ELBAT[NXTELB       07309000
                                 -BELB-1]).CLASS=0 AND (SPC="(" OR      07310000
                                 SPC=",");                              07311000
                                SHIFT("<", 0, 0);                       07312000
                                IF COUNT=0 THEN                         07313000
                                    GO SPCHR;                           07314000
                                SHAFT(IDENT[1], COUNT);                 07315000
                                IDENT[1].CTR ~ COUNT~COUNT-1;           07316000
                                GO IPART;                               07317000
                            END;                                        07318000
                        INSERT((COUNT+3).CHR,IDENT[(COUNT+11).WRD],     07319000
                          CHAR&SPC[TOC6],I~REAL(COUNT=0)+1);            07320000
                        COUNT~ COUNT+I;                                 07321000
                        IF MACROTOG THEN                                07322000
                            BEGIN                                       07323000
                                CONVERT(IDENT[1],COUNT~COUNT-2);        07324000
                                COUNT~(COUNT+1).D2;                     07325000
                            END;                                        07326000
                        IDENT[1].CTR ~ COUNT;                           07327000
                        SHIFT(0,IF MACROTOG THEN VSTRING ELSE           07328000
                          VOCTAL,0);                                    07329000
                        GO EXIT;                                        07330000
                    END;                                                07331000
                SHIFT(CHAR, 0, 0);                                      07332000
                GO EXIT;                                                07333000
PSEUDO:         IF NOREAD THEN                                          07334000
                    SHIFT(0, 63, 0)                                     07335000
                ELSE                                                    07336000
                    READACARD;                                          07337000
EXIT:                                                                   07338000
                IF FMTTOG AND NOT DEC THEN DECLARATION;                 07339000
            END;                                                        07340000
        TABLE~(ELB~ ELBAT[P-BELB]).CLASS;                               07341000
        COUNT ~ TTEM;                                                   07342000
    IF P < NXTELB THEN                                                  07343000
        IF A~ELB.CLASS ! 0 AND MACROTOG THEN BEGIN                      07344000
            MOVE(((TANK[ELB].CTR+10).WRD).IC,TANK[ELB],IDENT[1]);       07345000
            IF A=VSTRINGNAME OR A=VMFIELDID THEN % RESET LAD            07346000
                A ~ INDICT;                                             07347000
        END;                                                            07348000
$ INCLUDE 07351001, IF DEBUGGING;                                       07348999
        IF MONITER AND PRINT THEN                                       07349000
            WRITE(PRINTER,MON1,"ELBAT",P,"=",ELB,                       07350000
                ELB.CLASS,ELB.SUB,ELB.ADINFO);                          07351000
    END TABLE;                                                          07352000
        COMMENT  GETSPACE IS RESPONSIBLE FOR ASSIGNING PROPER           07800000
    MACRO ADDRESSES TO MACRO VARIABLES. ALLOCATION OVERFLOWS ARE        07801000
    CAUGHT. BASE VALUES ARE SET UP IN PROCEDURE "MACRO";                07802000
INTEGER PROCEDURE GETSPACE(OWNT,INT,SZ); VALUE OWNT,INT,SZ;             07803000
    BOOLEAN OWNT; INTEGER INT,SZ;                                       07804000
BEGIN                                                                   07805000
    LABEL LAP,GAP;                                                      07806000
    CASE 2|INT + REAL(OWNT) OF BEGIN                                    07807000
        BEGIN                                                           07808000
          IF SZ<0 THEN LACT ~ (LACT+4).D4 | 4 - 1;                      07809000
          IF GETSPACE~LACT~LACT+1 > LALFMAX THEN BEGIN                  07810000
            ERROR(92); % TOO MANY LOCAL VARIABLES                       07811000
            GETSPACE ~ LACT ~ LALFMAX;                                  07812000
          END;                                                          07813000
          SETINUSE(SPACEMAP[LACT.D32],LACT.M32);                        07814000
          IF SZ < 0 THEN LACT ~ LACT + 3 ELSE                           07815000
          IF SZ~(SZ+10).D512 ! 0 THEN LACT ~ LACT + SZ;                 07816000
        END;                                                            07817000
        BEGIN                                                           07818000
          IF SZ<0 THEN GLAT ~ (GLAT+4).D4 | 4 - 1;                      07819000
          IF GETSPACE~GLAT~GLAT+1 > GALFMAX THEN BEGIN                  07820000
            ERROR(93); % TOO MANY GLOBAL VARIABLES                      07821000
            GETSPACE ~ GLAT ~ GALFMAX;                                  07822000
          END;                                                          07823000
          SETINUSE(SPACEMAP[(GLAT-GALFBAS).D32+8],GLAT.M32);            07824000
          IF SZ < 0 THEN GLAT ~ GLAT + 3 ELSE                           07825000
          IF SZ~(SZ+10).D512 ! 0 THEN GLAT ~ GLAT + SZ;                 07826000
        END;                                                            07827000
        IF GETSPACE~LICT~LICT+1 > LINTMAX THEN BEGIN                    07828000
            ERROR(92);                                                  07829000
            GETSPACE ~ LINTMAX;                                         07830000
          END;                                                          07831000
        IF GETSPACE~GLIT~GLIT+1 > GINTMAX THEN BEGIN                    07832000
            ERROR(93);                                                  07833000
            GETSPACE ~ GINTMAX;                                         07834000
          END;                                                          07835000
LAP:    IF GETSPACE~LOCT~LOCT+1 > LPTRMAX THEN BEGIN                    07836000
            ERROR(92);                                                  07837000
            GETSPACE ~ LPTRMAX;                                         07838000
          END;                                                          07839000
GAP:    IF GETSPACE~GLOT~GLOT+1 > GPTRMAX THEN BEGIN                    07840000
            ERROR(93);                                                  07841000
            GETSPACE ~ GPTRMAX;                                         07842000
          END;                                                          07843000
        GO LAP;     % LOCAL INTEGER MPOINTER                            07844000
        GO GAP;     % GLOBAL INTEGER MPOINTER                           07845000
        ;           % LOCAL DICTIONARY ALPHA MSTRING                    07846000
        ;           % GLOBAL DICTIONARY ALPHA MSTRING                   07847000
        ;           % LOCAL DICTIONARY INTEGER MSTRING                  07848000
        ;           % GLOBAL DICTIONARY INTEGER MSTRING                 07849000
        GO LAP;     % LOCAL DICTIONARY MPOINTER                         07850000
        GO GAP;     % GLOBAL DICTIONARY MPOINTER                        07851000
    END GETSPACE CASE;                                                  07852000
END GETSPACE;                                                           07853000
$ INCLUDE 07968001, IF INTERMEDIATE;                                    07899999
                                                                        07900000
        COMMENT  COMPILEFIELD COMPILES B-5500 CODE INTO CODRAY FOR      07901000
    INTEGER MFIELDS & RETURNS AN MFIELD POINTER GIVING THE CODE         07902000
    SEGMENT & RELATIVE ADDRESS OF THE CODE. TWO-WAY CODE IS GIVEN:      07903000
    CODE TO ISOLATE THE FIELD & CODE TO PACK THE FIELD. OPTIMAL CODE    07904000
    IS PRODUCED FOR F&C-FIELDS AND RIGHTMOST BITS UP TO 10;             07905000
REAL PROCEDURE COMPILEFIELD(A,B); VALUE A,B; INTEGER A,B;               07906000
    BEGIN                                                               07907000
        INTEGER D,D1,D2;                                                07908000
        ARRAY C[0:3];                                                   07909000
        LABEL ISO,CATIT,NORM,EXIT,FINAL;                                07910000
        IF NXTCODE.CROW ! NOWCODE THEN                                  07911000
            READ(ISOCODE[NOWCODE~NXTCODE.CROW],120,CODRAY[*]);          07912000
        FILL C[*] WITH                                                  07913000
            OCT0055005540066231, % NOP,NOP,F-1,LFW                      07914000
            OCT0136005500450235, % R27,DIA 0,ISO 0,RTN                  07915000
            OCT0136014000550061, % R27,R30,DIA 0,DIB 0                  07916000
            OCT0055006502350055; % NOP,TRB 0,RTN,NOP                    07917000
        IF B=15 THEN BEGIN                                              07918000
            IF A=33 THEN BEGIN                                          07919000
                C[1].[12:24] ~ REAL(<141>); % 0,INX                     07920000
                GO CATIT;                                               07921000
            END ELSE                                                    07922000
            IF A=18 THEN BEGIN                                          07923000
                C[1].[12:24] ~ REAL(<7425>); % 0,CTF                    07924000
                GO CATIT;                                               07925000
            END;                                                        07926000
            GO ISO;                                                     07927000
        END;                                                            07928000
        IF B{10 AND A+B=48 THEN BEGIN % RIGHT END AND                   07929000
            D ~ 2*B - 1;                                                07930000
            C[1] ~ (*) & D[12:38:10] & REAL(<415>)[24:36:12];           07931000
            GO CATIT;                                                   07932000
        END;                                                            07933000
ISO:    IF (D~(48-A-B)MOD 6)+B { 39 THEN BEGIN                          07934000
            D2 ~ (D1~A DIV 6)|8 + (D2~A MOD 6);                         07935000
            D1 ~ ((A+B-1)DIV 6-D1+1)|8 + D;                             07936000
            C[1] ~ (*)&D2[TOC2]&D1[TOC4]; % DIA, ISO VARIANTS           07937000
            GO CATIT;                                                   07938000
        END ELSE BEGIN                                                  07939000
            ERROR(95); % INTEGER MFIELDS MUST BE { 39 BITS              07940000
            GO FINAL;                                                   07941000
        END;                                                            07942000
CATIT:  D ~ 48-B;                                                       07943000
        IF B = 15 THEN BEGIN                                            07944000
            IF A = 33 THEN D1 ~ 512 ELSE                                07945000
            IF A ! 18 THEN GO NORM;                                     07946000
            IF D = 18 THEN D1 ~ D1 + 256 ELSE                           07947000
            IF D ! 33 THEN GO NORM;                                     07948000
            C[2] ~ (*) & (D1+197)[24:38:10]  % FTC,FTF,CTF,CTC          07949000
                    & REAL(<235>)[36:36:12]; % RTN                      07950000
            GO EXIT;                                                    07951000
        END;                                                            07952000
NORM:                                                                   07953000
        D1 ~ (A DIV 6)|8 + (A~A MOD 6);                                 07954000
        D2 ~ (D DIV 6)|8 + (D~D MOD 6);                                 07955000
        C[2] ~ (*) & D1[TOC4] & D2[TOC6]; % DIA & DIB VARIANTS          07956000
        C[3] ~ (*) & B[TOC2]; % TRB VARIANT                             07957000
EXIT:                                                                   07958000
        MOVE(3,C[1],CODRAY[NXTCODE.CRAD]);                              07959000
        COMPILEFIELD ~ NXTCODE & 3[1:46:2];                             07960000
        WRITE(ISOCODE[NOWCODE],120,CODRAY[*]);                          07961000
        NXTCODE.CRAD ~ NXTCODE.CRAD + 3;                                07962000
        IF NXTCODE.CRAD } 117 THEN BEGIN                                07963000
            NXTCODE ~ 0 & (NOWCODE+1)[TOCROW];                          07964000
            WRITE(ISOCODE[NOWCODE+1],1,C[*]);                           07965000
        END;                                                            07966000
FINAL:                                                                  07967000
    END COMPILEFIELD;                                                   07968000
        COMMENT  DECLARATION HANDLES ALL DECLARATORS, EVEN FUNNY        08000000
    IN-LINE FORMATS. ENTRIES ARE NORMALLY PACKED INTO DECL AND THEN     08001000
    STASHED INTO MDECL FOR LATER RECALL. FUNNY ID IS RETURNED FOR       08002000
    SCANNING PURPOSES. MACRO DECLARATIONS CALL THE COMPILER("MACRO")    08003000
    TO GENERATE CODE. ASSOCIATED MVARIABLES ARE ALSO PROCESSED HERE;    08004000
PROCEDURE DECLARATION;                                                  08005000
    BEGIN                                                               08006000
        INTEGER S,Z,SGN,SNG,TDECP;                                      08007000
        REAL A,TEMPA;                                                   08007500
        BOOLEAN MTOG,MCTOG,OWT,PRCTOG,STRTOG,SWIT,MFLDTOG;              08008000
$ INCLUDE 08022001, IF DEBUGGING;                                       08008999
        FORMAT SS(I6"-"A1);                                             08009000
        PROCEDURE TRACER(T); VALUE T; BOOLEAN T;                        08010000
        BEGIN                                                           08011000
        ARRAY ERB[0:14];                                                08011999
        STREAM PROCEDURE TRACEIT(D,T,I); VALUE I,T;                     08012000
        BEGIN                                                           08013000
            DI~D; DS~8 LIT " "; SI~D; DS~14 WDS; DI~D;                  08014000
            DS~LIT "."; I(DS~LIT ".");                                  08015000
            T(DS~ 6 LIT "BEGIN ";JUMP OUT TO X);                        08016000
            DS~ 6 LIT "END   ";                                         08017000
X:          DS~ 11 LIT "DECLARATION";                                   08018000
        END TRACEIT;                                                    08019000
       TRACEIT(ERB,T,INDENT);                                           08020000
        WRITE(PRINTER,15,ERB[*]);                                       08021000
    END;                                                                08022000
%                                                                       08023000
    ALPHA STREAM PROCEDURE BLOCKNAME(BNO); VALUE BNO; BEGIN             08029000
        SI~LOC BNO; DI~LOC BLOCKNAME; DI~DI+2;                          08030000
        DS~2 LIT "5?"; DS~4 DEC;                                        08031000
    END BLOCKNAME;                                                      08032000
PROCEDURE STARTDEC(TDECP); INTEGER TDECP; BEGIN LABEL L; L:             08033000
        DEC ~ TRUE; INDENT ~ INDENT + 3;                                08034000
$ INCLUDE 08034051, IF DEBUGGING;                                       08034049
        IF TRACE THEN TRACER(TRUE);                                     08034050
        SAVEIT;                                                         08035000
        NWCT ~ 3;                                                       08036000
        TNWCT ~ 0;                                                      08037000
        TDECP ~ DECP;                                                   08038000
        DECL[0] ~ "     ";                                              08039000
        BLANK(DECL[1],3,62);                                            08040000
        IF FMTTOG THEN BEGIN                                            08041000
            STEPI;                                                      08042000
            DO BEGIN PACKET; STEPI END UNTIL TYPE=0 AND ELB=">";        08043000
            FMTTOG ~ <3>;                                               08043050
        END;                                                            08044000
    END;                                                                08045000
        LABEL LOWN,RES,REP,BRAIL,MON,SWTCH,STRM,PROC,STR,FFL,FMTL,      08046000
          DEFA,MAC,TEST,VAL,ENDEC,EXIT;                                 08047000
$ INCLUDE 08047051, IF INTERMEDIATE;                                    08047049
        LABEL MFLD;                                                     08047050
$ INCLUDE 08047101, IF ADVANCED;                                        08047099
        LABEL DCT,MPTR;                                                 08047100
        SWITCH SW ~ LOWN,RES,BRAIL,BRAIL,BRAIL,BRAIL,BRAIL,MON,MON,     08048000
          RES,RES,SWTCH,STRM,PROC,FFL,STR,FFL,FFL,FMTL,MAC,DEFA         08049000
$ INCLUDE 08049051, IF INTERMEDIATE;                                    08049049
          ,MFLD                                                         08049050
$ INCLUDE 08049101, IF ADVANCED;                                        08049099
          ,DCT,MPTR                                                     08049100
          ;                                                             08049150
%                                                                       08050000
        STARTDEC(TDECP);                                                08051000
        IF FMTTOG THEN GO ENDEC ELSE GO RES;                            08052000
REP:    PACKET;                                                         08053000
RES:    IF STEPI<VTORF OR TYPE>VOTHERS THEN                             08054000
            ERROR(2);                                                   08055000
        IF TYPE=VSTRINGNAME AND ELB.BLKLVL}MCBL THEN BEGIN              08056000
            ERROR(59); % DUP STRING DECL                                08057000
            STEPI;                                                      08058000
            GO TEST;                                                    08059000
        END;                                                            08060000
        SGN~ ELB.SUB;                                                   08061000
        PACKET;                                                         08062000
        IF TYPE=VOTHERS AND NOT MCTOG THEN                              08063000
            IF SGN=VALUEV THEN                                          08064000
                GO TO VAL                                               08065000
            ELSE                                                        08066000
                GO TO SW[SGN];                                          08067000
        IF (MTOG OR SPCT) AND NOT(STRTOG OR MCTOG OR MFLDTOG) THEN      08068000
            GO TO TEST;                                                 08069000
        IF ELB.BLKLVL}BLVL AND STRTOG THEN                              08070000
            ERROR(3);                                                   08071000
        IF PRCTOG THEN                                                  08072000
            BEGIN                                                       08073000
                A~ ADDICT(VALGOLIDENT, PROCEDUREV);                     08074000
                IF INDEXING THEN BEGIN                                  08075000
                    MOVE(8,DICT[A.IR,A.IC],INDEX(0));                   08076000
                    MOVE(1,LINKS[0],INDEX(9));                          08077000
                    WRITE(INDEX);                                       08078000
                    INXR ~ INXR + 1;                                    08079000
                END;                                                    08080000
                SPCT~ TRUE;                                             08081000
                ESCAN;                                                  08082000
                STARTBLOCK(FALSE);                                      08083000
                GO ENDEC;                                               08084000
            END;                                                        08085000
        IF MCTOG THEN                                                   08086000
            BEGIN                                                       08087000
                ARRAY B[0:20];                                          08088000
                INTEGER D,E,F;                                          08089000
                LABEL ED,XX,E108;                                       08090000
                MOVE(D~(IDENT[1].CTR+10).WRD,IDENT[1],B[1]);            08091000
                STEPI;                                                  08091100
$ INCLUDE 08110001, IF ADVANCED;                                        08091999
                IF TYPE=VSPECIAL AND ELB="=" THEN                       08092000
                    BEGIN                                               08093000
                        STEPI;                                          08094000
                        IF E~(IDENT[1].CTR+10).WRD>20-D THEN            08095000
                            BEGIN                                       08096000
                                ERROR(53);                              08097000
                                GO ED;                                  08098000
                            END;                                        08099000
                        MOVE(E, IDENT[1], B[D+1]);                      08100000
                        D~ D+E+1;                                       08101000
                        B[0]~ D;                                        08102000
                        MOVE(D, B, IDENT);                              08103000
                        A~ ADDICT(VOTHERS, REPLACEV);                   08104000
                        PUT(A,GET(A)&(A+D-E)[TOADINFO]);                08105000
ED:                                                                     08106000
                        IF STEPI!0 AND ELB!";" THEN                     08107000
                            ERROR(54);                                  08108000
                        GO ENDEC;                                       08109000
                    END;                                                08110000
                MOVE(D, B[1], IDENT[1]);                                08111000
                A~ ADDICT(VMACRONAME,Z);                                08112000
$ INCLUDE 08131001, IF INTERMEDIATE;                                    08112999
                IF TYPE=VOTHERS AND ELB.SUB=EQVV THEN BEGIN             08113000
                    EQVTOG ~ TRUE;                                      08114000
                    IF STEPI!VMACRONAME OR (LISTV!                      08115000
                        ELB.SUB!EQVV) THEN BEGIN                        08116000
                        ERROR(74);                                      08117000
                        PUT(A,GET(A)&VNOTDEC[TOCLSUB]);                 08118000
                        END                                             08119000
                    ELSE                                                08120000
                        PUT(A,GET(A)&LAD[TOADINFO]&EQVV[TOSUB]);        08121000
                    EQVTOG ~ FALSE;                                     08122000
                    IF STEPI!0 OR ELB!";" THEN                          08123000
                        ERROR(54);                                      08124000
                    IF INDEXING THEN BEGIN                              08125000
                        MOVE(1,LINKS[0],B[9]);                          08126000
                        E ~ LAD;                                        08127000
                        GO XX;                                          08128000
                    END;                                                08129000
                    GO ENDEC;                                           08130000
                END CHEAP MACRO;                                        08131000
                NEXTDICT~ NEXTDICT+2;                                   08132000
                DICT[NEXTDICT.IR,NEXTDICT.IC] ~                         08133000
                    DICT[(D~NEXTDICT+1).IR,D.IC] ~ 0;                   08134000
$ INCLUDE 08134201, IF ADVANCED;                                        08134019
                IF TYPE=VOTHERS AND ELB.SUB=IFV THEN BEGIN              08134020
                    IF STEPI!VNUMBER OR ELB.SUB!INTEGERV THEN BEGIN     08134040
E108:                   ERROR(108);                                     08134060
                        GO TEST;                                        08134100
                    END;                                                08134120
                    IF F~CONSTANTCONVERT(IDENT[1],COUNT)+1>36 THEN      08134140
                        GO E108;                                        08134160
                    STEPI;                                              08134180
                END CONDITIONAL MACRO;                                  08134200
                IF TYPE!0 OR ELB!";" THEN                               08135000
                    ERROR(4);                                           08136000
                IF INDEXING THEN MOVE(1,LINKS[0],B[9]);                 08137000
                MACRO(E ~ NEXTDICT-2);                                  08138000
                IF F > 0 THEN                                           08138050
                    DICT[E.IR,E.IC] ~ (*) & F[TOMCIF];                  08138100
                IF INDEXING THEN BEGIN                                  08139000
XX:                 MOVE(7,DICT[A.IR,A.IC],B[0]);                       08140000
                    B[7] ~ DICT[E.IR,E.IC];                             08141000
                    B[8] ~ DICT[(E~E+1).IR,E.IC];                       08142000
                    WRITE(INDEX,10,B[*]);                               08143000
                    INXR ~ INXR + 1;                                    08144000
                END;                                                    08145000
                GO ENDEC;                                               08146000
            END;                                                        08147000
        IF STRTOG THEN                                                  08148000
            BEGIN LABEL PTRP,TSMI;                                      08149000
                A~ ADDICT(VSTRINGNAME, SNG);                            08151000
                IF TEMPA.LIM = 0 THEN BEGIN                             08152000
                    TEMPA ~ NEXTDICT & NEXTDICT[TOREF];                 08153000
                END ELSE BEGIN                                          08154000
                    PUT(TEMPA,NEXTDICT&GET(TEMPA)[TOXF]);               08155000
                    TEMPA.LIM ~ NEXTDICT;                               08156000
                END;                                                    08157000
                NEXTDICT ~ NEXTDICT + 1;                                08158000
                PUT(TEMPA,A);                                           08159000
                DICT[(NEXTDICT+1).IR,(NEXTDICT+1).IC] ~ 0;              08160000
                IF STEPI!0 OR ELB!"[" THEN                              08161000
                    BEGIN                                               08162000
                        T ~ T - 1;                                      08163000
                        IF ELB = ";" THEN                               08164000
                            IF BOOLEAN(SNG).[46:1] THEN                 08165000
                                GO PTRP                                 08166000
                            ELSE                                        08167000
                            ERROR(80);                                  08168000
                        GO TSMI;                                        08169000
                    END;                                                08170000
                IF STEPI=0 AND ELB="*" THEN                             08171000
                    Z ~ SNG.[47:1]|9-1                                  08172000
                ELSE                                                    08173000
                IF TYPE = VNUMBER THEN BEGIN                            08174000
                    Z ~ CONSTANTCONVERT(IDENT[1],COUNT);                08175000
                    IF Z>8 AND BOOLEAN(SNG) THEN                        08176000
                        ERROR(5)                                        08177000
                    ELSE                                                08178000
                    IF Z>MSMAX AND NOT BOOLEAN(SNG) THEN                08179000
                        ERROR(5);                                       08180000
                END ELSE                                                08181000
                ERROR(5);                                               08182000
                IF STEPI!0 OR ELB!"]" THEN BEGIN                        08183000
                    ERROR(6);                                           08184000
                    GO TEST;                                            08185000
                END;                                                    08186000
$ INCLUDE 08192001, IF ADVANCED;                                        08186999
                IF STEPI=0 AND ELB="=" THEN BEGIN % OFFSET              08187000
                    IF STEPI = VNUMBER THEN                             08188000
                        Z ~ Z & CONSTANTCONVERT(IDENT[1],COUNT)         08189000
                          [TOOFFSET];                                   08190000
                END ELSE                                                08191000
                T ~ T - 1;                                              08192000
PTRP:           PUT(TEMPA,0&GET(TEMPA)[TOXF]);                          08193000
                TEMPA ~ TEMPA.REF;                                      08194000
                DO BEGIN                                                08195000
                    TEMPA ~ GET(A~TEMPA);                               08196000
                    PUT(A,(Z+REAL(SNG=0))&S~GETSPACE(OWT.[47:1],SNG,    08197000
                        Z)[TOREF]);                                     08198000
$ INCLUDE 08200001, IF DEBUGGING;                                       08198999
                    IF S ! 0 AND SRT AND PRINT THEN                     08199000
                      WRITE(PRINTER,SS,S,IF OWT THEN "G" ELSE "L");     08200000
                    IF INDEXING THEN BEGIN                              08201000
                        MOVE(8,DICT[TEMPA.XIR,TEMPA.XIC],DECL);         08202000
                        A ~ DECL[0];                                    08202100
                        MOVE(1,DICT[A.IR,A.IC],DECL[8]);                08203000
                        MOVE(1,LINKS,DECL[9]);                          08204000
                        WRITE(INDEX,10,DECL[*]);                        08205000
                        INXR ~ INXR + 1;                                08206000
                    END;                                                08207000
                END UNTIL TEMPA.LIM = 0;                                08208000
                IF BOOLEAN(SNG).[46:1] THEN GO ENDEC;                   08209000
TSMI:           IF STEPI ! VSPECIAL OR "," ! ELB ! ";" THEN BEGIN       08210000
                    ERROR(54);                                          08210500
                    GO ENDEC;                                           08211000
                END;                                                    08211500
                IF ELB = "," THEN GO REP;                               08212000
                IF ELB = ";" AND BOOLEAN(SNG).[46:1] THEN GO PTRP;      08212500
                GO ENDEC;                                               08213000
            END;                                                        08213500
$ INCLUDE 08257001, IF INTERMEDIATE;                                    08213999
        IF MFLDTOG THEN BEGIN                                           08214000
            LABEL MER,MER2,RETRY;                                       08215000
            INTEGER S1,S2;                                              08216000
            A ~ ADDICT(VMFIELDID,SNG);                                  08217000
            NEXTDICT ~ NEXTDICT + 1;                                    08218000
            IF STEPI ! 0 OR ELB ! "=" THEN                              08219000
                ERROR(61);                                              08220000
RETRY:      IF STEPI ! 0 OR ELB ! "[" THEN BEGIN                        08221000
                IF TYPE = VSTRINGNAME THEN BEGIN                        08222000
                    IF 4{(S1~ELB.SUB){5 THEN BEGIN % DICT OFFSET        08223000
                        S2 ~ GET(LAD).OFFSET;                           08224000
                        IF NOT(BOOLEAN(S1)EQV BOOLEAN(SNG)) THEN        08225000
                            ERROR(101); % MODE DISAGREEMENT             08226000
                    END;                                                08227000
                    IF STEPI!0 OR ELB!"." THEN                          08228000
                        ERROR(102); % MISSING "." IN OFFSET             08229000
                    GO RETRY;                                           08230000
                END ELSE BEGIN                                          08231000
MER:                ERROR(96); % MISSING "[:]"IN DECLARATION            08232000
                    GO TEST;                                            08233000
                END;                                                    08234000
            END;                                                        08235000
            IF STEPI ! VNUMBER THEN GO MER2;                            08236000
            S ~ CONSTANTCONVERT(IDENT[1],COUNT);                        08237000
            IF STEPI ! 0 OR ELB ! ":" THEN GO MER;                      08238000
            IF STEPI ! VNUMBER THEN GO MER2;                            08239000
            Z ~ CONSTANTCONVERT(IDENT[1],COUNT);                        08240000
            IF BOOLEAN(SNG) THEN BEGIN                                  08241000
                IF NOT(1{S{47 OR 1{Z{47 OR S+Z{48) THEN BEGIN           08242000
MER2:               ERROR(97); % INVALID MFIELD DESCRIPTION             08243000
                    GO TEST;                                            08244000
                END;                                                    08245000
                PUT(NEXTDICT-1,COMPILEFIELD(S,Z)&S2[TOMFOFF]);          08246000
            END ELSE                                                    08247000
                PUT(NEXTDICT-1,-Z&S[TOMFBGN]&S2[TOMFOFF]);              08248000
            IF INDEXING THEN BEGIN                                      08249000
                MOVE(8,DICT[A.IR,A.IC],INDEX(0));                       08250000
                MOVE(1,DICT[(A~NEXTDICT-1).IR,A.IC],INDEX(8));          08251000
                MOVE(1,LINKS[0],INDEX(9));                              08252000
                WRITE(INDEX);                                           08253000
                INXR ~ INXR + 1;                                        08254000
            END;                                                        08255000
            GO TEST;                                                    08256000
        END;                                                            08257000
        A ~ IF Z=VNOTDEC THEN ADDICT(VNOTDEC,0) ELSE                    08258000
          ADDICT(VALGOLIDENT,Z);                                        08259000
TEST:   ESCAN;                                                          08260000
        IF ELB!";" AND(ELB!"," OR SWIT) THEN                            08261000
            BEGIN                                                       08262000
                PACKET;                                                 08263000
                GO TO TEST                                              08264000
            END;                                                        08265000
        IF ELB="," THEN                                                 08266000
            GO TO REP;                                                  08267000
        GO TO ENDEC;                                                    08270000
LOWN:   OWT~ TRUE;                                                      08271000
        GO TO RES;                                                      08272000
BRAIL:                                                                  08273000
        SNG ~ REAL(SGN=INTEGERV) + SNG;                                 08274000
        GO TO RES;                                                      08275000
SWTCH:  SWIT~ TRUE;                                                     08276000
        Z~ SGN;                                                         08277000
        GO TO RES;                                                      08278000
STRM:   STREAMTOG~ TRUE;                                                08279000
        GO TO RES;                                                      08280000
PROC:   PROCTOG~ PRCTOG~ TRUE;                                          08281000
        GO TO RES;                                                      08282000
MAC:    IF MACROTOG THEN                                                08283000
            BEGIN LABEL L; L:                                           08284000
            ERROR(57); DO STEPI UNTIL TYPE=0 AND ELB=";"; STEPI;        08285000
            IF TYPE=VOTHERS AND ELB.SUB=BEGINV THEN                     08286000
                BEGIN                                                   08287000
                Z~ 1;                                                   08288000
                DO IF STEPI= VOTHERS THEN                               08289000
                    IF ELB.SUB=BEGINV THEN Z~Z+1 ELSE                   08290000
                    IF ELB.SUB=ENDV THEN Z~Z-1                          08291000
                UNTIL TYPE=0 AND ELB=";" AND Z=0;                       08292000
                END;                                                    08293000
            RECALLIT;  NXTELB~ T;                                       08294000
            GO EXIT;                                                    08295000
            END;                                                        08296000
        MCTOG ~ TRUE;                                                   08297000
        GO TO RES;                                                      08298000
MON:    MTOG~ TRUE;                                                     08299000
        GO TO RES;                                                      08300000
STR:    STRTOG~ TRUE;                                                   08301000
        OWT~ OWT OR NOT MACROTOG;                                       08302000
        GO TO RES;                                                      08303000
VAL:    Z~ VNOTDEC;                                                     08304000
        GO TO RES;                                                      08305000
FMTL:   FMTTOG ~ TRUE;                                                  08306000
FFL:    Z~ SGN;                                                         08307000
        GO TO RES;                                                      08308000
$ INCLUDE 08310001, IF INTERMEDIATE;                                    08308999
MFLD:   MFLDTOG ~ TRUE;                                                 08309000
        GO RES;                                                         08310000
$ INCLUDE 08316001, IF ADVANCED;                                        08310999
MPTR:   STRTOG ~ TRUE;                                                  08311000
        OWT ~ OWT OR NOT MACROTOG;                                      08312000
        SNG ~ SNG + 2;                                                  08313000
        GO RES;                                                         08314000
DCT:    SNG ~ 4;                                                        08315000
        GO RES;                                                         08316000
DEFA:   BEGIN                                                           08317000
        COMMENT  DEFINES, BOTH PARAMETRIC AND REGULAR ARE PROCESSED     08318000
    HERE. PACK PUTS TOGETHER RECONSTRUCTION INFO. SUBCLASS MEANS:       08319000
        0 = SIMPLE IDENTIFIER..REPLACE & RE-SEARCH DICT                 08320000
        1 = SEVERAL ELEMENTS..PSEUDOREAD                                08321000
        2 = SINGLE SPECIAL CHR..SET CHAR                                08322000
        3 = SEVERAL ELEMENTS, SPCHR LAST..PSEUDOREAD                    08323000
       >4 = PARAMETRIC WITH (N-4) PARAMS..INTERPRET&RECONSTRUCT         08324000
    CASES 0->3 ARE HANDLED DIRECTLY BY INDICT. PARAMETRICS MUST BE      08325000
    HANDLED BY TABLE ITSELF DUE TO RECURSION;                           08326000
            PROCEDURE PACK(H,HX,CON,CX,CL,B); VALUE HX,CL,B;            08327000
            ARRAY H,CON[0];                                             08328000
            REAL CL,CX,HX;                                              08329000
            BOOLEAN B;                                                  08330000
            BEGIN                                                       08331000
                INTEGER DCLASS,FIELD;                                   08332000
                IF NWCT > 5 THEN BEGIN                                  08333000
                    DCLASS ~ 3;                                         08334000
                    IF CX+NWCT~NWCT.WRD+1 > 127 THEN BEGIN              08335000
                        OVRFL;                                          08336000
                        CX ~ 0;                                         08337000
                    END;                                                08338000
                    FIELD ~ CX;                                         08339000
                    MOVE(NWCT,DECL,CON[CX]);                            08340000
                    CX ~ CX + NWCT;                                     08341000
                    DECL[0] ~ "     ";                                  08342000
                    BLANK(DECL[1],NWCT.C6,NWCT);                        08343000
                END ELSE BEGIN                                          08344000
                    DCLASS ~ NWCT - 3;                                  08345000
                    FIELD ~ DECL[0].C34;                                08346000
                    DECL[0] ~ "     ";                                  08347000
                END;                                                    08348000
                NWCT ~ 3;                                               08349000
                IF B THEN                                               08350000
                    H[HX] ~ 0&DCLASS[TOPDC1]&CL[TOPDP1]&                08351000
                        FIELD[TOPDF1]                                   08352000
                ELSE                                                    08353000
                    H[HX] ~ (*)&DCLASS[TOPDC2]&CL[TOPDP2]&              08354000
                        FIELD[TOPDF2];                                  08355000
            END PACK;                                                   08356000
            LABEL DEF,MORE,NEXTPARA;                                    08357000
            BOOLEAN PARATYPE;                                           08358000
            ARRAY HOLD,CON[0:127];                                      08359000
            INTEGER PNO,DNO,CX;                                         08360000
            REAL SPC,DC,A;                                              08361000
DEF:        IF STEPI<VTORF OR TYPE>VOTHERS THEN                         08362000
                ERROR(2);                                               08363000
            A ~ ADDICT(VDEFINEDID,0);                                   08364000
                IF STEPI=0 AND ELB="(" THEN BEGIN                       08365000
                    PARATYPE ~ TRUE;                                    08366000
                    DNO ~ 0;                                            08367000
                    NEXTDICT ~ NEXTDICT + 1; % ADINFO                   08368000
NEXTPARA:           IF STEPI=VPARAMID THEN ERROR(91);                   08369000
                    Z ~ ADDICT(VPARAMID,PNO~PNO+1);                     08370000
                    IF STEPI!0 OR NOT(ELB="," OR ELB=")") THEN          08371000
                        ERROR(90);                                      08372000
                    IF ELB = "," THEN GO NEXTPARA;                      08373000
                END ELSE T ~ T - 1;                                     08374000
            IF STEPI!0 OR ELB!"=" THEN BEGIN                            08375000
                T ~ T - 1;                                              08376000
                ERROR(61);                                              08377000
            END;                                                        08378000
            NWCT ~ 3;                                                   08379000
            TNWCT ~ CX ~ 0;                                             08380000
            DECL[0] ~ "     ";                                          08381000
            BLANK(DECL[1],3,62);                                        08382000
            TDECP ~ DECP;                                               08383000
            Z ~ DC ~ 0;                                                 08384000
            WHILE STEPI!0 OR  ELB!"#" OR DC!0 DO BEGIN                  08385000
                IF TYPE = VPARAMID THEN BEGIN                           08386000
                        DNO ~ DNO + 1;                                  08387000
                    PACK(HOLD,(DNO-1).D2,CON,CX,ELB.SUB,                08388000
                        BOOLEAN(DNO))                                   08389000
                END ELSE                                                08390000
                PACKET;                                                 08391000
                Z ~ Z + 1;                                              08392000
                IF TYPE=VOTHERS AND ELB.SUB=DEFINEV THEN                08393000
                    DC~ DC+1                                            08394000
                ELSE                                                    08395000
                    IF TYPE=0 AND ELB="#" THEN BEGIN                    08396000
                      IF STEPI=0 AND ELB=";" THEN                       08397000
                        DC~ DC-1;                                       08398000
                      T ~ T - 1;                                        08399000
                    END;                                                08400000
                SPC ~ REAL(TYPE!0);                                     08401000
            END;                                                        08402000
                IF PARATYPE THEN BEGIN                                  08403000
                    DNO ~ DNO + 1;                                      08404000
                    PACK(HOLD,(DNO-1).D2,CON,0,0,BOOLEAN(DNO));         08405000
                    DNO ~ DNO + 1;                                      08406000
                    PACK(HOLD,(DNO-1).D2,CON,0,0,BOOLEAN(DNO));         08407000
                    PUT(A,A~GET(A)&(PNO+4)[TOSUB]);                     08408000
                    PUT(A~A.ADINFO,(DNO+1)&DECP[TOREF]                  08409000
                        & CX[TOOFFSET]);                                08410000
                    WRITIT(HOLD,DNO.D2+1,DECP,MDECL);                   08411000
                    WRITIT(CON,CX,DECP,MDECL);                          08412000
                    FOR I~0 STEP 1 UNTIL 124 DO                         08413000
                        WHILE Z~DLIST[I] > A DO                         08414000
                            DLIST[I] ~ DICT[Z.IR,Z.IC];                 08415000
                    NEXTDICT ~ A ~ A + PNO + 1; % SPACE FOR PARAMS      08416000
                    DICT[(A~A+1).IR,A.IC] ~                             08417000
                        DICT[(A~A+1).IR,A.IC] ~ 0;                      08418000
                    PARATYPE ~ BOOLEAN(PNO~DNO~0);                      08419000
                    GO MORE;                                            08420000
                END;                                                    08421000
            PUT(A,GET(A)&(2|REAL(NOT BOOLEAN(SPC)).[47:1]+REAL(Z>1))    08422000
                [TOSUB]&TDECP[TOADINFO]);                               08423000
            IF TNWCT~TNWCT+NWCT~NWCT+2 > 2037 THEN BEGIN                08424000
                ERROR(62);                                              08425000
                TNWCT ~ 2037;                                           08426000
            END;                                                        08427000
            WRITIT(DECL,(TNWCT+6).WRD,DECP,MDECL);                      08428000
            IF TDECP.IR ! MSTORP.PDF2 THEN BEGIN                        08429000
                READIT(DECL,1,TDECP&1[TOREF],MDECL);                    08430000
%               FLUSH & GET BACK HEAD OF STRING                         08431000
            END;                                                        08432000
            MDECL[TDECP.IC].CTR ~ TNWCT-5-SPC;                          08433000
%                                                                       08434000
MORE:                                                                   08435000
            IF STEPI=0 AND ELB="," THEN BEGIN                           08436000
                GO DEF;                                                 08437000
            END ELSE BEGIN                                              08438000
            IF TYPE ! VSPECIAL OR ELB ! ";" THEN                        08438050
                ERROR(105);                                             08438100
                RECALLIT;                                               08439000
                NXTELB ~ T;                                             08440000
                GO EXIT;                                                08441000
            END;                                                        08442000
        END;                                                            08443000
ENDEC:  PACKET;                                                         08444000
        RECALLIT;                                                       08445000
        NXTELB~ T;                                                      08446000
        IF MCTOG OR STRTOG OR MFLDTOG THEN                              08447000
            GO EXIT;                                                    08448000
        TNWCT~TNWCT+NWCT~NWCT+2;                                        08449000
        IF TNWCT>8189 THEN ERROR(55);                                   08450000
        SNG ~ (TNWCT+6).WRD;                                            08451000
        IDENT[1]~ BLOCKNAME(BECT~ BECT+1);                              08452000
        PUT(A~ADDICT(VDECLRN,REAL(PRCTOG)),GET(A)&(LAD~TDECP)           08453000
            [TOADINFO]);                                                08454000
        SHIFT(0,VDECLRN,REAL(PRCTOG));                                  08455000
        WRITIT(DECL,(NWCT+6).WRD,DECP,MDECL);                           08456000
        IF TDECP.IR ! MSTORP.PDF2 THEN BEGIN                            08457000
                READIT(DECL,1,TDECP&1[TOREF],MDECL);                    08458000
%               FLUSH & GET BACK HEAD OF STRING                         08459000
        END;                                                            08460000
        MDECL[TDECP.IC].CTR ~ TNWCT-4;                                  08461000
EXIT:   CHAR ~ " "; % DECL FOLLOWED BY SEMI                             08462000
        NWCT ~ 3; % CLEAR RECORD OF USING DECL                          08463000
$ INCLUDE 08464001, IF DEBUGGING;                                       08463999
        IF TRACE THEN TRACER(FALSE);                                    08464000
        IF NOT (FMTTOG.[46:1] OR MACROTOG) THEN                         08465000
            BLOCKLEVEL ~ BLVL - NEST;                                   08465050
        DEC ~ FMTTOG ~ FALSE; INDENT ~ INDENT - 3;                      08465100
    END DECLARATION;                                                    08466000
COMMENT  INTERPRETATION SECTION. FIRST PROCEDURES ARE ALGOL-TYPE        10000000
    BROAD-SCOPE SYNTAX ANALYZERS. IF CALLED, AND THE ELEMENTS           10001000
    SCANNED AGREE WITH THE ANALYZER, THE ELEMENTS ARE PLACED INTO       10002000
    ACCM. IF FAILURE OCCURS DOWN THE LINE, -HM- IS RESTORED TO ITS      10003000
    PREVIOUS VALUE & A BLANK MOVED IN TO PROTECT FURTHER OPERATIONS     10004000
    IN ACCM. FAILURE USUALLY LEAVES MANY THINGS IN ACCM(ALTHOUGH HM     10005000
    PREVENTS THEIR ACCESS)AND MANY ITEMS PRESCANNED IN TANK-ELBAT.      10006000
    PROCEDURE NAMES PRETTY MUCH SELF-EXPLANATORY;                       10007000
        COMMENT  MACRON IS A RECURSIVE INTERPRETER FOR MACRO CODE.      10008000
    LOCAL VARIABLES OF MACROS ARE KEPT IN ARRAY -LOCAL-;                10009000
PROCEDURE MACRON(QQ,Q); VALUE QQ,Q; REAL QQ,Q;                          10010000
BEGIN                                                                   10011000
BOOLEAN PROCEDURE ARITHEXPRESS; FORWARD;                                10012000
BOOLEAN PROCEDURE BOOLEANEXPRESS; FORWARD;                              10013000
BOOLEAN PROCEDURE LINKPART(P); VALUE P; BOOLEAN P; FORWARD;             10014000
$ INCLUDE 10014101, IF DEBUGGING;                                       10014099
PROCEDURE TRACER(T);VALUE T;BOOLEAN T;FORWARD;                          10014100
        COMMENT  OPCODE DIALS INTO A CODE STRING & SPLITS NEXT          10015000
    INSTRUCTION INTO ITS OPCODE & ADDRESS COMPONENTS;                   10016000
STREAM PROCEDURE OPCODE(W,C,O,A);                                       10017000
    VALUE C;                                                            10018000
    BEGIN                                                               10019000
        DI~O; SI~W; SI~SI+C; DI~DI+7; DS~CHR;                           10020000
        DI~A; DS~6 LIT "0"; DS~2 CHR;                                   10021000
    END;                                                                10022000
        COMMENT  MOV IS AN ARBITRARY CHR SHUFFLER;                      10023000
STREAM PROCEDURE MOV(NN,N,S,SS,D,DD);                                   10024000
    VALUE NN, N, SS, DD;                                                10025000
    BEGIN                                                               10026000
        SI~S; SI~SI+SS; DI~D; DI~DI+DD;                                 10027000
        NN(DS~ 32 CHR;DS~ 32 CHR);                                      10028000
        DS~ N CHR                                                       10029000
    END;                                                                10030000
        COMMENT  SCANUMBERCONV IMPLEMENTS "ENTIER". THE FIRST DIGIT     10031000
    SEQUENCE IS FOUND IN THE MSTRING ARRAY. NO SIGN OR DECIMAL FIELD    10032000
    IS RECOGNIZED. SCAN LIMIT IS SET BY CTR FIELD;                      10033000
INTEGER STREAM PROCEDURE SCANUMBERCONV(S);                              10034000
    BEGIN                                                               10035000
        LOCAL O, N, M;                                                  10036000
        LABEL FD, EXIT;                                                 10037000
        SI~S; DI~LOC M; DI~DI+7; DS~CHR; DI~DI+7; DS~CHR;               10038000
        DI~DI+7; DS~CHR;                                                10039000
        O(IF SC}"0" THEN IF SC{"9" THEN JUMP OUT TO FD;SI~ SI+1);       10040000
        N(2(32(IF SC}"0" THEN IF SC{"9" THEN JUMP OUT 3 TO FD;          10041000
          SI~SI+1)));                                                   10042000
        M(4(32(32(IF SC}"0" THEN IF SC{"9" THEN JUMP OUT 4 TO FD;       10043000
          SI~SI+1))));                                                  10044000
        DI~ LOC SCANUMBERCONV;                                          10045000
        DS~ 8 LIT "+0000001";                                           10046000
        GO EXIT;                                                        10047000
FD:     8(SI~ SI+1;TALLY~ TALLY+1;IF SC<"0" THEN JUMP OUT;IF SC>"9"     10048000
          THEN JUMP OUT);                                               10049000
        N~ TALLY;                                                       10050000
        SI~ SI-N;                                                       10051000
        DI~ LOC SCANUMBERCONV;                                          10052000
        DS~ N OCT;                                                      10053000
EXIT:                                                                   10054000
    END SCANUMBERCONV;                                                  10055000
        COMMENT  TRANS TRANSFERS MACRO GENERATED WARNING & ERROR        10056000
    MESSAGES INTO A PRINT ROW;                                          10057000
STREAM PROCEDURE TRANS(S,D,M,N,ML,MN,E,A,T);                            10058000
    VALUE M,N,ML,E,T;                                                   10059000
    BEGIN                                                               10060000
        SI~S; SI~SI+3; DI~D; M(DS~32 CHR;DS~32 CHR);                    10061000
        DS~N CHR; DS~6 LIT " >>>>>";                                    10062000
        SI~MN; SI~SI+3; DS~ML CHR; DS~2 LIT " +";                       10063000
        SI~LOC E; DS~4 DEC; DI~A; DS~8 LIT "*";                         10064000
        SI~A; DS~3 WDS;                                                 10065000
        SI~LOC T; SI~SI+1; DS~7 CHR;                                    10066000
    END TRANS;                                                          10067000
$ INCLUDE 10085001, IF ADVANCED;                                        10067999
        COMMENT  UGET GETS & MONITORS ONE WORD FROM THE USER            10068000
    DICTIONARY;                                                         10069000
REAL PROCEDURE UGET(A); VALUE A; INTEGER A;                             10070000
    BEGIN                                                               10071000
        UGET ~ V ~ UDICT[A.IR,A.IC];                                    10072000
$ INCLUDE 10075001, IF DEBUGGING;                                       10072999
        IF MONITER AND PRINT THEN                                       10073000
            WRITE(PRINTER,MON1,"UDICT",A.ADINFO,"=",V,                  10074000
                V.BLKLVL,V.USIZ,V.UQUAL);                               10075000
    END;                                                                10076000
        COMMENT  UPUT PUTS & MONITORS ONE WORD INTO THE USER            10077000
    DICTIONARY;                                                         10078000
PROCEDURE UPUT(A,V); VALUE A,V; INTEGER A; REAL V;                      10079000
    BEGIN                                                               10080000
        UDICT[A.IR,A.IC] ~ V;                                           10081000
        IF MONITER AND PRINT THEN                                       10082000
            WRITE(PRINTER,MON1,"UDICT",A.ADINFO,"~",V,                  10083000
                V.BLKLVL,V.USIZ,V.UQUAL);                               10084000
    END;                                                                10085000
        COMMENT  LBRB IS USED TO RECURSIVELY SCAN OFF SUBSCRIPT         10100000
    EXPRESSIONS ASSOCIATED WITH VARIABLES;                              10101000
        PROCEDURE LBRB;                                                 10102000
            BEGIN                                                       10103000
$ INCLUDE 10103051, IF DEBUGGING;                                       10103049
            IF TRACE THEN TRACER(<2>);                                  10103050
                SETINSERT;                                              10104000
                WHILE(STEPI!VSPECIAL OR ELB!"]") AND TYPE!63 DO         10105000
                    IF TYPE!VSPECIAL THEN                               10106000
                        SETPACK                                         10107000
                    ELSE                                                10108000
                        IF ELB="[" THEN                                 10109000
                            LBRB                                        10110000
                        ELSE                                            10111000
                            SETINSERT;                                  10112000
                IF TYPE!63 THEN                                         10113000
                    SETINSERT;                                          10114000
            END;                                                        10115000
    BOOLEAN PROCEDURE RESWORD(RW);                                      10116000
        VALUE RW;                                                       10117000
        INTEGER RW;                                                     10118000
        BEGIN                                                           10119000
$ INCLUDE 10119051, IF DEBUGGING;                                       10119049
            IF TRACE THEN TRACER(<2>&<1>[TOREF]);                       10119050
            IF STEPI=VOTHERS AND ELB.SUB=RW THEN                        10120000
                BEGIN                                                   10121000
                    SETPACK;                                            10122000
                    RESWORD~ TRUE;                                      10123000
                END                                                     10124000
            ELSE                                                        10125000
                T~ T-1;                                                 10126000
        END RESWORD;                                                    10127000
    BOOLEAN PROCEDURE SPCHAR(SP);                                       10128000
        VALUE SP;                                                       10129000
        INTEGER SP;                                                     10130000
        BEGIN                                                           10131000
$ INCLUDE 10131051, IF DEBUGGING;                                       10131049
            IF TRACE THEN TRACER(<2>&<3>[TOREF]);                       10131050
            IF STEPI=VSPECIAL AND (ELB=SP OR SP=0) THEN                 10132000
                BEGIN                                                   10133000
                    SETINSERT;                                          10134000
                    SPCHAR~ TRUE;                                       10135000
                END                                                     10136000
            ELSE                                                        10137000
                T~ T-1;                                                 10138000
        END SPCHAR;                                                     10139000
    BOOLEAN PROCEDURE LOGICALVALUE;                                     10140000
        BEGIN                                                           10141000
$ INCLUDE 10141051, IF DEBUGGING;                                       10141049
            IF TRACE THEN TRACER(<2>&<5>[TOREF]);                       10141050
            IF STEPI=VTORF THEN                                         10142000
                BEGIN                                                   10143000
                    SETPACK;                                            10144000
                    LOGICALVALUE~ TRUE;                                 10145000
                END                                                     10146000
            ELSE                                                        10147000
                T~ T-1;                                                 10148000
        END LOGICALVALUE;                                               10149000
    BOOLEAN PROCEDURE NUMBER;                                           10150000
        BEGIN                                                           10151000
$ INCLUDE 10151051, IF DEBUGGING;                                       10151049
            IF TRACE THEN TRACER(<2>&<7>[TOREF]);                       10151050
            IF STEPI=VNUMBER THEN                                       10152000
                BEGIN                                                   10153000
                    SETPACK;                                            10154000
                    NUMBER~ TRUE;                                       10155000
                END                                                     10156000
            ELSE                                                        10157000
                T~ T-1;                                                 10158000
        END NUMBER;                                                     10159000
    BOOLEAN PROCEDURE STRINE;                                           10160000
        BEGIN                                                           10161000
$ INCLUDE 10161051, IF DEBUGGING;                                       10161049
            IF TRACE THEN TRACER(<2>&<11>[TOREF]);                      10161050
            IF STEPI=VSTRING THEN                                       10162000
                BEGIN                                                   10163000
                    SETPACK;                                            10164000
                    STRINE~ TRUE;                                       10165000
                END                                                     10166000
            ELSE                                                        10167000
                T~ T-1;                                                 10168000
        END STRINE;                                                     10169000
    PROCEDURE LPRP(LP,RP);                                              10170000
        VALUE LP, RP;                                                   10171000
        ALPHA LP, RP;                                                   10172000
        BEGIN                                                           10173000
$ INCLUDE 10173051, IF DEBUGGING;                                       10173049
            IF TRACE THEN TRACER(<2>&<13>[TOREF]);                      10173050
            SETINSERT;                                                  10174000
            WHILE(STEPI!VSPECIAL OR ELB!RP) AND TYPE!63 DO              10175000
                IF TYPE!VSPECIAL THEN                                   10176000
                    SETPACK                                             10177000
                ELSE                                                    10178000
                    IF ELB=LP THEN                                      10179000
                        LPRP(LP, RP)                                    10180000
                    ELSE                                                10181000
                        SETINSERT;                                      10182000
            IF TYPE!63 THEN                                             10183000
                SETINSERT;                                              10184000
        END;                                                            10185000
    BOOLEAN PROCEDURE PROCEDUREF;                                       10186000
        BEGIN                                                           10187000
$ INCLUDE 10187051, IF DEBUGGING;                                       10187049
            IF TRACE THEN TRACER(<2>&<14>[TOREF]);                      10187050
            IF PROCEDUREF~ STEPI=VALGOLIDENT OR TYPE=VNOTDEC OR         10188000
                TYPE=VOTHERS AND(IF ATOG THEN(I~ELB.SUB=REALV OR        10189000
                ENTIERV{I{ABSV) ELSE ELB.SUB=BOOLEANV) THEN             10190000
                BEGIN                                                   10191000
                    SETPACK;                                            10192000
                    IF STEPI=VSPECIAL AND(ELB="(" OR ELB="[") THEN      10193000
                        LPRP(ELB, IF ELB="(" THEN ")" ELSE "]")         10194000
                    ELSE                                                10195000
                        T~ T-1                                          10196000
                END                                                     10197000
            ELSE                                                        10198000
                T~ T-1                                                  10199000
        END;                                                            10200000
    BOOLEAN PROCEDURE RELATIONOP;                                       10201000
        BEGIN                                                           10202000
$ INCLUDE 10202051, IF DEBUGGING;                                       10202049
            IF TRACE THEN TRACER(<2>&<16>[TOREF]);                      10202050
            IF SPCHAR("=") THEN                                         10203000
                RELATIONOP~ TRUE                                        10204000
            ELSE                                                        10205000
                IF SPCHAR("!") THEN                                     10206000
                    RELATIONOP~ TRUE                                    10207000
                ELSE                                                    10208000
                    IF SPCHAR("<") THEN                                 10209000
                        RELATIONOP~ TRUE                                10210000
                    ELSE                                                10211000
                        IF SPCHAR(">") THEN                             10212000
                            RELATIONOP~ TRUE                            10213000
                        ELSE                                            10214000
                            IF SPCHAR("{") THEN                         10215000
                                RELATIONOP~ TRUE                        10216000
                            ELSE                                        10217000
                                IF SPCHAR("}") THEN                     10218000
                                    RELATIONOP~ TRUE;                   10219000
        END RELATIONOP;                                                 10220000
    PROCEDURE PWORD;                                                    10221000
        BEGIN                                                           10222000
            INTEGER TC, THM;                                            10223000
            LABEL EXIT;                                                 10224000
$ INCLUDE 10224051, IF DEBUGGING;                                       10224049
            IF TRACE THEN TRACER(<2>&<20>[TOREF]);                      10224050
            TC~ T;                                                      10225000
            THM~ HM;                                                    10226000
            IF SPCHAR(".") THEN                                         10227000
                IF SPCHAR("[") THEN                                     10228000
                    IF NUMBER THEN                                      10229000
                        IF SPCHAR(":") THEN                             10230000
                            IF NUMBER THEN                              10231000
                                IF SPCHAR("]") THEN                     10232000
                                    GO EXIT;                            10233000
            T~ TC;                                                      10234000
            HM~ THM;                                                    10235000
EXIT:                                                                   10236000
        END PWORD;                                                      10237000
    BOOLEAN PROCEDURE IFCLAUSE;                                         10250000
        BEGIN                                                           10251000
$ INCLUDE 10251051, IF DEBUGGING;                                       10251049
            IF TRACE THEN TRACER(<2>&<21>[TOREF]);                      10251050
            IF RESWORD(IFV) THEN                                        10252000
                IF BOOLEANEXPRESS THEN                                  10253000
                    IF RESWORD(THENV) THEN                              10254000
                        IFCLAUSE~ TRUE;                                 10255000
        END IFCLAUSE;                                                   10256000
    BOOLEAN PROCEDURE PRIMARY;                                          10257000
        BEGIN                                                           10258000
            INTEGER TC, THM;                                            10259000
            LABEL EXIT, CE;                                             10260000
            BOOLEAN P;                                                  10261000
$ INCLUDE 10261051, IF DEBUGGING;                                       10261049
            IF TRACE THEN TRACER(<2>&<23>[TOREF]);                      10261050
            TC~ T;                                                      10262000
            THM~ HM;                                                    10263000
            IF P~ PROCEDUREF THEN                                       10264000
                BEGIN                                                   10265000
                    TC~ T;                                              10266000
                    THM~ HM;                                            10267000
                    IF SPCHAR("~") THEN                                 10268000
                        IF ARITHEXPRESS THEN                            10269000
                            BEGIN                                       10270000
                                PRIMARY~ TRUE;                          10271000
                                GO EXIT;                                10272000
                            END;                                        10273000
                    T~ TC;                                              10274000
                    HM~ THM;                                            10275000
                    GO CE;                                              10276000
                END;                                                    10277000
            IF NUMBER THEN                                              10278000
                GO CE;                                                  10279000
            IF STRINE THEN                                              10280000
                GO CE;                                                  10281000
            IF SPCHAR("*") THEN                                         10282000
                GO CE;                                                  10283000
            IF SPCHAR("(") THEN                                         10284000
                IF ARITHEXPRESS THEN                                    10285000
                    IF SPCHAR(")") THEN                                 10286000
                        BEGIN                                           10287000
                            P~ TRUE;                                    10288000
                            GO CE                                       10289000
                        END;                                            10290000
            T~ TC;                                                      10291000
            HM~ THM;                                                    10292000
            GO EXIT;                                                    10293000
CE:         TC~ T;                                                      10294000
            THM~ HM;                                                    10295000
            PRIMARY~ TRUE;                                              10296000
            IF SPCHAR("&") THEN                                         10297000
                IF LINKPART(TRUE) THEN                                  10298000
                    GO EXIT;                                            10299000
            T~ TC;                                                      10300000
            HM~ THM;                                                    10301000
            IF P THEN                                                   10302000
                PWORD;                                                  10303000
EXIT:                                                                   10304000
        END PRIMARY;                                                    10305000
    BOOLEAN PROCEDURE FACTOR;                                           10306000
        BEGIN                                                           10307000
            INTEGER TC, THM;                                            10308000
            LABEL AGAIN, EXIT;                                          10309000
$ INCLUDE 10309051, IF DEBUGGING;                                       10309049
            IF TRACE THEN TRACER(<2>&<25>[TOREF]);                      10309050
            TC~ T;                                                      10310000
            THM~ HM;                                                    10311000
AGAIN:      IF NOT FACTOR~ PRIMARY THEN                                 10312000
                BEGIN                                                   10313000
                    T~ TC;                                              10314000
                    HM~ THM;                                            10315000
                    GO EXIT                                             10316000
                END;                                                    10317000
            IF SPCHAR("*") THEN                                         10318000
                GO AGAIN;                                               10319000
EXIT:                                                                   10320000
        END FACTOR;                                                     10321000
    BOOLEAN PROCEDURE TERM;                                             10322000
        BEGIN                                                           10323000
            INTEGER TC, THM;                                            10324000
            LABEL AGAIN, EXIT;                                          10325000
$ INCLUDE 10325051, IF DEBUGGING;                                       10325049
            IF TRACE THEN TRACER(<2>&<27>[TOREF]);                      10325050
            TC~ T;                                                      10326000
            THM~ HM;                                                    10327000
AGAIN:      IF NOT TERM~ FACTOR THEN                                    10328000
                BEGIN                                                   10329000
                    T~ TC;                                              10330000
                    HM~ THM;                                            10331000
                    GO EXIT                                             10332000
                END;                                                    10333000
            IF SPCHAR("|") THEN                                         10334000
                GO AGAIN;                                               10335000
            IF SPCHAR("/") THEN                                         10336000
                GO AGAIN;                                               10337000
            IF RESWORD(MODV) THEN                                       10338000
                GO AGAIN;                                               10339000
            IF RESWORD(DIVV) THEN                                       10340000
                GO AGAIN;                                               10341000
EXIT:                                                                   10342000
        END TERM;                                                       10343000
    BOOLEAN PROCEDURE SIMPLEARITHEXPRESS;                               10344000
        BEGIN                                                           10345000
            INTEGER TC, THM;                                            10346000
            LABEL AGAIN, EXIT;                                          10347000
$ INCLUDE 10347051, IF DEBUGGING;                                       10347049
            IF TRACE THEN TRACER(<2>&<30>[TOREF]);                      10347050
            TC~ T;                                                      10348000
            THM~ HM;                                                    10349000
            IF SPCHAR("+") THEN                                         10350000
                GO AGAIN;                                               10351000
            IF SPCHAR("-") THEN                                         10352000
                GO AGAIN;                                               10353000
AGAIN:      IF NOT SIMPLEARITHEXPRESS~ TERM THEN                        10354000
                BEGIN                                                   10355000
                    T~ TC;                                              10356000
                    HM~ THM;                                            10357000
                    GO EXIT                                             10358000
                END;                                                    10359000
            IF SPCHAR("+") THEN                                         10360000
                GO AGAIN;                                               10361000
            IF SPCHAR("-") THEN                                         10362000
                GO AGAIN;                                               10363000
EXIT:                                                                   10364000
        END SIMPLEARITHEXPRESS;                                         10365000
    BOOLEAN PROCEDURE ARITHEXPRESS;                                     10366000
        BEGIN                                                           10367000
            INTEGER TC, THM;                                            10368000
            LABEL EXIT;                                                 10369000
$ INCLUDE 10369051, IF DEBUGGING;                                       10369049
            IF TRACE THEN TRACER(<2>&<33>[TOREF]);                      10369050
            TC~ T;                                                      10370000
            THM~ HM;                                                    10371000
            IF IFCLAUSE THEN                                            10372000
                IF ARITHEXPRESS THEN                                    10373000
                    IF RESWORD(ELSEV) THEN                              10374000
                        IF ARITHEXPRESS THEN                            10375000
                            BEGIN                                       10376000
                                ARITHEXPRESS~ TRUE;                     10377000
                                GO EXIT                                 10378000
                            END;                                        10379000
            T~ TC;                                                      10380000
            HM~ THM;                                                    10381000
            IF SIMPLEARITHEXPRESS THEN                                  10382000
                BEGIN                                                   10383000
                    ARITHEXPRESS~ TRUE;                                 10384000
                    GO EXIT                                             10385000
                END;                                                    10386000
            T~ TC;                                                      10387000
            HM~ THM;                                                    10388000
EXIT:                                                                   10389000
        END ARITHEXPRESS;                                               10390000
    BOOLEAN PROCEDURE RELATION;                                         10400000
        BEGIN                                                           10401000
            INTEGER TC, THM;                                            10402000
            LABEL AGAIN, EXIT;                                          10403000
$ INCLUDE 10403051, IF DEBUGGING;                                       10403049
            IF TRACE THEN TRACER(<2>&<35>[TOREF]);                      10403050
            TC~ T;                                                      10404000
            THM~ HM;                                                    10405000
            IF ARITHEXPRESS THEN                                        10406000
                IF RELATIONOP THEN                                      10407000
AGAIN:              IF ARITHEXPRESS THEN                                10408000
                        BEGIN                                           10409000
                            IF RELATIONOP THEN                          10410000
                                GO AGAIN;                               10411000
                            RELATION~ TRUE;                             10412000
                            GO EXIT;                                    10413000
                        END;                                            10414000
            T~ TC;                                                      10415000
            HM~ THM;                                                    10416000
EXIT:                                                                   10417000
        END RELATION;                                                   10418000
    BOOLEAN PROCEDURE BPRIMARY;                                         10419000
        BEGIN                                                           10420000
            INTEGER TC, THM;                                            10421000
            LABEL EXIT, CE;                                             10422000
            BOOLEAN P;                                                  10423000
$ INCLUDE 10423051, IF DEBUGGING;                                       10423049
            IF TRACE THEN TRACER(<2>&<37>[TOREF]);                      10423050
            TC~ T;                                                      10424000
            THM~ HM;                                                    10425000
            TEMP~ REAL(RESWORD(NOTV));                                  10426000
            IF RELATION THEN                                            10427000
                GO CE;                                                  10428000
            IF P~ LOGICALVALUE THEN                                     10429000
                GO CE;                                                  10430000
            IF P~ PROCEDUREF THEN                                       10431000
                BEGIN                                                   10432000
                    TC~ T;                                              10433000
                    THM~ HM;                                            10434000
                    IF SPCHAR("~") THEN                                 10435000
                        IF BOOLEANEXPRESS THEN                          10436000
                            BEGIN                                       10437000
                                BPRIMARY~ TRUE;                         10438000
                                GO EXIT;                                10439000
                            END;                                        10440000
                    T~ TC;                                              10441000
                    HM~ THM;                                            10442000
                    GO CE;                                              10443000
                END;                                                    10444000
            IF SPCHAR("(") THEN                                         10445000
                IF BOOLEANEXPRESS THEN                                  10446000
                    IF SPCHAR(")") THEN                                 10447000
                        BEGIN                                           10448000
                            P~ TRUE;                                    10449000
                            GO CE;                                      10450000
                        END;                                            10451000
            T~ TC;                                                      10452000
            HM~ THM;                                                    10453000
            GO EXIT;                                                    10454000
CE:         TC~ T;                                                      10455000
            THM~ HM;                                                    10456000
            BPRIMARY~ TRUE;                                             10457000
            IF SPCHAR("&") THEN                                         10458000
                IF LINKPART(FALSE) THEN                                 10459000
                    GO EXIT;                                            10460000
            T~ TC;                                                      10461000
            HM~ THM;                                                    10462000
            IF P THEN                                                   10463000
                PWORD;                                                  10464000
EXIT:                                                                   10465000
        END BPRIMARY;                                                   10466000
    BOOLEAN PROCEDURE BFACTOR;                                          10467000
        BEGIN                                                           10468000
            INTEGER TC, THM;                                            10469000
            LABEL AGAIN, EXIT;                                          10470000
$ INCLUDE 10470051, IF DEBUGGING;                                       10470049
            IF TRACE THEN TRACER(<2>&<41>[TOREF]);                      10470050
            TC~ T;                                                      10471000
            THM~ HM;                                                    10472000
AGAIN:      IF NOT BFACTOR~ BPRIMARY THEN                               10473000
                BEGIN                                                   10474000
                    T~ TC;                                              10475000
                    HM~ THM;                                            10476000
                    GO EXIT                                             10477000
                END;                                                    10478000
            IF RESWORD(ANDV) THEN                                       10479000
                GO AGAIN;                                               10480000
EXIT:                                                                   10481000
        END BFACTOR;                                                    10482000
    BOOLEAN PROCEDURE BTERM;                                            10483000
        BEGIN                                                           10484000
            INTEGER TC, THM;                                            10485000
            LABEL AGAIN, EXIT;                                          10486000
$ INCLUDE 10486051, IF DEBUGGING;                                       10486049
            IF TRACE THEN TRACER(<2>&<43>[TOREF]);                      10486050
            TC~ T;                                                      10487000
            THM~ HM;                                                    10488000
AGAIN:      IF NOT BTERM~ BFACTOR THEN                                  10489000
                BEGIN                                                   10490000
                    T~ TC;                                              10491000
                    HM~ THM;                                            10492000
                    GO EXIT                                             10493000
                END;                                                    10494000
            IF RESWORD(ORV) THEN                                        10495000
                GO AGAIN;                                               10496000
EXIT:                                                                   10497000
        END BTERM;                                                      10498000
    BOOLEAN PROCEDURE IMPLICATION;                                      10499000
        BEGIN                                                           10500000
            INTEGER TC, THM;                                            10501000
            LABEL AGAIN, EXIT;                                          10502000
$ INCLUDE 10502051, IF DEBUGGING;                                       10502049
            IF TRACE THEN TRACER(<2>&<44>[TOREF]);                      10502050
            TC~ T;                                                      10503000
            THM~ HM;                                                    10504000
AGAIN:      IF NOT IMPLICATION~ BTERM THEN                              10505000
                BEGIN                                                   10506000
                    T~ TC;                                              10507000
                    HM~ THM;                                            10508000
                    GO EXIT                                             10509000
                END;                                                    10510000
            IF RESWORD(IMPV) THEN                                       10511000
                GO AGAIN;                                               10512000
EXIT:                                                                   10513000
        END IMPLICATION;                                                10514000
    BOOLEAN PROCEDURE SIMPLEBOOLEAN;                                    10515000
        BEGIN                                                           10516000
            INTEGER TC, THM;                                            10517000
            LABEL AGAIN, EXIT;                                          10518000
$ INCLUDE 10518051, IF DEBUGGING;                                       10518049
            IF TRACE THEN TRACER(<2>&<46>[TOREF]);                      10518050
            TC~ T;                                                      10519000
            THM~ HM;                                                    10520000
AGAIN:      IF NOT SIMPLEBOOLEAN~ IMPLICATION THEN                      10521000
                BEGIN                                                   10522000
                    T~ TC;                                              10523000
                    HM~ THM;                                            10524000
                    GO EXIT                                             10525000
                END;                                                    10526000
            IF RESWORD(EQVV) THEN                                       10527000
                GO AGAIN;                                               10528000
EXIT:                                                                   10529000
        END SIMPLEBOOLEAN;                                              10530000
    BOOLEAN PROCEDURE BOOLEANEXPRESS;                                   10531000
        BEGIN                                                           10532000
            INTEGER TC, THM;                                            10533000
            LABEL EXIT;                                                 10534000
$ INCLUDE 10534051, IF DEBUGGING;                                       10534049
            IF TRACE THEN TRACER(<2>&<50>[TOREF]);                      10534050
            TC~ T;                                                      10535000
            THM~ HM;                                                    10536000
            IF IFCLAUSE THEN                                            10537000
                IF BOOLEANEXPRESS THEN                                  10538000
                    IF RESWORD(ELSEV) THEN                              10539000
                        IF BOOLEANEXPRESS THEN                          10540000
                            BEGIN                                       10541000
                                BOOLEANEXPRESS~ TRUE;                   10542000
                                GO EXIT                                 10543000
                            END;                                        10544000
            T~ TC;                                                      10545000
            HM~ THM;                                                    10546000
            IF SIMPLEBOOLEAN THEN                                       10547000
                BEGIN                                                   10548000
                    BOOLEANEXPRESS~ TRUE;                               10549000
                    GO EXIT                                             10550000
                END;                                                    10551000
            T~ TC;                                                      10552000
            HM~ THM;                                                    10553000
EXIT:                                                                   10554000
        END BOOLEANEXPRESS;                                             10555000
    BOOLEAN PROCEDURE GPRIMARY(ATOG); VALUE ATOG; BOOLEAN ATOG;         10600000
        BEGIN                                                           10601000
            INTEGER TC, THM;                                            10602000
$ INCLUDE 10602051, IF DEBUGGING;                                       10602049
            IF TRACE THEN TRACER(<2>&<53>[TOREF]);                      10602050
            TC~ T;                                                      10603000
            THM~ HM;                                                    10604000
            IF ATOG THEN                                                10605000
                IF PRIMARY THEN                                         10606000
                    GPRIMARY~ TRUE                                      10607000
                ELSE                                                    10608000
                    BEGIN                                               10609000
                        T~ TC;                                          10610000
                        HM~ THM;                                        10611000
                    END                                                 10612000
            ELSE                                                        10613000
                IF BPRIMARY THEN                                        10614000
                    GPRIMARY~ TRUE                                      10615000
                ELSE                                                    10616000
                    BEGIN                                               10617000
                        T~ TC;                                          10618000
                        HM~ THM;                                        10619000
                    END;                                                10620000
        END GPRIMARY;                                                   10621000
    BOOLEAN PROCEDURE LINKDESCRIP;                                      10622000
        BEGIN                                                           10623000
            INTEGER TC, THM;                                            10624000
            LABEL EXIT;                                                 10625000
$ INCLUDE 10625051, IF DEBUGGING;                                       10625049
            IF TRACE THEN TRACER(<2>&<55>[TOREF]);                      10625050
            TC~ T;                                                      10626000
            THM~ HM;                                                    10627000
            IF TABLE(T)=VSPECIAL AND ELB="]" THEN                       10628000
                IF TABLE(T-1)=VNUMBER THEN                              10629000
                    IF TABLE(T-2)=VSPECIAL AND ELB=":" THEN             10630000
                        BEGIN                                           10631000
                            LINKDESCRIP~ TRUE;                          10632000
                            GO EXIT                                     10633000
                        END;                                            10634000
            IF SPCHAR("[") THEN                                         10635000
                IF NUMBER THEN                                          10636000
                    IF SPCHAR(":") THEN                                 10637000
                        IF NUMBER THEN                                  10638000
                            IF SPCHAR(":") THEN                         10639000
                                IF NUMBER THEN                          10640000
                                    IF SPCHAR("]") THEN                 10641000
                                        BEGIN                           10642000
                                            LINKDESCRIP~ TRUE;          10643000
                                            GO EXIT                     10644000
                                        END;                            10645000
            T~ TC;                                                      10646000
            HM~ THM;                                                    10647000
EXIT:                                                                   10648000
        END LINKDESCRIP;                                                10649000
    BOOLEAN PROCEDURE LINKPART(P); VALUE P; BOOLEAN P;                  10650000
        BEGIN                                                           10651000
            INTEGER TC, THM;                                            10652000
            LABEL AGAIN, EXIT;                                          10653000
$ INCLUDE 10653051, IF DEBUGGING;                                       10653049
            IF TRACE THEN TRACER(<2>&<57>[TOREF]);                      10653050
            TC~ T;                                                      10654000
            THM~ HM;                                                    10655000
AGAIN:      IF LINKPART~GPRIMARY(P) THEN                                10656000
                BEGIN                                                   10657000
                    IF NOT LINKPART~ LINKDESCRIP THEN                   10658000
                        BEGIN                                           10659000
                            T~ TC;                                      10660000
                            HM~ THM;                                    10661000
                            GO TO EXIT                                  10662000
                        END;                                            10663000
                    IF SPCHAR("&") THEN                                 10664000
                        GO TO AGAIN                                     10665000
                END;                                                    10666000
EXIT:                                                                   10667000
        END LINKPART;                                                   10668000
    BOOLEAN PROCEDURE EXPRESS(ATOG); VALUE ATOG; BOOLEAN ATOG;          10669000
    BEGIN                                                               10670000
        INTEGER TC, THM;                                                10671000
$ INCLUDE 10671051, IF DEBUGGING;                                       10671049
            IF TRACE THEN TRACER(<2>&<61>[TOREF]);                      10671050
        THM~ HM;                                                        10672000
        TC~ T;                                                          10673000
        IF ATOG THEN                                                    10674000
            IF NOT EXPRESS ~ ARITHEXPRESS THEN                          10675000
                BEGIN                                                   10676000
                    T~ TC;                                              10677000
                    HM~ THM                                             10678000
                END                                                     10679000
            ELSE                                                        10680000
        ELSE                                                            10681000
            IF NOT EXPRESS ~ BOOLEANEXPRESS THEN                        10682000
                BEGIN                                                   10683000
                    T~ TC;                                              10684000
                    HM~ THM                                             10685000
                END;                                                    10686000
    END EXPRESS;                                                        10687000
    BOOLEAN PROCEDURE STATE;                                            10700000
    BEGIN                                                               10701000
        BOOLEAN S,TOGGLE;                                               10702000
        INTEGER THENELS, BEGEND, DOUNT, FORDO;                          10703000
        LABEL CALTAB,CLTB,FO,THE,D,UNTI,ELS,EN,TEST,FINI,TGET,BEGI;     10704000
        SWITCH WHICHWAY ~ BEGI,FO,THE,D,UNTI,ELS,EN;                    10705000
$ INCLUDE 10705051, IF DEBUGGING;                                       10705049
            IF TRACE THEN TRACER(<2>&<63>[TOREF]);                      10705050
        IF STEPI=VSPECIAL THEN                                          10706000
            BEGIN                                                       10707000
                T~ T-1;                                                 10708000
                TOGGLE~ FALSE;                                          10709000
                GO FINI;                                                10710000
            END;                                                        10711000
TEST:   IF TYPE!VOTHERS THEN                                            10712000
            GO CALTAB;                                                  10713000
        GO WHICHWAY[ELB.SUB+1-BEGINV];                                  10714000
        GO CALTAB;                                                      10715000
BEGI:   BEGEND~ BEGEND+1;                                               10716000
        GO CALTAB;                                                      10717000
THE:    THENELS~ THENELS+1;                                             10718000
        GO CALTAB;                                                      10719000
D:      IF FORDO=0 THEN                                                 10720000
            DOUNT~ DOUNT+1                                              10721000
        ELSE                                                            10722000
            FORDO~ FORDO-1;                                             10723000
        GO CALTAB;                                                      10724000
FO:     FORDO~ FORDO+1;                                                 10725000
        GO CALTAB;                                                      10726000
ELS:    IF THENELS=0 THEN                                               10727000
            GO TGET;                                                    10728000
        THENELS~ THENELS-1;                                             10729000
        GO CALTAB;                                                      10730000
EN:     IF BEGEND=0 THEN                                                10731000
            GO TGET;                                                    10732000
        BEGEND~ BEGEND-1;                                               10733000
        GO CALTAB;                                                      10734000
UNTI:   IF S~ FORDO=0 THEN                                              10735000
            IF BEGEND=0 THEN                                            10736000
                IF DOUNT=0 THEN                                         10737000
                    GO TGET;                                            10738000
        IF NOT S THEN                                                   10739000
            DOUNT~ DOUNT-1;                                             10740000
CALTAB: IF TYPE=63 THEN                                                 10741000
            BEGIN                                                       10742000
                T~ T-1;                                                 10743000
                GO FINI                                                 10744000
            END;                                                        10745000
        SETPACK;                                                        10746000
CLTB:   IF STEPI!VSPECIAL THEN                                          10747000
            GO TEST;                                                    10748000
        IF ELB!";" OR BEGEND!0 THEN                                     10749000
            BEGIN                                                       10750000
                SETINSERT;                                              10751000
                GO CLTB;                                                10752000
            END;                                                        10753000
TGET:   TOGGLE~ TRUE;                                                   10754000
        T~ T-1;                                                         10755000
FINI: STATE ~ TOGGLE;                                                   10756000
    END STATE;                                                          10757000
    BOOLEAN PROCEDURE INTGER;                                           10800000
        BEGIN                                                           10801000
            INTEGER TC,THM;                                             10802000
            LABEL EXIT;                                                 10803000
$ INCLUDE 10803051, IF DEBUGGING;                                       10803049
            IF TRACE THEN TRACER(<2>&<64>[TOREF]);                      10803050
            TC ~ T;                                                     10804000
            THM ~ HM;                                                   10805000
            IF NUMBER THEN                                              10806000
                IF ELB.SUB = INTEGERV THEN BEGIN                        10807000
                    INTGER ~ TRUE;                                      10808000
                    GO EXIT;                                            10809000
                END;                                                    10810000
            T ~ TC;                                                     10811000
            HM ~ THM;                                                   10812000
EXIT:   END INTGER;                                                     10813000
                                                                        10814000
    BOOLEAN PROCEDURE IDENTIFIER;                                       10815000
        BEGIN                                                           10815100
$ INCLUDE 10815151, IF DEBUGGING;                                       10815149
            IF TRACE THEN TRACER(<2>&<66>[TOREF]);                      10815150
            IF VNOTDEC { STEPI { VOTHERS THEN BEGIN                     10816000
                IDENTIFIER ~ TRUE;                                      10817000
                SETPACK;                                                10818000
            END ELSE                                                    10819000
                T ~ T - 1;                                              10820000
        END;                                                            10820050
    BOOLEAN PROCEDURE DECLARASHUN;                                      10821000
        BEGIN                                                           10821100
$ INCLUDE 10821151, IF DEBUGGING;                                       10821149
            IF TRACE THEN TRACER(<2>&<70>[TOREF]);                      10821150
            IF STEPI = VDECLRN THEN BEGIN                               10822000
                DECLARASHUN ~ TRUE;                                     10823000
                SETPACK;                                                10824000
            END ELSE                                                    10825000
                T ~ T - 1;                                              10826000
        END;                                                            10826050
    BOOLEAN PROCEDURE ELEMENT;                                          10827000
        BEGIN                                                           10827100
$ INCLUDE 10827151, IF DEBUGGING;                                       10827149
            IF TRACE THEN TRACER(<2>&<72>[TOREF]);                      10827150
            IF ELEMENT ~ (STEPI ! 63) THEN                              10828000
                IF TYPE = VSPECIAL THEN                                 10829000
                    SETINSERT                                           10830000
                ELSE                                                    10831000
                    SETPACK                                             10832000
            ELSE                                                        10833000
                T ~ T - 1;                                              10834000
        END;                                                            10834050
    BOOLEAN PROCEDURE VARIABLE;                                         10835000
        BEGIN                                                           10835100
$ INCLUDE 10835151, IF DEBUGGING;                                       10835149
            IF TRACE THEN TRACER(<2>&<74>[TOREF]);                      10835150
            IF VARIABLE ~ (STEPI = VALGOLIDENT OR TYPE = VNOTDEC)       10836000
            THEN BEGIN                                                  10837000
                SETPACK;                                                10838000
                IF STEPI = VSPECIAL AND ELB = "[" THEN                  10839000
                    LBRB                                                10840000
                ELSE                                                    10841000
                    T ~ T - 1;                                          10842000
            END ELSE                                                    10843000
                T ~ T - 1;                                              10844000
        END;                                                            10844050
$ INCLUDE 10961001, IF ADVANCED;                                        10899999
        COMMENT  LINKIN PROCESSES RESERVE & RESERVESTMT REQUESTS.       10900000
    PATCHSPACE IS SET UP & A FUNNY 4-CHR ID IS PLACED IN ACCM TO        10901000
    BE OUTPUT. PATCH RESERVATIONS GO FROM 0->266 IN STEPS OF 2.         10902000
    AN OFFSET OF 500 IDENTIFIES STMT RESERVATION. ACTUAL                10903000
    ASSIGNMENT OF LINK RECORD IS DELAYED UNTIL FUNNY ID REACHES         10904000
    OUTER BLOCK & OUTPUT STRING;                                        10905000
                                                                        10906000
PROCEDURE LINKIN(CTL,W,SIZE,OFFSET);                                    10907000
VALUE SIZE,OFFSET,W;                                                    10908000
REAL W,SIZE,OFFSET; ARRAY CTL[0];                                       10909000
BEGIN                                                                   10910000
    LABEL EXIT;                                                         10911000
    IF NXTPTCHREC+SIZE}PATCHMAX OR NXTPTCHX}PTCHMAX THEN BEGIN          10912000
        ERROR(65);                                                      10913000
        GO EXIT;                                                        10914000
    END;                                                                10915000
    MOVEDEC(OFFSET+NXTPTCHX~*+2,ACCM,4,5,3,0);                          10916000
    ACCM[0].C23 ~ 12;                                                   10916500
    HM ~ 7;                                                             10917000
    CTL[W] ~ NXTPTCHX&1[TOPLINK];                                       10918000
    PATCHSPACE[NXTPTCHX]~ NXTPTCHREC;                                   10919000
    PATCHSPACE[NXTPTCHX+1] ~ -0&SIZE[TOPSIZE];                          10920000
    NXTPTCHREC ~ NXTPTCHREC + SIZE;                                     10921000
EXIT:                                                                   10922000
END LINKIN;                                                             10923000
        COMMENT  LINKOUT PROCESSES ACTUAL PATCH STMTS. SEQUENCE         10924000
    IN ACCM IS OUTPUT IN 72-CHR HUNKS TO RESERVED RECORDS IN THE        10925000
    FILE BACKPATCHES. ITEMS ARE NOT RESCANNED, ALTERED, PACKED OR       10926000
    NOTICED IN ANY WAY. IN PARTICULAR -DICT- DOES NOT USUALLY KNOW      10927000
    ABOUT DECLARED IDS HIDDEN IN PATCHES;                               10928000
PROCEDURE LINKOUT(CTL,W); VALUE W;                                      10929000
    ARRAY CTL[0]; INTEGER W;                                            10930000
BEGIN                                                                   10931000
    LABEL EXIT;                                                         10932000
    REAL A,B,C,D,I,J;                                                   10933000
  FORMAT PATCH("PATCH IMAGE #"I3" OF"I3" RESERVED AT "I9" IN CODE");    10933050
                                                                        10934000
    IF NOT BOOLEAN(C~CTL[W]).PLINK THEN BEGIN                           10935000
        ERROR(67);                                                      10936000
        GO EXIT;                                                        10937000
    END;                                                                10938000
    B ~ (J~PATCHSPACE[C]).PBASE; C ~ PATCHSPACE[C+1];                   10939000
    D ~ C.PSIZE;                                                        10940000
    IF INTERLIST THEN                                                   10940050
        WRITE(PRINTER,PATCH,C.PUSED,D,J.PSEQN);                         10940100
    A ~ HM-3;                                                           10941000
    I ~ 3;                                                              10942000
    IF A>0 THEN BEGIN                                                   10943000
        WHILE A}72 DO BEGIN                                             10944000
            BLANK(PPB,0,14);                                            10945000
            MOV(1,8,ACCM[I.WRD],I.CHR,PPB,0);                           10946000
            A ~ A - 72;                                                 10947000
            I ~ I + 72;                                                 10948000
            WRITE(BACKPATCHES[B+C.PUSED],9,PPB[*]);                     10949000
$ INCLUDE 10949101, IF COUNTING;                                        10949099
            PTCHCT ~ *+1;                                               10949100
            C.PUSED ~ C.PUSED + 1; IF C.PUSED > D THEN BEGIN            10950000
                ERROR(65);                                              10951000
                GO EXIT;                                                10952000
            END;                                                        10953000
        END; BLANK(PPB,0,14);                                           10954000
        MOV(A.C6,A,ACCM[I.WRD],I.CHR,PPB,0);                            10955000
        WRITE(BACKPATCHES[B+C.PUSED],9,PPB[*]);                         10956000
$ INCLUDE 10956101, IF COUNTING;                                        10956099
        PTCHCT ~ *+1;                                                   10956100
        BLANK(PPB,0,14);                                                10957000
        C.PUSED ~ C.PUSED + 1; IF C.PUSED > D THEN                      10958000
            ERROR(65); END;                                             10959000
EXIT:   PATCHSPACE[CTL[W]+1] ~ C;                                       10960000
END LINKOUT;                                                            10961000
$ INCLUDE 10980001, IF DEBUGGING;                                       10961999
        COMMENT TRACER TRACES ENTRY & EXIT OF MACROS                    10962000
    AND THE PROCESS OF TESTING SCAN TYPES;                              10963000
PROCEDURE TRACER(T); VALUE T; BOOLEAN T;                                10964000
BEGIN                                                                   10965000
    ARRAY ERB[0:14];                                                    10966000
    STREAM PROCEDURE TRACEIT(D,T,N,L,I); VALUE L,I,T;                   10967000
    BEGIN DI~D; DS~8 LIT " "; SI~D; DS~14 WDS; DI~D;                    10968000
        DS~LIT "."; I(DS~LIT ".");                                      10969000
        SI~LOC T; SI~SI+2; DS~6 CHR;                                    10970000
        SI~N; SI~SI+3; DS~L CHR; END;                                   10971000
    IF T.[46:1] THEN                                                    10972000
        TRACEIT(ERB," SCAN ",TYPENAME[I~REAL(T).REF],                   10973000
            TYPENAME[I].CTR,INDENT)                                     10974000
    ELSE                                                                10975000
        TRACEIT(ERB,IF T THEN "ENTER " ELSE "LEAVE ",                   10976000
            DICT[MADDR.IR,MADDR.IC],IF I~GET(MADDR).CTR > 50            10977000
            THEN 50 ELSE I,INDENT);                                     10978000
    WRITE(PRINTER,15,ERB[*]);                                           10979000
END;                                                                    10980000
% MACRON PROCEDURE BODY                                                 11000000
    BEGIN                                                               11001000
        INTEGER POS,TMADDR,ADDR,OP,X,D,THM,TOHM,                        11002000
          TFIELD,LNGTH,IADDR,TEMP;                                      11003000
        BOOLEAN LOADING;                                                11004000
        BOOLEAN TTOG, TATOG;                                            11005000
                                                                        11005998
        IF MACT } 1 THEN BEGIN % STACK LAST MACRO                       11026000
            X ~ D ~ 0;                                                  11027000
            IF OHM > 3 THEN BEGIN                                       11028000
                STACKIT(X~(OHM+7).WRD,OA[*]);                           11029000
            END;                                                        11029500
            IF HM > 3 THEN BEGIN                                        11030000
                STACKIT(D~(HM+7).WRD,ACCM[*]);                          11031000
            END;                                                        11031500
            LOCAL[0,253] ~ X & D[TOREF]; X ~ D ~ 0;                     11032000
            IF X~LOCAL[4,0].CTR ! 0 THEN BEGIN                          11033000
                STACKIT(X~(X+10).WRD,LOCAL[4,*]);                       11034000
            END;                                                        11034500
            IF D~LOCAL[3,0].CTR ! 0 THEN BEGIN                          11035000
                STACKIT(D~(D+10).WRD,LOCAL[3,*]);                       11036000
            END;                                                        11036500
            LOCAL[0,252] ~ X & D[TOREF];                                11037000
            IF X~LOCAL[0,254].MLOCT ! 0 THEN BEGIN                      11038000
                STACKIT(X,LOCAL[6,*]);                                  11039000
                LOCAL[6,0] ~ 0;                                         11040000
                MOVE(X-2,LOCAL[6,0],LOCAL[6,1]);                        11041000
            END;                                                        11042000
            X ~ (LOCAL[0,255].MLACT+3).D4;                              11043000
            FOR D~7 STEP 1 UNTIL X DO BEGIN                             11044000
                STACKIT(256,LOCAL[D,*]);                                11045000
            END;                                                        11045500
            STACKIT(256,LOCAL[0,*]);                                    11046000
            LOCAL[0,0] ~ 0;                                             11047000
            MOVE(LOCAL[0,255].MLICT-2,LOCAL[0,0],LOCAL[0,1]);           11048000
        END;                                                            11049000
        LOCAL[0,255] ~ A ~ GET(Q);                                      11050000
        POS ~ (X ~ LOCAL[0,254] ~ GET(Q+1))&0[TOREF];                   11051000
        TMADDR~ MADDR;                                                  11052000
        EP~ 0;                                                          11053000
        ACCM[0]~ "     ";                                               11054000
        PACKLAST ~ FALSE;                                               11054100
$ INCLUDE 11056001, IF DEBUGGING;                                       11054999
        IF MONITER OR ACMLST THEN                                       11055000
            BLANK(ACCM[1],0,13);                                        11056000
        MADDR~ QQ+1;                                                    11057000
        INDENT ~ INDENT + 3;                                            11058000
$ INCLUDE 11059001, IF DEBUGGING;                                       11058999
        IF TRACE THEN TRACER(TRUE);                                     11059000
        MACT~ MACT+1;                                                   11060000
        SAVEIT;                                                         11061000
        TTOG~ TOGGLE;                                                   11062000
        TOGGLE~ FALSE;                                                  11063000
        TATOG~ ATOG;                                                    11064000
        ATOG~ FALSE;                                                    11065000
        TFIELD ~ FIELD;                                                 11066000
        FIELD ~ 0;                                                      11067000
        THM ~ HM;                                                       11068000
        TOHM ~ OHM;                                                     11069000
        OHM ~ HM ~ 3;                                                   11069050
        READIT(LOCAL[2,*],A.MML,POS,MCODE);                             11070000
        READIT(LOCAL[1,*],A.MPOOL,POS,MCODE);                           11071000
        READIT(SPACEMAP,X.MSMAP,POS,MCODE);                             11072000
        X ~ A.MLACT;                                                    11072050
        FOR OP~28 STEP 1 UNTIL X DO                                     11072100
            IF INUSE(SPACEMAP[OP.D32],OP.M32) THEN                      11072150
                LOCAL[OP.D4,OP.M4|64] ~ 0;                              11072200
        IF X~A.MLICT-1 > 0 THEN BEGIN                                   11072250
            LOCAL[0,0] ~ 0;                                             11072300
            MOVE(X,LOCAL[0,0],LOCAL[0,1]);                              11072350
        END;                                                            11072400
        IF X~LOCAL[0,254].MLOCT > 0 THEN BEGIN                          11072450
            LOCAL[6,0] ~ 0;                                             11072500
            IF X~X-1 > 0 THEN MOVE(X,LOCAL[6,0],LOCAL[6,1]);            11072550
        END;                                                            11072600
        X ~ 0;                                                          11073000
$ INCLUDE 11076001, IF INTERMEDIATE;                                    11073999
        IF A.MLACT } 28 THEN                                            11074000
            IF W.SUB!0 THEN % LIST MACRO...NEEDS CALLING NAME           11075000
                OP ~ MOVER(IDENT[1].CTR+3,IDENT,8,LOCAL[7,*],0,0);      11076000
        BEGIN                                                           11077000
        COMMENT  ATOI CONVERTS NUMBERS IN ACCM TO AN INTEGER FORM;      11078000
INTEGER PROCEDURE ATOI;                                                 11079000
    ATOI ~ CONSTANTCONVERT(ACCM,IF A~HM-3-REAL(A~CR(ACCM,3)="+" OR      11080000
        A="-") > 8 THEN 8 ELSE A);                                      11081000
        COMMENT  COPE COPES WITH OPERATORS REQUIRING ACCESS TO ONLY     11200000
    ONE MSTRING OR MPOINTER AT A TIME. ALPHA OPERAND CALLS,             11201000
    COMPARISONS & ADDRESSED INTEGER ARITHMETIC OCCUR HERE, AMONG        11202000
    OTHER GOODIES. CHARACTERISTIC FIELDS IN THE MNEMON[OP] ENTRY        11203000
    GUIDE PROCESSING;                                                   11204000
PROCEDURE COPE(ARY,W,OP,SZ); VALUE W,OP,SZ;                             11205000
    INTEGER W,OP,SZ; ARRAY ARY[0];                                      11206000
    BEGIN                                                               11207000
        ALPHA A,B;                                                      11208000
        INTEGER C,LNGTH,N;                                              11209000
$ INCLUDE 11209101, IF DEBUGGING;                                       11209099
        FORMAT CM1("CLOSE PSEUDOREADER;"I4" CHR:");                     11209100
        ALPHA PROCEDURE ADJUST(A); VALUE A; ALPHA A;                    11210000
            ADJUST ~ IF A = "5?0001" THEN "5BEGIN" ELSE                 11211000
                     IF A = "5?0000" THEN "3END  " ELSE                 11212000
                     A;                                                 11213000
        LABEL OPDC,NSTR;                                                11214000
                                                                        11215000
        IF BOOLEAN((C~MNEMON[OP])).SCMP THEN BEGIN                      11216000
            IF C~(A~ADJUST(ACCM[0]&(HM-3)[TOCTR])).CTR <                11217000
               N~(B~ADJUST(ARY[W])).CTR THEN                            11218000
                N ~ 0                                                   11219000
            ELSE                                                        11220000
            IF C = N THEN                                               11221000
                N ~ 1                                                   11222000
            ELSE BEGIN                                                  11223000
                C ~ N;                                                  11224000
                N ~ 2;                                                  11225000
            END;                                                        11226000
            IF C > 5 THEN                                               11227000
                C ~ COMPARE(ACCM[0],ARY[W],C.C5,C.C6,C)                 11228000
            ELSE                                                        11229000
                C ~ COMPARE(A,B,0,0,C);                                 11230000
        END ELSE                                                        11231000
        IF BOOLEAN(C).MINT THEN BEGIN                                   11232000
            IF CONVERTTOG THEN BEGIN                                    11233000
                C ~ CONSTANTCONVERT(ARY[W],(A~ARY[W]).CTR               11234000
                  - REAL(C~CR(A,3)="+" OR C="-"));                      11235000
                CONVERTTOG ~ FALSE;                                     11236000
            END ELSE                                                    11237000
$ INCLUDE 11244001, IF INTERMEDIATE;                                    11237999
            IF FIELDTOG THEN BEGIN                                      11238000
                CAT1 ~ ARY[W];                                          11239000
                IF NOWCODE ! FIELD.CROW THEN                            11240000
                    READ(ISOCODE[NOWCODE~FIELD.CROW],120,CODRAY[*]);    11241000
                C ~ ISOCAT(FIELD.CRAD);                                 11242000
                FIELDTOG ~ FALSE;                                       11243000
            END ELSE                                                    11244000
                C ~ ARY[W];                                             11245000
        END;                                                            11246000
        A ~ ARY[W];                                                     11247000
                                                                        11248000
    CASE OP OF BEGIN                                                    11249000
        ;                              % NOPP                           11250000
        BEGIN                          % OPSR                           11251000
            PSEUDOREAD(ARY,A.CTR+1,W);                                  11252000
            NOREAD ~ TRUE;                                              11253000
        END;                                                            11254000
        BEGIN                          % CPSR                           11255000
            HM ~ 8|W + 3;                                               11256000
            IF A ~ -8|BW + BWMAX - BC - 1 < 0 THEN A ~ 0;               11257000
$ INCLUDE 11257101, IF DEBUGGING;                                       11257049
            IF PSEUDOTOG THEN                                           11257050
                WRITE(PRINTER,CM1,A);                                   11257100
            IF A!0 THEN HM ~ MOVER(A,BFR,8|BW+BC,ARY,HM,SZ);            11258000
            ARY[W].CTR ~ A;                                             11259000
$ INCLUDE 11259951, IF DEBUGGING;                                       11259799
            IF PSEUDOTOG THEN BEGIN                                     11259800
                HM ~ 3;                                                 11259825
                COPE(ARY,W,OP~OOPDC,0);                                 11259850
                HM ~ -A & OERRO[TOREF] & 2[TOOFFSET]                    11259900
            END ELSE                                                    11259950
            HM ~ 3;                                                     11264000
            READACARD;                                                  11265000
            NOREAD ~ FALSE;                                             11266000
        END;                                                            11267000
        TOGGLE ~ C=1 AND N=1;          % SEQL                           11268000
        TOGGLE ~ C!1 OR  N!1;          % SNEQ                           11269000
        TOGGLE ~ C=1 AND N=0 OR C=0;   % SLSS                           11270000
        TOGGLE ~ C=1 AND N<2 OR C=0;   % SLEQ                           11271000
        TOGGLE ~ C=1 AND N=2 OR C=2;   % SGTR                           11272000
        TOGGLE ~ C=1 AND N>0 OR C=2;   % SGEQ                           11273000
OPDC:   BEGIN                          % OPDC                           11274000
            IF LENGTHTOG THEN BEGIN                                     11275000
                LNGTH ~ PL;                                             11276000
                FC ~ TFC;                                               11277000
            END ELSE BEGIN                                              11278000
                LNGTH ~ A.CTR;                                          11279000
                FC ~ 0;                                                 11280000
            END;                                                        11281000
            IF CONVERTTOG THEN BEGIN                                    11282000
                LNGTH ~ LIMT;                                           11283000
$ INCLUDE 11290001, IF INTERMEDIATE;                                    11283999
                IF FIELDTOG THEN BEGIN                                  11284000
                    CAT1 ~ A;                                           11285000
                    IF NOWCODE ! FIELD.CROW THEN                        11286000
                    READ(ISOCODE[NOWCODE~FIELD.CROW],120,CODRAY[*]);    11287000
                    W ~ ISOCAT(FIELD.CRAD);                             11288000
                    FIELDTOG ~ FALSE;                                   11289000
                END ELSE                                                11290000
                W ~ A;                                                  11291000
                IF REAL(W<0)+HM+LNGTH > MSMAX THEN                      11292000
                    OVRFL                                               11293000
                ELSE BEGIN                                              11294000
                    IF W < 0 THEN BEGIN                                 11295000
                        P("-",ACCM[HM.WRD],HM.CHR);                     11296000
                        HM ~ HM+1;                                      11297000
                    END;                                                11298000
                    MOVEDEC(ABS(W),ACCM[HM.WRD],HM.CHR,8-LNGTH+FC,      11299000
                        LNGTH,0);                                       11300000
                END;                                                    11301000
                HM ~ HM + LNGTH;                                        11302000
            END ELSE BEGIN                                              11303000
                HM ~ MOVER(LNGTH,ARY,8|W+FC+3,ACCM,HM,0);               11304000
                IF C~FC+LNGTH-A.CTR > 0 THEN                            11305000
                    DIALB(C.C6,C,ACCM[(N~HM-C).WRD],N.CHR);             11306000
            END;                                                        11307000
            CONVERTTOG ~ LENGTHTOG ~ FIELDTOG ~ FALSE;                  11308000
        END OPDC;                                                       11309000
        BEGIN HM ~ HM + 1; GO OPDC END;% OPDS                           11310000
NSTR:   BEGIN                          % NSTR                           11311000
            IF LENGTHTOG THEN BEGIN                                     11312000
                IF PL = 0 THEN BEGIN   % APPEND                         11313000
                    PL ~ HM - 3;                                        11314000
                    FC ~ A.CTR + TFC;                                   11315000
                END ELSE                                                11316000
                IF B~A.CTR < FC THEN                                    11317000
                    ERROR(-94); % FIELD DISJOINT FROM CONTENTS          11318000
                C ~ MOVER(PL,ACCM,3,ARY,(FC~8|W+FC)+3,SZ)-8|W-3;        11319000
                IF N~PL+3-HM > 0 THEN                                   11320000
                    DIALB(N.C6,N,ARY[(N~FC+HM).WRD],N.CHR);             11321000
                IF C > B THEN                                           11322000
                    ARY[W].CTR ~ C;                                     11323000
                LENGTHTOG ~ FALSE;                                      11324000
            END ELSE                                                    11325000
            ARY[W] ~ 0&(MOVER(HM,ACCM,0,ARY,8|W,SZ)-8|W-3)              11326000
                [TOCTR]&ARY[W][18:18:30];                               11327000
            BASE ~ BASE.NEXT & T[TONEXT];                               11328000
        END;                                                            11329000
        GO NSTR;                       % DSTR                           11330000
        BEGIN                          % ISTR                           11331000
$ INCLUDE 11339001, IF INTERMEDIATE;                                    11331999
          IF FIELDTOG THEN BEGIN                                        11332000
            IF NOWCODE ! FIELD.CROW THEN                                11333000
                READ(ISOCODE[NOWCODE~FIELD.CROW],120,CODRAY[*]);        11334000
            CAT1 ~ A;                                                   11335000
            CAT2 ~ IACCM;                                               11336000
            ARY[W] ~ ISOCAT(FIELD.CRAD+1);                              11337000
            FIELDTOG ~ FALSE;                                           11338000
          END ELSE                                                      11339000
          ARY[W] ~ IACCM;                                               11340000
            BASE ~ BASE.NEXT & T[TONEXT];                               11341000
        END;                                                            11342000
        IACCM ~ C;                     % IOPC                           11343000
        IACCM ~ IACCM + C;             % ADDI                           11344000
        IACCM ~ IACCM - C;             % SUBI                           11345000
        IACCM ~ IACCM | C;             % MULI                           11346000
        IACCM ~ IACCM DIV C;           % DIVI                           11347000
        ERROR(7);                                                       11348000
        ERROR(7);                                                       11349000
    END COPE CASE;                                                      11350000
        IF BOOLEAN(MNEMON[OP]).ZACM THEN BEGIN                          11351000
            ACCM[0] ~ "     ";                                          11352000
$ INCLUDE 11354001, IF DEBUGGING;                                       11352999
            IF MONITER OR ACMLST THEN                                   11353000
                BLANK(ACCM[1],0,13);                                    11354000
            HM ~ 3;                                                     11355000
            PACKLAST ~ FALSE;                                           11355050
        END;                                                            11356000
    END COPE;                                                           11357000
        COMMENT  LOADSCAN IS USED TO PROCESS OPERATORS REQUIRING        11400000
    ACCESS TO 2 MSTRINGS AT ONCE. TYPICALLY THESE ARE LOADING ITEMS     11401000
    FOR TEST, SCAN UNTIL/THRU, & USER DICTIONARY ACTION;                11402000
        PROCEDURE LOADSCAN(ARY,W,THM,L4,OP);                            11403000
            VALUE OP,W;                                                 11404000
            REAL THM,OP,W; ARRAY ARY,L4[0];                             11405000
            BEGIN                                                       11406000
                BOOLEAN THRUTOG; REAL LNGTH;                            11407000
                LABEL LDFT,LNUM,XLNUM,LOTHERS,XLOTH,LDEC,SUCC,          11408000
                  STRT,EXIT,LOOP;                                       11409000
                ALPHA PROCEDURE ADJUST(A);VALUE A; ALPHA A;             11410000
                    IF A="5?0001" THEN ADJUST~ "5BEGIN" ELSE            11411000
                    IF A="5?0000" THEN ADJUST~ "3END  " ELSE            11412000
                    ADJUST~ A;                                          11413000
                IF OP = OLDFT THEN GO LDFT;                             11414000
                HM ~ HM + REAL(OP=OSUNS OR OP=OSTHS);                   11415000
                THRUTOG ~ OSTHC{OP{OSTHS;                               11416000
                TOGGLE ~ FALSE;                                         11417000
                THM ~ 0;                                                11418000
                GO STRT;                                                11419000
LOOP:           TOGGLE ~ TRUE;                                          11420000
                THM ~ 0;                                                11421000
                IF TYPE = 0 THEN SETINSERT ELSE SETPACK;                11422000
STRT:           IF STEPI=63 AND ELB.SUB=0 AND NOREAD THEN BEGIN         11423000
                    T ~ T - 1;                                          11424000
                    TOGGLE ~ FALSE;                                     11425000
                    GO EXIT;                                            11426000
                END;                                                    11427000
                IF TYPE = VOTHERS THEN GO LOTHERS ELSE                  11428000
                IF TYPE = VDECLRN THEN GO LDEC ELSE                     11429000
                IF TYPE ! 0 THEN GO LNUM;                               11430000
                DO BEGIN                                                11431000
                    IF L4[THM].C23=ELB+64 THEN                          11432000
                    GO SUCC;                                            11433000
                END UNTIL THM ~ L4[THM].LSLNK = 0;                      11434000
                GO LOOP;                                                11435000
LNUM:           DO BEGIN                                                11436000
                IF A~L4[THM].LSCMP ! TANK[ELB] THEN                     11437000
                    GO XLNUM;                                           11438000
                IF LNGTH ~ A.LSCTR < 6 THEN GO SUCC;                    11439000
                IF COMPARE(TANK[ELB],L4[THM],0,0,LNGTH)=1 THEN          11440000
                    GO SUCC;                                            11441000
XLNUM:          END UNTIL THM ~ L4[THM].LSLNK = 0;                      11442000
                GO LOOP;                                                11443000
LOTHERS:        IF BEGINV!I~ELB.SUB!ENDV THEN GO LNUM;                  11444000
                DO BEGIN                                                11445000
                A ~ L4[THM].LSCMP;                                      11446000
                IF I = BEGINV THEN                                      11447000
                    IF A="5BEGIN" THEN GO SUCC ELSE GO XLOTH            11448000
                ELSE                                                    11449000
                IF I = ENDV THEN                                        11450000
                    IF A="3END  " THEN GO SUCC;                         11451000
XLOTH:          END UNTIL THM ~ L4[THM].LSLNK = 0;                      11452000
                GO LOOP;                                                11453000
LDEC:           DO BEGIN                                                11454000
                IF L4[THM].LSCMP = "5?DECL" THEN GO SUCC;               11455000
                END UNTIL THM ~ L4[THM].LSLNK = 0;                      11456000
                GO LOOP;                                                11457000
SUCC:           IF THRUTOG THEN                                         11458000
                    IF TYPE=0 THEN SETINSERT                            11459000
                    ELSE                                                11460000
                        SETPACK;                                        11461000
                IF TOGGLE AND PACKLAST THEN HM ~ HM - 1;                11462000
                THM ~ MOVER(L4[THM].LSCTR,L4[*],                        11463000
                    8|THM+3,ARY,8|W+3,0);                               11464000
                ARY[W].CTR ~ THM-3;                                     11465000
                GO EXIT;                                                11466000
LDFT:                                                                   11467000
                IF NOT CONVERTTOG THEN BEGIN                            11468000
                    ARY[W] ~ ADJUST(ARY[W]);                            11469000
                    LNGTH ~ ARY[W].LSCTR;                               11470000
                END;                                                    11471000
                IF CONVERTTOG THEN                                      11472000
                    BEGIN                                               11473000
                        LNGTH~ LIMT;                                    11474000
                        W ~ ARY[W];                                     11475000
                        IF HM+LNGTH+REAL(W<0) > MSMAX THEN              11476000
                            OVRFL                                       11477000
                        ELSE BEGIN                                      11478000
                            IF W<0 THEN BEGIN                           11479000
                                P("-",ACCM[HM.WRD],HM.CHR);             11480000
                                HM ~ HM+1 END;                          11481000
                            MOVEDEC(ABS(W),ACCM[HM.WRD],                11482000
                              HM.CHR,8-LNGTH,LNGTH,0);                  11483000
                        END;                                            11484000
                        HM~ HM+LNGTH                                    11485000
                    END                                                 11486000
                ELSE                                                    11487000
                    HM ~ MOVER(LNGTH,ARY,8|W+3,ACCM,HM,0);              11488000
                    CONVERTTOG~ LENGTHTOG~ FALSE;                       11489000
                    ACCM[THM.LSBAK].LSLNK ~ THM;                        11490000
                    ACCM[THM].LSCTR ~ LNGTH;                            11491000
                    THM.LSBAK ~ THM;                                    11492000
                    THM.LSNXT ~ (HM+7).WRD;                             11493000
                    HM ~ THM.LSNXT|8 + 3;                               11494000
                    ACCM[THM] ~ "     ";                                11495000
EXIT:       END LOADSCAN;                                               11496000
$ INCLUDE 11754001, IF ADVANCED;                                        11499999
        COMMENT  INUDICT ATTEMPTS TO LOCATE ENTRIES IN THE USER         11500000
    DICTIONARY. AN ADDRESS IS RETURNED ONLY IF ALL SPECIFIED            11501000
    QUALITIES & QUALIFIERS AGREE. QUALIFIERS ARE EXAMINED IN A          11502000
    RECURSIVE MANNER;                                                   11503000
INTEGER PROCEDURE INUDICT(L4,I); VALUE I; INTEGER I; ARRAY L4[0];       11504000
    BEGIN                                                               11505000
        INTEGER NW,D,LNG;                                               11506000
        REAL A;                                                         11507000
        LABEL NEXT,CHECKPROPERTIES,LOOP,EXIT;                           11508000
                                                                        11509000
        A ~ L4[I];                                                      11510000
        NW ~ DLIST[(D~A.LSCMP)MOD 125+125];                             11511000
        LNG ~ A.LSCTR;                                                  11512000
        IF NW } 0 THEN DO BEGIN                                         11513000
            IF UDICT[NW.IR,NW.IC+2] = D THEN                            11514000
                IF COMPARE(UDICT[NW.IR,NW.IC+2],L4[I],0,0,LNG)=1        11515000
                  THEN GO CHECKPROPERTIES;                              11516000
NEXT:   END UNTIL NW ~ UDICT[NW.IR,NW.IC] < 0;                          11517000
        INUDICT ~ -1;                                                   11518000
        GO EXIT;                                                        11519000
CHECKPROPERTIES:                                                        11520000
        D ~ I;                                                          11521000
        WHILE D ~ L4[D].LSLNK ! 0 DO BEGIN                              11522000
            IF BOOLEAN(A~L4[D]).MFDG THEN BEGIN                         11523000
                IF BOOLEAN(A).MFINT THEN BEGIN                          11524000
                    FIELD ~ A;                                          11525000
                    FIELDTOG ~ TRUE;                                    11526000
                    COPE(UDICT[NW.IR,*],(LNG+10).WRD+NW.IC+A.MFOFF      11527000
                      +2,OIOPC,0);                                      11528000
                    IF L4[D+1] = IACCM THEN                             11529000
                        GO LOOP                                         11530000
                    ELSE                                                11531000
                        GO NEXT;                                        11532000
                END ELSE BEGIN                                          11533000
                    LENGTHTOG ~ TRUE;                                   11534000
                    TFC ~ A.MFBGN;                                      11535000
                    PL ~ A.MFLNG;                                       11536000
                    COPE(UDICT[NW.IR,*],(LNG+10).WRD+NW.IC+A.MFOFF      11537000
                      +2,OOPDC,0);                                      11538000
                    IF COMPARE(ACCM,L4[D+1],0,0,PL) = 1 THEN            11539000
                        GO LOOP                                         11540000
                    ELSE                                                11541000
                        GO NEXT;                                        11542000
                END;                                                    11543000
            END MFIELDS;                                                11544000
            D ~ INUDICT(L4,D);                                          11545000
            IF D < 0 THEN                                               11546000
                GO NEXT                                                 11547000
            ELSE BEGIN                                                  11548000
                INUDICT ~ NW+1;                                         11549000
                GO EXIT;                                                11550000
            END;                                                        11551000
LOOP:   END WHILE;                                                      11552000
        INUDICT ~ NW+1;                                                 11553000
EXIT:                                                                   11554000
    END INUDICT;                                                        11555000
        COMMENT  ADUDICT ADDS ENTRIES TO THE USER DICTIONARY.           11600000
    EVERYTHING USED IS TAKEN FROM LOCAL[4,*] AS STORED BY LDFT.         11601000
    WORDS RESERVED BEYOND IDENTIFIER ARE INITIALLY ZEROES TO PROTECT    11602000
    AGAINST FLAG BIT ERRORS. PROPERTIES & QUALIFIERS ARE ENTERED &      11603000
    CHECKED. ANY ERRORS CAUSE ENTRY NOT TO BE ENTERED & TOGGLE TO BE    11604000
    SET FALSE. UDICT ADDRESS RETURNED AS VALUE;                         11605000
INTEGER PROCEDURE ADUDICT(L4); ARRAY L4[0];                             11606000
    BEGIN                                                               11607000
        INTEGER LNG,NW,I,J,K,SZ;                                        11608000
        LABEL EXIT;                                                     11609000
        LNG ~ (L4[2].CTR+10).WRD;                                       11610000
        NW ~ DLIST[I~L4[2].LSCMP MOD 125+125];                          11611000
        SZ ~ L4[1];                                                     11612000
        IF (K~J~NXTUDICT).IC > 254-LNG-SZ THEN BEGIN                    11613000
            NXTUDICT ~ 0&(NXTUDICT+256)[IRTOIR];                        11614000
            IF BLIST[BLVL+50] = J THEN                                  11615000
                BLIST[BLVL+50] ~ NXTUDICT;                              11616000
        END;                                                            11617000
        J ~ NXTUDICT - LSTUDICT;                                        11618000
        UPUT(NXTUDICT,NW);                                              11619000
        MOVE(LNG,L4,UDICT[NXTUDICT.IR,NXTUDICT.IC+2]);                  11620000
        UPUT(NXTUDICT+1,0&SZ[TOUSIZ]&BLVL[TOBLKLVL]&J[TODIFF]);         11621000
        J ~ NXTUDICT + LNG + 2;                                         11622000
        IF SZ > 0 THEN BEGIN                                            11623000
            UDICT[J.IR,J.IC] ~ 0;                                       11624000
            IF SZ > 1 THEN                                              11625000
                MOVE(SZ-1,UDICT[J.IR,J.IC],UDICT[J.IR,J.IC+1]);         11626000
        END;                                                            11627000
        I ~ 0;                                                          11628000
        WHILE I ~ L4[I].LSLNK ! 0 DO BEGIN                              11629000
            IF BOOLEAN(A~L4[I]).MFDG THEN BEGIN % MFIELD                11630000
                IF BOOLEAN(A).MFINT THEN BEGIN                          11631000
                    FIELD ~ A;                                          11632000
                    FIELDTOG ~ TRUE;                                    11633000
                    IACCM ~ L4[I+1];                                    11634000
                    COPE(UDICT[J.IR,*],J.IC+A.MFOFF,OISTR,0);           11635000
                END ELSE BEGIN                                          11636000
                    IF ((LNG~L4[I+1].CTR)+10).WRD +                     11637000
                      (FIELD~L4[I]).MFOFF > SZ THEN                     11638000
                        OVRFL                                           11639000
                    ELSE                                                11640000
                        NW ~ MOVER(LNG+3,L4[*],(I+1)|8,                 11641000
                          UDICT[J.IR,*],(J.IC+A.MFOFF)|8,0);            11642000
                END;                                                    11643000
            END MFIELDS ELSE BEGIN % QUALIFIER                          11644000
                NW ~ INUDICT(L4[*],I+1);                                11645000
                IF NW > 0 THEN                                          11646000
                    UPUT(NXTUDICT+1,UGET(NXTUDICT+1)&NW[TOUQUAL])       11647000
                ELSE BEGIN                                              11648000
                    ERROR(99); % INVALID QUALIFIER                      11649000
                    NXTUDICT ~ K;                                       11650000
                    ADUDICT ~ -1;                                       11651000
                    GO EXIT;                                            11652000
                END;                                                    11653000
            END;                                                        11654000
        END WHILE LINKING;                                              11655000
        ADUDICT ~ NXTUDICT + 1;                                         11656000
        NXTUDICT ~ J + SZ + 1;                                          11657000
EXIT:                                                                   11658000
    END ADUDICT;                                                        11659000
        COMMENT  SEARCHUDICT IS USED FOR SERIAL SCANS OF THE USER       11700000
    DICTIONARY. SEARCHING BEGINS AT THE ITEM AFTER THE ADDRESS          11701000
    GIVEN BY THE POINTER IN L4[1]. VARIOUS CHECKS ARE MADE              11702000
    ALONG THE WAY FOR QUALIFICATION & ATTRIBUTES OF THE                 11703000
    ENTRIES. NON-MATCHES ARE BYPASSED.                                  11704000
        TOGGLE IS TRUE ONLY ON A VALID SEARCH: END-OF-DICTIONARY        11705000
    IMPLIES NO MATCH & SETS TOGGLE FALSE;                               11706000
INTEGER PROCEDURE SEARCHUDICT(L4); ARRAY L4[0];                         11707000
BEGIN                                                                   11708000
    INTEGER START,D;                                                    11709000
    LABEL MORE,OK,LOOP,EXIT;                                            11710000
    IF START ~ L4[1] = 0 THEN                                           11711000
        START ~ 1                                                       11712000
    ELSE                                                                11713000
MORE:   START ~ (UGET(START+1).CTR+7).WRD + UGET(START).USIZ + 1;       11714000
    IF START } NXTUDICT THEN BEGIN                                      11715000
        START ~ -1;                                                     11716000
        GO EXIT;                                                        11717000
    END;                                                                11718000
    D ~ 0;                                                              11719000
    WHILE D ~ L4[D].LSLNK ! 0 DO BEGIN                                  11720000
        IF BOOLEAN(A~L4[D]).MFDG THEN BEGIN                             11721000
            IF BOOLEAN(A).MFINT THEN BEGIN                              11722000
                FIELD ~ A;                                              11723000
                FIELDTOG ~ TRUE;                                        11724000
                COPE(UDICT[START.IR,*],(UGET(START+1).CTR+7).WRD +      11725000
                    A.MFOFF + START.IC + 2,OIOPC,0);                    11726000
                IF L4[D+1] = IACCM THEN                                 11727000
                    GO LOOP                                             11728000
                ELSE                                                    11729000
                    GO MORE;                                            11730000
            END ELSE BEGIN                                              11731000
                LENGTHTOG ~ TRUE;                                       11732000
                TFC ~ A.MFBGN;                                          11733000
                PL ~ A.MFLNG;                                           11734000
                COPE(UDICT[START.IR,*],(UGET(START+1).CTR+7).WRD +      11735000
                    A.MFOFF + START.IC + 2,OOPDC,0);                    11736000
                IF COMPARE(ACCM,L4[D+1],0,0,PL) = 1 THEN                11737000
                    GO LOOP                                             11738000
                ELSE                                                    11739000
                    GO MORE;                                            11740000
            END;                                                        11741000
        END MFIELDS;                                                    11742000
        IF D~INUDICT(L4,D) < 0 THEN                                     11743000
            GO MORE                                                     11744000
        ELSE                                                            11745000
            GO OK;                                                      11746000
LOOP: END WHILE;                                                        11747000
OK:                                                                     11748000
    ACCM[0] ~ "     ";                                                  11749000
        HM ~ MOVER(UGET(START+1).CTR,UDICT[START.IR,*],8|START.IC+10,   11750000
        ACCM,3,0);                                                      11751000
EXIT:                                                                   11752000
    SEARCHUDICT ~ START;                                                11753000
END SEARCHUDICT;                                                        11754000
COMMENT  BACK TO MACRON * * * * * * *;                                  12000000
    LABEL COPEWITHIT,LITERALLY,IOPROCESS,LOADNSCAN,                     12001000
        MAKEACASEOFIT,LITC,SCN,SCNTYP,OACC,EXIT,AGAIN,TEST;             12002000
TEST:       IF TOGGLE THEN                                              12003000
              IF CR(ACCM[(HM-1).WRD],(HM-1).CHR) = " " THEN             12003050
                HM ~ HM - 1;                                            12003100
            IF NOT TOGGLE THEN                                          12004000
            INSERT(HM.CHR,ACCM[HM.WRD]," ",1);                          12005000
AGAIN:      X~(EP~ X)+3;                                                12006000
            OPCODE(LOCAL[2,EP.WRD],EP.CHR,OP,ADDR);                     12007000
            GOP ~ OP; GADR ~ ADDR;                                      12008000
            IF IADDR!0 AND OP!OINT1 THEN BEGIN                          12009000
                IF ADDR~ IADDR>0 THEN                                   12010000
                    ADDR~ IADDR.REF;                                    12011000
                LIMT ~ IADDR.LIM; IADDR~ 0;                             12012000
                END;                                                    12013000
$ INCLUDE 12022001, IF DEBUGGING;                                       12013999
            IF MONITER OR ACMLST THEN                                   12014000
                IF PRINT THEN BEGIN                                     12015000
                    WRITE(PRINTER,15,ACCM[*]);                          12016000
                    IF MONITER THEN                                     12017000
                        WRITE(PRINTER,MON2,EP,A~MNEMON[OP],             12018000
                            IF BOOLEAN(A).DBA THEN "A" ELSE "I",        12019000
                    IF BOOLEAN(A).DBMN THEN MNEMON[ADDR] ELSE ADDR      12020000
                            ,T,TOGGLE,HM,OHM,IACCM,IACCM1);             12021000
                END;                                                    12022000
            IF ADDR<0 THEN                                              12023000
                IF BOOLEAN(MNEMON[OP]).ZACM THEN                        12024000
                    GO OACC                                             12025000
                ELSE                                                    12026000
                    GO AGAIN;                                           12027000
    IF OP { 20 THEN GO COPEWITHIT ELSE                                  12028000
    IF OP { 35 THEN GO LITERALLY ELSE                                   12029000
    IF OP { 40 THEN GO IOPROCESS ELSE                                   12030000
    IF OP { 50 THEN GO LOADNSCAN ELSE                                   12031000
    GO MAKEACASEOFIT;                                                   12032000
                                                                        12033000
COPEWITHIT:                                                             12034000
$ INCLUDE 12034551, IF ADVANCED;                                        12034049
        IF BOOLEAN(ADDR).UDCT THEN BEGIN                                12034050
            D ~ UGET(ADDR);                                             12034100
            TEMP ~ (UGET(ADDR+1).CTR+10).WRD;                           12034150
            IF BOOLEAN(LIMT).MFDCT THEN                                 12034200
                POS ~ (LIMT.MFLIM+10).WRD                               12034250
            ELSE                                                        12034300
                POS ~ D.USIZ - FIELD.MFOFF;                             12034350
            COPE(UDICT[ADDR.IR,*],ADDR.IC+TEMP+FIELD.MFOFF,             12034400
                OP,POS);                                                12034450
            GO AGAIN;                                                   12034500
        END;                                                            12034550
    IF BOOLEAN(MNEMON[OP]).MOVM THEN                                    12035000
        IF ADDR } GALFBAS THEN                                          12036000
            POS ~ (HOWLONG(SPACEMAP[(POS~ADDR-GALFBAS).D32+8],          12037000
                POS.M32)+POS.M4)|64                                     12038000
            ELSE                                                        12039000
            POS ~ (HOWLONG(SPACEMAP[ADDR.D32],ADDR.M32)+ADDR.M4)|64;    12040000
    IF ADDR } CONSBAS THEN                                              12041000
        COPE(LOCAL[1,*],ADDR-CONSBAS,OP,0)                              12042000
    ELSE                                                                12043000
    IF ADDR } GPTRBAS THEN                                              12044000
        COPE(GPTR,ADDR-GPTRBAS,OP,0)                                    12045000
    ELSE                                                                12046000
    IF ADDR } GINTBAS THEN                                              12047000
        COPE(GINT,ADDR-GINTBAS,OP,0)                                    12048000
    ELSE                                                                12049000
    IF ADDR } GALFBAS THEN                                              12050000
        COPE(GLOBAL[(ADDR-GALFBAS).D4,*],ADDR.M4|64,OP,POS)             12051000
    ELSE                                                                12052000
    IF ADDR } LPTRBAS THEN                                              12053000
        COPE(LOCAL[6,*],ADDR-LPTRBAS,OP,0)                              12054000
    ELSE                                                                12055000
    IF ADDR } LINTBAS THEN                                              12056000
        COPE(LOCAL[0,*],ADDR-LINTBAS,OP,0)                              12057000
    ELSE                                                                12058000
        COPE(LOCAL[ADDR.D4,*],ADDR.M4|64,OP,POS);                       12059000
$ INCLUDE 12059301, IF DEBUGGING;                                       12059049
    IF HM < 0 THEN BEGIN                                                12059050
        OP ~ HM.REF;                                                    12059100
        ADDR ~ HM.OFFSET;                                               12059150
        HM ~ HM.LIM + 3;                                                12059200
        GO IOPROCESS;                                                   12059250
    END;                                                                12059300
    GO AGAIN;                                                           12060000
                                                                        12061000
LITERALLY:                                                              12062000
    CASE OP-21 OF BEGIN                                                 12063000
        IACCM ~ ADDR;                  % ILIT                           12064000
        IACCM ~ IACCM + ADDR;          % ADDL                           12065000
        IACCM ~ IACCM - ADDR;          % SUBL                           12066000
        IACCM ~ IACCM | ADDR;          % MULL                           12067000
        IACCM ~ IACCM DIV ADDR;        % DIVL                           12068000
        BEGIN LNGTH ~ 1; GO LITC END;  % LT1C                           12069000
        BEGIN LNGTH ~ 2; GO LITC END;  % LT2C                           12070000
        BEGIN                          % LT2S                           12071000
            HM ~ HM + 1;                                                12072000
            LNGTH ~ 2;                                                  12073000
            GO LITC;                                                    12074000
        END;                                                            12075000
        ERROR(10);                                                      12076000
        ERROR(10);                                                      12077000
        ERROR(10);                                                      12078000
        ERROR(10);                                                      12079000
        ERROR(10);                                                      12080000
    END LITERAL CASE;                                                   12081000
    GO AGAIN;                                                           12082000
                                                                        12083000
LITC:                                                                   12084000
    IF HM+LNGTH > MSMAX THEN                                            12085000
        OVRFL                                                           12086000
    ELSE                                                                12087000
        M(ADDR,6,ACCM[HM.WRD],HM.CHR,0,0,LNGTH);                        12088000
    HM ~ HM + LNGTH;                                                    12089000
    GO AGAIN;                                                           12090000
                                                                        12091000
IOPROCESS:                                                              12092000
    CASE OP-36 OF BEGIN                                                 12093000
        BEGIN                          % INPT                           12094000
            PSEUDOREAD(ACCM,HM-2,0);                                    12095000
            RESCANTOG ~ BOOLEAN(ADDR);                                  12096000
        END;                                                            12097000
        BEGIN                          % OTPT                           12098000
$ INCLUDE 12104001, IF ADVANCED;                                        12098999
            IF ADDR ! 0 THEN BEGIN                                      12099000
                IF GPTRBAS { ADDR { GPTRMAX THEN                        12100000
                    LINKOUT(GPTR[*],ADDR-GPTRBAS)                       12101000
                ELSE                                                    12102000
                    LINKOUT(LOCAL[6,*],ADDR-LPTRBAS);                   12103000
                IF INTERLIST THEN BEGIN                                 12103050
                    OP ~ OERRO;                                         12103100
                    ADDR ~ 3;                                           12103150
                    GO IOPROCESS;                                       12103200
                END;                                                    12103250
            END ELSE                                                    12104000
                OHM ~ MOVER(HM-3,ACCM,3,OA,OHM,0) + 1;                  12105000
        END;                                                            12106000
        BEGIN                          % ERRO                           12107000
            ARRAY ERB[0:14];                                            12108000
            BLANK(ERB,0,14);                                            12109000
$ INCLUDE 12128001, IF ADVANCED;                                        12109999
            IF 2 { ADDR { 14 THEN BEGIN               % POST            12110000
                IF ADDR = 3 THEN                                        12111000
                    WRITE(PRINTER)                                      12112000
                ELSE                                                    12113000
                IF ADDR } 4 THEN                                        12114000
                    WRITE(PRINTER[ADDR-3]);                             12115000
                A ~ HM - I~3;                                           12116000
                IF A > 0 THEN BEGIN                                     12117000
                    WHILE A } 72 DO BEGIN                               12118000
                        MOV(1,8,ACCM[I.WRD],I.CHR,ERB,8);               12119000
                        A ~ A - 72;                                     12120000
                        I ~ I + 72;                                     12121000
                        WRITE(PRINTER,15,ERB[*]);                       12122000
                        BLANK(ERB,0,14);                                12123000
                    END;                                                12124000
                    MOV(A.C6,A,ACCM[I.WRD],I.CHR,ERB,8);                12125000
                    WRITE(PRINTER,15,ERB[*]);                           12126000
                END;                                                    12127000
            END ELSE                                                    12128000
            IF ADDR = 15 THEN BEGIN                   % RECOVER         12129000
                WRITE(ERB[*],<"ATTEMPT TO USE INVALID MACRO:",          12130000
                X65,4("******")>);                                      12130500
                OHM ~ MOVER(A~GET(MADDR).CTR,DICT[MADDR.IR,*],          12131000
                    I~MADDR.IC|8+3,OA,OHM,0)+1;                         12132000
                A ~ MOVER(A,DICT[MADDR.IR,*],I,ERB,32,0);               12133000
                MOVE(1,LINKS[1],ERB[11]);                               12133500
                WRITE(PRINTER,15,ERB[*]);                               12134000
                MOVE(1,LINKS[0],LINKS[1]);                              12134500
                GO EXIT;                                                12135000
$ INCLUDE 12135151, IF ADVANCED;                                        12135049
            END ELSE                                                    12135050
            IF ADDR > 15 THEN BEGIN                                     12135100
                ERRORLINK(ADDR-3);                                      12135150
            END ELSE BEGIN             % ERROR & WARNING                12136000
                IF ADDR = 0 THEN ERRCT ~ ERRCT + 1 ELSE                 12137000
                    IF NOT WARNTOG THEN GO OACC;                        12138000
                IF OP ~ GET(MADDR).CTR>78-D~HM-3 THEN                   12139000
                    D~ 78-OP;                                           12140000
                TRANS(ACCM,ERB,D.C6,D,OP,DICT[MADDR.IR,                 12141000
                  MADDR.IC],EP,ERB[10],IF ADDR=0 THEN                   12142000
                  " >>>>>>" ELSE " >NOTE>");                            12143000
                MOVE(1,LINKS[1],ERB[11]);                               12144000
                MOVE(1,LINKS[0],LINKS[1]);                              12145000
                WRITE(PRINTER[DBL],15,ERB[*]);                          12146000
            END;                                                        12147000
        END ERROR STMT;                                                 12148000
        ERROR(10);                                                      12149000
        ERROR(10);                                                      12150000
    END IOPROCESS;                                                      12151000
    GO OACC;                                                            12152000
                                                                        12153000
LOADNSCAN:                                                              12154000
    CASE OP-41 OF BEGIN                                                 12155000
        BEGIN                          % LDFT                           12156000
            LABEL L; L:                                                 12157000
                IF NOT LOADING THEN BEGIN                               12158000
                    LOADING ~ TRUE;                                     12159000
                    IF HM>3 THEN BEGIN                                  12160000
                        ACCM[0].CTR ~ HM - 3;                           12161000
                        MOVE((HM+10).WRD,ACCM,LOCAL[5,*]);              12162000
                        HM ~ 3;                                         12163000
                        LOADING ~ <3>;                                  12164000
                    END;                                                12165000
                    TEMP~ 0;                                            12166000
                END;                                                    12167000
                IF ADDR } CONSBAS THEN                                  12168000
                LOADSCAN(LOCAL[1,*],ADDR-CONSBAS,TEMP,LOCAL[4,*],OP)    12169000
                ELSE                                                    12170000
                IF ADDR } GINTBAS THEN                                  12171000
                    LOADSCAN(GINT,ADDR-GINTBAS,TEMP,LOCAL[4,*],OP)      12172000
                ELSE                                                    12173000
                IF ADDR } GALFBAS THEN                                  12174000
LOADSCAN(GLOBAL[(ADDR-GALFBAS).D4,*],ADDR.M4|64,TEMP,LOCAL[4,*],OP)     12175000
                ELSE                                                    12176000
                IF ADDR } LINTBAS THEN                                  12177000
                LOADSCAN(LOCAL[0,*],ADDR-LINTBAS,TEMP,LOCAL[4,*],OP)    12178000
                ELSE                                                    12179000
                IF ADDR } 2 THEN                                        12180000
            LOADSCAN(LOCAL[ADDR.D4,*],ADDR.M4|64,TEMP,LOCAL[4,*],OP)    12181000
                ELSE                                                    12182000
                IF ADDR = 1 THEN BEGIN                                  12183000
                    LOCAL[0,0] ~ IACCM;                                 12184000
                    LOADSCAN(LOCAL[0,*],0,TEMP,LOCAL[4,*],OP)           12185000
                END ELSE BEGIN                                          12186000
                    ACCM[0].CTR ~ HM - 3;                               12187000
                    LOADSCAN(ACCM[*],0,TEMP,LOCAL[4,*],OP);             12188000
                END;                                                    12189000
        END;                                                            12190000
        GO SCN;                        % SUNC                           12191000
        GO SCN;                        % SUNS                           12192000
        GO SCN;                        % STHC                           12193000
SCN:    BEGIN LABEL L; L:              % STHS                           12194000
            MOVE((HM+7).WRD,ACCM,LOCAL[4,*]); % SCAN POOL               12195000
            IF LOADING.[46:1] THEN BEGIN                                12196000
                BLANK(ACCM[1],(HM~(HM+7).WRD-2).C6,HM);                 12197000
                ACCM[0] ~ "     ";                                      12198000
                HM ~ LOCAL[5,0].CTR + 3;                                12199000
                MOVE((HM+7).WRD,LOCAL[5,*],ACCM);                       12200000
            END ELSE HM ~ 3;                                            12201000
            PACKLAST ~                                                  12201950
            LOADING ~ FALSE;                                            12202000
            IF ADDR } GALFBAS THEN                                      12203000
LOADSCAN(GLOBAL[(ADDR-GALFBAS).D4,*],ADDR.M4|64,TEMP,LOCAL[4,*],OP)     12204000
            ELSE                                                        12205000
          LOADSCAN(LOCAL[ADDR.D4,*],ADDR.M4|64,TEMP,LOCAL[4,*],OP);     12206000
            LOCAL[4,0] ~ 0;                                             12207000
        END;                                                            12208000
$ INCLUDE 12236001, IF ADVANCED;                                        12208999
        BEGIN                          % ENTR                           12209000
            LOADING ~ FALSE;                                            12210000
            MOVE((HM+7).WRD,ACCM,LOCAL[4,*]);                           12211000
            HM ~ 3;                                                     12212000
            ACCM[0] ~ "     ";                                          12213000
            TOGGLE ~ (IACCM~ADUDICT(LOCAL[4,*])) } 0;                   12214000
            LOCAL[4,0] ~ 0;                                             12215000
            OP ~ OISTR;                                                 12216000
            GO COPEWITHIT;                                              12217000
        END;                                                            12218000
        BEGIN                          % FIND                           12219000
            LOADING ~ FALSE;                                            12220000
            MOVE((HM+7).WRD,ACCM,LOCAL[4,*]);                           12221000
            HM ~ 3;                                                     12222000
            ACCM[0] ~ "     ";                                          12223000
            TOGGLE ~ (IACCM~INUDICT(LOCAL[4,*],0)) } 0;                 12224000
            LOCAL[4,0] ~ 0;                                             12225000
            OP ~ OISTR;                                                 12226000
            GO COPEWITHIT;                                              12227000
        END;                                                            12228000
        BEGIN                          % SRCH                           12229000
            LOADING ~ FALSE;                                            12230000
            MOVE((HM+7).WRD,ACCM,LOCAL[4,*]);                           12231000
            TOGGLE ~ (IACCM~SEARCHUDICT(LOCAL[4,*])) } 0;               12232000
            LOCAL[4,0] ~ 0;                                             12233000
            OP ~ OISTR;                                                 12234000
            GO COPEWITHIT;                                              12235000
        END;                                                            12236000
$ OMIT 12236151, IF ADVANCED;                                           12236049
        ERROR(10);                                    % ENTR            12236050
        ERROR(10);                                    % FIND            12236100
        ERROR(10);                                    % SRCH            12236150
        ERROR(10);                                                      12237000
        ERROR(10);                                                      12238000
    END LOADNSCAN CASE;                                                 12239000
    GO AGAIN;                                                           12240000
                                                                        12241000
MAKEACASEOFIT:                                                          12242000
    CASE OP-51 OF BEGIN                                                 12243000
SCNTYP: BEGIN LABEL L; L:              % TYPC                           12244000
            CASE ADDR OF BEGIN                                          12245000
                TOGGLE ~ EXPRESS(ATOG~FALSE);         % BEXP            12246000
                TOGGLE ~ SPCHAR(0);                   % CHARACTER       12247000
                TOGGLE ~ ELEMENT;                     % ELEMENT         12248000
                TOGGLE ~ EXPRESS(ATOG~TRUE);          % AEXP            12249000
                TOGGLE ~ IDENTIFIER;                  % IDENTIFIER      12250000
                TOGGLE ~ STATE;                       % STATEMENT       12251000
                TOGGLE ~ VARIABLE;                    % VARIABLE        12252000
                TOGGLE ~ DECLARASHUN;                 % DECLARATION     12253000
                TOGGLE ~ BPRIMARY;                    % BPRIMARY        12254000
                TOGGLE ~ PRIMARY;                     % PRIMARY         12255000
                ;;;;;;;;;;;;;;; % OPEN FOR ALPHA ONLY                   12256000
                TOGGLE ~ NUMBER;                      % NUMBER          12257000
                TOGGLE ~ INTGER;                      % INTGER          12258000
            END SCAN TYPES CASE;                                        12259000
            IF TOGGLE AND PACKLAST THEN % SUCCESS AND LAST=ALPHA        12259050
                IF CHAR ! " " THEN  % SPCHR LEFT OVER                   12259100
                    IF TABLET + 2 = NXTELB THEN BEGIN % UNUSED          12259150
                        BC ~ -(BW~(TEMP~8|BW+BC-1).WRD)|8+TEMP;         12259200
                        NXTELB ~ NXTELB - 1 END;                        12259300
            GO TEST;                                                    12260000
        END;                                                            12261000
        BEGIN HM ~ HM + 1; GO SCNTYP END;             % TYPS            12262000
        BEGIN                                         % SETL            12263000
$ INCLUDE 12279001, IF ADVANCED;                                        12263999
            IF ADDR ! 0 THEN BEGIN                                      12264000
                REAL OFFSET;                                            12265000
                IF LINTBAS { ADDR { LINTMAX THEN BEGIN                  12266000
                    ADDR ~ ADDR - LINTBAS + LPTRBAS;                    12267000
                    OFFSET ~ 500;                                       12268000
                END ELSE                                                12269000
                IF GINTBAS { ADDR { GINTMAX THEN BEGIN                  12270000
                    ADDR ~ ADDR - GINTBAS + GPTRBAS;                    12271000
                    OFFSET ~ 500;                                       12272000
                END;                                                    12273000
                IF GPTRBAS { ADDR { GPTRMAX THEN                        12274000
                    LINKIN(GPTR[*],ADDR-GPTRBAS,IACCM,OFFSET)           12275000
                ELSE                                                    12276000
                    LINKIN(LOCAL[6,*],ADDR-LPTRBAS,IACCM,OFFSET);       12277000
                OP ~ OOTPT;                                             12278000
                ADDR ~ 0;                                               12278100
                GO IOPROCESS;                                           12278200
            END RESERVE;                                                12279000
            PL ~ IACCM;                                                 12280000
            LENGTHTOG ~ TRUE;                                           12281000
        END SETL;                                                       12282000
        BEGIN BOOLEAN TOG;                            % CONV            12283000
$ INCLUDE 12292001, IF INTERMEDIATE;                                    12283999
            IF CONSBAS { ADDR { CONSMAX THEN BEGIN % MFIELD             12284000
                FIELD ~ LOCAL[1,ADDR-CONSBAS];                          12285000
                IF NOT FIELDTOG~BOOLEAN(FIELD.MFINT) THEN BEGIN         12286000
                    LENGTHTOG ~ TRUE;                                   12287000
                    IF BOOLEAN(FIELD).MFDCT THEN                        12287100
                      IF (LIMT~FIELD).MFLNG=0 THEN LENGTHTOG~FALSE;     12287200
                    TFC ~ FIELD.MFBGN;                                  12288000
                    PL  ~ FIELD.MFLNG;                                  12289000
                END;                                                    12290000
                    GO AGAIN;                                           12291000
            END ELSE                                                    12292000
            IF CONVBAS { ADDR { 1023 THEN BEGIN                         12293000
            CASE ADDR-CONVBAS OF BEGIN                                  12294000
                IACCM ~ IACCM DIV LOCAL[0,0]; % INTR & DIVIDE           12295000
                IACCM ~ IACCM | LOCAL[0,0];   % INTR & MULTIPLY         12296000
                IACCM ~ IACCM - LOCAL[0,0];   % INTR & SUBTRACT         12297000
                IACCM ~ IACCM + LOCAL[0,0];   % INTR & ADD              12298000
                IACCM ~ LOCAL[0,0];           % INTRINSIC CHANGE        12299000
                IACCM ~ IACCM DIV ATOI;% ALF->INT & DIVIDE              12300000
                IACCM ~ IACCM | ATOI;  % ALF->INT & MULTIPLY            12301000
                IACCM ~ IACCM - ATOI;  % ALF->INT & SUBTRACT            12302000
                IACCM ~ IACCM + ATOI;  % ALF->INT & ADD                 12303000
                IACCM ~ ATOI;          % ALF->INT CONVERT               12304000
                BEGIN                  % INT->ALF CONVERT (8 CHR)       12305000
                    IF TOG~(IACCM<0) THEN                               12306000
                        P("-",ACCM,3);                                  12307000
                    MOVEDEC(ABS(IACCM),ACCM,3+REAL(TOG),0,8,0);         12308000
                    HM ~ 11 + REAL(TOG);                                12309000
                    GO AGAIN;                                           12310000
                END;                                                    12311000
                IACCM1 ~ IACCM;        % INT-INT COMPARE                12312000
                BEGIN                  % ALF-INT COMPARE                12313000
                    IACCM1 ~ CONSTANTCONVERT(ACCM,IF A~HM-3-REAL(       12314000
                        A~CR(ACCM,3)="+" OR A="-") > 8 THEN             12315000
                        8 ELSE A);                                      12316000
                END;                                                    12317000
            END CASE;                                                   12318000
            GO OACC;                                                    12319000
            END;                                                        12320000
            CONVERTTOG ~ TRUE;                                          12321000
            LIMT ~ ADDR;                                                12322000
        END CONV;                                                       12323000
$ INCLUDE 12348001, IF ADVANCED;                                        12323999
        BEGIN LABEL L;                                % INDC            12324000
                IF ADDR{1 THEN BEGIN % TABLE SEARCH                     12325000
                    ACCM[0].CTR ~ HM-3;                                 12326000
                    MOVE((HM+10).WRD,ACCM,IDENT[1]);                    12327000
                    IACCM ~ -1; TOGGLE ~ FALSE;                         12328000
                    IF INDICT<0 THEN BEGIN                              12329000
                        ERROR(-75);                                     12330000
                        GO L;                                           12331000
                    END;                                                12332000
                    IF W.CLASS!VSTRINGNAME OR W.SUB>1 THEN BEGIN        12333000
                        ERROR(-76);                                     12334000
                        GO L;                                           12335000
                    END;                                                12336000
                    IF NOT(BOOLEAN(ADDR)EQV BOOLEAN(W.SUB))THEN         12337000
                        BEGIN ERROR(-77); GO L END;                     12338000
                    IACCM ~ GET(LAD); TOGGLE~ TRUE;                     12339000
                    GO OACC;                                            12340000
                END LOOKUP A SEQUENCE;                                  12341000
                IF(IADDR~IF LPTRBAS{ADDR{LPTRMAX THEN LOCAL[6,ADDR      12342000
                    -LPTRBAS] ELSE GPTR[ADDR-GPTRBAS])=0 THEN BEGIN     12343000
                        ERROR(-78);                                     12344000
                        IADDR ~ -1;                                     12345000
                        GO L;                                           12346000
                END ELSE BEGIN                                          12347000
L:                  ADDR ~ 1;                                           12347100
                    OP ~ OERRO;                                         12347200
                    GO IOPROCESS;                                       12347300
                END;                                                    12347400
        END;                                                            12348000
$ OMIT 12348051, IF ADVANCED;                                           12348049
        ERROR(10);                                    % INDC            12348050
        BEGIN                                         % ICMP            12349000
            IF ADDR } 64 THEN % INT-ALF COMPARE                         12350000
                IACCM~ CONSTANTCONVERT(ACCM[0],IF IACCM~ HM-3-REAL(     12351000
                    IACCM~CR(ACCM[0],3)="+" OR IACCM="-")>8 THEN 8      12352000
                    ELSE IACCM);                                        12353000
            CASE ADDR.C7 OF BEGIN                                       12354000
                TOGGLE~ IACCM1 = IACCM;                                 12355000
                TOGGLE~ IACCM1 ! IACCM;                                 12356000
                TOGGLE~ IACCM1 < IACCM;                                 12357000
                TOGGLE~ IACCM1 { IACCM;                                 12358000
                TOGGLE~ IACCM1 > IACCM;                                 12359000
                TOGGLE~ IACCM1 } IACCM;                                 12360000
                TOGGLE~ REAL(BOOLEAN(IACCM1)EQV BOOLEAN(IACCM))=        12360010
                    REAL(NOT FALSE);                                    12360020
                END;                                                    12361000
            GO OACC;                                                    12362000
        END;                                                            12363000
$ INCLUDE 12373901, IF ADVANCED;                                        12363999
        IF ADDR < 256 THEN CASE ADDR OF BEGIN         % TOGL            12364000
            DEC ~ FALSE;                                                12365000
            DEC ~ TRUE;                                                 12366000
            TOGGLE ~ DEC;                                               12366500
            ERROR(-72);                                                 12367000
            CMT ~ TRUE;                                                 12368000
            TOGGLE ~ CMT;                                               12368500
            ERROR(-72);                                                 12369000
            ENDT ~ TRUE;                                                12370000
            TOGGLE ~ ENDT;                                              12370500
            EQVTOG ~ FALSE;                                             12371000
            EQVTOG ~ TRUE;                                              12372000
            TOGGLE ~ EQVTOG;                                            12372050
        END SYSTEM TOGGLES ELSE BEGIN                                   12373000
            TEMP ~ ADDR.M4;                                             12373050
            ADDR ~ (ADDR-256).D4;                                       12373100
            IF TEMP = 0 THEN                                            12373150
                USERTOG ~ NOT MASK(ADDR) AND USERTOG                    12373200
            ELSE                                                        12373250
            IF TEMP = 1 THEN                                            12373300
                USERTOG ~ MASK(ADDR) OR USERTOG                         12373350
            ELSE                                                        12373400
                TOGGLE ~ REAL(MASK(ADDR) AND USERTOG) ! 0;              12373450
        END USER TOGGLES;                                               12373500
$ OMIT 12373951, IF ADVANCED;                                           12373949
        ERROR(10);                                                      12373950
        ERROR(10);                                                      12374000
        ERROR(10);                                                      12375000
        ERROR(10);                                                      12376000
            BEGIN                                     % JMPC            12377000
                LOCAL[3,0] ~ 0;                                         12378000
                IF ADDR > MLMAX EQV TOGGLE THEN                         12379000
                    X ~ ADDR.JADR;                                      12380000
            END;                                                        12381000
        X ~ ADDR;                                     % JUMP            12382000
        BEGIN                                         % INT1            12383000
            LABEL LINC;                                                 12384000
            CASE ADDR-64 OF BEGIN                                       12385000
                IACCM ~ BLVL-NEST-1;                  % OBGN            12386000
                IACCM ~ IACCM+(BLVL-NEST-1);          % +BGN            12387000
                IACCM ~ IACCM-(BLVL-NEST-1);          % -BGN            12388000
                IACCM ~ IACCM|(BLVL-NEST-1);          % |BGN            12389000
                IACCM ~ IACCM DIV (BLVL-NEST-1);      % /BGN            12390000
                IACCM ~ BLOCKLEVEL-1;                 % OBLK            12391000
                IACCM ~ IACCM+(BLOCKLEVEL-1);         % +BLK            12392000
                IACCM ~ IACCM-(BLOCKLEVEL-1);         % -BLK            12393000
                IACCM ~ IACCM|(BLOCKLEVEL-1);         % |BLK            12394000
                IACCM ~ IACCM DIV (BLOCKLEVEL-1);     % /BLK            12395000
                TFC ~ IACCM;                          % DIAL            12396000
LINC:           BEGIN                                 % LINC            12397000
                IF HM+5 > MSMAX THEN                                    12398000
                        OVRFL                                           12399000
                    ELSE                                                12400000
                        MOVEDEC(RCCNT,ACCM[HM.WRD],HM.CHR,3,5,0);       12401000
                    HM ~ HM + 5;                                        12402000
                END;                                                    12403000
                BEGIN HM ~ HM + 1; GO LINC END;       % LINS            12404000
                BEGIN                                 % ENTI            12405000
                    ACCM[0].CTR ~ HM-3;                                 12406000
                    IF NOT TOGGLE~(LOCAL[0,0] ~                         12407000
                      SCANUMBERCONV(ACCM) } 0) THEN                     12408000
                        ERROR(-8);                                      12409000
                END;                                                    12410000
                LOCAL[0,0] ~ HM - 3;                  % LENG            12411000
                BEGIN                                 % RDCD            12412000
                    NXTELB ~ T+1;                                       12413000
                    READACARD;                                          12414000
                    NOREAD ~ FALSE;                                     12415000
                END;                                                    12416000
                ;                                     % DELE            12417000
                BEGIN                                 % STCK            12418000
                ACCM[0].CTR ~ HM-3;                                     12419000
                STACKIT((HM+7).WRD,ACCM[*]);                            12420000
                END;                                                    12421000
                BEGIN                                 % NSTK            12422000
                UNSTACKIT(256,ACCM[*]);                                 12423000
                HM ~ ACCM[0].CTR + 3;                                   12424000
                END;                                                    12425000
                IACCM ~ T-BASE.CUR;                   % OSCT            12426000
                IACCM ~ IACCM + (T-BASE.CUR);         % +SCT            12427000
                IACCM ~ IACCM - (T-BASE.CUR);         % -SCT            12428000
                IACCM ~ IACCM | (T-BASE.CUR);         % |SCT            12429000
                IACCM ~ IACCM DIV (T-BASE.CUR);       % /SCT            12430000
$ INCLUDE 12433001, IF ADVANCED;                                        12430999
                IACCM ~ UDICT[IADDR.IR,IADDR.IC].BLKLVL;% LEVL          12431000
                IACCM ~ UDICT[IADDR.IR,IADDR.IC].UQUAL;% QUAL           12432000
                COPE(UDICT[IADDR.IR,*],IADDR.IC+1,OOPDC,0);%NAME        12433000
$ OMIT 12433151, IF ADVANCED;                                           12433049
                ERROR(10);                            % LEVL            12433050
                ERROR(10);                            % QUAL            12433100
                ERROR(10);                            % NAME            12433150
                GO EXIT;                              % EXIT            12434000
            END INT1 CASES;                                             12435000
            IF BOOLEAN(MNEMON[ADDR]).ZACM THEN                          12436000
                GO OACC                                                 12437000
            ELSE                                                        12438000
                GO AGAIN;                                               12439000
        END INT1;                                                       12440000
    END MAKEACASEOFIT;                                                  12441000
    GO AGAIN;                                                           12442000
OACC:                                                                   12443000
    ACCM[0] ~ "     ";                                                  12444000
$ INCLUDE 12446001, IF DEBUGGING;                                       12444999
    IF MONITER OR ACMLST THEN                                           12445000
        BLANK(ACCM[1],0,13);                                            12446000
    HM ~ 3;                                                             12447000
    PACKLAST ~ FALSE;                                                   12447100
    GO AGAIN;                                                           12448000
EXIT:                                                                   12449000
        END;                                                            12450000
        PSEUDOREAD(OA,OHM-3+REAL(OHM=3),0);                             13000000
        RECALLIT;                                                       13001000
        MACT~ MACT-1;                                                   13002000
        IF MACT } 1 THEN BEGIN % RECALL STACKED MACRO                   13003000
            UNSTACKIT(256,LOCAL[0,*]);                                  13004000
            POS ~ (X~LOCAL[0,254]).LIM;                                 13005000
            A ~ LOCAL[0,255];                                           13006000
            READIT(LOCAL[2,*],A.MML,POS,MCODE);                         13007000
            READIT(LOCAL[1,*],A.MPOOL,POS,MCODE);                       13008000
            READIT(SPACEMAP,X.MSMAP,POS,MCODE);                         13009000
            FOR D~(A.MLACT+3).D4 STEP -1 UNTIL 7 DO BEGIN               13010000
                UNSTACKIT(256,LOCAL[D,*]);                              13011000
            END;                                                        13011200
            IF X.MLOCT ! 0 THEN BEGIN                                   13012000
                UNSTACKIT(A.MLOCT,LOCAL[6,*]);                          13013000
            END;                                                        13013200
            IF X~LOCAL[0,252].REF ! 0 THEN BEGIN                        13014000
                UNSTACKIT(X,LOCAL[3,*]);                                13015000
            END;                                                        13015200
            IF X~LOCAL[0,252].LIM ! 0 THEN BEGIN                        13016000
                UNSTACKIT(X,LOCAL[4,*]);                                13017000
            END;                                                        13017200
            IF X~LOCAL[0,253].REF ! 0 THEN BEGIN                        13018000
                UNSTACKIT(X,ACCM[*]);                                   13019000
            END;                                                        13019200
            IF X~LOCAL[0,253].LIM ! 0 THEN BEGIN                        13020000
                UNSTACKIT(X,OA[*]);                                     13021000
            END;                                                        13021200
        END;                                                            13022000
        ATOG~ TATOG;                                                    13023000
        TOGGLE ~ TTOG;                                                  13023050
$ INCLUDE 13024001, IF DEBUGGING;                                       13023999
        IF TRACE THEN TRACER(FALSE);                                    13024000
        INDENT ~ INDENT - 3;                                            13024050
        MADDR~ TMADDR;                                                  13025000
        FIELD ~ TFIELD;                                                 13026000
        HM ~ THM;                                                       13027000
        OHM ~ TOHM;                                                     13028000
    END MACRON;                                                         13029000
                                                                        13030000
END MACROINTERPRETER;                                                   13031000
COMMENT MACRO COMPILER SECTION. CODE IS EMITTED AS 3-CHR SYLLABLES.     15000000
    THE FIRST CHR IS THE OP CODE (RANGE 0->63). THE NEXT 2 CHR ARE      15001000
    THE ADDRESS FIELD (RANGE 0->4095);                                  15002000
PROCEDURE MACRO(Q); VALUE Q; REAL Q;                                    15003000
BEGIN                                                                   15004000
PROCEDURE MULTIASSIGNMENT(A,B,C,D); VALUE A,B,C,D;                      15005000
    INTEGER A,B,C; REAL D; FORWARD;                                     15006000
PROCEDURE ASSIGNMENT(O,A); VALUE O,A; INTEGER O,A; FORWARD;             15007000
PROCEDURE STATEMENT; FORWARD;                                           15008000
PROCEDURE IASSIGNMENT(O,A); VALUE O,A; INTEGER O,A; FORWARD;            15009000
        COMMENT  OPCODE DIALS INTO A CODE STRING & SPLITS NEXT          15010000
    INSTRUCTION INTO ITS OPCODE & ADDRESS COMPONENTS;                   15011000
STREAM PROCEDURE OPCODE(W,C,O,A);                                       15012000
    VALUE C;                                                            15013000
    BEGIN                                                               15014000
        DI~O; SI~W; SI~SI+C; DI~DI+7; DS~CHR;                           15015000
        DI~A; DS~6 LIT "0"; DS~2 CHR;                                   15016000
    END;                                                                15017000
        COMMENT  LASTCODE CHECKS PREVIOUS OPERATION FOR EQUALITY        15018000
    WITH PARAMETERS. TRUE USUALLY INDICATES THAT CODE OPTIMIZATION      15019000
    CAN TAKE PLACE;                                                     15020000
BOOLEAN PROCEDURE LASTCODE(OP,ADDR);                                    15021000
    VALUE ADDR, OP;                                                     15022000
    INTEGER OP, ADDR;                                                   15023000
    BEGIN                                                               15024000
        INTEGER LASTADDR, LASTOP;                                       15025000
        OPCODE(MC[(ML-3).WRD],(ML-3).CHR,LASTOP,LASTADDR);              15026000
        LASTCODE ~ LASTOP=OP AND LASTADDR=ADDR AND NOJUMP;              15027000
        NOJUMP~ TRUE;                                                   15028000
    END LASTCODE;                                                       15029000
        COMMENT  DOCODE IS CODE EMITTER. CHECKS ARE MADE TO SEE IF      15030000
    CERTAIN CONSTANT OPERANDS CAN BE MADE INTO LITERAL VALUES TO        15031000
    REDUCE EXECUTION TIME. DUPLICATE CONSTANTS ARE PREVENTED;           15032000
PROCEDURE DOCODE(C,A);                                                  15033000
    VALUE C, A;                                                         15034000
    INTEGER C, A;                                                       15035000
    BEGIN                                                               15036000
        STREAM PROCEDURE MACODE(O,A,W,C);                               15037000
        VALUE A,O,C;                                                    15038000
        BEGIN                                                           15039000
            DI~W; DI~DI+C; SI~LOC O; SI~SI+7; DS~CHR;                   15040000
            IF SC!"+" THEN BEGIN SI~SI+6; DS~2 CHR END;                 15041000
        END;                                                            15042000
        LABEL DOIT,DONT,ANYWAY;                                         15043000
        BOOLEAN B;                                                      15044000
        ALPHA F;                                                        15045000
        IF C < 0 THEN BEGIN                                             15046000
            C ~ ABS(C);                                                 15047000
            I ~ IF BOOLEAN(F~IDENT[1]).MFDG THEN                        15048000
                5 ELSE F.CTR;                                           15049000
            GO ANYWAY;                                                  15050000
        END;                                                            15051000
        IF NOT (B~BOOLEAN(MNEMON[C])).MCON THEN GO DOIT;                15052000
        IF A } CONSBAS THEN                                             15053000
            BEGIN                                                       15054000
                I ~ IDENT[1].CTR;                                       15055000
                IF A = CONSMAX THEN                                     15056000
                    BEGIN                                               15057000
                    IF NOT B.MINT THEN BEGIN                            15058000
                        IF I=0 THEN GO DONT;                            15059000
                        IF I > 2 THEN GO ANYWAY;                        15060000
                        C ~ IF I=2 THEN OLT2C + REAL(C=OOPDS)           15061000
                          ELSE IF C=OOPDS THEN OLT2C ELSE OLT1C;        15062000
                        IF I=1 AND C=OLT2C THEN                         15063000
                            A ~ IDENT[1].C3 & " "[TOC6]                 15064000
                        ELSE                                            15065000
                        A ~ IDENT[1].C34;                               15066000
                        GO DOIT;                                        15067000
                    END                                                 15068000
                    ELSE                                                15069000
                    IF I>8 THEN ERROR(84) ELSE                          15070000
                        IF A ~ CONSTANTCONVERT(IDENT[1],I)<4096 THEN    15071000
                        BEGIN                                           15072000
                            C ~ C-OIOPC+OILIT; % CHR TO LIT SYL         15073000
                            ML ~ ML - 3; % COVER CONV SYL               15074000
                            GO DOIT;                                    15075000
                        END;                                            15076000
                END;                                                    15077000
ANYWAY:         A ~ 0;                                                  15078000
                WHILE A<CONP DO BEGIN                                   15079000
                V ~ IF BOOLEAN(F~COMCON[A]).MFDG THEN                   15080000
                    5 ELSE F.CTR;                                       15081000
                    IF I = V THEN                                       15082000
                        IF COMPARE(IDENT[1],COMCON[A],I.C5,I            15083000
                          .C6,I)=1 THEN                                 15084000
                            A ~ A + CONSBAS                             15085000
                        ELSE                                            15086000
                            A ~ A+(V+10).WRD                            15087000
                    ELSE                                                15088000
                        A ~ A+(V+10).WRD;                               15089000
                END;                                                    15090000
                IF A < CONSBAS THEN                                     15091000
                    IF CONP+I~(I+10).WRD > 256 THEN                     15092000
                        BEGIN                                           15093000
                            ERROR(11);                                  15094000
                            A ~ CONSBAS;                                15095000
                        END                                             15096000
                    ELSE                                                15097000
                        BEGIN                                           15098000
                            A ~ CONSBAS+CONP;                           15099000
                            MOVE(I, IDENT[1], COMCON[CONP]);            15100000
                            CONP~ CONP+I                                15101000
                        END;                                            15102000
            END;                                                        15103000
DOIT:                                                                   15104000
        IF ML > MLMAX THEN                                              15105000
            BEGIN                                                       15106000
                ERROR(12);                                              15107000
                ML ~ 3;                                                 15108000
                MACODE(OINT1,OEXIT,MC[0],0);                            15109000
            END;                                                        15110000
        MACODE(C,A,MC[ML.WRD],ML.CHR);                                  15111000
$ INCLUDE 15115001, IF DEBUGGING;                                       15111999
        IF DBUGN THEN                                                   15112000
            WRITE(PRINTER,DEBUG,ML,F~MNEMON[C],                         15113000
                IF BOOLEAN(F).DBA THEN "A" ELSE "I",                    15114000
                IF BOOLEAN(F).DBMN THEN MNEMON[A] ELSE A);              15115000
        ML ~ ML+3;                                                      15116000
DONT:                                                                   15117000
    END;                                                                15118000
PROCEDURE RECOVER;                                                      15119000
    DO STEPI UNTIL (TYPE=0 AND (ELB=";" OR ELB=","))                    15120000
        OR (TYPE=VOTHERS AND (I~ELB.SUB=ENDV OR I=THENV                 15121000
        OR I=ELSEV));                                                   15122000
        COMMENT  CLNGENT COMPILES LENGTH & ENTIER FUNCTIONS. A CHECK    15200000
    IS MADE TO SEE IF PREVIOUS OPERATOR CLEARED ACCM. IF NOT A STACK    15201000
    AND UNSTACK IS REQUIRED TO SAVE ITS CONTENTS;                       15202000
PROCEDURE CLNGENT(OP); VALUE OP; INTEGER OP;                            15203000
    BEGIN                                                               15204000
    BOOLEAN P;                                                          15205000
    INTEGER W,O;                                                        15206000
        STEPI;                                                          15207000
        IF ELB!"(" THEN                                                 15208000
            ERROR(13);                                                  15209000
        DECOMTOG~ TRUE;                                                 15210000
        OPCODE(MC[(ML-3).WRD],(ML-3).CHR,W,O);                          15211000
       IF P~NOT(BOOLEAN(MNEMON[IF W=OINT1 THEN O ELSE W].ZACM)) THEN    15212000
           DOCODE(OINT1,OSTCK);                                         15213000
        MULTIASSIGNMENT(OINT1,0&OP[TOREF],0,0);                         15214000
        IF P THEN DOCODE(OINT1,ONSTK);                                  15215000
        DECOMTOG~ FALSE;                                                15216000
        IF ELB!")" THEN                                                 15217000
            ERROR(14);                                                  15218000
    END;                                                                15219000
$ INCLUDE 15450001, IF INTERMEDIATE;                                    15299999
        COMMENT  FIELDIT PROCESSES LEFTPART MFIELD DESIGNATORS.         15300000
    EXPLICIT PARTIAL ALPHA MSTRINGS ARE NOT ALLOWED. A DICTIONARY       15301000
    MSTRING OVERRIDES ANY OFFSET CARRIED WITH THE FIELD. A SIMPLE       15302000
    MSTRING SETS THE OFFSET TO ZERO;                                    15303000
REAL PROCEDURE FIELDIT(SAVA,TYP); VALUE SAVA,TYP;                       15304000
REAL SAVA,TYP;                                                          15305000
BEGIN                                                                   15306000
    REAL F;                                                             15307000
    LABEL EXIT;                                                         15308000
    IF STEPI!VMFIELDID AND TYPE!0 AND ELB!"[" THEN BEGIN                15309000
        ERROR(102);                                                     15309500
        T ~ T - 1;                                                      15310000
        GO EXIT;                                                        15311000
    END;                                                                15312000
    IF NOT(BOOLEAN(ELB.SUB) EQV BOOLEAN(TYP))AND TYP!6 THEN BEGIN       15313000
        ERROR(101); % MFIELD MODE DISAGREEMENT                          15314000
        GO EXIT;                                                        15315000
    END;                                                                15316000
    F ~ GET(LAD);                                                       15317000
    IF TYP = 6 THEN                                                     15318000
        ITOG ~ BOOLEAN(TEMP~ELB.SUB)                                    15319000
    ELSE                                                                15320000
    IF 4 { TYP { 5 THEN BEGIN                                           15321000
        F ~ F & SAVA.OFFSET[TOMFOFF] & 1[TOMFDCT];                      15321500
        IF TYP~SAVA.LIM-REAL(TYP=4) = 0 THEN TYP ~ 4095;                15322000
        F ~ F & TYP[TOMFLIM];                                           15322500
    END ELSE                                                            15323000
    IF TYP < 4 THEN                                                     15324000
        F ~ F & 0[TOMFOFF];                                             15325000
    FIELDIT ~ F;                                                        15326000
EXIT:                                                                   15327000
END FIELDIT;                                                            15328000
        COMMENT  PARTIALWORD COMPILES CODE FOR INLINE PARTIAL           15400000
    MSTRING REQUESTS. THE PROCESS CONSISTS OF COMPUTING A STARTING      15401000
    CHR & "DIALING" THE INFO IN, THEN COMPUTING THE # OF CHR TO         15402000
    TRANSFER & "SETTING LENGTH". SNAZZY PROGRAMMERS CAN OMIT            15403000
    COMPUTING THE DIAL BY ENTERING "*" FOR THE EXPRESSION. THE VALUE    15404000
    IS KEPT UNTIL AN MFIELD OR RECURSION ALTERS IT;                     15405000
PROCEDURE PARTIALWORD;                                                  15406000
    BEGIN                                                               15407000
        LABEL EXIT;                                                     15408000
        IF PWT THEN                                                     15409000
            ERROR(52);                                                  15410000
        PWT~ TRUE;                                                      15411000
        IF STEPI = VMFIELDID THEN BEGIN                                 15412000
            IF NOT(BOOLEAN(ELB.SUB)EQV ITOG)AND TEMP!6 THEN BEGIN       15413000
                ERROR(101); % MFIELD MODE DISAGREEMENT                  15414000
                RECOVER;                                                15415000
                T ~ T - 1;                                              15416000
                GO EXIT;                                                15417000
            END;                                                        15418000
            IDENT[1] ~ GET(LAD);                                        15419000
            IF 4 { TEMP { 5 THEN                                        15420000
                IDENT[1].MFOFF ~ SAVEA.OFFSET                           15421000
            ELSE                                                        15422000
            IF TEMP < 4 THEN                                            15423000
                IDENT[1].MFOFF ~ 0;                                     15424000
            DOCODE(-OCONV,0);                                           15425000
            GO EXIT;                                                    15426000
        END;                                                            15427000
        IF TYPE!VSPECIAL OR ELB!"[" THEN                                15428000
            BEGIN                                                       15429000
            ERROR(15);                                                  15430000
            RECOVER; T~ T-1; GO EXIT; END;                              15431000
        LENGTHTOG~ TRUE;                                                15432000
        IF STEPI=VSPECIAL AND ELB="*" THEN                              15433000
            STEPI                                                       15434000
        ELSE                                                            15435000
            BEGIN                                                       15436000
                T~ T-1;                                                 15437000
                IASSIGNMENT(OINT1,ODIAL);                               15438000
            END;                                                        15439000
        IF ELB!":" THEN                                                 15440000
            ERROR(16);                                                  15441000
        IASSIGNMENT(OSETL, 0);                                          15442000
        IF ELB!"]" THEN                                                 15443000
        BEGIN                                                           15444000
            ERROR(17);                                                  15445000
        T~ T-1; END;                                                    15446000
EXIT:                                                                   15447000
        LENGTHTOG~ FALSE;                                               15448000
        PWT~ FALSE;                                                     15449000
    END PARTIALWORD;                                                    15450000
        COMMENT  CHECKMSTRING CHECKS AND VERIFIES THE POSSIBLE          15500000
    FORMATS OF MSTRING SYNTAX. THE RESULT IS AN ADDRESS, LIMIT, &       15501000
    OFFSET IN SAVEA AND A SUBCLASS IN ITOG & TEMP. THE PARAMETER        15502000
    DETERMINES IF SUBFIELD CODE IS TO BE EMITTED IN-LINE;               15503000
PROCEDURE CHECKMSTRING(LEFT); VALUE LEFT; BOOLEAN LEFT;                 15504000
BEGIN                                                                   15505000
    LABEL EXIT;                                                         15506000
    ITOG ~ BOOLEAN(TEMP~ELB.SUB);                                       15507000
    SAVEA ~ GET(LAD);                                                   15508000
    CFIELD ~ 0;                                                         15509000
    IF STEPI = VSPECIAL THEN BEGIN                                      15510000
$ INCLUDE 15526001, IF ADVANCED;                                        15510999
        IF ELB = "(" THEN BEGIN % OFFSET TO DICTIONARY MPOINTER         15511000
            IF NOT(4{TEMP{5) THEN BEGIN                                 15512000
                ERROR(98); % NO DICTIONARY MSTRING                      15513000
                GO EXIT;                                                15514000
            END;                                                        15515000
            IF STEPI ! VSTRINGNAME OR ELB.SUB ! 6 THEN BEGIN            15516000
                ERROR(99); % NO DICTIONARY MPOINTER                     15517000
                GO EXIT;                                                15518000
            END;                                                        15519000
            SAVEA ~ SAVEA & GET(LAD)[REFTOREF];                         15520000
            IF STEPI ! VSPECIAL OR ELB ! ")" THEN BEGIN                 15521000
                ERROR(69);                                              15522000
                T ~ T - 1;                                              15523000
            END;                                                        15524000
            IF STEPI!VSPECIAL OR ELB!"." THEN BEGIN                     15524050
                CFIELD ~ -0 & 1[TOMFDCT] & SAVEA[TOMFLIM];              15524500
                T ~ T - 1;                                              15524600
                GO EXIT;                                                15524700
            END;                                                        15525000
        END DICTIONARY OFFSET;                                          15526000
$ INCLUDE 15535001, IF INTERMEDIATE;                                    15526999
        IF ELB = "." THEN BEGIN % PARTIAL FIELD DESIGNATOR              15527000
            IF LEFT THEN % MAKE DESCRIPTOR                              15528000
            CFIELD ~ FIELDIT(SAVEA,TEMP)                                15529000
            ELSE BEGIN % EMIT CODE                                      15530000
                CFIELD ~ 0;                                             15531000
                PARTIALWORD;                                            15532000
            END;                                                        15533000
            GO EXIT;                                                    15534000
        END PARTIAL FIELD;                                              15535000
    END;                                                                15536000
    T ~ T - 1;                                                          15537000
EXIT:                                                                   15538000
END CHECKMSTRING;                                                       15539000
        COMMENT  MULTIASSIGNMENT IS RESPONSIBLE FOR MODE AGREEMENTS     15600000
    BETWEEN ITEMS IN THE LEFT PART LIST EMITTING CONVERTS WHERE         15601000
    REQUIRED. ANALYSIS IS RECURSIVE TO PERMIT RIGHT TO LEFT             15602000
    ASSIGNMENTS. RIGHTMOST RECEIVER DETERMINES MODE OF SEQUENCE;        15603000
PROCEDURE MULTIASSIGNMENT(OP,ADDR,MODE,MFIELD);                         15604000
VALUE OP,ADDR,MODE,MFIELD;                                              15605000
INTEGER OP,ADDR,MODE; REAL MFIELD;                                      15606000
BEGIN                                                                   15607000
    LABEL EXIT;                                                         15608000
    INTEGER TOP,TADDR,TMODE,TFIELD,I;                                   15609000
    I ~ T;                                                              15610000
    IF STEPI=VSTRINGNAME THEN BEGIN                                     15611000
        CHECKMSTRING(TRUE);                                             15612000
        TADDR ~ SAVEA;                                                  15613000
        TMODE ~ TEMP;                                                   15614000
        TFIELD ~ CFIELD;                                                15615000
        IF STEPI=0 AND ELB="~" THEN BEGIN                               15616000
            TOP~ IF BOOLEAN(TMODE) THEN OISTR ELSE ONSTR;               15617000
            MULTIASSIGNMENT(TOP,TADDR,TMODE,TFIELD);                    15618000
            IF NOT(BOOLEAN(MODE) EQV BOOLEAN(TMODE)) THEN               15619000
                DOCODE(OCONV,1021-MODE.[47:1]);                         15620000
$ INCLUDE 15622001, IF ADVANCED;                                        15620999
            IF MODE}2 THEN                                              15621000
                DOCODE(OINDC,ADDR.REF);                                 15622000
$ INCLUDE 15624001, IF INTERMEDIATE;                                    15622999
            IF IDENT[1]~MFIELD ! 0 THEN                                 15623000
                DOCODE(-OCONV,0);                                       15624000
            DOCODE(OP,ADDR.REF);                                        15625000
            GO EXIT;                                                    15626000
            END;                                                        15627000
        END;                                                            15628000
    T ~ I;                                                              15629000
$ INCLUDE 15634001, IF ADVANCED;                                        15629999
    IF MODE}2 THEN                                                      15630000
        BEGIN                                                           15631000
        TOP~ OP;                                                        15632000
        OP~ OINDC;                                                      15633000
        END;                                                            15634000
    IF BOOLEAN(MODE) THEN                                               15635000
        IASSIGNMENT(OP,ADDR.REF)                                        15636000
    ELSE                                                                15637000
        ASSIGNMENT(OP,ADDR.REF);                                        15638000
$ INCLUDE 15638501, IF INTERMEDIATE;                                    15638099
    IF BOOLEAN(IDENT[1]~MFIELD).MFDG THEN BEGIN                         15638100
        ML ~ ML - 3;                                                    15638200
        DOCODE(-OCONV,0);                                               15638300
        DOCODE(OP,ADDR.REF);                                            15638400
    END;                                                                15638500
$ INCLUDE 15643001, IF ADVANCED;                                        15638999
    IF MODE } 2 AND NOT NOJUMP.[46:1] THEN BEGIN                        15639000
        DOCODE(TOP,ADDR.REF);                                           15642000
        END;                                                            15643000
EXIT:                                                                   15644000
END MULTIASSIGNMENT;                                                    15645000
        COMMENT  BOOLEANEX COMPILES THE CODE FOR THE BOOLEAN            16000000
    EXPRESSION OF A CONDITIONAL EXPRESSION. A CERTAIN AMOUNT OF         16001000
    OPTIMIZATION OCCURS DEPENDING ON THE LEFT & RIGHT SEQUENCES.        16002000
    MIXED MODE COMPARISONS RESULT IN INTEGER COMPARES FOR NUMERICAL     16003000
    ACCURACY. IF RIGHT SEQ IS SIMPLE MSTRING, DIRECT COMPARE TO         16004000
    MSTRING RESULTS. OTHERWISE ACCM IS MOVE TO JUNK & REVERSE           16005000
    COMPARISON IS MADE WITH JUNK AFTER RIGHT SEQ IS EVALUATED;          16006000
PROCEDURE BOOLEANEX;                                                    16007000
    BEGIN                                                               16008000
    LABEL NEQ,LSS,LEQ,GTR,GEQ,EQL,IAS,AS,EXIT,RIGHTPART,E,IAS1,AS1,     16009000
        AS2;                                                            16010000
    BOOLEAN INTEGERCOMPARE;                                             16011000
        INTEGER OP;                                                     16012000
    INTEGER I;                                                          16013000
    SWITCH SW~GTR,GEQ,E,E,E,E,E,E,E,E,E,E,E,E,E,E,LSS,E,E,E,E,E,E,E,    16014000
                E,E,E,E,E,E,E,E,E,LEQ,E,E,E,E,E,E,E,E,E,E,E,E,NEQ,      16015000
                EQL;                                                    16016000
    BOOLEANEXTOG~ TRUE;                                                 16017000
    I ~ T;                                                              16018000
    IF STEPI=VSTRINGNAME THEN BEGIN                                     16019000
        CHECKMSTRING(TRUE);                                             16020000
        IF ITOG THEN % INTEGER MSTRING OR MPOINTER                      16021000
            BEGIN                                                       16022000
            IF STEPI=0 AND ELB="~" THEN                                 16023000
                MULTIASSIGNMENT(OISTR,SAVEA,TEMP,CFIELD)                16024000
            ELSE BEGIN                                                  16025000
                T ~ I;                                                  16026000
IAS:            IASSIGNMENT(0,0);                                       16027000
                ML~ ML-3;                                               16028000
                END;                                                    16029000
            INTEGERCOMPARE~ TRUE;                                       16030000
            END                                                         16031000
        ELSE BEGIN                                                      16032000
            IF STEPI=0 AND ELB="~" THEN                                 16033000
                MULTIASSIGNMENT(ONSTR,SAVEA,TEMP,CFIELD)                16034000
            ELSE BEGIN                                                  16035000
                T ~ I;                                                  16036000
AS:             ASSIGNMENT(0,0);                                        16037000
                ML~ ML-3;                                               16038000
                END;                                                    16039000
            END                                                         16040000
        END ELSE BEGIN                                                  16041000
            T~ T-1;                                                     16042000
            IF TYPE=VNUMBER THEN                                        16043000
                GO IAS;                                                 16044000
            IF TYPE=VMACRORES THEN BEGIN                                16045000
                IF (I~ELB.SUB) = TOGGLEM THEN BEGIN                     16046000
$ INCLUDE 16047401, IF ADVANCED;                                        16046099
                IF STEPI=VSPECIAL AND ELB="(" THEN BEGIN                16046100
                    LABEL ER;                                           16046200
                    IF STEPI=VNUMBER AND ELB.SUB=INTEGERV THEN BEGIN    16046300
                        IF I~CONSTANTCONVERT(IDENT[1],COUNT)>35 THEN    16046350
                        BEGIN ERROR(108); GO ER END;                    16046400
                        DOCODE(OTOGL,4|I+258);                          16046450
                    END ELSE BEGIN                                      16046500
                    IF TYPE!VMACRORES OR I~ELB.SUB<200 THEN BEGIN       16046550
                        ERROR(42);                                      16046600
ER:                     RECOVER;                                        16046650
                        GO EXIT;                                        16046700
                    END;                                                16046750
                    DOCODE(OTOGL,(I-200)|3+2);                          16046800
                    IF STEPI!VSPECIAL OR ELB!")" THEN BEGIN             16046900
                        ERROR(69);                                      16047000
                        GO ER;                                          16047100
                    END;                                                16047200
                    T ~ T + 1;                                          16047300
                    END END;                                            16047400
$ OMIT 16047501, IF ADVANCED;                                           16047499
                    T ~ T + 1;                                          16047500
                    GO EXIT;                                            16048000
                END ELSE                                                16049000
$ INCLUDE 16049471, IF ADVANCED;                                        16049009
                IF I = EQUIVM THEN BEGIN                                16049010
                    LABEL ER;                                           16049020
                    IF STEPI!VSPECIAL OR ELB!"(" THEN BEGIN             16049030
                        ERROR(68);                                      16049040
ER:                     RECOVER;                                        16049050
                        GO EXIT;                                        16049060
                    END;                                                16049070
                    IF STEPI!VSTRINGNAME OR (ELB.SUB).[46:1]=0 THEN     16049080
                        BEGIN ERROR(104); GO ER; END;                   16049090
                    DOCODE(OIOPC,GET(LAD).REF);                         16049100
                    DOCODE(OCONV,1022);                                 16049110
                    IF STEPI!VSPECIAL OR ELB!"," THEN BEGIN             16049120
                        ERROR(34);                                      16049130
                        GO ER;                                          16049140
                    END;                                                16049150
                    IF STEPI!VSTRINGNAME OR (ELB.SUB).[46:1]=0 THEN     16049160
                        BEGIN ERROR(104); GO ER; END;                   16049170
                    DOCODE(OIOPC,GET(LAD).REF);                         16049180
                    DOCODE(OICMP,6);                                    16049190
                    IF STEPI!VSPECIAL OR ELB!")" THEN BEGIN             16049200
                        ERROR(69);                                      16049210
                        GO ER;                                          16049220
                    END;                                                16049230
                    T ~ T + 1;                                          16049240
                    GO EXIT;                                            16049250
                END ELSE                                                16049260
                IF I = NULLM THEN BEGIN                                 16049270
                    LABEL ER;                                           16049280
                    BOOLEAN B;                                          16049290
                    IF B~(STEPI=VSPECIAL AND ELB="(") THEN STEPI;       16049300
                    IF TYPE!VSTRINGNAME OR (ELB.SUB).[46:1]=0 THEN      16049310
                    BEGIN ERROR(104);                                   16049320
ER:                     RECOVER;                                        16049330
                        GO EXIT;                                        16049340
                    END;                                                16049350
                    DOCODE(OIOPC,GET(LAD).REF);                         16049360
                    DOCODE(OCONV,1022);                                 16049370
                    DOCODE(OILIT,0);                                    16049380
                    DOCODE(OICMP,6);                                    16049390
                    IF B THEN                                           16049400
                      IF STEPI!VSPECIAL OR ELB!")" THEN BEGIN           16049410
                        ERROR(69);                                      16049420
                        GO ER;                                          16049430
                      END;                                              16049440
                    T ~ T + 1;                                          16049450
                    GO EXIT;                                            16049460
                END ELSE                                                16049470
                IF 50{I{74 OR 125{I{149 THEN                            16050000
                    GO IAS                                              16051000
                ELSE                                                    16052000
                IF 75{I{124 THEN                                        16053000
                GO AS;                                                  16054000
                ERROR(42);                                              16055000
                STEPI;                                                  16056000
            END;                                                        16057000
    GO AS;                                                              16058000
        END;                                                            16059000
        IF TYPE =VOTHERS THEN BEGIN                                     16060000
            IF ELB.SUB=THENV THEN BEGIN                                 16061000
                DOCODE(OINT1,ODELE);                                    16062000
                T~ T-1;                                                 16063000
                IF INTEGERCOMPARE THEN                                  16063800
                    IF TOGGLECT = 0 THEN                                16063850
                        ERROR(19)                                       16063900
                ELSE ELSE                                               16063950
                IF TOGGLECT > 1 THEN                                    16064000
                    ERROR(-70);                                         16065000
                GO EXIT;                                                16066000
            END;                                                        16067000
            GO E;                                                       16068000
            END ;                                                       16069000
        IF TYPE! 0 THEN GO E;                                           16070000
        GO SW[ELB-13];                                                  16071000
E:      ERROR(19);                                                      16072000
EQL:    OP~0; GO RIGHTPART;                                             16073000
NEQ:    OP~1; GO RIGHTPART;                                             16074000
LSS:    OP~2; GO RIGHTPART;                                             16075000
LEQ:    OP~3; GO RIGHTPART;                                             16076000
GTR:    OP~4; GO RIGHTPART;                                             16077000
GEQ:    OP~5;                                                           16078000
RIGHTPART:                                                              16079000
        IF STEPI=VSTRINGNAME THEN BEGIN                                 16080000
            I ~ T - 1;                                                  16081000
            CHECKMSTRING(TRUE);                                         16082000
            IF ITOG THEN BEGIN                                          16083000
                DOCODE(OCONV,1023-REAL(INTEGERCOMPARE));                16084000
                IF STEPI=0 AND ELB="~" THEN BEGIN                       16085000
                    MULTIASSIGNMENT(OISTR,SAVEA,TEMP,CFIELD);           16086000
                    T~ T-1;                                             16087000
                    DOCODE(OICMP,OP);                                   16088000
                    GO EXIT;                                            16089000
                    END                                                 16090000
                ELSE BEGIN                                              16091000
                    T ~ I;                                              16092000
IAS1:               IASSIGNMENT(OICMP,OP);                              16093000
                    T~ T-1;                                             16094000
                    GO EXIT;                                            16095000
                    END;                                                16096000
                END                                                     16097000
            ELSE BEGIN                                                  16098000
                IF INTEGERCOMPARE THEN BEGIN                            16099000
                    DOCODE(OCONV,1022);                                 16100000
                    IF STEPI=0 AND ELB="~" THEN BEGIN                   16101000
                        MULTIASSIGNMENT(ONSTR,SAVEA,TEMP,CFIELD);       16102000
                        DOCODE(OICMP,OP+64);                            16103000
                        T~ T-1;                                         16104000
                        GO EXIT;                                        16105000
                        END                                             16106000
                    ELSE BEGIN                                          16107000
                        T ~ I;                                          16108000
AS1:                    ASSIGNMENT(OICMP,OP+64);                        16109000
                        T~ T-1;                                         16110000
                        GO EXIT;                                        16111000
                        END                                             16112000
                END ELSE                                                16113000
                    IF STEPI=VOTHERS AND ELB.SUB= THENV THEN BEGIN      16114000
                        DOCODE(RELOP[OP],SAVEA.REF);                    16115000
                        T~ T-1;                                         16116000
                        GO EXIT;                                        16117000
                        END                                             16118000
                    ELSE BEGIN                                          16119000
                        DOCODE(ODSTR,12);                               16120000
                        IF TYPE=0 AND ELB="~" THEN BEGIN                16121000
                           MULTIASSIGNMENT(ONSTR,SAVEA,TEMP,CFIELD);    16122000
                            DOCODE(RELOP[OP+CREV],12);                  16123000
                            T~ T-1;                                     16124000
                            GO EXIT;                                    16125000
                            END;                                        16126000
                        T ~ I;                                          16127000
AS2:                    ASSIGNMENT(RELOP[OP+CREV],12);                  16128000
                        T~ T-1;                                         16129000
                        GO EXIT;                                        16130000
                        END;                                            16131000
            END END VSTRINGNAME;                                        16131100
        IF TYPE = VSTRING THEN                                          16132000
            IF STEPI = VOTHERS AND ELB.SUB = THENV THEN BEGIN           16133000
                T ~ T - 2;                                              16134000
                STEPI; % REGAIN ALPHA                                   16135000
                DOCODE(-RELOP[OP],CONSMAX);                             16136000
                GO EXIT;                                                16137000
            END ELSE T ~ T - 1;                                         16138000
%           END END;                                                    16139000
        T~ T-1;                                                         16140000
        IF TYPE = VNUMBER OR (TYPE = VMACRORES AND                      16141000
          (50{I~ELB.SUB{74 OR 125{I{149)) THEN BEGIN                    16142000
            DOCODE(OCONV,1023-REAL(INTEGERCOMPARE));                    16143000
            GO IAS1;                                                    16144000
        END;                                                            16145000
        IF INTEGERCOMPARE THEN BEGIN                                    16146000
            DOCODE(OCONV,1022);                                         16147000
            GO AS1;                                                     16148000
            END;                                                        16149000
        DOCODE(ODSTR,12);                                               16150000
        GO AS2;                                                         16151000
EXIT:   BOOLEANEXTOG~ FALSE;                                            16152000
END BOOLEANEX;                                                          16153000
        COMMENT  CONDITIONAL COMPILES THE CODE FOR -IF- STMTS BY        16200000
    DIRECTING OTHER COMPILER SECTIONS TO GAIN RECURSIVE ABILITY.        16201000
    OPTIMIZATION FOR MISSING THEN STATEMENTS IS ATTEMPTED, BUT THIS     16202000
    RESTRICTS MACRO ADDRESSING TO 2047 CHARACTERS OF CODE;              16203000
PROCEDURE CONDITIONAL;                                                  16204000
    BEGIN                                                               16205000
        INTEGER ADDR, D;                                                16206000
        LABEL ELSEIT,NOTQUITE;                                          16207000
        BOOLEAN CT,B;                                                   16208000
        INTEGER OP;                                                     16209000
        CT ~ COND;                                                      16210000
        COND ~ TRUE;                                                    16211000
        BOOLEANEX;                                                      16212000
        D~ ML;                                                          16213000
        DOCODE(OJMPC,0);                                                16214000
        IF STEPI!VOTHERS OR ELB.SUB!THENV THEN                          16215000
            ERROR(25);                                                  16216000
        STEPI;                                                          16217000
        STATEMENT;                                                      16218000
        NOJUMP~ FALSE;                                                  16219000
        OP ~ OJUMP;                                                     16220000
        IF B~(D+3=ML AND ELB.SUB=ELSEV) THEN BEGIN % THEN-ELSE          16221000
            OP ~ OJMPC;                                                 16222000
            GO ELSEIT;                                                  16223000
        END ELSE                                                        16224000
        IF D+6 = ML THEN BEGIN % CHECK FOR SIMPLE GO TO                 16225000
            OPCODE(MC[(ML-3).WRD],(ML-3).CHR,I,ADDR);                   16226000
            IF I = OJUMP AND ADDR < MLMAX THEN BEGIN % NON-FWD JUMP     16227000
                B ~ TRUE;                                               16228000
                D ~ MLMAX;                                              16229000
                ML ~ ML - 6;                                            16230000
                DOCODE(OJMPC,ADDR+2048);                                16231000
            END ELSE GO NOTQUITE;                                       16232000
        END ELSE BEGIN % NORMAL JUMP CONDITIONAL                        16233000
NOTQUITE:                                                               16234000
            ADDR ~ REAL(ELB.SUB=ELSEV)|3 + ML;                          16235000
            ML ~ D;                                                     16236000
            DOCODE(OJMPC,ADDR);                                         16237000
            ML ~ ADDR;                                                  16238000
        END;                                                            16239000
        IF ELB.SUB=ELSEV THEN                                           16240000
            BEGIN                                                       16241000
                IF NOT B THEN D ~ ML-3;                                 16242000
ELSEIT:         STEPI;                                                  16243000
                STATEMENT;                                              16244000
                NOJUMP~ FALSE;                                          16245000
                ADDR ~ ML;                                              16246000
                ML ~ D;                                                 16247000
                DOCODE(OP,REAL(OP=OJMPC)|2048+ADDR);                    16248000
                ML ~ ADDR;                                              16249000
            END;                                                        16250000
    COND~ CT;                                                           16251000
    END CONDITIONAL;                                                    16252000
        COMMENT  UNCONDITIONAL GENERATES CODE FOR -GO TO- STMTS.        16300000
    LABELS WHICH HAVE NOT BEEN PLACED NOR DECLARED ARE DECLARED.        16301000
    IF NOT PLACED, THE JUMP SYLLABLES ARE LINKED WITH THE LAST          16302000
    CODE SYLLABLE LINK PLACED IN WORD[N+1] OF THE DICT ENTRY;           16303000
PROCEDURE UNCONDITIONAL;                                                16304000
    BEGIN                                                               16305000
        INTEGER ADDR;                                                   16306000
        IF STEPI=VOTHERS AND ELB.SUB=TOV THEN                           16307000
            STEPI;                                                      16308000
        IF TYPE!VALGOLIDENT OR ELB.SUB!LABELV OR MCBL>ELB.BLKLVL        16309000
            THEN BEGIN                                                  16310000
                PUT(LAD~GET(ADDICT(VALGOLIDENT,LABELV)).ADINFO,-0);     16311000
                NEXTDICT~ NEXTDICT+1;                                   16312000
            END;                                                        16313000
        IF BOOLEAN(ADDR~GET(LAD)).PNOTT THEN                            16314000
            PUT(LAD,-ML);                                               16315000
        DOCODE(OJUMP,ABS(ADDR)&ADDR[36:1:1]);                           16316000
        STEPI                                                           16317000
    END UNCONDITIONAL;                                                  16318000
$ INCLUDE 16508001, IF ADVANCED;                                        16399999
        COMMENT  USERDICT COMPILES THE LOAD-FOR-TEST SEQUENCE FOR       16400000
    ENTER, FIND, & SEARCH. QUALIFICATION IS PERFORMED                   16401000
    BY RECURSION;                                                       16402000
PROCEDURE USERDICT(CODE,AD); VALUE CODE,AD; INTEGER CODE,AD;            16403000
BEGIN                                                                   16404000
    INTEGER ENDAD,ENDFIELD,F,TT;                                        16405000
    LABEL ERR,EXIT;                                                     16406000
    IF CODE = OSRCH THEN BEGIN                                          16407000
        IF STEPI ! VSPECIAL OR ELB ! "(" THEN BEGIN                     16408000
            ERROR(68);                                                  16409000
            GO ERR;                                                     16410000
        END;                                                            16411000
        IF STEPI ! VSTRINGNAME OR ELB.SUB ! 6 THEN BEGIN                16412000
            ERROR(99);                                                  16413000
            GO ERR;                                                     16414000
        END;                                                            16415000
        DOCODE(OIOPC,GET(LAD).REF);                                     16416000
        DOCODE(OLDFT,1);                                                16417000
        IF STEPI ! VSPECIAL OR ")"!ELB!"," THEN BEGIN                   16418000
            ERROR(34);                                                  16419000
            GO ERR;                                                     16420000
        END;                                                            16421000
        IF ELB = "," THEN BEGIN                                         16422000
            IF STEPI ! VSTRINGNAME OR BOOLEAN(ELB.SUB) THEN BEGIN       16423000
                ERROR(37);                                              16424000
                GO ERR;                                                 16425000
            END;                                                        16426000
            CHECKMSTRING(TRUE);                                         16427000
            ENDAD ~ SAVEA & TEMP[TOLIM];                                16428000
            ENDFIELD ~ CFIELD;                                          16429000
            IF STEPI ! VSPECIAL OR ELB ! ")" THEN BEGIN                 16430000
                ERROR(69);                                              16431000
                GO ERR;                                                 16432000
            END;                                                        16433000
        END COMMACHECK;                                                 16434000
    END SEARCH HANDLING ELSE                                            16435000
    IF CODE = OENTR THEN BEGIN                                          16436000
        IF STEPI ! VSPECIAL OR ELB ! "(" THEN BEGIN                     16437000
            ERROR(68);                                                  16438000
            GO ERR;                                                     16439000
        END;                                                            16440000
        IASSIGNMENT(OLDFT,1);                                           16441000
        IF TYPE ! VSPECIAL OR ELB ! ")" THEN BEGIN                      16442000
            ERROR(69);                                                  16443000
            GO ERR;                                                     16444000
        END;                                                            16445000
    END;                                                                16446000
    ASSIGNMENT(OLDFT,0);                                                16447000
    WHILE STEPI = VSPECIAL AND ELB = "(" DO BEGIN                       16448000
        F ~ 0;                                                          16449000
        IF STEPI = VSTRINGNAME AND (4{TEMP~ELB.SUB{5) THEN BEGIN        16450000
            A ~ GET(LAD).OFFSET;                                        16451000
            IF STEPI ! VSPECIAL OR ELB ! "." THEN BEGIN                 16452000
                ERROR(81);                                              16453000
                GO ERR;                                                 16454000
            END;                                                        16455000
            IF STEPI ! VMFIELDID THEN BEGIN                             16456000
                    ERROR(102); % MISSING MFIELD DESIGNATOR             16457000
                    GO ERR;                                             16458000
            END;                                                        16459000
            F ~ GET(LAD) & A[TOMFOFF];                                  16460000
        END ELSE                                                        16461000
        IF TYPE = VMFIELDID THEN                                        16462000
            F ~ GET(LAD)                                                16463000
        ELSE BEGIN                                                      16464000
ERR:        DO RECOVER UNTIL TYPE!0 OR ELB!",";                         16465000
            GO EXIT;                                                    16466000
        END;                                                            16467000
        IF STEPI ! VSPECIAL OR ELB ! "=" THEN BEGIN                     16468000
            ERROR(61);                                                  16469000
            GO ERR;                                                     16470000
        END;                                                            16471000
        IF BOOLEAN(TT~TEMP) THEN                                        16472000
            IASSIGNMENT(0,0)                                            16473000
        ELSE                                                            16474000
            ASSIGNMENT(0,0);                                            16475000
        ML ~ ML - 3;                                                    16476000
        IDENT[1] ~ F & 1[TOMFVN];                                       16477000
        DOCODE(-OCONV,0);                                               16478000
        DOCODE(OLDFT,TT.[47:1]);                                        16479000
        IF STEPI ! VSPECIAL OR ELB ! ")" THEN BEGIN                     16480000
            ERROR(69);                                                  16481000
            GO ERR;                                                     16482000
        END;                                                            16483000
    END FIELD CHECKS;                                                   16484000
    T ~ T - 1;                                                          16485000
    WHILE STEPI = VSPECIAL AND ELB = "[" DO BEGIN                       16486000
        USERDICT(0,0);                                                  16487000
        IF STEPI ! VSPECIAL OR ELB ! "]" THEN BEGIN                     16488000
            ERROR(36);                                                  16489000
            GO ERR;                                                     16490000
        END;                                                            16491000
    END QUALIFIER SEQUENCE;                                             16492000
    IF CODE = 0 THEN BEGIN % RECURSED                                   16493000
        T ~ T - 1;                                                      16494000
        GO EXIT;                                                        16495000
    END;                                                                16496000
    DOCODE(CODE,AD);                                                    16497000
    IF CODE = OSRCH THEN                                                16498000
        IF ENDAD ! 0 THEN BEGIN                                         16499000
            IF BOOLEAN(ENDAD).[46:1] THEN                               16500000
                DOCODE(OINDC,ENDAD.REF);                                16501000
            IF IDENT[1]~ENDFIELD ! 0 THEN                               16502000
                DOCODE(-OCONV,0);                                       16503000
            DOCODE(ODSTR,ENDAD.REF);                                    16504000
        END ELSE                                                        16505000
        DOCODE(OINT1,ODELE);                                            16506000
EXIT:                                                                   16507000
END USERDICT;                                                           16508000
        COMMENT  IASSIGNMENT IS RESPONSIBLE FOR COMPILING CODE FOR      16509000
    ANYTHING IN AN INTEGER SEQUENCE. AFTER COMPILING THE CODE, THE      16510000
    PARAMETERS ARE EMITTED TO EFFECT THE ACTION OF THE STATEMENT;       16511000
PROCEDURE IASSIGNMENT(O,AD);                                            16512000
    VALUE O, AD;                                                        16513000
    INTEGER O, AD;                                                      16514000
    BEGIN                                                               16515000
        LABEL AGAIN,EXIT,DONE,SKIP,LEVL,QUAL;                           16516000
        LABEL RSRV,RSRVSTMT,ENTR,FIND,SRCH,LINE,NTIER,LNGTH,            16517000
          BGN,BLK,SCT,NUMBER,INTGER,MATH,INT,OOPS,E;                    16518000
        SWITCH ISTMT ~ RSRV,RSRVSTMT,ENTR,FIND,SRCH,LEVL,QUAL;          16519000
        SWITCH IANDA ~ LINE,NTIER,LNGTH,E,E,E,E,E,E,E,BGN,BLK,SCT;      16520000
        SWITCH ISCAN ~ NUMBER,INTGER;                                   16521000
        INTEGER OP,ADDR;                                                16522000
$ INCLUDE 16523001, IF ADVANCED;                                        16522999
        BOOLEAN RST;                                                    16523000
        LABEL DC;                                                       16524000
        TOGGLECT ~ 0;                                                   16524050
        OP ~ OIOPC;                                                     16525000
AGAIN:  IF STEPI = VMACRORES THEN BEGIN                                 16526000
            IF 25 { I~ELB.SUB { 49 THEN                                 16527000
                GO ISTMT[I-24]                                          16528000
            ELSE                                                        16529000
            IF 50 { I { 74 THEN                                         16530000
                GO IANDA[I-49]                                          16531000
            ELSE                                                        16532000
            IF 125 { I { 149 THEN                                       16533000
                GO ISCAN[I-124];                                        16534000
OOPS: E:    ERROR(42);                                                  16535000
            DO RECOVER UNTIL TYPE!0 OR ELB!",";                         16536000
            GO EXIT;                                                    16537000
RSRVSTMT:                                                               16538000
$ INCLUDE 16544001, IF ADVANCED;                                        16538999
            IF RST ~ LPTRBAS { AD { LPTRMAX THEN                        16539000
                AD ~ AD - LPTRBAS + LINTBAS                             16540000
            ELSE                                                        16541000
            IF RST ~ GPTRBAS { AD { GPTRMAX THEN                        16542000
                AD ~ AD - GPTRBAS + GINTBAS                             16543000
            ELSE GO OOPS;                                               16544000
RSRV:                                                                   16545000
$ INCLUDE 16555001, IF ADVANCED;                                        16545999
            IF NOT RST THEN                                             16546000
                IF NOT(LPTRBAS{AD{LPTRMAX OR GPTRBAS{AD{GPTRMAX)THEN    16547000
                    GO OOPS;                                            16548000
            IF STEPI ! 0 OR ELB ! "(" THEN                              16549000
                ERROR(68);                                              16550000
            IASSIGNMENT(OSETL,AD);                                      16551000
            IF TYPE ! 0 OR ELB ! ")" THEN                               16552000
                ERROR(69);                                              16553000
            STEPI;                                                      16554000
            NOJUMP ~ <2>;                                               16554100
            GO EXIT;                                                    16555000
ENTR: FIND: SRCH:                                                       16556000
$ INCLUDE 16558001, IF ADVANCED;                                        16556999
            USERDICT(I-ENTERM+OENTR,AD);                                16557000
        TOGGLECT ~ TOGGLECT + 1;                                        16557050
            NOJUMP ~ <2>;                                               16557100
            GO EXIT;                                                    16558000
LEVL: QUAL:                                                             16559000
$ INCLUDE 16568001, IF ADVANCED;                                        16559999
            IF STEPI ! VSPECIAL OR ELB ! "(" THEN GO OOPS;              16560000
            IF STEPI ! VSTRINGNAME OR ELB.SUB ! 6 THEN BEGIN            16561000
                ERROR(99);                                              16562000
                GO EXIT;                                                16563000
            END;                                                        16564000
            DOCODE(OINDC,GET(LAD).REF);                                 16565000
            DOCODE(OINT1,IF I=LEVELM THEN OLEVL ELSE OQUAL);            16566000
            IF STEPI ! VSPECIAL OR ELB ! ")" THEN GO OOPS;              16567000
            GO MATH;                                                    16568000
$ OMIT 16568151, IF ADVANCED;                                           16568049
            ERROR(111);                                                 16568050
            RECOVER;                                                    16568100
            GO EXIT;                                                    16568150
LINE:       DOCODE(OINT1,OLINC);                                        16569000
MATH:       DOCODE(OCONV,1020-OP+OIOPC);                                16570000
            GO SKIP;                                                    16571000
$ INCLUDE 16577001, IF INTERMEDIATE;                                    16571999
            IF TRUE THEN BEGIN                                          16572000
NTIER:          CLNGENT(OENTI);                                         16572500
                TOGGLECT ~ TOGGLECT + 1;                                16573000
            END ELSE                                                    16574000
LNGTH:          CLNGENT(OLENG);                                         16575000
            DOCODE(OCONV,1015-OP+OIOPC);                                16576000
            GO SKIP;                                                    16577000
$ OMIT 16577101, IF INTERMEDIATE;                                       16577049
NTIER: LNGTH:                                                           16577050
            GO OOPS;                                                    16577100
BGN:        ADDR ~ OBGNL + OP - OIOPC;                                  16578000
            GO INT;                                                     16579000
BLK:        ADDR ~ OBLKL + OP - OIOPC;                                  16580000
            GO INT;                                                     16581000
SCT:        ADDR ~ OSCNT + OP - OIOPC;                                  16582000
INT:        DOCODE(OINT1,ADDR);                                         16583000
            GO SKIP;                                                    16584000
INTGER: NUMBER:                                                         16585000
$ OMIT 16585051, IF INTERMEDIATE;                                       16585049
            IF I=INTGERM THEN GO OOPS;                                  16585050
            DOCODE(OTYPC,I-100);                                        16586000
            TOGGLECT ~ TOGGLECT + 1;                                    16586050
            GO MATH;                                                    16587000
        END VMACRORES;                                                  16588000
        IF TYPE = VSTRINGNAME THEN                                      16589000
            CHECKMSTRING(FALSE) ELSE                                    16590000
        IF TYPE ! VNUMBER AND (TYPE!VSTRINGNAME OR NOT ITOG) THEN       16591000
            BEGIN                                                       16592000
                T~ T-1;                                                 16593000
                ERROR(27);                                              16594000
                DO RECOVER UNTIL TYPE!0 OR ELB!",";                     16595000
                GO EXIT;                                                16596000
            END;                                                        16597000
        IF TYPE=VNUMBER THEN                                            16598000
            BEGIN                                                       16599000
                DOCODE(OCONV, 0);                                       16600000
                ADDR ~ CONSMAX;                                         16601000
            END                                                         16602000
        ELSE                                                            16603000
            BEGIN                                                       16604000
                ADDR ~ SAVEA.REF;                                       16605000
                IF TEMP } 2 THEN                                        16606000
                    DOCODE(OINDC,ADDR)                                  16607000
                ELSE                                                    16608000
                IF OP = OIOPC THEN                                      16609000
                    IF LASTCODE(OISTR,ADDR) THEN                        16610000
                        GO SKIP;                                        16611000
            END;                                                        16612000
        DOCODE(OP, ADDR);                                               16613000
SKIP:   IF STEPI=VOTHERS THEN                                           16614000
            IF I~ELB.SUB=ENDV OR I=ELSEV OR                             16615000
                I=THENV AND BOOLEANEXTOG THEN                           16616000
                GO DONE;                                                16617000
        IF TYPE!VSPECIAL THEN                                           16618000
            BEGIN                                                       16619000
                DO RECOVER UNTIL TYPE!0 OR ELB!",";                     16620000
                ERROR(28);                                              16621000
                GO DONE;                                                16622000
            END;                                                        16623000
        IF LENGTHTOG THEN                                               16624000
            BEGIN                                                       16625000
                IF ELB=":" OR ELB="]" THEN                              16626000
                    GO DONE;                                            16627000
            END                                                         16628000
        ELSE                                                            16629000
            IF ELB=";" THEN                                             16630000
                GO DONE;                                                16631000
        IF ELB="-" THEN                                                 16632000
            OP~ OSUBI                                                   16633000
        ELSE                                                            16634000
        IF ELB="+" THEN                                                 16635000
            OP ~ OADDI                                                  16636000
        ELSE                                                            16637000
        IF ELB="|" THEN                                                 16638000
            OP ~ OMULI                                                  16639000
        ELSE                                                            16640000
        IF ELB="/" THEN                                                 16641000
            OP ~ ODIVI                                                  16642000
        ELSE                                                            16643000
            GO DONE;                                                    16644000
        GO AGAIN;                                                       16645000
DONE:   DOCODE(O, AD);                                                  16646000
        NOJUMP~ TRUE;                                                   16647000
EXIT:                                                                   16648000
    END IASSIGNMENT;                                                    16649000
        COMMENT  ASSIGNMENT IS RESPONSIBLE FOR COMPILING ALPHA          16700000
    SEQUENCES AND EMITTING THE ACTION PARAMETERS. A CERTAIN AMOUNT      16701000
    OF OPTIMIZATION IS ATTEMPTED;                                       16702000
PROCEDURE ASSIGNMENT(O,AD);                                             16703000
    VALUE O, AD;                                                        16704000
    INTEGER O, AD;                                                      16705000
    BEGIN                                                               16706000
        BOOLEAN CAT;                                                    16707000
        INTEGER OP, ADDR;                                               16708000
        INTEGER I;                                                      16709000
        LABEL STRNGNM,ER,MCRRSWD,AGAIN,TEND,EXIT,SEND,CEND,DONE,        16710000
          THRU,SUNTH,ENDSUN,LIN,BGN,BLK,SCT,INT,                        16711000
          UNTL,ENTI,LENG,QU,NAM;                                        16712000
        SWITCH SWAM ~ LIN,ENTI,LENG,ER,ER,ER,ER,ER,ER,ER,BGN,BLK,       16713000
          SCT,ER,ER,ER,ER,ER,ER,ER,ER,ER,ER,ER,ER,UNTL,THRU,QU,         16714000
          NAM;                                                          16715000
        CAT~ TRUE;                                                      16716000
        TOGGLECT ~ 0;                                                   16717000
AGAIN:  IF STEPI=VMACRORES THEN                                         16718000
            GO MCRRSWD;                                                 16719000
        IF TYPE=VSTRINGNAME THEN                                        16720000
            GO STRNGNM;                                                 16721000
        IF VNUMBER { TYPE { VTORF THEN                                  16722000
            BEGIN                                                       16723000
                ADDR ~ CONSMAX;                                         16724000
                GO SEND                                                 16725000
            END;                                                        16726000
        IF NOT BOOLEANEXTOG THEN BEGIN                                  16727000
ER:     ERROR(30);                                                      16728000
        RECOVER END; T~T-1;                                             16729000
        GO TEND;                                                        16730000
STRNGNM: CHECKMSTRING(FALSE);                                           16731000
        ADDR ~ SAVEA.REF;                                               16732000
        IF ITOG THEN                                                    16733000
            DOCODE(OCONV,SAVEA.LIM);                                    16734000
        IF TEMP } 2 THEN                                                16735000
            DOCODE(OINDC,ADDR)                                          16736000
        ELSE                                                            16737000
SEND:   IF LASTCODE(ODSTR,ADDR) THEN                                    16738000
            BEGIN                                                       16739000
                ML~ ML-3;                                               16740000
                DOCODE(ONSTR,-1);                                       16741000
                GO TEND;                                                16742000
            END;                                                        16743000
        IF CAT THEN                                                     16744000
            OP~ OOPDC                                                   16745000
        ELSE                                                            16746000
            OP~ OOPDS;                                                  16747000
        GO CEND;                                                        16748000
MCRRSWD:                                                                16749000
        GO SWAM[ELB.SUB-49];                                            16750000
        IF ADDR~ELB.SUB<100 THEN BEGIN                                  16751000
            ERROR(42);                                                  16752000
            RECOVER;                                                    16753000
            GO TEND;                                                    16754000
        END ELSE                                                        16755000
        IF ADDR~ ADDR-100=0 THEN                                        16756000
            IF STEPI=VMACRORES THEN                                     16757000
                IF ELB.SUB=EXPRESSIONM THEN                             16758000
                ELSE                                                    16759000
                    IF ELB.SUB=PRIMARYM THEN                            16760000
                        ADDR~8                                          16761000
                    ELSE                                                16762000
                        ERROR(63)                                       16763000
                ELSE                                                    16764000
                    ERROR(63);                                          16765000
        TOGGLECT ~ TOGGLECT + 1;                                        16766000
        OP ~ IF CAT THEN OTYPC ELSE OTYPS;                              16767000
        GO CEND;                                                        16768000
THRU:   OP ~ IF CAT THEN OSTHC ELSE OSTHS;                              16769000
        GO SUNTH;                                                       16770000
UNTL:   OP ~ IF CAT THEN OSUNC ELSE OSUNS;                              16771000
SUNTH:  I ~ 4;                                                          16772000
        IF STEPI = VSTRINGNAME THEN BEGIN LABEL L;                      16773000
            IF STEPI!VSPECIAL OR ELB!"~" THEN BEGIN                     16774000
                T ~ T - 2;                                              16775000
                STEPI;                                                  16776000
                DOCODE(OLDFT,GET(LAD).REF);                             16777000
                GO L;                                                   16778000
            END;                                                        16779000
            IF BOOLEAN(ELB.SUB) THEN ERROR(86) ELSE                     16780000
            I ~ GET(LAD).REF&ELB.SUB[TOPLINK];                          16781000
            IF STEPI!VSPECIAL OR ELB!"(" THEN BEGIN                     16782000
                ERROR(68);                                              16783000
                GO ENDSUN;                                              16784000
            END;                                                        16785000
            DO BEGIN                                                    16786000
                IF STEPI = VSTRING THEN                                 16787000
                    DOCODE(-OLDFT,CONSMAX)                              16788000
                ELSE                                                    16789000
                IF TYPE = VSTRINGNAME THEN BEGIN                        16790000
                    IF BOOLEAN(ELB.SUB) THEN                            16791000
                        DOCODE(OCONV,GET(LAD).LIM);                     16792000
                    IF ELB.SUB}2 THEN                                   16793000
                        DOCODE(OINDC,GET(LAD).REF);                     16794000
                    DOCODE(OLDFT,GET(LAD).REF);                         16795000
                END ELSE                                                16796000
                ERROR(60);                                              16797000
                IF STEPI!VSPECIAL OR NOT(ELB="," OR ELB=")") THEN       16798000
                    BEGIN ERROR(34); GO ENDSUN END;                     16799000
            END UNTIL ELB=")";                                          16800000
L:      END VSTRINGNAME ELSE                                            16801000
        IF TYPE = VSTRING THEN                                          16802000
            DOCODE(-OLDFT,CONSMAX)                                      16803000
        ELSE                                                            16804000
            ERROR(60);                                                  16805000
ENDSUN: IF BOOLEAN(ADDR~I).PLINK THEN                                   16806000
            DOCODE(OINDC,ADDR);                                         16807000
        GO CEND;                                                        16808000
QU:     IF STEPI!0 OR ELB!"(" THEN                                      16809000
            BEGIN                                                       16810000
                ERROR(32);                                              16811000
                T~ T-1                                                  16812000
            END;                                                        16813000
        IF CAT THEN                                                     16814000
            DOCODE(OLT1C,"" ")                                          16815000
        ELSE                                                            16816000
            DOCODE(OLT2C,"""&" "[TOC6]);                                16817000
        ASSIGNMENT(OLT1C,"" ");                                         16818000
        IF TYPE!0 OR ELB!")" THEN                                       16819000
            BEGIN                                                       16820000
                ERROR(33);                                              16821000
                T~ T-1;                                                 16822000
            END;                                                        16823000
        GO TEND;                                                        16824000
NAM:        IF STEPI ! VSPECIAL OR ELB ! "(" THEN BEGIN                 16825000
            ERROR(68);                                                  16826000
            T ~ T - 1;                                                  16827000
            GO TEND;                                                    16828000
        END;                                                            16829000
        IF STEPI ! VSTRINGNAME OR ELB.SUB ! 6 THEN GO ER;               16830000
        DOCODE(OINDC,GET(LAD).REF);                                     16831000
        DOCODE(OINT1,ONAME);                                            16832000
        IF STEPI ! VSPECIAL OR ELB ! ")" THEN BEGIN                     16833000
            ERROR(69);                                                  16834000
            T ~ T - 1;                                                  16835000
            GO TEND;                                                    16836000
        END;                                                            16837000
        GO DONE;                                                        16838000
LIN:    DOCODE(OINT1,OLINS - REAL(CAT));                                16839000
        GO TEND;                                                        16840000
BGN:    DOCODE(OINT1,OBGNL);                                            16841000
        GO INT;                                                         16842000
BLK:    DOCODE(OINT1,OBLKL);                                            16843000
        GO INT;                                                         16844000
SCT:    DOCODE(OINT1,OSCNT);                                            16845000
INT:    DOCODE(OISTR,LINTBAS);                                          16846000
        DOCODE(OCONV,3);                                                16847000
        DOCODE(OOPDC,LINTBAS);                                          16848000
        GO TEND;                                                        16849000
LENG:   IF TRUE THEN                                                    16850000
            BEGIN SAVEOP ~ OLENG; ADDR ~ 4 END                          16851000
        ELSE                                                            16852000
ENTI:       BEGIN SAVEOP ~ OENTI; ADDR ~ 8;                             16853000
                TOGGLECT ~ TOGGLECT + 1; END;                           16853050
        CLNGENT(SAVEOP);                                                16854000
        DOCODE(OCONV,ADDR);                                             16855000
        ADDR ~ LINTBAS;                                                 16856000
        OP~ OOPDC;                                                      16857000
CEND:   DOCODE(OP, ADDR);                                               16858000
TEND:                                                                   16859000
        IF STEPI=VOTHERS THEN                                           16860000
            IF  (I~ELB.SUB)=ENDV OR I=ELSEV OR(I=THENV AND              16861000
                BOOLEANEXTOG) THEN                                      16862000
                GO EXIT;                                                16863000
        IF ELB=")" OR ELB="(" OR ELB="]" THEN                           16864000
            GO EXIT;                                                    16865000
        IF ELB=";" THEN                                                 16866000
            GO EXIT;                                                    16867000
        CAT~ ELB="&";                                                   16868000
        IF ELB!"," AND ELB!"&" THEN                                     16869000
            IF BOOLEANEXTOG THEN                                        16870000
                GO EXIT                                                 16871000
            ELSE BEGIN                                                  16872000
                ERROR(34);                                              16873000
                RECOVER;                                                16874000
                IF TYPE!0 OR ELB!"," THEN GO EXIT;                      16875000
            END;                                                        16876000
        GO AGAIN;                                                       16877000
EXIT:   DOCODE(O, AD);                                                  16878000
        NOJUMP~ TRUE;                                                   16879000
DONE:                                                                   16880000
    END ASSIGNMENT;                                                     16881000
$ INCLUDE 16987001, IF INTERMEDIATE;                                    16899999
        COMMENT  DECOMPOSITION COMPILES CODE FOR MSTRING DECOMPOSI-     16900000
    TIONS. THE PROCESS IS: OPEN A PSEUDOREADER WITH THE CONTENTS        16901000
    OF THE MSTRING AND PROTECT IT FROM BEING FLUSHED DURING RESCAN.     16902000
    SCANNING STATEMENTS ENCLOSED IN "()" ARE COMPILED NORMALLY.         16903000
    AFTER EACH STMT, A TOGGLE JUMP IS MADE TO A RECOVERY AREA TO        16904000
    TRY TO RESTORE THE MSTRING TO ORIGINAL CONDITION & FLUSH THE        16905000
    PSEUDOREADER. NORMAL EXIT MAY CLOSE THE PSEUDOREADER BACK INTO      16906000
    AN MSTRING OR FLUSH THE UNSCANNED REMNANTS;                         16907000
PROCEDURE DECOMPOSITION;                                                16908000
    BEGIN                                                               16909000
        LABEL ER, AGAIN, EXIT;                                          16910000
        INTEGER LINKS,ADDR,D,OP;                                        16911000
        STREAM PROCEDURE MACODE(O,A,W,C); VALUE O,A,C; BEGIN            16912000
            DI~W; DI~DI+C; SI~LOC O; SI~SI+7; DS~CHR;                   16913000
            IF SC!"+" THEN BEGIN SI~SI+6; DS~2 CHR END;                 16914000
        END;                                                            16915000
        DECOMTOG~ TRUE;                                                 16916000
        IF SAVEA<0 THEN                                                 16917000
                DOCODE(OINDC,SAVEA~ ABS(SAVEA));                        16918000
        DOCODE(OOPSU,SAVEA.REF);                                        16919000
        LINKS~ 0;                                                       16920000
AGAIN:  IF STEPI=VSPECIAL AND ELB="(" THEN                              16921000
            BEGIN                                                       16922000
                STEPI;                                                  16923000
                STATEMENT;                                              16924000
                IF ELB!")" THEN                                         16925000
                    BEGIN                                               16926000
                        ERROR(35);                                      16927000
                        T~ T-1;                                         16928000
                    END;                                                16929000
                ML~ ML-3;                                               16930000
                OPCODE(MC[ML.WRD],ML.CHR,OP,ADDR);                      16931000
                IF OP=ODSTR OR OP=OISTR THEN                            16932000
                    BEGIN                                               16933000
                    IF LASTCODE(OINDC,ADDR) THEN                        16934000
                        BEGIN                                           16935000
                        ML~ ML-3;                                       16936000
                        D~ -ADDR;                                       16937000
                        END                                             16938000
                    ELSE                                                16939000
                    D~ ADDR                                             16940000
                    END                                                 16941000
                ELSE                                                    16942000
                    BEGIN                                               16943000
                        D~ 0;                                           16944000
                        DOCODE(0, 0);                                   16945000
                    END;                                                16946000
                ADDR~ LINKS;                                            16947000
                LINKS~ ML;                                              16948000
                DOCODE(OJMPC, ADDR);                                    16949000
                IF D!0 THEN                                             16950000
                    IF D<0 THEN                                         16951000
                        DOCODE(OINDC,D~ ABS(D));                        16952000
                    DOCODE(OP,D);                                       16953000
                IF STEPI=VSPECIAL THEN                                  16954000
                    IF ELB="," THEN                                     16955000
                        GO AGAIN;                                       16956000
                DOCODE(OINT1,ORDCD);                                    16957000
                GO EXIT;                                                16958000
            END;                                                        16959000
        IF TYPE=VSTRINGNAME THEN                                        16960000
            BEGIN                                                       16961000
                IF BOOLEAN(ELB.SUB) THEN                                16962000
                    BEGIN                                               16963000
                    ERROR(37);                                          16964000
                    DO RECOVER UNTIL TYPE!0 OR ELB!",";                 16965000
                    END;                                                16966000
                ADDR~ GET(LAD).REF;                                     16967000
                IF ELB.SUB}2 THEN                                       16968000
                    DOCODE(OINDC,ADDR);                                 16969000
                DOCODE(OCPSU,ADDR);                                     16970000
                STEPI;                                                  16971000
                GO EXIT;                                                16972000
            END;                                                        16973000
ER:     ERROR(38);                                                      16974000
        DO RECOVER UNTIL TYPE!0 OR ELB!",";                             16975000
EXIT:   DECOMTOG~ FALSE;                                                16976000
        DOCODE(OJUMP, ML+6);                                            16977000
        DOCODE(OINT1,ORDCD);                                            16978000
        DO                                                              16979000
            BEGIN                                                       16980000
                OPCODE(MC[LINKS.WRD],LINKS.CHR,OP,ADDR);                16981000
                MACODE(OP,ML-3,MC[LINKS.WRD],LINKS.CHR);                16982000
$ INCLUDE 16984001, IF DEBUGGING;                                       16982999
                IF DBUGN THEN                                           16983000
                    WRITE(PRINTER,DEBUG,LINKS,MNEMON[OP],"I",ML-3);     16984000
            END                                                         16985000
        UNTIL(LINKS~ ADDR)=0;                                           16986000
    END DECOMPOSITION;                                                  16987000
        COMMENT  STATEMENT INITIATES COMPILING OF ALL STATEMENTS.       17000000
    CERTAIN CONSTRUCTS ARE HANDLED INLINE, OTHERS REQUIRE RECURSIVE     17001000
    COMPILER PARTS. UNKNOWN IDS ARE ASSUMED TO BE LABELS AND ARE SO     17002000
    DECLARED. THIS PROCESS IS REQUIRED SINCE BEGINS&ENDS IN MACROS      17003000
    ARE CONSIDERED BLOCKS REGARDLESS OF ANY DECLARATIONS BEING          17004000
    PRESENT;                                                            17005000
PROCEDURE STATEMENT;                                                    17006000
    BEGIN                                                               17007000
        INTEGER I,ADDR;                                                 17008000
$ INCLUDE 17008051, IF ADVANCED;                                        17008049
        INTEGER LINKS;                                                  17008050
        LABEL AIDENTL,SET,RESET;                                        17009000
        LABEL NOTDEC,STRNGNM,E1,MCRRSWD,OTHRS,RSC,DLT,OTPT,INP,         17010000
          AGAIN,EXIT,ERRO,SC,WRNG,PTCH,POST,E39;                        17011000
        SWITCH STBEG ~ SC,E1,E1,E1,E1,NOTDEC,E1,E1,E1,E1,E1,            17012000
          E1,E1,E1,E1,E1,STRNGNM,AIDENTL,E1,E1,E1,MCRRSWD,              17013000
          OTHRS;                                                        17014000
        SWITCH STACT ~ INP,RSC,OTPT,ERRO,WRNG,POST,DLT,PTCH,            17015000
          SET,RESET;                                                    17016000
        STREAM PROCEDURE MACODE(O,A,W,C); VALUE O,A,C;                  17017000
        BEGIN DI~W; DI~DI+C; SI~LOC O; SI~SI+7; DS~CHR;                 17018000
            IF SC!"+" THEN BEGIN SI~SI+6; DS~2 CHR END;                 17019000
        END;                                                            17020000
AGAIN:  GO STBEG[TYPE+1];                                               17021000
        GO TO E1;                                                       17022000
STRNGNM: CHECKMSTRING(TRUE);                                            17023000
        IF STEPI ! VSPECIAL THEN                                        17024000
            BEGIN                                                       17025000
                ERROR(39);                                              17026000
                T~ T-1;                                                 17027000
                GO DLT;                                                 17028000
            END;                                                        17029000
        IF NOT DECOMTOG THEN                                            17030000
            IF ELB="-" THEN                                             17031000
                IF STEPI=VSPECIAL AND ELB=">" THEN                      17032000
                    BEGIN                                               17033000
                        IF ITOG THEN                                    17034000
                            ERROR(40);                                  17035000
                        IF TEMP}2 THEN SAVEA~ -SAVEA;                   17036000
$ INCLUDE 17037001, IF INTERMEDIATE;                                    17036999
                        DECOMPOSITION;                                  17037000
$ OMIT 17037101, IF INTERMEDIATE;                                       17037049
                        ERROR(110);                                     17037050
                        RECOVER;                                        17037100
                        GO EXIT;                                        17038000
                    END;                                                17039000
        IF ELB!"~" THEN                                                 17040000
            BEGIN                                                       17041000
                ERROR(41);                                              17042000
                T~ T-1;                                                 17043000
            END;                                                        17044000
$ INCLUDE 17062001, IF ADVANCED;                                        17044999
        IF TEMP}2 THEN                                                  17045000
            IF STEPI=0 AND ELB="[" THEN BEGIN                           17046000
                LINKS~ T+1;                                             17047000
                BOOLEANEXTOG~ TRUE;                                     17048000
                ASSIGNMENT(OINDC,TEMP.[47:1]);                          17049000
                IF TYPE!0 OR ELB!"]" THEN                               17050000
                    ERROR(79);                                          17051000
                IF T=LINKS THEN BEGIN                                   17052000
                    ML~ ML-3;                                           17053000
                    DOCODE(OILIT,0);                                    17054000
                    END;                                                17055000
                BOOLEANEXTOG~ FALSE;                                    17056000
                STEPI;                                                  17057000
                DOCODE(OISTR,SAVEA.REF);                                17058000
                GO SC;                                                  17059000
                END                                                     17060000
            ELSE                                                        17061000
                T~ T-1;                                                 17062000
        MULTIASSIGNMENT(IF ITOG THEN OISTR ELSE ODSTR, SAVEA,           17063000
            TEMP,CFIELD);                                               17064000
        GO EXIT;                                                        17065000
MCRRSWD:GO STACT[ELB.SUB+1];                                            17066000
        ERROR(42);                                                      17067000
        DO RECOVER UNTIL TYPE!0 OR ELB!",";                             17068000
        GO TO EXIT;                                                     17069000
PTCH:                                                                   17070000
$ OMIT 17070151, IF ADVANCED;                                           17070049
        ERROR(111);                                                     17070050
        RECOVER;                                                        17070100
        GO EXIT;                                                        17070150
$ INCLUDE 17084001, IF ADVANCED;                                        17070499
    BEGIN LABEL L; L:                                                   17070500
      IF STEPI!VSTRINGNAME OR NOT BOOLEAN(ELB.SUB) THEN BEGIN           17071000
            ERROR(64);                                                  17072000
        DO RECOVER UNTIL TYPE!0 OR ELB!",";                             17073000
            GO EXIT;                                                    17074000
        END;                                                            17075000
        SAVEA ~ GET(LAD).REF;                                           17076000
        IF STEPI!0 OR ELB!"," THEN BEGIN                                17077000
            ERROR(34);                                                  17078000
        DO RECOVER UNTIL TYPE!0 OR ELB!",";                             17079000
            GO EXIT;                                                    17080000
        END;                                                            17081000
        ASSIGNMENT(OOTPT,SAVEA);                                        17082000
        GO EXIT;                                                        17083000
      END;                                                              17084000
$ INCLUDE 17085001, IF INTERMEDIATE;                                    17084999
DLT:    ASSIGNMENT(OINT1,ODELE);                                        17085000
$ OMIT 17085101, IF INTERMEDIATE                                        17085049
DLT:    ERROR(110);                                                     17085050
        RECOVER;                                                        17085100
        GO EXIT;                                                        17086000
$ INCLUDE 17087001, IF INTERMEDIATE;                                    17086999
RSC:    ASSIGNMENT(OINPT,1);                                            17087000
$ OMIT 17087101, IF INTERMEDIATE;                                       17087049
RSC:    ERROR(110);                                                     17087050
        RECOVER;                                                        17087100
        GO EXIT;                                                        17088000
OTPT:   ASSIGNMENT(OOTPT, 0);                                           17089000
        GO EXIT;                                                        17090000
ERRO:   ASSIGNMENT(OERRO, 0);                                           17091000
        GO EXIT;                                                        17092000
WRNG:   ASSIGNMENT(OERRO,1);                                            17093000
        GO EXIT;                                                        17094000
POST: BEGIN LABEL L; L:                                                 17095000
        IF STEPI = VSPECIAL AND ELB = "[" THEN BEGIN                    17096000
            IF STEPI = VOTHERS THEN BEGIN                               17097000
                IF I~ELB.SUB = PAGEV THEN                               17098000
                    ADDR ~ 4                                            17099000
                ELSE                                                    17100000
                IF I = DBLV THEN                                        17101000
                    ADDR ~ 3                                            17102000
            END ELSE                                                    17103000
            IF TYPE = VNUMBER THEN BEGIN                                17104000
                I ~ CONSTANTCONVERT(IDENT[1],COUNT);                    17105000
                IF I<1 OR I>4092 THEN BEGIN                             17106000
                    ERROR(26);                                          17107000
                    ADDR ~ 2;                                           17108000
                END ELSE                                                17109000
                ADDR ~ I + 3;                                           17110000
            END ELSE                                                    17111000
            ERROR(26);                                                  17112000
            IF STEPI ! VSPECIAL OR ELB ! "]" THEN                       17113000
                ERROR(36);                                              17114000
        END ELSE BEGIN                                                  17115000
            T ~ T - 1;                                                  17116000
            ADDR ~ 2;                                                   17117000
        END;                                                            17118000
        ASSIGNMENT(OERRO,ADDR);                                         17119000
        GO EXIT;                                                        17120000
    END;                                                                17121000
INP:    ASSIGNMENT(OINPT, 0);                                           17122000
        GO EXIT;                                                        17123000
SET: RESET:                                                             17124000
$ INCLUDE 17140001, IF ADVANCED;                                        17124999
        BEGIN LABEL E;                                                  17125000
        INTEGER TR;                                                     17126000
        TR ~ REAL(ELB.SUB=SETM);                                        17127000
        DO BEGIN                                                        17128000
            IF STEPI ! VMACRORES OR I~ELB.SUB < 200 THEN BEGIN          17129000
                IF TYPE=VNUMBER AND I=INTEGERV THEN BEGIN               17129100
                    IF I~CONSTANTCONVERT(IDENT[1],COUNT)>35 THEN        17129200
                        BEGIN ERROR(108); GO E END;                     17129300
                    DOCODE(OTOGL,4|I+256+TR);                           17129400
                END ELSE BEGIN                                          17129500
                    ERROR(42);                                          17130000
E:                  RECOVER;                                            17131000
                    GO EXIT;                                            17132000
            END END ELSE                                                17133000
            DOCODE(OTOGL,(I-200)|3+TR);                                 17134000
            IF STEPI ! 0 OR NOT(ELB="," OR ELB=";") THEN BEGIN          17135000
                ERROR(34);                                              17136000
                GO E;                                                   17137000
            END;                                                        17138000
        END UNTIL ELB = ";";                                            17139000
        END SET RESET;                                                  17140000
$ OMIT 17140051, IF ADVANCED;                                           17140049
        ERROR(111);                                                     17140050
        GO EXIT;                                                        17141000
OTHRS:  IF I~ELB.SUB=ENDV THEN GO EXIT;                                 17142000
        IF I=ELSEV THEN                                                 17143000
            IF COND THEN GO EXIT ELSE GO E1;                            17144000
        IF I=BEGINV THEN                                                17145000
            BEGIN                                                       17146000
                DO                                                      17147000
                    BEGIN                                               17148000
                        STEPI;                                          17149000
                        STATEMENT                                       17150000
                    END                                                 17151000
                UNTIL TYPE=VOTHERS AND ELB.SUB=ENDV;                    17152000
                STEPI;                                                  17153000
                GO TO EXIT;                                             17154000
            END;                                                        17155000
        IF I=IFV THEN                                                   17156000
            BEGIN                                                       17157000
                CONDITIONAL;                                            17158000
                GO EXIT;                                                17159000
            END;                                                        17160000
        IF ELB.SUB=GOV THEN                                             17161000
            BEGIN                                                       17162000
                UNCONDITIONAL;                                          17163000
                GO EXIT;                                                17164000
            END;                                                        17165000
        GO E1;                                                          17166000
NOTDEC: PUT(GET(ADDICT(VALGOLIDENT, LABELV)), ML);                      17167000
        NOJUMP~ FALSE;                                                  17168000
        NEXTDICT~ NEXTDICT+1;                                           17169000
        IF STEPI!VSPECIAL OR ELB!":" THEN                               17170000
            ERROR(43)                                                   17171000
        ELSE                                                            17172000
            STEPI;                                                      17173000
        GO AGAIN;                                                       17174000
AIDENTL: IF ELB.BLKLVL<MCBL THEN                                        17175000
            GO NOTDEC                                                   17176000
        ELSE                                                            17177000
            IF ELB.SUB=LABELV THEN                                      17178000
                BEGIN INTEGER LINKS;                                    17179000
                    NOJUMP~ FALSE;                                      17180000
                    IF NOT BOOLEAN(LINKS~-GET(LAD)).PNOTT THEN          17181000
                      BEGIN                                             17182000
                        PUT(LAD,ML);                                    17183000
                        LINKS ~ LINKS.JADR;                             17184000
                        DO BEGIN                                        17185000
                            OPCODE(MC[LINKS.WRD],LINKS.CHR,OP,ADDR);    17186000
                            MACODE(OP,ML,MC[LINKS.WRD],LINKS.CHR);      17187000
$ INCLUDE 17190001, IF DEBUGGING;                                       17187999
                            IF DBUGN THEN                               17188000
                                WRITE(PRINTER,DEBUG,LINKS,              17189000
                                    MNEMON[OP],"I",ML);                 17190000
                        END UNTIL(LINKS~ADDR.JADR)=0;                   17191000
                        END                                             17192000
                    ELSE                                                17193000
                        ERROR(44);                                      17194000
                    IF STEPI!VSPECIAL OR ELB!":" THEN                   17195000
                        ERROR(45)                                       17196000
                    ELSE                                                17197000
                        STEPI;                                          17198000
                    GO AGAIN;                                           17199000
                END;                                                    17200000
SC:     IF ELB!";" THEN                                                 17201000
            BEGIN                                                       17202000
E1:         ERROR(46);                                                  17203000
            DO RECOVER UNTIL TYPE!0 OR ELB!","; END;                    17204000
EXIT:                                                                   17205000
    END STATEMENT;                                                      17206000
        COMMENT  MACRO IS THE DRIVER FOR MACRO COMPILATION. IT          18000000
    FORCES MACRO RESWDS IN & FORCES A PSEUDOBLOCK FOR THEIR CUTBACK.    18001000
    LOCAL ADDRESS ASSIGNMENTS ARE INITIALIZED. ONE CALL ON              18002000
    STATEMENT IS SUFFICIENT TO COMPILE THE MACRO CODE. WRAPUP           18003000
    CONSISTS OF WRITING THE CODE, CONSTANTS, & SIZING INFO INTO         18004000
    MCODE AND STORING POINTERS INTO THE DICT ENTRY. FINAL STEP IS TO    18005000
    DELINK THE RESWDS FROM THE SCANNERS VIEW;                           18006000
        INTEGER I,J;                                                    18007000
$ INCLUDE 18008001, IF DEBUGGING;                                       18007999
        FORMAT SS(I6"-"A1);                                             18008000
        SLIPWORDS(MACRODICT,ALGOLDICT);                                 18009000
        STARTBLOCK(FALSE);                                              18010000
        MC[ML~0] ~ 0;                                                   18011000
        IF MACT } 1 THEN                                                18012000
            MOVE(8,SPACEMAP,SPACEMAP[16]);                              18013000
        SPACEMAP[0] ~ 512;                                              18014000
        MOVE(7,SPACEMAP,SPACEMAP[1]);                                   18015000
$ INCLUDE 18017001, IF DEBUGGING;                                       18015999
        IF SRT AND PRINT THEN                                           18016000
            WRITE(PRINTER,SS,PT,"M");                                   18017000
        LACT ~ LALFBAS + 27;                                            18018000
        LICT ~ LINTBAS;                                                 18019000
        LOCT ~ LPTRBAS - 1;                                             18020000
        CONP~ 1;                                                        18021000
        DEC~ NOT MACROTOG~ TRUE;                                        18022000
        STEPI;                                                          18023000
        MCBL~ BLVL;                                                     18024000
        I ~ ERRCT;                                                      18025000
        STATEMENT;                                                      18026000
        DOCODE(OINT1,OEXIT);                                            18027000
        ML ~ (ML+7).WRD;                                                18028000
        PUT(Q,ML&CONP[TOMPOOL]&LACT[TOMLACT]&(LICT-LINTBAS+1)           18029000
            [TOMLICT]);                                                 18030000
        PUT(Q+1,PT&(LOCT-LPTRBAS+1)[TOMLOCT]&(J~REAL(LACT.M32!0)        18031000
            +LACT.D32)[TOMSMAP]);                                       18032000
        WRAPBLOCK(FALSE);                                               18033000
        IF I ! ERRCT THEN BEGIN                                         18034000
            ML ~ J ~ CONP ~ 0;                                          18035000
            DOCODE(OERRO,15);                                           18036000
            ML ~ 1;                                                     18036100
        END;                                                            18037000
        WRITIT(MC,ML,PT,MCODE);                                         18038000
        WRITIT(COMCON,CONP,PT,MCODE);                                   18039000
        ML ~ 0;                                                         18040000
        WRITIT(SPACEMAP,J,PT,MCODE);                                    18041000
        IF MACT } 1 THEN                                                18042000
            MOVE(8,SPACEMAP[16],SPACEMAP);                              18043000
        NEST ~ NEST-1;                                                  18044000
        FOR I~ 0 STEP 1 UNTIL 124 DO                                    18045000
            WHILE MACRODICT { A~DLIST[I] < ALGOLDICT DO                 18046000
                DLIST[I] ~ DICT[A.IR,A.IC];                             18047000
        DEC ~ NOT MACROTOG ~ FALSE;                                     18048000
END MACRO COMPILER;                                                     18049000
                                                                        18050000
        COMMENT  FILEPARAMETERS RETURNS THE 5 WORD FPB ENTRY FOR        20000000
    A FILE NAME (FN);                                                   20001000
COMMENT FILEPARAMETERS;                                                 20002000
PROCEDURE FILEPARAMETERS(FN,A);FILE FN;ARRAY A[0]; BEGIN REAL II;SAVE   20003000
ARRAY FPB[0:10];ARRAY T[0:1];INTEGER STREAM PROCEDURE LENGTH(A);BEGIN   20004000
SI~LOC A;DI~LOC LENGTH;SI~SI+1;DI~DI+6;DS~NUM;DS~CHR;END;REAL STREAM    20005000
PROCEDURE FNUM(F);BEGIN SI~F;SI~SI-24;DI~LOC F;DS~WDS;SI~F;SI~SI+34;DI~ 20006000
LOC FNUM;DI~DI+6;DS~2 CHR;DI~DI-2;DS~RESET;END;PROCEDURE FPBDESC(JUNK,  20007000
II,T);REAL JUNK,II;ARRAY T[0];BEGIN STREAM PROCEDURE F(JUNK,II,T);BEGIN 20008000
SI~II;SI~SI+8;DI~T;DS~WDS;SI~JUNK;14(SI~SI-8);DI~II;DI~DI+8;DS~WDS;END; 20009000
F(JUNK,II,T[0]);END;STREAM PROCEDURE RESTORE(II,T);BEGIN SI~T;DI~II;DI~ 20010000
DI+8;DS~WDS;END;STREAM PROCEDURE MOVE(A,B,N);VALUE N;BEGIN SI~A;DI~B;DS 20011000
~N WDS END;IF(II~LENGTH(A))>5 THEN II~5;FPBDESC(II+1,II,T);MOVE(FPB[    20012000
FNUM(FN)],A[0],II);RESTORE(II,T[0]);END;                                20013000
        COMMENT  INITIALIZATION BLOCK. SVT SETUP BY TESTING R+26        20014000
    FOR SYNTAX OR LIBRARY OPTIONS. IF LIBRARY, MF & F ARE SET UP &      20015000
    CODE FILE FLAG(WRL) RESET ELSE DUMMY TIME-DERIVED NAME GENERATED.   20016000
    IF PRINTER HAS BEEN EQUATED TO DISK, A DUMMY DISK FILE OF THE       20017000
    PROPER NAME AND SIZE(COMMON=RECORDS) IS LOCKED TO INITIALIZE THE    20018000
    PRINTER FILE.                                                       20019000
        THE LANGUAGE FILE IS THEN READ IN TO INITIALIZE TABLES &        20020000
    DEFINE THE PROPERTIES OF THE LANGUAGE TO BE PROCESSED.              20021000
        THE FIRST CARD IS THEN READ. IF IT WAS A MCALGOL $ CARD,        20022000
    THE CODE FILE NAME COULD HAVE BEEN CHANGED. IF THE CODE FILE IS     20023000
    NOT TO BE SAVED, A "$+PURGE" IMAGE IS PUT INTO THE OUTPUT STRING;   20024000
    BEGIN                                                               20025000
    FORMAT                                                              20026000
        BJ1("$+PURGE"),                                                 20027000
        BJ2("LANGUAGE LEVEL INCOMPATIBILITY, FLUSHED");                 20028000
        SETSIZE(128,LOCAL,0,2);                                         20029000
        BLANK(POB,0,14);                                                20030000
        SVT ~ (RCCNT>0)&(RCCNT<0)[46:47:1];                             20031000
        RCCNT~-1;                                                       20032000
        SEQN(0, POB[9]);                                                20033000
    IF SVT THEN BEGIN                                                   20034000
        FILEPARAMETERS(CODE,ERB[*]);                                    20035000
        FILL CODE WITH MF~ ERB[0].[6:42]; F~ ERB[1];                    20036000
    END ELSE BEGIN                                                      20037000
        SEQN(TIME(1), F);                                               20038000
        FILL CODE WITH MF~TIME(0)&"GW"[TOC12],F;                        20039000
    END;                                                                20040000
    NEWRESEQ ~ FALSE;                                                   20041000
    NEWSEQ ~ - 1000;                                                    20042000
    NEWINC ~ 1000;                                                      20043000
$ INCLUDE 20043530, IF COUNTING;                                        20043500
    PSEU ~ BFRMAX;                                                      20043510
    FILEPARAMETERS(PRINTER,ERB[*]);                                     20044000
    IF ERB[3].C7=12 THEN BEGIN                                          20045000
        FILE OUT DISK DISK SERIAL [20:(ERRCT+199) DIV 200|10]           20046000
            (1,10,100,SAVE 780);                                        20047000
        FILL DISK WITH ERB[0],ERB[1];                                   20048000
        WRITE (DISK); LOCK(DISK);                                       20049000
    END;                                                                20050000
    FILL LINKS[*] WITH 0, "        ";                                   20051000
$ INCLUDE 20052001, IF INTERMEDIATE;                                    20051999
    STARTUPISOCAT;                                                      20052000
FILL RELOP[*] WITH OSEQL,OSNEQ,OSLSS,OSLEQ,OSGTR,OSGEQ,                 20053000
                   OSEQL,OSNEQ,OSGTR,OSGEQ,OSLSS,OSLEQ;                 20054000
FILL MNEMON[*] WITH                                                     20055000
    "00NOPP","00OPSU","41CPSU","09SEQL","09SNEQ",                       20056000
    "09SLSS","09SLEQ","09SGTR","09SGEQ","02OPDC",                       20057000
    "02OPDS","40NSTR","41DSTR","00ISTR","06IOPC",                       20058000
    "06ADDI","06SUBI","06MULI","06DIVI","00OP19",                       20059000
    "00OP20","04ILIT","04ADDL","04SUBL","04MULL",                       20060000
    "04DIVL","10LT1C","10LT2C","10LT2S","00OP29",                       20061000
    "00OP30","00OP31","00OP32","00OP33","00OP34",                       20062000
    "00OP35","01INPT","01OTPT","01ERRO","00OP39",                       20063000
    "00OP40","00LDFT","00SUNC","00SUNS","00STHC",                       20064000
    "00STHS","01ENTR","01FIND","00SRCH","00OP49",                       20065000
    "00OP50","00TYPC","00TYPS","00SETL","00CONV",                       20066000
    "01INDC","00ICMP","00TOGL","00OP58","00OP59",                       20067000
    "00OP60","00JMPC","00JUMP","30INT1","04OBGN",                       20068000
    "04+BGN","04-BGN","04|BGN","04/BGN","04OBLK",                       20069000
    "04+BLK","04-BLK","04|BLK","04/BLK","00DIAL",                       20070000
    "00LINC","00LINS","05ENTI","05LENG","01RDCD",                       20071000
    "01DELE","01STCK","00NSTK","04OSCT","04+SCT",                       20072000
    "04-SCT","04|SCT","04/SCT","04LEVL","04QUAL",                       20073000
    "00NAME","00EXIT";                                                  20074000
$ INCLUDE 20074301, IF DEBUGGING;                                       20074019
FILL TYPENAME[*] WITH "4LBRB ","7RESWO","RD      ",                     20074020
        "6SPCHA","R       ","?LOGIC","ALVALUE ","6NUMBE","R       ",    20074040
        "6STRIN","G       ","4LPRP ","#PROCE","DUREF   ",               20074060
        "#RELAT","IONOP   ","5PWORD","8IFCLA","USE     ",               20074080
        "7PRIMA","RY      ","6FACTO","R       ","4TERM ",               20074100
        "BSIMPL","EARITHEX","PRESS   ","?ARITH","EXPRESS ",             20074120
        "8RELAT","ION     ","8BPRIM","ARY     ","7BFACT","OR      ",    20074140
        "5BTERM","@IMPLI","CATION  ",":SIMPL","EBOOLEAN",               20074160
        ":BOOLE","ANEXPRES","S       ","8GPRIM","ARY     ",             20074180
        "@LINKD","ESCRIP  ","8LINKP","ART     ",                        20074200
        "7EXPRE","SS      ","5STATE","6INTGR","R       ",               20074220
        "#IDENT","IFIER   ","@DECLA","RATION  ","7ELEME","NT      ",    20074240
        "8VARIA","BLE     ";                                            20074260
    MOVE(1,CARD(9),LINKS);                                              20075000
    SPACEMAP[0] ~ 512;                                                  20076000
    MOVE(15,SPACEMAP,SPACEMAP[1]);                                      20077000
    TABLET ~ LASTUSED ~ BNO ~ 1;                                        20078000
    NXTELB ~ 2;                                                         20079000
    FLUSHD ~ TRUE;                                                      20080000
    PRINT ~ WARNTOG ~ TRUE;                                             20081000
    CARDREC ~ DISKREC ~ -1;                                             20082000
    NXTPTCHREC ~ CODEREC ~ 0;                                           20083000
    BEGIN                                                               20084000
        FILE IN LANGUAGE DISK SERIAL(2,256);                            20100000
        ARRAY GLOBSIZE[0:63];                                           20101000
        FILEPARAMETERS(LANGUAGE,ERB[*]);                                20102000
        LANG ~ ERB[1];                                                  20103000
        IF LANG="LANGUAG" THEN                                          20104000
            LANG ~ "MCALGOL";                                           20105000
        IF ERB[0]=0 THEN FILL LANGUAGE WITH "MCALGOL";                  20106000
        FILEPARAMETERS(ERRORS,DFN[*]);                                  20106050
        IF DFN[0]=0 AND DFN[1]="ERRORS " THEN                           20106100
            FILL ERRORS WITH LANG;                                      20106150
            READ(LANGUAGE,*,DOLLARDICT,STREAMDICT,MACRODICT,            20107000
                ALGOLDICT,NEXTDICT,LASTDICT,NXTUDICT,LSTUDICT,          20108000
                BLVL,RCCNT,ERRCT,BECT,OHM,PT,DECP,GLAT,GLIT,GLOT,       20109000
                NXTPTCHX,NXTPTCHREC,NXTCODE);                           20110000
                                                                        20111000
            IF ERRCT!LANGUAGELEVEL AND ERRCT!0 THEN                     20112000
                BEGIN                                                   20113000
                WRITE(PRINTER,BJ2);                                     20114000
                GO ENDDD;                                               20115000
                END;                                                    20116000
            ERRCT ~ 0;                                                  20116050
            READ(LANGUAGE,250,DLIST[*]);                                20117000
            IF BLVL > 1 THEN BEGIN                                      20118000
                READ(LANGUAGE,256,DFN[*]);                              20119000
                MOVE(100,DFN,BLIST);                                    20120000
                MOVE(128,DFN[100],STMTSTACK);                           20121000
                OUTERLIMIT ~ DFN[228];                                  20122000
                STMTSTKLVL ~ DFN[229];                                  20123000
                SSTK ~ DFN[230];                                        20124000
                MOVE(8,DFN[231],SPACEMAP[8]);                           20125000
            END;                                                        20126000
            FOR I~0 STEP 256 UNTIL NEXTDICT-1 DO                        20127000
                READ(LANGUAGE,256,DICT[I.IR,*]);                        20128000
$ INCLUDE 2013001, IF ADVANCED;                                         20128999
            FOR I~0 STEP 256 UNTIL NXTUDICT-1 DO                        20129000
                READ(LANGUAGE,256,UDICT[I.IR,*]);                       20130000
%           WRITE(MSTOR[0],256,DFN[*]);                                 20131000
%           WRITE(MSTOR[150],256,DFN[*]);                               20132000
            FOR I~0 STEP 256 UNTIL PT-1 DO BEGIN                        20133000
                READ(LANGUAGE,256,MCODE[*]);                            20134000
                WRITE(MSTOR[I.IR],256,MCODE[*]);                        20135000
            END;                                                        20136000
            MSTORP.PDF1 ~ I.IR - 1;                                     20137000
            FOR I~0 STEP 256 UNTIL DECP-1 DO BEGIN                      20138000
                READ(LANGUAGE,256,MDECL[*]);                            20139000
                WRITE(MSTOR[I.IR+150],256,MDECL[*]);                    20140000
            END;                                                        20141000
            MSTORP.PDF2 ~ I.IR - 1;                                     20142000
            DECP.REF ~ 1;                                               20143000
            IF GLAT } GALFBAS THEN BEGIN                                20144000
                READ(LANGUAGE,A~(GLAT-GALFBAS+4).D4,GLOBSIZE[*]);       20145000
                FOR I~0 STEP 1 UNTIL A DO                               20146000
                    IF GLOBSIZE[I] ! 0 THEN                             20147000
                        READ(LANGUAGE,256,GLOBAL[I,*]);                 20148000
            END;                                                        20149000
            IF GLIT } GINTBAS THEN                                      20150000
                READ(LANGUAGE,GLIT-GINTBAS+1,GINT[*]);                  20151000
            IF GLOT } GPTRBAS THEN                                      20152000
                READ(LANGUAGE,GLOT-GPTRBAS+1,GPTR[*]);                  20153000
            HM~ 252;                                                    20154000
            FOR I~ 0 STEP 100 UNTIL OHM-100 DO                          20155000
                BEGIN                                                   20156000
                    IF HM=252 THEN                                      20157000
                        BEGIN                                           20158000
                            READ(LANGUAGE, 252, ACCM[*]);               20159000
                            HM~ 0                                       20160000
                        END;                                            20161000
                    MOVE(9, ACCM[HM], POB[*]);                          20162000
                    OUTPUT;                                             20163000
                    HM~ HM+9                                            20164000
                END;                                                    20165000
$ INCLUDE 20190001, IF INTERMEDIATE;                                    20165999
            IF NXTPTCHX}0 THEN BEGIN                                    20166000
                REAL A,B,C,D,E,F,G;                                     20167000
                READ(LANGUAGE,NXTPTCHX+2,PATCHSPACE[*]);                20168000
                G~252;                                                  20169000
                FOR A~0 STEP 2 UNTIL NXTPTCHX DO BEGIN                  20170000
                    E~PATCHSPACE[A]; F~PATCHSPACE[A+1];                 20171000
                    B ~ F.PUSED-1;                                      20172000
                    C ~ E.PBASE;                                        20173000
                    FOR D~0 STEP 1 UNTIL B DO BEGIN                     20174000
                        IF G=252 THEN BEGIN                             20175000
                            READ(LANGUAGE,252,OA[*]);                   20176000
                            G~0;                                        20177000
                        END;                                            20178000
                        MOVE(9,OA[G],PPB[*]);                           20179000
%                                                                       20180000
                        WRITE(BACKPATCHES[C+D],9,PPB[*]);               20181000
                        G~G+9;                                          20182000
            END END END;                                                20183000
            IF NXTCODE ! 0 THEN                                         20184000
                FOR I~0 STEP 2 UNTIL NXTCODE.CROW DO BEGIN              20185000
                    READ(LANGUAGE,240,OA[*]);                           20186000
                    WRITE(ISOCODE[I],120,OA[*]);                        20187000
                    MOVE(120,OA[120],OA);                               20188000
                    WRITE(ISOCODE[I+1],120,OA[*]);                      20189000
                END;                                                    20190000
            READACARD;                                                  20191000
            IF NOT SVT THEN                                             20192000
                BEGIN                                                   20193000
                WRITE(ACCM[*],BJ1);                                     20194000
                MOVE(9,ACCM,POB);                                       20195000
                OUTPUT;                                                 20196000
                END;                                                    20197000
        END;                                                            20198000
        COMCON[0]~ REAL(<17700000000>);                                 20199000
        OHM~ HM~ 3;                                                     20202000
        BFRLNG ~ 8|(BFRMAX-10);                                         20203000
    END;                                                                20204000
        COMMENT  OUTER BLOCK DRIVER SECTION. A GENERAL "STEPI" LOOP     21000000
    IS FORMED TO SCAN THE INPUT STRING. ANY ITEM REACHING HERE AS AN    21001000
    ELBAT WORD IS PUT INTO THE OUTPUT STRING BY THE HANDLERS OF         21002000
    THE -SCLASS- SWITCH. ITEMS SUCH AS DEFINES & MACROS AND THE         21003000
    MATERIAL THEY INTERCEPT IS NOT SEEN HERE UNTIL IT HAS FILTERED      21004000
    THRU TABLE WITH NO ACTION TAKEN UPON IT.                            21005000
        FROM THE HANDLERS THE ITEMS GO TO WRIT FOR ENTRY INTO A CODE    21006000
    RECORD. PACKING RULES ARE SELECTED FROM THE OUTCLASS SWITCH         21007000
    BEFORE RETURNING TO START FOR THE NEXT ITEM;                        21008000
BEGIN                                                                   21009000
    LABEL START,EC,SPECCHAR,OTHRS,DJ,PTCHS,PTCHX,                       21010000
      USUAL,WRIT,OT0,OT1,OT2,OT3,OT9;                                   21011000
    SWITCH SCLASS ~ SPECCHAR,USUAL,USUAL,USUAL,USUAL,USUAL,EC,EC,       21012000
      EC,EC,EC,EC,EC,EC,EC,EC,USUAL,USUAL,EC,EC,USUAL,                  21013000
      USUAL,OTHRS,DJ,PTCHS;                                             21014000
    SWITCH OUTCLASS~ OT3,OT0,OT2,OT2,OT0,OT3,OT1;                       21015000
        COMMENT  FLUSHIT PARTITIONS RECORDS FOR THE OUTER BLOCK         21016000
    OUTPUT STRING. OA CONTAINS ONE ITEM TO BE OUTPUT: ID, #, SP CHR,    21017000
    OR DECLARATION. AN ATTEMPT IS MADE TO NOT BREAK ITEMS ACROSS        21018000
    RECORD BOUNDARIES;                                                  21019000
PROCEDURE FLUSHIT;                                                      21020000
BEGIN                                                                   21021000
    WHILE A>0 DO                                                        21022000
        BEGIN IF A}72 AND NOT FLUSHD THEN OUTPUT;                       21023000
            WHILE A}72 DO BEGIN                                         21024000
                M(OA[I.WRD],I.CHR,POB,0,-0,1,8);                        21025000
                A ~ A - 72;                                             21026000
                I ~ I + 72;                                             21027000
                OUTPUT;                                                 21028000
            END;                                                        21029000
            IF 72-OUTC < A THEN OUTPUT;                                 21030000
            OP ~ A;                                                     21031000
            M(OA[I.WRD],I.CHR,POB[OUTC.WRD],OUTC.CHR,-0,OP.C6,OP);      21032000
            A~ A-OP;                                                    21033000
            I~ I+OP;                                                    21034000
            IF OUTC~ OUTC+OP=72 THEN BEGIN                              21035000
                OUTPUT; OUTC~ REAL(A=0); END                            21036000
            ELSE                                                        21037000
                FLUSHD~ FALSE;                                          21038000
        END;                                                            21039000
END FLUSHIT;                                                            21040000
                                                                        21041000
START: GO TO SCLASS[1+STEPI];                                           21042000
EC: ERROR(47);                                                          21043000
    GO TO START;                                                        21044000
SPECCHAR: IF ELB="$" AND NOT DOLLARSIGN THEN                            21045000
        BEGIN                                                           21046000
            DOLLARSIGN~ TRUE;                                           21047000
            OUTPUT                                                      21048000
        END                                                             21049000
    ELSE                                                                21050000
        IF ELB=";" AND DOLLARSIGN THEN                                  21051000
            BEGIN                                                       21052000
                DOLLARSIGN~ FALSE;                                      21053000
                OUTPUT;                                                 21054000
                GO TO START                                             21055000
            END;                                                        21056000
    OA[0]~ "1?    "&ELB[18:42:6];                                       21057000
    GO WRIT;                                                            21058000
OTHRS: IF (I~ELB.SUB) = BEGINV THEN BEGIN                               21059000
            IF STMTSTKLVL = OUTERLIMIT ! 0 THEN                         21060000
                IF NOT DJTOG.[46:1] THEN BEGIN                          21061000
                    T ~ T - 1;                                          21062000
                    OA[0] ~ "1;    ";                                   21063000
                    DJTOG ~ TRUE;                                       21064000
                    ELB ~ ";";                                          21065000
                    TYPE ~ 0;                                           21066000
                    GO WRIT;                                            21067000
                END;                                                    21068000
                    OA[0]~ "5BEGIN"; DJTOG~ TRUE;                       21069000
            P(" ",OA[1],0);                                             21070000
            OUTERLIMIT ~ OUTERLIMIT + 1 END                             21071000
    ELSE                                                                21072000
        IF I=ENDV THEN                                                  21073000
            BEGIN IF STMTSTKLVL } OUTERLIMIT!0 THEN BEGIN               21074000
                    T ~ T - 1;                                          21075000
                    OA[0] ~ "1;    ";                                   21076000
                    ELB ~ ";"; TYPE~0;                                  21077000
                    GO WRIT;                                            21078000
                END;                                                    21079000
                OA[0] ~ "3END  ";                                       21080000
                OUTERLIMIT ~ OUTERLIMIT - 1;                            21081000
            END                                                         21082000
        ELSE                                                            21083000
            GO TO USUAL;                                                21084000
    GO WRIT;                                                            21085000
DJ: I ~ LAD&1[TOREF];                                                   21086000
    DJTOG ~ DJTOG OR (IF ELB.SUB=1 THEN <3> ELSE TRUE);                 21087000
%                                                                       21088000
    READIT(OA,1,I,MDECL);                                               21091000
    I ~ I - 1;                                                          21091100
    A ~ OA[0].CTR;                                                      21092000
    READIT(OA,(A+10).WRD,I,MDECL); I~ 3;                                21093000
    FLUSHIT;                                                            21094000
    IF NOT FLUSHD AND UNPACK THEN OUTPUT;                               21095000
    GO START;                                                           21096000
PTCHS:                                                                  21097000
$ INCLUDE 21102001, IF INTERMEDIATE;                                    21097049
    I ~ IN4(TANK[ELB].[24:18]);                                         21097050
        IF I } 500 THEN BEGIN                                           21098000
            STMTSTACK[SSTK~SSTK+1] ~ (I-500)&                           21099000
                (STMTSTKLVL~OUTERLIMIT)[TOPLVL];                        21100000
            GO START;                                                   21101000
        END;                                                            21102000
PTCHX:                                                                  21103000
$ INCLUDE 21117001, IF INTERMEDIATE;                                    21103049
    IF NOT FLUSHD THEN OUTPUT;                                          21103050
        POB[0] ~ "LINK +1"&12[TOC0];                                    21104000
        OUTPUT;                                                         21105000
        IF ELB.CLASS ! VPATCHID THEN I ~ STMTSTACK[SSTK].PLOC;          21106000
        PATCHSPACE[I] ~ (*)&(SEQ-100)[TOPSEQN];                         21107000
        PATCHSPACE[I+1] ~ ABS(*)&CODEREC[TOPCODE];                      21108000
    IF ELB.CLASS ! VPATCHID THEN BEGIN                                  21109000
    IF STMTSTKLVL = (STMTSTKLVL~STMTSTACK[SSTK~SSTK-1].PLVL)THEN        21110000
            GO PTCHX;                                                   21111000
        IF DJTOG THEN BEGIN                                             21112000
            DJTOG ~ FALSE;                                              21113000
            IF TYPE=0 AND ELB=";" THEN GO START ELSE GO WRIT;           21114000
        END;                                                            21115000
        GO OT9;                                                         21116000
    END;                                                                21117000
        GO START;                                                       21118000
USUAL: MOVE(((TANK[ELB].CTR+10).WRD).IC,TANK[ELB],OA);                  21119000
WRIT:                                                                   21120000
$ INCLUDE 21127001, IF DEBUGGING;                                       21120049
    IF MONITER THEN                                                     21120050
        IF PRINT AND NOT INTERLIST THEN BEGIN                           21121000
            IF A~OA[0].CTR+1 < 117 THEN BEGIN % CLEAN OA FOR PRINT      21122000
                    BLANK(OA[15],0,14);                                 21123000
                    A ~ MOVER(120-A,OA,120,OA,A+3,0);                   21124000
                END;                                                    21125000
            WRITE(PRINTER, 15, OA[*]);                                  21126000
        END;                                                            21127000
    IF PREV AND OUTC!0 AND TYPE=0 THEN                                  21128000
        OUTC~ OUTC-1;                                                   21129000
    A ~ OA[0].CTR;                                                      21130000
    OA[0].C2 ~ " ";                                                     21131000
    I~ 3;                                                               21132000
    PREV~ TYPE!0;                                                       21133000
    IF OUTERLIMIT = STMTSTKLVL AND SSTK!0 THEN                          21134000
IF NOT DJTOG.[46:1] AND DJTOG THEN BEGIN OA[0].CTR~A;GO PTCHX;END       21135000
            ELSE IF ELB=";" THEN BEGIN                                  21136000
                FLUSHIT;                                                21137000
                GO PTCHX;                                               21138000
            END;                                                        21139000
    IF TYPE=VOTHERS THEN GO OUTCLASS[ELB.SUB-BEGINV+1];                 21140000
OT0: FLUSHIT;                                                           21141000
    IF ELB=";" AND UNPACK THEN                                          21142000
        OUTPUT;                                                         21143000
    GO OT9;                                                             21144000
OT1: IF NOT FLUSHD AND UNPACK THEN                                      21145000
        OUTPUT;                                                         21146000
    FLUSHIT;                                                            21147000
    GO OT9;                                                             21148000
OT3: IF NOT FLUSHD  AND UNPACK THEN                                     21149000
        OUTPUT;                                                         21150000
OT2: FLUSHIT;                                                           21151000
    IF UNPACK THEN                                                      21152000
        OUTPUT;                                                         21153000
OT9: IF NOT FLUSHD THEN                                                 21154000
        IF OUTC~ OUTC+REAL(PREV)=72 THEN                                21155000
            OUTPUT;                                                     21156000
    IF TYPE!VOTHERS AND ELB.SUB!BEGINV THEN                             21157000
    DJTOG ~ IF ELB=";" AND TYPE=0 THEN FALSE ELSE DJTOG AND <2>;        21158000
    GO TO START;                                                        21159000
END DRIVER BLOCK;                                                       21160000
        COMMENT  DP HANDLES THE CREATION OF A NEW LANGUAGE FILE.        25000000
    CHECKS ARE MADE THAT A COMPLETE FILE CAN BE OBTAINED BEFORE         25001000
    INAUGURATING THE DUMP. NO DECLARATIONS, INCLUDING MACROS CAN BE     25002000
    IN PROCESS, AND NO ERRORS CAN HAVE OCCURED;                         25003000
DP: BEGIN LABEL L; L:                                                   25004000
    IF OUTC > 0 THEN OUTPUT;                                            25005000
    IF MACROTOG OR MACT > 0 THEN ERROR(83);                             25006000
    IF DEC THEN ERROR(82);                                              25007000
    IF SEQ>0 AND SVT.[46:1] THEN ERROR(109);                            25007100
    IF ERRCT = 0 THEN BEGIN                                             25008000
        IF W~2 + (NEXTDICT+255).IR + (NXTUDICT+255).IR + (PT+255).IR    25009000
            + (DECP+255).IR + (CODEREC+27)DIV 28 + (NXTPTCHREC+27)      25010000
            DIV 28 + (GLAT-GALFBAS+1) + REAL(BLVL>1) +                  25011000
            REAL(GLAT}GALFBAS) + REAL(GLIT}GINTBAS) +                   25012000
            REAL(GLOT}GPTRBAS)                                          25013000
          > 50 THEN W ~ 50;                                             25014000
        BEGIN                                                           25015000
            FILE NEWLANGUAGE DISK SERIAL [20:W](2,256,SAVE 780);        25016000
            ARRAY GLOBSIZE[0:(GLAT-GALFBAS+4).D4];                      25017000
            FORMAT                                                      25018000
                DP1("NEW LANGUAGE FILE DUMPED TO "A1,A6"/"A1,A6);       25019000
            LABEL EOCODE;                                               25019500
            FILEPARAMETERS(NEWLANGUAGE,DFN[*]);                         25020000
            F ~ DFN[1];                                                 25021000
            IF MF~ DFN[0]=0 THEN                                        25022000
                FILL NEWLANGUAGE WITH MF~"MCALGOL";                     25023000
            WRITE(NEWLANGUAGE,*,DOLLARDICT,STREAMDICT,MACRODICT,        25024000
                ALGOLDICT,NEXTDICT,LASTDICT,NXTUDICT,LSTUDICT,          25025000
                BLVL,RCCNT,LANGUAGELEVEL,BECT,SEQ,PT,DECP~DECP.LIM,     25026000
                GLAT,GLIT,GLOT,NXTPTCHX,NXTPTCHREC,NXTCODE);            25027000
            WRITE(NEWLANGUAGE,250,DLIST[*]);                            25028000
            IF BLVL > 1 THEN BEGIN                                      25029000
                MOVE(100,BLIST,DFN);                                    25030000
                MOVE(128,STMTSTACK,DFN[100]);                           25031000
                DFN[228] ~ OUTERLIMIT;                                  25032000
                DFN[229] ~ STMTSTKLVL;                                  25033000
                DFN[230] ~ SSTK;                                        25034000
                MOVE(8,SPACEMAP[8],DFN[231]);                           25035000
                WRITE(NEWLANGUAGE,256,DFN[*]);                          25036000
            END;                                                        25037000
            FOR I~0 STEP 256 UNTIL NEXTDICT-1 DO                        25038000
                WRITE(NEWLANGUAGE,256,DICT[I.IR,*]);                    25039000
$ INCLUDE 25041001, IF ADVANCED;                                        25039999
            FOR I~0 STEP 256 UNTIL NXTUDICT-1 DO                        25040000
                WRITE(NEWLANGUAGE,256,UDICT[I.IR,*]);                   25041000
            IF BOOLEAN(MSTORP).PDP1 THEN                                25041900
                WRITE(MSTOR[MSTORP.PDF1],256,MCODE[*]);                 25041950
            FOR I~0 STEP 256 UNTIL PT-1 DO BEGIN                        25042000
                READ(MSTOR[I.IR],256,MCODE[*]);                         25043000
                WRITE(NEWLANGUAGE,256,MCODE[*]);                        25044000
            END;                                                        25045000
            IF BOOLEAN(MSTORP).PDP2 THEN                                25045900
                WRITE(MSTOR[MSTORP.PDF2+150],256,MDECL[*]);             25045950
            FOR I~0 STEP 256 UNTIL DECP-1 DO BEGIN                      25046000
                READ(MSTOR[I.IR+150],256,MDECL[*]);                     25047000
                WRITE(NEWLANGUAGE,256,MDECL[*]);                        25048000
            END;                                                        25049000
            IF GLAT } GALFBAS THEN BEGIN                                25050000
                A ~ GLAT - GALFBAS + 1;                                 25051000
                FOR I~0 STEP 1 UNTIL A DO                               25052000
                  IF INUSE(SPACEMAP[I.D32+8],I.M32) THEN                25053000
                    GLOBSIZE[I.D4] ~ * + GLOBAL[I.D4,I.M4|64].CTR;      25054000
                WRITE(NEWLANGUAGE,A~(A+3).D4,GLOBSIZE[*]);              25055000
                FOR I~0 STEP 1 UNTIL A DO                               25056000
                    IF GLOBSIZE[I] ! 0 THEN                             25057000
                        WRITE(NEWLANGUAGE,256,GLOBAL[I,*]);             25058000
            END;                                                        25059000
            IF GLIT } GINTBAS THEN                                      25060000
                WRITE(NEWLANGUAGE,GLIT-GINTBAS+1,GINT[*]);              25061000
            IF GLOT } GPTRBAS THEN                                      25062000
                WRITE(NEWLANGUAGE,GLOT-GPTRBAS+1,GPTR[*]);              25063000
            REWIND(CODE);                                               25064000
            OUTC~ 0;                                                    25065000
            FOR I~ 0 STEP 100 UNTIL SEQ-100 DO                          25066000
                BEGIN                                                   25067000
                    READ(CODE,9,ACCM[*])[EOCODE];                       25068000
                    MOVE(9, ACCM, OA[OUTC]);                            25069000
                    IF OUTC~ OUTC+9=252 THEN                            25070000
                        BEGIN                                           25071000
                            WRITE(NEWLANGUAGE, 252, OA[*]);             25072000
                            OUTC~ 0                                     25073000
                        END;                                            25074000
                END;                                                    25075000
EOCODE:                                                                 25075900
            IF OUTC>0 THEN                                              25076000
                WRITE(NEWLANGUAGE, OUTC, OA[*]);                        25077000
$ INCLUDE 25106001, IF INTERMEDIATE;                                    25077999
            IF NXTPTCHX}0 THEN BEGIN                                    25078000
                OUTC~0;                                                 25079000
                WRITE(NEWLANGUAGE,NXTPTCHX+2,PATCHSPACE[*]);            25080000
                FOR T~0 STEP 2 UNTIL NXTPTCHX DO BEGIN                  25081000
                    W~PATCHSPACE[T]; A~PATCHSPACE[T+1];                 25082000
                    BC ~ A.PUSED-1;                                     25083000
                    BW ~ W.PBASE;                                       25084000
                    FOR I~0 STEP 1 UNTIL BC DO BEGIN                    25085000
                        READ(BACKPATCHES[BW+BC],9,POB[*]);              25086000
                        MOVE(9,POB,OA[OUTC]);                           25087000
                        IF OUTC~OUTC+9=252 THEN BEGIN                   25088000
                            WRITE(NEWLANGUAGE,252,OA[*]);               25089000
                            OUTC~0;                                     25090000
                        END;                                            25091000
                    END;                                                25092000
                IF OUTC>0 THEN                                          25093000
                    WRITE(NEWLANGUAGE,OUTC,OA[*]);                      25094000
                END;                                                    25095000
            END;                                                        25096000
            IF NXTCODE ! 0 THEN BEGIN                                   25097000
                FOR I~0 STEP 1 UNTIL NXTCODE.CROW DO BEGIN              25098000
                    READ(ISOCODE[I],120,DFN[*]);                        25099000
                    MOVE(120,DFN,OA[I.[47:1]|120]);                     25100000
                    IF BOOLEAN(I) THEN                                  25101000
                        WRITE(NEWLANGUAGE,240,OA[*]);                   25102000
                END;                                                    25103000
                IF BOOLEAN(I) THEN                                      25104000
                    WRITE(NEWLANGUAGE,120,OA[*]);                       25105000
            END;                                                        25106000
            LOCK(NEWLANGUAGE);                                          25107000
            IF PRINT THEN BEGIN                                         25108000
                WRITE(PRINTER[DBL]);                                    25109000
                WRITE(PRINTER[DBL],DP1,MF.C1,MF,F.C1,F);                25110000
                END;                                                    25111000
            GO TO ENDDD                                                 25112000
            END;                                                        25113000
    END ELSE                                                            25114000
        RCCNT ~ -1;                                                     25115000
    END;                                                                25116000
        COMMENT  EOF IS REACHED BY READING BEYOND END OF FILE OR BY     26000000
    TABLE FINDING AN "END.". IF ERRORS HAVE OCCURRED THEY ARE LISTED    26001000
    IN ORDER. BACKPATCHES ARE LINKED IN IF USED. IF NO ERRORS OR A      26002000
    SAVE (AND NOT A SYNTAX RUN), THE CODE FILE IS LOCKED & THE          26003000
    CONTROL DECK (IF ANY) IS LISTED & ZIPPED;                           26004000
EOF: BEGIN                                                              26005000
%                                                                       26006000
        FORMAT                                                          26013000
            EJ1("NUMBER OF ERRORS ="I4,X12"LAST CARD IN ERROR "         26014000
                "HAS SEQUENCE #"),                                      26015000
            EJ2("?END"),                                                26016000
            EJ3("OUTPUT STRING LOCKED AS FILE:"A1,A6"/"A1,A6);          26017000
%                                                                       26018000
        SEQ~ 99999899;                                                  26019000
        BLANK(ERB,0,14);                                                26020000
        BW ~ BFRMAX-10;                                                 26021000
        BC~ 0;                                                          26022000
        IF BLVL>1 AND RCCNT}0 THEN                                      26023000
            DO                                                          26024000
                ERROR(50)                                               26025000
            UNTIL BLVL~ BLVL-1{1;                                       26026000
        OUTPUT;                                                         26027000
        OUTPUT;                                                         26028000
$ INCLUDE 26059001, IF INTERMEDIATE;                                    26028999
        IF NXTPTCHX}0 AND NOT SVT.[46:1] THEN BEGIN                     26029000
            POB[0] ~ "LINK +0"&12[TOC0];                                26030000
            WRITE(CODE,10,POB[*]);                                      26031000
            CODEREC ~ CODEREC + 1;                                      26032000
            FOR T~0 STEP 2 UNTIL NXTPTCHX DO BEGIN                      26033000
                W~PATCHSPACE[T]; F~PATCHSPACE[T+1];                     26034000
                BC ~ F.PUSED-1;                                         26035000
                BW ~ W.PBASE;                                           26036000
                SEQ~ W.PSEQN;                                           26037000
                PATCHSPACE[T] ~ CODEREC&BC[1:1:1];                      26038000
                FOR I~0 STEP 1 UNTIL BC DO BEGIN                        26039000
                    READ(BACKPATCHES[BW+I],9,POB[*]);                   26040000
                    SEQN(SEQ+I,POB[9]);                                 26041000
                    WRITE(CODE,10,POB[*]);                              26042000
                    CODEREC ~ CODEREC + 1;                              26043000
                END;                                                    26044000
                BLANK(POB,0,14);                                        26045000
                POB[9] ~                                                26045950
                POB[0] ~ "LINK   "&12[TOC0];                            26046000
                MOVEDEC(F.PLOC,POB[1],0,3,5,4);                         26047000
                WRITE(CODE,10,POB[*]);                                  26048000
                CODEREC ~ CODEREC + 1;                                  26049000
            END;                                                        26050000
            FOR T~NXTPTCHX STEP -2 UNTIL 0 DO BEGIN                     26051000
              IF PATCHSPACE[T]}0 AND PATCHSPACE[T+1]}0 THEN BEGIN       26052000
                BLANK(POB,0,9);                                         26053000
                POB[9] ~                                                26053950
                POB[0] ~ "LINK   "&12[1:43:5];                          26054000
                MOVEDEC(PATCHSPACE[T],POB[1],0,3,5,4);                  26055000
                WRITE(CODE[PATCHSPACE[T+1].PCODE-1],10,POB[*]);         26056000
              END;                                                      26057000
            END;                                                        26058000
        END LINK PATCHING;                                              26059000
        IF WRITEIT THEN                                                 26060000
            WRITE(PRINTER[PAGE]);                                       26061000
        IF IACCM~ERRCT>0 OR WARNING THEN                                26062000
            BEGIN                                                       26063000
            DEFINE ERRCT=IACCM#;                                        26064000
                WRITE(ERB[*],EJ1,ERRCT);                                26065000
                CLOSE(ERRORS);                                          26065050
                FILL ERRORS WITH "MCALGOL","ERRORS ";                   26065100
                MOVE(1, LINKS[1], ERB[9]);                              26066000
                WRITE(PRINTER[DBL], 15, ERB[*]);                        26067000
                FOR ERRCT~ 0 STEP 1 UNTIL 31 DO                         26068000
                  IF REAL(ATOG~ERRMASK[ERRCT])!0 THEN                   26069000
                    FOR T~0 STEP 1 UNTIL 31 DO                          26070000
                      IF REAL(MASK(T) AND ATOG)!0 THEN BEGIN            26071000
%                                                                       26072000
                            ERRORLINK(32|IACCM+T);                      26086000
                          END;                                          26087000
            END;                                                        26088000
        IF (SVT OR ERRCT=0) AND NOT SVT.[46:1] THEN                     26089000
          IF CODEREC > 0 THEN BEGIN                                     26090000
            IF PRINT THEN BEGIN                                         26090050
                SEARCH(CODE,ERB[*]);                                    26090100
                WRITE(PRINTER[DBL],EJ3,ERB[1].C1,ERB[1],                26090150
                  ERB[2].C1,ERB[2]);                                    26090200
            END;                                                        26090250
            LOCK(CODE);                                                 26091000
          END;                                                          26091500
        IF CCC>0 AND ERRCT=0 AND NOT SVT.[46:1] THEN                    26092000
            BEGIN                                                       26093000
                WRITE(CC,EJ2);                                          26094000
                IF CCLISTOG OR PRINT THEN BEGIN                         26095000
                    LABEL ALL; ARRAY A[0:9];                            26096000
                    REWIND(CC);                                         26097000
                    WRITE(PRINTER[DBL],<"LISTING OF CONTROL DECK">);    26098000
                    WHILE TRUE DO BEGIN                                 26099000
                        READ(CC,10,A[*])[ALL];                          26100000
                        WRITE(PRINTER,10,A[*]);                         26101000
                    END;                                                26102000
ALL:                END;                                                26103000
                ZIP WITH CC                                             26104000
            END;                                                        26105000
    END;                                                                26106000
ENDDD: IF WRITEIT THEN                                                  26107000
        TIMEIT(PRINTER);                                                26108000
    IF NEWFILE AND ERRCT=0 THEN LOCK(NEWDISK);                          26109000
    IF ERRCT=0 THEN                                                     26110000
        ERRCT~-1;                                                       27000000
IF INDEXING THEN BEGIN                                                  27001000
    REAL C,E,S,Q;                                                       27002000
    ARRAY Z[0:9],NAMES[0:12];                                           27003000
    BOOLEAN PROCEDURE INPUT(A); ARRAY A[0];                             27004000
    BEGIN                                                               27005000
        LABEL WHEW,NORM,EXIT;                                           27006000
        DEFINE APLACE = A[Q.WRD],Q.CHR #;                               27007000
    STREAM PROCEDURE SETUP(A,B,C,D,E,F,G); VALUE A,B,C,D,E,G;           27008000
    BEGIN                                                               27009000
        DI~F; DI~DI+G; SI~LOC A;                                        27010000
        DS~3 LIT "SZ:";  DS~5 DEC; F~DI; DI~DI-5; DS~4 FILL; DI~F;      27011000
        DS~5 LIT " LMS:";DS~4 DEC; F~DI; DI~DI-4; DS~3 FILL; DI~F;      27012000
        DS~6 LIT" LIMS:";DS~4 DEC; F~DI; DI~DI-4; DS~3 FILL; DI~F;      27013000
        DS~5 LIT " LMP:";DS~4 DEC; F~DI; DI~DI-4; DS~3 FILL; DI~F;      27014000
        DS~6 LIT" CONP:";DS~4 DEC; F~DI; DI~DI-4; DS~3 FILL;            27015000
    END;                                                                27016000
STREAM PROCEDURE MOV(NN,N,S,SS,D,DD);                                   27017000
    VALUE NN, N, SS, DD;                                                27018000
    BEGIN                                                               27019000
        SI~S; SI~SI+SS; DI~D; DI~DI+DD;                                 27020000
        NN(DS~ 32 CHR;DS~ 32 CHR);                                      27021000
        DS~ N CHR                                                       27022000
    END;                                                                27023000
                                                                        27024000
        READ(INDEX,10,Z[*])[WHEW];                                      27025000
        BLANK(A,0,14);                                                  27026000
        MOV(0,S~Z[1].LSCTR,Z[1],3,A[0],0); % ID                         27027000
        Q ~ IF S > 12 THEN S+1 ELSE 13;                                 27028000
        MOV(0,8,Z[9],0,APLACE); % SEQUENCE #                            27029000
        Q ~ Q + 9;                                                      27030000
        IF C~(E~Z[0]).CLASS = VMACRONAME THEN BEGIN                     27031000
            MOVEDEC(Z[8].MPT,APLACE,3,5,7);                             27032000
            Q ~ Q + 7;                                                  27033000
            IF S~E.SUB = LISTV THEN BEGIN                               27034000
                MOV(0,5,NAMES[0],0,APLACE);                             27035000
                Q ~ Q + 5;                                              27036000
            END ELSE                                                    27037000
            IF S = EQVV THEN BEGIN                                      27038000
                MOV(0,4,NAMES[1],0,APLACE);                             27039000
                Q ~ Q + 4;                                              27040000
            END;                                                        27041000
            MOV(0,6,NAMES[1],4,APLACE);                                 27042000
            IF Q { 60 THEN BEGIN Q ~ 63;                                27043000
            SETUP((S~Z[7]).MML,S.MLACT-27,S.MLICT-1,Z[8].MLOCT,         27044000
                S.MPOOL,APLACE) END;                                    27045000
        END ELSE                                                        27046000
        IF C = VSTRINGNAME THEN BEGIN % MSTRINGS & MPOINTERS            27047000
            MOVEDEC((C~Z[8]).REF,APLACE,3,5,7);                         27048000
            Q ~ Q + 7;                                                  27049000
            IF C.REF } GALFBAS THEN BEGIN                               27050000
                MOV(0,7,NAMES[2],2,APLACE);                             27051000
                Q ~ Q + 7;                                              27052000
            END ELSE BEGIN                                              27053000
                MOV(0,6,NAMES[3],1,APLACE);                             27054000
                Q ~ Q + 6;                                              27055000
            END;                                                        27056000
            IF BOOLEAN(S~E.SUB).[45:1] THEN BEGIN                       27057000
                MOV(0,20,NAMES[5],6,APLACE);                            27058000
                Q ~ Q + 20;                                             27059000
                GO EXIT;                                                27060000
            END ELSE                                                    27061000
            IF BOOLEAN(S) THEN BEGIN                                    27062000
                MOV(0,8,NAMES[4],0,APLACE);                             27063000
                Q ~ Q + 8;                                              27064000
            END ELSE BEGIN                                              27065000
                MOV(0,6,NAMES[5],0,APLACE);                             27066000
                Q ~ Q + 6;                                              27067000
            END;                                                        27068000
            IF BOOLEAN(S).[46:1] THEN BEGIN                             27069000
                MOV(0,9,NAMES[7],1,APLACE);                             27070000
                Q ~ Q + 9;                                              27071000
                GO EXIT;                                                27072000
            END;                                                        27073000
            MOV(0,8,NAMES[8],2,APLACE);                                 27074000
            Q ~ Q + 8;                                                  27075000
            IF S~C.LIM = 0 THEN                                         27076000
                MOV(0,3,NAMES[0],5,APLACE)                              27077000
            ELSE BEGIN                                                  27078000
NORM:           MOV(0,1,NAMES[0],5,APLACE);                             27079000
                Q ~ Q + 1;                                              27080000
                MOVEDEC(S-REAL(E.SUB=0),APLACE,4,4,7);                  27081000
                Q ~ Q + 4;                                              27082000
                MOV(0,1,NAMES[0],7,APLACE);                             27083000
            END;                                                        27084000
        END ELSE                                                        27085000
        IF C = VMFIELDID THEN BEGIN                                     27086000
            Q ~ Q + 7;                                                  27087000
            MOV(0,7,NAMES[9],2,APLACE);                                 27088000
        END ELSE BEGIN  % PROCEDURES                                    27089000
            Q ~ Q + 7;                                                  27090000
            MOV(0,10,NAMES[10],1,APLACE);                               27091000
        END;                                                            27092000
        IF FALSE THEN BEGIN                                             27093000
WHEW:       INPUT ~ TRUE;                                               27094000
            CLOSE(INDEX,PURGE) END;                                     27095000
EXIT:                                                                   27096000
    END INPUT;                                                          27097000
                                                                        27098000
    BOOLEAN PROCEDURE COMP(A,B); ARRAY A,B[0];                          27099000
    BEGIN                                                               27100000
        BOOLEAN STREAM PROCEDURE CMP(A,B); BEGIN SI~A; DI~B;            27101000
            IF 12 SC<DC THEN TALLY~1; CMP~TALLY END;                    27102000
        COMP ~ CMP(A[0],B[0]);                                          27103000
    END;                                                                27104000
    PROCEDURE HIV(A); ARRAY A[0];                                       27105000
    FOR S~0 STEP 1 UNTIL 14 DO A[S] ~ REAL(<1414141414141414>);         27106000
    PROCEDURE OUTPUT(LAST,A); VALUE LAST;                               27107000
    BOOLEAN LAST; ARRAY A[0];                                           27108000
        IF NOT LAST THEN                                                27109000
            WRITE(PRINTER,15,A[*]);                                     27110000
                                                                        27111000
FILL NAMES[*] WITH "LIST [*]","EQV MACR","O GLOBAL"," LOCAL  ",         27112000
    "INTEGER ","ALPHA DI","CTIONARY"," MPOINTE","R MSTRIN",             27113000
    "G MFIELD"," PROCEDU","RE      ";                                   27114000
    REWIND(INDEX);                                                      27115000
    WRITE(PRINTER[PAGE]);                                               27116000
    TIMEIT(PRINTER);                                                    27117000
    SORT(OUTPUT,INPUT,0,HIV,COMP,15,5000,16|INXR);                      27118000
    TIMEIT(PRINTER);                                                    27119000
END END;                                                                27120000
$ INCLUDE 27120190, IF COUNTING;                                        27120010
BEGIN                                                                   27120020
FORMAT CF1("SCANNERS A B S E:"4I10/                                     27120030
        "TABLE"I10" INDICT"I10/"PUT GET MOVER PUTTER:"4I10/             27120040
        "STACKING"2I10" STOR"I10" PSEUDO"I10/                           27120050
        I5" RESERVES FOR"I5" RECORDS;"I5" USED");                       27120052
WRITE(PRINTER,CF1,ASCT,BSCT,SSCT,ESCT,                                  27120060
    STEPCT,INDCT,PUTCT,GETCT,MOVCT,PTRCT,                               27120070
    STKCT,STKMX,MSTCT,BFRMAX-PSEU                                       27120080
$ INCLUDE 27120083, IF INTERMEDIATE;                                    27120081
    ,NXTPTCHX.D2,NXTPTCHREC,PTCHCT                                      27120082
    );                                                                  27120088
END;                                                                    27120090
END.                                                                    27121000
                                                                        27122000
                                                                        27123000
                                                                        27123100
%                                                                       99999999
                                                                        99999999
