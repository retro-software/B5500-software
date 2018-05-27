$ TAPE LIST                                                                     
                                                                        00000001
                                                                        00000002
COMMENT:  SHIPPING "CONTROL" EDITS OUT NON-STANDARD USAGES TO YIELD %Y9600000003
    A VERSION WHICH COMPILES ON A STANDARD SYSTEM.  SPECIAL SYSTEM  %Y9600000004
    DEPENDENT AND EXPERIMENTAL FEATURES ARE ELIDED.  REMNANTS OF    %Y9600000005
    EXCISED PATCHES MAY STILL REMAIN.  -- JANUARY 11, 1969 -- ;     %Y9600000006
COMMENT SUMMARY OF WESTINGHOUSE PATCHES                             %W-000000099
W-0  MISCELLANEOUS F69                                              %W-000000100
W-2  TIMEIT,TIMEITTOG, ETC. F76                                     %W-200000102
W-3  OCTAL CONSTANTS AND FILL IMPROVEMENTS                          %W-300000103
W-4  FLUSH                 NEEDS 7, 9, 10                           %W-400000104
W-5  LISTING FORMAT CHANGES F69,70,85,86                        %85 %W-500000105
W-6  MISCELLANEOUS FILE DECLARATIONS F69                            %W-600000106
W-7  $+ AND $- F65                                                  %W-700000107
W-8  STACKING OF LIST, PRT AND DEBUG  (ASSUMES 7)                   %W-800000108
W-9  ERROR DISPLAY CHANGES F69                                      %W-900000109
W10  WARNING MESSAGE F69                                            %W1000000110
W11  LONG JUMPS IN STREAM PROCEDURES                                %W1100000111
W12  ERROR RECOVERY                                                 %W1200000112
W13  $ PURGE OPTION.  ASSUMES 9                                     %W1300000113
W14  EFFICIENT STREAM PROCEDURE CODE F69                            %W1400000114
W15  CHANGES IN FILE SIZES.                                         %W1500000115
W16  ERROR DOCUMENTATION  (NEEDS 2 AND 4)                           %W1600000116
W17  $ DISK AND NEW DISK  (NEEDS 4) F77                             %W1700000117
W20  SOURCE IDENTIFICATION FLAGS                                    %W2000000120
W21  IMPEMENT CI ~ CI + <INTEGER>                                   %W2100000121
W22  CORE SIZE ESTIMATE                                             %W2200000122
W23  PATCH TO PROTECT PROCEDURE STACK                               %W2300000123
W24  "END *" CONSTRUCT                                              %W2400000124
W25  UNDECLARED LABELS IN STREAM PROCEDURES                         %W2500000125
W27  STEPI AND STEPIT F60                                           %W2700000127
W28  $ STOP OPTION                                                  %W2800000128
W30  SEGMENT TOO LONG                                               %W3000000130
W31  SUPPRESS SEGMENT ZERO MESSAGE                                  %W3100000131
W32  SUPPRESS STREAM PROCEDURES ON $ -STREAM,87                     %W3200000132
W33  IMPLEMENTATION OF NEW LIST SYNTAX F67,72,73                    %W3300000133
W35  PERMITS COLUMN 72 OF $$ CARD TO BE NON-BLANK                   %W3500000135
W37  ASSUME SINGLE SPACING UNLESS $ DOUBLE. F79                     %W3700000137
W40  EXPLICIT FORMAT. F65                                           %W4000000140
W43  COMPILER PRODUCED INDEX AND XREF F61,63,64,66,67,69            %W4300000143
W46  ADDITIONAL ERRORS IN COMPILOR NOT LISTED BY BURROUGHS. F69     %W4600000146
W47  BLANK OUT CALL ON DATIME CF 02231050.                          %W4700000147
W48  EFFICIENCY INCREASED BY SEGMENTATION F60,69,74                 %W4800000148
W49  RECORD COUNT INSERTED.  EXTEND EOF LABEL. F70                  %W4900000149
W50  ELIMINATE ERROR IN BUR. OCTAL CONSTANT CONSTRUCT(DOL QUOTE)    %W5000000150
W51  INTRODUCTION OF PATCHTOG OPTION F71,76                         %W5100000151
W52  IMPLEMENT ~* OPERATOR FOR IMPLIED P(DUP,LOD) F69               %W5200000152
W53  IMPLEMENT MULTI-RELATIONAL OPERATORS (A<B<C<...)               %W5300000153
W55  CORRECT VARIABLES FOR COMPUTING CORE ESTIMATE FOR BUFFERS. F69 %W5500000155
W56  PROCEDURE CCSCAN (USED BY W57 ET AL)                           %W5600000156
W57  HANDLING OF QM LINK AND QM END CARDS F59,61,65,84          %84 %W5700000157
W58  LIST FIRST FIVE ERRORS ON TWX AND TOTAL ERRORS  F58,74         %W5800000158
        TO BE REPLACED BY BURROUGHS --W99                           %W5800000159
W60 UV PARA DEFINES AND MOVE DEC ADTOG V-22                         %W6000000160
W61 USE COMMON(NEWDISK)-XREF,NO CODE WRITEN SYTAX CHK OR ERR,87     %W6100000161
W62 COLLECT USAGES OF $$ CARDS--LIBRARY AND XREF FILE DEC F62,76    %W6200000162
W65 ALLOW "%" AS EOC SENT.--MODIFY SCANNER--TABLE F65               %W6500000165
W66 ELIMINATE $" OCTAL CONSTANT F66,69                              %W6600000166
W74 EFF NOTES 519-523,NOTE P.O.,530-540, F69,74,79                  %W7400000174
W75 INLINE STREAM STATEMENTS, STACK PRIMARY, AND DESC USING "[...]".%W7500000175
W76 $SET, $OMIT, AND $INCLUDE (ALWAYS LIST),87                      %W7600000176
W77 $+MERGE, DISK FILE MERGE PATCHES DISK OR TAPE MAKES PSUEDO-DISK %W7700000177
W78 $+DOL TO LIST ALL DOLLAR CARDS                                  %W7800000178
W79 $+PAIRS--LIST BEGIN-END PAIRS AS "BNNN DD", N=NUMBER, D=DEPTH   %W7900000179
W82 FOR TIMING TESTS (NOT NORMALLY COMPILED IN)                     %W8200000182
W89 RETURN LIBRARY BUFFERS,REDUCE CORE LOADING,W62,W17,W76,SAVE LIN %W8900000189
W90 FOR BASIC-RESIZE(UVA UNCORE),TIMED,F92                          %W9000000190
W93 FLAG ALL W CONSTRUCTS IN ALL ALGOL COMPILERS                    %W9300000193
W99 TEMP CHANGES TO COME UP ON W9--WTIMEIT&TWX STILL IN             %W9900000198
END OF WESTINGHOUSE PATCHES;                                        %W-000000199
COMMENT   SUMMARY OF BURROUGHS PATCHES                              %B-000000200
END OF BURROUGHS PATCHES;                                           %B-000000299
COMMENT DESCRIPTION OF CONTENTS OF FILES                            %W-100000300
F58  LIST FIRST FIVE ERRORS ON TWX AND TOTAL ERRORS                 %W-100000358
F59 MISSING 1ST BEGIN M2 GAVE EOF-M7 FLUSHES W57                    %W-100000359
F60 UV PARA DEFINES AND MOVE DEC ADTOG V-22                         %W-100000360
F61 USE COMMON(NEWDISK)-XREF,CUT PRT,SAVE XREF INPUT,NO CODE SYN    %W-100000361
F62 COLLECT USAGES OF $$ CARDS--LIBRARY AND XREF FILE DEC           %W-100000362
F63 REMOVE XREF-SORT,LOCK XREF FILE,SETUP CONTROLDECK XREF/ALGOL    %W-100000363
F64 ALLOW XREF ON TAPE EASILY, REDO NAME CHANGE AND ZIP FOR OPRS    %W-100000364
F65 ALLOW "%" AS EOC SENT.--MODIFY SCANNER--TABLE                   %W-100000365
F66 CLEAN XREF FILE HANDLING, WARNING 99 ON $" OCTAL CONSTANTS      %W-100000366
F67 JUSTIFY BURROUGHS MARK 8 PATCHES 6 AND 7                        %W-100000367
F69 SYN ER $",0 SEG 0,RESEG TABLE,FLG L MARG SEQ ER,REMOTE=14,NOTES %W-100000369
F70 PRINT NEWDISK -TAPE REC # IN RIGHT MARGIN OF LISTING            %W-100000370
F71 SEQ # ON $$ CARD PUT IN NEWDISK,LISTED IF ACTUALLY IN NEW FILE  %W-100000371
F72 DROP W68,PUT W67 BACK IN AND PATCH                              %W-100000372
F73 RESTRICT FOR <LIST> CONSTRUCT TO PROCEDURE BODIES               %W-100000373
F74 EFF NOTES 519-523,NOTE P.O., MOD PROCSTMT,TWX WAIT TIME         %W-100000374
F75 INLINE STREAM STMTS,STACK PRIMARY, DESC USING [...],ERR91-97%83 %W7500000375
F76 $SET,OMIT,INCLUDE,VOID--ALWAYS LIST,MOVE SEARCHLIB,DOLLARCHK%83 %W7600000376
F77 $+MERGE COVERS DISK,ERR 499,**FOR LIB,W57,20,17,10,49,9,25,0%83 %W7700000377
F78 $+DOL TO LIST ALL $ CARDS--IF LIST IS ON                    %83 %W7800000378
F79 $+PAIRS TO LIST BEGIN-ENDS IN LEFT MARGIN--IF LIST IS ON    %83 %W7900000379
F82 TIMING TESTS--$SET--TIMES AMT SPENT IN SCANNER & 01ALGOL    %83 %W8200000382
F83 PULL UVA DEFINE--W60,REPLACE WITH B10,CORRECT ERR MESSAGES      %W8300000383
F84 REVISION OF EOF ACTION IN NEXTREC W57                       %84 %W5700000384
F85 LIST OF FILES USED W-5                                      %85 %W-500000385
F86 CREATION TIME AND DATE INTRINSICS--8 CHR STRINGS W-5        %86 %W-500000386
F87 NOSYN,ADDVALUE,SEQSUM,$OMIT,UPLEVEL IF PRT,STREAMTAG,ARRAYS     %W-100000387
F91 EFFICIENCY NOTES 530-540                                        %W7400000391
F92 MISC-TIMED                                                      %W-000000392
F61A ALL OUTPUT FILE ARE PURGED AFTER 1ST SYNTAX ERR SUPP61A    %83 %W6100000398
END OF FILE DESCRIPTIONS;                                           %W-100000399
         091          INLINE:  DESIGNATIONAL EXPRESSION NOT ALLOWED %W7500069991
         092          INLINE:  NOT EXPRESSION TYPE (ONE BELOW MKS)  %W7500069992
         093          INLINE:  MISSING ")" IN INLINE STREAM CALL.   %W7500069993
         094          INLINE:  MISSING "(" IN INLINE STREAM CALL.   %W7500069994
         095          INLINE:  MISSING ".," AFTER INLINE STREAM CALL%W7500069995
         096          INLINE:  EXTRA ":" IN INLINE STREAM CALL.     %W7500069996
         097          INLINE:  MISSING "~" IN INLINE STREAM CALL.   %W7500069997
099 FLAG $" OCTAL CONSTANTS.                                        %W6600069999
170          MISSING PROGRAM--NO BEGIN.                         %59 %W5700164200
190  "?" CARD NOT ALLOWED IN NON-DISK-TYPE CARD                 %61 %W5700164300
191  "? LINK" TARGET OUTSIDE FILE BOUNDARIES                    %61 %W5700164400
193  "?" IN COLUMN 1 SHOULD BE AVOIDED                          %61 %W5700164500
194  INVALID RECORD SPECIFIER IN "? LINK" CARD                  %61 %W5700164600
 275          STRMSTMT:     JUMP LONGER THAN 63 SYLLABLES (WARNING) %W1100206500
276 STREAM PROCEDURE NOT PERMITTED                                  %W3200206600
305  TABLE :  INVALID OCTAL CONSTANT .                              %W4600218101
306  TABLE :  INVALID OCTAL CONSTANT .                              %W4600218102
310 UNKNOWNSTMT:IMPROPER ERROR NAME IN A SET STATEMENT.             %W1200218110
311 UNKNOWNSTMT: IMPROPER TARGET ITEM IN A SET STATEMENT.           %W1200218120
312 UNKNOWNSTMT: TARGET ITEM IN SET STATEMENT MUST HAVE PRT ADDRESS.%W1200218130
313 UNKNOWNSTMT: TARGET PROCEDURE IN A SET STATEMENT MAY NOT HAVE   %W1200218140
        PARAMETERS.                                                 %W1200218150
314 TABLE: ERROR OPTION MAY NOT APPEAR AFTER FIRST BEGIN.           %W1200218160
315  IMPROPERLY PLACED LOAD CARD                     %3ALGOL        %W6100218161
316  ALGOL COMMON MUST BE SAME AT DUMP&LOAD TIMES    %3ALGOL        %W6100218162
431  IMPROPER END-OF-FILE LABEL IN READ OR SPACE STATEMENT          %W6100259000
432  IMPROPER PARITY IN SPACE OR READ STATEMENT                     %W6100260000
 475          FOR LISTNAME NOT FOLLOWED BY DO                       %W3300305006
 476        LISTGEN:  LISTGEN MAY NOT BE CALLED RECURSIVELY.        %W3300305007
499     IODEC:  IMPROPER FILE TYPE                              %77 %W-000305009
510 FORSTLIST: FOR <LIST> DO <STATEMENT> IN PROC BODY ONLY      %73 %W3300305100
519     GENGO:  DESIGNATIONAL EXPRESSION                        %91 %W7400305190
520  PROGDESCBLDR: ACCIDENTAL ENTRY DESCRIPTOR                      %W7400305200
521  PROGDESCBLDR: PROCEDURE DESCRIPTOR                             %W7400305210
522  PROGDESCBLDR: LABEL DESCRIPTOR                                 %W7400305220
523  PROGDESCBLDR: STREAM PROCEDURE DESCRIPTOR                      %W7400305230
 524  PARADEFINE: PARAMETER LIST NOT PROPERLY TERMINATED        %85 %B1000305240
 525  PARADEFINE: MISSING ")" OR "]"                            %85 %B1000305250
 526  PARADEFINE: IMPROPERLY FORMED PARAMETER                   %85 %B1000305260
 527  PARADEFINE: TOO MANY PARAMETERS                           %85 %B1000305270
528 END-OF-FILE REACHED BEFORE "END. ".  MISSING CLOSING BRACKET%84 %W5700305280
 529 FLAG BIT IN QUOTED STRING NOT ALLOWED                      %86 %W-500305290
530     ACTUALPARAPART:  NAME-CALL EXPRESSION REQUIRES          %91 %W7400305300
            CONSTRUCTION OF SUBROUTINE (EFFICIENCY)             %91 %W7400305301
531     ACTUALPARAPART:  SIMPLE SWITCH PARAMETER REQUIRES       %91 %W7400305310
            CONSTRUCTION OF SUBROUTINE (EFFICIENCY)             %91 %W7400305311
532     GOSTMT:  NON-LOCAL SWITCH.                              %91 %W7400305320
533     GOSTMT: NON-LOCAL LABEL.                                %91 %W7400305330
534     ACTUALPARAPART:  LABEL PARAMETER                        %91 %W7400305340
535  BLOCK:  EN--THIS SWITCH CONTAINS NON-LOCAL LABELS          %91 %W7400305350
536  BLOCK:  EN--THIS SWITCH WAS DECLARED FORWARD               %91 %W7400305360
537     GOSTMT:  SWITCH USED MORE THAN ONCE (EFFICIENCY)        %91 %W7400305370
538     EMITUP: THIS POWER IS OBTAINED USING LOG AND EXP (EFF)  %91 %W7400305380
539     ARRAE:  NONZERO LOWER BOUND IS LESS EFFICIENT           %91 %W7400305390
540     ARRAE:  VARIABLE BOUND SPEC IS LESS EFFICIENT           %91 %W7400305400
776     TABLE:  "$ STANDARD" USED                                   %W9300305774
777     TABLE:  "$ ERROR" USED                                      %W9300305777
778     TABLE:  "$ STOP" USED                                       %W9300305778
779     TABLE:  "$ NEW TAPE" USED                                   %W9300305779
780     TABLE:  "$ PURGE" USED                                      %W9300305780
781     TABLE:  "$ XREF" USED                                       %W9300305781
782     TABLE:  "$ SAVE" USED                                       %W9300305782
783     TABLE:  "$ INDEX" USED                                      %W9300305783
784     TABLE:  "$ +DOUBLE" USED                                    %W9300305784
785     TABLE:  "$ STREAM" USED                                     %W9300305785
786     TABLE:  "$ PATCH" USED                                      %W9300305786
787     TABLE:  "$ NOTES" USED                                      %W9300305787
788     TABLE:  "$ MERGE" USED                                      %W9300305788
789     TABLE:  "$ DOL" USED                                        %W9300305789
790     TABLE:  "$ PAIRS" USED                                      %W9300305790
791     TABLE:  "$ FILES" USED                                      %W9300305791
792     TABLE:  "$ INFO" USED                                       %W9300305792
793     TABLE:  "$+" OR "$-" USED                                   %W9300305793
794     TABLE:  "$ NEW DISK" USED                                   %W9300305794
795     TABLE:  "$ DISK" USED                                       %W9300305795
796     USEMODULES: "$ OMIT" OR "$ INCLUDE" USED                    %W9300305796
797     SETMODULES: "$ SET" USED                                    %W9300305797
798     NEXTREC:  "? END" CARD USED TO STOP READING IN A DISK FILE  %W9300305798
799     FORSTLIST: SET OF LIST NAMES ALLOWED AS FOR INDICES         %W9300305799
800     DOTSYNTAX:  PARTIAL WORD OPERATION                          %W9300305800
801     CHECKER: UPLEVEL ADDRSING ALLOWED IF PRT CELL               %W9300305801
802     CHECKBOUNDLVL: DYNAMIC ARRAY DEC ALLOWED AT OUTER LEVEL     %W9300305802
803     FILLSTMT: BLANK ALLOWED FOR ZERO                            %W9300305803
804     PARSE:  CONCATENATE OPERATION                               %W9300305804
805     FILLSTMT&TABLE: WRL OCTAL CONSTANT                          %W9300305805
806     OCTCON: WARNING IN LIEU OF ERROR                            %W9300305806
807     TABLE: 8 CHR STR ALLOWED IF NO FLAG BIT                     %W9300305807
808     TABLE: DATE&TIMECOMPILED INTRINSICS FOR OBJ PROG IDENT      %W9300305808
809     UNKNOWNSTMT: SET SYNTAX IS UNIQUE TO WRL ERROR OPTION       %W9300305809
810     BLOCK: BRACKET CONVENTION IS NOT STANDARD                   %W9300305810
811     IMPFUN: CHECK VALUE OF THIS EXPRESSION SINCE (SEE 812)      %W9300305811
812     IMPFUN: ONLY ARGUEMENT VALUES OF 0,1,2,3,4 ARE STANDARD     %W9300305812
813     INDEXS: USE OF INTEGER TO INDEX "CI" IS UNIQUE TO WRL       %W9300305813
814     DECLARELABEL: UNDECLARED LABEL ALLOWED--UNIQUE TO WRL       %W9300305814
815     ACTUALPARAPART:  ACTUAL LIST ALLOWED IN LIEU OF LIST NAME   %W9300305815
816     FORSTMT:  LIST NAME ALLOWED AS "FOR" INDEX                  %W9300305816
817     LISTELEMENT:  LIST IDENTIFIER ALLOWED AS LIST ELEMENT       %W9300305817
818     EXPLICITFORMAT: EXPLICITFORMAT IS NOT STANDARD              %W9300305818
819     PRIMARY OR VARIABLE: "~*" OPERATOR UNIQUE TO WRL            %W9300305819
820     RELATION: MULTIPLE RELATIONAL OPERATORS ARE UNIQUE TO WRL   %W9300305820
821     NEXTREC:  "? LINK" CARDS USED TO CONTROL READING DISK FILES %W9300305821
822     INLINE:  INLINE STREAM CODE IS UNIQUE TO WRL                %W9300305822
823     UNKNOWNSTMT: RESIZE SYNTAX IS UNIQUE TO WRL                 %W9300305823
824     UNKNOWNSTMT: TIMED SYNTAX IS UNIQUE TO WRL                  %W9300305824
825     LISTPARA: BRACKETS USED TO DELIMIT ACTUAL LIST              %W9300305825
826     PRIMARY OR BOOPRIM: STACK SYNTAX IS UNIQUE TO WRL           %W9300305826
827     LISTELEMENT:  SWITCH LIST DESIGNATOR ALLOWED AS LIST ELEMENT%W9300305827
830     BLOCK: STREAM PROCEDURE                                     %Y9500305830
870     TABLE: INCORRECT "$ ZERO" CARD.                             %Y9500305870
    900  MISMATCHED PARENTHESES                                     %W7600305900
    901  MISSING BOOLEAN OPERAND                                    %W7600305901
    902  TOO MANY BOOLEANS SET                                      %W7600305902
    903  MISSING COMMA                                              %W7600305903
    904  MISSING "IF"                                               %W7600305904
    905  UNRECOGNIZED WORD OR CHARACTER                             %W7600305905
    906  MISSING SEQUENCE NUMBER                                    %W7600305906
    907  MISSING SEMICOLON                                          %W7600305907
    908  MISSING BOOLEAN ID, "=", OR TRUTH VALUE ON SET CARD        %W7600305908
909 OVER 100 SYNTAX ERRORS                                          %W-400305909
BOOLEAN SHIPTOG; % TRUE TO WRITE PATCHES FOR CUBE                   %Y9600504900
REAL HOWFAR; % MAKE SEQ # AVAIL TO OT27 QUERY                       %W-000506500
SAVE ARRAY IMAGES[0:4, 0:9]; % FOR INPUT RECORDS                %84 %W5700507500
BOOLEAN ARRAY DISKTYPE[0:4];                                        %W7700507600
ARRAY LINKCT, FNAME [0:4];                                          %W7700507700
 BOOLEAN CARDEOF;                                                   %W5700507800
DEFINE NOSYN = IF ERRORCOUNT=0 THEN #;                              %W6100507900
BOOLEAN PGSKTOG;    COMMENT TRUE IF PAGE SKIP PERMITTED;            %W4300527500
DEFINE LOGMAX =11  #,                                               %W8200527501
        LOGCONST = 0.146#;                                          %W8200527521
                                                                        00527523
DEFINE LOGIN=#, LOGOUT=#;                                           %W8200527524
                                                                        00527526
                                                                        00527531
                                                                        00527541
                                                                        00527551
                                                                        00527561
                                                                        00527571
                                                                        00527581
                                                                        00527591
                                                                        00527596
BOOLEAN SAVETOG,NOTETOG; % TRUE IF NOTES                   %64  %69 %W4300527600
BOOLEAN NO519; % STOPS NOTE 519 FOR ACTION LABELS,SWITCHES      %91 %W7400527602
SAVE INTEGER ARRAY LOG,LOGCNT,LOGRTN[0:LOGMAX];                     %W8200527611
INTEGER TIMEV,LOGINX;                                               %W8200527621
                                                                        00527998
                                                                        00527999
IF SAVETIME > 0 THEN SAVETIME ~ 55; % TO FLAG 55ALGOL CODE FILES    %W9300528500
            BEGIN RR1~2; RR2~30; DISKTYPE[1]~TRUE END;              %Y9500530000
        IF PGSKTOG~EXAMIN(RR11+5)!12  THEN RR3~4 ELSE               %W4300531000
            BEGIN RR3~2; RR4~30 END;                                %Y9500532000
            BEGIN RR5~2; RR6~10; RR7~30 END ELSE                    %Y9500534000
            BEGIN RR5~2; RR6~56; RR7~10 END;                        %Y9500535000
            BEGIN RR8~10; RR9~30; DISKTYPE[2]~TRUE END ELSE         %Y9500537000
        IF EXAMIN(RR11+20)=12 THEN RR10~30;                         %Y9500539000
IF EXAMIN(RR11+40) = 12 THEN SAVETOG ~ TRUE;                    %64 %W4300540000
            % SEE 00530000 AND 00537000                             %W5700600000
         DISKTYPE[3] ~ TRUE;                                        %W5700600100
        DISKTYPE[4] ~ TRUE;                                         %W7700600150
         LINKCT[0] ~ 190;  COMMENT ERROR TYPE FOR QM CARDS;         %W5700600200
DEFINE PHONEYDEC= LABEL FORCESEGMENT; FORCESEGMENT: #;              %W4801000060
INTEGER LASTELCLASS;         %SAVES ELCLASS                         %W4001000713
BOOLEAN PATCHTOG;                                                   %W5101001050
BOOLEAN STREAMTAG;                                                  %W3201001500
BOOLEAN DOUBLETOG;  COMMENT USED TO INDICATE DOUBLE OR SINGLE SPACE;%W3701001600
$ OMIT 01002121, IF SHIPPING;                         % CHANGCS     %Y9601002099
                                                                        01002100
                                                                        01002110
                                                                        01002120
DEFINE WRL(WRL1)=BEGIN IF LASTUSED!7 THEN FEATURE(WRL1) END#;       %W9301002200
PROCEDURE FEATURE(N); VALUE N; INTEGER N; FORWARD; % RECORD USE     %W9301002300
SAVE ARRAY FEATURES[0:9]; % 64 6-BIT COUNTERS AND 96 FLAGS          %W9301002400
BOOLEAN WARNTOG;                                                    %W9301002500
DEFINE REC(REC1)=BEGIN IF LASTUSED!7 THEN BEGIN        % RECORD     %W9301002600
            WARNTOG ~ FALSE  & WARNTOG [18:33:15];     % USES       %W9301002700
                 FEATURE(REC1);                        % WITHOUT    %W9301002800
                 WARNTOG ~ WARNTOG.[18:15] END END #;  % WARNINGS   %W9301002900
 BOOLEAN TIMEITTOG; COMMENT TRUE IF TIMEIT HAS BEEN CALLED;         %W-201300010
BOOLEAN MERGETOG, MERGELAST;  % MERGE CONTROL                       %W7701330200
INTEGER MLCR, SLCR; % FOR MERGE AND DISK(SAVED) COMPARISONS         %W7701330400
BOOLEAN DOLTOG; %LIST ALL DOLLAR CARDS                              %W7801330600
BOOLEAN PAIRSTOG,IMAGE;%ON FOR BEGIN-END P.O.%CARD IMAGE IN LIN     %W7901330800
INTEGER PAIRCNT, PAIRLVL; %PAIR COUNT AND NESTING DEPTH             %W7901330850
INTEGER ARRAY PAIRARR[0:31]; %STORE COUNT VALUE AT EACH DEPTH       %W7901330900
BOOLEAN DISKTOG,NEWDSK,  STOPTOG;                                   %W1701339000
BOOLEAN ERROROPTION;                                                %W1201339001
INTEGER ARRAY RECORDCNT[0:6]; % NEXTREC.  5 IS NEWDISK#, SEQSUM IN 6%W7701339002
COMMENT SEE 01561054 OF FILE 77;                                %77 %W1701339003
REAL XPRE,XSUF;                                                 %63 %W4301384200
    BOOLEAN SEGTOG;                                                 %W3101398500
 BOOLEAN PURGETOG; COMMENT TRUE IF "TAPE" IS TO BE PURGED;          %W1301411500
 BOOLEAN SLABTOG; COMMENT USED TO SUPPRESS STREAM NOPS;             %W1401411600
 SAVE ARRAY LASTCARD[0:9];                                          %W5701510000
REAL TWXCOUNT, % NUMBER OF ERRORS LISTED ON REMOTE                  %W5801521000
     TWXTIME; % ELAPSED TIME AVAIL TO LIST ERRORS.  SIX BUFFERS--   %W5801521100
              % ONE FOR FINAL MESSAGE, FIVE FOR ERRORS. IF POS ONE  %W5801521110
              % IS AVAIL THUS INITIAL VALUE 2400=4|60|10 IN 60THS.  %W5801521120
              % 10 SECONDS TO WRITE MESSAGE IS CONSERVATIVE         %W5801521130
BOOLEAN TWXOFF; % TRUE IF TOO FREQUENT MESSAGES                     %W5801521200
INTEGER CHUNK;                                                     %%W1501556100
FILE OUT CODE DISK [20:CHUNK~IF ERRORCOUNT MOD 10000=0 THEN 40 ELSE %W1501556200
(ERRORCOUNT MOD 10000+9) DIV 10|10]                                 %W1501556250
(5,30, SAVE SAVETIME);                                              %W1501556300
FILE OUT LINE DISK SERIAL[20:IF ERRORCOUNT DIV 10000=0 THEN 300     %W1501559000
ELSE (ERRORCOUNT DIV 10000+19)DIV 20|20](RR3,15,RR4,SAVE 10);   %92 %W1501559005
SAVE % KEEP LIN FROM OLAY                                           %W8901559009
DEFINE WRITELINE=IF DOUBLETOG THEN WRITE(LINE[DBL],15,LIN[*]) ELSE  %W3701559030
    WRITE(LINE,15,LIN[*])#;                                         %W3701559035
LABEL ERRORPRINT;                                                   %W1601559050
SAVE FILE OUT NEWTAPE DISK SERIAL [20:IF ERRORCOUNT DIV 10000=0     %W1501560000
THEN 300 ELSE(ERRORCOUNT DIV 10000+29)DIV 30|30]                %92 %W1501560100
        (RR5,RR6,RR7,SAVE 1);                                       %W1501560200
    FILE IN CASTA DISK SERIAL "CASTA" "LIBRARY"(2,BUFFSIZE);        %W8901561020
    FILE IN CASTB DISK SERIAL "CASTB  " "LIBRARY" (2,BUFFSIZE);     %W-601561030
    FILE IN CASTC DISK SERIAL "CASTC  " "LIBRARY" (2,BUFFSIZE);     %W-601561040
FILE OUT INDEXANDXREF DISK SERIAL[20:IF ERRORCOUNT DIV 10000=0 THEN %W6101561041
60 ELSE (ERRORCOUNT DIV 10000 + 9) DIV 10|10](2,30,SAVE 100);       %W6201561042
FILE OUT NEWDISK DISK SERIAL[20:IF ERRORCOUNT DIV 10000=0 THEN      %W1701561045
300 ELSE(ERRORCOUNT DIV 10000+29)DIV 30|30](2,10,30,SAVE 10);       %W1701561046
    FILE IN ERRORS DISK RANDOM"ALGOL""ERRORS"(1,10,30);             %W1601561053
FILE IN DISK 12 (2,10,30); FILE IN MERGE 12 (2,10,30);              %W1701561054
ALPHA FILE OUT TWX 14(6,8);FILE STAT DISK RANDOM (1,15,30);         %W6201561057
ARRAY STATARRAY[0:14];                  INTEGER STATEOF;        %89 %W6201561058
ARRAY PRTABLE, TPRT[0:29];   DEFINE DATA=INDEXANDXREF#;         %61 %W4301561059
INTEGER LIBINDEX, LTLCR, MAXLTLCR, FILEINX;                     %87 %W-101561120
DEFINE SEQSUM = RECORDCNT[6]#; % AVOID APPARENT DESCRIPTORS IN PRT  %W-101561125
INTEGER XENDI, XREFC, LASTHM, XREFCT; REAL OLDTT;                   %W4301562500
BOOLEAN INDEX,XREF; INTEGER ISEQ;                               %64 %W4301562600
        LASTSEQUENCE = 50#, % ADDED 10 FOR CREATION INTRINSICS  %86 %W-501575000
    DEFINE IFCODE= IF SAVETIME } 0 AND ERRORCOUNT = 0 THEN #;       %W6101688005
BOOLEAN FILESTOG; % TO PRINT OUT SOURCE FILES                   %85 %W-501694500
    %W75 TO CONTROL INLINE STREAM.  (TAG OFF) IMP (TOG OFF).        %W7501695490
BOOLEAN INLINETOG;                                                  %W7501695500
    %       [47:1]  PROCESSING CALL                                 %W7501695520
    %       [46:1]  ONE PARAMETER BELOW MARK STACK--EXPRESSION TYPE %W7501695530
BOOLEAN INLINETAG; %INLINE ALLOWED--STACKED FOR LIBRARY CALL OUTS   %W7501695600
BOOLEAN STACKTAG;  % CAN INLINE WITHOUT STACK                       %W7501695700
DEFINE STEPI = (ELCLASS ~ TABLE(I~I+1))#;   %W27                %60 %W2701709400
PROCEDURE LISTPARA;  FORWARD;                                       %W3301715500
PROCEDURE PAIR  (B); VALUE B; BOOLEAN B; FORWARD;                   %W7901716500
    PROCEDURE SETMODULES;  FORWARD;%                                %W7601727000
    PROCEDURE USEMODULES;  FORWARD;%                                %W7601728000
                                                                        01729999
                                                                        01730499
                                                                        01730500
                                                                        01731000
 DEFINE STEPIT= ELCLASS ~ TABLE(I~I+1)#;                            %W2701741000
$ VOID 01743301                                                     %W4801741100
$ VOID 01769001 % OLD W16 PATCH                                     %W1601765100
    STREAM PROCEDURE BLANK(A);                                  %85 %W-501780000
        BEGIN DI~A; DS~8 LIT" "; SI~A; DS~14 WDS END;           %85 %W-501781000
    ARRAY SOURCE[0:23]; INTEGER LASTCOMPARE; %USED FOR SOURCE ID%77 %W2001799999
FORMAT PRINTSEGNO(X78,"START OF SEGMENT ********** ",I4),           %W-501800000
        PRINTSIZE(X78,I4," IS ",I4," LONG, NEXT SEG ",I4),          %W-501801000
                                                                        01807000
COMMENT:  NEXT CARD SUPPLIED IN LIEU OF WRL INTRINSICS;             %Y9601808000
DEFINE TIMECOMPILED="9:00:00"#, DATECOMPILED="1/11/69"#;            %Y9601809000
PROCEDURE TIMEIT(A);                                                %W-201810040
ARRAY A[0];                                                         %W-201810050
BEGIN                                                               %W-201810060
    DEFINE B=TIMEITTOG#;                                            %W-201810070
    INTEGER E,F,G,H,I;                                              %W-201810080
    STREAM PROCEDURE C(A,B,D); VALUE A; BEGIN                       %W-201810090
        SI~LOC A; SI~SI+3; DI~B; DS~2 OCT;                          %W-201810100
        DI~D; DS~3 OCT; END;                                        %W-201810110
    STREAM PROCEDURE D(A,B,C,E,F,G,H,I,J,K);                        %W-201810120
    VALUE A,B,C,E,F,G,H,I,J;                                        %W-201810130
    BEGIN                                                           %W-201810140
        LABEL L, M, N, O, P, Q;                                     %W-201810150
        DI~K; DS~8 LIT " "; SI~K; DS~14 WDS;                        %W-201810160
        DI~K; SI~LOC A; DS~2 DEC;                                   %W-201810170
        2(DS~ LIT":"; DS~ 2 DEC);                                   %W-201810180
        DI~DI+1; CI~CI+E;                                           %W-201810190
        DS~ 3 LIT"SUN"; GO L; SKIP SB;                              %W-201810200
        DS~ 3 LIT"MON"; GO L; SKIP SB;                              %W-201810210
        DS~ 4 LIT"TUES"; GO L; SKIP SB;                             %W-201810220
        DS~ 6 LIT"WEDNES"; GO L;                                    %W-201810230
        DS~ 5 LIT"THURS"; GO L;                                     %W-201810240
        DS~ 3 LIT"FRI"; GO L; SKIP SB;                              %W-201810250
        DS~ 5 LIT"SATUR";                                           %W-201810260
L:                                                                  %W-201810270
        DS~ 4 LIT"DAY,";                                            %W-201810280
        DI~ DI+1;                                                   %W-201810290
        CI~ CI+F;                                                   %W-201810300
        DS~ 3 LIT"JAN"; GO M; SKIP SB;                              %W-201810310
        DS~ 4 LIT"FEBR"; GO M; SKIP SB;                             %W-201810320
        DS~ 5 LIT"MARCH"; GO O;                                     %W-201810330
        DS~ 5 LIT"APRIL"; GO O;                                     %W-201810340
        DS~ 3 LIT"MAY"; GO O; SKIP SB;                              %W-201810350
        DS~ 4 LIT"JUNE"; GO O; SKIP SB;                             %W-201810360
        DS~ 4 LIT"JULY"; GO O; SKIP SB;                             %W-201810370
        DS~ 6 LIT"AUGUST"; GO O;                                    %W-201810380
        DS~ 6 LIT"SEPTEM"; GO N;                                    %W-201810390
        DS~ 4 LIT"OCTO"; GO N; SKIP SB;                             %W-201810400
        DS~ 5 LIT"NOVEM"; GO N;                                     %W-201810410
        DS~ 5 LIT"DECEM"; GO N;                                     %W-201810420
M:                                                                  %W-201810430
        DS~ 4 LIT"UARY"; GO O;                                      %W-201810440
N:                                                                  %W-201810450
        DS~ 3 LIT"BER";                                             %W-201810460
O:                                                                  %W-201810470
        DI~DI+1; A~DI; DI~LOC B; SI~LOC G; DS~2 DEC;                %W-201810480
        DI~A; SI~LOC B; IF SC="0" THEN SI~SI+1 ELSE DS~CHR;         %W-201810490
        DS~CHR; DS~4 LIT ", 19"; SI~LOC H;DS~2 DEC;                 %W-201810500
        IF SC!"+" THEN BEGIN                                        %W-201810510
            DI~DI+4; DS~16 LIT "PROCESSOR TIME ="; DI~DI+1;         %W-201810520
            A~CI; GO P; DI~DI+4; DS~10 LIT "I/O TIME =";            %W-201810530
            SI~LOC J; A~CI; GO P; GO Q;                             %W-201810540
P:          B~DI; DI~LOC C; DS~8 DEC; SI~LOC C; DI~B;               %W-201810550
            5(IF TOGGLE THEN IF SC="0" THEN SI~ SI+1 ELSE DS~ CHR   %W-201810560
                  ELSE DS~ CHR);                                    %W-201810570
            DS~CHR; DS~LIT "."; DS~2 CHR; DS~8 LIT " SECONDS";      %W-201810580
            CI~A;                                                   %W-201810590
Q:      END ELSE BEGIN DI~DI+10;                                    %W-201810600
            DS~21 LIT "B-5500 ALGOL COMPILER";                  %86 %W-501810605
            DI~K; DI~DI+63; DI~DI+26; DS~6 LIT "< WRL ";        %86 %W-501810610
DS~5 LIT TIMECOMPILED;DS~LIT" ";DS~8 LIT DATECOMPILED;DS~LIT">";%86 %W-501810615
            END                                                     %W-201810620
    END;                                                            %W-201810630
    C(TIME(0),E,F);                                                 %W-201810640
    G~ ((((E-1)DIV 4)+E+F)MOD 7)|5;                                 %W-201810650
    IF F<60 THEN F~F-1 ELSE IF E MOD 4=0 THEN F~F-1;                %W-201810660
    IF F> 212 THEN                                                  %W-201810670
        F~(F+31)DIV 61+F                                            %W-201810680
    ELSE                                                            %W-201810690
        IF F>59 THEN                                                %W-201810700
            F~ (F+62)DIV 61+F;                                      %W-201810710
    H~ TIME(1)|.016667;                                             %W-201810720
    D(H DIV 3600, I~H DIV 60 MOD 60, I~ H MOD 60,                   %W-201810730
      G,(F DIV 31)|5, H~ F MOD 31+1, E,                             %W-201810740
  IF B THEN H~ TIME(2)|1.6667 ELSE-1, IF B THEN H~ TIME(3)|1.6667   %W-201810750
      ELSE-1,A[0]);                                                 %W-201810760
    IF NOT B THEN BEGIN WRITE(LINE,15,A[*]);BLANK(A);B~TRUE END;%85 %W-501810770
END TIMEIT;                                                         %W-201810780
 DEFINE CHECKIT=IF NOT TIMEITTOG THEN BEGIN                         %W-201810790
    TIMEIT(LIN) END#;  % DELAY PRINTING                             %W7901810800
      PROCEDURE OCTCON(F,R); VALUE F; BOOLEAN F; REAL R; BEGIN      %W-301810810
 COMMENT OCTCON SETS FLAG BIT ONLY IF CALLED FROM FILLSTMT(I.E. IF  %W-301810820
    F IS FALSE);                                                    %W-301810830
    BOOLEAN STREAM PROCEDURE OC(AC,C1,C2,B,F,R); VALUE C1,C2,B,F;   %W-301810840
    BEGIN                                                           %W-301810850
        SI ~ AC; SI ~ SI+C1; DI ~ R; SKIP B DB;                     %W-301810860
        C2(IF SC>"7"THEN TALLY~1; IF SC<"0"THEN TALLY~1;SKIP 3SB;   %W-301810870
            3(IF SB THEN DS ~ SET ELSE DS ~ RESET; SKIP SB));       %W-301810880
        DI ~ R; DS ~ F RESET; OC ~ TALLY                            %W-301810890
    END OC;                                                         %W-301810900
    INTEGER C1,C2;                                                  %W-301810910
      IF COUNT>16 THEN BEGIN C1 ~ COUNT-13; C2 ~ 16 END             %W-301810920
                  ELSE BEGIN C1 ~ 3; C2 ~ COUNT END;                %W-301810930
      R ~ 0;                                                        %W-301810940
    IF OC(ACCUM[1],C1,C2,48-3|C2,F,R) THEN                          %W9301810950
        BEGIN FLAG(-303); WRL(6) END;                               %W9301810955
 END OCTCON;                                                        %W-301810960
 $ VOID 01840001                                                %76 %W-201810970
    REAL OMITCR; BOOLEAN STOPDOLLAR;                                %W7601840500
PROCEDURE INLINE; FORWARD;                                          %W7501840600
PROCEDURE WRITEPRT(WHAT, HOWMUCH,T);                                %W4301900000
VALUE WHAT, HOWMUCH, T;                                             %W4301900100
REAL    WHAT, HOWMUCH, T;                                           %W4301900200
BEGIN                                                               %W4301900300
LABEL EXIT;                                                 %63 %66 %W4301900310
    IF ERRORCOUNT ! 0 THEN GO EXIT;                         %63 %66 %W4301900320
    WHAT~ WHAT.LINK;                                                %W4301900350
    XREFC~ XREFC+1;                                                 %W4301900360
    PRTABLE[XENDI]~ T&(HOWMUCH+2)[18:33:15]&LASTHM[3:33:15];        %W4301900400
    LASTHM~ HOWMUCH+2;                                              %W4301900450
    OLDTT~0&LASTHM[3:33:15];                                        %W4301900460
    IF XENDI~ XENDI+1=30 THEN                                       %W4301900600
        BEGIN                                                       %W4301900700
                                                                %87 %W6101900799
            WRITE(DATA   , 30, PRTABLE[*]);                         %W4301900800
            XENDI~ 0                                                %W4301900900
        END;                                                        %W4301901000
    MOVE(1, INFO[LASTSEQROW, LASTSEQUENCE], PRTABLE[XENDI]);        %W4301901100
    IF XENDI~ XENDI+1=30 THEN                                       %W4301901200
        BEGIN                                                       %W4301901300
                                                                %87 %W6101901399
            WRITE(DATA, 30, PRTABLE[*]);                            %W4301901400
            XENDI~ 0                                                %W4301901500
        END;                                                        %W4301901600
    IF HOWMUCH>0 AND T.[33:15]!7 AND T.[33:15]!9 THEN               %W4301901605
    BEGIN                                                           %W4301901610
        PRTABLE[XENDI]~ TAKE(WHAT)&WHAT[35:35:13];                  %W4301901615
            WHAT~ WHAT+1;                                           %W4301901625
            HOWMUCH~ HOWMUCH-1;                                     %W4301901630
            IF XENDI~ XENDI+1=30 THEN                               %W4301901635
                BEGIN                                               %W4301901640
                                                                %87 %W6101901644
                    WRITE(DATA, 30, PRTABLE[*]);                    %W4301901645
                    XENDI~ 0                                        %W4301901650
                END                                                 %W4301901655
        END;                                                        %W4301901660
    WHILE HOWMUCH>0 DO                                              %W4301901700
        BEGIN                                                       %W4301901800
            IF T~ 30-XENDI>HOWMUCH THEN                             %W4301901900
                T~ HOWMUCH;                                         %W4301902000
            MOVE(T, INFO[WHAT.LINKR, WHAT.LINKC], PRTABLE[XENDI]);  %W4301902100
            IF XENDI~ XENDI+T=30 THEN                               %W4301902200
                BEGIN                                               %W4301902300
                                                                %87 %W6101902399
                    WRITE(DATA, 30, PRTABLE[*]);                    %W4301902400
                    XENDI~ 0                                        %W4301902500
                END;                                                %W4301902600
            WHAT~ WHAT+T;                                           %W4301902700
            HOWMUCH~ HOWMUCH-T                                      %W4301902800
    END; EXIT:                                              %63 %66 %W4301902900
END;                                                                %W4301903000
PROCEDURE BACKUPTO(W, I);                                           %W4301903100
VALUE W, I;                                                         %W4301903200
INTEGER W, I;                                                       %W4301903300
BEGIN                                                               %W4301903400
LABEL EXIT;                                                 %63 %66 %W4301903410
    IF ERRORCOUNT ! 0 THEN GO EXIT;                         %63 %66 %W4301903420
    WHILE W{XREFC DO                                                %W4301903500
        BEGIN                                                       %W4301903600
            IF XENDI~ XENDI-OLDTT.[3:15]<0 THEN                     %W4301903700
                BEGIN                                               %W4301903800
                    MOVE(30, PRTABLE[0], TPRT[0]);                  %W4301903850
                                                                %87 %W6101903899
                    READ REVERSE(DATA, 30, PRTABLE[*]);             %W4301903900
                    XENDI~ XENDI+30                                 %W4301904000
                END;                                                %W4301904100
            XREFC~ XREFC-1;                                         %W4301904200
            OLDTT~ PRTABLE[XENDI];                                  %W4301904300
        END;                                                        %W4301904400
    IF I!0 THEN                                                     %W4301904500
            PRTABLE[XENDI].[33:15]~ I;                              %W4301904700
    XREFC~ XREFC+1;                                                 %W4301905000
    IF XENDI~ XENDI+OLDTT.[18:15]>29 THEN                           %W4301905100
        BEGIN                                                       %W4301905200
                                                                %87 %W6101905299
            WRITE(DATA, 30, PRTABLE[*]);                            %W4301905300
            MOVE(30, TPRT[0], PRTABLE[0]);                          %W4301905350
            XENDI~ XENDI-30                                         %W4301905400
        END;                                                        %W4301905500
    LASTHM~ OLDTT.[18:15];                                          %W4301905540
    OLDTT~ 0&OLDTT[3:18:15]                                         %W4301905550
    ;EXIT:  END;                                            %63 %66 %W4301905600
PROCEDURE SEARCHLIB(DOLLAR); VALUE DOLLAR; BOOLEAN DOLLAR;      %A  %W6202013161
BEGIN LABEL EXIT, EXITOUT, NOPARTIAL; BOOLEAN TEMPLISTOG;           %W6202013162
STREAM PROCEDURE MAVE(D,S); VALUE S; BEGIN LABEL W,X,Y,Z;       %A  %W6202013163
DI~D;DS~8 LIT" ";SI~D;DS~8 WDS;DI~D;SI~S;                       %A  %W6202013164
W:  IF SC=" " THEN BEGIN SI~SI+1;GO W END; SI~SI+2;   %$$       %A  %W6202013165
X:  IF SC=" " THEN BEGIN SI~SI+1;GO X END; DS~CHR;    %A,B,OR C %A  %W6202013166
Y:  IF SC=" " THEN BEGIN SI~SI+1;GO Y END;                      %A  %W6202013167
Z:  IF SC=ALPHA THEN BEGIN DS~CHR;GO Z END;END; DEFINE EOT=600#;%A  %W6202013168
    IF DOLLAR THEN                                                  %W7602013169
      BEGIN COMMENT WE ARE ON A DOUBLE DOLLAR CARD;                 %W7602013170
                                                                        02013171
                                                                        02013172
                                                                        02013173
                                                                        02013174
                                                                        02013175
                                                                        02013176
                                                                        02013177
                                                                        02013178
                                                                        02013179
                                                                        02013180
                                                                        02013181
                                                                        02013182
                                                                        02013183
                                                                        02013184
                                                                        02013185
                                                                        02013186
                                                                        02013187
                                                                        02013188
                                                                        02013189
                                                                        02013190
                                                                        02013191
CARDCALL ~ LASTUSED < 3 OR LASTUSED = 5;                            %W7602013192
IF PATCHTOG THEN BEGIN TEMPLISTOG~LISTOG; LISTOG~FALSE END;         %W6202013220
Q~ACCUM[1]; FLAG(500); IF PATCHTOG THEN LISTOG~TEMPLISTOG; GO EXIT; %W6202013221
COMMENT WE ALWAYS LIST $$ CARDS;                                %A  %W6202013222
IF PATCHTOG THEN BEGIN TEMPLISTOG~LISTOG; LISTOG~FALSE END;         %W6202013303
Q~ACCUM[1]; FLAG(501); IF PATCHTOG THEN LISTOG~TEMPLISTOG; GO EXIT; %W6202013304
COMMENT WE ALWAYS LIST $$ CARDS;                                %A  %W6202013305
IF PATCHTOG THEN BEGIN TEMPLISTOG~LISTOG; LISTOG~FALSE END;         %W6202013348
Q~ACCUM[1]; FLAG(502); IF PATCHTOG THEN LISTOG~TEMPLISTOG; GO EXIT; %W6202013349
COMMENT WE ALWAYS LIST $$ CARDS;                                %A  %W6202013350
IF PATCHTOG THEN BEGIN TEMPLISTOG~LISTOG; LISTOG~FALSE END;         %W6202013378
Q~ACCUM[1]; FLAG(503); IF PATCHTOG THEN LISTOG~TEMPLISTOG; GO EXIT; %W6202013379
COMMENT WE ALWAYS LIST $$ CARDS;                                %A  %W6202013380
IF PATCHTOG THEN BEGIN TEMPLISTOG~LISTOG; LISTOG~FALSE END;         %W6202013403
Q~ACCUM[1]; FLAG(504); IF PATCHTOG THEN LISTOG~TEMPLISTOG; GO EXIT; %W6202013404
COMMENT WE ALWAYS LIST $$ CARDS;                                %A  %W6202013405
IF PATCHTOG THEN BEGIN TEMPLISTOG~LISTOG; LISTOG~FALSE END;         %W6202013433
Q~ACCUM[1]; FLAG(505); IF PATCHTOG THEN LISTOG~TEMPLISTOG; GO EXIT; %W6202013434
COMMENT WE ALWAYS LIST $$ CARDS;                                %A  %W6202013435
        NOPARTIAL:  COMMENT NOW SET UP THE LINKS;                   %W7502013455
            STACKTAG.[1:46] ~ STACKTAG;                             %W7502013474
            INLINETAG.[1:46] ~ INLINETAG;                           %W7502013475
         LISTOG.[1:46] ~ LISTOG;                                    %W-802013476
         PRTOG.[1:46] ~ PRTOG;                                      %W-802013477
         DEBUGTOG.[1:46] ~ DEBUGTOG;                                %W-802013478
        STREAMTAG.[1:46] ~ STREAMTAG;                               %W3202013479
    LIBARRAY[LIBINDEX+2].LSTUSD~ LASTCOMPARE;                       %W2002013485
        IF LIBINDEX>0 THEN IF CARDCALL AND PATCHTOG THEN BEGIN      %W5102013536
                LASTUSED~7; FIRSTIMEX~TRUE END                      %W5102013540
        ELSE IF CARDCALL AND PATCHTOG THEN LASTUSED~5 ELSE          %W5102013541
                             BEGIN LASTUSED~7;FIRSTIMEX~TRUE END;   %W5102013542
                LIBINDEX ~ LIBINDEX + 3;                            %W5102013543
    BEGIN COMMENT WE DID NOT COME FROM DOUBLE DOLLAR SO UNLINK;     %W7502013551
            STACKTAG ~ STACKTAG & STACKTAG[2:1:46];                 %W7502013554
            INLINETAG ~ INLINETAG & INLINETAG [2:1:46];             %W7502013555
         LISTOG   ~ LISTOG  &LISTOG  [2:1:46];                      %W-802013556
         PRTOG    ~ PRTOG   &PRTOG   [2:1:46];                      %W-802013557
         DEBUGTOG ~ DEBUGTOG&DEBUGTOG[2:1:46];                      %W-802013558
        STREAMTAG ~ STREAMTAG&STREAMTAG[2:1:46];                    %W3202013559
    LASTCOMPARE~ LIBARRAY[LIBINDEX+2].LSTUSD;                       %W2002013590
         IF LASTUSED{2 OR LASTUSED=5 THEN NCR~                      %W5702013617
                (CLCR - 9)                                      %84 %W5702013618
            ELSE                                                %84 %W5702013619
        IF LASTUSED{ 2 OR LASTUSED=5 THEN LCR ~ NCR+9 ELSE          %W5702013621
            NORELEASE ~ PATCHTOG AND (LASTUSED<3);                  %W5102013627
        IF LASTUSED=6 OR LASTUSED=7 THEN FIRSTIMEX~TRUE;            %W5102013628
FOR FILEINX~0,1,2 DO BEGIN CLOSE(LIBRARY[FILEINX]);% RETURN BUFFERS %W8902013650
DIRECTORY[FILEINX|3,0]~0;COMMENT CAN USE RESIZE                     %W8902013651
(DIRECTORY[FILEINX|3,*],56); END END  % RESET FOR MULTIPLE CALLS    %W8902013652
$ VOID 02020401     % KILL PRINTNOTE                          % %79 %W7402020000
STREAM PROCEDURE PRINT(LINE,NCR,R, SYMBOL, C, N);                   %W4902024000
                    VALUE NCR,R,SYMBOL,N;                           %W4902024100
BEGIN LOCAL CH2,CH; DI~LOC CH; DS~10 LIT "00 CT D M ";              %W7702025000
                DI ~ LINE; DS ~ 8 LIT " ";                          %W-502026000
                SI~NCR; DS~ 10 WDS;                                 %W4902027000
$ VOID 02029001                                                     %W4902028000
        SI~LOC SYMBOL;SI~SI+6;DS~LIT" ";DS~2 CHR;DS~LIT" ";         %W-502030000
    SI~ LOC R; NCR~DI; DI~LOC SYMBOL;  DS~8 DEC; DI~DI-8;           %W-502030100
    DS~2 FILL; DI~ LOC SYMBOL; DI~ DI+3;                            %W-502030150
    ;DS~3 FILL;DI~NCR;SI~LOC SYMBOL;DS~3 CHR;DS~LIT":";DS~4 CHR;    %W-502030200
        DS~LIT":";DS~CHR;                                           %W-502030300
        DS~2 LIT" ";                                                %W-502030400
    NCR~DI;SI~LINE;DS~WDS;SI~SI-8;DS~WDS;                           %W4902030500
    N(DI~NCR;DI~DI-1;SI~C;DS~5 DEC;SI~LOC CH;SI~SI+N;SI~SI+N;       %W4902030600
      DS~2CHR;DI~DI-7;DS~4 FILL;JUMP OUT);                          %W4902030700
    END PRINT;                                                      %W4902031000
INTEGER CNCR;                                                       %W5602041000
INTEGER PROCEDURE CCSCAN(RESULT); ALPHA RESULT;                     %W5602041010
BEGIN  INTEGER J,K;                                                 %W5602041020
    LABEL NOTINT,RESC,DONE;                                         %W5602041025
    INTEGER STREAM PROCEDURE CCS(NCRV,RS); VALUE NCRV;              %W5602041030
    BEGIN LABEL L1,L2,L3,SP;                                        %W5602041040
        LABEL L4;                                                   %W5602041045
        SI ~ NCRV; DI ~ RS;                                         %W5602041050
    L1: IF SC=" " THEN BEGIN SI~SI+1; GO L1 END;                    %W5602041060
        IF SC > "9" THEN GO SP;                                     %W5602041070
        IF SC } "0" THEN                                            %W5602041080
        BEGIN                                                       %W5602041090
    L4:     IF SC = "0" THEN BEGIN SI ~ SI+1; GO L4 END;            %W5602041100
            IF SC < "0" THEN SI~SI-1;                               %W5602041110
            IF SC > "9" THEN SI~SI-1;                               %W5602041120
            CCS ~ SI;                                               %W5602041125
            63(TALLY~TALLY+1; SI~SI+1; IF SC<"0" THEN JUMP OUT;     %W5602041130
                IF SC>"9"THEN JUMP OUT);                            %W5602041140
            NCRV ~ TALLY; SI ~ LOC RS; SI~SI-1;                     %W5602041180
            DS ~ 7 LIT"3000000"; DS ~ CHR;                          %W5602041190
        END ELSE                                                    %W5602041200
        IF SC=ALPHA THEN                                            %W5602041210
        BEGIN DS ~ LIT "1";                                         %W5602041220
            7(IF SC=ALPHA THEN DS~CHR ELSE DS~LIT" ");              %W5602041230
    L2:     IF SC=ALPHA THEN BEGIN SI~SI+1; GO TO L2 END;           %W5602041240
            CCS ~ SI;                                               %W5602041245
        END ELSE                                                    %W5602041250
        IF SC = """ THEN                                            %W5602041260
        BEGIN SI ~ SI+1; DS~LIT"1"; DS ~ CHR;                       %W5602041270
            6(IF SC!""" THEN DS ~ CHR ELSE DS ~ LIT" ");            %W5602041280
    L3:     IF SC!""" THEN BEGIN SI ~ SI+1; GO L3 END;              %W5602041290
            SI ~ SI+1; CCS ~ SI;                                    %W5602041300
        END ELSE                                                    %W5602041310
    SP: BEGIN                                                       %W5602041320
            DS ~ 7 LIT"2000000";                                    %W5602041330
            DS ~ CHR; CCS ~ SI;                                     %W5602041340
        END;                                                        %W5602041350
    END CCS;                                                        %W5602041370
    INTEGER STREAM PROCEDURE  INTS(NC,N,R); VALUE NC,N;             %W5602041380
    BEGIN SI~NC; DI~R; DS~N OCT; INTS~SI;                           %W5602041390
    END INTS;                                                       %W5602041400
DEFINE NCR=CNCR#;                                                   %W5602041405
    NCR ~ CCS(NCR,RESULT);                                          %W5602041410
    IF (CCSCAN~RESULT.[1:5])!3 THEN GO NOTINT;                      %W5602041420
    K ~ (K~((J~RESULT.[42:6])-1)MOD 8)+1;                           %W5602041430
    NCR ~ INTS(NCR,K,RESULT);                                       %W5602041440
    IF (J~J-K) { 0 THEN GO TO DONE;                                 %W5602041450
RESC: NCR ~ INTS(NCR,8,K);                                          %W5602041460
    RESULT ~ RESULT|100000000 + K;                                  %W5602041470
    IF (J~J-8) > 0 THEN GO TO RESC;                                 %W5602041480
    IF RESULT.[2:7] ! 0 THEN CCSCAN ~ 4;                            %W5602041490
    GO TO DONE;                                                     %W5602041500
NOTINT: RESULT.[1:5] ~ 0;                                           %W5602041510
DONE:                                                               %W5602041520
END CCSCAN;                                                         %W5602041530
SWITCH FILE SWF ~ CARD,CARD,TAPE,DISK,MERGE;                        %W7702044000
PROCEDURE NEXTREC(LCR,MAXLCR,FN); VALUE FN; INTEGER LCR,MAXLCR,FN;  %W5702045000
BEGIN COMMENT READS NEXT RECORD,HANDLES EOF CONDITIONS AND          %W5702045010
        LINK RECORDS.                                               %W5702045020
        FN = 0 (CAST), 1 (CARD), 2 (TAPE), 3 (DISK);                %W5702045030
    %   FN = 4 (MERGE),  5 (NEWDISK)                                %W7702045035
    REAL RS,T;                                                      %W5702045040
    LABEL EOF,AGAIN,EOF1,ERR,OK,NXT,FIX,NOTQM;                      %W5702045050
DEFINE LOGLVL=10#;LOGIN;                                            %W8202045051
    IF FN=0 THEN                                                    %W5702045070
    BEGIN RECOUNT ~ RECOUNT+1;                                      %W5702045080
        IF LCR ~ LCR+11 > MAXLCR THEN                               %W5702045090
        BEGIN READ(LIBRARY[FILEINX]);                               %W5702045100
            MAXLCR ~ (LCR ~ MKABS(LIBRARY[FILEINX](10))) + 46;      %W5702045110
        END;                                                        %W5702045120
        ADDER(SEQSUM,LCR,TRUE,TRUE);                                %W5702045130
    END ELSE                                                        %W5702045140
    IF FN=1 AND CARDEOF THEN                                        %W5702045150
    BEGIN                                                           %W5702045160
EOF:    IF FN = 1 THEN                                              %W5702045170
$ VOID  02045240                                                %84 %W5702045180
            CARDEOF ~ TRUE;                                         %W5702045240
            IF NOT PURGETOG THEN % GIVE BACK BUFFERS NOW        %84 %W5702045250
                IF FN=2 THEN LOCK(TAPE) ELSE CLOSE(SWF[FN]);    %84 %W5702045260
$ VOID  02045270                                                %84 %W5702045261
            % MAKE DISTINCT HI-VALUE IMAGES                     %84 %W5702045270
    IMAGES[FN,9]~("?????0"+FN)&"??"[1:37:11];                       %W5702045280
                                                                %84 %W5702045290
    END ELSE                                                        %W5702045300
    BEGIN IF FN<0 THEN                                              %W5702045303
        % SET UP POINTERS                                       %84 %W5702045306
        MAXLCR ~ LCR ~ MKABS( IMAGES[ FN~ABS(FN), 9 ] )         %84 %W5702045307
        ELSE                                                        %W5702045310
NXT:    BEGIN RECORDCNT[FN] ~ RECORDCNT[FN] + 1;                    %W5702045315
                                                                %77 %W5702045320
        END;                                                        %W5702045325
AGAIN:  READ(SWF[FN], 10, IMAGES[FN, *])[EOF];                  %77 %W5702045330
                                                                %84 %W5702045335
        IF EXAMIN(CNCR~LCR-9)!12 THEN GO TO OK;                     %W5702045340
        FNAME[FN] ~ -190;                                           %W5702045345
        IF NOT DISKTYPE[FN] THEN GO OK;                             %W5702045350
        ZOT(RECORDCNT[0],"%",LCR);                                  %W5702045355
        RS ~ CCSCAN(RS);                                            %W5702045360
        IF CCSCAN(RS)!1 THEN GO NOTQM;                              %W5702045370
        IF RS = "END    " THEN BEGIN WRL(-2); GO TO EOF END;    %93 %W5702045380
        IF RS ! "LINK   " THEN BEGIN                                %W5702045390
NOTQM:                              FNAME[FN]~-193; GO FIX END;     %W5702045395
        WRL(21);                                                    %W9302045397
        IF (T~CCSCAN(RS))=1 THEN GO ERR;                            %W5702045400
        IF T = 2 THEN                                               %W5702045410
        BEGIN                                                       %W5702045420
            IF RS = "+" THEN T ~ 1                                  %W5702045430
            ELSE IF RS = "-" THEN T ~ -1                            %W5702045440
            ELSE GO ERR;                                            %W5702045450
            IF CCSCAN(RS) ! 3 THEN GO ERR;                          %W5702045460
            RS ~ RECORDCNT[FN] + T|RS;                              %W5702045470
        END;                                                        %W5702045480
        IF RS<0 THEN GO EOF1;                                       %W5702045490
        IF RECORDCNT[FN]+1 = RS THEN GO NXT;                        %W5702045495
        IF RECORDCNT[FN]=RS THEN GO TO EOF;                         %W5702045500
        READ SEEK(SWF[FN][RS])[EOF1];                               %W5702045510
        RECORDCNT[FN] ~ RS;                                         %W5702045520
        LINKCT[FN] ~ LINKCT[FN]+1;                                  %W5702045530
        GO AGAIN;                                                   %W5702045540
EOF1:   FNAME[FN] ~ 191; GO TO FIX;                                 %W5702045550
ERR:    FNAME[FN] ~ 194;                                            %W5702045560
FIX:    MOVE(1,RECORDCNT[0],IMAGES[FN,9]); %RESTORE SEQUENCE NO %77 %W5702045570
OK: END;                                                            %W5702045600
LOGOUT                                                              %W8202045609
END NEXTREC;                                                        %W5702045610
PROCEDURE MERGECOMP; % SELECT NEXT RECORD TO RETURN AS "TAPE"       %W7702046000
    IF GT1 ~ COMPARE( SLCR, MLCR) = 0 THEN                          %W7702047000
        BEGIN                                                       %W7702048000
            TLCR ~ SLCR;                                            %W7702049000
            MERGELAST ~ FALSE                                       %W7702050000
        END                                                         %W7702051000
      ELSE                                                          %W7702052000
        BEGIN                                                       %W7702053000
            TLCR ~ MLCR;                                            %W7702054000
            MERGELAST ~ TRUE;                                       %W7702055000
            IF GT1 ! 1 THEN                                         %W7702056000
                NEXTREC(SLCR,MAXTLCR,REAL(DISKTOG)+2)               %W7702057000
        END MERGECOMP;                                              %W7702058000
                                                                        02067997
                                                                        02067998
                                                                        02067999
    LABEL DEBLANK,NUMBERS,IDBLDR,GNC,K,EXIT,FINIS,L, ERROR;         %W6502116000
                                                                    %W6502117000
    GO TO FINIS;                                                    %W6502128000
    COMMENT  6 OR 7 IS AN ERROR;                                    %W6502129000
$ VOID 02133001                                                     %W6502130000
$ VOID 02151001                                                     %W6502144000
COMMENT BLANKOUT DOES SAME AS CALL ON SCANNER WITH RESULT=7;        %W6502178005
INTEGER STREAM PROCEDURE BLANKOUT(NCRV); VALUE NCRV;                %W6502178010
BEGIN LABEL L;  SI ~ NCRV;                                          %W6502178015
    L: IF SC = " " THEN BEGIN SI~SI+1; GO TO L END;                 %W6502178020
    BLANKOUT ~ SI                                                   %W6502178025
END BLANKOUT;                                                       %W6502178030
PROCEDURE ANOTHERCARD;                                              %W6502178035
    BEGIN   READACARD;                                              %W6502178040
    IF LIBINDEX ! 0 THEN IF RECOUNT = FINISHPT THEN                 %W6502178045
        BEGIN SEARCHLIB(FALSE); READACARD; NORELEASE~FALSE END      %W6502178050
    END ANOTHERCARD;                                                %W6502178055
DEFINE LOGLVL=8#;LOGIN;                                             %W8202178105
    IF  RESULT = 7 THEN                                             %W6502178110
    WHILE EXAMIN(NCR ~ BLANKOUT(NCR)) = "%" DO ANOTHERCARD          %W6502178150
        ELSE                                                        %W6502178190
    IF NCR=LCR THEN BEGIN ANOTHERCARD; GO TO L END;                 %W6502178400
$ VOID 02180301                                                     %W6502178410
LOGOUT END SCANNER;                                                 %W8202180400
COMMENT SEQCHANGE WILL CONV A MACHINE NO. TO PROPER OUTPUT FORM;    %W4802187010
    STREAM PROCEDURE CHANGESEQ(VAL,OLDSEQ); VALUE OLDSEQ;           %W4802187020
        BEGIN DI~OLDSEQ; SI~VAL; DS~ 8 DEC END  CHANGESEQ;          %W4802187030
COMMENT K ADDED TO FLAG SEQ ERROR IN LEFT MARGIN OF LISTING;    %69 %W-502187040
STREAM PROCEDURE SEQERR(K,L);                                   %69 %W-502187041
BEGIN DI~K;DS~ 3 LIT  "SEQ"; DI~L; DI~DI+5; DS~ 3 LIT "SEQ" END;%69 %W-502187042
COMMENT NEWNUM PUTS REC# AND NT OR ND IN RIGHT MARGIN;          %70 %W-502187045
STREAM PROCEDURE NEWNUM(L,R,V); VALUE V;                        %70 %W-502187046
BEGIN DI~L; DI ~ DI - 1; L ~ DI;                                %70 %W-502187047
    SI~R; DS ~ 5 DEC; SI ~ LOC V; SI~ SI+5; DS ~ 3 CHR;         %70 %W-502187048
    DI ~L; DS ~ 4 FILL END;                                     %70 %W-502187049
    BOOLEAN STREAM PROCEDURE NONBLANK(FCR); VALUE FCR;              %W4802187050
    COMMENT SCANS CARD FOR ALL BLANKS-- TRUE IF ANY OTHER CHAR;     %W4802187060
        BEGIN LABEL NED; SI~FCR; TALLY~0;                           %W4802187070
            2(36(IF SC!" " THEN JUMP OUT 2 TO NED; SI~SI+1));       %W4802187080
            TALLY ~ 63; NED: TALLY ~ TALLY+1; NONBLANK ~ TALLY      %W4802187090
        END NONBLANK;                                               %W4802187100
COMMENT SEQ 02237500/02238400 MOVED SO THAT $$ CARD WOULD       %76 %W5102187500
    BE INCD AND LISTED FOR NEWDISK;                             %76 %W5102187505
    INTEGER STREAM PROCEDURE DOLLARCHECK(A); VALUE A;           %76 %W5102187510
        COMMENT 0 IF NOT DOLLAR COL 1 OR DOLLAR QUOTE           %76 %W5102187515
                1 IF DOLLAR DOLLAR                              %76 %W5102187520
                2 IF DOLLAR NOT DOLLAR;                         %76 %W5102187525
        BEGIN SI~A;                                             %76 %W5102187530
            IF SC="$" THEN                                      %76 %W5102187535
            BEGIN SI~SI+1;                                      %76 %W5102187540
                IF SC="$" THEN TALLY~1 ELSE TALLY~2             %76 %W5102187545
            END;  DOLLARCHECK~TALLY                             %76 %W5102187550
        END DOLLARCHECK;                                        %76 %W5102187555
    INTEGER ICNT;                                                   %W4902187900
 $ VOID 02196061                                                    %W5702188000
STREAM PROCEDURE SAY(S,D); VALUE S;                                 %W-102196070
        BEGIN SI~S; DI~D; DS~ 8 OCT END;                            %W-102196080
        IF GT1~LASTCOMPARE~COMPARE(TLCR,CLCR)=0   OR STOPTOG        %W2002196200
IF (IF STOPDOLLAR THEN TRUE                                         %W7602196270
    ELSE DOLLARCHECK(CLCR-9) ! 2 )                                  %W7602196275
                                  NEXTREC(LTLCR,MAXLTLCR,0) END     %W5702196305
ELSE BEGIN                                                          %W7702196310
    IF MERGELAST THEN                                               %W7702196313
        NEXTREC(MLCR,0,4)                                           %W7702196316
      ELSE                                                          %W7702196320
        NEXTREC(SLCR,MAXTLCR,REAL(DISKTOG)+2);                  %77 %W5702196323
    IF MERGETOG THEN MERGECOMP ELSE TLCR~SLCR;                      %W7702196326
END;END END;                                                        %W7702196329
            IF LASTUSED!1 THEN LASTUSED~ IF LIB THEN 5 ELSE 2;      %W4902196340
LABEL PASSED; % FOR $$ CARD-NEWDISK                             %71 %W5102196600
LABEL LISTIT; %LIST ALL DOLLAR CARDS                                %W7802196650
BOOLEAN STREAM PROCEDURE BLANKSEQ(L); VALUE L; BEGIN LABEL QQ; SI~L;%W4302198100
    8(IF SC!" " THEN JUMP OUT 1 TO QQ; SI~SI+1); TALLY~1;           %W4302198150
    QQ: BLANKSEQ ~ TALLY END;                                       %W4302198200
STREAM PROCEDURE PUTSEQ(L,S);VALUE L,S;BEGIN DI~L;DS~LIT"#";SI~LOC S%W4302198300
 ;DS~7DEC END;                                                      %W4302198400
STREAM PROCEDURE OMITFLAG(LIN);                                     %W7602198500
BEGIN DI ~ LIN; DS ~ 4 LIT "OMIT" END;                              %W7602198501
BOOLEAN PASSON;                                                     %W7602198550
    LABEL LIBONLY;                                                  %W5102198900
            LIBCLAST, LIBTLAST, LIBONLY;                            %W5102199010
DEFINE LOGLVL=9#;LOGIN;                                             %W8202199015
CARDONLY:     IF NORELEASE THEN GO TO EXIT;                         %W5702206000
              NEXTREC(LCR,CLCR,1); GO TO EXIT;                      %W5702206100
FIRSTIME:     NEXTREC(LCR,CLCR,-1);                                 %W5702207000
CARDLAST:     IF NORELEASE THEN GO TO COMPAR;                       %W5702210000
              NEXTREC(CLCR,0,1);                                    %W5702211000
TAPELAST:                                                           %W7702214100
    IF MERGELAST THEN                                               %W7702214200
        NEXTREC(MLCR,0,4)                                           %W7702214300
      ELSE                                                          %W7702214400
        NEXTREC(SLCR,MAXTLCR,REAL(DISKTOG)+2);                      %W7702214500
    IF MERGETOG THEN MERGECOMP ELSE TLCR~SLCR;                      %W7702215000
              IF LASTUSED=1 THEN GO EXIT;                           %W5702215200
              GO COMPAR;                                            %W5702215500
              NEXTREC(CLCR,0,1);                                    %W5702217200
%                                                                   %W5702218000
         NEXTREC(LTLCR,MAXLTLCR,0);                                 %W5702221000
                GO TO COMPAR;                                       %W5102221150
 LIBONLY:IF FIRSTIMEX THEN FIRSTIMEX ~ FALSE                        %W5702221200
                      ELSE NEXTREC(LTLCR,MAXLTLCR,0);               %W5702221250
         LCR ~ LTLCR; IF RECOUNT=FINISHPT THEN GO XIT;              %W5702221300
          GO EXIT;                                                  %W5102221400
    IF CARDEOF THEN IF LCR=CLCR THEN FLAG(528);                 %84 %W5702229200
PASSON ~ FALSE; % BE SURE ITS RESET %PRINT BUT NOT NEWDSK           %W7602229500
IF OMITCR ! 0 THEN                                                  %W7602230055
    IF COMPARE(LCR,OMITCR) ! 0 THEN                                 %W7602230060
        OMITCR ~ VOIDPLACE ~ 0;                                     %W7602230070
              IF (IF SEQTOG THEN NONBLANK(FCR) ELSE TRUE) THEN BEGIN%W5102230100
COMMENT SEQ # ON $$ CARD FOR SEQ ON NEWDISK                     %71 %W5102231000
    LIST $$ CARD ONLY IF WOULD APPEAR IN NEW DISK IF NEWDISK    %71 %W5102231002
    (IF PATCHTOG THEN NOT LISTED---B STANDARD);                 %71 %W5102231004
IF OMITCR ! 0 OR STOPDOLLAR THEN  GO PASSED;                        %W7602231010
IF GTI1 ~ DOLLARCHECK(FCR)! 0 THEN BEGIN                        %71 %W5102231012
  IF GTI1=1 THEN IF LASTUSED ! 7 THEN                           %71 %W5102231014
    IF NOT PATCHTOG OR LASTUSED =3                              %71 %W5102231016
    OR LASTUSED = 6 THEN GO PASSED;                             %71 %W5102231018
IF NOT STOPDOLLAR THEN BEGIN CNCR~FCR&1[32:47:1];IF CCSCAN(GT1)=1   %W7602231019
THEN BEGIN PASSON~TRUE;IF GT1!"VOID   " THEN IF GT1!"OMIT   " THEN  %W7602231020
IF GT1!"INCLUDE" THEN IF GT1!"SET    " THEN PASSON~FALSE END;       %W7602231021
IF PASSON THEN GO PASSED END ;                                      %W7602231022
IF DOLTOG THEN IF LASTUSED!7 THEN GO LISTIT;%LIST $ CARDS           %W7802231024
END ELSE                                                            %W7602231030
PASSED:  BEGIN                                                      %W7602231036
IF GT1="VOID   " THEN BEGIN PASSON~FALSE; GO LISTIT END;        %85 %W7602231042
            %   DELETES CALL ON DATIME                              %W9902231050
COMMENT INCREMENT SEQ# ONLY IF REC WOULD BE WRITTEN ON FILE;    %71 %W5102231075
IF SEQTOG AND LASTUSED!7 THEN BEGIN IF TOTALNO=-10 OR NEWBASE THEN  %W5102231100
IF INDEX THEN IF BLANKSEQ(LCR) THEN PUTSEQ(LCR,ISEQ~ISEQ+1);        %W4302231450
LISTIT:  %FOR LISTING DOLLAR CARDS                                  %W7802231475
BEGIN CHECKIT;ICNT~IF LASTUSED=1 OR LASTUSED=4 OR LASTUSED=2    %77 %W4902233000
    OR LASTUSED=5 THEN 1 ELSE                                   %77 %W4902233010
IF LASTUSED=3 THEN IF MERGELAST THEN 4 ELSE (REAL(DISKTOG)+2)ELSE 0;%W7702233030
WRITELINE;  % PRINT BEFORE FILLING                                  %W7902233035
        PRINT(LIN[0],FCR,SGNO|100000+(L DIV 4)|10+L.[46:2],         %W4902233040
            SOURCE[LASTUSED|3+LASTCOMPARE],RECORDCNT[ICNT],ICNT);   %W4902233100
                                                                    %W-502234000
% IF IMAGE IS TRUE WE CAN USE THE LEFT MARGIN                       %W7902234550
IF OMITCR!0 THEN OMITFLAG(LIN) ELSE IMAGE ~ TRUE;                   %W7602234560
IF CHECKTOG AND GT1=1 THEN                                          %W7902235000
%02235100 COVERS IF NOHEADING THEN DATIME                           %W9902235099
    BEGIN                                                           %W7902235100
    IMAGE ~ FALSE; %WE ARE GOING TO USE LEFT MARGIN                 %W7902235200
    SEQERR( LIN[0], LIN[14] )  %FLAG LEFT MARGIN                    %W7902235300
    END;                                                            %W7902235400
%DELAY PRINTING                                                     %W7902236000
    END      END;                                                   %W5102237000
    IF LASTUSED < 5 THEN                                            %W5702237005
    IF EXAMIN(FCR) = 12 THEN                                        %W5702237010
    BEGIN Q ~ ACCUM[1];                                             %W5702237015
        FLAG(ICNT~FNAME[IF LCR=CLCR THEN 1 ELSE IF LCR=SLCR     %77 %W5702237020
            THEN REAL(DISKTOG)+2 ELSE                               %W5702237025
            IF LCR=MLCR THEN 4 ELSE 0 ] ) ;                         %W7702237026
        IF ICNT > 0 THEN GO USETHESWITCH;    %W57 CUT PRT       %61 %W5702237030
    END;                                                            %W5702237040
                                                                %84 %W5702237050
    IF NEWDSK OR NEWTOG THEN IF LASTUSED!7 THEN                     %W5102237100
    BEGIN INTEGER COLS12;  LABEL COPYIT;                            %W5102237200
STREAM PROCEDURE BLANKET(D,SEQ);VALUE SEQ;                          %Y9602237210
    BEGIN DI~D; DS~8 LIT " "; SI~D; DS~8 WDS;SI~SEQ;DS~WDS END;     %Y9602237220
    LABEL NONO;                                                     %Y9602237250
    STREAM PROCEDURE MOVEREC(A,B);VALUE B;                          %W5102237300
        BEGIN  SI~B; DI~A; DS~10 WDS END;                           %W5102237400
$VOID 02238401                                                  %71 %W5102237499
IF SHIPTOG THEN IF (IF MERGELAST THEN TRUE ELSE LASTUSED!3) THEN    %Y9602238410
        GO TO COPYIT ELSE GO TO NONO;                               %Y9602238420
        COMMENT:  ALL EXCEPT ORIGINAL SYMBOLIC FILE;                %Y9602238430
IF OMITCR!0 OR STOPDOLLAR OR PASSON THEN GO COPYIT;                 %W7602238450
    IF (COLS12~DOLLARCHECK(FCR))!2 THEN                             %W5102238500
        IF PATCHTOG AND (LASTUSED<3 OR LASTUSED=5) THEN             %W5102238600
            BEGIN IF COLS12=0 THEN GO TO COPYIT END ELSE            %W5102238700
            BEGIN COPYIT:                                           %W5102238800
COMMENT IF NEWDSK THEN PUT #ND IN RT MARGIN OF LISTING-TEAR %70 %77 %W-502238810
    ELSE IF NEWTAPE THEN PUT #NT IN LISTING;                %70 %77 %W-502238820
IF LISTOG THEN NOSYN BEGIN                                      %77 %W-502238825
IF NEWDSK THEN NEWNUM(LIN[14],RECORDCNT[5]," ND");              %77 %W-502238830
IF NEWTOG THEN NEWNUM(LIN[14],RECORDCNT[5]," NT") END;          %77 %W-502238840
RECORDCNT[5]~RECORDCNT[5]+1; %INC REC CNT FOR NEWDISK(NEXT REC) %77 %W-502238850
NOSYN                                                               %W6102238899
    IF NEWDSK THEN BEGIN                                            %W5102238900
        IF (IF NOT SHIPTOG THEN FALSE ELSE                          %Y9602238910
                IF OMITCR ! 0 THEN TRUE ELSE PASSON )               %Y9602238920
            THEN BLANKET(NEWDISK(0),LCR)                            %Y9602238930
            ELSE                                                    %Y9602238940
                MOVEREC(NEWDISK(0),FCR);                            %W5102238950
                                     WRITE(NEWDISK) END;            %W5102239000
NOSYN                                                               %W6102239099
    IF NEWTOG THEN BEGIN                                            %W5102239100
        IF (IF NOT SHIPTOG THEN FALSE ELSE                          %Y9602239110
                IF OMITCR ! 0 THEN TRUE ELSE PASSON )               %Y9602239120
            THEN BLANKET(NEWTAPE(0),LCR)                            %Y9602239130
            ELSE                                                    %Y9602239140
                MOVEREC(NEWTAPE(0),FCR);                            %W5102239150
                                     WRITE(NEWTAPE) END             %W5102239200
    END; NONO:  END OF NEW TAPE OR DISK BLOCK;                      %W5102240000
IF OMITCR ! 0 THEN GO USETHESWITCH;                                 %W7602244500
            SAY(LCR,HOWFAR); %STORE SEQ# FOR OT27                   %W-102244700
XIT:LOGOUT END READACARD;                                           %W8202247000
COMMENT I.S.P. COMOUT REPLACES SCANNER-COMMENTS ----- RESULT=6;     %W6502307050
INTEGER STREAM PROCEDURE COMOUT(RESULT,NCRV); VALUE NCRV;           %W6502307100
    BEGIN   LABEL L; SI~ NCRV;                                      %W6502307150
        IF SC ! ";" THEN                                            %W6502307200
        BEGIN                                                       %W6502307250
            L:  SI~SI + 1; IF SC ! ";" THEN                         %W6502307300
            IF SC ! "%" THEN GO L ELSE                              %W6502307350
                BEGIN DI ~ RESULT; DI~DI+7; DS~LIT "0"  ;SI~SI-1 END%W6502307400
        END;                                                        %W6502307450
        SI~SI + 1; COMOUT ~ SI;                                     %W6502307500
    END COMOUT;                                                     %W6502307550
                                                                        02307599
                                                                        02307600
   CROSSHATCH,DBLDOLLAR,LESSTHAN;                                   %W-302310000
   CROSSHATCH,DBLDOLLAR,LESSTHAN;                                   %W-302312000
        BEGIN DI~A; DI~DI+3;                                        %W5002313400
            C(3(IF SB THEN TALLY~1; SKIP SB); %SET ERROR IF NOT 0-7.%W5002313600
              3(IF SB THEN DS~SET ELSE DS~RESET;SKIP SB));          %W5002313700
DEFINE LOGLVL = 5#;  LOGIN;                                         %W8202313950
 LESSTHAN:    IF STOPDEFINE THEN GO TO COMPLETE;                    %W-302334100
    RESULT~7; SCANNER;                                              %W-302334105
              COUNT ~ 0; RESULT ~ 3; SCANNER;                       %W-302334110
    RESULT~7; SCANNER;                                              %W-302334115
              IF EXAMIN(NCR)!">" THEN BEGIN                         %W-302334120
                IF COUNT=0 THEN BEGIN ACCUM[1]~"1<0000";            %W-302334130
                                      GO TO COMPLETE END;           %W-302334140
                ELBAT[NXTELBT] ~ T; NXTELBT ~ NXTELBT+1;            %W-302334150
                GO TO IPART END;                                    %W-302334160
              IF REAL(STREAMTOG)=2 THEN OCTCON(FALSE,ACCUM[0]) ELSE %W-302334170
              OCTCON(TRUE,C); T ~ 0&TRUTHV[2:41:7]&2[25:46:2];      %W-302334180
                    WRL(5); % MAKE WARNING IN XALGOL                %W9302334185
              RESULT ~ 0; SCANNER; GO TO COMPLETE;                  %W-302334190
QUOTE:      COUNT ~ 0;                                          %86 %W-502341000
            T ~ IF STREAMTOG THEN 63 ELSE 8; % ALLOW 8 CHR STRGS%86 %W-502342000
STOPDOLLAR ~ TRUE;                                                  %W7602342500
IF COUNT > T THEN BEGIN STOPDOLLAR~FALSE;GO ARGH END                %W7602345000
STOPDOLLAR ~ FALSE;                                                 %W7602346500
            IF COUNT < 8 THEN GO MOVEIT;                        %86 %W-502350000
                IF COUNT = 8 THEN %ARE FLAG BITS ALLOWED 46,47  %86 %W-502350010
                    IF REAL(STREAMTOG)= 0 THEN % NO             %86 %W-502350020
IF BOOLEAN(Q.[18:1]) THEN FLAG(529) ELSE                        %86 %W-502350030
        BEGIN WRL(7);                                               %W9302350050
        END;                                                        %W9302351500
BEGIN PHONEYDEC;     %REBLOCK TABLE FOR EFF                     %69 %W4802360500
END DEFINE BLOCK;   %REBLOCK TABLE FOR EFF                      %83 %W4802365799
COMMENT EFF HANDLING OF OCTAL CONSTANTS MOVE ADTOG W48;         %60 %W4802367050
IF GT1=""" THEN BEGIN FLAG( 99); %OCTAL CONST SYN ER 63068      %69 %W6602367100
             IF STOPDEFINE THEN GO TO COMPLETE ELSE                 %W5002367150
             BEGIN STREAMTOG.[2:1]~TRUE;  P~P-NXTELBT;              %W5002367200
                   GT1 ~ TABLE(NXTELBT);                            %W5002367210
                   NXTELBT ~ NXTELBT-1;  P ~ P+NXTELBT;             %W5002367500
                IF OCTIZE(ACCUM[1],COUNT.[47:1],COUNT)              %W5002367600
                THEN FLAG(306);ACCUM[1].[12:6]~COUNT~(COUNT+1)DIV 2;%W5002367700
                IF NOT STREAMTOG THEN                               %W5002367750
    END END;                                                        %W6602367900
BEGIN BOOLEAN ADTOG;LABEL PERCENT; % W76  W60                       %W4802367990
REAL U;                                                   %63   %64 %W4302367992
DEFINE NAMEXREF =                                         %63   %64 %W4302367994
BEGIN                                                     %63   %64 %W4302367996
                                                                        02367998
                                                                        02368000
                                                                        02368050
                                                                        02368100
                                                                        02368150
                                                                        02368200
                                                                        02368250
                                                                        02368300
                                                                        02368350
                                                                        02368400
                                                                        02368450
                                                                        02368500
                                                                        02368550
END#;                                                           %64 %W4302368600
COUNT~RESULT~ACCUM[1]~0; SCANNER; %USED TO BE REC 02368000      %64 %W4302368900
              ADTOG ~ TRUE;                                         %W-702369050
                                                                    %W7602369300
                    GO TO PERCENT                                   %W7602369400
IF Q = "3SET00" THEN BEGIN SETMODULES; GO PERCENT END;              %W7602369550
IF Q="7INCLU"OR Q="4OMIT0"THEN BEGIN USEMODULES;GO PERCENT END;     %W7602369600
IF Q="4ZERO0" THEN BEGIN COUNT~RESULT~ACCUM[1]~0;SCANNER;           %Y9502369610
IF Q~ACCUM[1] ! "4LEFT0" THEN BEGIN IF NOT DOLTOG THEN BEGIN        %Y9502369630
 U ~ REAL(LISTOG); LISTOG ~ FALSE END;                              %Y9502369635
FLAG(870);IF NOT DOLTOG THEN LISTOG~BOOLEAN(U) END; GO PERCENT END; %Y9502369640
IF Q="8STAND" THEN BEGIN REC(-24);               %IF TRUE THEN      %W9302369700
    WARNTOG~TRUE; GO PERCENT END;                %FLAG NONSTANDARD  %W9302369800
                     IF LASTUSED=1 THEN                             %W-702371500
                     LISTOG   ~ LISTOG  &FALSE[47:47:1];            %W-802372100
    PRTOG~ PRTOG&FALSE[47:47:1];                                    %W-802372200
                     DEBUGTOG ~ DEBUGTOG&FALSE[47:47:1];            %W-802372300
                     PUNCHTOG ~ DISKTOG ~ FALSE;                    %W1702372400
                    NEXTREC(SLCR,MAXTLCR, -2);                  %77 %W5702379400
     IF MERGETOG THEN MERGECOMP ELSE TLCR ~ SLCR END END            %W7702380000
    ELSE IF Q="4DISK0" THEN BEGIN IF LASTUSED=1 THEN LASTUSED~2;    %W1702380200
  LISTOG.[47:1]~ FALSE; PRTOG.[47:1]~ FALSE; DEBUGTOG.[47:1]~FALSE; %W1702380400
        REC(-5);                                                    %W9302380500
    PUNCHTOG ~ FALSE; DISKTOG ~ TRUE;                               %W5702380600
    IF MAXTLCR = 0 THEN                                             %W7702380650
    BEGIN NEXTREC(SLCR,MAXTLCR, -3);                                %W7702380700
    IF MERGETOG THEN MERGECOMP ELSE TLCR ~ SLCR END END             %W7702380800
              ELSE IF Q = "4CARD0" THEN BEGIN                       %W-702381000
                   LASTUSED ~ 1; COMMENT NEXT CARDS ARE CARD ONLY;  %W-702381300
                   LISTOG   ~ LISTOG  &FALSE[47:47:1];              %W-802381500
                   PRTOG    ~ PRTOG   &FALSE[47:47:1];              %W-802382000
                   DEBUGTOG ~ DEBUGTOG&FALSE[47:47:1];              %W-802382300
                   PUNCHTOG~FALSE;   END                            %W-702382500
     ELSE IF Q = "1-0000" THEN BEGIN REC(-7); ADTOG~FALSE END   %93 %W-702383000
              ELSE IF Q ! "1+0000" THEN GO TO PERCENT;              %W-702383500
        REC(-7);                                                    %W9302384500
    IF Q = "1-0000" THEN BEGIN REC(-7); ADTOG~FALSE END ELSE    %93 %W-702386000
IF Q="8STAND" THEN BEGIN REC(-24); WARNTOG~ADTOG END ELSE %T=NONSTD %W9302386090
IF Q="4ZERO0" THEN BEGIN COUNT~RESULT~ACCUM[1]~0; SCANNER;          %Y9502386100
IF Q~ACCUM[1] ! "4LEFT0" THEN BEGIN IF NOT DOLTOG THEN BEGIN        %Y9502386120
 U~REAL(LISTOG); LISTOG~FALSE END;                                  %Y9502386122
 FLAG(870); IF NOT DOLTOG THEN LISTOG ~ BOOLEAN(U); END END ELSE    %Y9502386125
IF Q= "4INFO0" THEN BEGIN LISTOG.[47:1]~NOTETOG~     DOLTOG~    %85 %W-502386130
    PAIRSTOG~FILESTOG~ADTOG; REC(-8);                               %W9302386131
IF XREF~ADTOG AND INDEX~SGAVL=2 OR INDEX THEN NAMEXREF END ELSE %85 %W-502386132
IF Q="5FILES" THEN BEGIN REC(-9); FILESTOG~TRUE END ELSE        %93 %W-502386140
IF Q="5ST?CK" THEN STACKTAG.[47:1] ~ ADTOG ELSE % CAN INLINE WITHOUT%W7502386150
IF Q="5PAIRS" THEN BEGIN REC(-10); PAIRSTOG~ADTOG END ELSE      %93 %W7902386160
IF Q="3DOL00" THEN BEGIN REC(-11); DOLTOG~ADTOG END ELSE%LIST $ %93 %W7802386170
IF Q="5MERGE" THEN BEGIN REC(-12); IF MERGETOG~ADTOG THEN BEGIN %93 %W7702386180
IF MLCR = 0 THEN BEGIN NEXTREC(MLCR,0,-4);                          %W7702386185
IF SLCR!0 AND SLCR!LCR THEN MERGECOMP END END END ELSE              %W7702386190
IF Q="5NOTES" THEN BEGIN REC(-13); NOTETOG~ADTOG END ELSE       %93 %W7402386200
    IF Q="5NOTES" THEN NOTETOG~ADTOG ELSE %PUT IN NOTES OPTN    %69 %W7402386210
IF Q="5PATCH" THEN BEGIN REC(-14); PATCHTOG~ADTOG END ELSE      %93 %W5102386300
                   IF Q = "4LIST0" THEN LISTOG.[47:1]~ADTOG ELSE    %W-802386500
                   IF Q = "3PRT00" THEN PRTOG.[47:1]~ ADTOG ELSE    %W-802387000
IF Q="6STREA" THEN BEGIN REC(-15); STREAMTAG.[47:1]~ADTOG END ELSE  %W3202387050
IF Q="6DOUBL" THEN BEGIN IF DOUBLETOG~ADTOG THEN REC(-16) END ELSE  %W3702387060
IF Q="5INDEX" THEN BEGIN IF INDEX~SGAVL=2 THEN BEGIN REC(-17);  %93 %W4302387200
        NAMEXREF END END ELSE                                   %93 %W6302387205
IF Q="4SAVE0" THEN BEGIN REC(-18);                              %93 %W4302387250
    SAVETOG ~ SAVETOG & TRUE [41:47:1] END ELSE                 %93 %W4302387260
IF Q="4XREF0" THEN BEGIN IF XREF~ADTOG AND INDEX~SGAVL=2 OR     %63 %W4302387300
    INDEX THEN BEGIN REC(-19); NAMEXREF END END ELSE            %93 %W4302387400
IF Q="4SHIP0" THEN SHIPTOG~TRUE ELSE % TURNON ONLY                  %Y9602387450
IF Q="5PURGE" THEN BEGIN REC(-20);                              %93 %W1302387500
    PURGETOG ~ PURGETOG OR ADTOG END ELSE                       %93 %W1302387600
IF Q="3NEW00"THEN BEGIN COUNT~RESULT~ACCUM[1]~0;SCANNER;            %W1702388000
IF Q~ACCUM[1] = "4TAPE0" THEN BEGIN REC(-21);                   %93 %W1702388010
    NEWTOG ~ NEWTOG OR ADTOG END ELSE                           %93 %W1702388015
IF Q="4DISK0" THEN BEGIN REC(-6); NEWDSK~NEWDSK OR ADTOG END    %93 %W1702388020
    ELSE GO PERCENT END ELSE                                        %W9302388025
IF Q="4STOP0" THEN BEGIN REC(-22);                              %93 %W2802388030
        STOPTOG ~ ADTOG; LASTUSED ~ 3 END ELSE                  %93 %W2802388040
IF Q="5ERROR" THEN IF SGAVL=2 THEN BEGIN REC(-23);              %93 %W1202388220
    ERROROPTION ~ TRUE END                                      %93 %W1202388230
        ELSE FLAG(-314) ELSE                                        %W1202388240
COMMENT TO CORRECT HANDLING OF $+  AND $-;                      %65 %W-702388700
IF Q="1+0000" THEN BEGIN WHILE EXAMIN(NCR) = " " AND (LCR.[33:15]-  %W6502388720
NCR.[33:15]!1 OR NCR.[30:3]!7) DO BEGIN RESULT~5;SCANNER END;   %65 %W-702388740
                             IF EXAMIN(NCR) < 10 THEN               %W-702388800
                                       SCANNER;                     %W-702388910
END ELSE BEGIN REC(-7); ADTOG~TRUE END END ELSE                 %93 %W-702388930
               BEGIN IF DEBUGTOG~DEBUGTOG&ADTOG[47:47:1] THEN BEGIN %W-802390000
                   END END ELSE                                     %W-802413000
                   IF Q = "5PUNCH" THEN PUNCHTOG ~ ADTOG ELSE       %W-702414000
GO TO PERCENT END UNTIL FALSE; PERCENT:                             %W7602415000
    END EFFICIENCY BLOCK;                                           %W4802415500
    IF LIBINDEX>0 THEN                                              %W2002416200
    IDENT:                                                          %W8202507300
                                                                        02507325
                                                                        02507350
                                                                        02507400
        IF T ~ SUPERSTACK[SCRAM ~ (Q ~ ACCUM[1]) MOD 125]!0         %W8202508000
                                                                        02508449
                                                                        02508450
BEGIN T ~ 0;                                                        %W8202511000
                                                                        02511099
                                                                        02511100
GO TO COMPLETE END;                                                 %W8202511200
                                                                        02517099
                                                                        02517100
                                                                        02517500
                                                                        02517600
COMMENT SCAN COMMENTS;                                              %W6502521000
        STOPDOLLAR ~ TRUE;                                          %W7602521005
DO BEGIN                                                            %W6502521010
    RESULT ~ 7;                                                     %W6502521020
    IF    NCR~ COMOUT(RESULT,NCR) = LCR THEN                        %W6502521030
        BEGIN   READACARD;                                          %W6502521040
            IF LIBINDEX !0 THEN IF RECOUNT=FINISHPT THEN            %W6502522000
                BEGIN                                               %W6502522010
                    SEARCHLIB(FALSE); READACARD; NORELEASE~FALSE    %W6502522020
                END                                                 %W6502522030
        END                                                         %W6502522040
   END UNTIL BOOLEAN(RESULT);                                       %W6502522050
       STOPDOLLAR ~ FALSE;                                          %W7602522060
    GO TO SCANAGAIN END END;                                        %W6502523000
IF XREF THEN IF NOT ENDTOG AND    T.LINKR!1 THEN WRITEPRT(T,1,8);   %W4302524500
BEGIN PHONEYDEC;    %REBLOCK TABLE FOR EFF                      %69 %W4802525500
  IF T.ADDRESS ! 0 THEN T~FIXDEFINEINFO(T) ELSE IF COUNT=12 THEN    %W9302526500
        IF (IF Q="?TIMEC" THEN TRUE ELSE Q="?DATEC") THEN           %W9302526550
        BEGIN                                                       %W9302526600
        BOOLEAN STREAM PROCEDURE TOFLAG(A,V); VALUE V;              %W9302526650
        BEGIN SI~A; DI~ LOC V; DI~DI+1;                             %W9302526700
            IF 7 SC=DC THEN TALLY ~ 1; TOFLAG~TALLY END;            %W9302526750
        IF TOFLAG(ACCUM[2],"OMPILED") THEN WRL(8);                  %W9302526800
        END;                                                        %W9302526850
END MACRO BLOCK;    %REBLOCK TABLE FOR EFF                      %69 %W4802532500
DBLDOLLAR:                                       %W69           %76 %W5102533010
$ VOID 02533499                                                 %76 %W5102533011
             SEARCHLIB(TRUE);   GO TO PERCENT;                      %W3502533500
                   MOVE(11,ELBAT[65],ELBAT);                        %W-302539000
                   NXTELBT ~ NXTELBT-65 END END;                    %W-302542000
LOGOUT                                                              %W8202547600
        BEGIN PHONEYDEC; COUNT ~ ACCUM[1] ~ 0;                      %W4802554000
LASTELCLASS ~ ELCLASS;       %SAVE NEXT TO LAST CHARACTER           %W4002554500
                    %ELIMINATE BLANKS                               %W6502555000
                                                                    %W7602579000
    INTEGER MYCLASS;%                                               %W7602581200
    REAL WORD;%                                                     %W7602581300
    INTEGER MAXBOOLS;%                                              %W7602581400
    INTEGER ARRAY OPTION[0:99];%                                    %W7602581500
    INTEGER PROCEDURE NEXT;  FORWARD;%                              %W7602581600
    DEFINE SKIPIT=MYCLASS~NEXT#;%                                   %W7602581700
    INTEGER PROCEDURE BOOLSEARCH(B);  VALUE B;  BOOLEAN B;  FORWARD;%W7602581800
    BOOLEAN PROCEDURE BOOLEXP;           FORWARD;%                  %W7602581900
    BOOLEAN PROCEDURE BOOLPRIM;          FORWARD;%                  %W7602582000
    PROCEDURE BOOLCOMP(B);  BOOLEAN B;   FORWARD;%                  %W7602582100
                                                                    %W7602582200
    INTEGER PROCEDURE BOOLSEARCH(B);%                               %W7602582300
    VALUE B;  BOOLEAN B;%                                           %W7602582400
    BEGIN LABEL EXIT, FOUND;%                                       %W7602582500
         INTEGER I, Q;%                                             %W7602582600
         Q ~ ABS(ACCUM[1]);%                                        %W7602582700
       DO IF ABS(OPTION[I])=Q THEN GO TO FOUND UNTIL I~I+1>MAXBOOLS;%W7602582800
         IF B THEN ELSE GO TO EXIT;%                                %W7602582900
         OPTION[MAXBOOLS ~ I] ~ Q;%                                 %W7602583000
         IF MAXBOOLS}99 THEN%                                       %W7602583100
         BEGIN FLAG(902);  MAXBOOLS ~ MAXBOOLS-1;  END;%            %W7602583200
    FOUND:%                                                         %W7602583300
         IF B THEN OPTION[I] ~ REAL(BOOLEAN(ABS(OPTION[I]))%        %W7602583400
           OR (BOOLEAN(-0) AND BOOLEAN(ACCUM[1])))%                 %W7602583500
         ELSE BOOLSEARCH ~ SIGN(OPTION[I]);%                        %W7602583600
    EXIT:%                                                          %W7602583700
    END BOOLSEARCH;%                                                %W7602583800
                                                                    %W7602583900
    INTEGER PROCEDURE NEXT;%                                        %W7602584000
    BEGIN LABEL EXIT;%                                              %W7602584100
         INTEGER T;%                                                %W7602584200
         DEFINE GOOF = FLAG(905)#,%                                 %W7602584300
                ERROR= BEGIN GOOF;  GO TO EXIT END#;%               %W7602584400
         COUNT ~ RESULT ~ ACCUM[1] ~ 0;%                            %W7602584500
         SCANNER;  Q ~ ACCUM[1];%                                   %W7602584600
         IF RESULT=3 THEN ERROR;%                                   %W7602584700
         IF RESULT=2 THEN%                                          %W7602584800
         BEGIN T ~ (T ~ Q.[18:6]-2)&T[42:41:3];%                    %W7602584900
IF T=11 OR T=19 OR T~IF T=25 THEN 20 ELSE T =20 THEN            %87 %W7602585000
        WORD ~ SPECIAL[T] ELSE GOOF; %DONT NEED SEMICOLON       %87 %W7602585050
              GO TO EXIT%                                           %W7602585100
         END SPECIAL CHARACTERS;%                                   %W7602585200
    COMMENT LOOK FOR BOOLEAN OPERATORS, THEN "BOOLEANS.";           %W7602585300
         IF Q="3NOT00" THEN T~NOTOP ELSE%                           %W7602585400
         IF Q="3AND00" THEN T~ANDOP ELSE%                           %W7602585500
         IF Q="2OR000" THEN T~OROP  ELSE%                           %W7602585600
         IF Q="3EQV00" THEN T~EQVOP ELSE                            %W7602585700
IF Q="3IMP00" THEN  T ~ IMPOP;                                      %W7602585750
         IF T!0 THEN WORD.CLASS ~ T ELSE%                           %W7602585800
         BEGIN COMMENT MUST BE BOOLEAN ID;                          %W7602585900
              IF (T ~ BOOLSEARCH(FALSE))!0 THEN%                    %W7602586000
              WORD ~ 1&BOOID[2:41:7]&T[1:1:1] ELSE GOOF;%           %W7602586100
         END;%                                                      %W7602586200
    EXIT:%                                                          %W7602586300
         NEXT ~ MYCLASS ~ WORD.CLASS;%                              %W7602586400
    END NEXT;%                                                      %W7602586500
                                                                    %W7602586600
    BOOLEAN PROCEDURE BOOLEXP;%                                     %W7602586700
    BEGIN BOOLEAN B;%                                               %W7602586800
         B ~ BOOLPRIM;%                                             %W7602586900
         WHILE MYCLASS}EQVOP AND MYCLASS{ANDOP DO BOOLCOMP(B);%     %W7602587000
    BOOLEXP ~ B END BOOLEXP;%                                       %W7602587100
                                                                    %W7602587200
    BOOLEAN PROCEDURE BOOLPRIM;%                                    %W7602587300
    BEGIN BOOLEAN B, KNOT;%                                         %W7602587400
         IF KNOT ~ (NEXT=NOTOP) THEN SKIPIT;%                       %W7602587500
         IF MYCLASS=LEFTPAREN THEN%                                 %W7602587600
         BEGIN B ~ BOOLEXP;%                                        %W7602587700
              IF MYCLASS!RTPAREN THEN FLAG(900);%                   %W7602587800
         END ELSE IF MYCLASS!BOOID THEN FLAG(901)%                  %W7602587900
         ELSE B ~ WORD<0;%                                          %W7602588000
         IF KNOT THEN B ~ NOT B;  SKIPIT;%                          %W7602588100
    BOOLPRIM ~ B END BOOLPRIM;%                                     %W7602588200
                                                                    %W7602588300
    PROCEDURE BOOLCOMP(B);  BOOLEAN B;%                             %W7602588400
    BEGIN REAL OPCLASS;%                                            %W7602588500
         BOOLEAN T;%                                                %W7602588600
         OPCLASS ~ MYCLASS;%                                        %W7602588700
         T ~ BOOLPRIM;%                                             %W7602588800
         WHILE OPCLASS<MYCLASS DO BOOLCOMP(T);%                     %W7602588900
         IF OPCLASS=ANDOP THEN B ~ B AND T ELSE%                    %W7602589000
         IF OPCLASS=OROP  THEN B ~ B OR  T ELSE%                    %W7602589100
        IF OPCLASS=IMPOP THEN B~ B IMP T ELSE                       %W7602589200
                    B ~  B EQV T;                                   %W7602589250
    END BOOLCOMP;%                                                  %W7602589300
                                                                    %W7602589400
    PROCEDURE USEMODULES;%                                          %W7602589500
    BEGIN BOOLEAN OMITOG;%                                          %W7602589600
         REAL TEMP;%                                                %W7602589700
         STREAM PROCEDURE BOMBSEQNO(ACCUM,VPL,OCR,COUNT);%          %W7602589800
         VALUE COUNT;%                                              %W7602589900
         BEGIN DI~VPL;  DS~8 LIT " ";  DI~VPL;  SI~ACCUM;%          %W7602590000
              SI~SI+3;  DS~COUNT CHR;  DI~VPL;  VPL~DI;%            %W7602590100
              DI~OCR;  SI~LOC VPL;  DS~WDS;%                        %W7602590200
         END BOMBING SEQUENCE NUMBERS TO RIGHT PLACES;%             %W7602590300
         OMITOG ~ ACCUM[1]="4OMIT0";%                               %W7602592200
        REC(-4);                                                    %W9302592250
         COUNT ~ RESULT ~ ACCUM[1] ~ 0;  SCANNER; Q ~ ACCUM[1];     %W7602592300
         IF RESULT=3 THEN BOMBSEQNO(ACCUM[1],VOIDPLACE,TEMP,%       %W7602592400
              IF COUNT<8 THEN COUNT ELSE 8) ELSE FLAG(906);%        %W7602592500
         COUNT ~ RESULT ~ ACCUM[1] ~ 0;  SCANNER; Q ~ ACCUM[1];     %W7602592600
         IF ACCUM[1]!"1,0000" THEN FLAG(903);%                      %W7602592700
         COUNT ~ RESULT ~ ACCUM[1] ~ 0;  SCANNER; Q ~ ACCUM[1];     %W7602592800
         IF ACCUM[1]!"2IF000" THEN FLAG(904);%                      %W7602592900
        IF BOOLEXP EQV OMITOG THEN OMITCR ~ TEMP ELSE VOIDPLACE ~ 0;%W7602593000
        IF MYCLASS!SEMICOLON THEN FLAG(907);                    %87 %W7602593100
    END USEMODULES;%                                                %W7602593200
    PROCEDURE SETMODULES;%                                          %W7602593300
    BEGIN LABEL EXIT;%                                              %W7602593400
         DEFINE ERROR= BEGIN Q~ACCUM[1];FLAG(908);  GO TO EXIT END#;%W7602593500
         DO BEGIN%                                                  %W7602593600
        REC(-3);                                                    %W9302593650
         COUNT ~ RESULT ~ ACCUM[1] ~ 0;  SCANNER;%                  %W7602593700
         IF RESULT!1 THEN ERROR;  Q ~ ACCUM[1];%                    %W7602593800
         COUNT ~ RESULT ~ ACCUM[1] ~ 0;  SCANNER;%                  %W7602593900
         IF ACCUM[1]!"1=0000" THEN ERROR;%                          %W7602594000
         COUNT ~ RESULT ~ ACCUM[1] ~ 0;  SCANNER;%                  %W7602594100
         IF ACCUM[1]!"5FALSE" THEN%                                 %W7602594200
           IF (Q ~ Q&REAL(ACCUM[1]="4TRUE0")[1:47:1])>0 THEN ERROR;%%W7602594300
         ACCUM[1] ~ Q;  Q ~ BOOLSEARCH(TRUE);%                      %W7602594400
         COUNT ~ RESULT ~ ACCUM[1] ~ 0;  SCANNER;%                  %W7602594500
         END UNTIL ACCUM[1]!"1,0000";%                              %W7602594600
    EXIT:%                                                          %W7602594700
    END SETMODULES;%                                                %W7602594800
BOOLEAN PROCEDURE EXPLICITFORMAT;  FORWARD;                         %W4003056500
BOOLEAN PROCEDURE FORMATPHRASE;  FORWARD;                           %W4003056600
IF NOTETOG THEN FLAG(538&"H"[1:43:2]); % SLOW POWER CALC        %91 %W7404078800
                    BEGIN WRITELINE; % PRINT BEFORE FILLING         %W7904149000
        IMAGE ~ FALSE;  DEBUGWORD( B2D(L), WORD, LIN ) END;         %W7904150000
                     IF REAL(BOOLEAN(INFO[0,255-N])EQV BOOLEAN(C))  %W-304209000
                        = REAL(NOT FALSE) THEN GO TO FOUND;         %W-304209500
ALPHA PROCEDURE   ZEARCH (Q,KEY);VALUE KEY;ARRAY Q[0]; REAL KEY;%64 %W4304231000
       L:     ZEARCH~Q[  I +1] ;                                %64 %W4304243000
        BEGIN  PHONEYDEC;                                           %W4804247090
        END B2D;                                                    %W4804248010
    BEGIN   LABEL LL;                                               %W3704278500
    WRITELINE; % PRINT BEFORE FILLING                               %W7904278750
    WRITE(LIN[*],BUG,B2D(L),COP[S.[42:6]],B2D(S.[36:6]),            %W3704280000
    WRITE(LIN[*],BUG,B2D(L),IF T1~S.[46:2]=1 THEN                   %W3704285000
    IMAGE ~ FALSE  END DEBUG;                                       %W7904287100
              IF L < 4092 THEN                                      %W3004293000
          IF BUGGER~ZEARCH(WOP,OP)="    " THEN                  %64 %W4304603000
COMMENT TO HANDLE PARA DEFINES MOVE STEPI W27;                  %60 %W2705002000
                                                                    %W2705003000
REAL T; BOOLEAN BT; % TEMP                                          %W5805013100
STREAM PROCEDURE ANYMORE(LIN,T); % HANDLES LINKS IN ALGOL/ERRORS    %W-905013200
    BEGIN SI~LIN; DI~T; DS~4 OCT; % PUT OUT LINK VALUE              %W-905013300
          SI~SI+4; DI~LIN; DS ~ 9 WDS; DS ~ 8 LIT " " END;          %W-905013400
STREAM PROCEDURE WRITERROR(ERN,LIN,SKW,SKC,ACC,CNT,LST);            %W-905016000
    VALUE ERN,SKC,CNT;                                              %W-905017000
BEGIN                                                           %69 %W-905018000
    DI ~ LIN; DS ~ 8 LIT " ";                                   %69 %W-905018100
    SI ~ LIN; DS ~ 9 WDS; DS ~ 4 CHR;                           %69 %W-905018200
    SI ~  LST; SI ~ SI-8; DS ~ 8 CHR;                           %69 %W-905018300
    SI ~ LIN; DS ~ 10 CHR;                                      %78 %W-905018400
    SI ~ LOC ERN;                                               %69 %W-905018500
    IF SC ! "H" THEN                                            %69 %W-905018600
        DS ~ 18 LIT "|"                                         %69 %W-905018700
        ELSE                                                    %69 %W-905018800
            DS ~ 18 LIT " ";                                    %69 %W-905019000
    DI ~ LST; DI ~ DI - 8; SI ~ LST; DS ~ WDS; DI ~ LIN;            %W-905019500
        SI~LOC ERN;IF SC="+"THEN DS~8LIT"WARNING "ELSE              %W1005020000
                    IF SC="8"THEN DS~8LIT"FLUSHED "ELSE             %W-405020500
IF SC="H" THEN DS ~ 5 LIT "NOTE " ELSE                          %69 %W-905020600
                        DS ~ 6 LIT"ERROR ";                         %W-905021000
        SI ~ LOC CNT; IF SC ! "+" THEN                              %W-905021500
        BEGIN SI ~ ACC; SI ~ SI + 3; DS ~ CNT CHR;                  %W-905022000
        END;                                                        %W-905023000
        DI~SKW; DI~DI+SKC;                                          %W-905024000
% 05024200 KILLS M9 CHANGES--NEAR LINE PO                           %W-905024100
$ VOID 05024601                                                     %W9905024200
        DS~3 LIT "<--";                                             %W-905024900
    END WRITERROR;                                                  %W-905025000
      OWN BOOLEAN LOOPTOG;                                          %W-405026000
IF ERRNUM } 0 THEN                                                  %W6105026550
NOSYN BEGIN CLOSE(DATA,PURGE); CLOSE(NEWTAPE, PURGE);               %W6105026600
            CLOSE(NEWDISK, PURGE); CLOSE (CODE, PURGE) END;         %W6105026700
          IF NOT LOOPTOG THEN BEGIN                                 %W-405027000
            IF ERRNUM = 1                                           %W-405028000
            OR ERRNUM = 6                                           %W-405029000
            OR ERRNUM = 12                                          %W-405029100
                OR ERRNUM = 870  % WRONG "$ ZERO"                   %Y9505029200
            OR ERRNUM = 200                                         %W-405030000
            OR ERRNUM = 135                                         %W-405030100
            OR ERRNUM = 259                                         %W-405030200
            OR ERRNUM = 528 % EOF BEFORE "END. "                %84 %W5705030250
        OR ERRNUM = 170        %MISSING PROG-NO 1ST BEGIN--EOF  %59 %W5705030300
            OR ERRORCOUNT } 100                                     %W-405031000
              THEN BEGIN LOOPTOG~ERRORTOG~TRUE;                     %W-405031300
IF ERRORCOUNT } 100 THEN ERRNUM ~ 909;                              %W-405031400
                         ERRNUM ~ ERRNUM & 8[1:43:5] END;           %W-405031500
IF ERRNUM } 0 THEN                                          %91  74 %W1005034000
BEGIN SPECTOG ~ FALSE; ERRORCOUNT ~ ERRORCOUNT +1;              %91 %W7405034500
END; COMMENT COUNT ERRORS;                                      %91 %W7405035000
IF ERRNUM } 0 OR LISTOG THEN BEGIN %WARN AND NOTES IF LIST  %69 %77 %W1005035500
IF NOT LISTOG  % DROPS AND NOT REMOTOG --M9                         %W9905036000
INTEGER ICNT;CHECKIT;ICNT~IF LASTUSED=1 OR LASTUSED=2 OR        %77 %W4905038000
    LASTUSED=5 THEN 1 ELSE IF LASTUSED=3 THEN                   %77 %W4905038050
    IF MERGELAST THEN 4 ELSE (REAL(DISKTOG)+2) ELSE 0;              %W7705038100
    WRITELINE; % PRINT BEFORE FILLING                               %W7905038300
        PRINT(LIN [0],FCR,-(ABS(ERRNUM)|10000+ERRORCOUNT)|10,SOURCE[%W-905038500
IF LASTUSED>7 THEN 0 ELSE                                       %77 %W-905038550
        LASTUSED|3+LASTCOMPARE],RECORDCNT[ICNT],ICNT);              %W4905038600
                    MOVE(1,INFO[LASTSEQROW,LASTSEQUENCE],LIN [10]); %W-505039000
                                                                    %W-505039500
% KILL TEMP IF NOHEADING THEN DATIME                                %W9905039600
        IMAGE ~ FALSE; % ERROR IMAGE NOT REGULAR IMAGE              %W7905040000
$ VOID 05043201 % OLD W16 PATCH                                     %W1605043100
IF DEFINEINDEX!0 THEN                                               %W-905044000
    BEGIN ZOT(ACCUM[0],0,MKABS(DEFINEARRAY[2]));                    %W-905044200
        RR1~ 10+(RR2~ DEFINEARRAY[2].[15:33]).[33:15]-RR2.[15:15];  %W-905044400
        RR2~ RR2.[30:3];                                            %W-905044600
        ZOT(ACCUM[0],"%",MKABS(DEFINEARRAY[2]))                     %W-905044800
    END                                                             %W-905045000
ELSE                                                                %W-905045100
    BEGIN RR1~ 1+NCR.[33:15]-FCR;                                   %W-905045200
        RR2~ NCR.[30:3]                                             %W-905045300
    END;                                                            %W-905045400
IF RR1<1 OR RR1>10 THEN RR1~ RR2~10;                                %W-905045500
IF ABS(ERRNUM-161)>1 AND DEFINEINDEX=0 THEN RR3 ~ -1                %W-905045600
                                       ELSE RR3 ~ Q.[12:6];         %W-905045650
    WRITELINE; % PRINT BEFORE FILLING                               %W7905045675
WRITERROR(ERRNUM, LIN, LIN[RR1], RR2, ACCUM[1], RR3,                %W-905045700
    INFO[LASTSEQROW,LASTSEQUENCE]);                                 %W-905045710
    BT~TRUE;  % BLANK FIRST TIME                                    %W-905045720
    T ~ ABS(ERRNUM);                                                %W-905045730
    DO BEGIN % LOOP FOR MULTILINE ERROR MESSAGES                    %W-905045740
        WRITELINE; IF BT THEN BEGIN BLANK(LIN); BT~FALSE END;       %W-905045750
        READ ( ERRORS[T], 10, LIN[*]);                              %W-905045760
        ANYMORE (LIN, T);                                           %W-905045770
        END UNTIL T=0;                                              %W-905045780
    WRITELINE; BLANK(LIN);                                          %W-905045800
IMAGE~FALSE; % SEE 05046050 FOR FOLLOWING:                          %W7905045900
IF NOT TWXOFF THEN IF ERRNUM}0 THEN                                 %W5805045910
    IF TWXTIME~MIN(2400,TWXTIME+(T~TIME(1)))<0 THEN TWXOFF~TRUE ELSE%W5805045920
        IF STATUS(STATARRAY[*]) { 0 THEN TWXOFF~TRUE ELSE           %W5805045930
            BEGIN                                                   %W5805045940
            TWXTIME~TWXTIME-T-600; % ALLOW 10 SECONDS               %W5805045950
            TWXCOUNT~TWXCOUNT+1; % SEE IF WE GET THEM ALL           %W5805045960
$ VOID 05046007 % OLD W58 PATCH                                     %W5805046000
            BEGIN                                                   %W5805046008
            LABEL DONE;                                             %W5805046010
            STREAM PROCEDURE OUTTWX(D,E,I,F,R,N); VALUE E,F,R,N;    %W5805046012
              BEGIN DI ~ D; DS ~ 2 LIT "{!"; SI ~ LOC E;            %W5805046014
                IF SC = "8" THEN DS ~ 8 LIT "FLUSHED " ELSE         %W5805046016
                DS ~ 6 LIT "ERROR "; DS ~ 3 DEC; DS ~ 4 LIT " AT";  %W5805046018
                SI ~ I; SI ~ SI +3; DS ~ N CHR; DS ~ 4 LIT " IN ";  %W5805046020
                SI ~ LOC F; SI ~ SI + 4; DS ~ 4 CHR;                %W5805046022
                DS ~ 9 LIT " RECORD #"; N ~ DI; DI ~ LOC F;         %W5805046024
                DS ~ 5 DEC; DI ~ LOC F; DS ~ 4 FILL;DI ~ N;         %W5805046026
                SI~LOC F; 4(IF SC! " " THEN DS~CHR ELSE SI~SI+1);   %W5805046028
                DS ~ CHR; DS ~ 3 LIT "{!~" END;                     %W5805046030
    IF DEFINEINDEX=0 THEN RR1 ~LASTUSED ELSE RR1 ~DEFINEARRAY[0];   %W5805046031
            OUTTWX(STATARRAY[1],ERRNUM,ACCUM[1],                    %W5805046032
              RR3~ IF RR1=1 OR LASTCOMPARE ! 0 THEN "1CARD"         %W5805046034
              ELSE IF RR1=3 THEN IF DISKTOG THEN "3DISK"            %W5805046036
              ELSE "2TAPE" ELSE "0CAST",                            %W5805046038
            RECORDCNT[RR3.[18:6]],                                  %W5805046040
            MIN( 14, Q.[12:6] ) );                                  %W5805046042
WRITE(TWX(STATARRAY[0],0),8,STATARRAY[*]) [DONE];%DONT WAIT     %74 %W5805046044
            DONE: END;                                              %W5805046046
            END IF NOT TWXOFF;                                      %W5805046048
COMMENT W58 APPEARS IN TWO PLACES -- 05046000 AND 09407500          %W5805046050
        PART 1 PRINTS THE 1ST 5 ERRORS ON THE TWX. 05046000         %W5805046051
        PART 2 PRINTS THE # OF ERRORS NOT LISTED. 09407500          %W5805046052
        A TEST IS MADE BEFOR EACH WRITE ON TWX TO INSURE            %W5805046053
            ATTACHED STATION.                                       %W5805046054
        PART 2 ASSUMES STATUS CONSTRUCT AND ERRORCOUNT ONLY.        %W5805046055
        PART 1:ASSUMES RECORDCNT WHICH IS DECLARED IN W49 AND       %W5805046056
            USED IN W57,W49.                                        %W5805046057
            ALWAYS PRINTS THE LAST IDENT SCANNED.                   %W5805046058
            ASSUMES STATUS CONSTRUCT.                               %W5805046059
            LASTUSED DETERMINES WHERE LAST RECORD COME FROM:        %W5805046060
            A.IF IN A DEFINE (DEFINEINDEX!0,LASTUSED>7              %W5805046061
               OR = 0) USE DEFINEARRAY[0] ELSE                      %W5805046062
            B.LASTUSED OF 1=CARD,2=TAPE,3=DISK,                     %W5805046063
                ELSE CAST.                                          %W5805046064
            VALUE OF REC # FROM RECORDCNT[LASTUSED{3]               %W5805046065
                (REC # NOT KEPT FOR CAST);                          %W5805046066
        END;                                                        %W5805046099
IF LOOPTOG THEN GO TO ERRORPRINT;                                   %W-405046100
                   IF ERRNUM } 0 THEN                               %W1005046900
    END END                                                         %W-905060000
$ VOID 05100001                                                     %W5805061000
            BEGIN PHONEYDEC; FLAG(ERRNUM);                          %W4805106000
THEN IF GT1~ ELBATWORD.[16:11]}512 THEN IF GT1 < 1024           %87 %W-105118500
%ALLOW UP LEVEL ADDRESSING IF THERE IS A PRT                    %87 %W-105118600
ELSE WRL(1) ELSE WRL(1)  %M9 BUT NOT XALGOL                         %W9305119500
            PHONEYDEC;                                              %W4805152500
            PHONEYDEC;                                              %W4805185100
        BEGIN PHONEYDEC;                                            %W4805188100
        END TAKEFRST;                                               %W4805189100
                PHONEYDEC;                                          %W4805200100
            BEGIN PHONEYDEC; GT1~0;                                 %W4805231000
        IF NOT STREAMTAG THEN BEGIN FLAG(276);ERRORTOG~TRUE;    %87 %W3205231500
            STREAMTAG~ TRUE END;                                %87 %W3205231600
            BEGIN PHONEYDEC;IF SPAC=0 THEN SPAC~GETSPACE(TRUE);     %W4805260000
    THEN BEGIN WRITELINE; WRITE(LIN[*],DEBUGDESC,B2D(SPAC),TYPE,%79 %W3705264000
        B2D(RELAD),SGNO); IMAGE ~ FALSE END; %PRINT-FILL        %79 %W3705265000
        REC(0);                                                     %W9305273200
            PHONEYDEC;                                              %W4805293100
        BEGIN PHONEYDEC; RANGE~ELCLASS}LOWER AND ELCLASS{UPPER END; %W4805304000
IF PRTOG THEN BEGIN WRITELINE; WRITE(LIN[*],PUTPRT,B2D(GS),     %79 %W3705377000
    IF Q THEN "P" ELSE "S"); IMAGE~FALSE END END GETSPACE;      %79 %W3705378000
                IF ELCLASS=CROSSHATCH THEN BEGIN                    %W5206003200
                STEPIT; SIMPARITH END ELSE                          %W5206003400
TIME:   PANA;                                                       %W9306073000
        IF (T1~GET(L-1)).[46:2] ! 0 THEN WRL(11) ELSE               %W9306073025
            IF T1 DIV 4 > 4 THEN WRL(12);                           %W9306073050
        EMITL(1); EMITO(COM);                                       %W9306073075
              INTEGER SL;                                           %W5206089500
                        IF STACKTAG THEN IF INLINETAG THEN          %W7506091115
IF Q="5STACK" THEN BEGIN WRL(26); STEPIT; GO TO LDOT END;       %93 %W7506091120
              IF ELCLASS=FACTOP THEN                                %W5206091200
                   IF LINKTOG THEN  %  LAST SYLLABLE IS NOT A LINK  %W5206091300
IF (SL~GET(L-1)).[46:2]=3 OR SL=673 THEN BEGIN %$"              %69 %W5206091400
        WRL(19);                                                %93 %W5206091450
                        ELBAT[I].CLASS~ELCLASS~CROSSHATCH; STEPIT;  %W5206091500
                        EMITO(DUP); EMITO(LOD); GO EXIT END;        %W5206091600
                        STEPIT;                                     %W7506124000
                        IF INLINETAG THEN                           %W7506124025
                            IF ELBAT[I].ADDRESS = STREAMV THEN      %W7506124050
                                BEGIN                               %W7506124075
                                    INLINE;                         %W7506124100
                                    IF NOT INLINETOG.[46:1] THEN    %W7506124125
                                        FLAG(092); %NOT EXPRESS TYPE%W7506124150
                                    INLINETOG ~ FALSE               %W7506124175
                                END                                 %W7506124200
                              ELSE                                  %W7506124225
                                    AEXP                            %W7506124250
                          ELSE                                      %W7506124275
                                AEXP;                               %W7506124300
                IF ELCLASS=CROSSHATCH THEN GT4~BOOSEC~BTYPE ELSE    %W5206152500
                        IF STACKTAG THEN IF INLINETAG THEN          %W7506226400
IF Q="5STACK" THEN BEGIN WRL(26); STEPIT; GO TO TD END;         %93 %W7506226500
    COMMENT BOOLEAN CONSTANT;                                       %W-306255000
              IF TYPE ~ 0&ELBAT[I][44:25:2]>7 THEN EMITNUM(C)       %W-306255500
                ELSE EMIT(TYPE); STEPIT; GO TO T;                   %W-306256000
                        STEPIT;                                     %W7506259000
                        IF INLINETAG THEN                           %W7506259050
                            IF ELBAT[I].ADDRESS = STREAMV THEN      %W7506259100
                                BEGIN                               %W7506259150
                                    INLINE;                         %W7506259200
                                    IF NOT INLINETOG.[46:1] THEN    %W7506259250
                                        FLAG(092); %NOT EXPRESS TYPE%W7506259300
                                    INLINETOG ~ FALSE;              %W7506259350
                                    TYPE ~ BOOPRIM ~ BTYPE          %W7506259400
                                END                                 %W7506259450
                              ELSE                                  %W7506259500
                                    TYPE ~ BOOPRIM ~ EXPRSS         %W7506259550
                          ELSE                                      %W7506259600
                              TYPE ~ BOOPRIM ~ EXPRSS;              %W7506259650
                BOOLEAN MULTIPLE; LABEL DOITAGAIN;                  %W5306282100
       DOITAGAIN:                                                   %W5306282900
                STEPIT; AEXP;                                       %W5306287000
                IF ELCLASS=RELOP THEN BEGIN                         %W5306287100
                    EMITPAIR(JUNK,SND); EMIT (OPERATOR);            %W5306287200
IF MULTIPLE THEN EMITO(LND) ELSE WRL(20); EMITV(JUNK);          %93 %W5306287300
                    MULTIPLE ~ TRUE; GO DOITAGAIN;                  %W5306287400
                END;                                                %W5306287500
                EMIT (OPERATOR);                                    %W5306287600
                IF MULTIPLE THEN EMITO(LND);                        %W5306287700
                STACKCT ~ 1;                                        %W5306287900
                EMIT(REAL(MULTIPLE)); L~L-1;                        %W5306288000
        REC(4);                                                     %W9306315200
IF NOTETOG THEN IF NOT NO519 THEN %FLAG USE OF GOTOSOLVER       %91 %W7406379500
    FLAG(519&"H"[1:43:2]);                                          %W7406379600
                PHONEYDEC;                                          %W4806414100
                PHONEYDEC;                                          %W4806420100
         BEGIN LABEL EXIT; REAL X;                                  %W2706501000
              FOR X~1 STEP 1 UNTIL 6 DO                             %W2706506000
EXIT:  FOR X:=X STEP 1 UNTIL 5 DO                                   %W2706512000
            PAIR(FALSE);   %END                                     %W7907015500
                        CARDEOF ~ TRUE;                             %W5707028000
%                                                                   %W5707029000
                        LASTUSED ~ 1;                               %W5707030000
ANOTHER:                                                            %W4007079000
IF FBIT THEN BEGIN VBIT~FALSE;SCLASS~LOCLID;                        %W7507081000
            IF SBIT THEN FBIT~FALSE END                             %W7507081500
             IF SCLASS=FRMTID THEN IF EXPLICITFORMAT THEN           %W4007094100
             BEGIN STEPIT;  GO TO COMMON END;                       %W4007094200
             ACLASS~STEPI; WHOLE~ELBAT[I];SCATTERELBAT;             %W4007094500
IF SCLASS=LISTID THEN IF ACLASS!LISTID THEN                     %93 %W3307094700
    BEGIN WRL(15); LISTPARA; GO TO COMMON END;                  %93 %W3307094800
IF NOT (INLINETAG AND SBIT AND T1=RTBRKET) THEN                     %W7507096500
IF NOTETOG THEN FLAG(530&"H"[1:43:2]);%NAME-CALL EXPR           %91 %W7407132500
IF NOTETOG THEN FLAG(530&"H"[1:43:2]);%NAME-CALL EXPR           %91 %W7407203500
THEN BEGIN  IF NOT (INLINETAG AND SBIT AND ELCLASS=RTBRKET) THEN    %W7507239000
                ERROR(129);  GO EXIT  END;                          %W7507239100
IF NOTETOG THEN FLAG(531&"H"[1:43:2]); %SIMPLE SWITCH PARAM     %91 %W7407270500
NO519~TRUE; % JUST FLAGGED IT                                   %91 %W7407270900
NO519~FALSE; %ALLOW AGAIN                                       %91 %W7407271100
IF NOTETOG THEN BEGIN FLAG(534&"H"[1:43:2]); NO519~TRUE END;    %91 %W7407373500
NO519~FALSE;                                                    %91 %W7407374500
   COMMENT   BOOLEAN CONSTANT;                                      %W-307376000
             IF T2 ~ 0 & WHOLE[44:25:2] > 7 THEN EMITNUM(C)         %W-307377000
             ELSE EMIT(T2); ACLASS ~ BOOID; GO TO BSX;              %W-307377500
                BEGIN                               %ELIMINATE  %74 %W4807402000
                    HOLE ~ 2 - REAL(FROM);          %NAME CALL  %74 %W4807402100
                    VARIABLE(HOLE);                 %EXPRESSION %74 %W4807402200
                    GO TO EXIT                      %PRT CELL   %74 %W4807402300
                END;                                %ACC ENTRY  %74 %W4807402400
    IF NOT LOCAL(ELBW~ELBAT[I]) THEN BEGIN    IF NOTETOG THEN   %91 %W7407508000
        IF NOT BOOLEAN(ELBW.FORMAL) THEN                        %91 %W7407508200
    %THESE CASES ARE CLEAR                                      %91 %W7407508250
        BEGIN FLAG((532+REAL(TB1))&"H"[1:43:2]); NO519~TRUE END;%91 %W7407508500
                GO GOMCP END;                                   %91 %W7407508750
              IF TB1 THEN BEGIN GOGEN(ELBW,BFW); STEPIT;        %91 %W7407509000
                                                                %91 %W7407511000
                ELSE BEGIN CALLSWITCH(ELBW); EMITO(BFW);        %91 %W7407526000
IF NOTETOG THEN FLAG(537&"H"[1:43:2]) END; %MORE THAN ONCE (SW) %91 %W7407526500
NO519~FALSE; % ALLOW NOTE 519                                   %91 %W7407528500
                PHONEYDEC;                                          %W4807538100
DEFINE ELBATWORD=RR9#, LINK=GT2#,ADDITIONAL                         %W4307595000
    DO BEGIN ADJUST;                                                %W4307596700
    IF XREF THEN BACKUPTO(XREFC-1, 0);                              %W4307596800
    IF INDEX THEN WRITEPRT(ELBAT[I].[35:13], 1, 6);                 %W4307596900
        IF STEPI!COLON THEN                                         %W4307597000
        LINK~(ADDITIONAL~TAKE(GT3~GIT(ELBATWORD)))                  %W4307601000
        PUT(-ADDITIONAL&L[36:36:12],GT3);                           %W4307610000
    PAIR(TRUE);%BEGIN                                               %W7907646445
    PAIR(FALSE);%END                                                %W7907646587
$ VOID 07674001                                                     %W-307661000
            LABEL L2,L3,L4;                                         %W-307675000
            BEGIN IF STEPI=LITNO OR ELCLASS=NONLITNO THEN GO L2;    %W-307689000
                IF ELCLASS = STRNGCON THEN                          %W-307690000
                  IF COUNT=8 THEN BEGIN MOVECHARACTERS(8,ACCUM[1],3,%W-307690500
                        EDOC[T2],0); GO TO L3 END ELSE GO TO L2;    %W-307691000
                IF ELCLASS = ADOP THEN BEGIN                        %W-307691500
                    IF STEPI=LITNO OR ELCLASS=NONLITNO THEN BEGIN   %W-307692000
                        C ~ C&ELBAT[I-1][1:21:1]; GO TO L2 END      %W-307692500
                    END ELSE                                        %W-307693000
                IF ELCLASS{IDMAX THEN BEGIN                         %W-307693500
                    IF ACCUM[1].[18:18] ="OCT" THEN BEGIN           %W-307694000
                        ACCUM[1].[18:18]~0;                         %W-307694500
                        OCTCON(FALSE,ACCUM[0]); GO TO L4 END        %W-307695000
                    END ELSE                                        %W-307695500
                IF ELCLASS=TRUTHV THEN BEGIN                        %W-307696000
                    IF ELBAT[I].ADDRESS>1 THEN BEGIN                %W-307697000
            L4:         MOVE(1,ACCUM[0],EDOC[T2]); GO TO L3 END END;%W-307698000
                I ~ I-1; C ~ 0; COMMENT BLANK FIELD IS ZERO;        %W-307699000
                    WRL(3);                                         %W9307699500
            L2: MOVE(1,C,EDOC[T2]);                                 %W-307700000
            L3: C ~ 0;                                              %W-307701000
DEFINE LOGLVL = 3#;  LOGIN;                                         %W8207727005
                                                                        07727006
                                                                        07727007
LDEC:                                                               %W7507731000
            IF INLINETAG THEN                                       %W7507731100
            IF ELBAT[I].ADDRESS = STREAMV THEN                      %W7507731200
                BEGIN INLINE; GO TO EXIT END;                       %W7507731400
            FLAG(146);                                              %W7507731600
LBEG:   PAIR(TRUE); IF STEPI = DECLARATORS % BEGIN                  %W7907754000
                        THEN BEGIN                                  %W7507755000
                            IF INLINETAG THEN                       %W7507755025
                            IF ELBAT[I].ADDRESS = STREAMV THEN      %W7507755050
                                IF STEPI = LEFTPAREN THEN           %W7507755100
                                    BEGIN                           %W7507755150
                                        I ~ I - 1;                  %W7507755200
                                        COMPOUNDTAIL;               %W7507755250
                                        GO TO EXIT                  %W7507755300
                                    END                             %W7507755350
                                  ELSE                              %W7507755400
                                        I ~ I - 1;                  %W7507755450
                            I ~ I - 1;                              %W7507755500
                            BLOCK ( FALSE )                         %W7507755550
                        END                                         %W7507755600
 % CATCH FOR INLINE STREAM STATEMENTS ( FIRST AFTER BEGIN)          %W7507755650
EXIT: LOGOUT END STMT;                                              %W8207757000
    IF Q="3SET00" AND ERROROPTION THEN                              %W1207810202
        BEGIN                                                       %W1207810204
            WRL(9);                                             %93 %W1207810205
            STEPIT;                                                 %W1207810206
            IF Q="7EXPOV" THEN J~21 ELSE IF Q="7INTOV" THEN J~ 22   %W1207810208
            ELSE IF Q="8EXPUN" THEN J~ 23 ELSE IF Q="7DIVZE" THEN   %W1207810210
            J~ 24 ELSE BEGIN ERR(310); GO XXX END;                  %W1207810212
IF STEPI!RELOP THEN BEGIN EMITL(0);EMITL(J);EMITO(STD);GO XXX END;  %W1207810214
        IF STEPI!FILEID AND(ELCLASS<BOOPROCID OR ELCLASS>INTID)THEN %W1207810218
            BEGIN ERROR(311); GO TO XXX END;                        %W1207810220
        IF N~ ELBAT[I].ADDRESS>511 AND N<1024 THEN                  %W1207810222
            BEGIN ERROR(312); GO TO XXX END;                        %W1207810224
           N~ N.[38:10];                                            %W1207810226
        IF ELCLASS<BOOID AND ELCLASS!FILEID THEN                    %W1207810228
           IF INFO[ELBAT[I].LINKR, ELBAT[I].LINKC+ELBAT[I].INCR]    %W1207810230
            .[40:8]!0 THEN BEGIN ERROR(313); GO TO XXX END;         %W1207810232
        EMITL(N); EMITL(J); EMITO(STD); STEPIT; GO TO XXX           %W1207810234
    END;                                                            %W1207810236
IF Q="6RESIZ" THEN                                                  %W9007821300
    BEGIN INTEGER T1;                                               %W9007821310
            WRL(23);                                            %93 %W9007821315
    IF STEPI!LEFTPAREN THEN FLAG(105);                              %W9007821320
    IF STEPI<BOOARRAYID OR ELCLASS>INTARRAYID                       %W9007821330
        THEN ERR(123);                                              %W9007821340
    T1~FL;    VARIABLE(T1);                                         %W9007821350
    IF TABLE(I-2)!FACTOP OR T1=0 THEN ERR(123);                     %W9007821360
    IF ELCLASS!COMMA THEN ERR(128);                                 %W9007821370
    STEPIT; AEXP; EMITPAIR(JUNK,ISN);                               %W9007821380
    EMITL(41); EMITO(COM);                                          %W9007821390
    EMITO(DEL);EMITO(DEL);                                          %W9007821400
    IF ELCLASS!RTPAREN THEN ERR(103) ELSE STEPIT;                   %W9007821410
    GO TO XXX;                                                      %W9007821420
    END ELSE                                                        %W9007821430
IF Q="5TIMED" THEN BEGIN WRL(24);N~1;C~42 END ELSE %PSUEDO-LOG  %93 %W9007860000
COMMENT FORSTLIST COMPILES THE CONSTRUCT FOR <LIST> DO <STATEMENT>; %W3307915000
PROCEDURE FORSTLIST(J);  VALUE J;  INTEGER J;                       %W3307915010
BEGIN                                                               %W3307915020
    INTEGER LISTADDRESS,JUMPPLACE,LL,NAME;                          %W3307915030
    REAL LINFO,TEMP,VOBIT;                                          %W3307915040
    CHECKER(ELBAT[I]);                                              %W3307915050
    COMMENT J=0 IF FORSTLIST CALLED BY FORSTMT                      %W3307915060
            J!0 IF FORSTLIST CALLED BY FORSTLIST;                   %W3307915070
    IF J=0 THEN                                                     %W3307915080
    BEGIN                                                           %W3307915090
        EMITL(0);  ADJUST;  JUMPPLACE ~ L;                          %W3307915100
    END ELSE JUMPPLACE ~ J;                                         %W3307915110
    EMITPAIR(LSTRTN,SND);                                           %W3307915120
    LISTADDRESS ~ ELBAT[I].ADDRESS;                                 %W3307915130
    COMMENT SET UP THE LIST AS IF IT WERE A REAL VARIABLE;          %W3307915140
    TEMP ~ TAKE(LINFO ~ ELBAT[I].LINK);                             %W3307915150
IF MODE = 0 THEN FLAG(510) ELSE                                 %73 %W3307915155
    NAME ~ GETSPACE(FALSE);                                         %W3307915160
    VOBIT ~ IF BOOLEAN(TEMP.FORMAL) THEN TEMP ELSE TEMP & 2[9:46:2];%W3307915170
    PUT(VOBIT & LEVEL[11:43:5] & REALID[2:41:7] & NAME[16:37:11],   %W3307915180
                    LINFO);                                         %W3307915190
    EMITN(LISTADDRESS);  EMITPAIR(NAME,STD);                        %W3307915200
    EMITV(LSTRTN);  EMITPAIR(0,GEQ);  LL ~ BUMPL;                   %W3307915210
    IF STEPI = COMMA THEN                                           %W3307915220
        IF STEPI=LISTID THEN BEGIN WRL(-1);                     %93 %W3307915230
            FORSTLIST(JUMPPLACE) END ELSE ERR(475)              %93 %W3307915235
    ELSE BEGIN                                                      %W3307915240
        IF ELCLASS!DOV THEN ERR(475) ELSE STEPIT;  EMITO(DEL);      %W3307915250
        COMMENT ERROR 475 INDICATES MISSING DO OR NON LIST ELEMENT; %W3307915260
        DIALA ~ DIALB ~ 0;                                          %W3307915270
        STMT;                                                       %W3307915280
        EMITL(5); EMITB(BBW,BUMPL,JUMPPLACE);                       %W3307915290
        CONSTANTCLEAN;  ADJUST;                                     %W3307915300
        EMITO(DEL);  EMITL(5);                                      %W3307915310
        EMITPAIR(LSTRTN,STD);                                       %W3307915320
    END;                                                            %W3307915330
    EMITB(BFC,LL,L-4);                                              %W3307915340
    COMMENT SET THE LIST BACK TO BE A LIST (NOW REAL);              %W3307915350
    PUT(TAKE(LINFO)& TEMP[1:1:34],LINFO);                           %W3307915360
    DIALA ~ DIALB ~ 0;                                              %W3307915370
END FORSTLIST;                                                      %W3307915380
             LABEL EXIT,XIT;                                        %W3308017000
              IF ELCLASS = LISTID AND NOT LISTMODE THEN             %W3308175500
    BEGIN WRL(16); FORSTLIST(0); GO TO XIT END;                 %93 %W3308175700
  EXIT: K~ 0;   XIT: END FORSTMT;                                   %W3308232000
NO519~TRUE; % NOT FOR ACTION LABELS                             %91 %W7408242500
NO519~FALSE; % ALLOW AGAIN                                      %91 %W7408245500
PROCEDURE LISTPARA;                                                 %W3308289100
BEGIN                                                               %W3308289200
    INTEGER LISTADDRESS;                                            %W3308289300
    IF ELCLASS=LFTBRKET THEN                                        %W3308289400
        BEGIN                                                       %W3308289500
            WRL(25);                                                %W9308289550
            STEPIT; RR1 ~ LISTGEN;                                  %W3308289600
            IF ELCLASS ! RTBRKET THEN ERR(158) ELSE STEPIT          %W3308289700
        END                                                         %W3308289800
    ELSE IF ELCLASS ! LISTID AND ELCLASS ! SUPERLISTID              %W3308289810
        THEN RR1 ~ LISTGEN                                          %W3308289820
        ELSE BEGIN                                                  %W3308289830
                CHECKER(ELBAT[I]);                                  %W3308289840
                LISTADDRESS  ~ ELBAT[I].ADDRESS;                    %W3308289850
                IF ELCLASS = SUPERLISTID THEN                       %W3308289860
                BEGIN COMMENT SUBSCRIPTED SWITCH LIST ID;           %W3308289870
                    BANA;EMITN(LISTADDRESS);              %B8-6 %67 %W3308289880
                    EMITO(LOD);                           %B8-6 %67 %W3308289884
                    EMITO(LOD);                                     %W3308289886
                END ELSE                                            %W3308289890
                BEGIN COMMENT A COMMON LIST ID;                     %W3308289900
                    EMITPAIR(LISTADDRESS,LOD);  STEPIT;             %W3308289910
                END                                                 %W3308289920
             END                                                    %W3308289930
END LISTPARA;                                                       %W3308289940
LABEL EXPFORMR;                                                     %W4008385550
              IF EXPLICITFORMAT THEN GO TO EXPFORMR;                %W4008430500
EXPFORMR:                                                           %W4008441500
    STEPIT; LISTPARA;                                               %W3308479000
$ VOID 08489571                                                     %W3308480000
LABEL EXPFORMW;                                                     %W4008591150
    CHKSECOND:  IF EXPLICITFORMAT THEN GO TO EXPFORMW ELSE STEPIT;  %W4008643000
EXPFORMW:                                                           %W4008646500
                                                                    %W3308687000
    STEPIT;  LISTPARA;                                              %W3308688000
$ VOID 08696541                                                     %W3308689000
NO519~TRUE; % NOT FOR ACTION LABELS                             %91 %W7408698150
NO519~FALSE; % ALLOW AGAIN                                      %91 %W7408698450
STREAM PROCEDURE INFOSET(OIV,NIV,I); VALUE OIV, NIV;            %86 %W-509006100
    BEGIN DI~I; DI~DI+1; SI~LOC OIV;                            %86 %W-509006200
        IF 8 SC=DC THEN %CHECK NOTHING MOVED,THEN WRITE IN INFO %86 %W-509006300
            BEGIN DI~DI-8; DS~ 8 CHR END                        %86 %W-509006400
    END INFOSET;                                                %86 %W-509006500
STREAM PROCEDURE WRTINTRSC(SGNO,ALFA,PRT,FIL,TOG);                  %W-509010000
    VALUE SGNO,PRT,TOG;                                             %W-509011000
            LABEL X, Y;                                             %W-509012500
        DS~ T1 CHR; DI~ FIL; DI~ DI+19;                             %W-509018000
TOG(DS~ 19 LIT" IS SEGMENT NUMBER "; JUMP OUT 1 TO X); DI~ DI+19;   %W-509019000
X:                                                                  %W-509019500
    TOG(DS~ 17 LIT", PRT ADDRESS IS "; JUMP OUT 1 TO Y); DI~ DI+17; %W-509021000
Y:                                                                  %W-509021500
DEFINE LOGLVL = 1#;  LOGIN;                                         %W8209025500
            PATCHTOG~ FALSE;                                        %W5109027400
            STREAMTAG ~ FALSE;                                      %W3209027500
            DOUBLETOG ~ FALSE;                                      %W3709027600
LISTOG~BOOLEAN(3);COMMENT KILL B USE OF COMMON                  %93 %W9909027999
COMMENT KILL BURROUGHS USE OF COMMON                                %W9909028099
    FILL SOURCE[*] WITH "**","**","**","SC","SC","SC","**","IT","RT"%W2009034100
      ,"ST","**","**","SC","**","**","**","IL","RL","SL","**","**", %W2009034200
    "**","**","**";                                             %77 %W2009034300
  OCT1340010450243571,  COMMENT  <;   OCT1260000000000000, COMMENT~;%W-309205000
% MAKE CREATION TIME AND DATE INTRINSIC DEFINES AT LEVEL ZERO   %86 %W-509214250
% EXPECT THESE IN LASTSEQUENCE -11 THRU -2                      %86 %W-509214275
    "0H000600","00?TIMEC","OMPILED ",                      %551 %86 %W-509214300
               OCT7700141460633144, OCT2577120000000000,%?? TIME%86 %W-509214325
    "0H000600","00?DATEC","OMPILED ",                      %556 %86 %W-509214350
               OCT7700141460242163, OCT2577120000000000,%?? DATE%86 %W-509214375
    "        "; % FOR SEQ # OF LAST ERROR                  %561 %86 %W-509214400
%   COMMENT LASTSEQROW, LASTSEQUENCE IS HERE;              %562 %86 %W-509214450
FOR NEXTINFO ~ 512 STEP 3 UNTIL 536, 551, 556 DO                %86 %W-509214505
        PUT( (TAKE(NEXTINFO))&                                  %86 %W-509214510
         OCT0147,            COMMENT JFW A       02  ;              %W2109218000
                                                                        09251100
COMMENT:  THIS CALL TAKES THE CURRENT TIME FROM "TIME(9)" IN THE    %Y9609251110
    FORM "09:30:45" AND PLACES IT INTO INFO TO BE THE VALUE OF      %Y9609251120
    "TIMECOMPILED" WHENEVER THE PROGRAM BEING COMPILED IS USED ---  %Y9609251130
INFOSET("?? TIME",TIME(9),INFO[LASTSEQROW,LASTSEQUENCE-8]);%FIX %86 %W-509251200
                                                                        09251300
COMMENT:  THIS CALL TAKES THE CURRENT DATE FROM "TIME(8)" IN THE    %Y9609251310
    FORM "01/11/69" TO BE THE VALUE OF "DATECOMPILED" ---           %Y9609251320
INFOSET("?? DATE",TIME(8),INFO[LASTSEQROW,LASTSEQUENCE-3]);%THEM%86 %W-509251400
        DO UNTIL STEPI = BEGINV OR ELCLASS = ENDV; %NO BEGIN-EOF%59 %W5709252000
        IF ELCLASS = ENDV THEN FLAG(170); %MISSING 1ST BEGIN-EOF%59 %W5709252010
        ADDVALUE ~ 1000; % DEFAULT VALUE FOR "SEQ" OPTION       %87 %W-109271400
    PAIR(TRUE); %BEGIN                                              %W7909271500
    SEGTOG~ TRUE;                                                   %W3109274500
    SEGTOG~ FALSE;                                                  %W3109275050
NOSYN                                                               %W6109275499
NOSYN                                                               %W6109275954
        IF XENDI>0 THEN WRITE(DATA, 30, PRTABLE[*]);                %W4309275955
    IF PURGETOG AND STOPTOG THEN CLOSE(CARD, RELEASE);              %W1309276200
    IF PURGETOG AND(ERRORCOUNT=0 OR STOPTOG)THEN CLOSE(DISK,PURGE)  %W1309276400
        ELSE CLOSE(DISK);                                           %W1309276401
IF ERRORCOUNT=0 OR STOPTOG THEN LOCK(NEWDISK, SAVE);                %W1709276405
         IF PURGETOG THEN CLOSE(TAPE,PURGE)ELSE LOCK(TAPE,RELEASE); %W1309276500
        LISTOG ~ TIMEITTOG;                                         %W-209276600
GT4~ 1;                                                             %W-509291500
    WRITELINE; % PRINT-FILL                                         %W7909300150
        B2D(GT2.[38:10]), LIN [0], GT4);                            %W-509301000
% DELAY PRINT--VALUE OF IMAGE IRRELAVANT IN WRAP UP                 %W7909302000
; GT4~0;                                                            %W-509302500
    BEGIN IFCODE MOVE(2,FROM[I DIV 128,I MOD 128],CODE(J));         %W6109328000
        IFCODE                                                      %W6109329500
    GTI1~ IF NOOFARRAYS>10 THEN 5000 ELSE NOOFARRAYS|500;          %%W2209361040
$ VOID 09361061                                                    %%W2209361050
     GTI1~ GTI1+(IF NEWINX>2000 THEN 1000 ELSE NEWINX/2);          %%W2209361080
                                                                   %%W2209361100
    IF GTI1>24000 THEN GTI1~ 24000;                                %%W2209361200
     IDARRAY[1]:=SGAVL;                                                 09400000
    IF  ERROROPTION THEN IDARRAY[8]~1&4[18:33:15]ELSE IDARRAY[8]~0; %W1209405500
        IFCODE BEGIN                                                %W6109406500
FOR GT1~9 STEP 1 UNTIL 29 DO IDARRAY[GT1]~0;%ZERO SEG 0         %69 %W-009406600
                                                                    %W6109407050
        LOCK(CODE,SAVE)  END; LOCK(NEWTAPE,SAVE);                   %W6109407100
    %ELIMINATE PASSING CORE ESTIMATE TO MCP--OBSOLETE           %87 %W2209407200
IF PURGETOG AND NOT STOPTOG AND ERRORCOUNT=0 THEN CLOSE(CARD,PURGE);%W1309407205
COMMENT SEE 05046050--THIS IS PART2;                                %W5809407501
IF ERRORCOUNT > TWXCOUNT THEN                                       %W5809407502
    IF STATUS(STATARRAY[*]) > 0 THEN                                %W5809407504
    BEGIN LABEL DONE;                                               %W5809407506
    STREAM PROCEDURE TWXOUT(D,N); VALUE N;                          %W5809407508
    BEGIN SI ~ LOC N; DI ~ LOC N; DS ~ 2 DEC; DI ~ D;               %W5809407510
      DS ~ 2 LIT "{!";SI ~ LOC N;                                   %W5809407512
      IF SC ! "0" THEN DS ~ CHR ELSE SI~ SI + 1; DS ~ CHR;          %W5809407514
      DS ~ 36 LIT " MORE ERRORS NOT LISTED ON REMOTE{!~" END;       %W5809407516
    TWXOUT(STATARRAY[1], ERRORCOUNT-TWXCOUNT);                      %W5809407518
    WRITE(TWX(STATARRAY[0],0),8,STATARRAY[*])[DONE]; % DONT WAIT    %W5809407520
    DONE: END;% END PART2 W58                                       %W5809407522
IF FILESTOG THEN                                                %85 %W-509407600
BEGIN                                                           %85 %W-509407610
    FORMAT  FILEHD1  (X6,"FILE NAME INFORMATION:"),             %85 %W-509407620
            FILEHD2  (X6,"INTERNAL  EXTERNAL");                 %85 %W-509407630
    PROCEDURE PRINT(F,V); VALUE V;                              %85 %W-509407670
        FILE F;                                                 %85 %W-509407680
        REAL V;                                                 %85 %W-509407690
        BEGIN                                                   %85 %W-509407700
     FORMAT FILELN   (X6,A1,A6," = ",A1,A6,"/",A1,A6);          %85 %W-509407701
    DEFINE  FILENM = STATARRAY[1].[6:6], STATARRAY[1],          %85 %W-509407702
                     STATARRAY[2].[6:6], STATARRAY[2] #;        %85 %W-509407703
            SEARCH(F, STATARRAY[*]);                            %85 %W-509407710
            WRITELINE;                                          %85 %W-509407720
            WRITE(LIN[*], FILELN, V.[6:6], V, FILENM);          %85 %W-509407730
        END;                                                    %85 %W-509407740
    IF LISTOG THEN BEGIN WRITE(LINE,15,LIN[*]); BLANK(LIN) END  %85 %W-509407750
        ELSE BEGIN TIMEIT(LIN); LISTOG~TRUE END;                %85 %W-509407760
    WRITELINE; WRITE(LIN[*],FILEHD1);                           %85 %W-509407770
    WRITELINE; WRITE(LIN[*],FILEHD2);                           %85 %W-509407780
    IF SAVETIME}0 THEN PRINT(CODE,"CODE   ");                   %85 %W-509407790
    PRINT(CARD,"CARD   ");                                      %85 %W-509407800
    IF MERGETOG THEN PRINT(MERGE,"MERGE  ");                    %85 %W-509407810
    IF MAXTLCR!0 THEN                                           %85 %W-509407820
        IF DISKTOG THEN PRINT(DISK,"DISK   ")                   %85 %W-509407830
            ELSE PRINT(TAPE,"TAPE   ");                         %85 %W-509407840
    IF NEWDSK THEN PRINT(NEWDISK, "NEWDISK");                   %85 %W-509407850
    IF NEWTOG THEN PRINT(NEWTAPE, "NEWTAPE");                   %85 %W-509407860
END FILESTOG;                                                   %85 %W-509407870
IF LISTOG THEN % DROPS OR NOT NOHEADING                             %W9909408000
COMMENT 09409000,09410000,09411000 ARE PATCHES BUT;%W-5 IN FORMAT   %W-509408900
        BEGIN FORMAT PAN("NUMBER OF ERRORS DETECTED =",I4,X26        ,  09409000
    "LAST CARD REFERENCED HAS SEQ # ", 2A4 ), PON                    (  09410000
                "PRT SIZE =",I4,"; TOTAL PGM SEGM SIZE =",I6         ,  09411000
I4), PUN("ESTIMATED CORE STORAGE REQUIREMENT =",I6," WORDS.");      %W7909413000
            FORMAT FL ("FILE "A4" CONTAINS"I5" LINK RECORDS.");     %W5709414000
            FILL FNAME[*] WITH "LIBR","CARD","TAPE","DISK","MRGE";  %W5709414500
    BEGIN LIST LL( ERRORCOUNT, GT1, GT2 ), LK( PRTIMAX, AKKUM,      %W7909417000
        IF DA{CHUNK THEN DA ELSE ((DA+CHUNK-1)DIV CHUNK)|CHUNK,     %W7909418000
        SGAVL-1), LM( GTI1); % BREAK UP LL                          %W7909419000
WRITE(LINE,15,LIN[*]); BLANK(LIN);                              %85 %W-509419050
WRITELINE; WRITE(LIN[*], PAN, LL); % PRINT-FILL                     %W7909419100
WRITELINE; WRITE(LIN[*], PON, LK);                                  %W7909419200
WRITELINE; WRITE(LIN[*], PUN, LM) END;                              %W7909419250
            FOR I ~ 1 STEP 1 UNTIL 4 DO                         %77 %W5709419300
                IF LINKCT[I]!0 THEN                                 %W5709419400
                BEGIN                                               %W5709419500
        WRITELINE; % PRINT-FILL                                     %W7909419550
                    WRITE(LIN[*]   ,FL,FNAME[I],LINKCT[I]);         %W5709419600
WRITE(LINE,15,LIN[*]); BLANK(LIN);                              %85 %W-509419650
% DELAY PRINT                                                       %W7909419700
                END;                                                %W5709419800
END END;LOGOUT END PROGRAM;                                         %W8209420000
BOOLEAN PROCEDURE EXPLICITFORMAT;                                   %W4009500000
    BEGIN INTEGER PRT;                                          %65 %W4009500010
    ARRAY TEDOC[0:7,0:127];                                         %W4009500015
        RESULT~7; SCANNER;                                      %65 %W4009500020
                                                                %65 %W4009500030
    IF EXAMIN(NCR) = "<" THEN                                   %65 %W4009500040
  BEGIN                                                             %W4009500050
    STOPDEFINE ~ TRUE;  STEPIT;  EXPLICITFORMAT ~ TRUE;             %W4009500100
        WRL(18);                                                %93 %W4009500200
    MOVECODE(TEDOC,EDOC);                                           %W4009500300
    GT5~SGNO;GT1~(2|SGAVL-1)&2[4:46:2];SGNO~SGAVL; %NEW SEGMENT     %W4009500400
    F~0; PRT~GETSPACE(TRUE);Z~PROGDESCBLDR(LDES,0,PRT);             %W4009500500
    ELCLASS ~ "<";  TB1 ~ FORMATPHRASE; SEGMENT(-F,SGNO,GT5);       %W4009500600
    SGAVL ~ SGAVL+1;  SGNO ~ GT5;  MOVECODE(TEDOC,EDOC);            %W4009500700
    IF LASTELCLASS ! ">" THEN ERR(136);                             %W4009500800
    ELBAT[I].CLASS ~ IF ELCLASS = "," THEN COMMA                    %W4009500900
                     ELSE IF ELCLASS = ")" THEN RTPAREN ELSE 0;     %W4009501000
    I ~ I-1;                                                        %W4009501100
    EMITL(0); EMITPAIR(PRT,LOD);                                    %W4009501200
  END                                                               %W4009501250
END EXPLICITFORMAT;                                                 %W4009501300
                PHONEYDEC;                                          %W4810070000
                    WRITELINE;   %PRINT FILL                        %W7910073500
                    IMAGE ~ FALSE END;                              %W7910075000
        ELSE BEGIN REPEAT ~ REAL(ELCLASS ! "(" AND ELCLASS ! "<" ); %W4010113000
        IF ELCLASS = "(" OR ELCLASS = "<"                           %W4010114000
        IF ELCLASS ! ")" AND ELCLASS ! ">" THEN GO TO EL;           %W4010124000
    ELSE IF ELCLASS = ")" OR ELCLASS = ">" THEN GO TO EXIT          %W4010134000
STOPDOLLAR ~ TRUE;                                                  %W7610136500
STOPDOLLAR ~ FALSE;                                                 %W7610143500
                    %ELIMINATE BLANKS                               %W6510167400
    BEGIN FLAG(524); GO TO FINAL END ELSE GO PACKIN             %83 %B1010264420
INTEGER TEMPCELL, LISTPTR;                                          %W3310286500
BOOLEAN LISTLVFLAG;                                                 %W3310286600
            INTEGER LISTADDRESS;                                    %W3310290500
             LISTLVFLAG ~ LISTLVFLAG AND (ELCLASS = LISTID          %W3310293200
                    OR ELCLASS = SUPERLISTID);                      %W3310293400
             IF ELCLASS = FORV THEN BEGIN COMMENT FORCLAUSE;        %W3310294000
                    FORSTMT;  LISTLVFLAG ~ FALSE END                %W3310294200
             ELSE IF ELCLASS=LISTID OR ELCLASS=SUPERLISTID THEN     %W3310298100
                BEGIN                                               %W3310298120
        WRL( IF ELCLASS=LISTID THEN 17 ELSE 27 );                   %W9310298140
                    IF LISTLVFLAG THEN L ~ L-3;                     %W3310298160
                    CHECKER(ELBAT[I]); LISTADDRESS~ELBAT[I].ADDRESS;%W3310298200
                    EMITL(0);EMITPAIR(LSTRTN,STD);                  %W3310298240
                    EMITNUM(L+3-LSTR+REAL(LISTPTR>1023));           %W3310298260
                    EMITPAIR(LISTPTR,STD);                          %W3310298280
                    IF ELCLASS=SUPERLISTID THEN                     %W3310298300
BEGIN BANA; EMITN(LISTADDRESS);                                 %72 %W3310298400
EMITO(LOD);                                                     %72 %W3310298430
                    EMITO(LOD); EMITO(XCH); EMITO(CDC)   END        %W3310298480
                    ELSE BEGIN EMITN(LISTADDRESS);STEPIT END;       %W3310298500
                    EMITV(LSTRTN);  EMITL(0);  EMITO(GEQ);          %W3310298600
                    EMITL(1);  EMITO(BFC);  EMITO(RTS);             %W3310298700
                    EMITO(DEL);  EMITL(5); EMITPAIR(LSTRTN,STD);    %W3310298850
                    LISTLVFLAG ~ TRUE;                              %W3310298870
                END                                                 %W3310298900
              ELSE IF GT1 ~ TABLE(I+1) = RTPAREN AND L=LSTR         %W3310298910
                    AND ELBAT[I].ADDRESS { 1023                     %W3310298915
                    AND ELCLASS}BOOID AND ELCLASS{INTID THEN        %W3310298920
                    COMMENT LIST CONSISTS OF A SINGLE SIMPLE VAR.;  %W3310298930
                    BEGIN L ~ L-4-REAL(LISTPTR>1023);               %W3310298940
                          CHECKER(ELBAT[I]);                        %W3310298945
                          EMITN(ELBAT[I].ADDRESS); EMITO(RTS);      %W3310298950
                          STEPIT;                                   %W3310298960
                    END                                             %W3310298970
               EMITPAIR(LISTPTR,STD);  COMMENT PREPARE LISTPTR FOR  %W3310302000
                IF (GT1 = COMMA                                     %W3310304000
STORE:         EMITPAIR(TEMPCELL,STD);  EMITN(TEMPCELL) END;        %W3310328000
             COMMENT TEST TO INSURE LISTGEN NOT CALLED RECURSIVELY; %W3310346200
             IF LISTMODE THEN ERR(476) ELSE                         %W3310346400
             BEGIN                                                  %W3310346600
             TEMPCELL ~ GETSPACE(FALSE);  LISTPTR~GETSPACE(FALSE);  %W3310348500
             EMITV(LSTRTN); EMITO(BFW);                             %W3310349400
            EMITL(5);  EMITPAIR(LSTRTN,STD);                        %W3310349600
            EMITPAIR(REAL(LISTPTR>1023)+2,BFW);                     %W3310349800
             EMITV(LISTPTR); EMITO(BFW);  LSTR ~ L;                 %W3310350000
              IF LISTLVFLAG THEN BEGIN L~L-3; LISTLVFLAG~FALSE END; %W3310358500
             END                                                    %W3310368000
NO519~TRUE; %WE ARE FLAGING SWITCHES IN BLOCK                   %91 %W7410957500
EXIT:   NO519~FALSE END SWITCHGEN; %ALLOW NOTE 519              %91 %W7412001000
    IF EL!RTPAREN AND EL!RTBRKET OR J!P  THEN FLAG(128);        %83 %B1012126000
        END ELSE FLAG(499);                                     %77 %W-013088110
              IF (IOTEMP~GET(L-2)).[46:2] =0 THEN CURRENT~IOTEMP    %W5513113000
IF (IOTEMP~GET(L-1)).[46:2]=0 THEN IOTEMP~IOTEMP DIV 4          %69 %W5513148200
      IF (IOTEMP~GET(L-1)).[46:2]=0 THEN IOTEMP~IOTEMP DIV 4        %W5513164010
                                    ELSE IOTEMP ~ 256;              %W5513164020
                IF NOT INLINETOG THEN                               %W7513307500
                    BEGIN                                           %W7513307510
                    END;                                            %W7513308500
                PHONEYDEC;                                          %W4813323100
    IF INDEX THEN                                                   %W4313349400
    WRITEPRT(LASTINFO, NEXTINFO-LASTINFO, 3&REAL(P2)[1:47:1]&REAL(P3%W4313349500
      )[2:47:1]);                                                   %W4313349501
IF NOTETOG THEN FLAG(539&"H"[1:43:2]); %NON-ZERO LOWER BOUND    %91 %W7413485500
IF NOTETOG THEN FLAG(539&"H"[1:43:2]); % NON-ZERO LOWER BOUND   %91 %W7413498500
    IF INDEX THEN WRITEPRT(SAVEINFO2-1, 1, 7);                      %W4313556500
                IF T3~ELBAT[I-1].ADDRESS ! 0 THEN               %91 %W7413575000
IF NOTETOG THEN FLAG(540&"H"[1:43:2]);%NON-ZERO BOUND SPEC      %91 %W7413575500
IF NOTETOG THEN FLAG(540&"H"[1:43:2]); % VARIABLE BOUND SPEC    %91 %W7413583500
                PHONEYDEC;                                          %W4813592100
    BEGIN LIST LL(SGAVL); IF LISTOG THEN BEGIN WRITELINE;           %W7913633000
        IMAGE ~ FALSE;                                              %W7913633050
        WRITE( LIN[*], PRINTSEGNO, LL) END                          %W7913633100
    END;                                                            %W7913633200
IF SEGTOG THEN                                                      %W3113683500
    BEGIN LIST LL (NO,SIZE,NOO); IF LISTOG THEN BEGIN           %79 %W3713684000
    IMAGE ~ FALSE;                                                  %W7913684050
    WRITELINE; WRITE(LIN[*],PRINTSIZE,LL) END END; %PRINT-FILL  %79 %W3713684100
    BEGIN IFCODE MOVE2(EDOC[CNTR.[38:3],CNTR.[41:7]],CODE(J));      %W6113692000
        IFCODE                                                      %W6113692250
BEGIN IFCODE MOVE2(ENIL[CNTR.[38:3],CNTR.[41:7]],CODE(J));          %W6113695400
IFCODE                                                              %W6113695525
                PHONEYDEC;                                          %W4813717100
        BT ~ (K~K+NCII)>REAL(FUNCTOG);                              %W2313764000
        IF BT THEN EMIT(0);                                         %W2313767900
   EMITB(IF BT THEN BBC ELSE BBW,L,IF RELAD=4095 THEN 0 ELSE RELAD);%W2313769000
IF NOT SPECTOG AND ELBAT[I].LVL=LEVEL THEN                          %W-113819350
IF LEVEL = 1 THEN BEGIN WRL(2); IF NOT WARNTOG THEN FLAG(-46) END   %W9313819375
            ELSE FLAG(46); %ALLOW DYNAMIC ARRAY AT OUTERMOST LEVEL  %W9313819400
PROCEDURE INLINE;                                                   %W7513948000
 BEGIN                                                              %W7513949000
                                                                        13949999
                                                                        13950000
                                                                        13950500
                                                                        13950600
                                                                        13951000
                                                                        13951500
                                                                        13952000
                                                                        13953000
                                                                        13954000
                                                                        13955000
                                                                        13955500
                                                                        13956000
                                                                        13957000
                                                                        13958000
                                                                        13958050
                                                                        13958100
                                                                        13958150
                                                                        13958200
                                                                        13958250
                                                                        13958300
                                                                        13958350
                                                                        13958400
                                                                        13958450
                                                                        13958500
                                                                        13960000
                                                                        13961000
                                                                        13961050
                                                                        13961100
                                                                        13961200
                                                                        13962000
                                                                        13963000
                                                                        13964000
                                                                        13965000
                                                                        13966000
                                                                        13966100
                                                                        13966200
                                                                        13966300
                                                                        13966400
                                                                        13966500
                                                                        13967000
                                                                        13968000
                                                                        13969000
                                                                        13970000
                                                                        13970500
                                                                        13970600
                                                                        13970700
                                                                        13971000
                                                                        13972000
                                                                        13972001
                                                                        13973000
                                                                        13973500
END INLINE;                                                         %W7513974000
                GOTSCHK, ENDDEC,                                    %W7514016000
DEFINE LOGLVL = 2#;  LOGIN;                                         %W8214034100
    IF INDEX THEN WRITEPRT(0, 0, 1);                                %W4314034500
    IF  ERROROPTION AND SGAVL=2 THEN                                %W1214048200
        FOR NCIIO~ 0 STEP 1 UNTIL 3 DO                              %W1214048300
            FIRSTXO~ GETSPACE(TRUE);                                %W1214048400
    IF INDEX AND STREAMTOG THEN WRITEPRT(BUP,(TAKE(BUP+1).[12:6]+18)%W4314090200
      DIV 8, 3);                                                    %W4314090400
                IF INLINETAG THEN                                   %W7514128050
                IF ELBAT[I].ADDRESS = STREAMV THEN                  %W7514128100
                    IF STEPI = LEFTPAREN  THEN                      %W7514128200
                        BEGIN                                       %W7514128300
                            I ~ I - 1;                              %W7514128400
                            GO TO ENDDEC                            %W7514128500
                        END                                         %W7514128600
                      ELSE                                          %W7514128700
                            I ~ I - 1;                              %W7514128800
%W75 CATCH FOR INLINE STREAM STATEMENTS (FIRST IN BLOCK)            %W7514128801
ENDDEC:                                                             %W7514131500
        THEN BEGIN HANDLESWLIST;                    %SAVE       %74 %W4814165510
            GOTSTORAGE ~ NOT SPECTOG OR GOTSTORAGE; %PRT        %74 %W4814165520
            GO TO START END;                        %CELL       %74 %W4814165530
          IF ELCLASS!RTPAREN THEN ERR(104) ELSE STEPIT;             %W3314177500
    IF INDEX THEN WRITEPRT(LINKF, 1, 3);                            %W4314211500
IF NOTETOG THEN FLAG((535+REAL(TB1))&"H"[1:43:2]); %COMPLEX SWIT%91 %W7414226500
    IF INDEX THEN BACKUPTO(XREFC-1, 5);                             %W4314240500
    IF (J~J+1)>9 THEN FLAG(527) ELSE                            %83 %B1014259080
IF PARM(ACCUM[1],GTA1,K,J) THEN FLAG(526) ELSE                  %83 %B1014259085
    IF K>62 THEN FLAG(141);                                     %83 %B1014259090
    IF ELCLASS!RTPAREN AND ELCLASS!RTBRKET THEN FLAG(525);      %83 %B1014259120
ERRORTOG ~ TRUE; % TURN LISTING OF ERRS BACK ON                 %83 %B1014266200
    XREFCT~ XREFC;                                                  %W4314306500
    IF INDEX THEN WRITEPRT(LINKF, 1, 3);                            %W4314311500
    IF INDEX THEN WRITEPRT(0, 0, 1);                                %W4314322500
IF STREAMTOG THEN BEGIN REC(30); STREAMWORDS END; % USES STR PROC   %Y9514324000
IF NOT (SLABTOG~ELCLASS!LFTBRKET) THEN WRL(10) ELSE             %93 %W1414325500
    IF INDEX THEN BACKUPTO(XREFCT+2, 0);                            %W4314333050
IF INDEX THEN IF NOT FWDTOG THEN WRITEPRT(MARK, 1, 9);              %W4314334500
      THEN IF ELCLASS!RTBRKET OR SLABTOG THEN FLAG(008);            %W1414337000
    IF XREF THEN BACKUPTO(XREFC-1, 0);                              %W4314348500
                    PAIR(TRUE); %BEGIN                              %W7914395500
IF INDEX THEN WRITEPRT(0, 0, 2);                                    %W4314463500
IF INDEX THEN BEGIN BACKUPTO(XREFCT+1,4);WRITEPRT(MARK,1,9)END;     %W4314471500
    BEGIN PAIR(TRUE); %BEGIN                                        %W7914479500
    END;                                                            %W7914484500
    IF ELCLASS = BEGINV THEN BEGIN                                  %W7914487500
    STEPIT; COMPOUNDTAIL; %STMT SEES BEGIN, RECHECKS DECL, CALLS CMP%W7914488000
        END ELSE STMT;                                              %W7914488500
IF INDEX THEN WRITEPRT(0, 0, 2);                                    %W4314493500
IF LEVEL > 1 THEN IF NOTETOG THEN FLAG(98&"H"[1:43:2]);%NOTE EFF%69 %W7414549500
    IF INDEX THEN WRITEPRT(0, 0, 2);                                %W4314604500
LOGOUT                                                              %W8214612500
                PHONEYDEC;                                          %W4815006100
      L1:          STEPIT; IF TALL.FORMALNAME THEN                  %W5215092000
                   IF T1!0 OR ELCLASS=FACTOP THEN BEGIN EMITO(DUP); %W5215095000
                   EMITO(COC); IF T1!0 AND ELCLASS=FACTOP THEN      %W5215095500
                   EMITO(DUP) END;                                  %W5215095600
                   ELSE IF T1!0 OR ELCLASS=FACTOP THEN BEGIN        %W5215097000
                   EMITV(TALL.ADDRESS);IF T1!0 AND ELCLASS=FACTOP   %W5215097500
                   THEN EMITO(DUP) END;                             %W5215097600
                   STACKCT ~ REAL(T1!0);                            %W5215098000
            IF ELCLASS=FACTOP THEN BEGIN WRL(19);               %93 %W5215098200
                ELBAT[I].CLASS ~ ELCLASS ~CROSSHATCH END;       %93 %W5215098400
                   IF ELCLASS=FACTOP THEN BEGIN                     %W5215306200
                    WRL(19);                                    %93 %W5215306300
                        ELBAT[I].CLASS~ELCLASS~CROSSHATCH;          %W5215306400
                        EMITO(DUP); IF T1=0 THEN EMITO(LOD) END;    %W5215306600
  PROCEDURE DECLARELABEL;                                           %W2516023100
    COMMENT DO LABEL DECLARATION UPON FIRST APPEARANCE OF LABEL;    %W2516023200
      BEGIN                                                         %W2516023300
        WRL(14);                                                %93 %W2516023350
        KLASSF ~ STLABID;                                           %W2516023400
        VONF ~ FORMALF ~ FALSE;                                     %W2516023500
        ADDRSF ~ 0;                                                 %W2516023600
        MAKEUPACCUM; E; PUTNBUMP(0);                                %W2516023700
        ELBAT[I] ~ ACCUM[0]&LASTINFO[35:35:13];                     %W2516023800
    IF XREF THEN BACKUPTO(XREFC-1, 0);                              %W4316023810
    IF INDEX THEN WRITEPRT(LASTINFO,(COUNT+18)DIV 8, 3);            %W4316023820
    END DECLARELABEL;                                               %W2516023900
              IF F=BNS THEN IF SLABTOG THEN                         %W1416036000
IF SLABTOG THEN BEGIN IF NOTETOG THEN FLAG(275&"H"[1:43:2]);    %69 %W1416048000
    ADJUST END                                                  %69 %W1416048001
                         ELSE BEGIN FLAG(275); ERRORTOG~TRUE END;   %W1416048500
IF SLABTOG THEN BEGIN IF NOTETOG THEN FLAG(275&"H"[1:43:2]) END %69 %W1416077500
                         ELSE BEGIN FLAG(275); ERRORTOG~TRUE END;   %W1416077700
              EMIT(BNS);                                            %W1416122000
              IF SLABTOG THEN BEGIN EMIT(NOP); EMIT(NOP) END;       %W1416122500
              IF SLABTOG THEN ADJUST;                               %W1416137000
         REAL GT1;                                                  %W1416157500
         IF SLABTOG THEN ADJUST;                                    %W1416158000
    IF INDEX THEN WRITEPRT(GT1.[35:13], 1, 6);                      %W4316159500
    LABEL JMPTBL;                                                   %W7916193500
    IF ELCLASS = BEGINV THEN GO JMPTBL;%SKIP TABLE CALL             %W7916221000
        IF ELCLASS = IFV OR                                         %W7916222000
JMPTBL:                                                             %W7916225250
              IF SLABTOG THEN                                       %W1416225500
        IF STEPI ! STLABID THEN                                 %77 %W2516253000
            IF (IF ELCLASS{IDMAX THEN ELCLASS!LOCLID ELSE       %77 %W2516253025
                (ELCLASS=SUPERLISTID OR ELCLASS=FAULTID) ) THEN %77 %W2516253050
                DECLARELABEL ELSE BEGIN ERR(260); GO TO EXIT END;   %W2516253100
    IF INDEX=2 THEN WRL(13);                                    %93 %W2116345500
DEFINE LOGLVL = 4#;  LOGIN;                                         %W8216476500
          IF ELCLASS=COLON THEN BEGIN ADJUST;STEPIT;GO START END;   %W1416478200
              IF ELCLASS=STLABID THEN GO TO L2;                     %W2516478400
            IF (IF ELCLASS{IDMAX THEN ELCLASS!LOCLID ELSE       %77 %W2516478600
                (ELCLASS=SUPERLISTID OR ELCLASS=FAULTID) ) THEN %77 %W2516478700
                BEGIN DECLARELABEL; GO TO L2 END;                   %W2516478800
                                                                    %W2516481000
        L6: PAIR(TRUE); I~I+1; COMPOUNDTAIL; GO FINI;               %W7916489000
FINI:LOGOUT END STREAMSTMT;                                         %W8216495000
PROCEDURE   PAIR(B);  %LIST BEGIN-END PAIRS                         %W7916500000
        VALUE B;                                                    %W7916501000
        BOOLEAN B;                                                  %W7916502000
    BEGIN                                                           %W7916503000
    STREAM PROCEDURE BLANK(L);                                      %W7916504000
        BEGIN DI~L; DS~8 LIT" "; SI~L; DS~14 WDS END BLANK;         %W7916505000
    STREAM PROCEDURE PUT( S, N, D, L );                             %W7916506000
            VALUE S, N, D;                                          %W7916507000
        BEGIN                                                       %W7916508000
        DI~L;                                                       %W7916509000
        SI~LOC S; SI~SI+7; DS~CHR; S~DI;                            %W7916510000
        DS~3 DEC; DI~DI+1; N~DI;                                    %W7916511000
        DI~S; DS~3 FILL; DI~N;                                      %W7916512000
        DS~2 DEC;                                                   %W7916513000
        DI~N; DS~FILL                                               %W7916514000
        END PUT;                                                    %W7916515000
IF B THEN                                                           %W7916515100
    BEGIN                                                           %W7916515200
        PAIRCNT ~ PAIRCNT + 1;                                      %W7916515300
        IF PAIRLVL < 32 THEN PAIRARR[PAIRLVL] ~ PAIRCNT;            %W7916515400
        PAIRLVL ~ PAIRLVL + 1;                                      %W7916515500
    END ELSE                                                        %W7916515600
        IF PAIRLVL ~ PAIRLVL - 1 < 0 THEN PAIRLVL ~ 0;              %W7916515700
IF PAIRSTOG THEN IF LISTOG THEN                                     %W7916515800
    BEGIN                                                           %W7916515900
    IF IMAGE THEN                                                   %W7916516000
            IMAGE ~ FALSE                                           %W7916517000
        ELSE                                                        %W7916518000
            BEGIN                                                   %W7916519000
            WRITELINE;                                              %W7916520000
            BLANK(LIN)                                              %W7916521000
            END;                                                    %W7916522000
    IF B THEN                                                       %W7916523000
        PUT("B",PAIRCNT,PAIRLVL,LIN[*])                             %W7916524000
      ELSE                                                          %W7916527000
        PUT("E", IF PAIRLVL                                         %W7916528000
                    } 32 THEN 0 ELSE                                %W7916528200
            PAIRARR[ PAIRLVL ],                                     %W7916529000
            PAIRLVL + 1, LIN[*] )                                   %W7916530000
END END PAIR;                                                       %W7916531000
PROCEDURE FEATURE(N); VALUE N; INTEGER N;                           %W9316532000
BEGIN % N=0-63 FOR COUNTERS, -1 TO -96 FOR FLAGS                    %W9316533000
    DEFINE BASE=800#; % MESSAGE IN BASE + N                         %W9316534000
    INTEGER I;                                                      %W9316535000
    STREAM PROCEDURE COUNT(V,C,W); VALUE V,C;                       %W9316536000
            BEGIN LABEL X;                                          %W9316537000
            SI~W; SI~SI+C; IF SC=""" THEN GO X; C~SI;               %W9316537500
        DI~LOC C; DI~DI-1; W~DI; DS~CHR;                            %W9316538000
        TALLY~V; TALLY~TALLY+1; V~TALLY;                            %W9316539000
        DI~C; SI~W; DS~CHR; X:                                      %W9316540000
        END COUNT;                                                  %W9316541000
    STREAM PROCEDURE FLAGIT(B,C,W); VALUE B,C;                      %W9316542000
        BEGIN DI~W;DI~DI+C;SKIP B DB; DS~SET END;                   %W9316543000
    IF WARNTOG THEN FLAG(-(BASE+N));                                %W9316544000
    IF N}0 THEN COUNT(0,I~N MOD 8, FEATURES[N DIV 8]) %CHAR-WORD    %W9316545000
        ELSE FLAGIT( I~(N ~ -N-1) MOD 6, %BIT % N ADJUSTED          %W9316546000
                I~(N~N DIV 6) MOD 8, %CHAR % TO RANGE               %W9316547000
                    FEATURES[8+N DIV 8] ); %WORD % 0 THRU 95        %W9316548000
    END FEATURE;                                                    %W9316549000
%17000000 COVERS TIME1~TIME(1);    PROGRAM;                         %W9916999999
ERRORCOUNT~0; PROGRAM;                                              %W-217000000
    IF TIMEITTOG THEN                                               %W-217000010
ERRORPRINT:                                                         %W1617000015
        BEGIN PHONEYDEC;                                        %85 %W-517000020
$ VOID 17000181 % OLD W16, W-5, W79 PATCHES                         %W1617000030
            WRITELINE; % PRINT LAST LINE                            %W7917000190
% UNNECESSARY SINCE CHANGE NOTE PRINT OUT--OLD W-2 PATCH            %W-217000195
        BLANK(LIN); WRITE(LINE,15,LIN[*]);                      %85 %W-517000199
            TIMEIT(LIN );        WRITELINE;                         %W-217000200
        END;                                                        %W-217000210
                                                                        17000300
IF INDEX THEN IF ERRORCOUNT ! 0 THEN CLOSE(DATA,PURGE) ELSE         %Y9617000400
    IF SAVETOG THEN LOCK(DATA) ELSE CLOSE(DATA);                    %Y9617000500
ENDOFITALL:  END OF MAIN BLOCK   END.                               %Y9617000600
                                                                        99999999
                                                                        99999999
