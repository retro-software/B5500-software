COMMENT                                                                      100
              TIME-PERT B5000                                                200
              AN ALGOL 60 PROGRAM FOR THE ANALYSIS                           300
              AND PLANNING OF NETWORK FLOW PROBLEMS                          400
              ANNE G. MACEK                                                  500
              (PROFESSIONAL SERVICES, BURROUGHS CORPORATION)                 600
              FIRST RELEASE DATE 7-1-64                                      700
                                                                       ;     800
COMMENT                                                                      900
              BLOCK STRUCTURE                                               1000
         :-------------------------                                         1100
         :                   DECLARE GLOBAL VARIABLES                       1200
         :                                                                  1300
         : PROCEDURE TYPEOUT                                                1400
         : :-----------------------                                         1500
         : :                 TYPE MESSAGES ON SUPERVISORY PRINTER           1600
         : :-----------------------                                         1700
         :                                                                  1800
         : PROCEDURE PURGE                                                  1900
         : :-----------------------                                         2000
         : :                 PURGE TAPE WITH A SAVE FACTOR                  2100
         : :-----------------------                                         2200
         :                                                                  2300
         : :-----------------------                                         2400
         : : CONTROL BLOCK   READ AND AUDIT CONTROL CARDS                   2500
         : :-----------------------                                         2600
         :                                                                  2700
         : :-----------------------                                         2800
         : :                 DECLARE ADDITIONAL GLOBAL VARIABLES            2900
         : :ORIGIN.................                                         3000
         : :      . CONTROL ROUTINE                                         3100
         : :      .................                                         3200
         : :                                                                3300
         : :BLOCK1:----------------                                         3400
         : :      :          READ SAVE RECORDS FOR AN UPDATE                3500
         : :      :          OR GENERATE REPORTS RUN                        3600
         : :      :----------------                                         3700
         : :                                                                3800
         : :BLOCK2:----------------                                         3900
         : :      : AUDIT BLOCK                                             4000
         : :      :    PROCEDURE UNPACK                                     4100
         : :      :    :-----------                                         4200
         : :      :    :-----------                                         4300
         : :      :    PROCEDURE RENUMBER                                   4400
         : :      :    :-----------                                         4500
         : :      :    :     COMPUTE RENUMBERED EVENT                       4600
         : :      :    :     WRITE EVENT NUMBER,RENUMBERED EVENT,           4700
         : :      :    :     AND EVENT DESCRIPTION ON EVENT TAPE            4800
         : :      :    :-----------                                         4900
         : :      :    PROCEDURE CARDAUDIT                                  5000
         : :      :    :-----------                                         5100
         : :      :    :     AUDIT ACTIVITIES                               5200
         : :      :    :     WRITE PREDECESSOR EVENT,SUCCESSOR EVENT,       5300
         : :      :    :     PACKED RENUMBERED EVENTS,ACTIVITY DATE,        5400
         : :      :    :     ACTIVITY DURATION, AND ACTIVITY                5500
         : :      :    :     DESCRIPTION ON ACTIVITY TAPE                   5600
         : :      :    :-----------                                         5700
         : :      :          CHECK NETWORK START DATE AND REPORT            5800
         : :      :          DATE                                           5900
         : :      :          READ AND AUDIT ACTIVITY CARDS                  6000
         : :      :    :-----------                                         6100
         : :      :    :     LIST ENDING AND STARTING EVENTS                6200
         : :      :    :-----------                                         6300
         : :      :    :-----------                                         6400
         : :      :    :     WRITE TALLY COUNTS ON ACTIVITY TAPE            6500
         : :      :    :     FILE                                           6600
         : :      :    :-----------                                         6700
         : :      :----------------                                         6800
         : :                                                                6900
         : :BLOCK3:----------------                                         7000
         : :      : RANK BLOCK                                              7100
         : :      :          RANK EVENTS                                    7200
         : :      :          WRITE EVENT RANKS ON EVENT TAPE                7300
         : :      :          WRITE ACTIVITY RANKS ON ACTIVITY               7400
         : :      :          TAPE                                           7500
         : :      :          LIST ACTIVITIES IN A LOOP                      7600
         : :      :----------------                                         7700
         : :                                                                7800
         : :BLOCK4:----------------                                         7900
         : :      : SORT BLOCK                                              8000
         : :      :    PROCEDURE TAPESORT                                   8100
         : :      :    :-----------                                         8200
         : :      :    :     SORT TAPE FILE                                 8300
         : :      :    :-----------                                         8400
         : :      :    :-----------                                         8500
         : :      :    :     FILL GLOBAL ARRAYS                             8600
         : :      :    :-----------                                         8700
         : :      :----------------                                         8800
         : :                                                                8900
         : :BLOCK5:----------------                                         9000
         : :      : NETANAL BLOCK                                           9100
         : :      :          CALCULATE EARLIEST START AND SUMS              9200
         : :      :          OF VARIANCE AND WRITE THEM ON                  9300
         : :      :          ACTIVITY TAPE                                  9400
         : :      :          WRITE EARLIEST OCCURRENCE AND SUMS             9500
         : :      :          OF VARIANCE ON EVENT TAPE                      9600
         : :      :          CALCULATE LATEST FINISH                        9700
         : :      :          WRITE LATEST FINISH AND ACTIVITY               9800
         : :      :          SLACK ON ACTIVITY TAPE                         9900
         : :      :          WRITE LATEST OCCURRENCE AND EVENT             10000
         : :      :          SLACK ON EVENT TAPE                           10100
         : :      :----------------                                        10200
         : :                                                               10300
         : :BLOCK6:----------------                                        10400
         : :      : LIST BLOCK                                             10500
         : :      :    PROCEDURE PACK                                      10600
         : :      :    :-----------                                        10700
         : :      :    :-----------                                        10800
         : :      :    PROCEDURE UPACK                                     10900
         : :      :    ............                                        11000
         : :      :    ............                                        11100
         : :      :                                                        11200
         : :      :    :-----------                                        11300
         : :      :    : PROCEDURE NORMPROB                                11400
         : :      :    : :---------                                        11500
         : :      :    : :   COMPUTE PROBABILITY OF MEETING                11600
         : :      :    : :   SCHEDULE DATES                                11700
         : :      :    : :------- -                                        11800
         : :      :    :     LIST ACTIVITIES                               11900
         : :      :    :-----------                                        12000
         : :      :                                                        12100
         : :      :   :------------                                        12200
         : :      :   :      LIST EVENTS                                   12300
         : :      :   :------------                                        12400
         : :      :----------------                                        12500
         : :                                                               12600
         : :BLOCK7:----------------                                        12700
         : :      : PLOT BLOCK                                             12800
         : :      :    PROCEDURE UPACK                                     12900
         : :      :    ............                                        13000
         : :      :    ............                                        13100
         : :      :    :-----------                                        13200
         : :      :    : PROCEDURE UNPACK                                  13300
         : :      :    : :---------                                        13400
         : :      :    : :---------                                        13500
         : :      :    :     COMPUTE STARTING DATE OF GRAPH                13600
         : :      :    :-----------                                        13700
         : :      :                                                        13800
         : :      :    :-----------                                        13900
         : :      :    :     WRITE HEADING IF PLOT BY DAYS OR              14000
         : :      :    :     WEEKS                                         14100
         : :      :    :-----------                                        14200
         : :      :                                                        14300
         : :      :    :-----------                                        14400
         : :      :    :     WRITE HEADING IF PLOT BY MONTHS               14500
         : :      :    :-----------                                        14600
         : :      :                                                        14700
         : :      :    :-----------                                        14800
         : :      :    :     WRITE HEADING IF PLOT BY QUARTERS             14900
         : :      :    :-----------                                        15000
         : :      :                                                        15100
         : :      :    :-----------                                        15200
         : :      :    : PROCEDURE PACK                                    15300
         : :      :    : :---------                                        15400
         : :      :    : :---------                                        15500
         : :      :    :     PLOT GRAPH                                    15600
         : :      :    :-----------                                        15700
         : :      :----------------                                        15800
         :  :                                                              15900
         : :FINISH:----------------                                        16000
         : :      : FINISH BLOCK                                           16100
         : :      :          WRITE SAVE RECORDS ON TAPE                    16200
         : :      :    :-----------                                        16300
         : :      :    :     LIST SAVE RECORDS AND ATIVITY TAPES           16400
         : :      :    :     IF PROGRAM TERMINATED BEFORE                  16500
         : :      :    :     COMPLETION BECASE OF DATA ERRORS              16600
         : :      :    :-----------                                        16700
         : :      :    :-----------                                        16800
         : :      :    :     LIST EXECUTED OPERATIONS                      16900
         : :      :    :-----------                                        17000
         : :      :----------------                                        17100
         : :-----------------------                                        17200
         :                   WRITE MESSAGES ON SUPERVISORY                 17300
         :                   PRINTER                                       17400
ENDOFPROG:-------------------------                                        17500
                                                                       ;   17600
         BEGIN                                                             17700
COMMENT                                                                    17800
              DEFINITIONS OF GLOBAL VARIABLES                              17900
              NOACT     NUMBER OF ACTIVITIES                               18000
              NOEVN     NUMBER OF EVENTS                                   18100
              MAXNOACT  MAXIMUM PERMISSIBLE NUMBER OF ACTIVITIES           18200
              MAXNOEVN  MAXIMUM PERMISSIBLE NUMBER OF EVENTS               18300
              MINEV     MINIMUM EVENT NUMBER                               18400
              MAXEV     MAXIMUM EVENT NUMBER                               18500
              RUN       RUN NUMBER                                         18600
              SDATE     NETWORK STARTING DATE                              18700
              SDAYS     NUMBER OF WORKING DAYS FROM 1/1/62 UNTIL NETWORK   18800
                        START DATE                                         18900
              WDPW      NUMBER OF WORKING DAYS PER WEEK                    19000
              RDATE     NETWORK REPORT DATE                                19100
              RDAYS     NUMBER OF WORKING DAYS FROM NETWORK START DATE     19200
                        UNTIL REPORT DATE                                  19300
              KEY       ACTIVITY NUMBER ON ACTIVITY TAPE FILE              19400
              I         PREDECESSOR EVENT NUMBER                           19500
              J         SUCCESSOR EVENT NUMBER                             19600
              RRIJ      PACKED VARIABLE CONTAINING RENUMBERED              19700
                        PREDECESSOR AND SUCCESSOR EVENTS                   19800
              ASD       FLAG DENOTING TYPE OF ACTIVITY DATE                19900
                           1 ACTUAL START DATE                             20000
                           2 ACTUAL COMPLETION DATE                        20100
                           3 DIRECTED START DATE                           20200
                           4 SCHEDULED COMPLETION DATE                     20300
              ADAYS     NUMBER OF WORKING DAYS FROM NETWORK START DATE     20400
                        UNTIL ACTIVITY DATE                                20500
              RANKI     ACTIVITY RANK -RANK OF PREDECESSOR EVENT-          20600
              TAL       PACKED VARIABLE CONTAINING NUMBER OF ACTIVITIES    20700
                        LEADING INTO AND NUMBER LEADING FROM SUCCESSOR     20800
                        EVENT AND NUMBER OF PRECEDING ACTIVITIES WHICH     20900
                        ARE COMPLETE                                       21000
              NOACTCOL,NOACTROW  UPPER BOUNDS FOR ARRAYS OF LENGTH NOACT   21100
              NOEVNCOL,NOEVNROW  UPPER BOUNDS FOR ARRAYS OF LENGTH NOEVN   21200
              NWPR      NUMBER OF WORDS PER LOGICAL RECORD ON ACTIVITY     21300
                        TAPE                                               21400
              SORTARRAYSIZE  NUMBER OF ROWS IN ARRAY CONTAINING            21500
                        ACTIVITIES TO BE SORTED                            21600
              NUMENDING MAXIMUM PERMISSIBLE NUMBER OF ENDING EVENTS        21700
              NUMSTARTING MAXIMUM PERMISSIBLE NUMBER OF STARTING EVENTS    21800
              CHAINLENGTH  LARGEST PERMISSIBLE NUMBER OF SERIAL            21900
                        ACTIVITIES                                         22000
              SRUN      RUN NUMBER READ FROM SAVE RECORDS TAPE             22100
              SSDATE    NETWORK START DATE READ FROM SAVE RECORDS TAPE     22200
              SNOACT    NUMBER OF ACTIVITIES READ FROM SAVE RECORDS TAPE   22300
              SSDAYS    NUMBER OF WORKING DAYS FROM 1/1/62 UNTIL NETWORK   22400
                        START DATE READ FROM SAVE RECORDS TAPE             22500
              RD        TAPE KEY OF TAPE FILE CONTAINING CURRENT           22600
                        ACTIVITY RECORDS                                   22700
              WR        TAPE KEY OF TAPE FILE TO BE WRITTEN NEXT           22800
              RN        RENUMBERED EVENT                                   22900
              EVNO      EVENT NUMBER                                       23000
              RANKE     EVENT RANK                                         23100
              TALLY     PACKED VARIABLE CONTAINING NUMBER OF ACTIVITIES    23200
                        LEADING INTO AND NUMBER LEADING FROM EVENT AND     23300
                        NUMBER OF PRECEDING ACTIVITIES WHICH ARE           23400
                        COMPLETE                                           23500
              NOSORT    NUMBER OF EVENT SORTS                              23600
              NCPR      NUMBER OF WORDS PER LOGICAL RECORD ON EVENT TAPE   23700
              RE        TAPE KEY OF TAPE FILE CONTAINING CURRENT EVENT     23800
                        RECORDS                                            23900
              SC        TAPE KEY OF TAPE FILE CONTAINING SAVE RECORDS      24000
              NE        TAPE KEY OF TAPE FILE CONTAINING NEW EVENT         24100
                        RECORDS                                            24200
              NTAPES    NUMBER OF TAPES TO BE USED FOR SORTING             24300
              ET        EXPECTED DURATION TIME OF AN ACTIVITY              24400
              VAR       ACTIVITY VARIANCE                                  24500
              SVAR      SUM OF VARIANCES ALONG MOST CRITICAL PATH          24600
                        LEADING TO GIVEN ACTIVITY                          24700
              ES        EARLIEST STARTING TIME FOR ACTIVITY                24800
              LF        LATEST FINISH TIME FOR ACTIVITY ALLOWED IN ORDER   24900
                        TO MEET SCHEDULE ENDING DATES                      25000
              SL        ACTIVITY SLACK                                     25100
              EEO       EARLIEST EVENT OCCURRENCE                          25200
              LEO       LATEST EVENT OCCURRENCE                            25300
              EVAR      SUM OF VARIANCES ALONG MOST CRITICAL PATH          25400
                        LEADING TO EVENT                                   25500
              SLCK      EVENT SLACK                                        25600
              NETNAME   ARRAY CONTAINING NETWORK NAME                      25700
              SNETNAME  ARRAY CONTAINING NETWORK NAME READ FROM SAVE       25800
                        RECORDS TAPE                                       25900
              REMARKS   ARRAY CONTAINING ACTIVITY DESCRIPTION              26000
              DSCPT     ARRAY CONTAINING EVENT DESCRIPTION                 26100
              PERIOD    INTERVAL OF TIME ESTIMATES -DAYS OR WEEKS-         26200
              ITEMNAME  ARRAY CONTAINING NAMES OF ITEMS IN ACTIVITY        26300
                        RECORD ON TAPE                                     26400
              EVENTITEM ARRAY CONTAINING NAMES OF ITEMS IN EVENT RECORD    26500
              INST      ARRAY CONTAINING SPO MESSAGES                      26600
              BLOCKTOTAL  NUMBER OF BLOCKS OF CODE TO BE EXECUTED          26700
              RSORTNUM  NUMBER OF ACTIVITY SORTS                           26800
              NOBLOCK   INDEX OF ARRAYS BLOC AND BLOCPARAM                 26900
              UPDATE    FLAG DENOTING TYPE OF AUDIT TO BE PERFORMED        27000
                           0 INITIAL INPUT AUDIT                           27100
                           1 UPDATE WITHOUT NETWORK CHANGES                27200
                           2 UPDATE WITH NETWORK CHANGES                   27300
              SAVETAPES BOOLEAN FLAG, TRUE IF INPUT TAPES FOR AN UPDATE    27400
                        OR GENERATE REPORTS RUN ARE TO BE SAVED            27500
              BSAVA     BOOLEAN FLAG, TRUE IF ACTIVITY TAPE IS TO BE       27600
                        SAVED DURING A GENERATE REPORTS RUN                27700
              BSAVE     BOOLEAN FLAG, TRUE IF EVENT TAPE IS TO BE SAVED    27800
                        DURING A GENERATE REPORTS RUN                      27900
              ONETIME   BOOLEAN FLAG, TRUE IF ONLY ONE TIME ESTIMATE       28000
                        IS SPECIFIED                                       28100
              LISTTAL   BOOLEAN FLAG, TRUE IF END TALLIES ARE TO BE        28200
                        LISTED                                             28300
              EVENTTAPE BOOLEAN FLAG, TRUE IF AN EVENT TAPE IS TO BE       28400
                        MADE                                               28500
              BLOC      ARRAY CONTAINING BLOCK NUMBERS WHICH ARE TO BE     28600
                        EXECUTED                                           28700
              BLOCPARAM  ARRAY CONTAINING PARAMETERS NECESSARY FOR         28800
                        CODING TO BE EXECUTED                              28900
              SORTCOL   ARRAY CONTAINING NUMBER OF SORTKEYS AND SORTKEY    29000
                        NUMBERS FOR ACTIVITY SORTS                         29100
              EVENTCOL  ARRAY CONTAINING NUMBER OF SORTKEYS AND SORTKEY    29200
                        NUMBERS FOR EVENT SORTS                            29300
              LL,K      INDICES FOR LISTS                                  29400
                                                                       ;   29500
INTEGER       NOACT,NOEVN,MAXNOACT,MAXNOEVN,MINEV,MAXEV,RUN,SDATE,SDAYS,   29600
              WDPW,RDATE,RDAYS,KEY,I,J,RRIJ,ASD,ADAYS,RANKI,TAL,LL,K  ;    29700
INTEGER       NOACTCOL,NOACTROW,NOEVNCOL,NOEVNROW,NWPR,SORTARRAYSIZE ;     29800
INTEGER       NUMENDING,NUMSTARTING,CHAINLENGTH  ;                         29900
INTEGER       SRUN,SSDATE,SNOACT,SSDAYS  ;                                 30000
INTEGER       RD,WR  ;                                                     30100
INTEGER       RN,EVNO,RANKE,TALLY,NOSORT,NCPR,RE,SC  ;                     30200
INTEGER       NE  ;                                                        30300
INTEGER       NTAPES  ;                                                    30400
FILE OUT      PRINTER 1(2,15)  ;                                           30500
FILE OUT      SPOOUT(1,20)  ;                                              30600
FILE          DUMMYFILE(1,10)  ;                                           30700
FILE          PERTA 2(2,256,19,SAVE 30)  ;                                 30800
FILE          PERTB 2(2,256,19,SAVE 30)  ;                                 30900
FILE          PERTC 2(2,256,250,SAVE 30)  ;                                31000
FILE          PERTE 2(2,256,19,SAVE 30)  ;                                 31100
FILE          PERTF 2(2,256,19,SAVE 30)  ;                                 31200
SWITCH FILE   SWF ~ DUMMYFILE,PERTA,PERTB,PERTC,PERTE,PERTF  ;             31300
REAL          ET,VAR,SVAR,ES,LF,SL ;                                       31400
REAL          EEO,LEO,EVAR,SLCK  ;                                         31500
ALPHA ARRAY   NETNAME[0:13],REMARKS[0:4] ;                                 31600
ALPHA ARRAY   DSCPT[0:4]  ;                                                31700
ALPHA ARRAY   SNETNAME[0:13]  ;                                            31800
ALPHA         PERIOD ;                                                     31900
ALPHA ARRAY   ITEMNAME[0:15] ;                                             32000
ALPHA ARRAY   EVENTITEM[0:10]  ;                                           32100
ARRAY         INST[0:5,0:12]  ;                                            32200
ALPHA ARRAY   FILENAMES[0:5]  ;                                            32300
INTEGER ARRAY BLOC[0:40],BLOCPARAM[0:40],SORTCOL[0:9,0:5]  ;               32400
INTEGER ARRAY EVENTCOL[0:9,0:5]  ;                                         32500
LABEL         ENDOFPROG  ;                                                 32600
LABEL         MAINSTART ;                                                  32700
LIST          SAVEREC1(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL],RUN,      32800
                   SDATE,SDAYS,RDATE,RDAYS,MAXNOACT,MAXNOEVN,              32900
                   CHAINLENGTH,NUMENDING,NUMSTARTING,WDPW,NOACT,NOEVN,     33000
                   NOACTROW,NOACTCOL,NOEVNROW,NOEVNCOL,MINEV,MAXEV,        33100
                   FOR LL ~ 1 STEP 1 UNTIL 5 DO SORTCOL[0,LL],FOR LL ~     33200
                   1 STEP 1 UNTIL 5 DO EVENTCOL[0,LL],SC,RD,RE,WR,NE),     33300
             SSAVEREC1(FOR LL ~ 1 STEP 1 UNTIL 13 DO SNETNAME[LL],SRUN,    33400
                   SSDATE,SSDAYS,RDATE,RDAYS,MAXNOACT,MAXNOEVN,            33500
                   CHAINLENGTH,NUMENDING,NUMSTARTING,WDPW,SNOACT,NOEVN,    33600
                   NOACTROW,NOACTCOL,NOEVNROW,NOEVNCOL,MINEV,MAXEV,        33700
                   FOR LL ~ 1 STEP 1 UNTIL 5 DO SORTCOL[0,LL],FOR LL ~     33800
                   1 STEP 1 UNTIL 5 DO EVENTCOL[0,LL],SC,RD,RE,WR,NE)  ;   33900
INTEGER       BLOCKTOTAL,RSORTNUM,NOBLOCK,UPDATE  ;                        34000
BOOLEAN       SAVETAPES  ;                                                 34100
BOOLEAN       BSAVE  ;                                                     34200
BOOLEAN       BSAVA  ;                                                     34300
BOOLEAN       ONETIME,LISTTAL  ;                                           34400
BOOLEAN       EVENTTAPE  ;                                                 34500
PROCEDURE     TYPEOUT(F,A)  ;                                              34600
COMMENT       THIS PROCEDURE TYPES OUT THE MESSAGES CONTAINED IN THE       34700
              ARRAY A ON THE SUPERVISORY PRINTER  ;                        34800
FILE          F  ;                                                         34900
ARRAY         A[0]  ;                                                      35000
         BEGIN                                                             35100
SAVE ARRAY    B[0:7]  ;                                                    35200
STREAM PROCEDURE TYPE(F,B)  ;                                              35300
         BEGIN                                                             35400
LOCAL         TA  ;                                                        35500
LABEL         L,L2  ;                                                      35600
COMMENT       BUILD DESCRIPTOR  ;                                          35700
              SI ~ B  ;  TA ~ SI  ;  DI ~ LOC TA  ;  DS ~ 5 LIT "{|000";   35800
              DI ~ F  ;  SI ~ LOC TA  ;  DS ~ 1 WDS  ;  RELEASE(F)  ;      35900
COMMENT       WAIT FOR PRESENCE  ;                                         36000
L:            SI ~ F  ;  SKIP 2 SB  ;  IF SB THEN GO TO L2  ; GO TO L  ;   36100
L2:      END  ;                                                            36200
STREAM PROCEDURE  LOAD(A,B)  ;                                             36300
         BEGIN                                                             36400
              SI ~ A  ;  DI ~ B  ;                                         36500
              10(SI ~ SI + 2  ;  DS ~ 6 CHR)  ;                            36600
         END  ;                                                            36700
              LOAD(A,B)  ;  TYPE(F,B)  ;                                   36800
         END  TYPEOUT  ;                                                   36900
PROCEDURE     PURGE(F,FACTOR)  ;                                           37000
COMMENT       THIS PROCEDURE PURGES A TAPE WITH A SAVE FACTOR  ;           37100
FILE          F  ;                                                         37200
INTEGER       FACTOR  ;                                                    37300
         BEGIN                                                             37400
ARRAY         TEMP[0:1]  ;                                                 37500
LABEL         L1,L2,RET  ;                                                 37600
SWITCH        SW ~ L1,L2,RET  ;                                            37700
INTEGER       SAVE1,SAVE2  ;                                               37800
STREAM PROCEDURE FIXSAVEFACTOR(F,SAVE1,SAVE2)  ;                           37900
         BEGIN                                                             38000
LOCAL         T  ;                                                         38100
              SI ~ F  ;  SI ~ SI - 24  ;  DI ~ LOC T  ;  DS ~ WDS  ;       38200
              SI ~ T  ;  SI ~ SI + 37  ;  DI ~ SAVE2  ;  DI ~ DI + 5  ;    38300
              DS ~ 3 CHR  ;                                                38400
              SI ~ SAVE1  ;  SI ~ SI + 5  ;  DI ~ T  ;  DI ~ DI + 37  ;    38500
              DS ~ 3 CHR  ;                                                38600
         END  FIXSAVEFACTOR  ;                                             38700
              SAVE1 ~ SAVE2 ~ 0  ;  REWIND(F)  ;                           38800
              FIXSAVEFACTOR(F,SAVE1,SAVE2)  ;                              38900
              WRITE(F,1,TEMP[*])  ;                                        39000
              FIXSAVEFACTOR(F,SAVE2,SAVE1)  ;                              39100
              GO TO SW[FACTOR]  ;                                          39200
L1:           CLOSE(F,SAVE)  ;  GO TO RET  ;                               39300
L2:           CLOSE(F,RELEASE)  ;                                          39400
RET:     END  PURGE  ;                                                     39500
MAINSTART:                                                                 39600
              FILL ITEMNAME[*] WITH "   KEY","  PRED","  SUCC",            39700
                   "  RRIJ","    ET","   VAR","   ASD",                    39800
                   " ADAYS","    ES","    LF","    SL","  SVAR",           39900
                   "  RANK"," TALLY","      ","      "  ;                  40000
              FILL INST[1,*] WITH "      ","IS THE"," SAVE ","RECORD",     40100
                   "S TAPE"," FILE.","~     "  ;                           40200
              FILL INST[2,*] WITH "      ","IS THE"," ACTIV","ITIES ",     40300
                   "TAPE F","ILE.~ "  ;                                    40400
              FILL INST[3,*] WITH "      ","IS THE"," EVENT"," TAPE ",     40500
                   "FILE.~"  ;                                             40600
              FILL INST[4,*] WITH "SAVE T","HIS TA","PE FOR"," AN UP",     40700
                   "DATE R","UN.~  "  ;                                    40800
              FILL INST[5,*] WITH "REMOVE"," THIS ","TAPE.~"  ;            40900
              FILL FILENAMES[*] WITH "DUMMY ","PERTA ","PERTB ",           41000
                   "PERTC ","PERTE ","PERTF "  ;                           41100
              ONETIME ~ LISTTAL ~ FALSE  ;                                 41200
         BEGIN                                                             41300
COMMENT       CONTROL BLOCK  ;                                             41400
COMMENT                                                                    41500
              DEFINITIONS OF VARIABLES IN CONTROL BLOCK                    41600
              NOMODULES NUMBER OF MEMORY MODULES ON THE B5000 SYSTEM       41700
                        BEING USED                                         41800
              CONTROL1  ARRAY CONTAINING CARD IMAGE OF A CONTROL CARD      41900
              CONTROL2  ARRAY CONTAINING VALUES OF INPUT PARAMETERS ON     42000
                        A CONTROL CARD                                     42100
              NOTES     ARRAY CONTAINING COMMENTS                          42200
              INA,INE   TEMPORARY ARRAYS                                   42300
              LL,MM,NN  INDICES FOR ARRAYS                                 42400
                                                                       ;   42500
INTEGER       MM,NN,LL  ;                                                  42600
INTEGER       NOMODULES  ;                                                 42700
ALPHA ARRAY   CONTROL1[0:6]  ;                                             42800
ALPHA ARRAY   NOTES[0:14]  ;                                               42900
INTEGER ARRAY CONTROL2[0:5]  ;                                             43000
INTEGER ARRAY INA,INE[0:9]  ;                                              43100
LABEL         L1,L2,L3,L4,L5  ;                                            43200
LABEL         ER  ;                                                        43300
LABEL         ER2,ER4,EOJ  ;                                               43400
LIST          CONTROLIN(FOR NN ~ 1 STEP 1 UNTIL 6 DO CONTROL1[NN],         43500
                   FOR NN ~ 1 STEP 1 UNTIL 5 DO CONTROL2[NN]),             43600
              REMARK(FOR NN ~ 1 STEP 1 UNTIL 14 DO NOTES[NN]),             43700
             CONTROLOUT1(FOR NN ~ 1 STEP 1 UNTIL 6 DO CONTROL1[NN]),       43800
              CONTROLOUT2(FOR NN ~ 1 STEP 1 UNTIL 6 DO CONTROL1[NN],       43900
                   CONTROL2[1]),                                           44000
              CONTROLOUT3(FOR NN ~ 1 STEP 1 UNTIL 6 DO CONTROL1[NN],       44100
                   CONTROL2[1],CONTROL2[2])  ;                             44200
FILE IN       FILECONTR(2,10)  ;                                           44300
FORMAT IN     FORMCONTR(6A6,X4,I10,4I5)  ;                                 44400
FORMAT        COMMENTS(13A6,A2)  ;                                         44500
FORMAT OUT    OUTER("CONTROL CARD INCORRECT. THE FOLLOWING CONTROL",       44600
                   "CARDS ARE IGNORED"),                                   44700
              OUTMOD("NO MODULES NOT SPECIFIED"),                          44800
              OUTTAPE("A MINIMUM OF THREE TAPES ARE NEEDED"),              44900
              OUTWDPW("NO WORKING DAYS PER WEEK INVALID"),                 45000
              OUTSORTA("TWO SUCCESSIVE ACTIVITY SORTS, SORTED ON FIRST",   45100
                   " ONLY"),                                               45200
              OUTSORTE("TWO SUCCESSIVE EVENT SORTS, SORTED ON FIRST ",     45300
                   "ONLY"),                                                45400
              OUTSEVENT("EVENT TAPE SORT, ALTHOUGH NO EVENT TAPE ",        45500
                   "SPECIFIED, AN EVENT TAPE IS ASSUMED"),                 45600
              OUTLEVENT("EVENT TAPE LIST, ALTHOUGH NO EVENT TAPE ",        45700
                   "SPECIFIED, AN EVENT TAPE IS ASSUMED"),                 45800
              OUTSORTKEYS("MORE THAN FOUR SORTKEYS CALLED FOR, FIRST ",    45900
                   "FOUR ARE USED"),                                       46000
              OUTREPORTS("MORE THAN NINE SORTS ARE CALLED FOR."//"THE ",   46100
                   "FOLLOWING CONTROL CARDS ARE IGNORED:"/),               46200
              OUTSORTDE("LAST EVENT SORT OMITTED BECAUSE SORT BY EVNO ",   46300
                   "IMMEDIATELY FOLLOWS"),                                 46400
              OUTSORTDA("LAST ACTIVITY SORT OMITTED BECAUSE SORT BY I ",   46500
                   "OR  J IMMEDIATELY FOLLOWS"),                           46600
              OUTLISTDE("LIST OF EVENTS BY DEPARTMENT NOT POSSIBLE ",      46700
                   "BECAUSE NO MORE EVENT SORTS ARE PERMITTED"),           46800
              OUTLISTDA("LIST OF ACTIVITIES BY DEPARTMENT NOT POSSIBLE",   46900
                   "BECAUSE NO MORE ACTIVITY SORTS ARE PERMITTED"),        47000
              OUTPLOT("PLOT INTERVAL INCORRECT, PLOT IGNORED"),            47100
              OUTPLOTD("GRAPH OF ACTIVITIES BY DEPARTMENT NOT POSSIBLE",   47200
                   " BECAUSE NO MORE ACTIVITY  SORTS ARE PERMITTED"),      47300
              OUTBLOC("THE TOTAL NUMBER OF SORT, LIST, AND PLOT CARDS ",   47400
                   "EXCEEDS THE MAX NO. ALLOWED"//"THE FOLLOWING ",        47500
                   "CONTROL CARDS ARE IGNORED:"/),                         47600
              OUTEOF("NO MCP END OF FILE CARD FOR FILE FILECONTR,"         47700
                   " THE FOLLOWING CARDS ARE IGNORED"),                    47800
              OUTCARD1(6A6),                                               47900
              OUTCARD2(6A6,X4,I10),                                        48000
              OUTCARD3(6A6,X4,I10,4I5),                                    48100
              OUTCARD4(6A6,X4,I10,I5)  ;                                   48200
              FOR NN ~ 0 STEP 1 UNTIL 40 DO BLOC[NN] ~ 0 ;                 48300
              FOR  NN ~ 0 STEP 1 UNTIL 40 DO BLOCPARAM[NN] ~ 0  ;          48400
COMMENT       READ PERT CONTROL CARDS  ;                                   48500
              WRITE(PRINTER[PAGE])  ;                                      48600
              MM ~ 1  ;                                                    48700
COMMENT       READ HEADER CARD  ;                                          48800
              READ(FILECONTR,FORMCONTR,CONTROLIN)[ENDOFPROG]  ;            48900
              IF (CONTROL1[1] ! "PERT C") OR (CONTROL1[2] ! "ONTROL")      49000
                   OR (CONTROL1[3] ! " CARDS") THEN                        49100
         BEGIN                                                             49200
              WRITE(PRINTER[DBL],OUTCARD3,CONTROLIN)  ;                    49300
              WRITE(PRINTER[DBL],OUTER)  ;                                 49400
              GO TO ENDOFPROG  ;                                           49500
         END  ;                                                            49600
              WRITE(PRINTER[DBL],OUTCARD1, CONTROLOUT1)  ;                 49700
COMMENT       READ COMMENT CARD  ;                                         49800
              READ(FILECONTR[NO],FORMCONTR,CONTROLIN)[ENDOFPROG]  ;        49900
              IF (CONTROL1[1] = "COMMEN") AND (CONTROL1[2] = "T     ")     50000
                   THEN                                                    50100
         BEGIN                                                             50200
              READ(FILECONTR,COMMENTS,REMARK)  ;                           50300
              WRITE(PRINTER[DBL],COMMENTS,REMARK)  ;                       50400
              FOR LL ~ 3 STEP 1 UNTIL 10 DO INST[0,LL-3] ~ NOTES[LL]  ;    50500
              INST[0,8] ~ ".~    "  ;                                      50600
              TYPEOUT(SPOOUT,INST[0,*])  ;                                 50700
         END  ;                                                            50800
COMMENT       READ NUMBER OF MODULES CARD  ;                               50900
              READ(FILECONTR,FORMCONTR,CONTROLIN)[ENDOFPROG]  ;            51000
              IF (CONTROL1[1] !"NUMBER") OR (CONTROL1[2] ! " OF MO")       51100
                   OR (CONTROL1[3] ! "DULES ") THEN                        51200
         BEGIN                                                             51300
              WRITE(PRINTER[DBL],OUTCARD3,CONTROLIN)  ;                    51400
              WRITE(PRINTER[DBL],OUTMOD)  ;                                51500
              GO TO ENDOFPROG  ;                                           51600
         END  ;                                                            51700
              NOMODULES ~ CONTROL2[1]  ;                                   51800
              WRITE(PRINTER[DBL],OUTCARD2,CONTROLOUT2)  ;                  51900
COMMENT       CALCULATE SORTARRAYSIZE  ;                                   52000
              IF (NOMODULES { 4) THEN SORTARRAYSIZE ~ 100 ELSE             52100
              SORTARRAYSIZE ~ 500  ;                                       52200
              BLOCKTOTAL ~ RSORTNUM ~ 0  ;                                 52300
              UPDATE ~ 0  ;                                                52400
              FOR NN ~ 1 STEP 1 UNTIL 9 DO INA[NN] ~ 0  ;                  52500
              FOR NN ~ 1 STEP 1 UNTIL 9 DO INE[NN] ~ 0  ;                  52600
              NOSORT ~ 0  ;  RSORTNUM ~ 0  ;                               52700
              BSAVA ~ BSAVE ~ FALSE  ;                                     52800
              EVENTTAPE ~ FALSE  ;                                         52900
              SAVETAPES ~ FALSE  ;                                         53000
COMMENT       READ TAPE CARDS  ;                                           53100
              READ(FILECONTR,FORMCONTR,CONTROLIN)[ENDOFPROG]  ;            53200
              IF (CONTROL1[1] = "NUMBER") AND (CONTROL1[2] = " OF TA")     53300
                   AND (CONTROL1[3] = "PES   ") THEN                       53400
         BEGIN                                                             53500
              WRITE(PRINTER[DBL],OUTCARD2,CONTROLOUT2)  ;                  53600
              NTAPES ~ CONTROL2[1]  ;                                      53700
              IF (NTAPES < 3) THEN                                         53800
         BEGIN                                                             53900
              WRITE(PRINTER[DBL],OUTTAPE)  ;                               54000
              NTAPES ~ 3  ;                                                54100
         END  ;                                                            54200
              READ(FILECONTR,FORMCONTR,CONTROLIN)[ENDOFPROG]  ;            54300
         END                                                               54400
              ELSE NTAPES ~ 3  ;                                           54500
              IF (CONTROL1[1] = "SAVE T") AND (CONTROL1[2] = "APES  ")     54600
                   THEN                                                    54700
         BEGIN                                                             54800
              SAVETAPES ~ TRUE  ;                                          54900
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  55000
              READ(FILECONTR,FORMCONTR,CONTROLIN)[ENDOFPROG]  ;            55100
         END  ;                                                            55200
              IF (CONTROL1[1] = "EVENT ") AND (CONTROL1[2] = "TAPE  ")     55300
                   THEN                                                    55400
         BEGIN                                                             55500
              EVENTTAPE ~ TRUE  ;                                          55600
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  55700
              READ(FILECONTR,FORMCONTR,CONTROLIN)[ENDOFPROG]  ;            55800
         END  ;                                                            55900
COMMENT       READ RUN TYPE CARDS  ;                                       56000
              MM ~ MM + 1  ;                                               56100
              IF (CONTROL1[1] = "INPUT ") AND (CONTROL1[2] = "AUDIT ")     56200
                   THEN                                                    56300
         BEGIN                                                             56400
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  56500
COMMENT       SET PARAMETERS FOR AN INPUT AUDIT RUN  ;                     56600
              BLOCKTOTAL ~ 4  ;                                            56700
              FOR NN ~ 1 STEP 1 UNTIL 4 DO BLOC[NN] ~ NN + 1  ;            56800
              RSORTNUM ~ 1  ;  INA[1] ~ 3  ;                               56900
              FILL SORTCOL[0,*] WITH 0,1,0,15,15,15  ;                     57000
              FILL SORTCOL[1,*] WITH 0,3,12,1,2,15  ;                      57100
              IF EVENTTAPE THEN FILL EVENTCOL[0,*] WITH 0,1,0,10,10,10 ;   57200
              GO TO L3                                                     57300
         END  ;                                                            57400
              IF (CONTROL1[1] = "UPDATE") THEN                             57500
         BEGIN                                                             57600
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  57700
COMMENT       SET PARAMETERS FOR AN UPDATE RUN  ;                          57800
              BLOCKTOTAL ~ 2  ;  BLOC[1] ~ 1  ;  BLOC[2] ~ 2  ;            57900
              READ(PERTC,*,SSAVEREC1)  ;                                   58000
              IF (CONTROL1[2] = " WITH ") AND (CONTROL1[3] = "CHANGE")     58100
                   THEN UPDATE ~ 2 ELSE UPDATE ~ 1  ;                      58200
              IF EVENTTAPE AND ((UPDATE = 2) AND (EVENTCOL[0,1] ! 0))      58300
                   THEN                                                    58400
         BEGIN                                                             58500
              BLOCPARAM[2] ~ 1  ;                                          58600
              IF (EVENTCOL[0,2] ! 1) THEN                                  58700
         BEGIN                                                             58800
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               58900
              BLOC[BLOCKTOTAL] ~ 4  ;                                      59000
              BLOCPARAM[BLOCKTOTAL] ~ 1  ;                                 59100
              NOSORT ~ 1  ;  INE[1] ~ BLOCKTOTAL  ;                        59200
              FILL EVENTCOL[1,*] WITH 0,1,1,10,10,10  ;                    59300
         END  ;                                                            59400
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               59500
         BLOC[BLOCKTOTAL] ~ 4  ;  BLOCPARAM[BLOCKTOTAL] ~ 2  ;             59600
              NOSORT ~ NOSORT + 1  ;                                       59700
              FILL EVENTCOL[NOSORT,*] WITH 0,1,1,10,10,10  ;               59800
         END  ;                                                            59900
              IF (UPDATE = 2) THEN                                         60000
         BEGIN                                                             60100
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               60200
              BLOC[BLOCKTOTAL] ~ 3  ;                                      60300
              BLOCPARAM[BLOCKTOTAL] ~ BLOCPARAM[2]  ;                      60400
         END  ;                                                            60500
              IF (UPDATE = 2) OR (SORTCOL[0,2] ! 12) OR                    60600
                   (SORTCOL[0,3] ! 1) OR (SORTCOL[0,4] ! 2) THEN           60700
         BEGIN                                                             60800
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               60900
              BLOC[BLOCKTOTAL] ~ 4  ;                                      61000
              RSORTNUM ~ 1  ;                                              61100
              INA[1] ~ BLOCKTOTAL  ;                                       61200
              FILL SORTCOL[1,*] WITH 0,3,12,1,2,15  ;                      61300
         END  ;                                                            61400
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               61500
              BLOC[BLOCKTOTAL] ~ 5  ;                                      61600
              GO TO L3                                                     61700
         END  ;                                                            61800
L1:           IF (CONTROL1[1] = "END OF") AND (CONTROL1[2] = " JOB  ")     61900
                   THEN                                                    62000
         BEGIN                                                             62100
              IF (MM = 2) THEN                                             62200
         BEGIN                                                             62300
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  62400
              GO TO ENDOFPROG  ;                                           62500
         END  ;                                                            62600
              GO TO EOJ  ;                                                 62700
         END  ;                                                            62800
              IF (CONTROL1[1] = "GENERA") AND (CONTROL1[2] = "TE REP")     62900
                   AND (CONTROL1[3] = "ORTS  ") THEN                       63000
         BEGIN                                                             63100
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  63200
              IF MM = 2 THEN                                               63300
         BEGIN                                                             63400
COMMENT       SET PARAMETERS FOR A GENERATE REPORTS RUN  ;                 63500
              BLOCKTOTAL ~ 1  ;  BLOC[1] ~ 1  ;                            63600
              READ(PERTC,*,SAVEREC1)  ;                                    63700
              IF SAVETAPES THEN                                            63800
         BEGIN                                                             63900
              BSAVE ~ TRUE  ;                                              64000
              BSAVA ~ TRUE  ;                                              64100
         END  ;                                                            64200
         END  ;                                                            64300
COMMENT       READ REPORT CARDS  ;                                         64400
L2:           READ(FILECONTR,FORMCONTR,CONTROLIN)[ER2]  ;                  64500
              IF (CONTROL1[1] = "END OF") AND (CONTROL1[2] = " JOB  ")     64600
              THEN IF (BLOCKTOTAL > 1) THEN GO TO EOJ ELSE                 64700
         BEGIN                                                             64800
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  64900
              GO TO ENDOFPROG  ;                                           65000
         END  ;                                                            65100
              IF (BLOCKTOTAL = 39) THEN                                    65200
         BEGIN                                                             65300
              WRITE(PRINTER[DBL],OUTBLOC)  ;                               65400
              WRITE(PRINTER[DBL],OUTCARD3,CONTROLIN)  ;                    65500
              GO TO ER  ;                                                  65600
         END  ;                                                            65700
              IF (CONTROL1[1] = "SORT  ") THEN                             65800
         BEGIN                                                             65900
              WRITE(PRINTER[DBL],OUTCARD3,CONTROLIN)  ;                    66000
              IF (CONTROL1[2] = "EVENTS")THEN                              66100
         BEGIN                                                             66200
              IF NOT EVENTTAPE THEN                                        66300
         BEGIN                                                             66400
              WRITE(PRINTER[DBL],OUTSEVENT)  ;                             66500
              EVENTTAPE ~ TRUE  ;                                          66600
         END  ;                                                            66700
              IF (INE[NOSORT] = BLOCKTOTAL) THEN                           66800
         BEGIN                                                             66900
              WRITE(PRINTER[DBL],OUTSORTE)  ;                              67000
              GO TO L2  ;                                                  67100
         END  ;                                                            67200
              NOSORT ~ NOSORT + 1  ;                                       67300
              IF NOSORT > 9 THEN                                           67400
         BEGIN                                                             67500
              WRITE(PRINTER[DBL],OUTREPORTS)  ;                            67600
              GO TO ER  ;                                                  67700
         END  ;                                                            67800
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               67900
              INE[NOSORT] ~ BLOCKTOTAL  ;                                  68000
              BLOC[BLOCKTOTAL] ~ 4  ;                                      68100
              BLOCPARAM[BLOCKTOTAL] ~ 1  ;                                 68200
              IF CONTROL2[1] > 4 THEN                                      68300
         BEGIN                                                             68400
              WRITE(PRINTER[DBL],OUTSORTKEYS)  ;                           68500
              EVENTCOL[NOSORT,1] ~ 4  ;                                    68600
         END                                                               68700
              ELSE EVENTCOL[NOSORT,1] ~ CONTROL2[1]  ;                     68800
              FOR NN ~ 2 STEP 1 UNTIL EVENTCOL[NOSORT,1] + 1 DO            68900
                   EVENTCOL[NOSORT,NN] ~ CONTROL2[NN]  ;                   69000
              FOR NN ~ EVENTCOL[NOSORT,1] + 2 STEP 1 UNTIL 5 DO            69100
                   EVENTCOL[NOSORT,NN] ~ 10  ;                             69200
              GO TO L2  ;                                                  69300
         END  ;                                                            69400
              IF (INA[RSORTNUM] = BLOCKTOTAL) THEN                         69500
         BEGIN                                                             69600
              WRITE(PRINTER,OUTSORTA)  ;  GO TO L2  ;                      69700
         END  ;                                                            69800
              RSORTNUM ~ RSORTNUM + 1  ;                                   69900
              IF RSORTNUM > 9 THEN                                         70000
         BEGIN                                                             70100
              WRITE(PRINTER,OUTREPORTS)  ;                                 70200
              GO TO ER  ;                                                  70300
         END  ;                                                            70400
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               70500
              INA[RSORTNUM] ~ BLOCKTOTAL  ;                                70600
              BLOC[BLOCKTOTAL] ~ 4  ;                                      70700
              IF (CONTROL2[1] > 4) THEN                                    70800
         BEGIN                                                             70900
              WRITE(PRINTER[DBL],OUTSORTKEYS)  ;                           71000
              SORTCOL[RSORTNUM,1] ~ 4  ;                                   71100
         END                                                               71200
              ELSE SORTCOL[RSORTNUM,1] ~ CONTROL2[1]  ;                    71300
              FOR NN ~ 2 STEP 1 UNTIL SORTCOL[RSORTNUM,1] + 1 DO           71400
              SORTCOL[RSORTNUM,NN] ~ CONTROL2[NN]  ;                       71500
              FOR NN ~ SORTCOL[RSORTNUM,1] + 2 STEP 1 UNTIL 5 DO           71600
              SORTCOL[RSORTNUM,NN] ~ 15  ;                                 71700
              GO TO L2                                                     71800
         END  ;                                                            71900
              IF (CONTROL1[1] = "LIST  ") THEN                             72000
         BEGIN                                                             72100
              IF (CONTROL1[6].[12:24] = "DEPT") THEN                       72200
              WRITE(PRINTER[DBL],OUTCARD2,CONTROLOUT2) ELSE                72300
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  72400
              IF (CONTROL1[2] = "EVENTS") OR ((CONTROL1[2] = "MILEST")     72500
                   AND (CONTROL1[3] = "ONES  ")) THEN                      72600
         BEGIN                                                             72700
              IF NOT EVENTTAPE THEN                                        72800
         BEGIN                                                             72900
              WRITE(PRINTER[DBL],OUTLEVENT)  ;                             73000
              EVENTTAPE ~ TRUE  ;                                          73100
         END  ;                                                            73200
              IF (CONTROL1[6] = "DEPT  ") OR (CONTROL1[2] = "MILEST")      73300
                   THEN                                                    73400
         BEGIN                                                             73500
              IF (EVENTCOL[NOSORT,2] ! 1) THEN                             73600
         BEGIN                                                             73700
              IF (INE[NOSORT] = BLOCKTOTAL) THEN                           73800
              WRITE(PRINTER[DBL],OUTSORTDE) ELSE                           73900
         BEGIN                                                             74000
              IF (BLOCKTOTAL = 38) THEN                                    74100
         BEGIN                                                             74200
              WRITE(PRINTER[DBL],OUTBLOC)  ;                               74300
              GO TO ER  ;                                                  74400
         END  ;                                                            74500
              IF (NOSORT = 9) THEN                                         74600
         BEGIN                                                             74700
              WRITE(PRINTER[DBL],OUTLISTDE)  ;                             74800
              GO TO L2  ;                                                  74900
         END  ;                                                            75000
              NOSORT ~ NOSORT + 1  ;                                       75100
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               75200
              INE[NOSORT] ~ BLOCKTOTAL  ;                                  75300
              BLOC[BLOCKTOTAL] ~ 4  ;                                      75400
              BLOCPARAM[BLOCKTOTAL] ~ 1  ;                                 75500
         END  ;                                                            75600
              FILL EVENTCOL[NOSORT,*] WITH 0,1,1,10,10,10  ;               75700
         END  ;                                                            75800
              IF (CONTROL1[6] = "DEPT  ") THEN                             75900
         BEGIN                                                             76000
              BLOCPARAM[BLOCKTOTAL+1].[16:3] ~ 1  ;                        76100
              BLOCPARAM[BLOCKTOTAL+1].[20:3] ~ CONTROL2[1]  ;              76200
         END                                                               76300
              ELSE BLOCPARAM[BLOCKTOTAL+1].[16:3] ~ 2  ;                   76400
         END  ;                                                            76500
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               76600
              BLOC[BLOCKTOTAL] ~ 6  ;                                      76700
              BLOCPARAM[BLOCKTOTAL].[9:3] ~ 1  ;                           76800
              GO TO L2  ;                                                  76900
         END  ;                                                            77000
              IF (CONTROL1[6].[12:24] = "DEPT") THEN                       77100
         BEGIN                                                             77200
              IF ((CONTROL1[6].[42:6] = "J") AND (SORTCOL[RSORTNUM,2]      77300
                   ! 2)) OR (SORTCOL[RSORTNUM,2] ! 1) THEN                 77400
         BEGIN                                                             77500
              IF (INA[RSORTNUM] = BLOCKTOTAL) THEN                         77600
                   WRITE(PRINTER[DBL],OUTSORTA) ELSE                       77700
         BEGIN                                                             77800
              IF (BLOCKTOTAL = 38) THEN                                    77900
         BEGIN                                                             78000
              WRITE(PRINTER[DBL],OUTBLOC)  ;                               78100
              GO TO ER ;                                                   78200
         END  ;                                                            78300
              IF (RSORTNUM = 9) THEN                                       78400
         BEGIN                                                             78500
         WRITE(PRINTER[DBL],OUTLISTDA)  ;                                  78600
              GO TO L2  ;                                                  78700
         END  ;                                                            78800
              RSORTNUM ~ RSORTNUM + 1  ;                                   78900
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               79000
              INA[RSORTNUM] ~ BLOCKTOTAL  ;                                79100
              BLOC[BLOCKTOTAL] ~ 4  ;                                      79200
         END  ;                                                            79300
              IF (CONTROL1[6].[42:6] = "J")THEN                            79400
                   FILL SORTCOL[RSORTNUM,*] WITH 0,1,2,15,15,15            79500
                   ELSE FILL SORTCOL[RSORTNUM,*] WITH 0,1,1,15,15,15  ;    79600
         END  ;                                                            79700
              IF (CONTROL1[6].[42:6] = "J")THEN                            79800
              BLOCPARAM[BLOCKTOTAL + 1].[16:3] ~ 2 ELSE                    79900
              BLOCPARAM[BLOCKTOTAL + 1].[16:3] ~ 1  ;                      80000
              BLOCPARAM[BLOCKTOTAL + 1].[20:3] ~ CONTROL2[1]  ;            80100
         END  ;                                                            80200
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               80300
              BLOC[BLOCKTOTAL] ~ 6  ;                                      80400
              FOR NN ~ 3,4 DO                                              80500
         BEGIN                                                             80600
              IF (CONTROL1[NN] = "DUMMY ") THEN                            80700
              BLOCPARAM[BLOCKTOTAL] ~ BLOCPARAM[BLOCKTOTAL] +1  ;          80800
              IF (CONTROL1[NN] = "COMPL ") THEN                            80900
              BLOCPARAM[BLOCKTOTAL] ~ BLOCPARAM[BLOCKTOTAL] + 2            81000
         END  ;                                                            81100
              GO TO L2                                                     81200
         END  ;                                                            81300
              IF (CONTROL1[1] = "PLOT  ") THEN                             81400
         BEGIN                                                             81500
              IF (CONTROL1[6].[12:24] = "DEPT") THEN                       81600
              WRITE(PRINTER[DBL],OUTCARD3,CONTROLOUT3) ELSE                81700
              WRITE(PRINTER[DBL],OUTCARD2,CONTROLOUT2)  ;                  81800
              IF (CONTROL1[6].[12:24] = "DEPT") THEN                       81900
         BEGIN                                                             82000
              IF ((CONTROL1[6].[42:6] = "J") AND (SORTCOL[RSORTNUM,2]      82100
                   ! 2)) OR (SORTCOL[RSORTNUM,2] ! 1) THEN                 82200
         BEGIN                                                             82300
              IF (INA[RSORTNUM] = BLOCKTOTAL) THEN                         82400
                   WRITE(PRINTER[DBL],OUTSORTDA) ELSE                      82500
         BEGIN                                                             82600
              IF(BLOCKTOTAL = 38) THEN                                     82700
         BEGIN                                                             82800
              WRITE(PRINTER[DBL],OUTBLOC)  ;                               82900
              GO TO ER  ;                                                  83000
         END  ;                                                            83100
              IF RSORTNUM = 9 THEN                                         83200
         BEGIN                                                             83300
              WRITE(PRINTER[DBL],OUTPLOT)  ;                               83400
              GO TO L2  ;                                                  83500
         END  ;                                                            83600
              RSORTNUM ~ RSORTNUM + 1  ;                                   83700
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               83800
              INA[RSORTNUM] ~ BLOCKTOTAL  ;                                83900
              BLOC[BLOCKTOTAL] ~ 4  ;                                      84000
         END  ;                                                            84100
              IF (CONTROL1[5].[42:6] = "J") THEN                           84200
                   FILL SORTCOL[RSORTNUM,*] WITH 0,1,2,15,15,15            84300
                   ELSE FILL SORTCOL[RSORTNUM,*] WITH 0,1,1,15,15,15  ;    84400
         END  ;                                                            84500
              IF (CONTROL1[5].[42:6] = "J") THEN                           84600
              BLOCPARAM[BLOCKTOTAL+1].[16:3] ~ 2 ELSE                      84700
              BLOCPARAM[BLOCKTOTAL+1].[16:3] ~ 1  ;                        84800
              BLOCPARAM[BLOCKTOTAL+1].[20:3] ~ CONTROL2[2]  ;              84900
         END ;                                                             85000
              IF (CONTROL1[5] = "DAYS  ") THEN                             85100
              BLOCPARAM[BLOCKTOTAL+1].[9:3] ~ 1 ELSE                       85200
              IF (CONTROL1[5] = "WEEKS ") THEN                             85300
              BLOCPARAM[BLOCKTOTAL+1].[9:3] ~ 2 ELSE                       85400
              IF (CONTROL1[5] = "MONTHS") THEN                             85500
              BLOCPARAM[BLOCKTOTAL + 1].[9:3] ~ 3 ELSE                     85600
              IF (CONTROL1[5] = "QTRS  ") THEN                             85700
              BLOCPARAM[BLOCKTOTAL+1].[9:3] ~ 4 ELSE                       85800
         BEGIN                                                             85900
              WRITE(PRINTER,OUTPLOT)  ;  GO TO L2                          86000
         END  ;                                                            86100
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               86200
              BLOC[BLOCKTOTAL] ~ 7  ;                                      86300
              FOR NN ~ 3,4 DO                                              86400
         BEGIN                                                             86500
              IF (CONTROL1[NN] = "DUMMY ") THEN                            86600
              BLOCPARAM[BLOCKTOTAL].[12:3] ~                               86700
                   BLOCPARAM[BLOCKTOTAL].[12:3] + 1  ;                     86800
              IF (CONTROL1[NN] = "COMPL ") THEN                            86900
              BLOCPARAM[BLOCKTOTAL].[12:3] ~                               87000
                   BLOCPARAM[BLOCKTOTAL].[12:3] + 2  ;                     87100
         END  ;                                                            87200
              BLOCPARAM[BLOCKTOTAL].[24:24] ~ CONTROL2[1].[24:24]  ;       87300
              GO TO L2                                                     87400
         END                                                               87500
         END  ;                                                            87600
              WRITE(PRINTER[DBL],OUTCARD3,CONTROLIN)  ;                    87700
              WRITE(PRINTER[DBL],OUTER)  ;                                 87800
              IF (BLOCKTOTAL } 2) THEN GO TO ER ELSE GO TO ENDOFPROG  ;    87900
COMMENT       READ OPTION CARDS FOR INPUT AUDIT OR UPDATE RUNS  ;          88000
L3:           READ(FILECONTR,FORMCONTR,CONTROLIN)[ER2]  ;                  88100
              IF (CONTROL1[1] = "DATA O") AND (CONTROL1[2] = "N TAPE")     88200
                   THEN                                                    88300
         BEGIN                                                             88400
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  88500
              IF (UPDATE = 0) THEN BLOCPARAM[1].[40:3] ~ 1                 88600
              ELSE BLOCPARAM[2].[40:3] ~ 1  ;                              88700
              GO TO L3  ;                                                  88800
         END  ;                                                            88900
              IF (CONTROL1[1] = "ONE TI") AND (CONTROL1[2] = "ME    ")     89000
                   THEN                                                    89100
         BEGIN                                                             89200
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  89300
              ONETIME ~ TRUE  ;  GO TO L3  ;                               89400
         END  ;                                                            89500
              IF (CONTROL1[1] = "LIST E") AND (CONTROL1[2] = "ND TAL")     89600
                   AND (CONTROL1[3] = "LIES  ") THEN                       89700
         BEGIN                                                             89800
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  89900
              LISTTAL ~ TRUE  ;  GO TO L3  ;                               90000
         END  ;                                                            90100
COMMENT       SET GLOBAL PARAMETERS  ;                                     90200
              IF UPDATE = 0 THEN                                           90300
         BEGIN                                                             90400
              CHAINLENGTH ~ MAXNOACT ~ MAXNOEVN ~ 511  ;                   90500
              NUMENDING ~ NUMSTARTING ~ 10  ;  WDPW ~ 5  ;                 90600
         END  ;                                                            90700
L4:           MM ~ MM + 1  ;                                               90800
              IF (CONTROL1[1] = "CHAIN ") AND (CONTROL1[2] = "LENGTH")     90900
                   THEN CHAINLENGTH ~ CONTROL2[1] ELSE                     91000
              IF (CONTROL1[2] = " ACTIV") AND (CONTROL1[3] = "ITIES ")     91100
                   THEN MAXNOACT ~ CONTROL2[1] ELSE                        91200
              IF (CONTROL1[2] = " EVENT") AND (CONTROL1[3] = "S     ")     91300
                   THEN MAXNOEVN ~ CONTROL2[1] ELSE                        91400
              IF (CONTROL1[1] = "WORKIN") AND (CONTROL1[2] = "G DAYS")     91500
                   AND (CONTROL1[3] = " PER W") AND (CONTROL1[4] =         91600
                   "EEK   ") THEN                                          91700
         BEGIN                                                             91800
              WDPW ~ CONTROL2[1]  ;                                        91900
              IF (WDPW > 7) OR (WDPW < 1) THEN                             92000
         BEGIN                                                             92100
              WRITE(PRINTER[DBL],OUTCARD2,CONTROLOUT2)  ;                  92200
              WRITE(PRINTER[DBL],OUTWDPW)  ;                               92300
              GO TO ENDOFPROG  ;                                           92400
         END                                                               92500
         END  ELSE                                                         92600
              IF (CONTROL1[2] = " ENDIN") AND (CONTROL1[3] = "G EVEN")     92700
                   AND (CONTROL1[4] = "TS    ") THEN NUMENDING ~           92800
                   CONTROL2[1] ELSE                                        92900
              IF (CONTROL1[2] = " START") AND (CONTROL1[3] = "ING EV")     93000
                   AND (CONTROL1[4] = "ENTS  ") THEN NUMSTARTING ~         93100
              CONTROL2[1] ELSE GO TO L1  ;                                 93200
              WRITE(PRINTER[DBL],OUTCARD2,CONTROLOUT2)  ;                  93300
              READ(FILECONTR,FORMCONTR,CONTROLIN)[ER2]  ;                  93400
              IF MM < 8 THEN GO TO L4  ;                                   93500
              GO TO L1 ;                                                   93600
ER:           READ(FILECONTR,FORMCONTR,CONTROLIN)[ER2]  ;                  93700
              IF (CONTROL1[1] ! "END OF") AND (CONTROL1[2] ! " JOB ")      93800
                   THEN                                                    93900
         BEGIN                                                             94000
              WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  94100
              GO TO ER  ;                                                  94200
         END  ;                                                            94300
EOJ:          WRITE(PRINTER[DBL],OUTCARD1,CONTROLOUT1)  ;                  94400
              BLOCKTOTAL ~ BLOCKTOTAL + 1  ;                               94500
              BLOC[BLOCKTOTAL] ~ 8  ;                                      94600
              READ(FILECONTR,FORMCONTR,CONTROLIN)[L5]  ;                   94700
              WRITE(PRINTER[DBL],OUTEOF)  ;                                94800
ER4:          WRITE(PRINTER[DBL],OUTCARD3,CONTROLIN)  ;                    94900
              READ(FILECONTR,FORMCONTR,CONTROLIN)[L5]  ;                   95000
              GO TO ER4  ;                                                 95100
ER2:          IF BLOCKTOTAL < 40 THEN BLOCKTOTAL ~ BLOCKTOTAL + 1  ;       95200
              BLOC[BLOCKTOTAL] ~ 8  ;                                      95300
L5:      END  CONTROL BLOCK  ;                                             95400
         BEGIN                                                             95500
COMMENT                                                                    95600
              DEFINITIONS OF GLOBAL VARIABLES                              95700
              EK        NUMBER OF ENDING EVENTS                            95800
              BK        NUMBER OF STARTING EVENTS                          95900
              TP        TEMPORARY TAPEKEY                                  96000
              REEV      ARRAY CONTAINING RENUMBERED ENDING EVENTS          96100
              RBEV      ARRAY CONTAINING RENUMBERED STARTING EVENTS        96200
              EVENT     ARRAY CONTAINING EVENT NUMBERS                     96300
              RIRJ      PACKED ARRAY CONTAINING RENUMBERED PREDECESSOR     96400
                        AND SUCCESSOR EVENT NUMBERS FOR EACH ACTIVITY      96500
                                                                       ;   96600
INTEGER       EK,BK,TP  ;                                                  96700
INTEGER ARRAY REEV[0:NUMENDING],RBEV[0:NUMSTARTING]  ;                     96800
INTEGER ARRAY EVENT[0:MAXNOEVN.[30:9],0:IF MAXNOEVN.[30:9] = 0 THEN        96900
                   MAXNOEVN.[39:9] ELSE 511] ,                             97000
              RIRJ[0:MAXNOACT.[30:9],0:IF MAXNOACT.[30:9] = 0              97100
                   THEN MAXNOACT.[39:9] ELSE 511]  ;                       97200
LIST          TREC(KEY,I,J,RRIJ,ET,VAR,ASD,ADAYS,ES,LF,SL,SVAR,RANKI,      97300
                   TAL,FOR LL ~ 1 STEP 1 UNTIL 4 DO REMARKS[LL])  ;        97400
LIST          EREC(RN,EVNO,RANKE,EEO,EVAR,LEO,SLCK,TALLY,FOR LL ~ 1 STEP   97500
                   1 UNTIL 4 DO DSCPT[LL])  ;                              97600
LIST          SAVEREC2(FOR LL ~ 0 STEP 1 UNTIL NOACTCOL DO                 97700
                   RIRJ[K,LL]),                                            97800
              SAVEREC3(FOR LL ~ 0 STEP 1 UNTIL NOEVNCOL DO EVENT[K,LL]);   97900
LABEL         BLOCK1,BLOCK2,BLOCK3,BLOCK4,BLOCK5,BLOCK6,BLOCK7,            98000
              ORIGIN,FINISH  ;                                             98100
SWITCH        BLOCKCONTROL ~ BLOCK1,BLOCK2,BLOCK3,BLOCK4,BLOCK5,BLOCK6,    98200
                   BLOCK7,FINISH  ;                                        98300
              IF EVENTTAPE THEN                                            98400
              FILL EVENTITEM[*] WITH " RENUM"," EVENT","  RANK",           98500
                   "   EEO","  EVAR","   LEO"," SLACK"," TALLY",           98600
                   "      ","      ","      "  ;                           98700
              RSORTNUM ~ 0  ;                                              98800
              NOBLOCK ~ 0  ;                                               98900
              NCPR ~ 12  ;                                                 99000
              NWPR ~ 19  ;                                                 99100
              NOSORT ~ 0  ;                                                99200
COMMENT       CONTROL ROUTINE  ;                                           99300
ORIGIN:       NOBLOCK ~ NOBLOCK + 1  ;                                     99400
              GO TO BLOCKCONTROL[BLOC[NOBLOCK]]  ;                         99500
BLOCK1:  BEGIN                                                             99600
COMMENT       READ SAVE RECORDS BLOCK  ;                                   99700
              FOR K ~ 0 STEP 1 UNTIL NOACTROW DO                           99800
              READ(PERTC,*,SAVEREC2)  ;                                    99900
              FOR K ~ 0 STEP 1 UNTIL NOEVNROW DO                          100000
              READ(PERTC,*,SAVEREC3)  ;                                   100100
              IF SAVETAPES THEN                                           100200
         BEGIN                                                            100300
              LOCK(PERTC,SAVE)  ;                                         100400
              INST[1,0] ~ FILENAMES[3]  ;                                 100500
              TYPEOUT(SPOOUT,INST[1,*])  ;  TYPEOUT(SPOOUT,INST[5,*])  ;  100600
         END                                                              100700
              ELSE PURGE(PERTC,1)  ;                                      100800
         END ;                                                            100900
              GO TO ORIGIN  ;                                             101000
BLOCK2:  BEGIN                                                            101100
COMMENT                                                                   101200
              DEFINITIONS OF VARIABLES IN AUDIT BLOCK                     101300
              OT        OPTIMISTIC TIME ESTIMATE                          101400
              ML        MOST LIKELY TIME ESTIMATE                         101500
              PT        PESSIMISTIC TIME ESTIMATE                         101600
              BOOL2     BOOLEAN FLAG, TRUE IF ERROR FOUND IN AUDIT BLOCK  101700
              BOOL4     BOOLEAN FLAG, TRUE IF ERROR IN TIME ESTIMATES     101800
              WEEKFLAG  BOOLEAN FLAG, TRUE IF TIME ESTIMATES ARE GIVEN    101900
                        IN WEEKS                                          102000
              BTAPE     BOOLEAN FLAG, TRUE IF ACTIVITY BEING AUDITED      102100
                        WAS READ FROM ACTIVITY TAPE FILE                  102200
              ASDAT     ACTIVITY DATE                                     102300
              DAS       DAY OF THE MONTH OF NETWORK START DATE            102400
              MOS       MONTH NUMBER OF NETWORK START DATE                102500
              YRS       YEAR OF NETWORK START DATE                        102600
              DAR       DAY OF THE MONTH OF NETWORK REPORT DATE           102700
              MOR       MONTH NUMBER OF REPORT DATE                       102800
              YRR       YEAR OF REPORT DATE                               102900
              DAA       DAY OF THE MONTH OF ACTIVITY DATE                 103000
              MOA       MONTH NUMBER OF ACTIVITY DATE                     103100
              YRA       YEAR OF ACTIVITY DATE                             103200
              NINES     9999999999                                        103300
              CSDATE    CORRECTED NETWORK START DATE                      103400
              CRDATE    CORRECTED REPORT DATE                             103500
              CASDATE   CORRECTED ACTIVITY DATE                           103600
              SWER      FLAG TO DENOTE TYPE OF ERROR IN AN AUDITED DATE   103700
              IKEY      KEY NUMBER OF ACTIVITY CARD                       103800
              CODE      FLAG TO DENOTE TYPE OF ACTIVITY CARD              103900
                           0 INITIAL ACTIVITES FOR INPUT AUDIT RUN        104000
                           1 UPDATE ACTIVITY TIMES                        104100
                           2 ADD NEW ACTIVITY                             104200
                           3 DELETE ACTIVITY                              104300
              NLINE     NUMBER OF LINES PRINTED ON PAGE BY PRINTER        104400
              TLINE     TOTAL NUMBER PRINTED LINES ALLOWED PER PAGE       104500
              PAGEN     PAGE NUMBER                                       104600
              SI        PREDECESSOR EVENT NUMBER ON ACTIVITY TAPE FILE    104700
              SJ        SUCCESSOR EVENT NUMBER ON ACTIVITY TAPE FILE      104800
              SASD      FLAG ON ACTIVITY TAPE FILE DENOTING TYPE OF       104900
                        ACTIVITY DATE                                     105000
              TAPEKEY   KEY OF NEXT ACTIVITY ON ACTIVITY TAPE FILE        105100
              TKEY      INDEX FOR READING ACTIVITY TAPE FILE              105200
              DELETEKEYS  NUMBER OF DELETED ACTIVITIES                    105300
              AC        TAPE KEY REFERRING TO NETWORK DATA FILE           105400
              DPM       ARRAY CONTAINING NUMBER OF DAYS IN EACH MONTH     105500
              TALLIES   PACKED ARRAY CONTAINING NUMBER OF TIMES           105600
                        RENUMBERED EVENTS ARE USED AS PREDECESSORS AND    105700
                        SUCCESSORS OF ACTIVITIES AND NUMBER OF PRECEDING  105800
                        ACTIVITIES THAT HAVE BEEN COMPLETED               105900
              SHEAD     ARRAY CONTAINING SCRAMBLE TABEL FOR RENUMBERING   106000
              MONTH     ARRAY CONTAINING ABBREVIATION OF EACH MONTH       106100
              SREMARKS  ARRAY CONTAINING ACTIVITY DESCRIPTION ON          106200
                        ACTIVITY TAPE FILE                                106300
              ITEMS     ARRAY CONTAINING TYPE OF NETWORK DATA - EVENTS    106400
                        OR ACTIVITIES                                     106500
              K,L,KR,KC,KRI,KCI,KRJ,KCJ  INDICES OF ARRAYS                106600
              TEMP,TEMP1  TEMPORARY LOCAL VARIABLES                       106700
                                                                       ;  106800
COMMENT       AUDIT BLOCK  ;                                              106900
REAL          OT,ML,PT;                                                   107000
BOOLEAN       BOOL2,BOOL4  ;                                              107100
BOOLEAN       WEEKFLAG  ;                                                 107200
BOOLEAN       BTAPE  ;                                                    107300
LABEL         START,LL1,LL2,LL4,LL5,ERR11,ERR12,ERR13,ERR21,ERR22,ERR23,  107400
              LL3,EOF,LL1A,LL2A  ;                                        107500
LABEL         CODE1  ;                                                    107600
LABEL         CODE3  ;                                                    107700
LABEL         LL1B,LL2B  ;                                                107800
LABEL         LL6,LL7  ;                                                  107900
LABEL         LOOP2,LL5A,LOOP3,EOF3  ;                                    108000
INTEGER       ASDAT,DAS,MOS,YRS,DAR,MOR,YRR,DAA,MOA,YRA,NINES,CSDATE,     108100
              CRDATE,CASDAT,SWER,TEMP1,K,L  ;                             108200
INTEGER       IKEY,CODE ;                                                 108300
INTEGER       NLINE,TLINE,PAGEN ;                                         108400
INTEGER       KR,KC,TEMP  ;                                               108500
INTEGER       SI,SJ,SASD,TAPEKEY  ;                                       108600
INTEGER       TKEY,DELETEKEYS  ;                                          108700
INTEGER       AC  ;                                                       108800
INTEGER ARRAY DPM[0:12] ;                                                 108900
INTEGER ARRAY TALLIES[0:MAXNOEVN.[30:9],0:IF MAXNOEVN.[30:9] = 0 THEN     109000
                   MAXNOEVN ELSE 511]  ;                                  109100
INTEGER ARRAY SHEAD[0:100]  ;                                             109200
ALPHA ARRAY   MONTH[0:12],SREMARKS[0:4]  ;                                109300
ALPHA ARRAY   ITEMS[0:2]  ;                                               109400
SWITCH        SW1 ~ LL3,ERR11,ERR12,ERR13 ;                               109500
SWITCH        SW2 ~ LL4,ERR21,ERR22,ERR23 ;                               109600
SWITCH        SW3 ~ CODE1,LL1A,CODE3  ;                                   109700
FILE IN       CARDR(2,10)  ;                                              109800
FILE IN       TAPER(2,56,10)  ;                                           109900
SWITCH FILE   SWA ~ CARDR,TAPER  ;                                        110000
LIST          DATA1(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL],RUN,SDATE,  110100
                   RDATE,PERIOD),                                         110200
              DATA2(IKEY,I,J,OT,ML,PT,ASDAT,ASD,FOR LL ~ 1 STEP 1 UNTIL   110300
                     4 DO REMARKS[LL],CODE) ,                             110400
              OUTER41(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL]),         110500
              OUTERACT(I,J),                                              110600
              STREC(KEY,SI,SJ,RRIJ,ET,VAR,SASD,ADAYS,ES,LF,SL,SVAR,       110700
                   RANKI,TAL,FOR LL ~ 1 STEP 1 UNTIL 4 DO SREMARKS[LL]),  110800
              OUTH(PAGEN,FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL],RUN,   110900
                   DAS,MONTH[MOS],YRS,DAR,MONTH[MOR],YRR,PERIOD),         111000
              OUT1(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL],PAGEN),      111100
              OUT3(NOACT,MINEV,NOEVN,MAXEV)  ;                            111200
LIST          DATA3(FOR LL ~ 1 STEP 1 UNTIL 2 DO ITEMS[LL]),              111300
              DATA4(EVNO,FOR LL ~ 1 STEP 1 UNTIL 4 DO DSCPT[LL])  ;       111400
LIST          ENDEVENT(EVENT[KR,KC],TALLIES[KR,KC].[30:9]),               111500
              STARTEVENT(EVENT[KR,KC],TALLIES[KR,KC].[21:9]),             111600
              NET(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL]),             111700
              TALS(EVENT[KR,KC],TALLIES[KR,KC].[30:9],                    111800
                   TALLIES[KR,KC].[21:9],TALLIES[KR,KC].[39:9])  ;        111900
FORMAT IN     FIN1(13A6,X2/I10,2I7,X3,A5),                                112000
              FIN2(I6,2I10,3F7.1,I6,I1,4A6,I2) ;                          112100
FORMAT IN     FIN3(2A6),                                                  112200
              FIN4(I10,X5,4A6)  ;                                         112300
FORMAT OUT    FORMH(////////////////////"TIME P.E.R.T. - INPUT AND ",     112400
                   "AUDIT OF ACTIVITY CARDS",X61,"PAGE",I6///"NETWORK ",  112500
                   "NAME",X3,13A6,X10,"RUN",I10///"START DATE ",I3,X1,    112600
                   A3,I3,X4,"REPORT DATE ",I3,X1,A3,I3,X4,"UNITS OF ",    112700
                   "TIME ",A5),                                           112800
              FOUT1("NETWORK NAME",X3,13A6,X17,"PAGE",I6),                112900
              FOUT2(I6,2I11,3F8.1,I7,I4,X2,4A6,I4),                       113000
              FOUT3("NUMBER OF ACTIVITES",I7,X5,                          113100
                     "MINIMUM EVENT NUMBER",I11/                          113200
                   "NUMBER OF EVENTS",I10,X5,                             113300
                     "MAXIMUM EVENT NUMBER",I11),                         113400
              SPO3("ERRORS WERE DETECTED") ,                              113500
              FOUT4(I10,X5,4A6),                                          113600
              FORMH4("LISTING OF ACTIVITY CARDS"//X3,"KEY",X7,"PRED",X7,  113700
                   "SUCC     OT",X6,"ML",X6,"PT    DATE TYPE    ",        113800
                   "ACTIVITY DESCRIPTION   CODE"/),                       113900
              FORMH5("LISTING OF EVENT CARDS"//"EVENT NUMBER",X10,        114000
                   "EVENT DESCRIPTION"/),                                 114100
              TAPEERROR("EVENT DESCRIPTION CARDS ARE PRESENT ALTHOUGH ",  114200
                   "NO EVENT TAPE IS SPECIFIED"/"AN EVENT TAPE IS ",      114300
                   "ASSUMED"/),                                           114400
              UPDATEERROR("EVENT TAPE IS TO BE UPDATED ALTHOUGH ",        114500
                   "NETWORK CHANGES ARE NOT SPECIFIED"/"CHANGES ARE ",    114600
                   "ASSUMED"/)  ;                                         114700
FORMAT OUT    ENDERROR("ENDING EVENT",I10," HAS",I5," ACTIVITIES ",       114800
                   "LEADING INTO IT.  ONLY ONE IS PERMITTED."),           114900
              STARTERROR("STARTING EVENT",I10," HAS",I5," ACTIVITIES ",   115000
                   "LEADING FROM IT.  ONLY ONE IS PERMITTED."),           115100
              HEADNET(13A6),                                              115200
              HEADTALS(X21,"NO. PRED.      NO. SUCC.      NO. COM."/X10,  115300
                   "EVENT",X8,"EVENTS",X9,"EVENTS    PRED. EVENTS"),      115400
              OUTTALS(4I15),                                              115500
              STARTINGERROR("TOO FEW STARTING EVENTS WERE ESTIMATED"),    115600
              ENDINGERROR("TOO FEW ENDING EVENTS WERE ESTIMATED")  ;      115700
FORMAT OUT    ERROR1("PRED EVENT NO. OUT OF RANGE") ,                     115800
              ERROR2("SUCC EVENT NO. OUT OF RANGE") ,                     115900
              ERROR5("SUCC EVENT NO. EQUALS PRED EVENT NO.") ,            116000
              ERROR6("OT IS GREATER THAN ML , OT SET EQUAL TO ML") ,      116100
              ERROR7("ML IS GREATER THAN PT , PT SET EQUAL TO ML") ,      116200
              ERROR8("THE CORRECTED ACTIVITY RECORD IS AS FOLLOWS") ,     116300
              ERROR9("INVALID NETWORK START DATE") ,                      116400
              ERROR10("INVALID NETWORK REPORT DATE") ,                    116500
              ERROR11("REPORT DATE EARLIER THAN START DATE,SET EQUAL") ,  116600
              ERROR12("ACTUAL START DATE LATER THAN REPORT DATE ",        116700
                      ", SET EQUAL") ,                                    116800
              ERROR13("ACTUAL COMP. DATE LATER THAN REPORT DATE ",        116900
                      ", SET EQUAL") ,                                    117000
              ERROR14("DIRECTED START DATE EARLIER THAN REPORT DATE ",    117100
                      ", SET EQUAL") ,                                    117200
              ERROR15("SCHEDULE DATE EARLIER THAN REPORT DATE , SET" ,    117300
                      " EQUAL") ,                                         117400
              ERROR16("INVALID ACTUAL START DATE") ,                      117500
              ERROR17("INVALID COMPLETION DATE") ,                        117600
              ERROR18("INVALID DIRECTED START DATE") ,                    117700
              ERROR19("INVALID SCHEDULE DATE") ,                          117800
              ERROR20("ACTUAL START DATE EARLIER THAN NETWORK START ",    117900
                      "DATE , SET EQUAL") ,                               118000
              ERROR21("ACTUAL COMPLETION DATE EARLIER THAN NETWORK ",     118100
                      "START DATE, SET EQUAL") ,                          118200
              ERROR24("OT IS ZERO AND ML IS NON-ZERO , SET EQUAL") ,      118300
              ERROR25("NETWORK START DATE IS NOT A WORKING DAY") ,        118400
              ERROR26("NETWORK REPORT DATE IS NOT A WORKING DAY"),        118500
              ERROR27("ACTUAL START DATE NOT A WORKING DAY") ,            118600
              ERROR28("ACTUAL COMP. DATE NOT A WORKING DAY") ,            118700
              ERROR29("DIRECTED START DATE NOT A WORKING DAY") ,          118800
              ERROR30("SCHEDULE DATE NOT A WORKING DAY") ,                118900
              ERROR33("NETWORK START DATE IS NOT VALID OR A WORKING " ,   119000
                      "DAY") ,                                            119100
              ERROR34("REPORT DATE IS NOT VALID OR A WORKING DAY") ,      119200
              ERROR35("ACTUAL START DATE IS NOT VALID OR A WORKING DAY"   119300
                      ) ,                                                 119400
              ERROR36("ACTUAL COMP. DATE IS NOT VALID OR A WORKING ",     119500
                      "DAY") ,                                            119600
              ERROR37("DIRECTED START DATE IS NOT VALID OR A WORKING ",   119700
                      "DAY") ,                                            119800
              ERROR38("SCHEDULE DATE IS NOT VALID OR A WORKING DAY") ,    119900
              ERROR39("NO. OF ACTIVITIES EXCEEDS MAX NO. ALLOWED"),       120000
              ERROR40("NO. OF EVENTS EXCEEDS MAX NO. ALLOWED"),           120100
              ERROR41(13A6/"NETWORK NAME FOR UPDATE DIFFERENT FROM ",     120200
                   "NETWORK NAME ON TAPE.  UPDATE NOT PERFORMED"/),       120300
              ERROR42("NETWORK STARTING DATE FOR UPDATE DIFFERENT FROM",  120400
                   "STARTING DATE ON TAPE. TAPE STARTING DATE USED."),    120500
              ERROR43("CHANGES WERE MADE IN THE NETWORK ALTHOUGH ",       120600
                   "UNSPECIFIED."),                                       120700
              ERROR44("UPDATE ACTIVITIES OUT OF SEQUENCE. ACTIVITY ",     120800
                   "IGNORED"),                                            120900
              ERROR45("NO ACTIVITIES CARD BEFORE ACTIVITY DATA CARDS"),   121000
              ERROR46("ACTIVITY ",I10," TO",I10," NOT A STARTING ",       121100
                   "ACTIVITY, DIRECTED START DATE NOT PERMITTED"),        121200
              ERROR47("ACTIVITY ",I10," TO",I10," NOT AN ENDING ",        121300
                   "ACTIVITY, SCHEDULE DATE NOT PERMITTED")  ;            121400
PROCEDURE     UNPACK(MODAYR,SDAYS,WDPW,DPM,MO,DA,YR,DAYS,CMODAYR,SWER) ;  121500
COMMENT       THIS PROCEDURE UNPACKS DATE MODAYR INTO MONTH NO., DAY,     121600
              AND YEAR AND CALCULATES NO. OF WORKING DAYS FROM SDAYS  ;   121700
COMMENT                                                                   121800
              DEFINITION OF VARIABLES IN PROCEDURE UNPACK                 121900
              MODAYR    DATE TO BE UNPACKED                               122000
              SDAYS     NUMBER OF WORKING DAYS FROM 1/1/62 TO A GIVEN     122100
                        DATE                                              122200
              WDPW      NUMBER OF WORKING DAYS PER WEEK                   122300
              MO        MONTH NUMBER OF MODAYR                            122400
              DA        DAY OF THE MONTH OF MODAYR                        122500
              YR        YEAR OF MODAYR                                    122600
              DAYS      NUMBER OF WORKING DAYS FROM A GIVEN DATE UNTIL    122700
                        MODAYR                                            122800
              CMODAYR   NEAREST VALID CALEDAR DATE TO MODAYR              122900
              SWER      FLAG DENOTING TYPE OF ERROR IN DATE MODAYR        123000
              DPM       ARRAY CONTAINING NUMBER OF DAYS IN EACH MONTH     123100
              TEMP      NUMBER OF YEARS FROM NEAREST LEAP YEAR TO YR      123200
              YEARS     NUMBER OF YEARS FROM 62 TO YR                     123300
              TYEARS    NUMBER OF YEARS FROM 61 TO YR                     123400
              LYEARS    NUMBER OF LEAP YEARS FROM 61 TO YR                123500
              RYEARS    NUMBER OF NON-LEAP YEARS FROM 61 TO YR            123600
              YDAYS     NUMBER OF DAYS IN YEARS                           123700
              MDAYS     NUMBER OF DAYS FROM FIRST DAY OF YR UNTIL THE     123800
                        FIRST DAY OF MONTH MO                             123900
              WEEKS     NUMBER OF WHOLE WORKING WEEKS FROM 1/1/62 UNTIL   124000
                        MODAYR                                            124100
              REMDA     DAYS NOT INCLUDEDED IN WEEKS                      124200
              BOOL      BOOLEAN FLAG, TRUE IF ERROR IN DATE               124300
              I         INDEX OF ARRAY DPM                                124400
                                                                       ;  124500
VALUE         MODAYR,SDAYS,WDPW  ;                                        124600
INTEGER       MODAYR,SDAYS,WDPW,MO,DA,YR,DAYS,CMODAYR,SWER ;              124700
INTEGER ARRAY DPM[0]  ;                                                   124800
         BEGIN                                                            124900
INTEGER       TEMP,YEARS,TYEARS,LYEARS,RYEARS,YDAYS,MDAYS,I,WEEKS,        125000
              REMDA  ;                                                    125100
BOOLEAN       BOOL  ;                                                     125200
LABEL         L1,L2,L3,L4,L5,L6,L7  ;                                     125300
              BOOL ~ FALSE ;                                              125400
COMMENT       UNPACK MODAYR INTO MONTH NO, DAY, AND YEAR  ;               125500
              YR ~ MODAYR MOD 100  ;                                      125600
              SWER ~ 1 ;                                                  125700
              MO ~ MODAYR DIV 10000  ;                                    125800
              DA ~ MODAYR DIV 100 MOD 100  ;                              125900
COMMENT       CHECK VALIDITY OF DATE  ;                                   126000
              IF YR } 62 THEN GO TO L1  ;                                 126100
              YR ~ 62  ;  MO ~ DA ~ 1  ;                                  126200
              BOOL ~ TRUE  ;  GO TO L6  ;                                 126300
L1:           IF YR { 80 THEN GO TO L2  ;                                 126400
              YR ~ 80 ;  MO ~ 12 ; DA ~ 31 ;                              126500
              BOOL ~ TRUE  ;  GO TO L6  ;                                 126600
L2:           IF MO } 1 THEN GO TO L3  ;                                  126700
              MO ~ DA ~ 1  ;  BOOL ~ TRUE  ;  GO TO L6  ;                 126800
L3:           IF MO { 12 THEN GO TO L4  ;                                 126900
              MO ~ 12  ;  DA ~ 31  ;  BOOL ~ TRUE  ;  GO TO L6  ;         127000
L4:           IF DA } 1 THEN GO TO L5 ;                                   127100
              DA ~ 1  ;  BOOL ~ TRUE  ;  GO TO L6  ;                      127200
L5:           IF DA { DPM[MO] THEN GO TO L6  ;                            127300
              TEMP ~ YR MOD 4  ;                                          127400
              IF (MO = 2) AND (TEMP = 0) AND (DA = 29) THEN GO TO L6  ;   127500
              DA ~ DPM[MO]  ;  BOOL ~ TRUE  ;                             127600
L6:           YEARS ~ YR - 62  ;                                          127700
COMMENT       CALCULATE NO. OF WORKING DAYS SINCE SDAYS  ;                127800
              IF (YEARS = 0) AND (MO = 1) THEN                            127900
         BEGIN                                                            128000
              DAYS ~ DA  ;  GO TO L7                                      128100
         END  ;                                                           128200
              TYEARS ~ YEARS + 1  ;                                       128300
              LYEARS ~ TYEARS DIV 4 ;                                     128400
              RYEARS ~ YEARS - LYEARS  ;                                  128500
              YDAYS ~ (365 | RYEARS)+(366 | LYEARS) ;                     128600
              MDAYS ~ 0  ;                                                128700
              FOR I ~ 1 STEP 1 UNTIL MO-1 DO MDAYS ~ MDAYS + DPM[I] ;     128800
              IF (YR MOD 4 = 0) AND (MO > 2) THEN MDAYS ~ MDAYS + 1  ;    128900
              DAYS ~ YDAYS + MDAYS + DA  ;                                129000
L7:           CMODAYR ~ 0  ;                                              129100
              WEEKS ~ DAYS DIV 7  ;                                       129200
              REMDA ~ DAYS - (WEEKS | 7)  ;                               129300
              DAYS ~ (WEEKS | WDPW) + REMDA - SDAYS  ;                    129400
              IF BOOL THEN CMODAYR ~ MO | 10000 + DA | 100 + YR  ;        129500
COMMENT       CHECK THAT DATE IS A WORKING DAY  ;                         129600
              IF ( ( WDPW ! 7) AND((REMDA > WDPW) OR (REMDA = 0)))        129700
                 THEN                                                     129800
         BEGIN                                                            129900
              IF BOOL THEN SWER ~ 4 ELSE SWER ~ 3                         130000
         END  ;                                                           130100
              IF BOOL THEN SWER ~ 2                                       130200
         END  UNPACK  ;                                                   130300
PROCEDURE     RENUMBER  ;                                                 130400
COMMENT       THIS PROCEDURE RENUMBERS EVENTS  ;                          130500
COMMENT                                                                   130600
              DEFINITIONS OF VARIABLES USED IN THE PROCEDURE RENUMBER     130700
              SCRAMBLEINDEX  INDEX FOR ARRAY SHEAD                        130800
              M,MM       TEMPORARY LOCAL VARIABLES                        130900
              LIFO       BOOLEAN VARIABLE, TRUE IF EVENT NUMBERS ARE TO   131000
                         BE CHECKED FROM LAST TO FIRST                    131100
              THIS PROCEDURE USES THE FOLLOWING VARIABLES DECLARED IN     131200
              OUTER BLOCKS                                                131300
                         EVENT,MAXNOEVN,NOEVN,TALLIES,SHEAD               131400
                        EVNO,EVENTTAPE,RN                                 131500
                                                                       ;  131600
         BEGIN                                                            131700
INTEGER       SCRAMBLEINDEX,M,MM  ;                                       131800
BOOLEAN       LIFO  ;                                                     131900
LABEL         RETEST,RETURN  ;                                            132000
DEFINE        I = M.[30:9],M.[39:9]#,                                     132100
              II = MM.[30:9],MM.[39:9]#,                                  132200
              DEFTALI = TALLIES[I].[3:18]#,                               132300
              DEFTALII = TALLIES[II].[3:18]#  ;                           132400
              LIFO ~ TRUE  ;                                              132500
              SCRAMBLEINDEX ~ (EVNO.[42:6] + EVNO.[36:6] + EVNO.[30:6] +  132600
                   EVNO.[24:6] + EVNO.[18:6] + EVNO.[12:6] + EVNO.[6:6])  132700
                   MOD 100  ;                                             132800
              M ~ SHEAD[SCRAMBLEINDEX]  ;                                 132900
              IF M = 0 THEN                                               133000
         BEGIN                                                            133100
              IF NOEVN = MAXNOEVN THEN                                    133200
         BEGIN                                                            133300
              WRITE(PRINTER,ERROR40)  ;                                   133400
              GO TO FINISH  ;                                             133500
         END  ;                                                           133600
              RN ~ M ~ NOEVN ~ NOEVN + 1  ;                               133700
              SHEAD[SCRAMBLEINDEX] ~ M  ;                                 133800
              EVENT[I] ~ EVNO  ;                                          133900
              IF EVENTTAPE THEN WRITE(SWF[NE],*,EREC)  ;                  134000
              GO TO RETURN  ;                                             134100
         END  ;                                                           134200
RETEST:       IF EVENT[I] = EVNO THEN                                     134300
         BEGIN                                                            134400
              RN ~ M  ;  GO TO RETURN  ;                                  134500
         END  ;                                                           134600
              MM ~ M  ;                                                   134700
              M ~ DEFTALI  ;                                              134800
              IF M ! 0 THEN GO TO RETEST  ;                               134900
              IF NOEVN = MAXNOEVN THEN                                    135000
         BEGIN                                                            135100
              WRITE(PRINTER,ERROR40)  ;                                   135200
              GO TO FINISH  ;                                             135300
         END  ;                                                           135400
              RN ~ M ~ NOEVN ~ NOEVN + 1  ;                               135500
              EVENT[I] ~ EVNO  ;                                          135600
              IF EVENTTAPE THEN WRITE(SWF[NE],*,EREC)  ;                  135700
              IF LIFO THEN                                                135800
         BEGIN                                                            135900
              DEFTALI ~ SHEAD[SCRAMBLEINDEX]  ;                           136000
              SHEAD[SCRAMBLEINDEX] ~ M  ;                                 136100
         END                                                              136200
              ELSE DEFTALII ~ M  ;                                        136300
RETURN:  END  RENUMBER  ;                                                 136400
PROCEDURE     CARDAUDIT(I,J,ASD,REMARKS)  ;                               136500
COMMENT       THIS PROCEDURE AUDITS ACTIVITY DATA  ;                      136600
COMMENT                                                                   136700
              DEFINITION OF VARIABLES IN PROCEDURE CARDAUDIT              136800
              I         PREDECESSOR EVENT NUMBER                          136900
              J         SUCCESSOR EVENT NUMBER                            137000
              ASD       FLAG DENOTING TYPE OF ACTIVITY DATE               137100
              REMARKS   ARRAY CONTAINING ACTIVITY DESCRIPTION             137200
              THIS PROCEDURE USES THE FOLLOWING VARIABLES DECLARED IN     137300
              OUTER BLOCKS                                                137400
                        KEY,BOOL4,UPDATE,RIRJ,EVNO,RN,SDAYS,DPM,SWER      137500
                        RRIJ,BTAPE,NINES,BOOL2,MINEV,MAXEV,ADAYS          137600
                        ONETIME,OT,PT,ML,ET,VAR,RDAYS,RDATE,DAA,DAR,MOA,  137700
                        MOR,YRA,YRR,ASDAT,CASDAT,SDATE,DAS,MOS,YRS,WDPW,  137800
                        NOACT,WEEKFLAG,TALLIES                            137900
                                                                       ;  138000
VALUE         I,J,ASD  ;                                                  138100
INTEGER   I,J,ASD;                                                        138200
ALPHA ARRAY   REMARKS[0]  ;                                               138300
         BEGIN                                                            138400
LABEL         L1,L2,L3,L5,L7,LL3,LL6,LL7,LL8,LL9,LL10,LL11,LL12,LL13,     138500
              LL14,LL15,LL16,LL17,LL18,LL19,LL20,LL21,LL22,LL23,ERR4,     138600
              ERR5,ERR6,ERR7,ERR31,ERR32,ERR33,EER,L6,L8  ;               138700
LABEL         L9  ;                                                       138800
SWITCH        SW1 ~ LL8,LL9,LL10,LL11 ;                                   138900
SWITCH        SW2 ~ ERR4,ERR5,ERR6,ERR7  ;                                139000
SWITCH        SW3 ~ LL16,LL17,LL18,LL19  ;                                139100
SWITCH        SW4 ~ LL20,LL21,LL22,LL23  ;                                139200
SWITCH        SW5 ~ LL6,ERR31,ERR32,ERR33 ;                               139300
DEFINE        DKEY = KEY.[30:9],KEY.[39:9]# ;                             139400
DEFINE        CI = TALLIES[RRIJ.[8:9],RRIJ.[17:9]].[21:9]#,               139500
              CJ = TALLIES[RRIJ.[30:9],RRIJ.[39:9]].[30:9]#,              139600
              AT = TALLIES[RRIJ.[30:9],RRIJ.[39:9]].[39:9]#  ;            139700
              BOOL4 ~ FALSE ;                                             139800
COMMENT       RENUMBER PREDECESSOR AND SUCCESSOR EVENTS AND COMPUTE       139900
              TALLIES  ;                                                  140000
              IF UPDATE ! 1 THEN                                          140100
         BEGIN                                                            140200
              EVNO ~ I  ;                                                 140300
              RENUMBER  ;                                                 140400
              RRIJ.[4:22] ~ RN.[26:22]  ;                                 140500
         END  ;                                                           140600
              CI ~ CI + 1  ;                                              140700
              IF UPDATE ! 1 THEN                                          140800
         BEGIN                                                            140900
              EVNO ~ J  ;                                                 141000
              RENUMBER  ;                                                 141100
              RRIJ.[26:22] ~ RN.[26:22]  ;                                141200
         END  ;                                                           141300
              CJ ~ CJ + 1  ;                                              141400
              IF ASD = 2 THEN AT ~ AT + 1  ;                              141500
              IF UPDATE = 1 THEN GO TO L6  ;                              141600
              RIRJ[DKEY] ~ RRIJ  ;                                        141700
COMMENT       AUDIT EVENT NUMBERS  ;                                      141800
              IF I > 0 AND I < NINES THEN GO TO L1  ;                     141900
              WRITE(PRINTER[DBL],ERROR1)  ;                               142000
              BOOL2 ~ TRUE  ;                                             142100
L1:           IF J > 0 AND J < NINES THEN GO TO L2 ;                      142200
              WRITE(PRINTER[DBL],ERROR2)  ;                               142300
              BOOL2 ~ TRUE  ;                                             142400
L2:           IF I ! J THEN GO TO L5 ;                                    142500
              WRITE(PRINTER[DBL],ERROR5)  ;                               142600
              BOOL2 ~ TRUE ;                                              142700
COMMENT       CALCULATE MIN. AND MAX. EVENT NOS.  ;                       142800
L5:           IF I < MINEV THEN MINEV ~ I  ;                              142900
              IF J < MINEV THEN MINEV ~ J  ;                              143000
              IF I > MAXEV THEN MAXEV ~ I  ;                              143100
              IF J > MAXEV THEN MAXEV ~ J  ;                              143200
L6:           IF BTAPE THEN GO TO LL7  ;                                  143300
COMMENT       AUDIT ACTIVITY TIMES  ;                                     143400
              IF ONETIME THEN                                             143500
         BEGIN                                                            143600
              ET ~ ML  ;  VAR ~ 0  ;                                      143700
              GO TO L3  ;                                                 143800
         END  ;                                                           143900
              IF ML > PT THEN                                             144000
         BEGIN                                                            144100
              WRITE(PRINTER[DBL],ERROR7)  ;                               144200
              BOOL4 ~ BOOL2 ~ TRUE  ;                                     144300
              PT ~ ML                                                     144400
         END  ;                                                           144500
              IF OT > ML THEN                                             144600
         BEGIN                                                            144700
              WRITE(PRINTER[DBL],ERROR6)  ;                               144800
              BOOL4 ~ BOOL2 ~ TRUE ;                                      144900
              OT ~ ML                                                     145000
         END                                                              145100
              ELSE IF (OT = 0) AND (ML ! 0) THEN                          145200
         BEGIN                                                            145300
              WRITE(PRINTER[DBL],ERROR24)  ;                              145400
              BOOL4 ~ BOOL2 ~ TRUE  ;                                     145500
              OT ~ ML                                                     145600
         END  ;                                                           145700
COMMENT       COMPUTE EXPECTED TIME OF ACTIVITY ;                         145800
              ET ~ (OT + 4 | ML + PT)/6.0  ;                              145900
              VAR ~ ((PT - OT)/6.0)*2  ;                                  146000
L3:           ADAYS ~ 0  ;                                                146100
              IF ASD = 0 THEN GO TO LL7 ;                                 146200
COMMENT       AUDIT ACTIVITY DATES  ;                                     146300
              UNPACK(ASDAT,SDAYS,WDPW,DPM,MOA,DAA,YRA,ADAYS,CASDAT,       146400
                   SWER ) ;                                               146500
              GO TO SW5[SWER] ;                                           146600
LL6:          GO TO SW1[ASD]  ;                                           146700
LL8:          IF ADAYS { RDAYS THEN GO TO LL13 ;                          146800
              WRITE(PRINTER[DBL],ERROR12)  ;                              146900
              GO TO LL12 ;                                                147000
LL9:          IF ADAYS { RDAYS THEN GO TO LL14 ;                          147100
              WRITE(PRINTER[DBL],ERROR13)  ;                              147200
              GO TO LL12 ;                                                147300
LL10:         IF ADAYS } RDAYS THEN GO TO LL7 ;                           147400
              WRITE(PRINTER[DBL],ERROR14)  ;                              147500
              GO TO LL12 ;                                                147600
LL11:         IF ADAYS } RDAYS THEN GO TO LL7 ;                           147700
              WRITE(PRINTER[DBL],ERROR15)  ;                              147800
              GO TO LL12  ;                                               147900
LL12:         ASDAT ~ RDATE  ;  BOOL2 ~ TRUE  ;                           148000
              ADAYS ~ RDAYS  ;                                            148100
              DAA ~ DAR ; MOA ~ MOR ; YRA ~ YRR ; GO TO LL7 ;             148200
LL13:         IF ADAYS } 0 THEN GO TO LL7 ;                               148300
              WRITE(PRINTER[DBL],ERROR20)  ;                              148400
              GO TO LL15 ;                                                148500
LL14:         IF ADAYS } 0 THEN GO TO LL7  ;                              148600
              WRITE(PRINTER[DBL],ERROR21)  ;                              148700
              GO TO LL15 ;                                                148800
ERR31:        ASDAT ~ CASDAT ; BOOL2 ~ TRUE ;                             148900
              GO TO SW2[ASD] ;                                            149000
ERR4:         WRITE(PRINTER[DBL],ERROR16)  ;  GO TO LL8  ;                149100
ERR5:         WRITE(PRINTER[DBL],ERROR17)  ;  GO TO LL9  ;                149200
ERR6:         WRITE(PRINTER[DBL],ERROR18)  ;  GO TO LL10  ;               149300
ERR7:         WRITE(PRINTER[DBL],ERROR19)  ;  GO TO LL11  ;               149400
ERR32:        BOOL2 ~ TRUE  ;                                             149500
              GO TO SW3[ASD]  ;                                           149600
LL16:         WRITE(PRINTER[DBL],ERROR27)  ;  GO TO LL8  ;                149700
LL17:         WRITE(PRINTER[DBL],ERROR28)  ;  GO TO LL9  ;                149800
LL18:         WRITE(PRINTER[DBL],ERROR29)  ;  GO TO LL10  ;               149900
LL19:         WRITE(PRINTER[DBL],ERROR30)  ;  GO TO LL11  ;               150000
ERR33:        ASDAT ~ CASDAT ; BOOL2 ~ TRUE ;                             150100
              GO TO SW4[ASD]  ;                                           150200
LL20:         WRITE(PRINTER[DBL],ERROR35)  ;  GO TO LL8  ;                150300
LL21:         WRITE(PRINTER[DBL],ERROR36)  ;  GO TO LL9  ;                150400
LL22:         WRITE(PRINTER[DBL],ERROR37)  ;  GO TO LL10  ;               150500
LL23:         WRITE(PRINTER[DBL],ERROR38)  ;  GO TO LL10  ;               150600
LL15:         ASDAT ~ SDATE ; BOOL2 ~ TRUE  ; ADAYS ~ 0 ;                 150700
              DAA ~ DAS ; MOA ~ MOS ; YRA ~ YRS ;                         150800
LL7:          NOACT ~ NOACT + 1  ;                                        150900
L8:           IF BOOL4 THEN                                               151000
         BEGIN                                                            151100
              WRITE(PRINTER[DBL],ERROR8)  ;                               151200
              IF WEEKFLAG THEN                                            151300
         BEGIN                                                            151400
              ML ~ ML / WDPW  ;  PT ~ PT / WDPW  ;  OT ~ OT / WDPW        151500
         END  ;                                                           151600
              WRITE(PRINTER[DBL],FOUT2,DATA2)  ;                          151700
         END  ;                                                           151800
L9:           IF BTAPE THEN WRITE(SWF[WR],*,STREC)                        151900
              ELSE WRITE(SWF[WR],*,TREC)  ;                               152000
         END  CARDAUDIT  ;                                                152100
START:        NOACT ~ KEY ~ 0  ;                                          152200
              DELETEKEYS ~ 0  ;                                           152300
              WEEKFLAG ~ FALSE  ;                                         152400
              FILL MONTH[*] WITH " ","JAN","FEB","MAR","APR","MAY",       152500
                   "JUN","JUL","AUG","SEP","OCT","NOV","DEC" ;            152600
              FILL DPM[*] WITH 0,31,28,31,30,31,30,31,31,30,31,30,31  ;   152700
              NINES ~ 9999999999  ;                                       152800
              IF UPDATE = 0 THEN                                          152900
         BEGIN                                                            153000
                   RD ~ 2  ;  WR ~ 1  ;  SC ~ 3  ;  RE ~ 4  ;             153100
              NE ~ 5  ;                                                   153200
         END  ;                                                           153300
              IF (BLOCPARAM[NOBLOCK].[45:3] ! 1) THEN NE ~ RE  ;          153400
              IF (BLOCPARAM[NOBLOCK].[40:3] = 1) THEN AC ~ 1              153500
                   ELSE AC ~ 0  ;                                         153600
COMMENT       INITIALIZE GLOBAL ARRAYS FOR INPUT AUDIT RUN  ;             153700
              IF UPDATE ! 1 THEN                                          153800
         BEGIN                                                            153900
              NOEVN ~ MAXEV ~ 0  ;                                        154000
              IF MAXNOEVN.[30:9] = 0 THEN                                 154100
         BEGIN                                                            154200
              TEMP1 ~ MAXNOEVN.[39:9]  ;                                  154300
              FOR L ~ 0 STEP 1 UNTIL TEMP1 DO EVENT[0,L] ~ 0              154400
         END  ELSE                                                        154500
         BEGIN                                                            154600
              TEMP1 ~ MAXNOEVN.[30:9]  ;                                  154700
              FOR L ~ 0 STEP 1 UNTIL TEMP1 DO                             154800
              FOR K ~ 0 STEP 1 UNTIL 511 DO                               154900
              EVENT[L,K] ~ 0                                              155000
         END ;                                                            155100
              IF MAXNOACT.[30:9] = 0 THEN                                 155200
         BEGIN                                                            155300
              TEMP1 ~ MAXNOACT.[39:9] ;                                   155400
              FOR L ~ 0 STEP 1 UNTIL TEMP1 DO                             155500
              RIRJ[0,L] ~ 0  ;                                            155600
         END  ELSE                                                        155700
         BEGIN                                                            155800
              TEMP1 ~ MAXNOACT.[30:9] ;                                   155900
              FOR L ~ 0 STEP 1 UNTIL TEMP1 DO                             156000
              FOR K ~ 0 STEP 1 UNTIL 511 DO                               156100
              RIRJ[0,L] ~ 0  ;                                            156200
         END  ;                                                           156300
              MINEV ~ NINES  ;                                            156400
         END  ;                                                           156500
              BOOL2 ~ BOOL4 ~ FALSE ;                                     156600
              BTAPE ~ FALSE  ;                                            156700
              WRITE(PRINTER[PAGE])  ;                                     156800
              PAGEN ~ 1  ;                                                156900
              TLINE ~ NLINE ~ 24  ;                                       157000
COMMENT       READ HEADER CARDS  ;                                        157100
              READ(SWA[AC],FIN1,DATA1)  ;                                 157200
              IF (PERIOD = "WEEKS") THEN WEEKFLAG ~ TRUE  ;               157300
              IF UPDATE ! 0 THEN                                          157400
         BEGIN                                                            157500
              FOR LL ~ 1 STEP 1 UNTIL 13 DO                               157600
              IF (NETNAME[LL] ! SNETNAME[LL]) THEN                        157700
         BEGIN                                                            157800
              WRITE(PRINTER[DBL],ERROR41)  ;                              157900
              GO TO ENDOFPROG  ;                                          158000
         END  ;                                                           158100
              IF (SDATE ! SSDATE) THEN                                    158200
         BEGIN                                                            158300
              WRITE(PRINTER[DBL],ERROR42)  ;                              158400
              BOOL2 ~ TRUE  ;                                             158500
              SDATE ~ SSDATE                                              158600
         END                                                              158700
         END  ;                                                           158800
COMMENT       AUDIT NETWORK START DATE  ;                                 158900
              UNPACK(SDATE,0,WDPW,DPM,MOS,DAS,YRS,SDAYS,CSDATE,SWER) ;    159000
              GO TO SW1[SWER] ;                                           159100
ERR11:        WRITE(PRINTER[DBL],ERROR9)  ;                               159200
              SDATE ~ CSDATE ; BOOL2 ~ TRUE ; GO TO LL3 ;                 159300
ERR12:        WRITE(PRINTER[DBL],ERROR25)  ;  BOOL2 ~ TRUE ; GO TO LL3 ;  159400
ERR13:        WRITE(PRINTER[DBL],ERROR33)  ;  BOOL2 ~ TRUE  ;             159500
              SDATE ~ CSDATE  ;                                           159600
COMMENT       AUDIT NETWORK REPORT DATE  ;                                159700
LL3:          UNPACK(RDATE,SDAYS,WDPW,DPM,MOR,DAR,YRR,RDAYS,CRDATE,       159800
                   SWER ) ;                                               159900
              GO TO SW2[SWER] ;                                           160000
ERR21:        WRITE(PRINTER[DBL],ERROR10)  ;                              160100
              RDATE ~ CRDATE ; BOOL2 ~ TRUE ; GO TO LL4 ;                 160200
ERR22:        WRITE(PRINTER[DBL],ERROR26)  ;  BOOL2 ~ TRUE ; GO TO LL4 ;  160300
ERR23:        WRITE(PRINTER[DBL],ERROR34)  ;  BOOL2 ~ TRUE  ;             160400
              RDATE ~ CRDATE  ;                                           160500
LL4:          IF RDAYS } 0 THEN GO TO LL5  ;                              160600
              RDAYS ~ 0 ; BOOL2 ~ TRUE ;                                  160700
              MOR ~ MOS ; DAR ~ DAS ; YRR ~ YRS ;                         160800
              WRITE(PRINTER[DBL],ERROR11)  ;                              160900
COMMENT       PRINT TITLE PAGE FOR DATA LISTING  ;                        161000
LL5:          WRITE(PRINTER,FORMH,OUTH)  ;                                161100
              RRIJ ~ ET ~ VAR ~ ASD ~ ADAYS ~ ES ~ LF ~ SL ~ SVAR ~       161200
                   RANKI ~ TAL ~ RN ~ EVNO ~ RANKE ~ EEO ~EVAR ~ LEO      161300
                   ~ SLCK ~ TALLY ~ 0  ;                                  161400
              FOR L ~ 1 STEP 1 UNTIL 4 DO DSCPT[L] ~ "      "  ;          161500
COMMENT       READ DATA TYPE CARD  ;                                      161600
              READ(SWA[AC],FIN3,DATA3)  ;                                 161700
              IF (ITEMS[1] = "EVENTS") THEN                               161800
         BEGIN                                                            161900
              IF NOT EVENTTAPE THEN                                       162000
         BEGIN                                                            162100
              WRITE(PRINTER,TAPEERROR)  ;                                 162200
              EVENTTAPE ~ BOOL2 ~ TRUE  ;                                 162300
         END  ;                                                           162400
              IF (UPDATE = 1) THEN                                        162500
         BEGIN                                                            162600
              WRITE(PRINTER,UPDATEERROR)  ;                               162700
              UPDATE ~ 2  ;                                               162800
              BOOL2 ~ TRUE  ;                                             162900
         END  ;                                                           163000
COMMENT       READ EVENT CARDS  ;                                         163100
LOOP2:        READ(SWA[AC],FIN4,DATA4)[LL2B]  ;                           163200
              IF (EVNO = NINES) THEN GO TO LL5A  ;                        163300
              IF (NLINE = TLINE) THEN                                     163400
         BEGIN                                                            163500
COMMENT       PRINT HEADING  ;                                            163600
              PAGEN ~ PAGEN + 1  ;  NLINE ~ 0  ;                          163700
              WRITE(PRINTER[PAGE])  ;                                     163800
              WRITE(PRINTER[DBL],FOUT1,OUT1)  ;                           163900
              WRITE(PRINTER,FORMH5)  ;                                    164000
         END  ;                                                           164100
COMMENT       LIST EVENT  ;                                               164200
              WRITE(PRINTER[DBL],FOUT4,DATA4)  ;                          164300
              NLINE ~ NLINE + 1  ;                                        164400
COMMENT       RENUMBER EVENT  ;                                           164500
              RENUMBER  ;                                                 164600
              GO TO LOOP2  ;                                              164700
LL5A:         NLINE ~ TLINE  ;                                            164800
              READ(SWA[AC],FIN3,DATA3)[LL2B]  ;                           164900
         END  ;                                                           165000
              TAPEKEY ~ 1  ;                                              165100
              IF (ITEMS[1] ! "ACTIVI") THEN                               165200
         BEGIN                                                            165300
              WRITE(PRINTER[DBL],ERROR45)  ;                              165400
              GO TO FINISH  ;                                             165500
         END  ;                                                           165600
COMMENT       READ ACTIVITY CARDS  ;                                      165700
LL1:          READ(SWA[AC],FIN2,DATA2)[LL2B]  ;                           165800
              IF NLINE = TLINE THEN                                       165900
         BEGIN                                                            166000
COMMENT       PRINT HEADING  ;                                            166100
              PAGEN ~ PAGEN + 1  ;  NLINE ~ 0  ;                          166200
              WRITE(PRINTER[PAGE])  ;  WRITE(PRINTER[DBL],FOUT1,OUT1)  ;  166300
              WRITE(PRINTER,FORMH4)  ;                                    166400
         END  ;                                                           166500
COMMENT       PRINT CARD IMAGE  ;                                         166600
              WRITE(PRINTER[DBL],FOUT2,DATA2)  ;                          166700
              NLINE ~ NLINE + 1  ;                                        166800
              IF UPDATE = 0 THEN GO TO LL1A  ;                            166900
              IF (IKEY < TAPEKEY) THEN                                    167000
         BEGIN                                                            167100
              BOOL2 ~ TRUE  ;                                             167200
              WRITE(PRINTER[DBL],ERROR44)  ;  GO TO LL1  ;                167300
         END  ;                                                           167400
              IF (IKEY > SNOACT) AND (UPDATE = 1) THEN                    167500
         BEGIN                                                            167600
              WRITE(PRINTER[DBL],ERROR43)  ;                              167700
              BOOL2 ~ TRUE  ;                                             167800
              UPDATE ~ 2  ;  CODE ~ 2  ;                                  167900
         END  ;                                                           168000
              IF KEY + DELETEKEYS > SNOACT THEN GO TO LL1A  ;             168100
              BTAPE ~ TRUE  ;                                             168200
              FOR TKEY ~ TAPEKEY STEP 1 UNTIL IKEY-1 DO                   168300
         BEGIN                                                            168400
              READ(SWF[RD],*,STREC)[LL1B]  ;                              168500
              KEY ~ KEY - DELETEKEYS  ;                                   168600
              CARDAUDIT(SI,SJ,SASD,SREMARKS)  ;                           168700
         END  ;                                                           168800
              BTAPE ~ FALSE  ;                                            168900
              READ(SWF[RD],*,STREC)[LL1B]  ;                              169000
              TAPEKEY ~ IKEY + 1  ;                                       169100
              IF (CODE ! 1) AND (UPDATE = 1) THEN                         169200
         BEGIN                                                            169300
              WRITE(PRINTER,ERROR43)  ;  UPDATE ~ 2  ;                    169400
              BOOL2 ~ TRUE  ;                                             169500
         END  ;                                                           169600
              GO TO SW3[CODE]  ;                                          169700
CODE3:        DELETEKEYS ~ DELETEKEYS + 1  ;                              169800
              KEY ~ KEY - DELETEKEYS  ;                                   169900
              GO TO LL1  ;                                                170000
CODE1:        IF SI ! I OR SJ ! J THEN                                    170100
         BEGIN                                                            170200
              BOOL2 ~ TRUE  ;                                             170300
              WRITE(PRINTER[DBL],ERROR43)  ;                              170400
              KEY ~ KEY - DELETEKEYS  ;                                   170500
              BTAPE ~ TRUE  ;                                             170600
              CARDAUDIT(SI,SJ,SASD,SREMARKS)  ;                           170700
              BTAPE ~ FALSE  ;                                            170800
              GO TO LL1  ;                                                170900
         END  ;                                                           171000
LL1B:         BTAPE ~ FALSE  ;                                            171100
COMMENT       AUDIT ACTIVITY CARDS  ;                                     171200
LL1A:         IF (UPDATE = 0) OR (IKEY > SNOACT) THEN                     171300
         BEGIN                                                            171400
              KEY ~ KEY + 1  ;                                            171500
              IF KEY > MAXNOACT THEN                                      171600
         BEGIN                                                            171700
              WRITE(PRINTER[DBL],ERROR39)  ;  GO TO FINISH  ;             171800
         END                                                              171900
         END                                                              172000
              ELSE KEY ~ KEY - DELETEKEYS  ;                              172100
COMMENT       CHECK PERIOD AND CONVERT TO DAYS ;                          172200
              IF WEEKFLAG THEN                                            172300
         BEGIN                                                            172400
              ML ~ ML | WDPW  ;  PT ~ PT | WDPW  ;  OT ~ OT | WDPW        172500
         END  ;                                                           172600
              CARDAUDIT(I,J,ASD,REMARKS)  ;                               172700
              GO TO LL1 ;                                                 172800
LL2B:         IF (AC = 1) THEN                                            172900
         BEGIN                                                            173000
              LOCK(TAPER,RELEASE)  ;  TYPEOUT(SPOOUT,INST[5,*])  ;        173100
         END  ;                                                           173200
              IF (UPDATE = 0) OR (KEY + DELETEKEYS } SNOACT)              173300
              THEN GO TO LL2  ;                                           173400
              BTAPE ~ TRUE  ;                                             173500
              FOR TKEY ~ TAPEKEY STEP 1 UNTIL SNOACT DO                   173600
         BEGIN                                                            173700
              READ(SWF[RD],*,STREC)[LL2]  ;                               173800
              KEY ~ KEY - DELETEKEYS  ;                                   173900
              CARDAUDIT(SI,SJ,SASD,SREMARKS)  ;                           174000
         END  ;                                                           174100
COMMENT       LIST NUMBER OF ACTIVITIES AND EVENTS  ;                     174200
LL2:          PAGEN ~ PAGEN + 1  ;                                        174300
              WRITE(PRINTER[PAGE])  ;  WRITE(PRINTER[DBL],FOUT1,OUT1)  ;  174400
              WRITE(PRINTER[DBL],FOUT3,OUT3)  ;                           174500
              REWIND(SWF[WR])  ;                                          174600
              IF EVENTTAPE THEN REWIND(SWF[NE])  ;                        174700
              IF (UPDATE ! 0) THEN IF SAVETAPES THEN                      174800
         BEGIN                                                            174900
              LOCK(SWF[RD],SAVE)  ;                                       175000
              INST[2,0] ~ FILENAMES[RD]  ;                                175100
              TYPEOUT(SPOOUT,INST[2,*])  ;  TYPEOUT(SPOOUT,INST[5,*])  ;  175200
         END                                                              175300
              ELSE REWIND(SWF[RD])  ;                                     175400
              TP ~ RD  ;  RD ~ WR  ;  WR ~ TP  ;                          175500
              IF UPDATE = 1 THEN GO TO LL2A  ;                            175600
              NOACTROW ~ NOACT.[30:9] ;                                   175700
              NOACTCOL ~ IF NOACTROW = 0 THEN NOACT.[39:9] ELSE 511 ;     175800
              NOEVNROW ~ NOEVN.[30:9] ;                                   175900
              NOEVNCOL ~ IF NOEVNROW = 0 THEN NOEVN.[39:9] ELSE 511 ;     176000
LL2A:         EK ~ BK ~ 0  ;                                              176100
              IF LISTTAL THEN                                             176200
         BEGIN                                                            176300
COMMENT       PRINT HEADING FOR LIST TALLIES  ;                           176400
              WRITE(PRINTER[PAGE])  ;                                     176500
              WRITE(PRINTER[DBL],HEADNET,NET)  ;                          176600
              WRITE(PRINTER,HEADTALS)  ;                                  176700
              WRITE(PRINTER[DBL])  ;                                      176800
         END  ;                                                           176900
              FOR K ~ 1 STEP 1 UNTIL NOEVN DO                             177000
         BEGIN                                                            177100
              KR ~ K.[30:9]  ;  KC ~ K.[39:9]  ;                          177200
COMMENT       LIST EVENT TALLIES  ;                                       177300
              IF LISTTAL THEN WRITE(PRINTER[DBL],OUTTALS,TALS)  ;         177400
COMMENT       DETERMINE STARTING AND ENDING EVENTS  ;                     177500
              IF TALLIES[KR,KC].[21:9] = 0 THEN                           177600
         BEGIN                                                            177700
              IF TALLIES[KR,KC].[30:9] ! 1 THEN                           177800
         BEGIN                                                            177900
              WRITE(PRINTER[DBL],ENDERROR,ENDEVENT)  ;                    178000
              BOOL2 ~ TRUE  ;                                             178100
         END  ;                                                           178200
              IF EK = NUMENDING THEN                                      178300
         BEGIN                                                            178400
              WRITE(PRINTER[DBL],ENDINGERROR)  ;                          178500
              BOOL2 ~ TRUE  ;  GO TO LL7  ;                               178600
         END  ;                                                           178700
              EK ~ EK + 1  ;                                              178800
              REEV[EK] ~ K  ;                                             178900
              GO TO LL6  ;                                                179000
         END  ;                                                           179100
              IF TALLIES[KR,KC].[30:9] = 0 THEN                           179200
         BEGIN                                                            179300
              IF TALLIES[KR,KC].[21:9] ! 1 THEN                           179400
         BEGIN                                                            179500
              WRITE(PRINTER[DBL],STARTERROR,STARTEVENT)  ;                179600
              BOOL2 ~ TRUE  ;                                             179700
         END  ;                                                           179800
              IF BK = NUMSTARTING THEN                                    179900
         BEGIN                                                            180000
              WRITE(PRINTER[DBL],STARTINGERROR)  ;                        180100
              BOOL2 ~ TRUE  ;  GO TO LL7  ;                               180200
         END  ;                                                           180300
              BK ~ BK + 1  ;                                              180400
              RBEV[BK] ~ K  ;                                             180500
         END  ;                                                           180600
LL6:     END  ;                                                           180700
LL7:     BEGIN                                                            180800
COMMENT       BLOCK TO LIST ENDING AND STARTING EVENTS  ;                 180900
LIST          OUTHEAD(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL],RUN),     181000
              OUT1(BK,FOR LL ~ 1 STEP 1 UNTIL BK DO EVENT[RBEV[LL]        181100
                   .[30:9],RBEV[LL].[39:9]]),                             181200
              OUT2(EK,FOR LL ~ 1 STEP 1 UNTIL EK DO EVENT[REEV[LL]        181300
                   .[30:9],REEV[LL].[39:9]])  ;                           181400
FORMAT OUT    STARTINGEVENTS("THE NETWORK CONTAINS",I4," STARTING ",      181500
                   "EVENTS WHICH ARE NUMBERED"/(5I12)),                   181600
              STARTINGEVENT("THE NETWORK CONTAINS",I4," STARTING EVENT",  181700
                   " WHICH IS NUMBERED"/I12),                             181800
              ENDINGEVENTS("THE NETWORK CONTAINS",I4," ENDING EVENTS ",   181900
                   "WHICH ARE NUMBERED"/(5I12)),                          182000
              ENDINGEVENT("THE NETWORK CONTAINS",I4," ENDING EVENT ",     182100
                   "WHICH IS NUMBERED"/I12),                              182200
              HEADING(13A6,X10,"RUN",I11)  ;                              182300
              WRITE(PRINTER[PAGE])  ;                                     182400
              WRITE(PRINTER[DBL],HEADING,OUTHEAD)  ;                      182500
              IF BK = 1 THEN WRITE(PRINTER[DBL],STARTINGEVENT,OUT1) ELSE  182600
                   WRITE(PRINTER[DBL],STARTINGEVENTS,OUT1)  ;             182700
              IF EK = 1 THEN WRITE(PRINTER[DBL],ENDINGEVENT,OUT2) ELSE    182800
                   WRITE(PRINTER[DBL],ENDINGEVENTS,OUT2)  ;               182900
         END  ;                                                           183000
         BEGIN                                                            183100
COMMENT       BLOCK TO UPDATE ACTIVITY TAPE FILE WITH TALLY COUNTS  ;     183200
INTEGER       KRI,KRJ,KCI,KCJ,TEMP  ;                                     183300
LABEL         LOOP,EOF  ;                                                 183400
LABEL         LAB1, LAB2, LAB3, LAB4, LAB  ;                              183500
SWITCH        SWL ~ LAB,LAB1,LAB2,LAB3,LAB4  ;                            183600
FORMAT OUT    ERROR48("ALL PRECEDING ACTIVITIES TO ACTIVITY ",I10," TO",  183700
                   I10,"NOT COMPLETED, ACTUAL START DATE NOT ALLOWED"),   183800
              ERROR49("ALL PRECEDING ACTIVITIES TO ACTIVITY ",I10," TO",  183900
                   I10,"NOT COMPLETED, ACTUAL COMPL DATE NOT ALLOWED") ;  184000
LOOP:         READ(SWF[RD],*,TREC)[EOF]  ;                                184100
              TAL ~ 0  ;                                                  184200
              TEMP ~ RRIJ.[4:22]  ;                                       184300
              KRI ~ TEMP.[30:9]  ;  KCI ~ TEMP.[39:9]  ;                  184400
              TEMP ~ RRIJ.[26:22]  ;                                      184500
              KRJ ~ TEMP.[30:9]  ;  KCJ ~ TEMP.[39:9]  ;                  184600
              TAL ~ TALLIES[KRJ,KCJ]  ;                                   184700
              IF (TALLIES[KRI,KCI].[30:9] = TALLIES[KRI,KCI].[39:9])      184800
                   AND (ASD ! 3) THEN TAL ~ - TAL  ;                      184900
              GO TO SWL[ASD + 1]  ;                                       185000
LAB1:         IF (TAL } 0) AND (TALLIES[KRI,KCI].[30:9] ! 0) THEN         185100
         BEGIN                                                            185200
              WRITE(PRINTER[DBL],ERROR48,OUTERACT)  ;                     185300
              BOOL2 ~ TRUE  ;                                             185400
         END  ;                                                           185500
              GO TO LAB  ;                                                185600
LAB2:         IF (TAL } 0) AND (TALLIES[KRI,KCI].[30:9] ! 0) THEN         185700
         BEGIN                                                            185800
              WRITE(PRINTER[DBL],ERROR49,OUTERACT)  ;                     185900
              BOOL2 ~ TRUE  ;                                             186000
         END  ;                                                           186100
              GO TO LAB  ;                                                186200
LAB3:         IF (TALLIES[KRI,KCI].[30:9] ! 0) THEN                       186300
         BEGIN                                                            186400
              WRITE(PRINTER[DBL],ERROR46,OUTERACT)  ;                     186500
              BOOL2 ~ TRUE  ;                                             186600
         END  ;                                                           186700
              GO TO LAB  ;                                                186800
LAB4:         IF (TALLIES[KRJ,KCJ].[21:9] ! 0) THEN                       186900
         BEGIN                                                            187000
              WRITE(PRINTER[DBL],ERROR47,OUTERACT)  ;                     187100
              BOOL2 ~ TRUE  ;                                             187200
         END  ;                                                           187300
LAB:          WRITE(SWF[WR],*,TREC)  ;                                    187400
              GO TO LOOP  ;                                               187500
EOF:          REWIND(SWF[RD])  ;  REWIND(SWF[WR])  ;                      187600
              TP ~ RD  ;  RD ~ WR  ;  WR ~ TP  ;                          187700
         END  ;                                                           187800
              IF BOOL2 THEN                                               187900
         BEGIN                                                            188000
              WRITE(PRINTER,SPO3)  ;                                      188100
              GO TO FINISH                                                188200
         END  ;                                                           188300
              IF EVENTTAPE THEN                                           188400
         BEGIN                                                            188500
COMMENT       WRITE EVENT NUMBERS AND TALLIES ON EVENT TAPE  ;            188600
LOOP3:        READ(SWF[NE],*,EREC)[EOF3]  ;                               188700
              TALLY ~ TALLIES[RN.[30:9],RN.[39:9]]  ;                     188800
              WRITE(SWF[WR],*,EREC)  ;                                    188900
              GO TO LOOP3  ;                                              189000
EOF3:         IF (UPDATE = 1) AND SAVETAPES THEN                          189100
         BEGIN                                                            189200
              LOCK(SWF[NE],SAVE)  ;                                       189300
              INST[3,0] ~ FILENAMES[NE]  ;                                189400
              TYPEOUT(SPOOUT,INST[3,*])  ;  TYPEOUT(SPOOUT,INST[5,*])  ;  189500
         END                                                              189600
              ELSE REWIND(SWF[NE])  ;                                     189700
              REWIND(SWF[WR])  ;                                          189800
              TP ~ NE  ;  NE ~ WR  ;  WR ~ TP  ;                          189900
              IF (RE = WR) THEN RE ~ NE  ;                                190000
         END  ;                                                           190100
         END  AUDIT BLOCK  ;                                              190200
              GO TO ORIGIN  ;                                             190300
BLOCK3:  BEGIN                                                            190400
COMMENT       RANK BLOCK  ;                                               190500
COMMENT                                                                   190600
              DEFINITIONS OF VARIABLES IN RANK BLOCK                      190700
              CH        MAXIMUM RANK PERMITTED                            190800
              SRN       RENUMBERED EVENT ON OLD EVENT TAPE                190900
              SEVNO     EVENT NUMBER ON OLD EVENT TAPE                    191000
              SRANKE    EVENT RANK ON OLD EVENT TAPE                      191100
              STALLY    TALLY ON OLD EVENT TAPE                           191200
              DIFMAX    INCREASE IN MAXRANK IN SUCCESSIVE PASSES          191300
              BOOL      BOOLEAN FLAG, TRUE IF A LOOP IS DETECTED IN THE   191400
                        NETWORK                                           191500
              TWOTAPES  BOOLEAN FLAG, TRUE IF TWO EVENT TAPES ARE TO BE   191600
                        MERGED                                            191700
              ERANK     ARRAY CONTAINING RANK OF EACH EVENT               191800
              SDSCPT    ARRAY CONTAINING EVENT DESCRIPTION ON OLD EVENT   191900
                        TAPE                                              192000
              SEEO      EARLIEST EVENT OCCURRENCE ON OLD EVENT TAPE       192100
              SEVAR     SUM OF VARIANCES ON OLD EVENT TAPE                192200
              SLEO      LATEST EVENT OCCURRENCE ON OLD EVENT TAPE         192300
              SSLCK     EVENT SLACK ON OLD EVENT TAPE                     192400
              MAXRANK   MAXIMUM RANK COMPUTED DURING CURRENT PASS         192500
              OLDMAX    MAXIMUM RANK COMPUTED DURING LAST PASS            192600
              TEMP,TEMP2  TEMPORARY VARIABLES                             192700
              K,L,KK,KRI,KCI,KRJ,KCJ,NN  INDICES FOR INDEXING ARRAYS      192800
                                                                       ;  192900
INTEGER       CH  ;                                                       193000
INTEGER       K,L  ;                                                      193100
INTEGER       SRN,SEVNO,SRANKE,STALLY  ;                                  193200
INTEGER       DIFMAX  ;                                                   193300
INTEGER       NN  ;                                                       193400
BOOLEAN       BOOL ;                                                      193500
BOOLEAN       TWOTAPES  ;                                                 193600
INTEGER ARRAY ERANK[0:NOEVNROW,0:NOEVNCOL] ;                              193700
ALPHA ARRAY   SDSCPT[0:4]  ;                                              193800
REAL          SEEO,SEVAR,SLEO,SSLCK  ;                                    193900
LABEL         L1, BEND2 ;                                                 194000
LABEL         LOOP,EOF,LOOP2,EOF2  ;                                      194100
LABEL         EOFA,L2  ;                                                  194200
LIST          SEREC(SRN,SEVNO,SRANKE,SEEO,SEVAR,SLEO,SSLCK,STALLY,FOR     194300
                   LL ~ 1 STEP 1 UNTIL 4 DO SDSCPT[LL])  ;                194400
LIST          OUT1(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL],RUN) ,       194500
              OUT2(KEY,EVENT[RIRJ[K,L].[8:9],RIRJ[K,L].[17:9]],           194600
                   EVENT[RIRJ[K,L].[30:9],RIRJ[K,L].[39:9]])  ;           194700
FORMAT OUT    LOOPERROR(13A6,X10,"RUN",I11/"THE NETWORK CONTAINS AT ",    194800
                   "LEAST ONE LOOP"/"SOME OR ALL OF THE FOLLOWING ",      194900
                   "ACTIVITIES FORM THE LOOP"/X8,"KEY",X7,"PRED",X7,      195000
                   "SUCC"/) ,                                             195100
              ERRORACTIVITY(3I11) ;                                       195200
INTEGER       KK,TEMP,MAXRANK  ;                                          195300
INTEGER       TEMP2,KRI,KCI,KRJ,KCJ  ;                                    195400
INTEGER       OLDMAX  ;                                                   195500
LABEL         LR  ;                                                       195600
COMMENT       SET MAXIMUM RANK FOR NETWORK  ;                             195700
              IF (CHAINLENGTH < NOEVN) THEN CH ~ CHAINLENGTH + 1          195800
                   ELSE CH ~ NOEVN  ;                                     195900
              OLDMAX ~ 0  ;                                               196000
              BOOL ~ FALSE  ;                                             196100
              TWOTAPES ~ FALSE  ;                                         196200
              FOR K ~ 0 STEP 1 UNTIL NOEVNROW DO                          196300
              FOR L ~ 0 STEP 1 UNTIL NOEVNCOL DO                          196400
              ERANK[K,L] ~ 0 ;                                            196500
COMMENT       COMPUTE EVENT RANKS  ;                                      196600
LR:           MAXRANK ~ 0  ;                                              196700
              FOR KK ~ 1 STEP 1 UNTIL NOACT DO                            196800
         BEGIN                                                            196900
              TEMP2 ~ RIRJ[KK.[30:9],KK.[39:9]]  ;                        197000
              KRI ~ TEMP2.[8:9]  ;  KCI ~ TEMP2.[17:9]  ;                 197100
              KRJ ~ TEMP2.[30:9]  ;  KCJ ~ TEMP2.[39:9]  ;                197200
              IF (ERANK[KRI,KCI] } ERANK[KRJ,KCJ]) THEN                   197300
         BEGIN                                                            197400
              TEMP ~ ERANK[KRJ,KCJ] ~ ERANK[KRI,KCI] + 1  ;               197500
              IF TEMP > MAXRANK THEN MAXRANK ~ TEMP                       197600
         END                                                              197700
         END  ;                                                           197800
COMMENT       TEST FOR A POSSIBLE LOOP  ;                                 197900
              IF (MAXRANK > CH) THEN GO TO L1  ;                          198000
              OLDMAX ~ MAXRANK  ;                                         198100
              IF (MAXRANK ! 0) THEN GO TO LR  ;                           198200
              IF EVENTTAPE THEN                                           198300
         BEGIN                                                            198400
COMMENT       WRITE EVENT RANKS ON EVENT TAPE FILE  ;                     198500
              SEVNO ~ 0  ;                                                198600
              IF (BLOCPARAM[NOBLOCK] = 1) THEN TWOTAPES ~ TRUE  ;         198700
LOOP:         READ(SWF[NE],*,EREC)[EOF]  ;                                198800
              IF TWOTAPES THEN                                            198900
         BEGIN                                                            199000
              DO READ(SWF[RE],*,SEREC)[EOFA] UNTIL (SEVNO } EVNO)  ;      199100
              IF (SEVNO = EVNO) THEN                                      199200
         BEGIN                                                            199300
              FOR NN ~ 1 STEP 1 UNTIL 4 DO                                199400
              IF (DSCPT[NN] ! "      ") THEN GO TO L2  ;                  199500
              FOR NN ~ 1 STEP 1 UNTIL 4 DO DSCPT[NN] ~ SDSCPT[NN]  ;      199600
         END  ;                                                           199700
         END  ;                                                           199800
L2:           RANKE ~ ERANK[RN.[30:9],RN.[39:9]]  ;                       199900
              WRITE(SWF[WR],*,EREC)  ;                                    200000
              GO TO LOOP  ;                                               200100
EOFA:         IF SAVETAPES THEN                                           200200
         BEGIN                                                            200300
              LOCK(SWF[RE],RELEASE)  ;                                    200400
              INST[3,0] ~ FILENAMES[RE]  ;                                200500
              TYPEOUT(SPOOUT,INST[3,*])  ;  TYPEOUT(SPOOUT,INST[5,*])  ;  200600
         END                                                              200700
              ELSE PURGE(SWF[RE],2)  ;                                    200800
              TWOTAPES ~ FALSE  ;                                         200900
              GO TO L2  ;                                                 201000
EOF:          REWIND(SWF[NE])  ;  REWIND(SWF[WR])  ;                      201100
              IF TWOTAPES THEN IF SAVETAPES THEN                          201200
         BEGIN                                                            201300
              LOCK(SWF[RE],RELEASE)  ;                                    201400
              INST[3,0] ~ FILENAMES[RE]  ;                                201500
              TYPEOUT(SPOOUT,INST[3,*])  ;  TYPEOUT(SPOOUT,INST[5,*])  ;  201600
         END                                                              201700
              ELSE PURGE(SWF[RE],2)  ;                                    201800
              TP ~ NE  ;  NE ~ RE  ;  RE ~ WR  ;  WR ~ TP  ;              201900
         END  ;                                                           202000
COMMENT       WRITE ACTIVITY RANKS ON TAPE  ;                             202100
LOOP2:        READ(SWF[RD],*,TREC)[EOF2]  ;                               202200
              RANKI ~ ERANK[RRIJ.[8:9],RRIJ.[17:9]]  ;                    202300
              WRITE(SWF[WR],*,TREC)  ;                                    202400
              GO TO LOOP2  ;                                              202500
EOF2:         REWIND(SWF[RD])  ;  REWIND(SWF[WR])  ;                      202600
              TP ~ RD  ;  RD ~ WR  ;  WR ~ TP  ;                          202700
              GO TO BEND2  ;                                              202800
COMMENT       PRINT LIST OF ACTIVITIES IN LOOP  ;                         202900
L1:           WRITE(PRINTER[PAGE])  ;                                     203000
              WRITE(PRINTER,LOOPERROR,OUT1) ;                             203100
              DIFMAX ~ MAXRANK - OLDMAX  ;                                203200
              FOR KEY ~ 1 STEP 1 UNTIL NOACT DO                           203300
         BEGIN                                                            203400
              K ~ KEY.[30:9]  ;  L~ KEY.[39:9]  ;                         203500
              IF (ERANK[RIRJ[K,L].[8:9],RIRJ[K,L].[17:9]] >               203600
                   CH - DIFMAX)                                           203700
              THEN WRITE(PRINTER,ERRORACTIVITY,OUT2)                      203800
         END ;                                                            203900
              GO TO FINISH ;                                              204000
BEND2:   END  RANK BLOCK  ;                                               204100
              GO TO ORIGIN  ;                                             204200
BLOCK4:  BEGIN                                                            204300
COMMENT       SORT BLOCK  ;                                               204400
COMMENT                                                                   204500
              DEFINITIONS OF VARIABLES IN SORT BLOCK                      204600
              NCOLS     NUMBER OF SORTKEYS                                204700
              COLSN     ARRAY CONTAINING SORTKEYS                         204800
              TEMPKEY   TEMPORARY VARIABLE                                204900
              L         INDEX FOR ARRAYS                                  205000
                                                                       ;  205100
INTEGER       NCOLS,L,TEMPKEY  ;                                          205200
INTEGER ARRAY COLSN[0:4]  ;                                               205300
LABEL         LOOP,EOF  ;                                                 205400
PROCEDURE     TAPESORT(NTAPES,NWPR,NCOLS,SORTKEYS,ARSIZE,SORTINPUT,       205500
                   SORTOUTPUT,SEQFLAG,SEQCOL,SAVEFACTOR)  ;               205600
COMMENT       THIS PROCEDURE SORTS A TAPE FILE  ;                         205700
COMMENT                                                                   205800
              DEFINITIONS OF SOME OF THE VARIABLES IN PROCEDURE TAPESORT  205900
              NTAPES    NUMBER OF TAPES USED FOR SORTING                  206000
              NWPR      NUMBER OF WORDS PER LOGICAL RECORD                206100
              NCOLS     NUMBER OF SORTKEYS                                206200
              SORTKEYS  ARRAY CONTAINING SORTKEY NUMBERS                  206300
              ARSIZ     NUMBER OF RECORDS WHICH CAN BE SORTED INTERNALLY  206400
              SORTINPUT INPUT TAPE FILE                                   206500
              SORTOUTPUT OUTPUT TAPE FILE                                 206600
              SEQFLAG   BOOLEAN FLAG, TRUE IF RECORDS ARE TO BE           206700
                        SEQUENCED AFTER THEY ARE SORTED                   206800
              SEQCOL    COLUMN IN WHICH RECORDS ARE TO BE SEQUENCED       206900
              SAVEFACTOR  BOOLEAN FLAG, TRUE IF INPUT FILE IS TO BE       207000
                        PRESERVED                                         207100
                                                                       ;  207200
INTEGER       NTAPES,NWPR,NCOLS,ARSIZE ;                                  207300
INTEGER       SEQCOL  ;                                                   207400
BOOLEAN       SEQFLAG,SAVEFACTOR  ;                                       207500
FILE          SORTINPUT,SORTOUTPUT ;                                      207600
INTEGER ARRAY SORTKEYS[0] ;                                               207700
         BEGIN                                                            207800
INTEGER       RECSIN,RECSOUT  ;                                           207900
REAL          EOFMARK ;                                                   208000
BOOLEAN       MOREDATA,EXITFLAG,LASTPASS,INITIAL ;                        208100
INTEGER       NNT,NT,JJ,J,I,COUNTER,COPYCOUNT,WRT,NEWRT,NSLEFT,           208200
              NNWPR,IND,TT,K,SMALL,LARGE,FIRST,SECOND,CC,H ;              208300
INTEGER       FLAG ;                                                      208400
LABEL         STEPUP,MERGEST,MERGE,RECORD,EOF1,EOF2,RETURN,CLOSEND ;      208500
LABEL         EOF3,TAPECOPY,ADJUST,EXIT,DISTRIB ;                         208600
LABEL         NOTALLODD,EOF4 ;                                            208700
LABEL         XCH ;                                                       208800
SWITCH        SWFLAG1 ~ XCH,RECORD ;                                      208900
SWITCH        SWFLAG2 ~ RECORD,XCH ;                                      209000
BOOLEAN ARRAY ASCENDFLAG[0:NTAPES] ;                                      209100
INTEGER ARRAY LIM[0:NTAPES],DIST[0:NTAPES] ;                              209200
INTEGER ARRAY TCHECK,TSUM,INDEX[0:NTAPES],RECINDEX[0:ARSIZE] ;            209300
REAL ARRAY    A[0:NTAPES,0:NWPR-1] ;                                      209400
SAVE REAL ARRAY  AA[0:ARSIZE,0:NWPR-1]  ;                                 209500
REAL ARRAY    EOFARRAY,BUFF[0:NWPR-1],NEOFARRAY[0:NWPR-1] ;               209600
FILE          TSORT2(2,100,NWPR)  ;                                       209700
FILE          TSORT3(2,100,NWPR)  ;                                       209800
FILE          TSORT4(2,100,NWPR)  ;                                       209900
FILE          TSORT5(2,100,NWPR)  ;                                       210000
FILE          TSORT6(2,100,NWPR)  ;                                       210100
FILE          TSORT7(2,100,NWPR)  ;                                       210200
FILE          TSORT8(2,100,NWPR)  ;                                       210300
FILE          TSORT9(2,100,NWPR)  ;                                       210400
SWITCH FILE   SWF ~ SORTINPUT,SORTOUTPUT,TSORT2,TSORT3,TSORT4,            210500
                    TSORT5,TSORT6,TSORT7,TSORT8,TSORT9 ;                  210600
FORMAT OUT    ERROR1("EOF1 LABEL PASSED"),                                210700
              ERROR2("EOF2 LABEL PASSED"),                                210800
              ERROR3("SORT NOT PERFORMED CORRECTLY, RECORDS IN ",         210900
                  "NOT EQUAL TO RECORDS OUT")  ;                          211000
PROCEDURE     TSORTW(N,NCOLS,SORTKEYS,A,INDEX,AFLAG) ;                    211100
VALUE         N,NCOLS ;                                                   211200
INTEGER       N,NCOLS ;                                                   211300
BOOLEAN       AFLAG ;                                                     211400
INTEGER ARRAY SORTKEYS,INDEX[0] ;                                         211500
ARRAY         A[0,0] ;                                                    211600
         BEGIN                                                            211700
INTEGER       M,K,J,I,IM,CC,CCC,TI,TIM,FLAG ;                             211800
LABEL         L1,L2,L3,L4,XCH ;                                           211900
SWITCH        SW1 ~ XCH,L4 ;                                              212000
SWITCH        SW2~ L4,XCH ;                                               212100
              IF AFLAG THEN FLAG ~ 1 ELSE FLAG ~ 2 ;                      212200
              M ~ N ;                                                     212300
L1:           M ~ M DIV 2 ;                                               212400
              IF M ! 0 THEN                                               212500
         BEGIN                                                            212600
              K ~ N - M ; J ~ 1 ;                                         212700
L2:           I ~ J ;                                                     212800
L3:           IM ~ I + M ; TI ~ INDEX[I] ; TIM ~ INDEX[IM] ;              212900
              FOR CC ~ 1 STEP 1 UNTIL NCOLS DO                            213000
         BEGIN                                                            213100
              CCC ~ SORTKEYS[CC] ;                                        213200
              IF A[TI,CCC] > A[TIM,CCC] THEN GO TO SW1[FLAG] ;            213300
              IF A[TI,CCC] < A[TIM,CCC] THEN GO TO SW2[FLAG] ;            213400
         END ;                                                            213500
              GO TO L4 ;                                                  213600
XCH:          INDEX[I] ~ TIM ; INDEX[IM] ~ TI ;                           213700
              I ~ I - M ; IF I } 1 THEN GO TO L3 ;                        213800
L4:           J ~ J + 1 ;                                                 213900
              IF J > K THEN GO TO L1 ELSE GO TO L2 ;                      214000
         END                                                              214100
         END TSORTW ;                                                     214200
PROCEDURE     INTERNALSORT(UNITIN,UNITOUT,NWPR,RECSIN,ARSIZE,NCOLS,       214300
                   AFLAG,RECSOUT,MOREDATA,RECINDEX,SORTKEYS,EOFARRAY,     214400
                   AA,TSORTW,SWF,SUM,INITIAL,NEOFARRAY,SEQFLAG,SEQCOL) ;  214500
INTEGER       UNITIN,UNITOUT,NWPR,RECSIN,ARSIZE,NCOLS,RECSOUT ;           214600
INTEGER       SUM ;                                                       214700
INTEGER       SEQCOL  ;                                                   214800
BOOLEAN       MOREDATA,AFLAG,INITIAL ;                                    214900
BOOLEAN       SEQFLAG  ;                                                  215000
INTEGER ARRAY RECINDEX,SORTKEYS[0] ;                                      215100
REAL ARRAY    EOFARRAY[0],AA[0,0],NEOFARRAY[0] ;                          215200
PROCEDURE     TSORTW ;                                                    215300
SWITCH FILE   SWF ;                                                       215400
         BEGIN                                                            215500
INTEGER       K,J,NNWPR ;                                                 215600
LABEL         L1,L2,L3,L4,RET,L5,L7,L8  ;                                 215700
              K ~ 0 ; NNWPR ~ NWPR - 1 ;                                  215800
L1:           K ~ K + 1 ; READ(SWF[UNITIN],NWPR,AA[K,*])[L3] ;            215900
              RECSIN ~ RECSIN + 1 ; RECINDEX[K] ~ K ;                     216000
              IF K ! ARSIZE THEN GO TO L1 ;                               216100
L2:           TSORTW(K,NCOLS,SORTKEYS,AA,RECINDEX,AFLAG) ;                216200
              IF INITIAL THEN                                             216300
         BEGIN                                                            216400
              IF MOREDATA THEN                                            216500
         BEGIN                                                            216600
              READ(SWF[UNITIN][NO],NWPR,AA[0,*])[L5]  ;                   216700
              GO TO L7  ;                                                 216800
         END  ;                                                           216900
L5:           MOREDATA ~ FALSE  ;                                         217000
              GO TO L8  ;                                                 217100
         END ;                                                            217200
L7:           IF AFLAG THEN WRITE(SWF[UNITOUT],NWPR,NEOFARRAY[*])         217300
              ELSE WRITE(SWF[UNITOUT],NWPR,EOFARRAY[*]) ;                 217400
L8:           AFLAG ~ NOT AFLAG ;                                         217500
              FOR J ~ 1 STEP 1 UNTIL K DO                                 217600
         BEGIN                                                            217700
              RECSOUT ~ RECSOUT + 1  ;                                    217800
              IF INITIAL AND NOT MOREDATA AND SEQFLAG THEN                217900
             AA[RECINDEX[J],SEQCOL] ~ RECSOUT  ;                          218000
              WRITE(SWF[UNITOUT],NWPR,AA[RECINDEX[J],*]) ;                218100
         END ;                                                            218200
              SUM ~ SUM + 1 ;                                             218300
              IF MOREDATA THEN                                            218400
              READ(SWF[UNITIN][NO],NWPR,AA[0,*])[L4] ;                    218500
              GO TO RET ;                                                 218600
L3:           MOREDATA ~ FALSE ; K ~ K - 1 ; GO TO L2 ;                   218700
L4:           MOREDATA ~ FALSE ;                                          218800
RET:          INITIAL ~ FALSE ;                                           218900
         END INTERNALSORT ;                                               219000
PROCEDURE     COPY(COPYCOUNT,UNITIN,UNITOUT,NWPR,EOFMARK,SWF,SORTKEYS);   219100
INTEGER       COPYCOUNT,UNITIN,UNITOUT,NWPR ;                             219200
REAL          EOFMARK ;                                                   219300
SWITCH FILE   SWF ;                                                       219400
INTEGER ARRAY SORTKEYS[0] ;                                               219500
         BEGIN                                                            219600
LABEL         L1,L2 ;                                                     219700
INTEGER       I,TALLY ;                                                   219800
ARRAY         TEMP[0:NWPR-1] ;                                            219900
              TALLY ~ COPYCOUNT ;                                         220000
              FOR I ~ 0 STEP 1 UNTIL NWPR-1 DO TEMP[I] ~ EOFMARK ;        220100
              WRITE(SWF[UNITOUT],NWPR,TEMP[*]) ;                          220200
L1:           READ REVERSE(SWF[UNITIN],NWPR,TEMP[ *])[L2] ;               220300
              IF ABS(TEMP[SORTKEYS[1]]) = EOFMARK THEN                    220400
         BEGIN                                                            220500
              TALLY ~ TALLY - 1 ;                                         220600
              IF TALLY = 0 THEN GO TO L2 ;                                220700
         END ;                                                            220800
              WRITE(SWF[UNITOUT],NWPR,TEMP[*]) ;                          220900
              GO TO L1 ;                                                  221000
L2:           REWIND(SWF[UNITIN]) ;                                       221100
         END COPY ;                                                       221200
              INITIAL ~ TRUE ;                                            221300
              RECSIN ~ RECSOUT ~ 0 ; LASTPASS ~ FALSE ;                   221400
              NNT ~ NTAPES - 1 ; NT ~ NTAPES ; JJ ~ 0 ;                   221500
              NNWPR ~ NWPR - 1 ;                                          221600
              EOFMARK ~ 549755813887 ;                                    221700
              EOFMARK.[3:6] ~ 63 ;                                        221800
              FOR I ~ 0 STEP 1 UNTIL NNWPR DO                             221900
         BEGIN                                                            222000
              EOFARRAY[I] ~ EOFMARK ; NEOFARRAY[I] ~ -EOFMARK ;           222100
         END ;                                                            222200
              FOR I ~ 0 STEP 1 UNTIL NTAPES DO                            222300
         BEGIN                                                            222400
              ASCENDFLAG[I] ~ FALSE ;                                     222500
              TSUM[I] ~ LIM[I] ~ TCHECK[I] ~ DIST[I] ~ 0 ;                222600
         END ;                                                            222700
              EXITFLAG ~ FALSE ; MOREDATA ~ TRUE ;                        222800
              TCHECK[1] ~ 1 ; I ~ 1 ;                                     222900
              ASCENDFLAG[1] ~ TRUE ;                                      223000
              INTERNALSORT(0,I,NWPR,RECSIN,ARSIZE,NCOLS,ASCENDFLAG[I],    223100
                   RECSOUT,MOREDATA,RECINDEX,SORTKEYS,EOFARRAY,AA,        223200
                   TSORTW,SWF,TSUM[I],INITIAL,NEOFARRAY,SEQFLAG,SEQCOL);  223300
              IF NOT MOREDATA THEN                                        223400
         BEGIN                                                            223500
              REWIND(SWF[I])  ;                                           223600
              IF SAVEFACTOR THEN                                          223700
         BEGIN                                                            223800
              LOCK(SWF[0],SAVE)  ;                                        223900
              TYPEOUT(SPOOUT,INST[5,*])  ;                                224000
              SAVEFACTOR ~ FALSE  ;                                       224100
         END                                                              224200
              ELSE PURGE(SWF[0],1)  ;                                     224300
              GO TO RETURN  ;                                             224400
         END ;                                                            224500
DISTRIB:      H ~ TCHECK[1] ;                                             224600
              FOR I ~ 2 STEP 1 UNTIL NNT DO                               224700
              TCHECK[I-1] ~ TCHECK[I] + H ; TCHECK[NNT] ~ H ;             224800
              FOR I ~ 1 STEP 1 UNTIL NNT DO                               224900
              IF BOOLEAN(TCHECK[I]) THEN                                  225000
         BEGIN                                                            225100
              FOR JJ ~ 1 STEP 1 UNTIL NNT DO                              225200
              LIM [JJ] ~ TCHECK[(I+JJ-2) MOD NNT + 1 ] ;                  225300
              GO TO EXIT ;                                                225400
         END ;                                                            225500
EXIT:         FOR I ~ 1 STEP 1 UNTIL NNT DO                               225600
              FOR J ~ TSUM[I]+1 STEP 1 UNTIL LIM[I] DO                    225700
              IF MOREDATA THEN                                            225800
         BEGIN                                                            225900
              INTERNALSORT(0,I,NWPR,RECSIN,ARSIZE,NCOLS,ASCENDFLAG[I],    226000
                   RECSOUT,MOREDATA,RECINDEX,SORTKEYS,EOFARRAY,AA,        226100
                   TSORTW,SWF,TSUM[I],INITIAL,NEOFARRAY,FALSE,0)  ;       226200
         END ELSE                                                         226300
         BEGIN                                                            226400
              IF ASCENDFLAG[I] THEN                                       226500
         BEGIN                                                            226600
              WRITE(SWF[I],NWPR,NEOFARRAY[*]) ;                           226700
         END ELSE                                                         226800
         BEGIN                                                            226900
              WRITE(SWF[I],NWPR, EOFARRAY[*]) ;                           227000
         END ;                                                            227100
              TSUM[I] ~ TSUM[I] + 1 ;                                     227200
              ASCENDFLAG[I] ~ NOT ASCENDFLAG[I] ;                         227300
         END ;                                                            227400
              IF MOREDATA THEN GO TO DISTRIB ;                            227500
              IF SAVEFACTOR THEN                                          227600
         BEGIN                                                            227700
              LOCK(SWF[0],SAVE)  ;                                        227800
              TYPEOUT(SPOOUT,INST[5,*])  ;                                227900
              SAVEFACTOR ~ FALSE  ;                                       228000
         END                                                              228100
              ELSE PURGE(SWF[0],1)  ;                                     228200
MERGEST:      NEWRT ~ WRT ~ NT ; NSLEFT ~ RECSOUT ~ 0 ;                   228300
              FOR I ~ 2 STEP 1 UNTIL NT DO ASCENDFLAG[I] ~ FALSE ;        228400
              ASCENDFLAG[1] ~ TRUE ;                                      228500
              FOR I ~ 1 STEP 1 UNTIL NNT DO                               228600
              IF NOT BOOLEAN(TSUM[I]) THEN GO TO NOTALLODD ;              228700
              COPY(TSUM[1],1,WRT,NWPR,EOFMARK,SWF,SORTKEYS) ;             228800
              TSUM[WRT] ~ TSUM[1] ; TSUM[1] ~ 0 ;                         228900
              WRT ~ NEWRT ~ 1 ;                                           229000
NOTALLODD:    NSLEFT ~ 0 ;                                                229100
              FOR I ~ 1 STEP 1 UNTIL NT DO                                229200
         BEGIN                                                            229300
              NSLEFT ~ NSLEFT + TSUM[I] ;                                 229400
              ASCENDFLAG[I] ~ FALSE ;                                     229500
         END ;                                                            229600
              IF NSLEFT = NNT THEN LASTPASS ~ TRUE ;                      229700
              ASCENDFLAG[1] ~ TRUE ;                                      229800
MERGE:        FOR I ~ 1 STEP 1 UNTIL  NT DO                               229900
         BEGIN                                                            230000
              INDEX[I] ~ I ;                                              230100
              IF I ! WRT THEN                                             230200
              READ REVERSE(SWF[I],NWPR,A[I,*])[EOF1] ;                    230300
         END ;                                                            230400
              FOR J ~ 0 STEP 1 UNTIL NNWPR DO                             230500
              IF ASCENDFLAG[WRT] THEN A[WRT,J] ~ EOFMARK ELSE             230600
              A[WRT,J] ~ -EOFMARK ;                                       230700
              TSORTW(NT,NCOLS,SORTKEYS,A,INDEX,ASCENDFLAG[WRT]) ;         230800
              IF NOT LASTPASS THEN                                        230900
              IF ASCENDFLAG[WRT] THEN                                     231000
              WRITE(SWF[WRT],NWPR,NEOFARRAY[*])                           231100
              ELSE WRITE(SWF[WRT],NWPR,EOFARRAY[*]) ;                     231200
RECORD:       IND ~ INDEX[1] ;                                            231300
              IF ABS(A[IND,SORTKEYS[1]]) = EOFMARK THEN                   231400
         BEGIN                                                            231500
              NSLEFT ~ NSLEFT - NNT + 1 ;                                 231600
              IF NSLEFT = NNT THEN LASTPASS ~ TRUE ;                      231700
              IF NSLEFT = 1 THEN GO TO CLOSEND ;                          231800
              ASCENDFLAG[WRT] ~ NOT ASCENDFLAG[WRT] ;                     231900
              FOR I ~ 1 STEP 1 UNTIL NT DO                                232000
         BEGIN                                                            232100
              TT ~ 1 ;                                                    232200
              IF I ! WRT THEN TT ~ TSUM[I] ~ TSUM[I] - 1 ELSE             232300
              TSUM[I] ~ TSUM[I] + 1 ;                                     232400
              IF TT = 0 THEN                                              232500
         BEGIN                                                            232600
              NEWRT ~ I ;                                                 232700
              REWIND(SWF[NEWRT]) ;                                        232800
EOF4:    END ;                                                            232900
         END ;                                                            233000
              IF WRT ! NEWRT THEN                                         233100
         BEGIN                                                            233200
              WRT ~ NEWRT ; GO TO NOTALLODD ;                             233300
         END ;                                                            233400
              WRT ~ NEWRT ; GO TO MERGE ;                                 233500
         END ;                                                            233600
              IF LASTPASS THEN                                            233700
         BEGIN                                                            233800
              RECSOUT ~ RECSOUT + 1  ;                                    233900
              IF SEQFLAG THEN A[IND,SEQCOL] ~ RECSOUT  ;                  234000
         END  ;                                                           234100
              WRITE(SWF[WRT],NWPR,A[IND,*]) ;                             234200
              READ REVERSE(SWF[IND],NWPR,A[IND,*])[EOF2] ;                234300
              SMALL ~ 1 ; LARGE ~ 2 ;                                     234400
              FLAG ~ IF ASCENDFLAG[WRT] THEN 1 ELSE 2 ;                   234500
ADJUST:       FIRST ~ INDEX[SMALL] ; SECOND ~ INDEX[LARGE] ;              234600
              FOR K ~ 1 STEP 1 UNTIL NCOLS DO                             234700
         BEGIN                                                            234800
              CC ~ SORTKEYS[K] ;                                          234900
              IF A[FIRST,CC] > A[SECOND,CC] THEN GO TO SWFLAG1[FLAG] ;    235000
              IF A[FIRST,CC] < A[SECOND,CC] THEN GO TO SWFLAG2[FLAG] ;    235100
         END ;                                                            235200
              GO TO RECORD ;                                              235300
XCH:          K ~ INDEX[SMALL] ; INDEX[SMALL] ~ INDEX[LARGE] ;            235400
              INDEX[LARGE] ~ K ; SMALL ~ LARGE ; LARGE ~ LARGE + 1 ;      235500
              IF LARGE { NT THEN GO TO ADJUST ELSE GO TO RECORD ;         235600
CLOSEND:      FOR J ~ 1 STEP 1 UNTIL NT DO REWIND(SWF[J]) ;               235700
              GO TO RETURN ;                                              235800
EOF1:         WRITE(PRINTER,ERROR1) ; GO TO RETURN ;                      235900
EOF2:         WRITE(PRINTER,ERROR2) ; GO TO RETURN ;                      236000
RETURN:       IF (RECSIN ! RECSOUT) THEN                                  236100
         BEGIN                                                            236200
              WRITE(PRINTER[DBL],ERROR3)  ;                               236300
              GO TO FINISH  ;                                             236400
         END  ;                                                           236500
         END  TAPESORT  ;                                                 236600
              IF (BLOCPARAM[NOBLOCK] = 1) THEN                            236700
         BEGIN                                                            236800
COMMENT       SORT EVENT TAPE  ;                                          236900
              IF BSAVE THEN                                               237000
         BEGIN                                                            237100
              INST[3,0] ~ FILENAMES[RE]  ;  TYPEOUT(SPOOUT,INST[3,*])  ;  237200
         END  ;                                                           237300
              NOSORT ~ NOSORT + 1  ;                                      237400
              NCOLS ~ EVENTCOL[NOSORT,1]  ;                               237500
              FOR L ~ 1 STEP 1 UNTIL NCOLS DO                             237600
                   COLSN[L] ~ EVENTCOL[NOSORT,L+1]  ;                     237700
              TAPESORT(NTAPES,NCPR,NCOLS,COLSN,SORTARRAYSIZE,SWF[RE],     237800
                   SWF[WR],FALSE,0,BSAVE)  ;                              237900
              TP ~ RE  ;  RE ~ WR  ;  WR ~ TP  ;                          238000
              GO TO ORIGIN  ;                                             238100
         END  ;                                                           238200
              IF BLOCPARAM[NOBLOCK] = 2 THEN                              238300
         BEGIN                                                            238400
COMMENT       SORT NEW EVENT TAPE FOR MERGING DURING AN UPDATE WITH       238500
              CHANGE RUN  ;                                               238600
              NOSORT ~ NOSORT + 1  ;                                      238700
              NCOLS ~ 1  ;  COLSN[1] ~ 1  ;                               238800
              TAPESORT(NTAPES,NCPR,NCOLS,COLSN,SORTARRAYSIZE,SWF[NE],     238900
                   SWF[WR],FALSE,0,FALSE)  ;                              239000
              TP ~ NE  ;  NE ~ WR  ;  WR ~ TP  ;                          239100
              GO TO ORIGIN  ;                                             239200
         END  ;                                                           239300
COMMENT       SORT ACTIVITY TAPE  ;                                       239400
              IF BSAVA THEN                                               239500
         BEGIN                                                            239600
              INST[2,0] ~ FILENAMES[RD]  ;  TYPEOUT(SPOOUT,INST[2,*])  ;  239700
         END  ;                                                           239800
              RSORTNUM~ RSORTNUM + 1  ;                                   239900
              NCOLS ~ SORTCOL[RSORTNUM,1] ;                               240000
              FOR L ~ 1 STEP 1 UNTIL NCOLS DO                             240100
              COLSN[L] ~ SORTCOL[RSORTNUM,L+1] ;                          240200
              TAPESORT(NTAPES,NWPR,NCOLS,COLSN,SORTARRAYSIZE,SWF[RD],     240300
                   SWF[WR],TRUE,0,BSAVA)  ;                               240400
              TP ~ RD  ;  RD ~ WR  ;  WR ~ TP  ;                          240500
COMMENT       FILL GLOBAL ARRAYS AFTER SORTING  ;                         240600
         BEGIN                                                            240700
              TEMPKEY ~ 0  ;                                              240800
LOOP:         READ(SWF[RD],*,TREC)[EOF]  ;                                240900
              KEY ~ TEMPKEY ~ TEMPKEY + 1  ;                              241000
              RIRJ[KEY.[30:9],KEY.[39:9]] ~ RRIJ  ;                       241100
              GO TO LOOP  ;                                               241200
EOF:          REWIND(SWF[RD])  ;                                          241300
         END  ;                                                           241400
         END  SORT BLOCK  ;                                               241500
              GO TO ORIGIN ;                                              241600
BLOCK5:  BEGIN                                                            241700
COMMENT       NETANAL BLOCK  ;                                            241800
COMMENT                                                                   241900
              DEFINITIONS OF VARIABLES IN NETANAL BLOCK                   242000
              TE,TI     TEMPORARY ARRAYS USED IN FORWARD AND BACKWARD     242100
                        PASSES                                            242200
              MN        LATEST ALLOWABLE TIME FOR EVENT TO MEET SCHEDULE  242300
              MX        MAXIMUM TOTAL TIME LEADING TO EVENT               242400
              TEMP,TEMP1,TEMP2,K,L  LOCAL VARIABLES USED FOR INDEXING     242500
                        ARRAYS                                            242600
              SEVENS    LARGEST PERMISSABLE REAL NUMBER ON B5000          242700
                                                                       ;  242800
ARRAY         TE,TI[0:NOEVNROW,0:NOEVNCOL] ;                              242900
REAL          MN,MX ;                                                     243000
REAL          SEVENS  ;                                                   243100
INTEGER       K,L,TEMP,TEMP1,TEMP2 ;                                      243200
DEFINE        DRI = RRIJ.[8:9],RRIJ.[17:9]#,                              243300
              DRJ = RRIJ.[30:9],RRIJ.[39:9]#  ;                           243400
LABEL         LL2  ;                                                      243410
LABEL         LL3,LL4,LL5,LL7,EOF  ;                                      243500
LABEL         LOOP   ;                                                    243600
LABEL         LOOP2,LOOP3,LOOP4,LOOP5,EOF2,EOF3,EOF4,EOF5,LL10,LL11,      243700
              LL12,LL13,LL14,L4  ;                                        243800
SWITCH        SW1 ~ LL3,LL7,LL2,LL4  ;                                    243900
SWITCH        SW2 ~ LL10,LL11,LL12,LL13  ;                                244000
              SEVENS ~ 549755813887  ;  SEVENS.[3:6] ~ 63  ;              244100
              FOR K ~ 0 STEP 1 UNTIL NOEVNROW DO                          244200
              FOR L ~ 0 STEP 1 UNTIL NOEVNCOL DO                          244300
              TE[K,L] ~ TI[K,L] ~ 0  ;                                    244400
COMMENT       PERFORM THE FORWARD PASS  ;                                 244500
LOOP:         READ(SWF[RD],*,TREC)[EOF]  ;                                244600
              IF ASD = 0 THEN GO TO LL4 ;                                 244700
              GO TO SW1[ASD] ;                                            244800
COMMENT       COMPUTE EARLIEST START AND SUMS OF VARIANCES FOR ACTIVITY;  244900
LL2:          TI[DRI] ~ ADAYS  ;                                          245000
LL3:          MX ~ ADAYS + ET  ;  ES ~ ADAYS  ;                           245010
              SVAR ~ VAR ; GO TO LL5 ;                                    245100
LL7:          ES ~ ADAYS - ET ; MX ~ ADAYS ; SVAR ~ 0 ; GO TO LL5 ;       245200
LL4:          ES ~ TI[DRI] ; MX ~ ES + ET ; SVAR ~ TE[DRI] + VAR ;        245300
LL5:          IF TI[DRJ] < MX THEN                                        245400
         BEGIN                                                            245500
              TI[DRJ] ~ MX ; TE[DRJ] ~ SVAR                               245600
         END ;                                                            245700
COMMENT       UPDATE ACTIVITY TAPE WITH EARLIEST START AND SUMS OF        245800
              VARIANCES  ;                                                245900
              WRITE(SWF[WR],*,TREC)  ;                                    246000
              GO TO LOOP  ;                                               246100
EOF:          REWIND(SWF[RD])  ;                                          246200
              TP ~ RD  ;  RD ~ WR  ;  WR ~ TP  ;                          246300
              IF EVENTTAPE THEN                                           246400
         BEGIN                                                            246500
COMMENT       UPDATE EVENT TAPE WITH EARLIEST OCCURRENCE AND SUMS OF      246600
              VARIANCES  ;                                                246700
LOOP2:        READ(SWF[RE],*,EREC)[EOF2]  ;                               246800
              EEO ~ TI[RN.[30:9],RN.[39:9]]  ;                            246900
              EVAR ~ TE[RN.[30:9],RN.[39:9]]  ;                           247000
              WRITE(SWF[WR],*,EREC)  ;                                    247100
              GO TO LOOP2  ;                                              247200
EOF2:         REWIND(SWF[RE])  ;  REWIND(SWF[WR])  ;                      247300
              TP ~ WR  ;  WR ~ RE  ;  RE ~ TP  ;                          247400
         END  ;                                                           247500
COMMENT       PERFORM BACKWARD PASS  ;                                    247600
              FOR K ~ 0 STEP 1 UNTIL NOEVNROW DO                          247700
              FOR L ~ 0 STEP 1 UNTIL NOEVNCOL DO                          247800
              TE[K,L] ~ SEVENS  ;                                         247900
              FOR K ~ 1 STEP 1 UNTIL EK DO                                248000
         BEGIN                                                            248100
              TEMP ~ REEV[K] ;                                            248200
              TEMP1 ~ TEMP.[30:9] ; TEMP2 ~ TEMP.[39:9] ;                 248300
              TE[TEMP1,TEMP2] ~ TI[TEMP1,TEMP2]                           248400
         END  ;                                                           248500
LOOP3:        READ REVERSE(SWF[RD],*,TREC)[EOF3]  ;                       248600
              IF ASD = 0 THEN GO TO LL12 ;                                248700
              GO TO SW2[ASD] ;                                            248800
COMMENT       COMPUTE LATEST FINISH FOR ACTIVITY  ;                       248900
LL10:         MN ~ ADAYS ; GO TO LL14 ;                                   249000
LL11:         MN ~ ADAYS-ET ; GO TO LL14 ;                                249100
LL13:         TE[DRJ] ~ ADAYS ;                                           249200
LL12:         MN ~ TE[DRJ] - ET  ;                                        249300
LL14:         IF (TE[DRI] > MN) THEN TE[DRI] ~ MN  ;                      249400
              GO TO LOOP3  ;                                              249500
EOF3:         REWIND(SWF[RD])  ;                                          249600
COMMENT       UPDATE ACTIVITY TAPE WITH LATEST FINISH AND SLACK  ;        249700
LOOP4:        READ(SWF[RD],*,TREC)[EOF4]  ;                               249800
              IF ASD = 2 THEN                                             249900
         BEGIN                                                            250000
              LF ~ ADAYS ; SL ~ 0 ; GO TO L4                              250100
         END ;                                                            250200
              LF ~ TE[DRJ] ;  SL ~ LF - ES - ET ;                         250300
L4:           WRITE(SWF[WR],*,TREC)  ;                                    250400
              GO TO LOOP4  ;                                              250500
EOF4:         REWIND(SWF[RD])  ;  REWIND(SWF[WR])  ;                      250600
              TP ~ RD  ;  RD ~ WR  ;  WR ~ TP  ;                          250700
              IF EVENTTAPE THEN                                           250800
         BEGIN                                                            250900
COMMENT       UPDATE EVENT TAPE WITH LATEST OCCURRENCE AND SLACK  ;       251000
LOOP5:        READ(SWF[RE],*,EREC)[EOF5]  ;                               251100
              LEO ~ TE[RN.[30:9],RN.[39:9]]  ;                            251200
              SLCK ~ LEO - EEO  ;                                         251300
              WRITE(SWF[WR],*,EREC)  ;                                    251400
              GO TO LOOP5  ;                                              251500
EOF5:         REWIND(SWF[RE])  ;  REWIND(SWF[WR])  ;                      251600
              TP ~ WR  ;  WR ~ RE  ;  RE ~ TP  ;                          251700
         END  ;                                                           251800
              PURGE(SWF[WR],1)  ;                                         251900
         END  NETANAL BLOCK  ;                                            252000
              GO TO ORIGIN  ;                                             252100
BLOCK6:  BEGIN                                                            252200
COMMENT       LIST BLOCK  ;                                               252300
COMMENT                                                                   252400
              DEFINITIONS OF VARIABLES IN LIST BLOCK                      252500
              TLINE     TOTAL NUMBER PRINTED LINES PERMITTED PER PAGE     252600
              PAGEN     PAGE NUMBER                                       252700
              MOS       MONTH NUMBER OF NETWORK START DATE                252800
              DAS       DAY OF THE MONTH OF NETWORK START DATE            252900
              YRS       YEAR OF NETWORK START DATE                        253000
              MOR       MONTH NUMBER OF REPORT DATE                       253100
              DAR       DAY OF THE MONTH OF NETWORK REPORT DATE           253200
              YRR       YEAR OF NETWORK REPORT DATE                       253300
              MOE       MONTH NUMBER OF ACTIVITY START DATE               253400
              DAE       DAY OF THE MONTH OF ACTIVITY START DATE           253500
              YRE       YEAR OF ACTIVITY START DATE                       253600
              MOF       MONTH NUMBER OF ACTIVITY FINISH DATE              253700
              DAF       DAY OF THE MONTH OF ACTIVITY FINISH DATE          253800
              YRF       YEAR OF ACTIVITY FINISH DATE                      253900
              DAA       DAY OF THE MONTH OF ACTIVITY DATE                 254000
              MOA       MONTH NUMBER OF ACTIVITY DATE                     254100
              YRA       YEAR OF ACTIVITY DATE                             254200
              NLINE     NUMBER OF LINES PRINTED ON PAGE BY PRINTER        254300
              TEMPI     LOCAL VARIABLE                                    254400
              PDTA      NUMBER OF ACTIVITIES LEADING INTO A SUCCESSOR     254500
                        EVENT                                             254600
              SUTA      NUMBER OF ACTIVITIES LEADING FROM A SUCCESSOR     254700
                        EVENT                                             254800
              ACTA      NUMBER OF ACTIVITIES LEADING INTO A SUCCESSOR     254900
                        EVENT WHICH HAVE BEEN COMPLETED                   255000
              DEPTDIG   NUMBER OF DIGITS IN DEPARTMENT NUMBER             255100
              DEPT      DEPARTMENT NUMBER                                 255200
              OLDDEPT   DEPARTMENT NUMBER OF PREVIOUS EVENT               255300
              DC        FLAG DENOTING WHICH ACTIVITIES ARE TO BE LISTED   255400
                           0 ALL ACTIVITIES                               255500
                           1 DUMMY ACTIVITES OMITTED                      255600
                           2 COMPLETED ACTIVITES OMITTED                  255700
                           3 DUMMY AND COMPLETED ACTIVITES OMITTED        255800
              DEPTIJ    FLAG DENOTING TYPE OF ACTIVITY DEPARTMENT         255900
                        REPORTS                                           256000
                           0 NO DEPARTMENT REPORTS                        256100
                           1 REPORTS BY DEPARTMENT OF PREDECESSOR EVENT   256200
                           2 REPORTS BY DEPARTMENT OF SUCCESSOR EVENT     256300
              NINES     9999999999                                        256400
              MILESTONE MILESTONE EVENT NUMBER                            256500
              PEC       FLAG, P DENOTING THAT ALL PRECEDING ACTIVITIES    256600
                        HAVE BEEN COMPLETED                               256700
              ATYPE     TYPE OF ACTIVITY DATE                             256800
              PROB      PROBABILITY OF MEETING A SHEDULE DATE             256900
              BOOL2     BOOLEAN FLAG, TRUE IF ACTIVITY DATE IS NOT VALID  257000
              BDUM      BOOLEAN FLAG, TRUE IF DUMMY ACTIVITIES ARE TO     257100
                        BE OMITTED FROM LISTING                           257200
              BCOMP     BOOLEAN FLAG, TRUE IF COMPLETED ACTIVITIES ARE    257300
                        TO BE OMITTED FROM LISTING                        257400
              BDUMCOMP  BOOLEAN FLAG, TRUE IF DUMMY AND COMPLETED         257500
                        ACTIVITIES ARE TO BE OMITTED FROM LISTING         257600
              MSLIST    BOOLEAN FLAG, TRUE IF MILESTONE REPORTS ARE TO    257700
                        BE GENERATED                                      257800
              DEPTLIST  BOOLEAN FLAG, TRUE IF EVENT DEPARTMENT REPORTS    257900
                        ARE TO BE GENERATED                               258000
              DPM       ARRAY CONTAINING NUMBER OF DAYS IN EACH MONTH     258100
              MONTH     ARRAY CONTAINING ABBREVIATION OF EACH MONTH       258200
              TITLE     ARRAY CONTAINING TITLE OF MILESTONE REPORT        258300
                                                                       ;  258400
INTEGER       TLINE,PAGEN,MOS,DAS,YRS,MOR,DAR,YRR,MOE,DAE,YRE,MOF,DAF,    258500
              YRF,NLINE,PDTA,SUTA,ACTA,TEMPI  ;                           258600
INTEGER       DEPTDIG,DEPT,OLDDEPT  ;                                     258700
BOOLEAN       BOOL2  ;                                                    258800
INTEGER ARRAY DPM[0:12] ;                                                 258900
ALPHA ARRAY   MONTH[0:12] ;                                               259000
LIST          OUT1(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL],PAGEN)  ;    259100
LIST          OUTH(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL]),            259200
              OUTHA(DAS,MONTH[MOS],YRS,NOACT,RUN,DAR,MONTH[MOR],YRR,      259300
                   NOEVN)  ;                                              259400
FORMAT OUT    FORM1("NETWORK NAME",X6,13A6,X14,"PAGE",I6)  ;              259500
FORMAT OUT    FORMH(////////////////////"NETWORK NAME",X6,13A6//),        259600
              FORMHA("NETWORK START DATE",I5,X1,A3,I3,X8,"NUMBER OF ",    259700
                   "ACTIVITIES",I11,X10,"RUN NUMBER",I11//"NETWORK ",     259800
                   "REPORT DATE",I4,X1,A3,I3,X8,"NUMBER OF EVENTS",I15);  259900
PROCEDURE     PACK(DAYS,SDAYS,WDPW,DPM,MO,DA,YR,BOOL) ;                   260000
COMMENT       THIS PROCEDURE COMPUTES THE MONTH NO, DAY, AND YEAR OF A    260100
              DATE SPECIFIED IN NUMBER OF WORKING DAYS FROM A GIVEN       260200
              DATE  ;                                                     260300
COMMENT                                                                   260400
              DEFINITIONS OF VARIABLES IN PROCEDURE PACK                  260500
              DAYS      DATE TO BE PACKED GIVEN IN NUMBER OF WORKING      260600
                        DAYS FROM A GIVEN DATE SDAYS                      260700
              SDAYS     NUMBER OF WORKING DAYS FROM 1/1/62 TO A GIVEN     260800
                        DATE                                              260900
              WDPW      NUMBER OF WORKING DAYS PER WEEK                   261000
              MO        MONTH NUMBER OF DATE DAYS                         261100
              DA        DAY OF THE MONTH OF DATE DAYS                     261200
              YR        YEAR OF DATE DAYS                                 261300
              BOOL      BOOLEAN FLAG, TRUE IF DAYS IS NOT A VALID DATE    261400
              DPM       ARRAY CONTAINING NUMBER OF DAYS IN EACH MONTH     261500
              TDAYS     NUMBER OF WORKING DAYS FROM 1/1/62 UNTIL DAYS     261600
              TEMP      NUMBER OF WORK WEEKS IN TDAYS                     261700
              TTDAY     NUMBER OF ACTUAL DAYS FROM 1/1/62 UNTIL DAYS      261800
              C         NUMBER OF YEARS FROM 1/1/61 UNTIL DAYS            261900
              K         NUMBER OF YEARS FROM LAST LEAP YEAR               262000
              M         NUMBER OF MONTHS IN DATE OF DAYS                  262100
                                                                       ;  262200
VALUE         DAYS,SDAYS,WDPW  ;                                          262300
INTEGER       DAYS,SDAYS,WDPW,MO,DA,YR ;                                  262400
BOOLEAN       BOOL ;                                                      262500
INTEGER ARRAY DPM[0] ;                                                    262600
         BEGIN                                                            262700
INTEGER       TDAYS,TEMP,TTDAY,C,K,M ;                                    262800
LABEL         T1,T2,T4,L1,L2,L3 ;                                         262900
              TDAYS ~ DAYS + SDAYS ;                                      263000
              TEMP ~ TDAYS MOD WDPW ;                                     263100
              TTDAY ~ (TDAYS DIV WDPW)  | 7 + TEMP  ;                     263200
              IF TEMP = 0 THEN TTDAY ~ TTDAY - 2 ;                        263300
              IF TTDAY < 0 THEN                                           263400
         BEGIN                                                            263500
              DA ~ 1 ; MO ~ 1 ; YR ~ 62 ;                                 263600
              BOOL ~ TRUE ; GO TO L3                                      263700
         END ;                                                            263800
              IF TTDAY > 6940 THEN                                        263900
         BEGIN                                                            264000
              DA ~ 31 ; MO ~ 12 ; YR ~ 80 ;                               264100
              BOOL ~ TRUE ; GO TO L3                                      264200
         END ;                                                            264300
              TTDAY ~ TTDAY + 365 ;                                       264400
              C ~ 0 ;                                                     264500
T1:           K ~ 0 ;                                                     264600
T2:           IF TTDAY { 365 THEN GO TO T4 ;                              264700
              TTDAY ~ TTDAY - 365 ;                                       264800
              K ~ K + 1 ; C ~ C + 1 ;                                     264900
              IF K ! 3 THEN GO TO T2 ;                                    265000
              IF TTDAY > 366 THEN                                         265100
         BEGIN                                                            265200
              TTDAY ~ TTDAY - 366 ; C ~ C + 1 ; GO TO T1                  265300
         END ;                                                            265400
T4:           YR ~ 61 + C ;                                               265500
              IF (YR MOD 4) ! 0 THEN                                      265600
         BEGIN                                                            265700
              M ~ 1 ; GO TO L1                                            265800
         END ;                                                            265900
              IF TTDAY { 31 THEN                                          266000
         BEGIN                                                            266100
              MO ~ 1 ; GO TO L2                                           266200
         END ;                                                            266300
              TTDAY ~ TTDAY - 31 ;                                        266400
              IF TTDAY { 29 THEN                                          266500
         BEGIN                                                            266600
              MO ~ 2 ; GO TO L2                                           266700
         END ;                                                            266800
              TTDAY ~ TTDAY - 29 ;  M ~ 3  ;                              266900
L1:           FOR MO ~ M STEP 1 UNTIL 12 DO                               267000
         BEGIN                                                            267100
              IF TTDAY { DPM[MO] THEN GO TO L2 ;                          267200
              TTDAY ~ TTDAY - DPM[MO]                                     267300
         END ;                                                            267400
L2:           DA ~ TTDAY ; BOOL ~ FALSE ;                                 267500
L3:      END  PACK  ;                                                     267600
PROCEDURE     UPACK(MODAYR,MO,DA,YR) ;                                    267700
COMMENT       THIS PROCEDURE UNPACKS DATE MODAYR INTO MONTH NO., DAY,     267800
              AND YEAR  ;                                                 267900
COMMENT                                                                   268000
              DEFINITIONS OF VARIABLES IN PROCEDURE UPACK                 268100
              MODAYR    SIX DIGIT DATE TO BE UNPACKED                     268200
              MO        MONTH NUMBER OF MODAYR                            268300
              DA        DAY OF THE MONTH OF MODAYR                        268400
              YR        YEAR OF MODAYR                                    268500
                                                                       ;  268600
VALUE         MODAYR ;                                                    268700
INTEGER       MODAYR,MO,DA,YR ;                                           268800
         BEGIN                                                            268900
              YR ~ MODAYR MOD 100 ;                                       269000
              MO ~ MODAYR DIV 10000 ;                                     269100
              DA ~ (MODAYR DIV 100) MOD 100                               269200
         END  UPACK  ;                                                    269300
              FILL DPM[*] WITH 0,31,28,31,30,31,30,31,31,30,31,30,31  ;   269400
              FILL MONTH[*] WITH " ","JAN","FEB","MAR","APR","MAY",       269500
                   "JUN","JUL","AUG","SEP","OCT","NOV","DEC"  ;           269600
              UPACK(SDATE,MOS,DAS,YRS)  ;  UPACK(RDATE,MOR,DAR,YRR)  ;    269700
              IF (BLOCPARAM[NOBLOCK].[9:3] = 0) THEN                      269800
         BEGIN                                                            269900
COMMENT       BLOCK TO LIST ACTIVITIES  ;                                 270000
INTEGER       DAA,MOA,YRA  ;                                              270100
INTEGER       DC,DEPTIJ  ;                                                270200
ALPHA         PEC,ATYPE  ;                                                270300
REAL          PROB  ;                                                     270400
BOOLEAN       BDUM,BCOMP  ;                                               270500
BOOLEAN       BDUMCOMP  ;                                                 270600
LABEL         L1,L2,L5,L6,L7,LL1,LL2,LL3,LL4,EOF  ;                       270700
LABEL         D1,D2,D3,D4,D5,D6  ;                                        270800
SWITCH        SW1 ~ LL1,LL2,LL3,LL4  ;                                    270900
SWITCH        SW2 ~ L6,D1,D2  ;                                           271000
SWITCH        SW3 ~ L1,D4,D5  ;                                           271100
LIST          OUT6(FOR LL ~ 2 STEP 1 UNTIL 5 DO ITEMNAME[SORTCOL          271200
                   [RSORTNUM,LL]]),                                       271300
              OUTR(FOR LL ~ 1 STEP 1 UNTIL 4 DO REMARKS[LL]),             271400
              OUT2(KEY,I,J,ET,VAR,DAA,MONTH[MOA],YRA,ATYPE,DAF,           271500
                   MONTH[MOF],YRF,SL,ES+ET,SVAR,PDTA,SUTA,ACTA,RANKI,     271600
                   PEC),                                                  271700
              OUT3(KEY,I,J,ET,VAR,DAE,MONTH[MOE],YRE,DAF,MONTH[MOF],      271800
                   YRF,SL,ES+ET,SVAR,PDTA,SUTA,ACTA,RANKI,PEC),           271900
              OUT4(KEY,I,J,ET,DAA,MONTH[MOA],YRA,ATYPE,PDTA,SUTA,ACTA,    272000
                   RANKI,PEC),                                            272100
              OUT5(KEY,I,J,ET,VAR,DAA,MONTH[MOA],YRA,ATYPE,DAE,           272200
                   MONTH[MOE],YRE,SL,ET+ES,SVAR,PDTA,SUTA,ACTA,RANKI,     272300
                   PEC,PROB)  ;                                           272400
FORMAT OUT    FORM6("DEPARTMENT REPORTS BY DEPARTMENT OF PREDECESSOR ",   272500
                   "EVENT SORTED BY",4(X2,A6)//),                         272600
              FORM7("DEPARTMENT REPORTS BY DEPARTMENT OF SUCCESSOR ",     272700
                   "EVENT SORTED BY",4(X2,A6)//),                         272800
              FORM8("LISTING OF ACTIVITIES SORTED BY",4(X2,A6)//),        272900
              FORM9("LISTING OF ACTIVITIES BY DEPARTMENT OF ",            273000
                   "PREDECESSOR EVENT",X26,"DEPARTMENT NUMBER",I10),      273100
              FORM10("LISTING OF ACTIVITIES BY DEPARTMENT OF SUCCESSOR",  273200
                   " EVENT",X28,"DEPARTMENT NUMBER",I10),                 273300
              FORMR(4A6),                                                 273400
              FORMDC("DUMMY AND COMPLETED ACTIVITIES ARE OMITTED FROM     273500
                   LISTING."),                                            273600
              FORMC("COMPLETED ACTIVITIES ARE OMITTED FROM LISTING."),    273700
              FORMD("DUMMY ACTIVITIES ARE OMITTED FROM LISTING."),        273800
              HEAD("  ACTIVITY DESCRIPTION     EXP./ACT",X7,"ACTIVITY ",  273900
                   "DATE  EARLIEST   LATEST",X9,"EXPECTED",X7,"PRE SUC",  274000
                   " ACT"/" KEY PREDECESSOR SUCCESSOR DURATION VAR.   ",  274100
                   " DATE   TYPE   START     FINISH   SLACK  COMP.   ",   274200
                   "VAR.  TAL TAL TAL RANK PROB"/),                       274300
              FORM2(I5,2I11,2F7.1,I3,X1,A3,I3,X1,A3,X10,I4,X1,A3,I3,      274400
                   3F7.1,3I4,I5,X1,A1),                                   274500
              FORM3(I5,2I11,2F7.1,X14,I3,X1,A3,I3,I4,X1,A3,I3,3F7.1,      274600
                   3I4,I5,X1,A1),                                         274700
              FORM4(I5,2I11,F7.1,X7,I3,X1,A3,I3,X1,A3,X42,3I4,I5,X1,A1),  274800
              FORM5(I5,2I11,2F7.1,I3,X1,A3,I3,X1,A3,I3,X1,A3,I3,X11,      274900
                   3F7.1,3I4,I5,X1,A1,F4.2)  ;                            275000
REAL PROCEDURE NORMPROB(SCHED,EXPT,SVAR) ;                                275100
COMMENT       THIS PROCEDURE COMPUTES THE PROBABILITY OF MEETING          275200
              SCHEDULE COMPLETIONS  ;                                     275300
COMMENT                                                                   275400
              DEFINITIONS OF VARIABLES IN PROCEDURE NORMPROB              275500
              SCHED     NUMBER OF WORKING DAYS FROM NETWORK START DATE    275600
                        UNTIL ACTIVITY SCHEDULE DATE                      275700
              EXPT      NUMBER OF WORKING DAYS FROM NETWORK START DATE    275800
                        UNTIL EXPECTED FINISH OF ACTIVITY                 275900
              SVAR      SUM OF THE VARIANCES ALONG MOST CRITICAL PATH     276000
                        LEADING TO GIVEN ACTIVITY                         276100
              V,Y,Z     LOCAL VARIABLES TO PROCEDURE                      276200
                                                                       ;  276300
VALUE         SCHED,EXPT,SVAR ;                                           276400
REAL          SCHED,EXPT,SVAR ;                                           276500
         BEGIN                                                            276600
REAL          V,Y,Z ;                                                     276700
              V ~ (SCHED - EXPT)/(SQRT(SVAR)) ;                           276800
              Y ~ ABS(V)/1.4142136 ;                                      276900
              Z ~ SIGN(V) ;                                               277000
              IF Y > 50.0 THEN NORMPROB ~ 0.5 | (1 + Z) ELSE              277100
              NORMPROB ~ 0.5 | (1.0 + Z | (1.0/((((0.078108 | Y +         277200
                0.000972) | Y + 0.230389) | Y + 0.278393) | Y + 1.0)*4))  277300
         END  NORMPROB  ;                                                 277400
              BDUM ~ BCOMP ~ BDUMCOMP ~ FALSE  ;                          277500
              DC ~ BLOCPARAM[NOBLOCK].[42:6]  ;                           277600
              IF (DC = 3) THEN BDUMCOMP ~ BCOMP ~ BDUM ~ TRUE ELSE        277700
              IF (DC = 1) THEN BDUM ~ TRUE ELSE                           277800
              IF (DC =2) THEN BCOMP ~ TRUE  ;                             277900
              NLINE ~ TLINE ~ 13  ;  PAGEN ~ 0  ;                         278000
COMMENT       WRITE TITLE PAGE  ;                                         278100
              WRITE(PRINTER[PAGE])  ;                                     278200
                   WRITE(PRINTER,FORMH,OUTH)  ;                           278300
              DEPTIJ ~ BLOCPARAM[NOBLOCK].[16:3]  ;                       278400
              GO TO SW2[DEPTIJ + 1]  ;                                    278500
L6:           WRITE(PRINTER,FORM8,OUT6)  ;  GO TO L7  ;                   278600
D1:           WRITE(PRINTER,FORM6,OUT6)  ;  GO TO D3  ;                   278700
D2:           WRITE(PRINTER,FORM7,OUT6)  ;                                278800
D3:           DEPTDIG ~ BLOCPARAM[NOBLOCK].[20:3]  ;                      278900
              IF (DEPTDIG = 0) THEN DEPTDIG ~ 4  ;                        279000
              OLDDEPT ~ 10 * DEPTDIG  ;                                   279100
              DEPTDIG ~ 10 * (10 - DEPTDIG)  ;                            279200
L7:           WRITE(PRINTER,FORMHA,OUTHA)  ;                              279300
COMMENT       READ ACTIVITY RECORD  ;                                     279400
L2:           READ(SWF[RD],*,TREC)[EOF]  ;                                279500
              IF (ASD = 2) AND BCOMP THEN GO TO L2  ;                     279600
              IF BDUM AND ((ET = 0) AND (VAR = 0)) THEN GO TO L2  ;       279700
              GO TO SW3[DEPTIJ + 1]  ;                                    279800
D4:           DEPT ~ I DIV DEPTDIG  ;  GO TO D6  ;                        279900
D5:           DEPT ~ J DIV DEPTDIG  ;                                     280000
D6:                IF (DEPT ! OLDDEPT) THEN                               280100
         BEGIN                                                            280200
              OLDDEPT ~ DEPT  ;                                           280300
              NLINE ~ 13  ;  PAGEN ~ 0  ;                                 280400
         END  ;                                                           280500
L1:           NLINE ~ NLINE + 1  ;                                        280600
              IF NLINE > TLINE THEN                                       280700
         BEGIN                                                            280800
COMMENT       WRITE HEADING  ;                                            280900
              PAGEN ~ PAGEN + 1  ;  NLINE ~ 1  ;                          281000
              WRITE(PRINTER[PAGE])  ;                                     281100
              WRITE(PRINTER[DBL],FORM1,OUT1)  ;                           281200
              IF (DEPTIJ = 1) THEN WRITE(PRINTER[DBL],FORM9,DEPT) ELSE    281300
              IF (DEPTIJ = 2) THEN WRITE(PRINTER[DBL],FORM10,DEPT)  ;     281400
              IF BDUMCOMP THEN WRITE(PRINTER[DBL],FORMDC) ELSE            281500
              IF BCOMP THEN WRITE(PRINTER[DBL],FORMC) ELSE                281600
              IF BDUM THEN WRITE(PRINTER[DBL],FORMD)  ;                   281700
              WRITE(PRINTER,HEAD) ;                                       281800
         END  ;                                                           281900
              PDTA ~ TAL.[30:9]  ;                                        282000
              SUTA ~ TAL.[21:9]  ;                                        282100
              ACTA ~ TAL.[39:9]  ;                                        282200
              IF TAL < 0 THEN PEC ~ "P" ELSE PEC ~ " "  ;                 282300
COMMENT       COMPUTE ACTIVITY EARLIEST START DATE  ;                     282400
              IF ES > ENTIER(ES) THEN TEMPI ~ ENTIER(ES) + 1 ELSE         282500
              TEMPI ~ ENTIER(ES) ;                                        282600
              PACK(TEMPI,SDAYS,WDPW,DPM,MOE,DAE,YRE,BOOL2) ;              282700
COMMENT       COMPUTE ACTIVITY LATEST FINISH DATE  ;                      282800
              IF LF > ENTIER(LF) THEN TEMPI ~ ENTIER(LF) + 1 ELSE         282900
              TEMPI ~ ENTIER(LF) ;                                        283000
              PACK(TEMPI,SDAYS,WDPW,DPM,MOF,DAF,YRF,BOOL2) ;              283100
              WRITE(PRINTER,FORMR,OUTR) ;                                 283200
L5:           IF ASD ! 0 THEN                                             283300
         BEGIN                                                            283400
              TEMPI ~ ADAYS ;                                             283500
COMMENT       COMPUTE ACTIVITY DATE AND LIST ACTIVITY RECORD  ;           283600
              PACK(TEMPI,SDAYS,WDPW,DPM,MOA,DAA,YRA,BOOL2) ;              283700
              GO TO SW1[ASD] ;                                            283800
LL1:          ATYPE ~ "ST."  ;  WRITE(PRINTER[DBL],FORM2,OUT2)  ;         283900
              GO TO L2  ;                                                 284000
LL2:          ATYPE ~ "COM"  ;  WRITE(PRINTER[DBL],FORM4,OUT4)  ;         284100
              GO TO L2  ;                                                 284200
LL3:          ATYPE ~ "DIR"  ;  WRITE(PRINTER[DBL],FORM2,OUT2)  ;         284300
              GO TO L2  ;                                                 284400
LL4:          ATYPE ~ "SCH" ;                                             284500
              IF SVAR ! 0 THEN                                            284600
              PROB ~ NORMPROB(ADAYS,ET+ES,SVAR) ELSE PROB ~ 0 ;           284700
              WRITE(PRINTER[DBL],FORM5,OUT5)  ;  GO TO L2                 284800
         END ;                                                            284900
              WRITE(PRINTER[DBL],FORM3,OUT3)  ;                           285000
              GO TO L2  ;                                                 285100
EOF:          REWIND(SWF[RD])  ;                                          285200
              GO TO ORIGIN                                                285300
         END  ;                                                           285400
         BEGIN                                                            285500
COMMENT       BLOCK TO LIST EVENTS  ;                                     285600
INTEGER       NINES,MILESTONE  ;                                          285700
BOOLEAN       MSLIST,DEPTLIST  ;                                          285800
ALPHA ARRAY   TITLE[0:12]  ;                                              285900
FILE IN       MSCARDS(2,10)  ;                                            286000
LABEL         LOOP,EOF  ;                                                 286100
LABEL         L1,L2  ;                                                    286200
LIST          OUT2(EVNO,FOR LL ~ 1 STEP 1 UNTIL 4 DO DSCPT[LL],DAE,       286300
                   MONTH[MOE],YRE,DAF,MONTH[MOF],YRF,SLCK,EVAR,RANKE,     286400
                   PDTA,SUTA,ACTA),                                       286500
              OUT3(FOR LL ~ 2 STEP 1 UNTIL 5 DO EVENTITEM[EVENTCOL        286600
                   [NOSORT,LL]]),                                         286700
              TITLELIST(FOR LL ~ 1 STEP 1 UNTIL 12 DO TITLE[LL]) ;        286800
FORMAT IN     MSTITLE(12A6),                                              286900
              MSFORM(I10)  ;                                              287000
FORMAT OUT    HEAD("    EVENT",X11,"EVENT",X22,"EARLIEST   ",             287100
                   "LATEST",X13,"TOTAL",X8,"PRE SUC ACT"/"    NUMBER",    287200
                   X7,"DESCRIPTION",X18,"OCCURRENCE OCCURRENCE",          287300
                   "  SLACK VARIANCE  RANK TAL TAL TAL"/),                287400
              FORM2(I10,X1,4A6,X11,I2,X1,A3,I3,I4,X1,A3,I3,2F8.1,I7,      287500
                   3I4),                                                  287600
              FORM3(I10,X1,4A6," STARTING  ",I2,X1,A3,I3,I4,X1,A3,I3,     287700
                   2F8.1,I7,3I4),                                         287800
              FORM4(I10,X1,4A6," ENDING    ",I2,X1,A3,I3,I4,X1,A3,I3,     287900
                   2F8.1,I7,3I4),                                         288000
              FORM5("DEPARTMENT REPORTS OF EVENTS"//),                    288100
              FORM6("MILESTONE REPORTS"//),                               288200
              FORM7("LISTING OF EVENTS SORTED BY",4(X2,A6)//),            288300
              FORM8("MILESTONE EVENT NUMBER",I10," NOT ON EVENT TAPE"),   288400
              FORM9("DEPARTMENT NUMBER",I10,X4,"EVENT REPORT"),           288500
              FORM10("MILESTONE REPORT ",12A6)  ;                         288600
              DEPTLIST ~ MSLIST ~ FALSE  ;                                288700
              NLINE ~ TLINE ~ 25  ;  PAGEN ~ 0  ;                         288800
              NINES ~ 9999999999  ;                                       288900
COMMENT       WRITE TITLE PAGE  ;                                         289000
              WRITE(PRINTER[PAGE])  ;  WRITE(PRINTER,FORMH,OUTH)  ;       289100
              IF (BLOCPARAM[NOBLOCK].[16:3] = 1) THEN                     289200
         BEGIN                                                            289300
              DEPTLIST ~ TRUE  ;                                          289400
              DEPTDIG ~ BLOCPARAM[NOBLOCK].[20:3]  ;                      289500
              IF (DEPTDIG = 0) THEN DEPTDIG ~ 4  ;                        289600
              OLDDEPT ~ 10 * DEPTDIG  ;                                   289700
              DEPTDIG ~ 10 * (10 - DEPTDIG)  ;                            289800
              WRITE(PRINTER[DBL],FORM5)  ;                                289900
         END                                                              290000
              ELSE IF (BLOCPARAM[NOBLOCK].[16:3] = 2) THEN                290100
         BEGIN                                                            290200
              MSLIST ~ TRUE  ;                                            290300
              WRITE(PRINTER,FORM6)  ;                                     290400
              READ(MSCARDS,MSTITLE,TITLELIST)[EOF]  ;                     290500
         END                                                              290600
              ELSE WRITE(PRINTER,FORM7,OUT3)  ;                           290700
              WRITE(PRINTER,FORMHA,OUTHA)  ;                              290800
COMMENT       READ EVENT RECORD  ;                                        290900
LOOP:         READ(SWF[RE],*,EREC)[EOF]  ;                                291000
              IF DEPTLIST THEN                                            291100
         BEGIN                                                            291200
              DEPT ~ EVNO DIV DEPTDIG  ;                                  291300
              IF DEPT ! OLDDEPT THEN                                      291400
         BEGIN                                                            291500
              OLDDEPT ~ DEPT  ;                                           291600
              NLINE ~ 25  ;  PAGEN ~ 0  ;                                 291700
         END  ;                                                           291800
         END                                                              291900
              ELSE IF MSLIST THEN                                         292000
         BEGIN                                                            292100
COMMENT       READ MILESTONE CARDS  ;                                     292200
L1:           READ(MSCARDS,MSFORM,MILESTONE)[EOF]  ;                      292300
              IF(MILESTONE = NINES) THEN                                  292400
         BEGIN                                                            292500
              READ(MSCARDS,MSTITLE,TITLELIST)[EOF]  ;                     292600
              NLINE ~ 25  ;  PAGEN ~ 0  ;                                 292700
              EVNO ~ 0  ;                                                 292800
              REWIND(SWF[RE])  ;                                          292900
              GO TO L1  ;                                                 293000
         END  ;                                                           293100
L2:           IF (EVNO < MILESTONE) THEN                                  293200
         BEGIN                                                            293300
              READ(SWF[RE],*,EREC)[EOF]  ;                                293400
              GO TO L2  ;                                                 293500
         END  ;                                                           293600
              IF (EVNO > MILESTONE) THEN                                  293700
         BEGIN                                                            293800
              WRITE(PRINTER[DBL],FORM8,MILESTONE)  ;                      293900
              GO TO L1  ;                                                 294000
         END  ;                                                           294100
         END  ;                                                           294200
              NLINE ~ NLINE + 1  ;                                        294300
              IF NLINE > TLINE THEN                                       294400
         BEGIN                                                            294500
COMMENT       WRITE HEADING  ;                                            294600
              PAGEN ~ PAGEN + 1  ;  NLINE ~ 1  ;                          294700
              WRITE(PRINTER[PAGE])  ;  WRITE(PRINTER[DBL],FORM1,OUT1)  ;  294800
              IF DEPTLIST THEN WRITE(PRINTER[DBL],FORM9,DEPT) ELSE        294900
              IF MSLIST THEN WRITE(PRINTER[DBL],FORM10,TITLELIST)  ;      295000
              WRITE(PRINTER,HEAD)  ;                                      295100
         END  ;                                                           295200
              PDTA ~ TALLY.[30:9]  ;                                      295300
              SUTA ~ TALLY.[21:9]  ;                                      295400
              ACTA ~ TALLY.[39:9]  ;                                      295500
COMMENT       COMPUTE EARLIEST EVENT OCCURRENCE DATE  ;                   295600
              TEMPI ~ ENTIER(EEO)  ;                                      295700
              IF (EEO > TEMPI) THEN TEMPI ~ TEMPI + 1  ;                  295800
              PACK(TEMPI,SDAYS,WDPW,DPM,MOE,DAE,YRE,BOOL2)  ;             295900
COMMENT       COMPUTE LATEST EVENT OCCURRENCE DATE  ;                     296000
              TEMPI ~ ENTIER(LEO)  ;                                      296100
              IF (LEO > TEMPI) THEN TEMPI ~ TEMPI + 1  ;                  296200
              PACK(TEMPI,SDAYS,WDPW,DPM,MOF,DAF,YRF,BOOL2)  ;             296300
COMMENT       LIST EVENT RECORD  ;                                        296400
              IF PDTA = 0 THEN                                            296500
              WRITE(PRINTER[DBL],FORM3,OUT2) ELSE                         296600
              IF SUTA = 0 THEN                                            296700
              WRITE(PRINTER[DBL],FORM4,OUT2) ELSE                         296800
              WRITE(PRINTER[DBL],FORM2,OUT2) ;                            296900
              GO TO LOOP  ;                                               297000
EOF:          REWIND(SWF[RE])  ;                                          297100
         END  ;                                                           297200
              END  ;                                                      297300
              GO TO ORIGIN  ;                                             297400
BLOCK7:  BEGIN                                                            297500
COMMENT       PLOT BLOCK  ;                                               297600
COMMENT                                                                   297700
              DEFINITIONS OF VARIABLES IN PLOT BLOCK                      297800
              PARA      FLAG DENOTING INTERVAL OF GRAPH                   297900
                           1 DAYS                                         298000
                           2 WEEKS                                        298100
                           3 MONTHS                                       298200
                           4 QUARTERS                                     298300
              MOS       MONTH NUMBER OF NETWORK START DATE                298400
              DAS       DAY OF THE MONTH OF NETWORK START DATE            298500
              YRS       YEAR OF NETWORK START DATE                        298600
              MOR       MONTH NUMBER OF REPORT DATE                       298700
              DAR       DAY OF THE MONTH OF REPORT DATE                   298800
              YRR       YEAR OF REPORT DATE                               298900
              PAGEN     PAGE NUMBER OF GRAPH                              299000
              TLINE     TOTAL NUMBER OF PRINTED LINES ALLOWED PER PAGE    299100
              NLINE     NUMBER OF LINES PRINTED ON PAGE BY PRINTER        299200
              PDATE     GRAPH STARTING DATE                               299300
              MOP       MONTH NUMBER OF GRAPH STARTING DATE               299400
              DAP       DAY OF THE MONTH OF GRAPH STARTING DATE           299500
              YRP       YEAR OF GRAPH STARTING DATE                       299600
              PDAYS     NUMBER OF WORKING DAYS FROM 1/1/62 UNTIL GRAPH    299700
                        STARTING DATE                                     299800
              CPDATE    CORRECTED GRAPH STARTING DATE                     299900
              QP        QUARTER OF GRAPH START DATE                       300000
              PY        YEAR FOR HEADING OF GRAPH BY QUARTERS             300100
              SWER      FLAG DENOTING TYPE OF ERROR IN GRAPH STARTING     300200
                        DATE                                              300300
              DC        FLAG DENOTING WHICH ACTIVITIES ARE TO BE PLOTTED  300400
                           0 ALL ACTIVITIES ARE TO BE PLOTTED             300500
                           1 DUMMY ACTIVITIES ARE TO BE OMITTED           300600
                           2 COMPLETED ACTIVITES ARE TO BE OMITTED        300700
                           3 DUMMY AND COMPLETED ACTIVITES ARE TO BE      300800
                             OMITTED                                      300900
              DEPT      DEPARTMENT NUMBER OF CURRENT EVENT                301000
              DEPTIJ    FLAG DENOTING TYPE OF DEPARTMENT GRAPHS           301100
                           0 NO DEPARTMENT GRAPHS                         301200
                           1 GRAPHS BY DEPARTMENT OF PREDECESSOR EVENT    301300
                           2 GRAPHS BY DEPARTMENT OF SUCCESSOR EVENT      301400
              DEPTDIG   NUMBER OF DIGITS IN DEPARTMENT NUMBER FIELD       301500
              OLDDEPT   DEPARTMENT NUMBER OF PREVIOUS EVENT               301600
              BDUM      BOOLEAN FLAG, TRUE IF DUMMY ACTIVITIES ARE TO BE  301700
                        OMITTED FROM GRAPH                                301800
              BCOMP     BOOLEAN FLAG, TRUE IF COMPLETED ACTIVITIES ARE    301900
                        TO BE OMITTED FROM GRAPH                          302000
              BDUMCOMP  BOOLEAN FLAG, TRUE IF BOTH DUMMY AND COMPLETED    302100
                        ACTIVITIES ARE TO BE OMITTED FROM LISTING         302200
              DEPTTITLE BOOLEAN FLAG, TRUE IF TITLE FOR DEPARTMENT        302300
                        REPORTS HAS JUST BEEN WRITTEN                     302400
              WD        INTERVAL OF GRAPH IF BY WEEKS OR DAYS             302500
              Y         ARRAY CONTAINING YEARS FOR HEADING OF GRAPH BY    302600
                        MONTHS                                            302700
              DPM       ARRAY CONTAINING NUMBER OF DAYS IN EACH MONTH     302800
              MONTH     ARRAY CONTAINING ABBREVIATION OF EACH MONTH       302900
              M         ARRAY CONTAINING INITIAL OF EACH MONTH            303000
              EST       START TIME OF ACTIVITY IN WHOLE WORKING DAYS      303100
              LFT       FINISH TIME OF ACTIVITY IN WHOLE WORKING DAYS     303200
              MOE       MONTH NUMBER OF ACTIVITY START DATE               303300
              DAE       DAY OF THE MONTH OF ACTIVITY START DATE           303400
              YRE       YEAR OF ACTIVITY START DATE                       303500
              MOF       MONTH NUMBER OF ACTIVITY FINISH DATE              303600
              DAF       DAY OF THE MONTH OF ACTIVITY FINISH DATE          303700
              YRF       YEAR OF ACTIVITY FINISH DATE                      303800
              STAR      GRAPH COLUMN NUMBER OF ACTIVITY START             303900
              FIN       GRAPH COLUMN NUMBER OF ACTIVITY FINISH            304000
              DUR       GRAPH COLUMN NUMBER OF LAST INTERVAL OF ACTIVITY  304100
                        DURATION                                          304200
              N         COLUMN NUMBER OF ACTIVITY FINISH                  304300
              CH        COLUMN NUMBER OF ACTIVITY START                   304400
              BOOL2     BOOLEAN FLAG, TRUE IF ACTIVITY START DATE OR      304500
                        FINISH DATE IS NOT A VALID CALENDAR DATE OR       304600
                        ACTIVITY DURATION EXTENDS BEYOND 12/31/80         304700
              BOOL3     BOOLEAN FLAG, TRUE IF FIN OR DUR IS GREATER       304800
                        THAN 66                                           304900
              BOOL4     BOOLEAN FLAG, TRUE IF ACTIVITY FINISH DATE IS     305000
                        EARLIER THAN GRAPH STARTING DATE                  305100
              BOOL5     BOOLEAN FLAG, TRUE IF ACTIVITY START DATE IS      305200
                        EARLIER THAN GRAPH STARTING DATE                  305300
              BOOL6     BOOLEAN FLAG, TRUE IF ACTIVITY START IS BEYOND    305400
                        BOUNDS OF GRAPH                                   305500
              BEYOND    LETTER TO APPEAR IN THE COLUMN TO THE RIGHT OF    305600
                        THE FIRST GRAPHICAL LINE FOR ACTIVITY             305700
              CONT      LETTER TO APPEAR IN THE COLUMN TO THE RIGHT OF    305800
                        THE SECOND GRAPHICAL LINE FOR ACTIVITY            305900
              ALPH      LETTER TO APPEAR IN COLUMN FOR ACTIVITY START     306000
                        DATE                                              306100
              BETA      LETTER TO APPEAR IN COLUMN FOR ACTIVITY FINISH    306200
                        DATE                                              306300
              BUFF1     ARRAY CONTAINING FIRST LINE OF GRAPHICAL OUTPUT   306400
                        FOR ACTIVITY                                      306500
              BUFF2     ARRAY CONTAINING SECOND LINE OF GRAPHICAL OUTPUT  306600
                        FOR ACTIVITY                                      306700
              E         ARRAY CONTAINING TYPE OF ACTIVITY START DATE      306800
              F         ARRAY CONTAINING TYPE OF ACTIVITY FINISH DATE     306900
              PPDAYS    LOCAL VARIABLE USED IN COMPUTING DUR,STAR, AND    307000
                        FIN WHEN GRAPH BY DAYS                            307100
              WKP,WKE,WKF,TEMP  LOCAL VARIABLES USED IN COMPUTING DUR,    307200
                        STAR, AND FIN WHEN GRAPH BY WEEKS                 307300
              YRES,YRFS,DMO,DDA,DYR,DYRS,QE,QF,DQ  LOCAL VARIABLES USED   307400
                        IN COMPUTING DUR, STAR, AND FIN WHEN GRAPH BY     307500
                        MONTHS OR QUARTERS                                307600
              NN        INDEX OF ARRAY BUFF1                              307700
              LL,MM     INDICES USED IN LISTS                             307800
                                                                       ;  307900
INTEGER       LL,MM,NN,PARA,MOS,DAS,YRS,MOR,DAR,YRR,PAGEN,TLINE,          308000
              NLINE,PDATE,MOP,DAP,YRP,PDAYS,CPDATE,QP,PY  ;               308100
INTEGER       SWER  ;                                                     308200
INTEGER       DC,DEPT,DEPTIJ,DEPTDIG,OLDDEPT  ;                           308300
BOOLEAN       BDUM,BCOMP  ;                                               308400
BOOLEAN       BDUMCOMP  ;                                                 308500
BOOLEAN       DEPTTITLE  ;                                                308600
ALPHA         WD  ;                                                       308700
INTEGER ARRAY Y[0:7],DPM[0:12]  ;                                         308800
ALPHA ARRAY   MONTH[0:12],M[0:12]  ;                                      308900
LABEL         HPL1A,HPL2A,HPL3A,HPL1,HPL2,HPL3,ERR,L1  ;                  309000
LABEL         START  ;                                                    309100
LABEL         D1,D2,D3,D4,D5,HPLA,HPLAA  ;                                309200
SWITCH        SW1 ~ HPL1A,HPL1A,HPL2A,HPL3A  ;                            309300
SWITCH        SW6 ~ HPL1,HPL1,HPL2,HPL3  ;                                309400
SWITCH        SWD1 ~ D1,D2,D3  ;                                          309500
LIST          OPR1(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL]),            309600
              OPR2(FOR LL ~ 2 STEP 1 UNTIL 5 DO ITEMNAME[SORTCOL          309700
                   [RSORTNUM,LL]]),                                       309800
              OPR5(DAS,MONTH[MOS],YRS,NOACT,RUN,DAR,MONTH[MOR],YRR,       309900
                   NOEVN),                                                310000
              OPR3(FOR LL ~ 1 STEP 1 UNTIL 13 DO NETNAME[LL],PAGEN,DAS,   310100
                   MONTH[MOS],YRS,DAR,MONTH[MOR],YRR)  ;                  310200
FORMAT OUT    FPR1(////////////////////"NETWORK NAME",X6,13A6///),        310300
              FPR2("A GRAPH OF THE NETWORK SORTED BY",4(X2,A6)///),       310400
              FPR5("NETWORK START DATE",I4,X1,A3,I3,X10,"NUMBER OF ",     310500
                   "ACTIVITIES ",I10,X10,"RUN NUMBER",I10//"NETWORK ",    310600
                   "REPORT DATE",I3,X1,A3,I3,X10,"NUMBER OF EVENTS",      310700
                   I15),                                                  310800
              FPR6("DEPARTMENT GRAPHS BY DEPARTMENT OF PREDECESSOR ",     310900
                   "EVENT SORTED BY",4(X2,A6)///),                        311000
              FPR7("DEPARTMENT GRAPHS BY DEPARTMENT OF SUCCESSOR EVENT",  311100
                   "SORTED BY",4(X2,A6)///),                              311200
              FPR8("GRAPH OF ACTIVITIES BY DEPARTMENT OF PREDECESSOR ",   311300
                   "EVENT",X39,"DEPARTMENT NUMBER",I10),                  311400
              FPR9("GRAPH OF ACTIVITIES BY DEPARTMENT OF SUCCESSOR "      311500
                   "EVENT",X41,"DEPARTMENT NUMBER",I10),                  311600
              FORMDC("DUMMY AND COMPLETED ACTIVITIES ARE OMITTED FROM ",  311700
                   "THE GRAPH."),                                         311800
              FORMC("COMPLETED ACTIVITIES ARE OMITTED FROM THE GRAPH."),  311900
              FORMD("DUMMY ACTIVITIES ARE OMITTED FROM THE GRAPH."),      312000
              FPR3(13A6,X33,"PAGE",I5/"NETWORK START DATE",I3,X1,A3,I3,   312100
                   X10,"NETWORK REPORT DATE",I3,X1,A3,I3)  ;              312200
PROCEDURE     UPACK(MODAYR,MO,DA,YR)  ;                                   312300
COMMENT       THIS PROCEDURE UNPACKS DATE MODAYR INTO MONTH NO., DAY,     312400
              AND YEAR  ;                                                 312500
COMMENT                                                                   312600
              FOR DEFINITIONS OF VARIABLES IN PROCEDURE UPACK SEE LIST    312700
              BLOCK                                                       312800
                                                                       ;  312900
VALUE         MODAYR  ;                                                   313000
INTEGER       MODAYR,MO,DA,YR  ;                                          313100
         BEGIN                                                            313200
              YR ~ MODAYR MOD 100  ;                                      313300
              MO ~ MODAYR DIV 10000  ;                                    313400
              DA ~ (MODAYR DIV 100) MOD 100                               313500
         END  UPACK  ;                                                    313600
START:        PARA ~ PDATE ~ 0  ;                                         313700
              BDUM ~ BCOMP ~ BDUMCOMP ~ FALSE  ;                          313800
              DEPTTITLE ~ FALSE  ;                                        313900
              PARA.[45:3] ~ BLOCPARAM[NOBLOCK].[9:3]  ;                   314000
              PDATE ~ BLOCPARAM[NOBLOCK].[24:24]  ;                       314100
              DC ~ BLOCPARAM[NOBLOCK].[12:3]  ;                           314200
              IF (DC = 3) THEN BDUMCOMP ~ BCOMP ~ BDUM ~ TRUE ELSE        314300
              IF (DC = 1) THEN BDUM ~ TRUE ELSE                           314400
              IF (DC = 2) THEN BCOMP ~ TRUE  ;                            314500
              UPACK(SDATE,MOS,DAS,YRS)  ;                                 314600
              UPACK(RDATE,MOR,DAR,YRR)  ;                                 314700
              FILL MONTH[*] WITH " ","JAN","FEB","MAR","APR","MAY",       314800
                   "JUN","JUL","AUG","SEP","OCT","NOV","DEC"  ;           314900
              FILL DPM[*] WITH 0,31,28,31,30,31,30,31,31,30,31,30,31  ;   315000
COMMENT       WRITE TITLE PAGE  ;                                         315100
              WRITE(PRINTER[PAGE])  ;                                     315200
              WRITE(PRINTER,FPR1,OPR1)  ;                                 315300
              DEPTIJ ~ BLOCPARAM[NOBLOCK].[16:3]  ;                       315400
              GO TO SWD1[DEPTIJ + 1]  ;                                   315500
D1:           WRITE(PRINTER,FPR2,OPR2)  ;  GO TO D5  ;                    315600
D2:           WRITE(PRINTER,FPR6,OPR2)  ;  GO TO D4  ;                    315700
D3:           WRITE(PRINTER,FPR7,OPR2)  ;                                 315800
D4:           DEPTDIG ~ BLOCPARAM[NOBLOCK].[20:3]  ;                      315900
              IF (DEPTDIG = 0) THEN DEPTDIG ~ 4  ;                        316000
              OLDDEPT ~ 10 * DEPTDIG  ;                                   316100
              DEPTDIG ~ 10 * (10 - DEPTDIG)  ;                            316200
D5:           WRITE(PRINTER,FPR5,OPR5)  ;                                 316300
COMMENT       COMPUTE STARTING DATE OF GRAPH  ;                           316400
              IF PDATE = 0 THEN PDATE ~ SDATE  ;                          316500
              IF PDATE ! SDATE THEN                                       316600
         BEGIN                                                            316700
FORMAT OUT    PLOTERROR("INVALID PLOT DATE.  DATE CORRECTED TO ",         316800
                   "NEAREST VALID DATE")  ;                               316900
PROCEDURE     UNPACK(MODAYR,SDAYS,WDPW,DPM,MO,DA,YR,DAYS,CMODAYR,SWER) ;  317000
COMMENT       THIS PROCEDURE UNPACKS DATE MODAYR INTO MONTH NO., DAY,     317100
              AND YEAR AND CALCULATES NO. OF WORKING DAYS FROM SDAYS  ;   317200
COMMENT                                                                   317300
                FOR DEFINITIONS OF VARIABLES IN PROCEDURE UNPACK SEE      317400
              AUDIT BLOCK                                                 317500
                                                                       ;  317600
VALUE         MODAYR,SDAYS,WDPW  ;                                        317700
INTEGER       MODAYR,SDAYS,WDPW,MO,DA,YR,DAYS,CMODAYR,SWER ;              317800
INTEGER ARRAY DPM[0]  ;                                                   317900
         BEGIN                                                            318000
INTEGER       TEMP,YEARS,TYEARS,LYEARS,RYEARS,YDAYS,MDAYS,I,WEEKS,        318100
              REMDA  ;                                                    318200
BOOLEAN       BOOL  ;                                                     318300
LABEL         L1,L2,L3,L4,L5,L6,L7  ;                                     318400
              BOOL ~ FALSE ;                                              318500
COMMENT       UNPACK MODAYR INTO MONTH NO., DAY, AND YEAR  ;              318600
              YR ~ MODAYR MOD 100  ;                                      318700
              SWER ~ 1 ;                                                  318800
              MO ~ MODAYR DIV 10000  ;                                    318900
              DA ~ MODAYR DIV 100 MOD 100  ;                              319000
COMMENT       CHECK VALIDITY OF DATE  ;                                   319100
              IF YR } 62 THEN GO TO L1  ;                                 319200
              YR ~ 62  ;  MO ~ DA ~ 1  ;                                  319300
              BOOL ~ TRUE  ;  GO TO L6  ;                                 319400
L1:           IF YR { 80 THEN GO TO L2  ;                                 319500
              YR ~ 80 ;  MO ~ 12 ; DA ~ 31 ;                              319600
              BOOL ~ TRUE  ;  GO TO L6  ;                                 319700
L2:           IF MO } 1 THEN GO TO L3  ;                                  319800
              MO ~ DA ~ 1  ;  BOOL ~ TRUE  ;  GO TO L6  ;                 319900
L3:           IF MO { 12 THEN GO TO L4  ;                                 320000
              MO ~ 12  ;  DA ~ 31  ;  BOOL ~ TRUE  ;  GO TO L6  ;         320100
L4:           IF DA } 1 THEN GO TO L5 ;                                   320200
              DA ~ 1  ;  BOOL ~ TRUE  ;  GO TO L6  ;                      320300
L5:           IF DA { DPM[MO] THEN GO TO L6  ;                            320400
              TEMP ~ YR MOD 4  ;                                          320500
              IF (MO = 2) AND (TEMP = 0) AND (DA = 29) THEN GO TO L6  ;   320600
              DA ~ DPM[MO]  ;  BOOL ~ TRUE  ;                             320700
COMMENT       CALCULATE NO.OF WORKING DAYS SINCE SDAYS  ;                 320800
L6:           YEARS ~ YR - 62  ;                                          320900
              IF (YEARS = 0) AND (MO = 1) THEN                            321000
         BEGIN                                                            321100
              DAYS ~ DA  ;  GO TO L7                                      321200
         END  ;                                                           321300
              TYEARS ~ YEARS + 1  ;                                       321400
              LYEARS ~ TYEARS DIV 4 ;                                     321500
              RYEARS ~ YEARS - LYEARS  ;                                  321600
              YDAYS ~ (365 | RYEARS)+(366 | LYEARS) ;                     321700
              MDAYS ~ 0  ;                                                321800
              FOR I ~ 1 STEP 1 UNTIL MO-1 DO MDAYS ~ MDAYS + DPM[I] ;     321900
              IF (YR MOD 4 = 0) AND (MO > 2) THEN MDAYS ~ MDAYS + 1  ;    322000
              DAYS ~ YDAYS + MDAYS + DA  ;                                322100
L7:           CMODAYR ~ 0  ;                                              322200
COMMENT       CHECK THAT DATE IS A WORKING DAY  ;                         322300
              WEEKS ~ DAYS DIV 7  ;                                       322400
              REMDA ~ DAYS - (WEEKS | 7)  ;                               322500
              DAYS ~ (WEEKS | WDPW) + REMDA - SDAYS  ;                    322600
              IF BOOL THEN CMODAYR ~ MO | 10000 + DA | 100 + YR  ;        322700
              IF ( ( WDPW ! 7) AND((REMDA > WDPW) OR (REMDA = 0)))        322800
                 THEN                                                     322900
         BEGIN                                                            323000
              IF BOOL THEN SWER ~ 4 ELSE SWER ~ 3                         323100
         END  ;                                                           323200
              IF BOOL THEN SWER ~ 2                                       323300
         END  UNPACK  ;                                                   323400
              UNPACK(PDATE,0,WDPW,DPM,MOP,DAP,YRP,PDAYS,CPDATE,SWER)  ;   323500
              IF SWER = 4 OR SWER = 2 THEN WRITE(PRINTER,PLOTERROR)       323600
         END                                                              323700
              ELSE                                                        323800
         BEGIN                                                            323900
              PDAYS ~ SDAYS  ;  MOP ~ MOS  ;  DAP ~ DAS  ;  YRP ~ YRS     324000
         END  ;                                                           324100
COMMENT       SET PARAMETERS FOR HEADINGS OF GRAPHS  ;                    324200
              PAGEN ~ 0  ;  TLINE ~ 12  ;                                 324300
              GO TO SW1[PARA]  ;                                          324400
HPL1A:        IF (PARA = 2) THEN WD ~ "WEEKS" ELSE WD ~ " DAYS"  ;        324500
              GO TO HPLAA  ;                                              324600
HPL2A:        FOR NN ~ 1 STEP 1 UNTIL 7 DO Y[NN] ~ YRP + NN - 1  ;        324700
              FILL M[*] WITH " ","J","F","M","A","M","J","J","A","S",     324800
                   "O","N","D"  ;                                         324900
              GO TO HPLAA  ;                                              325000
HPL3A:        QP ~ MOP DIV 3  ;                                           325100
              IF (MOP MOD 3 > 0) THEN QP ~ QP + 1  ;                      325200
              PY ~ YRP  ;                                                 325300
              IF (QP { 2) THEN PY ~ PY - 1  ;                             325400
HPLAA:        IF (DEPTIJ ! 0) THEN GO TO L1  ;                            325500
HPLA:         WRITE(PRINTER[PAGE])  ;  NLINE ~ 0  ;                       325600
              PAGEN ~ PAGEN + 1  ;  WRITE(PRINTER[DBL],FPR3,OPR3)  ;      325700
              IF (DEPTIJ = 1) THEN WRITE(PRINTER[DBL],FPR8,DEPT) ELSE     325800
              IF (DEPTIJ = 2) THEN WRITE(PRINTER[DBL],FPR9,DEPT)  ;       325900
              IF BDUMCOMP THEN WRITE(PRINTER[DBL],FORMDC) ELSE            326000
              IF BCOMP THEN WRITE(PRINTER[DBL],FORMC) ELSE                326100
              IF BDUM THEN WRITE(PRINTER[DBL],FORMD)  ;                   326200
              GO TO SW6[PARA]  ;                                          326300
COMMENT       WRITE PROPER HEADING FOR GRAPH  ;                           326400
HPL1:    BEGIN                                                            326500
COMMENT       WRITE HEADING FOR GRAPH BY DAYS OR WEEKS  ;                 326600
LIST          OPL11(WD,DAP,MONTH[MOP],YRP)  ;                             326700
FORMAT OUT    FPL11("GRAPH BY ",A5," BEGINNING AT",I4,X1,A3,I3,X23,       326800
                   "11111111112222222222333333333344444444445555555555",  326900
                   "6666666"/"PREDECESSOR  SUCCESSOR",X4,"ACTIVITY ",     327000
                   "DESCRIPTION",X6,"123456789012345678901234567890",     327100
                   "123456789012345678901234567890123456"/),              327200
              FPL12("PLOT DATE NOT A WORKING DAY. GRAPH BEGINS AT "       327300
                   "FIRST WORKING DAY AFTER PLOT DATE")  ;                327400
              WRITE(PRINTER,FPL11,OPL11)  ;                               327500
              IF SWER = 3 THEN WRITE(PRINTER[DBL],FPL12)  ;               327600
              GO TO L1                                                    327700
         END  ;                                                           327800
HPL2:    BEGIN                                                            327900
COMMENT       WRITE HEADING FOR GRAPH BY MONTHS  ;                        328000
LABEL         W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,W11,W12,W13,W14  ;           328100
SWITCH        SW2 ~ W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,W11,W12  ;             328200
LIST          OPL21(DAP,MONTH[MOP],YRP,FOR LL ~ 1 STEP 1 UNTIL 6 DO       328300
                   Y[LL]),                                                328400
              OPL22(DAP,MONTH[MOP],YRP,FOR LL ~ 2 STEP 1 UNTIL 6 DO       328500
                   Y[LL]),                                                328600
              OPL23(DAP,MONTH[MOP],YRP,FOR LL ~ 2 STEP 1 UNTIL 7 DO       328700
                   Y[LL]),                                                328800
              OPL24(FOR LL ~ MOP STEP 1 UNTIL 12 DO M[LL],FOR MM ~ 1      328900
                   STEP 1 UNTIL 4 DO FOR LL ~ 1 STEP 1 UNTIL 12 DO        329000
                   M[LL],FOR LL ~ 1 STEP 1 UNTIL 5+MOP DO M[LL]),         329100
              OPL25(FOR LL ~ MOP STEP 1 UNTIL 12 DO M[LL],FOR MM ~ 1      329200
                   STEP 1 UNTIL 5 DO FOR LL ~ 1 STEP 1  UNTIL 12 DO       329300
                   M[LL],FOR LL ~ 1 STEP 1 UNTIL MOP-7 DO M[LL])  ;       329400
FORMAT OUT    JAN("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I7,4I12,I9),          329500
              FEB("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I6,4I12,I10),         329600
              MAR("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I6,I11,3I12,I10),     329700
              APR("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I5,I11,3I12,I11),     329800
              MAY("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I5,I10,3I12,I11),     329900
              JUN("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I4,I10,4I12),         330000
              JUL("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I4,I9,4I12),          330100
              AUG("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I3,I9,4I12),          330200
              SEP("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I3,I8,4I12),          330300
              OTR("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I10,4I12,I1),         330400
              NOV("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I9,4I12,I8),          330500
              DEC("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I8,4I12,I9),          330600
              FPL25("PREDECESSOR  SUCCESSOR",X4,"ACTIVITY DESCRIPTION",   330700
                   X6,66A1)  ;                                            330800
              GO TO SW2[MOP]  ;                                           330900
W1:           WRITE(PRINTER,JAN,OPL21)  ;                                 331000
              GO TO W14  ;                                                331100
W2:           WRITE(PRINTER,FEB,OPL21)  ;                                 331200
              GO TO W14  ;                                                331300
W3:           WRITE(PRINTER,MAR,OPL21)  ;                                 331400
              GO TO W14  ;                                                331500
W4:           WRITE(PRINTER,APR,OPL21)  ;                                 331600
              GO TO W14  ;                                                331700
W5:           WRITE(PRINTER,MAY,OPL21)  ;                                 331800
              GO TO W14  ;                                                331900
W6:           WRITE(PRINTER,JUN,OPL21)  ;                                 332000
              GO TO W14  ;                                                332100
W7:           WRITE(PRINTER,JUL,OPL21)  ;                                 332200
              GO TO W14  ;                                                332300
W8:           WRITE(PRINTER,AUG,OPL21)  ;                                 332400
              GO TO W13  ;                                                332500
W9:           WRITE(PRINTER,SEP,OPL21)  ;                                 332600
              GO TO W13  ;                                                332700
W10:          WRITE(PRINTER,OTR,OPL22)  ;                                 332800
              GO TO W13  ;                                                332900
W11:          WRITE(PRINTER,NOV,OPL23)  ;                                 333000
              GO TO W13  ;                                                333100
W12:          WRITE(PRINTER,DEC,OPL23)  ;                                 333200
W13:          WRITE(PRINTER[DBL],FPL25,OPL25)  ;                          333300
              GO TO L1  ;                                                 333400
W14:          WRITE(PRINTER[DBL],FPL25,OPL24)  ;                          333500
              GO TO L1                                                    333600
         END  ;                                                           333700
              IF QP { 2 THEN PY ~ PY - 1  ;                               333800
HPL3:    BEGIN                                                            333900
COMMENT       WRITE HEADING FOR GRAPH BY QUARTERS  ;                      334000
LABEL         V1,V2,V3,V4,V5  ;                                           334100
SWITCH        SW3 ~ V1,V2,V3,V4  ;                                        334200
LIST          OPL31(DAP,MONTH[MOP],YRP,FOR LL ~ 1 STEP 1 UNTIL 16 DO      334300
                   PY + LL),                                              334400
              OPL32(DAP,MONTH[MOP],YRP,FOR LL ~ 1 STEP 1 UNTIL 17 DO      334500
                   PY + LL),                                              334600
              OPL34(FOR LL ~ QP STEP 1 UNTIL 4 DO LL,FOR MM ~ 1 STEP 1    334700
                   UNTIL 15 DO FOR LL ~ 1 STEP 1 UNTIL 4 DO LL,FOR LL ~   334800
                   1 STEP 1 UNTIL QP+1 DO LL),                            334900
              OPL35(4,FOR MM ~ 1 STEP 1 UNTIL 16 DO FOR LL ~ 1 STEP 1     335000
                   UNTIL 4 DO LL,1)  ;                                    335100
FORMAT OUT    FPL31("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I3,15I4),           335200
              FPL32("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I2,16I4),           335300
              FPL33("GRAPH BEGINS AT",I4,X1,A3,I3,X26,I5,15I4),           335400
              FPL34("GRAPH BEGINS AT",I4,X1,A3,I3,X26,16I4),              335500
              FPL35("PREDECESSOR  SUCCESSOR",X4,"ACTIVITY DESCRIPTION",   335600
                   X6,66I1)  ;                                            335700
              GO TO SW3[QP]  ;                                            335800
V1:           WRITE(PRINTER,FPL31,OPL31)  ;                               335900
              GO TO V5  ;                                                 336000
V2:           WRITE(PRINTER,FPL32,OPL32)  ;                               336100
              GO TO V5  ;                                                 336200
V3:           WRITE(PRINTER,FPL33,OPL31)  ;                               336300
              GO TO V5  ;                                                 336400
V4:           WRITE(PRINTER,FPL34,OPL31)  ;                               336500
              WRITE(PRINTER[DBL],FPL35,OPL35)  ;                          336600
              GO TO L1  ;                                                 336700
V5:           WRITE(PRINTER[DBL],FPL35,OPL34)                             336800
         END  ;                                                           336900
L1:      BEGIN                                                            337000
INTEGER       EST,LFT,MOE,DAE,YRE,MOF,DAF,YRF,STAR,FIN,DUR,N,CH  ;        337100
INTEGER       PPDAYS,WKP,WKE,WKF,TEMP,YRES,YRFS,DMO,DDA,DYR,DYRS,QE,QF,   337200
              DQ  ;                                                       337300
BOOLEAN       BOOL2,BOOL3,BOOL4,BOOL5,BOOL6  ;                            337400
ALPHA         BEYOND,CONT,ALPH,BETA  ;                                    337500
ALPHA ARRAY   BUFF1[0:66],BUFF2[0:66],E[0:3],F[0:3]  ;                    337600
LABEL         L2,L6,L7,L3,S1,S2,S3,S4,S5,S6,EOF  ;                        337700
LABEL         LOOP  ;                                                     337800
LABEL         D6,D7,D8,D9  ;                                              337900
SWITCH        SW4 ~ S1,S2,S2,S3,S1  ;                                     338000
SWITCH        SW5 ~ S4,S4,S5,S4,S6  ;                                     338100
SWITCH        SWD2 ~ D9,D6,D7  ;                                          338200
LIST          OPR4(I,J,FOR LL ~ 1 STEP 1 UNTIL 4 DO REMARKS[LL],FOR LL ~  338300
                   1 STEP 1 UNTIL 66 DO BUFF1[LL],BEYOND,FOR LL ~ 1,2,3   338400
                   DO E[LL],DAE,MONTH[MOE],YRE,FOR LL ~ 1,2,3 DO F[LL],   338500
                   DAF,MONTH[MOF],YRF,FOR LL ~ 1 STEP 1 UNTIL 66 DO       338600
                   BUFF2[LL],CONT)  ;                                     338700
FORMAT OUT    FPR4(I10,I12,X3,4A6,X3,66A1,X1,A1/3A5,I3,X1,A3,I3,X2,3A5,   338800
                   I2,X1,A3,I3,X1,66A1,X1,A1/)  ;                         338900
PROCEDURE     PACK(DAYS,SDAYS,WDPW,DPM,MO,DA,YR,BOOL) ;                   339000
COMMENT       THIS PROCEDURE COMPUTES THE MONTH NO., DAY, AND YEAR OF     339100
              A DATE DAYS GIVEN BY NO. OF WORKING DAYS FROM SDAYS  ;      339200
COMMENT                                                                   339300
              FOR DEFINITIONS OF VARIABLES IN PROCEDURE PACK SEE LIST     339400
              BLOCK                                                       339500
                                                                       ;  339600
VALUE         DAYS,SDAYS,WDPW  ;                                          339700
INTEGER       DAYS,SDAYS,WDPW,MO,DA,YR ;                                  339800
BOOLEAN       BOOL ;                                                      339900
INTEGER ARRAY DPM[0] ;                                                    340000
         BEGIN                                                            340100
INTEGER       TDAYS,TEMP,TTDAY,C,K,M ;                                    340200
LABEL         T1,T2,T4,L1,L2,L3 ;                                         340300
              TDAYS ~ DAYS + SDAYS ;                                      340400
              TEMP ~ TDAYS MOD WDPW ;                                     340500
              TTDAY ~ (TDAYS DIV WDPW) | 7 + TEMP   ;                     340600
              IF TEMP = 0 THEN TTDAY ~ TTDAY - 2 ;                        340700
              IF TTDAY < 0 THEN                                           340800
         BEGIN                                                            340900
              DA ~ 1 ; MO ~ 1 ; YR ~ 62 ;                                 341000
              BOOL ~ TRUE ; GO TO L3                                      341100
         END ;                                                            341200
              IF TTDAY > 6940 THEN                                        341300
         BEGIN                                                            341400
              DA ~ 31 ; MO ~ 12 ; YR ~ 80 ;                               341500
              BOOL ~ TRUE ; GO TO L3                                      341600
         END ;                                                            341700
              TTDAY ~ TTDAY + 365 ;                                       341800
              C ~ 0 ;                                                     341900
T1:           K ~ 0 ;                                                     342000
T2:           IF TTDAY { 365 THEN GO TO T4 ;                              342100
              TTDAY ~ TTDAY - 365 ;                                       342200
              K ~ K + 1 ; C ~ C + 1 ;                                     342300
              IF K ! 3 THEN GO TO T2 ;                                    342400
              IF TTDAY > 366 THEN                                         342500
         BEGIN                                                            342600
              TTDAY ~ TTDAY - 366 ; C ~ C + 1 ; GO TO T1                  342700
         END ;                                                            342800
T4:           YR ~ 61 + C ;                                               342900
              IF (YR MOD 4) ! 0 THEN                                      343000
         BEGIN                                                            343100
              M ~ 1 ; GO TO L1                                            343200
         END ;                                                            343300
              IF TTDAY { 31 THEN                                          343400
         BEGIN                                                            343500
              MO ~ 1 ; GO TO L2                                           343600
         END ;                                                            343700
              TTDAY ~ TTDAY - 31 ;                                        343800
              IF TTDAY { 29 THEN                                          343900
         BEGIN                                                            344000
              MO ~ 2 ; GO TO L2                                           344100
         END ;                                                            344200
              TTDAY ~ TTDAY - 29 ;  M ~ 3  ;                              344300
L1:           FOR MO ~ M STEP 1 UNTIL 12 DO                               344400
         BEGIN                                                            344500
              IF TTDAY { DPM[MO] THEN GO TO L2 ;                          344600
              TTDAY ~ TTDAY - DPM[MO]                                     344700
         END ;                                                            344800
L2:           DA ~ TTDAY ; BOOL ~ FALSE ;                                 344900
L3:      END  PACK  ;                                                     345000
              IF DEPTTITLE THEN                                           345100
         BEGIN                                                            345200
              DEPTTITLE ~ FALSE  ;  GO TO D9  ;                           345300
         END  ;                                                           345400
COMMENT       READ ACTIVITY RECORD  ;                                     345500
LOOP:         READ(SWF[RD],*,TREC)[EOF]  ;                                345600
              IF (ASD = 2) AND BCOMP THEN GO TO LOOP  ;                   345700
              IF BDUM AND ((ET = 0) AND (VAR = 0)) THEN GO TO LOOP  ;     345800
              GO TO SWD2[DEPTIJ + 1]  ;                                   345900
D6:           DEPT ~ I DIV DEPTDIG  ;  GO TO D8  ;                        346000
D7:           DEPT ~ J DIV DEPTDIG  ;                                     346100
D8:           IF (DEPT ! OLDDEPT) THEN                                    346200
         BEGIN                                                            346300
              OLDDEPT ~ DEPT  ;                                           346400
              DEPTTITLE ~ TRUE  ;                                         346500
              PAGEN ~ 0  ;  GO TO HPLA  ;                                 346600
         END  ;                                                           346700
D9:           BOOL2 ~ BOOL3 ~ BOOL4 ~ BOOL5 ~ BOOL6 ~ FALSE  ;            346800
              BEYOND ~ CONT ~ " "  ;                                      346900
COMMENT       COMPUTE ACTIVITY EARLIEST START DATE  ;                     347000
              EST ~ ENTIER(ES)  ;                                         347100
              IF ES > EST THEN EST ~ EST + 1  ;                           347200
              PACK(EST,SDAYS,WDPW,DPM,MOE,DAE,YRE,BOOL2)  ;               347300
COMMENT       COMPUTE ACTIVITY LATEST FINISH DATE  ;                      347400
              LFT ~ ENTIER(LF)  ;                                         347500
              IF LF > LFT THEN LFT ~ LFT + 1  ;                           347600
              PACK(LFT,SDAYS,WDPW,DPM,MOF,DAF,YRF,BOOL2)  ;               347700
              DUR ~ ENTIER(ET)  ;                                         347800
              DUR ~ DUR + EST  ;                                          347900
              IF PARA = 1 THEN                                            348000
         BEGIN                                                            348100
COMMENT       COMPUTE DUR, STAR, AND FIN FOR GRAPH BY DAYS  ;             348200
              PPDAYS ~ PDAYS - SDAYS  ;                                   348300
              DUR ~ DUR - PPDAYS  ;                                       348400
              STAR ~ EST - PPDAYS + 1  ;                                  348500
              FIN ~ LFT - PPDAYS + 1  ;  GO TO L2                         348600
         END  ;                                                           348700
              IF PARA = 2 THEN                                            348800
         BEGIN                                                            348900
COMMENT       COMPUTE DUR, STAR, AND FIN FOR GRAPH BY WEEKS  ;            349000
              WKP ~ PDAYS DIV WDPW  ;                                     349100
              IF PDAYS MOD WDPW > 0 THEN WKP ~ WKP + 1  ;                 349200
              WKE ~ (SDAYS + EST) DIV WDPW  ;                             349300
              IF (SDAYS + EST) MOD WDPW > 0 THEN WKE ~ WKE + 1  ;         349400
              STAR ~ WKE - WKP + 1  ;                                     349500
              WKF ~ (SDAYS + LFT) DIV WDPW  ;                             349600
              IF (SDAYS + LFT) MOD WDPW > 0 THEN WKF ~ WKF + 1  ;         349700
              FIN ~ WKF - WKP + 1 ;                                       349800
              DUR ~ DUR + SDAYS  ;                                        349900
              TEMP ~ DUR MOD WDPW  ;                                      350000
              DUR ~ DUR DIV WDPW  ;                                       350100
              IF TEMP > 0 THEN DUR ~ DUR + 1  ;                           350200
              DUR ~ DUR - WKP  ;                                          350300
              GO TO L2                                                    350400
         END  ;                                                           350500
              YRES ~ YRE - YRP  ;                                         350600
              YRFS ~ YRF - YRP  ;                                         350700
              PACK(DUR,SDAYS,WDPW,DPM,DMO,DDA,DYR,BOOL2)  ;               350800
              DYRS ~ DYR - YRP  ;                                         350900
              IF PARA = 3 THEN                                            351000
         BEGIN                                                            351100
COMMENT       COMPUTE DUR, STAR, AND FIN FOR GRAPH BY MONTHS  ;           351200
              STAR ~ MOE + YRES | 12 - MOP + 1  ;                         351300
              FIN ~ MOF + YRFS | 12 - MOP + 1  ;                          351400
              DUR ~ DMO + DYRS | 12 - MOP  ;                              351500
              GO TO L2                                                    351600
         END  ;                                                           351700
COMMENT       COMPUTE DUR, STAR, AND FIN FOR GRAPH BY QUARTERS  ;         351800
              QE ~ MOE DIV 3  ;                                           351900
              IF MOE MOD 3 > 0 THEN QE ~ QE + 1  ;                        352000
              STAR ~ QE + YRES | 4 - QP + 1  ;                            352100
              QF ~ MOF DIV 3  ;                                           352200
              IF MOF MOD 3 > 0 THEN QF ~ QF + 1  ;                        352300
              FIN ~ QF + YRFS | 4 - QP + 1  ;                             352400
              DQ ~ DMO DIV 3  ;                                           352500
              IF DMO MOD 3 > 0 THEN DQ ~ DQ + 1  ;                        352600
              DUR ~ DQ + DYRS | 4 - QP  ;                                 352700
COMMENT       SET PARAMETERS FOR PRINTING GRAPH  ;                        352800
L2:           FOR NN ~ 1 STEP 1 UNTIL 66 DO BUFF1[NN] ~ BUFF2[NN] ~       352900
                   "      "  ;                                            353000
              IF FIN < 1 THEN BOOL4 ~ TRUE  ;                             353100
              IF STAR < 1 THEN                                            353200
         BEGIN                                                            353300
              BOOL5 ~ TRUE  ;                                             353400
              BEYOND ~ IF EST > LFT THEN "$" ELSE IF ET > 0 THEN "X"      353500
                   ELSE IF FIN > STAR THEN "S" ELSE " "  ;                353600
              IF BOOL4 AND (DUR < 1) THEN GO TO SW4[ASD+1]  ;             353700
         END  ;                                                           353800
              IF STAR > 66 THEN                                           353900
         BEGIN                                                            354000
              BOOL6 ~ TRUE  ;                                             354100
              BEYOND ~ IF (ET ! 0) THEN (IF FIN } STAR THEN "X"           354200
              ELSE "$") ELSE (IF FIN > STAR THEN "S" ELSE (IF FIN         354300
              < STAR THEN "$" ELSE " "))  ;                               354400
              GO TO L3                                                    354500
         END  ;                                                           354600
              IF FIN > 66 THEN BOOL3 ~ TRUE  ;                            354700
              IF STAR < 0 THEN STAR ~ 0  ;                                354800
              IF FIN < 0  THEN FIN ~ 0  ;                                 354900
              N ~ FIN - 1  ;                                              355000
              IF (FIN = STAR) AND (FIN > 0) THEN N ~ STAR  ;              355100
              IF (ET > 0) THEN IF (ASD = 2) THEN N ~ DUR ~ FIN            355200
                   ELSE IF (DUR = STAR - 1) THEN DUR ~ STAR  ;            355300
              IF LFT < EST THEN N ~ 0  ;                                  355400
              IF N > 66 THEN                                              355500
         BEGIN                                                            355600
              BOOL3 ~ TRUE  ;  N ~ 66                                     355700
         END  ;                                                           355800
              IF DUR > 66 THEN                                            355900
         BEGIN                                                            356000
              BOOL3 ~ TRUE  ;  DUR ~ 66                                   356100
         END  ;                                                           356200
              CH ~ IF STAR > 0 THEN STAR ELSE 1  ;                        356300
              IF N < DUR THEN                                             356400
         BEGIN                                                            356500
              FOR NN ~ CH STEP 1 UNTIL N DO BUFF1[NN] ~ "X"  ;            356600
              FOR NN ~ IF N } CH THEN N+1 ELSE CH                         356700
                   STEP 1 UNTIL DUR DO BUFF1[NN] ~ "$"  ;                 356800
              GO TO L3                                                    356900
         END  ;                                                           357000
              FOR NN ~ CH STEP 1 UNTIL DUR DO BUFF1[NN] ~ "X"  ;          357100
              FOR NN ~ IF DUR } CH THEN DUR+1 ELSE CH                     357200
                   STEP 1 UNTIL N DO BUFF1[NN] ~ "S"  ;                   357300
L3:           IF BOOL3 AND NOT BOOL5 THEN BEYOND ~ BUFF1[66]  ;           357400
              GO TO SW4[ASD + 1]  ;                                       357500
S1:           ALPH ~ "E"  ;  E[1] ~ "EXPEC"  ;                            357600
              E[2] ~ "TED S"  ;  E[3] ~ "TART "  ;                        357700
              GO TO SW5[ASD + 1]  ;                                       357800
S2:           ALPH ~ "A"  ;  E[1] ~ "ACTUA"  ;                            357900
              E[2] ~ "L STA"  ;  E[3] ~ "RT   "  ;                        358000
              GO TO SW5[ASD + 1]  ;                                       358100
S3:           ALPH ~ "D"  ;  E[1] ~ "DIREC"  ;                            358200
              E[2] ~ "TED S"  ;  E[3] ~ "TART "  ;                        358300
              GO TO SW5[ASD + 1]  ;                                       358400
S4:           BETA ~ "L"  ;  F[1] ~ "LATES"  ;                            358500
              F[2] ~ "T FIN"  ;  F[3] ~ "ISH  "  ;                        358600
              GO TO L6  ;                                                 358700
S5:           BETA ~ "C"  ;  F[1] ~ "COMPL"  ;                            358800
              F[2] ~ "ETED "  ;  F[3] ~ "     "  ;                        358900
              GO TO L6  ;                                                 359000
S6:           BETA ~ "S"  ;  F[1] ~ "SCHED"  ;                            359100
              F[2] ~ ". FIN"  ;  F[3] ~ "ISH  "  ;                        359200
L6:           IF (STAR = FIN) OR ((BOOL5 OR BOOL6) AND (BOOL4 OR          359300
                   (FIN > 66))) THEN                                      359400
         BEGIN                                                            359500
              ALPH ~ "*"  ;                                               359600
              IF BOOL5 OR BOOL6 THEN CONT ~ ALPH                          359700
              ELSE BUFF2[STAR] ~ ALPH  ;                                  359800
              GO TO L7                                                    359900
         END  ;                                                           360000
              IF BOOL5 OR BOOL6 THEN CONT ~ ALPH                          360100
              ELSE BUFF2[STAR] ~ ALPH  ;                                  360200
              IF BOOL4 OR (FIN = 67) THEN                                 360300
         BEGIN                                                            360400
              BEYOND ~ " "  ;  CONT ~ BETA                                360500
         END                                                              360600
              ELSE IF (NOT BOOL3 OR (BUFF1[66] = "$"))                    360700
              THEN BUFF2[FIN] ~ BETA                                      360800
              ELSE CONT ~ BETA  ;                                         360900
L7:           IF BOOL2 THEN CONT ~ "X"  ;                                 361000
              WRITE(PRINTER,FPR4,OPR4) ;                                  361100
              NLINE ~ NLINE + 1  ;                                        361200
              IF(NLINE > TLINE) AND (KEY < NOACT) THEN GO TO HPLA  ;      361300
              GO TO LOOP  ;                                               361400
EOF:          REWIND(SWF[RD])  ;                                          361500
         END                                                              361600
         END  PLOT BLOCK  ;                                               361700
              GO TO ORIGIN ;                                              361800
FINISH:  BEGIN                                                            361900
COMMENT       BLOCK TO SAVE ALL THE CURRENT VALUES OF ALL THE GLOBAL      362000
              VARIABLES AND ARRAYS WHICH ARE REQUIRED FOR AN UPDATE       362100
              RUN ;                                                       362200
COMMENT                                                                   362300
              DEFINITIONS OF VARIABLES IN FINISH BLOCK                    362400
              NLINE     NUMBER OF LINES PRINTED                           362500
              TLINE     TOTAL NUMBER OF LINES PERMITTED PER PAGE          362600
              MM,NN,KK  INDICES FOR ARRAYS                                362700
                                                                       ;  362800
INTEGER       MM  ;                                                       362900
LABEL         LNE  ;                                                      363000
              FOR MM ~ 1 STEP 1 UNTIL 5 DO                                363100
              IF NOT ((RD = MM) OR (WR = MM) OR (SC = MM) OR (RE = MM))   363200
                   THEN                                                   363300
         BEGIN                                                            363400
              NE ~ MM ;  GO TO LNE  ;                                     363500
              END  ;                                                      363600
COMMENT       WRITE SAVE RECORDS TAPE  ;                                  363700
LNE:          FOR MM ~ 1 STEP 1 UNTIL 5 DO                                363800
                   SORTCOL[0,MM] ~ SORTCOL[RSORTNUM,MM]  ;                363900
              FOR MM ~ 1 STEP 1 UNTIL 5 DO                                364000
                   EVENTCOL[0,MM] ~ EVENTCOL[NOSORT,MM]  ;                364100
              WRITE(PERTC,*,SAVEREC1)  ;                                  364200
              FOR K ~ 0 STEP 1 UNTIL NOACTROW DO                          364300
              WRITE(PERTC,*,SAVEREC2)  ;                                  364400
              FOR K ~ 0 STEP 1 UNTIL NOEVNROW DO                          364500
              WRITE(PERTC,*,SAVEREC3)  ;                                  364600
              REWIND(PERTC)  ;                                            364700
              IF BLOC[NOBLOCK] ! 8 THEN                                   364800
         BEGIN                                                            364900
COMMENT       LIST SAVE RECORDS AND ACTIVITY TAPES IF PROGRAM WAS NOT     365000
              COMPLETED BECAUSE OF DATA ERRORS  ;                         365100
FORMAT OUT    FORM1("SAVE RECORDS TAPE"),                                 365200
              FORM2("NETWORK NAME",X6,13A6,X6,"RUN",I6//"NETWORK START",  365300
                   " DATE",I7,"  SDAYS",I7,X10,"NETWORK REPORT DATE",I6,  365400
                   "  RDAYS",I7//"MAX NO ACTIVITIES",I7,"    MAX NO ",    365500
                   "EVENTS",I7,"    CHAIN LENGTH",I7,"    MAX NO ",       365600
                   "ENDING EVENTS",I7/"MAX NO STARTING EVENTS",I7,X4,     365700
                   "NO WORKING DAYS PER WEEK",I2,"    NO ACTIVITIES",I7,  365800
                   "    NO EVENTS",I7/"NOACTROW",I4,"  NOACTCOL",I4,X4,   365900
                   "NOEVNROW",I4,"  NOEVNCOL",I4,"   MIN EVENT",I11,      366000
                   "    MAX EVENT",I11/"NO ACTIVITY SORTKEYS",I2,X2,      366100
                   "SORTKEY NOS",4I3,"    NO EVENT SORTKEYS",I2,X2,       366200
                   "SORTKEY NOS",4I3,/"TAPE KEYS    SAVE RECORDS TAPE",   366300
                   I2,"  ACTIVITY TAPE",I2,"  EVENT TAPE",I2,X2,          366400
                   "SCRATCH TAPES",2I2),                                  366500
              FORM3("RENUMBERED ACTIVITY PREDECESSOR AND SUCCESSOR ",     366600
                   "EVENTS"),                                             366700
              FORM4(5(3I7,X3)),                                           366800
              FORM5("EVENT NUMBERS"),                                     366900
              FORM6(6(I6,I11,X3)),                                        367000
              FORM7("ACTIVITY TAPE"),                                     367100
              FORM8("    KEY  ACTIVITY DESCRIPTION"/X17,"I",X10,"J ",     367200
                   "RENUM I RENUM J    ET    VAR ASD  ADAYS   ES     ",   367300
                   "LF     SL   SVAR   RANK",X7,"TALLIES"/),              367400
              FORM9(I6,X1,4A6/X6,2I11,2I8,2F7.1,I3,I6,4F7.1,I7,X3,A1,     367500
                   3I4/)  ;                                               367600
LIST          OUT4(FOR LL ~ 1 STEP 1 UNTIL NOACT DO [LL,RIRJ[LL.[30:9],   367700
              LL.[39:9]].[4:22],RIRJ[LL.[30:9],LL.[39:9]].[26:22]]),      367800
              OUT6(FOR LL ~ 1 STEP 1 UNTIL NOEVN DO [LL,EVENT[LL.[30:9],  367900
                   LL.[39:9]]]),                                          368000
              OUT9(KEY,FOR LL ~ 1 STEP 1 UNTIL 4 DO REMARKS[LL],I,J,      368100
                   RRIJ.[4:22],RRIJ.[26:22],ET,VAR,ASD,ADAYS,ES,LF,SL,    368200
                   SVAR,RANKI,IF TAL < 0 THEN "-" ELSE "+",TAL.[21:9],    368300
                   TAL.[30:9],TAL.[39:9])  ;                              368400
INTEGER       NLINE,TLINE  ;                                              368500
LABEL         LOOP,EOF  ;                                                 368600
COMMENT       LIST SAVE RECORDS  ;                                        368700
              WRITE(PRINTER[PAGE])  ;  WRITE(PRINTER[DBL],FORM1)  ;       368800
              WRITE(PRINTER,FORM2,SAVEREC1)  ;                            368900
              WRITE(PRINTER[DBL])  ;  WRITE(PRINTER[DBL],FORM3)  ;        369000
              WRITE(PRINTER,FORM4,OUT4)  ;  WRITE(PRINTER[DBL])  ;        369100
              WRITE(PRINTER[DBL],FORM5)  ;  WRITE(PRINTER,FORM6,OUT6)  ;  369200
              NLINE ~ TLINE ~ 14  ;                                       369300
COMMENT       LIST ACTIVITY RECORDS  ;                                    369400
LOOP:         READ(SWF[RD],*,TREC)[EOF]  ;                                369500
              IF NLINE = TLINE THEN                                       369600
         BEGIN                                                            369700
COMMENT       PRINT HEADING  ;                                            369800
              WRITE(PRINTER[PAGE])  ;  WRITE(PRINTER[DBL],FORM7)  ;       369900
              WRITE(PRINTER,FORM8)  ;                                     370000
              NLINE ~ 0  ;                                                370100
         END  ;                                                           370200
              WRITE(PRINTER,FORM9,OUT9)  ;                                370300
              NLINE ~ NLINE + 1  ;                                        370400
              GO TO LOOP  ;                                               370500
EOF:          REWIND(SWF[RD])  ;                                          370600
         END  ;                                                           370700
         BEGIN                                                            370800
COMMENT       LIST BLOCKS OF PERT PROGRAM WHICH HAVE BEEN EXECUTED        370900
              SUCCESSFULLY  ;                                             371000
INTEGER       MM  ;                                                       371100
INTEGER       KK  ;                                                       371200
INTEGER       NN  ;                                                       371300
LABEL         LFIN2,LFIN3,LFIN4,LFIN5,LFIN6,LFIN7,LFIN  ;                 371400
LABEL         LFINA,LFINI,LFINJ,LFINE,LFIND,LFINM,LFING,LFINGI,LFINGJ  ;  371500
LIST          LIST3(FOR LL ~ 2 STEP 1 UNTIL 5 DO                          371600
                   ITEMNAME[SORTCOL[KK,LL]]),                             371700
              LIST3E(FOR LL ~ 2 STEP 1 UNTIL 5 DO EVENTITEM               371800
                   [EVENTCOL[NN,LL]]),                                    371900
              LIST6(IF (BLOCPARAM[MM].[9:3] = 1) THEN "DAYS  "            372000
                   ELSE IF (BLOCPARAM[MM].[9:3] = 2) THEN "WEEKS "        372100
                   ELSE IF (BLOCPARAM[MM].[9:3] = 3) THEN "MONTHS"        372200
                   ELSE "QTRS  ",IF (BLOCPARAM[MM].[24:24] = 0) THEN      372300
                   SDATE ELSE BLOCPARAM[MM].[24:24])  ;                   372400
FORMAT OUT    FIN1("INPUT AUDIT EXECUTED SUCCESSFULLY"),                  372500
              FIN2("RANK EXECUTED SUCCESSFULLY"),                         372600
              FIN3A("SORT OF ACTIVITIES BY ",4(A6,X1),"EXECUTED",         372700
                   " SUCCESSFULLY"),                                      372800
              FIN3E("SORT OF EVENTS BY ",4(A6,X1),"EXECTUED ",            372900
                   "SUCCESSFULLY"),                                       373000
              FIN4("NETANAL EXECUTED SUCCESSFULLY"),                      373100
              FIN5A("LIST OF ACTIVITIES EXECUTED SUCCESSFULLY"),          373200
              FIN5DAI("DEPARTMENT REPORTS BY DEPARTMENT OF PREDECESSOR",  373300
                   " EVENT EXECUTED SUCCESSFULLY"),                       373400
              FIN5DAJ("DEPARTMENT REPORTS BY DEPARTMENT OF SUCCESSOR ",   373500
                   "EVENT EXECUTED SUCCESSFULLY"),                        373600
              FIN5E("LIST OF EVENTS EXECUTED SUCCESSFULLY"),              373700
              FIN5DE("DEPARTMENT REPORTS OF EVENTS EXECUTED ",            373800
                   "SUCCESSFULLY"),                                       373900
              FIN5M("MILESTONE REPORTS EXECUTED SUCCESSFULLY"),           374000
              FIN6("PLOT BY ",A6," BEGINNING AT ",I6," EXECUTED ",        374100
                   "SUCCESSFULLY"),                                       374200
              FIN6DI("DEPARTMENT GRAPHS BY DEPARTMENT OF PREDECESSOR ",   374300
                   "EVENT BY ",A6," BEGINNING AT ",I6," EXECUTED ",       374400
                   "SUCCESSFULLY"),                                       374500
              FIN6DJ("DEPARTMENT GRAPHS BY DEPARTMENT OF SUCCESSOR ",     374600
                   "EVENT BY ",A6," BEGINNING AT ",I6," EXECUTED ",       374700
                   "SUCCESSFULLY"),                                       374800
              FIN7("JOB COMPLETED"),                                      374900
              FIN8("JOB NOT COMPLETED BECAUSE OF ERRORS NOTED ABOVE")  ;  375000
SWITCH        SWFIN ~ LFIN,LFIN2,LFIN3,LFIN4,LFIN5,LFIN6,LFIN7  ;         375100
SWITCH        SW2 ~ LFINA,LFINI,LFINJ,LFINE,LFIND,LFINM  ;                375200
SWITCH        SW3 ~ LFING,LFINGI,LFINGJ  ;                                375300
              KK ~ 0  ;                                                   375400
              NN ~ 0  ;                                                   375500
              WRITE(PRINTER[PAGE])  ;                                     375600
              FOR MM ~ 1 STEP 1 UNTIL NOBLOCK - 1 DO                      375700
         BEGIN                                                            375800
              GO TO SWFIN[BLOC[MM]]  ;                                    375900
LFIN2:        WRITE(PRINTER[DBL],FIN1)  ;  GO TO LFIN  ;                  376000
LFIN3:        WRITE(PRINTER[DBL],FIN2)  ;  GO TO LFIN  ;                  376100
LFIN4:        IF (BLOCPARAM[MM] } 1) THEN                                 376200
         BEGIN                                                            376300
              NN ~ NN + 1  ;  WRITE(PRINTER[DBL],FIN3E,LIST3E)  ;         376400
         END                                                              376500
              ELSE                                                        376600
         BEGIN                                                            376700
              KK ~ KK + 1  ;  WRITE(PRINTER[DBL],FIN3A,LIST3)  ;          376800
         END  ;                                                           376900
              GO TO LFIN  ;                                               377000
LFIN5:        WRITE(PRINTER[DBL],FIN4)  ;  GO TO LFIN  ;                  377100
LFIN6:        GO TO SW2[BLOCPARAM[MM].[16:3] + (3 | BLOCPARAM[MM].[9:3])  377200
                   + 1]  ;                                                377300
LFINA:        WRITE(PRINTER[DBL],FIN5A)  ;  GO TO LFIN  ;                 377400
LFINI:        WRITE(PRINTER[DBL],FIN5DAI)  ;  GO TO LFIN  ;               377500
LFINJ:        WRITE(PRINTER[DBL],FIN5DAJ)  ;  GO TO LFIN  ;               377600
LFINE:        WRITE(PRINTER[DBL],FIN5E)  ;  GO TO LFIN  ;                 377700
LFIND:        WRITE(PRINTER[DBL],FIN5DE)  ;  GO TO LFIN  ;                377800
LFINM:        WRITE(PRINTER[DBL],FIN5M)  ;  GO TO LFIN  ;                 377900
LFIN7:        GO TO SW3[BLOCPARAM[MM].[16:3] + 1]  ;                      378000
LFING:        WRITE(PRINTER[DBL],FIN6,LIST6)  ;  GO TO LFIN  ;            378100
LFINGI:       WRITE(PRINTER[DBL],FIN6DI,LIST6)  ;  GO TO LFIN  ;          378200
LFINGJ:       WRITE(PRINTER[DBL],FIN6DJ,LIST6)  ;  GO TO LFIN  ;          378300
LFIN:    END  ;                                                           378400
              IF BLOC[NOBLOCK] = 8 THEN WRITE(PRINTER,FIN7)               378500
              ELSE WRITE(PRINTER,FIN8)  ;                                 378600
         END                                                              378700
         END                                                              378800
         END ;                                                            378900
COMMENT       WRITE SPO MESSAGES TO TELL WHICH TAPES SHOULD BE SAVED FOR  379000
              AN UPDATE RUN  ;                                            379100
              INST[1,0] ~ FILENAMES[SC]  ;                                379200
              TYPEOUT(SPOOUT,INST[1,*])  ;  TYPEOUT(SPOOUT,INST[4,*])  ;  379300
              INST[2,0] ~ FILENAMES[RD]  ;                                379400
              TYPEOUT(SPOOUT,INST[2,*])  ;  TYPEOUT(SPOOUT,INST[4,*])  ;  379500
              IF EVENTTAPE THEN                                           379600
         BEGIN                                                            379700
              INST[3,0] ~ FILENAMES[RE]  ;                                379800
              TYPEOUT(SPOOUT,INST[3,*])  ;  TYPEOUT(SPOOUT,INST[4,*])  ;  379900
         END  ;                                                           380000
ENDOFPROG: END .                                                          380100
