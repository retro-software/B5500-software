CLEANUP/DISK  A DISK PACKING PROGRAM FOR THE B5500                      00000000
%-----------------------------------------------------------------------00000100
JOE FRISCH  -- YOUNG&RUBICAM, NEW YORK ----  212-576-8757               00000200
NOTE: BECAUSE THE PROGRAM USES THE DISK DIRECTORY, OPTION  28 (DSKTOG)  00000300
MUST BE RESET.                                                          00000400
%-----------------------------------------------------------------------00000500
     WITH NORMAL USE, THE B5500 DISK WILL BECOME CHECKER-BOARDED WITH   00000600
FILES WHICH BREAK UP USABLE AREAS OF CONTIGUOUS STORAGE INTO SMALL DISK 00000700
AREAS THAT USUALLY REMAIN UNUSED.                                       00000800
     CHECKER-BOARDING OCCURS WHEN FILES THAT ARE SURROUNDED BY OTHER    00000900
FILES ARE REMOVED. FILES ARE REMOVED IN NORMAL OPERATION BY:            00001000
     A) OPERATOR TYPE-IN OF RM WHICH REMOVES AN OLD PROGRAM WHEN A      00001100
     DUP LIBRARY EXISTS,                                                00001200
     B) FILES CLOSED WITH PURGE,                                        00001300
     C) FILES CREATED WITHOUT SAVE FACTORS,                             00001400
     D) A "REMOVE": CONTROL CARD, TYPE-IN, "ZIP WITH" OR "PERFORM". ETC.00001500
     CHECKER-BOARDING CAUSES "NO USER DISK", I/O ERROR 81, AND I/O ERROR00001600
84 PROBLEMS. RECOVERY IS LIMITED TO:                                    00001700
     A) COLD START                                                      00001800
     B) REMOVING LARGE FILES, (IF THEY ARE NOT CONTIGUOUS YOU CREATE    00001900
MORE CHECKER-BOARDING),                                                 00002000
     C) SUSPENSION OF PROGRAMS THAT HAVE DISK SPACE PROBLEMS.           00002100
     TO COPE WITH THE CHECKER-BOARD PROBLEMS, WE HAVE WRITTEN A PROGRAM 00002200
WHICH WILL "PACK THE DISK AND AT THE SAME TIME WILL, MULTIPROCESS.      00002300
THE PROGRAM IS CLEANUP/DISK.                                            00002400
     CLEANUP/DISK PRINTS ON A LINE PRINTER, A "PICTURE" OF THE DISK,    00002500
OF THE DISK, (ONE DISK MOD TO AN 11 | 14 1/2 INCH PAGE).                00002600
IT THEN CREATES IN CORE, MCP DUMP CARDS, REMOVE CARDS, AND LOAD CARDS   00002700
AND USES THESE TO REMOVE THE CHECKER-BOARDED FILES AND RE-LOAD THEM SO  00002800
THAT THEY "PACK". THE PROGRAM CYCLES THROUGH DISK REMOVING, DUMPING, AND00002900
LOADING FILES UNTIL THERE ARE NO MORE "ISLANDS".                        00003000
     AFTER EACH CYCLE, A NEW PICTURE OF THE DISK IS PRINTED. THE NAMES  00003100
OF ALL FILES SO MANIPULATED ARE PRINTED AT THE BOTTOM OF EACH DISK      00003200
PICTURE.                                                                00003300
     FOLLOWING IS A DETAILED DESCRIPTION OF CLEANUP/DISK.               00003400
NOTE THAT ONE TAPE UNIT IS REQUIRED.                                    00003500
CLEANUP/DISK                                                            00003600
BEGINS BY TAKING AN INVENTORY OF ALL DISK IN USE. IT DOES THIS WITH A   00003700
DIRECTORY SEARCH, TURNING A BIT ON FOR EACH SEGMENT IN USE, IN AN ARRAY 00003800
CALLED MAP.                                                             00003900
IT THEN PRINTS A PICTURE OF THE DISK, 100 CHARACTERS TO THE LINE, 80    00004000
LINES TO THE MOD, EACH CHARACTER REPRESENTING 5 DISK SEGMENTS.          00004100
THE DARKER THE CHARACTER, THE MORE SEGMENTS OF THE 5 IN USE.            00004200
(AS WRITTEN,                                                            00004300
     5 IN USE PRINTS   @                                                00004400
     4 IN USE PRINTS   }                                                00004500
     3 IN USE PRINTS   "                                                00004600
     2 IN USE PRINTS   :                                                00004700
     1 IN USE PRINTS   .                                                00004800
     0 IN USE PRINTS   SPACE                                            00004900
THESE ARE DEFINED, AND CAN BE CHANGED BY CHANGING THE DEFINITIONS).     00005000
THE PRINTOUT ALSO CONTAINS A DATE, TIME, AND COORDINATES. THE PRINTOUT  00005100
IS FOR MONITORING PURPOSES, AND IS NOT NECESSARY TO THE OPERATION OF    00005200
PROGRAM (THE WRITE (PRINT - - - - -) STATEMENTS CAN BE REMOVED IF       00005300
DESIRED).                                                               00005400
AFTER THE PRINTOUT, THE PROGRAM SCANS THE MAP, AND CHARTS ISLANDS OF    00005500
DISK IN USE THAT ARE SURROUNDED BY UNUSED DISK.                         00005600
IT STORES THESE BY LOCATION, PRINTS THEM OUT, (FIRST AS RAW ADDRESSES   00005700
THEN AS MODIFIED TO BE COMPATIBLE WITH THE MCP ADDRESSING SCHEME),      00005800
CONVERTS THEM TO NAMES OF FILES, PRINTS OUT THE 1ST 7 1/2 (IN AN ARRAY) 00005900
IN RAW FORM,                                                            00006000
CONVERTS IT TO THE APPROPRIATE DUMPING INSTRUCTION FOR THE MCP, PRINTS  00006100
IT OUT, ZIPS, AND WAITS 10 SECONDS.  (IT INSTRUCTS THE MCP TO DUMP THE  00006200
FILES TO A TAPE CALLED "EMPIRE").                                       00006300
IT THEN DOES A ZIP TO REMOVE THEM.  AT THIS POINT IT TESTS (AT 10       00006400
SECOND INTERVALS) TO SEE IF THE LAST FILE TO BE REMOVED WAS IN FACT     00006500
REMOVED.  (THE PROGRAM SUSPENDS ITSELF UNTIL THIS REMOVE TAKES PLACE,   00006600
SO IF THE FILE IS IN USE IT WILL WAIT UNTIL IT IS FINISHED.  NOTE:      00006700
IF IT TRIES TO REMOVE A SECURED FILE, OR ITSELF, IT WILL OBVIOUSLY      00006800
SUSPEND ITSELF INDEFINITELY, SO IT SHOULD BE DSED).  THE PROGRAM SHOULD 00006900
BE EXECUTED WITH THE PRIVILEGED USER CODE.                              00007000
WHEN THE LAST FILE IS REMOVED,                                          00007100
IT DOES A                                                               00007200
CC LOAD FROM EMPIRE =/=; END.                                           00007300
(IF THE PROGRAM IS DSED, THIS LOAD SHOULD BE DONE MANUALLY).            00007400
THE PROGRAM THEN CHECKS TO SEE IF IT DID IN FACT FIND ANY ISLANDS, AND  00007500
IF NOT IT GOES TO END OF JOB, OTHERWISE IT CLEARS THE MAP AND STARTS    00007600
ALL OVER AGAIN.                                                         00007700
THE PROGRAM WAS WRITTEN FOR 5 MODS OF DISK WITH ELECTRONICS UNIT 0      00007800
(EU1 ) = THE FIRST 2 MODS.  THE DEFINES - WHICH MUST BE CHANGED FOR     00007900
DIFFERENT INSTALLATIONS ARE:                                            00008000
     MODS =  NUMBER OF MODS AVAILABLE                                   00008100
     EU1  =  NUMBER OF MODS IN ELECTRONICS UNIT 0 .                     00008200
THE DEFINITION OF WHAT AN "ISLAND" IS (AS OPPOSED TO A CONTINENT) CAN   00008300
ALSO BE VARIED BY MEANS OF A COMMON CARD.   CURRENTLY IT IS SET TO 8    00008400
(384 SEGMENTS, OR ABOUT 77 CHARACTERS ON THE PRINTOUT), THIS IS THE     00008500
LARGEST LANDMASS IT ORDINARILY WILL CONSIDER AN ISLAND. THE LAST 2      00008600
DIGITS OF THE COMMON CARD (IF ANY) WILL CHANGE THIS. THE AMOUNT OF "SEA"00008700
BETWEEN "ISLANDS" CAN ALSO BE VARIED (IT IS NOW SET TO 0-ABOUT 10       00008800
CHARACTERS - THE MINIMUM).  THE 2 DIGITS PRECEDING THE LAST 2 OF THE    00008900
COMMON CARD (I.E. COMMON DIV 100) DETERMINE THE MINIMUM WATER AREA      00009000
NECESSARY BEFORE THE PROGRAM LOOKS FOR ISLANDS.                         00009100
IF IT IS DESIRED TO CHANGE THE PROGRAM PARAMETERS, A "COMMON" CARD      00009200
SHOULD BE USED.  THE SYNTAX IS:                                         00009300
       COMMON = XXYY                                                    00009400
       WHERE: XX = UNUSED DISK (WATERAREA) IN DISK SEGMENTS X 48        00009500
              YY = THE SIZE OF A DISK ISLAND (LANDMASS) IN DISK         00009600
             SEGMENTS | 48.                                             00009700
EX:  IF ALL FILES REPRESENTING ISLANDS EQUAL TO OR SMALLER THAN 96      00009800
SEGMENTS AND SURROUNDED BY 48 SEGMENTS OR LESS OF UNUSED DISK ARE TO BE 00009900
PACKED,                                                                 00010000
  COMMON = 0102                                                         00010100
     01 X 48 = 48 SEGMENTS OF WATERAREA                                 00010200
     02 X 48 = 96 SEGMENTS OF LANDMASS                                  00010300
%-----------------------------------------------------------------------00010400
BEGIN                                                                   00010500
COMMENT        CUBE LIBRARY NUMBER IS Q000005.  PROGRAM NAME IS                 
               "CLEAN/YANDR".  THIS VERSION DATED 2/1/68;                       
INTEGER COMMON;                                                         00010600
FILE IN DIRECTORY DISK "DIRCTRY" "DISK" (1,30);                         00010700
ARRAY HEADER, NAMES [0:29];                                             00010800
INTEGER                                                                 00010900
ARRAY SHIPSLOG[0:30];                                                   00011000
INTEGER REPEAT;                                                         00011100
INTEGER LASTI, LASTJ;                                                   00011200
LABEL HEREWEGOAGAIN;                                                    00011300
FILE OUT PRINT 4(2,15);                                                 00011400
INTEGER G;                                                              00011500
DEFINE MODS =5#;                                                        00011600
DEFINE EU1= 2#;                                                         00011700
ARRAY MAP [0:MODS-1, 0:835];                                            00011800
HEREWEGOAGAIN:                                                          00011900
LASTI~LASTJ~                                                            00012000
SHIPSLOG[0]~0;                                                          00012100
WRITE(PRINT[PAGE]);                                                     00012200
BEGIN                                                                   00012300
PROCEDURE SET (BIGARRAY,STARTSAT, SIZE);                                00012400
VALUE STARTSAT,SIZE; INTEGER STARTSAT, SIZE;                            00012500
ARRAY BIGARRAY [0,0];                                                   00012600
BEGIN INTEGER STMOD40, FIRSTCHUNK;                                      00012700
STREAM PROCEDURE SETSTREAM ( ARR, DSKIP, DSET);                         00012800
VALUE DSKIP, DSET;                                                      00012900
BEGIN LOCAL A,B;                                                        00013000
DI~LOC B;                                                               00013100
SI~ LOC DSET; SI~ SI+ 5;                                                00013200
2(DI~DI+7; DS~CHR);                                                     00013300
DI~ ARR;                                                                00013400
SKIP DSKIP DB;                                                          00013500
B(4(32(DS~ 32 SET)));                                                   00013600
A( 2( DS~ 32 SET));                                                     00013700
DS ~ DSET SET;                                                          00013800
END SETSTREAM;                                                          00013900
IF (STMOD40~ STARTSAT MOD 40000) + SIZE GTR 40000 THEN                  00014000
BEGIN FIRSTCHUNK ~         40000-STMOD40;                               00014100
SET (MAP, STARTSAT, FIRSTCHUNK);                                        00014200
SET (MAP, STARTSAT + FIRSTCHUNK, SIZE - FIRSTCHUNK);                    00014300
END ELSE                                                                00014400
BEGIN                                                                   00014500
G~ STMOD40 MOD 48         ;                                             00014600
SETSTREAM(MAP[STARTSAT DIV 40000,STMOD40 DIV 48],G, SIZE);              00014700
END;                                                                    00014800
END SET;                                                                00014900
INTEGER I,J, K , SEGSPERROW, ROW;                                       00015000
LABEL EXIT;                                                             00015100
FOR J~15 STEP 16 WHILE TRUE DO BEGIN                                    00015200
READ (DIRECTORY[J],30,NAMES[*]);                                        00015300
FOR I~28 STEP -2 UNTIL 0 DO                                             00015400
IF NAMES[I] = 76 THEN GO TO EXIT ELSE                                   00015500
BEGIN IF NAMES[I] ! 12 THEN                                             00015600
BEGIN READ(DIRECTORY[J- (30-I) DIV 2], 30, HEADER[*]);                  00015700
SEGSPERROW~HEADER[8];                                                   00015800
FOR ROW ~10 STEP 1 UNTIL 29 DO                                          00015900
IF(K ~ HEADER[ROW] ) = 0 THEN ROW ~ 30                                  00016000
ELSE                                                                    00016100
BEGIN                                                                   00016200
IF K GEQ 1000000 THEN K ~ K- 1000000 + 40000| EU1  ;                    00016300
SET ( MAP,K, SEGSPERROW);                                               00016400
END                                                                     00016500
END                                                                     00016600
END                                                                     00016700
END                                                                     00016800
;                                                                       00016900
EXIT: END;                                                              00017000
BEGIN FORMAT                                                            00017100
FORMAT   FM1  ( "MOD",X1, I1, X1, I7 )        ;                         00017200
FORMAT FM2 ( X8,I7 )                     ;                              00017300
FORMAT FMH ( X20, 100I1, /    X 20, 100 I 1, /   X 20, 100 I 1 )   ;    00017400
DEFINE                                                                  00017500
TONEB = LIT " "#,                                                       00017600
TONE1 = LIT "."#,                                                       00017700
TONE2 = LIT ":"#,                                                       00017800
TONE3 = LIT "="#,                                                       00017900
TONE4 = LIT "}"#,                                                       00018000
TONE5 = LIT "@"#,                                                       00018100
THRU = STEP 1 UNTIL #;                                                  00018200
ARRAY LINE [0:14];                                                      00018300
INTEGER I, J           ;                                                00018400
INTEGER Q;                                                              00018500
STREAM PROCEDURE HALFTONE (INARRAY,SSKIP, LINE);                        00018600
VALUE SSKIP;                                                            00018700
BEGIN LOCAL A;                                                          00018800
LABEL LB, L1,L2,L3,L4,L5, EXIT;                                         00018900
SI~ INARRAY;                                                            00019000
SKIP SSKIP SB;                                                          00019100
DI~ LINE;                                                               00019200
DI~ DI + 20   ;                                                         00019300
 2(50( TALLY~ 0;                                                        00019400
5 ( IF SB THEN  TALLY~ TALLY + 1;                                       00019500
SKIP SB);                                                               00019600
A~ TALLY; CI~CI+A;                                                      00019700
GO TO LB; GO TO L1; GO TO L2; GO TO L3; GO TO L4; GO TO L5;             0001980 
LB:DS~TONEB; GO TO EXIT;                                                00019900
L2:DS~TONE2; GO TO EXIT;                                                00020100
L1:DS~TONE1; GO TO EXIT;                                                00020000
L3:DS~TONE3; GO TO EXIT;                                                00020200
L4:DS~TONE4; GO TO EXIT;                                                00020300
EXIT:));                                                                00020500
L5:DS~TONE5; GO TO EXIT;                                                00020400
END HALFTONE;                                                           00020600
BEGIN                                                                   00020700
PROCEDURE TIMEMAP (A,M,D,Y,DD )         ;                               00020800
VALUE  A   ;                                                            00020900
ALPHA A, DD ;                                                           00021000
INTEGER   M,D,Y     ;                                                   00021100
BEGIN                                                                   00021200
INTEGER YR,MO,DAY,   WEEKD, I,J    ;                                    00021300
FORMAT                                                                  00021400
FIN (X3,I2, I3 )         ;                                              00021500
INTEGER ARRAY MONTHCODE [0:12], AL[0:0]  ;                              00021600
ALPHA ARRAY WEEKDAY [0:6]     ;                                         00021700
LABEL SPOIL ;                                                           00021800
DEFINE LEAPYEAR = REAL (BOOLEAN (YR) AND BOOLEAN (3) ) = 0 #  ;         00021900
COMMENT THIS PROCEDURE WILL CHANGE TIME (0) TO FORM                     00022000
MO/DAY/YR  DAY OF WEEK      THOM STONE DEC 1967  ;                      00022100
FILL MONTHCODE [*] WITH 0,0,31,59,90,120,151,181,212,243,273,304,334 ;  00022200
AL[0] ~A ;                                                              00022300
READ (AL[*],FIN,YR,DAY ) ;                                              00022400
IF YR EQL 1967 THEN GO TO SPOIL ;                                       00022500
FOR I ~ 68 STEP 1 UNTIL YR DO                                           00022600
BEGIN IF I NEQ YR AND I NEQ 1968 AND REAL ( BOOLEAN (I)                 00022700
AND BOOLEAN (3) ) EQL 0                                                 00022800
THEN J ~J +2                                                            00022900
ELSE J ~ J + 1 ;  END ;                                                 00023000
SPOIL : IF YR EQL 67 AND DAY LSS 60       THEN J ~ -1                   00023100
ELSE IF YR EQL 67 THEN J ~ -1  ;                                        00023200
IF YR EQL 68 THEN J ~ 0     ;                                           00023300
WEEKD   ~ (DAY + J ) MOD 7       ;                                      00023400
IF LEAPYEAR AND DAY EQL 60 THEN                                         00023500
BEGIN MO ~ 2  ; DAY ~ 29; END                                           00023600
ELSE IF LEAPYEAR      AND DAY GTR 60                                    00023700
THEN DAY ~ DAY -1   ;                                                   00023800
MO ~ 13 ;                                                               00023900
DO MO ~MO -1 UNTIL DAY GTR MONTHCODE     [MO]     ;                     00024000
DAY ~ DAY- MONTHCODE [MO]    ;                                          00024100
FILL WEEKDAY [*] WITH "SUN","MON","TUE","WED",                          00024200
"THU","FRI","SAT"     ;                                                 00024300
M~ MO     ;                                                             00024400
D ~DAY   ;        Y~ YR    ;    DD~ WEEKDAY [WEEKD]    ;                00024500
END   ;                                                                 00024600
INTEGER MIN,HR, MO, DATE, YR         ;                                  00024700
ALPHA DAY            ;                                                  00024800
FORMAT                                                                  00024900
FTOUT ( I2,"/", I2, "/",I2,X2,A3,X3,I2, ":",I2 )       ;                00025000
MIN ~   TIME (1) / 3600                 ;                               00025100
HR  ~ MIN DIV 60         ;                                              00025200
MIN ~ MIN MOD         60      ;                                         00025300
TIMEMAP ( TIME (0) , MO, DATE, YR, DAY  )             ;                 00025400
WRITE ( PRINT,FTOUT, MO, DATE, YR, DAY, HR, MIN)         ;              00025500
END        ;                                                            00025600
WRITE ( PRINT, FMH, FOR I ~ 0,1,2  DO                                   00025700
[ FOR J ~ 0 THRU 99 DO IF I EQL 0 THEN J DIV 20 ELSE                    00025800
IF I EQL 1 THEN (J DIV 2 ) MOD 10 ELSE                                  00025900
IF BOOLEAN ( J ) THEN 5 ELSE 0 ] )  ;                                   00026000
LINE [14] ~ "    ";                                                     00026100
FOR J~ 0 THRU 4 DO                                                      00026200
BEGIN                                                                   00026300
FOR Q~0THRU 79 DO                                                       00026400
BEGIN                                                                   00026500
G~(500|Q) MOD 48;                                                       00026600
IF Q EQL 0 THEN                                                         00026700
   WRITE (     LINE [ * ] , FM1 ,J + 1 ,                                00026800
( IF J LSS EU1 THEN 40000|J + 500|Q ELSE                                00026900
1000000 + 40000| (J - EU1) + 500 | Q       ))                           00027000
ELSE                                                                    00027100
 WRITE    (    LINE [* ] , FM2,                                         00027200
( IF J LEQ 1 THEN 40000 | J + 500    | Q  ELSE                          00027300
1000000 + 40000 | ( J- 2 ) +500 | Q           ) )           ;           00027400
HALFTONE (MAP[J,(500|Q) DIV 48],    G , LINE   ) ;                      00027500
WRITE ( PRINT, 15, LINE [ * ]   )     ;                                 00027600
END   ;                                                                 00027700
END    ;                                                                00027800
WRITE ( PRINT, FMH, FOR I ~ 0,1,2  DO                                   00027900
[ FOR J ~ 0 THRU 99 DO IF I EQL 0 THEN J DIV 20 ELSE                    00028000
IF I EQL 1 THEN (J DIV 2 ) MOD 10 ELSE                                  00028100
IF BOOLEAN ( J ) THEN 5 ELSE 0 ] )  ;                                   00028200
BEGIN                                                                   00028300
INTEGER I;                                                              00028400
FORMAT FLOG (15I8);                                                     00028500
DEFINE                                                                  00028600
PLANET = MAP#,                                                          00028700
THRU = STEP 1 UNTIL#;                                                   00028800
        PROCEDURE CHARTISLANDS(       SHIPSLOG);                        00028900
ARRAY SHIPSLOG [0];                                                     00029000
BEGIN                                                                   00029100
MONITOR  INDEX;                                                         00029200
INTEGER LANDMASS, WATERAREA;                                            00029300
OWN                                                                     00029400
INTEGER LEAGUES, BUOY         ;                                         00029500
INTEGER WATERSIGHTED;                                                   00029600
OWN                                                                     00029700
INTEGER BEACON;                                                         00029800
LABEL PORTAGE, ENDOFVOYAGE, ISLAND, SEA    ;                            00029900
PROCEDURE RECORDISLANDINLOG(SHIPSLOG,ENDOFVOYAGE);                      00030000
ARRAY SHIPSLOG[0]; LABEL ENDOFVOYAGE;                                   00030100
BEGIN                                                                   00030200
INTEGER MARKER;                                                         00030300
INTEGER STREAM PROCEDURE COUNT(A);                                      00030400
BEGIN SI~A; 48(IF SB THEN JUMP OUT ELSE                                 00030500
BEGIN TALLY~TALLY+1; SKIP SB END);                                      00030600
COUNT~ TALLY END COUNT;                                                 00030700
SHIPSLOG[0] ~ SHIPSLOG[0]+1;                                            00030800
SHIPSLOG[SHIPSLOG[0]] ~                                                 00030900
(BEACON DIV 834) | 40000 +(BEACON MOD 834 ) | 48 +                      00031000
           ( COUNT  (PLANET[BEACON  DIV 834,BEACON  MOD 834]));         00031100
END RECORDISLANDINLOG;                                                  00031200
BOOLEAN PROCEDURE SURFACEISWET;                                         00031300
BEGIN                                                                   00031400
BOOLEAN STREAM PROCEDURE ALLZERO(A);                                    00031500
BEGIN LOCAL B;                                                          00031600
SI~A; DI~ LOC B;                                                        00031700
IF 8 SC = DC THEN TALLY~1;                                              00031800
ALLZERO~TALLY; END ALLZERO;                                             00031900
SURFACEISWET~ALLZERO(PLANET[LEAGUES DIV 834,LEAGUES MOD 834]);          00032000
END SURFACEISWET;                                                       00032100
DEFINE                                                                  00032200
SAIL = DO#, SOTHAT = DO#,PORT = DO#,                                    00032300
PROCEED = DO#,                                                          00032400
ANOTHERLEAGUE = LEAGUES~LEAGUES + 1 #,                                  00032500
SURFACEISDRY = NOT SURFACEISWET#,                                       00032600
MEASURE = FOR BUOY~ BUOY + 1 STEP 1 UNTIL #,                            00032700
LOWERDINGHY = BEACON ~ LEAGUES#,                                        00032800
DROPANCHOR = BUOY ~ 0#,                                                 00032900
CASTOFF= LEAGUES~ 0#,                                                   00033000
TRIANGULATE =  WATERSIGHTED ~ BUOY + WATERAREA #,                       00033100
ARCHIPELAGO =    BUOY LEQ LANDMASS #                                    00033200
;                                                                       00033300
IF COMMON MOD 10000 = 0 THEN                                            00033400
BEGIN                                                                   00033500
LANDMASS ~                                                              00033600
8                                                                       00033700
;                                                                       00033800
WATERAREA ~                                                             00033900
0                                                                       00034000
END ELSE     BEGIN                                                      00034100
LANDMASS ~ COMMON MOD 100; WATERAREA ~ (COMMON DIV 100) MOD 100; END    00034200
;                                                                       00034300
INDEX~ENDOFVOYAGE;                                                      00034400
CASTOFF;                                                                00034500
PORTAGE:                                                                00034600
DROPANCHOR;                                                             00034700
PROCEED                                                                 00034800
     ANOTHERLEAGUE  UNTIL SURFACEISWET;                                 00034900
MEASURE WATERAREA SOTHAT IF SURFACEISWET THEN ANOTHERLEAGUE             00035000
ELSE GO TO PORTAGE;                                                     00035100
SEA: SAIL ANOTHERLEAGUE UNTIL SURFACEISDRY;                             00035200
LOWERDINGHY;                                                            00035300
DROPANCHOR;                                                             00035400
MEASURE LANDMASS SOTHAT IF SURFACEISDRY THEN ANOTHERLEAGUE ELSE         00035500
GO TO ISLAND;                                                           00035600
GO TO PORTAGE;                                                          00035700
DROPANCHOR;                                                             00035800
ISLAND:                                                                 00035900
TRIANGULATE;                                                            00036000
MEASURE WATERSIGHTED SOTHAT IF SURFACEISDRY THEN IF                     00036100
ARCHIPELAGO THEN GO TO ISLAND ELSE GO TO PORTAGE ELSE ANOTHERLEAGUE;    00036200
RECORDISLANDINLOG(SHIPSLOG, ENDOFVOYAGE);                               00036300
GO TO SEA;                                                              00036400
ENDOFVOYAGE:                                                            00036500
END CHARTISLANDS;                                                       00036600
CHARTISLANDS (SHIPSLOG);                                                00036700
WRITE (PRINT,FLOG,FOR      I~0 THRU 30 DO SHIPSLOG[I]);                 00036800
FOR I~1 THRU 30 DO IF SHIPSLOG[I] GTR 40000| EU1     THEN               00036900
SHIPSLOG[I] ~ 1000000 + SHIPSLOG[I] - 40000| EU1   ;                    00037000
WRITE (PRINT,FLOG,FOR      I~0 THRU 30 DO SHIPSLOG[I]);                 00037100
END;                                                                    00037200
BEGIN                                                                   00037300
INTEGER PROCEDURE NAMEISLANDS (SHIPSLOG, REGISTER);                     00037400
ARRAY SHIPSLOG, REGISTER [0];                                           00037500
BEGIN                                                                   00037600
INTEGER ISLAND, TOTALISLANDS, L;                                        00037700
INTEGER I,J, K , SEGSPERROW, ROW;                                       00037800
INTEGER FOUND;                                                          00037900
LABEL EXIT;                                                             00038000
TOTALISLANDS~SHIPSLOG[0];                                               00038100
FOR J~15 STEP 16 WHILE TRUE DO BEGIN                                    00038200
READ (DIRECTORY[J],30,NAMES[*]);                                        00038300
FOR I~28 STEP -2 UNTIL 0 DO                                             00038400
IF NAMES[I] = 76 THEN GO TO EXIT ELSE                                   00038500
BEGIN IF NAMES[I] ! 12 THEN                                             00038600
BEGIN READ(DIRECTORY[J- (30-I) DIV 2], 30, HEADER[*]);                  00038700
SEGSPERROW~HEADER[8];                                                   00038800
FOR ROW ~10 STEP 1 UNTIL 29 DO                                          00038900
IF(K ~ HEADER[ROW] ) = 0 THEN ROW ~ 30                                  00039000
ELSE                                                                    00039100
FOR ISLAND ~1 STEP 1 UNTIL TOTALISLANDS DO                              00039200
IF K LEQ SHIPSLOG[ISLAND] THEN IF                                       00039300
         (K+SEGSPERROW) GTR SHIPSLOG[ISLAND] THEN                       00039400
BEGIN                                                                   00039500
FOUND~FOUND+1;                                                          00039600
K~30;                                                                   00039700
REGISTER[L]~ NAMES [I];                                                 00039800
REGISTER[L+1]~ NAMES [I+1];                                             00039900
L~L+2;                                                                  00040000
SHIPSLOG[ISLAND]~0;                                                     00040100
ISLAND~TOTALISLANDS+1;                                                  00040200
LASTI~I;                                                                00040300
LASTJ~J;                                                                00040400
END;                                                                    00040500
END                                                                     00040600
END                                                                     00040700
END                                                                     00040800
;                                                                       00040900
EXIT:                                                                   00041000
NAMEISLANDS~L;                                                          00041100
END;                                                                    00041200
PROCEDURE UNCHECKER (FILES, REGISTER);                                  00041300
VALUE FILES; INTEGER FILES; ARRAY REGISTER[0];                          00041400
BEGIN                                                                   00041500
ARRAY ZIPARRAY [0:70];                                                  00041600
STREAM PROCEDURE CLEAR40(A);                                            00041700
BEGIN DI~A;DS~ 8 LIT "        ";        SI~A;                           00041800
DS~40 WDS; DS~30WDS; END;                                               00041900
STREAM PROCEDURE SEMICOLEND(A);                                         00042000
BEGIN LOCAL B; LABEL LOOP;                                              00042100
SI~A;                                                                   00042200
34(SI~SI+16);                                                           00042300
LOOP: IF SC= " " THEN BEGIN SI~SI-1; GO TO LOOP END;                    00042400
B~SI;                                                                   00042500
DI~B;                                                                   00042600
DS~ 8 LIT ";END.   ";                                                   00042700
END;                                                                    00042800
FORMAT FDUMP ( "CC DUMP TO EMPIRE ",                                    00042900
60(A1,A6, "/", A1,A6, ",")),                                            00043000
FREMOVE ("CC REMOVE ",                                                  00043100
60(A1,A6, "/", A1,A6, ",")),                                            00043200
FLOAD                                                                   00043300
("CC LOAD FROM EMPIRE =/=; END. ");                                     00043400
INTEGER Q;                                                              00043500
CLEAR40(ZIPARRAY);                                                      00043600
WRITE (ZIPARRAY[*], FDUMP,                                              00043700
FOR Q~0 STEP 2 UNTIL FILES-1 DO [                                       00043800
REGISTER[Q].[6:6],     REGISTER [Q  ].[12:36],                          00043900
REGISTER [Q+1].[6:6],  REGISTER [Q+1].[12:36]   ] );                    00044000
SEMICOLEND(ZIPARRAY);                                                   00044100
WRITE (PRINT,15,ZIPARRAY [*]);                                          00044200
ZIP WITH ZIPARRAY [*];                                                  00044300
CLEAR40(ZIPARRAY);                                                      00044400
WRITE (ZIPARRAY[*], FREMOVE,                                            00044500
FOR Q~0 STEP 2 UNTIL FILES-1 DO [                                       00044600
REGISTER[Q].[6:6],     REGISTER [Q  ].[12:36],                          00044700
REGISTER [Q+1].[6:6],  REGISTER [Q+1].[12:36]   ] );                    00044800
SEMICOLEND(ZIPARRAY);                                                   00044900
WRITE (PRINT,15,ZIPARRAY [*]);                                          00045000
ZIP WITH ZIPARRAY [*];                                                  00045100
WHEN (10);                                                              00045200
DO BEGIN                                                                00045300
WHEN (10);                                                              00045400
READ(DIRECTORY[0], 30,NAMES[*]);                                        00045500
READ(DIRECTORY[LASTJ],30,NAMES[*]); END                                 00045600
UNTIL NAMES[LASTI]= 12;                                                 00045700
WRITE (ZIPARRAY [*], FLOAD );                                           00045800
WRITE (PRINT,15,ZIPARRAY [*]);                                          00045900
ZIP WITH ZIPARRAY [*];                                                  00046000
WHEN (10);                                                              00046100
END UNCHECKER;                                                          00046200
ALPHA ARRAY REGISTER [0:60];                                            00046300
INTEGER I;                                                              00046400
I~                                                                      00046500
NAMEISLANDS (SHIPSLOG,REGISTER);                                        00046600
WRITE (PRINT,15,REGISTER[*]);                                           00046700
IF I!0 THEN                                                             00046800
UNCHECKER (I,REGISTER);                                                 00046900
REPEAT~I;                                                               00047000
END                                                                     00047100
END;                                                                    00047200
IF REPEAT ! 0 THEN BEGIN                                                00047300
STREAM PROCEDURE CLEAR836 (ROW);                                        00047400
BEGIN                                                                   00047500
DI~ ROW ;   DS~ 8 LIT "00000000";                                       00047600
SI~ ROW; 13 (DS~ 60 WDS);   DS~ 55 WDS;                                 00047700
END CLEAR836;                                                           00047800
INTEGER I;                                                              00047900
FOR I~ 0 STEP 1 UNTIL MODS-1 DO                                         00048000
CLEAR836 (    MAP[I,*]);                                                00048100
GO TO HEREWEGOAGAIN;                                                    00048200
END;                                                                    00048300
END.                                                                    00048400
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
