SORTGEN     000106AA                                                    00000100
                                                                        00001000
                                   W. C. PRICE                          00002000
                                   PROGRAMMING SYSTEMS DEPARTMENT       00003000
                                   PASADENA                             00004000
                                   APRIL, 1967                          00005000
COMMENT                                                                 00006000
************************************************************************00007000
                                                                        00008000
         NOTE THAT THIS IS NOT AN OFFICIAL PRODUCT OF THE PROGRAMMING   00009000
SYSTEMS DEPT., NOR OF THE BURROUGHS CORP., AND THAT NO RESPONSIBILITY   00010000
IS ASSUMED BY THE BURROUGHS CORPORATION FOR THE USE OR MAINTENANCE OF   00011000
THIS PROGRAM.                                                           00012000
                                                                        00013000
***********************************************************************;00014000
                                                                        00015000
         THIS PROGRAM GENERATES A PROGRAM IN BURROUGHS EXTENDED ALGOL   00016000
FOR THE B-5500, TO PERFORM A SORT AS SPECIFIED BY ITS INPUT, SPECIFI-   00017000
CATION CARDS.  OPTIONALLY, THE GENERATED PROGRAM WILL BE COMPILED TO THE00018000
LIBRARY, OR FOR IMMEDIATE EXECUTION, OR LEFT ON THE DISK IN SYMBOLIC    00019000
FORM FOR USER MODIFICATION.  THE INPUT CONTAINS ONE SORT SPECIFICATION  00020000
CARD, FOLLOWED BY A KEY SPECIFICATION FOR EACH SORTING FIELD.  THESE    00021000
CARDS ARE READ BY THE GENERATOR FROM A FILE CALLED "SPEC".  THE FORMAT  00022000
OF THE SORT SPECIFICATION CARD IS AS FOLLOWS:                           00023000
    COLS:  FIELD DESCRIPTION:                                           00024000
     1- 7  PROGRAM IDENTIFICATION.  WHERE THIS FIELD CONTAINS |||||||,  00025000
              THE GENERATED SYMBOLIC PROGRAM IS NAMED |||||||/SYMB,     00026000
              AND THE COMPILED PROGRAM IS |||||||/SORT.  IF A DUMP-ON-  00027000
              ERROR OPTION IS SELECTED, THE ERROR FILE WILL BE |||||||/ 00028000
              ERRORS.                                                   00029000
     8-11  SORT RECORD SIZE:  THE NUMBER OF WORDS IN THE RECORD TO BE   00030000
              SORTED.                                                   00031000
    12-13  NUMBER OF SORT KEYS.                                         00032000
    14     NUMBER OF MAGNETIC TAPES THAT MAY BE USED BY THE SORT. IF NOT00033000
              BLANK, MUST BE 0, 3, 4, OR 5.                             00034000
    15     ERROR OPTION: ACTION UPON INPUT ERROR.                       00035000
              BLANK- TERMINATE THE SORT.                                00036000
                0  - PROCESS THE RECORD.                                00037000
                1  - DELETE THE ERROR RECORD(S).                        00038000
                2  - PRINT AND PROCESS.                                 00039000
                3  - PRINT AND DELETE.                                  00040000
    16     GENERATION OPTION.                                           00041000
                L  - COMPILE THE PROGRAM TO THE LIBRARY.                00042000
                S  - SYNTAX-CHECK AND LIST THE GENERATED PROGRAM.       00043000
              ANY OTHERS CAUSE THE GENERATED PROGRAM TO BE COMPILED,    00044000
              EXECUTED, AND BOTH THE SYMBOLIC AND COMPILED PROGRAMS TO  00045000
              BE REMOVED FROM THE DISK LIBRARY.                         00046000
    17-23  INPUT FILE IDENTIFICATION PREFIX: THE "FIRST NAME" OF THE    00047000
              FILE TO BE SORTED.  IF BLANK, THIS WILL BE CHANGED TO ALL 00048000
              ZEROES.                                                   00049000
    24-30  INPUT FILE IDENTIFICATION:  THE "SECOND NAME" OF THE INPUT.  00050000
    31     INPUT MEDIUM:  D= SERIAL DISK, OTHER=NON-DISK.               00051000
    32-35  INPUT RECORD SIZE IN WORDS, IF DIFFERENT FROM THE SORT       00052000
              RECORD SIZE.                                              00053000
    36-39  INPUT BLOCK SIZE, IF INPUT FILE IS BLOCKED.                  00054000
    40-46  OUTPUT FILE IDENTIFICATION PREFIX: SEE COLS 17-23.           00055000
    47-53  OUTPUT FILE IDENTIFICATION.                                  00056000
    54     OUTPUT MEDIUM: D= SERIAL DISK,OTHER NON-NUMERIC=TAPE         00057000
              IF NUMERIC, FOLLOWS STANDARD DESIGNATION.                 00058000
    55-58  OUTPUT RECORD SIZE, IF NOT EQUAL TO SORT RECORD SIZE.        00059000
              IF SPECIFIED, AND NOT SAME AS SORT RECORD SIZE, AN OUTPUT 00059100
              PROCEDURE WILL BE GENERATED.                              00059200
    59-62  OUTPUT BLOCK SIZE, IF OUTPUT IS BLOCKED.                     00060000
    63-65  SAVE-FACTOR FOR OUTPUT FILE.                                 00061000
    66-67  NUMBER OF AREAS IF OUTPUT FILE IS A NEW DISK FILE.           00062000
    68-71  AREA SIZE IN RECORDS IF OUTPUT IS NEW DISK FILE.             00063000
    72-73  MEMORY SIZE, IN "K" (1024).  IF EMPTY, 12000 WILL BE USED.   00064000
    74-78  DISK SIZE, IN 1000-WORDS.  IF EMPTY, 600000 WILL BE USED.    00065000
         WHERE A FIELD IS OPTIONAL, IT MAY BE LEFT BLANK OR FILLED WITH 00066000
ZEROES.                                                                 00067000
                                                                        00068000
------------------------------------------------------------------------00069000
                                                                        00070000
         THE FORMAT OF A KEY SPECIFICATION CARD IS AS FOLLOWS:          00071000
     1- 2  KEY NUMBER.  MUST NOT BE BLANK OR ZERO OR GREATER THAN THE   00072000
              NUMBER OF KEYS GIVEN IN COLS 12-13 OF THE SORT SPECIFI-   00073000
              CATION CARD.  DURING THE SORT, KEY NUMBER 1 IS THE MAJOR  00074000
              KEY.  THE KEY CARDS NEED NOT BE PRESENTED IN ORDER.       00075000
     3     ORDER.  IF "D", THE KEY IS DESCENDING: OTHERWISE, ASCENDING. 00076000
     4     MODE.  IF "O", THE KEY IS SINGLE-PRECISION COMPUTATIONAL,    00077000
              AND ITS SIZE IS ASSUMED TO BE ONE WORD.  IF ANYTHING ELSE,00078000
              THE KEY IS DISPLAY.                                       00079000
     5- 8  START OF KEY.  IF KEY IS COMPUTATIONAL, THE WORD WITHIN THE  00080000
              RECORD WHICH CONTAINS THE KEY.  IF DISPLAY, THE           00081000
              STARTING CHARACTER POSITION.  THE FIRST POSITION IN THE   00082000
              RECORD IS NUMBERED 1.                                     00083000
     9-10  KEY SIZE, IF DISPLAY.  IF GREATER THAN 63, IT WILL BE        00084000
              REDUCED TO 63.                                            00085000
                                                                        00086000
------------------------------------------------------------------------00087000
                                                                        00088000
SORT GENERATOR PROGRAM                                                  00089000
BEGIN                                                                   00090000
    ALPHA PID1,    % FIRST 3 CHARS OF PROGID                            00091000
          PID2,    % LAST 4 CHARS OF PROGID                             00092000
          ERROPTN, % OPTIONS FOR ERROR HANDLING                         00093000
          GENOPTN, % GENERATION OPTION                                  00094000
          NIP1,    % INPUT FILE ID PREFIX (MFID)                        00095000
          NIP2,                                                         00096000
          NID1,    % INPUT FILE ID                                      00097000
          NID2,                                                         00098000
          OIP1,    % OUTPUT FILE ID PREFIX                              00099000
          OIP2,                                                         00100000
          OID1,    % OUTPUT FILE ID                                     00101000
          OID2,                                                         00102000
          IN1,     % "DISK" IF DISK INPUT, BLANK IF NOT                 00103000
          IN2,     % "SERIAL" IF DISK IN                                00104000
          OT1,     % "DISK"  FOR OUTPUT                                 00105000
          OT2;     % "SERIAL"                                           00106000
    REAL  LINR;    % RUNNING LINE NR FOR OUTPUT                         00107000
    INTEGER RECSZ, % SORT RECORD SIZE                                   00108000
          INBLKSZ, % INPUT BLOCK SIZE                                   00109000
          INRECSZ, % INPUT RECORD SIZE                                  00110000
          NROWS,   % # ROWS IF DISK OUTPUT, ELSE FILE TYPE              00111000
          ROWSZ,   % OUTPUT ROW SIZE                                    00112000
          OUTRECSZ,%   "    RECORD SIZE                                 00113000
          OBLKSZ,  %   "    BLOCK   "                                   00114000
          SAVETIME,% SAVE TIME FOR OUTPUT FILE                          00115000
          NKEYS,   % # KEYS                                             00116000
          KEY,     % INDEX OF KEY BEING PROCESSED.                      00117000
          I,J,K,M,N,% SCRATCH VARIABLES                                 00118000
               CORESZ, % CORE SIZE                                      00119000
          DKSZ,    % DISK SIZE                                          00120000
          NTAPES;  % # BACKUP TAPES FOR ROY                             00121000
    BOOLEAN DKOT , % DISK OUTPUT                                        00122000
          DSPKEY,  % ONE OR MORE DISPLAY KEYS                           00123000
          OUTPRO,  % OUTPUT PROCEDURE NEEDED                            00124000
          INPRO,   % INPUT PROCEDURE NEEDED                             00125000
          DMPOPTN, % PRINT ERROR RECORDS                                00126000
          OMITOPTN;% BYPASS ERROR RECORDS                               00127000
    REAL STREAM PROCEDURE LINO(L);                                      00128000
    BEGIN DI ~ LOC LINO; DS~5 LIT "00001"; DI~DI-5; SI~L   ; DS~8 ADD;  00129000
         SI~LOC LINO; DI ~L; DS~WDS                                     00130000
    END LINO;                                                           00131000
    DEFINE L=LINO(LINR)#;                                               00132000
    FILE IN SPEC(2,10);                                                 00133000
    FILE OUT DISK DISK SERIAL [20:90] "SYMB" (2,10,150,SAVE 10);        00134000
    FORMAT FIN(A3,A4,I4,I2,I1,2A1,2(2(A3,A4),A1,2I4),I3,I2,I4,I2,I5   ),00135000
           FKE(I2,2A1,I4,I2);                                           00136000
    LIST LIN  (PID1,PID2,RECSZ,NKEYS,NTAPES,ERROPTN,GENOPTN,NIP1,NIP2,  00137000
               NID1,NID2,IN1,INRECSZ,INBLKSZ,OIP1,OIP2,OID1,OID2,OT1,   00138000
               OUTRECSZ,OBLKSZ,SAVETIME,NROWS,ROWSZ,CORESZ,DKSZ       );00139000
COMMENT *************** FIRST CODE--READ THE SPEC CARD ****************;00140000
    READ(SPEC,FIN,LIN);                                                 00141000
    BEGIN ARRAY ORDER,MODE,WORD,    SIZE [0:NKEYS];                     00142000
         INTEGER ARRAY KXC[0:NKEYS];                                    00143000
    INTEGER PROCEDURE STAR(B); VALUE B; BOOLEAN B;                      00144000
         STAR~ IF B THEN 1 ELSE -1;                                     00145000
    DEFINE DMPTOG= STAR(DMPOPTN)#, OMITOG=STAR(OMITOPTN)#;              00146000
    FORMAT F1 (X14,"SORTGEN GENERATED PROGRAM ","""A3,A4                00147000
                                                     ""/SORT",X18,O/    00148000
               "BEGIN",X67,O/                                           00149000
               X4,"FILE INPUT ",A4,A7,2(X1,""",A3,A4,"""),              00150000
                    "(2,",I4,",",I4,");",X12,O/                         00151000
               X4,"FILE OUTPUT ",A4,A7,A2,I2,A1,V4,A1,2(X1,""",A3,A4,   00152000
              """),             ,X15,O/                                 00153000
               X45,"(2,",I4,",",I4,",SAVE ",I3,");    ",O),             00154000
           F2 (X4,"BOOLEAN TEMP;",X55,O/                                00155000
               X4,"STREAM PROCEDURE FILLUP(A,S,N,C); VALUE S,N,C;",     00156000
                   X22,O/                                               00157000
               X4,"BEGIN SI~LOC C; SI~SI+7; DI~A; DI~DI+S;",X29,O/      00158000
               X9,"N(DS~CHR; SI~SI-1);",X44,O/                          00159000
               X4,"END FILLUP;",X57,O),                                 00160000
           F3 (X9,"BOOLEAN STREAM PROCEDURE K(SZ,SK,A,B); VALUE SZ,SK;",00161000
                   X12,O/                                               00162000
               X9,"BEGIN SI~A; SI~SI+SK; A~SI; DI~B; DI~DI+SK; B~DI;",  00163000
                   X14,O/                                               00164000
               X14,"IF SZ SC<DC THEN TALLY~1 ELSE",X29,O/               00165000
               X14,"BEGIN SI~A; DI~B; IF SZ SC!DC THEN TALLY~2 END;",   00166000
                   X11,O/                                               00167000
               X14,"K~TALLY",X51,O/                                     00168000
               X9,"END K;",X57,O),                                      00169000
           F4 (X4,"BOOLEAN PROCEDURE COMPARE(A,B); ARRAY A,B[0];",X23,O/00170000
               X4,"BEGIN",X63,O),                                       00171000
           F5 (X9,"IF REAL(COMPARE~K(",I2,",",I1,2(",",A1,"[",I4,"]"),  00172000
                   "))=0 THEN",X16,O),                                  00173000
           F6 (X9,"COMPARE~REAL(K(",I2,",",I1,2(",",A1,"[",I4,"]"),     00174000
                   "))!2",X24,O),                                       00175000
           F7 (X9,"IF ",A1,"[",I4,"]<",A1,"[",I4,"] THEN COMPARE~TRUE", 00176000
                   " ELSE",X22,O/                                       00177000
               X14,"IF A[",I4,"]!B[",I4,"] THEN COMPARE~FALSE ELSE",X16,00178000
                   O),                                                  00179000
           F8 (X9,"COMPARE~",A1,"[",I4,"]{",A1,"[",I4,"]",X40,O),       00180000
           F9 (X4,"END OF COMPARE PROCEDURE;",X43,O/                    00181000
               X4,"PROCEDURE STOPPER(RECORD); ARRAY RECORD[0];",X25,O/  00182000
               X4,"BEGIN",X63,O),                                       00183000
           F10(X9,"FILLUP(RECORD[",I4,"],",I1,2(",",I2),");",X34,O),    00184000
           F11(X9,"RECORD[",I4,"]~",A1,"REAL((NOT FALSE).[3:45]);",     00185000
                   X24,O),                                              00186000
           F12(X4,"END OF STOPPER PROCEDURE;",X43,O/                    00187000
              *(X4"READ(MYSELF); CLOSE(MYSELF,PURGE);"X34,O/),          00188000
               X4,"SORT(OUTP",A2,",INP",A2,",",I1,",STOPPER,COMPARE,",  00189000
                   I4","I5","I8");"X11,O/                               00190000
               "END OF SORT PROGRAM.  ",A3,A4,"/SORT",X38,O),           00191000
           F13(X4,"INTEGER RECORDCOUNT;",X48,O/                         00192000
               X4,"FILE OUT PRINTER 4 ",""",A3,A4,""",""ERRORS",        00193000
                   "" (2,15);",X24,O/                                   00194000
               X4,"PROCEDURE PRINT(RECORD); ARRAY RECORD[*];",X27,O/    00195000
               X4,"BEGIN ARRAY LINE[0:14];",X45,O/                      00196000
               *(X9,"INTEGER N;",X53,O/),                               00197000
               X9,"FORMAT E(",""INPUT PARITY ERROR, RECORD ","",I6,",   00198000
                   "":","");",X16,O/                                    00199000
               X9,"STREAM PROCEDURE MOVE(N,R,L); VALUE N;",X25,O/       00200000
               X9,"BEGIN DI~L; DS~8 LIT","" ",""; SI~L; DS~14 WDS; ",   00201000
                   "SI~R; DI~L;",X10,O/                                 00202000
               X14,"DI~DI+8; DS~N WDS",X41,O/                           00203000
               X9,"END MOVE;",X54,O/                                    00204000
               X9,"WRITE(PRINTER,E,RECORDCOUNT);",X34,O),               00205000
           F14(X9,"FOR N~0 STEP 14 UNTIL ",I4," DO",X34,O/              00206000
               X9,"BEGIN MOVE(14,RECORD[N],LINE); WRITE(PRINTER,15,",   00207000
                   "LINE[*]) END;  ",O),                                00208000
           F15(X9,"MOVE(",I2,",RECORD[",I4,"],LINE); WRITE(PRINTER,",   00209000
                   "15,LINE[*]);",X9,O),                                00210000
           F16(X9,"WRITE(PRINTER[DBL])",X44,O/                          00211000
               X4,"END PRINT;",X58,O),                                  00212000
           F17(X4,"BOOLEAN PROCEDURE INPRO(RECORD); ARRAY RECORD[0];",  00213000
                   X19,O/                                               00214000
               X4,"BEGIN LABEL EOF,PAR,REREAD,SORT;",X36,O/             00215000
               *(X9,"RECORDCOUNT~RECORDCOUNT+1;",X37,O/),               00216000
               X9,"INPRO~TRUE;",X52,O/                                  00217000
               X4,"REREAD:",X61,O/                                      00218000
               X9,"READ(INPUT,",I4,",RECORD[*])[EOF:PAR];",X27,O/       00219000
               X9,"INPRO~FALSE;",X51,O/                                 00220000
               X9,"GO TO SORT;",X52,O/                                  00221000
               X4,"PAR:",X64,O/                                         00222000
               *(X9,"PRINT(RECORD);",X49,O/),                           00223000
               *(X9,"READ(INPUT)[EOF];",X46,O/),                        00224000
               X9,"GO TO REREAD;",X50,O/                                00225000
               X4,"EOF:",X64,O/                                         00226000
             *(X9,"CLOSE(PRINTER);",X48,O/),                            00227000
               X4,"SORT:",X63,O/                                        00228000
               X4,"END OF INPUT PROCEDURE;",X45,O);                     00229000
    FORMAT F18(X4,"SORT PROGRAM ",""",A3,A4,"" SPECIFICATIONS:",X30,O/  00230000
               X9,"SORTING ",I4,"-WORD RECORDS, ON ",I2," KEYS, USING ",00231000
                   I1," TAPES.",X10,O/                                  00232000
               2(X9,A3,"PUT IS ",""",A3,A4,""/",""",A3,A4,"", WITH ",   00233000
                   I4,"-WORD RECORDS",X10,O/                            00234000
                 X14,"IN ",I4,"-WORD BLOCKS, ",*X37,                    00235000
                   *("ON DISK ",*("IN ",I2," AREAS OF ",I4," RECORDS  ")00236000
                               ,*X29), O/),                             00237000
               X14,"TO BE RETAINED ",I3," DAYS.",X34,O/                 00238000
               X9,"THE INPUT-ERROR OPTION SELECTED IS ",                00239000
                   *("TERMINATE.",X7),                                  00240000
                   *("DELETE.",X10),                                    00241000
                   *("DUMP AND DELETE. "),                              00242000
                   *("DUMP AND PROCESS."),                              00243000
                   *("PROCESS.",X9),                                    00244000
                   X11,O/                                               00245000
               X9,"THE KEYS ARE",X51,O                                  00246000
               *(/X14,*(I2," CHARACTERS, STARTING WITH CHARACTER ",     00247000
                        I4),                                            00248000
                      *("THE ",I4,"-TH WORD"),                          00249000
                      *(", ASCENDING "), *(", DESCENDING"), X3,         00250000
                      *X27,O));                                         00251000
    FORMAT F19 (X4"FILE MYSELF DISK """"A3,A4"""""SYMB""" (1,30);"X28,O)00252000
              ;                                                         00253000
    LIST L1   (PID1,PID2,L,L,IN1,IN2,NIP1,NIP2,NID1,NID2,               00254000
               INRECSZ,INBLKSZ,L,                                       00255000
               OT1,OT2,IF DKOT THEN " [" ELSE "  ",NROWS,IF DKOT THEN   00256000
               ":" ELSE " ",IF DKOT THEN "I" ELSE 17,IF DKOT THEN       00257000
               ROWSZ ELSE "    ",IF DKOT THEN "]" ELSE 48, OIP1,OIP2,   00258000
               OID1,OID2,L,                                             00259000
            OUTRECSZ,OBLKSZ,SAVETIME,L),                                00260000
         L2   (L,L,L,L,L),                                              00261000
         L3   (L,L,L,L,L,L),                                            00262000
         L4   (L,L),                                                    00263000
         L5   (SIZE[KEY],KXC[KEY],17+ORDER[KEY],WORD[KEY],18-ORDER[KEY],00264000
               WORD[KEY],L);                                            00265000
    DEFINE L6=L5#,L16=L4#;                                              00266000
    LIST L7   (17+ORDER[KEY],WORD[KEY],18-ORDER[KEY],WORD[KEY],L,       00267000
               WORD[KEY],WORD[KEY],L),                                  00268000
         L8   (17+ORDER[KEY],WORD[KEY],18-ORDER[KEY],WORD[KEY],L),      00269000
         L9   (L,L,L),                                                  00270000
         L10  (WORD[KEY],KXC[KEY],SIZE[KEY],IF ORDER[KEY]!0 THEN " "    00271000
               ELSE 12,L),                                              00272000
         L11  (WORD[KEY],IF ORDER[KEY]=0 THEN " " ELSE "-",L),          00273000
         L12  (L,I~STAR(GENOPTN!"L" AND GENOPTN!"S"),FOR J~I STEP -10   00274000
               WHILE J>0 DO L,                                          00275000
                 IF OUTPRO THEN "RO" ELSE "UT",IF INPRO THEN "RO" ELSE  00276000
               "UT",NTAPES,RECSZ, CORESZ,DKSZ,                          00277000
               L,PID1,PID2,L),                                          00278000
         L13  (L,PID1,PID2,L,  L,L,STAR(INRECSZ=14),                    00279000
                                              FOR KEY~REAL(INRECSZ{14)  00280000
               STEP 1 UNTIL 6 DO L),                                    00281000
         L14  (INRECSZ-14,L,L),                                         00282000
         L15  (INRECSZ MOD 14,INRECSZ DIV 14 | 14,L),                   00283000
         L17  (L,L,DMPTOG ,FOR KEY~REAL(TRUE AND NOT DMPOPTN) STEP 1    00284000
               UNTIL 2 DO-L, INRECSZ,L,L,L,L,                           00285000
                             DMPTOG , FOR KEY~1 STEP 1 UNTIL REAL       00286000
               (DMPOPTN) DO L,OMITOG  , FOR KEY~-1 STEP 1 UNTIL REAL    00287000
               (OMITOPTN) DO L, DMPTOG , FOR KEY~-1 STEP 1 UNTIL        00288000
               REAL(DMPOPTN  ) DO L);                                   00289000
    LIST L18  (PID1,PID2,L,RECSZ,NKEYS,NTAPES,L,                        00290000
               " IN",NIP1,NIP2,NID1,NID2,INRECSZ,L, INBLKSZ,IN1!"DISK", 00291000
               STAR(IN1="DISK"), FOR I~-1 STEP 1 WHILE I<1 AND          00292000
                   IN1="DISK" DO I,L  ,                                 00293000
               "OUT",OIP1,OIP2,OID1,OID2,OUTRECSZ,L,                    00294000
               OBLKSZ, TRUE AND NOT DKOT,     STAR(DKOT),               00295000
                   FOR I~0 STEP 1 WHILE DKOT AND I=0 DO [               00296000
                   FOR J~0 STEP 1 WHILE NROWS!0 AND J=0 DO[1,NROWS,     00297000
                        ROWSZ],STAR(NROWS=0)],L,                        00298000
               SAVETIME,L,                                              00299000
               STAR(ERROPTN>3),STAR(ERROPTN=1),STAR(ERROPTN=3),         00300000
               STAR(ERROPTN=2),STAR(ERROPTN=0),L,                       00301000
               L,NKEYS,FOR I~1 STEP 1 UNTIL NKEYS DO                    00302000
                  [FOR J~0 STEP 1 WHILE MODE[ I ]!"O" AND J=0 DO [1,    00303000
                    SIZE[I], WORD[I]|8+KXC[I],-1],                      00304000
                   FOR J~0 STEP 1 WHILE MODE[I]="O" AND J=0 DO          00305000
                  [-1,1,WORD[I]],                                       00306000
               STAR(ORDER[I]=0),STAR(ORDER[I]=1), MODE[I]="O",L]);      00307000
    LIST L19  (PID1,PID2,L);                                            00308000
         PROCEDURE CHECKID(A,B); ALPHA A,B;                             00309000
              IF A="   " AND B="    " THEN A~B~0;                       00310000
COMMENT ********** CHECK THE SPEC CARD AND READ THE KEY CARDS *********;00311000
         CHECKID(NIP1,NIP2);                                            00312000
         CHECKID(OIP1,OIP2);                                            00313000
         IF INRECSZ = 0 THEN INRECSZ ~ RECSZ ;                          00314000
         IF OUTRECSZ=0 THEN OUTRECSZ~RECSZ ELSE OUTPRO~RECSZ!OUTRECSZ;  00315000
         CORESZ~CORESZ|1024;  DKSZ~DKSZ|1000;                           00316000
         IF IN1="D" THEN BEGIN IN1~"DISK"; IN2~"SERIAL" END ELSE        00317000
                   IN1~IN2~"      ";                                    00318000
         IF OT1="D" THEN BEGIN                                          00319000
                   OT1~"DISK"; OT2~"SERIAL";                            00320000
                   DKOT ~ TRUE;                                         00321000
                         END ELSE                                       00322000
              BEGIN IF OT1 > 9 THEN NROWS~2 ELSE NROWS~ OT1;            00323000
                   OT1 ~ OT2 ~ "      ";                                00324000
              END;                                                      00325000
   IF(INPRO~ERROPTN { 3)THEN                                            00326000
              BEGIN DMPOPTN ~ ERROPTN >1;                               00327000
                   OMITOPTN ~ BOOLEAN(ERROPTN.[47:1])                   00328000
              END; FOR I~1 STEP 1 UNTIL NKEYS DO ORDER[I]~-1;           00329000
         FOR KEY ~ 1 STEP 1 UNTIL NKEYS DO                              00330000
         BEGIN READ(SPEC,FKE,I,J,K,M,N); M~M-1;                         00331000
             IF I>0 AND I{NKEYS THEN                                    00332000
              IF ORDER[I]<0 THEN                                        00333000
              BEGIN ORDER[I]~REAL(J="D");                               00334000
                IF(MODE[I]~K)="O" THEN WORD[I]~ M ELSE                  00335000
             BEGIN WORD[I]~M DIV 8; DSPKEY~TRUE END;                    00336000
                   IF M+N >RECSZ|8 THEN N~RECSZ|8-M;                    00337000
                   KXC[I]~ M MOD 8;                                     00338000
                   SIZE[I]~IF N>63 THEN 63 ELSE N                       00339000
              END ELSE KEY~KEY-1 ELSE KEY~KEY-1;                        00340000
         END KEY LOOP;                                                  00341000
    FILL DISK WITH PID2&PID1[6:30:18];                                  00342000
COMMENT *************** GENERATE THE SILLY THING **********************;00343000
         WRITE(DISK,F18,L18);                                           00344000
         WRITE(DISK,F1,L1);                                             00345000
         IF GENOPTN!"S" AND GENOPTN!"L" THEN WRITE(DISK,F19,L19);       00346000
         IF DSPKEY THEN WRITE(DISK,F2,L2);                              00347000
         IF DMPOPTN THEN                                                00348000
         BEGIN WRITE(DISK,F13,L13);                                     00349000
              IF INRECSZ>14 THEN WRITE(DISK,F14,L14);                   00350000
              IF INRECSZ{14 OR INRECSZ MOD 14 ! 0 THEN                  00351000
                   WRITE(DISK,F15,L15);                                 00352000
              WRITE(DISK,F16,L16)                                       00353000
         END;                                                           00354000
         IF INPRO THEN WRITE(DISK,F17,L17);                             00355000
         WRITE(DISK,F4,L4);                                             00356000
         IF DSPKEY THEN WRITE(DISK,F3,L3);                              00357000
         FOR KEY~1 STEP 1 UNTIL NKEYS-1 DO                              00358000
         IF MODE[KEY]="O" THEN WRITE(DISK,F7,L7)                        00359000
                          ELSE WRITE(DISK,F5,L5);                       00360000
         IF MODE[NKEYS]="O" THEN WRITE(DISK,F8,L8)                      00361000
                            ELSE WRITE(DISK,F6,L6);                     00362000
         WRITE(DISK,F9,L9);                                             00363000
         FOR KEY~1 STEP 1 UNTIL NKEYS DO                                00364000
         IF MODE[KEY]="O" THEN WRITE(DISK,F11,L11)                      00365000
                          ELSE WRITE(DISK,F10,L10);                     00366000
         WRITE(DISK,F12,L12);                                           00367000
COMMENT *************** GIVE IT TO THE COMPILER ***********************;00368000
         BEGIN ARRAY A[0:14];                                           00369000
              FORMAT ZIPPER("CC COMPILE """" A3,A4""/SORT ALGOL ",      00370000
                   *("LIBRARY"),*("SYNTAX"),";ALGOL FILE CARD=""""      00371000
                    A3,A4""/SYMB SERIAL; END.");                        00372000
              WRITE(A[*],ZIPPER,PID1,PID2,STAR(GENOPTN="L"),            00373000
                   STAR(GENOPTN="S"),PID1,PID2);                        00374000
    WRITE(DISK,15,A[*]);                                                00375000
              LOCK(DISK); ZIP WITH A[*];                                00376000
         END ZIP BLOCK;                                                 00377000
    END;                                                                00378000
END OF SORTGEN.                                                         00379000
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
