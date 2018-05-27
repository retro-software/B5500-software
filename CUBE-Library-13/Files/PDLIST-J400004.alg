         BEGIN                                                          00000100
COMMENT        CUBE LIBRARY NUMBER IS J400004.  THE PROGRAM NAME IS     00000200
               "PD/LIST".       THIS VERSION DATED 5/8/68;              00000300
COMMENT       LIST AND PUNCH THE DISK DIRECTORY ;                       00000400
BOOLEAN       PUNCHTOG ;                                                00000500
FILE          DIRECTORY DISK "DIRCTRY" "DISK" (1,30) ;                  00000600
FILE OUT      LINE 4(2,15) ;                                            00000700
FILE OUT      PUNCH 0(2,10) ;                                           00000800
DEFINE        DIR = DIRECTORY # ;                                       00000900
BOOLEAN       DEBUG ;                                                   00001000
INTEGER            D,E,F;                                               00001100
INTEGER       SEQ ;                                                     00001200
INTEGER       WI,RI ;                                                   00001300
INTEGER            C,I,J,K,L,M,N;                                       00001400
LABEL              EXIT,PICK,SKIPP;                                     00001500
ARRAY              NA,H[0:29];                                          00001600
LABEL         EOF ;                                                     00001700
ARRAY         B[0:300,0:16] ;                                           00001800
ARRAY         A[0:29] ;                                                 00001900
ARRAY         HEADER[0:29] ;                                            00002000
LIST         LP(A[I].[6:6],A[I],A[I+1].[6:6],A[I+1],SEQ) ;              00002100
LIST          LB(B[WI,0],B[WI,1].[6:6],B[WI,1],B[WI,2].[6:6],B[WI,2],   00002200
                  FOR RI ~ 3 STEP 1 UNTIL 16 DO B[WI,RI]) ;             00002300
FORMAT       FFHEAD("LEGEND FOR COLUMNS BELOW"/                         00002400
              "COL       DESCRIPTION"//                                 00002500
              "    SEQUENCE NUMBER IN PUNCHED DECK (IF ANY)"/           00002600
              "2,3  FILE IDENTIFIERS"/                                  00002700
              "4    RECORD LENGTH"/                                     00002800
              "5    BLOCK LENGTH"/                                      00002900
              "6    SAVE FACTOR"/                                       00003000
              "7    CREATION DAYE"/                                     00003100
              "8    DATE OF LAST ACCESS"/                               00003200
              "9    NUMBER OF RECORDS"/                                 00003300
              "10   DISK SEGMENTS PER ROW"/                             00003400
              "11   MAXIMUM NUMBER OF ROWS"/                            00003500
              "12   NUMBER OF WORDS ACTUALLY USED " /                   00003600
              "13   NUMBER OF WORDS ALLOCATED"/                         00003700
              "14   NUMBER OF WORDS WASTED"/                            00003800
              "15   NUMBER OF AREAS USED"/                              00003900
              "16   NUMBER OF WORDS IN AREAS USED"/                     00004000
              "17   UNUSED WORDS IN AREAS USED"//                       00004100
              " SEQ       FILE     ",                                   00004200
              "    4    5    6     7     8",X8,"9     10",              00004300
              " 11     12     13     14     15     16",                 00004400
              "     17"/) ;                                             00004500
FORMAT        FB(I4,X1,A1,A6,"/",A1,A6,3I5,2(X1,A5),I9,I7,I3,7I7) ;     00004600
FORMAT             F1("FILE ",A1,A6,"/",A1,A6,                          00004700
                      ": RECORD LENGTH=",I4,                            00004800
                      "; BLOCK LENGTH=",I4,                             00004900
                      "; SAVE FACTOR=",I3,                              00005000
                      "; CREATION DATE=",A5,";"/                        00005100
                      "  DATA OF LAST ACCESS=",A5,                      00005200
                      "; NUMBER OF RECORDS=",I8,                        00005300
                      "; DISK SEGMENTS/ROW=",I6,                        00005400
                      "; MAXIMUM NUMBER OF ROWS=",I2,";"/               00005500
                      "  NUMBER OF WORDS ACTUALLY USED=",I6,            00005600
                      "; TOTAL NUMBER OF WORDS ALLOCATED=",I6,";"/      00005700
                      "  NUMBER OF WORDS WASTED=",I6,/                  00005800
                   "NUMBER OF AREAS USED= ",I6,";",                     00005900
                   " TOTAL NUMER OF WORDS IN AREAS USED= ",I6,          00006000
                     /,"  UNUSED WORDS IN USED AREAS= ",I6,//);         00006100
FORMAT             F2(" ACCUMULATIVE NUMBER OF DISK WORDS USED= ",I8);  00006200
DEFINE             NI=IF NA[I] #,PICKUP=THEN GO TO PICK#;               00006300
FORMAT        FHEAD(A1," LOAD FROM SYSTEM -"/                           00006400
                    A1," DUMP TO SYSTEM -"/                             00006500
                    A1," REMOVE -") ,                                   00006600
              FEND("LAZY/GUY ; END .") ,                                00006700
              FP(A1,A6,"/",A1,A6,",-",X55,I8) ,                         00006800
              FL(A1,A6,"/",A1,A6,",-",I9) ;                             00006900
              DEBUG ~ TRUE ;                                            00007000
              DEBUG ~ FALSE ;                                           00007100
              WRITE(LINE[DBL],FHEAD,12,12,12) ;                         00007200
              IF PUNCHTOG THEN                                          00007300
              WRITE(PUNCH,FHEAD,12,12,12) ;                             00007400
              SEQ ~ 0 ;                                                 00007500
             FOR J ~ 15 STEP 16 WHILE TRUE DO                           00007600
         BEGIN                                                          00007700
              READ(DIRECTORY[J],30,A[*]) ;                              00007800
              FOR I ~ 28 STEP -2 UNTIL 0 DO                             00007900
        BEGIN                                                           00008000
              IF A[I] = 76 THEN GO TO EOF ;                             00008100
              IF A[I] ! 12 THEN                                         00008200
         BEGIN                                                          00008300
              IF A[I+1].[42:6] ! 12 THEN                                00008400
         BEGIN                                                          00008500
              SEQ ~ SEQ + 1 ;                                           00008600
              WRITE(LINE[DBL],FL,LP) ;                                  00008700
              IF PUNCHTOG THEN                                          00008800
              WRITE(PUNCH,FP,LP) ;                                      00008900
         END ;                                                          00009000
                   READ(DIR[J-(30-I) DIV 2],30,H[*]);                   00009100
                   IF H[0].[1:14]=0 OR H[0].[15:12]=0 THEN GO TO SKIPP; 00009200
                   L~ H[7] | H[0].[1:14];                               00009300
                   M~ ((H[0].[15:15] DIV 30)+                           00009400
                      (IF H[0].[15:15] MOD 30!0 THEN 1 ELSE 0))|        00009500
                      (((H[0].[1:14] | H[7]) DIV H[0].[15:15])+         00009600
                    (IF((H[7] | H[0].[1:14]) MOD H[0].[15:15])!0 THEN   00009700
                       1 ELSE 0)) | 30;                                 00009800
                   N~ M-L;                                              00009900
                   E~(M DIV (H[8] | 30))+(IF(M MOD (H[8]|30))!0 THEN    00010000
                   1 ELSE 0);                                           00010100
                   D~H[8] | 30 | E;                                     00010200
                   C~D-L;                                               00010300
                   F~F +D;                                              00010400
PICK:    BEGIN                                                          00010500
              IF DEBUG THEN                                             00010600
                   WRITE(LINE,F1,NA[I].[6:6],NA[I],NA[I+1].[6:6],       00010700
                         NA[I+1],H[0].[1:14],H[0].[15:15],H[3].[1:17],  00010800
                         H[3],H[4].[12:30],H[7],H[8],H[9],L,M,N,E,D,C); 00010900
              B[SEQ,0] ~ SEQ ; B[SEQ,1] ~ A[I] ; B[SEQ,2] ~ A[I+1] ;    00011000
              B[SEQ,3 ] ~ H[0].[1:14] ;   % RECORD LENGTH               00011100
              B[SEQ,4] ~ H[0].[15:15] ;   % BLOCKLENGTH                 00011200
              B[SEQ,5] ~ H[3].[1:17] ;   % SAVE FACTOR                  00011300
              B[SEQ,6] ~ H[3] ;     % CREATION DATE                     00011400
              B[SEQ,7] ~ H[4].[12:30] ;   %  DATE OF LAST ACCESS        00011500
              B[SEQ,8] ~ H[7] ;     %  NUMBER OF RECORDS                00011600
              B[SEQ,9] ~ H[8] ;   %  DISK SEGMENTS PER ROW              00011700
              B[SEQ,10] ~ H[9] ;   % MAXIMUM ROWS                       00011800
              B[SEQ,11] ~ L ;    % NUMBER OF WORDS ACTUALLY USED        00011900
              B[SEQ,12] ~ M ;    % NUMBER OF WORDS ALLOCATED            00012000
              B[SEQ,13] ~ N ;     % NUMBER OF WORDS WASTED              00012100
              B[SEQ,14] ~ E ;      % NUMBER OF AREAS USED               00012200
              B[SEQ,15] ~ D ;    % NUMBER OF WORDS IN AREAS USED        00012300
              B[SEQ,16] ~ C ;     % UNUSED WORDS IN AREAS USED          00012400
              WI ~ SEQ ;                                                00012500
              IF DEBUG THEN                                             00012600
              WRITE(LINE,FB,LB) ;                                       00012700
         END ;                                                          00012800
SKIPP:   END ;                                                          00012900
         END ;                                                          00013000
         END ;                                                          00013100
EXIT:                                                                   00013200
EOF:          WRITE(LINE[DBL],FEND) ;                                   00013300
              IF PUNCHTOG THEN                                          00013400
              WRITE(PUNCH,FEND) ;                                       00013500
              WRITE(LINE[PAGE]) ;                                       00013600
              WRITE(LINE,FFHEAD) ;                                      00013700
              FOR WI ~ 1 STEP 1 UNTIL SEQ DO                            00013800
              WRITE(LINE,FB,LB) ;                                       00013900
                   WRITE(LINE,F2,F);                                    00014000
         END .                                                          00014100
