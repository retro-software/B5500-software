 BEGIN                                                                  00000100
 COMMENT A B5500 ALGOL PROGRAM FOR PRINTING THE CONTENTS OF ANY FILE    00000200
         FROM THE DISKFILE.                                             00000300
 ********************************************************************** 00000400
                             DISCLAIMER                                 00000500
                             **********                                 00000600
         ALTHOUGH EACH PROGRAM HAS BEEN TESTED BY ITS CONTRIBUTOR,      00000700
         NO WARRANTY, EXPRESS OR IMPLIED, IS MADE BY THE CONTRIBUTOR,   00000800
         CUBE, OR THE BURROUGHS CORPORATION AS TO THE ACCURACY AND      00000900
         FUNCTIONING OF THE PROGRAM AND/OR RELATED PROGRAM MATERIAL     00001000
         AND NO RESPONSIBILITY IS ASSUMED BY THE CONTRIBUTOR, CUBE,     00001100
         OR THE BURROUGHS CORPORATION IN CONNECTION HEREWITH.           00001200
 **********************************************************************;00001300
 COMMENT THIS PROGRAM IS NAMED "DSKPRN2/DOWCHEM" PATTERNED AFTER        00001400
         "DSKPRNT/Q000004" FROM CUBE LIBRARY. IT IS WRITTEN IN ALGOL    00001500
         AND USES STREAM PROCEDURES, PARTIAL WORDS, AND CONCATENATION;  00001600
 COMMENT THIS PROGRAM IS A REVISION AND EXPANSION OF "DSKPRNT/YANDR",   00001700
         WHICH WAS OBTAINED FROM THE CUBE LIBRARY.                      00001800
         IT WAS WRITTEN BY JOE FRISCH OF YOUNG & RUBICAM, NEW YORK.     00001900
         THE REVISION WAS DONE BY V. E. WHITTIER,                       00002000
         COMPUTATION RESEARCH LAB., 1707 BLD., DOW CHEMICAL CO,         00002100
         MIDLAND, MICHIGAN  48640 ;                                     00002200
 COMMENT DATE OF THIS VERSION IS JULY  8, 1969;                         00002300
 COMMENT THIS PROGRAM PRINTS THE CONTENTS OF ANY DISK FILE ONTO         00002400
         A 120 COLUMN LINE PRINTER.                                     00002500
         THE PRINTOUT MAY BE EITHER ALPHA (15 WORDS PER LINE)           00002600
         OR OCTAL (6 WORDS PER LINE).                                   00002700
         EACH RECORD WILL START A LINE, AND, IF PRINTED IN ALPHA,       00002800
         RECORDS OF GREATER THAN 15 WORDS WILL REQUIRE MORE THAN        00002900
         ONE LINE --- IF PRINTED IN OCTAL, RECORDS OF GREATER           00003000
         THAN 6 WORDS WILL REQUIRE MORE THAN ONE LINE;                  00003100
 COMMENT THE TECHNIQUE FOR THE OCTAL PRINTOUT IS TAKEN FROM BURROUGHS   00003200
         PROGRAM "DUMP/ANALYZE";                                        00003300
 COMMENT THE USER COMMUNICATES WITH THE PROGRAM ONLY BY MEANS OF        00003400
         A LABEL EQUATION CARD AND A "COMMON" CARD;                     00003500
 COMMENT THE INPUT FILE MUST BE LABEL-EQUATED TO THE NAME "INPUT" AS    00003600
         A "SERIAL" FILE,  E.G.  CC FILE INPUT = A/B SERIAL;            00003700
 COMMENT RECORD AND BLOCK SIZES ARE DETERMINED PROGRAMMATICALLY,        00003800
         AND ARE PRINTED OUT AT THE BEGINNING OF THE LISTING.           00003900
         IN ADDITION, THE FILE NAME, AND NUMBER OF RECORDS IT CONTAINS  00004000
         ARE ALSO PRINTED AT THE BEGINNING.                             00004100
         APPROPRIATE ERROR MESSAGES ARE PRINTED FOR NON-EXISTENT        00004200
         OR NON-ACCESSABLE (BECAUSE OF FILE SECURITY) FILES;            00004300
 COMMENT SEVERAL OPTIONS ARE AVAILABLE BY USE OF THE "COMMON" CARD.     00004400
         THE COMMON CARD MAY CONTAIN AN INTEGER UP TO 8 DIGITS LONG.    00004500
         FOR THIS PROGRAM THE COMMON CARD SHOULD BE OF THIS FORM:       00004600
         COMMON = SSSSSRRW                                              00004700
         WHERE SSSSS = NUMBER OF RECORDS TO BE SKIPPED AT THE START     00004800
                       OF THE FILE. IF YOU WISH TO PRINT THE ENTIRE     00004900
                       FILE, THIS NUMBER WILL BE ZERO. IF RECORDS ARE   00005000
                       SKIPPED, NOTIFICATION IS PRINTED.                00005100
                  RR = NUMBER OF RECORDS TO BE PRINTED BETWEEN LINES    00005200
                       INDICATING RECORD COUNT. THE PERIODIC PRINTING   00005300
                       OF THE RECORD COUNT CAN HELP YOU TO LOCATE A     00005400
                       PARTICULAR RECORD.                               00005500
                       IF RR = 0 THEN A VALUE OF 20 IS USED.            00005600
                       THE RECORD COUNT IS PRINTED IN BOLD FACE.        00005700
                   W = AN INDICATION OF HOW TO PRINT THE FILE:          00005800
                       IF W = 0 PRINT IT AS ALPHA (15 WORDS PER LINE).  00005900
                       IF W ! 0 PRINT IT AS OCTAL (6 WORDS PER LINE).   00006000
         IF THERE IS NO COMMON CARD (OR IF COMMON = 0) THEN THE         00006100
         ENTIRE FILE WILL BE PRINTED AS ALPHA WITH A RECORD COUNT       00006200
         FOLLOWING EVERY 20TH RECORD;                                   00006300
 COMMENT TO LABEL-EQUATE THE PRINTER USE THE FILE NAME "PRINT";         00006400
 INTEGER COMMON, SEPARATOR, SKIP;                                       00006500
 BOOLEAN PRINTOCT;                                                      00006600
 LABEL WRITEALF, WRITEOCT, FINISH;                                      00006700
 INTEGER N1, N2;                                                        00006800
 INTEGER REMAINDER, CTR3, NUMBERWORDS, TOTALLRCOUNTER, LINECTR, J, M;   00006900
 FILE OUT PRINT 17 "LPA4840" (2,15);                                    00007000
 ARRAY A [0 : 6];                                                       00007100
 FORMAT FFILE(A1,A6,"/",A1,A6),                                         00007200
        FHEAD("WORDS PER RECORD = ",I4,                                 00007300
              ".  WORDS PER BLOCK = ",I4,                               00007400
              ".  TOTAL RECORDS = ",I8,"."),                            00007500
        FNO("IS NOT ON DISK."),                                         00007600
        FPRIV("IS PRIVATE, AND YOU MAY NOT ACCESS IT."),                00007700
        FCLOB("ERROR IN DISK DIRECTORY.  SHOWS 0 WORDS PER RECORD."),   00007800
        EOJ(///X10,"TOTAL LOGICAL RECORDS ",I6,X10,"END OF JOB."),      00007900
        PRINTFORMAT(X93,"END OF RECORD",I11,"."),                       00008000
        WONLINE(6(O," ",O,"   ")),                                      00008100
        F("NUMBER OF RECORDS SKIPPED IS",I5);                           00008200
 PROCEDURE WRITESEPARATOR;                                              00008300
 IF TOTALLRCOUNTER MOD SEPARATOR = 0 THEN                               00008400
 BEGIN WRITE (PRINT [NO], PRINTFORMAT, TOTALLRCOUNTER);                 00008500
    WRITE (PRINT [NO], PRINTFORMAT, TOTALLRCOUNTER);                    00008600
    WRITE (PRINT, PRINTFORMAT, TOTALLRCOUNTER);                         00008700
 END WRITESEPARATOR;                                                    00008800
 COMMENT HERE BEGINS THE FIRST OF 3 DISJOINT BLOCKS.                    00008900
         EACH STARTS WITH A DECLARATION OF THE DISK FILE "INPUT".       00009000
         IN THE FIRST BLOCK THIS IS SOMEWHAT OF A DUMMY DECLARATION     00009100
         SO THAT THE FILE WHICH THE USER WISHES TO PRINT CAN BE         00009200
         LABEL-EQUATED TO THIS FILE. THEN A "SEARCH" STATEMENT CAN      00009300
         DETERMINE THE PARAMETERS OF THE ACTUAL FILE, AND THESE         00009400
         PARAMETERS CAN BE USED IN THE SECOND OR THIRD BLOCK;           00009500
 BEGIN FILE IN INPUT DISK SERIAL (1, 1);                                00009600
    SEARCH (INPUT, A [*]);                                              00009700
    SKIP ~ COMMON DIV 1000;                                             00009800
    COMMENT "SKIP" CONTAINS THE NUMBER OF RECORDS TO BE SKIPPED;        00009900
    COMMON ~ COMMON MOD 1000;                                           00010000
    SEPARATOR ~ COMMON DIV 10;                                          00010100
    IF SEPARATOR = 0 THEN SEPARATOR ~ 20;                               00010200
    COMMENT "SEPARATOR" CONTAINS THE NUMBER OF RECORDS TO BE PRINTED    00010300
            BETWEEN PRINTOUTS OF RECORD COUNTS;                         00010400
    IF COMMON MOD 10 = 0 THEN ELSE PRINTOCT ~ TRUE;                     00010500
    COMMENT CHOOSE BETWEEN ALPHA AND OCTAL PRINTOUT;                    00010600
    TOTALLRCOUNTER ~ SKIP;                                              00010700
    WRITE (PRINT, FFILE, A [1] . [6 : 6], A [1], A [2] . [6 : 6], A [2  00010800
    ]);                                                                 00010900
    IF A [0] = - 1 THEN                                                 00011000
    COMMENT IF FILE IS NOT PRESENT, PRINT MESSAGE AND TERMINATE;        00011100
    BEGIN WRITE (PRINT, FNO);                                           00011200
       GO TO FINISH                                                     00011300
    END;                                                                00011400
    IF A [0] = 0 THEN                                                   00011500
    COMMENT IF FILE IS PRIVATE, PRINT MESSAGE AND TERMINATE;            00011600
    BEGIN WRITE (PRINT, FPRIV);                                         00011700
       GO TO FINISH                                                     00011800
    END;                                                                00011900
    N1 ~ A [3];                                                         00012000
    N2 ~ A [4];                                                         00012100
    WRITE (PRINT [DBL], FHEAD, N1, N2, 1 + A [5]);                      00012200
    IF N1 = 0 THEN                                                      00012300
    COMMENT IF ZERO WORDS PER RECORD, INDICATE ERROR AND TERMINATE;     00012400
    BEGIN WRITE (PRINT [DBL], FCLOB);                                   00012500
       GO TO FINISH;                                                    00012600
    END;                                                                00012700
    IF PRINTOCT THEN GO TO WRITEOCT;                                    00012800
 END;                                                                   00012900
 COMMENT HERE BEGINS THE SECOND OF 3 DISJOINT BLOCKS.                   00013000
         THIS ONE CONSTRUCTS A PROPER INPUT FILE DECLARATION            00013100
         AND PRINTS THE CONTENTS OF THE FILE IN ALPHA MODE;             00013200
 WRITEALF:                                                              00013300
 BEGIN FILE IN INPUT DISK SERIAL ((IF N2 > 100 THEN 1 ELSE 2), N1, (IF  00013400
    N2 ! N1 THEN N2 ELSE 0));                                           00013500
    ARRAY WRITEOUTAREA [0 : 14], READINAREAWORD [0 : N1];               00013600
    STREAM PROCEDURE CLEAR15 (A);                                       00013700
    BEGIN DI ~ A;                                                       00013800
       DS ~ 8 LIT "        ";                                           00013900
       SI ~ A;                                                          00014000
       DS ~ 14 WDS                                                      00014100
    END;                                                                00014200
    LABEL EOF, MORETHENONELINE;                                         00014300
    STREAM PROCEDURE MOVEWORDS (A, B, C);                               00014400
    VALUE A;                                                            00014500
    BEGIN SI ~ B;                                                       00014600
       DI ~ C;                                                          00014700
       DS ~ A WDS;                                                      00014800
    END;                                                                00014900
    COMMENT SPECIFY THE DESIRED FILE;                                   00015000
    FILL INPUT WITH A [1], A [2];                                       00015100
    CTR3 ~ 0;                                                           00015200
    IF SKIP ! 0 THEN                                                    00015300
    COMMENT SKIP SOME RECORDS, IF SO INDICATED;                         00015400
    BEGIN SPACE (INPUT, SKIP);                                          00015500
       WRITE (PRINT [DBL], F, SKIP);                                    00015600
    END;                                                                00015700
    COMMENT PREPARE TO PRINT 15 WORDS PER LINE;                         00015800
    IF N1 { 15 THEN                                                     00015900
    BEGIN CTR3 ~ 1;                                                     00016000
       REMAINDER ~ N1                                                   00016100
    END ELSE                                                            00016200
    BEGIN CTR3 ~ N1 DIV 15;                                             00016300
       REMAINDER ~ N1 MOD 15;                                           00016400
       IF REMAINDER ! 0 THEN CTR3 ~ CTR3 + 1                            00016500
    END;                                                                00016600
    J ~ 0;                                                              00016700
    CLEAR15 (WRITEOUTAREA [0]);                                         00016800
    IF CTR3 ! 1 THEN GO TO MORETHENONELINE;                             00016900
    COMMENT ROUTINE TO PRINT LOGICAL RECORDS OF 15 OR FEWER WORDS;      00017000
    BEGIN LABEL ONELINE, CONTIN1;                                       00017100
       ONELINE: NUMBERWORDS ~ REMAINDER;                                00017200
       CONTIN1: READ (INPUT, N1, READINAREAWORD [*]) [EOF];             00017300
       MOVEWORDS (NUMBERWORDS, READINAREAWORD [0], WRITEOUTAREA [0]);   00017400
       WRITE (PRINT, NUMBERWORDS, WRITEOUTAREA [*]);                    00017500
       TOTALLRCOUNTER ~ TOTALLRCOUNTER + 1;                             00017600
       WRITESEPARATOR;                                                  00017700
       GO TO CONTIN1                                                    00017800
    END;                                                                00017900
    COMMENT ROUTINE TO PRINT LOGICAL RECORDS OF MORE THAN 15 WORDS;     00018000
    MORETHENONELINE:                                                    00018100
    BEGIN LABEL AGAIN, CONTIN2, CONTIN3;                                00018200
       AGAIN: NUMBERWORDS ~ 15;                                         00018300
       LINECTR ~ 1;                                                     00018400
       CONTIN2: READ (INPUT, N1, READINAREAWORD [*]) [EOF];             00018500
       CONTIN3: MOVEWORDS (NUMBERWORDS, READINAREAWORD [J],             00018600
       WRITEOUTAREA [0]);                                               00018700
       WRITE (PRINT, 15, WRITEOUTAREA [*]);                             00018800
       J ~ J + 15;                                                      00018900
       LINECTR ~ LINECTR + 1;                                           00019000
       IF LINECTR < CTR3 THEN GO TO CONTIN3 ELSE                        00019100
       BEGIN LINECTR ~ 1;                                               00019200
          CLEAR15 (WRITEOUTAREA);                                       00019300
          IF REMAINDER = 0 THEN NUMBERWORDS ~ 15 ELSE NUMBERWORDS ~     00019400
          REMAINDER;                                                    00019500
          MOVEWORDS (NUMBERWORDS, READINAREAWORD [J], WRITEOUTAREA [0]  00019600
          );                                                            00019700
          WRITE (PRINT, 15, WRITEOUTAREA [*]);                          00019800
          TOTALLRCOUNTER ~ TOTALLRCOUNTER + 1;                          00019900
          J ~ 0;                                                        00020000
       END;                                                             00020100
       WRITESEPARATOR;                                                  00020200
       GO TO AGAIN;                                                     00020300
    END;                                                                00020400
    EOF: WRITE (PRINT, EOJ, TOTALLRCOUNTER);                            00020500
    GO TO FINISH;                                                       00020600
 END;                                                                   00020700
 COMMENT HERE BEGINS THE THIRD OF 3 DISJOINT BLOCKS.                    00020800
         THIS ONE CONSTRUCTS A PROPER INPUT FILE DECLARATION            00020900
         AND PRINTS THE CONTENTS OF THE FILE IN OCTAL MODE;             00021000
 WRITEOCT:                                                              00021100
 BEGIN FILE IN INPUT DISK SERIAL ((IF N2 > 100 THEN 1 ELSE 2), N1, (IF  00021200
    N2 ! N1 THEN N2 ELSE 0));                                           00021300
    ARRAY READINAREAWORD [0 : N1];                                      00021400
    LABEL EOF, MORETHENONELINE;                                         00021500
    REAL PROCEDURE OCTAL (N);                                           00021600
    VALUE N;                                                            00021700
    INTEGER N;                                                          00021800
    COMMENT N.[0:23] = 0 SO THAT IF N CONTAINS AT MOST A HALF-WORD THEN 00021900
            OCTAL IF PRINTED USING O FORMAT, OR A FORMAT FOR FEWER      00022000
            OCTADES WILL BE THE OCTAL REPRESENTATION OF N;              00022100
    OCTAL ~ N . [45 : 3] & (IF N > 7 THEN OCTAL (N . [24 : 21]) ELSE 0  00022200
    ) [3 : 9 : 39];                                                     00022300
    REAL STREAM PROCEDURE CHRS (AT, SKIPPING, MANY);                    00022400
    VALUE SKIPPING, MANY;                                               00022500
    COMMENT RETURNS 7 OR LESS CHARACTERS, AS REQUIRED;                  00022600
    BEGIN SI ~ AT;                                                      00022700
       SI ~ SI + SKIPPING;                                              00022800
       DI ~ LOC CHRS;                                                   00022900
       DS ~ 8 LIT "0";                                                  00023000
       DI ~ DI - MANY;                                                  00023100
       DS ~ MANY CHR;                                                   00023200
    END CHRS;                                                           00023300
    INTEGER PROCEDURE HIHALF (K);                                       00023400
    VALUE K;                                                            00023500
    INTEGER K;                                                          00023600
    HIHALF ~ CHRS (READINAREAWORD [K], 0, 4);                           00023700
    INTEGER PROCEDURE LOHALF (K);                                       00023800
    VALUE K;                                                            00023900
    INTEGER K;                                                          00024000
    LOHALF ~ CHRS (READINAREAWORD [K], 4, 4);                           00024100
    COMMENT SPECIFY THE DESIRED FILE;                                   00024200
    FILL INPUT WITH A [1], A [2];                                       00024300
    CTR3 ~ 0;                                                           00024400
    IF SKIP ! 0 THEN                                                    00024500
    COMMENT SKIP SOME RECORDS, IF SO INDICATED;                         00024600
    BEGIN SPACE (INPUT, SKIP);                                          00024700
       WRITE (PRINT [DBL], F, SKIP);                                    00024800
    END;                                                                00024900
    COMMENT PREPARE TO PRINT 6 WORDS PER LINE;                          00025000
    IF N1 { 6 THEN                                                      00025100
    BEGIN CTR3 ~ 1;                                                     00025200
       REMAINDER ~ N1                                                   00025300
    END ELSE                                                            00025400
    BEGIN CTR3 ~ N1 DIV 6;                                              00025500
       REMAINDER ~ N1 MOD 6;                                            00025600
       IF REMAINDER ! 0 THEN CTR3 ~ CTR3 + 1;                           00025700
    END;                                                                00025800
    J ~ 0;                                                              00025900
    IF CTR3 ! 1 THEN GO TO MORETHENONELINE;                             00026000
    COMMENT ROUTINE TO PRINT LOGICAL RECORDS OF 6 OR FEWER WORDS;       00026100
    BEGIN LABEL ONELINE, CONTIN1;                                       00026200
       ONELINE: NUMBERWORDS ~ REMAINDER - 1;                            00026300
       CONTIN1: READ (INPUT, N1, READINAREAWORD [*]) [EOF];             00026400
       WRITE (PRINT, WONLINE, FOR M ~ 0 STEP 1 UNTIL NUMBERWORDS DO [   00026500
       OCTAL (HIHALF (M)), OCTAL (LOHALF (M))]);                        00026600
       TOTALLRCOUNTER ~ TOTALLRCOUNTER + 1;                             00026700
       WRITESEPARATOR;                                                  00026800
       GO TO CONTIN1;                                                   00026900
    END;                                                                00027000
    COMMENT ROUTINE TO PRINT LOGICAL RECORDS OF MORE THAN 6 WORDS;      00027100
    MORETHENONELINE:                                                    00027200
    BEGIN LABEL AGAIN, CONTIN2, CONTIN3;                                00027300
       AGAIN: NUMBERWORDS ~ 6;                                          00027400
       LINECTR ~ 1;                                                     00027500
       CONTIN2: READ (INPUT, N1, READINAREAWORD [*]) [EOF];             00027600
       CONTIN3: WRITE (PRINT, WONLINE, FOR M ~ J STEP 1 UNTIL J + 5 DO  00027700
       [OCTAL (HIHALF (M)), OCTAL (LOHALF (M))]);                       00027800
       J ~ J + 6;                                                       00027900
       LINECTR ~ LINECTR + 1;                                           00028000
       IF LINECTR < CTR3 THEN GO TO CONTIN3 ELSE                        00028100
       BEGIN LINECTR ~ 1;                                               00028200
          IF REMAINDER = 0 THEN NUMBERWORDS ~ 5 ELSE NUMBERWORDS ~      00028300
          REMAINDER - 1;                                                00028400
          WRITE (PRINT, WONLINE, FOR M ~ J STEP 1 UNTIL J +             00028500
          NUMBERWORDS DO [OCTAL (HIHALF (M)), OCTAL (LOHALF (M))]);     00028600
          TOTALLRCOUNTER ~ TOTALLRCOUNTER + 1;                          00028700
          J ~ 0;                                                        00028800
       END;                                                             00028900
       WRITESEPARATOR;                                                  00029000
       GO TO AGAIN;                                                     00029100
    END;                                                                00029200
    EOF: WRITE (PRINT, EOJ, TOTALLRCOUNTER);                            00029300
    GO TO FINISH;                                                       00029400
 END;                                                                   00029500
 FINISH:                                                                00029600
 END .                                                                  00029700
