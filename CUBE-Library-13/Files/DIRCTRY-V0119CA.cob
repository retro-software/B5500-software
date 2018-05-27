000100DIRECTORY   000119CA                                                    00
000200                                                                        00
000300************************************************************************00
000400          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00
000500     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00
000600************************************************************************00
000700                                                                        00
000800                                                                        00
001000 *****       LINE 00915 SHOULD CONTAIN THE HIGHEST ADDRESS WHICH  DIRECT00
001100        OVERLAY DISK MAY OCCUPY AS STATED IN THE "BACKUP" CARD    DIRECT00
001200        OF THE COLD START DECK.                                   DIRECT00
001300                                                                  DIRECT00
001400 *****       LINES 01003 THRU 01022 SHOULD SHOW THE NUMBER OF     DIRECT00
001500        SEGMENTS OF DISK INSTALLED ON EACH OF THE EU-S ATTACHED   DIRECT00
001600      TO THE SYSTEM IF AVAILABLE DISK IS TO BE CORRECTLY STATED.  DIRECT00
001700                                                                  DIRECT00
001800 *****       THE COLD START DECK MUST CONTAIN A FILE CARD GROUP   DIRECT00
001900        FOR THE FILE   DIRCTRY/DISK.                              DIRECT00
002000                                                                  DIRECT00
002100 *****       DISKTOG MUST BE OFF WHEN THIS PROGRAM IS EXECUTING   DIRECT00
002200        ITS INPUT PHASE.                                          DIRECT00
002300                                                                  DIRECT00
002400 *****       MAX-LINES (SEE LINE 00712) SPECIFIES THE MAXIMUM     DIRECT00
002500        NUMBER OF LINES PER PAGE.                                 DIRECT00
002600                                                                  DIRECT00
002700 IDENTIFICATION DIVISION.                                         DIRECT00
002800 PROGRAM-ID.         LIST DIRECTORY.                              DIRECT00
002900 AUTHOR.             C CODDINGTON    BURROUGHS OAKLAND, CALIF.    DIRECT00
003000 DATE-WRITTEN.       MARCH 1968.                                  DIRECT00
003100 DATE-COMPILED.                                                   DIRECT00
003200 REMARKS.            CREATES FROM THE DISK DIRECTORY A FILE       DIRECT00
003300             WHICH CONTAINS ALPHA KEY RECORDS WITH A CODE         DIRECT00
003400             OF 1 AND ADDRESS KEY RECORDS WITH A CODE OF 2.       DIRECT00
003500                     SORTS THE ABOVE RECORDS.                     DIRECT00
003600                     LISTS DISK USAGE BY IDENTIFICATION.          DIRECT00
003700                     LISTS DISK USAGE AND AVAILABLE DISK          DIRECT00
003800             AREAS BY ADDRESS.                                    DIRECT00
003900                                                                  DIRECT00
004000 ENVIRONMENT DIVISION.                                            DIRECT00
004100 CONFIGURATION SECTION.                                           DIRECT00
004200 SOURCE-COMPUTER.    B-5500.                                      DIRECT00
004300 OBJECT-COMPUTER.    B-5500  MEMORY SIZE 6800 WORDS.              DIRECT00
004400 INPUT-OUTPUT SECTION.                                            DIRECT00
004500 FILE-CONTROL.       SELECT SORTER   ASSIGN TO SORT DISK.         DIRECT00
004600                     SELECT DIRECTORY ASSIGN TO DISK.             DIRECT00
004700                     SELECT LISTING ASSIGN TO PRINTER DISK.       DIRECT00
004800 I-O-CONTROL.        MULTIPLE FILE CONTAINS DIRECTORY             DIRECT00
004900                     VALUE OF MFID IS "DIRCTRY".                  DIRECT00
005000                                                                  DIRECT00
005100 DATA DIVISION.                                                   DIRECT00
005200 FILE SECTION.                                                    DIRECT00
005300 SD  SORTER          DATA RECORD SORT-REC.                        DIRECT00
005400 01  SORT-REC                        SZ 72.                       DIRECT00
005500     05              SORT-KEY.                                    DIRECT00
005600           10        TYPE                    PC 9.                DIRECT00
005700           10        PREFIX                  PC X(7).             DIRECT00
005800           10        SUFFIX                  PC X(7).             DIRECT00
005900           10        ROW-NUM                 PC 99.               DIRECT00
006000           10        MAX-ROWS                PC 99.               DIRECT00
006100           10        RL                      PC 9(4).             DIRECT00
006200           10        FILLER                  SZ 1.                DIRECT00
006300     05              BFACTOR                 PC 9(4).             DIRECT00
006400     05              PURGE-FACTOR            PC 999.              DIRECT00
006500     05              CREATIONXDATE           PC 9(5).             DIRECT00
006600     05              LAST-ACCESS-DATE        PC 9(5).             DIRECT00
006700     05              NO-OF-RECORDS           PC 9(9).             DIRECT00
006800     05              SEGMENTS-IN-ROW         PC 9(7).             DIRECT00
006900     05              ROW-ADDRESS             PC 9(7).             DIRECT00
007000     05              PREFIX-IN-ROW-ADDRESS                        DIRECT00
007100             REDEFINES ROW-ADDRESS           PC X(7).             DIRECT00
007200     05              USER-NUMBER.                                 DIRECT00
007300           10        FILLER                  SZ 1.                DIRECT00
007400           10        USERS-NO                PC X(7).             DIRECT00
007500                                                                  DIRECT00
007600 MD  DIRECTORY                                                    DIRECT00
007700                     FILE CONTAINS 1410 RECORDS                   DIRECT00
007800                     ACCESS MODE SEQUENTIAL                       DIRECT00
007900                     VALUE OF ID "DISK"                           DIRECT00
008000                     DATA RECORD DIRECTORY-RECORD.                DIRECT00
008100 01  DIRECTORY-RECORD                SZ 3840.                     DIRECT00
008200     05              FILE-RECORD     OC 15   SZ  240.             DIRECT00
008300           10        WORD-ZERO       CMP     PC 9(8).             DIRECT00
008400           10        WORD-1           CMP    PC 9(8).             DIRECT00
008500           10        WORD-2                  PC X(8).             DIRECT00
008600           10        WORD-3 (SAVE-CREAT).                         DIRECT00
008700                  15 PURGE-FACTOR            PC XXX.              DIRECT00
008800                  15 CREATIONXDATE           PC 9(5).             DIRECT00
008900           10        CMP-3 REDEFINES WORD-3  PC 9(8) CMP.         7-5-6800
009000           10        WORD-4.                                      DIRECT00
009100                  15 FILLER                  SZ 2.                DIRECT00
009200                  15 LAST-ACCESS-DATE        PC 9(5).             DIRECT00
009300                  15 FILLER                  SZ 1.                DIRECT00
009400           10        CMP-4 REDEFINES WORD-4  PC 9(8) CMP.         7-5-6800
009500           10        WORD-5                  PC 9(8).             DIRECT00
009600           10        WORD-6                  PC 9(8).             DIRECT00
009700           10        NO-OF-RECORDS (WORD-7)  PC 9(8) CMP.         DIRECT00
009800           10        SEGMENTS-IN-ROW (WORD-8) PC 9(7) CMP.        DIRECT00
009900           10        WORD-9 (ACT-ROW)        PC 9(8) CMP.         DIRECT00
010000           10        ROW-ADDRESS     OC 20   PC 9(8) CMP.         DIRECT00
010100     05              FILE-INDEX      OC 15.                       DIRECT00
010200           10        FILLER                  SZ 1.                DIRECT00
010300           10        PREFIX                  PC X(7).             DIRECT00
010400           10        FILLER                  SZ 1.                DIRECT00
010500           10        SUFFIX                  PC 9(7).             DIRECT00
010600                                                                  DIRECT00
010700 FD  LISTING                                                      DIRECT00
010800                     VALUE OF ID "DIRECTY"                        DIRECT00
010900                     DATA RECORD DIRECTORY-LISTING-RECORD.        DIRECT00
011000 01  DIRECTORY-LISTING-RECORD                SZ 132.              DIRECT00
011100     05              FILLER                  SZ 1.                DIRECT00
011200     05              PREFIX                  PC X(7).             DIRECT00
011300     05              B-SLASH-B               PC X(3).             DIRECT00
011400     05              SUFFIX                  PC X(7).             DIRECT00
011500     05              FILLER                  SZ 6.                DIRECT00
011600     05              RL                      PC Z(4).             DIRECT00
011700     05              FILLER                  SZ 7.                DIRECT00
011800     05              BFACTOR                 PC Z(4).             DIRECT00
011900     05              FILLER                  SZ 5.                DIRECT00
012000     05              PURGE-FACTOR            PC ZZZ.              DIRECT00
012100     05              FILLER                  SZ 6.                DIRECT00
012200     05              CREATIONXDATE           PC 9(5).             DIRECT00
012300     05              FILLER                  SZ 4.                DIRECT00
012400     05              LAST-ACCESS-DATE        PC 9(5).             DIRECT00
012500     05              NO-OF-RECORDS        PC Z(5),ZZZ,ZZZ.        DIRECT00
012600     05              FILLER                  SZ 9.                DIRECT00
012700     05              MAX-ROWS                PC Z9.               DIRECT00
012800     05              SEGMENTS-IN-ROW         PC ZZZ,ZZZ,ZZZ.      DIRECT00
012900     05              FILLER                  SZ 6.                DIRECT00
013000     05              ROW-NUM                 PC 99.               DIRECT00
013100     05              FILLER                  SZ 2.                DIRECT00
013200     05              ROW-ADDRESS             PC 9B9(6).           DIRECT00
013300     05              FILLER                  SZ 5.                DIRECT00
013400     05              PRINT-USER              SZ 7.                DIRECT00
013500                                                                  DIRECT00
013600 WORKING-STORAGE SECTION.                                         DIRECT00
013700 77          DE-BLOCK                CMP-1   PC 9(8).             DIRECT00
013800 77          REC-MAKE                CMP-1   PC 9(8).             DIRECT00
013900 77          MAX-LINES       VA 50   CMP-1   PC 9(8).             DIRECT00
014000 77          LINE-COUNT              CMP-1   PC 9(8).             DIRECT00
014100 77          PAGE-COUNT              CMP-1   PC 9(3).             DIRECT00
014200 77          STOP-DIRECTORY          CMP-1   PC 9(8).             DIRECT00
014300 77          TIME-HOLD               CMP-1   PC 9(8).             DIRECT00
014400 77          WORD-0                  CMP-1  PC 9(8).              DIRECT00
014500 77          NUMBER-OF-AVAILABLE-AREAS       PC 9999 CMP-1.       DIRECT00
014600 77          TOTAL-AVAILABLE-DISK    CMP-1  PC 9(11).             DIRECT00
014700 77          AVAIL-SEGS              CMP-1   PC 9(9).             DIRECT00
014800 77          TOTAL-NO-OF-FILES       CMP-1   PC 9999.             DIRECT00
014900 77          TEST-WORD-3             CMP-1   PC 9(8).             7-5-6800
015000 77          TEST-WORD-4             CMP-1   PC 9(8).             7-5-6800
015100 77          FORMAT-TEST     VA 0    CMP-1   PC 9(8).             7-5-6800
015200     88      NEW-FORMAT      VA 2.                                7-5-6800
015300 77          CONVERT-CMP             CMP-1   PC 9(8).             7-5-6800
015400                                                                  DIRECT00
015500 01  FIRST-HEADER                    SZ 132.                      DIRECT00
015600     05              REPORT-TITLE            SZ 75                DIRECT00
015700             VA "DISK DIRECTORY BY FILE IDENTIFICATIONS".         DIRECT00
015800     05              FILLER VA "TIME"        SZ 5.                DIRECT00
015900     05              HEAD-HOUR               PC 99.               DIRECT00
016000     05              FILLER          VA ":"  SZ 1.                DIRECT00
016100     05              HEAD-MINUTE             PC 99.               DIRECT00
016200     05              FILLER VA "   DATE"     SZ 8.                DIRECT00
016300     05              FILL-DATE               PC 99/99/99.         DIRECT00
016400     05              FILLER VA "      PAGE NO."      SZ 15.       DIRECT00
016500     05              HEAD-PAGE-NO            PC ZZZ.              DIRECT00
016600     05              FILLER VA SPACES        SZ 13.               DIRECT00
016700                                                                  DIRECT00
016800 01  SECOND-HEADER                   SZ 132.                      DIRECT00
016900     05              FILLER          SZ 128          VA           DIRECT00
017000 "  IDENTIFICATION      RECORD    RECORDS    SAVE   CREATION     LDIRECT00
017100-"AST    NUMBER OF  # OF ROWS   SEGMENTS     ROW   ADDRESS".      DIRECT00
017200     05              FILLER VA "USER" SZ 4.                       DIRECT00
017300                                                                  DIRECT00
017400 01  THIRD-HEADER                    SZ 132.                      DIRECT00
017500     05              FILLER          SZ 132          VA           DIRECT00
017600 " PREFIX    SUFFIX     LENGTH  PER BLOCK  FACTOR       DATE   ACCDIRECT00
017700-"ESS      RECORDS   DECLARED    PER ROW  NUMBER    OF ROW".      DIRECT00
017800                                                                  DIRECT00
017900 01  HOLD64                          SZ 64.                       DIRECT00
018000 01  OTHER-AREAS.                                                 DIRECT00
018100     05    TEST-14.                                               DIRECT00
018200          10    FILLER                       SZ 1.                DIRECT00
018300          10    TEST-OCTAL-14                PC X(7).             DIRECT00
018400     05    CMP-OCTAL-14 REDEFINES TEST-14    PC 9(8)     CMP.     DIRECT00
018500     05    TEST-ADDRESS.                                          DIRECT00
018600          10    T-EU-NO                      PC 99.               DIRECT00
018700          10    T-ADDRS                      PC 9(6).             DIRECT00
018800     05    TEST-1 REDEFINES TEST-ADDRESS     PC 9(8).             DIRECT00
018900     05    PREV-ADDRESS.                                          DIRECT00
019000          10    P-EU-NO      VA 00           PC 99.               DIRECT00
019100          10    P-ADDRS      VA 25000        PC 9(6).             DIRECT00
019200     05    PREV-1 REDEFINES PREV-ADDRESS     PC 9(8).             DIRECT00
019300     05              PURGE-DSPLY.                                 DIRECT00
019400           10        FILLER  VA 0            PC 9(5).             DIRECT00
019500           10        PURGE-HOLD              PC XXX.              DIRECT00
019600     05              PURGE-CMP REDEFINES PURGE-DSPLY              DIRECT00
019700                                         CMP PC 9(8).             DIRECT00
019800                                                                  DIRECT00
019900 01  TOTAL-LINE                      SZ 132.                      DIRECT00
020000     05      FILLER  VA "THERE ARE"          SZ 10.               DIRECT00
020100     05      TOTAL-AREAS                     PC ZZ,ZZZ.           DIRECT00
020200     05      FILLER  VA " UNUSED AREAS OF DISK WHICH CONTAIN A TOTDIRECT00
020300*    "AL OF "                                SZ 47.               DIRECT00
020400     05      AVAILABLE-SEGMENTS              PC ZZ,ZZZ,ZZZ,ZZZ.   DIRECT00
020500     05      FILLER VA " SEGMENTS OF AVAILABLE DISK."  SZ 55.     DIRECT00
020600                                                                  DIRECT00
020700 01  MAXIMUM-DISK-PER-EU.                                         DIRECT00
020800     05              MAX-1.                                       DIRECT00
020900           10        EU1     VA  80000   CMP PC 9(8).             DIRECT00
021000           10        EU2     VA 400000   CMP PC 9(8).             DIRECT00
021100           10        EU3     VA 400000   CMP PC 9(8).             DIRECT00
021200           10        EU4                 CMP PC 9(8).             DIRECT00
021300           10        EU5                 CMP PC 9(8).             DIRECT00
021400           10        EU6                 CMP PC 9(8).             DIRECT00
021500           10        EU7                 CMP PC 9(8).             DIRECT00
021600           10        EU8                 CMP PC 9(8).             DIRECT00
021700           10        EU9                 CMP PC 9(8).             DIRECT00
021800           10        EU10                CMP PC 9(8).             DIRECT00
021900           10        EU11                CMP PC 9(8).             DIRECT00
022000           10        EU12                CMP PC 9(8).             DIRECT00
022100           10        EU13                CMP PC 9(8).             DIRECT00
022200           10        EU14                CMP PC 9(8).             DIRECT00
022300           10        EU15                CMP PC 9(8).             DIRECT00
022400           10        EU16                CMP PC 9(8).             DIRECT00
022500           10        EU17                CMP PC 9(8).             DIRECT00
022600           10        EU18                CMP PC 9(8).             DIRECT00
022700           10        EU19                CMP PC 9(8).             DIRECT00
022800           10        EU20                CMP PC 9(8).             DIRECT00
022900     05              MAXIMUM-ADDRESS                              DIRECT00
023000       REDEFINES MAX-1       OC 20       CMP PC 9(8).             DIRECT00
023100                                                                  DIRECT00
023200 PROCEDURE DIVISION.                                              DIRECT00
023300 SORT-PARAGRAPH.                                                  DIRECT00
023400             SORT SORTER ON ASCENDING SORT-KEY                    DIRECT00
023500         INPUT PROCEDURE IS INPUTTER                              DIRECT00
023600         OUTPUT PROCEDURE IS OUTPUTTER.                           DIRECT00
023700                                                                  DIRECT00
023800 TURN-IT-OFF.                                                     DIRECT00
023900             STOP RUN.                                            DIRECT00
024000                                                                  DIRECT00
024100 INPUTTER SECTION.                                                DIRECT00
024200 IN-001.                                                          DIRECT00
024300             OPEN INPUT DIRECTORY.                                DIRECT00
024400             COMPUTE TIME-HOLD = DATA (1).                        DIRECT00
024500             MOVE TODAYS-DATE TO FILL-DATE.                       DIRECT00
024600                                                                  DIRECT00
024700 IN-002.                                                          DIRECT00
024800             READ DIRECTORY  AT END GO TO INPUT-END.              DIRECT00
024900                 NOTE  AT END SHOULD NOT OCCUR.                   DIRECT00
025000             MOVE 16   TO DE-BLOCK.                               DIRECT00
025100                                                                  DIRECT00
025200 IN-003-DEBLOCKER.                                                DIRECT00
025300             SUBTRACT 1 FROM DE-BLOCK.                            DIRECT00
025400             IF DE-BLOCK = 0           GO TO IN-002.              DIRECT00
025500                                                                  DIRECT00
025600                 NOTE  CHECK FOR END OF DIRECTORY OCTAL 114.      DIRECT00
025700             MOVE PREFIX OF DIRECTORY (DE-BLOCK) TO TEST-OCTAL-14.DIRECT00
025800             IF CMP-OCTAL-14 = 76 GO TO INPUT-END.                DIRECT00
025900                                                                  DIRECT00
026000                 NOTE  CHECK FOR UNUSED LOCATION OCTAL 14.        DIRECT00
026100             IF CMP-OCTAL-14 = 12 GO TO IN-003-DEBLOCKER.         DIRECT00
026200                                                                  DIRECT00
026300 IN-004-NOTE.    NOTE  THE FOLLOWING FORMATS DATA FROM THE DISK   DIRECT00
026400                     DIRECTORY INTO RECORDS FOR THE SORT.         DIRECT00
026500 IN-004-DEBLOCKER.                                                7-5-6800
026600             MOVE CMP-3(DE-BLOCK) TO TEST-WORD-3.                 7-5-6800
026700             MOVE CMP-4(DE-BLOCK) TO TEST-WORD-4.                 7-5-6800
026800             MOVE TEST-WORD-3 TO FORMAT-TEST[1:46:1].             7-5-6800
026900             IF NEW-FORMAT                                        7-5-6800
027000                 MOVE ZEROS TO CONVERT-CMP                        7-5-6800
027100                 MOVE TEST-WORD-3 TO CONVERT-CMP[12:30:18]        7-5-6800
027200                 MOVE CONVERT-CMP TO LAST-ACCESS-DATE             7-5-6800
027300               OF DIRECTORY (DE-BLOCK)                            7-5-6800
027400                 MOVE TEST-WORD-3 TO CONVERT-CMP[30:30:18]        7-5-6800
027500                 MOVE CONVERT-CMP TO CREATIONXDATE                7-5-6800
027600               OF DIRECTORY (DE-BLOCK)                            7-5-6800
027700                 MOVE ZEROS TO CONVERT-CMP                        7-5-6800
027800                 MOVE CMP-3(DE-BLOCK) TO TEST-WORD-3              7-5-6800
027900                 MOVE TEST-WORD-3 TO CONVERT-CMP[2:38:10]         7-5-6800
028000                 MOVE CONVERT-CMP TO TEST-WORD-3[31:1:17]         7-5-6800
028100                 MOVE TEST-WORD-3 TO CMP-3(DE-BLOCK).             7-5-6800
028200             MOVE PREFIX OF DIRECTORY (DE-BLOCK) TO               DIRECT00
028300         PREFIX OF SORTER.                                        DIRECT00
028400             MOVE SUFFIX OF DIRECTORY (DE-BLOCK) TO               DIRECT00
028500         SUFFIX OF SORTER.                                        DIRECT00
028600             MOVE 1 TO TYPE.                                      DIRECT00
028700             MOVE 00 TO ROW-NUM OF SORTER.                        DIRECT00
028800             MOVE WORD-9 (DE-BLOCK) TO MAX-ROWS OF SORTER.        DIRECT00
028900             MOVE WORD-ZERO(DE-BLOCK) TO WORD-0.                  DIRECT00
029000             MOVE ZEROS TO CMP-OCTAL-14.                          DIRECT00
029100             MOVE WORD-0 TO CMP-OCTAL-14[1:34:14].                DIRECT00
029200             MOVE CMP-OCTAL-14 TO RL OF SORTER.                   DIRECT00
029300             MOVE ZEROS TO CMP-OCTAL-14.                          DIRECT00
029400             MOVE WORD-0 TO CMP-OCTAL-14 [30:36:12].              DIRECT00
029500             MOVE WORD-2(DE-BLOCK) TO USER-NUMBER.                DIRECT00
029600             MOVE CMP-OCTAL-14 TO BFACTOR OF SORTER.              DIRECT00
029700             MOVE PURGE-FACTOR OF WORD-3(DE-BLOCK) TO PURGE-HOLD. DIRECT00
029800             MOVE PURGE-CMP TO PURGE-FACTOR OF SORTER.            DIRECT00
029900             MOVE CREATIONXDATE OF WORD-3(DE-BLOCK)               DIRECT00
030000         TO CREATIONXDATE OF SORT-REC.                            DIRECT00
030100             MOVE LAST-ACCESS-DATE OF DIRECTORY (DE-BLOCK)        DIRECT00
030200         TO LAST-ACCESS-DATE OF SORTER.                           DIRECT00
030300             ADD 1 TO WORD-7(DE-BLOCK).                           DIRECT00
030400             MOVE WORD-7 (DE-BLOCK) TO NO-OF-RECORDS OF SORTER.   DIRECT00
030500             MOVE WORD-8 (DE-BLOCK) TO SEGMENTS-IN-ROW OF SORTER. DIRECT00
030600                                                                  DIRECT00
030700             MOVE ZERO TO REC-MAKE.                               DIRECT00
030800 IN-005-NOTE.    NOTE  THE FOLLOWING CHECKS ROW ADDRESS           DIRECT00
030900                     A ROW ADDRESS OF 0 INDICATES THAT NO FURTHER DIRECT00
031000                     ROWS EXIST FOR THAT FILE AND AN ORGANIZED    DIRECT00
031100                     EXIT IS PROVIDED                             DIRECT00
031200                     A ROW ADDRESS OF OTHER THAN 0 WILL ALLOW     DIRECT00
031300                     2 RECORDS TO BE RELEASED TO THE SORT         DIRECT00
031400                        THE FIRST OR TYPE 1 WILL BE SORTED BY     DIRECT00
031500                        MFID-ID                                   DIRECT00
031600                        THE SECOND OR TYPE 2 WILL BE SORTED BY    DIRECT00
031700                        ADDRESS FOR MAP OF DISK UTILIZATION.      DIRECT00
031800 IN-005-RECORD-MAKER.                                             DIRECT00
031900             ADD 1 TO REC-MAKE.                                   DIRECT00
032000             IF REC-MAKE EXCEEDS 20 GO TO IN-003-DEBLOCKER.       DIRECT00
032100             IF ROW-ADDRESS OF DIRECTORY(DE-BLOCK, REC-MAKE)      DIRECT00
032200         EQUALS 0 GO TO IN-003-DEBLOCKER.                         DIRECT00
032300             ADD 1 TO ROW-NUM OF SORTER.                          DIRECT00
032400             MOVE ROW-ADDRESS OF DIRECTORY(DE-BLOCK, REC-MAKE) TO DIRECT00
032500         ROW-ADDRESS OF SORTER.                                   DIRECT00
032600             MOVE SORT-REC TO HOLD64.                             DIRECT00
032700             RELEASE SORT-REC.                                    DIRECT00
032800             MOVE HOLD64 TO SORT-REC.                             DIRECT00
032900             MOVE 2 TO TYPE.                                      DIRECT00
033000             MOVE ROW-ADDRESS OF SORTER TO PREFIX OF SORTER.      DIRECT00
033100             MOVE PREFIX OF DIRECTORY(DE-BLOCK) TO                DIRECT00
033200         PREFIX-IN-ROW-ADDRESS OF SORTER.                         DIRECT00
033300             RELEASE SORT-REC.                                    DIRECT00
033400             MOVE HOLD64 TO SORT-REC.                             DIRECT00
033500             GO TO IN-005-RECORD-MAKER.                           DIRECT00
033600                                                                  DIRECT00
033700 INPUT-END.                                                       DIRECT00
033800             CLOSE DIRECTORY WITH RELEASE.                        DIRECT00
033900                                                                  DIRECT00
034000 OUTPUTTER SECTION.                                               DIRECT00
034100 OUT-001.                                                         DIRECT00
034200             OPEN OUTPUT LISTING.                                 DIRECT00
034300             WRITE DIRECTORY-LISTING-RECORD                       DIRECT00
034400         BEFORE ADVANCING TO CHANNEL 1.                           DIRECT00
034500             COMPUTE HEAD-HOUR = TIME-HOLD DIV 216000.            DIRECT00
034600             COMPUTE HEAD-MINUTE = TIME-HOLD MOD                  DIRECT00
034700         216000 DIV 3600.                                         DIRECT00
034800             PERFORM OVERFLOW-HEADING.                            DIRECT00
034900                                                                  DIRECT00
035000 OUT-002-NOTE.   NOTE  THE FOLLOWING RETRIEVES SORTED TYPE 1      DIRECT00
035100                     RECORDS, FORMATS THEM AND PRINTS THEM.       DIRECT00
035200                     THE FIRST TYPE 2 RECORD TERMINATES           DIRECT00
035300                     THE PROCEDURE.                               DIRECT00
035400 OUT-002.                                                         DIRECT00
035500             RETURN SORTER   AT END GO TO END-OUTPUT.             DIRECT00
035600             IF TYPE = 2 GO TO OUT-003-PROCESS-TYPE-2.            DIRECT00
035700             IF ROW-NUM OF SORTER EXCEEDS 01                      DIRECT00
035800                 MOVE SPACES TO DIRECTORY-LISTING-RECORD          DIRECT00
035900                 MOVE ROW-ADDRESS OF SORTER TO ROW-ADDRESS        DIRECT00
036000                 OF DIRECTORY-LISTING-RECORD                      DIRECT00
036100                 MOVE ROW-NUM OF SORTER TO ROW-NUM                DIRECT00
036200                 OF DIRECTORY-LISTING-RECORD    ELSE              DIRECT00
036300             ADD 1 TO TOTAL-NO-OF-FILES                           DIRECT00
036400             MOVE CORRESPONDING SORT-REC                          DIRECT00
036500         TO DIRECTORY-LISTING-RECORD                              DIRECT00
036600             MOVE CORRESPONDING SORT-KEY                          DIRECT00
036700         TO DIRECTORY-LISTING-RECORD                              DIRECT00
036800             MOVE " / " TO B-SLASH-B                              DIRECT00
036900             IF USER-NUMBER ! "00000000"                          DIRECT00
037000                     MOVE USERS-NO TO PRINT-USER                  DIRECT00
037100                     ELSE MOVE SPACES TO PRINT-USER.              DIRECT00
037200             ADD 1 TO LINE-COUNT.                                 DIRECT00
037300             IF LINE-COUNT EXCEEDS MAX-LINES                      DIRECT00
037400                     WRITE DIRECTORY-LISTING-RECORD               DIRECT00
037500                 BEFORE ADVANCING TO CHANNEL 1                    DIRECT00
037600                     PERFORM OVERFLOW-HEADING                     DIRECT00
037700                         ELSE                                     DIRECT00
037800                     WRITE DIRECTORY-LISTING-RECORD.              DIRECT00
037900             GO TO OUT-002.                                       DIRECT00
038000                                                                  DIRECT00
038100 OVERFLOW-HEADING.                                                DIRECT00
038200             ADD 1 TO PAGE-COUNT.                                 DIRECT00
038300             MOVE PAGE-COUNT TO HEAD-PAGE-NO.                     DIRECT00
038400             WRITE DIRECTORY-LISTING-RECORD FROM                  DIRECT00
038500         FIRST-HEADER BEFORE ADVANCING 2 LINES.                   DIRECT00
038600             WRITE DIRECTORY-LISTING-RECORD FROM SECOND-HEADER.   DIRECT00
038700             WRITE DIRECTORY-LISTING-RECORD FROM                  DIRECT00
038800         THIRD-HEADER BEFORE ADVANCING 2 LINES.                   DIRECT00
038900             MOVE 0 TO LINE-COUNT.                                DIRECT00
039000             MOVE SPACES TO DIRECTORY-LISTING-RECORD.             DIRECT00
039100                                                                  DIRECT00
039200 OUT-003-PROCESS-TYPE-2.                                          DIRECT00
039300             MOVE SPACES TO DIRECTORY-LISTING-RECORD.             DIRECT00
039400             MOVE 0 TO PAGE-COUNT.                                DIRECT00
039500             WRITE DIRECTORY-LISTING-RECORD BEFORE                DIRECT00
039600         ADVANCING TO CHANNEL 1.                                  DIRECT00
039700             MOVE "MAP OF DISK UTILIZATION" TO REPORT-TITLE.      DIRECT00
039800             PERFORM OVERFLOW-HEADING.                            DIRECT00
039900                                                                  DIRECT00
040000 OUT-004-NOTE.   NOTE  THE FOLLOWING CREATES THE AVAILABLE        DIRECT00
040100                     DISK AREA LINES.                             DIRECT00
040200 OUT-004.                                                         DIRECT00
040300             MOVE PREFIX      OF SORTER TO TEST-1.                DIRECT00
040400                                                                  DIRECT00
040500 OUT-004-RECYCLE.                                                 DIRECT00
040600             IF PREV-ADDRESS > TEST-ADDRESS                       DIRECT00
040700                     GO TO OUT-005-PRINT-IN-USE-LINES.            DIRECT00
040800             IF PREV-ADDRESS = TEST-ADDRESS                       DIRECT00
040900                     COMPUTE PREV-1 = TEST-1                      DIRECT00
041000                 PLUS SEGMENTS-IN-ROW OF SORTER                   DIRECT00
041100                     GO TO OUT-005-PRINT-IN-USE-LINES.            DIRECT00
041200                                                                  DIRECT00
041300             IF P-EU-NO = T-EU-NO                                 DIRECT00
041400                     MOVE " -------   -------      ----       ----DIRECT00
041500*    "     ---      -----    -----   AVAILABLE  DISK  AREA"       DIRECT00
041600                 TO DIRECTORY-LISTING-RECORD                      DIRECT00
041700                     COMPUTE TOTAL-AVAILABLE-DISK =               DIRECT00
041800                 TOTAL-AVAILABLE-DISK + TEST-1 - PREV-1           DIRECT00
041900                     ADD 1 TO NUMBER-OF-AVAILABLE-AREAS           DIRECT00
042000                     MOVE "--" TO ROW-NUM                         DIRECT00
042100                 OF DIRECTORY-LISTING-RECORD                      DIRECT00
042200                     COMPUTE SEGMENTS-IN-ROW                      DIRECT00
042300                 OF DIRECTORY-LISTING-RECORD FROM                 DIRECT00
042400                 TEST-1 - PREV-1                                  DIRECT00
042500                     MOVE PREV-1 TO ROW-ADDRESS                   DIRECT00
042600                 OF DIRECTORY-LISTING-RECORD                      DIRECT00
042700                     ADD 1 TO LINE-COUNT                          DIRECT00
042800                     COMPUTE PREV-1 = TEST-1                      DIRECT00
042900                 PLUS SEGMENTS-IN-ROW OF SORTER                   DIRECT00
043000                     IF LINE-COUNT EXCEEDS MAX-LINES              DIRECT00
043100                             WRITE DIRECTORY-LISTING-RECORD       DIRECT00
043200                         BEFORE ADVANCING TO CHANNEL 1            DIRECT00
043300                             PERFORM OVERFLOW-HEADING             DIRECT00
043400                             GO TO OUT-005-PRINT-IN-USE-LINES     DIRECT00
043500                                 ELSE                             DIRECT00
043600                             WRITE DIRECTORY-LISTING-RECORD       DIRECT00
043700                         MOVE SPACES TO DIRECTORY-LISTING-RECORD  DIRECT00
043800                     GO TO OUT-005-PRINT-IN-USE-LINES.            DIRECT00
043900                                                                  DIRECT00
044000                 NOTE  THE NEXT IN USE AREA OF DISK IS NOT        DIRECT00
044100                     IN THE SAME DFEU.                            DIRECT00
044200             MOVE " -------   -------      ----       ----     ---DIRECT00
044300*    "      -----    -----   AVAILABLE  DISK  AREA"               DIRECT00
044400         TO DIRECTORY-LISTING-RECORD.                             DIRECT00
044500             MOVE "--" TO ROW-NUM OF DIRECTORY-LISTING-RECORD.    DIRECT00
044600             COMPUTE AVAIL-SEGS                                   DIRECT00
044700         FROM MAXIMUM-ADDRESS(P-EU-NO + 1) - P-ADDRS.             DIRECT00
044800             IF AVAIL-SEGS = 0                                    DIRECT00
044900                     MOVE ZEROS TO P-ADDRS                        DIRECT00
045000                     MOVE SPACES TO DIRECTORY-LISTING-RECORD      DIRECT00
045100                     GO TO OUT-004-RECYCLE.                       DIRECT00
045200             MOVE AVAIL-SEGS TO SEGMENTS-IN-ROW                   DIRECT00
045300         OF DIRECTORY-LISTING-RECORD.                             DIRECT00
045400             COMPUTE TOTAL-AVAILABLE-DISK = TOTAL-AVAILABLE-DISK  DIRECT00
045500         + AVAIL-SEGS.                                            DIRECT00
045600             ADD 1 TO NUMBER-OF-AVAILABLE-AREAS.                  DIRECT00
045700             MOVE PREV-1 TO ROW-ADDRESS OF LISTING.               DIRECT00
045800             ADD 1 TO P-EU-NO.                                    DIRECT00
045900             MOVE ZEROS TO P-ADDRS.                               DIRECT00
046000             ADD 1 TO LINE-COUNT.                                 DIRECT00
046100             IF LINE-COUNT EXCEEDS MAX-LINES                      DIRECT00
046200                     WRITE DIRECTORY-LISTING-RECORD               DIRECT00
046300                 BEFORE ADVANCING TO CHANNEL 1                    DIRECT00
046400                     PERFORM OVERFLOW-HEADING                     DIRECT00
046500                         ELSE                                     DIRECT00
046600                     WRITE DIRECTORY-LISTING-RECORD.              DIRECT00
046700             MOVE SPACES TO DIRECTORY-LISTING-RECORD.             DIRECT00
046800             GO TO OUT-004-RECYCLE.                               DIRECT00
046900                                                                  DIRECT00
047000 OUT-005-PRINT-IN-USE-LINES.                                      DIRECT00
047100             MOVE CORRESPONDING SORT-REC                          DIRECT00
047200         TO DIRECTORY-LISTING-RECORD.                             DIRECT00
047300             MOVE CORRESPONDING SORT-KEY                          DIRECT00
047400         TO DIRECTORY-LISTING-RECORD.                             DIRECT00
047500             MOVE PREFIX OF SORTER                                DIRECT00
047600         TO ROW-ADDRESS OF DIRECTORY-LISTING-RECORD.              DIRECT00
047700             MOVE ROW-ADDRESS OF SORTER                           DIRECT00
047800         TO PREFIX OF DIRECTORY-LISTING-RECORD.                   DIRECT00
047900             MOVE " / " TO B-SLASH-B.                             DIRECT00
048000             ADD 1 TO LINE-COUNT.                                 DIRECT00
048100             IF LINE-COUNT EXCEEDS MAX-LINES                      DIRECT00
048200                     WRITE DIRECTORY-LISTING-RECORD               DIRECT00
048300                 BEFORE ADVANCING TO CHANNEL 1                    DIRECT00
048400                     PERFORM OVERFLOW-HEADING                     DIRECT00
048500                         ELSE                                     DIRECT00
048600                     WRITE DIRECTORY-LISTING-RECORD.              DIRECT00
048700         MOVE SPACES TO DIRECTORY-LISTING-RECORD.                 DIRECT00
048800                                                                  DIRECT00
048900                 NOTE  GET THE NEXT RECORD FROM THE SORT.         DIRECT00
049000             RETURN SORTER   AT END GO TO END-OUTPUT.             DIRECT00
049100             GO TO OUT-004.                                       DIRECT00
049200                                                                  DIRECT00
049300 END-OUTPUT-NOTE.                                                 DIRECT00
049400                 NOTE  FIRST PRINT OUT AVAILABLE DISK STATISTICS  DIRECT00
049500                     FOR DISK AREAS PAST THE LAST USER AREA.      DIRECT00
049600 END-OUTPUT.                                                      DIRECT00
049700             IF P-ADDRS + 1 EXCEEDS MAXIMUM-ADDRESS(P-EU-NO + 1)  DIRECT00
049800                     ADD 1 TO P-EU-NO                             DIRECT00
049900                     MOVE ZEROS TO P-ADDRS                        DIRECT00
050000                     IF P-EU-NO = 20                              DIRECT00
050100                             GO TO TURN-OFF-THE-PROGRAM           DIRECT00
050200                     ELSE    GO TO END-OUTPUT.                    DIRECT00
050300                                                                  DIRECT00
050400             MOVE " -------   -------      ----       ----     ---DIRECT00
050500*    "      -----    -----   AVAILABLE  DISK  AREA"               DIRECT00
050600         TO DIRECTORY-LISTING-RECORD.                             DIRECT00
050700             MOVE "--" TO ROW-NUM OF DIRECTORY-LISTING-RECORD.    DIRECT00
050800             COMPUTE SEGMENTS-IN-ROW OF DIRECTORY-LISTING-RECORD  DIRECT00
050900         FROM MAXIMUM-ADDRESS(P-EU-NO + 1) - P-ADDRS.             DIRECT00
051000             COMPUTE TOTAL-AVAILABLE-DISK = TOTAL-AVAILABLE-DISK  DIRECT00
051100         + MAXIMUM-ADDRESS(P-EU-NO + 1) - P-ADDRS.                DIRECT00
051200             ADD 1 TO NUMBER-OF-AVAILABLE-AREAS.                  DIRECT00
051300             MOVE PREV-1 TO ROW-ADDRESS OF LISTING.               DIRECT00
051400             ADD 1 TO P-EU-NO.                                    DIRECT00
051500             MOVE ZEROS TO P-ADDRS.                               DIRECT00
051600             ADD 1 TO LINE-COUNT.                                 DIRECT00
051700             IF LINE-COUNT EXCEEDS MAX-LINES                      DIRECT00
051800                     WRITE DIRECTORY-LISTING-RECORD               DIRECT00
051900                 BEFORE ADVANCING TO CHANNEL 1                    DIRECT00
052000                     PERFORM OVERFLOW-HEADING                     DIRECT00
052100                         ELSE                                     DIRECT00
052200                     WRITE DIRECTORY-LISTING-RECORD.              DIRECT00
052300             GO TO END-OUTPUT.                                    DIRECT00
052400                                                                  DIRECT00
052500 TURN-OFF-THE-PROGRAM.                                            DIRECT00
052600             MOVE NUMBER-OF-AVAILABLE-AREAS TO TOTAL-AREAS.       DIRECT00
052700             MOVE TOTAL-AVAILABLE-DISK TO AVAILABLE-SEGMENTS.     DIRECT00
052800             WRITE DIRECTORY-LISTING-RECORD FROM TOTAL-LINE.      DIRECT00
052900             MOVE "THERE ARE        FILES ON DISK" TO TOTAL-LINE. DIRECT00
053000             MOVE TOTAL-NO-OF-FILES TO TOTAL-AREAS.               DIRECT00
053100             WRITE DIRECTORY-LISTING-RECORD FROM TOTAL-LINE.      DIRECT00
053200             CLOSE LISTING.                                       DIRECT00
053300                                                                  DIRECT00
053400 END-OF-PROGRAM SECTION.                                          DIRECT00
053500 LAST-PARAGRAPH.                                                  DIRECT00
053600             STOP RUN.                                            DIRECT00
053700 END-OF-JOB.                                                      DIRECT00
