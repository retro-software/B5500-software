BEGIN    % PROGRAM SUPER REPORT - A GENERAL REPORT WRITER               00000100
         %   BY RON JORGENSEN      YOUNG & RUBICAM INC.                 00000200
COMMENT        THE CUBE LIBRARY NUMBER IS S000002.  THE PROGRAM NAME IS 00000300
               "SUPER/REPORT".  THIS VERSION DATED 5/14/67;             00000400
COMMENT   FILE CARDSIN MUST BE LABEL EQUATED TO TAPE EITHER WHEN        00000500
          COMPILING OR EXECUTING  ;                                     00000600
DEFINE        PLUGMAX = 20 #,  CALCMAX = 5  #  ;                        00000700
DEFINE        DEVELOPEARRAYMAX = 30  # ;                                00000800
DEFINE        IMPDEVMAX =  12 # ;                                       00000900
DEFINE        THRU  =  STEP 1 UNTIL  # ;                                00001000
FILE          PTR 4 (2,15 ) ;                                           00001100
BOOLEAN       LISTING, RECORDTOTALING, DEVELOPING, DEVTOTALING,         00001200
              BREAKING, CALCULATING, TOTALING, TOPTITLEXISTS,           00001300
              BOMTITLEXISTS, COLUMNHEADINGS ;                           00001400
REAL          INDENT ;                                                  00001500
INTEGER       NUMTOPTITLES, NUMWDSPEREC, NUMWDSPERBLK, NUMHDREC, NUMCTL,00001600
              I, J, K, NUMHEDLINES, BOMTITLELINECT, NUMBOMTITLES,       00001700
              NUMRECTOTLFLDS, NUMDEVTOTLFLDS, NUMLINEFLDS, NUMTOTFLDS,  00001800
              NUMCTR, PGBRK, LINEMAX, DBLBRKMAX, DBLCTLMAX ;            00001900
INTEGER ARRAY ADDRECKEY8[1:100           , 0:2] ,                       00002000
              ADDEVKEY8 [1:20            , 0:2] ;                       00002100
BOOLEAN ARRAY      ISDEVELOPE [0:PLUGMAX],                              00002200
                   ISCALC  [0:CALCMAX];                                 00002300
ARRAY         CTLID8[0:35         ],                                    00002400
                  TOTLINEKEY8[0:120            ] ,                      00002500
              DATERAY [0:2]  ,                                          00002600
                   QUOTARRAY[0:14],                                     00002700
                   TOTALQUOT[0:14],                                     00002800
              TOPTITLERAY8[1:25          , 0:10] ,                      00002900
              BOMTITLERAY8[1:25          , 0:10] ,                      00003000
              SQUEEZE8[0: 140       ],                                  00003100
                   KEYARRAY8[1:15    ],                                 00003200
                   FIELDKEY8[0: 120             ],                      00003300
              COLUMNHEADS8[1:10         , 0:19 ] ;                      00003400
FORMAT        T (X27,11A6 / ) ,                                         00003500
              H (20A6) ;                                                00003600
LABEL         ENDJOB ;                                                  00003700
%-----------------------------------------------------------------------00003800
STREAM PROCEDURE MOVEWORDS (FROM, TU, NUMWORDS ) ;                      00003900
VALUE         NUMWORDS ;                                                00004000
BEGIN                                                                   00004100
              SI ~ FROM ;                                               00004200
              DI ~ TU   ;                                               00004300
              DS ~ NUMWORDS WDS ;                                       00004400
END OF MOVEWORDS PROCEDURE ;                                            00004500
%-----------------------------------------------------------------------00004600
INTEGER STREAM PROCEDURE COMPARE16CHAR (FIELDNAME, SQUEEZE, NUMCTR );   00004700
VALUE    NUMCTR ;                                                       00004800
BEGIN                                                                   00004900
LABEL    HIT ;                                                          00005000
                   DI ~ SQUEEZE ;                                       00005100
                   TALLY ~ 1 ;                                          00005200
            NUMCTR(SI ~ FIELDNAME ;                                     00005300
                   IF 16 SC = DC                                        00005400
                   THEN JUMP OUT TO HIT                                 00005500
                   ELSE TALLY ~ TALLY + 1 ) ;                           00005600
                   TALLY ~ 63 ;  % INVALID FIELD NAME SETS INVALID INDEX00005700
HIT :              COMPARE16CHAR ~ TALLY ;                              00005800
END OF PROCEDURE   COMPARE16CHAR ;                                      00005900
%-----------------------------------------------------------------------00006000
BEGIN                                                                   00006100
COMMENT  THE PRE-EDIT BLOCK READS AND INTERPRETS THE INPUT PARAMETERS ; 00006200
FILE IN       CARDSIN DISK SERIAL (2, 10, 150 ) ;                       00006300
FILE IN       DICTION DISK SERIAL (2, 3, 90);                           00006400
DEFINE        SCANIT = SCAN (CARDSIN, QC, TYPE, STRING )   #  ,         00006500
              IT     = STRING [0]     #   ;                             00006600
ARRAY         STRING [0:19] ,                                           00006700
              FILEFIELD [0:2] ,                                         00006800
              READRAY [0:9] ,                                           00006900
              PLUGDICT [0:PLUGMAX, 0:6 ],                               00007000
              CALCDICT [0:CALCMAX, 0:6 ],                               00007100
              IMPLYDEVELOPES [0:IMPDEVMAX, 0:2 ] ;                      00007200
REAL          NAME  ;                                                   00007300
INTEGER       PLUGNO, TYPE, CHARCT, S, WHERE, NUMERR, SKIPCHR,          00007400
              WHICHONE, A,B,C, IFNUM, IFDEC, DIGIT, NUMDEC, SPASE,      00007500
              TIPE, QUOTCHR, TQCHR, LINECHR, TOTLCHR  ;                 00007600
BOOLEAN       QC, TIPE2  ;                                              00007700
FORMAT                                                                  00007800
              F (2A6 ) ,                                                00007900
              F1 (X16, A1, I2, A3 ),                                    00008000
              F2 (X16, A1, I2, 4A1),                                    00008100
              F3 (X16, A1, I2, I3,A1 ),                                 00008200
              F4 ("NUMBER OF ERRORS DETECTED = ", I3 ) ,                00008300
              F5 (A8, 2A6, A4 ),                                        00008400
              F6 (I4, X3, 2A6, A4 ),                                    00008500
              F7 (I12),                                                 00008600
              F8 (X19, 3A1 ),                                           00008700
              D ("00",A2,"/",A2,"/00",A2,"    00DATE  ") ;              00008800
LABEL         TOTALS, EOF ;                                             00008900
COMMENT  SCAN, EDITNEW, SPLICEARRAY, AND CALENDAR ARE CALLED HERE FROM  00009000
         SUBLIBRARY. SCAN IS MODIFIED TO ACCEPT HYPHENS IN ALPHAMERICS ;00009100
ALPHA PROCEDURE YRDAYTONORM (A);                                        00009200
VALUE A; ALPHA A;                                                       00009300
COMMENT A IS OF FORM YYDDD I.E. SAME FORM AS TIME(0).                   00009400
YRDAYTONORM WILL BE IN FORMAT 312 REPRESENTING YR, MO, DA;              00009500
BEGIN INTEGER YR,MO,DAY, I;                                             00009600
DEFINE LEAPYEAR = REAL(BOOLEAN(YR) AND BOOLEAN(3)) = 0#;                00009700
ALPHA ARRAY AL[0:0], AT[0:0] ;                                          00009800
FORMAT                                                                  00009900
F1("00",3I2),                                                           00010000
F2 (X3,I2,I3);                                                          00010100
INTEGER ARRAY MONTHCODE [1:12];                                         00010200
FILL MONTHCODE[*] WITH 0,31,59,90,120,151,181,212,243,273,304,334;      00010300
AL[0]~A; READ (AL[*],F2,YR, DAY);                                       00010400
IF LEAPYEAR AND DAY = 60 THEN                                           00010500
BEGIN MO ~ 2; DAY~29 END ELSE BEGIN                                     00010600
IF LEAPYEAR AND DAY > 60 THEN DAY~ DAY-1;                               00010700
MO ~13; DO MO~MO-1 UNTIL DAY>MONTHCODE [MO] ;                           00010800
DAY~ DAY-MONTHCODE[MO] END;                                             00010900
WRITE(AT[*],F1,YR,MO,DAY);                                              00011000
YRDAYTONORM ~ AT[0];                                                    00011100
END YRDAYTONORM;                                                        00011200
%***********************************************************************00011300
STREAM PROCEDURE  EDITNEW  (INARRAY, OUTARRAY, QS )  ;                  00011400
VALUE         QS  ;                                                     00011500
BEGIN                                                                   00011600
COMMENT    EDITNEW SCANS A FREE FIELD RECORD (INARRAY) THEN             00011700
           IDENTIFIES AND STORES DISTINCT ENTITIES IN 2 WORD            00011800
           COUPLETS IN OUTARRAY.                                        00011900
           IF THE INPUT PARAMETER QS HAS BEEN SET FALSE THEN            00012000
           QUOTE MARKS WILL BE REGARDED AS DELIMETERS TO QUOTE          00012100
           STRINGS.                                                     00012200
           IF QS IS TRUE THEN QUOTE MARKS ARE REGARDED AS ANY           00012300
           OTHER SPECIAL CHARACTER.                                     00012400
           INARRAY IS A STRING OF CHARACTERS (MAXIMUM 80) ENDING        00012500
           WITH AN ARROW.                                               00012600
           OUTARRAY IS AN ARRAY OF 2 WORD COUPLETS.                     00012700
           IF A QUOTE STRING OR ALPHANUMERIC EXCEEDS 12 CHARACTERS      00012800
           THEN AN EXTRA COUPLET(S) IS FORMED TO CONTAIN THE EXCESS     00012900
           CHARACTERS.                                                  00013000
OUT ARRAY WILL NOT EXCEED 160 WORDS.                                    00013100
THE TYPES OF PHRASES RECOGNIZED ARE                                     00013200
   - ANY SPECIAL CHARACTER                                              00013300
   - ALPHANUMERICS OF ANY LENGTH CONTAINED ON A SINGLE RECORD           00013400
   - ANY STRING IN QUOTES (IF PERAMETER QS HAS BEEN SET TO FALSE)       00013500
     THE CHARACTER " IS PERMISSABLE AS THE FIRST CHARACTER IN A QUOTE.  00013600
   - UNSIGNED INTEGERS UP TO 8 DIGITS                                   00013700
INTEGERS CONTAINING MORE THAN 8 DIGITS ARE SPLIT IN TWO BY THE HARDWARE,00013800
E.G.,  987654321 BECOMES  9 87654321, REQUIRING 2 COUPLETS IN OUTARRAY. 00013900
CONTENTS  OF  OUTARRAY  COUPLET                            *******      00014000
    TYPE OF         CODE       CODE       PHRASE                 *      00014100
     PHRASE        COMPARE     WORD 1     WORD 2                 *      00014200
                                                                 *      00014300
UNSIGNED INTEGER   WORD1 = 0     0      INTEGER PHRASE           *      00014400
BLANKS             WORD1 = 2     2      6 BLANKS                 *      00014500
SPECIAL CHAR       WORD1 = 3     3      SPEC CHAR                *      00014600
ARROW              WORD1 = 4     4      ARROW                    *      00014700
ALPHANUMERIC       WORD1 > 4   PHRASE   PHRASE (CONT)            *      00014800
QUOTE STRING       WORD1 < 0   PHRASE   PHRASE (CONT)            *      00014900
PHRASE TYPES INTEGER, SPECIAL CHAR, AND ARROW, ARE RIGHT JUSTIFIED IN   00015000
WORD 2.  ALPHANUMERIC AND QUOTE PHRASES ARE LEFT JUSTIFIED IN THE       00015100
LAST 6 CHARACTERS OF WORDS 1 AND 2.                                     00015200
WORD 1 CONTAINS THE CODE IDENTIFYING THE TYPE OF PHRASE.                00015300
THE TABLE ABOVE SHOWS HOW WORD 1 MAY BE TESTED, IN WORD MODE, TO        00015400
IDENTIFY THE TYPE OF PHRASE CONTAINED IN THE COUPLET.                   00015500
BITS 3-8 ( 6 BITS )  OF WORD 1 CONTAIN  A (BINARY) COUNT OF CHARACTERS  00015600
IN THE PHRASE. THIS SERVES THREE PURPOSES--                             00015700
 -BLANKS AT THE END OF A QUOTE WHICH ARE PART OF THE QUOTE MAY BE       00015800
   DISTINGUISHED FROM FILLER BLANKS.                                    00015900
 -INTEGERS MAY BE DISTINGUISHED FROM SPLIT INTEGERS.                    00016000
 -THE COUNT GIVES THE NUMBER OF DECIMAL PLACES WHEN THE INTEGER FOLLOWS 00016100
   A PERIOD.                                                            00016200
THE COUNT MAY BE REFERENCED BY A PARTIAL WORD DESIGNATOR OF THIS FORM,  00016300
WORD1.[3:6]          ;                                                  00016400
LOCAL         SAVESI, NEXTSI, NEXTDI, CHARCT, R1, R2, R3, R4,           00016500
              BLANKS, IFQUOTE, IFMIDQUOTE, IFMIDALFA ;                  00016600
LABEL         PHRASE, COUNT, ALFATEST, QUOTE, UNQUOTE,                  00016700
              BLANK, SPECIALCHAR, ARRO, MIDQUOTE ,                      00016800
              SWIT, MOVEALFA, ARROW, CH1, CH2, CH3, CH4, CH5,           00016900
              CH6, CH7, CH8, CH9, CH10, CH11, CH12    ;                 00017000
              DI ~ LOC BLANKS ;                                         00017100
              DS ~ 6 LIT " "   ;                                        00017200
              DI  ~  INARRAY ;                                          00017300
           2 (DI  ~ DI +40 )   ;                                        00017400
              DS  ~ LIT "~"  ;                                          00017500
              SI ~ INARRAY ;                                            00017600
              NEXTSI ~ SI ;                                             00017700
              DI ~ OUTARRAY ;                                           00017800
              NEXTDI ~ DI   ;                                           00017900
PHRASE :      SI ~ NEXTSI  ;                                            00018000
              SAVESI ~  SI ;                                            00018100
              IFMIDQUOTE (JUMP OUT TO MIDQUOTE )  ;                     00018200
              IF SC = " " THEN GO TO BLANK  ;                           00018300
              IF SC = ALPHA  THEN GO TO  ALFATEST ;                     00018400
              IFMIDALFA (JUMP OUT TO ALFATEST) ;                        00018500
              IF SC =  "~"   THEN GO TO  ARROW    ;                     00018600
              IF SC =  """   THEN GO TO  QUOTE    ;                     00018700
SPECIALCHAR :                                                           00018800
              DI ~ NEXTDI  ;                                            00018900
              DS ~ 7 LIT "0" ;  DS ~ LIT "3" ;  %  FILL WORD 1          00019000
              DS ~ 7 LIT "0" ;  DS ~ CHR     ;  %  FILL WORD 2          00019100
              NEXTSI ~ SI  ; NEXTDI ~ DI  ;                             00019200
              GO  TO  PHRASE ;                                          00019300
BLANK  :      SI ~ SI +1 ;                                              00019400
              2 (40 (IF SC ! " " THEN JUMP OUT ELSE SI ~ SI +1 ) ) ;    00019500
              NEXTSI ~ SI ;                                             00019600
              DI ~  NEXTDI   ;                                          00019700
              DS ~ 7 LIT "0" ;                                          00019800
              DS ~   LIT "2" ;                                          00019900
              DS ~ 2 LIT "0" ;                                          00020000
              DS ~ 6 LIT " " ;                                          00020100
              NEXTDI ~ DI  ;                                            00020200
              GO TO PHRASE ;                                            00020300
ALFATEST:     TALLY ~  1    ;       % BEGIN TO COUNT ALPHANUMERIC CHAR  00020400
              SI    ~ SI +1 ;                                           00020500
           11(IF SC = ALPHA THEN BEGIN TALLY ~ TALLY +1 ;               00020600
                                       SI ~ SI + 1 END                  00020700
ELSE IF SC = "-" THEN BEGIN TALLY ~ TALLY +1; SI ~ SI +1; END           00020800
                             ELSE JUMP OUT ) ;                          00020900
              NEXTSI ~ SI  ;                                            00021000
              CHARCT ~  TALLY ;                                         00021100
              SI  ~  SAVESI   ;                                         00021200
              IFMIDALFA (JUMP OUT TO SWIT ) ;                           00021300
              CHARCT (IF SC { "Z" THEN JUMP OUT TO SWIT                 00021400
                                 ELSE SI ~ SI + 1  )  ;                 00021500
% INTEGER PHRASE                                                        00021600
              DI ~  NEXTDI   ;                                          00021700
              DS ~ 3 RESET  ;                                           00021800
              SI ~ LOC CHARCT ;  SI ~ SI +7 ;                           00021900
              6 ( IF SB THEN DS ~ SET ELSE DS ~ RESET  ;  SKIP SB  ) ;  00022000
              DS ~ 3 RESET  ;                                           00022100
              DS ~ 6 LIT "0" ;                                          00022200
              SI ~  SAVESI   ;                                          00022300
              DS ~  CHARCT OCT ;                                        00022400
              NEXTSI ~ SI ;  NEXTDI ~ DI ;                              00022500
              GO TO PHRASE  ;                                           00022600
QUOTE:                                                                  00022700
              QS (JUMP OUT TO SPECIALCHAR )  ;                          00022800
              SI     ~ SI +1  ;                                         00022900
              SAVESI ~ SI     ;                                         00023000
              IF SC = "~" THEN GO TO ARROW  ;                           00023100
MIDQUOTE :    TALLY ~ 0 ;                                               00023200
              IFMIDQUOTE ~TALLY ;                                       00023300
              TALLY ~ 1 ;                                               00023400
              SI ~  SI +1 ;                                             00023500
              12 (IF SC = "~" THEN JUMP OUT TO ARRO ;                   00023600
                 IF SC = """ THEN JUMP OUT TO UNQUOTE ;                 00023700
                 SI ~ SI + 1 ;                                          00023800
                 TALLY ~ TALLY + 1 ) ;                                  00023900
              TALLY ~ 1 ;                                               00024000
              IFMIDQUOTE ~ TALLY ;                                      00024100
              TALLY ~ 12 ;                                              00024200
              SI~ SI -2 ;                                               00024300
UNQUOTE :       SI   ~  SI +1 ;                                         00024400
ARRO  :       NEXTSI ~  SI    ;                                         00024500
              CHARCT ~ TALLY  ;                                         00024600
              TALLY ~ 1 ;  IFQUOTE ~ TALLY ;                            00024700
              % SIMULATED BOOLEAN "IFQUOTE" ~ TRUE                      00024800
SWIT:         CI ~ CI + CHARCT ;                                        00024900
              GO TO  CH1  ;                                             00025000
              GO TO  CH1  ;                                             00025100
              GO TO  CH2  ;                                             00025200
              GO TO  CH3  ;                                             00025300
              GO TO  CH4  ;                                             00025400
              GO TO  CH5  ;                                             00025500
              GO TO  CH6  ;                                             00025600
              GO TO  CH7  ;                                             00025700
              GO TO  CH8  ;                                             00025800
              GO TO  CH9  ;                                             00025900
              GO TO  CH10 ;                                             00026000
              GO TO  CH11 ;                                             00026100
              GO TO  CH12 ;                                             00026200
              COMMENT  SUBROUTINE AT CHI ASSIGNS VALUES TO REPEAT       00026300
                       FACTORS (R1,R2,R3,R4) CORRESPONDING TO           00026400
                       CHARCT = 1.  THESE REPEAT FACTORS ARE USED TO    00026500
                       LOAD THE OUTPUT COUPLET WITH CHARACTERS AND      00026600
                       BLANKS ;                                         00026700
CH1 :         TALLY  ~  1  ;     R1  ~  TALLY  ;                        00026800
              TALLY  ~  5  ;     R2  ~  TALLY  ;                        00026900
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00027000
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00027100
              GO TO MOVEALFA ;                                          00027200
CH2 :         TALLY  ~  2  ;     R1  ~  TALLY  ;                        00027300
              TALLY  ~  4  ;     R2  ~  TALLY  ;                        00027400
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00027500
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00027600
              GO TO MOVEALFA ;                                          00027700
CH3 :         TALLY  ~  3  ;     R1  ~  TALLY  ;                        00027800
              TALLY  ~  3  ;     R2  ~  TALLY  ;                        00027900
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00028000
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00028100
              GO TO MOVEALFA ;                                          00028200
CH4 :         TALLY  ~  4  ;     R1  ~  TALLY  ;                        00028300
              TALLY  ~  2  ;     R2  ~  TALLY  ;                        00028400
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00028500
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00028600
              GO TO MOVEALFA ;                                          00028700
CH5 :         TALLY  ~  5  ;     R1  ~  TALLY  ;                        00028800
              TALLY  ~  1  ;     R2  ~  TALLY  ;                        00028900
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00029000
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00029100
              GO TO MOVEALFA ;                                          00029200
CH6 :         TALLY  ~  6  ;     R1  ~  TALLY  ;                        00029300
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00029400
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00029500
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00029600
              GO TO MOVEALFA ;                                          00029700
CH7 :         TALLY  ~  6  ;     R1  ~  TALLY  ;                        00029800
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00029900
              TALLY  ~  1  ;     R3  ~  TALLY  ;                        00030000
              TALLY  ~  5  ;     R4  ~  TALLY  ;                        00030100
              GO TO MOVEALFA ;                                          00030200
CH8 :         TALLY  ~  6  ;     R1  ~  TALLY  ;                        00030300
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00030400
              TALLY  ~  2  ;     R3  ~  TALLY  ;                        00030500
              TALLY  ~  4  ;     R4  ~  TALLY  ;                        00030600
              GO TO MOVEALFA ;                                          00030700
CH9 :         TALLY  ~  6  ;     R1  ~  TALLY  ;                        00030800
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00030900
              TALLY  ~  3  ;     R3  ~  TALLY  ;                        00031000
              TALLY  ~  3  ;     R4  ~  TALLY  ;                        00031100
              GO TO MOVEALFA ;                                          00031200
CH10 :        TALLY  ~  6  ;     R1  ~  TALLY  ;                        00031300
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00031400
              TALLY  ~  4  ;     R3  ~  TALLY  ;                        00031500
              TALLY  ~  2  ;     R4  ~  TALLY  ;                        00031600
              GO TO MOVEALFA ;                                          00031700
CH11 :        TALLY  ~  6  ;     R1  ~  TALLY  ;                        00031800
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00031900
              TALLY  ~  5  ;     R3  ~  TALLY  ;                        00032000
              TALLY  ~  1  ;     R4  ~  TALLY  ;                        00032100
              GO TO MOVEALFA ;                                          00032200
CH12 :        TALLY  ~  6  ;     R1  ~  TALLY  ;                        00032300
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00032400
              TALLY  ~  6  ;     R3  ~  TALLY  ;                        00032500
              TALLY  ~  0  ;     R4  ~  TALLY  ;                        00032600
MOVEALFA:     DI ~  NEXTDI ;                                            00032700
              DS ~ RESET  ;                                             00032800
              SI ~ LOC IFQUOTE ;   SI ~ SI +7  ;                        00032900
              TALLY ~ 0 ;                                               00033000
              IF SC = "1"                                               00033100
              THEN BEGIN DS ~ SET ;    % QUOTE                          00033200
                         IFQUOTE ~ TALLY                                00033300
                   END                                                  00033400
              ELSE BEGIN DS ~ RESET; % ALPHA                            00033500
                         SI ~ NEXTSI ;                                  00033600
                         IF SC = ALPHA                                  00033700
                         THEN BEGIN TALLY ~ 1 ;                         00033800
                                    IFMIDALFA ~ TALLY ;                 00033900
                              END                                       00034000
                         ELSE IFMIDALFA ~ TALLY ;                       00034100
                   END ;                                                00034200
              SI ~ LOC CHARCT ;  SI ~ SI +7 ;                           00034300
              DS ~ RESET  ;                                             00034400
              6 ( IF SB THEN DS ~ SET ELSE DS ~ RESET  ;  SKIP SB  ) ;  00034500
              DS ~ 3 RESET  ;                                           00034600
              SI ~ SAVESI ;                                             00034700
              DS ~ R1 CHR ;                                             00034800
              SI ~ LOC BLANKS  ;                                        00034900
              DS ~ R2 CHR  ;                                            00035000
              DS ~ 2 LIT "0"   ;                                        00035100
              SI ~ SAVESI ;                                             00035200
              SI ~ SI + R1  ;                                           00035300
              DS ~ R3 CHR ;                                             00035400
              SI ~ LOC BLANKS  ;                                        00035500
              DS ~ R4 CHR  ;                                            00035600
              NEXTDI ~ DI ;                                             00035700
              GO TO PHRASE ;                                            00035800
ARROW :       DI ~ NEXTDI ;                                             00035900
              DS ~ 7 LIT "0" ;                                          00036000
              DS ~    LIT "4" ;                                         00036100
              DS ~ 7 LIT "0" ;                                          00036200
              DS ~    LIT "~" ;                                         00036300
              END OF EDITNEW  PROCEDURE ;                               00036400
%***********************************************************************00036500
PROCEDURE     SPLICEARRAY (RAY1, RAY2, CHARCT1, CHARCT2 ) ;             00036600
VALUE         CHARCT1, CHARCT2 ;                                        00036700
ARRAY         RAY1 [*], RAY2 [*] ;                                      00036800
INTEGER       CHARCT1, CHARCT2 ;                                        00036900
BEGIN                                                                   00037000
COMMENT  GLOBAL VARIABLES  - NONE.                                      00037100
         RAY1 AND RAY2 ARE ARRAYS IN WORD MODE.                         00037200
         SPLICEARRAY JOINS THE TWO ARRAYS TOGETHER, TO FORM A SINGLE    00037300
         ARRAY IN WORD MODE. CHARCT1 AND CHARCT2 ARE THE NUMBER OF      00037400
         CHARACTERS IN EACH ARRAY WHICH ARE TO BE USED IN FORMING THE   00037500
         NEW ARRAY. THE NEW ARRAY IS LOCATED AT RAY1.  ;                00037600
INTEGER       WDS1, CHR1, FILL1, WDS2, CHR2, FINALCHR, FINALFILL ;      00037700
BOOLEAN       IFEXTRAWORD ;                                             00037800
STREAM  PROCEDURE  TAGALONG (STRING, CUP, STRINGWDS, STRINGCHR,         00037900
                             STRINGFILL, CUPWDS,         IFEXTRAWORD,   00038000
                             FINALCHR, FINALFILL ) ;                    00038100
VALUE         STRINGWDS, STRINGCHR, STRINGFILL, CUPWDS,                 00038200
              IFEXTRAWORD, FINALCHR, FINALFILL  ;                       00038300
BEGIN                                                                   00038400
COMMENT    TAGALONG ALLOWS AN ARRAY (CUP) TO TAGALONG BEHIND            00038500
           THE LEADING ARRAY (STRING) BY FILLING MISSING                00038600
           CHARACTERS INTO THE LAST WORD OF THE LEADING ARRAY           00038700
           WITH THE FIRST CHARACTERS OF THE TAGALONG ARRAY.             00038800
           ALL THE FOLLOWING TAGALONG CHARACTERS ARE POSITIONED         00038900
           TO FIT INTO THE WORD STRUCTURE OF THE LEADING ARRAY;         00039000
              DI ~ STRING  ;                                            00039100
    STRINGWDS(DI ~ DI +8 ) ;                                            00039200
              DI ~ DI +2 ;                                              00039300
              DI ~ DI + STRINGCHR ;                                     00039400
              SI ~ CUP ;                                                00039500
              SI ~ SI +2  ;                                             00039600
       CUPWDS(DS ~ STRINGFILL CHR ;                                     00039700
              DS ~ 2 LIT "0" ;                                          00039800
              DS ~ STRINGCHR CHR ;                                      00039900
                     SI ~ SI + 2    ) ;                                 00040000
  IFEXTRAWORD(DS ~ STRINGFILL CHR ;  DS ~ 2 LIT "0"  ) ;                00040100
              DS ~ FINALCHR CHR ;                                       00040200
    FINALFILL(DS ~ LIT " " ) ;                                          00040300
END OF TAGALONG PROCEDURE ;                                             00040400
              WDS1 ~ CHARCT1 DIV 6 ;                                    00040500
              WDS2 ~ CHARCT2 DIV 6 ;                                    00040600
              CHR1 ~ CHARCT1 MOD 6 ;                                    00040700
              CHR2 ~ CHARCT2 MOD 6 ;                                    00040800
              FILL1 ~ 6 - CHR1 ;                                        00040900
              IF CHR2 > FILL1                                           00041000
              THEN BEGIN IFEXTRAWORD ~ TRUE ;                           00041100
                         FINALCHR ~ CHR2 - FILL1 ;                      00041200
                         FINALFILL ~ 6 - FINALCHR ;                     00041300
                   END                                                  00041400
              ELSE BEGIN FINALCHR ~ CHR2 ;                              00041500
                         FINALFILL ~ FILL1 - CHR2 ;                     00041600
                   END ;                                                00041700
              IF FINALFILL = 6                                          00041800
              THEN BEGIN FINALFILL ~ 0  ;                               00041900
                         FINALCHR ~ 6   ;                               00042000
                         WDS2 ~ WDS2 - 1  ;                             00042100
                         IF WDS2 < 0 THEN WDS2 ~ FINALCHR ~ 0  ;        00042200
                   END   ;                                              00042300
              TAGALONG (RAY1, RAY2, WDS1, CHR1, FILL1, WDS2,            00042400
                        IFEXTRAWORD, FINALCHR, FINALFILL          ) ;   00042500
END OF SPLICEARRAY PROCEDURE ;                                          00042600
%********************************************************************** 00042700
INTEGER PROCEDURE SCAN (INFILE, QC,  TYPE, STRING ) ;                   00042800
VALUE         QC   ;                                                    00042900
BOOLEAN       QC   ;                                                    00043000
FILE          INFILE  ;                                                 00043100
INTEGER       TYPE  ;                                                   00043200
ARRAY         STRING [0]  ;                                             00043300
BEGIN                                                                   00043400
COMMENT       GLOBAL  -- EDITNEW AND SPLICEARRAY PROCEDURES.            00043500
           SCAN SUPPLIES THE NEXT ENTITY FROM A FILE "INFILE" TO        00043600
           THE USER.                                                    00043700
           WHEN A RECORD IS FINISHED, SCAN WILL PRODUCE AN ARROW.       00043800
           THE ENTITY IS PLACED IN THE ARRAY "STRING". THE ENTITY       00043900
           IS PRECISELY IDENTIFIED BY THE VALUES WHICH THE PROCEDURE    00044000
           ASSIGNS TO "SCAN" AND TO "TYPE".                             00044100
ENTITY                  VALUE OF SCAN             VALUE OF TYPE    ***  00044200
END OF FILE                 = -9                     UNDEFINED       *  00044300
INTEGER                     =  0                   = DIGIT COUNT     *  00044400
REAL NUMBER                 =  1                     UNDEFINED       *  00044500
ARROW                       =  2                     UNDEFINED       *  00044600
SPECIAL CHARACTER           =  3                    IDENTIFIES CHARACTER00044700
ALPHANUMERIC             10 | CHARACTER COUNT        = -1            *  00044800
QUOTE STRING             10 | CHARACTER COUNT        = -2          ***  00044900
           THE USER SPECIFIES HOW QUOTES ARE TO BE TREATED BY           00045000
           ASSIGNING A VALUE OF TRUE OR FALSE TO THE PARAMETER "QC".    00045100
           - TRUE : QUOTES ARE CHARACTERS                               00045200
           - FALSE: QUOTES ARE QUOTE STRING DELIMITERS                  00045300
           THE 4 FORMAL PARAMETERS ARE THE ONLY COMMUNICATION LINKS     00045400
           BETWEEN THE PROCEDURE AND THE OUTER PROGRAM.                 00045500
           THE OUTPUT "STRING" MUST BE DECLARED AS A 20 WORD            00045600
           ARRAY. IT WILL CONTAIN THE OUTPUT IN WORD MODE. NUMERIC      00045700
           AND SPECIAL CHARACTER ENTITIES ARE PLACED INTO STRING        00045800
           [0], RIGHT JUSTIFIED.                                        00045900
           ALPHANUMERICS AND QUOTE STRINGS START IN STRING [0], AND     00046000
           ARE LEFT JUSTIFIED IN THE RIGHTMOST 6 CHARACTERS.            00046100
           THEREFORE, YOU MUST ALWAYS COMPARE TO A 6 CHARACTER QUOTE    00046200
           WHEN TESTING OUTPUT FROM SCAN,                               00046300
           HOWEVER, IT IS NOW A SIMPLE MATTER TO TEST THE FIRST FEW     00046400
           SIGNIFICENT CHARACTERS OF AN INPUT WORD AND IGNORE THE       00046500
           REMAINDER.                                                   00046600
           ALPHANUMERICS ARE LIMITED TO 80 CHARACTERS ON ONE RECORD.    00046700
           QUOTE STRINGS ARE LIMITED TO 120 CHARACTERS,                 00046800
           ON TWO CARDS ONLY.  SUCCESSIVE                               00046900
           QUOTE STRINGS MUST BE SEPARATED BY AT LEAST ONE BLANK        00047000
           SPACE.                                                       00047100
           A QUOTE STRING MAY BE INTERRUPTED BY AN ARROW OR             00047200
ALLOWED TO RUN OFF THE CARD,  AND THEN                                  00047300
           CONTINUED ON THE FOLLOWING RECORD IF A QUOTE MARK IS         00047400
           PUT INTO THE FIRST POSITION OF THE SECOND RECORD.            00047500
           THE PARAMETERS INFILE AND QC MAY NOT BE CHANGED IN THE       00047600
           MIDDLE OF A RECORD. THEY MAY ONLY BE CHANGED WHEN THE        00047700
           LAST ENTITY PRODUCED WAS AN ARROW. OTHERWISE THEY ARE        00047800
           IGNORED. THE USER MAY ACCESS AN ENTIRE RECORD (ONLY          00047900
           AFTER GETTING AN ARROW) IF HE EXECUTES A "READ NO"           00048000
           STATEMENT BEFORE CALLING SCAN AGAIN. WHEN A QUOTE STRING     00048100
           RUNS OVER TO A 2ND CARD, THE 2ND RECORD IS NOT ACCESSABLE.   00048200
           THE FOLLOWING INPUT QUANTITIES WILL BE RECOGNIZED AND        00048300
    +9.732641@-02       +63214612501.3     +.32106910     +21           00048400
    -2.65@+13           -413.1234567       -.6            -3216382      00048500
     3.14159@+12         2.1                .62                         00048600
           CONVERTED INTO NUMBERS. THEY MAY BE SIGNED OR UNSIGNED,      00048700
           DECIMALS, WHOLE NUMBERS WITH DECIMALS, OR SCIENTIFIC         00048800
           NOTATION;                                                    00048900
DEFINE        THRU  =  STEP 1 UNTIL  # ;                                00049000
OWN BOOLEAN   MIDFILE ;                                                 00049100
SAVE OWN REAL ARRAY  CUP [0:165 ] ;  % ARRAY OF WORD COUPLETS           00049200
ARRAY                INARRAY [0:10] ;                                   00049300
OWN INTEGER   I   ;                                                     00049400
INTEGER          I1, J, K, L, CHARCT, CHRCT  ;                          00049500
LABEL         GETCUPS, NEXTCUP, TESTBLANK, EOP, EOF, EF, ARROW ;        00049600
IF NOT MIDFILE THEN                                                     00049700
BEGIN  MIDFILE ~ TRUE ;                                                 00049800
GETCUPS : % ~~~                                                         00049900
READ (INFILE, 10, INARRAY [*] )  [EOF]  ;                               00050000
EDITNEW (INARRAY [0], CUP [0], QC  )  ;                                 00050100
I ~  0 ;                                                                00050200
GO TO TESTBLANK  ;                                                      00050300
END OF GETCUPS   ;                                                      00050400
IF I = -2 THEN GO TO ARROW ; %QUOTE FAILED TO CONTINUE TO THIS RECORD   00050500
IF I = -1 THEN BEGIN I ~ 0 ;                                            00050600
                     GO TO TESTBLANK ;                                  00050700
               END ;                                                    00050800
IF CUP [I ] = 4 THEN GO TO GETCUPS ;  % LAST PHRASE WAS ~               00050900
NEXTCUP : % ~~~                                                         00051000
I ~ I + 2 ;                                                             00051100
TESTBLANK : % ~~~                                                       00051200
IF CUP [I ] = 2 THEN GO TO NEXTCUP ;   % SKIP BLANK COUPLETS            00051300
IF CUP [I] = 4 THEN                                                     00051400
ARROW :              BEGIN  IF I = -2 THEN I ~ -1 ;                     00051500
                     SCAN ~ 2 ;                                         00051600
                     STRING [0] ~ "~"  ;                                00051700
                     GO TO EOP  ;                                       00051800
                     END  ;                                             00051900
IF CUP [I ]>4 THEN                                                      00052000
BEGIN                                  %  ALPHANUMERIC PHRASES          00052100
WHILE CUP [I +J ] > 4 DO BEGIN                                          00052200
CHARCT ~ CHARCT + CUP [I +J ].[3:6]  ;                                  00052300
J ~ J + 2 ;                                                             00052400
                         END ;                                          00052500
FOR K ~ 0 THRU (J-1) DO BEGIN                                           00052600
STRING [K] ~ CUP [I]  ;                                                 00052700
STRING [K ].[3:6] ~ "0"  ;                                              00052800
I ~ I + 1 ;             END ;                                           00052900
I ~ I - 2 ;                                                             00053000
SCAN ~ 10 | CHARCT ;                                                    00053100
TYPE ~ -1  ;                                                            00053200
GO TO EOP  ;                                                            00053300
END OF ALPHANUMERICS  ;                                                 00053400
IF CUP [I ]<0 THEN                                                      00053500
BEGIN                                  %  QUOTE STRING PHRASES          00053600
WHILE CUP [I +J ] < 0 DO BEGIN                                          00053700
CHARCT ~ CHARCT + CUP [I +J ].[3:6]  ;                                  00053800
J ~ J + 2 ;                                                             00053900
                         END ;                                          00054000
FOR K ~ 0 THRU (J-1) DO BEGIN                                           00054100
STRING [K] ~ CUP [I]  ;                                                 00054200
STRING [K].[1:8] ~ "0"  ;                                               00054300
I ~ I + 1 ;             END ;                                           00054400
IF CUP [I ] = 4         %  QUOTE FOLLOWED BY AN ARROW                   00054500
THEN BEGIN READ (INFILE, 10, INARRAY [*] )  [EF ] ;                     00054600
           EDITNEW (INARRAY, CUP, QC ) ;                                00054700
           I ~ 0  ;                                                     00054800
           IF CUP [I ]  < 0   % QUOTE IS CONTINUED FROM PREVIOUS RECORD 00054900
           THEN BEGIN K ~ 0  ;                                          00055000
                      WHILE CUP [I +K ] < 0  DO BEGIN                   00055100
                      CHRCT ~ CHRCT + CUP [I+K ].[3:6] ;                00055200
                      K ~ K + 2 ;               END ;                   00055300
                      IF CHARCT + CHRCT > 120                           00055400
                      THEN CHRCT ~ 120 - CHARCT ;                       00055500
                      SPLICEARRAY (STRING, CUP, CHARCT, CHRCT ) ;       00055600
                      CHARCT ~ CHARCT + CHRCT ;                         00055700
                      I ~ I + K ;                                       00055800
                END ;                                                   00055900
EF:  END ;                                                              00056000
I ~ I - 2 ;                                                             00056100
SCAN ~ 10 | CHARCT ;                                                    00056200
TYPE ~ -2  ;                                                            00056300
GO TO EOP  ;                                                            00056400
END OF QUOTE STRINGS  ;                                                 00056500
COMMENT  ONLY NUMBERS & SPECIAL CHARACTERS REMAIN TO BE TESTED FOR      00056600
         AT THIS POINT. NUMBERS WILL BE TESTED FIRST. IF A GROUP OF     00056700
         PHRASE COUPLETS CONSTITUTES A NUMBER, IT WILL BE CONVERTED TO  00056800
         A NUMBER AND PLACED IN THE FIRST WORD OF THE OUTPUT STRING.    00056900
         VARIABLES J,K,&L ARE USED TO KEEP TRACK OF POSITION IN THE     00057000
         COUPLET ARRAY --                                               00057100
                         J  CORRESPONDS TO  SIGN OF THE NUMBER          00057200
                         K      "       "   WHOLE NUMBER PART           00057300
                         L      "       "   DECIMAL PART  ;             00057400
STRING [0] ~  "0"  ;                                                    00057500
IF CUP [I] = 3 AND ( CUP [I+1] = "+" OR CUP [I+1] = "-" )               00057600
THEN J ~ 2 ELSE J ~ 0  ;                                                00057700
IF CUP [I+J ] = 0  % INTEGER                                            00057800
    THEN BEGIN STRING [0]  ~ CUP [I+J+1 ] ;                             00057900
               K ~ 2 ;                                                  00058000
               TYPE ~ CUP [I +J].[3:6] ;   % PUT DIGIT COUNT INTO TYPE  00058100
               IF CUP [I+J ].[3:6] > 8      %  SPLIT INTEGER            00058200
               THEN BEGIN STRING [0]~ STRING [0] | 10*8 + CUP [I+J+3 ]; 00058300
                          K ~ K +2                                      00058400
                      END ;                                             00058500
           END                                                          00058600
    ELSE K ~ 0  ;                                                       00058700
I1 ~ I +J +K ;                                                          00058800
IF CUP[I1] = 3 AND   CUP[I1+1] = "."  % DECIMAL                         00058900
    THEN IF CUP [I1 +2 ] = 0      % FOLLOWED BY INTEGER                 00059000
         THEN IF CUP [I1 +2 ] .[3:6 ]  { 8   % 8 DIGITS OR LESS         00059100
              THEN BEGIN L ~ 4 ;                                        00059200
                   STRING [0] ~ STRING [0] + CUP [I1 +3 ]               00059300
                                             / (10*CUP [I1+2].[3:6] )   00059400
                   END                                                  00059500
              ELSE BEGIN L ~ 6 ;   % SPLIT INTEGER                      00059600
                   STRING [0] ~ STRING [0] + CUP [I1 +3 ]               00059700
                                             /(10*(CUP[I1+2].[3:6]-8))  00059800
                                            + CUP [I1 +5 ]              00059900
                                             /(10*CUP[I1+2] .[3:6]) ;   00060000
                   END                                                  00060100
         ELSE L ~ 0                                                     00060200
    ELSE      L ~ 0  ;                                                  00060300
I1 ~ I1 + L ;                                                           00060400
IF K > 0 AND L > 0                                                      00060500
AND CUP [I+J ].[3:6 ] = 1                                               00060600
AND CUP [I1   ] = 3                                                     00060700
AND CUP [I1 +1 ] = "00000@"                                             00060800
AND CUP [I1+2 ] = 3                                                     00060900
AND(CUP [I1 +3 ] = "00000+" OR                                          00061000
    CUP [I1 +3 ] = "00000-" OR                                          00061100
    CUP [I1 +3 ] = "00000&" )                                           00061200
AND CUP [I1 +4 ] =  0                                                   00061300
AND CUP [I1 +4 ].[3:6 ] = 2                                             00061400
THEN BEGIN   % CONVERT SCIENTIFIC NOTATION TO A REAL NUMBER             00061500
           IF CUP [I1 +3 ] = "00000-"   % NEGATIVE EXPONENT             00061600
           THEN CUP [I1 +5 ] ~ -CUP [I1 +5 ] ;                          00061700
           STRING [0] ~ STRING [0] | 10*CUP [I1 +5 ] ;                  00061800
           I1 ~ I1 +6 ;   % POINT TO NEXT PHRASE                        00061900
      END ;                                                             00062000
IF K > 0 OR L > 0   % INTEGER OR REAL NUMBER HAS BEEN PUT INTO STRING   00062100
THEN BEGIN IF CUP [I ] = 3 AND CUP [I+1 ] = "-"                         00062200
           THEN STRING [0] ~ -STRING [0]  ;                             00062300
            IF L > 0 THEN SCAN ~ 1 ELSE SCAN ~ 0 ;                      00062400
           I ~ I1 -2 ;                                                  00062500
           GO TO EOP ;                                                  00062600
      END OF NUMBER CONVERSION AND TESTING ;                            00062700
SCAN ~ 3  ;        % SPECIAL CHARACTER PHRASE                           00062800
J ~ STRING [0] ~ CUP [I +1 ]  ;                                         00062900
IF      J     = "00000."   THEN       TYPE ~ 1   ELSE                   00063000
IF      J     = "00000["   THEN       TYPE ~ 2   ELSE                   00063100
IF      J     = "00000("   THEN       TYPE ~ 3   ELSE                   00063200
IF      J     = "00000<"   THEN       TYPE ~ 4   ELSE                   00063300
IF      J     = "00000!"   THEN       TYPE ~ 5   ELSE                   00063400
IF      J     = "00000&"   THEN       TYPE ~ 6   ELSE                   00063500
IF      J     = "00000$"   THEN       TYPE ~ 7   ELSE                   00063600
IF      J     = "00000*"   THEN       TYPE ~ 8   ELSE                   00063700
IF      J     = "00000)"   THEN       TYPE ~ 9   ELSE                   00063800
IF      J     = "00000;"   THEN       TYPE ~ 10  ELSE                   00063900
IF      J     = "00000{"   THEN       TYPE ~ 11  ELSE                   00064000
IF      J     = "00000-"   THEN       TYPE ~ 12  ELSE                   00064100
IF      J     = "00000/"   THEN       TYPE ~ 13  ELSE                   00064200
IF      J     = "00000,"   THEN       TYPE ~ 14  ELSE                   00064300
IF      J     = "00000%"   THEN       TYPE ~ 15  ELSE                   00064400
IF      J     = "00000="   THEN       TYPE ~ 16  ELSE                   00064500
IF      J     = "00000]"   THEN       TYPE ~ 17  ELSE                   00064600
IF      J     =      """   THEN       TYPE ~ 18  ELSE                   00064700
IF      J     = "00000#"   THEN       TYPE ~ 19  ELSE                   00064800
IF      J     = "00000@"   THEN       TYPE ~ 20  ELSE                   00064900
IF      J     = "00000:"   THEN       TYPE ~ 21  ELSE                   00065000
IF      J     = "00000>"   THEN       TYPE ~ 22  ELSE                   00065100
IF      J     = "00000}"   THEN       TYPE ~ 23  ELSE                   00065200
IF      J     = "00000+"   THEN       TYPE ~ 24  ELSE                   00065300
IF      J     = "00000|"   THEN       TYPE ~ 25  ; GO TO EOP      ;     00065400
EOF :   SCAN ~ -9  ;                                                    00065500
EOP :    END OF PROCEDURE SCAN  ;                                       00065600
%********************************************************************** 00065700
%-----------------------------------------------------------------------00065800
BOOLEAN STREAM PROCEDURE NAMESAME (READRAY, FILEFIELD );                00065900
BEGIN                                                                   00066000
         SI ~ READRAY ;                                                 00066100
         DI ~ FILEFIELD ;                                               00066200
         IF 24 SC = DC THEN       TALLY ~ 1 ELSE  TALLY ~ 0 ;           00066300
         NAMESAME ~ TALLY ;                                             00066400
END OF NAMESAME PROCEDURE ;                                             00066500
%-----------------------------------------------------------------------00066600
BOOLEAN STREAM PROCEDURE SAMENAME (READRAY, FILEFIELD );                00066700
BEGIN                                                                   00066800
         SI ~ READRAY ;                                                 00066900
         DI ~ FILEFIELD ;    DI ~ DI + 8 ;                              00067000
         IF 16 SC = DC THEN       TALLY ~ 1 ELSE  TALLY ~ 0 ;           00067100
         SAMENAME ~ TALLY ;                                             00067200
END OF SAMENAME PROCEDURE ;                                             00067300
%-----------------------------------------------------------------------00067400
STREAM PROCEDURE CREATEFILEFIELD (STRING, FILEFIELD );                  00067500
BEGIN                                                                   00067600
         SI ~ STRING ;                                                  00067700
         DI ~ FILEFIELD ;                                               00067800
         DI ~ DI + 8 ;                                                  00067900
      2( SI ~ SI + 2 ;                                                  00068000
         DS ~ 6 CHR ) ;                                                 00068100
         SI ~ SI +2 ;                                                   00068200
         DS ~ 4 CHR ;                                                   00068300
END OF CREATEFILEFIELD ;                                                00068400
%-----------------------------------------------------------------------00068500
STREAM PROCEDURE MOVECHAR (STRING, QUOTARRAY, A,B,C, CHARCT ) ;         00068600
VALUE         A,B,C, CHARCT ;                                           00068700
BEGIN                                                                   00068800
              SI ~ STRING ;                                             00068900
              SI ~ SI +2  ;                                             00069000
              DI ~ QUOTARRAY ;                                          00069100
              A(2(DI ~ DI +50)) ;                                       00069200
                B(DI ~ DI +10 ) ;                                       00069300
                  DI ~ DI + C   ;                                       00069400
              DS ~ CHARCT CHR ;                                         00069500
END MOVECHAR PROCEDURE ;                                                00069600
%-----------------------------------------------------------------------00069700
PROCEDURE     TURNONPLUGCASEBOOLEAN ;                                   00069800
BEGIN                                                                   00069900
INTEGER       I, J  ;                                                   00070000
FORMAT        F ("ERROR *** TYPE 1 FIELD NOT IN PLUGDICT ARRAY.") ;     00070100
LABEL         EOP   ;                                                   00070200
         FOR I ~ 0 THRU PLUGMAX DO BEGIN                                00070300
         IF NAMESAME (FILEFIELD, PLUGDICT [I,*] )                       00070400
         THEN BEGIN                                                     00070500
              FOR J ~ 3 THRU 6 DO                                       00070600
              ISDEVELOPE [PLUGDICT [I,J] ] ~ TRUE ;                     00070700
              GO TO EOP                                                 00070800
              END                                                       00070900
                                   END ;                                00071000
         NUMERR ~ NUMERR + 1 ;                                          00071100
         WRITE (PTR, 3, FILEFIELD [*] ) ;                               00071200
         WRITE (PTR [DBL], F ) ;                                        00071300
EOP : END OF TURNONPLUGCASEBOOLEAN PROCEDURE ;                          00071400
%-----------------------------------------------------------------------00071500
STREAM PROCEDURE FILLFIELDKEY (KEYCUP, G,H,I,J,K,L,M,N,O,P ) ;          00071600
VALUE                                  G,H,I,J,K,L,M,N,O,P   ;          00071700
BEGIN         DI ~ KEYCUP ;                                             00071800
              DS ~ 6 LIT "0" ;                                          00071900
              SI ~ LOC G ;                                              00072000
         10 ( SI ~ SI +7;                                               00072100
              DS ~ CHR ) ;                                              00072200
END FILLFIELDKEY PROCEDURE ;                                            00072300
%-----------------------------------------------------------------------00072400
ALPHA STREAM PROCEDURE  UNPACK (PACKEDNUM ) ;                           00072500
VALUE         PACKEDNUM ;                                               00072600
BEGIN                                                                   00072700
              SI ~ LOC PACKEDNUM ;                                      00072800
              DI ~ LOC UNPACK ;                                         00072900
              DS ~ 8 DEC  ;                                             00073000
END OF UNPACK PROCEDURE ;                                               00073100
%-----------------------------------------------------------------------00073200
FORMAT   %  ERROR MESSAGES                                              00073300
    E1  ("***ERROR1***  #FIELD NAME ON A BREAK CARD MUST BE FOLLOWED BY 00073400
A QUOTE WHICH NAMES THE FIELD" ),                                       00073500
    E2  ("***ERROR2***  #FIELD MUST BE A # SIGN FOLLOWED BY A 6 DIGIT NU00073600
MBER"/                                                                  00073700
"FIRST 2 DIGITS ARE FIELD LENGTH"/                                      00073800
"NEXT 3 DIGITS ARE STARTING POSITION"/                                  00073900
"FINAL DIGIT IS EITHER 0 FOR ALPHA OR 1 FOR NUMERIC FIELD" ),           00074000
    E3  ("***ERROR3***  THIS FIELD IS NOT ON THE DICTION FILE" ),       00074100
    E4  ("***ERROR4***  LEAD CARD MUST BE FIRST CARD" ),                00074200
    E5  ("***ERROR5***  LEAD  MUST BE FOLLOWED BY A FILE NAME WHICH DOES00074300
 NOT EXCEED 7 CHARACTERS" ),                                            00074400
    E6  ("***ERROR6***  LEAD CARD FILE NAME MUST BE FOLLOWED BY NUMBER O00074500
F WORDS PER RECORD THEN NUMBER WORDS PER BLOCK" ),                      00074600
    E7  ("***ERROR7***  PERAMETER CARD MUST BEGIN WITH PERAMETER TYPE-  00074700
     PAGELINES,SKIPHEAD,DATE,INDENT,TOPTITLE,BOTTOMTITLE"/ X67,         00074800
"BREAK,HEADINGS,LINE, OR TOTAL" ),                                      00074900
    E8  ("***ERROR8***  PAGELINES  MUST BE FOLLOWED BY AN INTEGER" ),   00075000
    E9  ("***ERROR9***  SKIPHEADS  MUST BE FOLLOWED BY AN INTEGER" ),   00075100
    E10 ("***ERROR10***  INDENT    MUST BE FOLLOWED BY AN INTEGER" ),   00075200
    E11 ("***ERROR11***  WORDS PER RECORD PRECEDES WORDS PER BLOCK. WDS/00075300
BLOCK IS AN EVEN MULTIPLE OF WORDS/RECORD" ),                           00075400
    E12 ("***ERROR12***  DATE  MUST BE FOLLOWED BY A QUOTE WHICH DOES NO00075500
T EXCEED 18 CHARACTERS" ),                                              00075600
    E13 ("***ERROR13***  THIS QUOTE MUST NOT EXCEED 66 CHARACTERS" ),   00075700
    E14 ("***ERROR14***  THE NUMBER OF SPACES PLUS THE NUMBER OF QUOTE C00075800
HARACTERS MUST NOT EXCEED 66" ),                                        00075900
    E15 ("***ERROR15***  A TITLE MUST BE IN QUOTES" ),                  00076000
    E16 ("***ERROR16***  NUMBER OF SPACES PLUS CHARACTERS MUST NOT EXCEE00076100
D 120 " ),                                                              00076200
    E17 ("***ERROR17***  FIELD NAME MUST BE 16 ALPHANUMERIC CHARACTERS  00076300
(INTERNAL HYPHEN ACCEPTABLE)" ),                                        00076400
    E18 ("***ERROR18***  TOTAL FIELD (TYPE 2) MAY NOT APPEAR ON A LINE C00076500
ARD OR BREAK CARD" ),                                                   00076600
    E19 ("***ERROR19***  THIS QUOTE MAY NOT EXCEED 6 CHARACTERS. LONGER 00076700
QUOTES MAY BE BUILT UP ON SUCCESSIVE CARDS WITH ZERO SPACEING" ),       00076800
    E20 ("***ERROR20***  INVALID FIELD NAME" ),                         00076900
    E21 ("***ERROR21*** ONLY TOTAL CARDS MAY FOLLOW A TOTAL CARD" ),    00077000
    E22 ("***ERROR22***  NUMBER OF DECIMALS MAY NOT EXCEED NUMBER OF DIG00077100
ITS" ),                                                                 00077200
    E23 ("***ERROR23***  ALPHA FIELDS MAY NOT BE TOTALED" ),            00077300
    E24 ("***ERROR24***  DICTION CONTAINS INVALID TYPE" ),              00077400
    E25 ("***ERROR25***  DEVELOPED TOTAL FIELD (TYPE 2) HAS NOT BEEN ENT00077500
ERED IN CALCDICT ARRAY" ),                                              00077600
    E26 ("***ERROR26***  IMPLYDEVELOPES ARRAY CONTAINS A FIELD WHICH IS 00077700
NOT ON THE DICTION FILE" ),                                             00077800
    E27 ("***ERROR27***  THE MAXIMUM NUMBER OF PRINT POSITIONS PER LINE 00077900
IS 120 (INCLUDE SPACES & DECIMAL POINTS)."/                             00078000
"YOUR LINE CARDS REQUIRE ", I3, " POSITIONS" ),                         00078100
    E28 ("***ERROR28***  THE MAXIMUM NUMBER OF PRINT POSITIONS PER LINE 00078200
IS 120 (INCLUDE SPACES & DECIMAL POINTS)."/                             00078300
"YOUR TOTAL CARDS REQUIRE ", I3, " POSITIONS" ),                        00078400
    E29 ("***ERROR29  IMPLYDEVELOPES ARRAY MUST NOT CONTAIN TYPE 2 FIELD00078500
S, USE CALCDICT ARRAY."  ),                                             00078600
    E30 ("***ERROR30***  TOTALS ARE LIMITED TO 12 DIGITS" ) ;           00078700
DEFINE                                                                  00078800
    ERROR1  = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00078900
              WRITE (PTR [DBL], E1    ) ; NUMERR ~ NUMERR + 1 ;         00079000
              GO TO ENDCARD  END # ,                                    00079100
    ERROR2  = BEGIN WRITE (PTR,F7, STRING [0]             ) ;           00079200
              WRITE (PTR [DBL], E2    ) ; NUMERR ~ NUMERR + 1 ;         00079300
              GO TO ENDCARD  END # ,                                    00079400
    ERROR3  = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00079500
              WRITE (PTR [DBL], E3    ) ; NUMERR ~ NUMERR + 1 ;         00079600
              GO TO ENDCARD  END # ,                                    00079700
    ERROR4  = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00079800
              WRITE (PTR [DBL], E4    ) ; NUMERR ~ NUMERR + 1 ;         00079900
              GO TO ENDCARD  END # ,                                    00080000
    ERROR5  = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00080100
              WRITE (PTR [DBL], E5    ) ; NUMERR ~ NUMERR + 1 ;         00080200
              GO TO ENDCARD  END # ,                                    00080300
    ERROR6  = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00080400
              WRITE (PTR [DBL], E6    ) ; NUMERR ~ NUMERR + 1 ;         00080500
              GO TO ENDCARD  END # ,                                    00080600
    ERROR7  = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00080700
              WRITE (PTR [DBL], E7    ) ; NUMERR ~ NUMERR + 1 ;         00080800
              GO TO ENDCARD  END # ,                                    00080900
    ERROR8  = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00081000
              WRITE (PTR [DBL], E8    ) ; NUMERR ~ NUMERR + 1 ;         00081100
              GO TO ENDCARD  END # ,                                    00081200
    ERROR9  = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00081300
              WRITE (PTR [DBL], E9    ) ; NUMERR ~ NUMERR + 1 ;         00081400
              GO TO ENDCARD  END # ,                                    00081500
    ERROR10 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00081600
              WRITE (PTR [DBL], E10   ) ; NUMERR ~ NUMERR + 1 ;         00081700
              GO TO ENDCARD  END # ,                                    00081800
    ERROR11 = BEGIN WRITE (PTR,F7, STRING [0]);                         00081900
              WRITE (PTR [DBL], E11   ) ; NUMERR ~ NUMERR + 1 ;         00082000
              GO TO ENDCARD  END # ,                                    00082100
    ERROR12 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00082200
              WRITE (PTR [DBL], E12   ) ; NUMERR ~ NUMERR + 1 ;         00082300
              GO TO ENDCARD  END # ,                                    00082400
    ERROR13 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00082500
              WRITE (PTR [DBL], E13   ) ; NUMERR ~ NUMERR + 1 ;         00082600
              GO TO ENDCARD  END # ,                                    00082700
    ERROR14 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00082800
              WRITE (PTR [DBL], E14   ) ; NUMERR ~ NUMERR + 1 ;         00082900
              GO TO ENDCARD  END # ,                                    00083000
    ERROR15 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00083100
              WRITE (PTR [DBL], E15   ) ; NUMERR ~ NUMERR + 1 ;         00083200
              GO TO ENDCARD  END # ,                                    00083300
    ERROR16 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00083400
              WRITE (PTR [DBL], E16   ) ; NUMERR ~ NUMERR + 1 ;         00083500
              GO TO ENDCARD  END # ,                                    00083600
    ERROR17 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00083700
              WRITE (PTR [DBL], E17   ) ; NUMERR ~ NUMERR + 1 ;         00083800
              GO TO ENDCARD  END # ,                                    00083900
    ERROR18 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00084000
              WRITE (PTR [DBL], E18   ) ; NUMERR ~ NUMERR + 1 ;         00084100
              GO TO ENDCARD  END # ,                                    00084200
    ERROR19 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00084300
              WRITE (PTR [DBL], E19   ) ; NUMERR ~ NUMERR + 1 ;         00084400
              GO TO ENDCARD  END # ,                                    00084500
    ERROR20 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00084600
              WRITE (PTR [DBL], E20   ) ; NUMERR ~ NUMERR + 1 ;         00084700
              GO TO ENDCARD  END # ,                                    00084800
    ERROR21 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00084900
              WRITE (PTR [DBL], E21   ) ; NUMERR ~ NUMERR + 1 ;         00085000
              GO TO ENDCARD  END # ,                                    00085100
    ERROR22 = BEGIN WRITE (PTR,F7, STRING [0]);                         00085200
              WRITE (PTR [DBL], E22   ) ; NUMERR ~ NUMERR + 1 ;         00085300
              GO TO ENDCARD  END # ,                                    00085400
    ERROR23 = BEGIN WRITE (PTR, F7, IT ) ;                              00085500
              WRITE (PTR [DBL], E23   ) ; NUMERR ~ NUMERR + 1 ;         00085600
              GO TO ENDCARD  END # ,                                    00085700
    ERROR24 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00085800
              WRITE (PTR [DBL], E24   ) ; NUMERR ~ NUMERR + 1 ;         00085900
              GO TO ENDCARD  END # ,                                    00086000
    ERROR25 = BEGIN WRITE (PTR, F, STRING [0], STRING [1] ) ;           00086100
              WRITE (PTR [DBL], E25   ) ; NUMERR ~ NUMERR + 1 ;         00086200
              GO TO ENDCARD  END # ,                                    00086300
    ERROR26 = BEGIN READ (IMPLYDEVELOPES [J,*], F5, FOR B ~ 0 THRU 3    00086400
                                                    DO STRING [B]  ) ;  00086500
              WRITE (PTR, F6, FOR B ~ 0 THRU 3 DO STRING [B] ) ;        00086600
              WRITE (PTR [DBL], E26   ) ; NUMERR ~ NUMERR + 1 ;         00086700
              END # ,                                                   00086800
    ERROR27 = BEGIN                                                     00086900
              WRITE (PTR [DBL], E27, LINECHR ); NUMERR ~ NUMERR + 1 ;   00087000
              END # ,                                                   00087100
    ERROR28 = BEGIN                                                     00087200
              WRITE (PTR [DBL], E28, TOTLCHR ); NUMERR ~ NUMERR + 1 ;   00087300
              END # ,                                                   00087400
    ERROR29 = BEGIN READ (IMPLYDEVELOPES [J,*], F5, FOR B ~ 0 THRU 3    00087500
                                                    DO STRING [B]  ) ;  00087600
              WRITE (PTR, F6, FOR B ~ 0 THRU 3 DO STRING [B] ) ;        00087700
              WRITE (PTR [DBL], E29   ) ; NUMERR ~ NUMERR + 1 ;         00087800
              END # ,                                                   00087900
    ERROR30 = BEGIN                                                     00088000
              WRITE (PTR [DBL], E30   ) ; NUMERR ~ NUMERR + 1 ;         00088100
              GO TO ENDCARD  END # ;                                    00088200
%-----------------------------------------------------------------------00088300
COMMENT      "SET PLUG PROCEDURE"                                       00088400
PURPOSE - TO INFORM THE PRE-EDIT BLOCK OF THE CASE NUMBER(S) OF THE PLUG00088500
IN PROCEDURES WHICH MUST BE EXECUTED TO DEVELOPE A GIVEN FIELD ON A LINE00088600
CARD. FOUR CASE NUMBERS MAY BE SUPPLIED.                                00088700
THE DEFINE OF PLUGMAX SHOULD BE INCREASED TO THE VALUE OF THE HIGHEST   00088800
PLUGIN NUMBER.                                                          00088900
PLUGDICT ROW NUMBER = PLUG IN NUMBER .                                  00089000
FILL PLUGDICT [0,*] WITH "0FILEXX", "FIELDXXX", "NAMEXXXX", 0, 0, 0, 0 ;00089100
%-----------------------------------------------------------------------00089200
COMMENT      "SET CALC PROCEDURE"                                       00089300
PURPOSE - TO INFORM THE PRE-EDIT BLOCK OF THE CASE NUMBER(S) OF THE CALC00089400
PROCEDURES WHICH MUST BE EXECUTED TO CALCULATE A GIVEN FIELD ON A TOTAL 00089500
CARD (TYPE 2).                                                          00089600
FOUR CASE NUMBERS MAY BE SUPPLIED.                                      00089700
THE DEFINE OF CALCMAX MUST INCREASED TO THE HIGHEST CASE NUMBER.        00089800
CALCDICT ROW NUMBER = CALC PROCEDURE NUMBER.                            00089900
FILL CALCDICT [0,*] WITH "0FILEXX", "FIELDXXX", "NAMEXXXX", 0, 0, 0, 0 .00090000
THE IMPLYDEVELOPE ARRAY MUST BE UPDATED IF A TYPE 2 TOTAL FIELD CALCUL- 00090100
ATES FROM TOTALS OF TYPE 1 (DEVELOPED) FIELDS, OR TYPE 0 FIELDS.        00090200
THE DEFINE OF IMPDEVMAX SHOULD BE INCREASED TO ALLOW FOR ADDITIONS TO   00090300
THE IMPLYDEVELOPES ARRAY.                                               00090400
FILL IMPLYDEVELOPES [0,*] WITH   0, "16CHARFI", "ELDNAME " ;            00090500
%-----------------------------------------------------------------------00090600
BEGIN %NESTED LEAD BLOCK                                                00090700
LABEL         SCANFILE, ENDCARD, EDITOPTITLE, EDITBOMTITLE, HITRECORD,  00090800
              GOTIT, EDITNUM, SKIPDICT, ERR3 ;                          00090900
    READ (CARDSIN [NO], 10, READRAY [*] ) ;                             00091000
    WRITE (PTR, 10,         READRAY [*] ) ;                             00091100
    S ~ SCANIT ;                                                        00091200
    IF IT = "LEAD  " AND S = 40 AND TYPE = -1         % LEAD CARD       00091300
    THEN BEGIN S ~ SCANIT ;                                             00091400
               IF IT = "DUMMY " THEN GO TO SKIPDICT ;                   00091500
               IF S { 70 AND TYPE = -1 AND S } 10                       00091600
               THEN BEGIN                                               00091700
                    FILEFIELD [0].[6:36] ~ STRING [0].[12:36] ;         00091800
                    FILEFIELD [0].[42:6] ~ STRING [1].[12:6 ] ;         00091900
                    FILL DICTION WITH FILEFIELD [0], * ;                00092000
                    END                                                 00092100
               ELSE ERROR5 ;                                            00092200
SKIPDICT :     S ~ SCANIT ;                                             00092300
    IF S = 0 THEN NUMWDSPEREC ~ IT   ELSE ERROR6  ;                     00092400
    S ~ SCANIT ;                                                        00092500
    IF S = 0 THEN NUMWDSPERBLK ~ IT  ELSE ERROR6  ;                     00092600
    IF NUMWDSPERBLK MOD NUMWDSPEREC ! 0 THEN  ERROR11 ;                 00092700
         END LEAD CARD                                                  00092800
    ELSE ERROR4 ;                                                       00092900
ENDCARD :                                                               00093000
    WHILE TRUE DO                                                       00093100
    IF S = 2                                                            00093200
    THEN GO TO SCANFILE                                                 00093300
    ELSE S ~ SCANIT  ;                                                  00093400
SCANFILE :                                                              00093500
    READ  (CARDSIN [NO], 10, READRAY [*] )[EOF] ;                       00093600
    WRITE (   PTR  ,     10, READRAY [*] ) ;                            00093700
    S ~ SCANIT ;   % START NEW PERAMETER CARD                           00093800
    IF TYPE ! -1 THEN  ERROR7 ;                                         00093900
    IF IT = "PAGELI"                 %  PAGELINES CARD                  00094000
    THEN BEGIN S ~ SCANIT ;                                             00094100
               IF S = 0                                                 00094200
               THEN BEGIN LINEMAX ~ IT ;                                00094300
                          GO TO ENDCARD ;                               00094400
                    END                                                 00094500
               ELSE ERROR8 ;                                            00094600
         END PAGELINES CARD ;                                           00094700
    IF IT = "SKIPHE"                 %  SKIPHEADS CARD                  00094800
    THEN BEGIN S ~ SCANIT ;                                             00094900
               IF S = 0                                                 00095000
               THEN BEGIN NUMHDREC ~ IT ;                               00095100
                          GO TO ENDCARD ;                               00095200
                    END                                                 00095300
               ELSE ERROR9 ;                                            00095400
         END SKIPHEADS CARD ;                                           00095500
    IF IT = "INDENT"                 %  INDENT CARD                     00095600
    THEN BEGIN S ~ SCANIT ;                                             00095700
               IF S = 0                                                 00095800
               THEN BEGIN INDENT ~ IT/2 ;                               00095900
                          GO TO ENDCARD ;                               00096000
                    END                                                 00096100
               ELSE ERROR10  ;                                          00096200
         END INDENT CARD ;                                              00096300
    IF IT = "DATE  "                 %  DATE CARD                       00096400
    THEN BEGIN S ~ SCANIT ;                                             00096500
               IF TYPE ! -2 OR S < 10 OR S > 180 THEN ERROR12           00096600
               ELSE BEGIN FOR I ~ 0 THRU 2 DO                           00096700
                          DATERAY [I] ~ STRING [I] ;                    00096800
                          GO TO ENDCARD ;                               00096900
                    END ;                                               00097000
         END DATE CARD ;                                                00097100
    IF IT = "TOPTIT"                 % TOPTITLES CARD                   00097200
    THEN BEGIN NUMTOPTITLES ~ NUMTOPTITLES + 1 ;                        00097300
               S ~ SCANIT ;                                             00097400
               IF TYPE = -2   % QUOTE                                   00097500
               THEN BEGIN IF S > 660  THEN ERROR13 ;                    00097600
                          CHARCT ~ S/10 ;                               00097700
                          SKIPCHR ~ (66 - CHARCT) DIV 2 ;               00097800
                          GO TO EDITOPTITLE ;                           00097900
                    END                                                 00098000
               ELSE IF S = 0   % INTEGER                                00098100
                    THEN BEGIN SKIPCHR ~ IT ;                           00098200
                               S ~ SCANIT   ;                           00098300
                               IF TYPE ! -2 OR S < 10 THEN ERROR15 ;    00098400
                               CHARCT ~ S/10 ;                          00098500
                               IF CHARCT + SKIPCHR > 66 THEN ERROR14 ;  00098600
EDITOPTITLE :                  FOR I ~ 0 THRU 10 DO                     00098700
                               TOPTITLERAY8 [NUMTOPTITLES,I] ~ "      ";00098800
                               SPLICEARRAY(TOPTITLERAY8[NUMTOPTITLES,*],00098900
                                           STRING, SKIPCHR, CHARCT ) ;  00099000
                               WRITE (PTR, T, FOR I ~ 0 THRU 10 DO      00099100
                                      TOPTITLERAY8 [NUMTOPTITLES,I] ) ; 00099200
                               GO TO ENDCARD ;                          00099300
                         END                                            00099400
                    ELSE ERROR15 ;                                      00099500
         END TOPTITLES CARD ;                                           00099600
    IF IT = "BOTTOM"                 % BOTTOMTITLES CARD                00099700
    THEN BEGIN NUMBOMTITLES ~ NUMBOMTITLES + 1 ;                        00099800
               S ~ SCANIT ;                                             00099900
               IF TYPE = -2   % QUOTE                                   00100000
               THEN BEGIN IF S > 660  THEN ERROR13 ;                    00100100
                          CHARCT ~ S/10 ;                               00100200
                          SKIPCHR ~ (66 - CHARCT) DIV 2 ;               00100300
                          GO TO EDITBOMTITLE  ;                         00100400
                    END                                                 00100500
               ELSE IF S = 0   % INTEGER                                00100600
                    THEN BEGIN SKIPCHR ~ IT ;                           00100700
                               S ~ SCANIT   ;                           00100800
                               IF TYPE ! -2 OR S < 10 THEN ERROR15 ;    00100900
                               CHARCT ~ S/10 ;                          00101000
                               IF CHARCT + SKIPCHR > 66 THEN ERROR14 ;  00101100
EDITBOMTITLE :                 FOR I ~ 0 THRU 10 DO                     00101200
                               BOMTITLERAY8 [NUMBOMTITLES,I] ~ "      ";00101300
                               SPLICEARRAY(BOMTITLERAY8[NUMBOMTITLES,*],00101400
                                           STRING, SKIPCHR, CHARCT ) ;  00101500
                               WRITE (PTR, T, FOR I ~ 0 THRU 10 DO      00101600
                                      BOMTITLERAY8 [NUMBOMTITLES,I] ) ; 00101700
                               GO TO ENDCARD ;                          00101800
                         END                                            00101900
                    ELSE ERROR15 ;                                      00102000
         END BOMTITLES CARD ;                                           00102100
    IF IT = "HEADIN"                 % HEADINGS  CARD                   00102200
    THEN BEGIN NUMHEDLINES ~ NUMHEDLINES + 1 ;                          00102300
               S ~ SCANIT ;                                             00102400
               IF S = 0  % INTEGER                                      00102500
               THEN BEGIN SKIPCHR ~ IT ;                                00102600
                          S ~ SCANIT ;                                  00102700
                    END                                                 00102800
               ELSE SKIPCHR ~ 0 ;                                       00102900
               IF TYPE ! -2 OR S < 10  THEN ERROR15 ;                   00103000
               CHARCT ~ S/10 ;                                          00103100
               IF CHARCT + SKIPCHR > 120  THEN ERROR16  ;               00103200
               FOR I ~ 0 THRU 19 DO                                     00103300
               COLUMNHEADS8[NUMHEDLINES,I]  ~ "      "  ;               00103400
               SPLICEARRAY (COLUMNHEADS8 [NUMHEDLINES,*], STRING,       00103500
                                          SKIPCHR    ,  CHARCT  ) ;     00103600
               WRITE (PTR [DBL], H, FOR J ~ 0 THRU 19 DO                00103700
                                    COLUMNHEADS8 [NUMHEDLINES,J] ) ;    00103800
               GO TO ENDCARD ;                                          00103900
         END HEADINGS CARD ;                                            00104000
         IF IT = "BREAK "            %  BREAK CARD                      00104100
         THEN BEGIN                                                     00104200
              NUMCTL ~ NUMCTL + 1 ;                                     00104300
              S ~ SCANIT  ;   % GET FIELD NAME                          00104400
              IF S = 3 AND STRING [0] = "#"                             00104500
              THEN BEGIN S ~ SCANIT ;        % POUND FIELD              00104600
                         IF S = 0 AND TYPE = 6  % 6 DIGIT INTEGER       00104700
                         THEN BEGIN CHARCT ~ IT DIV 10000 ;             00104800
                                    IF CHARCT > 12 THEN CHARCT ~ 12 ;   00104900
                                    KEYARRAY8 [NUMCTL].[42:6] ~ CHARCT ;00105000
                                    KEYARRAY8 [NUMCTL].[24:18]  % WHERE 00105100
                                    ~ UNPACK (IT MOD 10000 DIV 10 ) ;   00105200
                                    S ~ SCANIT ;                        00105300
                                    IF TYPE ! -2 OR S < 9               00105400
                                    THEN ERROR1                         00105500
                                    ELSE BEGIN CTLID8[2|NUMCTL  ] ~ IT ;00105600
                                               CTLID8[2|NUMCTL+1]       00105700
                                               ~ STRING [1] ;           00105800
                                               S ~ SCANIT ;             00105900
                                               IF TYPE = -1             00106000
                                               AND IT = "PAGE  "        00106100
                                               THEN PGBRK ~ 2|NUMCTL ;  00106200
                                               GO TO ENDCARD ;          00106300
                                         END                            00106400
                              END 6 DIGITS                              00106500
                         ELSE ERROR2 ;                                  00106600
                   END POUND FIELD ;                                    00106700
              IF S < 10 OR S > 160 OR TYPE ! -1 THEN ERROR17 ;          00106800
              IF S { 120 THEN STRING [2] ~ "      " ;                   00106900
              CREATEFILEFIELD (STRING, FILEFIELD );                     00107000
              FOR I ~ 1 THRU 1000 DO BEGIN                              00107100
              READ (DICTION [I], 3, READRAY [*] )  [ERR3]  ;            00107200
              IF SAMENAME (READRAY, FILEFIELD)                          00107300
              THEN GO TO HITRECORD  END LOOP ;                          00107400
ERR3 :        ERROR3 ;                                                  00107500
HITRECORD:    READ (READRAY [*], F1, TIPE, CHARCT, WHERE ) ;            00107600
              IF TIPE } 2 THEN ERROR18 ;                                00107700
              IF TIPE = 1 THEN TURNONPLUGCASEBOOLEAN ;                  00107800
              IF CHARCT > 12 THEN CHARCT ~ 12 ;                         00107900
              KEYARRAY8 [NUMCTL].[18:6] ~ TIPE ;                        00108000
              KEYARRAY8 [NUMCTL].[42:6] ~ CHARCT ;                      00108100
              KEYARRAY8 [NUMCTL].[24:18] ~ WHERE ;                      00108200
              CTLID8[2|NUMCTL] ~ STRING [0] ;                           00108300
              CTLID8[2|NUMCTL+1] ~ STRING [1] ;                         00108400
              FOR I ~ 1, 2 DO BEGIN                                     00108500
              S ~ SCANIT ;                                              00108600
              IF S = 2 THEN GO TO SCANFILE ;  % ARROW                   00108700
              IF S > 9 AND                                              00108800
                 TYPE = -2  % QUOTE FOR FIELD NAME                      00108900
              THEN BEGIN CTLID8[2|NUMCTL  ] ~ STRING [0] ;              00109000
                         CTLID8[2|NUMCTL+1] ~ STRING [1] ;              00109100
                   END ;                                                00109200
              IF TYPE = -1 AND STRING [0] = "PAGE  "                    00109300
              THEN PGBRK ~ 2 | NUMCTL ;                                 00109400
                              END LOOP ;                                00109500
              GO TO ENDCARD ;                                           00109600
              END BREAK CARD ;                                          00109700
    IF IT = "LINE  "                 % LINE CARD                        00109800
    THEN BEGIN                                                          00109900
              NUMLINEFLDS ~ NUMLINEFLDS + 1 ;                           00110000
              S ~ SCANIT ;                                              00110100
              IF S = 0 THEN BEGIN SPASE ~ IT ;                          00110200
                                  S ~ SCANIT ;                          00110300
                            END                                         00110400
                       ELSE SPASE ~ 1  ;                                00110500
              IF S = 3 AND IT = "#"                                     00110600
              THEN BEGIN              % POUND FIELD                     00110700
                        S ~ SCANIT ;                                    00110800
                        IF S ! 0 OR TYPE ! 6 THEN ERROR2 ;              00110900
                        WHICHONE ~ 1 ;                                  00111000
                        A ~ IT MOD 10000 DIV 1000  ;                    00111100
                        B ~ IT MOD 1000 DIV 100  ;                      00111200
                        C ~ IT MOD 100  DIV 10   ;                      00111300
                        CHARCT ~ IT DIV 10000 ;                         00111400
                        IFNUM ~ IT MOD 10  ;                            00111500
                   END                                                  00111600
              ELSE IF S > 9 AND TYPE = -1  % ALPHA FIELD                00111700
                   THEN BEGIN                                           00111800
                        IF S > 160 THEN ERROR17 ;                       00111900
                       IF S { 120 THEN STRING [2] ~ "      " ;          00112000
                        CREATEFILEFIELD (STRING, FILEFIELD ) ;          00112100
                        FOR I ~ 1 THRU 1000 DO BEGIN                    00112200
                        READ (DICTION [I], 3, READRAY [*] ) [ERR3]  ;   00112300
                        IF  SAMENAME (READRAY, FILEFIELD )              00112400
                        THEN GO TO GOTIT      END LOOP ;                00112500
GOTIT :                 READ (READRAY [*], F2, TIPE, CHARCT,            00112600
                                               A, B, C, IFNUM ) ;       00112700
                        IF TIPE = 0 OR TIPE = 1                         00112800
                        THEN WHICHONE ~ TIPE +1                         00112900
                        ELSE ERROR18 ;                                  00113000
                        IF TIPE = 1 THEN TURNONPLUGCASEBOOLEAN ;        00113100
                        END                                             00113200
                   ELSE IF S > 9 AND TYPE = -2   % QUOTE CONSTANT       00113300
                        THEN BEGIN                                      00113400
                             WHICHONE ~ 3 ;                             00113500
                             IF S > 60 THEN ERROR19 ;                   00113600
                             CHARCT ~ S/10 ;                            00113700
                             IFNUM ~ 0 ;                                00113800
                             A ~ QUOTCHR DIV 100 ;                      00113900
                             B ~ QUOTCHR DIV 10 MOD 10 ;                00114000
                             C ~ QUOTCHR MOD 10 ;                       00114100
                             QUOTCHR ~ QUOTCHR + CHARCT ;               00114200
                             MOVECHAR (STRING, QUOTARRAY, A,B,C,CHARCT);00114300
                             END                                        00114400
                        ELSE ERROR20 ;                                  00114500
EDITNUM :     IF IFNUM = 1  % NUMERIC FIELD                             00114600
              THEN BEGIN S ~ SCANIT ;                                   00114700
                         IF S = 0                                       00114800
                         THEN BEGIN IFDEC ~ 1 ;                         00114900
                                    DIGIT ~ CHARCT - IT  ;              00115000
                                    NUMDEC ~ IT ;                       00115100
                                    IF DIGIT < 0  THEN ERROR22 ;        00115200
                             END                                        00115300
                         ELSE BEGIN IFDEC ~ 0 ;                         00115400
                                    DIGIT ~ CHARCT ;                    00115500
                                    NUMDEC ~ 0 ;                        00115600
                             END ;                                      00115700
                   END ;                                                00115800
              FILLFIELDKEY (FIELDKEY8[2|(NUMLINEFLDS-1)], WHICHONE, A,B,00115900
                            C, CHARCT, IFNUM,IFDEC,DIGIT,NUMDEC,SPASE );00116000
              LINECHR ~ LINECHR + CHARCT + SPASE +(IF IFNUM = 1         00116100
                                                   THEN IFDEC           00116200
                                                   ELSE 0);             00116300
              IF LINECHR > 120 THEN ERROR27  ;                          00116400
              GO TO ENDCARD ;                                           00116500
         END LINE CARD ;                                                00116600
    IF IT = "TOTAL " THEN GO TO TOTALS ELSE ERROR7 ;                    00116700
END OF NESTED LEAD BLOCK;                                               00116800
%-----------------------------------------------------------------------00116900
TOTALS :                                                                00117000
BEGIN %NESTED TOTALS BLOCK                                              00117100
LABEL         GOTAHIT, TIPE0, EDITDEC, ENDCARD, SCANFILE, TOTCARD, ERR3;00117200
    GO TO TOTCARD ;                                                     00117300
ENDCARD :                                                               00117400
    WHILE TRUE DO                                                       00117500
    IF S = 2                                                            00117600
    THEN GO TO SCANFILE                                                 00117700
    ELSE S ~ SCANIT  ;                                                  00117800
SCANFILE :                                                              00117900
    READ  (CARDSIN [NO], 10, READRAY [*] )[EOF] ;                       00118000
    WRITE (   PTR  ,     10, READRAY [*] ) ;                            00118100
    S ~ SCANIT ;   % START NEW PERAMETER CARD                           00118200
    IF TYPE ! -1 THEN  ERROR7 ;                                         00118300
    IF IT ! "TOTAL " THEN ERROR21                                       00118400
    ELSE                                                                00118500
TOTCARD :                                                               00118600
         BEGIN                      %  TOTAL CARD                       00118700
              NUMTOTFLDS ~ NUMTOTFLDS + 1 ;                             00118800
              S ~ SCANIT ;                                              00118900
              IF S = 0 THEN BEGIN SPASE ~ IT ;                          00119000
                                  S ~ SCANIT ;                          00119100
                            END                                         00119200
                       ELSE SPASE ~ 1 ;                                 00119300
              IF S = 3 AND IT = "#"                                     00119400
              THEN BEGIN            % POUND FIELD                       00119500
                       S ~ SCANIT ;                                     00119600
                       IF S ! 0 OR TYPE ! 6 THEN ERROR2 ;               00119700
                      WHERE ~ IT DIV 10 MOD 1000 ;                      00119800
                      CHARCT ~ IT DIV 10000  ;                          00119900
                      IFNUM ~ IT MOD 10 ;                               00120000
                      IF IFNUM ! 1 THEN ERROR23 ;                       00120100
                      GO TO TIPE0 ;                                     00120200
                   END                                                  00120300
              ELSE IF S > 9 AND TYPE = -1  % ALPHA FIELD                00120400
                   THEN BEGIN                                           00120500
                          IF IT = "TALLY "                              00120600
                          THEN BEGIN WHICHONE ~ A ~ 0 ;                 00120700
                                     CHARCT ~ 4 ;                       00120800
                                     IFNUM ~ 1 ;                        00120900
                               END                                      00121000
                          ELSE BEGIN                                    00121100
                          IF S > 160 THEN ERROR17 ;                     00121200
                          IF S { 120 THEN STRING [2] ~ "      " ;       00121300
                          CREATEFILEFIELD (STRING, FILEFIELD ) ;        00121400
                        FOR I ~ 1 THRU 1000 DO BEGIN                    00121500
                        READ (DICTION [I], 3, READRAY [*] ) [ERR3]  ;   00121600
                          IF SAMENAME (READRAY, FILEFIELD )             00121700
                          THEN GO TO GOTAHIT    END LOOP ;              00121800
ERR3 :                    ERROR3 ;                                      00121900
GOTAHIT :                 READ (READRAY [*], F3, TIPE, CHARCT,          00122000
                                                       WHERE, IFNUM ) ; 00122100
                          IF IFNUM ! 1 THEN ERROR23 ;                   00122200
                          IF TIPE > 2 OR TIPE < 0 THEN ERROR24 ;        00122300
                          CASE TIPE OF BEGIN                            00122400
                          BEGIN    % TIPE 0                             00122500
                            MOVEWORDS (FILEFIELD [1],                   00122600
                                       SQUEEZE8 [2|NUMCTR], 2 ) ;       00122700
TIPE0 :                     WHICHONE ~ 0 ;                              00122800
                            NUMRECTOTLFLDS ~ NUMRECTOTLFLDS + 1 ;       00122900
                            NUMCTR ~ NUMCTR + 1 ;                       00123000
                            A ~ NUMCTR ;                                00123100
                            ADDRECKEY8[NUMRECTOTLFLDS, 0] ~ WHERE ;     00123200
                            ADDRECKEY8[NUMRECTOTLFLDS, 1] ~ CHARCT ;    00123300
                            ADDRECKEY8[NUMRECTOTLFLDS, 2] ~ NUMCTR ;    00123400
                          END OF TIPE 0 ;                               00123500
                          BEGIN    % TIPE 1                             00123600
                            MOVEWORDS (FILEFIELD [1],                   00123700
                                       SQUEEZE8 [2|NUMCTR], 2 ) ;       00123800
                            WHICHONE ~ 0  ;                             00123900
                            NUMDEVTOTLFLDS ~ NUMDEVTOTLFLDS + 1 ;       00124000
                            NUMCTR ~ NUMCTR + 1 ;                       00124100
                            A ~ NUMCTR ;                                00124200
                            ADDEVKEY8[NUMDEVTOTLFLDS, 0 ] ~ WHERE ;     00124300
                            ADDEVKEY8[NUMDEVTOTLFLDS, 01 ] ~ CHARCT ;   00124400
                            ADDEVKEY8[NUMDEVTOTLFLDS, 02 ] ~ NUMCTR ;   00124500
                            TURNONPLUGCASEBOOLEAN ;                     00124600
                          END OF TIPE 1 ;                               00124700
                          BEGIN    % TIPE 2                             00124800
                            WHICHONE ~ 2 ;                              00124900
                            TIPE2 ~ TRUE ;                              00125000
                            READ ( READRAY [*], F8, A, B, C ) ;         00125100
                            FOR I ~ 0 THRU CALCMAX DO BEGIN             00125200
                            IF NAMESAME (FILEFIELD, CALCDICT [I,*] )    00125300
                            THEN BEGIN FOR J ~ 3 THRU 6 DO              00125400
                                       ISCALC [CALCDICT[I,J] ] ~ TRUE ; 00125500
                                       GO TO EDITDEC ;                  00125600
                                 END                                    00125700
                                                      END LOOP ;        00125800
                            ERROR25 ;                                   00125900
                          END TIPE 2 ;                                  00126000
                                       END CASE STATEMENT  ;            00126100
                               END                                      00126200
                        END                                             00126300
                   ELSE IF S > 9 AND TYPE = -2  % QUOTE CONSTANT        00126400
                        THEN BEGIN                                      00126500
                               WHICHONE ~ 3 ;                           00126600
                               IF S > 60 THEN ERROR19 ;                 00126700
                               CHARCT ~ S/10 ;                          00126800
                               IFNUM ~ 0 ;                              00126900
                               A ~ TQCHR DIV 100 ;                      00127000
                               B ~ TQCHR DIV 10 MOD 10 ;                00127100
                               C ~ TQCHR MOD 10 ;                       00127200
                               TQCHR ~ TQCHR + CHARCT ;                 00127300
                               MOVECHAR (STRING,TOTALQUOT,A,B,C,CHARCT);00127400
                             END                                        00127500
                        ELSE ERROR20 ;                                  00127600
EDITDEC :     IF IFNUM = 1  % NUMERIC FIELD ;                           00127700
              THEN BEGIN                                                00127800
                     IF CHARCT >  12 THEN ERROR30 ;                     00127900
                     IF NOT TIPE2 THEN BEGIN                            00128000
                     WHILE CHARCT < 8 AND SPASE > 1 DO BEGIN            00128100
                           CHARCT ~ CHARCT + 1 ;                        00128200
                           SPASE ~ SPASE  - 1 ;        END LOOP ;       00128300
                     IF CHARCT > 8 THEN                                 00128400
                     WHILE CHARCT <12 AND SPASE > 1 DO BEGIN            00128500
                           CHARCT ~ CHARCT + 1 ;                        00128600
                           SPASE ~ SPASE  - 1 ;        END LOOP ;       00128700
                                       END                              00128800
                                  ELSE TIPE2 ~ FALSE ;                  00128900
                     S ~ SCANIT ;                                       00129000
                     IF S = 0                                           00129100
                     THEN BEGIN IFDEC ~ 1 ;                             00129200
                                DIGIT ~ CHARCT - IT ;                   00129300
                                NUMDEC ~ IT ;                           00129400
                                IF DIGIT < 0 THEN ERROR22 ;             00129500
                          END                                           00129600
                     ELSE BEGIN IFDEC ~ 0 ;                             00129700
                                DIGIT ~ CHARCT ;                        00129800
                                NUMDEC ~ 0 ;                            00129900
                          END ;                                         00130000
                   END ;                                                00130100
              FILLFIELDKEY (TOTLINEKEY8[2|(NUMTOTFLDS-1)], WHICHONE,    00130200
                               A, B, C,                                 00130300
                               CHARCT, IFNUM,IFDEC,DIGIT,NUMDEC,SPASE );00130400
              TOTLCHR ~ TOTLCHR + CHARCT + SPASE +(IF IFNUM = 1         00130500
                                                   THEN IFDEC           00130600
                                                   ELSE 0);             00130700
              IF TOTLCHR > 120 THEN ERROR28  ;                          00130800
              GO TO ENDCARD ;                                           00130900
         END ;                                                          00131000
END OF NESTED TOTALS BLOCK  ;                                           00131100
%-----------------------------------------------------------------------00131200
EOF :                                                                   00131300
BEGIN %NESTED EOF BLOCK                                                 00131400
LABEL         GOTHIT, ERR26, ENDCARD ;                                  00131500
    FOR I ~ 1 THRU CALCMAX DO BEGIN                                     00131600
       IF ISCALC [I]                                                    00131700
       THEN FOR J ~ 0 THRU IMPDEVMAX DO BEGIN                           00131800
              IF I = IMPLYDEVELOPES [J,0]                               00131900
                 AND COMPARE16CHAR(IMPLYDEVELOPES[J,1],SQUEEZE8,        00132000
                                                       NUMCTR   ) = 63  00132100
    COMMENT              A TYPE 2 TOTAL FIELD REQUIRES THE PRESENCE OF  00132200
                   A TOTALED DEVELOPED FIELD WHICH HAS NOT BEEN CALLED  00132300
                   FOR BY ANY TOTAL CARD. THEREFORE, THE DEVELOPED FIELD00132400
                   MUST NOW BE SET ON, KEYED TO ACCUMULATE, AND BE      00132500
                   LISTED IN THE "SQUEEZE" ARRAY.   ;                   00132600
              THEN BEGIN                                                00132700
                     MOVEWORDS (IMPLYDEVELOPES[J,1], FILEFIELD [1], 2 );00132800
                     FOR K ~ 1 THRU 1000 DO BEGIN                       00132900
                     READ (DICTION [K], 3, READRAY [*] ) [ERR26]  ;     00133000
                     IF SAMENAME (READRAY, FILEFIELD )                  00133100
                     THEN GO TO GOTHIT     END DICTIONARY LOOP ;        00133200
ERR26 :              ERROR26 ;                                          00133300
GOTHIT :             READ (READRAY [*], F3, TIPE, CHARCT, WHERE, IFNUM);00133400
                     IF IFNUM ! 1 THEN ERROR23 ;                        00133500
                     IF TIPE > 2 OR TIPE < 0 THEN ERROR24 ;             00133600
                     MOVEWORDS (FILEFIELD [1], SQUEEZE8 [2|NUMCTR], 2 );00133700
                     NUMCTR ~ NUMCTR +1 ;                               00133800
                     CASE TIPE OF BEGIN                                 00133900
                          BEGIN    % TIPE 0                             00134000
                            NUMRECTOTLFLDS ~ NUMRECTOTLFLDS + 1 ;       00134100
                            ADDRECKEY8[NUMRECTOTLFLDS, 0] ~ WHERE ;     00134200
                            ADDRECKEY8[NUMRECTOTLFLDS, 1] ~ CHARCT ;    00134300
                            ADDRECKEY8[NUMRECTOTLFLDS, 2] ~ NUMCTR ;    00134400
                          END OF TIPE 0 ;                               00134500
                          BEGIN    % TIPE 1                             00134600
                            NUMDEVTOTLFLDS ~ NUMDEVTOTLFLDS + 1 ;       00134700
                            ADDEVKEY8[NUMDEVTOTLFLDS, 0 ] ~ WHERE ;     00134800
                            ADDEVKEY8[NUMDEVTOTLFLDS, 01 ] ~ CHARCT ;   00134900
                            ADDEVKEY8[NUMDEVTOTLFLDS, 02 ] ~ NUMCTR ;   00135000
                            TURNONPLUGCASEBOOLEAN ;                     00135100
                          END OF TIPE 1 ;                               00135200
                            ERROR29 ;                                   00135300
                                  END CASE STATEMENT ;                  00135400
ENDCARD :          END ;                                                00135500
                                        END IMPLY LOOP ;                00135600
                              END ISCALC LOOP ;                         00135700
    IF INDENT = 0 THEN INDENT ~ 1 ;                                     00135800
    IF DATERAY  [0] = 0                                                 00135900
    THEN BEGIN                                                          00136000
              NAME ~ YRDAYTONORM (TIME(0) ) ;                           00136100
              WRITE (DATERAY [*], D, NAME.[24:12], NAME.[36:12],        00136200
                                                   NAME.[12:12] ) ;     00136300
         END                                                            00136400
    ELSE IF DATERAY [2] = 0 THEN DATERAY [2] ~ "      " ;               00136500
    IF LINEMAX = 0 THEN LINEMAX ~ 60 ;                                  00136600
    BOMTITLELINECT ~ LINEMAX - 2 | NUMBOMTITLES ;                       00136700
    LINEMAX ~ BOMTITLELINECT - 3 | NUMCTL  - 2 ;                        00136800
    IF NUMBOMTITLES > 0 THEN LINEMAX ~ LINEMAX - 3 ;                    00136900
    IF NUMLINEFLDS > 0 THEN BEGIN                                       00137000
                       LISTING ~ TRUE ;                                 00137100
                       A ~ 120 - LINECHR ;                              00137200
                       IF A > 63 THEN BEGIN B ~ A - 63 ;                00137300
                                            A ~ 63 ;                    00137400
                                      END                               00137500
                                 ELSE B ~ 0 ;                           00137600
                       FIELDKEY8 [0].[6:6] ~ NUMLINEFLDS ;              00137700
                       FIELDKEY8 [0].[12:6] ~ A ;                       00137800
                       FIELDKEY8 [0].[18:6] ~ B ;                       00137900
                            END                                         00138000
                       ELSE NUMLINEFLDS ~ 1 ;                           00138100
    IF NUMTOTFLDS  > 0 THEN BEGIN                                       00138200
                      TOTALING ~ TRUE ;                                 00138300
                       A ~ 120 - TOTLCHR ;                              00138400
                       IF A > 63 THEN BEGIN B ~ A - 63 ;                00138500
                                            A ~ 63 ;                    00138600
                                      END                               00138700
                                 ELSE B ~ 0 ;                           00138800
                     TOTLINEKEY8 [0].[6:6] ~ NUMTOTFLDS  ;              00138900
                     TOTLINEKEY8 [0].[12:6] ~ A ;                       00139000
                     TOTLINEKEY8 [0].[18:6] ~ B ;                       00139100
                            END                                         00139200
                       ELSE NUMTOTFLDS  ~ 1 ;                           00139300
    KEYARRAY8 [1].[6:6] ~ NUMCTL ;                                      00139400
    DBLBRKMAX ~ DBLCTLMAX ~ 2 | NUMCTL ;                                00139500
    IF NUMCTL > 0 THEN BREAKING ~ TRUE                                  00139600
                  ELSE DBLCTLMAX ~ 1 ;                                  00139700
    IF NUMRECTOTLFLDS > 0 THEN RECORDTOTALING ~ TRUE                    00139800
                          ELSE NUMRECTOTLFLDS ~ 1 ;                     00139900
    IF NUMDEVTOTLFLDS > 0 THEN    DEVTOTALING ~ TRUE                    00140000
                          ELSE NUMDEVTOTLFLDS ~ 1 ;                     00140100
    FOR I ~ 0 THRU PLUGMAX DO                                           00140200
    IF ISDEVELOPE [I] THEN DEVELOPING ~ TRUE ;                          00140300
    FOR I ~ 0 THRU CALCMAX DO                                           00140400
    IF ISCALC [I] THEN CALCULATING ~ TRUE ;                             00140500
    WRITE (PTR, F4, NUMERR ) ;                                          00140600
    IF NUMERR > 0 THEN GO TO ENDJOB ;                                   00140700
    WRITE (PTR [PAGE] ) ;                                               00140800
    IF NUMTOPTITLES > 0 THEN TOPTITLEXISTS ~ TRUE ;                     00140900
    IF NUMBOMTITLES > 0 THEN BOMTITLEXISTS ~ TRUE ;                     00141000
    IF NUMHEDLINES > 0 THEN COLUMNHEADINGS ~ TRUE                       00141100
                       ELSE NUMHEDLINES ~ 1 ;                           00141200
END OF NESTED EOF BLOCK ;                                               00141300
END OF PRE EDIT BLOCK ;                                                 00141400
%-----------------------------------------------------------------------00141500
BEGIN                                                                   00141600
COMMENT THIS BLOCK GENERATES THE WRITTEN REPORT.  ;                     00141700
LABEL         CONTROL, DEVELOPE, CTLBRK, EOJ ;                          00141800
FILE IN INFILE DISK SERIAL (2, NUMWDSPEREC, NUMWDSPERBLK );             00141900
INTEGER ARRAY ADDRECKEY [1:NUMRECTOTLFLDS, 0:2] ,                       00142000
              ADDEVKEY  [1:NUMDEVTOTLFLDS, 0:2] ;                       00142100
ARRAY         CTLID [0:DBLCTLMAX+1],                                    00142200
              CTLWD [0:DBLCTLMAX+1],                                    00142300
              STORECTL [0:DBLCTLMAX+1]  ,                               00142400
              COLUMNHEADS [1:NUMHEDLINES, 0:19 ] ,                      00142500
              TOPTITLERAY [0:NUMTOPTITLES, 0:10] ,                      00142600
              BOMTITLERAY [0:NUMBOMTITLES, 0:10] ,                      00142700
              SQUEEZE [0: 2|NUMCTR  ],                                  00142800
                   KEYARRAY [1:IF NUMCTL > 0 THEN NUMCTL ELSE 1],       00142900
                   FIELDKEY [0: 2|NUMLINEFLDS-1 ],                      00143000
                   TOTLINEKEY[0:2|NUMTOTFLDS-1 ] ,                      00143100
                        OUTARRAY [0:14 ] ,                              00143200
                        FILERAY  [1:NUMWDSPEREC ],                      00143300
                        TOTARRAY [0:DEVELOPEARRAYMAX],                  00143400
                        DEVARRAY [0:DEVELOPEARRAYMAX];                  00143500
SAVE INTEGER ARRAY      COUNTERS [-1:NUMCTL, 0:NUMCTR ] ;               00143600
INTEGER       BRK, DBLBRK, LINECT ;                                     00143700
BOOLEAN       JUSTBROKE ;                                               00143800
%-----------------------------------------------------------------------00143900
DEFINE        REPLACEOVERSIZEDARRAYS =                                  00144000
         NUMTOPTITLES ~ NUMTOPTITLES - 1 ; %COMPENSATE DOWNSHIFT IN     00144100
         NUMBOMTITLES ~ NUMBOMTITLES - 1 ; %DECLARATION OF ARRAY        00144200
                                           %LOWER LIMITS                00144300
         FOR I ~ 0 THRU DBLBRKMAX+1 DO CTLID[I] ~ CTLID8 [I] ;          00144400
         FILL CTLID [*] WITH  "00F I N ", "00A L   " ;                  00144500
        FILL STORECTL[*]WITH  "00T O T ", "00A L   " ;                  00144600
         FOR I ~ 1 THRU NUMHEDLINES DO                                  00144700
         WRITE (COLUMNHEADS [I,*], 20, COLUMNHEADS8 [I,*] ) ;           00144800
         FOR I ~ 1 THRU NUMRECTOTLFLDS DO                               00144900
         WRITE (ADDRECKEY [I,*], 3, ADDRECKEY8[I,*] ) ;                 00145000
         FOR I ~ 1 THRU NUMDEVTOTLFLDS DO                               00145100
         WRITE (ADDEVKEY  [I,*], 3, ADDEVKEY8 [I,*] ) ;                 00145200
         FOR I ~ 0 THRU NUMTOPTITLES DO                                 00145300
         WRITE (TOPTITLERAY[I,*],11,TOPTITLERAY8[I+1,*] ) ;             00145400
         FOR I ~ 0 THRU NUMBOMTITLES DO                                 00145500
         WRITE (BOMTITLERAY[I,*],11,BOMTITLERAY8[I+1,*] ) ;             00145600
         MOVEWORDS (SQUEEZE8, SQUEEZE, 2|NUMCTR ) ;                     00145700
         MOVEWORDS (KEYARRAY8, KEYARRAY, NUMCTL ) ;                     00145800
         MOVEWORDS (FIELDKEY8, FIELDKEY, 2|NUMLINEFLDS );               00145900
         MOVEWORDS (TOTLINEKEY8, TOTLINEKEY, 2|NUMTOTFLDS ) # ;         00146000
%-----------------------------------------------------------------------00146100
PROCEDURE     PRINTOP ;                                                 00146200
BEGIN                                                                   00146300
OWN INTEGER   PAGENO ;                                                  00146400
FORMAT        F (3A6, X90, I3 );                                        00146500
              PAGENO ~ PAGENO + 1 ;                                     00146600
              WRITE (PTR, F, FOR I ~ 0 THRU 2 DO DATERAY [I], PAGENO ); 00146700
              WRITE (PTR [DBL] ) ;                                      00146800
              LINECT ~ 4 ;                                              00146900
              IF    TOPTITLEXISTS THEN BEGIN                            00147000
              FOR I ~ 0 THRU NUMTOPTITLES DO BEGIN                      00147100
              WRITE (PTR, T, FOR J ~ 0 THRU 10 DO TOPTITLERAY [I,J] ) ; 00147200
              LINECT ~ LINECT + 2            END ;                      00147300
                                       WRITE (PTR [DBL] );              00147400
                                       LINECT ~ LINECT + 2 ;            00147500
                                       END ;                            00147600
END OF PRINTOP PROCEDURE ;                                              00147700
%-----------------------------------------------------------------------00147800
PROCEDURE     PRINTBOTTOMTITLES  ;                                      00147900
BEGIN                                                                   00148000
                                       WHILE LINECT < BOMTITLELINECT    00148100
              DO BEGIN WRITE (PTR ); LINECT ~ LINECT +1 ;  END ;        00148200
              FOR I ~ 0 THRU NUMBOMTITLES DO                            00148300
              WRITE (PTR, T, FOR J ~ 0 THRU 10 DO BOMTITLERAY [I,J] ) ; 00148400
END OF PRINTBOTTOMTITLES PROCEDURE  ;                                   00148500
%-----------------------------------------------------------------------00148600
PROCEDURE     PRINTBOTTOM ;                                             00148700
BEGIN                                                                   00148800
              IF BOMTITLEXISTS THEN PRINTBOTTOMTITLES ;                 00148900
              WRITE (PTR [PAGE] );                                      00149000
              PRINTOP ;                                                 00149100
              JUSTBROKE ~ FALSE ;                                       00149200
END OF PRINTBOTTOM PROCEDURE ;                                          00149300
%-----------------------------------------------------------------------00149400
DEFINE        SKIPHEADERECORDS = FOR I ~ 1 THRU NUMHDREC DO             00149500
                                 READ (INFILE )   #  ;                  00149600
%-----------------------------------------------------------------------00149700
STREAM PROCEDURE ASSEMBLECONTROLS (STRING, KEY, CTLWDS, DEVARRAY ) ;    00149800
BEGIN                                                                   00149900
LOCAL         NUMCTL, SAVEKEY, SAVEFILE, SAVECTL, R1,R2,R3,R4,          00150000
              A, B, C, CHARCT, WHICHONE ;                               00150100
LABEL         CH1,CH2,CH3,CH4,CH5,CH6,CH7,CH8,CH9,CH10,CH11,CH12,       00150200
              MOVEALFA, RDFILE, POINT ;                                 00150300
              SI ~ KEY ;                                                00150400
              SAVEKEY ~ SI ;                                            00150500
              SI ~ SI + 1  ;                                            00150600
              DI ~ LOC NUMCTL ;                                         00150700
              DI ~ DI +7 ;                                              00150800
              DS ~ CHR ;                                                00150900
              DI ~ CTLWDS ;                                             00151000
              SAVECTL ~ DI ;                                            00151100
     NUMCTL ( SI ~ SAVEKEY ;   % ALL FOLLOWING REPEATS NUMCTL TIMES     00151200
              SI ~ SI +3 ;                                              00151300
              DI ~ LOC WHICHONE ; DI ~ DI + 7 ; DS ~ CHR ;              00151400
              DI ~ LOC A        ; DI ~ DI + 7 ; DS ~ CHR ;              00151500
              DI ~ LOC B        ; DI ~ DI + 7 ; DS ~ CHR ;              00151600
              DI ~ LOC C        ; DI ~ DI + 7 ; DS ~ CHR ;              00151700
              DI ~ LOC CHARCT   ; DI ~ DI + 7 ; DS ~ CHR ;              00151800
              SAVEKEY ~ SI ;                                            00151900
              CI ~ CI + WHICHONE ;                                      00152000
              GO TO RDFILE ;                                            00152100
              SI ~ DEVARRAY ;                                           00152200
              GO TO POINT ;                                             00152300
RDFILE :      SI ~ STRING  ;                                            00152400
POINT :       A(2(SI ~ SI +50 ));                                       00152500
              B(  SI ~ SI +10 ) ;                                       00152600
                  SI ~ SI + C   ;   % SI POINTS TO KEY WORD IN STRING   00152700
              SAVEFILE ~ SI ;                                           00152800
              DI ~ SAVECTL ;                                            00152900
              CI ~ CI + CHARCT ;                                        00153000
              GO TO  CH1  ;                                             00153100
              GO TO  CH1  ;                                             00153200
              GO TO  CH2  ;                                             00153300
              GO TO  CH3  ;                                             00153400
              GO TO  CH4  ;                                             00153500
              GO TO  CH5  ;                                             00153600
              GO TO  CH6  ;                                             00153700
              GO TO  CH7  ;                                             00153800
              GO TO  CH8  ;                                             00153900
              GO TO  CH9  ;                                             00154000
              GO TO  CH10 ;                                             00154100
              GO TO  CH11 ;                                             00154200
              GO TO  CH12 ;                                             00154300
              COMMENT  SUBROUTINE AT CHI ASSIGNS VALUES TO REPEAT       00154400
                       FACTORS (R1,R2,R3,R4) CORRESPONDING TO           00154500
                       CHARCT = 1.  THESE REPEAT FACTORS ARE USED TO    00154600
                       LOAD THE OUTPUT COUPLET WITH CHARACTERS AND      00154700
                       BLANKS ;                                         00154800
CH1 :         TALLY  ~  1  ;     R1  ~  TALLY  ;                        00154900
              TALLY  ~  5  ;     R2  ~  TALLY  ;                        00155000
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00155100
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00155200
              GO TO MOVEALFA ;                                          00155300
CH2 :         TALLY  ~  2  ;     R1  ~  TALLY  ;                        00155400
              TALLY  ~  4  ;     R2  ~  TALLY  ;                        00155500
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00155600
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00155700
              GO TO MOVEALFA ;                                          00155800
CH3 :         TALLY  ~  3  ;     R1  ~  TALLY  ;                        00155900
              TALLY  ~  3  ;     R2  ~  TALLY  ;                        00156000
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00156100
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00156200
              GO TO MOVEALFA ;                                          00156300
CH4 :         TALLY  ~  4  ;     R1  ~  TALLY  ;                        00156400
              TALLY  ~  2  ;     R2  ~  TALLY  ;                        00156500
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00156600
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00156700
              GO TO MOVEALFA ;                                          00156800
CH5 :         TALLY  ~  5  ;     R1  ~  TALLY  ;                        00156900
              TALLY  ~  1  ;     R2  ~  TALLY  ;                        00157000
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00157100
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00157200
              GO TO MOVEALFA ;                                          00157300
CH6 :         TALLY  ~  6  ;     R1  ~  TALLY  ;                        00157400
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00157500
              TALLY  ~  0  ;     R3  ~  TALLY  ;                        00157600
              TALLY  ~  6  ;     R4  ~  TALLY  ;                        00157700
              GO TO MOVEALFA ;                                          00157800
CH7 :         TALLY  ~  6  ;     R1  ~  TALLY  ;                        00157900
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00158000
              TALLY  ~  1  ;     R3  ~  TALLY  ;                        00158100
              TALLY  ~  5  ;     R4  ~  TALLY  ;                        00158200
              GO TO MOVEALFA ;                                          00158300
CH8 :         TALLY  ~  6  ;     R1  ~  TALLY  ;                        00158400
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00158500
              TALLY  ~  2  ;     R3  ~  TALLY  ;                        00158600
              TALLY  ~  4  ;     R4  ~  TALLY  ;                        00158700
              GO TO MOVEALFA ;                                          00158800
CH9 :         TALLY  ~  6  ;     R1  ~  TALLY  ;                        00158900
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00159000
              TALLY  ~  3  ;     R3  ~  TALLY  ;                        00159100
              TALLY  ~  3  ;     R4  ~  TALLY  ;                        00159200
              GO TO MOVEALFA ;                                          00159300
CH10 :        TALLY  ~  6  ;     R1  ~  TALLY  ;                        00159400
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00159500
              TALLY  ~  4  ;     R3  ~  TALLY  ;                        00159600
              TALLY  ~  2  ;     R4  ~  TALLY  ;                        00159700
              GO TO MOVEALFA ;                                          00159800
CH11 :        TALLY  ~  6  ;     R1  ~  TALLY  ;                        00159900
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00160000
              TALLY  ~  5  ;     R3  ~  TALLY  ;                        00160100
              TALLY  ~  1  ;     R4  ~  TALLY  ;                        00160200
              GO TO MOVEALFA ;                                          00160300
CH12 :        TALLY  ~  6  ;     R1  ~  TALLY  ;                        00160400
              TALLY  ~  0  ;     R2  ~  TALLY  ;                        00160500
              TALLY  ~  6  ;     R3  ~  TALLY  ;                        00160600
              TALLY  ~  0  ;     R4  ~  TALLY  ;                        00160700
MOVEALFA :    DS ~ 2 LIT "0" ;                                          00160800
              DS ~ R1 CHR ;                                             00160900
              R2 (DS ~ LIT " " );                                       00161000
              DS ~ 2 LIT "0" ;                                          00161100
              DS ~ R3 CHR ;                                             00161200
              R4 (DS ~ LIT " " );                                       00161300
              SAVECTL ~ DI ;       )  ;                                 00161400
END OF ASSEMBLECONTROLWORDS PROCEDURE ;                                 00161500
%-----------------------------------------------------------------------00161600
PROCEDURE     WRITECONTROLS ;                                           00161700
BEGIN                                                                   00161800
FORMAT        INDIC   (X*, 2A6, X2, 2A6 /)  ;                           00161900
              IF JUSTBROKE AND PGBRK } DBLBRK                           00162000
                                OR LINECT } LINEMAX THEN PRINTBOTTOM ;  00162100
              FOR I ~ DBLBRK STEP 2 UNTIL DBLBRKMAX DO BEGIN            00162200
              WRITE (PTR, INDIC                , INDENT | I ,           00162300
                     CTLID[I], CTLID[I+1], CTLWD[I],      CTLWD[I+1] ); 00162400
              LINECT ~ LINECT + 2 ;                    END ;            00162500
              WRITE (PTR [DBL] );                                       00162600
              LINECT ~ LINECT + 2 ;                                     00162700
END OF WRITECONTROLS PROCEDURE ;                                        00162800
%-----------------------------------------------------------------------00162900
DEFINE        STORECONTROLS = FOR I ~ DBLBRK THRU DBLBRKMAX+1 DO        00163000
                              STORECTL [I] ~ CTLWD [I]  #  ;            00163100
%-----------------------------------------------------------------------00163200
PROCEDURE     PRINTCOLUMNHEADS ;                                        00163300
BEGIN                                                                   00163400
              FOR I ~ 1 THRU NUMHEDLINES DO                             00163500
              WRITE (PTR, H, FOR J ~ 0 THRU 19 DO COLUMNHEADS [I,J] ) ; 00163600
              WRITE (PTR [DBL] ) ;                                      00163700
              LINECT ~ LINECT + NUMHEDLINES + 2  ;                      00163800
END OF PRINTCOLUMNHEADS PROCEDURE ;                                     00163900
%-----------------------------------------------------------------------00164000
DEFINE   COMPARECONTROLS = FOR I ~ 2 THRU DBLBRKMAX+1 DO                00164100
                           IF CTLWD [I] ! STORECTL [I]                  00164200
                           THEN BEGIN IF I MOD 2 = 0                    00164300
                                      THEN DBLBRK ~ I                   00164400
                                      ELSE DBLBRK ~ I-1 ;               00164500
                                      JUSTBROKE ~ TRUE ;                00164600
                                      GO TO CTLBRK ;                    00164700
                                END # ;                                 00164800
%-----------------------------------------------------------------------00164900
DEFINE   DEVELOPEOUTPUTLINE =                                           00165000
      COUNTERS [NUMCTL,0] ~ COUNTERS [NUMCTL,0] + 1 ;                   00165100
                   % TALLY OF RECORDS PROCESSED                         00165200
      IF LISTING THEN                                                   00165300
              ASSEMBLELINE (FILERAY, DEVARRAY, QUOTARRAY,               00165400
                                               FIELDKEY, OUTARRAY ) ;   00165500
      IF RECORDTOTALING THEN                                            00165600
              ADDFIELDTOCOUNTERS (FILERAY, ADDRECKEY, NUMRECTOTLFLDS ); 00165700
      IF DEVTOTALING THEN                                               00165800
              ADDFIELDTOCOUNTERS (DEVARRAY, ADDEVKEY, NUMDEVTOTLFLDS )#;00165900
%-----------------------------------------------------------------------00166000
PROCEDURE     ADDFIELDTOCOUNTERS (INARRAY, INKEY, NUMKEY ) ;            00166100
VALUE         NUMKEY ;                                                  00166200
ARRAY         INARRAY[*] , INKEY[1,0] ;                                 00166300
INTEGER       NUMKEY ;                                                  00166400
BEGIN                                                                   00166500
INTEGER       WORK, CTR ;                                               00166600
FORMAT        F (X*, I* );                                              00166700
              FOR I ~ 1 THRU NUMKEY DO BEGIN                            00166800
              READ(INARRAY[*], F, INKEY [I,0], INKEY [I,1], WORK ) ;    00166900
              CTR ~ INKEY [I,2] ;                                       00167000
              COUNTERS [NUMCTL,CTR] ~ COUNTERS [NUMCTL,CTR] + WORK ;    00167100
                                       END LOOP ;                       00167200
END OF ADDFIELDTOCOUNTERS PROCEDURE ;                                   00167300
%-----------------------------------------------------------------------00167400
INTEGER PROCEDURE  COUNTERINDEX (FIELDNAME );                           00167500
ARRAY    FIELDNAME [0] ;                                                00167600
BEGIN                                                                   00167700
COMMENT  COUNTERINDEX IS THE INDEX NUMBER (OR SUBSCRIPT) OF A FIELD     00167800
         IN THE ARRAY OF TOTAL COUNTERS. FIELDNAME IS THE NAME OF A     00167900
         FIELD WHICH EITHER APPEARS ON A TOTAL CARD, OR IS IMPLIED BY A 00168000
         TOTAL CARD CONTAINING A TYPE 2 FIELD.                          00168100
         FIELDNAME IS A TWO WORD ARRAY [0:1].                           00168200
         FIELDNAME MUST BE FILLED WITH ALL 16 CHARACTERS OF THE         00168300
         FIELD NAME.                                                    00168400
         THE MINOR COUNTER IS  "COUNTERS [NUMCTL, <COUNTERINDEX> ] ".   00168500
         ANY TOTAL COUNTER IS  "COUNTERS [ BRK  , <COUNTERINDEX> ] ";   00168600
              COUNTERINDEX ~ COMPARE16CHAR (FIELDNAME,SQUEEZE [0],      00168700
                                                              NUMCTR ); 00168800
END OF PROCEDURE COUNTERINDEX ;                                         00168900
%-----------------------------------------------------------------------00169000
STREAM PROCEDURE MOVECHAR       (N,SOURCE,SSKIP,DEST,DSKIP);            00169100
     VALUE N,SSKIP,DSKIP;                                               00169200
     BEGIN                                                              00169300
       SI ~ SOURCE; DI ~ DEST ;                                         00169400
     SI ~ SI + SSKIP; DI ~ DI + DSKIP;                                  00169500
      DS ~ N CHR ;                                                      00169600
     END ;                                                              00169700
%-----------------------------------------------------------------------00169800
STREAM PROCEDURE   UNPACKTODEST (NOM, DIGITS, DEST, DSKIP ) ;           00169900
VALUE              NOM, DIGITS, DSKIP ;                                 00170000
BEGIN                                                                   00170100
LOCAL              DSKIP1 ;                                             00170200
              SI ~ LOC DSKIP ;                                          00170300
              SI ~ SI + 6 ;                                             00170400
              DI ~ LOC DSKIP1 ;                                         00170500
              DI ~ DI + 7 ;                                             00170600
              DS ~ CHR ;                                                00170700
              SI ~ LOC NOM ;                                            00170800
              DI ~ DEST ;                                               00170900
    DSKIP1 (2 (DI ~ DI + 32 ) ) ;                                       00171000
              DI ~ DI + DSKIP ;                                         00171100
              DS ~ DIGITS DEC ;                                         00171200
END UNPACKTODEST PROCEDURE ;                                            00171300
%-----------------------------------------------------------------------00171400
COMMENT  PROCEDURES TO BE PLUGGED INTO THE CASE STATEMENT OF THIS PRO-  00171500
         CEDURE MAY DEVELOPE LINE FIELDS ONLY, NOT TOTAL FIELDS.        00171600
         THESE FIELDS ARE OF TYPE 1 IN THE DICTIONARY ENTRY.            00171700
         PLUG-INS MAY MOVE&EDIT FROM"FILERAY"WHICH CONTAINS THE INPUT   00171800
         RECORD. SUBTOTALS FROM THE MINOR COUNTERS ARE REFERENCED AS    00171900
         COUNTERS[NUMCTL, <INDEX> ]                                     00172000
         WHERE <INDEX>  IS OBTAINED FROM THE PROCEDURE  COUNTERINDEX.   00172100
         RESULTS OF PREVIOUS PLUG-INS MAY PICKED UP FROM DEVARRAY IN    00172200
         DISPLAY FORM WITH A MOVE EDIT. IF A PLUG-IN IS EXECUTED, IT    00172300
         WILL BE EXECUTED IN THE ORDER IN WHICH IT APPEARS IN THE CASE  00172400
         STATEMENT.                                                     00172500
         EVERY PLUG-IN MUST PUT ITS OUTPUT IN DISPLAY FORM INTO         00172600
         "DEVARRAY", IN POSITIONS IMMEDIATELY FOLLOWING THOSE ALLOCATED 00172700
         TO THE PREVIOUS PLUG-IN.                                       00172800
         ALL PLUG-INS REQUIRE THE UPDATEING OF BOTH THE "DICTION" FILE  00172900
         AND THE "SETPLUG" PROCEDURE. ;                                 00173000
%-----------------------------------------------------------------------00173100
%-----------------------------------------------------------------------00173200
COMMENT  CALCULATION PROCEDURES TO BE PLUGGED IN WILL CALCULATE TOTAL   00173300
         FIELDS ONLY (TYPE 2 ).                                         00173400
         COUNTERS [BRK, <INDEX> ]   WILL REFERENCE THE TOTAL COUNTERS   00173500
         WHERE <INDEX> IS OBTAIN FROM THE PROCEDURE CONVERTINDEX.       00173600
         RESULTS OF PREVIOUS CALCULATIONS MAY BE PICKED UP FROM         00173700
         DEVARRAY.                                                      00173800
         EVERY CALCULATION PROCEDURE MAY BE CALLED IN THE ORDER IN      00173900
         WHICH IT APPEARS IN THE CASE STATEMENT. ITS OUTPUT MUST BE     00174000
         PUT INTO DEVARRAY (DISPLAY FORM) IN THE POSITIONS ALLOCATED    00174100
         TO IT.                                                         00174200
         ALL PLUG-INS REQUIRE THE UPDATEINGOF BOTH THE "DICTION" FILE   00174300
         AND THE "SETCALC" PROCEDURE.  ;                                00174400
%-----------------------------------------------------------------------00174500
PROCEDURE     FILLDEVELOPEARRAY ;       %  DEVELOPE EACH RECORD         00174600
BEGIN                                                                   00174700
INTEGER       I  ;                                                      00174800
              FOR I ~1 THRU PLUGMAX DO                                  00174900
              IF ISDEVELOPE [I] THEN                                    00175000
              CASE I OF BEGIN                                           00175100
           ;            % 0  CASE NUMBER             (ZERO IS NULL)     00175200
                        END CASE STATEMENT ;                            00175300
END OF FILLDEVELOPEARRAY ;                                              00175400
%-----------------------------------------------------------------------00175500
PROCEDURE     CALCULATEDEVELOPEARRAY ;   % FOR TOTAL CALCULATIONS       00175600
BEGIN                                                                   00175700
INTEGER       I ;                                                       00175800
              FOR I ~ 1 THRU CALCMAX DO                                 00175900
              IF ISCALC [I] THEN                                        00176000
              CASE I OF BEGIN                                           00176100
           ;            % 0  CASE NUMBER             (ZERO IS NULL)     00176200
              END ;                                                     00176300
END OF CALCULATEDEVELOPEARRAY ;                                         00176400
%-----------------------------------------------------------------------00176500
PROCEDURE     ACCESSRECORD ;                                            00176600
BEGIN                                                                   00176700
LABEL         ACCESS ;                                                  00176800
ACCESS :      READ (INFILE, NUMWDSPEREC, FILERAY [*] )  [EOJ] ;         00176900
                                                                        00177000
                                                                        00177100
              IF DEVELOPING THEN FILLDEVELOPEARRAY ;                    00177200
END ACCESSRECORD PROCEDURE ;                                            00177300
%-----------------------------------------------------------------------00177400
STREAM PROCEDURE ASSEMBLELINE (FILERAY, DEVARRAY, QUOTARRAY,            00177500
                                                   FIELDKEY, OUTARRAY );00177600
BEGIN                                                                   00177700
LOCAL    NUMFIELDS, BLANK1, BLANK2,                                     00177800
                    SAVEKEY, WHICHONE, A,B,C, CHARCT, IFNUM, IFDEC,     00177900
         DIGIT, NUMDEC, SPASE, WHEREOUT, MOREWORK, WORK,                00178000
              MORE64, PWR64, ENDSUM ;                                   00178100
LABEL    RDFILE, RDDEV, POINT, NUMERICS, ENDLOOP, GETCTR, FAIL ;        00178200
              SI ~     FIELDKEY ;                                       00178300
              SAVEKEY ~ SI ;                                            00178400
              SI ~ SI + 1   ;                                           00178500
              DI ~ LOC NUMFIELDS ;  DI ~ DI + 7 ;  DS ~ CHR ;           00178600
              DI ~ LOC BLANK1    ;  DI ~ DI + 7 ;  DS ~ CHR ;           00178700
              DI ~ LOC BLANK2    ;  DI ~ DI + 7 ;  DS ~ CHR ;           00178800
              DI ~ OUTARRAY ;                                           00178900
              WHEREOUT ~ DI ;                                           00179000
  NUMFIELDS ( SI ~ SAVEKEY ;                                            00179100
              SI ~ SI +6 ;                                              00179200
              DI ~ LOC WHICHONE ; DI ~ DI + 7 ; DS ~ CHR ;              00179300
              DI ~ LOC A        ; DI ~ DI + 7 ; DS ~ CHR ;              00179400
              DI ~ LOC B        ; DI ~ DI + 7 ; DS ~ CHR ;              00179500
              DI ~ LOC C        ; DI ~ DI + 7 ; DS ~ CHR ;              00179600
              DI ~ LOC CHARCT   ; DI ~ DI + 7 ; DS ~ CHR ;              00179700
              DI ~ LOC IFNUM    ; DI ~ DI + 7 ; DS ~ CHR ;              00179800
              DI ~ LOC IFDEC    ; DI ~ DI + 7 ; DS ~ CHR ;              00179900
              DI ~ LOC DIGIT    ; DI ~ DI + 7 ; DS ~ CHR ;              00180000
              DI ~ LOC NUMDEC   ; DI ~ DI + 7 ; DS ~ CHR ;              00180100
              DI ~ LOC SPASE    ; DI ~ DI + 7 ; DS ~ CHR ;              00180200
              SAVEKEY ~ SI ;                                            00180300
              CI ~ CI + WHICHONE ;                                      00180400
              GO TO GETCTR ;                                            00180500
              GO TO RDFILE ;                                            00180600
              GO TO RDDEV ;                                             00180700
              SI ~ QUOTARRAY ;                                          00180800
              GO TO POINT ;                                             00180900
RDFILE :      SI ~ FILERAY ;                                            00181000
              GO TO POINT ;                                             00181100
RDDEV :       SI ~ DEVARRAY ;                                           00181200
POINT :       A(2(SI ~ SI +50)) ;                                       00181300
              B  (SI ~ SI +10 ) ;                                       00181400
                  SI ~ SI + C   ;                                       00181500
              IFNUM (JUMP OUT TO NUMERICS) ;                            00181600
              DI ~ WHEREOUT ;                 % ALPHANUMERICS           00181700
              SPASE (DS ~ LIT " " ) ;                                   00181800
              DS ~ CHARCT CHR  ;                                        00181900
              WHEREOUT ~ DI ;                                           00182000
              GO TO ENDLOOP ;                                           00182100
GETCTR :      SI ~ FILERAY ;   % POINT TO ACTUAL PARAMETER, COUNTERS    00182200
              A  (SI ~ SI + 8  );                                       00182300
              DI ~ LOC WORK ;                                           00182400
              DS ~ CHARCT DEC ;                                         00182500
              SI ~ LOC WORK ;                                           00182600
              IF TOGGLE THEN GO TO NUMERICS ;                           00182700
              SI ~ LOC CHARCT ;                                         00182800
              SI ~ SI + 7 ;                                             00182900
              IF SC } "0" THEN IF SC { "8"                              00183000
              THEN BEGIN                                                00183100
                   SI ~ LOC SPASE ;                                     00183200
                   SI ~ SI + 7 ;                                        00183300
                   IF SC = "0" THEN GO TO FAIL ;                        00183400
                   IF SC = "1" THEN GO TO FAIL ;                        00183500
                4 (TALLY ~ CHARCT ;                                     00183600
                   TALLY ~ TALLY + 1 ;                                  00183700
                   CHARCT ~ TALLY ;                                     00183800
                   TALLY ~ DIGIT ;                                      00183900
                   TALLY ~ TALLY + 1 ;                                  00184000
                   DIGIT ~ TALLY ;                                      00184100
                   TALLY ~ SPASE ;                                      00184200
                   TALLY ~ TALLY + 63 ;                                 00184300
                   SPASE ~ TALLY ;                                      00184400
                   SI ~ LOC CHARCT ;                                    00184500
                   SI ~ SI + 7 ;                                        00184600
                   IF SC > "9" THEN JUMP OUT ;     %  SC = 12           00184700
                   SI ~ LOC SPASE ;                                     00184800
                   SI ~ SI + 7 ;                                        00184900
                   IF SC = "1" THEN JUMP OUT ) ;                        00185000
                   END ;                                                00185100
              SI ~ FILERAY ;                                            00185200
              A ( SI ~ SI + 8 ) ;                                       00185300
              SKIP 9 SB ;                                               00185400
              DI ~ LOC A ;                                              00185500
              DI ~ DI + 7 ;                                             00185600
              DS ~ 3 RESET ;                                            00185700
              3 (IF SB THEN DS ~ SET ELSE DS ~ RESET ;  SKIP SB ) ;     00185800
              DI ~ LOC B ;  DI ~ DI + 7 ;  DS ~ CHR ;                   00185900
              DI ~ LOC C ;  DI ~ DI + 7 ;  DS ~ CHR ;                   00186000
              DI ~ LOC ENDSUM ;                                         00186100
              DS ~ 4 LIT "0" ;                                          00186200
              DS ~ 4 CHR ;                                              00186300
              SI ~ LOC ENDSUM ;                                         00186400
              DI ~ LOC WORK ;                                           00186500
              DS ~ 4 LIT "0" ;                                          00186600
              DS ~ 8 DEC ;       % WORK EXTENDS INTO MOREWORK           00186700
              DI ~ LOC PWR64 ;     %  PWR64 EXTENDS INTO MORE64         00186800
              DS ~ 12 LIT "068719476736" ;   %  SIXTH POWER OF 64       00186900
              A ( SI ~ LOC PWR64 ; DI ~ LOC WORK ; DS ~ 12 ADD ) ;      00187000
              DI ~ LOC PWR64 ;                                          00187100
              DS ~ 12 LIT "001073741824" ;   %  FIFTH POWER OF 64       00187200
              B ( SI ~ LOC PWR64 ; DI ~ LOC WORK ; DS ~ 12 ADD ) ;      00187300
              DI ~ LOC PWR64 ;                                          00187400
              DS ~ 12 LIT "000016777216" ;   % FOURTH POWER OF 64       00187500
              C ( SI ~ LOC PWR64 ; DI ~ LOC WORK ; DS ~ 12 ADD ) ;      00187600
              DI ~ LOC WORK ;  DI ~ DI + 11 ;  DS ~ 2 RESET ;           00187700
              TALLY ~ 12 ;                                              00187800
              CHARCT (TALLY ~ TALLY + 63) ;                             00187900
              A ~ TALLY ;                     % A = 12 - CHARCT         00188000
              SI ~ LOC WORK ;                                           00188100
              A (IF SC = "0" THEN SI ~ SI+1 ELSE JUMP OUT TO FAIL) ;    00188200
              GO TO NUMERICS ;                                          00188300
FAIL :                   DI ~ WHEREOUT ;                                00188400
                         SPASE (DS ~ LIT " " ) ;                        00188500
                         CHARCT(DS ~ LIT "*") ;                         00188600
                         IFDEC (DS ~ LIT "*") ;                         00188700
                         WHEREOUT ~ DI ;                                00188800
                         GO TO ENDLOOP ;                                00188900
NUMERICS :                                                              00189000
       DIGIT (C ~ SI ;   DI ~ C ;                                       00189100
              TALLY ~ DIGIT ;                                           00189200
              TALLY ~ TALLY + 63 ;                                      00189300
              DIGIT ~ TALLY ;                                           00189400
              DS ~ DIGIT FILL ; % ZERO SURPRESS WHOLE DIGITS EXCEPT LAST00189500
              IFDEC (DS ~ FILL);% ALSO SURPRESS LAST IF FOLLOWED BY DEC.00189600
              TALLY ~ TALLY + 1;                                        00189700
              DIGIT ~ TALLY ;                                           00189800
              JUMP OUT ) ;                                              00189900
              DI ~ WHEREOUT ;                                           00190000
              SPASE (DS ~ LIT " " ) ;                                   00190100
              DS ~ DIGIT CHR ;                                          00190200
              IFDEC (DS ~ LIT "." );                                    00190300
              DS ~ NUMDEC CHR ;                                         00190400
              WHEREOUT ~ DI ;                                           00190500
ENDLOOP :                    ;   )  ;                                   00190600
              BLANK1 (DS ~ LIT " " ) ;                                  00190700
              BLANK2 (DS ~ LIT " " ) ;                                  00190800
END OF  ASSEMBLELINE PROCEDURE ;                                        00190900
%-----------------------------------------------------------------------00191000
PROCEDURE PRINTOTALS ;                                                  00191100
BEGIN                                                                   00191200
COMMENT   PRINTOTALS DOES  - PRINT INDICATIVE CONTROLS.                 00191300
                           - FILL TOTARRAY WITH CALCULATIONS FROM       00191400
                                  THE TOTAL COUNTERS.                   00191500
                           - ASSEMBLE & PRINT OUTPUT FROM COUNTERS,     00191600
                                  DEVARRAY, AND TOTALQUOT ARRAY.        00191700
                           - ROLL AND CLEAR COUNTERS.   ;               00191800
FORMAT        TOTAL   (X*,2A6, X2, 2A6, X2, "***TOTAL***" )  ;          00191900
              IF LINECT } LINEMAX                                       00192000
              THEN PRINTBOTTOM                                          00192100
              ELSE BEGIN WRITE (PTR [DBL] );                            00192200
                         LINECT ~ LINECT + 2 ;                          00192300
                   END ;                                                00192400
              FOR I ~ DBLBRKMAX STEP -2 UNTIL DBLBRK DO BEGIN %BRK LOOP 00192500
              BRK ~ I/2  ;                                              00192600
              IF CALCULATING THEN CALCULATEDEVELOPEARRAY ;              00192700
              IF TOTALING THEN BEGIN                                    00192800
              WRITE (PTR, TOTAL                  , INDENT|I, CTLID[I],  00192900
                     CTLID[I+1], STORECTL [I], STORECTL [I+1] ) ;       00193000
              ASSEMBLELINE (COUNTERS[BRK,*], TOTARRAY, TOTALQUOT,       00193100
                                             TOTLINEKEY, OUTARRAY );    00193200
              WRITE (PTR [DBL], 15, OUTARRAY[*] ) ;                     00193300
              LINECT ~ LINECT + 3 ;                                     00193400
                               END TOTALING ;                           00193500
              FOR J ~ 0 THRU NUMCTR DO BEGIN   % ROLL COUNTERS          00193600
              COUNTERS [BRK-1,J] ~ COUNTERS [BRK-1,J] + COUNTERS[BRK,J];00193700
              COUNTERS [BRK  ,J] ~ 0 ; END ROLLING COUNTERS ;           00193800
                                       END OF BRK LOOP ;                00193900
              WRITE (PTR [DBL] ) ;                                      00194000
              LINECT ~ LINECT + 2 ;                                     00194100
END  OF PRINTOTALS ;                                                    00194200
%-----------------------------------------------------------------------00194300
%********************************************************************** 00194400
                   REPLACEOVERSIZEDARRAYS ;                             00194500
                   PRINTOP ;                                            00194600
                   SKIPHEADERECORDS ;                                   00194700
                   ACCESSRECORD  ;                                      00194800
                   IF BREAKING THEN BEGIN                               00194900
                   DBLBRK ~ 2 ;                                         00195000
                   JUSTBROKE ~ FALSE ;                                  00195100
                   ASSEMBLECONTROLS (FILERAY, KEYARRAY, CTLWD [2],      00195200
                                                        DEVARRAY ) ;    00195300
CONTROL :          WRITECONTROLS ;                                      00195400
                   STORECONTROLS ;                                      00195500
                                    END ;                               00195600
                   IF COLUMNHEADINGS THEN PRINTCOLUMNHEADS ;            00195700
DEVELOPE :         DEVELOPEOUTPUTLINE ;                                 00195800
                   IF LISTING THEN BEGIN                                00195900
                   IF LINECT } LINEMAX   THEN BEGIN PRINTBOTTOM ;       00196000
                                                    IF COLUMNHEADINGS   00196100
                                                    THEN                00196200
                                                    PRINTCOLUMNHEADS ;  00196300
                                              END ;                     00196400
                   WRITE (PTR, 15, OUTARRAY [*] ) ;                     00196500
                   LINECT ~ LINECT + 1 END ;                            00196600
                   ACCESSRECORD  ;                                      00196700
                   IF BREAKING THEN BEGIN                               00196800
                   ASSEMBLECONTROLS (FILERAY, KEYARRAY, CTLWD [2],      00196900
                                                        DEVARRAY ) ;    00197000
                   COMPARECONTROLS  ;                                   00197100
                                    END ;                               00197200
                   GO TO DEVELOPE   ;                                   00197300
CTLBRK :           PRINTOTALS  ;                                        00197400
                   GO TO CONTROL  ;                                     00197500
EOJ :              IF TOTALING THEN BEGIN                               00197600
                                    DBLBRK ~ 0 ;                        00197700
                                    PRINTOTALS ;                        00197800
                                    END ;                               00197900
                   IF BOMTITLEXISTS THEN PRINTBOTTOMTITLES ;            00198000
END OF REPORT GENERATING BLOCK ;                                        00198100
%-----------------------------------------------------------------------00198200
ENDJOB :                                                                00198300
END                OF PROGRAM SUPER REPORT.                             00198400
END;END.        LAST CARD ON 0CRDING TAPE                               00198500
