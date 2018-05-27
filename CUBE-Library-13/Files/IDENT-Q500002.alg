                         DENVER, COLORADO  80210.                       00000100
                                                                        00000200
          ABSTRACT:        IDENT/SCANNER IS AN ALGOL-60 PROGRAM, WRITTEN00000300
                         FOR THE BURROUGHS B5500 COMPUTER.  GIVEN A LIST00000400
                         OF IDENTIFIERS, RESERVED WORDS AND/OR NUMERIC  00000500
                         STRINGS, IDENT/SCANNER WILL SCAN AN ALGOL OR   00000600
                         ESPOL PROGRAM FOR THE DESIRED ENTITIES AND     00000700
                         REPORT THE SEQUENCE NUMBERS OF THE CARDS (OR   00000800
                         CARD IMAGES) ON WHICH THE ENTITIES APPEAR.     00000900
                           NOTIFICATION IS GIVEN IN THE EVENT A SCANNED-00001000
                         FOR ENTITY DOES NOT APPEAR.                    00001100
                           IDENT/SCANNER IGNORES ENTITIES APPEARING IN  00001200
                         COMMENTS, STRINGS AND "COMMENTS" WHICH FOLLOW  00001300
                         FREE-FLOATING PERCENT SIGNS.                   00001400
                           THE PROGRAM TO BE SCANNED MAY COME FROM      00001500
                         CARDS, MAGNETIC TAPE, MULTI-FILE MAGNETIC TAPE 00001600
                         OR DISK FILE.  WHEN THE PROGRAM IS ON MAGNETIC 00001700
                         TAPE, A FILE DECLARATION WITH BUFFER SPECIFI-  00001800
                         CATIONS:                                       00001900
                                        (2, 56, 10)                     00002000
                         IS USED.  IF THE PROGRAM IS ON DISK FILE THEN  00002100
                                        (2, 10, 150)                    00002200
                         IS USED.                                       00002300
                           SEVERAL OPTIONS FOR CONTROLLING LISTING AND  00002400
                         ASSIGNMENT OF SEQUENCE NUMBERS ARE AVAILABLE,  00002500
                         AS WELL AS THE CAPABILITY OF DESIGNATING THE   00002600
                         FILE ON WHICH THE PROGRAM TO BE SCANNED IS     00002700
                         LOCATED.                                       00002800
                                                                        00002900
          MINIMUM SYSTEM CONFIGURATION:                                 00003000
                         1) PROCESSORS ................  1              00003100
                         2) MEMORY MODULES ............  4              00003200
                         3) TAPE TRANSPORTS ...........  3 (VARIES)     00003300
                         4) DISK FILE STORAGE UNITS ...  1              00003400
                         5) LINE PRINTERS .............  1              00003500
                         6) CARD READERS ..............  1              00003600
                                                                        00003700
          DESCRIPTION:     TO USE IDENT/SCANNER, A <SCAN CONTROL DECK>  00003800
                         MUST BE PREPARED, CONSISTING OF:               00003900
                              1) A <SCAN OPTION CARD> (MINIMUM OF ONE), 00004000
                              2) AN <IDENTIFIER CARD> (MINIMUM OF ONE), 00004100
                              3) AND, IF THE PROGRAM TO BE SCANNED IS ON00004200
                                 CARDS, AN <END-OF-DECK CARD> MUST BE   00004300
                                 PRESENT FOLLOWING THE <SCAN CONTROL    00004400
                                 DECK>.                                 00004500
                                                                        00004600
                           THE SYNTAX FOR THE <SCAN OPTION CARD> IS AS  00004700
                         FOLLOWS:                                       00004800
               <SCAN OPTION CARD> ::= $ <OPTION LIST>                   00004900
               <OPTION LIST> ::= <OPTION> / <OPTION LIST><OPTION>       00005000
               <OPTION> ::= <PROGRAM SOURCE> / <LIST OPTION> /          00005100
                            <SEQUENCE OPTION>                           00005200
               <PROGRAM SOURCE> ::= CARD / TAPE <LABEL PART> /          00005300
                                    DISK <LABEL PART>                   00005400
               <LABEL PART> ::= "<FILE-ID>" /                           00005500
                                "<MULTI-FILE-ID>"<SOLIDUS>"<FILE-ID>"   00005600
               <SOLIDUS> ::= /                                          00005700
               <LIST OPTION> ::= LIST                                   00005800
               <SEQUENCE OPTION> ::= SEQ                                00005900
                           THE "$" MUST APPEAR IN COLUMN 1 OF A <SCAN   00006000
                         OPTION CARD>.  THE <OPTION LIST> MAY APPEAR    00006100
                         ANYWHERE IN COLUMNS 2-72 IN A GENERALLY FREE-  00006200
                         FIELD FORMAT.  A LISTING OF THE SCANNED PROGRAM00006300
                         WILL BE PRODUCED ON THE LINE PRINTER IF THE    00006400
                         <LIST OPTION> IS INVOKED.  IF THE <SEQUENCE    00006500
                         OPTION> IS INVOKED, THE PROGRAM WILL BE RE-    00006600
                         SEQUENCED AS IT IS READ, AND CROSS-REFERENCED  00006700
                         OUTPUT WILL REFER TO THE NEW SEQUENCE NUMBERS  00006800
                         ASSIGNED BY IDENT/SCANNER.  LISTING OF THE     00006900
                         SCANNED PROGRAM IS IMPLICITLY EFFECTED WHEN THE00007000
                         <SEQUENCE OPTION> IS INVOKED.  THE <PROGRAM    00007100
                         SOURCE> OPTION MUST APPEAR ON A CONTROL CARD   00007200
                         IN THE <SCAN CONTROL DECK>.  IF NO <PROGRAM    00007300
                         SOURCE> IS NAMED, THE PROGRAM WILL TERMINATE   00007400
                         AFTER PRINTING AN APPROPRIATE ERROR MESSAGE.   00007500
                                                                        00007600
                           THE SYNTAX FOR AN <IDENTIFIER CARD> IS AS    00007700
                         FOLLOWS:                                       00007800
               <IDENTIFIER CARD> ::= <IDENTIFIER> / <INTEGER>           00007900
               <IDENTIFIER> ::= <LETTER> / <IDENTIFIER><LETTER> /       00008000
                                <IDENTIFIER><DIGIT>                     00008100
               <INTEGER> ::= <DIGIT> / <INTEGER><DIGIT>                 00008200
                           EACH <IDENTIFIER CARD> CONTAINS ONE (1)      00008300
                         <IDENTIFIER> OR <INTEGER> FOR WHICH THE PROGRAM00008400
                         IS TO BE SCANNED.  NO <IDENTIFIER> OR <INTEGER>00008500
                         MAY EXCEED 63 CHARACTERS IN LENGTH AND MAY     00008600
                         APPEAR ANYWHERE IN COLUMNS 1-72 OF THE CARD.   00008700
                                                                        00008800
                           THE SYNTAX FOR THE <END-OF-DECK CARD> IS AS  00008900
                         FOLLOWS:                                       00009000
               <END-OF-DECK CARD> ::= $ LAST                            00009100
                           AN <END-OF-DECK CARD> IS REQUIRED WHEN THE   00009200
                         PROGRAM TO BE SCANNED IS ON CARDS.  THE "$"    00009300
                         MUST APPEAR IN COLUMN 1, "LAST" MAY APPEAR     00009400
                         ANYWHERE IN COLUMNS 2-72.                      00009500
                           IDENT/SCANNER ASSUMES THAT THE PROGRAM DECK  00009600
                         WHICH IS TO BE SCANNED IMMEDIATELY FOLLOWS THE 00009700
                         "$ LAST" CARD.  IN THE EVENT A "$ LAST" CARD   00009800
                         APPEARS WHEN THE PROGRAM TO BE SCANNED IS ON A 00009900
                         TAPE FILE OR A DISK FILE, THE "$ LAST" CARD IS 00010000
                         IGNORED.                                       00010100
                                                                        00010200
                           A LISTING FOR THE <SCAN CONTROL DECK> IS     00010300
                         PRINTED ON THE LINE PRINTER AS THE <SCAN       00010400
                         CONTROL DECK> IS READ IN.  THE LISTING OF THE  00010500
                         PROGRAM TO BE SCANNED (IF LISTING WAS INVOKED) 00010600
                         APPEARS NEXT.  FINALLY, A LISTING OF ALL       00010700
                         <IDENTIFIER>S AND/OR <INTEGER>S IN THE         00010800
                         <SCAN CONTROL DECK> APPEARS WITH THE SEQUENCE  00010900
                         NUMBERS OF THE CARDS OR CARD IMAGES ON WHICH   00011000
                         EACH APPEARS (OR NOTIFICATION OF NON-PRESENT   00011100
                         ENTITIES).  THIS LISTING IS IN ALPHABETIC      00011200
                         ORDER.                                         00011300
                           THE LABEL FOR THE <SCAN CONTROL DECK> IS     00011400
                         "CARD".  THE SORT CALLS FOR 3 SORT TAPES, 8000 00011500
                         WORDS OF CORE AND 600000 WORDS OF DISK.  THIS  00011600
                         MAY BE ALTERED RATHER READILY BY CHANGING THE  00011700
                         DEFINE DECLARATIONS WHICH APPEAR NEAR THE FRONT00011800
                         OF THE PROGRAM.                                00011900
                                                                       ;00012000
                                                                        00012100
                                                                        00012200
DEFINE         TAPES = 3#,         COMMENT  NUMBER OF SORT TAPES;       00012300
               DISC  = 600000#,    COMMENT  NUMBER OF DISK WORDS;       00012400
               CORE  = 8000#;      COMMENT  NUMBER OF CORE WORDS;       00012500
FILE IN        CARD  (4, 10);                                           00012600
FILE OUT       LINE 18 (4, 15);                                         00012700
FILE IN        TAPE "MULTFYL" "FYLIDNT" (2, 56, 10);                    00012800
FILE IN        DISK DISK SERIAL "MULTFYL" "FYLIDNT" (2, 10, 150);       00012900
SWITCH FILE    SWF ~ CARD, TAPE, DISK;                                  00013000
FILE           SRTIN (2, 56, 10);                                       00013100
FILE           SORTED (2, 56, 10);                                      00013200
REAL ARRAY     ACCUM[0:63],                                             00013300
               INFO[0:7, 0:511],                                        00013400
               RSRVD[0:19],                                             00013500
               STACKHEAD[0:124],                                        00013600
               OUDT[0:10];                                              00013700
SAVE REAL ARRAY ARAY[0:10],                                             00013800
                LYNE[0:16];                                             00013900
REAL           NCR, FCR, LCR, ELCLASS;                                  00014000
REAL           BASE;                                                    00014100
INTEGER        RSLT, CNT, NEXTINFO, NDX1, NDX2, INCR, RECS, CTR;        00014200
INTEGER        FYLIN;                                                   00014300
INTEGER        LASTINFO, SEQNO;                                         00014400
BOOLEAN        PRINT, LAST;                                             00014500
BOOLEAN        SEQUENCE;                                                00014600
BOOLEAN        SEQUENTS;                                                00014700
BOOLEAN        CC;                                                      00014800
DEFINE         LINK  = [36:12]#,                                        00014900
               LINKC = [36: 3]#,                                        00015000
               LINKR = [39: 9]#,                                        00015100
               CLSS  = [29: 7]#,                                        00015200
               PRES = [18:1]#,                                          00015300
               TCNT = [ 6:6]#,                                          00015400
               OTHERV   = 000#,   COMMENT  USER ENTITY;                 00015500
               COMMENTV = 010#,   COMMENT  COMMENT;                     00015600
               PCTV     = 001#,   COMMENT  %;                           00015700
               QUOTEV   = 002#,   COMMENT  ";                           00015800
               SLASHV   = 003#,   COMMENT  /;                           00015900
               CARDV    = 004#,   COMMENT  CARD;                        00016000
               TAPEV    = 005#,   COMMENT  TAPE;                        00016100
               DISKV    = 006#,   COMMENT  DISK;                        00016200
               SEQV     = 007#,   COMMENT  SEQUENCE;                    00016300
               LISTV    = 008#,   COMMENT  LIST;                        00016400
               LASTV    = 009#;   COMMENT  LAST;                        00016500
LABEL          EOC, EOF, EOS;                                           00016600
LABEL          FINISH;                                                  00016700
FORMAT         INFOR (X40, "THIS OUTPUT PRODUCED BY:  IDENT/SCANNER." //00016800
                      X37, "A LISTING OF YOUR <SCAN CONTROL DECK> FO",  00016900
                           "LLOWS:" //);                                00017000
FORMAT         LSTHD (X35, "THIS IS THE LISTING OF THE PROGRAM BEING",  00017100
                           " SCANNED:" //);                             00017200
FORMAT         SKOWT (X31, "YOUR IDENTIFIERS APPEAR IN THE SCANNED P",  00017300
                           "ROGRAM AS FOLLOWS:" //);                    00017400
FORMAT         NOFYL (// X31, "ERROR -- MISSING <PROGRAM SOURCE> IN <S",00017500
                              "CAN CONTROL DECK>.");                    00017600
FORMAT         NOSCAN (// X29, "ERROR -- NO <IDENTIFIER CARD>S APPEAR ",00017700
                               "IN <SCAN CONTROL DECK>.");              00017800
PROCEDURE      PRINTALINE;                                              00017900
          BEGIN                                                         00018000
  STREAM PROCEDURE ZONK (ARAY, LYNE);                                   00018100
            BEGIN                                                       00018200
                 SI ~ ARAY;                                             00018300
                 DI ~ LYNE;                                             00018400
                 DS ~ 20 LIT " ";                                       00018500
                 2(DS ~ 40 CHR);                                        00018600
                 DS ~ 20 LIT " ";                                       00018700
            END OF ZONK;                                                00018800
               ZONK(ARAY, LYNE);                                        00018900
               WRITE(LINE, 15, LYNE[*]);                                00019000
          END OF PRINTALINE;                                            00019100
REAL STREAM PROCEDURE MKABS (LOCN);                                     00019200
          BEGIN                                                         00019300
               DI ~ LOCN;                                               00019400
               MKABS ~ DI;                                              00019500
          END OF MKABS;                                                 00019600
STREAM PROCEDURE GETTER (SEQC, SEQA);                                   00019700
          BEGIN                                                         00019800
               SI ~ SEQC;                                               00019900
               DI ~ SEQA;                                               00020000
               DS ~ WDS;                                                00020100
               DI ~ SEQC;                                               00020200
               DS ~ LIT "%";                                            00020300
          END OF GETTER;                                                00020400
STREAM PROCEDURE SCAN (NCR, NCRV, CNT, CNTV, ACCUM, RSLT, RSLTV, AC);   00020500
     VALUE     NCRV, CNTV, RSLTV, AC;                                   00020600
          BEGIN                                                         00020700
     LOCAL     TMP, ST1;                                                00020800
     LABEL     DEBLANK, ALFA, NUMB, EXIT;                               00020900
               SI ~ NCRV;                                               00021000
               DI ~ RSLT;                                               00021100
               DS ~ 7 LIT "0";                                          00021200
               CI ~ CI+RSLTV;      % SWITCH ON VALUE OF RSLTV           00021300
               GO TO DEBLANK;      %  0 = SCAN FOR NEXT LOGICAL ENTITY  00021400
               GO TO ALFA;         %  1 = CONTINUE SCANNING ALPHA       00021500
               GO TO NUMB;         %  2 = CONTINUE SCANNING NUMBER      00021600
DEBLANK:                                                                00021700
               IF SC=" "  THEN                                          00021800
            BEGIN                                                       00021900
                 SI ~ SI+1;                                             00022000
                 GO TO DEBLANK;                                         00022100
            END;                                                        00022200
               NCRV ~ SI;                                               00022300
               IF SC=ALPHA  THEN                                        00022400
            BEGIN                                                       00022500
                 IF SC<"0"  THEN  GO TO ALFA;                           00022600
                 GO TO NUMB;                                            00022700
            END;                                                        00022800
                TALLY ~ 1;                                              00022900
                DS ~ LIT "3";                                           00023000
                GO TO EXIT;                                             00023100
ALFA:                                                                   00023200
               DS ~ LIT "1";                                            00023300
               TALLY ~ 63;                                              00023400
               AC(TALLY ~ TALLY + 1;                                    00023500
                  IF SC=ALPHA  THEN  SI~SI+1  ELSE  JUMP OUT TO EXIT);  00023600
               GO TO EXIT;                                              00023700
NUMB:                                                                   00023800
               DS ~ LIT "2";                                            00023900
               TALLY ~ 63;                                              00024000
               AC(TALLY ~ TALLY + 1;                                    00024100
                  IF SC}"0"  THEN  SI~SI+1  ELSE  JUMP OUT TO EXIT);    00024200
EXIT:                                                                   00024300
               ST1 ~ TALLY;                                             00024400
               TALLY ~ TALLY + CNTV;                                    00024500
               TMP ~ TALLY;                                             00024600
               SI ~ LOC TMP;                                            00024700
               SI ~ SI + 7;                                             00024800
               DI ~ ACCUM;                                              00024900
               DI ~ DI + 1;                                             00025000
               DS ~ CHR;                                                00025100
               SI ~ NCRV;                                               00025200
               DI ~ DI + CNTV;                                          00025300
               DS ~ ST1 CHR;                                            00025400
               ST1 ~ SI;                                                00025500
               SI ~ LOC TMP;                                            00025600
               DI ~ CNT;                                                00025700
               DS ~ WDS;                                                00025800
               SI ~ LOC ST1;                                            00025900
               DI ~ NCR;                                                00026000
               DS ~ WDS;                                                00026100
          END OF SCAN;                                                  00026200
STREAM PROCEDURE PLANT (ARAY, SEQ);                                     00026300
          BEGIN                                                         00026400
               SI ~ SEQ;                                                00026500
               DI ~ ARAY;                                               00026600
               DS ~ 8 DEC;                                              00026700
          END OF PLANT;                                                 00026800
PROCEDURE      READACARD;                                               00026900
          BEGIN                                                         00027000
               READ(SWF[FYLIN], 10, ARAY[*])[EOF];                      00027100
               IF SEQUENTS  THEN                                        00027200
            BEGIN                                                       00027300
                 SEQNO ~ SEQNO+1;                                       00027400
                 PLANT(ARAY[9], SEQNO);                                 00027500
            END;                                                        00027600
               IF PRINT  THEN  PRINTALINE;                              00027700
               GETTER(ARAY[9], OUDT[9]);                                00027800
               NCR ~ FCR;                                               00027900
          END OF READACARD;                                             00028000
PROCEDURE      SCANNER;                                                 00028100
          BEGIN                                                         00028200
     LABEL     L;                                                       00028300
               RSLT ~ CNT ~ ACCUM[0] ~ 0;                               00028400
L:                                                                      00028500
               SCAN(NCR, NCR, CNT, CNT, ACCUM, RSLT, RSLT, 63-CNT);     00028600
               IF NCR.[33:15]=LCR.[33:15]  THEN                         00028700
            BEGIN                                                       00028800
                 READACARD;                                             00028900
                 IF RSLT=3  THEN  RSLT~CNT~ACCUM[0]~0;                  00029000
                 GO TO L;                                               00029100
            END;                                                        00029200
          END OF SCANNER;                                               00029300
REAL STREAM PROCEDURE EXAMINE (NCR);                                    00029400
     VALUE     NCR;                                                     00029500
          BEGIN                                                         00029600
               SI ~ NCR;                                                00029700
               DI ~ LOC EXAMINE;                                        00029800
               DS ~ 7 LIT "0";                                          00029900
               DS ~ CHR;                                                00030000
          END OF EXAMINE;                                               00030100
PROCEDURE      STEPIT (NCR);                                            00030200
     REAL      NCR;                                                     00030300
          BEGIN                                                         00030400
  REAL STREAM PROCEDURE STEPP (NCR);                                    00030500
       VALUE     NCR;                                                   00030600
            BEGIN                                                       00030700
                 DI ~ NCR;                                              00030800
                 DI ~ DI + 1;                                           00030900
                 STEPP ~ DI;                                            00031000
            END OF STEPP;                                               00031100
               NCR ~ STEPP(NCR);                                        00031200
          END OF STEPIT;                                                00031300
STREAM PROCEDURE STUFFER (ACCUM, INFO, CNT);                            00031400
     VALUE     CNT;                                                     00031500
          BEGIN                                                         00031600
               SI ~ ACCUM;                                              00031700
               DI ~ INFO;                                               00031800
               DS ~ 2 CHR;                                              00031900
               DS ~ CNT CHR;                                            00032000
          END OF STUFFER;                                               00032100
PROCEDURE      ENTERIT;                                                 00032200
          BEGIN                                                         00032300
               INCR~((CNT-6)DIV 8)+(IF(CNT-6)MOD 8!0 THEN 3 ELSE 2);    00032400
               IF NDX1~NEXTINFO.LINKR+INCR>511  THEN                    00032500
                 NEXTINFO ~ NEXTINFO + (NDX1-511);                      00032600
               LASTINFO ~ NEXTINFO;                                     00032700
               NDX1 ~ ENTIER(ACCUM[0].[12:36] MOD 125);                 00032800
               IF NDX2~STACKHEAD[NDX1]!0  THEN                          00032900
                 INFO[NEXTINFO.LINKC,NEXTINFO.LINKR] ~ NDX2;            00033000
               STACKHEAD[NDX1] ~ NEXTINFO;                              00033100
               NDX1 ~ NEXTINFO.LINKC;                                   00033200
               NDX2 ~ NEXTINFO.LINKR;                                   00033300
               STUFFER(ACCUM, INFO[NDX1,NDX2+1], CNT);                  00033400
               NEXTINFO ~ NEXTINFO + INCR;                              00033500
          END OF ENTERIT;                                               00033600
BOOLEAN STREAM PROCEDURE SAME (ACCUM, INFO, CNT);                       00033700
     VALUE     CNT;                                                     00033800
          BEGIN                                                         00033900
               SI ~ ACCUM;                                              00034000
               SI ~ SI + 2;                                             00034100
               DI ~ INFO;                                               00034200
               DI ~ DI + 2;                                             00034300
               IF CNT SC=DC  THEN  TALLY ~ 1  ELSE  TALLY ~ 0;          00034400
               SAME ~ TALLY;                                            00034500
          END OF SAME;                                                  00034600
BOOLEAN PROCEDURE LOOKUP;                                               00034700
          BEGIN                                                         00034800
     LABEL     EXIT;                                                    00034900
               LOOKUP ~ FALSE;                                          00035000
               NDX1 ~ ENTIER(ACCUM[0].[12:36] MOD 125);                 00035100
               IF NDX2 ~ STACKHEAD[NDX1]!0  THEN                        00035200
            BEGIN                                                       00035300
                 IF CC  THEN                                            00035400
              BEGIN                                                     00035500
          COMMENT   FORCE LOOKUP DOWN INTO CONTROL-CARD RESERVED WORDS  00035600
                     IN CASE ANY USER ENTITIES HAVE THE SAME NAME  ;    00035700
                   IF NDX2}BASE  THEN                                   00035800
                     DO  NDX2~INFO[NDX2.LINKC,NDX2.LINKR].LINK          00035900
                       UNTIL  NDX2<BASE  OR  NDX2=0;                    00036000
                   IF NDX2=0  THEN  GO TO EXIT;                         00036100
              END;                                                      00036200
         DO BEGIN                                                       00036300
                 NDX1 ~ NDX2.LINKC;                                     00036400
                 NDX2 ~ NDX2.LINKR;                                     00036500
                 IF CNT=INFO[NDX1,NDX2+1].TCNT  THEN                    00036600
                   IF SAME(ACCUM, INFO[NDX1,NDX2+1], CNT)  THEN         00036700
              BEGIN                                                     00036800
                   LOOKUP ~ TRUE;                                       00036900
                   IF ELCLASS~INFO[NDX1,NDX2].CLSS=OTHERV  THEN         00037000
                     INFO[NDX1,NDX2].PRES ~ 1;                          00037100
                   GO TO EXIT;                                          00037200
              END;                                                      00037300
            END UNTIL  NDX2~INFO[NDX1,NDX2].LINK=0;                     00037400
            END;                                                        00037500
EXIT:                                                                   00037600
          END OF LOOKUP;                                                00037700
PROCEDURE      SCANCONTROL;                                             00037800
          BEGIN                                                         00037900
     LABEL     L;                                                       00038000
L:                                                                      00038100
               RSLT ~ CNT ~ ACCUM[0] ~ 0;                               00038200
               SCAN(NCR, NCR, CNT, CNT, ACCUM, RSLT, RSLT, 63-CNT);     00038300
               IF NOT LOOKUP  THEN  GO TO L;                            00038400
          END OF SCANCONTROL;                                           00038500
STREAM PROCEDURE ZOT (ARAY);                                            00038600
          BEGIN                                                         00038700
               DI ~ ARAY;                                               00038800
               2(DS ~ 36 LIT " ");                                      00038900
          END OF ZOT;                                                   00039000
STREAM PROCEDURE MOVE (ACCUM, OUDT, CNT);                               00039100
     VALUE     CNT;                                                     00039200
          BEGIN                                                         00039300
               SI ~ ACCUM;                                              00039400
               SI ~ SI + 2;                                             00039500
               DI ~ OUDT;                                               00039600
               DS ~ CNT CHR;                                            00039700
          END OF MOVE;                                                  00039800
PROCEDURE      SPITOUT;                                                 00039900
          BEGIN                                                         00040000
               ZOT(OUDT);                                               00040100
               MOVE(ACCUM, OUDT, CNT);                                  00040200
               WRITE(SRTIN, 10, OUDT[*]);                               00040300
               RECS ~ RECS + 1;                                         00040400
          END OF SPITOUT;                                               00040500
STREAM PROCEDURE ZOTTER (ARAY);                                         00040600
          BEGIN                                                         00040700
               DI ~ ARAY;                                               00040800
               2(DS ~ 60 LIT " ");                                      00040900
          END OF ZOTTER;                                                00041000
STREAM PROCEDURE MOVER (ARAY, OUDT, LYNE);                              00041100
          BEGIN                                                         00041200
               SI ~ ARAY;                                               00041300
               DI ~ OUDT;                                               00041400
               DS ~ 9 WDS;                                              00041500
               SI ~ ARAY;                                               00041600
               DI ~ LYNE;                                               00041700
               DS ~ 9 WDS;                                              00041800
          END OF MOVER;                                                 00041900
BOOLEAN STREAM PROCEDURE EQUAL (ARAY, OUDT);                            00042000
          BEGIN                                                         00042100
               SI ~ ARAY;                                               00042200
               DI ~ OUDT;                                               00042300
               IF 63 SC=DC  THEN  TALLY~1  ELSE  TALLY~0;               00042400
               EQUAL ~ TALLY;                                           00042500
          END OF EQUAL;                                                 00042600
PROCEDURE      PRINTWORD;                                               00042700
          BEGIN                                                         00042800
               ZOTTER(LYNE);                                            00042900
               MOVER(ARAY, OUDT, LYNE);                                 00043000
               WRITE(LINE, 15, LYNE[*]);                                00043100
               ZOTTER(LYNE);                                            00043200
               NCR ~ FCR;                                               00043300
               CTR ~ 0;                                                 00043400
          END OF PRINTWORD;                                             00043500
STREAM PROCEDURE PUTPUT (NCR, NCRV, SEQ);                               00043600
     VALUE     NCRV;                                                    00043700
          BEGIN                                                         00043800
               SI ~ SEQ;                                                00043900
               DI ~ NCRV;                                               00044000
               DS ~ 2 LIT " ";                                          00044100
               DS ~ 8 CHR;                                              00044200
               NCRV ~ DI;                                               00044300
               DI ~ NCR;                                                00044400
               SI ~ LOC NCRV;                                           00044500
               DS ~ WDS;                                                00044600
          END OF PUTPUT;                                                00044700
PROCEDURE      PUTTER;                                                  00044800
          BEGIN                                                         00044900
               PUTPUT(NCR, NCR, ARAY[9]);                               00045000
               IF CTR~CTR+1=12  THEN                                    00045100
            BEGIN                                                       00045200
                 WRITE(LINE, 15, LYNE[*]);                              00045300
                 ZOTTER(LYNE);                                          00045400
                 CTR ~ 0;                                               00045500
                 NCR ~ FCR;                                             00045600
            END;                                                        00045700
          END OF PUTTER;                                                00045800
STREAM PROCEDURE COLLECT (NCR, TMP, CTR);                               00045900
     VALUE     NCR, CTR;                                                00046000
          BEGIN                                                         00046100
               SI ~ NCR;                                                00046200
               DI ~ TMP;                                                00046300
               DI ~ DI + CTR;                                           00046400
               DS ~ CHR;                                                00046500
          END OF COLLECT;                                               00046600
BOOLEAN PROCEDURE GETID (TMP);                                          00046700
     REAL      TMP;                                                     00046800
          BEGIN                                                         00046900
     INTEGER   CTR;                                                     00047000
     LABEL     EXIT;                                                    00047100
               GETID ~ FALSE;                                           00047200
               CTR ~ 0;                                                 00047300
               WHILE EXAMINE(NCR)!"""  AND  CTR<7  DO                   00047400
                 IF NCR=LCR  THEN  GO TO EXIT  ELSE                     00047500
            BEGIN                                                       00047600
                 CTR ~ CTR+1;                                           00047700
                 COLLECT(NCR, TMP, CTR);                                00047800
                 STEPIT(NCR);                                           00047900
            END;                                                        00048000
               GETID ~ TRUE;                                            00048100
               WHILE EXAMINE(NCR)!"""  DO                               00048200
                 IF NCR=LCR  THEN  GO TO EXIT  ELSE  STEPIT(NCR);       00048300
               STEPIT(NCR);                                             00048400
EXIT:                                                                   00048500
          END OF GETID;                                                 00048600
PROCEDURE      GRABFYLID;                                               00048700
          BEGIN                                                         00048800
     REAL      MFID, FID, TMPNCR;                                       00048900
     INTEGER   NOIDS, TYPE;                                             00049000
     LABEL     STOPSCAN;                                                00049100
               NOIDS ~ 0;                                               00049200
               IF TYPE~(ELCLASS-CARDV)>0  THEN                          00049300
            BEGIN                                                       00049400
                 MFID ~ FID ~ "      ";                                 00049500
           DO BEGIN                                                     00049600
                   SCANCONTROL;                                         00049700
                   IF ELCLASS!QUOTEV  THEN  GO TO STOPSCAN;             00049800
                   IF GETID(FID)  THEN                                  00049900
                BEGIN                                                   00050000
                     IF NOIDS~NOIDS+1=2  THEN  GO TO STOPSCAN;          00050100
                     TMPNCR ~ NCR;                                      00050200
                     SCANCONTROL;                                       00050300
                     IF ELCLASS!SLASHV  THEN                            00050400
                  BEGIN                                                 00050500
                       NCR ~ TMPNCR;                                    00050600
                       GO TO STOPSCAN;                                  00050700
                  END;                                                  00050800
                     MFID ~ FID;                                        00050900
                     FID ~ "      ";                                    00051000
                END;                                                    00051100
              END UNTIL FALSE;                                          00051200
STOPSCAN:                                                               00051300
                 IF NOIDS!0  THEN                                       00051400
              BEGIN                                                     00051500
                   FYLIN ~ TYPE;                                        00051600
                   IF NOIDS=1  THEN  MFID~0;                            00051700
                   FILL SWF[FYLIN] WITH MFID, FID;                      00051800
              END ELSE  FYLIN~-1;                                       00051900
            END ELSE  FYLIN~TYPE;                                       00052000
          END OF GRABFYLID;                                             00052100
PROCEDURE      CONTROLCARD;                                             00052200
          BEGIN                                                         00052300
     LABEL     EXIT;                                                    00052400
               CC ~ TRUE;                                               00052500
               STEPIT(NCR);                                             00052600
         DO BEGIN                                                       00052700
                 SCANCONTROL;                                           00052800
                 IF ELCLASS=PCTV  THEN  GO TO EXIT;                     00052900
                 IF ELCLASS}CARDV  AND  ELCLASS{DISKV  THEN             00053000
                   GRABFYLID  ELSE                                      00053100
                   IF ELCLASS=SEQV  THEN  SEQUENCE~PRINT~TRUE  ELSE     00053200
                     IF ELCLASS=LISTV  THEN  PRINT~TRUE  ELSE           00053300
                       IF ELCLASS=LASTV  THEN  LAST~TRUE;               00053400
            END UNTIL  ELCLASS=PCTV;                                    00053500
EXIT:                                                                   00053600
               CC ~ FALSE;                                              00053700
          END OF CONTROLCARD;                                           00053800
STREAM PROCEDURE JAMMER (CNT, INFO, ACCUM, SEQ);                        00053900
     VALUE     CNT;                                                     00054000
          BEGIN                                                         00054100
               SI ~ INFO;                                               00054200
               SI ~ SI+1;                                               00054300
               DI ~ ACCUM;                                              00054400
               DI ~ DI+1;                                               00054500
               DS ~ CHR;                                                00054600
               DS ~ CNT CHR;                                            00054700
               DS ~ 18 LIT "  ***** NO ENTRIES";                        00054800
               DI ~ SEQ;                                                00054900
               DS ~ 8 LIT " ";                                          00055000
          END OF JAMMER;                                                00055100
PROCEDURE      WHATSTHESCOOP;                                           00055200
          BEGIN                                                         00055300
     INTEGER   I;                                                       00055400
               FOR I ~ 0  STEP 1  UNTIL 124  DO                         00055500
                 IF NDX1~STACKHEAD[I]!0  THEN                           00055600
         DO BEGIN                                                       00055700
                 NDX2 ~ NDX1.LINKR;                                     00055800
                 NDX1 ~ NDX1.LINKC;                                     00055900
                 IF INFO[NDX1,NDX2].CLSS=OTHERV                         00056000
                   AND  INFO[NDX1,NDX2].PRES=0  THEN                    00056100
              BEGIN                                                     00056200
                   JAMMER(CNT~INFO[NDX1,NDX2+1].TCNT, INFO[NDX1,NDX2+1],00056300
                          ACCUM, OUDT[9]);                              00056400
                   IF CNT~CNT+18>63  THEN  CNT~63;                      00056500
                   SPITOUT;                                             00056600
              END;                                                      00056700
            END UNTIL  NDX1~INFO[NDX1,NDX2].LINK=0;                     00056800
          END OF WHATSTHESCOOP;                                         00056900
PROCEDURE      OUTT (BOOL, A);                                          00057000
     VALUE     BOOL;                                                    00057100
     BOOLEAN   BOOL;                                                    00057200
     REAL ARRAY A[0];                                                   00057300
          BEGIN                                                         00057400
               IF BOOL  THEN  REWIND(SORTED)  ELSE                      00057500
                 WRITE(SORTED, 10, A[*]);                               00057600
          END OF OUTT;                                                  00057700
BOOLEAN PROCEDURE INN (A);                                              00057800
     REAL ARRAY A[0];                                                   00057900
          BEGIN                                                         00058000
     LABEL     EOF, EXIT;                                               00058100
               READ(SRTIN, 10, A[*])[EOF];                              00058200
               INN ~ FALSE;                                             00058300
               GO TO EXIT;                                              00058400
EOF:                                                                    00058500
               INN ~ TRUE;                                              00058600
               CLOSE(SRTIN, RELEASE);                                   00058700
EXIT:                                                                   00058800
          END OF INN;                                                   00058900
PROCEDURE      HIVAL (A);                                               00059000
     REAL ARRAY A[0];                                                   00059100
          BEGIN                                                         00059200
               FILL A[*] WITH                                           00059300
          OCT1414141414141414, OCT1414141414141414, OCT1414141414141414,00059400
          OCT1414141414141414, OCT1414141414141414, OCT1414141414141414,00059500
          OCT1414141414141414, OCT1414141414141414, OCT1414141414141414,00059600
          OCT1414141414141414;                                          00059700
          END OF HIVAL;                                                 00059800
BOOLEAN PROCEDURE COMP (A, B);                                          00059900
     REAL ARRAY A, B[0];                                                00060000
          BEGIN                                                         00060100
  BOOLEAN STREAM PROCEDURE CMP (A, B);                                  00060200
            BEGIN                                                       00060300
      LABEL EXIT;                                                       00060400
                 SI ~ A;                                                00060500
                 DI ~ B;                                                00060600
                 10(IF 8 SC!DC  THEN                                    00060700
                      BEGIN                                             00060800
                           SI ~ SI-8;  DI ~ DI-8;                       00060900
                           IF 8 SC<DC  THEN  TALLY~1  ELSE  TALLY~0;    00061000
                           JUMP OUT TO EXIT;                            00061100
                      END);                                             00061200
  EXIT:                                                                 00061300
                 CMP ~ TALLY;                                           00061400
            END OF CMP;                                                 00061500
               COMP ~ CMP(A, B);                                        00061600
          END OF COMP;                                                  00061700
               FCR ~ MKABS(ARAY[0]);                                    00061800
               LCR ~ MKABS(ARAY[9]);                                    00061900
               WRITE(LINE, INFOR);                                      00062000
               FYLIN ~ -1;                                              00062100
               FILL RSRVD[*] WITH                                       00062200
                 "01%00000",  1,                                        00062300
        OCT0001770000000000,  2,                                        00062400
                 "01/00000",  3,                                        00062500
                 "04CARD00",  4,                                        00062600
                 "04TAPE00",  5,                                        00062700
                 "04DISK00",  6,                                        00062800
                 "03SEQ000",  7,                                        00062900
                 "04LIST00",  8,                                        00063000
                 "04LAST00",  9;                                        00063100
               NEXTINFO ~ 1;                                            00063200
               FILL ACCUM[*] WITH                                       00063300
                 "07COMMEN", "T0000000";                                00063400
               CNT ~ 7;                                                 00063500
               ENTERIT;                                                 00063600
               INFO[LASTINFO.LINKC,LASTINFO.LINKR].CLSS ~ COMMENTV;     00063700
               FOR RSLT~0  STEP 2  UNTIL 16  DO                         00063800
            BEGIN                                                       00063900
                 CNT~(ACCUM[0]~RSRVD[RSLT]).[6:6];                      00064000
                 ENTERIT;                                               00064100
                 INFO[LASTINFO.LINKC,LASTINFO.LINKR].CLSS~RSRVD[RSLT+1];00064200
            END;                                                        00064300
               BASE ~ NEXTINFO;                                         00064400
               SEQUENCE ~ SEQUENTS ~ PRINT ~ LAST ~ FALSE;              00064500
         DO BEGIN                                                       00064600
                 READ(CARD, 10, ARAY[*])[EOC];                          00064700
                 PRINTALINE;                                            00064800
                 GETTER(ARAY[9], OUDT[9]);                              00064900
                 NCR ~ FCR;                                             00065000
                 IF EXAMINE(NCR)="$"  THEN  CONTROLCARD  ELSE           00065100
              BEGIN                                                     00065200
                   SCANNER;                                             00065300
                   ENTERIT;                                             00065400
              END;                                                      00065500
            END UNTIL LAST;                                             00065600
EOC:                                                                    00065700
               IF FYLIN<0  THEN                                         00065800
            BEGIN                                                       00065900
                 WRITE(LINE, NOFYL);                                    00066000
                 GO TO FINISH;                                          00066100
            END;                                                        00066200
               IF NEXTINFO = BASE  THEN                                 00066300
            BEGIN                                                       00066400
                 WRITE(LINE, NOSCAN);                                   00066500
                 GO TO FINISH;                                          00066600
            END;                                                        00066700
               IF FYLIN!0  THEN  CLOSE(CARD, RELEASE);                  00066800
               IF PRINT  THEN                                           00066900
            BEGIN                                                       00067000
                 WRITE(LINE[PAGE]);                                     00067100
                 WRITE(LINE, LSTHD);                                    00067200
            END;                                                        00067300
               SEQUENTS ~ SEQUENCE;                                     00067400
               SEQNO ~ 0;                                               00067500
               READACARD;                                               00067600
               RECS ~ 0;                                                00067700
         DO BEGIN                                                       00067800
                 SCANNER;                                               00067900
                 IF RSLT=3  THEN                                        00068000
              BEGIN                                                     00068100
                   IF CNT~ACCUM[0].[12:6]="%"  THEN  READACARD  ELSE    00068200
                     IF CNT="""  THEN                                   00068300
                  BEGIN                                                 00068400
                       IF NCR=LCR  THEN  READACARD;                     00068500
                       STEPIT(NCR);                                     00068600
                       WHILE EXAMINE(NCR)!"""  DO                       00068700
                         IF NCR=LCR  THEN  READACARD  ELSE  STEPIT(NCR);00068800
                       STEPIT(NCR);                                     00068900
                  END;                                                  00069000
              END ELSE                                                  00069100
                   IF LOOKUP  THEN                                      00069200
              BEGIN                                                     00069300
                   IF ELCLASS=OTHERV  THEN  SPITOUT  ELSE               00069400
                     IF ELCLASS=COMMENTV  THEN                          00069500
                       WHILE EXAMINE(NCR)!";"  DO                       00069600
                         IF NCR=LCR  THEN  READACARD  ELSE  STEPIT(NCR);00069700
              END;                                                      00069800
            END UNTIL FALSE;                                            00069900
EOF:                                                                    00070000
               CLOSE(SWF[FYLIN], RELEASE);                              00070100
               WHATSTHESCOOP;                                           00070200
               REWIND(SRTIN);                                           00070300
               IF RECS!0  THEN                                          00070400
                 SORT(OUTT,INN,TAPES,HIVAL,COMP,10,CORE,DISC)  ELSE     00070500
                   GO TO FINISH;                                        00070600
               WRITE(LINE[PAGE]);                                       00070700
               WRITE(LINE, SKOWT);                                      00070800
               CTR ~ 0;                                                 00070900
               FCR ~ MKABS(LYNE[0]);                                    00071000
               NCR ~ FCR;                                               00071100
               READ(SORTED, 10, ARAY[*]);                               00071200
               PRINTWORD;                                               00071300
               PUTTER;                                                  00071400
         DO BEGIN                                                       00071500
                 READ(SORTED, 10, ARAY[*])[EOS];                        00071600
                 IF EQUAL(ARAY, OUDT)  THEN  PUTTER  ELSE               00071700
              BEGIN                                                     00071800
                   IF CTR!0  THEN                                       00071900
                BEGIN                                                   00072000
                     CTR ~ 0;                                           00072100
                     WRITE(LINE, 15, LYNE[*]);                          00072200
                END;                                                    00072300
                   WRITE(LINE);                                         00072400
                   WRITE(LINE);                                         00072500
                   PRINTWORD;                                           00072600
                   PUTTER;                                              00072700
              END;                                                      00072800
            END UNTIL FALSE;                                            00072900
EOS:                                                                    00073000
               IF CTR!0  THEN                                           00073100
                 WRITE(LINE, 15, LYNE[*]);                              00073200
FINISH:                                                                 00073300
          END.                                                          00073400
000100 REMARKS  PROGRAM SRTPERM, CUBE LIBRARY NUMBER IS M100002.        00073500
000200 REMARKS THIS VERSION DATED 06/01/67.                             00073600
000300 IDENTIFICATION DIVISION.                                         00073700
000400 PROGRAM-ID. "SRTPERM".                                           00073800
000500 ENVIRONMENT DIVISION.                                            00073900
000600 CONFIGURATION SECTION.                                           00074000
000700 SOURCE-COMPUTER. B-5000.                                         00074100
000800 OBJECT-COMPUTER. B-5000 MEMORY SIZE  4000  WORDS 4 TAPES.        00074200
000900 INPUT-OUTPUT SECTION.                                            00074300
001000 FILE-CONTROL.                                                    00074400
001100     SELECT INPUT-FILE ASSIGN TO TAPE.                            00074500
001200     SELECT SORT-FILE ASSIGN TO 3 SORT-TAPES.                     00074600
001300     SELECT OUTPUT-FILE ASSIGN TO TAPE.                           00074700
001400 I-O-CONTROL.                                                     00074800
001500     APPLY TECHNIQUE-A ON INPUT-FILE                              00074900
001600     APPLY TECHNIQUE-A  ON OUTPUT-FILE.                           00075000
001700 DATA DIVISION.                                                   00075100
001800 FILE SECTION.                                                    00075200
001900 FD  INPUT-FILE                                                   00075300
002000     RECORDING MODE IS STANDARD BLOCK CONTAINS 1 RECORDS          00075400
