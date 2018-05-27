          BEGIN                                                         00000100
COMMENT        PROGRAM INDEXER, CUBE LIBRARY NUMBER IS Q000001.         00000200
               THIS VERSION DATED 6/1/67,                               00000300
               A STACK ADJUSTMENT CARD SET EQUAL TO 1000 MUST BE USED   00000400
               AT COMPILE TIME;                                         00000500
COMMENT                                                                 00000600
                              I N D E X E R                             00000700
                                                                        00000800
               A PROGRAM TO PROVIDE AN INDEX OF THE IDENTIFIERS         00000900
                         USED IN AN ALGOL PROGRAM                       00001000
                                                                        00001100
                    WRITTEN BY:    ALAN BATSON                          00001200
                                   C-S CENTER                           00001300
                              UNIVERSITY OF VIRGINIA                    00001400
                                                                        00001500
DISCLAIMER:  THE AUTHORS OF THIS PROGRAM, THE CUBE ORGANIZATION,        00001600
     AND THE BURROUGHS CORPORATION, BELIEVE THIS PROGRAM MATERIAL       00001700
     TO BE CORRECT, HOWEVER, THEY BEAR NO RESPONSIBILITY,               00001800
     FINANCIAL OR OTHERWISE, FOR ERROR RESULTING FROM ITS USE, NOR      00001900
     ACCEPT ANY RESPONSIBILITY FOR ITS MAINTENANCE.                     00002000
THIS PROGRAM PRODUCES AN INDEX (DICTIONARY) OF ALL THE IDENTIFIERS USED 00002100
IN AN ALGOL PROGRAM.  THE INDEX IS IN ALPHABETICAL ORDER (ON THE FIRST  00002200
6 CHARACTERS OF EACH IDENTIFIER), AND NEARLY ALL OF THE RESERVED WORDS  00002300
ARE OMITTED.                                                            00002400
     AFTER EACH IDENTIFIER IS PRINTED THE SEQUENCE NUMBER (COLUMNS 73-8000002500
) OF THE CARDS ON WHICH THAT IDENTIFIER OCCURS.  IF AN IDENTIFIER OCCURS00002600
TWICE ON THE SAME CARD, THAT CARD-S SEQUENCE NUMBER WILL APPEAR TWICE.  00002700
THE SEQUENCE NUMBERS ARE PRINTED OUT IN THE ORDER IN WHICH THEY OCCUR   00002800
IN THE DECK ITSELF.                                                     00002900
INPUT:                                                                  00003000
     THE PROGRAM MAY BE EITHER ON CARDS OR ON AN ALGOL SYMBOLIC TAPE    00003100
LABELLED "0CRDIMG", WHICH IS BLOCKED 5 CARDS PER PHYSICAL RECORD.  THE  00003200
CARD READER FILE IS CALLED "CR", AND THE FIRST CARD IN THIS FILE MUST   00003300
BE A DOLLAR CARD OF THE TYPE                                            00003400
                    $ CARD  OR   $ TAPE                                 00003500
OF COURSE, IF THE DATA IS ON TAPE, THIS IS THE ONLY DATA CARD.          00003600
TIMING AND LIMITATIONS:                                                 00003700
     AS AN EXAMPLE, A 1000 CARD PROGRAM , COMING FROM CARDS, TOOK 200   00003800
SECONDS OF PROCESSOR TIME AND 104 SECONDS OF I/O TIME.  THE TOTAL TIME  00003900
IS NOT GREATLY DEPENDENT ON THE INPUT MEDIUM.                           00004000
     THE PROGRAM WILL ONLY HANDLE UP TO 935 IDENTIFIERS.  AFTER THIS    00004100
POINT HAS BEEN REACHED IN A PROGRAM, THE EXCESS IDENTIFIERS WHICH WILL  00004200
NOT BE INDEXED ARE PRINTED OUT EACH TIME THEY OCCUR, TOGETHER WITH THE  00004300
CARD SEQUENCE NUMBER.  IF THERE ARE A VERY LARGE NUMBER OF VERY LONG    00004400
IDENTIFIERS, THEN SOME OF THESE MAY BE KEPT TO ONLY 6 CHARACTERS, AND   00004500
ON THE PRINTOUT THE MISSING CHARACTERS ARE INDICATED BY /-S.            00004600
     THE PROGRAM HAS SUCCESSFULLY INDEXED BOTH THE ALGOL COMPILER AND   00004700
THE FORTRAN-TO-ALGOL TRANSLATOR.                                        00004800
NOTE:  THIS PROGRAM NEEDS A COMPILE-TIME STACK ADJUSTMENT.  1000 WORDS  00004900
WORK FINE.                                                              00005000
                                                                        00005100
                                                                        00005200
                                        ;                               00005300
FILE CR(2,10); FILE LP 4 (2,15);    COMMENT CARDS AND PRINTER;          00005400
INTEGER COUNTER;                        COMMENT THIS COUNTS THE NUMBER  00005500
                                        OF RECORDS WRITTEN ONTO MT1;    00005600
FORMAT XXX(X15, 12("*",X6));                                            00005700
INTEGER SIZE;                           COMMENT NUMBER OF CLOT ROWS-1;  00005800
INTEGER STCOUNT;                        COMMENT THE NUMBER OF NAMES;    00005900
FILE MT11(1,500);                                                       00006000
LABEL FINALLABEL;                                                       00006100
STREAM PROCEDURE MOVE(FROM,UNTO,N);                                     00006200
VALUE N;                                                                00006300
BEGIN                                                                   00006400
     SI~FROM; DI~UNTO; DS~ N WDS;                                       00006500
END OF MOVE;                                                            00006600
FORMAT ERRORFORMAT1(X10,"YOU HAVE MORE THAN  935 IDENTIFIERS. SORRY."/  00006700
               "HERE ARE THE NON-INDEXED IDENTIFIERS"/);                00006800
FORMAT ERRORFORMAT2(X10,"TOO MANY OCCURRENCES OF A NAME. SORRY");       00006900
FORMAT ERRORFORMAT3(X10,"INCORRECT DOLLAR CARD");                       00007000
ARRAY SYMBOL0,SYMBOL1[0:1022];          COMMENT THE SYMBOL TABLE;       00007100
DEFINE NAME=SYMBOL0[ALFA]#, IDEN= SYMBOL1[ALFA]#;                       00007200
ARRAY CONTINUE[0:2,0:1022];             COMMENT FOR NAME EXTENSIONS;    00007300
INTEGER C1,C2;                          COMMENT THESE POINT TO CONTINUE;00007400
BOOLEAN T;                              COMMENT TRUE IF CONTINUE UNFULL;00007500
BOOLEAN OVER;                           COMMENT ON ONCE SYMBOL HAS      00007600
                                              BEEN EXCEEDED;            00007700
     ARRAY LINE[0:14];                  COMMENT THE PRINTER ARRAY;      00007800
     STREAM PROCEDURE BLANKLINE(LINE);                                  00007900
     BEGIN                                                              00008000
          DI~LINE;15(DS~8 LIT"        ");                               00008100
     END OF BLANKLINE;                                                  00008200
          STREAM PROCEDURE GET(FROM,UNTO,POS);VALUE POS;                00008300
          BEGIN                                                         00008400
          SI~FROM;SI~SI+2;                                              00008500
          DI~UNTO;DI~DI+POS;                                            00008600
          6(IF SC="+" THEN JUMP OUT ELSE DS~1 CHR);                     00008700
          END;                                                          00008800
INTEGER I,J,K,INDEX;                                                    00008900
ARRAY BUFF[0:499];                      COMMENT THE TAPE BUFFER;        00009000
SIZE~49;                                                                00009100
                                                                        00009200
     BEGIN                              COMMENT THE INNER BLOCK         00009300
                                        FOR PASS ONE;                   00009400
    INTEGER N,POSI,DELT,M,FRONT,BACK,CHRCOUNT,J,L,NEXT,INP;             00009500
     BOOLEAN T1, DELTA;                                                 00009600
     ARRAY CARDNO[0:0];                                                 00009700
     ARRAY INFO[0:9];                   COMMENT THE CARD BUFFER;        00009800
     LABEL EOP1, TOPOFHEAP, COMMENTDELETE,L1,L2,L3,L4,START;            00009900
     ARRAY TRANSLATE[0:63];             COMMENT THE CODE FOR A CHAR.;   00010000
     COMMENT   I POINTS TO THE NEXT PLACE TO LOOK,                      00010100
               FRONT POINTS TO THE FIRST CHARACTER OF A FOUND NAME,     00010200
               INDEX POINTS TO THE NEXT AVAILABLE SPACE IN BUFF;        00010300
     FILE IN SMOG "0CRDIMG"(2,56,10);   COMMENT THE SYMBOLIC TAPE;      00010400
     SWITCH FILE INPUT~CR,SMOG;         COMMENT CARD OR TAPE;           00010500
     BOOLEAN RES;                                                       00010600
     ARRAY ARY[1:26];          COMMENT COUNTS NO. OF AS,BS ETC;         00010700
     BOOLEAN STREAM PROCEDURE FMT(I,N,INFO);                            00010800
     VALUE I,N;                                                         00010900
     BEGIN   LOCAL T;                                                   00011000
          SI~INFO;N(SI~SI+63;SI~SI+1); SI~SI+I; TALLY~0;                00011100
          IF SC="F" THEN  BEGIN  DI~LOC T; DS~ 7 LIT "FORMAT ";         00011200
                                   DI~DI-7; IF 7 SC=DC THEN             00011300
                                   TALLY~ 1 END;                        00011400
               FMT~ TALLY                                               00011500
     END OF FMT;                                                        00011600
                                                                        00011700
     BOOLEAN STREAM PROCEDURE COM(I,N,INFO);                            00011800
     VALUE I,N;                                                         00011900
     BEGIN  LOCAL T;                                                    00012000
          SI~INFO;N(SI~SI+63;SI~SI+1); SI~SI+I; TALLY~0;                00012100
          IF SC="C" THEN BEGIN DI~LOC T; DS~ 8 LIT "COMMENT ";          00012200
                                   DI~DI-8; IF 8 SC=DC  THEN            00012300
                                        TALLY~1  END;                   00012400
          COM~TALLY                                                     00012500
     END OF COM;                                                        00012600
     BOOLEAN STREAM PROCEDURE NOTDOLLAR(INFO);                          00012700
     BEGIN   SI~INFO; TALLY ~ 1; IF SC="$" THEN TALLY~0;                00012800
          NOTDOLLAR~TALLY;                                              00012900
     END OF NOTDOLLAR;                                                  00013000
     BOOLEAN STREAM PROCEDURE O8(I,N,INFO);  VALUE I,N;                 00013100
     BEGIN      LOCAL T;                                                00013200
          SI~INFO; N(SI~SI+63; SI~SI+1); SI~SI+I; TALLY~0;              00013300
          IF SC ="O" THEN BEGIN DI~LOC T; DS~ 3 LIT "OCT";              00013400
                              DI~DI-3; IF 3 SC=DC THEN                  00013500
                                   BEGIN IF SC{"9" THEN TALLY~1 END;    00013600
                              END;                                      00013700
          O8~TALLY                                                      00013800
     END;                                                               00013900
     PROCEDURE READACARD;                                               00014000
     BEGIN                                                              00014100
          STREAM PROCEDURE CARDID(C73, CARDNO);                         00014200
          BEGIN                                                         00014300
               SI~C73; DI~CARDNO; DS~ 8 CHR;                            00014400
               DI~C73; DS~ 1 LIT "%";                                   00014500
          END;                                                          00014600
     READ(INPUT[INP],10,INFO[*])[EOP1];                                 00014700
     CARDID(INFO[9], CARDNO);                                           00014800
     I~0;                                                               00014900
     END;                                                               00015000
     PROCEDURE LOOK;  COMMENT DOES NOT LOOK FOR PERCENTS;               00015100
     BEGIN                                                              00015200
          ALPHA STREAM PROCEDURE LOK(I,N,INFO);                         00015300
          VALUE I,N;                                                    00015400
          BEGIN                                                         00015500
               SI~INFO;N(SI~SI+63;SI~SI+1); SI~SI+I;                    00015600
               DI~LOC LOK; DI~DI+7; DS~ 1 CHR;                          00015700
          END;                                                          00015800
          LABEL TOP;                                                    00015900
          TOP: IF I=72 THEN READACARD;                                  00016000
          NEXT~LOK(I,I DIV 64, INFO);                                   00016100
          I~I+1;                                                        00016200
     END;                                                               00016300
     PROCEDURE FETCHANAME;                                              00016400
     BEGIN                                                              00016500
          STREAM PROCEDURE FETCHITBOY(I,N,POSI,DELT ,INFO,MARK);        00016600
          VALUE I,N;                                                    00016700
          BEGIN   LOCAL TEMP,TEMP1;                                     00016800
               LABEL DOWN,ENDOFCARD,GOTIT,HORROR,NOGO,STRING,FIN;       00016900
               SI~INFO;N(SI~SI+63;SI~SI+1);SI~SI+I; TALLY~0;            00017000
               63( IF SC=ALPHA THEN JUMP OUT 1 TO DOWN                  00017100
                              ELSE                                      00017200
                              IF SC="%" THEN JUMP OUT 1 TO ENDOFCARD;   00017300
                    IF SC=""" THEN JUMP OUT 1 TO STRING;                00017400
                    SI~SI+1; TALLY~TALLY +1);                           00017500
               DI~MARK; DS~8 LIT"00000000";GO TO NOGO;                  00017600
                                        COMMENT FOUND NOTHING;          00017700
               DOWN:TEMP1~TALLY;                                        00017800
               COMMENT TEMP1 NOW CONTAINS THE NUMBER OF CHARACTERS      00017900
                         PAST I WHERE STARTS THE ALPHA STRING;          00018000
               SI~SI+1; TALLY ~1;                                       00018100
               62(IF SC=ALPHA THEN BEGIN                                00018200
                                   SI~SI+1; TALLY~TALLY +1;             00018300
                                   END  ELSE                            00018400
                              BEGIN                                     00018500
                                   IF SC= "%" THEN JUMP OUT 1 TO        00018600
                                   HORROR; JUMP OUT 1 TO GOTIT;         00018700
                                   END );                               00018800
               COMMENT NOTE THAT IF AN IDENTIFIER HAS MORE THAN         00018900
                         63 CHARACTERS WE DROP OUT AND SPLIT THE        00019000
                         IDENTIFIER IN TWO(OR MORE);                    00019100
               GOTIT: DI~MARK; DS~ 8 LIT "00000001";                    00019200
               TEMP~ TALLY;SI~ LOC TEMP; DI~ DELT ; DS~ 1 WDS;          00019300
               COMMENT DELT  NOW CONTAINS THE NUMBER OF CHARACTERS      00019400
                         IN THE ALPHA STRING;                           00019500
               GO TO NOGO;                                              00019600
               ENDOFCARD:  DI~MARK; DS~ 8 LIT "00000002"; GO TO NOGO;   00019700
               HORROR:  GO TO GOTIT;    COMMENT THIS USELESS *$*$*$*$   00019800
                                        IS FOLLOWED WHEN NAMES          00019900
                                        ARE SPLIT ACROSS CARDS. WE      00020000
                                        TREAT THIS AS TWO NAMES;        00020100
              STRING:  DI~MARK; DS~ 8 LIT "00000004";                   00020200
              TEMP~TALLY; SI~LOC TEMP; DI~POSI; DS~ 1 WDS; GO TO FIN;   00020300
               NOGO:                                                    00020400
                    SI~LOC TEMP1; DI~POSI; DS~ 1 WDS;                   00020500
     FIN: END OF FETCHITBOY;                                            00020600
     COMMENT NOTE THAT FETCHITBOY COMES OUT WITH 4 VALUES POSSIBLE      00020700
               FOR MARK. 1 IS O.K. BUT THE OTHER(EVEN) ONES MEAN THINK; 00020800
     LABEL TOP, EXIT, STRINGCUTTER, WOP;                                00020900
     TOP:  FETCHITBOY(I,I DIV 64,    POSI, DELT, INFO, M);              00021000
     IF BOOLEAN(M) THEN GO TO  EXIT ELSE                                00021100
               BEGIN                                                    00021200
                         IF M=0 THEN I~I+63  ELSE                       00021300
                              IF M=2 THEN READACARD  ELSE               00021400
                                   GO TO STRINGCUTTER;                  00021500
                         GO TO TOP                                      00021600
                    END;                                                00021700
     STRINGCUTTER:  I~I+POSI+1;                                         00021800
                    LOOK; IF NEXT=""" THEN  BEGIN                       00021900
                              I~I+1; GO TO TOP                          00022000
                                   END;                                 00022100
          WOP:  LOOK; IF NEXT=""" THEN GO TO TOP                        00022200
                                   ELSE GO TO WOP;                      00022300
     EXIT: FRONT~I+POSI; CHRCOUNT~DELT ; I~FRONT+CHRCOUNT;              00022400
     END OF FETCHANAME WHICH MUST FOLLOW LOOK AND READACARD;            00022500
     BOOLEAN STREAM PROCEDURE PEEK(FRONT,N,INFO);                       00022600
     VALUE FRONT,N;                                                     00022700
     BEGIN    LABEL DOWN;                                               00022800
          SI~INFO; N(SI~SI+63;SI~SI+1);SI~SI+FRONT;TALLY~1;             00022900
          IF SB THEN GO TO DOWN;                                        00023000
          SKIP 1 SB;                                                    00023100
          IF SB THEN GO TO DOWN;                                        00023200
          TALLY~0;                                                      00023300
          DOWN: PEEK~TALLY;                                             00023400
     END OF PEEK;                                                       00023500
     PROCEDURE FORMATPROCESSOR;                                         00023600
     BEGIN                                                              00023700
          INTEGER Q,LR;                                                 00023800
          LABEL A1,A2,A3,A4,A;                                          00023900
          Q~LR~0;                                                       00024000
     A1:  LOOK; IF NEXT !"(" THEN GO TO A1;                             00024100
     A2:  LR~LR+1;                                                      00024200
     A:   LOOK; IF NEXT=""" THEN   BEGIN                                00024300
                    IF Q=0 THEN    BEGIN                                00024400
                         LOOK; IF NEXT=""" THEN BEGIN                   00024500
                              LOOK; Q~0          END                    00024600
                              ELSE Q~1; GO TO A;                        00024700
                              END;                                      00024800
                         Q~0; GO TO A                                   00024900
                              END;                                      00025000
          IF NEXT="(" THEN BEGIN                                        00025100
                    IF Q=0 THEN GO TO A2 ELSE GO TO A                   00025200
                              END;                                      00025300
          IF NEXT !")" THEN GO TO A;                                    00025400
          IF Q !0 THEN GO TO A;                                         00025500
          LR~LR-1; IF LR ! 0 THEN GO TO A;                              00025600
     A3:  LOOK; IF NEXT=";" THEN BEGIN T1~FALSE; GO TO A4 END;          00025700
          IF NEXT ! "," THEN GO TO A3;                                  00025800
     A4:                                                                00025900
     END OF  THE FORMATPROCESSOR;                                       00026000
     PROCEDURE LOOKUP(A,I);                                             00026100
     VALUE I; ARRAY A[0]; INTEGER I;                                    00026200
     BEGIN                                                              00026300
          INTEGER COUNT,C,WDCNT,L,M;  OWN INTEGER ALFA;                 00026400
          LABEL TOP, BOTTOM, NOTIT, FOUND;                              00026500
          PROCEDURE INSERT(A,I);                                        00026600
          VALUE I; ARRAY A[0]; INTEGER I;                               00026700
          BEGIN   LABEL DOWN, RESS;                                     00026800
                    PROCEDURE PIN(A,I);                                 00026900
                    VALUE I; ARRAY A[0]; INTEGER I;                     00027000
                    BEGIN    INTEGER W;                                 00027100
                    NAME~A[I];                                          00027200
                    W~A[I-1].[36:6];                                    00027300
                    IF W!1 AND NOT T THEN  IDEN.[24:3]~7;               00027400
                    IF W!1 AND T THEN   BEGIN                           00027500
                         IDEN.[37:1]~1;                                 00027600
                         MOVE(A[I+1],CONTINUE[C1,C2],W-1);              00027700
                         IDEN.[24:3]~C1.[45:3];                         00027800
                         IDEN.[27:10]~C2.[38:10];                       00027900
                         C2~C2+W-1;                                     00028000
                         IF C2}1009  THEN  BEGIN                        00028100
                                        C1~C1+1; C2~0;                  00028200
                                        IF C1=3 THEN T~FALSE            00028300
                                        END;                            00028400
                    END;                                                00028500
                    IDEN.[38:10]~1;                                     00028600
                    IDEN.[12:6]~W;                                      00028700
                    END OF PIN;                                         00028800
               IF DELTA THEN   BEGIN                                    00028900
                    PIN(A,I); IDEN.[18:6]~63; GO TO RESS                00029000
                              END;                                      00029100
               IF NAME=0 THEN BEGIN                                     00029200
                    J~TRANSLATE[A[I].[12:6]];                           00029300
                    PIN(A,I);                                           00029400
                    IDEN.[18:6]~J;                                      00029500
                    STCOUNT~STCOUNT+1;                                  00029600
                    GO TO DOWN    END;                                  00029700
               IF IDEN.[18:6]=63 THEN GO TO RESS;                       00029800
               IDEN.[38:10]~IDEN.[38:10]+1;                             00029900
               GO TO DOWN;                                              00030000
          RESS:     RES~TRUE;                                           00030100
          DOWN:     END OF INSERT;                                      00030200
          PROCEDURE PRINTIT(A,I); VALUE I; ARRAY A[0]; INTEGER I;       00030300
      BEGIN                                                             00030400
           INTEGER W,X,X2;                                              00030500
           W~A[I-1].[36:6];  BLANKLINE(LINE); X2~6|(W-1); W~0 ;         00030600
           FOR X~ 0 STEP 6 UNTIL X2 DO  BEGIN                           00030700
           GET(A[I+W], LINE,X);   W~W+1  END;                           00030800
           MOVE(CARDNO, LINE[10], 1);                                   00030900
           WRITE(LP,15,LINE[*]);                                        00031000
      END OF PRINTIT;                                                   00031100
          ALFA~(A[I].[12:6]-17)|25; ALFA~ALFA+ABS(A[I].[18:6]-17)|2;    00031200
          IF ALFA>1022 THEN ALFA~1000;                                  00031300
          COUNT~0; RES~FALSE;                                           00031400
     TOP: IF NOT DELTA  THEN                                            00031500
          BEGIN                                                         00031600
               IF A[I]=NAME THEN BEGIN                                  00031700
                    C~IDEN.[37:1]; WDCNT~A[I-1].[36:6];                 00031800
                    IF C=0 THEN BEGIN                                   00031900
                         IF WDCNT=1 THEN GO TO FOUND;                   00032000
                         IF IDEN.[24:3]=7 THEN GO TO FOUND              00032100
                                                 ELSE GO TO NOTIT       00032200
                                   END;                                 00032300
                    L~IDEN.[24:3];  M~IDEN.[27:10];                     00032400
                    FOR J~ 0 STEP 1 UNTIL (WDCNT-2) DO BEGIN            00032500
                         IF CONTINUE[L,M+J]!A[I+1+J]  THEN              00032600
                                        GO TO NOTIT   END;              00032700
                         GO TO FOUND    END;                            00032800
          END;                                                          00032900
     NOTIT:    IF NAME=0 THEN BEGIN                                     00033000
                              INSERT(A,I); GO TO BOTTOM;                00033100
                                        END;                            00033200
          ALFA~ALFA+1;                                                  00033300
          COUNT~COUNT+1;                                                00033400
          IF ALFA>1022 THEN ALFA~0;                                     00033500
          IF COUNT=1022 THEN  BEGIN                                     00033600
                    IF NOT OVER THEN BEGIN                              00033700
                         OVER~ TRUE; WRITE(LP,ERRORFORMAT1);   END;     00033800
                    PRINTIT(A,I); RES~ TRUE; GO TO BOTTOM;              00033900
                              END;                                      00034000
          GO TO TOP;                                                    00034100
     FOUND:    IF IDEN.[18:6]=63 THEN BEGIN                             00034200
                    RES~TRUE; GO TO BOTTOM    END;                      00034300
          INSERT(A,I);                                                  00034400
     BOTTOM:                                                            00034500
          END OF LOOKUP;                                                00034600
                                                                        00034700
     PROCEDURE STFILL;                                                  00034800
     BEGIN                                                              00034900
          ARRAY R[0:500];                                               00035000
          FOR J~0 STEP 3 UNTIL 231 DO R[J]~"10";                        00035100
          FOR J~0 STEP  3 UNTIL 231 DO R[J+2]~"      ";   I~0;          00035200
          FOR J~"ABS+++","ALPHA+","AND+++","ARCTAN","ARRAY+","ADD+++",  00035300
               "BEGIN+","CHR+++","CI++++","COS+++","DBL+++","DB++++",   00035400
               "DC++++","DEC+++","DEFINE","DI++++","DIV+++","DO++++",   00035500
               "DS++++","DUMP++","ELSE++","END+++","ENTIER","EQV+++",   00035600
               "EXP+++","FALSE+","FILE++","FILL++","FOR+++","GO++++",   00035700
               "IF++++","IMP+++","IN++++","JUMP++","LABEL+","LIST++",   00035800
               "LIT+++","LN++++","LOC+++","LOCAL+","MOD+++","NO++++",   00035900
               "NOT+++","NUM+++","OCT+++","OR++++","OUT+++","OWN+++",   00036000
               "PAGE++","READ++","REAL++","RESET+","SAVE++","SB++++",   00036100
               "SC++++","SET+++","SI++++","SIGN++","SIN+++","SKIP++",   00036200
               "SQRT++","STEP++","STREAM","SUB+++","SWITCH","TALLY+",   00036300
               "THEN++","TIME++","TO++++","TOGGLE","TRUE++","UNTIL+",   00036400
               "VALUE+","WDS+++","WHILE+","WITH++","WRITE+","ZON+++"    00036500
                              DO BEGIN                                  00036600
                                   R[I+1]~J;                            00036700
                                   I~I+3                                00036800
                                   END;                                 00036900
          COMMENT THATS THE SINGLE WORD ENTRIES;                        00037000
          FOR J~0 STEP 3 UNTIL 18 DO  R[J+234]~"20";   I~0;             00037100
          FOR J~"BOOLEA","N+++++","FORWAR","D+++++",                    00037200
                    "INTEGE","R+++++","MONITO","R+++++",                00037300
                    "PROCED","URE+++","RELEAS","E+++++",                00037400
                    "REVERS","E+++++"                                   00037500
                              DO BEGIN                                  00037600
                                   R[I+235]~J;                          00037700
                                   I~I+1;   IF I MOD 3=2 THEN I~I+1;    00037800
                                   END;                                 00037900
          COMMENT THATS THE OTHERS INTO THE ARRAY;                      00038000
          DELTA~TRUE;   COMMENT HERE WE GO;                             00038100
          FOR J~1 STEP 3 UNTIL 255 DO LOOKUP(R,J);                      00038200
          DELTA~FALSE;                                                  00038300
     END OF STFILL WHICH MUST FOLLOW LOOKUP;                            00038400
     PROCEDURE STUFFITAWAY;                                             00038500
     BEGIN                                                              00038600
          STREAM PROCEDURE GETSIX(FRONT,N,INFO,HOWMANY,PLUSES,OVT);     00038700
          VALUE FRONT,N,HOWMANY,PLUSES;                                 00038800
          BEGIN                                                         00038900
               SI~INFO; N(SI~SI+63;SI~SI+1); SI~SI+FRONT;               00039000
               DI~OVT;   DS~2 LIT "00";                                 00039100
               DS~HOWMANY CHR;                                          00039200
               PLUSES(DS~1 LIT "+");                                    00039300
          END;                                                          00039400
          M~L~0;   N~INDEX;                                             00039500
          L~CHRCOUNT MOD 6;   IF L=0 THEN L~CHRCOUNT DIV 6              00039600
                                        ELSE L~(CHRCOUNT DIV 6)+1;      00039700
          M~M& CHRCOUNT[42:42:6] & L[36:42:6];                          00039800
          BUFF[N]~M;   N~N+1;                                           00039900
          FOR J~0 STEP 1 UNTIL L-1 DO BEGIN                             00040000
               GETSIX(FRONT,FRONT DIV 64, INFO,CHRCOUNT,6-CHRCOUNT,     00040100
                                             BUFF[N]);                  00040200
               FRONT~FRONT+6;   CHRCOUNT~CHRCOUNT-6;   N~N+1;           00040300
                                             END;                       00040400
          MOVE(CARDNO,BUFF[N],1);                                       00040500
          N~N+1;                                                        00040600
          COMMENT NOW THE RECORD IS IN THE BUFFER,WITH                  00040700
          INDEX MARKING THE BEGINNING AND N MARKING WHERE               00040800
          WE START THE NEXT ONE;                                        00040900
     END;                                                               00041000
     PROCEDURE CHECKBUFF;                                               00041100
     BEGIN                                                              00041200
          IF INDEX}486 THEN BEGIN FOR J~INDEX STEP 1 UNTIL 499 DO       00041300
                                   BUFF[J]~0;                           00041400
                                   INDEX~0;   COUNTER~COUNTER+1;        00041500
                                   WRITE(MT11,500,BUFF[*]);             00041600
                              END;                                      00041700
     END OF CHECKBUFF;                                                  00041800
     COMMENT HERE WE GO ON PASS ONE;                                    00041900
     T1~FALSE; T~TRUE;   STCOUNT~0;                                     00042000
     COUNTER~INDEX~0;                   COMMENT ZERO RECORDS ON TAPE,   00042100
                                        NEXT ENTRY IN BUFF[*];          00042200
     C1~C2~0;                           COMMENT INITIALISE CONTINUE;    00042300
     FILL TRANSLATE[*] WITH    0, 0, 0, 0, 0, 0, 0, 0, 0, 0,            00042400
                               0, 0, 0, 0, 0, 0, 0, 1, 2, 3,            00042500
                               4, 5, 6, 7, 8, 9, 0, 0, 0, 0,            00042600
                               0, 0, 0,10,11,12,13,14,15,16,            00042700
                              17,18, 0, 0, 0, 0, 0, 0, 0, 0,            00042800
                              19,20,21,22,23,24,25,26, 0, 0,            00042900
                               0, 0, 0, 0;                              00043000
FOR I~ 0 STEP 1 UNTIL 1022 DO SYMBOL0[I]~0;                             00043100
     STFILL;                                                            00043200
     COMMENT NOW DELTA IS FALSE FOR THE REST OF THE RUN;                00043300
     RES~FALSE;                                                         00043400
     INP~0; READACARD;                                                  00043500
     IF NOTDOLLAR(INFO) THEN    BEGIN                                   00043600
               WRITE(LP,ERRORFORMAT3); GO TO FINALLABEL   END;          00043700
     FETCHANAME;   STUFFITAWAY;                                         00043800
     IF BUFF[INDEX+1]="TAPE++" THEN BEGIN                               00043900
          INP~1; CLOSE(CR,RELEASE); GO TO START   END;                  00044000
     IF BUFF[INDEX+1]="CARD++" THEN GO TO START;                        00044100
     WRITE(LP,ERRORFORMAT3); GO TO FINALLABEL;                          00044200
START:     OVER~ FALSE;                                                 00044300
     READACARD;                         COMMENT THIS PULLS IN A CARD    00044400
                                        PUTS THE CARDNUMBER INTO CARDNO 00044500
                                        AND PUTS A PERCENT WHERE        00044600
                                        COLUMN 73 WAS;                  00044700
TOPOFHEAP:     FETCHANAME;              COMMENT LOOKS FOR AN IDENTIFIER 00044800
                                        AND SETS FRONT AND CHRCOUNT;    00044900
     IF CHRCOUNT =6 THEN BEGIN          COMMENT LOOK FOR FORMAT;        00045000
                         IF FMT(FRONT,FRONT DIV 64,INFO) THEN BEGIN     00045100
                         T1~TRUE; GO TO TOPOFHEAP        END            00045200
                              END;                                      00045300
     IF CHRCOUNT=7 THEN BEGIN           COMMENT LOOK FOR COMMENT;       00045400
                         IF COM(FRONT,FRONT DIV 64,INFO) THEN           00045500
                              GO TO COMMENTDELETE                       00045600
                         END;                                           00045700
     IF O8(FRONT, FRONT DIV 64, INFO) THEN GO TO TOPOFHEAP;             00045800
                                        COMMENT DELETE OCTALS;          00045900
                                        COMMENT NOW WE PEEK AT THE      00046000
                                        FIRST CHARACTER TO MAKE SURE IT 00046100
                                        IS A LETTER;                    00046200
     IF PEEK(FRONT,FRONT DIV 64,INFO) THEN STUFFITAWAY                  00046300
                                        ELSE GO TO TOPOFHEAP;           00046400
     LOOKUP(BUFF,INDEX+1);                                              00046500
     IF RES THEN BEGIN RES~FALSE;IF T1 THEN GO TO TOPOFHEAP END         00046600
          ELSE  BEGIN                                                   00046700
                    INDEX~N; CHECKBUFF                                  00046800
                    END;                                                00046900
     IF NOT T1 THEN GO TO TOPOFHEAP;                                    00047000
                                                                        00047100
     FORMATPROCESSOR;                   COMMENT HERE WE GET RID OF      00047200
                                        THE REST OF A FORMAT DECL;      00047300
     GO TO TOPOFHEAP;                                                   00047400
                                                                        00047500
COMMENTDELETE: LOOK; IF NEXT=";" THEN GO TO TOPOFHEAP                   00047600
                                        ELSE GO TO COMMENTDELETE;       00047700
                                                                        00047800
EOP1: IF INDEX=0 THEN GO TO L1;                                         00047900
     FOR  J~ INDEX STEP 1 UNTIL 499 DO BUFF[J]~0;                       00048000
     COUNTER~COUNTER+1;                                                 00048100
     WRITE(MT11,500,BUFF[*]);                                           00048200
L1:  REWIND(MT11);  CLOSE(INPUT[INP],RELEASE);                          00048300
    I~J~0; FOR K~1 STEP 1 UNTIL 26 DO ARY[K]~0;                         00048400
L2: IF SYMBOL0[J]=0 OR SYMBOL1[J].[18:6]=63 THEN                        00048500
     BEGIN J~J+1; GO TO L2 END;                                         00048600
     SYMBOL0[I]~SYMBOL0[J]; SYMBOL1[I]~SYMBOL1[J];                      00048700
     K~SYMBOL1[I].[18:6];                                               00048800
     ARY[K]~ARY[K]+SYMBOL1[I].[38:10];                                  00048900
     I~I+1; IF I!STCOUNT   THEN BEGIN J~J+1; GO TO L2 END;              00049000
     SYMBOL0[STCOUNT]~"======";         COMMENT MARKER;                 00049100
          COMMENT NOW THE SYMBOL TABLE IS PACKED UP;                    00049200
          COMMENT NOW WE SORT IT;                                       00049300
L3: I~0; RES~FALSE;                                                     00049400
L4:  IF SYMBOL0[I+1]<SYMBOL0[I] THEN                                    00049500
     BEGIN                                                              00049600
          DELT~SYMBOL0[I+1]; SYMBOL0[I+1]~SYMBOL0[I]; SYMBOL0[I]~DELT;  00049700
          DELT~SYMBOL1[I+1]; SYMBOL1[I+1]~SYMBOL1[I]; SYMBOL1[I]~DELT;  00049800
     RES~TRUE                                                           00049900
     END;                                                               00050000
     I~I+1;                                                             00050100
     IF I!STCOUNT THEN GO TO L4;                                        00050200
     IF RES THEN GO TO L3;                                              00050300
END OF THE INNER BLOCK SO TURN BACK STORAGE *;                          00050400
                                                                        00050500
BEGIN                                                                   00050600
     ARRAY CLOT[0:SIZE,0:149];          COMMENT HOLDS THE CARDIDS FOR   00050700
                                        THE CURRENT LOT OF IDENTIFIERS; 00050800
     INTEGER ARRAY POINT[0:SIZE+2];     COMMENT POINTS TO ROWS OF CLOT  00050900
                                        HOLDING SYMBOL REFERENCES;      00051000
     INTEGER S;                                                         00051100
     INTEGER MIN,T,A,B,W,MAX;                                           00051200
     DEFINE NAM=SYMBOL0[I]#,ID=SYMBOL1[I]#;                             00051300
     INTEGER ARRAY ROW[0:SIZE];         COMMENT POINTS TO CURRENT       00051400
                                        POSITION IN EACH CLOT ROW;      00051500
     BOOLEAN TT;                                                        00051600
     INTEGER LINEPOS;                                                   00051700
     FILE M(1,500);                                                     00051800
     SWITCH FILE F~MT11,M;                                              00051900
     INTEGER IND;       COMMENT POINTS TO THE OUTPUT TAPE ARRAY;        00052000
     ARRAY BUF[0:499];        COMMENT THE OUTPUT ARRAY;                 00052100
     INTEGER IO;              COMMENT TELLS WHICH IS INPUT TAPE;        00052200
     INTEGER DUM;                                                       00052300
     LABEL TOP,AGAIN,BOT,LL1,ZOOP,LL2,NT,FD,KKK;                        00052400
     PROCEDURE PRINTNAME;                                               00052500
     BEGIN                                                              00052600
          LABEL UNDER;                                                  00052700
          STREAM PROCEDURE XES(LINE,NUMB);VALUE NUMB;                   00052800
          BEGIN DI~LINE;DI~DI+6;NUMB(DS~1 LIT"/");                      00052900
          END OF XES;                                                   00053000
     BLANKLINE(LINE); WRITE(LP,15,LINE[*]);                             00053100
     GET(NAM,LINE,0);                                                   00053200
     IF ID.[37:1]=1 THEN BEGIN W~ID.[12:6];C1~ID.[24:3];                00053300
                              C2~ID.[27:10];A~6;                        00053400
                              FOR T~0 STEP 1 UNTIL W-2 DO               00053500
                              BEGIN                                     00053600
                                   GET(CONTINUE[C1,C2+T],LINE,A);       00053700
                                   A~A+6;                               00053800
                              END;                                      00053900
                              GO TO UNDER;                              00054000
                         END;                                           00054100
     IF ID.[24:3]=7 THEN BEGIN W~ID.[12:6];                             00054200
                               XES(LINE,W);                             00054300
                         END;                                           00054400
UNDER:                                                                  00054500
     WRITE(LP,15,LINE[*]);                                              00054600
     END OF PRINTNAME;                                                  00054700
     PROCEDURE PRINTIDS;                                                00054800
     BEGIN                                                              00054900
          INTEGER M,N;                                                  00055000
          PROCEDURE PUTONEIN(N); VALUE N; INTEGER N;                    00055100
          BEGIN                                                         00055200
               STREAM PROCEDURE GET(CLOTI,LINE,LINEPOS,MESS);           00055300
               VALUE LINEPOS,MESS;                                      00055400
               BEGIN                                                    00055500
                    SI~CLOTI;DI~LINE;MESS(DI~DI+63;DI~DI+1);            00055600
                    DI~DI+LINEPOS;                                      00055700
                    DS~8 CHR;DS~1 LIT",";                               00055800
               END OF GET;                                              00055900
               N~(J-1)MOD 150;                                          00056000
               GET(CLOT[S,N],LINE,LINEPOS,LINEPOS DIV 64);              00056100
               IF N=149 THEN BEGIN                                      00056200
                                   S~S+1;                               00056300
                                   END;                                 00056400
               IF LINEPOS=100 THEN BEGIN                                00056500
                                        WRITE(LP,15,LINE[*]);           00056600
                                        BLANKLINE(LINE);                00056700
                                        LINEPOS~10                      00056800
                                        END                             00056900
                                   ELSE                                 00057000
                                        LINEPOS~LINEPOS+9;              00057100
               END OF PUTONEIN;                                         00057200
          BLANKLINE(LINE);LINEPOS~10;                                   00057300
          M~ID.[38:10];       COMMENT THE NUMBER OF IDS;                00057400
          FOR J~1 STEP 1 UNTIL M DO PUTONEIN(N);                        00057500
          WRITE(LP,15,LINE[*]);                                         00057600
          WRITE(LP,XXX);                                                00057700
     END OF PRINTIDS;                                                   00057800
     WRITE(LP[PAGE]);                                                   00057900
     READ(MT11,500,BUFF[*]);INDEX~0; IO~IND~0;                          00058000
     MIN~0;MAX~0; I~0;                                                  00058100
TOP:  S~0; POINT[0]~0;   I~MIN;                                         00058200
AGAIN: COUNTER~ID.[38:10];                                              00058300
     IF COUNTER   >150|SIZE THEN BEGIN WRITE(LP,ERRORFORMAT2);          00058400
                                        GO TO FINALLABEL                00058500
                              END;                                      00058600
     COUNTER~(COUNTER-1) DIV 150;                                       00058700
     POINT[S+1]~POINT[S]+COUNTER +1;                                    00058800
     I~I+1; S~S+1;                                                      00058900
     IF POINT[S]>SIZE+1 OR MIN+S=STCOUNT+1 THEN                         00059000
     BEGIN                                                              00059100
          S~S-2; I~I-2; GO TO BOT                                       00059200
     END;                                                               00059300
     GO TO AGAIN;                                                       00059400
BOT:                                    COMMENT HERE WE HAVE SOME       00059500
                                        IDENTIFIERS TO DUMP;            00059600
     MAX~MIN+S;                                                         00059700
     FOR J~0 STEP 1 UNTIL SIZE DO ROW[J]~0;                             00059800
LL1:A~BUFF[INDEX];B~BUFF[INDEX+1];                                      00059900
     W~A.[36:6];I~MIN;                                                  00060000
     IF A=0 THEN BEGIN                                                  00060100
                         READ(F[IO],500,BUFF[*])[ZOOP];                 00060200
                         INDEX~0;  GO TO LL1;                           00060300
                    END;                                                00060400
     S~0;                                                               00060500
LL2:IF NAM!B THEN GO TO NT;                                             00060600
     IF(ID.[37:1]=1)OR(ID.[24:3]=7)THEN TT~TRUE                         00060700
                                        ELSE TT~FALSE;                  00060800
     IF W=1 THEN                                                        00060900
     BEGIN IF TT THEN GO TO NT ELSE GO TO FD                            00061000
     END;                                                               00061100
     IF NOT TT THEN GO TO NT;                                           00061200
     IF ID.[24:3]=7 THEN GO TO FD;                                      00061300
     IF W!ID.[12:6]THEN GO TO NT;                                       00061400
     C1~ID.[24:3];C2~ID.[27:10];                                        00061500
     FOR K~2 STEP 1 UNTIL W DO                                          00061600
          IF BUFF[INDEX+K]!CONTINUE[C1,C2+K-2]THEN GO TO NT;            00061700
 GO TO FD;                                                              00061800
NT:IF I!MAX THEN BEGIN S~S+1;I~I+1; GO TO LL2 END;                      00061900
                                        COMMENT THE IDENTIFIER IS       00062000
                                        NOT IN THIS LOT,TRY THE NEXT    00062100
                                        IDENTIFIER;                     00062200
                              COMMENT MOVE THIS IDENTIFIER TO           00062300
                                             THE OUTPUT TAPE  ;         00062400
     MOVE(BUFF[INDEX],BUF[IND], W+2);                                   00062500
     IND~IND+W+2;                                                       00062600
     IF IND}486 THEN  BEGIN FOR DUM~IND STEP 1 UNTIL 499 DO             00062700
          BUF[DUM]~0; IND~0; WRITE(F[1-IO],500,BUF[*]);                 00062800
                         END;                                           00062900
     INDEX~INDEX+W+2;                                                   00063000
     GO TO LL1;                                                         00063100
FD:                                     COMMENT THIS IDENTIFIER IS      00063200
                                        INCLUDED IN THE CURRENT LOT;    00063300
     MOVE(BUFF[INDEX+W+1],CLOT[POINT[S],ROW[POINT[S]]],1);              00063400
     IF ROW[POINT[S]]=149 THEN                                          00063500
                              POINT[S]~POINT[S]+1                       00063600
                    ELSE                                                00063700
                              ROW[POINT[S]]~ROW[POINT[S]]+1;            00063800
     INDEX~INDEX+W+2;                                                   00063900
     GO TO LL1;                                                         00064000
                                                                        00064100
ZOOP:                                   COMMENT NOW WE HAVE COLLECTED   00064200
                                        ALL THE VALID CARDIDS FOR THIS  00064300
                                        PASS.PRINT THEM OUT;            00064400
     IF IND!0 THEN BEGIN FOR DUM~IND STEP 1 UNTIL 499 DO BUF[DUM]~0;    00064500
          WRITE(F[1-IO],500,BUF[*]);                                    00064600
                    END;                                                00064700
     REWIND(MT11); REWIND(M);  IO~1-IO;                                 00064800
     I~MIN; S~0;                                                        00064900
KKK:PRINTNAME;                                                          00065000
     PRINTIDS;                                                          00065100
     IF I!MAX THEN BEGIN                                                00065200
                    S~S+1; I~I+1; GO TO KKK;                            00065300
                    END;                                                00065400
     MIN~MAX+1;IF MIN!STCOUNT THEN  BEGIN                               00065500
          IND~INDEX~0; READ(F[IO],500,BUFF[*]); GO TO TOP;              00065600
                         END;                                           00065700
END OF THE SECOND PHASE OF INDEXER;                                     00065800
FINALLABEL:                                                             00065900
END OF THE INDEXER PROGRAM.                                             00066000
