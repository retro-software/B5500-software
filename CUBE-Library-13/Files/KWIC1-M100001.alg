BEGIN COMMENT NEW PERMUTED INDEX PROGRAM                                00000100
              JOHN SKELTON - BURROUGHS;                                 00000200
COMMENT        PROGRAM KWIK, CUBE LIBRARY NUMBER IS M100001.            00000300
               THIS VERSION DATED 5/31/67;                              00000400
  FILE IN CARD(2,10);                                                   00000500
  FILE OUT LINE 4(2,15);                                                00000600
  FILE    SORTEDP(1,78);                                                00000700
  FILE    SORTEDI(1,40);                                                00000800
  ARRAY ACCUM[0:8];                                                     00000900
      COMMENT ACCUM IS USED TO HOLD BLOCKED WORDS PICKED UP BY THE      00001000
              SCANNER AND IS OF THE FORM NAAAAAAA FOLLOWED BY A BLANK   00001100
              AT THE END OF THE WORD. N INDICATES THE LENGTH OF THE     00001200
              WORD PLUS THE BLANK;                                      00001300
  SAVE ARRAY BLOCK[0:100];                                              00001400
      COMMENT BLOCK IS USED TO HOLD ALL BLOCKED WORDS AND IS OF THE     00001500
              FORM 4AND X 3OR X 0 WHERE A LENGTH OF ZERO INDICATES THE  00001600
              END OF THE BLOCK ARRAY. XX IS THE LINK INTO TALLY ARRAY   00001700
              FOR THE NUMBER OF TIMES THE WORD HAS BEEN BLOCKED;        00001800
  ARRAY TALLY[0:63];                                                    00001900
      COMMENT TALLY IS A COUNT OF THE BLOCKED WORDS;                    00002000
  REAL NXTBLOCK;                                                        00002100
      COMMENT STREAM PROCEDURE POINTER TO NEXT AVAILABLE ENTRY IN BLOCK;00002200
  REAL NXTTALLY;                                                        00002300
      COMMENT INDEX FOR NEXT AVAILABLE TALLY CELL;                      00002400
  LABEL SORTBLOCK,OUTPUTBLOCK;                                          00002500
  BOOLEAN SORTPERMUTED;                                                 00002600
      COMMENT INDICATES A PERMUTED INDEX IS DESIRED;                    00002700
  BOOLEAN SORTLOCATION;                                                 00002800
      COMMENT INDICATES A SORT OF INPUT IS DESIRED;                     00002900
  BOOLEAN PRINTPERMUTED;                                                00003000
      COMMENT INDICATES A LISTING OF SORTED PERMUTED INDEX;             00003100
  BOOLEAN PRINTLOCATION;                                                00003200
      COMMENT INDICATES A LISTING OF SORTED INPUT;                      00003300
  ARRAY PERMUTEDHEADING1,                                               00003400
        PERMUTEDHEADING2,                                               00003500
        INDEXHEADING1,                                                  00003600
        INDEXHEADING2 [0:7];                                            00003700
      COMMENT THESE HOLD THE HEADING LINES;                             00003800
  ARRAY TITLEHEADINGPERMUTED,                                           00003900
        TITLEHEADINGINDEX[0:7];                                         00004000
      COMMENT THESE HOLD THE HEADING FOR TITLES;                        00004100
  ARRAY INDEXHEADINGPERMUTED,                                           00004200
        INDEXHEADINGINDEX[0:2];                                         00004300
      COMMENT THESE HOLD THE HEADINGS FOR INDEXES;                      00004400
  REAL LINESPERPAGE;                                                    00004500
  STREAM PROCEDURE MOVECHR(W)"CHRS "(N)"TIMES FROM"(A)"PLUS"(ASKIP)"TO"(00004600
                           B)"PLUS"(BSKIP); VALUE W,N,ASKIP,BSKIP;      00004700
    BEGIN                                                               00004800
      SI~A; SI~SI+ASKIP;                                                00004900
      DI~B; DI~DI+BSKIP;                                                00005000
      N(DS~W CHR);                                                      00005100
    END MOVECHR;                                                        00005200
 STREAM PROCEDURE MOVE(W)"WORDS"(N)"TIMES FROM"(A)"TO"(B);              00005300
   VALUE W,N;                                                           00005400
   BEGIN   SI~A;    DI~B;   N(DS~W WDS);                                00005500
   END MOVE;                                                            00005600
  STREAM PROCEDURE BLANK(W)"WORDS"(N)"TIMES IN BUFFER"(BUFF);           00005700
   VALUE W,N;                                                           00005800
   BEGIN  DI~BUFF;  N(W(DS~8 LIT "        "));                          00005900
   END BLANK;                                                           00006000
  REAL STREAM PROCEDURE MKABS(DESC);                                    00006100
   BEGIN DI~DESC;  MKABS~DI;                                            00006200
   END MKABS;                                                           00006300
      BLANK(8,1,PERMUTEDHEADING1[0]);                                   00006400
      BLANK(8,1,PERMUTEDHEADING2[0]);                                   00006500
      BLANK(8,1,INDEXHEADING1[0]);                                      00006600
      BLANK(8,1,INDEXHEADING2[0]);                                      00006700
      BLANK(8,1,TITLEHEADINGPERMUTED[0]);                               00006800
      BLANK(8,1,TITLEHEADINGINDEX[0]);                                  00006900
      BLANK(3,1,INDEXHEADINGPERMUTED[0]);                               00007000
      BLANK(3,1,INDEXHEADINGINDEX[0]);                                  00007100
      SORTPERMUTED~SORTLOCATION~PRINTPERMUTED~PRINTLOCATION~TRUE;       00007200
      LINESPERPAGE~50;                                                  00007300
BEGIN                                                                   00007400
      COMMENT THIS BLOCK SCANS CARDS, PERMUTES TAPES, ETC.;             00007500
  FILE OUT UNSORTP 2(2,78 ,SAVE 1);                                     00007600
  FILE OUT UNSORTI 2(2,40 ,SAVE 1);                                     00007700
      COMMENT CONTROL CARDS ARE OF THE FORM                             00007800
           62               80                                          00007900
      01   CONTROL BLOCK       WORD TO BE BLOCKED STARTS IN CC1         00008000
      02   CONTROL LINES       XX IN CC 1 AND 2                         00008100
      03   CONTROL PHEAD1                                               00008200
      04   CONTROL PHEAD2                                               00008300
      05   CONTROL IHEAD1                                               00008400
      06   CONTROL IHEAD2                                               00008500
      07   CONTROL PTHEAD                                               00008600
      08   CONTROL PIHEAD                                               00008700
      09   CONTROL IPHEAD                                               00008800
      10   CONTROL IIHEAD                                               00008900
      11   CONTROL END                                                  00009000
      12   CONTROL SORTP                                                00009100
      13   CONTROL SORTI                                                00009200
      14   CONTROL PRINTPONLY                                           00009300
      15   CONTROL PRINTIONLY                                           00009400
      16   CONTROL SORT                                                 00009500
      17   CONTROL PRINT                                                00009600
      18   NOT A CONTROL CARD;                                          00009700
  ARRAY TEST[0:50]; COMMENT CONTAINS ALPHA FOR ABOVE;                   00009800
  REAL TYPE; COMMENT TYPE OF CARD SCAN FINDS;                           00009900
  LABEL RET,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15,T16,     00010000
            T17,T18;                                                    00010100
  SWITCH SWITCHTYPE~T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T13,T14,T15, 00010200
                    T16,T17,T18;                                        00010300
   SAVE ARRAY HOLDP[0:77];  COMMENT HOLDS PERMUTED INDEXES;             00010400
  BOOLEAN FIRSTIME;                                                     00010500
  BOOLEAN ENDTOG;                                                       00010600
  STREAM PROCEDURE SCAN(BUFF,TYPE,ACCUM,TEST);                          00010700
    BEGIN                                                               00010800
        LOCAL TEMP;                                                     00010900
        LABEL FOUND,EXIT, THRU;                                         00011000
        DI~ BUFF;  DI~DI+61;  SI~TEST;                                  00011100
        17(IF 19 SC=DC THEN JUMP OUT TO FOUND                           00011200
                       ELSE SI~SI+5; DI~DI-19);                         00011300
        DI~TYPE; DS~8 LIT "0000000B";GO TO EXIT;                        00011400
     FOUND: DI~TYPE; DI~DI+7; SI~SI+4; DS~CHR;  DI~ACCUM ;              00011500
     SI~TYPE; SI~SI+7;                                                  00011600
     IF SC= "1" THEN BEGIN                                              00011700
                       SI~BUFF;  DI~DI+1;                               00011800
                        60(IF SC=" " THEN JUMP OUT TO THRU              00011900
                                     ELSE TALLY~TALLY+1;  DS~CHR);      00012000
                       THRU: TALLY~TALLY+1; DS~1 LIT " ";               00012100
                             TEMP~TALLY;  SI~LOC TEMP; SI~SI+7;         00012200
                         DI~ACCUM;  DS~CHR;  GO TO EXIT;                00012300
                     END;                                               00012400
     IF SC= "2" THEN BEGIN                                              00012500
                       DI~ACCUM; SI~BUFF; DS~2 OCT; END;                00012600
   EXIT: END SCAN;                                                      00012700
   STREAM PROCEDURE MAKBLOCK(ACCUM,BUFF);                               00012800
     BEGIN  LOCAL TEMP;                                                 00012900
       DI~BUFF;  DI~DI+4;                                               00013000
                 50(DS~ 6 LIT "999999"); DS~1 LIT "B";                  00013100
       DI~LOC TEMP; DI~DI+7; SI~ ACCUM;  DS~CHR;                        00013200
       DI~BUFF; DI~DI+4;   DS~TEMP CHR;                                 00013300
     END MAKBLOCK;                                                      00013400
  STREAM PROCEDURE ENTER     ( NCR,NCRV,ACCUM,INDEX);                   00013500
          VALUE NCRV,INDEX;                                             00013600
    BEGIN                                                               00013700
      LOCAL TEMP;                                                       00013800
        SI~ACCUM;   DI~LOC TEMP; DI~DI+7; DS~CHR;                       00013900
        SI~SI-1; DI~NCRV; DS~ TEMP CHR;  DS~CHR;                        00014000
        SI~LOC INDEX;  SI~SI+7; DS~CHR;                                 00014100
        TEMP~DI;  SI~LOC TEMP; DI~NCR; DS~WDS;                          00014200
    END ENTER;                                                          00014300
      FILL TEST[*] WITH                                                 00014400
        OCT2346456351464360,  OCT2243462342606060,  OCT6060606060606001,00014500
        OCT2346456351464360,  OCT4331452562606060,  OCT6060606060606002,00014600
        OCT2346456351464360,  OCT4730252124016060,  OCT6060606060606003,00014700
        OCT2346456351464360,  OCT4730252124026060,  OCT6060606060606004,00014800
        OCT2346456351464360,  OCT3130252124016060,  OCT6060606060606005,00014900
        OCT2346456351464360,  OCT3130252124026060,  OCT6060606060606006,00015000
        OCT2346456351464360,  OCT4763302521246060,  OCT6060606060606007,00015100
        OCT2346456351464360,  OCT3147302521246060,  OCT6060606060606010,00015200
        OCT2346456351464360,  OCT4731302521246060,  OCT6060606060606011,00015300
        OCT2346456351464360,  OCT3131302521246060,  OCT6060606060606012,00015400
        OCT2346456351464360,  OCT2545246060606060,  OCT6060606060606013,00015500
        OCT2346456351464360,  OCT6246516347606060,  OCT6060606060606014,00015600
        OCT2346456351464360,  OCT6246516331606060,  OCT6060606060606015,00015700
        OCT2346456351464360,  OCT4751314563474645,  OCT4370606060606016,00015800
        OCT2346456351464360,  OCT4751314563314645,  OCT4370606060606017,00015900
        OCT2346456351464360,  OCT6246516360606060,  OCT6060606060606020,00016000
        OCT2346456351464360,  OCT4751314563606060,  OCT6060606060606021;00016100
       FIRSTIME~TRUE;   ENDTOG~FALSE;                                   00016200
       NXTBLOCK~MKABS(BLOCK[0]);                                        00016300
 RET:  SCAN(CARD(0),TYPE,ACCUM[0],TEST[0]);                             00016400
       GO TO SWITCHTYPE[TYPE];                                          00016500
   T1: ENTER(NXTBLOCK,NXTBLOCK,ACCUM[0],NXTTALLY);                      00016600
       IF SORTPERMUTED THEN BEGIN                                       00016700
       BLANK(39,2,UNSORTP(0));                                          00016800
        MAKBLOCK(ACCUM[0],UNSORTP( 37));  RELEASE(UNSORTP);  END;       00016900
       RELEASE(CARD);  NXTTALLY~NXTTALLY+1;    GO TO RET;               00017000
   T2: LINESPERPAGE~ACCUM[0];                                           00017100
       RELEASE(CARD);  GO TO RET;                                       00017200
   T3: MOVE(8,1,CARD(0),PERMUTEDHEADING1[0]);                           00017300
       RELEASE(CARD);  GO TO RET;                                       00017400
   T4: MOVE(8,1,CARD(0),PERMUTEDHEADING2[0]);                           00017500
       RELEASE(CARD);  GO TO RET;                                       00017600
   T5: MOVE(8,1,CARD(0),INDEXHEADING1[0]);                              00017700
       RELEASE(CARD); GO TO RET;                                        00017800
   T6: MOVE(8,1,CARD(0),INDEXHEADING2[0]);                              00017900
       RELEASE(CARD); GO TO RET;                                        00018000
   T7: MOVE(8,1,CARD(0),TITLEHEADINGPERMUTED[0]);                       00018100
       RELEASE(CARD); GO TO RET;                                        00018200
   T8: MOVE(8,1,CARD(0),TITLEHEADINGINDEX[0]);                          00018300
       RELEASE(CARD); GO TO RET;                                        00018400
   T9: MOVE(3,1,CARD(0),INDEXHEADINGPERMUTED[0]);                       00018500
       RELEASE(CARD); GO TO RET;                                        00018600
   T10:MOVE(3,1,CARD(0),INDEXHEADINGINDEX[0]);                          00018700
       RELEASE(CARD); GO TO RET;                                        00018800
   T11:CLOSE(CARD,RELEASE); IF SORTPERMUTED OR SORTLOCATION             00018900
                               THEN BEGIN ENDTOG~TRUE; GO TO T18;END    00019000
                               ELSE GO TO OUTPUTBLOCK;                  00019100
   T12:SORTPERMUTED~PRINTPERMUTED~TRUE;                                 00019200
       SORTLOCATION ~ PRINTLOCATION ~ FALSE;                            00019300
       RELEASE(CARD); GO TO RET;                                        00019400
   T13:SORTLOCATION~PRINTLOCATION~TRUE;                                 00019500
       SORTPERMUTED~PRINTPERMUTED ~ FALSE;                              00019600
       RELEASE(CARD); GO TO RET;                                        00019700
   T14:SORTPERMUTED~FALSE;  PRINTPERMUTED~TRUE;                         00019800
       SORTLOCATION~PRINTLOCATION~ FALSE;                               00019900
       RELEASE(CARD); GO TO RET;                                        00020000
   T15:SORTLOCATION~FALSE;  PRINTLOCATION~TRUE;                         00020100
       SORTPERMUTED~PRINTPERMUTED~ FALSE;                               00020200
       RELEASE(CARD); GO TO RET;                                        00020300
   T16:SORTPERMUTED~PRINTPERMUTED~SORTLOCATION~PRINTLOCATION~TRUE;      00020400
       RELEASE(CARD);  GO TO RET;                                       00020500
   T17:SORTPERMUTED~SORTLOCATION~ FALSE;                                00020600
       PRINTPERMUTED~PRINTLOCATION~ TRUE;                               00020700
       RELEASE(CARD);   GO TO RET;                                      00020800
   T18:BEGIN                                                            00020900
         BOOLEAN STREAM PROCEDURE SEQCOMPARE(CARDBUFF,TAPEBUFF);        00021000
           BEGIN                                                        00021100
             SI~CARDBUFF; SI~SI+4;                                      00021200
             DI~TAPEBUFF;                                               00021300
             IF SC < "6"                                                00021400
                THEN IF 1 SC>DC                                         00021500
                        THEN IF 19 SC=DC                                00021600
                                THEN TALLY~1;  COMMENT TRUE IF SAME REC;00021700
             SEQCOMPARE~TALLY;                                          00021800
           END SEQCOMPARE;                                              00021900
         REAL STREAM PROCEDURE MAKSTART(BUFF);                          00022000
           BEGIN                                                        00022100
              DI~ BUFF;  DI~DI+4;    MAKSTART~DI;                       00022200
           END MAKSTART ;                                               00022300
         STREAM PROCEDURE ZOT(BUFF);                                    00022400
           BEGIN                                                        00022500
             DI~BUFF; DS~1 LIT "~";                                     00022600
           END ZOT;                                                     00022700
         BOOLEAN STREAM PROCEDURE DEBLANK(BUFFROM, BUFTO);              00022800
           BEGIN                                                        00022900
             LABEL FOUND,RET,EXIT,BLANK,ENDD;                           00023000
               SI~BUFFROM; SI~SI+4;                                     00023100
               30(10(IF SC=" " THEN SI~SI+1                             00023200
                               ELSE JUMP OUT 2 TO FOUND));              00023300
               GO TO EXIT;                                              00023400
        FOUND: DI~BUFTO; TALLY~1;                                       00023500
          RET: DS~CHR; IF SC=" " THEN DS~CHR;                           00023600
               BLANK:  IF SC="~" THEN GO TO ENDD;                       00023700
                       IF SC=" " THEN BEGIN                             00023800
                                          SI~SI+1;                      00023900
                                          GO TO BLANK;                  00024000
                                      END;                              00024100
                       GO TO RET;                                       00024200
       ENDD:   SI~BUFTO;  DI~BUFFROM; DI~DI+4; 10(DS~30CHR);            00024300
               DI~BUFTO; 30(DS~ 10 LIT "          " );                  00024400
         EXIT: DEBLANK~TALLY;                                           00024500
           END DEBLANK;                                                 00024600
        BOOLEAN STREAM PROCEDURE PERMUTE(BUFF,NCR,NCRV);  VALUE NCRV;   00024700
           BEGIN                                                        00024800
             LOCAL TEMPN,TEMPNCR;                                       00024900
             LABEL THRU,EXIT;                                           00025000
             SI~BUFF; SI~SI+4;                                          00025100
             63(IF SC!" " THEN BEGIN TALLY~TALLY+1; SI~SI+1; END        00025200
                          ELSE BEGIN TALLY~TALLY+1;                     00025300
                                     JUMP OUT 1 TO THRU; END;);         00025400
       THRU: TEMPN~TALLY; SI~SI+1; IF SC=" " THEN BEGIN TALLY~0;        00025500
                                                    GO TO EXIT; END     00025600
                                             ELSE TALLY~1;              00025700
             DI~NCRV;     DI~DI - TEMPN;     TEMPNCR~ DI;               00025800
             SI~NCRV;          30(DS~10 CHR); SI~SI+8;                  00025900
             TEMPN(DS~1 LIT " "); SI~LOC TEMPNCR; DI~NCR; DS~ WDS;      00026000
       EXIT: PERMUTE~TALLY;                                             00026100
       END PERMUTE;                                                     00026200
       LABEL     MVE,AGAIN,THRU;                                        00026300
       REAL CHARCT,NCR; INTEGER I;                                      00026400
         IF FIRSTIME THEN                                               00026500
                      BEGIN                                             00026600
      AGAIN:             BLANK(39,2,HOLDP[0]  );                        00026700
                         IF SORTLOCATION THEN BLANK(40,1,UNSORTI(0));   00026800
                         CHARCT~301;                                    00026900
      MVE:               MOVECHR(60,1,CARD(0),0,  HOLDP[(CHARCT-1)DIV 8]00027000
                              ,I~(CHARCT-1)MOD 8);                      00027100
                         CHARCT~CHARCT+60;                              00027200
                         MOVECHR(20,1,CARD(7),4,  HOLDP[75],0);         00027300
                         FIRSTIME~FALSE;                                00027400
                         IF ENDTOG                                      00027500
                               THEN GO TO SORTBLOCK                     00027600
                               ELSE BEGIN RELEASE (CARD); GO TO RET END;00027700
                      END;                                              00027800
         IF NOT ENDTOG THEN                                             00027900
                        IF SEQCOMPARE(CARD(7),  HOLDP[75])              00028000
                           THEN GO TO MVE;                              00028100
                       ZOT(  HOLDP[75]);                                00028200
                       IF NOT DEBLANK(  HOLDP[37],HOLDP  [0])           00028300
                            THEN BEGIN                                  00028400
                                   GO TO THRU;                          00028500
                                 END;                                   00028600
                       IF SORTLOCATION THEN BEGIN                       00028700
                                              MOVECHR(40,8,  HOLDP[37], 00028800
                                                      4,UNSORTI(0),0);  00028900
                                              RELEASE(UNSORTI);         00029000
                                            END;                        00029100
                       IF SORTPERMUTED  THEN                            00029200
                                       BEGIN                            00029300
                                          NCR~MAKSTART(  HOLDP[37]);    00029400
                   MOVECHR(39,16,HOLDP[ 0],0,UNSORTP( 0),0);            00029500
                                          RELEASE(UNSORTP);             00029600
                             WHILE PERMUTE(  HOLDP[37],NCR,NCR)         00029700
                              DO BEGIN                                  00029800
                                  MOVECHR(39,16,HOLDP[0],0,UNSORTP(0),  00029900
                                          0);   RELEASE(UNSORTP); END;  00030000
                                       END;                             00030100
              THRU:   IF ENDTOG THEN GO TO SORTBLOCK                    00030200
                                ELSE GO TO AGAIN;                       00030300
        END T18BLOCK;                                                   00030400
  END PERMUTEBLOCK;                                                     00030500
  SORTBLOCK: BEGIN                                                      00030600
                IF SORTPERMUTED THEN BEGIN                              00030700
                                       ZIP("SRTPERM","SPEC   ");        00030800
                                       READ(SORTEDP);                   00030900
                                       CLOSE(SORTEDP,SAVE);             00031000
                                     END ;                              00031100
                   IF SORTLOCATION THEN ZIP("SRTLOCN","SPEC   ");       00031200
             END SORTBLOCK;                                             00031300
  OUTPUTBLOCK:                                                          00031400
BEGIN                                                                   00031500
  INTEGER LINECT,PAGECT;                                                00031600
 STREAM PROCEDURE EXAMINF(BUFF,STRT,S,STPLOC); VALUE S;                 00031700
   BEGIN                                                                00031800
     LOCAL TEMP,TEMPSI,T1,T2,T3,T4;                                     00031900
     LABEL RET1,RET2,FOUND;                                             00032000
     SI~BUFF; SI~SI+S; DI~STRT; TEMP~SI; SI~LOC TEMP; DS~WDS; SI~TEMP;  00032100
     20(SI~SI+40; SI~SI+41;                                             00032200
        TEMPSI~SI;  TEMP~DI; SI~LOC TEMPSI; DI~LOC T1;                  00032300
        SI~SI+5; SKIP 3 SB; 5(DS~3 RESET; 3(IF SB THEN DS~1 SET         00032400
                                                  ELSE DS~1 RESET;      00032500
                                                  SKIP 1 SB;));         00032600
        SI~LOC TEMPSI; SI~SI+5; DS~3 RESET; 3(IF SB THEN DS~1 SET       00032700
                                                    ELSE DS~1 RESET;    00032800
                                                    SKIP 1 SB;);        00032900
        SI~LOC STPLOC; DI~LOC T3; SI~SI+5; SKIP 3 SB;                   00033000
                            5(DS~3 RESET; 3(IF SB THEN DS~1 SET         00033100
                                                  ELSE DS~1 RESET;      00033200
                                                  SKIP 1 SB));          00033300
                            DS~6 RESET;                                 00033400
        SI~LOC T1; DI~LOC T3;                                           00033500
        IF 6 SC}DC THEN SI~STPLOC                                       00033600
                   ELSE SI~TEMPSI;                                      00033700
                   DI~TEMP;                                             00033800
        IF SC!" "                                                       00033900
           THEN BEGIN                                                   00034000
                  RET1: SI~SI-1; IF SC!" " THEN GO TO RET1;             00034100
                        SI~SI+1; TEMP~SI; SI~LOC TEMP; DS~WDS; SI~TEMP; 00034200
                END                                                     00034300
           ELSE BEGIN                                                   00034400
                  SI~SI-1;                                              00034500
                  IF SC!" " THEN BEGIN SI~SI+2 ;                        00034600
                                  TEMP~SI; SI~LOC TEMP; DS~WDS; SI~TEMP;00034700
                                 END                                    00034800
                            ELSE BEGIN                                  00034900
                                  RET2: SI~SI-1; IF SC=" "              00035000
                                                    THEN GO TO RET2;    00035100
                                  SI~SI+1; TEMP~SI; SI~LOC TEMP; DS~WDS;00035200
                                  JUMP OUT 1 TO FOUND;                  00035300
                                 END                                    00035400
                END;);                                                  00035500
   FOUND: END EXAMINF;                                                  00035600
 STREAM PROCEDURE PRINTHEADING(BUFF,HEADING1,HEADING2);                 00035700
      BEGIN                                                             00035800
        DI~BUFF; 15(DS~1 LIT " ");                                      00035900
        SI~HEADING1; DS~60 CHR;  25(DS~1 LIT " ");                      00036000
        SI~HEADING2; DS~20 CHR;                                         00036100
      END PRINTHEADING;                                                 00036200
    STREAM PROCEDURE SETDESC(DESC,CONT); VALUE CONT;                    00036300
      BEGIN                                                             00036400
        LABEL PB,OK;                                                    00036500
        PB: SI~DESC;  SKIP 2 SB;  IF SB THEN GO TO OK ELSE GO TO PB;    00036600
        OK: DI~DESC; DI~DI+4; SKIP 3 DB; SI~LOC CONT; SI~SI+7;          00036700
        6(IF SB THEN DS~1 SET ELSE DS~1 RESET; SKIP 1 SB);              00036800
      END SETDESC;                                                      00036900
 DEFINE  SINGLE= 32 #,                                                  00037000
         DOUBL = 48 #,                                                  00037100
         PAGEEJ=  1 #;                                                  00037200
 REAL PAGECOUNT, LINECOUNT;                                             00037300
 INTEGER PREVFIRST, PREVCHR;                                            00037400
 STREAM PROCEDURE ENTERCHR(BUFF,SKP,CHAR); VALUE SKP,CHAR;              00037500
      BEGIN                                                             00037600
        DI~BUFF; DI~DI+SKP; SI~LOC CHAR; SI~SI+7;  DS~1 CHR;            00037700
      END ENTERCHR;                                                     00037800
 REAL STREAM PROCEDURE GETFIRST(BUFF,S); VALUE S ;                      00037900
      BEGIN                                                             00038000
        SI~BUFF; SI~SI+S; DI~LOC GETFIRST; DI~DI+7; DS~CHR;             00038100
      END GETFIRST;                                                     00038200
 STREAM PROCEDURE PRINTITLE(BUFF,TITLE,PAGE); VALUE PAGE;               00038300
      BEGIN                                                             00038400
        SI~LOC PAGE; SI~SI+7; DI~BUFF;                                  00038500
        IF SC="0" THEN DS~ 8 LIT "        "                             00038600
                  ELSE BEGIN                                            00038700
                       DS~5 LIT"PAGE ";                                 00038800
                       SI~LOC PAGE; DS~ 3 DEC;                          00038900
                       END;                                             00039000
        11(DS~2 LIT "  ");  SI~TITLE; DS~60 CHR; 15(DS~2 LIT "  ");     00039100
      END PRINTITLE;                                                    00039200
 STREAM PROCEDURE MOVETITLE(BUFF,SKP,FROM,N1,N2); VALUE SKP,FROM,N1,N2; 00039300
   BEGIN                                                                00039400
     DI~BUFF; DI~DI+SKP; SI~FROM; DS~N1 CHR; DS~N2 CHR;                 00039500
   END MOVE TITLE;                                                      00039600
 INTEGER ARRAY STRT[0:20];    BOOLEAN FRST;                             00039700
 INTEGER MVE1,MVE2,I,NUM1,NUM2,NUM,SKP, INDEX;                          00039800
  IF PRINTPERMUTED THEN                                                 00039900
 BEGIN                                                                  00040000
  LABEL EOF,REREAD,TESTCT,EN1;                                          00040100
  INTEGER FIRSTCHR,PREVFIRST,INDEX;                                     00040200
 STREAM PROCEDURE PRINTAST(BUFF,CER); VALUE CER;                        00040300
      BEGIN                                                             00040400
        DI~BUFF; SI~LOC CER; SI~SI+7; DS~CHR;                           00040500
        DS~1 LIT " ";                                                   00040600
        17(DS~1 LIT "*"); 25(DS~2 LIT "  "); 51(DS~1 LIT " ");          00040700
      END PRINTAST;                                                     00040800
 STREAM PROCEDURE PRINTBLOCK(BUFF,WORD,INX);  VALUE INX;                00040900
   BEGIN                                                                00041000
     LABEL FOUND;                                                       00041100
     DI~BUFF;  15(DS~ 8 LIT "        "); DI~ BUFF;                      00041200
     SI~ WORD;  SI~SI + 4;                                              00041300
     63(IF SC = " " THEN JUMP OUT 1 TO FOUND ELSE DS~ CHR);             00041400
     FOUND:  DS~ 32 LIT " HAS BEEN AUTOMATICALLY BLOCKED ";             00041500
     SI~ LOC INX;  DS~ 3 DEC;  DS~ 6 LIT " TIMES";                      00041600
   END PRINTBLOCK;                                                      00041700
 BOOLEAN STREAM PROCEDURE CHECKBLOCK(BUFF,TEST,INX);                    00041800
      BEGIN                                                             00041900
        LOCAL TEMP;                                                     00042000
        LABEL EXIT,RET;                                                 00042100
        SI~TEST;                                                        00042200
   RET: IF SC="0" THEN BEGIN TALLY~0;  GO TO EXIT; END;                 00042300
        DI~LOC TEMP; DI~DI+7;  DS~CHR;                                  00042400
        DI~BUFF; DI~DI+4;                                               00042500
        IF TEMP SC=DC THEN BEGIN                                        00042600
                             DI~INX;  DI~DI+7;  DS~CHR;                 00042700
                                                                        00042800
                             TALLY~1; GO TO EXIT;                       00042900
                           END;                                         00043000
        SI~SI+1;  GO TO RET;                                            00043100
  EXIT: CHECKBLOCK~TALLY;                                               00043200
      END CHECKBLOCK;                                                   00043300
 STREAM PROCEDURE EXAMINB(BUFF,STRT,S,STPLOC);  VALUE S;                00043400
   BEGIN                                                                00043500
     LOCAL TEMP,TEMPSI,T1,T2,T3,T4;                                     00043600
     LABEL RET1,RET2,FOUND;                                             00043700
     SI~BUFF;  SI~SI+S; DI~STRT; TEMP~SI; SI~LOC TEMP; DS~WDS; SI~TEMP; 00043800
     10(SI~SI-40; SI~SI-41;                                             00043900
        TEMPSI~SI; TEMP~DI; SI~LOC TEMPSI; DI~LOC T1;                   00044000
        SI~SI+5; SKIP 3 SB; 5(DS~3 RESET; 3(IF SB THEN DS~ 1 SET        00044100
                                                  ELSE DS~ 1 RESET;     00044200
                                                  SKIP 1 SB));          00044300
        SI~LOC TEMPSI; SI~SI+5; DS~3 RESET;3(IF SB THEN DS~1 SET        00044400
                                                  ELSE DS~1 RESET;      00044500
                                                  SKIP 1 SB;);          00044600
        SI~LOC STPLOC; DI~LOC T3; SI~SI+5; SKIP 3 SB;                   00044700
                            5(DS~3 RESET; 3(IF SB THEN DS~1 SET         00044800
                                                  ELSE DS~1 RESET;      00044900
                                                  SKIP 1 SB));          00045000
                            DS~6 RESET;                                 00045100
        SI~LOC T1; DI~LOC T3;                                           00045200
        IF 6 SC<DC THEN SI~STPLOC                                       00045300
                   ELSE SI~TEMPSI;                                      00045400
                   DI~TEMP;                                             00045500
        IF SC!" "                                                       00045600
           THEN BEGIN                                                   00045700
                  RET1: SI~SI+1; IF SC!" " THEN GO TO RET1;             00045800
                        SI~SI+1; TEMP~SI; SI~LOC TEMP; DS~WDS; SI~TEMP; 00045900
                END                                                     00046000
           ELSE BEGIN                                                   00046100
                  SI~SI+1;                                              00046200
                  IF SC!" " THEN BEGIN                                  00046300
                                   TEMP~SI; SI~LOC TEMP;                00046400
                                   DS~WDS; SI~TEMP;                     00046500
                                 END                                    00046600
                            ELSE BEGIN                                  00046700
                                   RET2: SI~SI+1; IF SC=" " THEN        00046800
                                                     GO TO RET2;        00046900
                                         TEMP~SI; SI~LOC TEMP; DS~WDS;  00047000
                                         JUMP OUT 1 TO FOUND;           00047100
                                 END;                                   00047200
                END;);                                                  00047300
   FOUND: END EXAMINB;                                                  00047400
         PAGECT~0; LINECT~LINESPERPAGE;                                 00047500
 REREAD: READ(SORTEDP[NO])[EOF] ;                                       00047600
         FIRSTCHR~ GETFIRST(SORTEDP(37),4);                             00047700
         IF FIRSTCHR!PREVFIRST OR LINECT > (LINESPERPAGE - 5)           00047800
             THEN BEGIN                                                 00047900
                    IF LINECT>(LINESPERPAGE-5)                          00048000
                       THEN BEGIN                                       00048100
                              BLANK(15,1,LINE(0));                      00048200
                              SETDESC(LINE,PAGEEJ);                     00048300
                              RELEASE(LINE);                            00048400
                              PRINTITLE(LINE(0),PERMUTEDHEADING1,       00048500
                                        PAGECT~PAGECT+1);               00048600
                              SETDESC(LINE,SINGLE);                     00048700
                              RELEASE(LINE);                            00048800
                              PRINTITLE(LINE(0),PERMUTEDHEADING2,0);    00048900
                              SETDESC(LINE,DOUBL );                     00049000
                              RELEASE(LINE);                            00049100
                              PRINTHEADING(LINE(0),TITLEHEADINGPERMUTED,00049200
                                           INDEXHEADINGPERMUTED);       00049300
                              SETDESC(LINE,DOUBL );                     00049400
                              RELEASE(LINE);                            00049500
                              LINECT~5;                                 00049600
                            END;                                        00049700
                    PRINTAST(LINE(0),PREVFIRST~FIRSTCHR);               00049800
                    SETDESC(LINE,DOUBL );                               00049900
                    RELEASE(LINE);                                      00050000
                    LINECT~LINECT+2;                                    00050100
                  END;                                                  00050200
         IF CHECKBLOCK(SORTEDP(37),BLOCK,INDEX)                         00050300
             THEN   IF GETFIRST(SORTEDP(75),0)!"B"                      00050400
                       THEN BEGIN                                       00050500
                              TALLY[INDEX]~TALLY[INDEX]+1;              00050600
                              RELEASE(SORTEDP);                         00050700
                              GO TO REREAD;                             00050800
                            END                                         00050900
                       ELSE BEGIN                                       00051000
                            IF TALLY[INDEX] ! 0 THEN                    00051100
                            BEGIN                                       00051200
                              PRINTBLOCK(LINE(0),SORTEDP(37),           00051300
                                         TALLY[INDEX]) ;                00051400
                              SETDESC(LINE,DOUBL );                     00051500
                              RELEASE(LINE);  LINECT~LINECT+2;          00051600
                            END;                                        00051700
                              RELEASE(SORTEDP);                         00051800
                              GO TO TESTCT;                             00051900
                            END;                                        00052000
         FRST~TRUE;                                                     00052100
         FOR I~0 STEP 1 UNTIL 20 DO STRT[I]~0;                          00052200
         EXAMINB(SORTEDP(37),STRT,4,SORTEDP(0));                        00052300
         I~0; DO IF STRT[I]=0 THEN GO TO EN1 UNTIL I~I+1>20;            00052400
    EN1: I~I-1;                                                         00052500
         DO BEGIN                                                       00052600
              NUM1~STRT[I].[33:15]|8 + STRT[I].[30:3];                  00052700
              NUM2~STRT[I-1].[33:15]|8+ STRT[I-1].[30:3];               00052800
              IF NUM~NUM2-NUM1 }1                                       00052900
                THEN BEGIN                                              00053000
                       IF NUM{63 THEN BEGIN                             00053100
                                        MVE1~NUM; MVE2~0;               00053200
                                      END                               00053300
                                 ELSE BEGIN                             00053400
                                        MVE1~63; MVE2~NUM-63;           00053500
                                      END;                              00053600
                       BLANK(15,1,LINE(0));                             00053700
                      MOVETITLE(LINE(0),10,STRT[I],MVE1,MVE2);          00053800
                      ENTERCHR(LINE(11),3,")");                         00053900
                       IF FRST THEN BEGIN                               00054000
                                      FRST~FALSE;                       00054100
                                      MOVECHR(19,1,SORTEDP(75),1,       00054200
                                              LINE(12),4);              00054300
                                      ENTERCHR(LINE(12),3," ");         00054400
                                    END;                                00054500
                       SETDESC(LINE,SINGLE); RELEASE(LINE);             00054600
                       LINECT~LINECT+1;                                 00054700
                     END;                                               00054800
            END UNTIL I~I-1=0;                                          00054900
         FOR I~0 STEP 1 UNTIL 20 DO STRT[I]~0;                          00055000
         SKP~0;                                                         00055100
         EXAMINF(SORTEDP(37),STRT,4,SORTEDP(75));                       00055200
         I~0 ;                                                          00055300
         DO BEGIN                                                       00055400
              NUM1~STRT[I].[33:15]|8 + STRT[I].[30:3];                  00055500
              NUM2~STRT[I+1].[33:15]|8 + STRT[I+1].[30:3];              00055600
              IF NUM~NUM2-NUM1}1                                        00055700
                THEN BEGIN                                              00055800
                       IF NUM{63 THEN BEGIN                             00055900
                                        MVE1~NUM; MVE2~0;               00056000
                                      END                               00056100
                                 ELSE BEGIN                             00056200
                                        MVE1~63; MVE2~NUM-63;           00056300
                                      END;                              00056400
                       BLANK(15,1,LINE(0));                             00056500
                       IF FRST THEN BEGIN                               00056600
                          FRST~ FALSE; MOVECHR(19,1,SORTEDP(75),1,      00056700
                                       LINE(12),4);  END;               00056800
                       MOVETITLE(LINE(0),SKP, STRT[I],MVE1,MVE2);       00056900
                       ENTERCHR(LINE(11),3,")");                        00057000
                       SETDESC(LINE,SINGLE); RELEASE(LINE);             00057100
                       SKP~10; LINECT~LINECT+1;                         00057200
                     END;                                               00057300
              I~I+1;                                                    00057400
            END UNTIL NUM <1;                                           00057500
            BLANK(15,1,LINE(0));                                        00057600
            SETDESC(LINE,SINGLE); RELEASE(LINE); LINECT~LINECT+1;       00057700
            RELEASE(SORTEDP);                                           00057800
 TESTCT: GO TO REREAD;                                                  00057900
  EOF: END;                                                             00058000
  IF PRINTLOCATION THEN                                                 00058100
   BEGIN                                                                00058200
     LABEL EOF,REREAD;                                                  00058300
                                                                        00058400
                                                                        00058500
                                                                        00058600
     PAGECT~0 ;                                                         00058700
     LINECT~LINESPERPAGE;                                               00058800
  REREAD: READ(SORTEDI[NO])[EOF];                                       00058900
     IF LINECT} LINESPERPAGE-5                                          00059000
       THEN BEGIN                                                       00059100
              BLANK(15,1,LINE(0));                                      00059200
              SETDESC(LINE,PAGEEJ); RELEASE(LINE);                      00059300
              PRINTITLE(LINE(0),INDEXHEADING1,PAGECT~PAGECT+1);         00059400
              SETDESC(LINE,SINGLE);  RELEASE(LINE);                     00059500
              PRINTITLE(LINE(0),INDEXHEADING2,0);                       00059600
              SETDESC(LINE,DOUBL ); RELEASE(LINE);                      00059700
              PRINTHEADING(LINE(0),TITLEHEADINGINDEX,INDEXHEADINGINDEX);00059800
              SETDESC(LINE,DOUBL ); RELEASE(LINE);                      00059900
              LINECT~5;                                                 00060000
            END;                                                        00060100
     FRST~TRUE;                                                         00060200
     FOR I~0 STEP 1 UNTIL 20 DO STRT[I]~0;                              00060300
         EXAMINF(SORTEDI( 0),STRT,0,SORTEDI(37));                       00060400
     I~0;                                                               00060500
     DO BEGIN                                                           00060600
          NUM1~STRT[I].[33:15]|8 + STRT[I].[30:3];                      00060700
          NUM2~STRT[I+1].[33:15]|8 + STRT[I+1].[30:3];                  00060800
          IF NUM~NUM2-NUM1}1                                            00060900
            THEN BEGIN                                                  00061000
                   IF NUM{63 THEN BEGIN                                 00061100
                                    MVE1~NUM; MVE2~0;                   00061200
                                  END                                   00061300
                             ELSE BEGIN                                 00061400
                                    MVE1~63; MVE2~NUM-63;               00061500
                                  END;                                  00061600
                   BLANK(15,1,LINE(0));                                 00061700
                   MOVETITLE(LINE(0),10,STRT[I],MVE1,MVE2);             00061800
                   ENTERCHR(LINE(11),3,")");                            00061900
                   IF FRST THEN BEGIN                                   00062000
                                  FRST~FALSE;                           00062100
                                  MOVECHR(19,1,SORTEDI(37),5,LINE(12),  00062200
                                          4);                           00062300
                                  ENTERCHR(LINE(12),3," ");             00062400
                                END;                                    00062500
                   SETDESC(LINE,SINGLE); RELEASE(LINE);                 00062600
                   LINECT~LINECT+1;                                     00062700
                 END;                                                   00062800
          I~I+1                                                         00062900
        END UNTIL NUM<1;                                                00063000
        BLANK(15,1,LINE(0));                                            00063100
        SETDESC(LINE,SINGLE);RELEASE(LINE); LINECT~LINECT+1;            00063200
        RELEASE(SORTEDI);                                               00063300
        GO TO REREAD;                                                   00063400
 EOF:END;                                                               00063500
  END OF OUTPUTBLOCK;                                                   00063600
 END OF PROGRAM.                                                        00063700
