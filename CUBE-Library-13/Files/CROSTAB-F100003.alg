          BEGIN                                                         00000000
COMMENT        CROSTAB/STATMAN;                                         00000100
COMMENT        CROSS TABULATION WITH CORRELATION AND CHI SQUARE         00000200
               C. L. CLARK                                              00000300
               PROFESSIONAL SERVICES DIVISIONAL GROUP                   00000400
               BURROUGHS CORPORATION                                    00000500
               PROGRAM CONTAINS CARDS                                   00000600
               CARD SEQUENCE STARTS WITH CTAB 1                         00000700
               FIRST RELEASE DATE 1;                                    00000800
INTEGER        NVAR,NREP,NSEL,I,J,K,L ;                                 00000900
INTEGER ARRAY  ID[0:4] ;                                                00001000
BOOLEAN        ZROW ;                                                   00001100
LABEL          START,NOMO,LL1 ;                                         00001200
FORMAT IN      FORM1(5A6,3I6,2L5),                                      00001300
               FORM2(14I5),                                             00001400
               FORM4(I3,I2,22I3) ;                                      00001500
FORMAT         FORM3(///////////////////////////////////////////////////00001600
               //////////////////////////////////////////////////);     00001700
FORMAT OUT     TITLE(X52,"CROSS TABULATION"///X45,5A6//),               00001800
              FRMO (X1,"VARIABLE",I4,"AND",I4,"RANGES ARE GREATER THAN",00001900
                    " 34"//),                                           00002000
               FRM1 (//////X1,"VARIABLE",I4," IS CROSS TABULATED WITH ",00002100
                    "VARIABLE",I4//                                     00002200
                    X1,"NUMBER OF REPLICATIONS IS",I5//                 00002300
                    X1,"VARIABLE MAXIMUM MINIMUM"/                      00002400
                    X1,I6,2I8/X1,I6,2I8//),                             00002500
               FRM2 (X1,"NO ENTRY IN THE TABLE"//),                     00002600
               FRM3 (" ROW",X7,"(",I2,")"/"TOTAL",X6,"RANGE"),          00002700
               FRM4 (I4,X7,I4," *",34I3),                               00002800
              FRM5 (X16,"*********************************************",00002900
               "***************************************************",   00003000
                    "********"),                                        00003100
               FRM6 (X11," (",I2,")",I4,17I6),                          00003200
               FRM7 (X11," RANGE",18I6),                                00003300
               FRM8 (/X11,"COLUMN",I4,17I6),                            00003400
               FRM9 (X11," TOTAL",18I6),                                00003500
               FRM10(/"GRAND TOTAL =",I6//),                            00003600
               FRM11(X1,"CORRELATION COEFFICIENT IS",F8.4//),           00003700
               FRM12(X1,"CHI SQUARE IS NOT COMPUTABLE"//),              00003800
               FRM13(X1,"CHI SQUARE =",F8.3,X5,"DF =",I5//),            00003900
               FRM14(X1,"OVERFLOW FREQUENCIES"/X4,"ROW  COLUMN   COUNT"/00004000
                    (X4,I3,X4,I3,X5,I5/)),                              00004100
               FRM15(//X1,"VALUES NOT ENTERED",I4),                     00004200
               FRM16(///X4,"CASE NO.",X5,"VARIABLE",I3,X5,"VARIABLE",I3/00004300
                    (X4,I5,X9,I6,X10,I6)) ;                             00004400
FILE IN        CARD (1,10) ;                                            00004500
FILE OUT       PRINT 4 (1,15) ;                                         00004600
DEFINE         LINE=PRINT#;                                             00004700
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00004800
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00004900
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00005000
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00005100
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00005200
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00005300
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00005400
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00005500
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00005600
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00005700
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00005800
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00005900
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00006000
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00006100
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00006200
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00006300
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00006400
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00006500
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300006600
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00006700
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00006800
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00006900
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00007000
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00007100
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00007200
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00007300
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00007400
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100007500
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00007600
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00007700
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00007800
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00007900
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00008000
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00008100
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00008200
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00008300
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00008400
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00008500
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00008600
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00008700
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00008800
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00008900
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00009000
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00009100
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00009200
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00009300
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00009400
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00009500
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00009600
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00009700
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00009800
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00009900
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00010000
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00010100
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00010200
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800010300
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00010400
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00010500
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00010600
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00010700
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00010800
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00010900
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00011000
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00011100
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00011200
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00011300
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00011400
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00011500
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00011600
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00011700
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00011800
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00011900
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00012000
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00012100
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00012200
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00012300
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00012400
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00012500
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00012600
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00012700
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00012800
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00012900
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00013000
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00013100
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00013200
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00013300
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00013400
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00013500
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00013600
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00013700
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00013800
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00013900
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00014000
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100014100
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00014200
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00014300
(136);EXIT:    FORMATPHRASE ~ ERRTOG;                                   00014400
           END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00014500
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00014600
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00014700
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000014800
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00014900
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00015000
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00015100
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00015200
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00015300
PROCEDURE      REINITFMT (INFORMAT, FMTNO);                             00015400
     FORMAT    INFORMAT;                                                00015500
     INTEGER   FMTNO;                                                   00015600
          BEGIN                                                         00015700
     DEFINE    NOFMTS = 1#;             % # OF VARIABLE FORMATS         00015800
     OWN INTEGER ARRAY ACC[0:NOFMTS-1]; % MUST BE DECLARED "OWN"        00015900
     INTEGER   NWDS, SDIV63, SMOD63;                                    00016000
STREAM PROCEDURE FILLWITHSTROKES (FMT, SDIV63, SMOD63, NWDS);           00016100
     VALUE     SDIV63, SMOD63, NWDS;                                    00016200
          BEGIN                                                         00016300
               DI ~ FMT;                     % INSERT SDIV63|63+SMOD63  00016400
               DI ~ DI+8;                    % EXPLICIT STROKES, AN     00016500
               SI ~ LOC SDIV63;              % IMPLICIT STROKE AND NMBR 00016600
               SI ~ SI+7;                    % OF WORDS INTO FMT SEGMENT00016700
               IF SC!"0"  THEN                                          00016800
                 SDIV63(63(DS~8 LIT "F00000+0"));                       00016900
               SMOD63(DS~8 LIT "F00000+0");                             00017000
               DS ~ 8 LIT "+0000001";                                   00017100
               SI ~ LOC NWDS;                                           00017200
               SI ~ SI+2;                                               00017300
               SKIP 3 SB;                                               00017400
               33(IF SB  THEN  DS~SET  ELSE  DS~RESET);                 00017500
               DS ~ 3 RESET;                                            00017600
               DS ~ 2 LIT "00";                                         00017700
          END OF FILLWITHSTROKES;                                       00017800
INTEGER PROCEDURE MAXWDS (INFORMAT);                                    00017900
     FORMAT    INFORMAT;                                                00018000
          BEGIN                                                         00018100
     INTEGER   DIV63, FLG, WORDS;                                       00018200
     LABEL     EXIT;                                                    00018300
  INTEGER STREAM PROCEDURE WRDS (FMT, D63, FLG);                        00018400
       VALUE   D63;                                                     00018500
            BEGIN                                                       00018600
       LOCAL   TMP;                                                     00018700
       LABEL   SCAN, FOUND, EXIT;                                       00018800
                 SI ~ LOC D63;                                          00018900
                 SI ~ SI+7;                                             00019000
                 IF SC="0"  THEN  BEGIN  SI~FMT; GO TO SCAN;  END;      00019100
                 SI ~ FMT;                                              00019200
                 D63(63(SI~SI+8));                                      00019300
  SCAN:                                                                 00019400
                 TALLY ~ 0;                                             00019500
                 TMP ~ TALLY;                                           00019600
                 DI ~ LOC TMP;                                          00019700
                 63(IF 4 SC=DC  THEN  JUMP OUT TO FOUND;                00019800
                    TALLY ~ TALLY+1;                                    00019900
                    DI ~ DI-4;                                          00020000
                    SI ~ SI+4);                                         00020100
                 GO TO EXIT;                                            00020200
  FOUND:                                                                00020300
                 SI ~ SI-4;                                             00020400
                 DI ~ FLG;                                              00020500
                 DS ~ WDS;                                              00020600
  EXIT:                                                                 00020700
                 WRDS ~ TALLY;                                          00020800
            END OF WRDS;                                                00020900
               WORDS ~ DIV63 ~ FLG ~ 0;                                 00021000
         DO BEGIN                                                       00021100
                 WORDS ~ WORDS+WRDS(INFORMAT, DIV63, FLG);              00021200
                 DIV63~DIV63+1;                                         00021300
            END UNTIL  FLG!0;                                           00021400
               MAXWDS ~ WORDS;                                          00021500
          END OF MAXWDS;                                                00021600
               IF NWDS~ACC[FMTNO-1]=0  THEN                             00021700
            ACC[FMTNO-1] ~ MAXWDS(INFORMAT)  ELSE                       00021800
            BEGIN                                                       00021900
                 SDIV63 ~ (NWDS-3) DIV 63;                              00022000
                 SMOD63 ~ (NWDS-3) MOD 63;                              00022100
                 FILLWITHSTROKES(INFORMAT, SDIV63, SMOD63, NWDS);       00022200
            END;                                                        00022300
          END OF REINITFMT;                                             00022400
START:    BEGIN                                                         00022500
LIST           HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NVAR,NREP,NSEL,   00022600
                    ZROW) ;                                             00022700
               READ (CARD ,FORM1,HEAD) [NOMO]                           00022800
          END ;                                                         00022900
LL1:      BEGIN                                                         00023000
INTEGER        NNN,NAXT,NUM,MAXX,MINX,NEWX,MUCH,OVER,LIMX,LIMY,EVEN,ODD,00023100
               MAXY,MINY,NEWY,DIF,IT,LOV,JUNX,JUNXY,TOTL,LL ;           00023200
INTEGER ARRAY  DATA[0:NVAR,0:NREP],MAX[0:NVAR],MIN[0:NVAR],             00023300
               LEV[0:22],X[0:NREP],Y[0:NREP],RANGX[0:40],RANGY[0:100],  00023400
               OUTX[0:50],OUTY[0:50],ITEM[0:50],PT[0:100,0:40],         00023500
               OPT[0:200,0:3],MARY[0:100],MARX[0:40] ;                  00023600
BOOLEAN        LOK,CROS ;                                               00023700
REAL           SUMX,SUMY,SUMXX,SUMXY,SUMYY,RXY,TOP,BOT,CHI,DEN,R,S ;    00023800
LABEL          L1,CROSS,CHANG,CORR,OFLO,L2,DROP,SKIP ;                  00023900
LIST           RANG(FOR I ~ 1 STEP 1 UNTIL NVAR DO [MAX[I],MIN[I]]),    00024000
               DATR(FOR K ~ 1 STEP 1 UNTIL NVAR DO DATA[K,J]),          00024100
               SEL (NAXT,NUM,FOR I ~ 1 STEP 1 UNTIL 22 DO LEV[I]),      00024200
               OUTT(FOR J ~ 0 STEP 1 UNTIL 4 DO ID[J]),                 00024300
               ROW (MARY[K],RANGY[K],                                   00024400
               FOR J ~ 1 STEP 1 UNTIL LIMX DO PT[K,J]),                 00024500
               ABSO(NEWX,RANGX[1],                                      00024600
                    FOR I ~ 1 STEP 1 UNTIL ODD DO RANGX[2|I+1]),        00024700
               ABSE(FOR I ~ 1 STEP 1 UNTIL EVEN DO RANGX[2|I]),         00024800
               CTLO(MARX[1],FOR I ~ 1 STEP 1 UNTIL ODD DO MARX[2|I+1]), 00024900
               CTLE(FOR I ~ 1 STEP 1 UNTIL EVEN DO MARX[2|I]),          00025000
               OFRE(FOR I ~ 1 STEP 1 UNTIL OVER DO [OPT[I,1],OPT[I,2],  00025100
                    OPT[I,3]]),                                         00025200
               OMIT(NEWY,NEWX,FOR I ~ 1 STEP 1 UNTIL JUNX DO [ITEM[I],  00025300
                    OUTY[I],OUTX[I]]) ;                                 00025400
               WRITE(PRINT[PAGE]) ;                                     00025500
               WRITE(PRINT,TITLE,OUTT) ;                                00025600
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00025700
               FOR J ~ 1 STEP 1 UNTIL NREP DO                           00025800
               DATA[I,J] ~ 0 ;                                          00025900
COMMENT        READ MAX AND MIN LIMITS FOR EACH VARIABLE ;              00026000
               READ(CARD ,FORM2,RANG) ;                                 00026100
               REINITFMT(FORM3,1);                                      00026200
               OBJECTFMTGEN(FORM3);                                     00026300
               FOR J ~ 1 STEP 1 UNTIL NREP DO                           00026400
               READ(CARD ,FORM3,DATR) ;                                 00026500
               NNN ~ 1 ;                                                00026600
L2:            IF NNN > NSEL THEN                                       00026700
               GO TO START ;                                            00026800
               READ(CARD ,FORM4,SEL) ;                                  00026900
               MAXX ~ MAX[NAXT] ;                                       00027000
               MINX ~ MIN[NAXT] ;                                       00027100
               NEWX ~ NAXT ;                                            00027200
               K ~ 0 ;                                                  00027300
               FOR J ~ 1 STEP 1 UNTIL NREP DO                           00027400
               X[J] ~ DATA[NAXT,J] ;                                    00027500
               MUCH ~ 1 ;                                               00027600
L1:            IF MUCH > NUM THEN                                       00027700
          BEGIN                                                         00027800
               NNN ~ NNN +1 ;                                           00027900
               GO TO L2                                                 00028000
          END ;                                                         00028100
               LOV ~ LEV[MUCH] ;                                        00028200
               FOR L ~ 1 STEP 1 UNTIL NREP DO                           00028300
               Y[L] ~ DATA[LOV,L] ;                                     00028400
               LOK ~ FALSE ;                                            00028500
               MAXY ~ MAX[LOV] ;                                        00028600
               MINY ~ MIN[LOV] ;                                        00028700
               NEWY ~ LOV ;                                             00028800
               DIF ~ MAXX - MINX ;                                      00028900
               IF DIF { 34 THEN                                         00029000
               GO TO CROSS ;                                            00029100
               DIF ~ MAXY - MINY ;                                      00029200
               IF DIF { 34 THEN                                         00029300
          BEGIN                                                         00029400
               LOK ~ TRUE ;                                             00029500
               CROS ~ TRUE ;                                            00029600
               GO TO CHANG                                              00029700
          END ;                                                         00029800
               WRITE(PRINT,FRMO,NEWY,NEWX) ;                            00029900
               LOK ~ FALSE ;                                            00030000
               GO TO CHANG ;                                            00030100
COMMENT        CROSS TABULATE ;                                         00030200
CROSS:         RANGX[1] ~ MINX ;                                        00030300
               RANGY[1] ~ MINY ;                                        00030400
               LIMX ~ MAXX - MINX + 1 ;                                 00030500
               LIMY ~ MAXY - MINY + 1 ;                                 00030600
               FOR I ~ 2 STEP 1 UNTIL LIMX DO                           00030700
               RANGX[I] ~ RANGX[I-1] + 1 ;                              00030800
               FOR I ~ 2 STEP 1 UNTIL LIMY DO                           00030900
               RANGY[I] ~ RANGY[I-1] + 1 ;                              00031000
COMMENT        COMPUTE FREQUENCY MATRIX AND LIST OF EXTREME VALUES ;    00031100
               JUNX ~ 0 ;                                               00031200
               JUNXY ~ 0 ;                                              00031300
               FOR I ~ 1 STEP 1 UNTIL LIMY DO                           00031400
               FOR J ~ 1 STEP 1 UNTIL LIMX DO                           00031500
               PT[I,J] ~ 0 ;                                            00031600
               FOR L ~ 1 STEP 1 UNTIL NREP DO                           00031700
          BEGIN                                                         00031800
              IF (X[L]>MAXX)OR(X[L]<MINX)OR(Y[L]>MAXY)OR(Y[L]<MINY)THEN 00031900
          BEGIN                                                         00032000
               JUNXY ~ JUNXY + 1 ;                                      00032100
               IF JUNXY { 49 THEN                                       00032200
          BEGIN                                                         00032300
               JUNX ~ JUNX + 1 ;                                        00032400
               OUTX[JUNX] ~ X[L] ;                                      00032500
               OUTY[JUNX] ~ Y[L] ;                                      00032600
               ITEM[JUNX] ~ L                                           00032700
          END                                                           00032800
          END                                                           00032900
               ELSE                                                     00033000
          BEGIN                                                         00033100
               I ~ X[L] - MINX + 1 ;                                    00033200
               J ~ Y[L] - MINY + 1 ;                                    00033300
               PT[J,I] ~ PT[J,I] + 1                                    00033400
          END                                                           00033500
          END ;                                                         00033600
COMMENT        COMPUTE OVERFLOW MATRIX ;                                00033700
               OVER ~ 0 ;                                               00033800
               FOR J ~ 1 STEP 1 UNTIL LIMY DO                           00033900
               FOR I ~ 1 STEP 1 UNTIL LIMX DO                           00034000
          BEGIN                                                         00034100
               IF PT[J,I] = 999 THEN                                    00034200
          BEGIN                                                         00034300
               OVER ~ OVER + 1 ;                                        00034400
               OPT[OVER,1] ~ RANGY[J] ;                                 00034500
               OPT[OVER,2] ~ RANGX[I] ;                                 00034600
               OPT[OVER,3] ~ PT[J,I] - 999;                             00034700
               PT[J,I] ~ PT[J,I] - OPT[OVER,3]                          00034800
          END                                                           00034900
          END ;                                                         00035000
               FOR I ~ 1 STEP 1 UNTIL LIMY DO                           00035100
               MARY[I] ~ 0 ;                                            00035200
               FOR J ~ 1 STEP 1 UNTIL LIMX DO                           00035300
               MARX[J] ~ 0 ;                                            00035400
               FOR I ~ 1 STEP 1 UNTIL LIMY DO                           00035500
               FOR J ~ 1 STEP 1 UNTIL LIMX DO                           00035600
          BEGIN                                                         00035700
               MARY[I] ~ MARY[I] + PT[I,J] ;                            00035800
               MARX[J] ~ MARX[J] + PT[I,J]                              00035900
          END ;                                                         00036000
               WRITE(PRINT,FRM1,NEWY,NEWX,NREP,NEWY,MAXY,MINY,NEWX,MAXX,00036100
                    MINX) ;                                             00036200
               TOTL ~ 0 ;                                               00036300
               FOR I ~ 1 STEP 1 UNTIL LIMX DO                           00036400
               TOTL ~ TOTL + MARX[I] ;                                  00036500
               IF TOTL { 0 THEN                                         00036600
          BEGIN                                                         00036700
               WRITE(PRINT,FRM2) ;                                      00036800
               GO TO CHANG                                              00036900
          END ;                                                         00037000
               WRITE(PRINT,FRM3,NEWY) ;                                 00037100
               IF ZROW THEN                                             00037200
          BEGIN                                                         00037300
               LL ~ 0 ;                                                 00037400
               FOR I ~ 1 STEP 1 UNTIL LIMY DO                           00037500
          BEGIN                                                         00037600
               IF MARY[I] = 0 THEN                                      00037700
               GO TO DROP ;                                             00037800
               LL ~ LL + 1 ;                                            00037900
               RANGY[LL] ~ RANGY[I] ;                                   00038000
               FOR J ~ 1 STEP 1 UNTIL LIMX DO                           00038100
               PT[LL,J] ~ PT[I,J] ;                                     00038200
               MARY[LL] ~ MARY[I] ;                                     00038300
DROP:     END ;                                                         00038400
               LIMY ~ LL ;                                              00038500
               LL ~ 0 ;                                                 00038600
               FOR I ~ 1 STEP 1 UNTIL LIMX DO                           00038700
          BEGIN                                                         00038800
               IF MARX[I] = 0 THEN                                      00038900
               GO TO SKIP ;                                             00039000
               LL ~ LL + 1 ;                                            00039100
               RANGX[LL] ~ RANGX[I] ;                                   00039200
               FOR J ~ 1 STEP 1 UNTIL LIMY DO                           00039300
               PT[J,LL] ~ PT[J,I] ;                                     00039400
               MARX[LL] ~ MARX[I] ;                                     00039500
SKIP:     END ;                                                         00039600
               LIMX ~ LL                                                00039700
          END ;                                                         00039800
               FOR L ~ 1 STEP 1 UNTIL LIMY DO                           00039900
          BEGIN                                                         00040000
               K ~ LIMY + 1 - L ;                                       00040100
               WRITE(PRINT,FRM4,ROW)                                    00040200
          END ;                                                         00040300
               WRITE(PRINT,FRM5) ;                                      00040400
               IF LIMX > 2|(LIMX DIV 2) THEN                            00040500
          BEGIN                                                         00040600
               ODD ~ LIMX DIV 2 ;                                       00040700
          EVEN ~ ODD                                                    00040800
          END                                                           00040900
               ELSE                                                     00041000
          BEGIN                                                         00041100
               ODD ~ LIMX DIV 2 - 1 ;                                   00041200
               EVEN ~ ODD + 1                                           00041300
          END ;                                                         00041400
               WRITE(PRINT,FRM6,ABSO) ;                                 00041500
               WRITE(PRINT,FRM7,ABSE) ;                                 00041600
               WRITE(PRINT,FRM8,CTLO) ;                                 00041700
               WRITE(PRINT,FRM9,CTLE) ;                                 00041800
               WRITE(PRINT,FRM10,TOTL) ;                                00041900
COMMENT        COMPUTE CORRELATION COEFFICIENT ;                        00042000
CORR:          SUMX ~ 0.0 ;                                             00042100
               SUMY ~ 0.0 ;                                             00042200
               SUMXX ~ 0.0 ;                                            00042300
               SUMXY ~ 0.0 ;                                            00042400
               SUMYY ~ 0.0 ;                                            00042500
               FOR I ~ 1 STEP 1 UNTIL NREP DO                           00042600
          BEGIN                                                         00042700
               R ~ X[I] ;                                               00042800
               S ~ Y[I] ;                                               00042900
               SUMX ~ SUMX + R ;                                        00043000
               SUMY ~ SUMY + S ;                                        00043100
               SUMXX ~ SUMXX + R|R ;                                    00043200
               SUMXY ~ SUMXY + R|S ;                                    00043300
               SUMYY ~ SUMYY + S|S                                      00043400
          END ;                                                         00043500
               TOP ~ NREP|SUMXY - SUMX|SUMY ;                           00043600
              BOT ~ SQRT((NREP|SUMXX-SUMX|SUMX)|(NREP|SUMYY-SUMY|SUMY));00043700
               IF BOT = 0.0 THEN                                        00043800
          BEGIN                                                         00043900
               WRITE(PRINT,FRM12) ;                                     00044000
               GO TO OFLO                                               00044100
          END ;                                                         00044200
               RXY ~ TOP / BOT ;                                        00044300
               WRITE(PRINT,FRM11,RXY) ;                                 00044400
COMMENT        COMPUTE CHI SQUARE ;                                     00044500
               CHI ~ 0.0 ;                                              00044600
               FOR I ~ 1 STEP 1 UNTIL LIMY DO                           00044700
               FOR J ~ 1 STEP 1 UNTIL LIMX DO                           00044800
          BEGIN                                                         00044900
               BOT ~ MARY[I] | MARX[J] ;                                00045000
               IF BOT = 0.0 THEN                                        00045100
          BEGIN                                                         00045200
               WRITE(PRINT,FRM12) ;                                     00045300
               GO TO OFLO                                               00045400
          END ;                                                         00045500
               TOP ~ PT[I,J] - BOT/TOTL ;                               00045600
               CHI ~ CHI + TOP|TOP|TOTL/BOT                             00045700
          END ;                                                         00045800
               IT ~ (LIMY-1)|(LIMX-1) ;                                 00045900
               WRITE(PRINT,FRM13,CHI,IT) ;                              00046000
COMMENT        LIST OVERFLOW FREQUENCIES ;                              00046100
OFLO:          IF OVER > 0 THEN                                         00046200
               WRITE(PRINT,FRM14,OFRE) ;                                00046300
COMMENT        LIST VALUES NOT ENTERED ;                                00046400
               IF JUNXY > 0 THEN                                        00046500
          BEGIN                                                         00046600
               WRITE(PRINT,FRM15,JUNXY) ;                               00046700
               IF JUNXY { 50 THEN                                       00046800
          BEGIN                                                         00046900
               WRITE(PRINT,FRM16,OMIT)                                  00047000
          END                                                           00047100
          END ;                                                         00047200
               CROS ~ FALSE ;                                           00047300
CHANG:    IF LOK THEN                                                   00047400
          BEGIN                                                         00047500
               IT ~ NEWY ;                                              00047600
               NEWY ~ NEWX ;                                            00047700
               NEWX ~ IT ;                                              00047800
               IT ~ MAXX ;                                              00047900
               MAXX ~ MAXY ;                                            00048000
               MAXY ~ IT ;                                              00048100
               IT ~ MINX ;                                              00048200
               MINX ~ MINY ;                                            00048300
               MINY ~ IT ;                                              00048400
               FOR L ~ 1 STEP 1 UNTIL NREP DO                           00048500
          BEGIN                                                         00048600
               IT ~ X[L] ;                                              00048700
               X[L] ~ Y[L] ;                                            00048800
               Y[L] ~ IT                                                00048900
          END ;                                                         00049000
               IF CROS THEN                                             00049100
               GO TO CROSS                                              00049200
          END ;                                                         00049300
               MUCH ~ MUCH + 1 ;                                        00049400
               GO TO L1                                                 00049500
          END ;                                                         00049600
NOMO:     END PROGRAM .                                                 00049700
                LAST CARD ON CRDIMG TAPE                                99999999
