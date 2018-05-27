BEGIN                                                                   00000000
COMMENT        ANOVA2/STATMAN;                                          00000100
COMMENT        BMD08V **ANOVA2**IS A TRANSLATION OF A PROGRAM           00000200
               RECEIVED FROM UCLA IN NOVEMBER OF 1966.                  00000300
               TRANSLATION ON THE FORTRAN VERSION WAS DONE              00000400
               BY VIRGINIA RECK WITH THE ASSISTANCE OF FORREST          00000500
               CARHART.  TRANSLATION WAS COMPLETED IN MAY OF 1967.      00000600
               ANOVA2 WILL BE ADDED TO THE STAT LIBRARY OF THE          00000700
               UNIVERSITY OF DENVER.                                    00000800
               DIRECTIONS AS TO HOW TO USE THIS PROGRAM ARE             00000900
               FOUND IN THE MANUAL OF BIOMEDICAL COMPUTER PROGRAMS      00001000
               AND IN THE STATISTICS MANUAL OF THE COMPUTING CENTER.   ;00001100
REAL ARRAY   DATA[0:1,0:63],SVS[0:11111,[512]];                         00001200
FILE IN        CARD (2,10);                                             00001300
DEFINE         TAPE5=CARD#;                                             00001400
FILE OUT PUNCH 0(2,10);                                                 00001500
FILE OUT       PRINT 4(2,15);                                           00001600
DEFINE LINE=PRINT#, TAPE6=PRINT#;                                       00001700
FILE           TAPE1(2,100),                                            00001800
               TAPE2(2,100),                                            00001900
               TAPE3(2,100),                                            00002000
               TAPE4(2,100);                                            00002100
FILE XXXXXX 2(2,15);                                                    00002200
SWITCH FILE FILESW~XXXXXX,TAPE1,TAPE2,TAPE3,TAPE4,TAPE5,TAPE6;          00002300
REAL ARRAY     BUFFER[0:22];                                     %VR&FFC00002400
LABEL  FINIS;                                                           00002500
REAL Q,XPR;    INTEGER K,JM1P1;                                  %VR    00002600
FORMAT F(//////"STOP / PAUSE  NO. ",I5), OKTL(256O);                    00002700
FORMAT QUIXOTE(//////////////////////////////////////////////////////// 00002800
////////////////////////////////);                                      00002900
BOOLEAN        B1,B2,B3,B4;                                             00003000
REAL PROCEDURE INT(ARG1);         VALUE ARG1;        REAL ARG1;         00003100
   INT~SIGN(ARG1)|ENTIER(ABS(ARG1));                                    00003200
REAL PROCEDURE TANH(ARG1);        VALUE ARG1;        REAL ARG1;         00003300
   TANH~((Q~EXP(ARG1|2))-1)/(Q+1);                                      00003400
REAL PROCEDURE MAX(ARG1,ARG2);    VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00003500
   MAX~IF ARG1}ARG2 THEN ARG1 ELSE ARG2;                                00003600
REAL PROCEDURE MIN(ARG1,ARG2);    VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00003700
   MIN~IF ARG1{ARG2 THEN ARG1 ELSE ARG2;                                00003800
REAL PROCEDURE DIM(ARG1,ARG2);    VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00003900
   DIM~MAX(ARG1-ARG2,0);                                                00004000
REAL PROCEDURE TSIGN(ARG1,ARG2);  VALUE ARG1,ARG2;   REAL ARG1,ARG2;    00004100
   TSIGN~SIGN(ARG2)|ABS(ARG1);                                          00004200
REAL PROCEDURE LOG(ARG1);         VALUE ARG1;        REAL ARG1;         00004300
   LOG~LN(ARG1)/2.30258509298;                                          00004400
PROCEDURE ERROR(ARG1);            VALUE ARG1;        REAL ARG1;         00004500
BEGIN WRITE(PRINT,F,ARG1); GO TO FINIS END;                             00004600
REAL ARRAY CBLKBUFTAP[0:1];                   COMMENT TRUE LENGTH=1;    00004700
COMMENT        OBJECTFMTGEN  IS USED TO READ IN VARIABLE FORMAT; %VR    00004800
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00004900
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00005000
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00005100
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00005200
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00005300
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00005400
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00005500
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00005600
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00005700
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00005800
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00005900
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00006000
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00006100
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00006200
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00006300
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00006400
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00006500
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00006600
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300006700
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00006800
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00006900
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00007000
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00007100
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00007200
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00007300
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00007400
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00007500
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100007600
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00007700
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00007800
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00007900
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00008000
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00008100
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00008200
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00008300
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00008400
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00008500
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00008600
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00008700
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00008800
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00008900
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00009000
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00009100
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00009200
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00009300
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00009400
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00009500
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00009600
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00009700
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00009800
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00009900
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00010000
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00010100
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00010200
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00010300
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800010400
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00010500
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00010600
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00010700
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00010800
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00010900
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00011000
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00011100
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00011200
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00011300
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00011400
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00011500
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00011600
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00011700
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00011800
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00011900
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00012000
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00012100
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00012200
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00012300
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00012400
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00012500
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00012600
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00012700
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00012800
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00012900
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00013000
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00013100
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00013200
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00013300
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00013400
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00013500
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00013600
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00013700
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00013800
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00013900
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00014000
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00014100
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100014200
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00014300
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00014400
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00014500
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00014600
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00014700
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000014800
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00014900
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00015000
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00015100
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00015200
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00015300
PROCEDURE  SRSS(JNCM,JNI,SVE,SVLQ,SVP,SVAA);                            00015400
  VALUE JNCM,JNI;                                                       00015500
  INTEGER JNCM,JNI;                                                     00015600
  INTEGER ARRAY SVLQ[0];                                                00015700
REAL ARRAY     SVE[0],SVP[0],SVAA[0];                                   00015800
BEGIN                                                                   00015900
DEFINE    JNBUFF  =CBLKBUFTAP[1]#;                                      00016000
OWN INTEGER DX1;                                                        00016100
OWN INTEGER JL,JI,JJ,JLL,JL0,JI1,JK0,JK1,JK,JLLL,JMM,JN;                00016200
OWN REAL JQ000HL,JU,JQ001HL,JQ002HL,JH,JA,JQ003HL,JQ004HL;              00016300
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  FNBOOL,FNAND,FNCOMPL;      00016400
FORMAT FL17(X16),                                                       00016500
FL15(A1," X(",12A1),                                                %VR 00016600
FL16(A4,2A6);                                                           00016700
LIST LIST1(JA,FOR DX1~JK0 STEP 1 UNTIL JK1 DO SVS[DX1],JU);             00016800
LIST LIST2(SVS[JLLL-2],SVS[JLLL-1],SVS[JLLL]);                          00016900
LABEL L4,L8,L10,L11,L14;                                                00017000
IF B1 THEN BEGIN                                                        00017100
B1~FALSE;                                                               00017200
  FILL DATA[0,*] WITH ")";                                              00017300
  JQ000HL~DATA[0,0];                                                    00017400
  FILL DATA[0,*] WITH ".";                                              00017500
  JQ001HL~DATA[0,0];                                                    00017600
  FILL DATA[0,*] WITH " ";                                              00017700
  JQ002HL~DATA[0,0];                                                    00017800
  FILL DATA[0,*] WITH "+";                                              00017900
  JQ003HL~DATA[0,0];                                                    00018000
  FILL DATA[0,*] WITH "-";                                              00018100
  JQ004HL~DATA[0,0];                                                    00018200
END;                                                                    00018300
COMMENT        SUBROUTINE SS FOR BMD08V  MARCH 1,1966            %VR    00018400
               DIMENSION S(2),E(2),LQ(2),P(2),AA(2);             %VR    00018500
JU~(JQ000HL);                                                           00018600
JL~JM1P1-1;                                                      %VR    00018700
COMMENT        THIS SETS UP PIRAMID OF ZEROS AND ONES;           %VR    00018800
JI~1;                                                                   00018900
DO BEGIN                                                                00019000
  JJ~1;                                                                 00019100
  DO BEGIN                                                              00019200
    JL~JL+1;                                                            00019300
    SVS[JL]~0 END UNTIL (JJ~(JJ+1))>JI;                                 00019400
  SVS[JL]~1 END UNTIL (JI~(JI+1))>JNCM;                                 00019500
JLL~JL;                                                                 00019600
JL0~JM1P1;                                                       %VR    00019700
JI1~1;                                                                  00019800
COMMENT        BY SUBTRACTION OF ONES,MINUS-ONES THIS SETS UP ARRAY     00019900
               WHICH INDICATES WHETHER CELL DEVS X(..) IS NEG OR POS    00020000
               OR SKIPPED - THE NUMBER OF X() INDICATED BY ONES,-ONES   00020100
               WILL BE DECREASED BY SVMN AT TIME OF PRINTING  ;  %VR    00020200
JI~2;                                                                   00020300
DO BEGIN                                                                00020400
  JJ~1;                                                                 00020500
  DO BEGIN                                                              00020600
IF REAL(BOOLEAN(SVE[JJ]) AND (NOT(BOOLEAN(SVE[JI]))))!0 THEN GO TO L4;  00020700
JK0~(JJ|(JJ-1)) DIV 2 + JM1P1;                                   %VR    00020800
    JL~JL0;                                                             00020900
    JK1~JK0+JJ-1;                                                       00021000
    JK~JK0;                                                             00021100
    DO BEGIN                                                            00021200
      JL~JL+1;                                                          00021300
      SVS[JL]~SVS[JL]-SVS[JK] END UNTIL (JK~(JK+1))>JK1;                00021400
    L4: END UNTIL (JJ~(JJ+1))>JI1;                                      00021500
  JL0~JL0+JI;                                                           00021600
  JI1~JI END UNTIL (JI~(JI+1))>JNCM;                                    00021700
JL~JLL;                                                                 00021800
JLLL~JLL+JNCM|JNI;                                                      00021900
JMM~JLLL;                                                               00022000
                                                                 %VR&FFC00022100
JI~1;                                                                   00022200
DO BEGIN                                                                00022300
  JN~JNI;                                                               00022400
COMMENT    SVLQ CONTAINS THE LOCATIONS IN SVS WHERE "X()" IS TO         00022500
           BE FOUND;                                             %VR    00022600
  SVLQ[JI]~JLLL-JMM;                                                    00022700
  JJ~1;                                                                 00022800
  DO BEGIN                                                              00022900
    JL~JL+1;                                                            00023000
COMMENT    SVP WHICH CORRESPONDS TO SVPL IN MAINPRO STUFFS SVS          00023100
           WITH INDEX LETTERS APPROPRIATE;                       %VR    00023200
    SVS[JL]~(JQ001HL);                                                  00023300
IF REAL(BOOLEAN(SVAA[JN]) AND BOOLEAN(SVE[JI]))=0 THEN GO TO L8;        00023400
    SVS[JL]~SVP[JN];                                                    00023500
    L8: JN~JN-1 END UNTIL (JJ~(JJ+1))>JNI;                              00023600
  JH~(JQ002HL);                                                         00023700
  JK0~JL+1;                                                             00023800
JK~(JI|(JI+1)) DIV 2 +JM1P1-1;                                   %VR    00023900
COMMENT        USES ARRAY SVS TO DETERMINE SIGN(-,+) AFFIXED            00024000
               OR SKIP OF X(.......);                            %VR    00024100
  JJ~1;                                                                 00024200
  DO BEGIN                                                              00024300
    JK1~JK0-1;                                                          00024400
    JK0~JK0-JNI;                                                        00024500
    IF (XPR~(SVS[JK]))<0 THEN GO TO L11 ELSE IF XPR=0 THEN GO TO        00024600
     L10;                                                               00024700
    JA~JH;                                                              00024800
    JH~(JQ003HL);                                                       00024900
    GO TO L14;                                                          00025000
    L11: JA~(JQ004HL);                                                  00025100
COMMENT        SRSS SETS UP CELL DEVIATION OUTPUT " X(..I...)";  %VR&FFC00025200
L14:           WRITE(BUFFER[*], FL15, LIST1);                    %VR&FFC00025300
                                                                 %VR&FFC00025400
    JLLL~JLLL+3;                                                        00025500
               READ(BUFFER[*], FL16, LIST2);                     %VR&FFC00025600
                                                                 %VR&FFC00025700
    L10: JK~JK-1 END UNTIL (JJ~(JJ+1))>JI;                              00025800
  END UNTIL (JI~(JI+1))>JNCM;                                           00025900
JL~JM1P1-1;                                                      %VR    00026000
JK0~JMM+1;                                                              00026100
COMMENT        ARRAY SVS MOVED INTO LOWER LOCATION OF SVS;       %VR    00026200
JK~JK0;                                                                 00026300
DO BEGIN                                                                00026400
  JL~JL+1;                                                              00026500
  SVS[JL]~SVS[JK] END UNTIL (JK~(JK+1))>JLLL;                           00026600
SVLQ[JNCM+1]~JLLL-JMM;                                                  00026700
 END;                                                                   00026800
PROCEDURE SRPMEANS(JE,SVA,SVPL,SVIN,JNI,JNV,JJL1);               %VR    00026900
VALUE  JE,JNI,JNV,JJL1;                                          %VR    00027000
INTEGER  JNI,JNV,JJL1;                                           %VR    00027100
  REAL JE;                                                              00027200
  INTEGER ARRAY SVIN[0];                                                00027300
REAL ARRAY   SVA[0],SVPL[0];                                            00027400
BEGIN                                                                   00027500
OWN INTEGER ARRAY SVLL[0:11],SVJO[0:11];                                00027600
OWN REAL ARRAY SVP[0:11];                                               00027700
OWN INTEGER DX1;                                                        00027800
OWN INTEGER JM,JI,JNJ,JN,JJO1,JJO2,JL1,JJ,JL0;                          00027900
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  FNBOOL,FNAND;              00028000
FORMAT     FL127(/" ",X4,A1," =",I7,8I12/(X3,9I12)/(X3,2I12)),   %VR&FFC00028100
FL7(/" ",8(X4,A1," =",I3)),                                             00028200
               FL27(X5,A1," =",I7,8I12/(X3,9I12)/(X3,2I12)),     %VR&FFC00028300
               FL24(X1,A1," =",I3,9R12.5/(X7,9R12.5)/(X7,2R12.5)),%VR&FC00028400
FL25(X4,I3,9R12.5/(X7,9R12.5)/(X7,2R12.5)),                      %VRJULY00028500
FL35(X7,9R12.5/(X7,9R12.5)/(X7,2R12.5));                         %VRJULY00028600
LIST LIST1(SVP[JNI],FOR DX1~1 STEP 1 UNTIL JJO1 DO DX1);                00028700
LIST LIST2(FOR DX1~JM STEP 1 UNTIL JNJ DO [SVP[DX1],SVLL[DX1]]);        00028800
LIST LIST3(SVP[JNI-1],JI,FOR DX1~JL0 STEP JNV UNTIL JL1 DO SVS[DX1]);   00028900
LIST LIST4(JI,FOR DX1~JL0 STEP JNV UNTIL JL1 DO SVS[DX1]);              00029000
LIST LIST5(FOR DX1~JL0 STEP JNV UNTIL JL1 DO SVS[DX1]);                 00029100
LABEL L1,L3,L5,L89,L11,L8,L9,L0;                                        00029200
JM~JNI+1;                                                               00029300
JI~1;                                                                   00029400
DO BEGIN                                                                00029500
IF REAL(BOOLEAN(SVA[JI]) AND BOOLEAN(JE))=0 THEN GO TO L1;              00029600
  JM~JM-1;                                                              00029700
  SVP[JM]~SVPL[JI];                                                     00029800
  SVJO[JM]~SVIN[JI];                                                    00029900
  L1: END UNTIL (JI~(JI+1))>JNI;                                        00030000
JNJ~JNI-2;                                                              00030100
JN~JNI-JM+1;                                                            00030200
JJO1~SVJO[JNI];                                                         00030300
JJO2~SVJO[JNI-1];                                                       00030400
COMMENT    JJL1 = JL1 IN MAIN PRO AND INDICATES WHERE THE NUMBERS       00030500
           TO BE PRINTED BY SRPMEANS ARE FOUND - BEFORE PRINTING        00030600
           JJL1~JJL1+1  - AFTER PRINTING CONTROL RETURNS TO MAIN        00030700
           PRO WHERE JL1 IS JACKED UP ACCORDING TO SVIL[JJ];     %VR    00030800
JL1~1-JNV+JJL1;                                                  %VR    00030900
IF (XPR~(JN-2))>0 THEN GO TO L5 ELSE IF XPR<0 THEN GO TO L3;            00031000
COMMENT        THIS IS THE PLACE WHERE "L=" OF CELL MEANS AND           00031100
               DEVIATIONS IS PRINTED;                            %VR    00031200
WRITE(TAPE6,FL127,LIST1);                                               00031300
GO TO L89;                                                              00031400
L5: JI~JM;                                                              00031500
DO BEGIN                                                                00031600
  SVLL[JI]~1 END UNTIL (JI~(JI+1))>JNI;                                 00031700
L11: WRITE(TAPE6,FL7,LIST2);                                            00031800
GO TO L8;                                                               00031900
L9: JI~JNJ;                                                             00032000
JJ~3;                                                                   00032100
DO BEGIN                                                                00032200
  SVLL[JI]~SVLL[JI]+1;                                                  00032300
  IF SVLL[JI]{SVJO[JI] THEN GO TO L11;                                  00032400
  SVLL[JI]~1;                                                           00032500
  JI~JI-1 END UNTIL (JJ~(JJ+1))>JN;                                     00032600
GO TO L0;                                                               00032700
L8: WRITE(TAPE6,FL27,LIST1);                                            00032800
L89: JI~1;                                                              00032900
JL0~JL1+JNV;                                                            00033000
JL1~JL1+JJO1|JNV;                                                       00033100
WRITE(TAPE6,FL24,LIST3);                                                00033200
JI~2;                                                                   00033300
DO BEGIN                                                                00033400
  JL0~JL1+JNV;                                                          00033500
  JL1~JL1+JJO1|JNV;                                                     00033600
  WRITE(TAPE6,FL25,LIST4);                                              00033700
  END UNTIL (JI~(JI+1))>JJO2;                                           00033800
IF JN{2 THEN GO TO L0 ELSE GO TO L9;                                    00033900
L3: JL0~JL1+JNV;                                                        00034000
JL1~JL1+JNV|JJO1;                                                       00034100
WRITE(TAPE6,FL127,LIST1);                                               00034200
WRITE(TAPE6,FL35,LIST5);                                                00034300
L0:  END;                                                               00034400
INTEGER PROCEDURE FNLWH(JP);                                            00034500
  REAL JP;                                                              00034600
BEGIN                                                                   00034700
OWN INTEGER JLWH;                                                       00034800
OWN REAL JQ000HL,JQ001HL,JQ002HL,JQ003HL,JQ004HL,JQ005HL,JQ006HL,       00034900
 JQ007HL,JQ008HL,JQ009HL,JQ010HL,JQ011HL;                               00035000
LABEL L14,L15,L16,L17,L18,L13,L19,L0;                                   00035100
IF B2 THEN BEGIN                                                        00035200
B2~FALSE;                                                               00035300
  FILL DATA[0,*] WITH "(";                                              00035400
  JQ000HL~DATA[0,0];                                                    00035500
  FILL DATA[0,*] WITH ")";                                              00035600
  JQ001HL~DATA[0,0];                                                    00035700
  FILL DATA[0,*] WITH " ";                                              00035800
  JQ002HL~DATA[0,0];                                                    00035900
  FILL DATA[0,*] WITH ",";                                              00036000
  JQ003HL~DATA[0,0];                                                    00036100
  FILL DATA[0,*] WITH ".";                                              00036200
  JQ004HL~DATA[0,0];                                                    00036300
  FILL DATA[0,*] WITH "=";                                              00036400
  JQ005HL~DATA[0,0];                                                    00036500
  FILL DATA[0,*] WITH "+";                                              00036600
  JQ006HL~DATA[0,0];                                                    00036700
  FILL DATA[0,*] WITH "-";                                              00036800
  JQ007HL~DATA[0,0];                                                    00036900
  FILL DATA[0,*] WITH "$";                                              00037000
  JQ008HL~DATA[0,0];                                                    00037100
  FILL DATA[0,*] WITH "*";                                              00037200
  JQ009HL~DATA[0,0];                                                    00037300
  FILL DATA[0,*] WITH "/";                                              00037400
  JQ010HL~DATA[0,0];                                                    00037500
  FILL DATA[0,*] WITH "@";                                              00037600
  JQ011HL~DATA[0,0];                                                    00037700
JLWH~0;                                                                 00037800
END;                                                                    00037900
IF (JP=JQ002HL) THEN GO TO L14;                                         00038000
IF (JP=JQ000HL) THEN GO TO L15;                                         00038100
IF (JP=JQ001HL) THEN GO TO L16;                                         00038200
IF (JP=JQ003HL) THEN GO TO L17;                                         00038300
IF (JP=JQ004HL) THEN GO TO L18;                                         00038400
IF (JP=JQ008HL) THEN GO TO L13;                                         00038500
IF (JP=JQ005HL OR JP=JQ006HL OR JP=JQ007HL OR JP=JQ009HL OR JP=JQ010HL  00038600
 OR JP=JQ011HL) THEN GO TO L19;                                         00038700
JLWH~2;                                                                 00038800
GO TO L0;                                                               00038900
L13: JLWH~7;                                                            00039000
GO TO L0;                                                               00039100
L14: JLWH~1;                                                            00039200
GO TO L0;                                                               00039300
L17: JLWH~3;                                                            00039400
GO TO L0;                                                               00039500
L15: JLWH~4;                                                            00039600
GO TO L0;                                                               00039700
L16: JLWH~5;                                                            00039800
GO TO L0;                                                               00039900
L18: JLWH~6;                                                            00040000
GO TO L0;                                                               00040100
L19: JLWH~8;                                                            00040200
L0: FNLWH~JLWH END;                                                     00040300
PROCEDURE SRANOVA(SVAJ,SVMN,SVCW,SVIN,SVIL,SVFF,SVID,JJK1,JJK2,         00040400
   JMK,JJK3,JJK5,JILL);                                                 00040500
  VALUE JJK1,JJK2,JJK3,JJK5;                                            00040600
  INTEGER JJK1,JJK2,JMK,JJK3,JJK5,JILL;                                 00040700
  INTEGER ARRAY SVMN[0],SVIN[0],SVIL[0],SVID[0];                        00040800
REAL ARRAY     SVAJ[0],SVCW[0],SVFF[0];                                 00040900
BEGIN                                                                   00041000
OWN INTEGER ARRAY SVIC[0:100,0:11],SVII[0:11],SVIJ[0:100,0:11];         00041100
OWN REAL ARRAY SVST[0:100],SVX[0:255],SVFP[0:100],SVSF[0:100],          00041200
 SVSG[0:100];                                                           00041300
OWN INTEGER JNI,JNCW,JMT,JNV,JI,JNI1,JJ1,JJ,JM,JL,JK,JKM,JNVV,JNN,      00041400
               DX1,JIO,JN,JKU,JLL,JK0,JK1,JL0,JL1,JNM,JK2,JK3;          00041500
OWN REAL JQ000CT,JFQ,JXK,JGR,JF;                                        00041600
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  FNOR,FNBOOL,FNAND,FNCOMPL; 00041700
LABEL L501,L410,L404,L408,L401,L407,L0,L12,L13,L107,L15,L16,L118,       00041800
 L119,L19,L22,L24,L25,L27,L32,L62,TFEW;                          %VRJUNE00041900
FORMAT   FTFEW("NOT ENOUGH DATA AS SPECIFIED BY INDEX LEVELS");  %VR    00042000
IF B3 THEN BEGIN                                                        00042100
B3~FALSE;                                                               00042200
  FILL DATA[0,*] WITH OCT100000;                                        00042300
  JQ000CT~DATA[0,0];                                                    00042400
END;                                                                    00042500
JNI~JJK1;                                                               00042600
JNCW~JJK2;                                                              00042700
JMT~JJK3;                                                               00042800
JNV~JJK5;                                                               00042900
IF JMT>0 THEN GO TO L501;                                               00043000
JI~2;                                                                   00043100
DO BEGIN                                                                00043200
  SVAJ[JNI1]~JQ000CT;                                                   00043300
SVCW[JI]~ REAL(BOOLEAN(SVCW[JI]) OR BOOLEAN(JQ000CT));                  00043400
  JJ1~JI-1;                                                             00043500
  L410: JJ~1;                                                           00043600
  DO BEGIN                                                              00043700
IF REAL(BOOLEAN(SVCW[JJ]) AND (NOT(BOOLEAN(SVCW[JI]))))!0 THEN GO TO    00043800
                L404;                                                   00043900
JM~SVIL[JI]-JMT+JM1P1-1;                                         %VR    00044000
JL~SVIL[JJ]-JMT+JM1P1-1;                                         %VR    00044100
    SVS[JM]~SVS[JM]-SVS[JL];                                            00044200
    L404: END UNTIL (JJ~(JJ+1))>JJ1;                                    00044300
  SVII[JNI1]~-2;                                                        00044400
  JK~1;                                                                 00044500
  DO BEGIN                                                              00044600
IF REAL(BOOLEAN(SVAJ[JK]) AND BOOLEAN(SVCW[JJ]))=0 THEN GO TO L408;     00044700
    SVII[JK]~SVII[JK]+1;                                                00044800
    IF SVII[JK]<0 THEN GO TO L401;                                      00044900
    SVII[JK]~-SVIN[JK];                                                 00045000
    L408: END UNTIL (JK~(JK+1))>JNI1;                                   00045100
  L401: JJ~1;                                                           00045200
  DO BEGIN                                                              00045300
IF REAL(BOOLEAN(SVCW[JJ]) AND (NOT(BOOLEAN(SVCW[JI]))))!0 THEN GO TO    00045400
                L407;                                                   00045500
    SVIL[JJ]~SVIL[JJ]+SVIJ[JJ,JK];                                      00045600
    L407: END UNTIL (JJ~(JJ+1))>JI;                                     00045700
  IF JNI1!JK THEN GO TO L410;                                           00045800
  END UNTIL (JI~(JI+1))>JNCW;                                           00045900
GO TO L0;                                                               00046000
L501: JKM~JMK;                                                          00046100
JI~1;                                                                   00046200
DO BEGIN                                                                00046300
  SVST[JI]~0 END UNTIL (JI~(JI+1))>100;                                 00046400
JNVV~JNV;                                                               00046500
JNN~1;                                                                  00046600
JI~1;                                                                   00046700
DO BEGIN                                                                00046800
  JNN~JNN|SVIN[JI] END UNTIL (JI~(JI+1))>JNI;                           00046900
JNI1~JNI+1;                                                             00047000
JJ~1;                                                                   00047100
DO BEGIN                                                                00047200
  SVID[JJ]~1;                                                           00047300
  SVIL[JJ]~1;                                                           00047400
  JIO~1;                                                                00047500
  JI~1;                                                                 00047600
  DO BEGIN                                                              00047700
IF REAL(BOOLEAN(SVCW[JJ]) AND BOOLEAN(SVAJ[JI]))=0 THEN GO TO L13;      00047800
    SVIC[JJ,JI]~1;                                                      00047900
    SVID[JJ]~SVID[JJ]|SVIN[JI];                                         00048000
    GO TO L12;                                                          00048100
    L13: JIO~JI;                                                        00048200
    SVIC[JJ,JI]~0;                                                      00048300
    L12: END UNTIL (JI~(JI+1))>JNI;                                     00048400
  IF SVMN[JJ]>0 THEN GO TO L107;                                        00048500
  JK~JIO;                                                               00048600
  DO BEGIN                                                              00048700
    SVIC[JJ,JK]~-SVIC[JJ,JK] END UNTIL (JK~(JK+1))>JNI;                 00048800
  L107: SVFP[JJ]~JNN DIV SVID[JJ];                                      00048900
  SVIC[JJ,JNI1]~-1 END UNTIL (JJ~(JJ+1))>JNCW;                          00049000
SVIN[JNI1]~2;                                                           00049100
JI~1;                                                                   00049200
DO BEGIN                                                                00049300
  JJ~1;                                                                 00049400
  DO BEGIN                                                              00049500
    IF SVIC[JJ,JI]!1 THEN GO TO L16;                                    00049600
    SVIJ[JJ,JI]~JNV;                                                    00049700
    SVIL[JJ]~SVIL[JJ]|SVIN[JI];                                         00049800
    GO TO L15;                                                          00049900
    L16: SVIJ[JJ,JI]~(1-SVIL[JJ])|JNV;                                  00050000
    L15: END UNTIL (JJ~(JJ+1))>JNCW;                                    00050100
  SVII[JI]~-SVIN[JI] END UNTIL (JI~(JI+1))>JNI1;                        00050200
JN~(JNVV|JNCW) DIV JNV+1;                                               00050300
JJ~1;                                                                   00050400
DO BEGIN                                                                00050500
  IF SVMN[JJ]{0 THEN GO TO L118;                                        00050600
  JN~JN+SVIL[JJ];                                                       00050700
  SVIL[JJ]~(JN-SVIL[JJ])|JNV;                                           00050800
  L118: END UNTIL (JJ~(JJ+1))>JNCW;                                     00050900
JMK~JN+1;                                                               00051000
JJ~1;                                                                   00051100
DO BEGIN                                                                00051200
  IF SVMN[JJ]>0 THEN GO TO L119;                                        00051300
  JN~JN+SVIL[JJ];                                                       00051400
  SVIL[JJ]~(JN-SVIL[JJ])|JNV;                                           00051500
  L119: END UNTIL (JJ~(JJ+1))>JNCW;                                     00051600
JKU~JKM;                                                                00051700
JFQ~0;                                                                  00051800
IF JN|JNV<JILL THEN GO TO L19;                                          00051900
JILL~0;                                                                 00052000
GO TO L0;                                                               00052100
L19: JM~1;                                                              00052200
DO BEGIN                                                                00052300
  JKU~JKU+1;                                                            00052400
  IF JKU{JKM THEN GO TO L22;                                            00052500
READ(FILESW[JMT],QUIXOTE,FOR DX1~1 STEP 1 UNTIL JKM DO SVX[DX1])[TFEW]; 00052600
  JKU~1;                                                                00052700
  L22: JXK~SVX[JKU];                                                    00052800
  SVST[JM]~SVST[JM]+JXK;                                                00052900
  JJ~1;                                                                 00053000
  DO BEGIN                                                              00053100
JN~SVIL[JJ]+JM+JM1P1-1;                                             %VR 00053200
SVS[JN]~SVS[JN]+JXK;                                                %VR 00053300
END UNTIL JJ~(JJ+1)>JNCW                                            %VR 00053400
END UNTIL JM~(JM+1)>JNV;                                            %VR 00053500
JFQ~JFQ+1;                                                              00053600
JI~1;                                                                   00053700
DO BEGIN                                                                00053800
  SVII[JI]~SVII[JI]+1;                                                  00053900
  IF SVII[JI]<0 THEN GO TO L24;                                         00054000
  SVII[JI]~-SVIN[JI] END UNTIL (JI~(JI+1))>JNI1;                        00054100
L24: JJ~2;                                                              00054200
DO BEGIN                                                                00054300
  SVIL[JJ]~SVIL[JJ]+SVIJ[JJ,JI];                                        00054400
       IF SVIC[JJ,JI]}0 THEN GO TO L25;                          %VR    00054500
JN~SVIL[JJ]+JM1P1-1;                                                %VR 00054600
  JK~1;                                                                 00054700
  DO BEGIN                                                              00054800
    SVSG[JK]~0 END UNTIL (JK~(JK+1))>JNV;                               00054900
  JLL~-SVIJ[JJ,JI] DIV JNV+1;                                           00055000
JK0~(JJ-1)|JNVV+JM1P1-1;                                            %VR 00055100
  JGR~SVFP[JJ]/JFQ;                                                     00055200
  JL~1;                                                                 00055300
  DO BEGIN                                                              00055400
    JM~JK0;                                                             00055500
    JK~1;                                                               00055600
    DO BEGIN                                                            00055700
      JN~JN+1;                                                          00055800
      SVSF[JK]~SVS[JN]-JGR|SVST[JK];                                    00055900
      SVSG[JK]~SVSG[JK]+SVSF[JK];                                       00056000
      JM~JM+1;                                                          00056100
      SVS[JM]~SVS[JM]+SVSF[JK]|SVSF[JK];                                00056200
      IF JI=JNI1 THEN GO TO L27;                                        00056300
      SVS[JN]~0;                                                        00056400
      L27: END UNTIL (JK~(JK+1))>JNV END UNTIL (JL~(JL+1))>JLL;         00056500
  JGR~JFQ/SVFP[JJ]-(JLL);                                               00056600
  IF JGR{0 THEN GO TO L25;                                              00056700
  JM~JK0;                                                               00056800
  JK~1;                                                                 00056900
  DO BEGIN                                                              00057000
    JM~JM+1;                                                            00057100
    SVS[JM]~SVS[JM]+SVSG[JK]|SVSG[JK]/JGR END UNTIL (JK~(JK+1))>JNV;    00057200
  L25: END UNTIL (JJ~(JJ+1))>JNCW;                                      00057300
IF JI!JNI1 THEN GO TO L19;                                              00057400
JK1~JM1P1-1;                                                        %VR 00057500
JK~JM1P1-1;                                                         %VR 00057600
JL0~SVIL[1]+JM1P1;                                                  %VR 00057700
JL1~SVIL[1]+JNV+JM1P1-1;                                            %VR 00057800
JL~JL0;                                                                 00057900
DO BEGIN                                                                00058000
  JK~JK+1;                                                              00058100
  SVS[JK]~SVS[JL]|SVS[JL] END UNTIL (JL~(JL+1))>JL1;                    00058200
JJ~1;                                                                   00058300
DO BEGIN                                                                00058400
  JNM~JNV-SVIJ[JJ,JNI1];                                                00058500
  SVMN[JJ]~SVMN[JJ]|JNM;                                                00058600
  JF~(JNN|JNV) DIV JNM;                                                 00058700
JK2~SVIL[JJ]+JM1P1;                                                 %VR 00058800
JK3~SVIL[JJ]+JNM+JM1P1-1;                                           %VR 00058900
  JK~JK2;                                                               00059000
  DO BEGIN                                                              00059100
    SVS[JK]~SVS[JK]/JF END UNTIL (JK~(JK+1))>JK3;                       00059200
  JF~JNN DIV SVID[JJ];                                                  00059300
  JK0~JK1+1;                                                            00059400
  JK1~JK1+JNVV;                                                         00059500
  JK~JK0;                                                               00059600
  DO BEGIN                                                              00059700
    SVS[JK]~SVS[JK]/JF END UNTIL (JK~(JK+1))>JK1 END UNTIL (JJ~(JJ+1))> 00059800
 JNCW;                                                                  00059900
JL1~JNVV+JM1P1-1;                                                  %VR  00060000
JJ~2;                                                                   00060100
DO BEGIN                                                                00060200
  JJ1~JJ-1;                                                             00060300
JL0~JL1+1;                                                         %VR  00060400
JL1~JL1+JNVV;                                                      %VR  00060500
  JK~1;                                                                 00060600
  DO BEGIN                                                              00060700
IF REAL(BOOLEAN(SVCW[JK]) AND (NOT(BOOLEAN(SVCW[JJ]))))!0 THEN GO TO    00060800
               L32;                                                     00060900
    IF JK{1 THEN GO TO L62;                                             00061000
JM~(JK-1)|JNVV+JM1P1-1;                                            %VR  00061100
    JL~JL0;                                                             00061200
    DO BEGIN                                                            00061300
      JM~JM+1;                                                          00061400
      SVS[JL]~SVS[JL]-SVS[JM] END UNTIL (JL~(JL+1))>JL1;                00061500
    L62: SVID[JJ]~SVID[JJ]-SVID[JK];                                    00061600
    L32: END UNTIL (JK~(JK+1))>JJ1 END UNTIL (JJ~(JJ+1))>JNCW;          00061700
GO TO L0;                                                        %VRJUNE00061800
TFEW:  WRITE(LINE,FTFEW);  GO TO FINIS;                          %VRJUNE00061900
L0:  END;                                                               00062000
PROCEDURE MAINPRO;                                                      00062100
BEGIN                                                                   00062200
DEFINE    JNBUFF  =CBLKBUFTAP[1]#;                                      00062300
OWN INTEGER ARRAY SVLD[0:100],SVLQ[0:100],SVIN[0:11],SVMN[0:100],       00062400
 SVIL[0:100],SVID[0:100],SVJ1[0:10];                                    00062500
OWN REAL ARRAY SVFIN[0:11],SVFOP[0:11],SVROF[0:11],SVAA[0:11],          00062600
 SVP[0:100],SVPL[0:11],SVA[0:99],SVB[0:50],SVC[0:100],                  00062700
 SVD[0:100],SVE[0:100],SVG[0:100],SVP1[0:100],SVP2[0:100],              00062800
SVFF[0:108],SVZ2[0:10];                                                 00062900
OWN INTEGER   DX1,DUM;                                           %VRJUNE00063000
OWN INTEGER JKLV,JMTP,JNV,JNI,JKM,JNF,JMT,JN,JI,JL,JM,JK,JLB,JMO,       00063100
 JMI,JLL,JNL,JJ,JJ0,JNCM,JNUU,JJ11,JM1,JL1,JN2,JN3,JL0,JM0,JM2P2,       00063200
JILL,JNVV,JNN,JJYP,JL8,JL9;                                      %VR    00063300
OWN REAL JPROB,JFINI,JPF,JPC,JPCOV,JON,JAP,JQ003HL,JQ004HL,JPA,JQ005HL, 00063400
 JQ006HL,JCO,JRB,JDO,JPE,JQ010CT,JQ012CT,JQ013CT,JQ017CT,JH,JQ020CT,    00063500
 JU1,JQ021CT,JU2,JQ025CT,JX,JY,JQ030CT,JU,JQ007HL,JQ008HL,JQ009HL,      00063600
 JQ010HL,JSM;                                                           00063700
COMMENT  THE FOLLOWING PROCEDURES ARE USED:  FNLWH,FNOR,FNBOOL,FNAND,   00063800
 FNCOMPL,SRSS,SRANOVA,SRPMEANS;                                         00063900
FORMAT FL134(" BMD08V - ANALYSIS OF VARIANCE - VERSION OF ",            00064000
 "MARCH  1, 1966    "/" HEALTH SCIENCES COMPUTING FACILITY, UCLA"///    00064100
 " PROBLEM CODE ",A6),                                                  00064200
FL100(2A6,5I2,A2),                                                      00064300
FL114(/" CONTROL CARD ERROR"),                                          00064400
FL118(13A6,A2),                                                         00064500
FL2001(/" THIS PROBLEM IS TOO LARGE"),                                  00064600
FL191(A6,20R3.0),                                                       00064700
FL11111(10R5.0),                                                        00064800
FL11112(10(X1,A3,X1)),                                                  00064900
FL989(X36,10A3),                                                        00065000
FL179(A6,66A1),                                                         00065100
FL722(/" INDEX           ",10(X4,A1)),                                  00065200
FL723(" NUMBER OF LEVELS",10I5),                                        00065300
FL724(" POPULATION SIZE ",10(X2,A3)),                                   00065400
FL93(/" DESIGN CARD",X5,72A1),                                          00065500
FL130(72A1),                                                            00065600
FL471(X18,72A1),                                                        00065700
               FL71(" (",I1,")",X4),                             %VR&FFC00065800
               FL70(" ",I2,")",X4),                              %VR&FFC00065900
               FL75(A4,X4),                                      %VR&FFC00066000
FL121(12A6),                                                            00066100
               FL805(11(R9.3,A3),X4),                            %VR&FFC00066200
FL821(22A6),                                                            00066300
                                                                        00066400
FL141(" ANALYSIS OF VARIANCE FOR DEPENDENT VARIABLE",I3//"     SOURCE", 00066500
 X13,"SUM OF SQUARES  DEGREES OF  MEAN SQUARE     EXPECTED MEAN SQUAR", 00066600
 "E"/X41,"FREEDOM"//),                                                  00066700
                                                                        00066800
          FL145(I3,X3,2A6,R18.4,X2,I8,R16.4,X5,4(2A6,")"), X1 /         00066900
               3(X67,4(2A6,")"),X1 /)),                          %VR&FFC00067000
FL400(" MEAN",R14.5//),                                          %VRJUNE00067100
FL422(" CELL DEVIATIONS  "),                                            00067200
               PTCH1(X36,10A3),                                         00067300
FTT("CELL MEANS"),                                               %VRJUNE00067400
FL733(/" ",X3,6(A4,2A6)/(X4,A4,2A6,A4,2A6,A4,2A6,A4,2A6,A4,2A6,A4,2A6));00067500
LIST LIST1(JPF,JPC,JNV,JNI,JKM,JNF,JMT,JPCOV);                          00067600
LIST LIST2(JPF,FOR DX1~1 STEP 1 UNTIL 13 DO SVC[DX1]);                  00067700
LIST LIST3(JPC);                                                        00067800
LIST LIST4(JAP,FOR DX1~1 STEP 1 UNTIL 10 DO SVFIN[DX1],FOR DX1~1 STEP 1 00067900
 UNTIL 10 DO SVFOP[DX1]);                                               00068000
LIST LIST5(FOR DX1~1 STEP 1 UNTIL 10 DO SVFOP[DX1]);                    00068100
LIST LIST6(FOR DX1~1 STEP 1 UNTIL JNI DO SVZ2[DX1]);                    00068200
LIST LIST7(JPA,FOR DX1~1 STEP 1 UNTIL 66 DO SVP[DX1]);                  00068300
LIST LIST8(FOR DX1~1 STEP 1 UNTIL JNI DO SVE[DX1]);                     00068400
LIST LIST9(FOR DX1~1 STEP 1 UNTIL JNI DO SVJ1[DX1]);                    00068500
LIST LIST10(FOR DX1~1 STEP 1 UNTIL 66 DO SVP[DX1]);                     00068600
LIST LIST11(FOR DX1~1 STEP 1 UNTIL 72 DO SVP[DX1]);                     00068700
LIST LIST12(JI);                                                        00068800
LIST LIST13(SVG[JI]);                                                   00068900
LIST LIST14(FOR DX1~1 STEP 1 UNTIL 12 DO SVP[DX1]);                     00069000
LIST LIST15(SVP1[JI],SVP2[JI]);                                         00069100
LIST LIST16(FOR DX1~JL0 STEP 1 UNTIL JL1 DO [SVFF[DX1],SVP[DX1]]);      00069200
LIST LIST17(FOR DX1~JM0 STEP 1 UNTIL JM1 DO SVS[DX1]);                  00069300
LIST LIST18(FOR DX1~1 STEP 1 UNTIL JNF DO SVFF[DX1]);                   00069400
LIST LIST19(JJ,SVP1[JJ],SVP2[JJ],SVS[JK],SVID[JJ],JSM,FOR DX1~JL0 STEP  00069500
1 UNTIL JL1 DO SVS[DX1]);                                               00069600
LIST LIST20(SVS[JL]);                                                   00069700
LIST LIST21(FOR DX1~JL8 STEP 1 UNTIL JL9 DO SVS[DX1]);                  00069800
LIST           LIST30(FOR DX1~1 STEP 1 UNTIL JNI DO SVZ2[DX1]);         00069900
LABEL L101,L7777,L111,L113,L20,L7766,L117,L2000,L3821,L103,L107,L5114,  00070000
 L135,L137,L31,L33,L178,L4,L36,L37,L8,L9,L10,L11,L13,L19,L21,L22,       00070100
 L23,L51,L25,L28,L53,L81,L83,L84,L89,L74,L72,L123,L95,L126,L602,L601,   00070200
 L3,L40,L7,L804,L131,L731,L401,L421;                                    00070300
SWITCH SWGO1~L31,L33,L20,L20,L20,L20,L20,L20;                           00070400
SWITCH SWGO2~L4,L36,L37,L8,L9,L10,L11,L20;                              00070500
INTEGER PROCEDURE ADD7090 (AC, SR);                              %VR&FFC00070600
     VALUE     AC, SR;                                           %VR&FFC00070700
     REAL      AC, SR;                                           %VR&FFC00070800
          BEGIN                                                  %VR&FFC00070900
     REAL      OSGN, SGN, MQ, SHIFT, CTR, CHAR;                  %VR&FFC00071000
               OSGN ~ 0 & AC[12:12:1];                           %VR&FFC00071100
               IF SR.[13:8]<AC.[13:8]  THEN                      %VR&FFC00071200
            BEGIN                                                %VR&FFC00071300
                 SGN.[12:1] ~ SR.[12:1];                         %VR&FFC00071400
                 MQ ~ AC;                                        %VR&FFC00071500
                 AC ~ SR;                                        %VR&FFC00071600
                 SR ~ MQ;                                        %VR&FFC00071700
            END ELSE  SGN.[12:1] ~ AC.[12:1];                    %VR&FFC00071800
               MQ ~ 0;                                           %VR&FFC00071900
               MQ.[12:1] ~ AC.[12:1];                            %VR&FFC00072000
               IF SHIFT~SR.[13:8]-AC.[13:8]>63  THEN  AC~0  ELSE %VR&FFC00072100
            BEGIN                                                %VR&FFC00072200
               IF SHIFT>0  THEN                                  %VR&FFC00072300
                 FOR CTR~1  STEP 1  UNTIL SHIFT  DO              %VR&FFC00072400
              BEGIN                                              %VR&FFC00072500
                   MQ~0&MQ[12:12:9]&MQ[22:21:26]&AC[21:47:1];    %VR&FFC00072600
                   AC~0&AC[12:12:9]&AC[22:21:26];                %VR&FFC00072700
              END;                                               %VR&FFC00072800
               AC.[13:8] ~ SR.[13:8];                            %VR&FFC00072900
               CHAR.[13:8] ~ AC.[13:8];                          %VR&FFC00073000
               IF AC.[12:1]=SR.[12:1]  THEN                      %VR&FFC00073100
            BEGIN                                                %VR&FFC00073200
                 AC.[13:35] ~ AC.[21:27] + SR.[21:27];           %VR&FFC00073300
                 IF AC.[13:8]>CHAR.[13:8]  THEN                  %VR&FFC00073400
              BEGIN                                              %VR&FFC00073500
                   MQ~0&MQ[12:12:9]&MQ[22:21:26]&AC[21:47:1];    %VR&FFC00073600
                   AC~0&AC[12:12:9]&AC[22:21:26]& 1[21:47:1];    %VR&FFC00073700
              END;                                               %VR&FFC00073800
                 AC.[13:8] ~ AC.[13:8] + CHAR.[13:8];            %VR&FFC00073900
            END ELSE                                             %VR&FFC00074000
            BEGIN                                                %VR&FFC00074100
                 IF SR.[21:27]>AC.[21:27]  THEN                  %VR&FFC00074200
              BEGIN                                              %VR&FFC00074300
                   AC.[12:1] ~ SR.[12:1];                        %VR&FFC00074400
                   MQ.[12:1] ~ SR.[12:1];                        %VR&FFC00074500
                   IF MQ.[21:27]!0  THEN                         %VR&FFC00074600
                BEGIN                                            %VR&FFC00074700
                     AC.[21:27] ~ SR.[21:27]-                    %VR&FFC00074800
                       REAL(NOT BOOLEAN(AC.[21:27]));            %VR&FFC00074900
                     MQ.[21:27]~REAL(NOT BOOLEAN(MQ.[21:27]))+1; %VR&FFC00075000
                END ELSE  AC.[21:27]~SR.[21:27]-AC.[21:27];      %VR&FFC00075100
              END ELSE  IF SR.[21:27]<AC.[21:27]  THEN           %VR&FFC00075200
                 AC.[21:27] ~ AC.[21:27] - SR.[21:27]  ELSE      %VR&FFC00075300
                   MQ.[12:9] ~ AC ~ OSGN;                        %VR&FFC00075400
            END;                                                 %VR&FFC00075500
               IF AC.[13:8]!0  OR  AC.[21:27]!0  THEN            %VR&FFC00075600
            BEGIN                                                %VR&FFC00075700
                 WHILE AC.[21:1]=0  DO                           %VR&FFC00075800
              BEGIN                                              %VR&FFC00075900
                   AC~0&AC[12:12:9]&AC[21:22:26]&MQ[47:21:1];    %VR&FFC00076000
                   MQ~0&MQ[12:12:9]&MQ[21:22:26];                %VR&FFC00076100
                   AC.[13:8] ~ AC.[13:8] - 1;                    %VR&FFC00076200
              END;                                               %VR&FFC00076300
                 MQ.[13:8] ~ AC.[13:8] - 27;                     %VR&FFC00076400
            END;                                                 %VR&FFC00076500
            END;                                                 %VR&FFC00076600
               ADD7090 ~ AC;                                     %VR&FFC00076700
          END OF ADD7090;                                        %VR&FFC00076800
IF B4 THEN BEGIN                                                        00076900
BEGIN                                                                   00077000
B4~FALSE;                                                               00077100
  FILL DATA[0,*] WITH "PROBLM","FINISH";                                00077200
  JPROB~DATA[0,0];                                                      00077300
  JFINI~DATA[0,1];                                                      00077400
  FILL DATA[0,*] WITH "NO";                                             00077500
  JON~DATA[0,0];                                                        00077600
  FILL DATA[0,*] WITH "INDEX ";                                         00077700
  JQ003HL~DATA[0,0];                                                    00077800
  FILL DATA[0,*] WITH "INF";                                            00077900
  JQ004HL~DATA[0,0];                                                    00078000
  FILL DATA[0,*] WITH OCT1000,OCT400,OCT200,OCT100,OCT40,OCT20,OCT10,   00078100
 OCT4,OCT2,OCT1;                                                        00078200
               FOR DX1~1 STEP 1 UNTIL 10 DO                             00078300
               SVAA[DX1]~DATA[0,DX1-1];                                 00078400
  FILL DATA[0,*] WITH "DESIGN";                                         00078500
  JQ005HL~DATA[0,0];                                                    00078600
  FILL DATA[0,*] WITH ",";                                              00078700
  JQ006HL~DATA[0,0];                                                    00078800
  FILL DATA[0,*] WITH OCT0;                                             00078900
  JQ010CT~DATA[0,0];                                                    00079000
  FILL DATA[0,*] WITH OCT200000000000;                                  00079100
  JQ012CT~DATA[0,0];                                                    00079200
  FILL DATA[0,*] WITH OCT400000000000;                                  00079300
  JQ013CT~DATA[0,0];                                                    00079400
  FILL DATA[0,*] WITH OCT377777777;                                     00079500
  JQ017CT~DATA[0,0];                                                    00079600
  FILL DATA[0,*] WITH OCT221400000000;                                  00079700
  JQ020CT~DATA[0,0];                                                    00079800
  FILL DATA[0,*] WITH OCT214400000000;                                  00079900
  JQ021CT~DATA[0,0];                                                    00080000
  FILL DATA[0,*] WITH OCT233400000000;                                  00080100
  JQ025CT~DATA[0,0];                                                    00080200
  FILL DATA[0,*] WITH OCT234400000000;                                  00080300
  JQ030CT~DATA[0,0];                                                    00080400
  FILL DATA[0,*] WITH " ";                                              00080500
  JQ007HL~DATA[0,0];                                                    00080600
  FILL DATA[0,*] WITH "(";                                              00080700
  JQ008HL~DATA[0,0];                                                    00080800
  FILL DATA[0,*] WITH ")";                                              00080900
  JQ009HL~DATA[0,0];                                                    00081000
  FILL DATA[0,*] WITH "MEAN";                                           00081100
  JQ010HL~DATA[0,0];                                                    00081200
END;                                                                    00081300
COMMENT        ANALYSIS OF VARIANCE  MARCH 1,1966  THIS IS A            00081400
               SIFTED VERSION OF BM08V ORIGINALLY WRITTEN IN FORTRAN II 00081500
               SOME MODIFICATIONS WERE MADE TO MAKE IT OPERABLE AND     00081600
               SLIGHTLY MORE EFFICIENT THAN THE SIFTED VERSION;  %VR    00081700
JKLV~-1;                                                                00081800
JMTP~5;                                                                 00081900
COMMENT        PROBLEM CARD READ IN WITH "PROBLM" IN FIRST 6 COLS;%VR&FF00082000
L101: READ(TAPE5,FL100,LIST1)[FINIS];                                   00082100
L7777: IF (JPF=JPROB) THEN GO TO L111;                                  00082200
IF (JPF=JFINI) THEN GO TO L113;                                         00082300
L20: IF JKLV}0 THEN GO TO L7766;                                        00082400
WRITE(TAPE6,FL114);                                                     00082500
JKLV~1;                                                                 00082600
L7766: READ(TAPE5,FL118,LIST2)[FINIS];                                  00082700
GO TO L7777;                                                            00082800
L113: IF (JMTP=5) THEN GO TO L117;                                      00082900
REWIND(FILESW[JMTP]);                                                   00083000
L117: GO TO FINIS;                                                      00083100
L2000: WRITE(TAPE6,FL2001);                                             00083200
JKLV~1;                                                                 00083300
GO TO L7766;                                                            00083400
L111: JNF~12|MAX(1,JNF);                                                00083500
IF JKLV{0 THEN GO TO L3821;                                             00083600
REWIND(TAPE1);                                                          00083700
               WRITE(TAPE1,*,LIST2);                                    00083800
REWIND(TAPE1);                                                          00083900
               READ(TAPE1,*,LIST1);                                     00084000
L3821: JKLV~-1;                                                         00084100
IF JMT>0 THEN GO TO L103;                                               00084200
JMT~5;                                                                  00084300
L103: IF JMT=JMTP THEN GO TO L107;                                      00084400
IF (JMTP=5) THEN GO TO L107;                                            00084500
REWIND(FILESW[JMTP]);                                                   00084600
L107: JMTP~JMT;                                                         00084700
IF (JPCOV=JON) THEN GO TO L5114;                                        00084800
IF (JMT=5) THEN GO TO L5114;                                            00084900
REWIND(FILESW[JMT]);                                                    00085000
L5114: WRITE(TAPE6[PAGE]);                                              00085100
COMMENT        HEADING IS PRINTED HERE "BMO8V-ANALYSIS OF VAR";  %VR&FFC00085200
       WRITE(TAPE6,FL134,LIST3);                                        00085300
COMMENT        INDEX CARD READ IN WITH "INDEX" IN FIRST 5 COLS.  %VR&FFC00085400
               FIRST READ INTO ARRAYS SVFIN AND SVFOP UNDER R FMT%VR&FFC00085500
               REREAD SVFOP DATA INTO SVZ2 UNDER ALPHA FORMAT;   %VR&FFC00085600
READ(TAPE5[NO],FL191,LIST4)[FINIS];                                     00085700
               READ(TAPE5,PTCH1,LIST30)[FINIS];                         00085800
IF JAP!(JQ003HL) THEN GO TO L20;                                        00085900
JN~JNI;                                                                 00086000
IF JNI|(11-JNI){0 THEN GO TO L2000;                                     00086100
REWIND(TAPE1);                                                          00086200
REWIND(TAPE2);                                                          00086300
JI~1;                                                                   00086400
DO BEGIN                                                                00086500
COMMENT        SVJ1(1-JNI) - INT (NUMERIC LEVELS OF INDICES),    %VR&FFC00086600
               SVIN(U-JNI) - INT (LEVELS OR INDICES IN REV-ORD), %VR&FFC00086700
               SVROF(1-JNI)- 0 WHERE POP SIZE {0 1 WHERE POP>0   %VR&FFC00086800
               SVZ2(1-JNI)- POPULATION SIZE;                     %VR&FFC00086900
  SVJ1[JI]~INT(SVFIN[JI]);                                              00087000
  SVIN[JN]~INT(SVFIN[JI]);                                              00087100
  IF SVFOP[JI]>0 THEN GO TO L137;                                       00087200
COMMENT        AN "INF" IS PUT IN ARRAY SVZ2 IF SVFOP{0 - INDICATE%VR&FC00087300
               THAT POPULATION SIZE IS INFINITE;                 %VR&FFC00087400
  SVROF[JN]~0;                                                          00087500
  SVZ2[JI]~(JQ004HL);                                                   00087600
  GO TO L135;                                                           00087700
  L137: SVROF[JN]~SVFIN[JI]/SVFOP[JI];                                  00087800
  L135: JN~JN-1 END UNTIL (JI~(JI+1))>JNI;                              00087900
COMMENT        DESIGN CARD READ INTO ARRAY SVP UNDER ALPHA FMT   %VR&FFC00088000
               "DESIGN" IN FIRST 6 COLS - LIST OF ONE-CHAR ALPHA %VR&FFC00088100
               NAMES FOR INDICES COLS 7-16 - NESTING SPECS.;     %VR&FFC00088200
READ(TAPE5,FL179,LIST7)[FINIS];                                         00088300
IF JPA!(JQ005HL) THEN GO TO L20;                                        00088400
JL~JNI;                                                                 00088500
JM~0;                                                                   00088600
JI~1;                                                                   00088700
DO BEGIN                                                                00088800
COMMENT        SVFIN(1-JNI)-LEVELS OF INDICES IN REV-ORDER       %VR&FFC00088900
               SVPL(1-JNI)-ALPHA NESTING SPECS REVERSED ORDER    %VR&FFC00089000
               SVE(1-JNI)-NESTING SPECS ORIG ORDER;              %VR&FFC00089100
  SVFIN[JI]~SVIN[JI];                                                   00089200
  JK~FNLWH(SVP[JI]);                                                    00089300
  GO TO SWGO1[JK];                                                      00089400
  L33: SVPL[JL]~SVP[JI];                                                00089500
  JM~JM+1;                                                              00089600
  SVE[JM]~SVP[JI];                                                      00089700
  JL~JL-1;                                                              00089800
  L31: END UNTIL (JI~(JI+1))>10;                                        00089900
IF JL!0 THEN GO TO L20;                                                 00090000
COMMENT        "INDEX" AND NAMES OF INDICES      (SVE)           %VR&FFC00090100
               "NUMBER OF LEVELS" PER INDEX      (SVJ1)          %VR&FFC00090200
               "POPULATION SIZE" PER INDEX       (SVZ2)          %VR&FFC00090300
               "DESIGN CARD",INDICIES,NESTING SPECS. PRINTED OUT;%VR&FFC00090400
WRITE(TAPE6,FL722,LIST8);                                               00090500
WRITE(TAPE6,FL723,LIST9);                                               00090600
WRITE(TAPE6,FL724,LIST6);                                               00090700
WRITE(TAPE6,FL93,LIST10);                                               00090800
SVP[10]~(JQ006HL);                                                      00090900
JCO~1;                                                                  00091000
JLB~-1;                                                                 00091100
JRB~-1;                                                                 00091200
JN~0;                                                                   00091300
JMO~10;                                                                 00091400
JMI~66;                                                                 00091500
COMMENT        CONTROL WORDS:            FORMED BELOW                   00091600
           SVA,SVB                                                      00091700
               SVC,SVD,SVE,SVG;                                  %VR    00091800
L178: JI~JMO;                                                           00091900
DO BEGIN                                                                00092000
  JK~FNLWH(SVP[JI]);                                                    00092100
  GO TO SWGO2[JK];                                                      00092200
     COMMENT        WE HAVE A ",". [L37]                        ;%VR&FFC00092300
  L37: IF JCO{0 THEN GO TO L20;                                         00092400
  JDO~1;                                                                00092500
  JCO~-1;                                                               00092600
  JPE~-1;                                                               00092700
  JLL~1;                                                                00092800
  JNL~0;                                                                00092900
  JN~JN+1;                                                              00093000
  SVA[JN]~JQ010CT;                                                      00093100
  SVB[JN]~JQ010CT;                                                      00093200
  GO TO L4;                                                             00093300
     COMMENT        WE HAVE A "$". [L11]                        ;%VR&FFC00093400
  L11: IF JDO{0 THEN GO TO L20 ELSE GO TO L13;                          00093500
  L13: SVA[JN]~JQ012CT;                                                 00093600
  JDO~-1;                                                               00093700
  GO TO L4;                                                             00093800
     COMMENT        WE HAVE A ")". [L9]                         ;%VR&FFC00093900
  L9: IF JRB{0 THEN GO TO L20;                                          00094000
  JLL~INT(-1);                                                          00094100
  JCO~1;                                                                00094200
  JPE~1;                                                                00094300
  JRB~-1;                                                               00094400
  GO TO L4;                                                             00094500
     COMMENT        WE HAVE A "(". [L8]                         ;%VR&FFC00094600
  L8: IF JLB{0 THEN GO TO L20;                                          00094700
  JNL~-1;                                                               00094800
  JRB~1;                                                                00094900
  JLB~-1;                                                               00095000
  JCO~-1;                                                               00095100
  JPE~-1;                                                               00095200
  GO TO L4;                                                             00095300
  L36: IF JLL{0 THEN GO TO L20;                                         00095400
  JM~1;                                                                 00095500
  DO BEGIN                                                              00095600
    IF (SVP[JI]=SVPL[JM]) THEN GO TO L19;                               00095700
    END UNTIL (JM~(JM+1))>JNI;                                          00095800
  GO TO L20;                                                            00095900
  L19: SVG[1]~SVAA[JM];                                                 00096000
  IF (XPR~(JNL))>0 THEN GO TO L23 ELSE IF XPR=0 THEN GO TO L22 ELSE GO  00096100
   TO L21;                                                              00096200
L22: SVA[JN]~REAL(BOOLEAN(SVG[1]) OR BOOLEAN(JQ013CT) OR BOOLEAN(SVA[JN]00096300
               ));                                                      00096400
  JNL~1;                                                                00096500
  JDO~-1;                                                               00096600
  JLB~1;                                                                00096700
  JCO~1;                                                                00096800
  JPE~1;                                                                00096900
  GO TO L4;                                                             00097000
L23: SVB[JN]~REAL(BOOLEAN(SVB[JN]) OR BOOLEAN(JQ013CT));                00097100
L21: SVB[JN]~REAL(BOOLEAN(SVB[JN]) OR BOOLEAN(SVG[1]));                 00097200
     COMMENT        JACK UP COLUMN POINTER. [L4]                ;%VR&FFC00097300
  L4: END UNTIL (JI~(JI+1))>JMI;                                        00097400
READ(TAPE5,FL130,LIST11)[FINIS];                                        00097500
JMO~1;                                                                  00097600
JMI~72;                                                                 00097700
WRITE(TAPE6,FL471,LIST11);                                              00097800
GO TO L178;                                                             00097900
     COMMENT        WE HAVE A ".". [L10]                        ;%VR&FFC00098000
L10: IF JPE{0 THEN GO TO L20;                                           00098100
SVC[1]~JQ013CT;                                                         00098200
SVD[1]~JQ010CT;                                                         00098300
SVMN[1]~1;                                                              00098400
JJ~1;                                                                   00098500
JK~1;                                                                   00098600
DO BEGIN                                                                00098700
  JJ0~JJ;                                                               00098800
  JI~1;                                                                 00098900
  DO BEGIN                                                              00099000
IF REAL((BOOLEAN(SVB[JK]) AND BOOLEAN(SVC[JI])) OR (BOOLEAN(SVA[JK]) AND00099100
               BOOLEAN(SVD[JI])))!0                                     00099200
     THEN GO TO L51;                                                    00099300
    JJ~JJ+1;                                                            00099400
    IF JJ}100 THEN GO TO L2000;                                         00099500
    SVMN[JJ]~0;                                                         00099600
SVD[JJ]~ REAL(BOOLEAN(SVB[JK]) OR BOOLEAN(SVD[JI]));                    00099700
SVC[JJ]~ REAL(BOOLEAN(SVA[JK]) OR BOOLEAN(SVC[JI]));                    00099800
SVE[JJ]~ REAL((BOOLEAN(SVC[JJ]) OR BOOLEAN(SVD[JJ])) AND BOOLEAN(JQ017CT00099900
               ));                                                      00100000
    L51: END UNTIL (JI~(JI+1))>JJ0 END UNTIL (JK~(JK+1))>JN;            00100100
JNCM~JJ;                                                                00100200
JI~1;                                                                   00100300
DO BEGIN                                                                00100400
IF REAL(BOOLEAN(SVA[JI]) AND BOOLEAN(JQ012CT)) = 0 THEN GO TO L25;      00100500
JH~ REAL((BOOLEAN(SVA[JI]) OR BOOLEAN(SVB[JI])) AND BOOLEAN(JQ017CT));  00100600
  JJ~1;                                                                 00100700
  DO BEGIN                                                              00100800
IF REAL((BOOLEAN(SVE[JJ]) AND (NOT (BOOLEAN(JH)))) OR (BOOLEAN(JH) AND  00100900
       (NOT (BOOLEAN(SVE[JJ])))))=0 THEN GO TO L28;                     00101000
    END UNTIL (JJ~(JJ+1))>JNCM;                                         00101100
  GO TO L20;                                                            00101200
  L28: SVMN[JJ]~1;                                                      00101300
  L25: END UNTIL (JI~(JI+1))>JN;                                        00101400
JU1~JQ020CT;                                                            00101500
JU2~JQ021CT;                                                            00101600
JI~1;                                                                   00101700
DO BEGIN                                                                00101800
  SVE[1]~JQ010CT;                                                       00101900
SVC[JI]~ REAL(BOOLEAN(SVC[JI]) AND BOOLEAN(JQ017CT));                   00102000
SVD[JI]~ REAL(BOOLEAN(SVD[JI]) AND BOOLEAN(JQ017CT));                   00102100
SVG[JI]~ REAL(BOOLEAN(JQ025CT) OR BOOLEAN(SVC[JI]));                    00102200
JX~ REAL(BOOLEAN(SVE[JI]) OR BOOLEAN(JQ025CT));                         00102300
L81: IF REAL(BOOLEAN(JX) AND BOOLEAN(JQ017CT))=0 THEN GO TO L83;        00102400
  JY~JX-1;                                                              00102500
JX~ REAL(BOOLEAN(JX) AND BOOLEAN(JY));                                  00102600
               SVG[JI] ~ ADD7090(SVG[JI], JU1);                  %VR&FFC00102700
  GO TO L81;                                                            00102800
L83: JX~ REAL(BOOLEAN(SVC[JI]) OR BOOLEAN(JQ025CT));                    00102900
L84: IF REAL(BOOLEAN(JX) AND BOOLEAN(JQ017CT))=0 THEN GO TO L53;        00103000
  JY~JX-1;                                                              00103100
JX~ REAL(BOOLEAN(JX) AND BOOLEAN(JY));                                  00103200
               SVG[JI] ~ ADD7090(SVG[JI], JU2);                  %VR&FFC00103300
  GO TO L84;                                                            00103400
  L53: END UNTIL (JI~(JI+1))>JNCM;                                      00103500
JNBUFF~3;                                                               00103600
IF (JMT=3) THEN JNBUFF~4;                                               00103700
JI~1;                                                                   00103800
DO BEGIN                                                                00103900
COMMENT    SVG CONTAINS " (#)" OR " ##)" - THE NUMBER WITHIN PAREN.     00104000
           IS SORTED INTO ASCENDING ORDER (IE.1-17)                     00104100
           SVC,SVD,SVMN ARE SORTED TO BE IN AGREEMENT WITH SVG;         00104200
           JX~JQ030CT;                     %234400000000          VR&FFC00104300
  JK~JI;                                                                00104400
  DO BEGIN                                                              00104500
               IF (JX.[12:1]=1 AND SVG[JK].[12:1]=0)  OR         %VR&FFC00104600
                 (JX.[12:1]=SVG[JK].[12:1]  AND                  %VR&FFC00104700
                   JX.[13:35]{SVG[JK].[13:35])  THEN  GO TO L89; %VR&FFC00104800
    JJ~JK;                                                              00104900
    JX~SVG[JK];                                                         00105000
    L89: END UNTIL (JK~(JK+1))>JNCM;                                    00105100
  SVG[JJ]~SVG[JI];                                                      00105200
COMMENT        FORMAT JI FOR PRINTING WITH "EXPECTED MEAN SQUARE"       00105300
               INFO. WRITE OUT " (N)" OR " NN)", DEPENDING ON           00105400
               MAGNITUDE OF JI (SEE FL71,FL70,FL75). READ THIS BACK     00105500
               INTO SVG DO THAT IT CAN BE RE-WRITTEN LATER UNDER        00105600
               FL805;                                            %VR&FFC00105700
L74: L72:      IF JI}10  THEN                                    %VR&FFC00105800
            BEGIN                                                %VR&FFC00105900
                 SVG[JI] ~ "(  ";                                %VR&FFC00106000
                 SVG[JI].[42:6] ~ ENTIER(JI MOD 10);             %VR&FFC00106100
                 SVG[JI].[36:6] ~ JI DIV 10;                     %VR&FFC00106200
            END ELSE  SVG[JI] ~ JI & " ("[30:36:12];             %VR&FFC00106300
                                                                 %VR&FFC00106400
  JU~SVC[JJ];                                                           00106500
  SVC[JJ]~SVC[JI];                                                      00106600
  SVC[JI]~JU;                                                           00106700
  JU~SVD[JJ];                                                           00106800
  SVD[JJ]~SVD[JI];                                                      00106900
  SVD[JI]~JU;                                                           00107000
  JNUU~SVMN[JI];                                                        00107100
  SVMN[JI]~SVMN[JJ];                                                    00107200
  SVMN[JJ]~JNUU;                                                        00107300
SVE[JI]~REAL(BOOLEAN(SVC[JI]) OR BOOLEAN(SVD[JI])) END UNTIL (JI~(JI+1))00107400
            >JNCM;                                                      00107500
JI~2;                                                                   00107600
COMMENT        THIS IS WHERE WE BUILD THE "SOURCE" COLUMN ALPHA         00107700
               INFO-FIRST 12 LOCS OF SVP ARE BLNKED AND USED     %VR&FFC00107800
               ONE LINE OF SOURCE INFOR DONE AT A TIME -SVPL WITH       00107900
               OCTAL JQ008HL,JQ009HL FILL SVP - SVP IS                  00108000
               WRITTEN ON TAPE &READ OFF AS SVP1&SVP2 (A FMT);   %VR&FFC00108100
DO BEGIN                                                                00108200
  JJ~1;                                                                 00108300
  DO BEGIN                                                              00108400
COMMENT    JQ007HL ~ " "       ;                                 %VR    00108500
                                                                        00108600
                                                                        00108700
    SVP[JJ]~(JQ007HL) END UNTIL (JJ~(JJ+1))>12;                         00108800
  JN~0;                                                                 00108900
  JL~JNI;                                                               00109000
  JJ~1;                                                                 00109100
  DO BEGIN                                                              00109200
IF REAL(BOOLEAN(SVAA[JL]) AND BOOLEAN(SVC[JI]))=0 THEN GO TO L123;      00109300
    JN~JN+1;                                                            00109400
    SVP[JN]~SVPL[JL];                                                   00109500
    L123: JL~JL-1 END UNTIL (JJ~(JJ+1))>JNI;                            00109600
IF REAL(BOOLEAN(SVD[JI]))=0 THEN GO TO L95;                             00109700
  JN~JN+1;                                                              00109800
COMMENT    JQ008HL  ~  "("      ;                                %VR    00109900
  SVP[JN]~(JQ008HL);                                                    00110000
  JL~JNI;                                                               00110100
  JJ~1;                                                                 00110200
  DO BEGIN                                                              00110300
IF REAL(BOOLEAN(SVAA[JL]) AND BOOLEAN(SVD[JI]))=0 THEN GO TO L126;      00110400
    JN~JN+1;                                                            00110500
    SVP[JN]~SVPL[JL];                                                   00110600
    L126: JL~JL-1 END UNTIL (JJ~(JJ+1))>JNI;                            00110700
  JN~JN+1;                                                              00110800
COMMENT    JQ009HL   ~  ")"      ;                               %VR    00110900
  SVP[JN]~(JQ009HL);                                                    00111000
L95:           WRITE(BUFFER[*], FL130, LIST14);                  %VR&FFC00111100
                                                                 %VR&FFC00111200
               READ(BUFFER[*], FL121, LIST15);                   %VR&FFC00111300
  END UNTIL (JI~(JI+1))>JNCM;                                           00111400
                                                                 %VR&FFC00111500
               SVP1[1] ~ "MEAN  ";                               %VR&FFC00111600
               SVP2[1]~"      ";                                 %VR&FFC00111700
JJ~JNCM+1;                                                              00111800
JI~2;                                                                   00111900
DO BEGIN                                                                00112000
  IF SVMN[JI]{0 THEN GO TO L602;                                        00112100
  SVJ1[1]~JJ-1;                                                         00112200
  JJ11~SVJ1[1];                                                         00112300
  JJ~1;                                                                 00112400
  DO BEGIN                                                              00112500
IF REAL(BOOLEAN(SVE[JJ]) AND (NOT(BOOLEAN(SVE[JI]))))!0 THEN GO TO L601 00112600
            ;                                                           00112700
    SVMN[JJ]~1;                                                         00112800
    L601: END UNTIL (JJ~(JJ+1))>JJ11;                                   00112900
  L602: END UNTIL (JI~(JI+1))>JNCM;                                     00113000
JM1~0;                                                                  00113100
COMMENT        EXPECTED MEANS SQUARE NUMBERS ARE BUILT HERE             00113200
               SVLD HAS THE ARRAY LOCATIONS OF SVS WHERE THE            00113300
               EMS WHICH CORRESPONDS TO MEAN AND SOURCE INDEX           00113400
               ARE STORED;                                       %VR    00113500
JM~1;                                                                   00113600
DO BEGIN                                                                00113700
  SVLD[JM]~JM1;                                                         00113800
  JL~1;                                                                 00113900
  JN~1;                                                                 00114000
  DO BEGIN                                                              00114100
IF REAL(BOOLEAN(SVE[JM]) AND  NOT (BOOLEAN(SVE[JN])))!0 THEN GO TO L3;  00114200
    SVFF[JL]~1;                                                         00114300
    JK~1;                                                               00114400
    DO BEGIN                                                            00114500
IF REAL(BOOLEAN(SVAA[JK]) AND (BOOLEAN(SVD[JN]) OR BOOLEAN(SVE[JM])))!0 00114600
      THEN  GO TO L40;                                                  00114700
IF REAL(BOOLEAN(SVAA[JK]) AND BOOLEAN(SVC[JN]))!0 THEN GO TO L7;        00114800
      SVFF[JL]~SVFF[JL]|SVFIN[JK];                                      00114900
      GO TO L40;                                                        00115000
      L7: SVFF[JL]~SVFF[JL]|(1-SVROF[JK]);                              00115100
      L40: END UNTIL (JK~(JK+1))>JNI;                                   00115200
    IF SVFF[JL]=0 THEN GO TO L3;                                        00115300
    SVP[JL]~SVG[JN];                                                    00115400
    JL~JL+1;                                                            00115500
    L3: END UNTIL (JN~(JN+1))>JNCM;                                     00115600
  JL1~0;                                                                00115700
  JN2~JL-1;                                                             00115800
  L804: JN3~MIN(JN2,11);                                                00115900
  JN2~JN2-JN3;                                                          00116000
  JL0~JL1+1;                                                            00116100
  JL1~JL1+JN3;                                                          00116200
               WRITE(BUFFER[*],FL805,LIST16);                    %VR&FFC00116300
                                                                 %VR&FFC00116400
  JM0~JM1+1;                                                            00116500
  JM1~JM1+2|JN3;                                                        00116600
COMMENT        SVFF AND SVP (JL0-JL1) WRITTEN ON TAPE            %VR&FFC00116700
               SVS(JMO TO JM1) FILLED WITH SVFF AND SVP          %VR&FFC00116800
               SVFF - R FMT,  SVP - A FMT;                       %VR&FFC00116900
               READ(BUFFER[*],FL821,LIST17);                     %VR&FFC00117000
                                                                 %VR&FFC00117100
  IF JN2>0 THEN GO TO L804;                                             00117200
  END UNTIL (JM~(JM+1))>JNCM;                                           00117300
BEGIN                                                            %VR    00117400
INTEGER    DUMMY;                                                %VR    00117500
SVLD[JNCM+1]~JM1;                                                %VR    00117600
OBJECTFMTGEN(QUIXOTE);                                           %VR    00117700
JM2P2~JM1;                                                              00117800
JM1P1~JM1+1;                                                            00117900
COMMENT    JM1P1 IS THE NEXT LOCATION IN SVS AT WHICH DATA              00118000
           MAY BE STORED - JM1P1= GLOBAL INTEGER USED BY SUBROUT ALSO;  00118100
COMMENT    SRSS FILLS SVS[JM1P1-JL] WITH " X(......)" FOR CELL          00118200
           DEVIATIONS - SVMN=0 WILL ELIMINATE THOSE NOT CALLED FOR      00118300
           ON DESIGN CARD;                                       %VR    00118400
SRSS(JNCM,JNI,SVE,SVLQ,SVPL,SVAA);                                      00118500
JM1P1~JM1P1+SVLQ[JNCM+1]+1;                                             00118600
JI~JM1P1;                                                               00118700
DO BEGIN                                                                00118800
SVS[JI]~0 END UNTIL (JI~(JI+1))>10000;                                  00118900
JILL~15000-JM1P1;                                                       00119000
COMMENT    SRANOVA CALCULATES CELL MEANS(JMT=5)-SVIL+JM1P1-1+JI         00119100
           CONTAINS THE LOCATIONS AT WHICH THE CELL MEANS ARE FOUND     00119200
           CELL MEANS ARE PRINTED BY SRPMEANS;                   %VR    00119300
SRANOVA(SVAA,SVMN,SVE,SVIN,SVIL,SVFF,SVID,JNI,JNCM,JKM,JMT,JNV,JILL);   00119400
IF JILL{0 THEN GO TO L2000;                                             00119500
JNVV~JNV;                                                               00119600
JNN~JNVV|JNCM;                                                          00119700
END;                                                                    00119800
L131:                                                                   00119900
BEGIN                                                                   00120000
COMMENT        INTEGER DUMMY BLOCKS ARE ADDED TO CUT THE SIZE OF        00120100
               THE SEGMENT TO PREVENT OVERFLOW OR SIZE EXCEEDED; %VR    00120200
INTEGER DUMMY;                                                   %VR    00120300
JJ~1;                                                                   00120400
DO BEGIN                                                                00120500
  JI~1;                                                                 00120600
  DO BEGIN                                                              00120700
IF SVMN[JI]!0 THEN BEGIN                                                00120800
JL1~SVIL[JI]+JJ;                                                 %VR    00120900
    JM~JL1+SVMN[JI];                                                    00121000
END;                                                                    00121100
END UNTIL JI~JI+1 >JNCM END UNTIL JJ~JJ+1>JNV;                          00121200
END;                                                                    00121300
JI~1;                                                                   00121400
DO BEGIN                                                                00121500
  JK~JM1P1+JI-1;                                                        00121600
  WRITE(TAPE6[PAGE]);                                                   00121700
  WRITE(TAPE6,FL141,LIST12);                                            00121800
COMMENT        THIS STATEMENT PRINTS OUT THE TITLE "ANALYSIS OF VARIANCE00121900
               " AND THE NUMBER OF DEPENDENT VARIABLES;          %VR&FFC00122000
  JJ~1;                                                                 00122100
  DO BEGIN                                                              00122200
IF SVID[JJ]=0 THEN JSM~0 ELSE                                    %VRJUNE00122300
JSM~SVS[JK]/(SVID[JJ]);                                                 00122400
    JL0~SVLD[JJ]+1;                                                     00122500
    JL1~SVLD[JJ+1];                                                     00122600
COMMENT        FIRST PAGE OF OUTPUT WITH DESIGN,EXPECT MEAN SQR; %VR&FFC00122700
COMMENT        LIST19 =JJ,SVP1[JJ],SVP2[JJ],SVS[JK],SVID[JJ],JSM,   %VR 00122800
               SVS[JL0-JL1]                                         %VR 00122900
               FORMAT 145=I3,X3,2A6,R18.4,I5,R16.4,X2,5(2A6)/       %VR 00123000
               X59,5(2A6)/X59,5(2A6) JL0=0,JL1=11                   %VR 00123100
           SVLD HAS JM1 - EXPECTED MEAN SQUARE LOCATIONS                00123200
           SVLQ - LOCATIONS OF SRSS  "X(......)   "                     00123300
               SVLQ HAS DIFF OF JLLL (AS IT INCREASES) AND JMM(CONST);  00123400
    WRITE(TAPE6,FL145,LIST19);                                          00123500
    JK~JK+JNVV END UNTIL (JJ~(JJ+1))>JNCM;                              00123600
JL~SVIL[1]+JI+JM1P1-1;                                              %VR 00123700
COMMENT        SVS[JL] CONTAINS THE MEAN-FL400 PRINTS MEAN;      %VRJUNE00123800
  WRITE(TAPE6,FL400,LIST20);                                            00123900
DUM~0;                                                           %VRJUNE00124000
  JJ~2;                                                                 00124100
  DO BEGIN                                                              00124200
    IF SVMN[JJ]=0 THEN GO TO L401;                                      00124300
JL1~SVIL[JJ]+JI+JM1P1-2;                                         %VR    00124400
COMMENT        FTT PRINTS "CELL MEANS";                          %VRJUNE00124500
IF DUM=0 THEN BEGIN WRITE(LINE,FTT); DUM~1; END;                 %VRJUNE00124600
COMMENT        SRPMEANS SETS UP THE INDICES WHICH DEFINE THE CELL MEANS 00124700
               AND DEVS - SVE CONTAINS THE COMBINATION OF INDICES TO BE 00124800
               USED IN ORDER - SOME ARE SKIPPED BY SVMN=0 (SRPMEANS NOT 00124900
               ENTERED THEN) SRPMEANS PRINTS OUT THESE INDICES AND THEIR00125000
               LEVELS - ALSO PRINTS THE "MATE" CELL MEANS(SRANOUA-SVS); 00125100
SRPMEANS(SVE[JJ],SVAA,SVPL,SVIN,JNI,JNV,JL1);                    %VR    00125200
    L401: END UNTIL (JJ~(JJ+1))>JNCM;                                   00125300
  JJYP~1;                                                               00125400
  JJ~2;                                                                 00125500
  DO BEGIN                                                              00125600
    IF SVMN[JJ]=0 THEN GO TO L731;                                      00125700
JL1~SVIL[JJ]+JI+JM1P1-2;                                         %VR    00125800
    JL8~SVLQ[JJ]+1+JM2P2;                                               00125900
    JL9~SVLQ[JJ+1]+JM2P2;                                               00126000
    IF JJYP{0 THEN GO TO L421;                                          00126100
    JJYP~0;                                                             00126200
COMMENT    SRANOVA CALCULATES CELL DEVS (JMT=-1)-SVIL[]+JM1P1-1+JI      00126300
           CONTAINS LOCS WHERE CELL DEVS ARE -CELL DEVS ARE STORED      00126400
           ON TOP OF CELL MEANS (SAME ADDRESSES)-SRPMEANS PRINTS;       00126500
SRANOVA(SVAA,SVMN,SVE,SVIN,SVIL,SVFF,SVID,JNI,JNCM,JKM,-JI,JNV,JILL);   00126600
COMMENT        "CELL DEVIATIONS" HEADING;                        %VR&FFC00126700
    WRITE(TAPE6,FL422);                                                 00126800
    L421: WRITE(TAPE6,FL733,LIST21);                                    00126900
SRPMEANS(SVE[JJ],SVAA,SVPL,SVIN,JNI,JNV,JL1);                    %VR    00127000
    L731: END UNTIL (JJ~(JJ+1))>JNCM END UNTIL (JI~(JI+1))>JNV;         00127100
GO TO L101;                                                             00127200
 END END;                                                               00127300
COMMENT  INITIALIZING BLOCK;                                            00127400
  XPR~Q~K~0;                                                            00127500
B1~TRUE;                                                                00127600
B2~TRUE;                                                                00127700
B3~TRUE;                                                                00127800
B4~TRUE;                                                                00127900
MAINPRO; FINIS:                                                         00128000
END.                                                                    00128100
                LAST CARD ON CRDIMG TAPE                                99999999
