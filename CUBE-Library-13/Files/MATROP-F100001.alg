          BEGIN                                                         00000000
COMMENT        MATROP/STATMAN  FEB 21,1969;                             00000100
COMMENT        MATRIX OPERATIONS VERSION WRITTEN AND ORGANIZED BY       00000200
               JOHN H. WACKWITZ   JULY 1966:  THIS PROGRAM PERFORMES    00000300
               ANY NUMBER OF MATRIX OPERATIONS SINGLY OR IN SERIES  ;   00000400
FILE IN        CARD  0 (2,10) ;                                         00000500
FILE OUT       LINE  4 (2,15) ;                                         00000600
FILE OUT       PUNCH 0 (2,15) ;                                         00000700
FILE           TAPE2 2 (2,100, SAVE 10) ;                               00000800
FILE           TEM   2 (2,100);                                         00000900
FILE           TAPE3 2 (2,100, SAVE 10) ;                               00001000
FILE           TAPE4 2 (2,100, SAVE 10) ;                               00001100
FILE           CORMAT 2(2,100, SAVE 10) ;                               00001200
FILE           FACMAT 2(2,100, SAVE 10) ;                               00001300
FILE           ROTMAT 2(2,100, SAVE 10) ;                               00001400
INTEGER        ROWS,COLS;                                               00001500
SWITCH FILE    FILESW  ~ TAPE2,TAPE2,TAPE2,TAPE3,TAPE4,CORMAT,FACMAT,   00001600
                         ROTMAT ;                                       00001700
INTEGER        NCOLA, NCOLB, NROWA, NROWB, INA, INB, IBEG, JBEG,        00001800
               IEND, JEND, LIN, CRD, TPE, NPROG, MISSL, DX1, I, J, K, L;00001900
INTEGER        MISS1, MISS2, MISS3, MISS4, MISS5, MISS6 ;               00002000
REAL           CONST, DET, EPS ;                                        00002100
INTEGER ARRAY  ID[0:11] ;                                               00002200
BOOLEAN        SAVEA, SAVEB ;                                           00002300
FORMAT        FIPAR(12A6/I2,X2,4(I1,2I4),R10.0,3I1,2L1,4I1);            00002400
LIST           LIPAR(FOR DX1 ~ 0 STEP 1 UNTIL 11 DO ID[DX1],NPROG,INA,  00002500
               NROWA,NCOLA,INB,NROWB,NCOLB,MISS1,IBEG,IEND,MISS2,JBEG,  00002600
               JEND,CONST,LIN,CRD,TPE,SAVEA,SAVEB,MISS3,MISS4,MISS5,    00002700
               MISS6) ;                                                 00002800
FORMAT         TITLE(//X51,"MATRIX OPERATIONS"//);                      00002900
FORMAT                                                                  00003000
               ERR1(//X10,"*** INPUT TAPE INCORRECTLY SPECIFIED ***"),  00003100
               ERR2(//X10,"*** OUTPUT TAPE INCORRECTLY SPECIFIED **"),  00003200
               ERR3(//X10,"*** MATRICES NOT CONFORMABLE TO ADDITION ",  00003300
                          "OR SUBTRACTION ***"),                        00003400
               ERR4(//X10,"*** INCORRECT NUMBER OF ROWS OR COLUMNS ",   00003500
                          "TO BE DELETED ***"),                         00003600
               ERR5(//X10,"*** AUGMENT MATRIX NOT CONFORMABLE TO ",     00003700
                          "ORIGINAL MATRIX ***"),                       00003800
               ERR6(//X10,"*** SPECIFY IF VECTOR IS ROW OR COLUMN ***"),00003900
               ERR7(//X10,"*** MATRICES NOT CONFORMABLE TO MULTIPLICAT",00004000
                              "ION ***"),                               00004100
               ERR8(//X10,"*** MATRIX IS SINGULAR  DETERMINANT = 0 ",   00004200
                          " NO INVERSE EXISTS ***"),                    00004300
               ERR9(//X10,"*** MATRIX IS NOT SQUARE NO INVERSE ***"),   00004400
               ERR10(//X10,"**** WRONG N INDICATED ON INPUT CARD ***"), 00004500
               ERR11(//X10,"MATRIX NOT SQUARE CAN NOT BE SCALED"//) ,   00004600
               FERR("EITHER ROW OR COLUMN NUMBERS HAVE BEEN LEFT EMPTY "00004601
               ,"ON PARAMETER CARD. NPROG = ",I5),         %VR 2/7/69   00004602
               ERR12(//X10,"NROWA!NROWB OR NCOLA!NCOLB"//),             00004610
               FDET (//X10,"DETERMINANT = ",R10.4//),                   00004700
               OKTL (100O);                                             00004800
LABEL          START, FINIS, NEXTSTEP;                                  00004900
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEA00005000
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",00005100
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"///),LAYT(//"EXECUTION TIM00005200
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///00005300
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=00005400
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAY00005500
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FE00005600
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.00005700
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];EN00005800
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THEN00005900
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,00006000
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       00006100
LABEL          E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11 ;                     00006200
PROCEDURE      ERROR (N) ;                                              00006300
VALUE          N ;                                                      00006400
INTEGER        N ;                                                      00006500
          BEGIN                                                         00006600
SWITCH         SWERR ~ E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11 ;             00006700
               GO TO SWERR[N] ;                                         00006800
          END  OF ERROR ;                                               00006900
START:         DATELINE("MATROP") ;                                     00007000
               WRITE(LINE,TITLE) ;                                      00007100
NEXTSTEP:      READ(CARD,FIPAR,LIPAR) [FINIS] ;                         00007200
               IF NPROG=5 OR NPROG=24 THEN ROWS~COLS~IF NCOLA>NROWA     00007300
                  THEN NCOLA ELSE NROWA                                 00007400
          ELSE IF NPROG=25 OR NPROG=27 THEN ROWS~COLS~NCOLA             00007500
          ELSE IF NPROG=13 THEN BEGIN ROWS~IF NROWA>NCOLA THEN NROWA    00007600
               ELSE NCOLA;    COLS~IF NCOLB>ROWS THEN NCOLB ELSE ROWS;  00007700
               END                                                      00007800
          ELSE IF NPROG=26 THEN BEGIN ROWS~NCOLB;                       00007900
                 COLS~NCOLA; END                                        00008000
          ELSE IF NPROG=28 THEN BEGIN ROWS~NROWA; COLS~IF NCOLA>NCOLB   00008100
               THEN NCOLA ELSE NCOLB; END                               00008200
          ELSE IF NPROG=30 OR NPROG=20 THEN BEGIN ROWS~IF               00008300
               NROWB>NCOLA THEN NROWB ELSE NCOLA;  ROWS~IF ROWS>NROWA   00008400
               THEN ROWS ELSE NROWA;  IF NPROG=30 THEN ROWS~ROWS+NROWB; 00008500
               COLS~NCOLA;                                              00008600
               END                                                      00008700
          ELSE IF NPROG=18 THEN BEGIN ROWS~IF IBEG>IEND THEN IBEG ELSE  00008800
               IEND;  IF NROWA>ROWS THEN ROWS~NROWA;  IF NROWB>ROWS THEN00008900
               ROWS~NROWB;  IF NCOLA>ROWS THEN ROWS~NCOLA;              00009000
               IF NCOLB>ROWS THEN ROWS~NCOLB; COLS~ROWS; END            00009100
          ELSE IF NPROG=8 THEN BEGIN IF IEND>0 THEN BEGIN               00009200
               ROWS~NROWA+NROWB;  COLS~NCOLA; END ELSE IF JEND>0 THEN   00009300
               BEGIN ROWS~NROWA; COLS~NCOLA+NCOLB; END ELSE ERROR(5);END00009400
          ELSE IF NPROG=9 THEN BEGIN IF NROWA=1 THEN ROWS~COLS~         00009500
               NCOLA ELSE IF NCOLA =1 THEN ROWS~COLS~NROWA ELSE         00009600
               ERROR(6); END                                            00009700
          ELSE BEGIN ROWS~NROWA; COLS~NCOLA; END;                       00009800
               IF ROWS=0 OR COLS=0 THEN BEGIN                           00009900
               WRITE(LINE,FERR,NPROG);                     %VR 2/7/69   00010000
                                                           %VR 2/7/69   00010100
               GO TO FINIS;                                             00010200
               END;                                                     00010300
          BEGIN                                                         00010400
COMMENT        THIS INNER BLOCK CREATED 12-23-66 BY WHE TO DYNAMICALLY  00010500
               DECLARE THE ARRAY A;                                     00010600
COMMENT        ARRAY A[0:NROWA,0:NCOLA] WAS TRIED BUT SEVERAL           00010700
               PROCEDURES INCLUDING MULTAB,TRANSPOSE,& VECTODIAG WILL   00010800
               NOT WORK WITH THIS.  DEC WAS CHANGED BACK TO             00010900
               A[0:150,0:150] PENDING FURTHER ANALYSIS                  00011000
                         WHE & FFC  12-27-66;                           00011100
LABEL          L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16;  00011200
LABEL          L17,L18,L19,L20,L21,L22,L23,L24,L25,L26,L27,L28,L29,L30; 00011300
LABEL          L31,L32;                                        %MC 2/69 00011400
SWITCH         SWPROG ~ L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,     00011500
                        L14,L15,L15,L17,L18,L19,L20,L21,L22,L23,L24,    00011600
               L25,L26,L27,L28,L29,L30,L31,L32;                %MC 2/69 00011700
LABEL          LOUT;                                                    00011800
REAL ARRAY     A[0:ROWS,0:COLS];                                        00011900
REAL ARRAY                  B[0:150],C[0:150],D[0:150],E[0:150];        00012000
FORMAT         SWFA(/////////////////////////////////////////////////   00012100
                    /////////////////////////////////////////////////   00012200
                    /////////////////////////////////////////////////   00012300
                    /////////////////////////////////////////////////); 00012400
FORMAT         SWFB(/////////////////////////////////////////////////   00012500
                    /////////////////////////////////////////////////   00012600
                    /////////////////////////////////////////////////   00012700
                    /////////////////////////////////////////////////); 00012800
LIST           LISTB(FOR DX1 ~ 1 STEP 1 UNTIL NCOLB DO B[DX1]) ,        00012900
               LISTA1(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO B[DX1]) ;       00013000
STREAM PROCEDURE    REINITIAL (FMT);                                    00013100
          BEGIN                                                         00013200
     LOCAL     STROKE;                                                  00013300
               SI ~FMT;                                                 00013400
               DI ~ LOC STROKE;                                         00013500
               DS ~ 8 LIT "F00000+0";                                   00013600
               SI ~ LOC STROKE;                                         00013700
               DI ~ FMT;                                                00013800
               DI ~ DI + 8;                                             00013900
               3(63(DS ~ WDS; SI ~ LOC STROKE););                       00014000
               7(DS ~ WDS;  SI ~ LOC STROKE);                           00014100
               DS ~ 8 LIT "+0000036";                                   00014200
               DS ~ 8 LIT "0000HY00";                                   00014300
          END  OF REINITIAL;                                            00014400
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00014500
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00014600
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00014700
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00014800
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00014900
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00015000
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00015100
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00015200
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00015300
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00015400
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00015500
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00015600
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00015700
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00015800
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00015900
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00016000
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00016100
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00016200
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300016300
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00016400
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00016500
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00016600
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00016700
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00016800
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00016900
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00017000
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00017100
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100017200
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00017300
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00017400
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00017500
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00017600
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00017700
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00017800
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00017900
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00018000
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00018100
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00018200
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00018300
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00018400
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00018500
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00018600
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00018700
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00018800
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00018900
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00019000
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00019100
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00019200
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00019300
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00019400
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00019500
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00019600
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00019700
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00019800
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00019900
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800020000
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00020100
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00020200
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00020300
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00020400
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00020500
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00020600
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00020700
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00020800
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00020900
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00021000
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00021100
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00021200
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00021300
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00021400
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00021500
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00021600
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00021700
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00021800
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00021900
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00022000
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00022100
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00022200
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00022300
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00022400
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00022500
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00022600
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00022700
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00022800
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00022900
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00023000
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00023100
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00023200
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00023300
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00023400
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00023500
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00023600
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00023700
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100023800
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00023900
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00024000
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00024100
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00024200
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00024300
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000024400
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00024500
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00024600
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00024700
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00024800
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00024900
PROCEDURE      READAMAT ;                                               00025000
          BEGIN                                                         00025100
COMMENT        READ INPUT A MATRIX FROM CARDS OR INTERM TAPE **** ;     00025200
LIST           REEDA(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO A[I,DX1]) ;      00025300
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00025400
               IF INA  = 1 THEN                                         00025500
               READ(CARD,SWFA,REEDA) [E10]  ELSE                        00025600
               READ(FILESW[INA] ,OKTL,REEDA)[E10] ;                     00025700
          END  OF READAMAT ;                                            00025800
PROCEDURE      MPRINT (NROW, NCOL, R, ID) ;                             00025900
VALUE          NROW, NCOL ;                                             00026000
INTEGER        NROW, NCOL ;                                             00026100
REAL ARRAY     R[0,0] ;                                                 00026200
INTEGER ARRAY  ID[0] ;                                                  00026300
          BEGIN                                                         00026400
INTEGER        J1, J2, JSEC ;                                           00026500
FORMAT         F(//"PRO-        A - MATRIX        B - MATRIX       CON",00026600
                   "STANT       DELETE ROW     DELETE COL     PRINT   ",00026700
                   "PUNCH   STORE"/"GRAM      IN    R     C     IN    ",00026800
                   "R     C",X21,   "BEG    END     BEG    END"/        00026900
                                   X1,I2,X8,I1,2(X3,I3),X5,I1,2(X3,I3), 00027000
                    X4,R10.0,X6,I3,X4,I3,X5,I3,X4,I3,3(X7,I1)//) ;      00027100
FORMAT         HEAD (/X5,12A6//X12,"SECTION",I3//),                     00027200
               FI   (/X3,"ROW",X3,10I11) ,                              00027300
               FDATA(I6,X4,10R11.2) ;                                   00027400
LIST           LI(FOR DX1~ J1 STEP 1 UNTIL J2 DO DX1),                  00027500
               LID1 (FOR DX1 ~ 0 STEP 1 UNTIL 11 DO ID[DX1],JSEC) ,     00027600
               LID2(NPROG,INA,NROWA,NCOLA,INB,NROWB,NCOLB,CONST,IBEG,   00027700
                    IEND,JBEG,JEND,LIN,CRD,TPE) ,                       00027800
               LDATA(I,FOR DX1 ~ J1 STEP 1 UNTIL J2 DO R[I,DX1]) ;      00027900
LABEL          LSTART;                                                  00028000
               WRITE (LINE,F,LID2) ;                                    00028100
               J1 ~ 0 ;                                                 00028200
               J2 ~ 0 ;                                                 00028300
               JSEC ~ 0 ;                                               00028400
LSTART:        J1 ~ J2 + 1 ;                                            00028500
               J2 ~ J1 + 9 ;                                            00028600
               IF J2 > NCOL THEN                                        00028700
               J2 ~ NCOL ;                                              00028800
               JSEC ~ JSEC + 1 ;                                        00028900
               WRITE(LINE,HEAD,LID1) ;                                  00029000
               WRITE(LINE,FI,LI) ;                                      00029100
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00029200
               WRITE(LINE,FDATA,LDATA) ;                                00029300
               IF J2 < NCOL THEN                                        00029400
          BEGIN                                                         00029500
               WRITE(LINE[PAGE]);                                       00029600
               GO TO LSTART ;                                           00029700
          END ;                                                         00029800
               WRITE(LINE[PAGE]) ;                                      00029900
          END  OF MPRINT ;                                              00030000
PROCEDURE      MPUNCH ( NROW, NCOL, R, ID, L) ;                         00030100
VALUE          NROW, NCOL, L ;                                          00030200
INTEGER        NROW, NCOL, L;                                           00030300
REAL ARRAY     R[0,0] ;                                                 00030400
INTEGER ARRAY  ID[0] ;                                                  00030500
          BEGIN                                                         00030600
INTEGER        J1,J2,JSEC,L1 ;                                          00030700
FORMAT         FID (12A6) ;                                             00030800
LIST           LID (FOR DX1 ~ 0 STEP 1 UNTIL 11 DO ID[DX1]) ;           00030900
FORMAT         FSMAL(2I4,X2,        10R7.2) ,                           00031000
               FLMED(2I4,X2,    10R7.4),                                00031100
               FINT(2I4,24I3),                                          00031200
               FLRGE(2I4,X2,        5R14.7) ;                           00031300
LIST           LDATA(          I, JSEC,FOR DX1 ~ J1 STEP 1 UNTIL J2 DO  00031400
                    R[I,DX1]) ;                                         00031500
LABEL          PSTART ;                                                 00031600
               WRITE(PUNCH,FID,LID) ;                                   00031700
               IF L = 1 OR L = 2 THEN                                   00031800
               L1 ~ 9 ELSE                                              00031900
               IF L=4 THEN                                              00032000
               L~23 ELSE                                                00032100
               L1 ~ 4 ;                                                 00032200
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00032300
          BEGIN                                                         00032400
               J1 ~ 0 ;                                                 00032500
               J2 ~ 0 ;                                                 00032600
               JSEC ~ 0 ;                                               00032700
PSTART:        J1 ~ J2 + 1 ;                                            00032800
               J2 ~ J1 + L1 ;                                           00032900
               IF J2 > NCOL THEN                                        00033000
               J2 ~ NCOL ;                                              00033100
               JSEC ~ JSEC + 1 ;                                        00033200
               IF L = 1 THEN                                            00033300
               WRITE(PUNCH,FSMAL,LDATA)  ELSE                           00033400
               IF L = 2 THEN                                            00033500
               WRITE(PUNCH,FLMED,LDATA) ELSE                            00033600
               IF L=3 THEN                                              00033700
               WRITE(PUNCH,FLRGE,LDATA)                                 00033800
               ELSE WRITE(PUNCH,FINT,LDATA);                            00033900
               IF J2 < NCOL THEN                                        00034000
               GO TO PSTART ;                                           00034100
          END ;                                                         00034200
          END  OF MPUNCH ;                                              00034300
PROCEDURE      MTAPE (NROW, NCOL, R, SWF) ;                             00034400
VALUE          NROW, NCOL ;                                             00034500
INTEGER        NROW, NCOL ;                                             00034600
REAL ARRAY     R[0,0] ;                                                 00034700
FILE OUT       SWF ;                                                    00034800
          BEGIN                                                         00034900
LIST           LTAPE (FOR DX1 ~ 1 STEP 1 UNTIL NCOL DO R[I,DX1]) ;      00035000
               WRITE(SWF,OKTL,NROW,NCOL) ;                              00035100
               FOR I ~ 1 STEP 1 UNTIL NROW DO                           00035200
               WRITE(SWF,OKTL,LTAPE) ;                                  00035300
               REWIND(SWF) ;                                            00035400
          END  OF MTAPE ;                                               00035500
PROCEDURE      ADD ;                                                    00035600
          BEGIN                                                         00035700
COMMENT        MATRIX ADDITION:  A ~ A + B  **** ;                      00035800
               IF NROWA ! NROWB   AND  NCOLA ! NCOLB                    00035900
               THEN                                                     00036000
               ERROR(3) ;                                               00036100
               FOR I ~ 1 STEP 1 UNTIL NROWB DO                          00036200
          BEGIN                                                         00036300
               IF INB > 1 THEN                                          00036400
               READ(FILESW[INB],OKTL,LISTB)[FINIS]   ELSE               00036500
               READ(CARD,SWFB,LISTB)[FINIS] ;                           00036600
               FOR J ~ 1 STEP 1 UNTIL NCOLB DO                          00036700
               A[I,J]  ~ A[I,J] + B[J] ;                                00036800
          END;                                                          00036900
          END  OF ADD ;                                                 00037000
PROCEDURE      SUB ;                                                    00037100
          BEGIN                                                         00037200
COMMENT        MATRIX SUBTRACTION:  A ~ A - B **** ;                    00037300
               IF NROWA ! NROWB   AND   NCOLA ! NCOLB                   00037400
               THEN                                                     00037500
               ERROR( 3) ;                                              00037600
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00037700
          BEGIN                                                         00037800
               IF INB > 1 THEN                                          00037900
               READ(FILESW[INB],OKTL,LISTB)[FINIS]  ELSE                00038000
               READ(CARD,SWFB,LISTB)[FINIS] ;                           00038100
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00038200
               A[I,J] ~ A[I,J] - B[J] ;                                 00038300
          END ;                                                         00038400
          END  OF SUB ;                                                 00038500
PROCEDURE      IDSTORE ;                                                00038600
          BEGIN                                                         00038700
COMMENT        PUT IDENTITY MATRIX IN STORAGE:  A ~ I  **** ;           00038800
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00038900
          BEGIN                                                         00039000
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00039100
               A[I,J] ~ 0 ;                                             00039200
               A[I,I] ~ 1 ;                                             00039300
          END ;                                                         00039400
          END  OF IDSTORE ;                                             00039500
PROCEDURE      TRANSPOSE ;                                              00039600
          BEGIN                                                         00039700
COMMENT        MATRIX TRANSPOSE:  A ~ A TRANSPOSE  **** ;               00039800
INTEGER        K, K1 ;                                                  00039900
               FOR K ~ 1 STEP 1 UNTIL NCOLA DO                          00040000
          BEGIN                                                         00040100
               K1 ~ K + 1;                                              00040200
               FOR J ~ K1 STEP 1 UNTIL NCOLA DO                         00040300
               IF K{NROWA THEN                                          00040400
               B[J] ~ A[K,J] ;                                          00040500
               FOR I ~ K1 STEP 1 UNTIL NROWA DO                         00040600
               A[K,I] ~ A[I,K] ;                                        00040700
                                                                        00040800
                                                                        00040900
                                                                        00041000
                                                                        00041100
                                                                        00041200
               FOR J ~ K1 STEP 1 UNTIL NCOLA DO                         00041300
               IF K{NROWA THEN                                          00041400
               A[J,K] ~ B[J] ;                                          00041500
          END ;                                                         00041600
               K ~ NCOLA ;                                              00041700
               NCOLA ~ NROWA ;                                          00041800
               NROWA ~ K ;                                              00041900
          END  OF TRANSPOSE ;                                           00042000
PROCEDURE      CONSTTIMESA ;                                            00042100
          BEGIN                                                         00042200
COMMENT        CONSTANT TIMES MATRIX :   A ~ KA  **** ;                 00042300
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00042400
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00042500
               A[I,J] ~ CONST | A[I,J] ;                                00042600
          END  OF CONSTTIMESA ;                                         00042700
PROCEDURE      DELETE ;                                                 00042800
          BEGIN                                                         00042900
COMMENT        DELETE COLUMNS AND/OR ROWS OF MATRIX *** ;               00043000
INTEGER        NDEL;                                                    00043100
               IF JBEG ! 0 THEN                                         00043200
          BEGIN                                                         00043300
               JBEG~JBEG+1; JEND~JEND+1;                                00043400
               NDEL ~ JEND - JBEG + 1 ;                                 00043500
               IF NDEL}NCOLA OR NDEL<1 OR JEND-1>NCOLA THEN             00043600
               ERROR(4) ;                                               00043700
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00043800
               FOR J~ JEND STEP 1 UNTIL NCOLA DO                        00043900
               A[I,J-NDEL] ~ A[I,J] ;                                   00044000
               NCOLA ~ NCOLA - NDEL ;                                   00044100
               JBEG~JBEG - 1;                                           00044200
               JEND~JEND - 1;                                           00044300
          END ;                                                         00044400
               IF IBEG > 0 THEN                                         00044500
          BEGIN                                                         00044600
               IBEG~IBEG+1; IEND~IEND+1;                                00044700
               NDEL ~ IEND - IBEG + 1 ;                                 00044800
               IF NDEL}NROWA OR NDEL<1 OR IEND-1>NROWA THEN             00044900
               ERROR(4) ;                                               00045000
               FOR I~ IEND STEP 1 UNTIL NROWA DO                        00045100
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00045200
               A[I-NDEL,J] ~ A[I,J] ;                                   00045300
               NROWA ~ NROWA - NDEL ;                                   00045400
               IBEG ~ IBEG - 1;                                         00045500
               IEND ~ IEND - 1;                                         00045600
          END ;                                                         00045700
          END  OF DELETE ;                                              00045800
PROCEDURE      AUGMENT ;                                                00045900
          BEGIN                                                         00046000
COMMENT        ADD ROWS OR COLUMNS TO MATRIX **** ;                     00046100
LIST           AUGR(FOR DX1~1 STEP 1 UNTIL NCOLA DO A[I,DX1]),          00046200
               AUGC(FOR DX1 ~ JBEG STEP 1 UNTIL JEND DO A[I,DX1]);      00046300
               IF IEND > 0  AND NCOLA = NCOLB  THEN                     00046400
          BEGIN                                                         00046500
               IBEG ~ NROWA + 1 ;                                       00046600
               IEND ~ NROWB + NROWA ;                                   00046700
               FOR I~ IBEG STEP 1 UNTIL IEND DO                         00046800
               IF INB > 1 THEN                                          00046900
               READ(FILESW[INB],OKTL,AUGR) [FINIS] ELSE                 00047000
               READ(CARD,SWFB,AUGR) [FINIS] ;                           00047100
               NROWA ~ IEND ;                                           00047200
          END  ELSE                                                     00047300
               IF JEND > 0  AND NROWA = NROWB  THEN                     00047400
          BEGIN                                                         00047500
               JBEG ~ NCOLA + 1 ;                                       00047600
               JEND ~ NCOLA + NCOLB ;                                   00047700
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00047800
               IF INB > 1 THEN                                          00047900
               READ(FILESW[INB],OKTL,AUGC)[FINIS]  ELSE                 00048000
               READ(CARD,SWFB,AUGC)[FINIS] ;                            00048100
               NCOLA ~ JEND ;                                           00048200
          END  ELSE ERROR(5) ;                                          00048300
          END  OF AUGMENT ;                                             00048400
PROCEDURE      VECTODIAG ;                                              00048500
          BEGIN                                                         00048600
COMMENT        MAKE DIAGONAL MATRIX FROM ROW OR COLUMN VECTOR *** ;     00048700
               IF NROWA = 1 THEN                                        00048800
          BEGIN                                                         00048900
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00049000
               B[J]~A[1,J]               ;                              00049100
               FOR I ~ 1 STEP 1 UNTIL NCOLA DO                          00049200
          BEGIN                                                         00049300
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO  A[I,J]~0;               00049400
               A[I,I]~ B[I];                                            00049500
          END;                                                          00049600
               NROWA ~ NCOLA ;                                          00049700
          END  ELSE                                                     00049800
               IF NCOLA = 1 THEN                                        00049900
          BEGIN                                                         00050000
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00050100
          BEGIN                                                         00050200
               B[I] ~ A[I,1] ;                                          00050300
               FOR J ~ 1 STEP 1 UNTIL NROWA DO                          00050400
               A[I,J] ~ 0 ;                                             00050500
               A[I,I] ~ B[I] ;                                          00050600
          END ;                                                         00050700
               NCOLA ~ NROWA ;                                          00050800
          END  ELSE ERROR(6)  ;                                         00050900
          END  OF VECTODIAG ;                                           00051000
PROCEDURE      DIAGTOVEC ;                                              00051100
          BEGIN                                                         00051200
COMMENT        ROW OR COLUMN VECTOR FROM DIAGONAL MATRIX **** ;         00051300
INTEGER        ROWORCOL ;                                               00051400
               ROWORCOL ~ MISS1 ;                                       00051500
               IF ROWORCOL = 1 THEN                                     00051600
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00051700
               A[1,J] ~ A[J,J] ELSE                                     00051800
               IF ROWORCOL = 0 THEN                                     00051900
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00052000
               A[I,1] ~ A[I,I] ELSE                                     00052100
               ERROR(6)  ;                                              00052200
               IF ROWORCOL = 1 THEN                                     00052300
               NROWA ~ 1   ELSE   NCOLA ~ 1 ;                           00052400
          END  OF DIAGTOVEC ;                                           00052500
PROCEDURE      TRITOSQ ;                                                00052600
          BEGIN                                                         00052700
COMMENT        CONVERT TRIANGULAR MATRIX TO SQUARE SYMMETRICAL FORM **; 00052800
LIST           READA1(FOR DX1 ~ I STEP 1 UNTIL NCOLA DO A[I,DX1]);      00052900
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00053000
          BEGIN                                                         00053100
               READ(CARD,SWFA,READA1)[FINIS] ;                          00053200
               FOR J ~ I STEP 1 UNTIL NCOLA DO                          00053300
               A[J,I] ~ A[I,J] ;                                        00053400
          END ;                                                         00053500
          END  OF TRITOSQ ;                                             00053600
PROCEDURE      COLNORM  ;                                               00053700
          BEGIN                                                         00053800
COMMENT        COLUMN NORMALIZATION OF A MATRIX ***** ;                 00053900
INTEGER        I ;                                                      00054000
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00054100
               B[J] ~ 0;                                                00054200
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00054300
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00054400
               B[J] ~ B[J] + A[I,J] * 2 ;                               00054500
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00054600
               B[J] ~ SQRT(B[J]) ;                                      00054700
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00054800
               FOR J~1 STEP 1 UNTIL NCOLA DO                            00054900
               A[I,J] ~ A[I,J] / B[J] ;                                 00055000
          END  OF COLNORM ;                                             00055100
PROCEDURE      SCALE ;                                                  00055200
          BEGIN                                                         00055300
COMMENT        SCALE A MATRIX BY MULTIPLYING ELEMENTS OF THE MATRIX BY  00055400
               1 / SQRT(OF THE DIAGONAL ELEMETTS)    ;                  00055500
               IF NROWA ! NCOLA THEN ERROR(11) ;                        00055600
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00055700
               B[I] ~ SQRT(A[I,I]) ;                                    00055800
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00055900
               FOR J ~ 1 STEP 1 UNTIL NROWA DO                          00056000
               A[I,J] ~ A[I,J] / (B[I] | B[J]) ;                        00056100
          END OF SCALE ;                                                00056200
PROCEDURE      MULTAB ;                                                 00056300
          BEGIN                                                         00056400
COMMENT        MATRIX MULTIPLICATION   A ~  A | B **** ;                00056500
LIST           LISTC(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO C[DX1]) ,        00056600
               LISTA(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO A[I,DX1]);       00056700
               IF NCOLA ! NROWB THEN  ERROR(7) ;                        00056800
               TRANSPOSE ;                                              00056900
                                                                        00057000
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00057100
               WRITE(TEM,OKTL,LISTA) ;                                  00057200
               REWIND(TEM) ;                                            00057300
               FOR I~1 STEP 1 UNTIL NCOLA DO                            00057400
               FOR J ~ 1 STEP 1 UNTIL NCOLB DO                          00057500
               A[I,J] ~ 0 ;                                             00057600
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00057700
          BEGIN                                                         00057800
               READ(TEM,OKTL,LISTC)[FINIS] ;                            00057900
               IF INB ! 1 THEN                                          00058000
               READ(FILESW[INB],OKTL,LISTB) [FINIS] ELSE                00058100
               READ(CARD,SWFB,LISTB) [FINIS] ;                          00058200
               FOR K ~ 1 STEP 1 UNTIL NCOLA DO                          00058300
               FOR J ~ 1 STEP 1 UNTIL NCOLB DO                          00058400
               A[K,J] ~ A[K,J] + C[K] | B[J] ;                          00058500
          END ;                                                         00058600
               NROWA ~ NCOLA ;                                          00058700
               NCOLA ~ NCOLB ;                                          00058800
               CLOSE(TEM,PURGE);                                        00058900
          END  OF MULT ;                                                00059000
PROCEDURE      MULTATRANSA ;                                            00059100
          BEGIN                                                         00059200
COMMENT        ***  AT|A : CAN BE USED FOR MINOR PRODUCTS WHERE THE     00059300
               NUMBER OF ROWS MAY EXCEED THE NUMBER OF ROWS DECLARED OR 00059400
               THE OVERALL SIZE OF THE MATRIX EXCEEDS THE MEMORY SPACE ;00059500
LIST           LISTA1(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO B[DX1]) ;       00059600
               FOR I ~ 1 STEP 1 UNTIL NCOLA DO                          00059700
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00059800
               A[I,J] ~ 0 ;                                             00059900
               IF INA = 0 THEN ERROR(1)  ;                              00060000
               FOR K ~ 1 STEP 1 UNTIL NROWA DO                          00060100
          BEGIN                                                         00060200
               IF INA = 1 THEN READ(CARD,SWFA,LISTA1) ELSE              00060300
               READ(FILESW[INA],OKTL,LISTA1) ;                          00060400
               FOR I ~ 1 STEP 1 UNTIL NCOLA DO                          00060500
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00060600
               A[I,J] ~ A[I,J] + B[I] | B[J] ;                          00060700
          END ;                                                         00060800
               NROWA ~ NCOLA ;                                          00060900
          END  OF MULTATRANSA ;                                         00061000
PROCEDURE      MULTBA ;                                                 00061100
          BEGIN                                                         00061200
INTEGER        L1,L2,P1,P2,JSEC,K ;                                     00061300
LIST           LISTT(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO C[DX1]),         00061400
               LISTL(FOR DX1~L1 STEP 1 UNTIL L2 DO C[DX1]),             00061500
               LISTP(K,JSEC,FOR DX1 ~ P1 STEP 1 UNTIL P2 DO C[DX1]),    00061600
               LPR(NPROG,INA,NROWA,NCOLA,INB,NROWB,NCOLB,CONST,IBEG,    00061700
                   IEND,JBEG,JEND,LIN,CRD,TPE),                         00061800
               LID  (FOR DX1 ~ 0 STEP 1 UNTIL 11 DO ID[DX1]) ;          00061900
FORMAT         TITLE(//X46,"MULTIPLICATION: A ~ B | A ;"//X5,12A6//),   00062000
               FMTL(X10,10(X1,R10.4)),                                  00062100
               FMTP (2I4,X2,5R14.6),                                    00062200
               FPR(/"PRO-        A - MATRIX        B - MATRIX       ",  00062300
                    "CONSTANT       DELETE ROW     DELETE COL     PRINT"00062400
                   ,"   PUNCH   STORE"/"GRAM      IN    R     C     IN",00062500
                    "   R     C",X21,"BEG    END     BEG    END"/       00062600
                    X1,I2,X8,I1,2(X3,I3),X5,I1,2(X3,I3),X4,R10.0,X6,I3, 00062700
                    X4,I3,X5,I3,X4,I3,3(X7,I1)/),                       00062800
               FID  (12A6) ;                                            00062900
FORMAT         FROW("ROW",I5),                                          00063000
               FNOMRE("PRODUCT IS NOT AVAILABLE FOR FURTHER CALCULATION"00063100
               ,"S");                                                   00063200
LABEL          STRTL, STRTP ;                                           00063300
               IF TPE { 1 OR TPE > 7 THEN                               00063400
               IF LIN>0 THEN WRITE(LINE,FNOMRE) ELSE ERROR(2);          00063500
                                                                        00063600
               IF INB = 0 THEN  ERROR(1) ;                              00063700
               IF NCOLB ! NROWA THEN ERROR(7) ;                         00063800
               IF LIN > 0 THEN WRITE(LINE,TITLE,LID) ;                  00063900
               IF LIN > 0 THEN WRITE(LINE,FPR,LPR);                     00064000
               IF CRD > 0 THEN WRITE(PUNCH,FID ,LID) ;                  00064100
               IF TPE>1 THEN                                            00064200
               WRITE(FILESW[TPE],OKTL,NROWB,NCOLA) ;                    00064300
               FOR K ~ 1 STEP 1 UNTIL NROWB DO                          00064400
          BEGIN                                                         00064500
               L1 ~ L2 ~ P1 ~ P2 ~ JSEC ~ 0 ;                           00064600
               IF INB = 1 THEN READ(CARD,SWFB,LISTB) ELSE               00064700
               READ(FILESW[INB],OKTL,LISTB) ;                           00064800
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00064900
          BEGIN                                                         00065000
               C[J] ~ 0 ;                                               00065100
               FOR I ~ 1 STEP 1 UNTIL NCOLB DO                          00065200
               C[J] ~ C[J] + B[I] | A[I,J] ;                            00065300
          END ;                                                         00065400
               IF TPE>1 THEN                                            00065500
               WRITE(FILESW[TPE],OKTL,LISTT) ;                          00065600
               IF LIN > 0 THEN                                          00065700
          BEGIN                                                         00065800
               WRITE(LINE[NO],FROW,K);                                  00065900
STRTL:         L1 ~ L2 + 1 ; L2 ~ L1 + 9 ; IF L2 > NCOLA THEN L2~NCOLA ;00066000
               WRITE(LINE,FMTL,LISTL) ;                                 00066100
               IF L2 < NCOLA THEN GO TO STRTL ;                         00066200
               WRITE(LINE);                                             00066300
          END ;                                                         00066400
               IF CRD > 0 THEN                                          00066500
          BEGIN                                                         00066600
STRTP:         P1~P2+1; P2~P1+4; IF P2 > NCOLA THEN P2~NCOLA;           00066700
               JSEC ~ JSEC+1; WRITE(PUNCH,FMTP,LISTP) ;                 00066800
               IF P2 < NCOLA THEN GO TO STRTP ;                         00066900
          END ;                                                         00067000
          END ;                                                         00067100
               WRITE(LINE[PAGE]);                                       00067200
               NROWA ~ NROWB ;                                          00067300
               IF TPE>1 THEN                                            00067400
               REWIND(FILESW[TPE]) ;                                    00067500
          END  OF MULTBA ;                                              00067600
PROCEDURE      MULTBTRANSA ;                                            00067700
          BEGIN                                                         00067800
COMMENT        MULTIPLY B TRANSPOSE | A: GIVEN B AND A, NEITHER OF      00067900
               WHICH ARE IN MEMORY ;                                    00068000
LIST           LISTB(FOR DX1 ~ 1 STEP 1 UNTIL NCOLB DO B[DX1]) ,        00068100
               LISTAC(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO C[DX1]);        00068200
               FOR I ~ 1 STEP 1 UNTIL NCOLB DO                          00068300
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00068400
               A[I,J] ~ 0 ;                                             00068500
               IF NROWA ! NROWB THEN ERROR(7) ;                         00068600
               IF INA = 0 OR INB = 0 OR INA = INB THEN ERROR(1) ;       00068700
               IF INB = 1 THEN                                          00068800
         BEGIN                                                          00068900
               REINITIAL (SWFB) ;                                       00069000
               OBJECTFMTGEN (SWFB) ;                                    00069100
         END ;                                                          00069200
               FOR K ~ 1 STEP 1 UNTIL NROWA DO                          00069300
          BEGIN                                                         00069400
               IF INA = 1 THEN READ(CARD,SWFA,LISTAC) [E10] ELSE        00069500
               READ(FILESW[INA],OKTL,LISTAC)[E10];IF INB = 1 THEN       00069600
               READ(CARD,SWFB    ,LISTB)[E10] ELSE READ(FILESW[INB],    00069700
                 OKTL,LISTB)[E10] ;                                     00069800
               FOR I ~ 1 STEP 1 UNTIL NCOLB DO                          00069900
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00070000
               A[I,J] ~ A[I,J] + B[I] | C[J] ;                          00070100
          END ;                                                         00070200
               NROWA~NCOLB;                                %VR 2/17/69  00070210
          END  OF MULTBTRANSA ;                                         00070300
PROCEDURE      SUMSANDSUMSQ ;                                           00070400
          BEGIN                                                         00070500
COMMENT        CALCULATE COLUMN SUMS AND SUMS OF SQUARES ;              00070600
FORMAT         SUMS(//X10,"COLUMN SUMS"//),                             00070700
               SUMSQ(//X10,"COLUMN SUMS OF SQUARES"//),                 00070800
               FSUM (10R12.5);                                          00070900
LIST           LSUM  (FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO C[DX1]) ,       00071000
               LSUMSQ(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO D[DX1]) ;       00071100
LIST           LISTA1(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO B[DX1]) ;       00071200
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00071300
               C[J] ~ D[J] ~ 0 ;                                        00071400
               IF INA = 0 THEN                                          00071500
          BEGIN                                                         00071600
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00071700
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00071800
          BEGIN                                                         00071900
               C[J] ~ C[J] + A[I,J] ;                                   00072000
               D[J] ~ D[J] + A[I,J] * 2 ;                               00072100
          END ;                                                         00072200
          END  ELSE                                                     00072300
          BEGIN                                                         00072400
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00072500
          BEGIN                                                         00072600
               IF INA = 1 THEN READ(CARD,SWFA,LISTA1) [E10] ELSE        00072700
               READ(FILESW[INA],OKTL,LISTA1)[E10] ;                     00072800
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00072900
          BEGIN                                                         00073000
               C[J] ~ C[J] + B[J] ;                                     00073100
               D[J] ~ D[J] + B[J] * 2 ;                                 00073200
          END ;                                                         00073300
          END ;                                                         00073400
          END ;                                                         00073500
               WRITE(LINE,SUMS) ;                                       00073600
               WRITE(LINE,FSUM,LSUM) ;                                  00073700
               WRITE(LINE,SUMSQ) ;                                      00073800
               WRITE(LINE,FSUM,LSUMSQ) ;                                00073900
               WRITE(LINE[DBL]) ;                                       00074000
          END  OF SUMSANDSUMSQ ;                                        00074100
PROCEDURE      MEANSANDSD ;                                             00074200
          BEGIN                                                         00074300
COMMENT        CALCULATE COLUMN MEANS,VARIANCES AND STANDARD DEVIATIONS;00074400
FORMAT         MEAN (//X10,"COLUMN MEANS"//) ,                          00074500
               STAND(//X10,"COLUMN STANDARD DEVIATIONS"//) ,            00074600
               VAR(//X10,"COLUMN VARIANCES"//),                         00074700
               FDAT (10R12.5) ;                                         00074800
LIST           LMEAN(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO C[DX1]),         00074900
               LVAR(FOR DX1~1 STEP 1 UNTIL NCOLA DO E[DX1]),            00075000
               LSD  (FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO D[DX1]);         00075100
               SUMSANDSUMSQ ;                                           00075200
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00075300
          BEGIN                                                         00075400
               C[J]~C[J]/NROWA;                                         00075500
               E[J]~D[J]/NROWA - C[J]*2;                                00075600
               D[J]~SQRT(E[J]);                                         00075700
          END ;                                                         00075800
               WRITE(LINE,MEAN) ;                                       00075900
               WRITE(LINE,FDAT,LMEAN) ;                                 00076000
               WRITE(LINE,STAND) ;                                      00076100
               WRITE(LINE,FDAT,LSD) ;                                   00076200
               WRITE(LINE,VAR);                                         00076300
               WRITE(LINE,FDAT,LVAR);                                   00076400
               WRITE(LINE[DBL]) ;                                       00076500
          END  OF MEANSANDSD ;                                          00076600
PROCEDURE      ZSCORE ;                                                 00076700
          BEGIN                                                         00076800
COMMENT        CALCULATE COLUMN STANDARD SCORES WITH OPTION TO NORMALIZE00076900
               BY 1/SQRT(NROWA);                                        00077000
LABEL          STRT, DUMMY ;                                            00077100
FORMAT         ZSCR (//X10,"COLUMN STANDARD SCORES"//) ,                00077200
               ZSCR1(  X10,"COLUMN STANDARD SCORES") ,                  00077300
               FDAT (/"SUBJECT",I6/(X10,10R10.3)/) ,                    00077400
               FDAT1(2I4,X2,10R7.3) ;                                   00077500
FORMAT         FPR(/"PRO-        A - MATRIX        B - MATRIX       ",  00077600
                    "CONSTANT       DELETE ROW     DELETE COL     PRINT"00077700
                   ,"   PUNCH   STORE"/"GRAM      IN    R     C     IN",00077800
                    "   R     C",X21,"BEG    END     BEG    END"/       00077900
                    X1,I2,X8,I1,2(X3,I3),X5,I1,2(X3,I3),X4,R10.0,X6,I3, 00078000
                    X4,I3,X5,I3,X4,I3,3(X7,I1)/);                       00078100
LIST           LPR(NPROG,INA,NROWA,NCOLA,INB,NROWB,NCOLB,CONST,IBEG,    00078200
                   IEND,JBEG,JEND,LIN,CRD,TPE);                         00078300
FORMAT         FINMEM(//"STANDARD SCORES ARE IN MEMORY AS A"//),        00078400
               FNOTIN(//"STANDARD SCORES ARE NOT IN MEMORY"//);         00078500
FORMAT         FSMAL(2I4,X2,10R7.2),                                    00078510
               FLMED(2I4,X2,10R7.2),                                    00078520
               FINT(2I4,24I3),                                          00078530
               FLRGE(2I4,X2,5R14.7),                                    00078540
               FID(12A6);                                               00078550
INTEGER        J1,J2,JSEC,L1;                                           00078600
LIST           LISTA2(I,     FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO B[DX1]), 00078700
               LISTA3(I,JSEC,FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO B[DX1]); 00078800
LIST           LID(FOR DX1~0 STEP 1 UNTIL J2 DO ID[DX1]);               00078810
LIST           LDATA(I,JSEC,FOR DX1~J1 STEP 1 UNTIL J2 DO B[DX1]);      00078820
REAL           NORM ;                                                   00078900
               IF MISS3>0 THEN NORM~SQRT(NROWA) ELSE NORM~1;            00079000
               IF INA = 1 THEN                                          00079100
               IF NROWA { 150 THEN                                      00079200
          BEGIN                                                         00079300
               READAMAT ;                                               00079400
               INA ~ 0 ;                                                00079500
               WRITE(LINE,FINMEM);                                      00079600
          END  ELSE                                                     00079700
          BEGIN                                                         00079800
               IF MISS1 { 1 OR MISS1 > 7 THEN ERROR(2) ;                00079900
               WRITE(FILESW[MISS1],OKTL,NROWA,NCOLA) ;                  00080000
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00080100
          BEGIN                                                         00080200
               READ(CARD,SWFA,LISTA1)[E10] ;                            00080300
               WRITE(FILESW[MISS1],OKTL,LISTA1) ;                       00080400
          END ;  REWIND(FILESW[MISS1]) ;   INA ~ MISS1 ;                00080500
               WRITE(LINE,FNOTIN);                                      00080600
          END ;                                                         00080700
              WRITE(LINE,FPR,LPR);                                      00080800
               IF MISS1>0 THEN READ(FILESW[INA],OKTL,NROWA,NCOLA);      00080810
               MEANSANDSD ;                                             00080900
               IF LIN>0 THEN WRITE(LINE,ZSCR); IF CRD>0 THEN            00081000
               WRITE(PUNCH,ZSCR1);                                      00081100
               IF INA ! 0 THEN                                          00081200
          BEGIN                                                         00081300
               REWIND(FILESW[INA]) ;                                    00081400
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00081500
          END  ELSE                                                     00081600
          BEGIN                                                         00081700
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00081800
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00081900
               A[I,J] ~  (A[I,J] - C[J])/(D[J] | NORM) ;                00082000
               IF LIN > 0 THEN                                          00082100
               MPRINT(NROWA,NCOLA,A,ID) ;                               00082200
               IF CRD > 0 AND CRD { 2 THEN                              00082300
               MPUNCH(NROWA,NCOLA,A,ID,CRD) ;                           00082400
               IF TPE > 1 AND TPE { 7 THEN                              00082500
               MTAPE(NROWA,NCOLA,A,FILESW[TPE]) ;                       00082600
               GO TO DUMMY ;                                            00082700
          END ;                                                         00082800
               IF TPE > 1 AND TPE { 7 AND TPE ! INA THEN                00082900
               WRITE(FILESW[TPE],OKTL,NROWA,NCOLA) ELSE ERROR(2) ;      00083000
               IF CRD>0 THEN                                            00083010
          BEGIN                                                         00083020
               WRITE(PUNCH,FID,LID);                                    00083030
               IF CRD=1 OR CRD=2 THEN L1~9 ELSE                         00083040
               IF CRD=4 THEN CRD~23 ELSE L1~4;                          00083050
          END;                                                          00083060
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00083100
          BEGIN                                                         00083200
               READ(FILESW[INA],OKTL,LISTA1) ;                          00083300
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00083400
               B[J] ~ (B[J] - C[J]) /(D[J] | NORM) ;                    00083500
               WRITE(FILESW[TPE],OKTL,LISTA1) ;                         00083600
               IF LIN > 0 THEN                                          00083700
               WRITE(LINE,FDAT,LISTA2) ;                                00083800
               IF CRD > 0 THEN                                          00083900
          BEGIN                                                         00084000
               J1 ~ J2 ~ JSEC ~ 0 ;                                     00084100
STRT:          J1 ~ J2 + 1 ;                                            00084200
               J2~J1+L1;                                                00084300
               IF J2 > NCOLA THEN J2 ~ NCOLA; JSEC ~ JSEC+1 ;           00084400
               IF CRD=1 THEN WRITE(PUNCH,FSMAL,LDATA)ELSE               00084410
               IF CRD=2 THEN WRITE(PUNCH,FLMED,LDATA) ELSE              00084420
               IF CRD=3 THEN WRITE(PUNCH,FLRGE,LDATA) ELSE              00084430
               WRITE(PUNCH,FINT,LDATA);                                 00084450
               WRITE(PUNCH,FDAT1,LISTA3) ;                              00084500
               IF J2 < NCOLA THEN GO TO STRT ;                          00084600
          END  END ;                                                    00084700
                IF SAVEA THEN CLOSE(FILESW[INA],RELEASE) ELSE           00084800
                CLOSE(FILESW[INA],PURGE) ; REWIND(FILESW[TPE]) ;        00084900
DUMMY:         WRITE(LINE[PAGE]);                                       00085000
           END OF ZSCORE;                                               00085100
PROCEDURE      INTERCOR ;                                               00085200
          BEGIN                                                         00085300
COMMENT        CORRELATIONAL ANALYSIS ;                                 00085400
INTEGER        CRD1, LIN1 ;                                             00085500
FORMAT         CORR(//X10,"CORRELATION MATRIX"//) ,                     00085600
               CORR1(X10,"CORRELATION  MATRIX") ;                       00085700
               CRD1 ~ CRD; LIN1 ~ LIN ; CRD ~ LIN ~ 0 ;                 00085800
               MISS4 ~ INA ;                                            00085900
               MISS2 ~ TPE ;   TPE ~ MISS1 ;                            00086000
               IF MISS1 < 2 OR MISS1 > 7 THEN ERROR(2) ;                00086100
               MISS3 ~ 1 ;                                              00086200
               ZSCORE ;                                                 00086300
               INA ~ TPE ;                                              00086400
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00086500
               MULTATRANSA ;                                            00086600
               CLOSE(FILESW[INA],PURGE) ;                               00086700
               CRD ~ CRD1 ; LIN ~ LIN1 ;                                00086800
               TPE ~ MISS2 ;  INA ~ MISS4 ;                             00086900
               IF LIN > 0 THEN                                          00087000
               WRITE(LINE,CORR) ;  IF CRD  > 0 THEN WRITE(PUNCH,CORR1); 00087100
          END  OF INTERCOR ;                                            00087200
PROCEDURE      EXTENSIONCOR ;                                           00087300
          BEGIN                                                         00087400
COMMENT        CORRELATION OF ONE SET OF VARIABLES WITH ANOTHER SET ;   00087500
FORMAT         EXCOR (//X10," EXTENSION CORRELATION MATRIX"//),         00087600
               EXCOR1(  X10," EXTENSION CORRELATION MATRIX");           00087700
FORMAT         FSTSCR("STANDARD SCORES FOR INPUT B"),                   00087800
               FSTSCA("STANDARD SCORES FOR INPUT A");                   00087900
INTEGER        LIN1, CRD1 ;                                             00088000
               IF CRD > 0 THEN WRITE(PUNCH,EXCOR1);                     00088100
               MISS3 ~ 1 ; CRD1 ~ CRD; LIN1~LIN; CRD ~ LIN ~ 0 ;        00088200
               IF MISS1<2 OR MISS1 >7 OR MISS2 < 2 OR MISS2 > 7         00088300
               OR MISS4 < 2 OR MISS4 > 7 OR MISS1=MISS2 OR              00088400
               MISS2=MISS4 OR MISS1=MISS4 THEN ERROR(1);                00088500
               MISS6 ~ TPE ;                                            00088600
               TPE~MISS2;                                               00088700
               ZSCORE ;                                                 00088800
               MPRINT(NROWA,NCOLA,A,ID);                                00088900
               WRITE(LINE,FSTSCA);                                      00089000
               IF INA > 1 THEN IF SAVEA THEN CLOSE(FILESW[INA],RELEASE) 00089100
               ELSE CLOSE(FILESW[INA],PURGE) ;                          00089200
               INA~INB;  NROWA~NROWB;  NCOLA~NCOLB;  TPE~MISS4;         00089300
               IF INA = 1 THEN                                          00089400
         BEGIN                                                          00089500
               REINITIAL (SWFA) ;                                       00089600
               OBJECTFMTGEN (SWFA) ;                                    00089700
         END ;                                                          00089800
               ZSCORE ;                                                 00089900
               MPRINT(NROWA,NCOLA,A,ID);                                00090000
               WRITE(LINE,FSTSCR);                                      00090100
               IF INA > 1 THEN IF SAVEB THEN CLOSE(FILESW[INA],RELEASE) 00090200
               ELSE CLOSE(FILESW[INA],PURGE) ;                          00090300
               INA~MISS2;  INB~MISS4;                                   00090400
               READ(FILESW[INA],OKTL,NROWA,NCOLA);                      00090500
               READ(FILESW[INB],OKTL,NROWB,NCOLB);                      00090600
               MULTBTRANSA ;                                            00090700
               NROWA~NCOLB;                                             00090800
               CLOSE(FILESW[INA],PURGE); CLOSE(FILESW[INB],PURGE) ;     00090900
               INA ~ 0; INB ~ 0 ; LIN ~ LIN1; CRD ~ CRD1 ; TPE ~ MISS6; 00091000
               IF LIN > 0 THEN WRITE(LINE,EXCOR) ;                      00091100
          END  OF EXTENSIONCOR ;                                        00091200
PROCEDURE      POWER ;                                                  00091300
          BEGIN                                                         00091400
COMMENT        RAISE ELEMENTS OF MATRIX TO A POWER  ***** ;             00091500
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00091600
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00091700
               A[I,J] ~ A[I,J] * MISS1 ;                                00091800
               CONST~MISS1;                                             00091900
          END  OF POWER ;                                               00092000
PROCEDURE     INVERT (N, A, EPS) ;                                      00092100
VALUE         N, EPS ;                                                  00092200
INTEGER       N ;                                                       00092300
REAL          EPS ;                                                     00092400
REAL ARRAY    A[0,0] ;                                                  00092500
         BEGIN                                                          00092600
INTEGER       I, J, K, II, N1, K2, L ;                                  00092700
REAL          BIG, TEMP, DIAG, Q ;                                      00092800
INTEGER ARRAY F[0:N] ;                                                  00092900
LABEL         I2, I3, I4, I5, I6, SK3 ;                                 00093000
COMMENT       THE MATRIX IS TRIANGULARIZED USING ROW PIVOTS.  IF THE    00093100
              LARGEST ELEMENT IN A NEWLY CALCULATED COLUMN IS, IN       00093200
              ABSOLUTE VALUE, LESS THAN EPS, AN EXIT IS MADE ;          00093300
               IF NROWA ! NCOLA THEN  ERROR(9) ;                        00093400
               DET ~ 1 ;                                                00093500
I2:           FOR I ~ 1 STEP 1 UNTIL N DO                               00093600
         BEGIN                                                          00093700
              II ~ I-1 ;                                                00093800
              FOR J ~ I STEP 1 UNTIL N DO                               00093900
         BEGIN                                                          00094000
              Q ~ 0 ;                                                   00094100
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[J,K] | A[K,I] + Q ;    00094200
              A[J,I] ~ A[J,I] - Q                                       00094300
         END ;                                                          00094400
              BIG ~ 0 ;  K2 ~ I ;                                       00094500
I3:           FOR K ~ I STEP 1 UNTIL N DO                               00094600
         BEGIN                                                          00094700
              IF ABS(A[K,I]) > BIG THEN                                 00094800
         BEGIN                                                          00094900
              BIG ~ ABS(A[K,I]) ;  K2 ~ K                               00095000
         END                                                            00095100
         END ;                                                          00095200
              IF BIG { EPS THEN   ERROR(8) ;                            00095300
              F[I] ~ K2 ;                                               00095400
              IF K2 ! I THEN                                            00095500
          BEGIN                                                         00095600
               DET ~ - DET ;                                            00095700
I4:           FOR K ~ 1 STEP 1 UNTIL N DO                               00095800
         BEGIN                                                          00095900
              TEMP ~ A[I,K] ;  A[I,K] ~ A[K2,K] ;  A[K2,K] ~ TEMP       00096000
          END                                                           00096100
         END ;                                                          00096200
              DIAG ~ A[I,I] ;  DET ~ DET | DIAG ;                       00096300
                                                                        00096400
COMMENT       A PIVOT WAS MADE.  A ROW IS NOW CALCULATED.  ;            00096500
                                                                        00096600
              FOR J ~ I+1 STEP 1 UNTIL N DO                             00096700
         BEGIN                                                          00096800
              Q ~ 0 ;                                                   00096900
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;    00097000
              A[I,J] ~ (A[I,J] - Q) / DIAG                              00097100
         END                                                            00097200
         END ;                                                          00097300
               IF NPROG = 16 THEN  GO TO LOUT ;                         00097400
                                                                        00097500
COMMENT       THE LOWER TRIANGULAR MATRIX IS INVERTED.  ;               00097600
                                                                        00097700
I5:           FOR I ~ 1 STEP 1 UNTIL N DO                               00097800
         BEGIN                                                          00097900
              II ~ I-1 ;  DIAG ~ A[I,I] ;                               00098000
              FOR J ~ 1 STEP 1 UNTIL I DO                               00098100
         BEGIN                                                          00098200
              IF I=J THEN A[I,J] ~ 1/DIAG ELSE                          00098300
         BEGIN                                                          00098400
              Q ~ 0 ;                                                   00098500
              FOR K ~ J STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;    00098600
              A[I,J] ~ -Q/DIAG                                          00098700
         END                                                            00098800
         END                                                            00098900
         END ;                                                          00099000
              N1 ~ N-1 ;                                                00099100
                                                                        00099200
COMMENT       THE UPPER TRIANGULAR MATRIX IS INVERTED.  ;               00099300
                                                                        00099400
              FOR I ~ N1 STEP -1 UNTIL 1 DO                             00099500
         BEGIN                                                          00099600
              II ~ I+1 ;                                                00099700
              FOR J ~ N STEP -1 UNTIL II DO                             00099800
         BEGIN                                                          00099900
              Q~0 ;  L ~ J-1 ;                                          00100000
              FOR K ~ II STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;    00100100
              A[I,J] ~ -A[I,J] - Q                                      00100200
         END                                                            00100300
         END ;                                                          00100400
                                                                        00100500
COMMENT       THE INVERTED TRIANGULAR MATRICES ARE MULTIPLIED  TOGETHER.00100600
              THEIR PRODUCT IS THE DESIRED INVERSE.  ;                  00100700
                                                                        00100800
              FOR I ~ 1 STEP 1 UNTIL N1 DO                              00100900
              FOR J ~ 1 STEP 1 UNTIL N DO                               00101000
         BEGIN                                                          00101100
              Q ~ 0 ;                                                   00101200
              IF I}J THEN                                               00101300
         BEGIN                                                          00101400
              FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;   00101500
              A[I,J] ~ A[I,J] + Q                                       00101600
         END                                                            00101700
              ELSE                                                      00101800
         BEGIN                                                          00101900
              FOR K ~ J STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;     00102000
              A[I,J] ~ Q                                                00102100
         END                                                            00102200
         END ;                                                          00102300
                                                                        00102400
COMMENT       THE COLUMNS OF THE PRODUCT ARE NOW PERMUTED IN SUCH A WAY 00102500
              AS TO COMPENSATE FOR THE ROW PIVOTS MADE IN THE COURSE OF 00102600
              TRIANGULARIZATION.  ;                                     00102700
                                                                        00102800
I6:           FOR J ~ N STEP -1 UNTIL 1 DO                              00102900
         BEGIN                                                          00103000
              K2 ~ F[J] ;                                               00103100
              IF F[J] = J THEN GO TO SK3 ;                              00103200
              FOR K ~ 1 STEP 1 UNTIL N DO                               00103300
         BEGIN                                                          00103400
              TEMP ~ A[K,K2] ;  A[K,K2] ~ A[K,J] ;  A[K,J] ~ TEMP       00103500
         END ;                                                          00103600
SK3:     END                                                            00103700
         END  OF INVERSE ;                                              00103800
PROCEDURE      EXTENSION ;                                              00103900
         BEGIN                                                          00104000
COMMENT        EXTENSION ANALYSIS: FE = RE | FC | (FCT | FC)*-1 ;       00104100
INTEGER        LIN1 , CRD1, TPE1 ;                                      00104200
               LIN1 ~ LIN ; CRD1 ~ CRD ; TPE1 ~ TPE ; CRD ~ LIN ~ 0 ;   00104300
               MISS1 ~ INB ;                                            00104400
               IBEG ~  NROWB ;  IEND ~ NCOLB ;                          00104500
               IF INA = 1 THEN                                          00104600
          BEGIN                                                         00104700
               READAMAT ;                                               00104800
               INA ~ 7 ;                                                00104900
               MTAPE(NROWA,NCOLA,A,FILESW[INA]) ;                       00105000
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00105100
          END ;                                                         00105200
               MISS5~INA;                                               00105300
               MULTATRANSA ;  REWIND(FILESW[INA]) ;                     00105400
               EPS ~ NROWA | .5 @ -10 ;                                 00105500
               INVERT(NROWA,A,EPS) ;                                    00105600
               TPE ~ 2 ;                                                00105700
               INB ~ INA ;                                              00105800
               READ(FILESW[INB],OKTL,NROWB,NCOLB) ;                     00105900
               MULTBA ;  REWIND(FILESW[INB]) ;                          00106000
               INA ~ TPE ;                                              00106100
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00106200
               READAMAT ;                                               00106300
               CLOSE(FILESW[INA],PURGE) ;                               00106400
               INB ~ MISS1 ;   NROWB ~ IBEG ;  NCOLB ~ IEND ;           00106500
               IF INB = 1 THEN                                          00106600
         BEGIN                                                          00106700
               REINITIAL (SWFB) ;                                       00106800
               OBJECTFMTGEN (SWFB) ;                                    00106900
         END ;                                                          00107000
               MULTBA ;                                                 00107100
               REWIND(FILESW[INB]) ;                                    00107200
               CONST ~ NROWA + NROWB ;                                  00107300
               IF CONST { 150 AND MISS3 > 0 THEN                        00107400
          BEGIN                                                         00107500
               INA~MISS5;                                               00107600
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00107700
               READAMAT ;  CLOSE(FILESW[INA],PURGE ) ;                  00107800
               INB ~ 2 ;                                                00107900
               READ(FILESW[INB],OKTL,NROWB,NCOLB) ;                     00108000
               IEND ~ 1 ;                                               00108100
               AUGMENT ;                                                00108200
                                                                        00108300
               CLOSE(TAPE2,PURGE) ;                                     00108400
               INA ~ 0 ;                                                00108500
          END ELSE                                                      00108600
          BEGIN                                                         00108700
               INA ~ 2 ;                                                00108800
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00108900
               READAMAT ;                                               00109000
               CLOSE(FILESW[INA],PURGE) ;                               00109100
               INA~MISS5;                                               00109200
                                                                        00109300
          END ;                                                         00109400
               LIN~LIN1;  CRD~CRD1;  INB~MISS1;  TPE~TPE1;              00109500
          END  OF EXTENSION ;                                           00109600
PROCEDURE      FACTSCRS1 ;                                              00109700
          BEGIN                                                         00109800
COMMENT        ***  FACTOR SCORES COMPUTED BY: F1 = Z|R*-1|A  ;         00109900
INTEGER        INZ,INF,NROWZ,NROWF,NCOLZ,NCOLF;                         00110000
LIST           LID(FOR DX1 ~ 0 STEP 1 UNTIL 11 DO ID[DX1]);             00110100
FORMAT         FSCR1(//X39,"FACTOR SCORES BY METHOD # 1: F1 = Z|R*-1|A"/00110200
                      /X5,12A6//) ;                                     00110300
               WRITE(LINE,FSCR1,LID) ;                                  00110400
               INZ ~ MISS1 ; NROWZ ~ IBEG ; NCOLZ ~ IEND ;              00110500
               EPS ~ NROWA | .5 @ -10;                                  00110600
               INVERT(NROWA,A,EPS) ;                                    00110700
               MULTAB ;                                                 00110800
               IF INA > 1 THEN IF SAVEA THEN CLOSE(FILESW[INA],RELEASE) 00110900
               ELSE CLOSE(FILESW[INA],PURGE) ; IF INB > 1 THEN IF SAVEB 00111000
               THEN CLOSE(FILESW[INB],RELEASE) ELSE CLOSE(FILESW[INB],  00111100
               PURGE) ; INB ~ INZ; NROWB ~ NROWZ; NCOLB ~ NCOLZ;        00111200
               IF INB = 1 THEN                                          00111300
          BEGIN                                                         00111400
               REINITIAL(SWFB) ;                                        00111500
               OBJECTFMTGEN(SWFB);                                      00111600
          END  ELSE READ(FILESW[INB],OKTL,NROWB,NCOLB) ;                00111700
               TPE ~ 3 ;                                                00111800
               MULTBA ;  INA ~ 0 ;                                      00111900
          END  OF FACTSCRS1 ;                                           00112000
PROCEDURE      FACTSCRS3 ;                                              00112100
          BEGIN                                                         00112200
COMMENT        ***  FACTOR SCORES BY:  F3 = Z|A|(AT|A)*-1 ;             00112300
LIST           LID(FOR DX1 ~ 0 STEP 1 UNTIL 11 DO ID[DX1]) ;            00112400
FORMAT         FSCR3(//X38,"FACTOR SCORES BY METHOD # 3: F3=Z|A|(AT|A)",00112500
                           "*-1"//X5,12A6//) ;                          00112600
INTEGER        INZ,INF,NROWZ,NROWF,NCOLZ,NCOLF,LIN1,CRD1;               00112700
               WRITE(LINE,FSCR3,LID) ;                                  00112800
               NCOLF~NCOLA;LIN1~LIN;CRD1~CRD;LIN~0;CRD~0;TPE~3;         00112900
               INZ~INB;NROWZ~NROWB;NCOLZ~NCOLB;INF~INA;NROWF~NROWA;     00113000
               IF INA = 1 THEN                                          00113100
          BEGIN                                                         00113200
               READAMAT ;                                               00113300
               INF ~ INA ~ 7 ;                                          00113400
               MTAPE(NROWA,NCOLA,A,FILESW[INA]) ;                       00113500
               READ(FILESW[INA],OKTL,NROWA,NCOLA);                      00113600
          END ;                                                         00113700
               MULTATRANSA;                                             00113800
               REWIND(FILESW[INA]) ;                                    00113900
               EPS ~ NROWA | .5 @ -10 ;                                 00114000
               INVERT(NROWA,A,EPS) ;                                    00114100
               INB ~ INF ;                                              00114200
               READ(FILESW[INB],OKTL,NROWB,NCOLB) ;                     00114300
               MULTBA ;                                                 00114400
               IF SAVEA THEN CLOSE(FILESW[INF],RELEASE) ELSE            00114500
               CLOSE(FILESW[INF],PURGE) ;                               00114600
               INA ~ 3 ;                                                00114700
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00114800
               READAMAT ;                                               00114900
               CLOSE(FILESW[INA],PURGE) ;                               00115000
               INB~INZ;NROWB~NROWZ;NCOLB~NCOLZ;LIN~LIN1;CRD~CRD1;       00115100
               IF INB = 1 THEN                                          00115200
         BEGIN                                                          00115300
               REINITIAL (SWFB) ;                                       00115400
               OBJECTFMTGEN (SWFB) ;                                    00115500
         END ;                                                          00115600
               MULTBA ;                                                 00115700
               INA ~ 0 ;                                                00115800
          END  OF FACTSCRS3 ;                                           00115900
PROCEDURE      FACTSCRS456 ;                                            00116000
          BEGIN                                                         00116100
COMMENT        *** FACTOR SCORES BY: F4=ZA. F5=ZB. F6=ZC ;              00116200
FORMAT         FSCR4(//X43,"FACTOR SCORES BY METHOD # ",I1," F=Z",A1//  00116300
                            X5,12A6);                                   00116400
ALPHA          NUM ;                                                    00116500
LIST           LID1 (MISS3,NUM,FOR DX1 ~ 0 STEP 1 UNTIL 11 DO ID[DX1]); 00116600
               IF MISS3=4 THEN NUM~"A" ELSE IF MISS3=5 THEN NUM~"B"     00116700
               ELSE NUM~"C" ;                                           00116800
               WRITE(LINE,FSCR4,LID1) ;                                 00116900
               IF MISS3 ! 4 THEN                                        00117000
               FOR I ~ 1 STEP 1 UNTIL NROWA DO                          00117100
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00117200
               IF A[I,J] { CONST THEN A[I,J] ~ 0 ELSE                   00117300
               IF MISS3 = 6 THEN A[I,J] ~ 1 ;                           00117400
               MULTBA ;                                                 00117500
          END  OF FACTSCRS456 ;                                         00117600
PROCEDURE      PROMAX ;                                                 00117700
          BEGIN                                                         00117800
COMMENT        ****  OBLIQUE ROTATION OF A FACTOR MATRIX  **** ;        00117900
INTEGER        K, CONST1 ;                                              00118000
FORMAT         PRO(//X25,"PROMAX OBLIQUE ROTATION OF A VARAMAX ROTATED",00118100
                         " FACTOR MATRIX: POWER = ",I3//);              00118200
FORMAT         F1(//X51,"TRANSFORMATION MATRIX"//),                     00118300
               F2(//X45,"REFERENCE VECTOR CORRELATIONS"//) ,            00118400
               F3(//X48,"FACTOR INTER CORRELATIONS"//) ,                00118500
               F4(//X47,"REFERENCE VECTOR STRUCTURE"///),               00118600
               P1(X8,"TRANSFORMATION MATRIX"),                          00118700
               P2(X10,"REFERENCE VECTORS CORRELATIONS") ,               00118800
               P3(X10,"FACTOR INTERCORRELATIONS") ,                     00118900
               P4(X10,"REFERENCE VECTOR STRUCTURE");                    00119000
LIST           LISTC(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO C[DX1]);         00119100
LIST           LISTA1(FOR DX1 ~ 1 STEP 1 UNTIL NCOLA DO B[DX1]) ;       00119200
               WRITE(LINE,PRO,CONST) ;                                  00119300
               IF INA = 1 THEN                                          00119400
          BEGIN                                                         00119500
               READAMAT ;  INA ~ 7;                                     00119600
               MTAPE(NROWA,NCOLA,A,FILESW[INA]) ;                       00119700
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00119800
          END ;                                                         00119900
               MISS1~INA;                                               00120000
               FOR I ~ 1 STEP 1 UNTIL NCOLA DO                          00120100
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00120200
               A[I,J] ~ 0 ;                                             00120300
               IF CONST = 0 THEN                                        00120400
          BEGIN                                                         00120500
               CONST1 ~ 1 ;                                             00120600
               IF NROWA!NROWB OR NCOLA!NCOLB THEN                       00120700
          BEGIN                                                         00120710
               WRITE(LINE,ERR12);                                       00120720
               GO TO FINIS;                                             00120730
          END;                                                          00120740
               REINITIAL (SWFB) ;                                       00120800
               OBJECTFMTGEN (SWFB) ;                                    00120900
           END  ELSE  CONST1 ~ CONST ;                                  00121000
               FOR K ~ 1 STEP 1 UNTIL NROWA DO                          00121100
          BEGIN                                                         00121200
               READ(FILESW[INA],OKTL,LISTA1);                           00121300
               IF INB = 1 THEN                                          00121400
               READ(CARD,SWFB,LISTC) [E10]  ELSE                        00121500
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00121600
               C[J] ~ B[J] ;                                            00121700
               FOR I ~ 1 STEP 1 UNTIL NCOLA DO                          00121800
               FOR J ~ 1 STEP 1 UNTIL NCOLA DO                          00121900
               A[I,J] ~ A[I,J] + B[I] | C[J] * CONST1 ;                 00122000
          END ;                                                         00122100
               REWIND(FILESW[INA]) ;                                    00122200
               NROWA ~ NCOLA ;                                          00122300
               MTAPE(NROWA,NCOLA,A,TAPE2) ;                             00122400
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00122500
               MULTATRANSA ;                                            00122600
               REWIND(FILESW[INA]) ;                                    00122700
               EPS ~ NROWA | .5 @ -10 ;                                 00122800
               INVERT(NROWA,A,EPS) ;                                    00122900
               INB ~ 2 ;                                                00123000
               READ(FILESW[INB],OKTL,NROWB,NCOLB)[E1] ;                 00123100
               MULTAB ;                                                 00123200
               CLOSE(FILESW[INB],PURGE) ;                               00123300
               COLNORM ;                                                00123400
               MTAPE(NROWA,NCOLA,A,TAPE2) ;                             00123500
               WRITE(LINE,F1) ;                                         00123600
               MPRINT(NROWA,NCOLA,A,ID) ;                               00123700
               IF MISS3 > 0 AND MISS3 { 2 THEN                          00123800
          BEGIN                                                         00123900
               WRITE(PUNCH,P1) ;                                        00124000
               MPUNCH(NROWA,NCOLA,A,ID,MISS3) ;                         00124100
          END ;                                                         00124200
               READ(TAPE2,OKTL,NROWA,NCOLA) ;                           00124300
               INA ~ 2 ;                                                00124400
               MULTATRANSA ;                                            00124500
               REWIND(TAPE2) ;                                          00124600
               WRITE(LINE,F2) ;                                         00124700
               MPRINT(NROWA,NCOLA,A,ID) ;                               00124800
               INA~MISS1;                                               00124900
               IF MISS4 > 0 AND MISS4 { 2 THEN                          00125000
          BEGIN                                                         00125100
               WRITE(PUNCH,P2) ;                                        00125200
               MPUNCH(NROWA,NCOLA,A,ID,MISS4) ;                         00125300
          END ;                                                         00125400
               INVERT(NROWA,A,EPS) ;                                    00125500
               SCALE ;                                                  00125600
               WRITE(LINE,F3) ;                                         00125700
               MPRINT(NROWA,NCOLA,A,ID) ;                               00125800
               IF MISS5 > 0 AND MISS5 { 2 THEN                          00125900
          BEGIN                                                         00126000
               WRITE(PUNCH,P3) ;                                        00126100
               MPUNCH(NROWA,NCOLA,A,ID,MISS5) ;                         00126200
          END ;                                                         00126300
               IF MISS6>0 THEN                                          00126400
               MTAPE(NROWA,NCOLA,A,CORMAT) ;                            00126500
               WRITE(LINE,F4) ;                                         00126600
               IF CRD > 0 THEN WRITE(PUNCH,P4) ;                        00126700
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00126800
               READAMAT ;                                               00126900
               READ(FILESW[INB],OKTL,NROWB,NCOLB)[E2] ;                 00127000
               MULTAB ;                                                 00127100
               CLOSE(FILESW[INB],PURGE) ; INB ~ 0 ;                     00127200
          END  OF PROMAX ;                                              00127300
PROCEDURE ROOTDIAG;                                                     00127400
          BEGIN                                                         00127500
COMMENT        ROW OR COLUMN VECTOR OF THE INVERSE OF THE SQUARE ROOT OF00127600
               DIAGONALS OF SQUARE MATRIX;                              00127700
INTEGER        ROWORCOL;                                                00127800
               ROWORCOL~MISS1;                                          00127900
               IF ROWORCOL=1 THEN                                       00128000
               FOR J~1 STEP 1 UNTIL NCOLA DO                            00128100
               A[1,J]~1/SQRT(A[J,J]) ELSE                               00128200
               IF ROWORCOL=0 THEN                                       00128300
               FOR I~1 STEP 1 UNTIL NROWA DO                            00128400
               A[I,1]~1/SQRT(A[I,I]) ELSE                               00128500
               ERROR(6);                                                00128600
               IF ROWORCOL=1 THEN NROWA~1 ELSE NCOLA~1;                 00128700
          END OF ROOTDIAG;                                              00128800
PROCEDURE      RECISQR;                                        %MC 2/69 00128805
          BEGIN                                                %MC 2/69 00128810
COMMENT        CASTS RECIPROCAL SQUARE ROOTS OF COLUMN SUMS OF          00128815
               SQUARES INTO A DIAGONAL MATRIX;                 %MC 2/69 00128820
               SUMSANDSUMSQ;                                   %MC 2/69 00128825
               FOR I~1 STEP 1 UNTIL NCOLA DO                   %MC 2/69 00128830
               D[I]~1/SQRT(D[I]);                              %MC 2/69 00128835
               FOR I~1 STEP 1 UNTIL NCOLA DO                   %MC 2/69 00128840
          BEGIN                                                %MC 2/69 00128845
               FOR J~1 STEP 1 UNTIL NCOLA DO                   %MC 2/69 00128850
               IF I=J THEN A[I,J]~D[I] ELSE                    %MC 2/69 00128855
               A[I,J]~0;                                       %MC 2/69 00128860
          END;                                                 %MC 2/69 00128865
               NROWA~NCOLA;                                    %MC 2/69 00128867
          END OF RECISQR;                                      %MC 2/69 00128870
COMMENT        THREE CARDS REMOVED TO OUTER BLOCK BY WHE 12-23-66;      00128900
               IF INA < 0 OR INA > 7 THEN                               00129000
               ERROR(1) ;                                               00129100
               IF INB < 0 OR INB > 7 THEN                               00129200
               ERROR(1) ;                                               00129300
               IF INA = 1 THEN                                          00129400
          BEGIN                                                         00129500
               REINITIAL (SWFA) ;                                       00129600
               OBJECTFMTGEN(SWFA) ;                                     00129700
          END  ELSE                                                     00129800
               IF INA > 1 THEN                                          00129900
               READ(FILESW[INA],OKTL,NROWA,NCOLA) ;                     00130000
               IF INA ! 0 THEN                                          00130100
               IF NPROG>2 AND NPROG<20 OR NPROG=31 THEN                 00130200
               READAMAT ;                                               00130300
               IF INB = 1 AND NPROG > 2 AND NPROG < 20 THEN             00130400
          BEGIN                                                         00130500
               REINITIAL (SWFB) ;                                       00130600
               OBJECTFMTGEN(SWFB) ;                                     00130700
          END  ELSE                                                     00130800
               IF INB > 1 THEN                                          00130900
               READ(FILESW[INB],OKTL,NROWB,NCOLB) ;                     00131000
               GO TO SWPROG[NPROG] ;                                    00131100
LOUT:          IF INA > 1 THEN                                          00131200
                    IF SAVEA THEN                                       00131300
               CLOSE(FILESW[INA],RELEASE)  ELSE                         00131400
               CLOSE(FILESW[INA],PURGE) ;                               00131500
               IF INB > 1  THEN                                         00131600
               IF SAVEB THEN                                            00131700
               CLOSE(FILESW[INB],RELEASE)  ELSE                         00131800
               CLOSE(FILESW[INB],PURGE) ;                               00131900
               IF NPROG } 15 AND NPROG { 16 THEN                        00132000
               WRITE(LINE,FDET,DET) ;                                   00132100
               IF NPROG<17 OR NPROG>23 THEN                             00132200
          BEGIN                                                         00132300
               IF CRD > 0 AND CRD < 4 THEN                              00132400
               MPUNCH (NROWA,NCOLA,A,ID,CRD) ;                          00132500
               IF TPE > 1 AND TPE { 7 THEN                              00132600
               MTAPE (NROWA,NCOLA,A,FILESW[TPE]) ELSE                   00132700
                     IF TPE ! 0 THEN ERROR(2) ;                         00132800
               IF LIN > 0 THEN                                          00132900
               MPRINT (NROWA,NCOLA,A,ID) ;                              00133000
          END ;                                                         00133100
               GO TO NEXTSTEP ;                                         00133200
L1:            READAMAT ;                                               00133300
               GO TO LOUT ;                                             00133400
L2:            IDSTORE ;                                                00133500
               GO TO LOUT ;                                             00133600
L3:            ADD ;                                                    00133700
               GO TO LOUT ;                                             00133800
L4:            SUB ;                                                    00133900
               GO TO LOUT ;                                             00134000
L5:            TRANSPOSE ;                                              00134100
               GO TO LOUT ;                                             00134200
L6:            CONSTTIMESA ;                                            00134300
               GO TO LOUT ;                                             00134400
L7:            DELETE ;                                                 00134500
               GO TO LOUT ;                                             00134600
L8:            AUGMENT ;                                                00134700
               GO TO LOUT ;                                             00134800
L9:            VECTODIAG ;                                              00134900
               GO TO LOUT ;                                             00135000
L10:           DIAGTOVEC ;                                              00135100
               GO TO LOUT ;                                             00135200
L11:           SCALE ;                                                  00135300
               GO TO LOUT ;                                             00135400
L12:           COLNORM ;                                                00135500
               GO TO LOUT ;                                             00135600
L13:           MULTAB ;                                                 00135700
               GO TO LOUT ;                                             00135800
L14:           POWER ;                                                  00135900
               GO TO LOUT ;                                             00136000
L15:           EPS ~  NROWA | .5 @ -10 ;                                00136100
               INVERT(NROWA,A,EPS) ;                                    00136200
               GO TO LOUT ;                                             00136300
L16:           EPS~NROWA|.5@-10;                                        00136400
               INVERT(NROWA,A,EPS);                                     00136500
               GO TO LOUT;                                              00136600
L17:           MULTBA ;                                                 00136700
               GO TO LOUT;                                              00136800
L18:           FACTSCRS1 ;                                              00136900
               GO TO LOUT ;                                             00137000
L19:           FACTSCRS456 ;                                            00137100
               GO TO LOUT ;                                             00137200
L20:           FACTSCRS3 ;                                              00137300
               GO TO LOUT ;                                             00137400
L21:           SUMSANDSUMSQ ;                                           00137500
               GO TO LOUT ;                                             00137600
L22:           MEANSANDSD ;                                             00137700
               GO TO LOUT ;                                             00137800
L23:           ZSCORE ;                                                 00137900
               GO TO LOUT ;                                             00138000
L24:           PROMAX ;                                                 00138100
               GO TO LOUT ;                                             00138200
L25:           MULTATRANSA ;                                            00138300
               GO TO LOUT ;                                             00138400
L26:           MULTBTRANSA ;                                            00138500
               GO TO LOUT ;                                             00138600
L27:           INTERCOR ;                                               00138700
               GO TO LOUT ;                                             00138800
L28:           EXTENSIONCOR ;                                           00138900
               GO TO LOUT ;                                             00139000
L29:           TRITOSQ ;                                                00139100
               GO TO LOUT ;                                             00139200
L30:           EXTENSION ;                                              00139300
               GO TO LOUT ;                                             00139400
L31:           ROOTDIAG;                                                00139500
               GO TO LOUT;                                              00139600
L32:           RECISQR;                                        %MC 2/69 00139605
               GO TO LOUT;                                     %MC 2/69 00139610
          END  INNER BLOCK FOR  ARRAY DEC;                              00139700
E1:            WRITE(LINE,ERR1) ;                                       00139800
               GO TO FINIS ;                                            00139900
E2:            WRITE(LINE,ERR2) ;                                       00140000
               GO TO FINIS ;                                            00140100
E3:            WRITE(LINE,ERR3) ;                                       00140200
               GO TO FINIS ;                                            00140300
E4:            WRITE(LINE,ERR4) ;                                       00140400
               GO TO FINIS ;                                            00140500
E5:            WRITE(LINE,ERR5) ;                                       00140600
               GO TO FINIS ;                                            00140700
E6:            WRITE(LINE,ERR6) ;                                       00140800
               GO TO FINIS ;                                            00140900
E7:            WRITE(LINE,ERR7) ;                                       00141000
               GO TO FINIS ;                                            00141100
E8:            WRITE(LINE,ERR8) ;                                       00141200
               GO TO FINIS ;                                            00141300
E9:            WRITE(LINE,ERR9) ;                                       00141400
               GO TO FINIS ;                                            00141500
E10:           WRITE(LINE,ERR10) ;                                      00141600
               GO TO FINIS ;                                            00141700
E11:           WRITE(LINE,ERR11) ;                                      00141800
FINIS:         DATELINE(0) ;                                            00141900
          END  OF MATROP .                                              00142000
                LAST CARD ON CRDIMG TAPE                                99999999
