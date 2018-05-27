          BEGIN                                                         00000000
COMMENT        COMFIX/STATMAN JAN 27,1969;                              00000100
FILE IN        CARD (2,10);                                             00000200
FILE OUT       PUNCH 0(2,10);                                           00000300
FILE OUT       LINE 18(2,15);                                           00000400
FILE IN        TAPE2 2 (2,100,SAVE 10),                                 00000500
               TAPE3 2 (2,100,SAVE 10),                                 00000600
               TAPE4 2 (2,100,SAVE 10),                                 00000700
               CORMAT 2 (2,100,SAVE 10),                                00000800
               FACMAT 2 (2,100,SAVE 10),                                00000900
               ROTMAT 2 (2,100,SAVE 10);                                00001000
SWITCH FILE    FILESW~LINE,CARD,TAPE2,TAPE3,TAPE4,CORMAT,FACMAT,ROTMAT; 00001100
INTEGER ARRAY  KR,KC,KP[0:60];                                          00001200
REAL ARRAY     COR,R[0:60,0:60];                                        00001300
ARRAY          HEAD[0:40];                                              00001400
INTEGER        I,J,NOPT,NORD,K,IR,IC,L,M,INP,OUTP,VECT;                 00001500
INTEGER        JSEC,JMAX,JMIN;                                 %MC 2/69 00001505
REAL           T;                                                       00001600
FORMAT         F1(I1,I2,3I1),                                           00001700
               F4(2I4,X2,7F10.5),                              %MC 2/69 00001800
               F5("ROW",I3,X4,11F10.4/(X10,11F10.4)),          %VR 1/69 00001900
               FMT1("***DEPENDENCY AT VARIABLE",I3," ***"),    %MC 1/69 00001905
               FMT2(//X50,"CORRELATION MATRIX"/X48,                     00001910
                    "WITH DESIRED DIAGONALS"//),               %MC 1/69 00001915
               OKTL(100O);                                              00002000
FORMAT         FC(40A2),                                                00002100
               FL("OPTIONAL DIAGONAL ENTRIES PROGRAM"/                  00002200
               "INPUT DIAGONAL OR CALCULATE MULTIPLE CORRELATION SQUARE"00002300
               /////X40,40A2);                                          00002400
FORMAT         F2(//////////////////////////////////////////////////////00002500
               //////////////////////////////////////////////////////); 00002600
FORMAT         F3(//////////////////////////////////////////////////////00002700
               ///////////////////////////////////////////////////////);00002800
FORMAT         FERROR("ERROR ON TAPE-NOT ENOUGH DATA");                 00002900
LIST           LST1(NOPT,NORD,INP,OUTP,VECT),                           00003000
               LSTR(FOR J~1 STEP 1 UNTIL NORD DO COR[I,J]),             00003100
               LST2(I,FOR J~1 STEP 1 UNTIL NORD DO COR[I,J]),           00003200
               LST9(FOR I~1 STEP 1 UNTIL 40 DO HEAD[I]),                00003300
               LSTP(I,JSEC,FOR J~JMIN STEP 1 UNTIL JMAX DO              00003310
                    COR[I,J]),                                 %MC 2/69 00003311
               LST3(FOR I~1 STEP 1 UNTIL NORD DO KP[I]);                00003400
LABEL          AGAIN,FINIS,L100,L101,L500,L19,L14,L13,L16,L21,EOT;      00003500
LABEL          LA;                                             %MC 2/69 00003501
SWITCH         SWGO1~L101,L100;                                         00003600
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00003700
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00003800
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00003900
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00004000
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00004100
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00004200
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00004300
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00004400
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00004500
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00004600
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00004700
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00004800
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00004900
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00005000
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00005100
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00005200
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00005300
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00005400
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300005500
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00005600
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00005700
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00005800
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00005900
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00006000
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00006100
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00006200
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00006300
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100006400
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00006500
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00006600
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00006700
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00006800
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00006900
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00007000
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00007100
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00007200
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00007300
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00007400
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00007500
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00007600
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00007700
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00007800
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00007900
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00008000
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00008100
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00008200
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00008300
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00008400
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00008500
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00008600
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00008700
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00008800
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00008900
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00009000
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00009100
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800009200
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00009300
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00009400
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00009500
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00009600
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00009700
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00009800
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00009900
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00010000
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00010100
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00010200
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00010300
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00010400
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00010500
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00010600
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00010700
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00010800
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00010900
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00011000
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00011100
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00011200
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00011300
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00011400
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00011500
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00011600
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00011700
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00011800
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00011900
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00012000
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00012100
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00012200
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00012300
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00012400
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00012500
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00012600
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00012700
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00012800
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00012900
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100013000
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00013100
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00013200
(136);EXIT:    FORMATPHRASE ~ ERRTOG;                                   00013300
           END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00013400
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00013500
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00013600
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000013700
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00013800
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00013900
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00014000
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00014100
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00014200
               READ(CARD,FC,LST9);                                      00014300
               WRITE(LINE,FL,LST9);                                     00014400
AGAIN:         READ(CARD,F1,LST1)[FINIS];       %PARAMETER CARD         00014500
               IF INP=1 THEN                                            00014600
               OBJECTFMTGEN(F2) ELSE              %VARIABLE FORMAT      00014700
               READ(FILESW[INP],OKTL,I,I)[EOT];   %FOR CORRELATION MATRX00014800
               IF INP=1 THEN                                            00014900
               FOR I~1 STEP 1 UNTIL NORD DO                             00015000
               READ(CARD,F2,LSTR)[FINIS] ELSE     %CORRELATION MATRIX   00015100
               FOR I~1 STEP 1 UNTIL NORD DO                             00015200
               READ(FILESW[INP],OKTL,LSTR)[EOT];                        00015300
               NOPT~NOPT+1;                                             00015400
               GO TO SWGO1[NOPT];           %OPTIONS 1 AND 2 LEGAL      00015500
L100:          IF VECT=1 THEN                                           00015600
               OBJECTFMTGEN(F3) ELSE                                    00015700
               READ(FILESW[VECT],OKTL,LST3)[EOT];                       00015800
               IF VECT=1 THEN                                           00015900
               READ(CARD,F3,LST3)[FINIS];         %ROW DATA             00016000
               FOR I~1 STEP 1 UNTIL NORD DO                             00016100
               COR[I,I]~KP[I];                                          00016200
               GO TO L500;                                              00016300
L101:          FOR I~1 STEP 1 UNTIL NORD DO                             00016400
               FOR J~1 STEP 1 UNTIL NORD DO                             00016500
               R[I,J]~COR[I,J];                                         00016600
COMMENT        BEGIN GAUSS-JORDAN INVERSION ROUTINE;                    00016700
               FOR I~1 STEP 1 UNTIL NORD DO                             00016800
               KP[I]~0;                                                 00016900
               FOR I~1 STEP 1 UNTIL NORD DO BEGIN                       00017000
               T~0;                                                     00017100
               FOR J~1 STEP 1 UNTIL NORD DO BEGIN                       00017200
               IF KP[J]=1 THEN GO TO L14;                               00017300
               FOR K~1 STEP 1 UNTIL NORD DO BEGIN                       00017400
               IF KP[K]=1 THEN GO TO L13;                               00017500
               IF T}ABS(R[J,K]) THEN GO TO L13;                         00017600
               IR~J;                                                    00017700
               IC~K;                                                    00017800
               T~ABS(R[J,K]);                                           00017900
L13:           END;                                                     00018000
L14:           END;                                                     00018100
               KP[IC]~KP[IC]+1;                                         00018200
               IF IR=IC THEN GO TO L16;                                 00018300
               FOR L~1 STEP 1 UNTIL NORD DO BEGIN                       00018400
               T~R[IR,L];                                               00018500
               R[IR,L]~R[IC,L];                                         00018600
               R[IC,L]~T;                                               00018700
               END;                                                     00018800
L16:           KR[I]~IR;                                                00018900
               KC[I]~IC;                                                00019000
               T~R[IC,IC];                                              00019100
               R[IC,IC]~1;                                              00019200
               FOR L~1 STEP 1 UNTIL NORD DO                             00019300
               R[IC,L]~R[IC,L]/T;                                       00019400
               FOR M~1 STEP 1 UNTIL NORD DO BEGIN                       00019500
               IF M=IC THEN GO TO L19;                                  00019600
               T~R[M,IC];                                               00019700
               R[M,IC]~0;                                               00019800
               FOR L~1 STEP 1 UNTIL NORD DO                             00019900
               R[M,L]~R[M,L]-R[IC,L]|T;                                 00020000
L19:           END;                                                     00020100
               END;                                                     00020200
               FOR I~1 STEP 1 UNTIL NORD DO BEGIN                       00020300
               J~NORD-I+1;                                              00020400
               IF KR[J]=KC[J] THEN GO TO L21;                           00020500
               K~KR[J];                                                 00020600
               L~KC[J];                                                 00020700
               FOR M~1 STEP 1 UNTIL NORD DO                             00020800
               T~R[M,K];                                                00020900
               R[M,K]~R[M,L];                                           00021000
               R[M,L]~T;                                                00021100
L21:           END;                                                     00021200
COMMENT        R NOW CONTAINS THE INVERSE;                              00021300
               FOR I~1 STEP 1 UNTIL NORD DO                             00021400
               IF R[I,I]=0 THEN                                         00021500
               WRITE(LINE,FMT1,I);                                      00021600
               FOR I~1 STEP 1 UNTIL NORD DO                             00021700
               COR[I,I]~1-(1/R[I,I]);                                   00021800
L500:          IF INP>1 AND INP=OUTP THEN REWIND(FILESW[INP]);          00021900
               IF OUTP=1 THEN                                  %MC 2/69 00021905
          BEGIN                                                %MC 2/69 00021910
               FOR I~1 STEP 1 UNTIL NORD DO                    %MC 2/69 00021915
          BEGIN                                                %MC 2/69 00021920
               JMAX~0;                                         %MC 2/69 00021925
LA:            JMIN~JMAX+1;                                    %MC 2/69 00021930
               IF NORD{JMAX~JMAX+7 THEN JMAX~NORD;             %MC 2/69 00021935
               JSEC~JSEC+1;                                    %MC 2/69 00021937
               WRITE(PUNCH,F4,LSTP);    % PUNCH CARD OUTPUT     MC 2/69 00021940
               IF JMAX!NORD THEN GO TO LA;                     %MC 2/69 00021945
          END;                                                 %MC 2/69 00021950
          END;                                                 %MC 2/69 00021955
               IF OUTP>1 THEN WRITE(FILESW[OUTP],OKTL,NORD,NORD);       00022000
               WRITE(LINE,FMT2);                                        00022100
                                                                        00022200
               FOR I~1 STEP 1 UNTIL NORD DO BEGIN                       00022300
               WRITE(LINE,F5,LST2);             %PRINTER OUTPUT         00022400
               WRITE(LINE);                                             00022500
                                                                        00022600
                                                                        00022700
               IF OUTP>1 THEN                                           00022800
               WRITE(FILESW[OUTP],OKTL,LSTR);   %TAPE OUTPUT            00022900
               END; GO TO FINIS;                                        00023000
EOT:           WRITE(LINE,FERROR);                                      00023100
FINIS:    END.                                                          00023200
                LAST CARD ON CRDIMG TAPE                                99999999
