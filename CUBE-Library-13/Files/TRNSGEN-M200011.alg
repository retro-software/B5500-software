          BEGIN                                                         00000000
COMMENT        TRNSGEN/STATMAN;                                         00000100
FILE           CARD 2 "CARD" (4,10);                                    00000200
FILE OUT       LINE 4(4,15);                                            00000300
REAL           NAME,ARG;                                                00000400
INTEGER        FIN, FOUT, NVPS, P, II, TT, KK, TRNGS;                   00000500
INTEGER        ORIG, NEWVARS, KHI, L, N, M, R;                          00000600
BOOLEAN        CARDS,TAPE,PRINT,SAVET;                                  00000700
BOOLEAN        PARITY, ERRTOG;                                          00000800
REAL ARRAY     PMS[0:3], TEMP[0:9];                                     00000900
REAL ARRAY     TRNG[0:99, 0:1];                                         00001000
REAL ARRAY     ERRA[0:14];                                              00001100
DEFINE         I= TRNG[P,0].[36:12]#,                                   00001200
               K= TRNG[P,0].[24:12]#,                                   00001300
               T= TRNG[P,0].[12:12]#,                                   00001400
               C= TRNG[P,1]#,                                           00001500
               J= TRNG[P,1]#;                                           00001600
FORMAT         UROWN(/////////////////////////////////////////////////  00001700
                     /////////////////////////////////////////////////  00001800
                     /////////////////////////////////////////////////);00001900
FORMAT         FTPE (100O);                                             00002000
FORMAT         PARAM(A5,L1,3(A5,X1),I5,X1,8A6);                         00002100
LIST           L1(PMS[0],SAVET,FOR P~1 STEP 1 UNTIL 3 DO PMS[P],NVPS,   00002200
                   FOR P~ 0 STEP 1 UNTIL 7 DO TEMP[P]);                 00002300
FORMAT         FRM(A6);                                                 00002400
FORMAT         TFMT(A6, I3, I2, I3, R6.0);                              00002500
LIST           TLIST(NAME, KK, TT, II, C);                              00002600
FORMAT         HEADING (X52,"TRANSGENERATIONS" // X34, "* ", 8A6, " *"//00002700
               X40, "INPUT MEDIA....................... ", A5  /        00002800
               X40,"INPUT MEDIA SAVED ................ ",A5/            00002900
               X40, "PRINTER OUTPUT.................... ", A5  /        00003000
               X40, "MAGNETIC TAPE OUTPUT.............. ", A5  /        00003100
               X40, "PUNCHED CARD OUTPUT............... ", A5  /        00003200
               X40, "NUMBER OF VARIABLES PER SUBJECT... ", I5  /  );    00003300
LIST           HDLST (FOR P~ 0 STEP 1 UNTIL 7 DO TEMP[P], PMS[0],       00003400
               IF SAVET THEN "YES  " ELSE "NO   ",                      00003500
                      IF PMS[1]="PRINT" THEN "YES  " ELSE "NONE ",      00003600
                      IF PMS[2]="     " THEN "NONE " ELSE PMS[2],       00003700
                      IF PMS[3]="CARDS" THEN "YES  " ELSE "NONE ",NVPS);00003800
FORMAT         SUBJS(/ X40, "NUMBER OF SUBJECTS................ ",I5/); 00003900
FORMAT    NFMT (X36,"ORIGINAL NUMBER OF VARIABLES PER SUBJECT ....",I3 /00004000
                X36,"NUMBER OF NEW VARIABLES TO BE GENERATED .....",I3 /00004100
                X36,"RESULTANT VARIABLES PER SUBJECT .............",I3);00004200
LIST      NLST (ORIG, NEWVARS, NVPS);                                   00004300
FORMAT         ERF (13A6, A2);                                          00004400
LIST           ERL (FOR TRNGS~0 STEP 1 UNTIL 13 DO ERRA[TRNGS]);        00004500
FORMAT         NTRNG(//"NO INPUT DATA"//);                              00004600
FORMAT         TERR(//"INVALID NUMERIC FIELD (R6.0) ON FOLLOWING CARD", 00004700
                       ":" //);                                         00004800
FORMAT         NIVS (//"THE NUMBER OF VARIABLES IS UNSPECIFIED."//);    00004900
FORMAT         NIMS(//"NO INPUT MEDIA SPECIFIED(I.E., CARDS, TAPE1, ",  00005000
                       "TAPE2,TAPE3,TAPE4,TAPE5)"//),                   00005100
               NIFS(//"NO INPUT FORMAT SPECIFICATIONS"//),              00005200
               ITNS(//"MISSING OF IMPROPER OUTPUT TAPE NUMBER(MUST BE ",00005300
                       "TAPE1,2,3,4 OR 5)"//);                          00005400
FORMAT         SZEFMT ("THE I- AND/OR J-SUBSCRIPT VALUE ON TRANSGENERA",00005500
                       "TION CARD #",I2," EXCEEDS THE LARGEST OF THE ", 00005600
                       "K-SUBSCRIPT VALUES.");                          00005700
LIST           SZELST (P+1);                                            00005800
FORMAT         DMS(/"DATA IS OUTSIDE THE RANGE SPECIFIED FOR TRANSGENE",00005900
                   "RATION CODE #",I2,". I=",I3,", C OR J=",R6.0,", K=",00006000
                I3, ", TRNGEN CARD #",I2/);                             00006100
LIST           DMSLST (T, I+1, J, K+1, P+1);                            00006200
LABEL          NOINPUT, IMPTPE, ERR, EXIT;                              00006300
LABEL               NVS;                                                00006400
LABEL          ERR2, EOCDS;                                             00006500
LABEL          NEXT;                                                    00006600
LABEL          RESUME, NAXT;                                            00006700
LABEL          SIZERR;                                                  00006800
REAL PROCEDURE ARCSIN(X);                                               00006900
REAL           X;                                                       00007000
          BEGIN                                                         00007100
               IF X=1 THEN ARCSIN ~ 1.507963 ELSE                       00007200
               IF X=-1 THEN ARCSIN ~ -1.507963 ELSE                     00007300
               ARCSIN ~ ARCTAN(X/(SQRT(1-X*2)))                         00007400
          END OF ARCSIN;                                                00007500
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00007600
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00007700
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00007800
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00007900
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00008000
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00008100
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00008200
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00008300
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00008400
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00008500
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00008600
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00008700
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00008800
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00008900
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00009000
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00009100
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00009200
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00009300
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300009400
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00009500
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00009600
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00009700
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00009800
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00009900
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00010000
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00010100
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00010200
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100010300
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00010400
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00010500
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00010600
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00010700
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00010800
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00010900
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00011000
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00011100
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00011200
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00011300
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00011400
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00011500
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00011600
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00011700
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00011800
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00011900
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00012000
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00012100
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00012200
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00012300
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00012400
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00012500
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00012600
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00012700
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00012800
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00012900
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00013000
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800013100
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00013200
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00013300
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00013400
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00013500
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00013600
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00013700
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00013800
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00013900
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00014000
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00014100
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00014200
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00014300
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00014400
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00014500
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00014600
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00014700
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00014800
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00014900
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00015000
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00015100
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00015200
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00015300
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00015400
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00015500
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00015600
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00015700
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00015800
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00015900
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00016000
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00016100
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00016200
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00016300
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00016400
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00016500
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00016600
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00016700
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00016800
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100016900
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00017000
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00017100
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00017200
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00017300
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00017400
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000017500
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00017600
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00017700
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00017800
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00017900
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00018000
PROCEDURE      COPY;                                                    00018100
          BEGIN                                                         00018200
FILE IN        TAP1 "TAPE1" (2, 100),                                   00018300
               TAP2 "TAPE2" (2, 100),                                   00018400
               TAP3 "TAPE3" (2,100),                                    00018500
               TAP4 "TAPE4" (2,100),                                    00018600
               TAP5 "CORMAT" (2,100);                                   00018700
FILE           TMP 2 "TEMP" (2, 100, SAVE 1);                           00018800
SWITCH FILE    SIN~CARD,TAP1,TAP2,TAP3,TAP4,TAP5;                       00018900
     REAL ARRAY X[0:NVPS-1];                                            00019000
     LIST      LTPE (FOR P~0 STEP 1 UNTIL NVPS-1 DO X[P]);              00019100
     LIST      LTPE1 (FOR P~0 STEP 1 UNTIL ORIG-1 DO X[P]);             00019200
     LABEL     EOF;                                                     00019300
               N~ 0;                                                    00019400
               IF FIN>0  THEN  READ(SIN[FIN], FTPE, L, M);              00019500
       DO BEGIN                                                         00019600
               IF FIN>0  THEN  READ(SIN[FIN], FTPE, LTPE1)[EOF]  ELSE   00019700
                 READ(CARD, UROWN, LTPE1)[EOF];                         00019800
               N~ N + 1;                                                00019900
               WRITE(TMP, FTPE, LTPE);                                  00020000
          END UNTIL FALSE;                                              00020100
EOF:           WRITE(LINE, SUBJS, N);                                   00020200
               IF FIN>0 AND NOT SAVET THEN CLOSE(SIN[FIN],PURGE)        00020300
               ELSE CLOSE(SIN[FIN],RELEASE);                            00020400
               REWIND(TMP);                                             00020500
               WRITE(LINE);                                             00020600
          END OF COPY;                                                  00020700
PROCEDURE      TRANSGENERATIONS;                                        00020800
          BEGIN                                                         00020900
FILE           TMP 2 "TEMP" (2, 100);                                   00021000
FILE OUT       TP1 "TAPE1" (2, 100, SAVE 10),                           00021100
               TP2 "TAPE2" (2, 100, SAVE 10),                           00021200
               TP3 "TAPE3" (2,100,SAVE 10),                             00021300
               TP4 "TAPE4" (2,100,SAVE 10),                             00021400
               TP5 "CORMAT" (2,100,SAVE 10);                            00021500
FILE OUT       PNCH 0(4,10);                                            00021600
SWITCH FILE    SOUT~TP1,TP2,TP3,TP4,TP5;                                00021700
REAL ARRAY     X[0:NVPS+10];                                            00021800
FORMAT         OUDT(6(E18.11, X2));                                     00021900
LIST           LTPE(FOR P ~0 STEP 1 UNTIL NVPS-1 DO X[P]);              00022000
FORMAT         PFOR (5E14.7, X2, 2I4);                                  00022100
LIST           PLST (FOR P~L STEP 1 UNTIL L+4 DO X[P], R, M~M+1);       00022200
LABEL          EOF, ERR1, START, EOF1;                                  00022300
LABEL          FINI,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15, 00022400
               L16,L17,L18,L19;                                         00022500
SWITCH         SWG ~L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15, 00022600
               L16,L17,L18,L19;                                         00022700
LABEL          EXIT;                                                    00022800
               IF TAPE  THEN  WRITE(SOUT[FOUT], FTPE, N, NVPS);         00022900
               R ~ 1;                                                   00023000
       DO BEGIN                                                         00023100
START:         READ(TMP, FTPE, LTPE)[EOF1];                             00023200
               FOR P~ 0 STEP 1 UNTIL TRNGS-1 DO                         00023300
          BEGIN                                                         00023400
               GO TO SWG[T];                                            00023500
L1:            IF X[I]}0 THEN                                           00023600
               X[K]~  SQRT(X[I]) ELSE                                   00023700
               GO TO ERR1;                                              00023800
               GO TO FINI;                                              00023900
L2:            IF X[I]}0 THEN                                           00024000
               X[K]~  SQRT(X[I]) + SQRT(X[I]+1) ELSE                    00024100
               GO TO ERR1;                                              00024200
               GO TO FINI;                                              00024300
L3:            IF X[I]>0 THEN                                           00024400
               X[K]~  LN(X[I])|0.434294481903 ELSE                      00024500
               GO TO ERR1;                                              00024600
               GO TO FINI;                                              00024700
L4:            X[K]~  EXP(X[I]);                                        00024800
               GO TO FINI;                                              00024900
L5:            IF ARG~ X[I] }0 AND ARG {1 THEN                          00025000
               X[K]~  ARCSIN(SQRT(X[I])) ELSE                           00025100
               GO TO ERR1;                                              00025200
               GO TO FINI;                                              00025300
L6:            IF ARG~ X[I]/N }0 AND ARG {1 THEN                        00025400
               X[K]~  ARCSIN(SQRT(X[I]/(N+1)))+ARCSIN(SQRT((X[I]+1)/    00025500
               (N+1)))                                                  00025600
               ELSE GO TO ERR1;                                         00025700
               GO TO FINI;                                              00025800
L7:            IF X[I]!0 THEN                                           00025900
               X[K]~  1/X[I] ELSE                                       00026000
               GO TO ERR1;                                              00026100
               GO TO FINI;                                              00026200
L8:            X[K]~  X[I]+C;                                           00026300
               GO TO FINI;                                              00026400
L9:            X[K]~  X[I]|C;                                           00026500
               GO TO FINI;                                              00026600
L10:           IF X[I]}0 THEN                                           00026700
               X[K]~  X[I]*C ELSE                                       00026800
               GO TO ERR1;                                              00026900
               GO TO FINI;                                              00027000
L11:           X[K]~ X[I]+X[J-1];                                       00027100
               GO TO FINI;                                              00027200
L12:           X[K]~ X[I]-X[J-1];                                       00027300
               GO TO FINI;                                              00027400
L13:           X[K]~ X[I]|X[J-1];                                       00027500
               GO TO FINI;                                              00027600
L14:           IF X[J-1]!0 THEN                                         00027700
               X[K]~ X[I]/X[J-1] ELSE                                   00027800
               GO TO ERR1;                                              00027900
               GO TO FINI;                                              00028000
L15:           IF X[I]}C THEN                                           00028100
               X[K]~  1 ELSE                                            00028200
               X[K]~  0;                                                00028300
               GO TO FINI;                                              00028400
L16:           IF X[I]}X[J-1] THEN                                      00028500
               X[K]~  1 ELSE                                            00028600
               X[K]~  0;                                                00028700
               GO TO FINI;                                              00028800
L17:           X[K]~  -X[I];                                            00028900
               GO TO FINI;                                              00029000
L18:           X[K]~  X[I]*3;                                           00029100
               GO TO FINI;                                              00029200
L19:           X[K]~  X[I]*5;                                           00029300
FINI:     END;                                                          00029400
               IF TAPE THEN                                             00029500
               WRITE(SOUT[FOUT], FTPE, LTPE);                           00029600
               IF PRINT THEN                                            00029700
          BEGIN                                                         00029800
               WRITE(LINE, OUDT, LTPE);                                 00029900
               WRITE(LINE);                                             00030000
          END;                                                          00030100
               IF CARDS THEN                                            00030200
          BEGIN                                                         00030300
               M~ L~ 0;                                                 00030400
               DO WRITE(PNCH, PFOR, PLST) UNTIL L~L+5  } NVPS;          00030500
          END;                                                          00030600
               R ~ R + 1;                                               00030700
          END UNTIL FALSE;                                              00030800
EOF1:          IF TAPE THEN CLOSE(SOUT[FOUT],RELEASE);                  00030900
               IF CARDS THEN CLOSE(PNCH, RELEASE);                      00031000
               CLOSE(TMP, PURGE);                                       00031100
               GO TO EXIT;                                              00031200
ERR1:          WRITE(LINE, DMS, DMSLST);                                00031300
               GO TO FINI;                                              00031400
EXIT:          END OF TRANSGENERATIONS;                                 00031500
               READ(CARD, PARAM, L1)[ERR];                              00031600
               WRITE(LINE, HEADING, HDLST);                             00031700
               IF NVPS{0  THEN GO TO NVS;                               00031800
               IF PMS[0]="CARDS" THEN FIN~ 0 ELSE                       00031900
               IF PMS[0]="TAPE1" THEN FIN~ 1 ELSE                       00032000
               IF PMS[0]="TAPE2" THEN FIN~ 2 ELSE                       00032100
               IF PMS[0]="TAPE3" THEN FIN~ 3 ELSE                       00032200
               IF PMS[0]="TAPE4" THEN FIN~4 ELSE                        00032300
               IF PMS[0]="TAPE5" THEN FIN~5 ELSE                        00032400
               GO TO NOINPUT;                                           00032500
               IF PMS[1]="PRINT" THEN PRINT~ TRUE                       00032600
               ELSE PRINT~ FALSE;                                       00032700
               IF PMS[2].[18:24]="TAPE" THEN                            00032800
          BEGIN                                                         00032900
               FOUT~ 9;                                                 00033000
               FOR P~1 STEP 1 UNTIL 5 DO                                00033100
               IF PMS[2].[45:3]=P THEN FOUT~ P-1;                       00033200
               IF FOUT=9 THEN GO TO IMPTPE;                             00033300
               TAPE~ TRUE;                                              00033400
          END ELSE TAPE~ FALSE;                                         00033500
               IF PMS[3]="CARDS" THEN CARDS~ TRUE ELSE                  00033600
               CARDS~ FALSE;                                            00033700
               P~ 0;                                                    00033800
               KHI~ -99;                                                00033900
RESUME:DO BEGIN                                                         00034000
               PARITY ~ FALSE;                                          00034100
               READ(CARD[NO],FRM,NAME)[ERR2];                  %MC 3/69 00034200
NEXT:          IF NAME ! "TRNGEN" THEN GO TO EOCDS;                     00034300
               READ(CARD,TFMT,TLIST)[ERR2:NAXT];               %MC 3/69 00034305
             TRNG[P,0]~0&TT[12:36:12]&(KK-1)[24:36:12]&(II-1)[36:36:12];00034400
               IF KK>KHI THEN KHI~ KK;                                  00034500
               P~ P+1;                                                  00034600
          END UNTIL FALSE;                                              00034700
NAXT:          PARITY ~ TRUE;                                           00034800
               IF NAME = "TRNGEN"  THEN                                 00034900
          BEGIN                                                         00035000
               WRITE(LINE, TERR);                                       00035100
               READ(CARD, ERF, ERL);                                    00035200
               WRITE(LINE, ERF, ERL);                                   00035300
               ERRTOG ~ TRUE;                                           00035400
               GO TO RESUME;                                            00035500
          END;                                                          00035600
EOCDS:         IF ERRTOG THEN GO TO EXIT;                               00035700
               TRNGS~ P;                                                00035800
                                                                        00035900
               ORIG~ NVPS;                                              00036000
               IF KHI>NVPS THEN                                         00036100
          BEGIN                                                         00036200
               FOR P~ 0 STEP 1 UNTIL TRNGS-1 DO                         00036300
          BEGIN                                                         00036400
               IF I>KHI-1 THEN GO TO SIZERR;                            00036500
               IF (T}11 AND T{14 OR T=16) AND J>KHI THEN GO TO SIZERR;  00036600
          END;                                                          00036700
               NVPS~ NVPS + (NEWVARS~ KHI-NVPS);                        00036800
               WRITE(LINE, NFMT, NLST);                                 00036900
          END;                                                          00037000
               WRITE(LINE);                                             00037100
               IF FIN=0 THEN BEGIN                                      00037200
               READ(CARD[NO], FRM, PMS[0])[ERR];                        00037300
               IF PMS[0]="FORMAT" THEN                                  00037400
               OBJECTFMTGEN(UROWN) ELSE                                 00037500
               GO TO ERR;                                               00037600
               WRITE(LINE);                                             00037700
               END ELSE                                                 00037800
               CLOSE(CARD,RELEASE);                                     00037900
               WRITE(LINE);                                             00038000
               COPY;                                                    00038100
               TRANSGENERATIONS;                                        00038200
               GO TO EXIT;                                              00038300
NOINPUT:       WRITE(LINE, NIMS);                                       00038400
               GO TO EXIT;                                              00038500
ERR:           WRITE(LINE, NIFS);                                       00038600
               GO TO EXIT;                                              00038700
IMPTPE:        WRITE(LINE, ITNS);                                       00038800
               GO TO EXIT;                                              00038900
NVS:           WRITE(LINE, NIVS);                                       00039000
               GO TO EXIT;                                              00039100
ERR2:          IF FIN = 0 THEN                                          00039200
          BEGIN                                                         00039300
               WRITE(LINE, NTRNG);                                      00039400
               GO TO EXIT;                                              00039500
          END;                                                          00039600
               GO TO EOCDS;                                             00039700
SIZERR:        WRITE(LINE, SZEFMT, SZELST);                             00039800
               GO TO EXIT;                                              00039900
EXIT:                                                                   00040000
          END.                                                          00040100
                LAST CARD ON CRDIMG TAPE                                99999999
