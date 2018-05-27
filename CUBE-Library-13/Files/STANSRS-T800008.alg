          BEGIN                                                         00000000
COMMENT        STANSRS/STATMAN  FEB 10,1969;                            00000100
FILE           CARD 2 "CARD" (4, 10);                                   00000200
FILE OUT       LINE 4(4,15);                                            00000300
REAL           SCALE, FACTOR, LARGEST;                                  00000400
INTEGER        FIN, FOUT, NVPS, P;                                      00000500
BOOLEAN        CARDS,TAPE,PRINT,SAVET;                                  00000600
REAL ARRAY     PMS[0:3], TEMP[0:9];                                     00000700
FORMAT         PARAM(A5,L1,3(A5,X1),I5,X1,8A6);                         00000800
FORMAT         FRM(A6);                                                 00000900
FORMAT         UROWN(/////////////////////////////////////////////////  00001000
                     /////////////////////////////////////////////////  00001100
                     /////////////////////////////////////////////////);00001200
FORMAT         FTPE(100O);                                              00001300
FORMAT         OUDT(6(E18.11, X2));                                     00001400
FORMAT         NIMS(//"NO INPUT MEDIA SPECIFIED(I.E., CARDS, TAPE1, ",  00001500
                       "TAPE2,TAPE3,TAPE4,TAPE5)"//),                   00001600
               NIFS(//"NO INPUT FORMAT SPECIFICATI0NS"//),              00001700
               ITNS(//"MISSING OF IMPROPER OUTPUT TAPE NUMBER(MUST BE ",00001800
                       "TAPE1,2,3,4 OR 5)"//);                          00001900
FORMAT         SCFACT (// X42,"CARD OUTPUT SCALING FACTOR: ", E8.1 //); 00002000
FORMAT         NIVS (//"THE NUMBER OF VARIABLES IS UNSPECIFIED."//);    00002100
FORMAT         HEADING (X53,"STANDARD SCORES" // X34, "* ", 8A6, " *" //00002200
               X40, "INPUT MEDIA....................... ", A5  /        00002300
               X40, "PRINTER OUTPUT.................... ", A5  /        00002400
               X40, "MAGNETIC TAPE OUTPUT.............. ", A5  /        00002500
               X40, "PUNCHED CARD OUTPUT............... ", A5  /        00002600
               X40, "NUMBER OF VARIABLES PER SUBJECT... ", I5  /  );    00002700
FORMAT         SUBJS (/ X40, "NUMBER OF SUBJECTS................ ",I5/);00002800
LIST           HDLST (FOR P~0 STEP 1 UNTIL 7 DO TEMP[P], PMS[0],        00002900
                      IF PMS[1]="PRINT" THEN "YES  " ELSE "NONE ",      00003000
                      IF PMS[2]="     " THEN "NONE " ELSE PMS[2],       00003100
                      IF PMS[3]="CARDS" THEN "YES  " ELSE "NONE ",NVPS);00003200
LABEL          NOINPUT, IMPTPE, ERR, EXIT;                              00003300
LABEL          NVS;                                                     00003400
LABEL          RIP;                                                     00003500
LIST           L1(PMS[0],SAVET,FOR P~1 STEP 1 UNTIL 3 DO PMS[P],NVPS,   00003600
                   FOR P~0 STEP 1 UNTIL 7 DO TEMP[P]);                  00003700
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00003800
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00003900
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00004000
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00004100
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00004200
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00004300
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00004400
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00004500
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00004600
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00004700
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00004800
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00004900
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00005000
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00005100
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00005200
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00005300
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00005400
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00005500
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300005600
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00005700
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00005800
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00005900
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00006000
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00006100
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00006200
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00006300
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00006400
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100006500
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00006600
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00006700
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00006800
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00006900
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00007000
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00007100
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00007200
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00007300
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00007400
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00007500
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00007600
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00007700
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00007800
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00007900
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00008000
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00008100
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00008200
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00008300
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00008400
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00008500
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00008600
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00008700
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00008800
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00008900
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00009000
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00009100
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00009200
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800009300
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00009400
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00009500
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00009600
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00009700
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00009800
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00009900
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00010000
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00010100
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00010200
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00010300
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00010400
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00010500
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00010600
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00010700
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00010800
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00010900
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00011000
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00011100
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00011200
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00011300
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00011400
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00011500
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00011600
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00011700
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00011800
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00011900
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00012000
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00012100
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00012200
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00012300
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00012400
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00012500
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00012600
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00012700
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00012800
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00012900
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00013000
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100013100
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00013200
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00013300
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00013400
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00013500
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00013600
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000013700
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00013800
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00013900
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00014000
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00014100
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00014200
PROCEDURE      ZSCORES;                                                 00014300
          BEGIN                                                         00014400
FILE IN        TAP1 "TAPE1" (2,100),                                    00014500
               TAP2 "TAPE2" (2,100),                                    00014600
               TAP3 "TAPE3" (2,100),                                    00014700
               TAP4 "TAPE4" (2,100),                                    00014800
               TAP5 "CORMAT" (2,100);                                   00014900
FILE OUT       TP1 "TAPE1" (2,100,SAVE 10),                             00015000
               TP2 "TAPE2" (2,100,SAVE 10),                             00015100
               TP3 "TAPE3" (2,100,SAVE 10),                             00015200
               TP4 "TAPE4" (2,100,SAVE 10),                             00015300
               TP5 "CORMAT" (2,100,SAVE 10),                            00015400
               PNCH 0(4,10);                                            00015500
FILE           TMP 2 "TEMP" (2, NVPS);                                  00015600
SWITCH FILE    SIN ~ CARD,TAP1,TAP2,TAP3,TAP4,TAP5;                     00015700
SWITCH FILE    SOUT~ TP1,TP2,TP3,TP4,TP5;                               00015800
INTEGER        N,I,J,K,L,M;                                             00015900
REAL ARRAY     X[0:NVPS-1],                                             00016000
               SUMX[0:NVPS-1],                                          00016100
               SUMX2[0:NVPS-1];                                         00016200
REAL ARRAY     LARGX[0:NVPS-1],                                         00016300
               SMALLM[0:NVPS-1],                                        00016400
               SMALLS[0:NVPS-1];                                        00016500
INTEGER ARRAY  CDOUT[0:NVPS+10];                                        00016600
FORMAT         PFOR(10(I7), X2, 2(I4));                                 00016700
FORMAT         FZSCOR(//"ZSCORES FOR EACH VARIABLE"//),    %2/69 VR     00016710
               FSTD(//"STANDARD DEVIATIONS FOR EACH VARIABLE"//),%VR2/6900016711
               FMEAN(//"MEAN FOR EACH VARIABLE"//),        %VR 2/69     00016712
               FOUD(10(F10.4,X2));                         %VR 2/69     00016713
LIST           LTPE(FOR K~ 0 STEP 1 UNTIL NVPS-1 DO X[K]);              00016800
LIST           PLST(FOR K~ L STEP 1 UNTIL L+9 DO CDOUT[K], I, M~ M+1);  00016900
LIST           LISTIN (FOR M~0 STEP 1 UNTIL NVPS-1  DO  X[M]);          00017000
LABEL          EOF, BACK, START, EOF1;                                  00017100
LABEL          URGH, OK;                                                00017200
               N~ 0;                                                    00017300
               J ~NVPS-1;                                               00017400
               IF FIN>0 THEN                                            00017500
               READ(SIN[FIN],FTPE,I,NVPS);                              00017600
START:         IF FIN=0 THEN                                            00017700
               READ(CARD,UROWN,LISTIN)[EOF]                             00017800
               ELSE READ(SIN[FIN],FTPE,LISTIN)[EOF];                    00017900
               WRITE(TMP, FTPE, LTPE);                                  00018000
               N ~ N + 1;                                               00018100
               FOR I~ 0 STEP 1 UNTIL J DO                               00018200
          BEGIN                                                         00018300
               SUMX[I]~ SUMX[I]+X[I];                                   00018400
               SUMX2[I]~ SUMX2[I]+X[I]*2;                               00018500
               IF LARGX[I] < X[I]  THEN  LARGX[I] ~ X[I];               00018600
          END;                                                          00018700
               GO TO START;                                             00018800
EOF:           IF FIN>0 AND NOT SAVET THEN CLOSE(SIN[FIN],PURGE)        00018900
               ELSE CLOSE(SIN[FIN],RELEASE);                            00019000
               REWIND(TMP);                                             00019100
               WRITE(LINE, SUBJS, N);                                   00019200
               FOR I ~ 0  STEP 1  UNTIL J  DO  SMALLM[I]~SMALLS[I]~@40; 00019300
               FOR I~ 0 STEP 1 UNTIL J DO                               00019400
          BEGIN                                                         00019500
               SUMX[I] ~SUMX[I]/N;                                      00019600
               SUMX2[I]~ SQRT(SUMX2[I]-N|SUMX[I]*2)/N;                  00019700
               IF SMALLM[I] > SUMX[I]  THEN  SMALLM[I] ~ SUMX[I];       00019800
               IF SMALLS[I] > SUMX2[I]  THEN  SMALLS[I] ~ SUMX2[I];     00019900
          END;                                                          00020000
               IF CARDS  THEN                                           00020100
          BEGIN                                                         00020200
               LARGEST ~ 0;                                             00020300
               FOR I ~ 0  STEP 1  UNTIL J  DO                           00020400
          BEGIN     IF SMALLS[I] < @-10  THEN  GO TO URGH;              00020500
               IF M ~ ABS((LARGX[I]-SMALLM[I])/SMALLS[I]) > LARGEST THEN00020600
                  LARGEST ~ M;                                          00020700
URGH:     END;                                                          00020800
               SCALE ~ 1000000000.0;                                    00020900
               DO  IF LARGEST|SCALE < 1000000  THEN  GO TO OK           00021000
                    UNTIL  SCALE ~ SCALE/10.0 < @-9;                    00021100
OK:            FACTOR ~ 1.0/SCALE;                                      00021200
               WRITE(LINE, SCFACT, FACTOR);                             00021300
          END;                                                          00021400
               I ~ 1;                                                   00021500
               IF PRINT THEN WRITE(LINE,FZSCOR);           %VR 2/69     00021510
       DO BEGIN                                                         00021600
               READ(TMP, FTPE, LTPE)[EOF1];                             00021700
               FOR K~ 0 STEP 1 UNTIL J DO                               00021800
               IF SUMX2[K] < @-10  THEN  X[K] ~ 9.9999@33  ELSE         00021900
               X[K]~ (X[K]-SUMX[K])/SUMX2[K];                           00022000
               IF TAPE THEN                                             00022100
               WRITE(SOUT[FOUT], FTPE, LTPE);                           00022200
               IF PRINT THEN                                            00022300
          BEGIN                                                         00022400
               WRITE(LINE, OUDT, LISTIN);                               00022500
               WRITE(LINE);                                             00022600
          END;                                                          00022700
               IF CARDS THEN                                            00022800
          BEGIN                                                         00022900
               FOR K~ 0 STEP 1 UNTIL J DO                               00023000
               CDOUT[K] ~ ENTIER(X[K]|SCALE);                           00023100
               L~ M~ 0;                                                 00023200
BACK:          WRITE(PNCH,PFOR,PLST);                                   00023300
               L~ L+10;                                                 00023400
               IF L<NVPS THEN GO TO BACK;                               00023500
          END;                                                          00023600
               I ~ I + 1;                                               00023700
          END  UNTIL FALSE;                                             00023800
EOF1:          IF TAPE THEN CLOSE(SOUT[FOUT],RELEASE);                  00023900
               IF CARDS THEN CLOSE(PNCH, RELEASE);                      00024000
               CLOSE(TMP,PURGE);                                        00024100
               WRITE(LINE,FMEAN);                          %VR 2/69     00024110
               WRITE(LINE,FOUD,FOR K~0 STEP 1 UNTIL J DO SUMX[K]);% 2/6900024111
               WRITE(LINE,FSTD);                            %VR 2/69    00024112
               WRITE(LINE,FOUD,FOR K~0 STEP 1 UNTIL J DO SUMX2[K]);%2/6900024113
          END OF ZSCORES;                                               00024200
               READ(CARD, PARAM, L1)[ERR];                              00024300
               WRITE(LINE, HEADING, HDLST);                             00024400
               IF NVPS { 0  THEN  GO TO NVS;                            00024500
               IF PMS[0]="CARDS" THEN FIN~ 0 ELSE                       00024600
               IF PMS[0]="TAPE1" THEN FIN~ 1 ELSE                       00024700
               IF PMS[0]="TAPE2" THEN FIN~ 2 ELSE                       00024800
               IF PMS[0]="TAPE3" THEN FIN~ 3 ELSE                       00024900
               IF PMS[0]="TAPE4" THEN FIN ~4 ELSE                       00025000
               IF PMS[0]="TAPE5" THEN FIN~5 ELSE                        00025100
               GO TO NOINPUT;                                           00025200
               IF PMS[1]="PRINT" THEN PRINT~ TRUE                       00025300
               ELSE PRINT~ FALSE;                                       00025400
               IF PMS[2].[18:24]="TAPE" THEN                            00025500
          BEGIN                                                         00025600
               FOUT~ 9;                                                 00025700
               FOR P~1 STEP 1 UNTIL 5 DO                                00025800
               IF PMS[2].[45:3]=P THEN  FOUT~ P-1;                      00025900
               IF FOUT=9 THEN GO TO IMPTPE;                             00026000
               TAPE~ TRUE;                                              00026100
          END ELSE TAPE~ FALSE;                                         00026200
               IF PMS[3]="CARDS" THEN CARDS~ TRUE ELSE                  00026300
               CARDS~ FALSE;                                            00026400
               IF FIN=0 THEN BEGIN                                      00026500
               READ(CARD[NO], FRM, PMS[0])[ERR];                        00026600
               IF PMS[0] = "FORMAT"  THEN                               00026700
               OBJECTFMTGEN(UROWN) ELSE                                 00026800
               GO TO ERR;                                               00026900
               WRITE(LINE);                                             00027000
               END ELSE                                                 00027100
               CLOSE(CARD,RELEASE);                                     00027200
               WRITE(LINE);                                             00027300
               ZSCORES;                                                 00027400
               GO TO EXIT;                                              00027500
NOINPUT:       WRITE(LINE, NIMS);                                       00027600
               GO TO EXIT;                                              00027700
ERR:           WRITE(LINE, NIFS);                                       00027800
               GO TO EXIT;                                              00027900
IMPTPE:        WRITE(LINE, ITNS);                                       00028000
               GO TO EXIT;                                              00028100
NVS:           WRITE(LINE, NIVS);                                       00028200
EXIT:                                                                   00028300
          END.                                                          00028400
                LAST CARD ON CRDIMG TAPE                                99999999
