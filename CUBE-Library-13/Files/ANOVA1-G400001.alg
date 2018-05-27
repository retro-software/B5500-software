BEGIN                                                                   00000000
COMMENT        ANOVA1/STATMAN;                                          00000100
FILE OUT       LINE 4 (2, 15);                                          00000200
FILE IN        CARD (2,10);                                             00000300
FORMAT         UROWN (////////////////////////////////////////////////  00000400
                      ////////////////////////////////////////////////  00000500
                      ////////////////////////////////////////////////  00000600
                      ////////////////////////////////////////////////);00000700
FILE OUT PUNCH 0(2,10);                                                 00000800
FILE IN        TAPE2 2 (2, 10);                                         00000900
DEFINE         CARDS=CARD#,                                             00001000
               TAP2=TAPE2#;                                             00001100
REAL ARRAY     IMG[0:9];                                                00001200
LABEL          EOCDS;                                                   00001300
BOOLEAN ARRAY  SENSL[0:4];                                              00001400
REAL  XPR;                                                              00001500
INTEGER  QQ;                                                            00001600
LABEL FINIS;                                                            00001700
PROCEDURE ERROR(N); VALUE N; INTEGER N; BEGIN                           00001800
  FORMAT F(///////"STOP / PAUSE  NO. ", I5);                            00001900
  WRITE (LINE,  F, N);  GO TO FINIS END;                                00002000
REAL PROCEDURE INT(ARG1); VALUE ARG1; REAL ARG1;                        00002100
    BEGIN INT~(SIGN(ARG1)|ENTIER(ABS(ARG1))) END;                       00002200
REAL PROCEDURE MAX(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00002300
  BEGIN MAX~(IF ARG1}ARG2 THEN ARG1 ELSE ARG2) END;                     00002400
REAL PROCEDURE MIN(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00002500
  BEGIN MIN~(IF ARG1{ARG2 THEN ARG1 ELSE ARG2) END;                     00002600
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00002700
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00002800
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00002900
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00003000
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00003100
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00003200
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00003300
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00003400
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00003500
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00003600
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00003700
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00003800
M PROCEDURE SETUP(TAP2,LINE,LCR);BEGIN LOCAL SET1;SI~TAP2;DI~LINE;DS~10W00003900
DS;40(DS~2LIT"  ");SI~TAP2;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~TA00004000
P2;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00004100
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00004200
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00004300
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00004400
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300004500
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00004600
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00004700
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00004800
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00004900
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00005000
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00005100
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00005200
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00005300
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100005400
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00005500
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00005600
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00005700
CEDURE READACARD;BEGIN READ(TAP2,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00005800
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00005900
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00006000
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00006100
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00006200
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00006300
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00006400
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00006500
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00006600
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00006700
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00006800
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00006900
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00007000
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00007100
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00007200
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00007300
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00007400
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00007500
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00007600
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00007700
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00007800
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00007900
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00008000
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00008100
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800008200
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00008300
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00008400
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00008500
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00008600
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00008700
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00008800
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00008900
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00009000
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00009100
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00009200
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00009300
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00009400
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00009500
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00009600
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00009700
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00009800
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00009900
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00010000
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00010100
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00010200
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00010300
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00010400
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00010500
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00010600
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00010700
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00010800
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00010900
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00011000
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00011100
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00011200
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00011300
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00011400
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00011500
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00011600
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00011700
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00011800
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00011900
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100012000
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00012100
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00012200
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00012300
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00012400
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00012500
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000012600
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00012700
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00012800
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00012900
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00013000
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00013100
COMMENT **************************  MAINPRO  ****************;          00013200
INTEGER ARRAY   LOCALARRAY000[0:100,0:11],  LOCALARRAY001[0:100],       00013300
        LOCALARRAY002[0:11],  LOCALARRAY003[0:100],                     00013400
               LOCALARRAY004[0:100];                                    00013500
REAL ARRAY   LOCALARRAY006[0:100],  LOCALARRAY007[0:9],                 00013600
               LOCALARRAY005[0:6],                                      00013700
               LOCALARRAY008[0:100],  LOCALARRAY009[0:14];              00013800
REAL ARRAY     COMMON[0:9, 0:511];                                      00013900
REAL ARRAY     IMAGE[0:9];                                              00014000
PROCEDURE MAINPRO;                                                      00014100
BEGIN                                                                   00014200
  DEFINE  SUMSQ  =COMMON#;                                              00014300
  DEFINE    INCQ  =  LOCALARRAY000#;                                    00014400
  DEFINE    LBAQ  =  LOCALARRAY001#;                                    00014500
  DEFINE    NLQ  =  LOCALARRAY002#;                                     00014600
  DEFINE    LSQQ  =  LOCALARRAY003#;                                    00014700
  DEFINE    NDFQ  =  LOCALARRAY004#;                                    00014800
  DEFINE    NAMEQ  =  LOCALARRAY005#;                                   00014900
  DEFINE    CWDQ  =  LOCALARRAY006#;                                    00015000
  DEFINE    FMTQ  =  LOCALARRAY007#;                                    00015100
  DEFINE    DATAQ  =  LOCALARRAY008#;                                   00015200
  DEFINE    CARDQ  =  LOCALARRAY009#;                                   00015300
DEFINE         CW = CWDQ[M]#,                                           00015400
               B0 = [38:1]#,  B1 = [39:1]#,  B2 = [40:1]#,              00015500
               B3 = [41:1]#,  B4 = [42:1]#,  B5 = [43:1]#,              00015600
               B6 = [44:1]#,  B7 = [45:1]#,  B8 = [46:1]#,              00015700
               B9 = [47:1]#,                                            00015800
               PCWD = CW.B0, CW.B1, CW.B2, CW.B3, CW.B4, CW.B5, CW.B6,  00015900
                      CW.B7, CW.B8, CW.B9#;                             00016000
INTEGER  DX1,DX2;                                                       00016100
INTEGER  INDEX1;                                                        00016200
OWN INTEGER NFQ, NDVQ, NCWQ, M, LCMAXQ, L, I2Q, I, J, LBASSQ,           00016300
       I1Q, NVPSQ, LCQ, K, N, N1Q, LDVQ;                                00016400
OWN REAL T;                                                             00016500
BOOLEAN        CONT, COMPCONT;                                          00016600
FORMAT         FL1 (12I4, 5A6, A2),                                     00016700
       FL4(80I1),                                                       00016800
               FL47 (//"ANALYSIS OF VARIANCE", X10, 5A6, A2 //),        00016900
       FL33(//"THIS DESIGN IS TOO LARGE FOR THE MEMORY"),               00017000
               FL17(8O),                                                00017100
               FL18 (I8),                                               00017200
               FL37 (13A6, A2),                                         00017300
       FL60(//"OUTPUT FOR DEPENDENT VARIABLE",I10),                     00017400
               FL66(//"      MEANS FOR CODE WORD      ", 10I1 //),      00017500
       FL68(10R12.3),                                                   00017600
       FL70(//"      CONTROL WORD      REDUCED SUM OF SQ      DEG O",   00017700
         "F FREEDOM      MEAN SQUARE"//),                               00017800
               FL74(X8,10I1,R23.3,I20,R17.6),                           00017900
               FL76 (//"      FIRST DATA CARD      ", 13A6, A2),        00018000
               FL78 (//"      LAST DATA CARD       ", 13A6, A2);        00018100
LIST LIST1(NFQ,NDVQ,NCWQ,FOR DX1 ~ 2 STEP 1 UNTIL 10 DO NLQ[DX1],       00018200
               FOR DX1~1 STEP 1 UNTIL 6  DO NAMEQ[DX1]),                00018300
     LIST2(FOR DX1 ~ 1 STEP 1 UNTIL NCWQ DO [FOR DX2 ~ 2 STEP 1 UNTIL   00018400
       10 DO INCQ[DX1,DX2],INCQ[DX1,1]]),                               00018500
               LIST3 (FOR DX1~1 STEP 1 UNTIL 6 DO NAMEQ[DX1]),          00018600
     LIST4(FOR DX1 ~ 1 STEP 1 UNTIL NCWQ DO CWDQ[DX1]),                 00018700
               LIST5 (NVPSQ),                                           00018800
               LIST6 (FOR INDEX1~1 STEP 1 UNTIL 14 DO CARDQ[INDEX1]),   00018900
     LIST7(LDVQ),                                                       00019000
               LIST8(PCWD),                                             00019100
     LIST9(FOR DX1 ~ I STEP NDVQ UNTIL J DO                             00019200
                    SUMSQ[DX1.[36:3],DX1.[39: 9]]),                     00019300
               LIST10 (PCWD, SUMSQ[I.[36:3],I.[39: 9]], NDFQ[M], T);    00019400
LIST           LIST11 (FOR DX1~1 STEP 1 UNTIL NVPSQ DO DATAQ[DX1]);     00019500
BEGIN                                                                   00019600
LABEL L2,L23,L11,L9,L19,L27,L20,L26,L29,L28,L31,L46;                    00019700
LABEL          L36, L34, L52, L53, L50, L64, L62;                       00019800
LABEL          DMP;                                                     00019900
STREAM PROCEDURE    INITIAL (CONT, COMPCONT);                           00020000
          BEGIN                                                         00020100
               DI ~ CONT;                                               00020200
               DS ~ 47 RESET;                                           00020300
               DS ~ SET;                                                00020400
               DI ~ COMPCONT;                                           00020500
               DS ~ 38 RESET;                                           00020600
               DS ~ 9 SET;                                              00020700
               DS ~ RESET;                                              00020800
          END  OF INITIAL;                                              00020900
STREAM PROCEDURE    REDUCE (IMAGE, CWD);                                00021000
          BEGIN                                                         00021100
               SI ~ IMAGE;                                              00021200
               DI ~ CWD;                                                00021300
               8(DS ~ 38 RESET; 10(SKIP 5 SB;                           00021400
                 IF SB  THEN  DS ~ SET  ELSE  DS ~ RESET; SKIP SB););   00021500
          END  OF REDUCE;                                               00021600
               INITIAL(CONT, COMPCONT);                                 00021700
          READ(TAPE2,FL1,LIST1)[FINIS];                                 00021800
     COMMENT   READ FIRST CONTROL CARD;                                 00021900
          READ(TAPE2,FL4,LIST2)[FINIS];                                 00022000
     COMMENT   READ CONTROL WORD CARDS;                                 00022100
          NLQ[1] ~ 1;                                                   00022200
          IF NDVQ{1 THEN GO TO L2;                                      00022300
          NLQ[NFQ+2] ~ NDVQ;                                            00022400
          FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                     00022500
            INCQ[M,NFQ+2] ~ 1  END;                                     00022600
          NFQ ~ NFQ+1;                                                  00022700
L2:       NFQ ~ NFQ+1;                                                  00022800
          LCMAXQ ~ 1;                                                   00022900
          FOR L ~ 1, L+1 STEP 1 UNTIL NFQ DO BEGIN                      00023000
            LCMAXQ ~ LCMAXQ|NLQ[L]  END;                                00023100
          FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                     00023200
            I2Q ~ 1;                                                    00023300
            FOR L ~ 1, L+1 STEP 1 UNTIL NFQ DO BEGIN                    00023400
              I2Q ~ I2Q|NLQ[L]*INCQ[M,L]  END;                          00023500
            NDFQ[M] ~ LCMAXQ DIV I2Q;                                   00023600
            IF INCQ[M,1]>0 THEN GO TO L9;                               00023700
            FOR L ~ 2, L+1 STEP 1 UNTIL NFQ DO BEGIN                    00023800
              IF INCQ[M,L]{0 THEN GO TO L11;                            00023900
              INCQ[M,L] ~ 0  END;                                       00024000
            IF NDVQ>1 THEN GO TO L23;                                   00024100
            LSQQ[M] ~ NFQ;                                              00024200
            GO TO L19;                                                  00024300
L23:        LSQQ[M] ~ NFQ-1;                                            00024400
            INCQ[M,NFQ] ~ 1;                                            00024500
            GO TO L19;                                                  00024600
L11:        LSQQ[M] ~ L-1;                                              00024700
            GO TO L19;                                                  00024800
L9:         LSQQ[M] ~ 1;                                                00024900
            INCQ[M,1] ~ 0;                                              00025000
L19:        FOR L ~ 1, L+1 STEP 1 UNTIL NFQ DO BEGIN                    00025100
              I ~ 1-INCQ[M,L];                                          00025200
              FOR J ~ L, J+1 STEP 1 UNTIL NFQ DO BEGIN                  00025300
                I ~ I|NLQ[J]*INCQ[M,J]  END;                            00025400
              INCQ[M,L] ~ 1-I  END;                                     00025500
            END;                                                        00025600
          LBAQ[1] ~ 1;                                                  00025700
          FOR M ~ 2, M+1 STEP 1 UNTIL NCWQ DO BEGIN                     00025800
            LBAQ[M] ~ LBAQ[M-1]+1-INCQ[M-1,1]  END;                     00025900
          LBASSQ ~ LBAQ[NCWQ]-INCQ[NCWQ,1]+1;                           00026000
          I1Q ~ LBASSQ+NCWQ|NDVQ;                                       00026100
          IF 16318}I1Q THEN GO TO L27;                                  00026200
          WRITE(LINE,FL47,LIST3);                                       00026300
          WRITE(LINE,FL33);                                             00026400
               GO TO FINIS;                                             00026500
L27:      FOR J ~ 1, J+1 STEP 1 UNTIL I1Q DO BEGIN                      00026600
                 SUMSQ[J.[36:3],J.[39: 9]] ~ 0;  END;                   00026700
          I ~ (NCWQ-1) DIV 8+1;                                         00026800
          FOR J ~ 1, J+1 STEP 1 UNTIL I DO BEGIN                        00026900
            SPACE(TAPE2,-1)  END;                                       00027000
               I ~ NCWQ DIV 8;                                          00027100
               IF NCWQ MOD 8 ! 0  THEN  I ~ I + 1;                      00027200
               FOR J ~ 1  STEP 1  UNTIL I  DO                           00027300
          BEGIN                                                         00027400
               READ(TAPE2, 10, IMAGE[*])[FINIS];                        00027500
               REDUCE(IMAGE, CWDQ[(J|8)-7]);                            00027600
          END;                                                          00027700
     COMMENT   THIS REREADS THE CONTROL WORD CARDS (ALTERNATE FORMAT);  00027800
               READ(TAPE2[NO], FL18, LIST5)[FINIS];                     00027900
               OBJECTFMTGEN(UROWN);                                     00028000
          READ(TAPE2,FL37,LIST6)[FINIS];                                00028100
     COMMENT   READS FIRST DATA CARD;                                   00028200
          SPACE(TAPE2,-1);                                              00028300
          LCQ ~ 1;                                                      00028400
L20:           READ(TAPE2, UROWN, LIST11)[FINIS];                       00028500
     COMMENT   READ DATA CARDS;                                         00028600
          FOR I1Q ~ 1, I1Q+1 STEP 1 UNTIL NVPSQ DO BEGIN                00028700
            I2Q ~ LCQ;                                                  00028800
            FOR J ~ 1, J+1 STEP 1 UNTIL NFQ DO BEGIN                    00028900
              L ~ NFQ+1-J;                                              00029000
              IF ((I2Q) MOD (NLQ[L]))>0 THEN GO TO L26;                 00029100
              I2Q ~ I2Q DIV NLQ[L]  END;                                00029200
L26:        FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                   00029300
              I ~ LBAQ[M];                                              00029400
                 SUMSQ[I.[36:3],I.[39: 9]] ~                            00029500
                 SUMSQ[I.[36:3],I.[39: 9]] + DATAQ[I1Q];                00029600
              LBAQ[M] ~ I+INCQ[M,L];                                    00029700
              IF L>LSQQ[M] THEN GO TO L28;                              00029800
              J ~ LBAQ[M];                                              00029900
              K ~ LBASSQ+(M-1)|NDVQ;                                    00030000
              FOR N ~ J, N+1 STEP 1 UNTIL I DO BEGIN                    00030100
                N1Q ~ K+((N-J) MOD (NDVQ));                             00030200
                 SUMSQ[N1Q.[36:3],N1Q.[39: 9]] ~                        00030300
                 SUMSQ[N1Q.[36:3],N1Q.[39: 9]] +                        00030400
                 SUMSQ[N.[36:3],N.[39: 9]]*2;                           00030500
                IF L{1 THEN GO TO L29;                                  00030600
                 SUMSQ[N.[36:3],N.[39: 9]] ~ 0;                         00030700
L29:            END;                                                    00030800
L28:          END;                                                      00030900
            IF LCMAXQ{LCQ THEN GO TO L31;                               00031000
            LCQ ~ LCQ+1  END;                                           00031100
          GO TO L20;                                                    00031200
L31:      FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                     00031300
               IF REAL(CONT AND BOOLEAN(CWDQ[M])) ! 0  THEN  GO TO L36  00031400
                    ELSE  GO TO L34;                                    00031500
L36:           I ~ LBAQ[M];                                             00031600
            J ~ LBAQ[M]-INCQ[M,1];                                      00031700
            FOR K ~ I, K+1 STEP 1 UNTIL J DO BEGIN                      00031800
                 SUMSQ[K.[36:3],K.[39: 9]] ~                            00031900
                 SUMSQ[K.[36:3],K.[39: 9]]/NDFQ[M];  END;               00032000
L34:      END;                                                          00032100
          FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                     00032200
            I ~ LBASSQ+(M-1)|NDVQ;                                      00032300
            J ~ I+NDVQ-1;                                               00032400
            FOR K ~ I, K+1 STEP 1 UNTIL J DO BEGIN                      00032500
                 SUMSQ[K.[36:3],K.[39: 9]] ~                            00032600
                 SUMSQ[K.[36:3],K.[39: 9]]/NDFQ[M];  END;               00032700
            END;                                                        00032800
DMP:                                                                    00032900
          FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                     00033000
            NDFQ[M] ~ LCMAXQ DIV (NDVQ|NDFQ[M])  END;                   00033100
          FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                     00033200
            IF M{1 THEN GO TO L46;                                      00033300
            I ~ M-1;                                                    00033400
            FOR J ~ 1, J+1 STEP 1 UNTIL I DO BEGIN                      00033500
               IF REAL(BOOLEAN(CWDQ[J]) AND NOT BOOLEAN(CWDQ[M]) AND    00033600
                    COMPCONT) ! 0  THEN  GO TO L50  ELSE  GO TO L52;    00033700
L52:           IF REAL(BOOLEAN(CWDQ[M]) AND NOT BOOLEAN(CWDQ[J]) AND    00033800
                    COMPCONT) ! 0  THEN  GO TO L53  ELSE  GO TO L50;    00033900
L53:           NDFQ[M] ~ NDFQ[M] - NDFQ[J];                             00034000
              FOR K ~ 1, K+1 STEP 1 UNTIL NDVQ DO BEGIN                 00034100
                L ~ K+(M-1)|NDVQ+LBASSQ-1;                              00034200
                N ~ K+(J-1)|NDVQ+LBASSQ-1;                              00034300
                 SUMSQ[L.[36:3],L.[39: 9]] ~                            00034400
                 SUMSQ[L.[36:3],L.[39: 9]]                              00034500
                -SUMSQ[N.[36:3],N.[39: 9]];  END;                       00034600
L50:      END;                                                          00034700
L46:        END;                                                        00034800
          WRITE(LINE,FL47,LIST3);                                       00034900
          FOR LDVQ ~ 1, LDVQ+1 STEP 1 UNTIL NDVQ DO BEGIN               00035000
            WRITE(LINE,FL60,LIST7);                                     00035100
            FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                   00035200
               IF REAL(CONT AND BOOLEAN(CWDQ[M])) ! 0  THEN  GO TO L64  00035300
                    ELSE  GO TO L62;                                    00035400
L64:           WRITE(LINE, FL66, LIST8);                                00035500
              I ~ LBAQ[M]+LDVQ-1;                                       00035600
              J ~ LBAQ[M]-INCQ[M,1];                                    00035700
              WRITE(LINE,FL68,LIST9);                                   00035800
L62:      END;                                                          00035900
            WRITE(LINE,FL70);                                           00036000
            FOR M ~ 1, M+1 STEP 1 UNTIL NCWQ DO BEGIN                   00036100
              I ~ LBASSQ+LDVQ+(M-1)|NDVQ-1;                             00036200
               T~SUMSQ[I.[36:3],I.[39: 9]]/NDFQ[M];                     00036300
              WRITE(LINE,FL74,LIST10);                                  00036400
              END;                                                      00036500
            END;                                                        00036600
          WRITE(LINE,FL76,LIST6);                                       00036700
          SPACE(TAPE2,-1);                                              00036800
          READ(TAPE2,FL37,LIST6)[FINIS];                                00036900
          WRITE(LINE,FL78,LIST6);                                       00037000
          ERROR(0);                                                     00037100
          END  END;                                                     00037200
COMMENT **************************  OUTER BLOCK  ************;          00037300
  XPR~QQ~0;                                                             00037400
            SENSL[1]~FALSE;                                             00037500
            SENSL[2]~FALSE;                                             00037600
            SENSL[3]~FALSE;                                             00037700
            SENSL[4]~FALSE;                                             00037800
               DO                                                       00037900
          BEGIN                                                         00038000
               READ(CARDS, 10, IMG[*])[EOCDS];                          00038100
               WRITE(TAPE2, 10, IMG[*]);                                00038200
          END  UNTIL FALSE;                                             00038300
EOCDS:         REWIND(TAPE2);                                           00038400
MAINPRO; FINIS:                                                         00038500
END OF OUTER BLK.                                                       00038600
                LAST CARD ON CRDIMG TAPE                                99999999
