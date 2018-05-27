              BEGIN                                                     00000100
COMMENT        THE CUBE LIBRARY NUMBER IS T800012. THE PROGRAM NAME IS  00000200
               BMD02R/DISK. THIS VERSION IS DATED 7/5/68;               00000300
% BMD02R ---- STEPWISE REGRESSION (ALGOL)                               00000400
FORMAT F1(/"NO. OF INDEX PLOTS REQUESTED = ",I2," WHICH IS GREATER THAN"00000500
         " ALLOWED."),                                                  00000600
       F2(/"A REQUEST HAS BEEN MADE TO PLOT VARIABLE",I3,". THIS IS GRE"00000700
         "ATER THAN THE",I3," VARIABLES IDENTIFIED FOR THIS PROBLEM."), 00000800
       F3(//"DATA ERROR. LAST DATA CARD READ IS PRINTED ABOVE.");       00000900
INTEGER        ERRORIDENT;                                              00001000
BOOLEAN        PAGEIT,FRFIELD;                                          00001100
LABEL          NEXTPROB;                                                00001200
INTEGER        DATAPRINT, OMITREGRESS;                                  00001300
FILE IN        CARD (2,10);                                     %9/69 MC00001400
DEFINE         TAPE=CARD#;                                      %9/69 MC00001500
FILE OUT       PUNCH 4 (2,15) ;                                         00001600
FILE PRINT 4(2,15);                                                     00001700
DEFINE LINE = PRINT # ;                                                 00001800
INTEGER       JN1,JNVA,BB,JN2;                                          00001900
FORMAT        DB1(//10R12.5//);                                         00002000
FORMAT FSETUP(X16,I4,X3,I2,X8,I2,X44,I1);                               00002100
FILE TAPE2 DISK SERIAL [20:1000](4,180);                                00002200
FILE OUTT 4(2,15); REAL REGRESSCHECK;                                   00002300
BOOLEAN ARRAY  SENSL[0:4];                                              00002400
REAL ARRAY  COMMON[0:33,0:511];                                         00002500
REAL  Q,XPR;                                                            00002600
REAL ARRAY ARRAY1, ARRAY3[0:4,0:511];                                   00002700
INTEGER  K;                                                             00002800
LABEL FINIS;                                                            00002900
INTEGER ARRAY  SVKTRANS[0:100,0:3];                                     00003000
                                                                        00003100
REAL ARRAY  SVX[0:81],  SVA[0:81,0:81],  SVC[0:100],  SVTRANS[0:100];   00003200
                                                                        00003300
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00003400
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00003500
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00003600
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00003700
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00003800
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00003900
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00004000
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00004100
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00004200
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00004300
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00004400
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00004500
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00004600
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00004700
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00004800
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00004900
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00005000
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00005100
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300005200
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00005300
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00005400
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00005500
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00005600
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00005700
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00005800
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00005900
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00006000
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100006100
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00006200
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00006300
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00006400
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00006500
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00006600
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00006700
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00006800
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00006900
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00007000
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00007100
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00007200
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00007300
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00007400
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00007500
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00007600
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00007700
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00007800
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00007900
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00008000
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00008100
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00008200
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00008300
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00008400
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00008500
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00008600
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00008700
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00008800
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800008900
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00009000
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00009100
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00009200
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00009300
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00009400
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00009500
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00009600
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00009700
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00009800
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00009900
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00010000
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00010100
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00010200
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00010300
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00010400
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00010500
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00010600
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00010700
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00010800
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00010900
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00011000
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00011100
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00011200
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00011300
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00011400
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00011500
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00011600
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00011700
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00011800
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00011900
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00012000
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00012100
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00012200
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00012300
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00012400
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00012500
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00012600
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100012700
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00012800
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00012900
(136);EXIT:    FORMATPHRASE ~ ERRTOG;                                   00013000
           END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00013100
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00013200
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00013300
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000013400
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00013500
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00013600
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00013700
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00013800
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00013900
PROCEDURE ERROR(N); VALUE N; INTEGER N; BEGIN                           00014000
  FORMAT F(///////"STOP / PAUSE  NO. ", I5);                            00014100
  WRITE (PRINT, F, N);  GO TO FINIS END;                                00014200
FORMAT OKTL (50O);                                                      00014300
REAL PROCEDURE INT(ARG1);                                               00014400
   VALUE ARG1;                                                          00014500
   REAL ARG1;                                                           00014600
BEGIN INT~(SIGN(ARG1)|ENTIER(ABS(ARG1))) END;                           00014700
REAL PROCEDURE MAX(ARG1,ARG2);                                          00014800
   VALUE ARG1,ARG2;                                                     00014900
   REAL ARG1,ARG2;                                                      00015000
BEGIN MAX~(IF ARG1}ARG2 THEN ARG1 ELSE ARG2) END;                       00015100
REAL PROCEDURE MIN(ARG1,ARG2);                                          00015200
   VALUE ARG1,ARG2;                                                     00015300
   REAL ARG1,ARG2;                                                      00015400
BEGIN MIN~(IF ARG1{ARG2 THEN ARG1 ELSE ARG2) END;                       00015500
COMMENT **************************  FNAF  ****************;             00015600
REAL PROCEDURE FNAF(JI,JJ);                                             00015700
  INTEGER JI,JJ;                                                        00015800
BEGIN                                                                   00015900
  DEFINE  SVSTDEV =COMMON#;                                             00016000
  DEFINE  SVKTRANS=COMMON#;                                             00016100
                                                                        00016200
                                                                        00016300
                                                                        00016400
                                                                        00016500
                                                                        00016600
                                                                        00016700
                                                                        00016800
                                                                        00016900
                                                                        00017000
OWN INTEGER JKKK, JLLL;                                                 00017100
OWN REAL JAF;                                                           00017200
LABEL DUMMY;                                                            00017300
JKKK~MIN(JI,JJ);                                                        00017400
JLLL~MAX(JI,JJ);                                                        00017500
JAF~SVA[JLLL,JKKK];                                                     00017600
GO TO DUMMY;                                                            00017700
DUMMY: FNAF~JAF  END;                                                   00017800
COMMENT **************************  SRAOUT  ****************;           00017900
PROCEDURE SRAOUT;                                                       00018000
BEGIN                                                                   00018100
  DEFINE  SVSTDEV =COMMON#;                                             00018200
  DEFINE  SVKTRANS=COMMON#;                                             00018300
  DEFINE  JIP     =COMMON[0,302]#;                                      00018400
                                                                        00018500
                                                                        00018600
                                                                        00018700
                                                                        00018800
                                                                        00018900
                                                                        00019000
                                                                        00019100
                                                                        00019200
                                                                        00019300
REAL ARRAY   SVFMT1[0:20];                                              00019400
INTEGER  DX1;                                                           00019500
INTEGER  INDEX1;                                                        00019600
OWN INTEGER JMN, JKK, JI, JNRTEN, JM, JMM, JLL, JKTOP, JNX, JNFX,       00019700
 JK, JJ, JNN, JID, JJK;                                                 00019800
FORMAT FL9901("(1H",I1,",1X,8HVARIABLE,5X,",I2,"(I2,9X)/3X,6HNUMBER/)"),00019900
FL9902(20A6),                                                           00020000
FL9903("(5X,I2,",I3,"X,",I2,"(F9.3,2X))");                              00020100
REAL PP;                                                                00020200
LABEL      DONE1  ;                                                     00020300
LABEL    DO4, DO5  ;                                                    00020400
INTEGER IP ;                                                            00020500
LABEL          R0,DO2  ;                                                00020600
LABEL DUMMY,L2,L3,L5,L6,L1000;                                          00020700
PROCEDURE HEAD (JKK,JMM)  ;                                             00020800
INTEGER        JKK,JMM  ;                                               00020900
          BEGIN                                                         00021000
INTEGER ARRAY   SVLLL[0: 80 ]  ;                                        00021100
INTEGER        J  ;                                                     00021200
LABEL          L5,L6  ;                                                 00021300
FORMAT    OUT2 (X2, "VARIABLE",X5,10(I2,X9)/)  ,                        00021400
          OUT3 (X3, "NUMBER " )  ;                                      00021500
INTEGER        JLL,DX1  ;                                               00021600
LIST LIST2(FOR DX1~1 STEP 1 UNTIL JMM DO SVLLL[DX1]);                   00021700
               J ~ JMM - JKK + 1 ;                                      00021800
               FOR JLL ~ 1 STEP 1 UNTIL JMM DO                          00021900
               SVLLL[JLL] ~ JLL + (JKK - 1 ) | 10 ;                     00022000
L6  :                                                                   00022100
               WRITE (PRINT,OUT2, LIST2 )  ;                            00022200
               WRITE (PRINT,OUT3 )  ;                                   00022300
          END;                                                          00022400
PROCEDURE DIAG (JKK,JMM,SVA,JID ) ;                                     00022500
INTEGER        JKK,JMM,JID  ;                                           00022600
REAL ARRAY     SVA[0,0]  ;                                              00022700
          BEGIN                                                         00022800
INTEGER        DX1,I,J,JKZ ;                                            00022900
INTEGER        JRR ;                                                    00023000
FORMAT  ANS9(X4,I2,X103,R10.3,X1),                                      00023100
          ANS8(X4,I2,X92,2(R10.3,X1)),                                  00023200
          ANS7(X4,I2,X81,3(R10.3,X1)),                                  00023300
          ANS6(X4,I2,X70,4(R10.3,X1)),                                  00023400
          ANS5(X4,I2,X59,5(R10.3,X1)),                                  00023500
          ANS4(X4,I2,X48,6(R10.3,X1)),                                  00023600
          ANS3(X4,I2,X37,7(R10.3,X1)),                                  00023700
          ANS2(X4,I2,X26,8(R10.3,X1)),                                  00023800
          ANS1(X4,I2,X15,9(R10.3,X1)),                                  00023900
          ANS0(X4,I2,X4 ,10(R10.3,X1));                                 00024000
LABEL     DO3  ;                                                        00024100
LABEL          S0,S1,S2,S3,S4,S5,S6,S7,S8,S9  ;                         00024200
SWITCH RES1 ~ S0,S1,S2,S3,S4,S5,S6,S7,S8,S9   ;                         00024300
LIST LIST6(JRR,FOR DX1~JKZ STEP 1 UNTIL JMM DO SVA[DX1,JRR]);           00024400
               JKK ~ JKK ;  JMM ~ JMM ;                                 00024500
               JRR ~ JID  ;                                             00024600
               J ~ JMM - JKK + 1 ;                                      00024700
               JKZ ~ JKK  ;                                             00024800
               FOR I ~ 1 STEP 1 UNTIL J DO                              00024900
          BEGIN                                                         00025000
               GO TO RES1[I]  ;                                         00025100
S0:            WRITE(PRINT,ANS0,LIST6) ; GO TO DO3  ;                   00025200
S1:            WRITE(PRINT,ANS1,LIST6) ; GO TO DO3  ;                   00025300
S2:            WRITE(PRINT,ANS2,LIST6) ; GO TO DO3  ;                   00025400
S3:            WRITE(PRINT,ANS3,LIST6) ; GO TO DO3  ;                   00025500
S4:            WRITE(PRINT,ANS4,LIST6) ; GO TO DO3  ;                   00025600
S5:            WRITE(PRINT,ANS5,LIST6) ; GO TO DO3  ;                   00025700
S6:            WRITE(PRINT,ANS6,LIST6) ; GO TO DO3  ;                   00025800
S7:            WRITE(PRINT,ANS7,LIST6) ; GO TO DO3  ;                   00025900
S8:            WRITE(PRINT,ANS8,LIST6) ; GO TO DO3  ;                   00026000
S9:            WRITE(PRINT,ANS9,LIST6) ; GO TO DO3  ;                   00026100
DO3:           JRR ~ JRR + 1  ;                                         00026200
               JKZ ~ JKZ + 1  ;                                         00026300
               END ;                                                    00026400
          END ;                                                         00026500
PROCEDURE SQUARE (JKK,JMM,JID,SVA)  ;                                   00026600
INTEGER        JKK,JMM,JID ;                                            00026700
REAL ARRAY     SVA[0,0]  ;                                              00026800
          BEGIN                                                         00026900
INTEGER        J,DX1,I,JIF  ;                                           00027000
LIST LIST6(JIF,FOR DX1~JKK STEP 1 UNTIL JMM DO SVA[DX1,JIF]);           00027100
FORMAT ANS0 (X4,I2,X4,10(R10.3,X1));                                    00027200
                                                                        00027300
               JKK ~ JKK ;  JMM ~ JMM ;                                 00027400
               JIF  ~ JID  ;                                            00027500
               J ~ JMM - JKK + 1  ;                                     00027600
               FOR I ~ 1 STEP 1 UNTIL 10 DO                             00027700
          BEGIN                                                         00027800
               WRITE ( PRINT,ANS0,LIST6 ) ;                             00027900
               JIF  ~ JIF  + 1  ;                                       00028000
          END ;                                                         00028100
          END ;                                                         00028200
INTEGER        JJJ ;                                                    00028300
INTEGER        ZONE,I,J,JKL,JIX,JKX,JJX,JDX ;                           00028400
               ZONE ~ ENTIER (JIP/10.01 ) + 1 ;                         00028500
               JKK ~ 1 ;  JMM ~ 10 ;  JID ~ 1 ;  JKL ~ 1 ;              00028600
               FOR I ~ ZONE STEP - 1 WHILE I > 0 DO                     00028700
          BEGIN                                                         00028800
               IF JIP - JMM < 0 THEN JMM ~ JIP  ;                       00028900
               JIX ~ JKL;  JKX ~ JMM;  JJX ~ JKK;  JDX ~ JID;           00029000
               HEAD (JIX,JKX)  ;                                        00029100
               DIAG (JJX,JKX,SVA,JDX )  ;                               00029200
               JJJ ~ ZONE - 1 ;                                         00029300
               FOR J ~ 1 STEP 1 UNTIL JJJ DO                            00029400
          BEGIN                                                         00029500
               JIX ~ JIX + 1  ;                                         00029600
               JJX ~ JJX + 10 ;  JKX ~ JKX + 10 ;                       00029700
               IF JIP - JKX < 0 THEN JKX ~ JIP  ;                       00029800
               HEAD ( JIX,JKX )  ;                                      00029900
               SQUARE ( JJX,JKX,JDX,SVA )  ;                            00030000
          END ;                                                         00030100
               JKL ~ JKL + 1 ;  JKK ~ JKK + 10 ; JMM ~ JMM + 10 ;       00030200
               JID ~ JID + 10 ;                                         00030300
               ZONE ~ ZONE - 1  ;                                       00030400
          END ;                                                         00030500
DUMMY: END;                                                             00030600
COMMENT **************************  SRCHECK  ****************;          00030700
PROCEDURE SRCHECK(JI,JJ,JII1,JJJ1,JSGN);                                00030800
  INTEGER JI,JJ,JII1,JJJ1;                                              00030900
  REAL JSGN;                                                            00031000
BEGIN                                                                   00031100
  DEFINE  SVSTDEV =COMMON#;                                             00031200
  DEFINE  SVKTRANS=COMMON#;                                             00031300
                                                                        00031400
                                                                        00031500
                                                                        00031600
                                                                        00031700
                                                                        00031800
                                                                        00031900
                                                                        00032000
                                                                        00032100
                                                                        00032200
FORMAT FL8001(X3,"I= ",I3," J= ",I3,X1,"C(I)= ",F3.0,X1,"C(J)= ",F3.0,  00032300
   X1,"SGN= ",F3.0," II1= ",I3," JJ1= ",I3);                            00032400
LIST LIST1(JI,JJ,SVC[JI],SVC[JJ],JSGN,JII1,JJJ1);                       00032500
LABEL DUMMY,L2,L3,L5,L4;                                                00032600
IF (SVC[JI])>0 THEN GO TO L2;                                           00032700
IF (SVC[JJ])>0 THEN GO TO L5 ELSE GO TO L3;                             00032800
L2: IF (SVC[JJ]){0 THEN GO TO L5;                                       00032900
L3: JSGN~1;                                                             00033000
GO TO L4;                                                               00033100
L5: JSGN~-1;                                                            00033200
L4: JII1~JJ;                                                            00033300
JJJ1~JI;                                                                00033400
WRITE(PRINT,FL8001,LIST1);                                              00033500
GO TO DUMMY;                                                            00033600
DUMMY: END;                                                             00033700
COMMENT **************************  SRRDLBL2  ****************;         00033800
PROCEDURE SRRDLBL2(JNLBVAR,JNVAR,SVARRAY);                              00033900
  VALUE JNLBVAR,JNVAR;                                                  00034000
  INTEGER JNLBVAR,JNVAR;                                                00034100
  REAL ARRAY  SVARRAY[0];                                               00034200
BEGIN                                                                   00034300
REAL ARRAY  ARRAY1[0:1];                                                00034400
  DEFINE  JLABEL  =ARRAY1[1]#;                                          00034500
  DEFINE  JALABEL =ARRAY1[1]#;                                          00034600
INTEGER ARRAY   SVIDUM[0:7];                                            00034700
REAL ARRAY   SVDUMY[0:7];                                               00034800
INTEGER  DX1;                                                           00034900
OWN INTEGER JI, JN, JITEST, JJ, JK;                                     00035000
FORMAT FL3(A3,X3,7(I4,A6)),                                             00035100
FL5(/" LABELS CARD NOT FOUND WHEN EXPECTED"),                           00035200
FL12(/" LABELS CARD INDEX",I7," INCORRECT. LABEL ",A6," IGNORED.");     00035300
LIST LIST1(JITEST,FOR DX1~1 STEP 1 UNTIL 7 DO [SVIDUM[DX1],SVDUMY[DX1]])00035400
 ;                                                                      00035500
LIST LIST2(JK,SVDUMY[JJ]);                                              00035600
LABEL DUMMY,L20,L4,L6,L11,L7,L13,L8,L9;                                 00035700
COMMENT EQUIVALENCE INTEGER AND FLOATING NAMES SO THAT INTEGER SUBTRACTI00035800
 ON;                                                                    00035900
COMMENT MAY BE USED TO TEST ALPHABETIC EQUALITY;                        00036000
JALABEL~(+"LAB");                                                       00036100
COMMENT BLANK VARIABLES;                                                00036200
JI~1;                                                                   00036300
DO BEGIN                                                                00036400
SVARRAY[JI]~(+"      ")  END UNTIL (JI~(JI+1))>JNVAR;                   00036500
COMMENT IF NO LABELS, RETURN;                                           00036600
IF JNLBVAR{0 THEN GO TO DUMMY;                                          00036700
JN~0;                                                                   00036800
COMMENT READ 1 LABELS CARD;                                             00036900
L20: READ(TAPE,FL3,LIST1)[FINIS];                                       00037000
COMMENT TEST FOR @LAB@ IN FIRST 3 COLS.;                                00037100
IF (JITEST=JLABEL) THEN GO TO L6 ELSE GO TO L4;                         00037200
COMMENT ERROR--PRINT MESSAGE AND QUIT;                                  00037300
L4: WRITE(PRINT,FL5);                                                   00037400
GO TO FINIS;                                                            00037500
COMMENT EXAMINE 7 FIELDS;                                               00037600
L6: JJ~1;                                                               00037700
DO BEGIN                                                                00037800
  JK~SVIDUM[JJ];                                                        00037900
  COMMENT TEST INDEX.  IF 0, IGNORE.  IF ILLEGAL, PRINT MESSAGE AND;    00038000
  COMMENT IGNORE EXCEPT TO COUNT;                                       00038100
  IF (XPR~(JK))=0 THEN GO TO L8;                                        00038200
  IF XPR<0 THEN GO TO L11;                                              00038300
  IF (JK{JNVAR) THEN GO TO L7;                                          00038400
  L11: WRITE(PRINT,FL12,LIST2);                                         00038500
  GO TO L13;                                                            00038600
  COMMENT MOVE LABEL TO ARRAY;                                          00038700
  L7: SVARRAY[JK]~SVDUMY[JJ];                                           00038800
  COMMENT STEP NUMBER OF VARIABLES;                                     00038900
  L13: JN~JN+1;                                                         00039000
  COMMENT TEST FOR END. IF END, RETURN. IF NOT, SCAN OTHER FIELDS.;     00039100
  IF (JN}JNLBVAR) THEN GO TO DUMMY;                                     00039200
  L8: END UNTIL (JJ~(JJ+1))>7;                                          00039300
GO TO L20;                                                              00039400
L9: GO TO DUMMY;                                                        00039500
DUMMY: END;                                                             00039600
COMMENT **************************  SRSTEP  ****************;           00039700
PROCEDURE SRSTEP( JKAY)  ;                                              00039800
INTEGER  JKAY  ;                                                        00039900
BEGIN                                                                   00040000
  DEFINE  JIP     =COMMON[0,302] #  ;                                   00040100
  DEFINE  JKAY    =COMMON[0,306] # ;                                    00040200
  DEFINE  JFLAG   =COMMON[0,307] # ;                                    00040300
                                                                        00040400
                                                                        00040500
                                                                        00040600
                                                                        00040700
                                                                        00040800
                                                                        00040900
                                                                        00041000
                                                                        00041100
                                                                        00041200
                                                                        00041300
                                                                        00041400
REAL ARRAY   SVU[0:81];                                                 00041500
OWN INTEGER JKAY1, JKAY2, JI, JJ;                                       00041600
OWN REAL JXAY3;                                                         00041700
LABEL DUMMY,L3,L2;                                                      00041800
               JKAY ~ JKAY ;                                            00041900
JKAY1~JKAY-1;                                                           00042000
JKAY2~JKAY+1;                                                           00042100
JXAY3~SVA[JKAY,JKAY];                                                   00042200
IF JKAY1{0 THEN GO TO L3;                                               00042300
JI~1;                                                                   00042400
DO BEGIN                                                                00042500
  SVU[JI]~SVA[JKAY,JI];                                                 00042600
  SVA[JKAY,JI]~0;                                                       00042700
  END UNTIL (JI~(JI+1))>JKAY1;                                          00042800
L3: SVU[JKAY]~-JFLAG;                                                   00042900
SVA[JKAY,JKAY]~0;                                                       00043000
IF (JKAY2>JIP) THEN GO TO L2;                                           00043100
JI~JKAY2;                                                               00043200
DO BEGIN                                                                00043300
  SVU[JI]~SVA[JI,JKAY];                                                 00043400
  SVA[JI,JKAY]~0;                                                       00043500
  END UNTIL (JI~(JI+1))>JIP;                                            00043600
L2: JI~1;                                                               00043700
DO BEGIN                                                                00043800
  JJ~JI;                                                                00043900
  DO BEGIN                                                              00044000
    SVA[JJ,JI]~SVA[JJ,JI]-(SVU[JI]|SVU[JJ])/JXAY3;                      00044100
    END UNTIL (JJ~(JJ+1))>JIP  END UNTIL (JI~(JI+1))>JIP;               00044200
GO TO DUMMY;                                                            00044300
DUMMY: END;                                                             00044400
COMMENT **************************  SRSTEPRG  ****************;         00044500
PROCEDURE SRSTEPRG(SVC)  ;                                              00044600
REAL ARRAY   SVC[0]  ;                                                  00044700
BEGIN                                                                   00044800
  DEFINE  SVSTDEV =COMMON#;                                             00044900
  DEFINE  SVKTRANS=COMMON#;                                             00045000
  DEFINE  JIP     =COMMON[0,302]#;                                      00045100
  DEFINE  JFINC   =COMMON[0,303]#;                                      00045200
  DEFINE  JFOUT   =COMMON[0,305]#;                                      00045300
  DEFINE  JKAY    =COMMON[0,306]#;                                      00045400
  DEFINE  JFLAG   =COMMON[0,307]#;                                      00045500
  DEFINE  JKDEP   =COMMON[0,310]#;                                      00045600
  DEFINE  JXN     =COMMON[0,313]#;                                      00045700
  DEFINE  JTOL    =COMMON[0,314]#;                                      00045800
  DEFINE  JDF     =COMMON[0,315]#;                                      00045900
                                                                        00046000
                                                                        00046100
                                                                        00046200
                                                                        00046300
                                                                        00046400
                                                                        00046500
OWN INTEGER JK, JKMIN, JKMAX;                                           00046600
OWN REAL JVIN, JVOUT, JVMIN, JVMAX, JVSUBK;                             00046700
COMMENT  THE FOLLOWING FUNCTIONS ARE REQUIRED:                          00046800
    FNAF;                                                               00046900
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00047000
    SRSTEP;                                                             00047100
LABEL DUMMY,L10,L1000,L2,L7,L6;                                         00047200
JVIN~((JFINC|SVA[JKDEP,JKDEP])/(JFINC+JXN-JDF-1))+2;                    00047300
               IF (JXN=JDF) THEN JVOUT~-7.0 ELSE                        00047400
JVOUT~((JFOUT|SVA[JKDEP,JKDEP])/(JXN-JDF))-7;                           00047500
JVMIN~9999.9;                                                           00047600
JVMAX~0;                                                                00047700
JK~1;                                                                   00047800
DO BEGIN                                                                00047900
SVC[JK]~SVC[JK];                                                        00048000
               SVA[JK,JK] ~ SVA[JK,JK]  ;                               00048100
  IF (XPR~(SVC[JK]-1))>0 THEN GO TO L10;                                00048200
  IF XPR=0 THEN GO TO L1000;                                            00048300
  JVSUBK~SVC[JK]+(FNAF(JK,JKDEP)*2)/ABS(SVA[JK,JK]);                    00048400
  IF (JVMIN{JVSUBK) THEN GO TO L1000;                                   00048500
  JVMIN~JVSUBK;                                                         00048600
  JKMIN~JK;                                                             00048700
  GO TO L1000;                                                          00048800
  L10: IF (SVA[JK,JK]<JTOL) THEN GO TO L1000;                           00048900
  JVSUBK~SVC[JK]+FNAF(JK,JKDEP)*2/SVA[JK,JK];                           00049000
  IF (JVSUBK{JVMAX) THEN GO TO L1000;                                   00049100
  JVMAX~JVSUBK;                                                         00049200
  JKMAX~JK;                                                             00049300
  L1000: END UNTIL (JK~(JK+1))>JIP;                                     00049400
IF (JVOUT{JVMIN) THEN GO TO L2;                                         00049500
SVC[JKMIN]~SVC[JKMIN]+9;                                                00049600
JKAY~INT(JKMIN);                                                        00049700
JFLAG~-1;                                                               00049800
GO TO L7;                                                               00049900
L2: IF (JVMAX<JVIN) THEN GO TO L6;                                      00050000
IF (JXN-JDF{3-SVC[JKMAX]) THEN GO TO L6;                                00050100
SVC[JKMAX]~SVC[JKMAX]-9;                                                00050200
JKAY~INT(JKMAX);                                                        00050300
JFLAG~1;                                                                00050400
L7: SRSTEP(JKAY)  ;                                                     00050500
JDF~JDF+JFLAG;                                                          00050600
GO TO DUMMY;                                                            00050700
L6: JFLAG~0;                                                            00050800
GO TO DUMMY;                                                            00050900
DUMMY: END;                                                             00051000
COMMENT **************************  SRTRANGN  ****************;         00051100
PROCEDURE SRTRANGN;                                                     00051200
BEGIN                                                                   00051300
  DEFINE  SVSTDEV =COMMON#;                                             00051400
  DEFINE  SVKTRANS=COMMON#;                                             00051500
  DEFINE  JNTGC   =COMMON[0,301]#;                                      00051600
  DEFINE  JNCASE  =COMMON[0,308]#;                                      00051700
  DEFINE  JNINCS  =COMMON[0,309]#;                                      00051800
                                                                        00051900
                                                                        00052000
                                                                        00052100
                                                                        00052200
                                                                        00052300
                                                                        00052400
                                                                        00052500
                                                                        00052600
                                                                        00052700
OWN INTEGER JI, JM, JN, JNTRANS, JNEWB;                                 00052800
OWN REAL JE, JFN, JB, JXNEWB;                                           00052900
FORMAT FL199(" TRANSGENERATION CARD ",I3," MISPUNCHED OR OUT OF ORDER"),00053000
FL201(" THE VALUE OF VARIABLE ",I4," IN CASE ",I5,                      00053100
   " VIOLATED THE RESTRICTIONS FOR TRANSGENERATION OF TYPE ",I3,"."/    00053200
   " THE PROGRAM CONTINUED LEAVING VARIABLE ",I4," UNCHANGED.");        00053300
LIST LIST1(JI);                                                         00053400
LIST LIST2(JN,JNINCS,SVKTRANS[(K~(3|JI-1)).[33:6],K.[39:9]],JM);        00053500
LABEL DUMMY,L99,L50,L1,L107,L108,L2,L111,L112,L3,L4,L5,L119,L6,L124,    00053600
 L129,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,L17,L20,L21,L22,L23,L24,     00053700
 L197,L198,L100;                                                        00053800
SWITCH SWGO1~L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,    00053900
 L17,L99,L99,L20,L21,L22,L23,L24;                                       00054000
FORMAT FL5775 (20A6);                                                   00054100
FORMAT FL5776 (20A6);                                                   00054200
REAL PROCEDURE DFASN(ARGXX);                                            00054300
VALUE ARGXX;                                                            00054400
REAL ARGXX;                                                             00054500
BEGIN DFASN~ARCTAN(ARGXX/SQRT(1-ARGXX*2))  END;                         00054600
JI~1;                                                                   00054700
DO BEGIN                                                                00054800
JM~SVKTRANS[(K~(3|JI-2)).[33:6],K.[39:9]];                              00054900
JN~SVKTRANS[(K~(3|JI)).[33:6],K.[39:9]];                                00055000
JNTRANS~SVKTRANS[(K~(3|JI-1)).[33:6],K.[39:9]];                         00055100
IF (JM>81) THEN GO TO L99;                                              00055200
IF (JN>81) THEN GO TO L99;                                              00055300
IF ((JNTRANS-25)|JNTRANS)<0 THEN GO TO L50;                             00055400
L99: WRITE(PRINT,FL199,LIST1);                                          00055500
GO TO L100;                                                             00055600
L50: GO TO SWGO1[JNTRANS];                                              00055700
L1: IF (XPR~(SVX[JN]))>0 THEN GO TO L108;                               00055800
IF XPR<0 THEN GO TO L198;                                               00055900
L107: SVX[JM]~0;                                                        00056000
GO TO L100;                                                             00056100
L108: SVX[JM]~SQRT(SVX[JN]);                                            00056200
GO TO L100;                                                             00056300
L2: IF (XPR~(SVX[JN]))>0 THEN GO TO L112;                               00056400
IF XPR<0 THEN GO TO L198;                                               00056500
L111: SVX[JM]~1;                                                        00056600
GO TO L100;                                                             00056700
L112: SVX[JM]~SQRT(SVX[JN])+SQRT(SVX[JN]+1);                            00056800
GO TO L100;                                                             00056900
L3: IF (SVX[JN]){0 THEN GO TO L198;                                     00057000
SVX[JM]~0.4342944819|LN(SVX[JN]);                                       00057100
GO TO L100;                                                             00057200
L4: SVX[JM]~EXP(SVX[JN]);                                               00057300
GO TO L100;                                                             00057400
L5: IF (XPR~(SVX[JN]))=0 THEN GO TO L107;                               00057500
IF XPR<0 THEN GO TO L198;                                               00057600
IF (SVX[JN]}1) THEN GO TO L119;                                         00057700
JE~SQRT(SVX[JN]);                                                       00057800
SVX[JM]~DFASN(JE);                                                      00057900
GO TO L100;                                                             00058000
L119: SVX[JM]~3.14159265/2;                                             00058100
GO TO L100;                                                             00058200
L6: JFN~JNCASE;                                                         00058300
JE~SVX[JN]/(JFN+1);                                                     00058400
JB~JE+1/(JFN+1);                                                        00058500
IF (XPR~(JE))>0 THEN GO TO L124;                                        00058600
IF XPR<0 THEN GO TO L198;                                               00058700
IF (XPR~(JB))=0 THEN GO TO L107;                                        00058800
IF XPR<0 THEN GO TO L198;                                               00058900
SVX[JM]~DFASN(SQRT(JB));                                                00059000
GO TO L100;                                                             00059100
L124: IF (XPR~(JB))>0 THEN GO TO L129;                                  00059200
IF XPR<0 THEN GO TO L198;                                               00059300
SVX[JM]~DFASN(SQRT(JE));                                                00059400
GO TO L100;                                                             00059500
L129: JE~SQRT(JE);                                                      00059600
JB~SQRT(JB);                                                            00059700
SVX[JM]~DFASN(JE)+DFASN(JB);                                            00059800
GO TO L100;                                                             00059900
L7: IF (SVX[JN])=0 THEN GO TO L198;                                     00060000
SVX[JM]~1/SVX[JN];                                                      00060100
GO TO L100;                                                             00060200
L8: SVX[JM]~SVX[JN]+SVTRANS[JI];                                        00060300
GO TO L100;                                                             00060400
L9: SVX[JM]~SVX[JN]|SVTRANS[JI];                                        00060500
GO TO L100;                                                             00060600
L10: IF (XPR~(SVX[JN]))=0 THEN GO TO L107;                              00060700
IF XPR<0 THEN GO TO L198;                                               00060800
SVX[JM]~SVX[JN]*SVTRANS[JI];                                            00060900
GO TO L100;                                                             00061000
L11: JNEWB~SVTRANS[JI];                                                 00061100
SVX[JM]~SVX[JN]+SVX[JNEWB];                                             00061200
GO TO L100;                                                             00061300
L12: JNEWB~SVTRANS[JI];                                                 00061400
SVX[JM]~SVX[JN]-SVX[JNEWB];                                             00061500
GO TO L100;                                                             00061600
L13: JNEWB~SVTRANS[JI];                                                 00061700
SVX[JM]~SVX[JN]|SVX[JNEWB];                                             00061800
GO TO L100;                                                             00061900
L14: JNEWB~SVTRANS[JI];                                                 00062000
IF (SVX[JNEWB])=0 THEN GO TO L197;                                      00062100
SVX[JM]~SVX[JN]/SVX[JNEWB];                                             00062200
GO TO L100;                                                             00062300
L15: IF (SVX[JN]}SVTRANS[JI]) THEN GO TO L111 ELSE GO TO L107;          00062400
L16: JXNEWB~SVTRANS[JI];                                                00062500
IF (SVX[JN]}(JXNEWB)) THEN GO TO L111 ELSE GO TO L107;                  00062600
L17: IF (SVX[JN]){0 THEN GO TO L198;                                    00062700
SVX[JM]~LN(SVX[JN]);                                                    00062800
GO TO L100;                                                             00062900
L20: SVX[JM]~SIN(SVX[JN]);                                              00063000
GO TO L100;                                                             00063100
L21: SVX[JM]~COS(SVX[JN]);                                              00063200
GO TO L100;                                                             00063300
L22: IF (SVX[JN]>1.57079632) THEN GO TO L198;                           00063400
IF (SVX[JN]+1.57079632)<0 THEN GO TO L198;                              00063500
SVX[JM]~ARCTAN(SVX[JN]);                                                00063600
GO TO L100;                                                             00063700
L23: JNEWB~SVTRANS[JI];                                                 00063800
IF (SVX[JN]){0 THEN GO TO L198;                                         00063900
SVX[JM]~SVX[JN]*SVX[JNEWB];                                             00064000
GO TO L100;                                                             00064100
L24: IF (SVTRANS[JI]){0 THEN GO TO L198;                                00064200
SVX[JM]~SVTRANS[JI]*SVX[JN];                                            00064300
GO TO L100;                                                             00064400
L197: JN~JNEWB;                                                         00064500
L198: WRITE(PRINT,FL201,LIST2);                                         00064600
L100: END UNTIL (JI~(JI+1))>JNTGC;                                      00064700
GO TO DUMMY;                                                            00064800
DUMMY: END;                                                             00064900
COMMENT **************************  SRVFCHCK  ****************;         00065000
PROCEDURE SRVFCHCK(JNVF);                                               00065100
  INTEGER JNVF;                                                         00065200
BEGIN                                                                   00065300
FORMAT FL4000(/" ",X23,                                                 00065400
   "NUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFIED, ASSUMED ",   00065500
   "TO BE 1.");                                                         00065600
LABEL DUMMY,L10,L50,L20;                                                00065700
IF JNVF>0 THEN GO TO L20;                                               00065800
L10: WRITE(PRINT,FL4000);                                               00065900
JNVF~1;                                                                 00066000
L50: GO TO DUMMY;                                                       00066100
L20: IF (JNVF>10) THEN GO TO L10 ELSE GO TO DUMMY;                      00066200
DUMMY: END;                                                             00066300
COMMENT **************************  MAINPRO  ****************;          00066400
PROCEDURE MAINPRO;                                                      00066500
BEGIN                                                                   00066600
  DEFINE  SVXMIN  =COMMON#;                                             00066700
  DEFINE  SVXMAX  =COMMON#;                                             00066800
  DEFINE  SVKTRANS=COMMON#;                                             00066900
  DEFINE  SVSTDEV =COMMON#;                                             00067000
  DEFINE  JNTGC   =COMMON[0,301]#;                                      00067100
  DEFINE  JIP     =COMMON[0,302]#;                                      00067200
  DEFINE  JFINC   =COMMON[0,303]#;                                      00067300
  DEFINE  JRESDF  =COMMON[0,304]#;                                      00067400
  DEFINE  JFOUT   =COMMON[0,305]#;                                      00067500
  DEFINE  JKAY    =COMMON[0,306]#;                                      00067600
  DEFINE  JFLAG   =COMMON[0,307]#;                                      00067700
  DEFINE  JN      =COMMON[0,308]#;                                      00067800
  DEFINE  JNINCS  =COMMON[0,309]#;                                      00067900
  DEFINE  JKDEP   =COMMON[0,310]#;                                      00068000
  DEFINE  JIR     =COMMON[0,311]#;                                      00068100
  DEFINE  JXN     =COMMON[0,313]#;                                      00068200
  DEFINE  JTOL    =COMMON[0,314]#;                                      00068300
  DEFINE  JDF     =COMMON[0,315]#;                                      00068400
                                                                        00068500
  DEFINE  SVP     =ARRAY1#;                                             00068600
  DEFINE  SVKP    =ARRAY1#;                                             00068700
                                                                        00068800
                                                                        00068900
                                                                        00069000
                                                                        00069100
  DEFINE  SVIQ    =ARRAY3#;                                             00069200
  DEFINE  SVQ     =ARRAY3#;                                             00069300
INTEGER ARRAY TRANS[0:100];                                             00069400
INTEGER ARRAY   SVNIEN[0:80],  SVINEN[0:80],  SVNN[0:5],                00069500
  SVIN[0:80],  SVIVPT[0:33],  SVNUSE[0:20];                             00069600
REAL ARRAY   SVPPP[0:255],  SVFMT1[0:20],  SVB[0:80],                   00069700
  SVD[0:80],  SVXMEAN[0:80],  SVF[0:80],  SVANAME[0:80],                00069800
  SVBNAME[0:80],  SVTOLEV[0:80],  SVR[0:80],  SVFE[0:80],               00069900
  SVRES[0:19,0:511],  SVALBEL[0:80],  SVCOL[0:11],  SVPINT[0:11],       00070000
  SVQINT[0:11],  SVFMT2[0:20],  SVBES[0:5];                             00070100
ARRAY         DATA[0:JN1,0:JN2];                                        00070200
REAL ARRAY    PREDIC[0:JN2];                                            00070300
INTEGER       JKL;                                                      00070400
INTEGER  DX1;                                                           00070500
INTEGER ARRAY   COL[0:80]  ;                                            00070600
INTEGER ARRAY  VVC[0:80]  ;                                             00070700
INTEGER        NON  ;                                                   00070800
INTEGER        CHOOSE;                                                  00070900
INTEGER  INDEX1;                                                        00071000
ARRAY A[0:BB,0:5];                                                      00071100
INTEGER KI;                                                             00071200
OWN INTEGER JI, JNOV, JNVA, JNAIT, JNSPC, JNLV, JNVFC, JKIP, JJ, JLLL,  00071300
 JK, JL, JM, JMAXSTP, JNVIP, JLKL, JIDF, JIRDF, JNVI, JNVO, JNGO,       00071400
 JLNV, JII, JLMN, JLLLL, JLLLLL, JMM, JIJK, JNO, JLJK, JIIX, JKKK,      00071500
 JJJ, JJ1, JJJJJJ, JIRES, JJQ, JMNMN, JKLN, JKLM;                       00071600
OWN REAL JPROBLM, JSUBPRO, JTRNGEN, JCONDEL, JYES, JDXPLTS, JXNO,       00071700
 JFINISH, JDOT, JXMAN, JCODE, JSDAM, JCOVP, JCORP, JZEROI, JWIND,       00071800
 JYMAN, JXM, JXK, JWMAN, JCDF, JRESID, JSUMTAB, JVMAN, JDUM, JENTER,    00071900
 JRESSS, JRESMS, JREGDF, JREGSS, JREGMS, JFRATIO, JSTERR, JXMULTR,      00072000
 JALPHA, JFKAY, JR1SQ, JRSQ, JRSQI, JRMIN, JRMAX, JSUMB, JSRS, JAI,     00072100
 JSPI, JSQI, JAJ, JXX, JXY;                                             00072200
COMMENT  THE FOLLOWING FUNCTIONS ARE REQUIRED:                          00072300
    FNAF;                                                               00072400
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00072500
    SRVFCHCK, SRRDLBL2, SRTRANGN, SRAOUT, SRSTEPRG, SRREMOVE;           00072600
FORMAT FL8003(                                                          00072700
              X12,"BMD02R - STEPWISE REGRESSION - VERSION OF JULY 17,19600072800
6 HEALTH SCIENCES COMPUTING FACILITY, UCLA"/ X12,"MODIFIED FOR USE ON TH00072900
E BURROUGHS B5500 OF THE LOVELACE FOUNDATION BIOMATH DEPT"//            00073000
X2,"PROBLEM",X17,A6/                                                    00073100
X2,"NUMBER OF CASES",X16,I4/X2,"NUMBER OF ORIGINAL VARIABLES",X5,I2/X2, 00073200
   "NUMBER OF VARIABLES ADDED",X8,I2/X2,"TOTAL NUMBER OF VARIABLES",X8, 00073300
   I2/X2,"NUMBER OF SUB-PROBLEMS",X11,I2),                              00073400
                                                                        00073500
FLH5774(//"  DATA TABLE "//),                                           00073600
FL5774 (8I9),                                                           00073700
FLL5774(////////////////////////////////////////////////////////////////00073800
         ////////////////////////////////////////////////////////////), 00073900
FL5776(20A6),                                                           00074000
FL8001(A6,X3,A6,X1,I4,5(X3,I2),X2,I2,5(X1,A3),X1,I2,8I1),               00074100
FL8002(A6,R3.0,R2.0,R3.0,R6.0),                                         00074200
FL8777(///" VARIABLE NUMBER SPECIFIED ON TRANSGENERATION CARD ",I4,     00074300
   " EXCEEDS P+Q"),                                                     00074400
               FLL600(I3,X2,R20.8),                             %9/69 MC00074500
FL8775(///" ILLEGAL TRANSGENERATION CODE SPECIFIED"),                   00074600
FL8004(12A6),                                                           00074700
FL8005(/////X4,"VARIABLE",X8,"MEAN",X7,"STANDARD DEVIATION"),           00074800
FL8006(X3,A6,I3,X2,F14.5,X4,F14.5),                                     00074900
FL8007(X6,I2,X6,F14.5,X4,F14.5),                                        00075000
FL8008(// "  COVARIANCE MATRIX"/),                                      00075100
FL8009(//"  CORRELATION MATRIX"/),                                      00075200
FL8010(A6,X2,I2,X2,I3,3(X2,F8.5),X3,I2,3(X2,A3)),                       00075300
FL8034("  ","SUB-PROBLM",I5/X3,"DEPENDENT VARIABLE",X11,I2/X3,          00075400
   "MAXIMUM NUMBER OF STEPS",X6,I2/X3,"F-LEVEL FOR INCLUSION",X4,F8.6/  00075500
   X3,"F-LEVEL FOR DELETION",X5,F8.6/X3,"TOLERANCE LEVEL",X10,F8.6),    00075600
FL8011(A6,66I1 )  ,                                                     00075700
FL9118(//" F-LEVEL INSUFFICIENT FOR FURTHER COMPUTATION"///),           00075800
FL8012(////X4,"STEP NUMBER",X2,I3/X4,"VARIABLE ",A6,"D ",I4//X4,        00075900
   "MULTIPLE R",X12,F9.4/X4,"STD. ERROR OF EST.",F13.4//X4,             00076000
   "ANALYSIS OF VARIANCE"/X27,"DF",X4,"SUM OF SQUARES",X4,"MEAN SQUARE",00076100
X4,"F RATIO"/X12,"REGRESSION",X3,I4,F16.3,F16.3,F12.3/X12,"RESIDUAL",X5,00076200
I4,F16.3,R16.3),                                                        00076300
FL8013(/X57,"."/X21,"VARIABLES IN EQUATION",X15,".",X19,                00076400
   "VARIABLES NOT IN EQUATION"/X57,"."/X6,"VARIABLE",X6,"COEFFICIENT",  00076500
   X2,"STD. ERROR",X2,"F TO REMOVE .",X5,"VARIABLE",X4,"PARTIAL CORR.", 00076600
   X5,"TOLERANCE",X4,"F TO ENTER"/X57,"."),                             00076700
FL8050(X57,"."/X6,"(CONSTANT",X5,F11.5," )",X24,"."),                   00076800
FL8051(X57,"."/X10,"(CONSTANT",X1,F11.5," )",X24,"."),                  00076900
FL8014(X5,A6,X1,I2,X1,F16.5,X1,F11.5,X1,F11.4,X2,".",X4,A6,X1,I2,X1,    00077000
   F15.5,X1,F13.4,X1,F12.4),                                            00077100
FL8015(X9,I2,X1,F19.5,X1,F11.5,X1,F11.4,"  .",X8,I2,X1,F18.5,X1,F13.4,  00077200
   X1,F12.4),                                                           00077300
FL8016(X5,A6,X1,I2,X1,F16.5,X1,F11.5,X1,F11.4,X2,"."),                  00077400
FL8017(X9,I2,X1,F19.5,X1,F11.5,X1,F11.4,"  ."),                         00077500
FL8018(X57,".",X4,A6,X1,I2,X1,F15.5,X1,F13.4,X1,F12.4),                 00077600
FL8019(X57,".",X8,I2,X1,F18.5,X1,F13.4,X1,F12.4),                       00077700
FL8036(" SPECIFIED STEP REACHED"),                                      00077800
FL9632(/////" SUMMARY TABLE OMITTED DUE TO LACK OF INFORMATION"),       00077900
FL9602("  SUMMARY TABLE"//X2,"STEP",X16,"VARIABLE",X10,"MULTIPLE",X17,  00078000
   "INCREASE",X10,"F VALUE TO",X5,"NUMBER OF INDEPENDENT"/X1,"NUMBER",  00078100
   X10,"ENTERED",X2,"REMOVED",X5,"R",X9 ,"RSQ",X15,"IN RSQ",X9,         00078200
   "ENTER OR REMOVE",X4,"VARIABLES INCLUDED"//),                        00078300
     FL9603(X2,I2,X15,I2,X9,F11.4,F12.4,X7,F12.4,X4,F16.4,X16,I2),      00078400
     FL9604(X2,I2,X24,I2,   F11.4,F12.4,X7,F12.4,X4,F16.4,X16,I2),      00078500
     FL9631(X3,I2,X9,A6,X1,I2,X10,F9.4,F12.4,X5,F14.4,F19.4,X15,I2) ,   00078600
     FL9612(X3,I2,X20,A6,X1,I2,F9.4,F13.4,X6,F14.4,X1,F19.4,X15,I2),    00078700
FL8032(A6,33I2),                                                        00078800
FL8031("  LIST OF RESIDUALS"///),                                       00078900
               FL9904(X2,I6,X6,3(F12.5,X5)),                            00079000
               FLL9904(X2,I6,X6,4(R12.5,X5)),                           00079100
          FL9902(X25"VALUES"/,X5,"CASE",X9,"ACTUAL",X9,"PREDICTED",     00079200
                 X9,"RESIDUAL"/),                                       00079300
          FL9903(5(I6,F15.5,X2))  ,                                     00079400
          FL9901(5(X3,"CASE",X6,"RESIDUAL",X2)) ,                       00079500
          FL9950(" PLOT OF RESIDUALS (Y-AXIS)",X35,".."/X3,"VS. VARIAB" 00079600
               ,"LE ",I3,  " (X-AXIS)",X34,"..") ,                      00079700
FL9960("(2X,",I1,"(1X,13HVS. VARIABLE ,I3, 9H (X-AXIS),34X,2H..,3X))"), 00079800
FL9961(" "),                                                            00079900
          FL9955(X2,6F10.3,".."/X7,F10.3,4F10.3,X5,"..")  ,             00080000
          FL9956(4(X8,51A1,X3,".."))  ,                                 00080100
FL9951(I1),                                                             00080200
FL9952(A1),                                                             00080300
          FL9953(   F7.2," .",50A1,X3,"..")  ,                          00080400
          FL9954(X8,".",50A1,X3,"..")  ,                                00080500
FL8020(///" FINISH CARD ENCOUNTERED"),                                  00080600
FL8021(" PROGRAM TERMINATED"),                                          00080700
FL8022(" NEITHER PROBLM NOR FINISH CARD ENCOUNTERED"),                  00080800
FL8023(/" NUMBER OF VARIABLES, P OR P+Q, OUTSIDE OF LIMITS."),          00080900
FL8024(" CARD INCORRECTLY PUNCHED OR MISSING. OFFENDING CARD FOLLOWS."  00081000
               //),                                                     00081100
FL8026(/" TRANSGENERATION"),                                            00081200
FL8029(/" SUB-PROBLEM"),                                                00081300
FL8030(/" CONTROL-DELETE"),                                             00081400
FL8033(/" INDEX-PLOT");                                                 00081500
FORMAT        OUTT(2R20.8);                                             00081600
LIST LIST1(JXMAN,JCODE,JN,JNOV,JNTGC,JNVA,JNAIT,JNSPC,JNLV,JSDAM,JCOVP, 00081700
JCORP,JZEROI,JWIND,JNVFC,CHOOSE,DATAPRINT,OMITREGRESS);                 00081800
LIST LIST2(JCODE,JN,JNOV,JNVA,JIP,JNSPC);                               00081900
LIST LIST3(JYMAN,FOR DX1~1 STEP 1 UNTIL 3 DO SVKTRANS[(K~(3|JI+DX1-3)). 00082000
[33:6],K.[39:9]],TRANS[JI]);                                            00082100
LIST LIST4(JI);                                                         00082200
LIST LIST5(FOR DX1~1 STEP 1 UNTIL JNVFC DO SVRES[(K~(DX1)).[34:5],K.    00082300
 [39:9]]);                                                              00082400
LIST LIST6(FOR DX1~1 STEP 1 UNTIL JNOV DO SVIN[DX1]);                   00082500
LIST LIST7(FOR DX1~1 STEP 1 UNTIL 255 DO SVPPP[DX1]);                   00082600
LIST LIST8(FOR DX1~1 STEP 1 UNTIL JLLL DO SVPPP[DX1]);                  00082700
LIST LIST9(SVALBEL[JI],JI,SVXMEAN[JI],SVSTDEV[(K~(JI+160)).[33:6],K.    00082800
 [39:9]]);                                                              00082900
LIST LIST10(JI,SVXMEAN[JI],SVSTDEV[(K~(JI+160)).[33:6],K.[39:9]]);      00083000
LIST LIST11(JWMAN,JKDEP,JMAXSTP,JFINC,JFOUT,JTOL,JNVIP,JCDF,JRESID,     00083100
 JSUMTAB);                                                              00083200
LIST LIST12(JM,JKDEP,JMAXSTP,JFINC,JFOUT,JTOL);                         00083300
LIST LIST13(JVMAN,FOR DX1~1 STEP 1 UNTIL JIP DO SVC[DX1]);              00083400
LIST LIST14(JVMAN,FOR DX1~1 STEP 1 UNTIL 66 DO SVC[DX1],JDUM,FOR DX1~67 00083500
 STEP 1 UNTIL JIP DO SVC[DX1]);                                         00083600
LIST LIST15(JL,JENTER,JKAY,JXMULTR,JSTERR,JIDF,JREGSS,JREGMS,JFRATIO,   00083700
 JIRDF,JRESSS,JRESMS);                                                  00083800
LIST LIST16(JL,JKAY,JFLAG,JXMULTR,JFKAY,JNVI);                          00083900
LIST LIST17(JALPHA);                                                    00084000
LIST LIST18(SVANAME[JI],SVINEN[JI],SVB[JI],SVD[JI],SVF[JI],SVBNAME[     00084100
 JI],SVNIEN[JI],SVR[JI],SVTOLEV[JI],SVFE[JI]);                          00084200
LIST LIST19(SVINEN[JI],SVB[JI],SVD[JI],SVF[JI],SVNIEN[JI],SVR[JI],      00084300
 SVTOLEV[JI],SVFE[JI]);                                                 00084400
LIST LIST20(SVANAME[JII],SVINEN[JII],SVB[JII],SVD[JII],SVF[JII]);       00084500
LIST LIST21(SVINEN[JII],SVB[JII],SVD[JII],SVF[JII]);                    00084600
LIST LIST22(SVBNAME[JII],SVNIEN[JII],SVR[JII],SVTOLEV[JII],SVFE[JII]);  00084700
LIST LIST23(SVNIEN[JII],SVR[JII],SVTOLEV[JII],SVFE[JII]);               00084800
LIST LIST24(JLMN,JKAY,JFLAG,JXMULTR,JFKAY,JNVI);                        00084900
LIST LIST25(JLMN,JKAY,JXMULTR,JRSQ,JRSQI,JFKAY,JNVI);                   00085000
LIST LIST26(JLMN,SVALBEL[JKAY],JKAY,JXMULTR,JRSQ,JRSQI,JFKAY,JNVI);     00085100
LIST LIST27(JWMAN,FOR DX1~1 STEP 1 UNTIL JNVIP DO SVIVPT[DX1]);         00085200
LIST LIST28(FOR DX1~1 STEP 1 UNTIL JLLLL DO SVPPP[DX1]);                00085300
LIST LIST29(JIJK);                                                      00085400
LIST LIST30(JI,SVX[JKDEP],PREDIC[JI],SVRES[(K~(JI)).[34:5],K.[39:9]]);  00085500
LIST LIST31(FOR DX1~1 STEP 1 UNTIL JIJK DO [SVNN[DX1],SVBES[DX1]]);     00085600
LIST LIST32(JKKK);                                                      00085700
LIST LIST33(FOR INDEX1~1 STEP 1 UNTIL 20 DO SVFMT1[INDEX1]);            00085800
LIST LIST34(JII,JJJ);                                                   00085900
LIST LIST35(JII);                                                       00086000
LIST LIST36(JKKK,JKKK);                                                 00086100
LIST LIST37(FOR DX1~1 STEP 2 UNTIL 11 DO SVPINT[DX1],FOR DX1~2 STEP 2   00086200
 UNTIL 11 DO SVPINT[DX1]);                                              00086300
LIST LIST38(FOR DX1~1 STEP 2 UNTIL 11 DO SVPINT[DX1],FOR DX1~1 STEP 2   00086400
 UNTIL 11 DO SVQINT[DX1],FOR DX1~2 STEP 2 UNTIL 11 DO SVPINT[DX1],FOR   00086500
 DX1~2 STEP 2 UNTIL 11 DO SVQINT[DX1]);                                 00086600
LIST LIST39(FOR DX1~1 STEP 1 UNTIL JMNMN DO JDOT);                      00086700
LIST LIST40(SVKP[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]);                   00086800
LIST LIST41(SVP[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]);                    00086900
LIST LIST42(SVIQ[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]);                   00087000
LIST LIST43(SVQ[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]);                    00087100
LIST LIST44(SVCOL[JKLN],FOR DX1~1 STEP 1 UNTIL 50 DO SVP[(K~(50|DX1+    00087200
 JK-50)).[36:3],K.[39:9]],SVCOL[JKLN],FOR DX1~1 STEP 1 UNTIL 50 DO SVQ[ 00087300
 (K~(50|DX1+JK-50)).[36:3],K.[39:9]]);                                  00087400
LIST LIST45(SVCOL[JKLN],FOR DX1~1 STEP 1 UNTIL 50 DO SVP[(K~(50|DX1+    00087500
 JK-50)).[36:3],K.[39:9]]);                                             00087600
LIST LIST46(FOR DX1~1 STEP 1 UNTIL 50 DO SVP[(K~(50|DX1+JK-50)).[36:3], 00087700
 K.[39:9]]);                                                            00087800
LIST LIST47(FOR DX1~1 STEP 1 UNTIL 50 DO SVP[(K~(50|DX1+JK-50)).[36:3], 00087900
 K.[39:9]],FOR DX1~1 STEP 1 UNTIL 50 DO SVQ[(K~(50|DX1+JK-50)).[36:3],  00088000
 K.[39:9]]);                                                            00088100
LIST LIST48(JKKK,JKKK,JKKK,JKKK);                                       00088200
SWITCH FORMAT SW ~                                                      00088300
      (A6,R3.0,R2.0,R3.0,R6.0),                                         00088400
      (A6,X2,I2,X2,I3,3(X2,F8.5),X3,I2,3(X2,A3)),                       00088500
      (A6,66I1 )  ,                                                     00088600
      (A6,33I2),                                                        00088700
      (A6,X2,I2,X2,I3,3(X2,F8.5),X3,I2,3(X2,A3))                        00088800
                                                                       ;00088900
SWITCH LIST SL ~ LIST3, LIST11, LIST14, LIST27, LIST11                 ;00089000
BEGIN                                                                   00089100
LABEL L1,L7,L7778,L9777,L9775,L1000,L108,L1120,L9,L10,L4300,L50001,     00089200
 L50003,L17,L29,L32,L331,L34,L352,L15000,L39,L391,L392,L393,L41,L42,    00089300
 L43,L16001,L16004,L16000,L471,L473,L44,L51,L52,L46,L54,L48,L57,L55,    00089400
 L117,L9622,L23312,L9611,L23314,L23310,L9605,L75,L51000,L25000,L81,     00089500
 L20200,L78,L24000,L72,L74,L73,L98,L99,L101,L20500,L61000,L20700,       00089600
 L116,L111,L9614,L9615,L9403,L83,L84,L86,L88,L89,L20900,L94,L9551,      00089700
 L95,L96,L9662,L20800,L114,L20401,L13000,L9999,L22100,L22200,L9003,     00089800
 L9004,L9009,L9011;                                                     00089900
LABEL          L1118 ;                                                  00090000
LABEL          L9002,L1119,L121,L19,L9010,L241                       ;  00090100
LABEL          L9001,L3,L9006,L122,L18,L242,L36,L443,L80001            ;00090200
LABEL          L2,L7777,L8,L11,L14,L15,L23,L824,L27,L35,L361,L442,      00090300
                  L9606,L8888,L76,L90                           ;       00090400
LABEL          LL1,LL2 ;                                                00090500
LABEL          DATAERROR;                                               00090600
          BEGIN                                                         00090700
REAL           ERSATZ  ;                                                00090800
               ERSATZ ~ 1.0 ;                                           00090900
JPROBLM~(+"PROBLM");                                                    00091000
JSUBPRO~(+"SUBPRO");                                                    00091100
JTRNGEN~(+"TRNGEN");                                                    00091200
JCONDEL~(+"CONDEL");                                                    00091300
JYES~(+"YES");                                                          00091400
JDXPLTS~(+"IDXPLT");                                                    00091500
JXNO~(+" NO");                                                          00091600
JFINISH~(+"FINISH");                                                    00091700
                                                                        00091800
REWIND(TAPE2);                                                          00091900
JDOT~(+".");                                                            00092000
SVNUSE[1]~1;                                                            00092100
JI~1;                                                                   00092200
          END;                                                          00092300
DO BEGIN                                                                00092400
  SVNUSE[JI]~0  END UNTIL (JI~(JI+1))>20;                               00092500
COMMENT READ PROBLEM CARD;                                              00092600
L1: READ(TAPE,FL8001,LIST1)[FINIS];                                     00092700
COMMENT CHECK PROBLEM CARD FOR VALID PARAMETERS;                        00092800
               IF JXMAN = JFINISH THEN GO TO L9001 ELSE GO TO L2 ;      00092900
COMMENT "B" CARD 2 IF(XMAN*(-PROBLM)+(-XMAN)*PROBLM) 9002,3,9002;       00093000
L2:            IF JXMAN = JPROBLM  THEN GO TO L3 ELSE GO TO L9002 ;     00093100
L3:                                                                     00093200
JIP~INT(JNOV+JNVA);                                                     00093300
IF NOT PAGEIT THEN PAGEIT~TRUE ELSE WRITE(PRINT[PAGE]);                 00093400
WRITE(PRINT,FL8003,LIST2);                                              00093500
IF ((JNOV-1)|(JNOV-81))}0 THEN GO TO L9003;                             00093600
IF ((JIP-1)|(JIP-81))}0 THEN GO TO L9003;                               00093700
SRVFCHCK(JNVFC);                                                        00093800
IF JNTGC>0 THEN GO TO L7 ELSE GO TO L108;                               00093900
COMMENT READ TRANSGENERATION CARDS;                                     00094000
L7: JI~1;                                                               00094100
DO BEGIN                                                                00094200
     READ (TAPE,FL8002,LIST3)[FINIS];                                   00094300
SVTRANS[JI] ~ TRANS[JI];                                                00094400
  COMMENT CHECK TRANSGENERATION PARAMETERS;                             00094500
               IF JYMAN = JTRNGEN THEN GO TO L7777 ELSE                 00094600
          BEGIN ERRORIDENT ~ 0; GO TO L9006 END;                        00094700
  COMMENT "B" CARD IF(YMAN*(-TRNGEN)+(-YMAN)*TRNGEN) 9006,7777,9006;    00094800
L7777:                                                                  00094900
  JKIP~JIP;                                                             00095000
  IF JNVA}0 THEN GO TO L7778;                                           00095100
  JKIP~JNOV;                                                            00095200
  L7778: IF (SVKTRANS[(K~(3|JI-2)).[33:6],K.[39:9]]>JKIP) THEN GO TO    00095300
   L9777;                                                               00095400
  IF (SVKTRANS[(K~(3|JI)).[33:6],K.[39:9]]>JKIP) THEN GO TO L9777;      00095500
  IF (SVKTRANS[(K~(3|JI-1)).[33:6],K.[39:9]]>24) THEN GO TO L9775 ELSE  00095600
   GO TO L1000;                                                         00095700
  L9777: WRITE(PRINT,FL8777,LIST4);                                     00095800
  JNAIT~-24;                                                            00095900
  GO TO L1000;                                                          00096000
  L9775: WRITE(PRINT,FL8775);                                           00096100
  JNAIT~-24;                                                            00096200
  L1000: END UNTIL (JI~(JI+1))>JNTGC;                                   00096300
L108: IF (XPR~(JNAIT))>0 THEN GO TO L8;                                 00096400
IF XPR<0 THEN GO TO L9999;                                              00096500
JNAIT~5;                                                                00096600
GO TO L1120;                                                            00096700
L8:            IF JWIND = JXNO THEN GO TO L1119 ELSE GO TO L1118 ;      00096800
COMMENT "B" CARD 8 IF(WIND*(-XNO)+(-WIND)*XNO)1118,1119,1118;           00096900
L1118:                                                                  00097000
COMMENT        REWIND (FILES[JNAIT])  THIS IS TO REWIND THE INPUT TAPE ;00097100
L1119:                                                                  00097200
SVNUSE[JNAIT]~1;                                                        00097300
L1120: IF JNLV>0 THEN GO TO L9 ELSE GO TO L10;                          00097400
COMMENT READ LABEL CARDS;                                               00097500
L9: SRRDLBL2(JNLV,JIP,SVALBEL);                                         00097600
COMMENT READ VARIABLE FORMAT CARDS;                                     00097700
L10: JNVFC~JNVFC|12;                                                    00097800
               OBJECTFMTGEN(FLL5774);                                   00097900
COMMENT INITIALIZE ACCUMULATORS AND MATRIX A;                           00098000
JXN~JN;                                                                 00098100
JI~1;                                                                   00098200
DO BEGIN                                                                00098300
  SVXMEAN[JI]~0;                                                        00098400
  JJ~1;                                                                 00098500
  DO BEGIN                                                              00098600
    SVA[JJ,JI]~0;                                                       00098700
    END UNTIL (JJ~(JJ+1))>JIP  END UNTIL (JI~(JI+1))>JIP;               00098800
JLLL~0;                                                                 00098900
IF FRFIELD THEN                                                         00099000
READ(TAPE,/,FOR JJ~1 STEP 1 UNTIL JN2 DO FOR DX1~1 STEP 1 UNTIL JN1 DO  00099100
              DATA[DX1,JJ])[DATAERROR];                                 00099200
IF DATAPRINT=1 THEN WRITE(PRINT,FLH5774);                               00099300
              JKL~1;                                                    00099400
JK~1;                                                                   00099500
DO BEGIN                                                                00099600
          BEGIN                                                         00099700
REAL           ERSATZ  ;                                                00099800
               ERSATZ ~ 1.0 ;                                           00099900
  JNINCS~INT(JK);                                                       00100000
IF FRFIELD THEN                                                         00100100
              FOR DX1~1 STEP 1 UNTIL JNOV DO SVX[DX1]~DATA[DX1,JKL] ELSE00100200
 READ(TAPE,FLL5774,FOR DX1~1 STEP 1 UNTIL JNOV DO SVX[DX1])[DATAERROR]; 00100300
IF DATAPRINT=1 THEN WRITE (PRINT, FLL5774,FOR DX1~1 STEP 1 UNTIL JNOV DO00100400
              SVX[DX1]);                                                00100500
          END;                                                          00100600
  IF JNTGC{0 THEN GO TO L11;                                            00100700
  SRTRANGN;                                                             00100800
L11:           IF JZEROI = JYES THEN GO TO L122 ELSE GO TO L121 ;       00100900
  COMMENT "B" CARD 11 IF(ZEROI*(-YES)+(-ZEROI)*YES) 121,122,121;        00101000
L122:                                                                   00101100
  JI~1;                                                                 00101200
  DO BEGIN                                                              00101300
    SVXMEAN[JI]~SVXMEAN[JI]+SVX[JI];                                    00101400
    JJ~JI;                                                              00101500
    DO BEGIN                                                            00101600
      SVA[JJ,JI]~SVA[JJ,JI]+SVX[JI]|SVX[JJ];                            00101700
      END UNTIL (JJ~(JJ+1))>JIP  END UNTIL (JI~(JI+1))>JIP;             00101800
  JXM~JN;                                                               00101900
  GO TO L4300;                                                          00102000
L121:                                                                   00102100
  JXK~JK;                                                               00102200
  JI~1;                                                                 00102300
  DO BEGIN                                                              00102400
    SVXMEAN[JI]~SVXMEAN[JI]+SVX[JI];                                    00102500
    END UNTIL (JI~(JI+1))>JIP;                                          00102600
  JI~1;                                                                 00102700
COMMENT  COMPUTING SUMS OF SQUARES AND CROSS PRODUCTS;                  00102800
  DO BEGIN                                                              00102900
    JJ~JI;                                                              00103000
    DO BEGIN                                                            00103100
               SVA[JJ,JI] ~ SVA[JJ,JI] +SVX[JI] | SVX[JJ] ;             00103200
      END UNTIL (JJ~(JJ+1))>JIP  END UNTIL (JI~(JI+1))>JIP;             00103300
  JXM~JN-1;                                                             00103400
  L4300: JL~1;                                                          00103500
              JKL~JKL+1;                                                00103600
WRITE(TAPE2,OKTL,FOR JL  ~ 1 STEP 1 UNTIL JIP DO SVX[JL ]);             00103700
  END UNTIL (JK~(JK+1))>JN;                                             00103800
SVNUSE[2]~1;                                                            00103900
IF JLLL{0 THEN GO TO L50003;                                            00104000
L50003:                                                                 00104100
REWIND(TAPE2);                                                          00104200
COMMENT REPLACE XMEAN WITH MEAN VECTOR, A WITH COVARIANCE MATRIX,AND;   00104300
COMMENT COMPUTE STANDARD DEVIATIONS;                                    00104400
               IF JZEROI = JYES THEN GO TO LL1 ELSE GO TO LL2 ;         00104500
COMMENT COMPUTING RESIDUAL SUMS OF SQUARES AND CROSS PRODUCTS;          00104600
LL2:           FOR JI ~ 1 STEP 1 UNTIL JIP DO                           00104700
               FOR JJ ~ JI STEP 1 UNTIL JIP DO                          00104800
               SVA[JJ,JI] ~ SVA[JJ,JI] - SVXMEAN[JI] | SVXMEAN[JJ]/JXN ;00104900
LL1:                                                                    00105000
JI~1;                                                                   00105100
DO BEGIN                                                                00105200
  SVXMEAN[JI]~SVXMEAN[JI]/JXN;                                          00105300
  JJ~JI;                                                                00105400
  DO BEGIN                                                              00105500
    SVA[JJ,JI]~SVA[JJ,JI]/JXM;                                          00105600
    END UNTIL (JJ~(JJ+1))>JIP  END UNTIL (JI~(JI+1))>JIP;               00105700
JI~1;                                                                   00105800
DO BEGIN                                                                00105900
  SVSTDEV[(K~(JI+160)).[33:6],K.[39:9]]~SQRT(SVA[JI,JI]);               00106000
  END UNTIL (JI~(JI+1))>JIP;                                            00106100
COMMENT WRITE OUT MEANS AND STANDARD DEVIATIONS IF REQUESTED;           00106200
               IF JSDAM = JYES THEN GO TO L14 ELSE GO TO L15 ;          00106300
COMMENT "B" CARD IF(SDAM*(-YES)+(-SDAM)*YES)15,14,15;                   00106400
L14:                                                                    00106500
WRITE(PRINT,FL8005);                                                    00106600
IF JNLV{0 THEN GO TO L17;                                               00106700
JI~1;                                                                   00106800
DO BEGIN                                                                00106900
  WRITE(PRINT,FL8006,LIST9);                                            00107000
  END UNTIL (JI~(JI+1))>JIP;                                            00107100
GO TO L15;                                                              00107200
L17: JI~1;                                                              00107300
DO BEGIN                                                                00107400
  WRITE(PRINT,FL8007,LIST10);                                           00107500
  END UNTIL (JI~(JI+1))>JIP;                                            00107600
COMMENT PRINT COVARIANCE MATRIX IF REQUESTED;                           00107700
L15:           IF JCOVP = JYES THEN GO TO L18 ELSE GO TO L19 ;          00107800
COMMENT "B" CARD 15 IF(COVP*(-YES)+(-COVP)*YES) 19,18,19;               00107900
L18:                                                                    00108000
WRITE(PRINT,FL8008);                                                    00108100
SRAOUT;                                                                 00108200
COMMENT REPLACE UPPER DIAGONAL SECTION OF MATRIX WITH CORRELATION MATRIX00108300
 ;                                                                      00108400
L19:                                                                    00108500
JI~1;                                                                   00108600
DO BEGIN                                                                00108700
  JJ~JI;                                                                00108800
  DO BEGIN                                                              00108900
    SVA[JJ,JI]~SVA[JJ,JI]/(SVSTDEV[(K~(JI+160)).[33:6],K.[39:9]]|       00109000
     SVSTDEV[(K~(JJ+160)).[33:6],K.[39:9]]);                            00109100
    SVA[JI,JJ]~SVA[JJ,JI];                                              00109200
    END UNTIL (JJ~(JJ+1))>JIP  END UNTIL (JI~(JI+1))>JIP;               00109300
COMMENT PRINT CORRELATION MATRIX IF REQUESTED;                          00109400
               IF JCORP = JYES THEN GO TO L23 ELSE GO TO L824 ;         00109500
COMMENT "B" CARD IF(CORP*(-YES)+(-CORP)*YES) 824,23,824;                00109600
L23:                                                                    00109700
WRITE(PRINT,FL8009);                                                    00109800
SRAOUT;                                                                 00109900
L824:          IF JZEROI = JYES THEN GO TO L242 ELSE GO TO L241 ;       00110000
COMMENT "B" CARD 824 IF(ZEROI*(-YES)+(-ZEROI)*YES) 241,242,241;         00110100
L241:                                                                   00110200
JXN~JXN-1;                                                              00110300
L242:                                                                   00110400
JM~1;                                                                   00110500
DO BEGIN                                                                00110600
  COMMENT RESTORE THE CORRELATION MATRIX;                               00110700
  JI~1;                                                                 00110800
  DO BEGIN                                                              00110900
    SVA[JI,JI]~1;                                                       00111000
    JK~JI+1;                                                            00111100
    JJ~JK;                                                              00111200
    DO BEGIN                                                            00111300
      SVA[JJ,JI]~SVA[JI,JJ];                                            00111400
      END UNTIL (JJ~(JJ+1))>JIP  END UNTIL (JI~(JI+1))>JIP;             00111500
  COMMENT READ SUB PROBLEM CARD;                                        00111600
     READ (TAPE,FL8010,LIST11)[FINIS];                                  00111700
               IF JWMAN = JSUBPRO THEN GO TO L27 ELSE                   00111800
               BEGIN ERRORIDENT ~ 1; GO TO L9009 END;                   00111900
  COMMENT "B" CARD IF(WMAN*(-SUBPRO)+(-WMAN)*SUBPRO) 9009,27,9009;      00112000
L27:                                                                    00112100
               IF JNVIP > 33 THEN                                       00112200
          BEGIN REGRESSCHECK ~ 3;                                       00112300
               ERRORIDENT~4;       GO TO L9009 END;                     00112400
  IF JFINC>0 THEN GO TO L29;                                            00112500
  JFINC~0.01;                                                           00112600
  L29: IF JFOUT>0 THEN GO TO L32;                                       00112700
  JFOUT~0.005;                                                          00112800
  L32: IF JTOL>0 THEN GO TO L331;                                       00112900
  JTOL~0.001;                                                           00113000
  L331: IF JMAXSTP>0 THEN GO TO L34;                                    00113100
  JMAXSTP~JIP|2;                                                        00113200
  L34: JI~1;                                                            00113300
  DO BEGIN                                                              00113400
    SVC[JI]~0;                                                          00113500
    END UNTIL (JI~(JI+1))>JIP;                                          00113600
               WRITE(PRINT[PAGE]);                                      00113700
  WRITE(PRINT,FL8034,LIST12);                                           00113800
               IF JCDF = JYES THEN GO TO L35 ELSE GO TO L36 ;           00113900
  COMMENT "B" CARD IF(CDF*(-YES)+(-CDF)*YES) 36,35,36;                  00114000
L35:                                                                    00114100
  IF (JIP>66) THEN GO TO L352;                                          00114200
     READ (TAPE,FL8011,LIST13)[FINIS];                                  00114300
  GO TO L361;                                                           00114400
   L352:  READ(TAPE,FL8011,LIST14)[FINIS];                              00114500
L361:          IF JVMAN = JCONDEL THEN GO TO L36 ELSE                   00114600
          BEGIN ERRORIDENT ~ 2; GO TO L9010 END;                        00114700
  COMMENT "B" CARD 361 IF(VMAN*(-CONDEL)+(-VMAN)*CONDEL)9010,36,9010;   00114800
L36:                                                                    00114900
  JI~1;                                                                 00115000
  DO BEGIN                                                              00115100
    IF (SVC[JI])>0 THEN GO TO L15000;                                   00115200
    SVC[JI]~2;                                                          00115300
    L15000: END UNTIL (JI~(JI+1))>JIP;                                  00115400
  SVC[JKDEP]~1;                                                         00115500
  JDF~0;                                                                00115600
  JLKL~0;                                                               00115700
  JL~0;                                                                 00115800
  L39: JL~JL+1;                                                         00115900
  COMMENT CALL SUBROUTINE TO ENTER VARIABLE,CALCULATE VALUES TO BE PRINT00116000
   ED;                                                                  00116100
  SRSTEPRG(SVC)  ;                                                      00116200
  IF (XPR~(JFLAG))>0 THEN GO TO L392;                                   00116300
  IF XPR<0 THEN GO TO L391;                                             00116400
  WRITE(PRINT,FL9118);                                                  00116500
  GO TO L117;                                                           00116600
  L391: JENTER~(+"REMOVE");                                             00116700
  GO TO L393;                                                           00116800
  L392: JENTER~(+"ENTERE");                                             00116900
  L393: JRESDF~JXN-JDF;                                                 00117000
          BEGIN                                                         00117100
REAL           ERSATZ  ;                                                00117200
               JXN ~ JXN ;   JRESDF ~ JRESDF ;                          00117300
               ERSATZ ~ 1.0 ;                                           00117400
  JRESSS~JXN|(SVSTDEV[(K~(JKDEP+160)).[33:6],K.[39:9]]*2)|SVA[JKDEP,    00117500
   JKDEP];                                                              00117600
               IF JRESDF = 0 THEN JRESMS ~ 0 ELSE                       00117700
  JRESMS~JRESSS/JRESDF;                                                 00117800
  JREGDF~JDF;                                                           00117900
  JREGSS~JXN|(SVSTDEV[(K~(JKDEP+160)).[33:6],K.[39:9]]*2)-JRESSS;       00118000
  JREGMS~JREGSS/JREGDF;                                                 00118100
               IF JRESMS = 0 THEN JFRATIO ~ 0 ELSE                      00118200
  JFRATIO~JREGMS/JRESMS;                                                00118300
IF JRESMS < 0 THEN JRESMS ~ 0;                                          00118400
  JSTERR~SQRT(JRESMS);                                                  00118500
  JXMULTR~SQRT(1-SVA[JKDEP,JKDEP]);                                     00118600
  JIDF~JDF;                                                             00118700
  JIRDF~JRESDF;                                                         00118800
  WRITE(PRINT,FL8012,LIST15);                                           00118900
  COMMENT A VARIABLE IS IN THE EQUATION IF C(I) IS LESS THAN OR EQUAL TO00119000
    0.0;                                                                00119100
  JNVI~0;                                                               00119200
  JNVO~0;                                                               00119300
  JALPHA~SVXMEAN[JKDEP];                                                00119400
  JI~1;                                                                 00119500
          END;                                                          00119600
  DO BEGIN                                                              00119700
    IF (JI=JKDEP) THEN GO TO L16000;                                    00119800
    IF (SVC[JI])>0 THEN GO TO L42 ELSE GO TO L41;                       00119900
    COMMENT COMPUTE MULTIPLE REGRESSION EQUATION COEFFICIENTS,STD.ERROR,00120000
     ;                                                                  00120100
    COMMENT AND F TO REMOVE, FOR VARIABLES IN THE REGRESSION;           00120200
    L41: JNVI~JNVI+1;                                                   00120300
    SVB[JNVI]~SVSTDEV[(K~(JKDEP+160)).[33:6],K.[39:9]]|FNAF(JI,JKDEP)/  00120400
     SVSTDEV[(K~(JI+160)).[33:6],K.[39:9]];                             00120500
    SVD[JNVI]~(JSTERR/SVSTDEV[(K~(JI+160)).[33:6],K.[39:9]])|SQRT(-SVA[ 00120600
     JI,JI]/JXN);                                                       00120700
               IF SVD[JNVI]=0.0 THEN SVF[JNVI]~0.0 ELSE                 00120800
    SVF[JNVI]~(SVB[JNVI]/SVD[JNVI])*2;                                  00120900
    JALPHA~JALPHA-SVB[JNVI]|SVXMEAN[JI];                                00121000
    SVANAME[JNVI]~SVALBEL[JI];                                          00121100
    SVINEN[JNVI]~JI;                                                    00121200
    GO TO L16001;                                                       00121300
    COMMENT A VARIABLE IS OUT OF THE REGRESSION IF C(I) IS GREATER THAN 00121400
     OR;                                                                00121500
    COMMENT EQUAL TO 1;                                                 00121600
    L42: IF (SVC[JI]}1) THEN GO TO L43 ELSE GO TO L16000;               00121700
    COMMENT COMPUTE PARTIAL CORRELATION COEFFICIENTS, TOLERANCE, AND;   00121800
    COMMENT F TO ENTER FOR VARIABLES OUT OF THE REGRESSION;             00121900
    L43: JNVO~JNVO+1;                                                   00122000
    SVBNAME[JNVO]~SVALBEL[JI];                                          00122100
    SVNIEN[JNVO]~JI;                                                    00122200
    SVTOLEV[JNVO]~SVA[JI,JI];                                           00122300
    SVR[JNVO]~FNAF(JI,JKDEP)/SQRT(SVA[JI,JI]|SVA[JKDEP,JKDEP]);         00122400
    SVFE[JNVO]~((FNAF(JI,JKDEP)*2)|(JRESDF-1))/(SVA[JI,JI]|SVA[JKDEP,   00122500
     JKDEP]-(FNAF(JI,JKDEP)*2));                                        00122600
    L16001: IF (JI!JKAY) THEN GO TO L16000;                             00122700
    IF (SVC[JI])>0 THEN GO TO L16004;                                   00122800
    JFKAY~SVF[JNVI];                                                    00122900
    GO TO L16000;                                                       00123000
    L16004: JFKAY~SVFE[JNVO];                                           00123100
    L16000: END UNTIL (JI~(JI+1))>JIP;                                  00123200
               IF JZEROI = JYES THEN GO TO L442 ELSE GO TO L443 ;       00123300
  COMMENT "B" CARD IF(ZEROI*(-YES)+(-ZEROI)*YES) 443,442,443;           00123400
L442:                                                                   00123500
  JALPHA~0;                                                             00123600
  COMMENT WRITE HEADING FOR COEFFICIENTS;                               00123700
L443:                                                                   00123800
  WRITE(PRINT,FL8013);                                                  00123900
     A[JLKL,0] ~JL;                                                     00124000
     A[JLKL,1] ~JKAY;                                                   00124100
     A[JLKL,2] ~JFLAG;                                                  00124200
     A[JLKL,3] ~JXMULTR;                                                00124300
     A[JLKL,4] ~JFKAY;                                                  00124400
     A[JLKL,5] ~JNVI;                                                   00124500
  JLKL~JLKL+1;                                                          00124600
  COMMENT PRINT THE REGRESSION ANALYSIS TABLE;                          00124700
  IF JNLV{0 THEN GO TO L471;                                            00124800
  WRITE(PRINT,FL8050,LIST17);                                           00124900
  GO TO L473;                                                           00125000
  L471: WRITE(PRINT,FL8051,LIST17);                                     00125100
  L473: JNGO~0;                                                         00125200
  L44: IF JNVO{0 THEN GO TO L46;                                        00125300
  IF JNVI{0 THEN GO TO L48;                                             00125400
  JLNV~MIN(JNVI,JNVO);                                                  00125500
  COMMENT NVO AND NVI BOTH POSITIVE,PRINT BOTH SIDES OF TABLE;          00125600
               IF OMITREGRESS = 1 THEN GO TO L46;                       00125700
  IF JNLV{0 THEN GO TO L51;                                             00125800
  JI~1;                                                                 00125900
  DO BEGIN                                                              00126000
    WRITE(PRINT,FL8014,LIST18);                                         00126100
    END UNTIL (JI~(JI+1))>JLNV;                                         00126200
  GO TO L52;                                                            00126300
  L51: JI~1;                                                            00126400
  DO BEGIN                                                              00126500
    WRITE(PRINT,FL8015,LIST19);                                         00126600
    END UNTIL (JI~(JI+1))>JLNV;                                         00126700
  L52: JNVI~JNVI-JLNV;                                                  00126800
  JNVO~JNVO-JLNV;                                                       00126900
  JNGO~JLNV;                                                            00127000
  GO TO L44;                                                            00127100
  COMMENT NVO ZERO, PRINT LEFT SIDE ONLY;                               00127200
  L46: IF JNVI{0 THEN GO TO L55;                                        00127300
  IF JNLV{0 THEN GO TO L54;                                             00127400
  JI~1;                                                                 00127500
  DO BEGIN                                                              00127600
    JII~JI+JNGO;                                                        00127700
    WRITE(PRINT,FL8016,LIST20);                                         00127800
    END UNTIL (JI~(JI+1))>JNVI;                                         00127900
  GO TO L55;                                                            00128000
  L54: JI~1;                                                            00128100
  DO BEGIN                                                              00128200
    JII~JI+JNGO;                                                        00128300
    WRITE(PRINT,FL8017,LIST21);                                         00128400
    END UNTIL (JI~(JI+1))>JNVI;                                         00128500
  GO TO L55;                                                            00128600
  COMMENT NVI ZERO,PRINT RIGHT SIDE ONLY;                               00128700
  L48: IF JNLV{0 THEN GO TO L57;                                        00128800
  JI~1;                                                                 00128900
  DO BEGIN                                                              00129000
    JII~JI+JNGO;                                                        00129100
    WRITE(PRINT,FL8018,LIST22);                                         00129200
    END UNTIL (JI~(JI+1))>JNVO;                                         00129300
  GO TO L55;                                                            00129400
  L57: JI~1;                                                            00129500
  DO BEGIN                                                              00129600
    JII~JI+JNGO;                                                        00129700
    WRITE(PRINT,FL8019,LIST23);                                         00129800
    END UNTIL (JI~(JI+1))>JNVO;                                         00129900
  L55: IF (JL<JMAXSTP) THEN GO TO L39;                                  00130000
  WRITE(PRINT,FL8036);                                                  00130100
L117:                                                                   00130200
                                                                        00130300
               IF JSUMTAB = JYES THEN GO TO L9606 ELSE GO TO L9605 ;    00130400
  COMMENT "B" CARD IF(SUMTAB*(-YES)+(-SUMTAB)*YES) 9605,9606,9605;      00130500
L9606:                                                                  00130600
  IF JLKL>0 THEN GO TO L9622;                                           00130700
  WRITE(PRINT,FL9632);                                                  00130800
  GO TO L9605;                                                          00130900
  L9622: WRITE(PRINT,FL9602);                                           00131000
  JR1SQ~0;                                                              00131100
  JI~1;                                                                 00131200
  DO BEGIN                                                              00131300
     KI ~ JI - 1;                                                       00131400
JLMN     ~ A[KI,0];                                                     00131500
JKAY     ~ A[KI,1];                                                     00131600
JFLAG    ~ A[KI,2];                                                     00131700
JXMULTR  ~ A[KI,3];                                                     00131800
JFKAY    ~ A[KI,4];                                                     00131900
JNVI     ~ A[KI,5];                                                     00132000
    JRSQ~JXMULTR*2;                                                     00132100
    JRSQI~JRSQ-JR1SQ;                                                   00132200
    JR1SQ~JRSQ;                                                         00132300
    IF JNLV>0 THEN GO TO L9611;                                         00132400
    IF (XPR~(JFLAG))=0 THEN GO TO L23310;                               00132500
    IF XPR<0 THEN GO TO L23312;                                         00132600
    WRITE(PRINT,FL9603,LIST25);                                         00132700
    GO TO L23310;                                                       00132800
    L23312: WRITE(PRINT,FL9604,LIST25);                                 00132900
    GO TO L23310;                                                       00133000
    L9611: IF (XPR~(JFLAG))=0 THEN GO TO L23310;                        00133100
    IF XPR<0 THEN GO TO L23314;                                         00133200
    WRITE(PRINT,FL9631,LIST26);                                         00133300
    GO TO L23310;                                                       00133400
    L23314: WRITE(PRINT,FL9612,LIST26);                                 00133500
    L23310: END UNTIL (JI~(JI+1))>JLKL;                                 00133600
L9605:                                                                  00133700
  IF JNVIP>0 THEN GO TO L75;                                            00133800
L8888:         IF JRESID = JYES THEN GO TO L80001 ELSE GO TO L13000 ;   00133900
  COMMENT "B" CARD 8888 IF(RESID*(-YES)+(-RESID)*YES)13000,80001,13000; 00134000
  COMMENT READ INXPLT CARD;                                             00134100
L75:  READ (TAPE,FL8032,LIST27)[FINIS];                                 00134200
               IF JWMAN = JDXPLTS THEN GO TO L76 ELSE                   00134300
          BEGIN ERRORIDENT ~ 3; GO TO L9011 END;                        00134400
  COMMENT "B" CARD IF(WMAN*(-DXPLTS)+(-WMAN)*DXPLTS) 9011,76,9011;      00134500
L76:                                                                    00134600
  JK~1;                                                                 00134700
  DO BEGIN                                                              00134800
    IF (SVIVPT[JK]>JIP) THEN                                            00134900
          BEGIN                                                         00135000
ERRORIDENT ~ 3; REGRESSCHECK ~ 4; GO TO L9011 END;                      00135100
    END UNTIL (JK~(JK+1))>JNVIP;                                        00135200
L80001:                                                                 00135300
  JJ~1;                                                                 00135400
  DO BEGIN                                                              00135500
    SVXMIN[(K~(JJ)).[33:6],K.[39:9]]~999999.9;                          00135600
    SVXMAX[(K~(JJ+80)).[33:6],K.[39:9]]~-999999.9;                      00135700
    END UNTIL (JJ~(JJ+1))>JNVIP;                                        00135800
  JRMIN~999999.9;                                                       00135900
  JRMAX~-999999.9;                                                      00136000
  COMMENT COMPUTE RESIDUALS;                                            00136100
  JLLLL~0;                                                              00136200
  JLLLLL~JLLLL;                                                         00136300
  JI~1;                                                                 00136400
  DO BEGIN                                                              00136500
    JJ~1;                                                               00136600
READ (TAPE2,OKTL,FOR DX1 ~ 1 STEP 1 UNTIL JIP DO SVX[DX1]);             00136700
COMMENT NEXT WRITE FOR DIAGNOSIS OF DISK FILE TROUBLE                   00136800
    WRITE(PRINT,FLL600,FOR DX1~1 STEP 1 UNTIL JIP DO[DX1,SVX[DX1]]      00136900
);                                                                      00137000
    JSUMB~0;                                                            00137100
    JNVI~0;                                                             00137200
    JJ~1;                                                               00137300
    DO BEGIN                                                            00137400
      IF (SVC[JJ])>0 THEN GO TO L25000;                                 00137500
      JNVI~JNVI+1;                                                      00137600
      JSUMB~JSUMB+SVB[JNVI]|SVX[JJ];                                    00137700
      L25000: END UNTIL (JJ~(JJ+1))>JIP;                                00137800
    JL~1;                                                               00137900
    DO BEGIN                                                            00138000
      JMM~SVIVPT[JL];                                                   00138100
      IF (SVXMIN[(K~(JL)).[33:6],K.[39:9]]{SVX[JMM]) THEN GO TO L81;    00138200
      SVXMIN[(K~(JL)).[33:6],K.[39:9]]~SVX[JMM];                        00138300
      L81: IF (SVXMAX[(K~(JL+80)).[33:6],K.[39:9]]}SVX[JMM]) THEN GO TO 00138400
       L20200;                                                          00138500
      SVXMAX[(K~(JL+80)).[33:6],K.[39:9]]~SVX[JMM];                     00138600
      L20200: END UNTIL (JL~(JL+1))>JNVIP;                              00138700
    SVRES[(K~(JI)).[34:5],K.[39:9]]~SVX[JKDEP]-JALPHA-JSUMB;            00138800
COMMENT NEXT STATEMENT FOR DIAGNOSIS IF DEPENDENT VARIABLES ARE EQUAL   00138900
IF REGRESSCHECK = SVX[JKDEP] THEN                                       00139000
WRITE(PRINT,OUTT,REGRESSCHECK,SVX[JKDEP]);                              00139100
REGRESSCHECK ~ SVX[JKDEP];                                              00139200
              PREDIC[JI]~JALPHA+JSUMB;                                  00139300
COMMENT DIAGNOSING TROUBLE WITH RESIDUAL TABLE                          00139400
WRITE(PRINT,FLL9904,JI,SVRES[K.[34:5],K.[39:9]],SVX[JKDEP],JALPHA       00139500
     ,JSUMB);                                                           00139600
    IF (JRMIN{SVRES[(K~(JI)).[34:5],K.[39:9]]) THEN GO TO L78;          00139700
    JRMIN~SVRES[(K~(JI)).[34:5],K.[39:9]];                              00139800
    L78: IF (JRMAX}SVRES[(K~(JI)).[34:5],K.[39:9]]) THEN GO TO L24000;  00139900
    JRMAX~SVRES[(K~(JI)).[34:5],K.[39:9]];                              00140000
    L24000: END UNTIL (JI~(JI+1))>JN;                                   00140100
  REWIND(TAPE2);                                                        00140200
               IF JRESID = JYES THEN GO TO L90 ELSE GO TO L73 ;         00140300
  COMMENT "B" CARD IF (RESID*(-YES)+(-RESID)*YES) 73,90,73;             00140400
L90:                                                                    00140500
               WRITE (  PRINT[PAGE])  ;                                 00140600
  WRITE(PRINT,FL8031);                                                  00140700
               IF (JN>48) THEN GO TO L72;              %P11/68          00140800
               WRITE(PRINT,FL9902)  ;                                   00140900
  JI~1;                                                                 00141000
  DO BEGIN                                                              00141100
READ(TAPE2,OKTL,FOR DX1~1 STEP 1 UNTIL JIP DO SVX[DX1]);                00141200
               WRITE (PRINT,FL9904, LIST30 )  ;                         00141300
    END UNTIL (JI~(JI+1))>JN;                                           00141400
REWIND(TAPE2);                                                          00141500
  GO TO L73;                                                            00141600
  L72: JIJK~5;                                                          00141700
               WRITE (PRINT,FL9901 )  ;                                 00141800
  JNO~(JN DIV 5)+1;                                                     00141900
  SVNN[1]~1;                                                            00142000
  JI~2;                                                                 00142100
  DO BEGIN                                                              00142200
    SVNN[JI]~((JI-1)|JNO)+1  END UNTIL (JI~(JI+1))>5;                   00142300
  JLJK~0;                                                               00142400
  JI~1;                                                                 00142500
  DO BEGIN                                                              00142600
    JLJK~JLJK+1;                                                        00142700
    IF (SVNN[5]{JN) THEN GO TO L74;                                     00142800
    JIJK~4;                                                             00142900
    L74: JJ~1;                                                          00143000
    DO BEGIN                                                            00143100
      JK~JLJK+((JJ-1)|JNO);                                             00143200
      SVBES[JJ]~SVRES[(K~(JK)).[34:5],K.[39:9]];                        00143300
      END UNTIL (JJ~(JJ+1))>JIJK;                                       00143400
               WRITE (PRINT,FL9903, LIST31 )  ;                         00143500
    JJ~1;                                                               00143600
    DO BEGIN                                                            00143700
      SVNN[JJ]~SVNN[JJ]+1  END UNTIL (JJ~(JJ+1))>5;                     00143800
    END UNTIL (JI~(JI+5))>JN;                                           00143900
  L73: JSRS~((JRMAX-JRMIN)/49);                                         00144000
  JI~1;                                                                 00144100
  DO BEGIN                                                              00144200
    JAI~1+5|(JI-1);                                                     00144300
    SVCOL[JI]~JRMIN+JSRS|(JAI-1);                                       00144400
    END UNTIL (JI~(JI+1))>11;                                           00144500
  JIIX~0;                                                               00144600
  IF JNVIP{0 THEN GO TO L13000;                                         00144700
          BEGIN                                                         00144800
REAL           ERSATZ  ;                                                00144900
FORMAT ZERO ("XMAX = XMIN");                                            00145000
LABEL          L8888,L118,L98,L99,L102,L101,L104,L20500,L20600,L61001,  00145100
                 L61000,L76000,L109,L20700,L9950,L9960,L110,L116,L111,  00145200
                 L9961,L9613,L9614,L9615,L9401,L9402,L9403,L82,L83,     00145300
                 L85,L84,L9951,L9952,L86,L112,L87,L88,L91,L89,L20900,   00145400
                 L9953,L9954,L93,L94,L9552,L9551,L95,L96,L9661,L9662,   00145500
                 L20800,L9956,L9955,L113,L114,L20401,L20400 ;           00145600
FORMAT         FL9902(20A6),FL5775(20A6),FL5776(20A6)  ;                00145700
  JI~1;                                                                 00145800
  DO BEGIN                                                              00145900
    JII~SVIVPT[JI];                                                     00146000
    IF (JI+1{JNVIP) THEN GO TO L98;                                     00146100
    JKKK~1;                                                             00146200
    GO TO L99;                                                          00146300
    L98: JKKK~1;                                                        00146400
                                                                        00146500
    JJJ~SVIVPT[JI+1];                                                   00146600
                                                                        00146700
    JJ1~JI+1;                                                           00146800
    L99: JSPI~((SVXMAX[(K~(JI+80)).[33:6],K.[39:9]]-SVXMIN[(K~(JI)).    00146900
     [33:6],K.[39:9]])/49);                                             00147000
IF JSPI = 0 THEN                                                        00147100
          BEGIN                                                         00147200
               WRITE (PRINT,ZERO);                                      00147300
               JSPI ~ 1 | @-4                                           00147400
          END;                                                          00147500
    IF (JKKK{1) THEN GO TO L101;                                        00147600
    JSQI~((SVXMAX[(K~(JJ1+80)).[33:6],K.[39:9]]-SVXMIN[(K~(JJ1)).[33:6],00147700
     K.[39:9]])/49);                                                    00147800
    L101: JJ~1;                                                         00147900
    DO BEGIN                                                            00148000
      JAJ~1+5|(JJ-1);                                                   00148100
      SVPINT[JJ]~SVXMIN[(K~(JI)).[33:6],K.[39:9]]+JSPI|(JAJ-1);         00148200
      IF (JKKK{1) THEN GO TO L20500;                                    00148300
      SVQINT[JJ]~SVXMIN[(K~(JJ1)).[33:6],K.[39:9]]+JSQI|(JAJ-1);        00148400
      L20500: END UNTIL (JJ~(JJ+1))>11;                                 00148500
    JK~1;                                                               00148600
    DO BEGIN                                                            00148700
      JJ~1;                                                             00148800
      DO BEGIN                                                          00148900
        SVKP[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]~INT(JIIX);              00149000
        SVIQ[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]~INT(JIIX);              00149100
        END UNTIL (JJ~(JJ+1))>50  END UNTIL (JK~(JK+1))>50;             00149200
          BEGIN                                                         00149300
REAL           ERSATZ  ;                                                00149400
LABEL          L61001,L61000,L76000,L109,L20700 ;                       00149500
               ERSATZ ~ 1.0 ;                                           00149600
    JLLL~JN|JIP;                                                        00149700
    JLLLL~0;                                                            00149800
    JLLLLL~JLLLL;                                                       00149900
    JJ~1;                                                               00150000
    DO BEGIN                                                            00150100
      JJJJJJ~1;                                                         00150200
READ (TAPE2,OKTL,FOR DX1 ~ 1 STEP 1 UNTIL JIP DO SVX[DX1]);             00150300
COMMENT  THE NEXT WRITE IS FOR DIAGNOSING DISK FILE HANDLING PROBLEMS.  00150400
      WRITE(PRINT,FLL600,FOR DX1~1 STEP 1 UNTIL JIP DO[DX1,SVX[DX1]]    00150500
);                                                                      00150600
JIR~ENTIER   (((SVX[JII]-SVXMIN[(K~(JI)).[33:6],K.[39:9]])/JSPI)+1.5);  00150700
JIRES ~                                                                 00150800
     ENTIER(((SVRES[(K~(JJ)).[34:5],K.[39:9]]-JRMIN)/JSRS)+1.5)  ;      00150900
      SVKP[(K~(50|JIR+JIRES-50)).[36:3],K.[39:9]]~INT(SVKP[(K~(50|      00151000
       JIR+JIRES-50)).[36:3],K.[39:9]]+1.1 )  ;                         00151100
      IF (JKKK{1) THEN GO TO L20700;                                    00151200
      JJQ~((SVX[JJJ]-SVXMIN[(K~(JJ1)).[33:6],K.[39:9]])/JSQI)+1.5;      00151300
      SVIQ[(K~(50|JJQ+JIRES-50)).[36:3],K.[39:9]]~INT(SVIQ[(K~(50|      00151400
       JJQ+JIRES-50)).[36:3],K.[39:9]]+1);                              00151500
      L20700: END UNTIL (JJ~(JJ+1))>JN;                                 00151600
          END;                                                          00151700
    REWIND(TAPE2);                                                      00151800
               WRITE(PRINT[PAGE])  ;                                    00151900
               WRITE(PRINT,FL9950,LIST35)  ;                            00152000
    L111: JXX~(+" ");                                                   00152100
    JXY~(+"*");                                                         00152200
    WRITE(PRINT,FL9961);                                                00152300
               WRITE(PRINT,FL9955,LIST37)  ;                            00152400
    JMNMN~51;                                                           00152500
L9403:         WRITE(PRINT,FL9956,LIST39)  ;                            00152600
    JKLN~0;                                                             00152700
    JKLM~5;                                                             00152800
    JK~1;                                                               00152900
    DO BEGIN                                                            00153000
      JJ~1;                                                             00153100
      DO BEGIN                                                          00153200
        IF (SVKP[(K~(50|JJ+JK-50)).[36:3],K.[39:9]])>0 THEN GO TO L83;  00153300
        SVP[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]~JXX;                     00153400
        GO TO L86;                                                      00153500
        L83: IF (SVKP[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]<10) THEN GO TO 00153600
         L84;                                                           00153700
        SVP[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]~JXY;                     00153800
        GO TO L86;                                                      00153900
L84:           K ~ 50 | JJ + JK - 50 ;                                  00154000
               SVP[K.[36:3],K.[39:9]] ~ SVKP[K.[36:3],K.[39:9]] ;       00154100
        L86: IF (JKKK{1) THEN GO TO L20900;                             00154200
        IF (SVIQ[(K~(50|JJ+JK-50)).[36:3],K.[39:9]])>0 THEN GO TO L88;  00154300
        SVQ[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]~JXX;                     00154400
        GO TO L20900;                                                   00154500
        L88: IF (SVIQ[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]<10) THEN GO TO 00154600
         L89;                                                           00154700
        SVQ[(K~(50|JJ+JK-50)).[36:3],K.[39:9]]~JXY;                     00154800
        GO TO L20900;                                                   00154900
L89:           K ~ 50 | JJ + JK - 50 ;                                  00155000
               SVQ[K.[36:3],K.[39:9]] ~ SVIQ[K.[36:3],K.[39:9]] ;       00155100
        L20900: END UNTIL (JJ~(JJ+1))>50;                               00155200
      JKLM~JKLM-1;                                                      00155300
      IF (JKLM}4) THEN GO TO L94;                                       00155400
      IF JKLM>0 THEN GO TO L96 ELSE GO TO L95;                          00155500
      L94: JKLN~JKLN+1;                                                 00155600
      IF (JKKK{1) THEN GO TO L9551;                                     00155700
               WRITE(PRINT,FL9953,LIST44)  ;                            00155800
      GO TO L20800;                                                     00155900
L9551:         WRITE(PRINT,FL9953,LIST45)  ;                            00156000
      GO TO L20800;                                                     00156100
      L95: JKLM~5;                                                      00156200
      L96: IF (JKKK>1) THEN GO TO L9662;                                00156300
               WRITE (PRINT,FL9954,LIST46 )  ;                          00156400
      GO TO L20800;                                                     00156500
L9662:         WRITE(  PRINT,FL9954,LIST47 )  ;                         00156600
      L20800: END UNTIL (JK~(JK+1))>50;                                 00156700
               WRITE(PRINT,FL9956,LIST39 )  ;                           00156800
    IF (JKKK>1) THEN GO TO L114;                                        00156900
               WRITE (PRINT,FL9955,LIST37 )  ;                          00157000
    GO TO L20401;                                                       00157100
L114:          WRITE(PRINT,FL9955,LIST38 )  ;                           00157200
    L20401: REWIND(TAPE2);                                              00157300
    END UNTIL (JI~(JI+1))>JNVIP;                                        00157400
               ERSATZ ~ 0.0  ;                                          00157500
          END;                                                          00157600
  L13000: END UNTIL (JM~(JM+1))>JNSPC;                                  00157700
GO TO NEXTPROB;                                                         00157800
L9001:                                                                  00157900
WRITE(PRINT,FL8020);                                                    00158000
L9999: WRITE(PRINT,FL8021);                                             00158100
JI~1;                                                                   00158200
DO BEGIN                                                                00158300
  IF (SVNUSE[JI]){0 THEN GO TO L22100;                                  00158400
COMMENT        A REWIND STATEMENT HAS BEEN REMOVED HERE  ;              00158500
  L22100: END UNTIL (JI~(JI+1))>4;                                      00158600
JI~7;                                                                   00158700
DO BEGIN                                                                00158800
  IF (SVNUSE[JI]){0 THEN GO TO L22200;                                  00158900
COMMENT SUBROUTINE REMOVE WAS CALLED FOR AT THIS POINT ;                00159000
  L22200: END UNTIL (JI~(JI+1))>20;                                     00159100
GO TO FINIS;                                                            00159200
L9002:                                                                  00159300
WRITE(PRINT,FL8022);                                                    00159400
GO TO L9999;                                                            00159500
L9003: WRITE(PRINT,FL8023);                                             00159600
GO TO L9999;                                                            00159700
L9004: WRITE(PRINT,FL8024);                                             00159800
               WRITE(PRINT,SW[ERRORIDENT], SL[ERRORIDENT]);             00159900
               IF REGRESSCHECK = 3 THEN                                 00160000
              WRITE(PRINT,F1,JNVIP);                                    00160100
               IF REGRESSCHECK = 4 THEN                                 00160200
               WRITE(PRINT,F2,SVIVPT[JK],JIP);                          00160300
               WRITE(LINE[DBL]);                                        00160400
GO TO L9999;                                                            00160500
L9006:                                                                  00160600
WRITE(PRINT,FL8026);                                                    00160700
GO TO L9004;                                                            00160800
L9009: WRITE(PRINT,FL8029);                                             00160900
GO TO L9004;                                                            00161000
L9010:                                                                  00161100
GO TO L9004;                                                            00161200
WRITE(PRINT,FL8030);                                                    00161300
L9011: WRITE(PRINT,FL8033);                                             00161400
GO TO L9004;                                                            00161500
DATAERROR:    WHEN(1); WRITE(PRINT,F3); GO TO FINIS;                    00161600
END  END;                                                               00161700
COMMENT **************************  OUTER BLOCK  ************;          00161800
PAGEIT~FALSE;                                                           00161900
NEXTPROB:                                                               00162000
READ(TAPE[NO],FSETUP,JN2,JN1,JNVA,FRFIELD);                             00162100
BB ~ 2 | (JN1  + JNVA);                                                 00162200
  XPR~Q~K~0;                                                            00162300
 SENSL[1]~FALSE;                                                        00162400
 SENSL[2]~FALSE;                                                        00162500
 SENSL[3]~FALSE;                                                        00162600
 SENSL[4]~FALSE;                                                        00162700
MAINPRO; FINIS:                                                         00162800
END OF OUTER BLK.                                                       00162900
                LAST CARD ON CRDIMG TAPE                                99999999
