BEGIN                                                                   00000000
COMMENT        ANCOVA1/STATMAN;                                         00000100
COMMENT        THIS IS BMD04V;                                          00000200
          FORMAT OKTL(50O);                                             00000300
     FILE TAPE2 2(2,50);                                                00000400
          FILE TAPE3 (2,56,10);                                         00000500
INTEGER        FORMOT;                                                  00000600
FORMAT         CHKFOR (A6);                                             00000700
FORMAT         NOFRM ("MISSING FORMAT SPECIFICATION CARD(S)");          00000800
FORMAT         FMT2(//////////////////////////////////////////////////  00000900
                    //////////////////////////////////////////////////  00001000
                    //////////////////////////////////////////////////  00001100
                    //////////////////////////////////////////////////  00001200
                    //////////////////////////////////////////////////);00001300
INTEGER Z;                                                              00001400
INTEGER ARRAY NN,SCALE[0:100];                                          00001500
FILE IN CARD 0(2,10);     FILE OUT  LINE 4(2,15);                       00001600
FILE OUT PUNCH 0(2,10);                                                 00001700
BOOLEAN        BADCARDFLAG;                                             00001800
BOOLEAN ARRAY  SENSL[0:4];                                              00001900
REAL ARRAY  COMMON[0:15];                                               00002000
REAL  XPR;                                                              00002100
INTEGER  QQ;                                                            00002200
LABEL FINIS;                                                            00002300
INTEGER ARRAY   NEWQ[0:99],  JUMPQ[0:99],  NAQ[0:99],                   00002400
     NSAMQ[0:99],  NSUBQ[0:6,0:99],  NGQ[0:99],  LLLQ[0:35],            00002500
     KKKQ[0:36],  NAMESQ[0:36],  INDEX1Q[0:36],  INDEX2Q[0:36];         00002600
REAL ARRAY   SLQ[0:26],  BNQ[0:99],  FMTQ[0:120],  DATAQ[0:100],        00002700
     CONTQ[0:6,0:99],  RELQ[0:6,0:99],  OPERQ[0:6,0:99],                00002800
     X[0:36],  SSVARQ[0:36,0:99],  TOTXXQ[0:36,0:36],                   00002900
     TOTXYQ[0:36],  TREXXQ[0:36,0:36],  TREXYQ[0:36],                   00003000
     ERRXXQ[0:36,0:36],  ERRXYQ[0:36],  B[0:35],  C[0:35],              00003100
     D[0:35],  SEBQ[0:35],  SECQ[0:35],  SEDQ[0:35],                    00003200
     ADYQ[0:99],  SEADYQ[0:99];                                         00003300
PROCEDURE      BADCARD(P);                                              00003400
VALUE          P;                                                       00003500
INTEGER        P;                                                       00003600
          BEGIN                                                         00003700
SWITCH FORMAT  SWF~("PROBLEM CARD SPECIFIES MORE VARIABLES THAN ",      00003800
                    "ALLOWED."),                                        00003900
                   ("PROBLEM CARD INDICATES THAT NUMBER OF VARIABLES "  00004000
                    "READ IN PLUS NUMBER ADDED EXCEEDS LIMIT OF 99."),  00004100
                   ("USER HAS DESIGNATED ILLEGAL BCD TAPE AS INPUT."),  00004200
                   ("LESS THAN TWO VARIABLES INPUT IMPLIES NO ",        00004300
                    "INDEPENDENT COVARIATE."),                          00004400
                   ("PROBLEM CARD INDICATES INCORRECT NUMBER OF ",      00004500
                    "TRANSGENERATION CARDS."),                          00004600
                   ("TRNGEN CARDS INCORRECTLY ORDERED OR PUNCHED."),    00004700
                   ("PROBLEM CARD INDICATES INCORRECT NUMBER OF ",      00004800
                    "GROUP DEFINITION CARDS."),                         00004900
                   ("GRPDEF CARDS INCORRECTLY ORDERED OR PUNCHED."),    00005000
                   ("SAMSIZ CARDS INCORRECTLY ORDERED OR PUNCHED."),    00005100
                   ("PROBLEM CARD INDICATES INCORRECT NUMBER OF ",      00005200
                    "LABELS CARDS."),                                   00005300
                   ("LABELS CARDS INCORRECTLY ORDERED OR PUNCHED."),    00005400
                   ("CVRSEL CARD  INCORRECTLY ORDERED OR PUNCHED."),    00005500
                   ("CVRSEL CARD INDICATES NUMBER OF COVARIATES ",      00005600
                    "EXCEEDS PROGRAM LIMIT OF 35."),                    00005700
                   ("NUMBER OF GROUPS AND COVARIATES RELATIVE TO ",     00005800
                    "NUMBER OF CASES INDICATES INSUFFICIENT DEGREES ",  00005900
                    "OF FREEDOM.");                                     00006000
FORMAT         FERR(//"ERROR FINISH:"/);                                00006100
               WRITE(LINE,FERR);                                        00006200
               WRITE(LINE,SWF[P]);                                      00006300
               BADCARDFLAG~TRUE;                                        00006400
          END  OF BADCARD;                                              00006500
 PROCEDURE DATELINE (PROGRAM);                                          00006600
 VALUE PROGRAM;                                                         00006700
 ALPHA PROGRAM;                                                         00006800
 BEGIN OWN BOOLEAN USED;                                                00006900
   FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",    00007000
   A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER" ///),                 00007100
               LAYT (/ / "EXECUTION TIME =", I5, X03, "I/O TIME =", I5, 00007200
" SECONDS  ",A4,I3,", ", A4, X03, "TIME:", I7           / / /);         00007300
    LABEL GOTIT;                                                        00007400
    ALPHA MO, MINS, FEB, HRS, YR, DAY;                                  00007500
    USED ~ USED AND PROGRAM . [18 : 6] = 0;                             00007600
    DAY ~ TIME (0);                                                     00007700
    YR ~ DAY . [18 : 12] + "1900";                                      00007800
    DAY ~ DAY . [42 : 6] + 10 | DAY . [36 : 6] + 100 | DAY . [30 : 6];  00007900
    FEB ~ IF YR . [42 : 6] MOD 4 = 0 THEN "(FEB." ELSE "&FEB.";         00008000
    FOR MO ~ "~JAN.", FEB, "~MAR.", "<APR.", "~ MAY", "<JUNE", "~JULY"  00008100
    , "~AUG.", "<SEPT", "~OCT.", "<NOV.", "~DEC." DO                    00008200
    BEGIN IF DAY { MO . [18 : 06] THEN GO TO GOTIT;                     00008300
       DAY ~ DAY - MO . [18 : 6];                                       00008400
    END;                                                                00008500
    GOTIT: MINS ~ TIME (1) / 3600;                                      00008600
    HRS ~ 100 | (MINS DIV 60) + MINS MOD 60;                            00008700
    IF USED THEN WRITE (LINE, LAYT, TIME (2) / 60, TIME (3) / 60, MO,   00008800
    DAY, YR, HRS) ELSE WRITE (LINE, HD,MO,DAY,YR,HRS,PROGRAM);          00008900
    USED ~ TRUE;                                                        00009000
 END OF DATELINE;                                                       00009100
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00009200
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00009300
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00009400
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00009500
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00009600
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00009700
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00009800
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00009900
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00010000
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00010100
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00010200
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00010300
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00010400
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00010500
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00010600
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00010700
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00010800
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00010900
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300011000
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00011100
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00011200
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00011300
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00011400
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00011500
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00011600
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00011700
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00011800
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100011900
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00012000
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00012100
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00012200
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00012300
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00012400
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00012500
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00012600
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00012700
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00012800
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00012900
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00013000
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00013100
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00013200
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00013300
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00013400
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00013500
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00013600
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00013700
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00013800
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00013900
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00014000
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00014100
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00014200
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00014300
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00014400
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00014500
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00014600
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800014700
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00014800
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00014900
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00015000
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00015100
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00015200
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00015300
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00015400
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00015500
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00015600
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00015700
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00015800
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00015900
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00016000
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00016100
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00016200
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00016300
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00016400
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00016500
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00016600
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00016700
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00016800
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00016900
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00017000
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00017100
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00017200
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00017300
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00017400
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00017500
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00017600
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00017700
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00017800
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00017900
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00018000
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00018100
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00018200
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00018300
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00018400
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100018500
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00018600
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00018700
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00018800
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00018900
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00019000
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000019100
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00019200
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00019300
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00019400
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00019500
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00019600
PROCEDURE ERROR(N); VALUE N; INTEGER N; BEGIN                           00019700
  FORMAT F(///////"STOP / PAUSE  NO. ", I5);                            00019800
  WRITE (LINE,  F, N);  GO TO FINIS END;                                00019900
REAL PROCEDURE INT(ARG1); VALUE ARG1; REAL ARG1;                        00020000
    BEGIN INT~(SIGN(ARG1)|ENTIER(ABS(ARG1))) END;                       00020100
REAL PROCEDURE MAX(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00020200
  BEGIN MAX~(IF ARG1}ARG2 THEN ARG1 ELSE ARG2) END;                     00020300
REAL PROCEDURE MIN(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00020400
  BEGIN MIN~(IF ARG1{ARG2 THEN ARG1 ELSE ARG2) END;                     00020500
COMMENT **************************  MATINV  ****************;           00020600
PROCEDURE MATINVQ(A,N,INQ,V);                                           00020700
       VALUE N;                                                         00020800
       INTEGER N;                                                       00020900
       INTEGER ARRAY  INQ[0];                                           00021000
       REAL ARRAY  A[0,0],V[0];                                         00021100
BEGIN                                                                   00021200
OWN REAL ARRAY   U[0:36];                                               00021300
OWN INTEGER I, K, L, J;                                                 00021400
OWN REAL D, P, T, H, Y;                                                 00021500
LABEL DUMMY,L7;                                                         00021600
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00021700
            V[I] ~ A[I,I];                                              00021800
            INQ[I] ~ 0  END;                                            00021900
          D ~ 1;                                                        00022000
          K ~ 1;                                                        00022100
          FOR L ~ 1, L+1 STEP 1 UNTIL N DO BEGIN                        00022200
            FOR I ~ 1, I+1 STEP 1 UNTIL K DO BEGIN                      00022300
              U[I] ~ A[I,K];                                            00022400
              A[I,K] ~ 0  END;                                          00022500
            P ~ U[K];                                                   00022600
            FOR I ~ K, I+1 STEP 1 UNTIL N DO BEGIN                      00022700
              U[I] ~ A[K,I];                                            00022800
              A[K,I] ~ 0  END;                                          00022900
            T ~ H;                                                      00023000
            H ~ -1@20;                                                  00023100
            INQ[K] ~ 1;                                                 00023200
            U[K] ~ -1;                                                  00023300
            D ~ D|P;                                                    00023400
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00023500
              Y ~ U[I]/P;                                               00023600
              FOR J ~ 1, J+1 STEP 1 UNTIL I DO BEGIN                    00023700
                A[J,I] ~ A[J,I]-Y|U[J]  END;                            00023800
              IF INQ[I]>0 THEN GO TO L7;                                00023900
              IF H}A[I,I]/V[I] THEN GO TO L7;                           00024000
              H ~ A[I,I]/V[I];                                          00024100
              K ~ I;                                                    00024200
L7:           END;                                                      00024300
            END;                                                        00024400
          FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                        00024500
            FOR J ~ 1, J+1 STEP 1 UNTIL I DO BEGIN                      00024600
              A[J,I] ~ -A[J,I];                                         00024700
              A[I,J] ~ A[J,I]  END;                                     00024800
            END;                                                        00024900
          GO TO DUMMY;                                                  00025000
DUMMY:    END;                                                          00025100
                                                                        00025200
COMMENT **************************  GRPSEL  ****************;           00025300
PROCEDURE GRPSELQ(M);                                                   00025400
       INTEGER M;                                                       00025500
BEGIN                                                                   00025600
OWN REAL ARRAY   B[0:6];                                                00025700
OWN INTEGER  AVISKIP;                                                   00025800
OWN INTEGER I, ISQ;                                                     00025900
OWN REAL FBQ, A, O, STARQ;                                              00026000
FORMAT FL4000(/" ",X14,"ILLEGAL OPERATION OR RELATION ",A2,             00026100
         ". PROGRAM WILL PROCEED TO NEXT JOB, IF ANY, OR TERMI","NATE." 00026200
         );                                                             00026300
LIST LIST1(RELQ[I,M]);                                                  00026400
LABEL DUMMY,L500,L96,L2,L4,L6,L8,L10,L11,L20,L50,L90,L100,L105,L110,    00026500
        L125,L145,L147,L150,L152,L170,L175,L222,L300,L333,L350,L104;    00026600
SWITCH ASSIGN~L105,L125;                                                00026700
          FBQ ~ 1;                                                      00026800
          AVISKIP ~ 1;                                                  00026900
          A ~ (+"A");                                                   00027000
          O ~ (+"O");                                                   00027100
          STARQ ~ (+"*");                                               00027200
L500:     FOR I ~ 1, I+1 STEP 1 UNTIL 6 DO BEGIN                        00027300
            ISQ ~ NSUBQ[I,M];                                           00027400
            IF RELQ[I,M]=(+"GT") THEN GO TO L2;                         00027500
            IF RELQ[I,M]=(+"LT") THEN GO TO L4;                         00027600
            IF RELQ[I,M]=(+"LE") THEN GO TO L6;                         00027700
            IF RELQ[I,M]=(+"GE") THEN GO TO L8;                         00027800
            IF RELQ[I,M]=(+"EQ") THEN GO TO L10;                        00027900
            IF RELQ[I,M]=(+"NE") THEN GO TO L11;                        00028000
            IF RELQ[I,M]=(+"  ") THEN GO TO L100;                       00028100
L96:        WRITE(LINE,FL4000,LIST1);                                   00028200
            M ~ -6;                                                     00028300
            GO TO DUMMY;                                                00028400
L2:         IF DATAQ[ISQ]>CONTQ[I,M] THEN GO TO L20 ELSE GO TO L50;     00028500
L4:         IF DATAQ[ISQ]}CONTQ[I,M] THEN GO TO L50 ELSE GO TO L20;     00028600
L6:         IF DATAQ[ISQ]>CONTQ[I,M] THEN GO TO L50 ELSE GO TO L20;     00028700
L8:         IF DATAQ[ISQ]}CONTQ[I,M] THEN GO TO L20 ELSE GO TO L50;     00028800
L10:        IF DATAQ[ISQ]=CONTQ[I,M] THEN GO TO L20 ELSE GO TO L50;     00028900
L11:        IF DATAQ[ISQ]=CONTQ[I,M] THEN GO TO L50;                    00029000
L20:        B[I] ~ 1;                                                   00029100
            GO TO L90;                                                  00029200
L50:        B[I] ~ 0;                                                   00029300
L90:        END;                                                        00029400
L100:     GO TO ASSIGN[AVISKIP];                                        00029500
L105:     IF OPERQ[1,M]=A THEN GO TO L147 ELSE GO TO L145;              00029600
L110:     FBQ ~ FBQ|B[1];                                               00029700
          GO TO L175;                                                   00029800
L125:     IF OPERQ[6,M-1]=A THEN GO TO L110 ELSE GO TO L150;            00029900
L145:     IF OPERQ[1,M]!O THEN GO TO L170;                              00030000
L147:     FBQ ~ B[1];                                                   00030100
          GO TO L175;                                                   00030200
L150:     IF OPERQ[6,M-1]=O THEN GO TO L152;                            00030300
          I ~ 6;                                                        00030400
          RELQ[6,M] ~ OPERQ[6,M-1];                                     00030500
          GO TO L96;                                                    00030600
L152:     IF FBQ>0 THEN GO TO L175 ELSE GO TO L147;                     00030700
L170:     FBQ ~ B[1];                                                   00030800
          GO TO L350;                                                   00030900
L175:     FOR I ~ 1, I+1 STEP 1 UNTIL 5 DO BEGIN                        00031000
            IF OPERQ[I,M]!A THEN GO TO L222;                            00031100
            FBQ ~ FBQ|B[I+1];                                           00031200
            GO TO L300;                                                 00031300
L222:       IF OPERQ[I,M]!O THEN GO TO L333;                            00031400
            IF FBQ>0 THEN GO TO L300;                                   00031500
            FBQ ~ B[I+1];                                               00031600
L300:       END;                                                        00031700
          IF OPERQ[6,M]=STARQ THEN GO TO L350;                          00031800
          M ~ M+1;                                                      00031900
          AVISKIP ~ 2;                                                  00032000
          GO TO L500;                                                   00032100
L333:     IF OPERQ[I,M]=STARQ THEN GO TO L350;                          00032200
          RELQ[I,M] ~ OPERQ[I,M];                                       00032300
          GO TO L96;                                                    00032400
L350:     IF FBQ{0 THEN GO TO DUMMY;                                    00032500
          DATAQ[100] ~ NGQ[M];                                          00032600
L104:     GO TO DUMMY;                                                  00032700
DUMMY:    END;                                                          00032800
                                                                        00032900
COMMENT **************************  PATTY2  ****************;           00033000
PROCEDURE PATTY2Q(A,N,NAMESQ);                                          00033100
       INTEGER N;                                                       00033200
       INTEGER ARRAY  NAMESQ[0];                                        00033300
       REAL ARRAY  A[0,0];                                              00033400
BEGIN                                                                   00033500
OWN INTEGER ARRAY   NNQ[0:8];                                           00033600
INTEGER  DX1;                                                           00033700
OWN INTEGER ITQ, KKQ, K1Q, K2Q, JXQ, LLLQ, LLQ, JJQ, I;                 00033800
FORMAT FL300(/" ",X13,A6,7(X8,A6)/),                                    00033900
       FL20(" ",A6,X1,8R14.4);                                          00034000
LIST LIST1(FOR DX1 ~ 1 STEP 1 UNTIL LLLQ DO NNQ[DX1]),                  00034100
     LIST2(NAMESQ[I],FOR DX1 ~ K1Q STEP 1 UNTIL K2Q DO A[DX1,           00034200
       I]);                                                             00034300
LABEL DUMMY,L5,L3;                                                      00034400
          ITQ ~ 1;                                                      00034500
          KKQ ~ 0;                                                      00034600
          K1Q ~ ITQ;                                                    00034700
          K2Q ~ MIN(8,N);                                               00034800
L5:       KKQ ~ KKQ+8;                                                  00034900
          IF N{KKQ THEN GO TO L3;                                       00035000
          ITQ ~ ITQ+1;                                                  00035100
          GO TO L5;                                                     00035200
L3:       FOR JXQ ~ 1, JXQ+1 STEP 1 UNTIL ITQ DO BEGIN                  00035300
            LLLQ ~ K2Q-K1Q+1;                                           00035400
            LLQ ~ 0;                                                    00035500
            FOR JJQ ~ K1Q, JJQ+1 STEP 1 UNTIL K2Q DO BEGIN              00035600
              LLQ ~ LLQ+1;                                              00035700
              NNQ[LLQ] ~ NAMESQ[JJQ]  END;                              00035800
            WRITE(LINE,FL300,LIST1);                                    00035900
            FOR I ~ 1, I+1 STEP 1 UNTIL N DO BEGIN                      00036000
              WRITE(LINE,FL20,LIST2);                                   00036100
              END;                                                      00036200
            K1Q ~ K2Q+1;                                                00036300
            K2Q ~ K1Q+7;                                                00036400
            K2Q ~ MIN(K2Q,N);                                           00036500
            END;                                                        00036600
          GO TO DUMMY;                                                  00036700
DUMMY:    END;                                                          00036800
                                                                        00036900
COMMENT **************************  TG41  ****************;             00037000
PROCEDURE TG41Q(IIQ,M);                                                 00037100
       VALUE M;                                                         00037200
       INTEGER IIQ,M;                                                   00037300
BEGIN                                                                   00037400
  DEFINE  NTGQ    =COMMON[4]#;                                          00037500
  DEFINE  NCASEQ  =COMMON[5]#;                                          00037600
OWN INTEGER I, JOKEQ, NWQ, NAAQ, NBQ, NSKIPQ;                           00037700
OWN REAL SAMPXQ, PI2Q, BBNQ, A, B;                                      00037800
FORMAT FL4001(/" TRANSGENERATOR CARD",I3,                               00037900
         " HAS VARIABLE NUMBERS TOO LARGE FOR PROGRAM."),               00038000
       FL4000(/" DATA OF VARIABLE",I4," VIOLATES RESTRICTION FOR CODE", 00038100
         I3,". THIS WAS FOR TREATMENT GROUP",I3," SAMPLE",I4),          00038200
       FL4002(" THE DATA WILL REMAIN UNCHANGED.");                      00038300
LIST LIST1(I),                                                          00038400
     LIST2(NAAQ,JUMPQ[I],IIQ,M);                                        00038500
LABEL DUMMY,L33,L1,L11,L12,L2,L13,L14,L3,L4,L5,L18,L6,L24,L287,L29,     00038600
        L7,L8,L9,L10,L111,L110,L120,L130,L140,L150,L160,L170,L180,      00038700
        L190,L20,L21,L22,L230,L97,L98,L99,L991,L1000;                   00038800
SWITCH SWGO1~L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L111,L111,L111,L111,L150,   00038900
        L111,L170,L180,L190,L20,L21,L22,L230;                           00039000
SWITCH SWGO2~L110,L120,L130,L140,L150,L160;                             00039100
          REAL PROCEDURE ASNQ(ARGX);                                    00039200
          VALUE ARGX;                                                   00039300
          REAL ARGX;                                                    00039400
          BEGIN ASNQ ~ ARCTAN(ARGX/SQRT(1-ARGX*2))  END;                00039500
          SAMPXQ ~ NCASEQ;                                              00039600
          PI2Q ~ 1.57079633;                                            00039700
          FOR I ~ 1, I+1 STEP 1 UNTIL NTGQ DO BEGIN                     00039800
          JOKEQ ~ JUMPQ[I];                                             00039900
          NWQ ~ NEWQ[I];                                                00040000
          IF NWQ}100 THEN GO TO L97;                                    00040100
          NAAQ ~ NAQ[I];                                                00040200
          IF NAAQ}100 THEN GO TO L97;                                   00040300
          NBQ ~ BNQ[I];                                                 00040400
          BBNQ ~ BNQ[I];                                                00040500
          IF JOKEQ{17 THEN GO TO L33;                                   00040600
          JOKEQ ~ JOKEQ-2;                                              00040700
          IF JOKEQ{22 THEN GO TO L33;                                   00040800
          JOKEQ ~ 23;                                                   00040900
L33:      GO TO SWGO1[JOKEQ];                                           00041000
L1:       IF (XPR ~ DATAQ[NAAQ])>0 THEN GO TO L12;                      00041100
          IF XPR<0 THEN GO TO L99;                                      00041200
L11:      DATAQ[NWQ] ~ 0;                                               00041300
          GO TO L1000;                                                  00041400
L12:      DATAQ[NWQ] ~ SQRT(DATAQ[NAAQ]);                               00041500
          GO TO L1000;                                                  00041600
L2:       IF (XPR ~ DATAQ[NAAQ])>0 THEN GO TO L14;                      00041700
          IF XPR<0 THEN GO TO L99;                                      00041800
L13:      DATAQ[NWQ] ~ 1;                                               00041900
          GO TO L1000;                                                  00042000
L14:      DATAQ[NWQ] ~ SQRT(DATAQ[NAAQ])+SQRT(DATAQ[NAAQ]+1);           00042100
          GO TO L1000;                                                  00042200
L3:       IF DATAQ[NAAQ]{0 THEN GO TO L99;                              00042300
          DATAQ[NWQ] ~ 0.4342944819|LN(DATAQ[NAAQ]);                    00042400
          GO TO L1000;                                                  00042500
L4:       DATAQ[NWQ] ~ EXP(DATAQ[NAAQ]);                                00042600
          GO TO L1000;                                                  00042700
L5:       IF (XPR ~ DATAQ[NAAQ])=0 THEN GO TO L11;                      00042800
          IF XPR<0 THEN GO TO L99;                                      00042900
          IF (XPR ~ DATAQ[NAAQ]-1)>0 THEN GO TO L99;                    00043000
          IF XPR<0 THEN GO TO L18;                                      00043100
          DATAQ[NWQ] ~ PI2Q;                                            00043200
          GO TO L1000;                                                  00043300
L18:      A ~ SQRT(DATAQ[NAAQ]);                                        00043400
          DATAQ[NWQ] ~ ASNQ(A);                                         00043500
          GO TO L1000;                                                  00043600
L6:       A ~ DATAQ[NAAQ]/(SAMPXQ+1);                                   00043700
          B ~ A+1/(SAMPXQ+1);                                           00043800
          IF (XPR ~ A)>0 THEN GO TO L24;                                00043900
          IF XPR<0 THEN GO TO L99;                                      00044000
          DATAQ[NWQ] ~ ASNQ(SQRT(B));                                   00044100
          GO TO L1000;                                                  00044200
L24:      IF (XPR ~ B)>0 THEN GO TO L29;                                00044300
          IF XPR<0 THEN GO TO L99;                                      00044400
          IF (XPR ~ A-1)>0 THEN GO TO L99;                              00044500
          IF XPR=0 THEN GO TO L287;                                     00044600
          DATAQ[NWQ] ~ ASNQ(SQRT(A))|2;                                 00044700
          GO TO L1000;                                                  00044800
L287:     DATAQ[NWQ] ~ 3.14159265;                                      00044900
          GO TO L1000;                                                  00045000
L29:      A ~ SQRT(A);                                                  00045100
          B ~ SQRT(B);                                                  00045200
          IF A>1 THEN GO TO L99;                                        00045300
          IF B>1 THEN GO TO L99;                                        00045400
          DATAQ[NWQ] ~ ASNQ(A)+ASNQ(B);                                 00045500
          GO TO L1000;                                                  00045600
L7:       IF DATAQ[NAAQ]=0 THEN GO TO L99;                              00045700
          DATAQ[NWQ] ~ 1/DATAQ[NAAQ];                                   00045800
          GO TO L1000;                                                  00045900
L8:       DATAQ[NWQ] ~ DATAQ[NAAQ]+BBNQ;                                00046000
          GO TO L1000;                                                  00046100
L9:       DATAQ[NWQ] ~ DATAQ[NAAQ]|BBNQ;                                00046200
          GO TO L1000;                                                  00046300
L10:      IF (XPR ~ DATAQ[NAAQ])=0 THEN GO TO L11;                      00046400
          IF XPR<0 THEN GO TO L99;                                      00046500
          DATAQ[NWQ] ~ DATAQ[NAAQ]*BBNQ;                                00046600
          GO TO L1000;                                                  00046700
L111:     IF NBQ}100 THEN GO TO L97;                                    00046800
          NSKIPQ ~ JOKEQ-10;                                            00046900
          GO TO SWGO2[NSKIPQ];                                          00047000
L110:     DATAQ[NWQ] ~ DATAQ[NAAQ]+DATAQ[NBQ];                          00047100
          GO TO L1000;                                                  00047200
L120:     DATAQ[NWQ] ~ DATAQ[NAAQ]-DATAQ[NBQ];                          00047300
          GO TO L1000;                                                  00047400
L130:     DATAQ[NWQ] ~ DATAQ[NAAQ]|DATAQ[NBQ];                          00047500
          GO TO L1000;                                                  00047600
L140:     IF DATAQ[NBQ]=0 THEN GO TO L98;                               00047700
          DATAQ[NWQ] ~ DATAQ[NAAQ]/DATAQ[NBQ];                          00047800
          GO TO L1000;                                                  00047900
L150:     IF DATAQ[NAAQ]}BBNQ THEN GO TO L13 ELSE GO TO L11;            00048000
L160:     IF DATAQ[NAAQ]}DATAQ[NBQ] THEN GO TO L13 ELSE GO TO L11;      00048100
L170:     IF DATAQ[NAAQ]{0 THEN GO TO L99;                              00048200
          DATAQ[NWQ] ~ LN(DATAQ[NAAQ]);                                 00048300
          GO TO L1000;                                                  00048400
L180:     DATAQ[NWQ] ~ SIN(DATAQ[NAAQ]);                                00048500
          GO TO L1000;                                                  00048600
L190:     DATAQ[NWQ] ~ COS(DATAQ[NAAQ]);                                00048700
          GO TO L1000;                                                  00048800
L20:      IF (DATAQ[NAAQ]-PI2Q)|(-PI2Q-DATAQ[NAAQ])>0 THEN GO TO L99;   00048900
          DATAQ[NWQ] ~ ARCTAN(DATAQ[NAAQ]);                             00049000
          GO TO L1000;                                                  00049100
L21:      IF (XPR ~ DATAQ[NAAQ])=0 THEN GO TO L11;                      00049200
          IF XPR<0 THEN GO TO L99;                                      00049300
          IF NBQ}100 THEN GO TO L97;                                    00049400
          DATAQ[NWQ] ~ DATAQ[NAAQ]*DATAQ[NBQ];                          00049500
          GO TO L1000;                                                  00049600
L22:      IF (XPR ~ BBNQ)=0 THEN GO TO L11;                             00049700
          IF XPR<0 THEN GO TO L99;                                      00049800
          DATAQ[NWQ] ~ BBNQ*DATAQ[NAAQ];                                00049900
          GO TO L1000;                                                  00050000
L230:     IF DATAQ[NAAQ]!0 THEN GO TO L1000;                            00050100
          IF (ABS(10)|SIGN(DATAQ[NAAQ]))}0 THEN GO TO L1000;            00050200
          DATAQ[NWQ] ~ BBNQ;                                            00050300
          GO TO L1000;                                                  00050400
L97:      WRITE(LINE,FL4001,LIST1);                                     00050500
          GO TO L991;                                                   00050600
L98:      NAAQ ~ NBQ;                                                   00050700
L99:      WRITE(LINE,FL4000,LIST2);                                     00050800
L991:     WRITE(LINE,FL4002);                                           00050900
L1000:    END;                                                          00051000
          GO TO DUMMY;                                                  00051100
DUMMY:    END;                                                          00051200
                                                                        00051300
COMMENT **************************  TPWD  ****************;             00051400
                                                                        00051500
COMMENT **************************  VFCHCK  ****************;           00051600
PROCEDURE VFCHCKQ(NVFQ);                                                00051700
       INTEGER NVFQ;                                                    00051800
BEGIN                                                                   00051900
FORMAT FL4000(/" ",X23,                                                 00052000
         "NUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFIE",        00052100
         "D, ASSUMED TO BE 1.");                                        00052200
LABEL DUMMY,L10,L50,L20;                                                00052300
          IF NVFQ>0 THEN GO TO L20;                                     00052400
L10:      WRITE(LINE,FL4000);                                           00052500
          NVFQ ~ 1;                                                     00052600
L50:      GO TO DUMMY;                                                  00052700
L20:      IF NVFQ>10 THEN GO TO L10 ELSE GO TO DUMMY;                   00052800
DUMMY:    END;                                                          00052900
                                                                        00053000
COMMENT **************************  MAINPRO  ****************;          00053100
PROCEDURE MAINPRO;                                                      00053200
BEGIN                                                                   00053300
  DEFINE  LHQ     =COMMON[1]#;                                          00053400
  DEFINE  IERRORQ =COMMON[2]#;                                          00053500
  DEFINE  NVARQ   =COMMON[3]#;                                          00053600
  DEFINE  NTGQ    =COMMON[4]#;                                          00053700
  DEFINE  NCASEQ  =COMMON[5]#;                                          00053800
REAL ARRAY  ARRAY1[0:1];                                                00053900
  DEFINE  IA1Q    =ARRAY1[1]#;                                          00054000
  DEFINE  A123Q   =ARRAY1[1]#;                                          00054100
REAL ARRAY  ARRAY2[0:1];                                                00054200
  DEFINE  IOUTQ   =ARRAY2[1]#;                                          00054300
  DEFINE  OUTQ    =ARRAY2[1]#;                                          00054400
REAL ARRAY  ARRAY3[0:1];                                                00054500
  DEFINE  IP1Q    =ARRAY3[1]#;                                          00054600
  DEFINE  P123Q   =ARRAY3[1]#;                                          00054700
REAL ARRAY  ARRAY4[0:1];                                                00054800
  DEFINE  A421Q   =ARRAY4[1]#;                                          00054900
  DEFINE  IA6Q    =ARRAY4[1]#;                                          00055000
REAL ARRAY  ARRAY5[0:1];                                                00055100
  DEFINE  A126Q   =ARRAY5[1]#;                                          00055200
  DEFINE  IA4Q    =ARRAY5[1]#;                                          00055300
REAL ARRAY  ARRAY6[0:1];                                                00055400
  DEFINE  A124Q   =ARRAY6[1]#;                                          00055500
  DEFINE  IA2Q    =ARRAY6[1]#;                                          00055600
REAL ARRAY  ARRAY7[0:1];                                                00055700
  DEFINE  A521Q   =ARRAY7[1]#;                                          00055800
  DEFINE  IA7Q    =ARRAY7[1]#;                                          00055900
REAL ARRAY  ARRAY8[0:1];                                                00056000
  DEFINE  A321Q   =ARRAY8[1]#;                                          00056100
  DEFINE  IA5Q    =ARRAY8[1]#;                                          00056200
REAL ARRAY  ARRAY9[0:1];                                                00056300
  DEFINE  A125Q   =ARRAY9[1]#;                                          00056400
  DEFINE  IA3Q    =ARRAY9[1]#;                                          00056500
REAL ARRAY  ARRAY10[0:1];                                               00056600
  DEFINE  KREPQ   =ARRAY10[1]#;                                         00056700
  DEFINE  REPQ    =ARRAY10[1]#;                                         00056800
OWN INTEGER ARRAY   NSAMQ[0:99],  LLLQ[0:35],  KKKQ[0:36],              00056900
        NAMESQ[0:36],  INDEX1Q[0:36],  INDEX2Q[0:36];                   00057000
OWN REAL ARRAY   SLQ[0:26],  FMTQ[0:120],  X[0:36],                     00057100
        SSVARQ[0:36,0:99],  TOTXXQ[0:36,0:36],  TOTXYQ[0:36],           00057200
        TREXXQ[0:36,0:36],  TREXYQ[0:36],  ERRXXQ[0:36,0:36],           00057300
        ERRXYQ[0:36],  B[0:35],  C[0:35],  D[0:35],                     00057400
        SEBQ[0:35],  SECQ[0:35],  SEDQ[0:35],  ADYQ[0:99],              00057500
        SEADYQ[0:99];                                                   00057600
INTEGER  DX1;                                                           00057700
OWN INTEGER  AVNSKIP, AVLSKIP, AVMUD;                                   00057800
OWN INTEGER ITAPEQ, IT3Q, NTAPEQ, KODEQ, NGRPSQ, NCARDQ, NCOVQ, NADDQ,  00057900
       NDVQ, INAMQ, LYNNQ, MTAPEQ, KVFQ, I, NVAR1Q, LRQ, J, NCARDSQ,    00058000
       N2Q, N1Q, NCRDSQ, K1Q, NNQ, NCOQ, KKQ, JJQ, JXQ, N3Q, MESSQ,     00058100
       IIQ, LGQ, M, K, N, L, NTQ, LTQ, N4Q, N5Q, N6Q;                   00058200
OWN REAL CODEQ, BIGQ, T, TOTYYQ, TREYYQ, ERRYYQ, SSDTREQ, SSDTOTQ,      00058300
       SSDERRQ, SSATREQ, SSATOTQ, SSAERRQ, TESTQ, DN4Q, DN5Q, DN6Q,     00058400
       DN1Q, SMATREQ, SMAERRQ, ADJTESQ, FTESTQ, SMATOTQ, T1Q;           00058500
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00058600
          REMOVE, TPWD, VFCHCK, GRPSEL, TG41, PATTY2, MATINV;           00058700
FORMAT FL102(" BMD04V-ANALYSIS OF COVARIANCE-MULTIPLE COVARIATES -",    00058800
         " VERSION OF JUNE 17, 1964",                                   00058900
               /"UNIVERSITY OF DENVER MODIFIED VERSION, W.H.E. AND ",   00059000
                "J.L.H., MARCH, 1965"//),                               00059100
       FL100(2A6,I2,I5,2I2,I3,I2,A3,I3,A3,I2,X27,3I2),                  00059200
               FL403("PROBLEM CARD INCORRECTLY ORDERED OR PUNCHED."),   00059300
       FL103(" PROBLEM CODE",20("."),A6/                                00059400
         " NUMBER OF TREATMENT GROUPS......",I6/                        00059500
         " TOTAL NUMBER OF CASES",11("."),I6/                           00059600
         " NUMBER OF CARDS PER CASE",8("."),I6/                         00059700
         " NUMBER OF ENTERING VARIABLES....",I6/                        00059800
         " NUMBER OF VARIABLES ADDED",7("."),I6/                        00059900
         " INDEX OF THE DEPENDENT VARIABLE.",I6/" PRINT DATA INPUT",16( 00060000
         "."),X3,A3/" NUMBER OF VARIABLES NAMED",7("."),I6/             00060100
         " USE DATA CARDS OF LAST PROBLEM..",X3,A3/                     00060200
         " NUMBER OF GROUP DEFINITION CARDS",I6/                        00060300
         " NUMBER OF TRANSGENERATION CARDS.",I6/                        00060400
         " SPECIFIED BCD TAPE NUMBER.......",I6/                        00060500
         " NUMBER OF VARIABLE FORMAT CARDS.",I6),                       00060600
       FL104(" TRANS-GENERATION CARDS"//),                              00060700
       FL105(" CARD NEW  TRANS OLD    CONST."/                          00060800
         "  NO. VAR.  CODE VAR(A) OR VAR(B)"//),                        00060900
       FL106(A6,I3,I2,I3,I6  ),                                         00061000
       FL107(I4,I5,I6,I5,X4,R9.4),                                      00061100
       FL150(/" ",X29,"TRANSGENERATION CARD",I3,                        00061200
         " HAS INVALID CODE FOR THIS PROGRAM."),                        00061300
       FL110(/" GROUP DEFINITION CARDS"),                               00061400
       FL111(A6,I2,6(I2,A2,R5.0,A1)),                                   00061500
       FL112(//" A CASE IS IN GROUP ",I2," IF,"),                       00061600
       FL113(" VAR(",I2,") IS ",A2," ",R9.4," ",A1),                    00061700
       FL108(A6,22I3),                                                  00061800
       FL101(A3,X3,7(I4,A6)),                                           00061900
       FL109(12A6),                                                     00062000
       FL114(13A6,A2),                                                  00062100
       FL151(A6,33I2),                                                  00062200
       FL115(/"     GROUP NO.      SAMPLE"/" SPECIFIED ASSIGNED* SIZE"/ 00062300
         /),                                                            00062400
       FL116(X5,I2,X7,I2,X4,I5),                                        00062500
       FL415(//" DATA INPUT (LAST VALUE FOR EACH CASE IS TREATMENT G",  00062600
         "ROUP NUMBER)"),                                               00062700
       FL418(/" CASE NUMBER ",I5),                                      00062800
       FL250(" ",10R11.4),                                              00062900
       FL117(///" VARIABLE MEANS FOR EACH GROUP"//),                    00063000
       FL888(" X(",I2,")"),                                             00063100
       FL777(A6),                                                       00063200
       FL118(/" GROUP NO. ",I2//),                                      00063300
       FL119(6(" (",A6,X1,R10.4,")")/),                                 00063400
       FL120(//" SUMS OF SQUARES AND CROSS PRODUCT MATRIX"),            00063500
       FMT1(40I2),                                                      00063600
       FL121(" FOR TOTAL."//),                                          00063700
       FL122(" FOR TREATMENT."//),                                      00063800
       FL123(" FOR ERROR."//),                                          00063900
       FL124(//" INVERSE OF THE COVARIATES"/" CROSS PRODUCT MATRIX"),   00064000
       FL125(" ",X35,"ANALYSIS OF COVARIANCE TABLE"/),                  00064100
       FL126(" ",93("*")),                                              00064200
       FL127(" *",X11,"I",X7,"I",3(X15,"I"),X7,"I",X15,"*"),            00064300
       FL128(" *",X11,"I",X7,"I",X15,                                   00064400
         "I   SUM-SQUARES I   SUM-SQUARES I",X7,"I",X15,"*"),           00064500
       FL129(" *   SOURCE  I   DF  I       YY      I     (DUE)    ",    00064600
         " I    (ABOUT)    I   DF  I   MEAN-SQUARE *"),                 00064700
       FL130(" *",91("-"),"*"),                                         00064800
       FL131(" * TREATMENT I",X7,"I",3(X15,"I"),X7,"I",X15,"*"),        00064900
       FL132(" * (BETWEEN) I ",I5," ","I ",R13.4," ",2("I ",X13," "),   00065000
         "I ",X5," I ",X13," *"),                                       00065100
       FL133(" *     ERROR I",X7,"I",3(X15,"I"),X7,"I",X15,"*"),        00065200
       FL134(" *  (WITHIN) I ",I5,X1,3("I ",R13.4," "),"I ",I5," I ",   00065300
         R13.4," *"),                                                   00065400
       FL136(" * + ERROR   I",X7,"I",3(X15,"I"),X7,"I",X15,"*"),        00065500
       FL137(" *   (TOTAL) I ",I5,X1,3("I ",R13.4," "),"I",I6," I",X15, 00065600
         "*"),                                                          00065700
       FL138(" *DIFFERENCE FOR TESTING ADJUSTED TREATMENT MEANS...",    00065800
         ".I ",R13.4," I ",I5," I ",R13.4," *"),                        00065900
       FL139(///" NULL HYPOTHESIS. NO DIFFERENCE AMONG TREATMENTS AFT", 00066000
         "ER"/X18,"ADJUSTING WITH COVARIATES."//),                      00066100
       FL140(X18,"F(",I5,",",I5,")=",R8.3//),                           00066200
       FL135(//" TABLE OF COEFFICIENTS"//),                             00066300
       FL142(" ",X16,"TREATMENT (BETWEEN)"),                            00066400
       FL152(" ",X8,"COEFFICIENT"//),                                   00066500
       FL144(" ",A6,3(X1,R13.4)),                                       00066600
       FL141(//" TABLE OF COEFFICIENTS,STANDARD ERRORS AND COMPUTED ",  00066700
         "T-VALUES"//),                                                 00066800
       FL145(" ",X17,"ERROR (WITHIN)"),                                 00066900
       FL143(" ",X8,"COEFFICIENT   STAND. ERROR      T-VALUE"//),       00067000
       FL146(" ",X11,"TREATMENT+ERROR (TOTAL)"),                        00067100
       FL147(" TABLE OF ADJUSTED MEANS AND STANDARD ERRORS"//),         00067200
       FL148(" TREATMENT  TREATMENT    ADJUSTED    SE ADJUSTED"/        00067300
         "   NO.",X7,"MEAN",X9,"MEAN",X9,"MEAN"/),                      00067400
       FL149(" ",I5,R16.4,2R13.4);                                      00067500
LIST LIST1(KODEQ,CODEQ,NGRPSQ,NCASEQ,NCARDQ,NCOVQ,NADDQ,NDVQ,OUTQ,INAMQ,00067600
               KREPQ,LYNNQ,NTGQ,MTAPEQ,KVFQ),                           00067700
     LIST2(CODEQ,NGRPSQ,NCASEQ,NCARDQ,NCOVQ,NADDQ,NDVQ,OUTQ,INAMQ,REPQ, 00067800
       LYNNQ,NTGQ,MTAPEQ,KVFQ),                                         00067900
     LIST3(KODEQ,NEWQ[I],JUMPQ[I],NAQ[I],BNQ[I]),                       00068000
     LIST4(I,NEWQ[I],JUMPQ[I],NAQ[I],BNQ[I]),                           00068100
     LIST5(I),                                                          00068200
     LIST6(KODEQ,NGQ[I],FOR DX1 ~ 1 STEP 1 UNTIL 6 DO [NSUBQ[DX1,       00068300
       I],RELQ[DX1,I],CONTQ[DX1,I],OPERQ[DX1,I]]),                      00068400
     LIST7(NGQ[I]),                                                     00068500
     LIST8(NSUBQ[J,I],RELQ[J,I],CONTQ[J,I],OPERQ[J,I]),                 00068600
     LIST9(KODEQ,FOR DX1 ~ N1Q STEP 1 UNTIL N2Q DO NSAMQ[DX1]),         00068700
     LIST10(KODEQ,FOR DX1 ~ N1Q STEP 1 UNTIL N2Q DO [INDEX1Q[DX1],      00068800
       KKKQ[DX1]]),                                                     00068900
     LIST11(FOR DX1 ~ 1 STEP 1 UNTIL KVFQ DO FMTQ[DX1]),                00069000
     LIST12(FOR DX1 ~ 1 STEP 1 UNTIL 14 DO X[DX1]),                     00069100
     LISTT13(FOR DX1 ~ 1 STEP 1 UNTIL NVARQ DO DATAQ[DX1]),             00069200
LISTT14(FOR DX1 ~ 1 STEP 1 UNTIL NVARQ DO NN[DX1]),                     00069300
     LIST13(FOR DX1 ~ 1 STEP 1 UNTIL NVARQ DO DATAQ[DX1],DATAQ[100]),   00069400
     LIST14(KODEQ,NCOQ),                                                00069500
     LIST15(KODEQ),                                                     00069600
     LIST16(I,NSAMQ[I],NGQ[I]),                                         00069700
     LIST17(KODEQ,NCOQ,FOR DX1 ~ N1Q STEP 1 UNTIL N3Q DO INDEX2Q[DX1]), 00069800
     LIST18(FOR DX1 ~ 1 STEP 1 UNTIL LRQ DO DATAQ[DX1],DATAQ[100]),     00069900
     LIST19(MESSQ),                                                     00070000
     LIST20(NAMESQ[LTQ]),                                               00070100
     LIST21(FOR DX1 ~ 1 STEP 1 UNTIL NVARQ DO [NAMESQ[DX1],SSVARQ[DX1,  00070200
       I]]),                                                            00070300
     LIST22(N1Q,TREYYQ),                                                00070400
     LIST23(N2Q,ERRYYQ,SSDERRQ,SSAERRQ,N5Q,SMAERRQ),                    00070500
     LIST24(N3Q,TOTYYQ,SSDTOTQ,SSATOTQ,N6Q),                            00070600
     LIST25(TESTQ,N1Q,ADJTESQ),                                         00070700
     LIST26(N1Q,N5Q,FTESTQ),                                            00070800
     LIST27(NAMESQ[I],D[I]),                                            00070900
     LIST28(NAMESQ[I],B[I],SEBQ[I],T),                                  00071000
     LIST29(NAMESQ[I],C[I],SECQ[I],T),                                  00071100
     LIST30(I,SSVARQ[NVARQ,I],ADYQ[I],SEADYQ[I]);                       00071200
BEGIN                                                                   00071300
LABEL L999,L2,L4,L1,L401,L400,L4115,L411,L10,L21,L14,L1430,L15,L512,    00071400
        L17,L37,L26,L299,L31,L30,L28,L40,L43,L252,L463,L466,L468,       00071500
        L470,L475,L417,L490,L49,L50,L625,L63;                           00071600
SWITCH ASSIGN~L50,L49,L466,L490,L468,L417;                              00071700
          ITAPEQ ~ 2;                                                   00071800
          REWIND(TAPE2);                                                00071900
          IT3Q ~ 3;                                                     00072000
          NTAPEQ ~ 5;                                                   00072100
          A123Q ~ (+"YES");                                             00072200
          A124Q ~ (+"FINISH");                                          00072300
          A125Q ~ (+"SAMSIZ");                                          00072400
          A126Q ~ (+"GRPDEF");                                          00072500
          A321Q ~ (+"TRNGEN");                                          00072600
          A421Q ~ (+"CVRSEL");                                          00072700
          A521Q ~ (+"LAB");                                             00072800
          P123Q ~ (+"PROBLM");                                          00072900
L999:     READ(CARD,FL100,LIST1)[FINIS];                                00073000
               BADCARDFLAG~FALSE;                                       00073100
          IF KODEQ!IA2Q THEN GO TO L1;                                  00073200
L2:       IF NTAPEQ{5 THEN GO TO L4;                                    00073300
L4:       GO TO FINIS;                                                  00073400
L1:       IF KODEQ=IP1Q THEN GO TO L400;                                00073500
L401:     WRITE(LINE,FL403);                                            00073600
          GO TO L2;                                                     00073700
L400:                                                                   00073800
               WRITE(LINE[PAGE]);                                       00073900
DATELINE("BMD04V");                                                     00074000
          WRITE(LINE,FL102);                                            00074100
BEGIN REAL DUPE; DUPE ~ 1;                                              00074200
          WRITE(LINE,FL103,LIST2);                                      00074300
          IF (NADDQ+98)|(NADDQ-35)}0 THEN                               00074400
               BADCARD(0);                                              00074500
               IF (NCOVQ+NADDQ)|(NCOVQ+NADDQ-100)}0 THEN                00074600
               BADCARD(1);                                              00074700
          IF (MTAPEQ-ITAPEQ)|(MTAPEQ-IT3Q)=0 THEN                       00074800
               BADCARD(2);                                              00074900
          NVARQ ~ INT(NCOVQ);                                           00075000
          REWIND(TAPE3);                                                00075100
          IERRORQ ~ INT(0);                                             00075200
          FOR I ~ 1, I+1 STEP 1 UNTIL 99 DO BEGIN                       00075300
            DATAQ[I] ~ 0  END;                                          00075400
          FOR I ~ 1, I+1 STEP 1 UNTIL 36 DO BEGIN                       00075500
            NAMESQ[I] ~ 0;                                              00075600
            INDEX1Q[I] ~ 0;                                             00075700
            INDEX2Q[I] ~ 0  END;                                        00075800
          LHQ ~ INT(NVARQ+1+NADDQ);                                     00075900
          NVAR1Q ~ LHQ-1;                                               00076000
          LRQ ~ NVARQ;                                                  00076100
               IF LHQ}101 THEN BADCARD(1);                              00076200
               IF 2>LHQ THEN BADCARD(3);                                00076300
          VFCHCKQ(KVFQ);                                                00076400
          AVNSKIP ~ 1;                                                  00076500
          IF (XPR ~ NTGQ)=0 THEN GO TO L10;                             00076600
          IF XPR<0 THEN                                                 00076700
               BADCARD(4);                                              00076800
          AVNSKIP ~ 2;                                                  00076900
          IF NTGQ>99 THEN                                               00077000
               BADCARD(4);                                              00077100
          WRITE(LINE,FL104);                                            00077200
          WRITE(LINE,FL105);                                            00077300
          END;                                                          00077400
          FOR I ~ 1, I+1 STEP 1 UNTIL NTGQ DO BEGIN                     00077500
            READ(CARD,FL106,LIST3)[FINIS];                              00077600
            IF KODEQ!IA5Q THEN                                          00077700
               BADCARD(5);                                              00077800
            WRITE(LINE,FL107,LIST4);                                    00077900
            IF (JUMPQ[I]-18)|(JUMPQ[I]-19)|(JUMPQ[I]-40)=0 THEN         00078000
              GO TO L4115;                                              00078100
            IF (XPR ~ JUMPQ[I]-41)>0 THEN GO TO L4115;                  00078200
            IF XPR=0 THEN GO TO L411;                                   00078300
            IF (JUMPQ[I]-25)|(39-JUMPQ[I])<0 THEN GO TO L411;           00078400
L4115:      WRITE(LINE,FL150,LIST5);                                    00078500
            LYNNQ ~ -99;                                                00078600
L411:       END;                                                        00078700
L10:      IF (XPR ~ LYNNQ)=0 THEN GO TO L14;                            00078800
          IF XPR<0 THEN                                                 00078900
               BADCARD(6);                                              00079000
          WRITE(LINE,FL110);                                            00079100
          FOR I ~ 1, I+1 STEP 1 UNTIL LYNNQ DO BEGIN                    00079200
            READ(CARD,FL111,LIST6)[FINIS];                              00079300
            IF KODEQ!IA4Q THEN                                          00079400
               BADCARD(7);                                              00079500
            WRITE(LINE,FL112,LIST7);                                    00079600
            FOR J ~ 1, J+1 STEP 1 UNTIL 6 DO BEGIN                      00079700
              WRITE(LINE,FL113,LIST8);                                  00079800
              IF OPERQ[J,I]=(+"*") THEN GO TO L21;                      00079900
              END;                                                      00080000
L21:        END;                                                        00080100
          GO TO L15;                                                    00080200
L14:      NCARDSQ ~ (NGRPSQ+21) DIV 22;                                 00080300
          N2Q ~ 0;                                                      00080400
          FOR J ~ 1, J+1 STEP 1 UNTIL NCARDSQ DO BEGIN                  00080500
            N1Q ~ N2Q+1;                                                00080600
            N2Q ~ N2Q+22;                                               00080700
            IF 99}N2Q THEN GO TO L1430;                                 00080800
            N2Q ~ 99;                                                   00080900
L1430:      READ(CARD,FL108,LIST9)[FINIS];                              00081000
            IF KODEQ!IA3Q THEN                                          00081100
               BADCARD(8);                                              00081200
            END;                                                        00081300
L15:      IF (XPR ~ INAMQ)=0 THEN GO TO L17;                            00081400
          IF XPR<0 THEN                                                 00081500
               BADCARD(9);                                              00081600
          NCRDSQ ~ (INAMQ+6) DIV 7;                                     00081700
          N2Q ~ 0;                                                      00081800
          FOR I ~ 1, I+1 STEP 1 UNTIL NCRDSQ DO BEGIN                   00081900
            N1Q ~ N2Q+1;                                                00082000
            N2Q ~ N2Q+7;                                                00082100
            IF 36}N2Q THEN GO TO L512;                                  00082200
            N2Q ~ 36;                                                   00082300
L512:       READ(CARD,FL101,LIST10)[FINIS];                             00082400
            IF KODEQ!IA7Q THEN                                          00082500
               BADCARD(10);                                             00082600
            END;                                                        00082700
L17:      KVFQ ~ KVFQ|12;                                               00082800
READ(CARD,FMT1,FOR I ~ 1 STEP 1 UNTIL NVARQ DO SCALE[I]);               00082900
          IF KREPQ=IA1Q THEN GO TO L37;                                 00083000
          K1Q ~ NCARDQ|NCASEQ;                                          00083100
               READ(CARD[NO], CHKFOR, FORMOT)[FINIS];                   00083200
               IF FORMOT ! "FORMAT"  THEN                               00083300
          BEGIN                                                         00083400
               WRITE(LINE, NOFRM);                                      00083500
               GO TO FINIS;                                             00083600
          END;                                                          00083700
               OBJECTFMTGEN(FMT2);                                      00083800
          FOR I ~ 1, I+1 STEP 1 UNTIL K1Q DO BEGIN                      00083900
          READ(CARD,FL114,LIST12)[FINIS];                               00084000
          WRITE(TAPE3,FL114,LIST12);                                    00084100
            END;                                                        00084200
REWIND(TAPE3);                                                          00084300
L37:      NTAPEQ ~ IT3Q;                                                00084400
          IF LYNNQ>0 THEN GO TO L26;                                    00084500
          FOR I ~ 1, I+1 STEP 1 UNTIL NGRPSQ DO BEGIN                   00084600
            NNQ ~ NSAMQ[I];                                             00084700
            FOR J ~ 1, J+1 STEP 1 UNTIL NNQ DO BEGIN                    00084800
READ(TAPE3,FMT2,LISTT14);                                               00084900
FOR Z ~ 1, Z + 1 STEP 1 UNTIL NVARQ DO                                  00085000
BEGIN                                                                   00085100
IF SCALE [Z] = 0 THEN DATAQ[Z] ~ NN[Z];                                 00085200
IF SCALE [Z] < 0 THEN DATAQ[Z] ~ NN[Z]|10*ABS(SCALE[Z])                 00085300
ELSE DATAQ[Z] ~ NN[Z]/(10*SCALE[Z]);                                    00085400
END;                                                                    00085500
COMMENT  SEE LINE 463 OF FORTRAN LISTING;                               00085600
              DATAQ[100] ~ I;                                           00085700
          WRITE(TAPE2,OKTL,LIST13);                                     00085800
              END;                                                      00085900
            END;                                                        00086000
          GO TO L28;                                                    00086100
L26:      FOR I ~ 1, I+1 STEP 1 UNTIL 99 DO BEGIN                       00086200
            NSAMQ[I] ~ 0  END;                                          00086300
          FOR I ~ 1, I+1 STEP 1 UNTIL NCASEQ DO BEGIN                   00086400
            DATAQ[100] ~ 0;                                             00086500
READ(TAPE3,FMT2,LISTT14);                                               00086600
FOR Z ~ 1, Z + 1 STEP 1 UNTIL NVARQ DO                                  00086700
BEGIN                                                                   00086800
IF SCALE [Z] = 0 THEN DATAQ[Z] ~ NN[Z];                                 00086900
IF SCALE [Z] < 0 THEN DATAQ[Z] ~ NN[Z]|10*ABS(SCALE[Z])                 00087000
ELSE DATAQ[Z] ~ NN[Z]/(10*SCALE[Z]);                                    00087100
END;                                                                    00087200
COMMENT  SEE LINE 472 OF FORTRAN LISTING;                               00087300
            FOR J ~ 1, J+1 STEP 1 UNTIL LYNNQ DO BEGIN                  00087400
              GRPSELQ(J);                                               00087500
COMMENT  SEE LINE 475 OF FORTRAN LISTING;                               00087600
              IF J>0 THEN GO TO L299;                                   00087700
              READ(CARD,FL151,LIST14)[FINIS];                           00087800
          REWIND(TAPE2);                                                00087900
              IF 33>NCOQ THEN GO TO L999;                               00088000
              READ(CARD,FL151,LIST15)[FINIS];                           00088100
              GO TO L999;                                               00088200
L299:         IF DATAQ[100]{0 THEN GO TO L31;                           00088300
              KKQ ~ NGQ[J];                                             00088400
              NSAMQ[KKQ] ~ NSAMQ[KKQ]+1;                                00088500
          WRITE(TAPE2,OKTL,LIST13);                                     00088600
              GO TO L30;                                                00088700
L31:          END;                                                      00088800
L30:        END;                                                        00088900
L28:      REWIND(TAPE2);                                                00089000
          IF LYNNQ{0 THEN GO TO L43;                                    00089100
          JJQ ~ 0;                                                      00089200
          FOR I ~ 1, I+1 STEP 1 UNTIL 99 DO BEGIN                       00089300
            NGQ[I] ~ 0  END;                                            00089400
          FOR I ~ 1, I+1 STEP 1 UNTIL 99 DO BEGIN                       00089500
            IF NSAMQ[I]{0 THEN GO TO L40;                               00089600
            JJQ ~ JJQ+1;                                                00089700
            NGQ[JJQ] ~ NSAMQ[I];                                        00089800
            NSAMQ[I] ~ JJQ;                                             00089900
L40:        END;                                                        00090000
          JXQ ~ MAX(JJQ,NGRPSQ);                                        00090100
          NGRPSQ ~ JJQ;                                                 00090200
          WRITE(LINE,FL115);                                            00090300
          FOR I ~ 1, I+1 STEP 1 UNTIL JXQ DO BEGIN                      00090400
            WRITE(LINE,FL116,LIST16);                                   00090500
            END;                                                        00090600
          GO TO L252;                                                   00090700
L43:      FOR I ~ 1, I+1 STEP 1 UNTIL NGRPSQ DO BEGIN                   00090800
            NGQ[I] ~ NSAMQ[I]  END;                                     00090900
L252:     NVARQ ~ INT(NVARQ+NADDQ);                                     00091000
          NCOVQ ~ NCOVQ+NADDQ-1;                                        00091100
               FOR I~0 STEP 1 UNTIL 36 DO                               00091200
               FOR J~0 STEP 1 UNTIL 36 DO                               00091300
               TOTXXQ[I,J]~0;                                           00091400
               FOR I~0 STEP 1 UNTIL 36 DO                               00091500
               FOR J~0 STEP 1 UNTIL 36 DO                               00091600
               TREXXQ[I,J]~0;                                           00091700
               FOR I~0 STEP 1 UNTIL 36 DO                               00091800
               FOR J~0 STEP 1 UNTIL 36 DO                               00091900
               ERRXXQ[I,J]~0;                                           00092000
               FOR I~0 STEP 1 UNTIL 36 DO                               00092100
               FOR J~0 STEP 1 UNTIL 99 DO                               00092200
               SSVARQ[I,J]~0;                                           00092300
               FOR I~0 STEP 1 UNTIL 36 DO                               00092400
          BEGIN                                                         00092500
               TOTXYQ[I]~0;                                             00092600
               TREXYQ[I]~0;                                             00092700
               ERRXYQ[I]~0;                                             00092800
          END;                                                          00092900
          AVLSKIP ~ 3;                                                  00093000
          AVMUD ~ 4;                                                    00093100
          N1Q ~ 1;                                                      00093200
          N3Q ~ 32;                                                     00093300
L463:     READ(CARD,FL151,LIST17)[FINIS];                               00093400
               IF KODEQ!IA6Q THEN BADCARD(11);                          00093500
               IF NCOQ>35 THEN BADCARD(12);                             00093600
               IF NGRPSQ+NCOQ+1>NCASEQ THEN BADCARD(13);                00093700
          GO TO ASSIGN[AVLSKIP];                                        00093800
L466:     IF 33>NCOQ THEN GO TO L470;                                   00093900
          AVLSKIP ~ 5;                                                  00094000
          N1Q ~ 33;                                                     00094100
          N3Q ~ NCOQ;                                                   00094200
          GO TO L463;                                                   00094300
L468:     INDEX2Q[36] ~ INDEX2Q[35];                                    00094400
          INDEX2Q[35] ~ INDEX2Q[34];                                    00094500
          INDEX2Q[34] ~ INDEX2Q[33];                                    00094600
          INDEX2Q[33] ~ NCOQ;                                           00094700
          NCOQ ~ N3Q;                                                   00094800
L470:          IF BADCARDFLAG THEN GO TO L999;                          00094900
COMMENT        THE FOLLOWING TWO CARDS ADDED 3-10-65 TO ALLOW USE OF    00095000
               ONLY THE SPECIFIED COVARIATES;                           00095100
                 NCOVQ~NCOQ;                                            00095200
                 NVARQ~NCOQ+1;                                          00095300
          INDEX2Q[NVARQ] ~ NDVQ;                                        00095400
          IF IOUTQ!IA1Q THEN GO TO L475;                                00095500
          AVMUD ~ 6;                                                    00095600
          MESSQ ~ 0;                                                    00095700
          WRITE(LINE,FL415);                                            00095800
L475:     FOR IIQ ~ 1, IIQ+1 STEP 1 UNTIL NGRPSQ DO BEGIN               00095900
            LGQ ~ NGQ[IIQ];                                             00096000
            FOR M ~ 1, M+1 STEP 1 UNTIL LGQ DO BEGIN                    00096100
          READ(TAPE2,OKTL,LIST18);                                      00096200
              GO TO ASSIGN[AVMUD];                                      00096300
L417:         MESSQ ~ MESSQ+1;                                          00096400
              WRITE(LINE,FL418,LIST19);                                 00096500
              WRITE(LINE,FL250,LIST18);                                 00096600
L490:         GO TO ASSIGN[AVNSKIP];                                    00096700
L49:          TG41Q(IIQ,M);                                             00096800
L50:          K ~ DATAQ[100];                                           00096900
              FOR J ~ 1, J+1 STEP 1 UNTIL NVARQ DO BEGIN                00097000
                I ~ INDEX2Q[J];                                         00097100
                SSVARQ[J,K] ~ SSVARQ[J,K]+DATAQ[I];                     00097200
                FOR N ~ 1, N+1 STEP 1 UNTIL NVARQ DO BEGIN              00097300
                  L ~ INDEX2Q[N];                                       00097400
                  TOTXXQ[N,J] ~ TOTXXQ[N,J]+(DATAQ[I]|DATAQ[            00097500
                    L]);                                                00097600
                  END;                                                  00097700
                END;                                                    00097800
              END;                                                      00097900
            END;                                                        00098000
          BEGIN                                                         00098100
INTEGER        DUMMY;                                                   00098200
          REWIND(TAPE2);                                                00098300
          NTQ ~ 0;                                                      00098400
          FOR I ~ 1, I+1 STEP 1 UNTIL NGRPSQ DO BEGIN                   00098500
            NTQ ~ NTQ+NGQ[I];                                           00098600
            END;                                                        00098700
          FOR J ~ 1, J+1 STEP 1 UNTIL NVARQ DO BEGIN                    00098800
            X[J] ~ 0;                                                   00098900
            FOR I ~ 1, I+1 STEP 1 UNTIL NGRPSQ DO BEGIN                 00099000
              X[J] ~ X[J]+SSVARQ[J,I]  END;                             00099100
            END;                                                        00099200
          BIGQ ~ NTQ;                                                   00099300
          FOR I ~ 1, I+1 STEP 1 UNTIL NVARQ DO BEGIN                    00099400
            FOR J ~ 1, J+1 STEP 1 UNTIL NVARQ DO BEGIN                  00099500
              TOTXXQ[J,I] ~ TOTXXQ[J,I]-(X[I]|X[J])/BIGQ;               00099600
              FOR K ~ 1, K+1 STEP 1 UNTIL NGRPSQ DO BEGIN               00099700
                T ~ NGQ[K];                                             00099800
                TREXXQ[J,I] ~ TREXXQ[J,I]+(SSVARQ[I,K]|SSVARQ[          00099900
                  J,K])/T;                                              00100000
                END;                                                    00100100
              TREXXQ[J,I] ~ TREXXQ[J,I]-(X[I]|X[J])/BIGQ;               00100200
              ERRXXQ[J,I] ~ TOTXXQ[J,I]-TREXXQ[J,I]  END;               00100300
            END;                                                        00100400
          FOR I ~ 1, I+1 STEP 1 UNTIL NGRPSQ DO BEGIN                   00100500
            D[1] ~ NGQ[I];                                              00100600
            FOR J ~ 1, J+1 STEP 1 UNTIL NVARQ DO BEGIN                  00100700
              SSVARQ[J,I] ~ SSVARQ[J,I]/D[1]  END;                      00100800
            END;                                                        00100900
          END;                                                          00101000
          WRITE(LINE,FL117);                                            00101100
          FOR LTQ ~ 1, LTQ+1 STEP 1 UNTIL NVARQ DO BEGIN                00101200
            I ~ INDEX2Q[LTQ];                                           00101300
            FOR J ~ 1, J+1 STEP 1 UNTIL INAMQ DO BEGIN                  00101400
              IF I=INDEX1Q[J] THEN GO TO L625;                          00101500
              END;                                                      00101600
            WRITE(PUNCH,FL888,LIST5);                                   00101700
            READ(CARD,FL777,LIST20)[FINIS];                             00101800
            GO TO L63;                                                  00101900
L625:       NAMESQ[LTQ] ~ KKKQ[J];                                      00102000
L63:        END;                                                        00102100
          BEGIN                                                         00102200
INTEGER        DUMMY;                                                   00102300
          FOR I ~ 1, I+1 STEP 1 UNTIL NGRPSQ DO BEGIN                   00102400
            WRITE(LINE,FL118,LIST5);                                    00102500
            WRITE(LINE,FL119,LIST21);                                   00102600
            END;                                                        00102700
          WRITE(LINE,FL120);                                            00102800
          WRITE(LINE,FL121);                                            00102900
          PATTY2Q(TOTXXQ,NVARQ,NAMESQ);                                 00103000
          WRITE(LINE,FL120);                                            00103100
          WRITE(LINE,FL122);                                            00103200
          PATTY2Q(TREXXQ,NVARQ,NAMESQ);                                 00103300
          WRITE(LINE,FL120);                                            00103400
          WRITE(LINE,FL123);                                            00103500
          PATTY2Q(ERRXXQ,NVARQ,NAMESQ);                                 00103600
          FOR I ~ 1, I+1 STEP 1 UNTIL NVARQ DO BEGIN                    00103700
            TOTXYQ[I] ~ TOTXXQ[NVARQ,I];                                00103800
            TREXYQ[I] ~ TREXXQ[NVARQ,I];                                00103900
            ERRXYQ[I] ~ ERRXXQ[NVARQ,I]  END;                           00104000
          TOTYYQ ~ TOTXYQ[NVARQ];                                       00104100
          TREYYQ ~ TREXYQ[NVARQ];                                       00104200
          ERRYYQ ~ ERRXYQ[NVARQ];                                       00104300
          MATINVQ(TOTXXQ,NCOVQ,LLLQ,KKKQ);                              00104400
          MATINVQ(TREXXQ,NCOVQ,LLLQ,KKKQ);                              00104500
          MATINVQ(ERRXXQ,NCOVQ,LLLQ,KKKQ);                              00104600
          WRITE(LINE,FL124);                                            00104700
          WRITE(LINE,FL121);                                            00104800
          PATTY2Q(TOTXXQ,NCOVQ,NAMESQ);                                 00104900
          WRITE(LINE,FL124);                                            00105000
          WRITE(LINE,FL122);                                            00105100
          PATTY2Q(TREXXQ,NCOVQ,NAMESQ);                                 00105200
          WRITE(LINE,FL124);                                            00105300
          WRITE(LINE,FL123);                                            00105400
          PATTY2Q(ERRXXQ,NCOVQ,NAMESQ);                                 00105500
          FOR I ~ 1, I+1 STEP 1 UNTIL NCOVQ DO BEGIN                    00105600
            B[I] ~ 0;                                                   00105700
            C[I] ~ 0;                                                   00105800
            D[I] ~ 0;                                                   00105900
            FOR J ~ 1, J+1 STEP 1 UNTIL NCOVQ DO BEGIN                  00106000
              B[I] ~ B[I]+ERRXXQ[J,I]|ERRXYQ[J];                        00106100
              C[I] ~ C[I]+TOTXXQ[J,I]|TOTXYQ[J];                        00106200
              D[I] ~ D[I]+TREXXQ[J,I]|TREXYQ[J]  END;                   00106300
            END;                                                        00106400
          SSDTREQ ~ 0;                                                  00106500
          SSDTOTQ ~ 0;                                                  00106600
          SSDERRQ ~ 0;                                                  00106700
          FOR I ~ 1, I+1 STEP 1 UNTIL NCOVQ DO BEGIN                    00106800
            SSDERRQ ~ SSDERRQ+B[I]|ERRXYQ[I];                           00106900
            SSDTREQ ~ SSDTREQ+D[I]|TREXYQ[I];                           00107000
            SSDTOTQ ~ SSDTOTQ+C[I]|TOTXYQ[I]  END;                      00107100
          END;                                                          00107200
          BEGIN REAL DUMDUM;  DUMDUM ~ 0;                               00107300
          SSDTREQ ~ SSDTOTQ-SSDERRQ;                                    00107400
          SSATREQ ~ TREYYQ-SSDTREQ;                                     00107500
          SSATOTQ ~ TOTYYQ-SSDTOTQ;                                     00107600
          SSAERRQ ~ ERRYYQ-SSDERRQ;                                     00107700
          N1Q ~ NGRPSQ-1;                                               00107800
          N2Q ~ BIGQ;                                                   00107900
          N2Q ~ N2Q-NGRPSQ;                                             00108000
          N3Q ~ BIGQ-1;                                                 00108100
          N4Q ~ N1Q;                                                    00108200
          N5Q ~ N2Q-NCOVQ;                                              00108300
          N6Q ~ N3Q-NCOVQ;                                              00108400
          TESTQ ~ SSATOTQ-SSAERRQ;                                      00108500
          DN4Q ~ N4Q;                                                   00108600
          DN5Q ~ N5Q;                                                   00108700
          DN6Q ~ N6Q;                                                   00108800
          DN1Q ~ N1Q;                                                   00108900
          SMATREQ ~ SSATREQ/DN4Q;                                       00109000
          SMAERRQ ~ SSAERRQ/DN5Q;                                       00109100
          ADJTESQ ~ TESTQ/DN1Q;                                         00109200
          FTESTQ ~ ADJTESQ/SMAERRQ;                                     00109300
          WRITE(LINE,FL125);                                            00109400
          WRITE(LINE,FL126);                                            00109500
          WRITE(LINE,FL127);                                            00109600
          WRITE(LINE,FL128);                                            00109700
          WRITE(LINE,FL129);                                            00109800
          WRITE(LINE,FL127);                                            00109900
          WRITE(LINE,FL130);                                            00110000
          WRITE(LINE,FL127);                                            00110100
          WRITE(LINE,FL131);                                            00110200
          SMATOTQ ~ SSATOTQ/DN6Q;                                       00110300
          WRITE(LINE,FL132,LIST22);                                     00110400
          WRITE(LINE,FL127);                                            00110500
          WRITE(LINE,FL130);                                            00110600
          WRITE(LINE,FL127);                                            00110700
          WRITE(LINE,FL133);                                            00110800
          WRITE(LINE,FL134,LIST23);                                     00110900
          WRITE(LINE,FL127);                                            00111000
          WRITE(LINE,FL130);                                            00111100
          WRITE(LINE,FL127);                                            00111200
          WRITE(LINE,FL131);                                            00111300
          WRITE(LINE,FL136);                                            00111400
          WRITE(LINE,FL137,LIST24);                                     00111500
          WRITE(LINE,FL127);                                            00111600
          WRITE(LINE,FL130);                                            00111700
          WRITE(LINE,FL127);                                            00111800
          WRITE(LINE,FL138,LIST25);                                     00111900
          WRITE(LINE,FL127);                                            00112000
          WRITE(LINE,FL126);                                            00112100
          WRITE(LINE,FL139);                                            00112200
          WRITE(LINE,FL140,LIST26);                                     00112300
          WRITE(LINE,FL135);                                            00112400
          WRITE(LINE,FL142);                                            00112500
          WRITE(LINE,FL152);                                            00112600
          END;                                                          00112700
          FOR I ~ 1, I+1 STEP 1 UNTIL NCOVQ DO BEGIN                    00112800
            WRITE(LINE,FL144,LIST27);                                   00112900
            END;                                                        00113000
          WRITE(LINE,FL141);                                            00113100
          WRITE(LINE,FL145);                                            00113200
          WRITE(LINE,FL143);                                            00113300
          FOR I ~ 1, I+1 STEP 1 UNTIL NCOVQ DO BEGIN                    00113400
            SEBQ[I] ~ SMAERRQ|ERRXXQ[I,I];                              00113500
            SEBQ[I] ~ SQRT(SEBQ[I]);                                    00113600
            T ~ B[I]/SEBQ[I];                                           00113700
            WRITE(LINE,FL144,LIST28);                                   00113800
            END;                                                        00113900
          WRITE(LINE,FL141);                                            00114000
          WRITE(LINE,FL146);                                            00114100
          WRITE(LINE,FL143);                                            00114200
          FOR I ~ 1, I+1 STEP 1 UNTIL NCOVQ DO BEGIN                    00114300
            SECQ[I] ~ SMATOTQ|TOTXXQ[I,I];                              00114400
            SECQ[I] ~ SQRT(SECQ[I]);                                    00114500
            T ~ C[I]/SECQ[I];                                           00114600
            WRITE(LINE,FL144,LIST29);                                   00114700
            END;                                                        00114800
          FOR I ~ 1, I+1 STEP 1 UNTIL NCOVQ DO BEGIN                    00114900
            X[I] ~ 0;                                                   00115000
            FOR J ~ 1, J+1 STEP 1 UNTIL NGRPSQ DO BEGIN                 00115100
              T1Q ~ NGQ[J];                                             00115200
              X[I] ~ X[I]+SSVARQ[I,J]|(T1Q/BIGQ)  END;                  00115300
            END;                                                        00115400
          FOR J ~ 1, J+1 STEP 1 UNTIL NGRPSQ DO BEGIN                   00115500
            T ~ 0;                                                      00115600
            T1Q ~ NGQ[J];                                               00115700
            T1Q ~ 1/T1Q;                                                00115800
            FOR I ~ 1, I+1 STEP 1 UNTIL NCOVQ DO BEGIN                  00115900
              T ~ T+B[I]|(SSVARQ[I,J]-X[I])  END;                       00116000
            ADYQ[J] ~ SSVARQ[NVARQ,J]-T;                                00116100
            FOR I ~ 1, I+1 STEP 1 UNTIL NCOVQ DO BEGIN                  00116200
              FOR K ~ 1, K+1 STEP 1 UNTIL NCOVQ DO BEGIN                00116300
                T1Q ~ T1Q+(SSVARQ[I,J]-X[I])|ERRXXQ[K,I]|(              00116400
                  SSVARQ[K,J]-X[K])  END;                               00116500
              END;                                                      00116600
            SEADYQ[J] ~ SQRT(SMAERRQ|T1Q)  END;                         00116700
          WRITE(LINE,FL147);                                            00116800
          WRITE(LINE,FL148);                                            00116900
          FOR I ~ 1, I+1 STEP 1 UNTIL NGRPSQ DO BEGIN                   00117000
            WRITE(LINE,FL149,LIST30);                                   00117100
            END;                                                        00117200
          GO TO L999;                                                   00117300
          END  END;                                                     00117400
                                                                        00117500
COMMENT **************************  OUTER BLOCK  ************;          00117600
  XPR~QQ~0;                                                             00117700
            SENSL[1]~FALSE;                                             00117800
            SENSL[2]~FALSE;                                             00117900
            SENSL[3]~FALSE;                                             00118000
            SENSL[4]~FALSE;                                             00118100
MAINPRO; FINIS:                                                         00118200
DATELINE(0);                                                            00118300
END OF OUTER BLK.                                                       00118400
                LAST CARD ON CRDIMG TAPE                                99999999
