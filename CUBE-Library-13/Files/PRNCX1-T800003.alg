         BEGIN                                                          PR1 000 
COMMENT        THE CUBE LIBRARY NUMBER IS T800003.  THE PROGRAM NAME IS         
               "PRNCX1/STAT1".  THIS VERSION DATED 9/20/68;                     
COMMENT        PRINCIPAL AXIS FACTOR ANALYSIS FROM CLARKS FACVAR        PR1 001 
               JOHN WACKWITZ 3-20-66;                                   PR1 002 
INTEGER       I,NVAR,NSAM,CAS;                                          PR1 003 
INTEGER        SFMT,INP,  RN,WRT,WRC;                                   PR1 004 
INTEGER ARRAY ID[0:4] ;                                                 PR1 005 
LABEL         START,NOMO,LL1 ;                                          PR1 006 
FORMAT IN      FORM1 (5A6,I3,I5,I3,5I1);                                PR1 007 
FORMAT         CHKFOR (A6);                                             PR1 008 
FORMAT         NOFRM ("ERROR: NO FORMAT SPECIFICATIONS FOR THIS RUN"//);PR1 009 
FORMAT         ERRCNT ("ERROR: WRONG N INDICATED ON HEADER CARD" //);   PR1 010 
LABEL          WRONGCOUNT;                                              PR1 011 
FORMAT         SWF (//////////////////////////////////////////////////  PR1 012 
                    //////////////////////////////////////////////////  PR1 013 
                    //////////////////////////////////////////////////  PR1 014 
                    //////////////////////////////////////////////////);PR1 015 
FORMAT         COF (//////////////////////////////////////////////////  PR1 016 
                    //////////////////////////////////////////////////  PR1 017 
                    //////////////////////////////////////////////////  PR1 018 
                    //////////////////////////////////////////////////);PR1 019 
FORMAT         FCM(100O) ;                                              PR1 020 
FORMAT OUT     TITLE(X45,"PRINCIPAL AXIS FACTOR ANALYSIS"////),         PR1 021 
              FRM1(X45,5A6//X1,"CASE",2I3/X1,"NUMBER OF VARIABLES",I4/  PR1 022 
                    X1,"SAMPLE SIZE",I6),                               PR1 023 
              FRM3 (8F15.5),                                            PR1 024 
              FRM5 (//X1,"CORRELATION COEFFICIENTS"),                   PR1 025 
              FRM6 (/X1,"ROW",I3/(10F12.5)/),                           PR1 026 
              FRM7 (//X1,"EIGENVALUES"),                                PR1 027 
              FRM8 (//X7,"ACCURACY INSUFFICIENT FOR MORE EIGENVALUES"), PR1 028 
              FRM9 (//X1,"CUMULATIVE PROPORTION OF TOTAL VARIANCE"),    PR1 029 
              FRM10(//X1,"EIGENVECTORS"),                               PR1 030 
              FRM11(/X1,"VECTOR",I3/(10F12.5)/),                        PR1 031 
              FRM12(//X1,"FACTOR MATRIX"),                              PR1 032 
              FRM13(/X1,"VARIABLE",I3/(10F12.5)/),                      PR1 033 
              FRM14(//X1,"CHECK MATRIX - EIGENVALUES ON DIAGONAL"),     PR1 034 
              FRM15(//X1,"CHECK MATRIX - CORRELATION COEFFICIENTS");    PR1 035 
FILE IN CORMAT   2(2,100) ;                                             PR1 036 
FILE IN        CARD(2,10);                                              PR1 037 
DEFINE         CARDR=CARD#;                                             PR1 038 
FILE OUT       PRINT  4(2,15);                                          PR1 039 
DEFINE         LINE=PRINT#;                                             PR1 040 
FILE OUT       PUNCH 0(2,10);                                           PR1 041 
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWDPR1 042 
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN RPR1 043 
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAYPR1 044 
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;SPR1 045 
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"PR1 046 
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATTPR1 047 
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(EPR1 048 
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");PR1 049 
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNTPR1 050 
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ERPR1 051 
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLAPR1 052 
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREAPR1 053 
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10WPR1 054 
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CAPR1 055 
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;RPR1 056 
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~PR1 057 
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE PR1 058 
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM PR1 059 
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+3PR1 060 
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;INPR1 061 
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTILPR1 062 
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREAPR1 063 
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,PR1 064 
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABEPR1 065 
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FIPR1 066 
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "TPR1 067 
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"PR1 068 
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+1PR1 069 
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTVPR1 070 
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;PR1 071 
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PROPR1 072 
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],LPR1 073 
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABEPR1 074 
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BEPR1 075 
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACCPR1 076 
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMIPR1 077 
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF EPR1 078 
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASSPR1 079 
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINALPR1 080 
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~PR1 081 
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END OPR1 082 
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER PR1 083 
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALUPR1 084 
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;PR1 085 
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;CPR1 086 
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~PR1 087 
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;EPR1 088 
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FMPR1 089 
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO REPR1 090 
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~LPR1 091 
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZPR1 092 
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LARPR1 093 
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(PR1 094 
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALUPR1 095 
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMBPR1 096 
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<8PR1 097 
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(PR1 098 
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~NPR1 099 
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGINPR1 100 
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,PR1 101 
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,WPR1 102 
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SKPR1 103 
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]PR1 104 
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCEDPR1 105 
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"BPR1 106 
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SIPR1 107 
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN SPR1 108 
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#PR1 109 
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0TPR1 110 
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPEPR1 111 
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEGPR1 112 
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:PR1 113 
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO PR1 114 
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"APR1 115 
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,PR1 116 
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN PR1 117 
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE IPR1 118 
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF ELPR1 119 
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEXPR1 120 
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0TPR1 121 
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,RPR1 122 
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN CODPR1 123 
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCUPR1 124 
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~FPR1 125 
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SKPR1 126 
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END ELPR1 127 
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDEPR1 128 
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END ELPR1 129 
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGINPR1 130 
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(ELPR1 131 
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-PR1 132 
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODEPR1 133 
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"THPR1 134 
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W1PR1 135 
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,RPR1 136 
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERRPR1 137 
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;DPR1 138 
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(PR1 139 
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHRPR1 140 
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[0PR1 141 
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(PPR1 142 
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]PR1 143 
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN PR1 144 
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,PR1 145 
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 PR1 146 
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEAPR1 147 
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",PR1 148 
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"///),LAYT(//"EXECUTION TIMPR1 149 
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///PR1 150 
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=PR1 151 
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAYPR1 152 
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FEPR1 153 
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.PR1 154 
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];ENPR1 155 
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THENPR1 156 
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,PR1 157 
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       PR1 158 
START:   BEGIN                                                          PR1 159 
LIST          HEAD(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],NVAR,NSAM,CAS,     PR1 160 
                 RN,INP,SFMT,WRT,WRC);                                  PR1 161 
              READ (CARDR,FORM1,HEAD) [NOMO] ;                          PR1 162 
              WRITE(PRINT[PAGE]) ;                                      PR1 163 
               DATELINE("PRNCX1");                                      PR1 164 
              WRITE(PRINT,TITLE);                                       PR1 165 
               IF INP < 1 THEN                                          PR1 166 
          BEGIN                                                         PR1 167 
               WRITE(LINE[DBL]);                                        PR1 168 
               READ(CARD[NO], CHKFOR, SFMT)[NOMO];                      PR1 169 
               IF SFMT ! "FORMAT"  THEN                                 PR1 170 
          BEGIN                                                         PR1 171 
               WRITE(LINE, NOFRM);                                      PR1 172 
               GO TO NOMO;                                              PR1 173 
          END  ELSE  OBJECTFMTGEN(SWF);                                 PR1 174 
               WRITE(LINE[DBL]);                                        PR1 175 
          END  ELSE  READ(CORMAT,FCM ,NVAR,NVAR) ;                      PR1 176 
         END ;                                                          PR1 177 
LL1:     BEGIN                                                          PR1 178 
INTEGER       EX ;                                                      PR1 179 
REAL          NS,NSM1;                                                  PR1 180 
LABEL         LL2 ;                                                     PR1 181 
              NS ~ NSAM ;                                               PR1 182 
              NSM1 ~ NS - 1.0 ;                                         PR1 183 
              EX ~ NVAR + 50 ;                                          PR1 184 
LL2:     BEGIN                                                          PR1 185 
INTEGER       J,JJ,K,KK,L,LL,II,M,MM,Z ;                                PR1 186 
INTEGER ARRAY T[0:99];                                                  PR1 187 
REAL          NORM,T1,T2,SYN,KOS,P,TA,TE,BE,W,S,MARY,B ;                PR1 188 
REAL ARRAY                     DATA[0:EX],SCA[0:NVAR,0:NVAR],           PR1 189 
              SUMXX[0:NVAR,0:NVAR],R[0:NVAR,0:NVAR],DIAG[0:NVAR],       PR1 190 
              VAL[0:NVAR],SD[0:NVAR],X[0:NVAR],SUMX[0:NVAR] ;           PR1 191 
LABEL          LCAS6,LCAS6A,                                            PR1 192 
                  LL23,LL24,               LL33,LL34;                   PR1 193 
LIST          OUTT(FOR I ~ 0 STEP 1 UNTIL 4 DO ID[I],CAS,RN,NVAR,NSAM), PR1 194 
              DATR(FOR L ~ 1 STEP 1 UNTIL NVAR DO DATA[L]),             PR1 195 
              CORR(I,FOR J ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]),          PR1 196 
              EVAL(FOR I ~ 1 STEP 1 UNTIL Z DO SUMX[I]),                PR1 197 
              CUMP(FOR I ~ 1 STEP 1 UNTIL Z DO R[I,1]),                 PR1 198 
              EVEC(J,FOR I ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]),          PR1 199 
              FACT(I,FOR J ~ 1 STEP 1 UNTIL Z DO R[I,J]),               PR1 200 
              CHEK(I,FOR J ~ 1 STEP 1 UNTIL Z DO SCA[I,J]),             PR1 201 
              CAS4(FOR J ~ 1 STEP 1 UNTIL NVAR DO SCA[I,J]) ;           PR1 202 
FILE          TAPEB 2 (1,10) ;                                          PR1 203 
              WRITE(PRINT,FRM1,OUTT) ;                                  PR1 204 
               IF INP > 0 THEN                                          PR1 205 
         BEGIN                                                          PR1 206 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 207 
         BEGIN                                                          PR1 208 
                               READ(CORMAT, FCM, CAS4);                 PR1 209 
          END;                                                          PR1 210 
         END  ELSE                                                      PR1 211 
         BEGIN                                                          PR1 212 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 213 
               READ(CARD,SWF,CAS4) END;                                 PR1 214 
               IF CAS!2 THEN GO TO LL33;                                PR1 215 
LCAS6:    BEGIN                                                         PR1 216 
REAL ARRAY     A[0:NVAR,0:NVAR];                                        PR1 217 
              LL ~ NVAR - 1 ;                                           PR1 218 
              FOR II ~ 1 STEP 1 UNTIL NVAR DO                           PR1 219 
         BEGIN                                                          PR1 220 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 221 
              T[I] ~ I ;                                                PR1 222 
              FOR I ~ II STEP 1 UNTIL LL DO                             PR1 223 
              T[I] ~ T[I+1] ;                                           PR1 224 
              T[NVAR] ~ II ;                                            PR1 225 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 226 
         BEGIN                                                          PR1 227 
              JJ ~ T[J] ;                                               PR1 228 
              FOR I ~ 1 STEP 1 UNTIL J DO                               PR1 229 
         BEGIN                                                          PR1 230 
              KK ~ T[I] ;                                               PR1 231 
              SUMXX[I,J] ~ SCA[KK,JJ]                                   PR1 232 
         END                                                            PR1 233 
         END ;                                                          PR1 234 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 235 
              R[1,J] ~ SUMXX[1,J] ;                                     PR1 236 
              FOR I ~ 1 STEP 1 UNTIL LL DO                              PR1 237 
         BEGIN                                                          PR1 238 
              FOR J ~ I STEP 1 UNTIL NVAR DO                            PR1 239 
                A[I,J] ~ R[I,J]/R[I,I] ;                                PR1 240 
              MM ~ I+1 ;                                                PR1 241 
              FOR J ~ MM STEP 1 UNTIL NVAR DO                           PR1 242 
         BEGIN                                                          PR1 243 
              S ~ 0.0 ;                                                 PR1 244 
              FOR M ~ 1 STEP 1 UNTIL I DO                               PR1 245 
              S ~ S +   A[M,MM]|R[M,J] ;                                PR1 246 
              R[MM,J] ~ SUMXX[MM,J] - S                                 PR1 247 
         END                                                            PR1 248 
         END ;                                                          PR1 249 
              DATA[II] ~ 0.0 ;                                          PR1 250 
              FOR I ~ 1 STEP 1 UNTIL LL DO                              PR1 251 
              DATA[II] ~ DATA[II] + R[I,NVAR]|  A[I,NVAR] ;             PR1 252 
              DATA[II] ~ DATA[II] / SUMXX[NVAR,NVAR]                    PR1 253 
         END ;                                                          PR1 254 
          END;                                                          PR1 255 
LCAS6A:       FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 256 
              SCA[I,I] ~ DATA[I] ;                                      PR1 257 
LL33:          IF  CAS =3 THEN                                          PR1 258 
          BEGIN                                                         PR1 259 
               OBJECTFMTGEN(COF);                                       PR1 260 
               WRITE(LINE[DBL]);                                        PR1 261 
               READ(CARD,COF,DATR);                                     PR1 262 
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           PR1 263 
               SCA[I,I] ~ DATA[I];                                      PR1 264 
          END;                                                          PR1 265 
              WRITE(PRINT,FRM5) ;                                       PR1 266 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 267 
              WRITE(PRINT,FRM6,CORR) ;                                  PR1 268 
COMMENT       CALCULATE EIGENVALUES AND EIGENVECTORS ;                  PR1 269 
LL34:    BEGIN                                                          PR1 270 
REAL ARRAY    IND[0:NVAR] ;                                             PR1 271 
LABEL         LL4,LL5,LL6,LL7,LL8,LL9,LL10,LL11,LL12,LL13,LL14,LL15,    PR1 272 
              LL16,LL17,LL18,LL19,LL20,LL21,LL22,LL36 ;                 PR1 273 
              NORM ~ 0.0 ;                                              PR1 274 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 275 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 276 
              NORM ~ NORM + SCA[I,J]*2 ;                                PR1 277 
              NORM ~ SQRT(NORM) ;                                       PR1 278 
              IF NVAR = 0 THEN                                          PR1 279 
              GO TO LL4 ;                                               PR1 280 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 281 
         BEGIN                                                          PR1 282 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 283 
              R[I,J] ~ 0.0 ;                                            PR1 284 
              R[I,I] ~ 1.0                                              PR1 285 
         END ;                                                          PR1 286 
LL4:          EX ~ 1 ;                                                  PR1 287 
              LL ~ NVAR - 2 ;                                           PR1 288 
              IF LL < 0 THEN                                            PR1 289 
              GO TO LL23 ;                                              PR1 290 
              IF LL = 0 THEN                                            PR1 291 
              GO TO LL5 ;                                               PR1 292 
              FOR I ~ 1 STEP 1 UNTIL LL DO                              PR1 293 
         BEGIN                                                          PR1 294 
              II ~ I + 2 ;                                              PR1 295 
              FOR J ~ II STEP 1 UNTIL NVAR DO                           PR1 296 
         BEGIN                                                          PR1 297 
              T1 ~ SCA[I,I+1] ;                                         PR1 298 
              T2 ~ SCA[I,J] ;                                           PR1 299 
LL20:         IF T2 ! 0 THEN                                            PR1 300 
         BEGIN                                                          PR1 301 
LL18:         TE ~ SQRT(T1|T1 + T2|T2) ;                                PR1 302 
              KOS ~ T1 / TE ;                                           PR1 303 
              SYN ~ T2 / TE ;                                           PR1 304 
              IF EX = 1 THEN                                            PR1 305 
              GO TO LL22                                                PR1 306 
              ELSE                                                      PR1 307 
              GO TO LL19                                                PR1 308 
         END ;                                                          PR1 309 
              IF EX = 1 THEN                                            PR1 310 
              GO TO LL36                                                PR1 311 
              ELSE                                                      PR1 312 
              GO TO LL18 ;                                              PR1 313 
LL22:         FOR K ~ I STEP 1 UNTIL NVAR DO                            PR1 314 
         BEGIN                                                          PR1 315 
              T2 ~ KOS | SCA[K,I+1] + SYN | SCA[K,J] ;                  PR1 316 
              SCA[K,J] ~ KOS | SCA[K,J] - SYN | SCA[K,I+1] ;            PR1 317 
              SCA[K,I+1] ~ T2                                           PR1 318 
         END ;                                                          PR1 319 
              FOR K ~ I STEP 1 UNTIL NVAR DO                            PR1 320 
         BEGIN                                                          PR1 321 
              T2 ~ KOS | SCA[I+1,K] + SYN | SCA[J,K] ;                  PR1 322 
              SCA[J,K] ~ KOS | SCA[J,K] - SYN | SCA[I+1,K] ;            PR1 323 
              SCA[I+1,K] ~ T2                                           PR1 324 
         END ;                                                          PR1 325 
              IF NVAR ! 0 THEN                                          PR1 326 
         BEGIN                                                          PR1 327 
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            PR1 328 
         BEGIN                                                          PR1 329 
              T2 ~ KOS | R[K,I+1] + SYN | R[K,J] ;                      PR1 330 
              R[K,J] ~ KOS | R[K,J] - SYN | R[K,I+1] ;                  PR1 331 
              R[K,I+1] ~ T2                                             PR1 332 
         END                                                            PR1 333 
         END ;                                                          PR1 334 
LL36:    END                                                            PR1 335 
         END ;                                                          PR1 336 
LL5:          FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 337 
         BEGIN                                                          PR1 338 
              DIAG[I] ~ SCA[I,I] ;                                      PR1 339 
              SUMX[I] ~ NORM ;                                          PR1 340 
              VAL[I] ~ -NORM                                            PR1 341 
         END ;                                                          PR1 342 
              FOR I ~ 2 STEP 1 UNTIL NVAR DO                            PR1 343 
         BEGIN                                                          PR1 344 
              SD[I-1] ~ SCA[I-1,I] ;                                    PR1 345 
              X[I-1] ~ SD[I-1]*2                                        PR1 346 
         END ;                                                          PR1 347 
              TA ~ 0.0 ;                                                PR1 348 
              I ~ 1 ;                                                   PR1 349 
LL13:         M ~ 0 ;                                                   PR1 350 
              T2 ~ 0.0 ;                                                PR1 351 
              T1 ~ 1.0 ;                                                PR1 352 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 353 
         BEGIN                                                          PR1 354 
              P ~ DIAG[J] - TA ;                                        PR1 355 
              IF T2 = 0 THEN                                            PR1 356 
              GO TO LL6 ;                                               PR1 357 
              IF T1 = 0 THEN                                            PR1 358 
              GO TO LL7 ;                                               PR1 359 
              TE ~ P|T1 - X[J-1]|T2 ;                                   PR1 360 
              GO TO LL8 ;                                               PR1 361 
LL6:          IF T1 < 0 THEN                                            PR1 362 
         BEGIN                                                          PR1 363 
              T1 ~ -1.0 ;                                               PR1 364 
              TE ~ -P ;                                                 PR1 365 
              GO TO LL8                                                 PR1 366 
         END ;                                                          PR1 367 
LL9:          T1 ~ 1.0 ;                                                PR1 368 
              TE ~ P ;                                                  PR1 369 
              GO TO LL8 ;                                               PR1 370 
LL7:          IF X[J-1] = 0 THEN                                        PR1 371 
              GO TO LL9 ;                                               PR1 372 
              IF T2 } 0 THEN                                            PR1 373 
         BEGIN                                                          PR1 374 
              TE ~ -1.0 ;                                               PR1 375 
              GO TO LL8                                                 PR1 376 
         END ;                                                          PR1 377 
              TE ~ 1.0 ;                                                PR1 378 
LL8:          IF ((T1 < 0) AND (TE < 0)) OR ((T1 } 0) AND (TE } 0)) THENPR1 379 
              M ~ M+1 ;                                                 PR1 380 
              T2 ~ T1 ;                                                 PR1 381 
              T1 ~ TE                                                   PR1 382 
              ;WHILE ABS(T1)>10 OR ABS(T2)>10 DO                        PR1 383 
          BEGIN                                                         PR1 384 
               T1 ~ T1/10;                                              PR1 385 
               T2~ T2/10;                                               PR1 386 
          END;                                                          PR1 387 
         END ;                                                          PR1 388 
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            PR1 389 
         BEGIN                                                          PR1 390 
              IF K { M THEN                                             PR1 391 
         BEGIN                                                          PR1 392 
              IF TA > VAL[K] THEN                                       PR1 393 
              VAL[K] ~ TA                                               PR1 394 
         END                                                            PR1 395 
              ELSE                                                      PR1 396 
         BEGIN                                                          PR1 397 
              IF TA < SUMX[K] THEN                                      PR1 398 
              SUMX[K] ~ TA                                              PR1 399 
         END                                                            PR1 400 
         END ;                                                          PR1 401 
LL12:         W ~ SUMX[I] - VAL[I] - 5.0@-8 ;                           PR1 402 
              IF W { 0 THEN                                             PR1 403 
              GO TO LL10 ;                                              PR1 404 
              IF SUMX[I] = 0 THEN                                       PR1 405 
              GO TO LL11 ;                                              PR1 406 
              W ~ ABS(VAL[I] / SUMX[I] - 1.0) - 5.0@-8 ;                PR1 407 
              IF W > 0 THEN                                             PR1 408 
              GO TO LL11 ;                                              PR1 409 
LL10:         I ~ I + 1 ;                                               PR1 410 
              IF I > NVAR THEN                                          PR1 411 
              GO TO LL14                                                PR1 412 
              ELSE                                                      PR1 413 
              GO TO LL12 ;                                              PR1 414 
LL11:         TA ~ (VAL[I] + SUMX[I]) / 2.0 ;                           PR1 415 
              GO TO LL13 ;                                              PR1 416 
LL14:         EX ~ 2 ;                                                  PR1 417 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 418 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 419 
              SCA[I,J] ~ 0.0 ;                                          PR1 420 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 421 
         BEGIN                                                          PR1 422 
              IF I = 1 THEN                                             PR1 423 
              GO TO LL21 ;                                              PR1 424 
              W ~ SUMX[I-1] - SUMX[I] - 5.0@-7 ;                        PR1 425 
              IF W { 0 THEN                                             PR1 426 
              GO TO LL16 ;                                              PR1 427 
              IF SUMX[I-1] ! 0 THEN                                     PR1 428 
         BEGIN                                                          PR1 429 
              W ~ ABS(SUMX[I] / SUMX[I-1] - 1.0) - 5.0@-7 ;             PR1 430 
              IF W { 0 THEN                                             PR1 431 
              GO TO LL16                                                PR1 432 
         END ;                                                          PR1 433 
LL21:         KOS ~ 1.0 ;                                               PR1 434 
              SYN ~ 0.0 ;                                               PR1 435 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 436 
         BEGIN                                                          PR1 437 
              IF J = 1 THEN                                             PR1 438 
              GO TO LL15                                                PR1 439 
              ELSE                                                      PR1 440 
              GO TO LL20 ;                                              PR1 441 
LL19:         X[J-1] ~ SYN ;                                            PR1 442 
              DATA[J-1] ~ KOS ;                                         PR1 443 
              VAL[J-1] ~ T1|KOS + T2|SYN ;                              PR1 444 
LL15:         T1 ~ (DIAG[J]-SUMX[I])|KOS - TA|SYN ;                     PR1 445 
              T2 ~ SD[J] ;                                              PR1 446 
              TA ~ SD[J]|KOS                                            PR1 447 
         END ;                                                          PR1 448 
              VAL[NVAR] ~ T1 ;                                          PR1 449 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 450 
              IND[J] ~ 0 ;                                              PR1 451 
LL16:         TE ~ NORM ;                                               PR1 452 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 453 
         BEGIN                                                          PR1 454 
              IF IND[J] } 1 THEN                                        PR1 455 
              GO TO LL17 ;                                              PR1 456 
              IF ABS(TE) { ABS(VAL[J]) THEN                             PR1 457 
              GO TO LL17 ;                                              PR1 458 
              TE ~ VAL[J] ;                                             PR1 459 
              LL ~ J ;                                                  PR1 460 
LL17:    END ;                                                          PR1 461 
              IND[LL] ~ 1 ;                                             PR1 462 
              P ~ 1.0 ;                                                 PR1 463 
              IF LL ! 1 THEN                                            PR1 464 
         BEGIN                                                          PR1 465 
              FOR K ~ 2 STEP 1 UNTIL LL DO                              PR1 466 
         BEGIN                                                          PR1 467 
              M ~ LL+1-K ;                                              PR1 468 
              SCA[M+1,I] ~ DATA[M]|P ;                                  PR1 469 
              P ~ -P|X[M]                                               PR1 470 
         END                                                            PR1 471 
         END ;                                                          PR1 472 
              SCA[1,I] ~ P                                              PR1 473 
         END ;                                                          PR1 474 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 475 
         BEGIN                                                          PR1 476 
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            PR1 477 
              IND[K] ~ SCA[K,J] ;                                       PR1 478 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 479 
         BEGIN                                                          PR1 480 
              SCA[I,J] ~ 0.0 ;                                          PR1 481 
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            PR1 482 
              SCA[I,J] ~ R[I,K]|IND[K] + SCA[I,J]                       PR1 483 
         END                                                            PR1 484 
         END                                                            PR1 485 
         END ;                                                          PR1 486 
LL23:         FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 487 
         BEGIN                                                          PR1 488 
              IF SUMX[I] { RN THEN                                      PR1 489 
         BEGIN                                                          PR1 490 
              Z ~ I-1 ;                                                 PR1 491 
              GO TO LL24                                                PR1 492 
         END                                                            PR1 493 
         END ;                                                          PR1 494 
              Z ~ NVAR ;                                                PR1 495 
LL24:         WRITE(PRINT,FRM7) ;                                       PR1 496 
              WRITE(PRINT,FRM3,EVAL) ;                                  PR1 497 
              IF NVAR > Z THEN                                          PR1 498 
              WRITE(PRINT,FRM8) ;                                       PR1 499 
COMMENT       CALCULATE CUMULATIVE PROPORTIONS OF TOTAL VARIANCE ;      PR1 500 
              R[1,1] ~ SUMX[1] ;                                        PR1 501 
              FOR I ~ 2 STEP 1 UNTIL Z DO                               PR1 502 
              R[I,1] ~ R[I-1,1] + SUMX[I] ;                             PR1 503 
              FOR I ~ 1 STEP 1 UNTIL Z DO                               PR1 504 
              R[I,1] ~ R[I,1] / (       R[Z,1]) ;                       PR1 505 
              WRITE(PRINT,FRM9) ;                                       PR1 506 
              WRITE(PRINT,FRM3,CUMP) ;                                  PR1 507 
              WRITE(PRINT,FRM10) ;                                      PR1 508 
              FOR J ~ 1 STEP 1 UNTIL Z DO                               PR1 509 
              WRITE(PRINT,FRM11,EVEC) ;                                 PR1 510 
              FOR I ~ 1 STEP 1 UNTIL Z DO                               PR1 511 
         BEGIN                                                          PR1 512 
              SUMX[I] ~ SQRT(SUMX[I]) ;                                 PR1 513 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 514 
              R[J,I] ~ SUMX[I]|SCA[J,I]                                 PR1 515 
         END ;                                                          PR1 516 
              WRITE(PRINT,FRM12) ;                                      PR1 517 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 518 
              WRITE(PRINT,FRM13,FACT) ;                                 PR1 519 
              IF NVAR > Z THEN                                          PR1 520 
         BEGIN                                                          PR1 521 
              FOR I ~ 1 STEP 1 UNTIL Z DO                               PR1 522 
              FOR J ~ 1 STEP 1 UNTIL Z DO                               PR1 523 
         BEGIN                                                          PR1 524 
            SCA[I,J] ~ 0.0 ;                                            PR1 525 
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            PR1 526 
               SCA[I,J] ~ SCA[I,J] + R[K,I]|R[K,J]                      PR1 527 
         END ;                                                          PR1 528 
              WRITE(PRINT,FRM14)                                        PR1 529 
         END                                                            PR1 530 
              ELSE                                                      PR1 531 
         BEGIN                                                          PR1 532 
              FOR I ~ 1 STEP 1 UNTIL NVAR DO                            PR1 533 
              FOR J ~ 1 STEP 1 UNTIL NVAR DO                            PR1 534 
         BEGIN                                                          PR1 535 
            SCA[I,J] ~ 0.0 ;                                            PR1 536 
              FOR K ~ 1 STEP 1 UNTIL NVAR DO                            PR1 537 
               SCA[I,J] ~ SCA[I,J] + R[I,K]|R[J,K]                      PR1 538 
         END ;                                                          PR1 539 
              WRITE(PRINT,FRM15)                                        PR1 540 
         END ;                                                          PR1 541 
              FOR I ~ 1 STEP 1 UNTIL Z DO                               PR1 542 
              WRITE(PRINT,FRM6,CHEK) ;                                  PR1 543 
               IF WRT > 0 OR WRC > 0 THEN                               PR1 544 
          BEGIN                                                         PR1 545 
COMMENT        WRITE FACTOR MATRIX ON TAPE AND CARDS;                   PR1 546 
FILE OUT       FACMAT 2(2,100, SAVE 10) ;                               PR1 547 
INTEGER        J1Q,J2Q,JSECQ;                                           PR1 548 
FORMAT         OKTL(100O),                                              PR1 549 
               PUNC(2I4,X2,10(X1,F6.4));                                PR1 550 
LIST           FACP(I,JSECQ,FOR J ~ J1Q STEP 1 UNTIL J2Q DO R[I,J]),    PR1 551 
               TAPO(FOR J ~ 1 STEP 1 UNTIL Z DO R[I,J]);                PR1 552 
LABEL          L77,L78,L79;                                             PR1 553 
               IF WRT > 0 THEN BEGIN                                    PR1 554 
               WRITE(FACMAT,OKTL,NVAR,Z) ;                              PR1 555 
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           PR1 556 
               WRITE(FACMAT,OKTL,TAPO) ;                                PR1 557 
               REWIND(FACMAT) ;                                         PR1 558 
          END ;                                                         PR1 559 
               IF WRC >0 THEN  BEGIN                                    PR1 560 
L77:           FOR I ~ 1 STEP 1 UNTIL NVAR DO                           PR1 561 
          BEGIN                                                         PR1 562 
               J1Q ~ 0;                                                 PR1 563 
               J2Q ~ 0;                                                 PR1 564 
               JSECQ ~ 0;                                               PR1 565 
L78:           J1Q ~ J2Q+1;                                             PR1 566 
               J2Q ~ J1Q+9;                                             PR1 567 
               IF J2Q { Z    THEN GO TO L79;                            PR1 568 
               J2Q ~ Z;                                                 PR1 569 
L79:           JSECQ ~ JSECQ+1;                                         PR1 570 
               WRITE(PUNCH,PUNC,FACP);                                  PR1 571 
               IF J2Q < Z    THEN GO TO L78;                            PR1 572 
          END;                                                          PR1 573 
          END END;                                                      PR1 574 
         END ;                                                          PR1 575 
         END ;                                                          PR1 576 
              GO TO START ;                                             PR1 577 
WRONGCOUNT:    WRITE(LINE, ERRCNT);                                     PR1 578 
NOMO:          DATELINE(0);                                             PR1 579 
          END.                                                          PR1 580 
