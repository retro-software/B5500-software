          BEGIN                                                         00000000
COMMENT        CORMISD/STATMAN JAN 21,1969;                             00000100
COMMENT                                                                 00000200
MAKMAST/MISDAT PROPARES A TAPE "MASTER" OF ALL THE DATA IT READS FROM   00000300
CARDS.  ANY MISSING DATA - BLANKS - ON CARD ARE REPLACED BY "NULL".     00000400
ONE TO SIX DIGIT FIELDS ARE ALLOWED (A6).  A REPLACABLE FORMAT CARD     00000500
SPECIFIES THE CARD DATA FORMAT.  ALL THE FIELDS MUST BE READ AS         00000600
ALPHA (IE. A4) TO FIND THE BLANKS.  AFTER "MASTER" HAS BEEN PREPARED    00000700
AND  HAS BEEN REWOUND THE PROGRAM IS NEXT DESIGNED TO COMPUTE THE N,    00000800
SUM XSQ, SUM YSQ, SUM Y, SUM X, AND SUMXY FOR UP TO 1023 SCORES (NVAR)  00000900
AND AS MANY CASES (NCASE) AS WILL FIT ON ONE MAGNETIC TAPE.  THE PROGRAM00001000
WILL ACCEPT DATA FROM TAPE (FILE "MASTER") WHICH CONTAINS ONE CASE OR   00001100
SUBJECT PER RECORD (NO BLOCKING FACTOR) AND ONE VARIABLE OR SCORE PER   00001200
COMPUTER WORD (FLT.PT.BINARY), THEREFORE 65 SCORES AND 500 CASES WILL   00001300
BE CONTAINED ON A TAPE COMPOSED OF 500 RECORDS (NO BLOCKING) AND 65     00001400
WRDS/REC.  A MISSING SCORE IS INDICATED BY THE CHARACTERS "NULL"        00001500
(45644343-RIGHT JUSTIFIED ) IN THE PLACE OF A SCORE.  ANY "NULL" SCORE  00001600
IS NOT CONSIDERED IN THE CORRELATION. PARTITIONING OF THE MATRIX IS     00001700
AUTOMATIC AND IS DEPENDENT ON THE AMOUNT OF MEMORY AVAILABLE (MEM)      00001800
AND THE NUMBER OF VARIABLES (NVAR).  THE PARTITIONING FACTOR (NPART)    00001900
IS INDEPENDENT OF THE NUMBER OF CASES.  NPART THE DETERMINES THE NUMBER 00002000
OF TAPE PASSES REQUIRED TO FORM ALL THE SUMS AFTER EACH PASS OF "MASTER"00002100
THE COMPUTED SUMS ARE WRITTEN ONTO TAPE "SUMS" UNTIL ALL SUMS ARE       00002200
FORMED.  IT IS THEN NECESSARY TO REARRANGE THE SUMS AND WRITE THEM      00002300
ONTO TAPE ARR OR ARR1 (7 WRDS/REC).  THE FIRST WORD OF EACH RECORD IS   00002400
USED INTERNALLY WITHIN THE PROGRAM.  WHEN REARRANGING IS FINISHED       00002500
(PERFORMED IN THE INNER BLOCK)  THE RESULTS ARE PRINTED (TWOTAPE PASSES 00002600
REQ.).                                                                  00002700
          FOR RETART PURPOSES THE PROGRAM HAS BEEN BROKEN INTO 5 PHASES 00002800
DESCRIBED BELOW:                                                        00002900
                                                                        00003000
          PHASE 0 - INITIAL STARTING OF THE PROGRAM, DATA COMES FROM    00003100
                    FILE "MASTER". (FILE PROTECT THIS TAPE)             00003200
                                                                        00003300
          PHASE 1 - THIS PHASE IS ENTERED ANY TIME AFTER FILE SUMS      00003400
                    IS OPENED.  THIS PHASE READS MASTER AND WRITES SUMS.00003500
       A CARD IS PUNCHED AT THE END OF EVERY TAPE WRITE FOR FILE "SUMS" 00003600
(EXCEPT THE LAST ONE).  THE PUNCH IS THEN CLOSED AND REOPENED WHEN      00003700
WHEN AGAIN NEEDED.  THE CARD THAT IS PUNCHED IS REQUIRED FOR ANY RESTART00003800
DURING PHASE 1 AND INDICATES TO THE PROGRAM WHERE FILE "SUMS" MUST BE   00003900
POSITIONED.  THIS CARD ALONG WITH ITS "LABEL" AND "EOF" CARD MUST FOLLOW00004000
THE "EOF" CARD THAT FOLLOWS THE STANDARD INPUT CARD.  IF A RESTART      00004100
IS MADE DURING ANY OTHER PHASE, THAN THIS SECOND CARD IS NOT REQUIRED.  00004200
       IF NO CARD(S) IS PUNCHED THAN "NPART" EQUALS 1.  IF "NPART" > 1  00004300
THEN (NPART-1) CARDS WILL BE PUNCHED (WITH THE PUNCH FILE OPENED        00004400
AND CLOSED FOR EACH CARD).                                              00004500
       THE LAST CARD PUNCHED SHOULD BE USED IN A RESTART DURING PHASE 1 00004600
ALTHO, ANY OF THE PREVIOUSLY PUNCHED CARDS WOULD ALSO WORK .            00004700
                                                                        00004800
          PHASE 2 - ENTERED AFTER EOT MESSAGE GIVEN FOR SUMS.  FILES    00004900
                    ARR AND/OR ARR1 ARE THEN OPENED.                    00005000
                                       FILE SUMS IS LOCKED AND THE      00005100
                    WRITE RING MUST BE PULLED BEFORE PROCEEDING (SAFETY 00005200
                    PRECAUTION).  THIS PHASE READ SUMS AND WRITES ARR   00005300
                    AND/OR ARR1.                                        00005400
                    ONE PASS IS MADE ON FILE "MASTER" SO THAT THE       00005500
                    DIAGNAL ELEMENTS MAY BE FORMED - "MASTER" MAY       00005600
                    THEN BE REMOVED.  (FOR A PHASE 2 RESTART)           00005700
                    (ALSO ON A PHASE 2 RESTART IT IS NECESSARY TO       00005800
                    PURGE FILES ARR AND/OR ARR1, IF OPENED)             00005900
                                                                        00006000
          PHASE 3 - ENTERED AFTER FILE ARR OR ARR1 IS LOCKED.  REMOVE   00006100
                    WRITE RING-AS PER SPO MESSAGE-TO PROCEED.  SUM XY,  00006200
                    N, ETC IS READ FROM TAPE AND PRINTED FOR ALL        00006300
                    COMBINATIONS.                                       00006400
                                                                        00006500
          PHASE 4 - ENTERED AFTER ALL SUMS HAVE BEEN PRINTED (IDENTITY  00006600
                    MATRIX IS LAST ONE PRINTED).  MEANS, STANDARD       00006700
                    DEVIATION, ETC IS NOW COMPUTED AND PRINTED BY MAKING00006800
                    AN ADDITIONAL PASS OF TAPE ARR OR ARR1.             00006900
                                                                        00007000
          ONE DATA CARD OF THE FOLLOWING FORMAT IS READ:                00007100
           (ALL COLUMN NUMBERS ARE INCLUSIVE)                           00007200
** CARD 1 **                                                            00007300
                                                                        00007400
                                                                        00007500
     COLS 1-72     IDENTIFICATION INFORMATION OF YOUR CHOICE TO BE      00007600
                   USED IN TITLE.                                       00007700
                                                                        00007800
                                                                        00007900
** CARD 2 **                                                            00008000
                                                                        00008100
                                                                        00008200
1    COLS 1-5      NUMBER OF VARIABLES (WRDS/REC) RIGHT JUSTIFIED       00008300
                                                                        00008400
2    COLS 6-10     NUMBER OF SUBJECTS (NO OF RECORDS) RIGHT JUSTIFIED   00008500
                                                                        00008600
3    COL 15        PRNT - IF YOU WANT THE DATA INPUT TO BE PRINTED      00008700
                   PUNCH     1      YES                                 00008800
                             BLANK     OTHERWISE                        00008900
                                                                        00009000
4    COLS 20       PRINTORNOT - IF YOU WANT THE SUM X,Y, X*2, Y*2, XY   00009100
                   PRINTED                                              00009200
                   PUNCH     1     YES                                  00009300
                             BLANK      OTHERWISE                       00009400
                                                                        00009500
5    COLS 26-30    CORROUT - IF YOU WANT TAPE "CORMAT" CREATED WITH YOUR00009600
                   FULL CORRELATION MATRIX OUTPUT                       00009700
                   PUNCH     TRUE    WANT "CORMAT"                      00009800
                             BLANK     OTHERWISE                        00009900
                                                                        00010000
6    COL 31        RESTART NUMBER (0,1,2,3 OR 4)                        00010100
                                                                        00010200
7    COL 32-35     I (RIGHT JUSTIFIED) A NONZERO INDICATES THAT ONLY    00010300
                   ALL I, J COMBINATIONS WILL BE FORMED AND PRINTED FOR 00010400
                   HELD CONSTANT AND J=I, ... NVAR (NO. OF VARIABLES)   00010500
                   NAMELY, IF THIS FIELD CONTAINS A 35 THEN COMBINATIONS00010600
                   35,35 35,36 35,37 35,38........35,NVAR-1, 35,NVAR    00010700
                   ARE FORMED AND PRINTED.  THIS FIELD AND COLS 36-39   00010800
                   MAY NOT BOTH BE NONBLANK ON THE SAME RUN.  THIS      00010900
                   OPTION IS IGNORED IF THE FIELD IS BLANK OR ZERO.     00011000
                                                                        00011100
8    COLS 36-39    J (RIGHT JUSTIFIED).  A NONZERO INDICATES THAT ONLY  00011200
                   ALL I, J COMBINATIONS WILL BE FORMED FOR J HELD      00011300
                   CONSTANT AND I=1,2....J. NAMELY, IF THIS FIELD       00011400
                   CONTAINS A 35 THE COMBINATIONS 1,35 2,353,35 4,35 .. 00011500
                   ... 34,35 35,35 ARE COMPUTED AND RESULTS PRINTED.    00011600
                   IF THIS FIELD IS BLANK OR A 1 THEN THE STANDARD      00011700
                   CORRELATION FOR ALL COMBINATIONS ARE COMPUTED.       00011800
                   RESTART NUMBER IS IGNORED IF THIS FIELD IS NONBLANK. 00011900
                   THIS FIELD AND COLS 32-35 MAY NOT BOTH BE NONBLANK   00012000
                    THE SAME RUN.                                       00012100
                                                                        00012200
          ******* OPTIONS 7 AND 8 ARE PRIMARILY PROVIDED FOR DATA       00012300
          ******* DEBUGING INCASE  CERTAIN SCORES ARE THOUGHT TO BE IN  00012400
          ******* ERROR                                                 00012500
                                                                        00012600
          AFTER ITEMS 1,2 HAVE BEEN ESTABLISHED THEY MAY NOT BE CHANGED 00012700
          ON ANY RESTART.                                               00012800
          ITEM 4 MAY BE CHANGED ON ANY RESTART.  IF COL 20 IS BLANK AND 00012900
          PHASE 3 IS INDICATED  ON THE RESTART CARD, THEN PHSAE 3       00013000
          IS PRINTED ANYWAY.  FOR A RESTART NUMBER OF 0,1, OR 2 AND     00013100
          ITEM 4 NONBLANK THEN PHASE 3 IS NOT PRINTED.                  00013200
APPROX. RUNNING TIME FOR A PROBLEM OF 500 CASES AND 112 VARIABLES       00013300
IS 1 HOUR AND 30 MIN.  PHASES 0,1, AND 2 ARE COMPUTE BOUND AND RAN      00013400
FOR ABOUT 1 HOUR AND 10 MIN,  PHASES 3 AND 4 ARE PRINT BOUND AND ACCOUNT00013500
FOR THE REMAINDER OF THE TIME.                                          00013600
                                    R.J.HAM - BURROUGHS CORP.         ; 00013700
                                                                        00013800
                                                                        00013900
FILE IN        CARD(2,10);                                              00014000
FILE OUT       LINE 4(2,15);                                            00014100
LABEL          FINIS;                                                   00014200
INTEGER        NVAR,N,I,S;                                              00014300
REAL ARRAY     TITLE[0:11];                                             00014400
INTEGER        NSUBJ, CSUBJ, PRNT;                                      00014500
BOOLEAN        CORROUT;                                                 00014600
INTEGER        PRINTORNOT;                                              00014700
INTEGER            RSTT;                                                00014800
INTEGER        ELEMNO;                                                  00014900
INTEGER        ELEMN;                                                   00015000
LIST           LPAR(FOR I~0 STEP 1 UNTIL 11 DO TITLE[I],NVAR,NSUBJ,     00015100
               PRNT,PRINTORNOT,CORROUT,RSTT,ELEMN,ELEMNO);              00015200
FORMAT         FERR(//"**********ERROR**********"//"THE NUMBER OF SUBJE"00015300
               ,"CT CARDS THAT WERE COUNTED DOES NOT EQUAL THE EXPECTED"00015400
               ," NUMBER INDICATED ON THE PARAMETER CARD."/"THUS THE PR"00015500
               ,"OGRAM HAS BEEN ENDED. TAPE * MASTER * SHOULD BE RECREA"00015600
               ,"TED WITH THE CORRECT NUMBER OF SUBJECTS.");            00015700
FORMAT         FPAR(12A6/2I5,X4,I1,X1,A4,X5,L5,I1,2I4);                 00015800
FORMAT         PFPAR (X24, 12A6 //                                      00015900
                      X41, "NUMBER OF VARIABLES .......... ", I7 /      00016000
                      X41, "NUMBER OF SUBJECTS ........... ", I7 /      00016100
                      X41, "PRINT MASTER TAPE ............ ", X5,A3/    00016200
                      X41, "PRINT SUM X, Y, X2, Y2, XY ... ",X5,A3/     00016300
                      X41, "CORMAT WRITTEN ............... ",X3,L5/);   00016400
LIST           PLPAR(FOR I~0 STEP 1 UNTIL 11 DO TITLE[I],NVAR,NSUBJ,    00016500
               IF PRNT=1 THEN "YES" ELSE " NO", IF PRINTORNOT=1 THEN    00016600
               "YES" ELSE " NO",CORROUT);                               00016700
FORMAT         SUBCNT (X41, "COMPUTER-COUNTED SUBJECTS .... ", I7 //);  00016800
LIST           LSUB (CSUBJ);                                            00016900
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00017000
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00017100
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00017200
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00017300
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00017400
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00017500
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00017600
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00017700
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00017800
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00017900
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00018000
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00018100
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00018200
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00018300
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00018400
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00018500
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00018600
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00018700
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300018800
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00018900
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00019000
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00019100
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00019200
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00019300
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00019400
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00019500
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00019600
2";TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY~TALLY+100019700
;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TALLY+CNTV00019800
;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~DI+CNTV;00019900
SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF SCAN;PRO00020000
CEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],PRNT[0],L00020100
CR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BEGIN LABE00020200
L L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR THEN BE00020300
GIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN CNT~ACC00020400
UM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;IF EXAMI00020500
N(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)ELSE IF E00020600
LCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER;ELCLASS00020700
~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TEMP,FINAL00020800
,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM WDS;ST1~00020900
SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);END;END O00021000
F MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN INTEGER 00021100
CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,FLG);VALU00021200
E CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~LOC ST1;00021300
DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;SI~FMT;C00021400
TR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TALLY+1;DI~00021500
DI-4;SI~SI+4);ST1~TALLY;GO TO EXIT;FND:ST1~TALLY;SI~SI-4;DI~FLG;DS~WDS;E00021600
XIT:SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FM00021700
AX~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RE00021800
TURN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~L00021900
OC F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZ00022000
E IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LAR00022100
GER;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(00022200
137);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALU00022300
E NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMB00022400
ER~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<800022500
THEN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(00022600
NFWDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~N00022700
FWD;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN00022800
 LABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,00022900
W2,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W00023000
,W1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SK00023100
IP[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]00023200
&REAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCED00023300
URE PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B00023400
";SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI00023500
+7;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S00023600
;INTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#00023700
;DEFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0T00023800
HEN BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPE00023900
AT~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEG00024000
IN SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:00024100
IF FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO 00024200
BEGIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"A00024300
ND ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,00024400
0,0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN 00024500
BEGIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE I00024600
F ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF EL00024700
CLASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEX00024800
TENT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0T00024900
HEN BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,R00025000
SCALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN COD00025100
E~100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCU00025200
M[1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F00025300
+1;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SK00025400
IP+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END EL00025500
SE BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE00025600
(W,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END EL00025700
SE IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN00025800
 CODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(EL00025900
CLASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-00026000
ELCLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE00026100
="X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"TH00026200
EN BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W100026300
~5;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,R00026400
EPEAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR00026500
(136);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;D00026600
O NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(00026700
32);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHR00026800
ASE;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[000026900
]);WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(P00027000
RNT[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9]00027100
;WHIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN 00027200
BEGIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,00027300
INFORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                 00027400
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEA00027500
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",00027600
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"///),LAYT(//"EXECUTION TIM00027700
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///00027800
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=00027900
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAY00028000
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FE00028100
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.00028200
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];EN00028300
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THEN00028400
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,00028500
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       00028600
INTEGER STREAM PROCEDURE NULLDATA (A);                                  00028700
        BEGIN                                                           00028800
LOCAL          ST1,NEG,ST2;                                             00028900
LABEL          NULL,CONV,EXIT;                                          00029000
               SI~A;                                                    00029100
               SI~SI+2;                                                 00029200
               ST1~SI;                                                  00029300
               SI~SI+5;                                                 00029400
               IF SC=" " THEN                                           00029500
               GO TO NULL;                                              00029600
               SI~ST1;                                                  00029700
               5(IF SC="-" THEN                                         00029800
               BEGIN                                                    00029900
               ST2~SI;                                                  00030000
               DI~LOC NEG;                                              00030100
               DS~LIT "1";                                              00030200
               DI~DI+8;                                                 00030300
               JUMP OUT TO CONV;                                        00030400
               END;                                                     00030500
               SI~SI+1);                                                00030600
CONV:          SI~LOC NEG;                                              00030700
               IF SC!"0" THEN                                           00030800
               BEGIN                                                    00030900
               DI~ST2;                                                  00031000
               DS~LIT "0";                                              00031100
               END;                                                     00031200
               SI~ST1;                                                  00031300
               DI~LOC NULLDATA;                                         00031400
               DS~6 OCT;                                                00031500
               SI~LOC NEG;                                              00031600
               IF SC!"0" THEN                                           00031700
               BEGIN                                                    00031800
               DI~LOC NULLDATA;                                         00031900
               SKIP 1 DB;                                               00032000
               DS~SET;                                                  00032100
               END;                                                     00032200
               GO TO EXIT;                                              00032300
NULL:          DI~LOC NULLDATA;                                         00032400
               DS~8 LIT "0000NULL";                                     00032500
EXIT:          END OF NULLDATA;                                         00032600
               READ(CARD, FPAR, LPAR);                                  00032700
               N ~ NVAR -1;                                             00032800
               CSUBJ ~ 0;                                               00032900
          BEGIN                                                         00033000
REAL ARRAY     A[0:N];                                                  00033100
FILE           MASTER 2 (2,NVAR,SAVE 10);                               00033200
LABEL          EXIT,L, EOT ;                                            00033300
FORMAT         DATA(/////////////////////////////////////////////////   00033400
                    /////////////////////////////////////////////////   00033500
                    /////////////////////////////////////////////////   00033600
                    /////////////////////////////////////////////////), 00033700
               FPRT(17(I6,X1));                                         00033800
LIST           LCARD(FOR I~0 STEP 1 UNTIL N DO A[I]),                   00033900
              LST1(FOR I ~ 0 STEP 1 UNTIL N DO                          00034000
               IF A[I] = "NULL" THEN 1000000 ELSE A[I]);                00034100
               DATELINE("MAKMST") ;                                     00034200
               WRITE(LINE, PFPAR, PLPAR);                               00034300
               OBJECTFMTGEN(DATA) ;                                     00034400
L:             READ(CARD,DATA ,LCARD)[EXIT];                            00034500
               CSUBJ ~ CSUBJ + 1;                                       00034600
               FOR I~0 STEP 1 UNTIL N DO                                00034700
                                                                        00034800
               A[I]~NULLDATA(A[I]);                                     00034900
               WRITE(MASTER,NVAR,A[*]);                                 00035000
               IF PRNT!0  THEN  BEGIN                                   00035100
                   WRITE(LINE, FPRT, LST1);                             00035200
                   WRITE(LINE);  END;                                   00035300
               GO TO L;                                                 00035400
EXIT:                                                                   00035500
                   CLOSE(CARD,RELEASE);                                 00035600
               WRITE(LINE, SUBCNT, LSUB);                               00035700
                 REWIND(MASTER);                                        00035800
               IF CSUBJ!NSUBJ THEN BEGIN                                00035900
               WRITE(LINE,FERR);                                        00036000
               GO TO FINIS; END;                                        00036100
               DATELINE(0) ;                                            00036200
                                                                        00036300
                                                                        00036400
         BEGIN                                                          00036500
                                                                        00036600
INTEGER        NVAR1,MEM,LLIM,ULIM,T1,T2,J,TEMP,                        00036700
                   REQMEN,REM,NP,NCASE,K   ;                            00036800
INTEGER            NPART,MEM1,REQMEM             ;                      00036900
LABEL              OK,CHECK  ;                                          00037000
FILE           TEM  2(1,200);                                           00037100
LABEL          STP;                                                     00037200
FILE OUT           PUNCH 0 (1,10);                                      00037300
FORMAT             FER(I6);                                             00037400
               MEM~10000;                                               00037500
               NCASE~NSUBJ;                                             00037600
               ELEMNO~ELEMNO-1;                                         00037700
                   NVAR1 ~ NVAR-1;                                      00037800
COMMENT            FIND BEST WAY TO PARTITION MATRIX CONSIDERING THE    00037900
                   AVAILABLE MOMORY AND NUMBER OF VARIABLES (NVAR) ;    00038000
                   NPART ~ 1;     COMMENT  PARTITION FACOTR ;           00038100
                   MEM1~MEM-2500; COMMENT  DECREASE MEMORY FOR SAFETY   00038200
                                           FACOTR ;                     00038300
COMMENT            DETERMINE MEMORY REQUIRED TO RUN THIS JOB;           00038400
       CHECK:      REQMEM ~ 3|NVAR*2/NPART;                             00038500
COMMENT            TRANSFER TO OK WHEN RIGHT"FIT" IS FOUND ;            00038600
                   IF REQMEM < MEM1 THEN GO TO OK;                      00038700
                   NPART ~ NPART+1; GO TO CHECK;                        00038800
COMMENT            NOW THAT PARTITIONING FACTOR HAS BEEN FOUND IT IS    00038900
                   NECESSARY TO DETERMINE UPPER AND LOWER BOUNDS SO THAT00039000
                   ALL COMBINATIONS CAN BE FIGURED, ALL COMBINATIONS    00039100
                   WILL BE STORED IN ARRAY PARTIT                       00039200
   ***********     NESTED BLOCK NUMBER 1     ***********             ;  00039300
     OK:                                                                00039400
              BEGIN                                                     00039500
BOOLEAN            DIR;                                                 00039600
INTEGER ARRAY      PARTIT[0:NPART|2];                                   00039700
FILE               SUMS (2,NVAR,SAVE 10);                               00039800
FILE               ARR (2,81,7,SAVE 10);                                00039900
FILE               ARR1(2,81,7,SAVE 10);                                00040000
SWITCH FILE        SC ~ ARR,ARR1;                                       00040100
INTEGER            CT,M,M1,BOTTOM,TOP,T,T1;                             00040200
INTEGER            M2,M3;                                               00040300
LABEL               RSTART0,RSTART1,RSTART2,RSTART3,RSTART4;            00040400
SWITCH  WHICH      ~RSTART0,RSTART1,RSTART2,RSTART3,RSTART4;            00040500
ARRAY              SCORE[0:NVAR1] ;                                     00040600
ARRAY A1[0:6];                                                          00040700
BOOLEAN            ONCE;                                                00040800
LABEL              PRT,EOF;                                             00040900
FORMAT             FM(I4,I5,X5,E13.6,X7,E13.6,X6,E13.6,X6,E13.6,X7,     00041000
                      E13.6,X7,I4);                                     00041100
FORMAT             HED(X3,"IDENT",X10,"SUM X",X14,"SUM X SQ",X12,       00041200
                       "SUM Y",X13,"SUM Y SQ",X13,"SUM XY",X12,"N"//);  00041300
ALPHA              NULL;                                                00041400
PROCEDURE          RESTART1;                                            00041500
              BEGIN                                                     00041600
                   READ(CARD,FER,NP);                                   00041700
                   CLOSE(CARD ,RELEASE);                                00041800
                   FOR M ~ 1 STEP 1 UNTIL NP DO                         00041900
              BEGIN                                                     00042000
                   LLIM ~ PARTIT[M |2-1];                               00042100
                   ULIM ~ PARTIT[M |2];                                 00042200
                   FOR I~1 STEP 1 UNTIL 3 DO                            00042300
                   FOR K~LLIM STEP 1 UNTIL ULIM DO                      00042400
                   READ(SUMS);                                          00042500
              END;                                                      00042600
                   NP~NP+1; COMMENT DETERMINE PASS AND POSITION SUMS;   00042700
              END  RESTART1;                                            00042800
PROCEDURE          DIAG(A);                                             00042900
ARRAY              A[0];                                                00043000
              BEGIN                                                     00043100
COMMENT  THIS      PROCEDURE COMPUTES Y ,Y*2,N, ETC. FOR 0,0,  E,1   2,200043200
                   .... NVAR1,NVAR1  -  RESULTS  ARE RETAINED IN        00043300
                   MEMORY AND FINALLY WRITTEN ON THE BACK END OF SC[R1];00043400
                   A[0]~1; COMMENT FOR COMPATIRILITY WHEN WRITTEN       00043500
                           ON TAPE - INDICATES RECORD IS COMPLETE ;     00043600
                   A[6]~A[1]~A[1]+SCORE[M]; COMMENT  SUM Y & X  ;       00043700
                   A[4]~A[3]~A[2]~SCORE[M]*2+A[2]; COMMENT YSQ,XSQ,& XY;00043800
                   A[5]~1+A[5];  COMMENT N;                             00043900
              END  DIAG;                                                00044000
ARRAY              DIAGN[0:NVAR1,0:6];                                  00044100
PROCEDURE          ROW (D,D1,D2);                                       00044200
                   ARRAY D,D1,D2[0] ;  COMMENT WAS *;                   00044300
              BEGIN                                                     00044400
                   T2 ~ SCORE[I];                                       00044500
                   FOR  J ~ I+1 STEP 1 UNTIL NVAR1 DO                   00044600
                   IF SCORE[J] ! NULL THEN                              00044700
              BEGIN                                                     00044800
                   T1 ~ SCORE[J] ;                                      00044900
                   D[J]  ~ T1 * 2 + D[J] ;      COMMENT SUM Y SQ ;      00045000
                   D1[J] ~ D1[J] + T1 ;         COMMENT SUM Y ;         00045100
                   D2[J] ~ T2 | T1  + D2[J] ;   COMMENT SUM XY ;        00045200
              END;                                                      00045300
                   FOR J ~ I-1 STEP -1 UNTIL 0  DO                      00045400
                   IF SCORE[J] ! NULL THEN                              00045500
              BEGIN                                                     00045600
                   T1 ~ SCORE[J] ;                                      00045700
                   D[J]  ~ D[J]+1;              COMMENT  N  ;           00045800
                   D1[J] ~ T1*2 + D1[J] ;        COMMENT  SUM X SQ;     00045900
                   D2[J] ~ D2[J] + T1 ;         COMMENT  SUM X   ;      00046000
              END                                                       00046100
              END  ROW ;                                                00046200
PROCEDURE          CORR ;                                               00046300
              BEGIN                                                     00046400
LABEL              L;                                                   00046500
                   LLIM ~ PARTIT[NP|2-1];                               00046600
                   ULIM ~ PARTIT[NP|2];                                 00046700
       L:     BEGIN                                                     00046800
COMMENT SAVE REMOVED HERE BY WHE 1-25-67    ;                           00046900
ARRAY              XSQY,NYSQ,XXY[LLIM:ULIM,0:NVAR1];                    00047000
LABEL              L1;                                                  00047100
                   FOR K~ 1 STEP 1 UNTIL NCASE DO                       00047200
              BEGIN                                                     00047300
               IF DIR THEN READ(MASTER,NVAR,SCORE[*]) ELSE              00047400
               READ REVERSE (MASTER,NVAR,SCORE[*]);                     00047500
COMMENT            FORM XSQ,X,ETC FOR ELEMENTS 0,0  1,1  2,2  ETC,"ONCE"00047600
                   IS TRUE ON 1ST TAPE PASS THEN BECOMES FALSE SO THAT, 00047700
                   IF NPART>1 THEN ELEMENTS FORMED ONLY ONCE;           00047800
                   IF ONCE THEN FOR M~0 STEP 1 UNTIL NVAR1 DO           00047900
                   IF SCORE[M]! NULL THEN DIAG(DIAGN[M,*]);             00048000
                   FOR I~ LLIM STEP 1 UNTIL ULIM DO                     00048100
                   IF SCORE[I] ! NULL THEN                              00048200
                   ROW(NYSQ[I,*],XSQY[I,*],XXY[I,*] );                  00048300
              END;                                                      00048400
                   FOR  K ~ LLIM STEP 1 UNTIL ULIM DO                   00048500
                   WRITE(SUMS,NVAR,XSQY[K,*])  ;                        00048600
                   FOR  K ~ LLIM STEP 1 UNTIL ULIM DO                   00048700
                   WRITE(SUMS,NVAR,NYSQ[K,*]);                          00048800
                   FOR  K ~ LLIM STEP 1 UNTIL ULIM DO                   00048900
                   WRITE(SUMS,NVAR,XXY[K,*]);                           00049000
       L1:                                                              00049100
                   IF NP !NPART THEN                                    00049200
              BEGIN                                                     00049300
                   WRITE(PUNCH,FER,NP);                                 00049400
                   CLOSE(PUNCH,RELEASE);                                00049500
                   NP~NP+1;                                             00049600
                   ONCE~FALSE;                                          00049700
                   DIR ~ NOT DIR ;                                      00049800
                   LLIM ~ PARTIT[NP|2-1] ;                              00049900
                   ULIM ~ PARTIT[NP|2] ;                                00050000
                   GO TO L ;                                            00050100
              END;                                                      00050200
              END  INNER BLOCK ;                                        00050300
               END  ;    COMMENT END PROCEDURE CORR;                    00050400
PROCEDURE ONECOMB;                                                      00050500
          BEGIN                                                         00050600
ARRAY         NN,XSQ,X,YSQ,Y,XY[0:NVAR1 ];                              00050700
REAL              XT,T1,T2;                                             00050800
INTEGER        N;                                                       00050900
REAL               DUMMY,SUMY,SUMYSQD,SUMXY,SUMXSQD,SUMX,               00051000
                   CORR,MEANX,MEANY,SIGMAX,SIGMAY;                      00051100
LIST               LST2 (I,J,CORR,N,MEANX,MEANY,SIGMAX,SIGMAY);         00051200
LIST               LST3 (J,I,CORR,N,MEANY,MEANX,SIGMAY,SIGMAX);         00051300
FORMAT OUT         FMT1 (X2,"IDENT",X8,"CORR",X9,"N",X11,"MEAN (X)",    00051400
                         X12,"MEAN (Y)",X11,"SIGMA (X)",X11,            00051500
                         "SIGMA (Y)"//);                                00051600
FORMAT OUT         FMT2 (I4,X1 ,I4,X4,F7.4,X6,I4,X7,F13.4,X7,F13.4,     00051700
                         X7,F13.4,X7,F13.4  );                          00051800
LABEL              L;                                                   00051900
BOOLEAN        B;                                                       00052000
COMMENT  THIS PROCEDURE COMPUTES AND PRINTS ALL COMBINATIONS OF I,J FOR 00052100
       EITHER I HELD CONSTANT AND J=I,...NVAR OR FOR J HELD CONSTANT    00052200
       AND I=1,2,....J.   OPTION COMES FROM THE INPUT CARD. "MASTER" IS 00052300
       READ AND ALL COMBINATIONS ARE FORMED AND PRINTED (ALL            00052400
       COMBINATIONS ARE RETAINED IN MEMORY;                             00052500
               ELEMN~ELEMN-1;                                           00052600
               IF ELEMN}0 THEN                                          00052700
          BEGIN                                                         00052800
               LLIM~ELEMN; ULIM~NVAR1; B~FALSE;                         00052900
               ELEMNO~ELEMN;                                            00053000
          END  ELSE                                                     00053100
          BEGIN                                                         00053200
               LLIM~0; ULIM~ELEMNO;  B~TRUE;                            00053300
          END;                                                          00053400
               I~ELEMNO;                                                00053500
               TOP~0;                                                   00053600
               FOR K~ 1 STEP 1 UNTIL NCASE DO                           00053700
          BEGIN                                                         00053800
               READ(MASTER,NVAR,SCORE[*]);                              00053900
               IF SCORE[I] ! NULL THEN                                  00054000
          BEGIN                                                         00054100
               T2~SCORE[I];                                             00054200
               FOR J~LLIM STEP 1 UNTIL ULIM DO                          00054300
               IF SCORE[J] ! NULL THEN                                  00054400
          BEGIN                                                         00054500
               T1~SCORE[J];                                             00054600
              NN[J]~NN[J]+1;  XSQ[J]~T1*2+XSQ[J];  X[J]~T1+X[J];        00054700
               YSQ[J]~T2*2+YSQ[J];  Y[J]~Y[J]+T2;  XY[J]~XY[J]+T1|T2;   00054800
          END;                                                          00054900
          END;                                                          00055000
          END;                                                          00055100
               CLOSE(MASTER,SAVE);                                      00055200
               J~ELEMNO+1;  WRITE(LINE,HED);                            00055300
               FOR I~LLIM STEP 1 UNTIL ULIM DO                          00055400
          BEGIN                                                         00055500
               IF B THEN                                                00055600
               WRITE(LINE,FM,I+1,J,X[I],XSQ[I],Y[I],YSQ[I],XY[I],NN[I]) 00055700
                ELSE                                                    00055800
               WRITE(LINE,FM,J,I+1,Y[I],YSQ[I],X[I],XSQ[I],XY[I],NN[I]);00055900
               TOP~TOP+1;                                               00056000
               IF TOP>54 THEN                                           00056100
          BEGIN                                                         00056200
               WRITE(LINE[PAGE]);                                       00056300
               WRITE(LINE,HED);                                         00056400
               TOP~0;                                                   00056500
          END;                                                          00056600
          END;                                                          00056700
               J~ELEMNO+1;                                              00056800
               WRITE(LINE[PAGE]);   WRITE(LINE,FMT1);                   00056900
               TOP~0;                                                   00057000
               FOR M~LLIM STEP 1 UNTIL ULIM DO                          00057100
          BEGIN                                                         00057200
               N~NN[M];   SUMY~Y[M];  SUMYSQD~YSQ[M];  SUMXY~XY[M];     00057300
               SUMXSQD~XSQ[M];  SUMX~X[M];                              00057400
               IF N = 0 THEN                                            00057500
         BEGIN                                                          00057600
               MEANX~MEANY~SIGMAX~SIGMAY~CORR~0;  GO TO L;              00057700
         END;                                                           00057800
                   MEANY ~ SUMY / N; MEANX ~ SUMX / N;                  00057900
                   SIGMAX ~ SQRT ((SUMXSQD / N) - (MEANX * 2));         00058000
                   SIGMAY ~ SQRT ((SUMYSQD / N) - (MEANY * 2));         00058100
                  XT~SUMXY-(SUMX | SUMY)/N;                             00058200
                   T1~SUMXSQD - (SUMX*2)/N;                             00058300
                   T2~SUMYSQD-(SUMY*2)/N;                               00058400
                   IF T1=0 OR T2=0 THEN CORR~0 ELSE                     00058500
                   CORR~XT/(SQRT(T1) | SQRT(T2));                       00058600
               I~M+1;                                                   00058700
     L:                                                                 00058800
               IF B THEN                                                00058900
               WRITE(LINE,FMT2,LST2)                                    00059000
                ELSE                                                    00059100
               WRITE(LINE,FMT2,LST3);                                   00059200
               TOP~TOP+1;                                               00059300
               IF TOP>54 THEN                                           00059400
          BEGIN                                                         00059500
          WRITE(LINE[PAGE]);  WRITE(LINE,FMT1);                         00059600
               TOP~0                                                    00059700
          END;                                                          00059800
          END;                                                          00059900
               GO TO STP;                                               00060000
          END  ONECOMB;                                                 00060100
COMMENT            STO COL. NOS. FOR PARTITIONED MATRIX IN PARTIT ;     00060200
                   NULL ~ "NULL";                                       00060300
               IF ELEMN}1 OR ELEMNO>0 THEN ONECOMB;                     00060400
                   ONCE~TRUE;                                           00060500
                   TEMP ~ NPART | 2;                                    00060600
                   PARTIT[1] ~ 0;                                       00060700
                   PARTIT[2] ~ (NVAR DIV NPART) -1 ;                    00060800
                   REM ~ NVAR MOD NPART ;                               00060900
                   FOR I ~ 3 STEP 2 UNTIL TEMP DO                       00061000
              BEGIN                                                     00061100
                   PARTIT[I] ~ PARTIT[I-1] + 1;                         00061200
                   PARTIT[I+1] ~ PARTIT[2] + PARTIT[I] ;                00061300
              END;                                                      00061400
                   IF REM ! 0 THEN PARTIT[I-1] ~ PARTIT[I-1] + REM;     00061500
                   DIR ~ TRUE;                                          00061600
                   NP ~ 1 ;                                             00061700
                   IF RSTT = 2 THEN                                     00061800
              BEGIN                                                     00061900
                   FOR K~1 STEP 1 UNTIL NCASE DO                        00062000
              BEGIN                                                     00062100
               READ(MASTER,NVAR,SCORE[*]);                              00062200
                   FOR M~0 STEP 1 UNTIL NVAR1 DO IF SCORE[M]!NULL       00062300
                   THEN DIAG(DIAGN[M,*]);                               00062400
              END;                                                      00062500
               CLOSE(MASTER,SAVE);                                      00062600
              END;                                                      00062700
                   GO TO WHICH[RSTT+1];                                 00062800
COMMENT       """""""""""""  START OF PHASE 1  """""""""""""            00062900
                   THIS PHASE ACTUALLY BEGINS WHEN FILE SUMS IS OPENED  00063000
                   AND WRITTEN ON       ;                               00063100
    RSTART1:       RESTART1;                                            00063200
COMMENT       """""""""""""  START OF PHASE 0  """""""""""""           ;00063300
    RSTART0:                                                            00063400
                   CORR;                                                00063500
               CLOSE(MASTER,SAVE);                                      00063600
               CLOSE(SUMS,SAVE);                                        00063700
COMMENT            REMOVE WRITE RING AS A SAFETY PRECAUTION  AND READ   00063800
               TAPE JUST LOCKED;                                        00063900
COMMENT                                                                 00064000
  **********************   INNER BLOCK   ***************************** ;00064100
COMMENT       """""""""""""  START OF PHASE 2  """""""""""""           ;00064200
    RSTART2:                                                            00064300
                                T~0 ; T1~1;                             00064400
                   FOR CT ~ 1 STEP 1 UNTIL NPART DO                     00064500
              BEGIN                                                     00064600
LABEL              RD,REST,INIT;                                        00064700
COMMENT SAVE REMOVED HERE BY WHE 1-25-67;                               00064800
ARRAY              D,D1,D2[PARTIT[CT|2-1]:PARTIT[CT|2],0:NVAR1],        00064900
                   A[0:6];                                              00065000
                   LLIM ~ PARTIT[CT|2-1];                               00065100
                   TOP ~ ULIM ~ PARTIT[CT|2] ;                          00065200
                   M1 ~ ULIM;   M ~ LLIM;                               00065300
                   FOR I~M STEP 1 UNTIL M1 DO READ(SUMS,NVAR,D[I,*]);   00065400
                   FOR I~M STEP 1 UNTIL M1 DO READ(SUMS,NVAR,D1[I,*]);  00065500
                   FOR I~M STEP 1 UNTIL M1 DO READ(SUMS,NVAR,D2[I,*]);  00065600
               IF CT=1 THEN GO TO INIT;                                 00065700
                   REWIND(SC[T]);     COMMENT REW HERE SO THAT FINAL    00065800
                                   TAPE IS NOT RESOUND ;                00065900
                   M2~0;  M3~PARTIT[CT|2-1]-1;                          00066000
COMMENT            A[0] IS TRUE IF RECORD IS COMPLETE;                  00066100
                   FOR I ~ M2 STEP 1 UNTIL M3 DO                        00066200
              BEGIN                                                     00066300
     RD:           READ(SC[T][NO],7,A[*]);                              00066400
                   IF BOOLEAN (A[0]) THEN                               00066500
              BEGIN                                                     00066600
                   WRITE(SC[T1],7,A[*]) ;  READ(SC[T]);                 00066700
                   GO TO RD ;                                           00066800
              END;                                                      00066900
                   FOR J ~ M STEP 1 UNTIL M1 DO                         00067000
               BEGIN                                                    00067100
                   READ(SC[T],7,A[*]);                                  00067200
                   A[4] ~ D[J,I]; A[5] ~ D1[J,I];  A[6]~D2[J,I];        00067300
                   A[0] ~ 1;                                            00067400
                   WRITE(SC[T1],7,A[*]);                                00067500
              END;                                                      00067600
     REST:         FOR J ~ TOP +1 STEP 1 UNTIL NVAR1  DO                00067700
              BEGIN                                                     00067800
                   READ(SC[T],7,A[*]);                                  00067900
                   WRITE(SC[T1],7,A[*]);                                00068000
              END;                                                      00068100
              END;                                                      00068200
COMMENT        FORM COMBINATIONS FROM TAPE SUMS;                        00068300
     INIT:         FOR I ~ M    STEP 1 UNTIL M1   DO                    00068400
              BEGIN                                                     00068500
                   FOR J ~ I+1 STEP 1 UNTIL M1   DO                     00068600
              BEGIN                                                     00068700
                   A[0]~1; A[1]~D [I,J];A[2]~D1[I,J]; A[3]~D2[I,J];     00068800
                   A[4]~D[J,I]; A[5]~ D1[J,I]; A[6]~D2[J,I];            00068900
                   WRITE (SC[T1],7,A[*]);                               00069000
              END;                                                      00069100
                   A[4]~A[5]~A[6]~A[0]~ 0;                              00069200
                   FOR J ~ ULIM+1 STEP 1 UNTIL NVAR1  DO                00069300
              BEGIN                                                     00069400
                   A[1]~D[I,J]; A[2]~D1[I,J]; A[3]~D2[I,J];             00069500
                   WRITE(SC[T1],7,A[*]);                                00069600
              END;                                                      00069700
              END;                                                      00069800
                   REWIND(SC[T]);                                       00069900
                   DOUBLE(T,T1,~,T1,T);                                 00070000
              END;                                                      00070100
COMMENT            END OF INNER BLOCK;                                  00070200
COMMENT            FINAL TAPE IS SC[T];                                 00070300
COMMENT            WRITE DIAG ELEMENTS ON BACK END OF  SC[T] TAPE ;     00070400
                   FOR I ~ 0 STEP 1 UNTIL NVAR1 DO                      00070500
                   WRITE(SC[T],7,DIAGN[I,*]);                           00070600
                  CLOSE(SC[T],SAVE);                                    00070700
               CLOSE(SUMS,SAVE);                                        00070800
COMMENT            REMOVE WRITE RING AS A SAFETY PRECAUTION  AND READ   00070900
               TAPE JUST LOCKED;                                        00071000
                   CLOSE(SC[T1],RELEASE);                               00071100
               IF PRINTORNOT!1 THEN GO TO RSTART4;                      00071200
COMMENT       """""""""""""  START OF PHASE 3  """""""""""""           ;00071300
    RSTART3:                                                            00071400
COMMENT            DETERMINE WHICH TAPE WAS FINAL OUTPUT TAPE AND PRINT 00071500
                   IT - THIS IS REDUNDANT BUT IS NEEDED IN CASE OF A    00071600
                   RESTART FOR PHASE 3 OR 4 ;                           00071700
                   IF BOOLEAN(NPART) THEN T~1 ELSE T~0;                 00071800
                   WRITE(LINE[PAGE]);                                   00071900
                   DATELINE("MISDAT");                                  00072000
                   WRITE(LINE,HED);  TOP~ 0;                            00072100
                   FOR M ~ 1 STEP 1 UNTIL NVAR1 DO                      00072200
               BEGIN                                                    00072300
                   FOR M1~ M+1 STEP 1 UNTIL NVAR DO                     00072400
              BEGIN                                                     00072500
                   READ(SC[T],7,A1[*]);                                 00072600
                   WRITE(LINE,FM,M,M1,A1[6],A1[4],A1[1],A1[2],A1[3],    00072700
                          A1[5]);                                       00072800
                   TOP ~ TOP+1;  IF TOP>46 THEN                         00072900
              BEGIN                                                     00073000
                   TOP~0; WRITE(LINE[PAGE]);                            00073100
                   DATELINE("MISDAT");                                  00073200
               WRITE(LINE,HED);                                         00073300
              END;                                                      00073400
               END;                                                     00073500
                   WRITE(LINE);  TOP ~ TOP+1;                           00073600
               IF TOP>46 THEN BEGIN TOP~0; WRITE(LINE[PAGE]);  %VR 1/69 00073610
               WRITE(LINE,HED); END;                           %VR 1/69 00073620
              END;                                                      00073700
                   TOP ~ TOP+2;                                         00073800
               IF TOP>46 THEN BEGIN TOP~0; WRITE(LINE[PAGE]);  %VR 1/69 00073810
               WRITE(LINE,HED); END;                           %VR 1/69 00073820
                   WRITE(LINE[DBL]); COMMENT GET DIAG ELEMENTS;         00073900
                   FOR M~1 STEP 1 UNTIL NVAR DO                         00074000
              BEGIN                                                     00074100
                   READ(SC[T],7,A1[*]);                                 00074200
                   WRITE(LINE,FM,M,M ,A1[6],A1[4],A1[1],A1[2],A1[3],    00074300
                          A1[5]);                                       00074400
                   TOP~TOP+1; IF TOP > 46 THEN                          00074500
              BEGIN                                                     00074600
                   TOP ~ 0;  WRITE(LINE[PAGE]);                         00074700
                   DATELINE("MISDAT");                                  00074800
                   WRITE(LINE,HED);                                     00074900
              END;                                                      00075000
              END;                                                      00075100
                   REWIND(SC[T]);                                       00075200
COMMENT            SUM Y, SUM Y SQ, SUM XY, ETC JUST PRINTED, REWIND    00075300
                   TPAE AND READ,COMPUTE, AND PRINT MEAN, STANDARD      00075400
                   DEVIATION, ETC.  ;                                   00075500
COMMENT       """""""""""""  START OF PHASE 4  """""""""""""           ;00075600
    RSTART4:                                                            00075700
COMMENT            DETERMINE WHICH TAPE WAS FINAL OUTPUT TAPE AND PRINT 00075800
                   IT - THIS IS REDUNDANT BUT IS NEEDED IN CASE OF A    00075900
                   RESTART FOR PHASE 3 OR 4 ;                           00076000
                   IF BOOLEAN(NPART) THEN T~1 ELSE T~0;                 00076100
              BEGIN                                                     00076200
COMMENT            R M WATTERS, SUBPROGRAM TO COMPUTE AND PRINT THE     00076300
                   MEAN OF X ANY Y, STANDARD DEVIATION OF X AND Y, AND  00076400
                   THE COEFFICIENT OF CORRELATION;                      00076500
FORMAT OUT         FMT1 (X2,"IDENT",X8,"CORR",X9,"N",X11,"MEAN (X)",    00076600
                         X12,"MEAN (Y)",X11,"SIGMA (X)",X11,            00076700
                         "SIGMA (Y)"//);                                00076800
FORMAT OUT         FMT2 (I4,X1 ,I4,X4,F7.4,X6,I4,X7,F13.4,X7,F13.4,     00076900
                         X7,F13.4,X7,F13.4  );                          00077000
INTEGER            I,J,N,PAGENO,LINENO;                                 00077100
REAL ARRAY     CM[0:NVAR];                                              00077200
REAL              XT,T1,T2;                                             00077300
REAL               DUMMY,SUMY,SUMYSQD,SUMXY,SUMXSQD,SUMX,               00077400
                   CORR,MEANX,MEANY,SIGMAX,SIGMAY;                      00077500
LIST               LST1 (DUMMY,SUMY,SUMYSQD,SUMXY,SUMXSQD,N,SUMX),      00077600
                   LST2 (I,J,CORR,N,MEANX,MEANY,SIGMAX,SIGMAY);         00077700
LABEL              EOF;                                                 00077800
PROCEDURE          SOLVER;                                              00077900
              BEGIN                                                     00078000
LABEL              L;                                                   00078100
               IF N = 0 THEN                                            00078200
         BEGIN                                                          00078300
               MEANX~MEANY~SIGMAX~SIGMAY~CORR~0;  GO TO L;              00078400
         END;                                                           00078500
                   MEANY ~ SUMY / N; MEANX ~ SUMX / N;                  00078600
                   SIGMAX ~ SQRT ((SUMXSQD / N) - (MEANX * 2));         00078700
                   SIGMAY ~ SQRT ((SUMYSQD / N) - (MEANY * 2));         00078800
                  XT~SUMXY-(SUMX | SUMY)/N;                             00078900
                   T1~SUMXSQD - (SUMX*2)/N;                             00079000
                   T2~SUMYSQD-(SUMY*2)/N;                               00079100
                   IF T1=0 OR T2=0 THEN CORR~0 ELSE                     00079200
                   CORR~XT/(SQRT(T1) | SQRT(T2));                       00079300
     L:                                                                 00079400
              END  OF PROCEDURE SOLVER;                                 00079500
PROCEDURE          NEWPAGE;                                             00079600
              BEGIN                                                     00079700
                   WRITE (LINE[PAGE]);                                  00079800
                   DATELINE("MISDAT");                                  00079900
                   LINENO ~ 0;                                          00080000
                   WRITE (LINE,FMT1);                                   00080100
              END  OF PROCEDURE NEWPAGE;                                00080200
PROCEDURE          PROCESSOR;                                           00080300
              BEGIN                                                     00080400
                   READ (SC[T],*,LST1)[EOF];                            00080500
                   SOLVER;                                              00080600
                   WRITE (LINE,FMT2,LST2);                              00080700
                   LINENO ~ LINENO + 1;                                 00080800
                   IF LINENO } 47 THEN NEWPAGE;                         00080900
              END  OF PROCEDURE PROCESSOR;                              00081000
COMMENT            *****START OF PROGRAM*****;                          00081100
                   NEWPAGE;                                             00081200
               IF CORROUT THEN                                          00081300
          BEGIN                                                         00081400
               FOR I~1 STEP 1 UNTIL NVAR1 DO                            00081500
          BEGIN                                                         00081600
               WRITE(LINE);                                             00081700
               LINENO~LINENO+1;                                         00081800
               IF LINENO}47 THEN NEWPAGE;                      %VR 1/69 00081810
               CM[I]~1.0;                                               00081900
               FOR J~I+1 STEP 1 UNTIL NVAR DO                           00082000
          BEGIN                                                         00082100
               PROCESSOR;                                               00082200
               CM[J]~CORR                                               00082300
          END;                                                          00082400
               WRITE(TEM   ,*,  FOR J~1 STEP 1 UNTIL NVAR DO CM[J]);    00082500
          END  OF ILOOP FOR CORROUT                                     00082600
          END                                                           00082700
               ELSE                                                     00082800
                   FOR I ~ 1 STEP 1 UNTIL NVAR1 DO                      00082900
              BEGIN                                                     00083000
                   WRITE (LINE);                                        00083100
                   LINENO ~ LINENO + 1;                                 00083200
               IF LINENO}47 THEN NEWPAGE;                               00083300
                   FOR J ~ I + 1 STEP 1 UNTIL NVAR DO                   00083400
                   PROCESSOR;                                           00083500
              END  OF ILOOP;                                            00083600
               REWIND(TEM   );                                          00083700
COMMENT            START OF IDENTITY MATRIX;                            00083800
                   WRITE(LINE[DBL]); LINENO~LINENO+2;                   00083900
                   FOR I ~ 1 STEP 1 UNTIL NVAR DO                       00084000
              BEGIN                                                     00084100
                   J ~ I; PROCESSOR;                                    00084200
              END  OF ILOOP;                                            00084300
EOF:          END  OF BLOCK WATTERS;                                    00084400
              END  NESTED BLOCK NUMBER 1;                               00084500
               IF NOT CORROUT THEN GO TO STP ;                          00084600
COMMENT ******** NEW BLOCK TO WRITE FULL CORR MATRIX ON TAPE;           00084700
          BEGIN                                                         00084800
REAL ARRAY     CMT[0:NVAR,0:NVAR];                                      00084900
FORMAT         FCORMAT(100O) ;                                          00085000
FILE           CORMAT 2(2,  100,SAVE 10);                               00085100
               FOR I~1 STEP 1 UNTIL NVAR1 DO                            00085200
BEGIN                                                                   00085300
               READ(TEM   ,*,  FOR J~1 STEP 1 UNTIL NVAR DO CMT[I,J]);  00085400
END;                                                                    00085500
               CLOSE(TEM ,RELEASE);                                     00085600
               FOR I~1 STEP 1 UNTIL NVAR1 DO                            00085700
               FOR J~I+1 STEP 1 UNTIL NVAR DO                           00085800
               CMT[J,I]~CMT[I,J];                                       00085900
               CMT[NVAR,NVAR]~1.0;                                      00086000
               WRITE(CORMAT,FCORMAT,NVAR) ;                             00086100
               FOR I~1 STEP 1 UNTIL NVAR DO                             00086200
               WRITE(CORMAT,FCORMAT,FOR J~1 STEP 1 UNTIL NVAR DO        00086300
                     CMT[I,J]);                                         00086400
          END;                                                          00086500
    STP:                                                                00086600
               DATELINE(0);                                             00086700
          END;                                                          00086800
               END;                                                     00086900
FINIS:         END OF PROGRAM.                                          00087000
                LAST CARD ON CRDIMG TAPE                                99999999
