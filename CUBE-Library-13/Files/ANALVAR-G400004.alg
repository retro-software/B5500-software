                  BEGIN                                                 00000000
COMMENT        ANALVAR/STATMAN;                                         00000100
COMMENT                                                                 00000200
                                                                        00000300
                    THIS PROGRAM WAS WRITTEN IN JULY 1967 BY            00000400
                    WALTER RASBACH AND REVISED BY V RECK IN SEPT        00000500
                    1967 WHEN IT WAS FOUND THAT WALTERS FORMAT          00000600
                    GENERATOR WOULD TAKE ONLY 15 VARIABLES              00000700
                    FOREST CARHARTS OBJECTFMTGEN WAS INSERTED           00000800
                    INTO THE PROGRAM IN PLACE OF WALTERS.               00000900
                                                                        00001000
                    THIS PROGRAM DOES A ONE-WAY ANALYSIS OF VARIANCE    00001100
               WITH UNEQUAL NUMBERS OF SUBJECTS IN EACH SAMPLE GROUP    00001200
               AND A VARIABLE NUMBER OF SEPARATE DEPENDANT VARIABLES.   00001300
                                                                        00001400
                    THE PROGRAM CHECKS FOR BLANK FIELDS IN THE DATA AND 00001500
               DOES NOT COUNT SUCH DATA IN THE SAMPLE SIZE.             00001600
                                                                        00001700
                    ALL DATA IS ASSUMED TO BE POITIVE OR ZERO AND ANY   00001800
               DATA THAT IS LESS THAN ZERO WILL BE TREATED AS A         00001900
               BLANK FIELD.                                             00002000
                    THE PROGRAM REQUIRES TWO INPUT FILES NAMED "CARD"   00002100
               AND "INPUT".  BOTH OF THESE FILES MAY BE CARD-READER     00002200
               FILES BUT IN THIS CASE THE FILE "CARD" MUST PRECEED THE  00002300
               FILE "INPUT".  THE FILE "CARD" IS A PARAMETER FILE AND   00002400
               THE FILE INPUT IS THE FILE WHICH DATA IS READ FROM.  THE 00002500
               FILE "INPUT" IS A SEPARATE FILE TO PERMIT  DATA READ     00002600
               FROM TAPE OR OTHER INPUT DEVICES.                        00002700
                                                                        00002800
                    THE ORDER OF THE PARAMETER CARDS IS :               00002900
                                                                        00003000
           CARD #            COL.                CONTENTS               00003100
                                                                        00003200
             1               1-2       NUMBER OF SAMPLE GROUPS          00003300
                             5-6       NUMBER OF DEPENDANT VARIABLES    00003400
                             9-11      SIZE OF BUFFER (PHYSICAL RECORD) 00003500
                                        FOR THE FILE INPUT              00003600
                             14-16     SIZE OF LOGICAL RECORD FOR THE   00003700
                                        FILE INPUT                      00003800
                    IF THE LAST TWO ITEMS ARE LEFT BLANK, THE FILE      00003900
                    INPUT IS ASSUMED TO BE A CARD-READER FILE.          00004000
                                                                        00004100
             2      TITLE CARD  COLUMN 40 ON THE CARD CORRESPONDS TO    00004200
                    COLUMN 60 ON THE LINE PRINTER.                      00004300
                                                                        00004400
             3      THIS CARD IS A FORMAT CARD.  IT SHOULD GIVE THE     00004500
                    FORMAT IN WHICH DATA IS TO BE READ FROM THE FILE    00004600
                    INPUT.  THERE MUST BE EITHER A LEFT PARENTHESIS IN  00004700
                    COLUMN 1 OR THE WORD "FORMAT" IN COLUMNS 1-6.  IF   00004800
                    THE FORMAT OPTION IS USED ALL COMMENTS BEFORE THE   00004900
                    FIRST LEFT PARENTHISIS OR SEMICOLON ARE IGNORED.    00005000
                    IF A SEMICOLON IS ENCONTERED BEFORE THE LEFT        00005100
                    PARENTHISIS OR IF THE FORMAT BETWEEN THE FIRST LEFT 00005200
                    PARENTHISIS AND THE CORRESPONDING RIGHT PARENTHISIS 00005300
                    IS NOT SYNTACTICALLY CORRECT ACCORDING TO B5500     00005400
                    ALGOL OR IF THE LAST RIGHT PARENTHISIS IS NOT       00005500
                    FOLLOWED BY A SEMICOLON OR THE GENERATED FORMAT IS  00005600
                    LARGER THAN 128 WORDS IN LENGTH THEN THE PROGRAM    00005700
                    IS TERMINATED BY AN EXPONENT OVERFLOW.              00005800
                    THE FORMAT GENERATOR ALLOWS ONLY THE PHRASES X,I,F  00005900
                    OR R.                                               00006000
                    AFTER THE FIRST ITEM ("FORMAT" OR "(" ),BLANKS MAY  00006100
                    BE FREELY USED EXCEPT IMBEDED IN A NUMBER.  IF THE  00006200
                    FORMAT REQUIRES MORE THAN ONE CARD THE GENERATOR    00006300
                    WILL READ CARDS UNTIL IT HAS READ A CARD WITH A     00006400
                    SEMICOLON ON IT.                                    00006500
                    EXAMPLE :                                           00006600
                    FORMAT(I2/10(I2,X2),X7,F6.4,3I3//),                 00006700
                                                                        00006800
             4      THIS CARD CONTAINS THE NUMBER OF SUBJECTS IN EACH   00006900
                    SAMPLE GROUP, 10 TO A CARD BY THE FORMAT (10I4).    00007000
                    IF THERE ARE MORE THAN 10 SAMPLE GROUPS, THE SIZES  00007100
                    ARE CONTINUED ON SUBSEQUENT CARDS, 10 TO A CARD.   ;00007200
                                                                        00007300
FILE IN                CARD(2,10);                                      00007400
FILE OUT       LINE  15(2,15);                                          00007500
REAL            NCR,LCR;                                                00007600
REAL           NGRPS,NVAR,BUFSIZE,LOGSIZE,NVR,NBUF,A,B,C,NST,D,E,F,T;   00007700
REAL           G;                                                       00007800
INTEGER        I;                                                       00007900
INTEGER                ERROR;                                           00008000
COMMENT    INFORM IS THE FORMAT IN WHICH THE GENERATED FORMAT IS PLACED;00008100
                                                                        00008200
FORMAT    INFORM(///////////////////////////////////////////////////////00008300
/////////////////////////////////////////////////////////////////////); 00008400
FORMAT                                                                  00008500
                       FOR2(I2,X2,I2,X2,I3,X2,I3),                      00008600
                       FOR3(X*,"                                    ",  00008700
                        "                                            "),00008800
                       FOR4(X50,"ANALYSIS OF VARIANCE"/                 00008900
                        X47,"WITH UNEQUAL SAMPLE SIZES."/),             00009000
                       FOR5(///X50,"NUMBER OF GROUPS=",I2//             00009100
                        X50,"NUMBER OF VARIABLES=",I2///                00009200
                        X53,"INPUT FORMAT:"/),                          00009300
                       FOR6(A6),FOR7(10I4),                             00009400
                       FOR8(X43,"GROUP ",I2,".  NUMBER OF SUBJECTS ",   00009500
                        I3,"."//"DEP. VAR.",X4,"N",X15,"SUM(X)",X14,    00009600
                        "SUM(X*2)",X17,"MEAN(X)",X15,"STND. DEV."//),   00009700
                       FOR9(I5,X7,I3,X12,R9.2,X13,R9.2,X14,R9.2,        00009800
                        X14,R9.2),                                      00009900
               FOR10("DEPENDANT VARIABLE ",I2,", N=",I4,"."//           00010000
                "SOURCE",X14,"SS.",X11,"DF.",X10,"MN.SQ."//3(A5,        00010100
                X12,R9.2,X7,I4,X8,R9.2/)//"F=",R10.3,X10,               00010200
                        "MEAN="R10.3////////),                          00010300
                       FORERR("DATA ERROR:",3(X4,I4));                  00010400
FORMAT         FRERR("DATA ERROR IN R EDITING PHRASE"/                  00010500
               " PLEASE CHECK YOUR DATA AND FORMAT"),                   00010600
               FRERR1(I4," CARDS IN GROUP ",I4," HAVE BEEN ",           00010700
               "READ"),                                                 00010800
               FINS("INSUFFICIENT DATA FOR READ"),                      00010900
               FINS1(I4," CARDS IN GROUP ",I4," HAVE BEEN",             00011000
               "READ");                                                 00011100
                                                                        00011200
                                                                        00011300
PROCEDURE OBJECTFMTGEN(INFORMAT);FORMAT INFORMAT;BEGIN OWN REAL NCR,NFWD00011400
,ELCLASS;OWN REAL LCR;OWN INTEGER CNT,TCNT,RSLT,F;OWN INTEGER FMAX;OWN R00011500
EAL ARRAY ACCUM[0:9];SAVE OWN REAL ARRAY GENF[0:259];SAVE OWN REAL ARRAY00011600
 IMAG[0:9];OWN REAL ARRAY PRNT[0:19];OWN BOOLEAN ERRTOG;LABEL FINISHED;S00011700
TREAM PROCEDURE TATTLE(F,LINE);VALUE F;BEGIN SI~LOC F;DI~LINE;10(DS~LIT"00011800
 ");DS~9LIT"FMT SIZE ";DS~3DEC;DS~4LIT" WDS";47(DS~2LIT"  ");END OF TATT00011900
LE;PROCEDURE FLAG(ERRNUM);INTEGER ERRNUM;BEGIN STREAM PROCEDURE INSERT(E00012000
RR,LINE,ACCUM,CNT);VALUE ERR,CNT;BEGIN SI~LOC ERR;DI~LINE;10(DS~LIT"X");00012100
DS~16LIT"  SYNTAX ERROR #";DS~3DEC;DS~4LIT"  ..";SI~ACCUM;SI~SI+3;DS~CNT00012200
 CHR;DS~4LIT"..  ";10(DS~LIT"X");36(DS~2LIT"  ");END OF INSERT;INSERT(ER00012300
RNUM,PRNT[0],ACCUM[1],CNT);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END OF FLA00012400
G;PROCEDURE ERR(ERRNUM);INTEGER ERRNUM;BEGIN FLAG(ERRNUM);END;REAL STREA00012500
M PROCEDURE SETUP(CARD,LINE,LCR);BEGIN LOCAL SET1;SI~CARD;DI~LINE;DS~10W00012600
DS;40(DS~2LIT"  ");SI~CARD;SET1~SI;DI~LOC SETUP;SI~LOC SET1;DS~WDS;DI~CA00012700
RD;9(DI~DI+8);SET1~DI;DS~LIT"%";SI~LOC SET1;DI~LCR;DS~WDS;END OF SETUP;R00012800
EAL STREAM PROCEDURE FMTF(FMTIN);BEGIN LOCAL ST;SI~FMTIN;DI~LOC FMTF;ST~00012900
SI;SI~LOC ST;DS~WDS;END OF FMTF;REAL STREAM PROCEDURE EXAMIN(NCR);VALUE 00013000
NCR;BEGIN SI~NCR;DI~LOC EXAMIN;DI~DI+7;DS~CHR;END OF EXAMIN;REAL STREAM 00013100
PROCEDURE CONV(ACCUM,SKP,N);VALUE SKP,N;BEGIN SI~ACCUM;SI~SI+SKP;SI~SI+300013200
;DI~LOC CONV;DS~N OCT;END OF CONV;REAL PROCEDURE CONVERT;BEGIN REAL T;IN00013300
TEGER N;T~CONV(ACCUM[1],TCNT,N~(CNT-TCNT)MOD 8);FOR N~TCNT+N STEP 8UNTIL00013400
 CNT-1DO T~T|100000000+CONV(ACCUM[1],N,8);CONVERT~T;END OF CONVERT;STREA00013500
M PROCEDURE SCAN(NCRV,NCR,ACCUM,CNT,CNTV,RSLT,RSLTV,AC);VALUE NCRV,CNTV,00013600
RSLTV,AC;BEGIN LOCAL ST1,ST2;LABEL DEBLANK,GETCHR,NUMBER,EXIT,FINIS;LABE00013700
L L;SI~NCRV;DI~RSLT;DI~DI+7;CI~CI+RSLTV;GO TO FINIS;GO TO FINIS;GO TO FI00013800
NIS;GO TO NUMBER;GO TO FINIS;GO TO GETCHR;GO TO FINIS;DEBLANK:IF SC=" "T00013900
HEN BEGIN L:SI~SI+1;IF SC=" "THEN GO TO L;END;GO TO FINIS;GETCHR:DS~LIT"00014000
2";TALLY~1;TALLY~1;SI~SI+1;GO TO EXIT;NUMBER:TALLY~63;DS~LIT"3";AC(TALLY00014100
~TALLY+1;IF SC<"0"THEN JUMP OUT TO EXIT;SI~SI+1);EXIT:ST1~TALLY;TALLY~TA00014200
LLY+CNTV;ST2~TALLY;DI~CNT;SI~LOC ST2;DS~WDS;DI~ACCUM;SI~SI-3;DS~3CHR;DI~00014300
DI+CNTV;SI~NCRV;DS~ST1 CHR;FINIS:DI~NCR;ST1~SI;SI~LOC ST1;DS~WDS;END OF 00014400
SCAN;PROCEDURE READACARD;BEGIN READ(CARD,10,IMAG[*]);NCR~SETUP(IMAG[0],P00014500
RNT[0],LCR);WRITE(LINE,15,PRNT[*]);END OF READACARD;PROCEDURE SCANNER;BE00014600
GIN LABEL L;L:SCAN(NCR,NCR,ACCUM[1],CNT,CNT,RSLT,RSLT,63-CNT);IF NCR=LCR00014700
 THEN BEGIN READACARD;GO TO L;END;END OF SCANNER;PROCEDURE NEXTENT;BEGIN00014800
 CNT~ACCUM[1]~0;IF EXAMIN(NCR)=" "THEN BEGIN RSLT~7;SCANNER;END DEBLANK;00014900
IF EXAMIN(NCR){9THEN BEGIN RSLT~3;SCANNER;TCNT~0;IF CNT>4THEN FLAG(140)E00015000
LSE IF ELCLASS~-CONVERT<-1023THEN FLAG(140)END ELSE BEGIN RSLT~5;SCANNER00015100
;ELCLASS~ACCUM[1].[18:6];END;END OF NEXTENT;STREAM PROCEDURE MOVECODE(TE00015200
MP,FINAL,RPT,REM);VALUE RPT,REM;BEGIN LOCAL ST1;SI~TEMP;DI~FINAL;DS~REM 00015300
WDS;ST1~SI;SI~LOC RPT;SI~SI+7;IF SC!"0"THEN BEGIN SI~ST1;RPT(DS~63WDS);E00015400
ND;END OF MOVECODE;PROCEDURE MAXWDS(INFORMAT);FORMAT INFORMAT;BEGIN OWN 00015500
INTEGER CTR,FLG;LABEL RETURN,EX;INTEGER STREAM PROCEDURE WDCTR(FMT,CTR,F00015600
LG);VALUE CTR;BEGIN LOCAL ST1;LABEL SCAN,FND,EXIT;SI~LOC CTR;SI~SI+7;DI~00015700
LOC ST1;DS~4LIT"0000";DI~DI-4;IF SC="0"THEN BEGIN SI~FMT;GO TO SCAN;END;00015800
SI~FMT;CTR(63(SI~SI+8));SCAN:63(IF 4SC=DC THEN JUMP OUT TO FND;TALLY~TAL00015900
LY+1;DI~DI-4;SI~SI+4); ST1~TALLY;                                %PTFFC 00016000
GO TO EXIT; FND: ST1~TALLY; SI~SI-4; DI~FLG; DS~WDS; EXIT:    %PTFFC    00016100
  SI~LOC ST1;DI~LOC WDCTR;DS~WDS;END OF WDCTR;FMAX~CTR~FLG~0;RETURN:FMAX00016200
~FMAX+WDCTR(INFORMAT,CTR,FLG);IF FLG!0THEN GO TO EX;CTR~CTR+1;GO TO RETU00016300
RN;EX:END OF MAXWDS;STREAM PROCEDURE LARGER(LINE,F);VALUE F;BEGIN SI~LOC00016400
 F;DI~LINE;10(DS~LIT"X");DS~41LIT"  FORMAT TOO LARGE (RECEIVER FMT SIZE 00016500
IS ";DS~3DEC;DS~9LIT" WORDS)  ";10(DS~LIT"X");47(DS~LIT" ");END OF LARGE00016600
R;PROCEDURE GETINT;BEGIN NEXTENT;IF ELCLASS~-ELCLASS<0THEN BEGIN FLAG(1300016700
7);ELCLASS~0END END GETINT;INTEGER PROCEDURE DIVIDE(NUMBER,P1,P2);VALUE 00016800
NUMBER;INTEGER P1,P2,NUMBER;BEGIN IF NUMBER<0THEN BEGIN FLAG(138);NUMBER00016900
~0END;P1~IF NUMBER<8THEN NUMBER ELSE 8;NUMBER~NUMBER-P1;P2~IF NUMBER<8TH00017000
EN NUMBER ELSE 8;DIVIDE~NUMBER-P2 END DIVIDE;STREAM PROCEDURE WHIPOUT(NF00017100
WDV,W,NFWD);VALUE NFWDV;BEGIN LOCAL ST;SI~W;DI~NFWDV;DS~WDS;ST~DI;DI~NFW00017200
D;SI~LOC ST;DS~WDS;END OF WHIPOUT;BOOLEAN PROCEDURE FORMATPHRASE;BEGIN L00017300
ABEL EL,EX,EXIT,L1,L2,L3;PROCEDURE EMITFORMAT(S,CODE,REPEAT,SKIP,W,W1,W200017400
,D1,D2);VALUE S,CODE,REPEAT,SKIP,W,W1,W2,D1,D2;REAL CODE,REPEAT,SKIP,W,W00017500
1,W2,D1,D2;BOOLEAN S;BEGIN IF W>63THEN FLAG(163);W~REPEAT&W[6:42:6]&SKIP00017600
[32:42:6]&W1[28:44:4]&W2[24:44:4]&D1[20:44:4]&D2[16:44:4]&CODE[2:44:4]&R00017700
EAL(S)[1:47:1];F~F+1;WHIPOUT(NFWD,W,NFWD);END EMITFORMAT;STREAM PROCEDUR00017800
E PACKALPHA(PLACE,LETTER,CTR);VALUE LETTER,CTR;BEGIN DI~PLACE;DS~LIT"B";00017900
SI~LOC CTR;SI~SI+7;DS~CHR;SI~PLACE;SI~SI+3;DS~5CHR;SI~LOC LETTER;SI~SI+700018000
;DS~CHR END PACKALPHA;INTEGER REPEAT,SKIP,W,W1,W2,D1,D2,CODE;BOOLEAN S;I00018100
NTEGER ST;DEFINE RRIGHT=0#,RLEFT=4#,RSTROKE=6#;DEFINE RSCALE=8#,RR=15#;D00018200
EFINE RD=0#,RX=2#,RA=4#,RI=6#,RF=8#,RE=10#,RO=12#,RL=14#;IF ELCLASS<0THE00018300
N BEGIN REPEAT~-ELCLASS;NEXTENT;IF ELCLASS=","THEN GO EX END ELSE REPEAT00018400
~REAL(ELCLASS!"("AND ELCLASS!"<");IF ELCLASS="("OR ELCLASS="<"THEN BEGIN00018500
 SKIP~F;EMITFORMAT(TRUE,RLEFT,REPEAT,1,0,0,0,0,0);DO BEGIN NEXTENT;EL:IF00018600
 FORMATPHRASE THEN GO TO EX END UNTIL ELCLASS!",";WHILE ELCLASS="/"DO BE00018700
GIN EMITFORMAT(TRUE,RSTROKE,0,1,0,0,0,0,0);NEXTENT END;IF ELCLASS!")"AND00018800
 ELCLASS!">"THEN GO TO EL;IF REPEAT=0THEN EMITFORMAT(TRUE,RSTROKE,1,0,0,00018900
0,0,0,0);S~TRUE;REPEAT~F-SKIP;CODE~RRIGHT END ELSE IF ELCLASS="O"THEN BE00019000
GIN CODE~RO;W~8END ELSE IF ELCLASS="D"THEN BEGIN CODE~RD;W~8END ELSE IF 00019100
ELCLASS=","THEN GO TO L2 ELSE IF ELCLASS="/"THEN GO TO EXIT ELSE IF ELCL00019200
ASS=")"OR ELCLASS=">"THEN GO TO EXIT ELSE IF ELCLASS="S"THEN BEGIN NEXTE00019300
NT;W~IF ELCLASS="-"THEN 1ELSE 0;IF ELCLASS>0THEN NEXTENT;IF ELCLASS>0THE00019400
N BEGIN ERR(136);GO TO EXIT END ELSE REPEAT~-ELCLASS;EMITFORMAT(TRUE,RSC00019500
ALE,REPEAT,0,W,0,0,0,0);GO TO L2 END ELSE IF ELCLASS="""THEN BEGIN CODE~00019600
100;ST~0;DO BEGIN SKIP~1;DO BEGIN RSLT~5;CNT~0;SCANNER;IF ELCLASS~ACCUM[00019700
1].[18:6]=CODE THEN BEGIN IF SKIP!1THEN BEGIN WHIPOUT(NFWD,W,NFWD);F~F+100019800
;END;GO TO L2 END;CODE~""";PACKALPHA(W,ELCLASS,SKIP);END UNTIL SKIP~SKIP00019900
+1=7;WHIPOUT(NFWD,W,NFWD);F~F+1;END UNTIL(ST~ST+6)>132;GO TO EX END ELSE00020000
 BEGIN CODE~ELCLASS;GETINT;W~ELCLASS;IF CODE="I"THEN BEGIN SKIP~DIVIDE(W00020100
,W1,W2);CODE~RI END ELSE IF CODE="F"THEN BEGIN CODE~RF;GO TO L1 END ELSE00020200
 IF CODE="R"THEN BEGIN CODE~RR;GO TO L1 END ELSE IF CODE="E"THEN BEGIN C00020300
ODE~RE;D1~1;L1:NEXTENT;IF ELCLASS!"."THEN GO TO EX;GETINT;IF DIVIDE(ELCL00020400
ASS+D1,D1,D2)>0THEN GO TO EX;IF CODE=RF OR CODE=RR THEN SKIP~DIVIDE(W-EL00020500
CLASS-1,W1,W2)ELSE IF SKIP~W-ELCLASS-6<0THEN GO TO EX END ELSE IF CODE="00020600
X"THEN BEGIN CODE~RX;W1~W.[38:4];SKIP~W~W.[42:6]END ELSE IF CODE="A"THEN00020700
 BEGIN CODE~RA;W1~6;GO TO L3 END ELSE IF CODE="L"THEN BEGIN CODE~RL;W1~500020800
;L3:IF W<W1 THEN W1~W;SKIP~W-W1 END ELSE GO EX END;EMITFORMAT(S,CODE,REP00020900
EAT,SKIP,W,W1,W2,D1,D2);L2:NEXTENT;GO TO EXIT;EX:FORMATPHRASE~TRUE;ERR(100021000
36);EXIT:END FORMATPHRASE;ERRTOG~FALSE;READACARD;NFWD~FMTF(GENF);F~0;DO 00021100
NEXTENT UNTIL ELCLASS="("OR ELCLASS=";";IF ELCLASS!"("THEN BEGIN FLAG(3200021200
);NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;ERRTOG~FORMATPHRAS00021300
E;IF ELCLASS=";"THEN GO TO FINISHED;FLAG(119);FINISHED:TATTLE(F,PRNT[0])00021400
;WRITE(LINE,15,PRNT[*]);MAXWDS(INFORMAT);IF F>FMAX THEN BEGIN LARGER(PRN00021500
T[0],FMAX);WRITE(LINE,15,PRNT[*]);ERRTOG~TRUE;END;NCR~0&(F+1)[24:39:9];W00021600
HIPOUT(NFWD,NCR,NFWD);CNT~(F+1)DIV 63;TCNT~(F+1)MOD 63;IF ERRTOG THEN BE00021700
GIN NCR~TIME(1);DO F~0UNTIL TIME(1)-NCR>200;F~@40*2;END;MOVECODE(GENF,IN00021800
FORMAT,CNT,TCNT);END OF OBJECTFMTGEN;                                   00021900
                                                                        00022000
                       READ(CARD,FOR2,NGRPS,NVAR,BUFSIZE,LOGSIZE);      00022100
                       WRITE(LINE,FOR4);                                00022200
                       READ(CARD,FOR3,0);                               00022300
                       WRITE(LINE,FOR3,20);                             00022400
                       WRITE(LINE,FOR5,NGRPS,NVAR);                     00022500
                       NGRPS~NGRPS-1;                                   00022600
                       NVR~NVAR-1;                                      00022700
               OBJECTFMTGEN(INFORM);                                    00022800
                       NBUF~2;                                          00022900
                       IF BUFSIZE=0 THEN                                00023000
                  BEGIN                                                 00023100
COMMENT    ASSUME CARD-READER FILE    ;                                 00023200
                       BUFSIZE~10;                                      00023300
                       LOGSIZE~0                                        00023400
                  END;                                                  00023500
                  BEGIN                                                 00023600
ARRAY                  SIG,VAR,NM,S,S2,BET,TTT,N[0:NVR],NUMS[0:NGRPS];  00023700
FILE IN                INPUT(NBUF,BUFSIZE,LOGSIZE);                     00023800
LABEL          INS,FINIS,RERR;                                          00023900
                       READ(CARD,FOR7,FOR A~0 STEP 1 UNTIL NGRPS DO     00024000
                        NUMS[A]);                                       00024100
                       CLOSE(CARD,RELEASE);                             00024200
                       A~0;                                             00024300
               DO BEGIN                                                 00024400
                       WRITE(LINE[PAGE]);                               00024500
                       NST~NUMS[A];                                     00024600
               FOR B~0 STEP 1 UNTIL NVR DO BEGIN                        00024700
               N[B]~0;S[B]~0; S2[B]~0;   END;                           00024800
               DO BEGIN                                                 00024900
                       READ(INPUT,INFORM,FOR B~0 STEP 1 UNTIL NVR DO    00025000
               VAR[B])[INS:RERR];                                       00025100
               B~0;                                                     00025200
COMMENT   THE NEXT STATEMENT CHECKS FOR NEGATIVE DATA AND BLANK DATA;   00025300
               DO IF NOT BOOLEAN (C~VAR[B]).[1:1]  OR VAR[B].[9:39]!0   00025400
               THEN                                                     00025410
                  BEGIN                                                 00025500
                       N[B]~N[B]+1;                                     00025600
                       S[B]~S[B]+C;                                     00025700
                       S2[B]~S2[B]+C*2;                                 00025800
               END UNTIL B~B+1 >NVR;                                    00025900
                  END  UNTIL NST~NST-1<1;                               00026000
                       WRITE(LINE,FOR8,A+1,NUMS[A]);                    00026100
                       B~0;                                             00026200
               DO BEGIN                                                 00026300
               IF N[B]=0 THEN                                  %9/69 MC 00026400
          BEGIN                                                %9/69 MC 00026410
               BET[B]~0;                                       %9/69 MC 00026420
               D~N[B];                                         %9/69 MC 00026430
               C~S[B];                                         %9/69 MC 00026440
          END ELSE                                             %9/69 MC 00026450
                       BET[B]~(C~S[B])*2/(D~N[B])+BET[B];               00026500
                       SIG[B]~SIG[B]+C;                                 00026600
                       NM[B]~NM[B]+D;                                   00026700
                       TTT[B]~TTT[B]+E~S2[B];                           00026800
               IF N[B]!0 THEN                                           00026900
                       WRITE(LINE,FOR9,B+1,D,C,E,C/D,SQRT((E-C*2/D)/D)) 00027000
                  END  UNTIL B~B+1>NVR                                  00027100
                  END  UNTIL A~A+1>NGRPS;                               00027200
                       WRITE(LINE[PAGE]);                               00027300
               A~0;                                                     00027400
               DO BEGIN                                                 00027500
                       B~(C~BET[A]-(D~SIG[A]*2/E~NM[A]))/NGRPS;         00027600
                       F~(T~TTT[A]-BET[A])/NBUF~E-NGRPS-1;              00027700
                       D~(NCR~TTT[A]-D)/LCR~E-1;                        00027800
               IF E=0 THEN G~0 ELSE G~SIG[A]/E;                         00027900
                       WRITE(LINE,FOR10,A+1,E,"A    ",C,NGRPS,B,"S/A  ",00028000
                        T,NBUF,F,"TOTAL",NCR,LCR,D,B/F,G);              00028100
                       IF A MOD 3=2 THEN                                00028200
                       WRITE(LINE[PAGE])                                00028300
                  END  UNTIL A~A+1>NVR;                                 00028400
               GO TO FINIS;                                             00028500
RERR:          WRITE(LINE,FRERR);                                       00028600
               WRITE(LINE,FRERR1,NUMS[A]-NST+1,A+1);                    00028700
               GO TO FINIS;                                             00028800
INS:           WRITE(LINE,FINS);                                        00028900
               WRITE(LINE,FINS1,NUMS[A]-NST+1,A+1);                     00029000
FINIS:            END                                                   00029100
                  END.                                                  00029200
                LAST CARD ON CRDIMG TAPE                                99999999
