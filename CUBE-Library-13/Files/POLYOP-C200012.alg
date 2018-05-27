BEGIN                                                                   00000100
COMMENT        LOUIS A. WITKIN, FLEXIBLE TELETYPE PROGRAM FOR MATRIX    00000200
               OPERATIONS.  UNIVERSITY OF DENVER, 2/23/68;              00000300
COMMENT        VERSION 4     4/2/69;                                    00000400
ALPHA FILE IN TTIN 14 (2,8);                                            00000500
ALPHA FILE OUT TTOUT 14 (2,8);                                          00000600
INTEGER STA;                                                            00000700
INTEGER        J,K;                                                     00000800
INTEGER PNT;                                                            00000900
REAL QUANT;                                                             00001000
REAL MSG;                                                               00001100
REAL NMSG;                                                              00001200
REAL NCA;                                                               00001300
REAL NLP;                                                               00001400
REAL WO;                                                                00001500
REAL MSGL;                                                              00001600
ALPHA LP;                                                               00001700
ALPHA CA;                                                               00001800
INTEGER        GP;                                                      00001900
LABEL          EQU;                                                     00002000
LABEL          NAME;                                                    00002100
LABEL          CHECK;                                                   00002200
LABEL          BMSG;                                                    00002300
ARRAY TYPE,NUM[0:3];                                                    00002400
ARRAY          CONST[0:9],POLY[0:9,0:20];                               00002500
ARRAY          SPOLY,WPOLY[0:20];                                       00002600
ALPHA LA;                                                               00002700
ALPHA QUEST;                                                            00002800
INTEGER I;                                                              00002900
LABEL ERR,INPUT,PRINTER,SET,    FREQL,ROOTS,STOP,TEACH,SIZE,LOCI,VL;    00003000
SWITCH SL~ERR,INPUT,PRINTER,SET,ERR,FREQL,ROOTS,STOP,TEACH,SIZE,LOCI,VL;00003100
LABEL MORE;                                                             00003200
LABEL         BRK;                                                      00003300
SAVE REAL ARRAY RD[0:7];                                                00003400
SAVE ARRAY WD[0:35];                                                    00003500
REAL NC,NCT;                                                            00003600
ARRAY LINE [0:7];                                                       00003700
INTEGER ERRTOG;                                                         00003800
LABEL INOP;                                                             00003900
LABEL ADD,SUB,DVD,MULT,NULL;                                            00004000
SWITCH F~ADD,SUB,DVD,MULT,INOP,INOP,INOP,NULL,INOP,INOP;                00004100
INTEGER LOC;                                                            00004200
LABEL RESET;                                                            00004300
DEFINE E1="01INVA","LID VE","RB~   ","      ",NCT,1," "#;               00004400
DEFINE E2="02INVA","LID ID","ENTIFI","ER~   ",NCT,1,2#;                 00004500
DEFINE E2A="02INVA","LID ID","ENTIFI","ER~   ",NCT,2,2#;                00004600
DEFINE E3="03MISS","ING -(","-~    ","      ",NMSG, 0,2#;               00004700
DEFINE E4="04MISS","ING -,","-~    ","      ",NLP, 0,1#;                00004800
DEFINE E5="05MISS","ING -)","-~    ","      ",WO , 0,1#;                00004900
DEFINE E6="06INVA","LID OP","ERATIO","N~    ",NCT,0,0#;                 00005000
DEFINE E7 ="07MISS","ING -=","-~    ","      ",NMSG,0,2#;               00005100
DEFINE E8="08INVA","LID IN","DEX~  ","      ",NMSG,0,2#;                00005200
DEFINE E9="09INVA","LID SI","ZE~   ","      ",NMSG,0,2#;                00005300
COMMENT E10="INDEX OVERFLOW OF";                                        00005400
DEFINE E11="11IDEN","T. INV",". FOR ","OPER.~",NMSG,0,MSGL#;            00005500
DEFINE E12="12MISS","ING -*","-~    ","      ",NMSG,0,2#;               00005600
DEFINE E13="13DIVI","DE BY ","ZERO~ ","      ",NMSG,0,2#;               00005700
DEFINE E14="14MISS","ING -/","-~    ","      ",NMSG,0,2#;               00005800
DEFINE E15="15INVA","LID PA","RAMETE","R~    ",WO , 0,1#;               00005900
DEFINE E16="16TOO ","MUCH C","OMMAND","~     ",NCT,1,"~"#;              00006000
FORMAT         FO1(X8,"{!POLYOP/TTY, VERSION 4.{!IF YOU NEED HELP, TY~",00006100
                    /X8,"PE ",""","TEACH",""",".{!!~"),                 00006200
               PNULL(X8,"{!P",A1," (",I*,") IS NULL.{!~"),              00006300
               PSIZE(X8,"{!P",A1," IS (",I*,").{!!~"),                  00006400
               WC(X8,"{!C",I1,"=",V10.3,"{!!~"),                        00006500
               RC (X8,"{!C",I1,"=",A1,"{!~"),                           00006600
               DONE(X8,"{!DONE.{!~");                                   00006700
                FORMAT FIC(X8,"{!NUMBER ERROR.  VALID CHARACTERS ARE 0",00006800
                        "123456789+-,.@. ~"/X8," TYPE LINE AGAIN.{!~"); 00006900
SWITCH FORMAT SF~(/),                                                   00007000
         (X8,"{!THE USER HAS TYPED A VERB THAT THE PROGRAM DOES NOT~",  00007100
         /X8,"{!RECOGNIZE.{!~"),                                        00007200
         (X8,"{!THE USER HAS TYPED AN IDENTIFIER THAT THE PROGRAM~",    00007300
         /X8,"{!DOES NOT RECOGNIZE.{!~"),                               00007400
         (X8,"{!THE USER HAS NOT TYPED A LEFT-PARENTHESIS.{!~"),        00007500
         (X8,"{!THE USER HAS NOT TYPED A COMMA.{!~"),                   00007600
         (X8,"{!THE USER HAS NOT TYPED A RIGHT-PARENTHESIS.{!~"),       00007700
         (X8,"{!THE USER HAS TYPED AN OPERATION THAT THE PROGRAM DOES~",00007800
         /X8,"{!NOT RECOGNIZE.{!~"),                                    00007900
         (X8,"{!THE USER HAS NOT TYPED AN EQUAL SIGN.{!~"),             00008000
         (X8,"{!THE USER HAS TYPED AN AREA SIZE THAT THE PROGRAM CAN~", 00008100
         /X8,"{!NOT HANDLE.{!~"),                                       00008200
         (X8,"{!THE USER ATTEMPTED AN OPERATION WITH IDENTIFIERS OF~",  00008300
         /X8,"{!THE WRONG SIZES.{!~"),                                  00008400
         (X8,"{!THE USER HAS TYPED TOO MANY VALUES.{!~"),               00008500
         (X8,"{!THE USER HAS ATTEMPTED AN OPERATION THAT THE PROGRAM~", 00008600
         /X8,"{!CAN NOT DO USING THE IDENTIFIERS THAT HE TYPED.{!~"),   00008700
         (X8,"{!THE USER HAS NOT TYPED A MULTIPLICATION SIGN.{!~"),     00008800
         (X8,"{!THE USER HAS ATTEMPTED AN INVALID DIVISION.{!~"),       00008900
         (X8,"{!THE USER HAS NOT TYPED A DIVISION SIGN.{!~"),           00009000
         (X8,"{!THE USER HAS TYPED AN ALPHA CHARACTER WHEN A NUMBER~",  00009100
         /X8,"{!WAS EXPECTED.{!~"),                                     00009200
         (X8,"{!THE USER HAS TYPED MORE DATA THAN THE SYNTAX ALLOWS.~", 00009300
         /X8,"{!~"),                                                    00009400
         (X8,"{!",""","INPUT","""," OR ",""","READ",""","--ENTER DATA~",00009500
         /X8" WHEN THE~",                                               00009600
         /X8,"{!COMPUTER TYPES A QUESTION MARK.  EXAMPLES:~",           00009700
         /X8,"{!INPUT C4~",                                             00009800
         /X8,"{!INPUT P2 (7)~",                                         00009900
         /X8,"{!--7 REPRESENTS THE DEGREE OF THE POLYNOMIAL--~",        00010000
         /X8,"{!ENTER THE COEFFICIENT OF THE LOWEST POWER FIRST.{!~"),  00010100
         (X8,"{!",""","WRITE","""," OR ",""","PRINT",""","--TYPE STOR~",00010200
         /X8,"ED DATA.  EXAMPLES:~",                                    00010300
         /X8,"{!WRITE C2~",                                             00010400
         /X8,"{!WRITE P3{!~"),                                          00010500
         (X8,"{!",""","SET","""," OR ",""","LET",""","--EXECUTE ARITH~",00010600
         /X8,"METIC OPERATIONS.~",                                      00010700
         /X8,"{!THERE ARE SIX POSSIBLE OPERATIONS.  EXAMPLES:~",        00010800
          /X8,"{! 1.  ADDITION:~",                                      00010900
          /X8,"{!    SET C1=C3+C4~",                                    00011000
          /X8,"{!    SET P5=P3+P4~",                                    00011100
          /X8,"{! 2.  SUBTRACTION:~",                                   00011200
          /X8,"{!    SET C1=C5-C6~",                                    00011300
          /X8,"{!    SET P7=P2-P1~",                                    00011400
          /X8,"{! 3.  MULTIPLICATION:~",                                00011500
          /X8,"{!    SET P6=P4*P2~",                                    00011600
          /X8,"{!    SET C9=C0*C1~",                                    00011700
          /X8,"{!    SET P5=C4*P2  --EVERY ELEMENT*CONSTANT--~",        00011800
          /X8,"{! 4.  DIVISION:~",                                      00011900
          /X8,"{!    SET C7=C5/C2~",                                    00012000
          /X8,"{!    SET P3 (9)=P1/P2~",                                00012100
          /X8,"{!--9 IS THE DEGREE OF THE QUOTIENT POLYNOMIAL--~",      00012200
          /X8,"{! 5.  EQUATE:~",                                        00012300
          /X8,"{!    SET C2=C3~",                                       00012400
          /X8,"{!    SET P5=P3~",                                       00012500
          /X8,"{!    SET P2 (7)= 5  --A PARTICULAR ELEMENT--~",         00012600
          /X8,"{!    SET C4= 1000000000001.0~",                         00012700
          /X8,"{!6.  NULLIFY:~",                                        00012800
          /X8,"{!    SET C9=NULL~",                                     00012900
         /X8,"{!    SET P3 (15)=NULL{!~"),                              00013000
         (/),                                                           00013100
         (X8,"{!",""","FREQ",""","--COMPUTES FREQUENCY RESP~",          00013200
          /X8,"ONSE AND TYPES THE SOLUTION.{!FOR EXAMPLE, IF YOU TYPE~",00013300
          /X8," ",""","FREQ P*/P#","""," WHERE * AND # ARE NUMBERS FR~",00013400
          /X8,"OM{!0 TO 9, THE PROGRAM WILL RETURN TO THE NEXT LINE. ~",00013500
          /X8," YOU MAY NOW EITHER{!TYPE A SINGLE VALUE OR AN INCREME~",00013600
    /X8,"NT LIKE ",""","(1,2,10)","""," WHERE 1 IS THE{!STARTING VALU~",00013700
          /X8,"E, 2 IS THE INCREMENT, AND 10 IS THE LIMITING VALUE.{!~",00013800
          /X8,"THESE TWO FORMS OF VALUES CAN BE USED IN ANY ORDER AND~",00013900
          /X8," ANY NUMBER OF{!TIMES.  WHEN YOU ARE FINISHED USING FR~",00014000
          /X8,"EQUENCY RESPONSE, TYPE ",""","OUT",""",".{!THE PROGRAM~",00014100
          /X8," WILL RESPOND BY TYPING ",""","DONE",""",".  YOU ARE N~",00014200
          /X8,"OW IN THE MAIN{!PART OF THE PROGRAM AND YOU MAY USE AN~",00014300
         /X8,"Y OPERATION (OR FREQUENCY{!RESPONSE AGAIN).{!~"),         00014400
         (X8,"{!",""","ROOTS",""","--FINDS THE ROOTS OF A POLYNOMIAL~", 00014500
         /X8,"{!AND TYPES THE SOLUTION.  EXAMPLE:~",                    00014600
         /X8,"{!ROOTS P3{!~"),                                          00014700
         (X8,"{!",""","STOP","""," OR ",""","QUIT",""","--END COMPUTA~",00014800
         /X8,"TION AND TERMINATE THE PROGRAM.{!~"),                     00014900
         (X8,"{!THE FOLLOWING COMPUTATIONAL AREAS ARE AVAILABLE:~",     00015000
          /X8,"{!     1.  10 CONSTANTS.~",                              00015100
          /X8,"{!     2.  10 POLYNOMIALS, 19TH DEGREE IN EACH.~",       00015200
          /X8,"{!THESE AREAS MAY BE REFERENCED AS C* OR P*,~",          00015300
          /X8,"{!     WHERE * IS A NUMBER FROM 0 TO 9.~",               00015400
         /X8,"{!!FOR THE MEANING OF A PARTICULAR COMMAND, TYPE ",""",   00015500
         "TEACH~",                                                      00015600
         /X8,""","{!FOLLOWED BY ONE OF THE FOLLOWING COMMANDS:~",       00015700
         /X8,"{!!",""","INPUT","""," OR ",""","READ",""~",              00015800
         /X8,"{!",""","WRITE","""," OR ",""","PRINT",""~",              00015900
         /X8,"{!",""","SET","""," OR ",""","LET",""~",                  00016000
         /X8,"{!",""","FREQ",""~",                                      00016100
         /X8,"{!",""","ROOTS",""~",                                     00016200
         /X8,"{!",""","STOP","""," OR ",""","QUIT",""~",                00016300
         /X8,"{!",""","SIZE",""~",                                      00016400
         /X8,"{!",""","LOCI",""~",                                      00016500
         /X8,"{!!FOR THE MEANING OF AN ERROR MESSAGE, TYPE ",""",       00016600
         "TEACH",""~",                                                  00016700
         /X8,"{!FOLLOWED BY THE NUMBER OF THE ERROR MESSAGE.{!~"),      00016800
         (X8,"{!",""","SIZE",""","--TYPES THE DEGREE OF A POLYNOMIAL.~",00016900
         /X8,"  EXAMPLE:~",                                             00017000
         /X8,"{!SIZE P2{!~");                                           00017100
SWITCH FORMAT SF2~                                                      00017200
(X8,"{!",""","LOCI",""","--IF THE USER TYPES ",""","LOCI (P*,P#)",""",  00017300
     ", WHERE * AND~",                                                  00017400
/X8,"{!# ARE NUMBERS FROM 0 TO 9, THE PROGRAM WILL ASK FOR A~",         00017500
/X8,"{!K VALUE.  THIS VALUE IS USED WHEN THE PROGRAM FINDS~",           00017600
/X8,"{!THE ROOTS OF P* + K X P# AND TYPES THE SOLUTION.  IF~",          00017700
/X8,"{!THE USER WANTS THE COMBINED POLYNOMIAL PRINTED, TYPE~",          00017800
/X8,"{!",""","LOCI (P*,P#) POLY",""",".  TYPE ",""","OUT","""," TO LEA",00017900
     "VE LOCI AND~",                                                    00018000
/X8,"{!RETURN TO THE MAIN SECTION OF THE PROGRAM.{!~");                 00018100
          REAL STREAM PROCEDURE MKABS(BUFF);                            00018200
            BEGIN                                                       00018300
              SI~BUFF; SI~SI+8; MKABS~SI;                               00018400
            END MKABS;                                                  00018500
          REAL STREAM PROCEDURE MKCHR(BUFF);                            00018600
            BEGIN                                                       00018700
              SI~BUFF; MKCHR~SI;                                        00018800
            END MKCHR;                                                  00018900
INTEGER PROCEDURE SCAN;                                                 00019000
          BEGIN                                                         00019100
          REAL STREAM PROCEDURE LOOK(DICT,NC,NCV,VAR);                  00019200
          VALUE NCV;                                                    00019300
            BEGIN                                                       00019400
            LABEL ALPH,GOTIT,ALL;                                       00019500
            LOCAL T,N,SSI;                                              00019600
              SI~NCV; 56(IF SC=" " THEN SI~SI+1 ELSE JUMP OUT TO ALPH); 00019700
          ALPH:T~SI; SI~DICT; 36(DI~LOC N; DI~DI+7; DS~CHR; DI~T;       00019800
              IF N SC=DC THEN                                           00019900
            BEGIN                                                       00020000
              T~DI; DI~VAR; DS~8 LIT "0"; DI~DI-N; SSI~SI; SI~T;        00020100
              SI~SI-N; DS~N CHR;SI~SSI; DI~T; JUMP OUT TO GOTIT;        00020200
            END ELSE                                                    00020300
            BEGIN                                                       00020400
              SI~SI-N; SI~SI+7;                                         00020500
            END);                                                       00020600
              SI~T; IF SC=ALPHA THEN IF SC}"0" THEN                     00020700
            BEGIN                                                       00020800
              DI~LOC SSI; DS~CHR; TALLY~1; 7(IF SC}"0" THEN             00020900
            BEGIN                                                       00021000
              DS~CHR; TALLY~TALLY+1;                                    00021100
            END ELSE JUMP OUT TO ALL);                                  00021200
          ALL:N~TALLY; T~SI; SI~LOC SSI; DI~VAR; DS~N OCT; TALLY~40;    00021300
              N~TALLY; SI~LOC N; SI~SI+7; DI~T; GO TO GOTIT;            00021400
            END;                                                        00021500
              TALLY~1; N~TALLY; SI~LOC N; SI~SI+7;                      00021600
          GOTIT:T~DI; DI~LOC LOOK; DI~DI+7; DS~CHR; SI~LOC T; DI~NC;    00021700
              DS~WDS;                                                   00021800
            END LOOK;                                                   00021900
               SCAN~LOOK(WD,NCT,NCT,QUANT);                             00022000
          END SCAN;                                                     00022100
          STREAM PROCEDURE MARK(RD,NCT);                                00022200
          VALUE NCT;                                                    00022300
            BEGIN                                                       00022400
              DI~RD; 8(DS~8 LIT " "); DI~RD; DI~DI+8; DS~2 LIT "{!";    00022500
              DI~NCT; DI~DI+1; DS~5 LIT "+---~";                        00022600
            END MARK;                                                   00022700
          STREAM PROCEDURE PRT(LINE,N1,N2,N3,N4,NC,BACK,NCH);           00022800
          VALUE N1,N2,N3,N4,NC,BACK,NCH;                                00022900
            BEGIN                                                       00023000
            LOCAL TEMP,T;                                               00023100
            LABEL DONE,STOP,CONT;                                       00023200
              DI~LINE; DI~DI+8; DS~8 LIT "{!ERR # "; SI~LOC N1; SI~SI+2;00023300
              DS~2 CHR; DS~3 LIT " - ";                                 00023400
              4(IF SC="~" THEN JUMP OUT TO DONE; DS~CHR); 3(SI~SI+2;    00023500
              6(IF SC="~" THEN JUMP OUT 2 TO DONE; DS~CHR));            00023600
          DONE:DS~3 LIT " .."; SI~NC; SI~SI-BACK; TEMP~SI; SI~LOC NCH;  00023700
              SI~SI+7; IF SC}"0" THEN                                   00023800
            BEGIN                                                       00023900
              SI~TEMP; NCH(IF SC="~" THEN JUMP OUT TO STOP; DS~CHR);    00024000
            END ELSE                                                    00024100
            BEGIN                                                       00024200
              SI~TEMP; T~DI; DI~LOC NCH; DI~DI+7;                       00024300
              13(IF SC="~" THEN JUMP OUT TO CONT;                       00024400
              IF SC=DC THEN JUMP OUT TO CONT; TALLY~TALLY+1; DI~DI-1);  00024500
          CONT:SI~TEMP; DI~T; TEMP~TALLY; DS~TEMP CHR;                  00024600
            END;                                                        00024700
          STOP:DS~5 LIT "..{!~";                                        00024800
            END PRT;                                                    00024900
PROCEDURE ERROR(N1,N2,N3,N4,NC,BACK,NCH);                               00025000
VALUE N1,N2,N3,N4,NC,BACK,NCH;                                          00025100
ALPHA N1,N2,N3,N4,NCH;                                                  00025200
INTEGER NC,BACK;                                                        00025300
          BEGIN                                                         00025400
               PRT(LINE,N1,N2,N3,N4,NC,BACK,NCH);                       00025500
               MARK(RD,NCT);                                            00025600
               WRITE(TTOUT(STA),8,RD[*]);                               00025700
               WRITE(TTOUT(STA),8,LINE[*]);                             00025800
               ERRTOG~ERRTOG+1;                                         00025900
               IF ERRTOG=5 THEN                                         00026000
WRITE(TTOUT(STA),<X8,"{!PLEASE READ THE INSTRUCTION MANUAL.{!!~">);     00026100
               IF ERRTOG=6 THEN                                         00026200
               GO TO STOP;                                              00026300
               GO TO MORE;                                              00026400
          END ERROR;                                                    00026500
COMMENT        REMOTE UNIT INPUT PROCEDURE PACKAGE                      00026600
-----------------------------------------------------------------------;00026700
          PROCEDURE STATION(STA);                                       00026800
            INTEGER  STA;                                               00026900
              BEGIN                                                     00027000
                ARRAY  RD1[0:7];                                        00027100
                  STA:=STATUS(RD1[*]);                                  00027200
                  STA.[9:9]:=RD1[0].[9:9];                              00027300
              END OF PROCEDURE STATION;                                 00027400
          INTEGER STREAM PROCEDURE FINDCH(B,CH);                        00027500
            VALUE  CH;                                                  00027600
              BEGIN                                                     00027700
                  DI:=LOC CH; DI:=DI+7; SI:=B;                          00027800
                  63(IF SC=DC THEN JUMP OUT; DI:=DI-1; TALLY:=TALLY+1); 00027900
                  FINDCH:=TALLY;                                        00028000
              END OF PROCEDURE FINDCH;                                  00028100
          BOOLEAN STREAM PROCEDURE ANYFUNNYCHRS(B,NCH);                 00028200
            VALUE  NCH;                                                 00028300
              BEGIN                                                     00028400
                LABEL  OK;                                              00028500
                  SI:=B; TALLY:=0; NCH(IF SC=ALPHA THEN BEGIN IF SC}"0" 00028600
                  THEN GO TO OK; END; IF SC="." THEN GO TO OK; IF SC=","00028700
                  THEN GO TO OK; IF SC="+" THEN GO TO OK; IF SC="-" THEN00028800
                  GO TO OK; IF SC="@" THEN GO TO OK; TALLY:=1; JUMP OUT;00028900
                  OK: SI:=SI+1);                                        00029000
                  ANYFUNNYCHRS:=TALLY                                   00029100
              END OF PROCEDURE ANYFUNNYCHRS;                            00029200
          BOOLEAN STREAM PROCEDURE LASTCOMMA(B,NCH);                    00029300
            VALUE  NCH;                                                 00029400
              BEGIN                                                     00029500
                SI:=B; SI:=SI+NCH; IF SC="," THEN TALLY:=1;             00029600
                LASTCOMMA:=TALLY                                        00029700
              END OF PROCEDURE LASTCOMMA;                               00029800
          STREAM PROCEDURE PUTCHRIN(B,NCH,CH);                          00029900
            VALUE  NCH,CH;                                              00030000
              BEGIN                                                     00030100
                DI:=B; SI:=LOC CH;  SI:=SI+7; DI:=DI+NCH; DS:=1 CHR     00030200
              END OF PROCEDURE PUTCHRIN;                                00030300
          INTEGER STREAM PROCEDURE COUNTCOMMAS(B,NCH);                  00030400
            VALUE  NCH;                                                 00030500
              BEGIN                                                     00030600
                SI:=B; NCH(IF SC="," THEN TALLY:=TALLY+1; SI:=SI+1);    00030700
                COUNTCOMMAS:=TALLY                                      00030800
              END OF PROCEDURE COUNTCOMMAS;                             00030900
          STREAM PROCEDURE MOVER(B,T,N);                                00031000
            VALUE  N;                                                   00031100
              BEGIN                                                     00031200
                SI:=B; DI:=T; DS:=N CHR; DS:=6 LIT " "                  00031300
              END OF PROCEDURE MOVER;                                   00031400
          STREAM PROCEDURE SHIFT(A,S,N);                                00031500
            VALUE  S,N;                                                 00031600
              BEGIN                                                     00031700
                SI:=LOC S; SI:=SI+7; IF SC!"0" THEN                     00031800
                BEGIN                                                   00031900
                  SI:=A; SI:=SI+S; DI:=A; DS:=N CHR                     00032000
                END                                                     00032100
              END OF PROCEDURE SHIFT;                                   00032200
          BOOLEAN STREAM PROCEDURE DECIMAL(B,NCH);                      00032300
            VALUE  NCH;                                                 00032400
              BEGIN                                                     00032500
                SI:=B; TALLY:=0; NCH(IF SC="." THEN BEGIN TALLY:=1; JUMP00032600
                OUT; END; SI:=SI+1); DECIMAL:=TALLY                     00032700
              END OF PROCEDURE DECIMAL;                                 00032800
          STREAM PROCEDURE MOVE(B,T,N);                                 00032900
            VALUE  N;                                                   00033000
              BEGIN                                                     00033100
                SI:=B; DI:=T; DS:=N CHR; DS:=20 LIT "0"                 00033200
              END OF PROCEDURE MOVE;                                    00033300
          PROCEDURE READVAL(V,STA);                                     00033400
            VALUE  STA; INTEGER  STA; REAL  V;                          00033500
              BEGIN                                                     00033600
                INTEGER  GP;                                            00033700
                FORMAT  FSO(X8,"{!TYPE VALUE AGAIN.{!~");               00033800
                LABEL  RER,XIT,TRYAGIN;                                 00033900
                ARRAY  RD1[0:7];                                        00034000
                ALPHA  LA,COMA;                                         00034100
                  LA:=31; COMA:=58;                                     00034200
          TRYAGIN:READ(TTIN(STA),8,RD1[*]);                             00034300
                  GP:=FINDCH(RD1[1],LA);                                00034400
                  IF ANYFUNNYCHRS(RD1[1],GP) THEN GO TO RER;            00034500
                  PUTCHRIN(RD1[1],GP,COMA); GP:=GP+1;                   00034600
                  MOVER(RD1[1],RD1[0],GP);                              00034700
                  READ(RD1[*],/,V)[:RER];  GO TO XIT;                   00034800
          RER:    WRITE(TTOUT(STA),FSO);  GO TO TRYAGIN;                00034900
          XIT:END OF PROCEDURE READVAL;                                 00035000
          PROCEDURE READROW(N,A,STA);                                   00035100
            VALUE  N,STA; INTEGER  N,STA; ARRAY  A[0];                  00035200
              BEGIN                                                     00035300
                INTEGER GP,I,J,CCM;                                     00035400
                ALPHA  LA,COMA;                                         00035500
                LABEL  MORE,XIT,R;                                      00035600
                FORMAT  F1(X8,"{!TYPING ERROR.  VALID CHARACTERS ARE 0",00035700
                        "123456789+-,.@. ~"/X8," TYPE LINE AGAIN.{!~"), 00035800
                        F2(X8,"{!~");                                   00035900
                ARRAY  RD1[0:7];                                        00036000
                  J:=1; LA:=31; COMA:=58;                               00036100
               NCT~NC;                                                  00036200
          MORE:   READ(TTIN(STA),8,RD1[*]);                             00036300
                  GP:=FINDCH(RD1[1],LA);                                00036400
                  IF ANYFUNNYCHRS(RD1[1],GP) THEN GO TO R;              00036500
                  IF NOT LASTCOMMA(RD1[1],GP-1) THEN                    00036600
                  BEGIN                                                 00036700
                    PUTCHRIN(RD1[1],GP,COMA); GP:=GP+1                  00036800
                  END;                                                  00036900
                  CCM:=COUNTCOMMAS(RD1[1],GP)-1;                        00037000
               IF J+CCM>N THEN                                          00037100
          BEGIN                                                         00037200
               WRITE (TTOUT(STA),<X8,"{!ERR # 10 - INDEX OVERFLOW OF .",00037300
                 ".",I2,"..{!~">,J+CCM-N);                              00037400
               GO TO MORE;                                              00037500
          END;                                                          00037600
                  MOVER(RD1[1],RD1[0],GP);                              00037700
                  READ(RD1[*],/,FOR I:=J STEP 1 UNTIL J+CCM DO A[I])[:R]00037800
                ; J:=J+CCM+1;                                           00037900
                  WRITE(TTOUT(STA),F2);                                 00038000
                  IF J{N THEN GO TO MORE ELSE GO TO XIT;                00038100
          R:      WRITE(TTOUT(STA),F1);  GO TO MORE;                    00038200
          XIT:END OF PROCEDURE READROW;                                 00038300
PROCEDURE      PRINT (RD);                                              00038400
ARRAY          RD[0];                                                   00038500
          BEGIN                                                         00038600
          BOOLEAN STREAM PROCEDURE LINE (NC,NCT,RD2);                   00038700
          VALUE NCT;                                                    00038800
            BEGIN                                                       00038900
            LOCAL T;                                                    00039000
            LABEL DONE;                                                 00039100
            SI~NCT; DI~RD2; DI~DI+8;                                    00039200
              55(IF SC="~" THEN                                         00039300
            BEGIN                                                       00039400
              TALLY~0; JUMP OUT TO DONE;                                00039500
            END;                                                        00039600
              DS~CHR); TALLY~1;                                         00039700
          DONE:DS~1 LIT "~"; T~SI; SI~LOC T; DI~NC; DS~WDS; LINE~TALLY; 00039800
            END LINE;                                                   00039900
ARRAY          RD2[0:7];                                                00040000
REAL           NCT;                                                     00040100
               NCT~MKABS(RD);                                           00040200
               WHILE LINE(NCT,NCT,RD2) DO                               00040300
               WRITE (TTOUT(STA),8,RD2[*]);                             00040400
               WRITE (TTOUT(STA),8,RD2[*]);                             00040500
          END PRINT;                                                    00040600
COMMENT----------------------------------------------------------------;00040700
PROCEDURE      SKIPBLANKS;                                              00040800
          BEGIN                                                         00040900
LABEL          MORESPACE;                                               00041000
MORESPACE:     IF FINDCH(RD[0]," " )=0 THEN                             00041100
          BEGIN                                                         00041200
               SHIFT(RD[0],1,GP:=GP-1);                                 00041300
               GO TO MORESPACE;                                         00041400
          END;                                                          00041500
          END  OF PROCEDURE SKIPBLANKS;                                 00041600
COMMENT        PROCEDURES ARE INSERTED IN THIS SPACE                    00041700
***********************************************************************;00041800
          PROCEDURE PREAD(N);                                           00041900
            ARRAY N[0];                                                 00042000
                  READROW(N[2],POLY[N[1],*],STA);                       00042100
PROCEDURE READARRAY (V);                                                00042200
REAL           V;                                                       00042300
          BEGIN                                                         00042400
INTEGER        GP;                                                      00042500
ALPHA          LA,COMA;                                                 00042600
               LA~31; COMA~58;                                          00042700
               GP~FINDCH(RD[0],LA);                                     00042800
               PUTCHRIN(RD[0],GP,COMA);                                 00042900
               READ(RD[*],/,V);                                         00043000
          END OF PROCEDURE READARRAY;                                   00043100
PROCEDURE      PWRITE(NUM);                                             00043200
ARRAY          NUM[0];                                                  00043300
          BEGIN                                                         00043400
FORMAT         FP(X8,"{!P",A1," (",I*,")={!~"),                         00043500
               F1(X8,"{!",*(V10.3,X2),"{!",*(V10.3,X2),"{!",*(V10.3,X2),00043600
                    "{!",*(V10.3,X2),"~");                              00043700
ALPHA ARRAY    RD1[0:32];                                               00043800
INTEGER        I;                                                       00043900
ALPHA          LA;                                                      00044000
LABEL          MORE;                                                    00044100
               LA:=31;                                                  00044200
               WRITE (TTOUT(STA),FP,NUM[1],IF NUM[2]-1<10 AND NUM[2]-1> 00044300
                    -1 THEN 1 ELSE 2,NUM[2]-1);                         00044400
               IF NUM[2]{6 THEN                                         00044500
               WRITE (RD1[*],F1,NUM[2],FOR I~1 STEP 1 UNTIL NUM[2] DO   00044600
                     [IF POLY[NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN   00044700
                    "E" ELSE "F",POLY[NUM[1],I]],-1,-1,-1) ELSE         00044800
               IF NUM[2]{12 THEN                                        00044900
               WRITE (RD1[*],F1,6,FOR I~1,2,3,4,5,6 DO [IF POLY[NUM[1], 00045000
                    I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE "F",POLY  00045100
                    [NUM[1],I]],NUM[2]-6,FOR I~7 STEP 1 UNTIL NUM[2] DO 00045200
                    [IF POLY[NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E"00045300
                    ELSE "F",POLY[NUM[1],I]],-1,-1) ELSE                00045400
               IF NUM[2]{18 THEN                                        00045500
               WRITE (RD1[*],F1,6,FOR I~1,2,3,4,5,6 DO [IF POLY[NUM[1], 00045600
                    I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE "F",POLY  00045700
                    [NUM[1],I]],6,FOR I~7,8,9,10,11,12 DO [IF POLY      00045800
                    [NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE   00045900
                    "F",POLY[NUM[1],I]],NUM[2]-12,FOR I~13 STEP 1 UNTIL 00046000
                    NUM[2] DO [IF POLY[NUM[1],I]<@-1 OR POLY[NUM[1],I]> 00046100
                    @5 THEN "E" ELSE "F",POLY[NUM[1],I]],-1) ELSE       00046200
               WRITE (RD1[*],F1,6,FOR I~1,2,3,4,5,6 DO [IF POLY[NUM[1], 00046300
                    I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE "F",POLY  00046400
                    [NUM[1],I]],6,FOR I~7,8,9,10,11,12 DO [IF POLY      00046500
                    [NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE   00046600
                    "F",POLY[NUM[1],I]],6,FOR I~13,14,15,16,17,18 DO [IF00046700
                    POLY[NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E"    00046800
                    ELSE "F",POLY[NUM[1],I]],NUM[2]-18,FOR I~19 STEP 1  00046900
                    UNTIL NUM[2] DO [IF POLY[NUM[1],I]<@-1 OR POLY      00047000
                    [NUM[1],I]>@5 THEN "E" ELSE "F",POLY[NUM[1],I]]);   00047100
               PRINT(RD1);                                              00047200
               WRITE (TTOUT(STA),<X8,"{!~">);                           00047300
          END  OF PROCEDURE PWRITE;                                     00047400
          REAL PROCEDURE MAX(A,N);                                      00047500
            VALUE  N; INTEGER  N; ARRAY  A[0];                          00047600
             BEGIN                                                      00047700
               INTEGER  I;                                              00047800
               REAL  M;      M:=A[1];                                   00047900
                 FOR I:=2 STEP 1 UNTIL N DO                             00048000
                 IF A[I]>M THEN M:=A[I];                                00048100
                 MAX:=M                                                 00048200
             END OF PROCEDURE MAX;                                      00048300
          REAL PROCEDURE MIN(A,N);                                      00048400
            VALUE  N; INTEGER  N; ARRAY  A[0];                          00048500
              BEGIN                                                     00048600
                INTEGER  I;                                             00048700
                REAL  M;    M:=A[1];                                    00048800
                  FOR I:=2 STEP 1 UNTIL N DO                            00048900
                  IF A[I]<M THEN M:=A[I];                               00049000
                  MIN:=M                                                00049100
              END OF PROCEDURE MIN;                                     00049200
          REAL PROCEDURE CABS(X,Y);                                     00049300
            VALUE  X,Y; REAL  X,Y;                                      00049400
              BEGIN                                                     00049500
          COMMENT  ALGORITHM 312  OCT. 1967 <PAUL FRIEDLAND>;           00049600
                X:=ABS(X);  Y:=ABS(Y);                                  00049700
                CABS:=IF X=0 THEN Y ELSE IF Y=0 THEN X ELSE             00049800
                      IF X>Y THEN X|SQRT(1+(Y/X)*2)                     00049900
                             ELSE Y|SQRT(1+(X/Y)*2)                     00050000
              END OF PROCEDURE CABS;                                    00050100
          PROCEDURE CMULT(XR,XI,YR,YI,AR,AI);                           00050200
            VALUE  XR,XI,YR,YI; REAL  XR,XI,YR,YI,AR,AI;                00050300
              BEGIN                                                     00050400
                AR:=XR|YR-XI|YI; AI:=XR|YI+XI|YR                        00050500
              END;                                                      00050600
          PROCEDURE CADD(XR,XI,YR,YI,AR,AI);                            00050700
            VALUE  XR,XI,YR,YI; REAL  XR,XI,YR,YI,AR,AI;                00050800
              BEGIN                                                     00050900
                AR:=XR+YR; AI:=XI+YI                                    00051000
              END;                                                      00051100
          REAL PROCEDURE ABSCPOLY(N,CO,ZR,ZI);                          00051200
            VALUE  N; INTEGER  N; ARRAY  CO[0]; REAL  ZR,ZI;            00051300
              BEGIN                                                     00051400
                INTEGER  I;                                             00051500
                REAL  YR,YI;                                            00051600
                  YR:=CO[N];                                            00051700
                  FOR I:=N STEP -1 UNTIL 1 DO                           00051800
                  BEGIN                                                 00051900
                    CMULT(YR,YI,ZR,ZI,YR,YI);                           00052000
                    CADD(YR,YI,CO[I-1],0,YR,YI)                         00052100
                  END;                                                  00052200
                  ABSCPOLY:=CABS(YR,YI)                                 00052300
              END OF PROCEDURE ABSCPOLY;                                00052400
          PROCEDURE DOUBLESYNTHEDIV(A,N,REALROOT,IMAGROOT);             00052500
            VALUE  N,REALROOT,IMAGROOT; INTEGER  N; REAL  REALROOT,     00052600
            IMAGROOT; ARRAY A[0];                                       00052700
              BEGIN                                                     00052800
                INTEGER  I;                                             00052900
                  A[N-1]:=A[N-1]+2|REALROOT|A[N];                       00053000
                  FOR I:=2 STEP 1 UNTIL N DO                            00053100
                  A[N-I]:=A[N-I]-(REALROOT*2+IMAGROOT*2)|A[N-I+2]       00053200
                          +2|REALROOT|A[N-I+1];                         00053300
                  FOR I:=0 STEP 1 UNTIL N-2 DO A[I]:=A[I+2]             00053400
              END OF PROCEDURE DOUBLESYNTHEDIV;                         00053500
          PROCEDURE SYNTHETICDIV(A,N,ROOT);                             00053600
            VALUE  N,ROOT; INTEGER  N; REAL  ROOT; ARRAY  A[0];         00053700
              BEGIN                                                     00053800
                INTEGER  I;                                             00053900
                  FOR I:=1 STEP 1 UNTIL N DO                            00054000
                  A[N-I]:=A[N-I]+ROOT|A[N-I+1];                         00054100
                  FOR I:=0 STEP 1 UNTIL N-1 DO A[I]:=A[I+1]             00054200
              END OF PROCEDURE SYNTHETICDIV;                            00054300
          PROCEDURE QADD(P,Q,PADDQ);                                    00054400
            ARRAY P,Q,PADDQ[0];                                         00054500
              BEGIN                                                     00054600
                INTEGER SMALL,I;                                        00054700
                  IF P[0]>Q[0] THEN                                     00054800
                    BEGIN  SMALL:=Q[0];  PADDQ[0]:=P[0];                00054900
                      FOR I:=SMALL+1 STEP 1 UNTIL P[0] DO               00055000
                      PADDQ[I]:=P[I]                                    00055100
                    END                                                 00055200
                  ELSE                                                  00055300
                    BEGIN  SMALL:=P[0];  PADDQ[0]:=Q[0];                00055400
                      FOR I:=SMALL+1 STEP 1 UNTIL Q[0] DO               00055500
                      PADDQ[I]:=Q[I]                                    00055600
                    END;                                                00055700
                  FOR I:=1 STEP 1 UNTIL SMALL DO PADDQ[I]:=P[I]+Q[I];   00055800
              END 0F PROCEDURE QADD;                                    00055900
          PROCEDURE QMULT(A,B,PROD);                                    00056000
            ARRAY A,B,PROD[0];                                          00056100
              BEGIN                                                     00056200
                INTEGER I,J;                                            00056300
                  FOR I:=0 STEP 1 UNTIL A[0]+B[0] DO PROD[I]:=0;        00056400
                  FOR I:=1 STEP 1 UNTIL A[0] DO                         00056500
                  FOR J:=1 STEP 1 UNTIL B[0] DO                         00056600
                  PROD[I+J-1]:=PROD[I+J-1]+A[I]|B[J];                   00056700
                  PROD[0]:=A[0]+B[0]-1;                                 00056800
              END OF PROCEDURE QMULT;                                   00056900
          PROCEDURE CTV(A,B,C);                                         00057000
            VALUE  C;                                                   00057100
            ARRAY  A,B[0];                                              00057200
            REAL  C;                                                    00057300
              BEGIN                                                     00057400
                INTEGER I;                                              00057500
               B[0]:=A[0];                                              00057600
                  FOR I:=1 STEP 1 UNTIL A[0] DO                         00057700
                  B[I]:=A[I]|C;                                         00057800
              END OF PROCEDURE CTV;                                     00057900
          PROCEDURE QSUB(P,Q,QSUBQ);                                    00058000
            ARRAY P,Q,QSUBQ[0];                                         00058100
              BEGIN                                                     00058200
                INTEGER SMALL,I;                                        00058300
                  IF P[0]>Q[0] THEN                                     00058400
                    BEGIN  SMALL:=Q[0];  QSUBQ[0]:=P[0];                00058500
                      FOR I:=SMALL+1 STEP 1 UNTIL P[0] DO               00058600
                        QSUBQ[I]:=P[I]                                  00058700
                    END                                                 00058800
                  ELSE                                                  00058900
                    BEGIN  SMALL:=P[0];  QSUBQ[0]:=Q[0];                00059000
                      FOR I:=SMALL+1 STEP 1 UNTIL Q[0] DO               00059100
                      QSUBQ[I]:=-Q[I]                                   00059200
                    END;                                                00059300
                  FOR I:=1 STEP 1 UNTIL SMALL DO QSUBQ[I]:=P[I]-Q[I];   00059400
              END OF PROCEDURE QSUB;                                    00059500
          PROCEDURE QDIV(N,D,Q,SIZE);                                   00059600
            VALUE SIZE; ARRAY N,D,Q[0]; INTEGER SIZE;                   00059700
            BEGIN                                                       00059800
               ARRAY TEMPN,TSUB[0:20];         INTEGER I,J;             00059900
               Q[0]~SIZE;  TSUB[0]~D[0];                                00060000
               FOR I~0 STEP 1 UNTIL N[0] DO TEMPN[I]~N[I];              00060100
               Q[1]~TEMPN[1]/D[1];                                      00060200
               FOR J~ 2 STEP 1 UNTIL Q[0] DO                            00060300
               BEGIN                                                    00060400
               FOR I~1 STEP 1 UNTIL TSUB[0] DO TSUB[I]~D[I]|Q[J-1];     00060500
               QSUB(TEMPN,TSUB,TEMPN);                                  00060600
               FOR I~2 STEP 1 UNTIL TEMPN[0] DO TEMPN[I-1]~TEMPN[I];    00060700
               TEMPN[TEMPN[0]]~0;                                       00060800
               Q[J]~ TEMPN[1]/D[1];                                     00060900
               END;                                                     00061000
            END OF PROCEDURE QDIV;                                      00061100
          PROCEDURE FINDROOT(A,N,ROOTR,ROOTI);                          00061200
            VALUE  N; INTEGER  N; ARRAY  A[0]; REAL  ROOTR,ROOTI;       00061300
              BEGIN                                                     00061400
                REAL  GUESR,INCR,COMP,GUESI,PGUS;                       00061500
                INTEGER  I,FLG1,FLG2;                                   00061600
                ARRAY  NBR,NBI,PNB[0:4];                                00061700
                LABEL  L1,L2,L3,L4,L5,L6,L7,XIT;                        00061800
                  IF N=1 THEN                                           00061900
                  BEGIN                                                 00062000
                    ROOTR:=-A[0]/A[1]; ROOTI:=0; GO TO XIT              00062100
                  END;                                                  00062200
                  GUESR:=GUESI:=FLG1:=FLG2:=0; INCR:=0.5; COMP:=@-20;   00062300
          L1:     IF FLG1<5 OR    FLG2>0 THEN                           00062400
                  BEGIN                                                 00062500
                    FLG1:=FLG1+1; GO TO L3                              00062600
                  END;                                                  00062700
          L2:     FLG1:=0; INCR:=8.0|INCR;                              00062800
          L3:     NBR[1]:=GUESR +INCR; NBI[1]:=GUESI;                   00062900
                  NBR[2]:=GUESR-INCR; NBI[2]:=GUESI;                    00063000
                  NBR[3]:=GUESR; NBI[3]:=GUESI+INCR;                    00063100
                  NBR[4]:=GUESR; NBI[4]:=GUESI-INCR;                    00063200
                  PGUS:=ABSCPOLY(N,A,GUESR,GUESI);                      00063300
                  FOR I:= 1,2,3,4 DO                                    00063400
                  PNB[I]:=ABSCPOLY(N,A,NBR[I],NBI[I]);                  00063500
                  IF FLG2>27 THEN GO TO L4;                             00063600
                  IF PNB[1]<@-30 AND PNB[2]<@-30 AND PNB[3]<@-30        00063700
                  AND PNB[4]<@-30 THEN GO TO L4;                        00063800
                  IF MAX(PNB,4)-MIN(PNB,4)<@-20 THEN GO TO L2;          00063900
          L4:     IF PGUS!0 THEN GO TO L6;                              00064000
          L5:     ROOTR:=GUESR; ROOTI:=GUESI;                           00064100
                  GO TO XIT;                                            00064200
          L6:     FOR I:=1,2,3,4 DO IF PNB[I]=0 THEN                    00064300
                  BEGIN                                                 00064400
                    ROOTR:=NBR[I]; ROOTI:=NBI[I];                       00064500
                    GO TO XIT                                           00064600
                  END;                                                  00064700
                  FOR I:=1,2,3,4 DO IF PGUS>PNB[I] THEN GO TO L7;       00064800
                  INCR:= INCR/2;                                        00064900
                  IF INCR<COMP OR FLG2>50 THEN GO TO L5;                00065000
                  FLG2:=FLG2+1; GO TO L3;                               00065100
          L7:     FOR I:=1,2,3,4 DO IF PNB[I]=MIN(PNB,4) THEN           00065200
          BEGIN                                                         00065300
                  GUESR:=NBR[I]; GUESI:=NBI[I]; GO TO L1                00065400
          END;                                                          00065500
          XIT:END OF PROCEDURE FINDROOT;                                00065600
PROCEDURE      PROOTS(B);                                               00065700
ARRAY          B[0];                                                    00065800
          BEGIN                                                         00065900
REAL ARRAY     A[0:B[0]];                                               00066000
INTEGER        NNOW,N,I;  REAL     RP,IP;                               00066100
FORMAT         FT(X8,"{!REAL PART   IMAG PART{!~"),                     00066200
               ANS(X8,"{!",*(E10.3,X2),"~");                            00066300
          NNOW:=N:=B[0]-1;  FOR I:=0 STEP 1 UNTIL N DO A[I]:=B[I+1];    00066400
               WRITE (TTOUT(STA),FT);                                   00066500
               FOR I:=1 STEP 1 UNTIL N DO                               00066600
          BEGIN                                                         00066700
               FINDROOT(A,NNOW,RP,IP);                                  00066800
               IF IP!0 THEN                                             00066900
          BEGIN                                                         00067000
               WRITE (TTOUT(STA),ANS,  2,RP,IP);                        00067100
               I:=I+1;                                                  00067200
               WRITE (TTOUT(STA),ANS,  2,RP,-IP);                       00067300
               DOUBLESYNTHEDIV(A,NNOW,RP,IP);                           00067400
               NNOW:=NNOW-2;                                            00067500
          END  ELSE                                                     00067600
          BEGIN                                                         00067700
               WRITE (TTOUT(STA),ANS,  1,RP);                           00067800
               SYNTHETICDIV(A,NNOW,RP);                                 00067900
               NNOW:=NNOW-1;                                            00068000
          END;                                                          00068100
          END;                                                          00068200
          END  OF PROCEDURE PROOTS;                                     00068300
          PROCEDURE QIEV(P,X,REEL,IMAG);                                00068400
            ARRAY P[0];  REAL X,REEL,IMAG;                              00068500
              BEGIN                                                     00068600
                INTEGER I;      REEL:=0; IMAG:=0;                       00068700
                  FOR I:=2|ENTIER((P[0]-1)/2) STEP -2 UNTIL 0 DO        00068800
                  REEL:=REEL|X|X + P[I+1]|(-1)*(ENTIER(I/2));           00068900
                  FOR I:=2|ENTIER(P[0]/2)-1 STEP -2 UNTIL 1 DO          00069000
                  IMAG:=IMAG|X|X + P[I+1]|(-1)*(ENTIER(I/2));           00069100
                  IMAG:=IMAG|X;                                         00069200
              END OF PROCEDURE QIEV;                                    00069300
          PROCEDURE   FREQ(W,N,D,MAG,ANGLE,DB);                         00069400
            VALUE W; ARRAY N,D[0]; REAL MAG,ANGLE,DB,W;                 00069500
              BEGIN                                                     00069600
                REAL AN,AD,NR,NI,DR,DI;                                 00069700
                  QIEV(N,W,NR,NI);                                      00069800
                  QIEV(D,W,DR,DI);                                      00069900
                  IF ABS(NR)>0 THEN AN~57.29578|ARCTAN(NI/NR) ELSE      00070000
                  AN~90;                                                00070100
                  IF NR < 0    THEN AN~AN+180;                          00070200
                  IF ABS(DR)>0 THEN AD~57.29578|ARCTAN(DI/DR) ELSE      00070300
                  AD~90;                                                00070400
                  IF DR < 0 THEN AD~AD+180;                             00070500
                  ANGLE~AN-AD;                                          00070600
                  MAG~SQRT(NR|NR+NI|NI)/SQRT(DR|DR+DI|DI);              00070700
                  DB~8.685989|LN(MAG);                                  00070800
              END OF PROCEDURE FREQ;                                    00070900
REAL PROCEDURE VAL (P,A);                                               00071000
REAL           P;                                                       00071100
ARRAY          A[0];                                                    00071200
          BEGIN                                                         00071300
INTEGER        I;                                                       00071400
REAL           TOT;                                                     00071500
               TOT~A[A[0]];                                             00071600
               FOR I~A[0]-1 STEP -1 UNTIL 1 DO                          00071700
               TOT~A[I]+P|TOT;                                          00071800
               VAL~TOT;                                                 00071900
          END  VAL;                                                     00072000
COMMENT****************************************************************;00072100
FILE           DISC DISK SERIAL "PROGRAM""USERS" (2,5,30,SAVE 9);   %SPY00072200
ARRAY          WATCHER [0:4];                                       %SPY00072300
FORMAT         FN(X8,"{!TYPE IN YOUR LAST NAME.{!~");               %SPY00072400
INTEGER        PNTR;         WATCHER[3]:=TIME(1);                   %SPY00072500
FILL WD[*] WITH                                                         00072600
OCT0551252124600200, % READ        CLASS= 2                             00072700
OCT0631454764636002, % INPUT       CLASS= 2                             00072800
OCT0666513163256003, % WRITE       CLASS= 3                             00072900
OCT0647513145636003, % PRINT       CLASS= 3                             00073000
OCT0462256360040000, % SET         CLASS= 4                             00073100
OCT0443256360040000, % LET         CLASS= 4                             00073200
OCT0662464365256005, % SOLVE       CLASS= 5                             00073300
OCT0526512550600600, % FREQ        CLASS= 6                             00073400
OCT0651464663626007, % ROOTS       CLASS= 7                             00073500
OCT0462634647100000, % STOP        CLASS= 8                             00073600
OCT0450643163100000, % QUIT        CLASS= 8                             00073700
OCT0563252123301100, % TEACH       CLASS= 9                             00073800
OCT0562317125601200, % SIZE        CLASS=10                             00073900
OCT0443462331130000, % LOCI        CLASS=11                             00074000
OCT0465214360140000, % VAL         CLASS=12                             00074100
OCT0144510000000000, % M           CLASS=41                             00074200
OCT0165520000000000, % V           CLASS=42                             00074300
OCT0123530000000000, % C           CLASS=43                             00074400
OCT0135250000000000, % (           CLASS=21                             00074500
OCT0155260000000000, % )           CLASS=22                             00074600
OCT0175270000000000, % =           CLASS=23                             00074700
OCT0120300000000000, % +           CLASS=24                             00074800
OCT0154310000000000, % -           CLASS=25                             00074900
OCT0161320000000000, % /           CLASS=26                             00075000
OCT0153330000000000, % *           CLASS=27                             00075100
OCT0324256334000000, % DET         CLASS=28                             00075200
OCT0331456535000000, % INV         CLASS=29                             00075300
OCT0363514536000000, % TRN         CLASS=30                             00075400
OCT0445644343370000, % NULL        CLASS=31                             00075500
OCT0425652523400000, % EVEC        CLASS=32                             00075600
OCT0331244541000000, % IDN         CLASS=33                             00075700
OCT0447464370450000, % POLY        CLASS=37                             00075800
OCT0147540000000000, % P           CLASS=44                             00075900
OCT0346646346000000, % OUT         CLASS=38                             00076000
OCT0172470000000000, % ,           CLASS=39                             00076100
OCT0137240000000000; % ~           CLASS=20                             00076200
COMMENT CONSTANT                   CLASS=40;                            00076300
               STATION(STA);                                            00076400
               NC~MKABS(RD);                                            00076500
               LA~31;                                                   00076600
               QUEST~12;                                                00076700
               LP~29;                                                   00076800
               CA~58;                                                   00076900
               NMSG~MKCHR(MSG);                                         00077000
               NMSG.[30:3]~1;                                           00077100
               NLP~MKCHR(LP);                                           00077200
               NLP.[30:3]~7;                                            00077300
               NCA~MKCHR(CA);                                           00077400
               NCA.[30:3]~7;                                            00077500
NAME:          WRITE (TTOUT(STA),FN);                               %SPY00077600
               READ (TTIN(STA,900),8,RD[*])[STOP:STOP];                 00077700
               NCT~NC;                                                  00077800
               PNT~SCAN;                                                00077900
               IF PNT>1 AND PNT<13 THEN GO TO NAME;                     00078000
               MOVER (RD[1],RD[0],56);                              %SPY00078100
               SKIPBLANKS;                                              00078200
               MOVER(RD[0],WATCHER[1],FINDCH(RD[0],LA));                00078300
               WATCHER[0]:=0;  SHIFT(WATCHER[0],7,8);               %SPY00078400
               PUTCHRIN(WATCHER[2],0,"0");                          %SPY00078500
               WATCHER[2]:=WATCHER[3];                              %SPY00078600
               WRITE (TTOUT(STA),FO1)[:MORE];                           00078700
MORE:          READ (TTIN(STA,900),8,RD[*])[STOP:STOP];                 00078800
               NCT~NC;                                                  00078900
               PNT~SCAN;                                                00079000
               GO TO SL[PNT];                                           00079100
ERR:           ERROR(E1);                                               00079200
TEACH:    BEGIN                                                         00079300
LABEL          ETCH;                                                    00079400
               GP~FINDCH(RD[1],LA);                                     00079500
               PUTCHRIN(RD[1],GP," ");                                  00079600
               PUTCHRIN(RD[1],GP+1,"~");                                00079700
               PNT~SCAN;                                                00079800
              IF PNT=20 THEN                                            00079900
         BEGIN                                                          00080000
              WRITE (TTOUT(STA),SF[24])[:BRK];                          00080100
              GO TO ETCH;                                               00080200
         END;                                                           00080300
              IF PNT=40 THEN                                            00080400
              IF QUANT>0 AND QUANT<17 THEN                              00080500
         BEGIN                                                          00080600
              WRITE (TTOUT(STA),SF[QUANT])[:BRK];                       00080700
              GO TO ETCH;                                               00080800
         END  ELSE                                                      00080900
          BEGIN                                                         00081000
              WRITE (TTOUT(STA),<X8,"{!MESSAGE ",I2,                    00081100
                    " DOES NOT EXIST.{!~">,QUANT);                      00081200
               GO TO MORE;                                              00081300
          END;                                                          00081400
              IF PNT>1 AND PNT<11 AND PNT!5 THEN                        00081500
              WRITE (TTOUT(STA),SF[15+PNT])[:BRK]                       00081600
               ELSE IF PNT>10 AND PNT<12 THEN                           00081700
               WRITE (TTOUT(STA),SF2[PNT-11])[:BRK]                     00081800
              ELSE ERROR(E1);                                           00081900
ETCH:          ERRTOG~0;                                                00082000
               GO TO MORE;                                              00082100
          END;                                                          00082200
SIZE:     BEGIN                                                         00082300
               PNT~SCAN;                                                00082400
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00082500
               TYPE[1]~QUANT;                                           00082600
               PNT~SCAN;                                                00082700
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00082800
               NUM[1]~QUANT;                                            00082900
               PNT~SCAN;                                                00083000
               IF PNT!20 THEN ERROR(E16);                               00083100
               IF TYPE[1]="P" THEN                                      00083200
               WRITE (TTOUT(STA),PSIZE,NUM[1],IF NUM[2]:=POLY[NUM[1],0] 00083300
                 -1<10 AND NUM[2]>-1 THEN 1 ELSE 2,NUM[2])[:MORE] ELSE  00083400
          BEGIN                                                         00083500
               MSG.[6:6]~"C";                                           00083600
               MSGL~1;                                                  00083700
               ERROR(E11);                                              00083800
          END;                                                          00083900
               ERRTOG~0;                                                00084000
               GO TO MORE;                                              00084100
          END;                                                          00084200
PRINTER:       BEGIN                                                    00084300
LABEL          ENDPRINT;                                                00084400
               PNT~SCAN;                                                00084500
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00084600
               TYPE[1]~QUANT;                                           00084700
               PNT~SCAN;                                                00084800
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00084900
               NUM[1]~QUANT;                                            00085000
               PNT~SCAN;                                                00085100
               IF PNT!20 THEN ERROR(E16);                               00085200
               IF TYPE[1]="P" THEN                                      00085300
          BEGIN                                                         00085400
               NUM[2]:=POLY[NUM[1],0];                                  00085500
               K:=0;                                                    00085600
               FOR I~1 STEP 1 UNTIL NUM[2] DO                           00085700
               IF POLY[NUM[1],I]!0 THEN K:=K+1;                         00085800
               IF K=0 THEN                                              00085900
          BEGIN                                                         00086000
               WRITE (TTOUT(STA),PNULL,NUM[1],IF NUM[2]-1<10 AND NUM[2] 00086100
                    -1>-1 THEN 1 ELSE 2,NUM[2]-1)[:BRK ];               00086200
               GO TO ENDPRINT;                                          00086300
          END;                                                          00086400
               PWRITE(NUM);                                             00086500
          END  ELSE                                                     00086600
               WRITE (TTOUT(STA),WC,NUM[1],IF CONST[NUM[1]]<@-1 OR      00086700
                    CONST[NUM[1]]>@5 THEN "E" ELSE "F",CONST[NUM[1]]);  00086800
ENDPRINT:      ERRTOG~0;                                                00086900
               WRITE (TTOUT(STA),<X8,"{!~">);                           00087000
               GO TO MORE;                                              00087100
          END;                                                          00087200
INPUT:    BEGIN                                                         00087300
LABEL          ENDINPUT;                                                00087400
FORMAT         FMQUEST(X8,"{!",2A1,"=",A1,"{!~");                       00087500
               LABEL CDONE;                                             00087600
               PNT~SCAN;                                                00087700
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00087800
               TYPE[1]~QUANT;                                           00087900
               PNT~SCAN;                                                00088000
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00088100
               NUM[1]~QUANT;                                            00088200
               IF TYPE[1]="C" THEN                                      00088300
               GO TO CDONE;                                             00088400
               PNT~SCAN;                                                00088500
               IF PNT!21 THEN                                           00088600
          BEGIN                                                         00088700
               MSG.[6:6]~TYPE[1];                                       00088800
               MSG.[12:6]~NUM[1];                                       00088900
               ERROR(E3);                                               00089000
          END;                                                          00089100
               PNT~SCAN;                                                00089200
               IF PNT!40 THEN                                           00089300
          BEGIN                                                         00089400
               WO~NLP;                                                  00089500
               ERROR(E15);                                              00089600
          END;                                                          00089700
               IF QUANT>19 THEN                                         00089800
          BEGIN                                                         00089900
               MSG.[6:6]~ENTIER(QUANT/10);                              00090000
               MSG.[12:6]~QUANT-10|ENTIER(QUANT/10);                    00090100
               ERROR(E8);                                               00090200
          END;                                                          00090300
               NUM[2]~QUANT;                                            00090400
               NUM[2]~NUM[2]+1;                                         00090500
               PNT~SCAN;                                                00090600
               IF PNT!22 THEN                                           00090700
          BEGIN                                                         00090800
               WO~NLP;                                                  00090900
               ERROR(E5);                                               00091000
          END;                                                          00091100
CDONE:         PNT~SCAN;                                                00091200
               IF PNT!20 THEN ERROR(E16);                               00091300
               IF TYPE[1]="P" THEN                                      00091400
          BEGIN                                                         00091500
               WRITE (TTOUT(STA),FMQUEST,TYPE[1],NUM[1],QUEST);         00091600
               FOR I~0 STEP 1 UNTIL 20 DO                               00091700
               POLY[NUM[1],I]:=0;                                       00091800
               POLY[NUM[1],0]:=NUM[2];                                  00091900
               PREAD(NUM);                                              00092000
          END  ELSE                                                     00092100
          BEGIN                                                         00092200
               WRITE (TTOUT(STA),RC,NUM[1],QUEST);                      00092300
               READVAL(CONST[NUM[1]],STA);                              00092400
          END;                                                          00092500
ENDINPUT:      ERRTOG~0;                                                00092600
               WRITE (TTOUT(STA),<X8,"{!~">);                           00092700
               GO TO MORE;                                              00092800
          END;                                                          00092900
SET:      BEGIN                                                         00093000
LABEL          ENDLET;                                                  00093100
LABEL STP2;                                                             00093200
LABEL CHKLA;                                                            00093300
LABEL STP1;                                                             00093400
LABEL CHKEQ;                                                            00093500
               I~0;                                                     00093600
               PNT~SCAN;                                                00093700
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00093800
               TYPE[1]~QUANT;                                           00093900
               PNT~SCAN;                                                00094000
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00094100
               NUM[1]~QUANT;                                            00094200
               IF TYPE[1]="C" THEN                                      00094300
               GO TO CHKEQ;                                             00094400
               PNT~SCAN;                                                00094500
               IF PNT=21 THEN                                           00094600
          BEGIN                                                         00094700
               PNT~SCAN;                                                00094800
               IF PNT!40 THEN                                           00094900
          BEGIN                                                         00095000
               WO~NLP;                                                  00095100
               ERROR(E15);                                              00095200
          END;                                                          00095300
               IF QUANT>19 THEN                                         00095400
          BEGIN                                                         00095500
               MSG.[6:6]~ENTIER(QUANT/10);                              00095600
               MSG.[12:6]~QUANT-10|ENTIER(QUANT/10);                    00095700
               ERROR(E8);                                               00095800
          END;                                                          00095900
               I~QUANT+1;                                               00096000
               PNT~SCAN;                                                00096100
               IF PNT!22 THEN                                           00096200
          BEGIN                                                         00096300
               WO~NLP;                                                  00096400
               ERROR(E5);                                               00096500
          END;                                                          00096600
CHKEQ:         PNT~SCAN;                                                00096700
          END;                                                          00096800
               IF PNT!23 THEN                                           00096900
          BEGIN                                                         00097000
               MSG.[6:6]~TYPE[1];                                       00097100
               MSG.[12:6]~NUM[1];                                       00097200
               ERROR(E7);                                               00097300
          END;                                                          00097400
               PNT~SCAN;                                                00097500
               IF PNT>27 AND PNT<39 THEN GO TO STP1;                    00097600
               IF PNT=40 THEN GO TO RESET;                              00097700
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00097800
               TYPE[2]~QUANT;                                           00097900
               PNT~SCAN;                                                00098000
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00098100
               NUM[2]~QUANT;                                            00098200
               PNT~SCAN;                                                00098300
               IF PNT=20 THEN GO TO EQU;                                00098400
               IF PNT>23 AND PNT<28 THEN GO TO STP2;                    00098500
               ERROR(E6);                                               00098600
STP1:          LOC~PNT-23;                                              00098700
               IF PNT=31 OR PNT=33 THEN GO TO CHKLA;                    00098800
               PNT~SCAN;                                                00098900
               IF PNT!21 THEN                                           00099000
          BEGIN                                                         00099100
               MSG.[6:6]~" ";                                           00099200
               MSG.[12:6]~" ";                                          00099300
               ERROR(E3);                                               00099400
          END;                                                          00099500
               PNT~SCAN;                                                00099600
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00099700
               TYPE[2]~QUANT;                                           00099800
               PNT~SCAN;                                                00099900
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00100000
               NUM[2]~QUANT;                                            00100100
               PNT~SCAN;                                                00100200
               IF PNT!22 THEN                                           00100300
          BEGIN                                                         00100400
               WO~NLP;                                                  00100500
               ERROR(E5);                                               00100600
          END;                                                          00100700
CHKLA:         PNT~SCAN;                                                00100800
               IF PNT!20 THEN ERROR(E16);                               00100900
               GO TO F[LOC];                                            00101000
STP2:          LOC~PNT-23;                                              00101100
               PNT~SCAN;                                                00101200
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00101300
               TYPE[3]~QUANT;                                           00101400
               PNT~SCAN;                                                00101500
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00101600
               NUM[3]~QUANT;                                            00101700
               GO TO CHKLA;                                             00101800
          END;                                                          00101900
INOP:          ERROR(E6);                                               00102000
               GO TO MORE;                                              00102100
RESET:         IF I=0 AND TYPE[1]!"C" THEN                              00102200
          BEGIN                                                         00102300
               MSG.[6:6]~TYPE[1];                                       00102400
               MSG.[12:6]~NUM[1];                                       00102500
               ERROR(E3);                                               00102600
          END;                                                          00102700
               MOVER(RD[1],RD[0],56);                                   00102800
               GP~FINDCH(RD[0],LA)+1;                                   00102900
               SHIFT(RD[0],K~FINDCH(RD[0],"=")+1,GP~GP-K);              00103000
               SKIPBLANKS;                                              00103100
               IF NOT ANYFUNNYCHRS(RD[0],GP-1) THEN                     00103200
          BEGIN                                                         00103300
               IF TYPE[1]="C" THEN                                      00103400
               READARRAY(CONST[NUM[1]]) ELSE                            00103500
               IF TYPE[1]="P" THEN                                      00103600
               READARRAY(POLY[NUM[1],I]);                               00103700
               GO TO CHECK;                                             00103800
          END  ELSE                                                     00103900
          BEGIN                                                         00104000
               WRITE (TTOUT(STA),FIC);                                  00104100
               GO TO MORE;                                              00104200
          END;                                                          00104300
ADD:      BEGIN                                                         00104400
LABEL          ENDADD;                                                  00104500
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="P" THEN      00104600
          BEGIN                                                         00104700
               QADD(POLY[NUM[2],*],POLY[NUM[3],*],POLY[NUM[1],*]);      00104800
               GO TO ENDADD;                                            00104900
          END  ELSE                                                     00105000
               IF TYPE[1]="C" AND TYPE[2]="C" AND TYPE[3]="C" THEN      00105100
          BEGIN                                                         00105200
               CONST[NUM[1]]:=CONST[NUM[2]]+CONST[NUM[3]];              00105300
               GO TO ENDADD;                                            00105400
          END  ELSE                                                     00105500
          BEGIN                                                         00105600
               MSG.[6:6]~TYPE[1];                                       00105700
               MSG.[12:6]~"-";                                          00105800
               MSG.[18:6]~TYPE[2];                                      00105900
               MSG.[24:6]~"-";                                          00106000
               MSG.[30:6]~TYPE[3];                                      00106100
               MSGL~5;                                                  00106200
               ERROR(E11);                                              00106300
          END;                                                          00106400
ENDADD:   END;                                                          00106500
               GO TO CHECK;                                             00106600
SUB:      BEGIN                                                         00106700
LABEL          ENDSUB;                                                  00106800
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="P" THEN      00106900
          BEGIN                                                         00107000
               QSUB(POLY[NUM[2],*],POLY[NUM[3],*],POLY[NUM[1],*]);      00107100
               GO TO ENDSUB;                                            00107200
          END  ELSE                                                     00107300
               IF TYPE[1]="C" AND TYPE[2]="C" AND TYPE[3]="C" THEN      00107400
          BEGIN                                                         00107500
               CONST[NUM[1]]:=CONST[NUM[2]]-CONST[NUM[3]];              00107600
               GO TO ENDSUB;                                            00107700
          END  ELSE                                                     00107800
          BEGIN                                                         00107900
               MSG.[6:6]~TYPE[1];                                       00108000
               MSG.[12:6]~"-";                                          00108100
               MSG.[18:6]~TYPE[2];                                      00108200
               MSG.[24:6]~"-";                                          00108300
               MSG.[30:6]~TYPE[3];                                      00108400
               MSGL~5;                                                  00108500
               ERROR(E11);                                              00108600
          END;                                                          00108700
ENDSUB:   END;                                                          00108800
               GO TO CHECK;                                             00108900
MULT:     BEGIN                                                         00109000
LABEL          ENDMULT;                                                 00109100
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="P" THEN      00109200
          BEGIN                                                         00109300
               IF POLY[NUM[2],0]+POLY[NUM[3],0]>20 THEN                 00109400
          BEGIN                                                         00109500
               MSG.[6:6]~TYPE[3];                                       00109600
               MSG.[12:6]~NUM[3];                                       00109700
               ERROR(E9);                                               00109800
          END;                                                          00109900
               QMULT(POLY[NUM[2],*],POLY[NUM[3],*],POLY[NUM[1],*]);     00110000
               GO TO ENDMULT;                                           00110100
          END  ELSE                                                     00110200
               IF TYPE[1]="C" AND TYPE[2]="C" AND TYPE[3]="C" THEN      00110300
          BEGIN                                                         00110400
               CONST[NUM[1]]:=CONST[NUM[2]]|CONST[NUM[3]];              00110500
               GO TO ENDMULT;                                           00110600
          END  ELSE                                                     00110700
               IF TYPE[1]="P" AND TYPE[2]="C" AND TYPE[3]="P" THEN      00110800
          BEGIN                                                         00110900
               CTV(POLY[NUM[3],*],POLY[NUM[1],*],CONST[NUM[2]]);        00111000
               GO TO ENDMULT;                                           00111100
          END  ELSE                                                     00111200
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="C" THEN      00111300
          BEGIN                                                         00111400
               CTV(POLY[NUM[2],*],POLY[NUM[1],*],CONST[NUM[3]]);        00111500
               GO TO ENDMULT;                                           00111600
          END  ELSE                                                     00111700
          BEGIN                                                         00111800
               MSG.[6:6]~TYPE[1];                                       00111900
               MSG.[12:6]~"-";                                          00112000
               MSG.[18:6]~TYPE[2];                                      00112100
               MSG.[24:6]~"-";                                          00112200
               MSG.[30:6]~TYPE[3];                                      00112300
               MSGL~5;                                                  00112400
               ERROR(E11);                                              00112500
          END;                                                          00112600
ENDMULT:  END;                                                          00112700
               GO TO CHECK;                                             00112800
DVD:      BEGIN                                                         00112900
LABEL          ENDDVD;                                                  00113000
               IF TYPE[1]="C" AND TYPE[2]="C" AND TYPE[3]="C" THEN      00113100
          BEGIN                                                         00113200
               IF CONST[NUM[3]]=0 THEN                                  00113300
          BEGIN                                                         00113400
               MSG.[6:6]~TYPE[3];                                       00113500
               MSG.[12:6]~NUM[3];                                       00113600
               ERROR(E13);                                              00113700
          END;                                                          00113800
               CONST[NUM[1]]:=CONST[NUM[2]]/CONST[NUM[3]];              00113900
               GO TO ENDDVD;                                            00114000
          END  ELSE                                                     00114100
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="P" THEN      00114200
          BEGIN                                                         00114300
               IF I=0 THEN                                              00114400
          BEGIN                                                         00114500
               MSG.[6:6]~TYPE[1];                                       00114600
               MSG.[12:6]~NUM[1];                                       00114700
               ERROR(E3);                                               00114800
          END;                                                          00114900
               K:=0;                                                    00115000
               FOR J:=1 STEP 1 UNTIL POLY[NUM[3],0] DO                  00115100
               IF POLY[NUM[3],J]=0 THEN K:=K+1;                         00115200
               IF K=POLY[NUM[3],0] THEN                                 00115300
          BEGIN                                                         00115400
               MSG.[6:6]~TYPE[3];                                       00115500
               MSG.[12:6]~NUM[3];                                       00115600
               ERROR(E13);                                              00115700
          END;                                                          00115800
               QDIV(POLY[NUM[2],*],POLY[NUM[3],*],POLY[NUM[1],*],I);    00115900
               GO TO ENDDVD;                                            00116000
          END  ELSE                                                     00116100
          BEGIN                                                         00116200
               MSG.[6:6]~TYPE[1];                                       00116300
               MSG.[12:6]~"-";                                          00116400
               MSG.[18:6]~TYPE[2];                                      00116500
               MSG.[24:6]~"-";                                          00116600
               MSG.[30:6]~TYPE[3];                                      00116700
               MSGL~5;                                                  00116800
               ERROR(E11);                                              00116900
          END;                                                          00117000
ENDDVD:   END;                                                          00117100
               GO TO CHECK;                                             00117200
EQU:      BEGIN                                                         00117300
LABEL          ENDEQU;                                                  00117400
               IF TYPE[1]="P" AND TYPE[2]="P" THEN                      00117500
          BEGIN                                                         00117600
               FOR I~1 STEP 1 UNTIL POLY[NUM[2],0] DO                   00117700
               POLY[NUM[1],I]:=POLY[NUM[2],I];                          00117800
               POLY[NUM[1],0]:=POLY[NUM[2],0];                          00117900
               GO TO ENDEQU;                                            00118000
          END  ELSE                                                     00118100
               IF TYPE[1]="C" AND TYPE[2]="C" THEN                      00118200
          BEGIN                                                         00118300
               CONST[NUM[1]]:=CONST[NUM[2]];                            00118400
               GO TO ENDEQU;                                            00118500
          END  ELSE                                                     00118600
          BEGIN                                                         00118700
               MSG.[6:6]~TYPE[1];                                       00118800
               MSG.[12:6]~"-";                                          00118900
               MSG.[18:6]~TYPE[2];                                      00119000
               MSGL~3;                                                  00119100
               ERROR(E11);                                              00119200
          END;                                                          00119300
ENDEQU:   END;                                                          00119400
               GO TO CHECK;                                             00119500
NULL:     BEGIN                                                         00119600
LABEL          ENDNULL;                                                 00119700
               IF TYPE[1]="P" THEN                                      00119800
          BEGIN                                                         00119900
               FOR J:=1 STEP 1 UNTIL I DO                               00120000
               POLY[NUM[1],J]:=0;                                       00120100
               POLY[NUM[1],0]:=I;                                       00120200
               GO TO ENDNULL;                                           00120300
          END  ELSE                                                     00120400
          BEGIN                                                         00120500
          CONST[NUM[1]]:=0;                                             00120600
               GO TO ENDNULL;                                           00120700
          END;                                                          00120800
ENDNULL:  END;                                                          00120900
CHECK:         WRITE (TTOUT(STA),DONE)[:BRK ];                          00121000
               ERRTOG~0;                                                00121100
               GO TO MORE;                                              00121200
ROOTS:    BEGIN                                                         00121300
               PNT~SCAN;                                                00121400
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00121500
               TYPE[1]~QUANT;                                           00121600
               PNT~SCAN;                                                00121700
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00121800
               NUM[1]~QUANT;                                            00121900
               PNT~SCAN;                                                00122000
               IF PNT!20 THEN ERROR(E16);                               00122100
               IF TYPE[1]!"P" THEN                                      00122200
          BEGIN                                                         00122300
               MSG.[6:6]~TYPE[1];                                       00122400
               MSGL~1;                                                  00122500
               ERROR(E11);                                              00122600
          END;                                                          00122700
               PROOTS(POLY[NUM[1],*]);                                  00122800
               ERRTOG~0;                                                00122900
               WRITE (TTOUT(STA),DONE)[:BRK ];                          00123000
               GO TO MORE;                                              00123100
          END;                                                          00123200
FREQL:    BEGIN                                                         00123300
LABEL MODE;                                                             00123400
PROCEDURE ERROR2(N1,N2,N3,N4,NC,BACK,NCH);                              00123500
VALUE N1,N2,N3,N4,NC,BACK,NCH;                                          00123600
ALPHA N1,N2,N3,N4,NCH;                                                  00123700
INTEGER NC,BACK;                                                        00123800
          BEGIN                                                         00123900
               PRT(LINE,N1,N2,N3,N4,NC,BACK,NCH);                       00124000
               MARK(RD,NCT);                                            00124100
               WRITE(TTOUT(STA),8,RD[*]);                               00124200
               WRITE(TTOUT(STA),8,LINE[*]);                             00124300
               ERRTOG~ERRTOG+1;                                         00124400
               IF ERRTOG=5 THEN                                         00124500
WRITE(TTOUT(STA),<X8,"{!PLEASE READ THE INSTRUCTION MANUAL.{!!~">);     00124600
               IF ERRTOG=6 THEN                                         00124700
               GO TO STOP;                                              00124800
               GO TO MODE;                                              00124900
          END ERROR2;                                                   00125000
LABEL               ENDFREQ;                                            00125100
LABEL          RBRK;                                                    00125200
LABEL          RMSG;                                                    00125300
BOOLEAN        FORLOOP;                                                 00125400
REAL           W,MAG,ANGLE,DB;                                          00125500
REAL           LOW,INCREM,HIGH;                                         00125600
FORMAT         HEADING(X8,"{!",X5,"W",X10,"MAG",X8,"ANGLE",X8,"DB{!!~"  00125700
),FI(X8,"{!INCREMENT CANT BE ZERO.  TYPE LINE AGAIN.{!~"),              00125800
               F1(X8,"{!",4(F10.3,X2),"~"),                             00125900
               F2(X8,"{!",X12,3(F10.3,X2),"{!!~"),                      00126000
               FE(X8,"{!TYPING ERROR.  TYPE LINE AGAIN.{!~");           00126100
               PNT~SCAN;                                                00126200
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00126300
               TYPE[1]~QUANT;                                           00126400
               PNT~SCAN;                                                00126500
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00126600
               NUM[1]~QUANT;                                            00126700
               PNT~SCAN;                                                00126800
               IF PNT!26 THEN                                           00126900
          BEGIN                                                         00127000
               MSG.[6:6]~TYPE[1];                                       00127100
               MSG.[12:6]~NUM[1];                                       00127200
               ERROR(E14);                                              00127300
          END;                                                          00127400
               PNT~SCAN;                                                00127500
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00127600
               TYPE[2]~QUANT;                                           00127700
               PNT~SCAN;                                                00127800
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00127900
               NUM[2]~QUANT;                                            00128000
               PNT~SCAN;                                                00128100
               IF PNT!20 THEN ERROR(E16);                               00128200
               IF TYPE[1]!"P" OR TYPE[2]!"P" THEN                       00128300
          BEGIN                                                         00128400
               MSG.[6:6]~TYPE[1];                                       00128500
               MSG.[12:6]~"-";                                          00128600
               MSG.[18:6]~TYPE[2];                                      00128700
               MSGL~3;                                                  00128800
               ERROR(E11);                                              00128900
          END;                                                          00129000
               WRITE (TTOUT(STA),HEADING);                              00129100
               NCT~NC;                                                  00129200
MODE:          FORLOOP:=FALSE;                                          00129300
               READ (TTIN(STA,900),8,RD[*])[STOP:STOP];                 00129400
               PNT~SCAN;                                                00129500
               IF PNT=38 THEN                                           00129600
               GO TO ENDFREQ;                                           00129700
               NCT~NC;                                                  00129800
               GP:=FINDCH(RD[1],LA)+1;                                  00129900
               MOVER(RD[1],RD[0],GP);                                   00130000
               SKIPBLANKS;                                              00130100
               IF FINDCH(RD[0],"(")<GP THEN                             00130200
          BEGIN                                                         00130300
               FORLOOP:=TRUE;                                           00130400
               IF COUNTCOMMAS(RD[0],GP)!2 THEN                          00130500
          BEGIN                                                         00130600
               WO~NLP;                                                  00130700
               ERROR2(E4);                                              00130800
          END;                                                          00130900
               IF FINDCH(RD[0],")")>GP THEN                             00131000
          BEGIN                                                         00131100
               WO~NCA;                                                  00131200
               ERROR2(E5);                                              00131300
          END;                                                          00131400
               SHIFT(RD[0],K~FINDCH(RD[0],"(")+1,GP~GP-K);              00131500
               PUTCHRIN(RD[0],FINDCH(RD[0],")"),CA);                    00131600
          END  ELSE                                                     00131700
               IF FINDCH(RD[0],")")<GP THEN                             00131800
          BEGIN                                                         00131900
               MSG.[6:6]~" ";                                           00132000
               MSG.[12:6]~" ";                                          00132100
               ERROR2(E3);                                              00132200
          END;                                                          00132300
               IF FORLOOP THEN                                          00132400
          BEGIN                                                         00132500
               READ (RD[*],/,LOW,INCREM,HIGH);                          00132600
IF INCREM=0 THEN BEGIN WRITE (TTOUT(STA),FI); GO TO MODE; END;          00132700
               FOR W:=LOW STEP INCREM UNTIL HIGH DO                     00132800
          BEGIN                                                         00132900
               FREQ(W,POLY[NUM[1],*],POLY[NUM[2],*],MAG,ANGLE,DB);      00133000
               WRITE (TTOUT(STA),F1,W,MAG,ANGLE,DB)[:RBRK];             00133100
          END;                                                          00133200
               WRITE (TTOUT(STA),<X8,"{!~">);                           00133300
               GO TO MODE;                                              00133400
          END;                                                          00133500
               IF ANYFUNNYCHRS(RD[0],GP-1) THEN                         00133600
          BEGIN                                                         00133700
               WRITE (TTOUT(STA),FE);                                   00133800
               GO TO MODE;                                              00133900
          END;                                                          00134000
               READARRAY(W);                                            00134100
               FREQ(W,POLY[NUM[1],*],POLY[NUM[2],*],MAG,ANGLE,DB);      00134200
               WRITE (TTOUT(STA),F2,MAG,ANGLE,DB)[:RBRK];               00134300
               GO TO MODE;                                              00134400
ENDFREQ:       ERRTOG~0;                                                00134500
               WRITE (TTOUT(STA),DONE)[:BRK ];                          00134600
               GO TO MORE;                                              00134700
RBRK:          READ (TTIN(STA,0),8,RD[*])[RMSG];                        00134800
RMSG:          WRITE (TTOUT(STA),<X8,"{!BREAK.{!~">);                   00134900
               GO TO MODE;                                              00135000
          END;                                                          00135100
LOCI:     BEGIN                                                         00135200
LABEL          KEQ,FINIS;                                               00135300
BOOLEAN        WP;                                                      00135400
FORMAT         FE(X8,"{!TYPING ERROR.  TYPE LINE AGAIN.{!~");           00135500
               WP~FALSE;                                                00135600
               PNT~SCAN;                                                00135700
               IF PNT!21 THEN                                           00135800
          BEGIN                                                         00135900
               MSG.[6:12]~"  ";                                         00136000
               ERROR(E3);                                               00136100
          END;                                                          00136200
               PNT~SCAN;                                                00136300
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00136400
               TYPE[1]~QUANT;                                           00136500
               PNT~SCAN;                                                00136600
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00136700
               NUM[1]~QUANT;                                            00136800
               PNT~SCAN;                                                00136900
               IF PNT!39 THEN ERROR(E4);                                00137000
               PNT~SCAN;                                                00137100
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00137200
               TYPE[2]~QUANT;                                           00137300
               PNT~SCAN;                                                00137400
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00137500
               NUM[2]~QUANT;                                            00137600
               PNT~SCAN;                                                00137700
               IF PNT!22 THEN                                           00137800
          BEGIN                                                         00137900
               WO~NCA;                                                  00138000
               ERROR(E5);                                               00138100
          END;                                                          00138200
               PNT~SCAN;                                                00138300
               IF PNT=37 THEN                                           00138400
          BEGIN                                                         00138500
               WP~TRUE;                                                 00138600
               PNT~SCAN;                                                00138700
          END;                                                          00138800
               IF PNT!20 THEN ERROR(E16);                               00138900
               IF TYPE[1]!"P" OR TYPE[2]!"P" THEN                       00139000
          BEGIN                                                         00139100
               MSG.[6:6]~TYPE[1];                                       00139200
               MSG.[12:6]~"-";                                          00139300
               MSG.[18:6]~TYPE[2];                                      00139400
               MSGL~3;                                                  00139500
               ERROR(E11);                                              00139600
          END;                                                          00139700
KEQ:           WRITE (TTOUT(STA),<X8,"{!K=",A1,"{!~">,QUEST);           00139800
               NCT~NC;                                                  00139900
               K~0;                                                     00140000
               READ (TTIN(STA,900),8,RD[*])[STOP:STOP];                 00140100
               PNT~SCAN;                                                00140200
               IF PNT=38 THEN GO TO FINIS;                              00140300
               GP:=FINDCH(RD[1],LA)+1;                                  00140400
               MOVER(RD[1],RD[0],GP);                                   00140500
               SKIPBLANKS;                                              00140600
               IF ANYFUNNYCHRS(RD[0],GP-1) THEN                         00140700
          BEGIN                                                         00140800
               WRITE (TTOUT(STA),FE);                                   00140900
               GO TO KEQ;                                               00141000
          END;                                                          00141100
               READARRAY(NUM[3]);                                       00141200
               CTV(POLY[NUM[2],*],SPOLY,NUM[3]);                        00141300
               QADD(SPOLY,POLY[NUM[1],*],WPOLY);                        00141400
               IF WP THEN                                               00141500
          BEGIN                                                         00141600
               IF POLY[0,0]!0 THEN                                      00141700
          BEGIN                                                         00141800
               FOR I~0 STEP 1 UNTIL POLY[0,0] DO                        00141900
               SPOLY[I]~POLY[0,I];                                      00142000
               K~1;                                                     00142100
          END;                                                          00142200
               FOR I~0 STEP 1 UNTIL WPOLY[0] DO                         00142300
               POLY[0,I]~WPOLY[I];                                      00142400
               I~NUM[1];                                                00142500
               J~NUM[2];                                                00142600
               NUM[1]~0;                                                00142700
               NUM[2]~WPOLY[0];                                         00142800
               PWRITE(NUM);                                             00142900
               NUM[1]~I;                                                00143000
               NUM[2]~J;                                                00143100
               POLY[0,0]~0;                                             00143200
               IF K=1 THEN                                              00143300
               FOR I~0 STEP 1 UNTIL SPOLY[0] DO                         00143400
               POLY[0,I]~SPOLY[I];                                      00143500
          END;                                                          00143600
               PROOTS(WPOLY);                                           00143700
               GO TO KEQ;                                               00143800
FINIS:         ERRTOG~0;                                                00143900
               WRITE (TTOUT(STA),DONE);                                 00144000
               GO TO MORE;                                              00144100
          END;                                                          00144200
VL:       BEGIN                                                         00144300
LABEL MODE;                                                             00144400
PROCEDURE ERROR2(N1,N2,N3,N4,NC,BACK,NCH);                              00144500
VALUE N1,N2,N3,N4,NC,BACK,NCH;                                          00144600
ALPHA N1,N2,N3,N4,NCH;                                                  00144700
INTEGER NC,BACK;                                                        00144800
          BEGIN                                                         00144900
               PRT(LINE,N1,N2,N3,N4,NC,BACK,NCH);                       00145000
               MARK(RD,NCT);                                            00145100
               WRITE(TTOUT(STA),8,RD[*]);                               00145200
               WRITE(TTOUT(STA),8,LINE[*]);                             00145300
               ERRTOG~ERRTOG+1;                                         00145400
               IF ERRTOG=5 THEN                                         00145500
WRITE(TTOUT(STA),<X8,"{!PLEASE READ THE INSTRUCTION MANUAL.{!!~">);     00145600
               IF ERRTOG=6 THEN                                         00145700
               GO TO STOP;                                              00145800
               GO TO MODE;                                              00145900
          END ERROR2;                                                   00146000
REAL           X,Y;                                                     00146100
LABEL          ENDVAL;                                                  00146200
LABEL          RBRK;                                                    00146300
LABEL          RMSG;                                                    00146400
BOOLEAN        FORLOOP;                                                 00146500
REAL           LOW,INCREM,HIGH;                                         00146600
FORMAT         HEADING(X8,"{!",X5,"X",X11,"Y{!!~"),                     00146700
  FI(X8,"{!INCREMENT CANT BE ZERO.  TYPE LINE AGAIN.{!~"),              00146800
               F1(X8,"{!",F10.3,X2,F10.3,"~"),                          00146900
               F2(X8,"{!",X12,F10.3,"{!!~"),                            00147000
               FE(X8,"{!TYPING ERROR.  TYPE LINE AGAIN.{!~");           00147100
               PNT~SCAN;                                                00147200
               IF PNT<43 OR PNT>44 THEN ERROR(E2);                      00147300
               TYPE[1]~QUANT;                                           00147400
               PNT~SCAN;                                                00147500
               IF PNT!40 OR QUANT>9 THEN ERROR(E2A);                    00147600
               NUM[1]~QUANT;                                            00147700
               PNT~SCAN;                                                00147800
               IF PNT!20 THEN ERROR(E16);                               00147900
               IF TYPE[1]!"P" THEN                                      00148000
          BEGIN                                                         00148100
               MSG.[6:6]~TYPE[1];                                       00148200
               MSGL~1;                                                  00148300
               ERROR(E11);                                              00148400
          END;                                                          00148500
               WRITE (TTOUT(STA),HEADING);                              00148600
               NCT~NC;                                                  00148700
MODE:          FORLOOP:=FALSE;                                          00148800
               READ (TTIN(STA,900),8,RD[*])[STOP:STOP];                 00148900
               PNT~SCAN;                                                00149000
               IF PNT=38 THEN                                           00149100
               GO TO ENDVAL;                                            00149200
               NCT~NC;                                                  00149300
               GP:=FINDCH(RD[1],LA)+1;                                  00149400
               MOVER(RD[1],RD[0],GP);                                   00149500
               SKIPBLANKS;                                              00149600
               IF FINDCH(RD[0],"(")<GP THEN                             00149700
          BEGIN                                                         00149800
               FORLOOP:=TRUE;                                           00149900
               IF COUNTCOMMAS(RD[0],GP)!2 THEN                          00150000
          BEGIN                                                         00150100
               WO~NLP;                                                  00150200
               ERROR2(E4);                                              00150300
          END;                                                          00150400
               IF FINDCH(RD[0],")")>GP THEN                             00150500
          BEGIN                                                         00150600
               WO~NCA;                                                  00150700
               ERROR2(E5);                                              00150800
          END;                                                          00150900
               SHIFT(RD[0],K~FINDCH(RD[0],"(")+1,GP~GP-K);              00151000
               PUTCHRIN(RD[0],FINDCH(RD[0],")"),CA);                    00151100
          END  ELSE                                                     00151200
               IF FINDCH(RD[0],")")<GP THEN                             00151300
          BEGIN                                                         00151400
               MSG.[6:6]~" ";                                           00151500
               MSG.[12:6]~" ";                                          00151600
               ERROR2(E3);                                              00151700
          END;                                                          00151800
               IF FORLOOP THEN                                          00151900
          BEGIN                                                         00152000
               READ (RD[*],/,LOW,INCREM,HIGH);                          00152100
IF INCREM=0 THEN BEGIN WRITE (TTOUT(STA),FI); GO TO MODE; END;          00152200
               FOR X~LOW STEP INCREM UNTIL HIGH DO                      00152300
          BEGIN                                                         00152400
               Y~VAL(X,POLY[NUM[1],*]);                                 00152500
               WRITE (TTOUT(STA),F1,X,Y)[:RBRK];                        00152600
          END;                                                          00152700
               WRITE (TTOUT(STA),<X8,"{!~">);                           00152800
               GO TO MODE;                                              00152900
          END;                                                          00153000
               IF ANYFUNNYCHRS(RD[0],GP-1) THEN                         00153100
          BEGIN                                                         00153200
               WRITE (TTOUT(STA),FE);                                   00153300
               GO TO MODE;                                              00153400
          END;                                                          00153500
               READARRAY(X);                                            00153600
               Y~VAL(X,POLY[NUM[1],*]);                                 00153700
               WRITE (TTOUT(STA),F2,Y)[:RBRK];                          00153800
               GO TO MODE;                                              00153900
ENDVAL:        ERRTOG~0;                                                00154000
               WRITE (TTOUT(STA),DONE)[:BRK ];                          00154100
               GO TO MORE;                                              00154200
RBRK:          READ (TTIN(STA,0),8,RD[*])[RMSG];                        00154300
RMSG:          WRITE (TTOUT(STA),<X8,"{!BREAK.{!~">);                   00154400
               GO TO MODE;                                              00154500
          END;                                                          00154600
BRK:           READ (TTIN(STA,0),8,RD[*])[BMSG];                        00154700
BMSG:         WRITE (TTOUT(STA),<X8,"{!BREAK.{!~">);                    00154800
              GO TO MORE;                                               00154900
STOP:          PNT~SCAN;                                                00155000
               IF PNT!20 THEN ERROR(E16);                               00155100
               WATCHER[4]:=TIME(2);   PUTCHRIN(WATCHER[1],0,"0");   %SPY00155200
               WATCHER[1]:=TIME(0);  WATCHER[1].[12:6]~2;           %SPY00155300
               WATCHER[3]:=TIME(1);                                 %SPY00155400
               READ (DISC[0],*,PNTR)[STOP];                         %SPY00155500
               WRITE (TTOUT(STA),<X8,"{!PROCESSOR TIME = ",F8.2,        00155600
                 " SEC.{!~">,WATCHER[4]/60);                            00155700
               IF WATCHER[0].[6:36]="WITKIN" THEN                       00155800
               WRITE(TTOUT(STA),<X8,"{!THERE ARE ",I2," USERS.{!!~">,   00155900
                    PNTR+1);                                            00156000
               WRITE(TTOUT(STA),<X8,"{!GOODBYE.{!!~">);                 00156100
               WRITE (DISC[PNTR+1],5,WATCHER[*]);                   %SPY00156200
               WRITE (DISC[0],*,PNTR+1);                            %SPY00156300
%              LOCK (DISC,SAVE);                                    %SPY00156400
%              IF PNTR+1}50 THEN                                        00156500
%         BEGIN                                                         00156600
%FORMAT        F("CC EXECUTE PROGRAM/READER              .B2/12137-025",00156700
%                   "3 81184 WITKIN LOUIS;END.");                       00156800
%              WRITE (CONST[*],F);                                      00156900
%              ZIP WITH CONST[*];                                       00157000
%         END;                                                          00157100
          END.                                                          00157200
