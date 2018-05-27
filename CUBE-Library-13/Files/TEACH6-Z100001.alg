          BEGIN                                                         00000000
COMMENT        PROGRAM TEACHER/DU, CUBE LIBRARY NUMBER IS Z100001.      00000100
               THIS VERSION DATED 10/06/67;                                     
COMMENT        COMPILATION WITH CORE=1500 AND PRIORITY=3 RECOMMENDED;   00000300
COMMENT        TEACHER/DU, A VERSION OF THE IBM COURSEWRITER            00000400
               PROGRAM FOR COMPUTER-AIDED INSTRUCTION.                  00000500
               VERSION  6 USES SEARCH INSTEAD OF LIBRARIAN TO BE        00000600
               COMPATIBLE WITH FILE SECURITY, & USES EFFICIENT BRANCH   00000700
               SUGGESTED BY DAVE DAHM.                                  00000800
               THIS PROGRAM INTERPRETIVELY EXECUTES PROGRAMMED INST     00000900
               MATERIAL PREPARED USING THE FOLLOWING 10 OPERATIONS:     00001000
RD  TYPES & WAITS FOR STUDENT TO PRESS~                                 00001100
QU  TYPES TEXT & CONTINUES TO NEXT STATEMENT                            00001200
TY  TYPES TEXT IF PRIOR COND. MET--FOLLOWS CA & 1 OR MORE CB.           00001300
CA  CORRECT ANSWER-COMPARED WITH STUDENTS ANSWER.                       00001400
CB  ADDITIONAL FORMS OF SAME CORRECT ANSWER                             00001500
WA  WRONG ANSWER-COMPARED WITH STUDENTS ANSWER                          00001600
WB  ADDL FORMS OF SAME WRONG ANSWER                                     00001700
UN  TEXT TO BE TYPED IF STUDENTS RESPONSE IS UNANTICIPATED              00001800
BR  BRANCH TO LABEL                                                     00001900
LA  LABEL                                                               00002000
    BLANK OP FIELD MEANS CONTINUATION OF PREVIOUS OP--LEGAL ONLY FOR    00002100
   QU,RD,TY, AND UN.                                                    00002200
COL  2-3  OP CODE A2                                                    00002300
COL  5-64 TEXT OR LABEL, 10A6                                           00002400
COL 73-80 SEQUENCE NUMBER                                               00002500
A[*] CONTAINS CHARACTER STRING AS FOLLOWS:                              00002600
   0 BLANK, 2-CHR OPCODE, BLANK, FIRST 4 CHR OF TEXT                    00002700
 1-7 56 CHR OF TEXT, MAKING TOTAL OF 60 CHR                             00002800
   8 RECORD NO. TO BRANCH TO IF OP IS BR.  IF 8 IS BLANKS THEN          00002900
      WE LOOK FOR LABEL & WRITE IT BACK FOR NEXT TIME.  IF NOT BLANK    00003000
      THEN WE READ COURSE[N]. IF LABEL MATCHES WE ARE DONE.  IF NOT,    00003100
      WE LOOK FOR IT & WRITE IT BACK.                                   00003200
   9 SEQUENCE NUMBER, 8 DIGITS ALPHA                                    00003300
*******************;                                                    00003400
ALPHA FILE IN  TTIN  14(4,10);                                          00003500
ALPHA FILE OUT TOUT  14(2,10);                                          00003600
FILE           COURSE DISK RANDOM "NAMEIT" "SAVE" (2,10,30);            00003700
ARRAY          DICT[0:6];                                               00003800
SAVE ARRAY     A[0:9];                                                  00003900
ARRAY          B[0:9];                                                  00004000
SAVE ARRAY     STU[0: 7];                                               00004100
ALPHA          ID1;                                                     00004200
ALPHA          OPCODE;                                                  00004300
INTEGER        I,N,NCA,NUN,STA,NC,M;                                    00004400
BOOLEAN        MATCH,RDFLAG;                                            00004500
BOOLEAN ARRAY  U[0:19];                                                 00004600
ALPHA ARRAY    OPS[0:11];                                               00004700
FORMAT         TPNAME(X8,"{!PLEASE TYPE COURSE NAME{!~"),               00004800
               TCH(X8,"{!YOU ARE USING TEACHER/DU, VERSION  6{!~");     00004900
FORMAT         F4(X8,"{!~"),                                            00005000
               FCA(X8,"HERE IS THE CORRECT ANSWER:{!~"),                00005100
            FNQU(X8,"NOW REPEAT THE CORRECT ANSWER.{!~");               00005200
FORMAT         FLEAVING(X8,"{!YOU ARE LEAVING THIS COURSE.{!~"),        00005300
               FNEWCOURSE(X8,"WOULD YOU LIKE TO SELECT ANOTHER ",       00005400
                             "COURSE.{!~"),                             00005500
          FQUIT(X8,"{!YOU HAVE QUIT BEFORE COMPLETING THE COURSE.{!~"), 00005600
         FEXIT(X8, "{!FINISHED. PLEASE LOG OUT.{!~"),                   00005700
               FYN(X8,"PLEASE ANSWER YES OR NO.{!~");                   00005800
FORMAT         FNOLAB(X8,"{!MISSING LABEL  ~"),                         00005900
               FNOCA(X8,"ERROR--NO CA FOLLOWS THIS QU{!~"),             00006000
               FNEXTQU(X8,"{!NO UN WAS PROVIDED. NEXT QU:{!~"),         00006100
               ER99(X8,"{!NO OPCODE 99 AT END OF COURSE{!~"),           00006200
               FBADOP(X8,"{!CA,CB,WB, OR TY INCORRECTLY PLACED{!~"),    00006300
               FBADNAME(X8,"{!NO COURSE NAMED ",A1,A6,". CHECK ",       00006400
                "NAME & RETYPE.{!~");                                   00006500
DEFINE    READSTU=READ(TTIN(STA),8,STU[*]);                             00006600
               WRITE(TOUT(STA),F4)#,                                    00006700
               LABWRD=A[0]#;                                            00006800
COMMENT        READSTU NOW WAITS INDEFINITELY FOR AN INPUT;             00006900
COMMENT        LATER PUT MESSAGE TO STUDENT IF HE WAITS LONGER THAN     00007000
               TIME IN READ STATEMENT--BY BRANCH TO NO-INPUT LABEL.     00007100
               ALSO NEED ABNORMAL-CONDITION LABEL LATER;                00007200
LABEL          LR,LF,NEXTCB,TESTTYC,NEXTREAD,NEXTQU,TESTWA,NEXTWB,      00007300
               TESTTYW,EXIT,LSEARCH,UN1,TMC,TMW,TERM,NO99,UNA,          00007400
               WANTNEW,WN1,TPNEW;                                       00007500
LABEL          RD,QU,TY,CA,CB,WA,WB,UN,BR,LA,L99,RDNAME;                00007600
SWITCH      OP~RD,QU,TY,CA,CB,WA,WB,UN,BR,LA,L99;                       00007700
STREAM PROCEDURE MOVEOP(F,T);                                           00007800
          BEGIN                                                         00007900
               SI~F; SI~SI+1; DI~T; DI~DI+6; DS~2 CHR;                  00008000
          END;                                                          00008100
PROCEDURE      NEXTLINE;                                                00008200
          BEGIN                                                         00008300
               N~N+1;                                                   00008400
               READ(COURSE[N],10,A[*])[NO99];                           00008500
               MOVEOP(A,OPCODE);                                        00008600
          END  OF NEXTLINE;                                             00008700
INTEGER STREAM PROCEDURE FINDGP(A);                                     00008800
          BEGIN                                                         00008900
LABEL          FOUND;                                                   00009000
               SI~A; 60(IF SC="~" THEN JUMP OUT 1 TO FOUND; SI~SI+1;    00009100
                 TALLY~TALLY+1);                                        00009200
FOUND:         FINDGP~TALLY;                                            00009300
          END;                                                          00009400
STREAM PROCEDURE MOVE(F,T,N);                                           00009500
VALUE          N;                                                       00009600
          BEGIN                                                         00009700
               SI~F; DI~T; DI~DI+1; DS~N CHR;                           00009800
          END;                                                          00009900
STREAM PROCEDURE FILLTHE(DICT);                                         00010000
          BEGIN                                                         00010100
               DI~DICT;                                                 00010200
               DI~DI+4; DS~4 LIT "STOP"; %[0]                           00010300
               DI~DI+4; DS~4 LIT "HELP"; %[1]                           00010400
               DI~DI+4; DS~10 LIT "START OVER"; DI~DI+2; %[2-3]         00010500
               DS~8 LIT "0SAVE   "; %[4]                                00010600
               DI~DI+4; DS~3 LIT "YES"; DI~DI+1; %[5]                   00010700
               DI~DI+4; DS~2 LIT "NO";  DI~DI+2; %[6]                   00010800
          END;                                                          00010900
INTEGER STREAM PROCEDURE NCH(A);                                        00011000
          BEGIN                                                         00011100
LABEL          BLANK,AGAIN;                                             00011200
               SI~A;                                                    00011300
               SI~SI+4;                                                 00011400
AGAIN:         60(IF SC=" " THEN JUMP OUT 1 TO BLANK;SI~SI+1;TALLY~     00011500
                TALLY+1);                                               00011600
BLANK:         SI~SI+1; TALLY~TALLY+1; IF SC!" " THEN GO TO AGAIN;      00011700
               SI~SI+1; TALLY~TALLY+1; IF SC!" " THEN GO TO AGAIN;      00011800
               TALLY~TALLY+62;                                          00011900
               NCH~TALLY;                                               00012000
          END;                                                          00012100
BOOLEAN STREAM PROCEDURE EQ(A,S,N);                                     00012200
VALUE          N;                                                       00012300
          BEGIN                                                         00012400
               TALLY~1;                                                 00012500
               SI~A; SI~SI+4;                                           00012600
                     DI~S; N(IF SC!DC THEN TALLY~0);                    00012700
               EQ~TALLY;                                                00012800
          END;                                                          00012900
PROCEDURE      TYPE;                                                    00013000
          BEGIN                                                         00013100
LABEL          L1;                                                      00013200
STREAM PROCEDURE MOVETEXT(F,T,N,M);                                     00013300
VALUE          N,M;                                                     00013400
          BEGIN                                                         00013500
               SI~F; SI~SI+M;                                           00013600
                     DI~T; DS~N CHR; DS~3 LIT "{!~";                    00013700
          END;                                                          00013800
STREAM PROCEDURE MOVE40(F,T);                                           00013900
          BEGIN                                                         00014000
               SI~F; SI~SI+4;                                           00014100
                         DI~T;DS~40 CHR; DS~1 LIT"~";                   00014200
          END;                                                          00014300
PROCEDURE WRITETEXT;                                                    00014400
          BEGIN                                                         00014500
               NC~NCH(A);                                               00014600
               IF NC{53 THEN                                            00014700
          BEGIN                                                         00014800
               MOVETEXT(A,STU[1],NC,4);                                 00014900
               WRITE(TOUT(STA),8,STU[*]);                               00015000
          END  ELSE                                                     00015100
          BEGIN                                                         00015200
               NC~NC-40;                                                00015300
               MOVE40(A,STU[1]);                                        00015400
               WRITE(TOUT(STA),8,STU[*]);                               00015500
               MOVETEXT(A,STU[1],NC,44);                                00015600
               WRITE(TOUT(STA),8,STU[*]);                               00015700
          END;                                                          00015800
          END  WRITETEXT;                                               00015900
               WRITETEXT;                                               00016000
L1:            NEXTLINE;                                                00016100
               IF OPCODE="  " THEN                                      00016200
          BEGIN                                                         00016300
               WRITETEXT;                                               00016400
               GO TO L1;                                                00016500
          END;                                                          00016600
          END  OF TYPE;                                                 00016700
PROCEDURE      BRANCH(A1);                                              00016800
ALPHA          A1;                                                      00016900
          BEGIN                                                         00017000
LABEL          GOTIT,NEXT,FINDLAB;                                      00017100
INTEGER        LOCLA,LOCBR;                                             00017200
STREAM PROCEDURE MOVE8(F,T);                                            00017300
          BEGIN                                                         00017400
               SI~F; DI~T; SI~SI+4; DS~8 CHR;  DS~1 LIT "~";            00017500
          END;                                                          00017600
STREAM PROCEDURE MOVELINE(F,T);                                         00017700
          BEGIN                                                         00017800
               SI~F; DI~T; DS~10 WDS;                                   00017900
          END;                                                          00018000
MONITOR        FLAG;                                                    00018100
               FLAG~FINDLAB;                                            00018200
               MOVE8(A1,STU[1]);                                        00018300
               LOCLA~A[8];                                              00018400
               COMMENT IF A[8] WAS BLANKS WE WILL GET A FLAG BIT ERROR  00018500
                & BRANCH TO FINDLAB;                                    00018600
               READ(COURSE[LOCLA],10,A[*]);                             00018700
               MOVEOP(A,OPCODE);                                        00018800
               IF OPCODE="LA" THEN IF EQ(LABWRD,STU[1],8) THEN          00018900
          BEGIN                                                         00019000
               N~LOCLA;                                                 00019100
               GO TO LSEARCH;                                           00019200
          END;                                                          00019300
               COMMENT IF WE GET HERE THERE WAS A RECNO BUT IT WAS      00019400
                WRONG, PROBABLY DUE TO A CHANGE SINCE LAST USE OF       00019500
                THIS COURSE;                                            00019600
               READ(COURSE[N],10,A[*]);                                 00019700
FINDLAB:                                                                00019800
               LOCBR~N;                                                 00019900
               MOVELINE(A,B);                                           00020000
               COMMENT SAVES REC & RECNO TO WRITE BACK AFTER LABEL      00020100
                ADDRESS HAS BEEN FILLED IN;                             00020200
               N~-1;                                                    00020300
NEXT:          NEXTLINE;                                                00020400
               IF OPCODE="99" THEN                                      00020500
          BEGIN                                                         00020600
               WRITE(TOUT(STA),FNOLAB);                                 00020700
               WRITE(TOUT(STA),8,STU[*]);                               00020800
               WRITE(TOUT(STA),F4);                                     00020900
               GO TO WANTNEW;                                           00021000
          END;                                                          00021100
               IF OPCODE="LA" THEN                                      00021200
               IF EQ(LABWRD,STU[1],8) THEN GO TO GOTIT;                 00021300
               GO TO NEXT;                                              00021400
GOTIT:                                                                  00021500
               B[8]~N;                                                  00021600
               WRITE(COURSE[LOCBR],10,B[*]);                            00021700
               GO TO LSEARCH;                                           00021800
          END  OF BRANCH;                                               00021900
PROCEDURE      TESTMATCH;                                               00022000
          BEGIN                                                         00022100
               NC~NCH(A);                                               00022200
               IF NC=FINDGP(STU[1]) AND                                 00022300
                 EQ(A,STU[1],NC) THEN MATCH~TRUE ELSE MATCH~FALSE;      00022400
          END;                                                          00022500
               FILL OPS[*] WITH 0,"RD","QU","TY","CA","CB","WA","WB",   00022600
                                  "UN","BR","LA","99";                  00022700
               FILLTHE(DICT);                                           00022800
               STA~STATUS(STU[*]);                                      00022900
               STA.[9:9]~STU[0].[9:9];                                  00023000
               WRITE(TOUT(STA),TCH);                                    00023500
TPNEW:         WRITE(TOUT(STA),TPNAME);                                 00023600
RDNAME:        READ(TTIN(STA),8,STU[*]);                                00023700
               WRITE(TOUT(STA),F4);                                     00023800
               ID1~"      ";                                            00023900
               I~FINDGP(STU[1]);                                        00024000
               IF I>7 THEN I~7;                                         00024100
               MOVE(STU[1],ID1,I);                                      00024200
               FILL COURSE WITH ID1,*;                                  00024300
               SEARCH(COURSE,A[*]);                                     00024400
               IF A[0]<2 THEN                                           00024500
          BEGIN                                                         00024600
               WRITE(TOUT(STA),FBADNAME,ID1.[6:6],ID1);                 00024700
               GO TO RDNAME;                                            00024800
          END;                                                          00024900
LF:            N~-1;                                                    00025000
               NEXTLINE;                                                00025100
               MATCH~RDFLAG~FALSE;                                      00025200
LSEARCH:       FOR I~1 STEP 1 UNTIL 11 DO IF OPCODE=OPS[I] THEN         00025300
               GO TO OP[I];                                             00025400
QU:            FOR I~0 STEP 1 UNTIL 19 DO U[I]~FALSE;                   00025500
               TYPE;                                                    00025600
               IF OPCODE!"CA" THEN BEGIN WRITE(TOUT(STA),FNOCA);        00025700
                GO TO NEXTQU; END;                                      00025800
               NCA~N; COMMENT SAVES LOC OF CA FOR LOOPING BACK;         00025900
NEXTREAD:      READSTU;                                                 00026000
               READ(COURSE[N],10,A[*])[NO99];                           00026100
               MOVEOP(A,OPCODE);                                        00026200
               IF EQ(DICT[0],STU[1],4) THEN                             00026300
               BEGIN WRITE(TOUT(STA),FQUIT);                            00026400
                 GO TO WANTNEW END;                                     00026500
               IF RDFLAG THEN BEGIN RDFLAG~FALSE; GO TO LSEARCH END;    00026600
               IF EQ(DICT[1],STU[1],4) THEN                             00026700
          BEGIN                                                         00026800
               WRITE(TOUT(STA),FCA);                                    00026900
               N~NCA;                                                   00027000
               READ(COURSE[N],10,A[*])[NO99];                           00027100
               TYPE;                                                    00027200
               WRITE(TOUT(STA),FNQU);                                   00027300
               N~NCA;                                                   00027400
               GO TO NEXTREAD;                                          00027500
          END;                                                          00027600
               IF EQ(DICT[2],STU[1],10) THEN GO TO LF;                  00027700
               TESTMATCH;                                               00027800
               IF MATCH THEN GO TO TESTTYC;                             00027900
NEXTCB:                                                                 00028000
               NEXTLINE;                                                00028100
               IF OPCODE="CB" OR OPCODE="CA" THEN                       00028200
          BEGIN                                                         00028300
TMC:           TESTMATCH;                                               00028400
               IF MATCH THEN GO TO TESTTYC;                             00028500
               NEXTLINE;                                                00028600
               IF OPCODE="CA" OR OPCODE="CB" THEN GO TO TMC;            00028700
          END;                                                          00028800
               IF OPCODE="TY" OR OPCODE="BR" OR OPCODE="  "             00028900
                 THEN GO TO NEXTCB;                                     00029000
               GO TO TESTWA;                                            00029100
TESTTYC:                                                                00029200
               NEXTLINE;                                                00029300
               IF OPCODE="CB" THEN GO TO TESTTYC;                       00029400
               IF OPCODE="BR" THEN BRANCH(LABWRD);                      00029500
               IF OPCODE="TY" THEN TYPE;                                00029600
               IF OPCODE="BR" THEN BRANCH(LABWRD);                      00029700
NEXTQU:                                                                 00029800
               IF OPCODE="RD" THEN GO TO RD;                            00029900
               IF OPCODE="QU" THEN GO TO QU;                            00030000
               IF OPCODE="99" THEN GO TO L99;                           00030100
               NEXTLINE;                                                00030200
               GO TO NEXTQU;                                            00030300
TESTWA:        IF OPCODE="WA" THEN GO TO WA;                            00030400
               IF OPCODE="UN" THEN GO TO UN;                            00030500
               NEXTLINE;                                                00030600
               GO TO TESTWA;                                            00030700
WA:            TESTMATCH;                                               00030800
               IF MATCH THEN GO TO TESTTYW;                             00030900
NEXTWB:                                                                 00031000
               NEXTLINE;                                                00031100
               IF OPCODE="WB" OR OPCODE="WA" THEN                       00031200
          BEGIN                                                         00031300
TMW:           TESTMATCH;                                               00031400
               IF MATCH THEN GO TO TESTTYW;                             00031500
               NEXTLINE;                                                00031600
               IF OPCODE="WB" OR OPCODE="WA" THEN GO TO TMW;            00031700
          END;                                                          00031800
               IF OPCODE="TY" OR OPCODE="  " OR OPCODE="BR"             00031900
                 THEN GO TO NEXTWB;                                     00032000
               IF OPCODE="UN" THEN GO TO UN;                            00032100
               WRITE(TOUT(STA),FNEXTQU);                                00032200
               GO TO NEXTQU;                                            00032300
TESTTYW:                                                                00032400
               NEXTLINE;                                                00032500
               IF OPCODE="WB" THEN GO TO TESTTYW;                       00032600
               IF OPCODE="BR" THEN BRANCH(LABWRD);                      00032700
               IF OPCODE="TY" THEN TYPE;                                00032800
               IF OPCODE="BR" THEN BRANCH(LABWRD);                      00032900
               N~NCA; COMMENT RESETS FOR NEXT TRY ON SAME QUESTION;     00033000
               GO TO NEXTREAD;                                          00033100
UN:            M~0;                                                     00033200
UNA:           NUN~N;                                                   00033300
               IF NOT U[M] THEN                                         00033400
          BEGIN                                                         00033500
               U[M]~TRUE;                                               00033600
               TYPE;                                                    00033700
               IF OPCODE="BR" THEN BRANCH(LABWRD);                      00033800
               N~NCA;                                                   00033900
               GO TO NEXTREAD;                                          00034000
          END;                                                          00034100
UN1:           M~M+1;                                                   00034200
               NEXTLINE;                                                00034300
               IF OPCODE="UN" THEN GO TO UNA;                           00034400
               IF OPCODE="  " THEN GO TO UN1;                           00034500
COMMENT        IF WE GET HERE WE HAVE PASSED LAST UN & WILL REPEAT IT.  00034600
               STUDENT CAN STILL GET OUT BY TYPING "HELP";              00034700
               N~NUN;                                                   00034800
               READ(COURSE[N],10,A[*])[NO99];                           00034900
               TYPE;                                                    00035000
               N~NCA;                                                   00035100
               GO TO NEXTREAD;                                          00035200
RD:            RDFLAG~TRUE;                                             00035300
               TYPE;                                                    00035400
               GO TO NEXTREAD;                                          00035500
TY: CA: CB: WB: WRITE(TOUT(STA),FBADOP);                                00035600
               GO TO NEXTQU;                                            00035700
BR:            BRANCH(LABWRD);                                          00035800
LA:            NEXTLINE;                                                00035900
               GO TO LSEARCH;                                           00036000
NO99:          WRITE(TOUT(STA),ER99);                                   00036100
L99:                                                                    00036200
WANTNEW:       CLOSE(COURSE,RELEASE);                                   00036300
               WRITE(TOUT(STA),FLEAVING);                               00036400
               WRITE(TOUT(STA),FNEWCOURSE);                             00036500
WN1:           READSTU;                                                 00036600
               IF EQ(DICT[5],STU[1],3) THEN GO TO TPNEW;                00036700
               IF EQ(DICT[6],STU[1],2) THEN GO TO EXIT;                 00036800
               WRITE(TOUT(STA),FYN);                                    00036900
               GO TO WN1;                                               00037000
EXIT:          WRITE(TOUT(STA),FEXIT);                                  00037100
TERM:     END.                                                          00037200
