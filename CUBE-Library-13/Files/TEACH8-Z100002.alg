          BEGIN                                                         00000000
COMMENT        CUBE LIBRARY NUMBER IS Z100002.  THIS VERSION                    
               DATED 1/30/68;                                                   
COMMENT        TEACHER/DU, A VERSION OF THE IBM COURSEWRITER            00000100
               PROGRAM FOR COMPUTER-AIDED INSTRUCTION.                  00000200
               VERSION  6 USES SEARCH INSTEAD OF LIBRARIAN TO BE        00000300
               COMPATIBLE WITH FILE SECURITY, & USES EFFICIENT BRANCH   00000400
               SUGGESTED BY DAVE DAHM.                                  00000500
               VERSION 7 IS RE-ENTRANT AND ALLOWS UMAX USERS, WHERE     00000600
               UMAX IS DEFINED IN FIRST STATEMENT OF PROGRAM.           00000700
               UMAX SET TO 4 AT U OF DENVER BUT DECLARATIONS FOR        00000800
               10 USERS ARE INCLUDED.                                   00000900
               PHILOSOPHY OF RE-ENTRANT VERSION IS TO PROCESS EACH      00001000
               USER UNTIL HE COMES TO A WRITE, THEN MARK THE PLACE      00001100
               HE WAS AND WORK ON THE NEXT USER. BOOLEAN ACTIVE IS KEPT 00001200
               TRUE ALONG WITH EITHER MOREPROCESS OR MORETYPE UNTIL     00001300
               ALL USERS ARE WAITING FOR INPUT.                         00001400
               VERSION 8 INCORPORATES BREAK LABELS ON ALL WRITES.       00001405
       THIS PROGRAM INTERPRETIVELY EXECUTES PROGRAMMED INSTRUCTION      00001500
       MATERIAL PREPARED USING THE FOLLOWING 10 OPERATIONS:             00001600
RD  TYPES & WAITS FOR STUDENT TO PRESS~                                 00001700
QU  TYPES TEXT & CONTINUES TO NEXT STATEMENT                            00001800
TY  TYPES TEXT IF PRIOR COND. MET--FOLLOWS CA & 1 OR MORE CB.           00001900
CA  CORRECT ANSWER-COMPARED WITH STUDENTS ANSWER.                       00002000
CB  ADDITIONAL FORMS OF SAME CORRECT ANSWER                             00002100
WA  WRONG ANSWER-COMPARED WITH STUDENTS ANSWER                          00002200
WB  ADDL FORMS OF SAME WRONG ANSWER                                     00002300
UN  TEXT TO BE TYPED IF STUDENTS RESPONSE IS UNANTICIPATED              00002400
BR  BRANCH TO LABEL                                                     00002500
LA  LABEL                                                               00002600
    BLANK OP FIELD MEANS CONTINUATION OF PREVIOUS OP--LEGAL ONLY FOR    00002700
   QU,RD,TY, AND UN.                                                    00002800
COL  2-3  OP CODE A2                                                    00002900
COL  5-64 TEXT OR LABEL, 10A6                                           00003000
COL 73-80 SEQUENCE NUMBER                                               00003100
A[*] CONTAINS CHARACTER STRING AS FOLLOWS:                              00003200
   0 BLANK, 2-CHR OPCODE, BLANK, FIRST 4 CHR OF TEXT                    00003300
 1-7 56 CHR OF TEXT, MAKING TOTAL OF 60 CHR                             00003400
   8 RECORD NO. TO BRANCH TO IF OP IS BR.  IF 8 IS BLANKS THEN          00003500
      WE LOOK FOR LABEL & WRITE IT BACK FOR NEXT TIME.  IF NOT BLANK    00003600
      THEN WE READ COURSE[N]. IF LABEL MATCHES WE ARE DONE.  IF NOT,    00003700
      WE LOOK FOR IT & WRITE IT BACK.                                   00003800
   9 SEQUENCE NUMBER, 8 DIGITS ALPHA                                    00003900
*******************;                                                    00004000
DEFINE         UMAX=04#;                                                00004100
 DEFINE        NTERM=04#;                                            %0400004105
ALPHA FILE IN  TTIN0   14(2,8),                                         00004200
               TTIN1   14(2,8),                                         00004300
               TTIN2   14(2,8),                                         00004400
               TTIN3   14(2,8),                                         00004500
               TTIN4   14(2,8),                                         00004600
               TTIN5   14(2,8),                                         00004700
               TTIN6   14(2,8),                                         00004800
               TTIN7   14(2,8),                                         00004900
               TTIN8   14(2,8),                                         00005000
               TTIN9   14(2,8),                                         00005100
               TTIN10  14(2,8);                                         00005200
SWITCH FILE    TTIN~TTIN0,TTIN1,TTIN2,TTIN3,TTIN4,TTIN5,TTIN6,TTIN7,    00005300
                    TTIN8,TTIN9,TTIN10;                                 00005400
ALPHA FILE OUT TOUT  14(2,10);                                          00005500
FILE           CRS0  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00005600
               CRS1  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00005700
               CRS2  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00005800
               CRS3  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00005900
               CRS4  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00006000
               CRS5  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00006100
               CRS6  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00006200
               CRS7  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00006300
               CRS8  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00006400
               CRS9  DISK RANDOM "NAMEIT" "SAVE" (1,10,30),             00006500
               CRS10 DISK RANDOM "NAMEIT" "SAVE" (1,10,30);             00006600
SWITCH FILE    COURSE~CRS0,CRS1,CRS2,CRS3,CRS4,CRS5,CRS6,CRS7,CRS8,     00006700
                      CRS9,CRS10;                                       00006800
SAVE                                                                    00006900
INTEGER ARRAY  STA[0:UMAX],  %TERM UNIT AND BUFFER NUMBER               00007000
               N[0:UMAX],    %LINE NUMBER                               00007100
               NCA[0:UMAX];  %LINE NUMBER OF CORRECT ANSWER             00007200
SAVE                                                                    00007300
INTEGER ARRAY  PLACE[0:UMAX];                                           00007400
 SAVE ARRAY USERS[0:NTERM|2];                                        %0400007405
SAVE                                                                    00007500
BOOLEAN ARRAY  RDFLAG[0:UMAX],                                          00007600
               MORETYPE[0:UMAX],                                        00007700
               U[0:UMAX,0:19],                                          00007800
               MOREPROCESS[0:UMAX],                                     00007900
               INUSE[0:UMAX];                                           00008000
SAVE                                                                    00008100
ARRAY          DICT[0:6];                                               00008200
SAVE ARRAY     A[0:9];                                                  00008300
ARRAY          B[0:9];                                                  00008400
SAVE ARRAY     STU[0: 7];                                               00008500
ALPHA          ID1;                                                     00008600
ALPHA          OPCODE;                                                  00008700
INTEGER        I,NUN,NC,M,USER;                                         00008800
BOOLEAN        MATCH,ANYUSER;                                        %0100008900
BOOLEAN        ARRAY ACT[0:UMAX];                                    %0100008905
DEFINE         ACTIVE=ACT[USER]#;                                    %0100008910
INTEGER        CT;                                                   %0100008915
ALPHA ARRAY    OPS[0:11];                                               00009000
LABEL          ENDSTA,PROC,LOOKACTIVE,ANYMORE,BRK;                      00009100
 INTEGER       NUSE,TEMP;                                            %0400009105
 LABEL         PRC,STATS,FOUNDIT;                                    %0400009110
FORMAT         TPNAME(X8,"{!PLEASE TYPE COURSE NAME{!~"),               00009200
               TCH(X8,"{!YOU ARE USING TEACHER/DU, VERSION 8{!~");      00009300
FORMAT         F4(X8,"{!~"),                                            00009400
               FCA(X8,"HERE IS THE CORRECT ANSWER:{!~"),                00009500
            FNQU(X8,"NOW REPEAT THE CORRECT ANSWER.{!~");               00009600
FORMAT         FLEAVING(X8,"{!YOU ARE LEAVING THIS COURSE.{!~"),        00009700
               FNEWCOURSE(X8,"WOULD YOU LIKE TO SELECT ANOTHER ",       00009800
                             "COURSE.{!~"),                             00009900
          FQUIT(X8,"{!YOU HAVE QUIT BEFORE COMPLETING THE COURSE.{!~"), 00010000
         FEXIT(X8, "{!FINISHED. PLEASE LOG OUT.{!~"),                   00010100
               FYN(X8,"PLEASE ANSWER YES OR NO.{!~");                   00010200
FORMAT         FNOLAB(X8,"{!MISSING LABEL  ~"),                         00010300
               FNOCA(X8,"ERROR--NO CA FOLLOWS THIS QU{!~"),             00010400
               FNEXTQU(X8,"{!NO UN WAS PROVIDED. NEXT QU:{!~"),         00010500
               ER99(X8,"{!NO OPCODE 99 AT END OF COURSE{!~"),           00010600
               FBADOP(X8,"{!CA,CB,WB, OR TY INCORRECTLY PLACED{!~"),    00010700
               FBADNAME(X8,"{!NO COURSE NAMED ",A1,A6,". CHECK ",       00010800
                "NAME & RETYPE.{!~");                                   00010900
FORMAT         FUSERS(X8,"USERS:",I2,". YOU ARE #",I2,".{!LINES:{!~"),  00011000
               FLINES(X8,I3,"{!~");                                     00011100
DEFINE         CALLBRANCH=BEGIN BRANCH(LABWRD);                         00011200
                          GO TO SW[PLACE[USER]];END#;                   00011300
DEFINE         LABWRD=A[0]#;                                            00011600
 PROCEDURE  UNATTATCH(USER);                                         %0400012005
    VALUE USER;                                                      %0400012010
    INTEGER USER;                                                    %0400012015
          BEGIN                                                      %0400012020
               INTEGER I;                                            %0400012025
               ANYUSER~INUSE[USER]~FALSE;                            %0400012030
               RELEASE(STA[USER]);                                   %0400012040
               STA[USER]~0;                                          %0400012041
               NUSE ~ NUSE -2;                                       %0400012042
               FOR I~1 STEP 1 UNTIL UMAX DO                          %0400012045
               IF INUSE[I] THEN ANYUSER~TRUE;                        %0400012050
          END  UNATTATCH;                                            %0400012055
PROCEDURE      BREAK;                                                   00012065
          BEGIN                                                         00012070
               UNATTATCH(USER);                                         00012075
               GO TO ANYMORE;                                           00012080
          END  BREAK;                                                   00012085
STREAM PROCEDURE MOVEOP(F,T);                                           00012100
          BEGIN                                                         00012200
               SI~F; SI~SI+1; DI~T; DI~DI+6; DS~2 CHR;                  00012300
          END;                                                          00012400
PROCEDURE      NO99;                                                    00012500
          BEGIN LABEL BRK;                                              00012600
               WRITE(TOUT(STA[USER]),ER99)[:BRK];                       00012700
               ACTIVE~MOREPROCESS[USER]~TRUE;                           00012800
               PLACE[USER]~6; GO TO ENDSTA;                             00012900
BRK:           BREAK;                                                   00012905
          END;                                                          00013000
PROCEDURE      NEXTLINE;                                                00013100
          BEGIN                                                         00013200
LABEL          CALLNO99,RETURN;                                         00013300
               N[USER]~N[USER]+1;                                       00013400
               READ(COURSE[USER][N[USER]],10,A[*])[CALLNO99];           00013500
               MOVEOP(A,OPCODE);                                        00013600
               GO TO RETURN;                                            00013700
CALLNO99:      NO99;                                                    00013800
RETURN:   END  OF NEXTLINE;                                             00013900
INTEGER STREAM PROCEDURE FINDGP(A);                                     00014000
          BEGIN                                                         00014100
LABEL          FOUND;                                                   00014200
               SI~A; 60(IF SC="~" THEN JUMP OUT 1 TO FOUND; SI~SI+1;    00014300
                 TALLY~TALLY+1);                                        00014400
FOUND:         FINDGP~TALLY;                                            00014500
          END;                                                          00014600
STREAM PROCEDURE MOVE(F,T,N);                                           00014700
VALUE          N;                                                       00014800
          BEGIN                                                         00014900
               SI~F; DI~T; DI~DI+1; DS~N CHR;                           00015000
          END;                                                          00015100
STREAM PROCEDURE FILLTHE(DICT);                                         00015200
          BEGIN                                                         00015300
               DI~DICT;                                                 00015400
               DI~DI+4; DS~4 LIT "STOP"; %[0]                           00015500
               DI~DI+4; DS~4 LIT "HELP"; %[1]                           00015600
               DI~DI+4; DS~12 LIT "HOW MANY USE";        %[2-3]         00015700
               DS~8 LIT "0SAVE   "; %[4]                                00015800
               DI~DI+4; DS~3 LIT "YES"; DI~DI+1; %[5]                   00015900
               DI~DI+4; DS~2 LIT "NO";  DI~DI+2; %[6]                   00016000
          END;                                                          00016100
INTEGER STREAM PROCEDURE NCH(A);                                        00016200
          BEGIN                                                         00016300
LABEL          BLANK,AGAIN;                                             00016400
               SI~A;                                                    00016500
               SI~SI+4;                                                 00016600
AGAIN:         60(IF SC=" " THEN JUMP OUT 1 TO BLANK;SI~SI+1;TALLY~     00016700
                TALLY+1);                                               00016800
BLANK:         SI~SI+1; TALLY~TALLY+1; IF SC!" " THEN GO TO AGAIN;      00016900
               SI~SI+1; TALLY~TALLY+1; IF SC!" " THEN GO TO AGAIN;      00017000
               TALLY~TALLY+62;                                          00017100
               NCH~TALLY;                                               00017200
          END;                                                          00017300
BOOLEAN STREAM PROCEDURE EQ(A,S,N);                                     00017400
VALUE          N;                                                       00017500
          BEGIN                                                         00017600
               TALLY~1;                                                 00017700
               SI~A; SI~SI+4;                                           00017800
                     DI~S; N(IF SC!DC THEN TALLY~0);                    00017900
               EQ~TALLY;                                                00018000
          END;                                                          00018100
PROCEDURE      TYPE;                                                    00018200
          BEGIN                                                         00018300
STREAM PROCEDURE MOVETEXT(F,T,N,M);                                     00018400
VALUE          N,M;                                                     00018500
          BEGIN                                                         00018600
               SI~F; SI~SI+M;                                           00018700
                     DI~T; DS~N CHR; DS~3 LIT "{!~";                    00018800
          END;                                                          00018900
STREAM PROCEDURE MOVE40(F,T);                                           00019000
          BEGIN                                                         00019100
               SI~F; SI~SI+4;                                           00019200
                         DI~T;DS~40 CHR; DS~1 LIT"~";                   00019300
          END;                                                          00019400
PROCEDURE WRITETEXT;                                                    00019500
          BEGIN LABEL BRK, ENDWRT;                                      00019600
               NC~NCH(A);                                               00019700
               IF NC{53 THEN                                            00019800
          BEGIN                                                         00019900
               MOVETEXT(A,STU[1],NC,4);                                 00020000
               WRITE(TOUT(STA[USER]),8,STU[*])[:BRK];                   00020100
          END  ELSE                                                     00020200
          BEGIN                                                         00020300
               NC~NC-40;                                                00020400
               MOVE40(A,STU[1]);                                        00020500
               WRITE(TOUT(STA[USER]),8,STU[*])[:BRK];                   00020600
               MOVETEXT(A,STU[1],NC,44);                                00020700
               WRITE(TOUT(STA[USER]),8,STU[*])[:BRK];                   00020800
          END;                                                          00020900
               GO TO ENDWRT;                                            00020990
BRK:           BREAK;                                                   00020995
ENDWRT:   END  WRITETEXT;                                               00021000
               WRITETEXT;                                               00021100
               NEXTLINE;                                                00021200
               ACTIVE~TRUE;                                             00021300
               IF OPCODE="  " THEN                                      00021400
               MORETYPE[USER]~TRUE ELSE                                 00021500
          BEGIN                                                         00021600
               MORETYPE[USER]~FALSE;                                    00021700
               MOREPROCESS[USER]~TRUE;                                  00021800
          END;                                                          00021900
          END  OF TYPE;                                                 00022000
 BOOLEAN PROCEDURE WHOISIT(L,U);                                     %0400022100
 VALUE         L;                                                    %0400022200
 REAL          L,U;                                                  %0400022300
          BEGIN                                                         00022400
INTEGER        I,J,K,LINE;                                              00022500
LABEL          DUN;                                                     00022600
               WHOISIT~TRUE;                                         %0400022605
               LINE~0&L[9:9:9];                                         00022700
               FOR I~1 STEP 1 UNTIL UMAX DO                             00022800
               IF LINE=STA[I] THEN                                      00022900
                 BEGIN                                                  00023000
                   J~I;                                                 00023100
                   GO TO DUN;                                           00023200
                 END;                                                   00023300
               WHOISIT~FALSE;                                        %0400023400
   DUN:        U~J;                                                  %0400024100
          END  OF WHOISIT;                                              00024200
PROCEDURE      TESTMATCH;                                               00024300
          BEGIN                                                         00024400
               NC~NCH(A);                                               00024500
               IF NC=FINDGP(STU[1]) AND                                 00024600
                 EQ(A,STU[1],NC) THEN MATCH~TRUE ELSE MATCH~FALSE;      00024700
          END;                                                          00024800
PROCEDURE      BRANCH(A1);                                              00024900
ALPHA          A1;                                                      00025000
          BEGIN                                                         00025100
LABEL          GOTIT,NEXT,FINDLAB,RETURN,BRK;                           00025200
INTEGER        LOCLA,LOCBR;                                             00025300
STREAM PROCEDURE MOVE8(F,T);                                            00025400
          BEGIN                                                         00025500
               SI~F; DI~T; SI~SI+4; DS~8 CHR;  DS~3 LIT "{!~";          00025600
          END;                                                          00025700
STREAM PROCEDURE MOVELINE(F,T);                                         00025800
          BEGIN                                                         00025900
               SI~F; DI~T; DS~10 WDS;                                   00026000
          END;                                                          00026100
MONITOR        FLAG;                                                    00026200
               FLAG~FINDLAB;                                            00026300
               MOVE8(A1,STU[1]);                                        00026400
               LOCLA~A[8];                                              00026500
               COMMENT IF A[8] WAS BLANKS WE WILL GET A FLAG BIT ERROR  00026600
                & BRANCH TO FINDLAB;                                    00026700
               READ(COURSE[USER][LOCLA],10,A[*]);                       00026800
               MOVEOP(A,OPCODE);                                        00026900
               IF OPCODE="LA" THEN IF EQ(LABWRD,STU[1],8) THEN          00027000
          BEGIN                                                         00027100
               N[USER]~LOCLA;                                           00027200
               PLACE[USER]~9;                                           00027300
               GO TO RETURN;                                            00027400
          END;                                                          00027500
               COMMENT IF WE GET HERE THERE WAS A RECNO BUT IT WAS      00027600
                WRONG, PROBABLY DUE TO A CHANGE SINCE LAST USE OF       00027700
                THIS COURSE;                                            00027800
               READ(COURSE[USER][N[USER]],10,A[*]);                     00027900
FINDLAB:                                                                00028000
               LOCBR~N[USER];                                           00028100
               MOVELINE(A,B);                                           00028200
               COMMENT SAVES REC & RECNO TO WRITE BACK AFTER LABEL      00028300
                ADDRESS HAS BEEN FILLED IN;                             00028400
               N[USER]~-1;                                              00028500
NEXT:          NEXTLINE;                                                00028600
               IF OPCODE="99" THEN                                      00028700
          BEGIN                                                         00028800
               WRITE(TOUT(STA[USER]),FNOLAB)[:BRK];                     00028900
               WRITE(TOUT(STA[USER]),8,STU[*])[:BRK];                   00029000
               PLACE[USER]~6;                                           00029100
               GO TO RETURN;                                            00029200
          END;                                                          00029300
               IF OPCODE="LA" THEN                                      00029400
               IF EQ(LABWRD,STU[1],8) THEN GO TO GOTIT;                 00029500
               GO TO NEXT;                                              00029600
GOTIT:                                                                  00029700
               B[8]~N[USER];                                            00029800
               WRITE(COURSE[USER][LOCBR],10,B[*]);                      00029900
               PLACE[USER]~9;                                           00030000
               GO TO RETURN;                                            00030100
BRK:           BREAK;                                                   00030105
RETURN:   END  OF BRANCH;                                               00030200
PROCEDURE      PROCESS;                                                 00030300
          BEGIN                                                         00030400
LABEL          NEXTCB,TESTTYC,NEXTQU,TESTWA,NEXTWB,TESTTYW,EXIT,UN1,    00030500
               TMC,TMW,MISS99,UNA,LSEARCH,NEXTREAD;                     00030600
LABEL          RD,QU,TY,CA,CB,WA,WB,UN,BR,LA,L99,PEX;                   00030700
LABEL          BRK;                                                     00030705
SWITCH      OP~RD,QU,TY,CA,CB,WA,WB,UN,BR,LA,L99;                       00030800
LABEL          P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,      00030900
               P16,P17;                                                 00031000
SWITCH      SW~P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,      00031100
               P16,P17;                                                 00031200
               ACTIVE~MOREPROCESS[USER]~FALSE;                          00031300
               IF PLACE[USER]>9 THEN                                    00031400
          BEGIN                                                         00031500
               READ(COURSE[USER][N[USER]],10,A[*])[MISS99];             00031600
               MOVEOP(A,OPCODE);                                        00031700
          END;                                                          00031800
               GO TO SW[PLACE[USER]];                                   00031900
P1:                                                                     00032000
               WRITE(TOUT(STA[USER]),TCH)[:BRK];                        00032100
               ACTIVE~MOREPROCESS[USER]~TRUE;                           00032200
               PLACE[USER]~2; GO TO EXIT;                               00032300
P2:                                                                     00032400
               WRITE(TOUT(STA[USER]),TPNAME)[:BRK];                     00032500
               PLACE[USER]~3; GO TO EXIT;                               00032600
P3:                                                                     00032700
               IF EQ(DICT[0],STU[1],4) THEN GO TO PEX;               %0500032705
               ID1~"      ";                                            00032800
               I~FINDGP(STU[1]);                                        00032900
               IF I>7 THEN I~7;                                         00033000
               MOVE(STU[1],ID1,I);                                      00033100
               FILL COURSE[USER] WITH ID1,*;                            00033200
               SEARCH(COURSE[USER],A[*]);                               00033300
               IF A[0]<2 THEN                                           00033400
          BEGIN                                                         00033500
               WRITE(TOUT(STA[USER]),FBADNAME,ID1.[6:6],ID1)[:BRK];     00033600
               PLACE[USER]~3; GO TO EXIT;                               00033700
          END;                                                          00033800
               N[USER]~-1;                                              00033900
               NEXTLINE;                                                00034000
               MATCH~FALSE;                                             00034100
               RDFLAG[USER]~FALSE;                                      00034200
P9:                                                                     00034300
LSEARCH:                                                                00034400
               FOR I~1 STEP 1 UNTIL 11 DO IF OPCODE=OPS[I] THEN         00034500
               GO TO OP[I];                                             00034600
QU:            FOR I~0 STEP 1 UNTIL 19 DO U[USER,I]~FALSE;              00034700
               TYPE;                                                    00034800
               PLACE[USER]~10;GO TO EXIT;                               00034900
P10:                                                                    00035000
               IF OPCODE!"CA" THEN                                      00035100
          BEGIN                                                         00035200
               WRITE(TOUT(STA[USER]),FNOCA)[:BRK];                      00035300
               ACTIVE~MOREPROCESS[USER]~TRUE;                           00035400
               PLACE[USER]~13;GO TO EXIT;                               00035500
          END;                                                          00035600
               NCA[USER]~N[USER]; COMMENT SAVES LOC OF CA FOR LOOPING;  00035700
               PLACE[USER]~11;GO TO EXIT;                               00035800
P11:                                                                    00035900
NEXTREAD:                                                               00036000
               IF EQ(DICT[0],STU[1],4) THEN                             00036100
          BEGIN                                                         00036200
               WRITE(TOUT(STA[USER]),FQUIT)[:BRK];                      00036300
               ACTIVE~MOREPROCESS[USER]~TRUE;                           00036400
               PLACE[USER]~6; GO TO EXIT;                               00036500
          END;                                                          00036600
               IF EQ(DICT[2],STU[1],12) THEN                         %0200036605
          BEGIN                                                      %0200036610
               M~0;                                                  %0200036615
               FOR I~1 STEP 1 UNTIL UMAX DO IF INUSE[I] THEN M~M+1;  %0200036620
               WRITE(TOUT(STA[USER]),FUSERS,M,USER)[:BRK];           %0200036625
               FOR I~1 STEP 1 UNTIL UMAX DO                          %0200036630
                 IF INUSE[I] THEN WRITE(TOUT(STA[USER]),FLINES,I)[:BRK];00 36635
               N[USER]~NCA[USER];                                    %0200036640
               PLACE[USER]~11; GO TO EXIT;                           %0200036645
          END;                                                       %0200036650
               IF RDFLAG[USER] THEN                                     00036700
          BEGIN                                                         00036800
               RDFLAG[USER]~FALSE;                                      00036900
               GO TO LSEARCH;                                           00037000
          END;                                                          00037100
               IF EQ(DICT[1],STU[1],4) THEN                             00037200
          BEGIN                                                         00037300
               WRITE(TOUT(STA[USER]),FCA)[:BRK];                        00037400
               ACTIVE~MOREPROCESS[USER]~TRUE;                           00037500
               PLACE[USER]~4; GO TO EXIT;                               00037600
P4:                                                                     00037700
               N[USER]~NCA[USER];                                       00037800
               READ(COURSE[USER][N[USER]],10,A[*])[MISS99];             00037900
               TYPE;                                                    00038000
               PLACE[USER]~5; GO TO EXIT;                               00038100
P5:                                                                     00038200
               WRITE(TOUT(STA[USER]),FNQU)[:BRK];                       0003830 
               N[USER]~NCA[USER];                                       00038400
               PLACE[USER]~11;GO TO EXIT;                               00038500
          END;                                                          00038600
               TESTMATCH;                                               00039700
               IF MATCH THEN GO TO TESTTYC;                             00039800
NEXTCB:                                                                 00039900
               NEXTLINE;                                                00040000
               IF OPCODE="CB" OR OPCODE="CA" THEN                       00040100
          BEGIN                                                         00040200
TMC:           TESTMATCH;                                               00040300
               IF MATCH THEN GO TO TESTTYC;                             00040400
               NEXTLINE;                                                00040500
               IF OPCODE="CA" OR OPCODE="CB" THEN GO TO TMC;            00040600
          END;                                                          00040700
               IF OPCODE="TY" OR OPCODE="BR" OR OPCODE="  "             00040800
                 THEN GO TO NEXTCB;                                     00040900
               GO TO TESTWA;                                            00041000
TESTTYC:                                                                00041100
               NEXTLINE;                                                00041200
               IF OPCODE="CB" THEN GO TO TESTTYC;                       00041300
               IF OPCODE="BR" THEN CALLBRANCH;                          00041400
               IF OPCODE="TY" THEN BEGIN TYPE;                          00041500
               PLACE[USER]~12;GO TO EXIT; END;                          00041600
P12:                                                                    00041700
               IF OPCODE="BR" THEN CALLBRANCH;                          00041800
P13:                                                                    00041900
NEXTQU:                                                                 00042000
               IF OPCODE="RD" THEN GO TO RD;                            00042100
               IF OPCODE="QU" THEN GO TO QU;                            00042200
               IF OPCODE="99" THEN GO TO L99;                           00042300
               NEXTLINE;                                                00042400
               GO TO NEXTQU;                                            00042500
TESTWA:        IF OPCODE="WA" THEN GO TO WA;                            00042600
               IF OPCODE="UN" THEN GO TO UN;                            00042700
               NEXTLINE;                                                00042800
               GO TO TESTWA;                                            00042900
WA:            TESTMATCH;                                               00043000
               IF MATCH THEN GO TO TESTTYW;                             00043100
NEXTWB:                                                                 00043200
               NEXTLINE;                                                00043300
               IF OPCODE="WB" OR OPCODE="WA" THEN                       00043400
          BEGIN                                                         00043500
TMW:           TESTMATCH;                                               00043600
               IF MATCH THEN GO TO TESTTYW;                             00043700
               NEXTLINE;                                                00043800
               IF OPCODE="WB" OR OPCODE="WA" THEN GO TO TMW;            00043900
          END;                                                          00044000
               IF OPCODE="TY" OR OPCODE="  " OR OPCODE="BR"             00044100
                 THEN GO TO NEXTWB;                                     00044200
               IF OPCODE="UN" THEN GO TO UN;                            00044300
               WRITE(TOUT(STA[USER]),FNEXTQU)[:BRK];                    00044400
               ACTIVE~MOREPROCESS[USER]~TRUE;                           00044500
               PLACE[USER]~13;GO TO EXIT;                               00044600
TESTTYW:                                                                00044700
               NEXTLINE;                                                00044800
               IF OPCODE="WB" THEN GO TO TESTTYW;                       00044900
               IF OPCODE="BR" THEN CALLBRANCH;                          00045000
               IF OPCODE="TY" THEN BEGIN TYPE;                          00045100
               PLACE[USER]~14;GO TO EXIT; END;                          00045200
P14:                                                                    00045300
               IF OPCODE="BR" THEN CALLBRANCH;                          00045400
               N[USER]~NCA[USER];                                       00045500
                 COMMENT RESETS FOR NEXT TRY ON SAME QUESTION;          00045600
               PLACE[USER]~11;GO TO EXIT;                               00045700
UN:            M~0;                                                     00045800
UNA:           NUN~N[USER];                                             00045900
               IF NOT U[USER,M] THEN                                    00046000
          BEGIN                                                         00046100
               U[USER,M]~TRUE;                                          00046200
               TYPE;                                                    00046300
               PLACE[USER]~15;GO TO EXIT;                               00046400
P15:                                                                    00046500
               IF OPCODE="BR" THEN CALLBRANCH;                          00046600
               N[USER]~NCA[USER];                                       00046700
               PLACE[USER]~11;GO TO EXIT;                               00046800
          END;                                                          00046900
UN1:           M~M+1;                                                   00047000
               NEXTLINE;                                                00047100
               IF OPCODE="UN" THEN GO TO UNA;                           00047200
               IF OPCODE="  " THEN GO TO UN1;                           00047300
COMMENT        IF WE GET HERE WE HAVE PASSED LAST UN & WILL REPEAT IT.  00047400
               STUDENT CAN STILL GET OUT BY TYPING "HELP";              00047500
               N[USER]~NUN;                                             00047600
               READ(COURSE[USER][N[USER]],10,A[*])[MISS99];             00047700
               TYPE;                                                    00047800
               PLACE[USER]~16;GO TO EXIT;                               00047900
P16:                                                                    00048000
               N[USER]~NCA[USER];                                       00048100
               PLACE[USER]~11;GO TO EXIT;                               00048200
RD:            RDFLAG[USER]~TRUE;                                       00048300
               TYPE;                                                    00048400
               PLACE[USER]~17;GO TO EXIT;                               00048500
P17:                                                                    00048600
               NCA[USER]~N[USER];                                    %0200048605
               PLACE[USER]~11;GO TO EXIT;                               00048700
TY:CA:CB:WB:   WRITE(TOUT(STA[USER]),FBADOP)[:BRK];                     00048800
               ACTIVE~MOREPROCESS[USER]~TRUE;                           00048900
               PLACE[USER]~13;GO TO EXIT;                               00049000
BR:            CALLBRANCH;                                              00049100
LA:            NEXTLINE;                                                00049200
               GO TO LSEARCH;                                           00049300
MISS99:        NO99;                                                    00049400
P6:                                                                     00049500
L99:                                                                    00049600
COMMENT        WANTNEW;                                                 00049700
               CLOSE(COURSE[USER],RELEASE);                             00049800
               WRITE(TOUT(STA[USER]),FLEAVING)[:BRK];                   000499  
               ACTIVE~MOREPROCESS[USER]~TRUE;                           00050000
               PLACE[USER]~7; GO TO EXIT;                               00050100
P7:                                                                     00050200
               WRITE(TOUT(STA[USER]),FNEWCOURSE)[:BRK];                 00050300
               ACTIVE~MOREPROCESS[USER]~FALSE;                          00050400
               PLACE[USER]~8; GO TO EXIT;                               00050500
BRK:           BREAK;                                                   00050500
P8:                                                                     00050600
               IF EQ(DICT[5],STU[1],3) THEN GO TO P2;                   00050700
               IF EQ(DICT[6],STU[1],2) THEN                             00050800
          BEGIN                                                         00050900
PEX:                                                                    00050905
               WRITE(TOUT(STA[USER]),FEXIT)[:BRK];                      00 5 0  
               UNATTATCH(USER);                                      %0400051100
               GO TO EXIT;                                              00051600
          END;                                                          00051700
               WRITE(TOUT(STA[USER]),FYN)[:BRK];                        00051800
               PLACE[USER]~8;                                           00051900
EXIT:     END  OF PROCESS;                                              00052000
               FILL OPS[*] WITH 0,"RD","QU","TY","CA","CB","WA","WB",   00052100
                                  "UN","BR","LA","99";                  00052200
               FILLTHE(DICT);                                           00052300
               ANYUSER~TRUE;                                            00052400
               GO TO ANYMORE;                                           00052500
BRK:           UNATTATCH(USER);                                         00052505
ANYMORE:       WHILE ANYUSER DO                                         00052600
          BEGIN                                                         00052700
  LOOKACTIVE:  FOR USER~ 1 STEP 1 UNTIL UMAX DO                         00053000
               IF INUSE[USER] AND ACT[USER] THEN                        00053100
          BEGIN IF STATUS(STA[USER],1).[28:3]=0 THEN                    00053200
           BEGIN                                                        00053300
               IF MOREPROCESS[USER] THEN GO TO PROC;                    00053400
               IF MORETYPE[USER] THEN                                   00053500
            BEGIN                                                       00053600
               READ(COURSE[USER][N[USER]],10,A[*]);                     00053700
               TYPE;                                                    00053800
               GO TO ENDSTA;                                            00053900
            END;                                                        00054000
               READ(TTIN[USER](STA[USER],0),8,STU[*])[ENDSTA:ENDSTA];   00054100
               WRITE(TOUT(STA[USER]),F4)[:BRK];                         00054200
     PROC:     PROCESS;                                                 00054300
   ENDSTA: END;                                                         00054400
          END;                                                          00054500
   STATS:      TEMP~STATUS(USERS[*]);                                   00054600
               IF NUSE < TEMP THEN                                      00054700
          BEGIN                                                         00054800
               FOR I~0 STEP 2 UNTIL TEMP-1 DO IF USERS[I+1]!-1 THEN     00054900
          BEGIN                                                         00055000
               FOR USER~1 STEP 1 UNTIL UMAX DO                          00055100
                 IF USERS[I].[9:9]=STA[USER].[9:9] THEN GO TO FOUNDIT;  00055200
               FOR USER~1 STEP 1 UNTIL UMAX DO                          00055300
                 IF STA[USER]=0 THEN                                    00055400
           BEGIN                                                        00055500
               INUSE[USER]~TRUE;                                        00055600
               STA[USER]~0&USERS[I][9:9:9];                             00055605
               PLACE[USER]~1;                                           00055610
               NUSE~NUSE+2;                                             00055615
               GO TO PRC;                                               00055620
           END;                                                         00055625
  FOUNDIT:END;                                                          00055630
          END;                                                          00055635
               READ(TTIN[0](0,0.5),8,STU[*])[LOOKACTIVE];               00055640
               IF NOT WHOISIT(STU[0],USER) THEN GO TO STATS;            00055645
               WRITE(TOUT(STA[USER]),F4)[:BRK];                         00055650
  PRC:         PROCESS;                                                 00055700
          END  OF WHILE ANYUSER LOOP;                                   00055800
          END.                                                          00055900
