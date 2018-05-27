          BEGIN                                                         00000100
COMMENT        LOUIS A. WITKIN, FLEXIBLE TELETYPE PROGRAM FOR MATRIX    00000200
               OPERATIONS.  UNIVERSITY OF DENVER, 2/23/68;              00000300
ALPHA FILE IN  TTIN 14 (1,8);                                           00000400
ALPHA FILE OUT TTOUT 14 (1,8);                                          00000500
INTEGER        STA,GP,DUMB,NEXTSTOP,CPACE,SKIPOVER,I,OLDDUMB;           00000600
INTEGER        J,K;                                                     00000700
ARRAY          MATRIX[0:9,0:10,0:10],VECTOR[0:9,0:10],CONST[0:9],MESSAGE00000800
                  [0:33],OPCODE[0:11],TYPE[0:3],NUM[0:3],POLY[0:9,0:20];00000900
ALPHA ARRAY    RD[0:7];                                                 00001000
ALPHA          QUEST,LA,VERB,BLNK,EQ,LP,RP,COMA,PL,MI,TI,OPER,DI;       00001100
FORMAT         FO1(X8,"{!MATROP/TTY, VERSION 3.{!IF YOU NEED HELP, TY~",00001200
                    /X8,"PE ",""","TEACH",""",".{!!~"),                 00001300
               FO2(X8,"{!!PLEASE READ THE INSTRUCTION MANUAL.{!!~"),    00001400
               FO3(X8,"{!GOODBYE.{!!~"),                                00001500
               MNULL(X8,"{!M",A1," (",I*,",",I*,") IS NULL.{!~"),       00001600
               VNULL(X8,"{!V",A1," (",I*,") IS NULL.{!~"),              00001700
               PNULL(X8,"{!P",A1," (",I*,") IS NULL.{!~"),              00001800
               MSIZE(X8,"{!M",A1," IS (",I*,",",I*,").{!!~"),           00001900
               VSIZE(X8,"{!V",A1," IS (",I*,").{!!~"),                  00002000
               PSIZE(X8,"{!P",A1," IS (",I*,").{!!~"),                  00002100
TEACH     (X8,"{!THE FOLLOWING COMPUTATIONAL AREAS ARE AVAILABLE:~",    00002200
          /X8,"{!     1.  10 MATRICES, 10 BY 10 ELEMENTS IN EACH.~",    00002300
          /X8,"{!     2.  10 VECTORS, 10 ELEMENTS IN EACH.~",           00002400
          /X8,"{!     3.  10 CONSTANTS.~",                              00002500
          /X8,"{!     4.  10 POLYNOMIALS, 19TH DEGREE IN EACH.~",       00002600
          /X8,"{!THESE AREAS MAY BE REFERENCED AS M*, V*, C*, OR P*,~", 00002700
          /X8,"{!     WHERE * IS A NUMBER FROM 0 TO 9.~",               00002800
          /X8,"{!FOR A LIST OF COMMANDS, TYPE ",""","COMMANDS",""",".~",00002900
          /X8,"{!FOR A LIST OF OPERATIONS, TYPE ",""","OPERATIONS",""", 00003000
               ".~",                                                    00003100
          /X8,"{!FOR A LIST OF ERROR MESSAGES, TYPE ",""","ERRORS",""", 00003200
               ".~",                                                    00003300
          /X8"{!!IF ANY COMMAND IS INCORRECT, THE COMPUTER WILL TYPE ~",00003400
               /X8,"AN ERROR MESSAGE.{!!~"),                            00003500
COMMANDS  (X8,"{!THE FOLLOWING ACTION VERBS ARE AVAILABLE:~",           00003600
          /X8,"{!     1.  ",""","STOP","""," OR ",""","QUIT",""","--EN",00003700
               "D COMPUTATION.~",                                       00003800
          /X8,"{!     2.  ",""","INPUT","""," OR ",""","READ",""","--E",00003900
               "NTER DATA WHEN THE~",                                   00004000
          /X8,"{!         COMPUTER TYPES A QUESTION MARK.  EXAMPLES:~", 00004100
          /X8,"{!INPUT M3 (4,8)~",                                      00004200
          /X8,"{!--4 REPRESENTS THE NUMBER OF ROWS,~",                  00004300
          /X8,"{!     8 THE NUMBER OF COLUMNS--~",                      00004400
          /X8,"{!INPUT V7 (5)~",                                        00004500
          /X8,"{!--5 REPRESENTS THE NUMBER OF ELEMENTS--~",             00004600
          /X8,"{!INPUT C4~",                                            00004700
          /X8,"{!INPUT P2 (7)~",                                        00004800
          /X8,"{!--7 REPRESENTS THE DEGREE OF THE POLYNOMIAL--~",       00004900
          /X8,"{!ENTER THE COEFFICIENT OF THE LOWEST POWER FIRST.~",    00005000
          /X8,"{!     3.  ",""","WRITE","""," OR ",""","PRINT",""","-~",00005100
               /X8,"-TYPE STORED DATA.  EXAMPLES:~",                    00005200
          /X8,"{!WRITE M1~",                                            00005300
          /X8,"{!WRITE V5~",                                            00005400
          /X8,"{!WRITE C2~",                                            00005500
          /X8,"{!WRITE P3~",                                            00005600
          /X8,"{!     4.  ",""","SET","""," OR ",""","LET",""","--EXE~",00005700
               /X8,"CUTE ARITHMETIC OPERATIONS.~",                      00005800
          /X8,"{!     5.  ",""","SIZE",""","--TYPES THE SIZE OF A MAT~",00005900
               /X8,"RIX OR VECTOR~",                                    00006000
            /X8,"{!         OR THE DEGREE OF A POLYNOMIAL.  EXAMPLES:~",00006100
          /X8,"{!SIZE M3~",                                             00006200
          /X8,"{!SIZE V5~",                                             00006300
          /X8,"{!SIZE P2~",                                             00006400
          /X8,"{!     6.  ",""","SOLVE",""","--SOLVES SIMULTANEOUS EQ~",00006500
               /X8,"UATIONS.  EXAMPLE:~",                               00006600
          /X8,"{!SOLVE M1*V3=V8~",                                      00006700
          /X8,"{!--M1 IS THE COEFFICIENT MATRIX--~",                    00006800
          /X8,"{!--V3 IS THE SOLUTION VECTOR--~",                       00006900
          /X8,"{!--V8 IS THE RIGHT-HAND-SIDE VECTOR--~",                00007000
          /X8,"{!     7.  ",""","ROOTS",""","--FINDS THE ROOTS OF A PO",00007100
               "LYNOMIAL AND~",                                         00007200
               /X8,"{!         TYPES THE SOLUTION.  EXAMPLE:~",         00007300
          /X8,"{!ROOTS P3~",                                            00007400
          /X8,"{!WHEN ",""","SET",""",", ",""","SOLVE","""," AND ",""", 00007500
               "ROOTS","""," ARE EXECUTED,{!~",                         00007600
            /X8,"     THE COMPUTER WILL TYPE ",""","DONE.",""",".{!!~"),00007700
OPERATIONS (X8,"{!THERE ARE ELEVEN POSSIBLE OPERATIONS.  EXAMPLES:~",   00007800
          /X8,"{! 1.  ADDITION:~",                                      00007900
          /X8,"{!    SET M1=M5+M6~",                                    00008000
          /X8,"{!    SET V3=V1+V7~",                                    00008100
          /X8,"{!    SET C1=C3+C4~",                                    00008200
          /X8,"{!    SET P5=P3+P4~",                                    00008300
          /X8,"{! 2.  SUBTRACTION:~",                                   00008400
          /X8,"{!    SET M3=M1-M7~",                                    00008500
          /X8,"{!    SET V1=V3-V4~",                                    00008600
          /X8,"{!    SET C1=C5-C6~",                                    00008700
          /X8,"{!    SET P7=P2-P1~",                                    00008800
          /X8,"{! 3.  MULTIPLICATION:~",                                00008900
          /X8,"{!    SET M1=M2*M3~",                                    00009000
          /X8,"{!    SET C1=V1*V2  --DOT PRODUCT--~",                   00009100
          /X8,"{!    SET P6=P4*P2~",                                    00009200
          /X8,"{!    SET V5=M3*V3~",                                    00009300
          /X8,"{!    SET C9=C0*C1~",                                    00009400
          /X8,"{!    SET M1=C2*M2  --EVERY ELEMENT*CONSTANT--~",        00009500
          /X8,"{!    SET V1=C3*V3  --EVERY ELEMENT*CONSTANT--~",        00009600
          /X8,"{!    SET P5=C4*P2  --EVERY ELEMENT*CONSTANT--~",        00009700
          /X8,"{! 4.  DIVISION:~",                                      00009800
          /X8,"{!    SET C7=C5/C2~",                                    00009900
          /X8,"{! 5.  DETERMINANT:~",                                   00010000
          /X8,"{!    SET C1=DET(M1)~",                                  00010100
          /X8,"{! 6.  TRANSPOSE:~",                                     00010200
          /X8,"{!    SET M2=TRN(M1)~",                                  00010300
          /X8,"{! 7.  EQUATE:~",                                        00010400
          /X8,"{!    SET M1=M5~",                                       00010500
          /X8,"{!    SET V3=V7~",                                       00010600
          /X8,"{!    SET C2=C3~",                                       00010700
          /X8,"{!    SET P5=P3~",                                       00010800
          /X8,"{!    SET M3 (4,8)= 3.14159  --A PARTICULAR ELEMENT--~", 00010900
          /X8,"{!    SET V7 (5)= 2.71828  --A PARTICULAR ELEMENT--~",   00011000
          /X8,"{!    SET P2 (7)= 5  --A PARTICULAR ELEMENT--~",         00011100
          /X8,"{!    SET C4= 1000000000001.0~",                         00011200
          /X8,"{! 8.  IDENTITY:~",                                      00011300
          /X8,"{!    SET M3 (4,4)=IDN~",                                00011400
          /X8,"{! 9.  INVERSE:~",                                       00011500
          /X8,"{!    SET M1=INV(M0)~",                                  00011600
          /X8,"{!10.  NULLIFY:~",                                       00011700
          /X8,"{!    SET M3 (4,8)=NULL~",                               00011800
          /X8,"{!    SET V5 (5)=NULL~",                                 00011900
          /X8,"{!    SET C9=NULL~",                                     00012000
          /X8,"{!    SET P3 (15)=NULL~",                                00012100
          /X8,"{!11.  EIGEN VECTORS:~",                                 00012200
          /X8,"{!    SET M3=EVEC(M2){!!~"),                             00012300
ERRORS    (X8,"{!THE ERROR MESSAGES ARE AS FOLLOWS:~",                  00012400
          /X8,"{! 1.  ",""","INVALID VERB",""","--THE USER HAS TYPED A",00012500
               " VERB THAT~",                                           00012600
               /X8,"{!     THE PROGRAM DOES NOT RECOGNIZE.~",           00012700
          /X8,"{! 2.  ",""","INVALID IDENTIFIER",""","--THE USER HAS T",00012800
               "YPED AN IDE~",                                          00012900
              /X8,"NTIFIER{!     THAT THE PROGRAM DOES NOT RECOGNIZE.~",00013000
          /X8,"{! 3.  ",""","MISSING -(-",""","--THE USER HAS NOT TYPE",00013100
               "D A LEFT-~",                                            00013200
               /X8,"PARENTHESIS.~",                                     00013300
          /X8,"{! 4.  ",""","MISSING -,-",""","--THE USER HAS NOT TYPE",00013400
               "D A COMMA.~",                                           00013500
          /X8,"{! 5.  ",""","MISSING -)-",""","--THE USER HAS NOT TYPE",00013600
               "D A RIGHT-~",                                           00013700
               /X8,"PARENTHESIS.~",                                     00013800
          /X8,"{! 6.  ",""","INVALID OPERATION",""","--THE USER HAS TY",00013900
               "PED AN~",                                               00014000
               /X8,"{!     OPERATION THAT THE PROGRAM DOES NOT RECOGNI",00014100
               "ZE.~",                                                  00014200
          /X8,"{! 7.  ",""","MISSING -=-",""","--THE USER HAS NOT TYPE",00014300
               "D AN EQUAL~",                                           00014400
               /X8," SIGN.~",                                           00014500
          /X8,"{! 8.  ",""","INVALID INDEX",""","--THE USER HAS TYPED ",00014600
               "AN AREA SIZE~",                                         00014700
               /X8,"{!     THAT THE PROGRAM CAN NOT HANDLE.~",          00014800
          /X8,"{! 9.  ",""","INVALID SIZE",""","--THE USER ATTEMPTED A",00014900
               "N OPERATION~",                                          00015000
               /X8,"{!     WITH IDENTIFIERS OF THE WRONG SIZES.~",      00015100
          /X8,"{!10.  ",""","INDEX OVERFLOW OF",""","--THE USER HAS TY",00015200
               "PED TOO MANY~",                                         00015300
               /X8," VALUES.~",                                         00015400
          /X8,"{!11.  ",""","IDENT. INVALID FOR OPER.",""","--THE USER",00015500
               " HAS ATTEMPT~",                                         00015600
              /X8,"ED{!     AN OPERATION THAT THE PROGRAM CAN NOT DO U",00015700
               "SING~",                                                 00015800
               /X8," THE IDENTIFIERS{!     THAT HE TYPED.~",            00015900
          /X8,"{!12.  ",""","MISSING -*-",""","--THE USER HAS NOT TYPE",00016000
               "D A MULTIPLI~",                                         00016100
              /X8,"CATION SIGN.~",                                      00016200
          /X8,"{!13.  ",""","DIVISION BY ZERO",""","--THE USER HAS ATT",00016300
               "EMPTED AN IN~",                                         00016400
              /X8,"VALID DIVISION.~",                                   00016500
          /X8,"{!!DATA FOLLOWING MESSAGES:~",                           00016600
          /X8,"{!FOR 1,2,6,8 THE PROGRAM TYPES THE INVALID CONSTRUCT.~",00016700
          /X8,"{!FOR 3,4,5,7,12 THE PROGRAM TYPES THE CHARACTERS{!   ~",00016800
               /X8,"  WHERE SCANNING STOPPED.~",                        00016900
          /X8,"{!FOR 9,11,13 THE PROGRAM TYPES THE IDENTIFIERS{!     ~",00017000
               /X8,"WHICH CAUSED THE ERROR.~",                          00017100
          /X8,"{!FOR 10 THE PROGRAM TYPES THE NUMBER OF EXCESSIVE VAL~",00017200
               /X8,"UES.{!!~"),                                         00017300
               DONE(X8,"{!DONE.{!~"),                                   00017400
               WC(X8,"{!C",I1,"=",V10.3,"{!!~"),                        00017500
               RC (X8,"{!C",I1,"=",A1,"{!~"),                           00017600
               ERR(X8,"{!",X*,  "+---{!~"/X8,"ERR # ",A3," - ",*A*,     00017700
              " ..",*V*,"..{!~");                                       00017800
LABEL          REED,STOP,PRINTER,INPUT,LET,RERR,BRK,TESTOPER,DUMREED,   00017900
                    SOLVER,ROOTS;                                       00018000
COMMENT        REMOTE UNIT INPUT PROCEDURE PACKAGE                      00018100
-----------------------------------------------------------------------;00018200
          PROCEDURE STATION(STA);                                       00018300
            INTEGER  STA;                                               00018400
              BEGIN                                                     00018500
                ARRAY  RD1[0:7];                                        00018600
                  STA:=STATUS(RD1[*]);                                  00018700
                  STA.[9:9]:=RD1[0].[9:9];                              00018800
              END OF PROCEDURE STATION;                                 00018900
          INTEGER STREAM PROCEDURE FINDCH(B,CH);                        00019000
            VALUE  CH;                                                  00019100
              BEGIN                                                     00019200
                  DI:=LOC CH; DI:=DI+7; SI:=B;                          00019300
                  63(IF SC=DC THEN JUMP OUT; DI:=DI-1; TALLY:=TALLY+1); 00019400
                  FINDCH:=TALLY;                                        00019500
              END OF PROCEDURE FINDCH;                                  00019600
          BOOLEAN STREAM PROCEDURE ANYFUNNYCHRS(B,NCH);                 00019700
            VALUE  NCH;                                                 00019800
              BEGIN                                                     00019900
                LABEL  OK;                                              00020000
                  SI:=B; TALLY:=0; NCH(IF SC=ALPHA THEN BEGIN IF SC}"0" 00020100
                  THEN GO TO OK; END; IF SC="." THEN GO TO OK; IF SC=","00020200
                  THEN GO TO OK; IF SC="+" THEN GO TO OK; IF SC="-" THEN00020300
                  GO TO OK; IF SC="@" THEN GO TO OK; TALLY:=1; JUMP OUT;00020400
                  OK: SI:=SI+1);                                        00020500
                  ANYFUNNYCHRS:=TALLY                                   00020600
              END OF PROCEDURE ANYFUNNYCHRS;                            00020700
          BOOLEAN STREAM PROCEDURE LASTCOMMA(B,NCH);                    00020800
            VALUE  NCH;                                                 00020900
              BEGIN                                                     00021000
                SI:=B; SI:=SI+NCH; IF SC="," THEN TALLY:=1;             00021100
                LASTCOMMA:=TALLY                                        00021200
              END OF PROCEDURE LASTCOMMA;                               00021300
          STREAM PROCEDURE PUTCHRIN(B,NCH,CH);                          00021400
            VALUE  NCH,CH;                                              00021500
              BEGIN                                                     00021600
                DI:=B; SI:=LOC CH;  SI:=SI+7; DI:=DI+NCH; DS:=1 CHR     00021700
              END OF PROCEDURE PUTCHRIN;                                00021800
          INTEGER STREAM PROCEDURE COUNTCOMMAS(B,NCH);                  00021900
            VALUE  NCH;                                                 00022000
              BEGIN                                                     00022100
                SI:=B; NCH(IF SC="," THEN TALLY:=TALLY+1; SI:=SI+1);    00022200
                COUNTCOMMAS:=TALLY                                      00022300
              END OF PROCEDURE COUNTCOMMAS;                             00022400
          STREAM PROCEDURE MOVER(B,T,N);                                00022500
            VALUE  N;                                                   00022600
              BEGIN                                                     00022700
                SI:=B; DI:=T; DS:=N CHR; DS:=6 LIT " "                  00022800
              END OF PROCEDURE MOVER;                                   00022900
          STREAM PROCEDURE SHIFT(A,S,N);                                00023000
            VALUE  S,N;                                                 00023100
              BEGIN                                                     00023200
                SI:=LOC S; SI:=SI+7; IF SC!"0" THEN                     00023300
                BEGIN                                                   00023400
                  SI:=A; SI:=SI+S; DI:=A; DS:=N CHR                     00023500
                END                                                     00023600
              END OF PROCEDURE SHIFT;                                   00023700
          BOOLEAN STREAM PROCEDURE DECIMAL(B,NCH);                      00023800
            VALUE  NCH;                                                 00023900
              BEGIN                                                     00024000
                SI:=B; TALLY:=0; NCH(IF SC="." THEN BEGIN TALLY:=1; JUMP00024100
                OUT; END; SI:=SI+1); DECIMAL:=TALLY                     00024200
              END OF PROCEDURE DECIMAL;                                 00024300
          STREAM PROCEDURE MOVE(B,T,N);                                 00024400
            VALUE  N;                                                   00024500
              BEGIN                                                     00024600
                SI:=B; DI:=T; DS:=N CHR; DS:=20 LIT "0"                 00024700
              END OF PROCEDURE MOVE;                                    00024800
          PROCEDURE READVAL(V,STA);                                     00024900
            VALUE  STA; INTEGER  STA; REAL  V;                          00025000
              BEGIN                                                     00025100
                INTEGER  GP;                                            00025200
                FORMAT  FSO(X8,"{!TYPE VALUE AGAIN.{!~");               00025300
                LABEL  RER,XIT,TRYAGIN;                                 00025400
                ARRAY  RD1[0:7];                                        00025500
                ALPHA  LA,COMA;                                         00025600
                  LA:=31; COMA:=58;                                     00025700
          TRYAGIN:READ(TTIN(STA),8,RD1[*]);                             00025800
                  GP:=FINDCH(RD1[1],LA);                                00025900
                  IF ANYFUNNYCHRS(RD1[1],GP) THEN GO TO RER;            00026000
                  PUTCHRIN(RD1[1],GP,COMA); GP:=GP+1;                   00026100
                  MOVER(RD1[1],RD1[0],GP);                              00026200
                  READ(RD1[*],/,V)[:RER];  GO TO XIT;                   00026300
          RER:    WRITE(TTOUT(STA),FSO);  GO TO TRYAGIN;                00026400
          XIT:END OF PROCEDURE READVAL;                                 00026500
          PROCEDURE READROW(N,A,STA);                                   00026600
            VALUE  N,STA; INTEGER  N,STA; ARRAY  A[0];                  00026700
              BEGIN                                                     00026800
                INTEGER GP,I,J,CCM;                                     00026900
                ALPHA  LA,COMA;                                         00027000
                LABEL  MORE,XIT,R;                                      00027100
                FORMAT  F1(X8,"{!TYPING ERROR.  VALID CHARACTERS ARE 0",00027200
                        "123456789+-,.@. ~"/X8," TYPE LINE AGAIN.{!~"), 00027300
                        F2(X8,"{!~");                                   00027400
                ARRAY  RD1[0:7];                                        00027500
                  J:=1; LA:=31; COMA:=58;                               00027600
          MORE:   READ(TTIN(STA),8,RD1[*]);                             00027700
                  GP:=FINDCH(RD1[1],LA);                                00027800
                  IF ANYFUNNYCHRS(RD1[1],GP) THEN GO TO R;              00027900
                  IF NOT LASTCOMMA(RD1[1],GP-1) THEN                    00028000
                  BEGIN                                                 00028100
                    PUTCHRIN(RD1[1],GP,COMA); GP:=GP+1                  00028200
                  END;                                                  00028300
                  CCM:=COUNTCOMMAS(RD1[1],GP)-1;                        00028400
               IF J+CCM>N THEN                                          00028500
          BEGIN                                                         00028600
               I:=0;                                                    00028700
               I.[36:6]:=1;                                             00028800
               WRITE (TTOUT(STA),ERR,0,I,3,6,MESSAGE[22],MESSAGE[23],   00028900
                    MESSAGE[24],1,"I",2,J+CCM-N)[:XIT];                 00029000
               GO TO MORE;                                              00029100
          END;                                                          00029200
                  MOVER(RD1[1],RD1[0],GP);                              00029300
                  READ(RD1[*],/,FOR I:=J STEP 1 UNTIL J+CCM DO A[I])[:R]00029400
                ; J:=J+CCM+1;                                           00029500
                  WRITE(TTOUT(STA),F2);                                 00029600
                  IF J{N THEN GO TO MORE ELSE GO TO XIT;                00029700
          R:      WRITE(TTOUT(STA),F1);  GO TO MORE;                    00029800
          XIT:END OF PROCEDURE READROW;                                 00029900
          PROCEDURE PRINT(RD1);                                         00030000
            ARRAY  RD1[0];                                              00030100
              BEGIN                                                     00030200
                ALPHA ARRAY  RD2[0:8];                                  00030300
                ALPHA  LA;                                              00030400
                INTEGER  I,J;                                           00030500
                LABEL  MORE;                                            00030600
                  LA:=31;                                               00030700
          MORE:   IF FINDCH(RD1[1],LA)+1>56 THEN                        00030800
              BEGIN                                                     00030900
                  MOVER(RD1[0],RD2[0],63);                              00031000
                  PUTCHRIN(RD2[7],7,LA);                                00031100
                  FOR I:=1 STEP 7 WHILE J:=FINDCH(RD1[I+7],LA)+1>56 DO  00031200
                  SHIFT(RD1[I],55,56);                                  00031300
                  SHIFT(RD1[I],55,J+1);                                 00031400
                  WRITE (TTOUT(STA),8,RD2[*]);                          00031500
                  GO TO MORE;                                           00031600
              END;                                                      00031700
                  WRITE (TTOUT(STA),8,RD1[*]);                          00031800
              END OF PROCEDURE PRINT;                                   00031900
COMMENT----------------------------------------------------------------;00032000
PROCEDURE      SKIPBLANKS;                                              00032100
          BEGIN                                                         00032200
LABEL          MORESPACE;                                               00032300
MORESPACE:     IF FINDCH(RD[0],BLNK)=0 THEN                             00032400
          BEGIN                                                         00032500
               SHIFT(RD[0],1,GP:=GP-1);                                 00032600
               SKIPOVER:=SKIPOVER+1;                                    00032700
               GO TO MORESPACE;                                         00032800
          END;                                                          00032900
          END  OF PROCEDURE SKIPBLANKS;                                 00033000
BOOLEAN PROCEDURE DUMPTHISONE;                                          00033100
          BEGIN                                                         00033200
               DUMPTHISONE:=FALSE;                                      00033300
               IF DUMB=5 THEN                                           00033400
          BEGIN                                                         00033500
               WRITE (TTOUT(STA),FO2);                                  00033600
               DUMB:=6;                                                 00033700
          END                                                           00033800
               ELSE IF DUMB}7 THEN                                      00033900
               DUMPTHISONE:=TRUE;                                       00034000
          END  OF PROCEDURE DUMPTHISONE;                                00034100
COMMENT        PROCEDURES ARE INSERTED IN THIS SPACE                    00034200
***********************************************************************;00034300
          PROCEDURE MREAD(N);                                           00034400
            ARRAY N[0];                                                 00034500
              BEGIN                                                     00034600
                INTEGER K;                                              00034700
                  FOR K:=1 STEP 1 UNTIL N[2] DO                         00034800
                  READROW(N[3],MATRIX[N[1],K,*],STA);                   00034900
              END OF PROCEDURE MREAD;                                   00035000
          PROCEDURE VREAD(N);                                           00035100
            ARRAY N[0];                                                 00035200
                  READROW(N[2],VECTOR[N[1],*],STA);                     00035300
          PROCEDURE PREAD(N);                                           00035400
            ARRAY N[0];                                                 00035500
                  READROW(N[2],POLY[N[1],*],STA);                       00035600
          PROCEDURE READARRAY(V);                                       00035700
            REAL V;                                                     00035800
              BEGIN                                                     00035900
                INTEGER  GP;                                            00036000
                FORMAT  FI(R20.10);                                     00036100
                ALPHA  QUEST,DCIM;  ARRAY RD1[0:7];                     00036200
                  QUEST:=12; DCIM:=26;                                  00036300
                  GP:=FINDCH(RD[0],QUEST);                              00036400
                  IF NOT DECIMAL(RD[0],GP) THEN                         00036500
                  BEGIN                                                 00036600
                    PUTCHRIN(RD[0],GP,DCIM);  GP:=GP+1;                 00036700
                  END;  MOVE(RD[0],RD1[0],GP);                          00036800
                  READ(RD1[*],FI,V);                                    00036900
              END OF PROCEDURE READARRAY;                               00037000
PROCEDURE      MWRITE(NUM);                                             00037100
ARRAY          NUM[0];                                                  00037200
          BEGIN                                                         00037300
FORMAT         FM(X8,"{!M",A1," (",I*,",",I*,")={!~"),                  00037400
               F1(X8,"{!",*(V10.3,X2),"~"),                             00037500
               F2(X8,"{!ROW ",I2,":     ",5(V10.3,X2),"{!",X12,         00037600
                    *(V10.3,X2),"{!~");                                 00037700
ALPHA ARRAY    RD1[0:19];                                               00037800
INTEGER        I,J;                                                     00037900
ALPHA          LA;                                                      00038000
LABEL          MORE;                                                    00038100
               LA:=31;                                                  00038200
               WRITE (TTOUT(STA),FM,NUM[1],IF NUM[2]<10 THEN 1 ELSE 2,  00038300
                    NUM[2],IF NUM[3]<10 THEN 1 ELSE 2,NUM[3]);          00038400
               FOR I~1 STEP 1 UNTIL NUM[2] DO                           00038500
          BEGIN                                                         00038600
               IF NUM[3]{6 THEN                                         00038700
               WRITE (RD1[*],F1,NUM[3],FOR J~1 STEP 1 UNTIL NUM[3] DO   00038800
                    [IF MATRIX[NUM[1],I,J]<@-1 OR MATRIX[NUM[1],I,J]>   00038900
                    @5 THEN "E" ELSE "F",MATRIX[NUM[1],I,J]]) ELSE      00039000
               WRITE (RD1[*],F2,I,FOR J~1,2,3,4,5 DO [IF MATRIX[NUM[1], 00039100
                    I,J]<@-1 OR MATRIX[NUM[1],I,J]>@5 THEN "E" ELSE     00039200
                    "F",MATRIX[NUM[1],I,J]],NUM[3]-5,FOR J~6 STEP 1     00039300
                    UNTIL NUM[3] DO [IF MATRIX[NUM[1],I,J]<@-1 OR       00039400
                    MATRIX[NUM[1],I,J]>@5 THEN "E" ELSE "F",MATRIX      00039500
                    [NUM[1],I,J]]);                                     00039600
               PRINT(RD1);                                              00039700
          END;                                                          00039800
               WRITE (TTOUT(STA),<X8,"{!~">);                           00039900
          END  OF PROCEDURE MWRITE;                                     00040000
PROCEDURE      VWRITE(NUM);                                             00040100
ARRAY          NUM[0];                                                  00040200
          BEGIN                                                         00040300
FORMAT         FV(X8,"{!V",A1," (",I*,")={!~"),                         00040400
               F1(X8,"{!",*(V10.3,X2),"{!",*(V10.3,X2),"~");            00040500
ALPHA ARRAY    RD1[0:16];                                               00040600
INTEGER        I;                                                       00040700
ALPHA          LA;                                                      00040800
LABEL          MORE;                                                    00040900
               LA:=31;                                                  00041000
               WRITE (TTOUT(STA),FV,NUM[1],IF NUM[2]<10 THEN 1 ELSE 2,  00041100
                    NUM[2]);                                            00041200
               IF NUM[2]{6 THEN                                         00041300
               WRITE (RD1[*],F1,NUM[2],FOR I~1 STEP 1 UNTIL NUM[2] DO   00041400
                    [IF VECTOR[NUM[1],I]<@-1 OR VECTOR[NUM[1],I]>@5 THEN00041500
                    "E" ELSE "F",VECTOR[NUM[1],I]],-1) ELSE             00041600
               WRITE (RD1[*],F1,6,FOR I~1,2,3,4,5,6 DO [IF VECTOR       00041700
                [NUM[1],I]<@-1 OR VECTOR[NUM[1],I]>@5 THEN "E" ELSE "F",00041800
                    VECTOR[NUM[1],I]],NUM[2]-6,FOR I~7 STEP 1 UNTIL     00041900
                    NUM[2] DO [IF VECTOR[NUM[1],I]<@-1 OR VECTOR[NUM[1],00042000
                    I]>@5 THEN "E" ELSE "F",VECTOR[NUM[1],I]]);         00042100
               PRINT(RD1);                                              00042200
               WRITE (TTOUT(STA),<X8,"{!~">);                           00042300
          END  OF PROCEDURE VWRITE;                                     00042400
PROCEDURE      PWRITE(NUM);                                             00042500
ARRAY          NUM[0];                                                  00042600
          BEGIN                                                         00042700
FORMAT         FP(X8,"{!P",A1," (",I*,")={!~"),                         00042800
               F1(X8,"{!",*(V10.3,X2),"{!",*(V10.3,X2),"{!",*(V10.3,X2),00042900
                    "{!",*(V10.3,X2),"~");                              00043000
ALPHA ARRAY    RD1[0:32];                                               00043100
INTEGER        I;                                                       00043200
ALPHA          LA;                                                      00043300
LABEL          MORE;                                                    00043400
               LA:=31;                                                  00043500
               WRITE (TTOUT(STA),FP,NUM[1],IF NUM[2]-1<10 AND NUM[2]-1> 00043600
                    -1 THEN 1 ELSE 2,NUM[2]-1);                         00043700
               IF NUM[2]{6 THEN                                         00043800
               WRITE (RD1[*],F1,NUM[2],FOR I~1 STEP 1 UNTIL NUM[2] DO   00043900
                     [IF POLY[NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN   00044000
                    "E" ELSE "F",POLY[NUM[1],I]],-1,-1,-1) ELSE         00044100
               IF NUM[2]{12 THEN                                        00044200
               WRITE (RD1[*],F1,6,FOR I~1,2,3,4,5,6 DO [IF POLY[NUM[1], 00044300
                    I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE "F",POLY  00044400
                    [NUM[1],I]],NUM[2]-6,FOR I~7 STEP 1 UNTIL NUM[2] DO 00044500
                    [IF POLY[NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E"00044600
                    ELSE "F",POLY[NUM[1],I]],-1,-1) ELSE                00044700
               IF NUM[2]{18 THEN                                        00044800
               WRITE (RD1[*],F1,6,FOR I~1,2,3,4,5,6 DO [IF POLY[NUM[1], 00044900
                    I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE "F",POLY  00045000
                    [NUM[1],I]],6,FOR I~7,8,9,10,11,12 DO [IF POLY      00045100
                    [NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE   00045200
                    "F",POLY[NUM[1],I]],NUM[2]-12,FOR I~13 STEP 1 UNTIL 00045300
                    NUM[2] DO [IF POLY[NUM[1],I]<@-1 OR POLY[NUM[1],I]> 00045400
                    @5 THEN "E" ELSE "F",POLY[NUM[1],I]],-1) ELSE       00045500
               WRITE (RD1[*],F1,6,FOR I~1,2,3,4,5,6 DO [IF POLY[NUM[1], 00045600
                    I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE "F",POLY  00045700
                    [NUM[1],I]],6,FOR I~7,8,9,10,11,12 DO [IF POLY      00045800
                    [NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E" ELSE   00045900
                    "F",POLY[NUM[1],I]],6,FOR I~13,14,15,16,17,18 DO [IF00046000
                    POLY[NUM[1],I]<@-1 OR POLY[NUM[1],I]>@5 THEN "E"    00046100
                    ELSE "F",POLY[NUM[1],I]],NUM[2]-18,FOR I~19 STEP 1  00046200
                    UNTIL NUM[2] DO [IF POLY[NUM[1],I]<@-1 OR POLY      00046300
                    [NUM[1],I]>@5 THEN "E" ELSE "F",POLY[NUM[1],I]]);   00046400
               PRINT(RD1);                                              00046500
               WRITE (TTOUT(STA),<X8,"{!~">);                           00046600
          END  OF PROCEDURE PWRITE;                                     00046700
          PROCEDURE  MADD(A,B,C);                                       00046800
            ARRAY  A,B,C[0,0];                                          00046900
              COMMENT  C=A+B (11/67);                                   00047000
              BEGIN                                                     00047100
                INTEGER I,J;                                            00047200
                  C[1,0]:= A[1,0];  C[0,1]:= A[0,1];                    00047300
                  FOR I:=1 STEP 1 UNTIL A[1,0] DO                       00047400
                  FOR J:=1 STEP 1 UNTIL A[0,1] DO                       00047500
                  C[I,J] := A[I,J] + B[I,J];                            00047600
              END OF PROCEDURE MADD;                                    00047700
          PROCEDURE  VADD(A,B,C);                                       00047800
            ARRAY  A,B,C[0];                                            00047900
              COMMENT  C=A+B (11/67);                                   00048000
              BEGIN                                                     00048100
                INTEGER I;                                              00048200
                  C[0]:=A[0];                                           00048300
                  FOR I:=1 STEP 1 UNTIL A[0] DO                         00048400
               C[I]:=A[I]+B[I];                                         00048500
              END OF PROCEDURE VADD;                                    00048600
          PROCEDURE QADD(P,Q,PADDQ);                                    00048700
            ARRAY P,Q,PADDQ[0];                                         00048800
              BEGIN                                                     00048900
                INTEGER SMALL,I;                                        00049000
                  IF P[0]>Q[0] THEN                                     00049100
                    BEGIN  SMALL:=Q[0];  PADDQ[0]:=P[0];                00049200
                      FOR I:=SMALL+1 STEP 1 UNTIL P[0] DO               00049300
                      PADDQ[I]:=P[I]                                    00049400
                    END                                                 00049500
                  ELSE                                                  00049600
                    BEGIN  SMALL:=P[0];  PADDQ[0]:=Q[0];                00049700
                      FOR I:=SMALL+1 STEP 1 UNTIL Q[0] DO               00049800
                      PADDQ[I]:=Q[I]                                    00049900
                    END;                                                00050000
                  FOR I:=1 STEP 1 UNTIL SMALL DO PADDQ[I]:=P[I]+Q[I];   00050100
              END 0F PROCEDURE QADD;                                    00050200
          PROCEDURE  MMULT(A,B,C);                                      00050300
            ARRAY  A,B,C[0,0];                                          00050400
              COMMENT C=AB  (11/67);                                    00050500
              BEGIN                                                     00050600
                INTEGER I,J,K;                                          00050700
                  C[1,0]:=A[1,0]; C[0,1]:=B[0,1];                       00050800
                  FOR I:=1 STEP 1 UNTIL C[1,0] DO                       00050900
                  FOR J:=1 STEP 1 UNTIL C[0,1] DO                       00051000
                    BEGIN                                               00051100
                     C[I,J]:=0;                                         00051200
                     FOR K:=1 STEP 1 UNTIL A[0,1] DO                    00051300
                     C[I,J]:=C[I,J]+ A[I,K]|B[K,J]                      00051400
                    END;                                                00051500
              END OF PROCEDURE MMULT;                                   00051600
          PROCEDURE VDOT(A,B,C);                                        00051700
            ARRAY  A,B[0];                                              00051800
            REAL  C;                                                    00051900
              BEGIN                                                     00052000
                 INTEGER  I;                                            00052100
                   FOR I:=1 STEP 1 UNTIL A[0] DO                        00052200
                   C:=A[I]|B[I]+C;                                      00052300
              END OF PROCEDURE VDOT;                                    00052400
          PROCEDURE QMULT(A,B,PROD);                                    00052500
            ARRAY A,B,PROD[0];                                          00052600
              BEGIN                                                     00052700
                INTEGER I,J;                                            00052800
                  FOR I:=0 STEP 1 UNTIL A[0]+B[0] DO PROD[I]:=0;        00052900
                  FOR I:=1 STEP 1 UNTIL A[0] DO                         00053000
                  FOR J:=1 STEP 1 UNTIL B[0] DO                         00053100
                  PROD[I+J-1]:=PROD[I+J-1]+A[I]|B[J];                   00053200
                  PROD[0]:=A[0]+B[0]-1;                                 00053300
              END OF PROCEDURE QMULT;                                   00053400
          PROCEDURE MTV(A,V,R);                                         00053500
                 ARRAY A[0,0],V,R[0];                                   00053600
                   COMMENT R=AV (MATRIX A POSTMULTIPLIED BY VECT0R) 168;00053700
                   BEGIN                                                00053800
                     INTEGER  I,J;                                      00053900
                       FOR I:=1 STEP 1 UNTIL A[1,0] DO                  00054000
                         BEGIN R[I]:=0;                                 00054100
                           FOR J:=1 STEP 1 UNTIL V[0] DO                00054200
                           R[I]:=R[I]+A[I,J]|V[J];                      00054300
                         END;                                           00054400
                       R[0]:=A[1,0];                                    00054500
                   END OF PROCEDURE MTV;                                00054600
          PROCEDURE CTM(A,B,C);                                         00054700
            VALUE  C;                                                   00054800
            ARRAY  A,B[0,0];                                            00054900
            REAL  C;                                                    00055000
              BEGIN                                                     00055100
                INTEGER  I,J;                                           00055200
               B[1,0]:=A[1,0];  B[0,1]:=A[0,1];                         00055300
                  FOR I:=1 STEP 1 UNTIL A[1,0] DO                       00055400
                  FOR J:=1 STEP 1 UNTIL A[0,1] DO                       00055500
                  B[I,J]:=A[I,J]|C;                                     00055600
              END OF PROCEDURE CTM;                                     00055700
          PROCEDURE CTV(A,B,C);                                         00055800
            VALUE  C;                                                   00055900
            ARRAY  A,B[0];                                              00056000
            REAL  C;                                                    00056100
              BEGIN                                                     00056200
                INTEGER I;                                              00056300
               B[0]:=A[0];                                              00056400
                  FOR I:=1 STEP 1 UNTIL A[0] DO                         00056500
                  B[I]:=A[I]|C;                                         00056600
              END OF PROCEDURE CTV;                                     00056700
          PROCEDURE  MTRAN(A,B);                                        00056800
            ARRAY  A,B[0,0];                                            00056900
              COMMENT B=A TRANSP0SE   (12/67);                          00057000
              BEGIN                                                     00057100
                INTEGER I,J;                                            00057200
                  FOR I:=1 STEP 1 UNTIL A[1,0] DO                       00057300
                  FOR J:=1 STEP 1 UNTIL A[0,1] DO                       00057400
                  B[J,I]:=A[I,J];                                       00057500
                  B[0,1]:=A[1,0]; B[1,0]:=A[0,1];                       00057600
              END OF PROCEDURE MTRAN;                                   00057700
          PROCEDURE  MID(I,N);                                          00057800
            VALUE N; INTEGER N;  ARRAY I[0,0];                          00057900
              COMMENT  GENERATE AN N BY N IDENTITY MATRIX  (12/67);     00058000
              BEGIN                                                     00058100
                INTEGER J,K;                                            00058200
                  FOR J:= 0 STEP 1 UNTIL N DO                           00058300
                  FOR K:= 0 STEP 1 UNTIL N DO I[J,K]:=0;                00058400
                  FOR J:= 1 STEP 1 UNTIL N DO I[J,J]:=1;                00058500
                  I[0,1]:=I[1,0]:= N;                                   00058600
              END OF PROCEDURE MID;                                     00058700
          PROCEDURE MSUB(A,B,C);                                        00058800
            ARRAY A,B,C[0,0];                                           00058900
              COMMENT  C=A-B    (12/67);                                00059000
              BEGIN                                                     00059100
                INTEGER I,J;                                            00059200
                  C[1,0]:=A[1,0];  C[0,1]:=A[0,1];                      00059300
                  FOR I:=1 STEP 1 UNTIL A[1,0] DO                       00059400
                  FOR J:=1 STEP 1 UNTIL A[0,1] DO                       00059500
                  C[I,J]:=A[I,J] - B[I,J];                              00059600
              END OF PROCEDURE MSUB;                                    00059700
          PROCEDURE  VSUB(A,B,C);                                       00059800
            ARRAY  A,B,C[0];                                            00059900
              COMMENT  C=A-B (11/67);                                   00060000
              BEGIN                                                     00060100
                INTEGER I;                                              00060200
                  C[0]:=A[0];                                           00060300
                  FOR I:=1 STEP 1 UNTIL A[0] DO                         00060400
               C[I]:=A[I]-B[I];                                         00060500
              END OF PROCEDURE VSUB;                                    00060600
          PROCEDURE QSUB(P,Q,QSUBQ);                                    00060700
            ARRAY P,Q,QSUBQ[0];                                         00060800
              BEGIN                                                     00060900
                INTEGER SMALL,I;                                        00061000
                  IF P[0]>Q[0] THEN                                     00061100
                    BEGIN  SMALL:=Q[0];  QSUBQ[0]:=P[0];                00061200
                      FOR I:=SMALL+1 STEP 1 UNTIL P[0] DO               00061300
                        QSUBQ[I]:=P[I]                                  00061400
                    END                                                 00061500
                  ELSE                                                  00061600
                    BEGIN  SMALL:=P[0];  QSUBQ[0]:=Q[0];                00061700
                      FOR I:=SMALL+1 STEP 1 UNTIL Q[0] DO               00061800
                      QSUBQ[I]:=-Q[I]                                   00061900
                    END;                                                00062000
                  FOR I:=1 STEP 1 UNTIL SMALL DO QSUBQ[I]:=P[I]-Q[I];   00062100
              END OF PROCEDURE QSUB;                                    00062200
          REAL PROCEDURE MDET(B);                                       00062300
            ARRAY  B[0,0];                                              00062400
              COMMENT  B0003      MODIFIED 11/67;                       00062500
              BEGIN                                                     00062600
                INTEGER  I,J,K,K2,L,N; REAL BIG,DIAG,TEMP,Q,EPS,D;      00062700
                LABEL  D1,D2,D3,EXIT;                                   00062800
                ARRAY  A[0:10,0:10];                                    00062900
                  FOR I:=0 STEP 1 UNTIL 10 DO                           00063000
                  FOR J:=0 STEP 1 UNTIL 10 DO                           00063100
                  A[I,J]:=B[I,J];                                       00063200
                  N:=A[0,1];  EPS:=@-6;                                 00063300
COMMENT       THE INPUT MATRIX IS DECOMPOSED INTO TWO TRIANGULAR        00063400
              MATRICES USING ROW PIVOTS. ;                              00063500
                                                                        00063600
              D ~ 1.0 ;                                                 00063700
D1:           FOR I ~ 1 STEP 1 UNTIL N DO                               00063800
         BEGIN                                                          00063900
              L ~ I-1 ;                                                 00064000
              FOR J ~ I STEP 1 UNTIL N DO                               00064100
         BEGIN                                                          00064200
              Q~0 ;                                                     00064300
              FOR K ~ 1 STEP 1 UNTIL L DO Q ~ A[J,K] | A[K,I] + Q ;     00064400
              A[J,I] ~ A[J,I] - Q                                       00064500
         END ;                                                          00064600
              BIG ~ 0 ;  K2 ~ I ;                                       00064700
D2:           FOR K ~ I STEP 1 UNTIL N DO                               00064800
         BEGIN                                                          00064900
              IF ABS(A[K,I]) > BIG THEN                                 00065000
         BEGIN                                                          00065100
              BIG ~ ABS(A[K,I]) ;  K2 ~ K                               00065200
         END                                                            00065300
         END ;                                                          00065400
              IF BIG < EPS THEN                                         00065500
         BEGIN                                                          00065600
              D ~ 0 ;  GO TO EXIT                                       00065700
         END ;                                                          00065800
                                                                        00065900
COMMENT       "BIG" IS THE LARGEST ELEMENT IN THE COLUMN JUST COMPUTED. 00066000
              IF NECESSARY, A ROW EXCHANGE IS MADE SO THAT "BIG" IS     00066100
              ON THE MAIN DIAGONAL ;                                    00066200
                                                                        00066300
              IF K2 ! I THEN                                            00066400
         BEGIN                                                          00066500
              D ~ -D ;                                                  00066600
              FOR K ~ 1 STEP 1 UNTIL N DO                               00066700
         BEGIN                                                          00066800
              TEMP ~ A[I,K] ;  A[I,K] ~ A[K2,K] ;  A[K2,K] ~ TEMP       00066900
         END                                                            00067000
         END ;                                                          00067100
                                                                        00067200
COMMENT       THE VALUE OF THE DETERMINANT, "D", IS THE PRODUCT OF THE  00067300
              DIAGONAL ELEMENTS OF THE DECOMPOSED MATRIX. ;             00067400
                                                                        00067500
D3:           DIAG ~ A[I,I] ;  D ~ D | DIAG ;                           00067600
                                                                        00067700
COMMENT       A ROW OF THE DECOMPOSED MATRIX IS NOW CALCULATED. ;       00067800
                                                                        00067900
              FOR J ~ I+1 STEP 1 UNTIL N DO                             00068000
         BEGIN                                                          00068100
              Q~0 ;                                                     00068200
              FOR K ~ 1 STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;     00068300
              A[I,J] ~ (A[I,J]-Q)/DIAG                                  00068400
         END                                                            00068500
         END ;                                                          00068600
          EXIT: MDET:=D;                                                00068700
              END OF PROCEDURE MDET;                                    00068800
          PROCEDURE MINV(B,A);                                          00068900
            ARRAY  A,B[0,0];                                            00069000
              COMMENT    B0004    MODIFIED  11/67 ;                     00069100
              BEGIN                                                     00069200
                INTEGER I,J,K,II,N1,K2,L,N;  REAL BIG,TEMP,DIAG,Q,EPS;  00069300
                INTEGER ARRAY  F[0:B[0,1]];  LABEL I2,I3,I4,I5,I6,SK3;  00069400
                LABEL  SINGULAR;                                        00069500
                FORMAT  F1(X8,"{!MATRIX IS SINGULAR.~");                00069600
                  FOR I:=0 STEP 1 UNTIL 10 DO                           00069700
                  FOR J:=0 STEP 1 UNTIL 10 DO                           00069800
                  A[I,J]:=B[I,J];                                       00069900
                  N:=A[0,1];  EPS:=@-6;                                 00070000
COMMENT       THIS PROCEDURE COMPUTES THE INVERSE OF A MATRIX.  IF THE  00070100
              MATRIX TO BE INVERTED IS SINGULAR, AN EXIT IS MADE TO THE 00070200
              NON-LOCAL LABEL "SINGULAR".  THE INVERSE MATRIX REPLACES  00070300
              THE ORIGINAL MATRIX IN MEMORY.                            00070400
                                                                        00070500
              R.D. RODMAN,                                              00070600
              (PROFESSIONAL SERVICES DIVISIONAL GROUP),                 00070700
                                                                        00070800
              CARD SEQUENCE BEGINS WITH INVS0010.                       00070900
              FIRST RELEASE 12/01/62.  ;                                00071000
                                                                        00071100
COMMENT       THE MATRIX IS TRIANGULARIZED USING ROW PIVOTS.  IF THE    00071200
              LARGEST ELEMENT IN A NEWLY CALCULATED COLUMN IS, IN       00071300
              ABSOLUTE VALUE, LESS THAN EPS, AN EXIT IS MADE TO THE     00071400
                                                                        00071500
              NON-LOCAL LABEL "SINGULAR". ;                             00071600
I2:           FOR I ~ 1 STEP 1 UNTIL N DO                               00071700
         BEGIN                                                          00071800
              II ~ I-1 ;                                                00071900
              FOR J ~ I STEP 1 UNTIL N DO                               00072000
         BEGIN                                                          00072100
              Q ~ 0 ;                                                   00072200
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[J,K] | A[K,I] + Q ;    00072300
              A[J,I] ~ A[J,I] - Q                                       00072400
         END ;                                                          00072500
              BIG ~ 0 ;  K2 ~ I ;                                       00072600
I3:           FOR K ~ I STEP 1 UNTIL N DO                               00072700
         BEGIN                                                          00072800
              IF ABS(A[K,I]) > BIG THEN                                 00072900
         BEGIN                                                          00073000
              BIG ~ ABS(A[K,I]) ;  K2 ~ K                               00073100
         END                                                            00073200
         END ;                                                          00073300
              IF BIG { EPS THEN                                         00073400
          BEGIN                                                         00073500
               WRITE (TTOUT(STA),F1);                                   00073600
               GO TO SINGULAR;                                          00073700
          END;                                                          00073800
              F[I] ~ K2 ;                                               00073900
              IF K2 ! I THEN                                            00074000
I4:           FOR K ~ 1 STEP 1 UNTIL N DO                               00074100
         BEGIN                                                          00074200
              TEMP ~ A[I,K] ;  A[I,K] ~ A[K2,K] ;  A[K2,K] ~ TEMP       00074300
         END ;                                                          00074400
              DIAG ~ A[I,I] ;                                           00074500
                                                                        00074600
COMMENT       A PIVOT WAS MADE.  A ROW IS NOW CALCULATED.  ;            00074700
                                                                        00074800
              FOR J ~ I+1 STEP 1 UNTIL N DO                             00074900
         BEGIN                                                          00075000
              Q ~ 0 ;                                                   00075100
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;    00075200
              A[I,J] ~ (A[I,J] - Q) / DIAG                              00075300
         END                                                            00075400
         END ;                                                          00075500
                                                                        00075600
COMMENT       THE LOWER TRIANGULAR MATRIX IS INVERTED.  ;               00075700
                                                                        00075800
I5:           FOR I ~ 1 STEP 1 UNTIL N DO                               00075900
         BEGIN                                                          00076000
              II ~ I-1 ;  DIAG ~ A[I,I] ;                               00076100
              FOR J ~ 1 STEP 1 UNTIL I DO                               00076200
         BEGIN                                                          00076300
              IF I=J THEN A[I,J] ~ 1/DIAG ELSE                          00076400
         BEGIN                                                          00076500
              Q ~ 0 ;                                                   00076600
              FOR K ~ J STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;    00076700
              A[I,J] ~ -Q/DIAG                                          00076800
         END                                                            00076900
         END                                                            00077000
         END ;                                                          00077100
              N1 ~ N-1 ;                                                00077200
                                                                        00077300
COMMENT       THE UPPER TRIANGULAR MATRIX IS INVERTED.  ;               00077400
                                                                        00077500
              FOR I ~ N1 STEP -1 UNTIL 1 DO                             00077600
         BEGIN                                                          00077700
              II ~ I+1 ;                                                00077800
              FOR J ~ N STEP -1 UNTIL II DO                             00077900
         BEGIN                                                          00078000
              Q~0 ;  L ~ J-1 ;                                          00078100
              FOR K ~ II STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;    00078200
              A[I,J] ~ -A[I,J] - Q                                      00078300
         END                                                            00078400
         END ;                                                          00078500
                                                                        00078600
COMMENT       THE INVERTED TRIANGULAR MATRICES ARE MULTIPLIED  TOGETHER.00078700
              THEIR PRODUCT IS THE DESIRED INVERSE.  ;                  00078800
                                                                        00078900
              FOR I ~ 1 STEP 1 UNTIL N1 DO                              00079000
              FOR J ~ 1 STEP 1 UNTIL N DO                               00079100
         BEGIN                                                          00079200
              Q ~ 0 ;                                                   00079300
              IF I}J THEN                                               00079400
         BEGIN                                                          00079500
              FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;   00079600
              A[I,J] ~ A[I,J] + Q                                       00079700
         END                                                            00079800
              ELSE                                                      00079900
         BEGIN                                                          00080000
              FOR K ~ J STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;     00080100
              A[I,J] ~ Q                                                00080200
         END                                                            00080300
         END ;                                                          00080400
                                                                        00080500
COMMENT       THE COLUMNS OF THE PRODUCT ARE NOW PERMUTED IN SUCH A WAY 00080600
              AS TO COMPENSATE FOR THE ROW PIVOTS MADE IN THE COURSE OF 00080700
              TRIANGULARIZATION.  ;                                     00080800
                                                                        00080900
I6:           FOR J ~ N STEP -1 UNTIL 1 DO                              00081000
         BEGIN                                                          00081100
              K2 ~ F[J] ;                                               00081200
              IF F[J] = J THEN GO TO SK3 ;                              00081300
              FOR K ~ 1 STEP 1 UNTIL N DO                               00081400
         BEGIN                                                          00081500
              TEMP ~ A[K,K2] ;  A[K,K2] ~ A[K,J] ;  A[K,J] ~ TEMP       00081600
         END ;                                                          00081700
SK3:     END;                                                           00081800
SINGULAR:     END OF PROCEDURE MINV;                                    00081900
PROCEDURE      SOLVE(M,X,V);                                            00082000
REAL ARRAY     M[0,0],V,X[0];                                           00082100
          BEGIN                                                         00082200
FORMAT         F1(X8,"{!THE COEFFICIENT MATRIX IS SINGULAR.~");         00082300
INTEGER        I,J,K,K2,L,N;                                            00082400
REAL           BIG,TEMP,DIAG,Q;                                         00082500
OWN INTEGER ARRAY  F[0:M[1,0]];                                         00082600
REAL ARRAY     D,RC,CC[0:M[1,0]],A[0:10,0:10],C[0:10];                  00082700
OWN REAL ARRAY B[0:M[1,0],0:M[1,0]];                                    00082800
LABEL          EXIT;                                                    00082900
               FOR I~0 STEP 1 UNTIL 10 DO                               00083000
               FOR J~0 STEP 1 UNTIL 10 DO                               00083100
               A[I,J]:=M[I,J];                                          00083200
               FOR I~0 STEP 1 UNTIL 10 DO                               00083300
               C[I]:=V[I];                                              00083400
               X[0]:=C[0];                                              00083500
               N:=M[1,0];                                               00083600
COMMENT        THE COEFFICIENT MATRIX IS TRIANGULARIZED;                00083700
               FOR I:= 1 STEP 1 UNTIL N DO                              00083800
               FOR J:= 1 STEP 1 UNTIL N DO                              00083900
               B[I,J]:= A[I,J];                                         00084000
               FOR I:=1 STEP 1 UNTIL N DO                               00084100
          BEGIN                                                         00084200
               L:= I - 1;                                               00084300
               FOR J ~ I STEP 1 UNTIL N DO                              00084400
          BEGIN                                                         00084500
               Q:= 0;                                                   00084600
               FOR K:= 1 STEP 1 UNTIL L DO Q:= B[J,K] | B[K,I] + Q;     00084700
               B[J,I] :=B[J,I] - Q                                      00084800
          END;                                                          00084900
               BIG:= 0;  K2:= I;                                        00085000
               FOR K:=I STEP 1 UNTIL N DO                               00085100
          BEGIN                                                         00085200
               IF ABS(B[K,I]) GTR BIG THEN                              00085300
          BEGIN                                                         00085400
               BIG:= ABS(B[K,I]); K2:= K                                00085500
          END                                                           00085600
          END;                                                          00085700
               IF BIG LEQ 0.5|N|@-10 THEN                               00085800
          BEGIN                                                         00085900
               WRITE (TTOUT(STA),F1);                                   00086000
               GO TO EXIT;                                              00086100
          END;                                                          00086200
               F[I]:=K2;                                                00086300
               IF K2 NEQ I THEN                                         00086400
               FOR K:=1 STEP 1 UNTIL N DO                               00086500
          BEGIN                                                         00086600
               TEMP:= A[K2,K]; A[K2,K]:= A[I,K]; A[I,K]:= TEMP;         00086700
               TEMP:= B[K2,K]; B[K2,K]:= B[I,K]; B[I,K]:= TEMP;         00086800
          END;                                                          00086900
               DIAG:= B[I,I];                                           00087000
               FOR J:=I+1 STEP 1 UNTIL N DO                             00087100
          BEGIN                                                         00087200
               Q:= 0;                                                   00087300
               FOR K:= 1 STEP 1 UNTIL L DO Q:= B[I,K] | B[K,J] + Q;     00087400
               B[I,J]:= (B[I,J] - Q)/DIAG                               00087500
          END                                                           00087600
          END;                                                          00087700
COMMENT        THE REDUCED "C" VECTOR IS COMPUTED. ;                    00087800
               FOR I:=1 STEP 1 UNTIL N DO                               00087900
          BEGIN                                                         00088000
               TEMP:= C[F[I]]; C[F[I]]:= C[I]; D[I]:= C[I]:= TEMP       00088100
          END;                                                          00088200
COMMENT        THE BACKWARD PASS,GIVING THE DESIRED SOLUTION,           00088300
               IS EXECUTED. ;                                           00088400
               FOR I:= 1 STEP 1 UNTIL N DO                              00088500
          BEGIN                                                         00088600
               L:= I - 1; Q:= 0;                                        00088700
               FOR K:=1 STEP 1 UNTIL L DO Q:= B[I,K] | D[K] + Q;        00088800
               D[I]:= (D[I] -  Q)/B[I,I]                                00088900
          END;                                                          00089000
               FOR I:=N STEP -1 UNTIL 1 DO                              00089100
          BEGIN                                                         00089200
               Q:= 0;                                                   00089300
               FOR K:= I+1 STEP 1 UNTIL N DO Q:= B[I,K] | X[K] + Q;     00089400
               X[I]:= D[I] - Q                                          00089500
          END;                                                          00089600
EXIT:     END;                                                          00089700
          PROCEDURE  MEVE(B,A);                                         00089800
                ARRAY  A,B[0,0];                                        00089900
              COMMENT  ALGORITHM 270 NOV 1965    A. NEWHOUSE;           00090000
              BEGIN                                                     00090100
                INTEGER ARRAY R,C[0:B[0,1]];  INTEGER I,J,K,M,JJ,KK,T,N;00090200
                REAL MAX,TEMP,EPS; LABEL L,SORT,REPEAT;                 00090300
                INTEGER EC;                                             00090400
                EPS:=10@-6;  N~B[0,1];                                  00090500
                FOR I~0 STEP 1 UNTIL 10 DO                              00090600
                FOR J~0 STEP 1 UNTIL 10 DO                              00090700
                A[I,J]:=B[I,J];                                         00090800
                FOR I:=1 STEP 1 UNTIL N DO  R[I]:=C[I]:=0;              00090900
                FOR M:=1 STEP 1 UNTIL N DO                              00091000
                  BEGIN  MAX:=0;                                        00091100
                    FOR K:=1 STEP 1 UNTIL N DO                          00091200
                      BEGIN IF R[K]!0 THEN GO TO L ELSE                 00091300
                        FOR J:=1 STEP 1 UNTIL N DO                      00091400
                        IF C[J] =0 AND ABS(A[K,J]) > MAX THEN           00091500
                          BEGIN KK:=K; JJ:=J; MAX:=ABS(A[K,J])          00091600
                          END J LOOP;                                   00091700
          L:          END K LOOP;                                       00091800
                    IF MAX < EPS THEN GO TO SORT;                       00091900
                    C[JJ]:=KK; R[KK]:=JJ; TEMP:=1/A[KK,JJ]; A[KK,JJ]:=1;00092000
                    FOR J:=1 STEP 1 UNTIL N DO A[KK,J]:=A[KK,J]|TEMP;   00092100
                    FOR K:=1 STEP 1 UNTIL KK-1,KK+1 STEP 1 UNTIL N DO   00092200
                      BEGIN TEMP:=A[K,JJ];  A[K,JJ]:=0;                 00092300
                        FOR J:=1 STEP 1 UNTIL N DO                      00092400
                          BEGIN  A[K,J]:= A[K,J]-TEMP | A[KK,J];        00092500
                            IF ABS(A[K,J]) < EPS THEN A[K,J]:=0         00092600
                          END;                                          00092700
                      END K LOOP;                                       00092800
                  END M LOOP;                                           00092900
          SORT: FOR J:=1 STEP 1 UNTIL N DO                              00093000
                  BEGIN                                                 00093100
          REPEAT:   IF C[J] !0 AND J!C[J] THEN                          00093200
                      BEGIN                                             00093300
                        FOR K:=1 STEP 1 UNTIL N DO                      00093400
                        IF R[K] =0 THEN                                 00093500
                          BEGIN TEMP:=A[K,J];                           00093600
                            A[K,J]:=A[K,C[J]];  A[K,C[J]]:=TEMP         00093700
                          END K LOOP;                                   00093800
                        T:=C[J]; C[J]:=C[T]; C[T]:= T; GO TO REPEAT     00093900
                      END;                                              00094000
                  END OF CONDITIONAL AND J LOOP;                        00094100
                EC:=0;                                                  00094200
                FOR K:= 1 STEP 1 UNTIL N DO                             00094300
                IF R[K]=0 THEN                                          00094400
                  BEGIN  EC:=EC+1; A[K,K]:=1;                           00094500
                    IF EC!K     THEN                                    00094600
                      BEGIN                                             00094700
                        FOR J:=1 STEP 1 UNTIL N DO A[EC,J]:=A[K,J]      00094800
                      END;                                              00094900
                  END OF CONDITI0NAL AND K LOOP;                        00095000
                A[1,0]:=EC;                                             00095100
              END OF PROCEDURE MEV;                                     00095200
          REAL PROCEDURE MAX(A,N);                                      00095300
            VALUE  N; INTEGER  N; ARRAY  A[0];                          00095400
             BEGIN                                                      00095500
               INTEGER  I;                                              00095600
               REAL  M;      M:=A[1];                                   00095700
                 FOR I:=2 STEP 1 UNTIL N DO                             00095800
                 IF A[I]>M THEN M:=A[I];                                00095900
                 MAX:=M                                                 00096000
             END OF PROCEDURE MAX;                                      00096100
          REAL PROCEDURE MIN(A,N);                                      00096200
            VALUE  N; INTEGER  N; ARRAY  A[0];                          00096300
              BEGIN                                                     00096400
                INTEGER  I;                                             00096500
                REAL  M;    M:=A[1];                                    00096600
                  FOR I:=2 STEP 1 UNTIL N DO                            00096700
                  IF A[I]<M THEN M:=A[I];                               00096800
                  MIN:=M                                                00096900
              END OF PROCEDURE MIN;                                     00097000
          REAL PROCEDURE CABS(X,Y);                                     00097100
            VALUE  X,Y; REAL  X,Y;                                      00097200
              BEGIN                                                     00097300
          COMMENT  ALGORITHM 312  OCT. 1967 <PAUL FRIEDLAND>;           00097400
                X:=ABS(X);  Y:=ABS(Y);                                  00097500
                CABS:=IF X=0 THEN Y ELSE IF Y=0 THEN X ELSE             00097600
                      IF X>Y THEN X|SQRT(1+(Y/X)*2)                     00097700
                             ELSE Y|SQRT(1+(X/Y)*2)                     00097800
              END OF PROCEDURE CABS;                                    00097900
          PROCEDURE CMULT(XR,XI,YR,YI,AR,AI);                           00098000
            VALUE  XR,XI,YR,YI; REAL  XR,XI,YR,YI,AR,AI;                00098100
              BEGIN                                                     00098200
                AR:=XR|YR-XI|YI; AI:=XR|YI+XI|YR                        00098300
              END;                                                      00098400
          PROCEDURE CADD(XR,XI,YR,YI,AR,AI);                            00098500
            VALUE  XR,XI,YR,YI; REAL  XR,XI,YR,YI,AR,AI;                00098600
              BEGIN                                                     00098700
                AR:=XR+YR; AI:=XI+YI                                    00098800
              END;                                                      00098900
          REAL PROCEDURE ABSCPOLY(N,CO,ZR,ZI);                          00099000
            VALUE  N; INTEGER  N; ARRAY  CO[0]; REAL  ZR,ZI;            00099100
              BEGIN                                                     00099200
                INTEGER  I;                                             00099300
                REAL  YR,YI;                                            00099400
                  YR:=CO[N];                                            00099500
                  FOR I:=N STEP -1 UNTIL 1 DO                           00099600
                  BEGIN                                                 00099700
                    CMULT(YR,YI,ZR,ZI,YR,YI);                           00099800
                    CADD(YR,YI,CO[I-1],0,YR,YI)                         00099900
                  END;                                                  00100000
                  ABSCPOLY:=CABS(YR,YI)                                 00100100
              END OF PROCEDURE ABSCPOLY;                                00100200
          PROCEDURE FINDROOT(A,N,ROOTR,ROOTI);                          00100300
            VALUE  N; INTEGER  N; ARRAY  A[0]; REAL  ROOTR,ROOTI;       00100400
              BEGIN                                                     00100500
                REAL  GUESR,INCR,COMP,GUESI,PGUS;                       00100600
                INTEGER  I,FLG1,FLG2;                                   00100700
                ARRAY  NBR,NBI,PNB[0:4];                                00100800
                LABEL  L1,L2,L3,L4,L5,L6,L7,XIT;                        00100900
                  IF N=1 THEN                                           00101000
                  BEGIN                                                 00101100
                    ROOTR:=-A[0]/A[1]; ROOTI:=0; GO TO XIT              00101200
                  END;                                                  00101300
                  GUESR:=GUESI:=FLG1:=FLG2:=0; INCR:=0.5; COMP:=@-20;   00101400
          L1:     IF FLG1<5 OR    FLG2>0 THEN                           00101500
                  BEGIN                                                 00101600
                    FLG1:=FLG1+1; GO TO L3                              00101700
                  END;                                                  00101800
          L2:     FLG1:=0; INCR:=8.0|INCR;                              00101900
          L3:     NBR[1]:=GUESR +INCR; NBI[1]:=GUESI;                   00102000
                  NBR[2]:=GUESR-INCR; NBI[2]:=GUESI;                    00102100
                  NBR[3]:=GUESR; NBI[3]:=GUESI+INCR;                    00102200
                  NBR[4]:=GUESR; NBI[4]:=GUESI-INCR;                    00102300
                  PGUS:=ABSCPOLY(N,A,GUESR,GUESI);                      00102400
                  FOR I:= 1,2,3,4 DO                                    00102500
                  PNB[I]:=ABSCPOLY(N,A,NBR[I],NBI[I]);                  00102600
                  IF FLG2>27 THEN GO TO L4;                             00102700
                  IF PNB[1]<@-30 AND PNB[2]<@-30 AND PNB[3]<@-30        00102800
                  AND PNB[4]<@-30 THEN GO TO L4;                        00102900
                  IF MAX(PNB,4)-MIN(PNB,4)<@-20 THEN GO TO L2;          00103000
          L4:     IF PGUS!0 THEN GO TO L6;                              00103100
          L5:     ROOTR:=GUESR; ROOTI:=GUESI;                           00103200
                  GO TO XIT;                                            00103300
          L6:     FOR I:=1,2,3,4 DO IF PNB[I]=0 THEN                    00103400
                  BEGIN                                                 00103500
                    ROOTR:=NBR[I]; ROOTI:=NBI[I];                       00103600
                    GO TO XIT                                           00103700
                  END;                                                  00103800
                  FOR I:=1,2,3,4 DO IF PGUS>PNB[I] THEN GO TO L7;       00103900
                  INCR:= INCR/2;                                        00104000
                  IF INCR<COMP OR FLG2>50 THEN GO TO L5;                00104100
                  FLG2:=FLG2+1; GO TO L3;                               00104200
          L7:     FOR I:=1,2,3,4 DO IF PNB[I]=MIN(PNB,4) THEN           00104300
          BEGIN                                                         00104400
                  GUESR:=NBR[I]; GUESI:=NBI[I]; GO TO L1                00104500
          END;                                                          00104600
          XIT:END OF PROCEDURE FINDROOT;                                00104700
          PROCEDURE DOUBLESYNTHEDIV(A,N,REALROOT,IMAGROOT);             00104800
            VALUE  N,REALROOT,IMAGROOT; INTEGER  N; REAL  REALROOT,     00104900
            IMAGROOT; ARRAY A[0];                                       00105000
              BEGIN                                                     00105100
                INTEGER  I;                                             00105200
                  A[N-1]:=A[N-1]+2|REALROOT|A[N];                       00105300
                  FOR I:=2 STEP 1 UNTIL N DO                            00105400
                  A[N-I]:=A[N-I]-(REALROOT*2+IMAGROOT*2)|A[N-I+2]       00105500
                          +2|REALROOT|A[N-I+1];                         00105600
                  FOR I:=0 STEP 1 UNTIL N-2 DO A[I]:=A[I+2]             00105700
              END OF PROCEDURE DOUBLESYNTHEDIV;                         00105800
          PROCEDURE SYNTHETICDIV(A,N,ROOT);                             00105900
            VALUE  N,ROOT; INTEGER  N; REAL  ROOT; ARRAY  A[0];         00106000
              BEGIN                                                     00106100
                INTEGER  I;                                             00106200
                  FOR I:=1 STEP 1 UNTIL N DO                            00106300
                  A[N-I]:=A[N-I]+ROOT|A[N-I+1];                         00106400
                  FOR I:=0 STEP 1 UNTIL N-1 DO A[I]:=A[I+1]             00106500
              END OF PROCEDURE SYNTHETICDIV;                            00106600
PROCEDURE      PROOTS(B);                                               00106700
ARRAY          B[0];                                                    00106800
          BEGIN                                                         00106900
REAL ARRAY     A[0:B[0]];                                               00107000
INTEGER        NNOW,N,I;  REAL     RP,IP;                               00107100
FORMAT         FT(X8,"{!ROOT      REAL PART           IMAG PART{!~"),   00107200
               ANS(X8,"{!",X1,I2,X3,*(E18.11,X4),"~");                  00107300
          NNOW:=N:=B[0]-1;  FOR I:=0 STEP 1 UNTIL N DO A[I]:=B[I+1];    00107400
               WRITE (TTOUT(STA),FT);                                   00107500
               FOR I:=1 STEP 1 UNTIL N DO                               00107600
          BEGIN                                                         00107700
               FINDROOT(A,NNOW,RP,IP);                                  00107800
               IF IP!0 THEN                                             00107900
          BEGIN                                                         00108000
               WRITE (TTOUT(STA),ANS,I,2,RP,IP);                        00108100
               I:=I+1;                                                  00108200
               WRITE (TTOUT(STA),ANS,I,2,RP,-IP);                       00108300
               DOUBLESYNTHEDIV(A,NNOW,RP,IP);                           00108400
               NNOW:=NNOW-2;                                            00108500
          END  ELSE                                                     00108600
          BEGIN                                                         00108700
               WRITE (TTOUT(STA),ANS,I,1,RP);                           00108800
               SYNTHETICDIV(A,NNOW,RP);                                 00108900
               NNOW:=NNOW-1;                                            00109000
          END;                                                          00109100
          END;                                                          00109200
          END  OF PROCEDURE PROOTS;                                     00109300
COMMENT****************************************************************;00109400
%FILE          DISC DISK SERIAL "PROGRAM""USERS" (2,5,30,SAVE 9);   %SPY00109500
ARRAY          WATCHER [0:4];                                       %SPY00109600
FORMAT         FN(X8,"{!TYPE IN YOUR LAST NAME.{!~");               %SPY00109700
INTEGER        PNTR;         WATCHER[3]:=TIME(1);                   %SPY00109800
               STATION(STA);                                            00109900
               QUEST:=12;  LA:=31;  BLNK:=48; EQ:=61; LP:=29;  RP:=45;  00110000
               COMA:=58; PL:=16; MI:=44; TI:=43; DI:=49;                00110100
               FILL MESSAGE[*] WITH "      ",                           00110200
               "INVALI","D VERB",                          % 01         00110300
               "INVALI","D IDEN","TIFIER",                 % 02         00110400
               "MISSIN","G -,- ",                          % 04         00110500
               "MISSIN","G -(- ",                          % 03         00110600
               "MISSIN","G -)- ",                          % 05         00110700
               "INVALI","D OPER","ATION ",                 % 06         00110800
               "MISSIN","G -=- ",                          % 07         00110900
               "  INVA","LID IN","DEX   ",                 % 08         00111000
               "INVALI","D SIZE",                          % 09         00111100
               "INDEX ","OVERFL","OW OF ",                 % 10         00111200
               "IDENT."," INVAL","ID FOR"," OPER.",        % 11         00111300
               "MISSIN","G -*- ",                          % 12         00111400
               " DIVIS","ION BY"," ZERO ";                 % 13         00111500
           FILL OPCODE[*] WITH " ","+","-","*","DET","TRN"," ","IDN",   00111600
                    "INV","NUL","/","EVE";                              00111700
               WRITE (TTOUT(STA),FN);                               %SPY00111800
               READ (TTIN(STA),8,RD[*]);                            %SPY00111900
               MOVER (RD[1],RD[0],56);                              %SPY00112000
               PUTCHRIN (RD[0],FINDCH(RD[0],LA),BLNK);              %SPY00112100
               SKIPBLANKS;  MOVER(RD[0],WATCHER[1],8);              %SPY00112200
               WATCHER[0]:=0;  SHIFT(WATCHER[0],7,8);               %SPY00112300
               PUTCHRIN(WATCHER[2],0,"0");                          %SPY00112400
               WATCHER[2]:=WATCHER[3];                              %SPY00112500
               WRITE (TTOUT(STA),FO1)[:BRK];                            00112600
REED:          DUMB:=0;                                                 00112700
DUMREED:       OLDDUMB:=DUMB;                                           00112800
               SKIPOVER:=0;                                             00112900
               FOR I~1,2,3 DO TYPE[I]~" ";                              00113000
               FOR I~1,2,3 DO NUM[I]~0;                                 00113100
               READ (TTIN(STA),8,RD[*]);                                00113200
               GP:=FINDCH(RD[1],LA)+1;                                  00113300
               MOVER(RD[1],RD[0],GP);                                   00113400
               PUTCHRIN(RD[0],GP-1,QUEST);                              00113500
               SKIPBLANKS;                                              00113600
               READ(RD[*],<A3>,VERB);                                   00113700
               IF VERB="TEA" THEN                                       00113800
          BEGIN                                                         00113900
               WRITE (TTOUT(STA),TEACH)[:REED];                         00114000
               GO TO REED;                                              00114100
          END  ELSE                                                     00114200
               IF VERB="COM" THEN                                       00114300
          BEGIN                                                         00114400
               WRITE (TTOUT(STA),COMMANDS)[:REED];                      00114500
               GO TO REED;                                              00114600
          END  ELSE                                                     00114700
               IF VERB="OPE" THEN                                       00114800
          BEGIN                                                         00114900
               WRITE (TTOUT(STA),OPERATIONS)[:REED];                    00115000
               GO TO REED;                                              00115100
          END  ELSE                                                     00115200
               IF VERB="ERR" THEN                                       00115300
          BEGIN                                                         00115400
               WRITE (TTOUT(STA),ERRORS)[:REED];                        00115500
               GO TO REED;                                              00115600
          END  ELSE                                                     00115700
               IF VERB="STO" OR VERB="QUI" THEN GO TO STOP ELSE         00115800
               IF VERB="SIZ" THEN NEXTSTOP:=0 ELSE                      00115900
               IF VERB="INP" OR VERB="REA" THEN NEXTSTOP:=1 ELSE        00116000
               IF VERB="PRI" OR VERB="WRI" THEN NEXTSTOP:=2 ELSE        00116100
               IF VERB="LET" OR VERB="SET" THEN NEXTSTOP:=3 ELSE        00116200
               IF VERB="SOL" THEN NEXTSTOP:=4 ELSE                      00116300
               IF VERB="ROO" THEN NEXTSTOP:=5 ELSE                      00116400
          BEGIN                                                         00116500
               READ (RD[*],<A*>,IF CPACE:=FINDCH(RD[0],BLNK){6 THEN     00116600
                    CPACE ELSE CPACE:=6,VERB);                          00116700
               SKIPOVER:=CPACE;                                         00116800
               WRITE (TTOUT(STA),ERR,SKIPOVER,1,2,6,MESSAGE[1],MESSAGE  00116900
                    [2],1,"A",CPACE,VERB)[:BRK];                        00117000
               DUMB:=DUMB+1;                                            00117100
               IF DUMPTHISONE THEN GO TO STOP ELSE GO TO DUMREED;       00117200
          END;                                                          00117300
               IF FINDCH(RD[0],BLNK)}GP THEN                            00117400
          BEGIN                                                         00117500
               READ (RD[*],<A6>,VERB);                                  00117600
               WRITE (TTOUT(STA),ERR,6,1,2,6,MESSAGE[1],MESSAGE[2],1,"A"00117700
                    ,6,VERB)[:BRK];                                     00117800
               DUMB:=DUMB+1;                                            00117900
               IF DUMPTHISONE THEN GO TO STOP ELSE GO TO DUMREED;       00118000
          END;                                                          00118100
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],BLNK)+1,GP:=GP-CPACE);   00118200
               SKIPBLANKS;                                              00118300
               SKIPOVER:=SKIPOVER+CPACE+2;                              00118400
               READ (RD[*],<A1,I1>,TYPE[1],NUM[1])[:RERR];              00118500
               IF (TYPE[1]!"M" AND TYPE[1]!"V" AND TYPE[1]!"C" AND      00118600
                    TYPE[1]!"P") OR NUM[1]>9 THEN                       00118700
          BEGIN                                                         00118800
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00118900
                    [4],MESSAGE[5],2,"A",1,TYPE[1],NUM[1])[:BRK];       00119000
               DUMB:=DUMB+1;                                            00119100
               IF DUMPTHISONE THEN GO TO STOP ELSE GO TO DUMREED;       00119200
          END;                                                          00119300
               IF NEXTSTOP=0 THEN                                       00119400
                                                                        00119500
          BEGIN                                                         00119600
               IF TYPE[1]="M" THEN                                      00119700
               WRITE (TTOUT(STA),MSIZE,NUM[1],IF NUM[2]:=MATRIX[NUM[1],100119800
                    ,0]<10 THEN 1 ELSE 2,NUM[2],IF NUM[3]:=MATRIX[NUM[1]00119900
                    ,0,1]<10 THEN 1 ELSE 2,NUM[3])[:BRK]                00120000
               ELSE IF TYPE[1]="V" THEN                                 00120100
               WRITE (TTOUT(STA),VSIZE,NUM[1],IF NUM[2]:=VECTOR[NUM[1],000120200
                    ]<10 THEN 1 ELSE 2,NUM[2])[:BRK]                    00120300
               ELSE IF TYPE[1]="P" THEN                                 00120400
               WRITE (TTOUT(STA),PSIZE,NUM[1],IF NUM[2]:=POLY[NUM[1],0] 00120500
                    -1<10 AND NUM[2]>-1 THEN 1 ELSE 2,NUM[2])[:BRK] ELSE00120600
          BEGIN                                                         00120700
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00120800
                    [4],MESSAGE[5],2,"A",1,TYPE[1],NUM[1])[:BRK];       00120900
               DUMB:=DUMB+1;                                            00121000
               IF DUMPTHISONE THEN GO TO STOP ELSE GO TO DUMREED;       00121100
          END;                                                          00121200
               GO TO REED;                                              00121300
          END  ELSE                                                     00121400
               IF NEXTSTOP=1 THEN GO TO INPUT ELSE                      00121500
               IF NEXTSTOP=3 THEN GO TO LET ELSE                        00121600
               IF NEXTSTOP=4 THEN GO TO SOLVER ELSE                     00121700
               IF NEXTSTOP=5 THEN GO TO ROOTS;                          00121800
PRINTER:  BEGIN                                                         00121900
LABEL          ENDPRINT;                                                00122000
               IF TYPE[1]="M" THEN                                      00122100
          BEGIN                                                         00122200
               NUM[2]:=MATRIX[NUM[1],1,0];                              00122300
               NUM[3]:=MATRIX[NUM[1],0,1];                              00122400
               K:=0;                                                    00122500
               FOR I~1 STEP 1 UNTIL NUM[2] DO                           00122600
               FOR J~1 STEP 1 UNTIL NUM[3] DO                           00122700
               IF MATRIX[NUM[1],I,J]!0 THEN K:=K+1;                     00122800
               IF K=0 THEN                                              00122900
          BEGIN                                                         00123000
               WRITE (TTOUT(STA),MNULL,NUM[1],IF NUM[2]<10 THEN 1 ELSE 200123100
                    ,NUM[2],IF NUM[3]<10 THEN 1 ELSE 2,NUM[3])[:BRK];   00123200
               GO TO ENDPRINT;                                          00123300
          END;                                                          00123400
               MWRITE(NUM);                                             00123500
          END  ELSE                                                     00123600
               IF TYPE[1]="V" THEN                                      00123700
          BEGIN                                                         00123800
               NUM[2]:=VECTOR[NUM[1],0];                                00123900
               K:=0;                                                    00124000
               FOR I~1 STEP 1 UNTIL NUM[2] DO                           00124100
               IF VECTOR[NUM[1],I]!0 THEN K:=K+1;                       00124200
               IF K=0 THEN                                              00124300
          BEGIN                                                         00124400
               WRITE (TTOUT(STA),VNULL,NUM[1],IF NUM[2]<10 THEN 1 ELSE 200124500
                    ,NUM[2])[:BRK];                                     00124600
               GO TO ENDPRINT;                                          00124700
          END;                                                          00124800
               VWRITE(NUM);                                             00124900
          END  ELSE                                                     00125000
               IF TYPE[1]="P" THEN                                      00125100
          BEGIN                                                         00125200
               NUM[2]:=POLY[NUM[1],0];                                  00125300
               K:=0;                                                    00125400
               FOR I~1 STEP 1 UNTIL NUM[2] DO                           00125500
               IF POLY[NUM[1],I]!0 THEN K:=K+1;                         00125600
               IF K=0 THEN                                              00125700
          BEGIN                                                         00125800
               WRITE (TTOUT(STA),PNULL,NUM[1],IF NUM[2]-1<10 AND NUM[2] 00125900
                    -1>-1 THEN 1 ELSE 2,NUM[2]-1)[:BRK];                00126000
               GO TO ENDPRINT;                                          00126100
          END;                                                          00126200
               PWRITE(NUM);                                             00126300
          END  ELSE                                                     00126400
               WRITE (TTOUT(STA),WC,NUM[1],IF CONST[NUM[1]]<@-1 OR      00126500
                    CONST[NUM[1]]>@5 THEN "E" ELSE "F",CONST[NUM[1]]);  00126600
ENDPRINT: END;                                                          00126700
               IF DUMPTHISONE THEN GO TO STOP;                          00126800
               WRITE (TTOUT(STA),<X8,"{!~">);                           00126900
               IF DUMB!OLDDUMB THEN GO TO DUMREED;                      00127000
               GO TO REED;                                              00127100
INPUT:    BEGIN                                                         00127200
LABEL          ENDINPUT;                                                00127300
FORMAT         FMQUEST(X8,"{!",2A1,"=",A1,"{!~");                       00127400
               IF TYPE[1]="M" THEN                                      00127500
          BEGIN                                                         00127600
                   IF FINDCH(RD[0],LP)<GP THEN                          00127700
          BEGIN                                                         00127800
               IF FINDCH(RD[0],COMA)>GP THEN                            00127900
          BEGIN                                                         00128000
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],LP)+1;                   00128100
               WRITE (TTOUT(STA),ERR,SKIPOVER,4,2,6,MESSAGE[6],MESSAGE  00128200
                    [7],1,"A",1,LP)[:BRK];                              00128300
               DUMB:=DUMB+1;                                            00128400
               GO TO ENDINPUT;                                          00128500
          END;                                                          00128600
               IF FINDCH(RD[0],RP)>GP THEN                              00128700
          BEGIN                                                         00128800
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],COMA)+1;                 00128900
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00129000
                    [11],1,"A",1,COMA)[:BRK];                           00129100
               DUMB:=DUMB+1;                                            00129200
               GO TO ENDINPUT;                                          00129300
          END;                                                          00129400
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00129500
               PUTCHRIN(RD[0],FINDCH(RD[0],RP),COMA);                   00129600
               READ (RD[*],/,NUM[2],NUM[3]);                            00129700
               IF NUM[2]>10 OR NUM[2]<1 THEN                            00129800
          BEGIN                                                         00129900
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],COMA)+2;                 00130000
               WRITE (TTOUT(STA),ERR,SKIPOVER,8,3,6,MESSAGE[17],MESSAGE 00130100
                    [18],MESSAGE[19],1,"I",2,NUM[2])[:BRK];             00130200
               DUMB:=DUMB+1;                                            00130300
               GO TO ENDINPUT;                                          00130400
          END;                                                          00130500
               IF NUM[3]>10 OR NUM[3]<1 THEN                            00130600
          BEGIN                                                         00130700
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],COMA)+1,GP:=GP-CPACE);   00130800
               SKIPOVER:=SKIPOVER+CPACE+2;                              00130900
               WRITE (TTOUT(STA),ERR,SKIPOVER,8,3,6,MESSAGE[17],MESSAGE 00131000
                    [18],MESSAGE[19],1,"I",2,NUM[3])[:BRK];             00131100
               DUMB:=DUMB+1;                                            00131200
               GO TO ENDINPUT;                                          00131300
          END;                                                          00131400
          END  ELSE                                                     00131500
          BEGIN                                                         00131600
               SKIPOVER:=SKIPOVER+2;                                    00131700
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00131800
                    [9],2,"A",1,TYPE[1],NUM[1])[:BRK];                  00131900
               DUMB:=DUMB+1;                                            00132000
               GO TO ENDINPUT;                                          00132100
          END;                                                          00132200
               WRITE (TTOUT(STA),FMQUEST,TYPE[1],NUM[1],QUEST);         00132300
               FOR I~0 STEP 1 UNTIL 10 DO                               00132400
               FOR J~0 STEP 1 UNTIL 10 DO                               00132500
               MATRIX[NUM[1],I,J]~0;                                    00132600
               MATRIX[NUM[1],1,0]:=NUM[2];                              00132700
               MATRIX[NUM[1],0,1]:=NUM[3];                              00132800
               MREAD(NUM);                                              00132900
          END  ELSE                                                     00133000
               IF TYPE[1]="V" THEN                                      00133100
          BEGIN                                                         00133200
                    IF FINDCH(RD[0],LP)<GP THEN                         00133300
          BEGIN                                                         00133400
               IF FINDCH(RD[0],RP)>GP THEN                              00133500
          BEGIN                                                         00133600
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],LP)+1;                   00133700
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00133800
                    [11],1,"A",1,LP)[:BRK];                             00133900
               DUMB:=DUMB+1;                                            00134000
               GO TO ENDINPUT;                                          00134100
          END;                                                          00134200
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00134300
               PUTCHRIN(RD[0],FINDCH(RD[0],RP),COMA);                   00134400
               READ (RD[*],/,NUM[2]);                                   00134500
               IF NUM[2]>10 OR NUM[2]<1 THEN                            00134600
          BEGIN                                                         00134700
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],COMA)+2;                 00134800
               WRITE (TTOUT(STA),ERR,SKIPOVER,8,3,6,MESSAGE[17],MESSAGE 00134900
                    [18],MESSAGE[19],1,"I",2,NUM[2])[:BRK];             00135000
               DUMB:=DUMB+1;                                            00135100
               GO TO ENDINPUT;                                          00135200
          END;                                                          00135300
          END  ELSE                                                     00135400
          BEGIN                                                         00135500
               SKIPOVER:=SKIPOVER+2;                                    00135600
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00135700
                    [9],2,"A",1,TYPE[1],NUM[1])[:BRK];                  00135800
               DUMB:=DUMB+1;                                            00135900
               GO TO ENDINPUT;                                          00136000
          END;                                                          00136100
               WRITE (TTOUT(STA),FMQUEST,TYPE[1],NUM[1],QUEST);         00136200
               FOR I~0 STEP 1 UNTIL 10 DO                               00136300
               VECTOR[NUM[1],I]~0;                                      00136400
               VECTOR[NUM[1],0]:=NUM[2];                                00136500
               VREAD(NUM);                                              00136600
          END  ELSE                                                     00136700
               IF TYPE[1]="P" THEN                                      00136800
          BEGIN                                                         00136900
                    IF FINDCH(RD[0],LP)<GP THEN                         00137000
          BEGIN                                                         00137100
               IF FINDCH(RD[0],RP)>GP THEN                              00137200
          BEGIN                                                         00137300
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],LP)+1;                   00137400
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00137500
                    [11],1,"A",1,LP)[:BRK];                             00137600
               DUMB:=DUMB+1;                                            00137700
               GO TO ENDINPUT;                                          00137800
          END;                                                          00137900
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00138000
               PUTCHRIN(RD[0],FINDCH(RD[0],RP),COMA);                   00138100
               READ (RD[*],/,NUM[2]);   NUM[2]:=NUM[2]+1;               00138200
               IF NUM[2]>20 OR NUM[2]<1 THEN                            00138300
          BEGIN                                                         00138400
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],COMA)+2;                 00138500
               WRITE (TTOUT(STA),ERR,SKIPOVER,8,3,6,MESSAGE[17],MESSAGE 00138600
                    [18],MESSAGE[19],1,"I",2,NUM[2]-1)[:BRK];           00138700
               DUMB:=DUMB+1;                                            00138800
               GO TO ENDINPUT;                                          00138900
          END;                                                          00139000
          END  ELSE                                                     00139100
          BEGIN                                                         00139200
               SKIPOVER:=SKIPOVER+2;                                    00139300
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00139400
                    [9],2,"A",1,TYPE[1],NUM[1])[:BRK];                  00139500
               DUMB:=DUMB+1;                                            00139600
               GO TO ENDINPUT;                                          00139700
          END;                                                          00139800
               WRITE (TTOUT(STA),FMQUEST,TYPE[1],NUM[1],QUEST);         00139900
               FOR I~0 STEP 1 UNTIL 20 DO                               00140000
               POLY[NUM[1],I]:=0;                                       00140100
               POLY[NUM[1],0]:=NUM[2];                                  00140200
               PREAD(NUM);                                              00140300
          END  ELSE                                                     00140400
          BEGIN                                                         00140500
               WRITE (TTOUT(STA),RC,NUM[1],QUEST);                      00140600
               READVAL(CONST[NUM[1]],STA);                              00140700
          END;                                                          00140800
ENDINPUT: END;                                                          00140900
               IF DUMPTHISONE THEN GO TO STOP;                          00141000
               WRITE (TTOUT(STA),<X8,"{!~">);                           00141100
               IF DUMB!OLDDUMB THEN GO TO DUMREED;                      00141200
               GO TO REED;                                              00141300
                                                                        00141400
LET:      BEGIN                                                         00141500
LABEL          ENDLET;                                                  00141600
               IF FINDCH(RD[0],EQ)>GP THEN                              00141700
          BEGIN                                                         00141800
               SKIPOVER:=SKIPOVER+1;                                    00141900
               WRITE (TTOUT(STA),ERR,SKIPOVER,7,2,6,MESSAGE[15],MESSAGE 00142000
                    [16],2,"A",1,TYPE[1],NUM[1])[:BRK];                 00142100
               DUMB:=DUMB+1;                                            00142200
               GO TO ENDLET;                                            00142300
          END;                                                          00142400
               IF FINDCH(RD[0],LP)<FINDCH(RD[0],EQ) THEN                00142500
          BEGIN                                                         00142600
               IF TYPE[1]="M" THEN                                      00142700
          BEGIN                                                         00142800
               IF FINDCH(RD[0],COMA)>FINDCH(RD[0],EQ) THEN              00142900
          BEGIN                                                         00143000
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],LP)+1;                   00143100
               WRITE (TTOUT(STA),ERR,SKIPOVER,4,2,6,MESSAGE[6],MESSAGE  00143200
                    [7],1,"A",1,LP)[:BRK];                              00143300
               DUMB:=DUMB+1;                                            00143400
               GO TO ENDLET;                                            00143500
          END;                                                          00143600
               IF FINDCH(RD[0],RP)>FINDCH(RD[0],EQ) THEN                00143700
          BEGIN                                                         00143800
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],COMA)+1;                 00143900
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00144000
                    [11],1,"A",1,COMA)[:BRK];                           00144100
               DUMB:=DUMB+1;                                            00144200
               GO TO ENDLET;                                            00144300
          END;                                                          00144400
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00144500
               PUTCHRIN(RD[0],FINDCH(RD[0],RP),COMA);                   00144600
               READ (RD[*],/,NUM[2],NUM[3]);                            00144700
               IF NUM[2]>10 OR NUM[2]<1 THEN                            00144800
          BEGIN                                                         00144900
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],COMA)+2;                 00145000
               WRITE (TTOUT(STA),ERR,SKIPOVER,8,3,6,MESSAGE[17],MESSAGE 00145100
                    [18],MESSAGE[19],1,"I",2,NUM[2])[:BRK];             00145200
               DUMB:=DUMB+1;                                            00145300
               GO TO ENDLET;                                            00145400
          END;                                                          00145500
               IF NUM[3]>10 OR NUM[3]<1 THEN                            00145600
          BEGIN                                                         00145700
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],COMA)+1,GP:=GP-CPACE);   00145800
               SKIPOVER:=SKIPOVER+CPACE+2;                              00145900
               WRITE (TTOUT(STA),ERR,SKIPOVER,8,3,6,MESSAGE[17],MESSAGE 00146000
                    [18],MESSAGE[19],1,"I",2,NUM[3])[:BRK];             00146100
               DUMB:=DUMB+1;                                            00146200
               GO TO ENDLET;                                            00146300
          END;                                                          00146400
          END  ELSE                                                     00146500
               IF TYPE[1]="V" THEN                                      00146600
          BEGIN                                                         00146700
               IF FINDCH(RD[0],RP)>FINDCH(RD[0],EQ) THEN                00146800
          BEGIN                                                         00146900
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],LP);                     00147000
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00147100
                    [11],1,"A",1,LP)[:BRK];                             00147200
               DUMB:=DUMB+1;                                            00147300
               GO TO ENDLET;                                            00147400
          END;                                                          00147500
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00147600
               PUTCHRIN(RD[0],FINDCH(RD[0],RP),COMA);                   00147700
               READ (RD[*],/,NUM[2]);                                   00147800
               IF NUM[2]>10 OR NUM[2]<1 THEN                            00147900
          BEGIN                                                         00148000
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],COMA)+2;                 00148100
               WRITE (TTOUT(STA),ERR,SKIPOVER,8,3,6,MESSAGE[17],MESSAGE 00148200
                    [18],MESSAGE[19],1,"I",2,NUM[2])[:BRK];             00148300
               DUMB:=DUMB+1;                                            00148400
               GO TO ENDLET;                                            00148500
          END;                                                          00148600
          END  ELSE                                                     00148700
          BEGIN                                                         00148800
               SKIPOVER:=SKIPOVER+0;                                    00148900
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00149000
                    [4],MESSAGE[5],2,"A",1,TYPE[1],NUM[1])[:BRK];       00149100
               DUMB:=DUMB+1;                                            00149200
               GO TO ENDLET;                                            00149300
          END;                                                          00149400
          END  ELSE                                                     00149500
               IF TYPE[1]="P" THEN                                      00149600
          BEGIN                                                         00149700
               IF FINDCH(RD[0],RP)>FINDCH(RD[0],EQ) THEN                00149800
          BEGIN                                                         00149900
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],LP);                     00150000
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00150100
                    [11],1,"A",1,LP)[:BRK];                             00150200
               DUMB:=DUMB+1;                                            00150300
               GO TO ENDLET;                                            00150400
          END;                                                          00150500
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00150600
               PUTCHRIN(RD[0],FINDCH(RD[0],RP),COMA);                   00150700
               READ (RD[*],/,NUM[2]);   NUM[2]:=NUM[2]+1;               00150800
               IF NUM[2]>20 OR NUM[2]<1 THEN                            00150900
          BEGIN                                                         00151000
               SKIPOVER:=SKIPOVER+FINDCH(RD[0],COMA)+2;                 00151100
               WRITE (TTOUT(STA),ERR,SKIPOVER,8,3,6,MESSAGE[17],MESSAGE 00151200
                    [18],MESSAGE[19],1,"I",2,NUM[2]-1)[:BRK];           00151300
               DUMB:=DUMB+1;                                            00151400
               GO TO ENDLET;                                            00151500
          END;                                                          00151600
          END  ELSE                                                     00151700
               IF FINDCH(RD[0],RP)<FINDCH(RD[0],EQ) THEN                00151800
          BEGIN                                                         00151900
               SKIPOVER:=SKIPOVER+1;                                    00152000
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00152100
                    [9],2,"A",1,TYPE[1],NUM[1])[:BRK];                  00152200
               DUMB:=DUMB+1;                                            00152300
               GO TO ENDLET;                                            00152400
          END;                                                          00152500
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],EQ)+1,GP:=GP-CPACE);     00152600
               SKIPBLANKS;                                              00152700
               SKIPOVER:=SKIPOVER+CPACE;                                00152800
               READ (RD[*],<A3>,OPER);                                  00152900
IF OPER.[30:6]="D" OR OPER.[30:6]="T" OR OPER.[30:6]="I" OR OPER.[30:6]=00153000
                    "N" OR OPER.[30:6]="E" THEN                         00153100
          BEGIN                                                         00153200
               SKIPOVER:=SKIPOVER+1;                                    00153300
               CPACE:=3;                                                00153400
               GO TO ENDLET;                                            00153500
          END;                                                          00153600
IF (OPER.[30:6]!"M" AND OPER.[30:6]!"V" AND OPER.[30:6]!"C" AND         00153700
                    OPER.[30:6]!"P") OR OPER.[36:6]>9 THEN              00153800
               IF ANYFUNNYCHRS(RD[0],GP-1) THEN                         00153900
          BEGIN                                                         00154000
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00154100
                    [4],MESSAGE[5],2,"A",1,TYPE[2],NUM[2])[:BRK];       00154200
               DUMB:=DUMB+1;                                            00154300
               GO TO ENDLET;                                            00154400
          END  ELSE                                                     00154500
          BEGIN                                                         00154600
               IF TYPE[1]="M" THEN                                      00154700
               READARRAY(MATRIX[NUM[1],NUM[2],NUM[3]]) ELSE             00154800
               IF TYPE[1]="V" THEN                                      00154900
               READARRAY(VECTOR[NUM[1],NUM[2]]) ELSE                    00155000
               IF TYPE[1]="C" THEN                                      00155100
               READARRAY(CONST[NUM[1]]) ELSE                            00155200
               IF TYPE[1]="P" THEN                                      00155300
               READARRAY(POLY[NUM[1],NUM[2]]);                          00155400
               DUMB:=0;                                                 00155500
               OLDDUMB:=1;                                              00155600
               WRITE (TTOUT(STA),DONE)[:BRK];                           00155700
               GO TO ENDLET;                                            00155800
          END;                                                          00155900
               READ (RD[*],<A1,I1>,TYPE[2],NUM[2]);                     00156000
               SHIFT(RD[0],2,GP:=GP-2);                                 00156100
               SKIPBLANKS;                                              00156200
               READ (RD[*],<A1>,OPER);                                  00156300
               IF OPER="*" OR OPER="+" OR OPER="-" OR OPER="/" THEN     00156400
               OPER:=OPER ELSE IF OPER=QUEST THEN OPER:=" " ELSE        00156500
          BEGIN                                                         00156600
               READ (RD[*],<3A1>,TYPE[1],TYPE[2],TYPE[3]);              00156700
               CPACE:=3;                                                00156800
               IF TYPE[3]{9 OR TYPE[3]="M" OR TYPE[3]="V" OR TYPE[3]="C"00156900
                    OR TYPE[3]="P" OR TYPE[3]=" " THEN                  00157000
               CPACE:=2;                                                00157100
               IF TYPE[2]{9 OR TYPE[2]="M" OR TYPE[2]="V" OR TYPE[2]="C"00157200
                    OR TYPE[2]="P" OR TYPE[2]=" " THEN                  00157300
               CPACE:=1;                                                00157400
               IF CPACE=3 THEN                                          00157500
          BEGIN                                                         00157600
               OPER:=TYPE[3];OPER.[36:6]:=TYPE[2];OPER.[30:6]:=TYPE[1]; 00157700
          END;                                                          00157800
               IF CPACE=2 THEN                                          00157900
          BEGIN                                                         00158000
               OPER:=TYPE[2];OPER.[36:6]:=TYPE[1];                      00158100
          END;                                                          00158200
               IF CPACE=1 THEN OPER:=TYPE[1];                           00158300
               SKIPOVER:=SKIPOVER+CPACE;                                00158400
               WRITE (TTOUT(STA),ERR,SKIPOVER,6,3,6,MESSAGE[12],MESSAGE 00158500
                    [13],MESSAGE[14],1,"A",CPACE,OPER)[:BRK];           00158600
               DUMB:=DUMB+1;                                            00158700
          END;                                                          00158800
               GO TO ENDLET;                                            00158900
               CPACE:=1;                                                00159000
ENDLET:   END;                                                          00159100
               IF DUMPTHISONE THEN GO TO STOP;                          00159200
               IF DUMB!OLDDUMB THEN GO TO DUMREED;                      00159300
TESTOPER: BEGIN                                                         00159400
LABEL          CHECK;                                                   00159500
LABEL          ADD,SUB,MULT,DET,TRN,EQU,IDN,INV,NULL,DVD,EVEC;          00159600
SWITCH         OP:=ADD,SUB,MULT,DET,TRN,EQU,IDN,INV,NULL,DVD,EVEC;      00159700
               FOR I~1 STEP 1 UNTIL 11 DO                               00159800
               IF OPCODE[I]=OPER THEN GO TO OP[I];                      00159900
               WRITE (TTOUT(STA),ERR,SKIPOVER,6,3,6,MESSAGE[12],MESSAGE 00160000
                    [13],MESSAGE[14],1,"A",CPACE,OPER)[:BRK];           00160100
               DUMB:=DUMB+1;                                            00160200
               GO TO CHECK;                                             00160300
ADD:      BEGIN                                                         00160400
LABEL          ENDADD;                                                  00160500
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],PL)+1,GP:=GP-CPACE);     00160600
               SKIPBLANKS;                                              00160700
               SKIPOVER:=SKIPOVER+CPACE;                                00160800
               READ (RD[*],<A1,I1>,TYPE[3],NUM[3]);                     00160900
               SKIPOVER:=SKIPOVER+2;                                    00161000
               IF (TYPE[3]!"M" AND TYPE[3]!"V" AND TYPE[3]!"C" AND      00161100
                    TYPE[3]!"P") OR NUM[3]>9 THEN                       00161200
          BEGIN                                                         00161300
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00161400
                    [4],MESSAGE[5],2,"A",1,TYPE[3],NUM[3])[:BRK];       00161500
               DUMB:=DUMB+1;                                            00161600
               GO TO ENDADD;                                            00161700
          END;                                                          00161800
               IF TYPE[1]="M" AND TYPE[2]="M" AND TYPE[3]="M" THEN      00161900
          BEGIN                                                         00162000
               IF MATRIX[NUM[2],1,0]!MATRIX[NUM[3],1,0] OR MATRIX       00162100
                    [NUM[2],0,1]!MATRIX[NUM[3],0,1] THEN                00162200
          BEGIN                                                         00162300
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00162400
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00162500
               DUMB:=DUMB+1;                                            00162600
               GO TO ENDADD;                                            00162700
          END;                                                          00162800
               MADD(MATRIX[NUM[2],*,*],MATRIX[NUM[3],*,*],MATRIX[NUM[1],00162900
                    *,*]);                                              00163000
               GO TO ENDADD;                                            00163100
          END  ELSE                                                     00163200
               IF TYPE[1]="V" AND TYPE[2]="V" AND TYPE[3]="V" THEN      00163300
          BEGIN                                                         00163400
               IF VECTOR[NUM[2],0]!VECTOR[NUM[3],0] THEN                00163500
          BEGIN                                                         00163600
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00163700
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00163800
               DUMB:=DUMB+1;                                            00163900
               GO TO ENDADD;                                            00164000
          END;                                                          00164100
               VADD(VECTOR[NUM[2],*],VECTOR[NUM[3],*],VECTOR[NUM[1],*]);00164200
               GO TO ENDADD;                                            00164300
          END  ELSE                                                     00164400
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="P" THEN      00164500
          BEGIN                                                         00164600
               QADD(POLY[NUM[2],*],POLY[NUM[3],*],POLY[NUM[1],*]);      00164700
               GO TO ENDADD;                                            00164800
          END  ELSE                                                     00164900
               IF TYPE[1]="C" AND TYPE[2]="C" AND TYPE[3]="C" THEN      00165000
          BEGIN                                                         00165100
               CONST[NUM[1]]:=CONST[NUM[2]]+CONST[NUM[3]];              00165200
               GO TO ENDADD;                                            00165300
          END  ELSE                                                     00165400
          BEGIN                                                         00165500
               I:=1;                                                    00165600
               I.[36:6]:=1;                                             00165700
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00165800
                    [26],MESSAGE[27],MESSAGE[28],5,"A",1,TYPE[1],MI,    00165900
                    TYPE[2],MI,TYPE[3])[:BRK];                          00166000
               DUMB:=DUMB+1;                                            00166100
               GO TO ENDADD;                                            00166200
          END;                                                          00166300
ENDADD:   END;                                                          00166400
               GO TO CHECK;                                             00166500
SUB:      BEGIN                                                         00166600
LABEL          ENDSUB;                                                  00166700
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],MI)+1,GP:=GP-CPACE);     00166800
               SKIPBLANKS;                                              00166900
               SKIPOVER:=SKIPOVER+CPACE;                                00167000
               READ (RD[*],<A1,I1>,TYPE[3],NUM[3]);                     00167100
               SKIPOVER:=SKIPOVER+2;                                    00167200
               IF (TYPE[3]!"M" AND TYPE[3]!"V" AND TYPE[3]!"C" AND      00167300
                    TYPE[3]!"P") OR NUM[3]>9 THEN                       00167400
          BEGIN                                                         00167500
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00167600
                    [4],MESSAGE[5],2,"A",1,TYPE[3],NUM[3])[:BRK];       00167700
               DUMB:=DUMB+1;                                            00167800
               GO TO ENDSUB;                                            00167900
          END;                                                          00168000
               IF TYPE[1]="M" AND TYPE[2]="M" AND TYPE[3]="M" THEN      00168100
          BEGIN                                                         00168200
               IF MATRIX[NUM[2],1,0]!MATRIX[NUM[3],1,0] OR MATRIX       00168300
                    [NUM[2],0,1]!MATRIX[NUM[3],0,1] THEN                00168400
          BEGIN                                                         00168500
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00168600
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00168700
               DUMB:=DUMB+1;                                            00168800
               GO TO ENDSUB;                                            00168900
          END;                                                          00169000
               MSUB(MATRIX[NUM[2],*,*],MATRIX[NUM[3],*,*],MATRIX[NUM[1],00169100
                    *,*]);                                              00169200
               GO TO ENDSUB;                                            00169300
          END  ELSE                                                     00169400
               IF TYPE[1]="V" AND TYPE[2]="V" AND TYPE[3]="V" THEN      00169500
          BEGIN                                                         00169600
               IF VECTOR[NUM[2],0]!VECTOR[NUM[3],0] THEN                00169700
          BEGIN                                                         00169800
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00169900
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00170000
               DUMB:=DUMB+1;                                            00170100
               GO TO ENDSUB;                                            00170200
          END;                                                          00170300
               VSUB(VECTOR[NUM[2],*],VECTOR[NUM[3],*],VECTOR[NUM[1],*]);00170400
               GO TO ENDSUB;                                            00170500
          END  ELSE                                                     00170600
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="P" THEN      00170700
          BEGIN                                                         00170800
               QSUB(POLY[NUM[2],*],POLY[NUM[3],*],POLY[NUM[1],*]);      00170900
               GO TO ENDSUB;                                            00171000
          END  ELSE                                                     00171100
               IF TYPE[1]="C" AND TYPE[2]="C" AND TYPE[3]="C" THEN      00171200
          BEGIN                                                         00171300
               CONST[NUM[1]]:=CONST[NUM[2]]-CONST[NUM[3]];              00171400
               GO TO ENDSUB;                                            00171500
          END  ELSE                                                     00171600
          BEGIN                                                         00171700
               I:=1;                                                    00171800
               I.[36:6]:=1;                                             00171900
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00172000
                    [26],MESSAGE[27],MESSAGE[28],5,"A",1,TYPE[1],MI,    00172100
                    TYPE[2],MI,TYPE[3])[:BRK];                          00172200
               DUMB:=DUMB+1;                                            00172300
               GO TO ENDSUB;                                            00172400
          END;                                                          00172500
ENDSUB:   END;                                                          00172600
               GO TO CHECK;                                             00172700
MULT:     BEGIN                                                         00172800
LABEL          ENDMULT;                                                 00172900
               SHIFT (RD[0],CPACE:=FINDCH(RD[0],TI)+1,GP:=GP-CPACE);    00173000
               SKIPBLANKS;                                              00173100
               SKIPOVER:=SKIPOVER+CPACE;                                00173200
               READ (RD[*],<A1,I1>,TYPE[3],NUM[3]);                     00173300
               SKIPOVER:=SKIPOVER+2;                                    00173400
               IF (TYPE[3]!"M" AND TYPE[3]!"V" AND TYPE[3]!"C" AND      00173500
                    TYPE[3]!"P") OR NUM[3]>9 THEN                       00173600
          BEGIN                                                         00173700
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00173800
                    [4],MESSAGE[5],2,"A",1,TYPE[3],NUM[3])[:BRK];       00173900
               DUMB:=DUMB+1;                                            00174000
               GO TO ENDMULT;                                           00174100
          END;                                                          00174200
               IF TYPE[1]="M" AND TYPE[2]="M" AND TYPE[3]="M" THEN      00174300
          BEGIN                                                         00174400
               IF MATRIX[NUM[2],0,1]!MATRIX[NUM[3],1,0] THEN            00174500
          BEGIN                                                         00174600
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00174700
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00174800
               DUMB:=DUMB+1;                                            00174900
               GO TO ENDMULT;                                           00175000
          END;                                                          00175100
              MMULT(MATRIX[NUM[2],*,*],MATRIX[NUM[3],*,*],MATRIX[NUM[1],00175200
                    *,*]);                                              00175300
               GO TO ENDMULT;                                           00175400
          END  ELSE                                                     00175500
               IF TYPE[1]="C" AND TYPE[2]="V" AND TYPE[3]="V" THEN      00175600
          BEGIN                                                         00175700
               IF VECTOR[NUM[2],0]!VECTOR[NUM[3],0] THEN                00175800
          BEGIN                                                         00175900
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00176000
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00176100
               DUMB:=DUMB+1;                                            00176200
               GO TO ENDMULT;                                           00176300
          END;                                                          00176400
               VDOT(VECTOR[NUM[2],*],VECTOR[NUM[3],*],CONST[NUM[1]]);   00176500
               GO TO ENDMULT;                                           00176600
          END  ELSE                                                     00176700
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="P" THEN      00176800
          BEGIN                                                         00176900
               IF POLY[NUM[2],0]+POLY[NUM[3],0]>20 THEN                 00177000
          BEGIN                                                         00177100
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00177200
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00177300
               DUMB:=DUMB+1;                                            00177400
               GO TO ENDMULT;                                           00177500
          END;                                                          00177600
               QMULT(POLY[NUM[2],*],POLY[NUM[3],*],POLY[NUM[1],*]);     00177700
               GO TO ENDMULT;                                           00177800
          END  ELSE                                                     00177900
               IF TYPE[1]="V" AND TYPE[2]="M" AND TYPE[3]="V" THEN      00178000
          BEGIN                                                         00178100
               IF MATRIX[NUM[2],0,1]!VECTOR[NUM[3],0] THEN              00178200
          BEGIN                                                         00178300
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00178400
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00178500
               DUMB:=DUMB+1;                                            00178600
               GO TO ENDMULT;                                           00178700
          END;                                                          00178800
              MTV(MATRIX[NUM[2],*,*],VECTOR[NUM[3],*],VECTOR[NUM[1],*]);00178900
               GO TO ENDMULT;                                           00179000
          END  ELSE                                                     00179100
               IF TYPE[1]="C" AND TYPE[2]="C" AND TYPE[3]="C" THEN      00179200
          BEGIN                                                         00179300
               CONST[NUM[1]]:=CONST[NUM[2]]|CONST[NUM[3]];              00179400
               GO TO ENDMULT;                                           00179500
          END  ELSE                                                     00179600
               IF TYPE[1]="M" AND TYPE[2]="C" AND TYPE[3]="M" THEN      00179700
          BEGIN                                                         00179800
               CTM(MATRIX[NUM[3],*,*],MATRIX[NUM[1],*,*],CONST[NUM[2]]);00179900
               GO TO ENDMULT;                                           00180000
          END  ELSE                                                     00180100
               IF TYPE[1]="M" AND TYPE[2]="M" AND TYPE[3]="C" THEN      00180200
          BEGIN                                                         00180300
               CTM(MATRIX[NUM[2],*,*],MATRIX[NUM[1],*,*],CONST[NUM[3]]);00180400
               GO TO ENDMULT;                                           00180500
          END  ELSE                                                     00180600
               IF TYPE[1]="V" AND TYPE[2]="C" AND TYPE[3]="V" THEN      00180700
          BEGIN                                                         00180800
               CTV(VECTOR[NUM[3],*],VECTOR[NUM[1],*],CONST[NUM[2]]);    00180900
               GO TO ENDMULT;                                           00181000
          END  ELSE                                                     00181100
               IF TYPE[1]="V" AND TYPE[2]="V" AND TYPE[3]="C" THEN      00181200
          BEGIN                                                         00181300
               CTV(VECTOR[NUM[2],*],VECTOR[NUM[1],*],CONST[NUM[3]]);    00181400
               GO TO ENDMULT;                                           00181500
          END  ELSE                                                     00181600
               IF TYPE[1]="P" AND TYPE[2]="C" AND TYPE[3]="P" THEN      00181700
          BEGIN                                                         00181800
               CTV(POLY[NUM[3],*],POLY[NUM[1],*],CONST[NUM[2]]);        00181900
               GO TO ENDMULT;                                           00182000
          END  ELSE                                                     00182100
               IF TYPE[1]="P" AND TYPE[2]="P" AND TYPE[3]="C" THEN      00182200
          BEGIN                                                         00182300
               CTV(POLY[NUM[2],*],POLY[NUM[1],*],CONST[NUM[3]]);        00182400
               GO TO ENDMULT;                                           00182500
          END  ELSE                                                     00182600
          BEGIN                                                         00182700
               I:=1;                                                    00182800
               I.[36:6]:=1;                                             00182900
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00183000
                    [26],MESSAGE[27],MESSAGE[28],5,"A",1,TYPE[1],MI,    00183100
                    TYPE[2],MI,TYPE[3])[:BRK];                          00183200
               DUMB:=DUMB+1;                                            00183300
               GO TO ENDMULT;                                           00183400
          END;                                                          00183500
ENDMULT:  END;                                                          00183600
               GO TO CHECK;                                             00183700
DVD:      BEGIN                                                         00183800
LABEL          ENDDVD;                                                  00183900
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],DI)+1,GP:=GP-CPACE);     00184000
               SKIPBLANKS;                                              00184100
               SKIPOVER:=SKIPOVER+CPACE;                                00184200
               READ (RD[*],<A1,I1>,TYPE[3],NUM[3]);                     00184300
               SKIPOVER:=SKIPOVER+2;                                    00184400
               IF (TYPE[3]!"M" AND TYPE[3]!"V" AND TYPE[3]!"C" AND      00184500
                    TYPE[3]!"P") OR NUM[3]>9 THEN                       00184600
          BEGIN                                                         00184700
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00184800
                    [4],MESSAGE[5],2,"A",1,TYPE[3],NUM[3])[:BRK];       00184900
               DUMB:=DUMB+1;                                            00185000
               GO TO ENDDVD;                                            00185100
          END;                                                          00185200
               IF TYPE[1]="C" AND TYPE[2]="C" AND TYPE[3]="C" THEN      00185300
          BEGIN                                                         00185400
               IF CONST[NUM[3]]=0 THEN                                  00185500
          BEGIN                                                         00185600
               I:=3;                                                    00185700
               I.[36:6]:=1;                                             00185800
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,3,6,MESSAGE[31],MESSAGE 00185900
                    [32],MESSAGE[33],2,"A",1,TYPE[3],NUM[3])[:BRK];     00186000
               DUMB:=DUMB+1;                                            00186100
               GO TO ENDDVD;                                            00186200
          END;                                                          00186300
               CONST[NUM[1]]:=CONST[NUM[2]]/CONST[NUM[3]];              00186400
               GO TO ENDDVD;                                            00186500
          END  ELSE                                                     00186600
          BEGIN                                                         00186700
               I:=1;                                                    00186800
               I.[36:6]:=1;                                             00186900
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00187000
                    [26],MESSAGE[27],MESSAGE[28],5,"A",1,TYPE[1],MI,    00187100
                    TYPE[2],MI,TYPE[3])[:BRK];                          00187200
               DUMB:=DUMB+1;                                            00187300
               GO TO ENDDVD;                                            00187400
          END;                                                          00187500
ENDDVD:   END;                                                          00187600
               GO TO CHECK;                                             00187700
DET:      BEGIN                                                         00187800
LABEL          ENDDET;                                                  00187900
               IF FINDCH(RD[0],LP)>GP THEN                              00188000
          BEGIN                                                         00188100
               SKIPOVER:=SKIPOVER+1;                                    00188200
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00188300
                    [9],1,"A",3,OPER)[:BRK];                            00188400
               DUMB:=DUMB+1;                                            00188500
               GO TO ENDDET;                                            00188600
          END;                                                          00188700
               IF FINDCH(RD[0],RP)>GP THEN                              00188800
          BEGIN                                                         00188900
               SKIPOVER:=SKIPOVER+3;                                    00189000
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00189100
                    [11],1,"A",1,LP)[:BRK];                             00189200
               DUMB:=DUMB+1;                                            00189300
               GO TO ENDDET;                                            00189400
          END;                                                          00189500
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00189600
               SKIPBLANKS;                                              00189700
               SKIPOVER:=SKIPOVER+CPACE;                                00189800
               READ (RD[*],<A1,I1>,TYPE[2],NUM[2]);                     00189900
               SKIPOVER:=SKIPOVER+2;                                    00190000
               IF (TYPE[2]!"M" AND TYPE[2]!"V" AND TYPE[2]!"C" AND      00190100
                    TYPE[2]!"P") OR NUM[2]>9 THEN                       00190200
          BEGIN                                                         00190300
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00190400
                    [4],MESSAGE[5],2,"A",1,TYPE[2],NUM[2])[:BRK];       00190500
               DUMB:=DUMB+1;                                            00190600
               GO TO ENDDET;                                            00190700
          END;                                                          00190800
               IF TYPE[1]="C" AND TYPE[2]="M" THEN                      00190900
          BEGIN                                                         00191000
               IF MATRIX[NUM[2],1,0]!MATRIX[NUM[2],0,1] THEN            00191100
          BEGIN                                                         00191200
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00191300
                    [21],2,"A",1,TYPE[2],NUM[2])[:BRK];                 00191400
               DUMB:=DUMB+1;                                            00191500
               GO TO ENDDET;                                            00191600
          END;                                                          00191700
               CONST[NUM[1]]:=MDET(MATRIX[NUM[2],*,*]);                 00191800
               GO TO ENDDET;                                            00191900
          END  ELSE                                                     00192000
          BEGIN                                                         00192100
               I:=1;                                                    00192200
               I.[36:6]:=1;                                             00192300
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00192400
                    [26],MESSAGE[27],MESSAGE[28],3,"A",1,TYPE[1],MI,    00192500
                    TYPE[2])[:BRK];                                     00192600
               DUMB:=DUMB+1;                                            00192700
               GO TO ENDDET;                                            00192800
          END;                                                          00192900
ENDDET:   END;                                                          00193000
               GO TO CHECK;                                             00193100
TRN:      BEGIN                                                         00193200
LABEL          ENDTRN;                                                  00193300
               IF FINDCH(RD[0],LP)>GP THEN                              00193400
          BEGIN                                                         00193500
               SKIPOVER:=SKIPOVER+1;                                    00193600
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00193700
                    [9],1,"A",3,OPER)[:BRK];                            00193800
               DUMB:=DUMB+1;                                            00193900
               GO TO ENDTRN;                                            00194000
          END;                                                          00194100
               IF FINDCH(RD[0],RP)>GP THEN                              00194200
          BEGIN                                                         00194300
               SKIPOVER:=SKIPOVER+3;                                    00194400
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00194500
                    [11],1,"A",1,LP)[:BRK];                             00194600
               DUMB:=DUMB+1;                                            00194700
               GO TO ENDTRN;                                            00194800
          END;                                                          00194900
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00195000
               SKIPBLANKS;                                              00195100
               SKIPOVER:=SKIPOVER+CPACE;                                00195200
               READ (RD[*],<A1,I1>,TYPE[2],NUM[2]);                     00195300
               SKIPOVER:=SKIPOVER+2;                                    00195400
               IF (TYPE[2]!"M" AND TYPE[2]!"V" AND TYPE[2]!"C" AND      00195500
                    TYPE[2]!"P") OR NUM[2]>9 THEN                       00195600
          BEGIN                                                         00195700
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00195800
                    [4],MESSAGE[5],2,"A",1,TYPE[2],NUM[2])[:BRK];       00195900
               DUMB:=DUMB+1;                                            00196000
               GO TO ENDTRN;                                            00196100
          END;                                                          00196200
               IF TYPE[1]="M" AND TYPE[2]="M" THEN                      00196300
          BEGIN                                                         00196400
               MTRAN(MATRIX[NUM[2],*,*],MATRIX[NUM[1],*,*]);            00196500
               GO TO ENDTRN;                                            00196600
          END  ELSE                                                     00196700
          BEGIN                                                         00196800
               I:=1;                                                    00196900
               I.[36:6]:=1;                                             00197000
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00197100
                    [26],MESSAGE[27],MESSAGE[28],3,"A",1,TYPE[1],MI,    00197200
                    TYPE[2])[:BRK];                                     00197300
               DUMB:=DUMB+1;                                            00197400
               GO TO ENDTRN;                                            00197500
          END;                                                          00197600
ENDTRN:   END;                                                          00197700
               GO TO CHECK;                                             00197800
EQU:      BEGIN                                                         00197900
LABEL          ENDEQU;                                                  00198000
               IF TYPE[1]="M" AND TYPE[2]="M" THEN                      00198100
          BEGIN                                                         00198200
               FOR I~1 STEP 1 UNTIL MATRIX[NUM[2],1,0] DO               00198300
               FOR J~1 STEP 1 UNTIL MATRIX[NUM[2],0,1] DO               00198400
               MATRIX[NUM[1],I,J]:=MATRIX[NUM[2],I,J];                  00198500
               MATRIX[NUM[1],1,0]:=MATRIX[NUM[2],1,0];                  00198600
               MATRIX[NUM[1],0,1]:=MATRIX[NUM[2],0,1];                  00198700
               GO TO ENDEQU;                                            00198800
          END  ELSE                                                     00198900
               IF TYPE[1]="V" AND TYPE[2]="V" THEN                      00199000
          BEGIN                                                         00199100
               FOR I~1 STEP 1 UNTIL VECTOR[NUM[2],0] DO                 00199200
               VECTOR[NUM[1],I]:=VECTOR[NUM[2],I];                      00199300
               VECTOR[NUM[1],0]:=VECTOR[NUM[2],0];                      00199400
               GO TO ENDEQU;                                            00199500
          END  ELSE                                                     00199600
               IF TYPE[1]="P" AND TYPE[2]="P" THEN                      00199700
          BEGIN                                                         00199800
               FOR I~1 STEP 1 UNTIL POLY[NUM[2],0] DO                   00199900
               POLY[NUM[1],I]:=POLY[NUM[2],I];                          00200000
               POLY[NUM[1],0]:=POLY[NUM[2],0];                          00200100
               GO TO ENDEQU;                                            00200200
          END  ELSE                                                     00200300
               IF TYPE[1]="C" AND TYPE[2]="C" THEN                      00200400
          BEGIN                                                         00200500
               CONST[NUM[1]]:=CONST[NUM[2]];                            00200600
               GO TO ENDEQU;                                            00200700
          END  ELSE                                                     00200800
          BEGIN                                                         00200900
               I:=1;                                                    00201000
               I.[36:6]:=1;                                             00201100
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00201200
                    [26],MESSAGE[27],MESSAGE[28],3,"A",1,TYPE[1],MI,    00201300
                    TYPE[2])[:BRK];                                     00201400
               DUMB:=DUMB+1;                                            00201500
               GO TO ENDEQU;                                            00201600
          END;                                                          00201700
ENDEQU:   END;                                                          00201800
               GO TO CHECK;                                             00201900
IDN:      BEGIN                                                         00202000
LABEL          ENDIDN;                                                  00202100
               IF TYPE[1]="M" THEN                                      00202200
          BEGIN                                                         00202300
               IF NUM[2]=0 AND NUM[3]=0 THEN                            00202400
          BEGIN                                                         00202500
               SKIPOVER:=SKIPOVER-1;                                    00202600
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00202700
                    [9],2,"A",1,TYPE[1],NUM[1])[:BRK];                  00202800
               DUMB:=DUMB+1;                                            00202900
               GO TO ENDIDN;                                            00203000
          END;                                                          00203100
               IF NUM[2]>NUM[3] THEN NUM[2]:=NUM[3];                    00203200
               MID(MATRIX[NUM[1],*,*],NUM[2]);                          00203300
               GO TO ENDIDN;                                            00203400
          END  ELSE                                                     00203500
          BEGIN                                                         00203600
               I:=1;                                                    00203700
               I.[36:6]:=1;                                             00203800
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00203900
                    [26],MESSAGE[27],MESSAGE[28],1,"A",1,TYPE[1])[:BRK];00204000
               DUMB:=DUMB+1;                                            00204100
               GO TO ENDIDN;                                            00204200
          END;                                                          00204300
ENDIDN:   END;                                                          00204400
               GO TO CHECK;                                             00204500
NULL:     BEGIN                                                         00204600
LABEL          ENDNULL;                                                 00204700
               IF TYPE[1]="M" THEN                                      00204800
               IF NUM[2]=0 AND NUM[3]=0 THEN                            00204900
          BEGIN                                                         00205000
          BEGIN                                                         00205100
               SKIPOVER:=SKIPOVER-1;                                    00205200
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00205300
                    [9],2,"A",1,TYPE[1],NUM[1])[:BRK];                  00205400
               DUMB:=DUMB+1;                                            00205500
               GO TO ENDNULL;                                           00205600
          END;                                                          00205700
               FOR I~1 STEP 1 UNTIL NUM[2] DO                           00205800
               FOR J~1 STEP 1 UNTIL NUM[3] DO                           00205900
               MATRIX[NUM[1],I,J]:=0;                                   00206000
               MATRIX[NUM[1],1,0]:=NUM[2];                              00206100
               MATRIX[NUM[1],0,1]:=NUM[3];                              00206200
               GO TO ENDNULL;                                           00206300
          END  ELSE                                                     00206400
               IF TYPE[1]="V" THEN                                      00206500
          BEGIN                                                         00206600
               IF NUM[2]=0 THEN                                         00206700
          BEGIN                                                         00206800
               SKIPOVER:=SKIPOVER-1;                                    00206900
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00207000
                    [9],2,"A",1,TYPE[1],NUM[1])[:BRK];                  00207100
               DUMB:=DUMB+1;                                            00207200
               GO TO ENDNULL;                                           00207300
          END;                                                          00207400
               FOR I~1 STEP 1 UNTIL NUM[2] DO                           00207500
               VECTOR[NUM[1],I]:=0;                                     00207600
               VECTOR[NUM[1],0]:=NUM[2];                                00207700
               GO TO ENDNULL;                                           00207800
          END  ELSE                                                     00207900
               IF TYPE[1]="P" THEN                                      00208000
          BEGIN                                                         00208100
               FOR I~1 STEP 1 UNTIL NUM[2] DO                           00208200
               POLY[NUM[1],I]:=0;                                       00208300
               POLY[NUM[1],0]:=NUM[2];                                  00208400
               GO TO ENDNULL;                                           00208500
          END  ELSE                                                     00208600
          BEGIN                                                         00208700
          CONST[NUM[1]]:=0;                                             00208800
               GO TO ENDNULL;                                           00208900
          END;                                                          00209000
ENDNULL:  END;                                                          00209100
               GO TO CHECK;                                             00209200
INV:      BEGIN                                                         00209300
LABEL          ENDINV;                                                  00209400
               IF FINDCH(RD[0],LP)>GP THEN                              00209500
          BEGIN                                                         00209600
               SKIPOVER:=SKIPOVER+1;                                    00209700
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00209800
                    [9],1,"A",3,OPER)[:BRK];                            00209900
               DUMB:=DUMB+1;                                            00210000
               GO TO ENDINV;                                            00210100
          END;                                                          00210200
               IF FINDCH(RD[0],RP)>GP THEN                              00210300
          BEGIN                                                         00210400
               SKIPOVER:=SKIPOVER+3;                                    00210500
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00210600
                    [11],1,"A",1,LP)[:BRK];                             00210700
               DUMB:=DUMB+1;                                            00210800
               GO TO ENDINV;                                            00210900
          END;                                                          00211000
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00211100
               SKIPBLANKS;                                              00211200
               SKIPOVER:=SKIPOVER+CPACE;                                00211300
               READ (RD[*],<A1,I1>,TYPE[2],NUM[2]);                     00211400
               SKIPOVER:=SKIPOVER+2;                                    00211500
               IF (TYPE[2]!"M" AND TYPE[2]!"V" AND TYPE[2]!"C" AND      00211600
                    TYPE[2]!"P") OR NUM[2]>9 THEN                       00211700
          BEGIN                                                         00211800
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00211900
                    [4],MESSAGE[5],2,"A",1,TYPE[2],NUM[2])[:BRK];       00212000
               DUMB:=DUMB+1;                                            00212100
               GO TO ENDINV;                                            00212200
          END;                                                          00212300
               IF TYPE[1]="M" AND TYPE[2]="M" THEN                      00212400
          BEGIN                                                         00212500
               IF MATRIX[NUM[2],1,0]!MATRIX[NUM[2],0,1] THEN            00212600
          BEGIN                                                         00212700
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00212800
                    [21],2,"A",1,TYPE[2],NUM[2])[:BRK];                 00212900
               DUMB:=DUMB+1;                                            00213000
               GO TO ENDINV;                                            00213100
          END;                                                          00213200
               MINV(MATRIX[NUM[2],*,*],MATRIX[NUM[1],*,*]);             00213300
               GO TO ENDINV;                                            00213400
          END  ELSE                                                     00213500
          BEGIN                                                         00213600
               I:=1;                                                    00213700
               I.[36:6]:=1;                                             00213800
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00213900
                    [26],MESSAGE[27],MESSAGE[28],3,"A",1,TYPE[1],MI,    00214000
                    TYPE[2])[:BRK];                                     00214100
               DUMB:=DUMB+1;                                            00214200
               GO TO ENDINV;                                            00214300
          END;                                                          00214400
ENDINV:   END;                                                          00214500
               GO TO CHECK;                                             00214600
EVEC:     BEGIN                                                         00214700
LABEL          ENDEVEC;                                                 00214800
               IF FINDCH(RD[0],LP)>GP THEN                              00214900
          BEGIN                                                         00215000
               SKIPOVER:=SKIPOVER+1;                                    00215100
               WRITE (TTOUT(STA),ERR,SKIPOVER,3,2,6,MESSAGE[8],MESSAGE  00215200
                    [9],1,"A",3,OPER)[:BRK];                            00215300
               DUMB:=DUMB+1;                                            00215400
               GO TO ENDEVEC;                                           00215500
          END;                                                          00215600
               IF FINDCH(RD[0],RP)>GP THEN                              00215700
          BEGIN                                                         00215800
               SKIPOVER:=SKIPOVER+3;                                    00215900
               WRITE (TTOUT(STA),ERR,SKIPOVER,5,2,6,MESSAGE[10],MESSAGE 00216000
                    [11],1,"A",1,LP)[:BRK];                             00216100
               DUMB:=DUMB+1;                                            00216200
               GO TO ENDEVEC;                                           00216300
          END;                                                          00216400
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],LP)+1,GP:=GP-CPACE);     00216500
               SKIPBLANKS;                                              00216600
               SKIPOVER:=SKIPOVER+CPACE;                                00216700
               READ (RD[*],<A1,I1>,TYPE[2],NUM[2]);                     00216800
               SKIPOVER:=SKIPOVER+2;                                    00216900
               IF (TYPE[2]!"M" AND TYPE[2]!"V" AND TYPE[2]!"C" AND      00217000
                    TYPE[2]!"P") OR NUM[2]>9 THEN                       00217100
          BEGIN                                                         00217200
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00217300
                    [4],MESSAGE[5],2,"A",1,TYPE[2],NUM[2])[:BRK];       00217400
               DUMB:=DUMB+1;                                            00217500
               GO TO ENDEVEC;                                           00217600
          END;                                                          00217700
               IF TYPE[1]="M" AND TYPE[2]="M" THEN                      00217800
          BEGIN                                                         00217900
               IF MATRIX[NUM[2],1,0]!MATRIX[NUM[2],0,1] THEN            00218000
          BEGIN                                                         00218100
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00218200
                    [21],2,"A",1,TYPE[2],NUM[2])[:BRK];                 00218300
               DUMB:=DUMB+1;                                            00218400
               GO TO ENDEVEC;                                           00218500
          END;                                                          00218600
               MEVE(MATRIX[NUM[2],*,*],MATRIX[NUM[1],*,*]);             00218700
               GO TO ENDEVEC;                                           00218800
          END  ELSE                                                     00218900
          BEGIN                                                         00219000
               I:=1;                                                    00219100
               I.[36:6]:=1;                                             00219200
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00219300
                    [26],MESSAGE[27],MESSAGE[28],3,"A",1,TYPE[1],MI,    00219400
                    TYPE[2])[:BRK];                                     00219500
               DUMB:=DUMB+1;                                            00219600
               GO TO ENDEVEC;                                           00219700
          END;                                                          00219800
ENDEVEC:  END;                                                          00219900
               GO TO CHECK;                                             00220000
CHECK:    END;                                                          00220100
               IF DUMPTHISONE THEN GO TO STOP;                          00220200
               IF DUMB!OLDDUMB THEN GO TO DUMREED;                      00220300
               WRITE (TTOUT(STA),DONE)[:BRK];                           00220400
               GO TO REED;                                              00220500
SOLVER:   BEGIN                                                         00220600
LABEL          ENDSOLVER;                                               00220700
               IF FINDCH(RD[0],TI)>GP THEN                              00220800
          BEGIN                                                         00220900
               SKIPOVER:=SKIPOVER+1;                                    00221000
               I:=2;                                                    00221100
               I.[36:6]:=1;                                             00221200
               WRITE(TTOUT(STA),ERR,SKIPOVER,I,2,6,MESSAGE[29],MESSAGE  00221300
                    [30],2,"A",1,TYPE[1],NUM[1])[:BRK];                 00221400
               DUMB:=DUMB+1;                                            00221500
               GO TO ENDSOLVER;                                         00221600
          END;                                                          00221700
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],TI)+1,GP:=GP-CPACE);     00221800
               SKIPBLANKS;                                              00221900
               READ (RD[*],<A1,I1>,TYPE[2],NUM[2]);                     00222000
               SKIPOVER:=SKIPOVER+CPACE+2;                              00222100
               IF (TYPE[2]!"M" AND TYPE[2]!"V" AND TYPE[2]!"C" AND      00222200
                    TYPE[2]!"P") OR NUM[2]>9 THEN                       00222300
          BEGIN                                                         00222400
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00222500
                    [4],MESSAGE[5],2,"A",1,TYPE[2],NUM[2])[:BRK];       00222600
               DUMB:=DUMB+1;                                            00222700
               GO TO ENDSOLVER;                                         00222800
          END;                                                          00222900
               IF FINDCH(RD[0],EQ)>GP THEN                              00223000
          BEGIN                                                         00223100
               SKIPOVER:=SKIPOVER+1;                                    00223200
               WRITE (TTOUT(STA),ERR,SKIPOVER,7,2,6,MESSAGE[15],MESSAGE 00223300
                    [16],2,"A",1,TYPE[2],NUM[2])[:BRK];                 00223400
               DUMB:=DUMB+1;                                            00223500
               GO TO ENDSOLVER;                                         00223600
          END;                                                          00223700
               SHIFT(RD[0],CPACE:=FINDCH(RD[0],EQ)+1,GP:=GP-CPACE);     00223800
               SKIPBLANKS;                                              00223900
               READ (RD[*],<A1,I1>,TYPE[3],NUM[3]);                     00224000
               SKIPOVER:=SKIPOVER+CPACE+2;                              00224100
               IF (TYPE[3]!"M" AND TYPE[3]!"V" AND TYPE[3]!"C" AND      00224200
                    TYPE[3]!"P") OR NUM[3]>9 THEN                       00224300
          BEGIN                                                         00224400
               WRITE (TTOUT(STA),ERR,SKIPOVER,2,3,6,MESSAGE[3],MESSAGE  00224500
                    [4],MESSAGE[5],2,"A",1,TYPE[3],NUM[3])[:BRK];       00224600
               DUMB:=DUMB+1;                                            00224700
               GO TO ENDSOLVER;                                         00224800
          END;                                                          00224900
               IF TYPE[1]="M" AND TYPE[2]="V" AND TYPE[3]="V" THEN      00225000
          BEGIN                                                         00225100
               IF MATRIX[NUM[1],1,0]!MATRIX[NUM[1],0,1] THEN            00225200
          BEGIN                                                         00225300
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00225400
                    [21],2,"A",1,TYPE[1],NUM[1])[:BRK];                 00225500
               DUMB:=DUMB+1;                                            00225600
               GO TO ENDSOLVER;                                         00225700
          END;                                                          00225800
               IF MATRIX[NUM[1],1,0]!VECTOR[NUM[3],0] THEN              00225900
          BEGIN                                                         00226000
               WRITE (TTOUT(STA),ERR,SKIPOVER,9,2,6,MESSAGE[20],MESSAGE 00226100
                    [21],2,"A",1,TYPE[3],NUM[3])[:BRK];                 00226200
               DUMB:=DUMB+1;                                            00226300
               GO TO ENDSOLVER;                                         00226400
          END;                                                          00226500
            SOLVE(MATRIX[NUM[1],*,*],VECTOR[NUM[2],*],VECTOR[NUM[3],*]);00226600
               GO TO ENDSOLVER;                                         00226700
          END  ELSE                                                     00226800
          BEGIN                                                         00226900
               I:=1;                                                    00227000
               I.[36:6]:=1;                                             00227100
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00227200
                    [26],MESSAGE[27],MESSAGE[28],5,"A",1,TYPE[1],MI,    00227300
                    TYPE[2],MI,TYPE[3])[:BRK];                          00227400
               DUMB:=DUMB+1;                                            00227500
               GO TO ENDSOLVER;                                         00227600
          END;                                                          00227700
ENDSOLVER:END;                                                          00227800
               IF DUMPTHISONE THEN GO TO STOP;                          00227900
               IF DUMB!OLDDUMB THEN GO TO DUMREED;                      00228000
               WRITE (TTOUT(STA),DONE)[:BRK];                           00228100
               GO TO REED;                                              00228200
ROOTS:    BEGIN                                                         00228300
LABEL          ENDROOTS;                                                00228400
               IF TYPE[1]="P" THEN                                      00228500
          BEGIN                                                         00228600
               PROOTS(POLY[NUM[1],*]);                                  00228700
               GO TO ENDROOTS;                                          00228800
          END  ELSE                                                     00228900
          BEGIN                                                         00229000
               I:=1;                                                    00229100
               I.[36:6]:=1;                                             00229200
               WRITE (TTOUT(STA),ERR,SKIPOVER,I,4,6,MESSAGE[25],MESSAGE 00229300
                    [26],MESSAGE[27],MESSAGE[28],1,"A",1,TYPE[1])[:BRK];00229400
               DUMB:=DUMB+1;                                            00229500
               GO TO ENDROOTS;                                          00229600
          END;                                                          00229700
ENDROOTS: END;                                                          00229800
               IF DUMPTHISONE THEN GO TO STOP;                          00229900
               IF DUMB!OLDDUMB THEN GO TO DUMREED;                      00230000
               WRITE (TTOUT(STA),DONE)[:BRK];                           00230100
               GO TO REED;                                              00230200
STOP:RERR:     WRITE (TTOUT(STA),FO3)[:BRK];                            00230300
               WATCHER[4]:=TIME(2);   PUTCHRIN(WATCHER[1],0,"0");   %SPY00230400
               WATCHER[1]:=TIME(0);  WATCHER[1].[12:6]~1;           %SPY00230500
               WATCHER[3]:=TIME(1);                                 %SPY00230600
%              READ (DISC[0],*,PNTR)[STOP];                         %SPY00230700
%              WRITE (DISC[PNTR+1],5,WATCHER[*]);                   %SPY00230800
%              WRITE (DISC[0],*,PNTR+1);                            %SPY00230900
%              LOCK (DISC,SAVE);                                    %SPY00231000
%              IF PNTR+1}50 THEN                                        00231100
%         BEGIN                                                         00231200
%FORMAT        F("CC EXECUTE PROGRAM/READER              .B2/12137-025",00231300
%                   "3 81184 WITKIN LOUIS;END.");                       00231400
%              WRITE (CONST[*],F);                                      00231500
%              ZIP WITH CONST[*];                                       00231600
%         END;                                                          00231700
BRK:      END.                                                          00231800
