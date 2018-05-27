BEGIN                                                                   00000100
COMMENT        CUBE LIBRARY NUMBER IS A200002.  PROGRAM NAME IS         00000200
               "CTRAN/EXECUT".  THIS VERSION DATED 4/29/68;             00000300
INTEGER QCNT, M,    MDCNT, ML, P, MM, MR, Z, IMAX;                      00000400
INTEGER  VAR, RL, LASTOP ;                                              00000500
INTEGER CCNT, J,Q,L,QL,FS,HP,F,IL,I,EP ,JL;                             00000600
INTEGER MAXCCNT, MAXQCNT, START ;                                       00000700
INTEGER COMPDECLARE;   LABEL SPACE1, SPACE2 ;                           00000800
INTEGER MF, PREVCCZ,SUMMZ;                                              00000900
INTEGER ZPI ;                                                           00001000
LABEL FINFULL;                                                          00001100
LABEL BLANK, RDC,EOC, FINISHCOMPDECLARE  ;                              00001200
ALPHA CJ,DD, WORD1, WORD2 , WORD3,SAVENAME;                             00001300
SAVE  ALPHA ARRAY ZP,DR, DI  [0:100 ];                                  00001400
SAVE  ALPHA ARRAY   D,S            [0:80];                              00001500
SAVE ALPHA ARRAY C,G,E,H  [0: 80]  ;                                    00001600
SAVE ALPHA ARRAY FMA, A, DOLCD, PREVCC ,ZP2   [0:10];                   00001700
BOOLEAN LISTSUPPRESSED, FULLLIST;                                       00001800
BOOLEAN ZPSTART,ZPEND ;                                                 00001900
FORMAT FMTNOLIST ( "$ CARD  " ), FMT72A1 ( 72A1 ), FMT80A1 ( 80A1 ),    00002000
    FMTBB70A1 ( "  ", 70A1 ), FMTBB69A1 ( "  ", 69A1 );                 00002100
STREAM PROCEDURE SLASH(A);                                              00002200
 BEGIN  DI~A;  DS~ 1 LIT "/"; END;                                      00002300
STREAM PROCEDURE FILL1(A);                                              00002400
 BEGIN                                                                  00002500
    DI~A;                                                               00002600
    DS~ 48 LIT "; ALGOL FILE CARD =       COMPLX0         DISK S";      00002700
    DS~  8 LIT "ERIAL   ";                                              00002800
 END;                                                                   00002900
STREAM PROCEDURE FILL2(A);                                              00003000
 BEGIN                                                                  00003100
    DI~A;                                                               00003200
    DS~ 32 LIT "; REMOVE  COMPLX0         ; END.";                      00003300
 END;                                                                   00003400
STREAM PROCEDURE FILCRD(A);                                             00003500
          BEGIN                                                         00003600
               DI~A; DS~20 LIT "$ CARD LIST         ";                  00003700
               DS~ 60 LIT " ";                                          00003800
          END;                                                          00003900
STREAM PROCEDURE XFERS(A,B,N)  ;                                        00004000
 COMMENT                                                                00004100
     THIS STREAM PROCEDURE TRANSFERS N WORDS FROM A ROW IN ARRAY (OR    00004200
BUFFER) A TO A ROW IN ARRAY (OR BUFFER) B, WHERE THE USER MUST SPECIFY  00004300
THE SUBSCRIPTS(OR RELATIVE ADDRESS) OF BOTH A AND B. W.ZWISLER,OCT 64;  00004400
    VALUE N  ;  BEGIN  LOCAL C;   ;  SI ~ LOC N  ;  SI ~ SI + 6  ;      00004500
    DI ~ LOC C  ;  DI ~ DI + 7  ;  DS ~ 1 CHR  ;  SI ~ A  ;  DI ~ B  ;  00004600
    2(C(DS ~ 32 WDS))  ;  DS ~ N WDS  ;  END  XFERS  ;                  00004700
SAVE ARRAY LPAR, FNLPAR,FNSTART      [0:10];                            00004800
                                                                        00004900
FILE CP DISK SERIAL [20:1000] "COMPLX" "CP" (3,10,60);                  00005000
FILE CTRAN  (2,10), LP 4(2,15);                                         00005100
                                                                        00005200
                                                                        00005300
PROCEDURE   MULDIV( C , IMAX );                                         00005400
%%  CONVERTS STATEMENTS WITH  + - | / TO REAL FORM    ******************00005500
VALUE  IMAX;  ALPHA ARRAY C[0];    INTEGER  IMAX;                       00005600
BEGIN                                                                   00005700
ALPHA CJ,DD;                                                            00005800
INTEGER  J;                                                             00005900
LABEL DIG3, DIG4;                                                       00006000
LABEL JUMP, MDHOP,           DIG1, DIG2;                                00006100
WRITE(CP,<  "%% ",77(A1)   >, FOR I~3 STEP 1 UNTIL  IMAX   DO C[I]);    00006200
IF IMAX > 77   THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00006300
FOR J~3 STEP 1 UNTIL IMAX  DO BEGIN                                     00006400
   CJ~C[J];                                                             00006500
   IF CJ =" "  OR CJ ="="  THEN GO TO JUMP;                             00006600
   IF CJ =":"  THEN CJ~"~";                                             00006700
   M~M+1;  D[M]~CJ;                                                     00006800
   IF MDCNT=1 THEN GO TO MDHOP;                                         00006900
   IF CJ="~"  OR                                                        00007000
      CJ="+"  OR                                                        00007100
      CJ=";"  OR                                                        00007200
      CJ="-"  THEN BEGIN ML~M+1; GO TO JUMP; END;                       00007300
   IF CJ="|"  THEN BEGIN   S[3]~"M"; S[4]~"U"; S[5]~"L"; P~6;           00007400
   MM~ M; MDCNT~1; GO TO JUMP;    END;                                  00007500
   IF CJ="/"  THEN BEGIN   S[3]~"D"; S[4]~"I"; S[5]~"V"; P~6;           00007600
   MM~ M; MDCNT~1; GO TO JUMP;    END;                                  00007700
MDHOP:                                                                  00007800
                                                                        00007900
   IF CJ="|"  OR                                                        00008000
      CJ="/"  OR                                                        00008100
      CJ="+"  OR                                                        00008200
      CJ="-"  OR                                                        00008300
      CJ=";"  THEN BEGIN         MR~M-1;                                00008400
%%  CHECK FOR DIGIT OR DECIMAL AT START OF VARIABLE    *              *R00008500
MF ~ MM - 1;                                                            00008600
IF D[ML    ].[42:2] =0 OR D[ML    ] = "."  THEN BEGIN                   00008700
FOR M ~ ML STEP 1 UNTIL MF DO S[P~P+1]~D[M];S[P~P+1]~",";S[P~P+1]~"0";  00008800
                              S[P~P+1]~","; GO TO DIG1; END;            00008900
                                                                        00009000
FOR M ~ ML STEP 1 UNTIL MF DO S[P~P+1]~D[M];S[P~P+1]~"R";S[P~P+1]~",";  00009100
FOR M ~ ML STEP 1 UNTIL MF DO S[P~P+1]~D[M];S[P~P+1]~"I";S[P~P+1]~",";  00009200
                                                                        00009300
DIG1:                                                                   00009400
IF D[MM+1  ].[42:2] =0 OR D[MM+1  ] = "."  THEN BEGIN                   00009500
FOR M~MM+1 STEP 1 UNTIL MR   DO S[P~P+1]~D[M];S[P~P+1]~",";S[P~P+1]~"0";00009600
                                S[P~P+1]~"," ;GO TO DIG2; END;          00009700
                                                                        00009800
FOR M~MM+1 STEP 1 UNTIL MR   DO S[P~P+1]~D[M];S[P~P+1]~"R";S[P~P+1]~",";00009900
FOR M~MM+1 STEP 1 UNTIL MR   DO S[P~P+1]~D[M];S[P~P+1]~"I";S[P~P+1]~",";00010000
DIG2:                                                                   00010100
    D[ML]~"Q";                                                          00010200
        WRITE ( FMA[*],<I3>,QCNT~QCNT+1);                               00010300
      READ ( FMA[*],<3A1>, D[ML+1], D[ML+2], D[ML+3]);                  00010400
     FOR I~1,2,3 DO IF D[ML+I]=" " THEN D[ML+I]~"0";                    00010500
FOR I~ML+4 STEP 1 UNTIL 80   DO D[I]~" ";                               00010600
MF ~ ML + 3;                                                            00010700
FOR M ~ ML STEP 1 UNTIL MF DO S[P~P+1]~D[M];S[P~P+1]~"R";S[P~P+1]~",";  00010800
FOR M ~ ML STEP 1 UNTIL MF DO S[P~P+1]~D[M];S[P~P+1]~"I";S[P~P+1]~")";  00010900
                                                           S[P~P+1]~";";00011000
%%   WRITE PROCEDURE FOR  + OR /     *************************%         00011100
WRITE(CP, FMTBB70A1  , FOR I ~1 STEP 1 UNTIL P  DO S[I]);               00011200
IF  P   > 70   THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00011300
D[ML+4]~CJ;      M~ML+4;                                                00011400
IF CJ="+" OR CJ ="-" THEN ML~ ML+5;                                     00011500
   IF CJ="|"  THEN BEGIN   S[3]~"M"; S[4]~"U"; S[5]~"L"; P~6;           00011600
     MM~ ML+4;                                                          00011700
                   GO TO JUMP; END;                                     00011800
   IF CJ="/"  THEN BEGIN   S[3]~"D"; S[4]~"I"; S[5]~"V"; P~6;           00011900
     MM~ ML+4;                                                          00012000
                   GO TO JUMP; END;                                     00012100
MDCNT~0;                                                                00012200
          END;                                                          00012300
JUMP:                                                                   00012400
   IF CJ=";"  THEN BEGIN                                                00012500
P~RL~0;  VAR~1;      LASTOP~0;                                          00012600
FOR I~3 STEP 1 UNTIL M   DO BEGIN   DD~D[I];                            00012700
IF DD = "~" OR                                                          00012800
   DD = "+" OR                                                          00012900
   DD = "-" OR                                                          00013000
   DD = ";"    THEN BEGIN                                               00013100
         VAR~0;                                                         00013200
         DR[P~P+1]~"R";   DI[P]~"I";                                    00013300
          IF  RL = 1 THEN P~P-1;          RL~0;                         00013400
         IF I - LASTOP = 1 THEN P~P-1;  LASTOP ~ I;                     00013500
DIG3:                                                                   00013600
         DR[P~P+1]~ DD ;  DI[P]~ DD ;                                   00013700
         GO TO DIG4;  END;                                              00013800
IF VAR=0  THEN BEGIN                                                    00013900
         IF  DD.[42:2]=0 OR DD="." THEN       RL~1  ELSE RL~0;  END;    00014000
VAR~1;                                                                  00014100
IF RL = 0 THEN GO TO DIG3;                                              00014200
         DR[P~P+1]~DD;  DI[P]~ "0";                                     00014300
DIG4:                                                                   00014400
END I;                                                                  00014500
%%  WRITE  RE  IM PARTS OF STATEMENTS WITH + -       *******************00014600
WRITE( CP, FMTBB69A1, FOR I ~ 1 STEP 1 UNTIL P DO DR[I] );              00014700
IF  P   > 69   THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00014800
WRITE( CP, FMTBB69A1, FOR I ~ 1 STEP 1 UNTIL P DO DI[I] );              00014900
IF  P   > 69   THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00015000
IF QCNT > MAXQCNT THEN MAXQCNT ~ QCNT;                                  00015100
QCNT ~ 0;                                                               00015200
         M~2; END;                                                      00015300
END J;                                                                  00015400
END PROCEDURE MULDIV  ;                                                 00015500
                                                                        00015600
                                                                        00015700
J ~ 1000000 - TIME(1)/60 ;                                              00015800
WRITE(A[*],<"00", I6 >, J );                                            00015900
A[0].[ 6: 6] ~ "C" ;                                                    00016000
SAVENAME ~ A[0] ;                                                       00016100
FILL CP WITH * , SAVENAME ;                                             00016200
QCNT ~ 0; M~0; S[1]~" ";  S[2]~"C";  S[6]~"(";                          00016300
   D[1]~"C";  D[2]~" ";  M~2;                                           00016400
MDCNT~0;                                                                00016500
MAXQCNT ~0;                                                             00016600
ZPEND ~ ZPSTART ~ FALSE ;    ZPI ~ -1 ;                                 00016700
               FILCRD(DOLCD);                                           00016800
LISTSUPPRESSED ~ FULLLIST ~ FALSE;                                      00016900
MAXCCNT~0;                                                              00017000
H[1]~H[2]~" ";    H[3]~"C";                                             00017100
G[1]~E[1]~"C";  G[2]~E[2]~" ";   E[3]~"C";                              00017200
L~F~0;               CCNT~0;       Q~2;                                 00017300
START ~ 0;                                                              00017400
Z~0;                                                                    00017500
RDC:                                                                    00017600
COMMENT...IF COMPILER LISTING HAS BEEN SUPPRESSED, RESTORE USERS        00017700
          LISTING CONVENTIONS HERE.;                                    00017800
IF LISTSUPPRESSED THEN                                                  00017900
BEGIN WRITE( CP, 10, DOLCD[*] );   Z ~ Z + 1;    LISTSUPPRESSED ~ FALSE;00018000
END;                                                                    00018100
COMMENT...READ A CARD OF THE DECK TO BE TRANSLATED.;                    00018200
READ(CTRAN  ,10,A[*])[EOC];                                             00018300
IF START = 0 THEN BEGIN                                                 00018400
    READ( A[*], <A5>, WORD3 );                                          00018500
    IF WORD3 = "BEGIN" THEN BEGIN                                       00018600
    WRITE(CP,<"BEGIN">);                                                00018700
Z~Z+1;                                                                  00018800
READ( A[*], FMT80A1, FOR J ~ 1 STEP 1 UNTIL 80 DO C[J] );               00018900
COMMENT...LEAVE 2 RECORDS FOR TRANSLATOR COMMENTS TO BE GENERATED AT    00019000
          THE END OF THE TRANSLATION.;                                  00019100
SUMMZ ~ Z;   WRITE( CP, 10, A[*] );    WRITE( CP, 10, A[*] );           00019200
Z ~ Z + 2;                                                              00019300
COMMENT...SEARCH FOR THE OCCURRENCE, ON THE INITIAL "BEGIN" CARD, OF    00019400
          THE CHARACTERS "%FULL", INDICATING THAT THE USER WANTS THE    00019500
          COMPILER TO LIST ALL OF THE TRANSLATED CARDS AND CTRBASIC.    00019600
          THE %FULL MUST                                                00019700
          BE THE FIRST 5 NON-BLANK CHARACTERS ON THE CARD AFTER "BEGIN";00019800
FOR I ~ 6 STEP 1 UNTIL 72 DO IF C[I] = "%" THEN  IF C[I+1] = "F" AND    00019900
C[I+2] = "U" AND C[I+3] = C[I+4] AND C[I+4] ="L" THEN                   00020000
BEGIN FULLLIST ~ TRUE;                                                  00020100
WRITE(CP,<"$$A CTRBASIC">);  Z~Z+1;                                     00020200
                          GO TO FINFULL;                                00020300
END;                                                                    00020400
WRITE(CP,<"$ CARD  ",/,"$$A CTRBASIC">);  WRITE(CP,10,DOLCD[*]); Z~Z+3; 00020500
FINFULL:                                                                00020600
WRITE( CP, < X5, 75A1 >, FOR I ~ 6 STEP 1 UNTIL 80 DO C[I] );  Z~Z+1;   00020700
     START~1;                                                           00020800
    GO TO RDC;                                                          00020900
    END;                                                                00021000
END;                                                                    00021100
                                                                        00021200
READ( A[*], FMT80A1, FOR J ~ 1 STEP 1 UNTIL 80 DO C[J] );               00021300
IF COMPDECLARE = 1 THEN GO TO FINISHCOMPDECLARE  ;                      00021400
IF C[1]!"C" THEN BEGIN                                                  00021500
COMMENT...TO HERE, IF THE FIRST CHARACTER ON THE CURRENT INPUT CARD IS  00021600
          NOT "C".  TEST FOR CARD COLUMNS 1 AND 2 = "$ ".  IF SO, SAVE  00021700
          THE CONTENTS OF THE CARD.;                                    00021800
IF C[1] = "$" AND C[2] ! "$" THEN XFERS( A[0], DOLCD[0], 10 );          00021900
    WRITE(CP, 10,A[*]);                                                 00022000
Z~Z+1;                                                                  00022100
    GO TO RDC;                                                          00022200
END;                                                                    00022300
IF C[2]=" " THEN BEGIN                                                  00022400
%%%  REDUCTION OF COMPLEX STATEMENT    *********************************00022500
WRITE(CP,<  "% ",78(A1)   >, FOR I~3 STEP 1 UNTIL  80 DO C[I]);         00022600
Z~Z+1;                                                                  00022700
COMMENT...ON OPTION, CAUSE COMPILER LISTING TO BE SUPPRESSED UNTIL THE  00022800
          NEXT INPUT CARD IS READ.;                                     00022900
IF NOT FULLLIST THEN                                                    00023000
BEGIN WRITE( CP, FMTNOLIST );    Z ~ Z + 1;   LISTSUPPRESSED ~ TRUE;    00023100
END;                                                                    00023200
FOR J~3 STEP 1 UNTIL 72 DO BEGIN                         CJ~C[J];       00023300
                                                                        00023400
IF CJ =":" THEN CJ ~"~";                                                00023500
IF CJ ="="  OR                                                          00023600
   CJ = " "  THEN GO TO BLANK;                                          00023700
         Q~Q+1;   G[Q]~CJ;                                              00023800
   IF CJ="~"  OR                                                        00023900
      CJ="|"  OR                                                        00024000
      CJ="/"  OR                                                        00024100
      CJ="+"  OR                                                        00024200
      CJ="-"  THEN  QL~Q+1;                                             00024300
IF CJ = "(" THEN                                                        00024400
   BEGIN LPAR[L~L+1]~Q;                                                 00024500
         IF Q-QL>1  AND Q - LPAR[L-1 ] > 1 THEN  %%THIS IS A FN (       00024600
             BEGIN FNLPAR[F~F+1]~Q;                                     00024700
                   IF QL>LPAR[L-1] THEN FNSTART[F]~QL                   00024800
                                ELSE    FNSTART[F]~LPAR[L-1]+1;         00024900
             END;                                                       00025000
   END;                                                                 00025100
IF CJ = ")" THEN   %%   E~   (...........)                              00025200
   BEGIN                                                                00025300
        WRITE ( FMA[*],<I3>,CCNT~CCNT+1);                               00025400
      READ ( FMA[*],<3A1>,E[4],  E[5],  E[6]  );                        00025500
      FOR I~4,5,6 DO IF E[I] = " " THEN E[I]~"0";                       00025600
         E[7]~"~";  EP~7;  IL~LPAR[L];                                  00025700
MF ~ Q - 1;    FOR I ~ IL + 1 STEP 1 UNTIL MF DO E[EP~EP+1] ~ G[I];     00025800
         E[EP~EP+1]~";";                                                00025900
         MULDIV( E, EP );                                               00026000
         IF IL > FNLPAR[F]   THEN  %%  NOT A FN                         00026100
             BEGIN FOR I~0,1,2,3 DO G[IL+I]~E[3+I];                     00026200
                   Q~IL+3;   L~L-1;  GO TO BLANK;                       00026300
             END;                                                       00026400
         %% AT THIS POINT THERE IS A FN AND IL = FNLPAR[F]              00026500
         HP~3;   FS~FNSTART[F];                                         00026600
         FOR I~ FS STEP 1 UNTIL IL DO H[HP~HP+1]~G[I];                  00026700
         G[ FS]~"C";                                                    00026800
        WRITE ( FMA[*],<I3>,CCNT~CCNT+1);                               00026900
      READ ( FMA[*],<3A1>,G[FS+1],  G[FS+2],  G[FS+3]  );               00027000
     FOR I~1,2,3 DO IF G[FS+I]=" " THEN G[FS+I]~"0";                    00027100
         Q~ FS+3;                                                       00027200
         FOR I~3,4,5,6 DO H[HP~HP+1]~E[I];                              00027300
              H[HP~HP+1]~"R"; H[HP~HP+1]~",";                           00027400
         FOR I~3,4,5,6 DO H[HP~HP+1]~E[I];                              00027500
              H[HP~HP+1]~"I"; H[HP~HP+1]~",";                           00027600
         FOR I~0,1,2,3 DO H[HP~HP+1]~ G[ FS+I];                         00027700
              H[HP~HP+1]~"R"; H[HP~HP+1]~",";                           00027800
         FOR I~0,1,2,3 DO H[HP~HP+1]~ G[ FS+I];                         00027900
              H[HP~HP+1]~"I"; H[HP~HP+1]~")";                           00028000
              H[HP~HP+1]~";";                                           00028100
    WRITE( CP, FMT72A1, FOR I ~ 1 STEP 1 UNTIL HP DO H[I] );            00028200
IF  HP  > 72   THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00028300
         L~L-1;  F~F-1;                                                 00028400
   END;                                                                 00028500
IF CJ = ";"  THEN                                                       00028600
   BEGIN                                                                00028700
         IF CCNT> MAXCCNT  THEN MAXCCNT ~ CCNT;                         00028800
         CCNT ~ 0 ;                                                     00028900
         MULDIV(  G , Q  );                                             00029000
         L~F  ~0;                Q~2;                                   00029100
   END;                                                                 00029200
BLANK:                                                                  00029300
END J;                                                                  00029400
GO TO RDC;                                                              00029500
END;                                                                    00029600
                                                                        00029700
%%  AT THIS POINT C[1] = C AND C[2] ! " "  E G  "COMMENT"  *************00029800
%%%%    CHECK FOR COMPLEX DECLARATIONS                                  00029900
                                                                        00030000
READ( A[*], < A6, A2 >, WORD1, WORD2 );                                 00030100
IF WORD1 = "COMPLE"  AND WORD2 = "X " THEN BEGIN                        00030200
   COMPDECLARE ~ 1;  I~0;      JL~72;                                   00030300
   WRITE(CP,<"REAL   %% THESE VARIABLES WERE DECLARED COMPLEX">);       00030400
Z~Z+1;                                                                  00030500
   FOR J~9 STEP 1 UNTIL 72 DO BEGIN CJ~C[J];                            00030600
         IF CJ = " " THEN GO TO SPACE1 ;                                00030700
         IF CJ = "," THEN BEGIN I~I+1; DR[I]~"R"; DI[I]~"I"; END;       00030800
         IF CJ = ";" THEN BEGIN I~I+1; DR[I]~"R"; DI[I]~"I";            00030900
                                I~I+1; DR[I]~","; DI[I]~";";            00031000
                   JL~J;  J~72;                                         00031100
                   COMPDECLARE ~ 0; GO TO SPACE1;  END;                 00031200
         I~I+1;  DR[I]~DI[I]~CJ;                                        00031300
         SPACE1 :                                                       00031400
   END;                                                                 00031500
WRITE( CP, FMTBB70A1, FOR J ~ 1 STEP 1 UNTIL I DO DR[J] );              00031600
IF  I   >  70  THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00031700
WRITE( CP, FMTBB70A1, FOR J ~ 1 STEP 1 UNTIL I DO DI[J] );              00031800
IF  I   >  70  THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00031900
WRITE( CP, FMT80A1, FOR J ~ 1 STEP 1 UNTIL JL DO " ", FOR J ~ JL + 1    00032000
    STEP 1 UNTIL 80 DO C[J] );                                          00032100
Z~Z+1;                                                                  00032200
   GO TO RDC;                                                           00032300
END;                                                                    00032400
%%%  HERE  IF C[1] = "C", C[2] ! " " OR TAIL OF COMPLEXDECLARE  ********00032500
FINISHCOMPDECLARE  :                                                    00032600
IF COMPDECLARE = 1 THEN BEGIN I~0;              JL~72;                  00032700
   FOR J~1 STEP 1 UNTIL 72 DO BEGIN CJ~C[J];                            00032800
         IF CJ = " " THEN GO TO SPACE2 ;                                00032900
         IF CJ = "," THEN BEGIN I~I+1; DR[I]~"R"; DI[I]~"I"; END;       00033000
         IF CJ = ";" THEN BEGIN I~I+1; DR[I]~"R"; DI[I]~"I";            00033100
                                I~I+1; DR[I]~","; DI[I]~";";            00033200
                   JL~J;  J~72;                                         00033300
                   COMPDECLARE ~ 0; GO TO SPACE2;  END;                 00033400
         I~I+1;  DR[I]~DI[I]~CJ;                                        00033500
         SPACE2 :                                                       00033600
   END;                                                                 00033700
WRITE( CP, FMTBB70A1, FOR J ~ 1 STEP 1 UNTIL I DO DR[J] );              00033800
IF  I   >  70  THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00033900
WRITE( CP, FMTBB70A1, FOR J ~ 1 STEP 1 UNTIL I DO DI[J] );              00034000
IF  I   >  70  THEN Z~Z+2 ELSE Z~Z+1;   %  .............................00034100
WRITE( CP, FMT80A1, FOR J ~ 1 STEP 1 UNTIL JL DO " ", FOR J ~ JL + 1    00034200
    STEP 1 UNTIL 80 DO C[J] );                                          00034300
Z~Z+1;                                                                  00034400
   GO TO RDC;                                                           00034500
END;                                                                    00034600
%%  AT THIS POINT C[1] = C AND C[2] ! " "  E G  "COMMENT"  *************00034700
COMMENT...TEST FOR A CONTROL CARD, THAT IS, A CARD WITH "CC" IN COLUMNS 00034800
          1 AND 2.;                                                     00034900
IF C[2] = "C" THEN                                                      00035000
BEGIN                                                                   00035100
    IF NOT ZPSTART THEN                                                 00035200
    BEGIN                                                               00035300
         XFERS( A[0], ZP[0], 9 ) ;                                      00035400
FILL1(FMA);                                                             00035500
         FMA[4].[ 6:42] ~ SAVENAME ;                                    00035600
SLASH(FMA[4]);                                                          00035700
         XFERS ( FMA[0],ZP[9], 7 );                                     00035800
         ZPI~16;                                                        00035900
         ZPSTART~TRUE;                                                  00036000
         GO TO RDC;                                                     00036100
    END;                                                                00036200
                                                                        00036300
                                                                        00036400
    FOR I ~ 3 STEP 1 UNTIL 72 DO                                        00036500
    IF C[I] = "D" AND C[I+1] = "A" AND C[I+2] ="T" AND C[I+3] = "A" THEN00036600
    BEGIN                                                               00036700
FILL2(A);                                                               00036800
           A[2].[ 6:42] ~ SAVENAME ;                                    00036900
SLASH(A[2]);                                                            00037000
    XFERS(A[0],  ZP[ZPI],4);                                            00037100
    ZPEND~TRUE;                                                         00037200
    END;                                                                00037300
    IF ZPEND THEN GO TO RDC ;                                           00037400
READ(A[*],<A2,A6>,WORD1,WORD2);                                         00037500
WRITE(FMA[*],<"; ",A6>,WORD2);                                          00037600
XFERS(FMA[*],ZP[ZPI],1);                                                00037700
XFERS(A[1],ZP[ZPI+1    ],8);  ZPI~ZPI+9;                                00037800
GO TO RDC;                                                              00037900
END;                                                                    00038000
WRITE(CP,10,A[*]);   Z~Z+1;                                             00038100
GO TO RDC;                                                              00038200
EOC:                                                                    00038300
COMMENT...WRITE OUT TRANSLATOR COMMENTS IN PREVIOUSLY PRESERVED RECORDS;00038400
WRITE(CP[SUMMZ], < "%%  CTRANEXECUTE: MAXQCNT = ", I3, X5, "MAXCCNT = ",00038500
    I3, X5, "Z = ", I6 >, MAXQCNT, MAXCCNT, Z );                        00038600
WRITE( CP[SUMMZ+1], < "%%  CTRANEXECUTE: PROCESSOR, I/O TIMES IN SEC. ",00038700
    2I10 >, TIME( 2 )/60, TIME( 3 )/60 );                               00038800
LOCK (CP);                                                              00038900
ZIP WITH ZP[*] ;                                                        00039000
END.                                                                    00039100
