BEGIN                                                                   00000100
COMMENT        CUBE LIBRARY IS A200001.  THIS VERSION                   00000200
               DATED 1/30/68;                                           00000300
INTEGER CCNT, J,Q,L,QL,FS,HP,F,IL,I,EP  , JL ;                          00000400
INTEGER MAXCCNT, MAXQCNT, START ;                                       00000500
INTEGER QCNT, M,    MDCNT, ML, P, MM, MR,    IMAX, MM1,ML3;             00000600
INTEGER COMPDECLARE;   LABEL SPACE1, SPACE2 ;                           00000700
INTEGER  VAR, RL, LASTOP , CPJ, CPX ;                                   00000800
LABEL BLANK, RDC,EOC, FINISHCOMPDECLARE  ;                              00000900
ALPHA CJ,DD, WORD1, WORD2 , WORD3 ;                                     00001000
SAVE ALPHA ARRAY C,G,E,H  [0: 80]  ;                                    00001100
SAVE ALPHA ARRAY  FMA, A [0:10];                                        00001200
SAVE ARRAY LPAR, FNLPAR,FNSTART      [0:10];                            00001300
SAVE  ALPHA ARRAY   D,S  , CPC     [0:80];                              00001400
SAVE  ALPHA ARRAY    DR, DI  [0:100 ];                                  00001500
                                                                        00001600
FILE CTRAN(2,10), LP 4(2,15),      CP 0(2,10);                          00001700
PROCEDURE CRUNCHOUT(  P, S  );                                          00001800
%  LAST CHAR IN CPC IS AT CPJ, EXCESS IS CPX = 72 - CPJ                 00001900
VALUE  P;  INTEGER  P;  ALPHA ARRAY S[0];                               00002000
    BEGIN   CPX ~ 72 - CPJ;                                             00002100
    IF CPX } P THEN BEGIN FOR I~1 STEP 1 UNTIL P  DO CPC[CPJ+I]~S[I] ;  00002200
         CPJ~CPJ+P; END                                                 00002300
    ELSE BEGIN      FOR I~1 STEP 1 UNTIL CPX  DO CPC[CPJ+I]~S[I];       00002400
    WRITE(LP,<72A1>, FOR I~1 STEP 1 UNTIL 72 DO CPC[I]);                00002500
    WRITE(CP,<72A1>, FOR I~1 STEP 1 UNTIL 72 DO CPC[I]);                00002600
    FOR I ~ CPX + 1 STEP 1 UNTIL P DO CPC[I-CPX]~S[I];                  00002700
    CPJ~ P- CPX ;                                                       00002800
    END;                                                                00002900
END PROCEDURE CRUNCHOUT ;                                               00003000
                                                                        00003100
PROCEDURE   MULDIV( C , IMAX );                                         00003200
%%  CONVERTS STATEMENTS WITH  + - | / TO REAL FORM    ******************00003300
VALUE  IMAX;  ALPHA ARRAY C[0];    INTEGER  IMAX;                       00003400
BEGIN                                                                   00003500
ALPHA CJ,DD;                                                            00003600
INTEGER  J;                                                             00003700
LABEL DIG3, DIG4;                                                       00003800
LABEL JUMP, MDHOP,           DIG1, DIG2;                                00003900
FOR J~3 STEP 1 UNTIL IMAX  DO BEGIN                                     00004000
   CJ~C[J];                                                             00004100
   IF CJ =" "  OR CJ ="="  THEN GO TO JUMP;                             00004200
   IF CJ =":"  THEN CJ~"~";                                             00004300
   M~M+1;  D[M]~CJ;                                                     00004400
   IF MDCNT=1 THEN GO TO MDHOP;                                         00004500
   IF CJ="~"  OR                                                        00004600
      CJ="+"  OR                                                        00004700
      CJ=";"  OR                                                        00004800
      CJ="-"  THEN BEGIN ML~M+1; GO TO JUMP; END;                       00004900
   IF CJ="|"  THEN BEGIN   S[3]~"M"; S[4]~"U"; S[5]~"L"; P~6;           00005000
   MM~ M; MDCNT~1; GO TO JUMP;    END;                                  00005100
   IF CJ="/"  THEN BEGIN   S[3]~"D"; S[4]~"I"; S[5]~"V"; P~6;           00005200
   MM~ M; MDCNT~1; GO TO JUMP;    END;                                  00005300
MDHOP:                                                                  00005400
                                                                        00005500
   IF CJ="|"  OR                                                        00005600
      CJ="/"  OR                                                        00005700
      CJ="+"  OR                                                        00005800
      CJ="-"  OR                                                        00005900
      CJ=";"  THEN BEGIN         MR~M-1;                                00006000
%%  CHECK FOR DIGIT OR DECIMAL AT START OF VARIABLE    *              *R00006100
                                                                        00006200
MM1 ~ MM - 1 ;                                                          00006300
IF D[ML    ].[42:2] =0 OR D[ML    ] = "."  THEN BEGIN                   00006400
FOR M~ML   STEP 1 UNTIL MM1  DO S[P~P+1]~D[M];S[P~P+1]~",";S[P~P+1]~"0";00006500
                                S[P~P+1]~"," ;GO TO DIG1; END;          00006600
                                                                        00006700
FOR M~ML   STEP 1 UNTIL MM1  DO S[P~P+1]~D[M];S[P~P+1]~"R";S[P~P+1]~",";00006800
FOR M~ML   STEP 1 UNTIL MM1  DO S[P~P+1]~D[M];S[P~P+1]~"I";S[P~P+1]~",";00006900
                                                                        00007000
DIG1:                                                                   00007100
IF D[MM+1  ].[42:2] =0 OR D[MM+1  ] = "."  THEN BEGIN                   00007200
FOR M~MM+1 STEP 1 UNTIL MR   DO S[P~P+1]~D[M];S[P~P+1]~",";S[P~P+1]~"0";00007300
                                S[P~P+1]~"," ;GO TO DIG2; END;          00007400
                                                                        00007500
FOR M~MM+1 STEP 1 UNTIL MR   DO S[P~P+1]~D[M];S[P~P+1]~"R";S[P~P+1]~",";00007600
FOR M~MM+1 STEP 1 UNTIL MR   DO S[P~P+1]~D[M];S[P~P+1]~"I";S[P~P+1]~",";00007700
DIG2:                                                                   00007800
    D[ML]~"Q";                                                          00007900
        WRITE ( FMA[*],<I3>,QCNT~QCNT+1);                               00008000
      READ ( FMA[*],<3A1>, D[ML+1], D[ML+2], D[ML+3]);                  00008100
     FOR I~1,2,3 DO IF D[ML+I]=" " THEN D[ML+I]~"0";                    00008200
FOR I~ML+4 STEP 1 UNTIL 80   DO D[I]~" ";  ML3~ML+3;                    00008300
FOR M~ML   STEP 1 UNTIL ML3  DO S[P~P+1]~D[M];S[P~P+1]~"R";S[P~P+1]~",";00008400
FOR M~ML   STEP 1 UNTIL ML3  DO S[P~P+1]~D[M];S[P~P+1]~"I";S[P~P+1]~")";00008500
                                                           S[P~P+1]~";";00008600
%%   WRITE PROCEDURE FOR  | OR /     ***********************************00008700
CRUNCHOUT( P, S );                                                      00008800
D[ML+4]~CJ;      M~ML+4;                                                00008900
IF CJ="+" OR CJ ="-" THEN ML~ ML+5;                                     00009000
   IF CJ="|"  THEN BEGIN   S[3]~"M"; S[4]~"U"; S[5]~"L"; P~6;           00009100
     MM~ ML+4;                                                          00009200
                   GO TO JUMP; END;                                     00009300
   IF CJ="/"  THEN BEGIN   S[3]~"D"; S[4]~"I"; S[5]~"V"; P~6;           00009400
     MM~ ML+4;                                                          00009500
                   GO TO JUMP; END;                                     00009600
MDCNT~0;                                                                00009700
          END;                                                          00009800
JUMP:                                                                   00009900
   IF CJ=";"  THEN BEGIN                                                00010000
P~RL~0;  VAR~1;      LASTOP~0;                                          00010100
FOR I~3 STEP 1 UNTIL M   DO BEGIN   DD~D[I];                            00010200
IF DD = "~" OR                                                          00010300
   DD = "+" OR                                                          00010400
   DD = "-" OR                                                          00010500
   DD = ";"    THEN BEGIN                                               00010600
         VAR~0;                                                         00010700
         DR[P~P+1]~"R";   DI[P]~"I";                                    00010800
          IF  RL = 1 THEN P~P-1;          RL~0;                         00010900
         IF I - LASTOP = 1 THEN P~P-1;  LASTOP ~ I;                     00011000
DIG3:                                                                   00011100
         DR[P~P+1]~ DD ;  DI[P]~ DD ;                                   00011200
         GO TO DIG4;  END;                                              00011300
IF VAR=0  THEN BEGIN                                                    00011400
         IF  DD.[42:2]=0 OR DD="." THEN       RL~1  ELSE RL~0;  END;    00011500
VAR~1;                                                                  00011600
IF RL = 0 THEN GO TO DIG3;                                              00011700
         DR[P~P+1]~DD;  DI[P]~ "0";                                     00011800
DIG4:                                                                   00011900
END I;                                                                  00012000
%%  WRITE  RE  IM PARTS OF STATEMENTS WITH + -       *******************00012100
CRUNCHOUT( P, DR );                                                     00012200
CRUNCHOUT( P, DI );                                                     00012300
IF QCNT > MAXQCNT THEN MAXQCNT ~ QCNT;                                  00012400
QCNT ~ 0;                                                               00012500
         M~2; END;                                                      00012600
END J;                                                                  00012700
END PROCEDURE MULDIV  ;                                                 00012800
                                                                        00012900
                                                                        00013000
                                                                        00013100
QCNT ~ 0; M~0; S[1]~" ";  S[2]~"C";  S[6]~"(";                          00013200
   D[1]~"C";  D[2]~" ";  M~2;                                           00013300
MDCNT~0;                                                                00013400
MAXQCNT ~0;  START ~ 0;                                                 00013500
MAXCCNT~0;             CPJ~0;                                           00013600
H[1]~H[2]~" ";    H[3]~"C";                                             00013700
G[1]~E[1]~"C";  G[2]~E[2]~" ";   E[3]~"C";                              00013800
L~F~0;               CCNT~0;       Q~2;                                 00013900
RDC:                                                                    00014000
READ(CTRAN,10,A[*])[EOC];                                               00014100
READ( A[*], < A1 >, CJ );                                               00014200
IF COMPDECLARE = 0 AND START = 1 AND CJ ! "C" THEN BEGIN                00014300
    WRITE(CP, 10,A[*]);                                                 00014400
    WRITE(LP, 10,A[*]); GO TO RDC; END;                                 00014500
READ(A[*],<80(A1)>, FOR J~1 STEP 1 UNTIL 80 DO C[J]);                   00014600
IF COMPDECLARE = 1 THEN GO TO FINISHCOMPDECLARE  ;                      00014700
IF START = 0 THEN BEGIN                                                 00014800
    READ( A[*],< A5 >, WORD3 );                                         00014900
    IF WORD3 = "BEGIN" THEN BEGIN   START ~ 1 ;                         00015000
    WRITE( CP,<"BEGIN",/,"$$A CTRBASIC",/,"     ",75A1>,                00015100
    FOR J~6 STEP 1 UNTIL 80 DO C[J] );                                  00015200
    WRITE( LP,<"BEGIN",/,"$$A CTRBASIC",/,"     ",75A1>,                00015300
    FOR J~6 STEP 1 UNTIL 80 DO C[J] );                                  00015400
    GO TO RDC;                                                          00015500
    END;                                                                00015600
    WRITE(CP,10,A[*]);    WRITE(LP,10,A[*]);   GO TO RDC;               00015700
END;                                                                    00015800
IF C[2]=" " THEN BEGIN                                                  00015900
%%%  REDUCTION OF COMPLEX STATEMENT    *********************************00016000
WRITE(LP,<  "% ",78(A1)   >, FOR I~3 STEP 1 UNTIL  80 DO C[I]);         00016100
WRITE(CP,<  "% ",78(A1)   >, FOR I~3 STEP 1 UNTIL  80 DO C[I]);         00016200
FOR J~3 STEP 1 UNTIL 72 DO BEGIN                         CJ~C[J];       00016300
                                                                        00016400
IF CJ =":" THEN CJ ~"~";                                                00016500
IF CJ ="="  OR                                                          00016600
   CJ = " "  THEN GO TO BLANK;                                          00016700
         Q~Q+1;   G[Q]~CJ;                                              00016800
   IF CJ="~"  OR                                                        00016900
      CJ="|"  OR                                                        00017000
      CJ="/"  OR                                                        00017100
      CJ="+"  OR                                                        00017200
      CJ="-"  THEN  QL~Q+1;                                             00017300
IF CJ = "(" THEN                                                        00017400
   BEGIN LPAR[L~L+1]~Q;                                                 00017500
         IF Q-QL>1  AND Q - LPAR[L-1 ] > 1 THEN  %%THIS IS A FN (       00017600
             BEGIN FNLPAR[F~F+1]~Q;                                     00017700
                   IF QL>LPAR[L-1] THEN FNSTART[F]~QL                   00017800
                                ELSE    FNSTART[F]~LPAR[L-1]+1;         00017900
             END;                                                       00018000
   END;                                                                 00018100
IF CJ = ")" THEN   %%   E~   (...........)                              00018200
   BEGIN                                                                00018300
        WRITE ( FMA[*],<I3>,CCNT~CCNT+1);                               00018400
      READ ( FMA[*],<3A1>,E[4],  E[5],  E[6]  );                        00018500
      FOR I~4,5,6 DO IF E[I] = " " THEN E[I]~"0";                       00018600
         E[7]~"~";  EP~7;  IL~LPAR[L];                                  00018700
         FOR I~IL+1    STEP 1 UNTIL Q-1 DO E[EP~EP+1]~G[I];             00018800
         E[EP~EP+1]~";";                                                00018900
         MULDIV( E, EP );                                               00019000
         IF IL > FNLPAR[F]   THEN  %%  NOT A FN                         00019100
             BEGIN FOR I~0,1,2,3 DO G[IL+I]~E[3+I];                     00019200
                   Q~IL+3;   L~L-1;  GO TO BLANK;                       00019300
             END;                                                       00019400
         %% AT THIS POINT THERE IS A FN AND IL = FNLPAR[F]              00019500
         HP~3;   FS~FNSTART[F];                                         00019600
         FOR I~ FS STEP 1 UNTIL IL DO H[HP~HP+1]~G[I];                  00019700
         G[ FS]~"C";                                                    00019800
        WRITE ( FMA[*],<I3>,CCNT~CCNT+1);                               00019900
      READ ( FMA[*],<3A1>,G[FS+1],  G[FS+2],  G[FS+3]  );               00020000
     FOR I~1,2,3 DO IF G[FS+I]=" " THEN G[FS+I]~"0";                    00020100
         Q~ FS+3;                                                       00020200
         FOR I~3,4,5,6 DO H[HP~HP+1]~E[I];                              00020300
              H[HP~HP+1]~"R"; H[HP~HP+1]~",";                           00020400
         FOR I~3,4,5,6 DO H[HP~HP+1]~E[I];                              00020500
              H[HP~HP+1]~"I"; H[HP~HP+1]~",";                           00020600
         FOR I~0,1,2,3 DO H[HP~HP+1]~ G[ FS+I];                         00020700
              H[HP~HP+1]~"R"; H[HP~HP+1]~",";                           00020800
         FOR I~0,1,2,3 DO H[HP~HP+1]~ G[ FS+I];                         00020900
              H[HP~HP+1]~"I"; H[HP~HP+1]~")";                           00021000
              H[HP~HP+1]~";";                                           00021100
CRUNCHOUT(HP, H  );                                                     00021200
         L~L-1;  F~F-1;                                                 00021300
   END;                                                                 00021400
IF CJ = ";"  THEN                                                       00021500
   BEGIN                                                                00021600
         IF CCNT> MAXCCNT  THEN MAXCCNT ~ CCNT;                         00021700
         CCNT ~ 0 ;                                                     00021800
         MULDIV(  G , Q  );                                             00021900
         L~F  ~0;                Q~2;                                   00022000
   END;                                                                 00022100
BLANK:                                                                  00022200
END J;                                                                  00022300
IF CPJ ! 0 THEN BEGIN                                                   00022400
    WRITE(CP,<72A1>, FOR I~1 STEP 1 UNTIL CPJ DO CPC[I]);               00022500
    WRITE(LP,<72A1>, FOR I~1 STEP 1 UNTIL CPJ DO CPC[I]);               00022600
    CPJ ~ 0;                                                            00022700
END;                                                                    00022800
GO TO RDC;                                                              00022900
END;                                                                    00023000
                                                                        00023100
%%  AT THIS POINT C[1] = C AND C[2] ! " "  E G  "COMMENT"  *************00023200
%%%%    CHECK FOR COMPLEX DECLARATIONS                                  00023300
                                                                        00023400
READ( A[*], < A6, A2 >, WORD1, WORD2 );                                 00023500
IF WORD1 = "COMPLE"  AND WORD2 = "X " THEN BEGIN                        00023600
   COMPDECLARE ~ 1;  I~0;           JL ~ 72;                            00023700
   WRITE(LP,<"REAL   %% THESE VARIABLES WERE DECLARED COMPLEX">);       00023800
   WRITE(CP,<"REAL   %% THESE VARIABLES WERE DECLARED COMPLEX">);       00023900
   FOR J~9 STEP 1 UNTIL 72 DO BEGIN CJ~C[J];                            00024000
         IF CJ = " " THEN GO TO SPACE1 ;                                00024100
         IF CJ = "," THEN BEGIN I~I+1; DR[I]~"R"; DI[I]~"I"; END;       00024200
         IF CJ = ";" THEN BEGIN I~I+1; DR[I]~"R"; DI[I]~"I";            00024300
                                I~I+1; DR[I]~","; DI[I]~";";            00024400
                   JL ~ J ;   J ~ 72;                                   00024500
                   COMPDECLARE ~ 0; GO TO SPACE1;  END;                 00024600
         I~I+1;  DR[I]~DI[I]~CJ;                                        00024700
         SPACE1 :                                                       00024800
   END;                                                                 00024900
   WRITE(LP,<"  ",70A1>, FOR J~1 STEP 1 UNTIL I DO DR[J] );             00025000
   WRITE(CP,<"  ",70A1>, FOR J~1 STEP 1 UNTIL I DO DR[J] );             00025100
   WRITE(LP,<"  ",70A1>, FOR J~1 STEP 1 UNTIL I DO DI[J] );             00025200
   WRITE(CP,<"  ",70A1>, FOR J~1 STEP 1 UNTIL I DO DI[J] );             00025300
    WRITE ( LP,<80A1>,FOR J ~ 1 STEP 1 UNTIL JL DO " ", FOR J ~ J+1     00025400
    STEP 1 UNTIL 80 DO C[J]);                                           00025500
    WRITE ( CP,<80A1>,FOR J ~ 1 STEP 1 UNTIL JL DO " ", FOR J ~ J+1     00025600
    STEP 1 UNTIL 80 DO C[J]);                                           00025700
   GO TO RDC;                                                           00025800
END;                                                                    00025900
%%%  HERE  IF C[1] = "C", C[2] ! " " OR TAIL OF COMPLEXDECLARE  ********00026000
FINISHCOMPDECLARE  :                                                    00026100
IF COMPDECLARE = 1 THEN BEGIN I~0;        JL~72;                        00026200
   FOR J~1 STEP 1 UNTIL 72 DO BEGIN CJ~C[J];                            00026300
         IF CJ = " " THEN GO TO SPACE2 ;                                00026400
         IF CJ = "," THEN BEGIN I~I+1; DR[I]~"R"; DI[I]~"I"; END;       00026500
         IF CJ = ";" THEN BEGIN I~I+1; DR[I]~"R"; DI[I]~"I";            00026600
                                I~I+1; DR[I]~","; DI[I]~";";            00026700
                   JL ~ J ;   J ~ 72;                                   00026800
                   COMPDECLARE ~ 0; GO TO SPACE2;  END;                 00026900
         I~I+1;  DR[I]~DI[I]~CJ;                                        00027000
         SPACE2 :                                                       00027100
   END;                                                                 00027200
   WRITE(LP,<"  ",70A1>, FOR J~1 STEP 1 UNTIL I DO DR[J] );             00027300
   WRITE(CP,<"  ",70A1>, FOR J~1 STEP 1 UNTIL I DO DR[J] );             00027400
   WRITE(LP,<"  ",70A1>, FOR J~1 STEP 1 UNTIL I DO DI[J] );             00027500
   WRITE(CP,<"  ",70A1>, FOR J~1 STEP 1 UNTIL I DO DI[J] );             00027600
    WRITE ( LP,<80A1>,FOR J ~ 1 STEP 1 UNTIL JL DO " ", FOR J ~ J+1     00027700
    STEP 1 UNTIL 80 DO C[J]);                                           00027800
    WRITE ( CP,<80A1>,FOR J ~ 1 STEP 1 UNTIL JL DO " ", FOR J ~ J+1     00027900
    STEP 1 UNTIL 80 DO C[J]);                                           00028000
   GO TO RDC;                                                           00028100
END;                                                                    00028200
%%  AT THIS POINT C[1] = C AND C[2] ! " "  E G  "COMMENT"  *************00028300
    WRITE(CP, 10,A[*]);                                                 00028400
    WRITE(LP, 10,A[*]); GO TO RDC;                                      00028500
EOC:                                                                    00028600
WRITE(LP,<"%%  CTRANCARDS:  MAXQCNT = ",I3,X5,"MAXCCNT = ",I3>,MAXQCNT, 00028700
         MAXCCNT);                                                      00028800
WRITE(CP,<"%%  CTRANCARDS:  MAXQCNT = ",I3,X5,"MAXCCNT = ",I3>,MAXQCNT, 00028900
         MAXCCNT);                                                      00029000
WRITE( CP,< "%%  CTRANCARDS:  PROCESSOR, I/O TIMES IN SEC.  ",2I10>,    00029100
    TIME(2)/60, TIME(3)/60 );                                           00029200
WRITE( LP,< "%%  CTRANCARDS:  PROCESSOR, I/O TIMES IN SEC.  ",2I10>,    00029300
    TIME(2)/60, TIME(3)/60 );                                           00029400
END.                                                                    00029500
