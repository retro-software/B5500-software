BEGIN  FILE IN CRA 0(2,10);  FILE OUT LPA 1(2,15);                      00000100
COMMENT        PROGRAM - ANALYTIC DIFFERENTIATION,                      00000200
               CUBE LIBRARY NUMBER IS D400001.                          00000300
               THIS VERSION DATED 6/20/67;                              00000400
LABEL L0,GETOUT;                                                        00000500
ARRAY P[1:15];                                                          00000600
INTEGER ARRAY       T[0:99];                                            00000700
ALPHA ORDER,VAR,CCC;                                                    00000800
BOOLEAN BOO,BOVAL,FEVAL;                                                00000900
INTEGER ALF,BET,GAM   ,J,BETM1,LPC,RPC,NUT,CALC,VARS,GG,I,N,            00001000
CC,KK,JSAVE,DEL,MU,SOVE,TOG,K;                                          00001100
ALPHA ARRAY PER[0:10],PQ[1:1],ZP[1:1] ,ORD[2:10],V[1:15],               00001200
 VER[0:10],  SAB[1:2,1:20],TAB[1:20];                                   00001300
FORMAT FFF5(X8,"I HAVE RUN OUT OF MEMORY-SUGGEST YOU INPUT A G# DATA CAR00001400
D OR SUBDIVIDE ORIGINAL EXPRESSION"),                                   00001500
       FFF4(A2,I4),                                                     00001600
       FFF2(X8,"TOUGH LUCK, I CANT DIFF.  ",A6,"  , ADD IT TO MY LIST AN00001700
D YOUR IN BUSINESS"),F8(/X8,"FUNCTION"),FFF3(///),                      00001800
    F26(////X8,"VALUE OF CONSTANTS FOR EVALUATING FUNCTION AND DERIVATIV00001900
ES"),F27(/X10,"SET ",I2/(X12,4(A6,X1,"=",X1,E16.9,X1)/)),               00002000
       F6(X8,"EITHER TOO LARGE AN INTEGER OR IDENTIFIER"),              00002100
       F1(72A1),F2(X8,"NUMBER OF ( AND ) IN FUNCTION ARE NOT EQUAL"),   00002200
  F3((X8,72A1/)),                                                       00002300
        F4(X8,"YOU HAVE USED AN OPERATOR WHICH IS NOT ALLOWED"),        00002400
F5(X8,"BETTER CHECK SYNTAX NEAR COLUMN ",I4),                           00002500
                    F7((X8,72A1/)),                                     00002600
                  F9(/X8,A3,X2,"DERIVATIVE WITH RESPECT TO  ",          00002700
9(A6,","));                                                             00002800
FILL ORD[*] WITH "2ND","3RD","4TH","5TH","6TH","7TH","8TH","9TH";       00002900
FILL V[*] WITH 10,9,8,7,7,6,6,5,5,4,4;                                  00003000
FILL P[*] WITH ".","*","%","|","/","-","+","(",")","@","#";             00003100
 FILL TAB[*] WITH "LOG","EXP","SIN","SINH","COSH","COS","TAN","TANH",   00003200
     "COT","COTH","SEC","CSC","SECH","CSCH";  FILL SAB[1,*] WITH        00003300
                0,  "EXP","COS","COSH","SINH","SIN","SEC","SECH","CSC", 00003400
    "CSCH","TAN","CSC","SECH","CSCH";                                   00003500
 FILL SAB[2,*] WITH 0,0,0,0,0,0,0,0,0,0,"SEC","COT","TANH","COTH";      00003600
GG~ 400;                                                                00003700
READ(CRA[NO],FFF4,CCC,CC); IF CCC.[36:12]="G#" THEN BEGIN GG~CC;        00003800
READ(CRA ,FFF4,CCC,CC) END;                                             00003900
IF GG<100 THEN GG~100;     IF GG>1020 THEN GG~1020;                     00004000
L0:                                                                     00004100
READ(CRA,/,CALC,VARS,      N,FOR I~1 STEP 1 UNTIL N DO VER[I])[GETOUT]; 00004200
  BEGIN                                                                 00004300
ALPHA ARRAY IDENT,TDENT[0:VARS]; ARRAY VAL[0:VARS,0:CALC],DER[0:GG ],   00004400
SS[0:GG ] , S[0:GG ] ,M[1:GG ,1:3]    ,L[-2:GG ],QQ[1:1,0:GG];          00004500
LABEL ST1,          SKIP,START,L1,L2,L3,L4,L5,L6,L7,L8,ST2,L9,          00004600
      L2P,L2PP,REP,L99,L98;                                             00004700
DEFINE D=M#,                                                            00004800
       LOOP = FOR I~1 STEP 1 UNTIL 72 DO#, A=S[BET]#, G=S[BETM1]#;      00004900
DEFINE TEST= IF A="+" OR A="-" OR A="|" OR A="/" OR A="*"OR A="("       00005000
 OR A="#"          OR A=" "#,              Q= QQ[1, #;                  00005100
LIST LIST1(LOOP S[BET+I]),                                              00005200
                 LIST3(ORDER,FOR I~1 STEP 1 UNTIL KK-1 DO PER[I]),      00005300
     LIST2(FOR I~1 STEP 1 UNTIL  BET DO  SS[I]);                        00005400
PROCEDURE BLANC(VARS,TDENT,IDENT); VALUE VARS;INTEGER VARS;             00005500
ALPHA ARRAY TDENT,IDENT[*]; BEGIN INTEGER I; LABEL KNOT;                00005600
FOR I~1 STEP 1 UNTIL VARS DO BEGIN TDENT[I]~"      ";                   00005700
TDENT[I].[42:6]~IDENT[I].[42:6];                                        00005800
 IF IDENT[I].[36:6]="0" THEN GO TO KNOT;                                00005900
TDENT[I].[36:6]~IDENT[I].[36:6];                                        00006000
 IF IDENT[I].[30:6]="0" THEN GO TO KNOT;                                00006100
TDENT[I].[30:6]~IDENT[I].[30:6];                                        00006200
 IF IDENT[I].[24:6]="0" THEN GO TO KNOT;                                00006300
TDENT[I].[24:6]~IDENT[I].[24:6];                                        00006400
 IF IDENT[I].[18:6]="0" THEN GO TO KNOT;                                00006500
TDENT[I].[18:6]~IDENT[I].[18:6];                                        00006600
 IF IDENT[I].[12:6]="0" THEN GO TO KNOT;                                00006700
TDENT[I].[12:6]~IDENT[I].[12:6];                                        00006800
KNOT: END END BLANC;                                                    00006900
REAL PROCEDURE PRIOR(Z);ALPHA Z;BEGIN LABEL L1; INTEGER J;              00007000
FOR J~1 STEP 1 UNTIL 11 DO BEGIN IF P[J]=Z THEN BEGIN PRIOR~V[J];       00007100
GO TO L1 END END; BOO~TRUE;L1: END;                                     00007200
PROCEDURE SCOPE;BEGIN LABEL L1,L2; L1: IF PRIOR(L[ALF-2]) { GAM THEN GO 00007300
 TO L2; I~I+1; FOR J~1 STEP 1 UNTIL 3 DO M[I,J]~L[ALF-J];               00007400
L[ALF-3].[30:6 ]~"!" ;L[ALF-3].[36:12]~I ;  ALF~ALF-2;GO TO L1; L2:END; 00007500
 PROCEDURE ROE(A,B,C,D);ARRAY A[*,*];INTEGER B,C,D; BEGIN A[B,C].[30:6]~00007600
 "!"; A[B,C].[36:12] ~ D; END;                                          00007700
REAL PROCEDURE SEAR(A,I); VALUE A,I; ALPHA A;  INTEGER I;               00007800
                                          BEGIN  LABEL L1,L2;           00007900
INTEGER K;  FOR K~1 STEP 1 UNTIL VARS DO BEGIN IF IDENT[K] = A THEN     00008000
GO TO L1 END; SEAR ~0; BOVAL ~ TRUE; GO TO L2; L1:SEAR ~VAL[K,I];       00008100
L2: END SEAR;                                                           00008200
PROCEDURE EVAL(Z);VALUE Z; INTEGER Z; BEGIN INTEGER I,J;                00008300
DEFINE T1  =D[J,1]#,T2  =D[J,2]#,T3=D[J,3]#,T4=DER[J]#;LABEL L1,L2;     00008400
               REAL FIR,SEC;                                            00008500
FORMAT F25(X8,"CANT EVALUATE NUMERICALLY- NO VALUE GIVEN FOR   ",A5),   00008600
       F24(X8,"CANT EVALUATE NUMERICALLY- DUE TO FUNCTION USED"),       00008700
       F23(/X8,"VALUE FOR SET  ",I2," OF CONSTANTS  ",E17.8);           00008800
BOVAL~FALSE; FOR I~ 1 STEP 1 UNTIL CALC DO BEGIN                        00008900
 FOR J~1 STEP 1 UNTIL Z DO BEGIN IF T2!"." THEN BEGIN IF T1.[30:6]!"!"  00009000
AND T1> 9 THEN FIR ~ SEAR(T1,I);IF BOVAL THEN BEGIN WRITE(LPA,F25,  T1);00009100
 GO TO L1 END; IF T1<10 THEN FIR~ T1; IF T1.[30:6]="!" THEN FIR ~       00009200
DER[T1.[36:12]] END; IF T3.[30:6]! "!" AND D[J,3]>9 THEN SEC~SEAR(T3,I);00009300
 IF BOVAL THEN BEGIN WRITE(LPA,F25,  T3); GO TO L1 END;                 00009400
 IF T3<10 THEN SEC ~ T3; IF T3.[30:6]="!" THEN SEC~DER[T3.[36:12]];     00009500
 IF T2 = "+" THEN T4~ FIR + SEC ELSE                                    00009600
 IF T2 = "-" OR T2="%"           THEN T4~ FIR - SEC ELSE                00009700
 IF T2 = "|" THEN T4~ FIR | SEC ELSE                                    00009800
 IF T2 = "/" THEN T4~ FIR / SEC ELSE                                    00009900
 IF T2 = "*" THEN T4~ FIR * SEC ELSE                                    00010000
 IF T2 = "." THEN BEGIN IF T1 ="EXP" THEN T4~ EXP(SEC) ELSE             00010100
 IF T1="LOG" THEN T4~ LN(SEC) ELSE IF T1="SIN" THEN T4~ SIN(SEC) ELSE   00010200
 IF T1="COS" THEN T4~ COS(SEC) ELSE IF T1="TAN" THEN T4~ SIN(SEC)/      00010300
 COS(SEC) ELSE IF T1="COT" THEN T4~ COS(SEC)/SIN(SEC) ELSE              00010400
 IF T1="CSC" THEN T4~ 1/SIN(SEC) ELSE IF T1="SEC" THEN T4~ 1/COS(SEC)   00010500
 ELSE IF T1="SINH" THEN T4~ (EXP(SEC)-EXP(-SEC))/2 ELSE                 00010600
      IF T1="COSH" THEN T4~ (EXP(SEC)+EXP(-SEC))/2 ELSE                 00010700
      IF T1="TANH" THEN T4~ (EXP(SEC)-EXP(-SEC))/(EXP(SEC)+EXP(-SEC))   00010800
ELSE IF  T1="SECH" THEN T4~ 2/(EXP(SEC)+EXP(-SEC))  ELSE                00010900
     IF  T1="CSCH" THEN T4~ 2/(EXP(SEC)-EXP(-SEC))  ELSE                00011000
     IF  T1="COTH" THEN T4~(EXP(SEC)+EXP(-SEC))/(EXP(SEC)-EXP(-SEC))    00011100
ELSE                      GO TO L2    END ELSE                          00011200
 GO TO L2     END; WRITE(LPA,F23, I,DER[Z]                              00011300
                                    ) END; GO TO L1; L2:WRITE(LPA,F24); 00011400
 L1: END EVAL;                                                          00011500
 PROCEDURE DIFF; BEGIN                                                  00011600
                       LABEL L1,L2,L3;ALF~M[I,3]; L1: IF ALF =VAR THEN  00011700
 BEGIN ALF ~1; GO TO L2 END; IF ALF.[30:6]!"!" THEN BEGIN ALF ~0; GO TO 00011800
 L2 END; ALF ~Q ALF.[36:12]]; L2: IF MU < 0 THEN GO TO L3; BET~ALF;     00011900
 IF MU=1 THEN GO TO L3; ALF~ M[I,1];MU~-1;GO TO L1; L3: END;            00012000
         BOOLEAN STREAM PROCEDURE FINDWORD(A,B,C);                      00012100
           VALUE B,C;                                                   00012200
           BEGIN                                                        00012300
             COMMENT FIND B IN ARRAY A. SEARCH C WORDS.                 00012400
                   MAX C { 60 WORDS. WORKS ONLY ON INT OR ALPA WORDS.;  00012500
             TALLY~0;  DI~LOC B;  SI~A;                                 00012600
             C(IF 8 SC = DC THEN BEGIN TALLY~1; JUMP OUT END;           00012700
               DI~DI-8);                                                00012800
             FINDWORD~TALLY;                                            00012900
           END FINDWORD;                                                00013000
         BOOLEAN STREAM PROCEDURE ALPHAW(A);                            00013100
           BEGIN                                                        00013200
           COMMENT A = WORD WITH CHAR. RIGHT JUSTIFIED.                 00013300
                   TRUE IF CHAR. IS A DIGIT OR LETTER.;                 00013400
           SI~A;  SI~SI+7;                                              00013500
           IF SC = ALPHA THEN TALLY~1 ELSE TALLY~0;                     00013600
           ALPHAW ~ TALLY;                                              00013700
         END ALPHAW;                                                    00013800
FEVAL ~ TRUE;                                                           00013900
IF CALC > 0  THEN BEGIN                                                 00014000
                  READ(CRA,/,FOR I~ 1 STEP 1 UNTIL VARS DO [IDENT[I],   00014100
FOR J~ 1 STEP 1 UNTIL CALC DO VAL[I,J]]);                               00014200
 BLANC(VARS,TDENT,IDENT);                                               00014300
 WRITE(LPA,F26); FOR I~1 STEP 1 UNTIL CALC DO WRITE(LPA,F27,            00014400
I,  FOR J ~1 STEP 1 UNTIL VARS DO[TDENT[J],  VAL[J,I] ] ) END;          00014500
 IF CALC{0 THEN WRITE(LPA,FFF3);                                        00014600
     LPC~RPC~0;                                                         00014700
 BLANC(N,PER,VER);                                                      00014800
BOO~FALSE;                                                              00014900
   BET~0;START: READ(CRA,F1,LIST1)[GETOUT]; LOOP       IF S[BET+I]=";"  00015000
 THEN GO TO SKIP ; BET ~BET +72;                                        00015100
 IF BET>GG-72 THEN BEGIN WRITE(LPA,FFF5); GO TO GETOUT END;             00015200
                                 GO TO START ; SKIP: BET ~ BET +I;      00015300
WRITE(LPA,F8);                                                          00015400
WRITE(LPA,F3,FOR I~1 STEP 1 UNTIL BET+1 DO S[I]);                       00015500
KK~2; VAR~VER[1]; ORDER~"1ST";                                          00015600
REP:                                                                    00015700
 ALF~1;I~0;L[0]~"@";  S[0]~"#";                                         00015800
ST1: BET~BET-1;IF A=" " THEN GO TO ST1;                                 00015900
 IF ALF >GG THEN BEGIN WRITE(LPA,FFF5); GO TO L0 END;                   00016000
IF BOO THEN BEGIN WRITE(LPA,F5,BET); GO TO L0 END;                      00016100
IF NOT ALPHAW(A) AND A!")" THEN GO TO L1;                               00016200
L2P: IF A!")" THEN BEGIN IF ALPHAW(S[BET+1]) THEN BEGIN ALF~ALF-1;      00016300
IF L[ALF].[36:6]="0" THEN BEGIN L[ALF].[36:6]~A.[42:6];GO TO L2PP END;  00016400
IF L[ALF].[30:6]="0" THEN BEGIN                                         00016500
                                L[ALF].[30:6]~A.[42:6];GO TO L2PP END;  00016600
IF L[ALF].[24:6]="0" THEN BEGIN L[ALF].[24:6]~A.[42:6];GO TO L2PP END;  00016700
IF L[ALF].[18:6]="0" THEN BEGIN L[ALF].[18:6]~A.[42:6];GO TO L2PP END;  00016800
IF L[ALF].[12:6]="0" THEN BEGIN L[ALF].[12:6]~A.[42:6];GO TO L2PP END;  00016900
WRITE(LPA,F6); GO TO L0; END END;      L2: L[ALF]~A;  L2PP: ALF~ALF + 1;00017000
                            IF A=")" THEN RPC~RPC+1;                    00017100
                           GO TO ST1;    L1:                            00017200
IF NOT FINDWORD(P[1],A,11) THEN BEGIN WRITE(LPA,F4);GO TO L0 END;       00017300
 IF A= "@"                 THEN BEGIN WRITE(LPA,F4);GO TO L0 END;       00017400
                                             IF A="(" THEN GO TO L3;    00017500
 IF A="-" THEN GO TO L4; IF A= "+" THEN GO TO L5;IF A="#" THEN BEGIN    00017600
 IF BET!0 THEN BEGIN WRITE(LPA,F4); GO TO L0 END; GO TO L6 END;         00017700
 IF A="." THEN GO TO L7; L8: GAM ~ PRIOR(A); SCOPE; IF A!"%" THEN GO TO 00017800
 L2;I~I+1; M[I,1]~0;M[I,2]~A;M[I,3]~L[ALF-1];L[ALF-1].[36:12]~I;        00017900
                    L[ALF-1].[30:6]~"!" ; GO TO ST1;                    00018000
 L3:GAM~ PRIOR("("); SCOPE; L[ALF-2]~L[ALF-1]; ALF ~ALF-1;              00018100
LPC~LPC+1; GO TO ST1;     L6: IF LPC ! RPC THEN BEGIN WRITE(LPA,F2);    00018200
GO TO L0 END;                   GAM~ PRIOR("@"); SCOPE; GO TO ST2;      00018300
L5: BETM1~BET-1; L99: IF G=" " THEN BEGIN BETM1~BETM1-1;GO TO L99 END;  00018400
    IF G= "(" OR G="-" OR G="+" OR G= "|" OR G="/" OR G="*"             00018500
OR G="." OR G="#"                                                       00018600
    THEN GO TO ST1; GO TO L8;                                           00018700
L4: BETM1~BET-1; L98: IF G=" " THEN BEGIN BETM1~BETM1-1;GO TO L98 END;  00018800
    IF G= "(" OR G="-" OR G="+" OR G= "|" OR G="/" OR G="*"             00018900
OR G="." OR G="#"                                                       00019000
    THEN       A~ "%"; GO TO L8;                                        00019100
L7: L[ALF+1].[42:6]~S[BET-1]; L[ALF+1].[36:6]~S[BET-2];                 00019200
    L[ALF+1].[30:6]~S[BET-3];BET~BET-4; TEST THEN BEGIN BET~BET+1;      00019300
   GO TO L9 END; L[ALF+1].[24:6]~ A;BET~BET-1;TEST THEN BEGIN BET~BET+1;00019400
   GO TO L9 END; L[ALF+1].[18:6]~ A;BET~BET-1;TEST THEN BEGIN BET~BET+1;00019500
   GO TO L9 END; L[ALF+1].[12:6]~ A; L9:L[ALF]~"."; ALF~ALF+2;GO TO ST1;00019600
ST2:                                                                    00019700
IF FEVAL THEN BEGIN EVAL(I); IF N{0 THEN GO TO L0 END;                  00019800
FEVAL ~ FALSE;                                                          00019900
BEGIN                                                                   00020000
 LABEL TB,T1,T2,T3,T4,T5;                                               00020100
 SWITCH SW ~T1,T2,T2,T2,T2,T2,T4,T4,T4,T4,T5,T5,T5,T5;                  00020200
LABEL POWW,BOWW,SOWW;                                                   00020300
LABEL   L10,L11,L12,L13,L14,SPIT,SPXT,SPET,SPOT,L15,L16,L17,L18,L19,L20;00020400
LABEL SPAZ;                                                             00020500
L10:DEL~J~I;I~0;  L11: J~J+1; L12:                                      00020600
IF J>GG THEN BEGIN WRITE(LPA,FFF5); GO TO L0 END;                       00020700
                                   IF I=DEL THEN GO TO L13;I~I+1;       00020800
 IF M[I,2] = "+" OR M[I,2]="-" OR M[I,2]="%" THEN GO TO L14;            00020900
 IF M[I,2] = "|" THEN BEGIN MU~0 ; DIFF; IF ALF!0 AND BET!0 THEN BEGIN  00021000
 D[J,1]~M[I,1]; D[J,2]~"|";                           D[J,3]~BET; J~J+1;00021100
 D[J,1]~ALF; D[J,2]~"|"; D[J,3]~M[I,3];J~J+1; D[J,2]~"+";ROE(D,J,1,J-1);00021200
 ROE(D,J,3,J-2); ROE(QQ,1,I,J) END;                                     00021300
 IF ALF !0 AND BET =0 THEN BEGIN  D[J,1]~M[I,3];D[J,2]~"|";D[J,3]~ALF;  00021400
 ROE(QQ,1,I, J ) END;                                                   00021500
 IF ALF= 0 AND BET! 0 THEN BEGIN  D[J,1]~ BET; D[J,2]~"|";D[J,3]~M[I,1];00021600
 ROE(QQ,1,I, J ) END;                                                   00021700
 IF ALF =0 AND BET= 0 THEN BEGIN Q I]~0;J~J-1 END;                      00021800
                                                          GO TO L11 END;00021900
 IF M[I,2] ="*" THEN BEGIN                                              00022000
MU~0; DIFF;                                                             00022100
                           IF M[I,1]=VAR  OR M[I,1].[30:6]="!"          00022200
                                         THEN BEGIN IF M[I,3]=VAR OR    00022300
 M[I,3].[30:6] ="!" THEN GO TO SPXT; GO TO SPIT;                        00022400
SPXT:                                                                   00022500
IF ALF=0 AND BET=0 THEN BEGIN Q I]~0; GO TO L12 END;IF BET=0 THEN       00022600
GO TO SPIT; IF ALF=0 THEN GO TO SPAZ;                                   00022700
                                                       TOG ~1; SPET:    00022800
 D[J,1]~M[I,1]; D[J,2] ~"*"; D[J,3]~M[I,3]; ROE(D,J+1,1,J);D[J+1,2]~"|";00022900
               D[J+1,3]~BET;                                            00023000
 D[J+2,1] ~"LOG"; D[J+2,2]~"."; D[J+2,3]~ M[I,1];ROE(D,J+3,1,J+2);      00023100
 ROE(D,J+3,3,J+1); D[J+3,2] ~ "|";  J~J+4; IF TOG =0 THEN GO TO SPOT;   00023200
SPIT: IF M[I,3]=0 OR ALF=0 THEN BEGIN Q I]~0; GO TO L12 END;            00023300
       D[J,1]~M[I,3]; D[J,2]~"-"; D[J,3]~ 1; D[J+1,1]~M[I,1];D[J+1,2]~  00023400
 "*"; ROE(D,J+1,3,J); ROE(D,J+2,3,J+1);D[J+2,2] ~"|"; D[J+2,1]~M[I,3];  00023500
 ROE(D,J+3,1,J+2); D[J+3,2]~ "|";                                       00023600
          D[J+3,3]~ALF; IF (M[I,3] =VAR OR M[I,3].[30:6] ="!"      )    00023700
 AND BET!0 THEN                                                         00023800
 BEGIN ROE(D,J+4,1,J+3); ROE(D,J+4,3,J-1); D[J+4,2]~"+";J~J+1 END;      00023900
                                 J~J+4; ROE(QQ,1,I,J-1); GO TO L12 END; 00024000
SPAZ: IF BET=0 THEN BEGIN Q I]~0; GO TO L12 END;                        00024100
 IF M[I,3]= VAR OR M[I,3].[30:6] ="!" THEN BEGIN TOG ~ 0;GO TO SPET;    00024200
 SPOT:      ROE(QQ,1,I,J-1);  GO TO L12 END; Q I]~0;GO TO L12 END;      00024300
 IF M[I,2]="." THEN BEGIN MU ~-1; DIFF;IF ALF = 0 THEN BEGIN Q I]~0;    00024400
 GO TO L12 END;           FOR K~1 STEP 1 UNTIL 14 DO BEGIN IF M[I,1]=   00024500
 TAB[K] THEN GO TO SW[K] END;                                           00024600
PQ[1]~M[I,1]; BLANC(1,ZP,PQ); WRITE(LPA,FFF2,ZP[1]); GO TO L0;          00024700
                              T1: D[J,1]~ ALF; D[J,2]~"/";D[J,3]~M[I,3];00024800
 GO TO TB ; T2: D[J,1]~SAB[1,K];D[J,2]~"."; D[J,3] ~ M[I,3]; J~J+1;     00024900
 ROE(D,J,1,J-1); D[J,2]~"|"; D[J,3]~ ALF; IF K=6 THEN GO TO T3;GO TO TB;00025000
 T3: J~J+1; D[J,1]~0; D[J,2]~"-";ROE(D,J,3,J-1); GO TO TB;              00025100
 T4:D[J,1]~SAB[1,K];D[J,2]~"."; D[J,3]~M[I,3]; J~J+1;ROE(D,J,1,J-1);    00025200
 D[J,2]~"*"; D[J,3]~2; J~J+1;ROE(D,J,1,J-1); D[J,2]~"|";D[J,3]~ ALF;    00025300
 IF K=9 OR K= 10 THEN GO TO T3; GO TO TB;                               00025400
 T5:D[J,1] ~SAB[1,K]; D[J,2]~"."; D[J,3]~M[I,3];J~J+1;D[J,1]~SAB[2,K];  00025500
 D[J,2]~"."; D[J,3]~ M[I,3]; J~J+1; ROE(D,J,1,J-2); ROE(D,J,3,J-1);     00025600
 D[J,2]~"|"; J~J+1; ROE(D,J,1,J-1); D[J,2]~"|"; D[J,3]~ALF;             00025700
 IF K!11 THEN GO TO T3; GO TO TB;                                       00025800
 TB: ROE(QQ,1,I,J);              GO TO L11 END;                         00025900
 IF M[I,2]="/" THEN BEGIN MU~0; DIFF;                                   00026000
 IF ALF=0 AND BET=0 THEN BEGIN Q I]~0; GO TO L12 END;                   00026100
 IF ALF!0 THEN BEGIN D[J,1]~ ALF; D[J,2]~"|";D[J,3] ~ M[I,3]; J~J+1 END;00026200
 IF BET!0 THEN BEGIN D[J,1]~ M[I,1]; D[J,2]~ "|";D[J,3]~ BET; J~J+1 END;00026300
 IF ALF=0 THEN BEGIN D[J,1]~0; D[J,2]~ "-";                             00026400
ROE(D,J,3,J-1);J~J+1 END; IF ALF!0 AND BET! 0 THEN BEGIN                00026500
 ROE(D,J,1,J-2); D[J,2]~ "-";ROE(D,J,3,J-1);J~J+1 END ; D[J,1]~ M[I,3]; 00026600
 D[J,2]~"*"; D[J,3]~2; J~ J+1; ROE(D,J,1,J-2); D[J,2]~ "/";             00026700
 ROE(D,J,3,J-1); ROE(QQ,1,I,J); GO TO L11 END;                          00026800
 L14: MU~0; DIFF; IF ALF = 0 THEN BEGIN IF BET = 0 THEN BEGIN Q I]~0;   00026900
 GO TO L12 END; IF M[I,2]="+" THEN BEGIN  Q I]~BET;       GO TO L12 END;00027000
 D[J,2]~"%"; GO TO L15 END; IF BET =0 THEN BEGIN Q I]~ALF;GO TO L12 END;00027100
 D[J,2]~ M[I,2]; L15: D[J,1]~ALF; D[J,3]~ BET;ROE(QQ,1,I,J);  GO TO L11;00027200
L13:                                                                    00027300
FOR I~1 STEP 1 UNTIL J-1 DO BEGIN IF D[I,1]<10 AND  D[I,3]<10 THEN      00027400
 BEGIN IF D[I,2] ="+" THEN ALF~D[I,1]+D[I,3];                           00027500
IF D[I,2]="|" THEN ALF~ D[I,1]|D[I,3];                                  00027600
IF D[I,2]="%" OR D[I,2]="-" THEN ALF~D[I,1]-D[I,3];                     00027700
IF D[I,2]="/" THEN GO TO SOWW;                                          00027800
IF D[I,2]="*" THEN ALF~D[I,1]*D[I,3]; IF ABS(ALF) }10 THEN GO TO SOWW;  00027900
IF ALF }0 THEN BEGIN                                                    00028000
D[I,2]~"|"; D[I,3]~1; D[I,1]~ALF END ELSE BEGIN D[I,1]~0; D[I,2]~"-";   00028100
D[I,3]~ ABS(ALF) END;                                                   00028200
FOR K~ I+1 STEP 1 UNTIL J-1 DO BEGIN IF D[K,1].[30:6]="!" THEN BEGIN    00028300
IF D[K,1].[36:12] =I THEN BEGIN IF ALF }0 THEN D[K,1]~ALF;              00028400
IF D[K,2]="+" AND ALF < 0 THEN BEGIN D[K,1]~D[K,3]; D[K,2]~"-";         00028500
D[K,3]~ ABS(ALF); GO TO POWW END;                                       00028600
IF ALF <0 AND D[K,2]="-" AND D[K,3]+ABS(ALF) <10 THEN BEGIN             00028700
D[K,3]~D[K,3]+ ABS(ALF); D[K,1]~0 END; POWW: END END;                   00028800
IF D[K,3].[30:6]="!" THEN BEGIN IF D[K,3].[36:12]=I THEN BEGIN          00028900
IF ALF}0 THEN D[K,3]~ALF; IF ALF<0 AND D[K,2]="+" THEN BEGIN            00029000
D[K,3]~ABS(ALF);                                                        00029100
D[K,2]~"-"; GO TO BOWW END; IF ALF<0 AND (D[K,2]="-" OR D[K,2]="%") THEN00029200
   BEGIN D[K,3]~ ABS(ALF); D[K,2]~"+" END;                              00029300
             BOWW: END END END; SOWW: END END;                          00029400
 FOR I~1 STEP 1 UNTIL J-1 DO BEGIN IF D[I,2]="*" THEN BEGIN IF          00029500
 D[I,3] =0 THEN BEGIN D[I,1]~D[I,3]~1; D[I,2]~"|" END; IF D[I,3]=1      00029600
 THEN D[I,2]~"|" END END;                                               00029700
END;                                                                    00029800
BEGIN                                                                   00029900
LABEL L16,L18,L20,L19,L17,L30,L97;                                      00030000
LABEL LL1;                                                              00030100
J~J-1;                                                                  00030200
JSAVE~J;                                                                00030300
 FOR I~1 STEP 1 UNTIL J DO BEGIN IF D[I,2]="%" THEN D[I,2]~"-" END;     00030400
 BET~1;ALF~0; L16: IF D[J,1].[30:6] ="!" THEN BEGIN ALF~ALF +1;L[ALF]~J;00030500
 K~ D[J,1].[36:12];                                                     00030600
 GO TO L17 END; IF D[J,2]="|" AND D[J,1]=1 THEN GO TO L30;              00030700
IF D[J,1]=0 THEN BEGIN IF D[J,2]= "+" THEN GO TO L30; IF D[J,2]="-"     00030800
 OR D[J,2]            ="%" THEN                                         00030900
BEGIN IF S[BET-1]="(" THEN GO TO L18; IF S[BET-1]="+" THEN BEGIN        00031000
 S[BET-1]~"-"; GO TO L30 END; IF S[BET-1] ="-" THEN BEGIN               00031100
 S[BET-1]~"+"; GO TO L30 END END END;                                   00031200
                S[BET] ~ D[J,1]; BET~ BET +1; L18: S[BET]~ D[J,2];      00031300
 IF S[BET] = "%" THEN S[BET]~"-" ;                                      00031400
 BET~BET+1;L30: IF D[J,3].[30:6] ="!" THEN BEGIN ALF~ALF+1;L[ALF]~0;    00031500
 K~ D[J,3].[36:12];     GO TO L17 END;                                  00031600
IF D[J,3]=0 AND (D[J,2]="+" OR D[J,2]="%" OR D[J,2]="-") THEN BEGIN     00031700
 BET~BET-1; GO TO L19 END;                                              00031800
 IF D[J,3]=1 AND D[J,2]="|" AND D[J,1]  ! 1                             00031900
                            THEN BEGIN BET~BET-1;GO TO L19 END;         00032000
                S[BET]~ D[J,3];BET~BET+1;L19: IF ALF = 0 THEN GO TO L20;00032100
 IF T[ALF]!0 THEN BEGIN S[BET] ~")";BET ~BET+1 END; IF L[ALF]=0 THEN    00032200
 BEGIN ALF~ ALF-1;GO TO L19 END; J~ L[ALF]; ALF~ALF-1; GO TO L18;       00032300
 L17: GAM~ PRIOR(D[J,2]); J~K;                                          00032400
IF NUT~PRIOR(D[J,2]) < GAM THEN GO TO LL1; IF (NUT=GAM) AND             00032500
(D[J,2] = "/") THEN GO TO LL1; T[ALF]~0; GO TO L16;                     00032600
 LL1:           S[BET] ~"("; BET~BET+1; T[ALF]~1; GO TO L16;            00032700
 L20:                                                                   00032800
K~0; FOR I~1 STEP 1 UNTIL BET -1 DO BEGIN                               00032900
 IF S[I]=0 THEN BEGIN IF S[I-1] ="(" OR I=1                             00033000
                                     THEN BEGIN K~K-1; GO TO L97 END;   00033100
IF S[I-1]="+" OR S[I-1]="-" THEN BEGIN K~K-2; GO TO L97 END END;        00033200
                                          IF S[I].[36:6] ="0" THEN      00033300
BEGIN SS[I+K]~S[I]; GO TO L97 END;                                      00033400
IF S[I].[12:6] !"0" THEN BEGIN SS[I+K].[42:6]~S[I].[12:6];K~K+1 END;    00033500
IF S[I].[18:6] !"0" THEN BEGIN SS[I+K].[42:6]~S[I].[18:6];K~K+1 END;    00033600
IF S[I].[24:6] !"0" THEN BEGIN SS[I+K].[42:6]~S[I].[24:6];K~K+1 END;    00033700
IF S[I].[30:6] !"0" THEN BEGIN SS[I+K].[42:6]~S[I].[30:6];K~K+1 END;    00033800
SS[I+K].[42:6]~S[I].[36:6];K~K+1; SS[I+K]~S[I].[42:6];                  00033900
L97: END; BET~ BET - 1 +K;                                              00034000
WRITE(LPA,F9,LIST3);                                                    00034100
WRITE(LPA,F7,LIST2);                                                    00034200
EVAL(JSAVE);                                                            00034300
END;                                                                    00034400
IF N>1 THEN BEGIN N~N-1; VAR~VER[KK]; ORDER~ORD[KK]; KK~KK+1;           00034500
FOR J~ 1 STEP 1 UNTIL BET DO S[J]~SS[J];BET~BET+1; S[BET]~";";          00034600
           GO TO REP END END;                                           00034700
   GO TO L0; GETOUT: END.                                               00034800
