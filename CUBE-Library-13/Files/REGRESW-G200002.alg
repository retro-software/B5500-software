BEGIN                                                                   00000100
COMMENT        PROGRAM - WESTINGHOUSE REGRESSION,                       00000200
               CUBE LIBRARY NUMBER IS G200002.                          00000300
               THIS VERSION DATED 6/20/67;                              00000400
COMMENT     A VERSATILE PROGRAM FOR REGRESSION ( CURVE-FITTING)         00000500
            PROBLEMS  -  DEVELOPED AT THE WESTINGHOUSE RESEARCH         00000600
            AND DEVELOPMENT CENTER.     ;                               00000700
FILE IN READER (2,11);                                                  00000800
FILE OUT PRINT 4 "MULTIPL" "REGRESS" (2,15);                            00000900
DEFINE  LCTR= ~ 1 STEP 1 UNTIL #;                                       00001000
DEFINE PRINTER=PRINT#;                                                  00001100
COMMENT PROCEDURES NECESSARY FOR TIMIT   APRIL 20,1964 FOLLOW;          00001200
REAL STREAM PROCEDURE DATEXX(RAWTIME);VALUE RAWTIME;BEGIN SI~LOC RAWTIME00001300
;DI~LOC DATEXX;DS~8 OCT;END;PROCEDURE TIMEIT(PRINTER);FILE PRINTER;BEGIN00001400
 ALPHA ARRAY MOTABLE[1:12];ALPHA AMPM,RAWTIME;INTEGER YR,DAY,TYME,I,YRDA00001500
Y,I1,I2,HOUR;INTEGER ARRAY MOADD[1:12];FORMAT T1("RUN DATE",X12,"RUN TIM00001600
E",X12,"PROCESSOR TIME",X6,"I/O TIME"/X1,A6,I2,",19",I2,X7,I2,":",2I1,A100001700
,"M",X10,I7," SEC.",X6,I7," SEC.");FILL MOTABLE[*]WITH"JAN.","FEB.","MAR00001800
CH ","APRIL ","MAY ","JUNE ","JULY ","AUG.","SEPT.","OCT.","NOV.","DEC."00001900
;FILL MOADD[*]WITH 31,28,31,30,31,30,31,31,30,31,30,31;WRITE(PRINTER[PAG00002000
E]);RAWTIME~TIME(0);YRDAY~DATEXX(RAWTIME);YR~YRDAY DIV 1000;DAY~YRDAY MO00002100
D(YR|1000);IF YR MOD 4=0 THEN MOADD[2]~29;YRDAY~0;FOR I~1 STEP 1 WHILE Y00002200
RDAY<DAY DO YRDAY~YRDAY+MOADD[I];I~I-1;DAY~DAY+MOADD[I]-YRDAY;TYME~TIME(00002300
1)/60;HOUR~TYME DIV(3600);IF HOUR>12 THEN BEGIN HOUR~HOUR-12;AMPM~"P"END00002400
 ELSE AMPM~"A";TYME~TYME MOD 3600;I2~(TYME DIV 60)MOD 10;I1~(TYME DIV 6000002500
)DIV 10;WRITE(PRINTER,T1,MOTABLE[I],DAY,YR,HOUR,I1,I2,AMPM,TIME(2)/60,TI00002600
ME(3)/60);END;                                                          00002700
PROCEDURE INVERTMATRIX(B,N,L,C);                                        00002800
VALUE N;                                                                00002900
ARRAY B[0,0],C[0,0];                                                    00003000
INTEGER N;                                                              00003100
REAL L;                                                                 00003200
BEGIN                                                                   00003300
ARRAY A[0:N,0:2|N];                                                     00003400
INTEGER I,J,K;                                                          00003500
INTEGER N2;                                                             00003600
REAL S;                                                                 00003700
LABEL NO1,RETURN;                                                       00003800
N2~2|N;                                                                 00003900
FOR I~  1 STEP 1 UNTIL N   DO BEGIN                                     00004000
FOR J~N+1 STEP 1 UNTIL N2  DO                                           00004100
A[I,J]~0;                                                               00004200
A[I,I+N]~1 END;                                                         00004300
FOR I~1 STEP 1 UNTIL N DO                                               00004400
FOR J~1 STEP 1 UNTIL N DO                                               00004500
A[I,J]~B[I,J];                                                          00004600
L~1;                                                                    00004700
FOR I~1 STEP 1 UNTIL N-1 DO                                             00004800
BEGIN                                                                   00004900
K~0;                                                                    00005000
S~ABS(A[I,I])        ;                                                  00005100
FOR J~I+1 STEP 1 UNTIL N DO                                             00005200
BEGIN                                                                   00005300
IF S < ABS(A[J,I]) THEN BEGIN K~J; S~ABS(A[J,I]) END;                   00005400
END;                                                                    00005500
IF S { 0 THEN BEGIN NO1:L~0; GO RETURN; END;                            00005600
IF K=0 THEN L~L|A[I,I];                                                 00005700
IF K > 0 THEN BEGIN                                                     00005800
L~L|A[K,I];                                                             00005900
FOR J~I STEP 1 UNTIL N2  DO BEGIN                                       00006000
S~A[I,J];                                                               00006100
A[I,J]~A[K,J];                                                          00006200
A[K,J]~S;                                                               00006300
END;                                                                    00006400
L~-L;                                                                   00006500
END;                                                                    00006600
FOR J~I+1 STEP 1 UNTIL N2  DO A[I,J]~A[I,J]/A[I,I];                     00006700
FOR J~I+1 STEP 1 UNTIL N2  DO BEGIN                                     00006800
IF A[I,J] ! 0 THEN BEGIN                                                00006900
FOR K~I+1 STEP 1 UNTIL N DO A[K,J]~A[K,J]-A[K,I]|A[I,J];                00007000
END;                                                                    00007100
END;                                                                    00007200
END;                                                                    00007300
IF A[N,N] = 0 THEN GO TO NO1;                                           00007400
L~L|A[N,N];                                                             00007500
FOR K~N+1 STEP 1 UNTIL N2 DO BEGIN                                      00007600
A[N,K  ]~A[N,K  ]/A[N,N]         ;                                      00007700
FOR I~N-1 STEP -1 UNTIL 1 DO                                            00007800
FOR J~I+1 STEP 1 UNTIL N DO                                             00007900
A[I,K  ]~A[I,K  ]-A[I,J]|A[J,K  ];           END;                       00008000
FOR I~1 STEP 1 UNTIL N DO                                               00008100
FOR J~1 STEP 1 UNTIL N DO                                               00008200
C[I,J]~A[I,J+N];                                                        00008300
RETURN: END OF INVERTMATRIX;                                            00008400
REAL PROCEDURE DOPR(A,B,N);                                             00008500
VALUE N;  INTEGER N;  ARRAY A,B[0];                                     00008600
COMMENT     DOUBLE PRECISION ROUTINE WHICH COMPUTES THE                 00008700
            (I,J)TH COMPONENT OF MATRIX C=A|B  ;                        00008800
BEGIN                                                                   00008900
INTEGER I;                                                              00009000
REAL D,E;                                                               00009100
D~E~0;  FOR I~1 STEP 1 UNTIL N DO                                       00009200
    DOUBLE(A[I],0,B[I],0,|,D,E,+,~,D,E);                                00009300
    DOPR~D;                                                             00009400
END DOPR;                                                               00009500
PROCEDURE MATRIXMPY(A,B,C,II,JJ,KK);                                    00009600
VALUE II,JJ,KK;     INTEGER II,JJ,KK;                                   00009700
ARRAY A,B,C[0,0];                                                       00009800
COMMENT MATRIX MULTIPLICATION   A=B | C;                                00009900
BEGIN                                                                   00010000
INTEGER I,J,K;                                                          00010100
ARRAY CC[0:JJ];                                                         00010200
FOR J LCTR KK DO BEGIN  FOR K LCTR JJ DO                                00010300
    CC[K]~C[K,J];                                                       00010400
    FOR I LCTR II DO    A[I,J]~DOPR(B[I,*],CC,JJ);                      00010500
END;                                                                    00010600
END MATRIXMPY;                                                          00010700
PROCEDURE MATRIX(A,II,JJ,MYPR);                                         00010800
    VALUE II,JJ,MYPR;  ARRAY A[0,0];                                    00010900
INTEGER II,JJ;  REAL MYPR;                                              00011000
COMMENT THIS PROCEDURE PRINTS OUT THE MATRIX A OF DIM. (II X JJ);       00011100
BEGIN                                                                   00011200
INTEGER I,J,S,L;                                                        00011300
ALPHA D;                                                                00011400
LABEL HON,RETURN;                                                       00011500
LIST    HEAD(FOR I~1 STEP 1 UNTIL L DO [D,S+I]),                        00011600
        M1(I,FOR J~S+1 STEP 1 UNTIL JJ DO  A[I,J]|MYPR),                00011700
        HEAD1(FOR I~1 STEP 1 UNTIL 6  DO [D,S+I]),                      00011800
        M2(FOR I~ 1 STEP 1 UNTIL II DO                                  00011900
            [I, FOR J~1 STEP 1 UNTIL 6 DO A[I,S+J]|MYPR]);              00012000
FORMAT FMT(X5,6(X6,A6,I3,X3)),                                          00012100
        FMT1("ROW",I3,6(X4,E14.7));                                     00012200
    S~0;    L~JJ;   D~"COLUMN";                                         00012300
HON:    IF L{6  THEN BEGIN WRITE(PRINT[DBL],FMT,HEAD);                  00012400
    FOR I~1 STEP 1 UNTIL II DO  WRITE(PRINT,FMT1,M1); GO RETURN; END;   00012500
        WRITE(PRINT[DBL],FMT,HEAD1);                                    00012600
    WRITE(PRINT[11],FMT1,M2);  WRITE(PRINT[PAGE]);                      00012700
    S~S+6;  L~L-6;  GO HON;                                             00012800
RETURN: END MATRIX;                                                     00012900
PROCEDURE HUNTER(M,N,R,X,Y);                                            00013000
VALUE  M,N,R;   INTEGER M,N,R;                                          00013100
ARRAY X,Y[0,0];                                                         00013200
COMMENT THIS PROCEDURE FINDS THE MIN. AND MAX. OF EACH INDEPENDENT      00013300
    VARIABLE X[I,J] J=1,...N AND STORES THEM IN Y[I,2] AND              00013400
    Y[I,1] RESPECTIVELY  ;                                              00013500
BEGIN                                                                   00013600
    INTEGER I,J;    REAL W,XMAX,XMIN;                                   00013700
    FOR I~ R STEP 1 UNTIL N DO                                          00013800
BEGIN                                                                   00013900
    XMAX~XMIN~X[1,I];                                                   00014000
    FOR J~2 STEP 1 UNTIL M DO                                           00014100
BEGIN                                                                   00014200
    W~ X[J,I];                                                          00014300
    IF XMAX<W   THEN XMAX~W;                                            00014400
    IF XMIN>W   THEN XMIN~W;                                            00014500
END;                                                                    00014600
    IF R=1 THEN BEGIN  Y[I,1]~XMAX; Y[I,2]~XMIN; END                    00014700
    ELSE BEGIN Y[I-1,1]~XMAX; Y[I-1,2]~XMIN; END                        00014800
END;                                                                    00014900
END HUNTER;                                                             00015000
COMMENT     THIS PROCEDURE MOVES N WORDS STARTING WITH LOCATION         00015100
            HERE TO LOCATION THERE  ;                                   00015200
STREAM PROCEDURE    MOVE(N,HERE,THERE);  VALUE N;                       00015300
    BEGIN   SI~ HERE;  DI~ THERE;  DS~ N WDS  END;                      00015400
PROCEDURE DELETE(M,N,Q,NOFDEL,DEL,X,NX,NEWN,NEWQ);                      00015500
VALUE   M,N,Q,NOFDEL;            INTEGER M,N,Q,NOFDEL,NEWN,NEWQ;        00015600
ARRAY   X,NX[0,0];  INTEGER ARRAY DEL[0];                               00015700
COMMENT     THIS PROCEDURE DELETES THE COLUMNS OF ARRAYS                00015800
            X AND NX SPECIFIED BY THE DEL ARRAY  ;                      00015900
BEGIN                                                                   00016000
INTEGER  I,J,K,C,JK,FQ,A,JJ;                                            00016100
JK~NEWQ;  A~0;                                                          00016200
IF NEWN > 0 THEN FOR K LCTR NEWN DO                                     00016300
    BEGIN  C~DEL[K]+1;  JJ~ IF K<NEWN THEN DEL[K+1] ELSE Q-N+1;         00016400
        NEWQ~ JJ-C; IF NEWQ>0 THEN FOR I  LCTR M DO                     00016500
                    MOVE(NEWQ,X[I,C],X[I,C-K]);                         00016600
        A~1;                                                            00016700
    END;                                                                00016800
Q~Q-NEWN;   FQ~Q-N+1;  J~ NOFDEL - NEWN;                                00016900
FOR K LCTR J DO                                                         00017000
BEGIN                                                                   00017100
    JJ~ IF (K+NEWN)<NOFDEL THEN DEL[K+NEWN+1] ELSE JK+1;                00017200
    C~ DEL[K+NEWN]+1;  NEWQ~ JJ-C;                                      00017300
    IF NEWQ>0 THEN FOR I LCTR M DO  MOVE(NEWQ,NX[I,C],NX[I,C-K]);       00017400
    IF A=0 THEN BEGIN IF C>FQ THEN N~N-1; END  ELSE  N~N-1;             00017500
    Q~ Q-1;                                                             00017600
END;                                                                    00017700
NEWN~ N;    NEWQ~ Q;                                                    00017800
END DELETE;                                                             00017900
PROCEDURE ORDERING(M,OBS,X,Y,PR);                                       00018000
VALUE M;    INTEGER M;                                                  00018100
ARRAY  X,Y,PR[0];                                                       00018200
INTEGER ARRAY OBS[0];                                                   00018300
COMMENT THIS PROCEDURE ORDERS THE X VECTOR FROM SMALLEST TO LARGEST,    00018400
    AND ADJUSTS THE Y AND PR VECTOR ACCORDINGLY.;                       00018500
BEGIN                                                                   00018600
    INTEGER I,J,INDEX,T1;                                               00018700
    REAL LOSER,T;                                                       00018800
    FOR I~1 STEP 1 UNTIL M DO OBS[I]~ I;                                00018900
    FOR I~1 STEP 1 UNTIL M-1 DO                                         00019000
BEGIN                                                                   00019100
    LOSER~ X[I];    INDEX~ I;                                           00019200
    FOR J~ I+1 STEP 1 UNTIL M DO                                        00019300
BEGIN                                                                   00019400
    IF LOSER > X[J] THEN BEGIN LOSER~X[J]; INDEX~J; END;                00019500
END;                                                                    00019600
    X[INDEX]~X[I];  X[I]~LOSER;                                         00019700
    T~Y[INDEX]; Y[INDEX]~Y[I];  Y[I]~ T;                                00019800
    T1~OBS[INDEX];  OBS[INDEX]~OBS[I];  OBS[I]~T1;                      00019900
    T~PR[INDEX];  PR[INDEX]~ PR[I];  PR[I]~ T;                          00020000
END;                                                                    00020100
END ORDERING;                                                           00020200
PROCEDURE POLYNOMIAL(ORDER,M,NX,NY,X,NCOLS,FLAG);                       00020300
VALUE ORDER,M,NX,NY;  INTEGER ORDER,M,NX,NY,NCOLS,FLAG;                 00020400
ARRAY X[0,0];                                                           00020500
COMMENT THIS PROCEDURE TRANSFORMS THE INDEPENDENT VARIABLES             00020600
    OF THE ARRAY X INTO THE POLYNOMIAL TERMS OF A 2ND-5TH               00020700
    ORDER POLYNOMIAL  ;                                                 00020800
BEGIN                                                                   00020900
INTEGER I,J,K,L,R,P,S,C;                                                00021000
LABEL ERR,RETURN;                                                       00021100
FORMAT F1("ORDER=",I3,"IS TOO LARGE FOR",I3,"VARIABLES");               00021200
FLAG~0; S~NX+NY;                                                        00021300
IF ORDER=2 THEN BEGIN                                                   00021400
FOR I LCTR M DO BEGIN C~0;                                              00021500
FOR J LCTR NX DO                                                        00021600
    FOR K~J STEP 1 UNTIL NX DO                                          00021700
    X[I,(C~C+1)]~X[I,J]|X[I,K];                                         00021800
    END; GO TO RETURN;                                                  00021900
END                                                                     00022000
ELSE                                                                    00022100
IF ORDER=3 THEN BEGIN                                                   00022200
FOR I LCTR M DO BEGIN C~0;                                              00022300
FOR J LCTR NX DO                                                        00022400
    FOR K~J STEP 1 UNTIL NX DO                                          00022500
    FOR L~K STEP 1 UNTIL NX DO                                          00022600
    X[I,(C~C+1)]~X[I,J]|X[I,K]|X[I,L];                                  00022700
    END; GO TO RETURN;                                                  00022800
END                                                                     00022900
ELSE                                                                    00023000
IF ORDER=4 THEN BEGIN                                                   00023100
FOR I LCTR M DO BEGIN C~0;                                              00023200
FOR J LCTR NX DO                                                        00023300
    FOR K~J STEP 1 UNTIL NX DO                                          00023400
    FOR L~K STEP 1 UNTIL NX DO                                          00023500
    FOR R~L STEP 1 UNTIL NX DO                                          00023600
    X[I,(C~C+1)]~X[I,J]|X[I,K]|X[I,L]|X[I,R];                           00023700
    END; GO TO RETURN;                                                  00023800
END                                                                     00023900
ELSE                                                                    00024000
IF ORDER=5 THEN BEGIN                                                   00024100
FOR I LCTR M DO BEGIN C~0;                                              00024200
FOR J LCTR NX DO                                                        00024300
    FOR K~J STEP 1 UNTIL NX DO                                          00024400
    FOR L~K STEP 1 UNTIL NX DO                                          00024500
    FOR R~L STEP 1 UNTIL NX DO                                          00024600
    FOR P~R STEP 1 UNTIL NX DO                                          00024700
    X[I,(C~C+1)]~X[I,J]|X[I,K]|X[I,L]|X[I,R]|X[I,P];                    00024800
    END; GO TO RETURN;                                                  00024900
END;                                                                    00025000
ERR: FLAG~1; WRITE(PRINT,F1,ORDER,NX);                                  00025100
RETURN: NCOLS~C;                                                        00025200
END POLYNOMIAL;                                                         00025300
PROCEDURE MATMPY(N,A,B,C,II,JJ);                                        00025400
VALUE N,II,JJ;  INTEGER N,II,JJ;                                        00025500
ARRAY  A,C[0],B[0,0];                                                   00025600
BEGIN                                                                   00025700
INTEGER I,J;    REAL Q;   LABEL RETURN;                                 00025800
COMMENT COL. VECTOR A=B | (COL. VECTOR C);                              00025900
    IF N=1  THEN  BEGIN                                                 00026000
    FOR I~1 STEP 1 UNTIL II DO BEGIN  Q~0;                              00026100
    FOR J~1 STEP 1 UNTIL JJ DO  Q~ Q + B[I,J] | C[J]; A[I]~Q; END;      00026200
    GO TO  RETURN;  END;                                                00026300
COMMENT ROW  VECTOR C=(ROW VECTOR A) |  B ;                             00026400
    FOR J~ 1 STEP 1 UNTIL JJ DO BEGIN Q~ 0;                             00026500
    FOR I~1  STEP 1 UNTIL II DO Q~ Q + A[I] | B[I,J]; C[J]~Q; END;      00026600
RETURN:END MATMPY;                                                      00026700
PROCEDURE REPLICATION(M,X,Y,SS,DF,XVAL,SSREP,TOTDF,COUNT);              00026800
VALUE M;                                                                00026900
INTEGER M,TOTDF,COUNT;  REAL SSREP;                                     00027000
ARRAY X,Y,SS,XVAL[0];  INTEGER ARRAY DF[0];                             00027100
COMMENT THIS PROCEDURE CALCULATES THE S.S. DUE TO VARIATION             00027200
    BETWEEN REPLICATE MEASUREMENTS.;                                    00027300
BEGIN                                                                   00027400
INTEGER I,ITEMP,J,K;  REAL TEMP,S,YBAR;                                 00027500
LABEL REPEAT,CONT;                                                      00027600
    I~1;  COUNT~0;                                                      00027700
REPEAT: COUNT~COUNT + 1;  TEMP~X[I];  ITEMP~I;                          00027800
    I~I+1;                                                              00027900
    WHILE X[I]=TEMP AND I{M DO I~I+1;                                   00028000
    IF I-ITEMP=1 THEN BEGIN SS[COUNT]~DF[COUNT]~0; XVAL[COUNT]~TEMP END 00028100
    ELSE BEGIN S~0; K~I-1;                                              00028200
    FOR J~ITEMP STEP 1 UNTIL K DO S~S+Y[J];  YBAR~S/(I-ITEMP);          00028300
    S~0; FOR J~ITEMP STEP 1 UNTIL K DO S~S+(Y[J]-YBAR)|(Y[J]-YBAR);     00028400
    SS[COUNT]~S; DF[COUNT]~I-1-ITEMP; XVAL[COUNT]~TEMP;  END;           00028500
    IF I>M THEN GO TO CONT;                                             00028600
    GO TO REPEAT;                                                       00028700
CONT:   SSREP~TOTDF~0;                                                  00028800
    FOR I~1 STEP 1 UNTIL COUNT DO BEGIN                                 00028900
    SSREP~SSREP + SS[I];  TOTDF~TOTDF + DF[I];  END;                    00029000
END REPLICATION;                                                        00029100
PROCEDURE LAGCORR(N,NOFK,X,Y,Z);                                        00029200
VALUE N,NOFK;   INTEGER N,NOFK;                                         00029300
ARRAY X[*],Y[*],Z[*];                                                   00029400
COMMENT     COMPUTES THE LAG CORRELATIONS OF THE RESIDUALS  ;           00029500
BEGIN                                                                   00029600
INTEGER J,K;    REAL T1,T2,T3,T4,T5,T6,T7,D,NU,DE;                      00029700
    FOR K~ 1 STEP 1 UNTIL NOFK  DO                                      00029800
BEGIN                                                                   00029900
    T1~T2~T3~T4~T5~0;                                                   00030000
    FOR J~ 1 STEP 1 UNTIL N-K  DO                                       00030100
BEGIN                                                                   00030200
    T6~ X[J];   T7~ Y[J+K];                                             00030300
    T1~ T1 + T6 | T7;  T2~ T2 + T6;  T3~ T3 + T7;                       00030400
    T4~ T4 + T6 | T6;  T5~ T5 + T7 | T7;                                00030500
END;                                                                    00030600
    D~ N-K;                                                             00030700
    NU~ T1/D - T2|T3/(D|D);                                             00030800
    DE~SQRT((T4/D-(T2|T2)/(D|D)) | (T5/D-(T3|T3)/(D|D)));               00030900
    Z[K]~NU/DE;                                                         00031000
END;                                                                    00031100
END LAGCORR;                                                            00031200
COMMENT     THE VPLOT PACKAGE CONTAINS 4 PROCEDURES: VPLOT1,VPLOT2,     00031300
            VPLOT3, AND VPLOT4 WHICH GIVE AN ON LINE PLOT OF THE        00031400
            RESIDUALS VS. INDEPENDENT VARIABLES ;                       00031500
ARRAY VOICIUNBONNOM[0:15];                                              00031600
ALPHA ARRAY OOOQ[-8:111,-2:60];                                         00031700
PROCEDURE VPLOT1(NHL,NSBH,NVL,NSBV);                                    00031800
    INTEGER NHL,NSBH,NVL,NSBV;                                          00031900
    COMMENT TO FILL PAGE NVL|(NSBV+1)=111, NHL|(NSBH+1){41 MAX;         00032000
    BEGIN                                                               00032100
        INTEGER NH,   I,J,DELNV,DELNH,TEMP1,TEMP2;                      00032200
        DEFINE V=VOICIUNBONNOM#,                                        00032300
               OONVL=V[4]#,                                             00032400
               NH2=V[3]#,                                               00032500
               NV=V[9]#,                                                00032600
               OONHL=V[10]#;                                            00032700
        OONVL~NVL;                                                      00032800
        OONHL~NHL;                                                      00032900
        NH~NHL|(NSBH+1)+2;                                              00033000
        NV~(NSBV+1)|NVL;                                                00033100
        NH2~NH-2;                                                       00033200
        V[12]~NSBV;                                                     00033300
        V[11]~NSBH;                                                     00033400
        FOR I~-8 STEP 1 UNTIL NV DO                                     00033500
        FOR J~-2 STEP 1 UNTIL NH DO OOOQ[I,J]~"      ";                 00033600
        TEMP1~NSBV+1;                                                   00033700
I~0;                                                                    00033800
        FOR J~0 STEP 1 UNTIL NH2 DO OOOQ[I,J]~":";                      00033900
        TEMP1~NSBH+1;                                                   00034000
    J~0;                                                                00034100
        FOR I~0 STEP 1 UNTIL NV DO OOOQ[I,J]~"-";                       00034200
        TEMP2~NSBV+1;                                                   00034300
        FOR J~0 STEP TEMP1 UNTIL NH2 DO                                 00034400
        FOR I~0 STEP TEMP2 UNTIL NV DO OOOQ[I,J]~".";                   00034500
    END VPLOT1;                                                         00034600
PROCEDURE VPLOT2(XMAX,XMIN,YMAX,YMIN);                                  00034700
    REAL XMAX,XMIN,YMAX,YMIN;                                           00034800
    BEGIN                                                               00034900
        REAL DELX,DELY,S,TEMP0,TEMP1,I,XX,YY;                           00035000
        LABEL L1;                                                       00035100
        DEFINE V=VOICIUNBONNOM#,                                        00035200
               LOG=0.43429448|LN#,                                      00035300
               DX=V[0]#,                                                00035400
               DY=V[1]#,                                                00035500
               NSBVP1=(V[12]+1)#,                                       00035600
               NSBHP1=(V[11]+1)#,                                       00035700
               FLAG=V[2]#,                                              00035800
               NH2=V[3]#,                                               00035900
               SCALEX=OOOQ[-6,-2]#,                                     00036000
               SCALEY=OOOQ[-6,NH2+2]#,                                  00036100
               OONVL=V[4]#,                                             00036200
               NV=V[9]#,                                                00036300
               OONHL=V[10]#;                                            00036400
        FLAG~0.0;                                                       00036500
V[7]~XMIN;                                                              00036600
V[8]~XMAX;                                                              00036700
V[13]~YMIN;                                                             00036800
V[14]~YMAX;                                                             00036900
        IF (XMAX-XMIN){0 OR (YMAX-YMIN){0 THEN BEGIN FLAG~1.0;GO L1 END;00037000
        COMMENT FOLLOWING  FOUR STATEMENTS OMITTED WHILE E FORMAT USED  00037100
        IF XMAX=0 THEN SCALEX~0                                         00037200
                  ELSE IF XMAX<0  THEN SCALEX~ENTIER(LOG(-XMAX)) ELSE   00037300
        SCALEX~ENTIER(LOG((XMAX-XMIN)/2.0)),                            00037400
        IF SCALEX<0 THEN SCALEX~SCALEX-1.0,                             00037500
        OOOQ[-6,-2]~SCALEX,                                             00037600
        REPLACE WHEN R FORMAT IMPLEMENTED;                              00037700
        SCALEX~0;                                                       00037800
        XX~YY~0;                                                        00037900
        IF YMAX=0 THEN SCALEY~0  ELSE SCALEY~ENTIER(LOG(ABS(YMAX)));    00038000
        IF SCALEY<0 THEN SCALEY~SCALEY-1.0;                             00038100
        OOOQ[-6,NH2+2.0]~SCALEY;                                        00038200
        DX~(XMAX-XMIN)/NV;                                              00038300
        DY~(YMAX-YMIN)/NH2;                                             00038400
        S~((YMAX-YMIN)/10*SCALEY)/OONHL;                                00038500
        V[6]~                                                           00038600
        OOOQ[-6,0]~TEMP0~YMIN/10*SCALEY;                                00038700
        OOOQ[-5,0]~1.0;   %FLAG FOR FMT TEST;                           00038800
        FOR I~1 STEP 1 UNTIL OONHL DO                                   00038900
        BEGIN                                                           00039000
            TEMP1~TEMP0+S;                                              00039100
            IF TEMP1=0 THEN   YY~I;                                     00039200
            OOOQ[-6,NSBHP1|I]~TEMP0~TEMP1;                              00039300
            OOOQ[-5,NSBHP1|I]~1;                                        00039400
        END;                                                            00039500
        S~((XMAX-XMIN)/10*SCALEX)/OONVL;                                00039600
        V[5]~                                                           00039700
        OOOQ[0,-2]~TEMP0~XMIN/10*SCALEX;                                00039800
        FOR I~1 STEP 1 UNTIL OONVL DO                                   00039900
        BEGIN                                                           00040000
            TEMP1~TEMP0+S;                                              00040100
            IF TEMP1 =0 THEN   XX~I;                                    00040200
            OOOQ[NSBVP1|I,-2]~TEMP0~TEMP1;                              00040300
        END;                                                            00040400
        IF   YY=0 THEN                                                  00040500
            BEGIN                                                       00040600
                IF YMIN=0 THEN V[6]~0                                   00040700
                ELSE                                                    00040800
                IF YMAX=-YMIN THEN V[6]~-SIGN(V[6])|0.5|OONHL|NSBHP1    00040900
                ELSE                                                    00041000
                V[6]~-YMIN/DY                                           00041100
            END                                                         00041200
            ELSE                                                        00041300
            V[6]~-SIGN(V[6])|  YY|NSBHP1;                               00041400
        IF   XX=0 THEN                                                  00041500
            BEGIN                                                       00041600
                IF XMAX=-XMIN THEN V[5]~-SIGN(V[5])|0.5|OONVL|NSBVP1    00041700
                ELSE                                                    00041800
                IF XMIN=0 THEN V[5]~0                                   00041900
                ELSE                                                    00042000
                V[5]~-XMIN/DX                                           00042100
            END ELSE                                                    00042200
            V[5]~-SIGN(V[5])|  XX|NSBVP1;                               00042300
             FOR I~0 STEP 1 UNTIL NV DO OOOQ[I,V[6]]~"-"; % BOB FARDO   00042400
L1: END VPLOT2;                                                         00042500
PROCEDURE VPLOT3(SYM,X,Y);                                              00042600
    ALPHA SYM; REAL X,Y;                                                00042700
    BEGIN                                                               00042800
        LABEL ERROR;                                                    00042900
        FORMAT NOTE1("FOLLOWING POINT CAN NOT BE PLOTTED ON GRAPH ",    00043000
                    "BELOW:(",E12.4,",",E12.4,")");                     00043100
        DEFINE V=VOICIUNBONNOM#,                                        00043200
               DX=V[0]#,                                                00043300
               DY=V[1]#,                                                00043400
               XX=V[5]#,                                                00043500
               YY=V[6]#,                                                00043600
               XMIN=V[7]#,                                              00043700
               XMAX=V[8]#,                                              00043800
               YMIN=V[13]#,                                             00043900
               YMAX=V[14]#,                                             00044000
               FLAG=V[2]#;                                              00044100
               IF FLAG=1.0 THEN GO ERROR;                               00044200
            IF X<XMIN OR X>XMAX OR Y<YMIN OR Y>YMAX THEN                00044300
                BEGIN                                                   00044400
                WRITE(PRINTER,NOTE1,X,Y);                               00044500
                GO ERROR;                                               00044600
                END;                                                    00044700
            OOOQ[XX+X/DX,YY+Y/DY]~SYM;                                  00044800
ERROR:  END VPLOT3;                                                     00044900
PROCEDURE VPLOT4(TITLEY,NUMY,NUMSP,HEADX);                              00045000
     ALPHA ARRAY TITLEY[*]; INTEGER NUMY,NUMSP; FORMAT HEADX;           00045100
    BEGIN                                                               00045200
        LABEL ERROR,L1,L2,L3,L4;                                        00045300
        INTEGER TEMP,I,J;                                               00045400
        ALPHA TEMP1;                                                    00045500
        FORMAT NOTE("VPLOT2 PARAMETER LIST HAS ONE MIN VALUE } MAX",    00045600
                    "VALUE, NO PLOT CAN BE MADE...SORRY..."),           00045700
               FSCALE("@",I3),                                          00045800
               FMTA(120A1);                                             00045900
SWITCH FORMAT FMTX ~ (X3,E8.1),(X4,E8.1),(X5,E8.1),(X6,E8.1),(X7,E8.1), 00046000
(X8,E8.1),(X9,E8.1),(X10,E8.1),(X11,E8.1),(X12,E8.1),(X13,E8.1),        00046100
(X14,E8.1),(X15,E8.1),(X16,E8.1),(X17,E8.1),(X18,E8.1),(X19,E8.1),      00046200
(X20,E8.1),(X21,E8.1),(X22,E8.1),(X23,E8.1),(X24,E8.1),(X25,E8.1),      00046300
(X26,E8.1),(X27,E8.1),(X28,E8.1),(X29,E8.1),(X30,E8.1),(X31,E8.1),      00046400
(X32,E8.1),(X33,E8.1),(X34,E8.1),(X35,E8.1),(X36,E8.1),(X37,E8.1),      00046500
(X38,E8.1),(X39,E8.1),                                                  00046600
(X40,E8.1),(X41,E8.1),(X42,E8.1),(X43,E8.1),(X44,E8.1),(X45,E8.1),      00046700
(X46,E8.1),(X47,E8.1),(X48,E8.1),(X49,E8.1),(X50,E8.1),(X51,E8.1),      00046800
(X52,E8.1),(X53,E8.1),(X54,E8.1),(X55,E8.1),(X56,E8.1),(X57,E8.1),      00046900
(X58,E8.1),(X59,E8.1),(X60,E8.1),(X61,E8.1),(X62,E8.1),(X63,E8.1),      00047000
(X64,E8.1),(X65,E8.1),(X66,E8.1),(X67,E8.1),(X68,E8.1),(X69,E8.1),      00047100
(X70,E8.1),(X71,E8.1),(X72,E8.1),(X73,E8.1),(X74,E8.1),(X75,E8.1),      00047200
(X76,E8.1),(X77,E8.1),(X78,E8.1),(X79,E8.1),(X80,E8.1),(X81,E8.1),      00047300
(X82,E8.1),(X83,E8.1),(X84,E8.1),(X85,E8.1),(X86,E8.1),(X87,E8.1),      00047400
(X88,E8.1),(X89,E8.1),(X90,E8.1),(X91,E8.1),(X92,E8.1),(X93,E8.1),      00047500
(X94,E8.1),(X95,E8.1),(X96,E8.1),(X97,E8.1),(X98,E8.1),(X99,E8.1),      00047600
(X100,E8.1),(X101,E8.1),(X102,E8.1),(X103,E8.1),(X104,E8.1),(X105,E8.1),00047700
(X106,E8.1),(X107,E8.1),(X108,E8.1),(X109,E8.1),(X110,E8.1),(X111,E8.1);00047800
        SWITCH FORMAT FMT~(A1,F 6.2,X1,112A1),(A1,F6.1,X1,112A1),       00047900
                         (A1,F6.0,X1,112A1),(A1,F6.3,X1,112A1);         00048000
        DEFINE V=VOICIUNBONNOM#, LOG=0.43429448|LN#,                    00048100
               FLAG=V[2]#,                                              00048200
               OONVL=V[4]#,                                             00048300
               NH2=V[3]#,                                               00048400
               SCALEX=OOOQ[-6,-2]#,                                     00048500
               SCALEY=OOOQ[-6,NH2+2]#,                                  00048600
               NV=V[9]#;                                                00048700
        WRITE(PRINTER[PAGE]);                                           00048800
        IF FLAG=1 THEN                                                  00048900
            BEGIN                                                       00049000
                WRITE(PRINTER,NOTE);                                    00049100
                GO ERROR;                                               00049200
            END;                                                        00049300
        TEMP~NH2-NUMSP;                                                 00049400
        J~0;                                                            00049500
        FOR I~1 STEP 1 UNTIL NUMY DO                                    00049600
            BEGIN                                                       00049700
                TEMP~TEMP-J;                                            00049800
                TEMP1~TITLEY[I];                                        00049900
                OOOQ[-8,TEMP]~TEMP1.[12:6];                             00050000
                OOOQ[-8,TEMP-1]~TEMP1.[18:6];                           00050100
                OOOQ[-8,TEMP-2]~TEMP1.[24:6];                           00050200
                OOOQ[-8,TEMP-3]~TEMP1.[30:6];                           00050300
                OOOQ[-8,TEMP-4]~TEMP1.[36:6];                           00050400
                OOOQ[-8,TEMP-5]~TEMP1.[42:6];                           00050500
                J~J+6;                                                  00050600
            END;                                                        00050700
        WRITE(PRINTER[DBL],FSCALE,SCALEY);                              00050800
        FOR TEMP~NH2 STEP -1 UNTIL 0 DO                                 00050900
            BEGIN                                                       00051000
                IF OOOQ[-5,TEMP]=1 THEN                                 00051100
                BEGIN                                                   00051200
                    IF OOOQ[-6,TEMP]=0 THEN BEGIN I~1;GO L3 END;        00051300
IF LOG(ABS(OOOQ[-6,TEMP]))<0 THEN BEGIN I ~3; GO TO L3;  END;           00051400
                I~ENTIER(LOG(ABS(OOOQ[-6,TEMP])));                      00051500
                    IF I>2 THEN I~2;                                    00051600
L3:                 WRITE(PRINTER,FMT[I],OOOQ[-8,TEMP],OOOQ[-6,TEMP],   00051700
                    FOR J~0 STEP 1 UNTIL NV DO OOOQ[J,TEMP]);           00051800
                    GO L2;                                              00051900
                END;                                                    00052000
                WRITE(PRINTER,FMTA,FOR J~-8 STEP 1 UNTIL NV DO          00052100
                OOOQ[J,TEMP]);                                          00052200
L2:         END;                                                        00052300
            COMMENT FOLLOWING STATEMENT OMITTED WHILE E FORMAT USED,    00052400
                    REPLACED WHEN R FORMAT IMPLEMENTED                  00052500
            WRITE(PRINTER[NO],FSCALE,SCALEX);                           00052600
            FOR TEMP~0 STEP 1 UNTIL NV DO                               00052700
                BEGIN                                                   00052800
                    IF OOOQ[TEMP,-2]="      " THEN GO L1;               00052900
L4:                 WRITE(PRINTER[NO],FMTX[TEMP],OOOQ[TEMP,-2]);        00053000
L1:             END;                                                    00053100
    WRITE(PRINTER[DBL]); WRITE(PRINTER,HEADX);                          00053200
ERROR:  WRITE(PRINTER[PAGE]);  END VPLOT4;                              00053300
REAL PROCEDURE ARCSIN(X);  VALUE X;  REAL X;                            00053400
BEGIN  ARCSIN~IF ABS(X)=1 THEN SIGN(X)|1.57079633                       00053500
            ELSE ARCTAN(X/SQRT(1-X|X));  END;                           00053600
REAL PROCEDURE  COSH(X);  VALUE X;  REAL X;                             00053700
BEGIN REAL T;  COSH~.5|((T~EXP(X))+1/T);  END;                          00053800
REAL PROCEDURE SINH(X);  VALUE X;  REAL X;                              00053900
BEGIN REAL T;  SINH~.5|((T~EXP(X))-1/T);  END;                          00054000
COMMENT       MAIN  PROGRAM  BEGINS  HERE         ;                     00054100
INTEGER M,N,Q,H,HH,OPT1,OPT2,OPT3,OPT4,OPT5,OPT6,OPT7,OPT8,OPT9,OPT10,  00054200
        OPT11,OPT12,OPT13,I,NOP,ORDER,NOFDEL,NORD,IND,NOLAG,REPSS;      00054300
REAL  LEV1,E1,LEV2,E2;                                                  00054400
ARRAY   POW[0:50];                                                      00054500
INTEGER ARRAY   ORD,DEL,T[0:50];                                        00054600
LABEL   START1,START2,FINISH;                                           00054700
LIST IN1(M,N,Q,H,OPT3,OPT5,OPT4,OPT6,OPT1,OPT7,OPT2,OPT9,OPT8,          00054800
        OPT10,OPT11,OPT12,OPT13,REPSS,NOLAG),                           00054900
     IN2(FOR I LCTR Q DO T[I]),                                         00055000
     IN3(NOP,FOR I LCTR NOP DO POW[I]),                                 00055100
     IN4(ORDER),                                                        00055200
     IN5(NOFDEL,FOR I LCTR NOFDEL DO DEL[I]),                           00055300
     IN6(LEV1,E1,LEV2,E2),                                              00055400
     IN7(NORD,FOR I LCTR NORD DO ORD[I]);                               00055500
FORMAT  NAME("                                                          00055600
                      ");                                               00055700
START1: BEGIN                                                           00055800
INTEGER DUMMY;                                                          00055900
READ(READER,NAME)[FINISH];                                              00056000
READ(READER,/,IN1);                                                     00056100
IF OPT3=1 THEN READ(READER,/,IN2);                                      00056200
IF OPT5=1 THEN READ(READER,/,IN3);                                      00056300
IF OPT4=1 THEN READ(READER,/,IN4);                                      00056400
IF OPT6=1 THEN READ(READER,/,IN5);                                      00056500
IF OPT11=1 THEN READ(READER,/,IN6);                                     00056600
IF OPT12=1  THEN READ(READER,/,IN7);                                    00056700
IF Q > H THEN HH~Q+1  ELSE  HH~H+1;                                     00056800
IF OPT4=1 THEN BEGIN                                                    00056900
IF ORDER=2 THEN HH~(N|(N+3))/2 +Q-N+1 ELSE                              00057000
IF ORDER=3 THEN HH~(N|((N+1)|(N+5)+6))/6 + Q-N+1                        00057100
ELSE                                                                    00057200
IF ORDER=4 THEN                                                         00057300
HH~(3+(3+(2/3+(N-3)/24)|(N-2))|(N-1))|N + Q + 1                         00057400
ELSE                                                                    00057500
IF ORDER=5 THEN                                                         00057600
HH~(4+(5+(5/3+(5/24+(N-4)/120)|(N-3))|(N-2))|(N-1))|N+Q+1;              00057700
END;    IF H>HH THEN HH ~ H ;                                           00057800
IND~0;                                                                  00057900
IF N=1 THEN IND~1;                                                      00058000
IF OPT3=1 THEN IF H-Q+N=1 THEN IND~1;                                   00058100
IF OPT1=1 THEN IF OPT3=1 THEN IF H-Q+N=2 THEN IND~1;                    00058200
END;                                                                    00058300
START2: BEGIN                                                           00058400
INTEGER FLAG2,FLAG3,NOFY,ORIGN,ORIGQ,NY,JJ,TR,KK,TEMP,LL,MM,IJ,J,K,     00058500
        FLAG4,NQ,NEWN,NEWQ,FLAG5,L,NVEC,FLAG1,ONE,TRA,KD,A1,A2,C,JK,    00058600
        FLAG6,KODE,NXXN;                                                00058700
ARRAY     NX[0:M,0:HH],AX[0:HH,0:HH],NB[0:HH,0:Q-N],MMIN[0:HH,0:2],     00058800
    XVAL,YV,PR,TY,TPR,XX,W[0:M+1],XTY,B,MN,VAR[0:HH],RESVAR[0:Q-N],     00058900
        Y[0:M,0:Q-N],X1[0:HH],X[0:M,0:HH],RX[0:HH,0:M];                 00059000
ARRAY   ANX[0:HH,0:HH],DEV[0:M+1],YHEAD[0:2],YMEAN,YVAR[0:Q-N];         00059100
INTEGER ARRAY   OBSNO,NEGA[0:M+1];                                      00059200
LABEL POLY,TRANS,DELETION,AGAIN,ORDERS,FINAL1,FINAL2,FINAL3,DONE,       00059300
      POLY1;                                                            00059400
ALPHA STAR;                                                             00059500
REAL  R,S,S1,DEVSS,TOTALSS,YBAR,REGSS,RSS,S2,S3,XBAR;                   00059600
REAL DEVMIN,DEVMAX,YMIN,YMAX,S4,S5,S6;                                  00059700
LIST OUT1(FOR I~2 STEP 1 UNTIL N DO [J~I-1,B[I],SQRT(RSS|AX[I,I]),      00059800
        MMIN[J,2],MMIN[J,1],MN[J],VAR[J]]),                             00059900
    OUT2(FOR I LCTR N DO [I,B[I],SQRT(RSS|AX[I,I]),MMIN[I,2],           00060000
        MMIN[I,1],MN[I],VAR[I]]),                                       00060100
    OUT3(N,REGSS,REGSS/N,(REGSS|(M-N))/(N|DEVSS)),                      00060200
    OUT4(N-1,REGSS,REGSS/(N-1),(REGSS|(M-N))/((N-1)|DEVSS)),            00060300
    IN4B(NVEC,FOR I LCTR NVEC DO FOR J LCTR H DO NX[I,J]),              00060400
    IN4C(NVEC,FOR I LCTR NVEC DO FOR J LCTR H DO  X[I,J]),              00060500
    OUT5(M-N,DEVSS,DEVSS/(M-N)),                                        00060600
    OUT6(RSS,SQRT(RSS),REGSS/TOTALSS),                                  00060700
    OUT7(FOR I LCTR M DO [I,R~YV[I],S~PR[I],R-S,TPR[I]]),               00060800
    OUT8(FOR I LCTR N DO [I,(R~B[I])/(S~(SQRT(RSS|AX[I,I]))),           00060900
        R-E1|S,R+E1|S, R-E2|S,R+E2|S,R|(MMIN[I,1]-MMIN[I,2])]),         00061000
    OUT8A(FOR I~1 DO [I,(R~B[I])/(S~(SQRT(RSS|AX[I,I]))),               00061100
        R-E1|S,R+E1|S,R-E2|S,R+E2|S]),                                  00061200
    OUT8B(FOR I~2 STEP 1 UNTIL N DO [I,(R~B[I])/(S~(SQRT(RSS|AX[I,I]))),00061300
        R-E1|S,R+E1|S,R-E2|S,R+E2|S,R|(MMIN[I-1,1]-MMIN[I-1,2])]),      00061400
    OUT9(FOR I LCTR N DO [I,ONE,S~(R~B[I])|R/AX[I,I],S,S/RSS,R]),       00061500
    OUT10(FOR I~2 STEP 1 UNTIL N DO [I-1,ONE,S~(R~B[I])|R/AX[I,I],      00061600
            S,S/RSS,R]),                                                00061700
    OUT10A(FOR I LCTR M DO FOR J LCTR H DO X[I,J]),                     00061800
    OUT10B(FOR I LCTR M DO FOR J LCTR H DO NX[I,J]),                    00061900
    OUT11(FOR J LCTR M DO [OBSNO[J],TY[J],TPR[J], DEV[J]     ,XX[J]]);  00062000
FORMAT XHEAD(X20,"INDEPENDENT VARIABLE  X   "),                         00062100
        LAGFMT("TABLE OF LAG CORRELATIONS OF RESIDUALS"//,              00062200
            X25,"ORDER",X14,"CORRELATION"//),                           00062300
        LAGFMT1(I30,X10,E15.8/),                                        00062400
        LAGFMT2("TABLE OF LAG CORRELATIONS OF THE ABOVE ORDERED",       00062500
            " RESIDUALS"//,X25,"ORDER",X14,"CORRELATION"//);            00062600
FORMAT  RESFMT("FREQUENCY TABLE OF RESIDUALS"//,X10,                    00062700
        "INTERVAL",X24,"FREQUENCY"/),                                   00062800
        RESFMT1(F11.5,"    -     ",F11.5,I18/),                         00062900
        RESFMT2("TABLE OF THE ACTUAL, EXPECTED, AND TOTAL NUMBER OF",   00063000
            " RUNS OF POS. AND NEG. SIGNS OF THE RESIDUALS"//,X4,       00063100
            "[NUMBER   OF   RUNS    OF   LENGTH   K ]",X19,             00063200
            "[NUMBER OF RUNS OF LENGTH  K  OR  MORE  ]"/,X4,            00063300
            "[POSITIVE ]    [NEGATIVE ]    [TOTAL   ]",X19,             00063400
            "[POSITIVE ]    [NEGATIVE ]    [TOTAL    ]"/,               00063500
            " K  OBS.   EXP.    OBS.   EXP.    OBS.   EXP.",X18,        00063600
            "OBS.   EXP.    OBS.   EXP.    OBS.   EXP."//),             00063700
        RESFMT3(I2,I5,F7.1,I8,F7.1,I8,F7.1,I22,F7.1,I8,F7.1,I8,F7.1//), 00063800
        RESFMT4(/"P[OF",I4," OR FEWER RUNS] = .05 (APPROX.)",//         00063900
        "P[OF AT LEAST ONE RUN OF LENGTH",I4," OR MORE] = .05(APPROX.)",00064000
        //);                                                            00064100
FORMAT F2(//"NO. OF DEPENDENT VARIABLES=",I3,//,                        00064200
        "NO. OF INDEPENDENT VARIABLES=",I3,//,                          00064300
        "NO. OF OBSERVATIONS=",I3),                                     00064400
        F3("THE INPUT VARIABLES",//),                                   00064500
        F4("AFTER TRANSFORMATION AND/OR DELETION"/,                     00064600
        "THE NO. OF DEPENDENT VARIABLES IS",I3,/,                       00064700
        "THE NO. OF INDEPENDENT VARIABLES IS",I3,/),                    00064800
        F5(/"THE DEPENDENT AND INDEPENDENT VARIABLES"/,                 00064900
        "AFTER TRANSFORMATION AND/OR DELETION"/),                       00065000
        F6("THE NORMAL MATRIX"//),                                      00065100
        F7("THE CORRELATION MATRIX OF THE INDEPENDENT VARIABLES"//),    00065200
        F7A(//"STOPPED BECAUSE THE CORRELATION MATRIX IS SINGULAR"),    00065300
        F8("THE INVERSE MATRIX"//),                                     00065400
        F8A1(//"THE DETERMINANT OF THE NORMAL  MATRIX IS",E20.8//,      00065500
            "THE TRACE OF THE INVERSE MATRIX IS",E20.8//),              00065600
        F8A(//"PRODUCT OF INVERSE AND NORMAL MATRICES"//),              00065700
        F8B(//"THE ELEMENTS OF THE INVERSE MATRIX ARE IN ERROR",        00065800
                " BY LESS THAN",E20.8//),                               00065900
        F11(X45,"DEPENDENT VARIABLE NO.",I3,//"VAR. NO.",X9,            00066000
        "COEFFICIENT",X7,"STANDARD ERROR",X4,"MIN. VALUE",X8,           00066100
        "MAX. VALUE",X9,"MEAN",X13,"VARIANCE"/,X35,"OF COEFF."/),       00066200
        F11A("DEP. VAR.",X39,3E18.8,E17.8/),                            00066300
        F12(/"CON. TERM",X3,2E18.8,//),                                 00066400
        F13(I8,X4,5E18.8,E17.8,//),                                     00066500
        F14(X25,"ANALYSIS OF VARIANCE TABLE"//,X2,"SOURCE",X21,         00066600
        "D.F.",X15,"SUM OF",X9,"MEAN",X12,"F-RATIO"/,X47,"SQUARES",     00066700
        X8,"SQUARES"/ ),                                                00066800
        F15("REGRESSION",X19,I3,X8,3E17.8,/ ),                          00066900
        F16("DEV. FROM REGRESSION",X9,I3,X8,2E17.8,/ ),                 00067000
        F16A("DEV. OF AVERAGES",X13,I3,X8,3E17.8/ ),                    00067100
        F16B("REPLICATE MEASUREMENTS",X7,I3,X8,2E17.8/ ),               00067200
        F16C(/X8,"X[",I2,"]",X9,"D.F.",X15,"S.S.",X16,"M.S."/),         00067300
        F16D(E15.8,I11,2E21.8/),                                        00067400
        F16E(/"CHI SQ. (",I4," D.F.) = ",E15.8,/ "THE ABOVE STATISTIC", 00067500
            " IS BASED ON THOSE REPLICATED GROUPS REMAINING AFTER"/,    00067600
            "EXCLUSION OF GROUPS (AND THEIR ASSOCIATED D. OF F. )",     00067700
            " HAVING ZERO VARIANCES."/),                                00067800
        F17("TOTAL",X24,I3,X8,E17.8,/  ),                               00067900
        F18("THE RESIDUAL VARIANCE IS",E15.8,//,                        00068000
            "THE SQUARE ROOT OF THE RESIDUAL VARIANCE IS",E15.8,// ,    00068100
            "THE COEFFICIENT OF DETERMINATION ,R SQUARED, IS",E15.8),   00068200
        F19(X35,"TABLE OF RESIDUALS"//,X5,"OBSERVATION NO.",X9,         00068300
            "Y OBSERVED",X11,"Y FITTED",X12,"DEVIATION",                00068400
            X10,"VARIANCE(Y)"//),                                       00068500
        F20(I20,4E20.8),                                                00068600
        F21("THE VARIANCE - COVARIANCE MATRIX OF THE COEFFICIENTS"/),   00068700
        F22("THE CORRELATION MATRIX OF THE COEFFICIENTS"/),             00068800
        F23("VALUES OF T AND CONFIDENCE LIMITS FOR THE COEFFICIENTS"//, 00068900
            "COEFF.",X12,"T-VALUE",X7,"LOWER",F5.1,X9,"UPPER",F5.1,X9,  00069000
            "LOWER",F5.1,X9,"UPPER",F5.1,X8,"COEFF. TIMES"/,            00069100
            " NO.",X26,"PER CENT LIMIT",X5,"PER CENT LIMIT",X5,         00069200
            "PER CENT LIMIT",X5,"PER CENT LIMIT",X6,"(XMAX-XMIN)"//),   00069300
        F24(I3,X3,5E19.8,E18.8//),                                      00069400
        F24A(I3,X3,5E19.8//),                                           00069500
        F24B(X88,"YMAX-YMIN=",X3,E18.8//),                              00069600
        F25(X31,"BREAKDOWN OF THE SUM OF SQUARES FOR EACH VARIABLE"/,   00069700
            X35,"ADJUSTED FOR ALL OTHERS"//,X22,"D.F.",X10,"S.S.",X15,  00069800
            "M.S.",X14,"F-RATIO",X10,"COEFFICIENT"//),                  00069900
        F26("VAR. NO.",I3,I14,2E18.8,2E21.8/),                          00070000
        F27("DEV. FROM REGRESSION",I5,2E18.8),                          00070100
        F28(X30,"DEPENDENT VARIABLE NO.",I3,//,X39,"Y-FITTED",X12,      00070200
            "VAR(Y)"//),                                                00070300
        F32(X35,"TABLE OF RESIDUALS"//,X25,                             00070400
            "ORDERING WITH RESPECT TO TERM NUMBER",I3,//),              00070500
        F33("OBSERVATION NO.",X9,"Y-OBSERVED",X11,"Y-FITTED",X12,       00070600
            "DEVIATION",X9,"ORDERED"/,X83,"TERM NO.",I3,//),            00070700
        F34(I15,4E20.8),                                                00070800
        F35("THE NEW SET OF POINTS AT WHICH EST(Y) AND VAR(Y) ARE",X1,  00070900
            "WANTED"/,"DEPENDENT AND INDEPENDENT VARIABLES"/),          00071000
        F36(X45,"DEPENDENT VARIABLE NO.",I3,//,X30,"Y-OBSERVED",X10,    00071100
            "Y-FITTED",X12,"DEVIATION",X14,"VAR(Y)",///),               00071200
        F37("AT NEW POINT NO.",I4,X1,4E20.8,//),                        00071300
        F38("SUM OF SQUARED DEVIATIONS IS",X2,E15.8),                   00071400
        F39(/"THE INDEPENDENT VARIABLES OF THE POLYNOMIAL"/);           00071500
COMMENT INPUT DATA;                                                     00071600
IF OPT4=0 AND OPT3 =0 THEN READ(READER,/,OUT10B)                        00071700
    ELSE READ(READER,/,OUT10A);                                         00071800
FOR I LCTR  M  DO   W[I]~1;                                             00071900
IF OPT1=1   THEN  FOR I LCTR M DO  W[I]~ X[I,H];                        00072000
COMMENT PRINT-OUT HEADING AND DATA;                                     00072100
WRITE(PRINT[PAGE]); WRITE(PRINT,NAME);                                  00072200
WRITE(PRINT,F2,Q-N,N,M);                                                00072300
WRITE(PRINT[PAGE]); WRITE(PRINT,F3);                                    00072400
IF OPT4=0 AND OPT3=0 THEN MATRIX(NX,M,H,1.0) ELSE MATRIX(X,M,H,1.0);    00072500
                    FLAG2~FLAG3~0; NOFY~1; ORIGN~N; ORIGQ~Q;            00072600
KODE~0;                                                                 00072700
POLY:  IF OPT3=1 THEN GO TO TRANS;                                      00072800
POLY1:                                                                  00072900
IF OPT4=1 THEN                                                          00073000
BEGIN                                                                   00073100
    FLAG2~1;                                                            00073200
COMMENT     POLYNOMIAL TRANSFORMATION WANTED;                           00073300
IJ~Q-N+1;                                                               00073400
NY~Q-N;                                                                 00073500
FOR I LCTR M DO BEGIN MOVE(N,X[I,IJ],NX[I,2]); NX[I,1]~1; END;          00073600
N~N+1;  Q~Q+1;                                                          00073700
POLYNOMIAL(ORDER,M,N,NY,NX,NQ,FLAG4);                                   00073800
    IF FLAG4=1  THEN GO TO DONE;                                        00073900
N~NQ;   Q~N+NY;                                                         00074000
IF KODE=0 THEN                                                          00074100
BEGIN   NEWN~0; NEWQ~N; KODE~1;                                         00074200
    IF OPT6=1 THEN FOR I LCTR NOFDEL DO BEGIN IF J~DEL[I]>NY            00074300
    THEN DEL[I]~J-NY  ELSE NEWN~NEWN+1; END;                            00074400
    NXXN~NEWN;                                                          00074500
END                                                                     00074600
ELSE  BEGIN NEWN~NXXN; NEWQ~N; END;                                     00074700
    GO TO DELETION;                                                     00074800
END POLY;                                                               00074900
TRANS:                                                                  00075000
IF OPT3=1  THEN                                                         00075100
BEGIN                                                                   00075200
COMMENT TRANSFORMATION  OF  VARIABLES;                                  00075300
LABEL  L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,L17,L18,  00075400
    L19,L20,LOOP,AROUND;                                                00075500
SWITCH TRA~L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,L15,L16,L17,  00075600
L18,L19,L20;        FLAG2~1;  JJ~0;                                     00075700
    FOR J LCTR Q DO                                                     00075800
BEGIN                                                                   00075900
    IF  T[J] < 10000 THEN BEGIN TR~T[J] DIV 100; KK~T[J]-100|TR;        00076000
    GO AROUND;  END;                                                    00076100
    TR~T[J] DIV 10000; TEMP~T[J]-10000 | TR;                            00076200
    LL~TEMP DIV 100;  MM~ TEMP-100 | LL;                                00076300
AROUND: IF TR=2  THEN JJ~JJ+1; FLAG1~1;                                 00076400
    FOR I LCTR M DO                                                     00076500
BEGIN                                                                   00076600
GO TO  TRA[TR];                                                         00076700
L1: NX[I,J]~X[I,KK]; GO LOOP;                                           00076800
L2: NX[I,J]~X[I,KK]*POW[JJ]; GO LOOP;                                   00076900
L3: NX[I,J]~EXP(X[I,KK]); GO LOOP;                                      00077000
L4: NX[I,J]~EXP(-X[I,KK]); GO LOOP;                                     00077100
L5: IF FLAG1=1 THEN BEGIN S~0; FOR K LCTR M DO S~S+X[K,KK];             00077200
    XBAR~S/M;   FLAG1~0;  END;                                          00077300
    NX[I,J]~X[I,KK] - XBAR;  GO LOOP;                                   00077400
L6: NX[I,J]~SIN(X[I,KK]); GO LOOP;                                      00077500
L7: NX[I,J]~COS(X[I,KK]); GO LOOP;                                      00077600
L8: NX[I,J]~LN(X[I,KK]); GO LOOP;                                       00077700
L9: NX[I,J]~ARCSIN(X[I,KK]);  GO LOOP;                                  00077800
L10:NX[I,J]~SINH(X[I,KK]);  GO LOOP;                                    00077900
L11:NX[I,J]~COSH(X[I,KK]);  GO LOOP;                                    00078000
L12:NX[I,J]~ NX[I,LL] | NX[I,MM];  GO LOOP;                             00078100
L13:NX[I,J]~X[I,LL] | X[I,MM];  GO LOOP;                                00078200
L14:NX[I,J]~X[I,LL] | NX[I,MM]; GO LOOP;                                00078300
L15:COMMENT TRANSFORMATION CARD CAN BE INSERTED HERE  ;                 00078400
    GO LOOP;                                                            00078500
L16:COMMENT TRANSFORMATION CARD CAN BE INSERTED HERE  ;                 00078600
    GO LOOP;                                                            00078700
L17:COMMENT TRANSFORMATION CARD CAN BE INSERTED HERE  ;                 00078800
    GO LOOP;                                                            00078900
L18:COMMENT TRANSFORMATION CARD CAN BE INSERTED HERE  ;                 00079000
    GO LOOP;                                                            00079100
L19:COMMENT TRANSFORMATION CARD CAN BE INSERTED HERE  ;                 00079200
    GO LOOP;                                                            00079300
L20:COMMENT TRANSFORMATION CARD CAN BE INSERTED HERE  ;                 00079400
    GO LOOP;                                                            00079500
LOOP:   END I;                                                          00079600
END J;                                                                  00079700
IF OPT4=1 THEN BEGIN FOR I LCTR M DO MOVE(Q,NX[I,1],X[I,1]);            00079800
            GO TO POLY1; END;                                           00079900
END TRANS;                                                              00080000
NEWN~0;  NEWQ~Q;                                                        00080100
    IF FLAG3=1 THEN GO TO FINAL1;                                       00080200
DELETION:                                                               00080300
BEGIN   REAL  XXXX;                                                     00080400
LABEL   OVER4;                                                          00080500
ARRAY   X2[0:HH,0:HH];                                                  00080600
IF OPT6=1 THEN                                                          00080700
BEGIN                                                                   00080800
COMMENT DELETION OF VARIABLES;                                          00080900
    DELETE(M,N,Q,NOFDEL,DEL,X,NX,NEWN,NEWQ);                            00081000
    N~NEWN; Q~NEWQ; FLAG2~1;                                            00081100
END;    IF FLAG2=1  THEN BEGIN                                          00081200
                    WRITE(PRINT[PAGE]); WRITE(PRINT,F4,Q-N,N); END;     00081300
IF OPT7=1 THEN                                                          00081400
BEGIN                                                                   00081500
COMMENT PRINT-OUT OF VARIABLES AFTER TRANSFORMATION AND/OR DELETION.;   00081600
IF OPT4=1 THEN BEGIN WRITE(PRINT,F39); MATRIX(NX,M,N,1.0); END          00081700
    ELSE BEGIN WRITE(PRINT,F5); MATRIX(NX,M,Q,1.0); END;                00081800
END;                                                                    00081900
IF FLAG3=1 THEN GO TO FINAL2;                                           00082000
    IJ~Q-N;                                                             00082100
IF OPT4=1 THEN FOR I LCTR M DO  MOVE(IJ,X[I,1],Y[I,1])                  00082200
    ELSE                                                                00082300
    BEGIN   FOR I LCTR M DO  MOVE(IJ,NX[I,1],Y[I,1]);                   00082400
        IF OPT2=1 THEN                                                  00082500
            BEGIN   FOR I LCTR M DO BEGIN NX[I,1]~1;                    00082600
                    MOVE(N,NX[I,IJ+1],NX[I,2]); END;                    00082700
                    N~N+1;   Q~Q+1;                                     00082800
            END                                                         00082900
            ELSE    FOR I LCTR M DO  MOVE(N,NX[I,IJ+1],NX[I,1]);        00083000
    END;                                                                00083100
IJ~OPT2+1;  HUNTER(M,N,IJ,NX,MMIN);                                     00083200
COMMENT THE MAX. AND MIN. VALUES OF X ARE IN MMIN.;                     00083300
FOR I LCTR M DO FOR J LCTR N DO RX[J,I]~NX[I,J];                        00083400
COMMENT DEVELOP X(T)WX MATRIX IN AX.;                                   00083500
FOR K LCTR N DO                                                         00083600
BEGIN   S~0;                                                            00083700
    FOR I~K STEP 1 UNTIL N DO                                           00083800
    BEGIN   R~0;                                                        00083900
        FOR J LCTR M DO                                                 00084000
        BEGIN                                                           00084100
            R~R+RX[K,J] | (S1~RX[I,J]) | (S2~W[J]);                     00084200
            S~ S+S1|S2;                                                 00084300
        END;                                                            00084400
        AX[I,K]~AX[K,I]~X2[I,K]~X2[K,I]~R;                              00084500
        MN[I]~ S;                                                       00084600
    END;                                                                00084700
END;                                                                    00084800
COMMENT CAL. MEANS AND VARIANCES OF DEPENDENT VARIABLES.;               00084900
    IJ~Q-N;                                                             00085000
FOR I LCTR IJ DO                                                        00085100
BEGIN   S~S1~S3~0;                                                      00085200
    FOR J LCTR M DO                                                     00085300
    BEGIN                                                               00085400
        S~ S+(R~Y[J,I])|(S2~W[J]);  S1~ S1+R|R|S2;  S3~S3+S2;           00085500
    END;                                                                00085600
    YMEAN[I]~S/S3;  YVAR[I]~(S1-S|S/S3)/(M-1);                          00085700
END;                                                                    00085800
COMMENT CAL. MEANS AND VARIANCES OF INDEPENDENT VARIABLES.;             00085900
J~OPT2+1;                                                               00086000
FOR I~J STEP 1 UNTIL N DO                                               00086100
BEGIN  MN[K~(I-J+1)]~ R~ MN[I]/S3; VAR[K]~(AX[I,I]-S3|R|R)/(M-1); END;  00086200
IF OPT9=1 THEN                                                          00086300
BEGIN                                                                   00086400
COMMENT PRINT-OUT OF NORMAL MATRIX.;                                    00086500
WRITE(PRINT[PAGE]); WRITE(PRINT,F6); MATRIX(AX,N,N,1.0);                00086600
END;                                                                    00086700
    FOR I LCTR N DO X1[I]~SQRT(AX[I,I]);                                00086800
    FOR I LCTR N DO FOR J~ I STEP 1 UNTIL N DO                          00086900
    ANX[J,I]~ ANX[I,J]~ AX[I,J]/(X1[I] | X1[J]);                        00087000
IF OPT8=1 THEN BEGIN                                                    00087100
    COMMENT COMPUTE AND PRINT - OUT THE CORRELATION MATRIX;             00087200
    I~N-OPT2;                                                           00087300
    J~0; WHILE J~J+1{I DO                                               00087400
    BEGIN                                                               00087500
        A1~J+OPT2;                                                      00087600
        K~J-1; WHILE K~K+1{I DO                                         00087700
        BEGIN                                                           00087800
            A2~K+OPT2;                                                  00087900
            X[J,K]~X[K,J]~(AX[A1,A2]-MN[J]|MN[K]|M)                     00088000
                        /((M-1)|SQRT(VAR[J]|VAR[K]));                   00088100
        END;                                                            00088200
    END;                                                                00088300
    WRITE(PRINT[PAGE]);  WRITE(PRINT,F7);  MATRIX(X,I,I,1.0);           00088400
END OPT8;                                                               00088500
COMMENT    CORRELATION MATRIX ANX INVERTED  ;                           00088600
INVERTMATRIX(ANX,N,S,ANX);                                              00088700
IF S < 1@-30 THEN BEGIN WRITE(PRINT,F7A); GO TO DONE; END;              00088800
COMMENT    INVERSE OF NORMAL MATRIX COMPUTED IN AX  ;                   00088900
FOR I LCTR N DO FOR J LCTR N DO AX[I,J]~ANX[I,J]/(X1[I]|X1[J]);         00089000
IF OPT10=1 THEN                                                         00089100
BEGIN                                                                   00089200
MATRIXMPY(ANX,X2,AX,N,N,N);                                             00089300
    WRITE(PRINT[PAGE]);                                                 00089400
    WRITE(PRINT,F8);  MATRIX(AX,N,N,1.0);                               00089500
    WRITE(PRINT,F8A);  MATRIX(ANX,N,N,1.0);                             00089600
S~S4~0;                                                                 00089700
FOR I LCTR N DO FOR J LCTR N DO                                         00089800
BEGIN  ANX[I,J]~ S1~ IF I=J THEN 1-ANX[I,J] ELSE -ANX[I,J];             00089900
    S~S+S1*2;                                                           00090000
END;                                                                    00090100
S1~SQRT(S);                                                             00090200
S5~S;                                                                   00090300
IF S>1 THEN                                                             00090400
BEGIN                                                                   00090500
    MATRIXMPY(X2,ANX,ANX,N,N,N);                                        00090600
    S~0;  FOR I LCTR N DO FOR J LCTR N DO S~S+X2[I,J]*2;                00090700
    IF S2~SQRT(S) > S1 THEN BEGIN S4~OPT10~1;                           00090800
    GO TO OVER4;  END;  S1~S2;                                          00090900
END;                                                                    00091000
COMMENT  CAL. NORM OF INVERSE MATRIX;                                   00091100
S~0;  FOR I LCTR N DO FOR J LCTR N DO S~S+AX[I,J]*2;                    00091200
    S2~SQRT(S);                                                         00091300
        S4~S2|S5|(SQRT(N)-1+1/(1-S1));                                  00091400
OVER4:  IF S1!0 THEN  WRITE(PRINT,F8B,S4/S1);                           00091500
END;                                                                    00091600
END DUMMYBLOCK;                                                         00091700
AGAIN:                                                                  00091800
    FOR I LCTR M DO YV[I]~Y[I,NOFY];                                    00091900
COMMENT CALCULATE MIN AND MAX OF THE DEPENDENT VAR.;                    00092000
    YMIN~YMAX~YV[1];                                                    00092100
    FOR J~2 STEP 1 UNTIL M DO BEGIN S~YV[J];                            00092200
    IF YMAX<S THEN YMAX~S;  IF YMIN>S THEN YMIN~S;  END;                00092300
COMMENT DEVELOP X(T)WY MATRIX;                                          00092400
FOR I LCTR N DO BEGIN R~0; FOR J LCTR M DO R~R+RX[I,J]|YV[J]|W[J];      00092500
    XTY[I]~R;   END;                                                    00092600
COMMENT CAL. PARAMETERS IN B.;                                          00092700
MATMPY(1,B,AX,XTY,N,N);                                                 00092800
COMMENT CAL. PREDICTIONS IN PR.;                                        00092900
MATMPY(1,PR,NX,B,M,N);                                                  00093000
COMMENT DEVELOP THE ANALYSIS OF VARIANCE TABLE;                         00093100
S~S1~0;                                                                 00093200
FOR I LCTR M DO BEGIN S~S+(YV[I]-PR[I])|(YV[I]-PR[I]);                  00093300
    S1~S1+YV[I];    END; DEVSS~S; TOTALSS~0; YBAR~S1/M;                 00093400
IF OPT2=1 THEN BEGIN FOR I LCTR M DO TOTALSS~TOTALSS+(YV[I]-YBAR)*2;END 00093500
    ELSE BEGIN FOR I LCTR M DO TOTALSS~YV[I]*2 + TOTALSS; END;          00093600
REGSS~TOTALSS-DEVSS;  RSS~DEVSS/(M-N);                                  00093700
COMMENT PRINT-OUT COEFF.,STD.ERROR,MIN.,MAX.,MEAN AND VARIANCE.;        00093800
WRITE(PRINT[PAGE]); WRITE(PRINT,F11,NOFY);                              00093900
WRITE(PRINT,F11A,YMIN,YMAX,YMEAN[NOFY],YVAR[NOFY]);                     00094000
IF OPT2=1 THEN BEGIN WRITE(PRINT,F12,B[1],SQRT(RSS|AX[1,1]));           00094100
WRITE(PRINT,F13,OUT1); END   ELSE  WRITE(PRINT,F13,OUT2);               00094200
COMMENT PRINT-OUT THE ANALYSIS OF VARIANCE TABLE.;                      00094300
WRITE(PRINT[PAGE]); WRITE(PRINT,F14);                                   00094400
BEGIN  REAL  YYYY;                                                      00094500
IF IND=0 AND REPSS=0 THEN                                               00094600
BEGIN                                                                   00094700
IF OPT2=1 THEN BEGIN WRITE(PRINT,F15,OUT4); WRITE(PRINT,F16,OUT5);      00094800
WRITE(PRINT,F17,M-1,TOTALSS); END                                       00094900
ELSE BEGIN  WRITE(PRINT,F15,OUT3); WRITE(PRINT,F16,OUT5);               00095000
WRITE(PRINT,F17,M,TOTALSS); END;                                        00095100
END;                                                                    00095200
IF IND=1 OR REPSS!0 THEN                                                00095300
BEGIN                                                                   00095400
IF REPSS>1 THEN FOR I LCTR M DO BEGIN XX[I]~NX[I,REPSS];TY[I]~YV[I]; END00095500
    ELSE FOR I LCTR M DO BEGIN XX[I]~RX[N,I]; TY[I]~YV[I]; END;         00095600
IF IND=1 THEN ORDERING(M,OBSNO,XX,TY,TPR);                              00095700
REPLICATION(M,XX,TY,TPR,OBSNO,XVAL,S,ONE,K);                            00095800
IF OPT2=1 THEN BEGIN WRITE(PRINT,F15,OUT4);                             00095900
IF S!0 THEN                                                             00096000
WRITE(PRINT,F16A,M-N-ONE,DEVSS-S,(DEVSS-S)/(M-N-ONE),                   00096100
        ((DEVSS-S)|ONE)/(S|(M-N-ONE)))                                  00096200
    ELSE WRITE(PRINT,F16,OUT5); IF S!0 THEN                             00096300
WRITE(PRINT,F16B,ONE,S,S/ONE); WRITE(PRINT,F17,M-1,TOTALSS);  END       00096400
    ELSE BEGIN  WRITE(PRINT,F15,OUT3);                                  00096500
IF S!0 THEN                                                             00096600
WRITE(PRINT,F16A,J~ (M-N-ONE),R~(DEVSS-S),R/J,(R| ONE)/(S|J))           00096700
    ELSE WRITE(PRINT,F16,OUT5);                                         00096800
IF S!0 THEN WRITE(PRINT,F16B,ONE,S,S/ONE);                              00096900
WRITE(PRINT,F17,M,TOTALSS); END;                                        00097000
IF S!0 THEN                                                             00097100
BEGIN                                                                   00097200
COMMENT PRINT OUT OF S.S. DUE TO REPLICATION AND CHI SQ. STATISTIC;     00097300
    I~N-OPT2;  WRITE(PRINT,F16C,I);  S2~S3~S4~S5~L~0;                   00097400
FOR I LCTR K DO                                                         00097500
BEGIN                                                                   00097600
IF R~OBSNO[I]!0 THEN WRITE(PRINT    ,F16D,XVAL[I],R,TPR[I],TPR[I]/R);   00097700
    IF S6~TPR[I]!0 THEN                                                 00097800
        BEGIN                                                           00097900
        S2~ S2+1/R; S3~S3+R; L~L+1; S4~S4+R|LN(S6); S5~S5+R|S6;         00098000
        END;                                                            00098100
END;                                                                    00098200
IF L>1 THEN                                                             00098300
BEGIN                                                                   00098400
    R~1+(1/(3|(L-1)))|(S2-1/S3);                                        00098500
    DEVMIN ~(1.0/R) | (S3|LN(S5/S3)-S4);                                00098600
WRITE(PRINT,F16E,L -1,DEVMIN);                                          00098700
END;                                                                    00098800
END;                                                                    00098900
END;                                                                    00099000
END  YYYY DUMMYBLOCK;                                                   00099100
WRITE(PRINT,F18,OUT6);                                                  00099200
IF OPT11=1 THEN                                                         00099300
BEGIN                                                                   00099400
COMMENT DEVELOP AND PRINT-OUT THE VAR.-COV. MATRIX;                     00099500
WRITE(PRINT[PAGE]); WRITE(PRINT,F21); MATRIX(AX,N,N,RSS);               00099600
COMMENT DEVELOP AND PRINT-OUT THE CORRELATION MATRIX;                   00099700
FOR I LCTR N DO X1[I]~ SQRT(AX[I,I]);                                   00099800
FOR I LCTR N DO FOR J~ I STEP 1 UNTIL N DO                              00099900
    ANX[J,I]~ANX[I,J]~AX[I,J]/(X1[I] | X1[J]);                          00100000
WRITE(PRINT[PAGE]); WRITE(PRINT,F22); MATRIX(ANX,N,N,1.0);              00100100
COMMENT DEVELOP AND PRINT-OUT THE T-VALUES AND CONFIDENCE LIMITS;       00100200
WRITE(PRINT[PAGE]); WRITE(PRINT,F23,LEV1,LEV1,LEV2,LEV2);               00100300
IF OPT2=0 THEN WRITE(PRINT,F24,OUT8) ELSE                               00100400
    BEGIN   WRITE(PRINT,F24A,OUT8A);                                    00100500
            WRITE(PRINT,F24,OUT8B);  END;                               00100600
WRITE(PRINT,F24B,YMAX-YMIN);                                            00100700
COMMENT BREAKDOWN OF S.S.;  WRITE(PRINT[PAGE]); WRITE(PRINT,F25); ONE~1;00100800
IF OPT2=1 THEN  WRITE(PRINT,F26,OUT10)  ELSE  WRITE(PRINT,F26,OUT9);    00100900
WRITE(PRINT,F27,OUT5);                                                  00101000
END;                                                                    00101100
BEGIN   REAL DUMMY2;                                                    00101200
LABEL OVER1;                                                            00101300
COMMENT     VARIANCES OF PREDICTIONS CALCULATED  ;                      00101400
I~0; WHILE I~I+1{M DO                                                   00101500
BEGIN                                                                   00101600
    S~0;                                                                00101700
    J~0; WHILE J~J+1{N DO XX[J]~NX[I,J];                                00101800
    MATMPY(2,XX,AX,TY,N,N);                                             00101900
    J~0; WHILE J~J+1{N DO S~S+XX[J] | TY[J];                            00102000
TPR[I]~(S+1)|RSS;                                                       00102100
END;                                                                    00102200
COMMENT    PRINT-OUT OF RESIDUAL TABLE AND VARIANCES  ;                 00102300
WRITE(PRINT[PAGE]);  WRITE(PRINT,F19);  WRITE(PRINT    ,F20,OUT7);      00102400
COMMENT DEVIATIONS CALCULATED.;                                         00102500
FOR I LCTR M DO DEV[I]~YV[I]-PR[I];                                     00102600
    DEVMAX~DEVMIN~DEV[1];                                               00102700
    FOR J~2 STEP 1 UNTIL M DO BEGIN S~DEV[J];                           00102800
    IF DEVMAX<S THEN DEVMAX~S; IF DEVMIN>S THEN DEVMIN~S; END;          00102900
IF M>12 THEN                                                            00103000
BEGIN                                                                   00103100
COMMENT FREQUENCY TABLE FOR RESIDUALS;                                  00103200
K~12;                                                                   00103300
S~(DEVMAX-DEVMIN)/K;                                                    00103400
FOR I LCTR K DO BEGIN TY[I]~DEVMIN+I|S;  OBSNO[I]~0; END;               00103500
    OBSNO[0]~0;  TY[0]~DEVMIN;  TY[K]~DEVMAX;                           00103600
FOR I LCTR M DO BEGIN S1~DEV[I];                                        00103700
    FOR J LCTR K DO                                                     00103800
    IF S1-TY[J]{0 THEN BEGIN OBSNO[J]~OBSNO[J]+1;                       00103900
    GO TO OVER1;  END;                                                  00104000
OVER1: END ILOOP;                                                       00104100
    WRITE(PRINT[PAGE]);  WRITE(PRINT,RESFMT);                           00104200
    WRITE(PRINT,RESFMT1,FOR I LCTR K DO                                 00104300
            [TY[I-1],TY[I],OBSNO[I]]);                                  00104400
END FREQUENCY TABLE;                                                    00104500
IF NOLAG!0 THEN                                                         00104600
BEGIN                                                                   00104700
COMMENT LAG CORRELATIONS CAL. AND PRINTED-OUT;                          00104800
LAGCORR(M,NOLAG,DEV,DEV,XX);                                            00104900
WRITE(PRINT[PAGE]); WRITE(PRINT,LAGFMT);                                00105000
WRITE(PRINT,LAGFMT1,FOR I LCTR NOLAG DO [I,XX[I]]);                     00105100
END;                                                                    00105200
END DUMMY2;                                                             00105300
FLAG5~FLAG6~0;                                                          00105400
ORDERS:                                                                 00105500
BEGIN    REAL DUMMY3;                                                   00105600
LABEL OVER2;                                                            00105700
IF OPT12=1 THEN                                                         00105800
BEGIN                                                                   00105900
COMMENT TABLES OF RESIDUALS WHILE ORDERING CALCULATED AND PRINTED-OUT.; 00106000
FOR I LCTR NORD DO                                                      00106100
    BEGIN   L~ ORD[I];  WRITE(PRINT[PAGE]); WRITE(PRINT,F32,L);         00106200
    YMIN~MMIN[L,2];  YMAX~MMIN[L,1];                                    00106300
    WRITE(PRINT,F33,L); IF OPT2=1 THEN L~L+1;                           00106400
    FOR J LCTR M DO BEGIN TY[J]~YV[J]; TPR[J]~PR[J]; END;               00106500
    IF FLAG6=1 THEN FOR J LCTR M DO XX[J]~NX[J,L]                       00106600
    ELSE IF L>N THEN FOR J LCTR M DO XX[J]~NX[J,N+1]                    00106700
ELSE FOR J LCTR M DO  XX[J]~RX[L,J];                                    00106800
    ORDERING(M,OBSNO,XX,TY,TPR);                                        00106900
        FOR J LCTR M DO DEV[J]~TY[J]-TPR[J];                            00107000
    WRITE(PRINT    ,F34,OUT11);                                         00107100
    IF FLAG6=1 THEN                                                     00107200
    BEGIN                                                               00107300
        DEVMAX~DEVMIN~DEV[1];                                           00107400
        FOR J~2 STEP 1 UNTIL M DO BEGIN S~DEV[J];                       00107500
        IF DEVMAX<S THEN DEVMAX~S; IF DEVMIN>S THEN DEVMIN~S; END;      00107600
    END;                                                                00107700
IF M}20 THEN                                                            00107800
BEGIN                                                                   00107900
FOR J LCTR  M DO OBSNO[J]~NEGA[J]~0;                                    00108000
    JJ~1;  KK~0;                                                        00108100
OVER2:  LL~SIGN(DEV[JJ]);                                               00108200
    JJ~JJ+1;   J~1;                                                     00108300
    WHILE SIGN(DEV[JJ])=LL AND JJ{M DO BEGIN JJ~JJ+1; J~J+1; END;       00108400
    IF LL}0 THEN OBSNO[J]~OBSNO[J]+1                                    00108500
            ELSE NEGA[J]~NEGA[J]+1;                                     00108600
    IF J>KK THEN KK~J;                                                  00108700
    IF JJ{M THEN GO TO OVER2;                                           00108800
J~K~L~IJ~0;                                                             00108900
FOR MM LCTR KK DO BEGIN J~J+OBSNO[MM]; K~K+NEGA[MM];                    00109000
    IJ~IJ+MM|OBSNO[MM];  L~L+MM|NEGA[MM];  END;                         00109100
WRITE(PRINT[PAGE]);  WRITE(PRINT,RESFMT2);                              00109200
S~IJ/M;  S1~L/M;                                                        00109300
JJ~LL~0;                                                                00109400
S4~M|S1;  S5~M|S;  S2~S4|S1;  S3~S5|S;                                  00109500
FOR MM LCTR KK DO                                                       00109600
BEGIN  J~J-JJ;  K~K-LL;                                                 00109700
    S2~S2|S;  S3~S3|S1;  S4~S4|S;  S5~S5|S1;                            00109800
    A1~OBSNO[MM];  A2~NEGA[MM];                                         00109900
    WRITE(PRINT,RESFMT3,MM,A1,S2,A2,S3,A1+A2,S2+S3,J,S4,K,S5,J+K,S4+S5);00110000
    JJ~A1;  LL~A2;                                                      00110100
END TABLE;                                                              00110200
L~.5|(M+1-1.645|SQRT(M-1));                                             00110300
K~(LN(M)+2.97011)/0.69315;                                              00110400
WRITE(PRINT,RESFMT4,L,  K);                                             00110500
END MTOOSMALL;                                                          00110600
COMMENT PLOT ROUTINES CALLED FOR PLOT OF X VS. RESIDUALS;               00110700
STAR~"X";                                                               00110800
YHEAD[1]~"DEVIAT";  YHEAD[2]~"IONS  ";                                  00110900
    VPLOT1(5,6,6,15);                                                   00111000
    VPLOT2(YMAX,YMIN,DEVMAX,DEVMIN);                                    00111100
    FOR J LCTR M DO                                                     00111200
    VPLOT3(STAR,XX[J],DEV[J]);                                          00111300
    VPLOT4(YHEAD,2,14,XHEAD);                                           00111400
IF NOLAG!0 THEN                                                         00111500
    BEGIN                                                               00111600
        LAGCORR(M,NOLAG,DEV,DEV,XX);                                    00111700
    WRITE(PRINT,LAGFMT2);                                               00111800
        WRITE(PRINT,LAGFMT1,FOR J LCTR NOLAG DO [J,XX[J]]);             00111900
    END LAGS;                                                           00112000
    END I LOOP;                                                         00112100
IF FLAG5=1 THEN GO TO FINAL3;                                           00112200
END ORDERS;                                                             00112300
END DUMMY3;                                                             00112400
COMMENT STORE B@S AND RESIDUAL VARIANCE.;                               00112500
FOR I LCTR N DO NB[I,NOFY]~ B[I]; RESVAR[NOFY]~RSS;                     00112600
IF Q-N>1 THEN                                                           00112700
BEGIN                                                                   00112800
COMMENT REPEAT WITH NEW DEPENDENT VARIABLE.;                            00112900
    NOFY~ NOFY+1;   Q~Q-1;                                              00113000
GO TO AGAIN;                                                            00113100
END;                                                                    00113200
IF OPT13 !1 THEN GO TO DONE;                                            00113300
COMMENT EST(Y) AND VAR(Y) WANTED AT NEW POINTS.;                        00113400
IF OPT1=1 THEN  H~H-1;                                                  00113500
COMMENT INPUT NEW DATA.;                                                00113600
IF OPT4=0 AND OPT3=0 THEN READ(READER,/,IN4B)                           00113700
    ELSE READ(READER,/,IN4C);                                           00113800
M~NVEC; N~ORIGN;    Q~ORIGQ;                                            00113900
COMMENT PRINT-OUT NEW DATA.;                                            00114000
WRITE(PRINT[PAGE]);  WRITE(PRINT,F35);                                  00114100
IF OPT4=0 AND OPT3=0 THEN MATRIX(NX,M,H,1.0) ELSE MATRIX(X,M,H,1.0);    00114200
IF OPT4=1 THEN BEGIN FLAG3~1; GO TO POLY; END;                          00114300
IF OPT3=1 THEN BEGIN FLAG3~1; GO TO TRANS; END;                         00114400
FINAL1:                                                                 00114500
    IF OPT6=1 THEN BEGIN FLAG3~1; NEWN~0; NEWQ~Q; GO TO DELETION; END;  00114600
FINAL2:                                                                 00114700
NOFY ~ IJ ~ Q-N ;                                                       00114800
IF OPT4=1 THEN FOR I LCTR M DO  MOVE(IJ,X[I,1],Y[I,1])                  00114900
    ELSE                                                                00115000
    BEGIN FOR I LCTR M DO MOVE(IJ,NX[I,1],Y[I,1]);                      00115100
        IF OPT2=1 THEN BEGIN FOR I LCTR M DO BEGIN NX[I,1]~ 1;          00115200
        MOVE(N,NX[I,IJ+1],NX[I,2]);  END;                               00115300
        N~N+1;  Q~Q+1;  END                                             00115400
        ELSE  FOR I LCTR M DO MOVE(N,NX[I,IJ+1],NX[I,1]);               00115500
    END;                                                                00115600
IF OPT12 =1 THEN BEGIN IJ~OPT2+1; HUNTER(M,N,IJ,NX,MMIN); END;          00115700
    JK~1;                                                               00115800
FINAL3: FOR C~JK STEP 1 UNTIL NOFY DO                                   00115900
BEGIN   S2~0;   WRITE(PRINT[PAGE]); WRITE(PRINT,F36,C);                 00116000
    FOR J LCTR N DO  B[J]~NB[J,C];                                      00116100
    FOR I LCTR M DO BEGIN  S~S1~0;                                      00116200
    FOR J LCTR N DO BEGIN XX[J]~NX[I,J]; S1~S1+B[J]|XX[J]; END;         00116300
    MATMPY(2,XX,AX,TY,N,N); FOR J LCTR N DO S~S+XX[J]| TY[J];           00116400
    S~(S+1)|RESVAR[C];  S3~Y[I,C];  PR[I]~S1;                           00116500
    WRITE(PRINT,F37,I,S3,S1,S3-S1,S);  S2~S2 + (S3-S1)| (S3-S1); END;   00116600
    WRITE(PRINT,F38,S2);                                                00116700
    JK~C+1;                                                             00116800
    IF OPT12=1 THEN BEGIN FLAG5~1; FOR I LCTR M DO YV[I]~ Y[I,C];       00116900
    FLAG6~1;  GO TO ORDERS;  END;                                       00117000
END;                                                                    00117100
DONE:  END;                                                             00117200
GO TO START1;                                                           00117300
FINISH: TIMEIT(PRINT);                                                  00117400
END PROGRAM.                                                            00117500
