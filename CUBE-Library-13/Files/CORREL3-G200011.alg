         BEGIN                                                          00000100
COMMENT        CUBE LIBRARY NUMBER IS G200011.  THE PROGRAM NAME IS     00000200
               "B0304/TTY".  THIS VERSION DATED 5/3/68;                 00000300
COMMENT       MULTIPLE REGRESSION AND CORRELATION ANALYSIS              00000400
              CHARLES L. CLARK                                          00000500
              PROFESSIONAL SERVICES DIVISIONAL GROUP                    00000600
              BURROUGHS CORPORATION                                     00000700
                MODIFIED FOR USE THROUGH TELETYPE. F LUCAS 5/25/67 ;    00000800
ALPHA FILE IN  TTIN 14 (2,8);                                           00000900
ALPHA FILE OUT TTOUT 14 (2,8);                                          00001000
FILE IN        FID DISK SERIAL "AAAAAAA" "BBBBBBB" (2,10,30);           00001100
DEFINE    RTT=READ(TTIN(STA)#, WTT=WRITE(TTOUT(STA)#, PRINT=TTOUT(STA)#;00001200
INTEGER       H, I, M, N, Q ;                                           00001300
INTEGER        STA,NCHAR,HWS;                                           00001400
ALPHA          ID1,ID2,ANOTH,AAA1,AAA2,BBB1,BBB2;                       00001500
ARRAY          TBUF[0:7];                                               00001600
INTEGER ARRAY ID[0:30];                                                 00001700
BOOLEAN       TR,MEAN,VAR,NORM,INV,RES,PCC,CC,CCINV,SWICH ;             00001800
LABEL         START, LL1,NOMO ;                                         00001900
FORMAT IN      F1(X8,*A1),   F2(X8,I*),  F4(X8,A*), F3(X8,A1,A6);       00002000
FORMAT IN      F5(X8,L*);                                               00002100
FORMAT IN      FORM3(4E13.6);                                           00002200
FORMAT OUT     FDM(X8,"{!NUMBER OF DATA POINTS = ~"),                   00002300
               FDN(X8,"{!NUMBER OF INDEPENDENT VARIABLES = ~"),         00002400
               FDQ(X8,"{!TOTAL NO. OF INDEPENDENT AND DEPENDENT ",      00002500
                      "VARIABLES = ~"),                                 00002600
               FDH(X8,"{!TOTAL NO. OF VARIABLES CONTRIBUTING TO EACH",  00002700
                     " OBS = ~"),                                       00002800
               FTF(X8,"{!!ENTER THE WORDS TRUE OR FALSE FOR THE ",      00002900
                  "FOLLOWING{!~"),                                      00003000
               FTRAN(X8,"{!ENTER TRANSFORMATION NO. ~"),                00003100
               FNAM(X8,"{!ENTER YOUR DATA NAMES.{!~"),                  00003200
               FPFX(X8,"PREFIX = ~"),                                   00003300
               FSFX(X8,"{!SUFFIX = ~"),                                 00003400
               FNXT(X8,"{!IF YOU WANT TO DO ANOTHER SET TYPE YES{!~"),  00003500
               FSTR(X8,"{!ENTER YOUR PROB. ID-UP TO 30 CHAR{!~"),       00003600
               FERO(X8,"{!YOUR FILE NOT FOUND. TRY AGAIN.{!~");         00003700
FORMAT OUT     FCRLF(X8,"{!~"),                                         00003800
               FLF(X8,"!~"),                                            00003900
               TITL1(X8,"{!GENERAL MULTIPLE REGRESSION AND ",           00004000
                  "CORRELATION ANALYSIS~"),                             00004100
               TITL2(X8,"{!!!",*A1,"{!~"),                              00004200
               TITL3(X8,"!SAMPLE SIZE",X2,I5,"{!~"),                    00004300
               TITL4(X8,"NO. OF INDEPENDENT VARIABLES",X2,I3,"{!~"),    00004400
               TITL5(X8,"NO. OF DEPENDENT VARIABLES",X4,I3,"{!~"),      00004500
               FRM1(X8,"!NORMAL MATRIX{!~"),                            00004600
               FRM2A(X8,"{!ROW",I3,"{!~"),                              00004700
               FRM2B(X8,3(   I3,X1,E13.6),"{!~"),                       00004800
               FRM3(X8,"{!!MEANS OF INDEPENDENT AND DEPENDENT",         00004900
                   " VARIABLE{!~"),                                     00005000
               FRM4(X8,3(   I3,X1,E13.6),"{!!~"),                       00005100
               FRM5(X8,"{!VARIANCES OF INDEPENDENT AND DEPENDENT",      00005200
                  " VARIABLES{!~"),                                     00005300
               FRM6(X8,"{!THE INVERSE MATRIX{!~"),                      00005400
             FRM7(X8,"{!!PRODUCT OF NORMAL AND INVERSE MATRICES{!~"),   00005500
               FRM8(X8,"{!!DEPENDENT VARIABLE NO.",I3,"{!~"),           00005600
               FRM9A(X8,"{!REGRESSION COEFFICIENTS{!!~"),               00005700
               FRM9B(X8,"THE CONSTANT TERM",X3,E15.8,"{!~"),            00005800
               FRM10A(X8,"{!THE ERROR SUM SQUARE IS",X3,E15.8,"{!~"),   00005900
               FRM10B(X8,"{!THEN ERROR MEAN SQUARE IS",X2,E15.8,"{!~"), 00006000
               FRM10C(X8,"{!THE STANDARD ERROR OF ESTIMATE IS",         00006100
               X3,E15.8,"{!~"),                                         00006200
               FRM10D(X8,"{!THE COEF. OF DETERMINATION IS",             00006300
               X3,E15.8,"{!!~"),                                        00006400
               FRM10E(X8,"THE MULTIPLE CORRELATION COEF. IS",           00006500
               X2,E15.8,"{!~"),                                         00006600
               FRM11(X8,"{!STANDARD DEVIATION OF THE REGRESSION",       00006700
                  " COEFFICIENT{!~"),                                   00006800
               FRM12(X8,"{!T VALUE OF REGRESSION COEFFICIENTS{!~"),     00006900
               FRM13A(X8,"{!!ANALYSIS OF VARIANCE{!~"),                 00007000
               FRM13B(X8,"FOR THE MULTIPLE LINEAR REGRESSION{!~"),      00007100
               FRM13C(X8,"{!SOURCE OF VAR  DF    SUM OF     ", X3,      00007200
                 "MEAN",X10,"{!~"),                                     00007300
               FRM13D(X28,"SQUARES      SQUARES      RATIO{!~"),        00007400
               FRM13E(X8,"DUE TO REGR.  ",I3,3E13.6,"~"),               00007500
               FRM13F(X8,"DEV ABOUT REGR ",I2,2E13.6,"~"),              00007600
               FRM13G(X8,"{!",X9,"TOTAL",I3,E13.6"{!!!~"),              00007700
               FRM14A(X20,"!TABLE OF RESIDUALS{!!~"),                   00007800
               FRM14B(X8,"OBS    Y CALCULATED",X4,"Y OBSERVED",X7,      00007900
                  "RESIDUAL{!~"),                                       00008000
               FRM15(X8,I3,X1,3E16.8,"{!~"),                            00008100
               FRM16A(X8,"{!TEST OF EXTREME RESIDUALS{!~"),             00008200
               FRM16B(X8,"RANGE OF RESIDUALS",X20,E15.8,"{!~"),         00008300
               FRM16C(X8,"RANGE / STANDARD ERROR OF ESTIMATE",X4,       00008400
                    E15.8,"{!~"),                                       00008500
               FRM17(X8,"CORRELATION COEFFICIENTS{!~"),                 00008600
               FRM18(X8,"{!!INVERSE OF CORRELATION MATRIX{!~"),         00008700
               FRM19(X8,"{!!PARTIAL CORRELATION COEFFICIENTS{!~");      00008800
PROCEDURE      BLANKIT(XX);                                             00008900
ALPHA          XX;                                                      00009000
          BEGIN                                                         00009100
            IF XX.[12:6]="~" THEN XX.[12:36]~"      " ELSE              00009200
            IF XX.[18:6]="~" THEN XX.[18:30]~"     " ELSE               00009300
            IF XX.[24:6]="~" THEN XX.[24:24]~"    " ELSE                00009400
            IF XX.[30:6]="~" THEN XX.[30:18]~"   " ELSE                 00009500
            IF XX.[36:6]="~" THEN XX.[36:12]~"  " ELSE                  00009600
            IF XX.[42:6]="~" THEN XX.[42:06]~" " ;                      00009700
          END OF BLANKIT;                                               00009800
INTEGER STREAM PROCEDURE FINDGP(B);                                     00009900
          BEGIN                                                         00010000
LABEL          HERE;                                                    00010100
               SI~B; 40(IF SC="~" THEN JUMP OUT 1 TO HERE;              00010200
               SI~SI+1; TALLY~TALLY+1);                                 00010300
HERE:          FINDGP~TALLY;                                            00010400
          END;                                                          00010500
SWITCH FORMAT  SWF~(X8,"{!ANY TRANSFORMATIONS DESIRED  ~"),             00010600
                    (X8,"{!PRINTOUT OF MEANS  ~"),                      00010700
                    (X8,"{!PRINTOUT OF VARIANCES  ~"),                  00010800
                    (X8,"{!PRINTOUT OF NORMAL MATRIX  ~"),              00010900
                    (X8,"{!PRINTOUT OF INV NORMAL MATRIX AND",          00011000
                            " PROD WITH NORM  ~"),                      00011100
                    (X8,"{!RESIDUAL TABLE DESIRED  ~"),                 00011200
                    (X8,"{!PARTIAL CORRELATIONS  ~"),                   00011300
                   (X8,"{!AUGMENTED CORREL. COEFFICIENT MATRIX  ~"),    00011400
                    (X8,"{!INVERSE OF AUG MATRIX  ~");                  00011500
LIST           LST1(NCHAR,TR), LST2(NCHAR,MEAN), LST3(NCHAR,VAR),       00011600
               LST4(NCHAR,NORM), LST5(NCHAR,INV), LST6(NCHAR,RES),      00011700
               LST7(NCHAR,PCC), LST8(NCHAR,CC), LST9(NCHAR,CCINV);      00011800
SWITCH LIST    SWL~LST1,LST2,LST3,LST4,LST5,LST6,LST7,LST8,LST9;        00011900
PROCEDURE      READIN(FORMOUT,W);                                       00012000
FORMAT OUT     FORMOUT;                                                 00012100
INTEGER        W;                                                       00012200
          BEGIN                                                         00012300
               WTT,FORMOUT);                                            00012400
               RTT,4,TBUF[*]);                                          00012500
               NCHAR~FINDGP(TBUF[1]);                                   00012600
               READ(TBUF[*],F2,NCHAR,W);                                00012700
          END OF READIN;                                                00012800
PROCEDURE      BOOLIN(FORM,OPT);                                        00012900
LIST           OPT;                                                     00013000
FORMAT         FORM;                                                    00013100
          BEGIN                                                         00013200
               WTT,FORM);                                               00013300
               RTT,4,TBUF[*]);                                          00013400
               NCHAR~FINDGP(TBUF[1]);                                   00013500
               READ(TBUF[*],F5,OPT);                                    00013600
          END OF BOOLIN;                                                00013700
               STA~STATUS(TBUF[*]);                                     00013800
               STA.[9:9]~TBUF[0].[9:9];                                 00013900
START:                                                                  00014000
               WTT,FSTR);                                               00014100
               RTT,8,TBUF[*]);                                          00014200
               NCHAR~FINDGP(TBUF[1]);                                   00014300
               HWS~NCHAR-1;                                             00014400
               READ(TBUF[*],F1,NCHAR,FOR I~0 STEP 1 UNTIL HWS DO        00014500
                         ID[I]);                                        00014600
               READIN(FDM,M);                                           00014700
               READIN(FDN,N);                                           00014800
               READIN(FDQ,Q);                                           00014900
               READIN(FDH,H);                                           00015000
               WTT,FTF);                                                00015100
               FOR I~ 0 STEP 1 UNTIL 8 DO BOOLIN(SWF[I],SWL[I]);        00015200
               WTT,FCRLF);                                              00015300
              SWICH ~ FALSE ;                                           00015400
LL1:     BEGIN                                                          00015500
INTEGER       J, K, L, P ;                                              00015600
FILE          TAPEA DISK SERIAL [1:100] (2,Q+1);                        00015700
ARRAY         X[0:Q],YSQ[0:Q-N],A[0:N+1,0:Q],B[0:H],SB[0:N] ,           00015800
              T[0:Q] ;                                                  00015900
REAL          C, RSS, RMS, F, ERS, EMS, SE, CD, MCC, HI, LO, R, S ;     00016000
LABEL         RERED, RESDU ;                                            00016100
LIST          TRAN (FOR J ~ 1 STEP 1 UNTIL Q DO T[J]) ,                 00016200
              DATA (FOR I ~ 1 STEP 1 UNTIL H DO B[I]) ,                 00016300
              OUT1 (FOR J ~ 0 STEP 1 UNTIL 4 DO ID[J], M, N,            00016400
                    L) ,                                                00016500
              OUT2 (I, FOR J ~ I STEP 1 UNTIL N DO                      00016600
                    [J, A[I,J]]) ,                                      00016700
              OUT3 (FOR I ~ 1 STEP 1 UNTIL N DO [I, B[I]]) ,            00016800
              OUT4 (FOR I ~ 1 STEP 1 UNTIL N DO [I, SB[I]]),            00016900
              OUT5 (FOR I ~ 1 STEP 1 UNTIL N DO [I, X[I]]) ,            00017000
              OUT6 (I, FOR J ~ I STEP 1 UNTIL N DO                      00017100
                    [J, X[J]]) ;                                        00017200
PROCEDURE      INVERT(N, B);                                            00017300
VALUE          N;                                                       00017400
INTEGER        N;                                                       00017500
ARRAY          B[0,0];                                                  00017600
COMMENT        THIS PROCEDURE INVERTS A SYMMETRIC MATRIX                00017700
               A[I,J] OF WHICH THE UPPER HALF IS GIVEN.                 00017800
               ALL FLOATING POINT OPERATIONS ARE DONE IN                00017900
               DOUBLE PRECISION;                                        00018000
          BEGIN                                                         00018100
INTEGER        I,J,K,L;                                                 00018200
ARRAY          AH, AL[0:N,0:N], ZH, ZL[0:N];                            00018300
REAL           DGH, DGL, RH, RL;                                        00018400
               FOR I ~ 1 STEP 1 UNTIL N DO                              00018500
               FOR J ~ 1 STEP 1 UNTIL N DO                              00018600
               DOUBLE(B[I,J],0,~,AH[I,J],AL[I,J]);                      00018700
               FOR K ~ 1 STEP 1 UNTIL N-1 DO                            00018800
          BEGIN                                                         00018900
               DOUBLE(AH[K,K], AL[K,K],~,DGH, DGL);                     00019000
               FOR I ~ K+1 STEP 1 UNTIL N DO                            00019100
          BEGIN                                                         00019200
               DOUBLE(AH[K,I], AL[K,I], DGH, DGL,/,~,RH, RL);           00019300
               FOR J ~ I STEP 1 UNTIL N DO                              00019400
               DOUBLE(AH[I,J], AL[I,J], AH[K,J], AL[K,J], RH, RL,|,-,~, 00019500
               AH[I,J], AL[I,J]);                                       00019600
               FOR L ~ 1 STEP 1 UNTIL K-1 DO                            00019700
               DOUBLE(AH[L,I], AL[L,I], AH[L,K], AL[L,K], RH, RL,|,-,~, 00019800
               AH[L,I], AL[L,I]);                                       00019900
               AH[K,I] ~ - RH;                                          00020000
               AL[K,I] ~ - RL;                                          00020100
          END                                                           00020200
          END  ;                                                        00020300
COMMENT        BACKWARD PASS;                                           00020400
               FOR K ~ 1 STEP 1 UNTIL N DO                              00020500
          BEGIN                                                         00020600
               FOR J ~ 1 STEP 1 UNTIL N DO                              00020700
               DOUBLE(AH[K,J], AL[K,J], AH[J,J], AL[J,J],/,~,           00020800
               ZH[J], ZL[J]);                                           00020900
               DOUBLE(,1.0, AH[K,K], AL[K,K],/,~, ZH[K], ZL[K]);        00021000
               FOR I ~ K STEP 1 UNTIL N DO                              00021100
          BEGIN                                                         00021200
               FOR J ~ I+1 STEP 1 UNTIL N DO                            00021300
               DOUBLE(ZH[I], ZL[I], ZH[J], ZL[J], AH[I,J], AL[I,J],|,+, 00021400
               ~, ZH[I], ZL[I]);                                        00021500
               B[K,I] ~ ZH[I]                                           00021600
          END                                                           00021700
          END                                                           00021800
          END  INVERT;                                                  00021900
PROCEDURE      INVPDS(N,B,DIAGONAL);                                    00022000
VALUE          N;                                                       00022100
INTEGER        N;                                                       00022200
REAL ARRAY     B[0,0], DIAGONAL[0];                                     00022300
COMMENT        DOUBLE PRECISION VERSION OF SYMMETRIC MATRIX             00022400
               INVERSION PROCEDURE;                                     00022500
          BEGIN                                                         00022600
INTEGER        I,J,K,I1,L;                                              00022700
REAL           DIAGH, DIAGL, QH, QL;                                    00022800
REAL ARRAY     AH, AL[0:N,0:N], TEMPH, TEMPL[0:N];                      00022900
LABEL          IN1, IN2, IN3;                                           00023000
               FOR I ~ 1 STEP 1 UNTIL N DO                              00023100
               FOR J ~ 1 STEP 1 UNTIL N DO                              00023200
               DOUBLE(B[I,J],0,~,AH[I,J],AL[I,J]);                      00023300
IN1:           FOR I ~ N STEP -1 UNTIL 1 DO                             00023400
          BEGIN                                                         00023500
               DIAGONAL[I] ~ B[I,I];                                    00023600
               FOR K~ I+1 STEP 1 UNTIL N DO                             00023700
               DOUBLE(AH[I,K], AL[I,K], AH[K,K], AL[K,K], |,~,          00023800
               TEMPH[K], TEMPL[K]);                                     00023900
               FOR J ~ I STEP -1 UNTIL 1 DO                             00024000
          BEGIN                                                         00024100
               DOUBLE(,0.0, ~, QH, QL);                                 00024200
               FOR K ~ I+1 STEP 1 UNTIL N DO                            00024300
               DOUBLE(QH, QL, AH[J,K], AL[J,K], TEMPH[K], TEMPL[K],     00024400
               |, +, ~, QH, QL);                                        00024500
               IF I=J THEN                                              00024600
          BEGIN                                                         00024700
               DOUBLE (AH[J,I], AL[J,I], QH, QL, -, ~, DIAGH, DIAGL);   00024800
               DOUBLE(DIAGH, DIAGL, ~, AH[J,I], AL[J,I])                00024900
          END                                                           00025000
               ELSE                                                     00025100
               DOUBLE(AH[J,I], AL[J,I], QH, QL, -, DIAGH, DIAGL, /, ~,  00025200
               AH[J,I], AL[J,I])                                        00025300
          END                                                           00025400
          END  ;                                                        00025500
IN2:           FOR I ~ N STEP -1 UNTIL 1 DO                             00025600
          BEGIN                                                         00025700
               I1 ~ I+1;                                                00025800
               DOUBLE(,1.0, AH[I,I], AL[I,I],  /, ~, AH[I,I], AL[I,I]); 00025900
               FOR J~ N STEP -1 UNTIL I1 DO                             00026000
          BEGIN                                                         00026100
               DOUBLE(,0.0, ~, QH, QL);                                 00026200
               L ~ J-1;                                                 00026300
               FOR K ~ I1 STEP 1 UNTIL L DO                             00026400
               DOUBLE(QH, QL, AH[I,K], AL[I,K], AH[K,J], AL[K,J],       00026500
               |, +, ~, QH, QL);                                        00026600
               DOUBLE(AH[I,J], AL[I,J], QH, QL,+, ~, AH[I,J], AL[I,J]); 00026700
               AH[I,J] ~ -AH[I,J];                                      00026800
               AL[I,J] ~ -AL[I,J]                                       00026900
          END                                                           00027000
          END  ;                                                        00027100
IN3:           FOR I~ N STEP -1 UNTIL 1 DO                              00027200
          BEGIN                                                         00027300
               I1 ~ I-1;                                                00027400
               DOUBLE(AH[I,I], AL[I,I], ~, DIAGH, DIAGL);               00027500
               FOR K ~ 1 STEP 1 UNTIL I1 DO                             00027600
               DOUBLE(AH[K,K], AL[K,K], AH[K,I], AL[K,I], |, ~,         00027700
               TEMPH[K], TEMPL[K]);                                     00027800
               FOR J ~ N STEP -1 UNTIL I DO                             00027900
          BEGIN                                                         00028000
               DOUBLE(,0.0, ~, QH, QL);                                 00028100
               FOR K ~ 1 STEP 1 UNTIL I1 DO                             00028200
               DOUBLE(QH, QL, AH[K,J], AL[K,J], TEMPH[K], TEMPL[K],     00028300
               |, +, ~,  QH, QL);                                       00028400
               IF I=J THEN                                              00028500
               DOUBLE(AH[I,J], AL[I,J], QH, QL, +, ~, AH[I,J], AL[I,J]) 00028600
               ELSE                                                     00028700
               DOUBLE(QH, QL, AH[I,J], AL[I,J], DIAGH, DIAGL, |,+,~,    00028800
               AH[I,J], AL[I,J])                                        00028900
          END                                                           00029000
          END  ;                                                        00029100
               FOR I ~ 1 STEP 1 UNTIL N DO                              00029200
               FOR J ~ I STEP 1 UNTIL N DO                              00029300
               B[I,J] ~ AH[I,J]                                         00029400
          END  INVPDS IN DOUBLE PRECISION;                              00029500
LABEL         NAXT, L1, L2, L3, L4, L5, L6, L7, L8, L9, L10,            00029600
              L11, L12, L13, L14, L15 ;                                 00029700
LABEL          L16,L17,L18,LMO;                                         00029800
SWITCH        U ~ L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,L13,L14,       00029900
                     L15,L16,L17,L18,L1;                                00030000
INTEGER        II, JJ;                                                  00030100
ARRAY          DIAGONAL[0:N], SAM[0:N,0:N];                             00030200
              IF TR THEN                                                00030300
               FOR I~1 STEP 1 UNTIL Q DO                                00030400
          BEGIN                                                         00030500
               WTT,FTRAN);                                              00030600
               RTT,4,TBUF[*]);                                          00030700
               NCHAR~FINDGP(TBUF[1]);                                   00030800
               READ(TBUF[*],F2,NCHAR,T[I]);                             00030900
          END;                                                          00031000
              FOR I ~ 0 STEP 1 UNTIL N DO                               00031100
              FOR J ~ I STEP 1 UNTIL Q DO                               00031200
              A[I,J] ~ 0 ;                                              00031300
               WTT,FCRLF);                                              00031400
               WTT,FNAM);                                               00031500
LMO:           WTT,FPFX);                                               00031600
               RTT,F3,AAA1,AAA2);                                       00031700
               NCHAR~FINDGP(TBUF[1]);                                   00031800
               ID1.[6:6]~AAA1;  ID1.[12:36]~AAA2;                       00031900
               IF NCHAR < 7 THEN BLANKIT(ID1);                          00032000
               WTT,FSFX);                                               00032100
               RTT,F3,BBB1,BBB2);                                       00032200
               NCHAR~FINDGP(TBUF[1]);                                   00032300
               ID2.[6:6]~BBB1;  ID2.[12:36]~BBB2;                       00032400
               IF NCHAR < 7 THEN BLANKIT(ID2);                          00032500
               FILL FID WITH ID1,ID2;                                   00032600
               SEARCH(FID,TBUF[*]);                                     00032700
               IF TBUF[0] < 2 THEN                                      00032800
          BEGIN                                                         00032900
               WTT,FERO);  GO TO LMO;                                   00033000
          END;                                                          00033100
              L ~ Q-N ;                                                 00033200
              FOR P ~ 1 STEP 1 UNTIL L DO                               00033300
              YSQ[P] ~ 0 ;                                              00033400
              FOR L ~ 1 STEP 1 UNTIL M DO                               00033500
         BEGIN                                                          00033600
              READ (FID  ,FORM3,DATA) ;                                 00033700
              FOR J ~ 1 STEP 1 UNTIL Q DO                               00033800
         BEGIN                                                          00033900
              IF TR THEN                                                00034000
         BEGIN                                                          00034100
              GO TO U[T[J]] ;                                           00034200
L1:           X[J] ~ B[J] ;                                             00034300
              GO TO NAXT ;                                              00034400
L2:           X[J] ~ B[J] | B[J] ;                                      00034500
              GO TO NAXT ;                                              00034600
L3:           X[J] ~ 1 / B[J] ;                                         00034700
              GO TO NAXT ;                                              00034800
L4:           X[J] ~ SQRT (B[J]) ;                                      00034900
              GO TO NAXT ;                                              00035000
L5:           X[J] ~ EXP (B[J]) ;                                       00035100
              GO TO NAXT ;                                              00035200
L6:           X[J] ~ LN (B[J]) ;                                        00035300
              GO TO NAXT ;                                              00035400
L7:           X[J] ~ SIN (B[J]) ;                                       00035500
              GO TO NAXT ;                                              00035600
L8:           X[J] ~ COS (B[J]) ;                                       00035700
              GO TO NAXT ;                                              00035800
L9:           X[J] ~ B[J+1] ;                                           00035900
              GO TO NAXT ;                                              00036000
L10:          X[J] ~ B[J+2] ;                                           00036100
              GO TO NAXT ;                                              00036200
L11:          X[J] ~ B[J+3] ;                                           00036300
              GO TO NAXT ;                                              00036400
L12:          X[J] ~ B[J+4] ;                                           00036500
              GO TO NAXT ;                                              00036600
L13:          X[J] ~ B[J+5] ;                                           00036700
              GO TO NAXT ;                                              00036800
L14:          X[J] ~ B[J+6] ;                                           00036900
              GO TO NAXT ;                                              00037000
L15:          X[J] ~ B[J+7] ;                                           00037100
              GO TO NAXT ;                                              00037200
L16:           X[J]~B[J]*3;                                             00037300
               GO TO NAXT;                                              00037400
L17:           X[J]~B[J]*4;                                             00037500
               GO TO NAXT;                                              00037600
L18:             X[J]~0.4342945|LN(B[J]);                               00037700
               GO TO NAXT      ;                                        00037800
NAXT:    END                                                            00037900
              ELSE                                                      00038000
              X[J] ~ B[J]                                               00038100
         END ;                                                          00038200
              WRITE(TAPEA,Q+1,X[*])                                     00038300
         END;                                                           00038400
              REWIND(TAPEA) ;                                           00038500
RERED:        FOR L ~ 1 STEP 1 UNTIL M DO                               00038600
         BEGIN                                                          00038700
              READ (TAPEA,Q+1,X[*]) ;                                   00038800
              IF SWICH THEN                                             00038900
         BEGIN                                                          00039000
              F ~ B[0] ;                                                00039100
              FOR I ~ 1 STEP 1 UNTIL N DO                               00039200
              F ~ F + B[I] | X[I] ;                                     00039300
              C ~ F - X[P] ;                                            00039400
              WRITE (PRINT,FRM15,L,F,X[P],C) ;                          00039500
              IF HI < C THEN                                            00039600
              HI ~ C ;                                                  00039700
              IF LO > C THEN                                            00039800
              LO ~ C                                                    00039900
         END                                                            00040000
              ELSE                                                      00040100
         BEGIN                                                          00040200
              FOR K ~ 1 STEP 1 UNTIL Q DO                               00040300
         BEGIN                                                          00040400
              C ~ X[K] ;                                                00040500
              A[0,K] ~ A[0,K] + C ;                                     00040600
              IF K { N THEN                                             00040700
         BEGIN                                                          00040800
              FOR J ~ K STEP 1 UNTIL Q DO                               00040900
              A[K,J] ~ A[K,J] + C | X[J]                                00041000
         END                                                            00041100
         END ;                                                          00041200
              FOR J ~ N+1 STEP 1 UNTIL Q DO                             00041300
              YSQ[J-N] ~ YSQ[J-N] + X[J] | X[J]                         00041400
         END                                                            00041500
         END ;                                                          00041600
              IF SWICH THEN                                             00041700
              GO TO RESDU ;                                             00041800
              FOR I ~ 1 STEP 1 UNTIL Q DO                               00041900
         BEGIN                                                          00042000
              R ~ A[0,I] / M ;                                          00042100
              IF I { N THEN                                             00042200
         BEGIN                                                          00042300
              C ~ A[I,I] - A[0,I] | R ;                                 00042400
              A[I,I] ~ C ;                                              00042500
              A[I,0] ~ C ;                                              00042600
              FOR J ~ I+1 STEP 1 UNTIL N DO                             00042700
         BEGIN                                                          00042800
              C ~ A[I,J] - A[0,J]|R ;                                   00042900
              A[I,J] ~ C ;                                              00043000
              A[J,I] ~ C                                                00043100
         END ;                                                          00043200
              FOR J ~ N+1 STEP 1 UNTIL Q DO                             00043300
              A[I,J] ~ A[I,J] - A[0,J]|R                                00043400
         END                                                            00043500
              ELSE                                                      00043600
              YSQ[I-N] ~ YSQ[I-N] - A[0,I] | R ;                        00043700
              A[0,I] ~ R                                                00043800
         END ;                                                          00043900
              L ~ Q-N ;                                                 00044000
               WTT,FCRLF); FOR I~0,1,2,3 DO WTT,FLF);                   00044100
               WTT,TITL1);                                              00044200
               WTT,TITL2,HWS+1,FOR I~0 STEP 1 UNTIL HWS DO ID[I]);      00044300
               WTT,TITL3,M); WTT,TITL4,N); WTT,TITL5,L);                00044400
          BEGIN                                                         00044500
INTEGER        DUMMY;                                                   00044600
COMMENT       PRINTOUT OF NORMAL MATRIX ;                               00044700
              IF NORM THEN                                              00044800
         BEGIN                                                          00044900
              WRITE (PRINT,FRM1) ;                                      00045000
              FOR I ~ 1 STEP 1 UNTIL N DO                               00045100
          BEGIN                                                         00045200
               WTT,FRM2A,I);                                            00045300
               WTT,FRM2B,FOR J~I STEP 1 UNTIL N DO [J,A[I,J]]);         00045400
          END;                                                          00045500
         END ;                                                          00045600
COMMENT       COMPUTATION OF MEANS AND VARIANCES ;                      00045700
              IF MEAN THEN                                              00045800
         BEGIN                                                          00045900
              FOR I ~ 1 STEP 1 UNTIL Q DO                               00046000
              X[I] ~ A[0,I] ;                                           00046100
              WRITE (PRINT,FRM3) ;                                      00046200
              L ~ N ;                                                   00046300
              N ~ Q ;                                                   00046400
              WRITE (PRINT,FRM4,OUT5) ;                                 00046500
              N ~ L                                                     00046600
         END ;                                                          00046700
              IF VAR THEN                                               00046800
         BEGIN                                                          00046900
              FOR I ~ 1 STEP 1 UNTIL N DO                               00047000
              X[I] ~ A[I,0] / (M-1) ;                                   00047100
              FOR I ~ N+1 STEP 1 UNTIL Q DO                             00047200
              X[I] ~ YSQ[I-N] / (M-1) ;                                 00047300
              WRITE (PRINT,FRM5) ;                                      00047400
              L ~ N ;                                                   00047500
              N ~ Q ;                                                   00047600
              WRITE (PRINT,FRM4,OUT5) ;                                 00047700
              N ~ L                                                     00047800
         END ;                                                          00047900
COMMENT                                                                 00048000
              INVERT (N, A) ;                                           00048100
               FOR II ~ 1 STEP 1 UNTIL N DO                             00048200
               FOR JJ ~ 1 STEP 1 UNTIL N DO                             00048300
               SAM[II,JJ] ~ A[II,JJ];                                   00048400
               INVPDS(N,SAM,DIAGONAL);                                  00048500
               FOR II ~ 1 STEP 1 UNTIL N DO                             00048600
               FOR JJ ~ 1 STEP 1 UNTIL N DO                             00048700
               A[II,JJ] ~ SAM[II,JJ];                                   00048800
COMMENT       PRINTOUT OF INVERSE MATRIX ;                              00048900
              IF INV THEN                                               00049000
         BEGIN                                                          00049100
              WRITE (PRINT,FRM6) ;                                      00049200
              FOR I ~ 1 STEP 1 UNTIL N DO                               00049300
          BEGIN                                                         00049400
               WTT,FRM2A,I);                                            00049500
               WTT,FRM2B,FOR J~I STEP 1 UNTIL N DO [J,X[J]]);           00049600
          END;                                                          00049700
COMMENT       PRODUCT OF NORMAL AND INVERSE MATRICES ;                  00049800
              WRITE (PRINT,FRM7) ;                                      00049900
              FOR I ~ 1 STEP 1 UNTIL N DO                               00050000
         BEGIN                                                          00050100
              FOR J ~ I STEP 1 UNTIL N DO                               00050200
         BEGIN                                                          00050300
              X[J] ~ 0 ;                                                00050400
              FOR L ~ 1 STEP 1 UNTIL I-1 DO                             00050500
              X[J] ~ X[J] + A[L,I] | A[J,L] ;                           00050600
              FOR L ~ I STEP 1 UNTIL N DO                               00050700
         BEGIN                                                          00050800
              IF L < J THEN                                             00050900
              S ~ A[J,L]                                                00051000
              ELSE IF L > J THEN                                        00051100
              S ~ A[L,J]                                                00051200
              ELSE                                                      00051300
              S ~ A[J,0] ;                                              00051400
              X[J] ~ X[J] + S | A[I,L]                                  00051500
         END                                                            00051600
         END ;                                                          00051700
          BEGIN                                                         00051800
               WTT,FRM2A,I);                                            00051900
               WTT,FRM2B,FOR J~I STEP 1 UNTIL N DO [J,X[J]]);           00052000
          END;                                                          00052100
         END                                                            00052200
         END ;                                                          00052300
              FOR I ~ 1 STEP 1 UNTIL N DO                               00052400
         BEGIN                                                          00052500
              R ~ A[I,0] ;                                              00052600
              FOR J ~ I+1 STEP 1 UNTIL N DO                             00052700
              A[J,I] ~ A[J,I] / SQRT (R | A[J,0])                       00052800
         END ;                                                          00052900
          END OF DUMMY BLOCK;                                           00053000
COMMENT       CONSIDER EACH DEPENDENT VARIABLE SEPARATELY ;             00053100
              FOR P ~ N+1 STEP 1 UNTIL Q DO                             00053200
         BEGIN                                                          00053300
COMMENT       COMPUTATION OF REGRESSION COEFFICIENTS ;                  00053400
              FOR I ~ 1 STEP 1 UNTIL N DO                               00053500
         BEGIN                                                          00053600
              B[I] ~ 0 ;                                                00053700
              FOR L ~ 1 STEP 1 UNTIL I-1 DO                             00053800
              B[I] ~ B[I] + A[L,I] | A[L,P] ;                           00053900
              FOR L ~ I STEP 1 UNTIL N DO                               00054000
              B[I] ~ B[I] + A[I,L] | A[L,P]                             00054100
         END ;                                                          00054200
              B[0] ~ A[0,P] ;                                           00054300
              FOR I ~ 1 STEP 1 UNTIL N DO                               00054400
              B[0] ~ B[0] - B[I] | A[0,I] ;                             00054500
COMMENT       COMPUTATION OF REGRESSION SUM OF SQUARES ;                00054600
              RSS ~ 0 ;                                                 00054700
              FOR I ~ 1 STEP 1 UNTIL N DO                               00054800
              RSS ~ RSS + B[I] | A[I,P] ;                               00054900
              RMS ~ RSS / N ;                                           00055000
COMMENT       COMPUTATION OF ERROR SUM SQUARE ;                         00055100
              C ~ YSQ[P-N] ;                                            00055200
              ERS ~ C - RSS ;                                           00055300
COMMENT       COMPUTATION OF ERROR MEAN SQUARE AND                      00055400
              STANDARD ERROR OF ESTIMATE ;                              00055500
              K ~ M-1 ;                                                 00055600
              L ~ K-N ;                                                 00055700
              EMS ~ ERS / L ;                                           00055800
              SE ~ SQRT (EMS) ;                                         00055900
COMMENT       COMPUTATION OF COEFFICIENT OF DETERMINATION ;             00056000
              CD ~ RSS / C ;                                            00056100
COMMENT       MULTIPLE CORRELATION COEFFICIENT ;                        00056200
              MCC ~ SQRT (CD) ;                                         00056300
COMMENT       COMPUTATION OF STANDARD DEVIATIONS AND T                  00056400
              VALUES OF REGRESSION COEFFICIENTS ;                       00056500
              FOR I ~ 1 STEP 1 UNTIL N DO                               00056600
         BEGIN                                                          00056700
              S ~ SQRT (EMS | A[I,I]) ;                                 00056800
              SB[I] ~ S ;                                               00056900
              X[I] ~ B[I] / S                                           00057000
         END ;                                                          00057100
              I ~ P-N ;                                                 00057200
              WRITE (PRINT,FRM8,I) ;                                    00057300
               WTT,FRM9A); WTT,FRM9B,B[0]);                             00057400
              WRITE (PRINT,FRM4,OUT3) ;                                 00057500
               WTT,FCRLF);                                              00057600
               WTT,FRM10A,ERS); WTT,FRM10B,EMS); WTT,FRM10C,SE);        00057700
               WTT,FRM10D,CD); WTT,FRM10E,MCC);                         00057800
              WRITE (PRINT,FRM11) ;                                     00057900
              WRITE (PRINT,FRM4,OUT4) ;                                 00058000
               WTT,FCRLF);                                              00058100
              WRITE (PRINT,FRM12) ;                                     00058200
              WRITE (PRINT,FRM4,OUT5) ;                                 00058300
COMMENT       ANALYSIS OF VARIANCE ;                                    00058400
              F ~ RMS / EMS ;                                           00058500
               WTT,FRM13A); WTT,FRM13B); WTT,FRM13C);                   00058600
               WTT,FRM13D); WTT,FRM13E,N,RSS,RMS,F);                    00058700
               WTT,FCRLF);                                              00058800
               WTT,FRM13F,L,ERS,EMS); WTT,FRM13G,K,C);                  00058900
COMMENT       COMPUTATION OF RESIDUES ;                                 00059000
              IF RES THEN                                               00059100
         BEGIN                                                          00059200
              REWIND(TAPEA) ;                                           00059300
              SWICH ~ TRUE ;                                            00059400
               WTT,FRM14A); WTT,FRM14B);                                00059500
              HI ~ -0.9@+49 ;                                           00059600
              LO ~  0.9@+49 ;                                           00059700
              GO TO RERED ;                                             00059800
RESDU:        CD ~ HI - LO ;                                            00059900
              F ~ CD / SE ;                                             00060000
               WTT,FRM16A); WTT,FRM16B,CD); WTT,FRM16C,F);              00060100
         END                                                            00060200
         END ;                                                          00060300
              IF PCC THEN                                               00060400
         BEGIN                                                          00060500
COMMENT       CONSIDER EACH DEPENDENT VARIABLE SEPARATELY ;             00060600
              FOR P ~ N+1 STEP 1 UNTIL Q DO                             00060700
         BEGIN                                                          00060800
COMMENT       GET AUGMENTED CORRELATION COEFFICIENT MATRIX ;            00060900
              L ~ P-N ;                                                 00061000
              FOR I ~ 1 STEP 1 UNTIL N+1 DO                             00061100
              A[I,I] ~ 1 ;                                              00061200
              FOR K ~ 1 STEP 1 UNTIL N DO                               00061300
         BEGIN                                                          00061400
              FOR J ~ K+1 STEP 1 UNTIL N DO                             00061500
              A[K,J] ~ A[J,K] ;                                         00061600
              A[K,N+1] ~ A[K,N+1] / SQRT (A[K,0] | YSQ[L])              00061700
         END ;                                                          00061800
              WRITE (PRINT,FRM8,L) ;                                    00061900
               WTT,FCRLF);                                              00062000
              N ~ N+1 ;                                                 00062100
              IF CC THEN                                                00062200
         BEGIN                                                          00062300
              WRITE (PRINT,FRM17) ;                                     00062400
              FOR I ~ 1 STEP 1 UNTIL N DO                               00062500
          BEGIN                                                         00062600
               WTT,FRM2A,I);                                            00062700
               WTT,FRM2B,FOR J~I STEP 1 UNTIL N DO [J,X[J]]);           00062800
          END;                                                          00062900
         END ;                                                          00063000
              INVERT (N, A) ;                                           00063100
              IF CCINV THEN                                             00063200
         BEGIN                                                          00063300
              WRITE (PRINT,FRM18) ;                                     00063400
              FOR I ~ 1 STEP 1 UNTIL N DO                               00063500
          BEGIN                                                         00063600
               WTT,FRM2A,I);                                            00063700
               WTT,FRM2B,FOR J~I STEP 1 UNTIL N DO [J,X[J]]);           00063800
          END;                                                          00063900
         END ;                                                          00064000
COMMENT       DETERMINE PARTIAL CORRELATION COEFFICIENTS ;              00064100
              R ~ A[N,N] ;                                              00064200
              N ~ N-1 ;                                                 00064300
              FOR J ~ 1 STEP 1 UNTIL N DO                               00064400
              X[J] ~ -A[J,N+1] / SQRT (R | A[J,J]) ;                    00064500
              WRITE (PRINT,FRM19) ;                                     00064600
              WRITE (PRINT,FRM4,OUT5) ;                                 00064700
COMMENT       MOVE COLUMN FOR NEXT DEPENDENT VARIABLE ;                 00064800
              IF P+1 { Q THEN                                           00064900
         BEGIN                                                          00065000
              FOR J ~ 1 STEP 1 UNTIL N+1 DO                             00065100
              A[J,N+1] ~ A[J,P+1]                                       00065200
         END                                                            00065300
         END                                                            00065400
         END                                                            00065500
         END ;                                                          00065600
               FOR I~0 STEP 1 UNTIL 5 DO WTT,FCRLF);                    00065700
               WTT,FNXT);                                               00065800
               RTT,8,TBUF[*]);   NCHAR~FINDGP(TBUF[1]);                 00065900
               READ(TBUF[*],F4,3,ANOTH);                                00066000
               REWIND(FID);                                             00066100
               IF ANOTH = "YES" THEN GO TO START ELSE GO TO NOMO;       00066200
NOMO:    END  PROGRAM.                                                  00066300
