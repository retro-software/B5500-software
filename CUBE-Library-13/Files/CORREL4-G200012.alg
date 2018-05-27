          BEGIN                                                         00000100
COMMENT        CUBE LIBRARY NUMBER IS G200012.  THE PROGRAM NAME IS     00000200
               "B0309A/TTY".  THIS VERSION DATED 5/3/68;                00000300
COMMENT        BASIC STATISTICAL ROUTINE WITH OMISSION OPTIONS          00000400
               CHARLES L. CLARK                                         00000500
               PROFESSIONAL SERVICES DIVISIONAL GROUP                   00000600
               BURROUGHS CORPORATION                                    00000700
               PROGRAM CONTAINS 105 CARDS                               00000800
               CARD SEQUENCE BEGINS WITH BSWO   1                       00000900
               FIRST RELEASE DATE JULY 25, 1964                         00001000
               CONVERTED TO TELETYPE JUNE 1967 F. LUCAS    ;            00001100
INTEGER        I,J,L,NSAM,NVAR,NCAS,NCOD ;                              00001200
INTEGER ARRAY  ID[0:4] ;                                                00001300
LABEL          START,L1,NOMO ;                                          00001400
FORMAT OUT     FHD(X8,"{!ENTER YOUR PROGRAM ID-UP TO 30 CHARACTERS{!~"),00001500
               FNS(X8,"{!SAMPLE SIZE = ~"),                             00001600
               FNV(X8,"{!NO. OF VARIABLES = ~"),                        00001700
               FMD(X8,"{!IF DATA OMISSIONS ARE TO BE MADE ENTER A ONE", 00001800
                   "  ~"),                                              00001900
               FNO(X8,"{!NO. OF VALUES TO BE LISTED FOR OMISSION = ~"), 00002000
               FCRLF(X8,"{!~"),                                         00002100
               FTL1(X8,"{!!!BASIC STATISTICAL VALUES{!!~"),             00002200
               FTL2(X8,*A1,"~"),                                        00002300
               FTL3(X8,"VAR NO",X6,"MEAN",X8,"S.D.",X4,"S.E. OF MEAN",  00002400
                   X2,"SAMPLE{!!~"),                                    00002500
            FTL4(X8,"VAR NO",X6,"MAXIMUM",X5,"MINIMUM",X6,"RANGE{!!~"), 00002600
               FRM1(X9,I4,X6,"NO DATA FOR THIS VARIABLE~"),             00002700
               FRM2A(X9,I4,F13.4,2F12.4,I7,"{!~"),                      00002800
               FRM2B(X9,I4,X3,3F12.4,"{!~"),                            00002900
               FRM3(X9,I4,X6,"ONLY ONE VALUE FOR THIS VARIABLE{!~"),    00003000
               FDN(X8,"{!ENTER DATA NAMES~"),                           00003100
               FPX(X8,"{!PREFIX = ~"),                                  00003200
               FSX(X8,"{!SUFFIX = ~"),                                  00003300
               FCC(X8,"{!IF YOU WISH TO SCALE YOUR DATA TYPE A ONE ~"), 00003400
               FSF(X8,"{!ENTER SCALE FACTOR FOR VAR ",I2," ~"),         00003500
               FCD(X8,"{!ENTER OMISSION CODE  ~"),                      00003600
               FOC(X8,"{!WHEN ENTERING YOUR OMISSIONS USE THE ",        00003700
                 "FORM XXXXX.XX~"),                                     00003800
               FOB(X8,"{!FILL IN ALL LEADING AND TRAILING ZEROES{!~");  00003900
ALPHA FILE IN  TTIN 14 (2,8);                                           00004000
ALPHA FILE OUT TTOUT 14 (2,8);                                          00004100
FILE IN        FID DISK SERIAL "AAAAAAA" "BBBBBBB" (2,10,30);           00004200
FORMAT IN      F1(X8,I*), F2(X8,*A1), F3(X8,A1,A6);                     00004300
ALPHA ARRAY    HH[0:30];                                                00004400
INTEGER        STA,NCHAR,NIN;                                           00004500
ALPHA          ID1,ID2,AA1,AA2,BB1,BB2,SCL;                             00004600
DEFINE         RTT=READ(TTIN(STA)#, WTT=WRITE(TTOUT(STA)#,              00004700
               PRINT=TTOUT(STA)#;                                       00004800
ARRAY          TB[0:7];                                                 00004900
FORMAT IN      FORM2( 7F8.2);                                           00005000
PROCEDURE      BLANKIT(XX);                                             00005100
ALPHA          XX;                                                      00005200
          BEGIN                                                         00005300
               IF XX.[12:6]="~" THEN XX.[12:36]~"      " ELSE           00005400
               IF XX.[18:6]="~" THEN XX.[18:30]~"     " ELSE            00005500
               IF XX.[24:6]="~" THEN XX.[24:24]~"    " ELSE             00005600
               IF XX.[30:6]="~" THEN XX.[30:18]~"   " ELSE              00005700
               IF XX.[36:6]="~" THEN XX.[36:12]~"  " ELSE               00005800
               IF XX.[42:6]="~" THEN XX.[42:06]~" " ELSE                00005900
          END OF BLANKIT;                                               00006000
INTEGER STREAM PROCEDURE FINDGP(B);                                     00006100
          BEGIN                                                         00006200
LABEL          HERE;                                                    00006300
               SI~B; 40(IF SC="~" THEN JUMP OUT 1 TO HERE;              00006400
               SI~SI+1; TALLY ~TALLY+1);                                00006500
HERE:          FINDGP~TALLY;                                            00006600
          END OF FINDGP;                                                00006700
               STA~STATUS(TB[*]);                                       00006800
               STA.[9:9]~TB[0].[9:9];                                   00006900
START:                                                                  00007000
               WTT,FHD);  RTT,8,TB[*]);                                 00007100
               NCHAR~FINDGP(TB[1]);                                     00007200
               NIN~NCHAR-1;                                             00007300
               READ(TB[*],F2,NCHAR,FOR I~0 STEP 1 UNTIL NIN DO HH[I]);  00007400
               WTT,FNS);  RTT,3,TB[*]);                                 00007500
               NCHAR~FINDGP(TB[1]);                                     00007600
               READ(TB[*],F1,NCHAR,NSAM);                               00007700
               WTT,FNV);  RTT,3,TB[*]);                                 00007800
               NCHAR~FINDGP(TB[1]);                                     00007900
               READ(TB[*],F1,NCHAR,NVAR);                               00008000
               WTT,FMD);  RTT,<X8,I1>,NCAS);                            00008100
               IF NCAS=1 THEN                                           00008200
          BEGIN                                                         00008300
               WTT,FNO);  RTT,3,TB[*]);                                 00008400
               NCHAR~FINDGP(TB[1]);                                     00008500
               READ(TB[*],F1,NCHAR,NCOD);                               00008600
          END;                                                          00008700
L1:       BEGIN                                                         00008800
BOOLEAN        NOMIS ;                                                  00008900
INTEGER ARRAY  NX[0:NVAR],S[0:NVAR] ;                                   00009000
REAL           W;                                                       00009100
ARRAY          DEN,RANG[0:NVAR];                                        00009200
ARRAY          CODE[0:50  ],SUMX[0:NVAR],SUMXX[0:NVAR],MINX[0:NVAR],    00009300
               MAXX[0:NVAR],XBAR[0:NVAR],SC[0:NVAR],X[0:NVAR] ;         00009400
LIST           SCAL(FOR I ~ 1 STEP 1 UNTIL NVAR DO S[I]),               00009500
               MISS(FOR I ~ 1 STEP 1 UNTIL NCOD DO CODE[I]),            00009600
               DATA(FOR J ~ 1 STEP 1 UNTIL NVAR DO X[J]),               00009700
               OUT1(FOR L ~ 0 STEP 1 UNTIL 4 DO ID[L]) ;                00009800
LABEL          NAXT ;                                                   00009900
LABEL          LOR;                                                     00010000
               WTT,FCC);  RTT,<X8,I1>,SCL);                             00010100
               IF SCL=1 THEN                                            00010200
          BEGIN                                                         00010300
               FOR I~1 STEP 1 UNTIL NVAR DO                             00010400
          BEGIN                                                         00010500
               WTT,FSF,I);                                              00010600
               RTT,4,TB[*]);  NCHAR~FINDGP(TB[1]);                      00010700
               READ(TB[*],F1,NCHAR,S[I]);                               00010800
          END;                                                          00010900
          END  ELSE FOR I~1 STEP 1 UNTIL NVAR DO S[I]~0;                00011000
               WTT,FCRLF);                                              00011100
               IF NCAS=1 THEN                                           00011200
          BEGIN                                                         00011300
               WTT,FOC);  WTT,FOB);                                     00011400
               FOR I~1 STEP 1 UNTIL NCOD DO                             00011500
          BEGIN                                                         00011600
               WTT,FCD);  RTT,<X8,F8.2>,CODE[I]);                       00011700
          END;                                                          00011800
          END;                                                          00011900
               WTT,FDN);                                                00012000
LOR:           WTT,FPX);  RTT,F3,AA1,AA2);                              00012100
               ID1.[6:6]~AA1;  ID1.[12:36]~AA2;                         00012200
               BLANKIT(ID1);                                            00012300
               WTT,FSX);  RTT,F3,BB1,BB2);                              00012400
               ID2.[6:6]~BB1;  ID2.[12:36]~BB2;                         00012500
               BLANKIT(ID2);                                            00012600
               FILL FID WITH ID1,ID2;                                   00012700
               SEARCH(FID,TB[*]);                                       00012800
               IF TB[0]<2 THEN                                          00012900
          BEGIN                                                         00013000
               WTT,<X8,"{!YOUR FILE NOT FOUND. TRY AGAIN.{!~">);        00013100
               GO TO LOR;                                               00013200
          END;                                                          00013300
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00013400
          BEGIN                                                         00013500
               SC[I] ~ 10.0*S[I] ;                                      00013600
               SUMX[I] ~ 0.0 ;                                          00013700
               SUMXX[I] ~ 0.0 ;                                         00013800
               MINX[I] ~ 0.9@49 ;                                       00013900
               MAXX[I] ~ -0.9@49 ;                                      00014000
               NX[I] ~ 0                                                00014100
          END ;                                                         00014200
               FOR I ~ 1 STEP 1 UNTIL NSAM DO                           00014300
          BEGIN                                                         00014400
               READ(FID,FORM2,DATA);                                    00014500
               FOR J ~ 1 STEP 1 UNTIL NVAR DO                           00014600
          BEGIN                                                         00014700
               W ~ X[J] ;                                               00014800
               NOMIS ~ TRUE ;                                           00014900
               IF NCAS = 1 THEN                                         00015000
          BEGIN                                                         00015100
               FOR L ~ 1 STEP 1 UNTIL NCOD DO                           00015200
          BEGIN                                                         00015300
               IF W = CODE[L] THEN                                      00015400
          BEGIN                                                         00015500
               NOMIS ~ FALSE ;                                          00015600
               GO TO NAXT                                               00015700
          END                                                           00015800
          END                                                           00015900
          END ;                                                         00016000
               IF NOMIS THEN                                            00016100
                                                                        00016200
          BEGIN                                                         00016300
               W ~ W|SC[J] ;                                            00016400
               NX[J] ~ NX[J] + 1 ;                                      00016500
               SUMX[J] ~ SUMX[J] + W ;                                  00016600
               SUMXX[J] ~ SUMXX[J] + W|W ;                              00016700
               IF MAXX[J] < W THEN                                      00016800
               MAXX[J] ~ W ;                                            00016900
               IF MINX[J] > W THEN                                      00017000
               MINX[J] ~ W                                              00017100
          END ;                                                         00017200
NAXT:     END                                                           00017300
          END ;                                                         00017400
               FOR I~0 STEP 1 UNTIL 2 DO WTT,FCRLF);                    00017500
               WTT,FTL1);                                               00017600
               WTT,FTL2,NIN+1,FOR I~0 STEP 1 UNTIL NIN DO HH[I]);       00017700
               FOR I~0 STEP 1 UNTIL 2 DO WTT,FCRLF);                    00017800
               WTT,FTL3);                                               00017900
               FOR I ~ 1 STEP 1 UNTIL NVAR DO                           00018000
          BEGIN                                                         00018100
               DEN[I]~NX[I];                                            00018200
               IF DEN[I]=0 THEN                                         00018300
               WRITE(PRINT,FRM1,I)                                      00018400
               ELSE IF DEN[I]=1 THEN                                    00018500
               WRITE(PRINT,FRM3,I)                                      00018600
               ELSE                                                     00018700
          BEGIN                                                         00018800
               XBAR[I]~SUMX[I]/DEN[I];                                  00018900
               SUMXX[I] ~ (SUMXX[I]-(SUMX[I]|SUMX[I])/DEN[I])/          00019000
                (DEN[I]-1.0);                                           00019100
               SUMXX[I] ~ SQRT(SUMXX[I]) ;                              00019200
               SUMX[I] ~ SUMXX[I]/SQRT(DEN[I]);                         00019300
               RANG[I] ~ MAXX[I] - MINX[I] ;                            00019400
               WTT,FRM2A,I,XBAR[I],SUMXX[I],SUMX[I],DEN[I]);            00019500
          END                                                           00019600
          END ;                                                         00019700
               FOR I~0 STEP 1 UNTIL 2 DO WTT,FCRLF);                    00019800
               WTT,FTL4);                                               00019900
               FOR I~1 STEP 1 UNTIL NVAR DO                             00020000
          BEGIN                                                         00020100
               IF DEN[I]=0 THEN WTT,FRM1,I);                            00020200
               IF DEN[I]=1 THEN WTT,FRM3,I);                            00020300
               WTT,FRM2B,I,MAXX[I],MINX[I],RANG[I]);                    00020400
          END;                                                          00020500
               FOR I~0 STEP 1 UNTIL 4 DO WTT,FCRLF);                    00020600
               WTT,<X8,"{!IF YOU ARE FINISHED TYPE 0 ELSE 1 ~">);       00020700
               RTT,<X8,I1>,TB[0]);                                      00020800
               REWIND(FID);                                             00020900
               IF TB[0]=1 THEN GO TO START;                             00021000
          END ;                                                         00021100
          END PROGRAM .                                                 00021200
