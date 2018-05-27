         BEGIN                                                          00000100
COMMENT        CUBE LIBRARY NUMBER IS E200010.  THE PROGRAM NAME IS     00000200
               "B0031/TTY".  THIS VERSION DATED 5/3/68;                 00000300
                                                                        00000400
COMMENT       LEAST SQUARES POLYNOMIAL CURVE-FITTING WITH WEIGHT        00000500
              OPTIONS. THE OMEGA VALUES MAY EITHER BE CONSTANTS, OR THEY00000600
              MAY START WITH ANY GIVEN INITIAL VALUE AND BE ITERATED TO 00000700
              SELFCONSISTENCY BY ASSIGNING A FUNCTION OF THE SQUARES    00000800
              OF THE NORMAL DIFFERENCES AS OMEGA.                       00000900
                                                                        00001000
              THE FOLLOWING BOOLEAN OPTIONS, IF TRUE, RESULT IN :       00001100
                   STEPN ~ FITTING IS CARRIED OUT FOR ALL ORDERS,       00001200
                           UP TO M { 10.                                00001300
                   INTEG ~ VALUES OF FIRST AND SECOND INTEGRAL ARE      00001400
                           COMPUTED FOR GRAPHS GOING THROUGH THE ORIGIN.00001500
                   DIFFL ~ FIRST AND SECOND DERIVATIVES ARE COMPUTED.   00001600
                   SELFC ~ WEIGHTS ARE ITERATED TO SELFCONSISTENCY FOR  00001700
                           AN INPUT THRESHOLD VALUE.                    00001800
                   FOLLW ~ SEQUENTIAL DATA SETS ARE PROCESSED.          00001900
                                                                        00002000
              DR EDGAR L. EICHHORN [ BURROUGHS PROFESSIONAL SERVICES].  00002100
                                                                        00002200
              NEW VERSION, 05-25-1964.                                  00002300
              CONVERTED TO TELETYPE SEPT. 27,1967                       00002400
              BY C. A. WILLIAMS.    ;                                   00002500
INTEGER       I,J,K,P,Q,S,N,M,CT,COUNT,STA,NCHAR,NKAR;                  00002600
                                                                        00002700
REAL          RT,SQUSUM,CALC,DIFF,DIFF2,FIRST,SECND,FACT1,FACT2,THOLD,  00002800
              OMS,OMGSUM,WSUM,XQ,YQ  ;                                  00002900
                                                                        00003000
ARRAY          TBUF[0:7];                                               00003100
INTEGER ARRAY  BB[0:30];                                                00003200
ALPHA FILE IN  TTIN 14 (2,8);                                           00003300
ALPHA FILE OUT TTOUT 14 (2,8);                                          00003400
FILE IN        FID DISK SERIAL "AAAAAAA" "BBBBBBB" (2,10,30);           00003500
DEFINE    RTT=READ(TTIN(STA)#, WTT=WRITE(TTOUT(STA)#, BUFFOUT=TTOUT(STA)00003600
#;                                                                      00003700
FORMAT IN      F1(X8,*A1), F3(X8,A1,A6);                                00003800
FORMAT IN      F5(X8,L*),                                               00003900
               FMIN1(3R12.5),FMIN2(X8,I2,X1,I2),                        00004000
               FMIN3(X8,2F12.7),FMIN4(X8,F12.7,X1,I2);                  00004100
ALPHA          ID1,ID2,AA1,AA2,BB1,BB2;                                 00004200
REAL ARRAY    PART[0:20],EL[0:13,0:14,0:13],DEN,                        00004300
              SUB,SOL[0:13],X,Y,OMEGA,PREV[0:250],                      00004400
               KALC,DYFF,FYRST,SECNDT[0:250];                           00004500
                                                                        00004600
BOOLEAN       STEPN,INTEG,DIFFL,SELFC,FOLLW  ;                          00004700
                                                                        00004800
LIST          ORDER(P,M),                                               00004900
              DETER(FACT1,FACT2),                                       00005000
              THC(THOLD,CT),                                            00005100
              INDATA(X[Q],Y[Q],OMEGA[Q]),                               00005200
              CN(SOL[Q],(Q-2)),                                         00005300
               LISTING1(Q,X[Q],Y[Q],OMEGA[Q]),                          00005400
               LISTING2(Q,KALC[Q],DYFF[Q],FYRST[Q],SECNDT[Q]),          00005500
               LISTING3(Q,KALC[Q],DYFF[Q]),                             00005600
               VARIANCE1((SQUSUM/OMGSUM)),                              00005700
               VARIANCE2((100.0)|(1.0- (SQUSUM/WSUM)));                 00005800
                                                                        00005900
FORMAT OUT     HEADLINE1(X10,"Q",X13,"X",X9,"Y[OBS]",X10,"OMEGA","{!~"),00006000
               HEADLINE2(X10,"Q",X7,"Y[CALC]",X7,"DIFF.","{!~"),        00006100
               DIFFLINE1(X10,"Q",X12,"X",X9,"Y[OBS]",X8,"OMEGA","{!~"), 00006200
               DIFFLINE2(X10,"Q",X7,"Y[CALC]",X7,"DIFF",X7,"SLOPE",X7,"C00006300
URV","{!~"),                                                            00006400
               INTGLINE1(X10,"Q",X12,"X",X9,"Y[OBS]",X10,"OMEGA","{!~"),00006500
               INTGLINE2(X10,"Q",X6,"Y[CALC]",X6,"DIFF",X4,"FIRST INT",X00006600
3,"SECOND INT","{!~"),                                                  00006700
               FPROID(X8,"{!ENTER YOUR PROB. ID-UP TO 30 CHAR{!~"),     00006800
               FSTEPN(X8,"{!FITTING FOR ORDERS UP TO M LEQ 10 TRUE OR FA00006900
LSE{!~"),                                                               00007000
               FINTEG(X8,"{!VALUES OF 1ST & 2ND INTEGRAL ARE COMPUTED~")00007100
,                                                                       00007200
               FINTEG1(X8,"{!FOR GRAPHS GOING THRU THE ORIGIN TRUE OR FA00007300
LSE{!~"),                                                               00007400
               FDIFFL(X8,"{!FIRST & SECOND DERIVATIVES COMPUTED TRUE OR 00007500
FALSE{!~"),                                                             00007600
               FSELFC(X8,"{!WEIGHTS ITERATED TO SELFCONSISTENCY FOR~"), 00007700
               FSELFC1(X8,"{!AN INPUT THRESHOLD VALUE TRUE OR FALSE{!~")00007800
,                                                                       00007900
               FFOLLW(X8,"{!SEQUENTIAL DATA SETS TO BE PROCESSED TRUE OR00008000
 FALSE{!~"),                                                            00008100
               FDN(X8,"{!ENTER DATA NAMES~"), FCRLF(X8,"{!~"),          00008200
               FPX(X8,"{!PREFIX = ~"),                                  00008300
               FSX(X8,"{!SUFFIX = ~"),                                  00008400
               CONF(X8,E14.7," X (X*",I2,")","{!~"),                    00008500
               LISTF1(X8,I3,X2,3(E12.5,X3),"{!~"),                      00008600
               LISTF2(X8,I3,X2,4(E12.5),"{!~"),                         00008700
               LISTF3(X8,I3,X2,2(E12.5),"{!~"),                         00008800
               CNTF(X8,"{!ITERATION NO. ",I2,"{!~"),                    00008900
               SUMF1(X8,"{!VARIANCE = ",E12.5,"{!~"),                   00009000
               SUMF2(X8,"GOODNESS-OF-FIT = "                            00009100
                    F10.5,X1,"%","{!~");                                00009200
                                                                        00009300
                                                                        00009400
                                                                        00009500
                                                                        00009600
                                                                        00009700
LABEL         ONSET,START,SUMS,AAAAA,SECOND,THIRD,FOURTH,HDLYNE2,       00009800
              FIFTH, SIXTH, SEVENTH, EIGHTH, NINTH, FINISH,TRIAGAIN;    00009900
                                                                        00010000
 PROCEDURE DATELINE (PROGRAM);                                          00010100
 VALUE PROGRAM;                                                         00010200
 ALPHA PROGRAM;                                                         00010300
 BEGIN OWN BOOLEAN USED;                                                00010400
FORMAT         HD1(X8,"{!",A4,I3,", ",A4,X2,"TIME:",I5,"{!~"),          00010500
               HD2(X8,"{!OUTPUT FROM PROGRAM ",A6,"{!!!!~"),            00010600
               LAYT(X8,"{!!","EXECUTION TIME =",I5," SECONDS","{!~"),   00010700
               LAYT1(X8,"I/O TIME =", I5," SECONDS","{!~"),             00010800
               LAYT2(X8,A4,I3,", ",A4, X03, "TIME:", I7,"{!!!!~");      00010900
    LABEL GOTIT;                                                        00011000
    ALPHA MO, MINS, FEB, HRS, YR, DAY;                                  00011100
    USED ~ USED AND PROGRAM . [18 : 6] = 0;                             00011200
    DAY ~ TIME (0);                                                     00011300
    YR ~ DAY . [18 : 12] + "1900";                                      00011400
    DAY ~ DAY . [42 : 6] + 10 | DAY . [36 : 6] + 100 | DAY . [30 : 6];  00011500
    FEB ~ IF YR . [42 : 6] MOD 4 = 0 THEN "(FEB." ELSE "&FEB.";         00011600
    FOR MO ~ "~JAN.", FEB, "~MAR.", "<APR.", "~ MAY", "<JUNE", "~JULY"  00011700
    , "~AUG.", "<SEPT", "~OCT.", "<NOV.", "~DEC." DO                    00011800
    BEGIN IF DAY { MO . [18 : 06] THEN GO TO GOTIT;                     00011900
       DAY ~ DAY - MO . [18 : 6];                                       00012000
    END;                                                                00012100
    GOTIT: MINS ~ TIME (1) / 3600;                                      00012200
    HRS ~ 100 | (MINS DIV 60) + MINS MOD 60;                            00012300
               IF USED THEN                                             00012400
          BEGIN                                                         00012500
               WTT,LAYT,TIME (2) / 60);                                 00012600
               WTT,LAYT1,TIME (3) / 60);                                00012700
               WTT,LAYT2,MO,DAY,YR,HRS);                                00012800
          END  ELSE                                                     00012900
          BEGIN                                                         00013000
               WTT,HD1,MO,DAY,YR,HRS);                                  00013100
               WTT,HD2,PROGRAM);                                        00013200
          END;                                                          00013300
    USED ~ TRUE;                                                        00013400
 END OF DATELINE;                                                       00013500
PROCEDURE      BLANKIT(XX);                                             00013600
ALPHA          XX;                                                      00013700
          BEGIN                                                         00013800
               IF XX.[12:6]="~" THEN XX.[12:36]~"      "ELSE            00013900
               IF XX.[18:6]="~" THEN XX.[18:30]~"     "ELSE             00014000
               IF XX.[24:6]="~" THEN XX.[24:24]~"    "ELSE              00014100
               IF XX.[30:6]="~" THEN XX.[30:18]~"   "ELSE               00014200
               IF XX.[36:6]="~" THEN XX.[36:12]~"  "ELSE                00014300
               IF XX.[42:6]="~" THEN XX.[42:06]~" ";                    00014400
          END OF BLANKIT;                                               00014500
INTEGER STREAM PROCEDURE FINDGP(B);                                     00014600
          BEGIN                                                         00014700
LABEL          HERE;                                                    00014800
               SI~B; 40(IF SC="~" THEN JUMP OUT 1 TO HERE;              00014900
               SI~SI+1; TALLY~TALLY+1);                                 00015000
HERE:          FINDGP~TALLY;                                            00015100
          END;                                                          00015200
               STA~STATUS(TBUF[*]);                                     00015300
               STA.[9:9]~TBUF[0].[9:9];                                 00015400
               DATELINE("LSTSQS");                                      00015500
               WTT,FPROID); RTT,8,TBUF[*]);                             00015600
               NCHAR~FINDGP(TBUF[1]);                                   00015700
               NKAR~NCHAR-1;                                            00015800
              READ(TBUF[*],F1,NCHAR,FOR I~0 STEP 1 UNTIL NKAR DO BB[I]);00015900
ONSET:        Q ~ 1  ;                                                  00016000
          BEGIN                                                         00016100
               WTT,FINTEG);                                             00016200
               WTT,FINTEG1);                                            00016300
          END;                                                          00016400
               RTT,4,TBUF[*]);                                          00016500
               NCHAR~FINDGP(TBUF[1]);                                   00016600
               READ(TBUF[*],F5,NCHAR,INTEG);                            00016700
               WTT,FDIFFL);  RTT,4,TBUF[*]);                            00016800
               NCHAR~FINDGP(TBUF[1]);                                   00016900
               READ(TBUF[*],F5,NCHAR,DIFFL);                            00017000
          BEGIN                                                         00017100
               WTT,FSELFC);                                             00017200
               WTT,FSELFC1);                                            00017300
          END;                                                          00017400
               RTT,4,TBUF[*]);                                          00017500
               NCHAR~FINDGP(TBUF[1]);                                   00017600
               READ(TBUF[*],F5,NCHAR,SELFC);                            00017700
               WTT,FSTEPN); RTT,4,TBUF[*]);                             00017800
               NCHAR~FINDGP(TBUF[1]);                                   00017900
               READ(TBUF[*],F5,NCHAR,STEPN);                            00018000
               WTT,FFOLLW); RTT,4,TBUF[*]);                             00018100
               NCHAR~FINDGP(TBUF[1]);                                   00018200
               READ(TBUF[*],F5,NCHAR,FOLLW);                            00018300
               IF (INTEG OR DIFFL) THEN                                 00018400
          BEGIN                                                         00018500
               WTT,<X8,"{!ENTER VALUES FOR FACT1 & FACT2 UNDER FORMAT 2F00018600
12.7{!~">);                                                             00018700
               RTT,FMIN3,DETER);                                        00018800
          END;                                                          00018900
               IF SELFC THEN                                            00019000
          BEGIN                                                         00019100
               WTT,<X8,"{!ENTER VALUES FOR THOLD AND CT, FORMAT F12.7,X100019200
,I2{!~">);                                                              00019300
               RTT,FMIN4,THC);                                          00019400
          END;                                                          00019500
               WTT,FDN);                                                00019600
TRIAGAIN:      WTT,FPX); RTT,F3,AA1,AA2);                               00019700
               ID1.[6:6]~AA1; ID1.[12:36]~AA2;                          00019800
               BLANKIT(ID1);                                            00019900
               WTT,FSX); RTT,F3,BB1,BB2);                               00020000
               ID2.[6:6]~BB1; ID2.[12:36]~BB2;                          00020100
               BLANKIT(ID2);                                            00020200
               FILL FID WITH ID1,ID2;                                   00020300
               SEARCH(FID,BB[*]);                                       00020400
               IF BB[0]<2 THEN                                          00020500
          BEGIN                                                         00020600
               WTT,<X8,"{!YOUR FILE NOT FOUND. TRYAGAIN.{!~">);         00020700
               GO TO TRIAGAIN;                                          00020800
          END;                                                          00020900
START:         READ(FID,FMIN1,INDATA);                                  00021000
              IF X[Q] = 0.0 AND Y[Q] = 0.0 THEN GO TO AAAAA  ;          00021100
              PREV[Q] ~ 0.0  ;                                          00021200
              Q ~ Q + 1  ;  GO TO START  ;                              00021300
                                                                        00021400
AAAAA  :      S ~ Q - 1  ;  COUNT ~ 0  ;                                00021500
               WTT,<X8,"{!INSERT VALUES FOR P AND M UNDER FORMAT I2,X1,I00021600
2{!~">);                                                                00021700
               RTT,FMIN2,ORDER);  WTT,FCRLF);                           00021800
              IF NOT STEPN THEN                                         00021900
         BEGIN                                                          00022000
              N ~ P  ;  GO TO SUMS                                      00022100
         END ;                                                          00022200
                                                                        00022300
              FOR N ~ P STEP 1 UNTIL M DO                               00022400
         BEGIN                                                          00022500
              IF SELFC THEN                                             00022600
              FOR Q ~ 1 STEP 1 UNTIL S DO OMEGA[Q] ~ 1.0  ;             00022700
                                                                        00022800
SUMS  :       IF S { N THEN GO TO FINISH  ;                             00022900
              COUNT ~ COUNT + 1  ;                                      00023000
                                                                        00023100
              FOR I ~ (2|N) STEP -1 UNTIL 0 DO PART[I] ~ 0.0  ;         00023200
              FOR I ~ 1 STEP 1 UNTIL (N+1) DO EL[I,(N+2),1] ~ 0.0  ;    00023300
              OMGSUM ~ WSUM ~ 0.0  ;                                    00023400
                                                                        00023500
              FOR Q ~ 1 STEP 1 UNTIL S DO                               00023600
         BEGIN                                                          00023700
              XQ ~ X[Q]  ;  YQ ~ Y[Q]  ;  OMS ~ OMEGA[Q]|OMEGA[Q]  ;    00023800
              OMGSUM ~ OMS + OMGSUM  ;  WSUM ~ OMS|YQ|YQ + WSUM  ;      00023900
                                                                        00024000
              FOR J ~ (2|N) STEP -1 UNTIL 0 DO                          00024100
              PART[J] ~ OMS|(XQ*J) + PART[J]  ;                         00024200
              FOR I ~ 1 STEP 1 UNTIL (N+1) DO                           00024300
              EL[I,(N+2),1] ~ - OMS|YQ|(XQ*(I-1)) + EL[I,(N+2),1]  ;    00024400
         END  ;                                                         00024500
                                                                        00024600
SECOND  :     FOR I ~ 1 STEP 1 UNTIL (N+1) DO                           00024700
              FOR J ~ 1 STEP 1 UNTIL (N+1) DO EL[I,J,1] ~ PART[I+N-J]  ;00024800
                                                                        00024900
THIRD:        Q ~ N + 1  ;  K ~ 1  ;                                    00025000
                                                                        00025100
FOURTH:       FOR I ~ 1 STEP 1 UNTIL Q DO                               00025200
              DEN[I] ~ EL[I,1,K]  ;                                     00025300
                                                                        00025400
              FOR I ~ 1 STEP 1 UNTIL Q DO                               00025500
              FOR J ~ 1 STEP 1 UNTIL (Q+1) DO                           00025600
              EL[I,J,(K+1)] ~ EL[I,J,K]/DEN[I]  ;                       00025700
                                                                        00025800
              FOR J ~ 1 STEP 1 UNTIL (Q+1) DO                           00025900
              SUB[J] ~ EL[1,J,(K+1)]  ;                                 00026000
                                                                        00026100
              FOR I ~ 1 STEP 1 UNTIL Q DO                               00026200
              FOR J ~ 1 STEP 1 UNTIL (Q+1) DO                           00026300
              EL[I,J,(K+1)] ~ EL[I,J,(K+1)] - SUB[J]  ;                 00026400
                                                                        00026500
              FOR I ~ 1 STEP 1 UNTIL Q DO                               00026600
              FOR J ~ 1 STEP 1 UNTIL (Q+1) DO                           00026700
              EL[I,J,(K+1)] ~ EL[(I+1),(J+1),(K+1)]  ;                  00026800
                                                                        00026900
              IF Q = 1 THEN GO TO FIFTH  ;                              00027000
              Q ~ Q - 1  ;  K ~ K + 1  ;  GO TO FOURTH  ;               00027100
                                                                        00027200
FIFTH:        SOL[1] ~ 1.0  ;                                           00027300
                                                                        00027400
              FOR I ~ 2 STEP 1 UNTIL (N+2) DO                           00027500
         BEGIN                                                          00027600
              RT ~ 0.0  ;                                               00027700
              FOR J ~ 1 STEP 1 UNTIL (I-1) DO                           00027800
              RT ~ RT + EL[(I-1),(J+1),(N+3-I)] | SOL[I-J]  ;           00027900
              SOL[I] ~ - RT/EL[(I-1),1,(N+3-I)]  ;                      00028000
         END ;                                                          00028100
                                                                        00028200
SIXTH:         FOR Q~2 STEP 1 UNTIL (N+2) DO WTT,CONF,CN);              00028300
               IF S>30 THEN WTT,<X8,"{!!!!!!!!!!~">);                   00028400
               WTT,CNTF,COUNT);                                         00028500
               IF INTEG THEN                                            00028600
               WTT,INTGLINE1) ELSE                                      00028700
               IF DIFFL THEN                                            00028800
               WTT,DIFFLINE1) ELSE                                      00028900
               WTT,HEADLINE1);                                          00029000
                                                                        00029100
SEVENTH:      SQUSUM ~ 0.0  ;                                           00029200
                                                                        00029300
              FOR Q ~ 1 STEP 1 UNTIL S DO                               00029400
         BEGIN                                                          00029500
              CALC ~ FIRST ~ SECND ~ 0.0  ;                             00029600
              FOR I ~ 1 STEP 1 UNTIL (N+1) DO                           00029700
              CALC ~ CALC + (X[Q] * (I - 1)) | SOL[I+1]  ;              00029800
              DIFF ~ Y[Q] - CALC  ;  DIFF2 ~ DIFF|DIFF  ;               00029900
               KALC[Q]~CALC; DYFF[Q]~DIFF;                              00030000
              IF INTEG THEN                                             00030100
              FOR I ~ 1 STEP 1 UNTIL (N+1) DO                           00030200
         BEGIN                                                          00030300
              FIRST ~ FIRST + (SOL[I+1]/I) | (X[Q] * I)  ;              00030400
              SECND ~ SECND + (SOL[I+1]/(I | (I + 1))) | (X[Q] * (I+1));00030500
               FYRST[Q]~FIRST;  SECNDT[Q]~SECND;                        00030600
         END ELSE                                                       00030700
                                                                        00030800
              IF DIFFL THEN                                             00030900
         BEGIN                                                          00031000
              FOR I ~ 2 STEP 1 UNTIL (N+1) DO                           00031100
              FIRST ~ FIRST + (SOL[I+1]) | (I - 1) | (X[Q] * (I - 2))  ;00031200
              FOR I ~ 3 STEP 1 UNTIL (N+1) DO                           00031300
              SECND ~ SECND + (SOL[I+1]) | (I - 1) | ( I - 2) |         00031400
                              (X[Q] * (I - 3))  ;                       00031500
               FYRST[Q]~FIRST;  SECNDT[Q]~SECND;                        00031600
         END ELSE                                                       00031700
              FACT1 ~ FACT2 ~ 0.0  ;                                    00031800
                                                                        00031900
              FIRST ~ FIRST | FACT1  ;  SECND ~ SECND | FACT2  ;        00032000
               FYRST[Q]~FIRST;  SECNDT[Q]~SECND;                        00032100
               WTT,LISTF1,LISTING1);                                    00032200
              PREV[Q] ~ OMEGA[Q]  ;                                     00032300
               IF SELFC THEN         OMEGA[Q] ~ EXP(- DIFF2)  ;         00032400
              SQUSUM ~ DIFF2|PREV[Q]|PREV[Q] + SQUSUM  ;                00032500
         END ;                                                          00032600
               WTT,<X8,"{!!!!~">);                                      00032700
               IF INTEG THEN                                            00032800
               WTT,INTGLINE2)                                           00032900
               ELSE IF DIFFL THEN                                       00033000
               WTT,DIFFLINE2)                                           00033100
               ELSE GO TO HDLYNE2;                                      00033200
               FOR Q~1 STEP 1 UNTIL S DO                                00033300
          BEGIN                                                         00033400
               WTT,LISTF2,LISTING2);                                    00033500
          END;                                                          00033600
               GO TO EIGHTH;                                            00033700
HDLYNE2:       WTT,HEADLINE2);                                          00033800
               FOR Q~1 STEP 1 UNTIL S DO                                00033900
          BEGIN                                                         00034000
               WTT,LISTF3,LISTING3);                                    00034100
          END;                                                          00034200
                                                                        00034300
EIGHTH:   BEGIN                                                         00034400
               WTT,SUMF1,VARIANCE1);                                    00034500
               WTT,SUMF2,VARIANCE2);                                    00034600
               WTT,<X8,"{!!!!!!!!!!~">);                                00034700
          END;                                                          00034800
              IF SELFC AND COUNT < CT THEN                              00034900
         BEGIN                                                          00035000
              FOR Q ~ 1 STEP 1 UNTIL S DO                               00035100
              IF ABS(OMEGA[Q] - PREV[Q]) > THOLD THEN GO TO SUMS        00035200
         END  ;                                                         00035300
              COUNT ~ 0  ;                                              00035400
              IF NOT STEPN THEN GO TO FINISH  ;                         00035500
                                                                        00035600
NINTH:        END ;                                                     00035700
                                                                        00035800
              IF FOLLW THEN                                             00035900
         BEGIN                                                          00036000
               WTT,<X8,"{!!!!!!!!!!~">); GO TO ONSET ;                  00036100
         END  ;                                                         00036200
                                                                        00036300
FINISH:                                                                 00036400
               DATELINE(0);                                             00036500
          END.                                                          00036600
