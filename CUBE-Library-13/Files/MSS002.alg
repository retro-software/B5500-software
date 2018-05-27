BEGIN                                                                       0000
  COMMENT     PROGRAM IN ALGOL 6O TO COMPARE LEASE TO PURCHASE;             0010
  INTEGER    DEPT,PPTT,RSDT,PRDT,U,G,SERES,EEL,CHECK,SW,MODEL,              0020
              MO,DAY,YEAR,CUMP,K,ROW,COL,A;                                 0030
  INTEGER ARRAY    TABLE[0:7,0:27],PURCH[0:9],LEASE[0:9],NETP[0:9],         0040
                   CNPI[0:9],RRV[0:9],DISC[0:27];                           0050
  REAL             N,F,J,EXCT,XPN,E,PP,C,I,MP,Z,ZP,B,TEMBV,P,Y,             0060
                   SHIFT,STX,TCL,TCP,    V,FIRST,TEST,ASSOC,R;              0070
  REAL ARRAY       RATE[0:9],S[0:9],M[0:9],L[0:9],D[0:23];                  0080
                                                                            0090
  FILE IN          DATA(1,10);                                              0100
                                                                            0110
                                                                            0120
                                                                            0130
  LIST        OL2    (MODEL),                                               0140
              OL3    (MO,DAY,YEAR),                                         0150
              OL4    (EEL,P);                                               0160
                                                                            0170
  FILE OUT PRINT(1,15);                                                     0180
                                                                            0190
  FORMAT OUT  FL1  (// X48,"LEASE-PURCHASE ANALYSIS",X49),                  0200
              FL2F   (/ X57,"F",I5,X57,A2),                                 0210
              FL2E   (/ X57,"E",I5,X57,A2),                                 0220
              FL2P   (/ X57,"P",I5,X57,A2),                                 0230
              FL2B   (/ X57,"B",I5,X57,A2),                                 0240
              FL2Z   (/ X58,I5,X57,A2),                                     0250
              FL3    (/ X55,I2,"/",I2,"/",I4,X55,A2),                       0260
              FL4    (//X9,"ESTIMATED ECONOMIC LIFE",I8,X43,                0270
                      "PURCHASE PRICE",F14.2,X9,A2),                        0280
              ERROR  (X45,"DATA-ERROR--CHECK INPUT CARDS",X46);             0290
                                                                            0300
  LABEL              START,MTN,EXCSE,ZEROZ,LSECM,ZEROB,BVCOM,               0310
                      MINUS,PLUS,NEXT,NOW,BOTOM,W2F,W2E,W2P,                0320
                       W2B,W,THREE,STOP1,DISCT,DISCT1,DISCT2,DISCT3,        0330
                        DISCT4,DISCT5,XXXX99XXXX,DBLBV,MPTT,SET,VCAL,       0340
                         LSE,DIP,PTAX,TRIAL ;                               0350
  SWITCH           SUBR~DISCT1,DISCT2,DISCT3,DISCT4,DISCT5;                 0360
  SWITCH           SHAZAM~W2B,W2F,W2E,W2P,W;                                0370
START:                                                                      0380
BEGIN                                                                       0390
  FORMAT IN  FDAT1  (X1, I3,I5,2I3,I5,I3,I2,F10.1,X45,A2),                  0400
             FDAT2  (X1,9F5.2,F6.3,4F5.3,X8,A2),                            0410
             FDAT3  (X1,3F5.3,2F6.4,F4.1,4F10.1,X8,A2),                     0420
              FDAT4  (X1, 5F10.1,F4.2,X25,A2),                              0430
              FDAT5  (X1, 4F10.1,X39,A2),                                   0440
              FDAT6  (X1, 5F10.1,F4.2,F5.3,F7.5,I2,I3,I2,X6,A2),            0450
             FDAT7  (X1,I3,3F5.3,F4.2,F5.3,I2,I4,X46,A2);                   0460
                                                                            0470
  LIST        LIST1  (SERES,MODEL,MO,DAY,YEAR,EEL,PRDT,P),                  0480
              LIST2  (S[1],S[2],S[3],S[4],S[5],S[6],S[7],S[8],S[9],Y,       0490
                      D[7],D[5],D[8],D[4]),                                 0500
              LIST3  (D[12],D[13],D[15],D[10],D[11],D[14],L[1],L[2],        0510
                      L[3],L[4]),                                           0520
              LIST4  (L[5],L[6],L[7],L[8],L[9],D[3]),                       0530
              LIST5  (M[1],M[2],M[3],M[4]),                                 0540
              LIST6  (M[5],M[6],M[7],M[8],M[9],D[2],D[1],D[16],             0550
                      DEPT,G,PPTT),                                         0560
              LIST7 (U,D[6],N,F,D[9],D[17],RSDT,CHECK);                     0570
              READ(DATA,FDAT1,LIST1)[XXXX99XXXX];                           0580
              READ(DATA,FDAT2,LIST2);                                       0590
              READ(DATA,FDAT3,LIST3);                                       0600
              READ(DATA,FDAT4,LIST4);                                       0610
              READ(DATA,FDAT5,LIST5);                                       0620
              READ(DATA,FDAT6,LIST6);                                       0630
              READ(DATA,FDAT7,LIST7);                                       0640
END;                                                                        0650
                                                                            0660
                                                                            0670
              IF CHECK ! -60 THEN GO TO STOP1;                              0680
              R ~ EXCT ~ J ~ CUMP ~ 0;                                      0690
              XPN ~ D[12]+(1-D[12])|D[13] ;                                 0700
              E ~ P; PP ~ P|(1+D[7])          ; C ~ PP;                     0710
              D[18]~(G DIV 2)|2;                                            0720
              IF D[18] > 8 THEN D[18] ~ 8 ;                                 0730
              FOR ROW ~ 1 STEP 1 UNTIL  7 DO BEGIN                          0740
                 FOR COL ~ 1 STEP 1 UNTIL 27 DO                             0750
                   TABLE[ROW,COL] ~ 0 END;                                  0760
              FOR K ~ 1  STEP 1 UNTIL 27 DO DISC[K] ~ 0 ;                   0770
              FOR K ~ 1 STEP 1 UNTIL 9 DO RATE[K] ~ 0 ;                     0780
              TABLE[4,1] ~ P|(1+D[7]+D[8]);                                 0790
              FOR A ~ 1 STEP 1 UNTIL 9 DO                                   0800
                 BEGIN                                                      0810
  COMMENT        INSURANCE ;                                                0820
                   I ~ (1-D[1]|(A-1))|P|D[16] ;                             0830
                    IF I > 0 THEN TABLE[3,3|A-2] ~ I ;                      0840
  COMMENT        MAINTENANCE ;                                              0850
                 MP ~ (1+D[2]|(S[A]-1))|M[A] ;                              0860
                 IF PRDT = 0 THEN                                           0870
                    BEGIN TABLE[1,3|A-1]~MP; GO TO EXCSE END;               0880
                 IF PRDT = 1 THEN                                           0890
                    BEGIN IF A=1 THEN GO TO EXCSE; GO TO MTN END;           0900
                 IF A=1 THEN                                                0910
                    BEGIN                                                   0920
                       TABLE[1,3|A-1] ~ 0.5|MP ; GO TO EXCSE                0930
                    END ;                                                   0940
  MTN:           TABLE[1,3|A-2] ~ MP ;                                      0950
  COMMENT        LEASE ;                                                    0960
                                                                            0970
  EXCSE:         IF D[5] = 0 THEN GO TO ZEROZ ;                             0980
                 Z ~ (1+D[3]|(S[A]-1))|D[5]|L[A] ;                          0990
                 IF (EXCT+Z { P|D[7]) THEN                                  1000
                    BEGIN                                                   1010
                       ZP ~ D[5];                                           1020
                       EXCT ~ EXCT+Z ; GO TO LSECM                          1030
                    END ;                                                   1040
                 IF EXCT < P|D[7] THEN                                      1050
                    BEGIN                                                   1060
                       ZP ~ (P|D[7]-EXCT)/(L[A]|(1+D[3]|(S[A]-1)));         1070
                       EXCT ~ EXCT+Z ;  GO TO LSECM                         1080
                    END ;                                                   1090
  ZEROZ:         ZP ~ 0 ;                                                   1100
  LSECM:         TABLE[6,3|A-1] ~ -(1+D[3]|(S[A]-1))|(1+D[4]+ZP)|L[A];      1110
  COMMENT        DEPRECIATION ;                                             1120
                 IF DEPT = 0 THEN                                           1130
                    BEGIN                                                   1140
                       IF A { G THEN                                        1150
                          BEGIN                                             1160
                             B ~ PP/G ; GO TO BVCOM                         1170
                          END ;                                             1180
                       GO TO ZEROB                                          1190
                    END ;                                                   1200
                 IF DEPT = 1 THEN                                           1210
                    BEGIN                                                   1220
                       IF A { G THEN                                        1230
                          BEGIN                                             1240
                             B ~ (PP|2|(G+1-A))/(G|(G+1)) ;                 1250
                             GO TO BVCOM                                    1260
                          END ;                                             1270
                       GO TO ZEROB                                          1280
                    END ;                                                   1290
                 IF A < G THEN                                              1300
                    BEGIN                                                   1310
                       B ~ (C+C)/G; GO TO BVCOM                             1320
                    END ;                                                   1330
                 IF A = G THEN                                              1340
                    BEGIN                                                   1350
                       B ~ C ; GO TO BVCOM                                  1360
                    END ;                                                   1370
  ZEROB:         B ~ 0 ;                                                    1380
  BVCOM:         C ~ C-B ;                                                  1390
  COMMENT        PROPERTY TAX ;                                             1400
                 IF PPTT = 0 THEN                                           1410
                    BEGIN                                                   1420
                       E ~ (P|(U-A+0.5))/U ; GO TO MPTT                     1430
                    END ;                                                   1440
                 IF PPTT = 1 THEN                                           1450
                    BEGIN                                                   1460
                       IF A{U THEN                                          1470
                          BEGIN                                             1480
                             E~(P|((U-A)|(U-A+1)+U-A+1))/(U|(U+1)) ;        1490
                             GO TO MPTT                                     1500
                          END ;                                             1510
                       GO TO SET                                            1520
                    END ;                                                   1530
                 IF A < U THEN                                              1540
                    BEGIN                                                   1550
                       TEMBV ~ E-0.5|J;  J ~ (2|TEMBV)/U ;                  1560
                                                                            1570
                       GO TO DBLBV                                          1580
                    END ;                                                   1590
                 IF A = U THEN                                              1600
                    BEGIN                                                   1610
                       J ~ E-0.5|J ; TEMBV ~ J ;  GO TO DBLBV               1620
                    END ;                                                   1630
                 GO TO SET ;                                                1640
  DBLBV:         E ~ TEMBV - 0.5|J ;                                        1650
  MPTT:           IF D[6]|P > E THEN                                        1660
  SET:             E ~ D[6]|P; TABLE[2,3|A-1] ~ E|D[15] ;                   1670
  COMMENT        DISPOSAL EFFECT AND SALES TAX ;                            1680
                 IF A = 1 THEN                                              1690
                    BEGIN                                                   1700
                       SHIFT ~ 0; STX ~ P|D[8] ;                            1710
                       TCL ~P|(1+D[7])|D[11] ;                              1720
                       D[19] ~ TCL ; TCP ~ P|(1+D[7])|D[10];                1730
                       D[20] ~ TCP ;                                        1740
                       GO TO VCAL                                           1750
                    END ;                                                   1760
                 SHIFT ~ SHIFT + S[A]; STX ~ 0 ; TCL ~ 0 ; TCP ~ 0;         1770
                                                                            1780
  VCAL:  V~P|(1-N|(1+(S[1]-1)|D[9])-F|((A-1)+(SHIFT-(A-1))|D[9]));          1790
                 IF  P|D[17] > V THEN V~P|D[17];                            1800
                 IF A < 4 THEN                                              1810
                    BEGIN                                                   1820
                       D[22] ~ D[20]; D[21] ~ D[19]; GO TO DIP              1830
                    END ;                                                   1840
                 IF A } D[18] THEN                                          1850
                    BEGIN                                                   1860
                                                                            1870
                       D[22] ~ 0; GO TO LSE                                 1880
                    END ;                                                   1890
                 D[23]~(A DIV 2)|2;                                         1900
                 D[22] ~ ((D[18] - D[23])/(D[18]-2))|D[20];                 1910
  LSE:           IF A } D[14] THEN                                          1920
                    BEGIN                                                   1930
                       D[21] ~ 0 ; GO TO DIP                                1940
                    END;                                                    1950
                 D[23]~(A DIV 2)|2;                                         1960
                 D[21] ~ ((D[14]-D[23])/(D[14]-2))|D[19] ;                  1970
  DIP:           IF RSDT = 0 THEN                                           1980
                    BEGIN                                                   1990
                       RRV[A] ~ -(V-(V-C-D[22]+D[21])|XPN-D[22]+D[21]);     2000
                       GO TO PTAX                                           2010
                    END ;                                                   2020
                 RRV[A] ~ -((V-D[21])|XPN-D[22]+D[21]); GO TO PTAX ;        2030
  COMMENT        INCOM TAX ;                                                2040
  PTAX:          TABLE [5,3|A] ~ -(TABLE[1,3|A-1]+TABLE[1,3|A-2]+           2050
                    TABLE[2,3|A-1]+TABLE[3,3|A-2]+B+STX)|XPN-TCP;           2060
                 TABLE[7,3|A] ~ - TABLE[6,3|A-1]       |XPN+TCL ;           2070
  COMMENT        TOTALS FOR PRINT ;                                         2080
                 PURCH[A] ~ TABLE[1,3|A-1] + TABLE[1,3|A-2]+TABLE[2,3|      2090
                    A-1] + TABLE[3,3|A-2] +TABLE[4,3|A-2]+TABLE[5,3|A];     2100
                 LEASE[A] ~ TABLE[6,3|A-1] + TABLE[7,3|A] ;                 2110
                 NETP[A] ~ PURCH[A] + LEASE[A] ;                            2120
                 CUMP ~ CUMP + NETP[A] ; CNPI[A] ~ CUMP+RRV[A] ;            2130
  COMMENT        DISCOUNTING ;                                              2140
                 FOR K ~ 3|A-2 STEP 1 UNTIL 3|A DO                          2150
                    BEGIN                                                   2160
                       FOR ROW ~ 1 STEP 1 UNTIL 7 DO                        2170
                          DISC[K] ~ DISC[K] + TABLE[ROW,K]                  2180
                    END ;                                                   2190
                 IF CNPI[A] > 0 THEN GO TO BOTOM ;                          2200
                 IF A > 1 THEN RATE[A] ~ RATE[A-1] ;                        2210
                 SW ~ 1; GO TO DISCT ;                                      2220
  DISCT1:        IF TEST { 0 THEN GO TO TRIAL;                              2230
                 GO TO BOTOM ;                                              2240
  TRIAL:         FIRST ~ R; ASSOC ~ TEST ; R ~ R+0.04 ;                     2250
                 SW ~ 2 ; GO TO DISCT ;                                     2260
  DISCT2:        IF TEST{ 0 THEN GO TO TRIAL;                               2270
                 R ~ FIRST + (0.04|ASSOC)/(ASSOC-TEST) ;                    2280
                 SW ~ 3; GO TO DISCT ;                                      2290
  DISCT3:        IF TEST { 0 THEN GO TO PLUS ;                              2300
  MINUS:         FIRST ~ R ; ASSOC ~ TEST ;                                 2310
                 R ~ R-0.001 ; SW ~ 4 ; GO TO DISCT ;                       2320
  DISCT4:        IF TEST > 0 THEN GO TO MINUS ;                             2330
                 IF ASSOC } -TEST THEN GO TO NEXT ; GO TO NOW ;             2340
  PLUS:          FIRST ~ R ; ASSOC ~ TEST ;                                 2350
                 R ~ R+0.001; SW ~ 5 ; GO TO DISCT ;                        2360
  DISCT5:        IF TEST < 0 THEN GO TO PLUS ;                              2370
                 IF TEST } -ASSOC THEN GO TO NOW ;                          2380
  NEXT:          RATE[A] ~ (1+R)/(1+Y)-0.9995; GO TO BOTOM ;                2390
  DISCT:         TEST ~ 0 ; FOR K ~ 1 STEP 1 UNTIL A DO                     2400
                    TEST ~ TEST + DISC[3|K-2]|(1+R)*(1-K)+                  2410
                       DISC[3|K-1]|(1+R)*(.5-K)+DISC[3|K]|(1+R)*(-K) ;      2420
                 TEST ~ TEST+RRV[A]|(1+R)*(-A) ;                            2430
                 GO TO SUBR[SW] ;                                           2440
  STOP1:      WRITE(PRINT[PAGE],ERROR) ;                                    2450
                 GO TO XXXX99XXXX ;                                         2460
  NOW:           RATE[A] ~ (1+FIRST)/(1+Y)-0.9995 ;                         2470
  BOTOM:         IF RATE[A] < 0 THEN RATE[A] ~ 0 ;                          2480
                 END;                                                       2490
              WRITE(PRINT,FL1) ;                                            2500
                 GO TO SHAZAM[SERES] ;                                      2510
  W2F:        WRITE(PRINT,FL2F,OL2) ; GO TO THREE;                          2520
  W2E:        WRITE(PRINT,FL2E,OL2) ; GO TO THREE;                          2530
  W2P:        WRITE(PRINT,FL2P,OL2) ; GO TO THREE;                          2540
  W2B:        WRITE(PRINT,FL2B,OL2) ; GO TO THREE;                          2550
  W:          WRITE(PRINT,FL2Z,OL2) ;                                       2560
  THREE:                                                                    2570
BEGIN                                                                       2580
  LIST        OL5    (L[1]),                                                2590
              OL9    (TABLE[1,2],(TABLE[1,4]+TABLE[1,5]),(TABLE[1,7]+       2600
                      TABLE[1,8]),(TABLE[1,10]+TABLE[1,11]),(TABLE[1,13]    2610
                     +TABLE[1,14]),(TABLE[1,16]+TABLE[1,17]),(TABLE[1,19    2620
                      ]+TABLE[1,20]),(TABLE[1,22]+TABLE[1,23]),             2630
                      (TABLE[1,25]+TABLE[1,26])),                           2640
              OL10   (TABLE[2,2],TABLE[2,5],TABLE[2,8],TABLE[2,11],         2650
                      TABLE[2,14],TABLE[2,17],TABLE[2,20],TABLE[2,23],      2660
                      TABLE[2,26]),                                         2670
              OL11   (TABLE[3,1],TABLE[3,4],TABLE[3,7],TABLE[3,10],         2680
                      TABLE[3,13],TABLE[3,16],TABLE[3,19],TABLE[3,22],      2690
                      TABLE[3,25]),                                         2700
              OL12    (TABLE[4,1]),                                         2710
              OL13   (TABLE[5,3],TABLE[5,6],TABLE[5,9],TABLE[5,12],         2720
                      TABLE[5,15],TABLE[5,18],TABLE[5,21],TABLE[5,24],      2730
                      TABLE[5,27]),                                         2740
              OL14   (PURCH[1],PURCH[2],PURCH[3],PURCH[4],PURCH[5],         2750
                      PURCH[6],PURCH[7],PURCH[8],PURCH[9]),                 2760
              OL16   (TABLE[6,2],TABLE[6,5],TABLE[6,8],TABLE[6,11],         2770
                      TABLE[6,14],TABLE[6,17],TABLE[6,20],TABLE[6,23],      2780
                      TABLE[6,26]),                                         2790
              OL17   (TABLE[7,3],TABLE[7,6],TABLE[7,9],TABLE[7,12],         2800
                      TABLE[7,15],TABLE[7,18],TABLE[7,21],TABLE[7,24],      2810
                      TABLE[7,27]),                                         2820
              OL18   (LEASE[1],LEASE[2],LEASE[3],LEASE[4],LEASE[5],         2830
                      LEASE[6],LEASE[7],LEASE[8],LEASE[9]),                 2840
              OL20   (NETP[1],NETP[2],NETP[3],NETP[4],NETP[5],NETP[6],      2850
                      NETP[7],NETP[8],NETP[9]),                             2860
              OL21   (RRV[1],RRV[2],RRV[3],RRV[4],RRV[5],RRV[6],            2870
                      RRV[7],RRV[8],RRV[9]),                                2880
              OL22   (CNPI[1],CNPI[2],CNPI[3],CNPI[4],CNPI[5],              2890
                      CNPI[6],CNPI[7],CNPI[8],CNPI[9]),                     2900
              OL24   (RATE[1],RATE[2],RATE[3],RATE[4],RATE[5],RATE[6],      2910
                      RATE[7],RATE[8],RATE[9]);                             2920
  FORMAT OUT  FL5    (// X83,"LEASE RATE-YEAR 1",F11.2,X9,A2),              2930
              FL6    (//),                                                  2940
              FL7D   (// X9,"YEAR",X28,"1",X8,"2",X8,"3",X8,"4",            2950
                      X8,"5",X8,"6",X8,"7",X8"8",X8,"9",X6,A2),             2960
              FL8    (// X2,"PURCHASE",X110,A2),                            2970
              FL9D   (X4,"MAINTENANCE",X21,9I9,A2),                         2980
              FL10D  (X4,"PROPERTY TAX",X20,9I9,X3,A2),                     2990
              FL11D  (X4,"INSURANCE",X23,9I9,X3,A2),                        3000
              FL12D  (X4,"PURCHASE AMOUNT WITH TAXES",X6,I9,X3,A2),         3010
              FL13D  (X4"INCOME TAX EFFECT",X15,9I9,X3,A2),                 3020
              FL14D  (// X6,"COSTS OF PURCHASE",X13,9I9,X3,A2),             3030
              FL15   (// X2,"LEASE",X113,A2),                               3040
              FL16D  (X4,"LEASE EXPENSE",X19,9I9,X3,A2),                    3050
              FL18D  (// X6,"EFFECT OF LEASE",X15,9I9,X3,A2),               3060
              FL19   (//),                                                  3070
              FL20D  (/ X2,"NET COSTS OF PURCHASE",X13,9I9,X3,A2),          3080
              FL21D  (/ X2,"EFFECT OF RESIDUAL VALUE",X10,9I9,X3,A2),       3090
              FL22D  (// X2,"CUMULATIVE NET PURCHASE INVESTMENT",9I9,       3100
                      X3,A2),                                               3110
              FL23   (//),                                                  3120
              FL24D  (X2,"RATE OF RETURN OF PURCHASE"X8,9F9.3,X3,A2);       3130
              WRITE(PRINT,FL3,OL3) ;                                        3140
              WRITE(PRINT[NO],FL4,OL4) ;                                    3150
              WRITE(PRINT,FL5,OL5);                                         3160
              WRITE(PRINT,FL6) ;                                            3170
              WRITE(PRINT,FL7D);                                            3180
               WRITE(PRINT,FL8) ;                                           3190
              WRITE(PRINT,FL9D,OL9);                                        3200
              WRITE(PRINT,FL10D,OL10);                                      3210
              WRITE(PRINT,FL11D,OL11);                                      3220
              WRITE(PRINT,FL12D,OL12);                                      3230
              WRITE(PRINT,FL13D,OL13);                                      3240
              WRITE(PRINT,FL14D,OL14);                                      3250
              WRITE(PRINT,FL15) ;                                           3260
              WRITE(PRINT,FL16D,OL16) ;                                     3270
              WRITE(PRINT,FL13D,OL17) ;                                     3280
              WRITE(PRINT,FL18D,OL18) ;                                     3290
              WRITE(PRINT[NO],FL19)   ;                                     3300
              WRITE(PRINT,FL20D,OL20) ;                                     3310
              WRITE(PRINT,FL21D,OL21) ;                                     3320
              WRITE(PRINT,FL22D,OL22) ;                                     3330
              WRITE(PRINT,FL23) ;                                           3340
              WRITE(PRINT[PAGE],FL24D,OL24) ;                               3350
END ;                                                                       3360
              GO TO START;                                                  3365
  XXXX99XXXX:                                                               3370
END.                                                                        3380
