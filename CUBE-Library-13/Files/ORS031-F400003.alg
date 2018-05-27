         BEGIN                                                              1000
COMMENT       JOHN R. KESSLER                                               2000
              PROFESSIONAL SERVICES DEPARTMENT                              3000
              BURROUGHS CORPORATION                                         4000
              10-19-63                                                      5000
              AN ALGOL PROGRAM FOR SOLVING NON-HOMOGENEOUS                  6000
              LINEAR EQUATIONS.                                             7000
         ***** MEANING OF MAIN VARIABLES *****                              8000
         A[I,J]    MATRIX ENTRIES                                           9000
         AI[I,J]   ROW INDICES FOR MATRIX ENTRIES                          10000
         B[I]      CURRENT RIGHT HAND SIDE                                 11000
         BBB       BBB + 1 = NUMBER OF RIGHT HAND SIDES YET                12000
                   TO BE SOLVED FOR                                        13000
         BH[I]     COLUMN NUMBERS IN ROWS UPON WHICH THEY PIVOTED          14000
         BL1       IF "TRUE" DUMP TRANSFORMATION VECTORS PERIODICALLY      15000
         BL2       INDICATES RESTART IF "TRUE"                             16000
         BL3       INDICATES RESTART IN MIDDLE OF PROGRAM IF "TRUE"        17000
         BOOL1     IF "TRUE" THEN RESTART USING DUMPED DATA                18000
         BOOL2     IF "TRUE" OUTPUT AFTER A COLUMN IS ENTERED              19000
         CNAME[I]  COLUMN NAMES                                            20000
         CNO[I]    COLUMN DENSITIES                                        21000
         DC[I]     NAMES OF COLUMNS DELETED                                22000
         E[I,J]    TRANSFORMATION ENTRIES                                  23000
         EI[I,J]   ROW INDICES FOR TRANSFORMATION ENTRIES                  24000
         INDEX     NUMBER OF COLUMN CURRENTLY BEING UPDATED                25000
         IT        ROW NUMBER OF PIVOT                                     26000
         K         ROW INDEX FOR FIRST EMPTY ENTRY IN E[I,J]               27000
         L         COLUMN INDEX FOR FIRST EMPTY ENTRY IN E[I,J]            28000
         M         TOTAL NUMBER OF ROWS                                    29000
         N         TOTAL NUMBER OF COLUMNS                                 30000
         NR        NUMBER OF COLUMNS ENTERED                               31000
         RH[I]     NAMES OF RIGHT HAND SIDES TO BE SOLVED FOR              32000
         RJ[I]     NAMES OF ROWS DELETED                                   33000
         RNAME[I]  ROW NAMES                                               34000
         SCN[I]    COLUMN NUMBERS, SORTED BY DENSITY                       35000
         TALLY[I]  ROW DENSITIES OF COLUMNS NOT ENTERED                    36000
         TE[I]     TRANSFORMATION VECTOR STARTING POSITION IN E[I,J],      37000
                   CONTAINS PIVOT POSITION ASSOCIATED WITH VECTOR          38000
         TOL1      TOLERANCE                                               39000
         TOL2      TOLERANCE                                               40000
         TW        NUMBER OF COLUMNS TEMPORARILY SKIPPED                   41000
         W[I]      COLUMN STARTING POSITIONS IN A[I,J], CONTAINS FLAG      42000
                   INDICATING WHETHER COLUMN ENTERED                       43000
                                                                       ;   44000
FILE IN       CARD(1,10);                                                  45000
FILE OUT      PRINTER 1(1,15);                                             46000
FILE OUT      TPR 2(1,1023);                                               47000
FORMAT IN     ALPH(A6),                                                    48000
              ALP (A6,X4,A6),                                              49000
              FO3(A6,X4,2F10.7),                                           50000
              TO1(A6,X6,3F12.10),                                          51000
              CCI(A6,X4,A6),                                               52000
              FOT(A6,X2,I4,X6,I4),                                         53000
              CH(2A6,F12.6),                                               54000
              AL(12A6);                                                    55000
FORMAT OUT    FO4("IMPROPER MATRIX - NOT SQUARE"//),                       56000
              IT1("NUMBER OF   COLUMN   NUMBER OF        NUMBER OF",       57000
              "        ROW NAME  ROW NUMBER                    ",          58000
              " NUMBER OF"),                                               59000
              IT2("ITERATIONS  ENTERED  COLUMNS ENTERED  COLUMNS ",        60000
              "SKIPPED  OF PIVOT  OF PIVOT      VALUE OF PIVOT   ",        61000
              "TRANSFORMATION ENTRIES"/),                                  62000
             FIT(X3,I4,X5,A6,X8,I4,X13,I4,X9,A6,X6,I4,X6,                  63000
              E17.10,X9,I5),                                               64000
              FORI("ROWS OF INVERSE"//),                                   65000
              FOX(///"THE FOLLOWING IS THE",I4,"TH ROW OF THE"             66000
              " INVERSE CORRESPONDING TO ROW ",A6//),                      67000
              FO2("NO SOLUTION - SINGULAR"//),                             68000
              FO11("A ROW HAS NO ELEMENTS"/),                              69000
              FO5("RANK NOT GIVEN"//),                                     70000
              PCH(2A6,F12.6),                                              71000
              KK("(",I4,")",X3,A6," = ",E17.10/),                          72000
              FO7(E17.10/),                                                73000
              CONT(A6,X20,"CONTROL CARD"/),                                74000
              FOS(A6,X2,I4,X6,I4,X4,"CONTROL CARD"/),                      75000
              FRR("FREQUENCY   ",I4,"          CONTROL CARD"/),            76000
              RES("RESTART                    CONTROL CARD"/),             77000
              FOF(A6,X4,A6),                                               78000
              FOC(A6,X4,2F10.7),                                           79000
              CCO(A6,X4,A6,X10,"CONTROL CARD"/),                           80000
              PRIN(A6,X4,"PRINTER",X9,"CONTROL CARD"/),                    81000
              TO3("TOLERANCE VALUES:    TOL1 = ",F12.10,X4,"TOL2 = ",      82000
              F12.10,X4,"TOL3 = ",F12.10),                                 83000
              CONT2(A6),                                                   84000
              PR((4(" [",I4,",",I4,"]=",E17.10)/)/),                       85000
              FOE("IN LIEU OF THE CORRECT CARD, THE FOLLOWING CARD",       86000
              " WAS PRESENT",//12A6),                                      87000
              INH("INHIBIT CORE CHECK        CONTROL CARD"/),              88000
              MEM("MEMORY",X6,I4,X10,"CONTROL CARD"/),                     89000
              OUTCORE(///"********** OUT OF CORE"),                        90000
              RETO("TOLERANCES SET TO ZERO"/),                             91000
              FO11A("A COLUMN HAS NO ELEMENTS"//),                         92000
              FO1(/////"SOLUTION:    ",A6//),                              93000
              FO6("FROM WHERE IS DATA TO BE READ"//),                      94000
              FO10("END OF PROGRAM."//);                                   95000
FORMAT        TIM(/"* TIME = ",F10.3/);                                    96000
REAL          ZEROTIME;                                                    97000
ALPHA ARRAY   HEAD[0:11];                                                  98000
REAL ARRAY    RESTART[0:10];                                               99000
INTEGER       I,RR;                                                       100000
ALPHA         CONTROL;                                                    101000
LABEL         STOP,SS;                                                    102000
LABEL         L1;                                                         103000
LABEL         ENDPROGRAM;                                                 104000
REAL          RK1,RK2;                                                    105000
BOOLEAN       BOOL1;                                                      106000
SS:           READ (CARD[NO],ALPH ,CONTROL);                              107000
              IF CONTROL = "HEADIN" THEN                                  108000
         BEGIN                                                            109000
LABEL         M1;                                                         110000
              READ (CARD    ,AL   ,CONTROL);                              111000
M1:           READ  (CARD   ,AL   , FOR I ~ 0 STEP 1 UNTIL 11 DO          112000
              HEAD[I]);                                                   113000
              IF HEAD[0] ! "END   " THEN                                  114000
         BEGIN                                                            115000
              WRITE(PRINTER,AL,FOR I ~ 0 STEP 1 UNTIL 11 DO HEAD[I]);     116000
              GO TO M1;                                                   117000
         END;                                                             118000
         END;                                                             119000
              WRITE(PRINTER[DBL]);                                        120000
              BOOL1 ~ FALSE;                                              121000
              READ(CARD[NO],ALPH,CONTROL);                                122000
              IF CONTROL = "RESTAR" THEN                                  123000
COMMENT       THE PROGRAM WILL RESTART USING DATA PREVIOUSLY DUMPED       124000
              AT THE END OF THE INPUT PROCEDURES.                     ;   125000
         BEGIN                                                            126000
FILE IN       TPR(1,1023);                                                127000
              READ(CARD,ALPH,CONTROL);                                    128000
              WRITE(PRINTER,RES);                                         129000
              READ(TPR,11,RESTART[*]);                                    130000
              RK1 ~ RESTART[5];                                           131000
              RK2 ~ RESTART[6];                                           132000
              BOOL1 ~ TRUE;                                               133000
              GO TO L1;                                                   134000
         END;                                                             135000
COMMENT       THE COMPUTER WILL STOP IF THE "RANK" CARD IS NOT PRESENT.;  136000
              READ(CARD[NO],ALPH,CONTROL);                                137000
              IF CONTROL = "RANK  " THEN                                  138000
         BEGIN                                                            139000
              READ(CARD,FOT,CONTROL,RK1,RK2);                             140000
              WRITE(PRINTER[DBL],FOS,CONTROL,RK1,RK2);                    141000
         END                                                              142000
              ELSE                                                        143000
         BEGIN                                                            144000
              READ(CARD,AL,FOR I ~ 0 STEP 1 UNTIL 11 DO HEAD[I]);         145000
              WRITE(PRINTER,FOE,FOR I ~ 0 STEP 1 UNTIL 11 DO HEAD[I]);    146000
              WRITE (PRINTER,FO5);                                        147000
              GO TO STOP;                                                 148000
         END;                                                             149000
L1:      BEGIN                                                            150000
INTEGER       FLAG,J,NR,H,RKM,M,N,RT,SQ,RL,F,INDEX,AA,IT,K,L,BBB;         151000
INTEGER       CST,FREQ;                                                   152000
LABEL         ENT,SEARCH,ENT1,EXTRACT,OUTPUT,M7,M8,RAN,TRAN;              153000
REAL          X,Y,XX,YY,T1,T2,P,Q,R,S,TOL1,TOL2,SUM,RS ;                  154000
REAL          Z,ZZ,DET,TG,SU,TW,SUT;                                      155000
REAL          TEMP,MINE,MAXE;                                             156000
REAL          TOL3;                                                       157000
BOOLEAN       BL1,BL2,BL3;                                                158000
BOOLEAN       BL4,BL5;                                                    159000
LABEL         LL;                                                         160000
BOOLEAN       BOOL2;                                                      161000
ARRAY         DUMPT[0:10];                                                162000
LABEL         DUMP1;                                                      163000
ALPHA         AC,AD,AE,DL;                                                164000
ARRAY         A,AI[0:7,0:1022],E,EI[0:31,0:1022],TALLY,V,B,BH[0:RK1],     165000
              W,TE[0:RK2],FF[0:10,0:1];                                   166000
INTEGER ARRAY U[0:40];                                                    167000
INTEGER ARRAY CNO[-1:RK2],COUNT,SCN[0:RK2];                               168000
ALPHA ARRAY   CON[0:10],SG,RNAME,RJ,RH[0:RK1],DC,CNAME[0:RK2];            169000
PROCEDURE     UPDATE(J);                                                  170000
COMMENT       THIS PROCEDURE UPDATES THE CHOSEN COLUMN DURING THE STAGES  171000
              PRIOR TO INITIALLY FINDING A SOLUTION. IT IS ALSO USED TO   172000
              FIND ANY ADDITIONAL SOLUTIONS USING THE SAME "A" BUT        173000
              DIFFERENT "B".;                                             174000
VALUE         J;                                                          175000
INTEGER       J;                                                          176000
         BEGIN                                                            177000
LABEL         BB;                                                         178000
              IF FLAG = 0 THEN                                            179000
              GO TO BB;                                                   180000
              X ~ W[J].[35:3];                                            181000
              Y ~ W[J].[38:10];                                           182000
BB:           XX ~ X;                                                     183000
              YY ~ W[J+1].[38:10] - 1;                                    184000
              FOR I ~ 0 STEP 1 UNTIL RKM DO V[I] ~ 0;                     185000
COMMENT       EXPAND THE COLUMN                                       ;   186000
              IF YY < Y THEN                                              187000
         BEGIN                                                            188000
              XX ~ X + 1;                                                 189000
              FOR I ~ Y STEP 1 UNTIL 1022 DO                              190000
              V[AI[X,I]] ~ A[X,I];                                        191000
              FOR I ~ 0 STEP 1 UNTIL YY DO                                192000
              V[AI[XX,I]] ~ A[XX,I];                                      193000
         END                                                              194000
              ELSE                                                        195000
              FOR I ~ Y STEP 1 UNTIL YY DO                                196000
              V[AI[X,I]] ~ A[X,I];                                        197000
COMMENT       UPDATE THE EXPANDED COLUMN                              ;   198000
              FOR I ~ 0 STEP 1 UNTIL NR - 1 DO                            199000
         BEGIN                                                            200000
              S ~ TE[I];                                                  201000
              P ~ S.[23:10];                                              202000
              T2 ~ V[P];                                                  203000
              IF ABS(T2) > TOL1 THEN                                      204000
         BEGIN                                                            205000
              V[P] ~ 0;                                                   206000
              Q ~ S.[33:5];                                               207000
              R ~ S.[38:10];                                              208000
              RS ~ TE[I+1].[38:10] - 1;                                   209000
              IF RS } R THEN                                              210000
              FOR F ~ R STEP 1 UNTIL RS DO                                211000
              V[EI[Q,F]] ~ E[Q,F] | T2 + V[EI[Q,F]]                       212000
              ELSE                                                        213000
         BEGIN                                                            214000
              S ~ Q + 1;                                                  215000
              FOR F ~ R STEP 1 UNTIL 1022 DO                              216000
              V[EI[Q,F]] ~ E[Q,F] | T2 + V[EI[Q,F]];                      217000
              FOR F ~ 0 STEP 1 UNTIL RS DO                                218000
              V[EI[S,F]] ~ E[S,F] | T2 + V[EI[S,F]];                      219000
         END;                                                             220000
         END;                                                             221000
         END;                                                             222000
         END;                                                             223000
COMMENT       THE FOLLOWING PROCEDURES ARE STANDARD FOR INPUT.;           224000
PROCEDURE          RWID(X, Y);                                            225000
FILE          X, Y;                                                       226000
COMMENT       READ THE ROW ID FROM X, MAKE ALL THE DELETIONS ACCORDING    227000
              TO DELETE CONTROL, CONSTRUCT THE TABLE OF SLACK TYPE SG     228000
              AND THE CORRESPONDING LIST OF RNAME THE SELECTED ROW        229000
              NAMES. WRITE OUT SELECTIONS OPTIONALLY.                     230000
              CALLS ON NO OTHER PROCEDURES.                               231000
              CALLED BY INPUT. ;                                          232000
         BEGIN                                                            233000
INTEGER       G, H, I, J, K, P, Q;                                        234000
ALPHA         D;                                                          235000
LABEL         R1, R2, R3, R4, R5, R6;                                     236000
FORMAT IN     Z(A5,I1,10A6);                                              237000
FORMAT OUT    T(A5,I1,10A6),                                              238000
              TT(11A6),                                                   239000
              MESS(/"THE INDICATOR CARD ROW ID HAS NOT BEEN FOUND. ",     240000
               "INSTEAD THE FOLLOWING CARD WAS ENCOUNTERED: "/            241000
              A5, I1, 10A6/"GET OFF THE B-5000."/);                       242000
ALPHA ARRAY   S, R, A, B[0:5];                                            243000
LIST         V(D, P, FOR I ~ 1 STEP 1 UNTIL (IF P=0 THEN 1 ELSE P) DO     244000
                [S[I], R[I]]),                                            245000
              W(D, K, FOR Q ~ 1 STEP 1 UNTIL K DO [A[Q], B[Q]]);          246000
FORMAT OUT    MESS1(/"THE COMBINED NUMBER OF FUNCTIONALS AND ",           247000
               "CONSTRAINTS WAS GIVEN AS M = ", I3/"THE SAME NUMBER ",    248000
               "COMPUTED FROM DATA IS M = ", I3, I5/);                    249000
              J ~ K ~ 0;                                                  250000
R1:           READ(X, Z, V);                                              251000
              IF D.[18:6] = "*" THEN                                      252000
         BEGIN                                                            253000
              IF RT = 1 THEN WRITE(Y,T,V); GO TO R1                       254000
         END;                                                             255000
              IF D ! "ROW I" THEN                                         256000
         BEGIN                                                            257000
              WRITE(PRINTER, MESS, V); GO TO ENDPROGRAM                   258000
         END;                                                             259000
              D ~ "ROW ID";                                               260000
              IF RT = 1 THEN                                              261000
              WRITE(Y,TT,V);                                              262000
R2:           READ(X, Z, V);                                              263000
              IF D.[18:6] = "*" THEN                                      264000
         BEGIN                                                            265000
              IF RT = 1 THEN WRITE(Y,Z,V); GO TO R2;                      266000
         END;                                                             267000
              IF D = "END  " THEN                                         268000
         BEGIN                                                            269000
              IF RT = 0 THEN GO TO R6 ELSE GO TO R5                       270000
         END;                                                             271000
COMMENT       IF THE COUNTER IS BLANK, THIS MEANS ONE ROW NAME;           272000
              IF P = 0 THEN P ~ 1;                                        273000
              FOR I ~ 1 STEP 1 UNTIL P DO                                 274000
         BEGIN                                                            275000
COMMENT       RT IS A FLAG FOR WRITING.                                   276000
              RL IS THE NUMBER OF DELETED ROWS;                           277000
              IF RL = 0 THEN                                              278000
         BEGIN                                                            279000
R3:           IF J ~ J+1 = U[5] THEN                                      280000
COMMENT       INSERT A BLANK RNAME CORRESPONDING TO INFEASIBILITY;        281000
         BEGIN                                                            282000
              SG[J] ~ "      "; RNAME[J] ~ "INFEAS"; GO TO R3             283000
         END;                                                             284000
              SG[J] ~ S[I]; RNAME[J] ~ R[I];                              285000
              IF RT ! 0 THEN                                              286000
COMMENT       SELECTED ROWS ARE WRITTEN OUT ON TAPE;                      287000
         BEGIN                                                            288000
              IF K } 5 THEN                                               289000
         BEGIN                                                            290000
              WRITE(Y, T, W); K ~ 0                                       291000
         END;                                                             292000
              K ~ K + 1; B[K] ~ R[I]; A[K] ~ S[I]; GO TO R4               293000
         END                                                              294000
         END  ELSE                                                        295000
         BEGIN                                                            296000
COMMENT       THERE ARE DELETED ROWS. RL IS THE NUMBER OF ROWS WHICH      297000
              WERE REJECTED BY THE CONTROL CARD DELETE. RJ IS THE LIST;   298000
              FOR H ~ 1 STEP 1 UNTIL RL DO                                299000
              IF R[I] = RJ[H] THEN GO TO R4;                              300000
              GO TO R3;                                                   301000
         END;                                                             302000
R4:      END;                                                             303000
COMMENT       READ A NEW RECORD;                                          304000
              GO TO R2;                                                   305000
COMMENT       THE END OF THE ROW ID RECORDS IS ENCOUNTERED.               306000
              WRITE THE LAST BUFFER AND THE END CARD;                     307000
R5:           D ~ "      "; WRITE(Y,T,W); D ~ "END   "; WRITE(Y,TT,V);    308000
R6:           WRITE(PRINTER, MESS1, M,J); M ~ J                           309000
END;                                                                      310000
PROCEDURE     MATRIXREAD(X, Y);                                           311000
FILE          X, Y;                                                       312000
COMMENT       MATRIXREAD WILL READ THE COEFFICIENT MATRIX FROM FILE X,    313000
              CHOOSE A SUBSET OF ROWS AND COLUMNS, CONSTRUCT THE SLACK    314000
              VECTORS IF NEEDED.                                          315000
              IT WILL WRITE ON FILE Y IF SO SIGNALLED BY THE RT FLAG.     316000
              CALLED BY THE INPUT ROUTINE.                                317000
              CALLS ON NO OTHER PROCEDURES;                               318000
         BEGIN                                                            319000
INTEGER       C,G,H,I,J,K,L,P,R,T,V  ;                                    320000
ALPHA         SL, IC, CN, RN, TM, CA, FL ;                                321000
REAL          Q;                                                          322000
LABEL         M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13,     323000
              M14, M15, M16, M17, M18, M19, M20, M21;                     324000
LIST          ML(IC, CN, RN, Q);                                          325000
FORMAT IN     MN(3A6, F12.6);                                             326000
FORMAT OUT    MT(3A6, F12.6),                                             327000
              MISSUM1("/TOO MANY VECTORS OR ELEMENTS. ROWS = ", I4,       328000
               "  COLUMNS = ",I4," ELEMENTS = "I4,",",I5/"GET OFF THE",   329000
               " B5000"/),                                                330000
              MISSUM2(/"THE FOLLOWING UNKNOWN MATRIX CARD SAYS: "/        331000
              3A6,F12.5,/"GET OFF THE B 5000"/)  ,                        332000
              MISSUM3(/"ILLEGAL CHARACTER IN ROW NAME ENTRY"/A6,",  ",    333000
                    A6/ "TREAT THIS AS AN EQUALITY CONSTRAINT."/),        334000
              MISSUM4(/"THE PROBLEM HAS ", I4, " ROWS AND ",I4,           335000
              " COLUMNS. ",I4," MATRIX ENTRIES AND ",I3," R.H.S.",I1/),   336000
              MISSUM5(/"THE FOLLOWING ROWS ARE NOT CONSIDERED IN THIS",   337000
               " PROBLEM."/),                                             338000
              MISSUM6(X20, I3,".  ", A6),                                 339000
              MISSUM7(/"INSTEAD OF THE MATRIX INDICATOR, THE FOLLOWING",  340000
               " UNKNOWN CARD WAS ENCOUNTERED"/3A6, F12.5/"GET OFF "      341000
               "THE B-5000."/),                                           342000
              MISSUM8(/"THE FOLLOWING SPLIT VECTOR WAS ENCOUNTERED"/      343000
               3A6, F12.5/ "ITS TWO PORTIONS ARE PRECEDED BY THE ",       344000
               "FOLLOWING VECTORS: ", A6, " AND ", A6/),                  345000
              MISSUM9(/"THE FOLLOWING VECTOR: ", A6, "  WAS DELETED ",    346000
               "BECAUSE IT WAS FOUND TO BE EMPTY."/);                     347000
              K ~ H ~ N ~ L ~ P ~ C ~ 0;         G ~ 0; SL ~ "/     ";    348000
              SUT ~ 0;                                                    349000
              K ~ -1;                                                     350000
              T ~ 0;                                                      351000
M1:           READ(X, MN, ML);                                            352000
              IF IC.[12:6] = "*" THEN GO TO M2;                           353000
              IF IC = "MATRIX" THEN GO TO M3;                             354000
COMMENT       THE MATRIX INDICATOR IS NOT FOUND;                          355000
              WRITE(PRINTER, MISSUM7, ML); GO TO ENDPROGRAM;              356000
M2:           IF RT = 1 THEN                                              357000
              WRITE(Y, MT, ML);                                           358000
              GO TO M1;                                                   359000
M3:           IF RT = 1 THEN                                              360000
              WRITE(Y, MT, ML);                                           361000
M4:           READ(X, MN, ML);                                            362000
COMMENT       TEST THE INDICATOR FOR BLANKS;                              363000
              IF IC = "      " THEN                                       364000
COMMENT       TEST FOR NEW VECTOR. BLANK SIGNIFIES SAME VECTOR;           365000
         BEGIN                                                            366000
              IF N > 0 THEN                                               367000
         BEGIN                                                            368000
COMMENT       T = 2 SIGNIFIES THAT A RIGHT HAND SIDE WAS ENCOUNTERED.     369000
              IN THIS CASE THE COLUMN NAMES ARE MEANINGLESS. NEW VECTORS  370000
              OF THE RHS ARE RECOGNIZED BY THE INDICATOR FIELD ONLY;      371000
              IF (CNAME[L] = CN) OR (CN = "      ") OR (T = 2) THEN       372000
              GO TO M9                                                    373000
         END;                                                             374000
COMMENT       TEST WHETHER THIS NEW VECTOR IS IN THE LIST OF DELETED      375000
              VECTORS DC[V], THEIR NUMBER IS SQ;                          376000
M5:           FOR V ~ 1 STEP 1 UNTIL SQ DO                                377000
              IF CN = DC[V] THEN                                          378000
         BEGIN                                                            379000
COMMENT       SKIP ALL THE ENTRIES OF THIS COLUMN;                        380000
              TM ~ CN;                                                    381000
M6:           READ(X, MN, ML);                                            382000
              IF IC = "      " THEN                                       383000
         BEGIN                                                            384000
              IF (TM = CN) OR (CN = "      ") THEN GO TO M6               385000
              ELSE GO TO M5                                               386000
         END;                                                             387000
              GO TO M13                                                   388000
         END;                                                             389000
COMMENT       WAS THIS "NEW" VECTOR ENCOUNTERED BEFORE. SPLIT VECTOR;     390000
              FOR J ~ 1 STEP 1 UNTIL L - 1 DO                             391000
              IF CN = CNAME[J] THEN                                       392000
         BEGIN                                                            393000
              FL ~ " SPLIT" ;                                             394000
M7:           TM ~ CN; WRITE(PRINTER, MISSUM8, IC, CN, RN, Q, CNAME[J-1]  395000
              , CNAME[L]);                                                396000
              GO TO M6                                                    397000
         END;                                                             398000
COMMENT       IS THIS A COLUMN IN WHICH ALL THE ROWS HAVE BEEN DELETED;   399000
              IF G = 1 THEN                                               400000
         BEGIN                                                            401000
              IF P = 0 THEN                                               402000
              WRITE(PRINTER, MISSUM9, CNAME[L]); GO TO M8                 403000
         END;                                                             404000
COMMENT       BUMP THE VECTOR COUNT N, THIS IS NOT A REJECTED COLUMN;     405000
              IF L { M AND L ! 0 THEN                                     406000
         BEGIN                                                            407000
              CNO[L-1] ~ SUT; COUNT[SUT] ~ COUNT[SUT] + 1;                408000
              SUT ~ 0;                                                    409000
         END;                                                             410000
              L ~ N ~ N + 1; G ~ 1;                                       411000
M8:           IF L { 1022 THEN CNAME[L] ~ CN ELSE GO TO M10;              412000
COMMENT       THE FIRST ENTRY OF A RHS DOES NOT HAVE A ROW NAME;          413000
              IF (IC = "FIRST ") OR (IC = "NEXT B") THEN GO TO M4;        414000
COMMENT       SEARCH THE ROW ID TABLE AND FIND THE INDEX OF THIS ENTRY;   415000
M9:           FOR I ~ 1 STEP 1 UNTIL M DO                                 416000
              IF RNAME[I] = RN THEN                                       417000
COMMENT       TEST WHETHER THERE ARE TOO MANY ELEMENTS;                   418000
         BEGIN                                                            419000
              IF K ~ K + 1 > 1022 THEN                                    420000
         BEGIN                                                            421000
              K ~ 0; IF H ~ H+1 > 7 THEN GO TO M10                        422000
         END;                                                             423000
              IF G = 1 THEN GO TO M11 ELSE GO TO M12;                     424000
M10:          WRITE(PRINTER, MISSUM1, M, N, H, K);                        425000
              GO TO ENDPROGRAM                                            426000
         END;                                                             427000
COMMENT       A REJECTED ROW HAS BEEN FOUND. CHECK WHETHER IT WAS         428000
              ALREADY FOUND BEFORE. IF NOT ADD TO THE LIST OF REJECTED    429000
              ROW NAMES;                                                  430000
              FOR R ~ 1 STEP 1 UNTIL C DO                                 431000
              IF RJ[R] = RN THEN GO TO M4;                                432000
              RJ[C ~ C+1] ~ RN; GO TO M4;                                 433000
COMMENT       STORE THIS ELEMENT, ITS LOCATION AND ITS INDEX;             434000
M11:          W[L-1].[38:10] ~ K; W[L-1].[35:3] ~ H; G ~ 2;               435000
M12:          AI[H,K] ~ I - 1;  A[H,K] ~ Q;  CN ~ CNAME[L];               436000
              TEMP ~ ABS(A[H,K]);  SUT ~ SUT + 1;                         437000
              IF TEMP ! 0 THEN                                            438000
         BEGIN                                                            439000
              IF TEMP > MAXE THEN MAXE ~ TEMP;                            440000
              IF TEMP < MINE THEN MINE ~ TEMP;                            441000
         END;                                                             442000
              GO TO M3;                                                   443000
         END;                                                             444000
COMMENT       THE INDICATOR FIELD IS NOT BLANK. IGNORE * REMARKS;         445000
M13:          IF IC.[12:6] = "*" THEN GO TO M3;                           446000
              IF IC = "FIRST " THEN GO TO M14;                            447000
              IF IC = "NEXT B" THEN GO TO M17;                            448000
              IF IC = "EOF   " THEN GO TO M20;                            449000
              WRITE(PRINTER, MISSUM2, ML); GO TO ENDPROGRAM;              450000
M14:          CNO[L-1] ~ SUT; COUNT[SUT] ~ COUNT[SUT] + 1;                451000
         BEGIN                                                            452000
M16:     END;                                                             453000
COMMENT       CONSTRUCT RIGHT HAND SIDES. G=1 IS FOR LAST VECTOR EMPTY;   454000
              T ~ 1;                                                      455000
M17:          IF G = 1 THEN                                               456000
         BEGIN                                                            457000
              IF T = 1 THEN L ~ N ~ N - 1 ELSE P ~ P - 1; G ~ 2;          458000
              WRITE(PRINTER, MISSUM9, CNAME[L+1])                         459000
         END  ;                                                           460000
              IF T = 1 THEN GO TO M18 ELSE GO TO M19;                     461000
COMMENT       PRINT THE PROBLEM SIZE BEFORE AND AFTER CONSTRUCTING THE    462000
              RIGHT HAND SIDES;                                           463000
M18:          WRITE(PRINTER, MISSUM4, M, N, H|1022+ K,P);                 464000
              IF T = 1 THEN T ~ 2 ELSE GO TO M21;                         465000
M19:          IF RT = 1 THEN                                              466000
              WRITE(Y, MT, IC, CN, RN);                                   467000
              P ~ P + 1;                                                  468000
              L ~ N + P; G ~ 1; GO TO M5;                                 469000
COMMENT       FINISHED READING. TIE THINGS UP AND PRINT PROBLEM SIZE;     470000
M20:          IF RT = 1 THEN                                              471000
              WRITE(Y, MT, IC, CN, RN);                                   472000
              U[18].[35:3] ~ H; U[18].[38:10] ~ K;                        473000
              IF G = 1 THEN                                               474000
         BEGIN                                                            475000
              P ~ P - 1; L ~ N + P; WRITE(PRINTER,MISSUM9,CNAME[L+1])     476000
         END;                                                             477000
              U[28] ~ P; CNAME[L+1] ~ "EOF   "; CNAME[0] ~ "ARTFCL";      478000
              IF K ~ K + 1 > 1022 THEN                                    479000
         BEGIN                                                            480000
              IF H ~ H+1{7 THEN K ~ 0 ELSE GO TO M10                      481000
         END;                                                             482000
              W[L].[38:10] ~ K; W[L].[35:3] ~ H; GO TO M18;               483000
COMMENT       PRINT OUT THE EXPLICITELY REJECTED ROWS IF SO DESIRED;      484000
M21:          IF DL = " PRINT" AND C >0 THEN                              485000
         BEGIN                                                            486000
              WRITE(PRINTER, MISSUM5);                                    487000
              WRITE(PRINTER, MISSUM6, FOR R ~ 1 STEP 1 UNTIL C DO         488000
                   [R, RJ[R]])                                            489000
         END  ;                                                           490000
              IF FL = " SPLIT" THEN     GO TO ENDPROGRAM                  491000
              END;                                                        492000
PROCEDURE     PRICES;                                                     493000
COMMENT       THIS PROCEDURE FINDS THE DESIRED ROWS OF THE INVERSE.       494000
              IT WILL ALSO SOLVE FOR CORRESPONDING ROWS OF THE            495000
              IDENTITY MATRIX (I.E., THAT MATRIX RESULTING FROM           496000
              MATRIX MULTIPLYING THE MATRIX AND ITS INVERSE).;            497000
         BEGIN                                                            498000
INTEGER       G,B;                                                        499000
FORMAT OUT    INVCK(5(" [",I4,"] ",E16.9)/);                              500000
FORMAT        YT(//"THE FOLLOWING IS THE",I4,"TH ROW OF THE MATRIX",      501000
             "PRODUCT OF THE DATA MATRIX WITH ITS INVERSE"/);             502000
INTEGER       CHK;                                                        503000
REAL          PP,RR,UU,VV,WW,KK;                                          504000
ARRAY         L,K[0:M];                                                   505000
ALPHA ARRAY   INV[0:M];                                                   506000
LABEL         AG;                                                         507000
              CHK ~ 0;                                                    508000
              WRITE(PRINTER,FORI);                                        509000
              READ(CARD,ALPH,CONTROL);                                    510000
AG:           READ(CARD,ALPH,CONTROL);                                    511000
              IF CONTROL = "CHECK " THEN                                  512000
COMMENT       FIND CORRESPONDING ROWS OF THE IDENTITY MATRIX          ;   513000
         BEGIN                                                            514000
              CHK ~ 1;                                                    515000
              WRITE(PRINTER,CONT,CONTROL);                                516000
              GO TO AG;                                                   517000
         END;                                                             518000
              IF CONTROL ! "ALL   " THEN                                  519000
COMMENT       SOLVE FOR THE ENTIRE INVERSE                            ;   520000
         BEGIN                                                            521000
              IF CONTROL ! "END   " THEN                                  522000
         BEGIN                                                            523000
              FOR I ~ 1 STEP 1 UNTIL M DO                                 524000
              IF RNAME[I] = CONTROL THEN                                  525000
              INV[I-1] ~ CONTROL;                                         526000
              WRITE(PRINTER,CONT2,CONTROL);                               527000
              GO TO AG;                                                   528000
         END;                                                             529000
         END                                                              530000
              ELSE                                                        531000
         BEGIN                                                            532000
              WRITE(PRINTER,CONT,CONTROL);                                533000
              READ(CARD,ALPH,CONTROL);                                    534000
              FOR I ~ 1 STEP 1 UNTIL M DO                                 535000
              INV[I-1] ~ RNAME[I];                                        536000
         END;                                                             537000
COMMENT       THE FOLLOWING SECTION SOLVES FOR A ROW OF THE INVERSE   ;   538000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                               539000
              IF INV[I] ! 0 THEN                                          540000
         BEGIN                                                            541000
              FOR G ~ 0 STEP 1 UNTIL M DO                                 542000
              K[G] ~ 0;                                                   543000
              FOR J ~ 0 STEP 1 UNTIL RKM DO                               544000
              IF BH[J] = I THEN                                           545000
              K[J] ~ 1;                                                   546000
              FOR G ~ NR STEP -1 UNTIL 1 DO                               547000
         BEGIN                                                            548000
              PP ~ TE[G].[38:10] - 1;                                     549000
              UU ~ TE[G-1].[38:10];                                       550000
              VV ~ TE[G-1].[33:5];                                        551000
              WW ~ TE[G-1].[23:10];                                       552000
              KK ~ 0;                                                     553000
              IF PP < UU THEN                                             554000
         BEGIN                                                            555000
              RR ~ VV + 1;                                                556000
              FOR B ~ UU STEP 1 UNTIL 1022 DO                             557000
              KK ~ KK + K[EI[VV,B]] | E[VV,B];                            558000
              FOR B ~ 0 STEP 1 UNTIL PP DO                                559000
              KK ~ KK + K[EI[RR,B]] | E[RR,B];                            560000
         END                                                              561000
              ELSE                                                        562000
              FOR B ~ UU STEP 1 UNTIL PP DO                               563000
              KK ~ KK + K[EI[VV,B]] | E[VV,B];                            564000
              K[WW] ~ KK;                                                 565000
         END;                                                             566000
              WRITE(PRINTER,FOX,I+1,INV[I]);                              567000
              WRITE(PRINTER,PR,FOR B~0 STEP 1 UNTIL RKM DO                568000
              [I+1,B+1,K[B]]);                                            569000
COMMENT       THE FOLLOWING SECTION SOLVES FOR A ROW OF THE               570000
              IDENTITY MATRIX                                         ;   571000
              IF CHK = 1 THEN                                             572000
         BEGIN                                                            573000
              FOR J ~ 0 STEP 1 UNTIL RKM DO                               574000
         BEGIN                                                            575000
              PP ~ W[J].[35:3];                                           576000
              UU ~ W[J].[38:10];                                          577000
              VV ~ W[J+1].[38:10] - 1;                                    578000
              KK ~ 0;                                                     579000
              IF VV } UU THEN                                             580000
              FOR F ~ UU STEP 1 UNTIL VV DO                               581000
              KK ~ KK + K[AI[PP,F]] | A[PP,F]                             582000
              ELSE                                                        583000
         BEGIN                                                            584000
              FOR F ~ UU STEP 1 UNTIL 1022 DO                             585000
              KK ~ KK + K[AI[PP,F]] | A[PP,F];                            586000
              FOR F ~ 0 STEP 1 UNTIL VV DO                                587000
              KK ~ KK + K[AI[PP+1,F]] | A[PP+1,F];                        588000
         END;                                                             589000
              V[J] ~ KK;                                                  590000
         END;                                                             591000
              WRITE(PRINTER,YT,I+1);                                      592000
              WRITE(PRINTER,INVCK,FOR F~0 STEP 1 UNTIL RKM DO             593000
              [F+1,V[F]]);                                                594000
         END;                                                             595000
         END;                                                             596000
         END;                                                             597000
PROCEDURE     ERROR(SOL);                                                 598000
COMMENT       THIS PROCEDURE SOLVES AND PRINTS THE RESULT OF              599000
              SUBTRACTING THE MATRIX PRODUCT OF THE SOLUTION AND THE      600000
              COEFFICIENT MATRIX FROM THE RIGHT HAND SIDE             ;   601000
VALUE         SOL;                                                        602000
ARRAY         SOL[0];                                                     603000
         BEGIN                                                            604000
ARRAY         ER[0:RKM];                                                  605000
REAL          R,B,C,D,RHSFG;                                              606000
FORMAT        KK("(",I4,")",X3,A6,"   ERROR = ",E17.10/);                 607000
FORMAT        SK(//"ERRORS RESULTING FROM SUBTRACTING THE MATRIX",        608000
              "PRODUCT OF THE DATA MATRIX AND THE SOLUTION FROM THE",     609000
              " RIGHT HAND SIDE"//);                                      610000
FORMAT        TK("****  MAXIMUM ERROR FOUND FOR ROW ",A6,                 611000
              " , ERROR = ",E16.9," ****"/);                              612000
              FOR I~0 STEP 1 UNTIL RKM DO ER[I] ~ 0;                      613000
              IF BBB ! -1 THEN                                            614000
         BEGIN                                                            615000
              FOR I ~ M+1 STEP 1 UNTIL N DO                               616000
              IF CNAME[I] = RH[BBB] THEN                                  617000
              RHSFG ~ I - 1                                               618000
         END                                                              619000
              ELSE                                                        620000
              RHSFG ~ M;                                                  621000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                               622000
         BEGIN                                                            623000
              R ~ W[I].[35:3];                                            624000
              B ~ W[I].[38:10];                                           625000
              C ~ W[I+1].[38:10] - 1;                                     626000
              IF C } B THEN                                               627000
              FOR J ~ B STEP 1 UNTIL C DO                                 628000
              ER[AI[R,J]] ~ ER[AI[R,J]] + A[R,J] | SOL[I]                 629000
              ELSE                                                        630000
         BEGIN                                                            631000
              D ~ R + 1;                                                  632000
              FOR J ~ B STEP 1 UNTIL 1022 DO                              633000
              ER[AI[R,J]] ~ ER[AI[R,J]] + A[R,J] | SOL[I];                634000
              FOR J ~ 0 STEP 1 UNTIL C DO                                 635000
              ER[AI[D,J]] ~ ER[AI[D,J]] + A[D,J] | SOL[I];                636000
         END;                                                             637000
         END;                                                             638000
              FOR I ~ 0 STEP 1 UNTIL RKM DO V[I] ~ 0;                     639000
              R ~ W[RHSFG].[35:3];                                        640000
              B ~ W[RHSFG].[38:10];                                       641000
              C ~ W[RHSFG+1].[38:10] - 1;                                 642000
              IF C } B THEN                                               643000
              FOR J ~ B STEP 1 UNTIL C DO                                 644000
              V[AI[R,J]] ~ A[R,J]                                         645000
              ELSE                                                        646000
         BEGIN                                                            647000
              D ~ R + 1;                                                  648000
              FOR J ~ B STEP 1 UNTIL 1022 DO                              649000
              V[AI[R,J]] ~ A[R,J];                                        650000
              FOR J ~ 0 STEP 1 UNTIL C DO                                 651000
              V[AI[D,J]] ~ A[D,J];                                        652000
         END;                                                             653000
              WRITE(PRINTER,SK);                                          654000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                               655000
              WRITE(PRINTER,KK,I+1,RNAME[I+1],(ER[I] - V[I]));            656000
             R ~ 0;                                                       657000
COMMENT       THE LARGEST ERROR IS FOUND AND SEPARATELY PRINTED       ;   658000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                               659000
              IF ABS(ER[I] - V[I]) > R THEN                               660000
         BEGIN                                                            661000
              R ~ ABS(ER[I] - V[I]);                                      662000
              B ~ I;                                                      663000
         END;                                                             664000
              WRITE(PRINTER,TK,RNAME[B+1],R);                             665000
         END;                                                             666000
              NR ~ 0;                                                     667000
              SU ~ 3333;                                                  668000
              FOR F ~ 0 STEP 1 UNTIL RK1 DO BH[F] ~ SU;                   669000
              RL ~ 0;                                                     670000
              SQ ~ 0;                                                     671000
              FLAG ~0;                                                    672000
              TOL1 ~ 0.0001;                                              673000
              TOL3 ~ 0.000001;                                            674000
              XX ~ 0;                                                     675000
              BBB ~ -1;                                                   676000
              DET ~ 1;                                                    677000
              FOR I ~ 0 STEP 1 UNTIL 40 DO                                678000
              U[I] ~ 9999;                                                679000
              BL1 ~ BL2 ~ BL3 ~ FALSE;                                    680000
              TW ~ 0;                                                     681000
              BL5 ~ TRUE;                                                 682000
              MAXE ~ 0;  MINE ~ @+50;                                     683000
              IF BOOL1 THEN                                               684000
COMMENT       THE PROGRAM IS RESTARTED USING NECESSARY DATA               685000
              PREVIOUSLY DUMPED AFTER THE END OF THE INPUT PROCEDURES ;   686000
         BEGIN                                                            687000
FILE IN       TPR(1,1023);                                                688000
              READ(TPR,11,RESTART[*]);                                    689000
              IT ~ RESTART[1];                                            690000
              M ~ RESTART[2];                                             691000
              N ~ RESTART[3];                                             692000
              BBB ~ RESTART[4];                                           693000
              TOL1 ~ RESTART[7];                                          694000
              TOL2 ~ RESTART[8];                                          695000
              RKM ~ M - 1;                                                696000
              FOR I ~ 0 STEP 1 UNTIL IT DO                                697000
         BEGIN                                                            698000
              A[I,0] ~ A[I,0];                                            699000
              READ(TPR,1023,A[I,*]);                                      700000
         END;                                                             701000
              FOR I ~ 0 STEP 1 UNTIL IT DO                                702000
         BEGIN                                                            703000
              AI[I,0] ~ AI[I,0];                                          704000
              READ(TPR,1023,AI[I,*]);                                     705000
         END;                                                             706000
              READ(TPR,RK1,TALLY[*]);                                     707000
              READ(TPR,RK1,B[*]);                                         708000
              READ(TPR,RK1,RNAME[*]);                                     709000
              READ(TPR,RK2,W[*]);                                         710000
              READ(TPR,RK2,CNAME[*]);                                     711000
              READ(TPR,RK2,SCN[*]);                                       712000
              BL2 ~ TRUE;                                                 713000
         END;                                                             714000
              READ(CARD[NO],ALPH,CONTROL);                                715000
              IF CONTROL = "RESUME" THEN                                  716000
COMMENT       THE PROGRAM IS RESUMED AT THE POINT AT WHICH THE            717000
              TRANSFORMATION VECTORS WERE LAST DUMPED                 ;   718000
         BEGIN                                                            719000
FILE IN       TPX(1,1023);                                                720000
              READ(CARD,ALPH,CONTROL);                                    721000
              WRITE(PRINTER[DBL],CONT,CONTROL);                           722000
              READ(TPX,11,DUMPT[*]);                                      723000
              K ~ DUMPT[1];                                               724000
              L ~ DUMPT[2];                                               725000
              CST ~ DUMPT[3];                                             726000
              NR ~ DUMPT[4];                                              727000
              TW ~ DUMPT[5];                                              728000
              RK1 ~ DUMPT[6];                                             729000
              RK2 ~ DUMPT[7];                                             730000
              TOL1 ~ DUMPT[8];                                            731000
              TOL2 ~ DUMPT[9];                                            732000
              READ(TPX,RK1,TALLY[*]);                                     733000
              READ(TPX,RK1,B[*]);                                         734000
              READ(TPX,RK1,BH[*]);                                        735000
              READ(TPX,RK2,W[*]);                                         736000
              READ(TPX,RK2,TE[*]);                                        737000
              READ(TPX,RK2,SCN[*]);                                       738000
              FOR I ~ 0 STEP 1 UNTIL K DO                                 739000
         BEGIN                                                            740000
              E[I,0] ~ E[I,0];                                            741000
              READ(TPX,1023,E[I,*]);                                      742000
         END;                                                             743000
              FOR I ~ 0 STEP 1 UNTIL K DO                                 744000
         BEGIN                                                            745000
              EI[I,0] ~ EI[I,0];                                          746000
              READ(TPX,1023,EI[I,*]);                                     747000
         END;                                                             748000
              BL3 ~ TRUE;                                                 749000
         END;                                                             750000
              IF BL2 THEN GO TO DUMP1;                                    751000
              READ (CARD[NO],ALPH,CONTROL);                               752000
              IF CONTROL = "DELETE" THEN                                  753000
         BEGIN                                                            754000
LABEL         M1;                                                         755000
              READ (CARD,ALPH,CONTROL);                                   756000
              WRITE(PRINTER,CONT,CONTROL);                                757000
M1:           READ (CARD[NO],ALPH,CONTROL);                               758000
              IF CONTROL ! "END   " THEN                                  759000
         BEGIN                                                            760000
              READ (CARD,ALP,AC,AD);                                      761000
              WRITE(PRINTER,FOF,AC,AD);                                   762000
              IF AC = "ROW   " THEN                                       763000
              RJ[RL ~ RL + 1] ~ AD                                        764000
              ELSE                                                        765000
              DC[SQ ~ SQ + 1] ~ AD;                                       766000
              GO TO M1;                                                   767000
         END;                                                             768000
              READ(CARD,ALPH,CONTROL);                                    769000
              WRITE(PRINTER[DBL]);                                        770000
         END;                                                             771000
         BEGIN                                                            772000
FILE OUT      TAPE1 2(1,10);                                              773000
FORMAT        INP(//"THE INPUT CONTROL CARD IS NOT PRESENT."),            774000
              REA(/"READ DATA                 CONTROL CARD"/),            775000
              REAE(/"READ DATA CONTROL CARD NOT PRESENT");                776000
BOOLEAN       BLA,BLB,BLC,BLD;                                            777000
              BLA ~ BLB ~ BLC ~ BLD ~ FALSE;                              778000
              READ(CARD,CCI,AC,AD);                                       779000
              IF AC = "INPUT " THEN                                       780000
         BEGIN                                                            781000
              WRITE(PRINTER,CCO,AC,AD);                                   782000
              IF AD = "CARD  " THEN                                       783000
              BLA ~ TRUE;                                                 784000
         END                                                              785000
              ELSE                                                        786000
         BEGIN                                                            787000
              WRITE(PRINTER,INP);                                         788000
              GO TO STOP;                                                 789000
         END;                                                             790000
              READ(CARD[NO],CCI,AC,AD);                                   791000
              IF AC = "PRINT " THEN                                       792000
         BEGIN                                                            793000
              READ(CARD,CCI,AC,AD);                                       794000
              IF AD = "PRINTE" THEN                                       795000
         BEGIN                                                            796000
              WRITE(PRINTER,PRIN,AC);                                     797000
              BLB ~ TRUE;                                                 798000
         END                                                              799000
              ELSE                                                        800000
              WRITE(PRINTER,CCO,AC,AD);                                   801000
              DL ~ "PRINT ";                                              802000
         END;                                                             803000
              READ(CARD[NO],CCI,AC,AD);                                   804000
              IF AC = "WRITE " THEN                                       805000
         BEGIN                                                            806000
              READ(CARD,CCI,AC,AD);                                       807000
              IF AD = "PRINTE" THEN                                       808000
         BEGIN                                                            809000
              WRITE(PRINTER,PRIN,AC);                                     810000
              BLC ~ TRUE;                                                 811000
         END                                                              812000
              ELSE                                                        813000
              WRITE(PRINTER,CCO,AC,AD);                                   814000
              RT ~ 1;                                                     815000
         END;                                                             816000
              READ(CARD,CCI,AC,AD);                                       817000
              IF AC = "READ D" THEN                                       818000
              WRITE(PRINTER,REA)                                          819000
              ELSE                                                        820000
         BEGIN                                                            821000
              WRITE(PRINTER,REAE);                                        822000
              GO TO STOP;                                                 823000
         END;                                                             824000
              IF BLA THEN                                                 825000
         BEGIN                                                            826000
              IF BLB THEN                                                 827000
         BEGIN                                                            828000
              RWID(CARD,PRINTER);                                         829000
              MATRIXREAD(CARD,PRINTER);                                   830000
         END                                                              831000
              ELSE                                                        832000
         BEGIN                                                            833000
              RWID(CARD,TAPE1);                                           834000
              MATRIXREAD(CARD,TAPE1);                                     835000
         END;                                                             836000
         END                                                              837000
              ELSE                                                        838000
         BEGIN                                                            839000
FILE IN       TAPEB(1,10);                                                840000
              READ(CARD[NO],ALP,AC,AD);                                   841000
              IF AC = "LOCATE" THEN                                       842000
         BEGIN                                                            843000
LABEL         T;                                                          844000
              READ(CARD,ALP,AC,AD);                                       845000
T:            READ(TAPEB,ALPH,CONTROL);                                   846000
              IF CONTROL ! AD THEN                                        847000
              GO TO T;                                                    848000
         END;                                                             849000
              IF BLB THEN                                                 850000
         BEGIN                                                            851000
              RWID(TAPEB,PRINTER);                                        852000
              MATRIXREAD(TAPEB,PRINTER);                                  853000
         END                                                              854000
              ELSE                                                        855000
         BEGIN                                                            856000
              RWID(TAPEB,TAPE1);                                          857000
              MATRIXREAD(TAPEB,TAPE1);                                    858000
         END;                                                             859000
         END;                                                             860000
         END;                                                             861000
              H ~ U[28];                                                  862000
              IF M ! N THEN                                               863000
COMMENT       THAT THE MATRIX IS NOT SQUARE WILL BE PRINTED AND THE       864000
              PROGRAM WILL HALT.                                      ;   865000
         BEGIN                                                            866000
              WRITE (PRINTER,FO4);                                        867000
              GO TO STOP;                                                 868000
         END;                                                             869000
              N ~ N + U[28];                                              870000
              TOL2 ~ ABS(MINE / MAXE);                                    871000
DUMP1:                                                                    872000
              WRITE(PRINTER[DBL]);                                        873000
              READ(CARD[NO],TO1,AC,R,S,RS);                               874000
              IF AC = "TOLERA" THEN                                       875000
         BEGIN                                                            876000
FORMAT        TO2("TOLERANCES                CONTROL CARD"/);             877000
              READ(CARD,TO1,AC,R,S,RS);                                   878000
              WRITE(PRINTER,TO2);                                         879000
              TOL1 ~ R;                                                   880000
              TOL2 ~ S;                                                   881000
              TOL3 ~ RS;                                                  882000
         END;                                                             883000
              WRITE(PRINTER,TO3,TOL1,TOL2,TOL3);                          884000
              WRITE(PRINTER[DBL]);                                        885000
              READ (CARD[NO],ALPH,CONTROL);                               886000
              IF CONTROL = "CHANGE" THEN                                  887000
COMMENT       APPROPRIATE COEFFICIENT MATRIX ENTRIES ARE GIVEN            888000
              NEW VALUES                                              ;   889000
         BEGIN                                                            890000
LABEL         RET;                                                        891000
INTEGER       RW,CLM;                                                     892000
REAL          VAL;                                                        893000
              READ(CARD,ALPH,CONTROL);                                    894000
              WRITE(PRINTER,CONT,CONTROL);                                895000
RET:          READ(CARD,CH,AC,AD,VAL);                                    896000
              IF AC ! "END   " THEN                                       897000
         BEGIN                                                            898000
              WRITE(PRINTER,PCH,AC,AD,VAL);                               899000
              FOR I ~ 1 STEP 1 UNTIL M DO                                 900000
              IF RNAME[I] = AD THEN                                       901000
              RW ~ I - 1;                                                 902000
              FOR I ~ 1 STEP 1 UNTIL N DO                                 903000
              IF CNAME[I] = AC THEN                                       904000
              CLM ~ I - 1;                                                905000
              X ~ W[CLM].[35:3];                                          906000
              Y ~ W[CLM].[38:10];                                         907000
              XX ~ W[CLM+1].[35:3];                                       908000
              YY ~ W[CLM+1].[38:10] - 1;                                  909000
              IF YY = -1 THEN XX ~ XX - 1;                                910000
              FOR I ~ X STEP 1 UNTIL XX DO                                911000
              FOR J ~ 0 STEP 1 WHILE (I!XX OR J{YY) AND J{1022 DO         912000
              IF J}Y OR I!X THEN                                          913000
              IF AI[I,J] = RW THEN                                        914000
              A[I,J] ~ VAL;                                               915000
              GO TO RET;                                                  916000
         END;                                                             917000
              WRITE(PRINTER[DBL]);                                        918000
         END;                                                             919000
              RKM ~ M - 1;                                                920000
COMMENT       THIS SECTION CHECKS TO FIND IF ANY COLUMN IS EMPTY.         921000
              EMPTY MEANS THAT NO ENTRY IS LARGER THAN A TOLERANCE,       922000
              TOL1. IF A COLUMN IS FOUND EMPTY THE NUMBER AND NAME        923000
              OF THIS COLUMN WILL BE PRINTED AND THE PROGRAM WILL         924000
              HALT.                                                   ;   925000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                               926000
         BEGIN                                                            927000
              F ~ 0;                                                      928000
              IT ~ W[I].[35:3];                                           929000
              INDEX ~ W[I].[38:10];                                       930000
              L ~ W[I+1].[38:10] - 1;                                     931000
              IF L < INDEX THEN                                           932000
         BEGIN                                                            933000
              K ~ IT + 1;                                                 934000
              FOR J ~ INDEX STEP 1 UNTIL 1022 DO                          935000
              IF ABS(A[IT,J]) > TOL1 THEN                                 936000
              F ~ F + 1;                                                  937000
              FOR J~0 STEP 1 UNTIL L DO IF ABS(A[K,J]) > TOL1 THEN        938000
              F ~ F + 1;                                                  939000
         END                                                              940000
              ELSE                                                        941000
              FOR J ~ INDEX STEP 1 UNTIL L DO                             942000
              IF ABS(A[IT,J]) > TOL1 THEN                                 943000
              F ~ F + 1;                                                  944000
              IF F = 0 THEN                                               945000
         BEGIN                                                            946000
              WRITE(PRINTER,FO11A);                                       947000
              WRITE(PRINTER,CONT,CNAME[I+1]);                             948000
              GO TO STOP;                                                 949000
         END;                                                             950000
         END;                                                             951000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                               952000
              TALLY[I] ~ 0;                                               953000
COMMENT       THIS SECTION CHECKS TO FIND IF ANY ROW IS EMPTY. EMPTY      954000
              MEANS THAT NO ENTRY IS LARGER THAN A TOLERANCE, TOL1.       955000
              IF A ROW IS FOUND EMPTY, THE NUMBER AND NAME OF THIS ROW    956000
              WILL BE PRINTED AND THE PROGRAM WILL HALT. THIS             957000
              SECTION ALSO FINDS THE DENSITIES OF THE ROWS.           ;   958000
         BEGIN                                                            959000
              IT ~ W[M].[33:5];                                           960000
              INDEX ~ W[M].[38:10] - 1;                                   961000
              IF INDEX = -1 THEN IT ~ IT - 1;                             962000
              FOR I ~ 0 STEP 1 UNTIL IT DO                                963000
              IF I ! IT THEN                                              964000
         BEGIN                                                            965000
              FOR J ~ 0 STEP 1 UNTIL 1022 DO                              966000
              IF ABS(A[I,J]) > TOL1 THEN                                  967000
              TALLY[AI[I,J]] ~ TALLY[AI[I,J]] + 1;                        968000
         END                                                              969000
              ELSE                                                        970000
              FOR J ~ 0 STEP 1 UNTIL INDEX DO                             971000
              IF ABS(A[I,J]) > TOL1 THEN                                  972000
              TALLY[AI[IT,J]] ~ TALLY[AI[I,J]] + 1;                       973000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                               974000
              IF TALLY[I] = 0 THEN                                        975000
              BEGIN                                                       976000
              WRITE(PRINTER,FO11);                                        977000
              WRITE(PRINTER,CONT,RNAME[I+1]);                             978000
              GO TO STOP;                                                 979000
         END;                                                             980000
         END;                                                             981000
              READ(CARD[NO],ALPH,CONTROL);                                982000
              IF CONTROL = "FACTOR" THEN                                  983000
COMMENT       APPROPRIATE COLUMNS WILL BE MULTIPLIED BY THE GIVEN         984000
              FACTORS.                                                ;   985000
         BEGIN                                                            986000
LABEL         REPSOL;                                                     987000
FORMAT        FR("THE SOLUTION FOR COLUMN ",A6," MUST BE MULTIPLIED BY",  988000
              " @",I4," TO OBTAIN THE CORRECT RESULTS."/);                989000
              READ(CARD,ALPH,CONTROL);                                    990000
              WRITE(PRINTER[DBL],CONT,CONTROL);                           991000
REPSOL:       READ(CARD,FOT,CONTROL,Z,ZZ);                                992000
              IF CONTROL ! "END   " THEN                                  993000
         BEGIN                                                            994000
              FOR I ~ 1 STEP 1 UNTIL M DO                                 995000
              IF CNAME[I] = CONTROL THEN                                  996000
         BEGIN                                                            997000
              WRITE(PRINTER,FR,CNAME[I],Z);                               998000
              Q ~ W[I-1].[35:3];                                          999000
              R ~ W[I-1].[38:10];                                        1000000
              RS ~ W[I].[38:10] - 1;                                     1001000
              IF RS}R THEN                                               1002000
              FOR J ~ R STEP 1 UNTIL RS DO                               1003000
              A[Q,J] ~ A[Q,J] | 10*Z                                     1004000
              ELSE                                                       1005000
         BEGIN                                                           1006000
              FOR J ~ R STEP 1 UNTIL 1022 DO                             1007000
              A[Q,J] ~ A[Q,J] | 10*Z;                                    1008000
              FOR J ~ 0 STEP 1 UNTIL RS DO                               1009000
              A[Q+1,J] ~ A[Q+1,J] | 10*Z;                                1010000
         END;                                                            1011000
              GO TO REPSOL                                               1012000
         END;                                                            1013000
         END;                                                            1014000
         END;                                                            1015000
              READ (CARD[NO],ALPH,CONTROL);                              1016000
              IF CONTROL = "RHS   " THEN                                 1017000
COMMENT       THE PROGRAM WILL SOLVE FOR ALL OF THE DESIRED RIGHT        1018000
              HAND SIDES.  IF THIS CONTROL CARD IS NOT PRESENT THE       1019000
              PROGRAM WILL SOLVE THE EQUATIONS USING THE FIRST RIGHT     1020000
              HAND SIDE.                                              ;  1021000
         BEGIN                                                           1022000
              READ(CARD,ALPH,CONTROL);                                   1023000
              WRITE(PRINTER,CONT,CONTROL);                               1024000
M7:           READ (CARD[NO],ALPH,CONTROL);                              1025000
              IF CONTROL ! "END   " THEN                                 1026000
         BEGIN                                                           1027000
              READ(CARD,ALPH,RH[BBB ~ BBB + 1]);                         1028000
              WRITE(PRINTER,CONT2,RH[BBB]);                              1029000
              GO TO M7;                                                  1030000
         END                                                             1031000
              ELSE                                                       1032000
              READ(CARD,ALPH,CONTROL);                                   1033000
              WRITE(PRINTER[DBL]);                                       1034000
         END;                                                            1035000
              FOR I ~ 0 STEP 1 UNTIL RKM DO B[I] ~ 0;                    1036000
              IF BBB = -1 THEN                                           1037000
         BEGIN                                                           1038000
              FLAG ~ 1;                                                  1039000
              UPDATE(M);                                                 1040000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                              1041000
              B[I] ~ V[I];                                               1042000
              FLAG ~ 0;                                                  1043000
              AE ~ CNAME[M+1];                                           1044000
         END                                                             1045000
              ELSE                                                       1046000
              FOR I ~ M + 1 STEP 1 UNTIL N DO                            1047000
              IF CNAME[I] = RH[BBB] THEN                                 1048000
         BEGIN                                                           1049000
              AE ~ CNAME[I];                                             1050000
              FLAG ~ 1;                                                  1051000
              UPDATE(I-1);                                               1052000
              FOR F ~ 0 STEP 1 UNTIL RKM DO                              1053000
              B[F] ~ V[F];                                               1054000
              FLAG ~ 0;                                                  1055000
              I ~ N ;                                                    1056000
         END;                                                            1057000
              IF NOT BL2 THEN                                            1058000
         BEGIN                                                           1059000
COMMENT       THE COLUMNS ARE SORTED BY THEIR DENSITIES.              ;  1060000
              FOR I ~ 1 STEP 1 UNTIL M DO                                1061000
              COUNT[I] ~ COUNT[I] + COUNT[I-1];                          1062000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                              1063000
         BEGIN                                                           1064000
              SCN[COUNT[CNO[I]]] ~ I;                                    1065000
              COUNT[CNO[I]] ~ COUNT[CNO[I]] - 1;                         1066000
         END;                                                            1067000
         END;                                                            1068000
              READ(CARD[NO],ALPH,CONTROL);                               1069000
              IF CONTROL = "DUMP  " THEN                                 1070000
COMMENT       THE INITIALIZED MATRICES WILL BE DUMPED AT THIS POINT.  ;  1071000
         BEGIN                                                           1072000
              RESTART[1] ~ IT ~ W[N].[35:3];                             1073000
              RESTART[2] ~ M;                                            1074000
              RESTART[3] ~ N;                                            1075000
              RESTART[4] ~ BBB;                                          1076000
              RESTART[5] ~ RK1;                                          1077000
              RESTART[6] ~ RK2;                                          1078000
              RESTART[7] ~ TOL1;                                         1079000
              RESTART[8] ~ TOL2;                                         1080000
              WRITE(TPR,11,RESTART[*]);                                  1081000
              FOR I ~ 0 STEP 1 UNTIL IT DO                               1082000
         BEGIN                                                           1083000
              A[I,0] ~ A[I,0];                                           1084000
              WRITE(TPR,1023,A[I,*]);                                    1085000
         END;                                                            1086000
              FOR I ~ 0 STEP 1 UNTIL IT DO                               1087000
         BEGIN                                                           1088000
              AI[I,0] ~ AI[I,0];                                         1089000
              WRITE(TPR,1023,AI[I,*]);                                   1090000
         END;                                                            1091000
              WRITE(TPR,RK1,TALLY[*]);                                   1092000
              WRITE(TPR,RK1,B[*]);                                       1093000
              WRITE(TPR,RK1,RNAME[*]);                                   1094000
              WRITE(TPR,RK2,W[*]);                                       1095000
              WRITE(TPR,RK2,CNAME[*]);                                   1096000
              WRITE(TPR,RK2,SCN[*]);                                     1097000
              READ(CARD,ALPH,CONTROL);                                   1098000
              WRITE(PRINTER[DBL],CONT,CONTROL);                          1099000
         END;                                                            1100000
              READ(CARD[NO],FOT,CONTROL,FREQ);                           1101000
              IF CONTROL = "FREQUE" THEN                                 1102000
COMMENT       THE TRANSFORMATION VECTORS WILL BE PERIODICALLY            1103000
              DUMPED.                                                 ;  1104000
         BEGIN                                                           1105000
              READ(CARD,FOT,CONTROL,FREQ);                               1106000
              WRITE(PRINTER,FRR,FREQ);                                   1107000
              BL1 ~ TRUE;                                                1108000
         END;                                                            1109000
              READ(CARD[NO],FOT,CONTROL,RS);                             1110000
              IF CONTROL = "MEMORY" THEN                                 1111000
         BEGIN                                                           1112000
              READ(CARD,FOT,CONTROL,RS);                                 1113000
              WRITE(PRINTER,MEM,RS);                                     1114000
              MAXE ~ 16000 + (RS - 4) | 2000;                            1115000
         END                                                             1116000
              ELSE                                                       1117000
              MAXE ~ 24000;                                              1118000
              BL4 ~ TRUE;                                                1119000
              READ(CARD[NO],ALPH,CONTROL);                               1120000
              IF CONTROL = "INHIBI" THEN                                 1121000
         BEGIN                                                           1122000
              READ(CARD,ALPH,CONTROL);                                   1123000
              WRITE(PRINTER,INH);                                        1124000
              BL4 ~ FALSE;                                               1125000
         END;                                                            1126000
              AA ~ 0;                                                    1127000
              BOOL2 ~ FALSE;                                             1128000
              READ(CARD[NO],ALPH,CONTROL);                               1129000
              IF CONTROL = "OUTPUT" THEN                                 1130000
COMMENT       AFTER A COLUMN IS ENTERED A LINE OF OUTPUT WILL OCCUR.  ;  1131000
         BEGIN                                                           1132000
              READ(CARD,ALPH,CONTROL);                                   1133000
              WRITE(PRINTER[DBL],CONT,CONTROL);                          1134000
              BOOL2 ~ TRUE;                                              1135000
         END;                                                            1136000
              IF BOOL2 THEN                                              1137000
         BEGIN                                                           1138000
              WRITE(PRINTER,IT1);                                        1139000
              WRITE(PRINTER,IT2);                                        1140000
         END;                                                            1141000
              ZEROTIME ~ TIME(1);                                        1142000
              WRITE(PRINTER,TIM,0);                                      1143000
              IF BL3 THEN GO TO ENT;                                     1144000
         BEGIN                                                           1145000
COMMENT       THIS SECTION SEARCHES FOR A SUITABLE COLUMN FROM "A" TO    1146000
ENTER THE BASIS.;                                                        1147000
              CST ~ 0;                                                   1148000
COMMENT       HAVE ALL THE COLUMNS BEEN ENTERED.                      ;  1149000
ENT:          IF NR = M THEN GO TO OUTPUT;                               1150000
COMMENT       HAVE ALL THE COLUMNS BEEN ENTERED THAT CAN BE BY THE       1151000
              FIRST METHOD FOR SELECTING PIVOTS.                      ;  1152000
SEARCH:       IF NR + TW = M THEN                                        1153000
COMMENT       ATTEMPT TO ENTER THE REMAINING COLUMNS BY THE SECOND       1154000
              METHOD OF SELECTING PIVOTS.                             ;  1155000
         BEGIN                                                           1156000
              IF BL5 THEN                                                1157000
              WRITE(PRINTER,RETO);                                       1158000
              BL5 ~ FALSE;                                               1159000
              FOR J ~ 0 STEP 1 UNTIL RKM DO                              1160000
              IF W[J].[25:10] ! 1023 THEN                                1161000
         BEGIN                                                           1162000
              FLAG ~ 1     ;                                             1163000
              UPDATE(J);                                                 1164000
              FLAG ~ 0;                                                  1165000
              INDEX ~ J;                                                 1166000
              SUM ~ 0;  IT ~ -1;                                         1167000
COMMENT       FIND THE LARGEST AVAILABLE PIVOT.                       ;  1168000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                              1169000
              IF ABS(V[I]) > SUM AND BH[I] = SU THEN                     1170000
         BEGIN                                                           1171000
              SUM ~ ABS(V[I]);   IT ~ I;                                 1172000
         END;                                                            1173000
COMMENT       IF A PIVOT HAS NOT BEEN FOUND GO TO THE OUTPUT             1174000
              SECTION.                                                ;  1175000
              IF IT = -1 THEN GO TO OUTPUT;                              1176000
              TW ~ TW - 1;                                               1177000
              BH[IT] ~ J;                                                1178000
              W[J].[25:10] ~ 1023;                                       1179000
              GO TO TRAN;                                                1180000
         END;                                                            1181000
         END;                                                            1182000
              CST ~ CST + 1;                                             1183000
COMMENT       SELECT THE NEXT COLUMN FROM THE LIST OF SORTED COLUMNS. ;  1184000
              J ~ SCN[CST];                                              1185000
EXTRACT:      X ~ W[J].[35:3];                                           1186000
              Y ~ W[J].[38:10];                                          1187000
ENT1:         INDEX ~ J;                                                 1188000
COMMENT       UPDATE THE SELECTED COLUMN.                             ;  1189000
              UPDATE(J);                                                 1190000
COMMENT       ATTEMPT TO FIND A PIVOT ROW BY MAXIMIZING OVER THE         1191000
              SET OF MINIMUM ROW DENSITIES FOR THE ROWS ORIGINALLY       1192000
              PRESENT IN THE COEFFICIENT MATRIX.                      ;  1193000
              IF X ! XX THEN                                             1194000
         BEGIN                                                           1195000
              SUM ~ @+50;                                                1196000
              IT ~ -1;                                                   1197000
              FOR I ~ Y STEP 1 UNTIL 1022 DO                             1198000
         BEGIN                                                           1199000
              RS ~ AI[X,I];                                              1200000
              IF BH[RS] = SU THEN                                        1201000
              IF TALLY[RS] { SUM THEN                                    1202000
         BEGIN                                                           1203000
              IF TALLY[RS] < SUM THEN                                    1204000
         BEGIN                                                           1205000
              IT ~ RS;                                                   1206000
              TG ~ ABS(V[RS]);                                           1207000
              SUM ~ TALLY[RS];                                           1208000
         END                                                             1209000
              ELSE                                                       1210000
              IF ABS(V[RS]) > TG THEN                                    1211000
         BEGIN                                                           1212000
              TG ~ ABS(V[RS]);                                           1213000
              IT ~ RS;                                                   1214000
         END;                                                            1215000
         END;                                                            1216000
         END;                                                            1217000
              FOR I ~ 0 STEP 1 UNTIL YY DO                               1218000
         BEGIN                                                           1219000
              RS ~ AI[XX,I];                                             1220000
              IF BH[RS] = SU THEN                                        1221000
              IF TALLY[RS] { SUM THEN                                    1222000
         BEGIN                                                           1223000
              IF TALLY[RS] < SUM THEN                                    1224000
         BEGIN                                                           1225000
              IT ~ RS;                                                   1226000
              TG ~ ABS(V[RS]);                                           1227000
              SUM ~ TALLY[RS];                                           1228000
         END                                                             1229000
              ELSE                                                       1230000
              IF ABS(V[RS]) > TG THEN                                    1231000
         BEGIN                                                           1232000
              TG ~ ABS(V[RS]);                                           1233000
              IT ~ RS;                                                   1234000
         END;                                                            1235000
         END;                                                            1236000
         END;                                                            1237000
         END                                                             1238000
              ELSE                                                       1239000
         BEGIN                                                           1240000
              SUM ~ @+50;                                                1241000
              IT ~ -1;                                                   1242000
              FOR I ~ Y STEP 1 UNTIL YY DO                               1243000
         BEGIN                                                           1244000
              RS ~ AI[X,I];                                              1245000
              IF BH[RS] = SU THEN                                        1246000
              IF TALLY[RS] { SUM THEN                                    1247000
         BEGIN                                                           1248000
              IF TALLY[RS] < SUM THEN                                    1249000
         BEGIN                                                           1250000
              IT ~ RS;                                                   1251000
              TG ~ ABS(V[RS]);                                           1252000
              SUM ~ TALLY[RS];                                           1253000
         END                                                             1254000
              ELSE                                                       1255000
              IF ABS(V[RS]) > TG THEN                                    1256000
         BEGIN                                                           1257000
              TG ~ ABS(V[RS]);                                           1258000
              IT ~ RS;                                                   1259000
         END;                                                            1260000
         END;                                                            1261000
         END;                                                            1262000
         END;                                                            1263000
              IF TG < TOL2 THEN                                          1264000
COMMENT       NO PIVOT WAS FOUND LARGER THAN A TOLERANCE, TOL2.       ;  1265000
              IT ~ -1;                                                   1266000
              IF IT = -1 THEN                                            1267000
         BEGIN                                                           1268000
              SUM ~ @+50;                                                1269000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                              1270000
              IF ABS(V[I]) > TOL2 THEN                                   1271000
              IF BH[I] = SU THEN                                         1272000
              IF TALLY[I] { SUM THEN                                     1273000
         BEGIN                                                           1274000
              IF TALLY[I] < SUM THEN                                     1275000
         BEGIN                                                           1276000
              IT ~ I;                                                    1277000
              TG ~ ABS(V[I]);                                            1278000
              SUM ~ TALLY[I];                                            1279000
         END                                                             1280000
              ELSE                                                       1281000
              IF ABS(V[I]) > TG THEN                                     1282000
         BEGIN                                                           1283000
              TG ~ ABS(V[I]);                                            1284000
              IT ~ I;                                                    1285000
         END;                                                            1286000
         END;                                                            1287000
         END;                                                            1288000
              IF IT = -1 THEN                                            1289000
COMMENT       NO PIVOT WAS FOUND. BOOST THE COUNTER FOR ROWS SKIPPED     1290000
              BY ONE.                                                 ;  1291000
         BEGIN                                                           1292000
              TW ~ TW + 1;                                               1293000
              GO TO SEARCH;                                              1294000
         END;                                                            1295000
COMMENT       STORE THE COLUMN NUMBER IN THE ROW UPON WHICH IT           1296000
              PIVOTED.                                                ;  1297000
              BH[IT] ~ INDEX;                                            1298000
COMMENT       MARK THE COLUMN AS ENTERED.                             ;  1299000
              W[J].[25:10] ~ 1023;                                       1300000
COMMENT       DECREASE THE TALLY  (ROW DENSITY) COUNT.                ;  1301000
              IF X ! XX THEN                                             1302000
         BEGIN                                                           1303000
              FOR I ~ Y STEP 1 UNTIL 1022 DO                             1304000
              TALLY[AI[X,I]] ~ TALLY[AI[X,I]] - 1;                       1305000
              FOR I ~ 0 STEP 1 UNTIL YY DO                               1306000
              TALLY[AI[XX,I]] ~ TALLY[AI[XX,I]] - 1;                     1307000
         END                                                             1308000
              ELSE                                                       1309000
              FOR I ~ Y STEP 1 UNTIL YY DO                               1310000
              TALLY[AI[X,I]] ~ TALLY[AI[X,I]] - 1;                       1311000
COMMENT       FIND NEW TRANSFORMATION VECTOR,"E", AND UPDATE "B".;       1312000
TRAN:    BEGIN                                                           1313000
              T1 ~  V[IT];                                               1314000
              V[IT] ~ -1;                                                1315000
              T2 ~ B[IT];                                                1316000
              B[IT] ~ 0;                                                 1317000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                              1318000
         BEGIN                                                           1319000
              V[I] ~ -V[I] / T1;                                         1320000
              B[I] ~ T2 | V[I] + B[I];                                   1321000
         END;                                                            1322000
COMMENT       STORE "E","EI","TE".;                                      1323000
              TE[NR].[23:10] ~ IT;                                       1324000
              K ~ TE[NR].[33:5];                                         1325000
              L ~ TE[NR].[38:10];                                        1326000
              FOR I ~ 0 STEP 1 UNTIL RKM DO                              1327000
              IF ABS(V[I]) > TOL3 THEN                                   1328000
              IF L ! 1023 THEN                                           1329000
         BEGIN                                                           1330000
              E[K,L] ~ V[I];                                             1331000
              EI[K,L] ~ I;                                               1332000
              L ~ L + 1;                                                 1333000
         END                                                             1334000
              ELSE                                                       1335000
         BEGIN                                                           1336000
              K ~ K+1;                                                   1337000
              L ~ 0;                                                     1338000
              I ~ I - 1;                                                 1339000
         END;                                                            1340000
              NR ~ NR + 1;                                               1341000
              TE[NR].[33:5] ~ K;                                         1342000
              TE[NR].[38:10] ~ L;                                        1343000
COMMENT       THE NEXT SECTION WILL DUMP THE TRANSFORMATION VECTORS      1344000
              AND OTHER DATA NEEDED TO RESUME EITHER AFTER               1345000
              MULTIPLES OF 7 ROWS OF TRANSFORMATION VECTOR ENTRIES       1346000
              HAVE BEEN GENERATED OR AT A GIVEN FREQUENCY.            ;  1347000
              IF K MOD 7 = 0 THEN                                        1348000
              IF FREQ = 0 THEN                                           1349000
              IF K ! 0 THEN                                              1350000
              GO TO LL;                                                  1351000
              IF BL1 THEN                                                1352000
              IF NR MOD FREQ = 0 THEN                                    1353000
LL:      BEGIN                                                           1354000
FILE OUT      TPX 2(1,1023);                                             1355000
              DUMPT[1] ~ K;                                              1356000
              DUMPT[2] ~ L;                                              1357000
              DUMPT[3] ~ CST;                                            1358000
              DUMPT[4] ~ NR;                                             1359000
              DUMPT[5] ~ TW;                                             1360000
              DUMPT[6] ~ RK1;                                            1361000
              DUMPT[7] ~ RK2;                                            1362000
              DUMPT[8] ~ TOL1;                                           1363000
              DUMPT[9] ~ TOL2;                                           1364000
              WRITE(TPX,11,DUMPT[*]);                                    1365000
              WRITE(TPX,RK1,TALLY[*]);                                   1366000
              WRITE(TPX,RK1,B[*]);                                       1367000
              WRITE(TPX,RK1,BH[*]);                                      1368000
              WRITE(TPX,RK2,W[*]);                                       1369000
              WRITE(TPX,RK2,TE[*]);                                      1370000
              WRITE(TPX,RK2,SCN[*]);                                     1371000
              FOR I ~ 0 STEP 1 UNTIL K DO                                1372000
         BEGIN                                                           1373000
              E[I,0] ~ E[I,0];                                           1374000
              WRITE(TPX,1023,E[I,*]);                                    1375000
         END;                                                            1376000
              FOR I ~ 0 STEP 1 UNTIL K DO                                1377000
         BEGIN                                                           1378000
              EI[I,0] ~ EI[I,0];                                         1379000
              WRITE(TPX,1023,EI[I,*]);                                   1380000
         END;                                                            1381000
         END;                                                            1382000
              IF BOOL2 THEN                                              1383000
COMMENT       A LINE OF OUTPUT PER COLUMN ENTERED WAS REQUESTED.      ;  1384000
              WRITE(PRINTER,FIT,AA~AA+1,CNAME[INDEX+1],NR,TW,            1385000
              RNAME[IT+1],IT+1,T1,1023|K+L);                             1386000
              IF BL4 THEN                                                1387000
              IF 1023 | K + L + M > MAXE THEN                            1388000
         BEGIN                                                           1389000
              WRITE(PRINTER,OUTCORE);                                    1390000
              GO TO STOP;                                                1391000
         END;                                                            1392000
              GO TO ENT;                                                 1393000
         END;                                                            1394000
COMMENT       THE FOLLOWING SECTION CHECKS TO SEE IF THE COEFFICIENT     1395000
              MATRIX IS SINGULAR. THE SOLUTION IS THEN PRINTED. ANY      1396000
              OTHER SOLUTIONS ARE COMPUTED AND PRINTED IF REQUESTED.   ; 1397000
OUTPUT:       FOR I ~ 0 STEP 1 UNTIL RKM DO                              1398000
              IF BH[I] =SU THEN                                          1399000
COMMENT       THE MATRIX WAS FOUND SINGULAR. A MESSAGE WILL BE PRINTED;  1400000
         BEGIN                                                           1401000
              WRITE(PRINTER,FO2);                                        1402000
              GO TO STOP;                                                1403000
         END;                                                            1404000
              FOR J ~ 0 STEP 1 UNTIL RKM DO                              1405000
              V[BH[J]] ~ B[J];                                           1406000
              WRITE(PRINTER,TIM,(TIME(1) - ZEROTIME)/60);                1407000
COMMENT       PRINT THE SOLUTION.                                     ;  1408000
              WRITE(PRINTER,FO1,AE);                                     1409000
              WRITE(PRINTER,KK,FOR F~0 STEP 1 UNTIL RKM DO               1410000
              [F+1,CNAME[F+1],V[F]]);                                    1411000
              ERROR(V);                                                  1412000
              BBB ~ BBB - 1;                                             1413000
M8:           IF BBB } 0 THEN                                            1414000
COMMENT       SOLVE FOR AND PRINT ANY ADDITIONAL REQUESTED SOLUTIONS. ;  1415000
         BEGIN                                                           1416000
             FOR I ~ 0 STEP 1 UNTIL RKM DO B[I] ~ 0;                     1417000
              FLAG ~ 1;                                                  1418000
              FOR I ~ M + 1 STEP 1 UNTIL N DO                            1419000
              IF CNAME[I] = RH[BBB] THEN                                 1420000
         BEGIN                                                           1421000
              WRITE(PRINTER,FO1,CNAME[I]);                               1422000
              UPDATE(I-1);                                               1423000
              FOR J ~ 0 STEP 1 UNTIL RKM DO                              1424000
              B[BH[J]] ~ V[J]   ;                                        1425000
              WRITE(PRINTER,KK,FOR F~0 STEP 1 UNTIL RKM DO               1426000
              [F+1,CNAME[F+1],B[F]]);                                    1427000
              ERROR(B);                                                  1428000
              BBB ~ BBB - 1;                                             1429000
              GO TO M8;                                                  1430000
         END;                                                            1431000
         END;                                                            1432000
         END;                                                            1433000
              READ(CARD[NO],ALPH,CONTROL);                               1434000
              IF CONTROL = "ROWS O" THEN                                 1435000
COMMENT       SOLVE FOR REQUESTED ROWS OF THE INVERSE.                ;  1436000
              PRICES;                                                    1437000
         END;                                                            1438000
COMMENT       IF THE "NEW PROBLEM" CONTROL CARD IS PRESENT THE PROGRAM   1439000
              WILL RUN AGAIN EXPECTING ALL NECESSARY CONTROL CARDS AND   1440000
              DATA TO IMMEDIATELY FOLLOW.                              ; 1441000
              READ(CARD[NO],ALPH,CONTROL);                               1442000
              IF CONTROL = "NEW PR" THEN                                 1443000
         BEGIN                                                           1444000
FORMAT        AN("*****",X43,"***** NEW PROBLEM. *****",X43,"*****");    1445000
              READ(CARD,ALPH,CONTROL);                                   1446000
              WRITE(PRINTER[PAGE]);                                      1447000
              WRITE(PRINTER,AN);                                         1448000
              GO TO SS;                                                  1449000
         END;                                                            1450000
              WRITE(PRINTER[DBL]);                                       1451000
              IF CONTROL = "CONCLU" THEN                                 1452000
              WRITE(PRINTER,FO10)                                        1453000
              ELSE                                                       1454000
         BEGIN                                                           1455000
              READ(CARD,AL,FOR I ~ 0 STEP 1 UNTIL 11 DO HEAD[I]);        1456000
              WRITE(PRINTER,FOE,FOR I ~ 0 STEP 1 UNTIL 11 DO HEAD[I]);   1457000
         END    ;                                                        1458000
              GO TO STOP;                                                1459000
ENDPROGRAM:   ;                                                          1460000
STOP:    END.                                                            1461000
