 LABEL  0000000000XXXXXX0010000001
         BEGIN
COMMENT       DETERMINATION OF FLOW AND PRESSURE DISTRIBUTIONS FOR
              FLUID FLOW IN A PIPELINE NETWORK.


              YUI LOONG WANG.
              PROFESSIONAL SERVICES GROUP, BURROUGHS CORPORATION.


              CARD SEQUENCE STARTS WITH NET-0001.
              FIRST RELEASE 9-1-63 ;


INTEGER       NOCASE, B, NODE, NOFEED, NOSINK, NOP, NONP, STATE, UMAX,
              DT, DBBN, NOLP, DPC, NNN, NPATH, LUMAX ;
BOOLEAN       BLOOP, BPO, BFLOW, OPT1, BERR, BINPUT ;
REAL          TEMP, MW, DENR, VIS, Z0, Z1, Z2, TOLP ;
REAL          T1, T2 ;
ALPHA         CASE, SYSTEM, AA ;
FILE IN       FILI(1,10) ;
FILE OUT      FILO 1(1,15) ;
LIST          IN1(CASE, B, NODE, NOFEED, NOSINK, NOP, NONP, SYSTEM),
              IN2(STATE, TEMP, MW, DENR, VIS, Z0, Z1, Z2),
              IN3(UMAX, DT, DBBN, DPC, TOLP),
              IN4(BLOOP,BPO, BFLOW, OPT1, BINPUT),
              OUTR1(B, NODE, NOFEED, NOSINK, NOP, NONP, SYSTEM, AA,
                     TEMP, MW, DENR, VIS, Z2, Z1, Z0, TOLP),
              OUTIN1(UMAX, DT, DBBN, DPC, BLOOP, BPO, BFLOW, OPT1),
              TITLE(CASE) ;
FORMAT        FMI1(A6, 6I6, A6),       FMI2(I2, 2F7.2, 5E11.4),
              FMI3(4I4, F8.4),         FMI4(5L5),
              FMO(//X30,"PIPELINE NETWORK ANALYSIS",
                   /////X20,"PROBLEM",A36),
              F1(///X20, "TOTAL RUNNING TIME FOR CASE ", A6,
                 " IS ", F6.2, " SECONDS"),
              FMOINA( ///X32, "PRINTOUT OF INPUT DATA"),
              FMOR1( ///X20,"BRANCHES", I35, //X20,"NODES", I38, //X20,
                    "FEEDS", I38, //X20, "SINKS", I38, //X20,
                    "PRESSURES SPECIFIED", I24, //X20,
                    "NON-PIPE MEMBERS", I27, //X20, "SYSTEM", A37,
                    //X20,"STATE", A38,
                    //X20,"TEMPERATURE", F26.1, " DEG F",
                    //X20,"MOLECULAR WEIGHT", F27.2,
                    //X20, "DENSITY", E27.4, " LB/CU FT",
                    //X20,"VISCOSITY", E31.4, " CP",
                    //X20,"COMPRESSIBILITY FACTOR",
              /X24, "Z2 = ", E11.4, /X24, "Z1 = ", E11.4,
              /X24, "Z0 = ", E11.4,
                    //X20, "PRESSURE  DROP TOLERANCE", F9.3,
                     " PSI/BRANCH"),
              FMOIN1( //X20,"UMAX  = ", I5, X15, "DT   = ", I5,
                      //X20,"DBBN  = ", I5, X15, "DPC  = ", I5,
                      //X20,"BLOOP = ", L5, X15, "BPO  = ", L5,
                      //X20,"BFLOW = ", L5, X15, "OPT1 = ", L5) ;
LABEL         START, FINI ;



START:        T1 ~TIME(1) ;
              READ(FILI, FMI1, IN1)[FINI] ;
              READ(FILI,FMI2,IN2) ;    READ(FILI,FMI3,IN3) ;
              READ(FILI,FMI4,IN4) ;
              NOLP ~ B - NODE + NOP ;


         BEGIN
INTEGER       I, J, K, N, N1, LP, N2, N3, N4, N5, N6, N7, N8, N9, U ;
INTEGER ARRAY SN, EN, A[0:B], NF[0:NOFEED], NS[0:NOSINK], NP[0:NOP],
              BNP, DIR[0:NONP], T[0:DT], TN1, TN2[0:30],
              NBL[0:IF NOLP > 30 THEN NOLP ELSE 30],
              NOBN[0:NODE], BBN[0:NODE, 0:DBBN], NPN1[0:NONP] ;
REAL ARRAY    L[0:B], D[0:B], ED[0:B], H[0:NODE], FEED[0:NOFEED],
              SINK[0:NOSINK], P[0:NOP], POL[0:NONP], POLC[0:NONP,0:DPC],
              DP[0:NOP-1], FLOW[0:B], PN[0:NODE], COP[0:B], PN1[0:NONP];
ALPHA ARRAY   NAME[0:NONP] ;
REAL          TRS1, TRS2, TRS3, TRS4, TRS5, TRS6, TRS7, TRS8, TRS9,
              TRS10, TRS11, TRS12, TRS13 ;


         BEGIN
COMMENT       THIS SECTION DETERMINES LOOPS AND PATHS, ESTIMATES FLOW
              PATTERNS, AND PERFORMS OTHER PRELIMINARY CHORES. ;

LABEL         PAT ;
LABEL         ER1 ;
INTEGER       DSR, MAXRANK ;
INTEGER ARRAY RANK[0:NODE] ;
ARRAY         F[0:30] ;
LIST          IN5(FOR J ~ 1 STEP 1 UNTIL B DO [ SN[J], EN[J], L[J],
                  D[J], ED[J], A[J]]),
              IN5A(FOR J ~ 1 STEP 1 UNTIL B DO [SN[J], EN[J], L[J],
                    D[J], ED[J], A[J], FLOW[J]]),
              IN6(FOR I ~ 1 STEP 1 UNTIL NODE DO H[I]),
              IN7(FOR K ~ 1 STEP 1 UNTIL NOFEED DO [NF[K], FEED[K]]),
              IN8(FOR K ~ 1 STEP 1 UNTIL NOSINK DO [NS[K], SINK[K]]),
              IN9(FOR K ~ 1 STEP 1 UNTIL NOP DO [NP[K], P[K]]),
              IN10( FOR K ~ 1 STEP 1 UNTIL NONP DO [ NAME[K], BNP[K],
                    DIR[K], POL[K]]),
              IN11( FOR N ~ 0 STEP 1 UNTIL POL[K] DO POLC[K,N]),
              ERRO(K),
              IN12(FOR K ~ 1 STEP 1 UNTIL NOLP DO NBL[K]),
              IN13(FOR J ~ 1 STEP 1 UNTIL N DO T[N1+J]),
              IN14(FOR  K ~ N2 STEP 1 UNTIL NOLP DO NBL[K]) ;
FORMAT        FMI5(2I5, F7.2, F8.4, F8.5, I2),
              FMI5A(2I5, F7.2, F8.4, F8.5, I2, F16.4),
              FMI6(8F7.2),             FMI789(4(I5, F9.2)),
              FMI10(A10, 3I5),         FMI11(5E16.8),
              FMI12(16I5),
              FMEA(/X20,"NUMBER OF BRANCHES AT NODE", I4,
                        "IS GREATER THEN DBBN"),
              FMEB(/X20,"TOTAL INPUT = ", F10.2,
                        "TOTAL OUTPUT = ", F10.2),
              FMEC(/X20,"FOR BRANCH", I4, ", EN > SN"),
              FMOINB(//X40, "BRANCH DESCRIPTIONS", ///X11, "BRANCH  ",
                     "CONNECTING NODES   DIAMETER    LENGTH    ",
                     "ROUGHNESS    A     FLOW", /X22, "SN", X6, "EN",
                     X7, "INCHES", X6, "FEET", X8, "E/D",A17/),
              FMOIN2(I15, I9, I8, F13.2, F11.1, F13.5, I5, F12.2),
              FMOINC(//X40, "NODAL SPECIFICATIONS", //X26, "NODE", X6,
                     "HEIGHT   SOURCE     SINK   PRESSURE",
                     /X37, "FEET", 2A10, X5, "PSI"/),
              FMOIN3(I29, F12.1, 3F10.2),
              FMOIND(//X40,"NON-PIPE MEMBERS",//X11,"NAME    BRANCH",
                     "     DIRECTION     POLYNOMIAL COEFFICIENTS"//),
              FMOIN4(A16, I6, I12, X6, 5F12.6),
              FMEE(/X20,"NUMBER OF BRANCHES WITH A[J] = 3 IS ",
                   I4, ", AND UNEQUAL TO NONP") ;


PROCEDURE     LOOPANDPATH(CASE, B, NODE, SN, EN, BLOOP, NPATH, TN1,
                   TN2, A, UMAX, LUMAX, DBN, DT, BPO, NBN, BN, LP,
                   NBL, T, BERR) ;


COMMENT       THIS PROCEDURE FORMS ALL THE INDEPENDENT LOOPS AND/OR
              PATHS BETWEEN GIVEN PAIRS OF NODES FOR A NETWORK.


              YUI LOONG WANG,
              PROFESSIONAL SERVICES GROUP, BORROUGHS CORP.


              CARD SEQUENCE STARTS WITH LOPA-0001,
              FIRST RELEASE 9-1-64.


VALUE         CASE, B, NODE, BLOOP, NPATH, UMAX, LUMAX, DBN, DT, BPO ;
INTEGER       B, NODE, NPATH, UMAX, LUMAX, DBN, DT, LP ;
BOOLEAN       BLOOP, BPO, BERR ;
ALPHA         CASE ;
INTEGER ARRAY SN, EN, TN1, TN2, A, NBN, NBL, T[0], BN[0,0] ;


         BEGIN
INTEGER       I, J, K, L, N, NN, N1, N2, N3, N4, N5, N6, N7, N8,
              NS, NE, U ;
BOOLEAN       TBS1, TBS2 ;
LABEL         PO, FINI ;
FORMAT OUT    FM1(///X20,"LOOPANDPATH",X5,"PROBLEM ",A6),
              FM2(//X15,"NUMBER OF BRANCHES = ", I20,
                  //X15,"NUMBER OF NODES = ", I23,
                  //X15,"NUMBER OF INDEPENDENT LOOPS FORMED = ", I4,
                  //X15,"NUMBER OF PATHS FORMED = ", I16),
              FM3(//X18, A4, I4, " (",I3," BRANCHES) NUMBER  BRANCH"/),
              FM4(I47, I7),
              ERR(///X10,"AN ERROR HAS BEEN FOUND: "),
              ERR2(/X15,"AFTER ",I3,"LOOPS, U EXCEEDS LUMAX"),
              ERR2A(/X15,"AFTER ",I3, "PATHS, U EXCEEDS UMAX"),
              ERR3(/X15,"AFTER ",I3,"LOOPS(PATHS), THE DIMENSION OF",
                   " ARRAY T HAS TO BE INCREASED"),
              ERR4(/X15,"BRANCH", I4, " ARE NOT USED IN",
                   " FORMING LOOPS") ;



              LP ~ B - NODE + 1 ;      BERR ~ FALSE ;


         BEGIN
COMMENT       IN THIS SECTION, ALL THE LOOPS AND/OR PATHS ARE FORMED. ;

LABEL         LF1, LF2, LF3, LF4, LF5, LF6, LF7, LF8, STOR ;
LABEL         PA ;
INTEGER       PB ;
INTEGER ARRAY PP[0:UMAX], P, PN[0:UMAX, 0:DBN] ;


              L ~ N ~ N1 ~ N2 ~ J ~ 0 ;     U ~ 1 ;
              IF BLOOP THEN
         BEGIN
              TBS2 ~ TRUE ;            GO TO LF1
         END ;

COMMENT       DETERMINE PATHES. ;

LF2:          N2 ~ N2 + 1 ;            IF N2 > NPATH THEN GO TO PO ;
              PB ~ J ~ 0 ;     NS ~ TN1[N2] ;     NE ~ TN2[N2] ;
              N3 ~ RANK[NE] ;          U ~ ABS(RANK[NS] - N3) ;
              IF U = 0 THEN U ~ 1 ;    GO TO PA ;
LF4:          U ~ U + 1 ;              NS ~ TN1[N2] ;
              IF U > UMAX THEN
         BEGIN
              WRITE(FILO, ERR) ;       WRITE(FILO, ERR2A, N2) ;
              BERR ~ TRUE ;            GO TO FINI
         END ;
              PB ~ 0 ;                 GO TO PA ;

COMMENT       DETERMINE LOOPS. ;

LF1:          N1 ~ N1 + 1 ;
              IF N1 > LP THEN
         BEGIN
              TBS2 ~ FALSE ;           GO TO LF2
         END ;
LF5:          J ~ J + 1 ;
              IF J > B THEN
         BEGIN
              U ~ U + 1 ;              J ~ 1 ;
              IF U > LUMAX THEN
         BEGIN
              WRITE(FILO, ERR) ;       WRITE(FILO, ERR2, N1) ;
              BERR ~ TRUE ;            GO TO FINI
         END ;
              FOR K ~ 1 STEP 1 UNTIL B DO IF A[K] = 0 THEN GO TO LF6 ;
              FOR K ~ 1 STEP 1 UNTIL B DO
              A[K] ~ IF A[K] = 1 THEN 0 ELSE 3 ;
              TBS1 ~ TRUE ;            U ~ 3 ;
         END ;
LF6:          IF A[J] ! 0 THEN GO TO LF5 ;
              NS ~ EN[J] ;             NE ~ SN[J] ;
              N3 ~ RANK[NE] ;          PB ~ J ;

COMMENT       FORM A PATH BETWEEN NODES NS AND NE WITH U BRANCHES. ;

PA:           K ~ 0 ;

COMMENT       SEARCH AND STORE ALL POSSIBLE K-TH BRANCH. ;

LF3:          NN ~ 0 ;     K ~ K + 1 ;     N4 ~ NBN[NS] ;
              FOR I ~ N4 STEP -1 UNTIL 1 DO
         BEGIN
              N5 ~ BN[NS,I] ;          N6 ~ ABS(N5) ;
              IF N6 ! ABS(PB) THEN IF N6 ! J THEN
              IF NOT TBS2 OR A[N6] ! 3 THEN
         BEGIN
              IF N5 > 0 THEN N7 ~ EN[N6] ELSE N7 ~ SN[N6] ;
              N8 ~ ABS(RANK[N7] - N3) ;
              IF N8 { (U-K) THEN
         BEGIN
              NN ~ NN + 1 ;     P[K,NN] ~ N5 ;     PN[K,NN] ~ N7
         END
         END
         END ;

COMMENT       EITHER SEARCH FOR THE (K+1)TH BRANCH OR FOR OTHER
              K-TH BRANCH. ;

              IF NN ! 0 THEN GO TO LF7 ;
LF8:          K ~ K - 1 ;
              IF K = 0 THEN
              IF TBS2 THEN GO TO LF5  ELSE GO TO LF4 ;
              PP[K] ~ I ~ PP[K] - 1 ;  IF I = 0 THEN GO TO LF8 ;
              PB ~ P[K,I] ;     NS ~ PN[K,I] ;     GO TO LF3 ;
LF7:          PP[K] ~ NN ;

COMMENT       IF K = U, CHECK WHETHER A LOOP OR PATH IS FORMED,
              OTHERWISE SEARCH FOR THE (K+1)TH BRANCH. ;

              IF K = U THEN
         BEGIN
              FOR I ~ NN STEP -1 UNTIL 1 DO
              IF PN[K,I] = NE THEN GO TO STOR ;
              GO TO LF8
         END
              ELSE
         BEGIN
              PB ~ P[K,NN] ;     NS ~ PN[K,NN] ;     GO TO LF3
         END ;


COMMENT       STORE THE LOOP OR PATH FORMED. ;

STOR:         L ~ L + 1 ;     PP[K] ~ I ;     N4 ~ N + 1 ;
              N ~ N + U ;              IF TBS2 THEN N ~ N + 1 ;
              NBL[L] ~ IF TBS2 THEN U + 1 ELSE U ;
              IF N > DT THEN
         BEGIN
              WRITE(FILO, ERR) ;       WRITE(FILO, ERR3, L) ;
              BERR ~ TRUE ;            GO TO FINI
         END ;
              IF TBS2 THEN
         BEGIN
              T[N4] ~ J ;     A[J] ~ 2 ;     N4 ~ N4 + 1 ;
         END ;
              FOR I ~ N4 STEP 1 UNTIL N DO
         BEGIN
              N5 ~ I + 1 - N4 ;        N5 ~ P[N5, PP[N5]] ;
              T[I] ~ N5 ;              N5 ~ ABS(N5) ;
              IF TBS2 THEN IF A[N5] = 0 THEN A[N5] ~ 1
         END ;
              IF TBS2 THEN GO TO LF1 ELSE GO TO LF2 ;
         END ;


COMMENT       IF BPO, PRINT OUT THE LOOPS OR PATHS. ;

PO:      BEGIN
              IF NOT BLOOP THEN LP ~ 0 ;
              IF BPO THEN
         BEGIN
              WRITE(FILO[PAGE]) ;      WRITE(FILO, FM1, CASE) ;
              WRITE(FILO, FM2, B, NODE, LP, NPATH) ;
              N ~ 0 ;
              FOR I ~ 1 STEP 1 UNTIL LP DO
         BEGIN
              U ~ NBL[I] ;             WRITE(FILO, FM3, "LOOP", I, U) ;
              FOR K ~ 1 STEP 1 UNTIL U DO WRITE(FILO, FM4, K, T[N+K]) ;
              N ~ N + U
         END ;
              FOR I ~ 1 STEP 1 UNTIL NPATH DO
         BEGIN
              U ~ NBL[I+LP] ;          WRITE(FILO, FM3, "PATH", I, U) ;
              FOR K ~ 1 STEP 1 UNTIL U DO WRITE(FILO, FM4, K, T[N+K]) ;
              N ~ N + U
         END
         END ;


COMMENT       CHECK TO SEE IF ALL AVAILABLE BRANCHES ARE USED IN
              FORMING LOOPS. ;

              IF BLOOP THEN IF NOT TBS1 THEN
              FOR J ~ 1 STEP 1 UNTIL B DO
              IF A[J] = 0 THEN
         BEGIN
              WRITE(FILO, ERR) ;       WRITE(FILO, ERR4, J) ;
              BERR ~ TRUE ;            GO TO FINI
         END
         END PO ;

FINI:    END LOOPANDPATH ;



              IF BINPUT THEN
         BEGIN
              WRITE(FILO[PAGE]) ;      WRITE(FILO, FMO, TITLE) ;
              IF STATE = 1 THEN AA ~ "  GAS " ELSE AA ~ "LIQUID" ;
              WRITE(FILO, FMOINA) ;    WRITE(FILO, FMOR1, OUTR1) ;
              WRITE(FILO, FMOIN1, OUTIN1) ;
              IF STATE = 1 THEN AA ~ "MCF/HR" ELSE AA ~ "  GPM " ;
              WRITE(FILO[PAGE]) ;      WRITE(FILO, FMOINB, AA) ;
         END ;
              IF BFLOW THEN READ(FILI,FMI5A,IN5A)
              ELSE READ(FILI,FMI5,IN5) ;
              READ(FILI,FMI6,IN6) ;    READ(FILI,FMI789,IN7) ;
              READ(FILI,FMI789,IN8) ;  READ(FILI,FMI789,IN9) ;
              READ(FILI,FMI10,IN10) ;
              IF NONP ! 0 THEN READ(FILI, FMI11, IN11) ;
              IF BINPUT THEN
         BEGIN
              FOR J ~ 1 STEP 1 UNTIL B DO
              IF BFLOW THEN
              WRITE(FILO, FMOIN2, J, SN[J],EN[J],L[J],D[J],ED[J],A[J],
                        FLOW[J])
              ELSE
              WRITE(FILO, FMOIN2, J,SN[J],EN[J],L[J],D[J],ED[J],A[J]) ;
              WRITE(FILO[PAGE]) ;      WRITE(FILO, FMOINC, AA, AA) ;
              FOR I ~ 1 STEP 1 UNTIL NODE DO
         BEGIN
              TRS1 ~ TRS2 ~ TRS3 ~ 0.0 ;
              FOR K ~ 1 STEP 1 UNTIL NOFEED DO
              IF I = NF[K] THEN TRS1 ~ FEED[K] ;
              FOR K ~ 1 STEP 1 UNTIL NOSINK DO
              IF I = NS[K] THEN TRS2 ~ SINK[K] ;
              FOR K ~ 1 STEP 1 UNTIL NOP DO
              IF I = NP[K] THEN TRS3 ~ P[K] ;
              IF TRS3 = 0.0 THEN WRITE(FILO,FMOIN3, I,H[I],TRS1,TRS2)
              ELSE WRITE(FILO,FMOIN3, I,H[I],TRS1,TRS2,TRS3)
         END ;
              IF NONP ! 0 THEN
         BEGIN
              WRITE(FILO[PAGE]) ;      WRITE(FILO, FMOIND) ;
         END ;
              FOR K ~ 1 STEP 1 UNTIL NONP DO
              WRITE(FILO, FMOIN4, NAME[K], BNP[K], DIR[K],
                     FOR N ~ 1 STEP 1 UNTIL POL[K] DO POLC[K,N]) ;
         END ;


COMMENT       FIRST DETERMINE THE NUMBER OF BRANCHES CONNECTED TO EACH
              NODE. STORE THAT IN NOBN[I] FOR NODE I, AND THE BRANCH
              NUMBERS IN BBN[I,]. ;

              FOR I ~ 1 STEP 1 UNTIL NODE DO  NOBN[I] ~ 0 ;
              FOR J ~ 1 STEP 1 UNTIL B  DO
         BEGIN
              I ~ SN[J] ;     NOBN[I] ~ N1 ~ NOBN[I] + 1 ;
              K ~ EN[J] ;     NOBN[K] ~ N2 ~ NOBN[K] + 1 ;
              IF (N1 > DBBN) OR (N2 > DBBN) THEN
         BEGIN
              IF N1 > DBBN THEN K ~ I ;
              WRITE(FILO,FMEA, ERRO) ;      GO TO START
         END ;
              BBN[I,N1] ~ J ;          BBN[K,N2] ~ -J
         END ;


COMMENT       CHECK POSSIBLE ERRORS IN INPUT DATA. ;

COMMENT       CHECK TOTAL INPUT AND OUTPUT. ;

              TRS1 ~ TRS2 ~ 0.0 ;
              FOR K ~ 1 STEP 1 UNTIL NOFEED DO  TRS1 ~ FEED[K] + TRS1 ;
              FOR K ~ 1 STEP 1 UNTIL NOSINK DO  TRS2 ~ SINK[K] + TRS2 ;
              IF ABS((TRS1 - TRS2) / TRS1) > 1.0@-6 THEN
         BEGIN
              WRITE(FILO, FMEB, TRS1, TRS2) ;    GO TO START
         END ;

COMMENT       CHECK SN AND EN, AND NON-PIPE ELEMENTS. ;

              FOR K ~ 1 STEP 1 UNTIL B DO
              IF EN[K] < SN[K] THEN
         BEGIN
              WRITE(FILO, FMEC, ERRO) ;     GO TO START
         END ;

COMMENT       DETERMINE THE RANK OF EACH NODES. THE FIRST NODE IS
              ARBITRARILY ASSIGNED A RANK OF 1. ;
              DSR ~ 40 ;
ER1:          DSR ~ DSR + 20 ;

         BEGIN
INTEGER ARRAY SR, SR1[0:DSR] ;
LABEL         R1 ;
              RANK[1] ~ SR[1] ~ N2 ~ N3 ~ MAXRANK ~ 1 ;
R1:           N1 ~ N2 ;     N2 ~ 0 ;     MAXRANK ~ MAXRANK + 1 ;
              FOR J ~ 1 STEP 1 UNTIL N1 DO SR1[J] ~ SR[J] ;
              FOR J ~ 1 STEP 1 UNTIL N1 DO
         BEGIN
              N4 ~ SR1[J] ;            N5 ~ NOBN[N4] ;
              FOR K ~ 1 STEP 1 UNTIL N5 DO
         BEGIN
              N6 ~ BBN[N4,K] ;
              IF N6 > 0 THEN N7 ~ EN[N6] ELSE N7 ~ SN[-N6] ;
              IF RANK[N7] = 0 THEN
         BEGIN
              RANK[N7] ~ MAXRANK ;     N2 ~ N2 + 1 ;
              IF N2 > DSR THEN GO TO ER1 ;
              SR[N2] ~ N7 ;
         END
         END
         END ;
              N3 ~ N3 + N2 ;     IF N3 < NODE THEN GO TO R1 ;
         END ;


COMMENT       INITIAL ESTIMATE OF FLOW DISTRIBUTION. ;

              IF NOT BFLOW THEN
         BEGIN
              U ~ K ~ 1 ;              N ~ 0 ;
PAT:          N ~ N + 1 ;              TN1[N] ~ 1 ;
              IF U { NOFEED THEN
         BEGIN
              IF NF[U] = 1 THEN
         BEGIN
              U ~ U + 1 ;     N ~ N - 1 ;     GO TO PAT
         END ;
              TN2[N] ~ NF[U] ;     F[N] ~ -FEED[U] ;     U ~ U + 1
         END
              ELSE IF K { NOSINK THEN
         BEGIN
              IF NS[K] = 1 THEN
         BEGIN
              K ~ K + 1 ;     N ~ N - 1 ;     GO TO PAT
         END ;
              IF NS[K] = 1 THEN K ~ K + 1 ;
              TN2[N] ~ NS[K] ;     F[N] ~ SINK[K] ;     K ~ K + 1
         END ;
              IF (N = 30) OR (K > NOSINK) THEN
         BEGIN
              LOOPANDPATH(CASE, B, NODE, SN, EN, FALSE, N, TN1,
                   TN2, A, UMAX, LUMAX, DBBN, DT, FALSE, NOBN, BBN,
                    LP, NBL, T, BERR) ;
              IF BERR THEN GO TO START ;
         END
              ELSE GO TO PAT ;
              N1 ~ 0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              N3 ~ N1 + 1 ;            N1 ~ NBL[I] + N1 ;
              FOR J ~ N3 STEP 1 UNTIL N1 DO
         BEGIN
              N2 ~ T[J] ;              N4 ~ ABS(N2) ;
              FLOW[N4] ~ SIGN(N2) | F[I] + FLOW[N4] ;
         END ;
         END ;
              IF K { NOSINK THEN
         BEGIN
              N ~ 0 ;                  GO TO PAT ;
         END ;
         END ;


COMMENT       EITHER READ IN LOOP DATA OR USE PROCEDURE LOOPFINDER. ;

              IF BLOOP THEN
         BEGIN
              N1 ~ 0 ;                 READ(FILI, FMI12, IN12) ;
              FOR K ~ 1 STEP 1 UNTIL NOLP DO
         BEGIN
              N ~ NBL[K] ;     READ(FILI,FMI12,IN13) ;     N1 ~ N1 + N
         END
         END
              ELSE
         BEGIN
              LUMAX ~ UMAX ;           NPATH ~ NOP - 1 ;
              FOR I ~ 2 STEP 1 UNTIL NOP DO
         BEGIN
              TN1[I-1] ~ NP[I] ;       TN2[I-1] ~ NP[1] ;
         END ;
              LOOPANDPATH(CASE, B, NODE, SN, EN,  TRUE, NPATH, TN1,
                    TN2, A, UMAX, LUMAX, DBBN, DT, BPO, NOBN, BBN,
                    LP, NBL, T, BERR) ;
              IF BERR THEN GO TO START ;
         END ;
              N2 ~ 0 ;
              FOR I ~ 1 STEP 1 UNTIL LP DO
         BEGIN
              N3 ~ N2 + 1 ;     N2 ~ NBL[I] + N2 ;     TRS2 ~ 0 ;
              U ~ 0 ;
              FOR J ~ N3 STEP 1 UNTIL N2 DO
         BEGIN
              K ~ ABS(T[J]) ;          TRS1 ~ ABS(FLOW[K]) ;
              IF TRS1 = 0.0 THEN U ~ 1 ;
              IF TRS2 < TRS1 THEN TRS2 ~ TRS1 ;
         END ;
              IF U ! 0 THEN
         BEGIN
              IF TRS2 = 0.0 THEN TRS2 ~ 0.5 | SINK[1] ;
              TRS2 ~ 0.2143 | TRS2 ;
              FOR J ~ N3 STEP 1 UNTIL N2 DO
         BEGIN
              K ~ ABS(T[J]) ;     FLOW[K] ~ SIGN(T[J]) | TRS2 + FLOW[K];
         END
         END ;
         END ;
              FOR J ~ 1 STEP 1 UNTIL B DO A[J] ~ 0 ;
              FOR J ~ 1 STEP 1 UNTIL NONP DO A[BNP[J]] ~ 3 ;
         END PRELIMINARY ;



COMMENT       THE FLOWS ARE NOW ADJUSTED TO SATISFY THE PRESSURE
              BALANCE. ;

COMMENT       THIS SECTION PERFORMS ITERATIVE CALCULATIONS BASED ON
              THE HARDY CROSS METHOD. ;

         BEGIN
INTEGER       ITER  ;
REAL          COP1, COP2, RE1, RE2, F, DQ, SUMP, SUMDP, PAVG, Z, RENO,
              G, DG ;
REAL ARRAY    C[0:B] ;
LIST          OUTIM1(ITER, N2, TRS4, TRS5, N4),
              OUTR2(J, N1, N2, FLOW[J], TRS1, COP[J], AA),
              OUTR3(I, PN[I], TRS1, TRS2),
              OUTR3A(I, PN[I], TRS1),
              OUTR3B(I, PN[I], TRS2) ;
FORMAT OUT    FMOIMA(//X36,"INTERMEDIATE   PRINTOUT", ///X15,
                     "ITERATION  LOOPS ADJUSTED  TOTAL ERROR   ",
                     "MAXIMUM ERROR   AT LOOP", /X46,"PSI",X11,"PSI"/),
              FMOIM1(/I20, I14, X5, 2E14.5, I9),
              FMORA(////X45,"FLOW DISTRIBUTION", ///X11, "BRANCH",
                    X3, "CONNECTING NODES", X7,"FLOW", X6,
                     "PRESSURE DROP   FRICTION COEFF    COMMENT", /X22,
                     "SN", X6, "EN", A15, X13, "PSI"//),
              FMOR2(I14, I10, I8, F17.3, F15.3, E19.6, A11),
              FMORB(////X40,"NODAL CONDITIONS",////X27, "NODE", X4,
                    "PRESSURE", X6, "SOURCE", X8, "SINK", X8,
                   //X33, "PSIG", 2A13///),
              FMOR3(I30, 3F13.3),
              FMOR3A(I30, F13.3, F26.3) ;
LABEL         REITERATE, RE ;

COMMENT       INITIAL SETUP. ;

              FOR I ~ 1 STEP 1 UNTIL NODE DO  PN[I] ~ P[1] ;
              FOR I ~ 2 STEP 1 UNTIL NOP DO
              DP[I-1] ~ H[NP[1]] - H[NP[I]] ;
              FOR I ~ 1 STEP 1 UNTIL NOP-1 DO
              DP[I] ~ DP[I] | DENR / 144.0 + P[1] - P[I+1] ;
              IF STATE = 1 THEN
         BEGIN
              RE1 ~ 6326.33 | DENR / VIS ;
              COP1 ~ (TEMP + 460.0) | DENR | DENR | 575.23455 / MW ;
         END
              ELSE IF STATE = 2 THEN
         BEGIN
              RE1 ~ 50.759 | DENR / VIS ;
              COP1 ~ 0.0017274 | DENR ;
         END ;
              FOR J ~ 1 STEP 1 UNTIL B DO C[J] ~ L[J] | COP1 / D[J]*5 ;
              ITER ~ 0 ;               Z ~ 1 ;


COMMENT       START ITERATION HERE. ;

REITERATE:    ITER ~ ITER + 1 ;
              IF ITER > 100 THEN GO TO START ;

COMMENT       CALCULATE THE PRESSURE DROP COEFFICIENTS FOR PIPES. ;
              FOR N5 ~ 1 STEP 1 UNTIL B DO IF A[N5] ! 3 THEN
         BEGIN
COMMENT       THIS SECTION CALCULATES THE FRICTION FACTOR F FOR
              BRANCH J FROM THE GIVEN REYNOLDS NUMBER-RENO. THE
              RELATIONS GIVEN BY COULSON AND RICHARDSON IN P.30 OF
              "CHEMICAL ENGINEERING", VOL I, IS USED. ;
              IF STATE = 1 THEN
         BEGIN
              PAVG ~ PN[SN[N5]] + PN[EN[N5]] ;
              Z ~ (Z2 | PAVG + Z1 + Z0 / PAVG)| 0.5 ;
         END ;
              RENO ~ ABS(FLOW[N5]) | RE1 | Z / D[N5] ;
              IF RENO = 0.0 THEN RENO ~ 50.0 ;

              IF RENO < 2000.0 THEN   F ~ 8.0 / RENO
              ELSE
         BEGIN
              G ~ 1.0 ;
              TRS1 ~ 14.3 ;      TRS2 ~ ED[N5] ;
              IF TRS2 ! 0.0 THEN
         BEGIN
              IF RENO > 1.0@7 THEN TRS1 ~ -2.5 | LN(TRS2) + 3.2
              ELSE FOR N6 ~ 1 STEP 1 WHILE ABS(G) > 1.0@-5 DO
         BEGIN
              TRS3 ~ TRS1 / (1.13 | RENO) + 0.27027 | TRS2 ;
              G ~ 2.5 | LN(TRS3) ;
              DG ~ (1.0 - 2.5 / TRS3) / (1.13 | RENO) ;
              TRS1 ~ G / DG + TRS1 ;
         END
         END
              ELSE FOR N6 ~ 1 STEP 1 WHILE ABS(G) > 1.0@-5 DO
         BEGIN
              G ~ 0.3 + 2.5 | LN(RENO / TRS1) - TRS1 ;
              DG ~ 2.5 / TRS1 + 1.0 ;  TRS1 ~ G / DG + TRS1 ;
         END ;
              F ~ 1.0 / TRS1*2 ;
         END ;
              COP[N5] ~ C[N5] | F | Z ;
         END COEFF ;


COMMENT       CALCULATE THE PRESSURE DROP COEFFICIENTS FOR NON-PIPES. ;

              FOR K ~ 1 STEP 1 UNTIL NONP DO
         BEGIN
              TRS1 ~ 0.0 ;      J ~ BNP[K] ;    TRS2 ~ ABS(FLOW[J]) ;
              FOR N ~ POL[K] STEP -1 UNTIL 0 DO
              TRS1 ~ TRS1 | TRS2 + POLC[K,N] ;
              COP[J] ~ TRS1 / TRS2*2 ;
         END ;


COMMENT       LOOP  BALANCING BY THE HARDY - CROSS METHOD. ;

              N1 ~ N2 ~ N4 ~ 0 ;   TRS4 ~ TRS5 ~ 0.0 ;
              FOR K ~ 1 STEP 1 UNTIL NOLP DO
         BEGIN
              N3 ~ NBL[K] ;        SUMP ~ SUMDP ~ 0.0 ;
COMMENT       CALCULATE PRSSURE DROPS AND THE PARTIALS FOR BRANCHES. ;
              FOR N ~ 1 STEP 1 UNTIL N3 DO
         BEGIN
              J ~ T[N1+N] ;            N5 ~ ABS(J) ;
              TRS7 ~ FLOW[N5] ;    TRS8 ~ ABS(TRS7) ;
              IF A[N5] ! 3 THEN
         BEGIN
              TRS10 ~ COP[N5] | TRS8 ;
              SUMDP ~ SUMDP + TRS10 + TRS10 ;
              SUMP ~ SIGN(J) | TRS7 | TRS10 + SUMP ;
         END
              ELSE
         BEGIN
              FOR I ~ 1 STEP 1 WHILE BNP[I] ! N5 DO ;
              IF (DIR[I] = 0) OR (SIGN(TRS7) = SIGN(DIR[I])) THEN
         BEGIN
              TRS9 ~ TRS10 ~ 0.0 ;
              FOR N6 ~ POL[I] STEP -1 UNTIL 1 DO
         BEGIN
              TRS11 ~ POLC[I,N6] ;
              TRS9 ~ TRS9 | TRS8 + TRS11 ;
              TRS10 ~ TRS10 | TRS8 + N6 | TRS11
         END ;
              TRS9 ~ TRS9 | TRS8 + POLC[I,0] ;
              COP[N5] ~ TRS9 / TRS8*2 ;
              SUMP ~ SIGN(TRS7) | SIGN(J) | TRS9 + SUMP ;
              SUMDP ~ SUMDP + TRS10 ;
         END
              ELSE COP[N5] ~ 0.0 ;
         END ;
         END ;


COMMENT       CHECK PRESSURE BALANCES AND IF NECESSARY MAKE FLOW
              ADJUSTMENTS. ;

              IF K > LP THEN SUMP ~ -DP[K-LP] + SUMP ;
              TRS6 ~ ABS(SUMP) ;       TRS4 ~ TRS4 + TRS6 ;
              IF TRS6 > TRS5 THEN
         BEGIN
              TRS5 ~ TRS6 ;        N4 ~ K
         END ;
              IF TRS6 > (N3 | TOLP)  THEN
         BEGIN
              N2 ~ N2 + 1 ;            DQ ~ -SUMP / SUMDP ;
              FOR N ~ 1 STEP 1 UNTIL N3 DO
         BEGIN
              J ~ T[N1+N] ;            N5 ~ ABS(J) ;
              FLOW[N5] ~ SIGN(J) | DQ + FLOW[N5]
         END ;
              IF K > LP THEN
         BEGIN
              J ~ T[N1+1] ;            U ~ T[N1+N3] ;
              N6 ~ IF J < 0 THEN EN[-J] ELSE SN[J] ;
              N7 ~ IF U < 0 THEN SN[-U] ELSE EN[U] ;
              FOR K ~ 1 STEP 1 UNTIL NOFEED DO
              IF NF[K] = N6 THEN FEED[K] ~ FEED[K] + DQ
              ELSE IF NF[K] = N7 THEN FEED[K] ~ FEED[K] - DQ ;
              FOR K ~ 1 STEP 1 UNTIL NOSINK DO
              IF NS[K] = N6 THEN SINK[K] ~ SINK[K] - DQ
              ELSE IF NS[K] = N7 THEN SINK[K] ~ SINK[K] + DQ ;
         END ;
         END ;
              N1 ~ N1 + N3
         END ;


COMMENT       PRINT OUT INTERMEDIATE RESULTS IF REQUIRED. ;

              IF OPT1 THEN
         BEGIN
              IF ITER = 1 THEN
         BEGIN
              WRITE(FILO[PAGE]) ;      WRITE(FILO, FMO, TITLE) ;
              WRITE(FILO, FMOIMA) ;
         END ;
              WRITE(FILO, FMOIM1, OUTIM1) ;
         END ;


COMMENT       CHECKS RESULTS. ;

              IF (N2 > 0) AND (STATE = 2) THEN GO TO REITERATE ;
              NNN ~ N2 ;


COMMENT       THIS SECTION   CALCULATES NODAL PRESSURES FROM THE FLOW
              DISTRIBUTION. ;

              FOR I ~ 1 STEP 1 UNTIL NODE  DO PN[I] ~ 0.0 ;
              N1 ~ 1 ;                 PN[NP[1]] ~ P[1] ;
RE:           FOR I ~ 1 STEP 1 UNTIL NODE DO
         BEGIN
              N3 ~ NOBN[I] ;           TRS3 ~ PN[I] ;
              IF TRS3 ! 0.0 THEN
              FOR K ~ 1 STEP 1 UNTIL N3 DO
         BEGIN
              N4 ~ BBN[I,K] ;    N6 ~ ABS(N4) ;
              IF N4 < 0 THEN N5 ~ SN[-N4] ELSE N5 ~ EN[N4] ;
              IF PN[N5] = 0.0 THEN
         BEGIN
              TRS1 ~ ABS(FLOW[N6]) | FLOW[N6] ;
              TRS2 ~ SIGN(N4) | COP[N6] | TRS1 ;
              PN[N5] ~ (H[N5] - H[I]) | DENR / 144.0 + TRS3 - TRS2 ;
              N1 ~ N1 + 1 ;
         END
         END
         END ;
              IF N1 < NODE THEN GO TO RE ;
              IF NNN ! 0 THEN GO TO REITERATE ;


COMMENT       THIS SECTION PRINTS OUT FLOW AND PRESSURE DISTIBUTIONS.  ;

              WRITE(FILO[PAGE]) ;      WRITE(FILO, FMO, TITLE) ;
              IF STATE = 1 THEN AA~ "   GAS"  ELSE AA ~ "LIQUID" ;
              WRITE(FILO, FMOR1, OUTR1) ;
              IF STATE = 1 THEN AA ~ "MCF/HR" ELSE AA ~ "  GPM " ;
              WRITE(FILO[PAGE]) ;      WRITE(FILO, FMORA, AA) ;
              FOR J ~ 1 STEP 1 UNTIL B DO
         BEGIN
              N1 ~ SN[J] ;     N2 ~ EN[J] ;
              TRS1 ~ PN[N1] - PN[N2] ; AA ~ "      " ;
              IF A[J] = 3 THEN
         BEGIN
              FOR K ~ 1 STEP 1 WHILE BNP[K] ! J DO ;
              AA ~ NAME[K]
         END ;
              WRITE(FILO, FMOR2, OUTR2)
         END ;
              WRITE(FILO[PAGE]) ;      WRITE(FILO, FMORB, AA, AA) ;
              FOR I ~ 1 STEP 1 UNTIL NODE DO
         BEGIN
              TRS1 ~ TRS2 ~ 0.0 ;
              FOR K ~ 1 STEP 1 UNTIL NOFEED DO
              IF  I = NF[K] THEN  TRS1 ~ FEED[K] ;
              FOR K ~ 1 STEP 1 UNTIL NOSINK DO
              IF  I = NS[K] THEN  TRS2 ~ SINK[K] ;
              IF TRS1 ! 0.0 THEN
         BEGIN
              IF TRS2 ! 0.0 THEN WRITE(FILO, FMOR3, OUTR3)
              ELSE WRITE(FILO, FMOR3, OUTR3A)
         END
              ELSE
         BEGIN
              IF TRS2 ! 0.0 THEN WRITE(FILO, FMOR3A, OUTR3B)
              ELSE WRITE(FILO, FMOR3, I, PN[I]) ;
         END
         END ;
         END ITERATIVE CALCULATIONS ;


         END ;
              T2 ~ (TIME(1) - T1) / 60.0 ;
              WRITE(FILO, F1, CASE, T2) ;
              GO TO START ;
FINI:    END .
 LABEL  000000000FILI   0010000001
TEST 2    35    30     3    11     2     0 WATER                         2  IN1
 2  70.0   18.0  0.50  @ 02 0.8   @ 00 0.1   @ 01 0.0   @ 00 0.0   @ 00    2 IN2
  10 500   5   1  0.001                                                  2  IN3
FALSE TRUEFALSE TRUE TRUE
    1    2 500.0  24.0    0.0     3                                      2  IN5
    2    4 120.0  16.0    0.0
    2    5 120.0  16.0    0.0
    3    4  80.0  16.0    0.0
    5    6  80.0  16.0    0.0
    3    7  95.0   8.0    0.0
    3    8  70.0  12.0    0.0
    4    9  70.0  12.0    0.0
    5   11  70.0  12.0    0.0
    6   16 170.0  14.0    0.0
    7    8  40.0   8.0    0.0
    8    9  80.0   8.0    0.0
    9   10  60.0   8.0    0.0
   10   11  60.0   8.0    0.0
    7   13 100.0  14.0    0.0
    9   14 100.0  12.0    0.0
   11   15 100.0  12.0    0.0
   12   13 270.0  14.0    0.0     3
   15   16  80.0   8.0    0.0
   13   17 150.0  14.0    0.0
   14   17 150.0  12.0    0.0
   14   18 175.0  12.0    0.0
   16   19 120.0  12.0    0.0
   18   19 223.0  12.0    0.0
   17   20  90.0  14.0    0.00    3
   18   21  90.0  12.0    0.00    3
   19   22  90.0  12.0    0.00    3
   20   23 120.0  14.0    0.00    3
   21   24 120.0  12.0    0.00    3
   22   25 120.0  12.0    0.00    3
   23   28 150.0  14.0    0.00    3
   24   29 150.0  12.0    0.00    3
   25   30 150.0  12.0    0.00    3
   26   27  60.0  12.0    0.00    3
   27   28 230.0  12.0    0.00    3
   0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0                  2  IN6
   0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0                  2  IN6
   0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0                  2  IN6
   0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0                  2  IN6
    1 20000.0    12 10000.0    19  4000.0                                2  IN7
    3  5300.0     6  7300.0     9   700.0    10   200.0                  2  IN8
   15   830.0    20  1250.0    21   120.0    26  4600.0                  2  IN8
   28  4600.0    29  5200.0    30  3900.0                                2  IN8
    1   200.0    12   200.0                                              2  IN9

TEST 1    41    28     4    12     1       WATER                        NET1IN1
 2 100.0   18.0  0.624 @ 02 0.1   @ 01 0.1   @ 01 0.0   @ 00 0.0   @ 00    1 IN2
  201000   5   0  0.0001
FALSE TRUEFALSE TRUE TRUE
    1    2  10.0   1.0    0.00                                          NET1IN5
    1    5  10.0   1.0    0.0
    2    3  10.0   1.0    0.0
    3    4  10.0   2.0    0.0
    3    6  15.0   1.0    0.0
    4    8  10.0   2.0    0.0
    5    6  15.0   1.0    0.0
    5    9  10.0   1.0    0.0
    5   11  20.0   2.0    0.0
    6    7  15.0   1.0    0.0
    7    8  10.0   1.0    0.0
    7   11  10.0   2.0    0.0
    8   12  10.0   1.0    0.0
    9   10  15.0   1.0    0.0
    9   13  10.0   1.0    0.0
   10   11  15.0   2.0    0.0
   10   14  10.0   1.0    0.0
   11   12  10.0   2.0    0.0
   11   15  10.0   2.0    0.0
   12   16  10.0   1.0    0.0
   13   14  15.0   1.0    0.0
   13   17  10.0   1.0    0.0
   14   15  15.0   1.0    0.0
   14   19  10.0   2.0    0.0
   15   16  10.0   1.0    0.0
   15   19  10.0   2.0    0.0
   16   20  10.0   1.0    0.0
   16   21  10.0   1.0    0.0
   17   18   8.0   1.0    0.0
   17   25  40.0   1.0    0.0
   18   19   8.0   2.0    0.0
   19   20   8.0   2.0    0.0
   19   22  10.0   2.0    0.0
   20   21   8.0   1.0    0.0
   21   28  40.0   1.0    0.0
   22   23  10.0   1.0    0.0
   23   24  10.0   1.0    0.0
   24   27  10.0   2.0    0.0
   25   26   8.0   1.0    0.0
   26   27   8.0   2.0    0.0
   27   28  16.0   2.0    0.0
   0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0                 NET1IN6
   0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0
   0.0    0.0    0.0    0.0    0.0    0.0    0.0    0.0
   0.0    0.0    0.0    0.0
    4   100.0    11   200.0    19   200.0    27   100.0                 NET1 IN7
    2    20.0     5    50.0    10    20.0    16    50.0                 NET1 IN8
   17    20.0    25   100.0    28   100.0     1    40.0
   12    40.0    18    50.0    21    50.0     6    60.0
    1   100.0                                                           NET1 IN9

