         BEGIN
COMMENT       THIS IS A TEST PROGRAM FOR GENERAL DISTILLATION
              MARK N. LAYMAN, PROFESSIONAL SERVICES, 11/20/63 ;
INTEGER       M2, U, KEY, NDATA ;
INTEGER       N, M, NLF, NVF, NLD, NVD, NQ, OPTION, CONDENSER, ITER,I,J;
ALPHA         DESC1, DESC2, DESC3, SPEC1, SPEC2, DATE1, DATE2, PROB ;
BOOLEAN       RITIP, RITMID, PCONST ;
REAL          P, RA, RD, TOLT, TOLV, TOLX, Q1, QM, V1, T1, HV1, LM, TM,
              AKR, BKR, CKR, DKR,
              HLM, TRS1 ;
FILE          FLIN (1,10) ;
FILE OUT      FLOT 1(1,15) ;
FILE          TAPEIO(1, 400, 100) ;
FORMAT IN     FRIN1 (8A6,10I2,2L6/L7,2E14.5,2F6.3,3E11.3) ;
FORMAT IN     FRIN1A(2I5) ;
LABEL         START, THEEND ;
START :       READ (FLIN, FRIN1, PROB, DESC1, DESC2, DESC3, SPEC1,SPEC2,
              DATE1, DATE2, OPTION, CONDENSER, N, M, NLF, NVF, NLD, NVD,
              NQ, ITER, RITIP, RITMID, PCONST, Q1, QM, RD, RA, TOLT,
              TOLV, TOLX) [THEEND] ;
              READ(FLIN, FRIN1A, KEY, NDATA) ;
         BEGIN
INTEGER ARRAY F[0:M], LFP[0:NLF], VFP[0:NVF], LDP[0:NLD], VDP[0:NVD],
              QP[0:NQ], LFS[0:NLF], VFS[0:NVF] ;
ALPHA ARRAY   NAMEA, NAMEB[0:N] ;
ARRAY         TDATA, TINV[0:NDATA], HLC, HVC, KC[0:N, 0:NDATA] ;
ARRAY         AK, BK, CK, DK, EK, FK, AHL, BHL, CHL, AHV, BHV, CHV[0:N];
ARRAY         TC, PC, XM, Y1[0:N], LF, TLF, HLF[0:NLF], VF, TVF,
              HVF[0:NVF], LD, TLD, HLD[0:NLD], VD, TVD, HVD[0:NVD],
              Q[0:NQ], L, V, T, HL, PHLT, HV, PHVT, P1[0:7+1], PDC,
              HLC5, HVC1[0:N, 0:4], CK1[0:N, 0:28], XLF[0:NLF, 0:N],
              YVF[0:NVF, 0:N], XLD[0:NLD, 0:N], YVD[0:NVD, 0:N],
              X, K, PKT, PHLX, PHVY[0:M+1, 0:N] ;
LABEL         LLL, VVV ;
REAL          TRS2, TRS3 ;
FORMAT IN     FRIN2 (F40.2),                 FRIN3 (8F10.4),
              FRIN4(12A6),             FRIN5(3E16.8),
              FRIN6(5(I2,E14.6)),      FRIN7(I5,3E20.8,I2),
              FRIN8 (5E16.8) ;
FORMAT OUT    HEAD1 (X10,"PROBLEM ",A6/),
              HEAD2(X35,"DATA INPUT",//),
              FHOP1(X10,"OPTION",I3," - ",A6," AND ",A6,"SPECIFIED"/),
              FOUT1(X15,"CONSTANT COLUMN PRESSURE =",F8.2," PSIA"//),
              HEAD3(X15,"TRAY",X12,"T",X19,"V"/),
              HEAD3A(X15,"TRAY",X10,"P",X15,"T",X15,"V"/),
              FOUT2(X15,I3,2F20.6),
              FOUT2A(X15,I3,3F16.6),
              FOUT3(X15,"CONDENSER DUTY =",E15.6," BTU",/X15,
                    "REBOULER DUTY =",E16.6," BTU"//),
              HEAD4(X35,"FEEDS"//),
              HEAD5(X10,"LFP",X2,"TRAY",X5, "LF",X10,"TLF",X9, "HLF",
                    X5,"COMPONENT",X10,"XLF"/),
              FOUT4(X10,I2,I6,3E12.4),
              FOUT5(X55,2A6,E15.6),
              HEAD6(X10,"VFP",X2,"TRAY",X5, "VF",X10,"TVF",X9, "HVF",
                    X5,"COMPONENT",X10,"YLF"/),
              HEAD7(//X35,"DRAWS"//),
              HEAD8(X10,"LDP",X10,"TRAY",X15,"LD"/),
              HEAD9(X10,"VDP",X10,"TRAY",X15,"VD"/),
              FOUT6(2I13,,X10,E14.6),
              HEAD10(//X30,"HEATERS AND INTERCOOLERS"//),
              HEAD11(X10,"QP",X10,"TRAY",X15,"Q"/),
              FELF ("SUM XLF[I,",I1,"] = ",E16.8),
              FEVF ("SUM YVF[I,",I1,"] = ",E16.8);
PROCEDURE     KCALC(N, T, P, K, PKT) ;
VALUE         N, T, P ;
INTEGER       N ;
REAL          T, P ;
ARRAY         K, PKT[0] ;
         BEGIN
INTEGER       I, U ;
REAL          A, B, C ;
LABEL         ERRK, INTER ;
FORMAT OUT    FMOEK(///"TEMPERATURE EXCEEDS LIMIT IN KCALC"///) ;
              FOR U ~ 1 STEP 1 UNTIL NDATA DO
              IF TDATA[U] > T THEN GO TO INTER ;
ERRK:         WRITE(FLOT, FMOEK) ;    GO TO START ;
INTER:        IF U = 1 THEN GO TO ERRK ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              A ~ (KC[I,U] - KC[I,U-1]) / (TINV[U] - TINV[U-1]) ;
              B ~ KC[I,U] - TINV[U] | A ;
              C ~ 1.0 / (T + 460.0) ;
              K[I] ~ EXP(A | C | B) ;  PKT[I] ~ -K[I] | A | C | C
         END ;
         END KCALC ;
PROCEDURE     HLCALC(N, T, P, Z, H, PHT, PHZ) ;
VALUE         N, T, P ;
INTEGER       N ;
REAL          T, P, H, PHT ;
ARRAY         Z, PHZ[0] ;
         BEGIN
INTEGER       I, U ;
REAL          A, B ;
LABEL         INTER ;
              FOR U ~ 1 STEP 1 UNTIL NDATA DO
              IF TDATA[U] > T THEN GO TO INTER ;
INTER:        H ~ PHT ~ 0.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              A ~ (HLC[I,U] - HLC[I,U-1]) / (TDATA[U] - TDATA[U-1]) ;
              PHZ[I] ~ B ~ (T - TDATA[U]) | A + HLC[I,U] ;
              H ~ Z[I] | B + H ;       PHT ~ Z[I] | A + PHT
         END ;
         END HLCALC ;
PROCEDURE     HVCALC(N, T, P, Z, H, PHT, PHZ) ;
VALUE         N, T, P ;
INTEGER       N ;
REAL          T, P, H, PHT ;
ARRAY         Z, PHZ[0] ;
         BEGIN
INTEGER       I, U ;
REAL          A, B ;
LABEL         INTER ;
              FOR U ~ 1 STEP 1 UNTIL NDATA DO
              IF TDATA[U] > T THEN GO TO INTER ;
INTER:        H ~ PHT ~ 0.0;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              A ~ (HVC[I,U] - HVC[I,U-1]) / (TDATA[U] - TDATA[U-1]) ;
              PHZ[I] ~ B ~ (T - TDATA[U]) |A + HVC[I,U] ;
              H ~ Z[I] | B + H ;       PHT ~ Z[I] | A + PHT
         END ;
         END HVCALC ;
PROCEDURE     INVERSE(N, A, EPS, SINGULAR) ;
COMMENT       THIS PROCEDURE COMPUTES THE INVERSE OF A MATRIX.  IF THE
              MATRIX TO BE INVERTED IS SINGULAR, AN EXIT IS MADE TO THE
              NON-LOCAL LABEL "SINGULAR".  THE INVERSE MATRIX REPLACES
              THE ORIGINAL MATRIX IN MEMORY.
              R.D. RODMAN,
              (PROFESSIONAL SERVICES DIVISIONAL GROUP),
              CARD SEQUENCE BEGINS WITH INVS0010.
              FIRST RELEASE 12/01/62.  ;
VALUE         N, EPS ;
INTEGER       N ;
REAL ARRAY    A[0,0] ;
REAL          EPS ;
LABEL         SINGULAR ;
         BEGIN
INTEGER       I, J, K, II, N1, K2, L ;
REAL          BIG, TEMP, DIAG, Q ;
LABEL         I2, I3, I4, I5, I6, SK3 ;
COMMENT       THE MATRIX IS TRIANGULARIZED USING ROW PIVOTS.  IF THE
              LARGEST ELEMENT IN A NEWLY CALCULATED COLUMN IS, IN
              ABSOLUTE VALUE, LESS THAN EPS, AN EXIT IS MADE TO THE
              NON-LOCAL LABEL "SINGULAR". ;
I2:           FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              II ~ I-1 ;
              FOR J ~ I STEP 1 UNTIL N DO
         BEGIN
              Q ~ 0 ;
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[J,K] | A[K,I] + Q ;
              A[J,I] ~ A[J,I] - Q
         END ;
              BIG ~ 0 ;  K2 ~ I ;
I3:           FOR K ~ I STEP 1 UNTIL N DO
         BEGIN
              IF ABS(A[K,I]) > BIG THEN
         BEGIN
              BIG ~ ABS(A[K,I]) ;  K2 ~ K
         END
         END ;
              IF BIG { EPS THEN GO TO SINGULAR ;
              F[I] ~ K2 ;
              IF K2 ! I THEN
I4:           FOR K ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TEMP ~ A[I,K] ; A[I,K] ~ A[K2,K] ;  A[K2,K] ~ TEMP
         END ;
              DIAG ~ A[I,I] ;
COMMENT       A PIVOT WAS MADE.  A ROW IS NOW CALCULATED. ;
              FOR J ~ I+1 STEP 1 UNTIL N DO
         BEGIN
              Q ~ 0 ;
              FOR K ~ 1 STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] | Q ;
              A[I,J] ~ (A[I,J] - Q) / DIAG
         END
         END ;
COMMENT       THE LOWER TRIANGULAR MATRIX IS INVERTED.  ;
I5:           FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              II ~ I-1 ;  DIAG ~ A[I,I] ;
              FOR J ~ 1 STEP 1 UNTIL I DO
         BEGIN
              IF I=J THEN A[I,J] ~ 1/DIAG ELSE
         BEGIN
              Q ~ 0 ;
              FOR K ~ J STEP 1 UNTIL II DO Q ~ A[I,K] | A[K,J] + Q ;
              A[I,J] ~ -Q/DIAG
         END
         END
         END ;
              N1 ~ N-1 ;
COMMENT       THE UPPER TRIANGULAR MATRIX IS INVERTED.  ;
         BEGIN
              II ~ I+1 ;
              FOR J ~ N STEP -1 UNTIL II DO
         BEGIN
              Q~0 ;  L ~ J-1 ;
              FOR K ~ II STEP 1 UNTIL L DO Q ~ A[I,K] | A[K,J] + Q ;
              A[I,J] ~ -A[I,J] - Q
         END
         END ;
COMMENT       THE INVERTED TRIANGULAR MATRICES ARE MULTIPLIED TOGETHER.
              THEIR PRODUCT IS THE DESIRED INVERSE.  ;
              FOR I ~ 1 STEP 1 UNTIL N1 DO
              FOR J ~ 1 STEP 1 UNTIL N DO
         BEGIN
              Q ~ 0 ;
              IF I}J THEN
         BEGIN
              FOR K ~ I+1 STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;
              A[I,J] ~ A[I,J] + Q
         END
              ELSE
         BEGIN
              FOR K ~ J STEP 1 UNTIL N DO Q ~ A[I,K] | A[K,J] + Q ;
              A[I,J] ~ Q
         END
         END ;
COMMENT       THE COLUMNS OF THE PRODUCT ARE NOW PERMUTED IN SUCH A WAY
              AS TO COMPENSATE FOR THE ROW PIVOTS MADE IN THE COURSE OF
              TRIANGULARIZATION.  ;
         BEGIN
I6:           FOR J ~ N STEP -1 UNTIL 1 DO
              IF F[J] = J THEN GO TO SK3 ;
              K2 ~ F[J] ;
              FOR K ~ 1 STEP 1 UNTIL N DO
    BEGIN
              TEMP ~ A[K,K2] ;  A[K,K2] ~ A[K,J] ;  A[K,J] ~ TEMP
         END ;
SK3:     END;
         END ;
PROCEDURE     GENERALDISTILLATION (PROB, DESC1, DESC2, DESC3, SPEC1,
              SPEC2, DATE1, DATE2, PCONST, RITMID, OPTION, CONDENSER,
              ITER, M, N, NLF, NVF, NLD, NVD, NQ, RA, RD, P, TOLX, TOLT,
              TOLV, LM, TM, HLM, V1, T1, HV1, Q1, QM, NAMEA, NAMEB, P1,
              T, V, LFP, LF, TLF, HLF, VFP, VF, TVF, HVF, LDP, LD, TLD,
              HLD, VDP, VD, TVD, HVD, QP, Q, XM, Y1, XLF, YVF, XLD,YVD);
VALUE         PROB, DESC1, DESC2, DESC3, SPEC1, SPEC2, DATE1, DATE2,
              PCONST, RITMID, OPTION, CONDENSER, ITER, N, M, NLF, NVF,
              NLD, NVD, NQ, RD, RA, P, TOLX, TOLT, TOLV, Q1, QM ;
INTEGER       OPTION, CONDENSER, ITER, N, M, NLF, NVF, NLD, NVD, NQ ;
ALPHA         PROB, DESC1, DESC2, DESC3, SPEC1, SPEC2, DATE1, DATE2 ;
BOOLEAN       PCONST, RITMID ;
REAL          RA, RD, P, TOLX, TOLT, TOLV, LM, TM, HLM, V1, T1, HV1, Q1,
              QM ;
INTEGER ARRAY LFP, VFP, LDP, VDP, QP[0] ;
ALPHA ARRAY   NAMEA, NAMEB[0] ;
ARRAY         P1, T, V, LF, TLF, HLF, VF, TVF, HVF, LD, TLD, HLD, VD,
              TVD, HVD, Q, XM, Y1[0], XLF, YVF, XLD, YVD[0,0] ;
         BEGIN
INTEGER       COUNT, U, O, N1, M0, M1, J0, J1, TIS1, TIS2 ;
INTEGER       TIS3, TIS4 ;
BOOLEAN       BSV1, BST1, BSTM, BV1, BRD, BT1, BTM ;
REAL          TRS1, TRS2, TRS3, TRS4 ;
INTEGER ARRAY LFP1, VFP1, LDP1, VDP1, QP1[0:M+1] ;
ARRAY         LNET, VNET, SUMFS, DA, DB, DC, DD, DV, DT, TRA1,
              TRA2, KH, DTB, DTC, DVA, DVB, DAX, DBX, DCX[0:M+1], TR2A1,
              TR2A2, DINV, KHV[0:M+1, 0:M+1], DX[0:N, 0:M],
              KX[0:M+1, 0:N], KXT, KXV[0:M+1, 0:M+1] ;
LABEL         ERRT, ERRV, FINISH, SOLUTION ;
LABEL         TPER ;
LIST          TAPEV(FOR U ~ 1 STEP 1 UNTIL M DO KXV[J,U]),
              TAPET(FOR U ~ 1 STEP 1 UNTIL M DO KXT[J,U]) ;
FORMAT        FTAPE(100O) ;
FORMAT OUT    FSOL1 (X25,"GENERAL DISTILLATION CALCULATIONS"////"CASE",
              X30,A6//"DATE",X30,2A6//"DESCRIPTION",X23,3A6////X20,
              "I. SUMMARY OF INPUT SPECIFICATIONS"///"OPTION",I2,X10,A6,
              " AND",A6," ARE SPECIFIED"//"NUMBER OF COMPONENTS",I20//
              "NUMBER OF TRAYS",I25//"CONSTANT COLUMN PRESSURE",L17//),
              FTPER(///X20, "ERROR IN TAPE"),
              FSOL2A("CONDENSER SPECIFICATION            TOTAL"//),
              FSOL2B("CONDENSER SPECIFICATION          PARTIAL"//),
              FSOL2C("CONDENSER SPECIFICATION             NONE"//),
              FSOL3 ("FEED, DRAW, HEATER AND INTERCOOLER LOCATIONS"//),
              FSOL4A("LIQUID FEED ",I2," AT TRAY ",I2,),
              FSOL4B("VAPOR  FEED ",I2," AT TRAY ",I2,),
              FSOL5A("LIQUID DRAW ",I2," AT TRAY ",I2,),
              FSOL5B("VAPOR  DRAW ",I2," AT TRAY ",I2,),
              FSOL6 ("HEAT  INPUT ",I2," AT TRAY ",I2,),
              FSOL7 (X25,"II. SUMMARY OF RESULTS"///"TRAY CONDITIONS"//
              "TRAY  TEMP.   LIQUID    VAPOR    LIQUID  VAPOR   LIQUID",
              "  VAPOR    HEAT  "/X15,"RATE     RATE      FEED   FEED ",
              "    DRAW   DRAW     INPUT"//"      DEG F   MOL/HR    ",
              "MOL/HR ",4("  MOL/HR")",   BTU/HR"/X66,"|10*-3"//),
              FSOL8 (I3,F8.2,F10.3,F9.3,F9.2,3F8.2,F9.2),
              FSOL9 ("TERMINAL CONDITIONS"//"OVERHEAD RATE",F18.3,X6,
              "BOTTOMS RATE",F17.3/"REFLUX RATIO",F19.3,X6,"BOIL-UP ",
              "RATIO",F16.3/"CONDENSER DUTY",F13.1," BTU",X6,"REBOILER",
              " DUTY",F12.1," BTU"/"CONDENSER TEMP.",F16.2,X6,
              "REBOILER TEMP",F16.2///),
              FSOL10("PRODUCT DISTRIBUTION"///"COMPONENT",X21,"XM[I]",
              X14,"Y1[I]"/),                 FSOL11(2A6,2E20.5),
              FSOL12(X25,"III. TRAY PRINTOUT"////),
              FSOL13  ("TRAY",I30//"TEMPERATURE",F23.2/"PRESSURE",F26.2/
              "LIQUID ENTHALPY",F19.3/"VAPOR ENTHALPY",F20.3/"HEAT ",
              "INPUT",F24.3///X30,"LIQUID FLOW",X13,"VAPOR FLOW"),
              FSOL14(//X45,"FEED"),          FSOL15(//X45,"DRAW"),
              FSOL16(X32,"LIQUID",X18,"VAPOR"),
              FSOL17(X9,"COMPONENT",X5,2("    MOL/HR  MOL FRACTION")),
              FSOL18A(X6,2A6,F15.3,E12.3),
              FSOL18B(X6,2A6,F39.3,E12.3),
              FSOL18C(X6,2A6,X3,2(F12.3,E12.3)),
              FSOL19A(//X11,"TOTAL",F17.3),
              FSOL19B(//X11,"TOTAL",F39.3),
             FSOL19C(///X11,"TOTAL",F17.3,F24.3),
              FINT1 (X25,"INITIAL ESTIMATES"//X10,"TRAY",X16,"L",X16,
              "COMPONENT",X10,"X"//),
              FINT2 (I12,F19.5),             FINT3 (X45,2A6,E15.5),
              FINT4 (X25,"INTERMEDIATE PRINTOUT - COUNT =",I2//),
              FINT5A(X40,"FULL ADJUSTMENT"/),
              FINT5B(X30,"ADJUSTMENT REDUCED BY",F7.5/),
              FINT6(X8,"TRAY",X4," DELTA T",X6,"DELTA  V",X8,"TEMP",
                    X8,"VAPOR RATE",X4,"LIQUID RATE"//),
              FINT7(I12, 5E14.4),
              FINT8(//X6,"CONDENSER DUTY             =  ", E14.4,
                     /X6,"REBOILER DUTY              =  ", E14.4,
                     /X6,"MAX ENTHALPY UNBALANCE     =  ", E14.4,
                     "  AT TRAY ", I3,
                     /X6,"MAX BUBBLE POINT DEVIATION =  ", E14.4,
                     "  AT TRAY ", I3,
                     /X6,"MAX COMP MATERIAL UNBALANCE=  ", E14.4,
                     "  AT TRAY ", I3, " FOR COMPONENT ", I3),
              FERRT ("ERROR IN T COEFFICIENT MATRIX"),
              FERRV ("ERROR IN V COEFFICIENT MATRIX"),
              FEND (/////X40,"END OF",A6," PROBLEM") ;
              COUNT ~ 1 ;                    N1 ~ N - 1 ;
              M0 ~ M - 1 ;                   M1 ~ M + 1 ;
              M2 ~ M + 2 ;
              TRS1 ~ 0.0 ;                   V1 ~ V[1] ;
              LF[0] ~ VF[0] ~ LD[0] ~ VD[0] ~ Q[0] ~ 0.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO XLF[0,I] ~ YVF[0,I] ~ 0.0 ;
              FOR J ~ 0 STEP 1 UNTIL M1 DO
              LFP1[J] ~ VFP1[J] ~ LDP1[J] ~ VDP1[J] ~ QP1[J] ~ 0 ;
              FOR J ~ O STEP 1 UNTIL M1 DO
              FOR U ~ O STEP 1 UNTIL M1 DO
              DINV[J,U] ~ KXT[J,U] ~ KXV[J,U] ~ 0.0 ;
              FOR I ~1 STEP 1 UNTIL N DO
              X[0,I] ~ KX[0,I] ~ KX[M1,I] ~ 0.0 ;
              L[0] ~ L[M1] ~ V[M1] ~ VNET[M1] ~ HL[0] ~ HV[M1] ~ 0.0 ;
              FOR U ~ 1 STEP 1 UNTIL NLF DO LFP1[LFP[U]] ~ U ;
              FOR U ~ 1 STEP 1 UNTIL NVF DO VFP1[VFP[U]] ~ U ;
              FOR U ~ 1 STEP 1 UNTIL NLD DO LDP1[LDP[U]] ~ U ;
              FOR U ~ 1 STEP 1 UNTIL NVD DO VDP1[VDP[U]] ~ U ;
              FOR U ~ 1 STEP 1 UNTIL NVD DO LDP1[LDP[U]] ~ U ;
              FOR U ~ 1 STEP 1 UNTIL NQ DO QP1[QP[U]] ~ U ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J1 ~ J + 1 ;                   TRS1 ~ LD[LDP1[J]] ;
              IF NOT PCONST THEN P ~ P1[J] ;
              KCALC(N, T[J], P, K[J,*], PKT[J,*]) ;
              SUMFS[J] ~ LF[LFP1[J]] + VF[VFP1[J1]] - VD[VDP1[J1]] +
              SUMFS[J-1] - TRS1 ;
              L[J] ~ V [J] ;
              L[J] ~ V[J1] + SUMFS[J] - V1 ;
              LNET[J] ~ L[J] + TRS1 ;
              VNET[J] ~ V[J] - VD[VDP1[J]]
         END  ;
              IF CONDENSER = 2 THEN
              FOR I ~ 1 STEP 1 UNTIL N DO K[1,I] ~ 1.0 ;
COMMENT       CONSTRUCT A TRIDIAGONAL MATRIX FROM THE
              COMPONENT MATERIAL BALANCE EQUATION ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ J - 1 ;                   J1 ~ J + 1 ;
              O ~ LFP1[J] ;                  U ~ VFP1[J1] ;
              DA[J] ~ - L[J0] ;
              DB[J] ~ K[J,I] | V[J] + LNET[J] ;
              DC[J] ~ - K[J1,I] | VNET[J1] ;
              DD[J] ~ XLF[O,I] | LF[O] + YVF[U,I] | VF[U] ;
         END  ;
COMMENT       SOLVE MATRICES FOR X[J,I] ;
              DA[1] ~ DC[M] ~ 0.0 ;
              TRA1 [1] ~ DC[1] / DB[1] ;     TRA2[1] ~ DD[1] / DB[1] ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ J - 1 ;
              TRS1 ~ TRA1[J0] | DA[J] + DB[J] ;
              TRA1[J] ~ - DC[J] / TRS1 ;
              TRA2[J] ~ - (TRA2[J0] | DA[J] - DD[J]) / TRS1
         END  ;
              FOR J ~ M STEP -1 UNTIL 1 DO
         BEGIN
              J0~ J - 1 ;
              TRA2[J0] ~ TRA1[J0] | TRA2[J] + TRA2[J0]
         END  ;
              FOR J ~ 1 STEP 1 UNTIL M DO
              IF TRA2[J] < 0.0 THEN X[J,I] ~ 0.0 ELSE X[J,I] ~ TRA2[J]
         END  ;
              WRITE (FLOT[PAGE]) ;
              IF RITMID THEN WRITE (FLOT, FINT1) ;
COMMENT       NORMALIZE THE MOL FRACTIONS ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ 0.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO TRS1 ~ X[J,I] + TRS1 ;
              FOR I ~ 1 STEP 1 UNTIL N DO X[J,I] ~ X[J,I] / TRS1 ;
              IF RITMID THEN
         BEGIN
              WRITE (FLOT, FINT2, J, L[J]) ; WRITE (FLOT, FINT3,
              FOR I ~ 1 STEP 1 UNTIL N DO [NAMEA[I], NAMEB[I], X[J,I]])
         END
         END  ;
              BV1 ~ BRD ~ BT1 ~ BTM ~ BSV1 ~ BST1 ~ BSTM ~ FALSE ;
              IF OPTION ! 1 THEN
                   IF OPTION = 2 THEN BV1 ~ BSV1 ~ TRUE
              ELSE IF OPTION = 3 THEN BRD ~ BSV1 ~ TRUE
              ELSE IF OPTION = 4 THEN BT1 ~ BST1 ~ TRUE
              ELSE IF OPTION =  5 THEN BTM ~ TRUE
              ELSE IF OPTION = 6 THEN BV1 ~ BRD ~ BSV1 ~ TRUE
              ELSE IF OPTION =  7 THEN BV1 ~ TRUE
              ELSE IF OPTION = 8 THEN BV1 ~ BT1 ~ BSV1 ~ TRUE
              ELSE IF OPTION = 9 THEN BV1 ~ BTM ~ BSV1 ~ BSTM ~ TRUE
              ELSE IF OPTION = 10 THEN BRD ~ TRUE
              ELSE IF OPTION = 11 THEN BRD ~ BT1 ~ BST1 ~ TRUE
              ELSE IF OPTION = 12 THEN BRD ~ BTM ~ BSV1 ~ BSTM ~ TRUE
              ELSE IF OPTION = 13 THEN BT1 ~ TRUE
              ELSE IF OPTION = 14 THEN BTM ~ BSTM ~ TRUE
              ELSE IF OPTION = 15 THEN BT1 ~ BTM ~ BST1 ~ BSTM ~ TRUE ;
         BEGIN
LABEL         REITERATE, ADJUST ;
REITERATE :   FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
COMMENT       COMPUTE K, ENTHALPIES AND PARTIAL DERIVATIVES
              FOR EACH ITERATION ;
              TRS1 ~ T[J] ;
              IF NOT PCONST THEN P ~ P1[J] ;
         BEGIN
              L[J] ~ V[J+1] + SUMFS[J] - V1 ;
              KCALC(N, T[J], P, K[J,*], PKT[J,*]) ;
              LNET[J] ~ L[J] + LD[LDP1[J]] ;
              VNET[J] ~ V[J] - VD[VDP1[J]]
         END  ;
              HLCALC(N, T[J], P, X[J,*], HL[J], PHLT[J], PHLX[J,*]) ;
              TRS1 ~ PHLX[J,KEY] ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              PHLX[J,I] ~ PHLX[J,I] - TRS1 ;
              KX[J,I] ~ K[J,I] | X[J,I]
    END  ;
              HVCALC(N, T[J], P, KX[J,*], HV[J], PHVT[J], PHVY[J,*]) ;
         END  ;
COMMENT       SET UP FOR TOTAL CONDENSER. ;
              IF CONDENSER = 2 THEN
         BEGIN
              HV[1] ~ HL[1] ;          PHVT[1] ~ PHLT[1] ;
              FOR I ~ 1 STEP 1 UNTIL N DO KX[1,I] ~ X[1,I] ;
         END  ;
COMMENT       CALCULATE CONDENSER AND REBOILER DUTIES INITIALLY. ;
              IF COUNT = 1 THEN
         BEGIN
              IF (BSV1 OR BST1) OR (OPTION = 5) THEN
              Q1 ~ V[1] | HV[1] + L[1] | HL[1] - V[2] | HV[2] ;
              J ~ M - 1 ;
              IF OPTION > 5 THEN
              QM ~ L[M] | HL[M] + V[M] | HV[M] - L[J] | HL[J] ;
         END ;
COMMENT       CHECK EQUATIONS ARE SATISTIED. ;
              TRA1[1] ~ TRA1[2] ~ TRA1[3] ~ 0 ;
              TIS2 ~ TIS3 ~ TIS4 ~ N1 ~ 0 ;
              FOR J ~ 1 STEP 1 UNTIL M DO
          BEGIN
              J0 ~ J - 1 ;             J1 ~ J + 1 ;
              O ~ LFP1[J] ;            U ~ VFP1[J1] ;
              TIS1 ~ QP1[J] ;          TRS1 ~ 1.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO
          BEGIN
              TRS1 ~ -K[J,I] | X[J,I] + TRS1 ;
              TRS2 ~ KX[J,I] | V[J] | X[J,I] | LNET[J] ;
              TRS3 ~ XLF[O,I] | LF[O] + YVF[U,I] | VF[U] ;
              DX[I,J] ~ TRS3 ~ X[J0,I] | L[J0] + KX[J1,I] | VNET[J1]
                    + TRS3 - TRS2 ;
              IF TRS2 ! 0.0 THEN TRS3 ~ ABS(TRS3 / TRS2) ;
              IF TRS3 > TRA1[1] THEN
         BEGIN
              TRA1[1] ~ TRS3 ;     N1 ~ I ;    TIS2 ~ J
         END ;
         END ;
              DT[J] ~ TRS1 ;
              IF ABS(TRS1) > TRA1[2] THEN
         BEGIN
              TRA1[2] ~ ABS(TRS1) ;  TIS3 ~ J
         END ;
              TRS1 ~ LNET[J] | HL[J] + V[J] | HV[J] ;
              TRS2 ~ HLF[O] | LF[O] + HVF[U] | VF[U] + Q[TIS1] ;
              IF J = 1 THEN TRS2 ~ TRS2 + Q1 ;
              IF J = M THEN TRS2 ~ TRS2 + QM ;
              DV[J] ~ (L[J0] | HL[J0] + VNET[J1] | HV[J1]
                   + TRS2) - TRS1 ;
              TRS1 ~ ABS(DV[J] / TRS1) ;
              IF TRS1 > TRA1[3] THEN
              BEGIN
              TRA1[3] ~ TRS1; TIS4 ~ J
         END ;
         END ;
              WRITE(FLOT, FINT8, Q1, QM, TRA1[3], TIS4, TRA1[2],
                    TIS3, TRA1[1], TIS2, N1) ;
              IF TRA1[3] < TOLV THEN IF TRA1[2] < TOLT THEN
              IF TRA1[1] < TOLX THEN GO TO SOLUTION ;
              IF COUNT > ITER THEN GO TO SOLUTION ;
COMMENT       COMPUTE COEFFICIENTS FOR EXPRESSING DELTA X IN TERMS OF
              DELTA T AND DELTA V USING MATERIAL BALANCES ;
COMMENT       CONSTRUCT DGX, DGX/DX[I], DGX/DT AND DGX/DV ;
              TRS2 ~ 1.0 / (1.0 + RD) ;
              FOR I ~ 1 STEP 1 UNTIL N DO IF I ! KEY THEN
         BEGIN
FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ J - 1 ;                   J1 ~ J + 1 ;
              O ~ LFP1[J] ;                  U ~ VFP1[J1] ;
              DA [J] ~ - L[J0] ;
              DB[J] ~ K[J,I] | V[J] + LNET[J] ;
              DC[J] ~ - K[J1,I] | VNET[J1] ;
              DD[J] ~ DX[I,J] ;
              TRS1 ~ X[J,I] | PKT[J,I];
              DTB[J] ~ V[J] | TRS1 ;
              DTC[J] ~ -VNET[J] | TRS1 ;
              DVA[J] ~ X[J0,I] - X[J,I] ;
              IF BV1 THEN DVA[J] ~ 0.0 ;
              IF BRD THEN DVA[J] ~ DVA[J] | TRS2 ;
              DVB[J] ~ KX[J,I] - X[J0,I] ;
         END  ;
              DTC[1] ~ 0.0 ;
              IF CONDENSER = 2 THEN
         BEGIN
              DB[1] ~ V[1] + LNET[1] ; DTB[1] ~ 0 ;
         END ;
              IF BT1 THEN DTB[1] ~ 0.0 ;
              IF BTM THEN
         BEGIN
         DTB[M] ~ 0.0;                      DTC[M0] ~ 0.0
         END  ;
              IF BRD THEN DVA[1] ~ DVB[1] | TRS2 + DVA[1] ;
              IF BV1 OR BRD THEN DVB[1] ~ 0.0 ;
              IF OPTION = 6 THEN DVA[1] ~ DVB[2] ~ 0.0 ;
COMMENT       THE TRIDIAGONAL MATRIX IS DECOMPOSED INTO TWO TRIANGULAR
              MATRICES ;
              DC[1] ~ DC[1] / DB[1] ;
              FOR J ~ 2 STEP 1 UNTIL M DO
         BEGIN
              DB[J] ~ - DC[J-1] | DA[J] + DB[J] ;
              DC[J] ~ DC[J] / DB[J]
         END  ;
COMMENT       THE UPPER TRIANGULAR MATRIX IS INVERTED ;
              FOR J ~ 1 STEP 1 UNTIL M0 DO DINV[J,J+1] ~ - DC[J] ;
              FOR J ~ 2 STEP 1 UNTIL M0 DO
         BEGIN
              J1 ~ M - J ;
              FOR U ~ 1 STEP 1 UNTIL J1 DO
              DINV[U,U+1] ~ - DINV[U+1,U+J] | DC[U]
         END  ;
COMMENT       THE LOWER TRIANGULAR MATRIX IS INVERTED ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ DINV[J,J] ~ 1.0 / DB[J] ;
              DC[J] ~ - DA[J] | TRS1
         END  ;
              FOR J ~ 1 STEP 1 UNTIL M0 DO
              BEGIN
              J1 ~ J + 1 ;
              FOR U ~ J1 STEP 1 UNTIL M DO
              DINV[U,U-J] ~ DINV[U-1,U-J] | DC[U]
         END  ;
COMMENT       THE INVERTED TRAINGULAR MATRICES ARE MULTIPLIED TOGETHER
              TO OBTAIN THE FINAL INVERSE ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ M - J ;                  DC[J] ~ 1.0 ;
              FOR U ~ 1 STEP 1 UNTIL J0 DO DC[U+J] ~ DINV[J,U+J] ;
              FOR U ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ 0.0 ;
           IF U } J  THEN J1 ~ U ELSE J1 ~ J ;
              FOR O ~ J1 STEP 1 UNTIL M DO
              TRS1 ~ DINV[O,U] | DC[O] + TRS1 ;
              DINV[J,U] ~ TRS1
         END
         END  ;
COMMENT       MULTIPLY THE INVERSE BY THE OTHER MATRICES
              TO OBTAIN THE PROPER COEFFICIENTS ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ J - 1 ;
              TRS2 ~ 0.0 ;
              FOR U ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS2 ~ DINV[J,U] | DD[U] + TRS2 ;
              KXV[J,U] ~ (DINV[U,J0] - DINV[U,J]) | DVB[J] ;
              IF BRD AND NOT BV1 THEN
              KXV[2,U] ~ -DINV[U,J] | DVA[J] + KXV[2,U] ELSE
              KXV[1,U] ~ -DINV[U,J] | DVA[J] + KXV[1,U]
         END  ;
              KX[J,I] ~ TRS2
         END  ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              WRITE(TAPEIO, FTAPE, TAPET) ;
              WRITE(TAPEIO, FTAPE, TAPEV) ;
         END ;
         END  ;
COMMENT       USE HEAT BALANCE EQUATIONS TO OBTAIN THE RELATION
              DELTA T = KH + KHV | (DELTA V) ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ J - 1 ;             J1 ~ J + 1 ;
              DD[J] ~ DV[J] ;
              DTC[J] ~ PHVY[J,KEY] | K[J,KEY] ;
              DTB[J] ~ 0.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO
              DTB[J] ~ PKT[J,I] | X[J,I] | PHVY[J,I] + DTB[J] ;
              IF J = 1 AND CONDENSER = 2 THEN DTB[1] ~ 0.0 ;
              FOR U ~ 1 STEP 1 UNTIL M DO
              TR2A2[J,U] ~ TR2A1[J,U] ~ 0.0
         END ;
COMMENT       CONSTRUCT THE DGH/DX MATRIX ;
              FOR I ~ N STEP -1 UNTIL 1 DO IF I ! KEY THEN
         BEGIN
              TRS1 ~ 0.0 ;
              TRS2 ~ K[1,I] | PHVY[1,I] - DTC[1] ;
              IF CONDENSER = 2 THEN TRS2 ~ PHLX[1,I] ;
              FOR J ~ M STEP -1 UNTIL 1 DO
         BEGIN
              READ REVERSE(TAPEIO, FTAPE, TAPEV) [:TPER] ;
              READ REVERSE(TAPEIO, FTAPE, TAPET) [:TPER] ;
         END ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ J - 1 ;             J1 ~ J + 1 ;
              DAX[J] ~ - L[J0] | TRS1 ;
              TRS1 ~ PHLX[J,I] ;
              DBX[J] ~ LNET[J] | TRS1 + V[J] | TRS2 ;
              TRS2 ~ K[J1,I] | PHVY[J1,I] - DTC[J1] ;
              DCX[J] ~ -VNET[J1] | TRS2
         END ;
COMMENT       COMPUTE SUMMATION OF DGH/DX | DGX, ADD TO DGH AND
              STORE IN DD ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ J - 1 ;             J1 ~ J + 1 ;
              DD[J] ~ -KX[J0,I] | DAX[J] - KX[J,I] | DBX[J]
                      -KX[J1,I] | DCX[J] + DD[J] ;
              FOR U ~ 1 STEP 1 UNTIL M DO
COMMENT       COMPUTE SUMMATION OF DGH/DX TIMES
              (1). KXT AND STORE IN TR2A2 ,
              (2). KXV AND STORE IN TR2A1 ;
         BEGIN
              TR2A2[J,U] ~ -KXT[U,J0] | DAX[J] - KXT[U,J] | DBX[J]
                           -KXT[U,J]
                           -KXT[U,J1] | DCX[J] + TR2A2[J,U] ;
              TR2A1[J,U] ~ KXV[U,J0] | DAX[J] + KXV[U,J] | DBX[J]
                        + KXV[U,J1] | DCX[J] + TR2A1[J,U] ;
         END
         END
         END ;
COMMENT       ADD DGH/DT AND DGH/DV TO TR2A2 AND TR2A1 RESPECTIVELY ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              J0 ~ J - 1 ;             J1 ~ J + 1 ;
              TR2A2[J,J0] ~-L[J0] | PHLT[J0] + TR2A2[J,J0] ;
              TR2A2[J,J] ~ +(PHVT[J] + DTB[J]) | V[J]
                           +LNET[J] | PHLT[J] + TR2A2[J,J] ;
              TR2A2[J,J1] ~-(PHVT[J1] + DTB[J1]) | VNET[J1]
                           +TR2A2[J,J1] ;
              IF J ! 1 THEN TR2A1[J,J] ~ TR2A1[J,J] + HV[J] - HL[J0] ;
              TR2A1[J,J1] ~ TR2A1[J,J1] + HL[J] - HV[J1]
         END ;
              IF BT1 THEN
         BEGIN
              TR2A2[1,1] ~ TR2A2[2,1] ~ 0.0 ;
              IF BST1 THEN TR2A2[1,1] ~ - Q1 ELSE TR2A2[M,1] ~ - QM ;
         END ;
              IF BTM THEN
         BEGIN
              TR2A2[M,M] ~ TR2A2[M0,M] ~ 0.0 ;
              IF BSTM THEN TR2A2[M,M] ~ - QM ELSE TR2A2[1,M] ~ - Q1
         END ;
              IF BV1 THEN
         BEGIN
              TR2A1[1,1] ~ 0.0 ;
              IF BSV1 THEN TR2A1[1,1] ~ - Q1 ELSE TR2A1[M,1] ~ - QM ;
              IF OPTION = 6 THEN
         BEGIN
              TR2A1[1,2] ~ TR2A1[2,2] ~ 0.0 ;
              TR2A1[M,2] ~ - QM
         END
         END
              ELSE
         BEGIN
              TRS2 ~ 1.0 / (1.0 + RD) ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ HL[J-1] - HL[J] ;
              IF J = 1 THEN TRS1 ~ HV[1] - HL[1] ;
              IF NOT BRD THEN TR2A1[J,1] ~ TRS1 + TR2A1[J,1]
              ELSE TR2A1[J,2] ~ TRS1 | TRS2 + TR2A1[J,2]
         END ;
              IF BRD THEN IF BSV1 THEN TR2A1[1,1] ~ - Q1 ELSE
         BEGIN
              TR2A1[1,1] ~ 0.0 ;             TR2A1[M,1] ~ - QM
         END
         END ;
              INVERSE(M, TR2A2, 1.0@-8, ERRT) ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ 0.0 ;
              FOR U ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ TR2A2[J,U] | DD[U] + TRS1 ;
              TRS2 ~ 0.0 ;
              FOR O ~ 1 STEP 1 UNTIL M DO
              TRS2 ~ TR2A2[J,O] | TR2A1[O,U] + TRS2 ;
              KHV[J,U] ~ - TRS2
         END ;
              KH[J] ~ TRS1
         END ;
COMMENT       USING BUBBLE - POINT CODITIONS TO SOLVE FOR DELTA V. ;
COMMENT       COMPUTE DGB, DGB/DT AND DGB/DX AND STORE IN TRA1,
              TRA2 AND TRS1 RESPECTIVELY. ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRA1[J] ~ DT[J] ;
              TRA2[J] ~ X[J,KEY] |PKT[J,KEY] ;
              FOR U ~ 1 STEP 1 UNTIL M DO TR2A1[J,U] ~ TR2A2[J,U] ~ 0.0
         END  ;
              FOR I ~ 1 STEP 1 UNTIL N DO IF I ! KEY THEN
         BEGIN
              FOR J ~1 STEP 1 UNTIL M DO
         BEGIN
              READ (TAPEIO, FTAPE, TAPET) [:TPER] ;
              READ (TAPEIO, FTAPE, TAPEV) [:TPER] ;
         END ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRA2[J] ~ X[J,I] | PKT[J,I] + TRA2[J] ;
              TRS1 ~ K[J,I] - K[J,KEY] ;
COMMENT       COMPUTE THE SUMMATION OF DGB/DX TIMES KXV, KXT AND KX,
              AND STORE IN TR2A1, TR2A2 AND TRA1 RESPECTIVELY. ;
              FOR U ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TR2A1[J,U] ~ KXV[U,J] | TRS1 + TR2A1[J,U] ;
              TR2A2[J,U] ~ KXT[U,J] | TRS1 + TR2A2[J,U]
         END ;
              TRA1[J] ~ -KX[J,I] | TRS1 + TRA1[J]
         END
         END ;
COMMENT       ADD DGB/DT TO TR2A2 AND THEN MULTIPY WITH KHV AND KH ;
              IF BT1 THEN TRA2[1] ~ 0.0 ;
              IF BTM THEN TRA2[M] ~ 0.0 ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TR2A2[J,J] ~ TR2A2[J,J] - TRA2[J] ;
              TRS2 ~ 0.0 ;
              FOR U ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS2 ~ TR2A2[J,U] | KH[U] + TRS2 ;
              TRS3 ~ 0.0 ;
              FOR O ~ 1 STEP 1 UNTIL M DO
              TRS3 ~ -TR2A2[J,O] | KHV[O,U] + TRS3 ;
              TR2A1[J,U] ~ TR2A1[J,U] + TRS3
         END ;
              TRA1[J] ~ TRA1[J] + TRS2
         END ;
COMMENT       INVERT TR2A1 AND CALCULATE DELTA V ;
              INVERSE(M, TR2A1, 1.0@-8, ERRV) ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ 0.0 ;
              FOR U ~ 1 STEP 1 UNTIL M DO
              TRS1 ~ TR2A1[J,U] | TRA1[U] + TRS1 ;
              DV[J] ~ TRS1
         END ;
COMMENT       COMPUTE DELTA T AND DELTA X ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ 0.0 ;
              FOR U ~ 1 STEP 1 UNTIL M DO
              TRS1 ~ KHV[J,U] | DV[U] + TRS1 ;
              DT[J] ~ KH[J] + TRS1
         END ;
              FOR I ~ N STEP -1 UNTIL 1 DO IF I ! KEY THEN
         BEGIN
              FOR M ~ M STEP -1 UNTIL 1 DO
         BEGIN
              READ REVERSE(TAPEIO, FTAPE, TAPEV) [:TPER] ;
              READ REVERSE(TAPEIO, FTAPE, TAPET) [:TPER] ;
         END ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS1 ~ 0.0 ;
              FOR U ~ 1 STEP 1 UNTIL M DO
              TRS1 ~ KXV[U,J] | DV[U] - KXT[U,J] | DT[U] + TRS1 ;
              DX[I,J] ~ KX[J,I] + TRS1
         END
         END ;
COMMENT       REDUCE ADJUSTMENT BY DESIRED FACTOR. IF RA IS NOT FED IN,
              THEN ADJUSTMENT WILL BE 0.8 OF ACTUAL VALUE ;
              IF BV1 OR BRD THEN TRS1 ~ ABS(DV[1])
              ELSE TRS1 ~ ABS(DV[1]) / V[1] ;
              FOR J ~ 2 STEP 1 UNTIL M DO
         BEGIN
              TRS3 ~ L[J-1] ;
              IF TRS3 < V[J] THEN TRS2 ~ ABS(DV[J]) / TRS3 ELSE
              TRS2 ~ ABS(DV[J]) / V[J] ;
              IF J = 2 AND (OPTION= 6) THEN TRS2 ~ ABS(DV[2]) ;
              IF TRS2 > TRS1 THEN TRS1 ~ TRS2
         END ;
              IF RA = 0.0 THEN RA ~ 0.8 ;
              WRITE (FLOT[PAGE]) ;           WRITE (FLOT, FINT4, COUNT);
              IF TRS1 > RA THEN
         BEGIN
              TRS1 ~ RA / TRS1 ;
              WRITE (FLOT, FINT5B, TRS1) ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              DV[J] ~ DV[J] | TRS1 ;
              DT[J] ~ DT[J] | TRS1 ;
              FOR I ~ 1 STEP 1 UNTIL N DO DX[I,J] ~ DX[I,J] | TRS1
         END
         END
              ELSE  WRITE (FLOT, FINT5A) ;
              WRITE (FLOT, FINT6) ;
              FOR J ~ 1 STEP 1 UNTIL M DO
              WRITE(FLOT, FINT7, J, DT[J], DV[J], T[J], V[J], L[J]) ;
ADJUST:       FOR J ~ 2 STEP 1 UNTIL M0 DO
         BEGIN
              T[J] ~ T[J] +DT[J] ;
              V[J] ~ V[J] + DV[J] ;
         END  ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TRS2 ~ 1.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO IF I ! KEY THEN
         BEGIN
              TRS1 ~ X[J,I] + DX[I,J] ;
              IF TRS1 < 0.0 THEN TRS1 ~ 0.0 ;
              X[J,I] ~ TRS1 ;               TRS2 ~ TRS2 - TRS1
         END  ;
              X[J,KEY] ~ TRS2 ;
              IF TRS2 < 0.0 THEN
         BEGIN
              TRS2 ~ 1.0 - TRS2 ;
              FOR I ~ 1 STEP 1 UNTIL N DO IF I ! KEY THEN
              X[J,I] ~ X[J,I] / TRS2 ;
              X[J,KEY] ~ 0.0
         END
         END  ;
              IF BT1 THEN IF BST1 THEN Q1 ~ (DT[1] + 1.0) | Q1
              ELSE QM ~ (DT[1] + 1.0) | QM ELSE T[1] ~ T[1] + DT[1] ;
              IF BTM THEN IF BSTM THEN QM ~ (DT[M] + 1.0) | QM
              ELSE Q1 ~ (DT[M] + 1.0) | Q1 ELSE T[M] ~ T[M] + DT[M] ;
              V[M] ~ V[M] + DV[M] ;
              IF BV1 OR BRD THEN
         BEGIN
              IF BSV1 THEN Q1 ~ (DV[1] + 1.0) | Q1
              ELSE QM ~ (DV[1] + 1.0) | QM ;
              IF OPTION = 6 THEN
         BEGIN
              QM ~ (DV[2] + 1.0) | QM ;      V[2] ~ (RD + 1.0) | V[1]
         END  ;
              IF BRD THEN V[1] ~ V[2] / (RD + 1.0)
         END
              ELSE V[1] ~ V[1] + DV[1] ;     V1 ~ V[1] ;
              COUNT ~ COUNT + 1 ;      GO TO REITERATE ;
         END  ITERATIVE COMPUTATIONS ;
SOLUTION :    WRITE (FLOT[PAGE]) ;
              WRITE (FLOT, FSOL1, PROB, DATE1, DATE2, DESC1, DESC2,
                     DESC3, OPTION, SPEC1, SPEC2, N, M, PCONST) ;
              IF CONDENSER = 2 THEN WRITE (FLOT, FSOL2A) ELSE
              IF CONDENSER = 1 THEN WRITE (FLOT, FSOL2B) ELSE
              WRITE (FLOT, FSOL2C) ;         WRITE (FLOT, FSOL3) ;
              WRITE (FLOT, FSOL4A, FOR U ~ 1 STEP 1 UNTIL NLF DO
                     [U, LFP[U]]) ;
              WRITE (FLOT, FSOL4B, FOR U ~ 1 STEP 1 UNTIL NVF DO
                     [U, VFP[U]]) ;
              IF NLD ! 0 THEN WRITE(FLOT, FSOL5A,
              FOR U ~ 1 STEP 1 UNTIL NLD DO [U, LDP[U]]) ;
              IF NVD ! 0 THEN WRITE (FLOT, FSOL5B,
              FOR U ~ 1 STEP 1 UNTIL NVD DO [U, VDP[U]]) ;
              IF NQ ! 0 THEN WRITE (FLOT, FSOL6,
              FOR U ~ 1 STEP 1 UNTIL NQ DO [U, QP[U]]) ;
              WRITE (FLOT[PAGE]) ;           WRITE (FLOT, FSOL7) ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              IF J = 1 THEN TRS1 ~ Q1   ELSE IF J = M THEN TRS1 ~ QM
              ELSE TRS1 ~ Q[QP1[J]] ;
              U ~ LFP1[J] ;             O ~ VFP1[J+1] ;
              I ~ LDP1[J] ;             J1 ~ VDP1[J] ;
              TRS1 ~ HLF[U] | LF[U] + HVF[O] | VF[O]
                   - LD[I] | HL[I] - VD[J1] | HV[J1] + TRS1 ;
              WRITE(FLOT, FSOL8, J, T[J], L[J], V[J], LF[U],
                    VF[O], LD[I], VD[J1], TRS1/1000.0) ;
         END  ;
              WRITE (FLOT[PAGE]) ;
              LM ~ L[M] ;         TM ~ T[M] ;         HLM ~ HL[M] ;
              T1 ~ T[1] ;    HV1 ~ HV[1] ;    RD ~ L[1] / V[1] ;
              WRITE (FLOT, FSOL9, V1, LM, RD, V[M]/LM, Q1, QM, T1, TM) ;
              WRITE (FLOT, FSOL10) ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              Y1[I] ~ KX[1,I] | V1 ;   XM[I] ~ X[M,I] | LM ;
              WRITE (FLOT, FSOL11, NAMEA[I], NAMEB[I], XM[I], Y1[I])
         END  ;
              WRITE (FLOT[PAGE]) ;           WRITE (FLOT, FSOL12) ;
              FOR J ~ 1 STEP 1 UNTIL M DO
         BEGIN
              TIS1 ~ LFP1[J] ;               TIS2 ~ VFP1[J] ;
              TRS3 ~ L[J] ;                  TRS4 ~ V[J] ;
              IF J ! 1 THEN WRITE (FLOT[PAGE]) ;
              IF J = 1 THEN TRS1 ~ Q1 ELSE IF J = M THEN TRS1 ~ QM
              ELSE TRS1 ~ Q[QP1[J]] ;
              IF NOT PCONST THEN P ~ P1 [J] ;
              WRITE (FLOT, FSOL13, J, T[J], P, HL[J], HV[J], TRS1) ;
              WRITE (FLOT, FSOL17) ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS1 ~ X[J,I] ;          TRS2 ~ KX[J,I] ;
              WRITE (FLOT, FSOL18C, NAMEA[I], NAMEB[I], TRS3 | TRS1,
                     TRS1, TRS4 | TRS2, TRS2)
         END  ;
              WRITE (FLOT, FSOL19C, TRS3, TRS4) ;
              IF (TIS1 + TIS2) > 0 THEN
         BEGIN
              TRS1 ~ LF[TIS1] ;              TRS2 ~ VF[TIS2] ;
              WRITE (FLOT, FSOL14) ;         WRITE (FLOT, FSOL16) ;
              WRITE (FLOT, FSOL17) ;
              IF TIS2 = 0 THEN
         BEGIN
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS3 ~ XLF[TIS1,I] ;
              WRITE (FLOT, FSOL18A, NAMEA[I], NAMEB[I], TRS3|TRS1, TRS3)
         END  ;
              WRITE (FLOT, FSOL19A, TRS1)
         END
              ELSE IF TIS1 = 0 THEN
         BEGIN
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS3 ~ YVF[TIS2,I] ;
              WRITE (FLOT, FSOL18B, NAMEA[I], NAMEB[I], TRS3|TRS2, TRS3)
         END  ;
              WRITE (FLOT, FSOL19B, TRS2)
         END
              ELSE
         BEGIN
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS3 ~ XLF[TIS1,I] ;           TRS4 ~ YVF[TIS2,I] ;
              WRITE (FLOT, FSOL18C, NAMEA[I], NAMEB[I], TRS3 | TRS1,
                     TRS3, TRS4 | TRS2, TRS4)
         END  ;
              WRITE (FLOT, FSOL19C, TRS1, TRS2)
         END
         END  ;
              TIS1 ~ LDP1[J] ;               TIS2 ~ VDP1[J] ;
              IF (TIS1 + TIS2) > 0 THEN
         BEGIN
              TRS1 ~ LD[TIS1] ;              TRS2 ~ VD[TIS2] ;
              WRITE (FLOT, FSOL15) ;         WRITE (FLOT, FSOL16) ;
              WRITE (FLOT, FSOL17) ;
              IF TIS2 = 0 THEN
         BEGIN
              TLD[TIS1] ~ T[J] ;             HLD[TIS1] ~ HL[J] ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS3 ~ XLD[TIS1,I] ~ X[J,I] ;
              WRITE (FLOT, FSOL18A, NAMEA[I], NAMEB[I], TRS3|TRS1, TRS3)
         END  ;
              WRITE (FLOT, FSOL19A, TRS1)
         END
              ELSE IF TIS1 = 0 THEN
         BEGIN
              TVD[TIS2] ~ T[J] ;             HVD[TIS2] ~ HV[J] ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS3 ~ YVD[TIS2,I] ~ K[J,I] | X[J,I] ;
              WRITE (FLOT, FSOL18B, NAMEA[I], NAMEB[I], TRS3|TRS2, TRS3)
         END  ;
              WRITE (FLOT, FSOL19B, TRS2)
         END
              ELSE
         BEGIN
              TLD[TIS1]~ TVD[TIS2] ~ T[J] ;
              HLD[TIS1] ~ HL[J] ;            HVD[TIS2] ~ HV[J] ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS3 ~ XLD[TIS1,I] ~ X[J,I] ;
              TRS4 ~ YVD[TIS2,I] ~ K[J,I] | TRS3 ;
              WRITE (FLOT, FSOL18C, NAMEA[I], NAMEB[I], TRS3 | TRS1,
                     TRS3, TRS4 | TRS2, TRS4)
         END  ;
              WRITE (FLOT, FSOL19C, TRS1, TRS2)
         END
         END
         END       ;
              GO TO FINISH ;
TPER:         WRITE(FLOT, FTPER) ;    GO TO THEEND ;
ERRT :        WRITE (FLOT, FERRT) ;         GO TO FINISH ;
ERRV :        WRITE (FLOT, FERRV) ;
FINISH :      WRITE (FLOT, FEND, PROB) ;
         END  GENERAL DISTILLATION ;
COMMENT       READ INPUT DATA SPECIFYING THE PROBLEM. ;
              IF PCONST THEN READ (FLIN, FRIN2, P) ELSE
              READ (FLIN, FRIN3, FOR J ~ 1 STEP 1 UNTIL M DO P1[J]) ;
              READ (FLIN, FRIN3,
                   FOR J ~ 1 STEP 1 UNTIL M DO [T[J], V[J]]) ;
              FOR J ~ 1 STEP 1 UNTIL NQ DO READ(FLIN,FRIN6,QP[J],Q[J]) ;
              FOR J ~ 1 STEP 1 UNTIL NLF DO
         BEGIN
              READ(FLIN, FRIN7, LFP[J], LF[J], TLF[J], HLF[J], LFS[J]) ;
              READ (FLIN, FRIN8, FOR I ~ 1 STEP 1 UNTIL N DO XLF[J,I])
         END  ;
              FOR J ~ 1 STEP 1 UNTIL NVF DO
         BEGIN
              READ(FLIN, FRIN7, VFP[J], VF[J], TVF[J], HVF[J], VFS[J]) ;
              READ (FLIN, FRIN8, FOR I ~ 1 STEP 1 UNTIL N DO YVF[J,I])
         END  ;
              READ (FLIN, FRIN6,
                   FOR J ~ 1 STEP 1 UNTIL NLD DO [LDP[J], LD[J]],
                   FOR J ~ 1 STEP 1 UNTIL NVD DO [VDP[J], VD[J]]) ;
              READ(FLIN, FRIN4, FOR I ~ 1 STEP 1 UNTIL N DO
                         [NAMEA[I], NAMEB[I]]) ;
COMMENT       READ PHYSICAL PROPERTY DATA. ;
              READ(FLIN, FRIN3,
                     FOR J ~ 1 STEP 1 UNTIL NDATA DO TDATA[J]) ;
              READ(FLIN, FRIN3, FOR I ~ 1 STEP 1 UNTIL N DO
                    FOR J ~ 1 STEP 1 UNTIL NDATA DO HLC[I,J]) ;
              READ(FLIN, FRIN3, FOR I ~ 1 STEP 1 UNTIL N DO
                    FOR J ~ 1 STEP 1 UNTIL NDATA DO HVC[I,J]) ;
              READ(FLIN, FRIN3, FOR I ~ 1 STEP 1 UNTIL N DO
                    FOR J ~ 1 STEP 1 UNTIL NDATA DO KC[I,J]) ;
              FOR J ~ 1 STEP 1 UNTIL NDATA DO
         BEGIN
              TINV[J] ~ 1.0 / (TDATA[J] + 460.0) ;
              FOR I ~ 1 STEP 1 UNTIL N DO KC[I,J] ~ LN(KC[I,J])
         END ;
              WRITE (FLOT[PAGE]) ;           WRITE (FLOT, HEAD1, PROB) ;
COMMENT       CHECK FEED AND CALCULATE FEED TEMPERATURES AND SPLITS
              IF NECESSARY. ;
              FOR J ~ 1 STEP 1 UNTIL NLF DO
              IF LFS[J] < 3 THEN
         BEGIN
              TRS1 ~ 0.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO TRS1 ~ XLF[J,I] + TRS1 ;
              IF ABS(TRS1 - 1.0) > 1.0@-6 THEN
              IF ABS(TRS1 - LF[J]) < 1.0@-6 THEN
              FOR I ~ 1 STEP 1 UNTIL N DO XLF[J,I] ~ XLF[J,I] / TRS1
              ELSE
         BEGIN
              WRITE(FLOT, FELF, J, TRS1) ;
              GO TO THEEND
         END ;
              IF LFS[J] = 2 THEN
         BEGIN
              TRS1 ~ TLF[J] ;
              IF NOT PCONST THEN P ~ P1[LFP[J]] ;
LLL:          KCALC(N, TRS1, P, K[1,*], PKT[1,*]) ;
              TRS2 ~ 1.0 ;             TRS3 ~ 0.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS2 ~ -XLF[J,I] | K[1,I] + TRS2 ;
              TRS3 ~ XLF[J,I] | PKT[1,I] + TRS3 ;
         END ;
              IF ABS(TRS2) > 1.0@-6 THEN
         BEGIN
              TRS1 ~ TRS2 / TRS3 + TRS1 ;
              GO TO LLL
         END ;
              TLF[J] ~ TRS1
         END
         END ;
              FOR J ~ 1 STEP 1 UNTIL NVF DO
              IF VFS[J] < 3 THEN
         BEGIN
              TRS1 ~ 0.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO TRS1 ~ YVF[J,I] + TRS1 ;
              IF ABS(TRS1 - 1.0) > 1.0@-6 THEN
              IF ABS(TRS1 - VF[J]) < 1.0@-6 THEN
              FOR I ~ 1 STEP 1 UNTIL N DO YVF[J,I] ~ YVF[J,I] / TRS1
              ELSE
         BEGIN
              WRITE(FLOT, FEVF, J, TRS1) ;
              GO TO THEEND
         END ;
              IF VFS[J] = 2 THEN
         BEGIN
              TRS1 ~ TVF[J] ;
              IF NOT PCONST THEN P ~ P1[VFP[J]];
VVV:          KCALC(N, TRS1, P, K[1,*], PKT[1,*]) ;
              TRS2 ~ 1.0 ;             TRS3 ~ 0.0 ;
              FOR I ~ 1 STEP 1 UNTIL N DO
         BEGIN
              TRS2 ~ -YVF[J,I] / K[1,I] + TRS2 ;
              TRS3 ~-YVF[J,I] |  PKT[1,I] / (K[1,I] | K[1,I]) + TRS3 ;
         END ;
              IF ABS(TRS2) > 1.0@-6 THEN
         BEGIN
              TRS1 ~ TRS2 / TRS3 + TRS1 ;
              GO TO VVV
         END ;
              TVF[J] ~ TRS1
         END
         END ;
              FOR U ~ 1 STEP 1 UNTIL NLF DO IF HLF[U] = 0THEN
         BEGIN
              IF NOT PCONST THEN P ~ P1[LFP[U]] ;
              HLCALC(N, TLF[U], P, XLF[U,*],HLF[U], TRS1, PHLX[1,*]) ;
         END ;
              FOR U ~ 1 STEP 1 UNTIL NVF DO IF HVF[U] = 0 THEN
         BEGIN
              IF NOT PCONST THEN  P ~ P1[VFP[U]] ;
              HVCALC(N, TVF[U], P, YVF[U,*], HVF[U], TRS1, PHVY[1,*]) ;
         END ;
              IF RITIP THEN
         BEGIN
              WRITE (FLOT, HEAD2) ;
              WRITE (FLOT, FHOP1, OPTION, SPEC1, SPEC2) ;
              IF PCONST THEN
         BEGIN
              WRITE (FLOT, FOUT1, P) ;       WRITE (FLOT, HEAD3) ;
              WRITE (FLOT, FOUT2, FOR J ~ 1 STEP 1 UNTIL M DO
                     [J, T[J], V[J]])
         END
              ELSE
         BEGIN
              WRITE (FLOT, HEAD3A) ;         WRITE (FLOT, FOUT2A,
              FOR J ~ 1 STEP 1 UNTIL M DO [J, P1[J], T[J], V[J]])
         END  ;
              WRITE (FLOT, FOUT3, Q1, QM) ;
              WRITE (FLOT, HEAD4) ;
              IF NLF ! 0 THEN WRITE (FLOT, HEAD5) ;
              FOR J ~ 1 STEP 1 UNTIL NLF DO
         BEGIN
              WRITE(FLOT, FOUT4, J, LFP[J], LF[J], TLF[J], HLF[J]) ;
              WRITE (FLOT, FOUT5, FOR I ~ 1 STEP 1 UNTIL N DO
                     [NAMEA[I], NAMEB[I], XLF[J,I]])
         END  ;
              IF NVF ! 0 THEN WRITE (FLOT, HEAD6) ;
              FOR J ~ 1 STEP 1 UNTIL NVF DO
         BEGIN
              WRITE(FLOT, FOUT4, J, VFP[J], VF[J], TVF[J], HVF[J]) ;
              WRITE (FLOT, FOUT5, FOR I ~ 1 STEP 1 UNTIL N DO
                     [NAMEA[I], NAMEB[I], YVF[J,I]])
         END ;
              IF (NLD + NVD) > 0 THEN WRITE (FLOT, HEAD7) ;
              IF NLD ! 0 THEN
         BEGIN
              WRITE (FLOT, HEAD8) ;
              WRITE (FLOT, FOUT6, FOR J ~ 1 STEP 1 UNTIL NLD DO
                    [J, LDP[J], LD[J]])
         END  ;
              IF NVD ! 0 THEN
         BEGIN
              WRITE (FLOT, HEAD9) ;
              WRITE (FLOT, FOUT6, FOR J ~ 1 STEP 1 UNTIL NVD DO
                    [J, VDP[J], VD[J]])
         END  ;
              IF NQ ! 0 THEN
         BEGIN
              WRITE (FLOT, HEAD10) ;         WRITE (FLOT, HEAD11) ;
              WRITE (FLOT, FOUT6, FOR J~ 1 STEP 1 UNTIL NQ DO
                    [J, QP[J],  Q[J]])
         END
         END  ;
              GENERALDISTILLATION (PROB, DESC1, DESC2, DESC3, SPEC1,
              SPEC2, DATE1, DATE2, PCONST, RITMID, OPTION, CONDENSER,
              ITER, M, N, NLF, NVF, NLD, NVD, NQ, RA, RD, P, TOLX, TOLT,
              TOLV, LM, TM, HLM, V1, T1, HV1, Q1, QM, NAMEA, NAMEB, P1,
              T, V, LFP, LF, TLF, HLF, VFP, VF, TVF, HVF, LDP, LD, TLD,
              HLD, VDP, VD, TVD, HVD, QP, Q, XM, Y1, XLF, YVF, XLD,YVD);
         END  .
         END  ;
              GO TO START ;
THEEND  :     ;
