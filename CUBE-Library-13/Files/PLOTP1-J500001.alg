COMMENT        PROCEDURE - PLOT,                                        00000100
               CUBE LIBRARY NUMBER IS J500001.                          00000200
               THIS VERSION DATED 5/31/67;                              00000300
PROCEDURE      PLOT (X, Y, ID, NW,   XSC, YSC, XID, YID, PLOTTER);      00000400
               VALUE XID, YID;                                          00000500
                             REAL XSC, YSC, XID, YID;                   00000600
               ARRAY X[0], Y[0], ID[0], NW[0];                          00000700
               FILE PLOTTER;                                            00000800
          BEGIN                                                         00000900
COMMENT        X[0] AND Y[0] ARE RESERVED FOR INPUT OF MINIMUM SCALE    00001000
               VALUES, WHERE X AND Y ARE VECTORS CONTAINING PLOT VALUES.00001100
               ID IS VECTOR CONTAINING ALPHA DESCRIPTION OF PLOT.       00001200
               NW IS 4 WORD ARRAY WITH LOWER BOUND OF 0 WHERE           00001300
                   0 = NUMBER OF X, Y VALUES IN X, Y ARRAYS             00001400
                   1 = NUMBER OF WORDS IN ID ARRAY (ID IS PRINTED ALPHA)00001500
                   2 = CHARACTER TO BE USED AS PLOT SYMBOL              00001600
                   3 = 0 IF PLOT IS TO BE CLEARED AND PRINTED           00001700
                   3 = 1 IF PLOT NOT TO BE PRINTED ON THIS CALL - RETAIN00001800
                   3 = 2 IF PLOT NOT TO BE CLEARED OR PRINTED THIS CALL 00001900
                   3 = 3 IF PLOT NOT TO BE CLEARED BUT IS TO BE PRINTED 00002000
               XSC AND YSC ARE SCALE VALUES FOR X AND Y, IF ZERO SCALES 00002100
                   WILL BE CALCULATED.                                  00002200
               XID AND YID ARE ONE WORD ALPHA AXIS IDENTIFICATION.      00002300
               PLOTTER IS FILE WHERE PLOT IS TO BE WRITTEN ;            00002400
OWN REAL ARRAY P[0:40, 0:17];                                           00002500
LABEL          EXIT;                                                    00002600
REAL           SYM, VID, V1, A, B;                                      00002700
INTEGER        I, J, K, L, M, N;                                        00002800
FORMAT         F1 (20A6);                                               00002900
FORMAT OUT     F3A (A1, X9,"-", 18A6);                                  00003000
FORMAT OUT     F3B (A1, R8.1, X1, "I", 18A6);                           00003100
FORMAT         F5 (X20, "SCALE FACTOR FOR ",A1, " = 10 TO THE ", I3,    00003200
                   " POWER");                                           00003300
FORMAT OUT     F6 (X11, 21("I----"));                                   00003400
FORMAT OUT     F8 (X11, 21(I4, X1));                                    00003500
FORMAT OUT     F10 (X50, A6);                                           00003600
LIST           L1 (FOR K ~ 0 STEP 1 UNTIL NW[1] DO ID[K]);              00003700
LIST           L3A(VID,          FOR K ~ 0 STEP 1 UNTIL 17 DO P[I,K]);  00003800
LIST           L3B(VID, V1,      FOR K ~ 0 STEP 1 UNTIL 17 DO P[I,K]);  00003900
LIST           L8 (FOR K ~ 0 STEP 5 UNTIL 100 DO (K | XSC + X[0])| B);  00004000
PROCEDURE      SCALE (VA, NV, SC);                                      00004100
               VALUE NV; INTEGER NV; ARRAY VA[0]; REAL SC;              00004200
          BEGIN                                                         00004300
REAL           MIN, MAX, RG;                                            00004400
REAL           I, J;                                                    00004500
INTEGER        K;                                                       00004600
               MIN ~ VA[1]; MAX ~ VA[1];                                00004700
               FOR K ~ 2 STEP 1 UNTIL NV DO                             00004800
          BEGIN                                                         00004900
               IF VA[K] < MIN THEN MIN ~ VA[K];                         00005000
               IF VA[K] > MAX THEN MAX ~ VA[K];                         00005100
          END;                                                          00005200
               J ~ 0;                                                   00005300
               IF ABS(MIN) > 10 THEN                                    00005400
          BEGIN                                                         00005500
               DO J ~ J + 1 UNTIL 10*J > ABS(MIN);                      00005600
               J ~ 10 * (J-1);                                          00005700
          END  ELSE                                                     00005800
               IF ABS(MIN) < 1 AND ABS(MIN) ! 0 THEN                    00005900
          BEGIN                                                         00006000
               DO J ~ J - 1 UNTIL 10*J < ABS(MIN);                      00006100
               IF J ! -1 THEN                                           00006200
               J ~ 10 * (J+1) ELSE J ~ 0.1;                             00006300
          END  ELSE                                                     00006400
               J ~ 1;                                                   00006500
               MIN ~ ENTIER(MIN/J) | J;                                 00006600
               IF VA[0] ! 0 AND VA[0] < MIN THEN MIN ~ VA[0];           00006700
               RG ~ MAX - MIN;  J ~ -1;                                 00006800
               IF RG > 10 THEN                                          00006900
               DO J ~ J + 1 UNTIL (I~10*J) } (RG / SC) ELSE             00007000
               IF RG / SC ! 0 THEN                                      00007100
          BEGIN                                                         00007200
               I ~ 0.1;                                                 00007300
               IF I > (RG/SC) THEN                                      00007400
               DO I ~ I | 0.1 UNTIL I { RG / SC;                        00007500
               I ~ I | 10;                                              00007600
          END ELSE                                                      00007700
               I ~ 10;                                                  00007800
               IF I } 1 AND I | 0.5 > (RG/SC) THEN I ~ I | 0.5;         00007900
               IF I < 1 AND I | 5 < (RG/SC) THEN I ~ I | 5;             00008000
               SC ~ I; VA[0] ~ MIN;                                     00008100
          END;                                                          00008200
ALPHA STREAM PROCEDURE CKPT (W, WP);                                    00008300
          BEGIN                                                         00008400
               LOCAL AD;                                                00008500
               SI ~ WP; DI ~ LOC AD; DS ~ WDS; SI ~ W; SI ~ SI + 2;     00008600
               SI ~ SI + AD; DI ~ LOC CKPT; DS ~ 7 LIT "0000000";       00008700
               DS ~ 1 CHR;                                              00008800
          END;                                                          00008900
STREAM PROCEDURE PT (W, WP, SYM);                                       00009000
          BEGIN                                                         00009100
LOCAL          AD;                                                      00009200
               SI ~ WP;  DI ~ LOC AD; DS~WDS; DI~W; DI~ DI+2; DI~DI+AD; 00009300
               SI ~ SYM; SI ~ SI + 7; DS ~ 1 CHR;                       00009400
          END;                                                          00009500
PROCEDURE      SCPT (SC, MIN, LB, UB, NP, SV);                          00009600
               VALUE SC, MIN, LB, UB, NP;                               00009700
               REAL SC, MIN, LB, UB;                                    00009800
               INTEGER NP, SV;                                          00009900
          BEGIN                                                         00010000
COMMENT        THIS PROCEDURE CLCULATES 10 TO X POWER FOR PRINTING      00010100
               AXIS VALUES;                                             00010200
               SV ~0;                                                   00010300
               IF SC | NP + MIN > UB THEN                               00010400
               DO SV~SV+1 UNTIL (SC | NP + MIN) | 10 * (-SV) { UB;      00010500
               IF SC < LB THEN                                          00010600
          BEGIN                                                         00010700
               IF SC | 5 < LB THEN                                      00010800
               DO SV ~ SV - 1 UNTIL (SC | 5) | 10 * (-SV) } LB;         00010900
               IF SC | 10 * (-SV) -ENTIER (SC | 10 * (-SV)) < LB THEN   00011000
               SV ~ SV - 1;                                             00011100
          END;                                                          00011200
          END;                                                          00011300
                                                                        00011400
ALPHA STREAM PROCEDURE CHT (W, C);                                      00011500
          BEGIN                                                         00011600
               LOCAL C1;                                                00011700
               DI ~ LOC C1; SI ~ C; DS ~ WDS;                           00011800
               DI ~ LOC CHT; DI ~ DI + 7; SI ~ W; SI ~ SI + C1;         00011900
               DS ~ 1 CHR;                                              00012000
          END;                                                          00012100
               N ~ NW[0];                                               00012200
               IF NW[3] = 0 OR  NW[3] = 3 THEN                          00012300
               WRITE (PLOTTER[PAGE]);                                   00012400
               IF NW[3] = 0 OR NW[3] = 1 THEN                           00012500
          BEGIN                                                         00012600
               FOR I ~ 0 STEP 1 UNTIL 40 DO                             00012700
               FOR K ~ 0 STEP 1 UNTIL 17 DO  P[I,K] ~ "      ";         00012800
          END;                                                          00012900
               IF XSC = 0 THEN                                          00013000
          BEGIN                                                         00013100
               XSC ~ 100; SCALE (X, N, XSC);                            00013200
          END;                                                          00013300
               IF YSC = 0 THEN                                          00013400
          BEGIN                                                         00013500
               YSC ~ 40; SCALE (Y, N, YSC);                             00013600
          END;                                                          00013700
               FOR I ~ 1 STEP 1 UNTIL N DO                              00013800
          BEGIN                                                         00013900
               K ~ (X[I] - X[0]) / XSC;                                 00014000
               L ~ (Y[I] - Y[0]) / YSC;                                 00014100
               M ~ K MOD 6;                                             00014200
               K ~ K DIV 6;                                             00014300
               IF L } 0 AND L{ 40 AND K } 0 AND K { 17 AND M } 0 THEN   00014400
          BEGIN                                                         00014500
               V1 ~ CKPT (P[L,K], M);                                   00014600
               IF V1 = " " THEN SYM ~ NW[2] ELSE                        00014700
               IF V1 < 7 THEN SYM ~ V1 + 1 ELSE                         00014800
               SYM ~ 2;                                                 00014900
               PT (P[L,K], M, SYM);                                     00015000
          END  ELSE                                                     00015100
          BEGIN                                                         00015200
               M ~ 0; SYM ~ "~";                                        00015300
               PT (P[0,0], M, SYM);                                     00015400
          END;                                                          00015500
          END;                                                          00015600
               IF NW[3] = 1 OR NW[3] = 2 THEN GO TO EXIT;               00015700
               WRITE (PLOTTER[DBL], F1, L1);                            00015800
               SCPT (YSC, Y[0], 0.1, 8@5, 40, M);                       00015900
               SCPT (XSC, X[0], 1.0, 9@3, 100, N);                      00016000
               WRITE (PLOTTER, F5, "Y", M);                             00016100
               WRITE (PLOTTER[DBL], F5, "X", N);                        00016200
               IF M = 0 THEN A ~ 1 ELSE A ~ 10 * (-M);                  00016300
               IF N = 0 THEN B ~ 1 ELSE B ~ 10 * (-N);                  00016400
               FOR I ~ 40 STEP -1 UNTIL 0 DO                            00016500
          BEGIN                                                         00016600
               IF    I < 31 AND I > 24 THEN                             00016700
          BEGIN                                                         00016800
               L ~ 32 - I;                                              00016900
               VID ~ CHT (YID, L);                                      00017000
          END  ELSE                                                     00017100
               VID ~ " ";                                               00017200
                                                                        00017300
               IF I MOD 5 = 0 THEN                                      00017400
          BEGIN                                                         00017500
               V1 ~ (I | YSC + Y[0] ) | A;                              00017600
               WRITE (PLOTTER, F3B, L3B);                               00017700
          END  ELSE                                                     00017800
               WRITE (PLOTTER, F3A, L3A);                               00017900
          END;                                                          00018000
               WRITE (PLOTTER, F6);                                     00018100
               WRITE (PLOTTER, F8, L8);                                 00018200
               WRITE (PLOTTER, F10, XID);                               00018300
EXIT:                                                                   00018400
          END;                                                          00018500
