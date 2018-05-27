          BEGIN                                                         00000100
COMMENT        PROGRAM - FIXFORM,                                       00000200
               CUBE LIBRARY NUMBER IS V000002.                          00000300
               THIS VERSION DATED 6/20/67;                              00000400
COMMENT                                                                 00000500
TITLE:  FIXFORM                                                         00000600
DISCLAIMER:                                                             00000700
     THE AUTHOR OF THIS PROGRAM,THE CUBE ORGANIZATION,AND THE BURROUGHS 00000800
CORPORATION, BELIEVE THIS PROGRAM MATERIAL TO BE CORRECT.  HOWEVER, THEY00000900
BEAR NO RESPONSIBILITY, FINANCIAL OR OTHERWISE, FOR ERRORS RESULTING    00001000
FROM ITS USE, NOR ACCEPT ANY RESPONSIBILITY FOR ITS MAINTENANCE.        00001100
AUTHOR:  SHERRY J. SCHLUTER                                             00001200
SOURCE INSTALLATION:  FRANK J. SEILER RESEARCH LABORATORY, OFFICE OF    00001300
AEROSPACE RESEARCH, U.S. AIR FORCE ACADEMY.                             00001400
ABSTRACT:                                                               00001500
     FIXFORM ALLOWS A FORMAT STATEMENT TO BE READ AS DATA DURING EXE-   00001600
CUTION OF A PROGRAM.  FIXFORM READS IN A CARD, SCANS IT, AND ALTERS A   00001700
15 WORD FORMAT CONTAINED WITHIN THE BODY OF THE CALLING PROGRAM. A,X,I, 00001800
F,E,AND R EDITING PHRASES WILL BE ACCEPTED. E,F,AND R WILL ALL BE       00001900
TRANSLATED AS AN R EDITING PHRASE.  THE DESCRIPTION OF THE FIELD CODE   00002000
USED IS FOUND IN THE ALGOL COMPILER SEQUENCE NUMBERS 10000100 THROUGH   00002100
10006300.  THE DATA FORMAT CAN BE { 15 WORDS BUT THE DUMMY FORMAT MUST  00002200
BE AT LEAST 15 WORDS LONG.                                              00002300
SYSTEM REQUIREMENTS: MINIMUM B5500 SYSTEM                               00002400
PROCEDURE:                                                              00002500
PROCEDURE  FIXFORM(FMT)                                                 00002600
FORMAT FMT;                                                             00002700
COMMENT        INPUT FILE CARD AND OUTPUT FILE LINE MUST BE DECLARED    00002800
               GLOBAL TO THE PROCEDURE;                                 00002900
COMMENT        LABEL FINIS MUST BE DECLARED GLOBAL TO THE PROCEDURE     00003000
               BECAUSE EXIT IS MADE TO FINIS IF A FORMAT ERROR IS       00003100
               ENCOUNTERED;                                             00003200
STREAM PROCEDURE FORM(FT,CODE);                                         00003300
          BEGIN                                                         00003400
               DI~FT;                                                   00003500
               SI~CODE;                                                 00003600
               DS~15 WDS;                                               00003700
          END;                                                          00003800
LABEL RETURN,ERROR;                                                     00003900
FORMAT         F1(72A1),F2("FORMAT ERROR ENCOUNTERED");                 00004000
        ARRAY  CODE[0:14];                                              00004100
ALPHA ARRAY    F[0:71];                                                 00004200
INTEGER        I,K,REP,WIDTH,D,N;                                       00004300
INTEGER TENS,ONES,WT,WO,DEC0,DEC1,NN;                                   00004400
DEFINE    S=CODE[K].[1:1]#,                                             00004500
          C=CODE[K].[2:4]#,                                             00004600
          W=CODE[K].[6:6]#,                                             00004700
         D2=CODE[K].[16:4]#,                                            00004800
         D1=CODE[K].[20:4]#,                                            00004900
         W2=CODE[K].[24:4]#,                                            00005000
         W1=CODE[K].[28:4]#,                                            00005100
       SKIP=CODE[K].[32:6]#,                                            00005200
     REPEAT=CODE[K].[38:10]#,                                           00005300
     NUMBER=F[I]="1" OR F[I]="2" OR F[I]="3" OR F[I]="4" OR F[I]="5" OR 00005400
            F[I]="6" OR F[I]="7" OR F[I]="8" OR F[I]="9" OR F[I]="0" #, 00005500
       CHAR=F[I]="A" OR F[I]="I" OR F[I]="X" OR F[I]="E" OR F[I]="F"    00005600
               OR F[I]="R" #;                                           00005700
INTEGER TR,L1,L2,L3,L4;                                                 00005800
BOOLEAN TRUTH;                                                          00005900
DEFINE CHECK= BEGIN L1~L1+1;                                            00006000
                    L2~L2+1;                                            00006100
                    L3~L3+1;                                            00006200
                    L4~L4+1;                                            00006300
              END  # ;                                                  00006400
BOOLEAN FIRST;                                                          00006500
INTEGER R1,R2,R3,R4;                                                    00006600
               READ(CARD,F1,FOR I~0 STEP 1 UNTIL 71 DO F[I]);           00006700
               FIRST~TRUE;                                              00006800
               K~0;                                                     00006900
               TRUTH~FALSE;                                             00007000
               L1~L2~L3~L4~TR~0;                                        00007100
               FOR I~0 STEP 1 UNTIL 71 DO                               00007200
          BEGIN  LABEL NEXT;                                            00007300
IF F[I]="(" THEN                                                        00007400
                BEGIN                                                   00007500
                     S~1; SKIP~1;CODE[K].[6:26]~0;                      00007600
                     C~4;                                               00007700
                     NN~I;                                              00007800
                     IF FIRST THEN REPEAT~0                             00007900
                            ELSE                                        00008000
                     BEGIN                                              00008100
                          I~I-1;                                        00008200
                          IF NUMBER                                     00008300
                          THEN                                          00008400
                              BEGIN                                     00008500
                                   I~I-1;                               00008600
                                   IF NUMBER                            00008700
                                   THEN BEGIN                           00008800
                                             TENS~F[I];                 00008900
                                             ONES~F[I+1];               00009000
                                        END                             00009100
                                   ELSE BEGIN                           00009200
                                             TENS~0;                    00009300
                                             ONES~F[I+1];               00009400
                                        END;                            00009500
                                   REP~10|TENS+ONES;                    00009600
                                   REPEAT~REP;                          00009700
                                       SKIP~1;                          00009800
                              END                                       00009900
                          ELSE REPEAT~0;                                00010000
                          TRUTH~TRUE;                                   00010100
                          TR~TR+1;                                      00010200
                          IF TR=1 THEN BEGIN L1~0;R1~REP END;           00010300
                          IF TR=2 THEN BEGIN L2~0;R2~REP END;           00010400
                          IF TR=3 THEN BEGIN L3~0;R3~REP END;           00010500
                          IF TR=4 THEN BEGIN L4~0;R4~REP END;           00010600
                     END;                                               00010700
                          I~NN;                                         00010800
                          K~K+1;                                        00010900
                          IF TRUTH THEN CHECK;                          00011000
                          FIRST~FALSE;                                  00011100
                          GO TO NEXT;                                   00011200
                END;                                                    00011300
               IF F[I]="/" THEN                                         00011400
             BEGIN                                                      00011500
                  S~1; C~6; SKIP~1;                                     00011600
                  CODE[K].[6:26]~0; REPEAT~0;                           00011700
                  IF TRUTH THEN CHECK;                                  00011800
                  K~K+1; GO TO NEXT;                                    00011900
             END;                                                       00012000
               IF F[I]="," THEN GO TO NEXT;                             00012100
               IF F[I]=" " THEN GO TO NEXT;                             00012200
               IF F[I]=")" THEN                                         00012300
              BEGIN                                                     00012400
                   LABEL DSK;                                           00012500
                   IF NOT TRUTH THEN                                    00012600
                        BEGIN                                           00012700
                        S~1; SKIP~0;C~6;                                00012800
                        CODE[K].[6:26]~0;                               00012900
                        REPEAT~1;                                       00013000
                        K~K+1;                                          00013100
                        END;                                            00013200
                   IF TR=0 THEN GO TO DSK;                              00013300
                   IF TR=1 THEN IF R1!0 THEN GO TO DSK;                 00013400
                   IF TR=3 THEN IF R3!0 THEN GO TO DSK;                 00013500
                   IF TR=2 THEN IF R2!0 THEN GO TO DSK;                 00013600
                   IF TR=4 THEN IF R4!0 THEN GO TO DSK;                 00013700
                   S~1; SKIP~0; C~6;                                    00013800
                   CODE[K].[6:26]~0;                                    00013900
                   REPEAT~1;                                            00014000
                   K~K+1;                                               00014100
                   IF TRUTH THEN CHECK;                                 00014200
DSK:                                                                    00014300
                   S~1;C~0;SKIP~1;                                      00014400
                   CODE[K].[6:26]~0;                                    00014500
                   IF TRUTH THEN BEGIN IF TR=1 THEN REP~L1;             00014600
                                       IF TR=2 THEN REP~L2;             00014700
                                       IF TR=3 THEN REP~L3;             00014800
                                       IF TR=4 THEN REP~L4;             00014900
                                 END                                    00015000
                                ELSE BEGIN REP~K;SKIP~0; END ;          00015100
                   TR~TR-1;                                             00015200
                   IF TR=0 THEN TRUTH~FALSE;                            00015300
                   REPEAT~REP;                                          00015400
                   K~K+1;                                               00015500
                   IF TRUTH THEN CHECK;                                 00015600
                   GO TO NEXT                                           00015700
              END;                                                      00015800
               IF CHAR THEN                                             00015900
             BEGIN                                                      00016000
                  NN~I;                                                 00016100
                  I~I-1;                                                00016200
                  IF NUMBER THEN                                        00016300
                BEGIN                                                   00016400
                     LABEL IMIN;                                        00016500
                     I~I-1;                                             00016600
                     TENS~0;                                            00016700
                     IF I<0 THEN GO TO IMIN;                            00016800
                     IF NUMBER THEN                                     00016900
                   BEGIN                                                00017000
                        TENS~F[I];                                      00017100
                        ONES~F[I+1];                                    00017200
                   END ;                                                00017300
IMIN:              BEGIN                                                00017400
                        ONES~F[I+1];                                    00017500
                        REP~10|TENS+ONES;                               00017600
                   END;                                                 00017700
                   I~I+1;                                               00017800
                END ELSE REP~1;                                         00017900
                IF REP=0 THEN REP~1;                                    00018000
                I~I+2;                                                  00018100
                     IF NUMBER THEN                                     00018200
                   BEGIN                                                00018300
                        I~I+1;                                          00018400
                        IF NUMBER THEN                                  00018500
                      BEGIN                                             00018600
                           WT~F[I-1];                                   00018700
                           WO~F[I];                                     00018800
                           WIDTH~10|WT+WO;                              00018900
                      END                                               00019000
                      ELSE                                              00019100
                      BEGIN                                             00019200
                           WO~F[I-1];                                   00019300
                          WIDTH~WO;                                     00019400
                           I~I-1;                                       00019500
                      END                                               00019600
                   END                                                  00019700
                   ELSE                                                 00019800
                        GO TO ERROR;                                    00019900
                        I~I+1;                                          00020000
                     IF F[I]="." THEN                                   00020100
                   BEGIN                                                00020200
                        I~I+1;                                          00020300
                        IF NUMBER THEN                                  00020400
                      BEGIN                                             00020500
                           I~I+1;                                       00020600
                           IF NUMBER THEN                               00020700
                         BEGIN                                          00020800
                              DEC0~F[I-1];                              00020900
                              DEC1~F[I];                                00021000
                              D~10|DEC0+DEC1;                           00021100
                         END                                            00021200
                         ELSE                                           00021300
                         BEGIN                                          00021400
                              DEC0~0;                                   00021500
                              DEC1~F[I-1];                              00021600
                              D~DEC1;                                   00021700
                         END;                                           00021800
                      END                                               00021900
                      ELSE GO TO ERROR;                                 00022000
                   END;                                                 00022100
                     I~NN;                                              00022200
                     IF F[I]="X" THEN                                   00022300
                   BEGIN                                                00022400
                        REPEAT~REP;                                     00022500
                        WO~WIDTH MOD 64;                                00022600
                        W~WO;                                           00022700
                        SKIP~WO;                                        00022800
                        W1~WIDTH DIV 64;                                00022900
                        W2~0;                                           00023000
                        D1~0;                                           00023100
                        D2~0;                                           00023200
                        S~0;                                            00023300
                        C~2;                                            00023400
                        K~K+1;                                          00023500
                        IF TRUTH THEN CHECK;                            00023600
                        GO TO NEXT;                                     00023700
                   END;                                                 00023800
                     IF F[I]="A" THEN                                   00023900
                   BEGIN                                                00024000
                        S~0;                                            00024100
                        C~4;                                            00024200
                        REPEAT~REP;                                     00024300
                        W2~0;                                           00024400
                        D1~0;                                           00024500
                        D2~0;                                           00024600
                        W~WIDTH;                                        00024700
                        IF WIDTH<6 THEN                                 00024800
                      BEGIN                                             00024900
                           W1~WIDTH;                                    00025000
                           SKIP~0;                                      00025100
                      END                                               00025200
                      ELSE                                              00025300
                      BEGIN                                             00025400
                           W1~6;                                        00025500
                           SKIP~WIDTH-6;                                00025600
                      END;                                              00025700
                        K~K+1;                                          00025800
                        IF TRUTH THEN CHECK;                            00025900
                        GO TO NEXT;                                     00026000
                   END;                                                 00026100
                     IF F[I]="I" THEN                                   00026200
                   BEGIN                                                00026300
                        S~0;                                            00026400
                        C~6;                                            00026500
                        REPEAT~REP;                                     00026600
                        W~WIDTH;                                        00026700
                        D1~0;                                           00026800
                        D2~0;                                           00026900
                        IF WIDTH>16 THEN SKIP~WIDTH-16                  00027000
                                    ELSE SKIP~0;                        00027100
                        IF WIDTH<8 THEN                                 00027200
                      BEGIN                                             00027300
                           W1~WIDTH;                                    00027400
                           W2~0;                                        00027500
                      END                                               00027600
                      ELSE                                              00027700
                      BEGIN                                             00027800
                           W1~8;                                        00027900
                           W2~WIDTH-SKIP-8;                             00028000
                      END;                                              00028100
                        K~K+1;                                          00028200
                        IF TRUTH THEN CHECK;                            00028300
                        GO TO NEXT;                                     00028400
                   END;                                                 00028500
                    IF F[I]="F" OR F[I]="E" OR F[I]="R" THEN            00028600
                   BEGIN                                                00028700
                        S~0;                                            00028800
                       C~15;                                            00028900
                        REPEAT~REP;                                     00029000
                        W~WIDTH;                                        00029100
                        IF D>16 THEN GO TO ERROR;                       00029200
                       IF D{8 THEN                                      00029300
                      BEGIN                                             00029400
                           D1~D;                                        00029500
                           D2~0;                                        00029600
                      END                                               00029700
                      ELSE                                              00029800
                      BEGIN                                             00029900
                           D1~8;                                        00030000
                           D2~D-8;                                      00030100
                      END;                                              00030200
                        IF (N~WIDTH-D-1)>16 THEN SKIP~WIDTH-D-17        00030300
                                            ELSE SKIP~0;                00030400
                        IF N>8 THEN                                     00030500
                      BEGIN                                             00030600
                           W1~8;                                        00030700
                           W2~N-8-SKIP;                                 00030800
                      END                                               00030900
                      ELSE                                              00031000
                      BEGIN                                             00031100
                           W1~N;                                        00031200
                           W2~0;                                        00031300
                      END;                                              00031400
                        K~K+1;                                          00031500
                        IF TRUTH THEN CHECK;                            00031600
                        GO TO NEXT;                                     00031700
                  END;                                                  00031800
             END;                                                       00031900
NEXT:     END;                                                          00032000
               FORM(FMT,CODE[0]);                                       00032100
               GO TO RETURN;                                            00032200
ERROR:         WRITE(LINE,F2);                                          00032300
               GO TO FINIS;                                             00032400
RETURN:   END OF FIXFORM;                                               00032500
                                                                        00032600
          BEGIN                                                         00032700
FILE IN CARD(1,10);                                                     00032800
FILE OUT       LINE 4(1,15);                                            00032900
LABEL          START,FINIS;                                             00033000
COMMENT        THIS IS THE DUMMY FORMAT.  IT IS 18 WORDS LONG;          00033100
FORMAT         FMT(X10,A1,X9,A2,X8,A3,X7,A4,X6,X5,X5,A6,X4,X3,5A3);     00033200
ARRAY X[0:10];                                                          00033300
INTEGER I,J;                                                            00033400
FORMAT F1(F10.2);                                                       00033500
COMMENT        INSERT FIXFORM AT THIS POINT;                            00033600
               J~0;                                                     00033700
               FIXFORM(FMT);                                            00033800
               READ(CARD,FMT,FOR I~0 STEP 1 UNTIL 10 DO X[I]);          00033900
START:         FIXFORM(FMT);                                            00034000
               J~J+1;                                                   00034100
               WRITE(LINE,FMT,FOR I~0 STEP 1 UNTIL 10 DO X[I]);         00034200
               IF J{5 THEN GO TO START;                                 00034300
FINIS:    END.                                                          00034400
