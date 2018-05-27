          BEGIN                                                         00000100
COMMENT        CUBE LIBRARY NUMBER IS H500011.  THE PROGRAM NAME IS     00000200
               "ECONSIM/TTY".  THIS VERSION DATED 5/3/68;               00000300
INTEGER        I,L,M,N,QUAR,U,K1,K3,K4,K;                               00000400
INTEGER        STA,NTEAMS,NCHAR,FF,II;                                  00000500
ALPHA          AAA1,AAA2;                                               00000600
ALPHA          IDNAME,FIRSTQ,ID2;                                       00000700
FILE IN        ES1 DISK SERIAL "FIRST" "ECSIM" (2,10,30);               00000800
FILE           ES2 DISK SERIAL "AAAAAAA" "ECSIM" (2,10,30);             00000900
FILE OUT      ES3 DISK SERIAL [1:13] "AAAAAAA" "ECSIM" (2,10,30,SAVE 1);00001000
ARRAY          TRA[0:9];                                                00001100
ALPHA FILE IN  TTIN 14(2,8);                                            00001200
ALPHA FILE OUT TTOUT 14(2,8);                                           00001300
REAL           Q,ZA,PA,DA,COUNT,YD,JSTAR;                               00001400
LABEL          DUNN;                                                    00001500
DEFINE         CARD=TTIN(STA)#,PRINT=TTOUT(STA)#;                       00001600
SWITCH FILE    SFES~ES1,ES2,ES3;                                        00001700
BOOLEAN        PCS;                                                     00001800
ARRAY          B[1:30],Y[1:40],A[ 1:4,1:10],R[1:4,1:10],J,C,T,W,P,PI,   00001900
               DELC,DIVIDENDS,D,Z,CASH[1:10];                           00002000
LIST           DETAILA (I,FOR U ~ 1 STEP 1 UNTIL 11 DO B[U]);           00002100
LIST           MASTER (N,QUAR,Q,FOR U ~ 1 STEP 1 UNTIL 40 DO Y[U]),     00002200
               LINE1 (I,QUAR),LINE2 (K1),LINE3 (B[2]), LINE4 (K3),      00002300
               DETAILB (I,FOR U ~ 1 STEP 1 UNTIL 6 DO B[U]);            00002400
LIST           LINE5 (K4),LINE7 (B[5]),LINE9 (B[6]), LINE10(B[7]),      00002500
               LINE11 (B[8]),LINE12(B[9]),LINE13(B[10]),LINE14(B[11]);  00002600
LIST           LINE15 (B[12],B[13]),LINE16(B[14]),LINE17(B[15]),        00002700
               LINE18 (B[16]),LINE19(B[17]),LINE20(B[18]),LINE22(B[19]);00002800
LIST           LINE24 (B[20]),LINE25(B[21]),LINE26(B[22]),              00002900
               LINE27 (B[23],B[24]),LINE28(B[25]),LINE31(B[26]);        00003000
LIST           LINE32 (B[27]),LINE33(B[28]),LINE34(B[29]);              00003100
LIST           S1 (I),S2(CASH[I]),S3(T[I]),S4(C[I]),                    00003200
               S5 (CASH[I]+T[I]+C[I]),CD1(I,A[2,I],A[3,I],A[4,I],       00003300
               R[2,I],R[3,I],R[4,I],T[I],C[I],J[I],W[I],CASH[I]);       00003400
LIST           CRD1 (N,QUAR,Q/N),                                       00003500
               CRD2 (Y[I],Y[I+1],Y[I+2],Y[I+3],Y[I+4],Y[I+5],Y[I+6],    00003600
               Y[I+7],Y[I+8],Y[I+9],Y[I+10],Y[I+11],Y[I+12],Y[I+13],    00003700
               Y[I+14],Y[I+15],Y[I+16],Y[I+17],Y[I+18],Y[I+19] );       00003800
FORMAT IN      IN1 (2I2,I8/20I4/20I4),                                  00003900
               INA (I2,6I7,I9,2I8,I2,I9),                               00004000
               INB(X8,I2,X1,F5.2,X1,4(I7,X1),I7);                       00004100
FORMAT IN      F1(X8,A3),  F2(X8,A1,A6), F3(X8,I1);                     00004200
FORMAT OUT     TITLE(X15,"{!O P E R A T I N G  S T A T E M E N T{!~"),  00004300
               REPORT1(X14,"{!FIRM",I4,X13,"PERIOD",I4,"{!~"),          00004400
               REPORT2(X14,"{!SALES VOLUME",X20,I10,"{!~"),             00004500
              REPORT3(X14,"{!PERCENT SHARE OF INDUSTRY SALES"F11.2,     00004600
                      "{!~"),                                           00004700
               REPORT4(X14,"CURRENT INVENTORY QUANTITY",   I10,"{!~"),  00004800
               REPORT5(X14,"PRODUCTION CPY. NEXT QUARTER",   I8 ,"{!~"),00004900
               REPORT6(X26,"{!PROFIT AND LOSS!{~"),                     00005000
               REPORT7(X8,"!I N C O M E.  SALES REVENUE",X17,"$",F9.0,  00005100
                         "~"),                                          00005200
               REPORT8(X9,"{!!EXPENSE{!~"),                             00005300
               REPORT9(X11,"MANUFACTURING COSTS",X10,"$",F9.0,"{!~"),   00005400
               REPORT10(X11,"REDUCTION IN INVENTORY VALUE",X2,F9.0,     00005500
                         "{!~"),                                        00005600
               REPORT11(X11,"ADMINISTRATION AND SELLING",X4,F9.0,"{!~"),00005700
               REPORT12(X11,"ADVERTISING",X19,F9.0,"{!~"),              00005800
               REPORT13(X11,"RESEARCH AND DEVELOPMENT",X6 ,F9.0,"{!~"), 00005900
               REPORT14(X11,"DEPRECIATION",X18,F9.0,"{!~"),             00006000
               REPORT15(X11,"MISCELLANEOUS",X17,F9.0,X2,"$",F9.0,"~"),  00006100
               REPORT16(X9,"PROFIT BEFORE INCOME TAX",X19,"$",F9.0"~"), 00006200
               REPORT17(X9,"ADDITION TO INCOME TAX FUND",X16,"$",F9.0,  00006300
                            "~"),                                       00006400
               REPORT18(     X9,"NET PROFIT AFTER INCOME TAX",X16,"$",  00006500
                   F9.0,"~"),                                           00006600
               REPORT19(X9,"DIVIDENDS PAID",X29"$",F9.0,"~"),           00006700
               REPORT20(X9,"ADDITIONS TO OWNERS EQUITY",X17,"$",F9.0,   00006800
                       "~");                                            00006900
FORMAT OUT     REPORT21(       X10,"RECEIPTS AND DISBURSEMENTS{!!!~"),  00007000
               REPORT22(X9,"RECEIPTS. SALES REVENUE",X20"$",            00007100
                      F9.0,"~"),                                        00007200
               REPORT23(X9,"D I S B U R S E M E N T S{!~");             00007300
FORMAT OUT     REPORT24(X9,"CASH EXPENSE",X19,"$",F9.0,"{!~"),          00007400
               REPORT25(X9,"ADDITION TO INCOME TAX FUND",X5,F9.0,"{!~"),00007500
               REPORT26(X11,"DIVIDENDS PAID",X16,F9.0,"{!~");           00007600
FORMAT OUT     REPORT27(X11,"INVESTMENT IN PLANT",X11,F9.0,X2,"$",F9.0, 00007700
                     "~"),                                              00007800
               REPORT28(X9,"ADDITIONS TO CASH ASSETS",X19,"$",F9.0"~"), 00007900
               REPORT29("!",X15,"FINANCIAL CONDITION{!!~");             00008000
FORMAT OUT     REPORT30(X9,"ASSETS{!~"),                                00008100
               REPORT31(X11,"NET CASH ASSETS",X26,"$",F9.0,"~"),        00008200
               REPORT32(X11,"INVENTORY VALUE",X26,"$",F9.0,"~");        00008300
FORMAT OUT     REPORT33(X11,"PLANT, NET BOOK VALUE",X20,"$",F9.0,"~"),  00008400
               REPORT34(X9,"O W N E R S  E Q U I T Y",X19,"$",F9.0"~"), 00008500
              F101(X8,"{!40 QUARTERS HAVE BEEN RUN. THE GAME IS OVER~");00008600
FORMAT OUT     FAG(X8,"{!YOU DID NOT TYPE THE CORRECT RESPONSE.",       00008700
                     " TRY AGAIN{!~"),                                  00008800
               FLX(X8,"{!REQUESTED FILE NOT ON THE DISK.{!~"),          00008900
               FLY(X8,"CHECK YOUR SPELLING AND TRY AGAIN{!~"),          00009000
         FPNO(X8,"{!NOTIFY COMP OPER FILE FIRST/ECSIM NOT ON DISK~"),   00009100
               FPMO(X8,"{!THE PROGRAM MUST BE RESTARTED{!~"),           00009200
               FENT(X8,"{!TT CC.CC NNNNNNN AAAAAAA RRRRRRR ",           00009300
                    "MMMMMMM DDDDDDD{!~"),                              00009400
               FENTER(X8,"{!~"),                                        00009500
           FSTART(X8,"{!THIS IS THE START OF THE ECONOMIC SIMULATOR~"), 00009600
        GSTART(X8,"{!IF YOU ARE STARTING WITH QUARTER ONE TYPE YES~"),  00009700
           HSTART(X8,"{!IF NOT TYPE NO{!~"),                            00009800
               FPN(X8,"{!ENTER YOUR ASSIGNED PROGRAM NAME{!~"),         00009900
               FNT(X8,"{!NUMBER OF TEAMS = ~");                         00010000
FORMAT OUT     SUM1 (X34,"FIRM",I4,"{!~");                              00010100
FORMAT OUT     MS1 (2I2,I8,X68),                                        00010200
               MS2 (20I4);                                              00010300
FORMAT OUT     PUN1 (I2,6I7,I9,2I8,I2,I9);                              00010400
LABEL          LFQ,LOQ,L1,L2;                                           00010500
ARRAY          TBUF[0:7],SEA[0:8];                                      00010600
INTEGER STREAM PROCEDURE FINDGP(A);                                     00010700
          BEGIN                                                         00010800
LABEL          FOUND;                                                   00010900
               SI~A; 60(IF SC="~" THEN JUMP OUT 1 TO FOUND; SI~SI+1;    00011000
               TALLY~TALLY+1);                                          00011100
FOUND:         FINDGP~TALLY;                                            00011200
          END  OF FINDGP;                                               00011300
               STA~STATUS(TBUF[*]);                                     00011400
               STA.[9:9]~TBUF[0].[9:9];                                 00011500
               WRITE(TTOUT(STA),FSTART); WRITE(PRINT,GSTART);           00011600
               WRITE(TTOUT(STA),HSTART);                                00011700
L1:            READ(TTIN(STA),F1,FIRSTQ);                               00011800
               IF FIRSTQ="YES" THEN                                     00011900
               GO TO LFQ;                                               00012000
               IF FIRSTQ.[30:12]="NO" THEN GO TO LOQ;                   00012100
               WRITE(TTOUT(STA),FAG);                                   00012200
               GO TO L1;                                                00012300
LFQ:           WRITE(TTOUT(STA),FNT);                                   00012400
               READ(TTIN(STA),F3,NTEAMS);                               00012500
LOQ:           WRITE(TTOUT(STA),FPN);                                   00012600
L2:            READ(TTIN(STA),8,TBUF[*]);                               00012700
               NCHAR~FINDGP(TBUF[1]);                                   00012800
               READ(TBUF[*],F2   ,  AAA1,AAA2);                         00012900
               IDNAME.[6:6]~AAA1.[42:6]; IDNAME.[12:36]~AAA2.[12:36];   00013000
               IF NCHAR<7 THEN                                          00013100
          BEGIN                                                         00013200
               IF IDNAME.[12:6]="~" THEN IDNAME.[12:36]~"      " ELSE   00013300
               IF IDNAME.[18:6]="~" THEN IDNAME.[18:30]~"     " ELSE    00013400
               IF IDNAME.[24:6]="~" THEN IDNAME.[24:24]~"    " ELSE     00013500
               IF IDNAME.[30:6]="~" THEN IDNAME.[30:18]~"   " ELSE      00013600
               IF IDNAME.[36:6]="~" THEN IDNAME.[36:12]~"  " ELSE       00013700
               IF IDNAME.[42:6]="~" THEN IDNAME.[42:6]~" ";             00013800
          END;                                                          00013900
               FILL ES2 WITH IDNAME,*;                                  00014000
               FILL ES3 WITH IDNAME,*;                                  00014100
               IF FIRSTQ="YES" THEN                                     00014200
          BEGIN                                                         00014300
               SEARCH(ES1,SEA[*]);                                      00014400
               IF SEA[0]<2 THEN                                         00014500
          BEGIN                                                         00014600
               WRITE(PRINT,FPNO);                                       00014700
               WRITE(PRINT,FPMO);                                       00014800
               GO TO DUNN;                                              00014900
          END;                                                          00015000
          END  ELSE                                                     00015100
          BEGIN                                                         00015200
               SEARCH(ES3,SEA[*]);                                      00015300
               IF SEA[0]<2 THEN                                         00015400
          BEGIN                                                         00015500
               WRITE(PRINT,FLX);                                        00015600
               WRITE(PRINT,FLY);                                        00015700
               GO TO L2;                                                00015800
          END;                                                          00015900
          END;                                                          00016000
          BEGIN                                                         00016100
LABEL          LOOP,START,RD,BOTTOM,COMP,BLK1,L1,L2,ND,RUN,PCH;         00016200
               IF FIRSTQ="YES" THEN NCHAR~0 ELSE NCHAR~1;               00016300
RUN:           READ(SFES[NCHAR],IN1,MASTER);                            00016400
               IF FIRSTQ="YES" THEN N~NTEAMS;                           00016500
               Q ~ Q|N;                                                 00016600
               FOR M ~ 1 STEP 1 UNTIL N DO                              00016700
          BEGIN                                                         00016800
LOOP:          READ(SFES[NCHAR],INA,DETAILA);                           00016900
               IF I ! M THEN                                            00017000
          BEGIN                                                         00017100
               GO TO LOOP;                                              00017200
          END;                                                          00017300
               FOR U ~ 1 STEP 1 UNTIL 3 DO                              00017400
          BEGIN                                                         00017500
               A [U+1,I] ~ B[U];                                        00017600
               R [U+1,I] ~ B[U+3];                                      00017700
          END;                                                          00017800
               T[I] ~ B[7]; C[I] ~ B[8];                                00017900
               J[I] ~ B[9]; W[I] ~ B[10];                               00018000
               CASH[I] ~ B[11];                                         00018100
          END;                                                          00018200
              IF FIRSTQ!"YES" THEN FF~1 ELSE FF~3;                      00018300
START:                                                                  00018400
               IF FIRSTQ="YES" AND  FF!1 THEN                           00018500
          BEGIN                                                         00018600
               FF~2;   GO TO PCH;                                       00018700
          END;                                                          00018800
               IF PCS THEN GO TO PCH;                                   00018900
               WRITE(TTOUT(STA),FENT);                                  00019000
               FOR M ~ 1 STEP 1 UNTIL N DO                              00019100
          BEGIN                                                         00019200
               IF W[M] = 1 THEN GO TO BOTTOM;                           00019300
               WRITE(TTOUT(STA),FENTER);                                00019400
RD:            READ (CARD,INB,DETAILB);                                 00019500
               IF I ! M THEN                                            00019600
          BEGIN                                                         00019700
               GO TO RD;                                                00019800
          END;                                                          00019900
               P[I] ~ B[1]; PI[I] ~ B[2];                               00020000
               A[1,I] ~ B[3]; R[1,I] ~ B[4];                            00020100
               DELC[I] ~ B[5];                                          00020200
               DIVIDENDS [I] ~ B[6];                                    00020300
BOTTOM:                                                                 00020400
          END;                                                          00020500
               FOR I~0 STEP 1 UNTIL 4 DO WRITE(PRINT,FENTER);           00020600
COMP:          ZA ~ PA ~ DA ~ COUNT ~ 0;                                00020700
               FOR I ~ 1 STEP 1 UNTIL N DO                              00020800
          BEGIN                                                         00020900
               IF W[I] = 1 THEN GO TO ND;                               00021000
               Z[I] ~ 2@-6|(1.00|(.18|A[1,I]+.12|A[2,I]+.06|A[3,I]      00021100
               +.03|A[4,I])+                                            00021200
               .06|R[1,I]+.12|R[2,I]+.25|R[3,I]+.18|R[4,I]);            00021300
               ZA ~ ZA+Z[I];PA ~ PA+1/P[I];                             00021400
               D[I] ~3|Z[I]|(10-P[I])/(2|(2|Z[I]+1));                   00021500
               DA~ DA+D[I]; COUNT~ COUNT+1;                             00021600
          END;                                                          00021700
               PA~ COUNT/PA;                                            00021800
               YD~ (3|ZA|(10-PA)/(2|(2|ZA+1)))|4@3|Y[QUAR];             00021900
               Q ~ .5|(Q+(YD|COUNT/4));                                 00022000
               FOR I ~ 1 STEP 1 UNTIL N DO                              00022100
          BEGIN                                                         00022200
               B[2] ~ D[I]/DA;                                          00022300
               JSTAR ~ J[I]+PI[I];                                      00022400
               IF B[2]|Q < JSTAR THEN                                   00022500
          BEGIN                                                         00022600
               B[1]~B[2]|Q;GO TO BLK1;                                  00022700
          END;                                                          00022800
               B[1]~JSTAR;                                              00022900
BLK1:          B[3]~JSTAR-B[1];                                         00023000
               B[28]~.975|C[I]+DELC[I];                                 00023100
               B[5] ~P[I]|B[1];                                         00023200
               B[4] ~.05|B[28];                                         00023300
               K1 ~B[1];K3 ~B[3];K4 ~B[4];                              00023400
               IF PI[I]< .05|C[I] THEN                                  00023500
          BEGIN                                                         00023600
               B[6]~3|PI[I];GO TO L1;                                   00023700
          END;                                                          00023800
               B[6]~ PI[I]|(2+PI[I]/B[4]);                              00023900
L1:            IF J[I] < B[1] THEN                                      00024000
          BEGIN                                                         00024100
               B[27]~B[6]|(1-(B[1]-J[I])/PI[I]);GO TO L2;               00024200
          END;                                                          00024300
               B[27]~B[6]+(T[I]|(J[I]-B[1]))/J[I];                      00024400
L2:            B[7]~T[I]-B[27];                                         00024500
               B[8]~ 200000+.035|B[28];                                 00024600
               B[9]~ A[1,I];                                            00024700
               B[10]~R[1,I];                                            00024800
               B[11]~.025|C[I];                                         00024900
               B[12]~.05|B[27];                                         00025000
               B[13]~0;                                                 00025100
               FOR K ~6 STEP 1 UNTIL 12 DO                              00025200
               B[13] ~ B[13]+B[K];                                      00025300
               B[14] ~ B[5]-B[13];                                      00025400
               B[15] ~ .52|B[14];                                       00025500
               B[16] ~ B[14]-B[15];                                     00025600
               B[17] ~ DIVIDENDS[I];                                    00025700
               B[18]~ B[16]-B[17];                                      00025800
               B[19]~ B[5];                                             00025900
               B[20]~ B[13]-B[7]-B[11];                                 00026000
               B[21]~ B[15];                                            00026100
               B[22]~ B[17];                                            00026200
               B[23]~ DELC[I];                                          00026300
               B[24]~ 0;                                                00026400
               FOR K ~ 20 STEP 1 UNTIL 23 DO                            00026500
               B[24] ~ B[24]+B[K];                                      00026600
               B[25] ~ B[5]-B[24];                                      00026700
               B[26] ~ CASH[I]+B[25];                                   00026800
               B[29] ~ B[28]+B[27]+B[26];                               00026900
               B[2]  ~ B[2]|100.00;                                     00027000
               FOR II~1 STEP 1 UNTIL 5 DO WRITE(PRINT,FENTER);          00027100
               WRITE (PRINT,     TITLE);                                00027200
               WRITE (PRINT,     REPORT1,LINE1);                        00027300
               WRITE (PRINT,     REPORT2,LINE2);                        00027400
               WRITE (PRINT,REPORT3,LINE3);                             00027500
               WRITE (PRINT,REPORT4,LINE4);                             00027600
               WRITE (PRINT,     REPORT5,LINE5);                        00027700
               WRITE (PRINT,     REPORT6);                              00027800
               WRITE (PRINT,     REPORT7,LINE7);                        00027900
               WRITE (PRINT,REPORT8);                                   00028000
               WRITE (PRINT,REPORT9,LINE9);                             00028100
               WRITE (PRINT,REPORT10,LINE10);                           00028200
               WRITE (PRINT,REPORT11,LINE11);                           00028300
               WRITE (PRINT,REPORT12,LINE12);                           00028400
               WRITE (PRINT,REPORT13,LINE13);                           00028500
               WRITE (PRINT,REPORT14,LINE14);                           00028600
               WRITE (PRINT,     REPORT15,LINE15);                      00028700
               WRITE(PRINT,FENTER);                                     00028800
               WRITE (PRINT,REPORT16,LINE16);                           00028900
               WRITE(PRINT,FENTER);                                     00029000
               WRITE (PRINT,REPORT17,LINE17);                           00029100
               WRITE(PRINT,FENTER);                                     00029200
               WRITE (PRINT,     REPORT18,LINE18);                      00029300
               WRITE(PRINT,FENTER);                                     00029400
               WRITE(PRINT,FENTER);                                     00029500
               WRITE (PRINT,REPORT19,LINE19);                           00029600
               WRITE(PRINT,FENTER);                                     00029700
               WRITE (PRINT,REPORT20,LINE20);                           00029800
               WRITE(PRINT,FENTER);                                     00029900
               WRITE(PRINT,FENTER);                                     00030000
               WRITE (PRINT,REPORT21);                                  00030100
               WRITE (PRINT,     REPORT22,LINE22);                      00030200
               WRITE(PRINT,FENTER);                                     00030300
               WRITE(PRINT,FENTER);                                     00030400
               WRITE (PRINT,REPORT23);                                  00030500
               WRITE (PRINT,REPORT24,LINE24);                           00030600
               WRITE (PRINT,REPORT25,LINE25);                           00030700
               WRITE (PRINT,REPORT26,LINE26);                           00030800
               WRITE (PRINT,     REPORT27,LINE27);                      00030900
               WRITE(PRINT,FENTER);                                     00031000
               WRITE (PRINT,     REPORT28,LINE28);                      00031100
               WRITE(PRINT,FENTER);                                     00031200
               WRITE(PRINT,FENTER);                                     00031300
               WRITE (PRINT,     REPORT29);                             00031400
               WRITE (PRINT,REPORT30);                                  00031500
               WRITE (PRINT,REPORT31,LINE31);                           00031600
               WRITE(PRINT,FENTER);                                     00031700
               WRITE (PRINT,REPORT32,LINE32);                           00031800
               WRITE(PRINT,FENTER);                                     00031900
               WRITE (PRINT,     REPORT33,LINE33);                      00032000
               WRITE(PRINT,FENTER);                                     00032100
               WRITE(PRINT,FENTER);                                     00032200
               WRITE (PRINT,REPORT34,LINE34);                           00032300
               WRITE(PRINT,FENTER);                                     00032400
               WRITE(PRINT,FENTER);                                     00032500
               WRITE(PRINT,FENTER);                                     00032600
               FOR K ~ 4 STEP -1 UNTIL 2 DO                             00032700
          BEGIN                                                         00032800
               R[K,I] ~ R[K-1,I];                                       00032900
               A[K,I] ~ A[K-1,I];                                       00033000
          END;                                                          00033100
               J[I] ~ B[3];                                             00033200
               C[I] ~ B[28];                                            00033300
               T[I] ~ B[27];                                            00033400
               CASH[I] ~ B[26];                                         00033500
          END;                                                          00033600
ND:            I~QUAR/4;                                                00033700
               IF 4|I = QUAR THEN                                       00033800
          BEGIN                                                         00033900
               FOR L ~ 1 STEP 1 UNTIL N DO                              00034000
          BEGIN                                                         00034100
               FOR II~1 STEP 1 UNTIL 8 DO WRITE(PRINT,FENTER);          00034200
               WRITE (PRINT,TITLE);                                     00034300
               FOR I ~ 1 STEP 1 UNTIL N DO                              00034400
          BEGIN                                                         00034500
               WRITE(PRINT,FENTER);                                     00034600
               WRITE (PRINT,SUM1,S1);                                   00034700
               WRITE (PRINT,REPORT31,S2);                               00034800
               WRITE(PRINT,FENTER);                                     00034900
               WRITE (PRINT,REPORT32,S3);                               00035000
               WRITE(PRINT,FENTER);                                     00035100
               WRITE (PRINT,     REPORT33,S4);                          00035200
               WRITE(PRINT,FENTER);                                     00035300
               WRITE(PRINT,FENTER);                                     00035400
               WRITE (PRINT,     REPORT34,S5);                          00035500
               WRITE(PRINT,FENTER);                                     00035600
               WRITE(PRINT,FENTER);                                     00035700
          END                                                           00035800
          END                                                           00035900
          END;                                                          00036000
               IF QUAR = 40 THEN                                        00036100
          BEGIN                                                         00036200
               IF NTEAMS=40 THEN                                        00036300
          BEGIN                                                         00036400
               WRITE(TTOUT(STA),F101);                                  00036500
               GO TO DUNN;                                              00036600
          END;                                                          00036700
               NTEAMS~40;                                               00036800
               PCS~FALSE;                                               00036900
               GO TO ND;                                                00037000
          END;                                                          00037100
               PCS~TRUE;                                                00037200
               IF FIRSTQ!"YES" THEN REWIND (ES2);                       00037300
               QUAR ~ QUAR+1; GO TO START;                              00037400
PCH:                                                                    00037500
               WRITE(SFES[FF],MS1,CRD1);                                00037600
               FOR I ~ 1 STEP 20 UNTIL 40 DO                            00037700
               WRITE (SFES[FF],MS2,CRD2);                               00037800
               FOR I ~ 1 STEP 1 UNTIL N DO                              00037900
               WRITE(SFES[FF],PUN1,CD1);                                00038000
               IF FIRSTQ="YES" AND FF=2 THEN                            00038100
          BEGIN                                                         00038200
               FF~1;                                                    00038300
               LOCK(ES3,SAVE);                                          00038400
               GO TO START;                                             00038500
          END;                                                          00038600
               GO TO DUNN;                                              00038700
               END;                                                     00038800
DUNN:                                                                   00038900
          END.                                                          00039000
