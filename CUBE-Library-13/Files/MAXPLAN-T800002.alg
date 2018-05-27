BEGIN                                                                   00000000
COMMENT        MAXPLAN/STATMAN;                                         00000100
FILE IN CARD 0(2,10);     FILE OUT  LINE 4(2,15);                       00000200
FILE OUT PUNCH 0(2,10);                                                 00000300
BOOLEAN ARRAY  SENSL[0:4];                                              00000400
REAL  XPR;                                                              00000500
INTEGER  QQ;                                                            00000600
LABEL FINIS;                                                            00000700
PROCEDURE ERROR(N); VALUE N; INTEGER N; BEGIN                           00000800
  FORMAT F(///////"STOP / PAUSE  NO. ", I5);                            00000900
  WRITE (LINE,  F, N);  GO TO FINIS END;                                00001000
REAL PROCEDURE INT(ARG1); VALUE ARG1; REAL ARG1;                        00001100
    BEGIN INT~(SIGN(ARG1)|ENTIER(ABS(ARG1))) END;                       00001200
REAL PROCEDURE MAX(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00001300
  BEGIN MAX~(IF ARG1}ARG2 THEN ARG1 ELSE ARG2) END;                     00001400
REAL PROCEDURE MIN(ARG1,ARG2); VALUE ARG1,ARG2; REAL ARG1,ARG2;         00001500
  BEGIN MIN~(IF ARG1{ARG2 THEN ARG1 ELSE ARG2) END;                     00001600
DEFINE         TAPE5=PUNCH#,  TAPE7=CARD#,SINQ=SIN#,SQRTQ=SQRT#,        00001700
               COSQ=COS#,ATANQ=ARCTAN#;                                 00001800
ARRAY          KOUNTQ,LKOUNTQ,NHYPQ,KTOTALQ,ISTABQ,CLONGQ,TCOSQ,TEMQ,   00001900
               SHIFTQ,EXANGQ[0:30],LHYQ[0:400],IPROGQ[0:60],ITPQ[0:9],  00002000
               IPARQ[0:23],VZEROQ[0:30,0:400],COSNQ,CLAMQ[0:30,0:30],   00002100
               FMTQ[0:98],ROWQ[0:150],TITLEQ[0:12];                     00002200
REAL           TOPQ,TIMEQ,TCCQ,TCQ,ANGQ,AHYPPCQ,PC1Q,PC2Q,PC3Q,TTOTALQ, 00002300
               TPLOTQ,UPQ;                                              00002400
INTEGER        NVARQ,NFACQ,     KSWTQ,KHWTQ,KOUNT2Q,K,IMPQ,LASTQ,       00002500
               LEGALQ,L,KRAPQ,MULTQ,KNQ,IMPSQ;                          00002600
DEFINE         IPLOTQ=IPARQ[8]#,KANGQ=IPARQ[9]#,KSTARTQ=IPARQ[10]#,     00002700
               NCYCLEQ=IPARQ[11]#,KCYCLEQ=IPARQ[12]#,TOTIMQ=IPARQ[13]#, 00002800
               WIDQ=IPARQ[14]#,SIGQ=IPARQ[15]#,XCLHCQ=IPARQ[16]#,       00002900
               NCLCYQ=IPARQ[17]#,NCLSPQ=IPARQ[18]#,NCLHOQ=IPARQ[19]#,   00003000
               ANFMINQ=IPARQ[20]#,ANGMINQ=IPARQ[21]#,CMAXQ=IPARQ[22]#,  00003100
               NSHQ=IPARQ[23]#;                                         00003200
PROCEDURE   TIMINQ(A);   %*******************************               00003300
       REAL A;                                                          00003400
       A~TIME(1);                                                       00003500
PROCEDURE   TIMOUTQ(A);  %*******************************               00003600
       REAL A;                                                          00003700
       A~(TIME(1)-A) DIV 60;                                            00003800
FILE IN FACMAT 2(2,100),    ROTMAT 2(2,100),  CORMAT 2(2,100);          00003900
FORMAT OKTL(100O);                                                      00004000
SWITCH FILE  SW~LINE,CARD,LINE,LINE,LINE,CORMAT,FACMAT,ROTMAT;          00004100
COMMENT **************************  PLOT  ****************;             00004200
PROCEDURE PLOTQ;                                                        00004300
BEGIN                                                                   00004400
OWN INTEGER I;                                                          00004500
LABEL DUMMY;                                                            00004600
          I ~ 0;                                                        00004700
          GO TO DUMMY;                                                  00004800
DUMMY:                                                                  00004900
END;                                                                    00005000
                                                                        00005100
COMMENT **************************  EXTRA  ****************;            00005200
PROCEDURE EXTRAQ;                                                       00005300
BEGIN                                                                   00005400
OWN REAL X;                                                             00005500
LABEL DUMMY;                                                            00005600
          COMMENT                                                       00005700
    THIS IS A DUMMY SUBROUTINE (SEE MAIN PROGRAM);                      00005800
          X ~ 0;                                                        00005900
          GO TO DUMMY;                                                  00006000
DUMMY:                                                                  00006100
END;                                                                    00006200
                                                                        00006300
COMMENT **************************  DUMMY  ****************;            00006400
PROCEDURE DUMMYQ;                                                       00006500
BEGIN                                                                   00006600
OWN REAL X;                                                             00006700
LABEL DUMMY;                                                            00006800
          COMMENT                                                       00006900
    THIS IS A DUMMY SUBROUTINE (SEE MAIN PROGRAM);                      00007000
          X ~ 0;                                                        00007100
          GO TO DUMMY;                                                  00007200
DUMMY:                                                                  00007300
END;                                                                    00007400
                                                                        00007500
COMMENT **************************  HYPPC  ****************;            00007600
PROCEDURE HYPPCQ;                                                       00007700
BEGIN                                                                   00007800
OWN INTEGER JBQ,JAQ,JCQ;                                                00007900
OWN REAL V;                                                             00008000
LABEL DUMMY,L6003;                                                      00008100
          COMMENT                                                       00008200
    THIS SUBROUTINE CALCULATES THE HYPERPLANE COUNT FOR EACH FACTOR     00008300
    AND ALSO THE COUNT AND PERCENT VALUE OF POINTS FOR THE TOTAL        00008400
    CALCULATE HYPERPLANE COUNT;                                         00008500
          KOUNT2Q ~ 0;                                                  00008600
          FOR JBQ ~ 1, JBQ+1 STEP 1 UNTIL NFACQ DO BEGIN                00008700
            NHYPQ[JBQ] ~ 0;                                             00008800
            FOR JAQ ~ 1, JAQ+1 STEP 1 UNTIL NVARQ DO BEGIN              00008900
              V ~ 0;                                                    00009000
              FOR JCQ ~ 1, JCQ+1 STEP 1 UNTIL NFACQ DO BEGIN            00009100
                V ~ V+VZEROQ[JCQ,JAQ]|CLAMQ[JBQ,JCQ]  END;              00009200
              IF ABS(V)>WIDQ THEN GO TO L6003;                          00009300
              KOUNT2Q ~ KOUNT2Q+1;                                      00009400
              NHYPQ[JBQ] ~ NHYPQ[JBQ]+1;                                00009500
L6003:        END;                                                      00009600
            END;                                                        00009700
          AHYPPCQ ~ (KOUNT2Q)|100/(NFACQ|NVARQ);                        00009800
          GO TO DUMMY;                                                  00009900
DUMMY:                                                                  00010000
END;                                                                    00010100
                                                                        00010200
COMMENT **************************  WEIGHT  ****************;           00010300
PROCEDURE WEIGHTQ;                                                      00010400
BEGIN                                                                   00010500
OWN INTEGER INDEXQ,ISQ,JSQ;                                             00010600
OWN REAL  V;                                                            00010700
LABEL DUMMY,L4,L5;                                                      00010800
          COMMENT                                                       00010900
    THIS IS THE SUBROUTINE THAT WEIGHTS THE POINTS WHEN  FACTOR K       00011000
    IS BEING SHIFTED ON FACTOR L;                                       00011100
          INDEXQ ~ KSWTQ+1;                                             00011200
          FOR ISQ ~ 1, ISQ+1 STEP 1 UNTIL NVARQ DO BEGIN                00011300
            LHYQ[ISQ] ~ 0;                                              00011400
            V ~ 0;                                                      00011500
            FOR JSQ ~ 1, JSQ+1 STEP 1 UNTIL NFACQ DO BEGIN              00011600
              V ~ V+VZEROQ[JSQ,ISQ]|CLAMQ[L,JSQ]  END;                  00011700
            IF ABS(V)<WIDQ THEN GO TO L5;                               00011800
            IF KSWTQ>0 THEN GO TO L4;                                   00011900
            LHYQ[ISQ] ~ 1;                                              00012000
            GO TO L5;                                                   00012100
L4:         LHYQ[ISQ] ~  INT((5|ABS(V))*INDEXQ);                        00012200
L5:         END;                                                        00012300
          GO TO DUMMY;                                                  00012400
DUMMY:                                                                  00012500
END;                                                                    00012600
                                                                        00012700
COMMENT **************************  MXHEAD  ****************;           00012800
PROCEDURE MXHEADQ;                                                      00012900
BEGIN                                                                   00013000
INTEGER  DX1;                                                           00013100
INTEGER  INDEX1;                                                        00013200
FORMAT FL100(///" MAXPLANE OF 1 DECEMBER 1965   "),                     00013300
       FL102(//" PARAMETERS ARE"/X7," (1)",R9.3,X7," (2)",I5,X11,       00013400
         " (3)",I5,X11," (4)",I5,X11," (5)",I5/X7," (6)",I5,X11,        00013500
         " (7)",I5,X11," (8)",I5,X11," (9)",I5,X11,"(10)",I5/X7,        00013600
         "(11)",I5,X11,"(12)",I5,X11,"(13)",R9.3,X7,"(14)",R9.3,X7,     00013700
         "(15)",R9.3/X7,"(16)",I5,X11,"(17)",R9.3,X7,"(18)",I5,X11,     00013800
         "(19)",I5,X11,"(20)",R9.3/X7,"(21)",R9.3,X7,"(22)",R9.3,X7,    00013900
         "(23)",I5,X11,"(24)",I5,X11,"(25)",R9.3/X7,"(26)",I5,X11,      00014000
         "(27)",I5/),                                                   00014100
       FL103(" "////X39,"I L L I N O I S   M A X P L A N E"//X24,       00014200
         12A6//X41,"ROTATION  TO  SIMPLE  STRUCTURE"/X41,"WITH",I4,     00014300
         " VARIABLES",I5," FACTORS"/X41,"HYPERPLANE WIDTH TAKEN AS",    00014400
         R6.3///),                                                      00014500
       FL101(X16,"SHIFTED ON",X9,"HYPERPLANE COUNT",X5,"HYPERPLANE"/    00014600
         X5,"FACTOR   FACTOR   ANGLE   THIS FACTOR  ALL FACTORS  ",     00014700
         "PER CENT");                                                   00014800
LIST LIST1(TOTIMQ,FOR DX1 ~ 2 STEP 1 UNTIL 12 DO IPARQ[DX1],WIDQ,SIGQ,  00014900
       ANFMINQ,NCLCYQ,XCLHCQ,NCLSPQ,NCLHOQ,UPQ,PC1Q,PC2Q,KHWTQ,KSWTQ,   00015000
       TTOTALQ,KRAPQ,MULTQ),                                            00015100
     LIST2(FOR INDEX1 ~ 1 STEP 1 UNTIL 12 DO TITLEQ[INDEX1],NVARQ,      00015200
       NFACQ,WIDQ);                                                     00015300
LABEL DUMMY;                                                            00015400
          COMMENT                                                       00015500
    THIS SUBROUTINE LISTS THE PARAMETERS AND PRINTS THE HEADING;        00015600
          WRITE(LINE,FL100);                                            00015700
          WRITE(LINE,FL102,LIST1);                                      00015800
          WRITE(LINE,FL103,LIST2);                                      00015900
          WRITE(LINE,FL101);                                            00016000
          GO TO DUMMY;                                                  00016100
DUMMY:                                                                  00016200
END;                                                                    00016300
                                                                        00016400
COMMENT **************************  TIMOK  ****************;            00016500
PROCEDURE TIMOKQ(L);                                                    00016600
       INTEGER L;                                                       00016700
BEGIN                                                                   00016800
OWN REAL  T,Y;                                                          00016900
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00017000
          TIMOUT, TIME;                                                 00017100
LABEL DUMMY,L1,L2;                                                      00017200
          COMMENT                                                       00017300
    THIS SUBROUTINE CALCULATES IF THERE IS ENOUGH TIME FOR ONE          00017400
    MORE CYCLE AND BASIC OUTPUT. TIME IS NOT ALLOWED FOR PLOTTING.;     00017500
          TIMOUTQ(TCQ);                                                 00017600
          TOPQ ~ MAX(TOPQ,TCQ);                                         00017700
          T ~ TIMEQ;                                                    00017800
          TIMOUTQ(T);                                                   00017900
          Y ~ (NVARQ);                                                  00018000
          IF TOTIMQ-T-TOPQ>.25|Y THEN GO TO L2 ELSE GO TO L1;           00018100
          COMMENT                                                       00018200
    THERE IS SUFFICIENT TIME;                                           00018300
L1:       L ~ 1;                                                        00018400
          GO TO DUMMY;                                                  00018500
          COMMENT                                                       00018600
    THERE IS NOT SUFFICIENT TIME;                                       00018700
L2:       L ~ -1;                                                       00018800
          GO TO DUMMY;                                                  00018900
DUMMY:                                                                  00019000
END;                                                                    00019100
                                                                        00019200
COMMENT **************************  MXEND  ****************;            00019300
PROCEDURE MXENDQ;                                                       00019400
BEGIN                                                                   00019500
OWN REAL  TTOTMQ,TTOTSQ;                                                00019600
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00019700
          TIMOUT, TIME;                                                 00019800
FORMAT FL5080(" ",X2,"NUMBER OF CYCLES =",I5,X10,                       00019900
         "TOTAL MAXPLANE TIME =",R4.0,R6.1/),                           00020000
       FL5081(/" LAST CYCLE TIME=",R6.1,X10,"GREATEST CYCLE TIME=",     00020100
         R6.1,X10,"TOTAL OUTPUT TIME=",R6.1,X10/                        00020200
         "PLOT SUBROUTINE TIME=",R6.1/" ",X52,                          00020300
         "(ABOVE TIMES ARE IN SECONDS)"/" "),                           00020400
       FL5003(/" TIMES FOR THIS RUN",X22,"TOTAL TIME =",R4.0,R6.1/      00020500
         X30,"(ABOVE TIMES IN MINUTES AND SECONDS   MM  SS.S)");        00020600
LIST LIST1(KCYCLEQ,TTOTMQ,TTOTSQ),                                      00020700
     LIST2(TTOTMQ,TTOTSQ),                                              00020800
     LIST3(TCQ,TOPQ,TCCQ,TPLOTQ);                                       00020900
LABEL DUMMY;                                                            00021000
          COMMENT                                                       00021100
    THIS SUBROUTINE PRINTS SOME STATISTICS GIVING THE NUMBER OF         00021200
    CYCLES AND VARIOUS TIMES;                                           00021300
          TIMOUTQ(TIMEQ);                                               00021400
          TTOTMQ ~ TTOTALQ/60;                                          00021500
          TTOTMQ ~ INT(TTOTMQ);                                         00021600
          TTOTSQ ~ TTOTALQ-60|TTOTMQ;                                   00021700
          WRITE(LINE,FL5080,LIST1);                                     00021800
          TTOTMQ ~ TIMEQ/60;                                            00021900
          TTOTMQ ~ INT(TTOTMQ);                                         00022000
          TTOTSQ ~ TIMEQ-60|TTOTMQ;                                     00022100
          WRITE(LINE,FL5003,LIST2);                                     00022200
          TIMOUTQ(TCCQ);                                                00022300
          WRITE(LINE,FL5081,LIST3);                                     00022400
          GO TO DUMMY;                                                  00022500
DUMMY:                                                                  00022600
END;                                                                    00022700
                                                                        00022800
COMMENT **************************  MXTEST  ****************;           00022900
PROCEDURE MXTESTQ;                                                      00023000
BEGIN                                                                   00023100
OWN INTEGER J;                                                          00023200
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00023300
          TIMOK, TIMOUT;                                                00023400
FORMAT FL112(X30,"*** DOUBLE SHIFTS STARTED"),                          00023500
       FL100(X30,"***MINIMUM ANGLE OF SEPARATION SET AT 30 DEGREES"),   00023600
       FL101(X30,                                                       00023700
         "***POINTS WEIGHTED 1 IN 1/1, 5 IN 2/3, 15 IN 1/3 OF ",        00023800
         "HYPERPLANE WIDTH "),                                          00023900
       FL102(X30,"***OUTLYING HYPERPLANE POINTS WEIGHTED"),             00024000
       FL103(X30,"***NO IMPROVEMENT IN STRUCTURE"),                     00024100
       FL104(X30,"***LESS THAN ",R4.1,                                  00024200
         " PERCENT IMPROVEMENT IN 3 CYCLES"),                           00024300
       FL111(X30,"***SS HAS BEEN REACHED. BEGINNING FINAL SCAN."),      00024400
       FL105(X30,"***2 DEGREE SHIFTS TO BE STARTED"),                   00024500
       FL106(X30,"***OUTLYING HYPERPLANE POINTS NOT WEIGHTED"),         00024600
       FL107(//X30,"***MAXIMUM SIMPLE STRUCTURE HAS BEEN REACHED"//),   00024700
       FL108(//X30,                                                     00024800
         "***MAXIMUM SIMPLE STRUCTURE FOR 6 DEGREE SHIFTS HAS ",        00024900
         "BEEN REACHED"//),                                             00025000
       FL109(//X30,                                                     00025100
         "***INSUFFICIENT TIME FOR NEXT CYCLE - PROGRAM WILL G",        00025200
         "O TO OUTPUT",".  THERE WILL BE NO PLOT."//),                  00025300
       FL110(X30,                                                       00025400
         "***THE SPECIFIED NUMBER OF CYCLES HAVE BEEN COMPLETE","D"/);  00025500
LIST LIST1(UPQ);                                                        00025600
LABEL DUMMY,L1,L2,L3,L4,L5,L7,L9,L10,L11,L12,L13,L14,L15,L16,L17,       00025700
        L80,L18,L182,L20,L21,L22,L23;                                   00025800
          COMMENT                                                       00025900
    THIS SUBROUTINE TESTS  FOR  TERMINATING CONDITIONS AT THE END       00026000
    OF EACH CYCLE                                                       00026100
    TEST IF SPECIFIED NUMBER OF CYCLES HAVE BEEN COMPLETED;             00026200
          IF NCYCLEQ=KCYCLEQ THEN GO TO L23 ELSE GO TO L1;              00026300
          COMMENT                                                       00026400
    TEST IF NUMBER OF CYCLES FOR MINIMUM ANGLE CHANGE ATTAINED;         00026500
L1:       IF NCLCYQ=KCYCLEQ THEN GO TO L12 ELSE GO TO L2;               00026600
          COMMENT                                                       00026700
    TEST IF HYPERPLANE 0/0 FOR MINIMUN ANGLE CHANGE ATTAINED;           00026800
L2:       IF XCLHCQ>AHYPPCQ THEN GO TO L3 ELSE GO TO L11;               00026900
          COMMENT                                                       00027000
    TEST IF CYCLE NUMBER FOR HYPERPLANE SPREAD WEIGHTING ATTAINED;      00027100
L3:       IF NCLSPQ=KCYCLEQ THEN GO TO L13 ELSE GO TO L4;               00027200
          COMMENT                                                       00027300
    TEST IF CYCLE NUMBER FOR WEIGHTING HYPERPLANE OUTLYERS ATTAINED;    00027400
L4:       IF NCLHOQ=KCYCLEQ THEN GO TO L14 ELSE GO TO L5;               00027500
          COMMENT                                                       00027600
    TEST FOR ANY IMPROVEMENT;                                           00027700
L5:       IF IMPQ{0 THEN GO TO L15;                                     00027800
          PC3Q ~ PC2Q;                                                  00027900
          PC2Q ~ PC1Q;                                                  00028000
          PC1Q ~ AHYPPCQ;                                               00028100
          COMMENT                                                       00028200
    TEST FOR 3 CYCLE IMPROVEMENT LESS THAN SPECIFIED AMOUNT;            00028300
          IF AHYPPCQ-PC3Q}UPQ THEN GO TO L7 ELSE GO TO L16;             00028400
          COMMENT                                                       00028500
    TEST IF THERE IS SUFFICIENT TIME FOR ANOTHER CYCLE;                 00028600
L7:       TIMOKQ(L);                                                    00028700
          IF L}0 THEN GO TO L22;                                        00028800
          LASTQ ~ 0;                                                    00028900
L9:       GO TO DUMMY;                                                  00029000
          COMMENT                                                       00029100
    SET INDICATOR TO SHOW THAT CYCLING TO BE TERMINATED;                00029200
L10:      LASTQ ~ 1;                                                    00029300
          TIMOUTQ(TCQ);                                                 00029400
          GO TO DUMMY;                                                  00029500
L11:      NCLSPQ ~ KCYCLEQ;                                             00029600
          NCLCYQ ~ KCYCLEQ;                                             00029700
          MULTQ ~ 1;                                                    00029800
          WRITE(LINE,FL112);                                            00029900
L12:      XCLHCQ ~ 200;                                                 00030000
          CMAXQ ~ 0.86603;                                              00030100
          ANGMINQ ~ 0.5235988;                                          00030200
          ANFMINQ ~ 30;                                                 00030300
          NSHQ ~ 20;                                                    00030400
          WRITE(LINE,FL100);                                            00030500
          GO TO L3;                                                     00030600
L13:      KHWTQ ~ KHWTQ+1;                                              00030700
          IF KHWTQ<1 THEN GO TO L4;                                     00030800
          WRITE(LINE,FL101);                                            00030900
          GO TO L4;                                                     00031000
L14:      IF KSWTQ<0 THEN GO TO L5;                                     00031100
          WRITE(LINE,FL102);                                            00031200
          KSWTQ ~ MAX(LEGALQ,1);                                        00031300
          GO TO L5;                                                     00031400
L15:      WRITE(LINE,FL103);                                            00031500
          GO TO L17;                                                    00031600
L16:      WRITE(LINE,FL104,LIST1);                                      00031700
          COMMENT                                                       00031800
    TERMINATE IF 2 DEGREE SHIFTS WERE BEING DONE, OTHERWISE TEST IF     00031900
    THEY ARE REQUIRED AND ADJUST PARAMETERS ACCORDINGLY;                00032000
L17:      IF KANGQ}2 THEN GO TO L80;                                    00032100
          IF KHWTQ{0 THEN GO TO L18;                                    00032200
L80:      IF KRAPQ!0 THEN GO TO L20;                                    00032300
          WRITE(LINE,FL111);                                            00032400
          KANGQ ~ 1;                                                    00032500
          MULTQ ~ 0;                                                    00032600
          KHWTQ ~ 0;                                                    00032700
          NSHQ ~ 20;                                                    00032800
          KRAPQ ~ 1;                                                    00032900
          ANFMINQ ~ 60;                                                 00033000
          XCLHCQ ~ 50;                                                  00033100
          NCLCYQ ~ 4;                                                   00033200
          ANGMINQ ~ 1.047198;                                           00033300
          ANGQ ~ .1047198;                                              00033400
          KSWTQ ~ 0;                                                    00033500
          FOR J ~ 1, J+1 STEP 1 UNTIL NFACQ DO BEGIN                    00033600
            ISTABQ[J] ~ 0  END;                                         00033700
          GO TO L7;                                                     00033800
L18:      IF KANGQ>0 THEN GO TO L21;                                    00033900
          IF NCLSPQ>KCYCLEQ THEN GO TO L182;                            00034000
          WRITE(LINE,FL105);                                            00034100
          CMAXQ ~ .86603;                                               00034200
          KANGQ ~ 2;                                                    00034300
          ANGMINQ ~ .5235988;                                           00034400
          ANFMINQ ~ 30;                                                 00034500
          NSHQ ~ 5;                                                     00034600
          KSWTQ ~ -5;                                                   00034700
          NCLHOQ ~ KCYCLEQ;                                             00034800
          PC1Q ~ 0;                                                     00034900
          PC2Q ~ 0;                                                     00035000
          WRITE(LINE,FL106);                                            00035100
          WRITE(LINE,FL100);                                            00035200
          GO TO L7;                                                     00035300
L182:     KHWTQ ~ KHWTQ+1;                                              00035400
          NCLSPQ ~ KCYCLEQ;                                             00035500
          IF KHWTQ<1 THEN GO TO L7;                                     00035600
          WRITE(LINE,FL101);                                            00035700
          PC2Q ~ 0;                                                     00035800
          PC3Q ~ 0;                                                     00035900
          GO TO L7;                                                     00036000
L20:      WRITE(LINE,FL107);                                            00036100
          GO TO L10;                                                    00036200
L21:      WRITE(LINE,FL108);                                            00036300
          GO TO L10;                                                    00036400
L22:      WRITE(LINE,FL109);                                            00036500
          TPLOTQ ~ 1;                                                   00036600
          LASTQ ~ 1;                                                    00036700
          GO TO DUMMY;                                                  00036800
L23:      WRITE(LINE,FL110);                                            00036900
          GO TO L10;                                                    00037000
DUMMY:                                                                  00037100
END;                                                                    00037200
                                                                        00037300
COMMENT **************************  MXROTN  ****************;           00037400
PROCEDURE MXROTNQ;                                                      00037500
BEGIN                                                                   00037600
OWN INTEGER NPOSQ,KPREVQ,KTOTQ,M,MAQ,ITOTQ,MEQ,MFQ,KJQ,K0Q,KLQ,MBQ;     00037700
OWN REAL  W3Q,W23Q,B,XNPOSQ,CORRMQ,TRYQ,EXQ,SHIFTLQ,SUMSQQ,V;           00037800
COMMENT  THE FOLLOWING FUNCTIONS ARE REQUIRED:                          00037900
          ATAN, SQRT, SIN;                                              00038000
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00038100
          WEIGHT;                                                       00038200
BEGIN                                                                   00038300
LABEL DUMMY,L32,L33,L34,L4102,L5014,L5073,L400,L401,L42,L5022,L43,      00038400
        L44,L45,L5075,L201,L540,L541,L542,L551,L553,L555,L320,L556,     00038500
        L321,L544,L68,L203;                                             00038600
          COMMENT                                                       00038700
    IN THIS SUBROUTINE FACTOR K IS ROTATED AGAINST FACTOR L.  ALL       00038800
    SHIFT POSITIONS THAT DO NOT VIOLATE THE ANGLE RESTRICTION ARE       00038900
    TRIED AND IF ANY OF THESE IS BETTER THAN THE ORIGINAL POSITION      00039000
    THE FACTOR IS SHIFTED TO THE BEST OF THESE.;                        00039100
          IMPSQ ~ 0;                                                    00039200
          W3Q ~ WIDQ/3;                                                 00039300
          W23Q ~ 2|W3Q;                                                 00039400
          COMMENT                                                       00039500
    CALCULATE ANGLES BETWEEN FACTORS;                                   00039600
          FOR L ~ 1, L+1 STEP 1 UNTIL NFACQ DO BEGIN                    00039700
            IF L!K THEN GO TO L32;                                      00039800
            EXANGQ[L] ~ 0;                                              00039900
            GO TO L34;                                                  00040000
L32:        IF ABS(COSNQ[L,K])<.0349 THEN GO TO L33;                    00040100
            EXANGQ[L] ~ ATANQ((SQRTQ(1-COSNQ[L,K]*2))/COSNQ[            00040200
              L,K]);                                                    00040300
            IF EXANGQ[L]>0 THEN GO TO L34;                              00040400
            EXANGQ[L] ~ EXANGQ[L]+3.14159;                              00040500
            GO TO L34;                                                  00040600
L33:        EXANGQ[L] ~ 1.57079;                                        00040700
L34:        LKOUNTQ[L] ~ 0  END;                                        00040800
          COMMENT                                                       00040900
    CYCLE THAT PAIRS FACTOR WITH EACH OTHER FACTOR STARTS;              00041000
          FOR L ~ 1, L+1 STEP 1 UNTIL NFACQ DO BEGIN                    00041100
            SHIFTQ[L] ~ 0;                                              00041200
            COMMENT                                                     00041300
    DO NOT TRY AND SHIFT FACTOR ON ITSELF;                              00041400
            IF L=K THEN GO TO L201;                                     00041500
            WEIGHTQ;                                                    00041600
            COMMENT                                                     00041700
    CALCULATE SHIFT POSITIONS;                                          00041800
            B ~ EXANGQ[L]-ANGMINQ;                                      00041900
            XNPOSQ ~ B/ANGQ;                                            00042000
            NPOSQ ~  INT(XNPOSQ);                                       00042100
            IF XNPOSQ}0 THEN GO TO L4102;                               00042200
            NPOSQ ~ NPOSQ-1;                                            00042300
L4102:      IF KANGQ<2 THEN GO TO L5014;                                00042400
            NPOSQ ~ 2;                                                  00042500
L5014:      KPREVQ ~ 0;                                                 00042600
            KTOTQ ~ 0;                                                  00042700
            COMMENT                                                     00042800
    START OF CYCLE THAT TRIES EACH SHIFT POSITION;                      00042900
            FOR M ~ 1, M+1 STEP 1 UNTIL NSHQ DO BEGIN                   00043000
              CORRMQ ~ M-NSHQ+NPOSQ;                                    00043100
              TRYQ ~ CORRMQ|ANGQ;                                       00043200
              EXQ ~ EXANGQ[L];                                          00043300
              IF TRYQ}0 THEN GO TO L5073;                               00043400
              EXQ ~ 3.14159-EXQ;                                        00043500
L5073:        SHIFTLQ ~ SINQ(TRYQ)/SINQ(EXQ-ABS(TRYQ));                 00043600
              COMMENT                                                   00043700
    CALCULATE LAMBDA VECTOR FOR PROPOSED SHIFT;                         00043800
              SUMSQQ ~ 0;                                               00043900
              FOR MAQ ~ 1, MAQ+1 STEP 1 UNTIL NFACQ DO BEGIN            00044000
                CLONGQ[MAQ] ~ CLAMQ[K,MAQ]+CLAMQ[L,MAQ]|SHIFTLQ;        00044100
                SUMSQQ ~ SUMSQQ+CLONGQ[MAQ]*2  END;                     00044200
              SUMSQQ ~ 1/SQRTQ(SUMSQQ);                                 00044300
              FOR MAQ ~ 1, MAQ+1 STEP 1 UNTIL NFACQ DO BEGIN            00044400
                CLONGQ[MAQ] ~ CLONGQ[MAQ]|SUMSQQ  END;                  00044500
              COMMENT                                                   00044600
    COUNT (WEIGHTED) SUM OF HYPERPLANE VARIABLES;                       00044700
              ITOTQ ~ 0;                                                00044800
              FOR MEQ ~ 1, MEQ+1 STEP 1 UNTIL NVARQ DO BEGIN            00044900
                COMMENT                                                 00045000
    TO AVOID FACTORS SHARING HYPERPLANES DO NOT COUNT POINTS THAT       00045100
    ARE IN THE HYPERPLANE OF L;                                         00045200
                IF LHYQ[MEQ]{0 THEN GO TO L42;                          00045300
                V ~ 0;                                                  00045400
                FOR MFQ ~ 1, MFQ+1 STEP 1 UNTIL NFACQ DO BEGIN          00045500
                  V ~ V+VZEROQ[MFQ,MEQ]|CLONGQ[MFQ]  END;               00045600
                V ~ ABS(V);                                             00045700
                IF V}WIDQ THEN GO TO L42;                               00045800
                IF KHWTQ>0 THEN GO TO L401 ELSE GO TO L400;             00045900
                COMMENT                                                 00046000
    DO NOT WEIGHT ACCORDING TO HYPERPLANE SPREAD;                       00046100
L400:           ITOTQ ~ ITOTQ+LHYQ[MEQ];                                00046200
                GO TO L42;                                              00046300
                COMMENT                                                 00046400
    WEIGHT ACCORDING TO HYPERPLANE SPREAD;                              00046500
L401:           ITOTQ ~ ITOTQ+LHYQ[MEQ];                                00046600
                COMMENT                                                 00046700
    LHY WILL BE 1 IF THERE IS NO WEIGHTING OF OUTLYERS;                 00046800
                IF V}W23Q THEN GO TO L42;                               00046900
                ITOTQ ~ ITOTQ+4;                                        00047000
                IF V}W3Q THEN GO TO L42;                                00047100
                ITOTQ ~ ITOTQ+5;                                        00047200
L42:            END;                                                    00047300
              IF CORRMQ=0 THEN GO TO L5022 ELSE GO TO L43;              00047400
              COMMENT                                                   00047500
    SET COUNT FOR ORIGINAL POSITION;                                    00047600
L5022:        KPREVQ ~ ITOTQ;                                           00047700
L43:          KOUNTQ[M] ~ ITOTQ;                                        00047800
              COMMENT                                                   00047900
    CHOOSE BEST POSITION (IT MAY BE THE ORIGINAL ONE);                  00048000
              IF KTOTQ>KOUNTQ[M] THEN GO TO L44;                        00048100
              KTOTQ ~ KOUNTQ[M];                                        00048200
              KJQ ~ M;                                                  00048300
L44:          END;                                                      00048400
            COMMENT                                                     00048500
    IF NO POSITION BETTER THAN ORIGINAL GO TO NEXT SHIFT PLANE;         00048600
            IF KTOTQ>KPREVQ THEN GO TO L45 ELSE GO TO L201;             00048700
            COMMENT                                                     00048800
    RECORD BEST SHIFT AND HYPERPLANE COUNT;                             00048900
L45:        CORRMQ ~ KJQ-NSHQ+NPOSQ;                                    00049000
            TRYQ ~ CORRMQ|ANGQ;                                         00049100
            EXQ ~ EXANGQ[L];                                            00049200
            EXANGQ[L] ~ TRYQ;                                           00049300
            IF TRYQ}0 THEN GO TO L5075;                                 00049400
            EXQ ~ 3.14159-EXQ;                                          00049500
L5075:      SHIFTQ[L] ~ SINQ(TRYQ)/SINQ(EXQ-ABS(TRYQ));                 00049600
            LKOUNTQ[L] ~ KOUNTQ[KJQ]-KPREVQ;                            00049700
L201:       END;                                                        00049800
          COMMENT                                                       00049900
    ALL BEST MOVES ARE NOW IN SHIFT(L);                                 00050000
          IF KANGQ>0 THEN GO TO L541;                                   00050100
L540:     KNQ ~ 1;                                                      00050200
          GO TO L542;                                                   00050300
L541:     IF MULTQ{0 THEN GO TO L540;                                   00050400
          KNQ ~ MAX(KNQ,2);                                             00050500
L542:     M ~ 0;                                                        00050600
          K0Q ~ 1;                                                      00050700
          FOR KLQ ~ 1, KLQ+1 STEP 1 UNTIL NFACQ DO BEGIN                00050800
            TEMQ[KLQ] ~ 0  END;                                         00050900
L551:     KTOTQ ~ 0;                                                    00051000
          FOR L ~ 1, L+1 STEP 1 UNTIL NFACQ DO BEGIN                    00051100
            IF LKOUNTQ[L]{KTOTQ THEN GO TO L553;                        00051200
            KTOTQ ~ LKOUNTQ[L];                                         00051300
            KJQ ~ L;                                                    00051400
            M ~ 1;                                                      00051500
L553:       END;                                                        00051600
          IF KTOTQ>0 THEN GO TO L555;                                   00051700
          IF M>0 THEN GO TO L556 ELSE GO TO L320;                       00051800
L555:     TEMQ[KJQ] ~ SHIFTQ[KJQ];                                      00051900
          LKOUNTQ[KJQ] ~ -LKOUNTQ[KJQ];                                 00052000
          KOUNTQ[K0Q] ~ KJQ;                                            00052100
          K0Q ~ K0Q+1;                                                  00052200
          IF K0Q>KNQ THEN GO TO L556 ELSE GO TO L551;                   00052300
          COMMENT                                                       00052400
    IF NO IMPROVING SHIFTS NOTE THAT THIS FACTOR WAS NOT MOVED;         00052500
L320:     ISTABQ[K] ~ 1;                                                00052600
          GO TO DUMMY;                                                  00052700
L556:     KOUNTQ[K0Q] ~ 0;                                              00052800
L321:     TEMQ[K] ~ 1;                                                  00052900
          COMMENT                                                       00053000
    CALCULATE THE NEW LAMBDA VECTOR FOR THIS FACTOR;                    00053100
          FOR MAQ ~ 1, MAQ+1 STEP 1 UNTIL NFACQ DO BEGIN                00053200
            V ~ 0;                                                      00053300
            FOR MBQ ~ 1, MBQ+1 STEP 1 UNTIL NFACQ DO BEGIN              00053400
              V ~ V+CLAMQ[MBQ,MAQ]|TEMQ[MBQ]  END;                      00053500
            CLONGQ[MAQ] ~ V  END;                                       00053600
          COMMENT                                                       00053700
    NORMALIZE;                                                          00053800
          SUMSQQ ~ 0;                                                   00053900
          FOR MAQ ~ 1, MAQ+1 STEP 1 UNTIL NFACQ DO BEGIN                00054000
            SUMSQQ ~ SUMSQQ+CLONGQ[MAQ]*2  END;                         00054100
          SUMSQQ ~ 1/SQRTQ(SUMSQQ);                                     00054200
          FOR MAQ ~ 1, MAQ+1 STEP 1 UNTIL NFACQ DO BEGIN                00054300
            CLONGQ[MAQ] ~ CLONGQ[MAQ]|SUMSQQ  END;                      00054400
          FOR MAQ ~ 1, MAQ+1 STEP 1 UNTIL NFACQ DO BEGIN                00054500
            V ~ 0;                                                      00054600
            IF MAQ=K THEN GO TO L68;                                    00054700
            FOR MBQ ~ 1, MBQ+1 STEP 1 UNTIL NFACQ DO BEGIN              00054800
              V ~ V+CLONGQ[MBQ]|CLAMQ[MAQ,MBQ]  END;                    00054900
            COMMENT                                                     00055000
    CHECK THAT ALL COSINES ARE WITHIN LEGAL RANGE;                      00055100
            IF ABS(V){CMAXQ THEN GO TO L68;                             00055200
            IF KNQ>1 THEN GO TO L544;                                   00055300
            KJQ ~ KOUNTQ[1];                                            00055400
            LKOUNTQ[KJQ] ~ 0;                                           00055500
            GO TO L542;                                                 00055600
L544:       KJQ ~ KOUNTQ[KNQ];                                          00055700
            TEMQ[KJQ] ~ 0;                                              00055800
            LKOUNTQ[KJQ] ~ -LKOUNTQ[KJQ];                               00055900
            KNQ ~ KNQ-1;                                                00056000
            GO TO L321;                                                 00056100
L68:        TCOSQ[MAQ] ~ V  END;                                        00056200
          COMMENT                                                       00056300
    SET COSINES AND LAMBDA MATRICES FOR NEW POSITION;                   00056400
          FOR MAQ ~ 1, MAQ+1 STEP 1 UNTIL NFACQ DO BEGIN                00056500
            COSNQ[K,MAQ] ~ TCOSQ[MAQ];                                  00056600
            CLAMQ[K,MAQ] ~ CLONGQ[MAQ];                                 00056700
            COSNQ[MAQ,K] ~ TCOSQ[MAQ]  END;                             00056800
          COMMENT                                                       00056900
    NOTE THERE WAS AN IMPROVEMENT;                                      00057000
          IMPSQ ~ 1;                                                    00057100
L203:     GO TO DUMMY;                                                  00057200
DUMMY:                                                                  00057300
END  END;                                                               00057400
                                                                        00057500
COMMENT **************************  MXOUT  ****************;            00057600
PROCEDURE MXOUTQ;                                                       00057700
BEGIN                                                                   00057800
INTEGER  DX1;                                                           00057900
OWN INTEGER J1Q,J2Q,J3Q,I,JAQ,JBQ,JCQ,MAXRQ,J,DX2;                      00058000
DEFINE         VECTORQ=LHYQ#;                                           00058100
OWN REAL  TIQ,X,V;                                                      00058200
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00058300
          TIMIN, TIME, TIMOUT, PWRITE, HYPPC;                           00058400
FORMAT FL3005("DATA (",I5,") IN FORMAT ( 6X,6E12.5 )"),                 00058500
       FL3006(2I3,6R12.5),                                              00058600
       FL3009("END OF DATA"),                                           00058700
       FL10(R4.0,11I2,S3,2R3.0,S1,R2.0,I2,S1,R4.0,2I2,S1,3R4.0,2I2,     00058800
         R10.2,2I1),                                                    00058900
       FL3004(//X30,                                                    00059000
         "***TRANSFORMATION MATRIX AND NEW PARAMETER CARD HAVE",        00059100
         " BEEN PUNCHED"///),                                           00059200
       FL3002(" ",X20,                                                  00059300
         "VECTOR COSINES  (MAXIMUM COSINE ALLOWED BETWEEN VECT",        00059400
         "ORS IS ",R6.3,")"),                                           00059500
       FL2002(/"  ",20R6.3/("  ",20R6.3)),                              00059600
       FL2012(" "//" ",X20,"TRANSFORMATION MATRIX (LAMBDA)"),           00059700
       FL8010(/" PER CENT OF VARIABLES IN",R6.3," HYPERPLANE IS",       00059800
         R5.1/),                                                        00059900
       FL2014(" ",X20,"REFERENCE STRUCTURE  V(RS)"//X3,20I6/X6,20I6),   00060000
       FL2003(/" ",I3,X1,20R6.3/(X8,20R6.3)),                           00060100
       FL2016(/" ",X3,20I6/(X4,20I6)),                                  00060200
       FL2015(" ",X20,                                                  00060300
         "REFERENCE VECTOR STRUCTURE V(RS) WITH NON-SALIENT VA",        00060400
         "LUES LESS THAN",R6.3," SUPPRESSED"//X7,30I4),                 00060500
       FL2044(I5,X2,S2,30R4.0);                                         00060600
LIST LIST1(NFACQ),                                                      00060700
     LIST2(K,J1Q,FOR DX1 ~ J2Q STEP 1 UNTIL J3Q DO CLAMQ[DX1,K]),       00060800
     LIST3(TOTIMQ,FOR DX1 ~ 2 STEP 1 UNTIL 12 DO IPARQ[DX1],WIDQ,SIGQ,  00060900
       ANFMINQ,NCLCYQ,XCLHCQ,NCLSPQ,NCLHOQ,UPQ,PC1Q,PC2Q,KHWTQ,KSWTQ,   00061000
       TTOTALQ,KRAPQ,MULTQ),                                            00061100
     LIST4(CMAXQ),                                                      00061200
     LIST5(FOR DX1 ~ 1 STEP 1 UNTIL NFACQ DO COSNQ[DX1,K]),             00061300
     LIST6(FOR DX1 ~ 1 STEP 1 UNTIL NFACQ DO CLAMQ[DX1,K]),             00061400
     LIST7(WIDQ,AHYPPCQ),                                               00061500
     LIST8(FOR DX1 ~ 1 STEP 1 UNTIL NFACQ DO DX1),                      00061600
     LIST9(K,FOR DX1 ~ 1 STEP 1 UNTIL NFACQ DO VZEROQ[DX1,K]),          00061700
     LIST10(FOR DX1 ~ 1 STEP 1 UNTIL NFACQ DO NHYPQ[DX1]),              00061800
     LIST11(SIGQ,FOR DX1 ~ 1 STEP 1 UNTIL NFACQ DO DX1),                00061900
     LIST12(J,FOR DX1 ~ 1 STEP 1 UNTIL NFACQ DO CLONGQ[DX1]);           00062000
LABEL DUMMY,L3007,L4021,L4023,L2999,L2019,L9902,L9900,L1,L2;            00062100
          COMMENT                                                       00062200
    THIS SUBROUTINE CONTROLS OUTPUT;                                    00062300
          TIMINQ(TCCQ);                                                 00062400
          COMMENT                                                       00062500
    PUNCH LAMBDA MATRIX;                                                00062600
          WRITE(TAPE5,FL3005,LIST1);                                    00062700
          FOR K ~ 1, K+1 STEP 1 UNTIL NFACQ DO BEGIN                    00062800
            J1Q ~ 1;                                                    00062900
L3007:      J3Q ~ 6|J1Q;                                                00063000
            J2Q ~ J3Q-5;                                                00063100
            J3Q ~ MIN(NFACQ,J3Q);                                       00063200
            WRITE(TAPE5,FL3006,LIST2);                                  00063300
            J1Q ~ J1Q+1;                                                00063400
            IF NFACQ>J3Q THEN GO TO L3007;                              00063500
            END;                                                        00063600
          WRITE(TAPE5,FL3009);                                          00063700
          TOTIMQ ~ TOTIMQ/60;                                           00063800
          COMMENT                                                       00063900
    SET FOR READING TRANSFORMATION MATRIX ON SUBSEQUENT RUN;            00064000
          IPARQ[4] ~ 2;                                                 00064100
          NCYCLEQ ~ NCYCLEQ-KCYCLEQ;                                    00064200
          NCYCLEQ ~ MAX(0,NCYCLEQ);                                     00064300
          TIQ ~ TIMEQ;                                                  00064400
          TIMOUTQ(TIQ);                                                 00064500
          TTOTALQ ~ TTOTALQ+TIQ;                                        00064600
          COMMENT                                                       00064700
    PUNCH CONTINUATION PARAMETER CARD;                                  00064800
          WRITE(TAPE5,FL10,LIST3);                                      00064900
          WRITE(LINE,FL3004);                                           00065000
          COMMENT                                                       00065100
    STORE RESULTS ON TAPE OR DISK IF REQUIRED;                          00065200
          IF IPARQ[7]{2 THEN GO TO L4021;                               00065300
          WRITE(SW[IPARQ[  7]],OKTL,NFACQ,NFACQ);                       00065400
                                    FOR DX1~1 STEP 1 UNTIL NFACQ DO     00065500
          WRITE(SW[IPARQ[  7]],OKTL,                                FOR 00065600
         DX2~1 STEP 1 UNTIL NFACQ DO COSNQ[DX2,DX1]);                   00065700
L4021:    IF IPARQ[3]{2 THEN GO TO L4023;                               00065800
          WRITE(SW[IPARQ[  3]],OKTL,NVARQ,NFACQ);                       00065900
                                    FOR DX1~1 STEP 1 UNTIL NVARQ DO     00066000
          WRITE(SW[IPARQ[  3]],OKTL,                                FOR 00066100
         DX2~1 STEP 1 UNTIL NFACQ DO VZEROQ[DX2,DX1]);                  00066200
L4023:    IF IPARQ[5]{2 THEN GO TO L2999;                               00066300
          WRITE(SW[IPARQ[  5]],OKTL,NFACQ,NFACQ);                       00066400
                                    FOR DX1~1 STEP 1 UNTIL NFACQ DO     00066500
          WRITE(SW[IPARQ[  5]],OKTL,                                FOR 00066600
         DX2~1 STEP 1 UNTIL NFACQ DO CLAMQ[DX2,DX1]);                   00066700
          COMMENT                                                       00066800
    PRINT COSINE MATRIX;                                                00066900
L2999:    WRITE(LINE,FL3002,LIST4);                                     00067000
          FOR K ~ 1, K+1 STEP 1 UNTIL NFACQ DO BEGIN                    00067100
            COSNQ[K,K] ~ 1;                                             00067200
            WRITE(LINE,FL2002,LIST5);                                   00067300
            END;                                                        00067400
          COMMENT                                                       00067500
    PRINT LAMBDA MATRIX;                                                00067600
          WRITE(LINE,FL2012);                                           00067700
          FOR K ~ 1, K+1 STEP 1 UNTIL NFACQ DO BEGIN                    00067800
            WRITE(LINE,FL2002,LIST6);                                   00067900
            END;                                                        00068000
          COMMENT                                                       00068100
    CALCULATE AND PRINT VARIOUS HYPERPLANE PERCENT VALUES;              00068200
          X ~ WIDQ;                                                     00068300
          FOR I ~ 1, I+1 STEP 1 UNTIL 5 DO BEGIN                        00068400
            WIDQ ~ I;                                                   00068500
            WIDQ ~ .05|WIDQ;                                            00068600
            HYPPCQ;                                                     00068700
            WRITE(LINE,FL8010,LIST7);                                   00068800
            END;                                                        00068900
          WIDQ ~ X;                                                     00069000
          HYPPCQ;                                                       00069100
          COMMENT                                                       00069200
    CALCULATE REFERENCE VECTOR MATRIX;                                  00069300
          FOR JAQ ~ 1, JAQ+1 STEP 1 UNTIL NVARQ DO BEGIN                00069400
            FOR JBQ ~ 1, JBQ+1 STEP 1 UNTIL NFACQ DO BEGIN              00069500
              V ~ 0;                                                    00069600
              FOR JCQ ~ 1, JCQ+1 STEP 1 UNTIL NFACQ DO BEGIN            00069700
                V ~ V+VZEROQ[JCQ,JAQ]|CLAMQ[JBQ,JCQ]  END;              00069800
              VECTORQ[JBQ] ~ V  END;                                    00069900
            FOR JBQ ~ 1, JBQ+1 STEP 1 UNTIL NFACQ DO BEGIN              00070000
              VZEROQ[JBQ,JAQ] ~ VECTORQ[JBQ]  END;                      00070100
            END;                                                        00070200
          COMMENT                                                       00070300
    PRINT REFERENCE VECTOR MATRIX;                                      00070400
          WRITE(LINE,FL2014,LIST8);                                     00070500
          FOR K ~ 1, K+1 STEP 1 UNTIL NVARQ DO BEGIN                    00070600
            WRITE(LINE,FL2003,LIST9);                                   00070700
            END;                                                        00070800
          WRITE(LINE,FL2016,LIST10);                                    00070900
          WRITE(LINE,FL8010,LIST7);                                     00071000
          IF IPARQ[6]{2 THEN GO TO L2019;                               00071100
          WRITE(SW[IPARQ[  6]],OKTL, NVARQ,NFACQ);                      00071200
                                    FOR DX1~1 STEP 1 UNTIL NVARQ DO     00071300
          WRITE(SW[IPARQ[  6]],OKTL,                                FOR 00071400
         DX2~1 STEP 1 UNTIL NFACQ DO VZEROQ[DX2,DX1]);                  00071500
          COMMENT                                                       00071600
    PRINT REFERENCE VECTOR MATRIX WITH NON-SALIENTS SUPPRESSED;         00071700
L2019:    WRITE(LINE,FL2015,LIST11);                                    00071800
          FOR J ~ 1, J+1 STEP 1 UNTIL NVARQ DO BEGIN                    00071900
            FOR K ~ 1, K+1 STEP 1 UNTIL NFACQ DO BEGIN                  00072000
              IF ABS(VZEROQ[K,J])}SIGQ THEN GO TO L9902;                00072100
              CLONGQ[K] ~ 0;                                            00072200
              GO TO L9900;                                              00072300
L9902:        CLONGQ[K] ~ VZEROQ[K,J];                                  00072400
L9900:        END;                                                      00072500
            WRITE(LINE,FL2044,LIST12);                                  00072600
            END;                                                        00072700
          IF TPLOTQ>0 THEN GO TO L1 ELSE GO TO DUMMY;                   00072800
          COMMENT                                                       00072900
    IF PROGRAM TERMINATED FOR LACK OF TIME SUPPRESS PLOT;               00073000
L1:       IPLOTQ ~ 0;                                                   00073100
L2:       GO TO DUMMY;                                                  00073200
DUMMY:                                                                  00073300
END;                                                                    00073400
                                                                        00073500
COMMENT **************************  MXCYCL  ****************;           00073600
PROCEDURE MXCYCLQ;                                                      00073700
BEGIN                                                                   00073800
OWN REAL  EXQ;                                                          00073900
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00074000
          TIMIN, MXROTN, HYPPC;                                         00074100
FORMAT FL101(X5,I4,I7,R10.3,X9,I6,X6,I6,X4,R8.1),                       00074200
       FL1988(X5,"CYCLE NO.",I4," WITH",I6," HYPERPLANE VARIABLES (",   00074300
         R6.2," PERCENT)."/);                                           00074400
LIST LIST1(K,L,TEMQ[L],NHYPQ[K],KOUNT2Q,AHYPPCQ),                       00074500
     LIST2(KCYCLEQ,KOUNT2Q,AHYPPCQ);                                    00074600
LABEL DUMMY,L1,L50,L10,L2,L4,L5;                                        00074700
          COMMENT                                                       00074800
    THIS SUBROUTINE CONTROLS THE CYCLING OF THE FACTOR K THAT IS        00074900
    BEING ROTATED;                                                      00075000
          TIMINQ(TCQ);                                                  00075100
          IMPQ ~ 0;                                                     00075200
          COMMENT                                                       00075300
    START ROTATING WITH THE FACTOR THAT IS SPECIFIED;                   00075400
          K ~ KSTARTQ;                                                  00075500
L1:       IF ISTABQ[K]>0 THEN GO TO L50 ELSE GO TO L10;                 00075600
          COMMENT                                                       00075700
    IF THE FACTOR WAS NOT MOVED LAST TIME SKIP IT FOR JUST THIS CYCLE;  00075800
L50:      ISTABQ[K] ~ 0;                                                00075900
          GO TO L5;                                                     00076000
L10:      MXROTNQ;                                                      00076100
          IF IMPSQ>0 THEN GO TO L2 ELSE GO TO L5;                       00076200
          COMMENT                                                       00076300
    IF A BETTER POSITION WAS FOUND FOR THE FACTOR PRINT HYPERPLANE      00076400
    INFORMATION;                                                        00076500
L2:       HYPPCQ;                                                       00076600
          IMPQ ~ 1;                                                     00076700
          EXQ ~ -1;                                                     00076800
          L ~ KOUNTQ[1];                                                00076900
L4:       WRITE(LINE,FL101,LIST1);                                      00077000
          IF MULTQ{0 THEN GO TO L5;                                     00077100
          IF L=KOUNTQ[2] THEN GO TO L5;                                 00077200
          L ~ KOUNTQ[2];                                                00077300
          IF L|(NFACQ+1-L){0 THEN GO TO L5;                             00077400
          IF TEMQ[L]!0 THEN GO TO L4;                                   00077500
L5:       K ~ K+1;                                                      00077600
          IF K{NFACQ THEN GO TO L1;                                     00077700
          KCYCLEQ ~ KCYCLEQ+1;                                          00077800
          COMMENT                                                       00077900
    AT END OF CYCLE PRINT CYCLE STATISTICS;                             00078000
          WRITE(LINE,FL1988,LIST2);                                     00078100
          GO TO DUMMY;                                                  00078200
DUMMY:                                                                  00078300
END;                                                                    00078400
                                                                        00078500
COMMENT **************************  COSCHK  ****************;           00078600
PROCEDURE COSCHKQ;                                                      00078700
BEGIN                                                                   00078800
INTEGER  DX1;                                                           00078900
OWN INTEGER NENDQ,I,J;                                                  00079000
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00079100
          MXCYCL, MXTEST, MXOUT, PLOT, MXEND;                           00079200
FORMAT FL100(" COSINES OF ANGLES BETWEEN VECTORS INVALID  (GREATER",    00079300
         " THAN",R6.3,                                                  00079400
         "). RUN WILL TERMINATE IF NOT CORRECTED IN 3 CYCLES."///X10,   00079500
         "COSINE MATRIX"/),                                             00079600
       FL101(/I5,19R6.3/X5,11R6.3),                                     00079700
       FL103(//X17,"HYPERPLANE COUNT",X5,"HYPERPLANE"/X5,               00079800
         "FACTOR  THIS FACTOR  ALL FACTORS  PER CENT"),                 00079900
       FL102(X30,                                                       00080000
         "COSINES WERE NOT CORRECTED IN 3 CYCLES. PROGRAM WILL",        00080100
         " GO TO OUTPUT WITHOUT PLOTTING."//);                          00080200
LIST LIST1(CMAXQ),                                                      00080300
     LIST2(I,FOR DX1 ~ 1 STEP 1 UNTIL NFACQ DO COSNQ[DX1,I]);           00080400
LABEL DUMMY,L2,L3,L4,L6,L8,L9;                                          00080500
          COMMENT                                                       00080600
    THIS SUBROUTINE CHECKS THAT THE COSINES BETWEEN THE FACTORS ARE     00080700
    WITHIN THE LIMITS SET BY THE MINIMUM ANGLE RESTRICTION. IF NOT,     00080800
    3 CYCLES ARE ALLOWED TO ADJUST THE ANGLES.   IF THERE ARE STILL     00080900
    INVALID ANGLES AFTER 3 CYCLES, THE PROGRAM PROCEEDS TO              00081000
    OUTPUT AND PRODUCTION IS TERMINATED.;                               00081100
          NENDQ ~ 0;                                                    00081200
          FOR I ~ 1, I+1 STEP 1 UNTIL NFACQ DO BEGIN                    00081300
            FOR J ~ 1, J+1 STEP 1 UNTIL NFACQ DO BEGIN                  00081400
              IF ABS(COSNQ[J,I])>CMAXQ THEN GO TO L3 ELSE GO TO L2;     00081500
              COMMENT                                                   00081600
    TEST FOR COSINE TOO LARGE;                                          00081700
L2:           END;                                                      00081800
            END;                                                        00081900
          GO TO DUMMY;                                                  00082000
L3:       IF NENDQ>0 THEN GO TO L6 ELSE GO TO L4;                       00082100
          COMMENT                                                       00082200
    BRANCH IF THIS IS NOT THE FIRST CLEAN-UP CYCLE;                     00082300
L4:       NCYCLEQ ~ KCYCLEQ+3;                                          00082400
          COMMENT                                                       00082500
    SET TO CYCLE FOR 3 CYCLES;                                          00082600
          NENDQ ~ 1;                                                    00082700
          WRITE(LINE,FL100,LIST1);                                      00082800
          FOR I ~ 1, I+1 STEP 1 UNTIL NFACQ DO BEGIN                    00082900
            WRITE(LINE,FL101,LIST2);                                    00083000
            END;                                                        00083100
          WRITE(LINE,FL103);                                            00083200
L6:       MXCYCLQ;                                                      00083300
          MXTESTQ;                                                      00083400
          COMMENT                                                       00083500
    TEST FOR END CONDITION;                                             00083600
          IF LASTQ{0 THEN GO TO L6;                                     00083700
          IF NCYCLEQ=KCYCLEQ THEN GO TO L8 ELSE GO TO L9;               00083800
          COMMENT                                                       00083900
    IF END CONDITION ON THIRD CYCLE SUPPRESS PRINT AND GO TO OUTPUT;    00084000
L8:       WRITE(LINE,FL102);                                            00084100
          IPLOTQ ~ 0;                                                   00084200
          COMMENT                                                       00084300
    IF NOT THIRD CYCLE JUST GO TO OUTPUT;                               00084400
L9:       MXOUTQ;                                                       00084500
          PLOTQ;                                                        00084600
          MXENDQ;                                                       00084700
          ERROR(0);                                                     00084800
DUMMY:                                                                  00084900
END;                                                                    00085000
                                                                        00085100
COMMENT **************************  MXZERO  ****************;           00085200
PROCEDURE MXZEROQ;                                                      00085300
BEGIN                                                                   00085400
INTEGER  INDEX1,DX1,DX2;                                                00085500
OWN INTEGER N,M,J;                                                      00085600
OWN REAL  V;                                                            00085700
COMMENT  THE FOLLOWING FUNCTIONS ARE REQUIRED:                          00085800
          COS;                                                          00085900
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00086000
          PREAD, MXHEAD, COSCHK;                                        00086100
FORMAT FL101(12A6),                                                     00086200
       FL100(/////                                                      00086300
         " ***ERROR IN PARAMETERS OR TRANSFORMATION MATRIX - P",        00086400
         "LEASE CHECK"//);                                              00086500
FORMAT         FL102(I4," VARIABLES",I8," FACTORS  --- INPUT MATRIX");  00086510
LIST LIST1(FOR INDEX1 ~ 1 STEP 1 UNTIL 12 DO TITLEQ[INDEX1]);           00086600
LABEL DUMMY,L2,L4,L301,L307,L303,L309,L311,L313,L5069,L5071,L402,       00086700
        L403,L404,L406,L28,L6,L9,L25,L5006,L31,L1003;                   00086800
          COMMENT                                                       00086900
    THIS SUBROUTINE READS IN THE FACTOE MATRIX, READS IN THE            00087000
    TRANSFORMATION MATRIX OR GENERATES AN IDENTITY TRANSFORMATION       00087100
    MATRIX, SETS THOSE PARAMETERS THAT ARE UNSPECIFIED TO               00087200
    STANDARD VALUES, AND CALCULATES THE THE VECTOR COSINES.;            00087300
          IF MULTQ>0 THEN GO TO L2;                                     00087400
          KNQ ~ 0;                                                      00087500
          GO TO L4;                                                     00087600
L2:       KNQ ~ 3;                                                      00087700
L4:       TPLOTQ ~ 0;                                                   00087800
          KSTARTQ ~ MAX(1,KSTARTQ);                                     00087900
          IF IPLOTQ>2 THEN GO TO L1003;                                 00088000
          IF KANGQ>2 THEN GO TO L1003;                                  00088100
          IF WIDQ>0 THEN GO TO L301;                                    00088200
          WIDQ ~ 0.2;                                                   00088300
L301:     IF SIGQ>0 THEN GO TO L307;                                    00088400
          SIGQ ~ .25;                                                   00088500
L307:     IF ANFMINQ>0 THEN GO TO L309;                                 00088600
          ANFMINQ ~ 60;                                                 00088700
          XCLHCQ ~ 50;                                                  00088800
L303:     NCLCYQ ~ 4;                                                   00088900
          GO TO L313;                                                   00089000
L309:     IF XCLHCQ>0 THEN GO TO L311;                                  00089100
          XCLHCQ ~ 100;                                                 00089200
L311:     IF NCLCYQ{0 THEN GO TO L303;                                  00089300
L313:     ANGMINQ ~ .0174533|ANFMINQ;                                   00089400
          ANGQ ~ .1047198;                                              00089500
          IF (XPR ~ KANGQ-2)>0 THEN GO TO L1003;                        00089600
          IF XPR<0 THEN GO TO L5069;                                    00089700
          ANGQ ~ ANGQ/3;                                                00089800
          NSHQ ~ 5;                                                     00089900
          GO TO L5071;                                                  00090000
L5069:    NSHQ ~  INT((3.14159-2|ANGMINQ)/ANGQ);                        00090100
L5071:    CMAXQ ~ COSQ(ANGMINQ);                                        00090200
          NCYCLEQ ~ NCYCLEQ+KCYCLEQ;                                    00090300
          LEGALQ ~ KSWTQ;                                               00090400
          IF (XPR ~ NCLSPQ)>0 THEN GO TO L403;                          00090500
          IF XPR=0 THEN GO TO L402;                                     00090600
          KHWTQ ~ -5;                                                   00090700
L402:     NCLSPQ ~ NCLCYQ+2;                                            00090800
L403:     IF (XPR ~ NCLHOQ)>0 THEN GO TO L406;                          00090900
          IF XPR=0 THEN GO TO L404;                                     00091000
          KSWTQ ~ -5;                                                   00091100
L404:     NCLHOQ ~ NCLSPQ;                                              00091200
L406:     IF UPQ>0 THEN GO TO L28;                                      00091300
          UPQ ~ 1;                                                      00091400
          COMMENT                                                       00091500
    READ IN TITLE;                                                      00091600
L28:      READ(TAPE7,FL101,LIST1)[FINIS];                               00091700
          IPARQ[2] ~ MAX(IPARQ[2],2);                                   00091800
          COMMENT                                                       00091900
    READ IN FACTOR MATRIX;                                              00092000
          READ(SW[IPARQ[  2]],OKTL,NVARQ,NFACQ);                        00092100
                                   FOR DX1~1 STEP 1 UNTIL NVARQ DO      00092200
          READ(SW[IPARQ[  2]],OKTL,                                FOR  00092300
         DX2~1 STEP 1 UNTIL NFACQ DO VZEROQ[DX2,DX1]);                  00092400
               WRITE(LINE,FL102,NVARQ,NFACQ);                           00092500
                                                                        00092600
       FOR DX1~1 STEP 1 UNTIL NVARQ DO                                  00092700
          WRITE(LINE,<20F6.3>, FOR DX2~1 STEP 1 UNTIL NFACQ DO          00092800
        VZEROQ[DX2,DX1]);                                               00092900
          IF IPARQ[4]>0 THEN GO TO L6 ELSE GO TO L9;                    00093000
          COMMENT                                                       00093100
    READ IN TRANSFORMATION MATRIX IF THERE IS ONE;                      00093200
L6:       READ(SW[IPARQ[  4]],OKTL,N,M);                                00093300
                                   FOR DX1~1 STEP 1 UNTIL N DO          00093400
          READ(SW[IPARQ[  4]],OKTL,                            FOR DX2~100093500
          STEP 1 UNTIL M DO CLAMQ[DX2,DX1]);                            00093600
          COMMENT                                                       00093700
    CHECK DIMENSIONS;                                                   00093800
          IF N!M THEN GO TO L1003;                                      00093900
          IF N=NFACQ THEN GO TO L25 ELSE GO TO L1003;                   00094000
          COMMENT                                                       00094100
    GENERATE IDENTITY CLAM IF NEEDED;                                   00094200
L9:       FOR J ~ 1, J+1 STEP 1 UNTIL NFACQ DO BEGIN                    00094300
            FOR K ~ 1, K+1 STEP 1 UNTIL NFACQ DO BEGIN                  00094400
              CLAMQ[K,J] ~ 0  END;                                      00094500
            CLAMQ[J,J] ~ 1  END;                                        00094600
          COMMENT                                                       00094700
    CALCULATE COSINE MATRIX;                                            00094800
L25:      FOR J ~ 1, J+1 STEP 1 UNTIL NFACQ DO BEGIN                    00094900
            KTOTALQ[J] ~ 0;                                             00095000
            ISTABQ[J] ~ 0;                                              00095100
            CLONGQ[J] ~ 0;                                              00095200
            TCOSQ[J] ~ 0;                                               00095300
            EXANGQ[J] ~ 0;                                              00095400
            FOR K ~ J, K+1 STEP 1 UNTIL NFACQ DO BEGIN                  00095500
              V ~ 0;                                                    00095600
              IF J=K THEN GO TO L5006;                                  00095700
              FOR L ~ 1, L+1 STEP 1 UNTIL NFACQ DO BEGIN                00095800
                V ~ V+CLAMQ[J,L]|CLAMQ[K,L]  END;                       00095900
L5006:        COSNQ[J,K] ~ V;                                           00096000
              COSNQ[K,J] ~ V  END;                                      00096100
            END;                                                        00096200
          MXHEADQ;                                                      00096300
          TOTIMQ ~ TOTIMQ|60;                                           00096400
          IF (XPR ~ IPLOTQ-2)>0 THEN GO TO L1003;                       00096500
          IF XPR=0 THEN GO TO DUMMY;                                    00096600
          COSCHKQ;                                                      00096700
L31:      GO TO DUMMY;                                                  00096800
L1003:    WRITE(LINE,FL100);                                            00096900
          COMMENT                                                       00097000
    RETURN PROGRAM CONTROL TO MONITOR SYSTEM WITH ERROR CONDITION;      00097100
          ERROR(0);                                                     00097200
DUMMY:                                                                  00097300
END;                                                                    00097400
                                                                        00097500
COMMENT **************************  MAINPRO  ****************;          00097600
PROCEDURE MAINPRO;                                                      00097700
BEGIN                                                                   00097800
INTEGER  DX1;                                                           00097900
COMMENT  THE FOLLOWING SUBROUTINES ARE REQUIRED:                        00098000
          TIMIN, TIME, MXZERO, MXOUT, PLOT, DUMMY, MXCYCL, MXTEST,      00098100
         MXEND, EXTRA;                                                  00098200
FORMAT FL100(R4.0,11I2,2R3.2,R2.0,I2,R4.1,2I2,3R4.1,2I2,R10.4,2I1),     00098300
       FL101(X30,"***ROTATION TERMINATED");                             00098400
LIST LIST1(TOTIMQ,FOR DX1 ~ 2 STEP 1 UNTIL 12 DO IPARQ[DX1],WIDQ,SIGQ,  00098500
       ANFMINQ,NCLCYQ,XCLHCQ,NCLSPQ,NCLHOQ,UPQ,PC1Q,PC2Q,KHWTQ,KSWTQ,   00098600
       TTOTALQ,KRAPQ,MULTQ);                                            00098700
LABEL L11,L2,L3;                                                        00098800
L11:      TIMINQ(TIMEQ);                                                00098900
          COMMENT                                                       00099000
    THE PARAMETER CARD IS READ;                                         00099100
          READ(TAPE7,FL100,LIST1)[FINIS];                               00099200
          KRAPQ ~ ABS(KRAPQ);                                           00099300
          MULTQ ~ ABS(MULTQ);                                           00099400
          MXZEROQ;                                                      00099500
          IF IPLOTQ}2 THEN GO TO L2 ELSE GO TO L3;                      00099600
          COMMENT                                                       00099700
    IF THIS IS A PLOT ONLY RUN THE PROGRAM PROCEEDS TO OUTPUT;          00099800
L2:       MXOUTQ;                                                       00099900
          PLOTQ;                                                        00100000
          GO TO L11;                                                    00100100
L3:       DUMMYQ;                                                       00100200
          COMMENT                                                       00100300
    DUMMY AND EXTRA, BELOW, ARE SUBROUTINES THAT RETURN CONTROL         00100400
    WITHOUT DOING ANYTHING. THEY ALLOW THE USER TO INSERT HIS OWN       00100500
    SUBROUTINES WHICH WILL WITH MOST COMPUTER SYSTEMS OVERRIDE THE      00100600
    DUMMY SUBROUTINES. A POSSIBLE USE FOR THIS SUBROUTINE, DUMMY,       00100700
    MIGHT BE TO BORDER THE FACTOR OR TRANSFORMATION MATRICES OR TO      00100800
    PERMUTE THE COLUMNS AS MIGHT BE DESIRED WITH KSTART NOT ONE.;       00100900
          MXCYCLQ;                                                      00101000
          MXTESTQ;                                                      00101100
          IF LASTQ{0 THEN GO TO L3;                                     00101200
          WRITE(LINE,FL101);                                            00101300
          MXOUTQ;                                                       00101400
          PLOTQ;                                                        00101500
          MXENDQ;                                                       00101600
          EXTRAQ;                                                       00101700
          COMMENT                                                       00101800
    EXTRA IS A DUMMY SUBROUTINE THAT DOES NOTHING BUT ALLOWS THE        00101900
    USER TO INSERT HIS OWN SUBROUTINE, E.G. TO CALCULATE THE FACTOR     00102000
    MATRICES FROM THE REFERENCE VECTOR MATRICES.;                       00102100
          GO TO L11;                                                    00102200
          END;                                                          00102300
                                                                        00102400
COMMENT **************************  OUTER BLOCK  ************;          00102500
  XPR~QQ~0;                                                             00102600
            SENSL[1]~FALSE;                                             00102700
            SENSL[2]~FALSE;                                             00102800
            SENSL[3]~FALSE;                                             00102900
            SENSL[4]~FALSE;                                             00103000
MAINPRO; FINIS:                                                         00103100
END OF OUTER BLK.                                                       00103200
                LAST CARD ON CRDIMG TAPE                                99999999
