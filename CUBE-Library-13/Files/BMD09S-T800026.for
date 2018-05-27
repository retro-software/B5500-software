FILE  5 = CARD, UNIT = READER, RECORD = 10, BUFFER = 2                  00001000
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00001100
FILE  3 = TAPE3,UNIT=TAPE,RECORD=50,BUFFER=2                            00001200
FILE  2 = TAPE2,UNIT=TAPE,RECORD=50,BUFFER=2                            00001300
C                       BMD09S TRANSGENERATION                          00001400
C             TRANSGENERATION - MAIN PROGRAM        AUGUST 25, 1964     00001500
CPROGRAM FLOW-                                                          00001600
C    1. DETERMINE TYPE OF INPUT AND READ IN VARIABLE INPUT FORMAT CARDS 00001700
C      A) PUNCHED INPUT- PREPARE BRANCHES AND LISTING                   00001800
C      B) BINARY TAPE INPUT- PREPARE BRANCHES AND LISTING, CHECK JIN    00001900
C      C) BCD TAPE INPUT- PREPARE BRANCHES AND LISTING                  00002000
C      D) NON-PUNCHED INPUT- PREPARE BRANCHES AND CHECK MIN             00002100
C      E) LIST METHOD OF INPUT                                          00002200
C      F) NON-BINARY INPUT- PREPARE BRANCHES, CHECK JIN, LIST FORMAT    00002300
C    2. DETERMINE TYPES OF OUTPUT, READ IN VARIABLE OUTPUT FORMATS      00002400
C      A) PRINTED OUTPUT- PREPARE BRANCHES AND LISTINGS, CHECK JPRT     00002500
C      B) PUNCHED OUTPUT- PREPARE BRANCHES AND LISTINGS, CHECK JPNCH AND00002600
C                                                                 NPUNCH00002700
C      C) BINARY OUTPUT- PREPARE BRANCHES AND LISTINGS, CHECK JTAPE     00002800
C      D) BCD OUTPUT- PREPARE BRANCHES AND LISTINGS, CHECK JTAPE        00002900
C      E) TAPE OUTPUT- CHECK MIN                                        00003000
C      F) NON-TAPE OUTPUT- PREPARE BRANCH                               00003100
C      G) LIST METHODS OF OUTPUT                                        00003200
C      H) LIST TYPE AND NUMBER OF OUTPUT TAPE                           00003300
C      I) READ AND LIST APPROPRIATE FORMAT CARDS                        00003400
C    3. READ TRANSGENERATION CARDS, CHECK CONSTRAINTS, SAVE LOCATIONS OF00003500
C       ALL TYPE-18 AND -19 TRANSGENERATIONS, STORE TYPE-40 CONSTANTS   00003600
C    4. RUN THROUGH DATA FOR MEAN AND STANDARD DEVIATION COMPUTATIONS   00003700
C                (IF DATA INPUT WAS FROM CARDS, IT IS WRITTEN ON TAPE 2)00003800
C    5. CHECK THAT NO VARIABLE USED IN TYPE-18 OR -19 TRANSGENERATION IS00003900
C       CHANGED (UNLESS BY ANOTHER TYPE-18) BEFORE THAT TRANSGENERATION 00004000
C    6. CALL SUBROUTINE TRANS TO TRANSGENERATE, ONE CASE AT A TIME FROM 00004100
C              TAPE, SAVE OLD AND NEW VARIABLES FOR FIRST AND LAST CASES00004200
C    7. AFTER EACH CASE THE REQUESTED OUTPUTS ARE MADE                  00004300
C    8. PRINT OUT OLD AND NEW VARIABLES FOR FIRST AND LAST CASES        00004400
CGUIDE TO FIRST LETTER IN FIXED-POINT VARIABLES-                        00004500
C    I    TO DETERMINE TRANSFERS AND FOR OUTERMOST DO LOOPS             00004600
C    J    NUMBER OF VARIABLE FORMAT CARDS (JIN,JPRT,JPNCH,JTAPE)        00004700
C    K    ALPHABETIC CONTROL WORDS AND CHECKS ON SAME                   00004800
C    L    THE 3 WORDS- LPRBM,LNAME,LTRGN                                00004900
C    M    TAPE NO. (MIN,MIN1,MOUT,MOUT1)                                00005000
C    N    ALL OTHER VARIABLES (EXCEPT KODE)                             00005100
CGUIDE TO DIMENSIONED VARIABLES-                                        00005200
C    CONS=   CONSTANTS IN TYPE-40 TRANSGEN. (ONLY 50 40"S PERMITTED)    00005300
C    CONST=  2ND VARIABLE NO. OR REGULAR CONSTANT FOR TRANSGENERATIONS  00005400
C    DATA=   DATA (TRANSGENERATED ONE CASE AT A TIME FROM TAPE)         00005500
C    FMT=    VARIABLE INPUT FORMAT                                      00005600
C    KODE=   TYPE OF TRANSGENERATION                                    00005700
C    NCON40= NUMBER OF TYPE-40 CONSTANTS                                00005800
C    NOTRAN= OLD VARIABLE TO BE TRANSGENERATED                          00005900
C    NSUM18= LOCATION OF CARDS HAVING TYPE-18 OR -19 TRANSGENERATIONS   00006000
C    NTRAN=  NEW VARIABLE AFTER TRANSGENERATION                         00006100
C    OUTPUT= USED FOR ALPHABETIC LISTING OF METHODS OF DATA OUTPUT      00006200
C    PRINT=  VARIABLE PRINT FORMAT                                      00006300
C    PUNCH=  VARIABLE PUNCH FORMAT                                      00006400
C    SAMPL=  USED TO SAVE OLD AND NEW VARIABLES FOR FIRST AND LAST CASES00006500
C    SUM18=  USED FOR SUM OF SQUARES IN TYPE-19 TRANSGENERATIONS        00006600
C    TAPE=   VARIABLE BCD TAPE FORMAT                                   00006700
C    TRAN40= USED FOR TEMPORARY STORAGE OF TYPE-40 CONSTANTS            00006800
CGUIDE TO PROBLEM CARD (WITH LIMITS OF PARAMETER VALUES)-               00006900
C    LPRBM=  IDENTIFIES PROBLEM CARD WITH WORD- PROBLM                  00007000
C    PRBM=   ALPHANUMERIC USER"S PROBLEM NUMBER OR OTHER IDENTIFICATION 00007100
C    NCASE=  NUMBER OF CASES (LIMIT OF 130000)                          00007200
C    NVAR=   NUMBER OF VARIABLES INPUTED (LIMIT OF 999)                 00007300
C    NADVAR= NUMBER OF VARIABLES ADDED BY TRANSGEN.  (NVAR+NADVAR  999) 00007400
C    NPUNCH= NO. OF VARIABLES PUNCHED/ BLANK IF SAME AS NOVAR (LIMIT999)00007500
C    NTRG=   NUMBER OF TRANSGENERATION CARDS(LIMIT 999)                 00007600
C    KINPUT= CRD FOR PUNCHED INPUT, BCD FROM BCD TAPE, BIN FROM BIN.TAPE00007700
C    MIN=    INPUT TAPE NO./ BLANK IF INPUT ON CARDS (LIMIT 16, NOT 5,6)00007800
C    KPRNT=  YES IF PRINT-OUT DESIRED/ OTHERWISE BLANK                  00007900
C    KPNCH=  YES IF PUNCHED CARD OUTPUT DESIRED/ OTHERWISE BLANK        00008000
C    KOUT=   BCD FOR BCD TAPE OUTPUT, BIN FOR BINARY TAPE, IF NONE BLANK00008100
C    MOUT=   OUTPUT TAPE NO./BLANK IF NONE (LIMIT OF 16, NOT 5 OR 6)    00008200
C    JIN=    NO. OF VARIABLE INPUT FORMAT CARDS/ BLANK IF BINARY INPUT  00008300
C    JPRT=   NO. OF VARIABLE PRINT FORMAT CARDS/ BLANK IF NO PRINT-OUT  00008400
C    JPNCH=  NO. OF VARIABLE PUNCH FORMAT CARDS/ BLANK IF NO PUNCHING   00008500
C    JTAPE=  NO. OF VARIABLE BCD FORMAT CARDS/ BLANK IF NO BCD OUTPUT   00008600
C                        (ALL VARIABLE FORMATS ARE LIMITED TO 10 CARDS) 00008700
CLIST OF VARIABLES NOT APPEARING IN DIMENSION OR PROBLEM CARD LISTS-    00008800
C    IFPNCH  WHETHER TO PUNCH OUTPUT                                    00008900
C    IFPRT   WHETHER TO PRINT OUTPUT                                    00009000
C    IMIN2   WHETHER TO SET MIN=2 AFTER INPUT IS WRITTEN ON SCRATCH TAPE00009100
C    INOFMT  WHETHER TO READ IN INPUT FORMAT CARDS                      00009200
C    INSAVE  WHETHER TO SAVE NEW VARIABLES                              00009300
C    INTAP   WHETHER BINARY OR BCD TAPE READ DURING MAIN DATA PASS      00009400
C    INTAPE  WHETHER BINARY OR BCD TAPE READ DURING PREPASS             00009500
C    IPRINT  WHETHER TO READ IN PRINT FORMAT CARDS                      00009600
C    IPUNCH  WHETHER TO READ IN PUNCH FORMAT CARDS                      00009700
C    IREWND  WHETHER TO REWIND INPUT TAPE                               00009800
C    ISAVE   WHETHER TO CONTINUE MAIN LOOP OR BRANCH OUT LAST TIME      00009900
C    ISPECL  WHETHER TO SAVE OLD VARIABLES FOR LAST CASE                00010000
C    ITAPE   WHETHER BINARY OR BCD OR NO TAPE OUTPUT                    00010100
C    IWRTAP  WHETHER TO READ IN BCD TAPE FORMAT CARDS                   00010200
C    IWRIT2  WHETHER TO WRITE PUNCHED INPUT ON SCRATCH TAPE AT PREPASS  00010300
C    I1SAVE  WHETHER TO SAVE OLD VARIABLES                              00010400
C    KARD,KBCD,KBIN,KEND,KPRBM,KTRA,KYES   SEE EQUIVALENCE STATEMENT    00010500
C    NCAS    (NO. OF CASES)-1                                           00010600
C    NLOC    USED TO POSITION NAMES OF TYPES OF OUTPUT                  00010700
C    NLOC18  THE LOCATION OF CARDS WITH TYPE-18 OR 19 TRANSGENERATIONS  00010800
C    NOVAR   NUMBER OF VARIABLES AFTER TRANSGENERATION                  00010900
C    NUM18   COUNTS THE NUMBER OF TYPE-18 AND -19 TRANSGENERATIONS      00011000
C    N18     THE VARIABLE USED IN A TYPE-18 OR -19 TRANSGENERATION      00011100
C    N40     COUNTS THE NUMBER OF TYPE-40 TRANSGENERATIONS              00011200
C    TEMP,TEMP1   NAME OF TYPE OF DATA INPUT                            00011300
CGUIDE TO STATEMENT NUMBERING-                                          00011400
C    200"S     TRANSFERS FROM IF AND COMPUTED GOTO STATEMENTS           00011500
C    300"S     TRANSFERS FROM UNCONDITIONAL GOTO STATEMENTS             00011600
C    400"S     END OF DO LOOPS  (ORDERED BY THE ORDER OF BEGINNING NOT  00011700
C    500"S     TRANSFERS FROM CHECKS ON PROBLM CARD       (END OF LOOPS)00011800
C    600"S     TRANSFERS FROM ASSIGNED GOTO STATEMENTS                  00011900
C    900"S     ERROR OUTPUTS                                            00012000
C    950"S     FORMAT STATEMENTS FOR ERROR OUTPUTS                      00012100
C    1000"S    FORMAT STATEMENTS FOR WRITE OUTPUTS                      00012200
C    2000"S    FORMAT STATEMENTS FOR READ INPUTS                        00012300
      DIMENSION CONS(7,50),CONST(999),DATA(1500),FMT(120),KODE(999),NCON00012400
     140(50),NOTRAN(999),NSUM18(999),NTRAN(999),OUTPUT(6),PRINT(120),PUN00012500
     2CH(120),SAMPL(3,999),SUM18(999),TAPE(120),TRAN40(7)               00012600
      COMMON CONS,CONST,DATA,KODE,NCASE,NCON40,NOTRAN,NTRAN,NTRG,NVAR   00012700
      EQUIVALENCE (A123,KPRBM),(B123,KEND),(C123,KTRA),(D123,KYES),(E12300012800
     1,KBCD),(F123,KBIN),(G123,KARD)                                    00012900
C                                                                       00013000
 1001 FORMAT(39H1BMD09S - TRANSGENERATION - VERSION OF                  00013100
     X13HAUG. 25, 1964/                                                 00013200
     X41H HEALTH SCIENCES COMPUTING FACILITY, UCLA//)                   00013300
C                                                                       00013400
      MIN1=5                                                            00013500
      MOUT1=6                                                           00013600
      A123=(+6HPROBLM)                                                  00013700
      B123=(+6HFINISH)                                                  00013800
      C123=(+6HTRNGEN)                                                  00013900
      D123=(+3HYES)                                                     00014000
      E123=(+3HBCD)                                                     00014100
      F123=(+3HBIN)                                                     00014200
      G123=(+3HCRD)                                                     00014300
  888 READ           (5,2001)LPRBM,      PRBM,NCASE,NVAR,NADVAR,NPUNCH, 00014400
     1NTRG,KINPUT,MIN,KPRNT,KPNCH,KOUT,MOUT,JIN,JPRT,JPNCH,JTAPE        00014500
 2001 FORMAT(2A6,I6,I3,I4,2I3,A3,I2,3A3,I2,17X,4I2)                     00014600
      IF(LPRBM-KEND)500,999,500                                         00014700
  500 IF(LPRBM-KPRBM)903,501,903                                        00014800
  501 WRITE            (6,1001)                                         00014900
      NOVAR=NVAR+NADVAR                                                 00015000
      WRITE            (6,1002)      PRBM,NCASE,NVAR,NOVAR,NTRG         00015100
 1002 FORMAT(1H    18X,12HPROBLEM NO. A6/22H0THIS PROBLEM CONTAINSI6,29H00015200
     1 CASES.  INITIALLY THERE ARE I3,34H VARIABLES, AFTER TRANSGENERATI00015300
     2ON-I3,11H VARIABLES./1H I3,33H TRANSGENERATIONS ARE TO BE MADE.)  00015400
      REWIND 2                                                          00015500
      ASSIGN 1000 TO IREWND                                             00015600
      ASSIGN 6093 TO KSKIP                                              00015700
      ASSIGN 2475 TO ISKIP                                              00015800
      IF(NCASE) 903,903,502                                             00015900
  502 IF(NVAR) 903,903,503                                              00016000
  503 IF(NOVAR) 903,903,504                                             00016100
  504 IF(NTRG)903,5045,505                                              00016200
 5045 ASSIGN 6096 TO KSKIP                                              00016300
      ASSIGN 280  TO ISKIP                                              00016400
  505 IF(NOVAR-999) 506,506,903                                         00016500
  506 IF(NCASE-130000)507,507,903                                       00016600
C                                                                       00016700
C                                                                       00016800
CERROR OUTPUTS                                                          00016900
C                                                                       00017000
  900 WRITE            (6,950)                                          00017100
  950 FORMAT(48H0AN IMPROPER INPUT OR OUTPUT TAPE WAS SPECIFIED.)       00017200
      GO TO 1000                                                        00017300
  901 WRITE            (6,951)                                          00017400
  951 FORMAT(49H0NO METHOD OF INPUT OR OUTPUT HAS BEEN SPECIFIED.)      00017500
      GO TO 1000                                                        00017600
  902 WRITE            (6,952)                                          00017700
  952 FORMAT(82H0AN IMPROPER NUMBER OF FORMAT CARDS ARE CALLED FOR SOME 00017800
     1INPUT OR OUTPUT OPERATION.)                                       00017900
      GO TO 1000                                                        00018000
  903 WRITE            (6,953)                                          00018100
  953 FORMAT(47H0CONTROL CARDS MISPUNCHED OR CARDS OUT OF ORDER)        00018200
      GO TO 1000                                                        00018300
  904 WRITE            (6,954)                                          00018400
  954 FORMAT(16H0A PROGRAM ERROR)                                       00018500
      GO TO 1000                                                        00018600
C                                                                       00018700
CEND OF PROGRAM                                                         00018800
C                                                                       00018900
  999 IF(MOUT1-6)297,299,297                                            00019000
 297  END FILE MOUT1                                                    00019100
      IF(MOUT1-6)296,299,298                                            00019200
 296  REWIND MOUT1                                                      00019300
      GO TO 299                                                         00019400
C298  CALL REMOVE(MOUT1)                                                00019500
  298 CONTINUE                                                          00019600
  299 GO TO IREWND,(699,1000)                                           00019700
  699 IF(MIN1-5)1000,1000,687                                           00019800
C 687 CALL REMOVE(MIN1)                                                 00019900
  687 CONTINUE                                                          00020000
 1000 CALL EXIT                                                         00020100
C                                                                       00020200
                                                                        00020300
CPART 1                                                                 00020400
C                                                                       00020500
  507 IF(KINPUT-KARD) 201,200,201                                       00020600
  200 TEMP=(+6HPUNCH )                                                  00020700
      TEMP1=(+6HCARDS )                                                 00020800
      MIN=5                                                             00020900
      ASSIGN 600 TO INOFMT                                              00021000
      ASSIGN 604 TO IWRIT2                                              00021100
      GO TO 206                                                         00021200
  201 IF(KINPUT-KBIN)203,202,203                                        00021300
  202 TEMP=(+6HBINARY)                                                  00021400
      TEMP1=(+6H TAPE )                                                 00021500
      ASSIGN 601 TO INOFMT                                              00021600
      ASSIGN 602 TO INTAPE                                              00021700
      ASSIGN 607 TO INTAP                                               00021800
      IF(JIN)902,205,902                                                00021900
  203 IF(KINPUT-KBCD)901,204,901                                        00022000
  204 TEMP=(+6H   BCD)                                                  00022100
      TEMP1=(+6H TAPE )                                                 00022200
      ASSIGN 600 TO INOFMT                                              00022300
  205 ASSIGN 605 TO IWRIT2                                              00022400
      ASSIGN 699 TO IREWND                                              00022500
      IF(MIN*(MIN-17))206,900,900                                       00022600
  206 WRITE            (6,1003)TEMP,TEMP1,MIN                           00022700
 1003 FORMAT(17H0INPUT DATA FROM 2A6,9H ON TAPE I2)                     00022800
      GO TO INOFMT,(600,601)                                            00022900
 600  CALL VFCHCK(JIN)                                                  00023000
  208 JIN=JIN*12                                                        00023100
      ASSIGN 603 TO INTAPE                                              00023200
      ASSIGN 606 TO INTAP                                               00023300
      READ           (5,2002)(FMT(I),I=1,JIN)                           00023400
 2002 FORMAT(12A6)                                                      00023500
      WRITE            (6,1004)(FMT(I),I=1,JIN)                         00023600
 1004 FORMAT(21H THE INPUT FORMAT IS-/(1H 12A6))                        00023700
C                                                                       00023800
                                                                        00023900
CPART 2                                                                 00024000
C                                                                       00024100
  601 NLOC=2                                                            00024200
      IPRINT=0                                                          00024300
      IPUNCH=0                                                          00024400
      IWRTAP=3                                                          00024500
      IF(KPRNT-KYES)211,209,211                                         00024600
 209  CALL VFCHCK(JPRT)                                                 00024700
  210 OUTPUT(1)=(+6H PRINT)                                             00024800
      OUTPUT(2)=(+6H,     )                                             00024900
      NLOC=NLOC+2                                                       00025000
      IPRINT=1                                                          00025100
      JPRT=JPRT*12                                                      00025200
      ASSIGN 612 TO IFPRT                                               00025300
      GO TO 312                                                         00025400
  211 ASSIGN 613 TO IFPRT                                               00025500
  312 IF(KPNCH-KYES) 216,213,216                                        00025600
 213  CALL VFCHCK(JPNCH)                                                00025700
  214 OUTPUT(NLOC-1)=(+6HPUNCH )                                        00025800
      OUTPUT(NLOC)=(+6HCARDS,)                                          00025900
      NLOC=NLOC+2                                                       00026000
      IPUNCH=1                                                          00026100
      JPNCH=JPNCH*12                                                    00026200
      ASSIGN 614 TO IFPNCH                                              00026300
      IF(NPUNCH)215,215,217                                             00026400
  215 NPUNCH=NOVAR                                                      00026500
      GO TO 217                                                         00026600
  216 ASSIGN 615 TO IFPNCH                                              00026700
  217 IF(KOUT-KBIN) 219,218,219                                         00026800
  218 OUTPUT(NLOC-1)=(+6HBINARY)                                        00026900
      OUTPUT(NLOC)=(+6H TAPE )                                          00027000
      IWRTAP=2                                                          00027100
      ASSIGN 617 TO ITAPE                                               00027200
      IF(JTAPE) 902,222,902                                             00027300
  219 IF(KOUT-KBCD) 224,220,224                                         00027400
 220  CALL VFCHCK(JTAPE)                                                00027500
  221 OUTPUT(NLOC-1)=(+6H   BCD)                                        00027600
      OUTPUT(NLOC)=(+6H TAPE )                                          00027700
      IWRTAP=1                                                          00027800
      JTAPE=JTAPE*12                                                    00027900
      ASSIGN 616 TO ITAPE                                               00028000
  222 IF(MOUT*(MOUT-17)) 223,900,900                                    00028100
  223 IF((MOUT-5)*(MOUT-6)*(MOUT- 2))225,900,225                        00028200
  224 ASSIGN 618 TO ITAPE                                               00028300
      NLOC=NLOC-2                                                       00028400
      IF(IPRINT+IPUNCH) 904,901,227                                     00028500
  225 CALL TPWD2(MOUT,MOUT1)                                            00028600
  227 WRITE            (6,1005)(OUTPUT(I),I=1,NLOC)                     00028700
 1005 FORMAT(17H0DATA OUTPUT IS- 3(2A6,1X))                             00028800
      CALL TPWD(MIN,MIN1)                                               00028900
      GO TO(231,231,232), IWRTAP                                        00029000
  231 WRITE            (6,1006) OUTPUT(NLOC-1),OUTPUT(NLOC),MOUT        00029100
 1006 FORMAT(16H OUTPUT TAPE IS 2A6,3HNO.I3)                            00029200
  232 IF(IPRINT) 904,241,240                                            00029300
  240 WRITE            (6,1007)                                         00029400
 1007 FORMAT(21H0THE PRINT FORMAT IS-)                                  00029500
      READ           (5,2002)(PRINT(I),I=1,JPRT)                        00029600
      WRITE            (6,1008)(PRINT(I),I=1,JPRT)                      00029700
 1008 FORMAT(1H 12A6)                                                   00029800
  241 IF(IPUNCH) 904,244,242                                            00029900
  242 WRITE            (6,1009)                                         00030000
 1009 FORMAT(35H0THE PUNCHED CARD OUTPUT FORMAT IS-)                    00030100
      READ           (5,2002)(PUNCH(I),I=1,JPNCH)                       00030200
      WRITE            (6,1008)(PUNCH(I),I=1,JPNCH)                     00030300
  244 GO TO (245,247,247),IWRTAP                                        00030400
  245 WRITE            (6, 1010)                                        00030500
 1010 FORMAT(31H0THE BCD OUTPUT TAPE FORMAT IS-)                        00030600
      READ           (5,2002)(TAPE(I),I=1,JTAPE)                        00030700
      WRITE            (6,1008)(TAPE(I),I=1,JTAPE)                      00030800
C                                                                       00030900
                                                                        00031000
CPART 3                                                                 00031100
C                                                                       00031200
  247 NUM18=0                                                           00031300
      N40=0                                                             00031400
      GO TO ISKIP,(2475,280)                                            00031500
 2475 WRITE            (6, 1011)                                        00031600
 1011 FORMAT(1H06X21H TRANSGENERATOR CARDS/5H0CARD4X3HNEW5X5HTRANS4X    00031700
     1 20HORIG.   ORIG. VAR(B)10X,17HTYPE 40 CONSTANTS/45H  NO. VARIABLE00031800
     2   CODE    VAR(A)   OR CONSTANT)                                  00031900
      DO 400 I=1,NTRG                                                   00032000
      READ           (5,2003)LTRGN,NTRAN(I),KODE(I),NOTRAN(I),CONST(I), 00032100
     1NCONS,(TRAN40(J),J=1,7)                                           00032200
 2003 FORMAT(A6,I3,I2,I3,F6.0,5X,I1,7F6.0)                              00032300
      IF(LTRGN-KTRA) 903,248,903                                        00032400
  248 IF(KODE(I)*(KODE(I)-27))249,905,251                               00032500
  249 IF((KODE(I)-18)*(KODE(I)-19)) 255,250,255                         00032600
  250 NUM18=NUM18+1                                                     00032700
      NSUM18(NUM18)=I                                                   00032800
      SUM18(I)=0                                                        00032900
      CONST(I)=0                                                        00033000
      GO TO 255                                                         00033100
 251  IF(KODE(I)-40)905,252,2515                                        00033200
 2515 IF(KODE(I)-42)255,252,254                                         00033300
  252 IF(NCONS*(NCONS-8)) 253,905,905                                   00033400
  253 N40=N40+1                                                         00033500
      DO 401 II=1,NCONS                                                 00033600
  401 CONS(II,N40)=TRAN40(II)                                           00033700
      NCON40(N40)=NCONS                                                 00033800
      WRITE            (6,1012)I,NTRAN(I),KODE(I),NOTRAN(I),CONST(I),   00033900
     1(CONS(II,N40),II=1,NCONS)                                         00034000
 1012 FORMAT(1H ,I3,I8,2I9,F15.5,5X5F14.5/50X,2F14.5)                   00034100
      GO TO 400                                                         00034200
 254  IF(KODE(I)-43)905,255,905                                         00034300
  905 WRITE            (6,955)                                          00034400
  955 FORMAT(112H0THE FOLLOWING TRANSGENERATION CARD HAS ILLEGAL CODE NU00034500
     1MBER OR NUMBER OF TYPE-40 CONSTANTS.  IT WILL BE SKIPPED.)        00034600
      WRITE            (6,1013)I,NTRAN(I),KODE(I),NOTRAN(I),CONST(I)    00034700
 1013 FORMAT(1H ,I3,I8,2I9,F15.5)                                       00034800
      WRITE            (6, 1020)                                        00034900
 1020 FORMAT(1H0)                                                       00035000
      KODE(I)=44                                                        00035100
      GO TO 400                                                         00035200
  255 WRITE            (6,1013)I,NTRAN(I),KODE(I),NOTRAN(I),CONST(I)    00035300
  400 CONTINUE                                                          00035400
      WRITE            (6, 1020)                                        00035500
      IF(N40-50) 260,260,906                                            00035600
  906 WRITE            (6,956)                                          00035700
  956 FORMAT(65H0MORE THAN 50 TYPE-40 TRANSGENERATIONS.  PROGRAM CANNOT 00035800
     1CONTINUE.)                                                        00035900
      GO TO 999                                                         00036000
  260 IF(NUM18) 904,280,261                                             00036100
C                                                                       00036200
                                                                        00036300
CPART 4                                                                 00036400
C                                                                       00036500
  261 ASSIGN 624 TO IMIN2                                               00036600
      DO 402 I=1,NCASE                                                  00036700
      GO TO INTAPE,(602,603)                                            00036800
  602 READ     (MIN)(DATA(II),II=1,NVAR)                                00036900
      GO TO 605                                                         00037000
  603 READ           (MIN,FMT)(DATA(II),II=1,NVAR)                      00037100
      GO TO IWRIT2,(604,605,622)                                        00037200
  604 ASSIGN 622 TO IWRIT2                                              00037300
      ASSIGN 623 TO IMIN2                                               00037400
  622 WRITE     (2)(DATA(II),II=1,NVAR)                                 00037500
  605 DO 402 I18=1,NUM18                                                00037600
      NLOC18=NSUM18(I18)                                                00037700
      N18=NOTRAN(NLOC18)                                                00037800
      IF(KODE(NLOC18)-18) 904,402,275                                   00037900
  275 SUM18(NLOC18)=DATA(N18)*DATA(N18)+SUM18(NLOC18)                   00038000
  402 CONST(NLOC18)=DATA(N18)+CONST(NLOC18)                             00038100
      GO TO IMIN2,(623,624)                                             00038200
  623 MIN=2                                                             00038300
      ASSIGN 607 TO INTAP                                               00038400
      ASSIGN 699 TO IREWND                                              00038500
      END FILE MIN                                                      00038600
  624 REWIND MIN                                                        00038700
      DO 403 I18=1,NUM18                                                00038800
      NLOC18=NSUM18(I18)                                                00038900
      CASE=NCASE                                                        00039000
      IF(KODE(NLOC18)-18) 904,277,278                                   00039100
  277 CONST(NLOC18)=CONST(NLOC18)/CASE                                  00039200
      GO TO 403                                                         00039300
  278 CONST(NLOC18)=(SUM18(NLOC18)-((CONST(NLOC18)*CONST(NLOC18))/CASE))00039400
     1/(CASE-1.0)                                                       00039500
      CONST(NLOC18)=SQRT (CONST(NLOC18))                                00039600
  403 CONTINUE                                                          00039700
C                                                                       00039800
                                                                        00039900
CPART 5                                                                 00040000
C                                                                       00040100
      DO 404 K=1,NTRG                                                   00040200
      DO 404 KK=1,NUM18                                                 00040300
      NLOC18=NSUM18(KK)                                                 00040400
      IF(NTRAN(K)-NOTRAN(NLOC18)) 404,279,404                           00040500
  279 IF(K-NLOC18) 379,404,404                                          00040600
  379 IF(KODE(K)-18) 907,404,907                                        00040700
  907 WRITE            (6, 957)K,NTRAN(K),KODE(NLOC18),NSUM18(KK),KODE(N00040800
     1LOC18)                                                            00040900
  957 FORMAT(25H0TRANSGENERATION CARD NO.I4,17H CHANGES VARIABLEI4,     00041000
     139H BEFORE THAT VARIABLE IS USED IN A TYPEI3/44H TRANSGENERATION O00041100
     2N TRANSGENERATION CARD NO.I4,12H.  THIS TYPEI3,37H TRANSGENERATION00041200
     3 WILL BECOME INVALID.)                                            00041300
  404 CONTINUE                                                          00041400
C                                                                       00041500
                                                                        00041600
CPART 6                                                                 00041700
C                                                                       00041800
  280 ASSIGN 405 TO ISAVE                                               00041900
      ASSIGN 608 TO I1SAVE                                              00042000
      ASSIGN 610 TO INSAVE                                              00042100
      ASSIGN 619 TO ISPECL                                              00042200
      NCAS=NCASE-1                                                      00042300
       NVA=NVAR+1                                                       00042400
      IF(NCAS) 903,281,282                                              00042500
  281 I=1                                                               00042600
      ASSIGN 621 TO ISAVE                                               00042700
      GO TO 383                                                         00042800
  282 DO 405 I=1,NCAS                                                   00042900
  383 DO 406 J=NVA,NOVAR                                                00043000
      SAMPL(1,J)=0.0                                                    00043100
      SAMPL(3,J)=0.0                                                    00043200
  406 DATA(J)=0.0                                                       00043300
      GO TO INTAP,(606,607)                                             00043400
  606 READ           (MIN,FMT)(DATA(II),II=1,NVAR)                      00043500
      GO TO 384                                                         00043600
  607 READ     (MIN)(DATA(II), II=1,NVAR)                               00043700
  384 GO TO ISPECL,(619,620)                                            00043800
  619 GO TO I1SAVE,(608,609)                                            00043900
  608 DO 407 J=1,NVAR                                                   00044000
  407 SAMPL(1,J)=DATA(J)                                                00044100
      ASSIGN 609 TO I1SAVE                                              00044200
      GO TO 609                                                         00044300
  620 DO 408 J=1,NVAR                                                   00044400
  408 SAMPL(3,J)=DATA(J)                                                00044500
  609 GO TO KSKIP,(6093,6096)                                           00044600
 6093 CALL TRANS(I)                                                     00044700
 6096 GO TO INSAVE,(610,611)                                            00044800
  610 DO 409 J=1,NOVAR                                                  00044900
  409 SAMPL(2,J)=DATA(J)                                                00045000
      ASSIGN 611 TO INSAVE                                              00045100
C                                                                       00045200
                                                                        00045300
CPART 7                                                                 00045400
C                                                                       00045500
  611 GO TO IFPRT,(612,613)                                             00045600
  612 WRITE            (6,1014)I                                        00045700
 1014 FORMAT(9H CASE NO.I6)                                             00045800
      WRITE            (6,PRINT)(DATA(J),J=1,NOVAR)                     00045900
  613 GO TO IFPNCH,(614,615)                                            00046000
  614 PUNCH PUNCH,(DATA(J),J=1,NPUNCH)                                  00046100
  615 GO TO ITAPE,(616,617,618)                                         00046200
  616 WRITE            (MOUT)TAPE,(DATA(J),J=1,NOVAR)                   00046300
      GO TO 618                                                         00046400
  617 WRITE     (MOUT)(DATA(J),J=1,NOVAR)                               00046500
  618 GO TO ISAVE,(405,621)                                             00046600
  405 CONTINUE                                                          00046700
      ASSIGN 620 TO ISPECL                                              00046800
      ASSIGN 621 TO ISAVE                                               00046900
      I=NCASE                                                           00047000
      GO TO 383                                                         00047100
C                                                                       00047200
                                                                        00047300
CPART 8                                                                 00047400
C                                                                       00047500
  621 WRITE            (6,1015) NCASE, NCASE                            00047600
 1015 FORMAT(62H1SAMPLE LISTING OF VARIABLES BEFORE AND AFTER TRANSGENER00047700
     1ATION.//13H VARIABLE NO.7X,17H   CASE  1 BEFORE4X,16H   CASE  1 AF00047800
     2TER 12X,6H  CASEI6,7H BEFORE4X,4HCASEI6,6H AFTER/      23X,15HTRAN00047900
     3SGENERATION5X,15HTRANSGENERATION14X,15HTRANSGENERATION5X,15HTRANSG00048000
     4ENERATION/)                                                       00048100
      DO 410 I=1,NOVAR                                                  00048200
  410 WRITE            (6,1016)I,SAMPL(1,I),SAMPL(2,I),SAMPL(3,I),DATA(I00048300
     1)                                                                 00048400
 1016 FORMAT(1H I8,F26.6,F20.6,F30.6,F21.6)                             00048500
      GO TO 888                                                         00048600
      END                                                               00048700
                                                                        00048800
      SUBROUTINE TPWD(NT1,NT2)                                          00048900
CTPWD    SUBROUTINE TPWD FOR BMD09S               DECEMBER 17, 1963     00049000
      IF(NT1)40,10,12                                                   00049100
 10   NT1=5                                                             00049200
 12   IF(NT1-NT2)14,19,14                                               00049300
 14   IF(NT2-5)15,19,17                                                 00049400
   15 REWIND NT2                                                        00049500
      GO TO 19                                                          00049600
C  17 CALL REMOVE(NT2)                                                  00049700
   17 CONTINUE                                                          00049800
   19 IF(NT1-5)18,24,18                                                 00049900
 18   IF(NT1-6)22,40,22                                                 00050000
 22   REWIND NT1                                                        00050100
 24   NT2=NT1                                                           00050200
 28   RETURN                                                            00050300
 40   WRITE            (6,49)                                           00050400
      CALL EXIT                                                         00050500
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              00050600
      END                                                               00050700
                                                                        00050800
      SUBROUTINE TPWD2(NT1,NT2)                                         00050900
CTPWD     SUBROUTINE TPWD2 FOR BMD09S             FEBRUARY 17, 1964     00051000
      IF(NT1-NT2)14,28,14                                               00051100
 14   IF(NT2*(NT2-6))16,22,16                                           00051200
 16   END FILE NT2                                                      00051300
      IF(NT2-4)15,15,17                                                 00051400
   15 REWIND NT2                                                        00051500
      GO TO 22                                                          00051600
C  17 CALL REMOVE(NT2)                                                  00051700
   17 CONTINUE                                                          00051800
 22   REWIND NT1                                                        00051900
 24   NT2=NT1                                                           00052000
 28   RETURN                                                            00052100
      END                                                               00052200
                                                                        00052300
      SUBROUTINE TRANS(NOCASE)                                          00052400
CTRAN  SUBROUTINE TRANS FOR BMD09S-DATA RECODE       APRIL  1, 1964     00052500
      ASNF(XX)=ATAN (XX/SQRT (1.0-XX**2))                               00052600
      DIMENSION CONS(7,50),CONST(999),DATA(1500),KODE(999),NCON40(50),  00052700
     1NOTRAN(999),NTRAN(999)                                            00052800
      COMMON CONS,CONST,DATA,KODE,NCASE,NCON40,NOTRAN,NTRAN,NTRG,NVAR   00052900
      II=0                                                              00053000
      DO 100 I=1,NTRG                                                   00053100
      N=NOTRAN(I)                                                       00053200
      M=NTRAN(I)                                                        00053300
      K=KODE(I)                                                         00053400
      GO TO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,00053500
     124, 25, 26,198,198,198,198,198,198,198,198,198,198,198,198,198,   00053600
     2 170,175,170,175,100),K                                           00053700
    1 IF(DATA(N))198,107,108                                            00053800
  107 DATA(M)=0.0                                                       00053900
      GOTO100                                                           00054000
  108 DATA(M)=SQRT (DATA(N))                                            00054100
      GOTO100                                                           00054200
    2 IF(DATA(N)) 198,111,112                                           00054300
  111 DATA(M)=1.0                                                       00054400
      GOTO100                                                           00054500
  112 DATA(M)=SQRT (DATA(N))+SQRT (DATA(N)+1.0)                         00054600
      GOTO100                                                           00054700
    3 IF(DATA(N)) 198,198,114                                           00054800
  114 DATA(M)= ALOG10(DATA(N))                                          00054900
      GOTO100                                                           00055000
    4 DATA(M)=EXP (DATA(N))                                             00055100
      GOTO100                                                           00055200
    5 IF(DATA(N)) 198,107,117                                           00055300
  117 IF(DATA(N)-1.0)118,119,119                                        00055400
  118 A=SQRT (DATA(N))                                                  00055500
      DATA(M)=ASNF(A)                                                   00055600
      GOTO100                                                           00055700
  119 DATA(M)=3.14159265/2.0                                            00055800
      GOTO100                                                           00055900
    6 FN=NCASE                                                          00056000
      A=DATA(N)/(FN+1.0)                                                00056100
      B=A+1.0/(FN+1.0)                                                  00056200
      IF(A) 198,123,124                                                 00056300
  123 IF(B)198,107,127                                                  00056400
  127 DATA(M)=ASNF(SQRT (B))                                            00056500
      GOTO100                                                           00056600
  124 IF(B)198,128,129                                                  00056700
  128 DATA(M)=ASNF(SQRT (A))                                            00056800
      GOTO100                                                           00056900
  129 A=SQRT (A)                                                        00057000
      B=SQRT (B)                                                        00057100
      DATA(M)=ASNF(A)+ASNF(B)                                           00057200
      GOTO100                                                           00057300
    7 IF(DATA(N))131,198,131                                            00057400
  131 DATA(M)=1.0/DATA(N)                                               00057500
      GOTO100                                                           00057600
    8 DATA(M)=DATA(N) +CONST(I)                                         00057700
      GOTO100                                                           00057800
    9 DATA(M)=DATA(N) *CONST(I)                                         00057900
      GOTO100                                                           00058000
   10 IF(DATA(N))198,107,133                                            00058100
  133 DATA(M)=DATA(N)**CONST(I)                                         00058200
      GOTO100                                                           00058300
   11 NEWB=CONST(I)                                                     00058400
      DATA(M)=DATA(N)+DATA(NEWB)                                        00058500
      GOTO100                                                           00058600
   12 NEWB=CONST(I)                                                     00058700
      DATA(M)=DATA(N)-DATA(NEWB)                                        00058800
      GOTO100                                                           00058900
   13 NEWB=CONST(I)                                                     00059000
      DATA(M)=DATA(N)*DATA(NEWB)                                        00059100
      GOTO100                                                           00059200
   14 NEWB=CONST(I)                                                     00059300
      IF(DATA(NEWB))134,198,134                                         00059400
  134 DATA(M)=DATA(N)/DATA(NEWB)                                        00059500
      GOTO100                                                           00059600
   15 IF(DATA(N)-CONST(I)) 107,111,111                                  00059700
   16 NEWB=CONST(I)                                                     00059800
      IF(DATA(N)-DATA(NEWB))107,111,111                                 00059900
   17 IF(DATA(N)) 198,198,163                                           00060000
  163 DATA(M)=ALOG(DATA(N))                                             00060100
      GO TO 100                                                         00060200
   18 DATA(M)=DATA(N)-CONST(I)                                          00060300
      GO TO 100                                                         00060400
   19 DATA(M)=DATA(N)/CONST(I)                                          00060500
      GO TO 100                                                         00060600
   20 DATA(M)=SIN (DATA(N))                                             00060700
      GO TO 100                                                         00060800
   21 DATA(M)=COS (DATA(N))                                             00060900
      GO TO 100                                                         00061000
   22 IF(DATA(N)-1.57079632) 186,186,198                                00061100
  186 IF(DATA(N)+1.57079632) 198,187,187                                00061200
  187 DATA(M)=  ATAN (DATA(N))                                          00061300
      GO TO 100                                                         00061400
   23 NEWB=CONST(I)                                                     00061500
      IF(DATA(N)) 198,198,188                                           00061600
  188 DATA(M)=DATA(N)**DATA(NEWB)                                       00061700
      GO TO 100                                                         00061800
   24 IF(CONST(I)) 198,198,189                                          00061900
  189 DATA(M)=CONST(I)**DATA(N)                                         00062000
      GO TO 100                                                         00062100
   25 DATA(M)=DATA(N)                                                   00062200
      GO TO 100                                                         00062300
   26 DATA(M)=CONST(I)                                                  00062400
      GO TO 100                                                         00062500
  170 II=II+1                                                           00062600
      L=NCON40(II)                                                      00062700
      DO 166 J=1,L                                                      00062800
      IF(DATA(N)-CONS(J,II))166,165,166                                 00062900
  165 C=SIGN (1.0,DATA(N))                                              00063000
      D=SIGN (1.0,CONS(J,II))                                           00063100
      IF(C+D) 167,166,167                                               00063200
  166 CONTINUE                                                          00063300
      GO TO 100                                                         00063400
  175 IF(DATA(N)) 100,168,100                                           00063500
  168 C=SIGN (1.0,DATA(N))                                              00063600
      D=SIGN (1.0,I)                                                    00063700
      IF(C+D)  100,167,100                                              00063800
  167 NEWB=CONST(I)                                                     00063900
      JUMP=K-39                                                         00064000
      GO TO (26,26,43,43),JUMP                                          00064100
   43 DATA(M)=DATA(NEWB)                                                00064200
      GO TO 100                                                         00064300
  198 WRITE            (6,201)N,NOCASE,K,M                              00064400
  100 CONTINUE                                                          00064500
  201 FORMAT(22H0THE VALUE OF VARIABLEI4,8H IN CASEI5,54H VIOLATED THE R00064600
     1ESTRICTIONS FOR TRANSGENERATION OF TYPEI3,1H./39H THE PROGRAM CONT00064700
     2INUED LEAVING VARIABLEI4,25H UNCHANGED FOR THIS CASE.)            00064800
      RETURN                                                            00064900
      END                                                               00065000
                                                                        00065100
      SUBROUTINE VFCHCK(NVF)                                            00065200
CVFCH      SUBROUTINE TO CHECK FOR PROPER NUMBER OF VARIABLE FORMAT CRDS00065300
      IF(NVF)10,10,20                                                   00065400
 10   WRITE            (6,4000)                                         00065500
      NVF=1                                                             00065600
 50   RETURN                                                            00065700
C                                                                       00065800
 20   IF(NVF-10)50,50,10                                                00065900
C                                                                       00066000
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIF00066100
     XIED, ASSUMED TO BE 1.)                                            00066200
      END                                                               00066300
