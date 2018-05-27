         BEGIN                                                          00000000
COMMENT        MISDATA/STATMAN JAN 21,1969;                             00000100
COMMENT                                                                 00000200
          THE FOLLOWING CORRELATION PROGRAM WAS DESIGNED TO COMPUTE N,  00000300
SUM XSQ, SUM YSQ, SUM Y, SUM X, AND SUMXY FOR UP TO 1023 SCORES (NVAR)  00000400
AND AS MANY CASES (NCASE) AS WILL FIT ON ONE MAGNETIC TAPE.  THE PROGRAM00000500
WILL ACCEPT DATA FROM TAPE (FILE"MASTER") - THIS FILE MUST BE GENERATED 00000600
VIA SOME OTHER PROGRAM AND MUST CONTAIN ONE CASE PER RECORD (NO BLOCKING00000700
FACTOR) AND ONE VARIABLE OR SCORE PER COMPUTER WORD (FLT.PT.BINARY),    00000800
THEREFORE 65 SCORES AND 500 CASES WILL BE CONTAINED ON A TAPE COMPOSED  00000900
OF 500 RECORDS (NO BLOCKING) AND 65 WRDS/REC. A MISSING SCORE IS        00001000
INDICATED BY THE CHARACTERS "NULL" (45644343-RIGHT JUSTIFIED) IN THE    00001100
PLACE OF A SCORE-THESE CHARACTERS MUSTBE WRITTEN ON TAPE BY THE EDITING 00001200
PROGRAM.  ANY "NULL" SCORE IS NOT CONSIDERED IN THE CORRELATION.        00001300
          PARTITIONING OF THE MATRIX IS AUTOMATIC AND IS DEPENDENT ON   00001400
THE AMOUNT OF MEMORY AVAILABLE (MEM) AND THE NUMBER OF VARIABLES (NVAR).00001500
THE PARTITIONING FACTOR (NPART) IS INDEPENDENT OF THE NUMBER OF CASES.  00001600
NPART THEN DETERMINES THE NUMBER OF TAPE PASSES REQUIRED TO FORM ALL THE00001700
SUMS AFTER EACH PASS OF "MASTER" THE COMPUTED SUMS ARE WRITTEN ONTO TAPE00001800
"SUMS" UNTIL ALL SUMS ARE FORMED, IT IS THEN NECESSARY TO REARRANGE THE 00001900
SUMS AND WRITE THEM ONTO TAPE ARR OR ARR1 (7 WRDS/REC).  THE FIRST WORD 00002000
OF EACH RECORD IS USED INTERNALLY WITHIN THE PROGRAM.  WHEN REARRANGING 00002100
IS FINISHED (PERFORMED IN THE INNER BLOCK) THE RESULTS ARE PRINTED (TWO 00002200
TAPE PASSES REQ).                                                       00002300
          FOR RETART PURPOSES THE PROGRAM HAS BEEN BROKEN INTO 5 PHASES 00002400
DESCRIBED BELOW:                                                        00002500
                                                                        00002600
          PHASE 0 - INITIAL STARTING OF THE PROGRAM, DATA COMES FROM    00002700
                    FILE "MASTER". (FILE PROTECT THIS TAPE)             00002800
                                                                        00002900
          PHASE 1 - THIS PHASE IS ENTERED ANY TIME AFTER FILE SUMS      00003000
                    IS OPENED.  THIS PHASE READS MASTER AND WRITES SUMS.00003100
       A CARD IS PUNCHED AT THE END OF EVERY TAPE WRITE FOR FILE "SUMS" 00003200
(EXCEPT THE LAST ONE).  THE PUNCH IS THEN CLOSED AND REOPENED WHEN      00003300
WHEN AGAIN NEEDED.  THE CARD THAT IS PUNCHED IS REQUIRED FOR ANY RESTART00003400
DURING PHASE 1 AND INDICATES TO THE PROGRAM WHERE FILE "SUMS" MUST BE   00003500
POSITIONED.  THIS CARD ALONG WITH ITS "LABEL" AND "EOF" CARD MUST FOLLOW00003600
THE "EOF" CARD THAT FOLLOWS THE STANDARD INPUT CARD.  IF A RESTART      00003700
IS MADE DURING ANY OTHER PHASE, THAN THIS SECOND CARD IS NOT REQUIRED.  00003800
       IF NO CARD(S) IS PUNCHED THAN "NPART" EQUALS 1.  IF "NPART" > 1  00003900
THEN (NPART-1) CARDS WILL BE PUNCHED (WITH THE PUNCH FILE OPENED        00004000
AND CLOSED FOR EACH CARD).                                              00004100
       THE LAST CARD PUNCHED SHOULD BE USED IN A RESTART DURING PHASE 1 00004200
ALTHO, ANY OF THE PREVIOUSLY PUNCHED CARDS WOULD ALSO WORK .            00004300
                                                                        00004400
          PHASE 2 - ENTERED AFTER EOT MESSAGE GIVEN FOR SUMS.  FILES    00004500
                    ARR AND/OR ARR1 ARE THEN OPENED.                    00004600
                                       FILE SUMS IS LOCKED AND THE      00004700
                    WRITE RING MUST BE PULLED BEFORE PROCEEDING (SAFETY 00004800
                    PRECAUTION).  THIS PHASE READ SUMS AND WRITES ARR   00004900
                    AND/OR ARR1.                                        00005000
                    ONE PASS IS MADE ON FILE "MASTER" SO THAT THE       00005100
                    DIAGNAL ELEMENTS MAY BE FORMED - "MASTER" MAY       00005200
                    THEN BE REMOVED.  (FOR A PHASE 2 RESTART)           00005300
                    (ALSO ON A PHASE 2 RESTART IT IS NECESSARY TO       00005400
                    PURGE FILES ARR AND/OR ARR1, IF OPENED)             00005500
                                                                        00005600
          PHASE 3 - ENTERED AFTER FILE ARR OR ARR1 IS LOCKED.  REMOVE   00005700
                    WRITE RING-AS PER SPO MESSAGE-TO PROCEED.  SUM XY,  00005800
                    N, ETC IS READ FROM TAPE AND PRINTED FOR ALL        00005900
                    COMBINATIONS.                                       00006000
                                                                        00006100
          PHASE 4 - ENTERED AFTER ALL SUMS HAVE BEEN PRINTED (IDENTITY  00006200
                    MATRIX IS LAST ONE PRINTED).  MEANS, STANDARD       00006300
                    DEVIATION, ETC IS NOW COMPUTED AND PRINTED BY MAKING00006400
                    AN ADDITIONAL PASS OF TAPE ARR OR ARR1.             00006500
                                                                        00006600
          ONE DATA CARD OF THE FOLLOWING FORMAT IS READ:                00006700
           (ALL COLUMN NUMBERS ARE INCLUSIVE)                           00006800
               1.  COLS 1-6  - NO. OF CASES (NO. OF RECORDS) - RIGHT    00006900
                               JUSTIFIED.                               00007000
               2.  COLS 7-12 - NO. OF VARIABLES (WDS/REC) - RIGHT       00007100
                               JUSTIFIED                                00007200
               3.  COL 24    - RESTART NO. (0,1,2,3, OR 4)              00007300
               4.  COL 30-34 - MEMORY SIZE                              00007400
               5.  COL 40-43, A NONBLANK CHARACTER ANYWHERE IN THIS     00007500
                              FIELD INHIBITS THE PRINTING OF PHASE 3    00007600
                              (PHASE 4 IS STILL PRINTED).  IF THIS FIELD00007700
                              IS BLANK THAN BOTH PHASE 3 AND 4 ARE      00007800
                              PRINTED.                                  00007900
               6.  COL 45-48 - I (RIGHT JUSTIFIED) A NONZERO INDICATES  00008000
                               THAT ONLY ALL I,J COMBINATIONS WILL BE   00008100
                               FORMED AND PRINTED FOR I HELD CONSTANT   00008200
                               AND J=I,...NVAR (NO. OF VARIABLES)       00008300
                               NAMELY, IF THIS FIELD CONTAINS A 35 THEN 00008400
                               COMBINATIONS 35,35  35,36  35,37  35,38 .00008500
                               ........35,NVAR-1,  35,NVAR ARE FORMED   00008600
                               AND PRINTED. THIS FIELD AND COLS 50-53   00008700
                               MAY NOT BOTH BE NONBLANK ON THE SAME RUN.00008800
                               THIS OPTION IS IGNORED IF THE FIELD IS   00008900
                               BLANK OR ZERO.                           00009000
               7.  COL 50-53 - J (RIGHT JUSTIFIED).  A NONZERO INDICATES00009100
                               THAT ONLY ALL I,J COMBINATIONS WILL BE   00009200
                               FORMED AND PRINTED FOR J HELD CONSTANT   00009300
                               AND I=1,2 ....J. NAMELY, IF THIS FIELD   00009400
                               CONTAINS A 35 THE COMBINATIONS 1,35  2,3500009500
                               3,35  4,35  5,35  ......  34,35  35,35   00009600
                               ARE COMPUTED AND RESULTS PRINTED.  IF    00009700
                               THIS FIELD IS BLANK OR A 1 THEN THE      00009800
                               STANDARD CORRELATION FOR ALL COMBINATIONS00009900
                               ARE COMPUTED.  RESTART NUMBER IS IGNORED 00010000
                               IF THIS FIELD IS NONBLANK                00010100
                               THIS FIELD AND COLS 45-48 MAY NOT BOTH BE00010200
                               NONBLANK ON THE SAME RUN.                00010300
               8.  COL 54-58 - CORROUT, A BOOLEAN VARIABLE.             00010400
                              IF TRUE, THEN FULL CORRELATION MATRIX     00010500
                               IS WRITTEN ON TAPE LABELLED "CORMAT"     00010600
                               WITH 10-DAY SAVE DATE.                   00010700
          ******* OPTIONS 6 AND 7 ARE PRIMARILY PROVIDED FOR DATA       00010800
          ******* DEBUGING INCASE  CERTAIN SCORES ARE THOUGHT TO BE IN  00010900
          ******* ERROR                                                 00011000
                                                                        00011100
          AFTER ITEMS 1,2, AND 4 HAVE BEEN ESTABLISHED, THEY MAY NOT BE 00011200
CHANGED ON ANY RESTART.                                                 00011300
          ITEM 5 MAY BE CHANGED ON ANY RESTART.  IF COL 40-43 IS NON-   00011400
          BLANK (OR CONTAINS A NONBLANK CHAR.) AND PHASE 3 IS INDICATED 00011500
          ON THE RESTART CARD, THEN PHASE 3 IS PRINTED ANYWAY.  FOR     00011600
          A RESTART NO. OF 0,1, OR 2 AND ITEM 5 NONBLANK THAN PHASE 3   00011700
          IS NOT PRINTED.                                               00011800
                                                                        00011900
APPROX. RUNNING TIME FOR A PROBLEM OF 500 CASES AND 112 VARIABLES       00012000
IS 1 HOUR AND 30 MIN.  PHASES 0,1, AND 2 ARE COMPUTE BOUND AND RAN      00012100
FOR ABOUT 1 HOUR AND 10 MIN,  PHASES 3 AND 4 ARE PRINT BOUND AND ACCOUNT00012200
FOR THE REMAINDER OF THE TIME.                                          00012300
                                    R.J.HAM - BURROUGHS CORP.         ; 00012400
                                                                        00012500
INTEGER            NVAR,NVAR1,MEM,LLIM,ULIM,T1,T2,I,J,TEMP,             00012600
                   REQMEN,REM,NP,NCASE,K   ;                            00012700
INTEGER            NPART,MEM1,REQMEM             ;                      00012800
INTEGER            RSTT;                                                00012900
LABEL              OK,CHECK  ;                                          00013000
ALPHA              PRINTORNOT;                                          00013100
ALPHA          TITLE1,TITLE2,TITLE3;                                    00013200
FILE OUT           LINE 4  (2,15);                                      00013300
FILE IN            CARD (1,10);                                         00013400
FILE           TEM  2(1,200);                                           00013500
BOOLEAN        CORROUT;                                                 00013600
LABEL          STP;                                                     00013700
INTEGER        ELEMNO;                                                  00013800
INTEGER        ELEMN;                                                   00013900
FILE OUT           PUNCH 0 (1,10);                                      00014000
FORMAT             FER(I6);                                             00014100
FORMAT         FTITLE(/X45,3A6/);                                       00014200
FORMAT             FIN (2I6,X11,I1,X5,I5,X5,A4,X1,I4,X1,I4,L5,X1,3A6);  00014300
PROCEDURE DATELINE(PROGRAM);VALUE PROGRAM;ALPHA PROGRAM;BEGIN OWN BOOLEA00014400
N USED;FORMAT HD(A4,I3,", ",A4,X2,"TIME:",I5,X10,"OUTPUT FROM PROGRAM ",00014500
A6,X10,"UNIVERSITY OF DENVER COMPUTING CENTER"   ),LAYT(//"EXECUTION TIM00014600
E =",I5,X03,"I/O TIME =",I5," SECONDS  ",A4,I3,", ",A4,X03,"TIME:",I7///00014700
);LABEL GOTIT;ALPHA MO,MINS,FEB,HRS,YR,DAY;USED~USED AND PROGRAM.[18:6]=00014800
0;DAY~TIME(0);YR~DAY.[18:12]+"1900";DAY~DAY.[42:6]+10|DAY.[36:6]+100|DAY00014900
.[30:6];FEB~IF YR.[42:6]MOD 4=0 THEN"(FEB."ELSE"&FEB.";FOR MO~"~JAN.",FE00015000
B,"~MAR.","<APR.","~ MAY","<JUNE","~JULY","~AUG.","<SEPT","~OCT.","<NOV.00015100
","~DEC."DO BEGIN IF DAY{MO.[18:06]THEN GO TO GOTIT;DAY~DAY-MO.[18:6];EN00015200
D;GOTIT:MINS~TIME(1)/3600;HRS~100|(MINS DIV 60)+MINS MOD 60;IF USED THEN00015300
 WRITE(LINE,LAYT,TIME(2)/60,TIME(3)/60,MO,DAY,YR,HRS)ELSE WRITE(LINE,HD,00015400
MO,DAY,YR,HRS,PROGRAM);USED~TRUE;END OF DATELINE;                       00015500
                   READ(CARD,FIN,NCASE,NVAR,RSTT,MEM,PRINTORNOT,ELEMN,  00015600
                   ELEMNO,CORROUT,TITLE1,TITLE2,TITLE3);                00015700
               ELEMNO~ELEMNO-1;                                         00015800
                   CLOSE(CARD,RELEASE);                                 00015900
                   NVAR1 ~ NVAR-1;                                      00016000
COMMENT            FIND BEST WAY TO PARTITION MATRIX CONSIDERING THE    00016100
                   AVAILABLE MOMORY AND NUMBER OF VARIABLES (NVAR) ;    00016200
                   NPART ~ 1;     COMMENT  PARTITION FACOTR ;           00016300
                   MEM1~MEM-2500; COMMENT  DECREASE MEMORY FOR SAFETY   00016400
                                           FACOTR ;                     00016500
COMMENT            DETERMINE MEMORY REQUIRED TO RUN THIS JOB;           00016600
       CHECK:      REQMEM ~ 3|NVAR*2/NPART;                             00016700
COMMENT            TRANSFER TO OK WHEN RIGHT"FIT" IS FOUND ;            00016800
                   IF REQMEM < MEM1 THEN GO TO OK;                      00016900
                   NPART ~ NPART+1; GO TO CHECK;                        00017000
COMMENT            NOW THAT PARTITIONING FACTOR HAS BEEN FOUND IT IS    00017100
                   NECESSARY TO DETERMINE UPPER AND LOWER BOUNDS SO THAT00017200
                   ALL COMBINATIONS CAN BE FIGURED, ALL COMBINATIONS    00017300
                   WILL BE STORED IN ARRAY PARTIT                       00017400
   ***********     NESTED BLOCK NUMBER 1     ***********             ;  00017500
     OK:                                                                00017600
              BEGIN                                                     00017700
BOOLEAN            DIR;                                                 00017800
INTEGER ARRAY      PARTIT[0:NPART|2];                                   00017900
FILE               SUMS (2,NVAR,SAVE 10);                               00018000
FILE               ARR (2,81,7,SAVE 10);                                00018100
FILE               ARR1(2,81,7,SAVE 10);                                00018200
SWITCH FILE        SC ~ ARR,ARR1;                                       00018300
INTEGER            CT,M,M1,BOTTOM,TOP,T,T1;                             00018400
FILE               RAWSC "MASTER" (1,NVAR);                             00018500
INTEGER            M2,M3;                                               00018600
LABEL               RSTART0,RSTART1,RSTART2,RSTART3,RSTART4;            00018700
SWITCH  WHICH      ~RSTART0,RSTART1,RSTART2,RSTART3,RSTART4;            00018800
ARRAY              SCORE[0:NVAR1] ;                                     00018900
ARRAY A1[0:6];                                                          00019000
BOOLEAN            ONCE;                                                00019100
LABEL              PRT,EOF;                                             00019200
FORMAT             FM(I4,I5,X5,E13.6,X7,E13.6,X6,E13.6,X6,E13.6,X7,     00019300
                      E13.6,X7,I4);                                     00019400
FORMAT             HED(X3,"IDENT",X10,"SUM X",X14,"SUM X SQ",X12,       00019500
                       "SUM Y",X13,"SUM Y SQ",X13,"SUM XY",X12,"N"//);  00019600
ALPHA              NULL;                                                00019700
PROCEDURE          RESTART1;                                            00019800
              BEGIN                                                     00019900
                   READ(CARD,FER,NP);                                   00020000
                   CLOSE(CARD ,RELEASE);                                00020100
                   FOR M ~ 1 STEP 1 UNTIL NP DO                         00020200
              BEGIN                                                     00020300
                   LLIM ~ PARTIT[M |2-1];                               00020400
                   ULIM ~ PARTIT[M |2];                                 00020500
                   FOR I~1 STEP 1 UNTIL 3 DO                            00020600
                   FOR K~LLIM STEP 1 UNTIL ULIM DO                      00020700
                   READ(SUMS);                                          00020800
              END;                                                      00020900
                   NP~NP+1; COMMENT DETERMINE PASS AND POSITION SUMS;   00021000
              END  RESTART1;                                            00021100
PROCEDURE          DIAG(A);                                             00021200
ARRAY              A[0];                                                00021300
              BEGIN                                                     00021400
COMMENT  THIS      PROCEDURE COMPUTES Y ,Y*2,N, ETC. FOR 0,0,  E,1   2,200021500
                   .... NVAR1,NVAR1  -  RESULTS  ARE RETAINED IN        00021600
                   MEMORY AND FINALLY WRITTEN ON THE BACK END OF SC[R1];00021700
                   A[0]~1; COMMENT FOR COMPATIRILITY WHEN WRITTEN       00021800
                           ON TAPE - INDICATES RECORD IS COMPLETE ;     00021900
                   A[6]~A[1]~A[1]+SCORE[M]; COMMENT  SUM Y & X  ;       00022000
                   A[4]~A[3]~A[2]~SCORE[M]*2+A[2]; COMMENT YSQ,XSQ,& XY;00022100
                   A[5]~1+A[5];  COMMENT N;                             00022200
              END  DIAG;                                                00022300
ARRAY              DIAGN[0:NVAR1,0:6];                                  00022400
PROCEDURE          ROW (D,D1,D2);                                       00022500
                   ARRAY D,D1,D2[0] ;  COMMENT WAS *;                   00022600
              BEGIN                                                     00022700
                   T2 ~ SCORE[I];                                       00022800
                   FOR  J ~ I+1 STEP 1 UNTIL NVAR1 DO                   00022900
                   IF SCORE[J] ! NULL THEN                              00023000
              BEGIN                                                     00023100
                   T1 ~ SCORE[J] ;                                      00023200
                   D[J]  ~ T1 * 2 + D[J] ;      COMMENT SUM Y SQ ;      00023300
                   D1[J] ~ D1[J] + T1 ;         COMMENT SUM Y ;         00023400
                   D2[J] ~ T2 | T1  + D2[J] ;   COMMENT SUM XY ;        00023500
              END;                                                      00023600
                   FOR J ~ I-1 STEP -1 UNTIL 0  DO                      00023700
                   IF SCORE[J] ! NULL THEN                              00023800
              BEGIN                                                     00023900
                   T1 ~ SCORE[J] ;                                      00024000
                   D[J]  ~ D[J]+1;              COMMENT  N  ;           00024100
                   D1[J] ~ T1*2 + D1[J] ;        COMMENT  SUM X SQ;     00024200
                   D2[J] ~ D2[J] + T1 ;         COMMENT  SUM X   ;      00024300
              END                                                       00024400
              END  ROW ;                                                00024500
PROCEDURE          CORR ;                                               00024600
              BEGIN                                                     00024700
LABEL              L;                                                   00024800
                   LLIM ~ PARTIT[NP|2-1];                               00024900
                   ULIM ~ PARTIT[NP|2];                                 00025000
       L:     BEGIN                                                     00025100
COMMENT SAVE REMOVED HERE BY WHE 1-25-67    ;                           00025200
ARRAY              XSQY,NYSQ,XXY[LLIM:ULIM,0:NVAR1];                    00025300
LABEL              L1;                                                  00025400
                   FOR K~ 1 STEP 1 UNTIL NCASE DO                       00025500
              BEGIN                                                     00025600
                   IF DIR THEN READ(RAWSC,NVAR,SCORE[*]) ELSE           00025700
                   READ REVERSE (RAWSC,NVAR,SCORE[*]) ;                 00025800
COMMENT            FORM XSQ,X,ETC FOR ELEMENTS 0,0  1,1  2,2  ETC,"ONCE"00025900
                   IS TRUE ON 1ST TAPE PASS THEN BECOMES FALSE SO THAT, 00026000
                   IF NPART>1 THEN ELEMENTS FORMED ONLY ONCE;           00026100
                   IF ONCE THEN FOR M~0 STEP 1 UNTIL NVAR1 DO           00026200
                   IF SCORE[M]! NULL THEN DIAG(DIAGN[M,*]);             00026300
                   FOR I~ LLIM STEP 1 UNTIL ULIM DO                     00026400
                   IF SCORE[I] ! NULL THEN                              00026500
                   ROW(NYSQ[I,*],XSQY[I,*],XXY[I,*] );                  00026600
              END;                                                      00026700
                   FOR  K ~ LLIM STEP 1 UNTIL ULIM DO                   00026800
                   WRITE(SUMS,NVAR,XSQY[K,*])  ;                        00026900
                   FOR  K ~ LLIM STEP 1 UNTIL ULIM DO                   00027000
                   WRITE(SUMS,NVAR,NYSQ[K,*]);                          00027100
                   FOR  K ~ LLIM STEP 1 UNTIL ULIM DO                   00027200
                   WRITE(SUMS,NVAR,XXY[K,*]);                           00027300
       L1:                                                              00027400
                   IF NP !NPART THEN                                    00027500
              BEGIN                                                     00027600
                   WRITE(PUNCH,FER,NP);                                 00027700
                   CLOSE(PUNCH,RELEASE);                                00027800
                   NP~NP+1;                                             00027900
                   ONCE~FALSE;                                          00028000
                   DIR ~ NOT DIR ;                                      00028100
                   LLIM ~ PARTIT[NP|2-1] ;                              00028200
                   ULIM ~ PARTIT[NP|2] ;                                00028300
                   GO TO L ;                                            00028400
              END;                                                      00028500
              END  INNER BLOCK ;                                        00028600
               END  ;    COMMENT END PROCEDURE CORR;                    00028700
PROCEDURE ONECOMB;                                                      00028800
          BEGIN                                                         00028900
ARRAY         NN,XSQ,X,YSQ,Y,XY[0:NVAR1 ];                              00029000
REAL              XT,T1,T2;                                             00029100
INTEGER        N;                                                       00029200
REAL               DUMMY,SUMY,SUMYSQD,SUMXY,SUMXSQD,SUMX,               00029300
                   CORR,MEANX,MEANY,SIGMAX,SIGMAY;                      00029400
LIST               LST2 (I,J,CORR,N,MEANX,MEANY,SIGMAX,SIGMAY);         00029500
LIST               LST3 (J,I,CORR,N,MEANY,MEANX,SIGMAY,SIGMAX);         00029600
FORMAT OUT         FMT1 (X2,"IDENT",X8,"CORR",X9,"N",X11,"MEAN (X)",    00029700
                         X12,"MEAN (Y)",X11,"SIGMA (X)",X11,            00029800
                         "SIGMA (Y)"//);                                00029900
FORMAT OUT         FMT2 (I4,X1 ,I4,X4,F7.4,X6,I4,X7,F13.4,X7,F13.4,     00030000
                         X7,F13.4,X7,F13.4  );                          00030100
LABEL              L;                                                   00030200
BOOLEAN        B;                                                       00030300
COMMENT  THIS PROCEDURE COMPUTES AND PRINTS ALL COMBINATIONS OF I,J FOR 00030400
       EITHER I HELD CONSTANT AND J=I,...NVAR OR FOR J HELD CONSTANT    00030500
       AND I=1,2,....J.   OPTION COMES FROM THE INPUT CARD. "MASTER" IS 00030600
       READ AND ALL COMBINATIONS ARE FORMED AND PRINTED (ALL            00030700
       COMBINATIONS ARE RETAINED IN MEMORY;                             00030800
               ELEMN~ELEMN-1;                                           00030900
               IF ELEMN}0 THEN                                          00031000
          BEGIN                                                         00031100
               LLIM~ELEMN; ULIM~NVAR1; B~FALSE;                         00031200
               ELEMNO~ELEMN;                                            00031300
          END  ELSE                                                     00031400
          BEGIN                                                         00031500
               LLIM~0; ULIM~ELEMNO;  B~TRUE;                            00031600
          END;                                                          00031700
               I~ELEMNO;                                                00031800
               TOP~0;                                                   00031900
               FOR K~ 1 STEP 1 UNTIL NCASE DO                           00032000
          BEGIN                                                         00032100
               READ(RAWSC,NVAR,SCORE[*]);                               00032200
               IF SCORE[I] ! NULL THEN                                  00032300
          BEGIN                                                         00032400
               T2~SCORE[I];                                             00032500
               FOR J~LLIM STEP 1 UNTIL ULIM DO                          00032600
               IF SCORE[J] ! NULL THEN                                  00032700
          BEGIN                                                         00032800
               T1~SCORE[J];                                             00032900
              NN[J]~NN[J]+1;  XSQ[J]~T1*2+XSQ[J];  X[J]~T1+X[J];        00033000
               YSQ[J]~T2*2+YSQ[J];  Y[J]~Y[J]+T2;  XY[J]~XY[J]+T1|T2;   00033100
          END;                                                          00033200
          END;                                                          00033300
          END;                                                          00033400
               CLOSE(RAWSC,SAVE);                                       00033500
               J~ELEMNO+1;  WRITE(LINE,HED);                            00033600
               FOR I~LLIM STEP 1 UNTIL ULIM DO                          00033700
          BEGIN                                                         00033800
               IF B THEN                                                00033900
               WRITE(LINE,FM,I+1,J,X[I],XSQ[I],Y[I],YSQ[I],XY[I],NN[I]) 00034000
                ELSE                                                    00034100
               WRITE(LINE,FM,J,I+1,Y[I],YSQ[I],X[I],XSQ[I],XY[I],NN[I]);00034200
               TOP~TOP+1;                                               00034300
               IF TOP>54 THEN                                           00034400
          BEGIN                                                         00034500
               WRITE(LINE[PAGE]);                                       00034600
               WRITE(LINE,FTITLE,TITLE1,TITLE2,TITLE3);                 00034700
               WRITE(LINE,HED);                                         00034800
               TOP~0;                                                   00034900
          END;                                                          00035000
          END;                                                          00035100
               J~ELEMNO+1;                                              00035200
               WRITE(LINE[PAGE]);   WRITE(LINE,FMT1);                   00035300
               TOP~0;                                                   00035400
               FOR M~LLIM STEP 1 UNTIL ULIM DO                          00035500
          BEGIN                                                         00035600
               N~NN[M];   SUMY~Y[M];  SUMYSQD~YSQ[M];  SUMXY~XY[M];     00035700
               SUMXSQD~XSQ[M];  SUMX~X[M];                              00035800
               IF N = 0 THEN                                            00035900
         BEGIN                                                          00036000
               MEANX~MEANY~SIGMAX~SIGMAY~CORR~0;  GO TO L;              00036100
         END;                                                           00036200
                   MEANY ~ SUMY / N; MEANX ~ SUMX / N;                  00036300
                   SIGMAX ~ SQRT ((SUMXSQD / N) - (MEANX * 2));         00036400
                   SIGMAY ~ SQRT ((SUMYSQD / N) - (MEANY * 2));         00036500
                  XT~SUMXY-(SUMX | SUMY)/N;                             00036600
                   T1~SUMXSQD - (SUMX*2)/N;                             00036700
                   T2~SUMYSQD-(SUMY*2)/N;                               00036800
                   IF T1=0 OR T2=0 THEN CORR~0 ELSE                     00036900
                   CORR~XT/(SQRT(T1) | SQRT(T2));                       00037000
               I~M+1;                                                   00037100
     L:                                                                 00037200
               IF B THEN                                                00037300
               WRITE(LINE,FMT2,LST2)                                    00037400
                ELSE                                                    00037500
               WRITE(LINE,FMT2,LST3);                                   00037600
               TOP~TOP+1;                                               00037700
               IF TOP>54 THEN                                           00037800
          BEGIN                                                         00037900
          WRITE(LINE[PAGE]);  WRITE(LINE,FMT1);                         00038000
               TOP~0                                                    00038100
          END;                                                          00038200
          END;                                                          00038300
               GO TO STP;                                               00038400
          END  ONECOMB;                                                 00038500
COMMENT            STO COL. NOS. FOR PARTITIONED MATRIX IN PARTIT ;     00038600
                   NULL ~ "NULL";                                       00038700
               IF ELEMN}1 OR ELEMNO>0 THEN ONECOMB;                     00038800
                   ONCE~TRUE;                                           00038900
                   TEMP ~ NPART | 2;                                    00039000
                   PARTIT[1] ~ 0;                                       00039100
                   PARTIT[2] ~ (NVAR DIV NPART) -1 ;                    00039200
                   REM ~ NVAR MOD NPART ;                               00039300
                   FOR I ~ 3 STEP 2 UNTIL TEMP DO                       00039400
              BEGIN                                                     00039500
                   PARTIT[I] ~ PARTIT[I-1] + 1;                         00039600
                   PARTIT[I+1] ~ PARTIT[2] + PARTIT[I] ;                00039700
              END;                                                      00039800
                   IF REM ! 0 THEN PARTIT[I-1] ~ PARTIT[I-1] + REM;     00039900
                   DIR ~ TRUE;                                          00040000
                   NP ~ 1 ;                                             00040100
                   IF RSTT = 2 THEN                                     00040200
              BEGIN                                                     00040300
                   FOR K~1 STEP 1 UNTIL NCASE DO                        00040400
              BEGIN                                                     00040500
                   READ(RAWSC,NVAR,SCORE[*]);                           00040600
                   FOR M~0 STEP 1 UNTIL NVAR1 DO IF SCORE[M]!NULL       00040700
                   THEN DIAG(DIAGN[M,*]);                               00040800
              END;                                                      00040900
               CLOSE(RAWSC,SAVE);                                       00041000
              END;                                                      00041100
                   GO TO WHICH[RSTT+1];                                 00041200
COMMENT       """""""""""""  START OF PHASE 1  """""""""""""            00041300
                   THIS PHASE ACTUALLY BEGINS WHEN FILE SUMS IS OPENED  00041400
                   AND WRITTEN ON       ;                               00041500
    RSTART1:       RESTART1;                                            00041600
COMMENT       """""""""""""  START OF PHASE 0  """""""""""""           ;00041700
    RSTART0:                                                            00041800
                   CORR;                                                00041900
               CLOSE(RAWSC,SAVE);                                       00042000
               CLOSE(SUMS,SAVE);                                        00042100
COMMENT            REMOVE WRITE RING AS A SAFETY PRECAUTION  AND READ   00042200
               TAPE JUST LOCKED;                                        00042300
COMMENT                                                                 00042400
  **********************   INNER BLOCK   ***************************** ;00042500
COMMENT       """""""""""""  START OF PHASE 2  """""""""""""           ;00042600
    RSTART2:                                                            00042700
                                T~0 ; T1~1;                             00042800
                   FOR CT ~ 1 STEP 1 UNTIL NPART DO                     00042900
              BEGIN                                                     00043000
LABEL              RD,REST,INIT;                                        00043100
COMMENT SAVE REMOVED HERE BY WHE 1-25-67;                               00043200
ARRAY              D,D1,D2[PARTIT[CT|2-1]:PARTIT[CT|2],0:NVAR1],        00043300
                   A[0:6];                                              00043400
                   LLIM ~ PARTIT[CT|2-1];                               00043500
                   TOP ~ ULIM ~ PARTIT[CT|2] ;                          00043600
                   M1 ~ ULIM;   M ~ LLIM;                               00043700
                   FOR I~M STEP 1 UNTIL M1 DO READ(SUMS,NVAR,D[I,*]);   00043800
                   FOR I~M STEP 1 UNTIL M1 DO READ(SUMS,NVAR,D1[I,*]);  00043900
                   FOR I~M STEP 1 UNTIL M1 DO READ(SUMS,NVAR,D2[I,*]);  00044000
               IF CT=1 THEN GO TO INIT;                                 00044100
                   REWIND(SC[T]);     COMMENT REW HERE SO THAT FINAL    00044200
                                   TAPE IS NOT RESOUND ;                00044300
                   M2~0;  M3~PARTIT[CT|2-1]-1;                          00044400
COMMENT            A[0] IS TRUE IF RECORD IS COMPLETE;                  00044500
                   FOR I ~ M2 STEP 1 UNTIL M3 DO                        00044600
              BEGIN                                                     00044700
     RD:           READ(SC[T][NO],7,A[*]);                              00044800
                   IF BOOLEAN (A[0]) THEN                               00044900
              BEGIN                                                     00045000
                   WRITE(SC[T1],7,A[*]) ;  READ(SC[T]);                 00045100
                   GO TO RD ;                                           00045200
              END;                                                      00045300
                   FOR J ~ M STEP 1 UNTIL M1 DO                         00045400
               BEGIN                                                    00045500
                   READ(SC[T],7,A[*]);                                  00045600
                   A[4] ~ D[J,I]; A[5] ~ D1[J,I];  A[6]~D2[J,I];        00045700
                   A[0] ~ 1;                                            00045800
                   WRITE(SC[T1],7,A[*]);                                00045900
              END;                                                      00046000
     REST:         FOR J ~ TOP +1 STEP 1 UNTIL NVAR1  DO                00046100
              BEGIN                                                     00046200
                   READ(SC[T],7,A[*]);                                  00046300
                   WRITE(SC[T1],7,A[*]);                                00046400
              END;                                                      00046500
              END;                                                      00046600
COMMENT        FORM COMBINATIONS FROM TAPE SUMS;                        00046700
     INIT:         FOR I ~ M    STEP 1 UNTIL M1   DO                    00046800
              BEGIN                                                     00046900
                   FOR J ~ I+1 STEP 1 UNTIL M1   DO                     00047000
              BEGIN                                                     00047100
                   A[0]~1; A[1]~D [I,J];A[2]~D1[I,J]; A[3]~D2[I,J];     00047200
                   A[4]~D[J,I]; A[5]~ D1[J,I]; A[6]~D2[J,I];            00047300
                   WRITE (SC[T1],7,A[*]);                               00047400
              END;                                                      00047500
                   A[4]~A[5]~A[6]~A[0]~ 0;                              00047600
                   FOR J ~ ULIM+1 STEP 1 UNTIL NVAR1  DO                00047700
              BEGIN                                                     00047800
                   A[1]~D[I,J]; A[2]~D1[I,J]; A[3]~D2[I,J];             00047900
                   WRITE(SC[T1],7,A[*]);                                00048000
              END;                                                      00048100
              END;                                                      00048200
                   REWIND(SC[T]);                                       00048300
                   DOUBLE(T,T1,~,T1,T);                                 00048400
              END;                                                      00048500
COMMENT            END OF INNER BLOCK;                                  00048600
COMMENT            FINAL TAPE IS SC[T];                                 00048700
COMMENT            WRITE DIAG ELEMENTS ON BACK END OF  SC[T] TAPE ;     00048800
                   FOR I ~ 0 STEP 1 UNTIL NVAR1 DO                      00048900
                   WRITE(SC[T],7,DIAGN[I,*]);                           00049000
                  CLOSE(SC[T],SAVE);                                    00049100
               CLOSE(SUMS,SAVE);                                        00049200
COMMENT            REMOVE WRITE RING AS A SAFETY PRECAUTION  AND READ   00049300
               TAPE JUST LOCKED;                                        00049400
                   CLOSE(SC[T1],RELEASE);                               00049500
                   IF PRINTORNOT ! "    " THEN GO TO RSTART4;           00049600
COMMENT       """""""""""""  START OF PHASE 3  """""""""""""           ;00049700
    RSTART3:                                                            00049800
COMMENT            DETERMINE WHICH TAPE WAS FINAL OUTPUT TAPE AND PRINT 00049900
                   IT - THIS IS REDUNDANT BUT IS NEEDED IN CASE OF A    00050000
                   RESTART FOR PHASE 3 OR 4 ;                           00050100
                   IF BOOLEAN(NPART) THEN T~1 ELSE T~0;                 00050200
                   WRITE(LINE[PAGE]);                                   00050300
                   DATELINE("MISDAT");                                  00050400
                   WRITE(LINE,HED);  TOP~ 0;                            00050500
                   FOR M ~ 1 STEP 1 UNTIL NVAR1 DO                      00050600
               BEGIN                                                    00050700
                   FOR M1~ M+1 STEP 1 UNTIL NVAR DO                     00050800
              BEGIN                                                     00050900
                   READ(SC[T],7,A1[*]);                                 00051000
                   WRITE(LINE,FM,M,M1,A1[6],A1[4],A1[1],A1[2],A1[3],    00051100
                          A1[5]);                                       00051200
                   TOP ~ TOP+1;  IF TOP>46 THEN                         00051300
              BEGIN                                                     00051400
                   TOP~0; WRITE(LINE[PAGE]);                            00051500
                   DATELINE("MISDAT");                                  00051600
               WRITE(LINE,HED);                                         00051700
              END;                                                      00051800
               END;                                                     00051900
                   WRITE(LINE);  TOP ~ TOP+1;                           00052000
              END;                                                      00052100
               IF TOP>46 THEN BEGIN TOP~0; WRITE(LINE[PAGE]);  %VR 1/69 00052110
               WRITE(LINE,HED); END;                           %VR 1/69 00052120
                   TOP ~ TOP+2;                                         00052200
               IF TOP>46 THEN BEGIN TOP~0; WRITE(LINE[PAGE]);  %VR 1/69 00052210
               WRITE(LINE,HED); END;                           %VR 1/69 00052220
                   WRITE(LINE[DBL]); COMMENT GET DIAG ELEMENTS;         00052300
                   FOR M~1 STEP 1 UNTIL NVAR DO                         00052400
              BEGIN                                                     00052500
                   READ(SC[T],7,A1[*]);                                 00052600
                   WRITE(LINE,FM,M,M ,A1[6],A1[4],A1[1],A1[2],A1[3],    00052700
                          A1[5]);                                       00052800
                   TOP~TOP+1; IF TOP > 46 THEN                          00052900
              BEGIN                                                     00053000
                   TOP ~ 0;  WRITE(LINE[PAGE]);                         00053100
                   DATELINE("MISDAT");                                  00053200
                   WRITE(LINE,HED);                                     00053300
              END;                                                      00053400
              END;                                                      00053500
                   REWIND(SC[T]);                                       00053600
COMMENT            SUM Y, SUM Y SQ, SUM XY, ETC JUST PRINTED, REWIND    00053700
                   TPAE AND READ,COMPUTE, AND PRINT MEAN, STANDARD      00053800
                   DEVIATION, ETC.  ;                                   00053900
COMMENT       """""""""""""  START OF PHASE 4  """""""""""""           ;00054000
    RSTART4:                                                            00054100
COMMENT            DETERMINE WHICH TAPE WAS FINAL OUTPUT TAPE AND PRINT 00054200
                   IT - THIS IS REDUNDANT BUT IS NEEDED IN CASE OF A    00054300
                   RESTART FOR PHASE 3 OR 4 ;                           00054400
                   IF BOOLEAN(NPART) THEN T~1 ELSE T~0;                 00054500
              BEGIN                                                     00054600
COMMENT            R M WATTERS, SUBPROGRAM TO COMPUTE AND PRINT THE     00054700
                   MEAN OF X ANY Y, STANDARD DEVIATION OF X AND Y, AND  00054800
                   THE COEFFICIENT OF CORRELATION;                      00054900
FORMAT OUT         FMT1 (X2,"IDENT",X8,"CORR",X9,"N",X11,"MEAN (X)",    00055000
                         X12,"MEAN (Y)",X11,"SIGMA (X)",X11,            00055100
                         "SIGMA (Y)"//);                                00055200
FORMAT OUT         FMT2 (I4,X1 ,I4,X4,F7.4,X6,I4,X7,F13.4,X7,F13.4,     00055300
                         X7,F13.4,X7,F13.4  );                          00055400
INTEGER            I,J,N,PAGENO,LINENO;                                 00055500
REAL ARRAY     CM[0:NVAR];                                              00055600
REAL              XT,T1,T2;                                             00055700
REAL               DUMMY,SUMY,SUMYSQD,SUMXY,SUMXSQD,SUMX,               00055800
                   CORR,MEANX,MEANY,SIGMAX,SIGMAY;                      00055900
LIST               LST1 (DUMMY,SUMY,SUMYSQD,SUMXY,SUMXSQD,N,SUMX),      00056000
                   LST2 (I,J,CORR,N,MEANX,MEANY,SIGMAX,SIGMAY);         00056100
LABEL              EOF;                                                 00056200
PROCEDURE          SOLVER;                                              00056300
              BEGIN                                                     00056400
LABEL              L;                                                   00056500
               IF N = 0 THEN                                            00056600
         BEGIN                                                          00056700
               MEANX~MEANY~SIGMAX~SIGMAY~CORR~0;  GO TO L;              00056800
         END;                                                           00056900
                   MEANY ~ SUMY / N; MEANX ~ SUMX / N;                  00057000
                   SIGMAX ~ SQRT ((SUMXSQD / N) - (MEANX * 2));         00057100
                   SIGMAY ~ SQRT ((SUMYSQD / N) - (MEANY * 2));         00057200
                  XT~SUMXY-(SUMX | SUMY)/N;                             00057300
                   T1~SUMXSQD - (SUMX*2)/N;                             00057400
                   T2~SUMYSQD-(SUMY*2)/N;                               00057500
                   IF T1=0 OR T2=0 THEN CORR~0 ELSE                     00057600
                   CORR~XT/(SQRT(T1) | SQRT(T2));                       00057700
     L:                                                                 00057800
              END  OF PROCEDURE SOLVER;                                 00057900
PROCEDURE          NEWPAGE;                                             00058000
              BEGIN                                                     00058100
                   WRITE (LINE[PAGE]);                                  00058200
                   DATELINE("MISDAT");                                  00058300
                   LINENO ~ 0;                                          00058400
               WRITE(LINE,FTITLE,TITLE1,TITLE2,TITLE3);                 00058500
                   WRITE (LINE,FMT1);                                   00058600
              END  OF PROCEDURE NEWPAGE;                                00058700
PROCEDURE          PROCESSOR;                                           00058800
              BEGIN                                                     00058900
                   READ (SC[T],*,LST1)[EOF];                            00059000
                   SOLVER;                                              00059100
                   WRITE (LINE,FMT2,LST2);                              00059200
                   LINENO ~ LINENO + 1;                                 00059300
                   IF LINENO } 47 THEN NEWPAGE;                         00059400
              END  OF PROCEDURE PROCESSOR;                              00059500
COMMENT            *****START OF PROGRAM*****;                          00059600
                   NEWPAGE;                                             00059700
               IF CORROUT THEN                                          00059800
          BEGIN                                                         00059900
               FOR I~1 STEP 1 UNTIL NVAR1 DO                            00060000
          BEGIN                                                         00060100
               WRITE(LINE);                                             00060200
               LINENO~LINENO+1;                                         00060300
               IF LINENO}47 THEN NEWPAGE;                      %VR 1/69 00060310
               CM[I]~1.0;                                               00060400
               FOR J~I+1 STEP 1 UNTIL NVAR DO                           00060500
          BEGIN                                                         00060600
               PROCESSOR;                                               00060700
               CM[J]~CORR                                               00060800
          END;                                                          00060900
               WRITE(TEM   ,*,  FOR J~1 STEP 1 UNTIL NVAR DO CM[J]);    00061000
          END  OF ILOOP FOR CORROUT                                     00061100
          END                                                           00061200
               ELSE                                                     00061300
                   FOR I ~ 1 STEP 1 UNTIL NVAR1 DO                      00061400
              BEGIN                                                     00061500
                   WRITE (LINE);                                        00061600
                   LINENO ~ LINENO + 1;                                 00061700
               IF LINENO}47 THEN NEWPAGE;                               00061800
                   FOR J ~ I + 1 STEP 1 UNTIL NVAR DO                   00061900
                   PROCESSOR;                                           00062000
              END  OF ILOOP;                                            00062100
               REWIND(TEM   );                                          00062200
COMMENT            START OF IDENTITY MATRIX;                            00062300
                   WRITE(LINE[DBL]); LINENO~LINENO+2;                   00062400
                   FOR I ~ 1 STEP 1 UNTIL NVAR DO                       00062500
              BEGIN                                                     00062600
                   J ~ I; PROCESSOR;                                    00062700
              END  OF ILOOP;                                            00062800
EOF:          END  OF BLOCK WATTERS;                                    00062900
              END  NESTED BLOCK NUMBER 1;                               00063000
               IF NOT CORROUT THEN GO TO STP ;                          00063100
COMMENT ******** NEW BLOCK TO WRITE FULL CORR MATRIX ON TAPE;           00063200
          BEGIN                                                         00063300
REAL ARRAY     CMT[0:NVAR,0:NVAR];                                      00063400
FORMAT         FCORMAT(100O) ;                                          00063500
FILE           CORMAT 2(2,  100,SAVE 10);                               00063600
               FOR I~1 STEP 1 UNTIL NVAR1 DO                            00063700
BEGIN                                                                   00063800
               READ(TEM   ,*,  FOR J~1 STEP 1 UNTIL NVAR DO CMT[I,J]);  00063900
END;                                                                    00064000
               CLOSE(TEM ,RELEASE);                                     00064100
               FOR I~1 STEP 1 UNTIL NVAR1 DO                            00064200
               FOR J~I+1 STEP 1 UNTIL NVAR DO                           00064300
               CMT[J,I]~CMT[I,J];                                       00064400
               CMT[NVAR,NVAR]~1.0;                                      00064500
               WRITE(CORMAT,FCORMAT,NVAR) ;                             00064600
               FOR I~1 STEP 1 UNTIL NVAR DO                             00064700
               WRITE(CORMAT,FCORMAT,FOR J~1 STEP 1 UNTIL NVAR DO        00064800
                     CMT[I,J]);                                         00064900
          END;                                                          00065000
    STP:                                                                00065100
               DATELINE(0);                                             00065200
              END  PROGRAM.                                             00065300
                LAST CARD ON CRDIMG TAPE                                99999999
