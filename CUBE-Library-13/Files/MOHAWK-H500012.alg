         BEGIN                                                          00001000
INTEGER COMMONER;  %NK,5-11-69                                          00002000
COMMENT  COMMONER FORMAT = PPN                                          00003000
PAGES = PP , MODESWITCH = N ;                                           00004000
COMMENT DICTIONARY ENTRY BEGINS HERE                                    00005000
PROGRAM-ID. 9UTL38R/SIMULA.                                   0009UTL38R00006000
SYNOPSIS : PROGRAM ACCEPTS A STANDARD BURROUGHS TAPE INPUT    0109UTL38R00007000
  AND SIMULATES A MOHAWK 1320 ON THE B5500 ON-LINE PRINTER,   0209UTL38R00008000
  INTERPRETING LAO,ECC,SFC, PRINT COMMAND, AND ADVANCE-TO-    0309UTL38R00009000
  HOME-ROW CHARACTERS.                                        0409UTL38R00010000
AUTHORS : H. WILES AND N.R. KARL, SOFTWARE SERVICES.          1009UTL38R00011000
DATE : OCTOBER 9, 1968.                                       2009UTL38R00012000
REVISION : MAY 16, 1969, NRK.                                 2509UTL38R00013000
INPUT :                                                       3009UTL38R00014000
(1) COMMON CONTROL CARD                                       3059UTL38R00015000
FORMAT : COMMON = PPN                                         3109UTL38R00016000
PP = LINES/PAGE, IF PP = 0 THEN PP := 66 BY DEFAULT           3209UTL38R00017000
N = MODESWITCH                                                3309UTL38R00018000
N = 0, IGNORE FORM CHECK                                      3329UTL38R00019000
N = 1, FORM CHECK MESSAGE WITH NO HALT TO EOJ                 3349UTL38R00020000
N = 2 EXACT SIMULATION, FORM CHECK HALT                       3369UTL38R00021000
(2) BURROUGHS STANDARD LABELED TAPE,EVEN PARITY,BLOCKED 1,    3409UTL38R00022000
  80 CHARACTERS PER RECORD.                                   3509UTL38R00023000
INPUT FILE IDENTIFIER : MOHAWK.                               3529UTL38R00024000
  (3) CARD DECK FORMAT :                                      3559UTL38R00025000
                                                              3609UTL38R00026000
CC EXECUTE 9UTL38R/SIMULA                                     3659UTL38R00027000
CC END.   END-OF-FILE                                         3709UTL38R00028000
                                                              3759UTL38R00029000
                                                              3809UTL38R00030000
                                                              3859UTL38R00031000
CC EXECUTE 9UTL38R/SIMULA                                     3909UTL38R00032000
CC COMMON = 2 . EXACT SIMULATION                              3929UTL38R00033000
CC FILE MOHAWK = BACKLOG                                      3949UTL38R00034000
CC END.  END-OF-FILE                                          3969UTL38R00035000
                                                              3979UTL38R00036000
NOTE : CC = QUESTION MARK IN BCL.                             3989UTL38R00037000
OUTPUT :                                                      3999UTL38R00037250
(1) PRINTER FILE IDENTIFIER = LINE.                           3999UTL38R00037500
                                                              3999UTL38R00038000
PROCESS :                                                     4009UTL38R00039000
(1) IT IS ASSUMED THAT THE 1320 OPERATOR INITIALIZES THE      4059UTL38R00040000
 PRINTER TO TOP OF PAGE.                                      4109UTL38R00041000
(2) AN 80 CHARACTER RECORD BEGINS READING IN.                 4159UTL38R00042000
(3) THE NEXT CHARACTER IS INTERPRETED AS A LINE-ADVANCE-      4209UTL38R00043000
 ORDER OR ADVANCE-TO-HOME-ROW.                                4259UTL38R00044000
(4) IF THE NEXT CHARACTER IS AN EDIT-CONTROL-CODE, GO TO (6)  4309UTL38R00045000
 ELSE GO TO (5).                                              4359UTL38R00046000
(5) MOVE CHARACTER TO PRINT LINE, IF THERE ARE 132 CHAR-      4409UTL38R00047000
ACTERS IN THE PRINT BUFFER, PRINT THE LINE AND GO TO (3) ELSE 4459UTL38R00048000
 GO TO (4).                                                   4609UTL38R00049000
(6) INTERPRET THE NEXT CHARACTER AS BA8421.                   4659UTL38R00050000
(61) IF B = 1 AND A8421 = 0000 THEN SPACE FILL THE PRINT      4709UTL38R00051000
 BUFFER, PRINT THE LINE, GO TO (3).                           4759UTL38R00052000
(62) IF B = 1 AND A8421 ! 00000 THEN SPACE FILL PRINT BUFFER, 4809UTL38R00053000
 PRINT THE LUNE, EMPTY RECORD BUFFER, GO TO (2).              4859UTL38R00054000
(63) IF B = 0 THEN SPACE FILL RECORD BUFFER DEPENDING ON      4909UTL38R00055000
VALUE OF A8421. LESS THAN OR EQUAL TO 30. GO TO (5).          4959UTL38R00056000
OUTPUT :                                                      5009UTL38R00057000
(1) PRINT LINES.                                              5109UTL38R00058000
(2) THE ERROR MESSAGE FOLLOWING IS TRANSMITTED TO THE LINE    5209UTL38R00059000
 PAINTER AFTER ADVANCING TO TOP OF PAGE :                     5309UTL38R00060000
"A FORM CHECK CONDITION HAS BEEN ENCOUNTERED IN LINE NO. ",NN 5409UTL38R00061000
" INPUT RECORD ", MM                                          5459UTL38R00062000
"RUN WILL NOT BE DISCONTINUED". NN IS LINE COUNT ON PAGE      5509UTL38R00063000
  BEING PRINTED WHEN ERROR CONDITION OCCURRED.                5609UTL38R00064000
MM IS INPUT RECORD NUMBER.                                    5709UTL38R00065000
MODIFICATION-POSSIBILITIES :                                  6009UTL38R00066000
(1) THE ECC CHARACTER CAN BE CHANGED BY MODIFYING A SINGLE    6109UTL38R00067000
 CARD, I.E. #212000.                                          6209UTL38R00068000
(2) THE ADVANCE-TO-HOME-ROW CHARACTER CAN BE CHANGED BY       6309UTL38R00069000
  MODIFYING A SINGLE CARD,I.E. #214000.                       6409UTL38R00070000
(3) IT HAS BEEN NECESSARY TO USE BOTH BURROUGHS INTERNAL AND  6509UTL38R00071000
  EXTERNAL CODE LITERALS AT DIFFERENT POINTS IN THE PROGRAM.  6609UTL38R00072000
        DICTIONARY ENTRY ENDS   HERE ;                                  00073000
COMMENT THIS MOHAWK SIMULATOR = ID OF 9UTL38R/SIMULA  ;                 00074000
COMMENT FLOWCHART BEGINS HERE                                           00075000
LABEL    SEQEND         SEQEND         DESCRIPTION                      00076000
         #186000        #191000        ALPHA PROCEDURE CONVERT2         00077000
         #174000        #184000        PROCEDURE FIXCNT2                00078000
         #192000                       BEGIN OF PROGRAM                 00079000
         #193000        #225000        INITIALIZATION                   00080000
         #196000        #205000        CHARACTER CONVERSION ARRAY       00081000
         #212000        #213000        SELECT EDIT CONTROL CODE         00082000
         #214000        #215000        SELECT ADVANCE TO HOME ROW CHAR  00083000
         #216000        #220000        FIND LINE/PAGE AND SIMULATION    00084000
                                         MODE FROM COMMON( COMMONER)    00085000
         #221000                       SET BUFFER SIZE                  00086000
         #222000                       LINE/PAGE DEFAULT = 66           00087000
         #223000                       INITIALIZE   FIRST RECORD SWITCH 00088000
         #224000                       INITIALIZE END OF SCAN MARK      00089000
         #226000        #333000        MAIN LOOP                        00090000
RDTAPE   #226000                       READ NEXT RECORD FROM TAPE       00091000
LNSKP    #232000        #244000        INTERPRET LINE ADVANCE ORDER     00092000
         #245000       #250000         INTERPRET ADVANCE TO HOME ROW    00093000
         #251000        #281000        GENERATE THE LINE ADVANCE        00094000
NEWPAGE  #282000        #285000        INITIALIZE PRINT LINE INDEX,     00095000
                                         TOP-OF-PAGE FLAG, AND SEE IF   00096000
                                       BUFFER IS EMPTY                  00097000
ECCHEK   #287000        #313000        INTERPRET SPACE FILL COMMAND     00098000
         #314000        #321000        SPACE FILL PRINT BUFFER          00099000
         #322000        #324000        MOVE CHAR TO PRINT  BUFFER SINCE 00100000
                                        IT IS NOT CONTROL               00101000
         #325000        #333000        IF PRINT BUFFER FULL, PRINT LINE 00102000
                                       INITIALIZE COUNTERS, IF ANYTHING 00103000
                                       LEFT IN TAPE BUFFER, GOTO LNSKP  00104000
                                       ELSE GOTO RDTAPE                 00105000
         #332000        #333000        IF YOU HAD AN ECC BEFORE, AND    00106000
                                         MORE IS IN TAPE BUFFER GOTO    00107000
                                       ECCHEK ELSE GOTO RDTAPE.         00108000
        FLOWCHART ENDS HERE;                                            00109000
COMMENT  TERM DEFINITION BEGINS HERE                                    00110000
    ECC =  EDIT CONTROL CODE, ALPHA CHARACTER, FORMAT A1                00111000
    SFC =  SPACE FILL CODE , INTEGER VALUE OF SPACES                    00112000
    LNCNT  =  CURRENT LINE COUNT ON MOHAWK PRINTER                      00113000
    PRNT  =  EXTERNAL B-BIT                                             00114000
     CONVERT2 = PROCEDURE, FROM INTERNAL CODE, OBTAIN EXTERNAL BIT      00115000
         CONFIGURATION,                                                 00116000
     FIXCNT2 = PROCEDURE, OBTAIN BBIT, VALUE OF A8421                   00117000
    E [  ] = ARRAY TO CONVERT INTERNAL CODE TO EQUIVALENT EXTERNAL      00118000
    HRC = EXTERNAL ADVANCE TO HOME ROW BIT CODE                         00119000
    ADV = INTERNAL ADVANCE TO HOME ROW CODE                             00120000
    COMMONER = RECEIPT AREA FOR COMMON VALUE                            00121000
    TIMEIO = IO CHANNEL TIME USED                                       00122000
    TIMEPROC = CPU TIME USED FOR THIS SIMULATION                        00123000
    PAGECNT = COUNT OF 1320 MOHAWK PAGES PRINTED                        00124000
    MODESWITCH = SIMULATION MODE FLAG                                   00125000
    RECCNT = COUNT OF MOHAWK INPUT RECORDS                              00126000
    PAGES  =  NUMBER OF LINES ON MOHAWK 1320                            00127000
    SZ     =  SIZE OF MOHAWK BUFFER -1                                  00128000
    NEXT = FIRST TAPE RECORD SWITCH                                     00129000
    CNT1 =  BINARY 1 LINE SKIP                                          00130000
    CNT2 =  BINARY 2 LINE SKIP                                          00131000
    CNT4 =  BINARY 4 LINE SKIP                                          00132000
    B[  ] = SIMULATED MOHAWK PRINT BUFFER                               00133000
    A[  ] = SIMULATED MOHAWK 1103 BUFFER                                00134000
    I   =  1103 BUFFER POINTER                                          00135000
    J  =  PRINT BUFFER POINTER                                          00136000
    ECCEXT = EXTERNAL BCL AVAILABLE AS INTERNAL, CONVERTED THRU         00137000
       I/O CHANNEL, IN CORE                                             00138000
    EL =  INTERNAL BCL  ECC TO BE CONVERTED TO EXTERNAL ECC CODE        00139000
       IN CORE                                                          00140000
    RD =  READ TAPE SWITCH                                              00141000
         TERM DEFINITION ENDS  HERE  ;                                  00142000
FORMAT OUTHEAD(X10,"MOHAWK SIMULATOR STATISTICS" ),                     00143000
    LINEFORM(I5, "  LINES PER PAGE " ) ,                                00144000
    RECFORM(I10, "  INPUT TAPE RECORDS " ) ,                            00145000
    PAGEFORM(I5, "  OUTPUT MOHAWK 1320 PAGES "),                        00146000
    IOTIME(I10, "  SECONDS OF INPUT/OUTPUT CHANNEL TIME USAGE "),       00147000
    PROCTIME(I10, "  SECONDS OF CPU TIME USAGE ");                      00148000
SWITCH FORMAT MODEFORM ~                                                00149000
              ( "COMMON =",I2, "  NO FORM CHECK MODE "),                00150000
               ("COMMON =",I2, "  FORM CHECK,NO HALT" ),                00151000
            ( "COMMON =",I2, "  EXACT SIMULATION ");                    00152000
INTEGER RECCNT,PAGECNT,TIMEIO,TIMEPROC;                                 00153000
ALPHA FILE IN MOHAWK(2,20);                                             00154000
ALPHA ECCEXT ;                                                          00155000
FILE OUT      LINE 4(2,17);                                             00156000
FORMAT IN     FMA(160A1);                                               00157000
FORMAT OUT    FM1(X132);                                                00158000
FORMAT OUT    FMB(132A1);                                               00159000
ALPHA ADV;         %NK,5-12-69                                          00160000
FORMAT OUT    FMCHEK("A FORM CHECK CONDITION HAS BEEN ENCOUNTER",       00161000
 "ED IN LINE NO. ",I4," INPUT RECORD NO. ", I10/                        00162000
    "R U N  W I L L  N O T  B E  D I S C O N T I N U E D" ) ;           00163000
ALPHA ARRAY E[0:63] ;                                                   00164000
ALPHA ARRAY   A[0:159], B[0:131];                                       00165000
INTEGER       CNT1,CNT2,CNT4,I,J,K,L,RD,PRNT,LNCNT,SFC,PAGES,CDCNT,SZ;  00166000
INTEGER MODESWITCH, KAR; %NK,5-13-69                                    00167000
INTEGER M;                                                              00168000
INTEGER RPT;   %NK, 7-28-69                                             00168500
BOOLEAN       NEXT;                                                     00169000
ALPHA EL;                                                               00170000
ALPHA HRC, HRCHAR ;                                                     00171000
ALPHA         ECC,CHAR;                                                 00172000
LABEL         NEWPAGE,NOCARD,EOJ,LNSKP,RDTAPE,ECCHEK,FORMCHEK,NOTFIRST; 00173000
PROCEDURE FIXCNT2(P,SFC,BBIT) ; %NK,5-12-69                             00174000
COMMENT WARNING: P IS EXTERNAL BIT CODE  REPRESENTATION IN CORE         00175000
OF THE CHARACTER;                                                       00176000
VALUE P;                                                                00177000
ALPHA P;                                                                00178000
INTEGER BBIT,SFC;                                                       00179000
BEGIN  SFC := 0 ;                                                       00180000
SFC.[43:5] := P.[43:5] ;                                                00181000
BBIT := 0 ;                                                             00182000
BBIT.[47:1] := P.[42:1] ;                                               00183000
END FIXCNT2 ;                                                           00184000
                                                                        00185000
ALPHA PROCEDURE CONVERT2(E,D) ; %NK,5-12-69                             00186000
VALUE D;                                                                00187000
ALPHA D;                                                                00188000
ALPHA ARRAY E[0] ;                                                      00189000
CONVERT2~ E[D] ;                                                        00190000
COMMENT END CONVERT2;                                                   00191000
 ;% *****   PROGRAM BEGINS HERE *****                                   00192000
TIMEIO ~ TIME(3) ;                                                      00193000
TIMEPROC ~ TIME(2) ;                                                    00194000
COMMENT E ARRAY PROVIDES EXTERNAL BITS FOR CHARACTER INTERNALLY;        00195000
FILL E[*] WITH "#", "1", "2", "3", "4", "5", "6", "7",                  00196000
               "8", "9", "@", "0", "0",                                 00197000
                                   ":", ">", "}", ",",                  00198000
               "/",  "S", "T", "U", "V", "W", "X", "Y",                 00199000
               "Z", "%", "!", " ", "=", "]", """, "$",                  00200000
               "J", "K", "L", "M", "N", "O", "P", "Q",                  00201000
               "R", "*", "-",                                           00202000
                         "|", ")", ";", "{", "+", "A",                  00203000
               "B", "C", "D", "E", "F",  "G", "H","I",                  00204000
               "[", "&", ".", "(", "<", "~" ; %NK,5-12-69               00205000
BEGIN LABEL MOD1 ;                                                      00206000
    LABEL SPCLOOP, CLS ;   %NK, 7-28-69                                 00206500
E[11].[44:2] := 3; % INSERTING OCT 12 FOR QUESTION MARK,NK,5-13-69      00207000
PAGECNT ~ 0 ;                                                           00208000
RECCNT :=  0 ;                                                          00209000
              LNCNT ~ 0;                                                00210000
              PRNT ~ 1;                                                 00211000
EL := ECCEXT := "|" ; % CHASE BRASS EDIT CONTROL CODE                   00212000
ECC ~ CONVERT2(E,EL) ;                                                  00213000
ADV := """ ;  % CHASE BRASS ADVANCE-TO-HOME-ROW CHARACTER               00214000
HRC := CONVERT2(E,ADV) ;     %NK,5-12-69                                00215000
    RPT := COMMONER DIV 1000;  %NK,7-28-69                              00215500
PAGES := COMMONER /10 - RPT | 100 ; % NK, 7-28-69                       00216000
MODESWITCH := COMMONER - PAGES | 10 - RPT | 1000 ; %NK, 7-28-69         00217000
MOD1:              %NK,5-12-69                                          00218000
IF MODESWITCH } 3 THEN BEGIN MODESWITCH ~ MODESWITCH - 3;  %NK,5-12-69  00219000
GO TO MOD1;END;            %NK,5-12-69                                  00220000
              SZ := 79 ; % MOHAWK BUFFER SIZE = 80                      00221000
IF PAGES = 0 THEN PAGES ~ 66;    % NK, 5-11-69                          00222000
              NEXT := FALSE;                                            00223000
              A[80] ~ "~";                                              00224000
FOR I := 1 STEP 1 UNTIL RPT DO BEGIN   %NK, 7-28-69                     00224200
SPCLOOP :  READ( MOHAWK) [ CLS] ;      %NK, 7-28-69                     00224300
GO TO SPCLOOP;       %NK, 7-28-69                                       00224400
CLS : CLOSE ( MOHAWK, *) ;         % NO REWIND   NK% 7-28-69            00224500
END ;         % NK, 7-28-69                                             00224600
END ; % INITIALIZATION                                                  00225000
RDTAPE:       READ(MOHAWK,FMA,FOR I:=0 STEP 1 UNTIL SZ DO A[I]) [EOJ];  00226000
RECCNT ~ RECCNT + 1 ;                                                   00227000
              IF NEXT THEN GO TO NOTFIRST;                              00228000
              IF A[80] = "~" THEN SZ := 79;                             00229000
              NEXT := TRUE;                                             00230000
NOTFIRST:     I := 0;                                                   00231000
LNSKP:        IF PRNT = 1 THEN                                          00232000
         BEGIN                                                          00233000
         CHAR ~A[I];                                                    00234000
KAR ~ CONVERT2(E,CHAR) ;                                                00235000
              FIXCNT2(KAR,  SFC,PRNT);                                  00236000
              IF NOT ((LNCNT = 0 AND PRNT = 1) OR (LNCNT > 0 AND        00237000
                   PRNT = 0) OR (LNCNT + SFC } PAGES)) THEN             00238000
BEGIN         %NK,5-12-69                                               00239000
IF MODESWITCH = 2 THEN GO TO EOJ;  %NK,5-12-69                          00240000
IF MODESWITCH = 1 THEN BEGIN WRITE(LINE[PAGE] ) ;  %NK,5-12-69          00241000
WRITE( LINE,FMCHEK,LNCNT, RECCNT) ;   %NK,5-16-69                       00242000
WRITE(LINE[PAGE] ) ; END ;   %NK,5-12-69                                00243000
END ;                                                                   00244000
IF KAR  = HRC OR                                                        00245000
LNCNT + SFC = PAGES THEN BEGIN                                          00246000
              LNCNT ~ 0;                                                00247000
              WRITE(LINE[PAGE]);                                        00248000
PAGECNT ~ PAGECNT + 1 ;                                                 00249000
              GO TO NEWPAGE; END;                                       00250000
              CNT1 ~ CNT2 ~ CNT4 ~ 0;                                   00251000
              IF SFC > 15 THEN BEGIN SFC ~ SFC - 16; CNT4 ~ 8; END;     00252000
              IF SFC >  7 THEN BEGIN SFC ~ SFC - 8 ; CNT4 ~ CNT4 +4;END;00253000
              IF SFC >  3 THEN BEGIN SFC ~ SFC - 4 ; CNT4 ~ CNT4+ 2;END;00254000
              IF SFC >  1 THEN BEGIN SFC ~ SFC - 2 ; CNT2 ~ 1; END;     00255000
              IF SFC >  0 THEN BEGIN SFC ~ SFC - 1;  CNT1 ~ 1; END;     00256000
              IF CNT4 ! 0 THEN                                          00257000
         BEGIN                                                          00258000
              FOR L ~ 1 STEP 1 UNTIL CNT4 DO                            00259000
              BEGIN                                                     00260000
              WRITE(LINE[DBL],FM1);                                     00261000
                   LNCNT ~ LNCNT + 2;                                   00262000
              END;                                                      00263000
         END;                                                           00264000
              IF CNT2 ! 0 THEN                                          00265000
         BEGIN                                                          00266000
              FOR L ~ 1 STEP 1 UNTIL CNT2 DO                            00267000
              BEGIN                                                     00268000
              WRITE(LINE[DBL],FM1);                                     00269000
                   LNCNT ~ LNCNT + 2;                                   00270000
              END;                                                      00271000
         END;                                                           00272000
              IF CNT1 ! 0 THEN                                          00273000
         BEGIN                                                          00274000
              FOR L ~ 1 STEP 1 UNTIL CNT1 DO                            00275000
              BEGIN                                                     00276000
                   WRITE(LINE,FM1);                                     00277000
                   LNCNT ~ LNCNT +1;                                    00278000
              END;                                                      00279000
         END;                                                           00280000
              IF LNCNT } PAGES THEN LNCNT ~ LNCNT - PAGES;              00281000
NEWPAGE:      PRNT ~ J ~ 0;                                             00282000
              I ~ I + 1;                                                00283000
              IF I > SZ THEN GO TO RDTAPE;                              00284000
         END;                                                           00285000
ECCHEK :      IF A[I] = ECCEXT THEN                                     00286000
         BEGIN                                                          00287000
              IF I > SZ - 1 THEN                                        00288000
           BEGIN                                                        00289000
              READ(MOHAWK,FMA,FOR I := 0 STEP 1 UNTIL SZ DO A[I])[EOJ]; 00290000
              I ~ -1;                                                   00291000
         END;                                                           00292000
         CHAR ~ A[I+1] ;                                                00293000
KAR ~ CONVERT2(E,CHAR) ; %NK,5-13-69                                    00294000
              FIXCNT2(KAR,  SFC,PRNT);                                  00295000
              IF (PRNT = 1 AND SFC > 0) THEN RD := 1 ELSE               00296000
              IF (PRNT = 1 AND SFC = 0) THEN RD := 0 ELSE               00297000
              IF PRNT = 0 THEN                                          00298000
           BEGIN                                                        00299000
              K ~ J;                                                    00300000
              J ~ J + SFC;                                              00301000
              IF J > 131 THEN BEGIN                                     00302000
                   M~J - 131;                                           00303000
                   J~ 131;                                              00304000
    FOR L ~ K STEP 1 UNTIL J DO B[L] ~ " " ;                            00305000
              WRITE(LINE[NO],FMB,FOR J~0 STEP 1 UNTIL 131 DO B[J]);     00306000
                   K~ 0;                                                00307000
                   J~M; END ;                                           00308000
              FOR L ~ K STEP 1 UNTIL J - 1 DO B[L] ~ " ";               00309000
              I ~ I + 2;                                                00310000
              IF I > SZ THEN GO TO RDTAPE;                              00311000
              GO TO ECCHEK;                                             00312000
           END;                                                         00313000
              FOR L ~ J STEP 1 UNTIL 131 DO B[L] ~ " ";                 00314000
              WRITE(LINE[NO],FMB,FOR J ~ 0 STEP 1 UNTIL 131 DO B[J]);   00315000
              J ~ 0;                                                    00316000
              IF RD = 1 THEN GO TO RDTAPE;                              00317000
              I ~ I + 2;                                                00318000
              IF I > SZ THEN GO TO RDTAPE;                              00319000
              GO TO LNSKP;                                              00320000
         END;                                                           00321000
              B[J] ~ A[I];                                              00322000
              J ~ J + 1;                                                00323000
              I ~ I + 1;                                                00324000
              IF J > 131 THEN                                           00325000
         BEGIN                                                          00326000
              WRITE(LINE[NO],FMB,FOR J ~ 0 STEP 1 UNTIL 131 DO B[J]);   00327000
              J ~ 0;                                                    00328000
              PRNT ~ 1;                                                 00329000
IF I > SZ THEN GO TO RDTAPE ELSE GO TO LNSKP;                           00330000
         END;                                                           00331000
              IF I > SZ THEN GO TO RDTAPE ELSE                          00332000
              GO TO ECCHEK;                                             00333000
EOJ :                                                                   00334000
BEGIN INTEGER Q;                                                        00335000
    WRITE(LINE[PAGE] ) ;                                                00336000
    WRITE(LINE,OUTHEAD) ;                                               00337000
    WRITE(LINE[DBL] ) ;                                                 00338000
    WRITE(LINE[DBL],RECFORM,RECCNT) ;                                   00339000
    WRITE(LINE[DBL], PAGEFORM,PAGECNT) ;                                00340000
    WRITE(LINE[DBL],LINEFORM,PAGES) ;                                   00341000
TIMEIO := (TIME(3) - TIMEIO)/60 ;                                       00342000
    WRITE(LINE[DBL],IOTIME,TIMEIO) ;                                    00343000
TIMEPROC := (TIME(2) - TIMEPROC)/60 ;                                   00344000
    WRITE(LINE[DBL],PROCTIME,TIMEPROC) ;                                00345000
    WRITE(LINE[DBL],MODEFORM[MODESWITCH],MODESWITCH) ;                  00346000
END ;                                                                   00347000
         END.                                                           00348000
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
