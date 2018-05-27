BEGIN                                                                   00001000
COMMENT BEGIN DOCUMENTATION:                                                    
PROGRAM-ID:   9UTL48R/MULTSUB                                 0019UTL48R/MULTSUB
SYNOPSIS:     THIS PROGRAM WILL LOCATE UP TO 20 PARTICULAR    0029UTL48R/MULTSUB
              DATA-NAMES OR CHARACTER STRINGS IN A CARD FILE  0039UTL48R/MULTSUB
              AND SUBSTITUTE A CORRESPONDING CHARACTER STRING 0049UTL48R/MULTSUB
              FOR EACH WHICH THE PROGRAM LOCATES.  THE NEW    0059UTL48R/MULTSUB
              CHARACTER STRING MAY BE SMALLER THAN, EQUAL TO, 0069UTL48R/MULTSUB
              OR GREATER THAN THE STRING BEING REPLACED.      0079UTL48R/MULTSUB
              WHEN NECESSARY, NEW CARDS WILL BE CREATED TO    0089UTL48R/MULTSUB
              HANDLE EXCESS.                                  0099UTL48R/MULTSUB
DATE-WRITTEN: APRIL 8, 1969                                   0109UTL48R/MULTSUB
AUTHOR:       D.M. KUZELA, SOFTWARE SERVICES, CHASE BRASS     0119UTL48R/MULTSUB
PUBLISHER: N R KARL, SOFTWARE SERVICES, CHASE BRASS, CLEVELAND                  
PUBLICATION-DATE: AUGUST 27, 1969                                               
LANGUAGE:     ALGOL                                           0129UTL48R/MULTSUB
PURPOSE:      EXPAND ABBREVIATIONS, CHANGE CHARACTER STRINGS, 0139UTL48R/MULTSUB
              OR ALTER LITERALS.                              0149UTL48R/MULTSUB
              (NOTE- A CHARACTER STRING ENCLOSED BY QUOTES    0169UTL48R/MULTSUB
              CAN BE CHANGED WITH THE QUOTES AS PART OF THE   0179UTL48R/MULTSUB
              CHARACTER STRING).                                                
INPUT:        1) FILE-ID:  CARD                               0189UTL48R1MULTSUB
              THIS CARD FILE CONTAINS THE CHARACTER STRINGS   0199UTL48R1MULTSUB
              WHICH ARE TO BE REPLACED AND THE REPLACEMENT    0209UTL48R1MULTSUB
              STRINGS. THE WORD TO BE REPLACED COMES FIRST    0219UTL48R1MULTSUB
              ON THE CARD OR IF THE STRINGS ARE TOO BIG TO    0229UTL48R1MULTSUB
              FIT TOGETHER ON THE SAME CARD, THEY ARE PLACED  0239UTL48R1MULTSUB
              ON SUCCESSIVE CARDS, THE STRING TO BE REPLACED  0249UTL48R1MULTSUB
              COMES ON THE FIRST CARD,  THE CHARACTER STRINGS 0259UTL48R1MULTSUB
              ARE  ENCLOSED BY QUOTATION MARKS AND ARE NOT TO 0269UTL48R1MULTSUB
              EXCEED 63 CHARACTERS IN LENGTH.                 0279UTL48R1MULTSUB
              IMPORTANT: THERE SHOULD ALWAYS BE COMPLETE PAIRS0289UTL48R1MULTSUB
              OF CHARACTER STRINGS.                           0299UTL48R1MULTSUB
              2) FILE-ID:  KARD                               0309UTL48R1MULTSUB
              THIS IS THE FILE UPON WHICH THE CHANGES ARE TO  0319UTL48R1MULTSUB
              BE MADE.  THIS DECK IS TREATED AS ALGOL OR      0329UTL48R1MULTSUB
              FORTRAN,.OR COBOL DEPENDING UPON THE COMMON CARD0339UTL48R1MULTSUB
              WHICH FOLLOWS THE EXECUTE CARD.  A "1" INDICATES0349UTL48R1MULTSUB
              COBOL, A "3", FORTRAN OR ALGOL.                 0359UTL48R1MULTSUB
              EXAMPLE:                                        0369UTL48R1MULTSUB
                      CC COMMON = 3                           0379UTL48R1MULTSUB
PROCESS:      EACH CARD IS CHECKED FOR ALL REPLACEMENT STRINGS0389UTL48R1MULTSUB
              AND IF LOCATED, THE REPLACEMENT IS MADE.  IF    0399UTL48R1MULTSUB
              NECESSARY, NEW RECORDS ARE PRODUCED TO HANDLE   0409UTL48R1MULTSUB
              OVERFLOW.  WHEN ALL CHECKS ARE MADE ON A RECORD,0419UTL48R1MULTSUB
              THE COMPLETE RECORD CARD IS PUNCHED.            0429UTL48R1MULTSUB
OUTPUT:       FILE-ID:  CRDPNCH                               0439UTL48R1MULTSUB
              THIS IS A NEW PUNCHED DECK, CONTAINING THE NEW  0449UTL48R1MULTSUB
              CHARACTER STRINGS.                              0459UTL48R1MULTSUB
BEGIN FLOWCHART:                                                                
LABEL  SEQBEG   SEQEND   DESCRIPTION                                            
      #303000  #319000   READ FILE OF STRINGS TO-BE-REPLACED AND THEIR          
                         REPLACEMENT STRINGS AND STORE THEM INTO AN ARRAY.      
                         CARD IMAGES OF THE RECORDS ON WHICH THE                
                         CHANGES ARE TO BE MADE ARE PUT INTO A DISK             
                         FILE.                                                  
01    #320000  #327000   READ CARD IMAGE RECORD.  PUT TO-BE-REPLACED            
                         AND REPLACEMENT PAIR INTO HOLD AREA.                   
02    #075000  #302000   LOOKFOR PROCEDURE(CHECKS FOR TO-BE-REPLACED            
                         STRING IN THE RECORD AND MAKES AN APPROPRIATE          
                         CHANGE-PROVIDES FOR NEW RECORDS IN CASE OF             
                         OVERFLOW).                                             
      #329000  #334000   CHECK VALUE FROM LOOKFOR PROCEDURE.                    
      #335000  #372000   CHECK FOR NUMBER OF TO-BE-REPLACED--REPLACEMENT        
                         STRINGS USED.  PROVIDES FOR WRITING NEW CARD           
                         IMAGE DISK RECORDS IF LOOKFOR CALLS FOR                
                         OVERFLOW.  IF ALL TO-BE-REPLACED SETS HAVE             
                         BEEN CHECKED AGAINST A PARTICULAR CARD IMAGE,          
                         THE IMAGE IS PUT ONTO A DISK FILE OF COMPLETED         
                         RECORDS AND THE PROGRAM RETURNS TO 01 TO HAVE          
                         A NEW IMAGE CHECKED AGAINST THE REPLACEMENT            
                         SETS, ELSE A NEW PAIR OF TO-BE-REPLACED--REPLACEMENT   
                         STRINGS IS ACCESSED AND THE PROGRAM RETURNED           
                         TO 02.                                                 
      #373000  #376000   PUNCH CARDS FROM THE DISK FILE OF NEW ALTERED          
                         RECORDS.                                               
END FLOWCHART.                                                                  
BEGIN TERM DEFINITION:                                                          
    CARD = FILE CONTAINING TO-BE-REPLACED AND REPLACEMENT STRINGS.              
    KARD = FILE CONTAINS RECORDS ON WHICH CHANGES ARE TO BE MADE.               
   PSUDO = DISK FILE OF IMAGES FROM KARD.                                       
     FIL = SEE PSUDO.                                                           
 PNCHOUT = FILE OF IMAGES AFTER CHANGES HAVE BEEN MADE.                         
   PUNCH = SEE PNCHOUT.                                                         
 CRDPNCH = PUNCHED CARDS(ONLY IMAGES ON WHICH CHANGES WERE MADE).               
       B = ARRAY IN WHICH TO-BE-REPLACED STRINGS ARE HELD FOR CHECKING AGAINST  
           CARD IMAGES.                                                         
       C = ARRAY IN WHICH REPLACEMENT STRINGS ARE HELD DURING CHECKING.         
      BC = STORAGE ARRAY FROM WHICH STRINGS ARE MOVED INTO B AND C AS NEEDED.   
       V = 80 POSITION CARD ARRAY.                                              
       A = 80 POSITION CARD IMAGE ARRAY.                                        
       L = 80 POSITION OVERFLOW ARRAY.                                          
  LIMITS = ARRAY CONTAINING CHARACTER COUNT OF CORRESPONDING ITEMS STORED IN BC.
LANGSWITCH                                                                      
         = LANGUAGE SWITCH.                                                     
  CLIMIT = SIZE OF STRING PRESENTLY HELD IN C.                                  
       I = ARRAY POSITION INDICATOR(BEGINNING OF TO-BE-REPLACED STRING).        
       J = ARRAY POSITION INDICATOR(END OF TO-BE-REPLACED STRING).              
    ADD1 = FILE ADDRESS.                                                        
   BCSUB = ARRAY SUBSCRIPT(REPLACEMENT PAIRS).                                  
     TYP = TYPE OF REPLACEMENT MADE.                                            
       N = ARRAY POSITION INDICATOR.                                            
     CNT = NUMBER OF TO-BE-REPLACED-REPLACEMENT PAIRS.                          
     OPT = REPLACEMENT INDICATOR SWITCH.                                        
     ANT = CARD SEQUENCE NUMBER.                                                
     NUB = LAST OLD FILE ADDRESS ACCESSED.                                      
END TERM DEFINITION.                                                            
END DOCUMENTATION;                                                              
INTEGER LANGSWITCH;                                                     00002000
FILE IN CARD(2,10);                                                     00003000
FILE IN KARD(2,10);                                                     00004000
FILE OUT PNCHOUT DISK SERIAL [20:100] "TOBEPNH" (2,10,30);              00005000
FILE OUT PSUDO DISK RANDOM [20:100] "INTERME" (2,10,30);                00006000
FILE FIL DISK RANDOM "INTERME" (2,10,30);                               00007000
FILE IN PUNCH DISK SERIAL "TOBEPNH" (2,10,30);                          00008000
FILE OUT CRDPNCH 0(2,10);                                               00009000
FORMAT FM(80A1);                                                        00010000
ALPHA ARRAY V,L,A[1:80];                                                00011000
ALPHA ARRAY B,C[1:64],BC[1:20,1:128];                                   00012000
INTEGER ARRAY LIMITS[1:20,1:2];                                         00013000
INTEGER ANT,OPT,NUB;                                                    00014000
INTEGER I,J,BLIMIT,CLIMIT,ADD1,BCSUB,N,TYP,CNT;                         00015000
LABEL BAC;                                                              00016000
LABEL ABA,ABB,ZZ,ABC,ABD,ABE,ABK,ABQ,ABF,ABZ,ABG,ABH,ABI,ABJ;           00017000
LABEL WRITE1,WRITE2,WRITE3;                                             00018000
LIST L1(FOR N~1 STEP 1 UNTIL 80 DO V[N]);                               00019000
LIST L2(FOR N~1 STEP 1 UNTIL 80 DO A[N]);                               00020000
LIST L3( FOR N~1 STEP 1 UNTIL 80 DO L[N]);                              00021000
PROCEDURE FREEREAD(BC,LIMITS,V,I,J);                                    00022000
VALUE I; INTEGER I,J;                                                   00023000
ALPHA ARRAY BC[1,1]; INTEGER ARRAY LIMITS[1,1]; ALPHA ARRAY V[1];       00024000
BEGIN                                                                   00025000
LABEL ZOT,SUT,ZOO,SOO,SOOT,SOUT;                                        00026000
INTEGER N,R,U,K;                                                        00027000
IF J!0 THEN BEGIN R~1; GO TO ZOT; END;                                  00028000
FOR N~1 STEP 1 UNTIL 80 DO IF V[N]=""" THEN  GO TO SUT;                 00029000
SUT:                                                                    00030000
N~N+1; BC[I,1]~V[N]; K~N; N~N+1;                                        00031000
ZOO:                                                                    00032000
IF V[N]!""" THEN BC[I,N-K+1]~V[N] ELSE                                  00033000
BEGIN     IF V[N+1]=""" THEN BEGIN BC[I,N-K+1]~V[N];                    00034000
LIMITS[I,1]~N-K+1; END ELSE LIMITS[I,1]~N-K;                            00035000
R~N+2; GO TO ZOT; END;                                                  00036000
N~N+1;  GO TO ZOO;                                                      00037000
ZOT:   U~0;                                                             00038000
FOR N~R STEP 1 UNTIL 80 DO IF V[N]=""" THEN GO TO SOO;                  00039000
J~1; GO TO SOOT;                                                        00040000
SOO: N~N+1; K~N; BC[I,65]~V[N]; N~N+1;                                  00041000
SOUT: IF V[N]!""" THEN BC[I,65+N+U-K]~V[N] ELSE BEGIN                   00042000
IF V[N+1]=""" THEN BEGIN BC[I,66+N+U-K]~V[N]; LIMITS[I,2]~N-K+U+1;      00043000
END ELSE LIMITS[I,2]~N-K+U; J~0; GO TO SOOT; END;                       00044000
IF  N=80 THEN BEGIN U~81-K; N~0; K~1; END;                              00045000
N~N+1;  GO TO SOUT;                                                     00046000
SOOT:   END;                                                            00047000
PROCEDURE NUMSEQ(LANGSWITCH,A,ANT);                                     00048000
INTEGER ANT,LANGSWITCH;                                                 00049000
ALPHA ARRAY A[1];                                                       00050000
BEGIN                                                                   00051000
LABEL MA,MB,MC,MD;                                                      00052000
INTEGER OK;                                                             00053000
INTEGER ARRAY IA[1:6];                                                  00054000
IF LANGSWITCH =3 THEN GO TO MA;                                         00055000
FOR OK~1 STEP 1 UNTIL 6 DO IA[OK]~A[OK];                                00056000
IF IA[6]+ANT}10 THEN BEGIN                                              00057000
IF IA[5]+1=10 THEN BEGIN                                                00058000
IA[4]~IA[4]+1; IA[5]~0;                                                 00059000
IA[6]~IA[6]+ANT-10; GO TO MB END ELSE BEGIN                             00060000
IA[5]~IA[5]+1; IA[6]~IA[6]+ANT-10;  GO TO MB; END; END;                 00061000
IA[6]~IA[6]+ANT; GO TO MB;                                              00062000
MA:                                                                     00063000
FOR OK~1 STEP 1 UNTIL 3 DO IA[OK]~A[77+OK];                             00064000
IF IA[3]+ANT}10 THEN BEGIN                                              00065000
IF IA[2]+1=10 THEN BEGIN IA[1]~IA[1]+1; IA[2]~0; IA[3]~IA[3]+ANT-10;    00066000
GO TO MD END ELSE BEGIN IA[2]~IA[2]+1; IA[3]~IA[3]+ANT-10;              00067000
GO TO MD;END; END;                                                      00068000
IA[3]~IA[3]+ANT;                                                        00069000
GO TO MD;                                                               00070000
MB: FOR OK~1 STEP 1 UNTIL 6 DO A[OK]~IA[OK]; GO TO MC;                  00071000
MD:                                                                     00072000
FOR OK~1 STEP 1 UNTIL 3 DO A[77+OK]~IA[OK];                             00073000
MC: END;                                                                00074000
INTEGER PROCEDURE LOOKFOR(L,A,B,C,BLIMIT,CLIMIT,LANGSWITCH);            00075000
VALUE LANGSWITCH; INTEGER LANGSWITCH,BLIMIT,CLIMIT;                     00076000
ALPHA ARRAY L,B,C,A[1];                                                 00077000
BEGIN                                                                   00078000
LABEL AGIN,DUNN,TERR,LAUR,JEAN,JO,NUCRD,BARB,SHEL,PAT,CONN,RHITE;       00079000
INTEGER MC,BSW,ASW,QT,F,N,I,J,T,M,Y,H,ASTART;                           00080000
BOOLEAN       TRU,TREW;                                                 00081000
BOOLEAN PROCEDURE  SERCH(A,B,I,J,ASTART,BLIMIT); %                      00082000
COMMENT BEGIN TERM DEFINITION HERE                                      00083000
     SEARCH > PROCEDURE WHICH SEARCHES A CARD IMAGE A[1:80] FOR FIELD   00084000
        TO RIGHT. IF A MATCH IS FOUND SEARCH~TRUE, I > STARTING         00085000
        POSITION OF B IN A, J > ENDING POSITION OF B IN A ELSE          00086000
        SEARCH~FALSE.                                                   00087000
    L > POINTER IN A FROM ASTART TO MATCHPOINT.                         00088000
     M > POINTER FOR A BEYOND MATCH POINT.                              00089000
     O > POINTER FOR B BEYOND MATCH POINT.                              00090000
        END   TERM DEFINITION HERE; %                                   00091000
VALUE ASTART,BLIMIT; %                                                  00092000
COMMENT BEGIN FLOWCHART HERE                                            00093000
   SEQBEGIN   SEQEND                PROCESS                             00094000
     400       450    SCAN CARD IMAGE A FOR MATCH WITH FIRST CHARACTER  00095000
     400       450     OF B.                                            00096000
     925       925    IF THERE IS NO MATCH ON CARD SET SEARCH ~ FALSE   00097000
     500       900    IF THERE IS A MATCH MARK THE START POINT IN A,    00098000
     650       700     STARTING WITH SECOND CHARACTER OF B SCAN FOR     00099000
     650       700     EQUIVALENCE OF FIELD IN A.                       00100000
     750       750    IF FURTHER EQUIVALENCE STOPS SHORT OF B[BLIMIT]   00101000
     750       750    GO TO 500, CONTINUING SCAN.                       00102000
     800       875    IF A COMPLETE MATCH OF B IS FOUND IN A            00103000
     800       800     MARK ENDPOINT IN A,                              00104000
     850       850     SET SEARCH~TRUE, AND GO OUT.                     00105000
        END   FLOWCHART HERE; %                                         00106000
INTEGER I,J,ASTART,BLIMIT; %                                            00107000
ALPHA ARRAY A,B[1]; %                                                   00108000
BEGIN %                                                                 00109000
     LABEL OWT; %                                                       00110000
        LABEL ARUND; %                                                  00111000
     INTEGER L,M,O ; %                                                  00112000
         FOR L ~ ASTART STEP 1 UNTIL 80 DO %                            00113000
           IF A[L] = B[1] THEN %                                        00114000
     BEGIN %                                                            00115000
          I ~ L ; %                                                     00116000
          M ~ L + 1; %                                                  00117000
           FOR O ~ 2 STEP 1 UNTIL BLIMIT DO                             00118000
               IF A[M] = B[O] THEN M ~ M+1 ELSE %                       00119000
               GO TO ARUND ; %                                          00120000
          J ~ M - 1 ; %                                                 00121000
           SERCH ~ TRUE ; %                                             00122000
          GO TO OWT; %                                                  00123000
ARUND : END ; %                                                         00124000
     SERCH ~ FALSE; %                                                   00125000
OWT : END ; %                                                           00126000
BOOLEAN PROCEDURE LEGALCOMP2(A,I,J,LANGSWITCH);                         00127000
VALUE I,J,LANGSWITCH;  INTEGER I,J,LANGSWITCH;                          00128000
ALPHA ARRAY A[1];                                                       00129000
BEGIN                                                                   00130000
LABEL  BEHIN1,BEHIN2,KNOWNO;                                            00131000
INTEGER M,N;  LABEL ALGFOR,OKAY,KAPUT; M~I-1; N~J+1;                    00132000
IF LANGSWITCH = 3 THEN GO TO ALGFOR;                                    00133000
IF I = 8 THEN GO TO BEHIN1;                                             00134000
IF A[M]=" " THEN GO TO BEHIN1;                                          00135000
IF A[M]="(" THEN GO TO BEHIN1;                                          00136000
IF A[M]="[" THEN GO TO BEHIN1;                                          00137000
GO TO KNOWNO;                                                           00138000
BEHIN1:                                                                 00139000
IF I=72 THEN GO TO OKAY;                                                00140000
IF A[N]=" " THEN GO TO OKAY;                                            00141000
IF A[N]="," THEN GO TO OKAY;                                            00142000
IF A[N]="." THEN GO TO OKAY;                                            00143000
IF A[N]=";" THEN GO TO OKAY;                                            00144000
IF A[N]="~" THEN GO TO OKAY;                                            00145000
IF A[N]=")" THEN GO TO OKAY;                                            00146000
IF A[N]="(" THEN GO TO OKAY;                                            00147000
IF A[N]="[" THEN GO TO OKAY;                                            00148000
IF A[N]="]" THEN GO TO OKAY;                                            00149000
IF A[N]=":" THEN GO TO OKAY;                                            00150000
GO TO KNOWNO;                                                           00151000
ALGFOR:                                                                 00152000
IF I = 1 THEN GO TO BEHIN2;                                             00153000
IF A[M]=" " THEN GO TO BEHIN2;                                          00154000
IF A[M]="<" THEN GO TO BEHIN2;                                          00155000
IF A[M]="~" THEN GO TO BEHIN2;                                          00156000
IF A[M]="&" THEN GO TO BEHIN2;                                          00157000
IF A[M]="[" THEN GO TO BEHIN2;                                          00158000
IF A[M]="*" THEN GO TO BEHIN2;                                          00159000
IF A[M]="(" THEN GO TO BEHIN2;                                          00160000
IF A[M]=";" THEN GO TO BEHIN2;                                          00161000
IF A[M]="{" THEN GO TO BEHIN2;                                          00162000
IF A[M]="-" THEN GO TO BEHIN2;                                          00163000
IF A[M]="/" THEN GO TO BEHIN2;                                          00164000
IF A[M]="," THEN GO TO BEHIN2;                                          00165000
IF A[M]="=" THEN GO TO BEHIN2;                                          00166000
IF A[M]=">" THEN GO TO BEHIN2;                                          00167000
IF A[M]="}" THEN GO TO BEHIN2;                                          00168000
IF A[M]="+" THEN GO TO BEHIN2;                                          00169000
IF A[M]="|" THEN GO TO BEHIN2;                                          00170000
IF A[M]="!" THEN GO TO BEHIN2;                                          00171000
IF A[M]=":" THEN GO TO BEHIN2;                                          00172000
GO TO KNOWNO;                                                           00173000
BEHIN2:                                                                 00174000
IF A[N]=" " THEN GO TO OKAY;                                            00175000
IF A[N]="<" THEN GO TO OKAY;                                            00176000
IF A[N]="~" THEN GO TO OKAY;                                            00177000
IF A[N]="&" THEN GO TO OKAY;                                            00178000
IF A[N]="[" THEN GO TO OKAY;                                            00179000
IF A[N]="]" THEN GO TO OKAY;                                            00180000
IF A[N]="(" THEN GO TO OKAY;                                            00181000
IF A[N]=")" THEN GO TO OKAY;                                            00182000
IF A[N]=";" THEN GO TO OKAY;                                            00183000
IF A[N]="{" THEN GO TO OKAY;                                            00184000
IF A[N]="-" THEN GO TO OKAY;                                            00185000
IF A[N]="/" THEN GO TO OKAY;                                            00186000
IF A[N]="," THEN GO TO OKAY;                                            00187000
IF A[N]="=" THEN GO TO OKAY;                                            00188000
IF A[N]=">" THEN GO TO OKAY;                                            00189000
IF A[N]="}" THEN GO TO OKAY;                                            00190000
IF A[N]="+" THEN GO TO OKAY;                                            00191000
IF A[N]="." THEN GO TO OKAY;                                            00192000
IF A[N]="|" THEN GO TO OKAY;                                            00193000
IF A[N]="!" THEN GO TO OKAY;                                            00194000
IF A[N]=":" THEN GO TO OKAY;                                            00195000
IF A[N]="%" THEN GO TO OKAY;                                            00196000
IF A[N]="*" THEN GO TO OKAY;                                            00197000
KNOWNO:  LEGALCOMP2~FALSE; GO TO KAPUT;                                 00198000
OKAY: LEGALCOMP2~TRUE;                                                  00199000
KAPUT:  END;                                                            00200000
PROCEDURE INCERT(A,B,C,L,I,J,N,BLIMIT,CLIMIT,ASW,BSW,T,M,MC,F,ASTART);  00201000
VALUE BLIMIT,CLIMIT;                                                    00202000
INTEGER N;                                                              00203000
INTEGER I,J,CLIMIT,BLIMIT,ASW,BSW,T,M,MC,F,ASTART;                      00204000
ALPHA ARRAY A,B,C,L[1];                                                 00205000
BEGIN                                                                   00206000
INTEGER X,Y,P;                                                          00207000
LABEL AB,BC,CD,DE,EF,FG;                                                00208000
LABEL  QUOL, OXT;                                                       00209000
IF BLIMIT < CLIMIT THEN GO TO AB;                                       00210000
FOR Y~1 STEP 1 UNTIL CLIMIT DO A[I+Y-1]~C[Y];                           00211000
FOR Y~I+CLIMIT STEP 1 UNTIL J DO A[Y]~" "; ASTART~J+1;                  00212000
T~0;                                                                    00213000
GO TO BC;                                                               00214000
AB: X~CLIMIT-BLIMIT;                                                    00215000
M~X;                                                                    00216000
IF ASW =1 THEN GO TO DE;                                                00217000
FOR Y~73 STEP 1 UNTIL 80 DO L[Y]~A[Y];                                  00218000
DE:                                                                     00219000
IF J+F|X>72 THEN GO TO CD;                                              00220000
IF I = 1 THEN GO TO QUOL;                                               00221000
FOR Y~N STEP 1 UNTIL I-1 DO L[Y+(F-1)|X]~A[Y];                          00222000
QUOL:                                                                   00223000
FOR Y~1 STEP 1 UNTIL CLIMIT DO L[Y+I+(F-1)|M-1]~C[Y];                   00224000
T~0;                                                                    00225000
MC~J+F|X;  F~F+1;                                                       00226000
ASW~1;                                                                  00227000
N~J+1;                                                                  00228000
ASTART~J+1;                                                             00229000
GO TO BC;                                                               00230000
CD: F~F-1;                                                              00231000
ASW~1;                                                                  00232000
IF I+F|X{73 THEN FOR Y~I-1 STEP -1 UNTIL N DO IF A[Y]=" " THEN          00233000
BEGIN P~Y  ; GO TO EF; END;                                             00234000
FOR Y~N+72-MC STEP -1 UNTIL N DO IF A[Y]=" " THEN BEGIN P~Y;            00235000
GO TO EF; END;                                                          00236000
EF: FOR Y~N STEP 1 UNTIL P DO L[Y+F|X]~A[Y];                            00237000
OXT:                                                                    00238000
BSW~1;                                                                  00239000
ASTART~P+1;                                                             00240000
FOR Y~ASTART+F|X STEP 1 UNTIL 72 DO L[Y]~" ";                           00241000
T~0;                                                                    00242000
BC:  END;                                                               00243000
ASTART~1;                                                               00244000
MC~CLIMIT-BLIMIT;                                                       00245000
BSW~0;                                                                  00246000
ASW~0;                                                                  00247000
QT~0; F~1; N~1;                                                         00248000
AGIN: TRU~SERCH(A,B,I,J,ASTART,BLIMIT);                                 00249000
IF NOT TRU THEN BEGIN                                                   00250000
F~F-1;                                                                  00251000
IF QT!0 THEN GO TO TERR;                                                00252000
LOOKFOR~0; GO TO DUNN;                                                  00253000
END;                                                                    00254000
         TREW ~LEGALCOMP2(A,I,J,LANGSWITCH);                            00255000
         IF NOT TREW THEN                                               00256000
    BEGIN                                                               00257000
ASTART~J+1; GO TO AGIN;                                                 00258000
    END;                                                                00259000
INCERT(A,B,C,L,I,J,N,BLIMIT,CLIMIT,ASW,BSW,T,M,MC,F,ASTART);            00260000
QT~1;                                                                   00261000
IF BSW=1 THEN GO TO PAT;                                                00262000
IF T=0 THEN GO TO AGIN;                                                 00263000
TERR:                                                                   00264000
IF BLIMIT}CLIMIT THEN BEGIN LOOKFOR~1; GO TO DUNN; END;                 00265000
LAUR:                                                                   00266000
FOR Y~72 STEP -1 UNTIL 1 DO IF A[Y]!" " THEN BEGIN                      00267000
IF 72-MC>Y-N THEN BEGIN H~Y; GO TO JO; END ELSE GO TO JEAN;             00268000
    END;                                                                00269000
JO: FOR Y~N STEP 1 UNTIL H DO L[Y+F|M]~A[Y];                            00270000
FOR Y~H+1+F|M STEP 1 UNTIL 72 DO L[Y]~" ";                              00271000
LOOKFOR~4; GO TO DUNN;                                                  00272000
JEAN:  FOR Y~N+F|M STEP 1 UNTIL 72 DO L[Y]~" ";                         00273000
FOR Y~72+N-1-MC STEP -1 UNTIL N-1 DO IF A[Y]=" " THEN BEGIN             00274000
ASTART~Y-1; GO TO NUCRD; END;                                           00275000
NUCRD:                                                                  00276000
FOR Y~ N  STEP 1 UNTIL ASTART DO L[Y+F|M]~A[Y];                         00277000
IF LANGSWITCH=3 THEN GO TO BARB;                                        00278000
FOR Y~ASTART+2 STEP 1 UNTIL 72 DO A[Y-ASTART+10]~A[Y];                  00279000
FOR Y~83 -ASTART STEP 1 UNTIL 72 DO A[Y]~" ";                           00280000
GO TO RHITE;                                                            00281000
BARB:                                                                   00282000
FOR Y~ASTART+2 STEP 1 UNTIL 72 DO A[Y-ASTART-1]~A[Y];                   00283000
FOR  Y~72-ASTART STEP 1 UNTIL 72 DO A[Y]~" ";                           00284000
RHITE:                                                                  00285000
      LOOKFOR~2;                                                        00286000
GO TO DUNN;                                                             00287000
PAT:                                                                    00288000
IF LANGSWITCH =3 THEN GO TO CONN;                                       00289000
FOR Y~ASTART STEP 1 UNTIL 72 DO A[Y-ASTART+12]~A[Y];                    00290000
FOR Y~85-ASTART STEP 1 UNTIL 72 DO A[Y]~" ";                            00291000
GO TO SHEL;                                                             00292000
CONN:                                                                   00293000
FOR Y~ASTART STEP 1 UNTIL 72 DO A[Y-ASTART+1]~A[Y];                     00294000
FOR Y~74-ASTART STEP 1 UNTIL 72 DO A[Y]~" ";                            00295000
SHEL: ASTART~1;                                                         00296000
BSW~0;                                                                  00297000
N~1;                                                                    00298000
ASW~0;                                                                  00299000
T~0;   QT~1; F~1;                                                       00300000
                    LOOKFOR~3;                                          00301000
DUNN: END;                                                              00302000
I~1;                                                                    00303000
ZZ: J~0;                                                                00304000
ABB:READ(CARD,FM,L1)[ABA];                                              00305000
FREEREAD(BC,LIMITS,V,I,J);                                              00306000
IF J=1 THEN GO TO ABB;                                                  00307000
I~I+1; GO TO ZZ;                                                        00308000
ABA: CLOSE(CARD,RELEASE); CNT~I-1;                                      00309000
ADD1~20;                                                                00310000
ABD: READ(KARD,FM,L1)[ABE];                                             00311000
IF LANGSWITCH=3 THEN FOR N~78 STEP 1 UNTIL 80 DO IF V[N]=" " THEN       00312000
V[N]~0;                                                                 00313000
IF LANGSWITCH=1 THEN FOR N~ 1 STEP 1 UNTIL  6 DO IF V[N]=" " THEN       00314000
V[N]~0;                                                                 00315000
WRITE(PSUDO[ADD1],FM,L1);                                               00316000
ADD1~ADD1+1;                                                            00317000
GO TO ABD;                                                              00318000
ABE: CLOSE(KARD,RELEASE); LOCK(PSUDO,SAVE);                             00319000
NUB~ADD1~20;                                                            00320000
BCSUB~1;                                                                00321000
ABK: READ(FIL[ADD1],FM,L2)[BAC];                                        00322000
IF ADD1>NUB THEN BEGIN ANT~OPT~0; NUB~ADD1;END;                         00323000
ABQ: BLIMIT~LIMITS[BCSUB,1]; CLIMIT~LIMITS[BCSUB,2];                    00324000
FOR N~1 STEP 1 UNTIL BLIMIT DO B[N]~BC[BCSUB,N];                        00325000
FOR N~65 STEP 1 UNTIL CLIMIT+64 DO C[N-64]~BC[BCSUB,N];                 00326000
ABZ:                                                                    00327000
TYP~LOOKFOR(L,A,B,C,BLIMIT,CLIMIT,LANGSWITCH);                          00328000
IF TYP=0 THEN GO TO ABG;                                                00329000
OPT~1;                                                                  00330000
IF TYP=1 THEN GO TO ABH;                                                00331000
IF TYP=2 THEN GO TO ABI;                                                00332000
IF TYP=4 THEN GO TO ABJ;                                                00333000
GO TO ABI;                                                              00334000
ABG:                                                                    00335000
     IF BCSUB>CNT-1 THEN GO TO WRITE1;                                  00336000
BCSUB~BCSUB+1; GO TO ABQ;                                               00337000
ABH: IF BCSUB>CNT-1 THEN GO TO WRITE1;                                  00338000
BCSUB~BCSUB+1; GO TO ABQ;                                               00339000
ABJ: IF BCSUB>CNT-1 THEN GO TO WRITE2;                                  00340000
BCSUB~BCSUB+1;                                                          00341000
WRITE(FIL[ADD1],FM,L3);                                                 00342000
GO TO ABK;                                                              00343000
ABI: IF BCSUB>CNT-1 THEN GO TO WRITE3;                                  00344000
BCSUB~BCSUB+1;                                                          00345000
WRITE(FIL[ADD1],FM,L2);   WRITE(FIL[ADD1-1],FM,L3);                     00346000
ADD1~ADD1-1;                                                            00347000
GO TO ABK;                                                              00348000
WRITE1:                                                                 00349000
IF OPT=0 THEN BEGIN ADD1~ADD1+1; BCSUB~1; GO TO ABK; END;               00350000
NUMSEQ(LANGSWITCH,A,ANT);                                               00351000
WRITE(PNCHOUT,FM,L2);                                                   00352000
ANT~ANT+1;                                                              00353000
ADD1~ADD1+1; BCSUB~1; GO TO ABK;                                        00354000
WRITE2:                                                                 00355000
NUMSEQ(LANGSWITCH,L,ANT);                                               00356000
WRITE(PNCHOUT,FM,L3);                                                   00357000
ANT~ANT+1;                                                              00358000
ADD1~ADD1+1; BCSUB~1; GO TO ABK;                                        00359000
WRITE3:                                                                 00360000
        IF TYP=3 THEN BEGIN                                             00361000
NUMSEQ(LANGSWITCH,L,ANT);                                               00362000
WRITE(PNCHOUT,FM,L3);                                                   00363000
ANT~ANT+1;                                                              00364000
GO TO ABZ; END;                                                         00365000
NUMSEQ(LANGSWITCH,L,ANT);                                               00366000
WRITE(PNCHOUT,FM,L3);                                                   00367000
ANT~ANT+1;                                                              00368000
NUMSEQ(LANGSWITCH,A,ANT);                                               00369000
WRITE(PNCHOUT,FM,L2);                                                   00370000
ANT~ANT+1;                                                              00371000
ADD1~ADD1+1; BCSUB~1; GO TO ABK;                                        00372000
BAC: LOCK(PNCHOUT,SAVE);                                                00373000
ABF: READ(PUNCH,FM,L1)[ABC];                                            00374000
WRITE(CRDPNCH,FM,L1); GO TO ABF;                                        00375000
ABC: END.                                                               00376000
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
