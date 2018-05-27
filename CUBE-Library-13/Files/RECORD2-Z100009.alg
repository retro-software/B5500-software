BEGIN                                                                   00001000
COMMENT BEGIN DOCUMENTATION:                                            00100000
PROGRAM-ID:   9TWX01R/RECORD                                                    
SYNOPSIS:     THIS PROGRAM ENABLES ONE TO USE THE REMOTE TO   0029TWX01R00102000
              DUMP PARTICULAR RECORDS OF DISK FILES(RANDOM OR 0039TWX01R00103000
              SERIAL) TO THE TELETYPE.  WHOLE RECORDS OR PARTS0099TWX01R00104000
              OF RECORDS ARE ACCESSIBLE.                      0059TWX01R00105000
DATE-WRITTEN: APRIL, 1969                                     0069TWX01R00106000
AUTHOR:       D.M. KUZELA, SOFTWARE SERVICES, CHASE BRASS,              00107000
PUBLISHER: N R KARL, SOFTWARE SERVICES, CHASE BRASS, CLEVELAND          00107500
PUBLICATION-DATE: AUGUST 27, 1969                                       00107600
LANGUAGE:     ALGOL                                           0089TWX01R00108000
INPUT:        INPUT IS ENTERED THRU THE REMOTE TERMINAL       0099TWX01R00109000
              AND FROM ACCESSED DISK FILES.                   0109TWX01R00110000
PROCESS:      THE PROGRAM IS EXECUTED FROM A REMOTE TERMINAL.                   
              A PRINTED LINE IS WRITTEN OUT TO THE TERMINAL:                    
              C ADDRESS MFID/FID RECORD-SIZE BLOCK-SIZE                         
              YOU MUST THEN TYPE IN THE REQUESTED ITEMS.                        
              THE ITEMS MUST APPEAR IN THE ABOVE ORDER AND                      
              EACH PAIR OF ITEMS MUST BE SEPARATED BY AT LEAST                  
              ONE BLANK.                                                        
              THE FIRST POSITION, C, MUST EITHER BE BLANK(IF                    
              ALL OF THE RECORD IS DESIRED) OR CONTAIN ANY                      
              CHARACTER(IF ONLY PART OF THE RECORD IS WANTED).                  
              IF "C" IS NOT LEFT BLANK, "BBBB TTTT" IS SENT                     
              TO THE REMOTE.  UNDER THE "B" S, THE POINT IN                     
              THE RECORD AT WHICH THE DUMP IS TO START IS                       
              ENTERED.  UNDER THE "T" S, THE NUMBER OF                          
              CHARACTERS TO BE DUMPED IS ENTERED.  ERROR                        
              MESSAGES ARE SENT IF THE ENTRIES UNDER B OR T                     
              CAUSE THE ACCESS TO FALL OUTSIDE THE RECORD.                      
              ALL MESSAGES TYPED IN ARE CONCLUDED WITH A "~".                   
              AFTER THE FIRST ACCESS, IT IS ASSUMED THE                         
              FOLLOWING ACCESSES ARE IN THE SAME FILE UNLESS                    
              A T*~ IS TYPED IN THE FIRST 3 POSITIONS AFTER                     
              GETTING A CUE TO ENTER A MESSAGE(A "*") OR BY                     
              GIVING A COMPLETE ENTRY OF THE NEW FILE S                         
              PARAMETERS AFTER GETTING THE CUE.                                 
              IF THE FOLLOWING ACCESS IS TO BE PART OF THE                      
              RECORD JUST PREVIOUSLY ACCESSED, A CHARACTER                      
              FOLLOWED BY A "~" IS TYPED.  IF THE ACCESS IS                     
              OF A DIFFERENT RECORD IN THE SAME FILE, THE                       
              FIRST POSITION IS EITHER FILLED OR BLANK,                         
              DEPENDING ON THE TYPE OF ACCESS DESIRED,                          
              FOLLOWED BY THE ADDRESS OF THE NEW RECORD.                        
              IF THE FILE BEING SOUGHT IS NOT ON DISK OR IS                     
              PRIVILEGED, AN APPROPRIATE MESSAGE IS SENT TO                     
              THE REMOTE.  IF A TYPING ERROR IS MADE, A "<"                     
              WILL REMOVE IT AND ALLOW YOU TO CONTINUE OR A                     
              "*~" WILL READY THE PROGRAM TO ACCEPT A                           
               COMPLETELY NEW MESSAGE.                                          
              IF THE FILE IS RANDOM, AN ADDRESS OF 1 WILL GET                   
              THE FIRST GOOD RECORD.  IF SERIAL, A 0 WILL BE                    
              NEEDED.                                                           
              "END~" IN THE FIRST 4 POSITIONS WILL END THE                      
              RUN.                                                              
          SUPPLEMENT TO CC0030                                                  
              IN TYPING IN THE INFORMATION REQUESTED BY THE                     
              PROGRAM:                                                          
              C ADDRESS MFID/FID RECORD-SIZE BLOCK-SIZE                         
              YOU MAY FOLLOW THE BLOCK-SIZE BY A SPACE AND THE                  
              LETTER "T" AND THE PROGRAM WILL BE IN TAPE                        
              ACCESS MODE.  ADDRESSES MAY BE TYPED IN AS WITH                   
              DISK FILES.  THESE "ADDRESSES" ARE RELATIVE TO                    
              THE FIRST RECORD WHICH IS RECORD ONE.  BY THE                     
              USE OF SPACE COMMANDS, A RANDOM TYPE OF ACCESS                    
              IS ACCOMPLISHED.                                                  
BEGIN FLOWCHART:                                                        00142000
LABEL  SEQBEG   SEQEND   DESCRIPTION                                    00143000
       #023000  #031000  GETDATACOM PROCEDURE(RETRIEVE BUFFER TERMINAL  00052000
                                             ADDRESS).                  00053000
01     #212000  #212000  SEND MESSAGE TO REMOTE( C ADDRESS MFID/FID     00054000
                                       RECORD-SIZE BLOCK-SIZE).         00055000
       #032000  #060000  REMOTEREAD2 PROCEDURE(READ MESSAGE TYPED AT    00056000
                                       REMOTE INTO ARRAY,AV).           00057000
       #061000  #107000  FREEFORM PROCEDURE(PACK FROM ARRAY AV INTO     00058000
                           ARRAY REMOTEREC.                             00059000
       #215000  #216000  CHECK FOR "*"(ERROR INDICATOR).                00060000
       #179000  #208000  NAMEFILL PROCEDURE(ASSIGNS VALUES TO           00061000
                                                 PARAMETERS).           00062000
02     #234000  #242000  FILL DISK FILE WITH ID AND PERFORM SEARCH.     00063000
03     #243000  #245000  READ RECORD.                                   00064000
       #013000  #022000  MANIPUL PROCEDURE(CHANGE NON-TRANSMITTABLE     00065000
                                                 CHARACTERS TO %).      00066000
       #248000  #248000  IF PARTIAL DUMP GO TO 04.                      00067000
       #248000  #252000  SEND RECORD OUT TO REMOTE.  GO TO 05.          00068000
& 04     #255000  #269000  CALL FOR STARTING LOCATION IN RECORD AND     00069000
                         FOR HOW MANY CHARACTERS TO BE DUMPED.          00070000
                         SEND PARTIAL OUT TO REMOTE.                    00071000
05     #271000  #291000  READ MESSAGE FROM REMOTE.  IF "T*" THEN CLOSE  00072000
                         CURRENT FILE AND GO TO 01.  IF "END" GO TO 06. 00073000
                         FREEFORM PROCEDURE.                            00074000
                         ERROR CHECK.. IF ERROR GO TO 05.               00075000
                         IF REMOTEREC[2]="%" THEN GO TO 04.             00076000
                         IF REMOTEREC[7]="%" THEN FILL ADDRESS AND GO TO00077000
                         03, ELSE CLOSE CURRENT FILE, NAMEFILL          00078000
                         PROCEDURE, AND GO TO 02.                       00079000
END FLOWCHART.                                                          00080000
BEGIN TERM DEFINITIONS HERE:                                            00081000
     COUNT = NUMBER OF UNTRANSMITTABLE CHARACTERS IN THE RECORD.        00082000
  BUFFSPEC = TERMINAL BUFFER ADDRESS.                                   00083000
   RECSIZE = RECORD SIZE IN CHARACTERS.                                 00084000
   RECRDSZ = RECORD SIZE IN WORDS.                                      00085000
   BLKSIZE = BLOCK SIZE IN WORDS.                                       00086000
      ADDR = ADDRESS OF RECORD BEING ACCESSED.                          00087000
      NUMB = PARTIAL ACCESS SWITCH.                                     00088000
         V = INITIAL POSITION OF PARTIAL ACCESS(WHERE ACCESS BEGINS).   00089000
         P = NUMBER OF CHARACTERS TO BE ACCESSED IN PARTIAL ACCESS.     00090000
         I = ARRAY POSITION POINTER.                                    00091000
         J = ARRAY POSITION POINTER.                                    00092000
END TERM DEFINITIONS.                                                   00093000
END DOCUMENTATION;                                                      00094000
LABEL LOOP2,LOOP3,LOOP6,LOOP4;                                                  
ALPHA FILE DATADEVICE 14(2,8) ; % 1 STATUS WORD + 78 CHAR CAPACITY      00003000
ALPHA FILE DATADEVICE1 14(2,8) ; % 1 STATUS WORD + 78 CHAR CAPACITY     00004000
INTEGER COUNT;                                                          00005000
INTEGER RECSIZE, BLKSIZE , BUFFSPEC ,ADDR,I,Q,NUMB,RECRDSZ;             00006000
ALPHA ARRAY AV[1:112];                                                  00007000
ALPHA ARRAY TTYREC[1:80],REMOTEREC[1:80], ID[1:2];                      00008000
ALPHA ARRAY DISKREC[1:1023   ] ;                                        00220000
ARRAY FILEPD[0:6];                                                      00221000
FORMAT MTNF(X8,"!{","OUTSIDE OF RECORD","!{~"),                         00222000
FBA(X8,"OUTSIDE OF FILE","~"),                                          00223000
FAB(X8,"{!*","~"),                                                              
RQF(X8,"PRIVILEGED FILE","!{~"), FQR(X8,"NOT ON DISK","!{~"),           00225000
FMT1(X8,"!{","BBBB TTTT","!{~"), RECFORM(*A1);                          00226000
INTEGER V,P;                                                            00227000
ALPHA ARRAY REMOREC[1:12];                                              00228000
INTEGER SAVSIZE,J,K,L;                                                          
FORMAT INIFIRM(X8,"{!",""","~"), INOFORM(X8,""","{!~"),                 00009000
       INIFORM(X8,"{!~") ,                                              00010000
QUESTFORM(X8,"{!","C ADDRESS MFID/FID RECORD-SIZE BLOCK-SIZE","{!~");   00011000
FORMAT FAA(X8,I3," CONTROL CHARACTERS APPEARED AND WERE CHANGED TO %"); 00012000
LIST RECLIST(RECSIZE,FOR I~1 STEP 1 UNTIL RECSIZE DO   DISKREC[I]);     00233000
PROCEDURE MANIPUL(DISKREC,COUNT,RECSIZE);                               00013000
ALPHA ARRAY DISKREC[1]; INTEGER COUNT,RECSIZE;                          00014000
BEGIN                                                                   00015000
INTEGER G;                                                              00016000
COUNT~0;                                                                00017000
FOR G~1 STEP 1 UNTIL RECSIZE DO BEGIN                                   00018000
IF DISKREC[G]="<" THEN BEGIN COUNT~COUNT+1; DISKREC[G]~"%"; END;        00019000
IF DISKREC[G]="}" THEN BEGIN COUNT~COUNT+1; DISKREC[G]~"%"; END;        00020000
IF DISKREC[G]="~" THEN BEGIN COUNT~COUNT+1; DISKREC[G]~"%"; END;        00021000
END; END;                                                               00022000
PROCEDURE GETDATACOM(DATADEVICE1,BUFFSPEC) ;                            00023000
FILE DATADEVICE1 ;                                                      00024000
INTEGER BUFFSPEC;                                                       00025000
BEGIN                                                                   00026000
ARRAY A[0:7] ;                                                          00027000
BUFFSPEC ~  STATUS(A[*]);                                               00028000
COMMENT THIS TECHNIQUE DERIVES FROM CUBE TIC-TAC-TOE PROGRAM ;          00029000
BUFFSPEC ~ A[0] ;                                                       00030000
END ; % GETDATACOM                                                      00031000
PROCEDURE REMOTEREAD2(DATADEVICE1,BUFFSPEC,REMOTEREC,REMSIZE);          00032000
COMMENT EXTERNAL TO PROCEDURE AS FIRST STEP IN PROGRAM ISSUE READ       00033000
SEEK AFTER GETDCOM;                                                     00034000
VALUE BUFFSPEC;                                                         00035000
INTEGER REMSIZE,BUFFSPEC;                                               00036000
ALPHA ARRAY REMOTEREC[1] ;                                              00037000
FILE DATADEVICE1;                                                       00038000
BEGIN                                                                   00039000
INTEGER  I,CHECK;                                                       00040000
FORMAT INFORM(X8,28A1);                                                 00041000
INTEGER ARRAY LIMITS[1:2] ;                                             00042000
LABEL AGAIN,READ2EXIT;                                                  00043000
LIST INLIST(FOR I~ LIMITS[1] STEP 1 UNTIL LIMITS[2] DO REMOTEREC[I]);   00044000
CHECK ~ 0 ;                                                             00045000
LIMITS[1]~1;                                                            00046000
IF REMSIZE<28 THEN LIMITS[2]~REMSIZE+1 ELSE LIMITS[2]~28;               00047000
READ SEEK(DATADEVICE1(BUFFSPEC));                                       00048000
AGAIN :                                                                 00049000
READ(DATADEVICE1(BUFFSPEC),INFORM,INLIST);                              00050000
IF STATUS(BUFFSPEC,0).[25:1]=0 THEN BEGIN                               00051000
REMOTEREC[I]~" "; GO TO READ2EXIT; END;                                 00052000
CHECK ~ REMSIZE - 28 ;                                                  00053000
IF CHECK > 28 THEN LIMITS[2] ~ 28 + LIMITS[2]                           00054000
ELSE LIMITS[2]~REMSIZE+1;                                               00055000
LIMITS[1] ~ LIMITS[1] + 28 ;                                            00056000
GO TO AGAIN;                                                            00057000
READ2EXIT :                                                             00058000
READ SEEK(DATADEVICE1(BUFFSPEC));                                       00059000
            END ; % REMOTEREAD2                                         00060000
PROCEDURE FREEFORM(AV,REMOTEREC);                                       00061000
ALPHA ARRAY AV,REMOTEREC[1];                                            00062000
BEGIN                                                                   00063000
INTEGER R,V,N,QTM,LIP,D,Z;                                              00064000
LABEL ABC,BCD,CDE,DEF,EFG;                                                      
FOR V~1 STEP 1 UNTIL 56 DO IF AV[V]="~" THEN BEGIN                      00066000
AV[V]~" "; AV[V+1]~"%"; V~56; END;                                      00067000
FOR V~ 2 STEP 1 UNTIL  6 DO REMOTEREC[V]~0;                             00068000
FOR V~21 STEP 1 UNTIL 27 DO REMOTEREC[V]~0;                             00069000
FOR V~7 STEP 1 UNTIL 20 DO REMOTEREC[V]~" ";                            00070000
REMOTEREC[1]~AV[1]; R~2;                                                00071000
FOR V~R STEP 1 UNTIL 56 DO IF AV[V]!" " THEN BEGIN                      00072000
IF AV[V]= "%" THEN BEGIN REMOTEREC[2]~"%"; GO TO EFG; END;              00073000
FOR N~V STEP 1 UNTIL 56 DO IF AV[N]= " " THEN BEGIN                     00074000
D~N-1;   QTM~1;                                                         00075000
FOR Z~D STEP -1 UNTIL V DO BEGIN LIP~7-QTM;                             00076000
REMOTEREC[LIP]~AV[Z]; QTM~QTM+1; END ;                                  00077000
R~N; GO TO BCD; END; END;                                               00078000
BCD:                                                                    00079000
FOR V~R STEP 1 UNTIL 56 DO IF AV[V]!" " THEN BEGIN                      00080000
IF AV[V]= "%" THEN BEGIN REMOTEREC[7]~"%"; GO TO EFG; END;              00081000
FOR N~V STEP 1 UNTIL 56 DO IF AV[N]= "/" THEN BEGIN                     00082000
D~N-1; QTM~1;                                                           00083000
FOR Z~V STEP 1 UNTIL D DO BEGIN   LIP~6+QTM;                            00084000
REMOTEREC[LIP]~AV[Z]; QTM~QTM+1; END;                                   00085000
FOR Z~N STEP 1 UNTIL 56 DO IF AV[Z] = " " THEN BEGIN                    00086000
D~Z-1; QTM~1;             N~N+1;                                        00087000
FOR Z~N STEP 1 UNTIL D DO BEGIN LIP~13+QTM;                             00088000
REMOTEREC[LIP]~AV[Z];                                                   00089000
QTM~QTM+1; END;                                                         00090000
R~D+1; GO TO CDE; END;                                                  00091000
END;                                                                    00092000
END;                                                                    00093000
CDE:                                                                    00094000
FOR Z~R STEP 1 UNTIL 56 DO IF AV[Z]!" " THEN BEGIN                      00095000
FOR N~Z STEP 1 UNTIL 56 DO IF AV[N]=" " THEN BEGIN                      00096000
D~N-1; QTM~1;                                                           00097000
FOR V~D STEP -1 UNTIL Z DO BEGIN  LIP~24-QTM;                           00098000
REMOTEREC[LIP]~AV[V]; QTM~QTM+1; END;                                   00099000
R~N; GO TO DEF; END; END;                                               00100000
DEF:                                                                    00101000
FOR V~R STEP 1 UNTIL 56 DO IF AV[V]!" " THEN BEGIN                      00102000
FOR Z~V STEP 1 UNTIL 56 DO IF AV[Z]=" " THEN BEGIN                      00103000
D~Z-1; QTM~1;                                                           00104000
FOR R~D STEP -1 UNTIL V DO BEGIN LIP~28-QTM;                            00105000
REMOTEREC[LIP]~AV[R]; QTM~QTM+1; END; GO TO ABC; END; END;              00106000
ABC:                                                                            
FOR V~D+1 STEP 1 UNTIL 56 DO IF AV[V]!" " THEN BEGIN                            
IF AV[V]="T" THEN REMOTEREC[28]~"T" ELSE REMOTEREC[28]~" ";                     
GO TO EFG; END;                                                                 
EFG: END;                                                                       
PROCEDURE REMOTEWRITE2(DATADEVICE,BUFFSPEC,REMOTEREC,REMSIZE) ;         00108000
COMMENT EXTERNAL TO PROCEDURE AS FIRST STEP IN PROGRAM ISSUE READ SEEK  00109000
AFTER GETDCOM;                                                          00110000
ALPHA ARRAY REMOTEREC[1] ;                                              00111000
INTEGER REMSIZE,BUFFSPEC;                                               00112000
FILE DATADEVICE;                                                        00113000
BEGIN                                                                   00114000
LABEL L1,L2,L3,L4,   L6,L7,L9,WRITE2EXIT,STEP1;                         00115000
INTEGER I,P,P1,Q,Z,SIZE,J,SIZ;                                          00116000
INTEGER ARRAY LIMITS[1:2] ;                                             00117000
LABEL STP;                                                              00118000
LABEL ONTOS;                                                            00119000
INTEGER ZP;                                                             00120000
FORMAT OUTFORM(X8,56A1);                                                00121000
ALPHA ARRAY WRITEREC[1:56],DELIMITER[1:7] ;                             00122000
LIST OUTLIST(FOR I~ LIMITS[1] STEP 1 UNTIL LIMITS[2] DO  WRITEREC[I]) ; 00123000
FILL DELIMITER[*] WITH 7,    % TOTAL OF 7 ENTRIES                       00124000
                       "{", % CR                                        00125000
                       "<", % RO                                        00126000
                       "!", % LF                                        00127000
                       ">",  % X-ON                                     00128000
                       "~", %END OF MESSAGE                             00129000
                       "}"; % EOT                                       00130000
Q~56;                                                                   00131000
ZP~1;                                                                   00132000
L1: SIZE~P1~1;                                                          00133000
LIMITS[1]~1;                                                            00134000
L2: FOR J~P1 STEP 1 UNTIL REMSIZE DO                                    00135000
BEGIN                                                                   00136000
FOR I~2 STEP 1 UNTIL DELIMITER[1] DO                                    00137000
IF REMOTEREC[J]=DELIMITER[I] THEN GO TO STP;                            00138000
P1~J;  GO TO STEP1;                                                     00139000
STP: END;                                                               00140000
LIMITS[2]~REMSIZE  ;                                                    00141000
WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);                            00142000
GO TO WRITE2EXIT;                                                       00143000
STEP1: IF REMOTEREC[J]="~" THEN BEGIN WRITEREC[SIZE]~"~";               00144000
LIMITS[2]~SIZE; WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);            00145000
GO TO WRITE2EXIT; END; IF J>REMSIZE THEN GO TO L3;                      00146000
WRITEREC[SIZE]~REMOTEREC[J];                                            00147000
IF SIZE}Q THEN GO TO L4;                                                00148000
IF ZP=1 THEN                                                            00149000
IF J-P1+1<71 THEN BEGIN SIZE~SIZE+1; J~J+1; GO TO STEP1; END            00150000
ELSE GO TO ONTOS;                                                       00151000
IF J-P1+1<72 THEN BEGIN SIZE~SIZE+1; J~J+1; GO TO STEP1; END;           00152000
ONTOS:                                                                  00153000
SIZE~SIZE+1; WRITEREC[SIZE]~"{";                                        00154000
SIZE~SIZE+1; IF SIZE>Q THEN GO TO L6 ELSE WRITEREC[SIZE]~"!";           00155000
IF SIZE}Q THEN BEGIN LIMITS[2]~SIZE ;                                   00156000
WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);                            00157000
SIZE~1 END ELSE SIZE~SIZE+1;                                            00158000
L9:                                                                     00159000
IF ZP=1 THEN BEGIN                                                      00160000
ZP~2;                                                                   00161000
P1~P1+71;     J~J+1; GO TO STEP1; END;                                  00162000
P1~P1+72; J~J+1; GO TO STEP1;                                           00163000
L3: LIMITS[2]~SIZE;                                                     00164000
    WRITEREC[SIZE]~"~"; WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);    00165000
GO TO WRITE2EXIT;                                                       00166000
L4: LIMITS[1]~1; LIMITS[2]~SIZE  ;                                      00167000
IF J-P1+1<72 THEN BEGIN WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);    00168000
SIZE~1;        END ELSE BEGIN                                           00169000
WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);                            00170000
SIZE~3; WRITEREC[  1 ]~"{"; WRITEREC[  2   ]~"!";                       00171000
P1~P1+72;END; J~J+1; GO TO STEP1;                                       00172000
L6: LIMITS[1]~1; LIMITS[2]~SIZE-1;                                      00173000
WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);                            00174000
SIZE~2; WRITEREC[SIZE]~"!";                                             00175000
GO TO L9;                                                               00176000
WRITE2EXIT:                                                             00177000
    END OF MULT;                                                        00178000
PROCEDURE NAMEFILL(REMOTEREC,NUMB,RECSIZE,RECRDSZ,BLKSIZE,ADDR,ID);     00179000
INTEGER NUMB,RECSIZE,RECRDSZ,BLKSIZE,ADDR;                              00180000
ALPHA ARRAY REMOTEREC,ID[1];                                            00181000
BEGIN                                                                   00182000
INTEGER EZ;                                                             00183000
FOR EZ~ 2 STEP 1 UNTIL  6 DO IF REMOTEREC[EZ]=" " THEN REMOTEREC[EZ]~0; 00184000
FOR EZ~21 STEP 1 UNTIL 23 DO IF REMOTEREC[EZ]=" " THEN REMOTEREC[EZ]~0; 00185000
FOR EZ~24 STEP 1 UNTIL 27 DO IF REMOTEREC[EZ]=" " THEN REMOTEREC[EZ]~0; 00186000
NUMB~REMOTEREC[1];                                                      00187000
ADDR ~  10000|REMOTEREC[ 2]+1000|REMOTEREC[ 3]+100|REMOTEREC[4 ]+       00188000
10|REMOTEREC[ 5]+REMOTEREC[ 6];                                         00189000
RECRDSZ~  100|REMOTEREC[21]+  10|REMOTEREC[22]+    REMOTEREC[23];       00190000
BLKSIZE~1000|REMOTEREC[24]+100|REMOTEREC[25]+10|REMOTEREC[26]+          00191000
REMOTEREC[27];                                                          00192000
RECSIZE~8|RECRDSZ;                                                      00193000
ID[ 1].[6:6]~REMOTEREC[ 7];                                             00194000
ID[1].[12:6]~REMOTEREC[ 8];                                             00195000
ID[1].[18:6]~REMOTEREC[ 9];                                             00196000
ID[1].[24:6]~REMOTEREC[10];                                             00197000
ID[1].[30:6]~REMOTEREC[11];                                             00198000
ID[1].[36:6]~REMOTEREC[12];                                             00199000
ID[1].[42:6]~REMOTEREC[13];                                             00200000
ID[2].[ 6:6]~REMOTEREC[14];                                             00201000
ID[2].[12:6]~REMOTEREC[15];                                             00202000
ID[2].[18:6]~REMOTEREC[16];                                             00203000
ID[2].[24:6]~REMOTEREC[17];                                             00204000
ID[2].[30:6]~REMOTEREC[18];                                             00205000
ID[2].[36:6]~REMOTEREC[19];                                             00206000
ID[2].[42:6]~REMOTEREC[20];                                             00207000
END;                                                                    00208000
GETDATACOM(DATADEVICE1,BUFFSPEC) ;                                      00209000
WRITE(DATADEVICE(BUFFSPEC),INIFORM) ;                                   00210000
LOOP3 :                                                                 00211000
WRITE(DATADEVICE(BUFFSPEC),QUESTFORM) ;                                 00212000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,    AV   ,56) ;                        00213000
FREEFORM(AV,REMOTEREC);                                                 00214000
FOR I ~ 1 STEP 1 UNTIL 28 DO                                            00215000
IF REMOTEREC[I] = "*" THEN GO TO LOOP3;                                 00216000
NAMEFILL(REMOTEREC,NUMB,RECSIZE,RECRDSZ,BLKSIZE,ADDR,ID);               00217000
LOOP2 :                                                                 00218000
IF REMOTEREC[28]="T" THEN GO TO LOOP4;                                          
        BEGIN COMMENT BLOCK 2 ;                                         00219000
LABEL LOOP1, FINEXIT,BLANKCHECK,ARUND,AGAIN;                            00230000
FILE DISKF1 DISK RANDOM (2,RECRDSZ,        BLKSIZE, SAVE 10);           00232000
FILL DISKF1 WITH ID[1], ID[2];                                          00234000
SEARCH(DISKF1,FILEPD[*]);                                               00235000
IF SIGN(FILEPD[0])=-1 AND SIGN(FILEPD[3])=-1 AND SIGN(FILEPD[4])=-1 AND 00236000
   SIGN(FILEPD[5])=-1 AND SIGN(FILEPD[6])=-1 THEN BEGIN                 00237000
WRITE(DATADEVICE(BUFFSPEC),FQR); GO TO ARUND; END;                      00238000
IF FILEPD[0]!0 THEN GO TO AGAIN;                                        00239000
FOR J~3 STEP 1 UNTIL 6 DO IF FILEPD[I]!0 THEN GO TO AGAIN;              00240000
WRITE(DATADEVICE(BUFFSPEC),RQF);                                        00241000
GO TO ARUND;                                                            00242000
AGAIN :                                                                 00243000
WRITE(DATADEVICE(BUFFSPEC),INIFORM) ;                                   00244000
READ(DISKF1[ADDR],RECFORM,RECLIST)[ARUND];                                      
          MANIPUL(DISKREC,COUNT,RECSIZE);                               00246000
IF COUNT!0 THEN WRITE(DATADEVICE(BUFFSPEC),FAA,COUNT);                  00247000
IF NUMB!" " THEN GO TO LOOP1;                                           00248000
WRITE(DATADEVICE(BUFFSPEC),INIFIRM);                                    00249000
REMOTEWRITE2(DATADEVICE,BUFFSPEC , DISKREC,RECSIZE);                    00250000
WRITE(DATADEVICE(BUFFSPEC),INOFORM);                                    00251000
GO TO ARUND;                                                            00252000
LOOP1:                                                                  00255000
WRITE(DATADEVICE(BUFFSPEC),FMT1);                                       00256000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,REMOREC,10);                           00257000
FOR I~1 STEP 1 UNTIL 10 DO IF REMOREC[I]="*" THEN GO TO LOOP1;          00258000
FOR I~1 STEP 1 UNTIL 9 DO IF REMOREC[I]=" " THEN REMOREC[I]~0;          00259000
V~1000|REMOREC[1]+100|REMOREC[2]+10|REMOREC[3]+REMOREC[4];              00260000
P~1000|REMOREC[6]+100|REMOREC[7]+10|REMOREC[8]+REMOREC[9];              00261000
IF V<1 THEN BEGIN WRITE(DATADEVICE(BUFFSPEC),MTNF); GO TO ARUND; END;   00262000
IF V+P-1>RECSIZE THEN                                                   00263000
            BEGIN WRITE(DATADEVICE(BUFFSPEC),MTNF); GO TO ARUND; END;   00264000
FOR I~V STEP 1 UNTIL V+P-1 DO TTYREC[I-V+1]~DISKREC[I];                 00265000
WRITE(DATADEVICE(BUFFSPEC),INIFORM) ;                                   00266000
WRITE(DATADEVICE(BUFFSPEC),INIFIRM);                                    00267000
REMOTEWRITE2(DATADEVICE,BUFFSPEC,TTYREC,P);                             00268000
WRITE(DATADEVICE(BUFFSPEC),INOFORM);                                    00269000
ARUND :                                                                 00270000
WRITE(DATADEVICE(BUFFSPEC),FAB);                                        00271000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,    AV   ,56) ;                        00272000
IF AV[1]="T" THEN IF AV[2]="*" THEN BEGIN CLOSE(DISKF1,RELEASE);        00273000
GO TO LOOP3; END;                                                       00274000
IF AV[1]!"E" THEN GO TO BLANKCHECK;                                     00275000
IF AV[2]!"N" THEN GO TO BLANKCHECK;                                     00276000
IF AV[3]!"D" THEN GO TO BLANKCHECK;                                     00277000
GO TO FINEXIT;                                                          00278000
BLANKCHECK :                                                            00279000
FREEFORM(AV,REMOTEREC);                                                 00280000
FOR I~1 STEP 1 UNTIL 28 DO                                              00281000
IF REMOTEREC[I] = "*" THEN GO TO ARUND;                                 00282000
IF REMOTEREC[2]="%" THEN GO TO LOOP1;                                   00283000
IF REMOTEREC[7]!"%" THEN BEGIN CLOSE(DISKF1,RELEASE);                   00284000
NAMEFILL(REMOTEREC,NUMB,RECSIZE,RECRDSZ,BLKSIZE,ADDR,ID);               00285000
GO TO LOOP2; END;                                                       00286000
NUMB~REMOTEREC[1];                                                      00287000
FOR  I~ 2 STEP 1 UNTIL  6 DO IF REMOTEREC[ I]=" " THEN REMOTEREC[ I]~0; 00288000
ADDR ~  10000|REMOTEREC[ 2]+1000|REMOTEREC[ 3]+100|REMOTEREC[4 ]+       00289000
10|REMOTEREC[ 5]+REMOTEREC[ 6];                                         00290000
GO TO AGAIN;                                                            00291000
FINEXIT :                                                               00292000
END ;                                                                   00293000
GO TO LOOP6;                                                                    
LOOP4:                                                                          
BEGIN                                                                   DTY00100
INTEGER SPAD,ADVR;                                                              
LABEL TAP5,TAP3,TAP2,TAP4,TAP6;                                                 
FILE IN TAPE 2(2,RECRDSZ,BLKSIZE,SAVE 10);                                      
FILL TAPE WITH ID[1],ID[2];                                                     
SPAD~ADDR-1;                                                                    
TAP5:                                                                           
ADVR~ADDR;                                                                      
SPACE(TAPE,SPAD)[TAP3];                                                         
WRITE(DATADEVICE(BUFFSPEC),INIFORM) ;                                   00171000
READ(TAPE,RECFORM,RECLIST)[TAP3];                                               
          MANIPUL(DISKREC,COUNT,RECSIZE);                                       
IF COUNT!0 THEN WRITE(DATADEVICE(BUFFSPEC),FAA,COUNT);                          
IF NUMB!" " THEN GO TO TAP2;                                                    
WRITE(DATADEVICE(BUFFSPEC),INIFIRM);                                    00174000
REMOTEWRITE2(DATADEVICE,BUFFSPEC , DISKREC,RECSIZE);                    00175000
WRITE(DATADEVICE(BUFFSPEC),INOFORM);                                    00176000
GO TO TAP3;                                                                     
TAP2:                                                                           
WRITE(DATADEVICE(BUFFSPEC),FMT1);                                       00179000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,REMOREC,10);                           00180000
FOR I~1 STEP 1 UNTIL 10 DO IF REMOREC[I]="*" THEN GO TO TAP2 ;          00181   
FOR I~1 STEP 1 UNTIL 9 DO IF REMOREC[I]=" " THEN REMOREC[I]~0;          00182000
V~1000|REMOREC[1]+100|REMOREC[2]+10|REMOREC[3]+REMOREC[4];              00183000
P~1000|REMOREC[6]+100|REMOREC[7]+10|REMOREC[8]+REMOREC[9];              00184000
IF V<1 THEN BEGIN WRITE(DATADEVICE(BUFFSPEC),MTNF); GO TO TAP3 ; END;           
IF V+P-1>RECSIZE THEN                                                           
            BEGIN WRITE(DATADEVICE(BUFFSPEC),MTNF); GO TO TAP3 ; END;           
FOR I~V STEP 1 UNTIL V+P-1 DO TTYREC[I-V+1]~DISKREC[I];                 00185000
WRITE(DATADEVICE(BUFFSPEC),INIFIRM);                                    00187000
REMOTEWRITE2(DATADEVICE,BUFFSPEC,TTYREC,P);                             00188000
WRITE(DATADEVICE(BUFFSPEC),INOFORM);                                    00189000
TAP3:                                                                           
WRITE(DATADEVICE(BUFFSPEC),FAB);                                                
REMOTEREAD2(DATADEVICE1,BUFFSPEC,    AV   ,56) ;                                
IF AV[1]="T" THEN IF AV[2]="*" THEN BEGIN CLOSE(TAPE  ,RELEASE);                
GO TO LOOP3; END;                                                               
IF AV[1]!"E" THEN GO TO TAP4;                                                   
IF AV[2]!"N" THEN GO TO TAP4;                                                   
IF AV[3]!"D" THEN GO TO TAP4;                                                   
GO TO TAP6;                                                                     
TAP4:                                                                           
FREEFORM(AV,REMOTEREC);                                                         
FOR I~1 STEP 1 UNTIL 28 DO                                                      
IF REMOTEREC[I] = "*" THEN GO TO TAP3;                                          
IF REMOTEREC[2]="%" THEN GO TO TAP2 ;                                           
IF REMOTEREC[7]!"%" THEN BEGIN CLOSE(TAPE  ,RELEASE);                   002     
NAMEFILL(REMOTEREC,NUMB,RECSIZE,RECRDSZ,BLKSIZE,ADDR,ID);               00202000
GO TO LOOP2; END;                                                               
NUMB~REMOTEREC[1];                                                      00204000
FOR  I~ 2 STEP 1 UNTIL  6 DO IF REMOTEREC[ I]=" " THEN REMOTEREC[ I]~0; 00205000
ADDR ~  10000|REMOTEREC[ 2]+1000|REMOTEREC[ 3]+100|REMOTEREC[4 ]+       00206000
10|REMOTEREC[ 5]+REMOTEREC[ 6];                                         00207000
SPAD~ADDR-ADVR-1;                                                               
GO TO TAP5;                                                                     
TAP6:                                                                           
END ;                                                                   00210000
LOOP6:                                                                          
END.                                                                            
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
