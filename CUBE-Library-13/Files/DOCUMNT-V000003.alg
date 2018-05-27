BEGIN                                                                   00001000
ALPHA FILE DATADEVICE  14(2,8);                                         00002000
ALPHA FILE DATADEVICE1 14(2,8);                                         00003000
ALPHA ARRAY B[1:56],A[1:1023],C[1:1023];                                00004000
ALPHA ARRAY  ID[1:2];                                                   00005000
INTEGER BUFFSPEC,SWC,P,I,V,X,Z,Y,RCDSIZE,BLKSIZE,SWT,ADR,ADDR,BADR,EADR,00006000
INCR,SWH;                                                               00007000
INTEGER RECORDZ;                                                        00008000
FORMAT Z1(X8,"{!*~"),F8(X8,"{!ILC*","~"),Z2(X8,"{!NCF*","~"),           00009000
Z3(*A1),Z4(X8,"{!REPACK*","~"),F10(X8,"{!~"),Z21(X8,"{!",I4,"**~"),     00010000
M1(X8,"{!FILE REMOVED","~"),M2(X8,"{!","OLD","~"),                      00011000
F7(X8," NLN*","~"),Z6(X8,""{!~"),F5(X8,"#",I4,"{!",""","~");            00012000
LABEL A1,A2,A3,A4,A6,EOJ,R1;                                            00013000
PROCEDURE REMOTEWRITE2(DATADEVICE,BUFFSPEC,REMOTEREC,REMSIZE) ;         00014000
COMMENT EXTERNAL TO PROCEDURE AS FIRST STEP IN PROGRAM ISSUE READ SEEK  00015000
AFTER GETDCOM;                                                          00016000
ALPHA ARRAY REMOTEREC[1] ;                                              00017000
INTEGER REMSIZE,BUFFSPEC;                                               00018000
FILE DATADEVICE;                                                        00019000
BEGIN                                                                   00020000
LABEL L1,L2,L3,L4,   L6,L7,L9,WRITE2EXIT,STEP1;                         00021000
INTEGER I,P,P1,Q,Z,SIZE,J,SIZ;                                          00022000
INTEGER ARRAY LIMITS[1:2] ;                                             00023000
LABEL STP;                                                              00024000
LABEL ONTOS;                                                            00025000
INTEGER ZP;                                                             00026000
FORMAT OUTFORM(X8,56A1);                                                00027000
ALPHA ARRAY WRITEREC[1:56],DELIMITER[1:7] ;                             00028000
LIST OUTLIST(FOR I~ LIMITS[1] STEP 1 UNTIL LIMITS[2] DO  WRITEREC[I]) ; 00029000
FILL DELIMITER[*] WITH 7,    % TOTAL OF 7 ENTRIES                       00030000
                       "{", % CR                                        00031000
                       "<", % RO                                        00032000
                       "!", % LF                                        00033000
                       ">",  % X-ON                                     00034000
                       "~", %END OF MESSAGE                             00035000
                       "}"; % EOT                                       00036000
Q~56;                                                                   00037000
ZP~1;                                                                   00038000
L1: SIZE~P1~1;                                                          00039000
LIMITS[1]~1;                                                            00040000
L2: FOR J~P1 STEP 1 UNTIL REMSIZE DO                                    00041000
BEGIN                                                                   00042000
FOR I~2 STEP 1 UNTIL DELIMITER[1] DO                                    00043000
IF REMOTEREC[J]=DELIMITER[I] THEN GO TO STP;                            00044000
P1~J;  GO TO STEP1;                                                     00045000
STP: END;                                                               00046000
LIMITS[2]~REMSIZE  ;                                                    00047000
WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);                            00048000
GO TO WRITE2EXIT;                                                       00049000
STEP1: IF REMOTEREC[J]="~" THEN BEGIN WRITEREC[SIZE]~"~";               00050000
LIMITS[2]~SIZE; WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);            00051000
GO TO WRITE2EXIT; END; IF J>REMSIZE THEN GO TO L3;                      00052000
WRITEREC[SIZE]~REMOTEREC[J];                                            00053000
IF SIZE}Q THEN GO TO L4;                                                00054000
IF ZP=1 THEN                                                            00055000
IF J-P1+1<71 THEN BEGIN SIZE~SIZE+1; J~J+1; GO TO STEP1; END            00056000
ELSE GO TO ONTOS;                                                       00057000
IF J-P1+1<72 THEN BEGIN SIZE~SIZE+1; J~J+1; GO TO STEP1; END;           00058000
ONTOS:                                                                  00059000
SIZE~SIZE+1; WRITEREC[SIZE]~"{";                                        00060000
SIZE~SIZE+1; IF SIZE>Q THEN GO TO L6 ELSE WRITEREC[SIZE]~"!";           00061000
IF SIZE}Q THEN BEGIN LIMITS[2]~SIZE ;                                   00062000
WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);                            00063000
SIZE~1 END ELSE SIZE~SIZE+1;                                            00064000
L9:                                                                     00065000
IF ZP=1 THEN BEGIN                                                      00066000
ZP~2;                                                                   00067000
P1~P1+71;     J~J+1; GO TO STEP1; END;                                  00068000
P1~P1+72; J~J+1; GO TO STEP1;                                           00069000
L3: LIMITS[2]~SIZE;                                                     00070000
    WRITEREC[SIZE]~"~"; WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);    00071000
GO TO WRITE2EXIT;                                                       00072000
L4: LIMITS[1]~1; LIMITS[2]~SIZE  ;                                      00073000
IF J-P1+1<72 THEN BEGIN WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);    00074000
SIZE~1;        END ELSE BEGIN                                           00075000
WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);                            00076000
SIZE~3; WRITEREC[  1 ]~"{"; WRITEREC[  2   ]~"!";                       00077000
P1~P1+72;END; J~J+1; GO TO STEP1;                                       00078000
L6: LIMITS[1]~1; LIMITS[2]~SIZE-1;                                      00079000
WRITE(DATADEVICE(BUFFSPEC),OUTFORM,OUTLIST);                            00080000
SIZE~2; WRITEREC[SIZE]~"!";                                             00081000
GO TO L9;                                                               00082000
WRITE2EXIT:                                                             00083000
    END OF MULT;                                                        00084000
PROCEDURE GETDATACOM(DATADEVICE1,BUFFSPEC) ;                            00085000
FILE DATADEVICE1 ;                                                      00086000
INTEGER BUFFSPEC;                                                       00087000
BEGIN                                                                   00088000
ARRAY A[0:7] ;                                                          00089000
BUFFSPEC ~  STATUS(A[*]);                                               00090000
COMMENT THIS TECHNIQUE DERIVES FROM CUBE TIC-TAC-TOE PROGRAM ;          00091000
BUFFSPEC ~ A[0] ;                                                       00092000
END ; % GETDATACOM                                                      00093000
PROCEDURE REMOTEREAD2(DATADEVICE1,BUFFSPEC,REMOTEREC,REMSIZE);          00094000
COMMENT EXTERNAL TO PROCEDURE AS FIRST STEP IN PROGRAM ISSUE READ       00095000
SEEK AFTER GETDCOM;                                                     00096000
VALUE BUFFSPEC;                                                         00097000
INTEGER REMSIZE,BUFFSPEC;                                               00098000
ALPHA ARRAY REMOTEREC[1] ;                                              00099000
FILE DATADEVICE1;                                                       00100000
BEGIN                                                                   00101000
INTEGER  I,CHECK;                                                       00102000
FORMAT INFORM(X8,28A1);                                                 00103000
INTEGER ARRAY LIMITS[1:2] ;                                             00104000
LABEL AGAIN,READ2EXIT;                                                  00105000
LIST INLIST(FOR I~ LIMITS[1] STEP 1 UNTIL LIMITS[2] DO REMOTEREC[I]);   00106000
CHECK ~ 0 ;                                                             00107000
LIMITS[1]~1;                                                            00108000
IF REMSIZE<28 THEN LIMITS[2]~REMSIZE+1 ELSE LIMITS[2]~28;               00109000
READ SEEK(DATADEVICE1(BUFFSPEC));                                       00110000
AGAIN :                                                                 00111000
READ(DATADEVICE1(BUFFSPEC),INFORM,INLIST);                              00112000
IF STATUS(BUFFSPEC,0).[25:1]=0 THEN BEGIN                               00113000
REMOTEREC[I]~" "; GO TO READ2EXIT; END;                                 00114000
CHECK ~ REMSIZE - 28 ;                                                  00115000
IF CHECK > 28 THEN LIMITS[2] ~ 28 + LIMITS[2]                           00116000
ELSE LIMITS[2]~REMSIZE+1;                                               00117000
LIMITS[1] ~ LIMITS[1] + 28 ;                                            00118000
GO TO AGAIN;                                                            00119000
READ2EXIT :                                                             00120000
READ SEEK(DATADEVICE1(BUFFSPEC));                                       00121000
            END ; % REMOTEREAD2                                         00122000
GETDATACOM(DATADEVICE,BUFFSPEC);                                        00123000
SWH~1;                                                                  00124000
A3:                                                                     00125000
WRITE(DATADEVICE(BUFFSPEC),Z1);                                         00126000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,B,56);                                 00127000
IF B[1]!"S" THEN                                                        00128000
IF B[1]="E" THEN GO TO EOJ ELSE BEGIN                                   00129000
WRITE(DATADEVICE(BUFFSPEC),F8); GO TO A3; END;                          00130000
A6:                                                                     00131000
SWC~0;                                                                  00132000
FOR I~2 STEP 1 UNTIL 56 DO IF B[I]="/" THEN BEGIN                       00133000
P~I-2; X~1; Z~2;                                                        00134000
ID[1]~ID[2]~"       ";                                                  00135000
A1:                                                                     00136000
                   ID[X].[ 6:6]~B[Z]; Z~Z+1; P~P-1;                     00137000
IF P=0 THEN GO TO R1;                                                   00138000
                   ID[X].[12:6]~B[Z]; Z~Z+1; P~P-1;                     00139000
IF P=0 THEN GO TO R1;                                                   00140000
                   ID[X].[18:6]~B[Z]; Z~Z+1; P~P-1;                     00141000
IF P=0 THEN GO TO R1;                                                   00142000
                   ID[X].[24:6]~B[Z]; Z~Z+1; P~P-1;                     00143000
IF P=0 THEN GO TO R1;                                                   00144000
                   ID[X].[30:6]~B[Z]; Z~Z+1; P~P-1;                     00145000
IF P=0 THEN GO TO R1;                                                   00146000
                   ID[X].[36:6]~B[Z]; Z~Z+1; P~P-1;                     00147000
IF P=0 THEN GO TO R1;                                                   00148000
                   ID[X].[42:6]~B[Z]; Z~Z+1; P~P-1;                     00149000
R1:                                                                     00150000
IF X=2 THEN GO TO A2;                                                   00151000
FOR Y~I+1 STEP 1 UNTIL 56 DO IF B[Y]="*" THEN BEGIN                     00152000
P~Y-I-1; Z~I+1; X~2; GO TO A1; END;                                     00153000
WRITE(DATADEVICE(BUFFSPEC),F8); GO TO A3;                               00154000
END; WRITE(DATADEVICE(BUFFSPEC),F8); GO TO A3;                          00155000
A2:                                                                     00156000
IF B[Y+1]="P" THEN BEGIN SWT~1; GO TO A4;END;                           00157000
SWT~3;                                                                  00158000
A4:                                                                     00159000
BEGIN                                                                   00160000
FILE IN DISC DISK RANDOM (2,30,30,SAVE 99); ARRAY FILCK[0:6];           00161000
LABEL A5;                                                               00162000
FILL DISC WITH ID[1],ID[2];                                             00163000
SEARCH(DISC,FILCK[*]);                                                  00164000
IF SIGN(FILCK[0])=-1 AND SIGN(FILCK[3])=-1 AND SIGN(FILCK[4])=-1 AND    00165000
   SIGN(FILCK[5])=-1 AND SIGN(FILCK[6])=-1 THEN BEGIN                   00166000
WRITE(DATADEVICE(BUFFSPEC),Z2);                                         00167000
GO TO A3; END;                                                          00168000
A5:                                                                     00169000
RCDSIZE~FILCK[3];                                                       00170000
BLKSIZE~FILCK[4];                                                       00171000
RECORDZ~RCDSIZE|8;                                                      00172000
CLOSE(DISC,RELEASE);                                                    00173000
END;                                                                    00174000
BEGIN                                                                   00175000
FILE TISK DISK RANDOM (2,RCDSIZE,BLKSIZE,SAVE 99);                      00176000
FILE OUT DISH DISK RANDOM [20:200] (2,RCDSIZE,BLKSIZE,SAVE 99);         00177000
LABEL A7,E3,A9,C5,C1,C2,D5,D7,E1,D9,D8,A8,D3,B3,D2,B9,B1,D1,B7,F1,F2,F3,00178000
F4,F6,B90;                                                              00179000
LIST L1(RECORDZ,FOR I~1 STEP 1 UNTIL RECORDZ DO A[I]);                  00180000
LIST L2(RECORDZ,FOR I~1 STEP 1 UNTIL RECORDZ DO C[I]);                  00181000
FILL TISK WITH ID[1],ID[2];                                             00182000
IF SWT=1 THEN BEGIN ADR~1;                                              00183000
READ(TISK[ADR],Z3,L1)[A7];                                              00184000
WRITE(DATADEVICE(BUFFSPEC),M1);                                         00185000
A7: CLOSE(TISK,PURGE); GO TO A3; END;                                   00186000
E3:                                                                     00187000
WRITE(DATADEVICE(BUFFSPEC),Z1);                                         00188000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,B,56);                                 00189000
IF B[1]!"D" THEN GO TO C5;                                              00190000
A9:                                                                     00191000
FOR I~1 STEP 1 UNTIL 56 DO IF B[I]="," THEN BEGIN X~I; GO TO C1; END;   00192000
X~56;                                                                   00193000
C1:                                                                     00194000
FOR I~1 STEP 1 UNTIL 56 DO IF B[I]="~" THEN BEGIN Y~I; GO TO C2; END;   00195000
Y~56;                                                                   00196000
C2:                                                                     00197000
IF Y>X THEN BEGIN                                                       00198000
P~X-2; V~0; Z~1;                                                        00199000
FOR I~X-1 STEP -1 UNTIL 2 DO BEGIN V~V+B[I]|Z; Z~10|Z; END;             00200000
BADR~V; V~0; Z~1;                                                       00201000
FOR I~Y-1 STEP -1 UNTIL X+1 DO BEGIN V~V+B[I]|Z; Z~Z|10; END;           00202000
EADR~V; INCR~V;                                                         00203000
END ELSE BEGIN  V~0; Z~1;                                               00204000
FOR I~Y-1 STEP -1 UNTIL 2 DO BEGIN V~B[I]|Z+V; Z~Z|10;                  00205000
END;                                                                    00206000
BADR~EADR~V; INCR~0; END;                                               00207000
GO TO A8;                                                               00208000
C5:                                                                     00209000
IF B[1]="P" THEN GO TO A9;                                              00210000
IF B[1]="I" THEN GO TO A9;                                              00211000
IF B[1]="S" THEN GO TO D5;                                              00212000
IF B[1]!"E" THEN BEGIN                                                  00213000
WRITE(DATADEVICE(BUFFSPEC),F8); GO TO E3; END;                          00214000
SWH~0;                                                                  00215000
D5:                                                                     00216000
IF SWC=1 THEN GO TO D7;                                                 00217000
CLOSE(TISK,RELEASE);                                                    00218000
IF SWH=0 THEN GO TO EOJ;                                                00219000
GO TO A6;                                                               00220000
D7:                                                                     00221000
FILL DISH WITH ID[1],ID[2];                                             00222000
ADR~ADDR~0;                                                             00223000
E1:                                                                     00224000
READ(TISK[ADR],Z3,L1)[D8];                                              00225000
FOR I~1 STEP 1 UNTIL 8  DO IF A[I]!"%" THEN GO TO D9;                   00226000
ADR~ADR+1; GO TO E1;                                                    00227000
D9:                                                                     00228000
WRITE(DISH[ADDR],Z3,L1); ADR~ADR+1; ADDR~ADDR+1; GO TO E1;              00229000
D8:                                                                     00230000
WRITE(DATADEVICE(BUFFSPEC),M2);                                         00231000
CLOSE(TISK,PURGE); LOCK(DISH,SAVE);                                     00232000
SWC~0;                                                                  00233000
IF SWH=0 THEN GO TO EOJ;                                                00234000
GO TO A6;                                                               00235000
A8:                                                                     00236000
IF B[1]="P" THEN GO TO B1;                                              00237000
IF B[1]="I" THEN GO TO B3;                                              00238000
WRITE(DATADEVICE(BUFFSPEC),Z4);                                         00239000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,C,8);                                  00240000
IF C[1]="Y" THEN BEGIN SWC~1; FOR I~1 STEP 1 UNTIL 8  DO A[I]~"%"; END  00241000
ELSE BEGIN SWC~0; FOR I~1 STEP 1 UNTIL  RECORDZ DO A[I]~" "; END;       00242000
FOR ADR~BADR STEP 1 UNTIL EADR DO WRITE(TISK[ADR],Z3,L1)[D3];           00243000
GO TO E3;                                                               00244000
D3: WRITE(DATADEVICE(BUFFSPEC),F7);  GO TO E3;                          00245000
B1:                                                                     00246000
WRITE(DATADEVICE(BUFFSPEC),F10);                                        00247000
FOR ADR~BADR STEP 1 UNTIL EADR DO BEGIN                                 00248000
WRITE(DATADEVICE(BUFFSPEC),F5,ADR)     ;                                00249000
READ(TISK[ADR],Z3,L1)[D1];                                              00250000
REMOTEWRITE2(DATADEVICE,BUFFSPEC,A,RECORDZ);                            00251000
WRITE(DATADEVICE(BUFFSPEC),Z6);                                         00252000
END;                                                                    00253000
GO TO E3;                                                               00254000
D1: WRITE(DATADEVICE(BUFFSPEC),F7);                                     00255000
GO TO E3;                                                               00256000
B3:                                                                     00257000
ADR~BADR;                                                               00258000
D2:                                                                     00259000
WRITE(DATADEVICE(BUFFSPEC),Z21,ADR);                                    00260000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,A,RECORDZ);                            00261000
IF A[1]!"%" THEN GO TO B9;                                              00262000
IF A[2]!"*" THEN GO TO B9;                                              00263000
IF A[3]="~" THEN GO TO E3;                                              00264000
B9:                                                                     00265000
FOR I~1 STEP 1 UNTIL RECORDZ DO IF A[I]="~" THEN BEGIN                  00266000
FOR Y~I STEP 1 UNTIL RECORDZ DO A[Y]~" "; GO TO B90;END;                00267000
B90:                                                                    00268000
WRITE(TISK[ADR],Z3,L1)[B7];                                             00269000
IF INCR=0 THEN GO TO E3;                                                00270000
ADR~ADR+INCR; GO TO D2;                                                 00271000
B7:                                                                     00272000
FILL DISH WITH ID[1],ID[2]; ADDR~1;                                     00273000
F1:                                                                     00274000
READ(TISK[ADDR],Z3,L2)[F2];                                             00275000
WRITE(DISH[ADDR],Z3,L2);                                                00276000
ADDR~ADDR+1; GO TO F1;                                                  00277000
F2:  CLOSE(TISK,PURGE);                                                 00278000
GO TO F3;                                                               00279000
F4:                                                                     00280000
WRITE(DATADEVICE(BUFFSPEC),Z1);                                         00281000
REMOTEREAD2(DATADEVICE1,BUFFSPEC,A,RECORDZ);                            00282000
IF A[1]!"%" THEN GO TO F3;                                              00283000
IF A[2]!"*" THEN GO TO F3;                                              00284000
IF A[3]="~" THEN GO TO F6;                                              00285000
F3:                                                                     00286000
WRITE(DISH[ADR],Z3,L1);                                                 00287000
IF INCR=0 THEN GO TO F6;                                                00288000
ADR~ADR+INCR;  GO TO F4;                                                00289000
F6: LOCK(DISH,SAVE); GO TO E3;                                          00290000
END;                                                                    00291000
EOJ:                                                                    00292000
END.                                                                    00293000
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
