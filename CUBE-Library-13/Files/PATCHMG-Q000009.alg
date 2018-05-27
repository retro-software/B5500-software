$ + PRT STREAM                                                          00000000
BEGIN % PATCHES/MERGER......WRL.......L.P.MCQUOWN......                 00001000
    INTEGER I,W,OBJ;                                                    00002000
    BOOLEAN VOID,Q; % S MUST FOLLOW Q                                   00003000
    ALPHA S,PATCHNUMBER,LASTSEQ;                                        00004000
    FILE IN PATCH(2,10,150);                                            00005000
    FILE OUT DECK DISK SERIAL[20:OBJ~IF I MOD 100000=0 THEN 150 ELSE    00006000
      (I MOD 100000+299) DIV 300|15](2,10,150,SAVE 780);                00007000
    FILE OUT PRINTER 6(2,15);                                           00008000
    FILE IN CARD(2,10,30);                                              00009000
    INTEGER PATCHCOUNT, CARDCOUNT;                                      00010000
    SAVE ARRAY B, C[0:14];                                              00011000
COMMENT FILEPARAMETERS;                                                 00012000
PROCEDURE FILEPARAMETERS(FN,A);FILE FN;ARRAY A[0]; BEGIN REAL II;SAVE   00013000
ARRAY FPB[0:10];ARRAY T[0:1];INTEGER STREAM PROCEDURE LENGTH(A);BEGIN   00014000
SI~LOC A;DI~LOC LENGTH;SI~SI+1;DI~DI+6;DS~NUM;DS~CHR;END;REAL STREAM    00015000
PROCEDURE FNUM(F);BEGIN SI~F;SI~SI-24;DI~LOC F;DS~WDS;SI~F;SI~SI+34;DI~ 00016000
LOC FNUM;DI~DI+6;DS~2 CHR;DI~DI-2;DS~RESET;END;PROCEDURE FPBDESC(JUNK,  00017000
II,T);REAL JUNK,II;ARRAY T[0];BEGIN STREAM PROCEDURE F(JUNK,II,T);BEGIN 00018000
SI~II;SI~SI+8;DI~T;DS~WDS;SI~JUNK;14(SI~SI-8);DI~II;DI~DI+8;DS~WDS;END; 00019000
F(JUNK,II,T[0]);END;STREAM PROCEDURE RESTORE(II,T);BEGIN SI~T;DI~II;DI~ 00020000
DI+8;DS~WDS;END;STREAM PROCEDURE MOVE(A,B,N);VALUE N;BEGIN SI~A;DI~B;DS 00021000
~N WDS END;IF(II~LENGTH(A))>5 THEN II~5;FPBDESC(II+1,II,T);MOVE(FPB[    00022000
FNUM(FN)],A[0],II);RESTORE(II,T[0]);END;                                00023000
PROCEDURE TIMEIT(A);FILE A; BEGIN OWN BOOLEAN B;INTEGER E,F,G,H,I;      00024000
STREAM PROCEDURE C(A,B,D);VALUE A;BEGIN SI~LOC A;SI~SI+3;DI~B;DS~2 OCT; 00025000
DI~D;DS~3 OCT END;STREAM PROCEDURE D(A,B,C,E,F,G,H,I,J,K);VALUE A,B,C,E 00026000
,F,G,H,I,J;BEGIN LABEL L,M,N,O,P,Q;DI~K;DS~8 LIT" ";SI~K;DS~14 WDS;DI~K 00027000
;SI~LOC A;DS~2 DEC;2(DS~LIT":";DS~2 DEC);DI~DI+1;CI~CI+E;DS~3 LIT"SUN"; 00028000
GO L;SKIP SB;DS~3 LIT"MON";GO L;SKIP SB;DS~4 LIT"TUES";GO L;SKIP SB;DS~ 00029000
6 LIT"WEDNES";GO L;DS~5 LIT"THURS";GO L;DS~3 LIT"FRI";GO L;SKIP SB;DS~5 00030000
LIT"SATUR";L:DS~4 LIT"DAY,";DI~DI+1;CI~CI+F;DS~3 LIT"JAN";GO M;SKIP SB; 00031000
DS~4 LIT"FEBR";GO M;SKIP SB;DS~5 LIT"MARCH";GO O;DS~5 LIT"APRIL";GO O;  00032000
DS~3 LIT"MAY";GO O;SKIP SB;DS~4 LIT"JUNE";GO O;SKIP SB;DS~4 LIT"JULY";  00033000
GO O;SKIP SB;DS~6 LIT"AUGUST";GO O;DS~6 LIT"SEPTEM";GO N;DS~4 LIT"OCTO" 00034000
;GO N;SKIP SB;DS~5 LIT"NOVEM";GO N;DS~5 LIT"DECEM";GO N;M:DS~4 LIT      00035000
"UARY";GO O;N:DS~3 LIT"BER";O:DI~DI+1;A~DI;DI~LOC B;SI~LOC G;DS~2 DEC;  00036000
DI~A;SI~LOC B;IF SC="0"THEN SI~SI+1 ELSE DS~CHR;DS~CHR;DS~4 LIT", 19";  00037000
SI~LOC H;DS~2 DEC;IF SC!"+"THEN BEGIN DI~DI+4;DS~16 LIT                 00038000
"PROCESSOR TIME =";DI~DI+1;A~CI;GO P;DI~DI+4;DS~10 LIT"I/O TIME =";DI~  00039000
DI+1;SI~LOC J;A~CI;GO P;GO Q;P:B~DI;DI~LOC C;DS~8 DEC;SI~LOC C;DI~B;5(  00040000
IF TOGGLE THEN IF SC="0"THEN SI~SI+1 ELSE DS~CHR ELSE DS~CHR);DS~CHR;DS 00041000
~LIT".";DS~2 CHR;DS~8 LIT" SECONDS";CI~A;Q:END END;ARRAY X[0:14];C(TIME 00042000
(0),E,F);G~((((E-1)DIV 4)+E+F)MOD 7)|5;IF F<60 THEN F~F-1 ELSE IF E MOD 00043000
4=0 THEN F~F-1;IF F>212 THEN F~(F+31)DIV 61+F ELSE IF F>59 THEN F~(F+62 00044000
)DIV 61+F;H~TIME(1)|.016667;D(H DIV 3600,I~H DIV 60 MOD 60,I~H MOD 60,G 00045000
,(F DIV 31)|5,H~F MOD 31+1,E,IF B THEN H~TIME(2)|1.6667 ELSE-1,IF B     00046000
THEN H~TIME(3)|1.6667 ELSE-1,X[0]);B~TRUE;WRITE(A[DBL],15,X[*])END;     00047000
BOOLEAN PROCEDURE FINDPATCH;                                            00048000
BEGIN BOOLEAN Q;                                                        00049000
    STREAM PROCEDURE N(A,P,M,F,C,W); VALUE W;                           00050000
    BEGIN LABEL L1,L2,L3,L4,L5,LT;                                      00051000
        SI~A; SI~SI+5; DI~A; DS~3 CHR;                                  00052000
        SI~C; DI~P;                                                     00053000
        DI~ DI+1; DS~7 LIT " "; DI~DI-7;                                00054000
L1:     IF SC=" " THEN BEGIN SI~SI+1; GO L1 END;                        00055000
        IF SC=""" THEN BEGIN SI~SI+1; DI~DI-1; DS~LIT "+"; GO LT END;   00056000
        W(IF SC="=" THEN JUMP OUT;DS~ CHR);                             00057000
LT:     DI~M; DS~8 LIT "0       ";                                      00058000
L2:     IF SC!"=" THEN BEGIN SI~SI+1; GO L2 END;                        00059000
        SI~SI+1; DI~DI-7;                                               00060000
L3:     IF SC=" " THEN BEGIN SI~SI+1; GO L3 END;                        00061000
        7(IF SC="/" THEN JUMP OUT; DS~ CHR);                            00062000
L4:     IF SC!"/" THEN BEGIN SI~SI+1; GO L4 END;                        00063000
        SI~SI+1;                                                        00064000
L5:     IF SC=" " THEN BEGIN SI~SI+1; GO L5 END;                        00065000
        DI~F; DS~8 LIT "0       "; DI~DI-7;                             00066000
        7(IF SC>"9" THEN JUMP OUT;DS~ CHR);                             00067000
    END;                                                                00068000
STREAM PROCEDURE LISTIT(A,PNO,SORC,MF,F,FNO,CW,W);                      00069000
VALUE PNO,SORC,MF,F,FNO,CW,W; BEGIN                                     00070000
    DI~A; DS~8 LIT " "; SI~A; DS~14 WDS;                                00071000
    DI~A; SI~LOC PNO; SI~SI+1; DS~W CHR;                                00072000
    DS~6 LIT " FROM "; SI~SI+CW; SI~SI+3; DS~4 CHR;                     00073000
    DS~4 LIT " IS "; SI~SI+1; DS~7 CHR; DS~LIT "/";                     00074000
    SI~SI+1; DS~7 CHR; DS~7 LIT ", FILE "; DS~3 DEC END;                00075000
                                                                        00076000
    LABEL E,L,M;                                                        00077000
    ALPHA MF,F;                                                         00078000
    ARRAY A[0:10];                                                      00079000
    CLOSE(PATCH);                                                       00080000
    IF PATCHCOUNT = 0 THEN BEGIN                                        00081000
        FILEPARAMETERS(PATCH,A[*]);                                     00082000
        IF A[0]!0 OR A[1]!"PATCH  " THEN BEGIN                          00083000
            IF A[0]=0 THEN I~2 ELSE I~12;                               00084000
            W.[2:1] ~ 1; MF ~ A[0]; F ~ A[1];                           00085000
            FILEPARAMETERS(CARD,A[*]);                                  00086000
            IF PATCHNUMBER~A[1] = "CARD   " THEN                        00087000
                PATCHNUMBER ~ -"       ";                               00088000
            GO M;                                                       00089000
        END;                                                            00090000
    END;                                                                00091000
L:  READ(CARD,10,A[*])[E];                                              00092000
    A[10]~ "=/0"+12;                                                    00093000
    PATCHNUMBER~ 0;                                                     00094000
    N(A[10],PATCHNUMBER,MF,F,A,W);                                      00095000
    IF PATCHNUMBER="       " THEN                                       00096000
        GO TO L;                                                        00097000
    IF Q~ F="       " THEN                                              00098000
        BEGIN                                                           00099000
            F~ MF;                                                      00100000
            I~ 2;                                                       00101000
            MF~ 0                                                       00102000
        END                                                             00103000
    ELSE                                                                00104000
        I~ 12;                                                          00105000
M:  FILL PATCH WITH MF, F,*,*,*,I;                                      00106000
    WRITE(PRINTER);                                                     00107000
    LISTIT(B,PATCHNUMBER,IF I=2 THEN "TAPE" ELSE "DISK",MF,F,           00108000
    PATCHCOUNT~PATCHCOUNT+1,8-W,W); WRITE(PRINTER,15,B[*]);             00109000
    CARDCOUNT~ -1;                                                      00110000
    FINDPATCH~ TRUE;                                                    00111000
E:                                                                      00112000
    LASTSEQ ~ -1;                                                       00113000
END;                                                                    00114000
BOOLEAN PROCEDURE INPUT(A);                                             00115000
ARRAY A[0];                                                             00116000
BEGIN                                                                   00117000
    LABEL L,E,T;                                                        00118000
    REAL I;                                                             00119000
    INTEGER STREAM PROCEDURE LINK(A,LNK);VALUE LNK;BEGIN LABEL L1,L2,L3;00120000
    DI~LOC LNK; DI~DI+3; SI~A;                                          00121000
    IF SC=DC THEN BEGIN                                                 00122000
L1:     IF SC=" " THEN BEGIN SI~SI+1; GO L1 END;                        00123000
        IF 4 SC=DC THEN BEGIN                                           00124000
L2:         IF SC=" " THEN BEGIN SI~SI+1; GO L2 END;                    00125000
            IF SC="+" THEN BEGIN TALLY~16; SI~SI+1 END; A~TALLY;        00126000
L3:         IF SC=" " THEN BEGIN SI~SI+1; GO L3 END; TALLY~0;           00127000
            8(IF SC}"0" THEN BEGIN SI~SI+1; TALLY~TALLY+1 END           00128000
                ELSE JUMP OUT); DI~LOC LINK;                            00129000
            LNK~TALLY; SI~SI-LNK; DS~LNK OCT;                           00130000
            DI~DI-8; SI~LOC LNK; SI~SI-1; DS~CHR;                       00131000
    END END END;                                                        00132000
    BOOLEAN STREAM PROCEDURE EQUAL(A,B,C); VALUE C; BEGIN SI~B; DI~A;   00133000
        IF 8 SC=DC THEN BEGIN TALLY~1; EQUAL~TALLY; SI~LOC C; DS~8 DEC; 00134000
        DI~DI-8; DS~7 FILL END; SI~A; DI~B; DS~WDS; END;                00135000
L:  READ(PATCH,10,A[*])[E]; CARDCOUNT ~ CARDCOUNT + 1;                  00136000
    IF I~LINK(A,"LINK"&12[18:42:6])!0 THEN BEGIN                        00137000
        IF I<0 THEN I~CARDCOUNT-I;                                      00138000
        READ SEEK(PATCH[I]);                                            00139000
        CARDCOUNT ~ I-1; GO L;                                          00140000
    END ELSE                                                            00141000
    IF EQUAL(A[9],LASTSEQ,CARDCOUNT) THEN BEGIN                         00142000
        WRITE(PRINTER,11,A[*]);                                         00143000
        GO TO L;                                                        00144000
    END;                                                                00145000
    A[10]~PATCHNUMBER; A[11]~PATCHCOUNT&CARDCOUNT                       00146000
      [18:33:15];  GO TO T;                                             00147000
E:  IF NOT BOOLEAN(W).[2:1] THEN                                        00148000
        IF FINDPATCH THEN                                               00149000
            GO TO L;                                                    00150000
    INPUT~ TRUE;                                                        00151000
    WRITE(PRINTER,</"THERE ARE "I4" PATCHES"/>,PATCHCOUNT);             00152000
    TIMEIT(PRINTER);                                                    00153000
    WRITE(PRINTER[PAGE]);                                               00154000
T: END;                                                                 00155000
BOOLEAN PROCEDURE COMPARE(A, B);                                        00156000
ARRAY A, B[0];                                                          00157000
BEGIN                                                                   00158000
    BOOLEAN STREAM PROCEDURE COM(A, B);                                 00159000
    BEGIN                                                               00160000
        SI~ A;                                                          00161000
        DI~ B;                                                          00162000
        IF 8 SC<DC THEN                                                 00163000
            TALLY~ 1                                                    00164000
        ELSE                                                            00165000
            BEGIN                                                       00166000
                SI~ SI-8;                                               00167000
                DI~ DI-8;                                               00168000
                IF 8 SC=DC THEN                                         00169000
                    TALLY~ 2                                            00170000
            END;                                                        00171000
        COM~ TALLY;                                                     00172000
    END;                                                                00173000
    INTEGER I;                                                          00174000
    IF(COMPARE~ COM(A[9], B[9])).[46:1] THEN                            00175000
      IF A[11].[33:15] = B[11].[33:15] THEN                             00176000
        COMPARE ~ A[11].[18:15] > B[11].[18:15]                         00177000
      ELSE                                                              00178000
        COMPARE~ A[11].[33:15]>B[11].[33:15]                            00179000
END;                                                                    00180000
PROCEDURE HIVAL(A);                                                     00181000
ARRAY A[0];                                                             00182000
BEGIN                                                                   00183000
    STREAM PROCEDURE H(A);                                              00184000
    BEGIN                                                               00185000
        DI~ A;                                                          00186000
        8(DS~ 2 RESET; DS~ 2 SET; DS~ 2 RESET);                         00187000
    END;                                                                00188000
    H(A[9]); H(A[10]);                                                  00189000
    A[11]~ REAL(NOT FALSE);                                             00190000
END;                                                                    00191000
PROCEDURE OUTPUT(T, A);                                                 00192000
VALUE T;                                                                00193000
BOOLEAN T;                                                              00194000
ARRAY A[0];                                                             00195000
BEGIN                                                                   00196000
    BOOLEAN STREAM PROCEDURE P(A, B, N,W);                              00197000
    VALUE N, B,W;                                                       00198000
    BEGIN LABEL Q;                                                      00199000
        DI~ A;                                                          00200000
        DI~ DI-W; DI~DI-1; A~DI; SI~A;                                  00201000
        IF SC=" " THEN BEGIN                                            00202000
        SI~LOC B; SI~ SI+1; DI~DI+1;                                    00203000
        IF W SC=DC THEN                                                 00204000
            BEGIN                                                       00205000
                DI~ DI-W; DI~DI-1;                                      00206000
                DS~ LIT"%";                                             00207000
                SI~LOC N; SI~SI+1;                                      00208000
                DS~W CHR; GO Q;                                         00209000
        END END;                                                        00210000
            TALLY~ 1;                                                   00211000
Q:      P~ TALLY                                                        00212000
    END;                                                                00213000
    BOOLEAN STREAM PROCEDURE E(A, B);                                   00214000
    BEGIN                                                               00215000
        SI~ A;                                                          00216000
        DI~ B; DI~ DI+8;                                                00217000
        IF 8 SC=DC THEN                                                 00218000
            TALLY~ 1                                                    00219000
        ELSE                                                            00220000
            BEGIN                                                       00221000
                SI~ SI-8;                                               00222000
                DI~ DI-8;                                               00223000
                DS~ WDS                                                 00224000
            END;                                                        00225000
        E~ TALLY                                                        00226000
    END;                                                                00227000
    STREAM PROCEDURE D(A, P, S,W);                                      00228000
    VALUE P,W;                                                          00229000
    BEGIN                                                               00230000
        DI~ A;                                                          00231000
        DS~ 28 LIT "% PATCH NUMBER FOR NEXT CARD";                      00232000
        DS~ 44 LIT " ="; DI~DI-W;                                       00233000
        SI~ LOC P;                                                      00234000
        SI~ SI+1;                                                       00235000
        DS~W CHR;                                                       00236000
        SI~ S;                                                          00237000
        DS~ WDS;                                                        00238000
    END;                                                                00239000
    STREAM PROCEDURE X(A,B,T,N,S1,OBJ,W);                               00240000
    VALUE T,N,W,S1,OBJ;                                                 00241000
    BEGIN LABEL L,XIT;                                                  00242000
        DI~B; DS~8 LIT " "; SI~B; DS~14 WDS;                            00243000
        DI~ B; SI~A;                                                    00244000
        DS~ 9 WDS;                                                      00245000
        T(DS~8 LIT " ";JUMP OUT 1 TO L);                                00246000
        SI~ SI-W; DI~DI-W;                                              00247000
        DS~4 LIT " "; DS~W CHR;                                         00248000
        DI~DI+4;                                                        00249000
L:                                                                      00250000
        DS~WDS;                                                         00251000
        DI~DI+2;                                                        00252000
        SI~ LOC N;                                                      00253000
        IF SC="+" THEN GO XIT;                                          00254000
        DI~DI+2; DS~3 DEC; DS~LIT ":"; T~DI;                            00255000
        DI~DI-4; DS~2 FILL; DI~T;                                       00256000
        DS~4 DEC; DI~DI+2; T~DI;                                        00257000
        DS~5 DEC; DI~DI-11; DS~3 FILL;                                  00258000
        DI~T; DS~4 FILL;                                                00259000
XIT:                                                                    00260000
    END;                                                                00261000
    BOOLEAN STREAM PROCEDURE COMP(A,B); BEGIN SI~A; DI~B;               00262000
        IF 8 SC}DC THEN TALLY~1; COMP~TALLY END;                        00263000
BOOLEAN STREAM PROCEDURE VOYD(A,I,VOID);VALUE VOID;BEGIN LABEL L1,L2,L0;00264000
DI~LOC VOID;DI~DI+3;SI~A;L0:IF SC=" " THEN BEGIN SI~SI+1;GO L0 END;     00265000
    IF SC=DC THEN BEGIN                                                 00266000
L1:     IF SC=" " THEN BEGIN SI~SI+1; GO L1 END;                        00267000
        IF 4 SC=DC THEN BEGIN                                           00268000
L2:         IF SC=" " THEN BEGIN SI~SI+1; GO L2 END;                    00269000
            8(IF SC}"0" THEN BEGIN SI~SI+1; TALLY~TALLY+1 END           00270000
                ELSE JUMP OUT);                                         00271000
            VOID~TALLY; SI~SI-VOID; DI~LOC A; DS~VOID OCT;              00272000
            TALLY~1; VOYD~TALLY; SI~LOC A; DI~I; DS~8 DEC;              00273000
    END END END;                                                        00274000
IF T THEN                                                               00275000
    BEGIN                                                               00276000
        TIMEIT(PRINTER);                                                00277000
        CLOSE(PRINTER);                                                 00278000
        WRITE(DECK,<X72"99999999">);                                    00279000
        LOCK(DECK);                                                     00280000
    END                                                                 00281000
ELSE                                                                    00282000
    BEGIN                                                               00283000
    IF VOID THEN IF COMP(A[9],I) THEN VOID~Q~FALSE ELSE Q~<2>;          00284000
    IF Q ~ E(A[9],Q) OR Q THEN                                          00285000
        A[9]~ "# HID #"&"#"[1:43:5]                                     00286000
    ELSE                                                                00287000
IF ABS(A[10])!"       " OR VOID THEN                                    00288000
    IF T~ P(A[9],"       ",IF VOID THEN "VOID   " ELSE A[10],           00289000
        IF VOID THEN 4 ELSE W) THEN BEGIN                               00290000
            D(C,IF VOID THEN "VOID   " ELSE A[10],A[9],                 00291000
                IF VOID THEN 4 ELSE W);                                 00292000
            X(C,B,0,-0,0,0,0);                                          00293000
            WRITE(PRINTER,15,B[*]);                                     00294000
        END;                                                            00295000
    IF REAL(Q) = 0 THEN                                                 00296000
        WRITE(DECK, 10, A[*]);                                          00297000
    X(A,B,(T OR Q).[47:1],A[11].[33:15],A[11].[18:15],OBJ~OBJ+          00298000
        REAL(REAL(Q)=0),IF VOID THEN 5 ELSE W+1);                       00299000
    WRITE(PRINTER,15,B[*]);                                             00300000
    IF NOT VOID THEN IF NOT Q THEN VOID~VOYD(A,I,"$VOID");              00301000
    Q.[47:1] ~ FALSE END;                                               00302000
END;                                                                    00303000
TIMEIT(PRINTER);                                                        00304000
S ~ 12;                                                                 00305000
W ~ IF I~I DIV 100000!0 THEN 72-I ELSE 3;                               00306000
IF W > 7 THEN W ~ 7 ELSE IF W { 0 THEN W ~ 3;                           00307000
FILEPARAMETERS(DECK,B[*]);                                              00308000
WRITE(PRINTER[DBL],<X*,"NEW PATCH DECK IS "A1,A6"/"A1,A6"["I5"]"/>,     00309000
    W+40,B[0].[6:6],B[0],B[1].[6:6],B[1],OBJ|20);                       00310000
OBJ ~ -1;                                                               00311000
IF FINDPATCH THEN                                                       00312000
    SORT(OUTPUT,INPUT,3,HIVAL,COMPARE,12, 5000,120000);                 00313000
END.                                                                    00314000
