BEGIN                                                                   00000100
COMMENT   CUBE LIBRARY VERSION 11/20/68                                 00000101
          CUBE LIBRARY NUMBER Q000006                                   00000102
          SOURCE FILE NAME LIBMAKE/Q000006                              00000103
          PROGRAM NAME LIBTAPE/MAKER;                                   00000104
COMMENT   FOR A DESCRIPTION OF HOW TO USE THIS PROGRAM WRITE TO:        00000200
          COMPUTER CENTER                                               00000300
          WESTINGHOUSE RESEARCH AND DEVELOPMENT CENTER                  00000400
          BEULAH ROAD                                                   00000500
          PITTSBURGH,PENNSYLVANIA    15235                              00000600
          -- AND REQUEST RESEARCH MEMO 67-1C4-COMP-M85                  00000700
             "PROGRAMMATIC LIBRARY TAPE MAINTENANCE ON THE B-5500";     00000800
INTEGER COMSW;                                                          00000900
INTEGER DUMNDX;                                                         00001000
INTEGER NPRIN,NPR;  ARRAY PRIN[0:511,0:14];                             00001100
BOOLEAN CODEFILE,TWOT,CHECKTAPE,ONET;                                   00001200
FILE IN CARD(2,10,30);                                                  00001300
FILE IN TAPE 2(1,1023);                                                 00001400
FILE OUT NEWTAP2 2(1,1023,SAVE 999);                                    00001500
FILE OUT NEWTAPE 2(1,1023,SAVE 999);                                    00001600
FILE SCRATCH DISK SERIAL [20:30](2,10,30);                              00001700
                                                                        00001800
                                                                        00001900
ARRAY TRFAR[0:14];                                                      00002000
LABEL ENOF,PARI;                                                        00002100
LABEL DONEREAD;                                                         00002200
DEFINE PRINTER=PRIN[NPRIN~NPRIN+1,*]#;     FILE OUT PRINT 4(2,15);      00002300
INTEGER ADDR,LEN;                                                       00002400
INTEGER TEM, ECT, COUNT, TAPNDX, NEWTAPENAME, N, TAPENAME, NDX,         00002500
    MFID, T,  J, FID, TE, I, NX      ,                                  00002600
    TU,                                                                 00002700
    FNDX,TFID,LASTFILE,                                                 00002800
    ERRCOUNT,R,                                                         00002900
    ND;                                                                 00003000
ARRAY RESERV[0:15],ID[0:10],DIR[0:1022],EXCPT[0:60],                    00003100
    DUMMY[0:10],                                                        00003200
ZEROF[0:29],                                                            00003300
    NEWDIR[0:1022], FILARR[0:5],  EX[0:30],                             00003400
    STORE[0:1],                                                         00003500
       HED[0:30], TAPEINFO[0:199,0:2];                                  00003600
REAL CHRGNO;                                                            00003700
BOOLEAN PROCEDURE DIRECTORYSEARCH(P,S,H);VALUE P,S;                     00003800
    ALPHA P,S; ARRAY H[0];                                              00003900
BEGIN BOOLEAN FF;                                                       00004000
    INTEGER N,RL,BL;                                                    00004100
    INTEGER STREAM PROCEDURE SIZE(A);                                   00004200
    BEGIN SI~LOC A; DI~LOC SIZE;DI~DI+6;SI~SI+1;                        00004300
        DS~NUM; DS~CHR;                                                 00004400
    END;                                                                00004500
    STREAM PROCEDURE MOVEHDR(B,H,N,F); VALUE N;                         00004600
    BEGIN SI~F;SI~SI-24;DI~LOC F;DS~WDS;                                00004700
        SI~F;SI~SI+56;SI~SI+56;DI~LOC F;DS~WDS;                         00004800
        SI~F;DI~H;DS~N WDS;                                             00004900
    END MOVEHDR;                                                        00005000
    LABEL START;                                                        00005100
    RL~BL~10;                                                           00005200
START: BEGIN FILE OUT DF DISK(1,RL,BL);                                 00005300
        FILL DF WITH P,S;                                               00005400
        IF FF THEN                                                      00005500
        BEGIN IF(N~SIZE(H))>30 THEN N~30;                               00005600
            MOVEHDR(DF(0),H,N,DF);                                      00005700
            FF ~ FALSE;                                                 00005800
        END ELSE                                                        00005900
        BEGIN SEARCH(DF,H[*]);                                          00006000
            DIRECTORYSEARCH ~ FF ~ H[0] > 0;                            00006100
            RL ~ H[3]; BL ~ H[4];                                       00006200
        END;                                                            00006300
    END;                                                                00006400
    IF FF THEN GO TO START;                                             00006500
END DIRECTORYSEARCH;                                                    00006600
BOOLEAN PROCEDURE DIRECTORYSCAN(MFID,FID,HEADER,TEST);                  00006700
ALPHA MFID, FID;                                                        00006800
ARRAY HEADER[0];                                                        00006900
BOOLEAN TEST;                                                           00007000
BEGIN                                                                   00007100
    OWN BOOLEAN FTOG;                                                   00007200
    FILE OUT DUMMY DISK[1: 1](1, 480);                                  00007300
    OWN ARRAY AREA[0:480];                                              00007400
    OWN INTEGER SEG,ROW;                                                00007500
    INTEGER SIZE,DESADD,BUFADD;                                         00007600
    LABEL START, LOOP, GOTONE, EOF;                                     00007700
    INTEGER STREAM PROCEDURE ABSADD(DES);                               00007800
    BEGIN                                                               00007900
        SI~ DES;                                                        00008000
        ABSADD~ SI                                                      00008100
    END;                                                                00008200
    STREAM PROCEDURE DISKREAD(DISK, DES, BUF, O);                       00008300
    VALUE DISK, DES, BUF;                                               00008400
    BEGIN                                                               00008500
        SI~ DES;                                                        00008600
        DI~ O;                                                          00008700
        DS~ WDS;                                                        00008800
        SI~ LOC BUF;                                                    00008900
        DI~ DES;                                                        00009000
        DS~ WDS;                                                        00009100
        DI~ DES;                                                        00009200
        DS~ 5 LIT "RP|AK";                                              00009300
        DS~ 3 RESET;                                                    00009400
        SI~ LOC DISK;                                                   00009500
        DI~ BUF;                                                        00009600
        DS~ 8 DEC                                                       00009700
    END;                                                                00009800
    STREAM PROCEDURE EVOM(A, B, C, D);                                  00009900
    VALUE B, C;                                                         00010000
    BEGIN                                                               00010100
        SI~ A;                                                          00010200
        DI~ B;                                                          00010300
        DS~ WDS;                                                        00010400
        SI~ C;                                                          00010500
        SI~ SI+8;                                                       00010600
        DI~ D;                                                          00010700
        8(DS~ 60 WDS)                                                   00010800
    END;                                                                00010900
    STREAM PROCEDURE MOVE(A, B, C);                                     00011000
    VALUE C;                                                            00011100
    BEGIN                                                               00011200
        SI~ A;                                                          00011300
        DI~ B;                                                          00011400
        DS~ C WDS                                                       00011500
    END;                                                                00011600
    INTEGER STREAM PROCEDURE LNGTH(A);                                  00011700
    BEGIN                                                               00011800
        SI~ LOC A;                                                      00011900
        DI~ LOC LNGTH;                                                  00012000
        SI~ SI+1;                                                       00012100
        DI~ DI+6;                                                       00012200
        DS~ NUM;                                                        00012300
        DS~ CHR                                                         00012400
    END;                                                                00012500
    IF SIZE~LNGTH(HEADER)>30 THEN SIZE ~ 30;                            00012600
    IF FTOG THEN GO TO LOOP;                                            00012700
    FTOG ~ TRUE;                                                        00012800
    SEG~ 987;                                                           00012900
START:                                                                  00013000
    BUFADD~ ABSADD(DUMMY(0))-1;                                         00013100
    DESADD~ ABSADD(DUMMY);                                              00013200
    DISKREAD(SEG~ SEG+16, DESADD, BUFADD, AREA[480]);                   00013300
    RELEASE(DUMMY);                                                     00013400
WAIT(DESADD,(0&1[2:47:1]));                                             00013500
    ROW~ 480;                                                           00013600
    EVOM(AREA[480], DESADD, BUFADD, AREA[0]);                           00013700
    FOR ROW~ ROW-2 STEP -2 UNTIL 450 DO                                 00013800
        BEGIN                                                           00013900
IF REAL(BOOLEAN(MFID~AREA[ROW])EQV BOOLEAN( 76 )   )=REAL(NOT FALSE)    00014000
                THEN GO TO EOF;                                         00014100
IF REAL(BOOLEAN(MFID)EQV BOOLEAN(12   ))=REAL(NOT FALSE)THEN GO LOOP;   00014200
            IF(FID~AREA[ROW+1]).[36:6]="~" THEN GO TO LOOP;             00014300
            FID ~ AREA[ROW+1];                                          00014400
            MOVE(AREA[(ROW-450)|15],HEADER[0],SIZE);                    00014500
            IF (DIRECTORYSCAN ~ TEST) THEN                              00014600
            GO TO GOTONE;                                               00014700
LOOP:                                                                   00014800
        END;                                                            00014900
    GO TO START;                                                        00015000
EOF:                                                                    00015100
    FTOG ~ FALSE;                                                       00015200
GOTONE:                                                                 00015300
END;                                                                    00015400
SAVE ARRAY INFO[0:900],ACARD[0:10];                                     00015500
BOOLEAN NTPROC,MAK;                                                     00015600
LABEL PAR,EOF;                                                          00015700
LABEL MAKTAPE, AFTERSEMI,ERR,PROCDISK,PROCTAPE,    EXIT;                00015800
BOOLEAN JTOG, ITOG, NOTRED;                                             00015900
INTEGER INITADDR;                                                       00016000
DEFINE C=COUNT#;                                                        00016100
DEFINE                                                                  00016200
    MAKE   =1 #,                                                        00016300
    DISPLAY=2 #,                                                        00016400
    FROM   =3 #,                                                        00016500
    DISKK  =4 #,                                                        00016600
    ALL    =5 #,                                                        00016700
    EXCEPT =6 #,                                                        00016800
    IDENT  =7 #,                                                        00016900
    COLON  =8 #,                                                        00017000
    SEMI   =9 #,                                                        00017100
    EQUAL  =10#,                                                        00017200
    SLASH  =11#,                                                        00017300
    COMMA  =12#,                                                        00017400
    ERRR  =13#,                                                         00017500
    ENDOFI =14#,                                                        00017600
    ASTER=15#,                                                          00017700
    LPAR=16#,                                                           00017800
    RPAR=17#;                                                           00017900
BOOLEAN STREAM PROCEDURE LABELOK(SOURCE,MFID,FID);                      00018000
BEGIN                                                                   00018100
    LOCAL T; LABEL FIN;                                                 00018200
    SI~SOURCE;                                                          00018300
    DI~LOC T; DS~8 LIT " LABEL  ";                                      00018400
    DI~DI-8; IF 8 SC=DC THEN                                            00018500
    BEGIN DI~MFID; IF 8 SC=DC THEN                                      00018600
        BEGIN DI~FID; IF 8 SC=DC THEN                                   00018700
            BEGIN TALLY~1; GO FIN                                       00018800
            END                                                         00018900
        END;                                                            00019000
    END;                                                                00019100
FIN: LABELOK~TALLY                                                      00019200
END;                                                                    00019300
BOOLEAN STREAM PROCEDURE ZEROHED(A,B);                                  00019400
BEGIN LABEL EXIT;                                                       00019500
      SI~A; DI~B; 30(IF 8 SC!DC THEN JUMP OUT TO EXIT);TALLY~1;         00019600
EXIT: ZEROHED~TALLY END;                                                00019700
STREAM PROCEDURE RENAME(A,B,N);                                         00019800
BEGIN SI~B; DI~A; DS~5 CHR; SI~N; DS~3 DEC END;                         00019900
INTEGER STREAM PROCEDURE PREFIX(M); VALUE M;                            00020000
    BEGIN                                                               00020100
    SI~LOC M; DI~LOC PREFIX; DI~DI+6;                                   00020200
    IF SC="1" THEN DS~LIT"*"                                            00020300
        ELSE IF SC="0" THEN DS~LIT" "                                   00020400
            ELSE DS~LIT"-";                                             00020500
        SI~SI+1;                                                        00020600
        DS~CHR                                                          00020700
    END;                                                                00020800
PROCEDURE DISPLAYHEADER(MFID,FID);                                      00020900
VALUE MFID,FID;                                                         00021000
INTEGER MFID,FID;                                                       00021100
BEGIN                                                                   00021200
        DEFINE H=HED#;                                                  00021300
INTEGER AREASIZE,YRC,DAYC,MC,YRA,DAYA,MA,AREAS,J;       %DU             00021400
REAL TMP2,YRTMP,T,TMP;                                                  00021500
BOOLEAN INTERLOCKFLAG,NOCS;                                             00021600
FORMAT FMT1(A2,A6"/"A1,A6"=  "A1,A6" ["I2":"I5"] ("I4","I4", SV",       00021700
    I4") ["I5"] "A3,I3 ", 19"I2" ("A3,I3 ") AREA "I2                    00021800
    "("I6")"A2,A1,I7),                                                  00021900
    FMT2(A2,A6"/"A1,A6" CONSISTS ONLY OF A ZERO HEADER");               00022000
ALPHA ARRAY MONTH[0:11];                                                00022100
ALPHA STREAM PROCEDURE OCV(OCTL);                       %DU             00022105
 BEGIN LOCAL ALF;                                        %DU            00022110
  SI:=OCTL;DI:=LOC ALF;DS:=8 DEC;SI:=LOC ALF;           %DU             00022115
  DI:=LOC OCV;DS:=WDS;                                  %DU             00022120
 END OCV;                                               %DU             00022125
FILL MONTH[*] WITH                                                      00022200
    "MAR","APR","MAY","JUN","JUL","AUG","SEP",                          00022300
    "OCT","NOV","DEC","JAN","FEB";                                      00022400
IF ZEROHED(H,ZEROF) THEN WRITE(PRINTER,FMT2,PREFIX(MFID),               00022500
MFID,FID.[6:6],FID) ELSE                                                00022600
BEGIN                                                                   00022700
    AREASIZE~H[0].[30:12] | H[8] DIV H[0].[42:6];                       00022800
    IF H[3].[1:1] NEQ 1 THEN TMP2:=H[3]                  %DU            00022900
     ELSE BEGIN J:=H[3].[12:18];TMP2:=OCV(J);END;       %DU             00022905
    IF(YRC~10|TMP2.[18:6]+TMP2.[24:6]) MOD 4.0=0 THEN                   00023000
       YRTMP~366.0 ELSE YRTMP~365.0;                                    00023100
    DAYC~10|TMP2.[36:6]+TMP2.[30:6]|100+TMP2.[42:6];                    00023200
    MC~(T~(DAYC+305.0) MOD YRTMP+0.5) DIV 30.6;                         00023300
    DAYC~T MOD 30.6 +0.5       ;                                        00023400
    IF H[3].[1:1] NEQ 1 THEN TMP2:=H[4]                  %DU            00023500
     ELSE BEGIN J:=H[3].[30:18];TMP2:=OCV(J);END;        %DU            00023505
IF(YRA~10|TMP2.[18:6] + TMP2.[24:6]) MOD 4.0=0 THEN      %DU            00023600
        YRTMP~366.0 ELSE YRTMP~365.0;                                   00023700
    DAYA~ 10| TMP2.[36:6] + TMP2.[30:6]| 100 +TMP2.[42:6]; %DU          00023800
    MA~(T~(DAYA+305.0) MOD YRTMP +0.5)  DIV 30.6;                       00023900
    DAYA~ T MOD 30.6 +0.5;                                              00024000
    INTERLOCKFLAG~BOOLEAN(H[4].[2:1]);                                  00024100
NOCS~ REAL(NOT BOOLEAN(H[6]))=REAL(NOT FALSE);                          00024200
    AREAS~0;                                                            00024300
    FOR I~10 STEP 1 UNTIL 29 DO                                         00024400
        IF HED[I]>0 THEN AREAS~AREAS+1;                                 00024500
    WRITE(PRINTER,FMT1,                                                 00024600
        PREFIX(MFID),     MFID, FID.[6:6],    FID,                      00024700
        (TMP~  HED[6]).[6:6], TMP, HED[9],      AREASIZE,               00024800
        HED[0].[1:14], HED[0].[15:15], HED[3].[2:10],  HED[7],  %DU     00024900
        MONTH[MC], DAYC,  YRC,  MONTH[MA], DAYA,  AREAS ,               00025000
        HED[8],  IF INTERLOCKFLAG THEN " *" ELSE "  ",                  00025100
        IF NOCS THEN "#" ELSE " ",   AREAS|HED[8]);                     00025200
END                                                                     00025300
        END DISPLAYHEADER;                                              00025400
INTEGER STREAM PROCEDURE GETADD(WORD);                                  00025500
BEGIN SI~WORD; GETADD~SI END;                                           00025600
STREAM PROCEDURE PUTINFO(WORD);                                         00025700
BEGIN DI~WORD; DS~LIT "."; DS~ LIT """; DS~LIT "." END;                 00025800
STREAM PROCEDURE UNLABEL (F);                                           00025900
    BEGIN                                                               00026000
        SI~F; SI~SI-24; DI~LOC F;  DS~WDS;                              00026100
        DI~F;  DI~DI+32;  SKIP 2 DB; DS~SET                             00026200
    END;                                                                00026300
STREAM PROCEDURE LABELF(F);                                             00026400
BEGIN                                                                   00026500
    SI~F; SI~SI-24; DI~LOC F; DS~WDS;                                   00026600
    DI~F; DI~DI+32; SKIP 2 DB; DS~RESET;                                00026700
END;                                                                    00026800
REAL PROCEDURE SCAN;                                                    00026900
BEGIN                                                                   00027000
LABEL ZERO,ONE,TWO,START,EOF;                                           00027100
SWITCH TYPESW~ ZERO,ONE,TWO      ;                                      00027200
INTEGER I,TYPE;                                                         00027300
INTEGER STREAM PROCEDURE SCANN(LEN,TYPE,DEST,ADDR);  VALUE ADDR;        00027400
BEGIN LABEL L,L1,L2;                                                    00027500
SI~ADDR;DI~DEST;DI~DI+1;                                                00027600
L:IF SC=" " THEN BEGIN SI~SI+1;GO L END;                                00027700
IF SC=ALPHA THEN                                                        00027800
BEGIN                                                                   00027900
DS~CHR;TALLY~1;                                                         00028000
L1:IF SC=ALPHA THEN                                                     00028100
BEGIN DS~CHR;TALLY~TALLY+1;GO L1 END;                                   00028200
DS~8 LIT " ";SCANN~SI;                                                  00028300
ADDR~TALLY;DI~LEN;SI~LOC ADDR;DS~WDS;                                   00028400
TALLY~1;ADDR~TALLY;DI~TYPE;SI~LOC ADDR;DS~WDS;                          00028500
END ELSE                                                                00028600
IF SC=""" THEN                                                          00028700
BEGIN SI~SI+1;                                                          00028800
L2:IF SC!""" THEN                                                       00028900
BEGIN DS~CHR;TALLY~TALLY+1;GO L2 END;                                   00029000
SI~SI+1;DS~8 LIT " ";SCANN~SI;                                          00029100
ADDR~TALLY;DI~LEN;SI~LOC ADDR;DS~WDS;                                   00029200
TALLY~2;ADDR~TALLY;DI~TYPE;SI~LOC ADDR;DS~WDS;                          00029300
END ELSE                                                                00029400
BEGIN DI~LEN;DI~DI+7;DS~CHR;SCANN~SI;                                   00029500
TALLY~3;ADDR~TALLY;DI~TYPE;SI~LOC ADDR;DS~WDS                           00029600
END                                                                     00029700
END SCANN;                                                              00029800
START:                                                                  00029900
LEN~0;                                                                  00030000
ID[0]~0;                                                                00030100
R~IDENT;                                                                00030200
    ADDR~ SCANN(LEN,TYPE,ID,ADDR);                                      00030300
    GO TYPESW[TYPE];                                                    00030400
ZERO: IF LEN{7 THEN                                                     00030500
    BEGIN                                                               00030600
        R~-2; WHILE RESERV[R~R+2]!0 DO                                  00030700
            IF REAL(BOOLEAN(RESERV[R]) EQV BOOLEAN(ID[0]))=REAL         00030800
            (NOT FALSE                                                  00030900
            ) THEN                                                      00031000
            BEGIN R~RESERV[R+1]; GO ONE END;                            00031100
            R~IDENT;                                                    00031200
    END;                                                                00031300
    GO ONE;                                                             00031400
TWO: IF LEN="." THEN                                                    00031500
    BEGIN READ(SCRATCH,10,ACARD[*]) [EOF]; ADDR~INITADDR;               00031600
        PUTINFO(ACARD[10]);                                             00031700
        GO START; EOF: R~ENDOFI; GO ONE;                                00031800
    END;                                                                00031900
    R~ IF LEN="," THEN COMMA ELSE                                       00032000
        IF LEN="/" THEN SLASH ELSE                                      00032100
    IF LEN="*" THEN ASTER ELSE                                          00032200
        IF LEN=";" THEN SEMI ELSE                                       00032300
    IF LEN="=" THEN EQUAL ELSE                                          00032400
    IF LEN=":" THEN COLON ELSE                                          00032500
    IF LEN="(" THEN LPAR ELSE IF LEN=")" THEN RPAR ELSE ERRR;           00032600
ONE: SCAN~R;                                                            00032700
END SCAN;                                                               00032800
PROCEDURE ERROR(N); VALUE N; INTEGER N;                                 00032900
BEGIN                                                                   00033000
STREAM PROCEDURE BLANKK(A);                                             00033100
BEGIN DI~A; DS~8 LIT " "; SI~A; DS~4 WDS END;                           00033200
    SWITCH FORMAT SWHFI~                                                00033300
("ZERO"),                                                               00033400
    ("NO TAPENAME"),                                                    00033500
    ("NO FROM"),                                                        00033600
    ("NO COLON"),                                                       00033700
    ("NO SLASH"),                                                       00033800
    ("ALL EXCEPT MFID/"),                                               00033900
    ("ALL EXCEPT =/"),                                                  00034000
    ("NO ID FOLLOWING *"),                                              00034100
    ("ALL EXCEPT"),                                                     00034200
    ("ALL"),                                                            00034300
    ("=/"),                                                             00034400
    ("=/ID EXCEPT"),                                                    00034500
    ("ID/= EXCEPT"),                                                    00034600
    ("ID/= EXCEPT MFID/"),                                              00034700
    ("MFID/"),                                                          00034800
    ("NOT DISK OR TN"),                                                 00034900
    ("INC MESSAGE"),                                                    00035000
    ("END OF FILE"),                                                    00035100
    ("PARITY ERROR"),                                                   00035200
    ("MISSING CHARGE NUMBER"),                                          00035300
    ("MISSING RIGHT PARENTHESIS");                                      00035400
ERRCOUNT~ERRCOUNT+1;                                                    00035500
BLANKK(PRIN[NPRIN+1,10]);                                               00035600
WRITE(PRINTER,10,ACARD[*]);                                             00035700
WRITE(PRINTER,SWHFI[N]);                                                00035800
IF R=SEMI THEN GO AFTERSEMI;                                            00035900
IF R=ENDOFI THEN GO EXIT;                                               00036000
WHILE TRUE DO                                                           00036100
BEGIN                                                                   00036200
IF SCAN=SEMI THEN GO AFTERSEMI;                                         00036300
IF R=ENDOFI THEN GO EXIT                                                00036400
END;                                                                    00036500
END;                                                                    00036600
STREAM PROCEDURE MOVECHARACTERS(N,A,AC,B,BC);                           00036700
VALUE N,AC,BC;                                                          00036800
BEGIN SI~A; SI~SI+AC; DI~B; DI~DI+BC; DS~N CHR END;                     00036900
STREAM PROCEDURE MOVE(N,A,B); VALUE N;                                  00037000
BEGIN SI~A; DI~B; DS~N WDS END;                                         00037100
PROCEDURE CHECKDUP(A,B); VALUE A,B; REAL A,B;                           00037200
BEGIN                                                                   00037300
    INTEGER CCNT,LIMIT; LABEL FIN;                                      00037400
FORMAT FMT1(A2,A6"/"A1,A6" DUPLICATED. THE LATTER FILE WILL BE ON TAPE")00037500
;                                                                       00037600
    LIMIT~COUNT-1;                                                      00037700
FOR CCNT~0 STEP 2 UNTIL LIMIT DO                                        00037800
BEGIN IF REAL(BOOLEAN(NEWDIR[CCNT]) EQV BOOLEAN(A))=REAL(NOT FALSE)     00037900
THEN IF REAL(BOOLEAN(NEWDIR[CCNT+1]) EQV BOOLEAN(B))=REAL(NOT FALSE)    00038000
THEN BEGIN MOVE(1,FILARR[2],NEWDIR[CCNT]); DUMNDX~DUMNDX+1;             00038100
RENAME(FILARR[3],FILARR[3],DUMNDX);                                     00038200
    MOVE (1,FILARR[3],NEWDIR[CCNT+1]);                                  00038300
WRITE(PRINTER,FMT1,PREFIX(A),A,B.[6:6],B);                              00038400
    GO FIN END END                                                      00038500
; FIN: END;                                                             00038600
PROCEDURE LOOKFOR(M,N,O); INTEGER M,N,O;                                00038700
BEGIN                                                                   00038800
    DEFINE LOOKUP=REAL(BOOLEAN(M) EQV BOOLEAN(N))= REAL(NOT FALSE)#;    00038900
WHILE                                                                   00039000
        DIRECTORYSCAN(STORE[0],STORE[1],HED,LOOKUP) DO                  00039100
BEGIN                                                                   00039200
    CHECKDUP(STORE[0],STORE[1]);                                        00039300
    MOVE(2,STORE[0],NEWDIR[COUNT+1]); COUNT~COUNT+2;                    00039400
    END;                                                                00039500
END;                                                                    00039600
PROCEDURE GETHEADER;                                                    00039700
BEGIN                                                                   00039800
        LABEL E;                                                        00039900
    RENAME(TFID,FILARR,FNDX);                                           00040000
    READ(TAPE,10,DUMMY[*])[EOF:PAR];                                    00040100
    CLOSE(TAPE,*);                                                      00040200
    IF LABELOK(DUMMY,TAPENAME,TFID) THEN                                00040300
    BEGIN                                                               00040400
        READ(TAPE,30,HED[*])[EOF:PAR];                                  00040500
    WHILE TRUE DO SPACE(TAPE,1)[E:PAR];                                 00040600
E:                                                                      00040700
        CLOSE(TAPE,*);  COMMENT GET PAST FILE AND TM;                   00040800
        SPACE(TAPE,1)[EOF:PAR];                                         00040900
    DISPLAYHEADER(DIR[J]&0[4:47:1],DIR[J+1]&0[4:47:1]);                 00041000
    END ELSE GO PAR                                                     00041100
END       ;                                                             00041200
PROCEDURE GETDISHED;                                                    00041300
BEGIN                                                                   00041400
INTEGER Z,I; LABEL EE1,EE2;                                             00041500
IF (Z~(FNDX-LASTFILE)-1)>0 THEN                                         00041600
BEGIN                                                                   00041700
    FOR I~1 STEP 1 UNTIL Z DO                                           00041800
    BEGIN                                                               00041900
    WHILE TRUE DO SPACE(TAPE,1)[EE1:PARI];                              00042000
    EE1: CLOSE(TAPE,*);                                                 00042100
    WHILE TRUE DO SPACE(TAPE,1)[EE2:PARI];                              00042200
    EE2: CLOSE(TAPE,*);                                                 00042300
    END;                                                                00042400
SPACE(TAPE,1)[ENOF:PARI];                                               00042500
END;                                                                    00042600
    GETHEADER;                                                          00042700
    LASTFILE~FNDX                                                       00042800
    END;                                                                00042900
PROCEDURE MARKINDIR;                                                    00043000
BEGIN LABEL FIN;                                                        00043100
    FOR J~0 STEP 2 WHILE TRUE DO                                        00043200
    BEGIN                                                               00043300
        IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(MFID))=REAL(NOT FALSE) THEN 00043400
        IF REAL(BOOLEAN(DIR[J+1]) EQV BOOLEAN(ID[0]))                   00043500
        =REAL(NOT FALSE) THEN                                           00043600
        BEGIN                                                           00043700
            DIR[J]~DIR[J]&1[4:47:1]; GO FIN;                            00043800
        END;                                                            00043900
IF REAL(BOOLEAN(DIR[J])EQV BOOLEAN(12   ))=REAL(NOT FALSE) THEN GO FIN; 00044000
    END;                                                                00044100
FIN: END;                                                               00044200
PROCEDURE ALLEXCEPT(MAK); VALUE MAK; BOOLEAN MAK;                       00044300
        BEGIN                                                           00044400
    LABEL IDE,II, HOPOUT,OUTHOP;                                        00044500
IF MAK THEN                                                             00044600
BEGIN                                                                   00044700
FILL TAPE WITH TAPENAME,"FILE000";                                      00044800
    READ(TAPE,1023,DIR[*])[ENOF:PARI];                                  00044900
    CLOSE(TAPE,RELEASE);                                                00045000
END;                                                                    00045100
    IDE:    IF TEM~SCAN{7  THEN                                         00045200
            BEGIN                                                       00045300
COMMENT ALL EXCEPT ID/;                                                 00045400
II:                                                                     00045500
            MOVE(1,ID,MFID);                                            00045600
            IF SCAN! SLASH THEN                                         00045700
ERROR(4);                                                               00045800
            IF T~SCAN{7 THEN                                            00045900
        BEGIN IF MAK THEN                                               00046000
            BEGIN MOVE(1,MFID,EXCPT[NDX~NDX+1]);                        00046100
            MOVE(1,ID,EXCPT[NDX~NDX+1]);                                00046200
        END ELSE MARKINDIR;                                             00046300
        IF T~SCAN=COMMA THEN GO IDE                                     00046400
            END ELSE                                                    00046500
            IF T=EQUAL THEN                                             00046600
            BEGIN                                                       00046700
COMMENT THIS HANDLES "ALL EXCEPT ID/=";                                 00046800
            FOR J~0 STEP 2 WHILE TRUE DO                                00046900
            IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(MFID))=                 00047000
            REAL(NOT FALSE) THEN                                        00047100
        BEGIN IF MAK THEN                                               00047200
            BEGIN MOVE(1,MFID,EXCPT[NDX~NDX+1]);                        00047300
        MOVE(1,DIR[J+1],EXCPT[NDX~NDX+1]);                              00047400
        END ELSE DIR[J]~DIR[J]&1[4:47:1];                               00047500
            END ELSE                                                    00047600
IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(12   )) =REAL(NOT FALSE)            00047700
            THEN GO HOPOUT;                                             00047800
ERROR(4);                                                               00047900
    HOPOUT: IF T~SCAN=COMMA THEN GO IDE                                 00048000
            END ELSE                                                    00048100
ERROR(5);                                                               00048200
            END ELSE                                                    00048300
            IF TEM=EQUAL THEN                                           00048400
            BEGIN                                                       00048500
            IF SCAN! SLASH THEN                                         00048600
COMMENT  ALL EXCEPT =/ID;                                               00048700
            IF SCAN> 7  THEN                                            00048800
ERROR(4);                                                               00048900
ERROR(6);                                                               00049000
            MOVE(1,ID,FID);                                             00049100
            FOR J~1 STEP 2 WHILE TRUE DO                                00049200
    BEGIN                                                               00049300
IF REAL(BOOLEAN(DIR[J-1]) EQV BOOLEAN(12   )) =REAL(NOT FALSE)          00049400
                THEN GO OUTHOP;                                         00049500
            IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(FID))=                  00049600
            REAL(NOT FALSE) THEN                                        00049700
        BEGIN IF MAK THEN                                               00049800
            BEGIN MOVE(1,DIR[J-1],EXCPT[NDX~NDX+1]);                    00049900
            MOVE(1,FID,EXCPT[NDX~NDX+1]);                               00050000
        END ELSE DIR[J-1]~DIR[J-1]&1[4 :47:1 ];                         00050100
    END END;                                                            00050200
        OUTHOP: IF T~SCAN=COMMA THEN GO IDE                             00050300
            END ELSE                                                    00050400
    IF TEM=ASTER THEN BEGIN IF TEM~SCAN>7 THEN                          00050500
ERROR(7);                                                               00050600
    ID[0]~ID[0]&1[5:47:1]; GO II END ELSE                               00050700
ERROR(8);                                                               00050800
        END;                                                            00050900
PROCEDURE EQIDEXD(MAK);VALUE MAK; BOOLEAN MAK;                          00051000
BEGIN                                                                   00051100
    LABEL LOP,LK;                                                       00051200
    IF TU~SCAN>7 THEN                                                   00051300
    IF TU=ASTER THEN BEGIN IF SCAN>7 THEN                               00051400
ERROR(7);                                                               00051500
    ID[0]~ID[0]&1[5:47:1] END ELSE                                      00051600
ERROR(12);                                                              00051700
    ND~0;                                                               00051800
LOP: MOVE(1,ID,MFID);                                                   00051900
    IF SCAN!SLASH  THEN                                                 00052000
ERROR(4);                                                               00052100
        IF TE~ SCAN {  7   THEN                                         00052200
        IF REAL(BOOLEAN(ID[0]) EQV BOOLEAN(FID))=REAL(NOT FALSE)  THEN  00052300
        BEGIN                                                           00052400
        MOVE(1,MFID,EX[ND~ND+1]);                                       00052500
            IF T~SCAN=COMMA THEN                                        00052600
         BEGIN IF TE~SCAN{7 THEN GO LOP;                                00052700
            IF TE=ASTER THEN IF TE~SCAN>7 THEN                          00052800
ERROR(7)                                                                00052900
           ELSE BEGIN ID[0]~ID[0]&1[5:47:1] ; GO LOP END                00053000
END                                                                     00053100
        END ELSE JTOG~TRUE ELSE ITOG~TRUE;                              00053200
LK: IF DIRECTORYSCAN(STORE[0],STORE[1],HED,REAL(BOOLEAN(STORE[1])       00053300
    EQV BOOLEAN(FID)) = REAL(NOT FALSE))  THEN                          00053400
    BEGIN                                                               00053500
        FOR I~1 STEP 1 UNTIL ND DO                                      00053600
        IF REAL(BOOLEAN(STORE[0]) EQV BOOLEAN(EX[I]))=REAL(NOT FALSE)   00053700
        THEN GO LK;                                                     00053800
        IF MAK THEN                                                     00053900
        BEGIN                                                           00054000
    CHECKDUP(STORE[0],FID);                                             00054100
    MOVE(1,STORE[0],NEWDIR[C~C+1]);MOVE(1,FID,NEWDIR[C~C+1]);           00054200
        END ELSE DISPLAYHEADER(STORE[0],STORE[1]);                      00054300
        GO LK;                                                          00054400
    END;                                                                00054500
        END EQIDEXD;                                                    00054600
PROCEDURE IDEQEXD(MAK); VALUE MAK; BOOLEAN MAK;                         00054700
BEGIN                                                                   00054800
    LABEL HERE,LOOK;                                                    00054900
        IF TU~SCAN>7 THEN                                               00055000
        IF TU=ASTER THEN BEGIN IF SCAN>7 THEN                           00055100
ERROR(7);                                                               00055200
        ID[0]~ID[0]&1[5:47:1] END ELSE                                  00055300
ERROR(12);                                                              00055400
        ND~0;                                                           00055500
HERE:  IF REAL(BOOLEAN(ID[0]) EQV BOOLEAN(MFID))=REAL(NOT FALSE)        00055600
        THEN                                                            00055700
        BEGIN                                                           00055800
            IF SCAN!SLASH THEN                                          00055900
ERROR(4);                                                               00056000
            IF SCAN> 7   THEN                                           00056100
ERROR(13);                                                              00056200
        MOVE(1,ID,EX[ND~ND+1]);                                         00056300
        IF T~SCAN=COMMA THEN                                            00056400
         BEGIN IF TE~SCAN{7 THEN GO HERE;                               00056500
        IF TE=ASTER THEN IF SCAN>7 THEN                                 00056600
ERROR(7)                                                                00056700
        ELSE                                                            00056800
        BEGIN ID[0]~ID[0]&1[5:47:1]; GO HERE;                           00056900
END                                                                     00057000
        END;                                                            00057100
END;                                                                    00057200
LOOK:   IF DIRECTORYSCAN(STORE[0],STORE[1],HED, REAL(BOOLEAN(STORE[0])  00057300
        EQV BOOLEAN(MFID))=REAL(NOT FALSE))  THEN                       00057400
            BEGIN                                                       00057500
            FOR I~1 STEP 1 UNTIL ND DO                                  00057600
            IF REAL(BOOLEAN(STORE[1]) EQV BOOLEAN(EX[I]))=              00057700
            REAL(NOT FALSE) THEN GO LOOK;                               00057800
        IF MAK THEN                                                     00057900
        BEGIN                                                           00058000
    CHECKDUP(MFID,STORE[1]);                                            00058100
        MOVE (1,MFID,NEWDIR[C~C+1]); MOVE(1,STORE[1],NEWDIR[C~C+1]);    00058200
        END ELSE DISPLAYHEADER(STORE[0],STORE[1]);                      00058300
            GO LOOK                                                     00058400
            END;                                                        00058500
END IDEQEXD;                                                            00058600
PROCEDURE IDEQEX(MAK); VALUE MAK; BOOLEAN MAK;                          00058700
BEGIN                                                                   00058800
    LABEL LP1,F,HOUT;                                                   00058900
        NX~0;                                                           00059000
        IF TU~SCAN>7 THEN                                               00059100
        IF TU=ASTER THEN BEGIN IF SCAN>7 THEN                           00059200
ERROR(7);                                                               00059300
        ID[0]~ID[0]&1[5:47:1] END ELSE                                  00059400
ERROR(12);                                                              00059500
    LP1: IF REAL(BOOLEAN(MFID)  EQV BOOLEAN(ID[0]))=REAL(NOT FALSE)     00059600
        THEN                                                            00059700
            BEGIN                                                       00059800
            IF SCAN!SLASH THEN ERROR(4);                                00059900
            IF SCAN>7 THEN ERROR(13);                                   00060000
            MOVE(1,ID,EX[NX~NX+1]);                                     00060100
            IF T~SCAN=COMMA THEN BEGIN IF TEM~SCAN{7 THEN GO LP1;       00060200
            IF TEM=ASTER THEN IF SCAN>7 THEN                            00060300
ERROR(7)                                                                00060400
            ELSE BEGIN ID[0]~ID[0]&1[5:47:1]; GO LP1 END                00060500
END;                                                                    00060600
            END;                                                        00060700
            FOR J~0 STEP 2 WHILE TRUE DO                                00060800
                BEGIN                                                   00060900
                IF REAL(BOOLEAN(DIR[J])EQV BOOLEAN(MFID))               00061000
                =REAL(NOT FALSE)                                        00061100
                THEN                                                    00061200
                    BEGIN                                               00061300
                    FOR I~1 STEP 1 UNTIL NX DO                          00061400
                    IF  REAL(BOOLEAN(DIR[J+1]) EQV BOOLEAN(EX[I]))=     00061500
                    REAL(NOT FALSE) THEN GO F;                          00061600
        IF MAK THEN                                                     00061700
        BEGIN                                                           00061800
    CHECKDUP(MFID,DIR[J+1]);                                            00061900
            MOVE(1,MFID,NEWDIR[C~C+1]);MOVE(1,DIR[J+1],NEWDIR[C~C+1]);  00062000
        END  ELSE DIR[J]~DIR[J]&1[4 :47:1 ];                            00062100
                    END ELSE                                            00062200
IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(12   ))=REAL(NOT FALSE)             00062300
                    THEN GO HOUT ;                                      00062400
            F:  END;                                                    00062500
        HOUT:                                                           00062600
        END IDEQEX;                                                     00062700
PROCEDURE EQIDEX(MAK); VALUE MAK; BOOLEAN MAK;                          00062800
BEGIN                                                                   00062900
    LABEL LP,GOUT,FIN;                                                  00063000
        IF TU~SCAN>7 THEN                                               00063100
        IF TU=ASTER THEN BEGIN IF SCAN>7 THEN                           00063200
ERROR(7);                                                               00063300
        ID[0]~ID[0]&1[5:47:1] END ELSE                                  00063400
ERROR(11);                                                              00063500
    NX~ 0 ;                                                             00063600
    LP: MOVE(1,ID,MFID);                                                00063700
        IF SCAN!SLASH THEN                                              00063800
ERROR(4);                                                               00063900
            IF T~SCAN {7      THEN                                      00064000
            IF REAL(BOOLEAN(ID[0]) EQV BOOLEAN(FID)) =REAL (NOT FALSE)  00064100
            THEN                                                        00064200
            BEGIN MOVE (1,MFID,EX[NX~NX+1]);                            00064300
            IF T~SCAN=COMMA THEN BEGIN                                  00064400
IF TEM~SCAN{7 THEN GO LP;                                               00064500
                                 IF TEM=ASTER THEN IF SCAN>7 THEN       00064600
ERROR(7)                                                                00064700
                                 ELSE BEGIN ID[0]~ID[0]&1[5:47:1]; GO   00064800
                                   LP END                               00064900
END END ELSE ITOG~TRUE ELSE JTOG~TRUE;                                  00065000
                FOR J~1 STEP 2 WHILE TRUE DO                            00065100
                BEGIN                                                   00065200
IF REAL(BOOLEAN(DIR[J-1])  EQV BOOLEAN(12   ))=REAL(NOT FALSE)          00065300
                THEN GO GOUT;                                           00065400
                IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(FID)) =             00065500
                REAL(NOT FALSE) THEN                                    00065600
                    BEGIN                                               00065700
                FOR I~ 1 STEP 1 UNTIL NX DO                             00065800
                    IF REAL(BOOLEAN(DIR[J-1]) EQV BOOLEAN(EX[I]))       00065900
                    = REAL(NOT FALSE) THEN GO FIN;                      00066000
    IF MAK THEN                                                         00066100
    BEGIN                                                               00066200
CHECKDUP(DIR[J-1],FID);                                                 00066300
        MOVE(1,DIR[J-1],NEWDIR[COUNT~COUNT+1]);                         00066400
        MOVE(1,FID,NEWDIR[COUNT~COUNT+1]);                              00066500
    END ELSE DIR[J-1]~DIR[J-1]&1[4 :47:1 ];                             00066600
    END;                                                                00066700
            FIN:END;                                                    00066800
    GOUT: END EQIDEX;                                                   00066900
FILL  RESERV[*] WITH                                                    00067000
    "MAKE   ",  1,                                                      00067100
    "DISPLAY",  2,                                                      00067200
    "FROM   ",  3,                                                      00067300
    "DISK   ",  4,                                                      00067400
    "ALL    ",  5,                                                      00067500
    "EXCEPT ",  6,                                                      00067600
0;                                                                      00067700
FILL FILARR[*] WITH                                                     00067800
    "FILE000",                                                          00067900
    "ALL    ", "DUMMY  ",                                               00068000
    "+FILE000";                                                         00068100
FOR I~0 STEP 1 UNTIL 29 DO ZEROF[I]~0;                                  00068200
COMMENT THE PROGRAM STARTS HERE;                                        00068300
    MAK~                                                                00068400
CODEFILE~TWOT~CHECKTAPE~                                                00068500
    ITOG~JTOG~FALSE;                                                    00068600
DUMNDX~-1;                                                              00068700
NPRIN~-1;                                                               00068800
CHRGNO~                                                                 00068900
ERRCOUNT~0;                                                             00069000
ADDR~                                                                   00069100
    INITADDR~GETADD(ACARD);                                             00069200
WHILE TRUE DO                                                           00069300
BEGIN READ(CARD,10,ACARD[*])[DONEREAD];                                 00069400
WRITE(SCRATCH,10,ACARD[*]);                                             00069500
END;                                                                    00069600
DONEREAD: REWIND(SCRATCH); CLOSE(CARD,RELEASE);                         00069700
READ(SCRATCH,10,ACARD[*])[EOF];                                         00069800
    PUTINFO(ACARD[10]);                                                 00069900
IF BOOLEAN(COMSW) THEN CODEFILE~TRUE;                                   00070000
IF BOOLEAN(COMSW.[46:1]) THEN TWOT~TRUE;                                00070100
IF BOOLEAN(COMSW.[45:1]) THEN CHECKTAPE~TRUE;                           00070200
ONET~TRUE;                                                              00070300
IF TEM~SCAN=MAKE                                                        00070400
THEN MAK~TRUE ELSE                                                      00070500
      IF TEM=DISPLAY THEN GO AFTERSEMI ELSE ERROR(0);                   00070600
    IF   SCAN> 7  THEN                                                  00070700
ERROR(1);                                                               00070800
    COUNT ~ -1;    TAPNDX~0;                                            00070900
    MOVE(1,ID,NEWTAPENAME);                                             00071000
AFTERSEMI:                                                              00071100
    IF T~SCAN!FROM THEN                                                 00071200
    IF T=ENDOFI THEN GO MAKTAPE ELSE ERROR(2);                          00071300
    N~ COUNT;                                                           00071400
    NTPROC~TRUE;                                                        00071500
    IF T~SCAN=DISKK THEN GO PROCDISK;                                   00071600
    IF T>7 THEN                                                         00071700
ERROR(15);                                                              00071800
    MOVE(1,ID,TAPENAME); IF SCAN!COLON THEN                             00071900
ERROR(3);                                                               00072000
IF NOT MAK THEN                                                         00072100
BEGIN FILL TAPE WITH TAPENAME,FILARR[0];                                00072200
       READ(TAPE,1023,DIR[*])[ENOF:PARI];                               00072300
       CLOSE(TAPE,*);                                                   00072400
END;                                                                    00072500
PROCTAPE:                                                               00072600
BEGIN                                                                   00072700
    LABEL HI,IDT,HH,HHH,HPOUT,LOOKFORSEMI,NT,G,TRY,                     00072800
ISEMI,                                                                  00072900
HOUT,H,HPO,                                                             00073000
    SETAPINFO,GETOUT;                                                   00073100
INTEGER Z;                                                              00073200
NOTRED~TRUE;                                                            00073300
    NDX~-1;                                                             00073400
    IF TEM~SCAN=ALL THEN                                                00073500
    BEGIN                                                               00073600
        IF T~SCAN=EXCEPT  THEN                                          00073700
    BEGIN IF MAK THEN BEGIN                                             00073800
        ALLEXCEPT(TRUE); GO LOOKFORSEMI END     ELSE                    00073900
        BEGIN                                                           00074000
            ALLEXCEPT(FALSE);                                           00074100
    UNLABEL(TAPE);  FNDX~0;                                             00074200
    FOR J~0 STEP 2 WHILE TRUE DO                                        00074300
    BEGIN                                                               00074400
        FNDX~FNDX+1;                                                    00074500
IF REAL(BOOLEAN(DIR[J])EQV BOOLEAN(12   ))=REAL(NOT FALSE) THEN GO HPOUT00074600
 ELSE                                                                   00074700
    IF NOT BOOLEAN(DIR[J].[4:1]) THEN                                   00074800
GETDISHED                                                               00074900
    END;                                                                00075000
HPOUT:                                                                  00075100
CLOSE(TAPE,RELEASE); LABELF(TAPE);                                      00075200
GO LOOKFORSEMI;                                                         00075300
        END;                                                            00075400
    END;                                                                00075500
        IF T=SLASH THEN                                                 00075600
        BEGIN                                                           00075700
COMMENT ALL MUST BE AN ID;                                              00075800
        MOVE(1,FILARR[1],MFID); GO IDT                                  00075900
        END                                                             00076000
        ELSE                                                            00076100
        BEGIN                                                           00076200
COMMENT COPY ALL FILES;                                                 00076300
    IF MAK THEN                                                         00076400
BEGIN FILL TAPE WITH TAPENAME,FILARR[0];                                00076500
    READ(TAPE,1023,DIR[*])[ENOF:PARI];                                  00076600
    CLOSE(TAPE,RELEASE);                                                00076700
    FOR J~0 STEP 2 WHILE TRUE DO                                        00076800
IF REAL(BOOLEAN(DIR[ J]) EQV BOOLEAN(12   ))=REAL(NOT FALSE)            00076900
    THEN GO SETAPINFO                                                   00077000
    ELSE                                                                00077100
    BEGIN                                                               00077200
    CHECKDUP(DIR[J],DIR[J+1]);                                          00077300
    MOVE(2,DIR[J],NEWDIR[COUNT~COUNT+1]);                               00077400
    COUNT~COUNT+1;                                                      00077500
    END;                                                                00077600
    END ELSE                                                            00077700
        BEGIN                                                           00077800
            COMMENT DISPLAY ALL HEADERS;                                00077900
UNLABEL(TAPE); FNDX~0;                                                  00078000
            FOR J~0 STEP 2 WHILE TRUE DO                                00078100
IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(12   ))= REAL(NOT FALSE)            00078200
            THEN GO HOUT                                                00078300
            ELSE                                                        00078400
            BEGIN                                                       00078500
    FNDX~FNDX+1;                                                        00078600
    GETHEADER;                                                          00078700
            END;                                                        00078800
HOUT:                                                                   00078900
CLOSE(TAPE,RELEASE); LABELF(TAPE);                                      00079000
GO LOOKFORSEMI END                                                      00079100
        END;                                                            00079200
    END ELSE                                                            00079300
H:  IF TEM=EQUAL THEN                                                   00079400
    BEGIN                                                               00079500
COMMENT MUST BE =/ID;                                                   00079600
        IF SCAN!SLASH THEN                                              00079700
ERROR(4);                                                               00079800
        IF SCAN> 7  THEN                                                00079900
ERROR(10);                                                              00080000
IF MAK THEN                                                             00080100
    IF NOTRED THEN                                                      00080200
    BEGIN FILL TAPE WITH TAPENAME,FILARR[0];                            00080300
    READ(TAPE,1023,DIR[*])[ENOF:PARI];                                  00080400
    NOTRED~FALSE; CLOSE(TAPE,RELEASE);                                  00080500
    END;                                                                00080600
        MOVE(1,ID,FID);                                                 00080700
        IF T ~ SCAN! EXCEPT THEN                                        00080800
        BEGIN                                                           00080900
COMMENT IT IS =/ID  ONLY;                                               00081000
        FOR J~1 STEP 2 WHILE TRUE DO                                    00081100
    BEGIN                                                               00081200
IF REAL(BOOLEAN(DIR[J-1]) EQV BOOLEAN(12   )) = REAL(NOT FALSE) THEN GO 00081300
        GETOUT;                                                         00081400
        IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(FID))=REAL(NOT FALSE) THEN  00081500
        BEGIN                                                           00081600
    IF MAK THEN BEGIN                                                   00081700
        COUNT~COUNT+1;                                                  00081800
    CHECKDUP(DIR[J-1],DIR[J]);                                          00081900
        MOVE(2,DIR[J-1],NEWDIR[COUNT]);COUNT~COUNT+1;                   00082000
    END ELSE DIR[J-1]~DIR[J-1]&1[4:47:1];                               00082100
    END END;                                                            00082200
GETOUT: IF T=COMMA THEN BEGIN TEM~SCAN; GO H END ELSE                   00082300
    IF MAK THEN GO LOOKFORSEMI ELSE GO ISEMI;                           00082400
        END ELSE                                                        00082500
        BEGIN                                                           00082600
COMMENT IT IS =/ID EXCEPT;                                              00082700
            EQIDEX(MAK);                                                00082800
            NX~0;                                                       00082900
            IF ITOG THEN BEGIN ITOG~FALSE; GO HH END ELSE               00083000
            IF JTOG THEN BEGIN JTOG~FALSE;  GO HHH END;                 00083100
            IF T=COMMA THEN GO H ELSE IF MAK THEN                       00083200
           GO LOOKFORSEMI ELSE GO ISEMI                                 00083300
    END                                                                 00083400
    END ELSE                                                            00083500
    IF TEM { 7    THEN                                                  00083600
    BEGIN                                                               00083700
HI:                                                                     00083800
    MOVE(1,ID,MFID); IF SCAN! SLASH                                     00083900
THEN ERROR(4);                                                          00084000
IDT:                                                                    00084100
    IF  T   ~SCAN{ 7    THEN                                            00084200
HH: BEGIN                                                               00084300
IF MAK THEN BEGIN                                                       00084400
    CHECKDUP(MFID,ID[0]);                                               00084500
    MOVE(1,MFID,NEWDIR[COUNT~COUNT+1]);                                 00084600
MOVE(1,ID,NEWDIR[COUNT~COUNT+1]);                                       00084700
    END ELSE MARKINDIR;                                                 00084800
COMMENT IT IS MFID/FID;                                                 00084900
IF T~SCAN=COMMA THEN BEGIN TEM~SCAN; GO H END ELSE                      00085000
IF MAK THEN GO LOOKFORSEMI ELSE GO ISEMI;                               00085100
    END ELSE                                                            00085200
HHH:IF T=EQUAL THEN                                                     00085300
    BEGIN                                                               00085400
    IF MAK THEN       IF NOTRED THEN                                    00085500
    BEGIN FILL TAPE WITH TAPENAME,FILARR[0];                            00085600
    READ(TAPE,1023,DIR[*])[ENOF:PARI];                                  00085700
    NOTRED~FALSE; CLOSE(TAPE,RELEASE);                                  00085800
    END;                                                                00085900
        IF T ~ SCAN=EXCEPT THEN                                         00086000
        BEGIN                                                           00086100
COMMENT IT IS ID/= EXCEPT;                                              00086200
        IDEQEX(MAK);                                                    00086300
        NX~0;                                                           00086400
        IF T=COMMA THEN GO H ELSE IF MAK THEN                           00086500
        GO LOOKFORSEMI ELSE GO ISEMI;                                   00086600
        END  ELSE                                                       00086700
        BEGIN                                                           00086800
COMMENT IT IS ID/=;                                                     00086900
        FOR J~0 STEP 2 WHILE TRUE DO                                    00087000
        IF REAL(BOOLEAN(MFID) EQV BOOLEAN(DIR[J]))=REAL(NOT FALSE)      00087100
        THEN                                                            00087200
        BEGIN IF MAK THEN BEGIN                                         00087300
    CHECKDUP(MFID,DIR[J+1]);                                            00087400
       MOVE(1,MFID,NEWDIR[COUNT~COUNT+1]);                              00087500
            MOVE(1,DIR[J+1],NEWDIR[C~C+1]);                             00087600
        END ELSE DIR[J]~DIR[J]&1[4:47:1];                               00087700
        END ELSE                                                        00087800
IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(12   ))=REAL(NOT FALSE)             00087900
        THEN GO HPO;                                                    00088000
    HPO: IF T=COMMA THEN BEGIN TEM~SCAN; GO H END                       00088100
        ELSE IF MAK THEN GO LOOKFORSEMI ELSE GO ISEMI                   00088200
    END                                                                 00088300
    END ELSE                                                            00088400
ERROR(14);                                                              00088500
END ELSE                                                                00088600
    IF TEM=ASTER THEN IF SCAN{7 THEN                                    00088700
    BEGIN ID[0]~ID[0]&1[5:47:1]; GO HI END ELSE                         00088800
ERROR(7) ELSE ERROR(16);                                                00088900
ISEMI:                                                                  00089000
    LASTFILE~0;                                                         00089100
    UNLABEL(TAPE); FNDX~0;                                              00089200
    FOR J~0 STEP 2 WHILE TRUE DO                                        00089300
    BEGIN FNDX~FNDX+1;                                                  00089400
    IF BOOLEAN(DIR[J].[4:1]) THEN                                       00089500
    GETDISHED ELSE                                                      00089600
IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(12   ))=REAL(NOT FALSE)             00089700
THEN BEGIN CLOSE(TAPE,RELEASE); LABELF(TAPE);                           00089800
GO LOOKFORSEMI END;                                                     00089900
    END;                                                                00090000
LOOKFORSEMI:                                                            00090100
COMMENT FILES ON TAPE;                                                  00090200
IF MAK THEN BEGIN                                                       00090300
    IF T=SEMI THEN                                                      00090400
    BEGIN                                                               00090500
NT: NTPROC~FALSE;                                                       00090600
    IF NDX>0 THEN                                                       00090700
        BEGIN                                                           00090800
        FOR J~0 STEP 2 WHILE TRUE DO                                    00090900
        BEGIN                                                           00091000
IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(12   ))=REAL(NOT FALSE) THEN GO G ; 00091100
            FOR I~0 STEP 2 UNTIL NDX DO                                 00091200
            IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(EXCPT[I]))=REAL(NOT     00091300
            FALSE)  THEN                                                00091400
            IF REAL(BOOLEAN(DIR[J+1]) EQV BOOLEAN(EXCPT[I+1]))          00091500
     =      REAL(NOT FALSE)  THEN GO  TRY  ;                            00091600
            C~C+1;                                                      00091700
CHECKDUP(DIR[J],DIR[J+1]);                                              00091800
        MOVE(2,DIR[J],NEWDIR[C]);                                       00091900
            C~C+1;                                                      00092000
    TRY:END;                                                            00092100
    G:                                                                  00092200
        END;                                                            00092300
SETAPINFO:                                                              00092400
        TAPEINFO[TAPNDX,0] ~ N+1;                                       00092500
        TAPEINFO[TAPNDX ,1]~  COUNT;                                    00092600
        MOVE(1,TAPENAME,TAPEINFO[TAPNDX,2]);                            00092700
        TAPNDX~TAPNDX+1;                                                00092800
        IF T=ENDOFI THEN GO MAKTAPE ELSE GO AFTERSEMI;                  00092900
END ELSE IF T=ENDOFI THEN                                               00093000
BEGIN IF NTPROC THEN GO NT ELSE GO MAKTAPE END ELSE                     00093100
ERROR(16);                                                              00093200
END ELSE BEGIN IF T=SEMI THEN GO AFTERSEMI                              00093300
ELSE IF T=ENDOFI THEN GO EXIT ELSE ERROR(16);                           00093400
END;                                                                    00093500
GO MAKTAPE;                                                             00093600
END;                                                                    00093700
PROCDISK:                                                               00093800
BEGIN                                                                   00093900
LABEL TN;                                                               00094000
LABEL PROC,LQ,HII,L,LL,ZZ,MUSTBESEMI;                                   00094100
INTEGER MMFID,FFID;                                                     00094200
FORMAT         FMT4A("NO FILE ON DISK ",A1,A6,"/",A1,A6);        %DU    00094205
IF SCAN!COLON THEN ERROR(3);                                            00094300
PROC:                                                                   00094400
    TE~SCAN;                                                            00094500
LQ:                                                                     00094600
IF TE{7 THEN                                                            00094700
    BEGIN                                                               00094800
HII:                                                                    00094900
    MOVE(1,ID,MFID);                                                    00095000
    IF SCAN!SLASH THEN                                                  00095100
ERROR(4);                                                               00095200
    IF TE~SCAN{7 THEN                                                   00095300
    BEGIN                                                               00095400
COMMENT IT IS MFID/FID;                                                 00095500
L:                                                                      00095600
IF CODEFILE THEN IF NOT DIRECTORYSEARCH(MFID,ID[0],HED) THEN            00095700
   BEGIN WRITE(PRINTER,FMT4A,MFID.[6:6],MFID,ID[0].[6:6],ID[0]); %DU    00095705
IF T~SCAN=COMMA THEN GO PROC ELSE GO MUSTBESEMI;                        00095800
   END;                                                          %DU    00095805
        IF MAK THEN BEGIN                                               00095900
    CHECKDUP(MFID,ID[0]);                                               00096000
        MOVE(1,MFID,NEWDIR[C~C+1]); MOVE(1,ID,NEWDIR[C~C+1]);           00096100
        END ELSE                                                        00096200
IF CODEFILE THEN DISPLAYHEADER(MFID,ID[0]) ELSE                         00096300
        IF DIRECTORYSEARCH(MFID,ID[0],HED) THEN                         00096400
        DISPLAYHEADER(MFID,ID[0]) ELSE                         %MAR 69  00096500
        WRITE(PRINTER,FMT4A,MFID.[6:6],MFID,ID[0].[6:6],ID[0]);%MAR 69  00096550
    IF T~  SCAN=COMMA THEN GO PROC     ELSE GO MUSTBESEMI;              00096600
    END ELSE                                                            00096700
LL:                                                                     00096800
    IF TE= EQUAL THEN                                                   00096900
    IF T~   SCAN! EXCEPT THEN                                           00097000
    BEGIN                                                               00097100
COMMENT IT IS ID/= ONLY;                                                00097200
    IF MAK THEN                                                         00097300
    LOOKFOR(STORE[0],MFID,STORE[1]) ELSE                                00097400
WHILE DIRECTORYSCAN(MMFID,FFID,HED,                                     00097500
REAL(BOOLEAN(MMFID) EQV BOOLEAN(MFID))=REAL(NOT FALSE))                 00097600
DO DISPLAYHEADER(MMFID,FFID);                                           00097700
    IF T=COMMA THEN GO PROC     ELSE GO MUSTBESEMI                      00097800
    END ELSE                                                            00097900
    BEGIN                                                               00098000
COMMENT IT IS ID/= EXCEPT;                                              00098100
IDEQEXD(MAK);                                                           00098200
    IF T=COMMA THEN GO LQ ELSE GO MUSTBESEMI;                           00098300
    END;                                                                00098400
    ERROR(16)                                                           00098500
    END ELSE                                                            00098600
ZZ:                                                                     00098700
    IF TE= EQUAL THEN                                                   00098800
    BEGIN                                                               00098900
    IF SCAN! SLASH THEN                                                 00099000
BEGIN IF R=LPAR THEN                                                    00099100
    BEGIN IF SCAN=IDENT THEN BEGIN MOVECHARACTERS(5,ID[0],1,            00099200
CHRGNO,3);                                                              00099300
    WHILE DIRECTORYSCAN(MMFID,FFID,HED,REAL(BOOLEAN(CHRGNO)             00099400
    EQV BOOLEAN(HED[6].[6:30]))=REAL(NOT FALSE)) DO                     00099500
    BEGIN CHECKDUP(MMFID,FFID);                                         00099600
    MOVE(1,MMFID,NEWDIR[COUNT+1]);                                      00099700
        MOVE(1,FFID,NEWDIR[COUNT~COUNT+2]);                             00099800
        END; IF SCAN=RPAR THEN IF T~SCAN =COMMA THEN GO PROC            00099900
    ELSE GO MUSTBESEMI ELSE ERROR(20);                                  00100000
    END ELSE ERROR(19) END ELSE ERROR(4)                                00100100
END;                                                                    00100200
    IF SCAN> 7  THEN                                                    00100300
ERROR(10);                                                              00100400
    MOVE(1,ID,FID);                                                     00100500
    IF T   ~ SCAN=EXCEPT THEN                                           00100600
    BEGIN                                                               00100700
COMMENT IT IS =/ID EXCEPT;                                              00100800
    EQIDEXD(MAK);                                                       00100900
    IF ITOG THEN BEGIN ITOG~FALSE; GO LL END;                           00101000
    IF JTOG THEN BEGIN JTOG~FALSE; GO  L END;                           00101100
    IF T=COMMA THEN GO ZZ       ELSE GO MUSTBESEMI;                     00101200
    END ELSE                                                            00101300
    BEGIN                                                               00101400
COMMENT IT IS =/ID ONLY;                                                00101500
        IF MAK THEN                                                     00101600
        LOOKFOR(STORE[1],FID,STORE[0]) ELSE                             00101700
WHILE DIRECTORYSCAN(MMFID,FFID,HED,                                     00101800
REAL(BOOLEAN(FFID) EQV BOOLEAN(FID))                                    00101900
=REAL(NOT FALSE)) DO DISPLAYHEADER(MMFID,FFID);                         00102000
        IF T=COMMA THEN GO PROC ELSE GO MUSTBESEMI;                     00102100
    END                                                                 00102200
    END ELSE                                                            00102300
    IF TE=ASTER THEN IF SCAN{7 THEN                                     00102400
    BEGIN ID[0]~ID[0]&1[5:47:1]; GO HII END ELSE                        00102500
ERROR(7)                                                                00102600
    ELSE                                                                00102700
ERROR(16);                                                              00102800
MUSTBESEMI:                                                             00102900
COMMENT FILES ON DISK;                                                  00103000
IF MAK THEN                                                             00103100
BEGIN                                                                   00103200
IF T=SEMI THEN                                                          00103300
    BEGIN                                                               00103400
TN: NTPROC~FALSE;                                                       00103500
    TAPEINFO[TAPNDX,0]  ~N+1;                                           00103600
    TAPEINFO[TAPNDX,1]  ~COUNT;                                         00103700
    TAPEINFO[TAPNDX,2]  ~-1;                                            00103800
    TAPNDX~TAPNDX+1;                                                    00103900
    IF T=ENDOFI THEN GO MAKTAPE ELSE GO AFTERSEMI;                      00104000
    END ELSE                                                            00104100
IF T=ENDOFI THEN                                                        00104200
BEGIN IF NTPROC THEN GO TN ELSE GO MAKTAPE END ELSE                     00104300
ERROR(16);                                                              00104400
END ELSE BEGIN IF T=SEMI THEN GO AFTERSEMI ELSE                         00104500
IF T=ENDOFI THEN GO EXIT ELSE ERROR(16);                                00104600
END;                                                                    00104700
END;                                                                    00104800
MAKTAPE:                                                                00104900
BEGIN                                                                   00105000
LABEL TRYA,TRYB;                                                        00105100
LABEL PARRR,ENDD,PARR2,ENDD2,FINNT,NS,NS2,QT,QTT,PP,ENDT;               00105200
LABEL EN2,PA2,EN3,PA3,EN4,PA4;                                          00105300
LABEL EN5;                                                              00105400
LABEL PARQ,PARQQ,PARZ,PARZZ,IFONE,IFTWO,NZ;                             00105500
INTEGER NSEGT,NTT;                                                      00105600
ARRAY ATTAPE[0:900];                                                    00105700
FORMAT FFFO("TAPE DOES NOT AGREE WITH TAPE");                           00105800
INTEGER NSEG,NSEG2,N2,STARTSEG;                                         00105900
LABEL FINTAPE,PARR,ENDO,PAR2,END2,QQQ,QQ,FINN,CHK,EE,EEE,GETFIL;        00106000
FORMAT FFO("TAPE AND DISK DO NOT CHECK");                               00106100
FORMAT FFD(2(A2,A6)),                                                   00106200
FFH("#ENTER NUMBER OF BOTH TAPES ",A1,A6," IN FORM XXX-X XXX-X~"),      00106300
FFI("#ENTER NUMBER OF TAPE ",A1,A6" IN FORM XXX-X~"),                   00106400
FFG(2A1,A6,X1,A1,A6);                                                   00106500
BOOLEAN NOTZEROF;                                                       00106600
LABEL E1,E2,E3,E4,E5,E6;                                                00106700
FORMAT  FMT2(A2,A6,"/",A1,A6),                                          00106800
        FMT3("FILES ON ",           A1,A6,":" ),                        00106900
FMT4("THERE IS NO FILE ",A2,A6"/"A1,A6);                                00107000
REAL T;                                                                 00107100
ARRAY CODEDIR[0:1022];                                                  00107200
ARRAY ATAPE,ATAPE2[0:1022];                                             00107300
    INTEGER TAP,STARTFIL,ENDFIL,FILD,                                   00107400
    Z,FILO,LASTFILE,                                                    00107500
            SEGPERAREA,DISKBUF,                                         00107600
    TFID,K,TEMP,                                                        00107700
    FNDX,                                                               00107800
    BEGSEG,BLKRED,WHATAREA,NOBLKS,LEFTOVER;                             00107900
    LABEL READSEG,REDSEG;                                               00108000
LABEL UPAREA,READPART;                                                  00108100
LABEL PARERR,BACKUP,NEXT,READTO,GETLABEL,ENO;                           00108200
LABEL NOTTHERE,G1;                                                      00108300
FORMAT FFOO("DIRECTORY NOT WRITTEN CORRECTLY");                         00108400
LABEL GETNEXT,NOTOK,THRU; INTEGER J;                                    00108500
STREAM PROCEDURE INCR(V,I);VALUE I;                                     00108600
BEGIN DI~V;SI~LOC I;DI~DI+5;SI~SI+5;DS~3ADD;END INCR;                   00108700
BOOLEAN PROCEDURE COMPR(TPNAM); FILE TPNAM;                             00108800
BEGIN LABEL EN,PA; INTEGER I;                                           00108900
BOOLEAN STREAM PROCEDURE COMPARDIR(M,N,A,B); VALUE M,N;                 00109000
BEGIN LABEL EXIT;                                                       00109100
SI~A; DI~B;                                                             00109200
    M(8(IF 60 SC!DC THEN JUMP OUT 2 TO EXIT));                          00109300
    N(IF 8 SC!DC THEN JUMP OUT TO EXIT);                                00109400
    TALLY~1;                                                            00109500
EXIT: COMPARDIR~TALLY                                                   00109600
END;                                                                    00109700
    WHILE TRUE DO                                                       00109800
    PA: SPACE(TPNAM,-1)[EN:PA]; EN: CLOSE(TPNAM,*);                     00109900
    READ(TPNAM,COUNT+2,ATAPE[*])[EOF:PARI];                             00110000
    IF CODEFILE THEN                                                    00110100
    BEGIN                                                               00110200
    IF COMPARDIR((COUNT+2) DIV 60,I~((COUNT+2) MOD 60),ATAPE,CODEDIR)   00110300
    THEN COMPR~TRUE                                                     00110400
    END ELSE                                                            00110500
    BEGIN                                                               00110600
    IF COMPARDIR((COUNT+2) DIV 60,I~((COUNT+2) MOD 60),ATAPE,NEWDIR)    00110700
    THEN COMPR~TRUE                                                     00110800
    END;                                                                00110900
END;                                                                    00111000
BOOLEAN STREAM PROCEDURE SAME(A,B,N); VALUE N;                          00111100
BEGIN LABEL EXIT;                                                       00111200
    SI~A; DI~B;                                                         00111300
    N(4(IF 60 SC!DC THEN JUMP OUT 2 TO EXIT));                          00111400
    TALLY~1                                                             00111500
;                                                                       00111600
EXIT: SAME~TALLY                                                        00111700
END;                                                                    00111800
FILE OUT DISKREADDUMMYFILE1 DISK SERIAL [1:1] (1,1022);                 00111900
PROCEDURE OPEN;BEGIN STREAM PROCEDURE O(B);SI~B;                        00112000
    O(DISKREADDUMMYFILE1(0));                                           00112100
END;                                                                    00112200
BOOLEAN PROCEDURE DISKREAD(SGNO,NOSEG,D,HDR); VALUE SGNO,NOSEG;         00112300
    REAL SGNO,NOSEG; ARRAY D[0],HDR[0]; COMMENT UNPROTECTED<<<<<<<;     00112400
BEGIN DEFINE D1 = DISKREADDUMMYFILE1#;                                  00112500
    FORMAT FFF("ADDR=",I8,"NOSEG=",I8);                                 00112600
    INTEGER ADDR;                                                       00112700
STREAM PROCEDURE MOVESEG(N,A,B); VALUE N;                               00112800
BEGIN SI~A; DI~B; N(DS~30 WDS) END;                                     00112900
    INTEGER STREAM PROCEDURE ABSADDR(A); BEGIN SI~A;ABSADDR~SI END;     00113000
    STREAM PROCEDURE SETUP(F,T,B,D,A); VALUE D,B,A;                     00113100
    BEGIN COMMENT FIRST SAVE NORMAL I/O DESCRIPTOR IN T;                00113200
        SI ~ F; DI ~ T; DS ~ WDS;                                       00113300
        COMMENT NEXT MAKE "D" A DESCRIPTOR & REPLACE I/O DESC BY IT;    00113400
        SI~LOC D; DI~F; DS~WDS; DI~DI-8; DS~SET;                        00113500
        COMMENT NOW PLACE DISK ADDRESS IN (BUFF-1):A SAVE ARRAY;        00113600
        SI ~ LOC A; DI ~ B; DS ~ 8 DEC;                                 00113700
    END SETUP;                                                          00113800
    STREAM PROCEDURE RESTORE(T,F);                                      00113900
    BEGIN SI ~ T; DI ~ F; DS ~ WDS END;                                 00114000
BOOLEAN STREAM PROCEDURE CHECKDISKPARITY(A);                            00114100
BEGIN                                                                   00114200
    SI~A;                                                               00114300
    SKIP 2 SB;                                                          00114400
    IF SB THEN TALLY~0 ELSE TALLY~1;                                    00114500
    CHECKDISKPARITY~TALLY                                               00114600
END CHECKDISKPARITY;                                                    00114700
        IF (ADDR ~ SGNO DIV HDR[8])<HDR[9] THEN                         00114800
        IF  DISKREAD ~(ADDR ~ HDR[10+ADDR])!0 THEN                      00114900
BEGIN SETUP(D1,HDR[30],DISKBUF,(0&2188[15:36:12]&880[3:36:12]           00115000
 &1[2:47:1])                                                            00115100
&DISKBUF[33:33:15]&NOSEG[27:42:6],                                      00115200
                     ADDR ~ SGNO MOD HDR[8] + ADDR);                    00115300
            RELEASE(D1);                                                00115400
WAIT(ABSADDR(D1),(0&1[19:47:1]&1[2:47:1]));                             00115500
        IF(CHECKDISKPARITY(D1)) THEN                                    00115600
        BEGIN                                                           00115700
    WRITE(PRINTER,FFF,ADDR,NOSEG);                                      00115800
        DISKREAD~FALSE                                                  00115900
        END ELSE                                                        00116000
        MOVESEG(NOSEG,D1(1),D[0]);                                      00116100
            RESTORE(HDR[30],D1);                                        00116200
        END;                                                            00116300
END DISKREAD;                                                           00116400
PROCEDURE WRITEZEROFILE;                                                00116500
    BEGIN  WRITE(NEWTAPE,30,ZEROF[*]);                                  00116600
    CLOSE(NEWTAPE,*);                                                   00116700
END;                                                                    00116800
INTEGER STREAM PROCEDURE LENGTH (A);                                    00116900
BEGIN SI~A; SI~SI-8; DI~LOC LENGTH;  DS~WDS END;                        00117000
BOOLEAN PROCEDURE NOFILE;                                               00117100
BEGIN                                                                   00117200
    LABEL HOPOUT,HP;                                                    00117300
INTEGER J,TIF;                                                          00117400
IF (NEWDIR[FILD+1])<0 THEN                                              00117500
BEGIN                                                                   00117600
        NOFILE~TRUE; GO HP                                              00117700
    END;                                                                00117800
    FOR J~0 STEP 2 WHILE TRUE DO                                        00117900
    BEGIN                                                               00118000
IF REAL(BOOLEAN(DIR[J]) EQV BOOLEAN(12   )) =REAL(NOT FALSE)            00118100
        THEN BEGIN NOFILE~TRUE;                                         00118200
WRITE(PRINTER,FMT4,IF CODEFILE THEN PREFIX(CODEDIR[FILD]) ELSE          00118300
PREFIX(NEWDIR[FILD]),NEWDIR[FILD],(T~NEWDIR[FILD+1]).[6:6],T);          00118400
    GO HP END                                                           00118500
        ELSE                                                            00118600
IF REAL(BOOLEAN(NEWDIR[FILD])EQV BOOLEAN(ABS(DIR[J])))                  00118700
    =REAL(NOT FALSE) THEN                                               00118800
        IF REAL(BOOLEAN(NEWDIR[FILD+1]) EQV BOOLEAN(DIR[J+1]))          00118900
        =REAL(NOT FALSE) THEN GO HOPOUT;                                00119000
    END;                                                                00119100
HOPOUT:  Z~(FILO~(J/2)+1)-LASTFILE;                                     00119200
    RENAME(TFID,FILARR,FILO)                                            00119300
;                                                                       00119400
LASTFILE~FILO;                                                          00119500
HP:                                                                     00119600
    FNDX~FNDX+1;                                                        00119700
    RENAME(TIF,FILARR,FNDX);                                            00119800
                                                                        00119900
    FILL NEWTAPE WITH NEWTAPENAME,TIF;                                  00120000
                                                                        00120100
END;                                                                    00120200
PROCEDURE WRITEFILE;                                                    00120300
BEGIN  LABEL FIN;                                                       00120400
    READ(TAPE[NO])[FIN];                                                00120500
    N~LENGTH(TAPE(0)); READ(TAPE,N,HED[*])[FIN:PARI];                   00120600
                     WRITE(NEWTAPE,N,HED[*]);                           00120700
        WHILE TRUE DO                                                   00120800
            BEGIN                                                       00120900
            READ(TAPE[NO])[FIN];                                        00121000
            N~LENGTH(TAPE(0));                                          00121100
            READ(TAPE,N,ATAPE[*])[FIN:PARI];                            00121200
                         WRITE(NEWTAPE,N,ATAPE[*]);                     00121300
            END;                                                        00121400
FIN: END;                                                               00121500
IF MAK THEN IF ERRCOUNT=0 THEN                                          00121600
BEGIN                                                                   00121700
NOTZEROF~TRUE;                                                          00121800
        OPEN; DISKBUF~GETADD(DISKREADDUMMYFILE1(0))-1;                  00121900
    FILL NEWTAPE WITH NEWTAPENAME,FILARR[0];                            00122000
                                                                        00122100
IF CODEFILE THEN BEGIN                                                  00122200
FOR I~0 STEP 2 UNTIL COUNT DO BEGIN                                     00122300
    CODEDIR[I]~0&NEWDIR[I][6:6:42];                                     00122400
    MOVE(1,NEWDIR[I+1],CODEDIR[I+1]);                                   00122500
    END;                                                                00122600
    CODEDIR[COUNT+1]~12; WRITE(NEWTAPE,COUNT+2,CODEDIR[*]) END          00122700
ELSE BEGIN                                                              00122800
    NEWDIR[COUNT+1]~12;                                                 00122900
    WRITE(NEWTAPE,COUNT+2,NEWDIR[*]);                                   00123000
END;                                                                    00123100
    IF CHECKTAPE THEN                                                   00123200
    IF COMPR(NEWTAPE) THEN CLOSE(NEWTAPE,*) ELSE                        00123300
    BEGIN WRITE(PRINTER,FFOO); IF TWOT THEN ONET~FALSE ELSE GO EXIT     00123400
    END ELSE                                                            00123500
    CLOSE(NEWTAPE,*);                                                   00123600
WRITE(PRINTER,FMT3,(T~NEWTAPENAME).[6:6],T);                            00123700
    TAP~-1;                                                             00123800
    FNDX~0;                                                             00123900
    WHILE TAP~TAP+1 <TAPNDX DO                                          00124000
    BEGIN                                                               00124100
        STARTFIL~TAPEINFO[TAP,0]; ENDFIL~TAPEINFO[TAP,1];               00124200
        IF TAPEINFO[TAP,2]=-1 THEN                                      00124300
        BEGIN                                                           00124400
COMMENT THE FILES ARE ON DISK;                                          00124500
        FOR FILD~STARTFIL STEP 2 UNTIL ENDFIL DO                        00124600
            BEGIN                                                       00124700
    FNDX~FNDX+1;                                                        00124800
    RENAME(TFID,FILARR,FNDX);                                           00124900
                                                                        00125000
    FILL NEWTAPE WITH NEWTAPENAME,TFID;                                 00125100
                                                                        00125200
IF (NEWDIR[FILD+1])<0 THEN                                              00125300
    BEGIN                                                               00125400
    WRITEZEROFILE;                                                      00125500
    GO NEXT END;                                                        00125600
            IF DIRECTORYSEARCH(NEWDIR[FILD],NEWDIR[FILD+1],HED) THEN    00125700
    BEGIN                                                               00125800
COMMENT  TRANSFER DISK FILES;                                           00125900
WRITE(PRINTER,FMT2,IF CODEFILE THEN PREFIX(CODEDIR[FILD]) ELSE          00126000
    PREFIX(NEWDIR[FILD]), NEWDIR[FILD],(T~NEWDIR[FILD+1]).[6:6],T);     00126100
    HED[4]~HED[4]&0[2:47:1];                                            00126200
                                                                        00126300
    WRITE(NEWTAPE,30,HED[*]);                                           00126400
                                                                        00126500
        IF (SEGPERAREA~HED[8]) MOD 30 =0 THEN                           00126600
        BEGIN                                                           00126700
        COMMENT  NO. DISK SEGS PER AREA IS A MULTIPLE OF 30;            00126800
    BEGSEG~BLKRED~WHATAREA~0; NOBLKS~HED[8] DIV 30;                     00126900
        GO UPAREA;                                                      00127000
READSEG:                                                                00127100
    IF  DISKREAD(BEGSEG,30,INFO,HED)  THEN                              00127200
    BEGIN                                                               00127300
                                                                        00127400
        WRITE(NEWTAPE,900,INFO[*]);                                     00127500
                                                                        00127600
            BEGSEG~BEGSEG+30;                                           00127700
        IF (BLKRED~BLKRED+1) < NOBLKS THEN                              00127800
            GO READSEG;                                                 00127900
COMMENT COMPARE HIGHEST LOGICAL RECORD NUMBER WRITTEN  IN               00128000
        WITH  NO. LOGICAL RECORDS WRITTEN ON TAPE;                      00128100
    END ELSE GO PARERR;                                                 00128200
UPAREA:                                                                 00128300
            WHILE (WHATAREA~WHATAREA+1){20 DO                           00128400
        BEGIN                                                           00128500
        IF HED[WHATAREA+9          ]=0 THEN                             00128600
        BEGSEG~BEGSEG+SEGPERAREA ELSE                                   00128700
        BEGIN BLKRED~0;                                                 00128800
        GO READSEG;                                                     00128900
        END                                                             00129000
        END; GO FINTAPE;                                                00129100
        END ELSE                                                        00129200
        BEGIN                                                           00129300
COMMENT NO. DISK SEGS PER AREA NOT A MULTIPLE OF 30;                    00129400
        BEGSEG~BLKRED~WHATAREA~0;                                       00129500
IF HED[10]!0 THEN BEGIN WHATAREA~1;                                     00129600
        IF(NOBLKS~HED[8] DIV 30) ! 0                                    00129700
        THEN                                                            00129800
        BEGIN                                                           00129900
REDSEG:                                                                 00130000
    IF  DISKREAD(BEGSEG,30,INFO,HED) THEN                               00130100
    BEGIN                                                               00130200
                                                                        00130300
        WRITE(NEWTAPE,900,INFO[*]);                                     00130400
                                                                        00130500
        BEGSEG~BEGSEG+30;                                               00130600
        IF (BLKRED~BLKRED+1) < NOBLKS THEN GO REDSEG;                   00130700
    END ELSE GO PARERR;                                                 00130800
         END;                                                           00130900
READPART:                                                               00131000
    IF DISKREAD(BEGSEG,LEFTOVER~ HED[8] MOD 30, INFO, HED) THEN         00131100
BEGIN IF ONET THEN                                                      00131200
    WRITE(NEWTAPE,LEFTOVER|30,INFO[*]);                                 00131300
                                                                        00131400
    END ELSE GO PARERR;                                                 00131500
        BEGSEG~BEGSEG+LEFTOVER;                                         00131600
END;                                                                    00131700
        WHILE(WHATAREA~WHATAREA+1){20 DO                                00131800
    BEGIN                                                               00131900
    IF HED[ WHATAREA + 9        ]  =0      THEN                         00132000
        BEGSEG~BEGSEG+SEGPERAREA ELSE                                   00132100
    BEGIN BLKRED~0;                                                     00132200
        IF NOBLKS!0 THEN GO REDSEG ELSE GO READPART END;                00132300
        END;                                                            00132400
FINTAPE:                                                                00132500
    CLOSE(NEWTAPE,*);                                                   00132600
    END END  ELSE                                                       00132700
    BEGIN  WRITEZEROFILE;                                               00132800
        WRITE(PRINTER,FMT4,PREFIX(NEWDIR[FILD]), NEWDIR[FILD],          00132900
        (T~NEWDIR[FILD+1]).[6:6],T)                                     00133000
    END      ;                                                          00133100
NEXT:                                                                   00133200
    END;                                                                00133300
    IF CHECKTAPE THEN                                                   00133400
BEGIN                                                                   00133500
    IF TAP=0 THEN                                                       00133600
    BEGIN REWIND(NEWTAPE); SPACE(NEWTAPE,1)[FINN:PARZ];                 00133700
    CLOSE(NEWTAPE,*);                                                   00133800
    END ELSE                                                            00133900
    BEGIN UNLABEL(NEWTAPE);                                             00134000
        J~(ENDFIL+1-STARTFIL)DIV 2+1;                                   00134100
        FOR I~1 STEP 1 UNTIL J DO                                       00134200
        BEGIN                                                           00134300
                WHILE TRUE DO                                           00134400
            PA2:          SPACE(NEWTAPE,-1)[EN2:PA2];  % GET OVER LABEL 00134500
            EN2:CLOSE(NEWTAPE,*);                                       00134600
               WHILE TRUE DO                                            00134700
            PA3:          SPACE(NEWTAPE,-1000)[EN3:PA3]; % GET OVER FILE00134800
            EN3:CLOSE(NEWTAPE,*);                                       00134900
        END;                                                            00135000
SPACE(NEWTAPE,1)[EN4:PARZ]; EN4: CLOSE(NEWTAPE,*);                      00135100
        LABELF(NEWTAPE);                                                00135200
SPACE(NEWTAPE,1)[EN5:PARZ];  EN5: CLOSE(NEWTAPE,*);                     00135300
END;                                                                    00135400
        FOR FILD~STARTFIL STEP 2 UNTIL ENDFIL DO                        00135500
        BEGIN                                                           00135600
            IF(NEWDIR[FILD+1])<0 THEN                                   00135700
NOTTHERE:   BEGIN                                                       00135800
            BEGIN SPACE(NEWTAPE,1)[FINN:PARZ];CLOSE(NEWTAPE,*); END;    00135900
    END ELSE BEGIN                                                      00136000
        IF DIRECTORYSEARCH(NEWDIR[FILD], NEWDIR[FILD+1],HED) THEN       00136100
        BEGIN   SEGPERAREA~HED[8];                                      00136200
                                                                        00136300
                                                                        00136400
    STARTSEG~0;                                                         00136500
    NOTZEROF~TRUE;                                                      00136600
        READ(NEWTAPE,30,ATAPE[*])[FINN:PARQ];                           00136700
        IF NOT SAME(HED,ATAPE,1) THEN                                   00136800
PARQ:                                                                   00136900
        BEGIN WRITE(PRINTER,FFO);                                       00137000
CLOSE(NEWTAPE);                                                         00137100
 GO EXIT                                                                00137200
        END;                                                            00137300
                                                                        00137400
WHILE TRUE DO                                                           00137500
BEGIN                                                                   00137600
    READ(NEWTAPE[NO])[FINN:PARZ];                                       00137700
    NSEG~(N~LENGTH(NEWTAPE(0))) DIV 30;                                 00137800
    READ(NEWTAPE,N,ATAPE[*])[FINN:PARZ];                                00137900
    IF(STARTSEG MOD SEGPERAREA)=0 THEN                                  00138000
    BEGIN  TRYA:                                                        00138100
        IF HED[10+WHATAREA~(STARTSEG DIV SEGPERAREA)]=0 THEN            00138200
        BEGIN STARTSEG~STARTSEG+SEGPERAREA;                             00138300
            IF WHATAREA<HED[9]-1 THEN GO TRYA  END END;                 00138400
            IF DISKREAD(STARTSEG,NSEG,INFO,HED) THEN                    00138500
            BEGIN IF NOT SAME(INFO,ATAPE,NSEG) THEN                     00138600
PARZ:BEGIN WRITE(PRINTER,FFO);                                          00138700
            CLOSE(NEWTAPE); GO EXIT;                                    00138800
        END ELSE STARTSEG~STARTSEG+NSEG                                 00138900
        END ELSE GO PARERR;                                             00139000
END END ELSE GO NOTTHERE END;FINN:CLOSE(NEWTAPE,*);END;END;             00139100
        END ELSE                                                        00139200
        BEGIN                                                           00139300
COMMENT THE FILES ARE ON TAPE;                                          00139400
    LASTFILE~0;                                                         00139500
    FILL TAPE WITH TAPEINFO[TAP,2],FILARR[0];                           00139600
    READ(TAPE,1023,DIR[*])[ENOF:PARI];                                  00139700
    CLOSE(TAPE,*);                                                      00139800
    J~-2;                                                               00139900
    FOR K~STARTFIL STEP 2 UNTIL ENDFIL DO                               00140000
    BEGIN IF NEWDIR[K+1]<0 THEN                                         00140100
BEGIN                                                                   00140200
          DIR[J~J+2]~-1; GO THRU END;                                   00140300
WHILE DIR[J~J+2]!12 DO                                                  00140400
        IF REAL(BOOLEAN(NEWDIR[K])EQV BOOLEAN(DIR[J]))=REAL(NOT FALSE)  00140500
        THEN IF REAL(BOOLEAN(NEWDIR[K+1]) EQV BOOLEAN(DIR[J+1]))=       00140600
        REAL(NOT FALSE) THEN                                            00140700
BEGIN                                                                   00140800
              DIR[J]~-DIR[J]; GO THRU END; GO NOTOK;                    00140900
THRU: END;                                                              00141000
COMMENT  FILES ARE IN ORDER ON THE TAPE;                                00141100
BEGIN                                                                   00141200
INTEGER LAST,J,CNT,MAXFI; REAL FFF;      LABEL EOF,NXT;                 00141300
CNT~0; MAXFI~(ENDFIL+1-STARTFIL)/2;                                     00141400
RENAME(FFF,FILARR,FNDX);                                                00141500
LAST~ -2;                                                               00141600
FOR J~0 STEP 2 WHILE CNT<MAXFI DO                                       00141700
    IF DIR[J]<0 THEN                                                    00141800
    BEGIN  CNT~CNT+1;                                                   00141900
    WHILE J-LAST>2 DO                                                   00142000
    BEGIN SPACE(TAPE,1)[ENOF:PARI];                                     00142100
            CLOSE(TAPE,*); LAST~LAST+2;                                 00142200
    END;                                                                00142300
    INCR(FFF,1); FNDX~FNDX+1;                                           00142400
                 FILL NEWTAPE WITH     NEWTAPENAME,FFF;                 00142500
IF DIR[J]=-1 THEN                                                       00142600
BEGIN SPACE(TAPE,1); WRITE(NEWTAPE,30,ZEROF[*]);                        00142700
END ELSE                                                                00142800
BEGIN                                                                   00142900
WRITEFILE;                                                              00143000
WRITE(PRINTER,FMT2,PREFIX(ABS(DIR[J])),DIR[J],(T~DIR[J+1]).[6:6],T);    00143100
END;                                                                    00143200
CLOSE(TAPE,*);                                                          00143300
                 CLOSE(NEWTAPE,*);                                      00143400
    LAST~J;                                                             00143500
    END;                                                                00143600
IF CHECKTAPE THEN                                                       00143700
BEGIN                                                                   00143800
BOOLEAN STREAM PROCEDURE CK(T1,T2);                                     00143900
BEGIN LOCAL M,N;                                                        00144000
LABEL XT;                                                               00144100
    TALLY~1;                                                            00144200
    SI~T1;SI~SI-2;DI~LOC M;DS~7LIT"0"; DS~CHR;DS~7LIT"0";               00144300
    DS~CHR;                                                             00144400
SI~T1; DI~T2;                                                           00144500
    SI~SI-8; DI~DI-8;                                                   00144600
    IF 8SC!DC THEN GO XT;                                               00144700
    M(2(32(IF 8SC!DC THEN JUMP OUT 3 TO XT)));                          00144800
N(IF 8SC!DC THEN JUMP OUT TO XT);                                       00144900
TALLY~0;                                                                00145000
XT:CK~TALLY;                                                            00145100
END CK;                                                                 00145200
FORMAT                                                                  00145300
    FOR1("READ REVERSE ERROR EOF ON NEWTAPE 1"),                        00145400
    FOR2("READ REV PAR ERROR ON NEWTAPE 1"),                            00145500
    FOR3("ORIG TAPE AND NEWTAPE 1 DO NOT CHECK"),                       00145600
    FOR4("READ REV EOF ERROR ON ORIG TAPE"),                            00145700
    FOR5("READ REV PAR ERROR ON ORIG TAPE");                            00145800
    LABEL P1,P2,P3,P4,P6,P7,P8,P10,P11,                                 00145900
          E1,E2,E3,E4,E6,E7,E8,E10,E11,                                 00146000
          EOF,NXT;                                                      00146100
LABEL E12;                                                              00146200
    J~LAST; LAST~LAST+2;                                                00146300
    UNLABEL(NEWTAPE); UNLABEL(TAPE);                                    00146400
    FOR J~J STEP -2 UNTIL 0 DO                                          00146500
    IF DIR[J]<0 THEN                                                    00146600
    BEGIN                                                               00146700
        WHILE LAST-J>2 DO                                               00146800
        BEGIN                                                           00146900
        P1: WHILE TRUE DO SPACE(TAPE,-1)[E1:P1]; E1:CLOSE(TAPE,*);      00147000
        P2: WHILE TRUE DO SPACE(TAPE,-1000)[E2:P2]; E2: CLOSE(TAPE,*);  00147100
LAST~LAST-2;                                                            00147200
        END;                                                            00147300
        P3: WHILE TRUE DO SPACE(TAPE,-1)[E3:P3]; E3: CLOSE(TAPE,*);     00147400
        P4:WHILE TRUE DO SPACE(NEWTAPE,-1)[E4:P4];E4:CLOSE(NEWTAPE,*);  00147500
IF DIR[J]=-1 THEN                                                       00147600
BEGIN                                                                   00147700
WHILE TRUE DO SPACE(TAPE,-1000)[E12:P7];                                00147800
E12:                                                                    00147900
WHILE TRUE DO SPACE(NEWTAPE,-1)[EOF:P8];                                00148000
END;                                                                    00148100
NXT:                                                                    00148200
    READ REVERSE(TAPE[NO])[EOF:NXT];                                    00148300
    READ REVERSE(NEWTAPE[NO])[E8:P8];                                   00148400
    IF CK(TAPE(0),NEWTAPE(0)) THEN GO E6;                               00148500
    READ REVERSE(TAPE)[E7:P7 ];                                         00148600
    READ REVERSE(NEWTAPE)[E8:P8];                                       00148700
    GO TO NXT;                                                          00148800
EOF:                                                                    00148900
    CLOSE(NEWTAPE,*); CLOSE(TAPE,*);                                    00149000
    LAST~J;                                                             00149100
    END;                                                                00149200
    CLOSE(TAPE);                                                        00149300
LABELF(TAPE);                                                           00149400
        P10: SPACE(NEWTAPE,1)[E10:P10]; E10: CLOSE(NEWTAPE,*);          00149500
        LABELF(NEWTAPE);                                                00149600
    IF TAP+1<TAPNDX THEN                                                00149700
    BEGIN                                                               00149800
        FOR J~1 STEP 1 UNTIL CNT DO                                     00149900
        BEGIN                                                           00150000
        SPACE(NEWTAPE,1)[E11:PARI]; E11: CLOSE(NEWTAPE,*);              00150100
        END;                                                            00150200
    END;                                                                00150300
GO GETNEXT;                                                             00150400
E8:                                                                     00150500
    WRITE(PRINTER,FOR1);  GO EXIT;                                      00150600
P8: WRITE(PRINTER,FOR2);  GO EXIT;                                      00150700
E6: WRITE(PRINTER,FOR3);                                                00150800
    GO EXIT;                                                            00150900
E7: WRITE(PRINTER,FOR4);  GO EXIT;                                      00151000
P7: WRITE(PRINTER,FOR5); GO EXIT;                                       00151100
END ELSE  CLOSE(TAPE);         %  DONT  CHECK TAPE                      00151200
GO GETNEXT;                                                             00151300
END; NOTOK: UNLABEL(TAPE);                                              00151400
    FILD~STARTFIL;                                                      00151500
FNDX~FNDX+1;RENAME(TFID,FILARR,FNDX);                                   00151600
FILL NEWTAPE WITH NEWTAPENAME,TFID;                                     00151700
    IF NOFILE THEN                                                      00151800
    WRITEZEROFILE                                                       00151900
        ELSE                                                            00152000
    BEGIN                                                               00152100
    IF Z>1  THEN                                                        00152200
    BEGIN                                                               00152300
        K~1 ;                                                           00152400
READTO:                                                                 00152500
WHILE TRUE DO SPACE(TAPE,1)[E1:PARI];                                   00152600
E1:                                                                     00152700
        CLOSE(TAPE,*);                                                  00152800
WHILE TRUE DO SPACE(TAPE,100)[E2:PARI];                                 00152900
E2:                                                                     00153000
        CLOSE(TAPE,*);  COMMENT PAST FILE AND TM;                       00153100
        SPACE(TAPE,1)[ENOF:PARI];                                       00153200
        IF(K~K+1)< Z  THEN GO READTO;                                   00153300
    END;                                                                00153400
  GETLABEL:                                                             00153500
        READ(TAPE,10,DUMMY[*])[ENOF:PARI]; CLOSE(TAPE,*);               00153600
        IF LABELOK(DUMMY,TAPEINFO[TAP,2],TFID) THEN                     00153700
BEGIN WRITEFILE;                                                        00153800
    IF NOT ZEROHED(HED,ZEROF) THEN                                      00153900
        WRITE(PRINTER,FMT2,IF CODEFILE THEN PREFIX(CODEDIR[FILD])       00154000
        ELSE PREFIX(NEWDIR[FILD]),NEWDIR[FILD],(T~NEWDIR[FILD+1]).[6:6],00154100
        T);                                                             00154200
IF CHECKTAPE THEN                                                       00154300
IF NOTZEROF THEN                                                        00154400
BEGIN                                                                   00154500
    NOTZEROF~TRUE;                                                      00154600
                                                                        00154700
    BEGIN  WHILE TRUE DO                                                00154800
        PARRR: SPACE(NEWTAPE,-1000)[ENDD:PARRR];                        00154900
        ENDD: CLOSE(NEWTAPE,*);                                         00155000
    END;                                                                00155100
    CLOSE(TAPE,*); SPACE(TAPE,-1)[EE:PARI]; EE: CLOSE(TAPE,*);          00155200
    WHILE TRUE DO                                                       00155300
    PP: SPACE(TAPE,-100)[ENDT:PP]; ENDT:                                00155400
CLOSE(TAPE,*);SPACE(TAPE,1)[EEE:PARI]; EEE: CLOSE(TAPE,*);              00155500
    WHILE TRUE DO                                                       00155600
    BEGIN                                                               00155700
            READ(TAPE[NO])[FINNT:PARI];                                 00155800
    NSEGT~(NTT~LENGTH(TAPE(0))) DIV 30;                                 00155900
    READ(TAPE,NTT,ATTAPE[*])[FINNT:PARI];                               00156000
                                                                        00156100
        BEGIN                                                           00156200
            READ(NEWTAPE[NO])[FINNT:NZ];                                00156300
            NSEG~(N~LENGTH(NEWTAPE(0))) DIV 30;                         00156400
            READ(NEWTAPE,N,ATAPE[*])[FINNT:NZ];                         00156500
        END;                                                            00156600
                                                                        00156700
QTT:                                                                    00156800
    BEGIN IF NTT=N THEN                                                 00156900
BEGIN                                                                   00157000
        IF NOT SAME(ATTAPE,ATAPE,NSEG) THEN                             00157100
    BEGIN NS: WRITE(PRINTER,FFFO);                                      00157200
CLOSE(NEWTAPE);                                                         00157300
    GO EXIT                                                             00157400
    END END ELSE                                                        00157500
BEGIN GO NS;  NZ: WRITE(PRINTER,FFFO);                                  00157600
    CLOSE(NEWTAPE); GO EXIT;   END;                                     00157700
    END;                                                                00157800
    END;                                                                00157900
END ELSE NOTZEROF~TRUE;                                                 00158000
FINNT:  IF ONET THEN CLOSE(NEWTAPE,*);                                  00158100
                                                                        00158200
    CLOSE(TAPE,*);                                                      00158300
    END ELSE                                                            00158400
GO PARI;                                                                00158500
        END;                                                            00158600
GETFIL:                                                                 00158700
        IF FILD~FILD+2<ENDFIL THEN                                      00158800
    BEGIN                                                               00158900
FNDX~FNDX+1; RENAME(TFID,FILARR,FNDX);                                  00159000
FILL NEWTAPE WITH NEWTAPENAME,TFID;                                     00159100
    IF NOFILE THEN WRITEZEROFILE                                        00159200
        ELSE                                                            00159300
        BEGIN                                                           00159400
        IF Z>0 THEN                                                     00159500
            BEGIN IF (TEMP~Z-1)>0 THEN                                  00159600
            BEGIN                                                       00159700
            FOR K~1 STEP 1 UNTIL TEMP DO                                00159800
            BEGIN                                                       00159900
WHILE TRUE DO SPACE(TAPE,1)[E3:PARI];                                   00160000
E3:                                                                     00160100
            CLOSE(TAPE,*);                                              00160200
WHILE TRUE DO SPACE(TAPE,100)[E4:PARI];                                 00160300
E4:                                                                     00160400
            CLOSE(TAPE,*);                                              00160500
            END                                                         00160600
            END;                                                        00160700
            SPACE(TAPE,1) [ENOF:PARI ];                                 00160800
            GO  GETLABEL;                                               00160900
            END ELSE                                                    00161000
    IF Z{0 THEN                                                         00161100
            BEGIN                                                       00161200
            Z~ABS(Z);                                                   00161300
            K~0;                                                        00161400
        READ REVERSE(TAPE,10,DUMMY[*])[ENO:PARI];                       00161500
ENO: CLOSE(TAPE,*);    COMMENT GET PAST TM;                             00161600
BACKUP:                                                                 00161700
WHILE TRUE DO SPACE(TAPE,-100)[E5:PARI];                                00161800
E5:                                                                     00161900
            CLOSE(TAPE,*);                                              00162000
            COMMENT GET PAST FILE;                                      00162100
            IF(K~K+1){ Z        THEN                                    00162200
            BEGIN                                                       00162300
WHILE TRUE DO SPACE(TAPE,-1)[E6:PARI];                                  00162400
E6:                                                                     00162500
            CLOSE(TAPE,*);                                              00162600
            GO BACKUP                                                   00162700
            END;                                                        00162800
            SPACE(TAPE,-1)[ENOF:PARI];                                  00162900
            GO GETLABEL;                                                00163000
            END;                                                        00163100
    END;                                                                00163200
GO GETFIL;                                                              00163300
END                                                                     00163400
;                                                                       00163500
CLOSE(TAPE,RELEASE);                                                    00163600
LABELF(TAPE);                                                           00163700
END;                                                                    00163800
GETNEXT:                                                                00163900
END;                                                                    00164000
IF TWOT THEN BEGIN  REWIND(NEWTAPE);                                    00164100
BEGIN FILE OUT NEWTAP2 9(1,1023);                                       00164200
BOOLEAN STREAM PROCEDURE CK(T1,T2);                                     00164300
BEGIN LOCAL M,N;                                                        00164400
LABEL XT;                                                               00164500
    TALLY~1;                                                            00164600
    SI~T1;SI~SI-2;DI~LOC M;DS~7LIT"0"; DS~CHR;DS~7LIT"0";               00164700
    DS~CHR;                                                             00164800
SI~T1; DI~T2;                                                           00164900
    SI~SI-8; DI~DI-8;                                                   00165000
    IF 8SC!DC THEN GO XT;                                               00165100
    M(2(32(IF 8SC!DC THEN JUMP OUT 3 TO XT)));                          00165200
N(IF 8SC!DC THEN JUMP OUT TO XT);                                       00165300
TALLY~0;                                                                00165400
XT:CK~TALLY;                                                            00165500
END CK;                                                                 00165600
FORMAT FOR6("PROBLEM WITH NEWTAPE 1"),                                  00165700
    FOR7("PROBLEM WITH NEWTAPE 2");                                     00165800
    ARRAY A[0:1022];                                                    00165900
    INTEGER TM,L;  LABEL L1,L2,EOF1,RDLBL,RDRCD,EOF,EOT,NXT,EOF2,DONE;  00166000
    DEFINE LOOPIT=L~LENGTH(NEWTAPE(0));                                 00166100
    READ(NEWTAPE,L,A[*])[ L1:L1   ]; WRITE(NEWTAP2,L,A[*])#,            00166200
    CLOSETHEM=CLOSE(NEWTAP2,*); CLOSE(NEWTAPE,*)#;                      00166300
    FILL NEWTAPE WITH NEWTAPENAME,"FILE000";                            00166400
    READ(NEWTAPE)[EOF1:L1   ];                                          00166500
    EOF1:  UNLABEL(NEWTAPE); REWIND(NEWTAPE); WHEN(3);                  00166600
    RDLBL: READ(NEWTAPE[NO])[EOT: L1 ];LOOPIT;CLOSETHEM; TM~TM+1;       00166700
    RDRCD: READ(NEWTAPE[NO])[EOF: L1 ];LOOPIT;  GO TO RDRCD;            00166800
    EOF:CLOSETHEM;TM~TM+1; LOOPIT; GO TO RDLBL;                         00166900
    EOT: CLOSETHEM;                                                     00167000
    IF CHECKTAPE THEN                                                   00167100
    BEGIN                                                               00167200
        READ REVERSE(NEWTAP2[NO])[NXT: L2   ];                          00167300
        FOR TM~TM STEP-1 UNTIL 0 DO                                     00167400
        BEGIN                                                           00167500
        NXT: READ REVERSE(NEWTAPE[NO])[EOF2:L1 ];                       00167600
    READ REVERSE(NEWTAP2[NO])[L2:L2 ];                                  00167700
    IF CK(NEWTAPE(0),NEWTAP2(0)) THEN GO L2;                            00167800
    READ REVERSE(NEWTAP2)[L2:L2 ];                                      00167900
    READ REVERSE(NEWTAPE)[L1:L1 ];                                      00168000
    GO TO NXT;                                                          00168100
        EOF2: CLOSETHEM;                                                00168200
        END;                                                            00168300
        REWIND(NEWTAPE); REWIND(NEWTAP2);                               00168400
        READ(NEWTAPE[NO])[ L1:L1   ];                                   00168500
        READ(NEWTAP2[NO])[ L2:L2   ];                                   00168600
        IF CK(NEWTAPE(0),NEWTAP2(0)) THEN GO L2;                        00168700
    END;  LOCK(NEWTAPE,RELEASE);  LOCK(NEWTAP2,RELEASE); GO DONE;       00168800
L1:     WRITE(PRINTER,FOR6); ONET~TWOT~FALSE;                           00168900
                                                  GO DONE;              00169000
L2:     WRITE(PRINTER,FOR7); TWOT~FALSE;                                00169100
DONE:                                                                   00169200
END                                                                     00169300
END ELSE  LOCK(NEWTAPE,RELEASE);                                        00169400
                                                                        00169500
END;                                                                    00169600
GO EXIT;                                                                00169700
PARERR: ERROR(18);                                                      00169800
END MAKTAPE;                                                            00169900
ENOF:                                                                   00170000
EOF:                                                                    00170100
ERROR(17);                                                              00170200
PAR:                                                                    00170300
PARI: ERROR(18);                                                        00170400
ERR:                                                                    00170500
EXIT:                                                                   00170600
WRITE(PRINT[PAGE]);                                                     00170700
FOR NPR~0 STEP 1 UNTIL NPRIN DO                                         00170800
WRITE(PRINT,15,PRIN[NPR,*]);                                            00170900
END.                                                                    00171000
                LAST CARD ON CRDIMG TAPE                                99999999
