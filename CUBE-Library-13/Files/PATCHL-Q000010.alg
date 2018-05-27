$ + STREAM PRT                                                          00000000
BEGIN % PATCHES/LISTER.......WRL        L.P.MCQUOWN.....                00001000
INTEGER COMMON;                                                         00002000
FILE CARD (1,10,150);                                                   00003000
FILE LINE 6(1,15);                                                      00004000
BOOLEAN PONLY,EXCEPTIONS;                                               00005000
INTEGER WD,COL,LNG,I;                                                   00006000
INTEGER REC;                                                            00007000
ARRAY LASTA[0:9],LYNE[0:14];                                            00008000
STREAM PROCEDURE MOVEPAT(A,L,CM64,CM1,LWD,LNG);                         00009000
    VALUE CM64,CM1,LNG;                                                 00010000
BEGIN                                                                   00011000
    SI~A; DI~L;                                                         00012000
    CM64(DS~8 WDS); DS~CM1 CHR;                                         00013000
    DI~LWD; DI~DI-LNG; DS~LNG CHR;                                      00014000
    DI~DI+4; DS~16 CHR;                                                 00015000
END;                                                                    00016000
STREAM PROCEDURE RECN(R,A); VALUE R;                                    00017000
BEGIN SI~LOC R; DI~A; DS~8 DEC; DI~DI-8; DS~7 FILL END;                 00018000
BOOLEAN STREAM PROCEDURE PERCENT(A,B); VALUE B;                         00019000
    BEGIN SI~A; SI~SI+B; IF SC="%" THEN TALLY~1; PERCENT~TALLY END;     00020000
BOOLEAN STREAM PROCEDURE EQUAL(A,B,C,D); VALUE C,D;                     00021000
BEGIN SI~A; SI~SI+D; DI~B; DI~DI+D; IF C SC=DC THEN TALLY~1;            00022000
    EQUAL~TALLY END;                                                    00023000
STREAM PROCEDURE BLANK(N,A); VALUE N;                                   00024000
    BEGIN DI~A; DS~8 LIT " "; SI~A; DS~N WDS END;                       00025000
BOOLEAN STREAM PROCEDURE COMP(A,B,C,D); VALUE C,D;                      00026000
    BEGIN SI~A; SI~SI+D; DI~B; DI~DI+D; IF C SC<DC THEN                 00027000
        TALLY~1; COMP~TALLY END;                                        00028000
STREAM PROCEDURE MOVE(N,A,B); VALUE N;                                  00029000
    BEGIN SI~A; DI~B; DS~N WDS END;                                     00030000
PROCEDURE HIV(A); ARRAY A[0];                                           00031000
 FOR I~0 STEP 1 UNTIL 9 DO A[I] ~ "99999999";                           00032000
BOOLEAN PROCEDURE INPUT(A); ARRAY A[0];                                 00033000
BEGIN                                                                   00034000
    LABEL MAHJONGG,HI;                                                  00035000
HI: READ(CARD,10,A[*])[MAHJONGG];                                       00036000
    RECN(REC,A[10]);                                                    00037000
    REC ~ REC + 1;                                                      00038000
    IF PONLY THEN                                                       00039000
        IF NOT PERCENT(A[WD],COL) THEN                                  00040000
            IF EXCEPTIONS THEN WRITE(LINE,10,A[*]) ELSE                 00041000
            GO HI;                                                      00042000
    IF FALSE THEN MAHJONGG: INPUT ~ TRUE;                               00043000
END INPUT;                                                              00044000
PROCEDURE OUTPUT(LAST,A); VALUE LAST; BOOLEAN LAST; ARRAY A[0];         00045000
BEGIN                                                                   00046000
    IF NOT LAST THEN BEGIN                                              00047000
        BLANK(14,LYNE);                                                 00048000
    MOVEPAT(A,LYNE,(COMMON-1).[36:6],COMMON-1,LYNE[10],LNG);            00049000
        IF NOT EQUAL(A[WD],LASTA[WD],LNG,COL) THEN BEGIN                00050000
            WRITE(LINE[PAGE]);                                          00051000
            MOVE(10,A,LASTA);                                           00052000
        END;                                                            00053000
        WRITE(LINE,15,LYNE[*]);                                         00054000
    END;                                                                00055000
END;                                                                    00056000
BOOLEAN PROCEDURE COMPARE(A,B); ARRAY A,B[0];                           00057000
    COMPARE ~ COMP(A[WD],B[WD],LNG+8,COL);                              00058000
    IF EXCEPTIONS ~ COMMON } 100 THEN COMMON ~ COMMON MOD 100;          00059000
    IF COMMON ! 0 THEN BEGIN                                            00060000
        PONLY ~ TRUE;                                                   00061000
        WD ~ (COMMON-1).[41:4];                                         00062000
        COL ~ (COMMON-1).[45:3];                                        00063000
        LNG ~ 73 - COMMON;                                              00064000
    END ELSE BEGIN                                                      00065000
        COMMON ~ 69;                                                    00066000
        WD ~ 8;                                                         00067000
        COL ~ 4;                                                        00068000
        LNG ~ 4;                                                        00069000
    END;                                                                00070000
    IF COMMON < 0 THEN PONLY ~ FALSE;                                   00071000
    SORT(OUTPUT,INPUT,0,HIV,COMPARE,11,5000,200000);                    00072000
END.                                                                    00073000
