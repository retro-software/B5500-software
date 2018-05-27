          BEGIN                                                         00000100
COMMENT        CUBELIB/LISTER LISTS THE DISK FILE CUBELIB/INDEX.        00000200
               TWO LINES OF DISK FILE PRODUCE EACH LINE OF OUTPUT.      00000300
               A | IN COLUMN 61 OF THE FIRST CARD OF A PAIR IS USED     00000400
               TO FLAG HEADING LINES AND BLANK LINES.                   00000500
               LIST IS DESIGNED TO BE MAINTAINED USING REMOTE/CARD AND  00000600
               MAY BE RESEQUENCED AT ANY TIME WITHOUT NOTICE.           00000700
               WITH COMMON=0 IT PRINTS FULL LIST WITH CATEGORY HEADINGS.00000800
               WITH COMMON=1 THE CATEGORY HEADINGS ARE OMITTED.         00000900
               WITH COMMON=2 THE LIST IS SORTED BY PROGRAM NAME         00001000
               WITH COMMON=3 THE FILE NAMES ARE PUNCHED FOR USE         00001100
                  BY LIBTAPE/MAKER;                                     00001200
COMMENT        CUBE LIBRARY VERSION 11/24/68                            00001300
               CUBE LIBRARY NUMBER Q000007                              00001400
               SOURCE FILE NAME CUBELST/Q000007                         00001500
               PROGRAM NAME CUBELIB/LISTER;                             00001600
INTEGER        COMMON;                                                  00001700
FILE IN        CUBE DISK SERIAL "CUBELIB""INDEX"(2,10,30);              00001800
FILE OUT       LINE 4(2,15);                                            00001900
FILE OUT       PCH 0(2,10);                                             00002000
FILE           TEMP DISK SERIAL[20:100]"CUBE""TEMP"(2,19,190);          00002100
ALPHA ARRAY    A,B[0:9],C[0:14],D[0:18];                                00002200
INTEGER        I,LCTR,PG;                                               00002300
LABEL          FULLLIST,LISTPROGS,SORTLIST,LAST,SORTIT,PUNCHNAMES;      00002400
FORMAT         HDG(X30,"LISTING OF CUBE LIBRARY",X20,"PAGE",I4//        00002500
               X13,"NAME/CUBE ID #",X3,"TITLE",X10,"DESCRIPTION",       00002600
               X29,"LANG",X2,"DATE",X5,"INST",X6,"AUTHOR"/);            00002700
FORMAT         F1(X10,8A6,A2,A1),                                       00002800
               F2(10A6),                                                00002900
               F3(X10,8A6,A2,10A6);                                     00003000
FORMAT         FP1(X10,2A6,A3),                                         00003100
               FP2(X10,2A6,A3,",");                                     00003200
PROCEDURE      NEWPAGE;                                                 00003300
          BEGIN                                                         00003400
               PG~PG+1;                                                 00003500
               LCTR~0;                                                  00003600
               WRITE(LINE[PAGE]);                                       00003700
               WRITE(LINE,HDG,PG);                                      00003800
          END;                                                          00003900
STREAM PROCEDURE MV1(A,B,C);                                            00004000
          BEGIN                                                         00004100
               SI~A;DI~C;DS~60 CHR;SI~B;DS~60 CHR;                      00004200
          END;                                                          00004300
STREAM PROCEDURE MV2(A,B,C);                                            00004400
          BEGIN                                                         00004500
               SI~A;DI~C;DS~60 CHR;SI~B;DS~60 CHR;                      00004600
          END;                                                          00004700
BOOLEAN STREAM PROCEDURE FLAG(A);                                       00004800
          BEGIN                                                         00004900
               SI~A;SI~SI+60;IF SC="|"THEN TALLY~1;FLAG~TALLY;          00005000
          END;                                                          00005100
PROCEDURE      HIVAL(A);                                                00005200
ARRAY          A[0];                                                    00005300
               A[0]~500000000000;                                       00005400
BOOLEAN PROCEDURE CMP(A,B);                                             00005500
ARRAY          A,B[0];                                                  00005600
          BEGIN                                                         00005700
LABEL          TR,RETURN;                                               00005800
               FOR I~0,1 DO                                             00005900
          BEGIN                                                         00006000
               IF A[I]<B[I] THEN GO TO TR;                              00006100
               IF A[I]>B[I] THEN GO TO RETURN;                          00006200
          END;                                                          00006300
TR:            CMP~TRUE;                                                00006400
RETURN:   END  CMP;                                                     00006500
PROCEDURE      OUTPRC(BOOL,AA);                                         00006600
VALUE          BOOL;                                                    00006700
BOOLEAN        BOOL;                                                    00006800
ARRAY          AA[0];                                                   00006900
          BEGIN                                                         00007000
               IF BOOL THEN CLOSE(TEMP) ELSE                            00007100
          BEGIN                                                         00007200
               WRITE(LINE,F3,FOR I~0 STEP 1 UNTIL 18 DO AA[I]);         00007300
               LCTR~LCTR+1; IF LCTR>48 THEN NEWPAGE;                    00007400
          END;                                                          00007500
          END;                                                          00007600
BOOLEAN PROCEDURE INPRC(AA);                                            00007700
ARRAY          AA[0];                                                   00007800
          BEGIN                                                         00007900
LABEL          RETURN,LAST;                                             00008000
               READ(TEMP,19,AA[*])[LAST];                               00008100
               GO TO RETURN;                                            00008200
LAST:          INPRC~TRUE;                                              00008300
RETURN:   END;                                                          00008400
               IF COMMON!3 THEN                                         00008500
               NEWPAGE;                                                 00008600
               IF COMMON=0 THEN GO TO FULLLIST;                         00008700
               IF COMMON=1 THEN GO TO LISTPROGS;                        00008800
               IF COMMON=2 THEN GO TO SORTLIST;                         00008900
               IF COMMON=3 THEN GO TO PUNCHNAMES;                       00009000
FULLLIST:                                                               00009100
               WHILE TRUE DO                                            00009200
          BEGIN                                                         00009300
               READ(CUBE,10,A[*])[LAST];                                00009400
               READ(CUBE,10,B[*]);                                      00009500
               MV1(A,B,C);                                              00009600
               WRITE(LINE,15,C[*]);                                     00009700
               LCTR~LCTR+1;IF LCTR>48 THEN NEWPAGE;                     00009800
          END;                                                          00009900
LISTPROGS:                                                              00010000
               WHILE TRUE DO                                            00010100
          BEGIN                                                         00010200
               READ(CUBE,10,A[*])[LAST];                                00010300
               READ(CUBE,10,B[*]);                                      00010400
               IF NOT FLAG(A) THEN                                      00010500
          BEGIN                                                         00010600
               MV2(A,B,C);                                              00010700
               WRITE(LINE,15,C[*]);                                     00010800
               LCTR~LCTR+1;IF LCTR>48 THEN NEWPAGE;                     00010900
          END;                                                          00011000
          END;                                                          00011100
PUNCHNAMES:                                                             00011200
               WHILE TRUE DO                                            00011300
          BEGIN                                                         00011400
               READ(CUBE,10,A[*])[LAST];                                00011500
               READ(CUBE,10,B[*]);                                      00011600
               IF NOT FLAG(A) THEN                                      00011700
          BEGIN                                                         00011800
               MV2(A,B,C);                                              00011900
               READ(C[*],FP1,FOR I~0,1,2 DO A[I]);                      00012000
               WRITE(PCH,FP2,FOR I~0,1,2 DO A[I]);                      00012100
          END;                                                          00012200
          END;                                                          00012300
SORTLIST:                                                               00012400
               WHILE TRUE DO                                            00012500
          BEGIN                                                         00012600
               READ(CUBE,F1,FOR I~0 STEP 1 UNTIL 9 DO D[I])[SORTIT];    00012700
               IF D[9]="|" THEN SPACE(CUBE,1) ELSE                      00012800
          BEGIN                                                         00012900
               READ(CUBE,F2,FOR I~9 STEP 1 UNTIL 18 DO D[I]);           00013000
               WRITE(TEMP,19,D[*]);                                     00013100
          END;                                                          00013200
          END;                                                          00013300
SORTIT:        REWIND(TEMP);                                            00013400
               SORT(OUTPRC,INPRC,0,HIVAL,CMP,19,6000);                  00013500
LAST:     END.                                                          00013600
