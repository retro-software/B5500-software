TAPEDUMP    000113AA                                                    00000100
                                                                        00000200
  **********************************************************************00000300
          PLEASE NOTE THAT THE BURROUGHS CORPORATION ASSUMES            00000400
     NO RESPONSIBILITY FOR THE USE OR MAINTENANCE OF THIS PROGRAM.      00000500
  ********************************************************************  00000600
                                                                        00000700
I. GENERAL                                                              00000800
   THE PURPOSE OF THIS PROGRAM IS TO PROVIDE A PRINTER LISTING OF       00000900
   THE CONTENTS OF MAGNETIC TAPE RECORDS OR FILES. THE PROGRAM          00001000
   PARAMETERS ARE ENTERED ON THE SPO FOLLOWING THE "ACCEPT" MESSAGE. A  00001100
   TAPE TO BE DUMPED MAY NOT CONTAIN RECORDS WHICH ARE LONGER THAN      00001200
   1023 B5500 WORDS. THIS UTILITY WILL PRINT BOTH THE OCTAL AND THE     00001300
   ALPHA REPRESENTATIONS OF ALL DATA READ FROM THE TAPE.                00001400
                                                                        00001500
II. CONTROL MESSAGES                                                    00001600
   EACH CONTROL CONTAINS FROM ONE TO FOUR FIELDS. FIELDS ARE ARRANGED   00001700
   IN ORDER OF FIELD 1 THRU FIELD 4 FROM LEFT TO RIGHT AND MAY BE       00001800
   SEPARATED BY BLANK COLUMNS. EACH FIELD IS THREE POSITIONS WIDE.      00001900
      1. COMMAND FIELD (FIELD 1)                                        00002000
         A. "DMP" - DUMP                                                00002100
         B. "SKP" - SKIP                                                00002200
         C. "REW" - REWIND                                              00002300
         D. "NTP" - NEW TAPE                                            00002400
         E. "DSC" - TERMINATE THE PROGRAM                               00002500
      2. COUNT FIELD (FIELD 2)                                          00002600
         IF FILE COUNT (NNNFIL, WHERE "NNN" IS FILE COUNT) IS           00002700
         NOT SPECIFIED, THE UTILITY ASSUMES THE NEXT FILE ON            00002800
         THE TAPE THEN OPENED.                                          00002900
         A. NNN - CONTAINS RECORD OR FILE COUNT ( 3 DIGITS )            00003000
      3. RECORD OR FILE FIELD (FIELD 3)                                 00003100
         A. "REC" - COUNT FIELD IS A RECORD COUNT                       00003200
         B. "FIL" - COUNT FIELD IS A FILE COUNT (# OF THE FILE          00003300
                    WHICH IS TO BE DUMPED)                              00003400
NOTE: THE PROGRAM CHECKS FOR LABELED FILES. THEREFORE, THE COUNT        00003500
      FIELD IS THE SAME FOR LABELED AND UNLABELED FILES.                00003600
      4. SHORT OR LONG FIELD (FIELD 4)                                  00003700
         "LNG" WILL BE ASSUMED IF "SHT" IS NOT SPECIFIED.               00003800
         A. "SHT" - PROGRAM WILL DUMP THE FIRST 6 WORDS IN              00003900
                    EACH RECORD.                                        00004000
         B. "LNG" - PROGRAM WILL DUMP THE ENTIRE CONTENTS OF            00004100
                    EACH RECORD.                                        00004200
                                                                        00004300
      CONTROL MESSAGES ARE SCANNED FROM LEFT TO RIGHT.                  00004400
      5. EXAMPLES OF CONTROL MESSAGES                                   00004500
         "DMP 001 FIL LNG"     DUMP THE FIRST FILE                      00004600
         "SKP"                 SKIP THE SECOND FILE                     00004700
         "DMP"                 DUMP THE THIRD FILE                      00004800
         "NTP"                 SWITCH TO A NEW TAPE                     00004900
         "DMP020RECSHT"        DUMP 20 RECORDS FROM THE FIRST FILE      00005000
         "DSC"                 DISCONTINUE THE PROGRAM                  00005100
                                                                        00005200
III. OPERATING INSTRUCTIONS                                             00005300
      A. FROM THE SPO, CARD READER, OR REMOTE SPO, EXECUTE              00005400
         THE PROGRAM.                                                   00005500
      B. FOLLOWING THE MESSAGE "NO FILE ULMTX", ENTER THE REPLY         00005600
         "|ULMTY", WHERE | IS THE MIX INDEX AND Y IS THE DRIVE          00005700
         ON WHICH TAPE TO BE DUMPED IS READY.                           00005800
      C. FOLLOWING THE MESSAGE "ACCEPT", ENTER THE CONTROL MESSAGE      00005900
         DESIRED. WHEN DUMPING A FILE, THE UTILITY WILL DUMP THE        00006000
         HEADER AND TRAILER LABELS OF A FILE AND THE ENTIRE FILE.       00006100
         IF AN ADDITIONAL CONTROL MESSAGE IS ENTERED AFTER THE          00006200
         TRAILER LABEL IS PRINTED, THE NEXT FILE ON THE SAME TAPE       00006300
         WILL BE USED.                                                  00006400
                                                                        00006500
                                                                        00006600
  BEGIN       COMMENT UTILITY/TAPEDUMP   LIBE900;  % MT|DUMP            00006800
  STREAM PROCEDURE GETUM(S,D);                                          00006900
   BEGIN                                                                00007000
  LOCAL T,U;                                                            00007100
    LABEL L1,OFF,DONE;                                                  00007200
   SI~S; DI~D;                                                          00007300
  4( L1: IF SC =       " " THEN BEGIN SI~SI+1; GO TO L1; END;           00007400
   IF SC =       "~" THEN JUMP OUT TO OFF;                              00007500
   TALLY ~ TALLY+1;                                                     00007600
   DI~DI+5; DS~ 3 CHR ;);                                               00007700
   OFF:  U~ TALLY;                                                      00007800
 SI~ LOC U;  SI~SI+7;                                                   00007900
    IF SC {       "1" THEN GO TO DONE;                                  00008000
 SI~D; SI~SI+13;                                                        00008100
  DI~ LOC T; DS~ 3 OCT ;                                                00008200
  SI~ LOC T; DI~D; DI~DI+8; DS~ 1 WDS ;                                 00008300
  DONE: END;                                                            00008400
  ARRAY JUNK[0:9];                                                      00008500
 ARRAY CNTL[0:9];                                                       00008600
  DEFINE COM =CNTL[0]#,                                                 00008700
     NFL=CNTL[1]#,                                                      00008800
     RORF=CNTL[2]#,                                                     00008900
     SORL=CNTL[3]#;                                                     00009000
  BOOLEAN REALLYFIRST;                                                  00009100
  LABEL NEXTL;                                                          00009200
  STREAM PROCEDURE EXPAND(SOURCE,DEST,N,I);                             00009300
    VALUE N,I;                                                          00009400
       BEGIN                                                            00009500
   LOCAL NU;                                                            00009600
   SI~ LOC I;                                                           00009700
    DI~ LOC NU;                                                         00009800
    DS~ 4 DEC ;                                                         00009900
    SI~ LOC NU;                                                         00010000
     DI~ DEST;                                                          00010100
  DS~ 2 LIT " (";                                                       00010200
   DS~ 4 NUM ;                                                          00010300
   DS~ 2 LIT ") ";                                                      00010400
    SI~SOURCE;                                                          00010500
  N(16(DS~3 RESET; 3( IF SB THEN DS~ 1 SET ELSE DS~ 1 RESET;            00010600
          SKIP 1 SB;)));                                                00010700
     END;                                                               00010800
   STREAM PROCEDURE REFORM(SOURCE,DEST);                                00010900
      BEGIN                                                             00011000
          SI~SOURCE;DI~DEST;                                            00011100
    DS~ 8 CHR ;                                                         00011200
  6( DS~ 16 CHR ; DS~ 2 LIT "  ");                                      00011300
  DS~ 4 LIT "    ";                                                     00011400
      END;                                                              00011500
   ARRAY PO[0:14];                                                      00011600
   STREAM PROCEDURE EXPANDALPH(SOURCE,DEST,N);                          00011700
     VALUE N;                                                           00011800
       BEGIN                                                            00011900
           SI~SOURCE;DI~DEST;                                           00012000
  DS~ 8 LIT "        ";                                                 00012100
           N(8 (DS~ 1 LIT " ";DS~ 1 CHR ;));                            00012200
      END;                                                              00012300
  FILE NFILES 11(1,10);                                                 00012400
   FILE ULMTX 9(2,1023);                                                00012500
  FILE LP 4(2,15);                                                      00012600
  DEFINE TP =ULMTX#;                                                    00012700
   ARRAY INP[0:1022];                                                   00012800
   ARRAY OP[0:14];                                                      00012900
   STREAM PROCEDURE LBLTST(S,D);                                        00013000
   BEGIN                                                                00013100
   DI~D;SI~S;SI~SI+1;DI~DI+3;DS~ 5 CHR ;                                00013200
   END;                                                                 00013300
  INTEGER STREAM PROCEDURE FINDN(FPLACE);                               00013400
  BEGIN                                                                 00013500
    SI~FPLACE;DI~ LOC FINDN; SI~SI-8; DS~ 1 WDS ;                       00013600
    END;                                                                00013700
   FORMAT FMT ("BLOCK NO.",I6,", SIZE IS ",I4," WORDS");                00013800
  STREAM PROCEDURE FILLBLANKS (DEST,SKP,COUNT);                         00013900
   VALUE SKP,COUNT;                                                     00014000
    BEGIN                                                               00014100
        DI~DEST;                                                        00014200
   DI~ DI+8;                                                            00014300
  SKP(2(DI~DI+8)); COUNT(2(DS~ 8 LIT "        "));                      00014400
     END;                                                               00014500
  FORMAT  FMT3 ("***EOF***");                                           00014600
  FORMAT  FMT4 ("PARITY ERROR OR ALPHA PARITY BLOCK ********");         00014700
  LABEL EFB,BACK,PARBACK,EOF,PAR;                                       00014800
   LABEL OFF;                                                           00014900
   INTEGER NF;                                                          00015000
  BOOLEAN FIRST,ONEREC;                                                 00015100
   ALPHA LF;                                                            00015200
   INTEGER BCOUNT;                                                      00015300
  INTEGER N,I,J,T;                                                      00015400
   LABEL IGN,IGN2,DONE,START,CONT;                                      00015500
  INTEGER NR;                                                           00015600
   FORMAT FMT5(A3,X1,I2,X1,A3,X1,A1);                                   00015700
   BOOLEAN PERR,EFL;                                                    00015800
   START: PERR~FALSE;EFL~FALSE;                                         00015900
  REALLYFIRST~TRUE;                                                     00016000
      READ(TP[NO])[IGN:IGN2];                                           00016100
    GO TO OFF;                                                          00016200
  IGN:EFL~TRUE;                                                         00016300
    GO TO OFF;                                                          00016400
   IGN2:PERR~TRUE;                                                      00016500
   OFF:                                                                 00016600
  READ(NFILES,10,JUNK[*])[NEXTL];                                       00016700
  NEXTL: GETUM(NFILES(0),CNTL[0]);                                      00016800
   IF COM="NTP" THEN BEGIN CLOSE(TP,RELEASE); GO TO START; END;         00016900
  IF COM="DSC" THEN GO TO DONE;                                         00017000
  IF COM="REW" THEN BEGIN REWIND(TP); GO TO OFF; END;                   00017100
   IF RORF="REC" THEN BEGIN                                             00017200
  NR~NFL;NF~1; END ELSE BEGIN NR~0;NF~NFL; END;                         00017300
  IF COM!"SKP" THEN                                                     00017400
  WRITE(LP[PAGE]);                                                      00017500
   ONEREC~FALSE;                                                        00017600
  FIRST~TRUE;                                                           00017700
   LF~BCOUNT~0;                                                         00017800
   IF EFL THEN GO TO EOF;                                               00017900
  IF PERR THEN GO TO PAR;                                               00018000
    GO TO PARBACK;                                                      00018100
   EFB: BCOUNT~0;                                                       00018200
   IF NOT FIRST THEN                                                    00018300
    IF (NF~NF-1) {0 THEN                                                00018400
       IF LF="LABEL" THEN ONEREC~TRUE ELSE GO TO OFF;                   00018500
  BACK:                                                                 00018600
    READ(TP[NO])[EOF:PAR];                                              00018700
  PARBACK:                                                              00018800
    N~FINDN(TP(0));                                                     00018900
  READ(TP,N,INP[*])[EOF:PAR];                                           00019000
    BCOUNT~BCOUNT+1;                                                    00019100
  IF COM!"SKP" THEN                                                     00019200
  WRITE( LP,FMT,BCOUNT,N);                                              00019300
    IF FIRST THEN                                                       00019400
     BEGIN                                                              00019500
  IF REALLYFIRST THEN                                                   00019600
     LBLTST(INP[0],LF);                                                 00019700
  REALLYFIRST~FALSE;                                                    00019800
    IF LF="LABEL"    THEN  NF~NF+3-NF;                                  00019900
    FIRST~FALSE;                                                        00020000
   END;                                                                 00020100
  IF COM!"SKP" THEN                                                     00020200
   FOR I~ 0 STEP 6 UNTIL N DO                                           00020300
     BEGIN                                                              00020400
     EXPAND(INP[I],OP[0],6,I);                                          00020500
      IF (T~N-I) <6 AND T!0 THEN                                        00020600
     FILLBLANKS(OP[0],T,6-T);                                           00020700
    REFORM(OP[0],PO[0]);                                                00020800
   WRITE(LP,15,PO[*]);                                                  00020900
   EXPANDALPH(INP[I],OP[0],6);                                          00021000
    IF T<6 AND T!0 THEN                                                 00021100
      FILLBLANKS(OP[0],T,6-T);                                          00021200
    REFORM(OP[0],PO[0]);                                                00021300
    WRITE(LP,15,PO[*]);                                                 00021400
  IF SORL="SHT" THEN GO TO CONT;                                        00021500
   END;                                                                 00021600
   CONT:                                                                00021700
   IF ONEREC THEN GO TO OFF;                                            00021800
  IF COM!"SKP" THEN                                                     00021900
     WRITE (LP[DBL]);                                                   00022000
   IF NR=BCOUNT THEN GO TO OFF;                                         00022100
   GO TO BACK;                                                          00022200
  EOF: IF COM="DMP" THEN                                                00022300
  BEGIN WRITE(LP,FMT3); WRITE(LP[DBL]); END;                            00022400
   CLOSE(TP,*);                                                         00022500
   GO TO EFB;                                                           00022600
    PAR:  IF COM="DMP" THEN WRITE(LP,FMT4);                             00022700
   GO TO PARBACK;                                                       00022800
    DONE:                                                               00022900
  END.                                                                  00023000
END;END.        LAST CARD ON 0CRDING TAPE                               99999999
