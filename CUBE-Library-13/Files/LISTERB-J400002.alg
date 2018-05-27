         BEGIN                                                          00000100
COMMENT        CUBE LIBRARY NUMBER IS J400002.  THE PROGRAM NAME IS     00000200
               "LISTER/B".      THIS VERSION DATED 5/8/68;              00000300
INTEGER       CASE ;                                                    00000400
FILE          PNCH 0(2,10,SAVE 10) ;                                    00000500
DEFINE        PUN =PNCH # ;                                             00000600
FILE          B(2,10) ; FILE LINE 4 (2,15) ; ARRAY A,C[0:15] ;          00000700
LABEL EOF ; INTEGER SEQ,LSEQ ;                                          00000800
INTEGER       NSEQ,FSEQ ;                                               00000900
INTEGER         EQL,LSS ;                                               00001000
BOOLEAN       PUNCHTOG ;                                                00001100
BOOLEAN       LISTOG,SEQTOG ;                                           00001200
FORMAT        FTOT(I8,"---LISTER/B SEQ ERRORS") ,                       00001300
              OK("--- NO LISTER/B ERRORS --") ;                         00001400
DEFINE        FIXNEWSEQ = NSEQ ~ NSEQ +2 ;                              00001500
              OCTDEC(SEQ,A[12]) ;                                       00001600
              OCTDEC(NSEQ,A[9]) # ;                                     00001700
ARRAY         BUFFER[0:15] ;                                            00001800
PROCEDURE     DATECONVERT(BUFFER) ;                                     00001900
ARRAY         BUFFER[*] ;                                               00002000
         BEGIN                                                          00002100
REAL          TIME0,TIME1 ;                                             00002200
INTEGER       YEAR,MONTH,DAY,HOUR,MINUTE,SECOND ;                       00002300
INTEGER       I,DAYS ;                                                  00002400
REAL          TEMP ;                                                    00002500
LABEL         GOOD,CONT ;                                               00002600
ARRAY         MONDAYS[0:15] ;                                           00002700
FORMAT        FTIME(X63,"LISTER/B DATE ",                               00002800
                                    ,I2,"/",I2,"/",I2,"  TIME ",        00002900
                   I2,".",I2,X37) ;                                     00003000
STREAM PROCEDURE DTOCT(S,SPOS,D,N) ;                                    00003100
VALUE         SPOS,N ;                                                  00003200
         BEGIN                                                          00003300
              SI ~ S ; SI ~ SI + SPOS ; DI ~ D ; DS ~ N OCT ;           00003400
         END DTOCT ;                                                    00003500
              FILL MONDAYS[*]WITH 0,                                    00003600
              31,59,90,120,151,181,212,243,273,304,334,365 ;            00003700
              TIME0 ~ TIME(0) ; TIME1 ~ TIME(1) ;                       00003800
              TEMP ~ TIME0 ;                                            00003900
              DTOCT(TEMP,3,YEAR,2) ; DTOCT(TEMP,5,DAYS,3) ;             00004000
              COMMENT  CHECK FOR LEAP YEAR AND FIX IT UP;               00004100
              IF YEAR MOD 4 = 0 AND YEAR MOD 100 ! 0                    00004200
                   THEN  FOR I ~ 2 STEP 1 UNTIL 12 DO                   00004300
                 MONDAYS[I] ~ MONDAYS[I] + 1 ;                          00004400
              FOR I ~ 1 STEP 1 UNTIL 12 DO                              00004500
         BEGIN                                                          00004600
              IF DAYS { MONDAYS[I] THEN GO TO GOOD ;                    00004700
         END ;                                                          00004800
              MONTH ~ 12 ; DAY ~ 31 ; GO TO CONT ;                      00004900
GOOD:         MONTH ~ I ; DAY ~ DAYS - MONDAYS[I-1] ;                   00005000
CONT:         TEMP ~ TIME1 ;                                            00005100
              HOUR ~ TEMP DIV 216000.0 ;                                00005200
              MINUTE ~ (TEMP DIV 3600) MOD 60 ;                         00005300
              SECOND ~(TEMP DIV 60 ) MOD 60 ;                           00005400
              IF LISTOG THEN                                            00005500
              WRITE( BUFFER[*],FTIME,MONTH,DAY,YEAR,HOUR,MINUTE) ;      00005600
         END DATECONVERT ;                                              00005700
STREAM PROCEDURE MOVE(S,D) ;                                            00005800
         BEGIN                                                          00005900
              SI ~ S ; DI ~ D ; DS ~ 15 WDS ;                           00006000
         END MOVE ;                                                     00006100
STREAM PROCEDURE CLEAR(A) ; BEGIN DI ~ A ; DS ~ 8 LIT "    " ;          00006200
              SI ~ A ; DS ~ 14 WDS ; END CLEAR ;                        00006300
STREAM PROCEDURE DOLLAR (A, A12);                                       00006400
         BEGIN                                                          00006500
              SI ~ A;   IF SC = "$" THEN                                00006600
              BEGIN DI ~ A12; DS ~ 11 LIT "DOLLAR CARD" END             00006700
         END CHECK FOR DOLLAR CARDS;                                    00006800
STREAM PROCEDURE ZONET  (A, A12);                                       00006900
         BEGIN                                                          00007000
LABEL         L ;                                                       00007100
              SI ~ A ;  8(IF SC = ALPHA THEN    SI ~ SI + 1 ELSE        00007200
         BEGIN    DI ~ A12 ; DS ~ 4 LIT "ZONE" ; JUMP OUT 1 TO L ; END; 00007300
                 ) ;                                                    00007400
L:                                                                      00007500
         END ZONET ;                                                    00007600
STREAM PROCEDURE OCTDEC(S,D) ; BEGIN SI ~ S ; DI ~ D ; DS ~ 8 DEC ; END;00007700
STREAM PROCEDURE DECOCT(S,D) ; BEGIN SI ~ S ; DI ~ D ;DS ~ 8 OCT ; END; 00007800
STREAM PROCEDURE SET(D) ; BEGIN DI ~ D ; DS ~ 8 LIT "****"; END ;       00007900
STREAM PROCEDURE SAT(D) ; BEGIN DI ~ D ; DS ~ 4 LIT "===="; END ;       00008000
BOOLEAN STREAM PROCEDURE  CHECKPAGE(S) ;                                00008100
         BEGIN                                                          00008200
              TALLY ~ 0 ;                                               00008300
              SI ~ S ; IF SC = "@" THEN TALLY ~ 1 ;                     00008400
              CHECKPAGE ~ TALLY ;                                       00008500
         END CHECKPAGE ;                                                00008600
              CLEAR(C) ;                                                00008700
               SEQ ~ LSEQ ~ EQL ~ LSS ~ 0 ;                             00008800
              LISTOG ~ (CASE MOD 2) =0 ;                                00008900
              DATECONVERT(BUFFER);                                      00009000
              IF LISTOG THEN                                            00009100
              WRITE(LINE[DBL],15,BUFFER[*]) ;                           00009200
              PUNCHTOG ~ (CASE MOD 2 ) =1 ;                             00009300
              SEQTOG ~ (CASE = 2 ) OR (CASE = 3 );                      00009400
      DO BEGIN                                                          00009500
              CLEAR(A) ;                                                00009600
              READ(B,10,A[*])[EOF] ;                                    00009700
              IF CHECKPAGE(A) THEN                                      00009800
         BEGIN                                                          00009900
              WRITE(LINE[PAGE]) ; WRITE(LINE[DBL],15,BUFFER[*]) ;       00010000
         END ;                                                          00010100
              ZONET(A[9],A[12]) ;                                       00010200
              DOLLAR (A [0], A [12]);                                   00010300
              DECOCT(A[9],SEQ) ;                                        00010400
              IF SEQ < LSEQ THEN                                        00010500
         BEGIN                                                          00010600
              LSS ~ LSS + 1 ; SET(A[11]) ;                              00010700
              IF SEQ = 0 AND SEQTOG THEN                                00010800
         BEGIN                                                          00010900
              NSEQ ~ FSEQ ~ LSEQ ; FIXNEWSEQ ;                          00011000
         END ;                                                          00011100
         END ELSE                                                       00011200
              IF SEQ =LSEQ THEN                                         00011300
         BEGIN                                                          00011400
              EQL ~ EQL + 1 ; SAT(A[11]) ;                              00011500
              IF SEQ = 0 AND SEQTOG THEN                                00011600
         BEGIN                                                          00011700
              FIXNEWSEQ ;                                               00011800
         END ;                                                          00011900
         END ELSE                                                       00012000
              IF LSEQ = 0 AND SEQTOG AND (SEQ = FSEQ) THEN              00012100
         BEGIN                                                          00012200
              FIXNEWSEQ ;                                               00012300
         END ;                                                          00012400
              IF PUNCHTOG THEN WRITE(PUN,10,A[*]) ;                     00012500
              IF LISTOG THEN                                            00012600
              WRITE(LINE[DBL],15,A[*]) ;                                00012700
              LSEQ ~ SEQ ;                                              00012800
              MOVE(A,C) ;                                               00012900
         END UNTIL FALSE ;                                              00013000
EOF:                                                                    00013100
              IF LISTOG THEN                                            00013200
              WRITE(LINE[DBL],FTOT,    LSS) ;                           00013300
              IF LISTOG THEN                                            00013400
         BEGIN                                                          00013500
              WRITE(LINE[PAGE]) ;                                       00013600
              WRITE(LINE[PAGE]) ;                                       00013700
         END ;                                                          00013800
         END .                                                          00013900
