         BEGIN                                                          00000100
COMMENT        CUBE LIBRARY NUMBER IS J400003.  THE PROGRAM NAME IS     00000200
               "LISTER/C".      THIS VERSION DATED 5/8/68;              00000300
INTEGER       CASE ;                                                    00000400
FILE          PNCH 0(2,10,SAVE 10) ;                                    00000500
DEFINE        PUN =PNCH # ;                                             00000600
FILE          B(2,10) ; FILE LINE 4 (2,15) ; ARRAY A,C[0:15] ;          00000700
FILE          TAPE(2,56,10) ;                                           00000800
FILE          DISK DISK SERIAL  (2,10,150);                             00000900
SWITCH FILE   SWIN ~ TAPE , DISK ;                                      00001000
ARRAY         CBUFF,TBUFF[0:15] ;                                       00001100
INTEGER       CSEQ,TSEQ,SWFIN ;                                         00001200
INTEGER       IJUNK ;                                                   00001300
BOOLEAN       PATCHTOG ;                                                00001400
BOOLEAN       FIXTOG ;                                                  00001500
BOOLEAN       SELECT ;                                                  00001600
DEFINE        SYM = SWIN[SWFIN]  # ;                                    00001700
DEFINE        RDC = READ(B   ,10,CBUFF[*])[EOF] ; CONV(CBUFF[9],CSEQ) #,00001800
              RDT = READ(SYM,10,TBUFF[*])[EOF] ; CONV(TBUFF[9],TSEQ) # ;00001900
LABEL EOF ; INTEGER SEQ,LSEQ ;                                          00002000
INTEGER       NSEQ,FSEQ ;                                               00002100
INTEGER         EQL,LSS ;                                               00002200
BOOLEAN       LASTALIST ;                                               00002300
BOOLEAN       PUNCHTOG ;                                                00002400
BOOLEAN       LISTOG,SEQTOG ;                                           00002500
FORMAT        FTOT(I8,"---LISTER/B SEQ ERRORS") ,                       00002600
              OK("--- NO LISTER/B ERRORS --") ;                         00002700
DEFINE        FIXNEWSEQ = NSEQ ~ NSEQ +2 ;                              00002800
              OCTDEC(SEQ,A[12]) ;                                       00002900
              OCTDEC(NSEQ,A[9]) # ;                                     00003000
ARRAY         BUFFER[0:15] ;                                            00003100
INTEGER STREAM PROCEDURE COUNTPERCENT(S) ;                              00003200
         BEGIN                                                          00003300
              SI ~ S ; TALLY ~ 0 ;                                      00003400
              2(36(IF SC = "%" THEN TALLY ~ TALLY + 1 ;                 00003500
              SI ~ SI + 1)) ;                                           00003600
              COUNTPERCENT ~ TALLY ;                                    00003700
         END ;                                                          00003800
STREAM PROCEDURE SETPERCENT(D) ; BEGIN DI ~ D ;                         00003900
              DS ~ 8 LIT "PERCENTS" ; END ;                             00004000
STREAM PROCEDURE SETFILE (D) ; BEGIN DI ~ D ;                           00004100
              DS ~ 11 LIT "FILE RECORD" ; END SETFILE ;                 00004200
PROCEDURE     DATECONVERT(BUFFER) ;                                     00004300
ARRAY         BUFFER[*] ;                                               00004400
         BEGIN                                                          00004500
REAL          TIME0,TIME1 ;                                             00004600
INTEGER       YEAR,MONTH,DAY,HOUR,MINUTE,SECOND ;                       00004700
INTEGER       I,DAYS ;                                                  00004800
REAL          TEMP ;                                                    00004900
LABEL         GOOD,CONT ;                                               00005000
ARRAY         MONDAYS[0:15] ;                                           00005100
FORMAT        FTIME(X63,"LISTER/B DATE ",                               00005200
                                    ,I2,"/",I2,"/",I2,"  TIME ",        00005300
                   I2,".",I2,X37) ;                                     00005400
STREAM PROCEDURE DTOCT(S,SPOS,D,N) ;                                    00005500
VALUE         SPOS,N ;                                                  00005600
         BEGIN                                                          00005700
              SI ~ S ; SI ~ SI + SPOS ; DI ~ D ; DS ~ N OCT ;           00005800
         END DTOCT ;                                                    00005900
              FILL MONDAYS[*]WITH 0,                                    00006000
              31,59,90,120,151,181,212,243,273,304,334,365 ;            00006100
              TIME0 ~ TIME(0) ; TIME1 ~ TIME(1) ;                       00006200
              TEMP ~ TIME0 ;                                            00006300
              DTOCT(TEMP,3,YEAR,2) ; DTOCT(TEMP,5,DAYS,3) ;             00006400
              COMMENT  CHECK FOR LEAP YEAR AND FIX IT UP;               00006500
              IF YEAR MOD 4 = 0 AND YEAR MOD 100 ! 0                    00006600
                   THEN  FOR I ~ 2 STEP 1 UNTIL 12 DO                   00006700
                 MONDAYS[I] ~ MONDAYS[I] + 1 ;                          00006800
              FOR I ~ 1 STEP 1 UNTIL 12 DO                              00006900
         BEGIN                                                          00007000
              IF DAYS { MONDAYS[I] THEN GO TO GOOD ;                    00007100
         END ;                                                          00007200
              MONTH ~ 12 ; DAY ~ 31 ; GO TO CONT ;                      00007300
GOOD:         MONTH ~ I ; DAY ~ DAYS - MONDAYS[I-1] ;                   00007400
CONT:         TEMP ~ TIME1 ;                                            00007500
              HOUR ~ TEMP DIV 216000.0 ;                                00007600
              MINUTE ~ (TEMP DIV 3600) MOD 60 ;                         00007700
              SECOND ~(TEMP DIV 60 ) MOD 60 ;                           00007800
              IF LISTOG THEN                                            00007900
              WRITE( BUFFER[*],FTIME,MONTH,DAY,YEAR,HOUR,MINUTE) ;      00008000
         END DATECONVERT ;                                              00008100
BOOLEAN STREAM PROCEDURE CHECKCHAR(S,SPOS,CHAR) ; VALUE SPOS,CHAR ;     00008200
         BEGIN                                                          00008300
              SI ~ S ; SI ~ SI + SPOS ; DI ~ LOC CHAR ;                 00008400
              DI ~ DI + 7 ; IF SC = DC THEN TALLY ~ 1 ELSE TALLY ~ 0 ;  00008500
              CHECKCHAR ~ TALLY ;                                       00008600
         END ;                                                          00008700
STREAM PROCEDURE MONITER(S,D) ;                                         00008800
         BEGIN                                                          00008900
LOCAL         T ;                                                       00009000
              DI ~ LOC T ; DS ~ 7 LIT "MONITOR"; DI ~ LOC T ; SI ~ S ;  00009100
              IF 7 SC = DC THEN                                         00009200
         BEGIN                                                          00009300
              SI ~ LOC T ; DI ~ D ; DS ~ 7 CHR ;                        00009400
         END ;                                                          00009500
         END ;                                                          00009600
STREAM PROCEDURE DUMPER(S,D) ;                                          00009700
         BEGIN                                                          00009800
LOCAL         T ;                                                       00009900
              DI ~ LOC T ; DS ~ 4 LIT "DUMP"   ; DI ~ LOC T ; SI ~ S ;  00010000
              IF 4 SC = DC THEN                                         00010100
         BEGIN                                                          00010200
              SI ~ LOC T ; DI ~ D ; DS ~ 4 CHR ;                        00010300
         END ;                                                          00010400
         END ;                                                          00010500
BOOLEAN STREAM PROCEDURE FINDLIST(S) ;                                  00010600
         BEGIN                                                          00010700
LOCAL         T ;                                                       00010800
LABEL         L ;                                                       00010900
              DI ~ LOC T ; DI ~ DI + 3 ; DS ~ 5 LIT "LIST " ;           00011000
              DI ~ DI - 5 ; SI ~ S ;                                    00011100
              TALLY ~ 0 ;                                               00011200
              63(IF SC = "L" THEN                                       00011300
         BEGIN                                                          00011400
              IF 5 SC = DC THEN                                         00011500
         BEGIN                                                          00011600
              TALLY ~ 1 ; JUMP OUT 1 TO L ;                             00011700
         END ;                                                          00011800
              SI ~ SI - 5 ; DI ~ DI - 5 ;                               00011900
         END ;                                                          00012000
              SI ~ SI + 1 ) ;                                           00012100
L:            FINDLIST ~ TALLY ;                                        00012200
         END ;                                                          00012300
STREAM PROCEDURE SETCHAR(D,DPOD,CHAR) ;                                 00012400
VALUE         DPOD,CHAR ;                                               00012500
         BEGIN                                                          00012600
              DI ~ D ; DI ~ DI + DPOD ; SI ~ LOC CHAR ; SI ~ SI + 7 ;   00012700
              DS ~1 CHR ;                                               00012800
         END SETCHAR ;                                                  00012900
STREAM PROCEDURE MOVE(S,D) ;                                            00013000
         BEGIN                                                          00013100
              SI ~ S ; DI ~ D ; DS ~ 15 WDS ;                           00013200
         END MOVE ;                                                     00013300
STREAM PROCEDURE CLEAR(A) ; BEGIN DI ~ A ; DS ~ 8 LIT "    " ;          00013400
              SI ~ A ; DS ~ 14 WDS ; END CLEAR ;                        00013500
STREAM PROCEDURE DOLLAR (A, A12);                                       00013600
         BEGIN                                                          00013700
              SI ~ A;   IF SC = "$" THEN                                00013800
              BEGIN DI ~ A12; DS ~ 11 LIT "DOLLAR CARD" END             00013900
         END CHECK FOR DOLLAR CARDS;                                    00014000
STREAM PROCEDURE ZONET  (A, A12);                                       00014100
         BEGIN                                                          00014200
LABEL         L ;                                                       00014300
              SI ~ A ;  8(IF SC = ALPHA THEN    SI ~ SI + 1 ELSE        00014400
         BEGIN    DI ~ A12 ; DS ~ 4 LIT "ZONE" ; JUMP OUT 1 TO L ; END; 00014500
                 ) ;                                                    00014600
L:                                                                      00014700
         END ZONET ;                                                    00014800
STREAM PROCEDURE CONV(S,D) ;                                            00014900
         BEGIN                                                          00015000
              SI ~ S ; DI ~ D ; DS ~ 8 OCT ;                            00015100
         END CONV ;                                                     00015200
PROCEDURE     GETRECORD ;                                               00015300
         BEGIN                                                          00015400
STREAM PROCEDURE MOVE(N,S,D) ;                                          00015500
VALUE         N ;                                                       00015600
         BEGIN                                                          00015700
              SI ~ S ; DI ~ D ; DS ~ N WDS ;                            00015800
         END MOVE ;                                                     00015900
              IF CSEQ = TSEQ THEN                                       00016000
         BEGIN                                                          00016100
              FIXTOG ~ TRUE ;                                           00016200
              IF PATCHTOG THEN                                          00016300
         BEGIN                                                          00016400
              SETFILE(TBUFF[12]) ;                                      00016500
              WRITE(LINE,15,TBUFF[*]) ;                                 00016600
              WRITE(LINE[DBL],15,CBUFF[*]) ;                            00016700
         END ;                                                          00016800
              MOVE(10,CBUFF,A); RDC ; RDT ;                             00016900
         END ELSE                                                       00017000
              IF CSEQ < TSEQ THEN                                       00017100
         BEGIN                                                          00017200
              FIXTOG ~ TRUE ;                                           00017300
              MOVE(10,CBUFF,A) ; RDC ;                                  00017400
         END ELSE                                                       00017500
         BEGIN                                                          00017600
              FIXTOG ~ FALSE ;                                          00017700
              MOVE(10,TBUFF,A) ; RDT ;                                  00017800
         END ;                                                          00017900
         END GETRECORD ;                                                00018000
STREAM PROCEDURE OCTDEC(S,D) ; BEGIN SI ~ S ; DI ~ D ; DS ~ 8 DEC ; END;00018100
STREAM PROCEDURE DECOCT(S,D) ; BEGIN SI ~ S ; DI ~ D ;DS ~ 8 OCT ; END; 00018200
STREAM PROCEDURE SET(D) ; BEGIN DI ~ D ; DS ~ 8 LIT "****"; END ;       00018300
STREAM PROCEDURE SAT(D) ; BEGIN DI ~ D ; DS ~ 4 LIT "===="; END ;       00018400
BOOLEAN STREAM PROCEDURE  CHECKPAGE(S) ;                                00018500
         BEGIN                                                          00018600
              TALLY ~ 0 ;                                               00018700
              SI ~ S ; IF SC = "@" THEN TALLY ~ 1 ;                     00018800
              CHECKPAGE ~ TALLY ;                                       00018900
         END CHECKPAGE ;                                                00019000
              CLEAR(C) ;                                                00019100
              IF CASE  } 100 THEN                                       00019200
         BEGIN                                                          00019300
INTEGER       COUNT,I ;                                                 00019400
DEFINE CARD = B # , LINE = PNCH # ;                                     00019500
ARRAY         A[0:15] ;                                                 00019600
LABEL         RD1,RD2,EOF1,EOF2 ;                                       00019700
FILE          DASK DISK SERIAL [1:1000] (2,10,150) ;                    00019800
              COUNT ~ CASE MOD 100 ;                                    00019900
RD2:          READ(CARD,10,A[*])[EOF2] ;                                00020000
              WRITE(DASK,10,A[*]) ;                                     00020100
              GO TO RD2 ;                                               00020200
EOF2:                                                                   00020300
              FOR I ~ 0 STEP 1 UNTIL COUNT DO                           00020400
         BEGIN                                                          00020500
              REWIND(DASK) ;                                            00020600
RD1:          READ(DASK,10,A[*])[EOF1] ;                                00020700
              WRITE(LINE,10,A[*]) ;                                     00020800
              GO TO RD1 ;                                               00020900
EOF1:                                                                   00021000
         END ;                                                          00021100
              GO TO EOF ;                                               00021200
         END ;                                                          00021300
               SEQ ~ LSEQ ~ EQL ~ LSS ~ 0 ;                             00021400
              SWFIN ~ 9 ;                                               00021500
              LISTOG ~ (CASE MOD 2) =0 ;                                00021600
              DATECONVERT(BUFFER);                                      00021700
              IF LISTOG THEN                                            00021800
              WRITE(LINE[DBL],15,BUFFER[*]) ;                           00021900
              CLEAR(CBUFF) ; CLEAR(TBUFF) ;                             00022000
              IF CASE = 5 OR CASE =7 THEN                               00022100
         BEGIN                                                          00022200
              PATCHTOG ~ TRUE ; LISTOG ~ FALSE ;                        00022300
         END ELSE                                                       00022400
              PUNCHTOG ~ CASE MOD 2 = 1 ;                               00022500
              SEQTOG ~ (CASE = 2 ) OR (CASE = 3 );                      00022600
              IF CASE > 3 THEN                                          00022700
         BEGIN                                                          00022800
              SWFIN ~ IF CASE > 5 THEN 1 ELSE 0 ;                       00022900
              RDC ; RDT ;                                               00023000
              SELECT ~ CHECKCHAR(CBUFF,0," $") AND                      00023100
                 CHECKCHAR(CBUFF,1,"$") ;                               00023200
         END ;                                                          00023300
      DO BEGIN                                                          00023400
              CLEAR(A) ;                                                00023500
              IF SWFIN ! 9 THEN GETRECORD ELSE                          00023600
              READ(B,10,A[*])[EOF] ;                                    00023700
              IF CHECKPAGE(A) THEN                                      00023800
         BEGIN                                                          00023900
              WRITE(LINE[PAGE]) ; WRITE(LINE[DBL],15,BUFFER[*]) ;       00024000
         END ;                                                          00024100
              ZONET(A[9],A[12]) ;                                       00024200
              IF SELECT THEN                                            00024300
              IF CHECKCHAR(A,0,"$")THEN                                 00024400
         BEGIN                                                          00024500
              LASTALIST ~ FALSE ;                                       00024600
              LISTOG ~ FINDLIST(A) ;                                    00024700
              IF LISTOG THEN LASTALIST ~TRUE ;                          00024800
         END ELSE                                                       00024900
         BEGIN                                                          00025000
              IF LASTALIST THEN WRITE(LINE[PAGE]) ;                     00025100
              LASTALIST ~ FALSE ;                                       00025200
         END ;                                                          00025300
              MONITER(A,A[12]) ;                                        00025400
              DUMPER (A,A[12]) ;                                        00025500
              DOLLAR (A [0], A [12]);                                   00025600
              DECOCT(A[9],SEQ) ;                                        00025700
              IF SEQ < LSEQ THEN                                        00025800
         BEGIN                                                          00025900
              LSS ~ LSS + 1 ; SET(A[11]) ;                              00026000
              IF SEQ = 0 AND SEQTOG THEN                                00026100
         BEGIN                                                          00026200
              NSEQ ~ FSEQ ~ LSEQ ; FIXNEWSEQ ;                          00026300
         END ;                                                          00026400
         END ELSE                                                       00026500
              IF SEQ =LSEQ THEN                                         00026600
         BEGIN                                                          00026700
              EQL ~ EQL + 1 ; SAT(A[11]) ;                              00026800
              IF SEQ = 0 AND SEQTOG THEN                                00026900
         BEGIN                                                          00027000
              FIXNEWSEQ ;                                               00027100
         END ;                                                          00027200
         END ELSE                                                       00027300
              IF LSEQ = 0 AND SEQTOG AND (SEQ = FSEQ) THEN              00027400
         BEGIN                                                          00027500
              FIXNEWSEQ ;                                               00027600
         END ;                                                          00027700
              IF PUNCHTOG THEN WRITE(PUN,10,A[*]) ;                     00027800
              IF LISTOG THEN                                            00027900
              IF FIXTOG THEN SETCHAR(A[10],4,"*" ) ELSE                 00028000
              SETCHAR(A[10],4," ") ;                                    00028100
              IF PATCHTOG THEN                                          00028200
         BEGIN                                                          00028300
              IJUNK ~ COUNTPERCENT(A) ;                                 00028400
              IF IJUNK > 1 THEN                                         00028500
         BEGIN                                                          00028600
              SETPERCENT(A[12]) ;                                       00028700
              WRITE(LINE[DBL],15,A[*]) ;                                00028800
         END ;                                                          00028900
         END ;                                                          00029000
              IF LISTOG OR (SELECT AND FIXTOG) THEN                     00029100
              WRITE(LINE[DBL],15,A[*]) ;                                00029200
              LSEQ ~ SEQ ;                                              00029300
              MOVE(A,C) ;                                               00029400
         END UNTIL FALSE ;                                              00029500
EOF:                                                                    00029600
              IF LISTOG THEN                                            00029700
              WRITE(LINE[DBL],FTOT,    LSS) ;                           00029800
              IF LISTOG OR SELECT THEN                                  00029900
         BEGIN                                                          00030000
              WRITE(LINE[PAGE]) ;                                       00030100
              WRITE(LINE[PAGE]) ;                                       00030200
         END ;                                                          00030300
         END .                                                          00030400
