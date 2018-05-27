         BEGIN                                                          00000100
COMMENT        CUBE LIBRARY NUMBER IS J400006.  THE PROGRAM NAME IS     00000200
               "DUMP/ARRAY".    THIS VERSION DATED 5/8/68;              00000300
PROCEDURE     DUMPWORD(WORD,FILENAME,FLAG,NAME) ;                       00000400
VALUE         WORD,FLAG,NAME ;                                          00000500
BOOLEAN       FLAG ;                                                    00000600
REAL          WORD ;  FILE FILENAME ;  ALPHA NAME ;                     00000700
         BEGIN                                                          00000800
COMMENT                                                                 00000900
              THIS PROCEDURE ACTS LIKE A MONITOR DECLARATION EXCEPT     00001000
              THAT THE RESULT IS IN OCTAL. THE CALL                     00001100
                   DUMPWORD(PARAVL,PRINT,"PARAVL")                      00001200
              WOULD RESULT IN AN OUTPUT ON THE FILE PRINT STARTING      00001300
              IN COLUMN ONE IN THE FOLLOWING FORMAT:-                   00001400
                   PARAVL   00000000 00006000                           00001500
              END OF COMMENT ;                                          00001600
ARRAY         ARRAYJUNK[0:15] ;                                         00001700
STREAM PROCEDURE EDIT(WORD,NAME,DEST,FLAG) ;                            00001800
VALUE         WORD,NAME,FLAG ;                                          00001900
         BEGIN                                                          00002000
              DI ~ DEST ; DS ~ 8 LIT "    " ; SI ~DEST ; DS ~ 14 WDS ;  00002100
              DI ~ DEST ;                                               00002200
              SI ~ LOC FLAG ; SI ~ SI + 7 ; IF SC = "1" THEN            00002300
         BEGIN                                                          00002400
              DI ~DI + 1 ; SI ~ LOC NAME ; SI ~ SI + 5 ; SKIP 3 SB ;    00002500
              5(3(DS ~ 1 RESET ) ; 3(IF SB THEN DS ~ 1 SET ELSE         00002600
              DS ~ 1 RESET ; SKIP 1 SB )) ;                             00002700
         END ELSE                                                       00002800
         BEGIN                                                          00002900
              SI ~ LOC NAME ; SI ~ SI + 2 ;                             00003000
              DS ~ 6 CHR ;                                              00003100
         END ;                                                          00003200
              DI ~ DI + 2 ; SI ~ LOC WORD ;                             00003300
              2(DS ~ 1 LIT " " ; 8( DS ~ 3 RESET ;                      00003400
              3(IF SB THEN DS ~ 1 SET ELSE DS ~ 1 RESET ; SKIP 1 SB ))) 00003500
         END EDIT ;                                                     00003600
              EDIT(WORD,NAME,ARRAYJUNK,FLAG) ;                          00003700
              WRITE(FILENAME[DBL],15,ARRAYJUNK[*]) ;                    00003800
         END DUMPWORD ;                                                 00003900
PROCEDURE     DUMPARRAYROW (A,LBOUND,UBOUND,ADDRESS,FILENAME,ANAME1,    00004000
                     ANAME2,NAMEFLAG,SPFLAG) ;                          00004100
VALUE         ADDRESS,LBOUND,UBOUND,NAMEFLAG,SPFLAG ;                   00004200
INTEGER       ADDRESS,LBOUND,UBOUND ;                                   00004300
ALPHA         ANAME1,ANAME2 ;                                           00004400
FILE          FILENAME ;                                                00004500
BOOLEAN       NAMEFLAG,SPFLAG ;                                         00004600
ARRAY         A[*] ;                                                    00004700
         BEGIN                                                          00004800
COMMENT                                                                 00004900
              THIS PROCEDURE WILL FURNISH AN OCTAL DUMP OF THE          00005000
              ARRAY ROW A ON THE FILE FILENAME, LBOUND AND UBOUND       00005100
              BEING THE LOWER AND UPPER BOUNDS OF THE ARRAY ROW ,       00005200
              THE OCTAL DUMP IS VERY SIMILAR TO A MEMORY DUMP AND THE   00005300
              VALUE OF ADDRESS IS REQUIRED FOR REFERENCE PURPOSES ,     00005400
              ANAME1 AND ANAME2 SHOULD BE 6 CHARACTER STRINGS WHICH     00005500
              ARE USED AS A TITLE FOR EACH DUMP PRODUCED BY A CALL ON   00005600
              THE PROCEDURE.                                            00005700
              FOR EXAMPLE THE FOLLOWING STATEMENTS :-                   00005800
              FOR I ~0 STEP 1 UNTIL NUMBEROFROWS DO                     00005900
              DUMPARRAYROW(A[I,*],0,511,I|512,PRINT,"ARRAY ","A     ")  00006000
              WILL DUMP THE TWO DIMENSIONAL ARRAY A AND THE DUMP        00006100
              WOULD LOOK LIKE THE DUMP OF LONG ONE DIMENSIONAL ARRAY ;  00006200
INTEGER       ADD,I,J,NPROWS,NLASTROW ;                                 00006300
BOOLEAN       SAMELINE ;                                                00006400
SAVE ARRAY    BUFFER,OLDBUFFER[0:14] ;                                  00006500
FORMAT        ASTER(X6,12(" * * * *")) ;                                00006600
FORMAT        NAMEFORM("ARRAY   ",2A6) ;                                00006700
STREAM PROCEDURE CLEAR(A,N) ;                                           00006800
VALUE         N ;                                                       00006900
         BEGIN                                                          00007000
            DI ~ A ; DS ~ 8 LIT "    " ; SI ~ A ; DS ~ N WDS ;          00007100
         END CLEAR ;                                                    00007200
STREAM PROCEDURE TABLEDIT(ADDRESS,TABLE,BUFFER,NWORDS) ;                00007300
VALUE         NWORDS ;                                                  00007400
         BEGIN                                                          00007500
              SI ~ ADDRESS ; DI ~ BUFFER ; SI ~ SI + 5 ; SKIP 3 SB ;    00007600
              5 ( 3 ( DS ~ 1 RESET ) ;                                  00007700
                              3 ( IF SB THEN DS ~ 1 SET ELSE DS ~ 1     00007800
                   RESET ; SKIP 1 SB ) ) ; SI ~ TABLE ; DS ~ 1 LIT " " ;00007900
         NWORDS(DS ~1 LIT " "; 2(DS ~ 1 LIT " " ; 8(3(DS ~ 1 RESET );   00008000
              3(IF SB THEN DS ~ 1 SET ELSE DS ~ 1 RESET ; SKIP 1 SB))));00008100
         END TABLEDIT ;                                                 00008200
STREAM PROCEDURE MOVEWORDS(SOURCE,DEST,NWORDS) ;                        00008300
VALUE         NWORDS ;                                                  00008400
         BEGIN                                                          00008500
LOCAL         T ;                                                       00008600
              SI ~ LOC NWORDS ; SI ~ SI + 6 ; DI ~LOC T ; DI ~ DI + 7 ; 00008700
              DS ~ 1 CHR ; SI ~ SOURCE ; DI ~ DEST ;                    00008800
              T ( DS ~ 32 WDS ; DS ~ 32 WDS ) ; DS ~ NWORDS WDS ;       00008900
         END MOVEWORDS ;                                                00009000
BOOLEAN STREAM PROCEDURE WORDEQUAL(S,D,NWORDS) ;                        00009100
VALUE         NWORDS ;                                                  00009200
         BEGIN                                                          00009300
LABEL         L ;                                                       00009400
              SI ~ S ; DI ~ D ; TALLY ~ 0 ;                             00009500
              NWORDS ( IF 8 SC ! DC THEN JUMP OUT 1 TO L ) ;            00009600
              TALLY ~ 1 ;                                               00009700
L:            WORDEQUAL ~ TALLY ;                                       00009800
         END WORDEQUALL ;                                               00009900
              CLEAR(BUFFER,14) ; CLEAR(OLDBUFFER,14) ;                  00010000
              SAMELINE ~ FALSE ;                                        00010100
              WRITE(FILENAME[DBL]) ;                                    00010200
              IF NAMEFLAG THEN                                          00010300
              WRITE(FILENAME[DBL],NAMEFORM,ANAME1,ANAME2 ) ;            00010400
              NPROWS ~ ((UBOUND+1-LBOUND) DIV 6 - 1 ) | 6 ;             00010500
              NLASTROW ~ (UBOUND + 1 - LBOUND ) MOD 6 ;                 00010600
              ADD ~ ADDRESS ;                                           00010700
              FOR I ~ 0 STEP 6 UNTIL NPROWS DO                          00010800
         BEGIN                                                          00010900
              TABLEDIT(ADD,A[I+LBOUND],BUFFER,6) ;                      00011000
              ADD ~ ADD + 6 ;                                           00011100
              IF WORDEQUAL(OLDBUFFER[1],BUFFER[1],14) THEN              00011200
         BEGIN                                                          00011300
              IF NOT SAMELINE THEN WRITE(FILENAME,ASTER) ;              00011400
              SAMELINE ~TRUE ;                                          00011500
         END ELSE                                                       00011600
         BEGIN                                                          00011700
              IF SPFLAG THEN                                            00011800
              WRITE(FILENAME[DBL],15,BUFFER[*]) ELSE                    00011900
              WRITE(FILENAME     ,15,BUFFER[*]) ;                       00012000
              SAMELINE ~ FALSE ;                                        00012100
         END ;                                                          00012200
              MOVEWORDS(BUFFER,OLDBUFFER,15) ;                          00012300
              CLEAR(BUFFER,14) ;                                        00012400
         END ;                                                          00012500
              IF NLASTROW !0 THEN                                       00012600
         BEGIN                                                          00012700
              CLEAR(BUFFER,14) ;                                        00012800
              TABLEDIT(ADD,A[(NPROWS+6)  +LBOUND],BUFFER,NLASTROW) ;    00012900
              IF SPFLAG THEN                                            00013000
              WRITE(FILENAME[DBL],15,BUFFER[*]) ELSE                    00013100
              WRITE(FILENAME     ,15,BUFFER[*]) ;                       00013200
         END ;                                                          00013300
         END DUMPARRAYROW ;                                             00013400
PROCEDURE     DUMPPRT(WORD,PRINT,PRTSIZE) ;                             00013500
VALUE         PRTSIZE ;  REAL  PRTSIZE, WORD ;                          00013600
FILE          PRINT ;                                                   00013700
         BEGIN                                                          00013800
REAL          PRTLOC ;                                                  00013900
COMMENT                                                                 00014000
              THIS PROCEDURE WILL GIVE AN OCTAL DUMP OF THE CALLING     00014100
              PROGRAMS PRT STARTING AT THE LOCATION OF THE REAL         00014200
              VARIABLE WORD AND OF LENGTH PRTSIZE ON THE FILE PRINT.    00014300
              THE PARAMETER PASSED FOR WORD SHOULD BE THE FIRST         00014400
              DECLARED VARIABLE IN THE CALLING PROGRAM (I.E. THE        00014500
              VARIABLE IN OCTAL LOCATION 25) ;                          00014600
ARRAY         PRTARRAY[0:512] ;                                         00014700
STREAM PROCEDURE MOVEWORDS(SOURCE,DEST,NWORDS) ;                        00014800
VALUE         NWORDS ;                                                  00014900
         BEGIN                                                          00015000
LOCAL         T ;                                                       00015100
              SI ~ LOC NWORDS ; SI ~ SI + 6 ; DI ~LOC T ; DI ~ DI + 7 ; 00015200
              DS ~ 1 CHR ; SI ~ SOURCE ; DI ~ DEST ;                    00015300
              T ( DS ~ 32 WDS ; DS ~ 32 WDS ) ; DS ~ NWORDS WDS ;       00015400
         END MOVEWORDS ;                                                00015500
REAL STREAM PROCEDURE MKABS(WORD) ;                                     00015600
         BEGIN                                                          00015700
              DI ~     WORD ; MKABS ~ DI ;                              00015800
         END MKABS ;                                                    00015900
              PRTLOC ~ MKABS(WORD) ;                                    00016000
              DUMPWORD(PRTLOC,PRINT,FALSE,"PRTLOC") ;                   00016100
              IF PRTSIZE > 512 THEN PRTSIZE ~ 512 ;                     00016200
              MOVEWORDS(WORD,PRTARRAY,PRTSIZE) ;                        00016300
              DUMPARRAYROW(PRTARRAY,0,PRTSIZE,21,PRINT,"PRT   ",        00016400
                   "DUMP  ",TRUE,TRUE) ;                                00016500
         END DUMPPRT ;                                                  00016600
         END .                                                          00016700
