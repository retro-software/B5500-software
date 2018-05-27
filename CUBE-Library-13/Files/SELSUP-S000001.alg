"SELECT/ASUPER"                                                         00000100
BEGIN                                                                   00000200
COMMENT   CUBE LIBRARY VERSION 11/20/68                                 00000201
          CUBE LIBRARY NUMBER S000001                                   00000202
          SOURCE FILE NAME SELSUP/S000001                               00000203
          PROGRAM NAME SELECT/SUPER;                                    00000204
INTEGER CORE;                                                           00000300
                                                                        00000400
DEFINE                                                                  00000500
PACKETMAX = 40#,                                                        00000600
PLUGMAX =15#;                                                           00000700
ARRAY SORTARRAY [0: 99],                                                00000800
SELECTARRAY [0:499],                                                    00000900
ACTIONARRAY [1:PACKETMAX,0:245],                                        00001000
ZIPARRAY [0:100]                                                        00001100
;                                                                       00001200
BOOLEAN ARRAY  ACTION [1:PACKETMAX];                                    00001300
BOOLEAN ARRAY PLUGBOOL[0:PLUGMAX];                                      00001400
INTEGER SORTKEYS, SELECTSIZE, PACKETS, ACTIONMAX, RECSIZE, INBLOCK,     00001500
LOWRANGE, HIGHRANGE, SKIPNO, LIMITNO, SKIPNUM, LIMITNUM,                00001600
INSIZE, OUTSIZE, INCOUNT, OUTCOUNT, SORTCOUNT,                          00001700
OUTBLOCK, PARAMS, HIGHPLUG ;                                            00001800
BOOLEAN  SELECTOPT, EXPLODOPT, TRACEOPT;                            %JPA00001900
BOOLEAN SELECTS, ZIPS, MUSTACT, MUSTSORT, MUSTCOUNT                     00002000
, RANGE                                                                 00002100
,PLUGIN, MON                                                            00002200
;                                                                       00002300
LABEL   EOJ;                                                            00002400
ACTIONMAX ~ SELECTSIZE~RECSIZE~PACKETS~1;                               00002500
IF CORE = 0 THEN CORE ~ 8000;                                           00002600
%-----------------------------------------------------------------------00002700
BEGIN                                                                   00002800
COMMENT  EDITS INPUT CARDS TO SELECTOR/MESS ;                           00002900
FILE IN CRD (2,10);                                                     00003000
FILE OUT PTR 18 (2,15);                                                 00003100
FILE IN DICTION DISK SERIAL (2,3,90) ;                                  00003200
                                                                        00003300
INTEGER ARRAY  OCTWDS[0:3],PACK[0:40]  ;                                00003400
                                                                        00003500
INTEGER H, OCTSKIP, OCTLIMIT;                                           00003600
REAL ARRAY  JUNK,JUNA,FILENAME,FIELDNAME[0:2] ,                         00003700
PLUGNAMES[0:2|PLUGMAX-1],                                               00003800
BLOCKING[0:3,0:2] ;                                                     00003900
SAVE REAL ARRAY  A[0:10] ;                                              00004000
                                                                        00004100
INTEGER  IDPOS,POS,FIRST ;                                              00004200
INTEGER SWI, ERR,ACTNO,ACTCOUNT,      PACKETNO,       IDNO ;            00004300
INTEGER  AT,LENGTH,CODE,NUMCHARS,SKIPDEST,SW,I,Q,ZIPNO,SORTNO ;         00004400
                                                                        00004500
ALPHA  CARDNAME,LASTNO,SEQNO  ;                                         00004600
ALPHA  ALPHSORT,ALPHID,ALPHACT,ALPHPACKET ;                             00004700
BOOLEAN  SYNTAX,SWITC ;                                                 00004800
FORMAT  LEADFT(A5,X60,X11,A4) ,                                         00004900
FT(2X40,A4),                                                            00005000
HEADING("SYNTAX LISTING"),                                              00005100
BADISTCD ("FIRST CARD WAS NOT A LEAD CARD") ,                           00005200
NOTNUFLEAD("NOT ENOUGH INFO ON LEAD CARD") ,                            00005300
TYPEFT(A4,2X36,A4) ,                                                    00005400
TYPEFT2(A3,X40,X33,A4) ,                                                00005500
ZIPMSG ("NO MORE THAN 3 ZIP CARDS ALLOWED"),                            00005600
SEQMSG("CARD IS OUT OF SEQUENCE"),                                      00005700
TOOMANYPACKETS("NO MORE THAN 40 PACKETS ALLOWED") ,                     00005800
TOOMANYIDS ("TOO MANY IDS PRESENT IN DECK" ),                           00005900
TOOMANYACTIONS ("ONLY UP TO 35 ACTIONS PER PACKET ALLOWED"),            00006000
NUTTIN("NO DATA PRESENT") ;                                             00006100
SWITCH FORMAT   ERRORS ~                                                00006200
("PROGRAMMING ERROR IF THIS OCCURS"),                                   00006300
("NO FILE FOUND"),                                                      00006400
("INTEGERS NOT OF REASONABLE SIZE FOUND ON LEAD CARD"),                 00006500
("2 OR 4 INTEGERS EXPECTED ON LEAD CARD") ,                             00006600
("CARD NOT SORT ZIP PAR OR PACKET AS EXPECTED"),                        00006700
("ILLEGITIMATE CHARACTER ON CARD") ,                                    00006800
("ONLY 10 ZIP CARDS ALLOWED") ,                                         00006900
("ONLY UP TO 1000 PARAMETER CARDS ALLOWED") ,                           00007000
("ONLY UP TO 99 SORT CARDS ALLOWED") ,                                  00007100
("NOT ENOUGH INFO ON SORT CARD") ,                                      00007200
("ASCENDING OR DESCENDING NOT FOUND ON SORT CARD"),                     00007300
("FIELD NAME NOT FOUND IN THE FILE DESIRED") ,                          00007400
(" FIELD NAME FOUND BUT WITH INCORRECT FILE NAME"),                     00007500
("CARD WAS NOT ID ACT MOVE SORT ZIP PAR OR PACKET AS EXPECTED") ,       00007600
("NOT ENOUGH INFO FOUND ON ID CARD"),                                   00007700
("ILLEGITIMATE CHAR FOUND-IS A SPACE BETWEEN FILE & REL OP"),           00007800
("RELATIONAL OP NOT FOUND-IS SPACE AFTER IT"),                          00007900
("DATA MUST BEGIN IN 1ST 60 COLS OF ID CARD"),                          00008000
("NOT ENOUGH INFO FOUND ON ACTION CARD"),                               00008100
("DATA MUST BEGIN IN 1ST 38 COLS OF ACTION CARD") ,                     00008200
("NOT ENOUGH INFO FOUND ON MOVE CARD") ,                                00008300
("TO OR DC NOT FOUND ON MOVE CARD") ,                                   00008400
("6 NUM DIGITS MUST IMMEDIATELY FOLLOW # SIGN") ,                       00008500
("FIELD NAME NOT GIVEN ON PLUG CARD") ,                                 00008600
("FIELD NAME ON PLUG CARD NOT IN PLUG ARRAY") ,                         00008700
("TWO INTEGERS OF REASONABLE SIZE NOT FOUND ON RANGE CARD"),        %JPA00008800
("UNIDENTIFIED OPTION WORD ON LEAD CARD" ) ,                            00008900
("INTEGERS NOT OF RESONABLE SIZE FOUND ON SKIP CARD" ),                 00009000
("INTEGERS NOT OF RESONABLE SIZE FOUND ON LIMIT CARD" ) ,               00009100
("TRACE OPTION WORKS ONLY IF ACTION AND/OR MOVE CARDS ARE PRESENTED."); 00009110
LABEL OPTNSCAN;                                                     %JPA00009200
LABEL  ACTIONCARD ;                                                     00009300
LABEL  EOLEAD,EOF,SORTZIPPARPACKET,WHATCARD,IDORACTION,NOCARDS ;        00009400
                                                                        00009500
%***********************************************************************00009600
BOOLEAN STREAM PROCEDURE ITSPLUGGED(JUNK,PLUGNAME,J,I);                 00009700
VALUE J;                                                                00009800
BEGIN SI~JUNK; DI~ PLUGNAME;                                            00009900
J(IF 16 SC!DC THEN BEGIN TALLY~TALLY+1;SI~SI-16 END                     00010000
ELSE BEGIN I~TALLY;TALLY~1;ITSPLUGGED~TALLY;JUMP OUT END )              00010100
END;                                                                    00010200
                                                                        00010300
%***********************************************************************00010400
BOOLEAN STREAM PROCEDURE  GETFIELDNAME (JUNK,FIELDNAME) ;               00010500
COMMENT  SIGNALS FINDING OF FIELDNAME ;                                 00010600
BEGIN  SI~ FIELDNAME ;  DI~JUNK ;                                       00010700
IF 16 SC = DC THEN TALLY ~1 ;   GETFIELDNAME ~ TALLY ;                  00010800
END GETFIELDNAME ;                                                      00010900
%***********************************************************************00011000
BOOLEAN STREAM PROCEDURE INTONLY (DECIN,OCTOUT) ;                       00011100
VALUE DECIN ;                                                           00011200
BEGIN     COMMENT   CHKS FOR AN INTEGER OF 1-7 DIGS, IF 8 DIGS OR       00011300
A NON-NUMER THEN INTONLY = FALSE ;                                      00011400
LOCAL T,S ;                                                             00011500
SI ~ LOC                                                                00011600
     DECIN; S ~ SI ; DI ~ OCTOUT ;                                      00011700
8(IF SC = " " THEN BEGIN T ~ TALLY ; SI ~ S ; DS ~ T OCT ;              00011800
TALLY        ~ 1 ; JUMP OUT END ;                                       00011900
IF SC < "0" THEN BEGIN TALLY ~ 0 ; JUMP OUT ; END ELSE                  00012000
BEGIN SI ~ SI + 1 ;     TALLY ~ TALLY + 1 END ) ;                       00012100
INTONLY ~ TALLY ;                                                       00012200
END INTONLY ;                                                           00012300
%***********************************************************************00012400
BOOLEAN STREAM PROCEDURE POUNDORDOLLAR (JUNK) ;                         00012500
BEGIN       SI~JUNK;   IF SC = "#"THEN TALLY~1 ELSE IF SC = "$" THEN    00012600
TALLY ~1;    POUNDORDOLLAR ~ TALLY   END ;                              00012700
%***********************************************************************00012800
STREAM PROCEDURE CONV (OCTIN,ALPHOUT);                                  00012900
COMMENT  CONVERTS OCTAL TO DECIMAL ;                                    00013000
BEGIN SI~OCTIN ; DI ~ ALPHOUT; DS ~ 8 DEC ;                             00013100
END CONV ;                                                              00013200
%***********************************************************************00013300
STREAM PROCEDURE SETARROW (CDINPUT) ;                                   00013400
COMMENT SETARROW BLANKS COLS 77-80 AND PUTS AN ARROW AFTER ;            00013500
BEGIN    DI ~ CDINPUT ; 2(DI~DI+38) ; DS~ 5 LIT "    ~"; END SETARROW;  00013600
%***********************************************************************00013700
STREAM PROCEDURE SENDZIPORPAR (A ,TYPEARRAY) ;                          00013800
              COMMENT SETS UP PAR OR ZIP INFO FOR SELECTOR/MESS ;       00013900
BEGIN                                                                   00014000
DI ~ TYPEARRAY ;                                                        00014100
SI ~ A ; SI ~ SI + 4 ; 2(DS~36 CHR) ;                                   00014200
END SENDZIPORPAR ;                                                      00014300
%***********************************************************************00014400
STREAM PROCEDURE MOVEAPHRASE (FROM,DEST,LENGTH,CODE,AT,NUMCHARS,        00014500
SKIPDEST) ;           VALUE  NUMCHARS,SKIPDEST ;                        00014600
COMMENT  SOURCE IS FROM (THE START OF ARRAY PASSED) IF LENGTH IS 0 OR   00014700
AT (POSITION IN ARRAY LEFT PREVIOUSLY). UP TO NUMCHARSS OF ARRAY IS     00014800
SCANNED EACH TIME.  LESS THAN NUMCHARS IF SPACE OR ARROW.  INFO TIL NUM-00014900
CHARS, SPACE OR ARROW IS PUT INTO DEST STARTING AT THE SKIPDEST + 1TH   00015000
CHARACTER.  CODE WILL BE 1 IF INFO IS FOUND AND THEN A SPACE OR ARROW.  00015100
CODE IS 2 IF NO INFO FOUND AND THEN AN ARROW.  CODE IS 2 IF NO INFO     00015200
FOUND AND THEN AN ARROW.  CODE IS 8 IF AFTER NUMCHARS IS FOUND THE LAST 00015300
CHAR WAS NOT A SPACE OR ARROW.  ON REENTRANT IF 1 THEN ON SPACE OR      00015400
ARROW.  IF 8 THEN ON INFO.  NO REENTRANT IF CODE IS 2.4 MEANDILSEGIT-   00015500
IMATE CHAR ;                                                            00015600
BEGIN  LABEL  L,N,EX ;            LOCAL  TRA,COD,LENGT ;                00015700
SI ~ LENGTH ; SI ~ SI + 7 ;                                             00015800
IF SC = "0" THEN SI ~ FROM ELSE BEGIN SI ~ AT; DI ~ LOC TRA;            00015900
DS ~ WDS ; SI ~ TRA END ;                                               00016000
DI ~ DEST; SKIPDEST(DS~ LIT "0"); DS~ 18 LIT " ";                       00016100
DI ~ DEST ; DI ~ DI + SKIPDEST ;                                        00016200
L: IF SC = " " THEN BEGIN SI ~ SI + 1 ; GO TO L END;                    00016300
IF SC = "~" THEN BEGIN LENGT ~ TALLY; TALLY ~ 2; COD ~ TALLY; END ELSE  00016400
BEGIN      NUMCHARS( IF SC = " " THEN JUMP OUT TO N;                    00016500
IF SC = "~" THEN JUMP OUT TO N;                                         00016600
TALLY ~ TALLY + 1 ; DS ~ CHR ) ;                                        00016700
LENGT ~ TALLY ;  TALLY ~ 8 ;  COD ~ TALLY ; GO TO EX ;                  00016800
N: LENGT ~ TALLY ; TALLY ~ 1 ; COD ~ TALLY ;                            00016900
EX:  TRA ~ SI ;  SI ~ LOC TRA ; DI ~ AT ; DS ~ WDS  END ;               00017000
SI ~LOC COD ; DI ~ CODE ;      DS ~ WDS ;   SI ~ LOC LENGT ;            00017100
DI ~ LENGTH ;    DS ~ WDS ;           END MOVEAPHRASE ;                 00017200
%***********************************************************************00017300
PROCEDURE  SEQCHK ;                                                     00017400
COMMENT CHECKS SEQUENCE NOS--CANCELS IT THRU SETARROW--SETS LENGTH TO   00017500
0 ----GLOBAL IS SEQNO,LASTNO,PTR ;                                      00017600
BEGIN                                                                   00017700
IF SEQNO < LASTNO THEN       WRITE(PTR,SEQMSG);                         00017800
LASTNO ~ SEQNO ;                  LENGTH ~ 0 ;                          00017900
END SEQCHK ;                                                            00018000
%***********************************************************************00018100
PROCEDURE EXCESSINFO;                                                   00018200
COMMENT  SKIPS CHARS OF FILE OR FIELDNAME > 7 OR > 24 RESPECTIVELY ---  00018300
PRO MOVEAPHRASE AND CODE ARE GLOBAL ;                                   00018400
BEGIN        LABEL   CODE8 ;                                            00018500
CODE8: IF CODE = 8 THEN  BEGIN  MOVEAPHRASE(A,JUNK,LENGTH,CODE,AT,8,1) ;00018600
GO TO CODE8 END;       END EXCESSINFO ;                                 00018700
%***********************************************************************00018800
PROCEDURE WRITEINFO ;                                                   00018900
COMMENT WRITES ERRORS OR GOOD CARD---- GLOBAL TO THIS IS PTR,ERR,       00019000
ERRORS,A ;                                                              00019100
IF ERR > 0 THEN BEGIN WRITE (PTR,ERRORS[ERR]) ; SYNTAX ~ TRUE ;         00019200
SETARROW(A) ;                                                           00019300
WRITE(PTR[NO],10,A[*]) ; WRITE (PTR,FT,IF CARDNAME = "LEAD " THEN       00019400
LASTNO ELSE SEQNO) ;                                                    00019500
WRITE (PTR); ERR~0; END ELSE IF MON OR SYNTAX THEN  BEGIN               00019600
WRITE(PTR[NO],10,A[*]) ; WRITE (PTR,FT,IF CARDNAME = "LEAD " THEN       00019700
LASTNO ELSE SEQNO) ;                WRITE(PTR) END ;                    00019800
COMMENT  ENDOF WRITEINFO ;                                              00019900
%***********************************************************************00020000
BOOLEAN PROCEDURE PUTFIELDNAME ;                                        00020100
BEGIN REAL PONDFIELD ;   LABEL EOPUTFIELDNAME ;                         00020200
%***********************************************************************00020300
STREAM PROCEDURE DSFIELDNAME(JUNK,FIELDNAME) ;                          00020400
BEGIN  SI~JUNK; SI~SI+1;DI~FIELDNAME; DI~DI+17; DS~6 CHR END ;          00020500
%***********************************************************************00020600
IF JUNK[0].[42:6] ! " " OR JUNK[0].[6:6] =" " OR JUNK[0].[36:6] = " "   00020700
THEN BEGIN ERR ~ 22; GO TO EOPUTFIELDNAME  END ;                        00020800
IF NOT INTONLY(JUNK[0].[6:42],PONDFIELD) THEN BEGIN ERR~22; GO TO       00020900
EOPUTFIELDNAME END ;   PUTFIELDNAME ~ TRUE ;                            00021000
DSFIELDNAME (JUNK,FIELDNAME) ;                                          00021100
EOPUTFIELDNAME: END ;                                                   00021200
%***********************************************************************00021300
                                                                        00021400
PROCEDURE PLUGCARD ;  COMMENT  PLUGCARD CONSISTS OF PLUG AND FIELD-NAME 00021500
IN FREE-FIELD FORMAT (PLUG MUST BE IN COLS 1-4) .  GLOBAL IS:           00021600
MOVEAPHRASE,SORTZIPPARPACKET,JUNK,LENGTH,AT,A,ERR,WRITEINFO,CRD,CODE,   00021700
SETARROW,GETFIELDNAME,PLUGIN, AND PLUGBOOL ;                            00021800
BEGIN       INTEGER  I ;    LABEL   EOPLUG ;                            00021900
READ(CRD,10,A[*]) ;      SETARROW(A) ;                                  00022000
MOVEAPHRASE(A,JUNK,LENGTH,CODE,AT,18,0)  ;                              00022100
IF CODE = 2 THEN BEGIN  ERR~23; GO TO EOPLUG  END;                      00022200
MOVEAPHRASE(A,JUNK,LENGTH,CODE,AT,16,0)  ;                              00022300
IF CODE = 2 THEN BEGIN  ERR~23; GO TO EOPLUG  END;                      00022400
IF ITSPLUGGED(JUNK,PLUGNAMES,PLUGMAX+1, I) THEN                         00022500
BEGIN  PLUGBOOL[I] ~ PLUGIN ~ TRUE ;  GO TO EOPLUG  END ;               00022600
ERR ~ 24 ;                                                              00022700
EOPLUG:  WRITEINFO ; GO TO SORTZIPPARPACKET  END  PLUGCARD  ;           00022800
%***********************************************************************00022900
PROCEDURE SKIPCARD;                                                     00023000
COMMENT READS SKIPCARD AND SETS UP SKIP INFO;                           00023100
BEGIN                                     LABEL EOSKIP;                 00023200
READ (CRD,10,A[*]);                                                     00023300
SETARROW (A) ;                                                          00023400
MOVEAPHRASE (A, JUNK, LENGTH, CODE, AT, 5, 0) ;                         00023500
MOVEAPHRASE (A, SKIPNO, LENGTH, CODE, AT, 6, 2) ;                       00023600
IF NOT INTONLY(SKIPNO, OCTSKIP) THEN BEGIN ERR ~ 27; GO TO EOSKIP END;  00023700
EOSKIP: WRITEINFO; GO TO SORTZIPPARPACKET                               00023800
END OF SKIPCARD;                                                        00023900
%***********************************************************************00024000
PROCEDURE RANGECARD ;  BEGIN  LABEL  EORANGE ;                          00024100
INTEGER PROCEDURE YRDAYTOJUL(I);                                        00024200
VALUE I; INTEGER I;                                                     00024300
YRDAYTOJUL~ 365|(I DIV 1000 - 8) +    (I DIV 1000 - 9) DIV 4            00024400
+ I MOD 1000 + 3;                                                       00024500
READ(CRD,10,A[*]) ;  SETARROW(A) ;                                      00024600
MOVEAPHRASE(A,JUNK,LENGTH,CODE,AT,18,0) ;                               00024700
MOVEAPHRASE(A,JUNK,LENGTH,CODE,AT,18,0) ;                               00024800
IF CODE = 2 THEN BEGIN ERR~25 ; GO TO EORANGE END ;                     00024900
IF NOT INTONLY (JUNK[0], LOWRANGE ) THEN                                00025000
BEGIN ERR~25;                                                           00025100
GO TO EORANGE END;                                                      00025200
MOVEAPHRASE(A,JUNK,LENGTH,CODE,AT,18,0) ;                               00025300
IF CODE = 2 THEN BEGIN ERR~25 ; GO TO EORANGE END ;                     00025400
IF NOT INTONLY (JUNK[0],HIGHRANGE ) THEN                                00025500
BEGIN ERR~25;                                                           00025600
GO TO EORANGE END;                                                      00025700
LOWRANGE~YRDAYTOJUL(LOWRANGE);                                          00025800
RANGE ~ TRUE ;                                                          00025900
EORANGE: WRITEINFO ; GO TO SORTZIPPARPACKET END RANGECARD ;             00026000
%***********************************************************************00026100
PROCEDURE LIMITCARD;                                                    00026200
COMMENT READS LIMITCARD AND SET UP LIMIT INFO ;                         00026300
BEGIN     LABEL EOLIMIT;                                                00026400
READ (CRD,10, A[*]) ;                                                   00026500
SETARROW(A) ;                                                           00026600
MOVEAPHRASE(A, JUNK, LENGTH, CODE, AT, 5, 0) ;                          00026700
MOVEAPHRASE(A, LIMITNO, LENGTH, CODE, AT, 6, 2);                        00026800
IF NOT INTONLY(LIMITNO,OCTLIMIT)                                        00026900
     THEN BEGIN ERR ~ 28; GO TO EOLIMIT END;                            00027000
     EOLIMIT: WRITEINFO; GO TO SORTZIPPARPACKET END OF LIMITCARD;       00027100
%***********************************************************************00027200
PROCEDURE ZIPCARD ;  COMMENT ZIPCARD PROCESSES UP TO 3 ZIP CARDS        00027300
AND PASSES COLS 5-76 (9WDS) TO ZIPARRAY WHICH IS GLOBAL--- OTHER GLOBAL 00027400
ARE : SORTZIPPARPACKET,MOVEAPHRASE,JUNK,LENGTH,AT,ZIPNO,CRD,A,ERR,      00027500
WRITEINFO,CODE,SENDZIPORPAR,ZIPARRAY ;                                  00027600
BEGIN   READ (CRD,10,A[*]) ;       SETARROW(A) ;                        00027700
IF ZIPNO > 10 THEN BEGIN                                                00027800
ERR ~ 6 ; WRITEINFO; GO TO SORTZIPPARPACKET END ;                       00027900
SENDZIPORPAR(A,ZIPARRAY[(ZIPNO - 1)| 9] );                              00028000
WRITEINFO ;                                                             00028100
                                             GO TO SORTZIPPARPACKET END;00028200
%***********************************************************************00028300
PROCEDURE SORTCARD ;                                                    00028400
COMMENT SHECKS ALL INFO ON SORT CARD AND SETS UP SORTARRAY;             00028500
BEGIN INTEGER Q,FIELDANDFILE;                                           00028600
LABEL  EOSORT,EOD ;                                                     00028700
%***********************************************************************00028800
INTEGER STREAM PROCEDURE ASCENDORDESCEND (JUNK) ;                       00028900
COMMENT  DETERMINES IF SORT IS ASCENDING OR DESCENDING ;                00029000
BEGIN       LOCAL  SEQUENCE,SEQUENCE2 ;                                 00029100
DI ~LOC SEQUENCE ; DS ~ 11 LIT "DESCENDING " ;                          00029200
SI ~ JUNK ;  DI ~ LOC SEQUENCE ;                                        00029300
IF 11 SC = DC THEN TALLY ~ 2 ELSE BEGIN  DI ~LOC SEQUENCE; DS ~         00029400
10 LIT "ASCENDING ";                                                    00029500
SI ~JUNK ; DI ~LOC SEQUENCE ;   IF 10 SC = DC THEN TALLY ~ 1 END ;      00029600
ASCENDORDESCEND        ~ TALLY ;                                        00029700
END ASCENDORDESCEND ;                                                   00029800
%***********************************************************************00029900
STREAM PROCEDURE SETUPSORTARRAY (FIELDNAME,SORTARRAY,Q) ;               00030000
VALUE  Q ;                                                              00030100
BEGIN                                                                   00030200
SI~FIELDNAME ; SI~SI+17 ; DI~SORTARRAY ;                                00030300
DI ~ DI + 1 ;  DS ~ 5 CHR      ;    SI ~ LOC Q ; SI ~ SI + 7 ;          00030400
IF SC = "1" THEN DS ~ 1 LIT "1" ELSE DS ~ LIT "0" ;                     00030500
END SETUPSORTARRAY;                                                     00030600
%***********************************************************************00030700
READ (CRD,10,A[*]) ; SETARROW(A) ;         IF SORTNO > 99 THEN          00030800
BEGIN  ERR ~ 8 ; WRITEINFO ;  GO TO SORTZIPPARPACKET  END ;             00030900
MOVEAPHRASE (A,JUNK,LENGTH,CODE,AT,18,0) ;                              00031000
IF CODE = 2 THEN BEGIN ERR ~ 9 ; GO TO EOSORT  END ;                    00031100
MOVEAPHRASE ( A,JUNK,LENGTH,CODE,AT,11,0) ;                             00031200
EXCESSINFO ;                                                            00031300
IF CODE = 2 THEN BEGIN ERR ~ 9 ; GO TO EOSORT  END ;                    00031400
IF (Q ~ ASCENDORDESCEND (JUNK) ) = 0 THEN BEGIN                         00031500
ERR ~10 ; GO TO EOSORT  END ;                                           00031600
MOVEAPHRASE (A,JUNK,LENGTH,CODE,AT,16,0) ;                              00031700
IF CODE = 2 THEN BEGIN ERR ~ 9 ; GO TO EOSORT  END ;                    00031800
IF POUNDORDOLLAR(JUNK) THEN IF NOT PUTFIELDNAME THEN GO TO EOSORT ELSE  00031900
BEGIN FIELDANDFILE ~ 2 ; GO TO EOD END ;                                00032000
WHILE TRUE DO BEGIN                                                     00032100
READ(DICTION,3,FIELDNAME[*]) [EOD] ;                                    00032200
IF GETFIELDNAME (JUNK,FIELDNAME) THEN BEGIN                             00032300
FIELDANDFILE ~2; GO TO EOD END; END;                                    00032400
                                                                        00032500
                                                                        00032600
EOD:  REWIND(DICTION) ;  IF FIELDANDFILE =0 THEN BEGIN ERR~11; GO TO    00032700
EOSORT  END;                                                            00032800
                                                                        00032900
SETUPSORTARRAY (FIELDNAME,SORTARRAY[SORTNO],Q);                         00033000
EOSORT: WRITEINFO;  GO TO SORTZIPPARPACKET ;                            00033100
END SORTCARD ;                                                          00033200
%***********************************************************************00033300
PROCEDURE  IDCARD ;                                                     00033400
COMMENT  READS ID CARDS AND SETS UP ID INFO EXCEPT NUM OF IDS           00033500
AND NUM OF PACKETS ;                                                    00033600
BEGIN  INTEGER FIELDANDFILE ;                                           00033700
LABEL  EOID,EOD ;                                                       00033800
%***********************************************************************00033900
STREAM PROCEDURE SETSELECT1 (FIELDNAME,SELECTARRAY) ;                   00034000
BEGIN  SI~FIELDNAME ; SI~SI+17 ;  DI~SELECTARRAY; DI~DI+1 ;             00034100
DS ~ 5 CHR ; DI ~DI + 1 ;                                               00034200
DS ~ CHR ;                                                              00034300
END SETSELECT1 ;                                                        00034400
%***********************************************************************00034500
STREAM PROCEDURE SETSELECT2 (SELECTARRAY,Q) ;      VALUE  Q ;           00034600
BEGIN  SI ~LOC Q ;   SI ~SI + 7 ;                                       00034700
DI ~SELECTARRAY ;  DI ~ DI + 6 ;     DS ~ CHR ;                         00034800
END SETSELECT2 ;                                                        00034900
%***********************************************************************00035000
STREAM PROCEDURE SETSELECT3 (AT,SELECTARRAY) ;    VALUE AT;             00035100
BEGIN          DI ~ SELECTARRAY ; DS ~ 16 LIT " " ; DI ~ DI - 16 ;      00035200
SI ~ AT ;           SI ~ SI + 1 ;                                       00035300
16(IF SC ! "~" THEN DS ~ 1 CHR ELSE JUMP OUT) ;                         00035400
END SETSELECT3 ;                                                        00035500
%***********************************************************************00035600
READ (CRD,10,A[*]) ;               SETARROW(A) ;                        00035700
IF IDPOS > 496 THEN BEGIN                                               00035800
WRITE (PTR,TOOMANYIDS) ;  GO TO EOJ  END ;                              00035900
MOVEAPHRASE (A,JUNK,LENGTH,CODE,AT, 3,0) ;                              00036000
MOVEAPHRASE (A,JUNK,LENGTH,CODE,AT,24,0) ;                              00036100
IF CODE = 2 THEN BEGIN ERR ~ 14 ; GO TO EOID END ;                      00036200
IF POUNDORDOLLAR(JUNK) THEN IF NOT PUTFIELDNAME THEN GO TO EOID   ELSE  00036300
BEGIN FIELDANDFILE ~ 2 ; GO TO EOD END ;                                00036400
WHILE TRUE DO BEGIN                                                     00036500
READ(DICTION,3,FIELDNAME[*]) [EOD] ;                                    00036600
IF GETFIELDNAME (JUNK,FIELDNAME) THEN BEGIN                             00036700
FIELDANDFILE ~ 2; GO TO EOD  END  END;                                  00036800
                                                                        00036900
                                                                        00037000
                                                                        00037100
EOD:  REWIND(DICTION) ;  IF FIELDANDFILE =0 THEN BEGIN ERR~11; GO TO    00037200
EOID END;                                                               00037300
IDPOS ~ IDPOS + 1 ;                                                     00037400
SETSELECT1(FIELDNAME[0], SELECTARRAY[IDPOS]);                           00037500
EXCESSINFO ;                                                            00037600
IF CODE = 2 THEN BEGIN ERR ~ 14 ; GO TO EOID END ;                      00037700
IF CODE = 4 THEN BEGIN ERR ~ 15 ; GO TO EOID END ;                      00037800
MOVEAPHRASE (A,JUNA,LENGTH,CODE,AT,2,6) ;                               00037900
IF JUNA[0] = "= " THEN      Q ~ 0 ELSE Q ~ 7 ;                          00038000
IF JUNA [0] =   "! " THEN Q ~ 1 ;                                       00038100
IF JUNA [0] =   "> " THEN Q ~ 2 ;                                       00038200
IF JUNA [0] =   "} " THEN Q ~ 3 ;                                       00038300
IF JUNA [0] =   "< " THEN Q ~ 4 ;                                       00038400
IF JUNA [0] =   "{ " THEN Q ~ 5 ;                                       00038500
IF JUNA [0] =   "AN" THEN Q ~ 6 ;                                       00038600
IF Q = 7 THEN BEGIN ERR~16; GO TO EOID END;                             00038700
SETSELECT2 (SELECTARRAY[IDPOS], Q);                                     00038800
IDPOS ~ IDPOS + 1 ;                                                     00038900
SETSELECT3(AT, SELECTARRAY[IDPOS]);                                     00039000
IDPOS ~ IDPOS + 1 ;                                                     00039100
EOID: WRITEINFO;  GO TO IDORACTION ;                                    00039200
END IDCARD ;                                                            00039300
%***********************************************************************00039400
PROCEDURE  ACTCARD ;                                                    00039500
BEGIN INTEGER FIELDANDFILE ;  LABEL  EOACT,EOD;                         00039600
%***********************************************************************00039700
STREAM PROCEDURE ACTIONINFO(AT,ACTIONARRAY,FIELDNAME); VALUE AT;        00039800
BEGIN LOCAL  X,Y;  LABEL STRIP;  SI~AT; SI~SI+1;                        00039900
DI ~ ACTIONARRAY; DS ~ 48 LIT " " ; DI ~ DI - 48;                       00040000
X~DI; DI~LOC Y; DS~6 LIT"STRIP "; DI ~ DI -6 ;                          00040100
                                   IF 6 SC =DC THEN BEGIN  DI~          00040200
FIELDNAME; DI~DI+6; DS~1 LIT"1"; GO TO STRIP; END ELSE BEGIN DI~        00040300
FIELDNAME; DI~DI+6;DS~1 LIT"0" END;      SI~SI-6; DI~X;                 00040400
48 ( IF SC ! "~" THEN DS ~ 1 CHR ELSE JUMP OUT) ;                       00040500
STRIP:                                                                  00040600
END ACTIONINFO ;                                                        00040700
%***********************************************************************00040800
STREAM PROCEDURE NUMINFO(ACTIONARRAY,FIELDNAME) ;   BEGIN               00040900
SI ~ FIELDNAME; DI ~ ACTIONARRAY; DS ~ 1 WDS;  END NUMINFO ;            00041000
%***********************************************************************00041100
READ (CRD,10,A[*]) [EOF] ;         SETARROW(A) ;                        00041200
IF ACTNO > 35 THEN BEGIN WRITE(PTR,TOOMANYACTIONS); GO TO EOJ END ;     00041300
MOVEAPHRASE (A,JUNK,LENGTH,CODE,AT,24,0) ;                              00041400
IF CODE =2 THEN BEGIN ERR ~18 ; GO TO EOACT  END ;                      00041500
MOVEAPHRASE (A,JUNK,LENGTH,CODE,AT,24,0) ;                              00041600
IF CODE =2 THEN BEGIN ERR ~18 ; GO TO EOACT  END ;                      00041700
IF POUNDORDOLLAR(JUNK) THEN IF NOT PUTFIELDNAME THEN GO TO EOACT  ELSE  00041800
BEGIN FIELDANDFILE ~ 2;GO TO EOD END ;                                  00041900
WHILE TRUE DO BEGIN                                                     00042000
READ(DICTION,3,FIELDNAME[*])        [EOD] ;                             00042100
IF GETFIELDNAME(JUNK,FIELDNAME) THEN BEGIN FIELDANDFILE ~2; GO TO EOD   00042200
END  END  ;                                                             00042300
                                                                        00042400
EOD:  REWIND(DICTION) ;  IF FIELDANDFILE =0 THEN BEGIN ERR~11; GO TO    00042500
EOACT END ;                                                             00042600
                                                                        00042700
                                                                        00042800
                                                                        00042900
                                                                        00043000
                                                                        00043100
                                                                        00043200
EXCESSINFO ;       IF CODE = 2 THEN BEGIN                               00043300
ERR ~ 18 ;  GO TO EOACT END ;                                           00043400
IF LENGTH > 38 THEN BEGIN ERR ~ 19 ; GO TO EOACT END ;                  00043500
Q ~ 7 | ACTNO - 5 ;                                                     00043600
ACTIONINFO(AT,ACTIONARRAY[PACKETNO,Q],FIELDNAME[2]) ;                   00043700
Q~7|ACTNO-6; BEGIN NUMINFO(ACTIONARRAY[                                 00043800
PACKETNO,Q],FIELDNAME[2]) ; ACTION[PACKETNO] ~ TRUE; IF ACTNO >ACTIONMAX00043900
 THEN ACTIONMAX~ACTNO; CONV(ACTNO,ALPHACT) ;                            00044000
ACTIONARRAY[PACKETNO,0] ~ALPHACT END;                                   00044100
EOACT: WRITEINFO ;                                                      00044200
END ACTIONCARD ;                                                        00044300
%***********************************************************************00044400
PROCEDURE  MOVECARD ;  BEGIN INTEGER FIELDANDFILE;  BOOLEAN TWOTIMES;   00044500
BOOLEAN  DC ;                                                           00044600
LABEL  EOD,EOMOVE,DOTWICE ;                                             00044700
%***********************************************************************00044800
STREAM PROCEDURE NUMINFO(ACTIONARRAY,FIELDNAME) ;    BEGIN              00044900
SI ~ FIELDNAME;  DI ~ ACTIONARRAY;  DS~ 1 LIT"1" ;  SI~SI +1 ; DS~2 CHR;00045000
           DI~DI+3; DS        ~3 CHR END;                               00045100
%***********************************************************************00045200
STREAM PROCEDURE  NUMINFO3(ACTIONARRAY,FIELDNAME) ;  BEGIN              00045300
SI ~ FIELDNAME ; DI ~ ACTIONARRAY; DS~ 1 LIT"2" ; SI~SI+1; DS~2 CHR;    00045400
DI~DI+3; DS ~ 3 CHR END ;                                               00045500
%***********************************************************************00045600
STREAM PROCEDURE NUMINFO2 (ACTIONARRAY,FIELDNAME) ;   BEGIN             00045700
SI ~ FIELDNAME ; DI ~ ACTIONARRAY;  SI ~SI+1; DI ~DI+1;                 00045800
IF 2 SC < DC THEN BEGIN SI ~ SI-2; DI~DI-2; DS~2 CHR END;  DS~3 CHR END;00045900
%***********************************************************************00046000
READ(CRD,10,A[*]) [EOF] ; SETARROW(A) ;                                 00046100
IF ACTNO       > 17 THEN BEGIN WRITE(PTR,TOOMANYACTIONS) ;GO TO EOJ END;00046200
MOVEAPHRASE(A,JUNK,LENGTH,CODE,AT,24,0) ;                               00046300
IF CODE = 2 THEN BEGIN ERR~20; GO TO EOMOVE END;                        00046400
DOTWICE:  MOVEAPHRASE(A,JUNK,LENGTH,CODE,AT,24,0) ;                     00046500
IF CODE = 2 THEN BEGIN ERR~20; GO TO EOMOVE END ;                       00046600
IF POUNDORDOLLAR(JUNK) THEN IF NOT PUTFIELDNAME THEN GO TO EOMOVE ELSE  00046700
BEGIN FIELDANDFILE ~ 2 ; GO TO EOD END ;                                00046800
WHILE TRUE DO BEGIN READ(DICTION,3,FIELDNAME[*]) [EOD]                  00046900
;  IF GETFIELDNAME(JUNK,FIELDNAME) THEN BEGIN FIELDANDFILE~2; GO TO EOD 00047000
END END ;                                                               00047100
EOD:  REWIND(DICTION) ;  IF FIELDANDFILE =0 THEN BEGIN ERR~11; GO TO    00047200
EOMOVE                                                                  00047300
END ;        IF NOT TWOTIMES THEN BEGIN EXCESSINFO; IF CODE = 2 THEN    00047400
BEGIN ERR~20; GO TO EOMOVE END END;                                     00047500
Q~ 7 |ACTNO-6 ;    IF NOT SYNTAX AND NOT TWOTIMES THEN BEGIN            00047600
IF DC THEN NUMINFO3(ACTIONARRAY[PACKETNO,Q],FIELDNAME[2]) ELSE          00047700
NUMINFO(ACTIONARRAY[PACKETNO,Q],FIELDNAME[2]) ; ACTION[PACKETNO]~TRUE ; 00047800
IF ACTNO > ACTIONMAX THEN ACTIONMAX ~ACTNO ; CONV(ACTNO,ALPHACT) ;      00047900
ACTIONARRAY[PACKETNO,0]~ALPHACT  END ;                                  00048000
IF NOT TWOTIMES THEN BEGIN TWOTIMES~TRUE; MOVEAPHRASE(A,JUNA,LENGTH,    00048100
CODE,AT,3,5); IF JUNA[0] ! "TO " AND JUNA[0] ! "DC " THEN BEGIN ERR~21; 00048200
GO TO EOMOVE END ;       IF JUNA[0] = "DC " THEN DC ~ TRUE ;            00048300
GO TO DOTWICE END; NUMINFO2(ACTIONARRAY[                                00048400
PACKETNO,Q],FIELDNAME[2]) ;    EOMOVE: WRITEINFO; END MOVECARD;         00048500
%***********************************************************************00048600
COMMENT  MAINPRO ;                                                      00048700
FILL PLUGNAMES[*] WITH                                                  00048800
0;                                                                      00048900
IF MON OR SYNTAX THEN                                                   00049000
WRITE(PTR,HEADING) ;                                                    00049100
READ (CRD[NO],LEADFT,CARDNAME,LASTNO) [NOCARDS] ;                       00049200
IF CARDNAME ! "LEAD " THEN BEGIN  WRITE (PTR,BADISTCD) ; GO TO EOJ END; 00049300
READ (CRD,10,A[*]) ;                                                    00049400
SETARROW    (A)   ;                                                     00049500
MOVEAPHRASE (A,JUNK,LENGTH,CODE,AT,5,0) ;                               00049600
MOVEAPHRASE (A,FILENAME,LENGTH,CODE,AT,8,1) ;                           00049700
EXCESSINFO;                                                             00049800
IF CODE = 2 THEN BEGIN WRITE(PTR,NOTNUFLEAD) ; GO TO EOJ END ;          00049900
IF CODE = 4 THEN BEGIN ERR ~5; GO TO EOLEAD END ;                       00050000
                                                                        00050100
IF FILENAME[0] ! "DUMMY  " THEN FILL DICTION WITH FILENAME[0],* ;       00050200
SELECTOPT:=TRUE;                                   %JF                  00050300
                                                                        00050400
                                                                        00050500
                                                                        00050600
                                                                        00050700
                                                                        00050800
                                                                        00050900
                                                                        00051000
                                                                        00051100
                                                                        00051200
                                                                        00051300
                                                                        00051400
FOR I ~ 0,1 DO BEGIN                                                    00051500
MOVEAPHRASE(A,BLOCKING[I,*],LENGTH,CODE,AT,6,2) ;                       00051600
IF CODE = 8 THEN BEGIN ERR~ 2 ; GO TO EOLEAD END ;                      00051700
IF CODE = 2 THEN BEGIN WRITE (PTR,NOTNUFLEAD);GO TO EOJ END ;           00051800
IF NOT INTONLY (BLOCKING[I,0],OCTWDS[I]) THEN BEGIN ERR~3; GO TO EOLEAD 00051900
END END ;                                                               00052000
MOVEAPHRASE(A,BLOCKING[2,*],LENGTH,CODE,AT,6,2) ;  IF CODE = 2 THEN     00052100
GO TO EOLEAD ;   IF INTONLY(BLOCKING[2,0],OCTWDS[2]) THEN BEGIN         00052200
IF CODE = 8 THEN BEGIN ERR~2 ; GO TO EOLEAD END ;                       00052300
MOVEAPHRASE(A,BLOCKING[3,*],LENGTH,CODE,AT,6,2);                        00052400
IF CODE = 2 THEN BEGIN ERR~ 3; GO TO EOLEAD END ;                       00052500
IF CODE = 8 THEN BEGIN ERR~2 ;GO TO EOLEAD END ;                        00052600
IF NOT INTONLY(BLOCKING[3,0],OCTWDS[3]) THEN BEGIN ERR~3; GO TO EOLEAD  00052700
END ;        MOVEAPHRASE(A,BLOCKING[2,*],      LENGTH,CODE,AT,6,2) ;    00052800
IF CODE = 2 THEN GO TO EOLEAD END ;      JUNA[0] ~ BLOCKING[2,0];       00052900
OPTNSCAN:                                                           %JPA00053000
    IF JUNA[0] = "SYNTAX" THEN SYNTAX ~ TRUE                        %JPA00053100
    ELSE IF JUNA[0] = "MON   " THEN MON ~ TRUE                      %JPA00053200
    ELSE IF JUNA[0] = "COUNT " THEN MUSTCOUNT ~ TRUE                %JPA00053300
    ELSE IF JUNA[0] = "SELECT" THEN SELECTOPT ~ TRUE                %JPA00053400
    ELSE IF JUNA[0] = "MAINTA" THEN SELECTOPT ~ FALSE               %JPA00053500
    ELSE IF JUNA[0] = "TRACE " THEN TRACEOPT ~ TRUE                 %JPA00053600
    ELSE IF JUNA[0] = "EXPLOD" THEN                                 %JPA00053700
    BEGIN                                                           %JPA00053800
         SELECTOPT ~ FALSE;                                         %JPA00053900
         EXPLODOPT ~ TRUE;                                          %JPA00054000
    END                                                             %JPA00054100
    ELSE                                                            %JPA00054200
    BEGIN                                                           %JPA00054300
         ERR ~ 26;                                                  %JPA00054400
         GO TO EOLEAD;                                              %JPA00054500
    END;                                                            %JPA00054600
    MOVEAPHRASE (A,JUNA,LENGTH,CODE,AT,6,2);                        %JPA00054700
    IF CODE ! 2 THEN GO TO OPTNSCAN;                                %JPA00054800
% 50300 THRU 50660 ARE CHANGES TO ALLOW NEW OPTIONS TO BE           %JPA00054900
% SCANNED FOR MULTIPLE HITS ON RECORDS                              %JPA00055000
EOLEAD:  WRITEINFO;                                                     00055100
IDPOS ~ 1 ;            POS ~ 1 ;                                        00055200
SORTZIPPARPACKET:                                                       00055300
READ (CRD[NO],TYPEFT2                                                   00055400
                    ,CARDNAME,SEQNO) [EOF] ;                            00055500
SEQCHK;                                                                 00055600
WHATCARD:                                                               00055700
IF CARDNAME = "ZIP"  THEN BEGIN ZIPNO~ZIPNO + 1; ZIPCARD END ;          00055800
IF CARDNAME = "PLU" THEN PLUGCARD ;                                     00055900
IF CARDNAME = "RAN" THEN RANGECARD ;                                    00056000
IF CARDNAME = "PAR" THEN BEGIN    READ(CRD,10,A[*]) ; WRITE(PTR,10,A[*])00056100
;   WRITE (PTR) ;                                                       00056200
BEGIN   SAVE FILE OUT SUPCARD DISK SERIAL [20:50] (2,10,150,SAVE 5) ;   00056300
PARAMS ~ 0 ;                                                            00056400
WHILE TRUE DO BEGIN    READ(CRD,10,A[*]) [EOF] ;                        00056500
IF MON THEN WRITE (PTR, 10, A[*]) ;                                     00056600
IF NOT SYNTAX THEN WRITE(SUPCARD,10,A[*])       END END END ;           00056700
IF CARDNAME = "SKI" THEN SKIPCARD;                                      00056800
IF CARDNAME = "LIM" THEN LIMITCARD;                                     00056900
IF CARDNAME = "SOR"  THEN BEGIN SORTNO~ SORTNO+ 1 ; SORTCARD END;       00057000
IF CARDNAME ! "PAC"  THEN BEGIN                                         00057100
IF SWI = 1 THEN BEGIN ERR~ 13; SWI ~ 0; END ELSE ERR ~ 4 ;              00057200
READ (CRD,10,A[*]) ;  WRITEINFO;  GO TO SORTZIPPARPACKET  END ;         00057300
PACKETNO ~ PACKETNO + 1 ; IDNO ~ 0 ; ACTNO ~ 0 ;                        00057400
SWITC ~ FALSE ;  FIRST := 1;                                            00057500
IF PACKETNO > PACKETMAX                                                 00057600
                 THEN BEGIN   WRITE (PTR,TOOMANYPACKETS) ;              00057700
GO TO EOJ END ;                                                         00057800
READ (CRD,10,A[*]) ; SETARROW(A) ;  WRITEINFO ;                         00057900
IDORACTION:                                                             00058000
READ(CRD[NO],TYPEFT2,CARDNAME,SEQNO) [EOF] ; SEQCHK ;                   00058100
IF CARDNAME =  "ID " THEN BEGIN IDNO ~ IDNO + 1; IDCARD END;            00058200
COMMENT AFTER ID CARD PRO WILL GO TO IDORACTION;                        00058300
ACTIONCARD:IF CARDNAME = "ACT" OR CARDNAME = "MOV" THEN BEGIN           00058400
ACTNO ~ ACTNO + 1 ; ACTCOUNT := ACTCOUNT + 1 ;                          00058500
IF FIRST = 1 THEN BEGIN CONV(IDNO,ALPHID);                              00058510
   SELECTARRAY[POS] := ALPHID; FIRST := 0; END;                         00058520
  IF NOT SWITC THEN BEGIN IDPOS:= POS := IDPOS + 1;                     00058600
  SWITC := TRUE END;                                                    00058700
IF CARDNAME = "ACT" THEN ACTCARD ELSE MOVECARD;     END ELSE            00058900
BEGIN  IF ACTNO = 0 THEN BEGIN                                          00059000
  CONV(IDNO,ALPHID); SELECTARRAY[POS] := ALPHID;                        00059200
IDPOS ~ POS ~ IDPOS + 1 END ;  SWI ~1 ; GO TO WHATCARD END ;            00059300
READ (CRD[NO],TYPEFT2,CARDNAME,SEQNO) [EOF] ;     SEQCHK;               00059400
GO TO ACTIONCARD ;                                                      00059500
NOCARDS:  WRITE(PTR,NUTTIN) ; GO TO EOJ ;                               00059600
EOF:                                                                    00059700
IF CARDNAME = "ID " THEN BEGIN                                          00059800
CONV(IDNO,ALPHID) ;                                                     00059900
SELECTARRAY[POS] ~ ALPHID ;                                             00060000
END ;                                                                   00060100
IF ACTCOUNT = 0 THEN IF TRACEOPT THEN BEGIN                             00060110
   ERR := 29; WRITEINFO; GO TO EOJ END;                                 00060200
PACKETS := PACKETNO;                                                    00060210
CONV (PACKETNO,ALPHPACKET);                                             00060300
SELECTARRAY[0] ~ ALPHPACKET ;                                           00060400
SORTKEYS ~ SORTNO ;                                                     00060500
CONV(SORTNO,ALPHSORT) ;   SORTARRAY[0] ~ ALPHSORT ;                     00060600
SELECTSIZE ~ IDPOS ;   INSIZE ~ OCTWDS [0];                             00060700
INBLOCK ~ OCTWDS   [1] ;                                                00060800
IF OCTWDS [2] = 0 THEN OUTSIZE ~ INSIZE ELSE OUTSIZE ~ OCTWDS[2];       00060900
IF OCTWDS[3] = 0 THEN OUTBLOCK ~ INBLOCK ELSE OUTBLOCK ~OCTWDS [3] ;    00061000
SKIPNUM ~ OCTSKIP;                                                      00061100
LIMITNUM := OCTLIMIT;                                                   00061110
ACTIONMAX ~ ACTIONMAX|7 +1;                                             00061200
                                                                        00061300
IF MON OR SYNTAX THEN                                                   00061400
BEGIN  INTEGER H; FORMAT FT1(A6),   FT2(A1,A6) ;                        00061500
FORMAT RF("RANGE IS ", I6, " TO ", I6);                                 00061600
IF RANGE THEN WRITE (PTR,RF,LOWRANGE,HIGHRANGE);                        00061700
                                                                        00061800
                                                                        00061900
WRITE (PTR,FT1,"SELECT") ;                                              00062000
WRITE(PTR,15,SELECTARRAY[*]) ;                                          00062100
                                    WRITE(PTR,FT1,"SORT  ") ;           00062200
WRITE(PTR,15,SORTARRAY[*]) ;                                            00062300
                                                                        00062400
FOR H ~ 1 STEP 1 UNTIL PACKETNO  DO WRITE(PTR,15,ACTIONARRAY[H,*]) ;    00062500
WRITE(PTR,FT1,"ZIP   ") ;     WRITE(PTR,15,ZIPARRAY[*]) ;               00062600
                                                                        00062700
                                                                        00062800
END;                                                                    00062900
IF PACKETNO ! 0 THEN SELECTS ~ TRUE ELSE PACKETS  ~ 1;                  00063000
FOR H~1 STEP 1 UNTIL PACKETS   DO MUSTACT~MUSTACT OR ACTION[H];         00063100
FOR H~0 STEP 1 UNTIL PLUGMAX DO IF PLUGBOOL[H] THEN HIGHPLUG~H;         00063200
                                                                        00063300
                                                                        00063400
                                                                        00063500
                                                                        00063600
IF ZIPNO !0 THEN ZIPS ~ TRUE ;                                          00063700
IF SORTNO !         0 THEN MUSTSORT ~ TRUE ;                            00063800
IF SYNTAX THEN GO TO EOJ;                                               00063900
RECSIZE~MAX(INSIZE,OUTSIZE);                                            00064000
CLOSE (PTR, RELEASE);                                                   00064100
SELECTSIZE~SELECTSIZE+1;                                                00064200
IF RANGE OR PLUGIN THEN SELECTS~TRUE;                                   00064300
END ;                                                                   00064400
%-----------------------------------------------------------------------00064500
%-----------------------------------------------------------------------00064600
%-----------------------------------------------------------------------00064700
BEGIN                                                                   00064800
FILE IN INMESS DISK SERIAL (2,INSIZE, INBLOCK);                         00064900
SAVE                                                                    00065000
     FILE OUT OUTMESS DISK SERIAL [20:2000](2,OUTSIZE,OUTBLOCK,SAVE 3); 00065100
FILE OUT PTR  18 (2,15);                                            %JPA00065200
ARRAY HITS [0:PACKETS+1], SAVEREC [0:INSIZE];                       %JPA00065300
INTEGER NEXTHIT;                                                    %JPA00065400
ARRAY RECORD [0: RECSIZE + 4],                                          00065500
    SORTAR [0:SORTKEYS],  SELECTAR [0: SELECTSIZE-1],                   00065600
ACTIONAR[1:PACKETS, 0:ACTIONMAX];                                       00065700
BOOLEAN ARRAY ACTIONS [1: PACKETS]          ;                           00065800
INTEGER I;                                                              00066000
LABEL EOF                 ;                                             00066100
%-----------------------------------------------------------------------00066200
STREAM PROCEDURE MOVEMANYWORDS (NOM,  SOURCE, DEST );                   00066300
VALUE NOM;                                                              00066400
BEGIN LOCAL S;                                                          00066500
SI~ LOC NOM; SI~SI + 6; DI~ LOC S; DI~ DI + 7; DS~ CHR;                 00066600
SI~ SOURCE; DI ~ DEST;                                                  00066700
S ( DS~ 32 WDS; DS~ 32 WDS);                                            00066800
DS ~ NOM WDS;                                                           00066900
END MOVEMANYWORDS;                                                      00067000
%-----------------------------------------------------------------------00067100
PROCEDURE PRINREC (LEN,RECRD,COPY);                                 %JPA00067200
VALUE LEN,COPY; INTEGER LEN,COPY; ARRAY RECRD[0];                   %JPA00067300
BEGIN                                                               %JPA00067400
   INTEGER I,L,L2    ; ARRAY B[0:15];                                   00067500
   STREAM PROCEDURE DOTEQLS (SIZE,ORIG,COPY);                           00067600
          VALUE SIZE;                                                   00067700
   BEGIN                                                                00067800
      SI~ORIG; DI~COPY;                                                 00067900
      SIZE (8( IF SC= DC THEN BEGIN DI~DI-1; DS~LIT "."; END ));        00068000
   END;                                                                 00068100
   I~0;                                                                 00068200
   IF COPY = 0 THEN  WRITE (PTR[DBL]);                                  00068300
   WHILE  LEN> 0 DO                                                     00068400
   BEGIN                                                                00068500
      IF LEN { 15 THEN L~ LEN ELSE L~ 15;                               00068600
         MOVEMANYWORDS (L,  RECRD[I],B);                            %JPA00068700
   IF ( I + L > INSIZE) THEN L2 ~ INSIZE - I ELSE L2 ~ L;               00068800
IF L2 { 15 THEN                                                         00068900
      IF COPY = 1 THEN DOTEQLS (L2,SAVEREC[I],B );                      00069000
         WRITE (PTR,L,B[*]);                                        %JPA00069100
         I~I+L;                                                     %JPA00069200
      LEN ~ LEN - L;                                                    00069300
SELECTOPT:=TRUE;                                       %JF              00069400
    END;                                                            %JPA00069500
END PRINREC;                                                        %JPA00069600
%-----------------------------------------------------------------------00069700
BOOLEAN PROCEDURE RANGECASE(RECORD);                                    00069800
ARRAY RECORD LB 0 RB ;                                                  00069900
BEGIN                                                                   00070000
INTEGER STARTS, ENDS, ALT, SHOWINGS;                                    00070100
INTEGER PROCEDURE YRDAYTOJUL(I);                                        00070200
VALUE I; INTEGER I;                                                     00070300
YRDAYTOJUL~ 365|(I DIV 1000 - 8) +    (I DIV 1000 - 9) DIV 4            00070400
+ I MOD 1000 + 3;                                                       00070500
STREAM PROCEDURE WRITESHOWINGS( RECORD, I);                             00070600
VALUE I;                                                                00070700
BEGIN SI~LOC I;DI~RECORD;DI~DI+60;DI~DI+54; DS~2 DEC;END;               00070800
STREAM PROCEDURE STARTSENDSALT (RECORD,STARTS,ENDS, ALT);               00070900
BEGIN SI~RECORD; SI~SI+27; DI~STARTS; DS~5 OCT;                         00071000
    DI~ENDS; DS~ 5 OCT; SI~ SI+10; DI~ALT; DS~OCT;                      00071100
END;                                                                    00071200
STARTSENDSALT (RECORD,STARTS,ENDS,ALT);                                 00071300
IF ALT {0 THEN ELSE                                                     00071400
BEGIN                                                                   00071500
STARTS~YRDAYTOJUL(STARTS);                                              00071600
    WHILE STARTS < LOWRANGE DO STARTS~STARTS+ALT|7;                     00071700
ENDS~YRDAYTOJUL(MIN(HIGHRANGE,ENDS));                                   00071800
IF STARTS>ENDS THEN ELSE                                                00071900
SHOWINGS~(ENDS-STARTS) DIV (7|ALT) + 1;                                 00072000
    IF SHOWINGS ! 0 THEN                                                00072100
         BEGIN RANGECASE~TRUE;WRITESHOWINGS(RECORD,SHOWINGS);END;       00072200
    END;                                                                00072300
END;                                                                    00072400
%-----------------------------------------------------------------------00072500
INTEGER STREAM PROCEDURE SELECTOR (ARR1,RECORD,HITS);               %JPA00072600
COMMENT SELECTOR TESTS THE ARRAY "RECORD" TO SEE IF IT QUALIFIES        00072700
UNDER THE CRITERIA DEFINED BY ARRAY "ARR1". "ARR1" IS DESCRIBED         00072800
BELOW USING  A MODIFIED COBOL NOTATION.                                 00072900
IF RECORD QUALIFIES, SELECTOR IS SET TO THE NUMBER OF THE               00073000
LAST PACKET UNDERWHICH IT QUALIFIED. IF QUALIFIED ON MORE THAN      %JPA00073100
ONE PACKET THEN THE NUMBER OF EACH PACKET QUALIFIED ON IS IN        %JPA00073200
HITS[1] THRU HITS[N] WHERE N IS THE NUMBER OF QUALIFICATIONS        %JPA00073300
HITS[N&1] WILL BE ZERO. IF HITS [0] NEGATIVE THEN FIRST             %JPA00073400
QUALIFICATION WILL CAUSE EXIT FROM ROUTINE.                         %JPA00073500
01 ARR1.                                                                00073600
  02 FILLER SZ 6.                                                       00073700
  02 PACKETS PC 99.                                                     00073800
  02 PACKET OC PACKETS TIMES.                                           00073900
   02.5   FILLER SZ 6.                                                  00074000
   02.5 IDS PC 99.                                                      00074100
   02.5 ID SZ 24 OC IDS TIMES.                                          00074200
    03 FILLER PC 9.                                                     00074300
    03 NUM PC 99. (NUM IS THE NUMBER OF CHARACTERS INVOLVED IN THE TEST)00074400
    03 WHERE PC 999.  (THE CHARACTER POSITION FROM WHICH TO BEGIN  COMP)00074500
    03 HOW PC 9. (DESCRIBES THE RELATIONAL OPERATOR FOR THE COMPARE)    00074600
         88  EQ  (=)  VA 0.                                             00074700
         88  NEQ (!) VA 1.                                              00074800
         88 GR  (>) VA 2.                                               00074900
         88 GREQ (}) VA 3.                                              00075000
         88  LS (<) VA 4.                                               00075100
         88 LSEQ ({) VA 5.                                              00075200
         88 AN (ALPHANUMERIC) VA 6.                                     00075300
    03 STRIP PC 9. (SHOULD FIELD IN RECORD BE TREATED AS NUMERIC)       00075400
         88 YES VA 1.                                                   00075500
    03 DATA-TO-COMPARE-ON SZ NUM.                                       00075600
    03 FILLER SZ 16 - NUM.                                              00075700
END COMMENT;                                                            00075800
BEGIN                                                                   00075900
LOCAL NOM, WHERE1, WHERE2, WHERE3, HOW, A, B, X, C, Y, UPTO,            00076000
    HITINDR,                                                        %JPA00076100
ORUPTO, IDS, PACKETS;                                                   00076200
LABEL EQ, NEQ, GR, GREQ, LS, LSEQ, AN, PASSED, EX;                      00076300
    DI~HITS; DI~DI+8; HITINDR~DI; DS~8 LIT "0";                     %JPA00076400
SI ~ ARR1; SI~ SI + 6; DI~LOC PACKETS; DS~ 2 OCT;                       00076500
PACKETS ( SI ~ SI + 6; DI ~ LOC IDS; DS ~ 2 OCT;                        00076600
TALLY ~ TALLY +1;                                                       00076700
UPTO ~SI;      ORUPTO~SI;                                               00076800
IDS( SI~UPTO;  SI~SI+1;                                                 00076900
DI~LOC NOM;                  DS~ 2 OCT;                                 00077000
DI ~  LOC WHERE1 ;  DS ~  OCT;                                          00077100
DI ~  LOC WHERE2 ;  DS ~  OCT;                                          00077200
DI ~  LOC WHERE3 ;  DS ~  OCT;                                          00077300
DI~LOC   HOW;      DS~   OCT;                                           00077400
A~SI;                                                                   00077500
SI~RECORD;                                                              00077600
WHERE1 (SI~SI+50; SI~SI+50);                                            00077700
WHERE2 (SI~ SI+10);                                                     00077800
SI~ SI+ WHERE3;                                                         00077900
B~SI; DI~LOC C; SI~A;                                                   00078000
IF SC = "1" THEN BEGIN SI~B; DS~NOM NUM END                             00078100
ELSE BEGIN SI~B; DS ~ NOM CHR END;                                      00078200
SI~LOC C; DI~UPTO; DI~ DI+8;                                            00078300
CI~ CI+HOW;                                                             00078400
GO TO EQ;GO TO NEQ;GO TO GR; GO TO GREQ; GO TO LS;GO TO LSEQ;GO TO AN;  00078500
 EQ:IF NOM SC = DC THEN GO TO PASSED ELSE  JUMP OUT TO EX;              00078600
NEQ:IF NOM SC ! DC THEN GO TO PASSED ELSE JUMP OUT TO EX;               00078700
 GR:IF NOM SC > DC THEN GO TO PASSED ELSE JUMP OUT TO EX;               00078800
GREQ:IF NOM SC} DC THEN GO TO PASSED ELSE JUMP OUT TO EX;               00078900
 LS: IF NOM SC< DC THEN GO TO PASSED ELSE JUMP OUT TO EX;               00079000
LSEQ:IF NOM SC{ DC THEN GO TO PASSED ELSE JUMP OUT TO EX;               00079100
 AN: NOM (IF SC = ALPHA THEN SI~SI+1 ELSE JUMP OUT 2 TO EX);            00079200
PASSED: SI~UPTO; SI~SI+24; UPTO~SI);                                    00079300
    SELECTOR~ TALLY; SI~HITS   ; IF   SC!"0" THEN JUMP OUT;         %JPA00079400
    SI~LOC SELECTOR; DI~HITINDR; DS~WDS; HITINDR~DI; DS~8 LIT "0";  %JPA00079500
EX: SI~ORUPTO; IDS(SI~SI+24); ORUPTO~SI);                               00079600
END SELECTOR;                                                           00079700
%-----------------------------------------------------------------------00079800
BOOLEAN STREAM PROCEDURE COMPARE (SORTARRAY, A, B);                     00079900
COMMENT COMPARE COMPARES ARRAYS A AND B  ACCORDING TO CRITERIA          00080000
SPECIFIED IN SORTARRAY.   IF COMPARE IS TRUE, ARRAY A COMES             00080100
"FIRST" OTHERWISE, B COMES FIRST.                                       00080200
THE FOLLOWING IS A COBOL TYPE DESCRIPTION OF SORTARRAY:                 00080300
01 SORTARRAY.                                                           00080400
    03 FILLER PC 999999.                                                00080500
    03 NUMKEYS PC 99. (NUMBER OF SORT KEYS TO FOLLOW)                   00080600
    03 KEY SZ 8 OCCURS NUMKEY TIMES.                                    00080700
         05 FILLER PC 9.                                                00080800
         05 NOM PC 99. (NUMBER OF CHARACTERS TO COMPARE ON)             00080900
         05 WHERE PC 999. (POSITION OF START OF COMPARE)                00081000
         05 SEQ PC 9. ( ASCENDING OR DESCENDING SEQUENCE)               00081100
              88 DESCENDING VA 0.                                       00081200
              88 ASCENDING VA 1.                                        00081300
         05 FILLER PC 9.                                                00081400
END COMMENT;                                                            00081500
BEGIN                                                                   00081600
LOCAL NUMKEYS, UPTO, KEY, NOM, WHERE1, WHERE2, WHERE3, SEQ    ;         00081700
LABEL EX      ;                                                         00081800
SI~SORTARRAY; SI~SI+6;DI~LOC NUMKEYS;DS~2 OCT; UPTO~SI;                 00081900
NUMKEYS ( SI~ UPTO;                                                     00082000
SI ~ SI + 1;                                                            00082100
DI~ LOC NOM; DS ~ 2 OCT;                                                00082200
DI ~ LOC WHERE1;DS ~ OCT;                                               00082300
DI ~ LOC WHERE2;DS ~ OCT;                                               00082400
DI ~ LOC WHERE3;DS ~ OCT;                                               00082500
DI ~ LOC SEQ; DS ~ OCT;                                                 00082600
SI~ A;          DI~B;                                                   00082700
WHERE1 (2( SI~SI+50;  DI~ DI+50));                                      00082800
WHERE2 (SI~SI+10; DI~DI+10);                                            00082900
SI~SI + WHERE3; DI ~ DI + WHERE3;                                       00083000
IF NOM SC = DC THEN GO TO EX;                                           00083100
SI ~ SI - NOM; DI ~ DI - NOM;                                           00083200
TALLY ~ SEQ;                                                            00083300
IF NOM SC > DC THEN TALLY ~ TALLY + 1;                                  00083400
COMPARE ~ TALLY; JUMP OUT;                                              00083500
EX: SI~UPTO; SI~SI+8; UPTO~SI);                                         00083600
END COMPARE;                                                            00083700
%-----------------------------------------------------------------------00083800
STREAM PROCEDURE HIVALUE (SORTARRAY, RECORD);                           00083900
COMMENT HIVALUE CREATES A RECORD WHICH WILL SORT LAST                   00084000
ACCORDING TO THE KEYS DEFINED BY SORTARRAY;                             00084100
BEGIN                                                                   00084200
LOCAL NUMKEYS, UPTO, KEY, NOM, WHERE1, WHERE2, WHERE3, SEQ    ;         00084300
SI~SORTARRAY; SI~SI+6;DI~LOC NUMKEYS;DS~2 OCT; UPTO~SI;                 00084400
NUMKEYS ( SI~ UPTO;                                                     00084500
SI ~ SI + 1;                                                            00084600
DI~ LOC NOM; DS ~ 2 OCT;                                                00084700
DI ~ LOC WHERE1;DS ~ OCT;                                               00084800
DI ~ LOC WHERE2;DS ~ OCT;                                               00084900
DI ~ LOC WHERE3;DS ~ OCT;                                               00085000
DI ~ RECORD;                                                            00085100
WHERE1 (DI~DI+50; DI~ DI + 50);                                         00085200
WHERE2 ( DI~ DI + 10);                                                  00085300
DI ~DI + WHERE3;                                                        00085400
IF SC = "1" THEN  NOM(DS~ LIT "9" ) ELSE NOM (DS~ LIT " ") ;            00085500
SI~UPTO; SI~ SI+ 8; UPTO ~SI)                                           00085600
END HIVALUE;                                                            00085700
%-----------------------------------------------------------------------00085800
STREAM                                                                  00085900
PROCEDURE ACT (ACTIONARRAY, RECORD);                                    00086000
COMMENT ACTION AFFECTS RECORD ACCORDING TO INSTRUCTIONS                 00086100
GIVEN IN ACTIONARRAY.                                                   00086200
01 ACTIONARRAY.                                                         00086300
    03 FILLER SZ 6.                                                     00086400
    03 ACTIONS PC 99.                                                   00086500
          03 ACTION SZ 56 OC ACTIONS TIMES.                             00086600
            05 FILLER SZ 1 VA 0.                                        00086700
         05 NUM PC 99.                                                  00086800
         05 WHERE PC 999.                                               00086900
         05 STRIP PC 9. (SHOULD ACTION BE STRIPPING OF ZONE BITS)       00087000
            88 YES VA 1.                                                00087100
         05 FILLER PC 9.                                                00087200
         05 DATA SZ 48.                                                 00087300
          03 MOVE REDEFINES ACTION.                                     00087400
         05 FILLER SZ 1.                                                00087500
           88 REGULAR VA 1.                                             00087600
           88 OCT-TO-DEC-CONVERT VA 2.                                  00087700
            05 NUM PC 99.                                               00087800
            WHERE-TO PC 999.                                            00087900
            05 WHERE-FROM PC 999.                                       00088000
            05 FILLER SZ 47.                                            00088100
END COMMENT;                                                            00088200
BEGIN                                                                   00088300
LOCAL ACTIONS, UPTO, NOM, WHERE1,WHERE2,WHERE3,STRIP,V,W,X,Y,Z,U;       00088400
SI~ACTIONARRAY; SI~SI+6; DI~LOC ACTIONS; DS~2 OCT; UPTO~SI;             00088500
ACTIONS (SI~UPTO;                                                       00088600
IF SC} "1" THEN W~SI;                                                   00088700
SI ~ SI + 1;                                                            00088800
DI~ LOC NOM; DS ~ 2 OCT;                                                00088900
DI ~ LOC WHERE1;DS ~ OCT;                                               00089000
DI ~ LOC WHERE2;DS ~ OCT;                                               00089100
DI ~ LOC WHERE3;DS ~ OCT;                                               00089200
DI ~ RECORD;                                                            00089300
WHERE1 (DI~DI+50; DI~ DI + 50);                                         00089400
WHERE2 ( DI~ DI + 10);                                                  00089500
DI ~DI + WHERE3;                                                        00089600
IF TOGGLE THEN BEGIN                                                    00089700
V~DI;                                                                   00089800
DI~LOC WHERE1; DS ~ OCT;                                                00089900
DI~LOC WHERE2; DS ~ OCT;                                                00090000
DI~LOC WHERE3; DS ~ OCT;                                                00090100
SI~RECORD;                                                              00090200
WHERE1 (SI~SI+50; SI~SI+50);                                            00090300
WHERE2(SI~SI+10);                                                       00090400
SI ~ SI  + WHERE3;                                                      00090500
DI~V;                                                                   00090600
X~SI; SI~W; IF SC= "1" THEN; SI~X; IF TOGGLE THEN DS~NOM CHR            00090700
ELSE DS~ NOM DEC;                                                       00090800
END ELSE                                                                00090900
IF SC = "1" THEN BEGIN SI~ LOC U; DS~ NOM ZON END ELSE                  00091000
         BEGIN SI~SI+2; DS~ NOM CHR END;                                00091100
SI~UPTO; SI~SI+56; UPTO~SI)                                             00091200
END ACTION;                                                             00091300
%-----------------------------------------------------------------------00091400
PROCEDURE DOPLUG;                                                       00091500
FOR I~ 0 STEP 1 UNTIL HIGHPLUG DO                                       00091600
IF PLUGBOOL[I] THEN CASE(I) OF                                          00091700
BEGIN                                                                   00091800
END;                                                                    00091900
BOOLEAN PROCEDURE INPROC (RECORD);                                      00092000
ARRAY RECORD [0];                                                       00092100
BEGIN                                                                   00092200
LABEL STARTS, EOFS;                                                     00092300
LABEL  DOACTS;                                                      %JPA00092400
INTEGER I;                                                              00092500
STARTS:                                                                 00092600
IF HITS[NEXTHIT ] = 0 THEN                                          %JPA00092800
BEGIN                                                               %JPA00092900
READ (INMESS, INSIZE, RECORD[*])[EOFS];                                 00093000
INCOUNT~INCOUNT+1;                                                      00093100
IF RANGE THEN IF NOT RANGECASE (RECORD)THEN GO TO STARTS;               00093200
                                                                        00093300
IF PLUGIN THEN DOPLUG;                                                  00093400
  I := SELECTOR(SELECTAR,RECORD,HITS[0]);                               00093500
  IF I = 0 THEN GO TO STARTS;                                           00093510
NEXTHIT~1;                                                          %JPA00093600
IF TRACEOPT THEN  PRINREC(INSIZE,RECORD,0);                         %JPA00093700
IF EXPLODOPT OR TRACEOPT THEN MOVEMANYWORDS(INSIZE,RECORD,SAVEREC); %JPA00093800
END                                                                 %JPA00093900
ELSE MOVEMANYWORDS(INSIZE,SAVEREC,RECORD);                          %JPA00094000
DOACTS: IF NOT SELECTOPT THEN I~ HITS[NEXTHIT];                     %JPA00094100
IF MUSTACT THEN                                                         00094200
IF ACTIONS[I] THEN ACT (ACTIONAR[I,*], RECORD);                         00094300
    NEXTHIT~NEXTHIT+1;                                              %JPA00094400
IF ((NOT EXPLODOPT) AND (HITS[NEXTHIT]!0)) THEN GO TO DOACTS;       %JPA00094500
    OUTCOUNT~OUTCOUNT+1;                                            %JPA00094600
IF LIMITNUM NEQ 0 THEN IF OUTCOUNT GTR LIMITNUM THEN                    00094610
GO TO EOFS;                                                             00094620
IF TRACEOPT THEN PRINREC(OUTSIZE,RECORD,1);                         %JPA00094700
IF FALSE THEN EOFS: INPROC ~ TRUE;                                      00094800
END INPROC;                                                             00094900
%-----------------------------------------------------------------------00095000
PROCEDURE OUTPROC (B, RECORD);                                          00095100
VALUE B; BOOLEAN B; ARRAY RECORD[0];                                    00095200
IF NOT B THEN WRITE(OUTMESS, OUTSIZE, RECORD[*]);                       00095300
%-----------------------------------------------------------------------00095400
BOOLEAN PROCEDURE COMP (A,B);                                           00095500
ARRAY A, B[0];                                                          00095600
BEGIN SORTCOUNT~SORTCOUNT+1;                                            00095700
COMP ~COMPARE (SORTAR,A,B);                                             00095800
END;                                                                    00095900
PROCEDURE HVL(A);                                                       00096000
ARRAY A[0];                                                             00096100
HIVALUE (SORTAR,A);                                                     00096200
%-----------------------------------------------------------------------00096300
%-----------------------------------------------------------------------00096500
READ (ACTION [*],PACKETS,ACTIONS [*]);                              %JPA00096600
MOVEMANYWORDS (SORTKEYS+1, SORTARRAY, SORTAR);                          00096700
MOVEMANYWORDS (SELECTSIZE, SELECTARRAY, SELECTAR);                      00096800
FOR I~1 STEP 1 UNTIL PACKETS DO  MOVEMANYWORDS(ACTIONMAX,               00096900
ACTIONARRAY[I, *], ACTIONAR[I, *]);                                     00097000
IF SELECTOPT THEN HITS[0]~-1;                                       %JPA00097100
NEXTHIT~1;                                                          %JPA00097200
IF SKIPNUM ! 0 THEN SPACE (INMESS,SKIPNUM) [EOF];                       00097300
                                                                        00097400
IF NOT MUSTSORT  AND NOT SELECTS THEN WHILE TRUE DO BEGIN               00097500
READ (INMESS, INSIZE, RECORD[*])[EOF ];                                 00097600
INCOUNT~OUTCOUNT~OUTCOUNT+1;                                            00097700
IF LIMITNUM NEQ 0 THEN IF OUTCOUNT GTR LIMITNUM THEN                    00097800
 BEGIN OUTCOUNT := OUTCOUNT - 1; GO TO EOF;  END;                       00097810
                                                                        00097900
WRITE (OUTMESS,OUTSIZE, RECORD[*]);                                     00098000
END ELSE                                                                00098100
IF MUSTSORT AND NOT SELECTS THEN                                        00098200
BEGIN DEFINE A=A#;                                                      00098300
SORT (OUTMESS,INMESS,3,HVL,COMP,RECSIZE,CORE)                           00098400
END                                                                     00098500
ELSE                                                                    00098600
IF MUSTSORT AND SELECTS THEN                                            00098700
BEGIN DEFINE A=A#;                                                      00098800
SORT (OUTPROC,INPROC,3,HVL,COMP,RECSIZE,CORE)                           00098900
END                                                                     00099000
ELSE                                                                    00099100
BEGIN LABEL STARTS;                                                     00099200
WHILE TRUE DO BEGIN                                                     00099300
IF INPROC(RECORD) THEN GO TO EOF;                                   %JPA00099400
%                                                                   %JPA00099500
%                                                                   %JPA00099600
%                                                                   %JPA00099700
%                                                                   %JPA00099800
%                                                                   %JPA00099900
%                                                                   %JPA00100000
%                                                                   %JPA00100100
WRITE (OUTMESS,OUTSIZE, RECORD[*]);                                     00100300
%                                                                   %JPA00100400
END;                                                                    00100500
END;                                                                    00100600
EOF:                                                                    00100700
IF ZIPS THEN ZIP WITH ZIPARRAY [*];                                     00100800
IF MUSTCOUNT THEN BEGIN                                                 00100900
FILE OUT PTR 18 (2,15);                                                 00101000
FORMAT FCOUNT ("RECORDS IN=", I6, ". OUT=", I6, ". RECORDS SORTED=",I6);00101100
FORMAT FCOUNT2(" INPUT RECORDS SKIPPED = ",I6,                          00101110
         "    LIMIT OF RECORDS OUT = ",I6);                             00101120
FORMAT FCOUNT3 ("INPUT RECORDS SKIPPED = ",I6,                          00101130
          "    NO LIMIT ON RECORDS OUT IS SET.");                       00101140
WRITE (PTR, FCOUNT, INCOUNT, OUTCOUNT, SORTCOUNT);                      00101200
 IF SKIPNUM NEQ 0 THEN IF LIMITNUM NEQ 0 THEN                           00101210
 WRITE (PTR, FCOUNT2,SKIPNUM,LIMITNUM);                                 00101220
  IF SKIPNUM NEQ 0 THEN IF LIMITNUM = 0 THEN                            00101230
  WRITE (PTR,FCOUNT3,SKIPNUM);                                          00101240
 IF SKIPNUM = 0 THEN IF LIMITNUM NEQ 0 THEN                             00101250
  WRITE (PTR,FCOUNT2,SKIPNUM,LIMITNUM);                                 00101260
END;                                                                    00101300
END;                                                                    00101400
                                                                        00101500
EOJ:                                                                    00101600
END.                                                                    00101700
END;END.        LAST CARD ON 0CRDING TAPE                               00101800
