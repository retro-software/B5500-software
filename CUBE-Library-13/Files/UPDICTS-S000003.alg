"UPDICT/ASUPER"                                                         00000100
BEGIN      FILE OUT PTR 4(1,15) ;                                       00000200
COMMENT   CUBE LIBRARY VERSION 11/20/68                                 00000201
          CUBE LIBRARY NUMBER S000003                                   00000202
          SOURCE FILE NAME UPDICTS/S000003                              00000203
          PROGRAM NAME UPDICT/SUPER;                                    00000204
FILE IN CRD(1,10) ;                                                     00000300
INTEGER  WDSPERREC,I;                                                   00000400
BOOLEAN  NEW ;                                                          00000500
ALPHA  FILENAME ;                                                       00000600
ARRAY A[0:10] ;                                                         00000700
LABEL  NOFC,ELFIN,NONEWDATA;                                            00000800
FORMAT NOTLISTONLYFT(/"BELOW FOLLOWS A LISTING OF THE DATA IN THE FILE" 00000900
/) ,                                                                    00001000
NONEWDATAFT(/"DATA NOT PRESENT FOR THE FILE TO BE CREATED") ,           00001100
LISTONLYFT(/"NO ADDITIONS OR REPLACEMENTS TO THE FILE WERE REQUESTED"/),00001200
NOFCFT(/"NO CARD GIVING THE FILE DESIRED IS PRESENT" ) ;                00001300
%***********************************************************************00001400
STREAM PROCEDURE SETARROW (A) ;                                         00001500
BEGIN  DI~ A; 2(DI~DI+40) ; DS ~ LIT"~"; END SETARROW;                  00001600
%***********************************************************************00001700
INTEGER STREAM PROCEDURE GETFILENAME(A,FILENAME,WDSPERREC) ;            00001800
BEGIN  LABEL  EOGETFILENAME,EXTRA,BLANKS ;                              00001900
     LOCAL  COUNT,ALPHWPR ;                                             00002000
SI~A;  IF SC = " " THEN BEGIN TALLY ~ 1; GO TO EOGETFILENAME  END ;     00002100
DI ~ FILENAME ; DS~ 8 LIT"0       " ; DI ~ DI-7;                        00002200
7(IF SC = ALPHA THEN DS ~ CHR ELSE JUMP OUT) ;                          00002300
IF SC = ALPHA THEN ELSE IF SC = " " THEN ELSE                           00002400
BEGIN TALLY ~ 2 ; GO TO EOGETFILENAME  END ;                            00002500
EXTRA: IF SC ! " "THEN BEGIN IF SC = "~" THEN                           00002600
BEGIN TALLY ~ 3 ; GO TO EOGETFILENAME; END ELSE SI~SI+1;  GO TO EXTRA   00002700
END ;                                                                   00002800
BLANKS: IF SC = " "THEN BEGIN SI~SI+1; GO TO BLANKS  END;               00002900
DI ~ LOC ALPHWPR ;                                                      00003000
8(IF SC } "0"      THEN BEGIN TALLY ~ TALLY+1; DS ~CHR;END ELSE         00003100
JUMP OUT) ;            COUNT ~ TALLY; DI~WDSPERREC; SI~LOC ALPHWPR ;    00003200
DS ~ COUNT OCT ;      TALLY ~ 0 ;                                       00003300
EOGETFILENAME:  GETFILENAME ~ TALLY ;                                   00003400
END GETFILENAME ;                                                       00003500
%***********************************************************************00003600
PROCEDURE ERR(ERRORNUMBER) ;  VALUE ERRORNUMBER ; INTEGER ERRORNUMBER;  00003700
BEGIN  SWITCH FORMAT ERRS~ ("X") ,                                      00003800
("FIRST CARD MUST HAVE FILE NAME BEGINNING IN COLUMN 1"),% 1            00003900
("ONLY LETTERS OR NUMBERS MAY APPEAR IN FILE NAME")  , % 2              00004000
("FILE NAME IS TOO LONG") , %3                                          00004100
                                                                      ("00004200
 WORDS PER RECORD MAY NOT BE NEGATIVE") , % 4                           00004300
("WORDS PER RECORD MAY NOT BE GREATER THAN 1023") , % 5                 00004400
( "FIELD NAME MUST BEGIN IN COLUMN 1") , % 6                            00004500
("ONLY DASHES,LETTERS OR NUMBERS PERMITTED IN FIELD NAME") , % 7        00004600
("NOT ENOUGH INFO ON CARD") , % 8                                       00004700
("FIRST CHARACTER OF DATA MUST BE 0,1 OR 2") , % 9                      00004800
("ONLY NUMERICS ARE PERMITTED IN THE DATA ASSOCIATED WITH A FIELD NAME")00004900
, % 10                                                                  00005000
("THE SEVENTH CHARACTER OF THE DATA MUST BE 0 OR 1 OR 2 3"),%11         00005100
( "THE VALUES OF WHERE AND NUMBER OF CHARACTERS IN THE FIELD ARE TOO"/  00005200
" LARGE CONSIDERING WORDS PER RECORD "/                                 00005300
"EVEN THOUGH AN ERROR THIS WILL BE ACTED ON AND WILL ADD TO OR "/       00005400
"REPLACE INFO IN THE FILE") ;                       % 12                00005500
FORMAT  BLANKFT(2X60) ;                                                 00005600
WRITE(PTR,BLANKFT) ;                                                    00005700
WRITE(PTR,ERRS[ERRORNUMBER]) ;                                          00005800
IF ERRORNUMBER < 6 THEN BEGIN WRITE(PTR,10,A[*]) ;                      00005900
WRITE(PTR,BLANKFT) ;                     GO TO ELFIN ; END ELSE         00006000
      WRITE(PTR,10,A[*]) ;             WRITE(PTR,BLANKFT) ;             00006100
END ERRPRO;                                                             00006200
%***********************************************************************00006300
SETARROW(A) ;                                                           00006400
READ (CRD,10,A[*]) [NOFC] ;                                             00006500
IF I~ GETFILENAME (A,FILENAME,WDSPERREC) ! 0 THEN ERR(I) ;              00006600
IF WDSPERREC < 0 THEN ERR(4) ;                                          00006700
IF WDSPERREC > 1023 THEN ERR(5) ;                                       00006800
IF WDSPERREC ! 0 THEN BEGIN     COMMENT  NEWFILE BLOCK ;                00006900
SAVE FILE OUT DICTION DISK SERIAL [20:100] (2,3,90,SAVE 999) ;          00007000
FILL DICTION WITH FILENAME,* ;                                          00007100
WRITE (DICTION,*,FILENAME,WDSPERREC) ;                                  00007200
NEW ~ TRUE ;                                                            00007300
READ (CRD[NO]) [NONEWDATA] ;                                            00007400
END  NEWFILE BLOCK ;                                                    00007500
BEGIN                                                                   00007600
FILE DICTION DISK SERIAL (2,3,90) ;                                     00007700
INTEGER  WHERE,NUMCHAR ;                                                00007800
ALPHA  DATA ;                                                           00007900
ALPHA ARRAY FIELDNAME [0:2] ,DICTIONFILE[0:1] ;                         00008000
FORMAT  NEWFT(/"BELOW FOLLOWS A LISTING OF DATA PUT INTO THE NEW FILE "/00008100
"AND ERRORS IF ANY WHICH WERE NOT PUT INTO THE NEW FILE"/) ,            00008200
CHANGEFT(/                                                              00008300
         "BELOW FOLLOWS A LISTING OF ADDITIONS OR REPLACEMENTS "/       00008400
"TO THE FILE AND ERRORS IF ANY WHICH WERE NOT ACTED UPON"/) ,           00008500
FILEINFO("DATA IS FOR THE ",A1,A6,"/ MASTER FILE, A FILE  "/            00008600
"OF ",I4," WORDS PER RECORD") ,                                         00008700
ADDFT(X26,"ADDITION") ,                                                 00008800
REPLACEFT(X26,"REPLACEMENT") ;                                          00008900
LABEL  EOF,LISTONLY,ENDING,NOTLISTONLY,READCARD,EOD,DICTREAD ;          00009000
%***********************************************************************00009100
INTEGER STREAM PROCEDURE GETDATA (A,FIELDNAME,DATA) ;                   00009200
BEGIN    LABEL  EOGETDATA,EXTRA,BLANKS ;                                00009300
SI~A; IF SC = " " THEN BEGIN TALLY ~ 6; GO TO EOGETDATA  END ;          00009400
DI~FIELDNAME ;     DS~16 LIT " ";  DI~DI-16;                            00009500
16(IF SC = ALPHA THEN DS~CHR ELSE IF SC = "-" THEN DS~CHR ELSE          00009600
IF SC = " " THEN JUMP OUT ELSE BEGIN TALLY~7;                           00009700
JUMP OUT TO EOGETDATA END) ;                                            00009800
EXTRA: IF SC ! " "THEN BEGIN  IF SC ="~" THEN                           00009900
BEGIN TALLY~8 ; GO TO EOGETDATA ; END ELSE SI~SI+1 ;  GO TO EXTRA END ; 00010000
BLANKS: IF SC = " " THEN BEGIN SI ~ SI+1; GO TO BLANKS END;             00010100
DI~ DATA ;                                                              00010200
IF SC } "0" THEN IF SC < "3" THEN DS ~CHR ELSE BEGIN TALLY~9; GO TO     00010300
EOGETDATA; END ELSE BEGIN TALLY~9; GO TO EOGETDATA END ;                00010400
5(IF SC } "0" THEN DS ~CHR ELSE BEGIN TALLY~ 10;                        00010500
JUMP OUT TO EOGETDATA END) ;                                            00010600
IF SC = "0" THEN DS~CHR ELSE IF SC = "1" THEN DS~CHR ELSE               00010700
IF SC = "2" THEN DS:= CHR ELSE IF SC = "3" THEN DS := CHR ELSE BEGIN    00010710
TALLY ~11; GO TO EOGETDATA END ;        DS~ LIT " " ;                   00010800
EOGETDATA: GETDATA ~TALLY ;                                             00010900
END GETDATA ;                                                           00011000
%***********************************************************************00011100
STREAM PROCEDURE WHEREVALUE (DATA,WHERE,NUMCHAR) ;                      00011200
BEGIN   SI~DATA ; SI~SI+1;  DI~NUMCHAR ; DS~2 OCT; DI~WHERE; DS~3 OCT ; 00011300
END WHEREVALUE ;                                                        00011400
%***********************************************************************00011500
BOOLEAN STREAM PROCEDURE REPLACE(DICTIONFILE,FIELDNAME) ;               00011600
BEGIN     SI~DICTIONFILE ;  DI~FIELDNAME ;                              00011700
IF 16 SC = DC THEN TALLY ~ 1 ;   REPLACE ~ TALLY ;                      00011800
END REPLACE ;                                                           00011900
%***********************************************************************00012000
FILL DICTION WITH FILENAME,* ;                                          00012100
READ(DICTION,*,FILENAME,WDSPERREC) ;                                    00012200
WRITE(PTR,FILEINFO,FILENAME.[6:6],FILENAME,WDSPERREC) ;                 00012300
READ (CRD[NO])  [LISTONLY] ;                                            00012400
BEGIN    FILE DICTION DISK SERIAL (1,1) ;                               00012500
COMMENT  0000000/DICTION IS A 1 WD FILE. IF A 1 IS IN IT IT MEANS A     00012600
NON-LIST EXECUTION TOOK PLACE ;                                         00012700
WRITE(DICTION,*,1) ;                                                    00012800
END DUMP BLOCK ;                                                        00012900
IF NEW THEN WRITE(PTR,NEWFT) ELSE WRITE(PTR,CHANGEFT) ;                 00013000
READCARD:                                                               00013100
READ(CRD,10,A[*]) [EOF] ;                                               00013200
IF I ~ GETDATA(A,FIELDNAME,DATA) ! 0 THEN BEGIN ERR(I); GO TO READCARD  00013300
END ;                                                                   00013400
                  WHEREVALUE(DATA,WHERE,NUMCHAR) ;                      00013500
IF WHERE + NUMCHAR > WDSPERREC | 8 THEN ERR(12) ;                       00013600
                                                                        00013700
FIELDNAME[2] ~ DATA ;                                                   00013800
IF NEW THEN BEGIN                        WRITE(DICTION,3,FIELDNAME[*]) ;00013900
WRITE(         PTR,3,FIELDNAME[*]) ; END IFNEW      ELSE                00014000
BEGIN                                                                   00014100
DICTREAD: READ(DICTION,2,DICTIONFILE[*]) [EOD] ;                        00014200
IF REPLACE(DICTIONFILE,FIELDNAME)  THEN      BEGIN                      00014300
READ REVERSE(DICTION) ;  WRITE(DICTION,3,FIELDNAME[*]) ;                00014400
WRITE(PTR[NO],3,FIELDNAME[*]) ;  WRITE(PTR,REPLACEFT) ;                 00014500
GO TO READCARD ; END IF REPLACE ELSE GO TO DICTREAD    ;                00014600
EOD:  WRITE(DICTION,3,FIELDNAME[*] ) ;                                  00014700
WRITE(PTR[NO],3,FIELDNAME[*]) ;     WRITE(PTR,ADDFT) ;                  00014800
REWIND(DICTION) ;  GO TO READCARD ;                                     00014900
END IF NOT NEW ;                                                        00015000
GO TO READCARD ;                                                        00015100
EOF:                                                                    00015200
GO TO NOTLISTONLY ;                                                     00015300
LISTONLY:                                                               00015400
WRITE(PTR,LISTONLYFT) ;                                                 00015500
NOTLISTONLY:        IF NOT NEW THEN BEGIN  WRITE(PTR,NOTLISTONLYFT) ;   00015600
REWIND(DICTION) ;        SPACE(DICTION,1) ;                             00015700
WHILE TRUE DO BEGIN     READ(DICTION,3,A[*]) [ENDING] ;                 00015800
WRITE(PTR,3,A[*]) ;                                                     00015900
END WHILE TRUE ;                                                        00016000
ENDING:                                                                 00016100
END IF NOT NEW ;                                                        00016200
END LARGE INNER BLOCK ;                                                 00016300
GO TO ELFIN ;                                                           00016400
NOFC:                                                                   00016500
WRITE(PTR,NOFCFT) ;   GO TO ELFIN ;                                     00016600
NONEWDATA: WRITE(PTR,NONEWDATAFT) ;                                     00016700
ELFIN:                                                                  00016800
END.                                                                    00016900
END;END.        LAST CARD ON 0CRDING TAPE                               00017000
