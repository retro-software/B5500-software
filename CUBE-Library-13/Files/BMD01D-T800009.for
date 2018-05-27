CID                     BMD01D                                                  
CID   0901HS 15 150    $BMD01D SIMPLE DATA DESCRIPTION                  D0100000
CIBJOB         ALTIO                                                    D0100010
CIBFTC D01D    LIST,M94,XR7                                             D0100020
C        SIMPLE DATA DESCRIPTION                      JUNE  6, 1966     D0100030
C               HEALTH SCIENCES COMPUTING FACILITY                      D0100040
C                      UCLA MEDICAL SCHOOL                              D0100050
C        THIS IS A SIFTED VERSION OF BMD01D ORIGINALLY WRITTEN IN       D0100060
C        FORTRAN II. SOME MODIFICATIONS WERE MADE TO MAKE IT OPERABLE   D0100070
C        AND SLIGHTLY MORE EFFICIENT THAN THE SIFTED VERSION.           D0100080
      DIMENSION FMT(120),SCALE(1000),NX(1000),DATA(1000),SUMX(1000),    D0100090
     1 SUMXX(1000),XBAR(1000),XMIN(1001),XMAX(1001),BLANK(9)            D0100100
      DIMENSION TRANS(8,100),KTRANS(4,100)                              D0100110
      COMMON  DATA   , TRANS  , KTRANS , NTR    , NCASE  , METHD        D0100120
      COMMON  BLANK  , NB     , NVAR   , NOVAR                          D0100130
      DATA A123,B123,C123,A2/6HPROBLM,6HFINISH,6HSPCVAL,6HTRNGEN/       D0100140
C                                                                       D0100150
  404 FORMAT(45H1BMD01D SIMPLE DATA DESCRIPTION - VERSION OF            D0100160
     118HJUNE  6, 1966     ,/                                           D0100170
     241H HEALTH SCIENCES COMPUTING FACILITY, UCLA//)                   D0100180
C                                                                       D0100190
      MIN=5                                                             D0100200
 1000 READ (5,100)TODE,PROB,NCASE,NVAR,NADVAR,METHD,NTR,NB, NTAPE,KVR   D0100210
      IF(TODE.EQ.B123)GO TO 997                                         D0100220
  401 IF(TODE.EQ.A123)GO TO 403                                         D0100230
  400 WRITE (6,402)                                                     D0100240
 997  IF(MIN-5)999,999,998                                              D0100250
  998 REWIND MIN                                                        D0100260
  999 STOP                                                              D0100270
 403  CALL TPWD(NTAPE,MIN)                                              D0100280
      IF(NB) 400,411,2223                                               D0100290
 2223 IF(NB-9)2222,400,400                                              D0100300
 2222 READ (5,102)SPEC,(BLANK(I),I=1,NB)                                D0100310
      IF(C123.NE.SPEC)GO TO 400                                         D0100320
  411 IF(KVR.GT.0.AND.KVR.LE.10)GO TO 203                               D0100330
      KVR=1                                                             D0100340
      WRITE(6,4000)                                                     D0100350
  203 IVR=KVR*12                                                        D0100360
      IF(NVAR)400,400,303                                               D0100370
 303  NOVAR=NVAR+NADVAR                                                 D0100380
      IF(NOVAR-1000) 304,304,400                                        D0100390
  304 IF(NTR-100)204,204,400                                            D0100400
  204 IF(NTR)400,205,206                                                D0100410
  205 ASSIGN 12 TO NNN                                                  D0100420
      ASSIGN 112 TO NJ                                                  D0100430
      GO TO 210                                                         D0100440
  206 ASSIGN 13 TO NNN                                                  D0100450
      ASSIGN 113 TO NJ                                                  D0100460
      DO 8 I=1,NTR                                                      D0100470
      READ (5,200)TODE,KTRANS(1,I),KTRANS(2,I),KTRANS(3,I),TRANS(8,I),KTD0100480
     1RANS(4,I),(TRANS(J,I),J=1,7)                                      D0100490
      IF(TODE.NE.A2)GO TO 400                                           D0100500
    8 CONTINUE                                                          D0100510
  210 WRITE (6,404)                                                     D0100520
      READ (5,101)(FMT(I),I=1,IVR)                                      D0100530
  207 WRITE (6,302)                                                     D0100540
      WRITE (6,500)PROB,METHD,NCASE,NB,NVAR,NTR,NADVAR,MIN, KVR         D0100550
      WRITE (6,501)(BLANK(I),I=1,NB)                                    D0100560
      WRITE (6,108)                                                     D0100570
      WRITE (6,106)(FMT(I),I=1,IVR)                                     D0100580
      GO TO NJ,(112,113)                                                D0100590
  113 WRITE (6,1403)                                                    D0100600
      WRITE (6,1400)                                                    D0100610
      DO 334 I=1,NTR                                                    D0100620
      IF(KTRANS(2,I)-40) 331,332,331                                    D0100630
  331 WRITE (6,1401)I,KTRANS(1,I),KTRANS(2,I),KTRANS(3,I),TRANS(8,I)    D0100640
      GO TO 334                                                         D0100650
  332 J=KTRANS(4,I)                                                     D0100660
      IF(J*(J-8)) 333,400,400                                           D0100670
  333 WRITE (6,1402)I,KTRANS(1,I),KTRANS(2,I),KTRANS(3,I),TRANS(8,I),(TRD0100680
     1ANS(JJ,I),JJ=1,J)                                                 D0100690
  334 CONTINUE                                                          D0100700
  112 DO 4 I=1,NOVAR                                                    D0100710
      SUMX(I)=0.0                                                       D0100720
      SUMXX(I)=0.0                                                      D0100730
      XMIN(I)=10.0**25                                                  D0100740
      XMAX(I)=-10.0**25                                                 D0100750
    4 NX(I)=0                                                           D0100760
      IF(METHD) 6,6,300                                                 D0100770
    6 ASSIGN 378 TO METD                                                D0100780
      GO TO 379                                                         D0100790
  300 ASSIGN 377 TO METD                                                D0100800
C        THE CODING USING THE MOD FUNCTION IS USED TO ALLOW THE TOTAL   D0100810
C        NUMBER OF CASES TO BE GREATER THAN 2**15 -1 (32767).           D0100820
  379 NCASE1=NCASE                                                      D0100830
  380 NCASE2=MOD(NCASE1,32767)                                          D0100840
      IF(NCASE2.EQ.0)NCASE2=32767                                       D0100850
      DO 50 I=1,NCASE2                                                  D0100860
      READ (MIN,FMT)(DATA(J),J=1,NVAR)                                  D0100870
      GO TO METD,(377,378)                                              D0100880
  377 CALL MISVAL                                                       D0100890
  378 GO TO NNN,(12,13)                                                 D0100900
   13 CALL TRNGEN(I)                                                    D0100910
      IF(-NVAR)12,12,400                                                D0100920
   12 DO 18 J=1,NOVAR                                                   D0100930
      IF(DATA(J)-999.00999) 66,18,66                                    D0100940
   66 NX(J)=NX(J)+1                                                     D0100950
      SUMX(J)=SUMX(J)+DATA(J)                                           D0100960
      SUMXX(J)=SUMXX(J)+DATA(J)**2                                      D0100970
      XMAX(J)=AMAX1(XMAX(J),DATA(J))                                    D0100980
      XMIN(J)=AMIN1(XMIN(J),DATA(J))                                    D0100990
   18 CONTINUE                                                          D0101000
   50 CONTINUE                                                          D0101010
      NCASE1=NCASE1-NCASE2                                              D0101020
      IF(NCASE1.GT.0)GO TO 380                                          D0101030
      WRITE (6,105)                                                     D0101040
      DO 110 I=1,NOVAR                                                  D0101050
      DIV=NX(I)                                                         D0101060
      IF(DIV)109,109,10                                                 D0101070
   10 XBAR(I)=SUMX(I)/DIV                                               D0101080
      SUMXX(I)=(SUMXX(I)-(SUMX(I)**2)/DIV)/(DIV-1.0)                    D0101090
      SUMXX(I)=SQRT(SUMXX(I))                                           D0101100
      SUMX(I)=SUMXX(I)/SQRT(DIV)                                        D0101110
      GO TO 11                                                          D0101120
  109 WRITE (6,103)                                                     D0101130
      GO TO 110                                                         D0101140
   11 RANGE=XMAX(I  )-XMIN(I  )                                         D0101150
      WRITE (6,104)I, XBAR(I),SUMXX(I),SUMX(I) ,NX(I) , XMAX(I ),XMIN(I D0101160
     1 ),RANGE                                                          D0101170
  110 CONTINUE                                                          D0101180
      GO TO 1000                                                        D0101190
C                                                                       D0101200
  100 FORMAT(2A6,I5,I3,I4,I1,I3,I1,39X,2I2)                             D0101210
  101 FORMAT(12A6)                                                      D0101220
  102 FORMAT(A6,8F6.0)                                                  D0101230
  103 FORMAT(26H NO DATA FOR THIS VARIABLE)                             D0101240
  104 FORMAT(2H  I4,F13.4,2F12.4,I7 ,4X,3F12.4)                         D0101250
  105 FORMAT(7H0VAR NO6X,4HMEAN8X,4HS.D.4X,12HS.E. OF MEAN2X,6HSAMPLE6X,D0101260
     130HMAXIMUM     MINIMUM      RANGE//)                              D0101270
  106 FORMAT(1H 12A6)                                                   D0101280
  107 FORMAT(1H 20F5.2)                                                 D0101290
  108 FORMAT(24H0VARIABLE FORMAT CARD(S))                               D0101300
  200 FORMAT(A6,I3,I2,I3,F6.0,5X,I1,7(F6.0))                            D0101310
  302 FORMAT(13H0PROBLEM CARD)                                          D0101320
  402 FORMAT(45H0CONTROL CARDS INCORRECTLY ORDERED OR PUNCHED)          D0101330
  500 FORMAT(15H PROBLEM NUMBER9X,A6,5X,13HMETHOD NUMBERI18/            D0101340
     1  16H NUMBER OF CASESI14,5X,24HNUMBER OF SPECIAL VALUESI7/        D0101350
     2  20H NUMBER OF VARIABLESI10,5X,26HNUMBER OF TRANSGENERATIONSI5/  D0101360
     3  26H NUMBER OF VARIABLES ADDEDI4,5X,17HINPUT TAPE NUMBERI14/     D0101370
     4  10X,31HNUMBER OF VARIABLE FORMAT CARDSI5)                       D0101380
  501 FORMAT(20H0SPECIAL VALUES CARD/1H 8F12.5)                         D0101390
 1400 FORMAT(46H0CARD    NEW     TRANS    ORIG.   ORIG. VAR(B)10X,17HTYPD0101400
     1E-40 CONSTANTS/45H  NO. VARIABLE   CODE    VAR(A)   OR CONSTANT)  D0101410
 1401 FORMAT(2H  I2,I8,2I9,F15.5)                                       D0101420
 1402 FORMAT(2H  I2,I8,2I9,F15.5,5X,5F14.5/50X,2F14.5)                  D0101430
 1403 FORMAT(1H06X,24H TRANS GENERATOR CARD(S))                         D0101440
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIFD0101450
     1IED, ASSUMED TO BE 1.)                                            D0101460
C                                                                       D0101470
      END                                                               D0101480
CIBFTC MISVA   LIST,M94,XR7                                             D0101490
C        SUBROUTINE MISVAL FOR BMD01D                 JUNE  6, 1966     D0101500
      SUBROUTINE MISVAL                                                 D0101510
      DIMENSION DATA(1000),TRANS(8,100),KTRANS(4,100),BLANK(9)          D0101520
      COMMON  DATA   , TRANS  , KTRANS , NTR    , NCASE  , METHD        D0101530
      COMMON  BLANK  , NB     , NVAR   , NOVAR                          D0101540
      DO 50 J=1,NVAR                                                    D0101550
      GO TO (10,20,30),METHD                                            D0101560
   10 IF(DATA(J)) 50,12,50                                              D0101570
   12 IF(SIGN(1.0,DATA(J))) 55,50,50                                    D0101580
   20 IF(DATA(J)) 54,22,54                                              D0101590
   22 IF(SIGN(1.0,DATA(J))) 55,54,54                                    D0101600
   54 DO 3 I=1,NB                                                       D0101610
      IF(DATA(J)-BLANK(I)) 3,55,3                                       D0101620
    3 CONTINUE                                                          D0101630
      GO TO 50                                                          D0101640
   30 DO 4 I=1,NB                                                       D0101650
      IF(DATA(J)-BLANK(I)) 4,55,4                                       D0101660
    4 CONTINUE                                                          D0101670
      GO TO 50                                                          D0101680
   55 DATA(J)=999.00999                                                 D0101690
   50 CONTINUE                                                          D0101700
      RETURN                                                            D0101710
      END                                                               D0101720
CIBFTC TPTW1   LIST,M94,XR7                                             D0101730
C        SUBROUTINE TPWD FOR BMD01D                   JUNE  6, 1966     D0101740
      SUBROUTINE TPWD(NT1,NT2)                                          D0101750
      IF(NT1)40,10,12                                                   D0101760
 10   NT1=5                                                             D0101770
 12   IF(NT1-NT2)14,19,14                                               D0101780
   14 IF(NT2.EQ.5)GO TO 18                                              D0101790
   17 REWIND NT2                                                        D0101800
   19 IF(NT1-5)18,24,18                                                 D0101810
 18   IF(NT1-6)22,40,22                                                 D0101820
 22   REWIND NT1                                                        D0101830
 24   NT2=NT1                                                           D0101840
 28   RETURN                                                            D0101850
 40   WRITE (6,49)                                                      D0101860
      STOP                                                              D0101870
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              D0101880
      END                                                               D0101890
CIBFTC TRNG    LIST,M94,XR7                                             D0101900
C        SUBROUTINE TRNGEN FOR BMD01D                 JUNE  6, 1966     D0101910
      SUBROUTINE TRNGEN(NINCS)                                          D0101920
C                                                                       D0101930
      DIMENSION DATA(1000),TRANS(8,100),KTRANS(4,100),BLANK(9)          D0101940
      COMMON  DATA   , TRANS  , KTRANS , NTR    , NCASE  , METHD        D0101950
      COMMON  BLANK  , NB     , NVAR   , NOVAR                          D0101960
C                                                                       D0101970
      ASN(XX)=ATAN(XX/SQRT(1.0-XX**2))                                  D0101980
C                                                                       D0101990
      PI2=1.57079633                                                    D0102000
      FN=NCASE                                                          D0102010
      IF(NVAR-1000)204,206,206                                          D0102020
 204  NVA=NVAR+1                                                        D0102030
      DO 205 J=NVA,NOVAR                                                D0102040
  205 DATA(J)=0.0                                                       D0102050
  206 DO 110 I=1,NTR                                                    D0102060
      M=KTRANS(1,I)                                                     D0102070
      N=KTRANS(3,I)                                                     D0102080
      NTRANS=KTRANS(2,I)                                                D0102090
      D2=DATA(N)                                                        D0102100
      IF((NTRANS-11)*(NTRANS-12)*(NTRANS-13)*(NTRANS-14)*(NTRANS-16)*   D0102110
     1(NTRANS-23)) 58,57,58                                             D0102120
   57 NEWB=TRANS(8,I)                                                   D0102130
      IF(DATA(NEWB)-999.00999) 58,92,58                                 D0102140
   58 IF(D2-999.00999)59,92,59                                          D0102150
   59 IF((KTRANS(2,I)-25)*KTRANS(2,I)) 50,99,60                         D0102160
   60 IF(KTRANS(2,I)-40)99,40,99                                        D0102170
   99 WRITE (6,199)I                                                    D0102180
      NVAR=-NVAR                                                        D0102190
      GOTO100                                                           D0102200
   50 GOTO(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,99,99,20,21,22,23, D0102210
     124),NTRANS                                                        D0102220
    1 IF(D2)198,107,108                                                 D0102230
  107 D1=0.0                                                            D0102240
      GOTO100                                                           D0102250
  108 D1=SQRT(D2)                                                       D0102260
      GOTO100                                                           D0102270
    2 IF(D2)198,111,112                                                 D0102280
  111 D1=1.0                                                            D0102290
      GOTO100                                                           D0102300
  112 D1=SQRT(D2)+SQRT(D2+1.0)                                          D0102310
      GOTO100                                                           D0102320
    3 IF(-D2)114,198,198                                                D0102330
  114 D1=EXP(D2)                                                        D0102340
      GOTO100                                                           D0102350
    4 D1=EXP(D2)                                                        D0102360
      GOTO100                                                           D0102370
    5 IF(-D2)117,107,198                                                D0102380
  117 IF(D2-1.0)118,119,198                                             D0102390
  118 D1=ASN(SQRT(D2))                                                  D0102400
      GOTO100                                                           D0102410
  119 D1=PI2                                                            D0102420
      GOTO100                                                           D0102430
    6 A=D2/(FN+1.0)                                                     D0102440
      B=A+1.0/(FN+1.0)                                                  D0102450
      IF(A) 198,127,124                                                 D0102460
  127 D1=ASN(SQRT(B))                                                   D0102470
      GOTO100                                                           D0102480
  124 IF(B)198,128,129                                                  D0102490
 128  IF(A-1.0)123,125,198                                              D0102500
  123 D1=ASN(SQRT(A))*2.0                                               D0102510
      GOTO100                                                           D0102520
  125 D1=3.14159265                                                     D0102530
      GO TO 100                                                         D0102540
  129 A=SQRT(A)                                                         D0102550
      B=SQRT(B)                                                         D0102560
      D1=ASN(A)+ASN(B)                                                  D0102570
      GOTO100                                                           D0102580
    7 IF(D2)131,198,131                                                 D0102590
  131 D1=1.0/D2                                                         D0102600
      GOTO100                                                           D0102610
    8 D1=D2+TRANS(8,I)                                                  D0102620
      GOTO100                                                           D0102630
    9 D1=D2*TRANS(8,I)                                                  D0102640
      GOTO100                                                           D0102650
   10 IF(-D2)133,107,198                                                D0102660
  133 D1=D2**TRANS(8,I)                                                 D0102670
      GOTO100                                                           D0102680
   11 D1=D2+DATA(NEWB)                                                  D0102690
      GOTO100                                                           D0102700
   12 D1=D2-DATA(NEWB)                                                  D0102710
      GOTO100                                                           D0102720
   13 D1=D2*DATA(NEWB)                                                  D0102730
      GOTO100                                                           D0102740
   14 IF(DATA(NEWB))134,198,134                                         D0102750
  134 D1=D2/DATA(NEWB)                                                  D0102760
      GOTO100                                                           D0102770
   15 IF(D2-TRANS(8,I))107,111,111                                      D0102780
   16 IF(D2-DATA(NEWB))107,111,111                                      D0102790
   17 IF(-D2)163,198,198                                                D0102800
  163 D1=ALOG(D2)                                                       D0102810
      GO TO 100                                                         D0102820
   20 D1=SIN(D2)                                                        D0102830
      GO TO 100                                                         D0102840
   21 D1=COS(D2)                                                        D0102850
      GO TO 100                                                         D0102860
   22 IF(D2-PI2)186,186,198                                             D0102870
  186 IF(D2+PI2)198,187,187                                             D0102880
  187 D2=ATAN(D2)                                                       D0102890
      GO TO 100                                                         D0102900
   23 IF(-D2)188,107,198                                                D0102910
  188 D1=D2**DATA(NEWB)                                                 D0102920
      GO TO 100                                                         D0102930
   24 IF(TRANS(8,I)) 198,107,189                                        D0102940
  189 D1=TRANS(8,I)**D2                                                 D0102950
      GO TO 100                                                         D0102960
   40 IF((KTRANS(4,I)-8)*KTRANS(4,I))45,99,99                           D0102970
   45 K=KTRANS(4,I)                                                     D0102980
      DO 41 J=1,K                                                       D0102990
      IF(D2-TRANS(J,I))41,42,41                                         D0103000
   42 C=SIGN(1.0,D2)                                                    D0103010
      D=SIGN(1.0,TRANS(J,I))                                            D0103020
      IF(C+D)43,41,43                                                   D0103030
   41 CONTINUE                                                          D0103040
      GO TO 110                                                         D0103050
   43 D1=TRANS(8,I)                                                     D0103060
      GOTO100                                                           D0103070
  198 WRITE (6,201)N,NINCS,KTRANS(2,I),M                                D0103080
   92 D1=999.00999                                                      D0103090
  100 DATA(M)=D1                                                        D0103100
  110 CONTINUE                                                          D0103110
  199 FORMAT(21H0TRANSGENERATION CARDI3,26HMISPUNCHED OR OUT OF ORDER)  D0103120
  201 FORMAT(22H0THE VALUE OF VARIABLEI4,8H IN CASEI5,54H VIOLATED THE RD0103130
     1ESTRICTIONS FOR TRANSGENERATION OF TYPEI3,1H./40H THE PROGRAM CONTD0103140
     2INUED TREATING VARIABLEI4,20H AS A MISSING VALUE.)                D0103150
      RETURN                                                            D0103160
      END                                                               D0103170
