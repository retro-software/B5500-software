FILE  5 = CARD, UNIT = READER, RECORD = 10, BUFFER = 2                  00000100
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00000200
FILE  7 = X85, UNIT = TAPE, RECORD = 3, BUFFER = 2                      00000300
C                                                                       00000400
C     BMDX85 ---- NON-LINEAR LEAST SQUARES ANALYSIS CURVE FIT (FORTRAN) 00000500
C                                                                       00000600
C        SUBROUTINE FUN FOR TEST PROBLEM OF BMDX85                      00000700
      SUBROUTINE FUN(F,FP,P,X,NP);                                      00000800
      DIMENSION X(1),FP(NP),P(NP)                                       00000900
      NPP=NP-1                                                          00001000
C        LOG TRANSFORM ON EXP CURVE                                     00001100
      U=0.0                                                             00001200
      DO 10003 I=1,NPP,2                                                00001300
      J=I+1                                                             00001400
      FP(I)= EXP(P(J)|X(1))                                             00001500
10003 U=U + P(I) | FP(I)                                                00001600
      IF (U) 10004, 10004, 10005                                        00001700
10004 WRITE(6,10000) U,(P(I),I=1,NP),X(1)                               00001800
      WRITE(6,10000)                                                    00001900
10000 FORMAT(1P10E12.4)                                                 00002000
10005 F = ALOG(U)                                                       00002100
      U =1/U                                                            00002200
      DO 10002 I=1,NPP,2                                                00002300
      J=I+1                                                             00002400
      FP(I)=U|FP(I)                                                     00002500
10002 FP(J)=P(I)|X(1)|FP(I)                                             00002600
      RETURN                                                            00002700
      END                                                               00002800
C        SUBROUTINE STEP FOR BMDX85                  APRIL  4, 1966     00002900
      SUBROUTINE STEP(A,D,P,P0,P1,NP1,TOL)                              00003000
      DIMENSION A(NP1,NP1),V(100),D(100),IN(100),P0(100),P1(100),P(100) 00003100
      NP#NP1-1                                                          00003200
      DO 20 I#1,NP1                                                     00003300
      V(I)#SQRT(A(I,I))                                                 00003400
      DO 20 J#1,I                                                       00003500
 20   A(I,J)#A(I,J)/(V(I)*V(J))                                         00003600
      DO 2 I#1,NP                                                       00003700
 2    IN(I)#0                                                           00003800
 8    K#0                                                               00003900
      R#0.                                                              00004000
      DO 1 I#1,NP                                                       00004100
      IF(IN(I).NE.0 .OR. A(I,I).LE.TOL) GO TO 1                         00004200
      Q#A(NP1,I)*A(NP1,I)/A(I,I)                                        00004300
      IF(Q.LE.R) GO TO 1                                                00004400
      K#I                                                               00004500
      R#Q                                                               00004600
 1    CONTINUE                                                          00004700
      IF(K.EQ.0) GO TO 3                                                00004800
      C#-1.                                                             00004900
 4    DO 5 I#1,K                                                        00005000
      D(I)#A(K,I)                                                       00005100
 5    A(K,I)#0.                                                         00005200
      PP#D(K)                                                           00005300
      DO 6 I#K,NP1                                                      00005400
      D(I)#A(I,K)                                                       00005500
 6    A(I,K)#0.                                                         00005600
      D(K)#C                                                            00005700
      IN(K)#IN(K)&1                                                     00005800
      DO 7 I#1,NP1                                                      00005900
      Y#D(I)/PP                                                         00006000
      DO 7 J#1,I                                                        00006100
 7    A(I,J)#A(I,J)-D(J)*Y                                              00006200
      GO TO 8                                                           00006300
 3    R#1.                                                              00006400
      DO 9 I#1,NP                                                       00006500
      IF(IN(I).NE.1) GO TO 9                                            00006600
      D(I)#A(NP1,I)*V(NP1)/V(I)                                         00006700
      H#AMAX1((P0(I)-P(I))/D(I),(P1(I)-P(I))/D(I))                      00006800
      IF(H.GE.R) GO TO 9                                                00006900
      R#H                                                               00007000
      K#I                                                               00007100
 9    CONTINUE                                                          00007200
      C#1.                                                              00007300
      IF(R.LE.TOL) GO TO 4                                              00007400
      DO 10 I#1,NP                                                      00007500
      IF(IN(I).NE.1) GO TO 11                                           00007600
      D(I)#D(I)*R                                                       00007700
      DO 21 J#1,I                                                       00007800
      A(I,J)#-A(I,J)/(V(I)*V(J))                                        00007900
 21   A(J,I)#A(I,J)                                                     00008000
      GO TO 10                                                          00008100
 11   D(I)#0.                                                           00008200
      DO 22 J#1,I                                                       00008300
      A(I,J)#0.                                                         00008400
 22   A(J,I)#0.                                                         00008500
 10   CONTINUE                                                          00008600
      RETURN                                                            00008700
      END                                                               00008800
C        SUBROUTINE MINIZ FOR BMDX85                 APRIL  4, 1966     00008900
      SUBROUTINE MINIZ(X,A,P,NV,NC,NP1,TOL,EPS,EE,ND,MXIT,LLK,P0,P1,F,T)00009000
      DIMENSION A(NP1,NP1),X(NV,NC),P(100),FP(100),D(100),P0(100),P1(10000009100
     X),F(NC)                                                           00009200
      DIMENSION PP0(100)                                                00009300
      DATA BLANK,ERR,OR,ME,AN,SQU,ARE/1H ,6H    ER,6HROR   ,6H    ME,   00009400
     X6HAN    ,6H    SQ,6HUARE  /                                       00009500
      NP#NP1-1                                                          00009600
      MNAIL # 10                                                        00009700
      NAIL # 0                                                          00009800
      DO 557 I#1,NP                                                     00009900
 557  PP0(I)#P(I)                                                       00010000
      NP3#NP-2                                                          00010100
      EPS1#5.*EPS&1.                                                    00010200
      IF(NP.EQ.1) GO TO 191                                             00010300
      IF(NP.GE.10) GO TO 192                                            00010400
      WRITE (6,26) (P0(I),I#1,NP)                                       00010500
 26   FORMAT(10H1MINIMA   1P9E12.4)                                     00010600
      WRITE (6,27) (P1(I),I#1,NP)                                       00010700
 27   FORMAT(10H0MAXIMA   1P9E12.4)                                     00010800
      IF(NP.EQ.2) WRITE (6,177) ERR,OR                                  00010900
      IF(NP.NE.2) WRITE (6,177) (BLANK,BLANK,I#1,NP3),ERR,OR            00011000
 177  FORMAT(36H0ITERATION     PARAMETERS                   16A6)       00011100
      NP3#NP&1                                                          00011200
      WRITE (6,178) (BLANK,BLANK,I#1,NP3),ME,AN                         00011300
      WRITE (6,178) (BLANK,BLANK,I#1,NP3),SQU,ARE                       00011400
 178  FORMAT(22A6)                                                      00011500
 194  LLL#5                                                             00011600
      LIT#0                                                             00011700
      HU#NC                                                             00011800
      H#0.                                                              00011900
      LLLL#5                                                            00012000
      E0#1.E20                                                          00012100
      DO 556 I#1,NP                                                     00012200
 556  P(I)#PP0(I)                                                       00012300
      FNC#NC-NP                                                         00012400
 29   DO 85 I#1,NP1                                                     00012500
      DO 85 J#1,I                                                       00012600
 85   A(I,J)#0.                                                         00012700
      DO 1 L#1,NC                                                       00012800
C     CALL FUN(F(L),FP,X(1,L),P,LLK)                                    00012900
      CALL FUN(F(L),FP,P,X(1,L),NP)                                     00013000
      FP(NP1)#X(ND,L)-F(L)                                              00013100
      DO 1 I#1,NP1                                                      00013200
      DO 1 J#1,I                                                        00013300
 1    A(I,J)#A(I,J)&FP(I)*FP(J)                                         00013400
      EE#A(NP1,NP1)/FNC                                                 00013500
      IF(EE.LE.E0) GO TO 871                                            00013600
      NAIL#NAIL&1                                                       00013700
      IF(NAIL.GT.MNAIL) GO TO 871                                       00013800
      DO 872 I#1,NP                                                     00013900
      P(I)#PP0(I)&D(I)/2.                                               00014000
 872  D(I)#D(I)/2.                                                      00014100
      GO TO 29                                                          00014200
 871  IF(NP.LT.10) WRITE (6,174) LIT,NAIL,(P(I),I#1,NP),EE              00014300
 174  FORMAT(I6,1X,I2,1X,1P10E12.4)                                     00014400
      IF(NP.GE.10) WRITE (6,198) LIT,NAIL,EE,(P(I),I#1,NP)              00014500
 198  FORMAT(I6,1X,I2,1X,1P10E12.4/(22X,1P9E12.4))                      00014600
      NAIL#0                                                            00014700
 8191 LIT#LIT&1                                                         00014800
      IF(LIT.GT.MXIT) GO TO 200                                         00014900
      IF(EE.EQ.0.) GO TO 470                                            00015000
      PC#ABS((E0-EE)/EE)                                                00015100
      E0#EE                                                             00015200
      IF(PC.LT.EPS) GO TO 100                                           00015300
      CALL STEP(A,D,P,P0,P1,NP1,TOL)                                    00015400
      LLL#LLLL                                                          00015500
 121  DO 28 I#1,NP                                                      00015600
      PP0(I)#P(I)                                                       00015700
 28   P(I)#P(I)&D(I)                                                    00015800
      GO TO 29                                                          00015900
 200  WRITE (6,201)                                                     00016000
 201  FORMAT(30H0THE PROCESS IS NOT CONVERGING)                         00016100
      GO TO 470                                                         00016200
 100  LLL#LLL-1                                                         00016300
      IF(LLL.NE.0) GO TO 121                                            00016400
 470  DO 778 I#1,NP                                                     00016500
 778  FP(I)#SQRT(ABS(A(I,I)*EE))                                        00016600
      WRITE (6,779) (FP(I),I#1,NP)                                      00016700
 779  FORMAT(50H0ASSYMPTOTIC STANDARD DEVIATIONS OF THE PARAMETERS//    00016800
     X(10X,1P10E12.4))                                                  00016900
      DO 842 I#1,NP                                                     00017000
      FP(I)#SQRT(ABS(A(I,I)))                                           00017100
      DO 842 J#1,I                                                      00017200
      A(I,J)#A(I,J)/(FP(I)*FP(J))                                       00017300
 842  A(J,I)#A(I,J)                                                     00017400
10000 FORMAT (10F12.4)                                                  00017500
      WRITE (6,774)                                                     00017600
 774  FORMAT(48H0ASSYMTOTIC CORRELATION MATRIX OF THE PARAMETERS)       00017700
      L1#0                                                              00017800
 371  L0#L1&1                                                           00017900
      L1#MIN0(NP,L1&10)                                                 00018000
      WRITE (6,372) (L,L#L0,L1)                                         00018100
 372  FORMAT(1H0,10I12)                                                 00018200
      DO 373 I#1,NP                                                     00018300
 373  WRITE (6,374) I,(A(I,J),J#L0,L1)                                  00018400
 374  FORMAT(I4,2X,10F12.5)                                             00018500
      IF(L1.NE.NP) GO TO 371                                            00018600
 1515 WRITE (6,553)                                                     00018700
 553  FORMAT(46H1CASE       F            Y-F         VARIABLES)         00018800
      DO 551 L#1,NC                                                     00018900
      TT#X(ND,L)-F(L)                                                   00019000
 551  WRITE (6,552) L,F(L),TT,(X(I,L),I#1,NV)                           00019100
 552  FORMAT(1X,I4,10F12.5/(29X,8F12.5))                                00019200
      RETURN                                                            00019300
 192  WRITE (6,23) (P0(I),I#1,NP)                                       00019400
 23   FORMAT(7H1MINIMA15X,1P9E12.4/(22X,1P9E12.4))                      00019500
      WRITE (6,22) (P1(I),I#1,NP)                                       00019600
 22   FORMAT(7H0MAXIMA15X,1P9E12.4/(22X,1P9E12.4))                      00019700
      WRITE (6,195)                                                     00019800
 195  FORMAT(34H0ITERATION    ERROR     PARAMETERS/14X,4HMEAN/14X,6HSQUA00019900
     XRE)                                                               00020000
      GO TO 194                                                         00020100
 191  WRITE (6,24) (P0(I),I#1,NP)                                       00020200
 24   FORMAT(10H1MINIMUM    1PE12.4)                                    00020300
      WRITE (6,25) (P1(I),I#1,NP)                                       00020400
 25   FORMAT(10H0MAXIMUM     1PE12.4)                                   00020500
      WRITE (6,196)                                                     00020600
 196  FORMAT(34H0ITERATION  PARAMETER    ERROR                          00020700
     X/25X,4HMEAN/25X,6HSQUARE)                                         00020800
      GO TO 194                                                         00020900
      END                                                               00021000
$ID   0901HS 15 150    $BMDX85 NON-LINEAR LEAST SQUARES                 00021100
C        NON-LINEAR LEAST SQUARES - MAIN PROGRAM     APRIL  4, 1966     00021200
      DIMENSION DATE(3)                                                 00021300
      DATA (DATE(I),I=1,3)/6HAPRIL ,6H4, 196,1H6/                       00021400
      LOGICAL BL                                                        00021500
C     BL(Z)#Z.EQ.0.0 .AND. SIGN(1.,Z).EQ.(-1.)                          00021600
      DATA YES,ONO,GUESS,PROBLM,FINISH,AMON,AMOX/3HYES,2HNO,5HPARAM,6HPR00021700
     XOBLM,6HFINISH,6HMINMUM,6HMAXMUM/                                  00021800
      DIMENSION A(15000),P(100),P0(100),P1(100),F(108)                  00021900
      LLK#0                                                             00022000
 15   READ (5,1) PP,PC,NV,ND,NC,NP,TOL,EPS,MXIT,NF,IT,RE                00022100
      LLK#LLK&1                                                         00022200
    1 FORMAT(2A6,2I4,I6,I4,2F6.0,3I4,A3)                                00022300
C     IF(BL(TOL)) TOL#.00001                                            00022400
      IF (TOL-0)31,30,31                                                00022500
  30  TOL = 0.00001                                                     00022600
C     IF(BL(EPS)) EPS#.00001                                            00022700
  31  IF (EPS-0)33,32,33                                                00022800
  32  EPS = 0.00001                                                     00022900
C     IF(BL(MXIT)) MXIT#100                                             00023000
  33  IF (MXIT-0)35,34,35                                               00023100
  34  MXIT = 100                                                        00023200
  35  TOL = ABS(TOL)                                                    00023300
      IF(RE.NE.YES) RE#ONO                                              00023400
      IF(IT.EQ.0) IT#5                                                  00023500
      IF(RE.EQ.YES) REWIND IT                                           00023600
      NF#MAX0(1,NF)                                                     00023700
      NF#MIN0(NF,9)                                                     00023800
      WRITE (6,1000) (DATE(I),I=1,3),PC,NV,ND,NC,NP,TOL                 00023900
      WRITE (6,1001) EPS,MXIT,NF,IT,RE                                  00024000
      NF#12*NF                                                          00024100
      READ (5,2) (F(I),I#1,NF)                                          00024200
 2    FORMAT(12A6)                                                      00024300
      WRITE (6,1002) (F(I),I#1,NF)                                      00024400
 1000 FORMAT( 48H1BMDX85 - NON LINEAR LEAST SQUARES - VERSION OF 3A6/   00024500
     X/60H0HEALTH SCIENCES COMPUTING FACILITY, UCLA                     00024600
     X/32H0PROBLM CODE                     A6                           00024700
     X/32H0NUMBER OF VARIABLES              I6                          00024800
     X/32H0INDEX OF THE DEPENDENT VARIABLE I6                           00024900
     X/32H0NUMBER OF CASES                 I6                           00025000
     X/32H0NUMBER OF PARAMETERS            I6                           00025100
     X/32H0TOLERANCE                       F10.6)                       00025200
 1001 FORMAT(32H0EPSILON                         F10.6                  00025300
     X/32H0MAXIMUM NUMBER OF ITERATIONS    I6                           00025400
     X/32H0NUMBER OF VARIABLE FORMAT CARDS  I6                          00025500
     X/32H0ALTERNATE INPUT TAPE NUMBER     I6                           00025600
     X/32H0REWIND OPTION                   A6)                          00025700
 1002 FORMAT(16H0VARIABLE FORMAT 16X,16A6/(32X,16A6))                   00025800
      J#1                                                               00025900
      DO 3 L#1,NC                                                       00026000
      READ (IT,F)   (P(I),I=1,NV)                                       00026100
      WRITE (6,10001) L,(P(I),I=1,NV)                                   00026200
C     CALL TRANS(P,L,NV,SELECT)                                         00026300
      DO 3 I#1,NV                                                       00026400
      A(J)#P(I)                                                         00026500
      J#J&1                                                             00026600
   3  CONTINUE                                                          00026700
10001 FORMAT (I5,2X,2F12.5)                                             00026800
      NP1#NP&1                                                          00026900
      L1#1&NV*NC                                                        00027000
      L2#L1&NP1*NP1                                                     00027100
      L3#L2&NC                                                          00027200
      L4#L3&NP1*NP1                                                     00027300
      IF(L4.GT.15000) GO TO 600                                         00027400
      LLJ#0                                                             00027500
      DO 7 I#1,NP                                                       00027600
      P1(I)#1.E20                                                       00027700
 7    P0(I)#-1.E20                                                      00027800
 81   READ (5,2) PP                                                     00027900
      IF(PP.EQ.AMON) READ (5,12) (P0(I),I#1,NP)                         00028000
      IF(PP.EQ.AMOX) READ (5,12)(P1(I),I#1,NP)                          00028100
      IF(PP.EQ.PROBLM) GO TO 15                                         00028200
      IF(PP.EQ.FINISH) STOP                                             00028300
      IF(PP.EQ.GUESS) GO TO 18                                          00028400
      IF(PP.EQ.AMON .OR. PP.EQ.AMOX) GO TO 81                           00028500
      READ (5,22) (P(I),I#1,14)                                         00028600
      WRITE (6,23) (P(I),I#1,14)                                        00028700
 22   FORMAT(13A6,A2)                                                   00028800
 23   FORMAT(19H NON CONTROL CARD (13A6,A2,1H))                         00028900
      GO TO 81                                                          00029000
 18   READ (5,12) (P(I),I#1,NP)                                         00029100
12    FORMAT(6X, 9F8.4)                                                 00029200
      CALL MINIZ(A,A(L1),P,NV,NC,NP1,TOL,EPS,EE,ND,MXIT,LLK,P0,P1,A(L2) 00029300
     X,A(L3))                                                           00029400
      GO TO 81                                                          00029500
 600  WRITE (6,601)                                                     00029600
 601  FORMAT(45H0CONTROL CARDS INCORRECTLY ORDERED OR PUNCHED)          00029700
      STOP                                                              00029800
      END                                                               00029900
