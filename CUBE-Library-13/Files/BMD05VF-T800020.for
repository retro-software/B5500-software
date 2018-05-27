FILE  5 = CRA, UNIT = READER, RECORD = 10, BUFFER = 2                   00000100
FILE  6 = LPA, UNIT = PRINTER, RECORD = 15, BUFFER = 2                  00000200
FILE  2 = TAPE2, UNIT = TAPE, RECORD = 10, BUFFER = 2                   00000300
CTPWD    SUBROUTINE TPWD FOR BMD05V         VERSION OF SEPT. 26, 1963   00000400
      SUBROUTINE TPWD(NT1,NT2)                                          00000500
      IF(NT1)40,10,12                                                   00000600
 10   NT1=5                                                             00000700
 12   IF(NT1-NT2)14,19,14                                               00000800
 14   IF(NT2-5)15,19,19                                                 00000900
   15 REWIND NT2                                                        00001000
      GO TO 19                                                          00001100
   19 IF(NT1-5)18,24,18                                                 00001200
 18   IF(NT1-6)22,40,22                                                 00001300
 22   REWIND NT1                                                        00001400
 24   NT2=NT1                                                           00001500
 28   RETURN                                                            00001600
 40   WRITE (6,49)                                                      00001700
 49   FORMAT(25H ERROR ON TAPE ASSIGNMENT)                              00001800
      END                                                               00001900
CTRANS        SUBROUTINE TRANS FOR BMD05V          OCTOBER 13, 1964     00002000
      SUBROUTINE TRANS (NVG,ONNN,IR,IERROR)                             00002100
      ASN (XX)=ATAN (XX/SQRT (1.0-XX**2))                               00002200
      DIMENSION S(61,63),ND(61),T(61,63),A(63),B(61),MP(61),MA(61,64),  00002300
     1SUM(64),ID(64),X(61,63),IFF(2,64),COE(61,64),P(1060),SU(64),      00002400
     2AM(61,64),FMT(61),NEWA(64),LCODE(61),LVA(61),BNEW(61)             00002500
      COMMON S,ND,T,A,B,MP,MA,SUM,ID,X,IFF,COE,P,SU,AM,    NEWA,        00002600
     1LCODE,LVA,BNEW,N,NP,NH,NO,CON,JUNK,NCO,NR                         00002700
      IERROR=0                                                          00002800
      DO 200 I=1,NVG                                                    00002900
      J=NEWA(I)                                                         00003000
      NTR=LCODE(I)                                                      00003100
      K=LVA(I)                                                          00003200
      IF(NTR*(NTR-15)) 4,99,99                                          00003300
    4 IF(NTR-11) 5,7,7                                                  00003400
    5 POWER=BNEW(I)                                                     00003500
      GO TO 8                                                           00003600
    7 II=BNEW(I)                                                        00003700
    8 GO TO(10,20,30,40,50,60,70,80,90,100,110,120,130,140),NTR         00003800
   10 IF(T(1,K))99,32,9                                                 00003900
    9 T(1,J)=SQRT (T(1,K))                                              00004000
      GO TO 200                                                         00004100
   20 IF(T(1,K))99,11,12                                                00004200
   11 T(1,J)=1.0                                                        00004300
      GO TO 200                                                         00004400
   12 T(1,J)=SQRT (T(1,K))+SQRT (T(1,K)+1.0)                            00004500
      GO TO 200                                                         00004600
   30 IF(T(1,K))99,99,14                                                00004700
   14 T(1,J)=ALOG10(T(1,K))                                             00004800
      GO TO 200                                                         00004900
   40 T(1,J)=EXP (T(1,K))                                               00005000
      GO TO 200                                                         00005100
   50 IF(T(1,K))99,32,17                                                00005200
   17 IF(T(1,K)-1.0)18,19,99                                            00005300
   19 T(1,J)=3.14159265/2.0                                             00005400
      GO TO 200                                                         00005500
   18 D=SQRT (T(1,K))                                                   00005600
      T(1,J)=ASN (D)                                                    00005700
      GO TO 200                                                         00005800
   60 D=T(1,K)/ONNN                                                     00005900
      E=D+1.0/ONNN                                                      00006000
      IF(D)99,23,24                                                     00006100
   23 IF(E)99,32,27                                                     00006200
   27 T(1,J)=ASN (SQRT (E))                                             00006300
      GO TO 200                                                         00006400
   24 IF(E)99,28,29                                                     00006500
   28 T(1,J)=ASN (SQRT (D))                                             00006600
      GO TO 200                                                         00006700
   29 D=SQRT (D)                                                        00006800
      E=SQRT (E)                                                        00006900
      T(1,J)=ASN (D)+ASN (E)                                            00007000
      GO TO 200                                                         00007100
   70 IF(T(1,K))31,99,31                                                00007200
   31 T(1,J)=1.0/T(1,K)                                                 00007300
      GO TO 200                                                         00007400
   80 T(1,J)=T(1,K)+POWER                                               00007500
      GO TO 200                                                         00007600
   90 T(1,J)=T(1,K)*POWER                                               00007700
      GO TO 200                                                         00007800
  100 IF(T(1,K))33,32,33                                                00007900
   32 T(1,J)=0.0                                                        00008000
      GO TO 200                                                         00008100
   33 T(1,J)=T(1,K)**POWER                                              00008200
      GO TO 200                                                         00008300
  110 T(1,J)=T(1,K)+T(1,II)                                             00008400
      GO TO 200                                                         00008500
  120 T(1,J)=T(1,K)-T(1,II)                                             00008600
      GO TO 200                                                         00008700
  130 T(1,J)=T(1,K)*T(1,II)                                             00008800
      GO TO 200                                                         00008900
  140 IF(T(1,II))34,99,34                                               00009000
   34 T(1,J)=T(1,K)/T(1,II)                                             00009100
      GO TO 200                                                         00009200
   99 WRITE (6,900)           I,IR,T(1,K)                               00009300
      IERROR=-99                                                        00009400
      GO TO 250                                                         00009500
  200 CONTINUE                                                          00009600
  900 FORMAT(51H0ERROR OCCURRED DURING TRANS-GENERATION PASS NUMBERI3,  00009700
     115H, DESIGN NUMBERI5,1H./15H THIS VALUE IS F15.5,2H.              00009800
     2   36HTHIS IS THE FIRST ERROR ENCOUNTERED./41H PROGRAM WILL GO TO 00009900
     3NEXT PROBLEM, IF ANY.)                                            00010000
  250 RETURN                                                            00010100
      END                                                               00010200
CVFCHCK SUBROUTINE  VFCHCK FOR BMD05V    MAY 26,1964                    00010300
      SUBROUTINE VFCHCK(NVF)                                            00010400
      IF(NVF)10,10,20                                                   00010500
 10   WRITE (6,4000)                                                    00010600
      NVF=1                                                             00010700
 50   RETURN                                                            00010800
C                                                                       00010900
   20 IF(NVF-5) 50,50,10                                                00011000
C                                                                       00011100
 4000 FORMAT(1H023X71HNUMBER OF VARIABLE FORMAT CARDS INCORRECTLY SPECIF00011200
     XIED, ASSUMED TO BE 1.)                                            00011300
      END                                                               00011400
CMATRIX       SUBROUTINE MATRIX FOR BMD05V         OCTOBER 13, 1964     00011500
      SUBROUTINE MATRIX (N1,N2)                                         00011600
      DIMENSION S(61,63),ND(61),T(61,63),A(63),B(61),MP(61),MA(61,64),  00011700
     1SUM(64),ID(64),X(61,63),IFF(2,64),COE(61,64),P(1060),SU(64),      00011800
     2AM(61,64),FMT(61),NEWA(64),LCODE(61),LVA(61),BNEW(61)             00011900
      COMMON S,ND,T,A,B,MP,MA,SUM,ID,X,IFF,COE,P,SU,AM,    NEWA,        00012000
     1LCODE,LVA,BNEW,N,NP,NH,NO,CON,JUNK,NCO,NR                         00012100
      JUNK=0                                                            00012200
      DO 12 I=1,NP                                                      00012300
 12   B(I)=0.0                                                          00012400
      IJ=0                                                              00012500
      NM=NP-1                                                           00012600
 46   IJ=IJ+1                                                           00012700
      NO=IJ                                                             00012800
      BIG=ABS (T(IJ,IJ))                                                00012900
      DO 50 I=IJ,NP                                                     00013000
      IF(BIG-ABS (T(I,IJ))) 47, 50, 50                                  00013100
 47   NO=I                                                              00013200
      BIG=ABS (T(I,IJ))                                                 00013300
 50   CONTINUE                                                          00013400
      IF(BIG-CON) 51, 51, 63                                            00013500
 51   B(IJ)=1.0                                                         00013600
      JUNK=JUNK+1                                                       00013700
      GO TO 46                                                          00013800
 49   JUNK=JUNK+1                                                       00013900
 52   K=NP                                                              00014000
      KM=K-1                                                            00014100
      DO 55 I=1,KM                                                      00014200
      SUK=0.0                                                           00014300
      KP=K-I                                                            00014400
      IF(B(KP)-1.0) 53, 56, 56                                          00014500
 56   B(KP)=0.0                                                         00014600
      GO TO 55                                                          00014700
 53   KQ=KP+1                                                           00014800
      DO 54 J=KQ,K                                                      00014900
 54   SUK=SUK+T(KP,J)*B(J)                                              00015000
      B(KP)=T(KP,N1)-SUK                                                00015100
 55   CONTINUE                                                          00015200
      GO TO 90                                                          00015300
 63   IF(NO-IJ) 66, 66, 64                                              00015400
 64   DO 65 J=IJ,N2                                                     00015500
      AA=T(IJ,J)                                                        00015600
      T(IJ,J)=T(NO,J)                                                   00015700
 65   T(NO,J)=AA                                                        00015800
 66   POV=T(IJ,IJ)                                                      00015900
      DO 70 J=IJ,N1                                                     00016000
 70   T(IJ,J)=T(IJ,J)/POV                                               00016100
      K=IJ+1                                                            00016200
      DO 75 I=K,NP                                                      00016300
      DO 75 J=K,N1                                                      00016400
 75   T(I,J)=T(I,J)-T(I,IJ)*T(IJ,J)                                     00016500
      IF(IJ-NM) 46, 80, 80                                              00016600
 80   IF(ABS (T(NP,NP))-CON) 49, 49, 85                                 00016700
 85   B(NP)=T(NP,N1)/T(NP,NP)                                           00016800
      GO TO 52                                                          00016900
 90   RETURN                                                            00017000
      END                                                               00017100
CPUNCH        SUBROUTINE PUNCH FOR BMD05V          OCTOBER 13, 1964     00017200
      SUBROUTINE PUNCH                                                  00017300
      DIMENSION S(61,63),ND(61),T(61,63),A(63),B(61),MP(61),MA(61,64),  00017400
     1SUM(64),ID(64),X(61,63),IFF(2,64),COE(61,64),P(1060),SU(64),      00017500
     2AM(61,64),FMT(61),NEWA(64),LCODE(61),LVA(61),BNEW(61)             00017600
      COMMON S,ND,T,A,B,MP,MA,SUM,ID,X,IFF,COE,P,SU,AM,    NEWA,        00017700
     1LCODE,LVA,BNEW,N,NP,NH,NO,CON,JUNK,NCO,NR                         00017800
      IF(NCO) 10, 10, 40                                                00017900
 10   WRITE (6,900)                                                     00018000
      DO 35 IJ=1,N                                                      00018100
      READ (2) NR                                                       00018200
      READ (2)     (P(I), I=1,NR)                                       00018300
      IF(NR-1) 15, 15, 20                                               00018400
 15   A(3)=P(1)                                                         00018500
      A(4)=0.0                                                          00018600
      GO TO 30                                                          00018700
 20   A(1)=0.0                                                          00018800
      A(2)=0.0                                                          00018900
      OR=NR                                                             00019000
      DO 25 I=1,NR                                                      00019100
      A(1)=A(1)+P(I)                                                    00019200
 25   A(2)=A(2)+P(I)**2                                                 00019300
      A(3)=A(1)/OR                                                      00019400
      A(4)=SQRT ((OR*A(2)-A(1)**2)/(OR*(OR-1.0)))                       00019500
 30   WRITE (6,903)             IJ,NR,A(3),A(4)                         00019600
 35   CONTINUE                                                          00019700
      GO TO 120                                                         00019800
 40   NNCO=NCO+1                                                        00019900
      WRITE (6,902)            (I,I=1,NCO)                              00020000
      DO 115 IJ=1,N                                                     00020100
      MCO=NCO                                                           00020200
      DO 45 I=1,NCO                                                     00020300
 45   T(I,1)=0.0                                                        00020400
      A(1)=0.0                                                          00020500
      A(2)=0.0                                                          00020600
      READ (2) NR                                                       00020700
      DO 55 I=1,NR                                                      00020800
      READ (2)     (P(J), J=1,NNCO)                                     00020900
      DO 50 J=1,NCO                                                     00021000
 50   T(J,1)=T(J,1)+P(J)                                                00021100
      A(1)=A(1)+P(NNCO)                                                 00021200
 55   A(2)=A(2)+P(NNCO)**2                                              00021300
      IF(NR-1) 60, 60, 65                                               00021400
 60   A(3)=A(1)                                                         00021500
      A(4)=0.0                                                          00021600
      GO TO 75                                                          00021700
 65   OR=NR                                                             00021800
      DO 70 I=1,NCO                                                     00021900
 70   T(I,1)=T(I,1)/OR                                                  00022000
      A(3)=A(1)/OR                                                      00022100
      A(4)=SQRT ((OR*A(2)-A(1)**2)/(OR*(OR-1.0)))                       00022200
 75   L1=1                                                              00022300
      L2=0                                                              00022400
 78   IF(MCO-5) 80, 80, 85                                              00022500
 80   L2=L2+MCO                                                         00022600
      GO TO 90                                                          00022700
 85   L2=L2+5                                                           00022800
 90   IF(L1-1) 95, 95, 100                                              00022900
 95   WRITE (6,903)             IJ,NR,A(3),A(4),(T(I,1), I=L1,L2)       00023000
      GO TO 105                                                         00023100
 100  WRITE (6,904)             (T(I,1), I=L1,L2)                       00023200
 105  MCO=MCO-5                                                         00023300
      IF(MCO) 115, 115, 110                                             00023400
 110  L1=L1+5                                                           00023500
      GO TO 78                                                          00023600
 115  CONTINUE                                                          00023700
 120  REWIND 2                                                          00023800
 900  FORMAT(1H0/7H0DESIGN3X6HNO. OF8X4HMEAN9X9HSTD. DEV./11X4HREPS11X  00023900
     1,1HY14X,1HY)                                                      00024000
 902  FORMAT(1H0/7H0DESIGN3X6HNO. OF8X4HMEAN9X9HSTD. DEV.10X19HMEANS OF 00024100
     1COVARIATES/11X4HREPS11X1HY14X1HY4X5I14/(46X5I14))                 00024200
 903  FORMAT(1H I4,5X,I4,F17.5,F15.5,F17.3,4F14.3)                      00024300
 904  FORMAT(1H 45X,F17.3,4F14.3)                                       00024400
      RETURN                                                            00024500
      END                                                               00024600
CID   0901HS 15 150    $BMD05V GENERAL LINEAR HYPOTH. FOR MISSING DATA  00024700
CBD05V        GENERAL LINEAR HYPOTHESIS               JULY 22, 1965     00024800
      DIMENSION S(61,63),ND(61),T(61,63),A(63),B(61),MP(61),MA(61,64),  00024900
     1SUM(64),ID(64),X(61,63),IFF(2,64),COE(61,64),P(1060),SU(64),      00025000
     2AM(61,64),FMT(61),NEWA(64),LCODE(61),LVA(61),BNEW(61)             00025100
      COMMON S,ND,T,A,B,MP,MA,SUM,ID,X,IFF,COE,P,SU,AM,    NEWA,        00025200
     1LCODE,LVA,BNEW,N,NP,NH,NO,CON,JUNK,NCO,NR                         00025300
      EQUIVALENCE (AA3,IA3),(AA4,IA4)                                   00025400
C                                                                       00025500
  901 FORMAT(46H1BMD05V - GENERAL LINEAR HYPOTHESIS - VERSION ,         00025600
     116HOF JULY 22, 1965/                                              00025700
     241H HEALTH SCIENCES COMPUTING FACILITY, UCLA//)                   00025800
C                                                                       00025900
      AA3=(+6HDESIGN)                                                   00026000
      AA4=(+6HHYPOTH)                                                   00026100
      MTAPE = 5                                                         00026200
      CON=1.0E-6                                                        00026300
      REWIND 2                                                          00026400
 5    ID(1)=0                                                           00026500
      IX=1                                                              00026600
      MP(1)=0                                                           00026700
      NVG=0                                                             00026800
      ASSIGN 130 TO NAB                                                 00026900
      READ (5,900)          AA1,PROB,N,NP,NCO,NH,NVG,ONNN,NA,NTAPE,L1   00027000
      AA2=(+6HPROBLM)                                                   00027100
      IF (AA1 - AA2) 6,10,6                                             00027200
 6    IF(AA1-(+6HFINISH)) 7, 500, 7                                     00027300
 7    WRITE (6,940)                                                     00027400
      GO TO 500                                                         00027500
   10 IF(NP*(NP-61)) 6001,7,7                                           00027600
 6001 IF((NCO+NP)*(NCO+NP-61)) 6002,7,7                                 00027700
 6002 IF(NH*(NH-58)) 6004,7,7                                           00027800
 6004 IF(NTAPE-2) 8,7,8                                                 00027900
 8    CALL TPWD(NTAPE,MTAPE)                                            00028000
      WRITE (6,901)                                                     00028100
      WRITE (6,902)             PROB,N                                  00028200
      NCN=NCO+1                                                         00028300
      IF(NVG) 11, 11, 310                                               00028400
 310  WRITE (6,937)                                                     00028500
      WRITE (6,938)                                                     00028600
      AA2=(+6HTRNGEN)                                                   00028700
      NTRGOF=0                                                          00028800
      DO 313 I=1,NVG                                                    00028900
      READ (5,936)            AA1,NEWA(I),LCODE(I),LVA(I),BNEW(I)       00029000
      IF ( AA1-AA2)311,6313,311                                         00029100
  311 WRITE (6,905) I                                                   00029200
      GO TO 6008                                                        00029300
 6313 IF(LCODE(1)*(LCODE(1)-15))6315,6315,6006                          00029400
 6315 WRITE (6,939)           I,NEWA(I),LCODE(I),LVA(I),BNEW(I)         00029500
      IF(NCO)312,312,313                                                00029600
  312 IF(-((NEWA(I)-1)*(LVA(I)-1)))6005,3125,6005                       00029700
 6005 WRITE (6,920)                                                     00029800
      NEWA(I)=1                                                         00029900
      LVA(I)=1                                                          00030000
      GO TO 313                                                         00030100
 3125 IF(10-LCODE(I))3126,313,313                                       00030200
 3126 BNEW(I)=1.0                                                       00030300
      GO TO 313                                                         00030400
 6006 WRITE (6,6007) I                                                  00030500
 6008 NTRGOF=NTRGOF+1                                                   00030600
      WRITE (6,915)                                                     00030700
  313 CONTINUE                                                          00030800
      IF(NTRGOF) 11,11,500                                              00030900
 11   NP1=NP                                                            00031000
      ONNN=ONNN+1.0                                                     00031100
      KM=NP+1                                                           00031200
      N1=KM+NCO                                                         00031300
      NP=N1-1                                                           00031400
      NPP=NP                                                            00031500
      CALL VFCHCK(L1)                                                   00031600
 12   L1=12*L1                                                          00031700
      READ (5,931)            (FMT(I), I=1,L1)                          00031800
      WRITE (6,903) NP                                                  00031900
      JX=NH+3                                                           00032000
      DO 370 J=1,JX                                                     00032100
 370  SUM(J)=0.0                                                        00032200
      N2=N1+1                                                           00032300
      DO 15 J=1,N1                                                      00032400
      DO 15 I=1,NP                                                      00032500
 15   S(I,J)=0.0                                                        00032600
      WRITE (6,933)                                                     00032700
      L1=N1                                                             00032800
      IERROR=0                                                          00032900
      N5=21                                                             00033000
      IF(NP1-21)163,165,165                                             00033100
  163 N5=NP1                                                            00033200
  165 DO 35 IJ=1,N                                                      00033300
      N4=N5                                                             00033400
      READ (5,904)          IA1,A(1),(P(I),I=1,N4)                      00033500
  334 IF(IA1-IA3)550,335,550                                            00033600
  335 IF(N4-NP1)337,339,339                                             00033700
  337 N3=N4+1                                                           00033800
      N4=N4+22                                                          00033900
      IF(NP1-N4)338,3385,3385                                           00034000
  338 N4=NP1                                                            00034100
 3385 READ (5,904)          IA1,(P(I),I=N3,N4)                          00034200
      GO TO 334                                                         00034300
 339  IF(IERROR)345,340,340                                             00034400
 340  DO 36 I=1,NP1                                                     00034500
 36   ND(I)=P(I)                                                        00034600
      IF(NP1-30)37,37,38                                                00034700
 37   WRITE (6,934)             IJ, (ND(I), I=1,NP1)                    00034800
      GO TO 39                                                          00034900
   38 WRITE (6,934)           IJ,(ND(I),I=1,30)                         00035000
      WRITE (6,935)           (ND(I),I=31,NP1)                          00035100
 39   NR=A(1)                                                           00035200
      WRITE (2) NR                                                      00035300
      ID(1)=ID(1)+NR                                                    00035400
 345  IF(NCO) 17, 17, 22                                                00035500
 17   L2=NP+NR                                                          00035600
      READ (MTAPE,FMT)          (P(I),I=L1,L2)                          00035700
      IF(IERROR)35,350,350                                              00035800
  350 IF(-NVG)18,2005,2005                                              00035900
   18 DO 19 I=L1,L2                                                     00036000
      T(1,1) =P(I)                                                      00036100
      CALL TRANS(NVG,ONNN,IJ,IERROR)                                    00036200
   19 P(I)=T(1,1)                                                       00036300
 2005 WRITE (2)    (P(I),I=L1,L2)                                       00036400
      DO 21 I=L1,L2                                                     00036500
 21   SUM(1)=SUM(1)+P(I)*P(I)                                           00036600
      DO 25 J=1,NP                                                      00036700
      DO 25 I=1,NP                                                      00036800
 25   S(I,J)=S(I,J)+(P(I)*P(J))*A(1)                                    00036900
      DO 30 I=L1,L2                                                     00037000
      DO 30 J=1,NP                                                      00037100
 30   S(J,N1)=S(J,N1)+P(J)*P(I)                                         00037200
      GO TO 35                                                          00037300
 22   DO 34 IK=1,NR                                                     00037400
      READ (MTAPE,FMT)          (P(I),I=KM,L1)                          00037500
      IF(IERROR)34,355,355                                              00037600
 355  NO=0                                                              00037700
      DO 24 I=KM,L1                                                     00037800
      NO=NO+1                                                           00037900
 24   T(1,NO)=P(I)                                                      00038000
      IF(NVG)375,375,26                                                 00038100
 26   CALL TRANS(NVG,ONNN,IJ,IERROR)                                    00038200
      NO=0                                                              00038300
      DO 23 I=KM,L1                                                     00038400
      NO=NO+1                                                           00038500
 23   P(I)=T(1,NO)                                                      00038600
 375  CONTINUE                                                          00038700
      WRITE (2)     (P(I), I=KM,L1)                                     00038800
      SUM(1)=SUM(1)+P(L1)*P(L1)                                         00038900
      DO 32 J=1,NP                                                      00039000
      DO 32 I=1,NP                                                      00039100
 32   S(I,J)=S(I,J)+P(I)*P(J)                                           00039200
      DO 33 J=1,NP                                                      00039300
 33   S(J,N1)=S(J,N1)+P(J)*P(L1)                                        00039400
 34   CONTINUE                                                          00039500
 35   CONTINUE                                                          00039600
      END FILE 2                                                        00039700
      REWIND 2                                                          00039800
      IF(IERROR)360,380,380                                             00039900
 360  DO 365 J=1,NH                                                     00040000
 365  READ (5,907)            AA1,(MP(I), I=1,NPP)                      00040100
      GO TO 5                                                           00040200
 380  DO 13 J=1,JX                                                      00040300
      SU(J)=0.0                                                         00040400
      DO 13 I=1,NP                                                      00040500
      AM(I,J)=0.0                                                       00040600
 13   COE(I,J)=0.0                                                      00040700
      DO 40 I=1,NP                                                      00040800
 40   S(I,N2)=S(I,N1)                                                   00040900
      DO 45 J=1,N2                                                      00041000
      DO 45 I=1,NP                                                      00041100
 45   T(I,J)=S(I,J)                                                     00041200
      DO 46 I=1,NP                                                      00041300
      DO 46 J=1,N1                                                      00041400
 46   X(I,J)=T(I,J)                                                     00041500
      DO 48 I=1,60                                                      00041600
 48   FMT(I)=0.0                                                        00041700
      FMT(1)=(+1H()                                                     00041800
      FMT(2)=(+6HI6,6X,)                                                00041900
      L1=NPP+3                                                          00042000
      FMT(L1)=(+6HF18.5,)                                               00042100
      L1=L1+1                                                           00042200
      FMT(L1)=(+1H))                                                    00042300
      CALL MATRIX (N1,N2)                                               00042400
      CALL PUNCH                                                        00042500
      DO 85 I=1,NP                                                      00042600
 85   MP(I)=0                                                           00042700
      ASSIGN 86 TO NNN                                                  00042800
      GO TO 157                                                         00042900
 86   DO 87 I=1,NP                                                      00043000
 87   MP(I)=1                                                           00043100
      ASSIGN 121 TO NNN                                                 00043200
      M1=N1                                                             00043300
      M2=N2                                                             00043400
      NT=0                                                              00043500
      GO TO 99                                                          00043600
 90   NT=NT+1                                                           00043700
      NO=NPP                                                            00043800
      READ (5,907)            IA1,(MP(I), I=1,NO)                       00043900
      IF(IA1-IA4)336,70,336                                             00044000
 336  WRITE (6,909)            IA4,NT                                   00044100
      GO TO 500                                                         00044200
 70   NP=0                                                              00044300
      DO 72 I=1,NO                                                      00044400
      IF(MP(I)) 72, 72, 71                                              00044500
 71   NP=NP+1                                                           00044600
      ND(NP)=I                                                          00044700
 72   CONTINUE                                                          00044800
      M1=NP+1                                                           00044900
      ND(M1)=N1                                                         00045000
      DO 75 J=1,M1                                                      00045100
      KK=ND(J)                                                          00045200
      DO 75 I=1,NP                                                      00045300
      MM=ND(I)                                                          00045400
 75   T(I,J)=S(MM,KK)                                                   00045500
      M2=M1+1                                                           00045600
      DO 77 I=1,NP                                                      00045700
 77   T(I,M2)=T(I,M1)                                                   00045800
      DO 79 J=1,M1                                                      00045900
      DO 79 I=1,NP                                                      00046000
 79   X(I,J)=T(I,J)                                                     00046100
      CALL MATRIX (M1,M2)                                               00046200
 99   IX=IX+1                                                           00046300
      DO 100 I=1,NP                                                     00046400
 100  SU(IX)=SU(IX)+B(I)*X(I,M1)                                        00046500
      SUM(IX)=SUM(1)-SU(IX)                                             00046600
      ID(IX)=ID(1)-(NP-JUNK)                                            00046700
      IF(NA) 145, 145, 116                                              00046800
 116  DO 140 I=1,NP                                                     00046900
      A(I)=0.0                                                          00047000
      DO 140 J=1,NP                                                     00047100
 140  A(I)=A(I)+X(I,J)*B(J)                                             00047200
      DO 141 I=1,NP                                                     00047300
 141  A(I)=A(I)-X(I,M1)                                                 00047400
 145  KK=0                                                              00047500
      DO 152 I=1,NPP                                                    00047600
      IF(MP(I)) 152, 152, 151                                           00047700
 151  KK=KK+1                                                           00047800
      AM(I,IX)=A(KK)                                                    00047900
      COE(I,IX)=B(KK)                                                   00048000
 152  CONTINUE                                                          00048100
 157  DO 158 I=1,NPP                                                    00048200
 158  MA(I,IX)=MP(I)                                                    00048300
      GO TO NNN, (86,121)                                               00048400
 121  IF(NT-NH) 90, 125, 125                                            00048500
 125  GO TO NAB, (130, 200)                                             00048600
 130  ASSIGN 200 TO NAB                                                 00048700
      DO 135 J=2,NPP                                                    00048800
 135  MP(J)=0                                                           00048900
      MP(1)=1                                                           00049000
      NP=1                                                              00049100
      T(1,1)=S(1,1)                                                     00049200
      M1=2                                                              00049300
      M2=3                                                              00049400
      T(1,M1)=S(1,N1)                                                   00049500
      T(1,M2)=S(1,N2)                                                   00049600
      X(1,1)=T(1,1)                                                     00049700
      X(1,2)=T(1,2)                                                     00049800
      CALL MATRIX (M1,M2)                                               00049900
      GO TO 99                                                          00050000
 200  AA2=ID(2)                                                         00050100
      DO 205 I=3,IX                                                     00050200
      IFF(1,I)=ID(I)-ID(2)                                              00050300
      IFF(2,I)=ID(2)                                                    00050400
      AA1=IFF(1,I)                                                      00050500
 205  P(I)=(AA2/AA1)*((SUM(I)-SUM(2))/SUM(2))                           00050600
      WRITE (6,925)                                                     00050700
      WRITE (6,908)                                                     00050800
      DO 207 I=1,NPP                                                    00050900
 207  FMT(I+2)=(+3HI1,)                                                 00051000
      DO 210 I=1,IX                                                     00051100
 210  WRITE (6,FMT)             I, (MA(J,I),J=1,NPP),SU(I)              00051200
      WRITE (6,925)                                                     00051300
      WRITE (6,910)                                                     00051400
      L1=1                                                              00051500
      L2=0                                                              00051600
      JK=IX                                                             00051700
 216  IF(JK-6) 220, 220, 225                                            00051800
 220  L2=L2+JK                                                          00051900
      GO TO 230                                                         00052000
 225  L2=L2+6                                                           00052100
 230  WRITE (6,912)             ( I, I=L1,L2)                           00052200
      WRITE (6,913)                                                     00052300
      DO 235 I=1,NPP                                                    00052400
 235  WRITE (6,911)             I, (COE(I,J), J=L1,L2)                  00052500
      WRITE (6,914)             (SUM(I), I=L1,L2)                       00052600
      WRITE (6,916)                                                     00052700
      WRITE (6,917)             (ID(J), J=L1,L2)                        00052800
      WRITE (6,918)                                                     00052900
      IF(L1-1) 240, 240, 243                                            00053000
 240  WRITE (6,919)             (P(J), J=3,L2)                          00053100
      GO TO 245                                                         00053200
 243  WRITE (6,926)             (P(J), J=L1,L2)                         00053300
 245  WRITE (6,916)                                                     00053400
      IF(L1-1) 250, 250, 253                                            00053500
 250  WRITE (6,921)             (IFF(1,I),IFF(2,I), I=3,L2)             00053600
      GO TO 260                                                         00053700
 253  WRITE (6,927)             (IFF(1,I),IFF(2,I), I=L1,L2)            00053800
 260  WRITE (6,922)                                                     00053900
      JK=JK-6                                                           00054000
      IF(JK) 270, 270, 265                                              00054100
 265  L1=L1+6                                                           00054200
      GO TO 216                                                         00054300
 270  IF(NA) 5, 5, 275                                                  00054400
 275  WRITE (6,923)                                                     00054500
      L1=1                                                              00054600
      L2=0                                                              00054700
      JK=IX                                                             00054800
 276  IF(JK-6) 280, 280, 285                                            00054900
 280  L2=L2+JK                                                          00055000
      GO TO 290                                                         00055100
 285  L2=L2+6                                                           00055200
 290  IF(L1-1) 295, 295, 300                                            00055300
 295  DO 296 I=1,NPP                                                    00055400
 296  WRITE (6,924)             I, (AM(I,J), J=2,L2)                    00055500
      GO TO 303                                                         00055600
 300  DO 301 I=1,NPP                                                    00055700
 301  WRITE (6,930)             I, (AM(I,J), J=L1,L2)                   00055800
 303  JK=JK-6                                                           00055900
      IF(JK) 5, 5, 305                                                  00056000
 305  L1=L1+6                                                           00056100
      WRITE (6,925)                                                     00056200
      GO TO 276                                                         00056300
 550  WRITE (6,909)           IA3,IJ                                    00056400
      GO TO 500                                                         00056500
 900  FORMAT(A6,A2,I3,4I2,F6.0,I2,41X,I2,I2)                            00056600
 902  FORMAT(17H0PROBLEM NUMBER  A2//27H NUMBER OF DESIGN CARD SETSI6)  00056700
  903 FORMAT(32H0NUMBER OF INDEPENDENT VARIABLES I6//)                  00056800
  904 FORMAT(A6,22F3.0)                                                 00056900
 905  FORMAT(31H0ERROR ON TRANS-GENERATION CARDI4)                      00057000
 906  FORMAT(72I1)                                                      00057100
 907  FORMAT(A6,66I1)                                                   00057200
 908  FORMAT(55H0HYPOTHESES AND SUMS OF SQUARES EXPLAINED BY HYPOTHESES/00057300
     1/)                                                                00057400
 909  FORMAT(1H0,24X,A6,5H CARD,I4,52H MISPUNCHED OR OUT OF ORDER. PROGR00057500
     XAM CANNOT PROCEED.)                                               00057600
 910  FORMAT(26H0ESTIMATES OF COEFFICIENTS/35X,19HH Y P O T H E S I S)  00057700
 911  FORMAT(I6,4X6F16.5)                                               00057800
 912  FORMAT(1H04X6I16/(5X6I16))                                        00057900
 913  FORMAT(9H VARIABLE)                                               00058000
 914  FORMAT(9H0RESIDUAL/9H SUM SQS.F17.5,5F16.5)                       00058100
  915 FORMAT(24H0PROGRAM WILL TERMINATE.)                               00058200
 916  FORMAT(11H0DEGREES OF)                                            00058300
 917  FORMAT(11H FREEDOM OFI10,5I16)                                    00058400
 918  FORMAT(10H RESIDUALS)                                             00058500
 919  FORMAT(10H0F TESTS  32X,4F16.5)                                   00058600
  920 FORMAT(1H0,23X,71HALL TRANSGENERATION CARD VARIABLES MUST BE 1 FOR00058700
     X THE NO COVARIATE CASE./26X,66HTHE ABOVE CARD IS INCORRECT. THE VA00058800
     XRIABLES WILL BE SET EQUAL TO 1.)                                  00058900
 921  FORMAT(11H FREEDOM OF32X,6(I9,1H ,I4,2H  ))                       00059000
 922  FORMAT(8H F TESTS/1H0)                                            00059100
 923  FORMAT(25H0ACCURACY OF COEFFICIENTS)                              00059200
 924  FORMAT(I6,20X,5F16.7)                                             00059300
 925  FORMAT(1H0)                                                       00059400
 926  FORMAT(10H0F TESTS  6F16.5)                                       00059500
 927  FORMAT(11H FREEDOM OF6(I9,1H I4,2H  ))                            00059600
 930  FORMAT(I6,4X,6F16.7)                                              00059700
 931  FORMAT(12A6)                                                      00059800
 933  FORMAT(7H0DESIGN//)                                               00059900
  934 FORMAT(1X,I4,3X,30I3)                                             00060000
  935 FORMAT(8X,30I3)                                                   00060100
 936  FORMAT(A6,I3,I2,I3,F6.0)                                          00060200
 937  FORMAT(1H06X,21HTRANS-GENERATION CARD)                            00060300
 938  FORMAT(46H0CARD    NEW     TRANS    ORIG.   ORIG. VAR(B)/45H  NO. 00060400
     1VARIABLE   CODE    VAR(A)   OR CONSTANT)                          00060500
 939  FORMAT(2H  I2,I8,2I9,4X,F10.5)                                    00060600
 940  FORMAT(22H0ERROR ON PROBLEM CARD)                                 00060700
 6007 FORMAT(47H0ILLEGAL TRANSGENERATION CODE SPECIFIED ON CARD,I3)     00060800
 500  IF(MTAPE-5)504,504,503                                            00060900
  503 CONTINUE                                                          00061000
 504  STOP                                                              00061100
      END                                                               00061200
